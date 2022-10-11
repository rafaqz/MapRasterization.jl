const DEFAULT_CATEGORY_TOLERANCE = 2.0

struct MapSelection{S<:NamedTuple,O}
    settings::S
    removals::Vector{Vector{Point2{Float32}}}
    points::Vector{Vector{Point2{Float32}}}
    output::O
end

struct MapState{S<:NamedTuple,PT,PY,T,K,O}
    settings::S
    points::PT
    polygons::PY
    category_tolerances::T
    category_keep::K
    output::O
end

@enum Panel zero_panel source_panel balance_panel blur_panel categorize_panel clean_panel fill_panel difference_panel polygons_panel warp_panel
const MAPTITLES = ["Source", "Balanced", "Blurred", "Categorized", "Cleaned", "Filled", "Difference", "Manual"]

function selectcolors(raw_map::AbstractArray, map_selection::MapSelection; kw...)
    points = map_selection.points
    selectcolors(raw_map; points, map_selection.settings..., kw...)
end
function selectcolors(raw_map::AbstractArray, map_selection::MapState; kw...)
    points = map_selection.points
    selectcolors(raw_map; points, map_selection.settings..., kw...)
end
function selectcolors(raw_map::AbstractArray{C};
    ncategories=5,
    match=(:color, :std, :stripe),
    points=Vector{Point2{Float32}}[Point2{Float32}[] for _ in 1:ncategories],
    polygons=[points],#[Vector{Point2{Float32}}[] for _ in 1:ncategories],
    scan_threshold=0.001,
    prune_threshold=0,
    stripe_radius=2,
    line_threshold=0.1,
    template=nothing,
    blur_repeat=1,
    fill_repeat=1,
    segment=false,
    category_tolerances=[DEFAULT_CATEGORY_TOLERANCE for _ in 1:length(points)],
    category_keep=Bool[true for _ in 1:length(points)],
    kw...
) where C
    match = Tuple(map(Symbol, match))
    settings_obs = (; map(Observable,
        (; scan_threshold, prune_threshold, line_threshold, blur_repeat,
         fill_repeat, stripe_radius, segment, ncategories=length(points))
        )...,
        match=map(Observable, _bools_from_match(match)),
    )
    points_obs = map(Observable, points)
    polygons_obs = map(Observable, polygons[1])
    category_tolerances = map(Observable, category_tolerances)
    category_keep = map(Observable, category_keep)
    obs = MapState(
        settings_obs,
        points_obs,
        polygons_obs,
        category_tolerances,
        category_keep,
        nothing,
    )
    maps = _make_maps(raw_map, obs)

    fig = Figure()
    layout = _layout_figure!(fig, maps, obs)

    # _select_common_points(maps.raw[], template; 
        # points=nothing, keys=nothing, missingval, fig, ax1=layout.axes[1], ax2=layout.axes[2]
    # )

    screen = display(fig)

    # Wait for user input
    while screen.window_open[]
        sleep(0.1)
    end

    # Return results
    removals = Vector{Point2{Float32}}[]
    points = map(getindex, points_obs)

    s = layout.sliders
    settings = (
        scan_threshold=s.scan_threshold.value[],
        prune_threshold=s.prune_threshold.value[],
        line_threshold=s.line_threshold.value[],
        blur_repeat=s.blur_repeat.value[],
        category_tolerances=map(x -> x.value[], layout.category_widgets.tolerance_sliders),
        category_keep=map(x -> x.active[], layout.category_widgets.keep_toggles),
        fill_repeat=s.fill_repeat.value[],
        stripe_radius=s.stripe_radius.value[],
        match=_unwrap(_match_from_bools(map(m -> m.active[], layout.match_toggles))),
        segment=layout.buttons.segment_toggle.active[],
    )

    return MapSelection(settings, removals, points, maps.filled[])
end

function _layout_figure!(fig, maps, obs)
    current_panel = Observable(1)
    point_category = Observable(1)

    polygon_number = Observable(1)
    panel = GridLayout(tellwidth = false)
    # Top buttons
    button_grid, buttons = _make_buttons!(fig, obs)
    # Pixel property match toggles
    match_toggles_grid, match_toggles = _make_match_toggles!(fig, obs.settings.match)
    # Constrol sliders
    slider_grid, panel_sliders, sliders = _make_sliders!(fig, obs)
    plot_grid = GridLayout(tellheight = false, tellwidth = false)
    hover_label = Label(fig, "")
    # Categories
    category_grid, category_widgets = _make_category_widgets!(fig, maps, obs)
    # Maps
    # Layout
    fig[1, 1:3] = panel
    panel[1, 1:2] = button_grid
    panel[2:3, 1] = match_toggles_grid
    panel[2:3, 2] = slider_grid
    panel[4:10, 1:2] = plot_grid
    panel[11, 1:2] = hover_label
    fig[1, 4] = category_grid

    axes, plots = _make_axes!(plot_grid, maps, current_panel)

    # Lump all layout objects in a NamedTuple
    layout = (;
        axes,
        buttons,
        category_widgets,
        current_panel,
        grids = (;
            panel,
            buttons=button_grid,
            category=category_grid,
            match_toggles=match_toggles_grid,
            plots=plot_grid,
            slider=slider_grid,
        ),
        hover_label,
        match_toggles,
        panel_sliders,
        plots,
        point_category,
        sliders,
    )
    # Join everything together
    _connect_observables!(fig, maps, obs, layout)

    return layout
end

_moveaxis(axes, x) = map(ax -> _moveaxis(ax, x), axes)
function _moveaxis(ax::Axis, x)
    fl = ax.finallimits[] + Point2(x...)
    ax.finallimits[] = fl
end

_kept_categories(bitindex::AbstractVector{Bool}) = (1:length(bitindex))[bitindex]
_kept_categories(widgets::NamedTuple) = _kept_categories(_category_bitindex(widgets))

function _category_bitindex(category_widgets::NamedTuple)
    map(t -> t.active[], category_widgets.keep_toggles)
end

function _make_match_toggles!(fig, match)
    toggles = [Toggle(fig; active) for active in map(getindex, match)]
    map(toggles, match) do t, m
        on(t.active) do x
            m[] = x
            notify(m)
        end
    end
    labels = [Label(fig, lift(x -> x ? "$l" : "$l", t.active))
        for (t, l) in zip(toggles, ["color", "texture", "stripe"])]
    labelled_toggles = map(zip(toggles, labels)) do xs
        g = GridLayout(tellwidth=false)
        g[1, 1] = xs[1]; g[1, 2] = xs[2]
        g
    end
    grid = grid!(permutedims(hcat(labelled_toggles...)), tellwidth=false, tellheight=false)
    return grid, toggles
end

function _make_maps(raw_map::AbstractMatrix, obs)
    s = obs.settings
    points = map(getindex, obs.points)
    raw = Observable(raw_map)
    balanced = Observable(_balance(raw_map, points; category_bitindex=map(getindex, obs.category_keep)))
    known_categories = Observable(_set_categories!(zeros(Int, size(raw_map)), points))
    blurred = Observable(
        blur(balanced[]; repeat=s.blur_repeat[])
    )
    std = Observable(_stds(blurred[]))
    stripe = Observable(_stripes(blurred[], radius=s.stripe_radius[]))
    categorized = Observable(fill!(similar(raw_map, Int), 0))
    lines_removed = Observable(fill!(similar(raw_map, Int), 0))
    filled = Observable(fill!(similar(raw_map, Int), 0))

    rgba_categorized = Observable(fill!(similar(raw_map, RGBA{Float64}), zero(RGBA{Float64})))
    rgba_lines_removed = Observable(fill!(similar(raw_map, RGBA{Float64}), zero(RGBA{Float64})))
    rgba_filled = Observable(fill!(similar(raw_map, RGBA{Float64}), zero(RGBA{Float64})))
    rgba_difference = Observable(fill!(similar(raw_map, RGBA{Float64}), zero(RGBA{Float64})))

    (; raw, std, stripe, balanced, known_categories, blurred, categorized, lines_removed, filled,
        rgba_categorized, rgba_lines_removed, rgba_filled, rgba_difference)
end

function _make_category_widgets!(fig, maps, obs)
    s = obs.settings
    points = map(getindex, obs.points)
    grid = GridLayout(tellheight=false, tellwidth=false)
    grid[1, 1] = controlgrid = GridLayout(tellheight=false, tellwidth=false, default_rowgap=0)

    controlgrid[1, 1] = plus_button = Button(fig, label="+")
    controlgrid[1, 2] = clear_button = Button(fig, label="clear points")
    controlgrid[1, 3] = color_number = Label(fig, "1")

    color_buttons = map(1:s.ncategories[]) do n
        _color_button(fig, points, maps.blurred[], n)
    end
    color_buttons[1].strokecolor = :black
    tolerance_sliders = map(t -> _tolerance_slider(fig, t), obs.category_tolerances)
    keep_toggles = map(k -> _keep_toggle(fig, k), obs.category_keep)
    widget_labels = map(["Color";; "Tolerance";; "Keep";;]) do s
        Label(fig, s)
    end
    grid[2, 1] = grid!(widget_labels)
    _set_category_rows!(fig, grid, color_buttons, tolerance_sliders, keep_toggles)

    widgets = (; color_buttons, tolerance_sliders, keep_toggles, clear_button, color_number, plus_button)
    return grid, widgets
end

function _set_category_rows!(fig, grid, color_buttons, tolerance_sliders, keep_toggles)
    widget_rows = map(color_buttons, tolerance_sliders, keep_toggles) do b, s, t
        [b;; s;; t;;]
    end
    grid[3:10, 1] = grid!(vcat(widget_rows...), default_rowgap=0)
end

_color_button(fig, points, source, n) =
    Button(fig; label=string(n), height=21, width=21, buttoncolor=_mean_color(source, points[n]))
_tolerance_slider(fig, t) = Slider(fig; height=21, startvalue=t, width=60, range=-2.0:0.01:20.0, horizontal=true)
_keep_toggle(fig, active) = Toggle(fig; active, height=21)

function _make_buttons!(fig, obs)
    grid = GridLayout(tellwidth = false)
    grid[1, 1] = balance = Button(fig; label="balance")
    grid[1, 2] = blur = Button(fig; label="blur")
    grid[1, 3] = categorize = Button(fig; label="categorize")
    grid[1, 4] = remove_lines = Button(fig; label="clean")
    grid[1, 5] = fill = Button(fig; label="fill")
    grid[1, 6] = previous = Button(fig; label="previous")
    grid[1, 7] = next = Button(fig; label="next")
    segment_toggle = Toggle(fig; active=obs.settings.segment)
    grid[1, 8] = grid!([Label(fig, "Use segmentation");; segment_toggle;;])
    buttons = (; balance, blur, categorize, fill, remove_lines, next, previous, segment_toggle)
    return grid, buttons
end

function _make_sliders!(fig, obs)
    s = obs.settings

    slidergrid = GridLayout(tellwidth=false, tellheight=false)

    # Blur
    blur_repeat_labelled = labelslider!(fig, "blur iterations", 0:20; height=20, tellwidth=false, sliderkw=(; startvalue=s.blur_repeat))
    slidergrid[1, 1] = blur_repeat_labelled.layout
    T = typeof(blur_repeat_labelled.layout)
    panel_sliders = fill(T[], length(instances(Panel)))
    panel_sliders[Int(blur_panel)] = [blur_repeat_labelled.layout]

    # Categorise
    stripe_radius_labelled = labelslider!(fig, "stripe radius", 1:20; height=20, tellwidth=false, sliderkw=(; startvalue=s.stripe_radius))
    slidergrid[2, 1] = stripe_radius_labelled.layout
    scan_threshold_labelled = labelslider!(fig, "scan threshold", _aslog(0.00001:0.0001:0.1); height=20, tellwidth=false, sliderkw=(; startvalue=s.scan_threshold))
    slidergrid[3, 1] = scan_threshold_labelled.layout
    panel_sliders[Int(categorize_panel)] = [stripe_radius_labelled.layout, scan_threshold_labelled.layout]

    # Clean
    line_threshold_labelled = labelslider!(fig, "line threshold", 0.0001:0.001:1.0; height=20, tellwidth=false, sliderkw=(; startvalue=s.line_threshold))
    slidergrid[4, 1] = line_threshold_labelled.layout
    prune_threshold_labelled = labelslider!(fig, "prune threshold", 0:1000; height=20, tellwidth=false, sliderkw=(; startvalue=s.prune_threshold))
    slidergrid[5, 1] = prune_threshold_labelled.layout
    panel_sliders[Int(clean_panel)] = [line_threshold_labelled.layout, prune_threshold_labelled.layout]

    # Fill
    fill_labelled = labelslider!(fig, "gap filling iterations", 0:20; height=20, tellwidth=false, sliderkw=(; startvalue=s.fill_repeat))
    slidergrid[6, 1] = fill_labelled.layout
    panel_sliders[Int(fill_panel)] = [fill_labelled.layout]

    sliders = (
        blur_repeat=blur_repeat_labelled.slider,
        scan_threshold=scan_threshold_labelled.slider,
        line_threshold=line_threshold_labelled.slider,
        prune_threshold=prune_threshold_labelled.slider,
        stripe_radius=stripe_radius_labelled.slider,
        fill_repeat=fill_labelled.slider,
    )

    return slidergrid, panel_sliders, sliders
end

function _make_axes!(plot_grid, maps, current_panel)
    ax1 = Axis(plot_grid[1, 1]; alignmode=Inside(), title=MAPTITLES[1])
    ax2 = Axis(plot_grid[1, 2]; alignmode=Inside(), title=MAPTITLES[1])
    ax1.aspect = ax2.aspect = AxisAspect(1)
    plots = map(_plotmaps(maps)) do m
        p = heatmap!(ax1, m)
        p.visible[] = false
        p
    end
    plots[1].visible[] = true
    return [ax1, ax2], plots
end

_plotmaps(maps) = [maps.raw, maps.balanced, maps.blurred, maps.rgba_categorized, maps.rgba_lines_removed, maps.rgba_filled, maps.rgba_difference, maps.balanced]

function _on_click_color_button!(color_buttons, point_category, i)
    foreach(b -> b.strokecolor = :transparent, color_buttons)
    color_buttons[i].strokecolor = :black
    point_category[] = i
    notify(point_category)
end

function _connect_observables!(fig, maps, obs, layout)
    (; axes, buttons, category_widgets, current_panel, panel_sliders, plots, point_category, sliders) = layout
    # Maps
    onany(maps.categorized) do cm
        maps.rgba_categorized[] = _recolor(cm, maps.blurred[], map(getindex, obs.points))
        notify(maps.rgba_categorized)
    end
    onany(maps.filled) do cm
        maps.rgba_filled[] = _recolor(cm, maps.blurred[], map(getindex, obs.points))
        maps.rgba_difference[] = maps.rgba_filled[] .- maps.raw[] .+ RGB(0.5)
        notify(maps.rgba_filled)
        notify(maps.rgba_difference)
    end
    on(maps.lines_removed) do cm
        maps.rgba_lines_removed[] = _recolor(cm, maps.blurred[], map(getindex, obs.points))
        notify(maps.rgba_lines_removed)
    end

    # Category Widgets
    map(enumerate(category_widgets.color_buttons)) do (i, btn)
        on(btn.clicks) do _
            _on_click_color_button!(category_widgets.color_buttons, point_category, i)
        end
    end
    on(category_widgets.plus_button.clicks) do _
        (; color_buttons, tolerance_sliders, keep_toggles) = category_widgets
        pointvec = Observable(Point2{Float32}[])
        push!(obs.points, pointvec)
        n = obs.settings.ncategories[] = length(obs.points)

        newbutton = _color_button(fig, map(getindex, obs.points), maps.blurred[], n)
        newslider = _tolerance_slider(fig, DEFAULT_CATEGORY_TOLERANCE)
        newtoggle = _keep_toggle(fig, true)

        push!(color_buttons, newbutton)
        push!(tolerance_sliders, newslider)
        push!(keep_toggles, newtoggle)

        on(newbutton.clicks) do _
            _on_click_color_button!(color_buttons, point_category, n)
        end

        textplots, scatterplots = on_point(n, pointvec, category_widgets, maps, axes)

        _set_category_rows!(fig, layout.grids.category, color_buttons,  tolerance_sliders, keep_toggles)
    end
    on(category_widgets.clear_button.clicks) do _
        pointvec = obs.points[point_category[]]
        deleteat!(pointvec[], eachindex(pointvec[]))
        notify(pointvec)
    end

    # Buttons
    on(buttons.previous.clicks) do _
        if current_panel[] > 1
            n = current_panel[] - 1
            _update_panel(axes, plots, panel_sliders, current_panel, n)
        end
    end
    on(buttons.next.clicks) do _
        if current_panel[] < length(instances(Panel))
            n = current_panel[] + 1
            _update_panel(axes, plots, panel_sliders, current_panel, n)
        end
    end
    on(buttons.balance.clicks) do _
        maps.balanced[] = _balance(maps.raw[], map(getindex, obs.points);
            category_bitindex=_category_bitindex(category_widgets)
        )
        _update_panel(axes, plots, panel_sliders, current_panel, Int(balance_panel))
        notify(maps.balanced)
    end
    on(buttons.blur.clicks) do _
        maps.blurred[] = blur(maps.balanced[]; repeat=sliders.blur_repeat.value[])
        maps.stripe[] = _stripes(maps.blurred[], radius=sliders.stripe_radius.value[])
        maps.std[] = _stds(maps.blurred[])
        _update_panel(axes, plots, panel_sliders, current_panel, Int(blur_panel))
        notify(maps.blurred)
    end
    on(buttons.categorize.clicks) do _
        points = map(getindex, obs.points)
        if any(p -> length(p) > 0, points)
            maps.categorized[] = _categorize(map(getindex, maps), points;
                tolerances=map(t -> t.value[], category_widgets.tolerance_sliders),
                scan_threshold=sliders.scan_threshold.value[],
                segmented=buttons.segment_toggle.active[],
                match=_match_from_bools(map(getindex, obs.settings.match)),
            )
            _update_panel(axes, plots, panel_sliders, current_panel, Int(categorize_panel))
            notify(maps.categorized)
        end
    end
    on(buttons.remove_lines.clicks) do _
        maps.lines_removed[] = _remove_lines(maps.categorized[], maps.known_categories[];
            categories=_kept_categories(category_widgets),
            line_threshold=sliders.line_threshold.value[],
            prune_threshold=sliders.prune_threshold.value[],
        )
        _update_panel(axes, plots, panel_sliders, current_panel, Int(clean_panel))
        notify(maps.lines_removed)
    end
    on(buttons.fill.clicks) do _
        maps.filled[] = _fill(maps, layout, obs)
        _update_panel(axes, plots, panel_sliders, current_panel, Int(fill_panel))
        notify(maps.filled)
    end

    # Other observables
    on(point_category) do n
        category_widgets.color_number.text[] = string(n)
        notify(category_widgets.color_number.text)
    end
    text_scatter_plots = map(enumerate(obs.points)) do (pc, pv)
        on_point(pc, pv, category_widgets, maps, axes)
    end
    textplots = first(text_scatter_plots)
    scatterplots = last(text_scatter_plots)

    # Add point input to maps
    map(axes) do ax
        dragselect!(fig, ax, obs.points; section=point_category)
    end

    map_stages = maps[(:raw, :balanced, :blurred, :categorized, :lines_removed, :filled, :rgba_difference)]

    hover_report!(layout.hover_label.text, fig, axes[1], map_stages, current_panel)

    # real_polygons = map(ps -> Observable(Polygon(ps)), obs.polygons[1])
    # map(obs.polygons, real_polygons) do po, rp
    #     on(po) do poly
    #         rp[] = Polygon(poly)
    #         notify(rp)
    #     end
    # end
    # map(real_polygons) do rp
    #     poly!(axes[6], rp)
    #     scatter!(axes[6], rp)
    # end
    # dragselect!(fig, last(axes), polygons_obs; section=polygon_number)

    # Keyboard arrow movement
    scale = lift(axes[1].finallimits) do fl
        round(Int, maximum(fl.widths) / 10)
    end
    on(events(fig).keyboardbutton) do event
        s = scale[]
        event.key == Keyboard.right  && _moveaxis(axes, (s, 0))
        event.key == Keyboard.up     && _moveaxis(axes, (0, s))
        event.key == Keyboard.left   && _moveaxis(axes, (-s, 0))
        event.key == Keyboard.down   && _moveaxis(axes, (0, -s))
        # Let the event reach other listeners
        return Consume(false)
    end
end

function _update_panel(axes, plots, panel_sliders, current_panel, n)
    current_panel[] = n
    axes[1].title[] = MAPTITLES[n]
    for p in plots
        p.visible[] = false
    end
    plots[n].visible[] = true
    for (i, p) in enumerate(panel_sliders), s in p
        s.halign[] = (i == n ? :left : :right)
        s.width[] = (i == n ? 600 : 0)
    end
    if n == Int(warp_panel)
        # Show two plots
    end
end

function on_point(point_category::Int, pv::Observable, category_widgets, maps, axes)
    on(pv) do pv
        meancolor = _mean_color(maps.balanced[], pv)
        category_widgets.color_buttons[point_category].buttoncolor[] = meancolor
        A = maps.known_categories[]
        replace!(A, point_category => 0)
        _set_categories!(A, pv, point_category)
    end
    point_color = lift(pv) do pointsvec
        _mean_color(maps.balanced[], pointsvec)
    end
    labels = lift(p -> [string(point_category) for _ in 1:length(p)], pv)
    textplots = map(axes) do ax
        t = Makie.text!(ax, labels; position=pv)
    end
    scatterplots = map(axes) do ax
        s = Makie.scatter!(ax, pv;
            color=point_color,
            strokecolor=:black,
            strokewidth=1,
        )
    end
    textplots, scatterplots
end


function _recolor(categories, source, points)
    colors = map(ps -> _mean_color(source, ps), points)
    map(categories) do cat
        cat == 0 ? RGBA(RGB(0), 0) : RGBA(colors[cat])
    end
end

_bools_from_match(xs::Val) = _bools_from_match(_unwrap(xs))
function _bools_from_match(xs::Tuple)
    [:color in xs, :std in xs, :stripe in xs]
end

function _match_from_bools(xs)
    syms = Symbol[]
    xs[1] && push!(syms, :color)
    xs[2] && push!(syms, :std)
    xs[3] && push!(syms, :stripe)
    return Val{(syms...,)}()
end

function _set_categories!(A, points::AbstractArray{<:AbstractArray})
    fill!(A, 0)
    for (i, pointvec) in enumerate(points)
        _set_categories!(A, pointvec, i)
    end
    return A
end
function _set_categories!(A, pointvec::AbstractArray{<:Point2}, i)
    for p in pointvec
        A[_point_index(p)...] = i
    end
end

function _fill(maps, layout, obs)
    values = maps.lines_removed[]
    known = maps.known_categories[]
    points = map(getindex, obs.points)
    categories = _kept_categories(layout.category_widgets)
    original = PixelProp.(maps.blurred[], maps.std[], maps.stripe[], maps.known_categories[])
    stats = _component_stats(original, points[categories])
    repeat = layout.sliders.fill_repeat.value[]
    match = _match_from_bools(map(m -> m.active[], layout.match_toggles))
    for i in 1:repeat
        values = fill_gaps(values;
            known, stats, categories, original, match, missingval=0,
        )
    end
    return values
end

function _mean_color(A::AbstractMatrix{C}, pointvec::AbstractVector) where C<:Colorant
    stats = _component_stats(A, pointvec, 0)
    return ismissing(stats) ? C(zero(RGB(first(A)))) : C(map(s -> s.mean, stats)...)
end

function _stds(A)
    stds = Neighborhoods.broadcast_neighborhood(Window{2}(), A) do hood
        rs = std(map(n -> n.r, hood))
        gs = std(map(n -> n.g, hood))
        bs = std(map(n -> n.b, hood))
        sum((rs, gs, bs))
    end

    return stds ./ maximum(stds)
end

function _aslog(range)
    x = first(range)
    y = last(range)
    return (((2 .^ (x:step(range):y) .- (2^x)) ./ 2^y)) .* (y - x) .+ x
end
