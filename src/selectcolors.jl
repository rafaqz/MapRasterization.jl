const DEFAULT_CATEGORY_TOLERANCE = 2.0

struct MapSelection{S<:NamedTuple,O}
    settings::S
    removals::Vector{Vector{Point2{Float32}}}
    points::Vector{Vector{Point2{Float32}}}
    output::O
end

# struct MapState{S<:NamedTuple,PT,PY,T,K,O}
#     settings::S
#     points::PT
#     polygons::PY
#     category_tolerances::T
#     category_keep::K
#     output::O
# end

@enum Panel zero_panel source_panel balance_panel blur_panel categorize_panel clean_panel fill_panel difference_panel heatmap_panel
const MAPTITLES = ["Source", "Balanced", "Smoothed", "Categorized", "Cleaned", "Filled", "Difference", "Final"]
const MAPKEYS = (:raw, :balanced, :blurred, :rgba_categorized, :rgba_cleaned, :rgba_filled, :rgba_difference, :filled)

function selectcolors(raw_map::AbstractArray, map_selection::MapSelection; kw...)
    points = map_selection.points
    selectcolors(raw_map; points, map_selection.settings..., kw...)
end
# function selectcolors(raw_map::AbstractArray, map_selection::NamedTuple; kw...)
#     points = map_selection.points
#     selectcolors(raw_map; points, map_selection.settings..., kw...)
# end
function selectcolors(raw_map::AbstractArray{C};
    ncategories::Int=5,
    match=(:color, :std, :stripe),
    points=[Point2{Float32}[] for _ in 1:ncategories],
    polygons=[[[Point2(100.0f0, 100.0f0)]] for _ in 1:length(points)],
    scan_threshold::Float64=0.001,
    prune_threshold::Int64=0,
    stripe_radius::Int64=2,
    line_threshold::Float64=0.1,
    template=nothing,
    blur_repeat::Int=1,
    fill_repeat::Int=1,
    segment::Bool=false,
    category_tolerances=[DEFAULT_CATEGORY_TOLERANCE for _ in 1:length(points)],
    category_keep=[true for _ in 1:length(points)],
    category_balance=[x == 1 for x in 1:length(points)],
    category_name=[" " for _ in 1:length(points)],
    kw...
) where C
    # Clean up polygon types
    polygons = broadcast(polygons) do ps
        a = Vector{Vector{Point2{Float32}}}(undef, length(ps))
        broadcast!(a, ps) do poly
            b = Vector{Point2{Float32}}(undef, length(poly))
            broadcast!(Point2{Float32}, b, poly)
            return b
        end
    end
    points = convert(Vector{Vector{Point2{Float32}}}, points)
    polygons = convert(Vector{Vector{Vector{Point2{Float32}}}}, polygons)
    category_name = convert(Vector{String}, category_name)
    category_tolerances = convert(Vector{Float64}, category_tolerances)
    category_keep = convert(Vector{Bool}, category_keep)
    category_balance = convert(Vector{Bool}, category_balance)
    match = Tuple(map(Symbol, match))
    println("Creating observables...")
    settings_obs = (; map(Observable,
        (; scan_threshold, prune_threshold, line_threshold, blur_repeat,
         fill_repeat, stripe_radius, segment, ncategories=length(points))
        )...,
        match=map(Observable, _bools_from_match(match)),
        point_select_active=Observable(true),
        polygon_select_active=Observable(false),
    )
    points_obs = map(Observable, points)
    polygons_obs = map(Observable, polygons)
    category_tolerances = map(Observable, category_tolerances)
    category_keep = map(Observable, category_keep)
    category_balance = map(Observable, category_balance)
    category_name = map(Observable, category_name)
    obs = (;
        settings=settings_obs,
        points=points_obs,
        polygons=polygons_obs,
        category_tolerances,
        category_keep,
        category_balance,
        category_name,
    )
    println("Building maps...")
    maps = _make_maps(raw_map, obs)
    @show typeof(maps)

    println("Building layout...")
    fig = Figure()
    layout = _layout_figure!(fig, maps, obs, polygons)

    println("Displaying interface...")
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
        category_name=map(x -> x.displayed_string[], layout.category_widgets.name_textboxes),
        category_keep=map(x -> x.active[], layout.category_widgets.keep_toggles),
        category_balance=map(x -> x.active[], layout.category_widgets.balance_toggles),
        fill_repeat=s.fill_repeat.value[],
        stripe_radius=s.stripe_radius.value[],
        match=_unwrap(_match_from_bools(map(m -> m.active[], layout.match_toggles))),
        segment=layout.buttons.segment_toggle.active[],
        polygons=map(getindex, obs.polygons),
    )

    return MapSelection(settings, removals, points, maps.filled[])
end

function _layout_figure!(fig, maps, obs, polygons)
    current_panel = Observable(1)
    point_category = Observable(1)

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
    # Layout
    fig[1, 1:3] = panel
    panel[1, 1:2] = button_grid
    panel[2:3, 1] = match_toggles_grid
    panel[2:3, 2] = slider_grid
    panel[4:10, 1:2] = plot_grid
    panel[11, 1:2] = hover_label
    fig[1, 4] = category_grid

    # Plot maps
    axis, plots = _make_axis!(plot_grid, maps, current_panel)

    # Lump all layout objects in a NamedTuple
    layout = (;
        axis,
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
    _connect_observables!(fig, maps, obs, layout, polygons)

    return layout
end

function _moveaxis(axis::Axis, x)
    fl = axis.finallimits[] + Point2(x...)
    axis.finallimits[] = fl
end

_kept_categories(bitindex::AbstractVector{Bool}) = (1:length(bitindex))[bitindex]
_kept_categories(widgets::NamedTuple) = _kept_categories(_category_bitindex(widgets.keep_toggles))
_balance_categories(bitindex::AbstractVector{Bool}) = (1:length(bitindex))[bitindex]
_balance_categories(widgets::NamedTuple) = _kept_categories(_category_bitindex(widgets.keep_toggles))

function _category_bitindex(toggles::Vector)
    map(t -> t.active[], toggles)
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
    println("Balancing...")
    balanced = Observable(_balance(raw_map, points; category_bitindex=map(getindex, obs.category_balance)))
    known_categories = Observable(_set_categories!(zeros(Int, size(raw_map)), points))
    println("Blurring...")
    blurred = Observable(
        blur(balanced[]; repeat=s.blur_repeat[])
    )
    println("Calculating stripes...")
    stripe = map(_ -> (0.0, 0.0), blurred[]) |> Observable
    println("Calculating texture...")
    std = map(_ -> 0.0, blurred[]) |> Observable
    println("Allocating...")
    categorized = Observable(fill!(similar(raw_map, Int), 0))
    cleaned = Observable(fill!(similar(raw_map, Int), 0))
    filled = Observable(fill!(similar(raw_map, Int), 0))

    rgba_categorized = Observable(fill!(similar(raw_map, RGBA{Float64}), zero(RGBA{Float64})))
    rgba_cleaned = Observable(fill!(similar(raw_map, RGBA{Float64}), zero(RGBA{Float64})))
    rgba_filled = Observable(fill!(similar(raw_map, RGBA{Float64}), zero(RGBA{Float64})))
    rgba_difference = Observable(fill!(similar(raw_map, RGBA{Float64}), zero(RGBA{Float64})))

    (; raw, std, stripe, balanced, known_categories, blurred, categorized, cleaned, filled,
        rgba_categorized, rgba_cleaned, rgba_filled, rgba_difference)
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
    balance_toggles = map(k -> _keep_toggle(fig, k), obs.category_balance)
    name_textboxes = map(k -> _name_textbox(fig, k), obs.category_name)
    # widget_labels = map(["Color";; "Name";; "Tolerance";; "Keep"]) do s
    widget_labels = map(["Color";; "Name";; "Tolerance";; "Keep";; "Balance";;]) do s
        Label(fig, s)
    end
    grid[2, 1] = grid!(widget_labels)
    _set_category_rows!(fig, grid, (; color_buttons, name_textboxes, tolerance_sliders, keep_toggles, balance_toggles))

    widgets = (; color_buttons, name_textboxes, tolerance_sliders, keep_toggles, balance_toggles, clear_button, color_number, plus_button)
    return grid, widgets
end

function _set_category_rows!(fig, grid, category_widgets)
    # widgets = category_widgets[(:color_buttons, :name_textboxes, :tolerance_sliders, :keep_toggles)]
    # widget_rows = map(widgets...) do b, tb, s, ktg
        # [b;; tb;; s;; ktg;;]
    widgets = category_widgets[(:color_buttons, :name_textboxes, :tolerance_sliders, :keep_toggles, :balance_toggles)]
    widget_rows = map(widgets...) do b, tb, s, ktg, btg
        [b;; tb;; s;; ktg;; btg;;]
    end
    grid[3:10, 1] = grid!(vcat(widget_rows...), default_rowgap=0)
end

_color_button(fig, points, source, n) =
    Button(fig; label=string(n), height=21, width=21, buttoncolor=_mean_color(source, points[n]))
_tolerance_slider(fig, t) = Slider(fig; height=21, startvalue=t, width=60, range=-2.0:0.01:20.0, horizontal=true)
_keep_toggle(fig, active) = Toggle(fig; active, height=21)
_name_textbox(fig, name) = Textbox(fig; stored_string=name, height=21, valign=:top, textpadding=(1, 1, 1, 1), cornerradius=1)

function _make_buttons!(fig, obs)
    grid = GridLayout(tellwidth = false)
    grid[1, 1] = balance = Button(fig; label="balance")
    grid[1, 2] = blur = Button(fig; label="smooth")
    grid[1, 3] = categorize = Button(fig; label="categorize")
    grid[1, 4] = clean = Button(fig; label="clean")
    grid[1, 5] = fill = Button(fig; label="fill")
    grid[1, 6] = previous = Button(fig; label="previous")
    grid[1, 7] = next = Button(fig; label="next")
    segment_toggle = Toggle(fig; active=obs.settings.segment)
    polygon_toggle = Toggle(fig; active=Observable(obs.settings.polygon_select_active[]))
    grid[1, 8] = grid!([Label(fig, "Use segmentation");; segment_toggle;;])
    grid[1, 9] = grid!([Label(fig, "Draw Polygons");; polygon_toggle;;])
    buttons = (; balance, blur, categorize, fill, clean, next, previous, segment_toggle, polygon_toggle)
    return grid, buttons
end


function _make_sliders!(fig, obs)
    s = obs.settings

    slidergrid = GridLayout(tellwidth=false, tellheight=false)

    # Blur
    blur_repeat_labelled = labelslider!(fig, "blur iterations", 0:30; height=20, tellwidth=false, sliderkw=(; startvalue=s.blur_repeat))
    slidergrid[1, 1] = blur_repeat_labelled.layout
    T = typeof(blur_repeat_labelled.layout)
    panel_sliders = fill(T[], length(MAPTITLES))
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
    fill_labelled = labelslider!(fig, "gap filling iterations", 0:30; height=20, tellwidth=false, sliderkw=(; startvalue=s.fill_repeat))
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

function _make_axis!(plot_grid, maps, current_panel)
    axis = Axis(plot_grid[1, 1]; alignmode=Inside(), title=MAPTITLES[1])
    axis.aspect = AxisAspect(1)
    plots = map(_plotmaps(maps)) do m
        p = heatmap!(axis, m)
        p.visible[] = false
        p
    end
    plots[1].visible[] = true
    return axis, plots
end

_plotmaps(maps) = map(k -> maps[k], MAPKEYS)

function _on_click_color_button!(color_buttons, point_category, i)
    foreach(b -> b.strokecolor = :transparent, color_buttons)
    color_buttons[i].strokecolor = :black
    point_category[] = i
    notify(point_category)
end

function _connect_observables!(fig, maps, obs, layout, polygons)
    (; axis, buttons, category_widgets, current_panel, panel_sliders, plots, point_category, sliders) = layout
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
    on(maps.cleaned) do cm
        maps.rgba_cleaned[] = _recolor(cm, maps.blurred[], map(getindex, obs.points))
        notify(maps.rgba_cleaned)
    end

    # Category Widgets
    map(enumerate(category_widgets.color_buttons)) do (i, btn)
        on(btn.clicks) do _
            _on_click_color_button!(category_widgets.color_buttons, point_category, i)
        end
    end
    on(category_widgets.plus_button.clicks) do _
        (; color_buttons, name_textboxes, tolerance_sliders, keep_toggles, balance_toggles) = category_widgets
        pointvec = Observable(Point2{Float32}[])
        push!(obs.points, pointvec)
        polygonvec = Observable([[Point2(100.0f0, 100.0f0)]])
        push!(obs.polygons, polygonvec)
        n = obs.settings.ncategories[] = length(obs.points)

        newbutton = _color_button(fig, map(getindex, obs.points), maps.blurred[], n)
        newslider = _tolerance_slider(fig, DEFAULT_CATEGORY_TOLERANCE)
        newkeeptoggle = _keep_toggle(fig, true)
        newbalancetoggle = _keep_toggle(fig, false)
        newtextbox = _name_textbox(fig, " ")

        push!(color_buttons, newbutton)
        push!(tolerance_sliders, newslider)
        push!(name_textboxes, newtextbox)
        push!(keep_toggles, newkeeptoggle)
        push!(balance_toggles, newbalancetoggle)

        on(newbutton.clicks) do _
            _on_click_color_button!(color_buttons, point_category, n)
        end

        point_textplot, point_scatterplot = on_point(n, pointvec, category_widgets, maps, axis)

        real_poly, poly_shapes, poly_nodes = on_polygon(n, polygonvec, category_widgets, maps, axis)
        push!(real_polygons, real_poly)

        _set_category_rows!(fig, layout.grids.category, category_widgets)
    end
    on(category_widgets.clear_button.clicks) do _
        pointvec = obs.points[point_category[]]
        deleteat!(pointvec[], eachindex(pointvec[]))
        notify(pointvec)
    end

    # Buttons
    on(buttons.polygon_toggle.active) do x
        obs.settings.point_select_active[] = !x
        obs.settings.polygon_select_active[] = x
        # notify(obs.settings.polygon_select_active)
        # notify(obs.settings.point_select_active)
    end
    on(buttons.previous.clicks) do _
        _previous_panel(layout, obs)
    end
    on(buttons.next.clicks) do _
        _next_panel(layout, obs)
    end
    on(buttons.balance.clicks) do _
        maps.balanced[] = _balance(maps.raw[], map(getindex, obs.points);
            category_bitindex=_category_bitindex(category_widgets.keep_toggles)
        )
        _update_panel(layout, Int(balance_panel))
        notify(maps.balanced)
    end
    on(buttons.blur.clicks) do _
        maps.blurred[] = blur(maps.balanced[]; repeat=sliders.blur_repeat.value[])
        if obs.settings.match[2][] 
            maps.std[] = _stds(maps.blurred[])
        end
        if obs.settings.match[3][] 
            maps.stripe[] = _stripes(maps.blurred[], radius=sliders.stripe_radius.value[])
        end
        _update_panel(layout, Int(blur_panel))
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
            _update_panel(layout, Int(categorize_panel))
            notify(maps.categorized)
        end
    end
    on(buttons.clean.clicks) do _
        mask = _polymask(map(getindex, real_polygons), maps.categorized[])
        masked = (1 .- mask) .* maps.categorized[]
        maps.cleaned[] = _clean(masked, maps.known_categories[];
            categories=_kept_categories(category_widgets),
            line_threshold=sliders.line_threshold.value[],
            prune_threshold=sliders.prune_threshold.value[],
        )
        _update_panel(layout, Int(clean_panel))
        notify(maps.cleaned)
    end
    on(buttons.fill.clicks) do _
        polys = map(getindex, real_polygons)
        mask = _polymask(polys, maps.categorized[])
        maps.filled[] = _fill(maps, layout, obs, mask)
        fill_raster = Raster(maps.filled[], dims(mask))
        map(enumerate(polys)) do (i, p)
            rasterize!(fill_raster, p; fill=i, shape=:polygon)
        end
        _update_panel(layout, Int(fill_panel))
        notify(maps.filled)
    end

    # Other observables
    on(point_category) do n
        category_widgets.color_number.text[] = string(n)
        notify(category_widgets.color_number.text)
    end
    point_text_scatter_plots = map(enumerate(obs.points)) do (pc, pv)
        on_point(pc, pv, category_widgets, maps, axis)
    end

    # Add point input to maps
    dragselect!(fig, axis, obs.points;
        section=point_category,
        active=obs.settings.point_select_active,
    )

    map_stages = map((:raw, :balanced, :blurred, :categorized, :cleaned, :filled, :rgba_difference, :filled)) do key
        maps[key]
    end

    hover_report!(layout.hover_label.text, fig, axis, map_stages, current_panel)


    # Why is this type so ridiculous, GeometryBasics?
    T = Polygon{2, Float32, Point2{Float32}, LineString{2, Float32, Point2{Float32}, Base.ReinterpretArray{Line{2, Float32}, 1, Tuple{Point2{Float32}, Point2{Float32}}, TupleView{Tuple{Point2{Float32}, Point2{Float32}}, 2, 1, Vector{Point2{Float32}}}, false}}, Vector{LineString{2, Float32, Point2{Float32}, Base.ReinterpretArray{Line{2, Float32}, 1, Tuple{Point2{Float32}, Point2{Float32}}, TupleView{Tuple{Point2{Float32}, Point2{Float32}}, 2, 1, Vector{Point2{Float32}}}, false}}}}
    real_polygons = map(enumerate(obs.polygons)) do (i, ps)
        real_poly, poly_shapes, poly_nodes = 
            on_polygon(i, ps, category_widgets, maps, axis)
        return real_poly
    end
    dragselect_polygon!(fig, axis, obs.polygons;
        section=point_category,
        active=obs.settings.polygon_select_active,
    )

    # Keyboard arrow movement
    scale = lift(axis.finallimits) do fl
        round(Int, maximum(fl.widths) / 10)
    end
    on(events(fig).keyboardbutton) do event
        event.action == Makie.Keyboard.press || return Consume(false)
        s = scale[]
        if event.key == Keyboard.right
            _moveaxis(axis, (s, 0))
        elseif event.key == Keyboard.up
            _moveaxis(axis, (0, s))
        elseif event.key == Keyboard.left
            _moveaxis(axis, (-s, 0))
        elseif event.key == Keyboard.down
            _moveaxis(axis, (0, -s))
        elseif event.key == Keyboard.tab
            _is_shift_pressed(fig) ? _previous_panel(layout, obs) : _next_panel(layout, obs)
        end
        # Let the event reach other listeners
        return Consume(false)
    end
end

function _polymask(polygons, A)
    ax = axes(A)
    raster = Raster(A, (X(ax[1]), Y(ax[2])))
    return boolmask(polygons; to=raster, shape=:polygon)
end

function _is_shift_pressed(fig)
    pressed = events(fig).keyboardstate
    Makie.Keyboard.left_shift in pressed || Makie.Keyboard.right_shift in pressed
end

function _previous_panel(layout, obs)
    if layout.current_panel[] > 1
        n = layout.current_panel[] - 1
        _update_panel(layout, n)
    end
end

function _next_panel(layout, obs)
    if layout.current_panel[] < length(MAPTITLES)
        n = layout.current_panel[] + 1
        _update_panel(layout, n)
    end
end

_update_panel(l, n) = _update_panel(l.axis, l.plots, l.panel_sliders, l.current_panel, n)
function _update_panel(axis, plots, panel_sliders, current_panel, n)
    current_panel[] = n
    axis.title[] = MAPTITLES[n]
    for p in plots
        p.visible[] = false
    end
    plots[n].visible[] = true
    for (i, p) in enumerate(panel_sliders), s in p
        s.halign[] = (i == n ? :left : :right)
        s.width[] = (i == n ? 600 : 0)
    end
end

function on_point(point_category::Int, pv::Observable, category_widgets, maps, axis)
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
    textplot = Makie.text!(axis, labels; 
        position=pv,
        textsize=12,
        color=(:black, 0.5),
    )
    scatterplot = Makie.scatter!(axis, pv;
        color=(point_color, 0.8),
        strokecolor=(:black, 0.5),
        strokewidth=1,
        markersize=8,
    )
    return textplot, scatterplot
end

function on_polygon(polygon_category::Int, point_polygons, category_widgets, maps, axis)
    real_polygons = lift(point_polygons) do ps
        if length(ps) == 0
            [Polygon([Point2(100.0f0, 100.0f0)])]
        else
            map(ps) do p
                Polygon(p)
            end
        end
    end
    all_points = lift(point_polygons) do ps
        if length(ps) > 1 
            reduce(vcat, ps)
        else
            [Point2(100.0f0, 100.0f0)]
        end
    end
    poly_shapes = poly!(axis, real_polygons)
    poly_nodes = scatter!(axis, all_points;
        markersize=8,
    )
    return real_polygons, poly_shapes, poly_nodes
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

function _fill(maps, layout, obs, mask)
    values = maps.cleaned[]
    known = maps.known_categories[]
    points = map(getindex, obs.points)
    categories = _kept_categories(layout.category_widgets)
    original = PixelProp.(HSL.(maps.blurred[]), maps.std[], maps.stripe[], maps.known_categories[])
    stats = _component_stats(original, points[categories])
    repeat = layout.sliders.fill_repeat.value[]
    match = _match_from_bools(map(m -> m.active[], layout.match_toggles))
    for i in 1:repeat
        values = fill_gaps(values;
            mask, known, stats, categories, original, match, missingval=0,
        )
    end
    return values
    # return _shrink_grow(values, 7, 9)
end

function _mean_color(A::AbstractMatrix{C}, pointvec::AbstractVector) where C<:Colorant
    stats = _component_stats(A, pointvec, 0)
    return ismissing(stats) ? C(zero(RGB(first(A)))) : C(map(s -> s.mean, stats)...)
end

function _stds(A)
    stds = Neighborhoods.broadcast_neighborhood(Window(2), A) do hood
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
