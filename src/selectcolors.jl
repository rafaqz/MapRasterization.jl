const DEFAULT_CATEGORY_TOLERANCE = 2.0

struct MapSelection{S<:NamedTuple,O}
    settings::S
    removals::Vector{Vector{Point2{Float32}}}
    points::Vector{Vector{Point2{Float32}}}
    output::O
end

function selectcolors(raw_map::AbstractArray, map_selection)
    points = map_selection.points
    selectcolors(raw_map; points, map_selection.settings...)
end
function selectcolors(raw_map::AbstractArray{C};
    ncategories=5,
    match=(:color, :std, :stripe),
    points=[Point2{Float32}[] for _ in 1:ncategories],
    scan_threshold=0.001,
    prune_threshold=0,
    stripe_radius=2,
    line_threshold=0.2,
    blur_repeat=1,
    clean_repeat=1,
    category_tolerances=[DEFAULT_CATEGORY_TOLERANCE for _ in 1:length(points)],
    category_keep=[true for _ in 1:length(points)],
    kw...
) where C
    ncategories = Observable(length(points))
    # Interactive point editing
    points_obs = map(Observable, points)
    match_obs = Observable{Val}(Val{(map(Symbol, match)...,)}())
    maps = _make_stages(raw_map, points; scan_threshold, match=match_obs[], blur_repeat, stripe_radius, line_threshold, prune_threshold)
    section = Observable(1)
    current_panel = Observable(1)

    fig = Figure()
    fig[1, 1:3] = panel = GridLayout(tellwidth = false)
    # Top buttons
    panel[1, 1:2], buttons = _make_buttons!(fig)
    # Pixel property match toggles
    panel[2:3, 1], toggles = _make_toggles!(fig, match) 
    # Constrol sliders
    panel[2:3, 2], sliders = _make_sliders!(fig;
        blur_repeat, scan_threshold, clean_repeat, line_threshold, prune_threshold, stripe_radius
    )
    panel[4:10, 1:2] = plotgrid = GridLayout(tellheight = false, tellwidth = false)
    # Maps
    axes = _make_axes!(plotgrid, maps; ncategories=ncategories[])


    # Categories
    category_grid, category_widgets = _make_category_widgets!(fig, maps, points; 
        ncategories=ncategories[], category_keep, category_tolerances
    )
    fig[1, 4] = category_grid

    screen = display(fig)

    # Join everything together
    _connect_observables!(fig, maps, panel, axes, sliders, buttons, category_grid, category_widgets, points_obs, match_obs, toggles, section, current_panel; ncategories)

    # Add point input to maps
    map(axes) do ax
        dragselect!(fig, ax, points_obs, size(raw_map); section)#, kw...)
    end

    # Wait for user input
    while screen.window_open[]
        sleep(0.1)
    end

    # Return results
    removals = Vector{Point2{Float32}}[]
    points = map(getindex, points_obs)
    settings = (
        scan_threshold=sliders.scan_threshold.value[],
        prune_threshold=sliders.prune_threshold.value[],
        line_threshold=sliders.line_threshold.value[],
        blur_repeat=sliders.blur_repeat.value[],
        category_tolerances=map(t -> t.value[], category_widgets.tolerance_sliders),
        category_keep=map(t -> t.active[], category_widgets.keep_toggles),
        clean_repeat=sliders.clean_repeat.value[],
        stripe_radius=sliders.stripe_radius.value[],
        match=_unwrap(match_obs[]),
    )

    return MapSelection(settings, removals, points, maps.cleaned[])
end

function _make_toggles!(fig, match) 
    toggles = [Toggle(fig; active) for active in _bools_from_match(match)]
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

function _make_stages(raw, points; 
    scan_threshold, match, blur_repeat, stripe_radius, line_threshold, prune_threshold,
)
    balanced = Observable(_balance(raw, points))
    known_categories = Observable(_set_categories!(zeros(Int, size(raw)), points))
    blurred = Observable(
        blur(balanced[]; repeat=blur_repeat)
    )
    std = Observable(_stds(blurred[]))
    stripe = Observable(_stripes(blurred[], radius=stripe_radius))
    segments = Observable(fast_scanning(PixelProp.(blurred[], std[], stripe[], known_categories[]), scan_threshold; match))
    segmented = Observable(map(i -> IS.segment_mean(segments[], i), IS.labels_map(segments[])))
    categorized = Observable(fill!(similar(raw, Int), 0))
    lines_removed = Observable(fill!(similar(raw, Int), 0))
    cleaned = Observable(fill!(similar(raw, Int), 0))

    rgba_segmented = Observable(RGBA.(segmented[]) .* 1.0)
    rgba_categorized = Observable(fill!(similar(raw, RGBA{Float64}), zero(RGBA{Float64})))
    rgba_lines_removed = Observable(fill!(similar(raw, RGBA{Float64}), zero(RGBA{Float64})))
    rgba_cleaned = Observable(fill!(similar(raw, RGBA{Float64}), zero(RGBA{Float64})))
    (; raw, std, stripe, balanced, known_categories, blurred, segmented, segments, categorized, lines_removed, cleaned,
        rgba_segmented, rgba_categorized, rgba_lines_removed, rgba_cleaned)
end

function _make_category_widgets!(fig, maps, points; 
    ncategories, category_keep, category_tolerances
)
    grid = GridLayout(tellheight=false, tellwidth=false)
    grid[1, 1] = controlgrid = GridLayout(tellheight=false, tellwidth=false)

    controlgrid[1, 1] = plus_button = Button(fig, label="+")
    controlgrid[1, 2] = clear_button = Button(fig, label="clear points")
    controlgrid[1, 3] = color_number = Label(fig, "1")

    color_buttons = map(1:ncategories) do n
        _color_button(fig, points, maps.blurred[], n)
    end
    color_buttons[1].strokecolor = :black
    tolerance_sliders = map(t -> _tolerance_slider(fig, t), category_tolerances) 
    keep_toggles = map(k -> _keep_toggle(fig, k), category_keep)
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
    grid[3:10, 1] = grid!(vcat(widget_rows...))
end

_color_button(fig, points, source, n) =
    Button(fig; label=string(n), height=30, width=30, buttoncolor=_mean_color(source, points[n]))
_tolerance_slider(fig, t) = Slider(fig; startvalue=t, width=60, range=-2.0:0.01:20.0, horizontal=true)
_keep_toggle(fig, active) = Toggle(fig; active)

function _make_buttons!(fig)
    grid = GridLayout(tellwidth = false)
    grid[1, 1] = balance = Button(fig, label="balance")
    grid[1, 2] = blur = Button(fig, label="blur")
    grid[1, 3] = scan = Button(fig, label="scan segments")
    grid[1, 4] = categorize = Button(fig, label="categorize")
    grid[1, 5] = remove_lines = Button(fig, label="remove lines")
    grid[1, 6] = clean = Button(fig, label="clean")
    grid[1, 7] = next = Button(fig, label="next")
    grid[1, 8] = previous = Button(fig, label="previous")
    buttons = (; balance, blur, scan, categorize, clean, remove_lines, next, previous)
    return grid, buttons
end

function _make_sliders!(fig;
    blur_repeat, scan_threshold, clean_repeat, line_threshold, prune_threshold, stripe_radius
)
    slidergrid = GridLayout(tellwidth=false, tellheight=false)

    blur_repeat_labelled = labelslider!(fig, "blur iterations", 0:20; height=20, sliderkw=(; startvalue=blur_repeat))
    slidergrid[1, 1] = blur_repeat_labelled.layout

    scan_threshold_labelled = labelslider!(fig, "scan threshold", _aslog(0.00001:0.0001:0.1); height=20, sliderkw=(; startvalue=scan_threshold))
    slidergrid[2, 1] = scan_threshold_labelled.layout

    clean_labelled = labelslider!(fig, "cleaning iterations", 0:20; height=20, sliderkw=(; startvalue=clean_repeat))
    slidergrid[3, 1] = clean_labelled.layout

    line_threshold_labelled = labelslider!(fig, "line threshold", 0.0001:0.001:1.0; height=20, sliderkw=(; startvalue=line_threshold))
    slidergrid[4, 1] = line_threshold_labelled.layout

    prune_threshold_labelled = labelslider!(fig, "prune threshold", 0:500; height=20, sliderkw=(; startvalue=prune_threshold))
    slidergrid[5, 1] = prune_threshold_labelled.layout

    stripe_radius_labelled = labelslider!(fig, "stripe radius", 1:20; height=20, sliderkw=(; startvalue=stripe_radius))
    slidergrid[6, 1] = stripe_radius_labelled.layout
    
    sliders = (
        clean_repeat=clean_labelled.slider,
        scan_threshold=scan_threshold_labelled.slider,
        blur_repeat=blur_repeat_labelled.slider,
        line_threshold=line_threshold_labelled.slider,
        prune_threshold=prune_threshold_labelled.slider,
        stripe_radius=stripe_radius_labelled.slider,
    )

    return slidergrid, sliders
end

function _make_axes!(plotgrid, maps; ncategories)
    # Images
    ax1 = Axis(plotgrid[1, 1]; alignmode=Inside(), title="Source")
    ax2 = Axis(plotgrid[1, 2]; alignmode=Inside(), title="Blurred")
    ax3 = Axis(plotgrid[1, 3]; alignmode=Inside(), title="Segmented")
    ax4 = Axis(plotgrid[2, 1]; alignmode=Inside(), title="Categorized")
    ax5 = Axis(plotgrid[2, 2]; alignmode=Inside(), title="Lines removed")
    ax6 = Axis(plotgrid[2, 3]; alignmode=Inside(), title="Cleaned")
    axes = ax1, ax2, ax3, ax4, ax5, ax6
    linkaxes!(axes...)
    map(ax -> ax.aspect = AxisAspect(1), axes)
    hm1 = heatmap!(ax1, maps.balanced)
    hm2 = heatmap!(ax2, maps.blurred)
    hm3 = heatmap!(ax3, maps.rgba_segmented)
    heatmap!(ax4, maps.rgba_categorized)
    heatmap!(ax5, maps.rgba_lines_removed)
    heatmap!(ax6, maps.rgba_cleaned)
    # heatmap!(ax4, maps.categorized; colormap=:thermal, colorrange=(0, ncategories), alpha=0.2)
    # heatmap!(ax5, maps.cleaned; colormap=:thermal, colorrange=(0, ncategories), alpha=0.2)
    # heatmap!(ax6, maps.rgba_lines_removed)
    return axes
end

function on_click_color_button!(color_buttons, section, i)
    foreach(b -> b.strokecolor = :transparent, color_buttons)
    color_buttons[i].strokecolor = :black
    section[] = i
    notify(section)
end

function _connect_observables!(
    fig, maps, panel, axes, sliders, buttons, category_grid, category_widgets,
    points_obs, match_obs, toggles, section, current_panel;
    ncategories 
)
    # Maps
    on(maps.segmented) do sm
        maps.rgba_segmented[] = RGBA.(sm) .* 1.0
        notify(maps.rgba_segmented)
    end
    onany(maps.categorized) do cm
        maps.rgba_categorized[] = _recolor(cm, maps.blurred[], map(getindex, points_obs))
        notify(maps.rgba_categorized)
    end
    onany(maps.cleaned) do cm
        maps.rgba_cleaned[] = _recolor(cm, maps.blurred[], map(getindex, points_obs))
        notify(maps.rgba_cleaned)
    end
    on(maps.lines_removed) do cm
        maps.rgba_lines_removed[] = _recolor(cm, maps.blurred[], map(getindex, points_obs))
        notify(maps.rgba_lines_removed)
    end

    # Toggles
    map(toggles) do toggle
        on(toggle.active) do _
            xs = map(t -> t.active[], toggles)
            match_obs[] = _match_from_bools(xs)
        end
    end
    # map(enumerate(plot_buttons)) do (i, btn)
    #     on(btn.clicks) do _
    #         figure[2, 1] = axes_[i]
    #     end
    # end

    # Category Widgets
    map(enumerate(category_widgets.color_buttons)) do (i, btn)
        on(btn.clicks) do _
            on_click_color_button!(category_widgets.color_buttons, section, i)
        end
    end
    on(category_widgets.plus_button.clicks) do _
        (; color_buttons, tolerance_sliders, keep_toggles) = category_widgets
        pointvec = Observable(Point2{Float32}[])
        push!(points_obs, pointvec)
        n = ncategories[] = length(points_obs)
        push!(color_buttons, _color_button(fig, map(getindex, points_obs), maps.blurred[], n))
        on(last(color_buttons).clicks) do _
            on_click_color_button!(color_buttons, section, n)
        end
        push!(tolerance_sliders, _tolerance_slider(fig, DEFAULT_CATEGORY_TOLERANCE))
        push!(keep_toggles, _keep_toggle(fig, true))
        on_point(n, pointvec, category_widgets, maps, axes)
        _set_category_rows!(fig, category_grid, color_buttons, tolerance_sliders, keep_toggles)
    end
    on(category_widgets.clear_button.clicks) do _
        pointvec = points_obs[section[]]
        deleteat!(pointvec[], eachindex(pointvec[]))
        notify(pointvec)
    end

    # Buttons
    on(buttons.next.clicks) do _
        if current_panel[] < 6
            delete!(panel)
            current_panel[] += 1
            gride = GridLayout()
            grid[1, 1] = axes[current_panel[]]
            fig[1, 1:3] = grid
        end
    end
    on(buttons.previous.clicks) do _
        if current_panel[] > 1
            delete!(panel)
            current_panel[] -= 1
            fig[1, 1:3] = panels[current_panel[]]
        end
    end
    on(buttons.balance.clicks) do _
        maps.balanced[] = _balance(maps.raw, map(getindex, points_obs))
        notify(maps.balanced)
    end
    on(buttons.blur.clicks) do _
        maps.blurred[] = blur(maps.balanced[]; repeat=sliders.blur_repeat.value[])
        maps.stripe[] = _stripes(maps.blurred[], radius=sliders.stripe_radius.value[])
        maps.std[] = _stds(maps.blurred[])
        notify(maps.blurred)
    end
    on(buttons.scan.clicks) do _
        scan_threshold = sliders.scan_threshold.value[]
        bm = maps.blurred[]
        println("calculating stripes...")
        println("scanning...")
        maps.segments[] = fast_scanning(PixelProp.(bm, maps.std[], maps.stripe[], maps.known_categories[]), sliders.scan_threshold.value[]; match=match_obs[])
        println("plotting...")
        maps.segmented[] = map(i -> IS.segment_mean(maps.segments[], i), IS.labels_map(maps.segments[]))
        notify(maps.segmented)
    end
    on(buttons.categorize.clicks) do _
        points = map(getindex, points_obs)
        if any(p -> length(p) > 0, points)
            maps.categorized[] = _categorize(
                maps, points;
                category_tolerances=map(t -> t.value[], category_widgets.tolerance_sliders),
                match=match_obs[],
            )
            notify(maps.categorized)
        end
    end
    on(buttons.remove_lines.clicks) do _
        maps.lines_removed[] = _remove_lines(maps.categorized[], maps.known_categories[];
            line_threshold=sliders.line_threshold.value[],
            prune_threshold=sliders.prune_threshold.value[],
        )
        notify(maps.lines_removed)
    end
    on(buttons.clean.clicks) do _
        maps.cleaned[] = _clean(maps.lines_removed[];
            points=map(getindex, points_obs),
            repeat=sliders.clean_repeat.value[],
            keep=map(t -> t.active[], category_widgets.keep_toggles),
        )
        notify(maps.cleaned)
    end

    # Other observables
    on(section) do n
        category_widgets.color_number.text[] = string(n)
        notify(category_widgets.color_number.text)
    end
    map(enumerate(points_obs)) do (section, pv)
        on_point(section, pv, category_widgets, maps, axes)
    end
end

function on_point(section, pv, category_widgets, maps, axes)
    on(pv) do pv
        category_widgets.color_buttons[section].buttoncolor[] = _mean_color(maps.balanced[], pv)
        A = maps.known_categories[]
        replace!(A, section => 0)
        _set_categories!(A, pv, section)
    end
    sectioncolor = lift(pv) do pointsvec
        _mean_color(maps.balanced[], pointsvec)
    end
    labels = lift(p -> [string(section) for _ in 1:length(p)], pv)
    map(axes) do ax
        Makie.text!(ax, labels; position=pv)
        Makie.scatter!(ax, pv;
            color=sectioncolor,
            strokecolor=:black,
            strokewidth=1,
        )
    end
end


function _recolor(categories, source, points)
    colors = map(ps -> _mean_color(source, ps), points)
    map(categories) do cat
        cat == 0 ? RGBA(RGB(0), 0) : RGBA(colors[cat])
    end
end

function _bools_from_match(xs)
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

function _categorize(maps, points::Vector;
    category_tolerances, match,
)
    # Categorize
    combined = map(maps.blurred[], maps.segmented[]) do b, s
        PixelProp(b, s.std, s.stripe, s.category)
    end
    category_stats = collect(_component_stats(combined, points))
    _categorize!(combined, maps.segments[], points;
        tolerances=category_tolerances, category_stats, match,
    )
end

function _clean(values::AbstractArray{Int}; 
    keep, points, repeat=5,
    categories=Tuple(i for i in 1:length(points) if keep[i])
)
    known = zeros(Int, size(values))
    foreach(enumerate(points)) do (category, pointvec)
        map(pointvec) do P
            I = _point_index(P)
            known[I...] = category
        end
    end
    for i in 1:repeat
        values = clean_categories(values;
            known,
            categories,
            keep_neigborless=false,
            missingval=0,
            despecle=true,
        )
    end
    return values
end

function _mean_color(A::AbstractMatrix{C}, pointvec::AbstractVector) where C<:Colorant
    stats = _component_stats(A, pointvec, 0)
    return ismissing(stats) ? C(zero(RGB(first(A)))) : C(map(s -> s.mean, stats)...)
end

function _stds(A)
    stds = Neighborhoods.broadcast_neighborhood(Window{2}(), A) do hood, val
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
    newrange = (((2 .^ (x:step(range):y) .- (2^x)) ./ 2^y)) .* (y - x) .+ x
end
