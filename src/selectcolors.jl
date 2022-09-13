struct MapSelection{S<:NamedTuple,O}
    settings::S
    removals::Vector{Vector{Point2{Float32}}}
    points::Vector{Vector{Point2{Float32}}}
    output::O
end

function selectcolors(raw_map::AbstractArray, map_selection)
    points = map_selection.points
    removals = map_selection.removals
    selectcolors(raw_map; points, removals, map_selection.settings...)
end
function selectcolors(raw_map::AbstractArray{C};
    ncategories=5,
    nremovals=5,
    match=(:color, :std, :stripe),
    points=[Point2{Float32}[] for _ in 1:ncategories],
    removals=[Point2{Float32}[] for _ in 1:nremovals],
    scan_threshold=0.001,
    prune_threshold=0,
    stripe_radius=5,
    line_threshold=1.0,
    blur_repeat=1,
    clean_repeat=0,
    line_tolerance=2.0,
    category_tolerances=[2.0 for _ in 1:ncategories],
    kw...
) where C
    match_obs = Observable{Val}(Val{(map(Symbol, match)...,)}())
    prune_threshold=0
    ncategories = length(points)
    nremovals = length(removals)

    # Output stages
    std_map = _stds(raw_map)
    balanced_map = Observable(_balance(raw_map, points))
    known_category_map = Observable(_set_categories!(zeros(Int, size(raw_map)), points))
    line_removed_map = Observable(_remove_lines(balanced_map[], removals;
        radius=stripe_radius, threshold=line_threshold, tolerance=line_tolerance,
    ))
    blurred_map = Observable(
        blur(line_removed_map[], std_map; 
            threshold=line_threshold, repeat=blur_repeat,
        )
    )
    stripe_map = Observable(_stripes(blurred_map[], radius=stripe_radius))
    segments = Observable{Any}(fast_scanning(PixelProp.(blurred_map[], std_map, stripe_map[], known_category_map[]), scan_threshold; match=match_obs[]))
    pruned = Observable{Any}(_maybe_prune(segments[], prune_threshold))
    segmented_map = Observable(map(i -> IS.segment_mean(pruned[], i), IS.labels_map(pruned[])))
    rgba_segmented_map = Observable(RGBA.(segmented_map[]) .* 1.0)

    categorized_map = Observable(similar(raw_map, Int))
    cleaned_map = Observable(similar(raw_map, Int))

    on(segmented_map) do sm
        A = RGBA.(sm) .* 1.0
        @show typeof(A)
        rgba_segmented_map[] = A
        notify(rgba_segmented_map)
    end

    # Figure
    fig = Figure()

    # Buttons
    section = Observable(1)

    fig[1, 1] = panel = GridLayout(tellwidth = false)

    panel[1, 1] = widgetgrid = GridLayout(tellwidth = false)

    # widgetgrid[1, 2] = previous = Button(fig, label="previous")
    # widgetgrid[1, 3] = next = Button(fig, label="next")
    widgetgrid[1, 2] = balance_btn = Button(fig, label="balance")
    widgetgrid[1, 3] = remove_lines_btn = Button(fig, label="remove lines")
    widgetgrid[1, 4] = blur_btn = Button(fig, label="blur")
    widgetgrid[1, 5] = scan_btn = Button(fig, label="scan segments")
    widgetgrid[1, 6] = categorize_btn = Button(fig, label="categorize")
    widgetgrid[1, 7] = clean_btn = Button(fig, label="clean")
    widgetgrid[1, 9] = clear_btn = Button(fig, label="clear points")
    widgetgrid[1, 10] = color_number = Label(fig, "1")

    # Sliders
    panel[2, 1] = slidergrid = GridLayout(tellwidth = false)

    line_threshold_labelled = labelslider!(fig, "line threshold", 0.0001:0.001:1.0; sliderkw=(; startvalue=line_threshold))
    line_threshold_slider = line_threshold_labelled.slider
    slidergrid[1, 1] = line_threshold_labelled.layout

    line_tolerance_labelled = labelslider!(fig, "line tolerance", 0.0:0.01:20.0; sliderkw=(; startvalue=line_tolerance))
    slidergrid[2, 1] = line_tolerance_labelled.layout
    line_tolerance_slider = line_tolerance_labelled.slider

    blur_repeat_labelled = labelslider!(fig, "blur iterations", 0:20; sliderkw=(; startvalue=blur_repeat))
    slidergrid[3, 1] = blur_repeat_labelled.layout
    blur_repeat_slider = blur_repeat_labelled.slider

    scan_threshold_labelled = labelslider!(fig, "scan threshold", _aslog(0.00001:0.0001:0.1); sliderkw=(; startvalue=scan_threshold))
    slidergrid[4, 1] = scan_threshold_labelled.layout
    scan_threshold_slider = scan_threshold_labelled.slider

    prune_labelled = labelslider!(fig, "prune threshold", 0:500; startvalue=prune_threshold)
    prune_slider = prune_labelled.slider
    slidergrid[5, 1] = prune_labelled.layout

    clean_labelled = labelslider!(fig, "cleaning iterations", 0:20; startvalue=clean_repeat)
    clean_slider = clean_labelled.slider
    slidergrid[6, 1] = clean_labelled.layout

    toggles = [Toggle(fig; active) for active in [true, true, true]]
    labels = [Label(fig, lift(x -> x ? "$l" : "$l", t.active))
        for (t, l) in zip(toggles, ["color", "texture", "stripe"])]

    panel[1, 2] = grid!(permutedims(hcat(map(xs -> (g = GridLayout(tellwidth=false); g[1, 1] = xs[1]; g[1, 2] = xs[2]; g), zip(toggles, labels)))), tellheight=false)
    # panel[1, 3] = grid!(hcat(toggles, labels), tellheight=false)

    # Color boxes
    # panel[1, 3] = remove_grid = GridLayout(tellwidth = false)
    # remove_buttons = remove_grid[1, 1:nremovals] = map(1:nremovals) do n
        # Button(fig; label=string(n), height=30, width=30, buttoncolor=_mean_color(balanced_map[], removals[n]))
    # end
    panel[2, 2] = color_grid = GridLayout(tellwidth = false)
    color_buttons = color_grid[1, 1:ncategories] = map(1:ncategories) do n
        Button(fig; label=string(n), height=30, width=30, buttoncolor=_mean_color(balanced_map[], points[n]))
    end
    category_tolerance_sliders = color_grid[2, 1:ncategories] = map(category_tolerances) do t
        Slider(fig; startvalue=t, range=-2.0:0.01:20.0, height=100, width=30, horizontal=false)
    end

    # Images
    fig[2, 1] = plotgrid = GridLayout(tellwidth = false)
    ax1 = Axis(plotgrid[1, 1]; title="Source")
    ax2 = Axis(plotgrid[1, 2]; title="Lines removed")
    ax3 = Axis(plotgrid[1, 3]; title="Blurred")
    ax4 = Axis(plotgrid[2, 1]; title="Segmented")
    ax5 = Axis(plotgrid[2, 2]; title="Categorized")
    ax6 = Axis(plotgrid[2, 3]; title="Cleaned")
    axes_ = ax1, ax2, ax3, ax4, ax5, ax6
    linkaxes!(axes_...)
    map(ax -> ax.aspect = AxisAspect(1), axes_)
    heatmap!(ax1, balanced_map)
    heatmap!(ax2, line_removed_map)
    heatmap!(ax3, blurred_map)
    heatmap!(ax4, rgba_segmented_map)
    heatmap!(ax5, categorized_map; colormap=:thermal, colorrange=(0, ncategories))
    heatmap!(ax6, cleaned_map; colormap=:thermal, colorrange=(0, ncategories))

    map(toggles) do toggle
        on(toggle.active) do _
            xs = map(t -> t.active[], toggles)
            match_obs[] = _match_from_bools(xs)
        end
    end

    # map(enumerate(remove_buttons)) do (i, cb)
    #     on(cb.clicks) do _
    #         section[] = i+ncategories
    #         notify(section)
    #     end
    # end
    map(enumerate(color_buttons)) do (i, cb)
        on(cb.clicks) do _
            section[] = i
            notify(section)
        end
    end

    # Button inputs
    on(balance_btn.clicks) do _
        balanced_map[] = _balance(raw_map, map(getindex, points_obs))
        notify(balanced_map)
    end
    on(remove_lines_btn.clicks) do _
        line_removed_map[] = _remove_lines(balanced_map[], map(getindex, points_obs[ncategories+1:end]);
            stds=std_map,
            radius=stripe_radius,
            threshold=line_threshold_slider.value[],
            tolerance=line_tolerance_slider.value[],
        )
        notify(line_removed_map)
    end
    on(blur_btn.clicks) do _
        blurred_map[] = blur(line_removed_map[], std_map; 
            threshold=line_threshold_slider.value[],
            repeat=blur_repeat_slider.value[]
        )
        notify(blurred_map)
    end
    onany(scan_btn.clicks) do _
        scan_threshold = scan_threshold_slider.value[]
        bm = blurred_map[]
        println("scanning...")
        stripe_map[] = _stripes(blurred_map[], radius=stripe_radius)
        segments[] = fast_scanning(PixelProp.(bm, std_map, stripe_map[], known_category_map[]), scan_threshold; match=match_obs[])
        println("plotting...")
        segmented_map[] = map(i -> IS.segment_mean(segments[], i), IS.labels_map(segments[]))
        notify(segmented_map)
    end
    on(categorize_btn.clicks) do _
        points = map(getindex, points_obs[1:ncategories])
        if any(p -> length(p) > 0, points)
            categorized_map[] = _categorize(
                segmented_map[], segments[], points;
                scan_threshold=scan_threshold_slider.value[],
                category_tolerances=map(t -> t.value[], category_tolerance_sliders),
                prune_threshold=prune_slider.value[],
                match=match_obs[],
            )
            notify(categorized_map)
        end
    end
    on(clean_btn.clicks) do _
        cleaned_map[] = _clean(categorized_map[];
            ncategories = ncategories,
            repeat=clean_slider.value[]
        )
        notify(cleaned_map)
    end
    on(clear_btn.clicks) do _
        pointvec = points_obs[section[]]
        deleteat!(pointvec[], eachindex(pointvec[]))
        notify(pointvec)
    end
    on(section) do n
        color_number.text[] = string(n)
        notify(color_number.text)
    end

    screen = display(fig)

    # Interactive point editing
    points_obs = map(Observable, vcat(points, removals))
    map(points_obs, color_buttons) do pv, b
        on(pv) do pv
            b.buttoncolor[] = _mean_color(balanced_map[], pv)
        end
    end
    map(enumerate(points_obs)) do (i, pv)
        on(pv) do pv
            A = known_category_map[]
            replace!(A, i => 0)
            _set_categories!(A, pv, i)
        end
    end
    map(enumerate(points_obs)) do (section, ps)
        sectioncolor = lift(ps) do pointsvec
            _mean_color(balanced_map[], pointsvec)
        end
        labels = lift(p -> [string(section) for _ in 1:length(p)], ps)
        map((ax1, ax2, ax3)) do ax
            Makie.text!(ax, labels; position=ps)
            Makie.scatter!(ax, ps;
                color=sectioncolor,
                strokecolor=:black,
                strokewidth=1,
            )
        end
    end

    map(axes_) do ax
        dragselect!(fig, ax, points_obs, size(raw_map); section, kw...)
    end

    # Wait
    while screen.window_open[]
        sleep(0.1)
    end

    # Results
    removals = map(getindex, points_obs[1+ncategories:nremovals+ncategories])
    points = map(getindex, points_obs[1:ncategories])
    settings = (
        scan_threshold=scan_threshold_slider.value[],
        prune_threshold=prune_slider.value[],
        line_threshold=line_threshold_slider.value[],
        line_tolerance=line_tolerance_slider.value[],
        blur_repeat=blur_repeat_slider.value[],
        category_tolerances=map(t -> t.value[], category_tolerance_sliders),
        match=_unwrap(match_obs[]),
    )

    return MapSelection(settings, removals, points, cleaned_map[])
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

function _categorize(A::AbstractArray, segments, points::Vector; 
    scan_threshold, category_tolerances, prune_threshold, match,
)
    # Categorize
    category_stats = collect(_component_stats(A, points))
    _categorize!(A, segments, points;
        scan_threshold, tolerances=category_tolerances, prune_threshold, category_stats, match,
    )
end

function _clean(values::AbstractArray{Int}; repeat=5, ncategories, categories=ntuple(identity, ncategories))
    @show repeat categories
    for i in 1:repeat
        values = clean_categories(values;
            categories,
            keep_neigborless=false,
            missingval=0,
            despecle=true,
        )
    end
    return values
end

function _maybe_prune(segments, prune_threshold)
    if prune_threshold > 0
        # prune_segments(segments,
        #     i -> (segment_pixel_count(segments[], i) < prune_threshold),
        #     (i, j) -> (-segment_pixel_count(segments[], j))
        # )
        segments
    else
        segments
    end
end

function _mean_color(A::AbstractMatrix{C}, pointvec::AbstractVector) where C<:Colorant
    stats = _component_stats(A, pointvec, 0)
    return ismissing(stats) ? C(zero(RGB(first(A)))) : C(map(s -> s.mean, stats)...)
end

function _stds(raw_map)
    stds = Neighborhoods.broadcast_neighborhood(Window{2}(), raw_map) do hood, val
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
