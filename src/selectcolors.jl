struct MapSelection{S,O}
    settings::S
    removals::Vector{Vector{Point2{Float32}}}
    points::Vector{Vector{Point2{Float32}}}
    output::O
end

function selectcolors(raw_map::AbstractArray, map_selection)
    points = map_selection.points
    removals = map_selection.removals
    (; scan_threshold, prune_threshold, scan_threshold, prune_threshold) = map_selection.settings
    selectcolors(raw_map; points, removals, scan_threshold, prune_threshold)
end
function selectcolors(raw_map::AbstractArray{C}; 
    ncategories=15, 
    nremovals=5, 
    points=[Point2{Float32}[] for _ in 1:ncategories],
    removals=[Point2{Float32}[] for _ in 1:nremovals],
    scan_threshold=0.05,
    prune_threshold=0,
    stripe_radius=5,
    line_threshold=0.01,
    line_tolerance=2.0,
    category_tolerance=2.0,
    kw...
) where C
    ncategories = length(points)
    nremovals = length(removals)

    # Output stages
    std_map = Neighborhoods.broadcast_neighborhood(Window{5}(), raw_map) do hood, val
        rs = std(map(n -> n.r, hood))
        gs = std(map(n -> n.g, hood))
        bs = std(map(n -> n.b, hood))
        sum((rs, gs, bs))
    end
    stripe_map = Observable{AbstractArray}(_stripes(raw_map, radius=stripe_radius))
    balanced_map = Observable{AbstractArray}(_balance((raw_map), points[1]))
    known_category_map = Observable{AbstractArray}(_set_categories!(zeros(Int, size(stripe_map[])), points))
    line_removed_map = Observable{AbstractArray}(_remove_lines(balanced_map[], removals;
        radius=stripe_radius, threshold=line_threshold, tolerance=line_tolerance,
    ))
    segments = Observable{Any}(fast_scanning(PixelProp.(line_removed_map[], std_map, stripe_map[], known_category_map[]), scan_threshold))
    pruned = Observable{Any}(_maybe_prune(segments[], prune_threshold))
    segmented_map = Observable{AbstractArray}(map(i -> IS.segment_mean(pruned[], i), IS.labels_map(pruned[])))
    rgb_segmented_map = Observable{AbstractArray}(RGB.(segmented_map[]))

    output_map = Observable(similar(raw_map, Int))
    categorized_map, output_map = if any(p -> length(p) > 0, points)
        (_update(segmented_map[], segments[], points, scan_threshold, category_tolerance))
    else
        (_ -> RGB(one(eltype(raw_map)))).(raw_map), similar(raw_map, Int) 
    end |> xs -> map(Observable{Any}, xs)
    rgb_categorized_map = Observable{AbstractArray}(RGB.(categorized_map[]))

    on(segmented_map) do sm 
        rgb_segmented_map[] = RGB.(sm)
    end
    on(categorized_map) do cm 
        rgb_categorized_map[] = RGB.(cm)
    end

    # Figure
    fig = Figure()

    # Buttons
    section = Observable(1)
    fig[1, 1] = panel = GridLayout(tellwidth = false)
    panel[1, 1] = widgetgrid = GridLayout(tellwidth = false)
    widgetgrid[1, 1] = color_number = Label(fig, "1")
    # widgetgrid[1, 2] = previous = Button(fig, label="previous")
    # widgetgrid[1, 3] = next = Button(fig, label="next")
    widgetgrid[1, 2] = balance = Button(fig, label="balance")
    widgetgrid[1, 3] = remove_lines = Button(fig, label="remove lines")
    widgetgrid[1, 4] = clear = Button(fig, label="clear")
    widgetgrid[1, 5] = update = Button(fig, label="update")
    
    # Sliders
    panel[2, 1] = slidergrid = GridLayout(tellwidth = false)

    line_threshold_labelled = labelslider!(fig, "line threshold", 0.0001:0.0001:0.1; sliderkw=(; startvalue=line_threshold))
    line_threshold_slider = line_threshold_labelled.slider
    slidergrid[1, 1] = line_threshold_labelled.layout

    category_tolerance_labelled = labelslider!(fig, "category tolerance", -1.0:0.01:10.0; sliderkw=(; startvalue=category_tolerance))
    slidergrid[2, 1] = category_tolerance_labelled.layout
    category_tolerance_slider = category_tolerance_labelled.slider

    line_tolerance_labelled = labelslider!(fig, "line tolerance", 0.0:0.01:10.0; sliderkw=(; startvalue=line_tolerance))
    slidergrid[3, 1] = line_tolerance_labelled.layout
    line_tolerance_slider = line_tolerance_labelled.slider

    scan_threshold_labelled = labelslider!(fig, "scan threshold", 0.001:0.001:0.2; sliderkw=(; startvalue=scan_threshold))
    slidergrid[4, 1] = scan_threshold_labelled.layout
    scan_threshold_slider = scan_threshold_labelled.slider

    prune_labelled = labelslider!(fig, "prune threshold", 0:200; startvalue=prune_threshold)
    prune_slider = prune_labelled.slider
    slidergrid[5, 1] = prune_labelled.layout

    # Color boxes
    panel[1, 2] = remove_grid = GridLayout(tellwidth = false)
    remove_buttons = remove_grid[1, 1:nremovals] = map(1:nremovals) do n
        Button(fig; label=string(n), height=30, width=30, buttoncolor=_mean_color(balanced_map[], removals[n]))
    end
    panel[2, 2] = color_grid = GridLayout(tellwidth = false)
    color_buttons = color_grid[1, 1:ncategories] = map(1:ncategories) do n
        Button(fig; label=string(n), height=30, width=30, buttoncolor=_mean_color(balanced_map[], points[n]))
    end

    # Images
    fig[2, 1] = plotgrid = GridLayout(tellwidth = false)
    ax1 = Axis(plotgrid[1, 1]; title="Source")
    ax2 = Axis(plotgrid[1, 2]; title="Lines removed")
    ax3 = Axis(plotgrid[2, 1]; title="Segmented")
    ax4 = Axis(plotgrid[2, 2]; title="Output")
    linkaxes!(ax1, ax2, ax3, ax4)
    ax1.aspect = ax2.aspect = ax3.aspect = ax4.aspect = AxisAspect(1)
    heatmap!(ax1, balanced_map)
    heatmap!(ax2, line_removed_map)
    heatmap!(ax3, rgb_segmented_map)
    heatmap!(ax4, output_map)

    onany(scan_threshold_slider.value, prune_slider.value, line_removed_map) do scan_threshold, prune_threshold, bm
        println("scanning...")
        segments[] = fast_scanning(PixelProp.(bm, std_map, stripe_map[], known_category_map[]), scan_threshold)
        println("pruning...")
        pruned[] = _maybe_prune(segments[], prune_threshold)
        println("plotting...")
        segmented_map[] = map(i -> IS.segment_mean(pruned[], i), IS.labels_map(pruned[]))
        notify(segmented_map)
    end

    map(enumerate(remove_buttons)) do (i, cb)
        on(cb.clicks) do _
            section[] = i+ncategories
            notify(section)
        end
    end
    map(enumerate(color_buttons)) do (i, cb)
        on(cb.clicks) do _
            section[] = i
            notify(section)
        end
    end

    # Button inputs
    on(balance.clicks) do _
        balanced_map[] = _balance(raw_map, points_obs[1][])
        notify(balanced_map)
    end
    on(remove_lines.clicks) do _
        line_removed_map[] = _remove_lines(balanced_map[], map(getindex, points_obs[ncategories+1:end]);
            radius=stripe_radius, threshold=line_threshold_slider.value[], tolerance=line_tolerance_slider.value[],
        )
        notify(line_removed_map)
    end
    on(update.clicks) do _
        categorized_map[], output_map[] = _update(
            segmented_map[], segments[], map(getindex, points_obs[1:ncategories]), 
            scan_threshold_slider.value[], category_tolerance_slider.value[]
        )
        notify(categorized_map)
    end
    on(section) do n
        color_number.text[] = string(n)
        notify(color_number.text)
    end
    on(clear.clicks) do _
        pointvec = points_obs[section[]]
        deleteat!(pointvec[], eachindex(pointvec[]))
        notify(pointvec)
    end

    screen = display(fig)

    # Interactive point editing
    points_obs = map(Observable, vcat(points, removals))
    map(points_obs, vcat(color_buttons, remove_buttons)) do pv, b
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

    dragselect!(fig, ax1, points_obs, size(raw_map); section, kw...)
    dragselect!(fig, ax2, points_obs, size(raw_map); section, kw...)
    dragselect!(fig, ax3, points_obs, size(raw_map); section, kw...)
    dragselect!(fig, ax4, points_obs, size(raw_map); section, kw...)

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
        line_tolerance=line_tolerance_slider.value[],
        category_tolerance=category_tolerance_slider.value[],
    )

    return MapSelection(settings, removals, points, output_map[])
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

function _update(A::AbstractArray, segments, points, scan_threshold, category_tolerance)
    pixels, values = _categorize(A, segments, points, scan_threshold, category_tolerance)
    # category_colors = (map(p -> _mean_color(pixels, p), points)...,)
    # missingval = RGBA(RGB(oneunit(first(pixels))), 0)
    # cleanedcolors = clean_categories(pixels; 
    #     category_colors, keep_neigborless=true, missingval
    # )
    pixels, values
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
