function selectcolors(raw_map::AbstractArray{C}; 
    ncolors=1, 
    points=[Point2{Float32}[] for _ in 1:ncolors], 
    scan_threshold=0.05,
    prune_threshold=0,
    error=2.0,
    kw...
) where C
    ncolors = length(points)
    segments = fast_scanning(raw_map, scan_threshold) |> Observable
    pruned = if prune_threshold > 0
        prune_segments(segments[], 
            i -> (segment_pixel_count(segments[], i) < prune_threshold), 
            (i, j) -> (-segment_pixel_count(segments[], j))
        )
    else
        segments[]
    end |> Observable
    segmented_map = map(i -> segment_mean(pruned[], i), labels_map(pruned[])) |> Observable
    # Figure
    fig = Figure()
    # Buttons
    section = Observable(1)
    fig[1, 1] = buttongrid = GridLayout(tellwidth = false)
    buttongrid[1, 1] = color_number = Label(fig, "1")
    buttongrid[1, 2] = previous = Button(fig, label="previous")
    buttongrid[1, 3] = next = Button(fig, label="next")
    buttongrid[1, 4] = clear = Button(fig, label="clear")
    buttongrid[1, 5] = update = Button(fig, label="update")
    error_labelled = labelslider!(fig, "error", 0.0:0.01:10.0; startvalue=error)
    buttongrid[1, 7] = error_labelled.layout
    error_slider = error_labelled.slider
    scan_threshold_labelled = labelslider!(fig, "scan threshold", 0.001:0.001:0.2; startvalue=scan_threshold)
    buttongrid[1, 6] = scan_threshold_labelled.layout
    scan_threshold_slider = scan_threshold_labelled.slider
    prune_labelled = labelslider!(fig, "prune threshold", 0:200; startvalue=error)
    prune_slider = prune_labelled.slider
    buttongrid[1, 8] = prune_labelled.layout
    # Color boxes
    fig[2, 1] = color_grid = GridLayout(tellwidth = false)
    color_boxes = color_grid[1, 1:ncolors] = map(1:ncolors) do n
        Button(fig; label=string(n), height=20, width=20, buttoncolor=_mean_point_color(segmented_map[], points[n]))
    end

    # Images
    fig[3, 1] = plotgrid = GridLayout(tellwidth = false)
    ax1 = Axis(plotgrid[1, 1]; title="Source")
    ax2 = Axis(plotgrid[1, 2]; title="Segmented")
    ax3 = Axis(plotgrid[1, 3]; title="Output")
    linkaxes!(ax1, ax2, ax3)
    ax1.aspect = ax2.aspect = ax3.aspect = AxisAspect(1)
    heatmap!(ax1, raw_map)
    heatmap!(ax2, segmented_map)
    _plot_output!(ax3, segmented_map[], segments[], points, scan_threshold, error)

    onany(scan_threshold_slider.value, prune_slider.value) do scan_threshold, prune_threshold
        println("scanning...")
        segments[] = fast_scanning(raw_map, scan_threshold)
        println("pruning...")
        pruned[] = if prune_threshold > 0
            prune_segments(segments[], 
                i -> (segment_pixel_count(segments[], i) < prune_threshold), 
                (i, j) -> (-segment_pixel_count(segments[], j))
            )
        else
            segments[]
        end
        println("plotting...")
        segmented_map[] = map(i -> segment_mean(pruned[], i), labels_map(pruned[]))
        notify(segmented_map)
    end

    map(enumerate(color_boxes)) do (i, cb)
        on(cb.clicks) do _
            section[] = i
            notify(section)
        end
    end

    positions = map(Observable, points)
    map(positions, color_boxes) do pv, cb
        on(pv) do pv
            cb.buttoncolor[] = _mean_point_color(segmented_map[], pv)
        end
    end
    on(update.clicks) do _
        _plot_output!(ax3, segmented_map[], pruned[], map(getindex, positions), scan_threshold_slider.value[], error_slider.value[])
    end

    on(section) do n
        color_number.text[] = string(n)
        notify(color_number.text)
    end
    on(previous.clicks) do _
        if section[] > 1
            section[] -= 1
            notify(section)
        end
    end
    on(next.clicks) do _
        if section[] < ncolors
            section[] += 1
            notify(section)
        end
    end
    on(clear.clicks) do _
        pointvec = positions[section[]]
        deleteat!(pointvec[], eachindex(pointvec[]))
        notify(pointvec)
    end
    screen = display(fig)
    # Points
    map(enumerate(positions)) do (section, ps)
        sectioncolor = lift(ps) do pointsvec
            _mean_point_color(raw_map, pointsvec)
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
    dragselect!(fig, ax1, positions, size(raw_map); section, kw...)
    dragselect!(fig, ax2, positions, size(raw_map); section, kw...)
    dragselect!(fig, ax3, positions, size(raw_map); section, kw...)
    println("Select points in rasters, then close the window")
    while screen.window_open[] 
        sleep(0.1)
    end
    return map(getindex, positions)
end

function _plot_output!(ax, A::AbstractArray, segments, points, scan_threshold, error)
    out = if any(map(>(0), length.(points)))
        _segment_filter(A, segments, points, scan_threshold, error)
        # _pixel_filter(A, points, error)
    else
        HSL.((_ -> one(eltype(A))).(A))
    end
    finallimits = ax.finallimits[]
    heatmap!(ax, out)
    ax.finallimits[] = finallimits
    notify(ax.finallimits)
end

function _pixel_filter(A, points, error)
    A = HSL.(A)
    categories = collect(skipmissing(RasterUtils._calc_category_stats(A, points)))
    meancolors = map(categories) do cat
        HSL(map(x -> x.mean, cat)...)
    end
    filt = map(A) do x
        c = RasterUtils._categorisecolor(x, categories)
        c == 0 ? RGB(1.0, 1.0, 1.0) : RGB(meancolors[c])
    end
end

function _segment_filter(A, segments, points, scan_threshold, error)
    A = HSL.(A)
    categories = collect(skipmissing(RasterUtils._calc_category_stats(A, points)))
    display(categories)
    meancolors = map(categories) do ctg
        HSL(map(x -> x.mean, ctg)...)
    end
    ctg_segments = map(enumerate(points)) do (ctg, pointvec)
        map(pointvec) do p
            [begin
                I = map(c -> round(Int, c), p) 
                N = I[1] + i, I[2] + j
                segments.image_indexmap[N...] => ctg
             end for i in -1:1, j in -1:1]
        end |> hcat
    end
    known_segment_categories = Dict(vcat(ctg_segments...))
    @show meancolors
    filt = map(enumerate(A)) do (i, x)
        seg = segments.image_indexmap[i]
        ctg = get(known_segment_categories, seg, 0)
        ctg = ctg == 0 ? RasterUtils._categorisecolor(x, categories; error) : ctg
        ctg == 0 ? RGB(1.0, 1.0, 1.0) : RGB(meancolors[ctg])
    end
end

function _mean_point_color(A::AbstractArray, pointsvec)
    stats = _calc_category_stats(A, pointsvec, 0)
    return ismissing(stats) ? one(RGB(first(A))) : RGB(map(s -> s.mean, stats)...)
end

function _categorisecolor(x::C, categories; error=2.0) where C
    nt = _asnamedtuple(x)
    errs = map(categories) do category
        ((x.h - category.h.mean)/360)^2 + (x.l - category.l.mean)^2# + (x.l - category.l.mean)^2 
    end
    _, best = findmin(errs)
    # return best
    isincategory = all(map(nt[(:h, :s)], categories[best][(:h, :s)]) do val, stats
        (val >= stats.min - stats.sd * error) && (val <= stats.max + stats.sd * error)
    end)
    return isincategory ? best : 0
end

function _calc_category_stats(A, pointvecs::AbstractArray{<:AbstractArray})
    _calc_category_stats.(Ref(A), pointvecs, eachindex(pointvecs))
end
function _calc_category_stats(A, pointvec::AbstractArray, cat::Int)
    length(pointvec) == 0 && return missing
    colors = map(pointvec) do P
        I = map(p -> round(Int, p), P)
        A[I...]
    end
    color_components = map(_keysnamedtuple(first(colors))) do k
        [getfield(c, k) for c in colors]
    end
    # Ouputput a NamedTuple of NamedTuple, like:
    # (r=(mean=?, min=?, max=?, sd=?, g=(mean=?...
    map(color_components) do c
        (; category=cat, mean=mean(c), min=minimum(c), max=maximum(c), sd=std(c))
    end
end

function _asnamedtuple(x::T) where T
    fnames = fieldnames(T)
    NamedTuple{fnames}(map(fn -> getfield(x, fn), fnames))
end
# @generated function _asnamedtuple2(x)
#     expr = Expr(:tuple)
#     fnames = fieldnames(x)
#     for nm in fnames
#         push!(expr.args, Expr(:., x, nm))
#     end
#     :(NamedTuple{$(QuoteNode(fnames))}($expr))
#     expr
# end

_keysnamedtuple(nt::NamedTuple) = NamedTuple{keys(nt)}(keys(nt))
_keysnamedtuple(x) = _keysnamedtuple(_asnamedtuple(x))

function _fitgradient(points)
    x_model = lm(@formula(x_unknown ~ x_known + y_known), points)
    y_model = lm(@formula(y_unknown ~ y_known + x_known), points)
    return x_model, y_model
end
