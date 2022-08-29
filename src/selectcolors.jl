function _plot_output!(ax, A::AbstractArray{C}, points) where C
    cats = collect(skipmissing(RasterUtils._calc_category_stats(A, points)))
    _plot_output!(ax, A, points, cats)
end

function _plot_output!(ax, A::AbstractArray{C}, points, cats) where C
    if length(cats) > 0
        meancolors = map(cats) do cat
            C(map(x -> x.mean, cat)...)
        end
        filt = map(A) do x
            c = RasterUtils._categorisecolor(x, cats)
            c == 0 ? RGBA(0.0, 0.0, 0.0, 0.0) : RGBA(meancolors[c])
        end
        heatmap!(ax, filt; opacity=0.5)
    end
end

function selectcolors(A::AbstractArray{C}; 
    ncolors=1, points=[Point2{Float32}[] for _ in 1:ncolors], kw...
) where C
    A = HSV.(A)
    # Figure
    fig = Figure()
    ax1 = Axis(fig[1, 1]; title="Source")
    ax2 = Axis(fig[1, 2]; title="Output")
    linkaxes!(ax1, ax2)
    ax1.aspect = ax2.aspect = AxisAspect(1)
    heatmap!(ax1, A)
    _plot_output!(ax2, A, points)
    fig[2, 1] = buttongrid = GridLayout(tellwidth = false)
    # Buttons
    section = Observable(1)
    buttongrid[1, 1] = color_number = Label(fig, "1")
    buttongrid[1, 2] = previous = Button(fig, label="previous")
    buttongrid[1, 3] = next = Button(fig, label="next")
    buttongrid[1, 4] = update = Button(fig, label="update")

    positions = map(Observable, points)
    on(update.clicks) do _
        _plot_output!(ax2, A, map(getindex, positions))
    end
    _plot_output!(ax2, A, map(getindex, positions))

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
    screen = display(fig)
    # Points
    map(enumerate(positions)) do (section, ps)
        sectioncolor = lift(ps) do psvec
            colors = map(psvec) do P
                I = map(P) do p 
                    round(Int, p)
                end
                checkbounds(Bool, A, I...) ? RGB(A[I...]) : missing
            end |> skipmissing |> collect
            length(colors) > 0 ? mean(colors) : one(RGB(first(A)))
        end
        labels = lift(p -> [string(section) for _ in 1:length(p)], ps)
        map((ax1, ax2)) do ax
            Makie.text!(ax, labels; position=ps)
            Makie.scatter!(ax, ps; 
                color=sectioncolor, 
                strokecolor=:black,
                strokewidth=1,
            )
        end
    end
    dragselect!(fig, ax1, positions, size(A); section, kw...)
    println("Select points in rasters, then close the window")
    while screen.window_open[] 
        sleep(0.1)
    end
    return map(getindex, positions)
end

function colors2categories(A::AbstractArray{C}; removepoints, regionpoints, kw...) where C
    toremove = _calc_category_stats(A, removepoints)
    tokeep = _calc_category_stats(A, regionpoints)
    broadcast(A) do a 
        x = _categorisecolor(a, toremove)
        # If we didn't remove it, classify it
        x == 0 ? _categorise(a, tokeep) : 0
    end
end

function _categorisecolor(x::C, categories; error=2.0) where C
    nt = _asnamedtuple(x)
    errs = map(categories) do category
        map(nt, category) do val, catstats
            (val - catstats.mean)^2
        end
    end
    _, best = findmin(errs)
    # return best
    isincategory = all(map(nt, categories[best]) do val, stats
        (val >= stats.min - stats.sd * error) && (val <= stats.max + stats.sd * error)
    end)
    return isincategory ? best : 0
end

function _calc_category_stats(A, pointvecs)
    map(pointvecs) do pointvec
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
            (mean=mean(c), min=minimum(c), max=maximum(c), sd=std(c))
        end
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
