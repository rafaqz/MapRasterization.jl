function manualwarp(As...; template::Raster, points=nothing, missingval=missing)
    points = select_common_points(As; template, points, missingval)
    applywarp(As...; template, points, missingval)
end

function applywarp(As::RasterStack; template, points=nothing, missingval=missing)
    As = map(A -> reorder(A, ForwardOrdered), As)
    template = reorder(template, ForwardOrdered)
    warped = _warp_from_points(As, template, points, missingval)
    return warped
end
function applywarp(As...; template, points=nothing, missingval=missing)
    A1 = first(As)
    if A1 isa Raster
        As = map(A -> reorder(A, ForwardOrdered), As)
    end
    template = reorder(template, ForwardOrdered)
    warped = _warp_from_points(As, template, points, missingval)
    # Show updated heatmap
    # display(Makie.heatmap(map(parent, dims(first(warped)))..., parent(first(warped))))
    if length(warped) == 1
        return first(warped)
    else
        return warped
    end
end

function _warp_from_points(As::RasterStack, template, points, missingval)
    rasters = _warp_from_points(values(As), template, points, missingval)
    RasterStack(rasters)
end
function _warp_from_points(As::Tuple, template, points, missingval)
    models = _fitlinearmodels(points)
    return map(A -> linearwarp(A; template, models, missingval), As)
end


"""
    select_common_points(A; template, kw...)

Select points in an image and a spatial Raster
in order to snap the image to the rasters coordinates.
"""
select_common_points(A; template, kw...) = _select_common_points(A, template; kw...)

_select_common_points(A, template::Raster; kw...) = 
    _select_common_points(A, parent(reorder(template, ForwardOrdered)); kw...)
_select_common_points(A::Raster, template; kw...) = 
    _select_common_points(parent(reorder(A, ForwardOrdered)), template; kw...)
_select_common_points(A::Raster, template::Raster; kw...) = 
    _select_common_points(parent(reorder(A, ForwardOrdered)), template; kw...)
function _select_common_points(A, template::AbstractArray; points=nothing, missingval)
    # map(A -> size(A) == size(first(As)), As) || throw(ArgumentError("Intput raster sizes are not the same"))
    fig = Figure()
    ax1 = Makie.Axis(fig[1,1]; title="Source raster with known crs/resolution - `template` kw")
    ax2 = Makie.Axis(fig[1,2]; title="First raster with unknown crs/resolution")
    ax1.aspect = ax2.aspect = Makie.AxisAspect(1)
    dragging1 = Ref(false)
    dragging2 = Ref(false)
    knownpoints, unknownpoints = if !isnothing(points) && Tables.rowcount(points) > 0
        table2points(points)
    else
        Point2{Float32}[], Point2{Float32}[]
    end
    @show knownpoints unknownpoints
    knownpoints = selectmultiple(parent(template), fig, ax1; dragging=dragging1, points=knownpoints)
    unknownpoints = selectmultiple(parent(A), fig, ax2; dragging=dragging2, points=unknownpoints)
    @show knownpoints unknownpoints
    finallimits = Ref{Any}(nothing)
    overlay = nothing
    lift(knownpoints, unknownpoints) do k, u
        (dragging1[] || dragging2[]) && return nothing # Dont update during drag
        len = min(length(k), length(u))
        (length(k) == length(u) && len >= 3) || return nothing
        points = points2table((known=k[1:len], unknown=u[1:len]))
        warped = linearwarp(A; template, points, missingval)
        finallimits[] = ax1.finallimits
        if !isnothing(overlay)
            delete!(ax1, overlay)
        end
        overlay = _heatmap!(ax1, parent(warped); colormap=(:viridis, 0.2)) 
        ax1.finallimits = finallimits[]
        return nothing
    end
    screen = display(fig)
    println("Select points in rasters, then close the window")
    while screen.window_open[] 
        sleep(0.1)
    end
    length(knownpoints[]) == length(unknownpoints[]) || error("Number of selected points must be the same for each raster")
    return points2table((known=knownpoints[], unknown=unknownpoints[]))
end



"""
    linearwarp(A; template, points, models, missingval)

Warp `A` to match `template`, following the points in `points`
or fitted linear models for x and y in the tuple `models`.

Gaps are filled with `missingval`.
"""
function linearwarp(A; template, points=nothing, models::Union{Nothing,Tuple}=nothing, missingval=missing)
    # @show points size(A) size(template)
    x_model, y_model = if isnothing(models) 
        isnothing(points) && error("pass either `points::Tuple` to fit or fitted `models::Tuple`")
        _fitlinearmodels(points)
    else
        models
    end
    # display(x_model); display(y_model)
    pixelpoints = vec(collect((x_known = x, y_known=y) for (x, y) in Tuple.(CartesianIndices(template))))
    xs = round.(Int, predict(x_model, pixelpoints))
    ys = round.(Int, predict(y_model, pixelpoints))
    T = promote_type(typeof(missingval), eltype(A))
    Awarped = Raster(similar(template, T); name=Rasters.name(A))
    Awarped .= missingval
    for (Ik, Iu) in  zip(CartesianIndices(template), CartesianIndex.(zip(xs, ys)))
        if checkbounds(Bool, A, Iu)
            Awarped[Ik] = A[Iu]
        end
    end
    return Awarped
end

function _fitlinearmodels(points)
    x_model = lm(@formula(x_unknown ~ x_known + y_known), points)
    y_model = lm(@formula(y_unknown ~ y_known + x_known), points)
    return x_model, y_model
end
