function manualwarp(As...; points=nothing, kw...)
    points = select_common_points(first(As); points, kw...)
    applywarp(As...; points, kw...)
end

function applywarp(As::RasterStack; template::Raster, kw...)
    As = map(A -> reorder(A, ForwardOrdered), As)
    template = reorder(template, ForwardOrdered)
    warped = _warp_from_points(As, template; kw...)
    return warped
end
function applywarp(As...; template::Raster, kw...)
    A1 = first(As)
    if A1 isa Raster
        As = map(A -> reorder(A, ForwardOrdered), As)
    end
    template = reorder(template, ForwardOrdered)
    warped = _warp_from_points(As, template; kw...)
    # Show updated heatmap
    if length(warped) == 1
        return first(warped)
    else
        return warped
    end
end

function _warp_from_points(As::RasterStack, template::Raster; kw...)
    rasters = _warp_from_points(values(As), template; kw...)
    RasterStack(rasters)
end
function _warp_from_points(As::Tuple, template::Raster; points, poly=1, kw...)
    models = _fitlinearmodels(points, poly)
    return map(A -> linearwarp(A; template, models, poly, kw...), As)
end


"""
    select_common_points(A; template, kw...)

Select points in an image and a spatial Raster
in order to snap the image to the rasters coordinates.
"""
function select_common_points(A::AbstractArray; template::AbstractArray, 
    points=nothing, keys=nothing, missingval=missing, kw...
)
    # Only works in GLMakie
    # GLMakie.activate()
    fig = Figure()
    ax1 = Makie.Axis(fig[1,1]; title="Source raster with known crs/resolution - `template` kw")
    ax2 = Makie.Axis(fig[1,2]; title="First raster with unknown crs/resolution")
    screen = display(fig)
    knownpoints, unknownpoints = _select_common_points(A, template::AbstractArray; 
        points, keys, missingval, fig, ax1, ax2, kw...
    )
    println("Select points in rasters, then close the window")
    while screen.window_open[] 
        sleep(0.1)
    end
    length(knownpoints[]) == length(unknownpoints[]) || @warn "Number of selected points not same for each image"
    l = min(length(knownpoints[]), length(unknownpoints[]))
    return points2table((known=knownpoints[], unknown=unknownpoints[]))
end

function _select_common_points(A::AbstractArray, template::AbstractArray; 
    points=nothing, keys=nothing, missingval=missing, fig, ax1, ax2, poly=1, kw...
)
    if template isa Raster
        template = reorder(template, Y=>ForwardOrdered)
    end
    dragging1 = Ref(false)
    dragging2 = Ref(false)
    knownpoints, unknownpoints = if !isnothing(points) && Tables.rowcount(points) > 0
        table2points(points; keys)
    else
        Point2{Float32}[], Point2{Float32}[]
    end
    unknownpoints = selectmultiple(A, fig, ax1; dragging=dragging1, points=unknownpoints)
    knownpoints = selectmultiple(template, fig, ax2; dragging=dragging2, points=knownpoints)
    if A isa AbstractVector
        # warped_vector_overlay = Observable{Any}(A)
        # _plot!(ax2, warped_vector_overlay)
    else
        warped_overlay = Observable(fill!(similar(parent(template), promote_type(typeof(missingval), eltype(A))), missingval))
        _plot!(ax2, warped_overlay; colormap=(:viridis, 0.2), transparency=false)
    end
    onany(knownpoints, unknownpoints) do k, u
        @show "warping"
        (dragging1[] || dragging2[]) && return nothing # Dont update during drag
        len = min(length(k), length(u))
        (length(k) == length(u) && len >= 3 * poly) || return nothing
        points = points2table((known=k[1:len], unknown=u[1:len]))
        if A isa AbstractVector
            w = linearwarp(A; template, points, missingval, poly)
            _plot!(ax2, w)
            # warped_vector_overlay[] = linearwarp(A; template, points, missingval, poly)
            # notify(warped_vector_overlay)
        else
            warped_overlay[] = parent(linearwarp(A; template, points, missingval, poly))
            notify(warped_overlay)
        end
        return nothing
    end
    return knownpoints, unknownpoints
end



"""
    linearwarp(A; template, points, models, missingval)

Warp `A` to match `template`, following the points in `points`
or fitted linear models for x and y in the tuple `models`.

Gaps are filled with `missingval`.
"""
function linearwarp(A::AbstractArray; 
    template::Raster, points=nothing, models::Union{Nothing,Tuple}=nothing, missingval=missing, poly=1
)
    x_model, y_model = if isnothing(models) 
        isnothing(points) && error("pass either `points::Tuple` to fit or fitted `models::Tuple`")
        _fitlinearmodels(points, poly)
    else
        models
    end
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
function linearwarp(geoms::AbstractVector; 
    template::Raster, points=nothing, models::Union{Nothing,Tuple}=nothing, poly=1, kw...
)
    x_model, y_model = if isnothing(models) 
        isnothing(points) && error("pass either `points::Tuple` to fit or fitted `models::Tuple`")
        _fitlinearmodels(points, poly)
    else
        models
    end
    # display(x_model); display(y_model)
    lx, ly = lookup(template, (X, Y))
    warped_geoms = map(geoms) do geom
        pixelpoints = map(GI.getpoint(geom)) do point
            (x_known = GI.x(point), y_known = GI.y(point))
        end
        xs = predict(x_model, pixelpoints)
        ys = predict(y_model, pixelpoints)
        if GI.geomtrait(geom) isa GI.AbstractLineStringTrait
            LineString(map(Point2, zip(xs, ys)))
        elseif GI.geomtrait(geom) isa GI.AbstractPolygonTrait
            Polygon(map(Point2, zip(xs, ys)))
        end
    end
    return warped_geoms
end

lookup_at(lookups, I) = map(getindex, lookups, I)

function _fitlinearmodels(points, poly)
    if poly == 1
        x_model = lm(@formula(x_unknown ~ x_known + y_known), points)
        y_model = lm(@formula(y_unknown ~ y_known + x_known), points)
    elseif poly == 1
        x_model = lm(@formula(x_unknown ~ x_known^2 + y_known^2 + x_known + y_known), points)
        y_model = lm(@formula(y_unknown ~ y_known^2 + x_known^2 + y_known + x_known), points)
    else
        error("poly above 2 does not really work")
    end

    return x_model, y_model
end
