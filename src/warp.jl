function manualwarp!(As...; points=nothing, kw...)
    points = select_warp_points(first(As); points, kw...)
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
function _warp_from_points(As::Tuple{AbstractMatrix,Vararg}, template::Raster; points, poly=1, kw...)
    models = _fitlinearmodels_a(points, poly)
    return map(A -> linearwarp(A; template, models, poly, kw...), As)
end
function _warp_from_points(As::Tuple{AbstractVector,Vararg}, template::Raster; points, poly=1, kw...)
    models = _fitlinearmodels(points, poly)
    return map(A -> linearwarp(A; template, models, poly, kw...), As)
end


"""
    select_warp_points(A; template, kw...)

Select points in an image and a spatial Raster
in order to snap the image to the rasters coordinates.
"""
function select_warp_points(A::AbstractArray; template::AbstractArray,
    points=nothing,
    keys=(:x_a, :y_a, :x_b, :y_b),
    missingval=missing, kw...
)
    # Only works in GLMakie
    # GLMakie.activate()
    fig = Figure()
    ax1 = Makie.Axis(fig[1,1]; title="Source raster with a crs/resolution - `template` kw")
    ax2 = Makie.Axis(fig[1,2]; title="First raster with b crs/resolution")
    screen = display(fig)
    apoints, bpoints = _select_warp_points(A, template::AbstractArray;
        points, keys, missingval, fig, ax1, ax2, kw...
    )
    println("Select points in rasters, then close the window")
    while screen.window_open[]
        sleep(0.1)
    end
    length(apoints[]) == length(bpoints[]) || @warn "Number of selected points not same for each image"
    l = min(length(apoints[]), length(bpoints[]))
    return points2table((a=apoints[][1:l], b=bpoints[][1:l]))
end

function _select_warp_points(A::AbstractArray, template::AbstractArray;
    points=nothing, keys=nothing, missingval=missing, fig, ax1, ax2, poly=1, kw...
)
    if template isa Raster
        template = reorder(template, Y=>ForwardOrdered)
    end
    dragging1 = Ref(false)
    dragging2 = Ref(false)
    apoints, bpoints = if !isnothing(points) && Tables.rowcount(points) > 0
        table2points(points; keys)
    else
        Point2{Float32}[], Point2{Float32}[]
    end
    apoints = selectmultiple(A, fig, ax1; dragging=dragging1, points=apoints)
    bpoints = selectmultiple(template, fig, ax2; dragging=dragging2, points=bpoints)
    hasplotted = false
    if A isa AbstractVector
        warped_overlay = Observable{Vector{eltype(A)}}(eltype(A)[])
    else
        W = fill!(similar(template, promote_type(typeof(missingval), eltype(A))), missingval)
        warped_overlay = Observable{Any}(W)
    end
    hasplotted = false
    onany(apoints, bpoints) do a, b
        println("warping")
        (dragging1[] || dragging2[]) && return nothing # Dont update during drag
        len = min(length(a), length(b))
        (length(a) == length(b) && len >= 3 * poly) || return nothing
        # if template isa Raster
        #     b = map(u) do p 
        #         ds = map(dims(template), Tuple(p)) do d, c
        #             DimensionalData.rebuild(d, Near(c))
        #         end
        #         DimensionalData.dims2indices(template, ds)
        #     end
        # end
        points = points2table((a=a[1:len], b=b[1:len]))
        w = linearwarp(A; template, points, missingval, poly)
        warped_overlay[] = parent(w)
        if !hasplotted
            if A isa AbstractVector
                _plot!(ax2, warped_overlay; color=(:red, 0.5), transparency=true)
            else
                _plot!(ax2, warped_overlay, colormap=(:viridis, 0.5), transparency=true)
            end
            hasplotted = true
        end
        notify(warped_overlay)
        return nothing
    end
    return apoints, bpoints
end

"""
    linearwarp(A; template, points, models, missingval)

Warp `A` to match `template`, following the points in `points`
or fitted linear models for x and y in the tuple `models`.

Gaps are filled with `missingval`.
"""
function linearwarp(A::AbstractArray;
    template, points=nothing, models::Union{Nothing,Tuple}=nothing, missingval=missing, poly=1
)
    x_model, y_model = if isnothing(models)
        isnothing(points) && error("pass either `points::Tuple` to fit or fitted `models::Tuple`")
        _fitlinearmodels_a(points, poly)
    else
        models
    end
    T = promote_type(typeof(missingval), eltype(A))
    B = similar(template, T)
    B .= missingval
    pixelpoints = vec(collect((x_b=x, y_b=y) for (x, y) in Tuple.(CartesianIndices(B))))
    a_xs = round.(Int, predict(x_model, pixelpoints))
    a_ys = round.(Int, predict(y_model, pixelpoints))
    a_indices = CartesianIndex.(zip(a_xs, ys))
    b_indices = CartesianIndices(B)
    for (Ia, Ib) in zip(a_indices, b_indices)
        if checkbounds(Bool, A, Ia)
            @inbounds B[Ib] = A[Ia]
        end
    end
    return B
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
    display(x_model); 
    display(y_model)
    warped_geoms = map(geoms) do geom
        pixelpoints = map(GI.getpoint(geom)) do point
            (x_a = GI.x(point), y_a = GI.y(point))
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

function _fitlinearmodels_a(points, poly)
    if poly == 1
        x_model = lm(@formula(x_a ~ x_b + y_b), points)
        y_model = lm(@formula(y_a ~ y_b + x_b), points)
    elseif poly == 1
        x_model = lm(@formula(x_a ~ x_b^2 + y_b^2 + x_b + y_b), points)
        y_model = lm(@formula(y_a ~ y_b^2 + x_b^2 + y_b + x_b), points)
    else
        error("poly above 2 does not really work")
    end
    return x_model, y_model
end
function _fitlinearmodels_b(points, poly)
    if poly == 1
        x_model = lm(@formula(x_b ~ x_a + y_a), points)
        y_model = lm(@formula(y_b ~ y_a + x_a), points)
    elseif poly == 1
        x_model = lm(@formula(x_b ~ x_a^2 + y_a^2 + x_a + y_a), points)
        y_model = lm(@formula(y_b ~ y_a^2 + x_a^2 + y_a + x_a), points)
    else
        error("poly above 2 does not really work")
    end
    return x_model, y_model
end
