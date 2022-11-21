
function selectmultiple(A, fig, ax; guide=nothing, transparency=false, points, kw...)
    _plot!(ax, A; transparency)
    isnothing(guide) || plot!(ax, guide; color=:red)
    positions = Observable(points)
    Makie.scatter!(ax, positions, color=1:30, colormap=:reds)
    labels = lift(p -> string.(1:length(p)), positions)
    Makie.text!(ax, labels; position=positions)
    dragselect!(fig, ax, positions; kw...)
    return positions
end

# function manualinput(A::Raster; points=Point2{Float32}[], keys=nothing)
#     points = table2points(points; keys)
#     A = reorder(A, ForwardOrdered)
#     fig = Figure()
#     ax = Makie.Axis(fig[1,1]; title="Source image")
#     ax.aspect = AxisAspect(1)
#     Makie.heatmap!(ax, lookup(A, X), lookup(A, Y), parent(A))
#     positions = Observable(points)
#     Makie.lines!(ax, positions; color=:red)
#     Makie.scatter!(ax, positions; color=:red)

#     l = lookup(template, X)
#     accuracy_scale = abs(l[2] - l[1])
#     dragselect!(fig, ax, positions; caninsert=true, accuracy_scale)
#     screen = display(fig)
#     println("Select polygons in rasters, then close the window")
#     while screen.window_open[]
#         sleep(0.1)
#     end
#     return positions[]
# end

_get(positions, ::Nothing) = positions
_get(positions, section::Observable) = _get(positions, section[])
_get(positions, section::Int) = positions[section]
# _get(positions::Observable, section::Int) = positions[][section]
# Allow nesting ?
# _get(positions, section::Tuple{Vararg{Observable{Int}}}) = _get(positions, map(getindex, section))
# _get(positions, section::Tuple{Vararg{Int}}) = _get(_get(positions, first(section[1])), Base.tail(section))
# _get(positions, section::Tuple{}) = positions
# _get(categories::Vector, section::Vector) = _get(categories[section[1]], section[2])


function dragselect!(fig, ax, positions;
    selected=Ref(false),
    dragging=Ref(false),
    caninsert=false,
    section=nothing,
    active=Ref(true),
    accuracy_scale=1
)
    selected[] = false
    # Get pixel click accuracy from the size of visable heatmap.
    accuracy = lift(ax.finallimits) do fl
        maximum(fl.widths) / 100 * accuracy_scale
    end
    idx = Ref(0)
    # Mouse down event
    on(events(fig).mousebutton, priority = 2) do event
        # @show "point", active[]
        active[] || return Consume(false)
        pos = Makie.mouseposition(ax.scene)
        pos_px = Makie.mouseposition_px(fig.scene)
        # Add points with left click
        if event.button == Mouse.left
            if event.action == Mouse.press
                if pos_px in ax.scene.px_area[]
                    insert = false
                    found = pointnear(_get(positions, section)[], pos, accuracy[]) do i
                        if isnothing(i)
                            return nothing
                        else
                            idx[] = i
                            true
                        end
                    end
                    if isnothing(found)
                        if !insert
                            push!(_get(positions, section)[], pos)
                            idx[] = lastindex(_get(positions, section)[])
                            notify(_get(positions, section))
                        end
                    end
                    dragging[] = true
                    selected[] = true
                else
                    selected[] = false
                end
            elseif event.action == Mouse.release
                dragging[] = false
                notify(_get(positions, section))
            end
        # Delete points with right click
        elseif event.button == Mouse.right
            if pos_px in ax.scene.px_area[]
                pointnear(_get(positions, section)[], pos, accuracy[]) do i
                    isnothing(i) || deleteat!(_get(positions, section)[], i)
                    notify(_get(positions, section))
                end
            end
            selected[] = false
        end
        return Consume(dragging[])
    end
    # Mouse drag event
    on(events(fig).mouseposition, priority = 2) do mp
        active[] || return Consume(false)
        if dragging[]
            pos = Makie.mouseposition(ax.scene)
            # Check for sync problems
            # if ipos in eachindex(positions[])
            _get(positions, section)[][idx[]] = pos
            notify(_get(positions, section))
            # end
            return Consume(true)
        end
        return Consume(false)
    end
end

function dragselect_polygon!(fig, ax, polygons;
    selected=Ref(false),
    dragging=Ref(false),
    caninsert=true,
    section=nothing,
    active=Ref(true),
    accuracy_scale=1,
)
    selected[] = false
    # Get pixel click accuracy from the size of visable heatmap.
    accuracy = lift(ax.finallimits) do fl
        maximum(fl.widths) / 100 * accuracy_scale
    end
    idx = Ref((0, 0))
    # Mouse down event
    on(events(fig).mousebutton, priority = 2) do event
        active[] || return Consume(false)
        pos = Makie.mouseposition(ax.scene)
        pos_px = Makie.mouseposition_px(fig.scene)
        # Add points with left click
        if event.button == Mouse.left
            if event.action == Mouse.press
                if pos_px in ax.scene.px_area[]
                    sec_polygons_obs = _get(polygons, section)
                    sec_polygons = sec_polygons_obs[]
                    insert = false
                    if _is_shift_pressed(fig)
                        push!(sec_polygons, [pos])
                        idx[] = (lastindex(sec_polygons), 1)
                    end
                    found = pointnear(sec_polygons, pos, accuracy[]) do I
                        if isnothing(I)
                            return nothing
                        else
                            idx[] = I
                            return true
                        end
                    end
                    if isnothing(found)
                        println(length(sec_polygons))
                        if caninsert && length(sec_polygons) > 0 && idx[][1] > 0 && length(sec_polygons[idx[][1]]) > 1
                            # Search backwards so we preference recent lines
                            i = idx[][1]
                            lastp = sec_polygons[end][end]
                            for j in eachindex(sec_polygons[i])[end-1:-1:1]
                                println("$i $j $pos")
                                p = sec_polygons[i][j]
                                online = ison(Line(lastp, p), pos, accuracy[] * 2)
                                if online
                                    insert = true
                                    idx[] = (i, j + 1)
                                    insert!(sec_polygons[i], j + 1, pos)
                                    notify(sec_polygons_obs)
                                    println("On the line!! $i, $j")
                                    break
                                end
                                lastp = p
                            end
                        end
                        if !insert
                            if length(sec_polygons) > 0
                                i = idx[][1]
                                if i == 0
                                    idx[] = (1, 1)
                                elseif idx[][2] > length(sec_polygons[i])
                                    idx[] = (i, length(sec_polygons[i]) + 1)
                                else
                                    idx[] = (i, idx[][2] + 1)
                                end
                                insert!(sec_polygons[idx[][1]], idx[][2], pos)
                            else
                                idx[] = (1, 1)
                                push!(sec_polygons, [pos])
                            end
                            notify(sec_polygons_obs)
                        end
                    end
                    dragging[] = true
                    selected[] = true
                else
                    selected[] = false
                end
            elseif event.action == Mouse.release
                sec_polygons_obs = _get(polygons, section)
                dragging[] = false
                notify(sec_polygons_obs)
            end
        # Delete points with right click
        elseif event.button == Mouse.right
            sec_polygons_obs = _get(polygons, section)
            sec_polygons = sec_polygons_obs[]
            if _is_shift_pressed(fig)
                for i in eachindex(sec_polygons)
                    if pos in sec_polygons[i]
                        deleteat!(sec_polygons, i)
                        idx[] = (1, 1)
                        notify(sec_polygons_obs)
                        break
                    end
                end
            elseif pos_px in ax.scene.px_area[]
                pointnear(sec_polygons, pos, accuracy[]) do I
                    if !isnothing(I)
                        deleteat!(sec_polygons[I[1]], I[2])
                        idx[] = (I[1], lastindex(sec_polygons[I[1]]))
                        if length(sec_polygons[I[1]]) == 0
                            deleteat!(sec_polygons, I[1])
                            if length(sec_polygons) > 0
                                s = max(1, I[1] - 1)
                                idx[] = (s, lastindex(sec_polygons[s]))
                            else
                                idx[] = (0, 0)
                            end
                        end
                        notify(sec_polygons_obs)
                    end
                end
            end
            selected[] = false
        end
        return Consume(dragging[])
    end
    # Mouse drag event
    on(events(fig).mouseposition, priority = 2) do mp
        active[] || return Consume(false)
        if dragging[]
            pos = Makie.mouseposition(ax.scene)
            sec_polygons_obs = _get(polygons, section)
            sec_polygons = sec_polygons_obs[]
            # Check for sync problems
            # if ipos in eachindex(polygons[])
            sec_polygons[idx[][1]][idx[][2]] = pos
            notify(sec_polygons_obs)
            # end
            return Consume(true)
        end
        return Consume(false)
    end
end

function _move(positions, i, dir)
    positions[][i] = positions[][i] .+ dir
    notify(positions)
end

function pointnear(f, positions::Vector{<:Point}, pos, accuracy)
    for i in eachindex(positions)[end:-1:1]
        p = positions[i]
        if p[1] in (pos[1]-accuracy..pos[1]+accuracy) &&
            p[2] in (pos[2]-accuracy..pos[2]+accuracy)
            return f(i)
        end
    end
    return nothing
end
function pointnear(f, positions::Vector{<:Vector}, pos, accuracy)
    for i in eachindex(positions)[end:-1:1]
        for j in eachindex(positions[i])
            p = positions[i][j]
            if p[1] in (pos[1]-accuracy..pos[1]+accuracy) &&
                p[2] in (pos[2]-accuracy..pos[2]+accuracy)
                return f((i, j))
            end
        end
    end
    return nothing
end


function ison(line, point, accuracy)
    (x1, y1), (x2, y2) = line
    x = point[1]
    y = point[2]
    grad = (y2 - y1) / (x2 - x1)
    if grad in (Inf, -Inf, NaN, NaN32)
        return x2 == x && inbounds((y1, y2), y)
    elseif grad == 0
        return y2 == y && inbounds((x1, x2), x)
    else
        inbounds((y1, y2), y) && inbounds((x1, x2), x) || return false
        if grad > -1 && grad < 1
            line_y = round(grad * (x - x1) + y1)
            return y in (line_y - accuracy)..(line_y + accuracy)
        else
            line_x = round((y - y1)/grad + x1)
            return x in (line_x - accuracy)..(line_x + accuracy)
        end
    end
end

inbounds((x1, x2), x) = x >= min(x1, x2) && x <= max(x1, x2)

function _plot!(ax::Axis, A, args...; kw...)
    if _typeof(A) <: AbstractVector
        if _eltype(A) <: Polygon
            poly!(ax, A; kw...)
        else
            lines!(ax, A; kw...)
        end
    else
        _heatmap!(ax, A, args...; kw...)
    end
end
_typeof(o::Observable) = typeof(o[])
_typeof(x) = typeof(x)
_eltype(x) = eltype(_typeof(x))

function _heatmap!(ax, A::AbstractDimArray, args...; colormap=:viridis, kw...)
    lookups = map(lookup(A, (X, Y))) do l
        DimensionalData.maybeshiftlocus(Center(), l)
    end
    if eltype(A) <: Colorant
        Makie.heatmap!(ax, lookups..., Float64.(Gray.(parent(A))); colormap, kw...)
    else
        Makie.heatmap!(ax, lookups..., parent(A); colormap, kw...)
    end
end
function _heatmap!(ax, A, args...; colormap=:viridis, kw...)
    if eltype(A) <: Colorant
        Makie.heatmap!(ax, args..., Float64.(Gray.(A)); colormap, kw...)
    else
        Makie.heatmap!(ax, args..., A; colormap, kw...)
    end
end

function points2table(points::Vector)
    map(points) do x, y
        (; x=Float64(x), y=Float64(y))
    end
    return merge.(pointvecs...)
end
function points2table(pointvecs::NamedTuple{Keys}) where Keys
    points = map(pointvecs, Keys) do points, K
        map(points) do xy
            NamedTuple{(Symbol("x_$K"), Symbol("y_$K"))}(map(Float64, xy))
        end
    end
    return merge.(points...)
end

table2points(A; keys=nothing) = _table2points(A, keys)
table2points(A::AbstractArray{<:NamedTuple}) = _table2points(A, keys(first(A)))
function _table2points(table, keys::NTuple{4})
    apoints = Point2{Float32}.(collect(zip(Tables.getcolumn(table, keys[1]), Tables.getcolumn(table, keys[2]))))
    bpoints = Point2{Float32}.(collect(zip(Tables.getcolumn(table, keys[3]), Tables.getcolumn(table, keys[4]))))
    return apoints, bpoints
end
function _table2points(table, keys::NTuple{2})
    return Point2{Float32}.(collect(zip(Tables.getcolumn(table, keys[1]), Tables.getcolumn(table, keys[2]))))
end

function hover_report!(string_obs, fig, ax, sources, source_id)
    on(events(fig).mouseposition, priority=2) do mp
        pos_px = Makie.mouseposition_px(fig.scene)
        if pos_px in ax.scene.px_area[]
            A = sources[source_id[]][]
            pos = Makie.mouseposition(ax.scene)
            I = round.(Int, pos)
            if checkbounds(Bool, A, I...)
                string_obs[] = string(A[I...])
            end
        end
    end
end

