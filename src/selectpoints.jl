
selectmultiple(A::Raster, fig, ax; kw...) = selectmultiple(parent(A), fig, ax; kw...)
function selectmultiple(A, fig, ax; transparency=false, points, kw...)
    _heatmap!(ax, A; transparency) 
    positions = Observable(points)
    Makie.scatter!(ax, positions, color=1:30, colormap=:reds)
    labels = lift(p -> string.(1:length(p)), positions)
    Makie.text!(ax, labels; position=positions)
    dragselect!(fig, ax, positions, size(A); kw...)
    return positions
end

function manualinput(A::Raster; points=Point2{Float32}[], keys=nothing)
    points = table2points(points; keys)
    A = reorder(A, ForwardOrdered)
    fig = Figure()
    ax = Makie.Axis(fig[1,1]; title="Source image")
    ax.aspect = AxisAspect(1)
    Makie.heatmap!(ax, parent(A))
    positions = Observable(points)
    Makie.lines!(ax, positions; color=:red)
    Makie.scatter!(ax, positions; color=:red)

    dragselect!(fig, ax, positions, size(A); caninsert=true)
    screen = display(fig)
    println("Select polygons in rasters, then close the window")
    while screen.window_open[] 
        sleep(0.1)
    end
    return positions[]
end

_get(positions, ::Nothing) = positions
_get(positions, section) = positions[section[]]

function dragselect!(fig, ax, positions, pixelsize; 
    selected=Ref(false), dragging=Ref(false), caninsert=false, section=nothing
)
    selected[] = false
    # Get pixel click accuracy from the size of visable heatmap.
    accuracy = lift(ax.finallimits) do fl
        round(Int, maximum(fl.widths) / 100)
    end
    idx = Ref(0)
    # Mouse down event
    on(events(fig).mousebutton, priority = 2) do event
        pos = Makie.mouseposition(ax.scene)
        ipos = round.(Int, pos)
        pos_px = Makie.mouseposition_px(fig.scene)
        # Add points with left click
        if event.button == Mouse.left
            if event.action == Mouse.press
                if pos_px in ax.scene.px_area[]                    
                    plt, i = pick(fig.scene, pos...)
                    idx[] = i
                    insert = false
                    found = pointnear(_get(positions, section)[], ipos, accuracy[]) do i
                        if isnothing(i) 
                            return nothing
                        else
                            idx[] = i
                            true
                        end
                    end
                    if isnothing(found)
                        if caninsert && length(_get(positions, section)[]) > 1
                            lastp = _get(positions, section)[][end]
                            # Search backwards so we preference recent lines
                            for i in eachindex(_get(positions, section)[])[end-1:-1:1]
                                p = _get(positions, section)[][i]
                                online = ison(Line(Point(lastp...), Point(p...)), Point(ipos...), accuracy[])
                                if online
                                    insert = true
                                    idx[] = i + 1
                                    insert!(_get(positions, section)[], i + 1, ipos)
                                    notify(_get(positions, section))
                                    break
                                end
                                lastp = p
                            end
                        end
                        if !insert
                            push!(_get(positions, section)[], ipos)
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
                pointnear(_get(positions, section)[], ipos, accuracy[]) do i
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
        if dragging[]
            pos = Makie.mouseposition(ax.scene)
            ipos = round.(Int, pos)
            # Check for sync problems
            # if ipos in eachindex(positions[])
            _get(positions, section)[][idx[]] = ipos
            notify(_get(positions, section))
            # end
            return Consume(true)
        end
        return Consume(false)
    end
    on(events(fig).keyboardbutton) do event
        if selected[] && event.action in (Keyboard.press, Keyboard.repeat)
            event.key == Keyboard.right  && _move(_get(positions, section), idx[], (1, 0))
            event.key == Keyboard.up     && _move(_get(positions, section), idx[], (0, 1))
            event.key == Keyboard.left   && _move(_get(positions, section), idx[], (-1, 0))
            event.key == Keyboard.down   && _move(_get(positions, section), idx[], (0, -1))
        end
        # Let the event reach other listeners
        return Consume(false)
    end
end

function _move(positions, i, dir)
    positions[][i] = positions[][i] .+ dir 
    notify(positions)
end

function pointnear(f, positions, ipos, accuracy)
    for i in eachindex(positions)[end:-1:1]
        p = positions[i]
        if p[1] in (ipos[1]-accuracy:ipos[1]+accuracy) && 
           p[2] in (ipos[2]-accuracy:ipos[2]+accuracy)
            # Remove a point
            return f(i)
            break
        end
    end
    return nothing
end

function ison(line, point, accuracy)
    (x1, y1), (x2, y2) = line
    x = round(point[1])
    y = round(point[2])
    grad = (y2 - y1) / (x2 - x1)
    if grad in (Inf, -Inf, NaN, NaN32)
        return x2 == x && inbounds((y1, y2), y)
    elseif grad == 0
        return y2 == y && inbounds((x1, x2), x)
    else
        inbounds((y1, y2), y) && inbounds((x1, x2), x) || return false
        if grad > -1 && grad < 1
            line_y = round(grad * (x - x1) + y1)
            return y in (line_y - accuracy):(line_y + accuracy)
        else
            line_x = round((y - y1)/grad + x1)
            return x in (line_x - accuracy):(line_x + accuracy)
        end
    end
end

inbounds((x1, x2), x) = x >= min(x1, x2) && x <= max(x1, x2)

function _heatmap!(ax, A; colormap=:viridis, transparency=false) 
    if eltype(A) <: Colorant
        Makie.heatmap!(ax, Float64.(Gray.(A)); colormap, transparency)
    else
        Makie.heatmap!(ax, A; colormap, transparency)
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
    return merge.(pointvecs...)
end

table2points(A::AbstractArray{Point2}; keys=nothing) = A
table2points(A::AbstractArray{Point2}; keys=nothing) = _table2points(A, keys)
function _table2points(table, keys::NTuple{4})
    knownpoints = Point2{Float32}.(collect(zip(Tables.getcolumn(table, keys[1]), Tables.getcolumn(table, keys[2]))))
    unknownpoints = Point2{Float32}.(collect(zip(Tables.getcolumn(table, keys[3]), Tables.getcolumn(table, keys[4]))))
    return knownpoints, unknownpoints
end
function _table2points(table, keys::NTuple{2})
    return Point2{Float32}.(collect(zip(Tables.getcolumn(table, keys[1]), Tables.getcolumn(table, keys[2]))))
end
