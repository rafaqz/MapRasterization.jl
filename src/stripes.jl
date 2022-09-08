
hood = LayeredPositional(
    vert=Positional((-1, 0), (1, 0)),
    horz=Positional((0, -1), (0, 1)),
    angle45=Positional((-1, -1), (1, 1)),
    angle135=Positional((-1, 1), (1, -1)),
)

function _pos(i, j, s)
    a = map(-s:-1) do x
        (i * x, j*x)
    end
    b = map(1:s) do x
        (i * x, j*x)
    end
    Positional(a..., b...)
end

function cross_layer_neighborhood(radius)
    hood = LayeredPositional(
        vert=_pos(1, 0, radius),
        horz=_pos(0, 1, radius),
        angle45=_pos(1, 1, radius),
        angle135=_pos(1, -1, radius),
    )
end

function _stripes(img; radius=3, hood=cross_layer_neighborhood(radius))
    Neighborhoods.broadcast_neighborhood(hood, img; padval=(0.0, 0.0)) do hood, val
        (; h, s, l) = HSL(val)
        dirs = map(neighbors(hood)) do layer
            mean(layer) do n
                (s - HSL(n).s)^2 + (l - HSL(n).l)^2
            end
        end
        (dirs.vert - dirs.horz), (dirs.angle45 - dirs.angle135)
    end
end

function _mean_point_stripyness(A::AbstractMatrix, pointvecs::AbstractVector{<:AbstractVector})
    _mean_point_stripyness.(Ref(A), pointvecs)
end
function _mean_point_stripyness(A::AbstractMatrix, pointvec::AbstractVector{<:Point2})
    length(pointvec) == 0 ? 0.0 : map(P -> _point_value(A, P), pointvec)
end

function _remove_lines(A, pointvecs; tolerance=2.0, threshold=0.01, kw...)
    categories = collect(skipmissing(_component_stats(A, pointvecs)))
    if length(categories) > 0
        stripyness = _stripes(A; kw...)
        stripyness = (xs -> sum(map(abs, xs))).(stripyness)
        stripyness = broadcast_neighborhood((h, _) -> mean(h), Window{7}(), stripyness)
        point_stripyness = _mean_point_stripyness(A, pointvecs)
        empty = RGBA(one(first(A)), 0)
        destriped = broadcast(A, stripyness) do pixel, strp
            a = RGBA(pixel)
            if strp > threshold
                # c = _categorise_color(a, categories; tolerance)
                # c == 0 ? a : RGBA(one(pixel), 0)
                empty
            else
                a
            end
        end
        hood = Moore{2}()
        broadcast_neighborhood(hood, destriped, A) do (dh, ah), (d, a)
            d === empty || return d # not empty, nothing to do here
            return RGBA(all(==(empty), dh) ? d : a)
        end
    else
        RGBA.(A)
    end
end
