function _pos(i, j, s)
    a = map(-s:-1) do x
        (i * x, j*x)
    end
    b = map(1:s) do x
        (i * x, j*x)
    end
    Neighborhoods.Layered(a=Positional(a...), b=Positional(b...))
end

function cross_layer_neighborhood(radius)
    hood = Neighborhoods.Layered(
        vert=_pos(1, 0, radius),
        horz=_pos(0, 1, radius),
        angle45=_pos(1, 1, radius),
        angle135=_pos(1, -1, radius),
    )
end

function _stripes(img; radius=3, hood=cross_layer_neighborhood(radius))
    stripes = Neighborhoods.broadcast_neighborhood(hood, img, img) do hood, val
        (; h, s, l) = HSL(val)
        dirs = map(neighbors(hood)) do layer
            # Take the minimum of the mean stripiness of either side
            map(layer) do side
                mean(side) do n
                    (s - HSL(n).s)^2 + (l - HSL(n).l)^2
                end
            end |> minimum
        end
        (dirs.vert - dirs.horz), (dirs.angle45 - dirs.angle135)
    end
    m = mean((mean(abs ∘ first, stripes), mean(abs ∘ last, stripes))) 
    return map(s -> s ./ m, stripes)
end

function _mean_point_stripyness(A::AbstractMatrix, pointvecs::AbstractVector{<:AbstractVector})
    _mean_point_stripyness.(Ref(A), pointvecs)
end
function _mean_point_stripyness(A::AbstractMatrix, pointvec::AbstractVector{<:Point2})
    length(pointvec) == 0 ? 0.0 : map(P -> _point_value(A, P), pointvec)
end

# function _remove_lines(A, pointvec; stds=_stds(A), tolerance=2.0, threshold=0.01, kw...)
#     # categories = collect(skipmissing(_component_stats(A, pointvecs)))
#     # if length(categories) > 0
#         m = maximum(stds) * threshold
#         # stripyness = _stripes(A; kw...)
#         # stripyness = (xs -> sum(map(abs, xs))).(stripyness)
#         # stripyness = broadcast_neighborhood((h, _) -> mean(h), Window{7}(), stripyness)
#         # point_stripyness = _mean_point_stripyness(A, pointvecs)
#         empty = RGBA(one(first(A)), 0)
#         destriped = broadcast(A, stds) do pixel, std
#             a = RGBA(pixel)
#             if std >= threshold
#                 # c = _categorise_color(a, categories; tolerance)
#                 # c == 0 ? a : RGBA(one(pixel), 0)
#                 empty
#             else
#                 a
#             end
#         end
#         # hood = Moore{2}()
#         # broadcast_neighborhood(hood, destriped, A) do (dh, ah), (d, a)
#         #     d === empty || return d # not empty, nothing to do here
#         #     return RGBA(all(==(empty), dh) ? d : a)
#         # end
#     # else
#         # RGBA.(A)
#     # end
# end
