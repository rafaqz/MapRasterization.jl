
#=
From: Fast Modified Vector Median Filter
B. Smolka1, M. Szczepanski, K.N. Plataniotis, and A.N. Venetsanopoulos
=#
function blur(A, stds; hood=Window{1}(), threshold=0.25, repeat=1)
    m = maximum(stds) * threshold
    for _ in 1:repeat
        A = broadcast_neighborhood(hood, A) do h, v
            v.alpha == 0 ? v : _clean_mean(h, v)
        end
    end
    return A
    # return A1
    # broadcast_neighborhood(maybe_use_closest_neigbor, hood, A)
    # mn = mapreduce(min, A1) do x
    #     minimum(_astuple(RGB(x)))
    # end
    # mx = mapreduce(max, A1) do x
    #     maximum(_astuple(RGB(x)))
    # end

    # @show mn mx
    # Renormalised
    # return map(A1) do c
    #     c1 = RGB(c) - RGB(mn) / (mx - mn)
    #     RGBA(mapc(x -> min(1.0, max(0.0, x)), c1), c.alpha)
    # end
end

function _clean_mean(h, v)
    v = v * 1.0
    val, count = reduce(h; init=(v, 1)) do (acc_val, acc_count), x
        x.alpha == 0 ? (acc_val, acc_count) : (acc_val + x, acc_count + 1)
    end
    return val / count
end
# function _clean_sum(h, v)
#     v = v * 1.0
#     return reduce(h; init=v) do acc_val, x
#         x.alpha == 0 ? acc_val : acc_val + x
#     end
# end
# function _clean_sum(f, h, v)
#     v = v * 1.0
#     return reduce(h; init=v) do acc_val, x
#         x.alpha == 0 ? acc_val : acc_val + x
#     end
# end

# function maybe_use_closest_neigbor(hood, val)
#     current_distance = neighbor_color_distances(hood, val)
#     distances = map(x -> neighbor_color_distances(hood, x), hood)
#     min_distance, i = findmin(distances)
#     return current_distance > min_distance ? hood[i] : val
# end

# function neighbor_color_distances(hood, val)
#     val.alpha == 0 && return typemax(Float64)
#     return _cleansum(map(neighbor -> ρ(neighbor, val), hood)
# end

# ρ(newcenter, neighbor) = _abs2(newcenter - neighbor)
