function _balance(A, points)
    if length(points) < 8
        @info "need more points to balance image"
        return A
    end
    big_table = map(CartesianIndices(A)) do I
        (i=I[1], j=I[2])
    end |> vec
    little_table = map(points) do (i, j)
        rgb = RGB(A[round(Int, i), round(Int, j)])
        map(Float64, (; r=rgb.r, g=rgb.g, b=rgb.b, i, j))
    end |> vec
    model_r = lm(@formula(r ~ i + j), little_table)
    model_g = lm(@formula(g ~ i + j), little_table)
    model_b = lm(@formula(b ~ i + j), little_table)
    pred_r = reshape(predict(model_r, big_table), size(A))
    pred_g = reshape(predict(model_g, big_table), size(A))
    pred_b = reshape(predict(model_b, big_table), size(A))
    mean_r = mean(pred_r)
    mean_g = mean(pred_g)
    mean_b = mean(pred_b)
    balanced_map = map(A, pred_r, pred_g, pred_b) do p, r, g, b 
        p1 = RGB(p)
        means = (p1.r-r+mean_r, p1.g-g+mean_g, p1.b-b+mean_b)
        RGB(map(x -> min(1, max(x, 0)), means)...)
    end
    return balanced_map
end

# function balance(A, points)
#     if length(points) < 8
#         @info "need more points to balance image"
#         return A
#     end
#     big_table = map(CartesianIndices(A)) do I
#         (i=I[1], j=I[2])
#     end |> vec
#     little_table = map(points) do (i, j)
#         hsl = HSL(A[round(Int, i), round(Int, j)])
#         map(Float64, (; h=hsl.h, s=hsl.s, l=hsl.l, i, j))
#     end |> vec
#     model_h = lm(@formula(h ~ i + j), little_table)
#     model_s = lm(@formula(s ~ i + j), little_table)
#     model_l = lm(@formula(l ~ i + j), little_table)
#     pred_h = reshape(predict(model_h, big_table), size(A))
#     pred_s = reshape(predict(model_s, big_table), size(A))
#     pred_l = reshape(predict(model_l, big_table), size(A))
#     mean_h = mean(pred_h)
#     mean_s = mean(pred_s)
#     mean_l = mean(pred_l)
#     balanced_map = map(A, pred_h, pred_s, pred_l) do p, h, s, l 
#         p1 = HSL(p)
#         RGB(HSL(p1.h-h+mean_h, p1.s-s+mean_s, p1.l-l+mean_l))
#     end
#     return balanced_map
# end
