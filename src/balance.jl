function _balance(A, points; category_bitindex)
    little_table = map(points[category_bitindex]) do pointvec
        category_colors = map(pointvec) do (i, j)
            RGB(A[round(Int, i), round(Int, j)])
        end
        category_mean_color = mean(category_colors)
        map(category_colors, pointvec) do c, (i, j)
            rgb = c - category_mean_color
            map(Float64, (; r=rgb.r, g=rgb.g, b=rgb.b, i, j))
        end
    end |> Iterators.flatten |> collect
    @assert little_table isa Vector{<:NamedTuple}
    if length(little_table) < 8
        return A .* 1.0
    end
    big_table = map(CartesianIndices(A)) do I
        (i=I[1], j=I[2])
    end |> vec
    models = (
        r=lm(@formula(r ~ i^3 + i^2 + i + j^3 + j^2 + j), little_table),
        g=lm(@formula(g ~ i^3 + i^2 + i + j^3 + j^2 + j), little_table),
        b=lm(@formula(b ~ i^3 + i^2 + i + j^3 + j^2 + j), little_table),
    )
    predictions = map(models) do model
        reshape(predict(model, big_table), size(A))
    end
    means = map(mean, predictions)
    balanced_map = broadcast(A, predictions...) do x, p_r, p_g, p_b 
        x1 = RGB(x)
        # Subtract the prediction from each pixel, and add the mean
        result = (x1.r-p_r+means.r, x1.g-p_g+means.g, x1.b-p_b+means.b)
        # Clamp and convert to Float64
        RGB(map(c -> min(1, max(c, 0)), result)...) .* 1.0
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
