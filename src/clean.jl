
function clean_categories(src::AbstractArray;
    categories=(), neighborhood=Moore{2,2}(), keep_neigborless=false,
    missingval=missing, despecle=true,
)
    counts = zeros(length(categories))
    ax = unpad_axes(src, neighborhood)
    dst = similar(src, promote_type(eltype(src), typeof(missingval)))
    dst .= missingval
    broadcast!(view(dst, ax...), CartesianIndices(ax)) do I
        DynamicGrids.Neighborhoods.applyneighborhood(neighborhood, src, I) do hood, v
            catcounts = map(categories) do c
                ds = DynamicGrids.distances(hood)
                acc = zero(1/first(ds))
                for i in 1:length(hood)
                    n = hood[i]
                    if n !== missingval && n == c 
                        acc += 1/ds[i]
                    end
                end
                acc
            end
            if all(==(0), catcounts)
                if keep_neigborless
                    return v
                else
                    return missingval
                end
            end
            if despecle && !isequal(v, missingval) && v in categories
                if (count(==(v), skipmissing(hood)) > length(hood) / 4)
                    return v
                else
                    c = categories[findmax(catcounts)[2]]
                    return c
                end
            else
                return categories[findmax(catcounts)[2]]
            end
        end
    end
    return dst
end

