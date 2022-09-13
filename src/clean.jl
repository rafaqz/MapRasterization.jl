
function clean_categories(src::AbstractArray;
    categories=(),
    neighborhood=Moore{2,2}(),
    keep_neigborless=false,
    missingval=missing,
    despecle=true,
)
    # if despecle
        src = broadcast_neighborhood(Moore{1,2}(), src) do hood, v
            catcounts = _countcats(hood, categories)
            if !isequal(v, missingval) && v in categories
                if (count(==(v), skipmissing(hood)) > 2)
                    return v
                else
                    if any(>(0), catcounts)
                        return categories[findmax(catcounts)[2]]
                    else
                        return missingval
                    end
                end
            else
                return categories[findmax(catcounts)[2]]
            end
        end
    # end
    return src
    # return broadcast_neighborhood(neighborhood, src, CartesianIndices(src)) do hood, v, I
    #     if v == missingval
    #         catcounts = _countcats(hood, categories)
    #         if all(map(==(0), catcounts))
    #             keep_neigborless ? v : missingval
    #         else
    #             return categories[findmax(catcounts)[2]]
    #         end
    #     else
    #         return v
    #     end
    # end
end

function _countcats(hood, categories)
    map(categories) do c
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
end
