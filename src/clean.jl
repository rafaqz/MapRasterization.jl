
function clean_categories(src::AbstractArray;
    known,
    categories=(),
    neighborhood=Moore{2,2}(),
    keep_neigborless=false,
    missingval=missing,
    despecle=true,
)
    @show sum(src)
    # if despecle
        src = broadcast_neighborhood(Moore{2,2}(), src, known) do hood, v, kn
            (kn != missingval && kn in categories) && return kn
            # If neighbors are missingval, return missingval
            missingcount = count(==(missingval), hood)
            matchcount = count(==(v), hood)
            if !isequal(v, missingval) && v in categories
                if matchcount > 10
                    return v
                elseif missingcount > 18
                    return missingval
                else
                    catcounts = _countcats(hood, categories)
                    if any(>(0), catcounts)
                        n, i = findmax(catcounts)
                        if n > missingcount - 5
                            return missingval
                        else
                            return categories[findmax(catcounts)[2]]
                        end
                    else
                        return missingval
                    end
                end
            elseif v == missingval
                if missingcount > (length(hood) - 2)
                    return missingval
                else
                    catcounts = _countcats(hood, categories)
                    n, i = findmax(catcounts)
                    return categories[i]
                end
            else
                missingval
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
