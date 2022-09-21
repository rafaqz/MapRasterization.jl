
function clean_categories(src::AbstractArray;
    known,
    categories=(),
    neighborhood=Moore{2,2}(),
    keep_neigborless=false,
    missingval=missing,
    despecle=true,
)
    @show sum(src)
        src = broadcast_neighborhood(Moore{1,2}(), src, known) do hood, v, kn
            kn != missingval && kn in categories && return kn
            (count(==(v), hood) > 2) ? v : missingval
        end
        src = broadcast_neighborhood(Moore{2,2}(), src, known) do hood, v, kn
            kn != missingval && kn in categories && return kn
            # If neighbors are missingval, return missingval
            missingcount = count(==(missingval), hood)
            matchcount = count(==(v), hood)
            if !isequal(v, missingval) && v in categories
                if matchcount > 7
                    return v
                elseif missingcount > 18
                    return missingval
                else
                    catcounts = _countcats(hood, categories)
                    n, i = findmax(catcounts)
                    if n > missingcount - 5
                        return categories[findmax(catcounts)[2]]
                    else
                        return missingval
                    end
                end
            elseif v == missingval
                if missingcount > 18
                    return missingval
                else
                    catcounts = _countcats(hood, categories)
                    n, i = findmax(catcounts)
                    return n > 2 ? categories[i] : missingval
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

const sm = IS.segment_mean
const spc = IS.segment_pixel_count

function _remove_lines(A, known_categories; line_threshold=0.01, prune_threshold=20, kw...)
    pick_neighbor(i, j) = sm(segments, j).color == 0 ? typemax(Int) : -spc(segments, j)
    pick_cell(i) = (sm(segments, i).color != 0 && spc(segments, i) < prune_threshold)
    scan_threshold = 0.000000001
    match = Val{(:color,)}()
    segments = fast_scanning(PixelProp.(A, ones(size(A)), fill((1.0, 1.0), size(A)), known_categories), scan_threshold; match)
    for (label, count) in segments.segment_pixel_count
        bbox = segments.segment_bbox[label]
        region = CartesianIndices(bbox[2] - bbox[1] + CartesianIndex(1, 1))
        area = length(region)
        ratio = count / area 
        if (count < prune_threshold) || (ratio < line_threshold)
            meanpixel = segments.segment_means[label]
            # if meanpixel.category == 0
                segments.segment_means[label] = PixelProp(zero(meanpixel.color), meanpixel.std, meanpixel.stripe, 0)
            # end
        end
    end
    
    # segments = IS.prune_segments(segments, pick_cell, pick_neighbor)
    return map(CartesianIndices(A)) do I
        x = segments.segment_means[segments.image_indexmap[I]].color
        # println(x)
        x
    end
end
