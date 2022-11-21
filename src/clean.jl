
function fill_gaps(src::AbstractArray;
    categories=(),
    neighborhood=VonNeumann{2}(),
    missingval=missing,
    stats,
    match,
    known,
    original,
    mask,
)
    broadcast_neighborhood(neighborhood, src, src, known, original, mask) do hood, v, kn, o, m
        m && return v
        kn != missingval && kn in categories && return kn
        missingcount = count(==(missingval), hood)
        if !isequal(v, missingval) && v in categories
            return v # This is a valid category already, return as is
        elseif v == missingval
            # If neighbors are mostly missingval, return missingval
            missingcount > (length(hood) - length(hood) ÷ 4) && return missingval
            catcounts = _countcats(hood, categories)
            maxcount, im = findmax(catcounts)
            maxcount > 1 || return missingval
            secondcount, is = reduce(enumerate(catcounts); init=(0=>0)) do (xacc, iacc), (i, x)
                i == im && return xacc => iacc # skip the max category
                x > xacc ? x => i : xacc => iacc
            end
            i = if secondcount > 1
                a = _category_error(o, stats[im], Val{(:color,)}())
                b = _category_error(o, stats[is], Val{(:color,)}())
                a <= b ? im : is
            else
                im
            end
            return categories[i]
        else
            return missingval
        end
    end
end

function _countcats(hood, categories)
    map(categories) do c
        ds = distances(hood)
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

function _clean(A, known_categories; categories, line_threshold=0.01, prune_threshold=20, kw...)
    pick_neighbor(i, j) = sm(segments, j).color == 0 ? typemax(Int) : -spc(segments, j)
    pick_cell(i) = (sm(segments, i).color != 0 && spc(segments, i) < prune_threshold)
    scan_threshold = 0.000000001
    match = Val{(:color,)}()
    segments = fast_scanning(PixelProp.(A, ones(size(A)), fill((1.0, 1.0), size(A)), known_categories), scan_threshold; match)
    for (label, count) in segments.segment_pixel_count
        bbox = segments.segment_bbox[label]
        region = CartesianIndices(bbox[2] - bbox[1] + CartesianIndex(1, 1))
        # Take the maximum square of the longest axis and halve it
        # Our line_threshold == 1 shape is a 1 * 2 rectangle
        rectangle_area = max(size(region)...)^2 / 2
        ratio = count / rectangle_area
        meanpixel = segments.segment_means[label]
        if !(meanpixel.color in categories)
            segments.segment_means[label] = PixelProp(zero(meanpixel.color), meanpixel.std, meanpixel.stripe, 0)
        elseif (count < prune_threshold) || (ratio < line_threshold)
            if meanpixel.category == 0
                segments.segment_means[label] = PixelProp(zero(meanpixel.color), meanpixel.std, meanpixel.stripe, 0)
            elseif !(meanpixel.color in (0, meanpixel.category))
                # Check if any segment has the wrong category, and change it
                # This allows forcing areas into the right category without
                # using segment mode to categorize.
                c = meanpixel.category
                fixedpixel = PixelProp(c, meanpixel.std, meanpixel.stripe, c)
                segments.segment_means[label] = fixedpixel
            end
        end
    end

    # segments = IS.prune_segments(segments, pick_cell, pick_neighbor)
    return map(CartesianIndices(A)) do I
        segments.segment_means[segments.image_indexmap[I]].color
    end
end

# Categorical dilation
function _dilate(A::AbstractArray; missingval=0)
    broadcast_neighborhood(Moore(1), A, A) do hood, x
        if x == missingval
            counts = map(neighbors(hood)) do groupn
                count(==(groupn), neighbors(hood))
            end
            _, i = findmax(counts)
            v = hood[i]
            if v == missingval
                i = findfirst(!=(missingval), hood)
                if isnothing(i)
                    missingval
                else
                    hood[i]
                end
            else
                v
            end
        else
            x
        end
    end
end

# Categorical erosion
function _erode(A::AbstractArray; missingval=0)
    broadcast_neighborhood(Moore(1), A, A) do hood, x
        if all(map(==(x), hood))
            missingval
        else
            x
        end
    end
end

_shrink_grow(A; missingval=0) = _grow(_shrink(A; missingval); missingval)
function _shrink_grow(A, i::Int, j::Int; missingval=0)
    for _ in 1:i
        A = _erode(A)
    end
    for _ in 1:j
        A = _dilate(A)
    end
    return A
end
