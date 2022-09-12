# Generate statistics for all components in the pixels of A
function _component_stats(A, pointvecs::AbstractArray{<:AbstractArray})
    _component_stats.(Ref(A), pointvecs, eachindex(pointvecs))
end
function _component_stats(A::AbstractArray, pointvec::AbstractArray, cat::Int)
    length(pointvec) == 0 && return missing
    pixels = _point_values(A, pointvec)
    return _component_stats(pixels)
end
function _component_stats(pixels::Vector{<:PixelProp})
    (
        color=_component_stats([x.color for x in pixels]),
        std=_component_stats([x.std for x in pixels]),
        stripe=_component_stats([x.stripe for x in pixels]),
        category=reduce(_mergecategory, (x.category for x in pixels); init=0)
    )
end
function _component_stats(colors::Vector{<:Colors.Colorant})
    map(_keysnamedtuple(first(colors))) do k
        _component_stats([getfield(c, k) for c in colors])
    end
end
function _component_stats(xs::Vector{<:NTuple{N}}) where N
    ntuple(N) do i
        _component_stats([x[i] for x in xs])
    end
end
function _component_stats(x::Vector{<:Union{Real,Colorant}})
    (; mean=mean(x), min=minimum(x), max=maximum(x), sd=std(x))
end


function _meancolors(::Type{T}, categories) where T <: PixelProp
    map(categories) do ctg
        ismissing(ctg) ? _colortype(T)(zero(RGB(0.0))) : _colortype(T)(map(x -> x.mean, ctg.color)...)
    end
end
function _meancolors(::Type{C}, categories) where C <: Colorant
    map(categories) do ctg
        ismissing(ctg) ? C(zero(RGB(0.0))) : C(map(x -> x.mean, ctg.color)...)
    end
end

# Categorize an array of pixels
function _categorize!(pixels::AbstractArray, segments, points;
    scan_threshold, tolerances, prune_threshold, category_stats, pixelprops,
)
    println("categorizing...")
    categorized_segments = for k in keys(segments.segment_means)
        mn = segments.segment_means[k]
        ctg = mn.category 
        if ctg == 0 
            ctg = _categorize(mn, category_stats, tolerances, pixelprops)
            segments.segment_means[k] = PixelProp(mn.color, mn.std, mn.stripe, ctg)
        end
    end
    println("post-pruning...")
    if prune_threshold > 0
        segments = IS.prune_segments(segments, 
            i -> (IS.segment_mean(segments, i).category != 0 && IS.segment_pixel_count(segments, i) < prune_threshold), 
            (i, j) -> if (IS.segment_mean(segments, j).category == 0 || IS.segment_mean(segments, j).category == IS.segment_mean(segments, i).category) 
                (-IS.segment_pixel_count(segments, j)) 
            else
                typemax(Int)
            end
        )
    end
    println("creating output...")
    category_ints = map(enumerate(pixels)) do (i, pix)
        segments.segment_means[segments.image_indexmap[i]].category
    end
    return category_ints
end
# Categorize a pixel
function _categorize(x, categories, tolerances, pixelprops=Val{(:color,:std,:stripe)}())
    errs = map(categories) do cat
        ismissing(cat) ? typemax(Float64) : _category_error(x, cat, pixelprops)
    end
    _, best = findmin(errs)
    return _isincategory(x, categories[best], tolerances[best], pixelprops) ? best : 0
end


function _category_error(val::PixelProp, stats, pixelprops=Val{(:color,:std,:stripe)}())
    sum(map(_category_error, _asnamedtuple(val)[_unwrap(pixelprops)], _asnamedtuple(stats)[_unwrap(pixelprops)]))
end
function _category_error(v::HSL, s)
    ((v.h - s.h.mean)/360)^2 + (v.s - s.s.mean)^2 + (v.l - s.l.mean)^2 
end
function _category_error(val::Colorant, stats)
    sum(map(_asnamedtuple(val), _asnamedtuple(stats)) do v, s 
        (v - _meanval(s))^2
    end)
end
function _category_error(val::Tuple, stats::Tuple)
    sum(map(val, stats) do v, s 
        (v - _meanval(s))^2
    end)
end
_category_error(v::Number, s) = (v - _meanval(s))^2

_meanval(x::NamedTuple) = x.mean
_meanval(x) = x

function _isincategory(
    val::PixelProp, stats::NamedTuple{(:color,:std,:stripe,:category)},
    tol, pixelprops::Val=Val{(:color,:std,:stripe)}()
)
    if (val.category != 0) && (stats.category != 0)
        return stats.category == val.category 
    end
    return all(map((v, s) -> _isincategory(v, s, tol), _asnamedtuple(val)[_unwrap(pixelprops)], stats[_unwrap(pixelprops)]))
end
function _isincategory(val::RGBA, stats, tol)
    val.alpha == 0 && return false
    _isincategory(RGB(val), stats[(:r, :g, :b)], tol)
end
function _isincategory(val::RGB, stats::NamedTuple{(:r,:g,:b)}, tol)
    all(map((v, s) -> _isincategory(v, s, tol), _asnamedtuple(val), stats))
end
function _isincategory(vals::Tuple, stats::Tuple, tol)
    all(map((v, s) -> _isincategory(v, s, tol), vals, stats))
end
function _isincategory(val::HSL, stats::NamedTuple{(:h,:s,:l)}, tol)
    isincats = map(_asnamedtuple(val)[(:h, :s)], categories[best][(:h, :s)]) do v, s
        _isincategory(v, s, tol)
    end 
    return all(isincats)
end
function _isincategory(v::Real, s, tol)
    (v >= (s.min - s.sd * tol)) && (v <= (s.max + s.sd * tol))
end


# Utils
function _asnamedtuple(x::T) where T
    fnames = fieldnames(T)
    NamedTuple{fnames}(map(fn -> getfield(x, fn), fnames))
end
function _astuple(x::T) where T
    fnames = fieldnames(T)
    map(fn -> getfield(x, fn), fnames)
end

_keysnamedtuple(nt::NamedTuple) = NamedTuple{keys(nt)}(keys(nt))
_keysnamedtuple(x) = _keysnamedtuple(_asnamedtuple(x))
_keysnamedtuple(::Type{T}) where T = NamedTuple{fieldnames(T)}

_colortype(pixels::AbstractArray) = _colortype(eltype(pixels))
_colortype(x) = _colortype(typeof(pixels))
_colortype(::Type{C}) where {C<:Colorant} = C
_colortype(::Type{P}) where {P<:PixelProp{C}} where C = C

_point_index(P) = map(p -> round(Int, p), P)
_point_value(A::AbstractMatrix, P) = A[_point_index(P)...]
_point_values(A::AbstractMatrix, pointvec::AbstractVector) = map(P -> _point_value(A, P), pointvec)

_unwrap(::Val{X}) where X = X
