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
function _categorize(maps::NamedTuple, points; kw...)
    pixels = PixelProp.(maps.blurred, maps.std, maps.stripe, maps.known_categories)
    _categorize(pixels, points; kw...)
end
function _categorize(pixels::AbstractArray, points;
    segmented=false, tolerances, scan_threshold=0.1, match,
)
    category_stats = collect(_component_stats(pixels, points))
    if segmented
        segments = fast_scanning(pixels, scan_threshold; match)
        println("categorizing...")
        categorized_segments = for k in keys(segments.segment_means)
            mn = segments.segment_means[k]
            ctg = mn.category 
            if ctg == 0 
                ctg = _categorize(mn, category_stats, tolerances, match)
                segments.segment_means[k] = PixelProp(mn.color, mn.std, mn.stripe, ctg)
            end
        end
        println("creating output...")
        segmented_map = map(i -> IS.segment_mean(segments, i), IS.labels_map(segments))
        category_ints = map(IS.labels_map(segments)) do i
            IS.segment_mean(segments, i).category
        end
    else
        category_ints = map(pixels) do pix
            _categorize(pix, category_stats, tolerances, match)
        end
    end
    return category_ints
end
# Categorize a pixel
function _categorize(x, categories, tolerances, match=Val{(:color,:std,:stripe)}())
    errs = map(categories) do cat
        ismissing(cat) ? typemax(Float64) : _category_error(x, cat, match)
    end
    _, best = findmin(errs)
    return _isincategory(x, categories[best], tolerances[best], match) ? best : 0
end


function _category_error(val::PixelProp, stats, match=DEFAULT_MATCH)
    mean(map(_category_error, _asnamedtuple(val)[_unwrap(match)], _asnamedtuple(stats)[_unwrap(match)]))
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
    mean(map(val, stats) do v, s 
        (v - _meanval(s))^2
    end)
end
_category_error(v::Number, s) = (v - _meanval(s))^2

_meanval(x::NamedTuple) = x.mean
_meanval(x) = x

function _isincategory(
    val::PixelProp, stats::NamedTuple{(:color,:std,:stripe,:category)},
    tol, match::Val=DEFAULT_MATCH
)
    if (val.category != 0) && (stats.category != 0)
        return stats.category == val.category 
    end
    return all(map((v, s) -> _isincategory(v, s, tol), _asnamedtuple(val)[_unwrap(match)], stats[_unwrap(match)]))
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
_unwrap(t::Tuple) = t
