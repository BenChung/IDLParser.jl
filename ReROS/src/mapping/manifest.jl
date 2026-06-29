# The mapper manifest: glob parsing/matching, entry construction, and selection
# (DESIGN-MAPPING.md §2.4, §12.2.3/§12.2.4). The manifest is an explicit value,
# never global state.

# ── Glob ──────────────────────────────────────────────────────────────────────
"""Parse a `/`-segmented glob (`*` within a segment, `**` spans segments) into a
compiled `Glob` with a regex and specificity counters."""
function parse_glob(pattern::AbstractString)
    p = String(pattern)
    io = IOBuffer()
    litlen = 0; nwild = 0
    i = firstindex(p)
    while i <= lastindex(p)
        c = p[i]
        if c == '*'
            j = nextind(p, i)
            if j <= lastindex(p) && p[j] == '*'
                print(io, ".*"); nwild += 1; i = nextind(p, j); continue   # '**' spans '/'
            end
            print(io, "[^/]*"); nwild += 1
        elseif c in ('.', '(', ')', '[', ']', '{', '}', '+', '?', '^', '\$', '|', '\\')
            print(io, '\\', c); litlen += 1
        else
            print(io, c); litlen += 1
        end
        i = nextind(p, i)
    end
    return Glob(p, Regex(string('^', String(take!(io)), '$')), litlen, nwild)
end

glob_match(g::Glob, s::AbstractString) = occursin(g.re, s)

NameMatch(pattern::AbstractString)  = NameMatch(parse_glob(pattern))
TopicMatch(pattern::AbstractString) = TopicMatch(parse_glob(pattern))

# ── MapperEntry construction ──────────────────────────────────────────────────
# A bare string selects by ROS type name; `requires` is auto-derived for a BSpec.
_as_body(f) = BFn(FByValue(f))
_as_body(b::Body) = b
_as_body(a::ArchetypeIL) = BSpec(a)

_as_matcher(m::Matcher) = m
_as_matcher(s::AbstractString) = NameMatch(s)

function MapperEntry(matcher, body; requires::Union{FieldShape,Nothing}=nothing, desc::AbstractString="")
    b = _as_body(body)
    req = requires === nothing ? requirement(b) : requires      # derive for BSpec (conform.jl)
    return MapperEntry(_as_matcher(matcher), b, req, String(desc))
end

# `"pkg/msg/Name" => mapper` pair form.
MapperEntry(p::Pair; kwargs...) = MapperEntry(first(p), last(p); kwargs...)

# ── MapperManifest ────────────────────────────────────────────────────────────
MapperManifest() = MapperManifest(MapperEntry[])
MapperManifest(es::MapperEntry...) = MapperManifest(collect(MapperEntry, es))
MapperManifest(ps::Pair...) = MapperManifest(MapperEntry[MapperEntry(p) for p in ps])

Base.copy(m::MapperManifest) = MapperManifest(copy(m.entries))
Base.push!(m::MapperManifest, e::MapperEntry) = (push!(m.entries, e); m)
Base.length(m::MapperManifest) = length(m.entries)
Base.isempty(m::MapperManifest) = isempty(m.entries)

"""Merge: later manifests override on conflict (their entries win ties, §2.4)."""
Base.merge(a::MapperManifest, bs::MapperManifest...) =
    MapperManifest(vcat(a.entries, (b.entries for b in bs)...))

function Base.show(io::IO, ::MIME"text/plain", m::MapperManifest)
    println(io, "MapperManifest (", length(m.entries), " entries)")
    for e in m.entries
        println(io, "  ", _matcher_label(e.matcher), rpad("", max(0, 28 - length(_matcher_label(e.matcher)))),
                " → ", isempty(e.desc) ? _body_label(e.body) : e.desc)
    end
end
_matcher_label(m::NameMatch)   = m.glob.pattern
_matcher_label(m::TopicMatch)  = "topic:" * m.glob.pattern
_matcher_label(m::StructMatch) = "shape{" * join((string(f.name) for f in m.shape.fields), ",") * "}"
_body_label(::BFn) = "fn"
_body_label(b::BSpec) = string(nameof(typeof(b.emit)))

# ── Selection (§2.4 binding step 1, §12.2.3 specificity) ──────────────────────
# Higher tuple = more specific: (tier, literal-length, -wildcards). An explicit TopicMatch
# (the topic the user pointed at) outranks a nominal NameMatch; exact (no wildcard) outranks
# globs within each; structs rank by field count, below names.
_specificity(m::TopicMatch)  = (m.glob.nwild == 0 ? 6 : 5, m.glob.litlen, -m.glob.nwild)
_specificity(m::NameMatch)   = (m.glob.nwild == 0 ? 4 : 3, m.glob.litlen, -m.glob.nwild)
_specificity(m::StructMatch) = (2, length(m.shape.fields), 0)

# `toplevel` is false while recursing into nested struct fields: a TopicMatch binds the
# top-level (topic, T) only — it must not re-fire on every nested message of that topic.
_matches(m::NameMatch,  typename, topic, ::Type; toplevel::Bool=true) = glob_match(m.glob, typename)
_matches(m::TopicMatch, typename, topic, ::Type; toplevel::Bool=true) = toplevel && glob_match(m.glob, topic)
_matches(m::StructMatch, typename, topic, T::Type; toplevel::Bool=true) = fits(m.shape, T)  # reflection (conform.jl)

"""Matching entries for `(typename, topic, T)`, most-specific first. Ties (equal
specificity) break toward the later manifest position."""
function select_candidates(m::MapperManifest, typename::AbstractString, topic::AbstractString,
                           T::Type; toplevel::Bool=true)
    scored = Tuple{Tuple{Int,Int,Int,Int},MapperEntry}[]
    for (i, e) in enumerate(m.entries)
        _matches(e.matcher, typename, topic, T; toplevel=toplevel) || continue
        push!(scored, ((_specificity(e.matcher)..., i), e))
    end
    sort!(scored; by = first, rev = true)
    return MapperEntry[e for (_, e) in scored]
end

"""The single best-matching entry for `(typename, topic, T)`, or `nothing`."""
function select_entry(m::MapperManifest, typename::AbstractString, topic::AbstractString,
                      T::Type; toplevel::Bool=true)
    c = select_candidates(m, typename, topic, T; toplevel=toplevel)
    return isempty(c) ? nothing : first(c)
end
