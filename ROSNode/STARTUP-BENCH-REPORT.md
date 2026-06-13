# Node startup benchmark — where the cold-start time goes

**Date:** 2026-06-13 · Julia 1.12.6 · 24-core dev box, ROSNode pkgimage already built,
private `zenohd` router on a random loopback port (multicast off). Measured with
`Base.@timed` (which on 1.12 reports `compile_time`/`recompile_time` per expression) and
`--trace-compile-timing`.

> **Update (2026-06-13) — RESOLVED. Cold start 13.6s → 5.3s → 0.88s.** See the
> [resolution section](#resolution-bootstrap-parse-baked-where-zenoh-is-loaded). The
> `@compile_workload`s took it to 5.3s; the residual ~4.1s was **one invalidation** —
> `Zenoh.pointer(::GuardedPayloadView)` invalidating the **IDL PEG-parser combinators**
> ROSMessages bakes Zenoh-free, re-JITed when the first `Context()` runs the bootstrap
> parse. Fix: run that parse (`_wellknown_entries()`) in ROSNode's own `@compile_workload`
> (Zenoh present), one line in `performance/warmup.jl`, no cross-repo edit →
> `recompile_time` 4.1s → 0.009s. The figures below are the *original* baseline.

> **TL;DR.** Building a trivial node — `Context()` + `Node()` + one `Timer()` — costs
> **~9.8s on first construction, of which ~99% is JIT compilation**. The irreducible
> runtime (Zenoh session open + the local entity declares) is **~0.18s**; everything else
> is one-time first-call codegen that a runtime pkgimage warm-up would remove. Full
> fresh-process wall clock is **~14s** (Julia init + pkgimage load + construction +
> teardown) at **~534 MB RSS**. The startup spike is *almost entirely* compilation, not
> transport. **~4.2s of it is wasted *recompilation* of invalidated methods** — a cost
> that baking the path (PLAN-NODE-PRECOMPILE / PLAN-WARMUP-REGISTRATION) won't remove on
> its own.

This is the *runtime* analog of `PRECOMPILE-REPORT.md`: that report fixed ROSNode's own
**package** precompile (the IDL-parser JIT, baked into ROSMessages' pkgimage). This one
measures the **first `run`/construction** at deploy time — the path the two `PLAN-*`
precompile docs target, which is **not** in any pkgimage today.

## Method

A node "containing a timer" is the lightest realistic node. Note from the code:

- **`Timer`** is a thin wrapper over `Base.Timer` (`base/time.jl`) — it declares **no**
  Zenoh entity and touches no discovery. It is *not* the expensive part.
- **`Node(ctx, name)`** declares a liveliness token *and*, by default
  (`serve_type_description=true`), serves `~/get_type_description` — a real **service
  queryable** with the node-default `warmup=:precompile`. That service + its codec is the
  real "entity" work a bare node still pays.
- **`Context()`** opens the session (`Base.open`), registers canonical + static types, and
  starts the `@ros2_lv/<domain>/**` discovery subscriber + its consumer task.

"Precompile-able" = JIT compilation, which a pkgimage / runtime warm-up can eliminate.
Measured two independent ways that agree: (1) `@timed`'s `compile_time`; (2) cold-vs-warm
delta (construct once cold, then re-construct in the same process where everything is
already compiled — the warm time is the irreducible runtime floor).

## Results

### 1. Cold vs warm (Context + Node + Timer), top-level `using`

| Phase | Cold total | compile (share) | of which **recompile** | Warm (irreducible) |
|---|---:|---:|---:|---:|
| `Context()` | 7.70 s | 7.70 s (99.1%) | **4.20 s** | ~0.15 s |
| `Node()` (default svc + sync warmup) | 1.97 s | 1.97 s (99.9%) | 0 | ~0.5 ms |
| `Timer()` | 0.09 s | 0.09 s (99.9%) | 0 | ~0.1 ms |
| **Total** | **9.76 s** | **9.76 s (~99%)** | **4.20 s** | **~0.18 s** |

- `using ROSNode` (pkgimage load, separate): **~0.9 s** (only ~12% compile — mostly
  loading + linking the `.so`).
- **Cold − Warm ≈ 9.6 s** — the JIT a warm-up would lift. Matches `compile_time` (~9.8 s).
- Warm Context re-open is ~0.11–0.17 s and grows with graph size (discovery history
  replay), not with code — it is genuine I/O.

### 2. Where the cold JIT lives (decomposition by ordering each phase to pay only its increment)

| Step | cold JIT | note |
|---|---:|---|
| `Base.open(cfg)` — Zenoh.jl session open | **0.03 s** | transport is **not** the problem; +~0.07 s real I/O |
| `Context()` — ROSNode type-registration + discovery | **~10.8 s** | ~78% of the spike; dominated by the kwcall inference tree |
| bare `Node(serve_type_description=false, warmup=:off)` | **1.10 s** | node identity + liveliness-token path |
| `Node()` increment — `get_type_description` svc + `:precompile` warmup | **0.78 s** | the service queryable + codec + warm |
| `Timer()` | 0.08 s | `Base.Timer` + clock path |

(Absolute values run higher here because of measurement ordering; the *ratios* are the
point. The transport layer — Zenoh / ROSZenoh — is a rounding error.)

### 3. Attribution by `--trace-compile-timing` (169 logged entries; inner inference rolls up into entry points)

| Module / entry | compile | what |
|---|---:|---|
| `Core.kwcall(Context …)` | **4.12 s** | the entire `Context()` constructor inference tree |
| `Core.kwcall(Node …)` | **1.14 s** | the entire `Node()` constructor tree |
| ROSNode (direct) | 2.45 s | discovery consumer loop (0.65 s), service consumer (0.77 s), `wire_get_type_description!` (0.36 s), `service_type_info_of` (0.21 s), codec, `_warmup!`, **+ ~1 s teardown** (`close(Node)`/`close(Entity)`/`close(_ServiceWire)`) |
| Base / Type ctors | 2.5 s | `Dict`, `NamedTuple` ctors, etc. pulled in by the above |
| ROSMessages | 0.32 s | type-description `_element_type` helpers (the `get_type_description` path) |
| Zenoh / ROSZenoh / CDRSerialization | 0.35 s | transport + codec — negligible |

## Conclusions

1. **~99% of node startup is JIT compilation — it is almost entirely precompile-able.**
   The genuine runtime floor (session open + local liveliness/queryable declares) is
   ~0.18 s. A timer-only node confirms the cost is in the *framework construction and
   dispatch machinery*, not in any one entity.

2. **`Context()` is ~80% of it** — the type-registration + discovery path, whose whole
   inference tree compiles on first construction. **`Node()`'s default
   `get_type_description` service + `:precompile` warmup adds ~0.8 s**; the bare node
   identity adds ~1.1 s.

3. **The transport layer is not the bottleneck.** `Base.open` JITs ~0.03 s; all of
   Zenoh/ROSZenoh/CDRSerialization is ~0.35 s.

4. **~4.2 s of the `Context()` compile is *recompilation* of invalidated methods** —
   adversarially confirmed real (it survives a normal top-level `using`, ruling out a
   late-`@eval` artifact, and is identical across runs). This is code the pkgimage already
   compiled that gets invalidated on first runtime use inside `Context()`. **It will not be
   fixed by baking the path** (the `PLAN-*` approach bakes *fresh* compilation); the
   invalidation source must be found and removed. *Follow-up:* run SnoopCompileCore's
   `@snoop_invalidations` around `Context()` to name the invalidator.

5. **Embedded extrapolation.** JIT is CPU-bound and scales ~linearly with core speed; the
   ~0.18 s I/O floor barely moves. On a board 5–10× slower than this dev box the ~14 s
   cold start becomes **1–2 minutes**, while the I/O stays sub-second — so on embedded the
   compilation share is *even more* dominant, consistent with "very, very slow to start up."
   The **534 MB RSS** for a trivial node is itself an embedded concern.

## Recommended fixes (highest leverage first)

1. **Bake the construction + dispatch path into ROSNode's pkgimage** — exactly
   `PLAN-NODE-PRECOMPILE.md` (inert `is_warming()` construction + a `@node`
   `@compile_workload`) and `PLAN-WARMUP-REGISTRATION.md`'s tier-2 scaffolding anchors.
   This benchmark quantifies the prize: **~9–10 s per cold start**, dominated by the
   `Context()` and `Node()` constructor trees + the discovery/service consumer loops — all
   type-agnostic, so a single bake helps every node.
2. **Fix the invalidation driving the ~4.2 s recompilation** (item 4). Independent of
   baking, and baking can't subsume it.
3. **Bake the teardown path too** (`close(Node)`/`close(Entity)`/`close(_ServiceWire)` ≈
   ~1 s) — it's in the same `@compile_workload` reach.

## How to reproduce

```bash
cd ROSNode
zenohd -l tcp/localhost:7447 &                      # any private router
ROS_BENCH_EP=tcp/localhost:7447 \
  julia --project=. -t4 - <<'JL'
import Dates
showstat(l,s) = @info l total_s=round(s.time;digits=3) compile_s=round(s.compile_time;digits=3) recompile_s=round(s.recompile_time;digits=3)
using ROSNode
EP = ENV["ROS_BENCH_EP"]
s1 = Base.@timed ROSNode.Context(; peers=[EP], localhost_only=true, home=Main); showstat("Context cold", s1)
s2 = Base.@timed ROSNode.Node(s1.value, "b"; warmup_sync=true);                 showstat("Node cold",    s2)
s3 = Base.@timed ROSNode.Timer(s2.value, Dates.Second(3600)) do; end;           showstat("Timer cold",   s3)
w1 = Base.@timed ROSNode.Context(; peers=[EP], localhost_only=true, home=Main); showstat("Context WARM", w1)  # ≈ irreducible I/O
close(s3.value); close(s1.value); close(w1.value)
JL
```

Add `--trace-compile=trace.txt --trace-compile-timing` to attribute the JIT by method.
Numbers vary ±1–2 s with machine load.

> **Build note (2026-06-13):** ROSNode did not precompile when this was run — a shell-style
> `\` line-continuation outside a string in `src/patterns/service.jl:247` (uncommitted
> service-detach work). Julia has no such continuation; the `@error` kwarg was folded onto
> one line so the package parses. Unrelated to startup; flagged for the commit.

---

## Update: precompile improvements landed + SnoopCompile root-cause

**Date:** 2026-06-13 (eve). After the precompile improvements (`@compile_workload`s in
`performance/warmup.jl`, `model/component/{run,precompile}.jl`, `patterns/{service,action/client}.jl`),
re-ran the same harness, then snooped with SnoopCompile (`@snoop_invalidations` /
`@snoop_inference` / `precompile_blockers`, from an isolated env via stacked
`JULIA_LOAD_PATH` so ROSNode's manifest is untouched).

### Rerun — cold start 13.6s → 5.3s

| Phase | before | **after** | what moved |
|---|---:|---:|---|
| `Context()` cold | 7.70 s | **5.03 s** | fresh-compile 3.5 s → **0.87 s** ✓ baked |
| — of which **recompile** | 4.20 s | **4.11 s** | **unchanged** ✗ |
| `Node()` cold | 1.97 s | **0.20 s** | **10×** ✓ baked |
| `Timer()` cold | 0.09 s | **0.007 s** | **13×** ✓ baked |
| **Cold total** (Ctx+Node+Timer) | 13.59 s | **5.26 s** | **−61%** |
| Warm total | 0.18 s | 0.006 s | — |

The workloads baked the *fresh* construction/dispatch/codec trees as intended. What's left
is **~78% the recompilation** the original report flagged (item 4) — it did **not** move,
confirming "baking can't subsume an invalidation."

### Root cause of the recompilation (SnoopCompile `precompile_blockers`)

`precompile_blockers(invalidations, @snoop_inference)` ranks invalidations by how much they
block *this workload's* precompilation:

| invalidating method insertion | invalidates | **blocked (this path)** |
|---|---|---:|
| **`Zenoh.pointer(p::GuardedPayloadView)`** (`Zenoh.jl/src/types/zref.jl:556`) | `pointer(::DenseVector{UInt8})` | **~4.89 s / 60 nodes** |
| `getproperty(::Type{rcl_interfaces.srv.GetParameterTypes}, ::Symbol)` (generated) | `getproperty(::DataType, ::Symbol)` | 0.019 s / 4 nodes |

So for **node startup the entire ~4.1 s recompilation is one method**:
`Zenoh.pointer(::GuardedPayloadView)` superseding `pointer(::DenseVector{UInt8})`, which the
codec/serialization path (every entity declare + warmup) leans on. The generated
`getproperty(::Type{ServiceMarker}, ::Symbol)` invalidator has the most *total* backedges
(seen invalidating **12 049** `getproperty(::DataType,::Symbol)` instances) but those are
broad sysimage code, **not** on the startup path — a latent hazard for other code, not the
startup tax. Other (smaller) superseding invalidators: `CDRSerialization.write(::MemBuf,::String)`
(820), `CDRSerialization.==(::CDRView{T},::T)`, and the `wellknown.jl:379`
`Base.convert(::Type{GeneratedType}, x)` guards.

### Residual *fresh* inference still paid at startup (the next bake targets)

Top `@snoop_inference` frames not yet baked (~30–110 ms each):

- **Discovery token parsing** — `parse_liveliness(::RmwZenoh,::String)` 93 ms,
  `decode_qos(::RmwZenoh,::String)` 63 ms, `_ingest_liveliness!` 44 ms. The liveliness
  consumer's per-token path.
- **The IDL PEG parser, JITing at *runtime*** — ~10 `PEG.cache_rule(...)` /
  `ROSMessages._{const_type,scalar,float,bool,string}_lit` frames (~400 ms total): something
  on the construction path parses interface text at runtime (the `get_type_description` /
  static-type registration path), re-incurring the parser JIT `PRECOMPILE-REPORT.md` fixed
  for *package* precompile but not for this runtime path.
- `_rosout_publisher!(::Node)` 67 ms (lazy `/rosout` logger), the `GetTypeDescription`
  serve path (`_serve_query` / `read_view` 43 ms each), `_register_static_types!` 42 ms,
  `Base.ScopedValues.Scope(...)` 108 ms.

### Revised recommendations

1. **Fix the `Zenoh.pointer(::GuardedPayloadView)` invalidation** — single biggest startup
   lever (~4.1 s, ~78% of what's left). It's in **Zenoh.jl** (a shared dep), so ask before
   editing. Options, cheapest first: re-`precompile` `pointer(::DenseVector{UInt8})` (and the
   codec instances behind it) *after* the offending method so ROSNode's pkgimage re-caches
   the invalidated instances; or remove the dispatch ambiguity at `zref.jl:556` (constrain
   the signature / avoid widening `Base.pointer`); SnoopCompile `@snoop_invalidations` over
   `using Zenoh` confirms the fix lands.
2. **Defuse the generated `getproperty(::Type{ServiceMarker}, ::Symbol)`** — not on the
   startup path but a 12k-instance latent invalidator; the codegen should not overload
   `getproperty` on the *type object* (use a plain function/accessor instead).
3. **Bake the residual fresh frames** — extend the runtime workload to the discovery
   liveliness/qos parse path + `_rosout_publisher!` + the runtime PEG-parser frames (~1 s
   total). Lower priority than (1).

---

## Resolution: bootstrap parse baked where Zenoh is loaded

**Date:** 2026-06-13 (late). The ~4.1s recompilation was chased to its exact cause with
SnoopCompile `precompile_blockers` + invalidation/inference-tree drilling.

### Root cause (definitive)

`precompile_blockers` over the `Context()+Node()+Timer()` construction returns exactly two
stale trees:

| invalidation | blocked |
|---|---:|
| `pointer(::GuardedPayloadView)` → `pointer(::DenseVector{UInt8})` | **4.95s / 60 nodes** |
| `getproperty(::Type{…ListParameters}, ::Symbol)` → `getproperty(::DataType,::Symbol)` | 0.02s / 4 nodes |

The 60 blocked nodes are **all ROSMessages IDL PEG-parser combinators** (`_prim_scalar`,
`_int_lit`, `_float_lit`, `_string_lit`, `_field_type`, `_element_type`, `_array_suffix`,
`_const_type`, `_line`, `_parse_scalar_value`, …). The chain:

1. A node serves `~/get_type_description` by default; `Context()` → `_register_canonical_types!`
   → `_canonical_entries()` → (bootstrap) `_build_wellknown_entries()` **parses** the vendored
   `type_description_interfaces` `.msg`/`.srv` via `message_il`/`service_il` at runtime — the
   memo (`_WELLKNOWN_ENTRIES[]`/`_CANONICAL_ENTRIES[]`) is a lazy `Ref`, filled on first `Context()`.
2. The PEG parser reads string code-units through a `DenseVector{UInt8}`, so its combinators
   have a backedge to `pointer(::DenseVector{UInt8})`.
3. **ROSMessages bakes those combinators in its own pkgimage — and ROSMessages does not depend
   on Zenoh** — so they're compiled with `pointer(::DenseVector{UInt8})` → Base's
   `pointer(::AbstractArray)`. When ROSNode loads ROSMessages *and* Zenoh, Zenoh's
   `pointer(::GuardedPayloadView)` (`GuardedPayloadView <: DenseVector{UInt8}`) supersedes that
   generic and **invalidates the baked combinators** → they re-JIT on the first `Context()`.

This is why the leaf re-anchor failed (it baked `pointer`, not the 60-combinator subtree), why
the codec buffers being concrete is irrelevant (this is the *parser's* byte access), and why
`getproperty`-on-Type was a red herring (0.02s).

### Fix (ROSNode-only, one line)

Run the bootstrap parse inside ROSNode's `@compile_workload` (`performance/warmup.jl`), which
executes *after* every dependency — including Zenoh — has loaded, so the combinators compile
against the final method table and bake **valid** into ROSNode's image:

```julia
@compile_workload begin
    # … existing codec round-trip …
    _wellknown_entries()      # bake the bootstrap IDL-parse combinators, Zenoh present
end
```

(The broader `_canonical_entries()` can't run here — it needs `staticgen.jl`, included after
`warmup.jl` — but the grammar is identical, so the bootstrap parse covers the invalidated
combinators.)

### Result

| metric | original | after `@compile_workload`s | **+ this fix** |
|---|---:|---:|---:|
| cold `Context()+Node()+Timer()` | 13.59s | 5.26s | **0.875s** |
| `Context()` cold | 11.07s | 5.03s | **0.583s** |
| `Context()` **recompile_time** | 4.86s | 4.11s | **0.009s** |
| warm total | 0.18s | 0.006s | 0.007s |
| ROSNode precompile (build) | — | ~17s | ~21s (+4s) |

**~15× faster cold start than the original, ~6× over the improved baseline; the recompilation
wall is gone.** Cost is +~4s one-time ROSNode build (the standard precompile-vs-startup trade,
as in this report's own ROSMessages workload). Residual `Context()` ≈ 0.58s is now *fresh*
compilation (the full `_canonical_entries()` parse the bootstrap subset doesn't cover, + discovery
+ registration) — no longer recompilation; further reducible by a second workload after
`staticgen.jl` that calls `_canonical_entries()`, but the 4.1s wall is what mattered.

### Lessons

- A dependency that subtypes a Base-method-bearing abstract type (`GuardedPayloadView <:
  DenseVector`, `MemBuf <: IO`) and loads *after* a precompiled consumer of the superseded
  generic invalidates that consumer's baked code. Re-baking the **whole invalidated subtree** in
  an image where the superseding method is present fixes it; re-anchoring the leaf does not.
- `precompile_blockers(@snoop_invalidations, @snoop_inference)` + dumping the blocked
  `InferenceTimingNode`s is the tool that names *what* recompiles — here, the IDL parser, not the
  codec or construction machinery the earlier hypotheses guessed.
