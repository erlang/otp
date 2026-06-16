ARM64 BEAM JIT easy wins

1. Port x86 list-fusion rules to arm for `is_nonempty_list` followed by `get_list`, `get_hd`, or `get_tl`.
2. Audit runtime crossings to keep `emit_enter_runtime` live-register counts as small as possible on hot paths.
3. Expand paired load/store peepholes for consecutive X/Y register traffic.
4. Keep success paths straight-line and push slow error paths out of line.
5. Tighten type-driven fast paths where `BeamTypeId` already proves the shape of the term.

Benchmark target:

- A small `list_bif_SUITE` benchmark that repeatedly executes `[H|T]` style matching on a fixed list. That should exercise the fused list instructions directly and provide a realistic before/after comparison on Apple Silicon.

Advanced JIT directions after audit:

1. Prefer guarded shape specialization only when the compiler/loader/JIT do
   not already carry the shape fact. For example, `is_map(M), map_get(K, M)`
   already lets the JIT skip the duplicate `map_get/2` map test via SSA type
   information, so an explicit ops-table fusion does not help.
2. Reuse existing specialized instruction families before adding new ones.
   Pattern map extraction already lowers through `get_map_elements` to
   `i_get_map_element_hash` for literal keys, including a precomputed
   `hashmap_make_hash(Key)`.
3. The remaining audited `map_get/2` gap is BIF-form literal-key lookup:
   `map_get(a, M)` and guard `map_get(a, M) =:= V` still emit
   `bif_map_get`, while pattern `#{a := V}` emits `i_get_map_element_hash`.
   That makes literal-key BIF lookup a narrower and better target than broad
   `is_map` + `map_get` fusion.
4. Cold outlined fallback is still interesting, but only with real cold block
   placement. Without that, the hot path still needs a branch around the cold
   code and the layout win can disappear.
5. More macro-op fusion should start from concrete dumps that prove the loader
   and SSA passes have not already produced an equivalent specialized opcode.
