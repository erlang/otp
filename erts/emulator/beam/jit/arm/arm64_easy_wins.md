ARM64 BEAM JIT easy wins

1. Port x86 list-fusion rules to arm for `is_nonempty_list` followed by `get_list`, `get_hd`, or `get_tl`.
2. Audit runtime crossings to keep `emit_enter_runtime` live-register counts as small as possible on hot paths.
3. Expand paired load/store peepholes for consecutive X/Y register traffic.
4. Keep success paths straight-line and push slow error paths out of line.
5. Tighten type-driven fast paths where `BeamTypeId` already proves the shape of the term.

Benchmark target:

- A small `list_bif_SUITE` benchmark that repeatedly executes `[H|T]` style matching on a fixed list. That should exercise the fused list instructions directly and provide a realistic before/after comparison on Apple Silicon.
