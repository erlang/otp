X86-64 BEAM JIT easy wins

1. Try the ARM64 guard-literal `map_get/2` prehash optimization on x86-64.
   Audit shows x86-64 `bif_map_get` still routes literal immediate guard keys
   through `i_get_map_element_shared`, while pattern extraction already uses
   `i_get_map_element_hash` with a loader-computed `hashmap_make_hash(Key)`.
   As on ARM64, scope the first attempt to `Fail != 0` guard contexts so body
   `map_get/2` badkey/badmap exception behavior remains untouched.
