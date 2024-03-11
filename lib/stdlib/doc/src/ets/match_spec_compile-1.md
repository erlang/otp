Transforms a [match specification](`m:ets#match_spec`) into an internal
representation that can be used in subsequent calls to `match_spec_run/2`. The
internal representation is opaque. To check the validity of a compiled match
specification, use `is_compiled_ms/1`.

If term `MatchSpec` does not represent a valid match specification, a `badarg`
exception is raised.

> #### Note {: .info }
>
> This function has limited use in normal code. It is used by the `m:dets`
> module to perform the `dets:select/1` operations.
