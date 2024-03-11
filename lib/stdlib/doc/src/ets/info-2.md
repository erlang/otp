Returns the information associated with `Item` for table `Table`, or returns
`undefined` if `Table` does not refer an existing ETS table. If `Table` is not
of the correct type, or if `Item` is not one of the allowed values, a `badarg`
exception is raised.

In addition to the `{Item,Value}` pairs defined for `info/1`, the following
items are allowed:

- `Item=binary, Value=BinInfo`

  `BinInfo` is a list containing miscellaneous information about binaries kept
  by the table. This `Item` can be changed or removed without prior notice. In
  the current implementation `BinInfo` is a list of tuples
  `{BinaryId,BinarySize,BinaryRefcCount}`.

- `Item=fixed, Value=boolean()`

  Indicates if the table is fixed by any process.

- [](){: #info_2_safe_fixed_monotonic_time }

  `Item=safe_fixed|safe_fixed_monotonic_time, Value={FixationTime,Info}|false`

  If the table is fixed using `safe_fixtable/2`, the call returns a tuple where
  `FixationTime` is the last time when the table changed from unfixed to fixed.

  The format and value of `FixationTime` depends on `Item`:

  - **`safe_fixed`** - `FixationTime` corresponds to the result returned by
    `erlang:timestamp/0` at the time of fixation. Notice that when the system
    uses single or multi
    [time warp modes](`e:erts:time_correction.md#time-warp-modes`) this can
    produce strange results, as the use of `safe_fixed` is not
    [time warp safe](`e:erts:time_correction.md#time-warp-safe-code`). Time warp
    safe code must use `safe_fixed_monotonic_time` instead.

  - **`safe_fixed_monotonic_time`** - `FixationTime` corresponds to the result
    returned by `erlang:monotonic_time/0` at the time of fixation. The use of
    `safe_fixed_monotonic_time` is
    [time warp safe](`e:erts:time_correction.md#time-warp-safe-code`).

  `Info` is a possibly empty lists of tuples `{Pid,RefCount}`, one tuple for
  every process the table is fixed by now. `RefCount` is the value of the
  reference counter and it keeps track of how many times the table has been
  fixed by the process.

  Table fixations are not limited to `safe_fixtable/2`. Temporary fixations may
  also be done by for example [traversing functions](`m:ets#traversal`) like
  `select` and `match`. Such table fixations are automatically released before
  the corresponding functions returns, but they may be seen by a concurrent call
  to `ets:info(T,safe_fixed|safe_fixed_monotonic_time)`.

  If the table is not fixed at all, the call returns `false`.

- `Item=stats, Value=tuple()`

  Returns internal statistics about tables on an internal format used by OTP
  test suites. Not for production use.

> #### Note {: .info }
>
> The execution time of this function is affected by the
> [`decentralized_counters`](`m:ets#new_2_decentralized_counters`) table option
> when the second argument of the function is `size` or `memory`. The execution
> time is much longer when the `decentralized_counters` option is set to `true`
> than when the `decentralized_counters` option is set to `false`.
