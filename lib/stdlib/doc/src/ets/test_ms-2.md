This function is a utility to test a [match specification](`m:ets#match_spec`)
used in calls to `select/2`. The function both tests `MatchSpec` for "syntactic"
correctness and runs the match specification against object `Tuple`.

If the match specification is syntactically correct, the function either returns
`{ok,Result}`, where `Result` is what would have been the result in a real
[`select/2`](`select/2`) call, or `false` if the match specification does not
match object `Tuple`.

If the match specification contains errors, tuple `{error, Errors}` is returned,
where `Errors` is a list of natural language descriptions of what was wrong with
the match specification.

This is a useful debugging and test tool, especially when writing complicated
[`select/2`](`select/2`) calls.

See also: `erlang:match_spec_test/3`.
