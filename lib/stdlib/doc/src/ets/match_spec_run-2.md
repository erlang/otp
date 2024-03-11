Executes the matching specified in a compiled
[match specification](`m:ets#match_spec`) on a list of terms. Term
`CompiledMatchSpec` is to be the result of a call to `match_spec_compile/1` and
is hence the internal representation of the match specification one wants to
use.

The matching is executed on each element in `List` and the function returns a
list containing all results. If an element in `List` does not match, nothing is
returned for that element. The length of the result list is therefore equal or
less than the length of parameter `List`.

_Example:_

The following two calls give the same result (but certainly not the same
execution time):

```erlang
Table = ets:new...
MatchSpec = ...
% The following call...
ets:match_spec_run(ets:tab2list(Table),
                   ets:match_spec_compile(MatchSpec)),
% ...gives the same result as the more common (and more efficient)
ets:select(Table, MatchSpec),
```

> #### Note {: .info }
>
> This function has limited use in normal code. It is used by the `m:dets`
> module to perform the `dets:select/1` operations and by Mnesia during
> transactions.
