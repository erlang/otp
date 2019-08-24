-module(bif1).

%% Other set of warnings due to removed of functions from
%% erl_bif_types.

-export([ets_rename/0, string_chars/0]).

ets_rename() ->
    A = ets:new(fipp, []),
    true = not is_atom(A),
    ets:rename(A, fopp). % No warning

string_chars() ->
    L2 = bif1_adt:opaque_string(),
    S = $A,
    string:chars(S, 10, L2). % Warning
