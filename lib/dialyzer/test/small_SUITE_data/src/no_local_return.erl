-module(no_local_return).

%% NOTE: No function is exported.  Dialyzer produced a bogus
%%      'Function foo/0 has no local return' warning
%% when in fact typer was finding correct return values for both
%% these functions.

foo() ->
 bar(42).

bar(X) ->
 lists:duplicate(X, gazonk).
