-module(yes_8).
-compile(export_all).

?MODULE() ->
    ok.

%% Cover use of floating point registers.

f(Pid, X) when is_float(X) ->
    Ref = make_ref(),
    Pid ! {X+3},
    receive
	Ref ->
	    ok
    end.
