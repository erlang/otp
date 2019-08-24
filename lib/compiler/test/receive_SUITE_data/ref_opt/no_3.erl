-module(no_3).
-compile(export_all).

?MODULE() ->
    ok.

f(X) ->
    Ref = case X of
	      false -> ref;
	      true -> make_ref()
	  end,
    receive
	Ref -> ok
    end.
