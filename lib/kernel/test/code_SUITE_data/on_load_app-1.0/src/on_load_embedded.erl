-module(on_load_embedded).
-export([status/0]).
-on_load(run_me/0).

run_me() ->
    spawn(fun() ->
		  register(everything_is_fine, self()),
		  receive Any ->
			  ok
		  end
	  end),
    true.

status() ->
    case whereis(everything_is_fine) of
	Pid when is_pid(Pid) ->
	    ok
    end.
