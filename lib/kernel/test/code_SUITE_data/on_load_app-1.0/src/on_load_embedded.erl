-module(on_load_embedded).
-export([status/0]).
-on_load(run_me/0).

run_me() ->
    %% An onload handler typically calls code:priv_dir/1
    %% or code:lib_dir/1, so make sure that it works.
    LibDir = code:lib_dir(on_load_app),
    PrivDir = code:priv_dir(on_load_app),
    LibDir = filename:dirname(PrivDir),
    ModPath = filename:join(filename:split(code:which(?MODULE))),
    LibDir = filename:dirname(filename:dirname(ModPath)),

    %% Start a process to remember that the on_load was called.
    spawn(fun() ->
		  register(everything_is_fine, self()),
		  receive Any ->
			  ok
		  end
	  end),
    ok.

status() ->
    case whereis(everything_is_fine) of
	Pid when is_pid(Pid) ->
	    ok
    end.
