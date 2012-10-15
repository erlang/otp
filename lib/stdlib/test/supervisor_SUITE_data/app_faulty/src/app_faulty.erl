-module(app_faulty).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    case app_faulty_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

stop(_State) ->
    ok.
