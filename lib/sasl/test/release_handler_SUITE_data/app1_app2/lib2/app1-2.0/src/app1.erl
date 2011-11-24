-module(app1).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([config_change/3]).

start(_Type, _StartArgs) ->
    case app1_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

stop(_State) ->
    ok.

config_change(Changed, _New, _Removed) ->
    catch ets:insert(otp_6162, hd(Changed)),
    ok.
