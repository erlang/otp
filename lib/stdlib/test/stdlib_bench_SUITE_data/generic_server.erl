-module(generic_server).

-export([start/1, reply/2, stop/1]).

-export([handle_call/3, handle_cast/2, init/1, terminate/2]).

-behaviour(gen_server).

-define(GEN_SERVER, gen_server).

start(State) ->
    {ok, Pid} = ?GEN_SERVER:start(?MODULE, State, []),
    Pid.

init(State) ->
    {ok, State}.

stop(P) ->
    ok = ?GEN_SERVER:stop(P).

reply(S, M) ->
    _M = ?GEN_SERVER:call(S, {reply, M}, infinity).

handle_call({reply, M}, _From, State) ->
    {reply, M, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
