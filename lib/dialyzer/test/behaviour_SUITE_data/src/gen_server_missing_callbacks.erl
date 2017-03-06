-module(gen_server_missing_callbacks).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, myserver}, ?MODULE, [], []).

init([]) ->
    ignore.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
