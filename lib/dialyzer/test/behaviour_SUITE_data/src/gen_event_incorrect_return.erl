-module(gen_event_incorrect_return).

-behaviour(gen_event).

-export([start_link/0, add_handler/0]).

-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_event:start_link({local, myserver}). 

add_handler() ->
    gen_event:add_handler(myserver, ?MODULE, []).

init([]) ->
    error.

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
