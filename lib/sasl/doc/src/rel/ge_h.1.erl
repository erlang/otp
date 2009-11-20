-module(ge_h).
-vsn(1).
-behaviour(gen_event).

-export([get_events/1]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, 
	 terminate/2, code_change/3]).

get_events(Mgr) -> 
    gen_event:call(Mgr, ge_h, get_events).

init(_) -> {ok, undefined}.

handle_event(Event, _LastEvent) -> 
    {ok, Event}.

handle_call(get_events, LastEvent) -> 
    {ok, [LastEvent], LastEvent}.

handle_info(Info, LastEvent) ->
    {ok, LastEvent}.

terminate(Arg, LastEvent) ->
    ok.

code_change(_OldVsn, LastEvent, _Extra) ->
    {ok, LastEvent}.

