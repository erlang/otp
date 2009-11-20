-module(ge_h).
-vsn(2).
-behaviour(gen_event).

-export([get_events/1]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, 
	 terminate/2, code_change/3]).

get_events(Mgr) -> 
    gen_event:call(Mgr, ge_h, get_events).

init(_) -> {ok, []}.

handle_event(Event, []) -> 
    {ok, [Event]};
handle_event(Event, [Event1 | _]) -> 
    {ok, [Event, Event1]}.

handle_call(get_events, Events) -> 
    Events.

handle_info(Info, Events) ->
    {ok, Events}.

terminate(Arg, Events) ->
    ok.

code_change(1, undefined, _Extra) -> 
    {ok, []};
code_change(1, LastEvent, _Extra) -> 
    {ok, [LastEvent]}.
