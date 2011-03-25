%%
%%  wings_io.erl --
%%
%%     This module contains most of the low-level GUI for Wings.
%%

-module(wings_io).

-export([get_matching_events/1]).

-define(EVENT_QUEUE, wings_io_event_queue).

%%%
%%% Input.
%%%

get_matching_events(Filter) ->
    Eq = get(?EVENT_QUEUE),
    get_matching_events_1(Filter, Eq, [], []).

get_matching_events_1(Filter, Eq0, Match, NoMatch) ->
    case queue:out(Eq0) of
	{{value,Ev},Eq} ->
	    case Filter(Ev) of
		false ->
		    get_matching_events_1(Filter, Eq, Match, [Ev|NoMatch]);
		true ->
		    get_matching_events_1(Filter, Eq, [Ev|Match], NoMatch)
	    end;
	{empty,{In,Out}} ->
	    case Match of
		[] -> [];
		_ ->
		    put(?EVENT_QUEUE, {In, lists:reverse(NoMatch, Out)}),
		    Match
	    end
    end.
