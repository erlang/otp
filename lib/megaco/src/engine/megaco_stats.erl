%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%

%%
%%-----------------------------------------------------------------
%% Purpose: Functions for handling statistic counters
%%-----------------------------------------------------------------
-module(megaco_stats).


%%-----------------------------------------------------------------
%% Application internal exports
%%-----------------------------------------------------------------

-export([init/1, init/2]).

-export([inc/2, inc/3, inc/4]).

-export([get_stats/1, get_stats/2, get_stats/3,
	 reset_stats/1, reset_stats/2]).

%% -include_lib("megaco/include/megaco.hrl").


%%-----------------------------------------------------------------
%% Func: init/1, init/2
%% Description: Initiate the statistics. Creates the stats table 
%%              and the global counters.
%%-----------------------------------------------------------------
init(Name) ->
    init(Name, []).

init(Name, GlobalCounters) ->
    ets:new(Name, [public, named_table, {keypos, 1}]),
    ets:insert(Name, {global_counters, GlobalCounters}),
    create_global_snmp_counters(Name, GlobalCounters).


create_global_snmp_counters(_Name, []) ->
    ok;
create_global_snmp_counters(Name, [Counter|Counters]) ->
    ets:insert(Name, {Counter, 0}),
    create_global_snmp_counters(Name, Counters).


%%-----------------------------------------------------------------
%% Func: inc/2, inc/3, inc/4
%% Description: Increment counter value. Default increment is one 
%%              (1). 
%%-----------------------------------------------------------------
inc(Tab, GlobalCnt) when is_atom(GlobalCnt) ->
    inc(Tab, GlobalCnt, 1).

inc(Tab, GlobalCnt, Incr) 
  when is_atom(GlobalCnt) andalso (is_integer(Incr) andalso (Incr > 0)) ->
    do_inc(Tab, GlobalCnt, Incr);
inc(Tab, Handle, Cnt) 
  when is_atom(Cnt) ->
    inc(Tab, Handle, Cnt, 1).

inc(Tab, Handle, Cnt, Incr) 
  when is_atom(Cnt) andalso (is_integer(Incr) andalso (Incr > 0)) ->
    Key = {Handle, Cnt}, 
    do_inc(Tab, Key, Incr).

do_inc(Tab, Key, Incr) ->
    case (catch ets:update_counter(Tab, Key, Incr)) of
        {'EXIT', {badarg, _Reason}} ->
            ets:insert(Tab, {Key, Incr}),
	    Incr;
        Val ->
            Val
    end.


%%-----------------------------------------------------------------
%% Func: get_stats/1, get_stats/2, get_stats/3 
%% Description: Get statistics
%%-----------------------------------------------------------------
get_stats(Ets) ->
    Handles = get_handles_and_global_counters(Ets),
    (catch do_get_stats(Ets, Handles, [])).

do_get_stats(_Ets, [], Acc) ->
    {ok, lists:reverse(Acc)};
do_get_stats(Ets, [Handle|Handles], Acc) ->
    case get_stats(Ets, Handle) of
	{ok, Stats} ->
	    do_get_stats(Ets, Handles, [{Handle, Stats}|Acc]);
	{error, Reason} ->
	    throw({error, Reason})
    end.

get_stats(Ets, GlobalCounter) when is_atom(GlobalCounter) ->
    case (catch ets:lookup(Ets, GlobalCounter)) of
	[{GlobalCounter, Val}] ->
	    {ok, Val};
	[] ->
	    {error, {no_such_counter, GlobalCounter}}
    end;

get_stats(Ets, Handle) ->
    case (catch ets:match(Ets, {{Handle, '$1'},'$2'})) of
	CounterVals when is_list(CounterVals) ->
	    {ok, [{Counter, Val} || [Counter, Val] <- CounterVals]};
	Other ->
	    {error, {unexpected_result, Other}}
    end.


get_stats(Ets, Handle, Counter) when is_atom(Counter) ->
    Key = {Handle, Counter}, 
    case (catch ets:lookup(Ets, Key)) of
	[{Key, Val}] ->
	    {ok, Val};
	_ ->
	    {error, {undefined_counter, Counter}}
    end.


%%-----------------------------------------------------------------
%% Funcs: reset_stats/1, reset_stats/2
%% Description: Reset statistics 
%%-----------------------------------------------------------------
reset_stats(Ets) ->
    Handles = get_handles_and_global_counters(Ets),
    (catch do_reset_stats(Ets, Handles, [])).

do_reset_stats(_Ets, [], Acc) ->
    {ok, lists:reverse(Acc)};
do_reset_stats(Ets, [Handle|Handles], Acc) ->
    case reset_stats(Ets, Handle) of
	{ok, OldStats} ->
	    do_reset_stats(Ets, Handles, [{Handle, OldStats}|Acc]);
	{error, Reason} ->
	    throw({error, Reason})
    end.

reset_stats(Ets, GlobalCounter) when is_atom(GlobalCounter) ->
    %% First get the current value of the counter
    case (catch ets:lookup(Ets, GlobalCounter)) of
	[{GlobalCounter, Val}] ->
	    ets:insert(Ets, {GlobalCounter, 0}),
	    {ok, Val};
	[] -> %% Oooups
	    {error, {no_such_counter, GlobalCounter}}
    end;

reset_stats(Ets, Handle) ->
    case (catch ets:match(Ets, {{Handle, '$1'},'$2'})) of
	CounterVals when is_list(CounterVals) ->
	    CVs = [{Counter, Val} || [Counter, Val] <- CounterVals],
	    reset_stats(Ets, Handle, CVs),
	    {ok, CVs};
	Other ->
	    {error, {unexpected_result, Other}}
    end.

reset_stats(_Ets, _Handle, []) ->
    ok;
reset_stats(Ets, Handle, [{Counter, _}|CVs]) ->
    ets:insert(Ets, {{Handle, Counter}, 0}),
    reset_stats(Ets, Handle, CVs).



%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
get_handles_and_global_counters(Ets) ->
    GlobalCounters = 
	case ets:lookup(Ets, global_counters) of
	    [{global_counters, GC}] ->
		GC;
	    [] ->
		[]
	end,
    L1 = ets:match(Ets, {{'$1', '_'}, '_'}),
    GlobalCounters ++ 
	lists:sort([Handle || [Handle] <- remove_duplicates(L1, [])]).
    
remove_duplicates([], L) ->
    L;
remove_duplicates([H|T], L) ->
    case lists:member(H,T) of
        true ->
            remove_duplicates(T, L);
        false ->
            remove_duplicates(T, [H|L])
    end.

