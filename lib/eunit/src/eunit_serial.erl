%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc Event serializing and multiplexing process, to be used as the
%% main "supervisor" process for en EUnit test runner. See eunit_proc
%% for details about the events that will be sent to the listeners
%% (provided to this process at startup). This process guarantees that
%% listeners will receive events in order, even if tests execute in
%% parallel. For every received 'begin' event, there will be exactly one
%% 'end' or 'cancel' event. For a cancelling event with identifier Id,
%% no further events will arrive whose identifiers have Id as prefix.

-module(eunit_serial).

-include("eunit.hrl").
-include("eunit_internal.hrl").

-export([start/1]).

%% Notes:
%% * Due to concurrency, there are no guarantees that we will receive
%% all status messages for the items within a group before we receive
%% the 'end' message of the group itself.
%% 
%% * A cancelling event may arrive at any time, and may concern items we
%% are not yet expecting (if tests are executed in parallel), or may
%% concern not only the current item but possibly a group ancestor of
%% the current item (as in the case of a group timeout).
%% 
%% * It is not possible to use selective receive to extract only those
%% cancelling messages that affect the current item and its parents;
%% basically, because we cannot have a dynamically computed prefix as a
%% pattern in a receive. Hence, we must extract each cancelling event as
%% it arrives and keep track of them separately.
%% 
%% * Before we wait for a new item, we must check whether it (and thus
%% also all its subitems, if any) is already cancelled.
%% 
%% * When a new cancelling event arrives, we must either store it for
%% future use, and/or cancel the current item and possibly one or more
%% of its parent groups.

-record(state, {listeners			 :: sets:set(),
		cancelled = eunit_lib:trie_new() :: gb_trees:tree(),
		messages  = dict:new()		 :: dict:dict()}).

start(Pids) ->
    spawn(serializer_fun(Pids)).

serializer_fun(Pids) ->
    fun () ->
            St = #state{listeners = sets:from_list(Pids),
                        cancelled = eunit_lib:trie_new(),
                        messages = dict:new()},
            expect([], undefined, 0, St),
            exit(normal)
    end.

%% collect beginning and end of an expected item; return {Done, NewSt}
%% where Done is true if there are no more items of this group
expect(Id, ParentId, GroupMinSize, St0) ->
    case wait(Id, 'begin', ParentId, GroupMinSize, St0) of
	{done, St1} ->
	    {true, St1};
	{cancel, prefix, _Msg, St1} ->
	    %% if a parent caused the cancel, signal done with group and
	    %% cast no cancel event (since the item might not exist)
	    {true, St1};
	{cancel, exact, Msg, St1} ->
	    cast_cancel(Id, Msg, St1),
	    {false, St1};
	{ok, Msg, St1} ->
	    %%?debugVal({got_begin, Id, Msg}),
	    cast(Msg, St1),
	    St2 = case Msg of
		      {status, _, {progress, 'begin', {group, _Info}}} ->
			  group(Id, 0, St1);
		      _ ->
			  St1
		  end,
	    case wait(Id, 'end', ParentId, GroupMinSize, St2) of
		{cancel, Why, Msg1, St3} ->
		    %% we know the item exists, so always cast a cancel
		    %% event, and signal done with the group if a parent
		    %% caused the cancel
		    cast_cancel(Id, Msg1, St3),
		    {(Why =:= prefix), St3};
		{ok, Msg1, St3} ->
		    %%?debugVal({got_end, Id, Msg1}),
		    cast(Msg1, St3),
		    {false, St3}
	    end
    end.

%% collect group items in order until group is done
group(ParentId, GroupMinSize, St) ->
    N = GroupMinSize + 1,
    case expect(ParentId ++ [N], ParentId, GroupMinSize, St) of
	{false, St1} ->
	    group(ParentId, N, St1);
	{true, St1} ->
	    St1
    end.

cast_cancel(Id, undefined, St) ->
    %% reasonable message for implicitly cancelled events
    cast({status, Id, {cancel, undefined}}, St);
cast_cancel(_Id, Msg, St) ->
    cast(Msg, St).

cast(Msg, St) ->
    sets:fold(fun (L, M) -> L ! M end, Msg, St#state.listeners),
    ok.

%% wait for a particular begin or end event, that might have arrived or
%% been cancelled already, or might become cancelled later, or might not
%% even exist (for the last+1 element of a group)
wait(Id, Type, ParentId, GroupMinSize, St) ->
    %%?debugVal({wait, Id, Type}),
    case check_cancelled(Id, St) of
	no ->
	    case recall(Id, St) of
		undefined ->
		    wait_1(Id, Type, ParentId, GroupMinSize, St);
		Msg ->
		    {ok, Msg, forget(Id, St)}
	    end;
	Why ->
	    %%?debugVal({cancelled, Why, Id, ParentId}),
	    {cancel, Why, recall(Id, St), forget(Id, St)}
    end.

%% the event has not yet arrived or been cancelled - wait for more info
wait_1(Id, Type, ParentId, GroupMinSize, St) ->
    receive
	{status, Id, {progress, Type, _}}=Msg ->
	    %%?debugVal({Type, ParentId, Id}),
	    {ok, Msg, St};
	{status, ParentId, {progress, 'end', {GroupMinSize, _}}}=Msg ->
	    %% the parent group ended (the final status of a group is
	    %% the count of its subitems), and we have seen all of its
	    %% subtests, so the currently expected event does not exist
	    %%?debugVal({end_group, ParentId, Id, GroupMinSize}),
	    {done, remember(ParentId, Msg, St)};
	{status, SomeId, {cancel, _Cause}}=Msg ->
	    %%?debugVal({got_cancel, SomeId, _Cause}),
	    St1 = set_cancelled(SomeId, Msg, St),
	    wait(Id, Type, ParentId, GroupMinSize, St1)
    end.

set_cancelled(Id, Msg, St0) ->
    St = remember(Id, Msg, St0),
    St#state{cancelled = eunit_lib:trie_store(Id, St0#state.cancelled)}.

check_cancelled(Id, St) ->
    %% returns 'no', 'exact', or 'prefix'
    eunit_lib:trie_match(Id, St#state.cancelled).

remember(Id, Msg, St) ->
    St#state{messages = dict:store(Id, Msg, St#state.messages)}.

forget(Id, St) ->
    %% this is just to enable garbage collection of old messages
    St#state{messages = dict:store(Id, undefined, St#state.messages)}.

recall(Id, St) ->
    case dict:find(Id, St#state.messages) of
	{ok, Msg} -> Msg;
	error -> undefined
    end.
