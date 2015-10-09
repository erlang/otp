%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: mnesia_late_loader.erl,v 1.1 2008/12/17 09:53:38 mikpe Exp $
%%
-module(mnesia_late_loader).

-export([
	 async_late_disc_load/3,
	 maybe_async_late_disc_load/3,
	 init/1,
	 start/0
	]).

%% sys callback functions
-export([
	 system_continue/3,
	 system_terminate/4,
	 system_code_change/4
	]).

-define(SERVER_NAME, ?MODULE).

-record(state, {supervisor}).

async_late_disc_load(Node, Tabs, Reason) ->
    Msg = {async_late_disc_load, Tabs, Reason},
    catch ({?SERVER_NAME, Node} ! {self(), Msg}).

maybe_async_late_disc_load(Node, Tabs, Reason) ->
    Msg = {maybe_async_late_disc_load, Tabs, Reason},
    catch ({?SERVER_NAME, Node} ! {self(), Msg}).

start() ->
    mnesia_monitor:start_proc(?SERVER_NAME, ?MODULE, init, [self()]).

init(Parent) ->
    %% Trap exit omitted intentionally
    register(?SERVER_NAME, self()),
    link(whereis(mnesia_controller)),  %% We may not hang
    mnesia_controller:merge_schema(),
    unlink(whereis(mnesia_controller)),
    mnesia_lib:set(mnesia_status, running),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(#state{supervisor = Parent}).

loop(State) ->
    receive
	{_From, {async_late_disc_load, Tabs, Reason}} ->
	    mnesia_controller:schedule_late_disc_load(Tabs, Reason),
	    loop(State);

	{_From, {maybe_async_late_disc_load, Tabs, Reason}} ->
	    GoodTabs =
		[T || T <- Tabs,
		      lists:member(node(),
				   mnesia_recover:get_master_nodes(T))],
	    mnesia_controller:schedule_late_disc_load(GoodTabs, Reason),
	    loop(State);

	{system, From, Msg} ->
	    mnesia_lib:dbg_out("~p got {system, ~p, ~p}~n",
			       [?SERVER_NAME, From, Msg]),
	    Parent = State#state.supervisor,
	    sys:handle_system_msg(Msg, From, Parent, ?MODULE, [], State);

	Msg ->
	    mnesia_lib:error("~p got unexpected message: ~p~n",
			     [?SERVER_NAME, Msg]),
	    loop(State)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% System upgrade

system_continue(_Parent, _Debug, State) ->
    loop(State).

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.
