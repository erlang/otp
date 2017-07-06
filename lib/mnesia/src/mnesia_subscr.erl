%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(mnesia_subscr).

-behaviour(gen_server).

-export([start/0,
	 set_debug_level/1,
	 subscribe/2,
	 unsubscribe/2,
	 unsubscribe_table/1,
	 subscribers/0,
	 report_table_event/4,
	 report_table_event/5, 
	 report_table_event/6,
	 report_activity/1
	]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

-compile({no_auto_import,[error/2]}).

-include("mnesia.hrl").

-import(mnesia_lib, [error/2]).
-record(state, {supervisor, pid_tab}).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [self()],
			  [{timeout, infinity}]).

set_debug_level(Level) ->
    OldEnv = application:get_env(mnesia, debug),
    case mnesia_monitor:patch_env(debug, Level) of
	{error, Reason} ->
	    {error, Reason};
	NewLevel ->
	    set_debug_level(NewLevel, OldEnv)
    end.

set_debug_level(Level, OldEnv) ->
    case mnesia:system_info(is_running) of
	no when OldEnv == undefined ->
	    none;
	no ->
	    {ok, E} = OldEnv,
	    E;
	_ ->
	    Old = mnesia_lib:val(debug),
	    Local = mnesia:system_info(local_tables),
	    E = whereis(mnesia_event),
	    Sub = fun(Tab) -> subscribe(E, {table, Tab}) end,
	    UnSub = fun(Tab) -> unsubscribe(E, {table, Tab}) end,
	
	    case Level of
		none ->
		    lists:foreach(UnSub, Local);
		verbose ->
		    lists:foreach(UnSub, Local);
		debug ->
		    lists:foreach(UnSub, Local -- [schema]),
		    Sub(schema);
		trace ->
		    lists:foreach(Sub, Local)
	    end,
	    mnesia_lib:set(debug, Level),
	    Old
    end.

subscribe(ClientPid, system) ->
    change_subscr(activate, ClientPid, system);
subscribe(ClientPid, activity) ->
    change_subscr(activate, ClientPid, activity);
subscribe(ClientPid, {table, Tab}) ->
    change_subscr(activate, ClientPid, {table, Tab, simple});
subscribe(ClientPid, {table, Tab, simple}) ->
    change_subscr(activate, ClientPid, {table, Tab, simple});
subscribe(ClientPid, {table, Tab, detailed}) ->
    change_subscr(activate, ClientPid, {table, Tab, detailed});
subscribe(_ClientPid, What) ->
    {error, {badarg, What}}.

unsubscribe(ClientPid, system) ->
    change_subscr(deactivate, ClientPid, system);
unsubscribe(ClientPid, activity) ->
    change_subscr(deactivate, ClientPid, activity);
unsubscribe(ClientPid, {table, Tab}) ->
    change_subscr(deactivate, ClientPid, {table, Tab, simple});
unsubscribe(ClientPid, {table, Tab, simple}) ->
    change_subscr(deactivate, ClientPid, {table, Tab, simple});
unsubscribe(ClientPid, {table, Tab, detailed}) ->
    change_subscr(deactivate, ClientPid, {table, Tab, detailed});
unsubscribe(_ClientPid, What) ->
    {error, {badarg, What}}.

unsubscribe_table(Tab) ->
    call({change, {deactivate_table, Tab}}).

change_subscr(Kind, ClientPid, What) ->
    call({change, {Kind, ClientPid, What}}).

subscribers() ->
    [whereis(mnesia_event) | mnesia_lib:val(subscribers)].

report_activity({dirty, _pid}) -> 
    ok;
report_activity(Tid) ->
    case ?catch_val(activity_subscribers) of
	{'EXIT', _} -> ok;
	Subscribers ->
	    deliver(Subscribers, {mnesia_activity_event, {complete, Tid}})
	end.

report_table_event(Tab, Tid, Obj, Op) ->   
    case ?catch_val({Tab, commit_work}) of
	{'EXIT', _} -> ok;
	Commit ->
	    case lists:keysearch(subscribers, 1, Commit) of
		false -> ok;
		{value, Subs} -> 
		    report_table_event(Subs, Tab, Tid, Obj, Op, undefined)
	    end
    end.

%% Backwards compatible for the moment when mnesia_tm get's updated!
report_table_event(Subscr, Tab, Tid, Obj, Op) ->
    report_table_event(Subscr, Tab, Tid, Obj, Op, undefined).

report_table_event({subscribers, S1, S2}, Tab, Tid, _Obj, clear_table, _Old) ->
    What   = {delete, {schema, Tab}, Tid},
    deliver(S1, {mnesia_table_event, What}),
    TabDef = mnesia_schema:cs2list(?catch_val({Tab, cstruct})),
    What2  = {write, {schema, Tab, TabDef}, Tid},
    deliver(S1, {mnesia_table_event, What2}),
    What3  = {delete, schema, {schema, Tab}, [{schema, Tab, TabDef}], Tid},
    deliver(S2, {mnesia_table_event, What3}),
    What4  = {write, schema,  {schema, Tab, TabDef}, [], Tid},
    deliver(S2, {mnesia_table_event, What4});

report_table_event({subscribers, Subscr, []}, Tab, Tid, Obj, Op, _Old) ->
    What = {Op, patch_record(Tab, Obj), Tid},
    deliver(Subscr, {mnesia_table_event, What});

report_table_event({subscribers, S1, S2}, Tab, Tid, Obj, Op, Old) ->
    Standard = {Op, patch_record(Tab, Obj), Tid},
    deliver(S1, {mnesia_table_event, Standard}), 
    Extended = what(Tab, Tid, Obj, Op, Old), 
    deliver(S2, Extended);

%% Backwards compatible for the moment when mnesia_tm get's updated!
report_table_event({subscribers, Subscr}, Tab, Tid, Obj, Op, Old) ->    
    report_table_event({subscribers, Subscr, []}, Tab, Tid, Obj, Op, Old).


patch_record(Tab, Obj) ->
    case Tab == element(1, Obj) of
	true -> 
	    Obj;
	false ->
	    setelement(1, Obj, Tab)
    end.

what(Tab, Tid, {RecName, Key}, delete, undefined) ->
    try mnesia_lib:db_get(Tab, Key) of
	Old -> %% Op only allowed for set table.
	    {mnesia_table_event, {delete, Tab, {RecName, Key}, Old, Tid}}
    catch error:_ ->
	    %% Record just deleted by a dirty_op or
	    %% the whole table has been deleted
	    ignore
    end;
what(Tab, Tid, Obj, delete, Old) ->
    {mnesia_table_event, {delete, Tab, Obj, Old, Tid}};
what(Tab, Tid, Obj, delete_object, _Old) ->
    {mnesia_table_event, {delete, Tab, Obj, [Obj], Tid}};
what(Tab, Tid, Obj, write, undefined) ->
    try	mnesia_lib:db_get(Tab, element(2, Obj)) of
	Old ->
	    {mnesia_table_event, {write, Tab, Obj, Old, Tid}}
    catch error:_ ->
	    ignore
    end;
what(Tab, Tid, Obj, write, Old) ->
    {mnesia_table_event, {write, Tab, Obj, Old, Tid}}.

deliver(_, ignore) -> 
    ok;
deliver([Pid | Pids], Msg) ->
    Pid ! Msg,
    deliver(Pids, Msg);
deliver([], _Msg) ->
    ok.

call(Msg) ->
    Pid = whereis(?MODULE),
    case Pid of
	undefined ->
	    {error, {node_not_running, node()}};
	Pid ->
	    Res = gen_server:call(Pid, Msg, infinity),
            %% We get an exit signal if server dies
            receive
                {'EXIT', Pid, _Reason} ->
                    {error, {node_not_running, node()}}
            after 0 ->
                    Res
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callback functions from gen_server

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([Parent]) ->
    process_flag(trap_exit, true),
    ClientPid = whereis(mnesia_event),
    link(ClientPid),
    mnesia_lib:verbose("~p starting: ~p~n", [?MODULE, self()]),
    Tab = ?ets_new_table(mnesia_subscr, [duplicate_bag, private]),
    ?ets_insert(Tab, {ClientPid, system}),
    {ok, #state{supervisor = Parent, pid_tab = Tab}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({change, How}, _From, State) ->
    Reply = do_change(How, State#state.pid_tab),
    {reply, Reply, State};

handle_call(Msg, _From, State) ->
    error("~p got unexpected call: ~tp~n", [?MODULE, Msg]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    error("~p got unexpected cast: ~tp~n", [?MODULE, Msg]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_info({'EXIT', Pid, _R}, State) when Pid == State#state.supervisor ->
    {stop, shutdown, State};

handle_info({'EXIT', Pid, _Reason}, State) ->
    handle_exit(Pid, State#state.pid_tab),
    {noreply, State};

handle_info(Msg, State) ->
    error("~p got unexpected info: ~tp~n", [?MODULE, Msg]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    prepare_stop(State#state.pid_tab),
    mnesia_monitor:terminate_proc(?MODULE, Reason, State).

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Upgrade process when its code is to be changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

do_change({activate, ClientPid, system}, SubscrTab) when is_pid(ClientPid) ->
    Var = subscribers,
    activate(ClientPid, system, Var, subscribers(), SubscrTab);
do_change({activate, ClientPid, activity}, SubscrTab) when is_pid(ClientPid) ->
    Var = activity_subscribers,
    activate(ClientPid, activity, Var, mnesia_lib:val(Var), SubscrTab);
do_change({activate, ClientPid, {table, Tab, How}}, SubscrTab) when is_pid(ClientPid) ->
    case ?catch_val({Tab, where_to_read}) of
	Node when Node == node() ->
	    Var = {Tab, commit_work},
	    activate(ClientPid, {table, Tab, How}, Var, mnesia_lib:val(Var), SubscrTab);
	{'EXIT', _} ->
	    {error, {no_exists, Tab}};
	_Node ->
	    {error, {not_active_local, Tab}}
    end;
do_change({deactivate, ClientPid, system}, SubscrTab) ->
    Var = subscribers,
    deactivate(ClientPid, system, Var, SubscrTab);
do_change({deactivate, ClientPid, activity}, SubscrTab) ->
    Var = activity_subscribers,
    deactivate(ClientPid, activity, Var, SubscrTab);
do_change({deactivate, ClientPid, {table, Tab, How}}, SubscrTab) ->
    Var = {Tab, commit_work},
    deactivate(ClientPid, {table, Tab, How}, Var, SubscrTab);
do_change({deactivate_table, Tab}, SubscrTab) ->
    Var = {Tab, commit_work},
    case ?catch_val(Var) of
	{'EXIT', _} ->
	    {error, {no_exists, Tab}};
	CommitWork ->
	    case lists:keysearch(subscribers, 1, CommitWork) of
		false ->
		    ok;
		{value, Subs} -> 
		    Simple   = {table, Tab, simple}, 
		    Detailed = {table, Tab, detailed}, 
		    Fs = fun(C) -> deactivate(C, Simple, Var, SubscrTab) end,
		    Fd = fun(C) -> deactivate(C, Detailed, Var, SubscrTab) end,
		    case Subs of
			{subscribers, L1, L2} -> 
			    lists:foreach(Fs, L1),
			    lists:foreach(Fd, L2);
			{subscribers, L1} ->
			    lists:foreach(Fs, L1)
		    end
	    end,
	    {ok, node()}
    end;
do_change(_, _) ->
    {error, badarg}.

activate(ClientPid, What, Var, OldSubscribers, SubscrTab) ->
    Old = 
	if Var == subscribers orelse Var == activity_subscribers ->
		OldSubscribers;
	   true -> 
		case lists:keysearch(subscribers, 1, OldSubscribers) of
		    false -> [];
		{value, Subs} -> 
			case Subs of
			    {subscribers, L1, L2} -> 
				L1 ++ L2;
			    {subscribers, L1} ->
				L1
			end
		end
	end,
    case lists:member(ClientPid, Old) of
	false ->
	    %% Don't care about checking old links
	    try link(ClientPid) of
		true ->
		    ?ets_insert(SubscrTab, {ClientPid, What}),
		    add_subscr(Var, What, ClientPid),
		    {ok, node()}
	    catch error:_ ->
		    {error, {no_exists, ClientPid}}
	    end;
	true ->
	    {error, {already_exists, What}}
    end.

%%-record(subscribers, {pids = []}).  Old subscriber record removed
%% To solve backward compatibility, this code is a cludge.. 
add_subscr(subscribers, _What, Pid) ->
    mnesia_lib:add(subscribers, Pid),
    {ok, node()};
add_subscr(activity_subscribers, _What, Pid) ->
    mnesia_lib:add(activity_subscribers, Pid),
    {ok, node()};
add_subscr({Tab, commit_work}, What, Pid) ->
    Commit = mnesia_lib:val({Tab, commit_work}),
    case lists:keysearch(subscribers, 1, Commit) of
	false ->
	    Subscr = 
		case What of 
		    {table, _, simple} -> 
			{subscribers, [Pid], []};
		    {table, _, detailed} ->
			{subscribers, [], [Pid]}
		end,
	    mnesia_lib:add({Tab, subscribers}, Pid),
	    mnesia_lib:set({Tab, commit_work}, 
			   mnesia_lib:sort_commit([Subscr | Commit]));
	{value, Old} ->
	    {L1, L2} = 
		case Old of
		    {subscribers, L} ->  %% Old Way
			{L, []};
		    {subscribers, SL1, SL2} -> 
			{SL1, SL2}
		end,
	    Subscr = 
		case What of 
		    {table, _, simple} -> 
			{subscribers, [Pid | L1], L2};
		    {table, _, detailed} ->  
			{subscribers, L1, [Pid | L2]}
		end,
	    NewC  = lists:keyreplace(subscribers, 1, Commit, Subscr),
	    mnesia_lib:set({Tab, commit_work}, 
			   mnesia_lib:sort_commit(NewC)),
	    mnesia_lib:add({Tab, subscribers}, Pid)
    end.

deactivate(ClientPid, What, Var, SubscrTab) ->
    ?ets_match_delete(SubscrTab, {ClientPid, What}),
    try
	?ets_lookup_element(SubscrTab, ClientPid, 1),
	ignore
    catch error:_ -> unlink(ClientPid)
    end,
    try
	del_subscr(Var, What, ClientPid),
	{ok, node()}
    catch _:_ ->
	    {error, badarg}
    end.

del_subscr(subscribers, _What, Pid) ->
    mnesia_lib:del(subscribers, Pid);
del_subscr(activity_subscribers, _What, Pid) ->
    mnesia_lib:del(activity_subscribers, Pid);
del_subscr({Tab, commit_work}, What, Pid) ->
    Commit = mnesia_lib:val({Tab, commit_work}),
    case lists:keysearch(subscribers, 1, Commit) of
	false ->
	    false;
	{value, Old} ->
	    {L1, L2} = 
		case Old of
		    {subscribers, L} ->  %% Old Way
			{L, []};
		    {subscribers, SL1, SL2} -> 
			{SL1, SL2}
		end,
	    Subscr = 
		case What of %% Ignore user error delete subscr from any list
		    {table, _, simple} -> 
			NewL1 = lists:delete(Pid, L1),
			NewL2 = lists:delete(Pid, L2),
			{subscribers, NewL1, NewL2};
		    {table, _, detailed} ->
			NewL1 = lists:delete(Pid, L1),
			NewL2 = lists:delete(Pid, L2),
			{subscribers, NewL1, NewL2}
		end,
	    case Subscr of
		{subscribers, [], []} ->
		    NewC = lists:keydelete(subscribers, 1, Commit),
		    mnesia_lib:del({Tab, subscribers}, Pid),
		    mnesia_lib:set({Tab, commit_work}, 
				   mnesia_lib:sort_commit(NewC));
		_ ->
		    NewC = lists:keyreplace(subscribers, 1, Commit, Subscr),
		    mnesia_lib:del({Tab, subscribers}, Pid),
		    mnesia_lib:set({Tab, commit_work}, 
				   mnesia_lib:sort_commit(NewC))
	    end
    end.

handle_exit(ClientPid, SubscrTab) ->
    do_handle_exit(?ets_lookup(SubscrTab, ClientPid)),
    ?ets_delete(SubscrTab, ClientPid).

do_handle_exit([{ClientPid, What} | Tail]) ->
    case What of
	system ->
	    del_subscr(subscribers, What, ClientPid);
	activity ->
	    del_subscr(activity_subscribers, What, ClientPid);
	{_, Tab, _Level} ->
	    del_subscr({Tab, commit_work}, What, ClientPid)    
    end,
    do_handle_exit(Tail);
do_handle_exit([]) ->
    ok.

prepare_stop(SubscrTab) ->
    mnesia_lib:report_system_event({mnesia_down, node()}),
    do_prepare_stop(?ets_first(SubscrTab), SubscrTab).

do_prepare_stop('$end_of_table', _SubscrTab) ->
    ok;
do_prepare_stop(ClientPid, SubscrTab) ->
    Next = ?ets_next(SubscrTab, ClientPid),
    handle_exit(ClientPid, SubscrTab),
    unlink(ClientPid),
    do_prepare_stop(Next, SubscrTab).

