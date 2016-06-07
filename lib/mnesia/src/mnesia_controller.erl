%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
%% The mnesia_init process loads tables from local disc or from
%% another nodes. It also coordinates updates of the info about
%% where we can read and write tables.
%%
%% Tables may need to be loaded initially at startup of the local
%% node or when other nodes announces that they already have loaded
%% tables that we also want.
%%
%% Initially we set the load request queue to those tables that we
%% safely can load locally, i.e. tables where we have the last
%% consistent replica and we have received mnesia_down from all
%% other nodes holding the table. Then we let the mnesia_init
%% process enter its normal working state.
%%
%% When we need to load a table we append a request to the load
%% request queue. All other requests are regarded as high priority
%% and are processed immediately (e.g. update table whereabouts).
%% We processes the load request queue as a "background" job..

-module(mnesia_controller).

-behaviour(gen_server).

%% Mnesia internal stuff
-export([
	 start/0,
	 i_have_tab/1,
	 info/0,
	 get_info/1,
	 get_workers/1,
	 force_load_table/1,
	 async_dump_log/1,
	 sync_dump_log/1,
	 snapshot_dcd/1,
	 connect_nodes/1,
         connect_nodes/2,
	 wait_for_schema_commit_lock/0,
	 release_schema_commit_lock/0,
	 create_table/1,
	 get_disc_copy/1,
	 get_remote_cstructs/0,  % new function
	 get_cstructs/0,         % old function
	 sync_and_block_table_whereabouts/4,
	 sync_del_table_copy_whereabouts/2,
	 block_table/1,
	 unblock_table/1,
	 block_controller/0,
	 unblock_controller/0,
	 unannounce_add_table_copy/2,
	 master_nodes_updated/2,
	 mnesia_down/1,
	 add_active_replica/2,
	 add_active_replica/3,
	 add_active_replica/4,
	 update/1,
	 change_table_access_mode/1,
	 change_table_majority/1,
	 del_active_replica/2,
	 wait_for_tables/2,
	 get_network_copy/3,
	 merge_schema/0,
	 start_remote_sender/4,
	 schedule_late_disc_load/2
	]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%% Module internal stuff
-export([call/1,
	 cast/1,
	 dump_and_reply/2,
	 load_and_reply/2,
	 send_and_reply/2,
	 wait_for_tables_init/2,
	 connect_nodes2/3
	]).

-compile({no_auto_import,[error/2]}).

-import(mnesia_lib, [set/2, add/2]).
-import(mnesia_lib, [fatal/2, error/2, verbose/2, dbg_out/2]).

-include("mnesia.hrl").

-define(SERVER_NAME, ?MODULE).

-record(state, {supervisor,
		schema_is_merged = false,
		early_msgs = [],
		loader_pid = [],     %% Was Pid is now [{Pid,Work}|..]
		loader_queue,        %% Was list is now gb_tree
		sender_pid = [],     %% Was a pid or undef is now [{Pid,Work}|..]
		sender_queue =  [],
		late_loader_queue,   %% Was list is now gb_tree
		dumper_pid,          %% Dumper or schema commit pid
		dumper_queue = [],   %% Dumper or schema commit queue
		others = [],         %% Processes that needs the copier_done msg
		dump_log_timer_ref,
		is_stopping = false
	       }).
%% Backwards Comp. Sender_pid is now a list of senders..
get_senders(#state{sender_pid = Pids}) when is_list(Pids) -> Pids.
%% Backwards Comp. loader_pid is now a list of loaders..
get_loaders(#state{loader_pid = Pids}) when is_list(Pids) -> Pids.
max_loaders() ->
    case ?catch_val(no_table_loaders) of
	{'EXIT', _} ->
	    mnesia_lib:set(no_table_loaders,1),
	    1;
	Val -> Val
    end.

-record(schema_commit_lock, {owner}).
-record(block_controller, {owner}).

-record(dump_log, {initiated_by,
		   opt_reply_to,
		   operation = dump_log
		  }).

-record(net_load, {table,
		   reason,
		   opt_reply_to,
		   cstruct = unknown
		  }).

-record(send_table, {table,
		     receiver_pid,
		     remote_storage
		    }).

-record(disc_load, {table,
		    reason,
		    opt_reply_to
		   }).

-record(late_load, {table,
		    reason,
		    opt_reply_to,
		    loaders
		   }).

-record(loader_done, {worker_pid,
		      is_loaded,
		      table_name,
		      needs_announce,
		      needs_sync,
		      needs_reply,
		      reply_to,
		      reply}).

-record(sender_done, {worker_pid,
		      worker_res,
		      table_name
		     }).

-record(dumper_done, {worker_pid,
		      worker_res
		     }).

val(Var) ->
    case ?catch_val(Var) of
	{'EXIT', _} -> mnesia_lib:other_val(Var);
	Value -> Value
    end.

start() ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, [self()],
			  [{timeout, infinity}
			   %% ,{debug, [trace]}
			  ]).

sync_dump_log(InitBy) ->
    call({sync_dump_log, InitBy}).

async_dump_log(InitBy) ->
    ?SERVER_NAME ! {async_dump_log, InitBy},
    ok.

snapshot_dcd(Tables) when is_list(Tables) ->
    case [T || T <- Tables,
	       mnesia_lib:storage_type_at_node(node(), T) =/= disc_copies] of
	[] ->
	    call({snapshot_dcd, Tables});
	BadTabs ->
	    {error, {not_disc_copies, BadTabs}}
    end.

%% Wait for tables to be active
%% If needed, we will wait for Mnesia to start
%% If Mnesia stops, we will wait for Mnesia to restart
%% We will wait even if the list of tables is empty
%%
wait_for_tables(Tabs, Timeout) when is_list(Tabs), Timeout == infinity ->
    do_wait_for_tables(Tabs, Timeout);
wait_for_tables(Tabs, Timeout) when is_list(Tabs),
                                    is_integer(Timeout), Timeout >= 0 ->
    do_wait_for_tables(Tabs, Timeout);
wait_for_tables(Tabs, Timeout) ->
    {error, {badarg, Tabs, Timeout}}.

do_wait_for_tables(Tabs, 0) ->
    reply_wait(Tabs);
do_wait_for_tables(Tabs, Timeout) ->
    Pid = spawn_link(?MODULE, wait_for_tables_init, [self(), Tabs]),
    receive
	{?SERVER_NAME, Pid, Res} ->
	    Res;
	{'EXIT', Pid, _} ->
	    reply_wait(Tabs)
    after Timeout ->
	    unlink(Pid),
	    exit(Pid, timeout),
	    reply_wait(Tabs)
    end.

reply_wait(Tabs) ->
    try mnesia_lib:active_tables() of
	Active when is_list(Active) ->
	    case Tabs -- Active of
		[] ->
		    ok;
		BadTabs ->
		    {timeout, BadTabs}
	    end
    catch exit:_ -> {error, {node_not_running, node()}}
    end.

wait_for_tables_init(From, Tabs) ->
    process_flag(trap_exit, true),
    Res = wait_for_init(From, Tabs, whereis(?SERVER_NAME)),
    From ! {?SERVER_NAME, self(), Res},
    unlink(From),
    exit(normal).

wait_for_init(From, Tabs, Init) ->
    try link(Init) of
	true when is_pid(Init) ->
	    cast({sync_tabs, Tabs, self()}),
	    rec_tabs(Tabs, Tabs, From, Init)
    catch error:_ -> %% Mnesia is not started
	    {error, {node_not_running, node()}}
    end.

sync_reply(Waiter, Tab) ->
    Waiter ! {?SERVER_NAME, {tab_synced, Tab}}.

rec_tabs([Tab | Tabs], AllTabs, From, Init) ->
    receive
	{?SERVER_NAME, {tab_synced, Tab}} ->
	    rec_tabs(Tabs, AllTabs, From, Init);

	{'EXIT', From, _} ->
	    %% This will trigger an exit signal
	    %% to mnesia_init
	    exit(wait_for_tables_timeout);

	{'EXIT', Init, _} ->
	    %% Oops, mnesia_init stopped,
	    exit(mnesia_stopped)
    end;
rec_tabs([], _, _, Init) ->
    unlink(Init),
    ok.

get_remote_cstructs() ->
    get_cstructs().  %% Sigh not forward compatible always check version

%% Old function kept for backwards compatibility; converts cstructs before sending.
get_cstructs() ->
    {cstructs, Cstructs, Running} = call(get_cstructs),
    Node = node(group_leader()),
    {cstructs, mnesia_schema:normalize_cs(Cstructs, Node), Running}.

update(Fun) ->
    call({update,Fun}).


mnesia_down(Node) ->
    case whereis(?SERVER_NAME) of
	undefined -> mnesia_monitor:mnesia_down(?SERVER_NAME, Node);
	Pid -> gen_server:cast(Pid, {mnesia_down, Node})
    end.

wait_for_schema_commit_lock() ->
    try
	Pid = whereis(?SERVER_NAME),
	link(Pid), %% Keep the link until release_schema_commit_lock
	gen_server:call(Pid, wait_for_schema_commit_lock, infinity)
    catch _:_ ->
	    mnesia:abort({node_not_running, node()})
    end.

block_controller() ->
    call(block_controller).

unblock_controller() ->
    cast(unblock_controller).

release_schema_commit_lock() ->
    cast({release_schema_commit_lock, self()}),
    unlink(whereis(?SERVER_NAME)).

%% Special for preparation of add table copy
get_network_copy(Tid, Tab, Cs) ->
%   We can't let the controller queue this one
%   because that may cause a deadlock between schema_operations
%   and initial tableloadings which both takes schema locks.
%   But we have to get copier_done msgs when the other side
%   goes down.
    call({add_other, self()}),
    Reason = {dumper,{add_table_copy, Tid}},
    Work = #net_load{table = Tab,reason = Reason,cstruct = Cs},
    %% I'll need this cause it's linked trough the subscriber
    %% might be solved by using monitor in subscr instead.
    process_flag(trap_exit, true),
    Load = load_table_fun(Work),
    Res = ?CATCH(Load()),
    process_flag(trap_exit, false),
    call({del_other, self()}),
    case Res of
 	#loader_done{is_loaded = true} ->
 	    Tab = Res#loader_done.table_name,
 	    case Res#loader_done.needs_announce of
 		true ->
 		    i_have_tab(Tab);
 		false ->
 		    ignore
 	    end,
 	    Res#loader_done.reply;
	#loader_done{} ->
 	    Res#loader_done.reply;
 	Else ->
 	    {not_loaded, Else}
    end.

%% This functions is invoked from the dumper
%%
%% There are two cases here:
%% startup ->
%%   no need for sync, since mnesia_controller not started yet
%% schema_trans ->
%%   already synced with mnesia_controller since the dumper
%%   is syncronously started from mnesia_controller

create_table(Tab) ->
    {loaded, ok} = mnesia_loader:disc_load_table(Tab, {dumper,create_table}).

get_disc_copy(Tab) ->
    disc_load_table(Tab, {dumper,change_table_copy_type}, undefined).

%% Returns ok instead of yes
force_load_table(Tab) when is_atom(Tab), Tab /= schema ->
    case ?catch_val({Tab, storage_type}) of
	ram_copies ->
	    do_force_load_table(Tab);
	disc_copies ->
	    do_force_load_table(Tab);
	disc_only_copies ->
	    do_force_load_table(Tab);
        {ext, _, _} ->
            do_force_load_table(Tab);
	unknown ->
	    set({Tab, load_by_force}, true),
	    cast({force_load_updated, Tab}),
	    wait_for_tables([Tab], infinity);
	{'EXIT', _} ->
	    {error, {no_exists, Tab}}
    end;
force_load_table(Tab) ->
    {error, {bad_type, Tab}}.

do_force_load_table(Tab) ->
    Loaded = ?catch_val({Tab, load_reason}),
    case Loaded of
	unknown ->
	    set({Tab, load_by_force}, true),
	    mnesia_late_loader:async_late_disc_load(node(), [Tab], forced_by_user),
	    wait_for_tables([Tab], infinity);
	{'EXIT', _} ->
	    set({Tab, load_by_force}, true),
	    mnesia_late_loader:async_late_disc_load(node(), [Tab], forced_by_user),
	    wait_for_tables([Tab], infinity);
	_ ->
	    ok
    end.
master_nodes_updated(schema, _Masters) ->
    ignore;
master_nodes_updated(Tab, Masters) ->
    cast({master_nodes_updated, Tab, Masters}).

schedule_late_disc_load(Tabs, Reason) ->
    MsgTag = late_disc_load,
    try_schedule_late_disc_load(Tabs, Reason, MsgTag).

try_schedule_late_disc_load(Tabs, _Reason, MsgTag)
  when Tabs == [], MsgTag /= schema_is_merged ->
    ignore;
try_schedule_late_disc_load(Tabs, Reason, MsgTag) ->
    GetIntents =
	fun() ->
		Item = mnesia_late_disc_load,
		Nodes = val({current, db_nodes}),
		mnesia:lock({global, Item, Nodes}, write),
		case multicall(Nodes -- [node()], disc_load_intents) of
		    {Replies, []} ->
			call({MsgTag, Tabs, Reason, Replies}),
			done;
		    {_, BadNodes} ->
			%% Some nodes did not respond, lets try again
			{retry, BadNodes}
		end
	end,
    case mnesia:transaction(GetIntents) of
	{atomic, done} ->
	    done;
	{atomic, {retry, BadNodes}} ->
	    verbose("Retry late_load_tables because bad nodes: ~p~n",
		    [BadNodes]),
	    try_schedule_late_disc_load(Tabs, Reason, MsgTag);
	{aborted, AbortReason} ->
	    fatal("Cannot late_load_tables~p: ~p~n",
		  [[Tabs, Reason, MsgTag], AbortReason])
    end.

connect_nodes(Ns) ->
    connect_nodes(Ns, fun default_merge/1).

connect_nodes(Ns, UserFun) ->
    case mnesia:system_info(is_running) of
	no ->
	    {error, {node_not_running, node()}};
	yes ->
	    Pid = spawn_link(?MODULE,connect_nodes2,[self(),Ns, UserFun]),
	    receive
		{?MODULE, Pid, Res, New} ->
		    case Res of
			ok ->
			    mnesia_lib:add_list(extra_db_nodes, New),
			    {ok, New};
			{aborted, {throw, Str}} when is_list(Str) ->
			    %%mnesia_recover:disconnect_nodes(New),
			    {error, {merge_schema_failed, lists:flatten(Str)}};
			Else ->
			    {error, Else}
		    end;
		{'EXIT', Pid, Reason} ->
		    {error, Reason}
	    end
    end.

connect_nodes2(Father, Ns, UserFun) ->
    Current = val({current, db_nodes}),
    abcast([node()|Ns], {merging_schema, node()}),
    {NewC, OldC} = mnesia_recover:connect_nodes(Ns),
    Connected = NewC ++OldC,
    New1 = mnesia_lib:intersect(Ns, Connected),
    New = New1 -- Current,
    process_flag(trap_exit, true),
    Res = try_merge_schema(New, [], UserFun),
    Msg = {schema_is_merged, [], late_merge, []},
    _ = multicall([node()|Ns], Msg),
    After = val({current, db_nodes}),
    Father ! {?MODULE, self(), Res, mnesia_lib:intersect(Ns,After)},
    unlink(Father),
    ok.

%% Merge the local schema with the schema on other nodes.
%% But first we must let all processes that want to force
%% load tables wait until the schema merge is done.

merge_schema() ->
    AllNodes = mnesia_lib:all_nodes(),
    case try_merge_schema(AllNodes, [node()], fun default_merge/1) of
	ok ->
	    schema_is_merged();
	{aborted, {throw, Str}} when is_list(Str) ->
	    fatal("Failed to merge schema: ~s~n", [Str]);
	Else ->
	    fatal("Failed to merge schema: ~p~n", [Else])
    end.

default_merge(F) ->
    F([]).

try_merge_schema(Nodes, Told0, UserFun) ->
    case mnesia_schema:merge_schema(UserFun) of
	{atomic, not_merged} ->
	    %% No more nodes that we need to merge the schema with
	    %% Ensure we have told everybody that we are running
	    case val({current,db_nodes}) -- mnesia_lib:uniq(Told0) of
		[] ->  ok;
		Tell ->
		    im_running(Tell, [node()]),
		    ok
	    end;
	{atomic, {merged, OldFriends, NewFriends}} ->
	    %% Check if new nodes has been added to the schema
	    Diff = mnesia_lib:all_nodes() -- [node() | Nodes],
	    mnesia_recover:connect_nodes(Diff),

	    %% Tell everybody to adopt orphan tables
	    im_running(OldFriends, NewFriends),
	    im_running(NewFriends, OldFriends),
	    Told = case lists:member(node(), NewFriends) of
		       true -> Told0 ++ OldFriends;
		       false -> Told0 ++ NewFriends
		   end,
	    try_merge_schema(Nodes, Told, UserFun);
	{atomic, {"Cannot get cstructs", Node, Reason}} ->
	    dbg_out("Cannot get cstructs, Node ~p ~p~n", [Node, Reason]),
	    timer:sleep(300), % Avoid a endless loop look alike
	    try_merge_schema(Nodes, Told0, UserFun);
	{aborted, {shutdown, _}} ->  %% One of the nodes is going down
	    timer:sleep(300), % Avoid a endless loop look alike
	    try_merge_schema(Nodes, Told0, UserFun);
	Other ->
	    Other
    end.

im_running(OldFriends, NewFriends) ->
    abcast(OldFriends, {im_running, node(), NewFriends}).

schema_is_merged() ->
    MsgTag = schema_is_merged,
    SafeLoads = initial_safe_loads(),

    %% At this point we do not know anything about
    %% which tables that the other nodes already
    %% has loaded and therefore we let the normal
    %% processing of the loader_queue take care
    %% of it, since we at that time point will
    %% know the whereabouts. We rely on the fact
    %% that all nodes tells each other directly
    %% when they have loaded a table and are
    %% willing to share it.

    try_schedule_late_disc_load(SafeLoads, initial, MsgTag).


cast(Msg) ->
    case whereis(?SERVER_NAME) of
	undefined -> ok;
	Pid ->  gen_server:cast(Pid, Msg)
    end.

abcast(Nodes, Msg) ->
    gen_server:abcast(Nodes, ?SERVER_NAME, Msg).

call(Msg) ->
    case whereis(?SERVER_NAME) of
	undefined ->
	    {error, {node_not_running, node()}};
	Pid ->
	    link(Pid),
	    Res = gen_server:call(Pid, Msg, infinity),
	    unlink(Pid),

	    %% We get an exit signal if server dies
            receive
                {'EXIT', Pid, _Reason} ->
                    {error, {node_not_running, node()}}
            after 0 ->
                    Res
            end
    end.

remote_call(Node, Func, Args) ->
    try gen_server:call({?MODULE, Node}, {Func, Args, self()}, infinity)
    catch exit:Error -> {error, Error}
    end.

multicall(Nodes, Msg) ->
    {Good, Bad} = gen_server:multi_call(Nodes, ?MODULE, Msg, infinity),
    PatchedGood = [Reply || {_Node, Reply} <- Good],
    {PatchedGood, Bad}.  %% Make the replies look like rpc:multicalls..
%%    rpc:multicall(Nodes, ?MODULE, call, [Msg]).

next_async_dump_log() ->
    Interval = mnesia_monitor:get_env(dump_log_time_threshold),
    Msg = {next_async_dump_log, time_threshold},
    Ref = erlang:send_after(Interval, self(), Msg),
    Ref.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([Parent]) ->
    process_flag(trap_exit, true),
    mnesia_lib:verbose("~p starting: ~p~n", [?SERVER_NAME, self()]),

    %% Handshake and initialize transaction recovery
    %% for new nodes detected in the schema
    All = mnesia_lib:all_nodes(),
    Diff = All -- [node() | val(original_nodes)],
    mnesia_lib:unset(original_nodes),
    mnesia_recover:connect_nodes(Diff),

    Ref = next_async_dump_log(),
    mnesia_dumper:start_regulator(),

    Empty = gb_trees:empty(),
    {ok, #state{supervisor = Parent, dump_log_timer_ref = Ref,
		loader_queue = Empty,
		late_loader_queue = Empty}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, Reply, State}     (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call({sync_dump_log, InitBy}, From, State) ->
    Worker = #dump_log{initiated_by = InitBy,
		       opt_reply_to = From
		      },
    State2 = add_worker(Worker, State),
    noreply(State2);

handle_call({snapshot_dcd, Tables}, From, State) ->
    Worker = #dump_log{initiated_by = user,
		       opt_reply_to = From,
		       operation = fun() ->
					   mnesia_dumper:snapshot_dcd(Tables)
				   end},
    State2 = add_worker(Worker, State),
    noreply(State2);

handle_call(wait_for_schema_commit_lock, From, State) ->
    Worker = #schema_commit_lock{owner = From},
    State2 = add_worker(Worker, State),
    noreply(State2);

handle_call(block_controller, From, State) ->
    Worker = #block_controller{owner = From},
    State2 = add_worker(Worker, State),
    noreply(State2);

handle_call({update,Fun}, From, State) ->
    Res = ?CATCH(Fun()),
    reply(From, Res),
    noreply(State);

handle_call(get_cstructs, From, State) ->
    Tabs = val({schema, tables}),
    Cstructs = [val({T, cstruct}) || T <- Tabs],
    Running = val({current, db_nodes}),
    reply(From, {cstructs, Cstructs, Running}),
    noreply(State);

handle_call({schema_is_merged, [], late_merge, []}, From,
	    State = #state{schema_is_merged = Merged}) ->
    case Merged of
	{false, Node} when Node == node(From) ->
	    Msgs = State#state.early_msgs,
	    State1 = State#state{early_msgs = [], schema_is_merged = true},
	    handle_early_msgs(lists:reverse(Msgs), State1);
	_ ->
	    %% Ooops this came to early, before we have merged :-)
	    %% or it came to late or from a node we don't care about
	    reply(From, ignore),
	    noreply(State)
    end;

handle_call({schema_is_merged, TabsR, Reason, RemoteLoaders}, From, State) ->
    State2 = late_disc_load(TabsR, Reason, RemoteLoaders, From, State),

    %% Handle early messages
    Msgs = State2#state.early_msgs,
    State3 = State2#state{early_msgs = [], schema_is_merged = true},
    handle_early_msgs(lists:reverse(Msgs), State3);

handle_call(disc_load_intents,From,State = #state{loader_queue=LQ,late_loader_queue=LLQ}) ->
    LQTabs  = gb_trees:keys(LQ),
    LLQTabs = gb_trees:keys(LLQ),
    ActiveTabs = lists:sort(mnesia_lib:local_active_tables()),
    reply(From, {ok, node(), ordsets:union([LQTabs,LLQTabs,ActiveTabs])}),
    noreply(State);

handle_call({update_where_to_write, [add, Tab, AddNode], _From}, _Dummy, State) ->
    Current = val({current, db_nodes}),
    Res =
	case lists:member(AddNode, Current) and
	    (State#state.schema_is_merged == true) of
	    true ->
		mnesia_lib:add_lsort({Tab, where_to_write}, AddNode),
		update_where_to_wlock(Tab);
	    false ->
		ignore
	end,
    {reply, Res, State};

handle_call({add_active_replica, [Tab, ToNode, RemoteS, AccessMode], From},
	    ReplyTo, State) ->
    KnownNode = lists:member(ToNode, val({current, db_nodes})),
    Merged = State#state.schema_is_merged,
    if
	KnownNode == false ->
	    reply(ReplyTo, ignore),
	    noreply(State);
	Merged == true ->
	    Res = case ?catch_val({Tab, cstruct}) of
		      {'EXIT', _} ->  %% Tab deleted
			  deleted;
		      _ ->
			  add_active_replica(Tab, ToNode, RemoteS, AccessMode)
		  end,
	    reply(ReplyTo, Res),
	    noreply(State);
	true -> %% Schema is not merged
	    Msg = {add_active_replica, [Tab, ToNode, RemoteS, AccessMode], From},
	    Msgs = State#state.early_msgs,
	    reply(ReplyTo, ignore),   %% Reply ignore and add data after schema merge
	    noreply(State#state{early_msgs = [{call, Msg, undefined} | Msgs]})
    end;

handle_call({unannounce_add_table_copy, [Tab, Node], From}, ReplyTo, State) ->
    KnownNode = lists:member(node(From), val({current, db_nodes})),
    Merged = State#state.schema_is_merged,
    if
	KnownNode == false ->
	    reply(ReplyTo, ignore),
	    noreply(State);
	Merged == true ->
	    Res = unannounce_add_table_copy(Tab, Node),
	    reply(ReplyTo, Res),
	    noreply(State);
	true -> %% Schema is not merged
	    Msg = {unannounce_add_table_copy, [Tab, Node], From},
	    Msgs = State#state.early_msgs,
	    reply(ReplyTo, ignore),   %% Reply ignore and add data after schema merge
	    %% Set ReplyTO to undefined so we don't reply twice
	    noreply(State#state{early_msgs = [{call, Msg, undefined} | Msgs]})
    end;

handle_call({net_load, Tab, Cs}, From, State) ->
    State2 =
	case State#state.schema_is_merged of
	    true ->
		Worker = #net_load{table = Tab,
				   opt_reply_to = From,
				   reason = {dumper,{add_table_copy, unknown}},
				   cstruct = Cs
				  },
		add_worker(Worker, State);
	    false ->
		reply(From, {not_loaded, schema_not_merged}),
		State
	end,
    noreply(State2);

handle_call(Msg, From, State) when State#state.schema_is_merged /= true ->
    %% Buffer early messages
    Msgs = State#state.early_msgs,
    noreply(State#state{early_msgs = [{call, Msg, From} | Msgs]});

handle_call({late_disc_load, Tabs, Reason, RemoteLoaders}, From, State) ->
    State2 = late_disc_load(Tabs, Reason, RemoteLoaders, From, State),
    noreply(State2);

handle_call({unblock_table, Tab}, _Dummy, State) ->
    Var = {Tab, where_to_commit},
    case val(Var) of
	{blocked, List} ->
	    set(Var, List); % where_to_commit
	_ ->
	    ignore
    end,
    {reply, ok, State};

handle_call({block_table, [Tab], From}, _Dummy, State) ->
    case lists:member(node(From), val({current, db_nodes})) of
	true ->
	    block_table(Tab);
	false ->
	    ignore
    end,
    {reply, ok, State};

handle_call({check_w2r, _Node, Tab}, _From, State) ->
    {reply, val({Tab, where_to_read}), State};

handle_call({add_other, Who}, _From, State = #state{others=Others0}) ->
    Others = [Who|Others0],
    {reply, ok, State#state{others=Others}};
handle_call({del_other, Who}, _From, State = #state{others=Others0}) ->
    Others = lists:delete(Who, Others0),
    {reply, ok, State#state{others=Others}};

handle_call(Msg, _From, State) ->
    error("~p got unexpected call: ~p~n", [?SERVER_NAME, Msg]),
    noreply(State).

late_disc_load(TabsR, Reason, RemoteLoaders, From,
	       State = #state{loader_queue = LQ, late_loader_queue = LLQ}) ->
    verbose("Intend to load tables: ~p~n", [TabsR]),
    ?eval_debug_fun({?MODULE, late_disc_load},
		    [{tabs, TabsR},
		     {reason, Reason},
		     {loaders, RemoteLoaders}]),

    reply(From, queued),
    %% RemoteLoaders is a list of {ok, Node, Tabs} tuples

    %% Remove deleted tabs and queued/loaded
    LocalTabs = gb_sets:from_ordset(lists:sort(mnesia_lib:val({schema,local_tables}))),
    Filter = fun(TabInfo0, Acc) ->
		     TabInfo = {Tab,_} =
			 case TabInfo0 of
			     {_,_} -> TabInfo0;
			     TabN -> {TabN,Reason}
			 end,
		     case gb_sets:is_member(Tab, LocalTabs) of
			 true ->
			     case ?catch_val({Tab, where_to_read}) == node() of
				 true -> Acc;
				 false ->
				     case gb_trees:is_defined(Tab,LQ) of
					 true ->  Acc;
					 false -> [TabInfo | Acc]
				     end
			     end;
			 false -> Acc
		     end
	     end,

    Tabs = lists:foldl(Filter, [], TabsR),

    Nodes = val({current, db_nodes}),
    LateQueue = late_loaders(Tabs, RemoteLoaders, Nodes, LLQ),
    State#state{late_loader_queue = LateQueue}.

late_loaders([{Tab, Reason} | Tabs], RemoteLoaders, Nodes, LLQ) ->
    case gb_trees:is_defined(Tab, LLQ) of
	false ->
	    LoadNodes = late_load_filter(RemoteLoaders, Tab, Nodes, []),
	    case LoadNodes of
		[] ->  cast({disc_load, Tab, Reason}); % Ugly cast
		_ ->   ignore
	    end,
	    LateLoad = #late_load{table=Tab,loaders=LoadNodes,reason=Reason},
	    late_loaders(Tabs, RemoteLoaders, Nodes, gb_trees:insert(Tab,LateLoad,LLQ));
	true ->
	    late_loaders(Tabs, RemoteLoaders, Nodes, LLQ)
    end;
late_loaders([], _RemoteLoaders, _Nodes, LLQ) ->
    LLQ.

late_load_filter([{error, _} | RemoteLoaders], Tab, Nodes, Acc) ->
    late_load_filter(RemoteLoaders, Tab, Nodes, Acc);
late_load_filter([{badrpc, _} | RemoteLoaders], Tab, Nodes, Acc) ->
    late_load_filter(RemoteLoaders, Tab, Nodes, Acc);
late_load_filter([RL | RemoteLoaders], Tab, Nodes, Acc) ->
    {ok, Node, Intents} = RL,
    Access = val({Tab, access_mode}),
    LocalC = val({Tab, local_content}),
    StillActive = lists:member(Node, Nodes),
    RemoteIntent = lists:member(Tab, Intents),
    if
	Access == read_write,
	LocalC == false,
	StillActive == true,
	RemoteIntent == true ->
	    Masters = mnesia_recover:get_master_nodes(Tab),
	    case lists:member(Node, Masters) of
		true ->
		    %% The other node is master node for
		    %% the table, accept his load intent
		    late_load_filter(RemoteLoaders, Tab, Nodes, [Node | Acc]);
		false when Masters == [] ->
		    %% The table has no master nodes
		    %% accept his load intent
		    late_load_filter(RemoteLoaders, Tab, Nodes, [Node | Acc]);
		false ->
		    %% Some one else is master node for
		    %% the table, ignore his load intent
		    late_load_filter(RemoteLoaders, Tab, Nodes, Acc)
	    end;
	true ->
	    late_load_filter(RemoteLoaders, Tab, Nodes, Acc)
    end;
late_load_filter([], _Tab, _Nodes, Acc) ->
    Acc.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_cast({release_schema_commit_lock, _Owner}, State) ->
    if
	State#state.is_stopping == true ->
	    {stop, shutdown, State};
	true ->
	    case State#state.dumper_queue of
		[#schema_commit_lock{}|Rest] ->
		    [_Worker | Rest] = State#state.dumper_queue,
		    State2 = State#state{dumper_pid = undefined,
					 dumper_queue = Rest},
		    State3 = opt_start_worker(State2),
		    noreply(State3);
		_ ->
		    noreply(State)
	    end
    end;

handle_cast(unblock_controller, State) ->
    if
	State#state.is_stopping == true ->
	    {stop, shutdown, State};
	is_record(hd(State#state.dumper_queue), block_controller) ->
	    [_Worker | Rest] = State#state.dumper_queue,
	    State2 = State#state{dumper_pid = undefined,
				 dumper_queue = Rest},
	    State3 = opt_start_worker(State2),
	    noreply(State3)
    end;

handle_cast({mnesia_down, Node}, State) ->
    maybe_log_mnesia_down(Node),
    mnesia_lib:del({current, db_nodes}, Node),
    mnesia_lib:unset({node_up, Node}),
    mnesia_checkpoint:tm_mnesia_down(Node),
    Alltabs = val({schema, tables}),
    reconfigure_tables(Node, Alltabs),
    %% Done from (external point of view)
    mnesia_monitor:mnesia_down(?SERVER_NAME, Node),

    %% Fix if we are late_merging against the node that went down
    case State#state.schema_is_merged of
	{false, Node} ->
	    spawn(?MODULE, call, [{schema_is_merged, [], late_merge, []}]);
	_ ->
	    ignore
    end,

    %% Fix internal stuff
    LateQ = remove_loaders(Alltabs, Node, State#state.late_loader_queue),

    case get_senders(State) ++ get_loaders(State) of
	[] -> ignore;
	Senders ->
	    lists:foreach(fun({Pid,_}) -> Pid ! {copier_done, Node} end,
			  Senders)
    end,
    lists:foreach(fun(Pid) -> Pid ! {copier_done,Node} end,
		  State#state.others),

    Remove = fun(ST) ->
		     node(ST#send_table.receiver_pid) /= Node
	     end,
    NewSenders = lists:filter(Remove, State#state.sender_queue),
    Early = remove_early_messages(State#state.early_msgs, Node),
    noreply(State#state{sender_queue = NewSenders,
			early_msgs = Early,
			late_loader_queue = LateQ
		       });

handle_cast({merging_schema, Node}, State) ->
    case State#state.schema_is_merged of
	false ->
	    %% This comes from dynamic connect_nodes which are made
	    %% after mnesia:start() and the schema_merge.
	    ImANewKidInTheBlock =
		(val({schema, storage_type}) == ram_copies)
		andalso (mnesia_lib:val({schema, local_tables}) == [schema]),
	    case ImANewKidInTheBlock of
		true ->  %% I'm newly started ram_node..
		    noreply(State#state{schema_is_merged = {false, Node}});
		false ->
		    noreply(State)
	    end;
	_ -> %% Already merging schema.
	    noreply(State)
    end;

handle_cast(Msg, State) when State#state.schema_is_merged /= true ->
    %% Buffer early messages
    Msgs = State#state.early_msgs,
    noreply(State#state{early_msgs = [{cast, Msg} | Msgs]});

%% This must be done after schema_is_merged otherwise adopt_orphan
%% might trigger a table load from wrong nodes as a result of that we don't
%% know which tables we can load safly first.
handle_cast({im_running, Node, NewFriends}, State) ->
    LocalTabs = mnesia_lib:local_active_tables() -- [schema],
    RemoveLocalOnly = fun(Tab) -> not val({Tab, local_content}) end,
    Tabs = lists:filter(RemoveLocalOnly, LocalTabs),
    Nodes = mnesia_lib:union([Node],val({current, db_nodes})),
    Ns = mnesia_lib:intersect(NewFriends, Nodes),
    abcast(Ns, {adopt_orphans, node(), Tabs}),
    noreply(State);

handle_cast({disc_load, Tab, Reason}, State) ->
    Worker = #disc_load{table = Tab, reason = Reason},
    State2 = add_worker(Worker, State),
    noreply(State2);

handle_cast(Worker = #send_table{}, State) ->
    State2 = add_worker(Worker, State),
    noreply(State2);

handle_cast({sync_tabs, Tabs, From}, State) ->
    %% user initiated wait_for_tables
    handle_sync_tabs(Tabs, From),
    noreply(State);

handle_cast({i_have_tab, Tab, Node}, State) ->
    case lists:member(Node, val({current, db_nodes})) of
	true ->
	    State2 = node_has_tabs([Tab], Node, State),
	    noreply(State2);
	false ->
	    noreply(State)
    end;

handle_cast({force_load_updated, Tab}, State) ->
    case val({Tab, active_replicas}) of
	[] ->
	    %% No valid replicas
	    noreply(State);
	[SomeNode | _] ->
	    State2 = node_has_tabs([Tab], SomeNode, State),
	    noreply(State2)
    end;

handle_cast({master_nodes_updated, Tab, Masters}, State) ->
    Active = val({Tab, active_replicas}),
    Valid =
	case val({Tab, load_by_force}) of
	    true ->
		Active;
	    false ->
		if
		    Masters == [] ->
			Active;
		    true ->
			mnesia_lib:intersect(Masters, Active)
		end
	end,
    case Valid of
	[] ->
	    %% No valid replicas
	    noreply(State);
	[SomeNode | _] ->
	    State2 = node_has_tabs([Tab], SomeNode, State),
	    noreply(State2)
    end;

handle_cast({adopt_orphans, Node, Tabs}, State) ->
    State2 = node_has_tabs(Tabs, Node, State),

    case ?catch_val({node_up,Node}) of
	true -> ignore;
	_ ->
	    %% Register the other node as up and running
	    set({node_up, Node}, true),
	    mnesia_recover:log_mnesia_up(Node),
	    verbose("Logging mnesia_up ~w~n",[Node]),
	    mnesia_lib:report_system_event({mnesia_up, Node}),
	    %% Load orphan tables
	    LocalTabs = val({schema, local_tables}) -- [schema],
	    Nodes = val({current, db_nodes}),
	    {LocalOrphans, RemoteMasters} =
		orphan_tables(LocalTabs, Node, Nodes, [], []),
	    Reason = {adopt_orphan, node()},
	    mnesia_late_loader:async_late_disc_load(node(), LocalOrphans, Reason),

	    Fun =
		fun(N) ->
			RemoteOrphans =
			    [Tab || {Tab, Ns} <- RemoteMasters,
				    lists:member(N, Ns)],
			mnesia_late_loader:maybe_async_late_disc_load(N, RemoteOrphans, Reason)
		end,
	    lists:foreach(Fun, Nodes)
    end,
    noreply(State2);

handle_cast(Msg, State) ->
    error("~p got unexpected cast: ~p~n", [?SERVER_NAME, Msg]),
    noreply(State).

handle_sync_tabs([Tab | Tabs], From) ->
    case val({Tab, where_to_read}) of
	nowhere ->
	    case get({sync_tab, Tab}) of
		undefined ->
		    put({sync_tab, Tab}, [From]);
		Pids ->
		    put({sync_tab, Tab}, [From | Pids])
	    end;
	_ ->
	    sync_reply(From, Tab)
    end,
    handle_sync_tabs(Tabs, From);
handle_sync_tabs([], _From) ->
    ok.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_info({next_async_dump_log, InitBy}, State) ->
    async_dump_log(InitBy),
    Ref = next_async_dump_log(),
    noreply(State#state{dump_log_timer_ref=Ref});

handle_info({async_dump_log, InitBy}, State) ->
    Worker = #dump_log{initiated_by = InitBy},
    State2 = add_worker(Worker, State),
    noreply(State2);

handle_info(#dumper_done{worker_pid=Pid, worker_res=Res}, State) ->
    if
	State#state.is_stopping == true ->
	    {stop, shutdown, State};
	Res == dumped, Pid == State#state.dumper_pid ->
	    [Worker | Rest] = State#state.dumper_queue,
	    reply(Worker#dump_log.opt_reply_to, Res),
	    State2 = State#state{dumper_pid = undefined,
				 dumper_queue = Rest},
	    State3 = opt_start_worker(State2),
	    noreply(State3);
	true ->
	    fatal("Dumper failed: ~p~n state: ~p~n", [Res, State]),
	    {stop, fatal, State}
    end;

handle_info(Done = #loader_done{worker_pid=WPid, table_name=Tab}, State0) ->
    LateQueue0 = State0#state.late_loader_queue,
    State1 = State0#state{loader_pid = lists:keydelete(WPid,1,get_loaders(State0))},

    State2 =
	case Done#loader_done.is_loaded of
	    true ->
		%% Optional table announcement
		if
		    Done#loader_done.needs_announce == true,
		    Done#loader_done.needs_reply == true ->
			i_have_tab(Tab),
			%% Should be {dumper,{add_table_copy, _}} only
			reply(Done#loader_done.reply_to,
			      Done#loader_done.reply);
		    Done#loader_done.needs_reply == true ->
			%% Should be {dumper,{add_table_copy,_}} only
			reply(Done#loader_done.reply_to,
			      Done#loader_done.reply);
		    Done#loader_done.needs_announce == true, Tab == schema ->
			i_have_tab(Tab);
		    Done#loader_done.needs_announce == true ->
			i_have_tab(Tab),
			%% Local node needs to perform user_sync_tab/1
			Ns = val({current, db_nodes}),
			abcast(Ns, {i_have_tab, Tab, node()});
		    Tab == schema ->
			ignore;
		    true ->
			%% Local node needs to perform user_sync_tab/1
			Ns = val({current, db_nodes}),
			AlreadyKnows = val({Tab, active_replicas}),
			abcast(Ns -- AlreadyKnows, {i_have_tab, Tab, node()})
		end,
		%% Optional user sync
		case Done#loader_done.needs_sync of
		    true -> user_sync_tab(Tab);
		    false -> ignore
		end,
		State1#state{late_loader_queue=gb_trees:delete_any(Tab, LateQueue0)};
	    false ->
		%% Either the node went down or table was not
		%% loaded remotly yet
		case Done#loader_done.needs_reply of
		    true ->
			reply(Done#loader_done.reply_to,
			      Done#loader_done.reply);
		    false ->
			ignore
		end,
		case ?catch_val({Tab, active_replicas}) of
		    [_|_] -> % still available elsewhere
			{value,{_,Worker}} = lists:keysearch(WPid,1,get_loaders(State0)),
			add_loader(Tab,Worker,State1);
		    _ ->
			DelState = State1#state{late_loader_queue=gb_trees:delete_any(Tab, LateQueue0)},
			case ?catch_val({Tab, storage_type}) of
			    ram_copies ->
				cast({disc_load, Tab, ram_only}),
				DelState;
			    _ ->
				DelState
			end
		end
	end,
    State3 = opt_start_worker(State2),
    noreply(State3);

handle_info(#sender_done{worker_pid=Pid, worker_res=Res}, State)  ->
    Senders = get_senders(State),
    {value, {Pid,_Worker}} = lists:keysearch(Pid, 1, Senders),
    if
	Res == ok ->
	    State2 = State#state{sender_pid = lists:keydelete(Pid, 1, Senders)},
	    State3 = opt_start_worker(State2),
	    noreply(State3);
	true ->
	    %% No need to send any message to the table receiver
	    %% since it will soon get a mnesia_down anyway
	    fatal("Sender failed: ~p~n state: ~p~n", [Res, State]),
	    {stop, fatal, State}
    end;

handle_info({'EXIT', Pid, R}, State) when Pid == State#state.supervisor ->
    ?SAFE(set(mnesia_status, stopping)),
    case State#state.dumper_pid of
	undefined ->
	    dbg_out("~p was ~p~n", [?SERVER_NAME, R]),
	    {stop, shutdown, State};
	_ ->
	    noreply(State#state{is_stopping = true})
    end;

handle_info({'EXIT', Pid, R}, State) when Pid == State#state.dumper_pid ->
    case State#state.dumper_queue of
	[#schema_commit_lock{}|Workers] -> %% Schema trans crashed or was killed
	    dbg_out("WARNING: Dumper ~p exited ~p~n", [Pid, R]),
	    State2 = State#state{dumper_queue = Workers, dumper_pid = undefined},
	    State3 = opt_start_worker(State2),
	    noreply(State3);
	_Other ->
	    fatal("Dumper or schema commit crashed: ~p~n state: ~p~n", [R, State]),
	    {stop, fatal, State}
    end;

handle_info(Msg = {'EXIT', Pid, R}, State) when R /= wait_for_tables_timeout ->
    case lists:keymember(Pid, 1, get_senders(State)) of
	true ->
	    %% No need to send any message to the table receiver
	    %% since it will soon get a mnesia_down anyway
	    fatal("Sender crashed: ~p~n state: ~p~n", [{Pid,R}, State]),
	    {stop, fatal, State};
	false ->
	    case lists:keymember(Pid, 1, get_loaders(State)) of
		true ->
		    fatal("Loader crashed: ~p~n state: ~p~n", [R, State]),
		    {stop, fatal, State};
		false ->
		    error("~p got unexpected info: ~p~n", [?SERVER_NAME, Msg]),
		    noreply(State)
	    end
    end;

handle_info({From, get_state}, State) ->
    From ! {?SERVER_NAME, State},
    noreply(State);

%% No real need for buffering
handle_info(Msg, State) when State#state.schema_is_merged /= true ->
    %% Buffer early messages
    Msgs = State#state.early_msgs,
    noreply(State#state{early_msgs = [{info, Msg} | Msgs]});

handle_info({'EXIT', Pid, wait_for_tables_timeout}, State) ->
    sync_tab_timeout(Pid, get()),
    noreply(State);

handle_info(Msg, State) ->
    error("~p got unexpected info: ~p~n", [?SERVER_NAME, Msg]),
    noreply(State).

sync_tab_timeout(Pid, [{{sync_tab, Tab}, Pids} | Tail]) ->
    case lists:delete(Pid, Pids) of
	[] ->
	    erase({sync_tab, Tab});
	Pids2 ->
	    put({sync_tab, Tab}, Pids2)
    end,
    sync_tab_timeout(Pid, Tail);
sync_tab_timeout(Pid, [_ | Tail]) ->
    sync_tab_timeout(Pid, Tail);
sync_tab_timeout(_Pid, []) ->
    ok.

%% Pick the load record that has the highest load order
%% Returns {BestLoad, RemainingQueue} or {none, []} if queue is empty
pick_next(Queue) ->
    List = gb_trees:values(Queue),
    case pick_next(List, none, none) of
	none -> {none, gb_trees:empty()};
	{Tab, Worker} -> {Worker, gb_trees:delete(Tab,Queue)}
    end.

pick_next([Head = #net_load{table=Tab}| Tail], Load, Order) ->
    select_best(Head, Tail, ?catch_val({Tab, load_order}), Load, Order);
pick_next([Head = #disc_load{table=Tab}| Tail], Load, Order) ->
    select_best(Head, Tail, ?catch_val({Tab, load_order}), Load, Order);
pick_next([], none, _Order) ->
    none;
pick_next([], Load, _Order) ->
    {element(2,Load), Load}.

select_best(_Head, Tail, {'EXIT', _WHAT}, Load, Order) ->
    %% Table have been deleted drop it.
    pick_next(Tail, Load, Order);
select_best(Load, Tail, Order, none, none) ->
    pick_next(Tail, Load, Order);
select_best(Load, Tail, Order, _OldLoad, OldOrder) when Order > OldOrder ->
    pick_next(Tail, Load, Order);
select_best(_Load, Tail, _Order, OldLoad, OldOrder) ->
    pick_next(Tail, OldLoad, OldOrder).

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    mnesia_monitor:terminate_proc(?SERVER_NAME, Reason, State).

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Upgrade process when its code is to be changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State0, _Extra) ->
    %% Loader Queue
    State1 = case State0#state.loader_pid of
		 Pids when is_list(Pids) -> State0;
		 undefined -> State0#state{loader_pid = [],loader_queue=gb_trees:empty()};
		 Pid when is_pid(Pid) ->
		     [Loader|Rest] = State0#state.loader_queue,
		     LQ0 = [{element(2,Rec),Rec} || Rec <- Rest],
		     LQ1 = lists:sort(LQ0),
		     LQ  = gb_trees:from_orddict(LQ1),
		     State0#state{loader_pid=[{Pid,Loader}], loader_queue=LQ}
	     end,
    %% LateLoaderQueue
    State = if is_list(State1#state.late_loader_queue) ->
		    LLQ0 = State1#state.late_loader_queue,
		    LLQ1 = lists:sort([{element(2,Rec),Rec} || Rec <- LLQ0]),
		    LLQ  = gb_trees:from_orddict(LLQ1),
		    State1#state{late_loader_queue=LLQ};
	       true ->
		    State1
	    end,
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

maybe_log_mnesia_down(N) ->
    %% We use mnesia_down when deciding which tables to load locally,
    %% so if we are not running (i.e haven't decided which tables
    %% to load locally), don't log mnesia_down yet.
    case mnesia_lib:is_running() of
	yes ->
	    verbose("Logging mnesia_down ~w~n", [N]),
	    mnesia_recover:log_mnesia_down(N),
	    ok;
	_ ->
	    Filter = fun(Tab) ->
			     inactive_copy_holders(Tab, N)
		     end,
	    HalfLoadedTabs = lists:any(Filter, val({schema, local_tables}) -- [schema]),
	    if
		HalfLoadedTabs == true ->
		    verbose("Logging mnesia_down ~w~n", [N]),
		    mnesia_recover:log_mnesia_down(N),
		    ok;
		true ->
		    %% Unfortunately we have not loaded some common
		    %% tables yet, so we cannot rely on the nodedown
		    log_later   %% BUGBUG handle this case!!!
	    end
    end.

inactive_copy_holders(Tab, Node) ->
    Cs = val({Tab, cstruct}),
    case mnesia_lib:cs_to_storage_type(Node, Cs) of
	unknown ->
	    false;
	_Storage ->
	    mnesia_lib:not_active_here(Tab)
    end.

orphan_tables([Tab | Tabs], Node, Ns, Local, Remote) ->
    Cs = val({Tab, cstruct}),
    CopyHolders = mnesia_lib:copy_holders(Cs),
    RamCopyHolders = Cs#cstruct.ram_copies,
    DiscCopyHolders = CopyHolders -- RamCopyHolders,
    DiscNodes = val({schema, disc_copies}),
    LocalContent = Cs#cstruct.local_content,
    RamCopyHoldersOnDiscNodes = mnesia_lib:intersect(RamCopyHolders, DiscNodes),
    Active = val({Tab, active_replicas}),
    BeingCreated = (?catch_val({Tab, create_table}) == true),
    Read = val({Tab, where_to_read}),
    case lists:member(Node, DiscCopyHolders) of
	_ when BeingCreated == true ->
	    orphan_tables(Tabs, Node, Ns, Local, Remote);
	_ when Read == node() -> %% Allready loaded
	    orphan_tables(Tabs, Node, Ns, Local, Remote);
	true when Active == [] ->
	    case DiscCopyHolders -- Ns of
		[] ->
		    %% We're last up and the other nodes have not
		    %% loaded the table. Lets load it if we are
		    %% the smallest node.
		    case lists:min(DiscCopyHolders) of
			Min when Min == node() ->
			    case mnesia_recover:get_master_nodes(Tab) of
				[] ->
				    L = [Tab | Local],
				    orphan_tables(Tabs, Node, Ns, L, Remote);
				Masters ->
				    R = [{Tab, Masters} | Remote],
				    orphan_tables(Tabs, Node, Ns, Local, R)
			    end;
			_ ->
			    orphan_tables(Tabs, Node, Ns, Local, Remote)
		    end;
		_ ->
		    orphan_tables(Tabs, Node, Ns, Local, Remote)
	    end;
	false when Active == [], DiscCopyHolders == [], RamCopyHoldersOnDiscNodes == [] ->
	    %% Special case when all replicas resides on disc less nodes
	    orphan_tables(Tabs, Node, Ns, [Tab | Local], Remote);
	_ when LocalContent == true ->
	    orphan_tables(Tabs, Node, Ns, [Tab | Local], Remote);
	_ ->
	    orphan_tables(Tabs, Node, Ns, Local, Remote)
    end;
orphan_tables([], _, _, LocalOrphans, RemoteMasters) ->
    {LocalOrphans, RemoteMasters}.

node_has_tabs([Tab | Tabs], Node, State) when Node /= node() ->
    State2 =
	try update_whereabouts(Tab, Node, State) of
	    State1 = #state{} -> State1
	catch exit:R -> %% Tab was just deleted?
		case ?catch_val({Tab, cstruct}) of
		    {'EXIT', _} -> State; % yes
		    _ ->  erlang:error(R)
		end
	end,
    node_has_tabs(Tabs, Node, State2);
node_has_tabs([Tab | Tabs], Node, State) ->
    user_sync_tab(Tab),
    node_has_tabs(Tabs, Node, State);
node_has_tabs([], _Node, State) ->
    State.

update_whereabouts(Tab, Node, State) ->
    Storage = val({Tab, storage_type}),
    Read = val({Tab, where_to_read}),
    LocalC = val({Tab, local_content}),
    BeingCreated = (?catch_val({Tab, create_table}) == true),
    Masters = mnesia_recover:get_master_nodes(Tab),
    ByForce = val({Tab, load_by_force}),
    GoGetIt =
	if
	    ByForce == true ->
		true;
	    Masters == [] ->
		true;
	    true ->
		lists:member(Node, Masters)
	end,

    dbg_out("Table ~w is loaded on ~w. s=~w, r=~w, lc=~w, f=~w, m=~w~n",
	    [Tab, Node, Storage, Read, LocalC, ByForce, GoGetIt]),
    if
	LocalC == true ->
	    %% Local contents, don't care about other node
	    State;
	BeingCreated == true ->
	    %% The table is currently being created
	    %% It will be handled elsewhere
	    State;
	Storage == unknown, Read == nowhere ->
	    %% No own copy, time to read remotely
	    %% if the other node is a good node
	    add_active_replica(Tab, Node),
	    case GoGetIt of
		true ->
		    set({Tab, where_to_read}, Node),
		    user_sync_tab(Tab),
		    State;
		false ->
		    State
	    end;
	Storage == unknown ->
	    %% No own copy, continue to read remotely
	    add_active_replica(Tab, Node),
	    NodeST = mnesia_lib:semantics(mnesia_lib:storage_type_at_node(Node, Tab), storage),
	    ReadST = mnesia_lib:semantics(mnesia_lib:storage_type_at_node(Read, Tab), storage),
	    if   %% Avoid reading from disc_only_copies
		NodeST == disc_only_copies ->
		    ignore;
		ReadST == disc_only_copies ->
		    mnesia_lib:set_remote_where_to_read(Tab);
		true ->
		    ignore
	    end,
	    user_sync_tab(Tab),
	    State;
	Read == nowhere ->
	    %% Own copy, go and get a copy of the table
	    %% if the other node is master or if there
	    %% are no master at all
	    add_active_replica(Tab, Node),
	    case GoGetIt of
		true ->
		    Worker = #net_load{table = Tab,
				       reason = {active_remote, Node}},
		    add_worker(Worker, State);
		false ->
		    State
	    end;
	true ->
	    %% We already have an own copy
	    add_active_replica(Tab, Node),
	    user_sync_tab(Tab),
	    State
    end.

initial_safe_loads() ->
    case val({schema, storage_type}) of
	ram_copies ->
	    Downs = [],
	    Tabs = val({schema, local_tables}) -- [schema],
	    LastC = fun(T) -> last_consistent_replica(T, Downs) end,
	    lists:zf(LastC, Tabs);

	disc_copies ->
	    Downs = mnesia_recover:get_mnesia_downs(),
	    dbg_out("mnesia_downs = ~p~n", [Downs]),

	    Tabs = val({schema, local_tables}) -- [schema],
	    LastC = fun(T) -> last_consistent_replica(T, Downs) end,
	    lists:zf(LastC, Tabs)
    end.

last_consistent_replica(Tab, Downs) ->
    Cs = val({Tab, cstruct}),
    Storage = mnesia_lib:cs_to_storage_type(node(), Cs),
    Ram = Cs#cstruct.ram_copies,
    Disc = Cs#cstruct.disc_copies,
    DiscOnly = Cs#cstruct.disc_only_copies,
    Ext = Cs#cstruct.external_copies,
    BetterCopies0 = mnesia_lib:remote_copy_holders(Cs) -- Downs,
    BetterCopies = BetterCopies0 -- Ram,
    AccessMode = Cs#cstruct.access_mode,
    Copies = mnesia_lib:copy_holders(Cs),
    Masters = mnesia_recover:get_master_nodes(Tab),
    LocalMaster0 = lists:member(node(), Masters),
    LocalContent = Cs#cstruct.local_content,
    RemoteMaster =
	if
	    Masters == [] -> false;
	    true -> not LocalMaster0
	end,
    LocalMaster =
	if
	    Masters == [] -> false;
	    true -> LocalMaster0
	end,
    if
	Copies == [node()]  ->
	    %% Only one copy holder and it is local.
	    %% It may also be a local contents table
	    {true, {Tab, local_only}};
	LocalContent == true ->
	    {true, {Tab, local_content}};
	LocalMaster == true ->
	    %% We have a local master
	    {true, {Tab, local_master}};
	RemoteMaster == true ->
	    %% Wait for remote master copy
	    false;
	Storage == ram_copies ->
	    if
		Disc == [], DiscOnly == [], Ext == [] ->
		    %% Nobody has copy on disc
		    {true, {Tab, ram_only}};
		true ->
		    %% Some other node has copy on disc
		    false
	    end;
	AccessMode == read_only ->
	    %% No one has been able to update the table,
	    %% i.e. all disc resident copies are equal
	    {true, {Tab, read_only}};
	BetterCopies /= [], Masters /= [node()] ->
	    %% There are better copies on other nodes
	    %% and we do not have the only master copy
	    false;
	true ->
	    {true, {Tab, initial}}
    end.

reconfigure_tables(N, [Tab |Tail]) ->
    del_active_replica(Tab, N),
    case val({Tab, where_to_read}) of
	N ->  mnesia_lib:set_remote_where_to_read(Tab);
	_ ->  ignore
    end,
    reconfigure_tables(N, Tail);
reconfigure_tables(_, []) ->
    ok.

remove_loaders([Tab| Tabs], N, Loaders) ->
    LateQ = drop_loaders(Tab, N, Loaders),
    remove_loaders(Tabs, N, LateQ);
remove_loaders([],_, LateQ) -> LateQ.

remove_early_messages([], _Node) ->
    [];
remove_early_messages([{call, {add_active_replica, [_, Node, _, _], _}, _}|R], Node) ->
    remove_early_messages(R, Node); %% Does a reply before queuing
remove_early_messages([{call, {block_table, _, From}, ReplyTo}|R], Node)
  when node(From) == Node ->
    reply(ReplyTo, ok),  %% Remove gen:server waits..
    remove_early_messages(R, Node);
remove_early_messages([{cast, {i_have_tab, _Tab, Node}}|R], Node) ->
    remove_early_messages(R, Node);
remove_early_messages([{cast, {adopt_orphans, Node, _Tabs}}|R], Node) ->
    remove_early_messages(R, Node);
remove_early_messages([M|R],Node) ->
    [M|remove_early_messages(R,Node)].

%% Drop loader from late load queue and possibly trigger a disc_load
drop_loaders(Tab, Node, LLQ) ->
    case gb_trees:lookup(Tab,LLQ) of
	none ->
	    LLQ;
	{value, H} ->
	    %% Check if it is time to issue a disc_load request
	    case H#late_load.loaders of
		[Node] ->
		    Reason = {H#late_load.reason, last_loader_down, Node},
		    cast({disc_load, Tab, Reason});  % Ugly cast
		_ ->
		    ignore
	    end,
	    %% Drop the node from the list of loaders
	    H2 = H#late_load{loaders = H#late_load.loaders -- [Node]},
	    gb_trees:update(Tab, H2, LLQ)
    end.

add_active_replica(Tab, Node) ->
    add_active_replica(Tab, Node, val({Tab, cstruct})).

add_active_replica(Tab, Node, Cs = #cstruct{}) ->
    Storage = mnesia_lib:schema_cs_to_storage_type(Node, Cs),
    AccessMode = Cs#cstruct.access_mode,
    add_active_replica(Tab, Node, Storage, AccessMode).

%% Block table primitives

block_table(Tab) ->
    Var = {Tab, where_to_commit},
    Old = val(Var),
    New = {blocked, Old},
    set(Var, New). % where_to_commit

unblock_table(Tab) ->
    call({unblock_table, Tab}).

is_tab_blocked(W2C) when is_list(W2C) ->
    {false, W2C};
is_tab_blocked({blocked, W2C}) when is_list(W2C) ->
    {true, W2C}.

mark_blocked_tab(true, Value) ->
    {blocked, Value};
mark_blocked_tab(false, Value) ->
    Value.

%%

add_active_replica(Tab, Node, Storage, AccessMode) ->
    Var = {Tab, where_to_commit},
    {Blocked, Old} = is_tab_blocked(val(Var)),
    Del = lists:keydelete(Node, 1, Old),
    case AccessMode of
	read_write ->
	    New = lists:sort([{Node, Storage} | Del]),
	    set(Var, mark_blocked_tab(Blocked, New)), % where_to_commit
	    mnesia_lib:add_lsort({Tab, where_to_write}, Node);
	read_only ->
	    set(Var, mark_blocked_tab(Blocked, Del)),
	    mnesia_lib:del({Tab, where_to_write}, Node)
    end,

    update_where_to_wlock(Tab),
    add({Tab, active_replicas}, Node).

del_active_replica(Tab, Node) ->
    Var = {Tab, where_to_commit},
    {Blocked, Old} = is_tab_blocked(val(Var)),
    Del = lists:keydelete(Node, 1, Old),
    New = lists:sort(Del),
    set(Var, mark_blocked_tab(Blocked, New)),      % where_to_commit
    mnesia_lib:del({Tab, active_replicas}, Node),
    mnesia_lib:del({Tab, where_to_write}, Node),
    update_where_to_wlock(Tab).

change_table_access_mode(Cs) ->
    W = fun() ->
		Tab = Cs#cstruct.name,
		lists:foreach(fun(N) -> add_active_replica(Tab, N, Cs) end,
			      val({Tab, active_replicas}))
	end,
    update(W).

change_table_majority(Cs) ->
    W = fun() ->
		Tab = Cs#cstruct.name,
		set({Tab, majority}, Cs#cstruct.majority),
		update_where_to_wlock(Tab)
	end,
    update(W).

update_where_to_wlock(Tab) ->
    WNodes = val({Tab, where_to_write}),
    Majority = ?catch_val({Tab, majority}) == true,
    set({Tab, where_to_wlock}, {WNodes, Majority}).

%% node To now has tab loaded, but this must be undone
%% This code is rpc:call'ed from the tab_copier process
%% when it has *not* released it's table lock
unannounce_add_table_copy(Tab, To) ->
    ?SAFE(del_active_replica(Tab, To)),
    try To = val({Tab , where_to_read}),
	 mnesia_lib:set_remote_where_to_read(Tab)
    catch _:_ -> ignore
    end.

user_sync_tab(Tab) ->
    case val(debug) of
	trace ->
	    mnesia_subscr:subscribe(whereis(mnesia_event), {table, Tab});
	_ ->
	    ignore
    end,

    case erase({sync_tab, Tab}) of
	undefined ->
	    ok;
	Pids ->
	    lists:foreach(fun(Pid) -> sync_reply(Pid, Tab) end, Pids)
    end.

i_have_tab(Tab) ->
    case val({Tab, local_content}) of
	true ->
	    mnesia_lib:set_local_content_whereabouts(Tab);
	false ->
	    set({Tab, where_to_read}, node())
    end,
    add_active_replica(Tab, node()).

sync_and_block_table_whereabouts(Tab, ToNode, RemoteS, AccessMode) when Tab /= schema ->
    Current = val({current, db_nodes}),
    Ns =
	case lists:member(ToNode, Current) of
	    true -> Current -- [ToNode];
	    false -> Current
	end,
    _ = remote_call(ToNode, block_table, [Tab]),
    [remote_call(Node, add_active_replica, [Tab, ToNode, RemoteS, AccessMode]) ||
	Node <- [ToNode | Ns]],
    ok.

sync_del_table_copy_whereabouts(Tab, ToNode) when Tab /= schema ->
    Current = val({current, db_nodes}),
    Ns =
	case lists:member(ToNode, Current) of
	    true -> Current;
	    false -> [ToNode | Current]
	end,
    Args = [Tab, ToNode],
    [remote_call(Node, unannounce_add_table_copy, Args) || Node <- Ns],
    ok.

get_info(Timeout) ->
    case whereis(?SERVER_NAME) of
	undefined ->
	    {timeout, Timeout};
	Pid ->
	    Pid ! {self(), get_state},
	    receive
		{?SERVER_NAME, State = #state{loader_queue=LQ,late_loader_queue=LLQ}} ->
		    {info,State#state{loader_queue=gb_trees:to_list(LQ),
				      late_loader_queue=gb_trees:to_list(LLQ)}}
	    after Timeout ->
		    {timeout, Timeout}
	    end
    end.

get_workers(Timeout) ->
    case whereis(?SERVER_NAME) of
	undefined ->
	    {timeout, Timeout};
	Pid ->
	    Pid ! {self(), get_state},
	    receive
		{?SERVER_NAME, State = #state{}} ->
		    {workers, get_loaders(State), get_senders(State), State#state.dumper_pid}
	    after Timeout ->
		    {timeout, Timeout}
	    end
    end.

info() ->
    Tabs = mnesia_lib:local_active_tables(),
    io:format( "---> Active tables <--- ~n", []),
    info(Tabs).

info([Tab | Tail]) ->
    case val({Tab, storage_type}) of
	disc_only_copies ->
	    info_format(Tab,
			dets:info(Tab, size),
			dets:info(Tab, file_size),
			"bytes on disc");
        {ext, Alias, Mod} ->
            info_format(Tab,
                        Mod:info(Alias, Tab, size),
                        Mod:info(Alias, Tab, memory),
                        "words of mem");
	_ ->
	    info_format(Tab,
			?ets_info(Tab, size),
			?ets_info(Tab, memory),
			"words of mem")
    end,
    info(Tail);
info([]) -> ok.


info_format(Tab, Size, Mem, Media) ->
    StrT = mnesia_lib:pad_name(atom_to_list(Tab), 15, []),
    StrS = mnesia_lib:pad_name(integer_to_list(Size), 8, []),
    StrM = mnesia_lib:pad_name(integer_to_list(Mem), 8, []),
    io:format("~s: with ~s records occupying ~s ~s~n",
	      [StrT, StrS, StrM, Media]).

%% Handle early arrived messages
handle_early_msgs([Msg | Msgs], State) ->
    %% The messages are in reverse order
    case handle_early_msg(Msg, State) of
%%         {stop, Reason, Reply, State2} ->  % Will not happen according to dialyzer
%% 	    {stop, Reason, Reply, State2};
        {stop, Reason, State2} ->
	    {stop, Reason, State2};
	{noreply, State2} ->
	    handle_early_msgs(Msgs, State2);
 	{reply, Reply, State2} ->
	    {call, _Call, From} = Msg,
	    reply(From, Reply),
 	    handle_early_msgs(Msgs, State2)
    end;
handle_early_msgs([], State) ->
    noreply(State).

handle_early_msg({call, Msg, From}, State) ->
    handle_call(Msg, From, State);
handle_early_msg({cast, Msg}, State) ->
    handle_cast(Msg, State);
handle_early_msg({info, Msg}, State) ->
    handle_info(Msg, State).

noreply(State) ->
    {noreply, State}.

reply(undefined, Reply) ->
    Reply;
reply(ReplyTo, Reply) ->
    gen_server:reply(ReplyTo, Reply),
    Reply.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Worker management

%% Returns new State
add_worker(Worker = #dump_log{}, State) ->
    InitBy = Worker#dump_log.initiated_by,
    Queue = State#state.dumper_queue,
    Status =
        case lists:keymember(InitBy, #dump_log.initiated_by, Queue) of
            true when Worker#dump_log.opt_reply_to == undefined ->
                %% The same threshold has been exceeded again,
                %% before we have had the possibility to
                %% process the older one.
                DetectedBy = {dump_log, InitBy},
                Event = {mnesia_overload, DetectedBy},
                mnesia_lib:report_system_event(Event),
                true;
            _ ->
                false
        end,
    mnesia_recover:log_dump_overload(Status),
    Queue2 = Queue ++ [Worker],
    State2 = State#state{dumper_queue = Queue2},
    opt_start_worker(State2);
add_worker(Worker = #schema_commit_lock{}, State) ->
    Queue = State#state.dumper_queue,
    Queue2 = Queue ++ [Worker],
    State2 = State#state{dumper_queue = Queue2},
    opt_start_worker(State2);
add_worker(Worker = #net_load{}, State) ->
    opt_start_worker(add_loader(Worker#net_load.table,Worker,State));
add_worker(Worker = #send_table{}, State) ->
    Queue = State#state.sender_queue,
    State2 = State#state{sender_queue = Queue ++ [Worker]},
    opt_start_worker(State2);
add_worker(Worker = #disc_load{}, State) ->
    opt_start_worker(add_loader(Worker#disc_load.table,Worker,State));
% Block controller should be used for upgrading mnesia.
add_worker(Worker = #block_controller{}, State) ->
    Queue = State#state.dumper_queue,
    Queue2 = [Worker | Queue],
    State2 = State#state{dumper_queue = Queue2},
    opt_start_worker(State2).

add_loader(Tab,Worker,State = #state{loader_queue=LQ0}) ->
    case gb_trees:is_defined(Tab, LQ0) of
	true -> State;
	false ->
	    LQ=gb_trees:insert(Tab, Worker, LQ0),
	    State#state{loader_queue=LQ}
    end.

%% Optionally start a worker
%%
%% Dumpers and loaders may run simultaneously
%% but neither of them may run during schema commit.
%% Loaders may not start if a schema commit is enqueued.
opt_start_worker(State) when State#state.is_stopping == true ->
    State;
opt_start_worker(State) ->
    %% Prioritize dumper and schema commit
    %% by checking them first
    case State#state.dumper_queue of
	[Worker | _Rest] when State#state.dumper_pid == undefined ->
	    %% Great, a worker in queue and neither
	    %% a schema transaction is being
	    %% committed and nor a dumper is running

	    %% Start worker but keep him in the queue
	    if
		is_record(Worker, schema_commit_lock) ->
		    ReplyTo = Worker#schema_commit_lock.owner,
		    reply(ReplyTo, granted),
		    {Owner, _Tag} = ReplyTo,
		    opt_start_loader(State#state{dumper_pid = Owner});

		is_record(Worker, dump_log) ->
		    Pid = spawn_link(?MODULE, dump_and_reply, [self(), Worker]),
		    State2 = State#state{dumper_pid = Pid},

		    %% If the worker was a dumper we may
		    %% possibly be able to start a loader
		    %% or sender
		    State3 = opt_start_sender(State2),
		    opt_start_loader(State3);

		is_record(Worker, block_controller) ->
		    case {get_senders(State), get_loaders(State)} of
			{[], []} ->
			    ReplyTo = Worker#block_controller.owner,
			    reply(ReplyTo, granted),
			    {Owner, _Tag} = ReplyTo,
			    State#state{dumper_pid = Owner};
			_ ->
			    State
		    end
	    end;
	_ ->
	    %% Bad luck, try with a loader or sender instead
	    State2 = opt_start_sender(State),
	    opt_start_loader(State2)
    end.

opt_start_sender(State) ->
    case State#state.sender_queue of
	[]->   State; 	    %% No need
	SenderQ ->
	    {NewS,Kept} = opt_start_sender2(SenderQ, get_senders(State),
					    [], get_loaders(State)),
	    State#state{sender_pid = NewS, sender_queue = Kept}
    end.

opt_start_sender2([], Pids,Kept, _) -> {Pids,Kept};
opt_start_sender2([Sender|R], Pids, Kept, LoaderQ) ->
    Tab = Sender#send_table.table,
    Active = val({Tab, active_replicas}),
    IgotIt = lists:member(node(), Active),
    IsLoading = lists:any(fun({_Pid,Loader}) ->
				  Tab == element(#net_load.table, Loader)
			  end, LoaderQ),
    if
	IgotIt, IsLoading  ->
	    %% I'm currently finishing loading the table let him wait
	    opt_start_sender2(R,Pids, [Sender|Kept], LoaderQ);
	IgotIt ->
	    %% Start worker but keep him in the queue
	    Pid = spawn_link(?MODULE, send_and_reply,[self(), Sender]),
	    opt_start_sender2(R,[{Pid,Sender}|Pids],Kept,LoaderQ);
	true ->
	    verbose("Send table failed ~p not active on this node ~n", [Tab]),
	    Sender#send_table.receiver_pid ! {copier_done, node()},
	    opt_start_sender2(R,Pids, Kept, LoaderQ)
    end.

opt_start_loader(State = #state{loader_queue = LoaderQ}) ->
    Current = get_loaders(State),
    Max = max_loaders(),
    case gb_trees:is_empty(LoaderQ) of
	true ->
	    State;
	_ when length(Current) >= Max ->
	    State;
	false ->
	    SchemaQueue = State#state.dumper_queue,
	    case lists:keymember(schema_commit_lock, 1, SchemaQueue) of
		false ->
		    case pick_next(LoaderQ) of
			{none,Rest} ->
			    State#state{loader_queue=Rest};
			{Worker,Rest} ->
			    case already_loading(Worker, get_loaders(State)) of
				true ->
				    opt_start_loader(State#state{loader_queue = Rest});
				false ->
				    %% Start worker but keep him in the queue
				    Pid = load_and_reply(self(), Worker),
				    State#state{loader_pid=[{Pid,Worker}|get_loaders(State)],
						loader_queue = Rest}
			    end
		    end;
		true ->
		    %% Bad luck, we must wait for the schema commit
		    State
	    end
    end.

already_loading(#net_load{table=Tab},Loaders) ->
    already_loading2(Tab,Loaders);
already_loading(#disc_load{table=Tab},Loaders) ->
    already_loading2(Tab,Loaders).

already_loading2(Tab, [{_,#net_load{table=Tab}}|_]) -> true;
already_loading2(Tab, [{_,#disc_load{table=Tab}}|_]) -> true;
already_loading2(Tab, [_|Rest]) -> already_loading2(Tab,Rest);
already_loading2(_,[]) -> false.

start_remote_sender(Node, Tab, Receiver, Storage) ->
    Msg = #send_table{table = Tab,
		      receiver_pid = Receiver,
		      remote_storage = Storage},
    gen_server:cast({?SERVER_NAME, Node}, Msg).

dump_and_reply(ReplyTo, Worker) ->
    %% No trap_exit, die intentionally instead
    Res = case Worker#dump_log.operation of
	      dump_log ->
		  mnesia_dumper:opt_dump_log(Worker#dump_log.initiated_by);
	      F when is_function(F, 0) ->
		  F()
	  end,
    ReplyTo ! #dumper_done{worker_pid = self(),
			   worker_res = Res},
    unlink(ReplyTo),
    exit(normal).

send_and_reply(ReplyTo, Worker) ->
    %% No trap_exit, die intentionally instead
    Res = mnesia_loader:send_table(Worker#send_table.receiver_pid,
				   Worker#send_table.table,
				   Worker#send_table.remote_storage),
    ReplyTo ! #sender_done{worker_pid = self(),
			   worker_res = Res},
    unlink(ReplyTo),
    exit(normal).

load_and_reply(ReplyTo, Worker) ->
    Load = load_table_fun(Worker),
    SendAndReply =
	fun() ->
		process_flag(trap_exit, true),
		Done = Load(),
		ReplyTo ! Done#loader_done{worker_pid = self()},
		unlink(ReplyTo),
		exit(normal)
	end,
    spawn_link(SendAndReply).

%% Now it is time to load the table
%% but first we must check if it still is neccessary
load_table_fun(#net_load{cstruct=Cs, table=Tab, reason=Reason, opt_reply_to=ReplyTo}) ->
    LocalC = val({Tab, local_content}),
    AccessMode = val({Tab, access_mode}),
    ReadNode = val({Tab, where_to_read}),
    Active = filter_active(Tab),
    Done = #loader_done{is_loaded = true,
			table_name = Tab,
			needs_announce = false,
			needs_sync = false,
			needs_reply = (ReplyTo /= undefined),
			reply_to = ReplyTo,
			reply = {loaded, ok}
		       },
    AddTableCopy = case Reason of
		       {dumper,{add_table_copy,_}} -> true;
		       _ -> false
		   end,
    if
	ReadNode == node() ->
	    %% Already loaded locally
	    fun() -> Done end;
	LocalC == true ->
	    fun() ->
		    Res = mnesia_loader:disc_load_table(Tab, load_local_content),
		    Done#loader_done{reply = Res, needs_announce = true, needs_sync = true}
	    end;
	AccessMode == read_only, not AddTableCopy ->
	    fun() -> disc_load_table(Tab, Reason, ReplyTo) end;
	true ->
	    fun() ->
		    %% Either we cannot read the table yet
		    %% or someone is moving a replica between
		    %% two nodes
		    Res = mnesia_loader:net_load_table(Tab, Reason, Active, Cs),
		    case Res of
			{loaded, ok} ->
			    Done#loader_done{needs_sync = true,
					     reply = Res};
			{not_loaded, _} ->
			    Done#loader_done{is_loaded = false,
					     reply = Res}
		    end
	    end
    end;
load_table_fun(#disc_load{table=Tab, reason=Reason, opt_reply_to=ReplyTo}) ->
    ReadNode = val({Tab, where_to_read}),
    Active = filter_active(Tab),
    Done = #loader_done{is_loaded = true,
			table_name = Tab,
			needs_announce = false,
			needs_sync = false,
			needs_reply = false
		       },
    if
	Active == [], ReadNode == nowhere ->
	    %% Not loaded anywhere, lets load it from disc
	    fun() -> disc_load_table(Tab, Reason, ReplyTo) end;
	ReadNode == nowhere ->
	    %% Already loaded on other node, lets get it
	    Cs = val({Tab, cstruct}),
	    fun() ->
		    case mnesia_loader:net_load_table(Tab, Reason, Active, Cs) of
			{loaded, ok} ->
			    Done#loader_done{needs_sync = true};
			{not_loaded, storage_unknown} ->
			    Done#loader_done{is_loaded = false};
			{not_loaded, ErrReason} ->
			    Done#loader_done{is_loaded = false,
					     reply = {not_loaded,ErrReason}}
		    end
	    end;
	true ->
	    %% Already readable, do not worry be happy
	    fun() -> Done end
    end.

disc_load_table(Tab, Reason, ReplyTo) ->
    Done = #loader_done{is_loaded = true,
			table_name = Tab,
			needs_announce = false,
			needs_sync = false,
			needs_reply = ReplyTo /= undefined,
			reply_to = ReplyTo,
			reply = {loaded, ok}
		       },
    Res = mnesia_loader:disc_load_table(Tab, Reason),
    if
	Res == {loaded, ok} ->
	    Done#loader_done{needs_announce = true,
			     needs_sync = true,
			     reply = Res};
	ReplyTo /= undefined ->
	    Done#loader_done{is_loaded = false,
			     reply = Res};
	true ->
	    fatal("Cannot load table ~p from disc: ~p~n", [Tab, Res])
    end.

filter_active(Tab) ->
    ByForce = val({Tab, load_by_force}),
    Active  = val({Tab, active_replicas}),
    Masters = mnesia_recover:get_master_nodes(Tab),
    Ns = do_filter_active(ByForce, Active, Masters),
    %% Reorder the so that we load from fastest first
    LS = ?catch_val({Tab, storage_type}),
    DOC = val({Tab, disc_only_copies}),
    {Good,Worse} =
	case LS of
	    disc_only_copies ->
		G = mnesia_lib:intersect(Ns, DOC),
		{G,Ns--G};
	    _ ->
		G = Ns -- DOC,
		{G,Ns--G}
	end,
    %% Pick a random node of the fastest
    Len = length(Good),
    if
	Len > 0 ->
	    R = erlang:phash(node(), Len+1),
	    random(R-1,Good,Worse);
	true  ->
	    Worse
    end.

random(N, [H|R], Acc) when N > 0 ->
    random(N-1,R, [H|Acc]);
random(0, L, Acc) ->
    L ++ Acc.

do_filter_active(true, Active, _Masters) ->
    Active;
do_filter_active(false, Active, []) ->
    Active;
do_filter_active(false, Active, Masters) ->
    mnesia_lib:intersect(Active, Masters).


