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
-module(mnesia_monitor).

-behaviour(gen_server).

%% Public exports
-export([
	 close_dets/1,
	 close_log/1,
	 detect_inconcistency/2,
	 get_env/1,
	 init/0,
	 mktab/2,
	 unsafe_mktab/2,
         unsafe_create_external/4,
	 mnesia_down/2,
	 needs_protocol_conversion/1,
	 negotiate_protocol/1,
	 disconnect/1,
	 open_dets/2,
	 unsafe_open_dets/2,
	 open_log/1,
	 patch_env/2,
	 protocol_version/0,
	 reopen_log/3,
	 set_env/2,
	 start/0,
	 start_proc/4,
	 sync_log/1,
	 terminate_proc/3,
	 unsafe_close_dets/1,
	 unsafe_close_log/1,
	 use_dir/0,
	 do_check_type/2
	]).

%% gen_server callbacks
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%% Internal exports
-export([
	 call/1,
	 cast/1,
	 detect_partitioned_network/2,
	 has_remote_mnesia_down/1,
	 negotiate_protocol_impl/2
	]).

-compile({no_auto_import,[error/2]}).

-import(mnesia_lib, [dbg_out/2, verbose/2, error/2, fatal/2, set/2]).

-include("mnesia.hrl").

-record(state, {supervisor, pending_negotiators = [],
		going_down = [], tm_started = false, early_connects = [],
		connecting, mq = [], remote_node_status = []}).

-define(current_protocol_version,  {8,3}).

-define(previous_protocol_version, {8,2}).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
			  [self()], [{timeout, infinity}
				     %% ,{debug, [trace]}
				    ]).

init() ->
    call(init).

mnesia_down(From, Node) ->
    cast({mnesia_down, From, Node}).

mktab(Tab, Args) ->
    unsafe_call({mktab, Tab, Args}).
unsafe_mktab(Tab, Args) ->
    unsafe_call({unsafe_mktab, Tab, Args}).

open_dets(Tab, Args) ->
    unsafe_call({open_dets, Tab, Args}).
unsafe_open_dets(Tab, Args) ->
    unsafe_call({unsafe_open_dets, Tab, Args}).

close_dets(Tab) ->
    unsafe_call({close_dets, Tab}).

unsafe_close_dets(Name) ->
    unsafe_call({unsafe_close_dets, Name}).

open_log(Args) ->
    unsafe_call({open_log, Args}).

reopen_log(Name, Fname, Head) ->
    unsafe_call({reopen_log, Name, Fname, Head}).

sync_log(Name) ->
    unsafe_call({sync_log, Name}).

close_log(Name) ->
    unsafe_call({close_log, Name}).

unsafe_close_log(Name) ->
    unsafe_call({unsafe_close_log, Name}).

unsafe_create_external(Tab, Alias, Mod, Cs) ->
    unsafe_call({unsafe_create_external, Tab, Alias, Mod, Cs}).

disconnect(Node) ->
    cast({disconnect, Node}).

%% Returns GoodNoodes
%% Creates a link to each compatible monitor and
%% protocol_version to agreed version upon success

negotiate_protocol([]) -> [];
negotiate_protocol(Nodes) ->
    call({negotiate_protocol, Nodes}).

negotiate_protocol_impl(Nodes, Requester) ->
    Version    = mnesia:system_info(version),
    Protocols  = acceptable_protocol_versions(),
    MonitorPid = whereis(?MODULE),
    Msg = {negotiate_protocol, MonitorPid, Version, Protocols},
    {Replies, _BadNodes} = multicall(Nodes, Msg),
    Res = check_protocol(Replies, Protocols),
    ?MODULE ! {protocol_negotiated,Requester,Res},
    unlink(whereis(?MODULE)),
    ok.

check_protocol([{Node, {accept, Mon, Version, Protocol}} | Tail], Protocols) ->
    case lists:member(Protocol, Protocols) of
	true ->
	    case Protocol == protocol_version() of
		true ->
		    set({protocol, Node}, {Protocol, false});
		false ->
		    set({protocol, Node}, {Protocol, true})
	    end,
	    [node(Mon) | check_protocol(Tail, Protocols)];
	false  ->
	    verbose("Failed to connect with ~p. ~p protocols rejected. "
		    "expected version = ~p, expected protocol = ~p~n",
		    [Node, Protocols, Version, Protocol]),
	    unlink(Mon), % Get rid of unneccessary link
	    check_protocol(Tail, Protocols)
    end;
check_protocol([{Node, {reject, _Mon, Version, Protocol}} | Tail], Protocols) ->
    verbose("Failed to connect with ~p. ~p protocols rejected. "
	    "expected version = ~p, expected protocol = ~p~n",
	    [Node, Protocols, Version, Protocol]),
    check_protocol(Tail, Protocols);
check_protocol([{error, _Reason} | Tail], Protocols) ->
    dbg_out("~p connect failed error: ~p~n", [?MODULE, _Reason]),
    check_protocol(Tail, Protocols);
check_protocol([{badrpc, _Reason} | Tail], Protocols) ->
    dbg_out("~p connect failed badrpc: ~p~n", [?MODULE, _Reason]),
    check_protocol(Tail, Protocols);
check_protocol([], [Protocol | _Protocols]) ->
    set(protocol_version, Protocol),
    [].

protocol_version() ->
    case ?catch_val(protocol_version) of
	{'EXIT', _} -> ?current_protocol_version;
	Version -> Version
    end.

%% A sorted list of acceptable protocols the
%% preferred protocols are first in the list
acceptable_protocol_versions() ->
    [protocol_version(), ?previous_protocol_version, {8,1}].

needs_protocol_conversion(Node) ->
    case {?catch_val({protocol, Node}), protocol_version()} of
	{{'EXIT', _}, _} ->
	    false;
	{{_, Bool}, ?current_protocol_version} ->
	    Bool;
	{{_, Bool}, _} ->
	    not Bool
    end.

cast(Msg) ->
    case whereis(?MODULE) of
	undefined -> ok;
	Pid ->  gen_server:cast(Pid, Msg)
    end.

unsafe_call(Msg) ->
    case whereis(?MODULE) of
	undefined -> {error, {node_not_running, node()}};
	Pid -> gen_server:call(Pid, Msg, infinity)
    end.

call(Msg) ->
    case whereis(?MODULE) of
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

multicall(Nodes, Msg) ->
    rpc:multicall(Nodes, ?MODULE, call, [Msg]).

start_proc(Who, Mod, Fun, Args) ->
    Args2 = [Who, Mod, Fun, Args],
    proc_lib:start_link(mnesia_sp, init_proc, Args2, infinity).

terminate_proc(Who, R, State) when R /= shutdown, R /= killed ->
    fatal("~p crashed: ~p state: ~p~n", [Who, R, State]);

terminate_proc(Who, Reason, _State) ->
    mnesia_lib:verbose("~p terminated: ~p~n", [Who, Reason]),
    ok.

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
    ?ets_new_table(mnesia_gvar, [set, public, named_table]),
    ?ets_new_table(mnesia_stats, [set, public, named_table]),
    set(subscribers, []),
    set(activity_subscribers, []),
    mnesia_lib:verbose("~p starting: ~p~n", [?MODULE, self()]),
    Version = mnesia:system_info(version),
    set(version, Version),
    dbg_out("Version: ~p~n", [Version]),

    try process_config_args(env()) of
	ok ->
	    mnesia_lib:set({'$$$_report', current_pos}, 0),
	    Level = mnesia_lib:val(debug),
	    mnesia_lib:verbose("Mnesia debug level set to ~p\n", [Level]),
	    set(mnesia_status, starting), %%  set start status
	    set({current, db_nodes}, [node()]),
	    set(use_dir, use_dir()),
	    mnesia_lib:create_counter(trans_aborts),
	    mnesia_lib:create_counter(trans_commits),
	    mnesia_lib:create_counter(trans_log_writes),
	    Left = get_env(dump_log_write_threshold),
	    mnesia_lib:set_counter(trans_log_writes_left, Left),
	    mnesia_lib:create_counter(trans_log_writes_prev),
	    mnesia_lib:create_counter(trans_restarts),
	    mnesia_lib:create_counter(trans_failures),
	    set(checkpoints, []),
	    set(pending_checkpoints, []),
	    set(pending_checkpoint_pids, []),

	    {ok, #state{supervisor = Parent}}
    catch _:Reason ->
	    mnesia_lib:report_fatal("Bad configuration: ~p~n", [Reason]),
	    {stop, {bad_config, Reason}}
    end.

use_dir() ->
    case ?catch_val(use_dir) of
	{'EXIT', _} ->
	    case get_env(schema_location) of
		disc -> true;
		opt_disc -> non_empty_dir();
		ram -> false
	    end;
	Bool ->
	    Bool
    end.

%% Returns true if the Mnesia directory contains
%% important files
non_empty_dir() ->
    mnesia_lib:exists(mnesia_bup:fallback_bup()) or
    mnesia_lib:exists(mnesia_lib:tab2dmp(schema)) or
    mnesia_lib:exists(mnesia_lib:tab2dat(schema)).

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call({mktab, Tab, Args}, _From, State) ->
    try ?ets_new_table(Tab, Args) of
	Reply ->
	    {reply, Reply, State}
    catch error:ExitReason ->
	    Msg = "Cannot create ets table",
	    Reason = {system_limit, Msg, Tab, Args, ExitReason},
	    fatal("~p~n", [Reason]),
	    {noreply, State}
    end;

handle_call({unsafe_mktab, Tab, Args}, _From, State) ->
    try ?ets_new_table(Tab, Args) of
	Reply ->
	    {reply, Reply, State}
    catch error:ExitReason ->
	    {reply, {error, ExitReason}, State}
    end;

handle_call({open_dets, Tab, Args}, _From, State) ->
    case mnesia_lib:dets_sync_open(Tab, Args) of
	{ok, Tab} ->
	    {reply, {ok, Tab}, State};

	{error, Reason} ->
	    Msg = "Cannot open dets table",
	    Error = {error, {Msg, Tab, Args, Reason}},
	    fatal("~p~n", [Error]),
	    {noreply, State}
    end;

handle_call({unsafe_open_dets, Tab, Args}, _From, State) ->
    case mnesia_lib:dets_sync_open(Tab, Args) of
	{ok, Tab} ->
	    {reply, {ok, Tab}, State};
	{error, Reason} ->
	    {reply, {error,Reason}, State}
    end;

handle_call({close_dets, Tab}, _From, State) ->
    ok = mnesia_lib:dets_sync_close(Tab),
    {reply, ok, State};

handle_call({unsafe_close_dets, Tab}, _From, State) ->
    mnesia_lib:dets_sync_close(Tab),
    {reply, ok, State};

handle_call({open_log, Args}, _From, State) ->
    Res = disk_log:open([{notify, true}|Args]),
    {reply, Res, State};

handle_call({reopen_log, Name, Fname, Head}, _From, State) ->
    case disk_log:reopen(Name, Fname, Head) of
	ok ->
	    {reply, ok, State};

        {error, Reason} ->
	    Msg = "Cannot rename disk_log file",
            Error = {error, {Msg, Name, Fname, Head, Reason}},
	    fatal("~p~n", [Error]),
 	    {noreply, State}
    end;

handle_call({sync_log, Name}, _From, State) ->
    {reply, disk_log:sync(Name), State};

handle_call({close_log, Name}, _From, State) ->
    case disk_log:close(Name) of
	ok ->
	    {reply, ok, State};

        {error, Reason} ->
	    Msg = "Cannot close disk_log file",
            Error = {error, {Msg, Name, Reason}},
	    fatal("~p~n", [Error]),
	    {noreply, State}
    end;

handle_call({unsafe_close_log, Name}, _From, State) ->
    _ = disk_log:close(Name),
    {reply, ok, State};

handle_call({unsafe_create_external, Tab, Alias, Mod, Cs}, _From, State) ->
    case catch Mod:create_table(Alias, Tab, mnesia_schema:cs2list(Cs)) of
	{'EXIT', ExitReason} ->
	    {reply, {error, ExitReason}, State};
	Reply ->
	    {reply, Reply, State}
    end;

handle_call({negotiate_protocol, Mon, _Version, _Protocols}, _From, State)
  when State#state.tm_started == false ->
    State2 =  State#state{early_connects = [node(Mon) | State#state.early_connects]},
    {reply, {node(), {reject, self(), uninitialized, uninitialized}}, State2};

%% From remote monitor..
handle_call({negotiate_protocol, Mon, Version, Protocols}, From, State)
  when node(Mon) /= node() ->
    Protocol = protocol_version(),
    MyVersion = mnesia:system_info(version),
    case lists:member(Protocol, Protocols) of
	true ->
	    accept_protocol(Mon, MyVersion, Protocol, From, State);
	false ->
	    %% in this release we should be able to handle the previous
	    %% protocol
	    case hd(Protocols) of
		?previous_protocol_version ->
		    accept_protocol(Mon, MyVersion, ?previous_protocol_version, From, State);
		_ ->
		    verbose("Connection with ~p rejected. "
			    "version = ~p, protocols = ~p, "
			    "expected version = ~p, expected protocol = ~p~n",
			    [node(Mon), Version, Protocols, MyVersion, Protocol]),
		    {reply, {node(), {reject, self(), MyVersion, Protocol}}, State}
	    end
    end;

%% Local request to negotiate with other monitors (nodes).
handle_call({negotiate_protocol, Nodes}, From, State) ->
    case mnesia_lib:intersect(State#state.going_down, Nodes) of
	[] ->
	    spawn_link(?MODULE, negotiate_protocol_impl, [Nodes, From]),
	    {noreply, State#state{connecting={From,Nodes}}};
	_ ->  %% Cannot connect now, still processing mnesia down
	    {reply, busy, State}
    end;

handle_call(init, _From, State) ->
    _ = net_kernel:monitor_nodes(true),
    EarlyNodes = State#state.early_connects,
    State2 = State#state{tm_started = true},
    {reply, EarlyNodes, State2};

handle_call(Msg, _From, State) ->
    error("~p got unexpected call: ~p~n", [?MODULE, Msg]),
    {noreply, State}.

accept_protocol(Mon, Version, Protocol, From, State) ->
    Reply = {node(), {accept, self(), Version, Protocol}},
    Node = node(Mon),
    Pending0 = State#state.pending_negotiators,
    Pending = lists:keydelete(Node, 1, Pending0),
    case lists:member(Node, State#state.going_down) of
	true ->
	    %% Wait for the mnesia_down to be processed,
	    %% before we reply
	    P = Pending ++ [{Node, Mon, From, Reply}],
	    {noreply, State#state{pending_negotiators = P}};
	false ->
	    %% No need for wait
	    link(Mon),  %% link to remote Monitor
	    case Protocol == protocol_version() of
		true ->
		    set({protocol, Node}, {Protocol, false});
		false ->
		    set({protocol, Node}, {Protocol, true})
	    end,
	    {reply, Reply, State#state{pending_negotiators = Pending}}
    end.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_cast({mnesia_down, mnesia_controller, Node}, State) ->
    mnesia_tm:mnesia_down(Node),
    {noreply, State};

handle_cast({mnesia_down, mnesia_tm, Node}, State) ->
    Down = {mnesia_down, Node},
    mnesia_lib:report_system_event(Down),
    GoingDown = lists:delete(Node, State#state.going_down),
    State2 = State#state{going_down = GoingDown},
    Pending = State#state.pending_negotiators,
    State3 = check_raise_conditon_nodeup(Node, State2),
    case lists:keysearch(Node, 1, Pending) of
	{value, {Node, Mon, ReplyTo, Reply}} ->
	    %% Late reply to remote monitor
	    link(Mon),  %% link to remote Monitor
	    gen_server:reply(ReplyTo, Reply),
	    P2 = lists:keydelete(Node, 1,Pending),
	    State4 = State3#state{pending_negotiators = P2},
	    process_q(State4);
	false ->
	    %% No pending remote monitors
	    process_q(State3)
    end;

handle_cast({disconnect, Node}, State) ->
    case rpc:call(Node, erlang, whereis, [?MODULE]) of
	{badrpc, _} ->
	    ignore;
	undefined ->
	    ignore;
	RemoteMon when is_pid(RemoteMon) ->
	    unlink(RemoteMon)
    end,
    {noreply, State};

handle_cast({inconsistent_database, Context, Node}, State) ->
    Msg = {inconsistent_database, Context, Node},
    mnesia_lib:report_system_event(Msg),
    {noreply, State};

handle_cast(Msg, State) ->
    error("~p got unexpected cast: ~p~n", [?MODULE, Msg]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_info({'EXIT', Pid, R}, State) when Pid == State#state.supervisor ->
    dbg_out("~p was ~p by supervisor~n",[?MODULE, R]),
    {stop, R, State};

handle_info({'EXIT', Pid, fatal}, State) when node(Pid) == node() ->
    dbg_out("~p got FATAL ERROR from: ~p~n",[?MODULE, Pid]),
    %% This may hang supervisor if a shutdown happens at the same time as an fatal
    %% is in progress
    %% exit(State#state.supervisor, shutdown),
    %% It is better to kill an innocent process
    ?SAFE(exit(whereis(mnesia_locker), kill)),
    {noreply, State};

handle_info(Msg = {'EXIT',Pid,_}, State) ->
    Node = node(Pid),
    if
	Node /= node(), State#state.connecting == undefined ->
	    %% Remotly linked process died, assume that it was a mnesia_monitor
	    mnesia_recover:mnesia_down(Node),
	    mnesia_controller:mnesia_down(Node),
	    {noreply, State#state{going_down = [Node | State#state.going_down]}};
	Node /= node() ->
	    {noreply, State#state{mq = State#state.mq ++ [{info, Msg}]}};
	true ->
	    %% We have probably got an exit signal from
	    %% disk_log or dets
	    Hint = "Hint: check that the disk still is writable",
	    fatal("~p got unexpected info: ~p; ~p~n",
		  [?MODULE, Msg, Hint])
    end;

handle_info({protocol_negotiated, From,Res}, State) ->
    From = element(1,State#state.connecting),
    gen_server:reply(From, Res),
    process_q(State#state{connecting = undefined});

handle_info({check_nodeup, Node}, State) ->
    State2 = check_mnesia_down(Node, State),
    {noreply, State2};

handle_info({nodeup, Node}, State) ->
    State2 = remote_node_status(Node, up, State),
    State3 = check_mnesia_down(Node, State2),
    {noreply, State3};

handle_info({nodedown, Node}, State) ->
    State2 = remote_node_status(Node, down, State),
    {noreply, State2};

handle_info({disk_log, _Node, Log, Info}, State) ->
    case Info of
	{truncated, _No} ->
	    ok;
	_ ->
	    mnesia_lib:important("Warning Log file ~p error reason ~s~n",
				 [Log, disk_log:format_error(Info)])
    end,
    {noreply, State};

handle_info(Msg, State) ->
    error("~p got unexpected info (~p): ~p~n", [?MODULE, State, Msg]).

process_q(State = #state{mq=[]}) -> {noreply,State};
process_q(State = #state{mq=[{info,Msg}|R]}) ->
    handle_info(Msg, State#state{mq=R});
process_q(State = #state{mq=[{cast,Msg}|R]}) ->
    handle_cast(Msg, State#state{mq=R});
process_q(State = #state{mq=[{call,From,Msg}|R]}) ->
    handle_call(Msg, From, State#state{mq=R}).

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    terminate_proc(?MODULE, Reason, State).

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Upgrade process when its code is to be changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------


code_change(_, {state, SUP, PN, GD, TMS, EC}, _) ->
    {ok, #state{supervisor=SUP, pending_negotiators=PN,
		going_down = GD, tm_started =TMS, early_connects = EC}};

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

process_config_args([]) ->
    ok;
process_config_args([C|T]) ->
    V = get_env(C),
    dbg_out("Env ~p: ~p~n", [C, V]),
    mnesia_lib:set(C, V),
    process_config_args(T).

set_env(E,Val) ->
    mnesia_lib:set(E, check_type(E,Val)),
    ok.

get_env(E) ->
    case ?catch_val(E) of
	{'EXIT', _} ->
	    case application:get_env(mnesia, E) of
		{ok, Val} ->
		    check_type(E, Val);
		undefined ->
		    check_type(E, default_env(E))
	    end;
	Val ->
	    Val
    end.

env() ->
    [
     access_module,
     allow_index_on_key,
     auto_repair,
     backup_module,
     debug,
     dir,
     dump_disc_copies_at_startup,
     dump_log_load_regulation,
     dump_log_time_threshold,
     dump_log_update_in_place,
     dump_log_write_threshold,
     event_module,
     extra_db_nodes,
     ignore_fallback_at_startup,
     fallback_error_function,
     fold_chunk_size,
     max_wait_for_decision,
     schema_location,
     core_dir,
     pid_sort_order,
     no_table_loaders,
     dc_dump_limit,
     send_compressed,
     schema
    ].

default_env(access_module) ->
    mnesia;
default_env(auto_repair) ->
    true;
default_env(allow_index_on_key) ->
    false;
default_env(backup_module) ->
    mnesia_backup;
default_env(debug) ->
    none;
default_env(dir) ->
    Name = lists:concat(["Mnesia.", node()]),
    filename:absname(Name);
default_env(dump_disc_copies_at_startup) ->
    true;
default_env(dump_log_load_regulation) ->
    false;
default_env(dump_log_time_threshold) ->
    timer:minutes(3);
default_env(dump_log_update_in_place) ->
    true;
default_env(dump_log_write_threshold) ->
    1000;
default_env(event_module) ->
    mnesia_event;
default_env(extra_db_nodes) ->
    [];
default_env(ignore_fallback_at_startup) ->
    false;
default_env(fallback_error_function) ->
    {mnesia, lkill};
default_env(fold_chunk_size) ->
    100;
default_env(max_wait_for_decision) ->
    infinity;
default_env(schema_location) ->
    opt_disc;
default_env(core_dir) ->
    false;
default_env(pid_sort_order) ->
    false;
default_env(no_table_loaders) ->
    2;
default_env(dc_dump_limit) ->
    4;
default_env(send_compressed) ->
    0;
default_env(schema) ->
    [].

check_type(Env, Val) ->
    try do_check_type(Env, Val)
    catch error:_ -> exit({bad_config, Env, Val})
    end.

do_check_type(access_module, A) when is_atom(A) -> A;
do_check_type(allow_index_on_key, B) -> bool(B);
do_check_type(auto_repair, B) -> bool(B);
do_check_type(backup_module, B) when is_atom(B) -> B;
do_check_type(debug, debug) -> debug;
do_check_type(debug, false) -> none;
do_check_type(debug, none) -> none;
do_check_type(debug, trace) -> trace;
do_check_type(debug, true) -> debug;
do_check_type(debug, verbose) -> verbose;
do_check_type(dir, V) -> filename:absname(V);
do_check_type(dump_disc_copies_at_startup, B) -> bool(B);
do_check_type(dump_log_load_regulation, B) -> bool(B);
do_check_type(dump_log_time_threshold, I) when is_integer(I), I > 0 -> I;
do_check_type(dump_log_update_in_place, B) -> bool(B);
do_check_type(dump_log_write_threshold, I) when is_integer(I), I > 0 -> I;
do_check_type(event_module, A) when is_atom(A) -> A;
do_check_type(ignore_fallback_at_startup, B) -> bool(B);
do_check_type(fallback_error_function, {Mod, Func})
  when is_atom(Mod), is_atom(Func) -> {Mod, Func};
do_check_type(extra_db_nodes, L) when is_list(L) ->
    Fun = fun(N) when N == node() -> false;
	     (A) when is_atom(A) -> true
	  end,
    lists:filter(Fun, L);
do_check_type(fold_chunk_size, I) when is_integer(I), I > 0;
				       I =:= infinity -> I;
do_check_type(max_wait_for_decision, infinity) -> infinity;
do_check_type(max_wait_for_decision, I) when is_integer(I), I > 0 -> I;
do_check_type(schema_location, M) -> media(M);
do_check_type(core_dir, "false") -> false;
do_check_type(core_dir, false) -> false;
do_check_type(core_dir, Dir) when is_list(Dir) -> Dir;
do_check_type(pid_sort_order, r9b_plain) -> r9b_plain;
do_check_type(pid_sort_order, "r9b_plain") -> r9b_plain;
do_check_type(pid_sort_order, standard) -> standard;
do_check_type(pid_sort_order, "standard") -> standard;
do_check_type(pid_sort_order, _) -> false;
do_check_type(no_table_loaders, N) when is_integer(N), N > 0 -> N;
do_check_type(dc_dump_limit,N) when is_number(N), N > 0 -> N;
do_check_type(send_compressed, L) when is_integer(L), L >= 0, L =< 9 -> L;
do_check_type(schema, L) when is_list(L) -> L.

bool(true) -> true;
bool(false) -> false.

media(disc) -> disc;
media(opt_disc) -> opt_disc;
media(ram) -> ram.

patch_env(Env, Val) ->
    try do_check_type(Env, Val) of
	NewVal ->
	    application_controller:set_env(mnesia, Env, NewVal),
	    NewVal
    catch error:_ ->
	    {error, {bad_type, Env, Val}}
    end.

detect_partitioned_network(Mon, Node) ->
    detect_inconcistency([Node], running_partitioned_network),
    unlink(Mon),
    exit(normal).

detect_inconcistency([], _Context) ->
    ok;
detect_inconcistency(Nodes, Context) ->
    Downs = [N || N <- Nodes, mnesia_recover:has_mnesia_down(N)],
    {Replies, _BadNodes} =
	rpc:multicall(Downs, ?MODULE, has_remote_mnesia_down, [node()]),
    report_inconsistency(Replies, Context, ok).

has_remote_mnesia_down(Node) ->
    HasDown = mnesia_recover:has_mnesia_down(Node),
    Master  = mnesia_recover:get_master_nodes(schema),
    if
	HasDown == true, Master == [] ->
	    {true, node()};
	true ->
	    {false, node()}
    end.

report_inconsistency([{true, Node} | Replies], Context, _Status) ->
    %% Oops, Mnesia is already running on the
    %% other node AND we both regard each
    %% other as down. The database is
    %% potentially inconsistent and we has to
    %% do tell the applications about it, so
    %% they may perform some clever recovery
    %% action.
    Msg = {inconsistent_database, Context, Node},
    mnesia_lib:report_system_event(Msg),
    report_inconsistency(Replies, Context, inconsistent_database);
report_inconsistency([{false, _Node} | Replies], Context, Status) ->
    report_inconsistency(Replies, Context, Status);
report_inconsistency([{badrpc, _Reason} | Replies], Context, Status) ->
    report_inconsistency(Replies, Context, Status);
report_inconsistency([], _Context, Status) ->
    Status.

remote_node_status(Node, Status, State) ->
    {ok, Nodes} = mnesia_schema:read_nodes(),
    case lists:member(Node, Nodes) of
	true ->
	    update_node_status({Node, Status}, State);
	_ ->
	    State
    end.

update_node_status({Node, down}, State = #state{remote_node_status = RNodeS}) ->
    RNodeS2 = lists:ukeymerge(1, [{Node, down}], RNodeS),
    State#state{remote_node_status = RNodeS2};
update_node_status({Node, up}, State = #state{remote_node_status = RNodeS}) ->
    case lists:keyfind(Node, 1, RNodeS) of
	{Node, down} ->
	    RNodeS2 = lists:ukeymerge(1, [{Node, up}], RNodeS),
	    State#state{remote_node_status = RNodeS2};
	_ ->
	    State
    end.

check_raise_conditon_nodeup(Node, State = #state{remote_node_status = RNodeS}) ->
    case lists:keyfind(Node, 1, RNodeS) of
	{Node, up} ->
	    self() ! {check_nodeup, Node};
	_ ->
	    ignore
    end,
    State#state{remote_node_status = lists:keydelete(Node, 1, RNodeS)}.

check_mnesia_down(Node, State = #state{remote_node_status = RNodeS}) ->
    %% Check if the network has been partitioned
    %% due to communication failure.

    HasDown   = mnesia_recover:has_mnesia_down(Node),
    ImRunning = mnesia_lib:is_running(),
    if
	%% If I'm not running the test will be made later.
	HasDown == true, ImRunning == yes ->
	    spawn_link(?MODULE, detect_partitioned_network, [self(), Node]),
	    State#state{remote_node_status = lists:keydelete(Node, 1, RNodeS)};
	true ->
	    State
    end.
