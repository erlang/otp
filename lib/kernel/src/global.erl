%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
-module(global).
-behaviour(gen_server).

%% Global provides global registration of process names. The names are
%% dynamically kept up to date with the entire network. Global can
%% operate in two modes: in a fully connected network, or in a
%% non-fully connected network. In the latter case, the name
%% registration mechanism won't work. 
%% As a separate service Global also provides global locks.

%% External exports
-export([start/0, start_link/0, stop/0, sync/0, sync/1,
	 whereis_name/1,  register_name/2,
         register_name/3, register_name_external/2, register_name_external/3,
         unregister_name_external/1,re_register_name/2, re_register_name/3,
	 unregister_name/1, registered_names/0, send/2, node_disconnected/1,
	 set_lock/1, set_lock/2, set_lock/3,
	 del_lock/1, del_lock/2,
	 trans/2, trans/3, trans/4,
	 random_exit_name/3, random_notify_name/3, notify_all_name/3]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3, resolve_it/4]).

-export([info/0]).

-include_lib("stdlib/include/ms_transform.hrl").

%% Set this variable to 'allow' to allow several names of a process.
%% This is for backward compatibility only; the functionality is broken.
-define(WARN_DUPLICATED_NAME, global_multi_name_action).

%% Undocumented Kernel variable. Set this to 0 (zero) to get the old
%% behaviour.
-define(N_CONNECT_RETRIES, global_connect_retries).
-define(DEFAULT_N_CONNECT_RETRIES, 5).

%%% In certain places in the server, calling io:format hangs everything,
%%% so we'd better use erlang:display/1.
%%% my_tracer is used in testsuites

%% uncomment this if tracing is wanted
%%-define(DEBUG, true).
-ifdef(DEBUG).
-define(trace(T), erlang:display({format, node(), cs(), T})).
  cs() ->
     {_Big, Small, Tiny} = erlang:timestamp(),
     (Small rem 100) * 100 + (Tiny div 10000).
%-define(trace(T), (catch my_tracer ! {node(), {line,?LINE}, T})).
-else.
-define(trace(_), ok).
-endif.

%% These are the protocol versions:
%% Vsn 1 is the original protocol.
%% Vsn 2 is enhanced with code to take care of registration of names from
%%       non erlang nodes, e.g. C-nodes.
%% Vsn 3 is enhanced with a tag in the synch messages to distinguish
%%       different synch sessions from each other, see OTP-2766.
%% Vsn 4 uses a single, permanent, locker process, but works like vsn 3
%%       when communicating with vsn 3 nodes. (-R10B)
%% Vsn 5 uses an ordered list of self() and HisTheLocker when locking
%%       nodes in the own partition. (R11B-)

%% Current version of global does not support vsn 4 or earlier.

-define(vsn, 5).

%%-----------------------------------------------------------------
%% connect_all = boolean() - true if we are supposed to set up a
%%                           fully connected net
%% known       = [Node] - all nodes known to us
%% synced      = [Node] - all nodes that have the same names as us
%% resolvers   = [{Node, MyTag, Resolver}] - 
%%                        the tag separating different synch sessions, 
%%                        and the pid of the name resolver process
%% syncers     = [pid()] - all current syncers processes
%% node_name   = atom()  - our node name (can change if distribution
%%                         is started/stopped dynamically)
%%
%% In addition to these, we keep info about messages arrived in
%% the process dictionary:
%% {pre_connect, Node} = {Vsn, InitMsg} - init_connect msgs that
%%                         arrived before nodeup
%% {wait_lock, Node}   = {exchange, NameList, _NamelistExt} | lock_is_set
%%                        - see comment below (handle_cast)
%% {save_ops, Node}    = {resolved, HisKnown, NamesExt, Res} | [operation()] 
%%                        - save the ops between exchange and resolved
%% {prot_vsn, Node}    = Vsn - the exchange protocol version (not used now)
%% {sync_tag_my, Node} =  My tag, used at synchronization with Node
%% {sync_tag_his, Node} = The Node's tag, used at synchronization
%% {lock_id, Node} = The resource locking the partitions
%%-----------------------------------------------------------------
-record(state, {connect_all        :: boolean(),
		known = []         :: [node()],
		synced = []        :: [node()],
		resolvers = [],
		syncers = []       :: [pid()],
		node_name = node() :: node(),
		the_locker, the_registrar, trace,
                global_lock_down = false :: boolean()
               }).
-type state() :: #state{}.

%%% There are also ETS tables used for bookkeeping of locks and names
%%% (the first position is the key):
%%%
%%% global_locks (set): {ResourceId, LockRequesterId, [{Pid,RPid,ref()]}
%%%   Pid is locking ResourceId, ref() is the monitor ref.
%%%   RPid =/= Pid if there is an extra process calling erlang:monitor().
%%% global_names (set):  {Name, Pid, Method, RPid, ref()}
%%%   Registered names. ref() is the monitor ref.
%%%   RPid =/= Pid if there is an extra process calling erlang:monitor().
%%% global_names_ext (set): {Name, Pid, RegNode}
%%%   External registered names (C-nodes).
%%%   (The RPid:s can be removed when/if erlang:monitor() returns before 
%%%    trying to connect to the other node.)
%%% 
%%% Helper tables:
%%% global_pid_names (bag): {Pid, Name} | {ref(), Name}
%%%   Name(s) registered for Pid.
%%%   There is one {Pid, Name} and one {ref(), Name} for every Pid.
%%%   ref() is the same ref() as in global_names.
%%% global_pid_ids (bag): {Pid, ResourceId} | {ref(), ResourceId}
%%%   Resources locked by Pid.
%%%   ref() is the same ref() as in global_locks.
%%%
%%% global_pid_names is a 'bag' for backward compatibility.
%%% (Before vsn 5 more than one name could be registered for a process.)
%%%
%%% R11B-3 (OTP-6341): The list of pids in the table 'global_locks'
%%% was replaced by a list of {Pid, Ref}, where Ref is a monitor ref.
%%% It was necessary to use monitors to fix bugs regarding locks that
%%% were never removed. The signal {async_del_lock, ...} has been
%%% kept for backward compatibility. It can be removed later.
%%% 
%%% R11B-4 (OTP-6428): Monitors are used for registered names.
%%% The signal {delete_name, ...} has been kept for backward compatibility.
%%% It can be removed later as can the deleter process.
%%% An extra process calling erlang:monitor() is sometimes created.
%%% The new_nodes messages has been augmented with the global lock id.
%%%
%%% R14A (OTP-8527): The deleter process has been removed.

start() -> 
    gen_server:start({local, global_name_server}, ?MODULE, [], []).

start_link() -> 
    gen_server:start_link({local, global_name_server}, ?MODULE, [], []).

stop() -> 
    gen_server:call(global_name_server, stop, infinity).

-spec sync() -> 'ok' | {'error', Reason :: term()}.
sync() ->
    case check_sync_nodes() of
	{error, _} = Error ->
	    Error;
	SyncNodes ->
	    gen_server:call(global_name_server, {sync, SyncNodes}, infinity)
    end.

-spec sync([node()]) -> 'ok' | {'error', Reason :: term()}.
sync(Nodes) ->
    case check_sync_nodes(Nodes) of
	{error, _} = Error ->
	    Error;
	SyncNodes ->
	    gen_server:call(global_name_server, {sync, SyncNodes}, infinity)
    end.

-spec send(Name, Msg) -> Pid when
      Name :: term(),
      Msg :: term(),
      Pid :: pid().
send(Name, Msg) ->
    case whereis_name(Name) of
	Pid when is_pid(Pid) ->
	    Pid ! Msg,
	    Pid;
	undefined ->
	    exit({badarg, {Name, Msg}})
    end.

%% See OTP-3737.
-spec whereis_name(Name) -> pid() | 'undefined' when
      Name :: term().
whereis_name(Name) ->
    where(Name).

node_disconnected(Node) ->
    global_name_server ! {nodedown, Node}.

%%-----------------------------------------------------------------
%% Method = function(Name, Pid1, Pid2) -> Pid | Pid2 | none
%% Method is called if a name conflict is detected when two nodes
%% are connecting to each other. It is supposed to return one of
%% the Pids or 'none'. If a pid is returned, that pid is
%% registered as Name on all nodes. If 'none' is returned, the
%% Name is unregistered on all nodes. If anything else is returned,
%% the Name is unregistered as well.
%% Method is called once at one of the nodes where the processes reside
%% only. If different Methods are used for the same name, it is
%% undefined which one of them is used.
%% Method blocks the name registration, but does not affect global locking.
%%-----------------------------------------------------------------
-spec register_name(Name, Pid) -> 'yes' | 'no' when
      Name :: term(),
      Pid :: pid().
register_name(Name, Pid) when is_pid(Pid) ->
    register_name(Name, Pid, fun random_exit_name/3).

-type method() :: fun((Name :: term(), Pid :: pid(), Pid2 :: pid()) ->
                             pid() | 'none').

-spec register_name(Name, Pid, Resolve) -> 'yes' | 'no' when
      Name :: term(),
      Pid :: pid(),
      Resolve :: method().
register_name(Name, Pid, Method0) when is_pid(Pid) ->
    Method = allow_tuple_fun(Method0),
    Fun = fun(Nodes) ->
        case (where(Name) =:= undefined) andalso check_dupname(Name, Pid) of
            true ->
                gen_server:multi_call(Nodes,
                                      global_name_server,
                                      {register, Name, Pid, Method}),
                yes;
            _ ->
                no
        end
    end,
    ?trace({register_name, self(), Name, Pid, Method}),
    gen_server:call(global_name_server, {registrar, Fun}, infinity).

check_dupname(Name, Pid) ->
    case ets:lookup(global_pid_names, Pid) of
        [] ->
            true;
        PidNames -> 
            case application:get_env(kernel, ?WARN_DUPLICATED_NAME) of
                {ok, allow} ->
                    true;
                _ ->
                    S = "global: ~w registered under several names: ~tw\n",
                    Names = [Name | [Name1 || {_Pid, Name1} <- PidNames]],
                    error_logger:error_msg(S, [Pid, Names]),
                    false
            end
    end.

-spec unregister_name(Name) -> _ when
      Name :: term().
unregister_name(Name) ->
    case where(Name) of
	undefined ->
	    ok;
	_ ->
	    Fun = fun(Nodes) ->
			  gen_server:multi_call(Nodes,
						global_name_server,
						{unregister, Name}),
			  ok
		  end,
            ?trace({unregister_name, self(), Name}),
            gen_server:call(global_name_server, {registrar, Fun}, infinity)
    end.

-spec re_register_name(Name, Pid) -> 'yes' when
      Name :: term(),
      Pid :: pid().
re_register_name(Name, Pid) when is_pid(Pid) ->
    re_register_name(Name, Pid, fun random_exit_name/3).

-spec re_register_name(Name, Pid, Resolve) -> 'yes' when
      Name :: term(),
      Pid :: pid(),
      Resolve :: method().
re_register_name(Name, Pid, Method0) when is_pid(Pid) ->
    Method = allow_tuple_fun(Method0),
    Fun = fun(Nodes) ->
		  gen_server:multi_call(Nodes,
					global_name_server,
					{register, Name, Pid, Method}),
		  yes
	  end,
    ?trace({re_register_name, self(), Name, Pid, Method}),
    gen_server:call(global_name_server, {registrar, Fun}, infinity).

-spec registered_names() -> [Name] when
      Name :: term().
registered_names() ->
    MS = ets:fun2ms(fun({Name,_Pid,_M,_RP,_R}) -> Name end),
    ets:select(global_names, MS).

%%-----------------------------------------------------------------
%% The external node (e.g. a C-node) registers the name on an Erlang
%% node which links to the process (an Erlang node has to be used
%% since there is no global_name_server on the C-node). If the Erlang
%% node dies the name is to be unregistered on all nodes. Normally
%% node(Pid) is compared to the node that died, but that does not work
%% for external nodes (the process does not run on the Erlang node
%% that died). Therefore a table of all names registered by external
%% nodes is kept up-to-date on all nodes.
%%
%% Note: if the Erlang node dies an EXIT signal is also sent to the
%% C-node due to the link between the global_name_server and the
%% registered process. [This is why the link has been kept despite
%% the fact that monitors do the job now.]
%%-----------------------------------------------------------------
register_name_external(Name, Pid) when is_pid(Pid) ->
    register_name_external(Name, Pid, fun random_exit_name/3).

register_name_external(Name, Pid, Method) when is_pid(Pid) ->
    Fun = fun(Nodes) ->
		  case where(Name) of
		      undefined ->
			  gen_server:multi_call(Nodes,
						global_name_server,
						{register_ext, Name, Pid, 
                                                 Method, node()}),
			  yes;
		      _Pid -> no
		  end
	  end,
    ?trace({register_name_external, self(), Name, Pid, Method}),
    gen_server:call(global_name_server, {registrar, Fun}, infinity).

unregister_name_external(Name) ->
    unregister_name(Name).

-type id() :: {ResourceId :: term(), LockRequesterId :: term()}.

-spec set_lock(Id) -> boolean() when
      Id :: id().
set_lock(Id) ->
    set_lock(Id, [node() | nodes()], infinity, 1).

-type retries() :: non_neg_integer() | 'infinity'.

-spec set_lock(Id, Nodes) -> boolean() when
      Id :: id(),
      Nodes :: [node()].
set_lock(Id, Nodes) ->
    set_lock(Id, Nodes, infinity, 1).

-spec set_lock(Id, Nodes, Retries) -> boolean() when
      Id :: id(),
      Nodes :: [node()],
      Retries :: retries().
set_lock(Id, Nodes, Retries) when is_integer(Retries), Retries >= 0 ->
    set_lock(Id, Nodes, Retries, 1);
set_lock(Id, Nodes, infinity) ->
    set_lock(Id, Nodes, infinity, 1).

set_lock({_ResourceId, _LockRequesterId}, [], _Retries, _Times) ->
    true;
set_lock({_ResourceId, _LockRequesterId} = Id, Nodes, Retries, Times) ->
    ?trace({set_lock,{me,self()},Id,{nodes,Nodes},
            {retries,Retries}, {times,Times}}),
    case set_lock_on_nodes(Id, Nodes) of
	true -> 
            ?trace({set_lock_true, Id}),
            true;
        false=Reply when Retries =:= 0 ->
            Reply;
	false ->
	    random_sleep(Times),
	    set_lock(Id, Nodes, dec(Retries), Times+1)
    end.

-spec del_lock(Id) -> 'true' when
      Id :: id().
del_lock(Id) ->
    del_lock(Id, [node() | nodes()]).

-spec del_lock(Id, Nodes) -> 'true' when
      Id :: id(),
      Nodes :: [node()].
del_lock({_ResourceId, _LockRequesterId} = Id, Nodes) ->
    ?trace({del_lock, {me,self()}, Id, {nodes,Nodes}}),
    gen_server:multi_call(Nodes, global_name_server, {del_lock, Id}),
    true.

-type trans_fun() :: function() | {module(), atom()}.

-spec trans(Id, Fun) -> Res | aborted when
      Id :: id(),
      Fun :: trans_fun(),
      Res :: term().
trans(Id, Fun) -> trans(Id, Fun, [node() | nodes()], infinity).

-spec trans(Id, Fun, Nodes) -> Res | aborted when
      Id :: id(),
      Fun :: trans_fun(),
      Nodes :: [node()],
      Res :: term().
trans(Id, Fun, Nodes) -> trans(Id, Fun, Nodes, infinity).

-spec trans(Id, Fun, Nodes, Retries) -> Res | aborted when
      Id :: id(),
      Fun :: trans_fun(),
      Nodes :: [node()],
      Retries :: retries(),
      Res :: term().
trans(Id, Fun, Nodes, Retries) ->
    case set_lock(Id, Nodes, Retries) of
	true ->
            try 
                Fun()
            after
                del_lock(Id, Nodes)
            end;
	false ->
	    aborted
    end.

info() ->
    gen_server:call(global_name_server, info, infinity).

%%%-----------------------------------------------------------------
%%% Call-back functions from gen_server
%%%-----------------------------------------------------------------

-spec init([]) -> {'ok', state()}.

init([]) ->
    process_flag(trap_exit, true),
    _ = ets:new(global_locks, [set, named_table, protected]),
    _ = ets:new(global_names, [set, named_table, protected,
                               {read_concurrency, true}]),
    _ = ets:new(global_names_ext, [set, named_table, protected]),

    _ = ets:new(global_pid_names, [bag, named_table, protected]),
    _ = ets:new(global_pid_ids, [bag, named_table, protected]),

    %% This is for troubleshooting only.
    DoTrace = os:getenv("GLOBAL_HIGH_LEVEL_TRACE") =:= "TRUE",
    T0 = case DoTrace of
             true -> 
                 send_high_level_trace(),
                 [];
             false -> 
                 no_trace
         end,

    Ca = case init:get_argument(connect_all) of
             {ok, [["false"]]} ->
                 false;
             _ ->
                 true
         end,
    S = #state{the_locker = start_the_locker(DoTrace),
               trace = T0,
               the_registrar = start_the_registrar(),
               connect_all = Ca},
    {ok, trace_message(S, {init, node()}, [])}.

%%-----------------------------------------------------------------
%% Connection algorithm
%% ====================
%% This algorithm solves the problem with partitioned nets as well.
%%
%% The main idea in the algorithm is that when two nodes connect, they
%% try to set a lock in their own partition (i.e. all nodes already
%% known to them; partitions are not necessarily disjoint). When the
%% lock is set in each partition, these two nodes send each other a
%% list with all registered names in resp partition (*). If no conflict
%% is found, the name tables are just updated. If a conflict is found,
%% a resolve function is called once for each conflict. The result of
%% the resolving is sent to the other node. When the names are
%% exchanged, all other nodes in each partition are informed of the
%% other nodes, and they ping each other to form a fully connected
%% net.
%%
%% A few remarks:
%% 
%% (*) When this information is being exchanged, no one is allowed to
%%     change the global register table. All calls to register etc are
%%     protected by a lock. If a registered process dies during this
%%     phase the name is unregistered on the local node immediately,
%%     but the unregistration on other nodes will take place when the
%%     deleter manages to acquire the lock. This is necessary to
%%     prevent names from spreading to nodes where they cannot be
%%     deleted.
%%
%% - It is assumed that nodeups and nodedowns arrive in an orderly
%%   fashion: for every node, nodeup is followed by nodedown, and vice
%%   versa. "Double" nodeups and nodedowns must never occur. It is
%%   the responsibility of net_kernel to assure this.
%%
%% - There is always a delay between the termination of a registered
%%   process and the removal of the name from Global's tables. This
%%   delay can sometimes be quite substantial. Global guarantees that
%%   the name will eventually be removed, but there is no
%%   synchronization between nodes; the name can be removed from some
%%   node(s) long before it is removed from other nodes.
%%
%% - Global cannot handle problems with the distribution very well.
%%   Depending on the value of the kernel variable 'net_ticktime' long
%%   delays may occur. This does not affect the handling of locks but
%%   will block name registration.
%% 
%% - Old synch session messages may linger on in the message queue of
%%   global_name_server after the sending node has died. The tags of
%%   such messages do not match the current tag (if there is one),
%%   which makes it possible to discard those messages and cancel the
%%   corresponding lock.
%%
%% Suppose nodes A and B connect, and C is connected to A.
%% Here's the algorithm's flow:
%%
%% Node A
%% ------
%% << {nodeup, B}
%%   TheLocker ! {nodeup, ..., Node, ...} (there is one locker per node)
%% B ! {init_connect, ..., {..., TheLockerAtA, ...}}
%% << {init_connect, TheLockerAtB}
%%   [The lockers try to set the lock]
%% << {lock_is_set, B, ...}
%%   [Now, lock is set in both partitions]
%% B ! {exchange, A, Names, ...}
%% << {exchange, B, Names, ...}
%%   [solve conflict]
%% B ! {resolved, A, ResolvedA, KnownAtA, ...}
%% << {resolved, B, ResolvedB, KnownAtB, ...}
%% C ! {new_nodes, ResolvedAandB, [B]}
%%
%% Node C
%% ------
%% << {new_nodes, ResolvedOps, NewNodes}
%%   [insert Ops]
%% ping(NewNodes)
%% << {nodeup, B}
%% <ignore this one>
%%
%% Several things can disturb this picture.
%%
%% First, the init_connect message may arrive _before_ the nodeup
%% message due to delay in net_kernel. We handle this by keeping track
%% of these messages in the pre_connect variable in our state.
%%
%% Of course we must handle that some node goes down during the
%% connection.
%%
%%-----------------------------------------------------------------
%% Messages in the protocol
%% ========================
%% 1. Between global_name_servers on connecting nodes
%%    {init_connect, Vsn, Node, InitMsg}
%%         InitMsg = {locker, _Unused, HisKnown, HisTheLocker}
%%    {exchange, Node, ListOfNames, _ListOfNamesExt, Tag}
%%    {resolved, Node, HisOps, HisKnown, _Unused, ListOfNamesExt, Tag}
%%         HisKnown = list of known nodes in Node's partition
%% 2. Between lockers on connecting nodes
%%    {his_locker, Pid} (from our global)
%%    {lock, Bool} loop until both lockers have lock = true,
%%          then send to global_name_server {lock_is_set, Node, Tag}
%% 3. Connecting node's global_name_server informs other nodes in the same 
%%    partition about hitherto unknown nodes in the other partition
%%    {new_nodes, Node, Ops, ListOfNamesExt, NewNodes, ExtraInfo}
%% 4. Between global_name_server and resolver
%%    {resolve, NameList, Node} to resolver
%%    {exchange_ops, Node, Tag, Ops, Resolved} from resolver
%% 5. sync protocol, between global_name_servers in different partitions
%%    {in_sync, Node, IsKnown}
%%          sent by each node to all new nodes (Node becomes known to them)
%%-----------------------------------------------------------------

-spec handle_call(term(), {pid(), term()}, state()) ->
        {'noreply', state()} |
	{'reply', term(), state()} |
	{'stop', 'normal', 'stopped', state()}.

handle_call({registrar, Fun}, From, S) ->
    S#state.the_registrar ! {trans_all_known, Fun, From},
    {noreply, S};

%% The pattern {register,'_','_','_'} is traced by the inviso
%% application. Do not change.
handle_call({register, Name, Pid, Method}, {FromPid, _Tag}, S0) ->
    S = ins_name(Name, Pid, Method, FromPid, [], S0),
    {reply, yes, S};

handle_call({unregister, Name}, _From, S0) ->
    S = delete_global_name2(Name, S0),
    {reply, ok, S};

handle_call({register_ext, Name, Pid, Method, RegNode}, {FromPid,_Tag}, S0) ->
    S = ins_name_ext(Name, Pid, Method, RegNode, FromPid, [], S0),
    {reply, yes, S};

handle_call({set_lock, Lock}, {Pid, _Tag}, S0) ->
    {Reply, S} = handle_set_lock(Lock, Pid, S0),
    {reply, Reply, S};

handle_call({del_lock, Lock}, {Pid, _Tag}, S0) ->
    S = handle_del_lock(Lock, Pid, S0),
    {reply, true, S};

handle_call(get_known, _From, S) ->
    {reply, S#state.known, S};

handle_call(get_synced, _From, S) ->
    {reply, S#state.synced, S};

handle_call({sync, Nodes}, From, S) ->
    %% If we have several global groups, this won't work, since we will
    %% do start_sync on a nonempty list of nodes even if the system
    %% is quiet.
    Pid = start_sync(lists:delete(node(), Nodes) -- S#state.synced, From),
    {noreply, S#state{syncers = [Pid | S#state.syncers]}};

handle_call(get_protocol_version, _From, S) ->
    {reply, ?vsn, S};

handle_call(get_names_ext, _From, S) ->
    {reply, get_names_ext(), S};

handle_call(info, _From, S) ->
    {reply, S, S};

%% "High level trace". For troubleshooting only.
handle_call(high_level_trace_start, _From, S) ->
    S#state.the_locker ! {do_trace, true},
    send_high_level_trace(),
    {reply, ok, trace_message(S#state{trace = []}, {init, node()}, [])};
handle_call(high_level_trace_stop, _From, S) ->
    #state{the_locker = TheLocker, trace = Trace} = S,
    TheLocker ! {do_trace, false},
    wait_high_level_trace(),
    {reply, Trace, S#state{trace = no_trace}};
handle_call(high_level_trace_get, _From, #state{trace = Trace}=S) ->
    {reply, Trace, S#state{trace = []}};

handle_call(stop, _From, S) ->
    {stop, normal, stopped, S};

handle_call(Request, From, S) ->
    error_logger:warning_msg("The global_name_server "
                             "received an unexpected message:\n"
                             "handle_call(~tp, ~tp, _)\n",
                             [Request, From]),
    {noreply, S}.

%%========================================================================
%% init_connect
%%
%%========================================================================

-spec handle_cast(term(), state()) -> {'noreply', state()}.

handle_cast({init_connect, Vsn, Node, InitMsg}, S) ->
    %% Sent from global_name_server at Node.
    ?trace({'####', init_connect, {vsn, Vsn}, {node,Node},{initmsg,InitMsg}}),
    case Vsn of
	%% It is always the responsibility of newer versions to understand
	%% older versions of the protocol.
	{HisVsn, HisTag} when HisVsn > ?vsn ->
	    init_connect(?vsn, Node, InitMsg, HisTag, S#state.resolvers, S);
	{HisVsn, HisTag} ->
	    init_connect(HisVsn, Node, InitMsg, HisTag, S#state.resolvers, S);
	%% To be future compatible
	Tuple when is_tuple(Tuple) ->
	    List = tuple_to_list(Tuple),
	    [_HisVsn, HisTag | _] = List,
	    %% use own version handling if his is newer.
	    init_connect(?vsn, Node, InitMsg, HisTag, S#state.resolvers, S);
	_ ->
	    Txt = io_lib:format("Illegal global protocol version ~p Node: ~p\n",
                                [Vsn, Node]),
	    error_logger:info_report(lists:flatten(Txt))
    end,
    {noreply, S};

%%=======================================================================
%% lock_is_set
%%
%% Ok, the lock is now set on both partitions. Send our names to other node.
%%=======================================================================
handle_cast({lock_is_set, Node, MyTag, LockId}, S) ->
    %% Sent from the_locker at node().
    ?trace({'####', lock_is_set , {node,Node}}),
    case get({sync_tag_my, Node}) of
	MyTag ->
            lock_is_set(Node, S#state.resolvers, LockId),
            {noreply, S};
	_ -> %% Illegal tag, delete the old sync session.
	    NewS = cancel_locker(Node, S, MyTag),
	    {noreply, NewS}
    end;

%%========================================================================
%% exchange
%%
%% Here the names are checked to detect name clashes.
%%========================================================================
handle_cast({exchange, Node, NameList, _NameExtList, MyTag}, S) ->
    %% Sent from global_name_server at Node.
    case get({sync_tag_my, Node}) of
	MyTag ->
	    exchange(Node, NameList, S#state.resolvers),
	    {noreply, S};
	_ -> %% Illegal tag, delete the old sync session.
	    NewS = cancel_locker(Node, S, MyTag),
	    {noreply, NewS}
    end;

%% {exchange_ops, ...} is sent by the resolver process (which then
%% dies). It could happen that {resolved, ...} has already arrived
%% from the other node. In that case we can go ahead and run the
%% resolve operations. Otherwise we have to save the operations and
%% wait for {resolve, ...}. This is very much like {lock_is_set, ...}
%% and {exchange, ...}.
handle_cast({exchange_ops, Node, MyTag, Ops, Resolved}, S0) ->
    %% Sent from the resolver for Node at node().
    ?trace({exchange_ops, {node,Node}, {ops,Ops},{resolved,Resolved},
            {mytag,MyTag}}),
    S = trace_message(S0, {exit_resolver, Node}, [MyTag]),
    case get({sync_tag_my, Node}) of
	MyTag ->
            Known = S#state.known,
	    gen_server:cast({global_name_server, Node},
			    {resolved, node(), Resolved, Known,
			     Known,get_names_ext(),get({sync_tag_his,Node})}),
            case get({save_ops, Node}) of
                {resolved, HisKnown, Names_ext, HisResolved} ->
                    put({save_ops, Node}, Ops),
                    NewS = resolved(Node, HisResolved, HisKnown, Names_ext,S),
                    {noreply, NewS};
                undefined -> 
                    put({save_ops, Node}, Ops),
                    {noreply, S}
            end;
	_ -> %% Illegal tag, delete the old sync session.
	    NewS = cancel_locker(Node, S, MyTag),
	    {noreply, NewS}
    end;

%%========================================================================
%% resolved
%%
%% Here the name clashes are resolved.
%%========================================================================
handle_cast({resolved, Node, HisResolved, HisKnown, _HisKnown_v2, 
             Names_ext, MyTag}, S) ->
    %% Sent from global_name_server at Node.
    ?trace({'####', resolved, {his_resolved,HisResolved}, {node,Node}}),
    case get({sync_tag_my, Node}) of
	MyTag -> 
            %% See the comment at handle_case({exchange_ops, ...}).
            case get({save_ops, Node}) of
                Ops when is_list(Ops) ->
                    NewS = resolved(Node, HisResolved, HisKnown, Names_ext, S),
                    {noreply, NewS};
                undefined ->
                    Resolved = {resolved, HisKnown, Names_ext, HisResolved},
                    put({save_ops, Node}, Resolved),
                    {noreply, S}
            end;
	_ -> %% Illegal tag, delete the old sync session.
	    NewS = cancel_locker(Node, S, MyTag),
	    {noreply, NewS}
    end;

%%========================================================================
%% new_nodes
%%
%% We get to know the other node's known nodes.
%%========================================================================
handle_cast({new_nodes, Node, Ops, Names_ext, Nodes, ExtraInfo}, S) ->
    %% Sent from global_name_server at Node.
    ?trace({new_nodes, {node,Node},{ops,Ops},{nodes,Nodes},{x,ExtraInfo}}),
    NewS = new_nodes(Ops, Node, Names_ext, Nodes, ExtraInfo, S),
    {noreply, NewS};

%%========================================================================
%% in_sync
%%
%% We are in sync with this node (from the other node's known world).
%%========================================================================
handle_cast({in_sync, Node, _IsKnown}, S) ->
    %% Sent from global_name_server at Node (in the other partition).
    ?trace({'####', in_sync, {Node, _IsKnown}}),
    lists:foreach(fun(Pid) -> Pid ! {synced, [Node]} end, S#state.syncers),
    NewS = cancel_locker(Node, S, get({sync_tag_my, Node})),
    reset_node_state(Node),
    NSynced = case lists:member(Node, Synced = NewS#state.synced) of
		  true -> Synced;
		  false -> [Node | Synced]
	      end,
    {noreply, NewS#state{synced = NSynced}};

%% Called when Pid on other node crashed
handle_cast({async_del_name, _Name, _Pid}, S) ->
    %% Sent from the_deleter at some node in the partition but node() (-R13B)
    %% The DOWN message deletes the name.
    %% R14A nodes and later do not send async_del_name messages.
    {noreply, S};

handle_cast({async_del_lock, _ResourceId, _Pid}, S) ->
    %% Sent from global_name_server at some node in the partition but
    %% node(). (-R13B)
    %% The DOWN message deletes the lock.
    %% R14A nodes and later do not send async_del_lock messages.
    {noreply, S};

handle_cast(Request, S) ->
    error_logger:warning_msg("The global_name_server "
                             "received an unexpected message:\n"
                             "handle_cast(~tp, _)\n", [Request]),
    {noreply, S}.

%%========================================================================

-spec handle_info(term(), state()) ->
        {'noreply', state()} | {'stop', term(), state()}.

handle_info({'EXIT', Locker, _Reason}=Exit, #state{the_locker=Locker}=S) ->
    {stop, {locker_died,Exit}, S#state{the_locker=undefined}};
handle_info({'EXIT', Registrar, _}=Exit, #state{the_registrar=Registrar}=S) ->
    {stop, {registrar_died,Exit}, S#state{the_registrar=undefined}};
handle_info({'EXIT', Pid, _Reason}, S) when is_pid(Pid) ->
    ?trace({global_EXIT,_Reason,Pid}),
    %% The process that died was a synch process started by start_sync
    %% or a registered process running on an external node (C-node).
    %% Links to external names are ignored here (there are also DOWN
    %% signals).
    Syncers = lists:delete(Pid, S#state.syncers),
    {noreply, S#state{syncers = Syncers}};

handle_info({nodedown, Node}, S) when Node =:= S#state.node_name ->
    %% Somebody stopped the distribution dynamically - change
    %% references to old node name (Node) to new node name ('nonode@nohost')
    {noreply, change_our_node_name(node(), S)};

handle_info({nodedown, Node}, S0) ->
    ?trace({'####', nodedown, {node,Node}}),
    S1 = trace_message(S0, {nodedown, Node}, []),
    S = handle_nodedown(Node, S1),
    {noreply, S};

handle_info({extra_nodedown, Node}, S0) ->
    ?trace({'####', extra_nodedown, {node,Node}}),
    S1 = trace_message(S0, {extra_nodedown, Node}, []),
    S = handle_nodedown(Node, S1),
    {noreply, S};

handle_info({nodeup, Node}, S) when Node =:= node() ->
    ?trace({'####', local_nodeup, {node, Node}}),
    %% Somebody started the distribution dynamically - change
    %% references to old node name ('nonode@nohost') to Node.
    {noreply, change_our_node_name(Node, S)};

handle_info({nodeup, _Node}, S) when not S#state.connect_all ->
    {noreply, S};

handle_info({nodeup, Node}, S0) when S0#state.connect_all ->
    IsKnown = lists:member(Node, S0#state.known) or
              %% This one is only for double nodeups (shouldn't occur!)
              lists:keymember(Node, 1, S0#state.resolvers),
    ?trace({'####', nodeup, {node,Node}, {isknown,IsKnown}}),
    S1 = trace_message(S0, {nodeup, Node}, []),
    case IsKnown of
	true ->
	    {noreply, S1};
	false ->
	    resend_pre_connect(Node),

	    %% erlang:unique_integer([monotonic]) is used as a tag to
	    %% separate different synch sessions
	    %% from each others. Global could be confused at bursty nodeups
	    %% because it couldn't separate the messages between the different
	    %% synch sessions started by a nodeup.
	    MyTag = erlang:unique_integer([monotonic]),
	    put({sync_tag_my, Node}, MyTag),
            ?trace({sending_nodeup_to_locker, {node,Node},{mytag,MyTag}}),
	    S1#state.the_locker ! {nodeup, Node, MyTag},

            %% In order to be compatible with unpatched R7 a locker
            %% process was spawned. Vsn 5 is no longer compatible with
            %% vsn 3 nodes, so the locker process is no longer needed.
            %% The permanent locker takes its place.
            NotAPid = no_longer_a_pid,
            Locker = {locker, NotAPid, S1#state.known, S1#state.the_locker},
	    InitC = {init_connect, {?vsn, MyTag}, node(), Locker},
	    Rs = S1#state.resolvers,
            ?trace({casting_init_connect, {node,Node},{initmessage,InitC},
                    {resolvers,Rs}}),
	    gen_server:cast({global_name_server, Node}, InitC),
            Resolver = start_resolver(Node, MyTag),
            S = trace_message(S1, {new_resolver, Node}, [MyTag, Resolver]),
	    {noreply, S#state{resolvers = [{Node, MyTag, Resolver} | Rs]}}
    end;

handle_info({whereis, Name, From}, S) ->
    do_whereis(Name, From),
    {noreply, S};

handle_info(known, S) ->
    io:format(">>>> ~p\n",[S#state.known]),
    {noreply, S};

%% "High level trace". For troubleshooting only.
handle_info(high_level_trace, S) ->
    case S of 
        #state{trace = [{Node, _Time, _M, Nodes, _X} | _]} ->
            send_high_level_trace(),
            CNode = node(),
            CNodes = nodes(),
            case {CNode, CNodes} of
                {Node, Nodes} ->
                    {noreply, S};
                _ ->
                    {New, _, Old} = 
                        sofs:symmetric_partition(sofs:set([CNode|CNodes]),
                                                 sofs:set([Node|Nodes])),
                    M = {nodes_changed, {sofs:to_external(New),
                                         sofs:to_external(Old)}},
                    {noreply, trace_message(S, M, [])}
            end;
        _ ->
            {noreply, S}
    end;
handle_info({trace_message, M}, S) ->
    {noreply, trace_message(S, M, [])};
handle_info({trace_message, M, X}, S) ->
    {noreply, trace_message(S, M, X)};

handle_info({'DOWN', MonitorRef, process, _Pid, _Info}, S0) ->
    S1 = delete_lock(MonitorRef, S0),
    S = del_name(MonitorRef, S1),
    {noreply, S};

handle_info(Message, S) ->
    error_logger:warning_msg("The global_name_server "
                             "received an unexpected message:\n"
                             "handle_info(~tp, _)\n", [Message]),
    {noreply, S}.


%%========================================================================
%%========================================================================
%%=============================== Internal Functions =====================
%%========================================================================
%%========================================================================

-define(HIGH_LEVEL_TRACE_INTERVAL, 500). % ms

wait_high_level_trace() ->
    receive
        high_level_trace ->
            ok
    after ?HIGH_LEVEL_TRACE_INTERVAL+1 ->
            ok
    end.

send_high_level_trace() ->
    erlang:send_after(?HIGH_LEVEL_TRACE_INTERVAL, self(), high_level_trace).

-define(GLOBAL_RID, global).

%% Similar to trans(Id, Fun), but always uses global's own lock
%% on all nodes known to global, making sure that no new nodes have
%% become known while we got the list of known nodes.
trans_all_known(Fun) ->
    Id = {?GLOBAL_RID, self()},
    Nodes = set_lock_known(Id, 0),
    try
        Fun(Nodes)
    after
        delete_global_lock(Id, Nodes)
    end.

set_lock_known(Id, Times) -> 
    Known = get_known(),
    Nodes = [node() | Known],
    Boss = the_boss(Nodes),
    %% Use the  same convention (a boss) as lock_nodes_safely. Optimization.
    case set_lock_on_nodes(Id, [Boss]) of
        true ->
            case lock_on_known_nodes(Id, Known, Nodes) of
                true ->
                    Nodes;
                false -> 
                    del_lock(Id, [Boss]),
                    random_sleep(Times),
                    set_lock_known(Id, Times+1)
            end;
        false ->
            random_sleep(Times),
            set_lock_known(Id, Times+1)
    end.

lock_on_known_nodes(Id, Known, Nodes) ->
    case set_lock_on_nodes(Id, Nodes) of
        true ->
            (get_known() -- Known) =:= [];
        false ->
            false
    end.

set_lock_on_nodes(_Id, []) ->
    true;
set_lock_on_nodes(Id, Nodes) ->
    case local_lock_check(Id, Nodes) of 
        true ->
            Msg = {set_lock, Id},
            {Replies, _} = 
                gen_server:multi_call(Nodes, global_name_server, Msg),
            ?trace({set_lock,{me,self()},Id,{nodes,Nodes},{replies,Replies}}),
            check_replies(Replies, Id, Replies);
        false=Reply ->
            Reply
    end.

%% Probe lock on local node to see if one should go on trying other nodes.
local_lock_check(_Id, [_] = _Nodes) ->
    true;
local_lock_check(Id, Nodes) ->
    not lists:member(node(), Nodes) orelse (can_set_lock(Id) =/= false).

check_replies([{_Node, true} | T], Id, Replies) ->
    check_replies(T, Id, Replies);
check_replies([{_Node, false=Reply} | _T], _Id, [_]) ->
    Reply;
check_replies([{_Node, false=Reply} | _T], Id, Replies) ->
    TrueReplyNodes = [N || {N, true} <- Replies],
    ?trace({check_replies, {true_reply_nodes, TrueReplyNodes}}),
    gen_server:multi_call(TrueReplyNodes, global_name_server, {del_lock, Id}),
    Reply;
check_replies([], _Id, _Replies) ->
    true.

%%========================================================================
%% Another node wants to synchronize its registered names with us.
%% Both nodes must have a lock before they are allowed to continue.
%%========================================================================
init_connect(Vsn, Node, InitMsg, HisTag, Resolvers, S) ->
    %% It is always the responsibility of newer versions to understand
    %% older versions of the protocol.
    put({prot_vsn, Node}, Vsn),
    put({sync_tag_his, Node}, HisTag),
    case lists:keyfind(Node, 1, Resolvers) of
        {Node, MyTag, _Resolver} ->
            MyTag = get({sync_tag_my, Node}), % assertion
	    {locker, _NoLongerAPid, _HisKnown0, HisTheLocker} = InitMsg,
	    ?trace({init_connect,{histhelocker,HisTheLocker}}),
	    HisKnown = [],
	    S#state.the_locker ! {his_the_locker, HisTheLocker,
				  {Vsn,HisKnown}, S#state.known};
        false ->
            ?trace({init_connect,{pre_connect,Node},{histag,HisTag}}),
            put({pre_connect, Node}, {Vsn, InitMsg, HisTag})
    end.

%%========================================================================
%% In the simple case, we'll get lock_is_set before we get exchange,
%% but we may get exchange before we get lock_is_set from our locker.
%% If that's the case, we'll have to remember the exchange info, and
%% handle it when we get the lock_is_set. We do this by using the
%% process dictionary - when the lock_is_set msg is received, we store
%% this info. When exchange is received, we can check the dictionary
%% if the lock_is_set has been received. If not, we store info about
%% the exchange instead. In the lock_is_set we must first check if
%% exchange info is stored, in that case we take care of it.
%%========================================================================
lock_is_set(Node, Resolvers, LockId) ->
    gen_server:cast({global_name_server, Node},
                    {exchange, node(), get_names(), _ExtNames = [],
                     get({sync_tag_his, Node})}),
    put({lock_id, Node}, LockId),
    %% If both have the lock, continue with exchange.
    case get({wait_lock, Node}) of
	{exchange, NameList} ->
	    put({wait_lock, Node}, lock_is_set),
	    exchange(Node, NameList, Resolvers);
	undefined ->
	    put({wait_lock, Node}, lock_is_set)
    end.

%%========================================================================
%% exchange
%%========================================================================
exchange(Node, NameList, Resolvers) ->
    ?trace({'####', exchange, {node,Node}, {namelist,NameList}, 
            {resolvers, Resolvers}}),
    case erase({wait_lock, Node}) of
	lock_is_set ->
            {Node, _Tag, Resolver} = lists:keyfind(Node, 1, Resolvers),
            Resolver ! {resolve, NameList, Node};
	undefined ->
	    put({wait_lock, Node}, {exchange, NameList})
    end.

resolved(Node, HisResolved, HisKnown, Names_ext, S0) ->
    Ops = erase({save_ops, Node}) ++ HisResolved,
    %% Known may have shrunk since the lock was taken (due to nodedowns).
    Known = S0#state.known,
    Synced = S0#state.synced,
    NewNodes = [Node | HisKnown],
    sync_others(HisKnown),
    ExtraInfo = [{vsn,get({prot_vsn, Node})}, {lock, get({lock_id, Node})}],
    S = do_ops(Ops, node(), Names_ext, ExtraInfo, S0),
    %% I am synced with Node, but not with HisKnown yet
    lists:foreach(fun(Pid) -> Pid ! {synced, [Node]} end, S#state.syncers),
    S3 = lists:foldl(fun(Node1, S1) -> 
                             F = fun(Tag) -> cancel_locker(Node1,S1,Tag) end,
                             cancel_resolved_locker(Node1, F)
                     end, S, HisKnown),
    %% The locker that took the lock is asked to send 
    %% the {new_nodes, ...} message. This ensures that
    %% {del_lock, ...} is received after {new_nodes, ...} 
    %% (except when abcast spawns process(es)...).
    NewNodesF = fun() ->
                        gen_server:abcast(Known, global_name_server,
                                          {new_nodes, node(), Ops, Names_ext,
                                           NewNodes, ExtraInfo})
                end,
    F = fun(Tag) -> cancel_locker(Node, S3, Tag, NewNodesF) end,
    S4 = cancel_resolved_locker(Node, F),
    %% See (*) below... we're node b in that description
    AddedNodes = (NewNodes -- Known),
    NewKnown = Known ++ AddedNodes,
    S4#state.the_locker ! {add_to_known, AddedNodes},
    NewS = trace_message(S4, {added, AddedNodes}, 
                         [{new_nodes, NewNodes}, {abcast, Known}, {ops,Ops}]),
    NewS#state{known = NewKnown, synced = [Node | Synced]}.

cancel_resolved_locker(Node, CancelFun) ->
    Tag = get({sync_tag_my, Node}),
    ?trace({calling_cancel_locker,Tag,get()}),
    S = CancelFun(Tag),
    reset_node_state(Node),
    S.

new_nodes(Ops, ConnNode, Names_ext, Nodes, ExtraInfo, S0) ->
    Known = S0#state.known,
    %% (*) This one requires some thought...
    %% We're node a, other nodes b and c:
    %% The problem is that {in_sync, a} may arrive before {resolved, [a]} to
    %% b from c, leading to b sending {new_nodes, [a]} to us (node a).
    %% Therefore, we make sure we never get duplicates in Known.
    AddedNodes = lists:delete(node(), Nodes -- Known),
    sync_others(AddedNodes),
    S = do_ops(Ops, ConnNode, Names_ext, ExtraInfo, S0),
    ?trace({added_nodes_in_sync,{added_nodes,AddedNodes}}),
    S#state.the_locker ! {add_to_known, AddedNodes},
    S1 = trace_message(S, {added, AddedNodes}, [{ops,Ops}]),
    S1#state{known = Known ++ AddedNodes}.

do_whereis(Name, From) ->
    case is_global_lock_set() of
	false ->
	    gen_server:reply(From, where(Name));
	true ->
	    send_again({whereis, Name, From})
    end.

-spec terminate(term(), state()) -> 'ok'.

terminate(_Reason, _S) ->
    true = ets:delete(global_names),
    true = ets:delete(global_names_ext),
    true = ets:delete(global_locks),
    true = ets:delete(global_pid_names),
    true = ets:delete(global_pid_ids),
    ok.

-spec code_change(term(), state(), term()) -> {'ok', state()}.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%% The resolver runs exchange_names in a separate process. The effect
%% is that locks can be used at the same time as name resolution takes
%% place.
start_resolver(Node, MyTag) ->
    spawn(fun() -> resolver(Node, MyTag) end).

resolver(Node, Tag) ->
    receive 
        {resolve, NameList, Node} ->
            ?trace({resolver, {me,self()}, {node,Node}, {namelist,NameList}}),
            {Ops, Resolved} = exchange_names(NameList, Node, [], []),
            Exchange = {exchange_ops, Node, Tag, Ops, Resolved},
            gen_server:cast(global_name_server, Exchange),
            exit(normal);
        _ -> % Ignore garbage.
            resolver(Node, Tag)
    end.

resend_pre_connect(Node) ->
    case erase({pre_connect, Node}) of
	{Vsn, InitMsg, HisTag} ->
	    gen_server:cast(self(), 
                            {init_connect, {Vsn, HisTag}, Node, InitMsg});
	_ ->
	    ok
    end.

ins_name(Name, Pid, Method, FromPidOrNode, ExtraInfo, S0) ->
    ?trace({ins_name,insert,{name,Name},{pid,Pid}}),
    S1 = delete_global_name_keep_pid(Name, S0),
    S = trace_message(S1, {ins_name, node(Pid)}, [Name, Pid]),
    insert_global_name(Name, Pid, Method, FromPidOrNode, ExtraInfo, S).

ins_name_ext(Name, Pid, Method, RegNode, FromPidOrNode, ExtraInfo, S0) ->
    ?trace({ins_name_ext, {name,Name}, {pid,Pid}}),
    S1 = delete_global_name_keep_pid(Name, S0),
    dolink_ext(Pid, RegNode),
    S = trace_message(S1, {ins_name_ext, node(Pid)}, [Name, Pid]),
    true = ets:insert(global_names_ext, {Name, Pid, RegNode}),
    insert_global_name(Name, Pid, Method, FromPidOrNode, ExtraInfo, S).

where(Name) ->
    case ets:lookup(global_names, Name) of
	[{_Name, Pid, _Method, _RPid, _Ref}] ->
	    if node(Pid) == node() ->
		    case is_process_alive(Pid) of
			true  -> Pid;
			false -> undefined
		    end;
	       true ->
		    Pid
	    end;
	[] -> undefined
    end.

handle_set_lock(Id, Pid, S) ->
    ?trace({handle_set_lock, Id, Pid}),
    case can_set_lock(Id) of
        {true, PidRefs} ->
	    case pid_is_locking(Pid, PidRefs) of
		true -> 
                    {true, S};
		false -> 
                    {true, insert_lock(Id, Pid, PidRefs, S)}
	    end;
        false=Reply ->
            {Reply, S}
    end.

can_set_lock({ResourceId, LockRequesterId}) ->
    case ets:lookup(global_locks, ResourceId) of
	[{ResourceId, LockRequesterId, PidRefs}] ->
            {true, PidRefs};
	[{ResourceId, _LockRequesterId2, _PidRefs}] ->
            false;
	[] ->
            {true, []}
    end.

insert_lock({ResourceId, LockRequesterId}=Id, Pid, PidRefs, S) ->
    {RPid, Ref} = do_monitor(Pid),
    true = ets:insert(global_pid_ids, {Pid, ResourceId}),
    true = ets:insert(global_pid_ids, {Ref, ResourceId}),
    Lock = {ResourceId, LockRequesterId, [{Pid,RPid,Ref} | PidRefs]},
    true = ets:insert(global_locks, Lock),
    trace_message(S, {ins_lock, node(Pid)}, [Id, Pid]).

is_global_lock_set() ->
    is_lock_set(?GLOBAL_RID).

is_lock_set(ResourceId) ->
    ets:member(global_locks, ResourceId).

handle_del_lock({ResourceId, LockReqId}, Pid, S0) ->
    ?trace({handle_del_lock, {pid,Pid},{id,{ResourceId, LockReqId}}}),
    case ets:lookup(global_locks, ResourceId) of
	[{ResourceId, LockReqId, PidRefs}]->
            remove_lock(ResourceId, LockReqId, Pid, PidRefs, false, S0);
	_ -> S0
    end.

remove_lock(ResourceId, LockRequesterId, Pid, [{Pid,RPid,Ref}], Down, S0) ->
    ?trace({remove_lock_1, {id,ResourceId},{pid,Pid}}),
    true = erlang:demonitor(Ref, [flush]),
    kill_monitor_proc(RPid, Pid),
    true = ets:delete(global_locks, ResourceId),
    true = ets:delete_object(global_pid_ids, {Pid, ResourceId}),
    true = ets:delete_object(global_pid_ids, {Ref, ResourceId}),
    S = case ResourceId of
            ?GLOBAL_RID -> S0#state{global_lock_down = Down};
            _ -> S0
        end,
    trace_message(S, {rem_lock, node(Pid)}, 
                  [{ResourceId, LockRequesterId}, Pid]);
remove_lock(ResourceId, LockRequesterId, Pid, PidRefs0, _Down, S) ->
    ?trace({remove_lock_2, {id,ResourceId},{pid,Pid}}),
    PidRefs = case lists:keyfind(Pid, 1, PidRefs0) of
                  {Pid, RPid, Ref} ->
                      true = erlang:demonitor(Ref, [flush]),
                      kill_monitor_proc(RPid, Pid),
                      true = ets:delete_object(global_pid_ids, 
                                               {Ref, ResourceId}),
                      lists:keydelete(Pid, 1, PidRefs0);
                  false ->
                      PidRefs0
              end,
    Lock = {ResourceId, LockRequesterId, PidRefs},
    true = ets:insert(global_locks, Lock),
    true = ets:delete_object(global_pid_ids, {Pid, ResourceId}),
    trace_message(S, {rem_lock, node(Pid)}, 
                  [{ResourceId, LockRequesterId}, Pid]).

kill_monitor_proc(Pid, Pid) ->
    ok;
kill_monitor_proc(RPid, _Pid) ->
    exit(RPid, kill).

do_ops(Ops, ConnNode, Names_ext, ExtraInfo, S0) ->
    ?trace({do_ops, {ops,Ops}}),

    XInserts = [{Name, Pid, RegNode, Method} || 
                   {Name2, Pid2, RegNode} <- Names_ext,
                   {insert, {Name, Pid, Method}} <- Ops,
                   Name =:= Name2, Pid =:= Pid2],
    S1 = lists:foldl(fun({Name, Pid, RegNode, Method}, S1) ->
                             ins_name_ext(Name, Pid, Method, RegNode, 
                                          ConnNode, ExtraInfo, S1)
                     end, S0, XInserts),

    XNames = [Name || {Name, _Pid, _RegNode, _Method} <- XInserts],
    Inserts = [{Name, Pid, node(Pid), Method} || 
                  {insert, {Name, Pid, Method}} <- Ops,
                  not lists:member(Name, XNames)],
    S2 = lists:foldl(fun({Name, Pid, _RegNode, Method}, S2) ->
                            ins_name(Name, Pid, Method, ConnNode, 
                                     ExtraInfo, S2)
                    end, S1, Inserts),

    DelNames = [Name || {delete, Name} <- Ops],
    lists:foldl(fun(Name, S) -> delete_global_name2(Name, S) 
                end, S2, DelNames).

%% It is possible that a node that was up and running when the
%% operations were assembled has since died. The final {in_sync,...}
%% messages do not generate nodedown messages for such nodes. To
%% compensate "artificial" nodedown messages are created. Since
%% monitor_node may take some time processes are spawned to avoid
%% locking up the global_name_server. Should somehow double nodedown
%% messages occur (one of them artificial), nothing bad can happen
%% (the second nodedown is a no-op). It is assumed that there cannot
%% be a nodeup before the artificial nodedown.
%%
%% The extra nodedown messages generated here also take care of the
%% case that a nodedown message is received _before_ the operations
%% are run.
sync_others(Nodes) ->
    N = case application:get_env(kernel, ?N_CONNECT_RETRIES) of
            {ok, NRetries} when is_integer(NRetries), 
                                NRetries >= 0 -> NRetries;
            _ -> ?DEFAULT_N_CONNECT_RETRIES
        end,
    lists:foreach(fun(Node) -> 
                          spawn(fun() -> sync_other(Node, N) end)
                  end, Nodes).

sync_other(Node, N) ->
    erlang:monitor_node(Node, true, [allow_passive_connect]),
    receive
        {nodedown, Node} when N > 0 ->
            sync_other(Node, N - 1);
        {nodedown, Node} ->
            ?trace({missing_nodedown, {node, Node}}),
            error_logger:warning_msg("global: ~w failed to connect to ~w\n",
                                     [node(), Node]),
            global_name_server ! {extra_nodedown, Node}
    after 0 ->
            gen_server:cast({global_name_server,Node}, {in_sync,node(),true})
    end.
    % monitor_node(Node, false),
    % exit(normal).

insert_global_name(Name, Pid, Method, FromPidOrNode, ExtraInfo, S) ->
    {RPid, Ref} = do_monitor(Pid),
    true = ets:insert(global_names, {Name, Pid, Method, RPid, Ref}),
    true = ets:insert(global_pid_names, {Pid, Name}),
    true = ets:insert(global_pid_names, {Ref, Name}),
    case lock_still_set(FromPidOrNode, ExtraInfo, S) of
        true -> 
            S;
        false ->
            %% The node that took the lock has gone down and then up
            %% again. The {register, ...} or {new_nodes, ...} message
            %% was delayed and arrived after nodeup (maybe it caused
            %% the nodeup). The DOWN signal from the monitor of the
            %% lock has removed the lock. 
            %% Note: it is assumed here that the DOWN signal arrives
            %% _before_ nodeup and any message that caused nodeup.
            %% This is true of Erlang/OTP.
            delete_global_name2(Name, S)
    end.

lock_still_set(PidOrNode, ExtraInfo, S) ->
    case ets:lookup(global_locks, ?GLOBAL_RID) of
        [{?GLOBAL_RID, _LockReqId, PidRefs}] when is_pid(PidOrNode) -> 
            %% Name registration.
            lists:keymember(PidOrNode, 1, PidRefs);
        [{?GLOBAL_RID, LockReqId, _PidRefs}] when is_atom(PidOrNode) ->
            {?GLOBAL_RID, LockId} = extra_info(lock, ExtraInfo),
            LockReqId =:= LockId;
        [] ->
            not S#state.global_lock_down
    end.

extra_info(Tag, ExtraInfo) ->
    %% ExtraInfo used to be a list of nodes (vsn 2).
    case catch lists:keyfind(Tag, 1, ExtraInfo) of
        {Tag, Info} ->
            Info;
        _ ->
            undefined
    end.

del_name(Ref, S) ->
    NameL = [Name || 
                {_, Name} <- ets:lookup(global_pid_names, Ref),
                {_, _Pid, _Method, _RPid, Ref1} <- 
                    ets:lookup(global_names, Name),
                Ref1 =:= Ref],
    case NameL of
        [Name] ->
            delete_global_name2(Name, S);
        [] ->
            S
    end.

%% Keeps the entry in global_names for whereis_name/1.
delete_global_name_keep_pid(Name, S) ->
    case ets:lookup(global_names, Name) of
        [{Name, Pid, _Method, RPid, Ref}] ->
            delete_global_name2(Name, Pid, RPid, Ref, S);
        [] ->
            S
    end.

delete_global_name2(Name, S) ->
    case ets:lookup(global_names, Name) of
        [{Name, Pid, _Method, RPid, Ref}] ->
            true = ets:delete(global_names, Name),
            delete_global_name2(Name, Pid, RPid, Ref, S);
        [] ->
            S
    end.

delete_global_name2(Name, Pid, RPid, Ref, S) ->
    true = erlang:demonitor(Ref, [flush]),
    kill_monitor_proc(RPid, Pid),
    delete_global_name(Name, Pid),
    ?trace({delete_global_name,{item,Name},{pid,Pid}}),
    true = ets:delete_object(global_pid_names, {Pid, Name}),
    true = ets:delete_object(global_pid_names, {Ref, Name}),
    case ets:lookup(global_names_ext, Name) of
	[{Name, Pid, RegNode}] ->
            true = ets:delete(global_names_ext, Name),
            ?trace({delete_global_name, {name,Name,{pid,Pid},{RegNode,Pid}}}),
	    dounlink_ext(Pid, RegNode);
	[] ->
            ?trace({delete_global_name,{name,Name,{pid,Pid},{node(Pid),Pid}}}),
            ok
    end,
    trace_message(S, {del_name, node(Pid)}, [Name, Pid]).

%% delete_global_name/2 is traced by the inviso application. 
%% Do not change.
delete_global_name(_Name, _Pid) ->
    ok.

%%-----------------------------------------------------------------
%% The locker is a satellite process to global_name_server. When a
%% nodeup is received from a new node the global_name_server sends a
%% message to the locker. The locker tries to set a lock in our
%% partition, i.e. on all nodes known to us. When the lock is set, it
%% tells global_name_server about it, and keeps the lock set.
%% global_name_server sends a cancel message to the locker when the
%% partitions are connected.

%% There are two versions of the protocol between lockers on two nodes:
%% Version 1: used by unpatched R7.
%% Version 2: the messages exchanged between the lockers include the known 
%%            nodes (see OTP-3576).
%%-----------------------------------------------------------------

-define(locker_vsn, 2).

-record(multi, 
        {local = [],          % Requests from nodes on the local host.
         remote = [],         % Other requests.
         known = [],          % Copy of global_name_server's known nodes. It's
                              % faster to keep a copy of known than asking 
                              % for it when needed.
         the_boss,            % max([node() | 'known'])
         just_synced = false, % true if node() synced just a moment ago
                              %% Statistics:
         do_trace             % bool()
        }).

-record(him, {node, locker, vsn, my_tag}).

start_the_locker(DoTrace) ->
    spawn_link(init_the_locker_fun(DoTrace)).

-spec init_the_locker_fun(boolean()) -> fun(() -> no_return()).

init_the_locker_fun(DoTrace) ->
    fun() ->
            process_flag(trap_exit, true),    % needed?
            S0 = #multi{do_trace = DoTrace},
            S1 = update_locker_known({add, get_known()}, S0),
            loop_the_locker(S1),
            erlang:error(locker_exited)
    end.

loop_the_locker(S) ->
    ?trace({loop_the_locker,S}),
    receive 
        Message when element(1, Message) =/= nodeup ->
            the_locker_message(Message, S)
    after 0 ->
            Timeout = 
                case {S#multi.local, S#multi.remote} of
                    {[],[]} ->
                        infinity;
                    _ ->
                        %% It is important that the timeout is greater
                        %% than zero, or the chance that some other node
                        %% in the partition sets the lock once this node
                        %% has failed after setting the lock is very slim.
                        if
                            S#multi.just_synced ->
                                0; % no reason to wait after success
                            S#multi.known =:= [] ->
                                200; % just to get started 
                            true ->
                                erlang:min(1000 + 100*length(S#multi.known),
                                           3000)
                        end
                end,
            S1 = S#multi{just_synced = false},
            receive 
                Message when element(1, Message) =/= nodeup ->
                    the_locker_message(Message, S1)
            after Timeout ->
                    case is_global_lock_set() of
                        true -> 
                            loop_the_locker(S1);
                        false -> 
                            select_node(S1)
                    end
            end
    end.

the_locker_message({his_the_locker, HisTheLocker, HisKnown0, _MyKnown}, S) ->
    ?trace({his_the_locker, HisTheLocker, {node,node(HisTheLocker)}}),
    {HisVsn, _HisKnown} = HisKnown0,
    true = HisVsn > 4,
    receive
        {nodeup, Node, MyTag} when node(HisTheLocker) =:= Node ->
            ?trace({the_locker_nodeup, {node,Node},{mytag,MyTag}}),
            Him = #him{node = node(HisTheLocker), my_tag = MyTag,
                       locker = HisTheLocker, vsn = HisVsn},
            loop_the_locker(add_node(Him, S));
        {cancel, Node, _Tag, no_fun} when node(HisTheLocker) =:= Node ->
            loop_the_locker(S)
    after 60000 ->
            ?trace({nodeupnevercame, node(HisTheLocker)}),
            error_logger:error_msg("global: nodeup never came ~w ~w\n",
                                   [node(), node(HisTheLocker)]),
            loop_the_locker(S#multi{just_synced = false})
    end;
the_locker_message({cancel, _Node, undefined, no_fun}, S) ->
    ?trace({cancel_the_locker, undefined, {node,_Node}}),
    %% If we actually cancel something when a cancel message with the
    %% tag 'undefined' arrives, we may be acting on an old nodedown,
    %% to cancel a new nodeup, so we can't do that.
    loop_the_locker(S);
the_locker_message({cancel, Node, Tag, no_fun}, S) ->
    ?trace({the_locker, cancel, {multi,S}, {tag,Tag},{node,Node}}),
    receive
        {nodeup, Node, Tag} ->
            ?trace({cancelnodeup2, {node,Node},{tag,Tag}}),
            ok
    after 0 ->
            ok
    end,
    loop_the_locker(remove_node(Node, S));
the_locker_message({lock_set, _Pid, false, _}, S) ->
    ?trace({the_locker, spurious, {node,node(_Pid)}}),
    loop_the_locker(S);
the_locker_message({lock_set, Pid, true, _HisKnown}, S) ->
    Node = node(Pid),
    ?trace({the_locker, self(), spontaneous, {node,Node}}),
    case find_node_tag(Node, S) of
        {true, MyTag, HisVsn} ->
            LockId = locker_lock_id(Pid, HisVsn),
            {IsLockSet, S1} = lock_nodes_safely(LockId, [], S),
            Pid ! {lock_set, self(), IsLockSet, S1#multi.known},
            Known2 = [node() | S1#multi.known],
            ?trace({the_locker, spontaneous, {known2, Known2},
                    {node,Node}, {is_lock_set,IsLockSet}}),
            case IsLockSet of
                true ->
                    gen_server:cast(global_name_server, 
                                    {lock_is_set, Node, MyTag, LockId}),
                    ?trace({lock_sync_done, {pid,Pid}, 
                            {node,node(Pid)}, {me,self()}}),
                    %% Wait for global to tell us to remove lock.
                    %% Should the other locker's node die,
                    %% global_name_server will receive nodedown, and
                    %% then send {cancel, Node, Tag}.
                    receive
                        {cancel, Node, _Tag, Fun} ->
                            ?trace({cancel_the_lock,{node,Node}}),
                            call_fun(Fun),
                            delete_global_lock(LockId, Known2)
                    end,
                    S2 = S1#multi{just_synced = true},
                    loop_the_locker(remove_node(Node, S2));
                false ->
                    loop_the_locker(S1#multi{just_synced = false})
            end;
        false ->
            ?trace({the_locker, not_there, {node,Node}}),
            Pid ! {lock_set, self(), false, S#multi.known},
            loop_the_locker(S)
    end;
the_locker_message({add_to_known, Nodes}, S) ->
    S1 = update_locker_known({add, Nodes}, S),
    loop_the_locker(S1);
the_locker_message({remove_from_known, Node}, S) ->
    S1 = update_locker_known({remove, Node}, S),
    loop_the_locker(S1);
the_locker_message({do_trace, DoTrace}, S) ->
    loop_the_locker(S#multi{do_trace = DoTrace});
the_locker_message(Other, S) ->
    unexpected_message(Other, locker),
    ?trace({the_locker, {other_msg, Other}}),
    loop_the_locker(S).

%% Requests from nodes on the local host are chosen before requests
%% from other nodes. This should be a safe optimization.
select_node(S) ->
    UseRemote = S#multi.local =:= [],
    Others1 = if UseRemote -> S#multi.remote; true -> S#multi.local end,
    Others2 = exclude_known(Others1, S#multi.known),
    S1 = if 
             UseRemote -> S#multi{remote = Others2}; 
             true -> S#multi{local = Others2} 
         end,
    if 
        Others2 =:= [] ->
            loop_the_locker(S1);
        true -> 
            Him = random_element(Others2),
            #him{locker = HisTheLocker, vsn = HisVsn,
                 node = Node, my_tag = MyTag} = Him,
            HisNode = [Node],
            Us = [node() | HisNode],
            LockId = locker_lock_id(HisTheLocker, HisVsn),
            ?trace({select_node, self(), {us, Us}}),
            %% HisNode = [Node] prevents deadlock:
            {IsLockSet, S2} = lock_nodes_safely(LockId, HisNode, S1),
            case IsLockSet of
                true -> 
                    Known1 = Us ++ S2#multi.known,
                    ?trace({sending_lock_set, self(), {his,HisTheLocker}}),
                    HisTheLocker ! {lock_set, self(), true, S2#multi.known},
                    S3 = lock_is_set(S2, Him, MyTag, Known1, LockId),
                    loop_the_locker(S3);
                false ->
                    loop_the_locker(S2)
            end
    end.

%% Version 5: Both sides use the same requester id. Thereby the nodes
%% common to both sides are locked by both locker processes. This
%% means that the lock is still there when the 'new_nodes' message is
%% received even if the other side has deleted the lock.
locker_lock_id(Pid, Vsn) when Vsn > 4 ->
    {?GLOBAL_RID, lists:sort([self(), Pid])}.

lock_nodes_safely(LockId, Extra, S0) ->
    %% Locking node() could stop some node that has already locked the
    %% boss, so just check if it is possible to lock node().
    First = delete_nonode([S0#multi.the_boss]),
    case ([node()] =:= First) orelse (can_set_lock(LockId) =/= false) of
        true ->
            %% Locking the boss first is an optimization.
            case set_lock(LockId, First, 0) of
                true ->
                    S = update_locker_known(S0),
                    %% The boss may have changed, but don't bother.
                    Second = delete_nonode([node() | Extra] -- First),
                    case set_lock(LockId, Second, 0) of
                        true ->
                            Known = S#multi.known,
                            case set_lock(LockId, Known -- First, 0) of
                                true ->
                                    _ = locker_trace(S, ok, {First, Known}),
                                    {true, S};
                                false ->
                                    %% Since the boss is locked we
                                    %% should have gotten the lock, at
                                    %% least if no one else is locking
                                    %% 'global'. Calling set_lock with
                                    %% Retries > 0 does not seem to
                                    %% speed things up.
                                    SoFar = First ++ Second,
                                    del_lock(LockId, SoFar),
                                    _ = locker_trace(S, not_ok, {Known,SoFar}),
                                    {false, S}
                            end;
                        false ->
                            del_lock(LockId, First),
                            _ = locker_trace(S, not_ok, {Second, First}),
                            {false, S}
                    end;
                false ->
                    _ = locker_trace(S0, not_ok, {First, []}),
                    {false, S0}
            end;
        false ->
            {false, S0}
    end.

delete_nonode(L) ->
    lists:delete(nonode@nohost, L).

%% Let the server add timestamp.
locker_trace(#multi{do_trace = false}, _, _Nodes) ->
    ok;
locker_trace(#multi{do_trace = true}, ok, Ns) ->
    global_name_server ! {trace_message, {locker_succeeded, node()}, Ns};
locker_trace(#multi{do_trace = true}, not_ok, Ns) ->
    global_name_server ! {trace_message, {locker_failed, node()}, Ns};
locker_trace(#multi{do_trace = true}, rejected, Ns) ->
    global_name_server ! {trace_message, {lock_rejected, node()}, Ns}.

update_locker_known(S) ->
    receive
        {add_to_known, Nodes} ->
            S1 = update_locker_known({add, Nodes}, S),
            update_locker_known(S1);
        {remove_from_known, Node} ->
            S1 = update_locker_known({remove, Node}, S),
            update_locker_known(S1)
    after 0 ->
            S
    end.

update_locker_known(Upd, S) ->
    Known = case Upd of
                {add, Nodes} -> Nodes ++ S#multi.known;
                {remove, Node} -> lists:delete(Node, S#multi.known)
            end,
    TheBoss = the_boss([node() | Known]), 
    S#multi{known = Known, the_boss = TheBoss}.

random_element(L) ->
    E = abs(erlang:monotonic_time()
		bxor erlang:unique_integer()) rem length(L),
    lists:nth(E+1, L).

exclude_known(Others, Known) ->
    [N || N <- Others, not lists:member(N#him.node, Known)].

lock_is_set(S, Him, MyTag, Known1, LockId) ->
    Node = Him#him.node,
    receive
	{lock_set, P, true, _} when node(P) =:= Node ->
	    gen_server:cast(global_name_server, 
                            {lock_is_set, Node, MyTag, LockId}),
	    ?trace({lock_sync_done, {p,P, node(P)}, {me,self()}}),

	    %% Wait for global to tell us to remove lock. Should the
            %% other locker's node die, global_name_server will
            %% receive nodedown, and then send {cancel, Node, Tag, Fun}.
	    receive
		{cancel, Node, _, Fun} ->
                    ?trace({lock_set_loop, {known1,Known1}}),
                    call_fun(Fun),
		    delete_global_lock(LockId, Known1)
	    end,
            S#multi{just_synced = true,
                    local = lists:delete(Him, S#multi.local),
                    remote = lists:delete(Him, S#multi.remote)};
	{lock_set, P, false, _} when node(P) =:= Node ->
            ?trace({not_both_set, {node,Node},{p, P},{known1,Known1}}),
            _ = locker_trace(S, rejected, Known1),
	    delete_global_lock(LockId, Known1),
	    S;
	{cancel, Node, _, Fun} ->
	    ?trace({the_locker, cancel2, {node,Node}}),
            call_fun(Fun),
            _ = locker_trace(S, rejected, Known1),
	    delete_global_lock(LockId, Known1),
            remove_node(Node, S);
	{'EXIT', _, _} ->
	    ?trace({the_locker, exit, {node,Node}}),
            _ = locker_trace(S, rejected, Known1),
	    delete_global_lock(LockId, Known1),
	    S
        %% There used to be an 'after' clause (OTP-4902), but it is 
        %% no longer needed:
        %% OTP-5770. Version 5 of the protocol. Deadlock can no longer
        %% occur due to the fact that if a partition is locked, one
        %% node in the other partition is also locked with the same
        %% lock-id, which makes it impossible for any node in the
        %% other partition to lock its partition unless it negotiates
        %% with the first partition.
    end.

%% The locker does the {new_nodes, ...} call before removing the lock.
call_fun(no_fun) ->
    ok;
call_fun(Fun) ->
    Fun().

%% The lock on the boss is removed last. The purpose is to reduce the
%% risk of failing to lock the known nodes after having locked the
%% boss. (Assumes the boss occurs only once.)
delete_global_lock(LockId, Nodes) ->
    TheBoss = the_boss(Nodes),
    del_lock(LockId, lists:delete(TheBoss, Nodes)),
    del_lock(LockId, [TheBoss]).

the_boss(Nodes) ->
    lists:max(Nodes).

find_node_tag(Node, S) ->
    case find_node_tag2(Node, S#multi.local) of
        false -> 
            find_node_tag2(Node, S#multi.remote);
        Reply ->
            Reply
    end.

find_node_tag2(_Node, []) ->
    false;
find_node_tag2(Node, [#him{node = Node, my_tag = MyTag, vsn = HisVsn} | _]) ->
    {true, MyTag, HisVsn};
find_node_tag2(Node, [_E | Rest]) ->
    find_node_tag2(Node, Rest).

remove_node(Node, S) ->
    S#multi{local = remove_node2(Node, S#multi.local),
            remote = remove_node2(Node, S#multi.remote)}.

remove_node2(_Node, []) ->
    [];
remove_node2(Node, [#him{node = Node} | Rest]) ->
    Rest;
remove_node2(Node, [E | Rest]) ->
    [E | remove_node2(Node, Rest)].

add_node(Him, S) ->
    case is_node_local(Him#him.node) of
        true ->
            S#multi{local = [Him | S#multi.local]};
        false ->
            S#multi{remote = [Him | S#multi.remote]}
    end.

is_node_local(Node) ->
    {ok, Host} = inet:gethostname(),
    case catch split_node(atom_to_list(Node), $@, []) of
	[_, Host] -> 
            true;
	_ ->
            false
    end.

split_node([Chr|T], Chr, Ack) -> [lists:reverse(Ack)|split_node(T, Chr, [])];
split_node([H|T], Chr, Ack)   -> split_node(T, Chr, [H|Ack]);
split_node([], _, Ack)        -> [lists:reverse(Ack)].

cancel_locker(Node, S, Tag) ->
    cancel_locker(Node, S, Tag, no_fun).

cancel_locker(Node, S, Tag, ToBeRunOnLockerF) ->
    S#state.the_locker ! {cancel, Node, Tag, ToBeRunOnLockerF},
    Resolvers = S#state.resolvers,
    ?trace({cancel_locker, {node,Node},{tag,Tag},
            {sync_tag_my, get({sync_tag_my, Node})},{resolvers,Resolvers}}),
    case lists:keyfind(Node, 1, Resolvers) of
	{_, Tag, Resolver} ->
            ?trace({{resolver, Resolver}}),
            exit(Resolver, kill),
            S1 = trace_message(S, {kill_resolver, Node}, [Tag, Resolver]),
	    S1#state{resolvers = lists:keydelete(Node, 1, Resolvers)};
	_ ->
	    S
    end.

reset_node_state(Node) ->
    ?trace({{node,Node}, reset_node_state, get()}),
    erase({wait_lock, Node}),
    erase({save_ops, Node}),
    erase({pre_connect, Node}),
    erase({prot_vsn, Node}),
    erase({sync_tag_my, Node}),
    erase({sync_tag_his, Node}),
    erase({lock_id, Node}).

%% Some node sent us his names. When a name clash is found, the resolve
%% function is called from the smaller node => all resolve funcs are called
%% from the same partition.
exchange_names([{Name, Pid, Method} | Tail], Node, Ops, Res) ->
    case ets:lookup(global_names, Name) of
	[{Name, Pid, _Method, _RPid2, _Ref2}] ->
	    exchange_names(Tail, Node, Ops, Res);
	[{Name, Pid2, Method2, _RPid2, _Ref2}] when node() < Node ->
	    %% Name clash!  Add the result of resolving to Res(olved).
	    %% We know that node(Pid) =/= node(), so we don't
	    %% need to link/unlink to Pid.
	    Node2 = node(Pid2), %% Node2 is connected to node().
	    case rpc:call(Node2, ?MODULE, resolve_it,
			  [Method2, Name, Pid, Pid2]) of
		Pid ->
		    Op = {insert, {Name, Pid, Method}},
		    exchange_names(Tail, Node, [Op | Ops], Res);
		Pid2 ->
		    Op = {insert, {Name, Pid2, Method2}},
		    exchange_names(Tail, Node, Ops, [Op | Res]);
		none ->
		    Op = {delete, Name},
		    exchange_names(Tail, Node, [Op | Ops], [Op | Res]);
		{badrpc, Badrpc} ->
		    error_logger:info_msg("global: badrpc ~w received when "
					  "conflicting name ~tw was found\n",
					  [Badrpc, Name]),
		    Op = {insert, {Name, Pid, Method}},
		    exchange_names(Tail, Node, [Op | Ops], Res);
		Else ->
		    error_logger:info_msg("global: Resolve method ~w for "
					  "conflicting name ~tw returned ~tw\n",
					  [Method, Name, Else]),
		    Op = {delete, Name},
		    exchange_names(Tail, Node, [Op | Ops], [Op | Res])
	    end;
	[{Name, _Pid2, _Method, _RPid, _Ref}] ->
	    %% The other node will solve the conflict.
	    exchange_names(Tail, Node, Ops, Res);
	_ ->
	    %% Entirely new name.
	    exchange_names(Tail, Node,
			   [{insert, {Name, Pid, Method}} | Ops], Res)
    end;
exchange_names([], _, Ops, Res) ->
    ?trace({exchange_names_finish,{ops,Ops},{res,Res}}),
    {Ops, Res}.

resolve_it(Method, Name, Pid1, Pid2) ->
    catch Method(Name, Pid1, Pid2).

minmax(P1,P2) ->
    if node(P1) < node(P2) -> {P1, P2}; true -> {P2, P1} end.

-spec random_exit_name(Name, Pid1, Pid2) -> pid() when
      Name :: term(),
      Pid1 :: pid(),
      Pid2 :: pid().
random_exit_name(Name, Pid, Pid2) ->
    {Min, Max} = minmax(Pid, Pid2),
    error_logger:info_msg("global: Name conflict terminating ~tw\n",
			  [{Name, Max}]),
    exit(Max, kill),
    Min.

-spec random_notify_name(Name, Pid1, Pid2) -> pid() when
      Name :: term(),
      Pid1 :: pid(),
      Pid2 :: pid().
random_notify_name(Name, Pid, Pid2) ->
    {Min, Max} = minmax(Pid, Pid2),
    Max ! {global_name_conflict, Name},
    Min.

-spec notify_all_name(Name, Pid1, Pid2) -> 'none' when
      Name :: term(),
      Pid1 :: pid(),
      Pid2 :: pid().
notify_all_name(Name, Pid, Pid2) ->
    Pid ! {global_name_conflict, Name, Pid2},
    Pid2 ! {global_name_conflict, Name, Pid},
    none.

dolink_ext(Pid, RegNode) when RegNode =:= node() -> 
    link(Pid);
dolink_ext(_, _) -> 
    ok.

dounlink_ext(Pid, RegNode) when RegNode =:= node() ->
    unlink_pid(Pid);
dounlink_ext(_Pid, _RegNode) ->
    ok.

unlink_pid(Pid) ->
    case ets:member(global_pid_names, Pid) of
	false ->
            case ets:member(global_pid_ids, Pid) of
                false -> 
		    unlink(Pid);
		true -> 
                    ok
	    end;
	true -> 
            ok
    end.

pid_is_locking(Pid, PidRefs) ->
    lists:keyfind(Pid, 1, PidRefs) =/= false.

delete_lock(Ref, S0) ->
    Locks = pid_locks(Ref),
    F = fun({ResourceId, LockRequesterId, PidRefs}, S) -> 
                {Pid, _RPid, Ref} = lists:keyfind(Ref, 3, PidRefs),
                remove_lock(ResourceId, LockRequesterId, Pid, PidRefs, true, S)
        end,
    lists:foldl(F, S0, Locks).

pid_locks(Ref) ->
    L = lists:flatmap(fun({_, ResourceId}) ->
                              ets:lookup(global_locks, ResourceId)
                      end, ets:lookup(global_pid_ids, Ref)),
    [Lock || Lock = {_Id, _Req, PidRefs} <- L, 
             rpid_is_locking(Ref, PidRefs)].

rpid_is_locking(Ref, PidRefs) ->
    lists:keyfind(Ref, 3, PidRefs) =/= false.

handle_nodedown(Node, S) ->
    %% DOWN signals from monitors have removed locks and registered names.
    #state{known = Known, synced = Syncs} = S,
    NewS = cancel_locker(Node, S, get({sync_tag_my, Node})),
    NewS#state.the_locker ! {remove_from_known, Node},
    reset_node_state(Node),
    NewS#state{known = lists:delete(Node, Known),
               synced = lists:delete(Node, Syncs)}.

get_names() ->
    ets:select(global_names, 
               ets:fun2ms(fun({Name, Pid, Method, _RPid, _Ref}) -> 
                                  {Name, Pid, Method} 
                          end)).

get_names_ext() ->
    ets:tab2list(global_names_ext).

get_known() ->
    gen_server:call(global_name_server, get_known, infinity).

random_sleep(Times) ->
    _ = case Times rem 10 of
	    0 ->
		_ = rand:seed(exsplus);
	    _ ->
		ok
	end,
    %% First time 1/4 seconds, then doubling each time up to 8 seconds max.
    Tmax = if Times > 5 -> 8000;
	      true -> ((1 bsl Times) * 1000) div 8
	   end,
    T = rand:uniform(Tmax),
    ?trace({random_sleep, {me,self()}, {times,Times}, {t,T}, {tmax,Tmax}}),
    receive after T -> ok end.

dec(infinity) -> infinity;
dec(N) -> N - 1.

send_again(Msg) ->
    Me = self(),
    spawn(fun() -> timer(Me, Msg) end).

timer(Pid, Msg) ->
    random_sleep(5),
    Pid ! Msg.

change_our_node_name(NewNode, S) ->
    S1 = trace_message(S, {new_node_name, NewNode}, []),
    S1#state{node_name = NewNode}.

trace_message(#state{trace = no_trace}=S, _M, _X) ->
    S;
trace_message(S, M, X) ->
    S#state{trace = [trace_message(M, X) | S#state.trace]}.

trace_message(M, X) ->
    {node(), erlang:timestamp(), M, nodes(), X}.

%%-----------------------------------------------------------------
%% Each sync process corresponds to one call to sync. Each such
%% process asks the global_name_server on all Nodes if it is in sync
%% with Nodes. If not, that (other) node spawns a syncer process that
%% waits for global to get in sync with all Nodes. When it is in
%% sync, the syncer process tells the original sync process about it.
%%-----------------------------------------------------------------
start_sync(Nodes, From) ->
    spawn_link(fun() -> sync_init(Nodes, From) end).

sync_init(Nodes, From) ->
    lists:foreach(fun(Node) -> monitor_node(Node, true) end, Nodes),
    sync_loop(Nodes, From).

sync_loop([], From) ->
    gen_server:reply(From, ok);
sync_loop(Nodes, From) ->
    receive
	{nodedown, Node} ->
	    monitor_node(Node, false),
	    sync_loop(lists:delete(Node, Nodes), From);
	{synced, SNodes} ->
	    lists:foreach(fun(N) -> monitor_node(N, false) end, SNodes),
	    sync_loop(Nodes -- SNodes, From)
    end.

%%%=======================================================================
%%% Get the current global_groups definition
%%%=======================================================================
check_sync_nodes() ->
    case get_own_nodes() of
	{ok, all} ->
	    nodes();
	{ok, NodesNG} ->
	    %% global_groups parameter is defined, we are not allowed to sync
	    %% with nodes not in our own global group.
	    intersection(nodes(), NodesNG);
	{error, _} = Error ->
	    Error
    end.

check_sync_nodes(SyncNodes) ->
    case get_own_nodes() of
	{ok, all} ->
	    SyncNodes;
	{ok, NodesNG} ->
	    %% global_groups parameter is defined, we are not allowed to sync
	    %% with nodes not in our own global group.
	    OwnNodeGroup = intersection(nodes(), NodesNG),
	    IllegalSyncNodes = (SyncNodes -- [node() | OwnNodeGroup]),
	    case IllegalSyncNodes of
		[] -> SyncNodes;
		_ -> {error, {"Trying to sync nodes not defined in "
                              "the own global group", IllegalSyncNodes}}
	    end;
	{error, _} = Error ->
	    Error
    end.

get_own_nodes() ->
    case global_group:get_own_nodes_with_errors() of
        {error, Error} ->
            {error, {"global_groups definition error", Error}};
        OkTup ->
            OkTup
    end.

%% The registrar is a helper process that registers and unregisters
%% names. Since it never dies it assures that names are registered and
%% unregistered on all known nodes. It is started by and linked to
%% global_name_server.

start_the_registrar() ->
    spawn_link(fun() -> loop_the_registrar() end).

loop_the_registrar() ->
    receive 
        {trans_all_known, Fun, From} ->
            ?trace({loop_the_registrar, self(), Fun, From}),
            gen_server:reply(From, trans_all_known(Fun));
	Other ->
            unexpected_message(Other, register)
    end,
    loop_the_registrar().

unexpected_message({'EXIT', _Pid, _Reason}, _What) ->
    %% global_name_server died
    ok;
unexpected_message(Message, What) -> 
    error_logger:warning_msg("The global_name_server ~w process "
                             "received an unexpected message:\n~tp\n",
                             [What, Message]).

%%% Utilities

%% When/if erlang:monitor() returns before trying to connect to the
%% other node this function can be removed.
do_monitor(Pid) ->
    case (node(Pid) =:= node()) orelse lists:member(node(Pid), nodes()) of
        true ->
            %% Assume the node is still up
            {Pid, erlang:monitor(process, Pid)};
        false ->
            F = fun() -> 
                        Ref = erlang:monitor(process, Pid),
                        receive 
                            {'DOWN', Ref, process, Pid, _Info} ->
                                exit(normal)
                        end
                end,
            erlang:spawn_monitor(F)
    end.

intersection(_, []) -> 
    [];
intersection(L1, L2) ->
    L1 -- (L1 -- L2).

%% Support legacy tuple funs as resolve functions.
allow_tuple_fun({M, F}) when is_atom(M), is_atom(F) ->
    fun M:F/3;
allow_tuple_fun(Fun) when is_function(Fun, 3) ->
    Fun.
