%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
-moduledoc """
A global name registration facility.

This module consists of the following services:

- Registration of global names
- Global locks
- Maintenance of the fully connected network

[](){: #prevent_overlapping_partitions }

As of OTP 25, `global` will by default prevent overlapping partitions due to
network issues by actively disconnecting from nodes that reports that they have
lost connections to other nodes. This will cause fully connected partitions to
form instead of leaving the network in a state with overlapping partitions.

> #### Warning {: .warning }
>
> Prevention of overlapping partitions can be disabled using the
> [`prevent_overlapping_partitions`](kernel_app.md#prevent_overlapping_partitions)
> `kernel(6)` parameter, making `global` behave like it used to do. This is,
> however, problematic for all applications expecting a fully connected network
> to be provided, such as for example `mnesia`, but also for `global` itself. A
> network of overlapping partitions might cause the internal state of `global`
> to become inconsistent. Such an inconsistency can remain even after such
> partitions have been brought together to form a fully connected network again.
> The effect on other applications that expects that a fully connected network
> is maintained may vary, but they might misbehave in very subtle hard to detect
> ways during such a partitioning. Since you might get hard to detect issues
> without this fix, you are _strongly_ advised _not_ to disable this fix. Also
> note that this fix _has_ to be enabled on _all_ nodes in the network in order
> to work properly.

> #### Note {: .info }
>
> None of the above services will be reliably delivered unless both of the
> kernel parameters [`connect_all`](kernel_app.md#connect_all) and
> [`prevent_overlapping_partitions`](kernel_app.md#prevent_overlapping_partitions)
> are enabled. Calls to the `global` API will, however, _not_ fail even though
> one or both of them are disabled. You will just get unreliable results.

These services are controlled through the process `global_name_server` that
exists on every node. The global name server starts automatically when a node is
started. With the term _global_ is meant over a system consisting of many Erlang
nodes.

The ability to globally register names is a central concept in the programming
of distributed Erlang systems. In this module, the equivalent of the
[`register/2`](`register/2`) and [`whereis/1`](`whereis/1`) BIFs (for local name
registration) are provided, but for a network of Erlang nodes. A registered name
is an alias for a process identifier (pid). The global name server monitors
globally registered pids. If a process terminates, the name is also globally
unregistered.

The registered names are stored in replica global name tables on every node.
There is no central storage point. Thus, the translation of a name to a pid is
fast, as it is always done locally. For any action resulting in a change to the
global name table, all tables on other nodes are automatically updated.

Global locks have lock identities and are set on a specific resource. For
example, the specified resource can be a pid. When a global lock is set, access
to the locked resource is denied for all resources other than the lock
requester.

Both the registration and lock services are atomic. All nodes involved in these
actions have the same view of the information.

The global name server also performs the critical task of continuously
monitoring changes in node configuration. If a node that runs a globally
registered process goes down, the name is globally unregistered. To this end,
the global name server subscribes to `nodeup` and `nodedown` messages sent from
module `net_kernel`. Relevant Kernel application variables in this context are
`net_setuptime`, `net_ticktime`, and `dist_auto_connect`. See also
[`kernel(6)`](kernel_app.md#net_setuptime).

The name server also maintains a fully connected network. For example, if node
`N1` connects to node `N2` (which is already connected to `N3`), the global name
servers on the nodes `N1` and `N3` ensure that also `N1` and `N3` are connected.
In this case, the name registration service cannot be used, but the lock
mechanism still works.

If the global name server fails to connect nodes (`N1` and `N3` in the example),
a warning event is sent to the error logger. The presence of such an event does
not exclude the nodes to connect later (you can, for example, try command
`rpc:call(N1, net_adm, ping, [N2])` in the Erlang shell), but it indicates a
network problem.

> #### Note {: .info }
>
> If the fully connected network is not set up properly, try first to increase
> the value of `net_setuptime`.

## See Also

`m:global_group`, `m:net_kernel`
""".
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
	 unregister_name/1, registered_names/0, send/2,
	 set_lock/1, set_lock/2, set_lock/3,
	 del_lock/1, del_lock/2,
	 trans/2, trans/3, trans/4,
	 random_exit_name/3, random_notify_name/3, notify_all_name/3,
         disconnect/0]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3, resolve_it/4, get_locker/0]).

-export([info/0]).

-include_lib("stdlib/include/ms_transform.hrl").

%% Set this variable to 'allow' to allow several names of a process.
%% This is for backward compatibility only; the functionality is broken.
-define(WARN_DUPLICATED_NAME, global_multi_name_action).

%% Undocumented Kernel variable.
-define(N_CONNECT_RETRIES, global_connect_retries).
-define(DEFAULT_N_CONNECT_RETRIES, 0).

%% Time that we keep information about multicasted lost_connection
%% messages...
-define(lost_conn_info_cleanup_time, 60*60*1000).

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

-define(MAX_64BIT_SMALL_INT, ((1 bsl 59) - 1)).
-define(MIN_64BIT_SMALL_INT, (-(1 bsl 59))).

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
%% Vsn 6 does not send any message between locker processes on different
%%       nodes, but uses the server as a proxy.
%% Vsn 7 - propagate global versions between nodes, so we always know
%%         versions of known nodes
%%       - optional "prevent overlapping partitions" fix supported
%% Vsn 8 - "verify connection" part of the protocol preventing
%%         deadlocks in connection setup due to locker processes
%%         being out of sync
%%       - "prevent overlapping partitions" fix also for systems
%%         configured to use global groups

%% Current version of global does not support vsn 4 or earlier.

-define(vsn, 8).

%% Version when the "verify connection" part of the protocol
%% was introduced.
-define(verify_connect_vsn, 8).
%% Version where "prevent overlapping partitions" fix for global groups
%% was introduced.
-define(gg_pop_vsn, 8).
%% Version when the "prevent overlapping partitions" fix was introduced.
-define(pop_vsn, 7).
%% Version when the "propagate global protocol versions" feature
%% was introduced.
-define(pgpv_vsn, 7).

%%-----------------------------------------------------------------
%% connect_all = boolean() - true if we are supposed to set up a
%%                           fully connected net
%% known       = #{} - Map of known nodes including protocol version
%%                     as well as some other information. See state
%%                     record declaration below for more info.
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
-record(conf, {connect_all        :: boolean(),
               prevent_over_part  :: boolean()
              }).
                
-record(state, {conf = #conf{}     :: #conf{},
		known = #{}        :: #{
                           %% Known connected node with protocol
                           %% version as value
                           node() => non_neg_integer(),
                           %% Connecting node, not yet known, with
                           %% protocol version as value
                           {pending, node()} => non_neg_integer(),
                           %% Node currently being removed
                           {removing, node()} => yes,
                           %% Connection id of connected nodes
                           {connection_id, node()} => integer()
                          },
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
%%% global_locks (set): {ResourceId, LockRequesterId, [{Pid,ref()]}
%%%   Pid is locking ResourceId, ref() is the monitor ref.
%%% global_names (set):  {Name, Pid, Method, ref()}
%%%   Registered names. ref() is the monitor ref.
%%% global_names_ext (set): {Name, Pid, RegNode}
%%%   External registered names (C-nodes).
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
%%% global_lost_connections (set):
%%%   {{NodeA, NodeB}, {ExtendedCreationA, OpIdA, Timer}
%%%   Information about lost connections (reported by NodeA) used by
%%%   the "prevent overlapping partitions" fix. The timer is used is
%%%   used to remove the entry when not needed anymore.
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
%%%
%%% Erlang/OTP 22.0: The extra process calling erlang:monitor() is removed.

-doc false.
start() -> 
    gen_server:start({local, global_name_server}, ?MODULE, [], []).

-doc false.
start_link() -> 
    gen_server:start_link({local, global_name_server}, ?MODULE, [], []).

-doc false.
stop() -> 
    gen_server:call(global_name_server, stop, infinity).

-doc """
Synchronizes the global name server with all nodes known to this node. These are
the nodes that are returned from `erlang:nodes()`. When this function returns,
the global name server receives global information from all nodes. This function
can be called when new nodes are added to the network.

The only possible error reason `Reason` is
`{"global_groups definition error", Error}`.
""".
-spec sync() -> 'ok' | {'error', Reason :: term()}.
sync() ->
    case check_sync_nodes() of
	{error, _} = Error ->
	    Error;
	SyncNodes ->
	    gen_server:call(global_name_server, {sync, SyncNodes}, infinity)
    end.

-doc false.
-spec sync([node()]) -> 'ok' | {'error', Reason :: term()}.
sync(Nodes) ->
    case check_sync_nodes(Nodes) of
	{error, _} = Error ->
	    Error;
	SyncNodes ->
	    gen_server:call(global_name_server, {sync, SyncNodes}, infinity)
    end.

-doc """
Sends message `Msg` to the pid globally registered as `Name`.

If `Name` is not a globally registered name, the calling function exits with
reason `{badarg, {Name, Msg}}`.
""".
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
-doc """
Returns the pid with the globally registered name `Name`. Returns `undefined` if
the name is not globally registered.
""".
-spec whereis_name(Name) -> pid() | 'undefined' when
      Name :: term().
whereis_name(Name) ->
    where(Name).

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
-doc(#{equiv => register_name/3}).
-spec register_name(Name, Pid) -> 'yes' | 'no' when
      Name :: term(),
      Pid :: pid().
register_name(Name, Pid) when is_pid(Pid) ->
    register_name(Name, Pid, fun random_exit_name/3).

-type method() :: fun((Name :: term(), Pid :: pid(), Pid2 :: pid()) ->
                             pid() | 'none').

-doc """
Globally associates name `Name` with a pid, that is, globally notifies all nodes
of a new global name in a network of Erlang nodes.

When new nodes are added to the network, they are informed of the globally
registered names that already exist. The network is also informed of any global
names in newly connected nodes. If any name clashes are discovered, function
`Resolve` is called. Its purpose is to decide which pid is correct. If the
function crashes, or returns anything other than one of the pids, the name is
unregistered. This function is called once for each name clash.

> #### Warning {: .warning }
>
> If you plan to change code without restarting your system, you must use an
> external fun (`fun Module:Function/Arity`) as function `Resolve`. If you use a
> local fun, you can never replace the code for the module that the fun belongs
> to.

Three predefined resolve functions exist:
[`random_exit_name/3`](`random_exit_name/3`),
[`random_notify_name/3`](`random_notify_name/3`), and
[`notify_all_name/3`](`notify_all_name/3`). If no `Resolve` function is defined,
`random_exit_name` is used. This means that one of the two registered processes
is selected as correct while the other is killed.

This function is completely synchronous, that is, when this function returns,
the name is either registered on all nodes or none.

The function returns `yes` if successful, `no` if it fails. For example, `no` is
returned if an attempt is made to register an already registered process or to
register a process with a name that is already in use.

> #### Note {: .info }
>
> Releases up to and including Erlang/OTP R10 did not check if the process was
> already registered. The global name table could therefore become inconsistent.
> The old (buggy) behavior can be chosen by giving the Kernel application
> variable `global_multi_name_action` the value `allow`.

If a process with a registered name dies, or the node goes down, the name is
unregistered on all nodes.
""".
-spec register_name(Name, Pid, Resolve) -> 'yes' | 'no' when
      Name :: term(),
      Pid :: pid(),
      Resolve :: method().
register_name(Name, Pid, Method0) when is_pid(Pid) ->
    Method = allow_tuple_fun(Method0),
    Fun = fun(Nodes) ->
        case (where(Name) =:= undefined) andalso check_dupname(Name, Pid) of
            true ->
                _ = gen_server:multi_call(Nodes,
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
                    logger:log(error, S, [Pid, Names]),
                    false
            end
    end.

-doc "Removes the globally registered name `Name` from the network of Erlang nodes.".
-spec unregister_name(Name) -> _ when
      Name :: term().
unregister_name(Name) ->
    case where(Name) of
	undefined ->
	    ok;
	_ ->
	    Fun = fun(Nodes) ->
			  _ = gen_server:multi_call(Nodes,
                                                    global_name_server,
                                                    {unregister, Name}),
			  ok
		  end,
            ?trace({unregister_name, self(), Name}),
            gen_server:call(global_name_server, {registrar, Fun}, infinity)
    end.

-doc(#{equiv => re_register_name/3}).
-spec re_register_name(Name, Pid) -> 'yes' when
      Name :: term(),
      Pid :: pid().
re_register_name(Name, Pid) when is_pid(Pid) ->
    re_register_name(Name, Pid, fun random_exit_name/3).

-doc """
Atomically changes the registered name `Name` on all nodes to refer to `Pid`.

Function `Resolve` has the same behavior as in
[`register_name/2,3`](`register_name/2`).
""".
-spec re_register_name(Name, Pid, Resolve) -> 'yes' when
      Name :: term(),
      Pid :: pid(),
      Resolve :: method().
re_register_name(Name, Pid, Method0) when is_pid(Pid) ->
    Method = allow_tuple_fun(Method0),
    Fun = fun(Nodes) ->
		  _ = gen_server:multi_call(Nodes,
                                            global_name_server,
                                            {register, Name, Pid, Method}),
		  yes
	  end,
    ?trace({re_register_name, self(), Name, Pid, Method}),
    gen_server:call(global_name_server, {registrar, Fun}, infinity).

-doc "Returns a list of all globally registered names.".
-spec registered_names() -> [Name] when
      Name :: term().
registered_names() ->
    MS = ets:fun2ms(fun({Name,_Pid,_M,_R}) -> Name end),
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
-doc false.
register_name_external(Name, Pid) when is_pid(Pid) ->
    register_name_external(Name, Pid, fun random_exit_name/3).

-doc false.
register_name_external(Name, Pid, Method) when is_pid(Pid) ->
    Fun = fun(Nodes) ->
		  case where(Name) of
		      undefined ->
			  _ = gen_server:multi_call(Nodes,
                                                    global_name_server,
                                                    {register_ext, Name, Pid,
                                                     Method, node()}),
			  yes;
		      _Pid -> no
		  end
	  end,
    ?trace({register_name_external, self(), Name, Pid, Method}),
    gen_server:call(global_name_server, {registrar, Fun}, infinity).

-doc false.
unregister_name_external(Name) ->
    unregister_name(Name).

-type id() :: {ResourceId :: term(), LockRequesterId :: term()}.

-doc(#{equiv => set_lock/3}).
-spec set_lock(Id) -> boolean() when
      Id :: id().
set_lock(Id) ->
    set_lock(Id, [node() | nodes()], infinity, 1).

-type retries() :: non_neg_integer() | 'infinity'.

-doc(#{equiv => set_lock/3}).
-spec set_lock(Id, Nodes) -> boolean() when
      Id :: id(),
      Nodes :: [node()].
set_lock(Id, Nodes) ->
    set_lock(Id, Nodes, infinity, 1).

-doc """
Sets a lock on the specified nodes (or on all nodes if none are specified) on
`ResourceId` for `LockRequesterId`. If a lock already exists on `ResourceId` for
another requester than `LockRequesterId`, and `Retries` is not equal to `0`, the
process sleeps for a while and tries to execute the action later. When `Retries`
attempts have been made, `false` is returned, otherwise `true`. If `Retries` is
`infinity`, `true` is eventually returned (unless the lock is never released).

If no value for `Retries` is specified, `infinity` is used.

This function is completely synchronous.

If a process that holds a lock dies, or the node goes down, the locks held by
the process are deleted.

The global name server keeps track of all processes sharing the same lock, that
is, if two processes set the same lock, both processes must delete the lock.

This function does not address the problem of a deadlock. A deadlock can never
occur as long as processes only lock one resource at a time. A deadlock can
occur if some processes try to lock two or more resources. It is up to the
application to detect and rectify a deadlock.

> #### Note {: .info }
>
> Avoid the following values of `ResourceId`, otherwise Erlang/OTP does not work
> properly:
>
> - `dist_ac`
> - `global`
> - `mnesia_adjust_log_writes`
> - `mnesia_table_lock`
""".
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

-doc(#{equiv => del_lock/2}).
-spec del_lock(Id) -> 'true' when
      Id :: id().
del_lock(Id) ->
    del_lock(Id, [node() | nodes()]).

-doc "Deletes the lock `Id` synchronously.".
-spec del_lock(Id, Nodes) -> 'true' when
      Id :: id(),
      Nodes :: [node()].
del_lock({_ResourceId, _LockRequesterId} = Id, Nodes) ->
    ?trace({del_lock, {me,self()}, Id, {nodes,Nodes}}),
    _ = gen_server:multi_call(Nodes, global_name_server, {del_lock, Id}),
    true.

-type trans_fun() :: function() | {module(), atom()}.

-doc(#{equiv => trans/4}).
-spec trans(Id, Fun) -> Res | aborted when
      Id :: id(),
      Fun :: trans_fun(),
      Res :: term().
trans(Id, Fun) -> trans(Id, Fun, [node() | nodes()], infinity).

-doc(#{equiv => trans/4}).
-spec trans(Id, Fun, Nodes) -> Res | aborted when
      Id :: id(),
      Fun :: trans_fun(),
      Nodes :: [node()],
      Res :: term().
trans(Id, Fun, Nodes) -> trans(Id, Fun, Nodes, infinity).

-doc """
Sets a lock on `Id` (using `set_lock/3`). If this succeeds, `Fun()` is evaluated
and the result `Res` is returned. Returns `aborted` if the lock attempt fails.
If `Retries` is set to `infinity`, the transaction does not abort.

`infinity` is the default setting and is used if no value is specified for
`Retries`.
""".
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

-doc false.
info() ->
    gen_server:call(global_name_server, info, infinity).

-doc """
Disconnect from all other nodes known to `global`. A list of node names (in an
unspecified order) is returned which corresponds to the nodes that were
disconnected. All disconnect operations performed have completed when
`global:disconnect/0` returns.

The disconnects will be made in such a way that only the current node will be
removed from the cluster of `global` nodes. If
[`prevent_overlapping_partitions`](`m:global#prevent_overlapping_partitions`) is
enabled and you disconnect, from other nodes in the cluster of `global` nodes,
by other means, `global` on the other nodes may partition the remaining nodes in
order to ensure that no overlapping partitions appear. Even if
`prevent_overlapping_partitions` is disabled, you should preferably use
`global:disconnect/0` in order to remove current node from a cluster of `global`
nodes, since you otherwise likely _will_ create overlapping partitions which
might [cause problems](`m:global#prevent_overlapping_partitions`).

Note that if the node is going to be halted, there is _no_ need to remove it
from a cluster of `global` nodes explicitly by calling `global:disconnect/0`
before halting it. The removal from the cluster is taken care of automatically
when the node halts regardless of whether `prevent_overlapping_partitions` is
enabled or not.

If current node has been configured to be part of a
[_global group_](`m:global_group`), only connected and/or synchronized nodes in
that group are known to `global`, so `global:disconnect/0` will _only_
disconnect from those nodes. If current node is _not_ part of a _global group_,
all [connected visible nodes](`erlang:nodes/0`) will be known to `global`, so
`global:disconnect/0` will disconnect from all those nodes.

Note that information about connected nodes does not instantaneously reach
`global`, so the caller might see a node part of the result returned by
[`nodes()`](`erlang:nodes/0`) while it still is not known to `global`. The
disconnect operation will, however, still not cause any overlapping partitions
when `prevent_overlapping_partitions` is enabled. If
`prevent_overlapping_partitions` is disabled, overlapping partitions might form
in this case.

Note that when `prevent_overlapping_partitions` is enabled, you may see warning
reports on other nodes when they detect that current node has disconnected.
These are in this case completely harmless and can be ignored.
""".
-doc(#{since => <<"OTP 25.1">>}).
-spec disconnect() -> [node()].

disconnect() ->
    gen_server:call(global_name_server, disconnect, infinity).

%%%-----------------------------------------------------------------
%%% Call-back functions from gen_server
%%%-----------------------------------------------------------------

-doc false.
-spec init([]) -> {'ok', state()}.

init([]) ->
    _ = process_flag(async_dist, true),
    process_flag(trap_exit, true),

    %% Monitor all 'nodeup'/'nodedown' messages of visible nodes.
    %% In case
    %%
    %% * no global group is configured, we use these as is. This
    %%   way we know that 'nodeup' comes before any traffic from
    %%   the node on the newly established connection and 'nodedown'
    %%   comes after any traffic on this connection from the node.
    %%
    %% * global group is configured, we ignore 'nodeup' and instead
    %%   rely on 'group_nodeup' messages passed by global_group and
    %%   filter 'nodedown' based on if the node is part of our group
    %%   or not. We need to be prepared for traffic from the node
    %%   on the newly established connection arriving before the
    %%   'group_nodeup'. 'nodedown' will however not arrive until
    %%   all traffic from the node on this connection has arrived.
    %%
    %% In case a connection goes down and then up again, the
    %% 'nodedown' for the old connection is nowadays guaranteed to
    %% be delivered before the 'nodeup' for the new connection.
    %%
    %% By keeping track of connection_id for all connections we
    %% can differentiate between different instances of connections
    %% to the same node.
    ok = net_kernel:monitor_nodes(true, #{connection_id => true}),

    %% There are most likely no connected nodes at this stage,
    %% but check to make sure...
    Known = lists:foldl(fun ({N, #{connection_id := CId}}, Cs) ->
                                Cs#{{connection_id, N} => CId}
                        end,
                        #{},
                        nodes(visible, #{connection_id => true})),

    _ = ets:new(global_locks, [set, named_table, protected]),
    _ = ets:new(global_names, [set, named_table, protected,
                               {read_concurrency, true}]),
    _ = ets:new(global_names_ext, [set, named_table, protected]),

    _ = ets:new(global_pid_names, [bag, named_table, protected]),
    _ = ets:new(global_pid_ids, [bag, named_table, protected]),
    _ = ets:new(global_lost_connections, [set, named_table, protected]),
    _ = ets:new(global_node_resources, [set, named_table, protected]),

    %% This is for troubleshooting only.
    DoTrace = os:getenv("GLOBAL_HIGH_LEVEL_TRACE") =:= "TRUE",
    T0 = case DoTrace of
             true -> 
                 send_high_level_trace(),
                 [];
             false -> 
                 no_trace
         end,

    Ca = case application:get_env(kernel, connect_all) of
             {ok, CaBool} when is_boolean(CaBool) ->
                 CaBool;
             {ok, CaInvalid} ->
                 error({invalid_parameter_value, connect_all, CaInvalid});
             undefined ->
                 CaBool = case init:get_argument(connect_all) of
                              {ok, [["false" | _] | _]} ->
                                  false;
                              _ ->
                                  true
                          end,
                 ok = application:set_env(kernel, connect_all, CaBool,
                                          [{timeout, infinity}]),
                 CaBool
         end,

    POP = case application:get_env(kernel,
                                   prevent_overlapping_partitions) of
              {ok, PopBool} when is_boolean(PopBool) ->
                  PopBool;
              {ok, PopInvalid} ->
                  error({invalid_parameter_value,
                         prevent_overlapping_partitions,
                         PopInvalid});
              undefined ->
                  true
          end,

    S = #state{the_locker = start_the_locker(DoTrace),
               known = Known,
               trace = T0,
               the_registrar = start_the_registrar(),
               conf = #conf{connect_all = Ca,
                            prevent_over_part = POP}},
    _ = rand:seed(default,
                  (erlang:monotonic_time(nanosecond) rem 1000000000)
                  + (erlang:system_time(nanosecond) rem 1000000000)),
    CreX = ((rand:uniform(?MAX_64BIT_SMALL_INT - ?MIN_64BIT_SMALL_INT)
             - 1) band (bnot ((1 bsl 32) -1))),
    put(creation_extension, CreX),
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
%% - The lockers begin locking operations as soon as the init_connect
%%   messages has been exchanged and do not wait for init_connect_ack.
%%   They could even complete before init_connect_ack messages are
%%   received. The init_connect_ack messages are only there to confirm
%%   that both nodes has the same view of which connect session is
%%   ongoing. If lockers get out of sync, the lock will not be able
%%   to be aquired on both nodes. The out of sync lock operation will
%%   be detected when the init_connect_ack message is received and the
%%   operation can be cancelled and then restarted.
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
%% B ! {init_connect_ack, ...}
%% << {init_connect_ack, ...}
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
%% When cancelling a connect, also the remote node is nowadays also
%% informed using:
%% B ! {cancel_connect, ...}
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
%% 2. Between global_name_server and my locker (the local locker)
%%    {nodeup, Node, Tag}
%%    {his_the_locker, Pid, {Vsn, HisKnown}, MyKnown}
%%    {cancel, Node, Tag, Fun | no_fun}
%%    {add_to_known, Nodes}
%%    {remove_from_known, Nodes}
%%    {lock_set, Pid, LockIsSet, HisKnown} (acting as proxy)
%% 3. Between lockers on connecting nodes (via proxy global_name_server)
%%    {lock_set, Pid, LockIsSet, HisKnown}
%%      loop until both lockers have LockIsSet =:= true,
%%      then send to global_name_server {lock_is_set, Node, Tag, LockId}
%% 4. Connecting node's global_name_server informs other nodes in the same
%%    partition about hitherto unknown nodes in the other partition
%%    {new_nodes, Node, Ops, ListOfNamesExt, NewNodes, ExtraInfo}
%% 5. Between global_name_server and resolver
%%    {resolve, NameList, Node} to resolver
%%    {exchange_ops, Node, Tag, Ops, Resolved} from resolver
%% 6. sync protocol, between global_name_servers in different partitions
%%    {in_sync, Node, IsKnown}
%%          sent by each node to all new nodes (Node becomes known to them)
%%-----------------------------------------------------------------

%% ----------------------------------------------------------------
%% Prevent Overlapping Partitions Algorithm
%% ========================================
%%
%% 1. When a node lose connection to another node it sends a
%%    {lost_connection, LostConnNode, OtherNode} message to all
%%    other nodes that it knows of.
%% 2. When a lost_connection message is received the receiver
%%    first checks if it has seen this message before. If so, it
%%    just ignores it. If it has not seen it before, it sends the
%%    message to all nodes it knows of. This in order to ensure
%%    that all connected nodes will receive this message. It then
%%    sends a {remove_connection, LostConnRecvNode} message (where
%%    LostConnRecvNode is its own node name) to OtherNode and
%%    clear all information about OtherNode so OtherNode wont be
%%    part of ReceiverNode's cluster anymore. When this information
%%    has been cleared, no lost_connection will be triggered when
%%    a nodedown message for the connection to OtherNode is
%%    received.
%% 3. When a {remove_connection, LostConnRecvNode} message is
%%    received, the receiver node takes down the connection to
%%    LostConnRecvNode and clears its information about
%%    LostConnRecvNode so it is not part of its cluster anymore.
%%    Both nodes will receive a nodedown message due to the
%%    connection being closed, but none of them will send
%%    lost_connection messages since they have cleared information
%%    about the other node.
%%
%% This will take down more connections than the minimum amount
%% of connections to remove in order to form fully connected
%% partitions. For example, if the user takes down a connection
%% between two nodes, the rest of the nodes will disconnect from
%% both of these nodes instead of just one. This is due to:
%% * We do not want to partition a remaining network when a node
%%   has halted. When you receive a nodedown and/or lost_connection
%%   messages you don't know if the corresponding node has halted
%%   or if there are network issues.
%% * We need to decide which connection to take down as soon as
%%   we receive a lost_connection message in order to prevent
%%   inconsistencies entering global's state.
%% * All nodes need to make the same choices independent of
%%   each other.
%% 
%% ----------------------------------------------------------------

-doc false.
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
    {reply, mk_known_list(0, S), S};

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

handle_call(disconnect, _From, #state{known = Known} = S0) ->
    %% Disconnect from all nodes global knows of without
    %% sending any lost_connection messages...
    Nodes = maps:fold(fun ({connection_id, N}, _, Ns) when is_atom(N) ->
                              case global_group:participant(N) of
                                  false ->
                                      Ns;
                                  true ->
                                      ?trace({'####', disconnect, {node,N}}),
                                      net_kernel:async_disconnect(N),
                                      [N|Ns]
                              end;
                          (_, _, Ns) ->
                              Ns
                      end, [], Known),
    S1 = lists:foldl(fun (N, SAcc0) ->
                             receive {nodedown, N, I} -> ok end,
                             ?trace({'####', nodedown, {node,N,I}}),
                             SAcc1 = trace_message(SAcc0, {nodedown, N, I}, []),
                             SAcc2 = handle_nodedown(N, SAcc1, ignore_node),
                             NewKnown = maps:remove({connection_id, N},
                                                    SAcc2#state.known),
                             SAcc2#state{known = NewKnown}
                     end, S0, Nodes),
    {reply, Nodes, S1};

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
    logger:log(warning, "The global_name_server "
               "received an unexpected message:\n"
               "handle_call(~tp, ~tp, _)\n",
               [Request, From]),
    {noreply, S}.

%%========================================================================
%% init_connect
%%
%%========================================================================

-doc false.
-spec handle_cast(term(), state()) -> {'noreply', state()}.

handle_cast({init_connect, Vsn, Node, InitMsg}, S0) ->
    %% Sent from global_name_server at Node.
    ?trace({'####', init_connect, {vsn, Vsn}, {node,Node},{initmsg,InitMsg}}),
    S = case Vsn of
            %% It is always the responsibility of newer versions to understand
            %% older versions of the protocol.
            {HisVsn, HisTag} when HisVsn > ?vsn ->
                init_connect(?vsn, Node, InitMsg, HisTag, HisVsn, S0);
            {HisVsn, HisTag} ->
                init_connect(HisVsn, Node, InitMsg, HisTag, HisVsn, S0);
            %% To be future compatible
            Tuple when is_tuple(Tuple) ->
                List = tuple_to_list(Tuple),
                [HisVsn, HisTag | _] = List,
                %% use own version handling if his is newer.
                init_connect(?vsn, Node, InitMsg, HisTag, HisVsn, S0);
            _ ->
                Txt = io_lib:format("Illegal global protocol version ~p Node: ~p\n",
                                    [Vsn, Node]),
                logger:log(info, lists:flatten(Txt)),
                S0
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
            Known = mk_known_list(node_vsn(Node, S), S),
	    gen_server:cast({global_name_server, Node},
			    {resolved, node(), Resolved, Known,
			     unused,get_names_ext(),get({sync_tag_his,Node})}),
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
handle_cast({resolved, Node, HisResolved, HisKnown, _Unused, 
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
handle_cast({in_sync, Node, _IsKnown}, #state{known = Known,
                                              synced = Synced} = S0) ->
    %% Sent from global_name_server at Node (in the other partition).
    ?trace({'####', in_sync, {Node, _IsKnown}}),
    lists:foreach(fun(Pid) -> Pid ! {synced, [Node]} end, S0#state.syncers),
    NSynced = case lists:member(Node, Synced) of
		  true -> Synced;
		  false -> [Node | Synced]
	      end,
    S1 = S0#state{known = maps:remove({pending, Node}, Known),
                  synced = NSynced},
    %% {pending, Node} removed, i.e., we wont send a cancel_connect
    %% message to Node in cancel_locker()...
    S2 = cancel_locker(Node, S1, get({sync_tag_my, Node})),
    reset_node_state(Node),
    {noreply, S2};

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

handle_cast({lock_set, Pid, _Set, _HisKnown, MyTag} = Message, S) ->
    #state{the_locker = Locker} = S,
    Node = node(Pid),
    case get({sync_tag_my, Node}) of
        MyTag ->
            Locker ! Message,
            ok;
        _NewMyTag ->
            ok
    end,
    {noreply, S};
handle_cast({lock_set, _Pid, _Set, _HisKnown} = Message, S) ->
    #state{the_locker = Locker} = S,
    Locker ! Message,
    {noreply, S};

handle_cast(Request, S) ->
    logger:log(warning, "The global_name_server "
               "received an unexpected message:\n"
               "handle_cast(~tp, _)\n", [Request]),
    {noreply, S}.

%%========================================================================

-doc false.
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

handle_info({nodedown, Node, _Info}, S) when Node =:= S#state.node_name ->
    %% Somebody stopped the distribution dynamically - change
    %% references to old node name (Node) to new node name ('nonode@nohost')
    {noreply, change_our_node_name(node(), S)};

handle_info({nodedown, Node, Info}, S0) ->
    ?trace({'####', nodedown, {node,Node, Info}}),
    S1 = trace_message(S0, {nodedown, Node, Info}, []),
    NodeDownType = case global_group:participant(Node) of
                       true -> disconnected;
                       false -> ignore_node
                   end,
    S2 = handle_nodedown(Node, S1, NodeDownType),
    Known = maps:remove({connection_id, Node}, S2#state.known),
    {noreply, S2#state{known = Known}};

handle_info({extra_nodedown, Node}, S0) ->
    ?trace({'####', extra_nodedown, {node,Node}}),
    S1 = trace_message(S0, {extra_nodedown, Node}, []),
    NodeDownType = case global_group:participant(Node) of
                       true -> disconnected;
                       false -> ignore_node
                   end,
    %% Syncers wont notice this unless we send them
    %% a nodedown...
    lists:foreach(fun(Pid) -> Pid ! {nodedown, Node} end, S1#state.syncers),
    S2 = handle_nodedown(Node, S1, NodeDownType),
    Known = maps:remove({connection_id, Node}, S2#state.known),
    {noreply, S2#state{known = Known}};

handle_info({group_nodedown, Node, CId},
            #state{known = Known,
                   conf = #conf{connect_all = CA,
                                prevent_over_part = POP}} = S0) ->
    %% Node is either not part of our group or its global_group
    %% configuration is not in sync with our configuration...
    ?trace({'####', group_nodedown, {node,Node}}),
    S1 = trace_message(S0, {group_nodedown, Node, CId}, []),
    S = case maps:get({connection_id, Node}, Known, not_connected) of
            CId ->
                %% We cannot rely on 'DOWN' messages for locks and
                %% names on this node since the connection can remain
                %% up. Explicitly take care of them...
                S2 = delete_node_resources(Node, S1),

                %% Syncers wont notice this unless we send them
                %% a nodedown...
                lists:foreach(fun(Pid) -> Pid ! {nodedown, Node} end,
                              S2#state.syncers),
                NodeDownType = case (CA andalso POP
                                     andalso global_group:member(Node)) of
                                   false -> ignore_node;
                                   true -> disconnected
                               end,
                handle_nodedown(Node, S2, NodeDownType);

            _ ->
                %% Corresponding connection no longer exist; ignore it...
                S1
        end,
    {noreply, S};

handle_info({nodeup, Node, _Info}, S) when Node =:= node() ->
    ?trace({'####', local_nodeup, {node, Node}}),
    %% Somebody started the distribution dynamically - change
    %% references to old node name ('nonode@nohost') to Node.
    {noreply, change_our_node_name(Node, S)};

handle_info({nodeup, Node, #{connection_id := CId}},
            #state{known = Known,
                   conf = #conf{connect_all = false}} = S) ->
    {noreply, S#state{known = Known#{{connection_id, Node} => CId}}};

handle_info({nodeup, Node, #{connection_id := CId}},
            #state{known = Known} = S0) ->
    ?trace({'####', nodeup, {node,Node}}),
    S1 = S0#state{known = Known#{{connection_id, Node} => CId}},
    S2 = trace_message(S1, {nodeup, Node}, []),
    S3 = case global_group:group_configured() of
             false ->
                 handle_nodeup(Node, S2);
             true ->
                 %% We will later get a 'group_nodeup' message if
                 %% the node is in our group, and has a group
                 %% configuration that is in sync with our
                 %% configuration...
                 S2
         end,
    {noreply, S3};

handle_info({group_nodeup, Node, CId}, #state{known = Known} = S0) ->
    %% This message may arrive after other traffic from Node
    %% on the current connection...
    ?trace({'####', group_nodeup, {node,Node,CId}}),
    S1 = trace_message(S0, {group_nodeup, Node, CId}, []),
    S2 = case maps:get({connection_id, Node}, Known, not_connected) of
             CId -> handle_nodeup(Node, S1);
             _ -> S1 %% Late group_nodeup; connection has gone down...
         end,
    {noreply, S2};

handle_info({whereis, Name, From}, S) ->
    _ = do_whereis(Name, From),
    {noreply, S};

handle_info({lost_connection, NodeA, XCreationA, OpIdA, NodeB} = Msg,
            #state{conf = #conf{connect_all = true,
                                prevent_over_part = true}} = S0) ->
    %% Message introduced in protocol version ?pop_vsn
    %%
    %% NodeA reports that it lost connection to NodeB. If we got a
    %% connection to NodeB, we need to disconnect it in order to
    %% prevent overlapping partitions...
    LcKey = {NodeA, NodeB},
    S1 = case get_lost_connection_info(LcKey) of
             {XCreationA, OpId, _Tmr} when OpIdA =< OpId ->
                 %% We have already handled this lost connection
                 %% message...
                 S0;
             {_, _, Tmr} ->
                 %% Inform all other nodes we know of about this as well. This
                 %% in order to prevent that some nodes in this cluster wont
                 %% get the original message from NodeA. This ensures that all
                 %% nodes will know of this connection loss, even if some nodes
                 %% lose connection to NodeA while NodeA is multicasting that
                 %% it lost the connection to NodeB.
                 gns_volatile_multicast(Msg, NodeA, ?pop_vsn, true, S0),

                 %% Save info about this lost_connection message...
                 save_lost_connection_info(LcKey, XCreationA, OpIdA, Tmr),

                 RmNode = case node() == NodeB of
                              false ->
                                  NodeB;
                              true ->
                                  %% This toghether with NodeA being known by
                                  %% us probably is unusal, but can happen
                                  %% since lost_connection messages are
                                  %% reapeted by receiving nodes. All other
                                  %% nodes will remove us, so there is no
                                  %% need to make them remove NodeA as well.
                                  %% We therefore request removal from NodeA
                                  %% and wait for the nodedown which
                                  %% eventually will come since NodeA reported
                                  %% this...
                                  NodeA
                          end,

                 case is_node_potentially_known(RmNode, S0)
                     andalso global_group:participant(RmNode) of
                     false ->
                         S0;
                     true ->
                         {NDType, What} =
                             case node_vsn(RmNode, S0) of
                                 Vsn when Vsn < ?pop_vsn ->
                                     net_kernel:async_disconnect(RmNode),
                                     {remove_connection, "disconnected old"};
                                 Vsn ->
                                     gns_volatile_send(RmNode,
                                                       {remove_connection,
                                                        node()}),
                                     case global_group:member(RmNode)
                                         andalso Vsn >= ?gg_pop_vsn of
                                         true ->
                                             {ignore_node,
                                              "excluded global group member"};
                                         false ->
                                             {remove_connection,
                                              "requested disconnect from"}
                                     end
                             end,
                         logger:log(warning,
                                    "'global' at node ~p ~s node ~p in order "
                                    "to prevent overlapping partitions",
                                    [node(), What, RmNode]),
                         handle_nodedown(RmNode, S0, NDType)
                 end
         end,

    {noreply, S1};

handle_info({lost_connection, _NodeA, _XCreationA, _OpIdA, _NodeB}, S) ->
    %% Message introduced in protocol version ?pop_vsn
    {noreply, S};

handle_info({timeout, _, _} = TmoMsg, S) ->
    %% Instance specific message
    remove_lost_connection_info(TmoMsg),
    {noreply, S};

handle_info({remove_connection, Node}, S0) ->
    %% Message introduced in protocol version ?pop_vsn
    S2 = case is_node_potentially_known(Node, S0) of
             false ->
                 S0;
             true ->
                 {NDType, What}
                     = case global_group:member(Node) of
                           true ->
                               global_group ! {disconnect_node, Node},
                               {ignore_node, "excluded global group member"};
                           false ->
                               net_kernel:async_disconnect(Node),
                               {remove_connection, "disconnected"}
                       end,
                 S1 = handle_nodedown(Node, S0, NDType),
                 logger:log(warning,
                            "'global' at node ~p ~s node ~p in order to "
                            "prevent overlapping partitions",
                            [node(), What, Node]),
                 S1
         end,
    {noreply, S2};

handle_info({cancel_connect, Node, MyTag}, S0) ->
    %% An ongoing connect was canceled by the other side...
    %%
    %% Message introduced in protocol version ?verify_connect_vsn

    S3 = case get({sync_tag_my, Node}) of
             MyTag ->
                 S1 = cancel_locker(Node, S0, MyTag),
                 reset_node_state(Node),
                 S2 = S1#state{known = maps:remove({pending, Node},
                                                   S1#state.known)},
                 restart_connect(Node, MyTag, S2);
             _ ->
                 S0
         end,
    {noreply, S3};

handle_info({init_connect_ack, Node, HisMyTag, HisHisTag}, S0) ->
    %% Message introduced in protocol version ?verify_connect_vsn

    MyMyTag = get({sync_tag_my, Node}),
    MyHisTag = get({sync_tag_his, Node}),
    S1 = case MyMyTag =:= HisMyTag andalso MyHisTag =:= HisHisTag of
             true ->
                 S0;
             false ->
                 %% Connection attempt out of sync; cancel
                 %% this attempt and start over...

                 send_cancel_connect_message(Node, HisHisTag),
                 restart_connect(Node, MyMyTag, S0)
         end,
    {noreply, S1};

handle_info({prepare_shutdown, From, Ref}, S0) ->
    %% Prevent lost_connection messages being sent due to
    %% connections being taken down during the shutdown...
    S1 = S0#state{conf = #conf{connect_all = false,
                               prevent_over_part = false}},
    From ! {Ref, ok},
    {noreply, S1};

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
    delete_node_resource_info(MonitorRef),
    S1 = delete_lock(MonitorRef, S0),
    S = del_name(MonitorRef, S1),
    {noreply, S};

handle_info(Message, S) ->
    logger:log(warning, "The global_name_server "
               "received an unexpected message:\n"
               "handle_info(~tp, _)\n", [Message]),
    {noreply, S}.


%%========================================================================
%%========================================================================
%%=============================== Internal Functions =====================
%%========================================================================
%%========================================================================

save_node_resource_info(Node, Mon) ->
    NewRes = case ets:lookup(global_node_resources, Node) of
                 [] -> #{Mon => ok};
                 [{Node, OldRes}] -> OldRes#{Mon => ok}
             end,
    true = ets:insert(global_node_resources, [{Node, NewRes}, {Mon, Node}]),
    ok.

delete_node_resource_info(Mon) ->
    case ets:lookup(global_node_resources, Mon) of
        [] ->
            ok;
        [{Mon, Node}] ->
            [{Node, OldRes}] = ets:lookup(global_node_resources, Node),
            NewRes = maps:remove(Mon, OldRes),
            true = ets:delete(global_node_resources, Mon),
            case maps:size(NewRes) of
                0 ->
                    true = ets:delete(global_node_resources, Node),
                    ok;
                _ ->
                    true = ets:insert(global_node_resources, {Node, NewRes}),
                    ok
            end
    end.

delete_node_resources(Node, #state{} = State) ->
    case ets:lookup(global_node_resources, Node) of
        [] ->
            State;
        [{Node, Resources}] ->
            true = ets:delete(global_node_resources, Node),
            maps:fold(fun (Mon, ok, AccS0) ->
                              erlang:demonitor(Mon, [flush]),
                              true = ets:delete(global_node_resources, Mon),
                              AccS1 = delete_lock(Mon, AccS0),
                              del_name(Mon, AccS1)
                      end,
                      State, Resources)
    end.

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
    _ = gen_server:multi_call(
          TrueReplyNodes, global_name_server, {del_lock, Id}),
    Reply;
check_replies([], _Id, _Replies) ->
    true.

%%========================================================================
%% Another node wants to synchronize its registered names with us.
%% Both nodes must have a lock before they are allowed to continue.
%%========================================================================
init_connect(Vsn, Node, InitMsg, HisTag, HisVsn,
             #state{known = Known0} = S0) ->
    %% It is always the responsibility of newer versions to understand
    %% older versions of the protocol.
    try
        S1 = case maps:is_key({pending, Node}, Known0) of
                 false ->
                     S0;
                 true ->
                     %% This should not be possible unless global group has
                     %% been configured. We got an already ongoing connection
                     %% setup with Node and get yet another connection attempt
                     %% from Node.

                     if HisVsn < ?verify_connect_vsn ->
                             %% Old node that cannot handle this; give up...
                             erlang:disconnect_node(Node),
                             logger:log(error,
                                        "'global' at node ~p got an out of "
                                        "sync connection attempt from old "
                                        "version ~p node ~p. Disconnecting "
                                        "from it.", [node(), HisVsn, Node]),
                             throw({return, S0});

                        true ->
                             %% Cancel this new connection attempt as well
                             %% as the ongoing connection setup then restart
                             %% the connect by making a new connection
                             %% attempt...

                             send_cancel_connect_message(Node, HisTag),
                             MyOldTag = get({sync_tag_my, Node}),
                             restart_connect(Node, MyOldTag, S0)
                     end
             end,

        put({prot_vsn, Node}, Vsn),
        put({sync_tag_his, Node}, HisTag),
        case lists:keyfind(Node, 1, S1#state.resolvers) of
            {Node, MyTag, _Resolver} ->
                MyTag = get({sync_tag_my, Node}), % assertion
                {locker, _NoLongerAPid, _HisKnown0, HisTheLocker} = InitMsg,
                ?trace({init_connect,{histhelocker,HisTheLocker}}),
                HisKnown = [],
                S1#state.the_locker ! {his_the_locker, HisTheLocker,
                                       {Vsn,HisKnown}, HisTag, MyTag},
                if HisVsn < ?verify_connect_vsn ->
                        ok;
                   true ->
                        gns_volatile_send(Node,
                                          {init_connect_ack, node(),
                                           HisTag, MyTag})
                end,
                Known1 = S1#state.known,
                S1#state{known = Known1#{{pending, Node} => HisVsn}};
            false ->
                ?trace({init_connect,{pre_connect,Node},{histag,HisTag}}),
                put({pre_connect, Node}, {Vsn, InitMsg, HisTag}),
                S1
        end

    catch
        throw:{return, S} ->
            S
    end.

restart_connect(Node, MyTag, S0) ->
    %% We got a mismatch in connection setup; cancel
    %% current connection setup and try again...

    S1 = cancel_locker(Node, S0, MyTag),
    reset_node_state(Node),
    S2 = S1#state{known = maps:remove({pending, Node},
                                      S1#state.known)},

    if is_integer(MyTag) ->
            %% Node is up from our perspective; start a new resolver
            %% and send a new init_connect...
            handle_nodeup(Node, S2);
       true ->
            %% Node is down from our prespective; wait until
            %% global_group say Node is up by sending us a
            %% group_nodeup message...
            S2
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
    NewNodes = make_node_vsn_list([Node | HisKnown], S0),
    HisKnownNodes = node_list(HisKnown),
    sync_others(HisKnownNodes),
    ExtraInfo = [{vsn,get({prot_vsn, Node})}, {lock, get({lock_id, Node})}],
    S1 = do_ops(Ops, node(), Names_ext, ExtraInfo, S0),
    %% I am synced with Node, but not with HisKnown yet
    lists:foreach(fun(Pid) -> Pid ! {synced, [Node]} end, S1#state.syncers),
    S2 = lists:foldl(fun(CnclNode, AccS) ->
                             F = fun(Tag, CnclS) ->
                                         cancel_locker(CnclNode, CnclS, Tag)
                                 end,
                             cancel_resolved_locker(CnclNode, F, AccS)
                     end, S1, HisKnownNodes),
    %% The locker that took the lock is asked to send 
    %% the {new_nodes, ...} message. This ensures that
    %% {del_lock, ...} is received after {new_nodes, ...} 
    NewNodesMsg = {new_nodes, node(), Ops, Names_ext, NewNodes, ExtraInfo},
    NewNodesF = fun() ->
                        maps:foreach(
                          fun (N, V) when is_atom(N), V >= ?pgpv_vsn ->
                                  gen_server:cast({global_name_server, N},
                                                  NewNodesMsg);
                              (N, _OldV) when is_atom(N) ->
                                  gen_server:cast({global_name_server, N},
                                                  {new_nodes, node(), Ops,
                                                   Names_ext, node_list(NewNodes),
                                                   ExtraInfo});
                              (_, _) ->
                                  ok
                          end,
                          Known)
                end,
    F = fun(Tag, CnclS) ->
                cancel_locker(Node, CnclS, Tag, NewNodesF)
        end,
    S3 = cancel_resolved_locker(Node, F, S2),
    %% See (*) below... we're node b in that description
    {AddedNodes, S4} = add_to_known(NewNodes, S3),
    S4#state.the_locker ! {add_to_known, AddedNodes},
    S5 = trace_message(S4, {added, AddedNodes}, 
                       [{new_nodes, NewNodes}, {abcast, Known}, {ops,Ops}]),
    S5#state{synced = [Node | Synced]}.

cancel_resolved_locker(Node, CancelFun, #state{known = Known} = S0) ->
    Tag = get({sync_tag_my, Node}),
    ?trace({calling_cancel_locker,Tag,get()}),
    S1 = S0#state{known = maps:remove({pending, Node}, Known)},
    %% {pending, Node} removed, i.e., we wont send a cancel_connect
    %% message to Node in CancelFun()...
    S2 = CancelFun(Tag, S1),
    reset_node_state(Node),
    S2.

new_nodes(Ops, ConnNode, Names_ext, Nodes, ExtraInfo, S0) ->
    %% (*) This one requires some thought...
    %% We're node a, other nodes b and c:
    %% The problem is that {in_sync, a} may arrive before {resolved, [a]} to
    %% b from c, leading to b sending {new_nodes, [a]} to us (node a).
    %% Therefore, we make sure we never get duplicates in Known.
    {AddedNodes, S1} = add_to_known(Nodes, S0),
    sync_others(AddedNodes),
    S2 = do_ops(Ops, ConnNode, Names_ext, ExtraInfo, S1),
    ?trace({added_nodes_in_sync,{added_nodes,AddedNodes}}),
    S2#state.the_locker ! {add_to_known, AddedNodes},
    trace_message(S2, {added, AddedNodes}, [{ops,Ops}]).

do_whereis(Name, From) ->
    case is_global_lock_set() of
	false ->
	    gen_server:reply(From, where(Name));
	true ->
	    send_again({whereis, Name, From})
    end.

-doc false.
-spec terminate(term(), state()) -> 'ok'.

terminate(_Reason, _S) ->
    true = ets:delete(global_names),
    true = ets:delete(global_names_ext),
    true = ets:delete(global_locks),
    true = ets:delete(global_pid_names),
    true = ets:delete(global_pid_ids),
    ok.

-doc false.
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
	[{_Name, Pid, _Method, _Ref}] ->
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
    Ref = erlang:monitor(process, Pid),
    save_node_resource_info(node(Pid), Ref),
    true = ets:insert(global_pid_ids, {Pid, ResourceId}),
    true = ets:insert(global_pid_ids, {Ref, ResourceId}),
    Lock = {ResourceId, LockRequesterId, [{Pid,Ref} | PidRefs]},
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

remove_lock(ResourceId, LockRequesterId, Pid, [{Pid,Ref}], Down, S0) ->
    ?trace({remove_lock_1, {id,ResourceId},{pid,Pid}}),
    delete_node_resource_info(Ref),
    true = erlang:demonitor(Ref, [flush]),
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
                  {Pid, Ref} ->
                      delete_node_resource_info(Ref),
                      true = erlang:demonitor(Ref, [flush]),
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
            logger:log(warning, "'global' at ~w failed to connect to ~w\n",
                       [node(), Node]),
            global_name_server ! {extra_nodedown, Node}
    after 0 ->
            gen_server:cast({global_name_server,Node}, {in_sync,node(),true})
    end.
    % monitor_node(Node, false),
    % exit(normal).

insert_global_name(Name, Pid, Method, FromPidOrNode, ExtraInfo, S) ->
    Ref = erlang:monitor(process, Pid),
    save_node_resource_info(node(Pid), Ref),
    true = ets:insert(global_names, {Name, Pid, Method, Ref}),
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
                {_, _Pid, _Method, Ref1} <-
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
        [{Name, Pid, _Method, Ref}] ->
            delete_global_name2(Name, Pid, Ref, S);
        [] ->
            S
    end.

delete_global_name2(Name, S) ->
    case ets:lookup(global_names, Name) of
        [{Name, Pid, _Method, Ref}] ->
            true = ets:delete(global_names, Name),
            delete_global_name2(Name, Pid, Ref, S);
        [] ->
            S
    end.

delete_global_name2(Name, Pid, Ref, S) ->
    delete_node_resource_info(Ref),
    true = erlang:demonitor(Ref, [flush]),
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

%% As of version 6 of global, lockers (on different nodes) do not
%% communicate directly with each other. Instead messages are sent via
%% the server. This is in order to avoid race conditions where a
%% {lock_set, ...} message is received after (before) a nodedown
%% (nodeup).
%% -----------------------------------------------------------------

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

-record(him, {node, locker, vsn, my_tag, his_tag}).

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
        Message ->
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
                Message ->
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

the_locker_message({his_the_locker, HisTheLocker, HisKnown0, HisTag, MyTag} = _HtlMsg, S) ->
    ?trace({the_locker, his_the_locker, {node,node(HisTheLocker)}, _HtlMsg}),
    {HisVsn, _HisKnown} = HisKnown0,
    true = HisVsn > 4,
    Him = #him{node = node(HisTheLocker), my_tag = MyTag,
               his_tag = HisTag, locker = HisTheLocker, vsn = HisVsn},
    loop_the_locker(add_node(Him, S));
the_locker_message({cancel, _Node, undefined, no_fun} = _CMsg, S) ->
    ?trace({cancel_the_locker, undefined, {node,_Node}, _CMsg}),
    %% If we actually cancel something when a cancel message with the
    %% tag 'undefined' arrives, we may be acting on an old nodedown,
    %% to cancel a new nodeup, so we can't do that.
    loop_the_locker(S);
the_locker_message({cancel, Node, Tag, no_fun} = _CMsg, S) ->
    ?trace({the_locker, cancel, {multi,S}, {tag,Tag},{node,Node}}),
    loop_the_locker(remove_node(Node, Tag, S));
the_locker_message({lock_set, _Pid, false, _, _} = _Msg, S) ->
    ?trace({the_locker, spurious, {node,node(_Pid)}, _Msg}),
    loop_the_locker(S);
the_locker_message({lock_set, _Pid, false, _} = _Msg, S) ->
    ?trace({the_locker, spurious, {node,node(_Pid)}, _Msg}),
    loop_the_locker(S);
the_locker_message({lock_set, Pid, true, _HisKnown, MyTag} = _Msg, S) ->
    Node = node(Pid),
    ?trace({the_locker, self(), spontaneous, {node,Node}, _Msg}),
    case find_node_tag(Node, S) of
        {true, MyTag, HisVsn, HisTag} ->
            LockId = locker_lock_id(Pid, HisVsn),
            {IsLockSet, S1} = lock_nodes_safely(LockId, [], S),
            send_lock_set(S1, IsLockSet, Pid, HisVsn, HisTag),
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
                    wait_cancel_lock(Node, LockId, MyTag, Known2,
                                     the_locker_message_wait_cancel, S1),
                    S2 = S1#multi{just_synced = true},
                    loop_the_locker(remove_node(Node, MyTag, S2));
                false ->
                    loop_the_locker(S1#multi{just_synced = false})
            end;
        false ->
            ?trace({the_locker, not_there, {node,Node}}),
            send_lock_set(S, false, Pid, _HisVsn=5, 0),
            loop_the_locker(S)
    end;
the_locker_message({lock_set, Pid, true, HisKnown}, S) ->
    case find_node_tag(node(Pid), S) of
        {true, MyTag, _HisVsn, _HisTag} ->
            the_locker_message({lock_set, Pid, true, HisKnown, MyTag}, S);
        false ->
            ?trace({the_locker, not_there, {node,Node}}),
            send_lock_set(S, false, Pid, _HisVsn=5, 0),
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
the_locker_message({get_state, _, _} = Msg, S) ->
    get_state_reply(Msg, the_locker_message, S),
    loop_the_locker(S);
the_locker_message(Other, S) ->
    unexpected_message(Other, locker),
    ?trace({the_locker, {other_msg, Other}}),
    loop_the_locker(S).

get_state_reply({get_state, From, Ref}, Where, S) ->
    %% Ref should always be in first element of the reply!
    From ! {Ref, Where, S},
    ok.

-doc false.
get_locker() ->
    #state{the_locker = TheLocker} = info(),
    TheLocker.

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
                 node = Node, my_tag = MyTag, his_tag = HisTag} = Him,
            HisNode = [Node],
            Us = [node() | HisNode],
            LockId = locker_lock_id(HisTheLocker, HisVsn),
            ?trace({select_node, self(), {us, Us}}),
            %% HisNode = [Node] prevents deadlock:
            {IsLockSet, S2} = lock_nodes_safely(LockId, HisNode, S1),
            case IsLockSet of
                true -> 
                    Known1 = Us ++ S2#multi.known,
                    send_lock_set(S2, true, HisTheLocker, HisVsn, HisTag),
                    S3 = lock_is_set(S2, Him, MyTag, Known1, LockId),
                    loop_the_locker(S3);
                false ->
                    loop_the_locker(S2)
            end
    end.

send_lock_set(S, IsLockSet, HisTheLocker, Vsn, HisTag) ->
    ?trace({sending_lock_set, self(), {his,HisTheLocker}}),
    Message = if Vsn < ?verify_connect_vsn ->
                      {lock_set, self(), IsLockSet, S#multi.known};
                 true ->
                      {lock_set, self(), IsLockSet, S#multi.known, HisTag}
              end,
    if
        Vsn < 6 ->
            HisTheLocker ! Message,
            ok;
        true ->
            gen_server:cast({global_name_server, node(HisTheLocker)}, Message)
    end.

%% Version 5-6: Both sides use the same requester id. Thereby the nodes
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
    NewS = S#multi{known = Known, the_boss = TheBoss},
    NewS.

random_element(L) ->
    E = abs(erlang:monotonic_time()
		bxor erlang:unique_integer()) rem length(L),
    lists:nth(E+1, L).

exclude_known(Others, Known) ->
    [N || N <- Others, not lists:member(N#him.node, Known)].

lock_is_set(S, Him, MyTag, Known1, LockId) ->
    Node = Him#him.node,
    receive
	{lock_set, P, true, _, MyTag} when node(P) =:= Node ->
            lock_is_set_true_received(S, Him, MyTag, Known1, LockId, P);
	{lock_set, P, true, _, _OldMyTag} when node(P) =:= Node ->
            lock_is_set(S, Him, MyTag, Known1, LockId);
	{lock_set, P, true, _} when node(P) =:= Node ->
            lock_is_set_true_received(S, Him, MyTag, Known1, LockId, P);
	{lock_set, P, false, _, MyTag} when node(P) =:= Node ->
            ?trace({not_both_set, {node,Node},{p, P},{known1,Known1}}),
            _ = locker_trace(S, rejected, Known1),
	    delete_global_lock(LockId, Known1),
	    S;
	{lock_set, P, false, _, _OldMyTag} when node(P) =:= Node ->
            lock_is_set(S, Him, MyTag, Known1, LockId);
	{lock_set, P, false, _} when node(P) =:= Node ->
            ?trace({not_both_set, {node,Node},{p, P},{known1,Known1}}),
            _ = locker_trace(S, rejected, Known1),
	    delete_global_lock(LockId, Known1),
	    S;
	{cancel, Node, MyTag, Fun} = _CMsg ->
	    ?trace({the_locker, cancel2, {node,Node}, _CMsg}),
            call_fun(Fun),
            _ = locker_trace(S, rejected, Known1),
	    delete_global_lock(LockId, Known1),
            remove_node(Node, MyTag, S);
        {get_state, _, _} = Msg ->
            get_state_reply(Msg, lock_is_set, S),
            lock_is_set(S, Him, MyTag, Known1, LockId)

        %% There used to be an 'after' clause (OTP-4902), but it is 
        %% no longer needed:
        %% OTP-5770. Version 5 of the protocol. Deadlock can no longer
        %% occur due to the fact that if a partition is locked, one
        %% node in the other partition is also locked with the same
        %% lock-id, which makes it impossible for any node in the
        %% other partition to lock its partition unless it negotiates
        %% with the first partition.
    end.

lock_is_set_true_received(S, Him, MyTag, Known1, LockId, P) ->
    Node = node(P),
    gen_server:cast(global_name_server, 
                    {lock_is_set, Node, MyTag, LockId}),
    ?trace({lock_sync_done, {p,P, node(P)}, {me,self()}}),

    %% Wait for global to tell us to remove lock. Should the
    %% other locker's node die, global_name_server will
    %% receive nodedown, and then send {cancel, Node, Tag, Fun}.
    wait_cancel_lock(Node, LockId, MyTag, Known1,
                     lock_is_set_wait_cancel, S),
    S#multi{just_synced = true,
            local = lists:delete(Him, S#multi.local),
            remote = lists:delete(Him, S#multi.remote)}.

wait_cancel_lock(Node, LockId, MyTag, Known, Where, S) ->
    receive
        {cancel, Node, MyTag, Fun} = _CMsg ->
            ?trace({cancel_the_lock, {where, _Where},
                    {node,Node}, {known, Known}, _CMsg}),
            call_fun(Fun),
            delete_global_lock(LockId, Known);

        {get_state, _, _} = Msg ->
            get_state_reply(Msg, Where, S),
            wait_cancel_lock(Node, LockId, MyTag, Known, Where, S)
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
find_node_tag2(Node, [#him{node = Node, my_tag = MyTag, vsn = HisVsn, his_tag = HisTag} | _]) ->
    {true, MyTag, HisVsn, HisTag};
find_node_tag2(Node, [_E | Rest]) ->
    find_node_tag2(Node, Rest).

remove_node(Node, Tag, S) ->
    S#multi{local = remove_node2(Node, Tag, S#multi.local),
            remote = remove_node2(Node, Tag, S#multi.remote)}.

remove_node2(_Node, _Tag, []) ->
    [];
remove_node2(Node, Tag, [#him{node = Node, my_tag = Tag} | Rest]) ->
    Rest;
remove_node2(Node, Tag, [E | Rest]) ->
    [E | remove_node2(Node, Tag, Rest)].

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
    CMsg = {cancel, Node, Tag, ToBeRunOnLockerF},
    S#state.the_locker ! CMsg,
    Resolvers = S#state.resolvers,
    ?trace({cancel_locker, {node,Node},{tag,Tag},
            {sync_tag_my, get({sync_tag_my, Node})},{resolvers,Resolvers}}),
    send_cancel_connect(Node, Tag, S),
    case lists:keyfind(Node, 1, Resolvers) of
	{_, Tag, Resolver} ->
            ?trace({{resolver, Resolver}}),
            exit(Resolver, kill),
            S1 = trace_message(S, {kill_resolver, Node}, [Tag, Resolver]),
	    S1#state{resolvers = lists:keydelete(Node, 1, Resolvers)};
	_ ->
	    S
    end.

send_cancel_connect(Node, MyTag, #state{known = Known}) ->
    %% Send a cancel_connect message if we got an ongoing
    %% connect...
    try
        case maps:find({pending, Node}, Known) of
            {ok, Vsn} when Vsn < ?verify_connect_vsn ->
                throw(ignore);
            error ->
                throw(ignore);
            {ok, _Vsn} ->
                ok
        end,
        case get({sync_tag_my, Node}) of
            MyTag -> ok;
            _ -> throw(ignore)
        end,
        send_cancel_connect_message(Node, get({sync_tag_his, Node}))
    catch
        throw:ignore ->
            ok
    end.

send_cancel_connect_message(Node, HisTag) ->
    Msg = {cancel_connect, node(), HisTag},
    To = {global_name_server, Node},
    _ = erlang:send(To, Msg, [noconnect]),
    ok.

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
	[{Name, Pid, _Method, _Ref2}] ->
	    exchange_names(Tail, Node, Ops, Res);
	[{Name, Pid2, Method2, _Ref2}] when node() < Node ->
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
		    logger:log(info, "global: badrpc ~w received when "
                               "conflicting name ~tw was found\n",
                               [Badrpc, Name]),
		    Op = {insert, {Name, Pid, Method}},
		    exchange_names(Tail, Node, [Op | Ops], Res);
		Else ->
		    logger:log(info, "global: Resolve method ~w for "
                               "conflicting name ~tw returned ~tw\n",
                               [Method, Name, Else]),
		    Op = {delete, Name},
		    exchange_names(Tail, Node, [Op | Ops], [Op | Res])
	    end;
	[{Name, _Pid2, _Method, _Ref}] ->
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

-doc false.
resolve_it(Method, Name, Pid1, Pid2) ->
    catch Method(Name, Pid1, Pid2).

minmax(P1,P2) ->
    if node(P1) < node(P2) -> {P1, P2}; true -> {P2, P1} end.

-doc """
Can be used as a name resolving function for `register_name/3` and
`re_register_name/3`.

The function randomly selects one of the pids for registration and kills the
other one.
""".
-spec random_exit_name(Name, Pid1, Pid2) -> pid() when
      Name :: term(),
      Pid1 :: pid(),
      Pid2 :: pid().
random_exit_name(Name, Pid, Pid2) ->
    {Min, Max} = minmax(Pid, Pid2),
    logger:log(info, "global: Name conflict terminating ~tw\n",
               [{Name, Max}]),
    exit(Max, kill),
    Min.

-doc """
Can be used as a name resolving function for `register_name/3` and
`re_register_name/3`.

The function randomly selects one of the pids for registration, and sends the
message `{global_name_conflict, Name}` to the other pid.
""".
-spec random_notify_name(Name, Pid1, Pid2) -> pid() when
      Name :: term(),
      Pid1 :: pid(),
      Pid2 :: pid().
random_notify_name(Name, Pid, Pid2) ->
    {Min, Max} = minmax(Pid, Pid2),
    Max ! {global_name_conflict, Name},
    Min.

-doc """
Can be used as a name resolving function for `register_name/3` and
`re_register_name/3`.

The function unregisters both pids and sends the message
`{global_name_conflict, Name, OtherPid}` to both processes.
""".
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
                {Pid, Ref} = lists:keyfind(Ref, 2, PidRefs),
                remove_lock(ResourceId, LockRequesterId, Pid, PidRefs, true, S)
        end,
    lists:foldl(F, S0, Locks).

pid_locks(Ref) ->
    L = lists:flatmap(fun({_, ResourceId}) ->
                              ets:lookup(global_locks, ResourceId)
                      end, ets:lookup(global_pid_ids, Ref)),
    [Lock || Lock = {_Id, _Req, PidRefs} <- L, 
             ref_is_locking(Ref, PidRefs)].

ref_is_locking(Ref, PidRefs) ->
    lists:keyfind(Ref, 2, PidRefs) =/= false.                                  

handle_nodeup(Node, #state{the_locker = TheLocker,
                           resolvers = Rs,
                           known = Known} = S0) ->
    case maps:is_key(Node, Known) orelse lists:keymember(Node, 1, Rs) of
        true ->
            %% Already known or in progress...
            S0;

        false ->
            %% Make ourselves known to Node...

            %%
            %% In case a global group is configured, we might already
            %% have received an init_connect message from Node (the
            %% group_nodeup message may be delivered after early traffic
            %% over the channel have been delivered). If we already have
            %% gotten an init_connect, resend it to ourselves...
            %%
            resend_pre_connect(Node),

            %% erlang:unique_integer([monotonic]) is used as a tag to
            %% separate different synch sessions
            %% from each others. Global could be confused at bursty nodeups
            %% because it couldn't separate the messages between the different
            %% synch sessions started by a nodeup.
            MyTag = erlang:unique_integer([monotonic]),
            put({sync_tag_my, Node}, MyTag),

            %% In order to be compatible with unpatched R7 a locker
            %% process was spawned. Vsn 5 is no longer compatible with
            %% vsn 3 nodes, so the locker process is no longer needed.
            %% The permanent locker takes its place.
            NotAPid = no_longer_a_pid,
            Locker = {locker, NotAPid, Known, TheLocker},
            InitC = {init_connect, {?vsn, MyTag}, node(), Locker},
            ?trace({casting_init_connect, {node,Node},{initmessage,InitC},
                    {resolvers,Rs}}),
            gen_server:cast({global_name_server, Node}, InitC),
            Resolver = start_resolver(Node, MyTag),
            S1 = trace_message(S0, {new_resolver, Node}, [MyTag, Resolver]),
            S1#state{resolvers = [{Node, MyTag, Resolver} | Rs]}
    end.

handle_nodedown(Node, #state{synced = Syncs,
                             known = Known0} = S, What) ->
    %% DOWN signals from monitors have removed locks and registered names.
    NewS = cancel_locker(Node, S, get({sync_tag_my, Node})),
    NewS#state.the_locker ! {remove_from_known, Node},
    reset_node_state(Node),
    Known1 = case What of
                 remove_connection ->
                     maps:put({removing, Node}, yes, Known0);
                 ignore_node ->
                     maps:remove({removing, Node}, Known0);
                 disconnected ->
                     case maps:get({removing, Node}, Known0, no) of
                         yes ->
                             maps:remove({removing, Node}, Known0);
                         no ->
                             inform_connection_loss(Node, S),
                             Known0
                     end
             end,
    Known2 = maps:remove({pending, Node}, Known1),
    Known3 = maps:remove(Node, Known2),
    NewS#state{known = Known3, synced = lists:delete(Node, Syncs)}.

inform_connection_loss(Node,
                       #state{conf = #conf{connect_all = true,
                                           prevent_over_part = true}} = S) ->
    Msg = {lost_connection,
           node(),
           (get(creation_extension)
            + erlang:system_info(creation)
            - ?MIN_64BIT_SMALL_INT),
           erlang:unique_integer([monotonic]),
           Node},
    gns_volatile_multicast(Msg, Node, ?pop_vsn, true, S);
inform_connection_loss(_Node, #state{}) ->
    ok.

%%
%% Volatile send (does not bring up connections and does not
%% preserve signal order) of Msg to global name server at Node...
%%

gns_volatile_send(Node, Msg) ->
    _ = erlang:send({global_name_server, Node}, Msg, [noconnect]),
    ok.

%%
%% Volatile multicast of Msg to all global name servers on known nodes
%% (and pending known nodes if AlsoPend is true) using protocol version
%% MinVer or larger...
%%
gns_volatile_multicast(Msg, IgnoreNode, MinVer,
                       AlsoPend, #state{known = Known}) ->
    maps:foreach(fun (Node, Ver) when is_atom(Node),
                                      Node =/= IgnoreNode,
                                      Ver >= MinVer ->
                         _ = erlang:send({global_name_server, Node}, Msg,
                                         [noconnect]);
                     ({pending, Node}, Ver) when AlsoPend == true,
                                                 Node =/= IgnoreNode,
                                                 Ver >= MinVer ->
                         _ = erlang:send({global_name_server, Node}, Msg,
                                         [noconnect]);
                     (_, _) ->
                         ok
                 end, Known),
    ok.

is_node_potentially_known(Node, #state{known = Known}) ->
    maps:is_key(Node, Known) orelse maps:is_key({pending, Node}, Known).

node_vsn(Node, #state{}) when node() == Node ->
    ?vsn;
node_vsn(Node, #state{known = Known}) ->
    case maps:find(Node, Known) of
        {ok, Ver} ->
            Ver;
        error ->
            case maps:find({pending, Node}, Known) of
                {ok, Ver} ->
                    Ver;
                error ->
                    0
            end
    end.

node_list(NList) ->
    lists:map(fun (N) when is_atom(N) ->
                      N;
                  ({N, _V}) when is_atom(N) ->
                      N
              end, NList).
    

make_node_vsn_list(NList, #state{} = S) ->
    lists:map(fun ({N, 0}) when is_atom(N) ->
                      {N, node_vsn(N, S)};
                  (N) when is_atom(N) ->
                      {N, node_vsn(N, S)};
                  ({N, V} = NV) when is_atom(N),
                                     is_integer(V) ->
                      NV
              end, NList).

mk_known_list(Vsn, #state{known = Known}) when Vsn < ?pgpv_vsn ->
    lists:foldl(fun ({N, _V}, Ns) when is_atom(N) ->
                        [N | Ns];
                    (_, Ns) ->
                        Ns
                end,
                [],
                maps:to_list(Known));
mk_known_list(_Vsn, #state{known = Known}) ->
    lists:foldl(fun ({N, _V} = NV, NVs) when is_atom(N) ->
                        [NV | NVs];
                    (_, Ns) ->
                        Ns
                end,
                [],
                maps:to_list(Known)).

add_to_known(AddKnown, #state{known = Known} = S) ->
    Fun = fun (N, Acc) when N == node() ->
                  Acc;
              ({N, _V}, Acc) when N == node() ->
                  Acc;
              (N, {A, K} = Acc) when is_atom(N) ->
                  case maps:is_key(N, K) of
                      true -> Acc;
                      false -> {[N|A], maps:put(N, 0, K)}
                  end;
              ({N, V}, {A, K} = Acc) ->
                  case maps:find(N, K) of
                      error ->
                          {[N|A], maps:put(N, V, K)};
                      {ok, NV} when NV >= 0 ->
                          Acc;
                      {ok, _UnknownVsn} ->
                          %% Update version, but don't count
                          %% it as an added node...
                          {A, maps:put(N, V, K)}
                  end
          end,

    {Added, NewKnown} = lists:foldl(Fun, {[], Known}, AddKnown),
    {Added, S#state{known = NewKnown}}.

get_lost_connection_info(LcKey) ->
    case ets:lookup(global_lost_connections, LcKey) of
        [{LcKey, LcValue}] ->
            LcValue;
        _ ->
            {undefined, undefined, undefined}
    end.

save_lost_connection_info(LcKey, XCre, OpId, undefined) ->
    %% Make sure we clean up old unused information about
    %% lost connections...
    Tmr = erlang:start_timer(?lost_conn_info_cleanup_time,
                             self(), {lost_connection, LcKey}),
    Value = {XCre, OpId, Tmr},
    _ = ets:insert(global_lost_connections, {LcKey, Value}),
    ok;
save_lost_connection_info(LcKey, XCre, OpId, OldTmr) ->
    %% Cancel lost connection info cleanup timer for info being replaced...
    _ = erlang:cancel_timer(OldTmr, [{async, true}, {info, false}]),
    save_lost_connection_info(LcKey, XCre, OpId, undefined).

remove_lost_connection_info({timeout, Tmr, {lost_connection, LcKey}}) ->
    case ets:lookup(global_lost_connections, LcKey) of
        [{LcKey, {_, _, Tmr}}] ->
            _ = ets:delete(global_lost_connections, LcKey),
            ok;
        _ ->
            ok
    end;
remove_lost_connection_info(_) ->
    ok.

get_names() ->
    ets:select(global_names, 
               ets:fun2ms(fun({Name, Pid, Method, _Ref}) ->
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
	    intersection(NodesNG, nodes());
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
	    OwnNodeGroup = intersection(NodesNG, nodes()),
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
    logger:log(warning, "The global_name_server ~w process "
               "received an unexpected message:\n~tp\n",
               [What, Message]).

%%% Utilities

intersection(_, []) -> 
    [];
intersection(L1, L2) ->
    L1 -- (L1 -- L2).

%% Support legacy tuple funs as resolve functions.
allow_tuple_fun({M, F}) when is_atom(M), is_atom(F) ->
    fun M:F/3;
allow_tuple_fun(Fun) when is_function(Fun, 3) ->
    Fun.
