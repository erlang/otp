%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
-module(net_kernel).
-moduledoc """
Erlang networking kernel.

The net kernel is a system process, registered as `net_kernel`, which must be
operational for distributed Erlang to work. The purpose of this process is to
implement parts of the BIFs [`spawn/4`](`spawn/4`) and
[`spawn_link/4`](`spawn_link/4`), and to provide monitoring of the network.

An Erlang node is started using command-line flag `-name` or `-sname`:

```text
$ erl -sname foobar
```

It is also possible to call [`net_kernel:start(foobar, #{})`](`start/2`)
directly from the normal Erlang shell prompt:

```erlang
1> net_kernel:start(foobar, #{name_domain => shortnames}).
{ok,<0.64.0>}
(foobar@gringotts)2>
```

If the node is started with command-line flag `-sname`, the node name is
`foobar@Host`, where `Host` is the short name of the host (not the fully
qualified domain name). If started with flag `-name`, the node name is
`foobar@Host`, where `Host` is the fully qualified domain name. For more
information, see [`erl`](`e:erts:erl_cmd.md`).

Normally, connections are established automatically when another node is
referenced. This functionality can be disabled by setting Kernel configuration
parameter `dist_auto_connect` to `never`, see [`kernel(6)`](kernel_app.md). In
this case, connections must be established explicitly by calling
`connect_node/1`.

Which nodes that are allowed to communicate with each other is handled by the
magic cookie system, see section [Distributed Erlang](`e:system:distributed.md`)
in the Erlang Reference Manual.

> #### Warning {: .warning }
>
> Starting a distributed node without also specifying
> [`-proto_dist inet_tls`](`e:erts:erl_cmd.md#proto_dist`) will expose the node
> to attacks that may give the attacker complete access to the node and in
> extension the cluster. When using un-secure distributed nodes, make sure that
> the network is configured to keep potential attackers out. See the
> [Using SSL for Erlang Distribution](`e:ssl:ssl_distribution.md`) User's Guide
> for details on how to setup a secure distributed node.
""".

-compile(nowarn_deprecated_catch).

-behaviour(gen_server).

-define(nodedown(N, State), verbose({?MODULE, ?LINE, nodedown, N}, 1, State)).
%%-define(nodeup(N, State), verbose({?MODULE, ?LINE, nodeup, N}, 1, State)).

%%-define(dist_debug, true).

-ifdef(dist_debug).
-define(debug(Term), erlang:display(Term)).
-else.
-define(debug(Term), ok).
-endif.

-ifdef(dist_debug).
-define(connect_failure(Node,Term),
	io:format("Net Kernel 2: Failed connection to node ~p, reason ~p~n",
		  [Node,Term])).
-else.
-define(connect_failure(Node,Term),noop).
-endif.

%% Default ticktime change transition period in seconds
-define(DEFAULT_TRANSITION_PERIOD, 60).

%-define(TCKR_DBG, 1).

-ifdef(TCKR_DBG).
-define(tckr_dbg(X), erlang:display({?LINE, X})).
-else.
-define(tckr_dbg(X), ok).
-endif.

%% Documented API functions.

-export([allow/1, allowed/0,
	 connect_node/1,
	 monitor_nodes/1,
	 monitor_nodes/2,
	 setopts/2,
	 getopts/2,
	 start/2,
	 start/1,
	 stop/0]).

%% Exports for internal use.

-export([start_link/1,
	 kernel_apply/3,
	 longnames/0,
         nodename/0,
	 protocol_childspecs/0,
	 epmd_module/0,
         get_state/0]).

-export([disconnect/1, async_disconnect/1, passive_cnct/1]).
-export([hidden_connect_node/1]).
-export([set_net_ticktime/1, set_net_ticktime/2, get_net_ticktime/0]).

-export([node_info/1, node_info/2, nodes_info/0,
	 connecttime/0,
	 i/0, i/1, verbose/1]).

-export([publish_on_node/1]).

%% Internal exports for spawning processes.

-export([do_spawn/3,
	 spawn_func/6,
	 ticker/2,
	 ticker_loop/2,
	 aux_ticker/4]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
	 terminate/2,code_change/3]).

-export([passive_connect_monitor/2]).

-import(error_logger,[error_msg/2]).

-type node_name_type() :: static | dynamic.

-record(state, {
	  node,         %% The node name including hostname
	  type,         %% long or short names
	  tick,         %% tick information
	  connecttime,  %% the connection setuptime.
	  connections,  %% table of connections
	  conn_owners = #{}, %% Map of connection owner pids,
	  dist_ctrlrs = #{}, %% Map of dist controllers (local ports or pids),
	  pend_owners = #{}, %% Map of potential owners
	  listen,       %% list of  #listen
	  allowed,       %% list of allowed nodes in a restricted system
	  verbose = 0,   %% level of verboseness
          dyn_name_pool = #{},  %% Reusable remote node names: #{Host => [{Name,Creation}]}
          supervisor,   %% Our supervisor (net_sup | net_sup_dynamic | {restart,Restarter})
          req_map = #{} %% Map for outstanding async requests
	 }).

-record(listen, {
		 listen,     %% listen socket
		 accept,     %% accepting pid
		 address,    %% #net_address
		 module      %% proto module
		}).

-define(LISTEN_ID, #listen.listen).
-define(ACCEPT_ID, #listen.accept).

-type connection_state() :: pending | up | up_pending.
-type connection_type() :: normal | hidden.

-include("net_address.hrl").
-include_lib("kernel/include/logger.hrl").

%% Relaxed typing to allow ets:select without Dialyzer complains.
-record(connection, {
    node :: node() | '_',                  %% remote node name
    conn_id,                               %% Connection identity
    state :: connection_state() | '_',
    owner :: pid() | '_',                  %% owner pid
    ctrlr,                                 %% Controller port or pid
    pending_owner :: pid() | '_' | undefined,   %% possible new owner
    address = #net_address{} :: #net_address{} | '_',
    waiting = [],                          %% queued processes
    type :: connection_type() | '_',
    remote_name_type :: node_name_type() | '_',
    creation :: integer() | '_' | undefined,     %% only set if remote_name_type == dynamic
    named_me = false :: boolean() | '_'          %% did peer gave me my node name?
}).

-record(barred_connection, {
	  node %% remote node name
	 }).

-record(tick,
        {ticker     :: pid(),                 %% ticker
         time       :: pos_integer(),         %% net tick time (ms)
         intensity  :: 4..1000                %% ticks until timeout
        }).

-record(tick_change,
        {ticker     :: pid(),                 %% ticker
         time       :: pos_integer(),         %% net tick time (ms)
         intensity  :: 4..1000,               %% ticks until timeout
         how        :: 'longer' | 'shorter'   %% What type of change
        }).

%% Default connection setup timeout in milliseconds.
%% This timeout is set for every distributed action during
%% the connection setup.
-define(SETUPTIME, 7000).

%%% BIF

-export([dflag_unicode_io/1]).

-doc false.
-spec dflag_unicode_io(pid()) -> boolean().

dflag_unicode_io(_) ->
    erlang:nif_error(undef).

%%% End of BIF

%% Interface functions

-doc false.
kernel_apply(M,F,A) ->         request({apply,M,F,A}).

-doc """
Permits access to the specified set of nodes.

Before the first call to [`allow/1`](`allow/1`), any node with the correct
cookie can be connected. When [`allow/1`](`allow/1`) is called, a list of
allowed nodes is established. Any access attempts made from (or to) nodes not in
that list will be rejected.

Subsequent calls to [`allow/1`](`allow/1`) will add the specified nodes to the
list of allowed nodes. It is not possible to remove nodes from the list.

Disallowing an already connected node will not cause it to be disconnected. It
will, however, prevent any future reconnection attempts.

Passing `Nodes` as an empty list has never any affect at all.

Returns `error` if any element in `Nodes` is not an atom, and `ignored` if the
local node is not alive.
""".
-spec allow(Nodes) -> ok | error | ignored when
      Nodes :: [node()].
allow(Nodes) ->                request({allow, Nodes}).

-doc """
Returns a list of nodes that are explicitly allowed to connect to the node by calling
[`allow/1`](`allow/1`). If empty list is returned, it means that any node using the
same cookie will be able to connect.
""".
-doc(#{since => <<"OTP 28.0">>}).
-spec allowed() -> {ok, Nodes} | ignored when
      Nodes :: [node()].
allowed() ->                   request(allowed).

-doc false.
longnames() ->                 request(longnames).

-doc false.
nodename() ->                  request(nodename).

-doc """
Get the current state of the distribution for the local node.

Returns a map with (at least) the following key-value pairs:

- **`started => Started`** - Valid values for `Started`:

  - **`no`** - The distribution is not started. In this state none of the other
    keys below are present in the map.

  - **`static`** - The distribution was started with command line arguments
    [`-name`](`e:erts:erl_cmd.md#name`) or
    [`-sname`](`e:erts:erl_cmd.md#sname`).

  - **`dynamic`** - The distribution was started with
    [`net_kernel:start/1`](`start/1`) and can be stopped with
    [`net_kernel:stop/0`](`start/1`).

- **`name => Name`** - The name of the node. Same as returned by `erlang:node/0`
  except when `name_type` is `dynamic` in which case `Name` may be `undefined`
  (instead of `nonode@nohost`).

- **`name_type => NameType`** - Valid values for `NameType`:

  - **`static`** - The node has a static node name set by the node itself.

  - **`dynamic`** - The distribution was started in
    [dynamic node name](`e:system:distributed.md#dyn_node_name`) mode, and will
    get its node name assigned from the first node it connects to. If key `name`
    has value `undefined` that has not happened yet.

- **`name_domain => NameDomain`** - Valid values for `NameDomain`:

  - **`shortnames`** - The distribution was started to use node names with a
    short host portion (not fully qualified).

  - **`longnames`** - The distribution was started to use node names with a long
    fully qualified host portion.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec get_state() -> #{started => no | static | dynamic,
                       name => atom(),
                       name_type => static | dynamic,
                       name_domain => shortnames | longnames}.
get_state() ->
    case whereis(net_kernel) of
        undefined ->
            case retry_request_maybe(get_state) of
                ignored ->
                    #{started => no};
                Reply ->
                    Reply
            end;
        _ ->
            request(get_state)
    end.

-doc """
Turns a distributed node into a non-distributed node.

For other nodes in the network, this is the same as the node going down.
Only possible when the net kernel was started using `start/2`, otherwise
`{error, not_allowed}` is returned. Returns `{error, not_found}` if the local
node is not alive.
""".
-spec stop() -> ok | {error, Reason} when
      Reason :: not_allowed | not_found.
stop() ->
    erl_distribution:stop().

-type node_info() ::
    {address, #net_address{}} |
    {type, connection_type()} |
    {in, non_neg_integer()} |
    {out, non_neg_integer()} |
    {owner, pid()} |
    {state, connection_state()}.

-doc false.
-spec node_info(node()) -> {ok, [node_info()]} | {error, bad_node}.
node_info(Node) ->
    get_node_info(Node).

-doc false.
-spec node_info(node(), address) -> {ok, Address} | {error, bad_node} when Address :: #net_address{};
    (node(), type) -> {ok, Type} | {error, bad_node} when Type :: connection_type();
    (node(), in | out) -> {ok, Bytes} | {error, bad_node} when Bytes :: non_neg_integer();
    (node(), owner) -> {ok, Owner} | {error, bad_node} when Owner :: pid();
    (node(), state) -> {ok, State} | {error, bad_node} when State :: connection_state().
    %(node(), term()) -> {error, invalid_key} | {error, bad_node}.
node_info(Node, Key) ->
    get_node_info(Node, Key).

-doc false.
-spec nodes_info() -> {ok, [{node(), [node_info()]}]}.
nodes_info() ->
    get_nodes_info().

-doc false.
i() ->                         print_info().
-doc false.
i(Node) ->                     print_info(Node).

-doc false.
verbose(Level) when is_integer(Level) ->
    request({verbose, Level}).

-doc """
Sets `net_ticktime` (see [`kernel(6)`](kernel_app.md)) to `NetTicktime` seconds.
`TransitionPeriod` defaults to `60`.

Some definitions:

- **Minimum transition traffic interval (`MTTI`)** -
  `minimum(NetTicktime, PreviousNetTicktime)*1000 div 4` milliseconds.

- **Transition period** - The time of the least number of consecutive `MTTI`s to
  cover `TransitionPeriod` seconds following the call to
  [`set_net_ticktime/2`](`set_net_ticktime/2`) (that is,
  ((`TransitionPeriod*1000 - 1) div MTTI + 1)*MTTI` milliseconds).

If `NetTicktime < PreviousNetTicktime`, the `net_ticktime` change is done at the
end of the transition period; otherwise at the beginning. During the transition
period, `net_kernel` ensures that there is outgoing traffic on all connections
at least every `MTTI` millisecond.

> #### Note {: .info }
>
> The `net_ticktime` changes must be initiated on all nodes in the network (with
> the same `NetTicktime`) before the end of any transition period on any node;
> otherwise connections can erroneously be disconnected.

Returns one of the following:

- **`unchanged`** - `net_ticktime` already has the value of `NetTicktime` and is
  left unchanged.

- **`change_initiated`** - `net_kernel` initiated the change of `net_ticktime`
  to `NetTicktime` seconds.

- **`{ongoing_change_to, NewNetTicktime}`** - The request is _ignored_ because
  `net_kernel` is busy changing `net_ticktime` to `NewNetTicktime` seconds.
""".
-spec set_net_ticktime(NetTicktime, TransitionPeriod) -> Res when
      NetTicktime :: pos_integer(),
      TransitionPeriod :: non_neg_integer(),
      Res :: unchanged
           | change_initiated
           | {ongoing_change_to, NewNetTicktime},
      NewNetTicktime :: pos_integer().
set_net_ticktime(T, TP) when is_integer(T), T > 0, is_integer(TP), TP >= 0 ->
    ticktime_res(request({new_ticktime, T*1000, TP*1000})).

-doc(#{equiv => set_net_ticktime(NetTicktime, ?DEFAULT_TRANSITION_PERIOD)}).
-spec set_net_ticktime(NetTicktime) -> Res when
      NetTicktime :: pos_integer(),
      Res :: unchanged
           | change_initiated
           | {ongoing_change_to, NewNetTicktime},
      NewNetTicktime :: pos_integer().
set_net_ticktime(T) when is_integer(T) ->
    set_net_ticktime(T, ?DEFAULT_TRANSITION_PERIOD).

-doc """
Returns currently used net tick time in seconds.

For more information see the [`net_ticktime`](kernel_app.md#net_ticktime)
`Kernel` parameter.

Defined return values (`Res`):

- **`NetTicktime`** - `net_ticktime` is `NetTicktime` seconds.

- **`{ongoing_change_to, NetTicktime}`** - `net_kernel` is currently changing
  `net_ticktime` to `NetTicktime` seconds.

- **`ignored`** - The local node is not alive.
""".
-spec get_net_ticktime() -> Res when
      Res :: NetTicktime | {ongoing_change_to, NetTicktime} | ignored,
      NetTicktime :: pos_integer().
get_net_ticktime() ->
    ticktime_res(request(ticktime)).

%% The monitor_nodes() feature has been moved into the emulator.
%% The feature is reached via (intentionally) undocumented process
%% flags (we may want to move it elsewhere later). In order to easily
%% be backward compatible, errors are created here when process_flag()
%% fails.
-doc(#{equiv => monitor_nodes(Flag, [])}).
-spec monitor_nodes(Flag) -> ok | Error when
      Flag :: boolean(),
      Error :: error | {error, term()}.
monitor_nodes(Flag) ->
    case catch process_flag(monitor_nodes, Flag) of
	N when is_integer(N) -> ok;
	_ -> mk_monitor_nodes_error(Flag, [])
    end.

-doc """
The calling process subscribes or unsubscribes to node status change messages. A
`nodeup` message is delivered to all subscribing processes when a new node is
connected, and a `nodedown` message is delivered when a node is disconnected.

If `Flag` is `true`, a new subscription is started. If `Flag` is `false`, all
previous subscriptions started with the same `Options` are stopped. Two option
lists are considered the same if they contain the same set of options.

Delivery guarantees of `nodeup`/`nodedown` messages:

- `nodeup` messages are delivered before delivery of any signals from the remote
  node through the newly established connection.
- `nodedown` messages are delivered after all the signals from the remote node
  over the connection have been delivered.
- `nodeup` messages are delivered after the corresponding node appears in
  results from `erlang:nodes()`.
- `nodedown` messages are delivered after the corresponding node has disappeared
  in results from `erlang:nodes()`.
- As of OTP 23.0, a `nodedown` message for a connection being taken down will be
  delivered before a `nodeup` message due to a new connection to the same node.
  Prior to OTP 23.0, this was not guaranteed to be the case.

The format of the node status change messages depends on `Options`. If `Options`
is the empty list or if `net_kernel:monitor_nodes/1` is called, the format is as
follows:

```erlang
{nodeup, Node} | {nodedown, Node}
  Node = node()
```

When `Options` is the empty map or empty list, the caller will only subscribe
for status change messages for visible nodes. That is, only nodes that appear in
the result of `erlang:nodes/0`.

If `Options` equals anything other than the empty list, the format of the status
change messages is as follows:

```erlang
{nodeup, Node, Info} | {nodedown, Node, Info}
  Node = node()
  Info = #{Tag => Val} | [{Tag, Val}]
```

`Info` is either a map or a list of 2-tuples. Its content depends on `Options`.
If `Options` is a map, `Info` will also be a map. If `Options` is a list, `Info`
will also be a list.

When `Options` is a map, currently the following associations are allowed:

- **`connection_id => boolean()`** - If the value of the association equals
  `true`, a `connection_id => ConnectionId` association will be included in the
  `Info` map where `ConnectionId` is the connection identifier of the connection
  coming up or going down. For more info about this connection identifier see
  the documentation of [erlang:nodes/2](`m:erlang#connection_id`).

- **`node_type => NodeType`** - Valid values for `NodeType`:

  - **`visible`** - Subscribe to node status change messages for visible nodes
    only. The association `node_type => visible` will be included in the `Info`
    map.

  - **`hidden`** - Subscribe to node status change messages for hidden nodes
    only. The association `node_type => hidden` will be included in the `Info`
    map.

  - **`all`** - Subscribe to node status change messages for both visible and
    hidden nodes. The association `node_type => visible | hidden` will be
    included in the `Info` map.

  If no `node_type => NodeType` association is included in the `Options` map,
  the caller will subscribe for status change messages for visible nodes only,
  but _no_ `node_type => visible` association will be included in the `Info`
  map.

- **`nodedown_reason => boolean()`** - If the value of the association equals
  `true`, a `nodedown_reason => Reason` association will be included in the
  `Info` map for `nodedown` messages.

  [](){: #nodedown_reasons } `Reason` can, depending on which distribution
  module or process that is used, be any term, but for the standard TCP
  distribution module it is one of the following:

  - **`connection_setup_failed`** - The connection setup failed (after `nodeup`
    messages were sent).

  - **`no_network`** - No network is available.

  - **`net_kernel_terminated`** - The `net_kernel` process terminated.

  - **`shutdown`** - Unspecified connection shutdown.

  - **`connection_closed`** - The connection was closed.

  - **`disconnect`** - The connection was disconnected (forced from the current
    node).

  - **`net_tick_timeout`** - Net tick time-out.

  - **`send_net_tick_failed`** - Failed to send net tick over the connection.

  - **`get_status_failed`** - Status information retrieval from the `Port`
    holding the connection failed.

When `Options` is a list, currently `ListOption` can be one of the following:

- **`connection_id`** - A `{connection_id, ConnectionId}` tuple will be included
  in `Info` where `ConnectionId` is the connection identifier of the connection
  coming up or going down. For more info about this connection identifier see
  the documentation of [erlang:nodes/2](`m:erlang#connection_id`).

- **`{node_type, NodeType}`** - Valid values for `NodeType`:

  - **`visible`** - Subscribe to node status change messages for visible nodes
    only. The tuple `{node_type, visible}` will be included in the `Info` list.

  - **`hidden`** - Subscribe to node status change messages for hidden nodes
    only. The tuple `{node_type, hidden}` will be included in the `Info` list.

  - **`all`** - Subscribe to node status change messages for both visible and
    hidden nodes. The tuple `{node_type, visible | hidden}` will be included in
    the `Info` list.

  If no `{node_type, NodeType}` option has been given. The caller will subscribe
  for status change messages for visible nodes only, but _no_
  `{node_type, visible}` tuple will be included in the `Info` list.

- **`nodedown_reason`** - The tuple `{nodedown_reason, Reason}` will be included
  in the `Info` list for `nodedown` messages.

  See the documentation of the
  [`nodedown_reason => boolean()`](`m:net_kernel#nodedown_reasons`) association
  above for information about possible `Reason` values.

Example:

```erlang
(a@localhost)1> net_kernel:monitor_nodes(true, #{connection_id=>true, node_type=>all, nodedown_reason=>true}).
ok
(a@localhost)2> flush().
Shell got {nodeup,b@localhost,
                  #{connection_id => 3067552,node_type => visible}}
Shell got {nodeup,c@localhost,
                  #{connection_id => 13892107,node_type => hidden}}
Shell got {nodedown,b@localhost,
                    #{connection_id => 3067552,node_type => visible,
                      nodedown_reason => connection_closed}}
Shell got {nodedown,c@localhost,
                    #{connection_id => 13892107,node_type => hidden,
                      nodedown_reason => net_tick_timeout}}
Shell got {nodeup,b@localhost,
                  #{connection_id => 3067553,node_type => visible}}
ok
(a@localhost)3>
```
""".
-spec monitor_nodes(Flag, Options) -> ok | Error when
      Flag :: boolean(),
      Options :: OptionsList | OptionsMap,
      OptionsList :: [ListOption],
      ListOption :: connection_id
                  | {node_type, NodeType}
                  | nodedown_reason,
      OptionsMap :: #{connection_id => boolean(),
                      node_type => NodeType,
                      nodedown_reason => boolean()},
      NodeType :: visible | hidden | all,
      Error :: error | {error, term()}.
monitor_nodes(Flag, Opts) ->
    try
        MapOpts = if is_map(Opts) ->
                          error = maps:find(list, Opts),
                          Opts;
                     is_list(Opts) ->
                          lists:foldl(fun (nodedown_reason, Acc) ->
                                              Acc#{nodedown_reason => true};
                                          (connection_id, Acc) ->
                                              Acc#{connection_id => true};
                                          ({node_type, Val}, Acc) ->
                                              case maps:find(node_type, Acc) of
                                                  error -> ok;
                                                  {ok, Val} -> ok
                                              end,
                                              Acc#{node_type => Val}
                                      end,
                                      #{list => true},
                                      Opts)
                  end,
        true = is_integer(process_flag({monitor_nodes, MapOpts}, Flag)),
        ok
    catch
        _:_ ->
            mk_monitor_nodes_error(Flag, Opts)
    end.

%% ...
ticktime_res({A, I}) when is_atom(A), is_integer(I) -> {A, I div 1000};
ticktime_res(I)      when is_integer(I)          -> I div 1000;
ticktime_res(A)      when is_atom(A)             -> A.

%% Called though BIF's

%%% Long timeout if blocked (== barred), only affects nodes with
%%% {dist_auto_connect, once} set.
-doc false.
passive_cnct(Node) ->
    case request({passive_cnct, Node}) of
        ignored -> false;
        Other -> Other
    end.

-doc false.
disconnect(Node) ->            request({disconnect, Node}).

-doc false.
async_disconnect(Node) ->
    gen_server:cast(net_kernel, {async_disconnect, Node}).

%% Should this node publish itself on Node?
-doc false.
publish_on_node(Node) when is_atom(Node) ->
    global_group:publish(persistent_term:get({?MODULE, publish_type},
                                             hidden),
                         Node).

-doc """
Establishes a connection to `Node`.

Returns `true` if a connection was established or was already established or if
`Node` is the local node itself. Returns `false` if the connection attempt failed,
and `ignored` if the local node is not alive.
""".
-spec connect_node(Node) -> boolean() | ignored when
      Node :: node().
%% explicit connects
connect_node(Node) when is_atom(Node) ->
    request({connect, normal, Node}).
-doc false.
hidden_connect_node(Node) when is_atom(Node) ->
    request({connect, hidden, Node}).

-doc false.
passive_connect_monitor(From, Node) ->
    ok = monitor_nodes(true,[{node_type,all}]),
    Reply = case lists:member(Node,nodes([connected])) of
                true ->
                    true;
                _ ->
                    receive
                        {nodeup,Node,_} ->
                            true
                    after connecttime() ->
                            false
                    end
            end,
    ok = monitor_nodes(false,[{node_type,all}]),
    {Pid, Tag} = From,
    erlang:send(Pid, {Tag, Reply}).

%% If the net_kernel isn't running we ignore all requests to the
%% kernel, thus basically accepting them :-)
request(Req) ->
    case whereis(net_kernel) of
        P when is_pid(P) ->
            try
                gen_server:call(net_kernel,Req,infinity)
            catch
                exit:{Reason,_} when Reason =:= noproc;
                                     Reason =:= shutdown;
                                     Reason =:= killed ->
                    retry_request_maybe(Req)
            end;
        _ ->
            retry_request_maybe(Req)
    end.

retry_request_maybe(Req) ->
    case erts_internal:dynamic_node_name() of
        true ->
            %% net_kernel must be restarting due to lost connection
            %% toward the node that named us.
            %% We want reconnection attempts to succeed so we wait and retry.
            receive after 100 -> ok end,
            request(Req);

        false ->
            ignored
    end.


%% This function is used to dynamically start the
%% distribution.

-doc """
Turns a non-distributed node into a distributed node by starting `net_kernel`
and other necessary processes.

If `Name` is set to _`undefined`_ the distribution will be started to request a
dynamic node name from the first node it connects to. See
[Dynamic Node Name](`e:system:distributed.md#dyn_node_name`). Setting `Name` to
`undefined` implies options `dist_listen => false` and `hidden => true`.

Currently supported options:

- **`name_domain => NameDomain`** - Determines the host name part of the node
  name. If `NameDomain` equals `longnames`, fully qualified domain names will be
  used which also is the default. If `NameDomain` equals `shortnames`, only the
  short name of the host will be used.

- **`net_ticktime => NetTickTime`** - _Net tick time_ to use in seconds.
  Defaults to the value of the [`net_ticktime`](kernel_app.md#net_ticktime)
  `kernel(6)` parameter. For more information about _net tick time_, see the
  `kernel` parameter. However, note that if the value of the `kernel` parameter
  is invalid, it will silently be replaced by a valid value, but if an invalid
  `NetTickTime` value is passed as option value to this function, the call will
  fail.

- **`net_tickintensity => NetTickIntensity`** - _Net tick intensity_ to use.
  Defaults to the value of the
  [`net_tickintensity`](kernel_app.md#net_tickintensity) `kernel(6)` parameter.
  For more information about _net tick intensity_, see the `kernel` parameter.
  However, note that if the value of the `kernel` parameter is invalid, it will
  silently be replaced by a valid value, but if an invalid `NetTickIntensity`
  value is passed as option value to this function, the call will fail.

- **`dist_listen => boolean()`** - Enable or disable listening for incoming
  connections. Defaults to the value of the
  [`-dist_listen`](`e:erts:erl_cmd.md#dist_listen`) `erl` command line argument.
  Note that `dist_listen => false` implies `hidden => true`.

  If `undefined` has been passed as `Name`, the `dist_listen` option will be
  overridden with `dist_listen => false`.

- **`hidden => boolean()`** - Enable or disable hidden node. Defaults to `true`
  if the [`-hidden`](`e:erts:erl_cmd.md#hidden`) `erl` command line argument has
  been passed; otherwise `false`.

  If `undefined` has been passed as `Name`, or the option `dist_listen` equals
  `false`, the `hidden` option will be overridden with `hidden => true`.
""".
-doc(#{since => <<"OTP 24.3">>}).
-spec start(Name, Options) -> {ok, pid()} | {error, Reason} when
      Options :: #{name_domain => NameDomain,
                   net_ticktime => NetTickTime,
                   net_tickintensity => NetTickIntensity,
                   dist_listen => boolean(),
                   hidden => boolean()},
      Name :: atom(),
      NameDomain :: shortnames | longnames,
      NetTickTime :: pos_integer(),
      NetTickIntensity :: 4..1000,
      Reason :: {already_started, pid()} | term().

start(Name, Options) when is_atom(Name), is_map(Options) ->
    try
        maps:foreach(fun (name_domain, Val) when Val == shortnames;
                                                 Val == longnames ->
                             ok;
                         (net_ticktime, Val) when is_integer(Val),
                                                  Val > 0 ->
                             ok;
                         (net_tickintensity, Val) when is_integer(Val),
                                                       4 =< Val,
                                                       Val =< 1000 ->
                             ok;
                         (dist_listen, Val) when is_boolean(Val) ->
                             ok;
                         (hidden, Val) when is_boolean(Val) ->
                             ok;
                         (Opt, Val) ->
                             error({invalid_option, Opt, Val})
                     end, Options)
    catch error:Reason ->
            error(Reason, [Name, Options])
    end,
    erl_distribution:start(Options#{name => Name});
start(Name, Options) when is_map(Options) ->
    error(invalid_name, [Name, Options]);
start(Name, Options) ->
    error(invalid_options, [Name, Options]).

-doc """
Turns a non-distributed node into a distributed node by starting `net_kernel`
and other necessary processes.

`Options` list can only be exactly one of the following lists (order is
imporant):

- **`[Name]`** - The same as `net_kernel:start([Name, longnames, 15000])`.

- **`[Name, NameDomain]`** - The same as
  `net_kernel:start([Name, NameDomain, 15000])`.

- **`[Name, NameDomain, TickTime]`** - The same as
  [`net_kernel:start(Name, #{name_domain => NameDomain, net_ticktime => ((TickTime*4-1) div 1000) + 1, net_tickintensity => 4})`](`start/2`).
  Note that `TickTime` is _not_ the same as net tick time expressed in
  milliseconds. `TickTime` is the time between ticks when net tick intensity
  equals `4`.
""".
-doc(#{ deprecated => ~"Use start/2 instead" }).
-spec start(Options) -> {ok, pid()} | {error, Reason} when
      Options :: nonempty_list(Name | NameDomain | TickTime),
      Name :: atom(),
      NameDomain :: shortnames | longnames,
      TickTime :: pos_integer(),
      Reason :: {already_started, pid()} | term().

start([Name]) when is_atom(Name) ->
    start([Name, longnames, 15000]);
start([Name, NameDomain]) when is_atom(Name),
                               is_atom(NameDomain) ->
    start([Name, NameDomain, 15000]);
start([Name, NameDomain, TickTime]) when is_atom(Name),
                                         is_atom(NameDomain),
                                         is_integer(TickTime),
                                         TickTime > 0 ->
    %% NetTickTime is in seconds. TickTime is time in milliseconds
    %% between ticks when net tick intensity is 4. We round upwards...
    NetTickTime = ((TickTime*4-1) div 1000)+1,
    start(Name, #{name_domain => NameDomain,
                  net_ticktime => NetTickTime,
                  net_tickintensity => 4}).

%% This is the main startup routine for net_kernel (only for internal
%% use) by the Kernel application.

-doc false.
start_link(StartOpts) ->
    case gen_server:start_link({local, net_kernel}, ?MODULE,
			       make_init_opts(StartOpts), []) of
	{ok, Pid} ->
	    {ok, Pid};
	{error, {already_started, Pid}} ->
	    {ok, Pid};
	_Error ->
	    exit(nodistribution)
    end.

make_init_opts(Opts) ->
    %% Net tick time given in seconds, but kept in milliseconds...
    NTT1 = case maps:find(net_ticktime, Opts) of
               {ok, NTT0} ->
                   NTT0*1000;
               error ->
                   case application:get_env(kernel, net_ticktime) of
                       {ok, NTT0} when is_integer(NTT0), NTT0 < 1 ->
                           1000;
                       {ok, NTT0} when is_integer(NTT0) ->
                           NTT0*1000;
                       _ ->
                           60000
                   end
           end,

    NTI = case maps:find(net_tickintensity, Opts) of
              {ok, NTI0} ->
                  NTI0;
              error ->
                  case application:get_env(kernel, net_tickintensity) of
                      {ok, NTI0} when is_integer(NTI0), NTI0 < 4 ->
                          4;
                      {ok, NTI0} when is_integer(NTI0), NTI0 > 1000 ->
                          1000;
                      {ok, NTI0} when is_integer(NTI0) ->
                          NTI0;
                      _ ->
                          4
                  end
          end,

    %% Net tick time needs to be a multiple of net tick intensity;
    %% round net tick time upwards if not...
    NTT = if NTT1 rem NTI =:= 0 -> NTT1;
             true -> ((NTT1 div NTI) + 1) * NTI
          end,

    ND = case maps:find(name_domain, Opts) of
             {ok, ND0} ->
                 ND0;
             error ->
                 longnames
         end,

    DL = case split_node(maps:get(name, Opts)) of
             {"undefined", _} ->
                 %% dynamic node name implies dist_listen=false
                 false;
             _ ->
                 case maps:find(dist_listen, Opts) of
                     error ->
                         dist_listen_argument();
                     {ok, false} ->
                         false;
                     _ ->
                         true
                 end
         end,

    H = case DL of
            false ->
                %% dist_listen=false implies hidden=true
                true;
            true ->
                case maps:find(hidden, Opts) of
                    error ->
                        hidden_argument();
                    {ok, true} ->
                        true;
                    _ ->
                        false
                 end
         end,

    Opts#{net_ticktime => NTT,
          net_tickintensity => NTI,
          name_domain => ND,
          dist_listen => DL,
          hidden => H}.

-doc false.
init(#{name := Name,
       name_domain := NameDomain,
       net_ticktime := NetTicktime,
       net_tickintensity := NetTickIntensity,
       clean_halt := CleanHalt,
       supervisor := Supervisor,
       dist_listen := DistListen,
       hidden := Hidden}) ->
    %% We enable async_dist so that we won't need to do the
    %% nosuspend/spawn trick which just cause even larger
    %% memory consumption...
    _ = process_flag(async_dist, true),
    process_flag(trap_exit,true),
    persistent_term:put({?MODULE, publish_type},
                        if Hidden -> hidden;
                           true -> normal
                        end),
    case init_node(Name, NameDomain, CleanHalt, DistListen) of
	{ok, Node, Listeners} ->
	    process_flag(priority, max),
            TickInterval = NetTicktime div NetTickIntensity,
	    Ticker = spawn_link(net_kernel, ticker, [self(), TickInterval]),
	    {ok, #state{node = Node,
			type = NameDomain,
			tick = #tick{ticker = Ticker,
                                     time = NetTicktime,
                                     intensity = NetTickIntensity},
			connecttime = connecttime(),
			connections =
			ets:new(sys_dist,[named_table,
					  protected,
					  {keypos, #connection.node}]),
			listen = Listeners,
			allowed = [],
			verbose = 0,
                        supervisor = Supervisor
		       }};
	Error ->
            _ = persistent_term:erase({?MODULE, publish_type}),
            erts_internal:dynamic_node_name(false),
	    {stop, Error}
    end.

do_auto_connect_1(Node, ConnId, From, State) ->
    case ets:lookup(sys_dist, Node) of
        [#barred_connection{}] ->
            case ConnId of
                passive_cnct ->
                    spawn(?MODULE,passive_connect_monitor,[From,Node]),
                    {noreply, State};
                _ ->
                    erts_internal:abort_pending_connection(Node, ConnId),
                    {reply, false, State}
            end;

        ConnLookup ->
            do_auto_connect_2(Node, ConnId, From, State, ConnLookup)
    end.

do_auto_connect_2(Node, passive_cnct, From, State, ConnLookup) ->
    try erts_internal:new_connection(Node) of
        ConnId ->
            do_auto_connect_2(Node, ConnId, From, State, ConnLookup)
    catch
        _:_ ->
            error_logger:error_msg("~n** Cannot get connection id for node ~w~n",
                                   [Node]),
            {reply, false, State}
    end;
do_auto_connect_2(Node, ConnId, From, State, ConnLookup) ->
    case ConnLookup of
        [#connection{conn_id=ConnId, state = up}] ->
            {reply, true, State};
        [#connection{conn_id=ConnId, waiting=Waiting}=Conn] ->
            case From of
                noreply -> ok;
                _ -> ets:insert(sys_dist, Conn#connection{waiting = [From|Waiting]})
            end,
            {noreply, State};

        _ ->
            case application:get_env(kernel, dist_auto_connect) of
                {ok, never} ->
                    ?connect_failure(Node,{dist_auto_connect,never}),
                    erts_internal:abort_pending_connection(Node, ConnId),
                    {reply, false, State};

                %% This might happen due to connection close
                %% not being propagated to user space yet.
                %% Save the day by just not connecting...
                {ok, once} when ConnLookup =/= [],
                                (hd(ConnLookup))#connection.state =:= up ->
                    ?connect_failure(Node,{barred_connection,
                                           ets:lookup(sys_dist, Node)}),
                    {reply, false, State};
                _ ->
                    case setup(Node, ConnId, normal, From, State) of
                        {ok, SetupPid} ->
                            Owners = State#state.conn_owners,
                            {noreply,State#state{conn_owners=Owners#{SetupPid => Node}}};
                        _Error  ->
                            ?connect_failure(Node, {setup_call, failed, _Error}),
                            erts_internal:abort_pending_connection(Node, ConnId),
                            {reply, false, State}
                    end
            end
    end.

do_explicit_connect([#connection{conn_id = ConnId, state = up}], _, _, ConnId, _From, State) ->
    {reply, true, State};
do_explicit_connect([#connection{conn_id = ConnId}=Conn], _, _, ConnId, From, State)
  when Conn#connection.state =:= pending;
       Conn#connection.state =:= up_pending ->
    Waiting = Conn#connection.waiting,
    ets:insert(sys_dist, Conn#connection{waiting = [From|Waiting]}),
    {noreply, State};
do_explicit_connect([#barred_connection{}], Type, Node, ConnId, From , State) ->
    %% Barred connection only affects auto_connect, ignore it.
    do_explicit_connect([], Type, Node, ConnId, From , State);
do_explicit_connect(_ConnLookup, Type, Node, ConnId, From , State) ->
    case setup(Node,ConnId,Type,From,State) of
        {ok, SetupPid} ->
            Owners = State#state.conn_owners,
            {noreply,State#state{conn_owners=Owners#{SetupPid => Node}}};
        _Error ->
            ?connect_failure(Node, {setup_call, failed, _Error}),
            {reply, false, State}
    end.

%% ------------------------------------------------------------
%% handle_call.
%% ------------------------------------------------------------

%%
%% Passive auto-connect to Node.
%% The response is delayed until the connection is up and running.
%%
-doc false.
handle_call({passive_cnct, Node}, From, State) when Node =:= node() ->
    async_reply({reply, true, State}, From);
handle_call({passive_cnct, Node}, From, State) ->
    verbose({passive_cnct, Node}, 1, State),
    R = do_auto_connect_1(Node, passive_cnct, From, State),
    return_call(R, From);

%%
%% Explicit connect
%% The response is delayed until the connection is up and running.
%%
handle_call({connect, _, Node}, From, State) when Node =:= node() ->
    async_reply({reply, true, State}, From);
handle_call({connect, _Type, _Node}, _From, #state{supervisor = {restart,_}}=State) ->
    {noreply, State};
handle_call({connect, Type, Node}, From, State) ->
    verbose({connect, Type, Node}, 1, State),
    ConnLookup = ets:lookup(sys_dist, Node),
    R = try erts_internal:new_connection(Node) of
            ConnId ->
                R1 = do_explicit_connect(ConnLookup, Type, Node, ConnId, From, State),
                case R1 of
                    {reply, true, _S} -> %% already connected
                        ok;
                    {noreply, _S} -> %% connection pending
                        ok;
                    {reply, false, _S} -> %% connection refused
                        erts_internal:abort_pending_connection(Node, ConnId)
                end,
                R1
        catch
            _:_ ->
                error_logger:error_msg("~n** Cannot get connection id for node ~w~n",
                                       [Node]),
                {reply, false, State}
        end,
    return_call(R, From);

%%
%% Close the connection to Node.
%%
handle_call({disconnect, Node}, From, State) when Node =:= node() ->
    async_reply({reply, false, State}, From);
handle_call({disconnect, Node}, From, State) ->
    verbose({disconnect, Node}, 1, State),
    {Reply, State1} = do_disconnect(Node, State, false),
    async_reply({reply, Reply, State1}, From);

%%
%% The spawn/4 BIF ends up here.
%%
handle_call({spawn,M,F,A,Gleader},{From,Tag},State) when is_pid(From) ->
    do_spawn([no_link,{From,Tag},M,F,A,Gleader],[],State);

%%
%% The spawn_link/4 BIF ends up here.
%%
handle_call({spawn_link,M,F,A,Gleader},{From,Tag},State) when is_pid(From) ->
    do_spawn([link,{From,Tag},M,F,A,Gleader],[],State);

%%
%% The spawn_opt/5 BIF ends up here.
%%
handle_call({spawn_opt,M,F,A,O,L,Gleader},{From,Tag},State) when is_pid(From) ->
    do_spawn([L,{From,Tag},M,F,A,Gleader],O,State);

%%
%% Only allow certain nodes.
%%
handle_call({allow, Nodes}, From, State) ->
    case all_atoms(Nodes) of
	true ->
	    Allowed = lists:uniq(State#state.allowed ++ Nodes),
            async_reply({reply,ok,State#state{allowed = Allowed}},
                        From);
	false ->
	    async_reply({reply,error,State}, From)
    end;

handle_call(allowed, From, #state{allowed = Allowed} = State) ->
    async_reply({reply,{ok,Allowed},State}, From);

%%
%% authentication, used by auth. Simply works as this:
%% if the message comes through, the other node IS authorized.
%%
handle_call({is_auth, _Node}, From, State) ->
    async_reply({reply,yes,State}, From);

%%
%% Not applicable any longer !?
%%
handle_call({apply,_Mod,_Fun,_Args}, {Pid, _Tag} = From, State)
  when is_pid(Pid), node(Pid) =:= node() ->
    async_reply({reply, not_implemented, State}, From);

handle_call(longnames, From, State) ->
    async_reply({reply, get(longnames), State}, From);

handle_call(nodename, From, State) ->
    async_reply({reply, State#state.node, State}, From);

handle_call({verbose, Level}, From, State) ->
    async_reply({reply, State#state.verbose, State#state{verbose = Level}},
                From);

%%
%% Set new ticktime
%%

%% The tick field of the state contains either a #tick{} or a
%% #tick_change{} record.

handle_call(ticktime, From, #state{tick = #tick{time = T}} = State) ->
    async_reply({reply, T, State}, From);
handle_call(ticktime, From, #state{tick = #tick_change{time = T}} = State) ->
    async_reply({reply, {ongoing_change_to, T}, State}, From);

handle_call({new_ticktime,T,_TP}, From, #state{tick = #tick{time = T}} = State) ->
    ?tckr_dbg(no_tick_change),
    async_reply({reply, unchanged, State}, From);

handle_call({new_ticktime,T,TP}, From, #state{tick = #tick{ticker = Tckr,
                                                           time = OT,
                                                           intensity = I}} = State) ->
    ?tckr_dbg(initiating_tick_change),
    %% We need to preserve tick intensity and net tick time needs to be a
    %% multiple of tick intensity...
    {NT, NIntrvl} = case T < I of
                        true ->
                            %% Max 1 tick per millisecond implies that
                            %% minimum net tick time equals intensity...
                            {I, 1};
                        _ ->
                            NIntrvl0 = T div I,
                            case T rem I of
                                0 ->
                                    {T, NIntrvl0};
                                _ ->
                                    %% Round net tick time upwards...
                                    {(NIntrvl0+1)*I, NIntrvl0+1}
                            end
                    end,
    case NT == OT of
        true ->
                async_reply({reply, unchanged, State}, From);
        false ->
            start_aux_ticker(NIntrvl, OT div I, TP),
            How = case NT > OT of
                      true ->
                          ?tckr_dbg(longer_ticktime),
                          Tckr ! {new_ticktime, NIntrvl},
                          longer;
                      false ->
                          ?tckr_dbg(shorter_ticktime),
                          shorter
                  end,
            async_reply({reply, change_initiated,
                         State#state{tick = #tick_change{ticker = Tckr,
                                                         time = NT,
                                                         intensity = I,
                                                         how = How}}}, From)
    end;

handle_call({new_ticktime,_T,_TP},
	    From,
	    #state{tick = #tick_change{time = T}} = State) ->
    async_reply({reply, {ongoing_change_to, T}, State}, From);

handle_call({setopts, new, Opts}, From, State) ->
    setopts_new(Opts, From, State);

handle_call({setopts, Node, Opts}, From, State) ->
    opts_node(setopts, Node, Opts, From, State);

handle_call({getopts, Node, Opts}, From, State) ->
    opts_node(getopts, Node, Opts, From, State);


handle_call(get_state, From, State) ->
    Started = case State#state.supervisor of
                  net_sup -> static;
                  _ -> dynamic
              end,
    {NameType,Name} = case {erts_internal:dynamic_node_name(), node()} of
                          {false, Node} ->
                              {static, Node};
                          {true, nonode@nohost} ->
                              {dynamic, undefined};
                          {true, Node} ->
                              {dynamic, Node}
                      end,
    NameDomain = case get(longnames) of
                     true -> longnames;
                     false -> shortnames
                 end,
    Return = #{started => Started,
               name_type => NameType,
               name => Name,
               name_domain => NameDomain},
    async_reply({reply, Return, State}, From);

handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% ------------------------------------------------------------
%% handle_cast.
%% ------------------------------------------------------------

-doc false.
handle_cast({async_disconnect, Node}, State) when Node =:= node() ->
    {noreply, State};
handle_cast({async_disconnect, Node}, State) ->
    verbose({async_disconnect, Node}, 1, State),
    {_Reply, State1} = do_disconnect(Node, State, true),
    {noreply, State1};

handle_cast(_, State) ->
    {noreply,State}.

%% ------------------------------------------------------------
%% code_change.
%% ------------------------------------------------------------

-doc false.
code_change(_OldVsn, State, _Extra) ->
    {ok,State}.

%% ------------------------------------------------------------
%% terminate.
%% ------------------------------------------------------------

-doc false.
terminate(Reason, State) ->
    case State of
        #state{supervisor = {restart, _}} ->
            ok;
        _ ->
            _ = persistent_term:erase({?MODULE, publish_type}),
            erts_internal:dynamic_node_name(false)
    end,

    case Reason of
        no_network ->
            ok;
        _ ->
            lists:foreach(
              fun(#listen {listen = Listen,module = Mod}) ->
                      case Listen of
                          undefined -> ignore;
                          _ -> Mod:close(Listen)
                      end
              end, State#state.listen)
    end,
    lists:foreach(fun(Node) -> ?nodedown(Node, State)
                  end, get_nodes_up_normal() ++ [node()]).

%% ------------------------------------------------------------
%% handle_info.
%% ------------------------------------------------------------

%%
%% Asynchronous auto connect request
%%
-doc false.
handle_info({auto_connect,Node, DHandle}, State) ->
    verbose({auto_connect, Node, DHandle}, 1, State),
    ConnId = DHandle,
    NewState =
        case do_auto_connect_1(Node, ConnId, noreply, State) of
            {noreply, S} ->           %% Pending connection
                S;

            {reply, true, S} ->  %% Already connected
                S;

            {reply, false, S} -> %% Connection refused
                S
        end,
    {noreply, NewState};

%%
%% accept a new connection.
%%
handle_info({accept,AcceptPid,Socket,Family,Proto}=Accept, State) ->
    case get_proto_mod(Family,Proto,State#state.listen) of
	{ok, Mod} ->
	    Pid = Mod:accept_connection(AcceptPid,
					Socket,
                                        State#state.node,
					State#state.allowed,
					State#state.connecttime),
            verbose({Accept,Pid}, 2, State),
	    AcceptPid ! {self(), controller, Pid},
	    {noreply,State};
	_ ->
            verbose({Accept,unsupported_protocol}, 2, State),
	    AcceptPid ! {self(), unsupported_protocol},
	    {noreply, State}
    end;

%%
%% New dist controller has been registered
%%
handle_info({dist_ctrlr, Ctrlr, Node, SetupPid} = Msg,
	    #state{dist_ctrlrs = DistCtrlrs} = State) ->
    case ets:lookup(sys_dist, Node) of
	[Conn] when (Conn#connection.state =:= pending)
                    andalso (Conn#connection.owner =:= SetupPid)
                    andalso (Conn#connection.ctrlr =:= undefined)
                    andalso (is_port(Ctrlr) orelse is_pid(Ctrlr))
                    andalso (node(Ctrlr) == node()) ->
            link(Ctrlr),
            verbose(Msg, 2, State),
            ets:insert(sys_dist, Conn#connection{ctrlr = Ctrlr}),
            {noreply, State#state{dist_ctrlrs = DistCtrlrs#{Ctrlr => Node}}};
	_ ->
            error_msg("Net kernel got ~tw~n",[Msg]),
	    {noreply, State}
    end;

%%
%% A node has successfully been connected.
%%
handle_info({SetupPid, {nodeup,Node,Address,Type,NamedMe} = Nodeup},
            #state{tick = Tick} = State) ->
    case ets:lookup(sys_dist, Node) of
	[Conn] when (Conn#connection.state =:= pending)
                    andalso (Conn#connection.owner =:= SetupPid)
                    andalso (Conn#connection.ctrlr /= undefined) ->
            ets:insert(sys_dist, Conn#connection{state = up,
                                                 address = Address,
                                                 waiting = [],
                                                 type = Type,
                                                 named_me = NamedMe}),
            TickIntensity = case Tick of
                              #tick{intensity = TI} -> TI;
                              #tick_change{intensity = TI} ->  TI
                         end,
            SetupPid ! {self(), inserted, TickIntensity},
            reply_waiting(Node,Conn#connection.waiting, true),
            State1 = case NamedMe of
                         true -> State#state{node = node()};
                         false -> State
                     end,
            verbose(Nodeup, 1, State1),
            verbose({nodeup,Node,SetupPid,Conn#connection.ctrlr}, 2, State1),
            {noreply, State1};
	_ ->
	    SetupPid ! {self(), bad_request},
	    {noreply, State}
    end;

%%
%% Mark a node as pending (accept) if not busy.
%%
handle_info({AcceptPid, {accept_pending,MyNode,NodeOrHost,Type}}, State0) ->
    {NameType, Node, Creation,
     ConnLookup, State} = ensure_node_name(NodeOrHost, State0),
    case ConnLookup of
	[#connection{state=pending}=Conn] ->
	    if
		MyNode > Node ->
		    AcceptPid ! {self(),{accept_pending,nok_pending}},
                    verbose({accept_pending_nok, Node, AcceptPid}, 2, State),
		    {noreply,State};
		true ->
		    %%
		    %% A simultaneous connect has been detected and we want to
		    %% change pending process.
		    %%
		    OldOwner = Conn#connection.owner,
                    case maps:is_key(OldOwner, State#state.conn_owners) of
                        true ->
                            verbose({remark,OldOwner,AcceptPid}, 2, State),
                            ?debug({net_kernel, remark, old, OldOwner, new, AcceptPid}),
                            exit(OldOwner, remarked),
                            receive
                                {'EXIT', OldOwner, _} = Exit ->
                                    verbose(Exit, 2, State),
                                    true
                            end;
                        false ->
                            verbose(
                              {accept_pending, OldOwner, inconsistency},
                              2, State),
                            ok % Owner already exited
                    end,
		    ets:insert(sys_dist, Conn#connection{owner = AcceptPid}),
		    AcceptPid ! {self(),{accept_pending,ok_pending}},
                    Owners = maps:remove(OldOwner, State#state.conn_owners),
		    {noreply, State#state{conn_owners=Owners#{AcceptPid => Node}}}
	    end;
	[#connection{state=up}=Conn] ->
	    AcceptPid ! {self(), {accept_pending, up_pending}},
	    ets:insert(sys_dist, Conn#connection { pending_owner = AcceptPid,
						  state = up_pending }),
	    Pend = State#state.pend_owners,
	    {noreply, State#state { pend_owners = Pend#{AcceptPid => Node} }};
	[#connection{state=up_pending}] ->
	    AcceptPid ! {self(), {accept_pending, already_pending}},
	    {noreply, State};
	_ ->
            try erts_internal:new_connection(Node) of
                ConnId ->
                    ets:insert(sys_dist, #connection{node = Node,
                                                     conn_id = ConnId,
                                                     state = pending,
                                                     owner = AcceptPid,
                                                     type = Type,
                                                     remote_name_type = NameType,
                                                     creation = Creation}),
                    Ret = case NameType of
                              static -> ok;
                              dynamic-> {ok, Node, Creation}
                          end,
                    AcceptPid ! {self(),{accept_pending,Ret}},
                    Owners = State#state.conn_owners,
                    {noreply, State#state{conn_owners = Owners#{AcceptPid => Node}}}
            catch
                _:_ ->
                    error_logger:error_msg("~n** Cannot get connection id for node ~w~n",
                                           [Node]),
                    AcceptPid ! {self(),{accept_pending,nok_pending}},
                    {noreply, State}
            end
    end;

handle_info({SetupPid, {is_pending, Node}}, State) ->
    Reply = case maps:get(SetupPid, State#state.conn_owners, undefined) of
                Node -> true;
                _ -> false
            end,
    SetupPid ! {self(), {is_pending, Reply}},
    {noreply, State};

handle_info({AcceptPid, {wait_pending, Node}}, State) ->
    case get_conn(Node) of
        {ok, #connection{state = up_pending,
                         ctrlr = OldCtrlr,
                         pending_owner = AcceptPid}} ->
            %% Kill old controller to make sure new connection setup
            %% does not get stuck.
            ?debug({net_kernel, wait_pending, kill, OldCtrlr, new, AcceptPid}),
            exit(OldCtrlr, wait_pending);
        _ ->
            %% Old connnection maybe already gone
            ignore
    end,
    %% Exiting controller will trigger {Kernel,pending} reply
    %% in up_pending_nodedown()
    {noreply, State};

%%
%% Responses to asynchronous requests we've made...
%%
handle_info({ReqId, Reply},
            #state{req_map = ReqMap} = S) when is_map_key(ReqId, ReqMap) ->
    handle_async_response(reply, ReqId, Reply, S);
handle_info({'DOWN', ReqId, process, _Pid, Reason},
            #state{req_map = ReqMap} = S) when is_map_key(ReqId, ReqMap) ->
    handle_async_response(down, ReqId, Reason, S);

%%
%% Handle different types of process terminations.
%%
handle_info({'EXIT', From, Reason}, State) ->
    handle_exit(From, Reason, State);

%%
%% Handle badcookie and badname messages !
%%
handle_info({From,registered_send,To,Mess},State) ->
    send(From,To,Mess),
    {noreply,State};

%% badcookies SHOULD not be sent
%% (if someone does erlang:set_cookie(node(),foo) this may be)
handle_info({From,badcookie,_To,_Mess}, State) ->
    error_logger:error_msg("~n** Got OLD cookie from ~w~n",
			   [getnode(From)]),
    {_Reply, State1} = do_disconnect(getnode(From), State, false),
    {noreply,State1};

%%
%% Tick all connections.
%%
handle_info(tick, State) ->
    ?tckr_dbg(tick),
    maps:foreach(fun (Pid, _Node) ->
                     Pid ! {self(), tick}
              end,
              State#state.conn_owners),
    {noreply,State};

handle_info(aux_tick, State) ->
    ?tckr_dbg(aux_tick),
    maps:foreach(fun (Pid, _Node) ->
                     Pid ! {self(), aux_tick}
              end,
              State#state.conn_owners),
    {noreply,State};

handle_info(transition_period_end,
	    #state{tick = #tick_change{ticker = Tckr,
				       time = T,
                                       intensity = I,
				       how = How}} = State) ->
    ?tckr_dbg(transition_period_ended),
    case How of
	shorter ->
            Interval = T div I,
            Tckr ! {new_ticktime, Interval},
            ok;
	_ ->
            ok
    end,
    {noreply,State#state{tick = #tick{ticker = Tckr,
                                      time = T,
                                      intensity = I}}};

handle_info(X, State) ->
    error_msg("Net kernel got ~tw~n",[X]),
    {noreply,State}.

ensure_node_name(Node, State) when is_atom(Node) ->
    {static, Node, undefined, ets:lookup(sys_dist, Node), State};
ensure_node_name(Host, State0) when is_list(Host) ->
    case string:split(Host, "@", all) of
        [Host] ->
            {Node, Creation, State1} = generate_node_name(Host, State0),
            case ets:lookup(sys_dist, Node) of
                [#connection{}] ->
                    %% Either a static named connection setup used a recycled
                    %% dynamic name or we have an unlikely random dynamic
                    %% name clash. Either way try again.
                    ensure_node_name(Host, State1);

                ConnLookup ->
                    {dynamic, Node, Creation, ConnLookup, State1}
            end;

        _ ->
            {error, Host, undefined, [], State0}
    end.

generate_node_name(Host, State0) ->
    NamePool = State0#state.dyn_name_pool,
    case maps:get(Host, NamePool, []) of
        [] ->
            Name = integer_to_list(rand:uniform(1 bsl 64), 36),
            {list_to_atom(Name ++ "@" ++ Host),
             create_creation(),
             State0};

        [{Node,Creation} | Rest] ->
            {Node, Creation,
             State0#state{dyn_name_pool = NamePool#{Host => Rest}}}
    end.



%% -----------------------------------------------------------
%% Handle exit signals.
%% We have 6 types of processes to handle.
%%
%%    1. The Listen process.
%%    2. The Accept process.
%%    3. Connection owning processes.
%%    4. The ticker process.
%%   (5. Garbage pid.)
%%
%% The process type function that handled the process throws
%% the handle_info return value !
%% -----------------------------------------------------------

handle_exit(Pid, Reason, State) ->
    catch do_handle_exit(Pid, Reason, State).

do_handle_exit(Pid, Reason, State) ->
    listen_exit(Pid, Reason, State),
    accept_exit(Pid, Reason, State),
    conn_own_exit(Pid, Reason, State),
    dist_ctrlr_exit(Pid, Reason, State),
    pending_own_exit(Pid, Reason, State),
    ticker_exit(Pid, Reason, State),
    restarter_exit(Pid, Reason, State),
    verbose({'EXIT', Pid, Reason}, 2, State),
    {noreply,State}.

listen_exit(Pid, Reason, State) ->
    case lists:keymember(Pid, ?LISTEN_ID, State#state.listen) of
	true ->
            verbose({listen_exit, Pid, Reason}, 2, State),
	    error_msg("** Netkernel terminating ... **\n", []),
	    throw({stop,no_network,State});
	false ->
	    false
    end.

accept_exit(Pid, Reason, State) ->
    Listen = State#state.listen,
    case lists:keysearch(Pid, ?ACCEPT_ID, Listen) of
	{value, ListenR} ->
	    ListenS = ListenR#listen.listen,
	    Mod = ListenR#listen.module,
            verbose({accept_exit, Pid, Reason, Mod}, 2, State),
	    AcceptPid = Mod:accept(ListenS),
	    L = lists:keyreplace(Pid, ?ACCEPT_ID, Listen,
				 ListenR#listen{accept = AcceptPid}),
	    throw({noreply, State#state{listen = L}});
	_ ->
	    false
    end.

conn_own_exit(Pid, Reason, #state{conn_owners = Owners} = State) ->
    case maps:get(Pid, Owners, undefined) of
        undefined -> false;
        Node ->
            verbose({conn_own_exit, Pid, Reason, Node}, 2, State),
            throw({noreply, nodedown(Pid, Node, Reason, State)})
    end.

dist_ctrlr_exit(Pid, Reason, #state{dist_ctrlrs = DCs} = State) ->
    case maps:get(Pid, DCs, undefined) of
        undefined -> false;
        Node ->
            verbose({dist_ctrlr_exit, Pid, Reason, Node}, 2, State),
            throw({noreply, nodedown(Pid, Node, Reason, State)})
    end.

pending_own_exit(Pid, Reason, #state{pend_owners = Pend} = State) ->
    case maps:get(Pid, Pend, undefined) of
        undefined ->
            false;
        Node ->
	    State1 = State#state { pend_owners = maps:remove(Pid, Pend)},
	    case get_conn(Node) of
		{ok, Conn} when Conn#connection.state =:= up_pending ->
                    verbose(
                      {pending_own_exit, Pid, Reason, Node, up_pending},
                      2, State),
		    reply_waiting(Node,Conn#connection.waiting, true),
		    Conn1 = Conn#connection { state = up,
					      waiting = [],
					      pending_owner = undefined },
		    ets:insert(sys_dist, Conn1);
		_ ->
                    verbose({pending_own_exit, Pid, Reason, Node}, 2, State),
		    ok
	    end,
	    throw({noreply, State1})
    end.

ticker_exit(
  Pid, Reason,
  #state{tick = #tick{ticker = Pid, time = T} = Tck} = State) ->
    verbose({ticker_exit, Pid, Reason, Tck}, 2, State),
    Tckr = restart_ticker(T),
    throw({noreply, State#state{tick = Tck#tick{ticker = Tckr}}});
ticker_exit(
  Pid, Reason,
  #state{tick = #tick_change{ticker = Pid, time = T} = TckCng} = State) ->
    verbose({ticker_exit, Pid, Reason, TckCng}, 2, State),
    Tckr = restart_ticker(T),
    throw({noreply, Reason, State#state{tick = TckCng#tick_change{ticker = Tckr}}});
ticker_exit(_, _, _) ->
    false.

restarter_exit(Pid, Reason, State) ->
    case State#state.supervisor of
        {restart, Pid} ->
            verbose({restarter_exit, Pid, Reason}, 2, State),
	    error_msg(
              "** Distribution restart failed, net_kernel terminating... **\n",
              []),
	    throw({stop, restarter_exit, State});
        _ ->
            false
    end.


%% -----------------------------------------------------------
%% A node has gone down !!
%% nodedown(Owner, Node, Reason, State) -> State'
%% -----------------------------------------------------------

nodedown(Exited, Node, Reason, State) ->
    case get_conn(Node) of
	{ok, Conn} ->
	    nodedown(Conn, Exited, Node, Reason, Conn#connection.type, State);
	_ ->
	    State
    end.

get_conn(Node) ->
    case ets:lookup(sys_dist, Node) of
	[Conn = #connection{}] -> {ok, Conn};
	_      -> error
    end.

delete_owner(Owner, #state{conn_owners = Owners} = State) ->
    State#state{conn_owners = maps:remove(Owner, Owners)}.

delete_ctrlr(Ctrlr, #state{dist_ctrlrs = DCs} = State) ->
    State#state{dist_ctrlrs = maps:remove(Ctrlr, DCs)}.

nodedown(Conn, Exited, Node, Reason, Type, State) ->
    case Conn#connection.state of
	pending ->
	    pending_nodedown(Conn, Exited, Node, Type, State);
	up ->
	    up_nodedown(Conn, Exited, Node, Reason, Type, State);
	up_pending ->
	    up_pending_nodedown(Conn, Exited, Node, Reason, Type, State);
	_ ->
	    State
    end.

pending_nodedown(#connection{owner = Owner,
                             waiting = Waiting,
                             conn_id = CID} = Conn,
                 Exited, Node, Type, State0) when Owner =:= Exited ->
    %% Owner exited!
    State2 = case erts_internal:abort_pending_connection(Node, CID) of
                 false ->
                     %% Just got connected but that message has not
                     %% reached us yet. Wait for controller to exit and
                     %% handle this then...
                     State0;
                 true ->
                     %% Don't bar connections that have never been alive
                     State1 = delete_connection(Conn, false, State0),
                     reply_waiting(Node, Waiting, false),
                     case Type of
                         normal ->
                             ?nodedown(Node, State1);
                         _ ->
                             ok
                     end,
                     State1
    end,
    delete_owner(Owner, State2);
pending_nodedown(#connection{owner = Owner,
                             ctrlr = Ctrlr,
                             waiting = Waiting} = Conn,
                 Exited, Node, Type, State0) when Ctrlr =:= Exited ->
    %% Controller exited!
    %%
    %% Controller has been registered but crashed
    %% before sending mark up message...
    %%
    %% 'nodeup' messages has been sent by the emulator,
    %% so bar the connection...
    State1 = delete_connection(Conn, true, State0),
    reply_waiting(Node,Waiting, true),
    case Type of
        normal ->
            ?nodedown(Node, State1);
        _ ->
            ok
    end,
    delete_owner(Owner, delete_ctrlr(Ctrlr, State1));
pending_nodedown(_Conn, _Exited, _Node, _Type, State) ->
    State.

up_pending_nodedown(#connection{owner = Owner,
                                ctrlr = Ctrlr,
                                pending_owner = AcceptPid} = Conn,
                    Exited, Node, _Reason,
                    _Type, State) when Ctrlr =:= Exited  ->
    %% Controller exited!
    Conn1 = Conn#connection { owner = AcceptPid,
                              conn_id = erts_internal:new_connection(Node),
                              ctrlr = undefined,
			      pending_owner = undefined,
			      state = pending },
    ets:insert(sys_dist, Conn1),
    AcceptPid ! {self(), pending},
    Pend = maps:remove(AcceptPid, State#state.pend_owners),
    Owners = State#state.conn_owners,
    State1 = State#state{conn_owners = Owners#{AcceptPid => Node},
                         pend_owners = Pend},
    delete_owner(Owner, delete_ctrlr(Ctrlr, State1));
up_pending_nodedown(#connection{owner = Owner},
                    Exited, _Node, _Reason,
                    _Type, State) when Owner =:= Exited  ->
    %% Owner exited!
    delete_owner(Owner, State);
up_pending_nodedown(_Conn, _Exited, _Node, _Reason, _Type, State) ->
    State.

up_nodedown(#connection{owner = Owner,
                        ctrlr = Ctrlr} = Conn,
            Exited, Node, _Reason, Type, State0) when Ctrlr =:= Exited ->
    %% Controller exited!
    State1 = delete_connection(Conn, true, State0),
    case Type of
	normal -> ?nodedown(Node, State1);
	_ -> ok
    end,
    delete_owner(Owner, delete_ctrlr(Ctrlr, State1));
up_nodedown(#connection{owner = Owner},
            Exited, _Node, _Reason,
            _Type, State) when Owner =:= Exited  ->
    %% Owner exited!
    delete_owner(Owner, State);
up_nodedown(_Conn, _Exited, _Node, _Reason, _Type, State) ->
    State.

delete_connection(#connection{named_me = true}, _, State) ->
    restart_distr(State);

delete_connection(#connection{node = Node}=Conn, MayBeBarred, State) ->
    BarrIt = MayBeBarred andalso
        case application:get_env(kernel, dist_auto_connect) of
            {ok, once} ->
                true;
            _ ->
                false
        end,
    case BarrIt of
        true ->
	    ets:insert(sys_dist, #barred_connection{node = Node});
	_ ->
	    ets:delete(sys_dist, Node)
    end,
    case Conn#connection.remote_name_type of
        dynamic ->
            %% Return remote node name to pool
            [_Name,Host] = string:split(atom_to_list(Node), "@", all),
            NamePool0 = State#state.dyn_name_pool,
            DynNames = maps:get(Host, NamePool0, []),
            false = lists:keyfind(Node, 1, DynNames), % ASSERT
            FreeName = {Node, next_creation(Conn#connection.creation)},
            NamePool1 = NamePool0#{Host => [FreeName | DynNames]},
            State#state{dyn_name_pool = NamePool1};

        static ->
            State
    end.

restart_distr(State) ->
    Restarter = spawn_link(fun() -> restart_distr_do(State#state.supervisor) end),
    State#state{supervisor = {restart, Restarter}}.

restart_distr_do(NetSup) ->
    process_flag(trap_exit,true),
    ok = supervisor:terminate_child(kernel_sup, NetSup),
    case supervisor:restart_child(kernel_sup, NetSup) of
        {ok, Pid} when is_pid(Pid) ->
            ok
    end.

%% -----------------------------------------------------------
%% monitor_nodes/[1,2] errors
%% -----------------------------------------------------------

check_opt(Opt, Opts) ->
    check_opt(Opt, Opts, false, []).

check_opt(_Opt, [], false, _OtherOpts) ->
    false;
check_opt(_Opt, [], {true, ORes}, OtherOpts) ->
    {true, ORes, OtherOpts};
check_opt(Opt, [Opt|RestOpts], false, OtherOpts) ->
    check_opt(Opt, RestOpts, {true, Opt}, OtherOpts);
check_opt(Opt, [Opt|RestOpts], {true, Opt} = ORes, OtherOpts) ->
    check_opt(Opt, RestOpts, ORes, OtherOpts);
check_opt({Opt, value}=TOpt,
	  [{Opt, _Val}=ORes|RestOpts],
	  false,
	  OtherOpts) ->
    check_opt(TOpt, RestOpts, {true, ORes}, OtherOpts);
check_opt({Opt, value}=TOpt,
	  [{Opt, _Val}=ORes|RestOpts],
	  {true, ORes}=TORes,
	  OtherOpts) ->
    check_opt(TOpt, RestOpts, TORes, OtherOpts);
check_opt({Opt, value},
	  [{Opt, _Val} = ORes1| _RestOpts],
	  {true, {Opt, _OtherVal} = ORes2},
	  _OtherOpts) ->
    throw({error, {option_value_mismatch, [ORes1, ORes2]}});
check_opt(Opt, [OtherOpt | RestOpts], TORes, OtherOpts) ->
    check_opt(Opt, RestOpts, TORes, [OtherOpt | OtherOpts]).

check_options(Opts) when is_list(Opts) ->
    RestOpts1 = case check_opt({node_type, value}, Opts) of
		    {true, {node_type,Type}, RO1} when Type =:= visible;
						       Type =:= hidden;
						       Type =:= all ->
			RO1;
		    {true, {node_type, _Type} = Opt, _RO1} ->
			throw({error, {bad_option_value, Opt}});
		    false ->
			Opts
		end,
    RestOpts2 = case check_opt(nodedown_reason, RestOpts1) of
		    {true, nodedown_reason, RO2} ->
			RO2;
		    false ->
			RestOpts1
		end,
    case RestOpts2 of
	[] ->
	    %% This should never happen since we only call this function
	    %% when we know there is an error in the option list
	    {error, internal_error};
	_ ->
	    {error, {unknown_options, RestOpts2}}
    end;
check_options(Opts) when is_map(Opts) ->
    BadMap0 = case maps:find(connection_id, Opts) of
                  error ->
                      Opts;
                  {ok, CIdBool} when is_boolean(CIdBool) ->
                      maps:remove(connection_id, Opts);
                   {ok, BadCIdVal} ->
                      throw({error,
                             {bad_option_value,
                              #{connection_id => BadCIdVal}}})
              end,
    BadMap1 = case maps:find(nodedown_reason, BadMap0) of
                  error ->
                      BadMap0;
                  {ok, NRBool} when is_boolean(NRBool) ->
                      maps:remove(nodedown_reason, BadMap0);
                  {ok, BadNRVal} ->
                      throw({error,
                             {bad_option_value,
                              #{nodedown_reason => BadNRVal}}})
              end,
    BadMap2 = case maps:find(node_type, BadMap1) of
                  error ->
                      BadMap1;
                  {ok, NTVal} when NTVal == visible; NTVal == hidden; NTVal == all ->
                      maps:remove(node_type, BadMap1);
                  {ok, BadNTVal} ->
                      throw({error,
                             {bad_option_value,
                              #{node_type => BadNTVal}}})
              end,
    if map_size(BadMap2) == 0 ->
	    {error, internal_error};
       true ->
            throw({error, {unknown_options, BadMap2}})
    end;
check_options(Opts) ->
    {error, {invalid_options, Opts}}.
    

mk_monitor_nodes_error(Flag, _Opts) when Flag =/= true, Flag =/= false ->
    error;
mk_monitor_nodes_error(_Flag, Opts) ->
    case catch check_options(Opts) of
	{error, _} = Error ->
	    Error;
	UnexpectedError ->
	    {error, {internal_error, UnexpectedError}}
    end.

% -------------------------------------------------------------

do_disconnect(Node, State, Async) ->
    case ets:lookup(sys_dist, Node) of
	[Conn] when Conn#connection.state =:= up ->
	    disconnect_ctrlr(Conn#connection.ctrlr, State, Async);
	[Conn] when Conn#connection.state =:= up_pending ->
	    disconnect_ctrlr(Conn#connection.ctrlr, State, Async);
	_ ->
	    {false, State}
    end.

disconnect_ctrlr(Ctrlr, S0, Async) ->
    exit(Ctrlr, disconnect),
    S2 = case Async of
             true ->
                 S0;
             false ->
                 receive
                     {'EXIT',Ctrlr,Reason} ->
                         {_,S1} = handle_exit(Ctrlr, Reason, S0),
                         S1
                 end
         end,
    {true, S2}.


%%
%%
%%

%% Return a list of all nodes that are 'up' and not hidden.
get_nodes_up_normal() ->
    ets:select(sys_dist, [{#connection{node = '$1',
                                       state = up,
                                       type = normal,
                                       _ = '_'},
                           [],
                           ['$1']}]).

-doc false.
ticker(Kernel, Tick) when is_integer(Tick) ->
    process_flag(priority, max),
    ?tckr_dbg(ticker_started),
    ticker_loop(Kernel, Tick).

-doc false.
ticker_loop(Kernel, Tick) ->
    receive
	{new_ticktime, NewTick} ->
	    ?tckr_dbg({ticker_changed_time, Tick, NewTick}),
	    ?MODULE:ticker_loop(Kernel, NewTick)
    after Tick ->
	    Kernel ! tick,
	    ?MODULE:ticker_loop(Kernel, Tick)
    end.

start_aux_ticker(NewTick, OldTick, TransitionPeriod) ->
    spawn_link(?MODULE, aux_ticker,
	       [self(), NewTick, OldTick, TransitionPeriod]).

-doc false.
aux_ticker(NetKernel, NewTick, OldTick, TransitionPeriod) ->
    process_flag(priority, max),
    ?tckr_dbg(aux_ticker_started),
    TickInterval = case NewTick > OldTick of
		       true  -> OldTick;
		       false -> NewTick
		   end,
    NoOfTicks = case TransitionPeriod > 0 of
		    true ->
			%% 1 tick to start
			%% + ticks to cover the transition period
			1 + (((TransitionPeriod - 1) div TickInterval) + 1);
		    false ->
			1
		end,
    aux_ticker1(NetKernel, TickInterval, NoOfTicks).

aux_ticker1(NetKernel, _, 1) ->
    NetKernel ! transition_period_end,
    NetKernel ! aux_tick,
    bye;
aux_ticker1(NetKernel, TickInterval, NoOfTicks) ->
    NetKernel ! aux_tick,
    receive
    after TickInterval ->
	    aux_ticker1(NetKernel, TickInterval, NoOfTicks-1)
    end.

send(_From,To,Mess) ->
    case whereis(To) of
	undefined ->
	    Mess;
	P when is_pid(P) ->
	    P ! Mess
    end.

-ifdef(UNUSED).

safesend(Name,Mess) when is_atom(Name) ->
    case whereis(Name) of
	undefined ->
	    Mess;
	P when is_pid(P) ->
	    P ! Mess
    end;
safesend(Pid, Mess) -> Pid ! Mess.

-endif.

-doc false.
do_spawn(SpawnFuncArgs, SpawnOpts, State) ->
    [_,From|_] = SpawnFuncArgs,
    case catch spawn_opt(?MODULE, spawn_func, SpawnFuncArgs, SpawnOpts) of
	{'EXIT', {Reason,_}} ->
            async_reply({reply, {'EXIT', {Reason,[]}}, State}, From);
	{'EXIT', Reason} ->
	    async_reply({reply, {'EXIT', {Reason,[]}}, State}, From);
	_ ->
	    {noreply,State}
    end.

%% This code is really intricate. The link will go first and then comes
%% the pid, This means that the client need not do a network link.
%% If the link message would not arrive, the runtime system shall
%% generate a nodedown message

-doc false.
spawn_func(link,{From,Tag},M,F,A,Gleader) ->
    link(From),
    gen_server:reply({From,Tag},self()),  %% ahhh
    group_leader(Gleader,self()),
    apply(M,F,A);
spawn_func(_,{From,Tag},M,F,A,Gleader) ->
    gen_server:reply({From,Tag},self()),  %% ahhh
    group_leader(Gleader,self()),
    apply(M,F,A).

%% -----------------------------------------------------------
%% Set up connection to a new node.
%% -----------------------------------------------------------

setup(Node, ConnId, Type, From, State) ->
    case setup_check(Node, State) of
		{ok, L} ->
		    Mod = L#listen.module,
		    LAddr = L#listen.address,
		    MyNode = State#state.node,
		    Pid = Mod:setup(Node,
				    Type,
				    MyNode,
				    State#state.type,
				    State#state.connecttime),
                    verbose(
                      {setup,Node,Type,MyNode,State#state.type,Pid},
                      2, State),
		    Addr = LAddr#net_address {
					      address = undefined,
					      host = undefined },
                    Waiting = case From of
                                  noreply -> [];
                                  _ -> [From]
                              end,
		    ets:insert(sys_dist, #connection{node = Node,
                                                     conn_id = ConnId,
						     state = pending,
						     owner = Pid,
						     waiting = Waiting,
						     address = Addr,
						     type = normal,
                                                     remote_name_type = static}),
		    {ok, Pid};
		Error ->
		    Error
    end.

setup_check(Node, State) ->
    Allowed = State#state.allowed,
    case lists:member(Node, Allowed) of
	false when Allowed =/= [] ->
	    error_msg("** Connection attempt with "
		      "disallowed node ~w ** ~n", [Node]),
	    {error, bad_node};
       _ ->
            case select_mod(Node, State#state.listen) of
                {ok, _L}=OK -> OK;
                Error -> Error
            end
    end.

%%
%% Find a module that is willing to handle connection setup to Node
%%
select_mod(Node, [L|Ls]) ->
    Mod = L#listen.module,
    case Mod:select(Node) of
	true -> {ok, L};
	false -> select_mod(Node, Ls)
    end;
select_mod(Node, []) ->
    {error, {unsupported_address_type, Node}}.

get_proto_mod(Family,Protocol,[L|Ls]) ->
    A = L#listen.address,
    if  L#listen.accept =/= undefined,
        A#net_address.family =:= Family,
       A#net_address.protocol =:= Protocol ->
	    {ok, L#listen.module};
       true ->
	    get_proto_mod(Family,Protocol,Ls)
    end;
get_proto_mod(_Family, _Protocol, []) ->
    error.

%% -------- Initialisation functions ------------------------

init_node(Name, LongOrShortNames, CleanHalt, Listen) ->
    case create_name(Name, LongOrShortNames, 1) of
	{ok,Node} ->
	    case start_protos(Node, CleanHalt, Listen) of
		{ok, Ls} ->
		    {ok, Node, Ls};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

%% Create the node name
create_name(Name, LongOrShortNames, Try) ->
    put(longnames, case LongOrShortNames of
		       shortnames -> false;
		       longnames -> true
		   end),
    {Head,Host1} = create_hostpart(Name, LongOrShortNames),
    case Host1 of
	{ok,HostPart} ->
            case valid_name_head(Head) of
                true ->
                    {ok,list_to_atom(Head ++ HostPart)};
                false ->
                    error_logger:info_msg("Invalid node name!\n"
                                          "Please check your configuration\n"),
                    {error, badarg}
            end;
	{error,long} when Try =:= 1 ->
	    %% It could be we haven't read domain name from resolv file yet
	    inet_config:do_load_resolv(os:type(), longnames),
	    create_name(Name, LongOrShortNames, 0);
        {error, hostname_not_allowed} ->
            error_logger:info_msg("Invalid node name!\n"
                                  "Please check your configuration\n"),
            {error, badarg};
	{error,Type} ->
	    error_logger:info_msg(
	      lists:concat(["Can't set ", Type, " node name!\n"
			    "Please check your configuration\n"])),
	    {error,badarg}
    end.

create_hostpart(Name, LongOrShortNames) ->
    {Head,Host} = split_node(Name),
    Host1 = case {Host,LongOrShortNames} of
		{[$@,_|_] = Host,longnames} ->
                    validate_hostname(Host);
		{[$@,_|_],shortnames} ->
		    case lists:member($.,Host) of
			true -> {error,short};
			_ ->
                            validate_hostname(Host)
		    end;
		{_,shortnames} ->
		    case inet_db:gethostname() of
			H when is_list(H), length(H)>0 ->
			    {ok,"@" ++ H};
			_ ->
			    {error,short}
		    end;
		{_,longnames} ->
		    case {inet_db:gethostname(),inet_db:res_option(domain)} of
			{H,D} when is_list(D), is_list(H),
                        length(D)> 0, length(H)>0 ->
			    {ok,"@" ++ H ++ "." ++ D};
			_ ->
			    {error,long}
		    end
	    end,
    {Head,Host1}.

validate_hostname([$@|HostPart] = Host) ->
    {ok, MP} = re:compile("^[!-ÿ]*$", [unicode]),
    case re:run(HostPart, MP) of
        {match, _} ->
            {ok, Host};
        nomatch ->
            {error, hostname_not_allowed}
    end.

valid_name_head(Head) ->
    {ok, MP} = re:compile("^[0-9A-Za-z_\\-]+$", [unicode]),
        case re:run(Head, MP) of
            {match, _} ->
                true;
            nomatch ->
                false
    end.

split_node(Name) ->
    lists:splitwith(fun(C) -> C =/= $@ end, atom_to_list(Name)).

%%
%%
%%
-doc false.
protocol_childspecs() ->
    Protos = proto_dist_argument(),
    protocol_childspecs(Protos).

protocol_childspecs([]) ->
    [];
protocol_childspecs([H|T]) ->
    Mod = list_to_atom(H ++ "_dist"),
    case (catch Mod:childspecs()) of
	{ok, Childspecs} when is_list(Childspecs) ->
	    Childspecs ++ protocol_childspecs(T);
	_ ->
	    protocol_childspecs(T)
    end.

%%
%% epmd_module argument -> module_name of erl_epmd or similar gen_server_module.
%%

-doc false.
epmd_module() ->
    ModuleParameterResult =
        case application:get_env(kernel, epmd_module) of
            {ok, Module} when is_atom(Module) ->
                {ok, Module};
            {ok, Invalid} ->
                error({invalid_parameter_value, epmd_module, Invalid});
            undefined ->
                undefined
        end,
    ModuleArgumentResult =
        case init:get_argument(epmd_module) of
            {ok,[[Mod | _] | _]} ->
                {ok, list_to_atom(Mod)};
            _ ->
                undefined
        end,
    case {ModuleParameterResult, ModuleArgumentResult} of
        {undefined, undefined} -> erl_epmd;
        {{ok, ModuleParameter}, undefined} -> ModuleParameter;
        {undefined, {ok, ModuleArgument}} -> ModuleArgument;
        _ -> error({invalid_configuration, "either -epmd_module or kernel epmd_module should be specified, not both"})
    end.

%%
%% -dist_listen argument -> whether the erlang distribution should listen for connections
%%

dist_listen_argument() ->
    case init:get_argument(dist_listen) of
        {ok,[["false" | _] | _]} ->
            false;
        _ ->
            true
    end.

%%%
%%% -hidden command line argument
%%%

hidden_argument() ->
    case init:get_argument(hidden) of
        {ok,[[] | _]} ->
            true;
        {ok,[["true" | _] | _]} ->
            true;
        _ ->
            false
    end.

%%%
%%% -proto_dist command line argument
%%%

proto_dist_argument() ->
    case init:get_argument(proto_dist) of
        {ok, [Protos | Rest]} ->
            case Rest of
                [] ->
                    ok;
                _ ->
                    ?LOG_WARNING("Multiple -proto_dist given to erl, using the first, ~p", [Protos])
            end,
            Protos;
        _ ->
            ["inet_tcp"]
    end.

%%
%% Start all protocols
%%

start_protos(Node, CleanHalt, Listen) ->
    Protos = proto_dist_argument(),
    start_protos(Node, Protos, CleanHalt, Listen).

start_protos(Node, Ps, CleanHalt, Listen) ->
    Listeners = case Listen of
                    false -> start_protos_no_listen(Node, Ps, [], CleanHalt);
                    _ -> start_protos_listen(Node, Ps, CleanHalt)
                end,
    case Listeners of
	[] ->
	    case CleanHalt of
		true -> halt(1);
		false -> {error, badarg}
	    end;
	Ls ->
	    {ok, Ls}
    end.

start_protos_no_listen(Node, [Proto | Ps], Ls, CleanHalt) ->
    {Name, "@"++Host}  = split_node(Node),
    Ok = case Name of
             "undefined" ->
                 erts_internal:dynamic_node_name(true),
                 true;
             _ ->
                 (set_node(Node, create_creation()) =:= ok)
         end,
    case Ok of
        true ->
            auth:sync_cookie(),
            Mod = list_to_atom(Proto ++ "_dist"),
            Address =
                try Mod:address(Host)
                catch error:undef ->
                        Mod:address()
                end,
            L = #listen {
                   listen = undefined,
                   address = Address,
                   accept = undefined,
                   module = Mod },
            start_protos_no_listen(Node, Ps, [L|Ls], CleanHalt);
        false ->
            S = "invalid node name: " ++ atom_to_list(Node),
            proto_error(CleanHalt, Proto, S),
            start_protos_no_listen(Node, Ps, Ls, CleanHalt)
    end;
start_protos_no_listen(_Node, [], Ls, _CleanHalt) ->
    Ls.

create_creation() ->
    Cr = try binary:decode_unsigned(crypto:strong_rand_bytes(4)) of
             Creation ->
                 Creation
         catch _:_ ->
                 rand:uniform((1 bsl 32)-1)
         end,
    wrap_creation(Cr).

next_creation(Creation) ->
    wrap_creation(Creation + 1).

%% Avoid small creations 0,1,2,3
wrap_creation(Cr) when Cr >= 4 andalso Cr < (1 bsl 32) ->
    Cr;
wrap_creation(Cr) ->
    wrap_creation((Cr + 4) band ((1 bsl 32) - 1)).
    

start_protos_listen(Node, Ps, CleanHalt) ->
    case split_node(Node) of
        {"undefined", _} ->
            error({internal_error, "Dynamic node name and dist listen both enabled"});
        {Name, "@"++Host} ->
            start_protos_listen(list_to_atom(Name), Host, Node, Ps, [], CleanHalt)
    end.
start_protos_listen(Name, Host, Node, [Proto | Ps], Ls, CleanHalt) ->
    Mod = list_to_atom(Proto ++ "_dist"),
    try try Mod:listen(Name,Host)
        catch error:undef ->
                Mod:listen(Name)
        end of
        {ok, {Socket, Address, Creation}} ->
            case set_node(Node, Creation) of
                ok ->
                    AcceptPid = Mod:accept(Socket),
                    auth:sync_cookie(),
                    L = #listen{
                           listen = Socket,
                           address = Address,
                           accept = AcceptPid,
                           module = Mod },
                    start_protos_listen(Name, Host, Node, Ps, [L|Ls], CleanHalt);
                _ ->
                    Mod:close(Socket),
                    S = "invalid node name: " ++ atom_to_list(Node),
                    proto_error(CleanHalt, Proto, S),
                    start_protos_listen(Name, Host, Node, Ps, Ls, CleanHalt)
            end;
        {error, duplicate_name} ->
            S = "the name " ++ atom_to_list(Node) ++
                " seems to be in use by another Erlang node",
            proto_error(CleanHalt, Proto, S),
            start_protos_listen(Name, Host, Node, Ps, Ls, CleanHalt);
        {error, Reason} ->
            register_error(CleanHalt, Proto, Reason),
            start_protos_listen(Name, Host, Node, Ps, Ls, CleanHalt)
    catch error:undef ->
            proto_error(CleanHalt, Proto, "not supported"),
            start_protos_listen(Name, Host, Node, Ps, Ls, CleanHalt);
          _:Reason ->
            register_error(CleanHalt, Proto, Reason),
            start_protos_listen(Name, Host, Node, Ps, Ls, CleanHalt)
    end;
start_protos_listen(_Name, _Host, _Node, [], Ls, _CleanHalt) ->
    Ls.

register_error(false, Proto, Reason) ->
    S = io_lib:format("register/listen error: ~p", [Reason]),
    proto_error(false, Proto, lists:flatten(S));
register_error(true, Proto, Reason) ->
    S = "Protocol '" ++ Proto ++ "': register/listen error: ",
    erlang:display_string(stdout, S),
    erlang:display(Reason).

proto_error(CleanHalt, Proto, String) ->
    S = "Protocol '" ++ Proto ++ "': " ++ String ++ "\n",
    case CleanHalt of
	false ->
	    error_logger:info_msg(S);
	true ->
	    erlang:display_string(S)
    end.

set_node(Node, Creation) when Creation < 0 ->
    set_node(Node, create_creation());
set_node(Node, Creation) when node() =:= nonode@nohost ->
    case catch erlang:setnode(Node, Creation) of
        true ->
            ok;
        {'EXIT',Reason} ->
            {error,Reason}
    end;
set_node(Node, _Creation) when node() =:= Node ->
    ok.

-doc false.
connecttime() ->
    case application:get_env(kernel, net_setuptime) of
	{ok,Time} when is_number(Time), Time >= 120 ->
	    120 * 1000;
	{ok,Time} when is_number(Time), Time > 0 ->
	    round(Time * 1000);
	_ ->
	    ?SETUPTIME
    end.

%% -------- End initialisation functions --------------------

%% ------------------------------------------------------------
%% Node information.
%% ------------------------------------------------------------

get_node_info(Node) ->
    case ets:lookup(sys_dist, Node) of
        [#connection{owner = Owner, state = up, address = Addr, type = Type}] ->
            MRef = monitor(process, Owner),
            Owner ! {self(), get_status},
            receive
                {Owner, get_status, {ok, Read, Write}} ->
                    demonitor(MRef, [flush]),
                    {ok, [{owner, Owner}, {state, up}, {address, Addr},
                        {type, Type}, {in, Read}, {out, Write}]};
                {'DOWN', MRef, process, Owner, _Info} ->
                    {error, bad_node}
            end;
        [#connection{owner = Owner, state = State, address = Addr, type = Type}] ->
            {ok, [{owner, Owner}, {state, State}, {address, Addr},
                  {type, Type}, {in, 0}, {out, 0}]};
        _ ->
            {error, bad_node}
    end.

get_node_info(Node, Key) ->
    case get_node_info(Node) of
        {ok, Info} ->
            case lists:keyfind(Key, 1, Info) of
                {Key, Value} ->
                    {ok, Value};
                false ->
                    {error, invalid_key}
            end;
        {error, bad_node} ->
            {error, bad_node}
    end.


get_nodes_info() ->
    Conns = ets:select(sys_dist, [{#connection{_ = '_'}, [], ['$_']}]),
    Info = multi_info(Conns, {self(), get_status}, #{}, []),
    {ok, Info}.

multi_info([], _Msg, PidToRef, NodeInfos) ->
    multi_receive(PidToRef, NodeInfos);
multi_info([#connection{owner = Owner, state = up} = Conn | Conns], Msg, PidToRef, NodeInfos) ->
    % connection is up, try to figure out in/out bytes
    MRef = erlang:monitor(process, Owner),
    Owner ! Msg,
    multi_info(Conns, Msg, maps:put(Owner, {MRef, Conn}, PidToRef), NodeInfos);
multi_info([#connection{node = Node, owner = Owner, type = Type,
    state = State, address = Addr} | Conns], Msg, PidToRef, NodeInfos) ->
    % connection is not up: in/out bytes are zero
    multi_info(Conns, Msg, PidToRef, [
        {Node, [{owner, Owner}, {state, State}, {address, Addr}, {type, Type}, {in, 0}, {out, 0}]}
        | NodeInfos]).

multi_receive(PidToRef, NodeInfos) when map_size(PidToRef) =:= 0 ->
    NodeInfos;
multi_receive(PidToRef, NodeInfos) ->
    receive
        {DistProc, get_status, {ok, Read, Write}} ->
            {{MRef, #connection{node = Node, owner = Owner, type = Type,
                state = State, address = Addr}}, NewRefs} = maps:take(DistProc, PidToRef),
            erlang:demonitor(MRef, [flush]),
            multi_receive(NewRefs, [
                {Node, [{owner, Owner}, {state, State}, {address, Addr}, {type, Type}, {in, Read}, {out, Write}]}
                | NodeInfos]);
        {'DOWN', _MRef, process, Pid, _Info} ->
            % connection went down: reproducing compatible behaviour with
            %   not showing any information about this connection
            multi_receive(maps:remove(Pid, PidToRef), NodeInfos)
    end.

%% ------------------------------------------------------------
%% Misc. functions
%% ------------------------------------------------------------

reply_waiting(_Node, Waiting, Rep) ->
    case Rep of
	false ->
	    ?connect_failure(_Node, {setup_process, failure});
	_ ->
	    ok
    end,
    reply_waiting1(lists:reverse(Waiting), Rep).

reply_waiting1([From|W], Rep) ->
    gen_server:reply(From, Rep),
    reply_waiting1(W, Rep);
reply_waiting1([], _) ->
    ok.

-ifdef(UNUSED).

delete_all(From, [From |Tail]) -> delete_all(From, Tail);
delete_all(From, [H|Tail]) ->  [H|delete_all(From, Tail)];
delete_all(_, []) -> [].

-endif.

all_atoms([]) -> true;
all_atoms([N|Tail]) when is_atom(N) ->
    all_atoms(Tail);
all_atoms(_) -> false.

%% It is assumed that only net_kernel uses restart_ticker()
restart_ticker(Time) ->
    ?tckr_dbg(restarting_ticker),
    self() ! aux_tick,
    spawn_link(?MODULE, ticker, [self(), Time]).

%% ------------------------------------------------------------
%% Print status information.
%% ------------------------------------------------------------

print_info() ->
    nformat("Node", "State", "Type", "In", "Out", "Address"),
    {ok, NodesInfo} = nodes_info(),
    {In,Out} = lists:foldl(fun display_info/2, {0,0}, NodesInfo),
    nformat("Total", "", "",
	    integer_to_list(In), integer_to_list(Out), "").

display_info({Node, Info}, {I,O}) ->
    State = atom_to_list(fetch(state, Info)),
    In = fetch(in, Info),
    Out = fetch(out, Info),
    Type = atom_to_list(fetch(type, Info)),
    Address = fmt_address(fetch(address, Info)),
    nformat(atom_to_list(Node), State, Type,
	    integer_to_list(In), integer_to_list(Out), Address),
    {I+In,O+Out}.

fmt_address(undefined) ->
    "-";
fmt_address(A) ->
    case A#net_address.family of
	inet ->
	    case A#net_address.address of
		{IP,Port} ->
		    inet_parse:ntoa(IP) ++ ":" ++ integer_to_list(Port);
		_ -> "-"
	    end;
	inet6 ->
	    case A#net_address.address of
		{IP,Port} ->
		    inet_parse:ntoa(IP) ++ "/" ++ integer_to_list(Port);
		_ -> "-"
	    end;
	_ ->
	    lists:flatten(io_lib:format("~p", [A#net_address.address]))
    end.

fetch(Key, Info) ->
    case lists:keysearch(Key, 1, Info) of
	{value, {_, Val}} -> Val;
	false -> 0
    end.

nformat(A1, A2, A3, A4, A5, A6) ->
    io:format("~-20s ~-7s ~-6s ~8s ~8s ~s~n", [A1,A2,A3,A4,A5,A6]).

print_info(Node) ->
    case node_info(Node) of
	{ok, Info} ->
	    State = fetch(state, Info),
	    In = fetch(in, Info),
	    Out = fetch(out, Info),
	    Type = fetch(type, Info),
	    Address = fmt_address(fetch(address, Info)),
	    io:format("Node     = ~p~n"
		      "State    = ~p~n"
		      "Type     = ~p~n"
		      "In       = ~p~n"
		      "Out      = ~p~n"
		      "Address  = ~s~n",
		      [Node, State, Type, In, Out, Address]);
	Error ->
	    Error
    end.

verbose(Term, Level, #state{verbose = Verbose}) when Verbose >= Level ->
    error_logger:info_report({net_kernel, Term});
verbose(_, _, _) ->
    ok.

getnode(P) when is_pid(P) -> node(P);
getnode(P) -> P.

return_call({noreply, _State}=R, _From) ->
    R;
return_call(R, From) ->
    async_reply(R, From).

-compile({inline, [async_reply/2]}).
async_reply({reply, _Msg, _State} = Res, _From) ->
    %% This function call is kept in order to not unnecessarily create a huge diff
    %% in the code.
    %%
    %% Here we used to send the reply explicitly using 'noconnect' and 'nosuspend'.
    %%
    %% * 'noconnect' since setting up a connection from net_kernel itself would
    %%   deadlock when connects were synchronous. Since connects nowadays are
    %%   asynchronous this is no longer an issue.
    %% * 'nosuspend' and spawn a process taking care of the reply in case
    %%   we would have suspended. This in order not to block net_kernel. We now
    %%   use 'async_dist' enabled and by this prevent the blocking, keep the
    %%   signal order, avoid one extra copying of the reply, avoid the overhead
    %%   of creating a process, and avoid the extra memory consumption due to the
    %%   extra process.
    Res.

handle_async_response(ResponseType, ReqId, Result, #state{req_map = ReqMap0} = S0) ->
    if ResponseType == down -> ok;
       true -> _ = erlang:demonitor(ReqId, [flush]), ok
    end,
    case maps:take(ReqId, ReqMap0) of

        {{SetGetOpts, From}, ReqMap1} when SetGetOpts == setopts;
                                           SetGetOpts == getopts ->
            Reply = case ResponseType of
                        reply -> Result;
                        down -> {error, noconnection}
                    end,
            gen_server:reply(From, Reply),
            {noreply, S0#state{req_map = ReqMap1}};

        {{setopts_new, Op}, ReqMap1} ->
            case maps:get(Op, ReqMap1) of
                {setopts_new, From, 1} ->
                    %% Last response for this operation...
                    gen_server:reply(From, ok),
                    ReqMap2 = maps:remove(Op, ReqMap1),
                    {noreply, S0#state{req_map = ReqMap2}};
                {setopts_new, From, N} ->
                    ReqMap2 = ReqMap1#{Op => {setopts_new, From, N-1}},
                    {noreply, S0#state{req_map = ReqMap2}}
            end
    end.

send_owner_request(ReqOpMap, Label, Owner, Msg) ->
    ReqId = monitor(process, Owner),
    Owner ! {self(), ReqId, Msg},
    ReqOpMap#{ReqId => Label}.

-doc """
Set one or more options for distribution sockets. Argument `Node` can be either
one node name or the atom `new` to affect the distribution sockets of all future
connected nodes.

The return value is the same as from `inet:setopts/2` or `{error, noconnection}`
if `Node` is not a connected node or `new`.

If `Node` is `new` the `Options` will then also be added to kernel configuration
parameters [inet_dist_listen_options](kernel_app.md#inet_dist_listen_options)
and [inet_dist_connect_options](kernel_app.md#inet_dist_connect_options).

Returns `ignored` if the local node is not alive.
""".
-doc(#{since => <<"OTP 19.1">>}).
-spec setopts(Node, Options) -> ok | {error, Reason} | ignored when
      Node :: node() | new,
      Options :: [inet:socket_setopt()],
      Reason :: inet:posix() | noconnection.

setopts(Node, Opts) when is_atom(Node), is_list(Opts) ->
    request({setopts, Node, Opts}).

setopts_new(Opts, From, State) ->
    %% First try setopts on listening socket(s)
    %% Bail out on failure.
    %% If successful, we are pretty sure Opts are ok
    %% and we continue with config params and pending connections.
    case setopts_on_listen(Opts, State#state.listen) of
	ok ->
	    setopts_new_1(Opts, From, State);
	Fail ->
            async_reply({reply, Fail, State}, From)
    end.

setopts_on_listen(_, []) -> ok;
setopts_on_listen(Opts, [#listen {listen = LSocket, module = Mod} | T]) ->
    try Mod:setopts(LSocket, Opts) of
	ok ->
	    setopts_on_listen(Opts, T);
	Fail -> Fail
    catch
	error:undef -> {error, enotsup}
    end.

setopts_new_1(Opts, From, #state{req_map = ReqMap0} = State) ->
    ConnectOpts = case application:get_env(kernel, inet_dist_connect_options) of
		      {ok, CO} -> CO;
		      _ -> []
		  end,
    application:set_env(kernel, inet_dist_connect_options,
			merge_opts(Opts,ConnectOpts)),
    ListenOpts = case application:get_env(kernel, inet_dist_listen_options) of
		     {ok, LO} -> LO;
		     _ -> []
		 end,
    application:set_env(kernel, inet_dist_listen_options,
			merge_opts(Opts, ListenOpts)),
    case lists:keyfind(nodelay, 1, Opts) of
	{nodelay, ND} when is_boolean(ND) ->
	    application:set_env(kernel, dist_nodelay, ND);
	_ -> ignore
    end,

    %% Update any pending connections
    PendingConns = ets:select(sys_dist, [{'_',
					  [{'=/=',{element,#connection.state,'$_'},up}],
					  ['$_']}]),

    Op = make_ref(),
    SendReq = fun (ReqMap, N, Owner) ->
                      {send_owner_request(ReqMap, {setopts_new, Op},
                                          Owner,
                                          {setopts, Opts}),
                       N+1}
              end,
    {ReqMap1, NoReqs} = lists:foldl(fun(#connection{state = pending,
                                                    owner = Owner},
                                        {ReqMap, N}) ->
                                            SendReq(ReqMap, N, Owner);
                                       (#connection{state = up_pending,
                                                    pending_owner = Owner},
                                        {ReqMap, N}) ->
                                            SendReq(ReqMap, N, Owner);
                                       (_, Acc) ->
                                            Acc
                                    end,
                                    {ReqMap0, 0},
                                    PendingConns),
    if NoReqs == 0 ->
            async_reply({reply, ok, State}, From);
       true ->
            %% Reply made later from handle_async_response() when
            %% we've got responses from all owners that we've
            %% made requests to...
            ReqMap2 = ReqMap1#{Op => {setopts_new, From, NoReqs}},
            {noreply, State#state{req_map = ReqMap2}}
    end.

merge_opts([], B) ->
    B;
merge_opts([H|T], B0) ->
    {Key, _} = H,
    B1 = lists:filter(fun({K,_}) -> K =/= Key end, B0),
    merge_opts(T, [H | B1]).

-doc """
Get one or more options for the distribution socket connected to `Node`.

If `Node` is a connected node the return value is the same as from
[`inet:getopts(Sock, Options)`](`inet:getopts/2`) where `Sock` is the
distribution socket for `Node`.

Returns `ignored` if the local node is not alive or `{error, noconnection}` if
`Node` is not connected.
""".
-doc(#{since => <<"OTP 19.1">>}).
-spec getopts(Node, Options) ->
	{'ok', OptionValues} | {'error', Reason} | ignored when
      Node :: node(),
      Options :: [inet:socket_getopt()],
      OptionValues :: [inet:socket_setopt()],
      Reason :: inet:posix() | noconnection.

getopts(Node, Opts) when is_atom(Node), is_list(Opts) ->
    request({getopts, Node, Opts}).

opts_node(Op, Node, Opts, From, #state{req_map = ReqMap0} = S0) ->
    case ets:lookup(sys_dist, Node) of
        [Conn] when Conn#connection.state =:= up ->
            ReqMap1 = send_owner_request(ReqMap0,
                                         {Op, From},
                                         Conn#connection.owner,
                                         {Op, Opts}),
            %% Reply made later from handle_async_response() when
            %% we get a response from the owner that we made the
            %% request to...
            S1 = S0#state{req_map = ReqMap1},
            {noreply, S1};
        _ ->
            async_reply({reply, {error, noconnection}, S0}, From)
    end.
