%%
%% Copyright WhatsApp Inc. and its affiliates. All rights reserved.
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

%% @doc
%% Controller for additional Erlang node running on the same host,
%%  or in a different container/host (e.g. Docker).
%%
%% == Terms ==
%% Origin node - Erlang VM instance that spawns additional nodes.
%% Peer node - a node spawned by the origin.
%% Control process - a process running on origin node, if it terminates,
%%  peer node terminates too.
%% Control connection - a connection between origin and peer, can be
%%  ether Erlang Distribution connection, or alternative one.
%%
%% I/O is forwarded from peer node to origin via control connection.
%%
%% When standard_io is used as alternative connection, peer node
%%  uses standard out to multiplex console output and control sequences.
%% Characters in range of 192-255 are reserved for control sequences,
%%  see encode_port_data for details. If peer node attempts to print
%%  characters in this range, an controlling process on the origin
%%  node may terminate with an error (because CRC check will fail).
%%
%% Alternative connection via TCP does not have that limitation, but
%%  it also does not redirect console I/O from the peer node.
%% @end
-module(peer).
-author("maximfca@gmail.com").

%% This mode has to be compilable on old nodes, so we ifdef out all
%% -doc attributes when compiling before 27.
-if(?OTP_RELEASE < 27).
-define(NO_DOCS, true).
-endif.

-ifndef(NO_DOCS).
-moduledoc({file, "../doc/src/peer.md"}).
-moduledoc(#{since => "OTP 25.0"}).
-endif.

%% API
-export([
         start_link/0,
         start_link/1,
         start/1,
         stop/1,

         random_name/0,
         random_name/1,

         get_state/1,

         call/4,
         call/5,
         cast/4,
         send/3
        ]).

-export_type([server_ref/0]).

-ifndef(NO_DOCS).
-doc "Identifies the controlling process of a peer node.".
-endif.
-type server_ref() :: % What stop, get_state, call, cast, send accepts
        pid().


%% Could be gen_statem too, with peer_state, but most interactions
%%  are anyway available in all states.
-behaviour(gen_server).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2
        ]).

%% Internal exports for stdin/stdout, non-distribution RPC, and tests
-export([
         start/0, %% this function must be named "start", requirement for user.erl

         %% Peer supervision...
         supervision_child_spec/0,
         start_supervision/0,
         init_supervision/2,
         system_continue/3,
         system_terminate/4,
         system_code_change/4,
         system_get_state/1,
         system_replace_state/2
        ]).

-behaviour(sys).

%% Origin node will listen to the specified port (port 0 is auto-select),
%%  or specified IP/Port, and expect peer node to connect to this port.
-ifndef(NO_DOCS).
-doc "
Alternative connection between the origin and the peer. When the connection
closes, the peer node terminates automatically.

If the `peer_down` startup flag is set to `crash`, the controlling process on
the origin node exits with corresponding reason, effectively providing a two-way link.

When `connection` is set to a port number, the origin starts listening on the
requested TCP port, and the peer node connects to the port. When it is set to an
`{IP, Port}` tuple, the origin listens only on the specified IP. The port number
can be set to 0 for automatic selection.

Using the `standard_io` alternative connection starts the peer attached to the
origin (other connections use `-detached` flag to erl). In this mode peer and
origin communicate via stdin/stdout.
".
-endif.
-type connection() ::
        Port :: 0..65535 |
                {inet:ip_address(), 0..65535} |
                standard_io.

%% Specification for boot waiting
-ifndef(NO_DOCS).
-doc "
Specifies start/start_link timeout in milliseconds. Can be set to `false`,
allowing the peer to start asynchronously. If `{Pid, Tag}` is specified instead
of a timeout, the peer will send `Tag` to the requested process.

The default is `15_000` ms.
".
-endif.
-type wait_boot() ::
        timeout() |                 %% wait for node to boot (default, 15 sec),
        {pid(), Tag :: term()} |    %% do not wait, send {Tag, {started, node(), pid()}} to Pid when node boots
        false.                      %% don't wait, don't notify

-ifndef(NO_DOCS).
-doc "
Overrides executable to start peer nodes with.

By default it is the path to \"erl\", taken from `init:get_argument(progname)`.
If `progname` is not known, `peer` makes best guess given the current ERTS version.

When a tuple is passed, the first element is the path to executable, and the
second element is prepended to the final command line. This can be used to start
peers on a remote host or in a Docker container. See the examples above.

This option is useful for testing backwards compatibility with previous
releases, installed at specific paths, or when the Erlang installation location
is missing from the `PATH`.
".
-endif.
-type exec() ::
        file:name() |               %% path to "erl" (default is init:get_argument(progname))
        {file:name(), [string()]}.  %% SSH support: {"/usr/bin/ssh", ["account@host_b", "/usr/bin/erl"]}

%% Peer node start options
-ifndef(NO_DOCS).
-doc "
Options that can be used when starting a `peer` node through `start/1` and
[`start_link/0,1`](`start_link/0`).

- **`name`** - Node name (the part before \"@\"). When `name` is not specified,
  but `host` is, `peer` follows compatibility behaviour and uses the origin node
  name.

- **`longnames`** - Use long names to start a node. Default is taken from the
  origin using `net_kernel:longnames()`. If the origin is not distributed, short
  names is the default.

- **`host`** - Enforces a specific host name. Can be used to override the
  default behaviour and start \"node@localhost\" instead of \"node@realhostname\".

- **`peer_down`** - Defines the peer control process behaviour when the control
  connection is closed from the peer node side (for example when the peer
  crashes or dumps core). When set to `stop` (default), a lost control
  connection causes the control process to exit normally. Setting `peer_down` to
  `continue` keeps the control process running, and `crash` will cause the
  controlling process to exit abnormally.

- **`connection`** - Alternative connection specification. See the
  [`connection` datatype](`t:connection/0`).

- **`exec`** - Alternative mechanism to start peer nodes with, for example, ssh
  instead of the default bash.

- **`detached`** - Defines whether to pass the `-detached` flag to the started
  peer. This option cannot be set to `false` using the `standard_io` alternative
  connection type. Default is `true`.

- **`args`** - Extra command line arguments to append to the \"erl\" command.
  Arguments are passed as is, no escaping or quoting is needed or accepted.

- **`post_process_args`** - Allows the user to change the arguments passed to
  `exec` before the peer is started. This can for example be useful when the
  `exec` program wants the arguments to \"erl\" as a single argument. Example:

  ```erlang
  peer:start(#{ name => peer:random_name(),
    exec => {os:find_executable(\"bash\"),[\"-c\",\"erl\"]},
    post_process_args =>
       fun([\"-c\"|Args]) -> [\"-c\", lists:flatten(lists:join($\\s, Args))] end
    }).
  ```

- **`env`** - List of environment variables with their values. This list is
  applied to a locally started executable. If you need to change the environment
  of the remote peer, adjust `args` to contain `-env ENV_KEY ENV_VALUE`.

- **`wait_boot`** - Specifies the start/start_link timeout. See
  [`wait_boot` datatype](`t:wait_boot/0`).

- **`shutdown`** - Specifies the peer node stopping behaviour. See
  [`stop()`](`stop/1`).
".
-endif.
-type start_options() ::
        #{
          name => atom() | string(),          %% node name (part before @), if not defined, peer
          %%  starts in non-distributed mode (requires alternative connection)
          longnames => boolean(),             %% long/short names (default is net_kernel:longnames(), and shortnames)
          host => string(),                   %% force hostname (when not specified, actual peer node hostname is used)
          peer_down => stop | continue | crash,   %% stop (default): when peer terminates, peer control process
          %%  stops normally regardless of the reason.
          %% continue: when peer terminates, peer control process stays up
          %%  saving exit reason in the state
          %% crash: when peer terminates, origin process
          %%  terminates with underlying reason
          connection => connection(),         %% alternative connection specification
          exec => exec(),                     %% path to executable, or SSH/Docker support
          detached => boolean(),              %% if the node should be start in detached mode
          args => [string()],                 %% additional command line parameters to append
          post_process_args =>
              fun(([string()]) -> [string()]),%% fix the arguments
          env => [{string(), string()}],      %% additional environment variables
          wait_boot => wait_boot(),           %% default is synchronous start with 15 sec timeout
          shutdown => close |                 %% close supervision channel
          halt | %% The default...            %% stop node using erlang:halt() wait default timeout for nodedown
          {halt, disconnect_timeout()} |      %% stop node using erlang:halt() wait timeout() for nodedown
          disconnect_timeout()                %% send init:stop() request and wait up to specified timeout for nodedown
         }.

%% Peer node states
-ifndef(NO_DOCS).
-doc "Peer node state.".
-endif.
-type peer_state() :: booting | running | {down, Reason :: term()}.

-export_type([
              start_options/0,
              peer_state/0,
              exec/0,
              disconnect_timeout/0
             ]).

%% Maximum integer timeout value in a receive...
-define (MAX_INT_TIMEOUT, 4294967295).

%% Default time we wait for distributed connection to be removed,
%% when shutdown type is halt, before we disconnect from the node...
-define (DEFAULT_HALT_DISCONNECT_TIMEOUT, 5000).

%% Minimum time we wait for distributed connection to be removed,
%% before we disconnect from the node (except in the shutdown
%% close case)...
-define (MIN_DISCONNECT_TIMEOUT, 1000).

%% Socket connect timeout, for TCP connection.
-define (CONNECT_TIMEOUT, 10000).

%% Socket accept timeout, for TCP connection.
-define (ACCEPT_TIMEOUT, 60000).

%% How long to wait for graceful shutdown.
-define (SHUTDOWN_TIMEOUT, 10000).

%% Synchronous RPC timeout for alternative connection.
-define (SYNC_RPC_TIMEOUT, 5000).

%% Default timeout for peer node to boot.
-define (WAIT_BOOT_TIMEOUT, 15000).

-ifndef(NO_DOCS).
-doc "Disconnect timeout. See [`stop()`](`stop/1`).".
-endif.
-type disconnect_timeout() :: ?MIN_DISCONNECT_TIMEOUT..?MAX_INT_TIMEOUT | infinity.

%% Peer supervisor channel connect timeout.
-define(PEER_SUP_CHANNEL_CONNECT_TIMEOUT, 30000).

%% @doc Creates random node name, using "peer" as prefix.
-ifndef(NO_DOCS).
-doc #{ equiv => random_name(peer) }.
-doc(#{since => <<"OTP 25.0">>}).
-endif.
-spec random_name() -> string().
random_name() ->
    random_name(?MODULE_STRING).

%% @doc Creates sufficiently random node name,
%%      using OS process ID for origin VM, resulting name
%%      looks like prefix-3-7161
-ifndef(NO_DOCS).
-doc "
Creates a sufficiently unique node name for the current host, combining a
prefix, a unique number, and the current OS process ID.

> #### Note {: .info }
>
> Use the `?CT_PEER([\"erl_arg1\"])` macro provided by Common Test
> `-include_lib(\"common_test/include/ct.hrl\")` for convenience. It starts a new
> peer using Erlang distribution as the control channel, supplies thes calling
> module's code path to the peer, and uses the calling function name for the
> name prefix.
".
-doc(#{since => <<"OTP 25.0">>}).
-endif.
-spec random_name(Prefix :: string() | atom()) -> string().
random_name(Prefix) ->
    OsPid = os:getpid(),
    Uniq = erlang:unique_integer([positive]),
    lists:concat([Prefix, "-", Uniq, "-", OsPid]).

%% @doc Starts a distributed node with random name, on this host,
%%      and waits for that node to boot. Returns full node name,
%%      registers local process with the same name as peer node.
-ifndef(NO_DOCS).
-doc "The same as [`start_link(#{name => random_name()})`](`start_link/1`).".
-doc(#{since => <<"OTP 25.0">>}).
-endif.
-spec start_link() -> {ok, pid(), node()} | {error, Reason :: term()}.
start_link() ->
    start_link(#{name => random_name()}).

%% @doc Starts peer node, linked to the calling process.
%%      Accepts additional command line arguments and
%%      other important options.
-ifndef(NO_DOCS).
-doc "
Starts a peer node in the same way as `start/1`, except that the peer node is
linked to the currently executing process. If that process terminates, the peer
node also terminates.

Accepts `t:start_options/0`. Returns the controlling process and the full peer
node name, unless `wait_boot` is not requested and host name is not known in
advance.

When the `standard_io` alternative connection is requested, and `wait_boot` is
not set to `false`, a failed peer boot sequence causes the caller to exit with
the `{boot_failed, {exit_status, ExitCode}}` reason.
".
-doc(#{since => <<"OTP 25.0">>}).
-endif.
-spec start_link(start_options()) -> {ok, pid()} | {ok, pid(), node()} | {error, Reason}
              when Reason :: term().
start_link(Options) ->
    start_it(Options, start_link).

%% @doc Starts peer node, not linked to the calling process.
-ifndef(NO_DOCS).
-doc "
Starts a peer node with the specified `t:start_options/0`. Returns the
controlling process and the full peer node name, unless `wait_boot` is not
requested and the host name is not known in advance.
".
-doc(#{since => <<"OTP 25.0">>}).
-endif.
-spec start(start_options()) -> {ok, pid()} | {ok, pid(), node()} | {error, Reason}
              when Reason :: term().
start(Options) ->
    start_it(Options, start).

%% @doc Stops controlling process, shutting down peer node synchronously
-ifndef(NO_DOCS).
-doc "
Stops a peer node. How the node is stopped depends on the
[`shutdown`](`t:start_options/0`) option passed when starting the peer node.
Currently the following `shutdown` options are supported:

- **`halt`** - This is the default shutdown behavior. It behaves as `shutdown`
  option `{halt, DefaultTimeout}` where `DefaultTimeout` currently equals
  `5000`.

- **`{halt, Timeout :: disconnect_timeout()}`** - Triggers a call to
  [`erlang:halt()`](`erlang:halt/0`) on the peer node and then waits for the
  Erlang distribution connection to the peer node to be taken down. If this
  connection has not been taken down after `Timeout` milliseconds, it will
  forcefully be taken down by `peer:stop/1`. See the
  [warning](`m:peer#dist_connection_close`) below for more info about this.

- **`Timeout :: disconnect_timeout()`** - Triggers a call to
  [`init:stop()`](`init:stop/0`) on the peer node and then waits for the Erlang
  distribution connection to the peer node to be taken down. If this connection
  has not been taken down after `Timeout` milliseconds, it will forcefully be
  taken down by `peer:stop/1`. See the [warning](`m:peer#dist_connection_close`)
  below for more info about this.

- **`close`** - Close the _control connection_ to the peer node and return. This
  is the fastest way for the caller of `peer:stop/1` to stop a peer node.

  Note that if the Erlang distribution connection is not used as control
  connection it might not have been taken down when `peer:stop/1` returns. Also
  note that the [warning](`m:peer#dist_connection_close`) below applies when the
  Erlang distribution connection is used as control connection.

[](){: #dist_connection_close }

> #### Warning {: .warning }
>
> In the cases where the Erlang distribution connection is taken down by
> `peer:stop/1`, other code independent of the peer code might react to the
> connection loss before the peer node is stopped which might cause undesirable
> effects. For example, [`global`](`m:global#prevent_overlapping_partitions`)
> might trigger even more Erlang distribution connections to other nodes to be
> taken down. The potential undesirable effects are, however, not limited to
> this. It is hard to say what the effects will be since these effects can be
> caused by any code with links or monitors to something on the origin node, or
> code monitoring the connection to the origin node.
".
-doc(#{since => <<"OTP 25.0">>}).
-endif.
-spec stop(Dest :: server_ref()) -> ok.
stop(Dest) ->
    gen_server:stop(Dest).

%% @doc returns peer node state.
-ifndef(NO_DOCS).
-doc "
Returns the peer node state.

The initial state is `booting`; the node stays in that state until then boot
script is complete, and then the node progresses to `running`. If the node stops
(gracefully or not), the state changes to `down`.
".
-doc(#{since => <<"OTP 25.0">>}).
-endif.
-spec get_state(Dest :: server_ref()) -> peer_state().
get_state(Dest) ->
    gen_server:call(Dest, get_state).

%% @doc Calls M:F(A) remotely, via alternative connection, with default 5 seconds timeout
-ifndef(NO_DOCS).
-doc(#{equiv => call(Dest, Module, Function, Args, ?SYNC_RPC_TIMEOUT)}).
-doc(#{since => <<"OTP 25.0">>}).
-endif.
-spec call(Dest :: server_ref(), Module :: module(), Function :: atom(),
           Args :: [term()]) -> Result :: term().
call(Dest, M, F, A) ->
    call(Dest, M, F, A, ?SYNC_RPC_TIMEOUT).

%% @doc Call M:F(A) remotely, timeout is explicitly specified
-ifndef(NO_DOCS).
-doc "
Uses the alternative connection to evaluate
[`apply(Module, Function, Args)`](`apply/3`) on the peer node and returns the
corresponding value `Result`.

`Timeout` is an integer representing the timeout in milliseconds or the atom
`infinity` which prevents the operation from ever timing out.

When an alternative connection is not requested, this function will raise `exit`
signal with the `noconnection` reason. Use `m:erpc` module to communicate over
Erlang distribution.
".
-doc(#{since => <<"OTP 25.0">>}).
-endif.
-spec call(Dest :: server_ref(), Module :: module(), Function :: atom(),
           Args :: [term()], Timeout :: timeout()) -> Result :: term().
call(Dest, M, F, A, Timeout) ->
    case gen_server:call(Dest, {call, M, F, A}, Timeout) of
        {ok, Reply} ->
            Reply;
        {Class, {Reason, Stack}} ->
            erlang:raise(Class, Reason, Stack);
        {error, Reason} ->
            erlang:error(Reason)
    end.

%% @doc Cast M:F(A) remotely, don't care about the result
-ifndef(NO_DOCS).
-doc "
Uses the alternative connection to evaluate
[`apply(Module, Function, Args)`](`apply/3`) on the peer node. No response is
delivered to the calling process.

`peer:cast/4` fails silently when the alternative connection is not configured.
Use `m:erpc` module to communicate over Erlang distribution.
".
-doc(#{since => <<"OTP 25.0">>}).
-endif.
-spec cast(Dest :: server_ref(), Module :: module(), Function :: atom(), Args :: [term()]) -> ok.
cast(Dest, M, F, A) ->
    gen_server:cast(Dest, {cast, M, F, A}).

%% @doc Sends a message to pid or named process on the peer node
%%  using alternative connection. No delivery guarantee.
-ifndef(NO_DOCS).
-doc "
Uses the alternative connection to send Message to a process on the the peer node.

Silently fails if no alternative connection is configured. The process can
be referenced by process ID or registered name.
".
-doc(#{since => <<"OTP 25.0">>}).
-endif.
-spec send(Dest :: server_ref(), To :: pid() | atom(), Message :: term()) -> ok.
send(Dest, To, Message) ->
    gen_server:cast(Dest, {send, To, Message}).

%%--------------------------------------------------------------------
%%% gen_server callbacks

-record(peer_state, {
                     options :: start_options(),
                     %% full node name, can be 'undefined'
                     node :: atom(),
                     %% debugging information: executable and arguments used to
                     %%  start the peer
                     exec :: file:name(),
                     args :: [string()],
                     %% alternative connection socket/port
                     connection :: undefined | port() | gen_tcp:socket(),
                     %% listening socket, while waiting for network alternative connection
                     listen_socket :: undefined | gen_tcp:socket(),
                     %% accumulator for RPC over standard_io
                     stdio = <<>> :: binary(),
                     %% peer state
                     peer_state = booting :: peer_state(),
                     %% pid/ref saved for gen:reply() when node is booted, or false
                     notify = false :: false | {pid(), reference()},
                     %% counter (reference) for calls.
                     %% it is not possible to use erlang reference, or pid,
                     %%  because it changes when node becomes distributed dynamically.
                     seq = 0 :: non_neg_integer(),
                     %% outstanding calls
                     outstanding = #{} :: #{non_neg_integer() => {reference(), pid()}}
                    }).

-type state() :: #peer_state{}.

-ifndef(NO_DOCS).
-doc false.
-endif.
-spec init([Name :: atom(), ... ]) -> {ok, state()}.
init([Notify, Options]) ->
    process_flag(trap_exit, true), %% need this to ensure terminate/2 is called

    {ListenSocket, Listen} = maybe_listen(Options),
    {Exec, Args} = command_line(Listen, Options),

    Env = maps:get(env, Options, []),

    PostProcessArgs = maps:get(post_process_args, Options, fun(As) -> As end),
    FinalArgs = PostProcessArgs(Args),

    %% close port if running detached
    Conn =
        case maps:find(connection, Options)  of
            {ok, standard_io} ->
                %% Cannot detach a peer that uses stdio. Request exit_status.
                open_port({spawn_executable, Exec},
                          [{args, FinalArgs}, {env, Env}, hide,
                           binary, exit_status, stderr_to_stdout]);
            _ ->
                Port = open_port({spawn_executable, Exec},
                                 [{args, FinalArgs}, {env, Env}, hide, binary]),
                %% peer can close the port before we get here which will cause
                %%  port_close to throw. Catch this and ignore.
                catch erlang:port_close(Port),
                receive {'EXIT', Port, _} -> undefined end
        end,

    %% Remove the default 'halt' shutdown option if present; the default is
    %% defined in terminate()...
    SaveOptions = case maps:find(shutdown, Options) of
                      {ok, halt} ->
                          maps:remove(shutdown, Options);
                      _ ->
                          Options
                  end,

    State = #peer_state{options = SaveOptions, notify = Notify, args = Args, exec = Exec},

    %% accept TCP connection if requested
    if ListenSocket =:= undefined ->
            {ok, State#peer_state{connection = Conn}};
       true ->
            _ = prim_inet:async_accept(ListenSocket, ?ACCEPT_TIMEOUT),
            {ok, State#peer_state{listen_socket = ListenSocket}}
    end.

%% not connected: no alternative connection available
-ifndef(NO_DOCS).
-doc false.
-endif.
handle_call({call, _M, _F, _A}, _From, #peer_state{connection = undefined} = State) ->
    {reply, {error, noconnection}, State};

handle_call({call, M, F, A}, From,
            #peer_state{connection = Port, options = #{connection := standard_io},
                        outstanding = Out, seq = Seq} = State) ->
    origin_to_peer(port, Port, {call, Seq, M, F, A}),
    {noreply, State#peer_state{outstanding = Out#{Seq => From}, seq = Seq + 1}};

handle_call({call, M, F, A}, From,
            #peer_state{connection = Socket, outstanding = Out, seq = Seq} = State) ->
    origin_to_peer(tcp, Socket, {call, Seq, M, F, A}),
    {noreply, State#peer_state{outstanding = Out#{Seq => From}, seq = Seq + 1}};

handle_call({starting, Node}, _From, #peer_state{ options = Options } = State) ->
    case maps:find(shutdown, Options) of
        {ok, {Timeout, MainCoverNode}} when is_integer(Timeout),
                                            is_atom(MainCoverNode) ->
            %% The node was started using test_server:start_peer/2 with cover enabled
            %% so we should start cover on the starting node.
            Modules = erpc:call(MainCoverNode,cover,modules,[]),
            Sticky = [ begin erpc:call(Node, code, unstick_mod, [M]), M end
                       || M <- Modules, erpc:call(Node, code, is_sticky,[M])],
            _ = erpc:call(MainCoverNode, cover, start, [Node]),
            _ = [erpc:call(Node, code,stick_mod,[M]) || M <- Sticky],
            ok;
        _ ->
            ok
    end,
    {reply, ok, State};
handle_call(get_node, _From, #peer_state{node = Node} = State) ->
    {reply, Node, State};

handle_call(get_state, _From, #peer_state{peer_state = PeerState} = State) ->
    {reply, PeerState, State};

handle_call(group_leader, _From, State) ->
    {reply, group_leader(), State}.

-ifndef(NO_DOCS).
-doc false.
-endif.
handle_cast({cast, _M, _F, _A}, #peer_state{connection = undefined} = State) ->
    {noreply, State};

handle_cast({cast, M, F, A},
            #peer_state{connection = Port, options = #{connection := standard_io}} = State) ->
    origin_to_peer(port, Port, {cast, M, F, A}),
    {noreply, State};

handle_cast({cast, M, F, A}, #peer_state{connection = Socket} = State) ->
    origin_to_peer(tcp, Socket, {cast, M, F, A}),
    {noreply, State};

handle_cast({send, _Dest, _Message}, #peer_state{connection = undefined} = State) ->
    {noreply, State};

handle_cast({send, Dest, Message},
            #peer_state{connection = Port, options = #{connection := standard_io}} = State) ->
    origin_to_peer(port, Port, {message, Dest, Message}),
    {noreply, State};

handle_cast({send, Dest, Message}, #peer_state{connection = Socket} = State) ->
    origin_to_peer(tcp, Socket, {message, Dest, Message}),
    {noreply, State}.

%%--------------------------------------------------------------------
%% alternative connections handling

%% alternative communications - request or response from peer
-ifndef(NO_DOCS).
-doc false.
-endif.
handle_info({tcp, Socket, SocketData},  #peer_state{connection = Socket} = State) ->
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, handle_alternative_data(tcp, binary_to_term(SocketData), State)};

%% standard_io
handle_info({Port, {data, PortData}}, #peer_state{connection = Port, stdio = PrevBin} = State) ->
    {Str, NewBin} = decode_port_data(PortData, <<>>, PrevBin),
    Str =/= <<>> andalso io:put_chars(Str),
    {noreply, handle_port_binary(NewBin, State)};

%% booting: accepted TCP connection from the peer, but it is not yet
%%  complete handshake
handle_info({inet_async, LSock, _Ref, {ok, CliSocket}},
            #peer_state{listen_socket = LSock} = State) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    ok = inet:setopts(CliSocket, [{active, once}]),
    catch gen_tcp:close(LSock),
    {noreply, State#peer_state{connection = CliSocket, listen_socket = undefined}};

handle_info({inet_async, LSock, _Ref, {error, Reason}},
            #peer_state{listen_socket = LSock} = State) ->
    %% failed to accept a TCP connection
    catch gen_tcp:close(LSock),
    %% stop unconditionally, it is essentially a part of gen_server:init callback
    {stop, {inet_async, Reason}, State#peer_state{connection = undefined, listen_socket = undefined}};

%% booting: peer notifies via Erlang distribution
handle_info({started, Node}, State)->
    true = erlang:monitor_node(Node, true),
    {noreply, boot_complete(Node, started, State)};

%% nodedown: no-alternative dist-connected peer node is down
handle_info({nodedown, Node}, #peer_state{connection = undefined} = State) ->
    maybe_stop({nodedown, Node}, State);

%% external program exited, returned Status code
handle_info({Port, {exit_status, Status}}, #peer_state{connection = Port} = State) ->
    catch erlang:port_close(Port),
    maybe_stop({exit_status, Status}, State);

%% port terminated: cannot proceed, stop the server
handle_info({'EXIT', Port, Reason}, #peer_state{connection = Port} = State) ->
    catch erlang:port_close(Port),
    maybe_stop(Reason, State);

handle_info({tcp_closed, Sock}, #peer_state{connection = Sock} = State) ->
    %% TCP connection closed, no i/o port - assume node is stopped
    catch gen_tcp:close(Sock),
    maybe_stop(tcp_closed, State#peer_state{connection = undefined}).

%%--------------------------------------------------------------------
%% cleanup/termination

-ifndef(NO_DOCS).
-doc false.
-endif.
-spec terminate(Reason :: term(), state()) -> ok.
terminate(_Reason, #peer_state{connection = Port, options = Options, node = Node}) ->
    case {maps:get(shutdown, Options, {halt, ?DEFAULT_HALT_DISCONNECT_TIMEOUT}),
          maps:find(connection, Options)} of
        {close, {ok, standard_io}} ->
            Port /= undefined andalso (catch erlang:port_close(Port));
        {close, {ok, _TCP}} ->
            Port /= undefined andalso (catch gen_tcp:close(Port));
        {close, error} ->
            _ = erlang:disconnect_node(Node);
        {{halt, Timeout}, {ok, standard_io}} ->
            Port /= undefined andalso (catch erlang:port_close(Port)),
            wait_disconnected(Node, {timeout, Timeout});
        {{halt, Timeout}, {ok, _TCP}} ->
            Port /= undefined andalso (catch gen_tcp:close(Port)),
            wait_disconnected(Node, {timeout, Timeout});
        {{halt, Timeout}, error} ->
            try
                _ = erpc:call(Node, erlang, halt, [], Timeout),
                ok
            catch
                error:{erpc,noconnection} -> ok;
                _:_ -> force_disconnect_node(Node)
            end;
        {Shutdown, error} ->
            Timeout = shutdown(dist, undefined, Node, Shutdown),
            wait_disconnected(Node, {timeout, Timeout});
        {Shutdown, {ok, standard_io}} ->
            Timeout = shutdown(port, Port, Node, Shutdown),
            Deadline = deadline(Timeout),
            receive {'EXIT', Port, _Reason2} -> ok after Timeout -> ok end,
            catch erlang:port_close(Port),
            wait_disconnected(Node, Deadline);
        {Shutdown, {ok, _TCP}} ->
            Timeout = shutdown(tcp, Port, Node, Shutdown),
            Deadline = deadline(Timeout),
            receive {tcp_closed, Port} -> ok after Timeout -> ok end,
            catch catch gen_tcp:close(Port),
            wait_disconnected(Node, Deadline)
    end,
    ok.

%%--------------------------------------------------------------------
%% Internal implementation

deadline(infinity) ->
    {timeout, infinity};
deadline(Timeout) when is_integer(Timeout) ->
    {deadline, erlang:monotonic_time(millisecond) + Timeout}.

wait_disconnected(Node, WaitUntil) ->
    %% Should only be called just before we are exiting the caller, so
    %% we do not bother disabling nodes monitoring if we enable it and
    %% do not flush any nodeup/nodedown messages that we got due to the
    %% nodes monitoring...
    case lists:member(Node, nodes(connected)) of
        false ->
            ok;
        true ->
            _ = net_kernel:monitor_nodes(true, [{node_type, all}]),
            %% Need to check connected nodes list again, since it
            %% might have disconnected before we enabled nodes
            %% monitoring...
            case lists:member(Node, nodes(connected)) of
                false ->
                    ok;
                true ->
                    Tmo = case WaitUntil of
                              {timeout, T} ->
                                  T;
                              {deadline, T} ->
                                  TL = T - erlang:monotonic_time(millisecond),
                                  if TL < 0 -> 0;
                                     true -> TL
                                  end
                          end,
                    receive {nodedown, Node, _} -> ok
                    after Tmo -> force_disconnect_node(Node)
                    end
            end
    end.

force_disconnect_node(Node) ->
    _ = erlang:disconnect_node(Node),
    logger:warning("peer:stop() timed out waiting for disconnect from "
                   "node ~p. The connection was forcefully taken down.",
                   [Node]).

%% This hack is a temporary workaround for test coverage reports
shutdown(_Type, _Port, Node, Timeout) when is_integer(Timeout); Timeout =:= infinity ->
    erpc:cast(Node, init, stop, []),
    Timeout;
shutdown(dist, undefined, Node, {Timeout, CoverNode}) when is_integer(Timeout); Timeout =:= infinity ->
    rpc:call(CoverNode, cover, flush, [Node]),
    erpc:cast(Node, init, stop, []),
    Timeout;
shutdown(Type, Port, Node, {Timeout, CoverNode}) when is_integer(Timeout); Timeout =:= infinity ->
    rpc:call(CoverNode, cover, flush, [Node]),
    Port /= undefined andalso origin_to_peer(Type, Port, {cast, init, stop, []}),
    Timeout.

%% Verify options correctness (Dialyzer also does the job, but slightly less convenient)
verify_args(Options) ->
    %% verify that "Args" is valid - common problem is when Args aren't strings
    Args = maps:get(args, Options, []),
    is_list(Args) orelse error({invalid_arg, Args}),
    [error({invalid_arg, Arg}) || Arg <- Args, not io_lib:char_list(Arg)],
    %% alternative connection must be requested for non-distributed node,
    %%  or a distributed node when origin is not alive
    is_map_key(connection, Options)
        orelse
          (is_map_key(name, Options) andalso erlang:is_alive()) orelse error(not_alive),
    %% exec must be a string, or a tuple of string(), [string()]
    case maps:find(exec, Options) of
        {ok, {Exec, Strs}} ->
            io_lib:char_list(Exec) orelse error({exec, Exec}),
            [error({exec, Str}) || Str <- Strs, not io_lib:char_list(Str)],
            ok;
        {ok, Exec} when is_list(Exec) ->
            io_lib:char_list(Exec) orelse error({exec, Exec}),
            ok;
        error ->
            ok;
        {ok, Err} ->
            error({exec, Err})
    end,
    case maps:find(shutdown, Options) of
        {ok, close} ->
            ok;
        {ok, halt} ->
            ok;
        {ok, {halt, Tmo}} when (is_integer(Tmo)
                                andalso ?MIN_DISCONNECT_TIMEOUT =< Tmo
                                andalso Tmo =< ?MAX_INT_TIMEOUT)
                               orelse Tmo == infinity ->
            ok;
        {ok, Tmo} when (is_integer(Tmo)
                        andalso ?MIN_DISCONNECT_TIMEOUT =< Tmo
                        andalso Tmo =< ?MAX_INT_TIMEOUT)
                       orelse Tmo == infinity ->
            ok;
        {ok, {Tmo, Node}} when ((is_integer(Tmo)
                                 andalso ?MIN_DISCONNECT_TIMEOUT =< Tmo
                                 andalso Tmo =< ?MAX_INT_TIMEOUT)
                                orelse Tmo == infinity)
                               andalso is_atom(Node) ->
            ok;
        error ->
            ok;
        {ok, Err2} ->
            error({shutdown, Err2})
    end,
    case maps:find(detached, Options) of
        {ok, false} when map_get(connection, Options) =:= standard_io ->
            error({detached, cannot_detach_with_standard_io});
        _ ->
            ok
    end.

make_notify_ref(infinity) ->
    {self(), make_ref()};
make_notify_ref(WaitBoot) when is_integer(WaitBoot) ->
    {self(), make_ref()};
make_notify_ref({ReplyTo, Tag}) when is_pid(ReplyTo) ->
    {ReplyTo, Tag};
make_notify_ref(false) ->
    false.

start_it(Options, StartFun) ->
    verify_args(Options),
    WaitBoot = maps:get(wait_boot, Options, ?WAIT_BOOT_TIMEOUT),
    Notify = make_notify_ref(WaitBoot),
    case gen_server:StartFun(?MODULE, [Notify, Options], []) of
        {ok, Pid} when WaitBoot =:= infinity; is_integer(WaitBoot) ->
            {_, Ref} = Notify,
            Mref = erlang:monitor(process, Pid),
            receive
                {Ref, {started, NodeName, Pid}} ->
                    erlang:demonitor(Mref, [flush]),
                    {ok, Pid, NodeName};
                {Ref, {boot_failed, Reason, Pid}} ->
                    erlang:demonitor(Mref, [flush]),
                    erlang:exit({boot_failed, Reason});
                {'DOWN', Mref, _, _, Reason} ->
                    erlang:exit(Reason)
            after
                WaitBoot ->
                    _ = gen_server:stop(Pid),
                    erlang:demonitor(Mref, [flush]),
                    erlang:exit(timeout)
            end;
        {ok, Pid} when is_map_key(host, Options) ->
            {ok, Pid, node_name(Options)};
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

node_name(#{name := Name, host := Host}) ->
    list_to_atom(lists:concat([Name, "@", Host]));
node_name(_Options) ->
    undefined.

%% Lost control connection to the peer while the node was
%%  booting, this generally means a crash
maybe_stop(Reason, #peer_state{peer_state = booting} = State) ->
    _ = boot_complete(Reason, boot_failed, State),
    maybe_stop(Reason, State#peer_state{peer_state = {down, Reason}});
%%
maybe_stop(Reason, #peer_state{options = #{peer_down := crash}} = State) ->
    {stop, Reason, State#peer_state{peer_state = {down, Reason}, connection = undefined}};
%% if state was already down, keep the original reason
maybe_stop(_Reason, #peer_state{options = #{peer_down := continue}, peer_state = {down, _}} = State) ->
    {noreply, State};
%% continue working setting peer state to down
maybe_stop(Reason, #peer_state{options = #{peer_down := continue}} = State) ->
    {noreply, State#peer_state{peer_state = {down, Reason}}};
%% default: ignore Reason and shut down normally
maybe_stop(Reason, State) ->
    {stop, normal, State#peer_state{peer_state = {down, Reason}}}.

%% i/o protocol from origin:
%%  * {io_reply, ...}
%%  * {message, To, Content}
%%  * {call, From, M, F, A}
%%
%% i/o port protocol, from peer:
%%  * {io_request, From, ReplyAs, Request}
%%  * {message, To, Content}
%%  * {reply, From, ok | throw | error | exit | crash, Result | {Reason, Stack}}

%% Handles bytes coming from alternative connection, forwarding as needed.
handle_alternative_data(Kind, {io_request, From, FromRef, IoReq}, #peer_state{connection = Conn} = State) ->
    %% TODO: make i/o completely async
    Reply = {io_reply, From, FromRef, forward_request(IoReq)},
    origin_to_peer(Kind, Conn, Reply),
    State;
handle_alternative_data(_Kind, {message, To, Content}, State) ->
    To ! Content,
    State;
handle_alternative_data(_Kind, {reply, Seq, Class, Result}, #peer_state{outstanding = Out} = State) ->
    {From, NewOut} = maps:take(Seq, Out),
    gen:reply(From, {Class, Result}),
    State#peer_state{outstanding = NewOut};
handle_alternative_data(_Kind, {started, NodeName}, State)->
    boot_complete(NodeName, started, State).

forward_request(Req) ->
    GL = group_leader(),
    MRef = erlang:monitor(process, GL),
    GL ! {io_request,self(), MRef, Req},
    receive
        {io_reply, MRef, Reply} ->
            erlang:demonitor(MRef, [flush]),
            Reply;
        {'DOWN', MRef, _, _, _} ->
            {error, terminated}
    end.

%% generic primitive to send data from origin to peer via alternative connection
origin_to_peer(tcp, Sock, Term) ->
    ok = gen_tcp:send(Sock, term_to_binary(Term));
origin_to_peer(port, Port, Term) ->
    true = erlang:port_command(Port, encode_port_data(term_to_binary(Term))).

%% generic primitive to send data from peer to origin
peer_to_origin(tcp, Sock, Term) ->
    ok = gen_tcp:send(Sock, term_to_binary(Term));
peer_to_origin(port, Port, Term) ->
    %% converts Erlang term to terminal codes
    %% Every binary byte is converted into two 4-bit sequences.
    Bytes = term_to_binary(Term),
    true = erlang:port_command(Port, encode_port_data(Bytes)).

%% convert/escape Erlang term into terminal codes.
%% Protocol consists of Erlang terms serialised into
%% External Term Format (ETF) via term_to_binary. Then
%% every byte of the resulting binary is split into two
%% nibbles (4-bit sequences), which are encoded into two
%% characters.
%% Control characters must have first two and last two bits
%% set, so the byte looks this way: 11xxxx11.
%% Example encoding, hexadecimal 16#0F will be encoded as
%%  11000011 11111111 (decimal 195 255).
encode_port_data(Bytes) ->
    Size = byte_size(Bytes),
    Crc = erlang:crc32(Bytes),
    Total = <<Size:32, Bytes/binary, Crc:32>>,
    <<<<3:2, Upper:4, 3:2, 3:2, Lower:4, 3:2>> || <<Upper:4, Lower:4>> <= Total>>.

%% convert terminal codes to Erlang term, printing everything that
%%  was detected as text
decode_port_data(<<>>, Str, Bin) ->
    {Str, Bin};
decode_port_data(<<3:2, Quad:4, 3:2, Rest/binary>>, Str, Bin) ->
    decode_port_data(Rest, Str, <<Bin/bitstring, Quad:4>>);
decode_port_data(<<Char:8, Rest/binary>>, Str, Bin) ->
    decode_port_data(Rest, <<Str/binary, Char>>, Bin).

%% recursively process buffers, potentially changing the state
handle_port_binary(<<Size:32, Payload:Size/binary, Crc:32, Rest/binary>>, State) ->
    Crc = erlang:crc32(Payload),
    Term = binary_to_term(Payload),
    NewState = handle_alternative_data(port, Term, State),
    handle_port_binary(Rest, NewState);
handle_port_binary(NewBin, State) ->
    State#peer_state{stdio = NewBin}.

boot_complete(Node, _Result, #peer_state{notify = false} = State) ->
    State#peer_state{peer_state = running, node = Node};
boot_complete(Node, Result, #peer_state{notify = {ReplyTo, Tag}} = State) ->
    ReplyTo ! {Tag, {Result, Node, self()}},
    State#peer_state{peer_state = running, node = Node}.

%% check if TCP connection is enabled, and starts listener
maybe_listen(#{connection := Port}) when is_integer(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {reuseaddr, true}, {packet, 4}]),
    {ok, WaitPort} = inet:port(LSock),
    %% try guessing a local IP address
    {ok, Ifs} = inet:getifaddrs(),
    %% next incantation gets all IP addresses of all local interfaces that are up
    LocalUp = lists:append(
                [proplists:get_all_values(addr, Opts)
                 || {_, Opts} <- Ifs, lists:member(up, proplists:get_value(flags, Opts, []))]),
    %% filter invalid addresses
    Local = prefer_localhost([Valid || Valid <- LocalUp, is_list(inet:ntoa(Valid))], [], []),
    {LSock, {Local, WaitPort}};
maybe_listen(#{connection := {Ip, Port}}) when is_integer(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {reuseaddr, true}, {packet, 4}, {ip, Ip}]),
    WaitPort = if Port =:= 0 -> {ok, Dyn} = inet:port(LSock), Dyn; true -> Port end,
    {LSock, {[Ip], WaitPort}};
maybe_listen(_Options) ->
    {undefined, undefined}.

%% prefer localhost, IPv6 localhost, then everything else
prefer_localhost([], Preferred, Other) ->
    Preferred ++ Other;
prefer_localhost([{127, _, _, _} = Local | Tail], Preferred, Other) ->
    prefer_localhost(Tail, [Local | Preferred], Other);
prefer_localhost([{0, 0, 0, 0, 0, 0, 0, 1} = Local | Tail], Preferred, Other) ->
    prefer_localhost(Tail, [Local | Preferred], Other);
prefer_localhost([Local | Tail], Preferred, Other) ->
    prefer_localhost(Tail, Preferred, [Local | Other]).

name_arg(error, error, _) ->
    []; %% no name, no host - starting node that is not distributed
name_arg(error, {ok, Host}, LongOrShort) ->
    %% interesting fallback: host is set, name is not. Do what predecessor did,
    %%  take the current node name and use it...
    [Name, _] = string:lexemes(atom_to_list(node()), "@"),
    name_arg(Name ++ "@" ++ Host, error, LongOrShort);
name_arg({ok, Name}, Host, LongOrShort) ->
    name_arg(Name, Host, LongOrShort); %% unpack node name
name_arg(Name, Host, LongOrShort) when is_atom(Name) ->
    name_arg(atom_to_list(Name), Host, LongOrShort); %% convert atom to list for command line
name_arg(Name, Host, {ok, ignored}) ->
    name_arg(Name, Host, {ok, false}); %% fallback to shortnames when origin is not distributed
name_arg(Name, Host, error) ->
    name_arg(Name, Host, {ok, net_kernel:longnames()}); %% no longnames present it start options
name_arg(Name, {ok, Host}, LongOrShort) ->
    name_arg(Name ++ "@" ++ Host, error, LongOrShort); %% merge host part
%% only these are actual name creation clauses
name_arg(Name, error, {ok, true}) ->
    ["-name", Name];
name_arg(Name, error, {ok, false}) ->
    ["-sname", Name].

command_line(Listen, Options) ->
    %% Node name/sname
    NameArg = name_arg(maps:find(name, Options), maps:find(host, Options), maps:find(longnames, Options)),
    %% additional command line args
    CmdOpts = maps:get(args, Options, []),

    %% If we should detach from the node. We use -detached to tell erl to detach
    %% and -peer_detached to tell peer:start that we are detached.
    DetachArgs = case maps:get(detached, Options, true) of
                     true -> ["-detached","-peer_detached"];
                     false -> []
                 end,

    %% start command
    StartCmd =
        case Listen of
            undefined when map_get(connection, Options) =:= standard_io ->
                ["-user", atom_to_list(?MODULE)];
            undefined ->
                Self = base64:encode_to_string(term_to_binary(self())),
                DetachArgs ++ ["-user", atom_to_list(?MODULE), "-origin", Self];
            {Ips, Port} ->
                IpStr = lists:concat(lists:join(",", [inet:ntoa(Ip) || Ip <- Ips])),
                DetachArgs ++ ["-user", atom_to_list(?MODULE), "-origin", IpStr, integer_to_list(Port)]
        end,
    %% build command line
    {Exec, PreArgs} = exec(Options),
    {Exec, PreArgs ++ NameArg ++ CmdOpts ++ StartCmd}.

exec(#{exec := Prog}) when is_list(Prog) ->
    {Prog, []};
exec(#{exec := {Prog, Args}}) when is_list(Prog), is_list(Args) ->
    {Prog, Args};
exec(Options) when not is_map_key(exec, Options) ->
    case init:get_argument(progname) of
        {ok, [[Prog]]} ->
            case os:find_executable(Prog) of
                Exec when is_list(Exec) ->
                    {Exec, []};
                false ->
                    maybe_otp_test_suite(Prog)
            end;
        _ ->
            default_erts()
    end.

maybe_otp_test_suite(Prog) ->
    case string:split(Prog, "cerl ") of
        [CerlPath, Args] ->
            %% This is a hack to handle the 'cerl' script used
            %% by the Erlang/OTP test suites. When 'cerl'
            %% starts the runtime system, it typically sets
            %% 'progname' to the path of the 'cerl' script,
            %% followed by an argument. For example:
            %%
            %%     /<full_path_to>/cerl -debug
            %%     /<full_path_to>/cerl -asan
            %%     /<full_path_to>/cerl -gcov
            %%
            %% We should find a better way to handle this, for
            %% example by passing the emulator type and flavor
            %% using the '-emu_type' and '-emu_flavor'
            %% options. However, this is not without
            %% complications as those options do not really
            %% work for an installed system. Also, it is
            %% probably a good idea to stop using 'slave'
            %% before attempting to do this.
            {filename:join(CerlPath, "cerl"), parse_args(Args)};
        _ ->
            default_erts()
    end.


%% Split command line string into a list of arguments.
-spec parse_args(string()) -> [string()].
parse_args([]) ->
    [];
parse_args([Deep | _] = AlreadyParsed) when is_list(Deep) ->
    AlreadyParsed;
parse_args(CmdLine) ->
    %% following regex splits command line, preserving quoted arguments, into argv[] list
    Re = <<"((?:\"[^\"\\\\]*(?:\\\\[\\S\\s][^\"\\\\]*)*\"|'[^'\\\\]*(?:\\\\[\\S\\s][^'\\\\]*)*'|\\/[^\\/\\\\]*(?:\\\\[\\S\\s][^\\/\\\\]*)*\\/[gimy]*(?=\\s|$)|(?:\\\\\\s|\\S))+)(?=\\s|$)">>,
    {match, Args} = re:run(CmdLine, Re, [{capture, all_but_first, list}, global]),
    %% unquote arguments. It is possible to change regex capture groups to avoid extra processing.
    [unquote(Arg) || [Arg] <- Args].

unquote([Q | Arg]) when Q =:= $\"; Q =:= $\' ->
    case lists:last(Arg) of
        Q -> lists:droplast(Arg);
        _ -> [Q | Arg]
    end;
                        unquote(Arg) ->
    Arg.

%% if progname is not known, use `erlexec` from the same ERTS version we're currently running
%% BINDIR environment variable is already set
%% EMU variable it also set
default_erts() ->
    Root = code:root_dir(),
    Erts = filename:join(Root, lists:concat(["erts-", erlang:system_info(version)])),
    BinDir = filename:join(Erts, "bin"),
    {filename:join(BinDir, "erlexec"), []}.

%%--------------------------------------------------------------------
%% peer node implementation

notify_when_started(Kind, Port) ->
    init:notify_when_started(self()) =:= started andalso
        notify_started(Kind, Port),
    ok.

notify_started(dist, Process) ->
    Process ! {started, node()},
    ok;
notify_started(Kind, Port) ->
    peer_to_origin(Kind, Port, {started, node()}).

%%
%% Supervision of peer user process (which supervise the control channel) making
%% sure that the peer node is halted if the peer user process crashes...
%%

-ifndef(NO_DOCS).
-doc false.
-endif.
supervision_child_spec() ->
    case init:get_argument(user) of
        {ok, [["peer"]]} ->
            {ok, #{id => peer_supervision,
                   start => {?MODULE, start_supervision, []},
                   restart => permanent,
                   shutdown => 1000,
                   type => worker,
                   modules => [?MODULE]}};
        _ ->
            none
    end.

-ifndef(NO_DOCS).
-doc false.
-endif.
start_supervision() ->
    proc_lib:start_link(?MODULE, init_supervision, [self(), true]).

start_orphan_supervision() ->
    proc_lib:start(?MODULE, init_supervision, [self(), false]).

-record(peer_sup_state, {parent, channel, in_sup_tree}).

-ifndef(NO_DOCS).
-doc false.
-endif.
-spec init_supervision(term(), term()) -> no_return().
init_supervision(Parent, InSupTree) ->
    try
        process_flag(priority, high),
        process_flag(trap_exit, true),
        register(peer_supervision, self()),
        proc_lib:init_ack(Parent, {ok, self()}),
        Channel = receive
                      {channel_connect, Ref, From, ConnectChannel} ->
                          true = is_pid(ConnectChannel),
                          From ! Ref,
                          try
                              link(ConnectChannel)
                          catch error:noproc ->
                                  exit({peer_channel_terminated, noproc})
                          end,
                          ConnectChannel
                  after
                      ?PEER_SUP_CHANNEL_CONNECT_TIMEOUT ->
                          exit(peer_channel_connect_timeout)
                  end,
        loop_supervision(#peer_sup_state{parent = Parent,
                                         channel = Channel,
                                         in_sup_tree = InSupTree})
    catch
        _:_ when not InSupTree ->
            erlang:halt(1)
    end.

peer_sup_connect_channel(PeerSupervision, PeerChannelHandler) ->
    Ref = make_ref(),
    PeerSupervision ! {channel_connect, Ref, self(), PeerChannelHandler},
    receive
        Ref -> ok
    after
        ?PEER_SUP_CHANNEL_CONNECT_TIMEOUT ->
            exit(peer_supervision_connect_timeout)
    end.

loop_supervision(#peer_sup_state{parent = Parent,
                                 channel = Channel} = State) ->
    receive
        {'EXIT', Channel, Reason} ->
            exit({peer_channel_terminated, Reason});
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [], State);
        _ ->
            loop_supervision(State)
    end.


-ifndef(NO_DOCS).
-doc false.
-endif.
system_continue(_Parent, _, #peer_sup_state{} = State) ->
    loop_supervision(State).

-ifndef(NO_DOCS).
-doc false.
-endif.
system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

-ifndef(NO_DOCS).
-doc false.
-endif.
system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.

-ifndef(NO_DOCS).
-doc false.
-endif.
system_get_state(State) ->
    {ok, State}.

-ifndef(NO_DOCS).
-doc false.
-endif.
system_replace_state(StateFun, State) ->
    NState = StateFun(State),
    {ok, NState, NState}.

%% End of peer user supervision

%% I/O redirection: peer side
-ifndef(NO_DOCS).
-doc false.
-endif.
-spec start() -> pid().
start() ->
    try
        PeerChannelHandler = start_peer_channel_handler(),
        PeerSup = case whereis(peer_supervision) of
                      PeerSup0 when is_pid(PeerSup0) ->
                          PeerSup0;
                      undefined ->
                          {ok, PeerSup0} = start_orphan_supervision(),
                          PeerSup0
                  end,
        peer_sup_connect_channel(PeerSup, PeerChannelHandler),
        PeerChannelHandler
    catch _:_ ->
            erlang:halt(1)
    end.

start_peer_channel_handler() ->
    case init:get_argument(origin) of
        {ok, [[IpStr, PortString]]} ->
            %% enter this clause when -origin IpList Port is specified in the command line.
            Port = list_to_integer(PortString),
            Ips = [begin {ok, Addr} = inet:parse_address(Ip), Addr end ||
                      Ip <- string:lexemes(IpStr, ",")],
            TCPConnection = spawn(fun () -> tcp_init(Ips, Port) end),
            _ = case init:get_argument(peer_detached) of
                    {ok, _} ->
                        _ = register(user, TCPConnection);
                    error ->
                        _= user_sup:init(
                             [Flag || Flag <- init:get_arguments(), Flag =/= {user,["peer"]}])
                end,
            TCPConnection;
        {ok, [[Base64EncProc]]} ->
            %% No alternative connection, but have "-origin Base64EncProc"
            OriginProcess = binary_to_term(base64:decode(Base64EncProc)),
            OriginLink = spawn(
                           fun () ->
                                   MRef = monitor(process, OriginProcess),
                                   notify_when_started(dist, OriginProcess),
                                   origin_link(MRef, OriginProcess)
                           end),
            ok = gen_server:call(OriginProcess, {starting, node()}),
            _ = case init:get_argument(peer_detached) of
                    {ok, _} ->
                        %% We are detached, so setup 'user' process, I/O redirection:
                        %%   ask controlling process who is the group leader.
                        GroupLeader = gen_server:call(OriginProcess, group_leader),
                        RelayPid = spawn(fun () ->
                                                 link(OriginLink),
                                                 relay(GroupLeader)
                                         end),
                        _ = register(user, RelayPid);
                    error ->
                        %% We are not detached, so after we spawn the link process we
                        %% start the terminal as normal but without the -user peer flag.
                        _ = user_sup:init(
                              [Flag || Flag <- init:get_arguments(), Flag =/= {user,["peer"]}])
                end,
            OriginLink;
        error ->
            %% no -origin specified, meaning that standard I/O is used for alternative
            spawn(fun io_server/0)
    end.

relay(GroupLeader) ->
    receive
        IO ->
            GroupLeader ! IO,
            relay(GroupLeader)
    end.

origin_link(MRef, Origin) ->
    receive
        {'DOWN', MRef, process, Origin, _Reason} ->
            erlang:halt();
        {init, started} ->
            notify_started(dist, Origin),
            origin_link(MRef, Origin)
    end.

-spec io_server() -> no_return().
io_server() ->
   try
       process_flag(trap_exit, true),
       Port = erlang:open_port({fd, 0, 1}, [eof, binary]),
       register(user, self()),
       group_leader(self(), self()),
       notify_when_started(port, Port),
       io_server_loop(port, Port, #{}, #{}, <<>>)
    catch
        _:_ ->
            erlang:halt(1)
    end.

-spec tcp_init([term()], term()) -> no_return().
tcp_init(IpList, Port) ->
    try
        Sock = loop_connect(IpList, Port),
        erlang:group_leader(self(), self()),
        notify_when_started(tcp, Sock),
        io_server_loop(tcp, Sock, #{}, #{}, undefined)
    catch
        _:_ ->
            erlang:halt(1)
    end.

loop_connect([], _Port) ->
    error(noconnection);
loop_connect([Ip | More], Port) ->
    case gen_tcp:connect(Ip, Port, [binary, {packet, 4}], ?CONNECT_TIMEOUT) of
        {ok, Sock} ->
            Sock;
        _Error ->
            loop_connect(More, Port)
    end.

%% Message protocol between peers
io_server_loop(Kind, Port, Refs, Out, PortBuf) ->
    receive
        {io_request, From, ReplyAs, Request} when is_pid(From) ->
            %% i/o request from this node, forward it to origin
            peer_to_origin(Kind, Port, {io_request, From, ReplyAs, Request}),
            io_server_loop(Kind, Port, Refs, Out, PortBuf);
        {Port, {data, Bytes}} when Kind =:= port ->
            {_Str, NewBin} = decode_port_data(Bytes, <<>>, PortBuf),
            {NewRefs, NewOut, NewBuf} = handle_port_alternative(NewBin, Refs, Out),
            io_server_loop(Kind, Port, NewRefs, NewOut, NewBuf);
        {Port, eof} when Kind =:= port ->
            %% stdin closed, if there is no active alternative, stop the node
            erlang:halt(1);
        {'EXIT', Port, badsig} when Kind =:= port ->
            %% ignore badsig (what is it?)
            io_server_loop(Kind, Port, Refs, Out, PortBuf);
        {'EXIT', Port, _Reason} when Kind =:= port ->
            %% stdin closed, if there is no active alternative, stop the node
            erlang:halt(1);
        {tcp, Port, Data} when Kind =:= tcp ->
            ok = inet:setopts(Port, [{active, once}]), %% flow control
            {NewRefs, NewOut} = handle_peer_alternative(binary_to_term(Data), Refs, Out),
            io_server_loop(Kind, Port, NewRefs, NewOut, PortBuf);
        {tcp_closed, Port} when Kind =:= tcp ->
            %% TCP connection closed, time to shut down
            erlang:halt(1);
        {reply, Seq, Class, Reply} when is_integer(Seq), is_map_key(Seq, Out) ->
            %% stdin/stdout RPC
            {CallerRef, Out2} = maps:take(Seq, Out),
            Refs2 = maps:remove(CallerRef, Refs),
            erlang:demonitor(CallerRef, [flush]),
            peer_to_origin(Kind, Port, {reply, Seq, Class, Reply}),
            io_server_loop(Kind, Port, Refs2, Out2, PortBuf);
        %% stdin/stdout message forwarding
        {message, To, Content} ->
            peer_to_origin(Kind, Port, {message, To, Content}),
            io_server_loop(Kind, Port, Refs, Out, PortBuf);
        {'DOWN', CallerRef, _, _, Reason} ->
            %% this is really not expected to happen, because "do_call"
            %%  catches all exceptions
            {Seq, Refs3} = maps:take(CallerRef, Refs),
            {CallerRef, Out3} = maps:take(Seq, Out),
            peer_to_origin(Kind, Port, {reply, Seq, crash, Reason}),
            io_server_loop(Kind, Port, Refs3, Out3, PortBuf);
        {init, started} ->
            notify_started(Kind, Port),
            io_server_loop(Kind, Port, Refs, Out, PortBuf);
        _Other ->
            %% below, what is it?
            io_server_loop(Kind, Port, Refs, Out, PortBuf)
    end.

handle_peer_alternative({io_reply, From, FromRef, Reply}, Refs, Out) ->
    From ! {io_reply, FromRef, Reply},
    {Refs, Out};
handle_peer_alternative({call, Seq, M, F, A}, Refs, Out) ->
    CallerRef = do_call(Seq, M, F, A),
    {Refs#{CallerRef => Seq}, Out#{Seq => CallerRef}};
handle_peer_alternative({cast, M, F, A}, Refs, Out) ->
    %% spawn a separate process to avoid blocking further RPC
    spawn(fun () -> erlang:apply(M, F, A) end),
    {Refs, Out};
handle_peer_alternative({message, Dest, Message}, Refs, Out) ->
    Dest ! Message,
    {Refs, Out}.

%% single port input message may contain multiple messages
handle_port_alternative(<<Size:32, Payload:Size/binary, Crc:32, Rest/binary>>, Refs, Out) ->
    Crc = erlang:crc32(Payload), %% assert
    {NewRefs, NewOut} = handle_peer_alternative(binary_to_term(Payload), Refs, Out),
    handle_port_alternative(Rest, NewRefs, NewOut);
handle_port_alternative(Rest, Refs, Out) ->
    {Refs, Out, Rest}.

do_call(Seq, M, F, A) ->
    Proxy = self(),
    {_, CallerRef} =
        spawn_monitor(
          fun () ->
                  %% catch all errors, otherwise emulator will log
                  %%  ERROR REPORT when it is not expected
                  try
                      Proxy ! {reply, Seq, ok, erlang:apply(M, F, A)}
                  catch
                      Class:Reason:Stack ->
                          Proxy ! {reply, Seq, Class, {Reason, Stack}}
                  end
          end),
    CallerRef.
