%%
%% Copyright 2020 Jérôme de Bretagne
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
%% %ExternalCopyright%
%%
-module(erl_uds_dist).

%%
%% A distributed Erlang system consists of a number of Erlang runtime
%% systems, or Erlang nodes, communicating with each other. The default
%% Erlang distribution protocol (inet_tcp_dist) is using TCP/IP sockets.
%%
%% This is an example of how to plug in an alternative distribution
%% protocol using distribution controller processes. Erlang
%% distribution can use whatever underlying protocols as long as the
%% implementation reliably delivers data chunks to the receiving
%% Erlang node in the order they were sent by the sending node.
%%
%% This example uses stream-oriented Unix Domain Sockets (of the
%% SOCK_STREAM type from the AF_UNIX socket family, also known as
%% AF_LOCAL) as the protocol for exchanging data, allowing Erlang nodes
%% running locally on the same host to communicate with each other.
%%
%% The original uds_dist module is using a port driver written in C,
%% erl_uds_dist is using distribution controllers implemented by Erlang
%% processes instead, so with the code written entirely in Erlang.
%%
%% This implementation is based on the gen_tcp_dist.erl example.
%%

%%
%% To enable this distribution, start erl with the -proto_dist parameter:
%%
%% erl -proto_dist erl_uds -no_epmd -sname node@localhost
%%     -pa path/to/erl_uds_dist.beam
%%
%% or
%%
%% erl -proto_dist erl_uds -no_epmd -name node@127.0.0.1
%%     -pa path/to/erl_uds_dist.beam
%%


%% Handle the connection setup phase with other Erlang nodes
-export([listen/1, accept/1, accept_connection/5,
         setup/5, close/1, select/1, address/0]).

%% Optional functions for alternative distribution protocol
-export([setopts/2, getopts/2]).

%% Internal export
-export([accept_loop/2, accept_supervisor/6, setup_supervisor/5]).

-import(error_logger, [error_msg/2]).

-include_lib("kernel/include/net_address.hrl").

-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").

%%
%% If tracing is wanted, uncomment the dist_trace macro in dist_util.hrl
%% to enable all the calls to trace below, or copy the trace macro here.
%%
%% Tracing will freeze the initial boot when a -name or -sname parameter
%% is passed to start directly distributed nodes. To make it work,
%% launch non-distributed nodes first (without -name and -sname) then
%% call net_kernel:start/1 to enable the distribution in a second stage.
%%
%% Uncomment these two lines to disable the trace macro locally:
%% -undef(trace).
%% -define(trace(Fmt, Args), ok).
%%


%% Set the distribution protocol version statically (the different values
%% are listed in epmd.mk). All nodes are expected to use the same version
%% when using this distribution, to avoid the need for epmd.
-undef(ERL_DIST_VER).
-ifdef(ERL_DIST_VER_6).
%% Set it to 6 when supporting 32-bit big Creation numbers
-define(ERL_DIST_VER, 6).
-else.
%% Set it to 5 when supporting Creation numbers in the 1..3 range
-define(ERL_DIST_VER, 5).
-endif.


-spec select(NodeName) -> boolean() when
      NodeName :: node().
%% ---------------------------------------------------------------------
%% Return true if the host name part of NodeName is valid for use
%% with this protocol; false otherwise.
%%
%% For Unix Domain Sockets, the host name part doesn't matter, as such
%% sockets are only available for other nodes running locally on the
%% same host, so always return true.
%% ---------------------------------------------------------------------
select(_NodeName) ->
    true.


-spec listen(NodeNameWithoutHost) ->
                    {ok, {ListeningSocket, Address, Creation}} |
                    {error, Reason} when
      NodeNameWithoutHost :: node(),
      ListeningSocket :: gen_tcp:socket(),
      Address :: #net_address{},
      Creation :: 1..16#FFFFFFFF,
      Reason :: system_limit | inet:posix().
%% ---------------------------------------------------------------------
%% Listen for incoming connection requests on the specified Unix Domain Socket.
%% It is called only once when the distribution protocol is brought up.
%%
%% NodeNameWithoutHost defines the listening Unix Domain Socket to use, it is
%% the part before the '@' character in a full node name. It is usually a file
%% pathname in the local filesystem (limited in length to 107 bytes on Linux)
%% encoded according to the current system encoding mode, which can be either
%% relative or absolute. Erlang node names have some restrictions; as of this
%% writing, they are limited to the following characters: 0-9 A-Z a-z _ and -
%% (cf. net_kernel:valid_name_head/1) so they can't contain . / or \. As a
%% result, the socket file pathname is relative to the current directory.
%%
%% Return:
%% - ListeningSocket, a handle which is later passed to the accept/1 callback,
%%   i.e. the listening Unix Domain Socket through which this Erlang node
%%   is accessible.
%% - Address, a #net_address{} record with information about the address
%%   for this node.
%% - Creation, an integer 1, 2 or 3, that has to change for different
%%   instances of Erlang nodes created with the same node name.
%% ---------------------------------------------------------------------
listen(NodeNameWithoutHost) ->
    ?trace("~p~n", [{?MODULE, listen, self()}]),

    SocketPathname = to_string(NodeNameWithoutHost),
    %% Use the gen_tcp module for Unix Domain Sockets of the SOCK_STREAM
    %% socket type.
    %%
    %% The options passed to gen_tcp:listen:
    %% - {ifaddr, {local, Pathname :: binary() | string()} indicates to use a
    %%   Unix Domain Socket and defines which file pathname to listen on.
    %% - binary, to have each packet delivered as a binary.
    %% - {active, false} sets the listening socket in passive mode, meaning
    %%   the packets must be explicitly retrieved by calling recv/2,3.
    %% - {packet, 2} specifies a packet header length of 2 bytes, which
    %%   is expected in every message of the distribution protocol
    %%   until the initial distribution handshake completes.
    %%
    %% As documented, the port number must weirdly be set to 0 when using
    %% gen_tcp API functions for Unix Domain Sockets.
    case gen_tcp:listen(0, [{ifaddr, {local, SocketPathname}},
                            binary, {active, false}, {packet, 2}]) of
        %% Successful setup of the listening socket
        {ok, ListeningSocket} ->
            %% Get the listening socket address in a {local, Pathname} format
            {ok, SocketAddress} = inet:sockname(ListeningSocket),
            {ok, {ListeningSocket,
                  #net_address{address = SocketAddress,
                               %% Simply use 'localhost' as a convention
                               %% as host is not used in net_kernel.
                               host = localhost,
                               %% 'local' is the address family used for
                               %% Unix Domain Sockets.
                               family = local,
                               %% 'stream' is the convention chosen to
                               %% represent to SOCK_STREAM socket type.
                               protocol = stream},
                  %% Get the Creation number for the current Node instance
                  get_creation(SocketPathname)}};

        %% The specified file pathname is already in use or the filesystem
        %% socket object already exists.
        {error, eaddrinuse} ->
            %% Check that another Erlang node instance with the same node name
            %% is not currently running. Try to connect to this file pathname.
            case gen_tcp:connect({local, SocketPathname},
                                 0,
                                 [binary, {active, false}, {packet, 2}]) of
                {ok, SocketWithAnotherNode} ->
                    %% Connect has succeeded, so there is another Erlang node
                    %% already running and listening on this same pathname.
                    gen_tcp:close(SocketWithAnotherNode),
                    ?trace("Another node is already running with the same "
                           "node name: ~p~n", [SocketPathname]),
                    {error, duplicate_name};

                _ ->
                    %% No other Erlang node is listening on this file pathname
                    %% so this is just an existing file or a previous socket
                    %% object left after a crash/abort. Delete the file and
                    %% retry to setup the listening socket.
                    %%
                    %% The raw option is passed to bypass the need for a file
                    %% server, which is not available and registered yet during
                    %% the early boot stage.
                    case file:delete(SocketPathname, [raw]) of
                        ok ->
                            %% The file has been deleted, let's try again
                            listen(NodeNameWithoutHost);
                        {error, enoent} ->
                            %% enoent - No such file or directory to delete
                            %% anymore; unexpected but let's try again.
                            listen(NodeNameWithoutHost);
                        {error, eacces} ->
                            %% eacces - Permission denied
                            ?trace("The file ~p cannot be deleted, "
                                   "permission denied~n",
                                   [SocketPathname]),
                            {error, eacces};
                        _DeleteError ->
                            ?trace("Error returned by file:delete(~p, [raw]): "
                                   "~p~n", [SocketPathname, _DeleteError]),
                            _DeleteError
                    end
            end;

        Error ->
            Error
    end.


-spec address() -> Address :: #net_address{}.
%% ---------------------------------------------------------------------
%% Support the -dist_listen false option, so that a distribution can be
%% started without listening for incoming connections.
%%
%% In that case, address/0 is called in order to get the Address part of
%% the listen/1 function without creating a listening socket. All fields
%% except address have to be set in the returned Address record.
%%
%% This is used to support the dynamic name feature introduced in OTP 23.0,
%% which allows to start a node with an 'undefined' name at first. It will
%% then get its actual name randomly from the first node it connects to.
%% ---------------------------------------------------------------------
address() ->
    #net_address{%% Simply use 'localhost' as a convention
                 %% as host is not used in net_kernel.
                 host = localhost,
                 %% 'local' is the address family used for
                 %% Unix Domain Sockets.
                 family = local,
                 %% 'stream' is the convention chosen to
                 %% represent to SOCK_STREAM socket type.
                 protocol = stream}.


-spec get_creation(SocketPathname) -> Creation :: 1..16#FFFFFFFF when
      SocketPathname :: string().
%% ---------------------------------------------------------------------
%% Return the Creation number for the Erlang node which is accessible
%% through the Unix Domain Socket listening on the file pathname
%% SocketPathname.
%%
%% The Creation number has to change for different instances of Erlang
%% nodes created with the same distribution name. It is stored in every
%% process identifier sent to another node, so that process identifiers
%% from one given node do not remain valid when sent to a new node
%% instance with the same name.
%%
%% Support small Creation numbers in the 1..3 range with distribution
%% protocol version 5 and 32-bit big Creation numbers in the range
%% 4..4294967295 with protocol version 6. The value 0 must be avoided
%% for normal operations as it is used as a wild card for debug purpose.
%%
%% For big Creations numbers, simply create a new random value each time.
%%
%% For small Creation numbers in the 1..3 range, the value is saved on
%% the filesystem in the file pathname SocketPathname with the added
%% ".uds" extension, stored using a 1 byte character. With this convention,
%% an Erlang node can retrieve the previous value from the filesystem
%% on a new invocation to make sure the Creation number is incremented.
%% ---------------------------------------------------------------------
get_creation(SocketPathname) ->
    %% Check the distribution protocol version
    case ?ERL_DIST_VER of
        6 ->
            %% For big Creations numbers, simply create a new random value
            %% each time, while avoiding the 0..3 range.
            3 + rand:uniform((1 bsl 32) - 4);
        _ ->
            %% For small Creation numbers, open the file ending with the
            %% ".uds" extension read/write, in binary mode (so that read
            %% operations return binaries), in raw mode (so that the file
            %% operations bypass the need for a file server, which is not
            %% available and registered yet during the early boot stage).
            case file:open(SocketPathname ++ ".uds",
                           [raw, read, write, binary]) of
                {ok, File} ->
                    %% Read 1 byte from File, normally its full content
                    Creation =
                        case file:read(File, 1) of
                            %% Try to match this 1-byte binary with a
                            %% character in the $1..$2 range.
                            {ok, <<X:8>>} when $0 < X, X < $3 ->
                                %% Increment the previous value if found
                                X + 1;
                            _ ->
                                %% Start or wrap back to $1 otherwise
                                $1
                        end,
                    %% Write the new Creation number in position 0 in File,
                    %% so that it overwrites the previous value.
                    file:pwrite(File, 0, <<Creation:8>>),
                    file:close(File),
                    %% Convert the 1 byte character to its integer value
                    binary_to_integer(<<Creation:8>>);

                {error, _Reason} ->
                    %% The file couldn't be opened, return a random value
                    rand:uniform(3)
            end
    end.


-spec accept(ListeningSocket) -> AcceptPid :: pid() when
      ListeningSocket :: gen_tcp:socket().
%% ---------------------------------------------------------------------
%% Accept new connection attempts from other Erlang nodes.
%%
%% accept/1 spawns an accept_loop process that accepts connections and
%% returns its process identifier.
%%
%% The caller of the accept_loop is a representative for net_kernel and
%% is identified as Kernel below. This may or may not be the process
%% registered as net_kernel.
%%
%% When a new connection is accepted, the accept_loop creates a distribution
%% controller, whose job is to dispatch traffic on the connection, then
%% it informs Kernel about the accepted connection.
%%
%% The ListeningSocket argument will be the same as the ListeningSocket handle
%% of the return value from the listen/1 callback above, i.e. the listening
%% Unix Domain Socket through which this Erlang node is accessible.
%%
%% accept/1 is called only once when the distribution protocol is brought up.
%% ---------------------------------------------------------------------
accept(ListeningSocket) ->
    spawn_opt(?MODULE, accept_loop, [self(), ListeningSocket],
              %% Spawn on max priority
              [link, {priority, max}]).

accept_loop(Kernel, ListeningSocket) ->
    case gen_tcp:accept(ListeningSocket) of
        {ok, Socket} ->
            %% Create a distribution controller process in charge of the
            %% accepted connection, available through Socket.
            DistCtrl = spawn_dist_controller(Socket),
            ?trace("~p~n",[{?MODULE, accept, accepted, Socket,
                            DistCtrl, self()}]),
            %% Set this process as the new controlling process of Socket, i.e.
            %% the process that receives messages from Socket.
            flush_controller(DistCtrl, Socket),
            gen_tcp:controlling_process(Socket, DistCtrl),
            flush_controller(DistCtrl, Socket),
            %% Inform Kernel about the accepted connection. DistCtrl is
            %% passed as the identifier of the distribution controller,
            %% 'local' as the address family for Unix Domain Sockets and
            %% 'stream' as the protocol for the SOCK_STREAM socket type.
            Kernel ! {accept, self(), DistCtrl, local, stream},
            receive
                %% The request was accepted. SupervisorPid is the process
                %% identifier of the connection supervisor process (created
                %% in the accept_connection/5 callback).
                {Kernel, controller, SupervisorPid} ->
                    %% Set SupervisorPid as the supervisor of the
                    %% distribution controller.
                    call_controller(DistCtrl, {supervisor, SupervisorPid}),
                    %% And continue with the handshake.
                    SupervisorPid ! {self(), controller};
                %% The request was rejected, this is a fatal error, the
                %% accept_loop process should terminate.
                {Kernel, unsupported_protocol} ->
                    exit(unsupported_protocol)
            end,
            accept_loop(Kernel, ListeningSocket);
        {error, closed} ->
            ?trace("~p~n",[{?MODULE, accept, ListeningSocket,
                            closed, self()}]),
            exit(closing_connection);
        Error ->
            ?trace("~p~n",[{?MODULE, accept, ListeningSocket,
                            Error, self()}]),
            exit(Error)
    end.


-spec accept_connection(AcceptPid, DistCtrl, MyNode, Allowed, SetupTime) ->
                               ConnectionSupervisorPid :: pid() when
      AcceptPid :: pid(),
      DistCtrl :: pid(),
      MyNode :: node(),
      Allowed :: list(),
      SetupTime :: non_neg_integer().
%% ---------------------------------------------------------------------
%% accept_connection/5 spawns an accept_supervisor process that accepts
%% a new connection attempt from another Erlang node and performs the
%% handshake with the other side. Callbacks and other information needed
%% for the handshake are provided in a #hs_data{} record. If the handshake
%% successfully completes, this process will continue to function as the
%% connection supervisor as long as the connection is up.
%%
%% The process identifier of this accept_supervisor is returned.
%%
%% The caller of accept_supervisor is a representative for net_kernel and
%% is identified as Kernel below.
%%
%% AcceptPid is the process identifier created by accept/1.
%%
%% DistCtrl is the identifier of the distribution controller process in
%% charge of the connection, as created by the accept_loop process above.
%%
%% MyNode is the name of this node.
%%
%% The Allowed argument is to be passed during the handshake.
%%
%% SetupTime is used to create a setup timer, to be passed during the
%% handshake.
%% ---------------------------------------------------------------------
accept_connection(AcceptPid, DistCtrl, MyNode, Allowed, SetupTime) ->
    spawn_opt(?MODULE, accept_supervisor,
              [self(), AcceptPid, DistCtrl, MyNode, Allowed, SetupTime],
              dist_util:net_ticker_spawn_options()).

accept_supervisor(Kernel, AcceptPid, DistCtrl, MyNode, Allowed, SetupTime) ->
    ?trace("~p~n", [{?MODULE, accept_connection, self()}]),
    receive
        {AcceptPid, controller} ->
            Timer = dist_util:start_timer(SetupTime),
            HSData0 = hs_data_common(DistCtrl),
            HSData = HSData0#hs_data{
                       kernel_pid = Kernel,
                       this_node = MyNode,
                       socket = DistCtrl,
                       timer = Timer,
                       allowed = Allowed,
                       %% Return the remote address using the #net_address{}
                       %% record format.
                       f_address =
                           fun(_,_) ->
                                   #net_address{
                                      %% Unix Domain Sockets don't have a
                                      %% socket address in a {local, Pathname}
                                      %% format on the 'connect' side, which
                                      %% is unnamed when not bound.
                                      address = [],
                                      %% Simply use 'localhost' as a convention
                                      %% as host is not used in net_kernel.
                                      host = localhost,
                                      %% 'local' is the address family used
                                      %% for Unix Domain Sockets.
                                      family = local,
                                      %% 'stream' is the convention chosen to
                                      %% represent the SOCK_STREAM socket type.
                                      protocol = stream}
                           end
                      },
            ?trace("handshake_other_started received on node (~p)~n",
                   [MyNode]),
            dist_util:handshake_other_started(HSData)
    end.

%% ---------------------------------------------------------------------
%% Define common values of the handshake data record, defined in
%% kernel/include/dist_util.hrl
%% ---------------------------------------------------------------------
hs_data_common(DistCtrl) ->
    TickHandler = call_controller(DistCtrl, tick_handler),
    Socket = call_controller(DistCtrl, socket),
    #hs_data{%% Flags the node should use. Simply use the default config
             this_flags = 0,
             %% Send Packet to the other side
             f_send =
                 fun(Ctrl, Packet) ->
                         call_controller(Ctrl, {send, Packet})
                 end,
             %% Receive a packet of Length bytes, within Timeout milliseconds
             f_recv =
                 fun(Ctrl, Length, Timeout) ->
                         case call_controller(Ctrl, {recv, Length, Timeout}) of
                             {ok, Bin} when is_binary(Bin) ->
                                 {ok, binary_to_list(Bin)};
                             Other -> Other
                         end
                 end,
             %% Set the Socket options before nodeup is delivered to net_kernel
             f_setopts_pre_nodeup =
                 fun(Ctrl) ->
                         call_controller(Ctrl, pre_nodeup)
                 end,
             %% Set the Socket options after nodeup is delivered to net_kernel
             f_setopts_post_nodeup =
                 fun(Ctrl) ->
                         call_controller(Ctrl, post_nodeup)
                 end,
             %% Get the identifier of the low level entity that handles the
             %% connection (often DistCtrl itself).
             f_getll =
                 fun(Ctrl) ->
                         call_controller(Ctrl, getll)
                 end,

             %% The following two functions are used in the tick loop:
             %% Send a 'tick' request to the tick handler
             mf_tick = fun (Ctrl) when Ctrl == DistCtrl ->
                               TickHandler ! tick
                       end,
             %% Get stats about send, received or pending packets
             mf_getstat =
                 fun(Ctrl) when Ctrl == DistCtrl ->
                         case inet:getstat(Socket,
                                           [recv_cnt, send_cnt, send_pend]) of
                             {ok, Stat} ->
                                 split_stat(Stat, 0, 0, 0);
                             Error ->
                                 Error
                         end
                 end,

             %% New in kernel-5.1 (OTP 19.1):

             %% List of Socket options to set on future connections
             mf_setopts = fun (Ctrl, Options) when Ctrl == DistCtrl ->
                               setopts(Socket, Options)
                          end,
             %% List of Socket options to read for future connections
             mf_getopts = fun (Ctrl, Options) when Ctrl == DistCtrl ->
                               getopts(Socket, Options)
                          end,

             %% New in kernel-6.0 (OTP 21.0):

             %% Function called when the handshake has completed and the
             %% distribution connection is up. The distribution controller
             %% can begin dispatching traffic.
             %%
             %% DHandle is a distribution handle identifying the connection and
             %% needed for a few erlang:dist_ctrl_xxx built-in functions.
             f_handshake_complete = fun (Ctrl, Node, DHandle) ->
                                            call_controller(Ctrl,
                                                            {handshake_complete,
                                                             Node, DHandle})
                                    end
             %% Optional fields in the handshake data record:
             %% add_flags       Distribution flags to add to the connection
             %% reject_flags    Distribution flags to reject
             %% require_flags   Distribution flags that are required

             %% New in kernel-7.0 (OTP 23.0):

             %% other_creation  Creation number of the other node, passed
             %%                 in the new handshake protocol introduced
             %%                 in distribution protocol version 6.
             %% this_creation   Used with dynamic node name, that can be
             %%                 requested by a connecting node from the
             %%                 accepting node it first connects to, as
             %%                 part of the handshake. This Creation
             %%                 number to set is received at the same time.
      }.


%% ---------------------------------------------------------------------
%% Return the Stat output from inet:getstat in the format expected by
%% the mf_getstat fun as defined in dist_util.hrl
%% ---------------------------------------------------------------------
split_stat([{recv_cnt, R}|Stat], _, W, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_cnt, W}|Stat], R, _, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_pend, P}|Stat], R, W, _) ->
    split_stat(Stat, R, W, P);
split_stat([], R, W, P) ->
    {ok, R, W, P}.


-spec setopts(ListeningSocket, Options) -> ok | {error, Error} when
      ListeningSocket :: gen_tcp:socket(),
      Options :: [inet:socket_setopt()],
      Error :: inet:posix().
%% ---------------------------------------------------------------------
%% Set the list of options to apply on future connections.
%%
%% ListeningSocket is the handle originally passed from the listen/1 callback.
%%
%% Options is the list of options to apply, with a set of forbidden ones.
%% ---------------------------------------------------------------------
setopts(ListeningSocket, Options) ->
    case [Option || {K, _} = Option <- Options,
                    K =:= active orelse K =:= deliver orelse K =:= packet] of
        [] -> inet:setopts(ListeningSocket, Options);
        Opts1 -> {error, {badopts, Opts1}}
    end.


-spec getopts(ListeningSocket, Options) -> {ok, OptionValues} |
                                           {error, Error} when
      ListeningSocket :: gen_tcp:socket(),
      Options :: [inet:socket_getopt()],
      OptionValues :: [inet:socket_setopt() | gen_tcp:pktoptions_value()],
      Error :: inet:posix().
%% ---------------------------------------------------------------------
%% Set the list of options to read for future connections.
%%
%% ListeningSocket is the handle originally passed from the listen/1 callback.
%%
%% Options is the list of options.
%% ---------------------------------------------------------------------
getopts(ListeningSocket, Options) ->
    inet:getopts(ListeningSocket, Options).


-spec setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
                   ConnectionSupervisorPid :: pid() when
      Node :: node(),
      Type :: atom(),
      MyNode :: node(),
      LongOrShortNames :: shortnames | longnames,
      SetupTime :: non_neg_integer().
%% ---------------------------------------------------------------------
%% setup/5 spawns a setup_supervisor process that initiates a new connection
%% attempt with another Erlang node and performs the handshake with the
%% other side. Callbacks and other information needed for the handshake are
%% provided in a #hs_data{} record. If the handshake successfully completes,
%% this process will continue to function as a connection supervisor as long
%% as the connection is up (still 'ticking').
%%
%% The process identifier of this setup_supervisor is returned.
%%
%% The spawned setup_supervisor process creates a separate distribution
%% controller responsible for dispatching traffic on the connection.
%%
%% The caller of setup_supervisor is a representative for net_kernel and
%% is identified as Kernel below.
%%
%% Node is the name of the other Erlang node to connect to, it defines the
%% listening Unix Domain Socket it listens on. The socket file pathname is
%% the part before the '@' character for a full node name in Name@Host format
%% (whether short or long names are used) or the entire Node name otherwise.
%%
%% Type is the connection type to be passed during the handshake.
%%
%% MyNode is the name of this node.
%%
%% The LongOrShortNames argument is either the 'longnames' atom or the
%% 'shortnames' atom, indicating whether long or short names are used. This
%% distinction is simply ignored as all nodes are running locally on the
%% same host with this alternative Erlang distribution protocol.
%%
%% SetupTime is used to create a setup timer, to be passed during the
%% handshake.
%% ---------------------------------------------------------------------
setup(Node, Type, MyNode, _LongOrShortNames, SetupTime) ->
    spawn_opt(?MODULE, setup_supervisor,
	      [self(), Node, Type, MyNode, SetupTime],
              dist_util:net_ticker_spawn_options()).

setup_supervisor(Kernel, Node, Type, MyNode, SetupTime) ->
    ?trace("~p~n", [{?MODULE, setup, self(), Node}]),
    %% No need for a host name lookup as this alternative Erlang distribution
    %% protocol only supports nodes running locally on the same host.
    %%
    %% Retrieve the socket pathname from Node.
    SocketPathname = get_pathname(Node),
    %% The options passed to connect:
    %% - {local, Pathname :: binary() | string()} indicates to use a
    %%   Unix Domain Socket and defines which file pathname to connect to.
    %% - binary, to have each packet delivered as a binary.
    %% - {active, false} sets the socket in passive mode, meaning
    %%   the packets must be explicitly retrieved by calling recv/2,3.
    %% - {packet, 2} specifies a packet header length of 2 bytes, which
    %%   is expected in every message of the distribution protocol
    %%   until the initial handshake completes.
    %%
    %% As documented, the port number must weirdly be set to 0 when using
    %% gen_tcp API functions for Unix Domain Sockets.
    case gen_tcp:connect({local, SocketPathname},
                         0,
                         [binary, {active, false}, {packet, 2}]) of
        {ok, Socket} ->
            Timer = dist_util:start_timer(SetupTime),
            %% Create a distribution controller process in charge of
            %% dispatching traffic on the connection to the other Erlang node,
            %% available through Socket.
            DistCtrl = spawn_dist_controller(Socket),
            %% Set this process as the supervisor of the distribution controller
            call_controller(DistCtrl, {supervisor, self()}),
            %% Set this process as the new controlling process of Socket, i.e.
            %% the process that receives messages from Socket.
            flush_controller(DistCtrl, Socket),
            gen_tcp:controlling_process(Socket, DistCtrl),
            flush_controller(DistCtrl, Socket),
            %% Get the remote socket address in a {local, Pathname} format
            {ok, SocketAddress} = inet:peername(Socket),
            HSData0 = hs_data_common(DistCtrl),
            HSData = HSData0#hs_data{
                       kernel_pid = Kernel,
                       other_node = Node,
                       this_node = MyNode,
                       socket = DistCtrl,
                       timer = Timer,
                       other_version = ?ERL_DIST_VER,
                       request_type = Type,
                       %% Return the remote address using the #net_address{}
                       %% record format.
                       f_address =
                           fun(_,_) ->
                                   #net_address{
                                      address = SocketAddress,
                                      %% Simply use 'localhost' as a convention
                                      %% as host is not used in net_kernel.
                                      host = localhost,
                                      %% 'local' is the address family used
                                      %% for Unix Domain Sockets.
                                      family = local,
                                      %% 'stream' is the convention chosen to
                                      %% represent the SOCK_STREAM socket type.
                                      protocol = stream}
                           end
                      },
            %% Start the handshake and check that the connection is up
            %% (still 'ticking').
            ?trace("handshake_we_started with node on socket (~p)~n",
                   [SocketPathname]),
            dist_util:handshake_we_started(HSData);

        _Other ->
            ?trace("gen_tcp:connect to node (~p) failed (~p).~n",
                   [Node, _Other]),
            ?shutdown(Node)
    end.


-spec close(ListeningSocket) -> ok when
      ListeningSocket :: gen_tcp:socket().
%% ---------------------------------------------------------------------
%% Close the listening Unix Domain Socket through which this Erlang node
%% is accessible.
%% ---------------------------------------------------------------------
close(ListeningSocket) ->
    %% Get the listening socket address in a {local, Pathname} format
    {ok, SocketAddress} = inet:sockname(ListeningSocket),
    {local, SocketPathname} = SocketAddress,
    %% Remove the socket file from the filesystem. The raw option is used
    %% to bypass the need for a file server, which may not be available
    %% and registered anymore (for instance during a node shutdown phase).
    file:delete(SocketPathname, [raw]),
    gen_tcp:close(ListeningSocket).


-spec get_pathname(Node) -> Pathname when
      Node :: node(),
      Pathname :: string().
%% ---------------------------------------------------------------------
%% Retrieve the socket pathname from Node.
%%
%% The socket pathname is the part before the '@' character for a full node
%% name in Name@Host format (whether short or long names are used) or the
%% entire Node name otherwise.
%% ---------------------------------------------------------------------
get_pathname(Node) ->
    NodeString = atom_to_list(Node),
    lists:takewhile(fun(C) -> C =/= $@ end, NodeString).


%% ---------------------------------------------------------------------
%% Flush all the tcp and tcp_closed received messages and transfer them
%% to the Pid process. This is used when setting Pid as the new controlling
%% process of Socket. This function needs to be called twice: just before
%% and right after calling controlling_process(Socket, Pid).
%% ---------------------------------------------------------------------
flush_controller(Pid, Socket) ->
    receive
        {tcp, Socket, Data} ->
            Pid ! {tcp, Socket, Data},
            flush_controller(Pid, Socket);
        {tcp_closed, Socket} ->
            Pid ! {tcp_closed, Socket},
            flush_controller(Pid, Socket)
    after 0 ->
            ok
    end.


%% ---------------------------------------------------------------------
%% Distribution controller processes
%%
%% There will be five parties working together when the
%% connection is up:
%%
%% - The gen_tcp socket. It provides a connection to the other
%%   node through a Unix Domain Socket.
%%
%% - The output handler. It will dispatch all outgoing traffic
%%   from this node to the remote node through the socket. This
%%   process is registered as a distribution controller for this
%%   connection.
%%
%% - The input handler. It will dispatch all incoming traffic
%%   from the remote node to this node through the socket. This
%%   process is also the socket controlling process, receiving
%%   incoming traffic in active mode using {active, N}.
%%
%% - The tick handler. It sends asynchronous tick messages to the
%%   socket to check for node liveness. It executes on max priority
%%   since it is important to get ticks through to the other end.
%%
%% - The connection supervisor, provided by dist_util. It monitors
%%   traffic and issues tick requests to the tick handler when
%%   no outgoing traffic is happening. If no incoming traffic is
%%   received, the other node is considered to be down and the
%%   connection is closed. This process also executes on max priority.
%%
%%   These parties are linked together so should one of them fail,
%%   all of them are terminated and the connection is taken down.
%% ---------------------------------------------------------------------


%% In order to avoid issues with lingering signal binaries,
%% enable off-heap message queue data as well as fullsweep
%% after 0. The fullsweeps will be cheap since there is more
%% or less no live data.
-define(DIST_CONTROLLER_COMMON_SPAWN_OPTS,
        [{message_queue_data, off_heap},
         {fullsweep_after, 0}]).


%% ---------------------------------------------------------------------
%% Setup the distribution controller by spawning the tick handler
%% and starting the setup loop.
%% ---------------------------------------------------------------------
spawn_dist_controller(Socket) ->
    spawn_opt(fun() -> dist_controller_setup(Socket) end,
              %% Spawn on max priority
              [{priority, max}] ++ ?DIST_CONTROLLER_COMMON_SPAWN_OPTS).

dist_controller_setup(Socket) ->
    TickHandler = spawn_opt(fun() -> tick_handler(Socket) end,
              %% Spawn on max priority
              [link, {priority, max}] ++ ?DIST_CONTROLLER_COMMON_SPAWN_OPTS),
    dist_controller_setup_loop(Socket, TickHandler, undefined).


%% ---------------------------------------------------------------------
%% During the handshake phase, loop in dist_controller_setup_loop(). When the
%% connection is up, spawn an input handler and continue as output handler.
%%
%% Sup, the connection supervisor
%% ---------------------------------------------------------------------
dist_controller_setup_loop(Socket, TickHandler, Sup) ->
    receive
        {tcp_closed, Socket} ->
            exit(connection_closed);

        %% Set Pid as the connection supervisor, link with it and
        %% send the linking result back.
        {Ref, From, {supervisor, Pid}} ->
            Res = link(Pid),
            From ! {Ref, Res},
            dist_controller_setup_loop(Socket, TickHandler, Pid);

        %% Send the tick handler to the From process
        {Ref, From, tick_handler} ->
            From ! {Ref, TickHandler},
            dist_controller_setup_loop(Socket, TickHandler, Sup);

        %% Send the socket to the From process
        {Ref, From, socket} ->
            From ! {Ref, Socket},
            dist_controller_setup_loop(Socket, TickHandler, Sup);

        %% Send Packet onto the socket and send the result back
        {Ref, From, {send, Packet}} ->
            Res = gen_tcp:send(Socket, Packet),
            From ! {Ref, Res},
            dist_controller_setup_loop(Socket, TickHandler, Sup);

        %% Receive a packet of Length bytes, within Timeout milliseconds
        {Ref, From, {recv, Length, Timeout}} ->
            Res = gen_tcp:recv(Socket, Length, Timeout),
            From ! {Ref, Res},
            dist_controller_setup_loop(Socket, TickHandler, Sup);

        %% Send the low level distribution controller pid to the From process
        {Ref, From, getll} ->
            From ! {Ref, {ok, self()}},
            dist_controller_setup_loop(Socket, TickHandler, Sup);

        %% Set the Socket options just before the connection is established
        %% for normal data traffic and before nodeup is delivered. A nodeup
        %% message is delivered when a new node is connected.
        {Ref, From, pre_nodeup} ->
            %% Switch the distribution protocol to a packet header of
            %% 4 bytes which is used to store the length of each packet
            %% sent over the streamed Unix Domain Sockets.
            Res = inet:setopts(Socket,
                               [{active, false},
                                {packet, 4}]),
            From ! {Ref, Res},
            dist_controller_setup_loop(Socket, TickHandler, Sup);

        %% Set the Socket options just after the connection is established
        %% for normal data traffic and after nodeup is delivered.
        {Ref, From, post_nodeup} ->
            %% Switch the distribution protocol to a packet header of
            %% 4 bytes, as explained above.
            %% The previous pre_nodeup case should normally be enough.
            Res = inet:setopts(Socket,
                               [{active, false},
                                {packet, 4}]),
            From ! {Ref, Res},
            dist_controller_setup_loop(Socket, TickHandler, Sup);

        %% The handshake has completed and the connection is up, the
        %% distribution controller can begin dispatching traffic.
        {Ref, From, {handshake_complete, _Node, DHandle}} ->
            From ! {Ref, ok},
            %% Handshake complete! Begin dispatching traffic

            %% Use a separate process for dispatching input. This
            %% is not necessary, but it enables parallel execution
            %% of independent work loads at the same time as it
            %% simplifies the implementation.
            InputHandler = spawn_opt(
                             fun() -> dist_controller_input_handler(DHandle,
                                                                    Socket,
                                                                    Sup)
                             end,
                             [link] ++ ?DIST_CONTROLLER_COMMON_SPAWN_OPTS),

            %% Set this process as the new controlling process of Socket, i.e.
            %% the process that receives messages from Socket.
            flush_controller(InputHandler, Socket),
            gen_tcp:controlling_process(Socket, InputHandler),
            flush_controller(InputHandler, Socket),

            %% Register the input handler process
            erlang:dist_ctrl_input_handler(DHandle, InputHandler),

            %% Inform the input handler that it has been registered
            InputHandler ! DHandle,

            %% From now on, execute on normal priority
            process_flag(priority, normal),

            %% Request notification when outgoing data is available to fetch.
            %% A dist_data message will be sent.
            erlang:dist_ctrl_get_data_notification(DHandle),

            %% And continue as output handler
            dist_controller_output_handler(DHandle, Socket)
    end.


%% ---------------------------------------------------------------------
%% Call the distribution controller with Message and get Result in return.
%%
%% The distribution controller is monitored to be notified if it has been
%% terminated.
%% ---------------------------------------------------------------------
call_controller(DistCtrl, Message) ->
    Ref = erlang:monitor(process, DistCtrl),
    DistCtrl ! {Ref, self(), Message},
    receive
        {Ref, Result} ->
            erlang:demonitor(Ref, [flush]),
            Result;
        {'DOWN', Ref, process, DistCtrl, Reason} ->
            exit({dist_controller_exit, Reason})
    end.


%% Use active 10 for good throughput while still maintaining back-pressure
%% if the input controller isn't able to handle all incoming messages.
%% This approach is re-used as-is from the gen_tcp_dist.erl example.
-define(ACTIVE_INPUT, 10).


%% ---------------------------------------------------------------------
%% Input handler
%%
%% Dispatch all traffic from the remote node coming to this node through
%% the socket.
%% ---------------------------------------------------------------------
dist_controller_input_handler(DHandle, Socket, Sup) ->
    link(Sup),
    receive
        %% Wait for the input handler to be registered before starting
        %% to deliver incoming data.
        DHandle ->
            dist_controller_input_loop(DHandle, Socket, 0)
    end.


dist_controller_input_loop(DHandle, Socket, N) when N =< ?ACTIVE_INPUT/2 ->
    %% Set the socket in active mode and define the number of received data
    %% packets that will be delivered as {tcp, Socket, Data} messages.
    inet:setopts(Socket, [{active, ?ACTIVE_INPUT - N}]),
    dist_controller_input_loop(DHandle, Socket, ?ACTIVE_INPUT);

dist_controller_input_loop(DHandle, Socket, N) ->
    receive
        %% In active mode, data packets are delivered as messages
        {tcp, Socket, Data} ->
            %% When data is received from the remote node, deliver it
            %% to the local node.
            try erlang:dist_ctrl_put_data(DHandle, Data)
            catch _ : _ -> death_row()
            end,
            %% Decrease the counter when looping so that the socket is
            %% set with {active, Count} again to receive more data.
            dist_controller_input_loop(DHandle, Socket, N-1);

        %% Connection to remote node terminated
        {tcp_closed, Socket} ->
            exit(connection_closed);

        %% Ignore all other messages
        _ ->
            dist_controller_input_loop(DHandle, Socket, N)
    end.


%% ---------------------------------------------------------------------
%% Output handler
%%
%% Dispatch all outgoing traffic from this node to the remote node through
%% the socket.
%% ---------------------------------------------------------------------
dist_controller_output_handler(DHandle, Socket) ->
    receive
        dist_data ->
            %% Available outgoing data to send from this node
            try dist_controller_send_data(DHandle, Socket)
            catch _ : _ -> death_row()
            end,
            dist_controller_output_handler(DHandle, Socket);

        _ ->
            %% Ignore all other messages
            dist_controller_output_handler(DHandle, Socket)
    end.

dist_controller_send_data(DHandle, Socket) ->
    %% Fetch data from the local node to be sent to the remote node
    case erlang:dist_ctrl_get_data(DHandle) of
        none ->
            %% Request notification when more outgoing data is available.
            %% A dist_data message will be sent.
            erlang:dist_ctrl_get_data_notification(DHandle);
        Data ->
            socket_send(Socket, Data),
            %% Loop as long as there is more data available to fetch
            dist_controller_send_data(DHandle, Socket)
    end.


%% ---------------------------------------------------------------------
%% Tick handler
%%
%%
%% The tick handler process writes a tick message to the socket when it
%% receives a 'tick' request from the connection supervisor.
%% ---------------------------------------------------------------------
tick_handler(Socket) ->
    ?trace("~p~n", [{?MODULE, tick_handler, self()}]),
    receive
        tick ->
            %% May block due to busy port...
            socket_send(Socket, "");
        _ ->
            ok
    end,
    tick_handler(Socket).


%% ---------------------------------------------------------------------
%% Send Data on Socket
%% ---------------------------------------------------------------------
socket_send(Socket, Data) ->
    try gen_tcp:send(Socket, Data) of
        ok -> ok;
        {error, Reason} -> death_row({send_error, Reason})
    catch
        Type : Reason -> death_row({send_error, {Type, Reason}})
    end.


%% ---------------------------------------------------------------------
%% death_row
%%
%% When the connection is on its way down, operations begin to fail. We
%% catch the failures and call this function waiting for termination. We
%% should be terminated by one of our links to the other involved parties
%% that began bringing the connection down. By waiting for termination we
%% avoid altering the exit reason for the connection teardown. We however
%% limit the wait to 5 seconds and bring down the connection ourselves if
%% not terminated...
%% ---------------------------------------------------------------------
death_row() ->
    death_row(connection_closed).

death_row(normal) ->
    %% We do not want to exit with normal exit reason since it won't
    %% bring down linked processes...
    death_row();

death_row(Reason) ->
    receive after 5000 -> exit(Reason) end.


%% ---------------------------------------------------------------------
%% to_string(S) -> string()
%%
%%
%% to_string/1 creates a string from an atom or a string.
%% ---------------------------------------------------------------------
to_string(S) when is_atom(S) -> atom_to_list(S);
to_string(S) when is_list(S) -> S.
