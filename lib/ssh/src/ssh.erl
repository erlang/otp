%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2004-2025. All Rights Reserved.
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

-module(ssh).
-moduledoc """
Main API of the ssh application

This is the interface module for the `SSH` application. The Secure Shell (SSH)
Protocol is a protocol for secure remote login and other secure network services
over an insecure network. See [ssh](ssh_app.md#supported) for details of
supported RFCs, versions, algorithms and unicode handling.

With the SSH application it is possible to start _clients_ and to start
_daemons_ (servers).

Clients are started with `connect/2`, `connect/3` or `connect/4`. They open an
encrypted connection on top of TCP/IP. In that encrypted connection one or more
channels could be opened with
[ssh_connection:session_channel/2,4](`ssh_connection:session_channel/2`).

Each channel is an isolated "pipe" between a client-side process and a
server-side process. Those process pairs could handle for example file transfers
(sftp) or remote command execution (shell, exec and/or cli). If a custom shell
is implemented, the user of the client could execute the special commands
remotely. Note that the user is not necessarily a human but probably a system
interfacing the SSH app.

A server-side subssystem (channel) server is requested by the client with
`ssh_connection:subsystem/4`.

A server (daemon) is started with [daemon/1](`daemon/2`), `daemon/2` or
[daemon/3](`daemon/2`). Possible channel handlers (subsystems) are declared with
the [subsystem](`t:subsystem_daemon_option/0`) option when the daemon is
started.

To just run a shell on a remote machine, there are functions that bundles the
needed three steps needed into one: [shell/1,2,3](`shell/1`). Similarly, to just
open an sftp (file transfer) connection to a remote machine, the simplest way is
to use [ssh_sftp:start_channel/1,2,3](`ssh_sftp:start_channel/1`).

To write your own client channel handler, use the behaviour
`m:ssh_client_channel`. For server channel handlers use `m:ssh_server_channel`
behaviour (replaces ssh_daemon_channel).

Both clients and daemons accept options that control the exact behaviour. Some
options are common to both. The three sets are called
[Client Options](`t:client_options/0`), [Daemon Options](`t:daemon_options/0`)
and [Common Options](`t:common_options/0`).

The descriptions of the options uses the
[Erlang Type Language](`e:system:typespec.md`) with explaining text.

> #### Note {: .info }
>
> See also [SSH Application Reference](index.html) and [Examples](using_ssh.md) section.

## Keys and files

A number of objects must be present for the SSH application to work. Those
objects are per default stored in files. The default names, paths and file
formats are the same as for [OpenSSH](http://www.openssh.com). Keys could be
generated with the `ssh-keygen` program from OpenSSH. See the
[User's Guide](using_ssh.md#running-an-erlang-ssh-daemon).

The paths could easily be changed by options:
[`user_dir`](`t:ssh_file:user_dir_common_option/0`) and
[`system_dir`](`t:ssh_file:system_dir_daemon_option/0`).

A completely different storage could be interfaced by writing callback modules
using the behaviours `m:ssh_client_key_api` and/or `m:ssh_server_key_api`. A
callback module is installed with the option
[`key_cb`](`t:key_cb_common_option/0`) to the client and/or the daemon.

### Daemons

The keys are by default stored in files:

- Mandatory: one or more _Host key(s)_, both private and public. Default is to
  store them in the directory `/etc/ssh` in the files

  - `ssh_host_dsa_key` and `ssh_host_dsa_key.pub`
  - `ssh_host_rsa_key` and `ssh_host_rsa_key.pub`
  - `ssh_host_ecdsa_key` and `ssh_host_ecdsa_key.pub`

  The host keys directory could be changed with the option
  [`system_dir`](`t:ssh_file:system_dir_daemon_option/0`).

- Optional: one or more _User's public key_ in case of `publickey`
  authorization. Default is to store them concatenated in the file
  `.ssh/authorized_keys` in the user's home directory.

  The user keys directory could be changed with the option
  [`user_dir`](`t:ssh_file:user_dir_common_option/0`).

### Clients

The keys and some other data are by default stored in files in the directory
`.ssh` in the user's home directory.

The directory could be changed with the option
[`user_dir`](`t:ssh_file:user_dir_common_option/0`).

- Optional: a list of _Host public key(s)_ for previously connected hosts. This
  list is handled by the SSH application without any need of user assistance.
  The default is to store them in the file `known_hosts`.

  The `t:host_accepting_client_options/0` are associated with this list of keys.

- Optional: one or more _User's private key(s)_ in case of `publickey`
  authorization. The default files are
  - `id_dsa` and `id_dsa.pub`
  - `id_rsa` and `id_rsa.pub`
  - `id_ecdsa` and `id_ecdsa.pub`
""".

-include("ssh.hrl").
-include("ssh_connect.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/inet.hrl").

-export([start/0, start/1, stop/0,
	 connect/2, connect/3, connect/4,
	 close/1, connection_info/2,
         connection_info/1,
	 channel_info/3,
	 daemon/1, daemon/2, daemon/3,
	 daemon_info/1, daemon_info/2,
         daemon_replace_options/2,
         set_sock_opts/2, get_sock_opts/2,
	 default_algorithms/0,
         chk_algos_opts/1,
	 stop_listener/1, stop_listener/2,  stop_listener/3,
	 stop_daemon/1, stop_daemon/2, stop_daemon/3,
	 shell/1, shell/2, shell/3,
         tcpip_tunnel_from_server/5, tcpip_tunnel_from_server/6,
         tcpip_tunnel_to_server/5, tcpip_tunnel_to_server/6
	]).

%% In move from public_key
-export([hostkey_fingerprint/1, hostkey_fingerprint/2
        ]).
         

%%% Internal export
-export([is_host/2, update_lsocket/3]).

-behaviour(ssh_dbg).
-export([ssh_dbg_trace_points/0, ssh_dbg_flags/1, ssh_dbg_on/1, ssh_dbg_off/1,
         ssh_dbg_format/2, ssh_dbg_format/3]).

%%% "Deprecated" types export:
-export_type([ssh_daemon_ref/0, ssh_connection_ref/0, ssh_channel_id/0]).
-doc(#{group => <<"Deprecated">>}).
-opaque ssh_daemon_ref()     :: daemon_ref().
-doc(#{group => <<"Deprecated">>}).
-opaque ssh_connection_ref() :: connection_ref().
-doc(#{group => <<"Deprecated">>}).
-opaque ssh_channel_id()     :: channel_id().


%%% Type exports
-export_type([daemon_ref/0,
              connection_ref/0,
	      channel_id/0,
              client_options/0, client_option/0,
              daemon_options/0, daemon_option/0,
              common_options/0,
              role/0,
              subsystem_spec/0,
              algs_list/0,
              double_algs/1,
              modify_algs_list/0,
              alg_entry/0,
              kex_alg/0,
              pubkey_alg/0,
              cipher_alg/0,
              mac_alg/0,
              compression_alg/0,
              host/0,
              open_socket/0,
              ip_port/0
	     ]).


-doc """
Opaque data type representing a daemon.

Returned by the functions [`daemon/1,2,3`](`daemon/1`).
""".
-opaque daemon_ref()         :: pid() .
-doc """
Opaque data type representing a channel inside a connection.

Returned by the functions
[ssh_connection:session_channel/2,4](`ssh_connection:session_channel/2`).
""".
-opaque channel_id()     :: non_neg_integer().
-doc """
Opaque data type representing a connection between a client and a server
(daemon).

Returned by the functions [`connect/2,3,4`](`connect/3`) and
[`ssh_sftp:start_channel/2,3`](`ssh_sftp:start_channel/2`).
""".
-type connection_ref()       :: pid().  % should be -opaque, but that gives problems

%%--------------------------------------------------------------------
%% Description: Starts the ssh application. Default type
%% is temporary. see application(3)
%%--------------------------------------------------------------------
-doc(#{equiv => start/1}).
-spec start() -> ok | {error, term()}.

start() ->
    start(temporary).

-doc """
Utility function that starts the applications `crypto`, `public_key`, and `ssh`.
Default type is `temporary`. For more information, see the `m:application`
manual page in Kernel.
""".
-spec start(Type) -> ok | {error, term()} when
      Type :: permanent | transient | temporary .

start(Type) ->
    case application:ensure_all_started(ssh, Type) of
        {ok, _} ->
            %% Clear cached default_algorithms (if exists) ...
            ssh_transport:clear_default_algorithms_env(),
            %% ... and rebuild them taking configure options in account
            ssh_transport:default_algorithms(),
            ok;
        Other ->
            Other
    end.

%%--------------------------------------------------------------------
%% Description: Stops the ssh application.
%%--------------------------------------------------------------------
-doc """
Stops the `ssh` application. For more information, see the `m:application`
manual page in Kernel.
""".
-spec stop() -> ok | {error, term()}.

stop() ->
    application:stop(ssh).

%%--------------------------------------------------------------------
%% Description: Starts an ssh connection.
%%--------------------------------------------------------------------
-define(IS_VALID_OPTIONS(Options), is_list(Options)).
-define(IS_VALID_PORT(Port), (is_integer(Port) andalso Port > 0)).
-define(IS_VALID_TIMEOUT(Timeout),
        (Timeout == infinity
         orelse (is_integer(Timeout)
                 andalso Timeout >= 0))).

-doc(#{equiv => connect/4}).
-doc(#{since => <<"OTP 19.0">>}).
-spec connect(OpenTcpSocket, Options)
             -> {ok, connection_ref()}
              | {error, term()} when
      OpenTcpSocket :: open_socket(),
      Options :: client_options().

connect(OpenTcpSocket, Options) when ?IS_VALID_OPTIONS(Options) ->
    connect(OpenTcpSocket, Options, infinity);
connect(_OpenTcpSocket, Options) ->
    bad_arg([{options, Options}]).

-doc(#{equiv => connect/4}).
-spec connect(open_socket(), client_options(), timeout()) ->
                     {ok, connection_ref()} | {error, term()}
           ; (host(), inet:port_number(), client_options()) ->
                     {ok, connection_ref()} | {error, term()}.

connect(Host, Port, Options) when ?IS_VALID_PORT(Port),
                                  ?IS_VALID_OPTIONS(Options) ->
    Timeout = proplists:get_value(connect_timeout, Options, infinity),
    connect(Host, Port, Options, Timeout);
connect(Socket, UserOptions, NegotiationTimeout)
  when ?IS_VALID_OPTIONS(UserOptions),
       ?IS_VALID_TIMEOUT(NegotiationTimeout) ->
    case ssh_options:handle_options(client, UserOptions) of
	{error, Error} ->
	    {error, Error};

	Options = #{} ->
            case valid_socket_to_use(Socket, ?GET_OPT(transport,Options)) of
                ok ->
                    continue_connect(Socket, Options, NegotiationTimeout);
                {error,SockError} ->
                    {error,SockError}
            end
    end;
connect(_HostOrSocket, PortOrOptions, OptionsOrTimeout) ->
    bad_arg(PortOrOptions, OptionsOrTimeout).

-doc """
Connects to an SSH server at the `Host` on `Port`.

As an alternative, an already open TCP socket could be passed to the function in
`TcpSocket`. The SSH initiation and negotiation will be initiated on that one
with the SSH that should be at the other end.

No channel is started. This is done by calling
[ssh_connection:session_channel/2,4](`ssh_connection:session_channel/2`).

The `NegotiationTimeout` is in milli-seconds. The default value is `infinity` or
the value of the [`connect_timeout`](`t:connect_timeout_client_option/0`)
option, if present. For connection timeout, use the option
[`connect_timeout`](`t:connect_timeout_client_option/0`).
""".
-spec connect(Host, Port, Options, NegotiationTimeout)
             -> {ok, connection_ref()}
              | {error, term()} when
      Host :: host(),
      Port :: inet:port_number(),
      Options :: client_options(),
      NegotiationTimeout :: timeout().

connect(Host0, Port, UserOptions, NegotiationTimeout)
  when ?IS_VALID_PORT(Port),
       ?IS_VALID_OPTIONS(UserOptions),
       ?IS_VALID_TIMEOUT(NegotiationTimeout) ->
    case ssh_options:handle_options(client, UserOptions) of
	{error, Reason} ->
            {error, Reason};

        Options ->
            SocketOpts = ?GET_OPT(socket_options,Options) ++ [{active,false}],
            Host = mangle_connect_address(Host0, Options),
            try
                transport_connect(Host, Port, SocketOpts, Options)
            of
                {ok, Socket} ->
                    continue_connect(Socket, Options, NegotiationTimeout);
                {error, Reason} ->
                    {error, Reason}
            catch
                _:badarg -> {error, {options,?GET_OPT(socket_options,Options)}};
                _:{error,Reason} -> {error,Reason};
                error:Error -> {error,Error};
                Class:Error -> {error, {Class,Error}}
            end
    end;
connect(_Host, Port, UserOptions, NegotiationTimeout) ->
    bad_arg([{port, Port},
             {options, UserOptions},
             {timeout, NegotiationTimeout}]).

bad_arg(Args) ->
    hd(bad_args(Args)).

%% Special handling for finding the incorrect args for connect/3,
%% which has two distinctly different signatures.
bad_arg(Arg2, Arg3) ->
    E0 = bad_args([{port, Arg2}, {options, Arg3}]),
    E1 = bad_args([{options, Arg2}, {timeout, Arg3}]),
    %% Select the case with only one error
    case {E0, E1} of
        {[Error], _}    -> Error;
        {_, [Error]}    -> Error;
        {[Error, _], _} -> Error
    end.

%% Return list of errors
-spec bad_args([{'options' | 'port' | 'timeout', any()}]) ->
          [{'error', term()}].
bad_args(Args) ->
    IsErr = fun(true, _) -> false;
               (false, Error) -> {true, {error, Error}}
            end,
    Check =
        fun({options, Arg}) -> IsErr(?IS_VALID_OPTIONS(Arg), invalid_options);
           ({timeout, Arg}) -> IsErr(?IS_VALID_TIMEOUT(Arg), invalid_timeout);
           ({port, Arg})    -> IsErr(?IS_VALID_PORT(Arg), invalid_port)
        end,

    lists:filtermap(Check, Args).

%%%----------------
continue_connect(Socket, Options0, NegTimeout) ->
    {ok, {SockHost,SockPort}} = inet:sockname(Socket),
    Options = ?PUT_INTERNAL_OPT([{negotiation_timeout,NegTimeout}], Options0),
    Address = #address{address = SockHost,
                       port = SockPort,
                       profile = ?GET_OPT(profile,Options)
                      },
    ssh_system_sup:start_connection(client, Address, Socket, Options).

%%--------------------------------------------------------------------
-doc "Closes an SSH connection.".
-spec close(ConnectionRef) -> ok | {error,term()} when
      ConnectionRef :: connection_ref() .
%%
%% Description: Closes an ssh connection.
%%--------------------------------------------------------------------
close(ConnectionRef) ->
    ssh_connection_handler:stop(ConnectionRef).

%%--------------------------------------------------------------------
%% Description: Retrieves information about a connection.
%%---------------------------------------------------------------------
-type version() :: {protocol_version(), software_version()}.
-type protocol_version() :: {Major::pos_integer(), Minor::non_neg_integer()}.
-type software_version() :: string().
-type conn_info_algs() :: [{kex, kex_alg()}
                           | {hkey, pubkey_alg()}
                           | {encrypt, cipher_alg()}
                           | {decrypt, cipher_alg()}
                           | {send_mac, mac_alg()}
                           | {recv_mac, mac_alg()}
                           | {compress, compression_alg()}
                           | {decompress, compression_alg()}
                           | {send_ext_info, boolean()}
                           | {recv_ext_info, boolean()}
                          ].
-type conn_info_channels() :: [proplists:proplist()].

-doc """
Return values from the `connection_info/1` and `connection_info/2` functions.

> #### Note {: .info }
>
> Note that `options` info tuple contains only items with nondefault values.
""".
-type connection_info_tuple() ::
        {client_version, version()}
      | {server_version, version()}
      | {user, string()}
      | {peer, {inet:hostname(), ip_port()}}
      | {sockname, ip_port()}
      | {options, client_options()}
      | {algorithms, conn_info_algs()}
      | {channels, conn_info_channels()}.

-doc(#{equiv => connection_info/2}).
-doc(#{since => <<"OTP 22.1">>}).
-spec connection_info(ConnectionRef) ->
          InfoTupleList | {error, term()} when
      ConnectionRef :: connection_ref(),
      InfoTupleList :: [InfoTuple],
      InfoTuple :: connection_info_tuple().

connection_info(ConnectionRef) ->
    connection_info(ConnectionRef, []).

-doc """
Returns information about a connection intended for e.g debugging or logging.

When the `Key` is a single `Item`, the result is a single `InfoTuple`
""".
-spec connection_info(ConnectionRef, ItemList|Item) ->
          InfoTupleList | InfoTuple | {error, term()} when
      ConnectionRef :: connection_ref(),
      ItemList :: [Item],
      Item :: client_version | server_version | user | peer | sockname | options | algorithms | sockname,
      InfoTupleList :: [InfoTuple],
      InfoTuple :: connection_info_tuple().

connection_info(ConnectionRef, Key) ->
    ssh_connection_handler:connection_info(ConnectionRef, Key).

%%--------------------------------------------------------------------
-doc false.
-spec channel_info(connection_ref(), channel_id(), [atom()]) -> proplists:proplist().
%%
%% Description: Retrieves information about a connection.
%%--------------------------------------------------------------------
channel_info(ConnectionRef, ChannelId, Options) ->
    ssh_connection_handler:channel_info(ConnectionRef, ChannelId, Options).

%%--------------------------------------------------------------------
%% Description: Starts a server listening for SSH connections
%% on the given port.
%%--------------------------------------------------------------------
-doc(#{equiv => daemon/3}).
-spec daemon(inet:port_number()) ->  {ok,daemon_ref()} | {error,term()}.

daemon(Port) ->
    daemon(Port, []).


-doc(#{equiv => daemon/3}).
-spec daemon(inet:port_number()|open_socket(), daemon_options()) -> {ok,daemon_ref()} | {error,term()}.

daemon(Port, UserOptions) when 0 =< Port,Port =< 65535 ->
    daemon(any, Port, UserOptions);

daemon(Socket, UserOptions) ->
    case ssh_options:handle_options(server, UserOptions) of
        #{} = Options0 ->
            case valid_socket_to_use(Socket, ?GET_OPT(transport,Options0)) of
                ok ->
                    try
                        %% throws error:Error if no usable hostkey is found
                        ssh_connection_handler:available_hkey_algorithms(server, Options0),
                        {ok, {SockHost,SockPort}} = inet:sockname(Socket),
                        Address = #address{address = SockHost,
                                           port = SockPort,
                                           profile = ?GET_OPT(profile,Options0)
                                          },
                        Options = ?PUT_INTERNAL_OPT({connected_socket, Socket}, Options0),
                        case ssh_system_sup:start_connection(server, Address, Socket, Options) of
                            {ok,Pid} ->
                                {ok,Pid};
                            {error, {already_started, _}} ->
                                {error, eaddrinuse};
                            {error, Error} ->
                                {error, Error}
                        end
                    catch
                        error:{shutdown,Err} ->
                            {error,Err};
                        exit:{noproc, _} ->
                            {error, ssh_not_started};
                        C:R ->
                            {error,{could_not_start_connection,{C,R}}}
                    end;

                {error,SockError} ->
                    {error,SockError}
            end;
        {error,OptionError} ->
            {error,OptionError}
    end.

-doc """
daemon(HostAddress, Port, Options)

Starts a server listening for SSH connections on the given port. If the `Port`
is 0, a random free port is selected. See `daemon_info/1` about how to find the
selected port number.

As an alternative, an already open TCP socket could be passed to the function in
`TcpSocket`. The SSH initiation and negotiation will be initiated on that one
when an SSH starts at the other end of the TCP socket.

For a description of the options, see [Daemon Options](`t:daemon_options/0`).

Please note that by historical reasons both the `HostAddress` argument and the
[gen_tcp connect_option() `{ip,Address}`](`t:gen_tcp:connect_option/0`) set the
listening address. This is a source of possible inconsistent settings.

The rules for handling the two address passing options are:

- if `HostAddress` is an IP-address, that IP-address is the listening address.
  An 'ip'-option will be discarded if present.
- if `HostAddress` is the atom `loopback`, the listening address is `loopback`
  and an loopback address will be chosen by the underlying layers. An
  'ip'-option will be discarded if present.
- if `HostAddress` is the atom `any` and no 'ip'-option is present, the
  listening address is `any` and the socket will listen to all addresses
- if `HostAddress` is `any` and an 'ip'-option is present, the listening address
  is set to the value of the 'ip'-option
""".
-spec daemon(any | inet:ip_address(), inet:port_number(), daemon_options()) -> {ok,daemon_ref()} | {error,term()}
           ;(socket, open_socket(), daemon_options()) -> {ok,daemon_ref()} | {error,term()}.

daemon(Host0, Port0, UserOptions0)
  when 0 =< Port0, Port0 =< 65535, Host0 == any ;
       Host0 == loopback ; is_tuple(Host0) ->
    {Host1, UserOptions} = handle_daemon_args(Host0, UserOptions0),
    case ssh_options:handle_options(server, UserOptions) of
        #{} = Options0 ->
            case ssh_lsocket:get_lsocket(Host1, Port0, Options0) of
                {ok, {LSocketProvider, LSocket}} ->
                    {Host, Port, Options1} =
                        update_lsocket(LSocket, LSocketProvider, Options0),
                    try
                        %% Host,Port is what to use for the system
                        %% supervisor to register its name (see
                        %% #address record); LSocket is owned by
                        %% LSocketProvider process.  Ownership will be
                        %% transferred once ssh_acceptor_sup is
                        %% started.

                        %% throws error:Error if no usable hostkey is found
                        ssh_connection_handler:available_hkey_algorithms(server, Options1),
                        ssh_system_sup:start_system(#address{address = Host,
                                                             port = Port,
                                                             profile = ?GET_OPT(profile,Options1)},
                                                    Options1)
                    of
                        {ok, DaemonRef} ->
                            {ok, DaemonRef};
                        {error, {already_started, _}} -> % ssh_system_sup with #address already register
                            close_listen_socket(LSocket, Options1),
                            {error, eaddrinuse};
                        {error, Error} ->
                            close_listen_socket(LSocket, Options1),
                            {error, Error}
                    catch
                        error:{shutdown, Err} -> % no suitable host key
                            close_listen_socket(LSocket, Options1),
                            {error, Err};
                        exit:{noproc, _} -> % ssh application not started
                            close_listen_socket(LSocket, Options1),
                            {error, ssh_not_started};
                        error:Error ->
                            close_listen_socket(LSocket, Options1),
                            {error, Error};
                        _C:_E ->
                            {error,{cannot_start_daemon,_C,_E}}
                    end;
                {error, {_, LSocketError}} ->
                    {error, LSocketError}
            end;
        OptionError = {error, _} ->
            OptionError
    end;
daemon(_, _, _) ->
    {error, badarg}.

%%--------------------------------------------------------------------
-doc """
Replaces the options in a running daemon with the options in `NewUserOptions`.
Only connections established after this call are affected, already established
connections are not.

> #### Note {: .info }
>
> In the final phase of this function, the listening process is restarted.
> Therfore a connection attempt to the daemon in this final phase could fail.

The handling of Erlang configurations is described in the User's Guide; see
chapters [Configuration in SSH](configurations.md) and
[Configuring algorithms in SSH](configure_algos.md).
""".
-doc(#{since => <<"OTP 25.1">>}).
-spec daemon_replace_options(DaemonRef, NewUserOptions) -> {ok,daemon_ref()}
                                                         | {error,term()} when
      DaemonRef :: daemon_ref(),
      NewUserOptions :: daemon_options().

daemon_replace_options(DaemonRef, NewUserOptions) ->
    case ssh_system_sup:get_acceptor_options(DaemonRef) of
        {ok, Options0} ->
            Options = ssh_options:merge_options(server, NewUserOptions, Options0),
            ssh_system_sup:restart_acceptor(DaemonRef, Options);
        {error, _Reason} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
-doc """
Return values from the `daemon_info/1` and `daemon_info/2` functions.

> #### Note {: .info }
>
> Note that `options` info tuple contains only items with nondefault values.
""".
-type daemon_info_tuple() ::
        {port, inet:port_number()}
      | {ip, inet:ip_address()}
      | {profile, atom()}
      | {options, daemon_options()}.

-doc(#{equiv => daemon_info/2}).
-doc(#{since => <<"OTP 19.0">>}).
-spec daemon_info(DaemonRef) -> {ok,InfoTupleList} | {error,bad_daemon_ref} when
      DaemonRef :: daemon_ref(),
      InfoTupleList :: [InfoTuple],
      InfoTuple :: daemon_info_tuple().

daemon_info(DaemonRef) ->
    case ssh_system_sup:get_daemon_listen_address(DaemonRef) of
        {ok,A} ->
            Address =
                case inet:parse_strict_address(A#address.address) of
                    {ok,IP} -> A#address{address=IP};
                    _ -> A
                end,
            Opts =
                %% Pick a subset of the Options to present:
                case ssh_system_sup:get_options(DaemonRef, Address) of
                    {ok, OptMap} ->
                        lists:sort(
                          maps:to_list(
                            ssh_options:keep_set_options(
                              server,
                              ssh_options:keep_user_options(server,OptMap))));
                    _ ->
                        []
                end,
            
	    {ok, [{port,    Address#address.port},
                  {ip,      Address#address.address},
                  {profile, Address#address.profile},
                  {options, Opts}
                 ]};
        
	_ ->
	    {error,bad_daemon_ref}
    end.

-doc """
Returns information about a daemon intended for e.g debugging or logging.

When the `Key` is a single `Item`, the result is a single `InfoTuple`

Note that [`daemon_info/1`](`daemon_info/1`) and
[`daemon_info/2`](`daemon_info/2`) returns different types due to compatibility
reasons.
""".
-doc(#{since => <<"OTP 22.1">>}).
-spec daemon_info(DaemonRef, ItemList|Item) ->  InfoTupleList|InfoTuple | {error,bad_daemon_ref} when
      DaemonRef :: daemon_ref(),
      ItemList :: [Item],
      Item :: ip | port | profile | options,
      InfoTupleList :: [InfoTuple],
      InfoTuple :: daemon_info_tuple().

daemon_info(DaemonRef, Key) when is_atom(Key) ->
    case daemon_info(DaemonRef, [Key]) of
        [{Key,Val}] -> {Key,Val};
        Other -> Other
    end;
daemon_info(DaemonRef, Keys) ->
    case daemon_info(DaemonRef) of
        {ok,KVs} ->
            [{Key,proplists:get_value(Key,KVs)} || Key <- Keys,
                                                   lists:keymember(Key,1,KVs)];
        _ ->
            []
    end.

%%--------------------------------------------------------------------
%% Description: Stops the listener, but leaves
%% existing connections started by the listener up and running.
%%--------------------------------------------------------------------
-doc(#{equiv => stop_listener/3}).
-spec stop_listener(daemon_ref()) -> ok.

stop_listener(SysSup) ->
    ssh_system_sup:stop_listener(SysSup).


-doc(#{equiv => stop_listener/3}).
-spec stop_listener(inet:ip_address(), inet:port_number()) -> ok.

stop_listener(Address, Port) ->
    stop_listener(Address, Port, ?DEFAULT_PROFILE).


-doc """
Stops the listener, but leaves existing connections started by the listener
operational.
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec stop_listener(any|inet:ip_address(), inet:port_number(), term()) -> ok.

stop_listener(Address, Port, Profile) ->
    lists:foreach(fun({Sup,_Addr}) ->
                          stop_listener(Sup)
                  end,
                  ssh_system_sup:addresses(#address{address=Address,
                                                    port=Port,
                                                    profile=Profile})).

-doc(#{equiv => stop_daemon/3}).
-spec stop_daemon(DaemonRef::daemon_ref()) -> ok.

stop_daemon(SysSup) ->
    ssh_system_sup:stop_system(SysSup).


-doc(#{equiv => stop_daemon/3}).
-spec stop_daemon(inet:ip_address(), inet:port_number()) -> ok.

stop_daemon(Address, Port) ->
    stop_daemon(Address, Port, ?DEFAULT_PROFILE).


-doc "Stops the listener and all connections started by the listener.".
-doc(#{since => <<"OTP 21.0">>}).
-spec stop_daemon(any|inet:ip_address(), inet:port_number(), atom()) -> ok.

stop_daemon(Address, Port, Profile) ->
    lists:foreach(fun({Sup,_Addr}) ->
                          stop_daemon(Sup)
                  end,
                  ssh_system_sup:addresses(#address{address=Address,
                                                    port=Port,
                                                    profile=Profile})).

%%--------------------------------------------------------------------
%% Description: Starts an interactive shell to an SSH server on the
%% given <Host>. The function waits for user input,
%% and will not return until the remote shell is ended.(e.g. on
%% exit from the shell)
%%--------------------------------------------------------------------
-doc(#{equiv => shell/3}).
-spec shell(open_socket() | host() | connection_ref()) ->  _.

shell(ConnectionRef) when is_pid(ConnectionRef) ->
    case ssh_connection:session_channel(ConnectionRef, infinity) of
	{ok,ChannelId}  ->
	    success = ssh_connection:ptty_alloc(ConnectionRef, ChannelId,
                                                [{pty_opts, [{echo,0}]}
                                                ]),
            success = ssh_connection:send_environment_vars(ConnectionRef, ChannelId,
                                                           ["LANG", "LC_ALL"]),
	    Args = [{channel_cb, ssh_shell},
		    {init_args,[ConnectionRef, ChannelId]},
		    {cm, ConnectionRef}, {channel_id, ChannelId}],
	    {ok, State} = ssh_client_channel:init([Args]),
            try
                ssh_client_channel:enter_loop(State)
            catch
                exit:normal ->
                    ok
            end;
	Error ->
	    Error
    end;

shell(Dest) ->
    case is_host(Dest, []) of
        true ->
            shell(Dest, ?SSH_DEFAULT_PORT, []);
        false ->
            %% Maybe socket
            shell_socket(Dest, [])
    end.



-doc(#{equiv => shell/3}).
-spec shell(open_socket() | host(), client_options()) ->  _.

shell(Dest, Options) ->
    case is_host(Dest, Options) of
        true ->
            shell(Dest, ?SSH_DEFAULT_PORT, Options);
        false ->
            %% Maybe socket
            shell_socket(Dest, Options)
    end.

shell_socket(Socket, Options) ->
    case connect(Socket, Options) of
        {ok,ConnectionRef} ->
            shell(ConnectionRef),
            close(ConnectionRef);
        Error ->
            Error
    end.
    


-doc """
Connects to an SSH server at `Host` and `Port` (defaults to 22) and starts an
interactive shell on that remote host.

As an alternative, an already open TCP socket could be passed to the function in
`TcpSocket`. The SSH initiation and negotiation will be initiated on that one
and finally a shell will be started on the host at the other end of the TCP
socket.

For a description of the options, see [Client Options](`t:client_options/0`).

The function waits for user input, and does not return until the remote shell is
ended (that is, exit from the shell).
""".
-spec shell(Host, Port, Options) -> _ when
      Host :: host(),
      Port :: inet:port_number(),
      Options :: client_options() .

shell(Host, Port, Options) ->
    case connect(Host, Port, Options) of
        {ok,ConnectionRef} ->
            shell(ConnectionRef),
            close(ConnectionRef);
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
-doc """
Returns a key-value list, where the keys are the different types of algorithms
and the values are the algorithms themselves.

See the [User's Guide](configure_algos.md#example_default_algorithms) for an
example.
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec default_algorithms() -> algs_list() .
%%--------------------------------------------------------------------
default_algorithms() ->
    ssh_transport:default_algorithms().

%%--------------------------------------------------------------------
-doc false.
-spec chk_algos_opts(client_options()|daemon_options()) -> internal_options() | {error,term()}.
%%--------------------------------------------------------------------
chk_algos_opts(Opts) ->
    case lists:foldl(
           fun({preferred_algorithms,_}, Acc) -> Acc;
              ({modify_algorithms,_}, Acc) -> Acc;
              (KV, Acc) -> [KV|Acc]
           end, [], Opts)
    of
        [] ->
            case ssh_options:handle_options(client, Opts) of
                M when is_map(M) ->
                    maps:get(preferred_algorithms, M);
                Others ->
                    Others
            end;
        OtherOps ->
            {error, {non_algo_opts_found,OtherOps}}
    end.


%%--------------------------------------------------------------------
-doc """
Sets tcp socket options on the tcp-socket below an ssh connection.

This function calls the `inet:setopts/2`, read that documentation and for
`t:gen_tcp:option/0`.

All gen_tcp socket options except

- `active`
- `deliver`
- `mode` and
- `packet`

are allowed. The excluded options are reserved by the SSH application.

> #### Warning {: .warning }
>
> This is an extremely dangerous function. You use it on your own risk.
>
> Some options are OS and OS version dependent. Do not use it unless you know
> what effect your option values will have on an TCP stream.
>
> Some values may destroy the functionality of the SSH protocol.
""".
-doc(#{since => <<"OTP 22.3">>}).
-spec set_sock_opts(ConnectionRef, SocketOptions) ->
                           ok | {error, inet:posix()}  when
      ConnectionRef :: connection_ref(),
      SocketOptions :: [gen_tcp:option()] .
%%--------------------------------------------------------------------
set_sock_opts(ConnectionRef, SocketOptions) ->
    ssh_connection_handler:set_sock_opts(ConnectionRef, SocketOptions).

%%--------------------------------------------------------------------
-doc """
Get tcp socket option values of the tcp-socket below an ssh connection.

This function calls the `inet:getopts/2`, read that documentation.
""".
-doc(#{since => <<"OTP 22.3">>}).
-spec get_sock_opts(ConnectionRef, SocketGetOptions) ->
                           ok | {error, inet:posix()}  when
      ConnectionRef :: connection_ref(),
      SocketGetOptions :: [gen_tcp:option_name()] .
%%--------------------------------------------------------------------
get_sock_opts(ConnectionRef, SocketGetOptions) ->
    ssh_connection_handler:get_sock_opts(ConnectionRef, SocketGetOptions).

%%--------------------------------------------------------------------
%% Ask local client to listen to ListenHost:ListenPort.  When someone
%% connects that address, connect to ConnectToHost:ConnectToPort from
%% the server.
%%--------------------------------------------------------------------
-doc(#{equiv => tcpip_tunnel_to_server/6}).
-doc(#{since => <<"OTP 23.0">>}).
-spec tcpip_tunnel_to_server(ConnectionRef,
                             ListenHost, ListenPort,
                             ConnectToHost, ConnectToPort
                          ) ->
                                  {ok,TrueListenPort} | {error, term()} when
      ConnectionRef :: connection_ref(),
      ListenHost :: host(),
      ListenPort :: inet:port_number(),
      ConnectToHost :: host(),
      ConnectToPort :: inet:port_number(),
      TrueListenPort :: inet:port_number().

tcpip_tunnel_to_server(ConnectionHandler, ListenHost, ListenPort, ConnectToHost, ConnectToPort) ->
    tcpip_tunnel_to_server(ConnectionHandler, ListenHost, ListenPort, ConnectToHost, ConnectToPort, infinity).


-doc """
Tells the local client to listen to `ListenHost:ListenPort`. When someone
connects to that address, the connection is forwarded in an encrypted channel to
the peer server of `ConnectionRef`. That server then connects to
`ConnectToHost:ConnectToPort`.

The returned `TrueListenPort` is the port that is listened to. It is the same as
`ListenPort`, except when `ListenPort = 0`. In that case a free port is selected
by the underlying OS.

Note that in case of an Erlang/OTP SSH server (daemon) as peer, that server must
have been started with the option
[tcpip_tunnel_in](`t:tcpip_tunnel_in_daemon_option/0`) to allow the connection.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec tcpip_tunnel_to_server(ConnectionRef,
                             ListenHost, ListenPort,
                             ConnectToHost, ConnectToPort,
                             Timeout) ->
                                  {ok,TrueListenPort} | {error, term()} when
      ConnectionRef :: connection_ref(),
      ListenHost :: host(),
      ListenPort :: inet:port_number(),
      ConnectToHost :: host(),
      ConnectToPort :: inet:port_number(),
      Timeout :: timeout(),
      TrueListenPort :: inet:port_number().

tcpip_tunnel_to_server(ConnectionHandler, ListenHost, ListenPort, ConnectToHost0, ConnectToPort, Timeout) ->
    SockOpts = [],
    try
        list_to_binary(
          case mangle_connect_address(ConnectToHost0,SockOpts) of
              IP when is_tuple(IP) -> inet_parse:ntoa(IP);
              _ when is_list(ConnectToHost0) -> ConnectToHost0
          end)
    of
        ConnectToHost ->
            ssh_connection_handler:handle_direct_tcpip(ConnectionHandler,
                                                       mangle_tunnel_address(ListenHost), ListenPort,
                                                       ConnectToHost, ConnectToPort,
                                                       Timeout)
    catch
        _:_ ->
            {error, bad_connect_to_address}
    end.

%%--------------------------------------------------------------------
%% Ask remote server to listen to ListenHost:ListenPort.  When someone
%% connects that address, connect to ConnectToHost:ConnectToPort from
%% the client.
%%--------------------------------------------------------------------
-doc(#{equiv => tcpip_tunnel_from_server/6}).
-doc(#{since => <<"OTP 23.0">>}).
-spec tcpip_tunnel_from_server(ConnectionRef,
                               ListenHost, ListenPort,
                               ConnectToHost, ConnectToPort
                              ) ->
                                    {ok,TrueListenPort} | {error, term()} when
      ConnectionRef :: connection_ref(),
      ListenHost :: host(),
      ListenPort :: inet:port_number(),
      ConnectToHost :: host(),
      ConnectToPort :: inet:port_number(),
      TrueListenPort :: inet:port_number().

tcpip_tunnel_from_server(ConnectionRef, ListenHost, ListenPort, ConnectToHost, ConnectToPort) ->
    tcpip_tunnel_from_server(ConnectionRef, ListenHost, ListenPort, ConnectToHost, ConnectToPort, infinity).

-doc """
Asks the remote server of `ConnectionRef` to listen to `ListenHost:ListenPort`.
When someone connects that address, the connection is forwarded in an encrypted
channel from the server to the client. The client (that is, at the node that
calls this function) then connects to `ConnectToHost:ConnectToPort`.

The returned `TrueListenPort` is the port that is listened to. It is the same as
`ListenPort`, except when `ListenPort = 0`. In that case a free port is selected
by the underlying OS.

Note that in case of an Erlang/OTP SSH server (daemon) as peer, that server must
have been started with the option
[tcpip_tunnel_out](`t:tcpip_tunnel_out_daemon_option/0`) to allow the
connection.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec tcpip_tunnel_from_server(ConnectionRef,
                               ListenHost, ListenPort,
                               ConnectToHost, ConnectToPort,
                               Timeout) ->
                                    {ok,TrueListenPort} | {error, term()} when
      ConnectionRef :: connection_ref(),
      ListenHost :: host(),
      ListenPort :: inet:port_number(),
      ConnectToHost :: host(),
      ConnectToPort :: inet:port_number(),
      Timeout :: timeout(),
      TrueListenPort :: inet:port_number().

tcpip_tunnel_from_server(ConnectionRef, ListenHost0, ListenPort, ConnectToHost0, ConnectToPort, Timeout) ->
    SockOpts = [],
    ListenHost = mangle_tunnel_address(ListenHost0),
    ConnectToHost = mangle_connect_address(ConnectToHost0, SockOpts),
    case ssh_connection_handler:global_request(ConnectionRef, "tcpip-forward", true, 
                                               {ListenHost,ListenPort,ConnectToHost,ConnectToPort},
                                               Timeout) of
        {success,<<>>} ->
            {ok, ListenPort};
        {success,<<TruePort:32/unsigned-integer>>} when ListenPort==0 ->
            {ok, TruePort};
        {success,_} = Res ->
            {error, {bad_result,Res}};
        {failure,<<>>} ->
            {error,not_accepted};
        {failure,Error} ->
            {error,Error};
        Other ->
            Other
    end.

%%--------------------------------------------------------------------
%% In move from public_key
%%--------------------------------------------------------------------
-doc(#{equiv => hostkey_fingerprint/2}).
-doc(#{since => <<"OTP 24.0">>}).
-spec hostkey_fingerprint(public_key:public_key()) -> string().

hostkey_fingerprint(Key) ->
    sshfp_string(md5, ssh_message:ssh2_pubkey_encode(Key) ).

-doc """
hostkey_fingerprint([DigestType], HostKey) ->
[string()]hostkey_fingerprint(DigestType, HostKey) -> string()

Calculates a ssh fingerprint from a public host key as openssh does.

The algorithm in [`hostkey_fingerprint/1`](`hostkey_fingerprint/1`) is md5 to be
compatible with older ssh-keygen commands. The string from the second variant is
prepended by the algorithm name in uppercase as in newer ssh-keygen commands.

Examples:

```erlang
 2> ssh:hostkey_fingerprint(Key).
 "f5:64:a6:c1:5a:cb:9f:0a:10:46:a2:5c:3e:2f:57:84"

 3> ssh:hostkey_fingerprint(md5,Key).
 "MD5:f5:64:a6:c1:5a:cb:9f:0a:10:46:a2:5c:3e:2f:57:84"

 4> ssh:hostkey_fingerprint(sha,Key).
 "SHA1:bSLY/C4QXLDL/Iwmhyg0PGW9UbY"

 5> ssh:hostkey_fingerprint(sha256,Key).
 "SHA256:aZGXhabfbf4oxglxltItWeHU7ub3Dc31NcNw2cMJePQ"

 6> ssh:hostkey_fingerprint([sha,sha256],Key).
 ["SHA1:bSLY/C4QXLDL/Iwmhyg0PGW9UbY",
  "SHA256:aZGXhabfbf4oxglxltItWeHU7ub3Dc31NcNw2cMJePQ"]
```
""".
-doc(#{since => <<"OTP 24.0">>}).
-spec hostkey_fingerprint(TypeOrTypes, Key) -> StringOrString
                                                   when
      TypeOrTypes :: public_key:digest_type() | [public_key:digest_type()],
      Key :: public_key:public_key(),
      StringOrString :: string() | [string()] .

hostkey_fingerprint(HashAlgs, Key) when is_list(HashAlgs) ->
    EncKey = ssh_message:ssh2_pubkey_encode(Key),
    [sshfp_full_string(HashAlg,EncKey) || HashAlg <- HashAlgs];
hostkey_fingerprint(HashAlg, Key) when is_atom(HashAlg) ->
    EncKey = ssh_message:ssh2_pubkey_encode(Key),
    sshfp_full_string(HashAlg, EncKey).


sshfp_string(HashAlg, EncodedKey) ->
    %% Other HashAlgs than md5 will be printed with
    %% other formats than hextstr by
    %%    ssh-keygen -E <alg> -lf <file>
    fp_fmt(sshfp_fmt(HashAlg), crypto:hash(HashAlg, EncodedKey)).

sshfp_full_string(HashAlg, EncKey) ->
    lists:concat([sshfp_alg_name(HashAlg),
		  [$: | sshfp_string(HashAlg, EncKey)]
		 ]).

sshfp_alg_name(sha) -> "SHA1";
sshfp_alg_name(Alg) -> string:to_upper(atom_to_list(Alg)).

sshfp_fmt(md5) -> hexstr;
sshfp_fmt(_) -> b64.

fp_fmt(hexstr, Bin) ->
    lists:flatten(string:join([io_lib:format("~2.16.0b",[C1]) || <<C1>> <= Bin], ":"));
fp_fmt(b64, Bin) ->
    %% This function clause *seems* to be
    %%    [C || C<-base64:encode_to_string(Bin), C =/= $=]
    %% but I am not sure. Must be checked.
    B64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
    BitsInLast = 8*byte_size(Bin) rem 6,
    Padding = (6-BitsInLast) rem 6, % Want BitsInLast = [1:5] to map to padding [5:1] and 0 -> 0
    [lists:nth(C+1,B64Chars) || <<C:6>> <= <<Bin/binary,0:Padding>> ].

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%% The handle_daemon_args/2 function basically only sets the ip-option in Opts
%% so that it is correctly set when opening the listening socket.
handle_daemon_args(any, Opts) ->
    case proplists:get_value(ip, Opts) of
        undefined -> {any, Opts};
        IP -> {IP, Opts}
    end;
handle_daemon_args(IPaddr, Opts) when is_tuple(IPaddr) ; IPaddr == loopback ->
    case proplists:get_value(ip, Opts) of
        undefined -> {IPaddr, [{ip,IPaddr}|Opts]};
        IPaddr -> {IPaddr, Opts};
        IP -> {IPaddr, [{ip,IPaddr}|Opts--[{ip,IP}]]} %% Backward compatibility
    end.

valid_socket_to_use(Socket, {tcp,_,_}) ->
    %% Is this tcp-socket a valid socket?
    try {is_tcp_socket(Socket),
         {ok,[{active,false}]} == inet:getopts(Socket, [active])
        }
    of
        {true,  true} -> ok;
        {true, false} -> {error, not_passive_mode};
        _ ->             {error, not_tcp_socket}
    catch
        _:_ ->           {error, bad_socket}
    end;

valid_socket_to_use(_, {L4,_,_}) ->
    {error, {unsupported,L4}}.


is_tcp_socket(Socket) ->
    case inet:getopts(Socket, [delay_send]) of
        {ok,[_]} -> true;
        _ -> false
    end.

close_listen_socket(ListenSocket, Options) ->
    try
        {_, Callback, _} = ?GET_OPT(transport, Options),
        Callback:close(ListenSocket)
    catch
        _C:_E -> ok
    end.

transport_connect(Host, Port, SocketOpts, Options) ->
    {_, Callback, _} = ?GET_OPT(transport, Options),
    Callback:connect(Host, Port, SocketOpts, ?GET_OPT(connect_timeout,Options)).

-doc false.
is_host(X, Opts) ->
    try is_host1(mangle_connect_address(X, Opts))
    catch
        _:_ -> false
    end.

is_host1(L) when is_list(L) -> true; %% "string()"
is_host1(T) when tuple_size(T)==4 -> lists:all(fun(I) -> 0=<I andalso I=<255 end,
                                               tuple_to_list(T));
is_host1(T) when tuple_size(T)==16 -> lists:all(fun(I) -> 0=<I andalso I=<65535 end,
                                                tuple_to_list(T));
is_host1(loopback) -> true.

mangle_connect_address(A,  #{socket_options := SockOpts}) ->
    mangle_connect_address(A, SockOpts);
mangle_connect_address(A, SockOpts) ->
    mangle_connect_address1(A, proplists:get_value(inet6,SockOpts,false)).

loopback(true) -> {0,0,0,0,0,0,0,1};
loopback(false) ->      {127,0,0,1}.

mangle_connect_address1( loopback,     V6flg) -> loopback(V6flg);
mangle_connect_address1(      any,     V6flg) -> loopback(V6flg);
mangle_connect_address1({0,0,0,0},         _) -> loopback(false);
mangle_connect_address1({0,0,0,0,0,0,0,0}, _) -> loopback(true);
mangle_connect_address1(       IP,     _) when is_tuple(IP) -> IP;
mangle_connect_address1(A, _) ->
    case catch inet:parse_address(A) of
        {ok,         {0,0,0,0}} -> loopback(false);
        {ok, {0,0,0,0,0,0,0,0}} -> loopback(true);
        _ -> A
    end.

mangle_tunnel_address(any) -> <<"">>;
mangle_tunnel_address(loopback) -> <<"localhost">>;
mangle_tunnel_address({0,0,0,0}) -> <<"">>;
mangle_tunnel_address({0,0,0,0,0,0,0,0}) -> <<"">>;
mangle_tunnel_address(IP) when is_tuple(IP) -> list_to_binary(inet_parse:ntoa(IP));
mangle_tunnel_address(A) when is_atom(A) -> mangle_tunnel_address(atom_to_list(A));
mangle_tunnel_address(X) when is_list(X) -> case catch inet:parse_address(X) of
                                     {ok, {0,0,0,0}} -> <<"">>;
                                     {ok, {0,0,0,0,0,0,0,0}} -> <<"">>;
                                     _ -> list_to_binary(X)
                                 end.
-doc false.
update_lsocket(LSocket, LSocketProvider, Options0) ->
    {ok, {LHost, LPort}} = inet:sockname(LSocket),
    Options = ?PUT_INTERNAL_OPT({lsocket,
                                 {LSocket, LHost, LPort, LSocketProvider}}, Options0),
    {LHost, LPort, Options}.

%%%################################################################
%%%#
%%%# Tracing
%%%#

-doc false.
ssh_dbg_trace_points() -> [tcp, connections].

-doc false.
ssh_dbg_flags(tcp) -> [c];
ssh_dbg_flags(connections) -> [c].

-doc false.
ssh_dbg_on(tcp) ->
    dbg:tpl(?MODULE, transport_connect, 4, x),
    dbg:tpl(?MODULE, close_listen_socket, 2, x);
ssh_dbg_on(connections) ->
    dbg:tpl(?MODULE, update_lsocket, 3, x).

-doc false.
ssh_dbg_off(tcp) ->
    dbg:ctpl(?MODULE, transport_connect, 4),
    dbg:ctpl(?MODULE, close_listen_socket, 2);
ssh_dbg_off(connections) ->
    dbg:ctpl(?MODULE, update_lsocket, 3).

-doc false.
ssh_dbg_format(tcp, {call, {?MODULE,close_listen_socket, [ListenSocket, _Opts]}}) ->
    ["TCP socket listening closed\n",
     io_lib:format("Sock: ~p~n", [ListenSocket])
    ];
ssh_dbg_format(tcp, {return_from, {?MODULE,close_listen_socket,2}, _Result}) ->
    skip;
ssh_dbg_format(Tracepoint , Event = {call, {?MODULE, Function, Args}}) ->
    [io_lib:format("~w:~w/~w> ~s", [?MODULE, Function, length(Args)] ++
                       ssh_dbg_comment(Tracepoint, Event))];
ssh_dbg_format(Tracepoint, Event = {return_from, {?MODULE,Function,Arity}, Ret}) ->
    [io_lib:format("~w:~w/~w returned ~W> ~s", [?MODULE, Function, Arity, Ret, 3] ++
                  ssh_dbg_comment(Tracepoint, Event))].

ssh_dbg_comment(connections, {call, {?MODULE, update_lsocket, [LSocket, LSocketProvider, _]}}) ->
    [io_lib:format("LSocket = ~p, LSocketProvider = ~p", [LSocket, LSocketProvider])];
ssh_dbg_comment(connections, {return_from, {?MODULE, update_lsocket,3}, {LHost, LPort, _}}) ->
    [io_lib:format("LHost = ~p, LPort = ~p", [LHost, LPort])];
ssh_dbg_comment(_, _) ->
    [""].

-doc false.
ssh_dbg_format(tcp, {call, {?MODULE,transport_connect, [Host,Port,SockOpts,_Opts]}}, Stack) ->
    {skip, [{transport_connect,Host,Port,SockOpts}|Stack]};
ssh_dbg_format(tcp, {return_from, {?MODULE,transport_connect,4}, {ok,Sock}},
               [{transport_connect,Host,Port,SockOpts}|Stack]) ->
    {["TCP connected to\n",
      io_lib:format("Host: ~p~n"
                    "Port: ~p~n"
                    "SockOpts: ~p~n"
                    "Socket: ~p~n", [Host,Port,SockOpts,Sock])
     ],
     Stack};
ssh_dbg_format(tcp, {return_from, {?MODULE,transport_connect,4}, Result},
               [{transport_connect,Host,Port,SockOpts}|Stack]) ->
    {["TCP connected FAILED to\n",
      io_lib:format("Host: ~p~n"
                    "Port: ~p~n"
                    "SockOpts: ~p~n"
                    "Result: ~p~n", [Host,Port,SockOpts,Result])
     ],
     Stack}.
