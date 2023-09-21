%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2023. All Rights Reserved.
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
-export([is_host/2]).

-behaviour(ssh_dbg).
-export([ssh_dbg_trace_points/0, ssh_dbg_flags/1, ssh_dbg_on/1, ssh_dbg_off/1, ssh_dbg_format/2, ssh_dbg_format/3]).

%%% "Deprecated" types export:
-export_type([ssh_daemon_ref/0, ssh_connection_ref/0, ssh_channel_id/0]).
-opaque ssh_daemon_ref()     :: daemon_ref().
-opaque ssh_connection_ref() :: connection_ref().
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


-opaque daemon_ref()         :: pid() .
-opaque channel_id()     :: non_neg_integer().
-type connection_ref()       :: pid().  % should be -opaque, but that gives problems

%%--------------------------------------------------------------------
%% Description: Starts the ssh application. Default type
%% is temporary. see application(3)
%%--------------------------------------------------------------------
-spec start() -> ok | {error, term()}.

start() ->
    start(temporary).

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

-spec connect(OpenTcpSocket, Options)
             -> {ok, connection_ref()}
              | {error, term()} when
      OpenTcpSocket :: open_socket(),
      Options :: client_options().

connect(OpenTcpSocket, Options) when ?IS_VALID_OPTIONS(Options) ->
    connect(OpenTcpSocket, Options, infinity);
connect(_OpenTcpSocket, Options) ->
    bad_arg([{options, Options}]).

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
            SocketOpts = [{active,false} | ?GET_OPT(socket_options,Options)],
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
    ssh_system_sup:start_subsystem(client, Address, Socket, Options).

%%--------------------------------------------------------------------
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

-type connection_info_tuple() ::
        {client_version, version()}
      | {server_version, version()}
      | {user, string()}
      | {peer, {inet:hostname(), ip_port()}}
      | {sockname, ip_port()}
      | {options, client_options()}
      | {algorithms, conn_info_algs()}
      | {channels, conn_info_channels()}.
        
-spec connection_info(ConnectionRef) -> InfoTupleList when
      ConnectionRef :: connection_ref(),
      InfoTupleList :: [InfoTuple],
      InfoTuple :: connection_info_tuple().

connection_info(ConnectionRef) ->                                      
    connection_info(ConnectionRef, []).

-spec connection_info(ConnectionRef, ItemList|Item) ->  InfoTupleList|InfoTuple when
      ConnectionRef :: connection_ref(),
      ItemList :: [Item],
      Item :: client_version | server_version | user | peer | sockname | options | algorithms | sockname,
      InfoTupleList :: [InfoTuple],
      InfoTuple :: connection_info_tuple().

connection_info(ConnectionRef, Key) ->
    ssh_connection_handler:connection_info(ConnectionRef, Key).

%%--------------------------------------------------------------------
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
-spec daemon(inet:port_number()) ->  {ok,daemon_ref()} | {error,term()}.

daemon(Port) ->
    daemon(Port, []).


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
                        case ssh_system_sup:start_subsystem(server, Address, Socket, Options) of
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



-spec daemon(any | inet:ip_address(), inet:port_number(), daemon_options()) -> {ok,daemon_ref()} | {error,term()}
           ;(socket, open_socket(), daemon_options()) -> {ok,daemon_ref()} | {error,term()}
            .

daemon(Host0, Port0, UserOptions0) when 0 =< Port0, Port0 =< 65535,
                                        Host0 == any ; Host0 == loopback ; is_tuple(Host0) ->
    try
        {Host1, UserOptions} = handle_daemon_args(Host0, UserOptions0),
        #{} = Options0 = ssh_options:handle_options(server, UserOptions),
        %% We need to open the listen socket here before start of the system supervisor. That
        %% is because Port0 might be 0, or if an FD is provided in the Options0, in which case
        %% the real listening port will be known only after the gen_tcp:listen call.
        maybe_open_listen_socket(Host1, Port0, Options0)
    of
        {Host, Port, ListenSocket, Options1} ->
            try
                %% Now Host,Port is what to use for the supervisor to register its name,
                %% and ListenSocket, if provided,  is for listening on connections. But
                %% it is still owned by self()...

                %% throws error:Error if no usable hostkey is found
                ssh_connection_handler:available_hkey_algorithms(server, Options1),
                ssh_system_sup:start_system(server,
                                            #address{address = Host,
                                                     port = Port,
                                                     profile = ?GET_OPT(profile,Options1)},
                                            Options1)
            of
                {ok,DaemonRef} when ListenSocket == undefined ->
                    {ok,DaemonRef};
                {ok,DaemonRef} ->
                    receive
                        {request_control, ListenSocket, ReqPid} ->
                            ok = controlling_process(ListenSocket, ReqPid, Options1),
                            ReqPid ! {its_yours,ListenSocket}
                    end,
                    {ok,DaemonRef};
                {error, {already_started, _}} ->
                    close_listen_socket(ListenSocket, Options1),
                    {error, eaddrinuse};
                {error, Error} ->
                    close_listen_socket(ListenSocket, Options1),
                    {error, Error}
            catch
                error:{shutdown,Err} ->
                    close_listen_socket(ListenSocket, Options1),
                    {error,Err};
                exit:{noproc, _} ->
                    close_listen_socket(ListenSocket, Options1),
                    {error, ssh_not_started};
                error:Error ->
                    close_listen_socket(ListenSocket, Options1),
                    error(Error);
                exit:Exit ->
                    close_listen_socket(ListenSocket, Options1),
                    exit(Exit)
            end
    catch
        throw:bad_fd ->
            {error,bad_fd};
        throw:bad_socket ->
            {error,bad_socket};
        error:{badmatch,{error,Error}} ->
            {error,Error};
        error:Error ->
            {error,Error};
        _C:_E ->
            {error,{cannot_start_daemon,_C,_E}}
    end;

daemon(_, _, _) ->
    {error, badarg}.

%%--------------------------------------------------------------------
-spec daemon_replace_options(DaemonRef, NewUserOptions) -> {ok,daemon_ref()}
                                                         | {error,term()} when
      DaemonRef :: daemon_ref(),
      NewUserOptions :: daemon_options().

daemon_replace_options(DaemonRef, NewUserOptions) ->
    {ok,Os0} = ssh_system_sup:get_acceptor_options(DaemonRef),
    Os1 = ssh_options:merge_options(server, NewUserOptions, Os0),
    ssh_system_sup:replace_acceptor_options(DaemonRef, Os1).

%%--------------------------------------------------------------------
-type daemon_info_tuple() ::
        {port, inet:port_number()}
      | {ip, inet:ip_address()}
      | {profile, atom()}
      | {options, daemon_options()}.

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
-spec stop_listener(daemon_ref()) -> ok.

stop_listener(SysSup) ->
    ssh_system_sup:stop_listener(SysSup).


-spec stop_listener(inet:ip_address(), inet:port_number()) -> ok.

stop_listener(Address, Port) ->
    stop_listener(Address, Port, ?DEFAULT_PROFILE).


-spec stop_listener(any|inet:ip_address(), inet:port_number(), term()) -> ok.

stop_listener(Address, Port, Profile) ->
    lists:foreach(fun({Sup,_Addr}) ->
                          stop_listener(Sup)
                  end,
                  ssh_system_sup:addresses(server,
                                           #address{address=Address,
                                                    port=Port,
                                                    profile=Profile})).

%%--------------------------------------------------------------------
%% Description: Stops the listener and all connections started by
%% the listener.
%%--------------------------------------------------------------------
-spec stop_daemon(DaemonRef::daemon_ref()) -> ok.

stop_daemon(SysSup) ->
    ssh_system_sup:stop_system(server, SysSup).


-spec stop_daemon(inet:ip_address(), inet:port_number()) -> ok.

stop_daemon(Address, Port) ->
    stop_daemon(Address, Port, ?DEFAULT_PROFILE).


-spec stop_daemon(any|inet:ip_address(), inet:port_number(), atom()) -> ok.

stop_daemon(Address, Port, Profile) ->
    lists:foreach(fun({Sup,_Addr}) ->
                          stop_daemon(Sup)
                  end,
                  ssh_system_sup:addresses(server,
                                           #address{address=Address,
                                                    port=Port,
                                                    profile=Profile})).

%%--------------------------------------------------------------------
%% Description: Starts an interactive shell to an SSH server on the
%% given <Host>. The function waits for user input,
%% and will not return until the remote shell is ended.(e.g. on
%% exit from the shell)
%%--------------------------------------------------------------------
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
-spec default_algorithms() -> algs_list() .
%%--------------------------------------------------------------------
default_algorithms() ->
    ssh_transport:default_algorithms().

%%--------------------------------------------------------------------
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
-spec set_sock_opts(ConnectionRef, SocketOptions) ->
                           ok | {error, inet:posix()}  when
      ConnectionRef :: connection_ref(),
      SocketOptions :: [gen_tcp:option()] .
%%--------------------------------------------------------------------
set_sock_opts(ConnectionRef, SocketOptions) ->
    ssh_connection_handler:set_sock_opts(ConnectionRef, SocketOptions).

%%--------------------------------------------------------------------
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
-spec hostkey_fingerprint(public_key:public_key()) -> string().

hostkey_fingerprint(Key) ->
    sshfp_string(md5, ssh_message:ssh2_pubkey_encode(Key) ).

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

%%%----------------------------------------------------------------
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

%%%----------------------------------------------------------------
maybe_open_listen_socket(Host, Port, Options) ->
    Opened =
        case ?GET_SOCKET_OPT(fd, Options) of
            undefined when Port == 0 ->
                ssh_acceptor:listen(0, Options);
            Fd when is_integer(Fd) ->
                %% Do gen_tcp:listen with the option {fd,Fd}:
                ssh_acceptor:listen(0, Options);
            undefined ->
                open_later
        end,
    case Opened of
        {ok,LSock} ->
            {ok,{LHost,LPort}} = inet:sockname(LSock),
            {LHost, LPort, LSock, ?PUT_INTERNAL_OPT({lsocket,{LSock,self()}}, Options)};
        open_later ->
            {Host, Port, undefined, Options};
        Others ->
            Others
    end.

%%%----------------------------------------------------------------
close_listen_socket(ListenSocket, Options) ->
    try
        {_, Callback, _} = ?GET_OPT(transport, Options),
        Callback:close(ListenSocket)
    catch
        _C:_E -> ok
    end.

controlling_process(ListenSocket, ReqPid, Options) ->
    {_, Callback, _} = ?GET_OPT(transport, Options),
    Callback:controlling_process(ListenSocket, ReqPid).

transport_connect(Host, Port, SocketOpts, Options) ->
    {_, Callback, _} = ?GET_OPT(transport, Options),
    Callback:connect(Host, Port, SocketOpts, ?GET_OPT(connect_timeout,Options)).
    
%%%----------------------------------------------------------------
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

%%%----------------------------------------------------------------
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

%%%----------------------------------------------------------------
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


%%%################################################################
%%%#
%%%# Tracing
%%%#

ssh_dbg_trace_points() -> [tcp].

ssh_dbg_flags(tcp) -> [c].

ssh_dbg_on(tcp) -> dbg:tpl(?MODULE, controlling_process, 3, x),
                   dbg:tpl(?MODULE, transport_connect, 4, x),
                   dbg:tpl(?MODULE, close_listen_socket, 2, x).
                   
ssh_dbg_off(tcp) ->dbg:ctpl(?MODULE, controlling_process, 3),
                   dbg:ctpl(?MODULE, transport_connect, 4),
                   dbg:ctpl(?MODULE, close_listen_socket, 2).

ssh_dbg_format(tcp, {call, {?MODULE,controlling_process, [ListenSocket, ReqPid, _Opts]}}) ->
    ["TCP socket transferred to\n",
     io_lib:format("Sock: ~p~n"
                   "ToPid: ~p~n", [ListenSocket, ReqPid])
    ];
ssh_dbg_format(tcp, {return_from, {?MODULE,controlling_process,3}, _Result}) ->
    skip;

ssh_dbg_format(tcp, {call, {?MODULE,close_listen_socket, [ListenSocket, _Opts]}}) ->
    ["TCP socket listening closed\n",
     io_lib:format("Sock: ~p~n", [ListenSocket])
    ];
ssh_dbg_format(tcp, {return_from, {?MODULE,close_listen_socket,2}, _Result}) ->
    skip.


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
