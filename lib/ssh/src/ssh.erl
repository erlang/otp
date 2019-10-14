%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2018. All Rights Reserved.
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
	 default_algorithms/0,
         chk_algos_opts/1,
	 stop_listener/1, stop_listener/2,  stop_listener/3,
	 stop_daemon/1, stop_daemon/2, stop_daemon/3,
	 shell/1, shell/2, shell/3
	]).

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
-spec connect(OpenTcpSocket, Options) -> {ok,connection_ref()} | {error,term()} when
      OpenTcpSocket :: open_socket(),
      Options :: client_options().

connect(OpenTcpSocket, Options) when is_port(OpenTcpSocket),
                                     is_list(Options) ->
    connect(OpenTcpSocket, Options, infinity).


-spec connect(open_socket(), client_options(), timeout()) ->
                     {ok,connection_ref()} | {error,term()}
           ; (host(), inet:port_number(), client_options()) ->
                     {ok,connection_ref()} | {error,term()}.

connect(Socket, UserOptions, NegotiationTimeout) when is_port(Socket),
                                                      is_list(UserOptions) ->
    case ssh_options:handle_options(client, UserOptions) of
	{error, Error} ->
	    {error, Error};
	Options ->
            case valid_socket_to_use(Socket, ?GET_OPT(transport,Options)) of
		ok ->
		    {ok, {Host,_Port}} = inet:peername(Socket),
		    Opts = ?PUT_INTERNAL_OPT([{user_pid,self()}, {host,Host}], Options),
		    ssh_connection_handler:start_connection(client, Socket, Opts, NegotiationTimeout);
		{error,SockError} ->
		    {error,SockError}
	    end
    end;

connect(Host, Port, Options) when is_integer(Port),
                                  Port>0,
                                  is_list(Options) ->
    connect(Host, Port, Options, infinity).


-spec connect(Host, Port, Options, NegotiationTimeout) -> {ok,connection_ref()} | {error,term()} when
      Host :: host(),
      Port :: inet:port_number(),
      Options :: client_options(),
      NegotiationTimeout :: timeout().

connect(Host0, Port, UserOptions, Timeout) when is_integer(Port),
                                               Port>0,
                                               is_list(UserOptions) ->
    case ssh_options:handle_options(client, UserOptions) of
	{error, _Reason} = Error ->
	    Error;
        Options ->
	    {_, Transport, _} = TransportOpts = ?GET_OPT(transport, Options),
	    ConnectionTimeout = ?GET_OPT(connect_timeout, Options),
            SocketOpts = [{active,false} | ?GET_OPT(socket_options,Options)],
            Host = mangle_connect_address(Host0, SocketOpts),
	    try Transport:connect(Host, Port, SocketOpts, ConnectionTimeout) of
		{ok, Socket} ->
		    Opts = ?PUT_INTERNAL_OPT([{user_pid,self()}, {host,Host}], Options),
		    ssh_connection_handler:start_connection(client, Socket, Opts, Timeout);
		{error, Reason} ->
		    {error, Reason}
	    catch
		exit:{function_clause, _F} ->
		    {error, {options, {transport, TransportOpts}}};
		exit:badarg ->
		    {error, {options, {socket_options, SocketOpts}}}
	    end
    end.

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

daemon(Socket, UserOptions) when is_port(Socket) ->
    try
        #{} = Options = ssh_options:handle_options(server, UserOptions),

        case valid_socket_to_use(Socket, ?GET_OPT(transport,Options)) of
            ok ->
                {ok, {IP,Port}} = inet:sockname(Socket),
                finalize_start(IP, Port, ?GET_OPT(profile, Options),
                               ?PUT_INTERNAL_OPT({connected_socket, Socket}, Options),
                               fun(Opts, DefaultResult) ->
                                       try ssh_acceptor:handle_established_connection(
                                             IP, Port, Opts, Socket)
                                       of
                                           {error,Error} ->
                                               {error,Error};
                                           _ ->
                                               DefaultResult
                                       catch
                                           C:R ->
                                               {error,{could_not_start_connection,{C,R}}}
                                       end
                               end);
            {error,SockError} ->
                {error,SockError}
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

daemon(Port, UserOptions) when 0 =< Port, Port =< 65535 ->
    daemon(any, Port, UserOptions).


-spec daemon(any | inet:ip_address(), inet:port_number(), daemon_options()) -> {ok,daemon_ref()} | {error,term()}
           ;(socket, open_socket(), daemon_options()) -> {ok,daemon_ref()} | {error,term()}
            .

daemon(Host0, Port0, UserOptions0) when 0 =< Port0, Port0 =< 65535,
                                        Host0 == any ; Host0 == loopback ; is_tuple(Host0) ->
    try
        {Host1, UserOptions} = handle_daemon_args(Host0, UserOptions0),
        #{} = Options0 = ssh_options:handle_options(server, UserOptions),
        {open_listen_socket(Host1, Port0, Options0), Options0}
    of
        {{{Host,Port}, ListenSocket}, Options1} ->
            try
                %% Now Host,Port is what to use for the supervisor to register its name,
                %% and ListenSocket is for listening on connections. But it is still owned
                %% by self()...
                finalize_start(Host, Port, ?GET_OPT(profile, Options1),
                               ?PUT_INTERNAL_OPT({lsocket,{ListenSocket,self()}}, Options1),
                               fun(Opts, Result) ->
                                       {_, Callback, _} = ?GET_OPT(transport, Opts),
                                       receive
                                           {request_control, ListenSocket, ReqPid} ->
                                               ok = Callback:controlling_process(ListenSocket, ReqPid),
                                               ReqPid ! {its_yours,ListenSocket},
                                               Result
                                       end
                               end)
            of
                {error,Err} ->
                    close_listen_socket(ListenSocket, Options1),
                    {error,Err};
                OK ->
                    OK
            catch
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
    case catch ssh_system_sup:acceptor_supervisor(DaemonRef) of
	AsupPid when is_pid(AsupPid) ->
	    [{Host,Port,Profile}] =
		[{Hst,Prt,Prf} 
                 || {{ssh_acceptor_sup,Hst,Prt,Prf},_Pid,worker,[ssh_acceptor]} 
                        <- supervisor:which_children(AsupPid)],
            IP =
                case inet:parse_strict_address(Host) of
                    {ok,IP0} -> IP0;
                    _ -> Host
                end,

            Opts =
                case ssh_system_sup:get_options(DaemonRef, Host, Port, Profile) of
                    {ok, OptMap} ->
                        lists:sort(
                          maps:to_list(
                            ssh_options:keep_set_options(
                              server,
                              ssh_options:keep_user_options(server,OptMap))));
                    _ ->
                        []
                end,
            
	    {ok, [{port,Port},
                  {ip,IP},
                  {profile,Profile},
                  {options,Opts}
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

stop_listener(any, Port, Profile) ->
    map_ip(fun(IP) ->
                   ssh_system_sup:stop_listener(IP, Port, Profile) 
           end, [{0,0,0,0},{0,0,0,0,0,0,0,0}]);
stop_listener(Address, Port, Profile) ->
    map_ip(fun(IP) ->
                   ssh_system_sup:stop_listener(IP, Port, Profile) 
           end, {address,Address}).

%%--------------------------------------------------------------------
%% Description: Stops the listener and all connections started by
%% the listener.
%%--------------------------------------------------------------------
-spec stop_daemon(DaemonRef::daemon_ref()) -> ok.

stop_daemon(SysSup) ->
    ssh_system_sup:stop_system(SysSup).


-spec stop_daemon(inet:ip_address(), inet:port_number()) -> ok.

stop_daemon(Address, Port) ->
    stop_daemon(Address, Port, ?DEFAULT_PROFILE).


-spec stop_daemon(any|inet:ip_address(), inet:port_number(), atom()) -> ok.

stop_daemon(any, Port, Profile) ->
    map_ip(fun(IP) ->
                   ssh_system_sup:stop_system(IP, Port, Profile) 
           end, [{0,0,0,0},{0,0,0,0,0,0,0,0}]);
stop_daemon(Address, Port, Profile) ->
    map_ip(fun(IP) ->
                   ssh_system_sup:stop_system(IP, Port, Profile) 
           end, {address,Address}).

%%--------------------------------------------------------------------
%% Description: Starts an interactive shell to an SSH server on the
%% given <Host>. The function waits for user input,
%% and will not return until the remote shell is ended.(e.g. on
%% exit from the shell)
%%--------------------------------------------------------------------
-spec shell(open_socket() | host() | connection_ref()) ->  _.

shell(Socket) when is_port(Socket) ->
    shell(Socket, []);

shell(ConnectionRef) when is_pid(ConnectionRef) ->
    case ssh_connection:session_channel(ConnectionRef, infinity) of
	{ok,ChannelId}  ->
	    success = ssh_connection:ptty_alloc(ConnectionRef, ChannelId, []),
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

shell(Host) ->
    shell(Host, ?SSH_DEFAULT_PORT, []).


-spec shell(open_socket() | host(), client_options()) ->  _.

shell(Socket, Options) when is_port(Socket) ->
    case connect(Socket, Options) of
        {ok,ConnectionRef} ->
            shell(ConnectionRef),
            close(ConnectionRef);
        Error ->
            Error
    end;

shell(Host, Options) ->
    shell(Host, ?SSH_DEFAULT_PORT, Options).


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
open_listen_socket(_Host0, Port0, Options0) ->
    {ok,LSock} =
        case ?GET_SOCKET_OPT(fd, Options0) of
            undefined ->
                ssh_acceptor:listen(Port0, Options0);
            Fd when is_integer(Fd) ->
                %% Do gen_tcp:listen with the option {fd,Fd}:
                ssh_acceptor:listen(0, Options0)
        end,
    {ok,{LHost,LPort}} = inet:sockname(LSock),
    {{LHost,LPort}, LSock}.

%%%----------------------------------------------------------------
close_listen_socket(ListenSocket, Options) ->
    try
        {_, Callback, _} = ?GET_OPT(transport, Options),
        Callback:close(ListenSocket)
    catch
        _C:_E -> ok
    end.

%%%----------------------------------------------------------------
finalize_start(Host, Port, Profile, Options0, F) ->
    try
        %% throws error:Error if no usable hostkey is found
        ssh_connection_handler:available_hkey_algorithms(server, Options0),

        sshd_sup:start_child(Host, Port, Profile, Options0)
    of
        {error, {already_started, _}} ->
            {error, eaddrinuse};
        {error, Error} ->
            {error, Error};
        Result = {ok,_} ->
            F(Options0, Result)
    catch
        error:{shutdown,Err} ->
            {error,Err};
        exit:{noproc, _} ->
            {error, ssh_not_started}
    end.

%%%----------------------------------------------------------------
map_ip(Fun, {address,IP}) when is_tuple(IP) ->
    Fun(IP);
map_ip(Fun, {address,Address}) ->
    IPs = try {ok,#hostent{h_addr_list=IP0s}} = inet:gethostbyname(Address),
               IP0s
          catch
              _:_ -> []
          end,
    map_ip(Fun, IPs);
map_ip(Fun, IPs) ->
    lists:map(Fun, IPs).

%%%----------------------------------------------------------------
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
