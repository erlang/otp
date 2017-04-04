%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
	 channel_info/3,
	 daemon/1, daemon/2, daemon/3,
	 daemon_info/1,
	 default_algorithms/0,
	 stop_listener/1, stop_listener/2,  stop_listener/3,
	 stop_daemon/1, stop_daemon/2, stop_daemon/3,
	 shell/1, shell/2, shell/3
	]).

%%% Type exports
-export_type([ssh_daemon_ref/0,
              ssh_connection_ref/0,
	      ssh_channel_id/0,
              role/0,
              subsystem_spec/0,
              subsystem_name/0,
              channel_callback/0,
              channel_init_args/0,
              algs_list/0,
              alg_entry/0,
              simple_algs/0,
              double_algs/0
	     ]).

-opaque ssh_daemon_ref()     :: daemon_ref() .
-opaque ssh_connection_ref() :: connection_ref() .
-opaque ssh_channel_id()     :: channel_id().

%%--------------------------------------------------------------------
-spec start() -> ok | {error, term()}.
-spec start(permanent | transient | temporary) -> ok | {error, term()}.
%%
%% Description: Starts the ssh application. Default type
%% is temporary. see application(3)
%%--------------------------------------------------------------------
start() ->
    start(temporary).

start(Type) ->
    case application:ensure_all_started(ssh, Type) of
        {ok, _} ->
            ok;
        Other ->
            Other
    end.

%%--------------------------------------------------------------------
-spec stop() -> ok | {error, term()}.
%%
%% Description: Stops the ssh application.
%%--------------------------------------------------------------------
stop() ->
    application:stop(ssh).

%%--------------------------------------------------------------------
-spec connect(inet:socket(), proplists:proplist()) -> ok_error(connection_ref()).

-spec connect(inet:socket(), proplists:proplist(), timeout()) -> ok_error(connection_ref())
           ; (string(), inet:port_number(), proplists:proplist()) -> ok_error(connection_ref()).

-spec connect(string(), inet:port_number(), proplists:proplist(), timeout()) -> ok_error(connection_ref()).

%%
%% Description: Starts an ssh connection.
%%--------------------------------------------------------------------
connect(Socket, UserOptions) when is_port(Socket),
                                  is_list(UserOptions) ->
    connect(Socket, UserOptions, infinity).

connect(Socket, UserOptions, Timeout) when is_port(Socket),
                                           is_list(UserOptions) ->
    case ssh_options:handle_options(client, UserOptions) of
	{error, Error} ->
	    {error, Error};
	Options ->
            case valid_socket_to_use(Socket, ?GET_OPT(transport,Options)) of
		ok ->
		    {ok, {Host,_Port}} = inet:sockname(Socket),
		    Opts = ?PUT_INTERNAL_OPT([{user_pid,self()}, {host,Host}], Options),
		    ssh_connection_handler:start_connection(client, Socket, Opts, Timeout);
		{error,SockError} ->
		    {error,SockError}
	    end
    end;

connect(Host, Port, UserOptions) when is_integer(Port),
                                      Port>0,
                                      is_list(UserOptions) ->
    connect(Host, Port, UserOptions, infinity).

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
-spec close(pid()) -> ok.
%%
%% Description: Closes an ssh connection.
%%--------------------------------------------------------------------
close(ConnectionRef) ->
    ssh_connection_handler:stop(ConnectionRef).

%%--------------------------------------------------------------------
-spec connection_info(pid(), [atom()]) -> [{atom(), term()}].
%%
%% Description: Retrieves information about a connection.
%%--------------------------------------------------------------------
connection_info(ConnectionRef, Options) ->
    ssh_connection_handler:connection_info(ConnectionRef, Options).

%%--------------------------------------------------------------------
-spec channel_info(pid(), channel_id(), [atom()]) -> [{atom(), term()}].
%%
%% Description: Retrieves information about a connection.
%%--------------------------------------------------------------------
channel_info(ConnectionRef, ChannelId, Options) ->
    ssh_connection_handler:channel_info(ConnectionRef, ChannelId, Options).

%%--------------------------------------------------------------------
-spec daemon(inet:port_number()) ->  ok_error(daemon_ref()).
-spec daemon(inet:port_number()|inet:socket(), proplists:proplist()) -> ok_error(daemon_ref()).
-spec daemon(any | inet:ip_address(), inet:port_number(), proplists:proplist()) -> ok_error(daemon_ref())
           ;(socket, inet:socket(), proplists:proplist()) -> ok_error(daemon_ref())
            .

%% Description: Starts a server listening for SSH connections
%% on the given port.
%%--------------------------------------------------------------------
daemon(Port) ->
    daemon(Port, []).


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


daemon(Host0, Port0, UserOptions0) when 0 =< Port0, Port0 =< 65535,
                                        Host0 == any ; Host0 == loopback ; is_tuple(Host0) ->
    try
        {Host1, UserOptions} = handle_daemon_args(Host0, UserOptions0),
        #{} = Options0 = ssh_options:handle_options(server, UserOptions),

        {{Host,Port}, ListenSocket} =
            open_listen_socket(Host1, Port0, Options0),

        %% Now Host,Port is what to use for the supervisor to register its name,
        %% and ListenSocket is for listening on connections. But it is still owned
        %% by self()...

        finalize_start(Host, Port, ?GET_OPT(profile, Options0),
                       ?PUT_INTERNAL_OPT({lsocket,{ListenSocket,self()}}, Options0),
                       fun(Opts, Result) ->
                               {_, Callback, _} = ?GET_OPT(transport, Opts),
                               receive
                                   {request_control, ListenSocket, ReqPid} ->
                                       ok = Callback:controlling_process(ListenSocket, ReqPid),
                                       ReqPid ! {its_yours,ListenSocket},
                                       Result
                               end
                       end)
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
-spec daemon_info(daemon_ref()) -> ok_error( [{atom(), term()}] ).

daemon_info(Pid) ->
    case catch ssh_system_sup:acceptor_supervisor(Pid) of
	AsupPid when is_pid(AsupPid) ->
	    [{IP,Port,Profile}] =
		[{IP,Prt,Prf} 
                 || {{ssh_acceptor_sup,Hst,Prt,Prf},_Pid,worker,[ssh_acceptor]} 
                        <- supervisor:which_children(AsupPid),
                    IP <- [case inet:parse_strict_address(Hst) of
                               {ok,IP} -> IP;
                               _ -> Hst
                           end]
                ],
	    {ok, [{port,Port},
                  {ip,IP},
                  {profile,Profile}
                 ]};
	_ ->
	    {error,bad_daemon_ref}
    end.

%%--------------------------------------------------------------------
-spec stop_listener(daemon_ref()) -> ok.
-spec stop_listener(inet:ip_address(), inet:port_number()) -> ok.
%%
%% Description: Stops the listener, but leaves
%% existing connections started by the listener up and running.
%%--------------------------------------------------------------------
stop_listener(SysSup) ->
    ssh_system_sup:stop_listener(SysSup).
stop_listener(Address, Port) ->
    stop_listener(Address, Port, ?DEFAULT_PROFILE).
stop_listener(any, Port, Profile) ->
    map_ip(fun(IP) ->
                   ssh_system_sup:stop_listener(IP, Port, Profile) 
           end, [{0,0,0,0},{0,0,0,0,0,0,0,0}]);
stop_listener(Address, Port, Profile) ->
    map_ip(fun(IP) ->
                   ssh_system_sup:stop_listener(IP, Port, Profile) 
           end, {address,Address}).

%%--------------------------------------------------------------------
-spec stop_daemon(daemon_ref()) -> ok.
-spec stop_daemon(inet:ip_address(), inet:port_number()) -> ok.
-spec stop_daemon(inet:ip_address(), inet:port_number(), atom()) -> ok.
%%
%% Description: Stops the listener and all connections started by
%% the listener.
%%--------------------------------------------------------------------
stop_daemon(SysSup) ->
    ssh_system_sup:stop_system(SysSup).
stop_daemon(Address, Port) ->
    stop_daemon(Address, Port, ?DEFAULT_PROFILE).
stop_daemon(any, Port, Profile) ->
    map_ip(fun(IP) ->
                   ssh_system_sup:stop_system(IP, Port, Profile) 
           end, [{0,0,0,0},{0,0,0,0,0,0,0,0}]);
stop_daemon(Address, Port, Profile) ->
    map_ip(fun(IP) ->
                   ssh_system_sup:stop_system(IP, Port, Profile) 
           end, {address,Address}).

%%--------------------------------------------------------------------
-spec shell(inet:socket() | string()) ->  _.
-spec shell(inet:socket() | string(), proplists:proplist()) ->  _.
-spec shell(string(), inet:port_number(), proplists:proplist()) ->  _.

%%   Host = string()
%%   Port = integer()
%%   Options = [{Option, Value}]
%%
%% Description: Starts an interactive shell to an SSH server on the
%% given <Host>. The function waits for user input,
%% and will not return until the remote shell is ended.(e.g. on
%% exit from the shell)
%%--------------------------------------------------------------------
shell(Socket) when is_port(Socket) ->
    shell(Socket, []);
shell(Host) ->
    shell(Host, ?SSH_DEFAULT_PORT, []).

shell(Socket, Options) when is_port(Socket) ->
    start_shell( connect(Socket, Options) );
shell(Host, Options) ->
    shell(Host, ?SSH_DEFAULT_PORT, Options).

shell(Host, Port, Options) ->
    start_shell( connect(Host, Port, Options) ).


start_shell({ok, ConnectionRef}) ->
    case ssh_connection:session_channel(ConnectionRef, infinity) of
	{ok,ChannelId}  ->
	    success = ssh_connection:ptty_alloc(ConnectionRef, ChannelId, []),
	    Args = [{channel_cb, ssh_shell},
		    {init_args,[ConnectionRef, ChannelId]},
		    {cm, ConnectionRef}, {channel_id, ChannelId}],
	    {ok, State} = ssh_channel:init([Args]),
	    ssh_channel:enter_loop(State);
	Error ->
	    Error
    end;
start_shell(Error) ->
    Error.

%%--------------------------------------------------------------------
-spec default_algorithms() -> algs_list() .
%%--------------------------------------------------------------------
default_algorithms() ->
    ssh_transport:default_algorithms().

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
finalize_start(Host, Port, Profile, Options0, F) ->
    try
        sshd_sup:start_child(Host, Port, Profile, Options0)
    of
        {error, {already_started, _}} ->
            {error, eaddrinuse};
        {error, Error} ->
            {error, Error};
        Result = {ok,_} ->
            F(Options0, Result)
    catch
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
