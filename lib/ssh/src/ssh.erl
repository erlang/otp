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
		    Opts = ?PUT_INTERNAL_OPT([{user_pid,self()}, {host,fmt_host(Host)}], Options),
		    ssh_connection_handler:start_connection(client, Socket, Opts, Timeout);
		{error,SockError} ->
		    {error,SockError}
	    end
    end;

connect(Host, Port, UserOptions) when is_integer(Port),
                                      Port>0,
                                      is_list(UserOptions) ->
    connect(Host, Port, UserOptions, infinity).

connect(Host, Port, UserOptions, Timeout) when is_integer(Port),
                                               Port>0,
                                               is_list(UserOptions) ->
    case ssh_options:handle_options(client, UserOptions) of
	{error, _Reason} = Error ->
	    Error;
        Options ->
	    {_, Transport, _} = TransportOpts = ?GET_OPT(transport, Options),
	    ConnectionTimeout = ?GET_OPT(connect_timeout, Options),
            SocketOpts = [{active,false} | ?GET_OPT(socket_options,Options)],
	    try Transport:connect(Host, Port, SocketOpts, ConnectionTimeout) of
		{ok, Socket} ->
		    Opts = ?PUT_INTERNAL_OPT([{user_pid,self()}, {host,fmt_host(Host)}], Options),
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
                finalize_start(fmt_host(IP), Port, ?GET_OPT(profile, Options),
                               ?PUT_INTERNAL_OPT({connected_socket, Socket}, Options),
                               fun(Opts, DefaultResult) ->
                                       try ssh_acceptor:handle_established_connection(
                                             ?GET_INTERNAL_OPT(address, Opts),
                                             ?GET_INTERNAL_OPT(port, Opts),
                                             Opts, 
                                             Socket)
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


daemon(Host0, Port0, UserOptions0) when 0 =< Port0, Port0 =< 65535 ->
    try
        {Host1, UserOptions} = handle_daemon_args(Host0, UserOptions0),
        #{} = Options0 = ssh_options:handle_options(server, UserOptions),
        
        {{Host,Port}, ListenSocket} =
            open_listen_socket(Host1, Port0, Options0),

        %% Now Host,Port is what to use for the supervisor to register its name,
        %% and ListenSocket is for listening on connections. But it is still owned
        %% by self()...

        finalize_start(fmt_host(Host), Port, ?GET_OPT(profile, Options0),
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
    end.


%%--------------------------------------------------------------------
-spec daemon_info(daemon_ref()) -> ok_error( [{atom(), term()}] ).

daemon_info(Pid) ->
    case catch ssh_system_sup:acceptor_supervisor(Pid) of
	AsupPid when is_pid(AsupPid) ->
	    [{ListenAddr,Port,Profile}] =
		[{LA,Prt,Prf} || {{ssh_acceptor_sup,LA,Prt,Prf},
                                  _WorkerPid,worker,[ssh_acceptor]} <- supervisor:which_children(AsupPid)],
	    {ok, [{port,Port},
                  {listen_address,ListenAddr},
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
stop_listener(Address, Port, Profile) ->
    ssh_system_sup:stop_listener(Address, Port, Profile).

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
    ssh_system_sup:stop_system(Address, Port, ?DEFAULT_PROFILE).
stop_daemon(Address, Port, Profile) ->
    ssh_system_sup:stop_system(Address, Port, Profile).

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
handle_daemon_args(HostAddr, Opts) ->
    IP = proplists:get_value(ip, Opts),
    IPh = case inet:parse_strict_address(HostAddr) of
              {ok, IPtuple}   -> IPtuple;
              {error, einval} when is_tuple(HostAddr),
                                   size(HostAddr)==4 ; size(HostAddr)==6 -> HostAddr;
              _ -> undefined
          end,
    handle_daemon_args(HostAddr, IPh, IP, Opts).


%% HostAddr is 'any'
handle_daemon_args(any, undefined, undefined, Opts) -> {any, Opts};
handle_daemon_args(any, undefined, IP,        Opts) -> {IP,  Opts};

%% HostAddr is 'loopback' or "localhost" 
handle_daemon_args(loopback, undefined, {127,_,_,_}=IP,       Opts) -> {IP,  Opts};
handle_daemon_args(loopback, undefined, {0,0,0,0,0,0,0,1}=IP, Opts) -> {IP,  Opts};
handle_daemon_args(loopback, undefined, undefined,            Opts) -> 
    IP = case proplists:get_value(inet,Opts) of
             true -> {127,0,0,1};
             inet -> {127,0,0,1};
             inet6 -> {0,0,0,0,0,0,0,1};
             _ -> case proplists:get_value(inet6,Opts) of
                      true -> {0,0,0,0,0,0,0,1};
                      _ -> {127,0,0,1} % default if no 'inet' nor 'inet6'
                      end
         end,
    {IP, [{ip,IP}|Opts]};
handle_daemon_args("localhost", IPh, IP, Opts) ->
    handle_daemon_args(loopback, IPh, IP, Opts);

%% HostAddr is ip and no ip-option
handle_daemon_args(_, IP, undefined, Opts) when is_tuple(IP) -> {IP, [{ip,IP}|Opts]};

%% HostAddr and ip-option are equal
handle_daemon_args(_, IP, IP, Opts) when is_tuple(IP) -> {IP, Opts};

%% HostAddr is ip, but ip-option is different!
handle_daemon_args(_, IPh, IPo, _) when is_tuple(IPh), is_tuple(IPo) -> error({eoption,{ip,IPo}});

%% Something else. Whatever it is, it is wrong.
handle_daemon_args(_, _, _, _) -> error(badarg).

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
open_listen_socket(Host0, Port0, Options0) ->
    case ?GET_SOCKET_OPT(fd, Options0) of
        undefined ->
            {ok,LSock} = ssh_acceptor:listen(Port0, Options0),
            {ok,{_,LPort}} = inet:sockname(LSock),
            {{Host0,LPort}, LSock};
        
        Fd when is_integer(Fd) ->
            %% Do gen_tcp:listen with the option {fd,Fd}:
            {ok,LSock} = ssh_acceptor:listen(0, Options0),
            {ok,{LHost,LPort}} = inet:sockname(LSock),
            {{LHost,LPort}, LSock}
    end.

%%%----------------------------------------------------------------
finalize_start(Host, Port, Profile, Options0, F) ->
    Options = ?PUT_INTERNAL_OPT([{address, Host},
                                 {port, Port},
                                 {role, server}], Options0),
    case ssh_system_sup:system_supervisor(Host, Port, Profile) of
	undefined ->
	    try sshd_sup:start_child(Options) of
		{error, {already_started, _}} ->
		    {error, eaddrinuse};
		{error, Error} ->
                    {error, Error};
		Result = {ok,_} ->
                    F(Options, Result)
	    catch
		exit:{noproc, _} ->
		    {error, ssh_not_started}
	    end;
	Sup  ->
	    AccPid = ssh_system_sup:acceptor_supervisor(Sup),
	    case ssh_acceptor_sup:start_child(AccPid, Options) of
		{error, {already_started, _}} ->
		    {error, eaddrinuse};
		{error, Error} ->
                    {error, Error};
		{ok, _} ->
                    F(Options, {ok,Sup})
	    end
    end.

%%%----------------------------------------------------------------
fmt_host(any) -> any;
fmt_host(IP)  when is_tuple(IP) ->  inet:ntoa(IP);
fmt_host(Str) when is_list(Str) -> Str.
