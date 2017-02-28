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
		    Opts = ?PUT_INTERNAL_OPT([{user_pid,self()}, {host,Host}], Options),
		    ssh_connection_handler:start_connection(client, Socket, Opts, Timeout);
		{error, Reason} ->
		    {error, Reason}
	    catch
		exit:{function_clause, _F} ->
                    io:format('function_clause ~p~n',[_F]),
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


daemon(Port, UserOptions) when is_integer(Port), Port >= 0 ->
    daemon(any, Port, UserOptions);

daemon(Socket, UserOptions) when is_port(Socket) ->
    daemon(socket, Socket, UserOptions).


daemon(Host0, Port, UserOptions0) ->
    {Host, UserOptions} = handle_daemon_args(Host0, UserOptions0),
    start_daemon(Host, Port, ssh_options:handle_options(server, UserOptions)).

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
handle_daemon_args(Host, UserOptions0) ->
    case Host of
        socket ->
            {Host, UserOptions0};
        any ->
            {ok, Host0} = inet:gethostname(),
            Inet = proplists:get_value(inet, UserOptions0, inet),
            {Host0, [Inet | UserOptions0]};
        {_,_,_,_} ->
            {Host, [inet, {ip,Host} | UserOptions0]};
        {_,_,_,_,_,_,_,_} ->
            {Host, [inet6, {ip,Host} | UserOptions0]};
        _ ->
            error(badarg)
    end.

%%%----------------------------------------------------------------
valid_socket_to_use(Socket, {tcp,_,_}) ->
    %% Is this tcp-socket a valid socket?
    case {is_tcp_socket(Socket),
          {ok,[{active,false}]} == inet:getopts(Socket, [active])
         }
    of
        {true, true} ->
            ok;
        {true, false} ->
            {error, not_passive_mode};
        _ ->
            {error, not_tcp_socket}
    end;

valid_socket_to_use(_, {L4,_,_}) ->
    {error, {unsupported,L4}}.


is_tcp_socket(Socket) ->
    case inet:getopts(Socket, [delay_send]) of
        {ok,[_]} -> true;
        _ -> false
    end.

%%%----------------------------------------------------------------
start_daemon(_, _, {error,Error}) ->
    {error,Error};

start_daemon(socket, Socket, Options) ->
    case valid_socket_to_use(Socket, ?GET_OPT(transport,Options)) of
        ok ->
            try
                do_start_daemon(Socket, Options)
            catch
                throw:bad_fd -> {error,bad_fd};
                throw:bad_socket -> {error,bad_socket};
                _C:_E -> {error,{cannot_start_daemon,_C,_E}}
            end;
        {error,SockError} ->
            {error,SockError}
    end;

start_daemon(Host, Port, Options) ->
    try
        do_start_daemon(Host, Port, Options)
    catch
        throw:bad_fd -> {error,bad_fd};
        throw:bad_socket -> {error,bad_socket};
        _C:_E -> {error,{cannot_start_daemon,_C,_E}}
    end.


do_start_daemon(Socket, Options) ->
    {ok, {IP,Port}} =
	try {ok,_} = inet:sockname(Socket)
	catch
	    _:_ -> throw(bad_socket)
	end,
    Host = fmt_host(IP),
    Opts = ?PUT_INTERNAL_OPT([{connected_socket, Socket},
                              {address, Host},
                              {port, Port},
                              {role, server}], Options),
    
    Profile = ?GET_OPT(profile, Options),
    case ssh_system_sup:system_supervisor(Host, Port, Profile) of
	undefined ->
	    try sshd_sup:start_child(Opts) of
		{error, {already_started, _}} ->
		    {error, eaddrinuse};
		Result = {ok,_} ->
		    call_ssh_acceptor_handle_connection(Host, Port, Opts, Socket, Result);
		Result = {error, _} ->
		    Result
	    catch
		exit:{noproc, _} ->
		    {error, ssh_not_started}
	    end;
	Sup  ->
	    AccPid = ssh_system_sup:acceptor_supervisor(Sup),
	    case ssh_acceptor_sup:start_child(AccPid, Opts) of
		{error, {already_started, _}} ->
		    {error, eaddrinuse};
		{ok, _} ->
		    call_ssh_acceptor_handle_connection(Host, Port, Opts, Socket, {ok,Sup});
		Other ->
		    Other
	    end
    end.

do_start_daemon(Host0, Port0, Options0) ->
    {Host,Port1} =
	try
	    case ?GET_SOCKET_OPT(fd, Options0) of
		undefined ->
		    {Host0,Port0};
		Fd when Port0==0 ->
		    find_hostport(Fd)
	    end
	catch
	    _:_ -> throw(bad_fd)
	end,
    {Port, WaitRequestControl, Options1} =
	case Port1 of
	    0 -> %% Allocate the socket here to get the port number...
		{ok,LSock} = ssh_acceptor:callback_listen(0, Options0),
		{ok,{_,LPort}} = inet:sockname(LSock),
		{LPort,
		 LSock,
		 ?PUT_INTERNAL_OPT({lsocket,{LSock,self()}}, Options0)
		};
	    _ ->
		{Port1, false, Options0}
	end,
    Options = ?PUT_INTERNAL_OPT([{address, Host},
                                 {port, Port},
                                 {role, server}], Options1),
    Profile = ?GET_OPT(profile, Options0),
    case ssh_system_sup:system_supervisor(Host, Port, Profile) of
	undefined ->
	    try sshd_sup:start_child(Options) of
		{error, {already_started, _}} ->
		    {error, eaddrinuse};
		Result = {ok,_} ->
		    sync_request_control(WaitRequestControl, Options),
		    Result;
		Result = {error, _} ->
		    Result
	    catch
		exit:{noproc, _} ->
		    {error, ssh_not_started}
	    end;
	Sup ->
	    AccPid = ssh_system_sup:acceptor_supervisor(Sup),
	    case ssh_acceptor_sup:start_child(AccPid, Options) of
		{error, {already_started, _}} ->
		    {error, eaddrinuse};
		{ok, _} ->
		    sync_request_control(WaitRequestControl, Options),
		    {ok, Sup};
		Other ->
		    Other
	    end
    end.

call_ssh_acceptor_handle_connection(Host, Port, Options, Socket, DefaultResult) ->
    {_, Callback, _} = ?GET_OPT(transport, Options),
    try ssh_acceptor:handle_connection(Callback, Host, Port, Options, Socket)
    of
        {error,Error} -> {error,Error};
        _ -> DefaultResult
    catch
        C:R -> {error,{could_not_start_connection,{C,R}}}
    end.
             

sync_request_control(false, _Options) ->
    ok;
sync_request_control(LSock, Options) ->
    {_, Callback, _} = ?GET_OPT(transport, Options),
    receive
	{request_control,LSock,ReqPid} ->
	    ok = Callback:controlling_process(LSock, ReqPid),
	    ReqPid ! {its_yours,LSock},
	    ok
    end.

find_hostport(Fd) ->
    %% Using internal functions inet:open/8 and inet:close/0.
    %% Don't try this at home unless you know what you are doing!
    {ok,S} = inet:open(Fd, {0,0,0,0}, 0, [], tcp, inet, stream, inet_tcp),
    {ok, HostPort} = inet:sockname(S),
    ok = inet:close(S),
    HostPort.

fmt_host({A,B,C,D}) ->
    lists:concat([A,".",B,".",C,".",D]);
fmt_host(T={_,_,_,_,_,_,_,_}) ->
    lists:flatten(string:join([io_lib:format("~.16B",[A]) || A <- tuple_to_list(T)], ":")).
