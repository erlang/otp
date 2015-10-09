%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2013. All Rights Reserved.
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
-module(ssl_tls_dist_proxy).


-export([listen/1, accept/1, connect/2, get_tcp_address/1]).
-export([init/1, start_link/0, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3, ssl_options/2]).

-include_lib("kernel/include/net_address.hrl").

-record(state, 
	{listen,
	 accept_loop
	}).

-define(PPRE, 4).
-define(PPOST, 4).


%%====================================================================
%% Internal application API
%%====================================================================

listen(Name) ->
    gen_server:call(?MODULE, {listen, Name}, infinity). 

accept(Listen) ->
    gen_server:call(?MODULE, {accept, Listen}, infinity).

connect(Ip, Port) ->
    gen_server:call(?MODULE, {connect, Ip, Port}, infinity).

%%====================================================================
%% gen_server callbacks
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(priority, max),
    {ok, #state{}}.

handle_call({listen, Name}, _From, State) ->
    case gen_tcp:listen(0, [{active, false}, {packet,?PPRE}]) of
	{ok, Socket} ->
	    {ok, World} = gen_tcp:listen(0, [{active, false}, binary, {packet,?PPRE}]),
	    {ok, TcpAddress} = get_tcp_address(Socket),
	    {ok, WorldTcpAddress} = get_tcp_address(World),
	    {_,Port} = WorldTcpAddress#net_address.address,
	    {ok, Creation} = erl_epmd:register_node(Name, Port),
	    {reply, {ok, {Socket, TcpAddress, Creation}},
	     State#state{listen={Socket, World}}};
	Error ->
	    {reply, Error, State}
    end;

handle_call({accept, Listen}, {From, _}, State = #state{listen={_, World}}) ->
    Self = self(),
    ErtsPid = spawn_link(fun() -> accept_loop(Self, erts, Listen, From) end),
    WorldPid = spawn_link(fun() -> accept_loop(Self, world, World, Listen) end),
    {reply, ErtsPid, State#state{accept_loop={ErtsPid, WorldPid}}};

handle_call({connect, Ip, Port}, {From, _}, State) ->
    Me = self(),
    Pid = spawn_link(fun() -> setup_proxy(Ip, Port, Me) end),
    receive 
	{Pid, go_ahead, LPort} -> 
	    Res = {ok, Socket} = try_connect(LPort),
	    case gen_tcp:controlling_process(Socket, From) of
		{error, badarg} = Error -> {reply, Error, State};   % From is dead anyway.
		ok ->
		    flush_old_controller(From, Socket),
		    {reply, Res, State}
	    end;
	{Pid, Error} ->
	    {reply, Error, State}
    end;

handle_call(_What, _From, State) ->
    {reply, ok, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, State}.

terminate(_Reason, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
get_tcp_address(Socket) ->
    case inet:sockname(Socket) of
	{ok, Address} ->
	    {ok, Host} = inet:gethostname(),
	    NetAddress = #net_address{
			    address = Address,
			    host = Host,
			    protocol = proxy,
			    family = inet
			   },
	    {ok, NetAddress};
	{error, _} = Error -> Error
    end.

accept_loop(Proxy, erts = Type, Listen, Extra) ->
    process_flag(priority, max),
    case gen_tcp:accept(Listen) of
	{ok, Socket} ->
	    Extra ! {accept,self(),Socket,inet,proxy},
	    receive 
		{_Kernel, controller, Pid} ->
		    ok = gen_tcp:controlling_process(Socket, Pid),
		    flush_old_controller(Pid, Socket),
		    Pid ! {self(), controller};
		{_Kernel, unsupported_protocol} ->
		    exit(unsupported_protocol)
	    end;
	Error ->
	    exit(Error)
    end,
    accept_loop(Proxy, Type, Listen, Extra);

accept_loop(Proxy, world = Type, Listen, Extra) ->
    process_flag(priority, max),
    case gen_tcp:accept(Listen) of
	{ok, Socket} ->
	    Opts = get_ssl_options(server),
	    case ssl:ssl_accept(Socket, Opts) of
		{ok, SslSocket} ->
		    PairHandler =
			spawn_link(fun() ->
					   setup_connection(SslSocket, Extra)
				   end),
		    ok = ssl:controlling_process(SslSocket, PairHandler),
		    flush_old_controller(PairHandler, SslSocket);
		_ ->
		    gen_tcp:close(Socket)
	    end;
	Error ->
	    exit(Error)
    end,
    accept_loop(Proxy, Type, Listen, Extra).

try_connect(Port) ->
    case gen_tcp:connect({127,0,0,1}, Port, [{active, false}, {packet,?PPRE}]) of
	R = {ok, _S} ->
	    R;
	{error, _R} ->
	    try_connect(Port)
    end.

setup_proxy(Ip, Port, Parent) ->
    process_flag(trap_exit, true),
    Opts = get_ssl_options(client),
    case ssl:connect(Ip, Port, [{active, true}, binary, {packet,?PPRE}] ++ Opts) of
	{ok, World} ->
	    {ok, ErtsL} = gen_tcp:listen(0, [{active, true}, {ip, {127,0,0,1}}, binary, {packet,?PPRE}]),
	    {ok, #net_address{address={_,LPort}}} = get_tcp_address(ErtsL),
	    Parent ! {self(), go_ahead, LPort},
	    case gen_tcp:accept(ErtsL) of
		{ok, Erts} ->
		    %% gen_tcp:close(ErtsL),
		    loop_conn_setup(World, Erts);
		Err ->
		    Parent ! {self(), Err}
	    end;
	Err ->
	    Parent ! {self(), Err}
    end.

setup_connection(World, ErtsListen) ->
    process_flag(trap_exit, true),
    {ok, TcpAddress} = get_tcp_address(ErtsListen),
    {_Addr,Port} = TcpAddress#net_address.address,
    {ok, Erts} = gen_tcp:connect({127,0,0,1}, Port, [{active, true}, binary, {packet,?PPRE}]),
    ssl:setopts(World, [{active,true}, {packet,?PPRE}]),
    loop_conn_setup(World, Erts).

loop_conn_setup(World, Erts) ->
    receive 
	{ssl, World, Data = <<$a, _/binary>>} ->
	    gen_tcp:send(Erts, Data),
	    ssl:setopts(World, [{packet,?PPOST}]),
	    inet:setopts(Erts, [{packet,?PPOST}]),
	    loop_conn(World, Erts);
	{tcp, Erts, Data = <<$a, _/binary>>} ->
	    ssl:send(World, Data),
	    ssl:setopts(World, [{packet,?PPOST}]),
	    inet:setopts(Erts, [{packet,?PPOST}]),
	    loop_conn(World, Erts);
	{ssl, World, Data = <<_, _/binary>>} ->
	    gen_tcp:send(Erts, Data),
	    loop_conn_setup(World, Erts);
	{tcp, Erts, Data = <<_, _/binary>>} ->
	    ssl:send(World, Data),
	    loop_conn_setup(World, Erts);
	{ssl, World, Data} ->
	    gen_tcp:send(Erts, Data),
	    loop_conn_setup(World, Erts);
	{tcp, Erts, Data} ->
	    ssl:send(World, Data),
	    loop_conn_setup(World, Erts);
	{tcp_closed, Erts} ->
	    ssl:close(World);
	{ssl_closed,  World} ->
	    gen_tcp:close(Erts);
	{ssl_error, World, _} ->

	    ssl:close(World)
    end.

loop_conn(World, Erts) ->
    receive 
	{ssl, World, Data} ->
	    gen_tcp:send(Erts, Data),
	    loop_conn(World, Erts);
	{tcp, Erts, Data} ->
	    ssl:send(World, Data),
	    loop_conn(World, Erts);
	{tcp_closed, Erts} ->
	    ssl:close(World);
	{ssl_closed,  World} ->
	    gen_tcp:close(Erts);
	{ssl_error, World, _} ->
	    ssl:close(World)
    end.

get_ssl_options(Type) ->
    case init:get_argument(ssl_dist_opt) of
	{ok, Args} ->
	    [{erl_dist, true} | ssl_options(Type, lists:append(Args))];
	_ ->
	    [{erl_dist, true}]
    end.

ssl_options(_,[]) ->
    [];
ssl_options(server, ["client_" ++ _, _Value |T]) ->
    ssl_options(server,T);
ssl_options(client, ["server_" ++ _, _Value|T]) ->
    ssl_options(client,T);
ssl_options(server, ["server_certfile", Value|T]) ->
    [{certfile, Value} | ssl_options(server,T)];
ssl_options(client, ["client_certfile", Value | T]) ->
    [{certfile, Value} | ssl_options(client,T)];
ssl_options(server, ["server_cacertfile", Value|T]) ->
    [{cacertfile, Value} | ssl_options(server,T)];
ssl_options(client, ["client_cacertfile", Value|T]) ->
    [{cacertfile, Value} | ssl_options(client,T)];
ssl_options(server, ["server_keyfile", Value|T]) ->
    [{keyfile, Value} | ssl_options(server,T)];
ssl_options(client, ["client_keyfile", Value|T]) ->
    [{keyfile, Value} | ssl_options(client,T)];
ssl_options(server, ["server_password", Value|T]) ->
    [{password, Value} | ssl_options(server,T)];
ssl_options(client, ["client_password", Value|T]) ->
    [{password, Value} | ssl_options(client,T)];
ssl_options(server, ["server_verify", Value|T]) ->
    [{verify, atomize(Value)} | ssl_options(server,T)];
ssl_options(client, ["client_verify", Value|T]) ->
    [{verify, atomize(Value)} | ssl_options(client,T)];
ssl_options(server, ["server_reuse_sessions", Value|T]) ->
    [{reuse_sessions, atomize(Value)} | ssl_options(server,T)];
ssl_options(client, ["client_reuse_sessions", Value|T]) ->
    [{reuse_sessions, atomize(Value)} | ssl_options(client,T)];
ssl_options(server, ["server_secure_renegotiate", Value|T]) ->
    [{secure_renegotiate, atomize(Value)} | ssl_options(server,T)];
ssl_options(client, ["client_secure_renegotiate", Value|T]) ->
    [{secure_renegotiate, atomize(Value)} | ssl_options(client,T)];
ssl_options(server, ["server_depth", Value|T]) ->
    [{depth, list_to_integer(Value)} | ssl_options(server,T)];
ssl_options(client, ["client_depth", Value|T]) ->
    [{depth, list_to_integer(Value)} | ssl_options(client,T)];
ssl_options(server, ["server_hibernate_after", Value|T]) ->
    [{hibernate_after, list_to_integer(Value)} | ssl_options(server,T)];
ssl_options(client, ["client_hibernate_after", Value|T]) ->
    [{hibernate_after, list_to_integer(Value)} | ssl_options(client,T)];
ssl_options(server, ["server_ciphers", Value|T]) ->
    [{ciphers, Value} | ssl_options(server,T)];
ssl_options(client, ["client_ciphers", Value|T]) ->
    [{ciphers, Value} | ssl_options(client,T)];
ssl_options(server, ["server_dhfile", Value|T]) ->
    [{dhfile, Value} | ssl_options(server,T)];
ssl_options(server, ["server_fail_if_no_peer_cert", Value|T]) ->
    [{fail_if_no_peer_cert, atomize(Value)} | ssl_options(server,T)];
ssl_options(_,_) ->
    exit(malformed_ssl_dist_opt).

atomize(List) when is_list(List) ->
    list_to_atom(List);
atomize(Atom) when is_atom(Atom) ->
    Atom.

flush_old_controller(Pid, Socket) ->
    receive
	{tcp, Socket, Data} ->
	    Pid ! {tcp, Socket, Data},
	    flush_old_controller(Pid, Socket);
	{tcp_closed, Socket} ->
	    Pid ! {tcp_closed, Socket},
	    flush_old_controller(Pid, Socket);
	{ssl, Socket, Data} ->
	    Pid ! {ssl, Socket, Data},
	    flush_old_controller(Pid, Socket);
	{ssl_closed, Socket} ->
	    Pid ! {ssl_closed, Socket},
	    flush_old_controller(Pid, Socket)
    after 0 ->
	    ok
    end.
