%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dgud@erlang.org>
%%% @copyright (C) 2010, Dan Gudmundsson
%%% @doc  start server with -proto_dist inet_proxy and net_kernel:start([s@faenor, shortnames]).
%%%
%%% @end
%%% Created : 22 Jun 2010 by Dan Gudmundsson <dgud@erlang.org>
%%%-------------------------------------------------------------------
-module(proxy_server).

-export([init/1, start_link/0, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-include_lib("kernel/src/net_address.hrl").
-include_lib("kernel/src/dist.hrl").
-include_lib("kernel/src/dist_util.hrl").

-record(state, 
	{listen,
	 accept_loop
	}).

-define(PPRE, 4).
-define(PPOST, 4).

start_link() ->
    gen_server:start_link({local, proxy_server}, proxy_server, [], []).

init([]) ->
    io:format("~p: init~n",[self()]),
    process_flag(priority, max),
    {ok, #state{}}.

handle_call(What = {listen, Name}, _From, State) ->
    io:format("~p: call listen ~p~n",[self(), What]),
    case gen_tcp:listen(0, [{active, false}, {packet,?PPRE}]) of
	{ok, Socket} ->
	    {ok, World} = gen_tcp:listen(0, [{active, false}, binary, {packet,?PPRE}]),
	    TcpAddress = get_tcp_address(Socket),
	    WorldTcpAddress = get_tcp_address(World),
	    {_,Port} = WorldTcpAddress#net_address.address,
	    {ok, Creation} = erl_epmd:register_node(Name, Port),
	    {reply, {ok, {Socket, TcpAddress, Creation}},
	     State#state{listen={Socket, World}}};
	Error ->
	    {reply, Error, State}
    end;

handle_call(What = {accept, Listen}, {From, _}, State = #state{listen={_, World}}) ->
    io:format("~p: call accept ~p~n",[self(), What]),
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
	    ok = gen_tcp:controlling_process(Socket, From),
	    {reply, Res, State};
	{Pid, Error} ->
	    {reply, Error, State}
    end;

handle_call({get_remote_id, {Socket,_Node}}, _From, State) ->
    Address = get_tcp_address(Socket),
    io:format("~p: get_remote_id ~p~n",[self(), Address]),
    {reply, Address, State};

handle_call(What, _From, State) ->
    io:format("~p: call ~p~n",[self(), What]),
    {reply, ok, State}.

handle_cast(What, State) ->
    io:format("~p: cast ~p~n",[self(), What]),
    {noreply, State}.

handle_info(What, State) ->
    io:format("~p: info ~p~n",[self(), What]),
    {noreply, State}.

terminate(_Reason, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_tcp_address(Socket) ->
    {ok, Address} = inet:sockname(Socket),
    {ok, Host} = inet:gethostname(),
    #net_address{
		  address = Address,
		  host = Host,
		  protocol = proxy,
		  family = inet
		}.

accept_loop(Proxy, Type, Listen, Extra) ->
    process_flag(priority, max),
    case Type of
	erts ->
	    case gen_tcp:accept(Listen) of
		{ok, Socket} ->
		    io:format("~p: erts accept~n",[self()]),
		    Extra ! {accept,self(),Socket,inet,proxy},
		    receive 
			{_Kernel, controller, Pid} ->
			    ok = gen_tcp:controlling_process(Socket, Pid),
			    Pid ! {self(), controller};
			{_Kernel, unsupported_protocol} ->
			    exit(unsupported_protocol)
		    end;
		Error ->
		    exit(Error)
	    end;
	world ->
	    case gen_tcp:accept(Listen) of
		{ok, Socket} ->
		    Opts = get_ssl_options(server),
		    {ok, SslSocket} = ssl:ssl_accept(Socket, Opts),
		    io:format("~p: world accept~n",[self()]),
		    PairHandler = spawn_link(fun() -> setup_connection(SslSocket, Extra) end),
		    ok = ssl:controlling_process(SslSocket, PairHandler);
		Error ->
		    exit(Error)
	    end
    end,
    accept_loop(Proxy, Type, Listen, Extra).


try_connect(Port) ->
    case gen_tcp:connect({127,0,0,1}, Port, [{active, false}, {packet,?PPRE}]) of
	R = {ok, _S} ->
	    R;
	{error, _R} ->
	    io:format("Failed ~p~n",[_R]),
	    try_connect(Port)
    end.

setup_proxy(Ip, Port, Parent) ->
    process_flag(trap_exit, true),
    Opts = get_ssl_options(client),
    case ssl:connect(Ip, Port, [{active, true}, binary, {packet,?PPRE}] ++ Opts) of
	{ok, World} ->
	    {ok, ErtsL} = gen_tcp:listen(0, [{active, true}, binary, {packet,?PPRE}]),
	    #net_address{address={_,LPort}} = get_tcp_address(ErtsL),
	    Parent ! {self(), go_ahead, LPort},
	    case gen_tcp:accept(ErtsL) of
		{ok, Erts} ->
		    %% gen_tcp:close(ErtsL),
		    io:format("World ~p Erts ~p~n",[World, Erts]),
		    loop_conn_setup(World, Erts);
		Err ->
		    Parent ! {self(), Err}
	    end;
	Err ->
	    Parent ! {self(), Err}
    end.

setup_connection(World, ErtsListen) ->
    process_flag(trap_exit, true),
    io:format("Setup connection ~n",[]),
    TcpAddress = get_tcp_address(ErtsListen),
    {_Addr,Port} = TcpAddress#net_address.address,
    {ok, Erts} = gen_tcp:connect({127,0,0,1}, Port, [{active, true}, binary, {packet,?PPRE}]),
    ssl:setopts(World, [{active,true}, {packet,?PPRE}]),
    io:format("~p ~n",[?LINE]),
    loop_conn_setup(World, Erts).

loop_conn_setup(World, Erts) ->
    receive 
	{ssl, World, Data = <<$a, _/binary>>} ->
	    gen_tcp:send(Erts, Data),
	    io:format("Handshake finished World -> Erts ~p ~c~n",[size(Data), $a]),
	    ssl:setopts(World, [{packet,?PPOST}]),
	    inet:setopts(Erts, [{packet,?PPOST}]),
	    loop_conn(World, Erts);
	{tcp, Erts, Data = <<$a, _/binary>>} ->
	    ssl:send(World, Data),
	    io:format("Handshake finished Erts -> World ~p ~c~n",[size(Data), $a]),
	    ssl:setopts(World, [{packet,?PPOST}]),
	    inet:setopts(Erts, [{packet,?PPOST}]),
	    loop_conn(World, Erts);

	{ssl, World, Data = <<H, _/binary>>} ->
	    gen_tcp:send(Erts, Data),
	    io:format("Handshake World -> Erts ~p ~c~n",[size(Data), H]),
	    loop_conn_setup(World, Erts);
	{tcp, Erts, Data = <<H, _/binary>>} ->
	    ssl:send(World, Data),
	    io:format("Handshake Erts -> World ~p ~c~n",[size(Data), H]),
	    loop_conn_setup(World, Erts);
	{ssl, World, Data} ->
	    gen_tcp:send(Erts, Data),
	    io:format("World -> Erts ~p <<>>~n",[size(Data)]),
	    loop_conn_setup(World, Erts);
	{tcp, Erts, Data} ->
	    ssl:send(World, Data),
	    io:format("Erts -> World ~p <<>>~n",[size(Data)]),
	    loop_conn_setup(World, Erts);
	Other ->
	    io:format("~p ~p~n",[?LINE, Other])
    end.

loop_conn(World, Erts) ->
    receive 
	{ssl, World, Data = <<H, _/binary>>} ->
	    gen_tcp:send(Erts, Data),
	    io:format("World -> Erts ~p ~c~n",[size(Data), H]),
	    loop_conn(World, Erts);
	{tcp, Erts, Data = <<H, _/binary>>} ->
	    ssl:send(World, Data),
	    io:format("Erts -> World ~p ~c~n",[size(Data), H]),
	    loop_conn(World, Erts);
	{ssl, World, Data} ->
	    gen_tcp:send(Erts, Data),
	    io:format("World -> Erts ~p <<>>~n",[size(Data)]),
	    loop_conn(World, Erts);
	{tcp, Erts, Data} ->
	    ssl:send(World, Data),
	    io:format("Erts -> World ~p <<>>~n",[size(Data)]),
	    loop_conn(World, Erts);

	Other ->
	    io:format("~p ~p~n",[?LINE, Other])
    end.

get_ssl_options(Type) ->
    case init:get_argument(ssl_dist_opt) of
	{ok, Args} ->
	    ssl_options(Type, Args);
	_ ->
	    []
    end.

ssl_options(_,[]) ->
    [];
ssl_options(server, [["server_certfile", Value]|T]) ->
    [{certfile, Value} | ssl_options(server,T)];
ssl_options(client, [["client_certfile", Value]|T]) ->
    [{certfile, Value} | ssl_options(client,T)];
ssl_options(server, [["server_cacertfile", Value]|T]) ->
    [{cacertfile, Value} | ssl_options(server,T)];
ssl_options(server, [["server_keyfile", Value]|T]) ->
    [{keyfile, Value} | ssl_options(server,T)];
ssl_options(Type, [["client_certfile", _Value]|T]) ->
    ssl_options(Type,T);
ssl_options(Type, [["server_certfile", _Value]|T]) ->
    ssl_options(Type,T);
ssl_options(Type, [[Item, Value]|T]) ->
    [{atomize(Item),fixup(Value)} | ssl_options(Type,T)];
ssl_options(Type, [[Item,Value |T1]|T2]) ->
    ssl_options(atomize(Type),[[Item,Value],T1|T2]);
ssl_options(_,_) ->
    exit(malformed_ssl_dist_opt).

fixup(Value) ->
    case catch list_to_integer(Value) of
	{'EXIT',_} ->
	    Value;
	Int ->
	    Int
    end.

atomize(List) when is_list(List) ->
    list_to_atom(List);
atomize(Atom) when is_atom(Atom) ->
    Atom.
