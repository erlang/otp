%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%

%%----------------------------------------------------------------------
%% Purpose : UDP API Wrapper
%%----------------------------------------------------------------------

-module(dtls_udp).

-behavior(gen_server).

-export([connect/3, connect/4, accept/2, listen/2, shutdown/2, close/1, controlling_process/2]).
-export([send/2, recv/2, recv/3, handle_ssl_info/2]).
-export([getopts/2, setopts/2, port/1, peername/1, sockname/1]).
-export([connection_type/1, callback_info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("ssl_internal.hrl").

-define(PROTOCOL, ?MODULE).

-define(ECLOSED, {error, closed}).
-define(ENOTCONN, {error, enotconn}).

connect(Address, Port, Options, _Timeout) ->
    connect(Address, Port, Options).

connect(Address, Port, Opts0) ->
    Options = lists:filter(fun({packet, _}) -> false;
			      ({packet_size, _}) -> false;
			      (_) -> true end, Opts0),
    case gen_udp:open(0, Options) of
	{ok, Socket} ->
	    case gen_udp:connect(Socket, Address, Port) of
		ok ->
		    {ok, Socket};
		Error = {error, _Reason} ->
		    Error
	    end;
	Error = {error, _Reason} ->
	    Error
    end.

accept(ListenSocket, Timeout) ->
    call(ListenSocket, accept, Timeout, infinity).

listen(Port, Options) ->
%%    gen_server:start_link(?MODULE, [Port, Options], [{debug, [trace]}]).
    gen_server:start_link(?MODULE, [self(), Port, Options], []).

controlling_process(Socket, Pid) when is_port(Socket) ->
    gen_udp:controlling_process(Socket, Pid);
controlling_process(Socket, Pid) ->
    call(Socket, controlling_process, {self(), Pid}).

close(Socket) when is_port(Socket) ->
    gen_udp:close(Socket);
close(Socket) ->
    call(Socket, close, undefined).

send(Socket, Data) when is_port(Socket) ->
    gen_udp:send(Socket, Data);
send(Socket, Data) ->
    call(Socket, send, Data).

recv(Socket, Length) ->
    recv(Socket, Length, infinity).

recv(Socket, Length, Timeout) when is_port(Socket) ->
    case gen_udp:recv(Socket, Length, Timeout) of
	{ok, {_Address, _Port, Packet}} ->
	    {ok, Packet};
	Error ->
	    Error
    end;
recv(Socket, Length, Timeout) ->
    call(Socket, recv, {Length, Timeout}).

shutdown(Socket, _How) when is_port(Socket) ->
    ok;
shutdown(Socket, How) ->
    call(Socket, shutdown, How).

%% map UDP port info's to three-tupple format
handle_ssl_info(Socket, {udp, Socket, _Address, _Port, Packet}) ->
    {next, {?PROTOCOL, Socket, Packet}};
handle_ssl_info(_, Info) ->
    Info.

getopts(Socket, Options) when is_port(Socket) ->
    inet:getopts(Socket, Options);
getopts(Socket, Options) ->
    call(Socket, getopts, Options).

setopts(Socket, Options) when is_port(Socket) ->
    inet:setopts(Socket, Options);
setopts(Socket, Options) ->
    call(Socket, setopts, Options).

peername(Socket) when is_port(Socket) ->
    inet:peername(Socket);
peername(Socket) ->
    call(Socket, peername, undefined).

sockname(Socket) when is_port(Socket) ->
    inet:sockname(Socket);
sockname(Socket) ->
    call(Socket, sockname, undefined).

port(Socket) when is_port(Socket) ->
    inet:port(Socket);
port(Socket) ->
    call(Socket, port, undefined).

connection_type(_Socket) ->
    datagram.

callback_info() ->
    {?MODULE, ?PROTOCOL, udp_closed, udp_error}.

%%----------------------------------
%% Port Logic
%%----------------------------------

call(Socket, Request, Args) ->
    call(Socket, Request, Args, 5000).

call(Socket, Request, Args, Timeout) when is_pid(Socket) ->
    call_socket(Socket, {Request, undefined, Args}, Timeout);
call({Socket, SslSocket}, Request, Args, Timeout) when is_pid(Socket) ->
    call_socket(Socket, {Request, SslSocket, Args}, Timeout).

call_socket(Socket, Request, Timeout) ->
    try
	gen_server:call(Socket, Request, Timeout)
    catch
	exit:{noproc,_} -> ?ECLOSED
    end.

ssl_socket(SslSocketId) ->
    {self(), SslSocketId}.

-record(state, {socket, owner, mode, ip_conns, ssl_conns, state = init, accepting}).
-record(ssl_socket, {id, owner, mode, queue}).

init([Owner, Port, Options0]) ->
    Options = proplists:expand([{binary, [{mode, binary}]},
				{list, [{mode, list}]}], Options0),
    Opts0 = lists:keystore(active, 1, Options, {active, true}),
    Opts = lists:keystore(mode, 1, Opts0, {mode, binary}),
    case gen_udp:open(Port, Opts) of
	{ok, Socket} ->
	    {ok, #state{socket = Socket,
			owner = Owner,
			mode = proplists:get_value(mode, Options, list),
			state = listen,
			ip_conns = gb_trees:empty(),
			ssl_conns = gb_trees:empty()}};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
-spec terminate(reason(), #state{}) -> ok.
%%
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
-spec code_change(term(), #state{}, list()) -> {ok, #state{}}.
%%
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% universal Socket operations
%% ---------------------------------------------------------------------------

handle_call({sockname, _, _}, _From, State = #state{socket = Socket}) ->
    Reply = inet:sockname(Socket),
    {reply, Reply, State};

handle_call({port, _, _}, _From, State = #state{socket = Socket}) ->
    Reply = inet:port(Socket),
    {reply, Reply, State};

%% ---------------------------------------------------------------------------
%% Listening Socket operations
%% ---------------------------------------------------------------------------

handle_call({accept, undefined, Timeout}, From, State = #state{state = listen}) ->
    {noreply, State#state{state = accepting, accepting = From}, Timeout};
handle_call({accept, undefined, _Timeout}, _From, State) ->
    {reply, {error, already_listening}, State};

handle_call({getopts, undefined, Options}, _From, State = #state{socket = Socket, mode = Mode}) ->
    case inet:getopts(Socket, Options) of
	{ok, SocketOptions} ->
	    Reply = {ok, lists:keystore(mode, 1, SocketOptions, {mode, Mode})};
	Reply ->
	    ok
    end,
    {reply, Reply, State};

handle_call({setopts, undefined, Options}, _From, State = #state{socket = Socket, mode = Mode}) ->
    Opts0 = lists:keystore(active, 1, Options, {active, true}),
    Opts = lists:keydelete(mode, 1, Opts0),
    Reply = inet:setopts(Socket, Opts),
    {reply, Reply, State#state{mode = proplists:get_value(mode, Options, Mode)}};

handle_call({controlling_process, undefined, {Old, New}}, _From, State = #state{owner = Old}) ->
    {reply, ok, State#state{owner = New}};
handle_call({controlling_process, undefined, _}, _From, State) ->
    {reply, {error, not_owner}, State};

handle_call({close, undefined, _Args}, _From, State0 = #state{socket = Socket}) ->
    Reply = gen_udp:close(Socket),
    State = reply_accept(?ECLOSED, State0),
    {reply, Reply, State#state{state = closed}};

handle_call({_, undefined, _Args}, _From, State = #state{state = closed}) ->
    {reply, ?ECLOSED, State};

handle_call({_, undefined, _Args}, _From, State) ->
    {reply, ?ENOTCONN, State};

%% ---------------------------------------------------------------------------
%% Connected Socket operations
%% ---------------------------------------------------------------------------

handle_call({close, SslSocketId, _}, From, State) ->
    with_socket(SslSocketId, SslSocketId, From, ok, fun socket_close/4, State);

handle_call({shutdown, SslSocketId, How}, From, State) ->
    with_socket(SslSocketId, How, From, ?ECLOSED, fun socket_shutdown/4, State);

handle_call({recv, SslSocketId, Args}, From, State) ->
    with_socket(SslSocketId, Args, From, ?ECLOSED, fun socket_recv/4, State);

handle_call({send, SslSocketId, Packet}, From, State) ->
    with_socket(SslSocketId, Packet, From, ?ENOTCONN, fun socket_send/4, State);

handle_call({setopts, SslSocketId, Options}, From, State) ->
    with_socket(SslSocketId, {SslSocketId, Options}, From, ?ENOTCONN, fun socket_setopts/4, State);

handle_call({peername, SslSocketId, Args}, From, State) ->
    with_socket(SslSocketId, Args, From, ?ENOTCONN, fun socket_peername/4, State);

handle_call({controlling_process, SslSocketId, Args}, From, State) ->
    with_socket(SslSocketId, Args, From, ?ENOTCONN, fun socket_controlling_process/4, State);

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%handle_call(Request, From, State = #state{socket = Socket, connections = Cons}) ->
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(timeout, State0 = #state{state = accepting}) ->
    State = reply_accept({error, timeout}, State0),
    {noreply, State};

handle_info({udp, Socket, IP, InPortNo, Packet},
	    State0 = #state{socket = Socket, ip_conns = IpConns}) ->
    IpKey = {IP, InPortNo},
    State1 = case gb_trees:lookup(IpKey, IpConns) of
		 none ->
		     handle_accept(IpKey, Packet, State0);
		 {value, SslSocket} ->
		     handle_packet(IpKey, SslSocket, Packet, State0)
	    end,
    inet:setopts(Socket, [{active, once}]),
    {noreply, State1};

handle_info(_Info, State) ->
    {noreply, State}.

handle_packet(IpKey, Socket0 = #ssl_socket{mode = passive, queue = Queue}, Packet,
	      State0 = #state{ip_conns = IpConns0}) ->
    Socket1 = Socket0#ssl_socket{queue = queue:in(Packet, Queue)},
    IpConns1 = gb_trees:update(IpKey, Socket1, IpConns0),
    State0#state{ip_conns = IpConns1};

handle_packet(_IpKey, #ssl_socket{id = SslSocketId, mode = _Mode, owner = Owner}, Packet, State) ->
    Owner ! {?PROTOCOL, ssl_socket(SslSocketId), Packet},
    State.

handle_accept(IpKey = {Address, Port}, Packet,
	      State0 = #state{socket = Socket,
			      ip_conns = IpConns0, ssl_conns = SslConns0,
			      state = accepting,
			      accepting = Accepting}) ->
    case dtls_connection:handle_packet(Address, Port, Packet) of
	{reply, Data} ->
	    gen_udp:send(Socket, Address, Port, Data),
	    State0;

	accept ->
	    %% NOTE: ClientHello's are decode twice, should this be changed?
	    {Owner, _} = Accepting,
	    SslSocketId = make_ref(),
	    SslSocket = #ssl_socket{id = SslSocketId, owner = Owner,
				    mode = passive, queue = queue:from_list([Packet])},

	    SslConns1 = gb_trees:insert(SslSocketId, IpKey, SslConns0),
	    IpConns1  = gb_trees:insert(IpKey, SslSocket, IpConns0),
	    State = reply_accept({ok, ssl_socket(SslSocketId)}, State0),
	    State#state{ip_conns = IpConns1, ssl_conns = SslConns1};

	_ ->
	    %% silently ignore
	    State0
    end;

handle_accept(_IpKey, _Packet, State) ->
    State.

send(Socket, Address, Port, Data)
  when is_binary(Data) ->
    gen_udp:send(Socket, Address, Port, Data);
send(_Socket, _Address, _Port, []) ->
    ok;
send(Socket, Address, Port, [H|T]) ->
    case gen_udp:send(Socket, Address, Port, H) of
	ok ->
	    send(Socket, Address, Port, T);
	Other ->
	    Other
    end.

reply_accept(Reply, State = #state{state = accepting, accepting = Accepting}) ->
    gen_server:reply(Accepting, Reply),
    State#state{state = listen, accepting = undefined};
reply_accept(_Reply, State) ->
    State.

%% ---------------------------------------------------------------------------
%% Socket Handling functions
%% ---------------------------------------------------------------------------
with_socket(SslSocketId, Args, From, Error, Fun, State = #state{ssl_conns = SslConns}) ->
    case gb_trees:lookup(SslSocketId, SslConns) of
	none ->
	    {reply, Error, State};
	{value, IpKey} ->
	    Fun(IpKey, Args, From, State)
    end.

socket_close(IpKey, SslSocketId, _From, State = #state{ip_conns = IpConns0, ssl_conns = SslConns0}) ->
    IpConns = gb_trees:delete_any(IpKey, IpConns0),
    SslConns = gb_trees:delete_any(SslSocketId, SslConns0),
    {reply, ok, State#state{ip_conns = IpConns, ssl_conns = SslConns}}.

socket_shutdown(_IpKey, _Args, _From, State) ->
    {reply, ok, State}.

%%
%% TODO: timeout handling
%%
socket_recv(IpKey, {_Length = 0, _Timeout}, _From,
	    State = #state{ip_conns = IpConns0}) ->
    ct:pal("socket_recv: ~p~n", [IpKey]),
    SslSocket = gb_trees:get(IpKey, IpConns0),
    IpConns = gb_trees:update(IpKey,
			      SslSocket#ssl_socket{queue = queue:new()},
			      IpConns0),
    Reply = binary:list_to_bin(queue:to_list(SslSocket#ssl_socket.queue)),
    {reply, {ok, Reply}, State#state{ip_conns = IpConns}};

socket_recv(IpKey, {_Length = 0, _Timeout = 0}, _From,
	    State = #state{ip_conns = IpConns0}) ->
    SslSocket = gb_trees:get(IpKey, IpConns0),
    IpConns = gb_trees:update(IpKey,
			      SslSocket#ssl_socket{queue = queue:new()},
			      IpConns0),
    Reply = binary:list_to_bin(queue:to_list(SslSocket#ssl_socket.queue)),
    {reply, {ok, Reply}, State#state{ip_conns = IpConns}}.

socket_send({Address, Port}, Packet, _From,
	    State = #state{socket = Socket}) ->
    Reply = send(Socket, Address, Port, Packet),
    {reply, Reply, State}.


socket_setopts(IpKey, {SslSocketId, Options}, _From,
	       State = #state{ip_conns = IpConns0}) ->
    case proplists:get_value(active, Options) of
	Active when Active /= false ->
	    SslSocket = gb_trees:get(IpKey, IpConns0),
	    #ssl_socket{owner = Owner, queue = Queue} = SslSocket,

	    [Owner ! {?PROTOCOL, ssl_socket(SslSocketId), Packet} || Packet <- queue:to_list(Queue)],
	    IpConns1 = gb_trees:update(IpKey,
				       SslSocket#ssl_socket{mode = active, queue = queue:new()},
				       IpConns0),
	    {reply, ok, State#state{ip_conns = IpConns1}};
	_ ->
	    {reply, ok, State}
    end.

socket_peername(IpKey, _, _From, State) ->
    {reply, {ok, IpKey}, State}.

socket_controlling_process(IpKey, {Old, Pid}, _From,
			   State = #state{ip_conns = IpConns0}) ->
    SslSocket = gb_trees:get(IpKey, IpConns0),
    case SslSocket of
	#ssl_socket{owner = Old} ->
	    IpConns1  = gb_trees:update(IpKey, SslSocket#ssl_socket{owner = Pid}, IpConns0),
	    {reply, ok, State#state{ip_conns = IpConns1}};
	_ ->
	    {reply, {error, not_owner}, State}
    end.
