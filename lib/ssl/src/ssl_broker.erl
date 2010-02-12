%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2010. All Rights Reserved.
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

%%% Purpose : SSL broker

-module(ssl_broker).
-behaviour(gen_server).

%% This module implements brokers for ssl. A broker is either a connector, 
%% an acceptor, or a listener. All brokers are children to ssl_broker_sup,
%% to which they are linked. Each broker is also linked to ssl_server, and
%% to its client.
%%
%% The purpose of the broker is to set up SSL connections through calls to
%% ssl_server and gen_tcp. All control information goes to the server,
%% while all data is exchanged directly between gen_tcp and the port program
%% of the ssl_server.
%%
%% A broker is created by a call to start_broker/3 (do *not* use start_link/4
%% - it is for ssl_broker_sup to call that one), and then call listen/3, 
%% accept/4, or connect/5. 
%%
%% The following table shows all functions dependency on status, active 
%% mode etc.
%%
%% Permitted status transitions: 
%%
%%		nil	->	open 
%%		open	->	closing | closed (termination)
%%		closing	->	closed (termination)
%%
%% We are rather sloppy about nil, and consider open/closing == !closed,
%% open/closing/closed === any  etc.
%%
%%
%%	function/	 valid			mode		new
%%	message		 status					state
%%								
%%	calls							
%%	-----							
%%	recv		 open			passive		ditto
%%	send		 open			any		ditto
%%	transport_accept nil			any		open
%%      ssl_accept       nil                    any             open
%%	connect		 nil			any		open
%%	listen		 nil			any		open
%%	peername	 open/closing		any		ditto
%%	setopts		 open/closing		any		ditto
%%	getopts		 open/closing		any		ditto
%%	sockname	 open/closing		any		ditto
%%	peercert	 open/closing		any		ditto
%%	inhibit		 any			any		ditto
%%	release		 any			any		ditto
%%	close		 any			any		closed (1)
%%
%%	info							
%%	----							
%%	tcp		 open			active		ditto
%%	tcp_closed	 open | closing		active		closing
%%	tcp_error	 open | closing		active		closing
%%
%%	(1) We just terminate.
%%
%% TODO
%%
%% XXX Timeouts are not checked (integer or infinity).
%%
%% XXX The collector thing is not gen_server compliant.
%%
%% NOTE: There are three different "modes": (a) passive or active mode,
%% specified as {active, bool()}, and (b) list or binary mode, specified 
%% as {mode, list | binary}, and (c) encrypted or clear mode
%%

-include("ssl_int.hrl").

%% External exports 

-export([start_broker/1, start_broker/2, start_link/3,
	 transport_accept/3, ssl_accept/2,
	 close/1, connect/5, connection_info/1, controlling_process/2,
	 listen/3, recv/3, send/2, getopts/2, getopts/3, setopts/2,
	 sockname/1, peername/1, peercert/1]).

-export([listen_prim/5, connect_prim/8,
	 transport_accept_prim/5, ssl_accept_prim/6]).

%% Internal exports 

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2, collector_init/1]).

-include("ssl_broker_int.hrl").

%% start_broker(Type) -> {ok, Pid} | {error, Reason}
%% start_broker(Type, GenOpts) -> {ok, Pid} | {error, Reason}
%%            Type = accept | connect | listen
%%            GenOpts = /standard gen_server options/
%%
%% This is the function to be called from the interface module ssl.erl.
%% Links to the caller.
%%
start_broker(Type) ->
    start_broker(Type, []).

start_broker(Type, GenOpts) ->
    case lists:member(Type, [listener, acceptor, connector]) of
	true ->
	    case supervisor:start_child(ssl_broker_sup, 
					[self(), Type, GenOpts]) of
		{ok, Pid} ->
		    link(Pid),
		    {ok, Pid};
		{error, Reason} ->
		    {error, Reason}
	    end;
	false  ->
	    {error, ebrokertype}
    end.

%% start_link(Client, Type, GenOpts) -> {ok, Pid} | {error, Reason}
%%	      
%%	Type = accept | connect | listen
%%	GenOpts = /standard gen_server options/
%%
%% This function is called by ssl_broker_sup and must *not* be called
%% from an interface module (ssl.erl).

start_link(Client, Type, GenOpts) ->
    gen_server:start_link(?MODULE, [Client, Type], GenOpts).


%% accept(Pid, ListenSocket, Timeout) -> {ok, Socket} | {error, Reason}
%%  
%% Types:   Pid = pid() of acceptor
%%          ListenSocket = Socket = sslsocket()
%% 	    Timeout = timeout()
%%
%% accept(Pid, ListenSocket, Timeout) 
%%   when is_pid(Pid), is_record(ListenSocket, sslsocket) ->
%%     Req = {accept, self(), ListenSocket, Timeout},
%%     gen_server:call(Pid, Req, infinity).

%% transport_accept(Pid, ListenSocket, Timeout) -> {ok, Socket} | 
%% 						   {error, Reason}
%%  
%% Types:   Pid = pid() of acceptor
%%          ListenSocket = Socket = sslsocket()
%% 	    Timeout = timeout()
%%
transport_accept(Pid, #sslsocket{} = ListenSocket, Timeout) when is_pid(Pid) ->
     Req = {transport_accept, self(), ListenSocket, Timeout},
     gen_server:call(Pid, Req, infinity).

%% ssl_accept(Pid, Socket, Timeout) -> {ok, Socket} | {error, Reason}
%%
%% Types:   Pid = pid() of acceptor
%%          ListenSocket = Socket = sslsocket()
%% 	    Timeout = timeout()
%%
ssl_accept(#sslsocket{pid = Pid} = Socket, Timeout) ->
    Req = {ssl_accept, self(), Socket, Timeout},
    gen_server:call(Pid, Req, infinity).

%% close(Socket) -> ok | {error, Reason}
%%  
%% Types:   Socket = sslsocket() | pid()
%%
close(#sslsocket{pid = Pid}) ->
    close(Pid);
close(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, {close, self()}, infinity).

%% connect(Pid, Address, Port, Opts, Timeout) -> {ok, Socket} | {error, Reason}
%%  
%% Types:   Pid = pid() of connector
%%          Address  = string() | {byte(), byte(), byte(), byte()}
%%          Port = int()
%%          Opts = options()
%% 	    Timeout = timeout()
%%          Socket = sslsocket()
%%
connect(Pid, Address, Port, Opts, Timeout) when is_pid(Pid), is_list(Opts) ->
    case are_connect_opts(Opts) of
	true ->
	    Req = {connect, self(), Address, Port, Opts, Timeout},
	    gen_server:call(Pid, Req, infinity);
	false  ->
	    {error, eoptions}
    end.

%%
%% connection_info(Socket) -> {ok, {Protocol, Cipher} | {error, Reason}
%%
connection_info(#sslsocket{pid = Pid}) ->
    Req = {connection_info, self()},
    gen_server:call(Pid, Req, infinity).

%% controlling_process(Socket, NewOwner) -> ok | {error, Reason}

controlling_process(#sslsocket{pid = Pid}, NewOwner) when is_pid(NewOwner) ->
    case gen_server:call(Pid, {inhibit_msgs, self()}, infinity) of
	ok ->
	    transfer_messages(Pid, NewOwner),
	    gen_server:call(Pid, {release_msgs, self(), NewOwner}, infinity);
	Error ->
	    Error
    end.

%% listen(Pid, Port, Opts) -> {ok, ListenSocket} | {error, Reason}
%%  
%% Types:   Pid = pid() of listener
%%          Port = int()
%%          Opts = options()
%%          ListenSocket = sslsocket()
%%
listen(Pid, Port, Opts) when is_pid(Pid) ->
    case are_listen_opts(Opts) of
	true ->
	    Req = {listen, self(), Port, Opts}, 
	    gen_server:call(Pid, Req, infinity);
	false  ->
	    {error, eoptions}
    end.


%%
%% peername(Socket) -> {ok, {Address, Port}} | {error, Reason}
%%
peername(#sslsocket{pid = Pid}) ->
    Req = {peername, self()},
    gen_server:call(Pid, Req, infinity).


%% recv(Socket, Length, Timeout) -> {ok, Data} | {error, Reason}
%%
%% Types:   Socket = sslsocket()
%%          Length = Timeout = integer()
%%          Data = bytes() | binary()
%%
recv(#sslsocket{pid = Pid}, Length, Timeout) ->
    Req = {recv, self(), Length, Timeout}, 
    gen_server:call(Pid, Req, infinity).


%% send(Socket, Data) -> ok | {error, Reason}
%%  
%% Types:   Socket = sslsocket()
%%
send(#sslsocket{pid = Pid}, Data) ->
    gen_server:call(Pid, {send, self(), Data}, infinity).


%% getopts(Socket, OptTags) -> {ok, Opts} | {error, einval}
%%  
%% Types:	Pid = pid() of broker
%%  		Timeout = timeout()
%%		OptTags = option_tags()
%%		Opts = options()
%%
getopts(Socket, OptTags) ->
    getopts(Socket, OptTags, infinity).

getopts(#sslsocket{pid = Pid}, OptTags, Timeout) when is_list(OptTags) ->
    Req = {getopts, self(), OptTags}, 
    gen_server:call(Pid, Req, Timeout).


%%
%% setopts(Socket, Opts) -> ok | {error, Reason}
%%
setopts(#sslsocket{pid = Pid}, Opts) ->
    Req = {setopts, self(), Opts},
    gen_server:call(Pid, Req, infinity).

%%
%% sockname(Socket) -> {ok, {Address, Port}} | {error, Reason}
%%
sockname(#sslsocket{pid = Pid}) ->
    Req = {sockname, self()},
    gen_server:call(Pid, Req, infinity).


%%
%% peercert(Socket) -> {ok, Cert} | {error, Reason}
%%
peercert(#sslsocket{pid = Pid}) ->
    Req = {peercert, self()},
    gen_server:call(Pid, Req, infinity).

%%
%%  INIT
%%

%% init
%%
init([Client, Type]) ->
    process_flag(trap_exit, true),
    link(Client),
    Debug = case application:get_env(ssl, edebug) of
		{ok, true} -> 
		    true;
		_ ->
		    case application:get_env(ssl, debug) of
			{ok, true} ->
			    true;
			_  ->
			    os:getenv("ERL_SSL_DEBUG") =/= false
		    end
	    end,
    Server = whereis(ssl_server),
    if 
	is_pid(Server) ->
	    link(Server),
	    debug1(Debug, Type, "in start, client = ~w", [Client]),
	    {ok, #st{brokertype = Type, server = Server, client = Client,
		     collector = Client, debug = Debug}};
	true  ->
	    {stop, no_ssl_server}
    end.


%%
%% HANDLE CALL
%%

%% recv - passive mode
%%
handle_call({recv, Client, Length, Timeout}, _From,
	    #st{active = false, proxysock = Proxysock, status = Status} = St) ->
    debug(St, "recv: client = ~w~n", [Client]),
    if 
	Status =/= open ->
 	    {reply, {error, closed}, St};
	true ->
	    case gen_tcp:recv(Proxysock, Length, Timeout) of
		{ok, Data} ->
		    {reply, {ok, Data}, St};
		{error, timeout} ->
		    {reply, {error, timeout}, St};
		{error, Reason} ->
		    {reply, {error, Reason}, St#st{status = closing}}
	    end
    end;

%% send
%% 
handle_call({send, Client, Data}, _From, St) ->
    debug(St, "send: client = ~w~n", [Client]),
    if 
	St#st.status =/= open ->
 	    {reply, {error, closed}, St};
	true ->
	    case gen_tcp:send(St#st.proxysock, Data) of
		ok ->
		    {reply, ok, St};
		{error, _Reason} ->
		    {reply, {error, closed}, St#st{status = closing}}
	    end
    end;

%% transport_accept 
%% 
%% Client = pid of client 
%% ListenSocket = sslsocket()
%%
handle_call({transport_accept, Client, ListenSocket, Timeout}, _From, St) ->
    debug(St, "transport_accept: client = ~w, listensocket = ~w~n", 
	  [Client, ListenSocket]),
    case getopts(ListenSocket, tcp_listen_opt_tags(), ?DEF_TIMEOUT) of 
	{ok, LOpts} ->
	    case transport_accept_prim(
		   ssl_server, ListenSocket#sslsocket.fd, LOpts, Timeout, St) of
		{ok, ThisSocket, NSt} ->
		    {reply, {ok, ThisSocket}, NSt};
		{error, Reason, St} ->
		    What = what(Reason),
		    {stop, normal, {error, What}, St}
	    end;
	{error, Reason} ->
	    What = what(Reason),
	    {stop, normal, {error, What}, St}
    end;

%% ssl_accept 
%% 
%% Client = pid of client 
%% ListenSocket = sslsocket()
%%
handle_call({ssl_accept, Client, Socket, Timeout}, _From, St) ->
    debug(St, "ssl_accept: client = ~w, socket = ~w~n", [Client, Socket]),
    case ssl_accept_prim(ssl_server, gen_tcp, Client, St#st.opts, Timeout, St#st{thissock=Socket}) of
	{ok, Socket, NSt} ->
	    {reply, ok, NSt};
	{error, Reason, St} ->
	    What = what(Reason),
	    {stop, normal, {error, What}, St}
    end;

%% connect
%%
%% Client = client pid
%% Address = hostname | ipstring | IP
%% Port = integer()
%% Opts = options()
%%
handle_call({connect, Client, Address, Port, Opts, Timeout}, _From, St) ->
    debug(St, "connect: client = ~w, address = ~p, port = ~w~n",
	  [Client, Address, Port]),
    case connect_prim(ssl_server, gen_tcp, Client, Address, Port, Opts, 
		      Timeout, St) of
	{ok, Res, NSt} ->
	    {reply, {ok, Res}, NSt};
	{error, Reason, NSt} ->
	    What = what(Reason),
	    {stop, normal, {error, What}, NSt}
    end;

%% connection_info
%%
handle_call({connection_info, Client}, _From, St) ->
    debug(St, "connection_info: client = ~w~n", [Client]),
    Reply = ssl_server:connection_info(St#st.fd),
    {reply, Reply, St};

%% close from client
%%
handle_call({close, Client}, _From, St) ->
    debug(St, "close: client = ~w~n", [Client]),
    %% Terminate
    {stop, normal, ok, St#st{status = closed}};

%% listen
%% 
%% Client = pid of client
%% Port = int()
%% Opts = options()
%%
handle_call({listen, Client, Port, Opts}, _From, St) ->
    debug(St, "listen: client = ~w, port = ~w~n",
	  [Client, Port]),
    case listen_prim(ssl_server, Client, Port, Opts, St) of
	{ok, Res, NSt} ->
	    {reply, {ok, Res}, NSt};
	{error, Reason, NSt} ->
	    What = what(Reason),
	    {stop, normal, {error, What}, NSt}
    end;

%% peername
%%
handle_call({peername, Client}, _From, St) ->
    debug(St, "peername: client = ~w~n", [Client]),
    Reply = case ssl_server:peername(St#st.fd) of
		{ok, {Address, Port}} ->
		    {ok, At} = inet_parse:ipv4_address(Address),
		    {ok, {At, Port}};
		Error ->
		    Error
	    end,
    {reply, Reply, St};

%% setopts
%%
handle_call({setopts, Client, Opts0}, _From, St0) ->
    debug(St0, "setopts: client = ~w~n", [Client]),
    OptsOK = case St0#st.brokertype of
		 listener ->
		     are_opts(fun is_tcp_listen_opt/1, Opts0);
		 acceptor ->
		     are_opts(fun is_tcp_accept_opt/1, Opts0);
		 connector ->
		     are_opts(fun is_tcp_connect_opt/1, Opts0)
	     end,
    if 
	OptsOK =:= false ->
	    {reply, {error, eoptions}, St0};
	true ->
	    Opts1 = lists:keydelete(nodelay, 1, Opts0),
	    case inet:setopts(St0#st.proxysock, Opts1) of
		ok ->
		    Opts2 = replace_opts(Opts1, St0#st.opts),
		    Active = get_active(Opts2),
		    St2 = St0#st{opts = Opts2, 
				 active = Active},
		    case get_nodelay(Opts0) of
			empty ->
			    {reply, ok, St2};
			Bool ->
			    case setnodelay(ssl_server, St0, Bool) of
				ok ->
				    Opts3 = replace_opts([{nodelay, Bool}],
							 Opts2),
				    St3 = St0#st{opts = Opts3, 
						 active = Active},
				    {reply, ok, St3};
				{error, Reason} ->
				    {reply, {error, Reason}, St2}
			    end
		    end;
		{error, Reason} ->
		    {reply, {error, Reason}, St0}
	    end
    end;

%% sockname
%%
handle_call({sockname, Client}, _From, St) ->
    debug(St, "sockname: client = ~w~n", [Client]),
    Reply = case ssl_server:sockname(St#st.fd) of
		{ok, {Address, Port}} ->
		    {ok, At} = inet_parse:ipv4_address(Address),
		    {ok, {At, Port}};
		Error ->
		    Error
	    end,
    {reply, Reply, St};

%% peercert
%%
handle_call({peercert, Client}, _From, St) ->
    debug(St, "peercert: client = ~w~n", [Client]),
    Reply = ssl_server:peercert(St#st.fd),
    {reply, Reply, St};

%% inhibit msgs
%%
handle_call({inhibit_msgs, Client}, _From, #st{client = Client} = St) ->
    debug(St, "inhibit_msgs: client = ~w~n", [Client]),
    {ok, Collector} = start_collector(),
    {reply, ok, St#st{collector = Collector}};

%% release msgs
%%
handle_call({release_msgs, Client, NewClient}, _From,
	    #st{client = Client, collector = Collector} = St) ->
    debug(St, "release_msgs: client = ~w~n", [Client]),
    unlink(Client),
    link(NewClient),
    release_collector(Collector, NewClient),
    NSt = St#st{client = NewClient, collector = NewClient},
    {reply, ok, NSt};

%% getopts
%%
handle_call({getopts, Client, OptTags}, _From, St) ->
    debug(St, "getopts: client = ~w~n", [Client]),
    Reply = case are_opt_tags(St#st.brokertype, OptTags) of
		true ->
		    {ok, extract_opts(OptTags, St#st.opts)};
		_ ->
		    {error, einval}
	    end,
    {reply, Reply, St};

%% bad call
%%
handle_call(Request, _From, St) ->
    debug(St, "++++ ssl_broker: bad call: ~w~n", [Request]),
    {reply, {error, {badcall, Request}}, St}.

%%
%% HANDLE CAST
%%

handle_cast(Request, St) ->
    debug(St, "++++ ssl_broker: bad cast: ~w~n", [Request]),
    {stop, {error, {badcast, Request}}, St}.

%% 
%% HANDLE INFO
%%

%% tcp - active mode
%%
%% The collector is different from client only during change of
%% controlling process.
%%
handle_info({tcp, Socket, Data},
	    #st{active = Active, collector = Collector, status = open,
		proxysock = Socket, thissock = Thissock} = St) 
  when Active =/= false ->
    debug(St, "tcp: socket = ~w~n", [Socket]),
    Msg = {ssl, Thissock, Data},
    Collector ! Msg,
    if
	Active =:= once -> 
	    {noreply, St#st{active = false}};
	true -> 
	    {noreply, St}
    end;

%% tcp_closed - from proxy socket, active mode
%%
%%
handle_info({tcp_closed, Socket},
	    #st{active = Active, collector = Collector,
		proxysock = Socket, thissock = Thissock} = St) 
  when Active =/= false ->
    debug(St, "tcp_closed: socket = ~w~n", [Socket]),
    Msg = {ssl_closed, Thissock},
    Collector ! Msg,
    if
	Active =:= once -> 
	    {noreply, St#st{status = closing, active = false}};
	true ->
	    {noreply, St#st{status = closing}}
    end;

%% tcp_error - from proxy socket, active mode
%%
%%
handle_info({tcp_error, Socket, Reason},
	    #st{active = Active, collector = Collector,
		proxysock = Socket} = St) 
  when Active =/= false ->
    debug(St, "tcp_error: socket = ~w, reason = ~w~n", [Socket, Reason]),
    Msg = {ssl_error, Socket, Reason},
    Collector ! Msg,
    if
	Active =:= once -> 
	    {noreply, St#st{status = closing, active = false}};
	true ->
	    {noreply, St#st{status = closing}}
    end;

%% EXIT - from client
%% 
%%
handle_info({'EXIT', Client, Reason}, #st{client = Client} = St) ->
    debug(St, "exit client: client = ~w, reason = ~w~n", [Client, Reason]),
    {stop, normal, St#st{status = closed}};	% do not make noise

%% EXIT - from server
%%
%%
handle_info({'EXIT', Server, Reason}, #st{server = Server} = St) ->
    debug(St, "exit server: reason = ~w~n", [Reason]),
    {stop, Reason, St};

%% handle info catch all
%%
handle_info(Info, St) ->
    debug(St, " bad info: ~w~n", [Info]),
    {stop, {error, {badinfo, Info}}, St}.


%% terminate
%%
%% 
terminate(Reason, St) ->
    debug(St, "in terminate reason: ~w, state: ~w~n", [Reason, St]),
    ok.

%% code_change
%%
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Primitive interface
%%
listen_prim(ServerName, Client, Port, Opts, St) ->
    LOpts = get_tcp_listen_opts(Opts),
    SSLOpts = get_ssl_opts(Opts),
    FlagStr =mk_ssl_optstr(SSLOpts),
    BackLog = get_backlog(LOpts),
    IP = get_ip(LOpts),
    case ssl_server:listen_prim(ServerName, IP, Port, FlagStr, BackLog) of
	{ok, ListenFd, _Port0} ->
	    ThisSocket = #sslsocket{fd = ListenFd, pid = self()},
	    StOpts = add_default_tcp_listen_opts(LOpts) ++
		add_default_ssl_opts(SSLOpts),
	    NSt = St#st{fd = ListenFd, 
			active = get_active(LOpts), % irrelevant for listen
			opts = StOpts,
			thissock = ThisSocket, 
			status = open},
	    debug(St, "listen: ok: client = ~w, listenfd = ~w~n", 
		  [Client, ListenFd]),
	    {ok, ThisSocket, NSt};
	{error, Reason} ->
	    {error, Reason, St}
    end.

connect_prim(ServerName, TcpModule, Client, FAddress, FPort, Opts, 
	     Timeout, St) ->
    COpts = get_tcp_connect_opts(Opts),
    SSLOpts = get_ssl_opts(Opts),
    FlagStr = mk_ssl_optstr(SSLOpts),
    case inet:getaddr(FAddress, inet) of
	{ok, FIP} ->
	    %% Timeout is gen_server timeout - hence catch
	    LIP = get_ip(COpts),
	    LPort = get_port(COpts),
	    case (catch ssl_server:connect_prim(ServerName, 
						LIP, LPort, FIP, FPort, 
						FlagStr, Timeout)) of
		{ok, Fd, ProxyPort} ->
		    case connect_proxy(ServerName, TcpModule, Fd, 
				       ProxyPort, COpts, Timeout) of
			{ok, Socket} ->
			    ThisSocket = #sslsocket{fd = Fd, pid = self()}, 
			    StOpts = add_default_tcp_connect_opts(COpts) ++
				add_default_ssl_opts(SSLOpts),
			    NSt = St#st{fd = Fd, 
					active = get_active(COpts),
					opts = StOpts,
					thissock = ThisSocket, 
					proxysock = Socket, 
					status = open},
			    case get_nodelay(COpts) of
				true -> setnodelay(ServerName, NSt, true);
				_ -> ok
			    end,
			    debug(St, "connect: ok: client = ~w, fd = ~w~n",
				  [Client, Fd]),
			    {ok, ThisSocket, NSt};
			{error, Reason} ->
			    {error, Reason, St}
		    end;
		{'EXIT', Reason} ->
		    {error, Reason, St};
		{error, Reason} ->
		    {error, Reason, St}
	    end;
	{error, Reason} ->
	    {error, Reason, St}
    end.

transport_accept_prim(ServerName, ListenFd, LOpts, Timeout, St) -> 
    AOpts = get_tcp_accept_opts(LOpts),
    FlagStr = "",
    %% Timeout is gen_server timeout - hence catch.
    case (catch ssl_server:transport_accept_prim(ServerName, ListenFd,
						 FlagStr, Timeout)) of 
	{ok, Fd, ProxyPort} ->
	    ThisSocket = #sslsocket{fd = Fd, pid = self()}, 
	    NSt = St#st{fd = Fd, 
			active = get_active(AOpts),
			opts = AOpts,
			thissock = ThisSocket,
			proxyport = ProxyPort,
			encrypted = false},
	    debug(St, "transport_accept: ok: fd = ~w~n", [Fd]),
	    {ok, ThisSocket, NSt};
	{'EXIT', Reason} ->
	    debug(St, "transport_accept: EXIT: Reason = ~w~n", [Reason]),
	    {error, Reason, St};
	{error, Reason} ->
	    debug(St, "transport_accept: error: Reason = ~w~n", [Reason]),
	    {error, Reason, St}
    end.

ssl_accept_prim(ServerName, TcpModule, Client, LOpts, Timeout, St) -> 
    FlagStr = [],
    SSLOpts = [],
    AOpts = get_tcp_accept_opts(LOpts),
    %% Timeout is gen_server timeout - hence catch.
    debug(St, "ssl_accept_prim: self() ~w Client ~w~n", [self(), Client]),
    Socket = St#st.thissock,
    Fd = Socket#sslsocket.fd,
    A = (catch ssl_server:ssl_accept_prim(ServerName, Fd, FlagStr, Timeout)),
    debug(St, "ssl_accept_prim: ~w~n", [A]),
    case A of 
	ok ->
	    B = connect_proxy(ServerName, TcpModule, Fd, 
			       St#st.proxyport, AOpts, Timeout),
	    debug(St, "ssl_accept_prim: connect_proxy ~w~n", [B]),
	    case B of
		{ok, Socket2} ->
		    StOpts = add_default_tcp_accept_opts(AOpts) ++
			add_default_ssl_opts(SSLOpts),
		    NSt = St#st{opts = StOpts,
				proxysock = Socket2,
				encrypted = true,
				status = open},
		    case get_nodelay(AOpts) of
			true -> setnodelay(ServerName, NSt, true);
			_ -> ok
		    end,
		    debug(St, "transport_accept: ok: client = ~w, fd = ~w~n",
			  [Client, Fd]),
		    {ok, St#st.thissock, NSt};
		{error, Reason} ->
		    {error, Reason, St}
	    end;
	{'EXIT', Reason} ->
	    {error, Reason, St};
	{error, Reason} ->
	    {error, Reason, St}
    end.


%%
%% LOCAL FUNCTIONS
%%

%% 
%% connect_proxy(Fd, ProxyPort, TOpts, Timeout) -> {ok, Socket} | 
%%						   {error, Reason}
%%
connect_proxy(ServerName, TcpModule, Fd, ProxyPort, TOpts, Timeout) ->
    case TcpModule:connect({127, 0, 0, 1}, ProxyPort, TOpts, Timeout) of
	{ok, Socket} ->
	    {ok, Port} = inet:port(Socket),
	    A = ssl_server:proxy_join_prim(ServerName, Fd, Port),
	    case A of
		ok ->
		    {ok, Socket};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.


setnodelay(ServerName, St, Bool) ->
    case ssl_server:setnodelay_prim(ServerName, St#st.fd, Bool) of
	ok ->
	    case inet:setopts(St#st.proxysock, [{nodelay, Bool}]) of
		ok ->
		    ok;
		{error, Reason} ->
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%
%% start_collector()
%%
%% A collector is a little process that keeps messages during change of
%% controlling process. 
%% XXX This is not gen_server compliant :-(.
%%
start_collector() ->
    Pid = spawn_link(?MODULE, collector_init, [self()]),
    {ok, Pid}.

%%
%% release_collector(Collector, NewOwner)
%%
release_collector(Collector, NewOwner) ->
    Collector ! {release, self(), NewOwner},
    receive
	%% Reap collector
	{'EXIT', Collector, normal} ->
	    ok
    end.

%%
%% collector_init(Broker) -> void()
%%
collector_init(Broker) ->
    receive
	{release, Broker, NewOwner} ->
	    transfer_messages(Broker, NewOwner)
    end.

%% 
%% transfer_messages(Pid, NewOwner) -> void()
%%
transfer_messages(Pid, NewOwner) ->    
    receive
	{ssl, Sock, Data} ->
	    NewOwner ! {ssl, Sock, Data},
	    transfer_messages(Pid, NewOwner);
	{ssl_closed, Sock} ->
	    NewOwner ! {ssl_closed, Sock},
	    transfer_messages(Pid, NewOwner);
	{ssl_error, Sock, Reason} ->
	    NewOwner ! {ssl_error, Sock, Reason},
	    transfer_messages(Pid, NewOwner)
    after 0 ->
	    ok
    end.

%%
%% debug(St, Format, Args) -> void() - printouts
%%
debug(St, Format, Args) ->
    debug1(St#st.debug, St#st.brokertype, Format, Args).

debug1(true, Type, Format0, Args) ->
    {_MS, S, MiS} = erlang:now(),
    Secs = S rem 100, 
    MiSecs = MiS div 1000,
    Format = "++++ ~3..0w:~3..0w ssl_broker (~w)[~w]: " ++ Format0, 
    io:format(Format, [Secs, MiSecs, self(), Type| Args]);
debug1(_, _, _, _) ->
    ok.

%%
%% what(Reason) -> What
%% 
what(Reason) when is_atom(Reason) ->
    Reason;
what({'EXIT', Reason}) ->
    what(Reason);
what({What, _Where}) when is_atom(What) ->
    What;
what(Reason) ->
    Reason.


%%
%% OPTIONS
%%
%% Note that `accept' has no options when invoked, but get all its options
%% by inheritance from `listen'. 
%%

are_opt_tags(listener, OptTags) ->
    is_subset(OptTags, listen_opt_tags());
are_opt_tags(acceptor, OptTags) ->
    is_subset(OptTags, accept_opt_tags());
are_opt_tags(connector, OptTags) ->
    is_subset(OptTags, connect_opt_tags()).

listen_opt_tags() ->				
    tcp_listen_opt_tags() ++ ssl_opt_tags().

accept_opt_tags() ->
    tcp_gen_opt_tags().

connect_opt_tags() ->
    tcp_gen_opt_tags() ++ ssl_opt_tags().

tcp_listen_opt_tags() ->				
    tcp_gen_opt_tags() ++ tcp_listen_only_opt_tags().

tcp_gen_opt_tags() ->
    %% All except `reuseaddr' and `deliver'.	
    [nodelay, active, packet, mode, header].	

tcp_listen_only_opt_tags() ->
    [ip, backlog].

ssl_opt_tags() ->
    %% XXX Should remove cachetimeout.
    [verify, depth, certfile, password, cacertfile, ciphers, cachetimeout].

%% Options

%%
%% are_*_opts(Opts) -> boolean()
%%
are_connect_opts(Opts) ->
    are_opts(fun is_connect_opt/1, Opts).

are_listen_opts(Opts) ->
    are_opts(fun is_listen_opt/1, Opts).

are_opts(F, Opts) ->
    lists:all(F, transform_opts(Opts)).

%%
%% get_*_opts(Opts) -> Value
%%
get_tcp_accept_opts(Opts) ->
    [O || O <- transform_opts(Opts), is_tcp_accept_opt(O)].

get_tcp_connect_opts(Opts) ->
    [O || O <- transform_opts(Opts), is_tcp_connect_opt(O)].

get_tcp_listen_opts(Opts) ->
    [O || O <- transform_opts(Opts), is_tcp_listen_opt(O)].

get_ssl_opts(Opts) ->
    [O || O <- transform_opts(Opts), is_ssl_opt(O)].

get_active(Opts) ->
    get_tagged_opt(active, Opts, true).

get_backlog(Opts) ->
    get_tagged_opt(backlog, Opts, ?DEF_BACKLOG).

get_ip(Opts) ->
    get_tagged_opt(ip, Opts, {0, 0, 0, 0}).

get_port(Opts) ->
    get_tagged_opt(port, Opts, 0).

get_nodelay(Opts) ->
    get_tagged_opt(nodelay, Opts, empty).

%%
%% add_default_*_opts(Opts) -> NOpts
%%

add_default_tcp_accept_opts(Opts) ->
    add_default_opts(Opts, default_tcp_accept_opts()).

add_default_tcp_connect_opts(Opts) ->
    add_default_opts(Opts, default_tcp_connect_opts()).

add_default_tcp_listen_opts(Opts) ->
    add_default_opts(Opts, default_tcp_listen_opts()).

add_default_ssl_opts(Opts) ->
    add_default_opts(Opts, default_ssl_opts()).

add_default_opts(Opts, DefOpts) ->
    TOpts = transform_opts(Opts),
    TOpts ++ [DP || {DTag, _DVal} = DP <- DefOpts,
		    not lists:keymember(DTag, 1, TOpts)].

default_tcp_accept_opts() ->
    [O || O <- default_opts(), is_tcp_accept_opt(O)].

default_tcp_connect_opts() ->
    [O || O <- default_opts(), is_tcp_connect_opt(O)].

default_tcp_listen_opts() ->
    [O || O <- default_opts(), is_tcp_listen_opt(O)].

default_ssl_opts() ->
    [O || O <- default_opts(), is_ssl_opt(O)].

default_opts() ->
    [{mode, list}, {packet, 0}, {nodelay, false}, {active, true},
     {backlog, ?DEF_BACKLOG}, {ip, {0, 0, 0, 0}},
     {verify, 0}, {depth, 1}].


%% Transform from old to new options, and also from old gen_tcp
%% options to new ones. All returned options are tagged options.
%%
transform_opts(Opts) ->
    lists:flatmap(fun transform_opt/1, Opts).

transform_opt(binary) -> 	[{mode, binary}];
transform_opt(list) -> 		[{mode, list}];
transform_opt({packet, raw}) ->	[{packet, 0}];
transform_opt(raw) -> 		[];
transform_opt(Opt) -> 		[Opt].

%% NOTE: The is_*_opt/1 functions must be applied on transformed options
%% only.

is_connect_opt(Opt) ->
    is_tcp_connect_opt(Opt) or is_ssl_opt(Opt).

is_listen_opt(Opt) ->
    is_tcp_listen_opt(Opt) or is_ssl_opt(Opt).

is_tcp_accept_opt(Opt) ->
    is_tcp_gen_opt(Opt).

is_tcp_connect_opt(Opt) ->
    is_tcp_gen_opt(Opt) or is_tcp_connect_only_opt(Opt).

is_tcp_listen_opt(Opt) ->
    is_tcp_gen_opt(Opt) or is_tcp_listen_only_opt(Opt).

%% General options supported by gen_tcp: All except `reuseaddr' and
%% `deliver'.
is_tcp_gen_opt({mode, list}) -> true;
is_tcp_gen_opt({mode, binary}) -> true;
is_tcp_gen_opt({header, Sz}) when is_integer(Sz), 0 =< Sz -> true; 
is_tcp_gen_opt({packet, Sz}) when is_integer(Sz), 0 =< Sz, Sz =< 4-> true;
is_tcp_gen_opt({packet, sunrm}) -> true;
is_tcp_gen_opt({packet, asn1}) -> true;
is_tcp_gen_opt({packet, cdr}) -> true;
is_tcp_gen_opt({packet, fcgi}) -> true;
is_tcp_gen_opt({packet, line}) -> true;
is_tcp_gen_opt({packet, tpkt}) -> true;
is_tcp_gen_opt({packet, http}) -> true;
is_tcp_gen_opt({packet, httph}) -> true;
is_tcp_gen_opt({nodelay, true}) -> true;
is_tcp_gen_opt({nodelay, false}) -> true;
is_tcp_gen_opt({active, true}) -> true;
is_tcp_gen_opt({active, false}) -> true;
is_tcp_gen_opt({active, once}) -> true;
is_tcp_gen_opt({keepalive, true}) -> true;
is_tcp_gen_opt({keepalive, false}) -> true;
is_tcp_gen_opt({ip, Addr}) -> is_ip_address(Addr);
is_tcp_gen_opt(_Opt) -> false.

is_tcp_listen_only_opt({backlog, Size}) when is_integer(Size), 0 =< Size -> 
    true;
is_tcp_listen_only_opt({reuseaddr, Bool}) when is_boolean(Bool) ->
    true;
is_tcp_listen_only_opt(_Opt) -> false.

is_tcp_connect_only_opt({port, Port}) when is_integer(Port), 0 =< Port -> true;
is_tcp_connect_only_opt(_Opt) -> false.

%% SSL options

is_ssl_opt({verify, Code}) when 0 =< Code, Code =< 2 -> true;
is_ssl_opt({depth, Depth}) when 0 =< Depth -> true;
is_ssl_opt({certfile, String}) -> is_string(String);
is_ssl_opt({keyfile, String}) -> is_string(String);
is_ssl_opt({password, String}) -> is_string(String);
is_ssl_opt({cacertfile, String}) -> is_string(String);
is_ssl_opt({ciphers, String}) -> is_string(String);
is_ssl_opt({cachetimeout, Timeout}) when Timeout >= 0 -> true;
is_ssl_opt(_Opt) -> false.

%% Various types
is_string(String) when is_list(String) ->
    lists:all(fun (C) when is_integer(C), 0 =< C, C =< 255 -> true; 
		  (_C) -> false end, 
	      String);
is_string(_) ->
    false.

is_ip_address(Addr) when tuple_size(Addr) =:= 4 ->
    is_string(tuple_to_list(Addr));
is_ip_address(Addr) when is_list(Addr) ->
    is_string(Addr);
is_ip_address(_) ->
    false.

get_tagged_opt(Tag, Opts, Default) ->
    case lists:keysearch(Tag, 1, Opts) of
	{value, {_, Value}} ->
	    Value;
	_Other ->
	    Default
    end.

%%
%%  mk_ssl_optstr(Opts) -> string()
%%
%%  Makes a "command line" string of SSL options
%%
mk_ssl_optstr(Opts) ->
    lists:flatten([mk_one_ssl_optstr(O) || O <- Opts]).

mk_one_ssl_optstr({verify, Code}) ->
    [" -verify ", integer_to_list(Code)];
mk_one_ssl_optstr({depth, Depth}) ->
    [" -depth ", integer_to_list(Depth)];
mk_one_ssl_optstr({certfile, String}) -> 
    [" -certfile ", String];
mk_one_ssl_optstr({keyfile, String}) -> 
    [" -keyfile ", String];
mk_one_ssl_optstr({password, String}) -> 
    [" -password ", String];
mk_one_ssl_optstr({cacertfile, String}) ->
    [" -cacertfile ", String];
mk_one_ssl_optstr({ciphers, String}) -> 
    [" -ciphers ", String];
mk_one_ssl_optstr({cachetimeout, Timeout}) ->
    [" -cachetimeout ", integer_to_list(Timeout)];
mk_one_ssl_optstr(_) ->
    "".

extract_opts(OptTags, Opts) ->
    [O || O = {Tag,_} <- Opts, lists:member(Tag, OptTags)].

replace_opts(NOpts, Opts) ->
    lists:foldl(fun({Key, Val}, Acc) -> 
			lists:keyreplace(Key, 1, Acc, {Key, Val});
		   %% XXX Check. Patch from Chandrashekhar Mullaparthi.
		   (binary, Acc) ->
			lists:keyreplace(mode, 1, Acc, {mode, binary})
		end,
		Opts, NOpts).

%% Misc

is_subset(A, B) ->
    [] =:= A -- B.
