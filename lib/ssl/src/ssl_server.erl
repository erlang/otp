%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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

%%% Purpose : SSL server

%%
%% TODO
%%
%% XXX The ip option in listen is not general enough. It is assumed
%%     to be a tuple, which is not always the case.

-module(ssl_server).
-behaviour(gen_server).

%% External exports 
-export([start_link/0]).  

-export([transport_accept/2, transport_accept/3, ssl_accept/2, ssl_accept/3,
	 ciphers/0, connect/5, connect/6,
	 connection_info/1, close/1, listen/3, listen/4, peercert/1,
	 peername/1, proxy_join/2, seed/1, setnodelay/2, sockname/1,
	 version/0]).

-export([start_link_prim/0]).
-export([ssl_accept_prim/4, transport_accept_prim/4,
	 connect_prim/7, close_prim/2, 
	 listen_prim/5, proxy_join_prim/3, peername_prim/2, setnodelay_prim/3, 
	 sockname_prim/2]).

-export([dump/0, dump/1]).
-export([enable_debug/0, disable_debug/0, set_debug/1]).
-export([enable_debugmsg/0, disable_debugmsg/0, set_debugmsg/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 code_change/3, terminate/2]).

-include("ssl_int.hrl").

-record(st, {
	  port = [],			% port() of port program
	  progpid = [],			% OS pid of port program
	  debug = false,		% debug printout flag
	  cons = [], 			% All brokers except pending accepts
	  paccepts = [], 		% Pending accept brokers
	  proxylsport = [], 		% proxy listen socket port
	  intref = 0,			% internal reference counter
	  compvsn = "",			% ssl compile library version
	  libvsn = "",			% ssl library version
	  ciphers = []			% available ciphers
	 }).


%% In all functions below IP is a four tuple, e.g. {192, 236, 52, 7}. 
%% Port, Fd and ListenFd are integers; Flags is a string of characters.
%%
%% The prefixes F and L mean foreign and local, respectively. 
%% Example: FIP (IP address for foreign end).

%%
%% start_link() -> {ok, Pid} | {error, Reason}
%%
start_link() ->
    gen_server:start_link({local, ssl_server}, ssl_server, [], []).

start_link_prim() ->
    gen_server:start_link({local, ssl_server_prim}, ssl_server, [], []).

%%
%% transport_accept(ListenFd, Flags) -> {ok, Fd, ProxyLLPort} |
%%			      {error, Reason}
%%
transport_accept(ListenFd, Flags) ->
    transport_accept(ListenFd, Flags, infinity).
transport_accept(ListenFd, Flags, Timeout) ->
    transport_accept_prim(ssl_server,ListenFd, Flags, Timeout).

transport_accept_prim(ServerName, ListenFd, Flags, Timeout) ->
    Req = {transport_accept, self(), ListenFd, Flags}, 
    gen_server:call(ServerName, Req, Timeout).

%%
%% ssl_accept(ListenFd, Flags) -> {ok, Fd, ProxyLLPort} |
%%			      {error, Reason}
%%
ssl_accept(ListenFd, Flags) ->
    ssl_accept(ListenFd, Flags, infinity).
ssl_accept(ListenFd, Flags, Timeout) ->
    ssl_accept_prim(ssl_server, ListenFd, Flags, Timeout).

ssl_accept_prim(ServerName, Fd, Flags, Timeout) ->
    Req = {ssl_accept, Fd, Flags}, 
    gen_server:call(ServerName, Req, Timeout).

%%
%% ciphers() -> {ok, Ciphers} 
%%
ciphers() ->
    gen_server:call(ssl_server, ciphers, infinity).

%%
%% close(Fd) -> ok
%%
close(Fd) -> 
    close_prim(ssl_server, Fd).
close_prim(ServerName, Fd) -> 
    gen_server:call(ServerName, {close, self(), Fd}, infinity),
    ok.

%%
%% connect(LIP, LPort, FIP, FPort, Flags) -> {ok, Fd, ProxyLFPort} |
%%					 {error, Reason}
%%
connect(LIP, LPort, FIP, FPort, Flags) ->
    connect(LIP, LPort, FIP, FPort, Flags, infinity).
connect(LIP, LPort, FIP, FPort, Flags, Timeout) ->
    connect_prim(ssl_server, LIP, LPort, FIP, FPort, Flags, Timeout).

connect_prim(ServerName, LIP, LPort, FIP, FPort, Flags, Timeout) ->
    Req = {connect, self(), LIP, LPort, FIP, FPort, Flags},
    gen_server:call(ServerName, Req, Timeout).

%%
%% connection_info(Fd) -> {ok, {Protocol, Cipher}} | {error, Reason}
%%
connection_info(Fd) ->
    Req = {connection_info, self(), Fd},
    gen_server:call(ssl_server, Req, infinity).
  
%%
%% listen(IP, LPort, Flags), 
%% listen(IP, LPort, Flags, BackLog) -> {ok, ListenFd, LPort0} | 
%%                                    {error, Reason}
%%
listen(IP, LPort, Flags) ->
    listen(IP, LPort, Flags, ?DEF_BACKLOG).
listen(IP, LPort, Flags, BackLog) ->
    listen_prim(ssl_server, IP, LPort, Flags, BackLog).
listen_prim(ServerName, IP, LPort, Flags, BackLog) ->
    Req = {listen, self(), IP, LPort, Flags, BackLog},
    gen_server:call(ServerName, Req, infinity).

%%
%% peercert(Fd) -> {ok, Cert} | {error, Reason}
%%
peercert(Fd) ->
    Req = {peercert, self(), Fd},
    gen_server:call(ssl_server, Req, infinity).

%%
%% peername(Fd) -> {ok, {Address, Port}} | {error, Reason}
%%
peername(Fd) ->
    peername_prim(ssl_server, Fd).
peername_prim(ServerName, Fd) ->
    Req = {peername, self(), Fd},
    gen_server:call(ServerName, Req, infinity).

%%
%% proxy_join(Fd, LPort) -> ok | {error, Reason}
%%
proxy_join(Fd, LPort) ->
    proxy_join_prim(ssl_server, Fd, LPort).
proxy_join_prim(ServerName, Fd, LPort) ->
    Req = {proxy_join, self(), Fd, LPort},
    gen_server:call(ServerName, Req, infinity).

%%
%%  seed(Data)
%%
seed(Data) ->
    Req = {seed, Data},
    gen_server:call(ssl_server, Req, infinity).
    
%%
%%  set_nodelay(Fd, Boolean)
%%
setnodelay(Fd, Boolean) ->
    setnodelay_prim(ssl_server, Fd, Boolean).
setnodelay_prim(ServerName, Fd, Boolean) ->
    Req = {setnodelay, self(), Fd, Boolean},
    gen_server:call(ServerName, Req, infinity).
    
%%
%% sockname(Fd) -> {ok, {Address, Port}} | {error, Reason}
%%
sockname(Fd) ->
    sockname_prim(ssl_server, Fd).
sockname_prim(ServerName, Fd) ->
    Req = {sockname, self(), Fd},
    gen_server:call(ServerName, Req, infinity).

%%
%% version() -> {ok, {CompVsn, LibVsn}} 
%%
version() ->
    gen_server:call(ssl_server, version, infinity).


enable_debug() ->
    set_debug(true).

disable_debug() ->
    set_debug(false).

set_debug(Bool) ->
    set_debug(Bool, infinity).

set_debug(Bool, Timeout) when is_boolean(Bool) ->
    Req = {set_debug, Bool, self()},
    gen_server:call(ssl_server, Req, Timeout).
                
enable_debugmsg() ->
    set_debugmsg(true).

disable_debugmsg() ->
    set_debugmsg(false).

set_debugmsg(Bool) ->
    set_debugmsg(Bool, infinity).

set_debugmsg(Bool, Timeout) when is_boolean(Bool) ->
    Req = {set_debugmsg, Bool, self()},
    gen_server:call(ssl_server, Req, Timeout).

dump() ->
    dump(infinity).

dump(Timeout) ->
    Req = {dump, self()}, 
    gen_server:call(ssl_server, Req, Timeout).

%%
%% init
%%
init([]) ->
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
    ProgDir = 
	case init:get_argument(ssl_portprogram_dir) of
	    {ok, [[D]]} ->
		D;
	    _ ->
		find_priv_bin()
	end,
    {Program, Flags} = mk_cmd_line("ssl_esock"),
    Cmd = filename:join(ProgDir, Program) ++ " " ++ Flags,
    debug1(Debug, " start, Cmd =  ~s~n", [Cmd]), 
    case (catch open_port({spawn, Cmd}, [binary, {packet, 4}])) of
	Port when is_port(Port) ->
	    process_flag(trap_exit, true), 
	    receive 
		{Port, {data, Bin}} ->
		    {ProxyLLPort, ProgPid, CompVsn, LibVsn, Ciphers} = 
			decode_msg(Bin, [int16, int32, string, string, 
					 string]), 
		    debug1(Debug, "port program pid = ~w~n", 
			   [ProgPid]), 
		    {ok, #st{port = Port, 
			     proxylsport = ProxyLLPort,
			     progpid = ProgPid, 
			     debug = Debug, 
			     compvsn = CompVsn, 
			     libvsn = LibVsn,
			     ciphers = Ciphers}};
		{'EXIT', Port, Reason} ->
		    {stop, Reason}
	    end;
	{'EXIT', Reason} ->
	    {stop, Reason}
    end.

%%
%% transport_accept
%%
handle_call({transport_accept, Broker, ListenFd, Flags}, From, St) ->
    debug(St, "transport_accept: broker = ~w, listenfd = ~w~n", 
	  [Broker, ListenFd]),
    case get_by_fd(ListenFd, St#st.cons) of
	{ok, {ListenFd, _, _}} ->
	    send_cmd(St#st.port, ?TRANSPORT_ACCEPT, [int32(ListenFd), Flags, 0]),
	    PAccepts = add({ListenFd, Broker, From}, St#st.paccepts),
	    %%
	    %% We reply when we get TRANSPORT_ACCEPT_REP or ASYNC_ACCEPT_ERR
	    %% 
	    {noreply, St#st{paccepts = PAccepts}};
	_Other ->
	    {reply, {error, ebadf}, St}
    end;

%%
%% ssl_accept
%%
handle_call({ssl_accept, Fd, Flags}, From, St) ->
    case replace_from_by_fd(Fd, St#st.cons, From) of
	{ok, _, Cons} = _Rep ->
	    send_cmd(St#st.port, ?SSL_ACCEPT, [int32(Fd), Flags, 0]),
	    %% We reply when we get SSL_ACCEPT_REP or ASYNC_ACCEPT_ERR
	    {noreply, St#st{cons = Cons}};
	_Other ->
	    {reply, {error, ebadf}, St}
    end;

%%
%% version
%%
handle_call(ciphers, From, St) ->
    debug(St, "ciphers: from = ~w~n", [From]),
    {reply, {ok, St#st.ciphers}, St};

%%
%% connect
%%
handle_call({connect, Broker, LIP, LPort, FIP, FPort, Flags}, From, St) ->
    debug(St, "connect: broker = ~w, ip = ~w, "
	  "sport = ~w~n", [Broker, FIP, FPort]),
    Port = St#st.port,
    LIPStr = ip_to_string(LIP),
    FIPStr = ip_to_string(FIP),
    IntRef = new_intref(St),
    send_cmd(Port, ?CONNECT, [int32(IntRef),
			      int16(LPort), LIPStr, 0,
			      int16(FPort), FIPStr, 0,
			      Flags, 0]),
    Cons = add({{intref, IntRef}, Broker, From}, St#st.cons),
    %% We reply when we have got CONNECT_SYNC_ERR, or CONNECT_WAIT 
    %% and CONNECT_REP, or CONNECT_ERR.
    {noreply, St#st{cons = Cons, intref = IntRef}};

%%
%% connection_info
%%
handle_call({connection_info, Broker, Fd}, From, St) ->
    debug(St, "connection_info: broker = ~w, fd = ~w~n",
	  [Broker, Fd]),
    case replace_from_by_fd(Fd, St#st.cons, From) of
	{ok, _, Cons} ->
	    send_cmd(St#st.port, ?GETCONNINFO, [int32(Fd)]),
	    %% We reply when we get GETCONNINFO_REP or GETCONNINFO_ERR.
	    {noreply, St#st{cons = Cons}};
	_Other ->
	    {reply, {error, ebadf}, St}
    end;

%%
%% close
%%
handle_call({close, Broker, Fd}, _From, St) ->
    debug(St, "close: broker = ~w, fd = ~w~n",
	  [Broker, Fd]),
    #st{port = Port, cons = Cons0, paccepts = PAccepts0} = St,
    case delete_by_fd(Fd, Cons0) of
	%% Must match Broker pid; fd may be reused already.
	{ok, {Fd, Broker, _}, Cons} ->
	    send_cmd(Port, ?CLOSE, int32(Fd)),
	    %% If Fd is a listen socket fd, there might be pending
	    %% accepts for that fd.
	    case delete_all_by_fd(Fd, PAccepts0) of
		{ok, DelAccepts, RemAccepts} ->
		    %% Reply {error, closed} to all pending accepts
		    lists:foreach(fun({_, _, AccFrom}) ->
					  gen_server:reply(AccFrom,
							   {error, closed})
				  end, DelAccepts),
		    {reply, ok,
		     St#st{cons = Cons, paccepts = RemAccepts}};
		_ ->
		    {reply, ok, St#st{cons = Cons}}
	    end;
	_ ->
	    {reply, ok, St}
    end;

%%
%% listen
%%
handle_call({listen, Broker, IP, LPort, Flags, BackLog}, From, St) ->
    debug(St, "listen: broker = ~w, IP = ~w, "
	  "sport = ~w~n", [Broker, IP, LPort]),
    Port = St#st.port,
    IPStr = ip_to_string(IP),
    IntRef = new_intref(St),
    send_cmd(Port, ?LISTEN, [int32(IntRef), int16(LPort), IPStr, 0,
			     int16(BackLog), Flags, 0]),
    Cons = add({{intref, IntRef}, Broker, From}, St#st.cons),
    %% We reply when we have got LISTEN_REP.
    {noreply, St#st{cons = Cons, intref = IntRef}};

%%
%% peercert
%%
handle_call({peercert, Broker, Fd}, From, St) ->
    debug(St, "peercert: broker = ~w, fd = ~w~n",
	  [Broker, Fd]),
    case replace_from_by_fd(Fd, St#st.cons, From) of 
	{ok, _, Cons} ->
	    send_cmd(St#st.port, ?GETPEERCERT, [int32(Fd)]),
	    %% We reply when we get GETPEERCERT_REP or GETPEERCERT_ERR.
	    {noreply, St#st{cons = Cons}};
	_Other ->
	    {reply, {error, ebadf}, St}
    end;


%%
%% peername
%%
handle_call({peername, Broker, Fd}, From, St) ->
    debug(St, "peername: broker = ~w, fd = ~w~n",
	  [Broker, Fd]),
    case replace_from_by_fd(Fd, St#st.cons, From) of 
	{ok, _, Cons} ->
	    send_cmd(St#st.port, ?GETPEERNAME, [int32(Fd)]),
	    %% We reply when we get GETPEERNAME_REP or GETPEERNAME_ERR.
	    {noreply, St#st{cons = Cons}};
	_Other ->
	    {reply, {error, ebadf}, St}
    end;

%%
%% proxy join
%%
handle_call({proxy_join, Broker, Fd, LPort}, From, St) ->
    debug(St, "proxy_join: broker = ~w, fd = ~w, "
	  "sport = ~w~n", [Broker, Fd, LPort]),
    case replace_from_by_fd(Fd, St#st.cons, From) of 
	{ok, _, Cons} ->
	    send_cmd(St#st.port, ?PROXY_JOIN, [int32(Fd), 
						     int16(LPort)]), 
	    %% We reply when we get PROXY_JOIN_REP, or PROXY_JOIN_ERR.
	    {noreply, St#st{cons = Cons}};
	_Other ->
	    {reply, {error, ebadf}, St}
    end;

%%
%% seed
%%
handle_call({seed, Data}, _From, St) when is_binary(Data) ->
    send_cmd(St#st.port, ?SET_SEED, [int32(byte_size(Data)), Data]),
    {reply, ok, St};

handle_call({seed, Data}, From, St) ->
    case catch list_to_binary(Data) of
	{'EXIT', _} ->
	    {reply, {error, edata}, St};
	Bin  ->
	    handle_call({seed, Bin}, From, St)
    end;

%%
%% setnodelay
%%
handle_call({setnodelay, Broker, Fd, Boolean}, From, St) ->
    debug(St, "setnodelay: broker = ~w, fd = ~w, "
	  "boolean = ~w~n", [Broker, Fd, Boolean]),
    case replace_from_by_fd(Fd, St#st.cons, From) of 
	{ok, _, Cons} ->
	    Val = if Boolean == true -> 1; true -> 0 end,
	    send_cmd(St#st.port, ?SET_SOCK_OPT, 
		     [int32(Fd), ?SET_TCP_NODELAY, Val]),
	    %% We reply when we get IOCTL_OK or IOCTL_ERR.
	    {noreply, St#st{cons = Cons}};
	_Other ->
	    {reply, {error, ebadf}, St}
    end;

%%
%% sockname
%%
handle_call({sockname, Broker, Fd}, From, St) ->
    debug(St, "sockname: broker = ~w, fd = ~w~n",
	  [Broker, Fd]),
    case replace_from_by_fd(Fd, St#st.cons, From) of 
	{ok, _, Cons} ->
	    send_cmd(St#st.port, ?GETSOCKNAME, [int32(Fd)]),
	    %% We reply when we get GETSOCKNAME_REP or GETSOCKNAME_ERR.
	    {noreply, St#st{cons = Cons}};
	_Other ->
	    {reply, {error, ebadf}, St}
    end;

%%
%% version
%%
handle_call(version, From, St) ->
    debug(St, "version: from = ~w~n", [From]),
    {reply, {ok, {St#st.compvsn, St#st.libvsn}}, St};

%%
%% dump
%%
handle_call({dump, Broker}, _From, St) ->
    debug(St, "dump: broker = ~w", [Broker]),
    Port = St#st.port,
    send_cmd(Port, ?DUMP_CMD, []),
    {reply, ok, St};

%%
%% set_debug
%%
handle_call({set_debug, Bool, Broker}, _From, St) ->
    debug(St, "set_debug: broker = ~w", [Broker]),
   Value = case Bool of 
                true ->
                    1;
                false ->
                    0
            end,
    Port = St#st.port,
    send_cmd(Port, ?DEBUG_CMD, [Value]),
    {reply, ok, St};

%%
%% set_debugmsg
%%
handle_call({set_debugmsg, Bool, Broker}, _From, St) ->
    debug(St, "set_debugmsg: broker = ~w", [Broker]),
    Value = case Bool of 
                true ->
                    1;
                false ->
                    0
            end,
    Port = St#st.port,
    send_cmd(Port, ?DEBUGMSG_CMD, [Value]),
    {reply, ok, St};

handle_call(Request, _From, St) ->
    debug(St, "unexpected call: ~w~n", [Request]),
    Reply = {error, {badcall, Request}},
    {reply, Reply, St}.

%%
%% handle_cast(Msg, St)
%%


handle_cast(Msg, St) ->
    debug(St, "unexpected cast: ~w~n", [Msg]),
    {noreply, St}.

%%
%% handle_info(Info, St)
%%

%% Data from port
%%
handle_info({Port, {data, Bin}},
	    #st{cons = StCons, paccepts = Paccepts,
		port = Port, proxylsport = Proxylsport} = St) 
  when is_binary(Bin) ->
    %% io:format("++++ ssl_server got from port: ~w~n", [Bin]),
    <<OpCode:8, _/binary>> = Bin,
    case OpCode of
	%%
	%% transport_accept
	%%
	?TRANSPORT_ACCEPT_ERR when byte_size(Bin) >= 5 ->
	    {ListenFd, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(St, "transport_accept_err: listenfd = ~w, "
		  "reason = ~w~n", [ListenFd, Reason]),
	    case delete_last_by_fd(ListenFd, Paccepts) of
		{ok, {_, _, From}, PAccepts} ->
		    gen_server:reply(From, {error, Reason}),
		    {noreply, St#st{paccepts = PAccepts}};
		_Other ->
		    %% Already closed
		    {noreply, St}
	    end;
	?TRANSPORT_ACCEPT_REP when byte_size(Bin) >= 9 ->
	    {ListenFd, Fd} = decode_msg(Bin, [int32, int32]),
	    debug(St, "transport_accept_rep: listenfd = ~w, "
		  "fd = ~w~n", [ListenFd, Fd]),
	    case delete_last_by_fd(ListenFd, Paccepts) of
		{ok, {_, Broker, From}, PAccepts} ->
		    Reply = {ok, Fd, Proxylsport},
		    gen_server:reply(From, Reply),
		    debug(St, "transport_accept_rep: From = ~w\n", [From]),
		    Cons = add({Fd, Broker, From}, StCons),
		    {noreply, St#st{cons = Cons, paccepts = PAccepts}};
		_Other ->
		    %% Already closed
		    {noreply, St}
	    end;
	
	%%
	%% ssl_accept
	%%
	?SSL_ACCEPT_ERR when byte_size(Bin) >= 5 ->
	    {Fd, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(St, "ssl_accept_err: listenfd = ~w, "
		  "reason = ~w~n", [Fd, Reason]),
	    %% JC: remove this?
	    case delete_last_by_fd(Fd, StCons) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {error, Reason}),
		    {noreply, St#st{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, St}
	    end;
	?SSL_ACCEPT_REP when byte_size(Bin) >= 5 ->
	    Fd = decode_msg(Bin, [int32]),
	    debug(St, "ssl_accept_rep: Fd = ~w\n", [Fd]),
	    case replace_from_by_fd(Fd, StCons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, ok),
		    {noreply, St#st{cons = Cons}};
		_ ->
		    {noreply, St}
	    end;

	%%
	%% connect
	%%
	?CONNECT_SYNC_ERR when byte_size(Bin) >= 5 ->
	    {IntRef, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(St, "connect_sync_err: intref = ~w, "
		  "reason = ~w~n", [IntRef, Reason]),
	    case delete_by_intref(IntRef, StCons) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {error, Reason}),
		    {noreply, St#st{cons = Cons}};
		_Other ->
		    {noreply, St}
	    end;
	?CONNECT_WAIT when byte_size(Bin) >= 9 ->  
	    {IntRef, Fd} = decode_msg(Bin, [int32, int32]),
	    debug(St, "connect_wait: intref = ~w, "
		  "fd = ~w~n", [IntRef, Fd]),
	    case replace_fd_by_intref(IntRef, StCons, Fd) of
		{ok, _, Cons} ->
		    %% We reply when we get CONNECT_REP or CONNECT_ERR
		    {noreply, St#st{cons = Cons}};
		_Other ->
		    %% We have a new Fd which must be closed
		    send_cmd(Port, ?CLOSE, int32(Fd)),
		    {noreply, St}
	    end;
	?CONNECT_REP when byte_size(Bin) >= 5 ->  
	    %% after CONNECT_WAIT
	    Fd = decode_msg(Bin, [int32]),
	    debug(St, "connect_rep: fd = ~w~n", [Fd]),
	    case replace_from_by_fd(Fd, StCons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {ok, Fd, Proxylsport}),
		    {noreply, St#st{cons = Cons}};
		_Other ->
		    {noreply, St}
	    end;
	?CONNECT_ERR when byte_size(Bin) >= 5 ->
	    {Fd, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(St, "connect_err: fd = ~w, "
		  "reason = ~w~n", [Fd, Reason]),
	    case delete_by_fd(Fd, StCons) of
		{ok, {_, _, From}, Cons} ->
		    %% Fd not yet published - hence close ourselves
		    send_cmd(Port, ?CLOSE, int32(Fd)),
		    gen_server:reply(From, {error, Reason}),
		    {noreply, St#st{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, St}
	    end;

	%%
	%% connection_info
	%%
	?GETCONNINFO_REP when byte_size(Bin) >= 5 ->
	    {Fd, Protocol, Cipher} = decode_msg(Bin, [int32, string, string]),
	    debug(St, "connection_info_rep: fd = ~w, "
		  "protcol = ~p, ip = ~p~n", [Fd, Protocol, Cipher]),
	    case replace_from_by_fd(Fd, StCons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {ok, {protocol_name(Protocol),
						 Cipher}}),
		    {noreply, St#st{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, St}
	    end;
	?GETCONNINFO_ERR when byte_size(Bin) >= 5 ->
	    {Fd, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(St, "connection_info_err: fd = ~w, "
		  "reason = ~w~n", [Fd, Reason]),
	    case replace_from_by_fd(Fd, StCons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {error, Reason}),
		    {noreply, St#st{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, St}
	    end;

	%%
	%% listen
	%%
	?LISTEN_SYNC_ERR when byte_size(Bin) >= 5 ->
	    {IntRef, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(St, "listen_sync_err: intref = ~w, "
		  "reason = ~w~n", [IntRef, Reason]),
	    case delete_by_intref(IntRef, StCons) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {error, Reason}),
		    {noreply, St#st{cons = Cons}};
		_Other ->
		    {noreply, St}
	    end;
	?LISTEN_REP when byte_size(Bin) >= 11 ->  
	    {IntRef, ListenFd, LPort} = decode_msg(Bin, [int32, int32, int16]),
	    debug(St, "listen_rep: intref = ~w, "
		  "listenfd = ~w, sport = ~w~n", [IntRef, ListenFd, LPort]),
	    case replace_fd_from_by_intref(IntRef, StCons, ListenFd, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {ok, ListenFd, LPort}),
		    {noreply, St#st{cons = Cons}};
		_Other ->
		    %% ListenFd has to be closed.
		    send_cmd(Port, ?CLOSE, int32(ListenFd)),
		    {noreply, St}
	    end;

	%%
	%% proxy join
	%%
	?PROXY_JOIN_REP when byte_size(Bin) >= 5 -> 
	    Fd = decode_msg(Bin, [int32]),
	    debug(St, "proxy_join_rep: fd = ~w~n",
		  [Fd]),
	    case get_by_fd(Fd, StCons) of
		{ok, {_, _, From}} ->
		    gen_server:reply(From, ok),
		    {noreply, St};
		_Other ->
		    %% Already closed
		    {noreply, St}
	    end;
	?PROXY_JOIN_ERR when byte_size(Bin) >= 5 -> 
	    {Fd, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(St, "proxy_join_rep: fd = ~w, "
		  "reason = ~w~n", [Fd, Reason]),
	    case delete_by_fd(Fd, StCons) of
		{ok, {_, _, From}, Cons} ->
		    case Reason of
			enoproxysocket ->	
			    send_cmd(Port, ?CLOSE, int32(Fd));
			_ ->
			    ok
			    %% Must not close Fd since it is published
		    end,
		    gen_server:reply(From, {error, Reason}),
		    {noreply, St#st{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, St}
	    end;

	%%
	%% peername
	%%
	?GETPEERNAME_REP when byte_size(Bin) >= 5 ->
	    {Fd, LPort, IPString} = decode_msg(Bin, [int32, int16, string]),
	    debug(St, "getpeername_rep: fd = ~w, "
		  "sport = ~w, ip = ~p~n", [Fd, LPort, IPString]),
	    case replace_from_by_fd(Fd, StCons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {ok, {IPString, LPort}}),
		    {noreply, St#st{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, St}
	    end;
	?GETPEERNAME_ERR when byte_size(Bin) >= 5 ->
	    {Fd, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(St, "getpeername_err: fd = ~w, "
		  "reason = ~w~n", [Fd, Reason]),
	    case replace_from_by_fd(Fd, StCons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {error, Reason}),
		    {noreply, St#st{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, St}
	    end;

	%%
	%% ioctl
	%%
	?IOCTL_OK when byte_size(Bin) >= 5 ->
	    Fd = decode_msg(Bin, [int32]),
	    debug(St, "ioctl_ok: fd = ~w~n",
		  [Fd]),
	    case replace_from_by_fd(Fd, StCons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, ok),
		    {noreply, St#st{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, St}
	    end;
	?IOCTL_ERR when byte_size(Bin) >= 5 ->
	    {Fd, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(St, "ioctl_err: fd = ~w, "
		  "reason = ~w~n", [Fd, Reason]),
	    case replace_from_by_fd(Fd, StCons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {error, Reason}),
		    {noreply, St#st{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, St}
	    end;

	%%
	%% sockname
	%%
	?GETSOCKNAME_REP when byte_size(Bin) >= 5 ->
	    {Fd, LPort, IPString} = decode_msg(Bin, [int32, int16, string]),
	    debug(St, "getsockname_rep: fd = ~w, "
		  "sport = ~w, ip = ~p~n", [Fd, LPort, IPString]),
	    case replace_from_by_fd(Fd, StCons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {ok, {IPString, LPort}}),
		    {noreply, St#st{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, St}
	    end;
	?GETSOCKNAME_ERR when byte_size(Bin) >= 5 ->
	    {Fd, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(St, "getsockname_err: fd = ~w, "
		  "reason = ~w~n", [Fd, Reason]),
	    case replace_from_by_fd(Fd, StCons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {error, Reason}),
		    {noreply, St#st{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, St}
	    end;

	%%
	%% peercert
	%%
	?GETPEERCERT_REP when byte_size(Bin) >= 5 ->
	    {Fd, Cert} = decode_msg(Bin, [int32, bin]),
	    debug(St, "getpeercert_rep: fd = ~w~n", [Fd]),
	    case replace_from_by_fd(Fd, StCons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {ok, Cert}),
		    {noreply, St#st{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, St}
	    end;
	?GETPEERCERT_ERR when byte_size(Bin) >= 5 ->
	    {Fd, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(St, "getpeercert_err: fd = ~w, reason = ~w~n", 
		  [Fd, Reason]),
	    case replace_from_by_fd(Fd, StCons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {error, Reason}),
		    {noreply, St#st{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, St}
	    end
    end;

%%
%% EXIT
%%
handle_info({'EXIT', Pid, Reason}, St) when is_pid(Pid) ->
    debug(St, "exit pid = ~w, "
	  "reason = ~w~n", [Pid, Reason]),
    case delete_by_pid(Pid, St#st.cons) of
	{ok, {{intref, _}, Pid, _}, Cons} ->
	    {noreply, St#st{cons = Cons}};
	{ok, {Fd, Pid, _}, Cons} ->
	    send_cmd(St#st.port, ?CLOSE, int32(Fd)), 
	    %% If Fd is a listen socket fd, there might be pending
	    %% accepts for that fd.
	    case delete_all_by_fd(Fd, St#st.paccepts) of
		{ok, DelAccepts, RemAccepts} ->
		    %% Reply {error, closed} to all pending accepts.
		    lists:foreach(fun({_, _, From}) ->
					  gen_server:reply(From, 
							   {error, closed}) 
				  end, DelAccepts),
		    {noreply, 
		     St#st{cons = Cons, paccepts = RemAccepts}};
		_ ->
		    {noreply, St#st{cons = Cons}}
	    end;
	_ ->
	    case delete_by_pid(Pid, St#st.paccepts) of
		{ok, {ListenFd, _, _}, PAccepts} ->
		    %% decrement ref count in port program
		    send_cmd(St#st.port, ?NOACCEPT, int32(ListenFd)),
		    {noreply, St#st{paccepts = PAccepts}};
		_ ->
		    {noreply, St}
	    end
    end;

%%
%% 'badsig' means bad message to port. Port program is unaffected.
%%
handle_info({'EXIT', Port, badsig}, #st{port = Port} = St) ->
    debug(St, "badsig!!!~n", []),
    {noreply, St};

handle_info({'EXIT', Port, Reason}, #st{port = Port} = St) ->
    {stop, Reason, St};

handle_info(Info, St) ->
    debug(St, "unexpected info: ~w~n", [Info]),
    {noreply, St}.

%%
%% terminate(Reason, St) -> any
%%
terminate(_Reason, _St) ->
    ok.

%% 
%% code_change(OldVsn, St, Extra) -> {ok, NSt}
%%
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%
%% Send binary command to sock
%%
send_cmd(Port, Cmd, Args) ->
    Port ! {self(), {command, [Cmd| Args]}}.

%%
%% add(Descr, Cons) -> NCons
%%
add(D, L) -> 
    [D| L].

%%
%% get_by_fd(Fd, Cons) -> {ok, Descr} | not_found
%%
get_by_fd(Fd, Cons) ->
    get_by_pos(Fd, 1, Cons).

%%
%% delete_by_fd(Fd, Cons) -> {ok, OldDesc, NewCons} | not_found.
%%
delete_by_fd(Fd, Cons) ->
    delete_by_pos(Fd, 1, Cons).

%%
%% delete_all_by_fd(Fd, Cons) -> {ok, DelCons, RemCons} | not_found.
%%
delete_all_by_fd(Fd, Cons) ->
    delete_all_by_pos(Fd, 1, Cons).

%%
%% delete_by_intref(IntRef, Cons) -> {ok, OldDesc, NewCons} | not_found.
%%
delete_by_intref(IntRef, Cons) ->
    delete_by_pos({intref, IntRef}, 1, Cons).

%%
%% delete_by_pid(Pid, Cons) -> {ok, OldDesc, NewCons} | not_found.
%%
delete_by_pid(Pid, Cons) ->
    delete_by_pos(Pid, 2, Cons).

%%
%% delete_last_by_fd(Fd, Cons) -> {ok, OldDesc, NCons} | not_found
%%
delete_last_by_fd(Fd, Cons) ->
    case dlbf(Fd, Cons) of 
	{X, L} ->
	    {ok, X, L};
	_Other  ->
	    not_found
    end.

dlbf(Fd, [H]) -> 
    last_elem(Fd, H, []);
dlbf(Fd, [H|T]) ->
    case dlbf(Fd, T) of
	{X, L} -> 
	    {X, [H|L]};
	L -> 
	    last_elem(Fd, H, L)
    end;
dlbf(_Fd, []) ->  
    [].

last_elem(Fd, H, L) when element(1, H) == Fd ->
    {H, L};
last_elem(_, H, L) ->
    [H|L].


%%
%% replace_from_by_fd(Fd, Cons, From) -> {ok, OldDesc, NewList} | not_found
%%
replace_from_by_fd(Fd, Cons, From) ->
    replace_posn_by_pos(Fd, 1, Cons, [{From, 3}]).

%%
%% replace_fd_by_intref(IntRef, Cons, Fd) -> {ok, OldDesc, NewList} | not_f.
%%
replace_fd_by_intref(IntRef, Cons, Fd) ->
    replace_posn_by_pos({intref, IntRef}, 1, Cons, [{Fd, 1}]).

%%
%% replace_fd_from_by_intref(IntRef, Cons, NFd, From) -> 
%%					{ok, OldDesc, NewList} |  not_found
%%
replace_fd_from_by_intref(IntRef, Cons, NFd, From) ->
    replace_posn_by_pos({intref, IntRef}, 1, Cons, [{NFd, 1}, {From, 3}]).


%%
%% All *_by_pos functions
%%

get_by_pos(Key, Pos, [H|_]) when element(Pos, H) == Key -> 
    {ok, H};
get_by_pos(Key, Pos, [_|T]) -> 
    get_by_pos(Key, Pos, T);
get_by_pos(_, _, []) -> 
    not_found.

delete_by_pos(Key, Pos, Cons) ->
    case delete_by_pos1(Key, Pos, {not_found, Cons}) of
	{not_found, _} ->
	    not_found;
	{ODesc, NCons} ->
	    {ok, ODesc, NCons}
    end.
delete_by_pos1(Key, Pos, {_R, [H|T]}) when element(Pos, H) == Key ->
    {H, T};
delete_by_pos1(Key, Pos, {R, [H|T]}) ->
    {R0, T0} = delete_by_pos1(Key, Pos, {R, T}),
    {R0, [H| T0]};
delete_by_pos1(_, _, {R, []}) ->
    {R, []}.

delete_all_by_pos(Key, Pos, Cons) ->
    case lists:foldl(fun(H, {Ds, Rs}) when element(Pos, H) == Key ->
			     {[H|Ds], Rs};
			(H, {Ds, Rs}) ->
			     {Ds, [H|Rs]} 
		     end, {[], []}, Cons) of
	{[], _} ->
	    not_found;
	{DelCons, RemCons} ->
	    {ok, DelCons, RemCons}
    end.

replace_posn_by_pos(Key, Pos, Cons, Repls) ->
    replace_posn_by_pos1(Key, Pos, Cons, Repls, []).

replace_posn_by_pos1(Key, Pos, [H0| T], Repls, Acc)
  when element(Pos, H0) =:= Key ->
    H = lists:foldl(fun({Val, VPos}, Tuple) -> 
			    setelement(VPos, Tuple, Val) 
		    end, H0, Repls), 
    {ok, H0, lists:reverse(Acc, [H| T])};
replace_posn_by_pos1(Key, Pos, [H|T], Repls, Acc) ->
    replace_posn_by_pos1(Key, Pos, T, Repls, [H| Acc]);
replace_posn_by_pos1(_, _, [], _, _) ->
    not_found.

%%
%% Binary/integer conversions
%%
int16(I) ->
    %%[(I bsr 8) band 255, I band 255].
    <<I:16>>.

int32(I) -> 
    %%     [(I bsr 24) band 255,
    %%      (I bsr 16) band 255,
    %%      (I bsr  8) band 255,
    %%      I band 255].
    <<I:32>>.

%% decode_msg(Bin, Format) -> Tuple | integer() | atom() | string() | 
%%				list of binaries()
%%
%% Decode message from binary
%% Format = [spec()]
%% spec() = int16 | int32 | string | atom | bin | bins
%%
%% Notice:  The first byte (op code) of the binary message is removed.
%% Notice:  bins returns a *list* of binaries. 
%%  
decode_msg(<<_, Bin/binary>>, Format) ->
    Dec = dec(Format, Bin),
    case Dec of
	[Dec1] -> Dec1;
	_  -> list_to_tuple(Dec)
    end.

dec([], _) ->
    [];
dec([int16| F], <<N:16, Bin/binary>>) ->
    [N| dec(F, Bin)];
dec([int32| F], <<N:32, Bin/binary>>) ->
    [N| dec(F, Bin)];
dec([string| F], Bin0) ->
    {Cs, Bin1} = dec_string(Bin0),
    [Cs| dec(F, Bin1)];
dec([atom|F], Bin0) ->
    {Cs, Bin1} = dec_string(Bin0),
    [list_to_atom(Cs)| dec(F, Bin1)];

dec([bin|F], Bin) ->
    {Bin1, Bin2} = dec_bin(Bin),
    [Bin1| dec(F, Bin2)].

%% NOTE: This clause is not actually used yet.
%% dec([bins|F], <<N:32, Bin0/binary>>) ->
%%     {Bins, Bin1} = dec_bins(N, Bin0),
%%     [Bins| dec(F, Bin1)].

dec_string(Bin) ->
    dec_string(Bin, []).

dec_string(<<0, Bin/binary>>, RCs) ->
    {lists:reverse(RCs), Bin};
dec_string(<<C, Bin/binary>>, RCs) ->
    dec_string(Bin, [C| RCs]).

dec_bin(<<L:32, Bin0/binary>>) ->
    <<Bin1:L/binary, Bin2/binary>> = Bin0,
    {Bin1, Bin2}.

%% dec_bins(N, Bin) ->
%%     dec_bins(N, Bin, []).

%% dec_bins(0, Bin, Acc) ->
%%     {lists:reverse(Acc), Bin};
%% dec_bins(N, Bin0, Acc) when N > 0 ->
%%     {Bin1, Bin2} = dec_bin(Bin0),
%%     dec_bins(N - 1, Bin2, [Bin1| Acc]).

%%
%% new_intref
%%
new_intref(St) ->
    (St#st.intref + 1) band 16#ffffffff.

%%
%% {Program, Flags} = mk_cmd_line(DefaultProgram)
%%
mk_cmd_line(Default) ->
    {port_program(Default), 
     lists:flatten([debug_flag(), " ", debug_port_flag(), " ",
		    debugdir_flag(), " ", 
		    msgdebug_flag(), " ", proxylsport_flag(), " ", 
		    proxybacklog_flag(), " ", ephemeral_rsa_flag(), " ",
		    ephemeral_dh_flag(), " ",
		    protocol_version_flag(), " "])}.

port_program(Default) ->
    case application:get_env(ssl, port_program) of
	{ok, Program} when is_list(Program) ->
	    Program;
	_Other ->
	    Default
    end.

%%
%% As this server may be started by the distribution, it is not safe to assume 
%% a working code server, neither a working file server.
%% I try to utilize the most primitive interfaces available to determine
%% the directory of the port_program.
%%
find_priv_bin() ->
    PrivDir = case (catch code:priv_dir(ssl)) of
		  {'EXIT', _} ->
		      %% Code server probably not startet yet
		      {ok, P} = erl_prim_loader:get_path(),
		      ModuleFile = atom_to_list(?MODULE) ++ extension(),
		      Pd = (catch lists:foldl
			    (fun(X,Acc) ->
				     M = filename:join([X, ModuleFile]),
				     %% The file server probably not started
				     %% either, has to use raw interface.
				     case file:raw_read_file_info(M) of 
					 {ok,_} -> 
					     %% Found our own module in the
					     %% path, lets bail out with
					     %% the priv_dir of this directory
					     Y = filename:split(X),
					     throw(filename:join
						   (lists:sublist
						    (Y,length(Y) - 1) 
						    ++ ["priv"])); 
					 _ -> 
					     Acc 
				     end 
			     end,
			     false,P)),
		      case Pd of
			  false ->
			      exit(ssl_priv_dir_indeterminate);
			  _ ->
			      Pd
		      end;
		  Dir ->
		      Dir
	      end,
    filename:join([PrivDir, "bin"]).

extension() ->
    %% erlang:info(machine) returns machine name as text in all uppercase
    "." ++ string:to_lower(erlang:system_info(machine)).

debug_flag() ->
    case os:getenv("ERL_SSL_DEBUG") of
	false ->
	    get_env(debug, "-d");
	_ ->
	    "-d"
    end.

debug_port_flag() ->
    case os:getenv("ERL_SSL_DEBUGPORT") of
	false ->
	    get_env(debug, "-d");
	_ ->
	    "-d"
    end.

msgdebug_flag() ->
    case os:getenv("ERL_SSL_MSGDEBUG") of
	false ->
	    get_env(msgdebug, "-dm");
	_  ->
	    "-dm"
    end.

proxylsport_flag() ->
    case application:get_env(ssl, proxylsport) of
	{ok, PortNum} ->
	    "-pp " ++ integer_to_list(PortNum);
	_Other ->
	    ""
    end.

proxybacklog_flag() ->
    case application:get_env(ssl, proxylsbacklog) of
	{ok, Size} ->
	    "-pb " ++ integer_to_list(Size);
	_Other ->
	    ""
    end.

debugdir_flag() ->
    case os:getenv("ERL_SSL_DEBUG") of
	false ->
	    case application:get_env(ssl, debugdir) of
		{ok, Dir} when is_list(Dir) ->
		    "-dd " ++ Dir;
		_Other ->
		    ""
	    end;
	_  ->
	    "-dd ./"
    end.
    
ephemeral_rsa_flag() ->
    case application:get_env(ssl, ephemeral_rsa) of
	{ok, true} ->
	    "-ersa ";
	_Other ->
	    ""
    end.

ephemeral_dh_flag() ->
    case application:get_env(ssl, ephemeral_dh) of
	{ok, true} ->
	    "-edh ";
	_Other ->
	    ""
    end.

protocol_version_flag() ->
    case application:get_env(ssl, protocol_version) of
	{ok, []} ->
	    "";
	{ok, Vsns} when is_list(Vsns) ->
	    case transform_vsns(Vsns) of
		N when (N > 0) ->
		    "-pv " ++ integer_to_list(N);
		_ ->
		    ""
	    end;
	_Other ->
	    ""
    end.

transform_vsns(Vsns) ->
    transform_vsns(Vsns, 0).

transform_vsns([sslv2| Vsns], I) ->
    transform_vsns(Vsns, I bor ?SSLv2);
transform_vsns([sslv3| Vsns], I) ->
    transform_vsns(Vsns, I bor ?SSLv3);
transform_vsns([tlsv1| Vsns], I) ->
    transform_vsns(Vsns, I bor ?TLSv1);
transform_vsns([_ | Vsns], I) ->
    transform_vsns(Vsns, I);
transform_vsns([], I) ->
    I.

protocol_name("SSLv2") -> sslv2;
protocol_name("SSLv3") -> sslv3;
protocol_name("TLSv1") -> tlsv1.

get_env(Key, Val) ->
    case application:get_env(ssl, Key) of
	{ok, true} ->
	    Val;
	_Other ->
	    ""
    end.

ip_to_string({A,B,C,D}) ->
    [integer_to_list(A),$.,integer_to_list(B),$.,
     integer_to_list(C),$.,integer_to_list(D)].

debug(St, Format, Args) ->
    debug1(St#st.debug, Format, Args).

debug1(true, Format0, Args) ->
    {_MS, S, MiS} = erlang:now(),
    Secs = S rem 100, 
    MiSecs = MiS div 1000,
    Format = "++++ ~3..0w:~3..0w ssl_server (~w): " ++ Format0, 
    io:format(Format, [Secs, MiSecs, self()| Args]);
debug1(_, _, _) ->
    ok.
