%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2010. All Rights Reserved.
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
-module(erl_epmd).

-behaviour(gen_server).

-ifdef(DEBUG).
-define(port_please_failure(), io:format("Net Kernel 2: EPMD port please failed at ~p:~p~n", [?MODULE,?LINE])).
-define(port_please_failure2(Term), io:format("Net Kernel 2: EPMD port please failed at ~p:~p [~p]~n", [?MODULE,?LINE,Term])).
-else.
-define(port_please_failure(), noop).
-define(port_please_failure2(Term), noop).
-endif.

%% External exports
-export([start/0, start_link/0, stop/0, port_please/2, 
	 port_please/3, names/0, names/1,
	 register_node/2, open/0, open/1, open/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-import(lists, [reverse/1]).

-record(state, {socket, port_no = -1, name = ""}).
-type state() :: #state{}.

-include("inet_int.hrl").
-include("erl_epmd.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    gen_server:start({local, erl_epmd}, ?MODULE, [], []).


start_link() ->
    gen_server:start_link({local, erl_epmd}, ?MODULE, [], []).


stop() ->
    gen_server:call(?MODULE, stop, infinity).


%% Lookup a node "Name" at Host
%% return {port, P, Version} | noport
%%

port_please(Node, Host) ->
  port_please(Node, Host, infinity).

port_please(Node,HostName, Timeout) when is_atom(HostName) ->
  port_please1(Node,atom_to_list(HostName), Timeout);
port_please(Node,HostName, Timeout) when is_list(HostName) ->
  port_please1(Node,HostName, Timeout);
port_please(Node, EpmdAddr, Timeout) ->
  get_port(Node, EpmdAddr, Timeout).



port_please1(Node,HostName, Timeout) ->
  case inet:gethostbyname(HostName, inet, Timeout) of
    {ok,{hostent, _Name, _ , _Af, _Size, [EpmdAddr | _]}} ->
      get_port(Node, EpmdAddr, Timeout);
    Else ->
      Else
  end.

names() -> 
    {ok, H} = inet:gethostname(),
    names(H).

names(HostName) when is_atom(HostName) ->
  names1(atom_to_list(HostName));
names(HostName) when is_list(HostName) ->
  names1(HostName);
names(EpmdAddr) ->
  get_names(EpmdAddr).

names1(HostName) ->
  case inet:gethostbyname(HostName) of
    {ok,{hostent, _Name, _ , _Af, _Size, [EpmdAddr | _]}} ->
      get_names(EpmdAddr);
    Else ->
      Else
  end.


register_node(Name, PortNo) ->
    gen_server:call(erl_epmd, {register, Name, PortNo}, infinity).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

-spec init(_) -> {'ok', state()}.

init(_) ->
    {ok, #state{socket = -1}}.
	    
%%----------------------------------------------------------------------

-type calls() :: 'client_info_req' | 'stop' | {'register', term(), term()}.

-spec handle_call(calls(), term(), state()) ->
        {'reply', term(), state()} | {'stop', 'shutdown', 'ok', state()}.

handle_call({register, Name, PortNo}, _From, State) ->
    case State#state.socket of
	P when P < 0 ->
	    case do_register_node(Name, PortNo) of
		{alive, Socket, Creation} ->
		    S = State#state{socket = Socket,
				    port_no = PortNo,
				    name = Name},
		    {reply, {ok, Creation}, S};
		Error ->
		    {reply, Error, State}
	    end;
	_ ->
	    {reply, {error, already_registered}, State}
    end;

handle_call(client_info_req, _From, State) ->
    Reply = {ok,{r4,State#state.name,State#state.port_no}},
    {reply, Reply, State};
  
handle_call(stop, _From, State) ->
    {stop, shutdown, ok, State}.

%%----------------------------------------------------------------------

-spec handle_cast(term(), state()) -> {'noreply', state()}.

handle_cast(_, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------

-spec handle_info(term(), state()) -> {'noreply', state()}.

handle_info({tcp_closed, Socket}, State) when State#state.socket =:= Socket ->
    {noreply, State#state{socket = -1}};
handle_info(_, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------

-spec terminate(term(), state()) -> 'ok'.

terminate(_, #state{socket = Socket}) when Socket > 0 ->
    close(Socket),
    ok;
terminate(_, _) ->
    ok.

%%----------------------------------------------------------------------

-spec code_change(term(), state(), term()) -> {'ok', state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

get_epmd_port() ->
    case init:get_argument(epmd_port) of
	{ok, [[PortStr|_]|_]} when is_list(PortStr) ->
	    list_to_integer(PortStr);
	error ->
	    ?erlang_daemon_port
    end.
	    
%%
%% Epmd socket
%%
open() -> open({127,0,0,1}).  % The localhost IP address.

open({A,B,C,D}=EpmdAddr) when ?ip(A,B,C,D) ->
    gen_tcp:connect(EpmdAddr, get_epmd_port(), [inet]);
open({A,B,C,D,E,F,G,H}=EpmdAddr) when ?ip6(A,B,C,D,E,F,G,H) ->
    gen_tcp:connect(EpmdAddr, get_epmd_port(), [inet6]).

open({A,B,C,D}=EpmdAddr, Timeout) when ?ip(A,B,C,D) ->
    gen_tcp:connect(EpmdAddr, get_epmd_port(), [inet], Timeout);
open({A,B,C,D,E,F,G,H}=EpmdAddr, Timeout) when ?ip6(A,B,C,D,E,F,G,H) ->
    gen_tcp:connect(EpmdAddr, get_epmd_port(), [inet6], Timeout).

close(Socket) ->
    gen_tcp:close(Socket).

do_register_node(NodeName, TcpPort) ->
    case open() of
	{ok, Socket} ->
	    Name = to_string(NodeName),
	    Extra = "",
	    Elen = length(Extra),
	    Len = 1+2+1+1+2+2+2+length(Name)+2+Elen,
	    gen_tcp:send(Socket, [?int16(Len), ?EPMD_ALIVE2_REQ,
				   ?int16(TcpPort),
				   $M,
				   0,
				   ?int16(epmd_dist_high()),
				   ?int16(epmd_dist_low()),
				   ?int16(length(Name)),
				   Name,
				   ?int16(Elen),
				   Extra]),
	    wait_for_reg_reply(Socket, []);
	Error ->
	    Error
    end.

epmd_dist_high() ->
    case os:getenv("ERL_EPMD_DIST_HIGH") of
	false ->
	   ?epmd_dist_high; 
	Version ->
	    case (catch list_to_integer(Version)) of
		N when is_integer(N), N < ?epmd_dist_high ->
		    N;
		_ ->
		   ?epmd_dist_high
	    end
    end.

epmd_dist_low() ->
    case os:getenv("ERL_EPMD_DIST_LOW") of
	false ->
	   ?epmd_dist_low; 
	Version ->
	    case (catch list_to_integer(Version)) of
		N when is_integer(N), N > ?epmd_dist_low ->
		    N;
		_ ->
		   ?epmd_dist_low
	    end
    end.
		    


%%% (When we reply 'duplicate_name', it's because it's the most likely
%%% reason; there is no interpretation of the error result code.)
wait_for_reg_reply(Socket, SoFar) ->
    receive
	{tcp, Socket, Data0} ->
	    case SoFar ++ Data0 of
		[$y, Result, A, B] ->
		    case Result of
			0 ->
			    {alive, Socket, ?u16(A, B)};
			_ ->
			    {error, duplicate_name}
		    end;
		Data when length(Data) < 4 ->
		    wait_for_reg_reply(Socket, Data);
		Garbage ->
		    {error, {garbage_from_epmd, Garbage}}
	    end;
	{tcp_closed, Socket} ->
	    {error, epmd_close}
    after 10000 ->
	    gen_tcp:close(Socket),
	    {error, no_reg_reply_from_epmd}
    end.
    
%%
%% Lookup a node "Name" at Host
%%

get_port(Node, EpmdAddress, Timeout) ->
    case open(EpmdAddress, Timeout) of
	{ok, Socket} ->
	    Name = to_string(Node),
	    Len = 1+length(Name),
	    gen_tcp:send(Socket, [?int16(Len),?EPMD_PORT_PLEASE2_REQ, Name]),
	    wait_for_port_reply(Socket, []);
	_Error -> 
	    ?port_please_failure2(_Error),
	    noport
    end.


wait_for_port_reply(Socket, SoFar) ->
    receive
	{tcp, Socket, Data0} ->
%	    io:format("got ~p~n", [Data0]),
	    case SoFar ++ Data0 of
		[$w, Result | Rest] ->
		    case Result of
			0 ->
			    wait_for_port_reply_cont(Socket, Rest);
			_ ->
			    ?port_please_failure(),
			    wait_for_close(Socket, noport)
		    end;
		Data when length(Data) < 2 ->
		    wait_for_port_reply(Socket, Data);
		Garbage ->
		    ?port_please_failure(),
		    {error, {garbage_from_epmd, Garbage}}
	    end;
	{tcp_closed, Socket} ->
	    ?port_please_failure(),
	    closed
    after 10000 ->
	    ?port_please_failure(),
	    gen_tcp:close(Socket),
	    noport
    end.

wait_for_port_reply_cont(Socket, SoFar) when length(SoFar) >= 10 ->
    wait_for_port_reply_cont2(Socket, SoFar);
wait_for_port_reply_cont(Socket, SoFar) ->
    receive
	{tcp, Socket, Data0} ->
	    case SoFar ++ Data0 of
		Data when length(Data) >= 10 ->
		    wait_for_port_reply_cont2(Socket, Data);
		Data when length(Data) < 10 ->
		    wait_for_port_reply_cont(Socket, Data);
		Garbage ->
		    ?port_please_failure(),
		    {error, {garbage_from_epmd, Garbage}}
	    end;
	{tcp_closed, Socket} ->
	    ?port_please_failure(),
	    noport
    after 10000 ->
	    ?port_please_failure(),
	    gen_tcp:close(Socket),
	    noport
    end.

wait_for_port_reply_cont2(Socket, Data) ->
    [A, B, _Type, _Proto, HighA, HighB,
     LowA, LowB, NLenA, NLenB | Rest] = Data,
    wait_for_port_reply_name(Socket,
			     ?u16(NLenA, NLenB),
			     Rest),
    Low = ?u16(LowA, LowB),
    High = ?u16(HighA, HighB),
    Version = best_version(Low, High),
%    io:format("Returning ~p~n", [{port, ?u16(A, B), Version}]),
    {port, ?u16(A, B), Version}.
%    {port, ?u16(A, B)}.

%%% Throw away the rest of the message; we won't use any of it anyway,
%%% currently.
wait_for_port_reply_name(Socket, Len, Sofar) ->
    receive
	{tcp, Socket, _Data} ->
%	    io:format("data = ~p~n", _Data),
	    wait_for_port_reply_name(Socket, Len, Sofar);
	{tcp_closed, Socket} ->
	    "foobar"
    end.
		    

best_version(Low, High) ->
    OurLow =  epmd_dist_low(),
    OurHigh =  epmd_dist_high(),
    select_best_version(OurLow, OurHigh, Low, High).

%%% We silently assume that the low's are not greater than the high's.
%%% We should report if the intervals don't overlap.
select_best_version(L1, _H1, _L2, H2) when L1 > H2 ->
    0;
select_best_version(_L1, H1, L2, _H2) when L2 > H1 ->
    0;
select_best_version(_L1, H1, L2, _H2) when L2 > H1 ->
    0;
select_best_version(_L1, H1, _L2, H2) ->
    erlang:min(H1, H2).

wait_for_close(Socket, Reply) ->
    receive
	{tcp_closed, Socket} -> 
	    Reply
    after 10000 ->
	    gen_tcp:close(Socket),
	    Reply
    end.


%%
%% Creates a (flat) null terminated string from atom or list.
%%

to_string(S) when is_atom(S) -> atom_to_list(S);
to_string(S) when is_list(S) -> S.

%%
%% Find names on epmd
%%
%%
get_names(EpmdAddress) ->
    case open(EpmdAddress) of
	{ok, Socket} ->
	    do_get_names(Socket);
	_Error ->
	    {error, address}
    end.

do_get_names(Socket) ->
    gen_tcp:send(Socket, [?int16(1),?EPMD_NAMES]),
    receive
	{tcp, Socket, [P0,P1,P2,P3|T]} ->
	    EpmdPort = ?u32(P0,P1,P2,P3),
	    case get_epmd_port() of
		EpmdPort ->
		    names_loop(Socket, T, []);
		_ ->
		    close(Socket),
		    {error, address}
	    end;
	{tcp_closed, Socket} ->
	    {ok, []}
    end.

names_loop(Socket, Acc, Ps) ->
    receive
	{tcp, Socket, Bytes} ->
	    {NAcc, NPs} = scan_names(Acc ++ Bytes, Ps),
	    names_loop(Socket, NAcc, NPs);
	{tcp_closed, Socket} ->
	    {_, NPs} = scan_names(Acc, Ps),
	    {ok, NPs}
    end.

scan_names(Buf, Ps) ->
    case scan_line(Buf, []) of
	{Line, NBuf} ->
	    case parse_line(Line) of
		{ok, Entry} -> 
		    scan_names(NBuf, [Entry | Ps]);
		error ->
		    scan_names(NBuf, Ps)
	    end;
	[] -> {Buf, Ps}
    end.


scan_line([$\n | Buf], Line) -> {reverse(Line), Buf};
scan_line([C | Buf], Line) -> scan_line(Buf, [C|Line]);
scan_line([], _) -> [].

parse_line("name " ++ Buf0) ->
    case parse_name(Buf0, []) of
	{Name, Buf1}  ->
	    case Buf1 of
		"at port " ++ Buf2 ->
		    case catch list_to_integer(Buf2) of
			{'EXIT', _} -> error;
			Port -> {ok, {Name, Port}}
		    end;
		_ -> error
	    end;
	error -> error
    end;
parse_line(_) -> error.


parse_name([$\s | Buf], Name) -> {reverse(Name), Buf};
parse_name([C | Buf], Name) -> parse_name(Buf, [C|Name]);
parse_name([], _Name) -> error.
