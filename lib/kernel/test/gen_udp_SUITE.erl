%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2011. All Rights Reserved.
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
%
% test the behavior of gen_udp. Testing udp is really a very unfunny task,
% because udp is not deterministic.
%
-module(gen_udp_SUITE).
-include_lib("test_server/include/test_server.hrl").


-define(default_timeout, ?t:minutes(1)).

% XXX - we should pick a port that we _know_ is closed. That's pretty hard.
-define(CLOSED_PORT, 6666).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([send_to_closed/1, 
	 buffer_size/1, binary_passive_recv/1, bad_address/1,
	 read_packets/1, open_fd/1, connect/1, implicit_inet6/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [send_to_closed, buffer_size, binary_passive_recv,
     bad_address, read_packets, open_fd, connect,
     implicit_inet6].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Case, Config) ->
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%-------------------------------------------------------------
%% Send two packets to a closed port (on some systems this causes the socket
%% to be closed).

send_to_closed(doc) ->
    ["Tests core functionality."];
send_to_closed(suite) ->
    [];
send_to_closed(Config) when is_list(Config) ->
    ?line {ok, Sock} = gen_udp:open(0),
    ?line ok = gen_udp:send(Sock, {127,0,0,1}, ?CLOSED_PORT, "foo"),
    timer:sleep(2),
    ?line ok = gen_udp:send(Sock, {127,0,0,1}, ?CLOSED_PORT, "foo"),
    ?line ok = gen_udp:close(Sock),
    ok.



%%-------------------------------------------------------------
%% Test that the UDP socket buffer sizes are settable

buffer_size(suite) ->
    [];
buffer_size(doc) ->
    ["Test UDP buffer size setting."];
buffer_size(Config) when is_list(Config) ->
    ?line Len = 256,
    ?line Bin = list_to_binary(lists:seq(0, Len-1)),
    ?line M = 8192 div Len,
    ?line Spec0 =
	[{opt,M},{safe,M-1},{long,M+1},
	 {opt,2*M},{safe,2*M-1},{long,2*M+1},
	 {opt,4*M},{safe,4*M-1},{long,4*M+1}],
    ?line Spec =
	[case Tag of
	     opt ->
		 [{recbuf,Val*Len},{sndbuf,(Val + 2)*Len}];
	     safe ->
		 {list_to_binary(lists:duplicate(Val, Bin)),
		  [correct]};
	     long ->
		 {list_to_binary(lists:duplicate(Val, Bin)),
		  [truncated,emsgsize,timeout]}
	 end || {Tag,Val} <- Spec0],
    %%
    ?line {ok, ClientSocket}  = gen_udp:open(0, [binary]),
    ?line {ok, ClientPort} = inet:port(ClientSocket),
    ?line Client = self(),
    ?line ClientIP = {127,0,0,1},
    ?line ServerIP = {127,0,0,1},
    ?line Server =
	spawn_link(
	  fun () -> 
		  {ok, ServerSocket}  = gen_udp:open(0, [binary]),
		  {ok, ServerPort} = inet:port(ServerSocket),
		  Client ! {self(),port,ServerPort},
		  buffer_size_server(Client, ClientIP, ClientPort, 
				     ServerSocket, 1, Spec),
		  ok = gen_udp:close(ServerSocket)
	  end),
    ?line Mref = erlang:monitor(process, Server),
    ?line receive
	      {Server,port,ServerPort} ->
		  ?line buffer_size_client(Server, ServerIP, ServerPort,
					   ClientSocket, 1, Spec)
	  end,
    ?line ok = gen_udp:close(ClientSocket),
    ?line receive
	      {'DOWN',Mref,_,_,normal} ->
		  ?line ok
	  end.

buffer_size_client(_, _, _, _, _, []) ->
    ?line ok;
buffer_size_client(Server, IP, Port, 
		   Socket, Cnt, [Opts|T]) when is_list(Opts) ->
    ?line ok = inet:setopts(Socket, Opts),
    ?line Server ! {self(),setopts,Cnt},
    ?line receive {Server,setopts,Cnt} -> ok end,
    ?line buffer_size_client(Server, IP, Port, Socket, Cnt+1, T);
buffer_size_client(Server, IP, Port, 
		   Socket, Cnt, [{B,Replies}|T]) when is_binary(B) ->
    ?line ok = gen_udp:send(Socket, IP, Port, B),
    ?line receive
	      {Server,Cnt,Reply} ->
		  ?line case lists:member(Reply, Replies) of
			    true -> ok;
			    false ->
				?line 
				    ?t:fail({reply_mismatch,Cnt,Reply,Replies,
					     byte_size(B),
					     inet:getopts(Socket,
							  [sndbuf,recbuf])})
			end
	  end,
    ?line buffer_size_client(Server, IP, Port, Socket, Cnt+1, T).

buffer_size_server(_, _, _, _, _, []) -> 
    ok;
buffer_size_server(Client, IP, Port, 
		   Socket, Cnt, [Opts|T]) when is_list(Opts) ->
    receive {Client,setopts,Cnt} -> ok end,
    ok = inet:setopts(Socket, Opts),
    Client ! {self(),setopts,Cnt},
    buffer_size_server(Client, IP, Port, Socket, Cnt+1, T);
buffer_size_server(Client, IP, Port, 
		   Socket, Cnt, [{B,_}|T]) when is_binary(B) ->
    Client ! 
	{self(),Cnt,
	 receive
	     {udp,Socket,IP,Port,D} when is_binary(D) ->
		 SizeD = byte_size(D),
		 case B of
		     D ->                             correct;
		     <<D:SizeD/binary,_/binary>> ->   truncated
		 end;
	     {udp_error,Socket,Error} ->              Error
	 after 5000 ->                                timeout
	 end},
    buffer_size_server(Client, IP, Port, Socket, Cnt+1, T).



%%-------------------------------------------------------------
%% OTP-3823 gen_udp:recv does not return address in binary mode
%%

binary_passive_recv(suite) ->
    [];
binary_passive_recv(doc) ->
    ["OTP-3823 gen_udp:recv does not return address in binary mode"];
binary_passive_recv(Config) when is_list(Config) ->
    ?line D        = "The quick brown fox jumps over a lazy dog",
    ?line B        = list_to_binary(D),
    ?line {ok, R}  = gen_udp:open(0, [binary, {active, false}]),
    ?line {ok, RP} = inet:port(R),
    ?line {ok, S}  = gen_udp:open(0),
    ?line {ok, SP} = inet:port(S),
    ?line ok       = gen_udp:send(S, localhost, RP, D),
    ?line {ok, {{127, 0, 0, 1}, SP, B}} = gen_udp:recv(R, byte_size(B)+1),
    ?line ok       = gen_udp:close(S),
    ?line ok       = gen_udp:close(R),
    ok.


%%-------------------------------------------------------------
%% OTP-3836 inet_udp crashes when IP-address is larger than 255.

bad_address(suite) ->
    [];
bad_address(doc) ->
    ["OTP-3836 inet_udp crashes when IP-address is larger than 255."];
bad_address(Config) when is_list(Config) ->
    ?line {ok, R}  = gen_udp:open(0),
    ?line {ok, RP} = inet:port(R),
    ?line {ok, S}  = gen_udp:open(0),
    ?line {ok, _SP} = inet:port(S),
    ?line {'EXIT', badarg} = 
	(catch gen_udp:send(S, {127,0,0,1,0}, RP, "void")),
    ?line {'EXIT', badarg} = 
	(catch gen_udp:send(S, {127,0,0,256}, RP, "void")),
    ?line ok       = gen_udp:close(S),
    ?line ok       = gen_udp:close(R),
    ok.


%%-------------------------------------------------------------
%% OTP-6249 UDP option for number of packet reads
%%
%% Starts a slave node that on command sends a bunch of messages
%% to our UDP port. The receiving process just receives and
%% ignores the incoming messages, but counts them.
%% A tracing process traces the receiving process for
%% 'receive' and scheduling events. From the trace, 
%% message contents is verified; and, how many messages
%% are received per in/out scheduling, which should be
%% the same as the read_packets parameter.
%% 
%% What happens on the SMP emulator remains to be seen...
%%

read_packets(doc) ->
    ["OTP-6249 UDP option for number of packet reads."];
read_packets(Config) when is_list(Config) ->
    case erlang:system_info(smp_support) of
	false ->
	    read_packets_1();
	true ->
	    %% We would need some new sort of tracing to test this
	    %% option reliably in an SMP emulator.
	    {skip,"SMP emulator"}
    end.

read_packets_1() ->
    ?line N1 = 5,
    ?line N2 = 7,
    ?line {ok,R} = gen_udp:open(0, [{read_packets,N1}]),
    ?line {ok,RP} = inet:port(R),
    ?line {ok,Node} = start_node(gen_udp_SUITE_read_packets),
    ?line Die = make_ref(),
    ?line Loop = erlang:spawn_link(fun () -> infinite_loop(Die) end),
    %%
    ?line Msgs1 = [erlang:integer_to_list(M) || M <- lists:seq(1, N1*3)],
    ?line [V1|_] = read_packets_test(R, RP, Msgs1, Node),
    ?line {ok,[{read_packets,N1}]} = inet:getopts(R, [read_packets]),
    %%
    ?line ok = inet:setopts(R, [{read_packets,N2}]),
    ?line Msgs2 = [erlang:integer_to_list(M) || M <- lists:seq(1, N2*3)],
    ?line [V2|_] = read_packets_test(R, RP, Msgs2, Node),
    ?line {ok,[{read_packets,N2}]} = inet:getopts(R, [read_packets]),
    %%
    ?line stop_node(Node),
    ?line Mref = erlang:monitor(process, Loop),
    ?line Loop ! Die,
    ?line receive
	      {'DOWN',Mref,_,_, normal} ->
		  case {V1,V2} of
		      {N1,N2} ->
			  ok;
		      _ when V1 =/= N1, V2 =/= N2 ->
			  ok
		  end
	  end.

infinite_loop(Die) ->
    receive 
	Die ->
	    ok
    after
	0 ->
	    infinite_loop(Die)
    end.

read_packets_test(R, RP, Msgs, Node) ->
    Len = length(Msgs),
    Receiver = self(),
    Tracer =
	spawn_link(
	  fun () ->
		  receive
		      {Receiver,get_trace} ->
			  Receiver ! {self(),{trace,flush()}}
		  end
	  end),
    Sender =
	spawn_opt(
	  Node,
	  fun () ->
		  {ok,S}  = gen_udp:open(0),
		  {ok,SP} = inet:port(S),
		  Receiver ! {self(),{port,SP}},
		  receive
		      {Receiver,go} ->
			  read_packets_send(S, RP, Msgs)
		  end
	  end, 
	  [link,{priority,high}]),
    receive
	{Sender,{port,SP}} ->
	    erlang:trace(self(), true,
			 [running,'receive',{tracer,Tracer}]),
	    erlang:yield(),
	    Sender ! {Receiver,go},
	    read_packets_recv(Len),
	    erlang:trace(self(), false, [all]),
	    Tracer ! {Receiver,get_trace},
	    receive
		{Tracer,{trace,Trace}} ->
		    read_packets_verify(R, SP, Msgs, Trace)
	    end
    end.

read_packets_send(S, RP, [Msg|Msgs]) ->
    ok = gen_udp:send(S, localhost, RP, Msg),
    read_packets_send(S, RP, Msgs);
read_packets_send(_S, _RP, []) ->
    ok.

read_packets_recv(0) ->
    ok;
read_packets_recv(N) ->
    receive
	_ ->
	    read_packets_recv(N - 1)
    after 5000 ->
	    timeout
    end.

read_packets_verify(R, SP, Msg, Trace) ->    
    lists:reverse(
	  lists:sort(read_packets_verify(R, SP, Msg, Trace, 0))).
    
read_packets_verify(R, SP, Msgs, [{trace,Self,OutIn,_}|Trace], M) 
  when Self =:= self(), OutIn =:= out;
       Self =:= self(), OutIn =:= in ->
    push(M, read_packets_verify(R, SP, Msgs, Trace, 0));
read_packets_verify(R, SP, [Msg|Msgs],
		      [{trace,Self,'receive',{udp,R,{127,0,0,1},SP,Msg}}
		       |Trace], M) 
  when Self =:= self() ->
    read_packets_verify(R, SP, Msgs, Trace, M+1);
read_packets_verify(_R, _SP, [], [], M) ->
    push(M, []);
read_packets_verify(_R, _SP, Msgs, Trace, M) ->
    ?t:fail({read_packets_verify,mismatch,Msgs,Trace,M}).

push(0, Vs) ->
    Vs;
push(V, Vs) ->
    [V|Vs].

flush() ->
    receive
	X ->
	    [X|flush()]
    after 200 ->
	    []
    end.



open_fd(suite) ->
    [];
open_fd(doc) ->
    ["Test that the 'fd' option works"];
open_fd(Config) when is_list(Config) ->
    Msg = "Det gör ont när knoppar brista. Varför skulle annars våren tveka?",
    Addr = {127,0,0,1},
    {ok,S1}   = gen_udp:open(0),
    {ok,P2} = inet:port(S1),
    {ok,FD}   = prim_inet:getfd(S1),
    {error,einval} = gen_udp:open(P2, [inet6, {fd,FD}]),
    {ok,S2}   = gen_udp:open(P2, [{fd,FD}]),
    {ok,S3}   = gen_udp:open(0),
    {ok,P3} = inet:port(S3),
    ok = gen_udp:send(S3, Addr, P2, Msg),
    receive
	{udp,S2,Addr,P3,Msg} ->
	    ok = gen_udp:send(S2,Addr,P3,Msg),
	    receive
		{udp,S3,Addr,P2,Msg} ->
		    ok
	    after 1000 ->
		    ?t:fail(io_lib:format("~w", [flush()]))
	    end
    after 1000 ->
	    ?t:fail(io_lib:format("~w", [flush()]))
    end.


%
% Utils
%
start_node(Name) ->
    Pa = filename:dirname(code:which(?MODULE)),
    ?t:start_node(Name, slave, [{args, "-pa " ++ Pa}]).

stop_node(Node) ->
    ?t:stop_node(Node).


connect(suite) ->
    [];
connect(doc) ->
    ["Test that connect/3 has effect"];
connect(Config) when is_list(Config) ->
    ?line Addr = {127,0,0,1},
    ?line {ok,S1} = gen_udp:open(0),
    ?line {ok,P1} = inet:port(S1),
    ?line {ok,S2} = gen_udp:open(0),
    ?line ok = inet:setopts(S2, [{active,false}]),
    ?line ok = gen_udp:close(S1),
    ?line ok = gen_udp:connect(S2, Addr, P1),
    ?line ok = gen_udp:send(S2, <<16#deadbeef:32>>),
    ?line ok = case gen_udp:recv(S2, 0, 5) of
        {error,econnrefused} -> ok;
	{error,econnreset} -> ok;
	Other -> Other
    end,
    ok.

implicit_inet6(Config) when is_list(Config) ->
    ?line Host = ok(inet:gethostname()),
    ?line
	case inet:getaddr(Host, inet6) of
	    {ok,Addr} ->
		?line implicit_inet6(Host, Addr);
	    {error,Reason} ->
		{skip,
		 "Can not look up IPv6 address: "
		 ++atom_to_list(Reason)}
	end.

implicit_inet6(Host, Addr) ->
    ?line Active = {active,false},
    ?line
	case gen_udp:open(0, [inet6,Active]) of
	    {ok,S1} ->
		?line Loopback = {0,0,0,0,0,0,0,1},
		?line io:format("~s ~p~n", ["::1",Loopback]),
		?line implicit_inet6(S1, Active, Loopback),
		?line ok = gen_udp:close(S1),
		%%
		?line Localhost = "localhost",
		?line Localaddr = ok(inet:getaddr(Localhost, inet6)),
		?line io:format("~s ~p~n", [Localhost,Localaddr]),
		?line S2 = ok(gen_udp:open(0, [{ip,Localaddr},Active])),
		?line implicit_inet6(S2, Active, Localaddr),
		?line ok = gen_udp:close(S2),
		%%
		?line io:format("~s ~p~n", [Host,Addr]),
		?line S3 = ok(gen_udp:open(0, [{ifaddr,Addr},Active])),
		?line implicit_inet6(S3, Active, Addr),
		?line ok = gen_udp:close(S3);
	    _ ->
		{skip,"IPv6 not supported"}
	end.

implicit_inet6(S1, Active, Addr) ->
    ?line P1 = ok(inet:port(S1)),
    ?line S2 = ok(gen_udp:open(0, [inet6,Active])),
    ?line P2 = ok(inet:port(S2)),
    ?line ok = gen_udp:connect(S2, Addr, P1),
    ?line ok = gen_udp:connect(S1, Addr, P2),
    ?line {Addr,P2} = ok(inet:peername(S1)),
    ?line {Addr,P1} = ok(inet:peername(S2)),
    ?line {Addr,P1} = ok(inet:sockname(S1)),
    ?line {Addr,P2} = ok(inet:sockname(S2)),
    ?line ok = gen_udp:send(S1, Addr, P2, "ping"),
    ?line {Addr,P1,"ping"} = ok(gen_udp:recv(S2, 1024, 1000)),
    ?line ok = gen_udp:send(S2, Addr, P1, "pong"),
    ?line {Addr,P2,"pong"} = ok(gen_udp:recv(S1, 1024)),
    ?line ok = gen_udp:close(S2).

ok({ok,V}) -> V.
