%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2017. All Rights Reserved.
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
%% Test the behavior of gen_udp. Testing udp is really a very unfunny task,
%% because udp is not deterministic.
%%
-module(gen_udp_SUITE).
-include_lib("common_test/include/ct.hrl").


%% XXX - we should pick a port that we _know_ is closed. That's pretty hard.
-define(CLOSED_PORT, 6666).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([send_to_closed/1, active_n/1,
	 buffer_size/1, binary_passive_recv/1, bad_address/1,
	 read_packets/1, open_fd/1, connect/1, implicit_inet6/1,
	 local_basic/1, local_unbound/1,
	 local_fdopen/1, local_fdopen_unbound/1, local_abstract/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [send_to_closed, buffer_size, binary_passive_recv,
     bad_address, read_packets, open_fd, connect,
     implicit_inet6, active_n,
     {group, local}].

groups() -> 
    [{local, [],
      [local_basic, local_unbound,
       local_fdopen, local_fdopen_unbound, local_abstract]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(local, Config) ->
    case gen_udp:open(0, [local]) of
	{ok,S} ->
	    ok = gen_udp:close(S),
	    Config;
	{error,eafnosupport} ->
	    {skip, "AF_LOCAL not supported"}
    end;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(local, _Config) ->
    delete_local_filenames();
end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%%-------------------------------------------------------------
%% Send two packets to a closed port (on some systems this causes the socket
%% to be closed).

%% Tests core functionality.
send_to_closed(Config) when is_list(Config) ->
    {ok, Sock} = gen_udp:open(0),
    ok = gen_udp:send(Sock, {127,0,0,1}, ?CLOSED_PORT, "foo"),
    timer:sleep(2),
    ok = gen_udp:send(Sock, {127,0,0,1}, ?CLOSED_PORT, "foo"),
    ok = gen_udp:close(Sock),
    ok.



%%-------------------------------------------------------------
%% Test that the UDP socket buffer sizes are settable

%% Test UDP buffer size setting.
buffer_size(Config) when is_list(Config) ->
    Len = 256,
    Bin = list_to_binary(lists:seq(0, Len-1)),
    M = 8192 div Len,
    Spec0 =
	[{opt,M},{safe,M-3},{long,M+1},
	 {opt,2*M},{safe,2*M-3},{long,2*M+1},
	 {opt,4*M},{safe,4*M-3},{long,4*M+1}],
    Spec =
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
    {ok, ClientSocket}  = gen_udp:open(0, [binary]),
    {ok, ClientPort} = inet:port(ClientSocket),
    Client = self(),
    ClientIP = {127,0,0,1},
    ServerIP = {127,0,0,1},
    Server =
	spawn_link(
	  fun () -> 
		  {ok, ServerSocket}  = gen_udp:open(0, [binary]),
		  {ok, ServerPort} = inet:port(ServerSocket),
		  Client ! {self(),port,ServerPort},
		  buffer_size_server(Client, ClientIP, ClientPort, 
				     ServerSocket, 1, Spec),
		  ok = gen_udp:close(ServerSocket)
	  end),
    Mref = erlang:monitor(process, Server),
    receive
	{Server,port,ServerPort} ->
	    buffer_size_client(Server, ServerIP, ServerPort,
			       ClientSocket, 1, Spec)
    end,
    ok = gen_udp:close(ClientSocket),
    receive
	{'DOWN',Mref,_,_,normal} ->
	    ok
    end.

buffer_size_client(_, _, _, _, _, []) ->
    ok;
buffer_size_client(Server, IP, Port, 
		   Socket, Cnt, [Opts|T]) when is_list(Opts) ->
    io:format("buffer_size_client Cnt=~w setopts ~p.~n", [Cnt,Opts]),
    ok = inet:setopts(Socket, Opts),
    Server ! {self(),setopts,Cnt},
    receive {Server,setopts,Cnt} -> ok end,
    buffer_size_client(Server, IP, Port, Socket, Cnt+1, T);
buffer_size_client(Server, IP, Port, 
		   Socket, Cnt, [{B,Replies}|T]=Opts) when is_binary(B) ->
    io:format(
      "buffer_size_client Cnt=~w send size ~w expecting ~p.~n",
      [Cnt,size(B),Replies]),
    ok = gen_udp:send(Socket, IP, Port, <<Cnt,B/binary>>),
    receive
	{Server,Cnt,Reply} ->
	    Tag =
		if
		    is_tuple(Reply) ->
			element(1, Reply);
		    is_atom(Reply) ->
			Reply
		end,
	    case lists:member(Tag, Replies) of
		true -> ok;
		false ->
		    ct:fail({reply_mismatch,Cnt,Reply,Replies,
			     byte_size(B),
			     inet:getopts(Socket,
					  [sndbuf,recbuf])})
	    end,
	    buffer_size_client(Server, IP, Port, Socket, Cnt+1, T)
    after 1313 ->
	    buffer_size_client(Server, IP, Port, Socket, Cnt, Opts)
    end.

buffer_size_server(_, _, _, _, _, []) -> 
    ok;
buffer_size_server(Client, IP, Port, 
		   Socket, Cnt, [Opts|T]) when is_list(Opts) ->
    receive {Client,setopts,Cnt} -> ok end,
    io:format("buffer_size_server Cnt=~w setopts ~p.~n", [Cnt,Opts]),
    ok = inet:setopts(Socket, Opts),
    Client ! {self(),setopts,Cnt},
    buffer_size_server(Client, IP, Port, Socket, Cnt+1, T);
buffer_size_server(Client, IP, Port, 
		   Socket, Cnt, [{B,_}|T]) when is_binary(B) ->
    io:format(
      "buffer_size_server Cnt=~w expecting size ~w.~n",
      [Cnt,size(B)]),
    Client ! 
	{self(),Cnt,
	 case buffer_size_server_recv(Socket, IP, Port, Cnt) of
	     D when is_binary(D) ->
		 SizeD = byte_size(D),
		 io:format(
		   "buffer_size_server Cnt=~w received size ~w.~n",
		   [Cnt,SizeD]),
		 case B of
		     D ->
			 correct;
		     <<D:SizeD/binary,_/binary>> ->
			 truncated;
		     _ ->
			 {unexpected,D}
		 end;
	     Error ->
		 io:format(
		   "buffer_size_server Cnt=~w received error ~w.~n",
		   [Cnt,Error]),
		 Error
	 end},
    buffer_size_server(Client, IP, Port, Socket, Cnt+1, T).

buffer_size_server_recv(Socket, IP, Port, Cnt) ->
    receive
	{udp,Socket,IP,Port,<<Cnt,B/binary>>} ->
	    B;
	{udp,Socket,IP,Port,<<_/binary>>} ->
	    buffer_size_server_recv(Socket, IP, Port, Cnt);
	{udp_error,Socket,Error} ->
	    Error
    after 5000 ->
	    {timeout,flush()}
    end.



%%-------------------------------------------------------------
%% OTP-3823 gen_udp:recv does not return address in binary mode
%%

%% OTP-3823 gen_udp:recv does not return address in binary mode.
binary_passive_recv(Config) when is_list(Config) ->
    D1       = "The quick brown fox jumps over a lazy dog",
    D2       = list_to_binary(D1),
    D3       = ["The quick", <<" brown ">>, "fox jumps ", <<"over ">>,
		<<>>, $a, [[], " lazy ", <<"dog">>]],
    D2       = iolist_to_binary(D3),
    B        = D2,
    {ok, R}  = gen_udp:open(0, [binary, {active, false}]),
    {ok, RP} = inet:port(R),
    {ok, S}  = gen_udp:open(0),
    {ok, SP} = inet:port(S),
    ok       = gen_udp:send(S, localhost, RP, D1),
    {ok, {{127, 0, 0, 1}, SP, B}} = gen_udp:recv(R, byte_size(B)+1),
    ok       = gen_udp:send(S, localhost, RP, D2),
    {ok, {{127, 0, 0, 1}, SP, B}} = gen_udp:recv(R, byte_size(B)+1),
    ok       = gen_udp:send(S, localhost, RP, D3),
    {ok, {{127, 0, 0, 1}, SP, B}} = gen_udp:recv(R, byte_size(B)+1),
    ok       = gen_udp:close(S),
    ok       = gen_udp:close(R),
    ok.


%%-------------------------------------------------------------
%% OTP-3836 inet_udp crashes when IP-address is larger than 255.

%% OTP-3836 inet_udp crashes when IP-address is larger than 255.
bad_address(Config) when is_list(Config) ->
    {ok, R}  = gen_udp:open(0),
    {ok, RP} = inet:port(R),
    {ok, S}  = gen_udp:open(0),
    {ok, _SP} = inet:port(S),
    {'EXIT', badarg} =
	(catch gen_udp:send(S, {127,0,0,1,0}, RP, "void")),
    {'EXIT', badarg} =
	(catch gen_udp:send(S, {127,0,0,256}, RP, "void")),
    ok       = gen_udp:close(S),
    ok       = gen_udp:close(R),
    ok.


%%-------------------------------------------------------------
%% OTP-6249 UDP option for number of packet reads
%%
%% Starts a slave node that on command sends a bunch of messages
%% to our UDP port. The receiving process just receives and
%% ignores the incoming messages.
%% A tracing process traces the receiving port for
%% 'send' and scheduling events. From the trace,
%% how many messages are received per in/out scheduling,
%% which should never be more than the read_packet parameter.

%% OTP-6249 UDP option for number of packet reads.
read_packets(Config) when is_list(Config) ->
    N1 = 5,
    N2 = 1,
    Msgs = 30000,
    {ok,R} = gen_udp:open(0, [{read_packets,N1}]),
    {ok,RP} = inet:port(R),
    {ok,Node} = start_node(gen_udp_SUITE_read_packets),
    Die = make_ref(),
    %%
    {V1, Trace1} = read_packets_test(R, RP, Msgs, Node),
    {ok,[{read_packets,N1}]} = inet:getopts(R, [read_packets]),
    %%
    ok = inet:setopts(R, [{read_packets,N2}]),
    {V2, Trace2} = read_packets_test(R, RP, Msgs, Node),
    {ok,[{read_packets,N2}]} = inet:getopts(R, [read_packets]),
    %%
    stop_node(Node),
    ct:log("N1=~p, V1=~p vs N2=~p, V2=~p",[N1,V1,N2,V2]),

    dump_terms(Config, "trace1.terms", Trace2),
    dump_terms(Config, "trace2.terms", Trace2),

    %% Because of the inherit racy-ness of the feature it is
    %% hard to test that it behaves correctly.
    %% Right now (OTP 21) a port task takes 5% of the
    %% allotted port task reductions to execute, so
    %% the max number of executions a port is allowed to
    %% do before being re-scheduled is N * 20

    if
        V1 > (N1 * 20) ->
            ct:fail("Got ~p msgs, max was ~p", [V1, N1]);
        V2 > (N2 * 20) ->
            ct:fail("Got ~p msgs, max was ~p", [V2, N2]);
        true ->
            ok
    end.

dump_terms(Config, Name, Terms) ->
    FName = filename:join(proplists:get_value(priv_dir, Config),Name),
    file:write_file(FName, term_to_binary(Terms)),
    ct:log("Logged terms to ~s",[FName]).

infinite_loop(Die) ->
    receive 
	Die ->
	    ok
    after
	0 ->
	    infinite_loop(Die)
    end.

read_packets_test(R, RP, Msgs, Node) ->
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
	    erlang:trace(R, true,
			 [running_ports,'send',{tracer,Tracer}]),
	    erlang:yield(),
	    Sender ! {Receiver,go},
	    read_packets_recv(Msgs),
	    erlang:trace(R, false, [all]),
	    Tracer ! {Receiver,get_trace},
	    receive
		{Tracer,{trace,Trace}} ->
		    {read_packets_verify(R, SP, Trace), Trace}
	    end
    end.

read_packets_send(_S, _RP, 0) ->
    ok;
read_packets_send(S, RP, Msgs) ->
    ok = gen_udp:send(S, localhost, RP, "UDP FLOOOOOOD"),
    read_packets_send(S, RP, Msgs - 1).

read_packets_recv(0) ->
    ok;
read_packets_recv(N) ->
    receive
	_ ->
	    read_packets_recv(N - 1)
    after 5000 ->
	    timeout
    end.

read_packets_verify(R, SP, Trace) ->
    [Max | _] = Pkts = lists:reverse(lists:sort(read_packets_verify(R, SP, Trace, 0))),
    ct:pal("~p",[lists:sublist(Pkts,10)]),
    Max.

read_packets_verify(R, SP, [{trace,R,OutIn,_}|Trace], M) 
  when OutIn =:= out; OutIn =:= in ->
    push(M, read_packets_verify(R, SP, Trace, 0));
read_packets_verify(R, SP, [{trace, R,'receive',timeout}|Trace], M) ->
    push(M, read_packets_verify(R, SP, Trace, 0));
read_packets_verify(R, SP,
		    [{trace,R,'send',{udp,R,{127,0,0,1},SP,_Msg}, Self} | Trace], M)
  when Self =:= self() ->
    read_packets_verify(R, SP, Trace, M+1);
read_packets_verify(_R, _SP, [], M) ->
    push(M, []);
read_packets_verify(_R, _SP, Trace, M) ->
    ct:fail({read_packets_verify,mismatch,Trace,M}).

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



%% Test that the 'fd' option works.
open_fd(Config) when is_list(Config) ->
    Msg = "Det gör ont när knoppar brista. Varför skulle annars våren tveka?",
    Addr = {127,0,0,1},
    {ok,S1}   = gen_udp:open(0),
    {ok,P2} = inet:port(S1),
    {ok,FD}   = prim_inet:getfd(S1),
    {error,einval} = gen_udp:open(0, [inet6, {fd,FD}]),
    {ok,S2}   = gen_udp:open(0, [{fd,FD}]),
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
		    ct:fail(io_lib:format("~w", [flush()]))
	    end
    after 1000 ->
	    ct:fail(io_lib:format("~w", [flush()]))
    end.

active_n(Config) when is_list(Config) ->
    N = 3,
    S1 = ok(gen_udp:open(0, [{active,N}])),
    [{active,N}] = ok(inet:getopts(S1, [active])),
    ok = inet:setopts(S1, [{active,-N}]),
    receive
        {udp_passive, S1} -> ok
    after
        5000 ->
            exit({error,udp_passive_failure})
    end,
    [{active,false}] = ok(inet:getopts(S1, [active])),
    ok = inet:setopts(S1, [{active,0}]),
    receive
        {udp_passive, S1} -> ok
    after
        5000 ->
            exit({error,udp_passive_failure})
    end,
    ok = inet:setopts(S1, [{active,32767}]),
    {error,einval} = inet:setopts(S1, [{active,1}]),
    {error,einval} = inet:setopts(S1, [{active,-32769}]),
    ok = inet:setopts(S1, [{active,-32768}]),
    receive
        {udp_passive, S1} -> ok
    after
        5000 ->
            exit({error,udp_passive_failure})
    end,
    [{active,false}] = ok(inet:getopts(S1, [active])),
    ok = inet:setopts(S1, [{active,N}]),
    ok = inet:setopts(S1, [{active,true}]),
    [{active,true}] = ok(inet:getopts(S1, [active])),
    receive
        _ -> exit({error,active_n})
    after
        0 ->
            ok
    end,
    ok = inet:setopts(S1, [{active,N}]),
    ok = inet:setopts(S1, [{active,once}]),
    [{active,once}] = ok(inet:getopts(S1, [active])),
    receive
        _ -> exit({error,active_n})
    after
        0 ->
            ok
    end,
    {error,einval} = inet:setopts(S1, [{active,32768}]),
    ok = inet:setopts(S1, [{active,false}]),
    [{active,false}] = ok(inet:getopts(S1, [active])),
    S1Port = ok(inet:port(S1)),
    S2 = ok(gen_udp:open(0, [{active,N}])),
    S2Port = ok(inet:port(S2)),
    [{active,N}] = ok(inet:getopts(S2, [active])),
    ok = inet:setopts(S1, [{active,N}]),
    [{active,N}] = ok(inet:getopts(S1, [active])),
    lists:foreach(
      fun(I) ->
              Msg = "message "++integer_to_list(I),
              ok = gen_udp:send(S2, "localhost", S1Port, Msg),
              receive
                  {udp,S1,_,S2Port,Msg} ->
                      ok = gen_udp:send(S1, "localhost", S2Port, Msg)
              after
                  5000 ->
                      exit({error,timeout})
              end,
              receive
                  {udp,S2,_,S1Port,Msg} ->
                      ok
              after
                  5000 ->
                      exit({error,timeout})
              end
      end, lists:seq(1,N)),
    receive
        {udp_passive,S1} ->
            [{active,false}] = ok(inet:getopts(S1, [active]))
    after
        5000 ->
            exit({error,udp_passive})
    end,
    receive
        {udp_passive,S2} ->
            [{active,false}] = ok(inet:getopts(S2, [active]))
    after
        5000 ->
            exit({error,udp_passive})
    end,
    S3 = ok(gen_udp:open(0, [{active,0}])),
    receive
        {udp_passive,S3} ->
            [{active,false}] = ok(inet:getopts(S3, [active]))
    after
        5000 ->
            exit({error,udp_passive})
    end,
    ok = gen_udp:close(S3),
    ok = gen_udp:close(S2),
    ok = gen_udp:close(S1),
    ok.


local_basic(_Config) ->
    SFile = local_filename(server),
    SAddr = {local,bin_filename(SFile)},
    CFile = local_filename(client),
    CAddr = {local,bin_filename(CFile)},
    _ = file:delete(SFile),
    _ = file:delete(CFile),
    %%
    S = ok(gen_udp:open(0, [{ifaddr,{local,SFile}},{active,false}])),
    C = ok(gen_udp:open(0, [{ifaddr,{local,CFile}},{active,false}])),
    SAddr = ok(inet:sockname(S)),
    CAddr = ok(inet:sockname(C)),
    local_handshake(S, SAddr, C, CAddr),
    ok = gen_udp:close(S),
    ok = gen_udp:close(C),
    %%
    ok = file:delete(SFile),
    ok = file:delete(CFile),
    ok.

local_unbound(_Config) ->
    SFile = local_filename(server),
    SAddr = {local,bin_filename(SFile)},
    _ = file:delete(SFile),
    %%
    S = ok(gen_udp:open(0, [{ifaddr,SAddr},{active,false}])),
    C = ok(gen_udp:open(0, [local,{active,false}])),
    SAddr = ok(inet:sockname(S)),
    local_handshake(S, SAddr, C, undefined),
    ok = gen_udp:close(S),
    ok = gen_udp:close(C),
    %%
    ok = file:delete(SFile),
    ok.

local_fdopen(_Config) ->
    SFile = local_filename(server),
    SAddr = {local,bin_filename(SFile)},
    CFile = local_filename(client),
    CAddr = {local,bin_filename(CFile)},
    _ = file:delete(SFile),
    _ = file:delete(CFile),
    %%
    S0 = ok(gen_udp:open(0, [{ifaddr,SAddr},{active,false}])),
    C = ok(gen_udp:open(0, [{ifaddr,{local,CFile}},{active,false}])),
    SAddr = ok(inet:sockname(S0)),
    CAddr = ok(inet:sockname(C)),
    Fd = ok(prim_inet:getfd(S0)),
    S = ok(gen_udp:open(0, [{fd,Fd},local,{active,false}])),
    SAddr = ok(inet:sockname(S)),
    local_handshake(S, SAddr, C, CAddr),
    ok = gen_udp:close(S),
    ok = gen_udp:close(S0),
    ok = gen_udp:close(C),
    %%
    ok = file:delete(SFile),
    ok = file:delete(CFile),
    ok.

local_fdopen_unbound(_Config) ->
    SFile = local_filename(server),
    SAddr = {local,bin_filename(SFile)},
    _ = file:delete(SFile),
    %%
    S = ok(gen_udp:open(0, [{ifaddr,SAddr},{active,false}])),
    C0 = ok(gen_udp:open(0, [local,{active,false}])),
    SAddr = ok(inet:sockname(S)),
    Fd = ok(prim_inet:getfd(C0)),
    C = ok(gen_udp:open(0, [{fd,Fd},local,{active,false}])),
    local_handshake(S, SAddr, C, undefined),
    ok = gen_udp:close(S),
    ok = gen_udp:close(C),
    ok = gen_udp:close(C0),
    %%
    ok = file:delete(SFile),
    ok.

local_abstract(_Config) ->
    case os:type() of
	{unix,linux} ->
	    S = ok(gen_udp:open(0, [{ifaddr,{local,<<>>}},{active,false}])),
	    C = ok(gen_udp:open(0, [{ifaddr,{local,<<>>}},{active,false}])),
	    {local,_} = SAddr = ok(inet:sockname(S)),
	    {local,_} = CAddr = ok(inet:sockname(C)),
	    local_handshake(S, SAddr, C, CAddr),
	    ok = gen_udp:close(S),
	    ok = gen_udp:close(C),
	    ok;
	_ ->
	    {skip,"AF_LOCAL Abstract Addresses only supported on Linux"}
    end.


local_handshake(S, SAddr, C, CAddr) ->
    SData = "9876543210",
    CData = "0123456789",
    ok = gen_udp:send(C, SAddr, 0, CData),
    case ok(gen_tcp:recv(S, 112)) of
	{{unspec,<<>>}, 0, CData} when CAddr =:= undefined ->
	    ok;
	{{local,<<>>}, 0, CData} when CAddr =:= undefined ->
	    ok;
	{CAddr, 0, CData} when CAddr =/= undefined ->
	    ok = gen_udp:send(S, CAddr, 0, SData),
	    {SAddr, 0, SData} = ok(gen_tcp:recv(C, 112)),
	    ok

    end.

%%
%% Utils
%%

start_node(Name) ->
    Pa = filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, slave, [{args, "-pa " ++ Pa}]).

stop_node(Node) ->
    test_server:stop_node(Node).


%% Test that connect/3 has effect.
connect(Config) when is_list(Config) ->
    Addr = {127,0,0,1},
    {ok,S1} = gen_udp:open(0),
    {ok,P1} = inet:port(S1),
    {ok,S2} = gen_udp:open(0),
    ok = inet:setopts(S2, [{active,false}]),
    ok = gen_udp:close(S1),
    ok = gen_udp:connect(S2, Addr, P1),
    ok = gen_udp:send(S2, <<16#deadbeef:32>>),
    ok = case gen_udp:recv(S2, 0, 500) of
	     {error,econnrefused} -> ok;
	     {error,econnreset} -> ok;
	     Other -> Other
	 end,
    ok.

implicit_inet6(Config) when is_list(Config) ->
    Host = ok(inet:gethostname()),
    case inet:getaddr(Host, inet6) of
	{ok,Addr} ->
	    implicit_inet6(Host, Addr);
	{error,Reason} ->
	    {skip,
	     "Can not look up IPv6 address: "
	     ++atom_to_list(Reason)}
    end.

implicit_inet6(Host, Addr) ->
    Active = {active,false},
    Loopback = {0,0,0,0,0,0,0,1},
    case gen_udp:open(0, [inet6,Active,{ip, Loopback}]) of
	{ok,S1} ->
	    io:format("~s ~p~n", ["::1",Loopback]),
	    implicit_inet6(S1, Active, Loopback),
	    ok = gen_udp:close(S1),
	    %%
	    Localaddr = ok(get_localaddr()),
	    S2 = ok(gen_udp:open(0, [{ip,Localaddr},Active])),
	    implicit_inet6(S2, Active, Localaddr),
	    ok = gen_udp:close(S2),
	    %%
	    io:format("~s ~p~n", [Host,Addr]),
	    S3 = ok(gen_udp:open(0, [{ifaddr,Addr},Active])),
	    implicit_inet6(S3, Active, Addr),
	    ok = gen_udp:close(S3);
	_ ->
	    {skip,"IPv6 not supported"}
    end.

implicit_inet6(S1, Active, Addr) ->
    P1 = ok(inet:port(S1)),
    S2 = ok(gen_udp:open(0, [inet6,Active])),
    P2 = ok(inet:port(S2)),
    ok = gen_udp:connect(S2, Addr, P1),
    ok = gen_udp:connect(S1, Addr, P2),
    {Addr,P2} = ok(inet:peername(S1)),
    {Addr,P1} = ok(inet:peername(S2)),
    {Addr,P1} = ok(inet:sockname(S1)),
    {Addr,P2} = ok(inet:sockname(S2)),
    ok = gen_udp:send(S1, Addr, P2, "ping"),
    {Addr,P1,"ping"} = ok(gen_udp:recv(S2, 1024, 1000)),
    ok = gen_udp:send(S2, Addr, P1, "pong"),
    {Addr,P2,"pong"} = ok(gen_udp:recv(S1, 1024)),
    ok = gen_udp:close(S2).

ok({ok,V}) -> V;
ok(NotOk) ->
    try throw(not_ok)
    catch
	throw:Thrown:Stacktrace ->
	    erlang:raise(
	      error, {Thrown, NotOk}, tl(Stacktrace))
    end.


local_filename(Tag) ->
    "/tmp/" ?MODULE_STRING "_" ++ os:getpid() ++ "_" ++ atom_to_list(Tag).

bin_filename(String) ->
    unicode:characters_to_binary(String, file:native_name_encoding()).

delete_local_filenames() ->
    _ =
	[file:delete(F) ||
	    F <-
		filelib:wildcard(
		  "/tmp/" ?MODULE_STRING "_" ++ os:getpid() ++ "_*")],
    ok.

get_localaddr() ->
    get_localaddr(["localhost", "localhost6", "ip6-localhost"]).

get_localaddr([]) ->
    {error, localaddr_not_found};
get_localaddr([Localhost|Ls]) ->
    case inet:getaddr(Localhost, inet6) of
       {ok, LocalAddr} ->
           io:format("~s ~p~n", [Localhost, LocalAddr]),
           {ok, LocalAddr};
       _ ->
           get_localaddr(Ls)
    end.
