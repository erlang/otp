%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2019. All Rights Reserved.
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
	 buffer_size/1, binary_passive_recv/1, max_buffer_size/1, bad_address/1,
	 read_packets/1, open_fd/1, connect/1, implicit_inet6/1,
         recvtos/1, recvtosttl/1, recvttl/1, recvtclass/1,
         sendtos/1, sendtosttl/1, sendttl/1, sendtclass/1,
	 local_basic/1, local_unbound/1,
	 local_fdopen/1, local_fdopen_unbound/1, local_abstract/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [send_to_closed, buffer_size, binary_passive_recv, max_buffer_size,
     bad_address, read_packets, open_fd, connect,
     implicit_inet6, active_n,
     recvtos, recvtosttl, recvttl, recvtclass,
     sendtos, sendtosttl, sendttl, sendtclass,
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
%% OTP-15206: Keep buffer small for udp
%%-------------------------------------------------------------
max_buffer_size(Config) when is_list(Config) ->
    {ok, Socket}  = gen_udp:open(0, [binary]),
    ok = inet:setopts(Socket,[{recbuf, 1 bsl 20}]),
    {ok, [{buffer, 65536}]} = inet:getopts(Socket,[buffer]),
    gen_udp:close(Socket).

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

    dump_terms(Config, "trace1.terms", Trace1),
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



recvtos(_Config) ->
    test_recv_opts(
      inet, [{recvtos,tos,96}], false,
      fun recvtos_ok/2).

recvtosttl(_Config) ->
    test_recv_opts(
      inet, [{recvtos,tos,96},{recvttl,ttl,33}], false,
      fun (OSType, OSVer) ->
              recvtos_ok(OSType, OSVer) andalso recvttl_ok(OSType, OSVer)
      end).

recvttl(_Config) ->
    test_recv_opts(
      inet, [{recvttl,ttl,33}], false,
      fun recvttl_ok/2).

recvtclass(_Config) ->
    {ok,IFs} = inet:getifaddrs(),
    case
        [Name ||
            {Name,Opts} <- IFs,
            lists:member({addr,{0,0,0,0,0,0,0,1}}, Opts)]
    of
        [_] ->
            test_recv_opts(
              inet6, [{recvtclass,tclass,224}], false,
              fun recvtclass_ok/2);
        [] ->
            {skip,ipv6_not_supported,IFs}
    end.


sendtos(_Config) ->
    test_recv_opts(
      inet, [{recvtos,tos,96}], true,
      fun sendtos_ok/2).

sendtosttl(_Config) ->
    test_recv_opts(
      inet, [{recvtos,tos,96},{recvttl,ttl,33}], true,
      fun (OSType, OSVer) ->
              sendtos_ok(OSType, OSVer) andalso sendttl_ok(OSType, OSVer)
      end).

sendttl(_Config) ->
    test_recv_opts(
      inet, [{recvttl,ttl,33}], true,
      fun sendttl_ok/2).

sendtclass(_Config) ->
    {ok,IFs} = inet:getifaddrs(),
    case
        [Name ||
            {Name,Opts} <- IFs,
            lists:member({addr,{0,0,0,0,0,0,0,1}}, Opts)]
    of
        [_] ->
            test_recv_opts(
              inet6, [{recvtclass,tclass,224}], true,
              fun sendtclass_ok/2);
        [] ->
            {skip,ipv6_not_supported,IFs}
    end.

%% These version numbers are just above the highest noted in daily tests
%% where the test fails for a plausible reason, that is the lowest
%% where we can expect that the test might succeed, so
%% skip on platforms lower than this.
%%
%% On newer versions it might be fixed, but we'll see about that
%% when machines with newer versions gets installed...
%% If the test still fails for a plausible reason these
%% version numbers simply should be increased.
%% Or maybe we should change to only test on known good platforms?

%% Using the option returns einval, so it is not implemented.
recvtos_ok({unix,darwin}, OSVer) -> not semver_lt(OSVer, {17,6,0});
%% Using the option returns einval, so it is not implemented.
recvtos_ok({unix,openbsd}, OSVer) -> not semver_lt(OSVer, {6,6,0});
%% Using the option returns einval, so it is not implemented.
recvtos_ok({unix,sunos}, OSVer) -> not semver_lt(OSVer, {5,12,0});
%%
recvtos_ok({unix,_}, _) -> true;
recvtos_ok(_, _) -> false.

%% Option has no effect
recvttl_ok({unix,sunos}, OSVer) -> not semver_lt(OSVer, {5,12,0});
%%
recvttl_ok({unix,_}, _) -> true;
recvttl_ok(_, _) -> false.

%% Using the option returns einval, so it is not implemented.
recvtclass_ok({unix,darwin}, OSVer) -> not semver_lt(OSVer, {9,9,0});
recvtclass_ok({unix,linux}, OSVer) -> not semver_lt(OSVer, {2,6,11});
%% Option has no effect
recvtclass_ok({unix,sunos}, OSVer) -> not semver_lt(OSVer, {5,12,0});
%%
recvtclass_ok({unix,_}, _) -> true;
recvtclass_ok(_, _) -> false.


%% To send ancillary data seems to require much higher version numbers
%% than receiving it...
%%

%% Using the option returns einval, so it is not implemented.
sendtos_ok({unix,darwin}, OSVer) -> not semver_lt(OSVer, {19,0,0});
sendtos_ok({unix,openbsd}, OSVer) -> not semver_lt(OSVer, {6,6,0});
sendtos_ok({unix,sunos}, OSVer) -> not semver_lt(OSVer, {5,12,0});
sendtos_ok({unix,linux}, OSVer) -> not semver_lt(OSVer, {4,0,0});
sendtos_ok({unix,freebsd}, OSVer) -> not semver_lt(OSVer, {12,1,0});
%%
sendtos_ok({unix,_}, _) -> true;
sendtos_ok(_, _) -> false.

%% Using the option returns einval, so it is not implemented.
sendttl_ok({unix,darwin}, OSVer) -> not semver_lt(OSVer, {19,0,0});
sendttl_ok({unix,linux}, OSVer) -> not semver_lt(OSVer, {4,0,0});
%% Using the option returns enoprotoopt, so it is not implemented.
sendttl_ok({unix,freebsd}, OSVer) -> not semver_lt(OSVer, {12,1,0});
%% Option has no effect
sendttl_ok({unix,sunos}, OSVer) -> not semver_lt(OSVer, {5,12,0});
sendttl_ok({unix,openbsd}, OSVer) -> not semver_lt(OSVer, {6,6,0});
%%
sendttl_ok({unix,_}, _) -> true;
sendttl_ok(_, _) -> false.

%% Using the option returns einval, so it is not implemented.
sendtclass_ok({unix,darwin}, OSVer) -> not semver_lt(OSVer, {9,9,0});
sendtclass_ok({unix,linux}, OSVer) -> not semver_lt(OSVer, {2,6,11});
%% Option has no effect
sendtclass_ok({unix,sunos}, OSVer) -> not semver_lt(OSVer, {5,12,0});
%%
sendtclass_ok({unix,_}, _) -> true;
sendtclass_ok(_, _) -> false.


semver_lt({X1,Y1,Z1}, {X2,Y2,Z2}) ->
    if
        X1 > X2 -> false;
        X1 < X2 -> true;
        Y1 > Y2 -> false;
        Y1 < Y2 -> true;
        Z1 > Z2 -> false;
        Z1 < Z2 -> true;
        true -> false
    end;
semver_lt(_, {_,_,_}) -> false.

test_recv_opts(Family, Spec, TestSend, OSFilter) ->
    OSType = os:type(),
    OSVer = os:version(),
    case OSFilter(OSType, OSVer) of
        true ->
            io:format("Os: ~p, ~p~n", [OSType,OSVer]),
            test_recv_opts(Family, Spec, TestSend, OSType, OSVer);
        false ->
            {skip,{not_supported_for_os_version,{OSType,OSVer}}}
    end.
%%
test_recv_opts(Family, Spec, TestSend, _OSType, _OSVer) ->
    Timeout = 5000,
    RecvOpts = [RecvOpt || {RecvOpt,_,_} <- Spec],
    TrueRecvOpts = [{RecvOpt,true} || {RecvOpt,_,_} <- Spec],
    FalseRecvOpts = [{RecvOpt,false} || {RecvOpt,_,_} <- Spec],
    Opts = [Opt || {_,Opt,_} <- Spec],
    OptsVals = [{Opt,Val} || {_,Opt,Val} <- Spec],
    TrueRecvOpts_OptsVals = TrueRecvOpts ++ OptsVals,
    Addr =
        case Family of
            inet ->
                {127,0,0,1};
            inet6 ->
                {0,0,0,0,0,0,0,1}
        end,
    %%
    {ok,S1} =
        gen_udp:open(0, [Family,binary,{active,false}|TrueRecvOpts]),
    {ok,P1} = inet:port(S1),
    {ok,TrueRecvOpts} = inet:getopts(S1, RecvOpts),
    ok = inet:setopts(S1, FalseRecvOpts),
    {ok,FalseRecvOpts} = inet:getopts(S1, RecvOpts),
    ok = inet:setopts(S1, TrueRecvOpts_OptsVals),
    {ok,TrueRecvOpts_OptsVals} = inet:getopts(S1, RecvOpts ++ Opts),
    %%
    %% S1 now has true receive options and set option values
    %%
    {ok,S2} =
        gen_udp:open(0, [Family,binary,{active,true}|FalseRecvOpts]),
    {ok,P2} = inet:port(S2),
    {ok,FalseRecvOpts_OptsVals2} = inet:getopts(S2, RecvOpts ++ Opts),
    OptsVals2 = FalseRecvOpts_OptsVals2 -- FalseRecvOpts,
    %%
    %% S2 now has false receive options and default option values,
    %% OptsVals2 contains the default option values
    %%
    ok = gen_udp:send(S2, {Addr,P1}, <<"abcde">>),
    ok = gen_udp:send(S1, Addr, P2, <<"fghij">>),
    TestSend andalso
        begin
            ok = gen_udp:send(S2, Addr, P1, OptsVals, <<"ABCDE">>),
            ok = gen_udp:send(S2, {Addr,P1}, OptsVals, <<"12345">>)
        end,
    {ok,{_,P2,OptsVals3,<<"abcde">>}} = gen_udp:recv(S1, 0, Timeout),
    verify_sets_eq(OptsVals3, OptsVals2),
    TestSend andalso
        begin
            {ok,{_,P2,OptsVals0,<<"ABCDE">>}} = gen_udp:recv(S1, 0, Timeout),
            {ok,{_,P2,OptsVals1,<<"12345">>}} = gen_udp:recv(S1, 0, Timeout),
            verify_sets_eq(OptsVals0, OptsVals),
            verify_sets_eq(OptsVals1, OptsVals)
        end,
    receive
        {udp,S2,_,P1,<<"fghij">>} ->
            ok;
        Other1 ->
            exit({unexpected,Other1})
    after Timeout ->
            exit(timeout)
    end,
    %%
    ok = inet:setopts(S1, FalseRecvOpts),
    {ok,FalseRecvOpts} = inet:getopts(S1, RecvOpts),
    ok = inet:setopts(S2, TrueRecvOpts),
    {ok,TrueRecvOpts} = inet:getopts(S2, RecvOpts),
    %%
    %% S1 now has false receive options and set option values
    %%
    %% S2 now has true receive options and default option values
    %%
    ok = gen_udp:send(S2, {Addr,P1}, [], <<"klmno">>),
    ok = gen_udp:send(S1, {Family,{loopback,P2}}, <<"pqrst">>),
    TestSend andalso
        begin
            ok = gen_udp:send(S1, {Family,{loopback,P2}}, OptsVals2, <<"PQRST">>)
        end,
    {ok,{_,P2,<<"klmno">>}} = gen_udp:recv(S1, 0, Timeout),
    receive
        {udp,S2,_,P1,OptsVals4,<<"pqrst">>} ->
            verify_sets_eq(OptsVals4, OptsVals);
        Other2 ->
            exit({unexpected,Other2})
    after Timeout ->
            exit(timeout)
    end,
    TestSend andalso
        receive
            {udp,S2,_,P1,OptsVals5,<<"PQRST">>} ->
                verify_sets_eq(OptsVals5, OptsVals2);
            Other3 ->
                exit({unexpected,Other3})
        after Timeout ->
                exit(timeout)
        end,
    ok = gen_udp:close(S1),
    ok = gen_udp:close(S2),
%%%    exit({{_OSType,_OSVer},success}), % In search for the truth
    ok.

verify_sets_eq(L1, L2) ->
    L = lists:sort(L1),
    case lists:sort(L2) of
        L ->
            ok;
        _ ->
            exit({sets_neq,L1,L2})
    end.


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
	{ok,{16#fe80,0,0,0,_,_,_,_} = Addr} ->
	    {skip,
	     "Got link local IPv6 address: "
	     ++inet:ntoa(Addr)};
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
	throw:not_ok:Stacktrace ->
	    raise_error({not_ok, NotOk}, tl(Stacktrace))
    end.

raise_error(Reason, Stacktrace) ->
    erlang:raise(error, Reason, Stacktrace).

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
