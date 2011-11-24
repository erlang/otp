%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2011. All Rights Reserved.
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
-module(erl_distribution_wb_SUITE).

-include_lib("test_server/include/test_server.hrl").
-include_lib("kernel/include/inet.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([init_per_testcase/2, end_per_testcase/2, whitebox/1, 
	 switch_options/1, missing_compulsory_dflags/1]).

%% 1)
%%
%% Connections are now always set up symetrically with respect to
%% publication. If connecting node doesn't send DFLAG_PUBLISHED
%% the other node wont send DFLAG_PUBLISHED. If the connecting
%% node send DFLAG_PUBLISHED but the other node doesn't send
%% DFLAG_PUBLISHED, the connecting node should consider its
%% DFLAG_PUBLISHED as dropped, i.e the connecting node wont be
%% published on the other node.

-define(to_port(Socket, Data),
	case inet_tcp:send(Socket, Data) of
	    {error, closed} ->
		self() ! {tcp_closed, Socket},
	        {error, closed};
	    R ->
	        R
        end).

-define(DFLAG_PUBLISHED,1).
-define(DFLAG_ATOM_CACHE,2).
-define(DFLAG_EXTENDED_REFERENCES,4).
-define(DFLAG_DIST_MONITOR,8).
-define(DFLAG_FUN_TAGS,16#10).
-define(DFLAG_DIST_MONITOR_NAME,16#20).
-define(DFLAG_HIDDEN_ATOM_CACHE,16#40).
-define(DFLAG_NEW_FUN_TAGS,16#80).
-define(DFLAG_EXTENDED_PIDS_PORTS,16#100).

%% From R9 and forward extended references is compulsory
%% From R10 and forward extended pids and ports are compulsory
-define(COMPULSORY_DFLAGS, (?DFLAG_EXTENDED_REFERENCES bor ?DFLAG_EXTENDED_PIDS_PORTS)).


-define(shutdown(X), exit(X)).
-define(int16(X), [((X) bsr 8) band 16#ff, (X) band 16#ff]).

-define(int32(X), 
	[((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
	 ((X) bsr 8) band 16#ff, (X) band 16#ff]).

-define(i16(X1,X0),
        (?u16(X1,X0) - 
         (if (X1) > 127 -> 16#10000; true -> 0 end))).

-define(u16(X1,X0),
        (((X1) bsl 8) bor (X0))).

-define(u32(X3,X2,X1,X0),
        (((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [whitebox, switch_options, missing_compulsory_dflags].

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


init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog=?t:timetrap(?t:minutes(1)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

switch_options(doc) ->
    ["Tests switching of options for the tcp port, as this is done"
     " when the distribution port is to be shortcut into the emulator."
     " Maybe this should be in the inet test suite, but only the distribution"
     " does such horrible things..."];
switch_options(Config) when is_list(Config) ->
    ok = test_switch_active(),
    ok = test_switch_active_partial() ,
    ok = test_switch_active_and_packet(),    
    ok.
    

whitebox(doc) ->
    ["Whitebox testing of distribution handshakes. Tests both BC with R5 and "
     "the md5 version. Note that after R6B, this should be revised to "
     "remove BC code."];
whitebox(Config) when is_list(Config) ->
    ?line {ok, Node} = start_node(?MODULE,""),
    ?line Cookie = erlang:get_cookie(),
    ?line {_,Host} = split(node()),
    ?line ok = pending_up_md5(Node, join(ccc,Host), Cookie),
    ?line ok = simultaneous_md5(Node, join('A',Host), Cookie),
    ?line ok = simultaneous_md5(Node, join(zzzzzzzzzzzzzz,Host), Cookie),
    ?line stop_node(Node),
    ok.
    
%%
%% The actual tests
%%

%%
%% Switch tcp options test
%%

test_switch_active() ->
    ?line {Client, Server} = socket_pair(0, 4),
    ?line ok = write_packets_32(Client, 1, 5),
    receive after 2000 -> ok end,
    ?line ok = read_packets(Server, 1, 1),
    receive after 2000 -> ok end,
    ?line ok = read_packets(Server, 2, 2),
    ?line inet:setopts(Server, [{active, true}]),
    ?line ok = receive_packets(Server, 3, 5),
    close_pair({Client, Server}),
    ok.
    
test_switch_active_partial() ->
    ?line {Client, Server} = socket_pair(0, 4),
    ?line ok = write_packets_32(Client, 1, 2),
    ?line ok = gen_tcp:send(Client,[?int32(4), [0,0,0]]),
    receive after 2000 -> ok end,
    ?line ok = read_packets(Server, 1, 1),
    receive after 2000 -> ok end,
    ?line ok = read_packets(Server, 2, 2),
    ?line inet:setopts(Server, [{active, true}]),
    ?line ok = gen_tcp:send(Client,[3]),
    ?line ok = write_packets_32(Client, 4, 5),
    ?line ok = receive_packets(Server, 3, 5),
    close_pair({Client, Server}),
    ok.
    
do_test_switch_active_and_packet(SendBefore, SendAfter) ->
    ?line {Client, Server} = socket_pair(0, 2),
    ?line ok = write_packets_16(Client, 1, 2),
    ?line ok = gen_tcp:send(Client,SendBefore),
    receive after 2000 -> ok end,
    ?line ok = read_packets(Server, 1, 1),
    receive after 2000 -> ok end,
    ?line ok = read_packets(Server, 2, 2),
    ?line inet:setopts(Server, [{packet,4}, {active, true}]),
    ?line ok = gen_tcp:send(Client,SendAfter),
    ?line ok = write_packets_32(Client, 4, 5),
    ?line ok = receive_packets(Server, 3, 5),
    close_pair({Client, Server}),
    ok.

test_switch_active_and_packet() ->
    ?line ok = do_test_switch_active_and_packet([0],[0,0,4,0,0,0,3]),
    ?line ok = do_test_switch_active_and_packet([0,0],[0,4,0,0,0,3]),
    ?line ok = do_test_switch_active_and_packet([0,0,0],[4,0,0,0,3]),
    ?line ok = do_test_switch_active_and_packet([0,0,0,4],[0,0,0,3]),
    ?line ok = do_test_switch_active_and_packet([0,0,0,4,0],[0,0,3]),
    ?line ok = do_test_switch_active_and_packet([0,0,0,4,0,0],[0,3]),
    ?line ok = do_test_switch_active_and_packet([0,0,0,4,0,0,0],[3]),
    ?line ok = do_test_switch_active_and_packet([0,0,0,4,0,0,0,3],[]),
    ok.


%%
%% Handshake tests
%%
pending_up_md5(Node,OurName,Cookie) ->
    ?line {NA,NB} = split(Node),
    ?line {port,PortNo,_} = erl_epmd:port_please(NA,NB),
    ?line {ok, SocketA} = gen_tcp:connect(atom_to_list(NB),PortNo,
					  [{active,false},
					   {packet,2}]),
    ?line send_name(SocketA,OurName,5),
    ?line ok = recv_status(SocketA),
    ?line {hidden,Node,5,HisChallengeA} = recv_challenge(SocketA), % See 1)
    ?line OurChallengeA = gen_challenge(),
    ?line OurDigestA = gen_digest(HisChallengeA, Cookie),
    ?line send_challenge_reply(SocketA, OurChallengeA, OurDigestA),
    ?line ok = recv_challenge_ack(SocketA, OurChallengeA, Cookie),
    %%%
    %%% OK, one connection is up, now lets be nasty and try another up:
    %%%
    %%% But wait for a while, the other node might not have done setnode 
    %%% just yet...
    ?line receive after 1000 -> ok end,
    ?line {ok, SocketB} = gen_tcp:connect(atom_to_list(NB),PortNo,
					  [{active,false},
					   {packet,2}]),
    ?line send_name(SocketB,OurName,5),
    ?line alive = recv_status(SocketB),
    ?line send_status(SocketB, true),
    ?line gen_tcp:close(SocketA),
    ?line {hidden,Node,5,HisChallengeB} = recv_challenge(SocketB), % See 1)
    ?line OurChallengeB = gen_challenge(),
    ?line OurDigestB = gen_digest(HisChallengeB, Cookie),
    ?line send_challenge_reply(SocketB, OurChallengeB, OurDigestB),
    ?line ok = recv_challenge_ack(SocketB, OurChallengeB, Cookie),
    %%%
    %%% Well, are we happy?
    %%%
    
    ?line inet:setopts(SocketB, [{active, false},
				 {packet, 4}]),    
    ?line gen_tcp:send(SocketB,build_rex_message('',OurName)),
    ?line {Header, Message} = recv_message(SocketB),
    ?line io:format("Received header ~p, data ~p.~n",
	      [Header, Message]),
    ?line gen_tcp:close(SocketB),
    ok.

simultaneous_md5(Node, OurName, Cookie) when OurName < Node ->
    ?line pong = net_adm:ping(Node),
    ?line LSocket = case gen_tcp:listen(0, [{active, false}, {packet,2}]) of
		  {ok, Socket} ->
		      Socket;
		  Else ->
		      exit(Else)
	      end,
    ?line EpmdSocket = register(OurName, LSocket, 1, 5),
    ?line {NA, NB} = split(Node),
    ?line rpc:cast(Node, net_adm, ping, [OurName]),
    ?line receive after 1000 -> ok end,
    ?line {port, PortNo, _} = erl_epmd:port_please(NA,NB),
    ?line {ok, SocketA} = gen_tcp:connect(atom_to_list(NB),PortNo,
					  [{active,false},
					   {packet,2}]),
    ?line send_name(SocketA,OurName,5),
    %% We are still not marked up on the other side, as our first message 
    %% is not sent.
    ?line SocketB = case gen_tcp:accept(LSocket) of
		  {ok, Socket1} ->
		      ?line Socket1;
		  Else2 ->
		      ?line exit(Else2)
	      end,
    ?line nok = recv_status(SocketA),
    % Now we are expected to close A
    ?line gen_tcp:close(SocketA),
    % But still Socket B will continue
    ?line {normal,Node,5} = recv_name(SocketB),  % See 1)
    ?line send_status(SocketB, ok_simultaneous),
    ?line MyChallengeB = gen_challenge(),
    ?line send_challenge(SocketB, OurName, MyChallengeB,5),
    ?line HisChallengeB = recv_challenge_reply(SocketB, MyChallengeB, Cookie),
    ?line DigestB = gen_digest(HisChallengeB,Cookie),
    ?line send_challenge_ack(SocketB, DigestB),
    ?line inet:setopts(SocketB, [{active, false},
			  {packet, 4}]),    
    % This should be the ping message.
    ?line {Header, Message} = recv_message(SocketB),
    ?line io:format("Received header ~p, data ~p.~n",
	      [Header, Message]),
    ?line gen_tcp:close(SocketB),
    ?line gen_tcp:close(LSocket),
    ?line gen_tcp:close(EpmdSocket),
    ok;
    
simultaneous_md5(Node, OurName, Cookie) when OurName > Node ->
    ?line pong = net_adm:ping(Node),
    ?line LSocket = case gen_tcp:listen(0, [{active, false}, {packet,2}]) of
			{ok, Socket} ->
			    ?line Socket;
			Else ->
			    ?line exit(Else)
		    end,
    ?line EpmdSocket = register(OurName, LSocket, 1, 5),
    ?line {NA, NB} = split(Node),
    ?line rpc:cast(Node, net_adm, ping, [OurName]),
    ?line receive after 1000 -> ok end,
    ?line {port, PortNo, _} = erl_epmd:port_please(NA,NB),
    ?line {ok, SocketA} = gen_tcp:connect(atom_to_list(NB),PortNo,
					  [{active,false},
					   {packet,2}]),
    ?line SocketB = case gen_tcp:accept(LSocket) of
		  {ok, Socket1} ->
		      ?line Socket1;
		  Else2 ->
		      ?line exit(Else2)
	      end,
    ?line send_name(SocketA,OurName,5),
    ?line ok_simultaneous = recv_status(SocketA),
    %% Socket B should die during this
    ?line case catch begin
			 ?line {normal,Node,5} = recv_name(SocketB),  % See 1)
			 ?line send_status(SocketB, ok_simultaneous),
			 ?line MyChallengeB = gen_challenge(),
			 ?line send_challenge(SocketB, OurName, MyChallengeB,
					      5),
			 ?line HisChallengeB = recv_challenge_reply(
						 SocketB, 
						 MyChallengeB, 
						 Cookie),
			 ?line DigestB = gen_digest(HisChallengeB,Cookie),
			 ?line send_challenge_ack(SocketB, DigestB),
			 ?line inet:setopts(SocketB, [{active, false},
						      {packet, 4}]),    
			 ?line {HeaderB, MessageB} = recv_message(SocketB),
			 ?line io:format("Received header ~p, data ~p.~n",
					 [HeaderB, MessageB])
		     end of
	      {'EXIT', Exitcode} ->
		  ?line io:format("Expected exitsignal caught: ~p.~n",
				  [Exitcode]);
	      Success ->
		  ?line io:format("Unexpected success: ~p~n",
				  [Success]),
		  ?line exit(unexpected_success)
	  end,
    ?line gen_tcp:close(SocketB),
    %% But still Socket A will continue
    ?line {hidden,Node,5,HisChallengeA} = recv_challenge(SocketA), % See 1)
    ?line OurChallengeA = gen_challenge(),
    ?line OurDigestA = gen_digest(HisChallengeA, Cookie),
    ?line send_challenge_reply(SocketA, OurChallengeA, OurDigestA),
    ?line ok = recv_challenge_ack(SocketA, OurChallengeA, Cookie),
    
    ?line inet:setopts(SocketA, [{active, false},
				 {packet, 4}]),    
    ?line gen_tcp:send(SocketA,build_rex_message('',OurName)),
    ?line {Header, Message} = recv_message(SocketA),
    ?line io:format("Received header ~p, data ~p.~n",
	      [Header, Message]),
    ?line gen_tcp:close(SocketA),
    ?line gen_tcp:close(LSocket),
    ?line gen_tcp:close(EpmdSocket),
    ok.

missing_compulsory_dflags(doc) -> [];
missing_compulsory_dflags(Config) when is_list(Config) ->
    ?line [Name1, Name2] = get_nodenames(2, missing_compulsory_dflags),
    ?line {ok, Node} = start_node(Name1,""),
    ?line {NA,NB} = split(Node),
    ?line {port,PortNo,_} = erl_epmd:port_please(NA,NB),
    ?line {ok, SocketA} = gen_tcp:connect(atom_to_list(NB),PortNo,
					  [{active,false},
					   {packet,2}]),
    ?line BadNode = list_to_atom(atom_to_list(Name2)++"@"++atom_to_list(NB)),
    ?line send_name(SocketA,BadNode,5,0),
    ?line not_allowed = recv_status(SocketA),
    ?line gen_tcp:close(SocketA),
    ?line stop_node(Node),
    ?line ok.

%%
%% Here comes the utilities
%%

%%
%% Switch option utilities
%%
write_packets_32(_, M, N) when M > N ->
    ok;
write_packets_32(Sock, M, N) ->
    ok = gen_tcp:send(Sock,[?int32(4), ?int32(M)]),
    write_packets_32(Sock, M+1, N).

write_packets_16(_, M, N) when M > N ->
    ok;
write_packets_16(Sock, M, N) ->
    ok = gen_tcp:send(Sock,[?int16(4), ?int32(M)]),
    write_packets_16(Sock, M+1, N).

read_packets(_, M, N) when M > N ->
    ok;
read_packets(Sock, M, N) ->
    Expected = ?int32(M),
    case gen_tcp:recv(Sock, 0) of
	{ok, Expected} ->
	    read_packets(Sock, M+1, N);
	{ok, Unexpected} ->
	    exit({unexpected_data_read, Unexpected});
	Error ->
	    exit({error_read, Error})
    end.

receive_packets(Sock, M, N) when M > N ->
    receive
	{tcp, Sock, Data} ->
	    exit({extra_data, Data})
    after 0 ->
	    ok
    end;

receive_packets(Sock, M, N) ->
    Expect = ?int32(M),
    receive
	{tcp, Sock, Expect} ->
	    receive_packets(Sock, M+1, N); 
	{tcp, Sock, Unexpected} ->
	    exit({unexpected_data_received, Unexpected})
    after 500 ->
	    exit({no_data_received_for,M})
    end.

socket_pair(ClientPack, ServerPack) ->
    {ok, Listen} = gen_tcp:listen(0, [{active, false}, 
				      {packet, ServerPack}]),
    {ok, Host} = inet:gethostname(),
    {ok, Port} = inet:port(Listen),
    {ok, Client} = gen_tcp:connect(Host, Port, [{active, false}, 
						{packet, ClientPack}]),
    {ok, Server} = gen_tcp:accept(Listen),
    gen_tcp:close(Listen),
    {Client, Server}.
    
close_pair({Client, Server}) ->
    gen_tcp:close(Client),
    gen_tcp:close(Server),
    ok.


%%
%% Handshake utilities
%%

%%
%% MD5 hashing
%%

%% This is no proper random number, but that is not really important in 
%% this test
gen_challenge() ->
    {_,_,N} = erlang:now(), 
    N.
    
%% Generate a message digest from Challenge number and Cookie	
gen_digest(Challenge, Cookie) when is_integer(Challenge), is_atom(Cookie) ->
    C0 = erlang:md5_init(),
    C1 = erlang:md5_update(C0, atom_to_list(Cookie)),
    C2 = erlang:md5_update(C1, integer_to_list(Challenge)),
    binary_to_list(erlang:md5_final(C2)).


%%
%% The differrent stages of the MD5 handshake
%%

send_status(Socket, Stat) ->
    case gen_tcp:send(Socket, [$s | atom_to_list(Stat)]) of
	{error, _} ->
	    ?shutdown(could_not_send_status);
	_ -> 
	    true
    end.


recv_status(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, [$s|StrStat]} ->
	    list_to_atom(StrStat);
	Bad ->
	    exit(Bad)
    end.

send_challenge(Socket, Node, Challenge, Version) ->
    send_challenge(Socket, Node, Challenge, Version, ?COMPULSORY_DFLAGS).
send_challenge(Socket, Node, Challenge, Version, Flags) ->
    {ok, {{_Ip1,_Ip2,_Ip3,_Ip4}, _}} = inet:sockname(Socket),
    ?to_port(Socket, [$n,?int16(Version),?int32(Flags),
		      ?int32(Challenge), atom_to_list(Node)]).

recv_challenge(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	{ok,[$n,V1,V0,Fl1,Fl2,Fl3,Fl4,CA3,CA2,CA1,CA0 | Ns]} ->
	    Flags = ?u32(Fl1,Fl2,Fl3,Fl4),
	    Type = case Flags band ?DFLAG_PUBLISHED of
		       0 ->
			   hidden;
		       _ ->
			   normal
		   end,
	    Node =list_to_atom(Ns),
	    Version = ?u16(V1,V0),
	    Challenge = ?u32(CA3,CA2,CA1,CA0),
	    {Type,Node,Version,Challenge};
	_ ->
	    ?shutdown(no_node)	    
    end.

send_challenge_reply(Socket, Challenge, Digest) ->
    ?to_port(Socket, [$r,?int32(Challenge),Digest]).

recv_challenge_reply(Socket, ChallengeA, Cookie) ->
    case gen_tcp:recv(Socket, 0) of
	{ok,[$r,CB3,CB2,CB1,CB0 | SumB]} when length(SumB) == 16 ->
	    SumA = gen_digest(ChallengeA, Cookie),
	    ChallengeB = ?u32(CB3,CB2,CB1,CB0),
	    if SumB == SumA ->
		    ChallengeB;
	       true ->
		    ?shutdown(bad_challenge_reply)
	    end;
	_ ->
	    ?shutdown(no_node)
    end.

send_challenge_ack(Socket, Digest) ->
    ?to_port(Socket, [$a,Digest]).

recv_challenge_ack(Socket, ChallengeB, CookieA) ->
    case gen_tcp:recv(Socket, 0) of
	{ok,[$a | SumB]} when length(SumB) == 16 ->
	    SumA = gen_digest(ChallengeB, CookieA),
	    if SumB == SumA ->
		    ok;
	       true ->
		    ?shutdown(bad_challenge_ack)
	    end;
	_ ->
	    ?shutdown(bad_challenge_ack)
    end.

send_name(Socket, MyNode0, Version) ->
    send_name(Socket, MyNode0, Version, ?COMPULSORY_DFLAGS).
send_name(Socket, MyNode0, Version, Flags) ->
    MyNode = atom_to_list(MyNode0),
    ok = ?to_port(Socket, [<<$n,Version:16,Flags:32>>|MyNode]).

%%
%% recv_name is common for both old and new handshake.
%%
recv_name(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	{ok,Data} ->
	    get_name(Data);
	Res ->
	    ?shutdown({no_node,Res})
    end.

get_name([$m,VersionA,VersionB,_Ip1,_Ip2,_Ip3,_Ip4|OtherNode]) ->
    {normal, list_to_atom(OtherNode), ?u16(VersionA,VersionB)};
get_name([$h,VersionA,VersionB,_Ip1,_Ip2,_Ip3,_Ip4|OtherNode]) ->
    {hidden, list_to_atom(OtherNode), ?u16(VersionA,VersionB)};
get_name([$n,VersionA, VersionB, Flag1, Flag2, Flag3, Flag4 | OtherNode]) ->
    Type = case ?u32(Flag1, Flag2, Flag3, Flag4) band ?DFLAG_PUBLISHED of
	       0 ->
		   hidden;
	       _ -> 
		   normal
	   end,
    {Type, list_to_atom(OtherNode), 
     ?u16(VersionA,VersionB)};
get_name(Data) ->
    ?shutdown(Data).

%%
%% The communication with EPMD follows
%%
get_epmd_port() ->
    case init:get_argument(epmd_port) of
        {ok, [[PortStr|_]|_]} when is_list(PortStr) ->
            list_to_integer(PortStr);
        error ->
            4369 % Default epmd port
    end.

do_register_node(NodeName, TcpPort, VLow, VHigh) ->    
    case gen_tcp:connect({127,0,0,1}, get_epmd_port(), []) of
	{ok, Socket} ->
	    {N0,_} = split(NodeName),
	    Name = atom_to_list(N0),
	    Extra = "",
	    Elen = length(Extra),
	    Len = 1+2+1+1+2+2+2+length(Name)+2+Elen,
	    gen_tcp:send(Socket, [?int16(Len), $x,
				   ?int16(TcpPort),
				   $M,
				   0,
				   ?int16(VHigh),
				   ?int16(VLow),
				   ?int16(length(Name)),
				   Name,
				   ?int16(Elen),
				   Extra]),
	    case wait_for_reg_reply(Socket, []) of
		{error, epmd_close} ->
		    exit(epmd_broken);
		Other ->
		    Other
	    end;
	Error ->
	    Error
    end.

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


register(NodeName, ListenSocket, VLow, VHigh) ->
    {ok,{_,TcpPort}} = inet:sockname(ListenSocket),
    case do_register_node(NodeName, TcpPort, VLow, VHigh) of
	{alive, Socket, _Creation} ->
	    Socket;
	Other ->
	    exit(Other)
    end.


%%
%% Utilities
%%

%% Split a nodename
split([$@|T],A) ->
    {lists:reverse(A),T};
split([H|T],A) ->
    split(T,[H|A]).

split(Atom) ->
    {A,B} = split(atom_to_list(Atom),[]),
    {list_to_atom(A),list_to_atom(B)}.

%% Build a distribution message that will make rex answer
build_rex_message(Cookie,OurName) ->
    [$?,term_to_binary({6,self(),Cookie,rex}),
     term_to_binary({'$gen_cast',
		    {cast,
		     rpc,
		     cast,
		     [OurName, hello, world, []],
		     self()} })].

%% Receive a distribution message    
recv_message(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	{ok,Data} ->
	    B0 = list_to_binary(Data),
	    {_,B1} = erlang:split_binary(B0,1),
	    Header = binary_to_term(B1),
	    Siz = byte_size(term_to_binary(Header)),
	    {_,B2} = erlang:split_binary(B1,Siz),
	    Message = case (catch binary_to_term(B2)) of
			  {'EXIT', _} ->
			      could_not_digest_message;
			  Other ->
			      Other
		      end,
	    {Header, Message};
	Res ->
	    exit({no_message,Res})
    end. 

%% Build a nodename
join(Name,Host) ->
    list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Host)).

%% start/stop slave.
start_node(Name, Param) ->
    ?t:start_node(Name, slave, [{args, Param}]).

stop_node(Node) ->
    ?t:stop_node(Node).


get_nodenames(N, T) ->
    get_nodenames(N, T, []).

get_nodenames(0, _, Acc) ->
    Acc;
get_nodenames(N, T, Acc) ->
    {A, B, C} = now(),
    get_nodenames(N-1, T, [list_to_atom(atom_to_list(?MODULE)
					++ "-"
					++ atom_to_list(T)
					++ "-"
					++ integer_to_list(A)
					++ "-"
					++ integer_to_list(B)
					++ "-"
					++ integer_to_list(C)) | Acc]).
