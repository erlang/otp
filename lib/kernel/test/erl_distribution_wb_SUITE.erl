%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2023. All Rights Reserved.
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
-module(erl_distribution_wb_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/inet.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([init_per_testcase/2, end_per_testcase/2, whitebox/1, 
	 switch_options/1, missing_compulsory_dflags/1,
         dflag_mandatory_25/1]).


-define(to_port(Socket, Data),
	case inet_tcp:send(Socket, Data) of
	    {error, closed} ->
		self() ! {tcp_closed, Socket},
	        {error, closed};
	    R ->
	        R
        end).

-define(DIST_VER_HIGH, 6).
-define(DIST_VER_LOW, 5).

-define(DFLAG_PUBLISHED,                16#01).
-define(DFLAG_ATOM_CACHE,               16#02).
-define(DFLAG_EXTENDED_REFERENCES,      16#04).
-define(DFLAG_DIST_MONITOR,             16#08).
-define(DFLAG_FUN_TAGS,                 16#10).
-define(DFLAG_NEW_FUN_TAGS,             16#80).
-define(DFLAG_EXTENDED_PIDS_PORTS,     16#100).
-define(DFLAG_EXPORT_PTR_TAG,          16#200).
-define(DFLAG_BIT_BINARIES,            16#400).
-define(DFLAG_NEW_FLOATS,              16#800).
-define(DFLAG_UTF8_ATOMS,            16#10000).
-define(DFLAG_MAP_TAG,               16#20000).
-define(DFLAG_BIG_CREATION,          16#40000).
-define(DFLAG_HANDSHAKE_23,        16#1000000).
-define(DFLAG_UNLINK_ID,           16#2000000).
-define(DFLAG_MANDATORY_25_DIGEST, 16#4000000).
-define(DFLAG_V4_NC,             16#400000000).

%% From OTP R9 extended references are compulsory.
%% From OTP R10 extended pids and ports are compulsory.
%% From OTP 20 UTF8 atoms are compulsory.
%% From OTP 21 NEW_FUN_TAGS is compulsory (no more tuple fallback {fun, ...}).
%% From OTP 23 BIG_CREATION is compulsory.
%% From OTP 25 HANDSHAKE_23, NEW_FLOATS, MAP_TAG, EXPORT_PTR_TAG, and BIT_BINARIES are compulsory.

-define(DFLAGS_MANDATORY_25,
        (?DFLAG_EXTENDED_REFERENCES bor
             ?DFLAG_FUN_TAGS bor
             ?DFLAG_EXTENDED_PIDS_PORTS bor
             ?DFLAG_UTF8_ATOMS bor
             ?DFLAG_NEW_FUN_TAGS bor
             ?DFLAG_BIG_CREATION bor
             ?DFLAG_HANDSHAKE_23 bor
             ?DFLAG_NEW_FLOATS bor
             ?DFLAG_MAP_TAG bor
             ?DFLAG_EXPORT_PTR_TAG bor
             ?DFLAG_BIT_BINARIES bor
             ?DFLAG_HANDSHAKE_23)).

%% From OTP 26 V4_NC and UNLINK_ID are compulsory.

-define(DFLAGS_MANDATORY_26,
        (?DFLAG_V4_NC bor
             ?DFLAG_UNLINK_ID)).

-define(COMPULSORY_DFLAGS,
        (?DFLAGS_MANDATORY_25 bor
             ?DFLAGS_MANDATORY_26)).

-define(PASS_THROUGH, $p).

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

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [whitebox, switch_options, missing_compulsory_dflags, dflag_mandatory_25].

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
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

%% Tests switching of options for the tcp port, as this is done
%% when the distribution port is to be shortcut into the emulator.
%% Maybe this should be in the inet test suite, but only the distribution
%% does such horrible things...
switch_options(Config) when is_list(Config) ->
    ok = test_switch_active(),
    ok = test_switch_active_partial() ,
    ok = test_switch_active_and_packet(),    
    ok.


%% Whitebox testing of distribution handshakes.
whitebox(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(),
    Cookie = erlang:get_cookie(),
    {_,Host} = split(node()),
    [begin
         io:format("Test TrustEpmd=~p\n", [TrustEpmd]),
         ok = pending_up_md5(Node, join(ccc,Host), TrustEpmd, Cookie),
         ok = simultaneous_md5(Node, join('A',Host), TrustEpmd, Cookie),
         ok = simultaneous_md5(Node, join(zzzzzzzzzz,Host), TrustEpmd, Cookie)
     end
     || TrustEpmd <- [true, false]],
    peer:stop(Peer),
    ok.

%%
%% The actual tests
%%

%%
%% Switch tcp options test
%%

test_switch_active() ->
    {Client, Server} = socket_pair(0, 4),
    ok = write_packets_32(Client, 1, 5),
    receive after 2000 -> ok end,
    ok = read_packets(Server, 1, 1),
    receive after 2000 -> ok end,
    ok = read_packets(Server, 2, 2),
    inet:setopts(Server, [{active, true}]),
    ok = receive_packets(Server, 3, 5),
    close_pair({Client, Server}),
    ok.

test_switch_active_partial() ->
    {Client, Server} = socket_pair(0, 4),
    ok = write_packets_32(Client, 1, 2),
    ok = gen_tcp:send(Client,[?int32(4), [0,0,0]]),
    receive after 2000 -> ok end,
    ok = read_packets(Server, 1, 1),
    receive after 2000 -> ok end,
    ok = read_packets(Server, 2, 2),
    inet:setopts(Server, [{active, true}]),
    ok = gen_tcp:send(Client,[3]),
    ok = write_packets_32(Client, 4, 5),
    ok = receive_packets(Server, 3, 5),
    close_pair({Client, Server}),
    ok.

do_test_switch_active_and_packet(SendBefore, SendAfter) ->
    {Client, Server} = socket_pair(0, 2),
    ok = write_packets_16(Client, 1, 2),
    ok = gen_tcp:send(Client,SendBefore),
    receive after 2000 -> ok end,
    ok = read_packets(Server, 1, 1),
    receive after 2000 -> ok end,
    ok = read_packets(Server, 2, 2),
    inet:setopts(Server, [{packet,4}, {active, true}]),
    ok = gen_tcp:send(Client,SendAfter),
    ok = write_packets_32(Client, 4, 5),
    ok = receive_packets(Server, 3, 5),
    close_pair({Client, Server}),
    ok.

test_switch_active_and_packet() ->
    ok = do_test_switch_active_and_packet([0],[0,0,4,0,0,0,3]),
    ok = do_test_switch_active_and_packet([0,0],[0,4,0,0,0,3]),
    ok = do_test_switch_active_and_packet([0,0,0],[4,0,0,0,3]),
    ok = do_test_switch_active_and_packet([0,0,0,4],[0,0,0,3]),
    ok = do_test_switch_active_and_packet([0,0,0,4,0],[0,0,3]),
    ok = do_test_switch_active_and_packet([0,0,0,4,0,0],[0,3]),
    ok = do_test_switch_active_and_packet([0,0,0,4,0,0,0],[3]),
    ok = do_test_switch_active_and_packet([0,0,0,4,0,0,0,3],[]),
    ok.


%%
%% Handshake tests
%%
pending_up_md5(Node,OurName,TrustEpmd,Cookie) ->
    {NA,NB} = split(Node),
    {port,PortNo,EpmdSaysVersion} = erl_epmd:port_please(NA,NB),
    {ok, SocketA} = gen_tcp:connect(atom_to_list(NB),PortNo,
				    [{active,false},
				     {packet,2}]),
    AssumedVersion = case TrustEpmd of
                         true -> EpmdSaysVersion;
                         false -> ?DIST_VER_LOW
                     end,
    SentNameMsg = send_name(SocketA,OurName,AssumedVersion),
    ok = recv_status(SocketA),
    {Node,HisChallengeA} = recv_challenge(SocketA),
    OurChallengeA = gen_challenge(),
    OurDigestA = gen_digest(HisChallengeA, Cookie),
    send_complement(SocketA, SentNameMsg),
    send_challenge_reply(SocketA, OurChallengeA, OurDigestA),
    ok = recv_challenge_ack(SocketA, OurChallengeA, Cookie),
%%%
%%% OK, one connection is up, now lets be nasty and try another up:
%%%
%%% But wait for a while, the other node might not have done setnode
%%% just yet...
    receive after 1000 -> ok end,
    {ok, SocketB} = gen_tcp:connect(atom_to_list(NB),PortNo,
				    [{active,false},
				     {packet,2}]),
    SentNameMsg = send_name(SocketB,OurName,AssumedVersion),
    alive = recv_status(SocketB),
    send_status(SocketB, true),
    gen_tcp:close(SocketA),
    {Node,HisChallengeB} = recv_challenge(SocketB),
    OurChallengeB = gen_challenge(),
    OurDigestB = gen_digest(HisChallengeB, Cookie),
    send_complement(SocketB, SentNameMsg),
    send_challenge_reply(SocketB, OurChallengeB, OurDigestB),
    ok = recv_challenge_ack(SocketB, OurChallengeB, Cookie),
%%%
%%% Well, are we happy?
%%%

    inet:setopts(SocketB, [{active, false},
			   {packet, 4}]),
    gen_tcp:send(SocketB,build_rex_message('',OurName)),
    {Header, Message} = recv_message(SocketB),
    io:format("Received header ~p, data ~p.~n",
	      [Header, Message]),
    gen_tcp:close(SocketB),
    ok.

simultaneous_md5(Node, OurName, TrustEpmd, Cookie) when OurName < Node ->
    pong = net_adm:ping(Node),
    LSocket = case gen_tcp:listen(0, [{active, false}, {packet,2}]) of
		  {ok, Socket} ->
		      Socket;
		  Else ->
		      exit(Else)
	      end,
    EpmdSocket = register_node(OurName, LSocket),
    {NA, NB} = split(Node),
    rpc:cast(Node, net_adm, ping, [OurName]),
    receive after 1000 -> ok end,
    {port, PortNo, EpmdSaysVersion} = erl_epmd:port_please(NA,NB),
    {ok, SocketA} = gen_tcp:connect(atom_to_list(NB),PortNo,
				    [{active,false},
				     {packet,2}]),
    AssumedVersion = case TrustEpmd of
                         true -> EpmdSaysVersion;
                         false -> ?DIST_VER_LOW
                     end,
    send_name(SocketA, OurName, AssumedVersion),
    %% We are still not marked up on the other side, as our first message 
    %% is not sent.
    SocketB = case gen_tcp:accept(LSocket) of
		  {ok, Socket1} ->
		      Socket1;
		  Else2 ->
		      exit(Else2)
	      end,
    nok = recv_status(SocketA),
    %% Now we are expected to close A
    gen_tcp:close(SocketA),
    %% But still Socket B will continue
    {Node,GotFlags} = recv_name(SocketB),
    true = (GotFlags band ?DFLAG_HANDSHAKE_23) =/= 0,
    send_status(SocketB, ok_simultaneous),
    MyChallengeB = gen_challenge(),
    send_challenge(SocketB, OurName, MyChallengeB, GotFlags),
    {ok,HisChallengeB} = recv_challenge_reply(SocketB, MyChallengeB, Cookie),
    DigestB = gen_digest(HisChallengeB,Cookie),
    send_challenge_ack(SocketB, DigestB),
    inet:setopts(SocketB, [{active, false},
			   {packet, 4}]),
    %% This should be the ping message.
    {Header, Message} = recv_message(SocketB),
    io:format("Received header ~p, data ~p.~n",
	      [Header, Message]),
    gen_tcp:close(SocketB),
    gen_tcp:close(LSocket),
    gen_tcp:close(EpmdSocket),
    ok;

simultaneous_md5(Node, OurName, TrustEpmd, Cookie) when OurName > Node ->
    pong = net_adm:ping(Node),
    LSocket = case gen_tcp:listen(0, [{active, false}, {packet,2}]) of
		  {ok, Socket} ->
		      Socket;
		  Else ->
		      exit(Else)
	      end,
    EpmdSocket = register_node(OurName, LSocket),
    {NA, NB} = split(Node),
    rpc:cast(Node, net_adm, ping, [OurName]),
    receive after 1000 -> ok end,
    {port, PortNo, EpmdSaysVersion} = erl_epmd:port_please(NA,NB),
    {ok, SocketA} = gen_tcp:connect(atom_to_list(NB),PortNo,
				    [{active,false},
				     {packet,2}]),
    SocketB = case gen_tcp:accept(LSocket) of
		  {ok, Socket1} ->
		      Socket1;
		  Else2 ->
		      exit(Else2)
	      end,
    AssumedVersion = case TrustEpmd of
                         true -> EpmdSaysVersion;
                         false -> ?DIST_VER_LOW
                     end,
    SentNameMsg = send_name(SocketA,OurName, AssumedVersion),
    ok_simultaneous = recv_status(SocketA),
    %% Socket B should die during this
    case catch begin
		   {Node, GotFlagsB} = recv_name(SocketB),
                   true = (GotFlagsB band ?DFLAG_HANDSHAKE_23) =/= 0,
		   send_status(SocketB, ok_simultaneous),
		   MyChallengeB = gen_challenge(),
		   send_challenge(SocketB, OurName, MyChallengeB, GotFlagsB),
		   {ok,HisChallengeB} = recv_challenge_reply(
				     SocketB,
				     MyChallengeB,
				     Cookie),
		   DigestB = gen_digest(HisChallengeB,Cookie),
		   send_challenge_ack(SocketB, DigestB),
		   inet:setopts(SocketB, [{active, false},
					  {packet, 4}]),
		   {HeaderB, MessageB} = recv_message(SocketB),
		   io:format("Received header ~p, data ~p.~n",
			     [HeaderB, MessageB])
	       end of
	{'EXIT', Exitcode} ->
	    io:format("Expected exitsignal caught: ~p.~n",
		      [Exitcode]);
	Success ->
	    io:format("Unexpected success: ~p~n",
		      [Success]),
	    exit(unexpected_success)
    end,
    gen_tcp:close(SocketB),
    %% But still Socket A will continue
    {Node,HisChallengeA} = recv_challenge(SocketA),
    OurChallengeA = gen_challenge(),
    OurDigestA = gen_digest(HisChallengeA, Cookie),
    send_complement(SocketA, SentNameMsg),
    send_challenge_reply(SocketA, OurChallengeA, OurDigestA),
    ok = recv_challenge_ack(SocketA, OurChallengeA, Cookie),

    inet:setopts(SocketA, [{active, false},
			   {packet, 4}]),
    gen_tcp:send(SocketA,build_rex_message('',OurName)),
    {Header, Message} = recv_message(SocketA),
    io:format("Received header ~p, data ~p.~n",
	      [Header, Message]),
    gen_tcp:close(SocketA),
    gen_tcp:close(LSocket),
    gen_tcp:close(EpmdSocket),
    ok.

missing_compulsory_dflags(Config) when is_list(Config) ->
    Cookie = erlang:get_cookie(),
    {ok, Peer, Node} = ?CT_PEER(),
    {NA,NB} = split(Node),
    {port,PortNo,_} = erl_epmd:port_please(NA,NB),
    [begin
         io:format("Assumed version ~p, Missing flags ~.16B\n",
                   [Version, MissingFlags]),
         {ok, SocketA} = gen_tcp:connect(atom_to_list(NB),PortNo,
                                         [{active,false},
                                          {packet,2}]),
         BadNode = list_to_atom(?CT_PEER_NAME()++"@"++atom_to_list(NB)),
         Flags = ?COMPULSORY_DFLAGS band (bnot MissingFlags),
         SentNameMsg = send_name(SocketA, BadNode, Version, Flags),
         case {Version, MissingFlags bsr 32} of
             {?DIST_VER_LOW, HighFlags} when HighFlags =/= 0 ->
                 %% Missing flag in high word, peer will not detect that
                 %% until we send complement.
                 ok = recv_status(SocketA),
                 {Node,HisChallengeA} = recv_challenge(SocketA),
                 OurChallengeA = gen_challenge(),
                 OurDigestA = gen_digest(HisChallengeA, Cookie),
                 send_complement(SocketA, SentNameMsg, Flags),
                 send_challenge_reply(SocketA, OurChallengeA, OurDigestA),

                 %% Would normally expect recv_challenge_ack but dist_util
                 %% reacts to missing flags with a status message instead.
                 not_allowed = recv_status(SocketA);

             _ ->
                 not_allowed = recv_status(SocketA)
         end,
         gen_tcp:close(SocketA)
     end
     || Version <- lists:seq(?DIST_VER_LOW, ?DIST_VER_HIGH),
        MissingFlags <- [?DFLAG_BIT_BINARIES,
                         ?DFLAG_HANDSHAKE_23,
                         ?DFLAG_V4_NC]],

    peer:stop(Peer),
    ok.

%% Test that instead of passing all compulsory flags, we can instead
%% pass only ?DFLAG_MANDATORY_25_DIGEST to ensure that we will be able to communicate
%% with a future release where ?DFLAG_MANDATORY_25_DIGEST is mandatory.
dflag_mandatory_25(_Config) ->
    {ok, Peer, Node} = ?CT_PEER(),
    {NA,NB} = split(Node),
    {port,PortNo,_} = erl_epmd:port_please(NA, NB),
    {ok, SocketA} = gen_tcp:connect(atom_to_list(NB),
                                    PortNo,
                                    [{active,false},{packet,2}]),
    OtherNode = list_to_atom(?CT_PEER_NAME()++"@"++atom_to_list(NB)),
    send_name(SocketA, OtherNode, ?DIST_VER_HIGH,
              ?DFLAG_MANDATORY_25_DIGEST bor ?DFLAGS_MANDATORY_26),
    ok = recv_status(SocketA),
    gen_tcp:close(SocketA),
    peer:stop(Peer),
    ok.

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

gen_challenge() ->
    rand:uniform(1000000).

%% Generate a message digest from Challenge number and Cookie	
gen_digest(Challenge, Cookie) when is_integer(Challenge), is_atom(Cookie) ->
    C0 = erlang:md5_init(),
    C1 = erlang:md5_update(C0, atom_to_list(Cookie)),
    C2 = erlang:md5_update(C1, integer_to_list(Challenge)),
    binary_to_list(erlang:md5_final(C2)).


%%
%% The different stages of the MD5 handshake
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

send_challenge(Socket, Node, Challenge, GotFlags) ->
    Flags = ?COMPULSORY_DFLAGS bor ?DFLAG_MANDATORY_25_DIGEST,
    send_challenge(Socket, Node, Challenge, GotFlags, Flags).

send_challenge(Socket, Node, Challenge, GotFlags, Flags) ->
    true = (GotFlags band ?DFLAG_HANDSHAKE_23) =/= 0,
    {ok, {{_Ip1,_Ip2,_Ip3,_Ip4}, _}} = inet:sockname(Socket),
    NodeName = atom_to_list(Node),
    Nlen = length(NodeName),
    Creation = erts_internal:get_creation(),
    ?to_port(Socket, [$N, <<(Flags bor ?DFLAG_HANDSHAKE_23):64>>,
                      <<Challenge:32>>, <<Creation:32>>,
                      <<Nlen:16>>, NodeName
                      ]).

recv_challenge(Socket) ->
    {ok, Msg} = gen_tcp:recv(Socket, 0),
    %%io:format("recv_challenge Msg=~p\n", [Msg]),
    case Msg of
        [$N, F7,F6,F5,F4,F3,F2,F1,F0, CA3,CA2,CA1,CA0,
         Cr3,Cr2,Cr1,Cr0, NL1,NL0 | Ns] ->
	    <<Flags:64>> = <<F7,F6,F5,F4,F3,F2,F1,F0>>,
            verify_flags(Flags),
            <<Creation:32>> = <<Cr3,Cr2,Cr1,Cr0>>,
            true = (Creation =/= 0),
            <<NameLen:16>> = <<NL1,NL0>>,
            NameLen = length(Ns),
	    Node = list_to_atom(Ns),
	    Challenge = ?u32(CA3,CA2,CA1,CA0),
	    {Node, Challenge};

	_ ->
	    ?shutdown(no_node)	    
    end.

verify_flags(Flags) ->
    RequiredFlags = ?COMPULSORY_DFLAGS bor ?DFLAG_MANDATORY_25_DIGEST,
    if
        Flags band RequiredFlags =:= RequiredFlags ->
            ok;
        true ->
            io:format("Given flags:    ~.16.0B\n", [Flags]),
            io:format("Required flags: ~.16.0B\n", [RequiredFlags]),
            ct:fail(missing_dflags)
    end.

send_complement(Socket, SentNameMsg) ->
    send_complement(Socket, SentNameMsg, ?COMPULSORY_DFLAGS).

send_complement(Socket, SentNameMsg, Flags) ->
    case SentNameMsg of
        $n ->
            FlagsHigh = Flags bsr 32,
            ?to_port(Socket, [$c,
                              <<FlagsHigh:32>>,
                              ?int32(erts_internal:get_creation())]);
        $N ->
            ok
    end.

send_challenge_reply(Socket, Challenge, Digest) ->
    ?to_port(Socket, [$r,?int32(Challenge),Digest]).

recv_challenge_reply(Socket, ChallengeA, Cookie) ->
    case gen_tcp:recv(Socket, 0) of
	{ok,[$r,CB3,CB2,CB1,CB0 | SumB]=Data} when length(SumB) == 16 ->
	    SumA = gen_digest(ChallengeA, Cookie),
	    ChallengeB = ?u32(CB3,CB2,CB1,CB0),
	    if SumB == SumA ->
		    {ok,ChallengeB};
	       true ->
		    {error,Data}
	    end;
	Err ->
            {error,Err}
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
	    end
    end.

send_name(Socket, MyNode0, AssumedVersion) ->
    Flags = ?COMPULSORY_DFLAGS bor ?DFLAG_MANDATORY_25_DIGEST,
    send_name(Socket, MyNode0, AssumedVersion, Flags).

send_name(Socket, MyNode0, AssumedVersion, Flags) ->
    MyNode = atom_to_list(MyNode0),
    if (AssumedVersion =:= ?DIST_VER_LOW) ->
            ok = ?to_port(Socket, [<<$n,?DIST_VER_HIGH:16,Flags:32>>|MyNode]),
            $n;

       (AssumedVersion > ?DIST_VER_LOW) ->
            Creation = erts_internal:get_creation(),
            NameLen = length(MyNode),
            ok = ?to_port(Socket, [<<$N, Flags:64,
                                     Creation:32,NameLen:16>>|MyNode]),
            $N
    end.

recv_name(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	{ok,Data} ->
	    get_name(Data);
	Res ->
	    ?shutdown({no_node,Res})
    end.

get_name([$N, F7,F6,F5,F4,F3,F2,F1,F0,
          _C3,_C2,_C1,_C0, NLen1,NLen2 | OtherNode]) ->
    <<Flags:64>> = <<F7,F6,F5,F4,F3,F2,F1,F0>>,
    true = (Flags band ?DFLAG_HANDSHAKE_23) =/= 0,
    <<NameLen:16>> = <<NLen1,NLen2>>,
    NameLen = length(OtherNode),
    {list_to_atom(OtherNode), Flags};
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

do_register_node(NodeName, TcpPort) ->
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
				  ?int16(?DIST_VER_HIGH),
				  ?int16(?DIST_VER_LOW),
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
		[$v, Result, A, B, C, D] ->
		    case Result of
			0 ->
			    {alive, Socket, ?u32(A, B, C, D)};
			_ ->
			    {error, duplicate_name}
		    end;
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


register_node(NodeName, ListenSocket) ->
    {ok,{_,TcpPort}} = inet:sockname(ListenSocket),
    case do_register_node(NodeName, TcpPort) of
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
    [?PASS_THROUGH,term_to_binary({6,self(),Cookie,rex}),
     term_to_binary({'$gen_cast',
		     {cast,
		      erlang,
		      send,
		      [{regname, OurName}, "hello world"],
                      self()}})].

%% Receive a distribution message    
recv_message(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok,[]} ->
            recv_message(Socket);  %% a tick, ignore
	{ok,Data} ->
	    B0 = list_to_binary(Data),
	    <<?PASS_THROUGH, B1/binary>> = B0,
	    {Header,Siz} = binary_to_term(B1,[used]),
	    <<_:Siz/binary,B2/binary>> = B1,
	    Message = case (catch binary_to_term(B2)) of
			  {'EXIT', _} ->
			      {could_not_digest_message,B2};
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
