%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2023. All Rights Reserved.
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
-module(ei_tmo_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/inet.hrl").
-include("ei_tmo_SUITE_data/ei_tmo_test_cases.hrl").

-export([all/0, groups/0, suite/0,
         init_per_testcase/2, end_per_testcase/2,
         framework_check/1, ei_accept_tmo/1, ei_connect_tmo/1,
         ei_send_tmo/1,
         ei_send_failure_tmo/1,
	 ei_connect_unreachable_tmo/0, ei_connect_unreachable_tmo/1,
         ei_recv_tmo/1,
         ei_dflags/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() -> 
    [framework_check,
     ei_connect_unreachable_tmo,
     ei_send_failure_tmo,
     {group, default},
     {group, ussi}].

groups() ->
    Members = [ei_recv_tmo,
               ei_accept_tmo,
               ei_connect_tmo,
               ei_send_tmo,
               ei_dflags],
    [{default, [], Members},
     {ussi, [], Members}].

get_group(Config) ->
    proplists:get_value(name, proplists:get_value(tc_group_properties,Config)).

init_per_testcase(Case, Config) ->
    runner:init_per_testcase(?MODULE, Case, Config).

end_per_testcase(_Case, _Config) ->
    ok.

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
%% From OTP 25 NEW_FLOATS, MAP_TAG, EXPORT_PTR_TAG, and BIT_BINARIES are compulsory.

-define(DFLAGS_MANDATORY_25, 
        (?DFLAG_EXTENDED_REFERENCES bor
             ?DFLAG_FUN_TAGS bor
             ?DFLAG_EXTENDED_PIDS_PORTS bor
             ?DFLAG_UTF8_ATOMS bor
             ?DFLAG_NEW_FUN_TAGS bor
             ?DFLAG_BIG_CREATION bor
             ?DFLAG_NEW_FLOATS bor
             ?DFLAG_MAP_TAG bor
             ?DFLAG_EXPORT_PTR_TAG bor
             ?DFLAG_BIT_BINARIES bor
             ?DFLAG_HANDSHAKE_23)).

%% From OTP 26 V4_NC, and UNLINK_ID are compulsory.

-define(DFLAGS_MANDATORY_26,
        (?DFLAG_V4_NC bor
             ?DFLAG_UNLINK_ID)).

-define(COMPULSORY_DFLAGS,
        (?DFLAGS_MANDATORY_25 bor
             ?DFLAGS_MANDATORY_26)).

%% Check the framework.
framework_check(Config) when is_list(Config) ->
    %%dbg:tracer(),
    %%dbg:p(self()),
    P = runner:start(Config, ?framework_check),
    runner:send_term(P,{hello,world}),
    {term, {hello,world}} = runner:get_term(P),
    runner:recv_eot(P),
    ok.


%% Check recv with timeouts.
ei_recv_tmo(Config) when is_list(Config) ->
    do_one_recv(Config,c_node_recv_tmo_1),
    do_one_recv_failure(Config,c_node_recv_tmo_2),
    ok.


do_one_recv(Config,CNode) ->
    {_,Host} = split(node()),
    P1 = runner:start(Config, ?recv_tmo),
    runner:send_term(P1,{CNode,
                         erlang:get_cookie(),
                         node(),
                         get_group(Config)}),
    {term, X} = runner:get_term(P1, 10000),
    true = is_integer(X),
    CNode1 = join(CNode,Host),
    Term1 = {hej,[hopp,{i,[lingon,"skogen"]}]},
    {test,CNode1} ! Term1,
    {term, Term1} = runner:get_term(P1, 10000),
    runner:recv_eot(P1).

do_one_recv_failure(Config,CNode) ->
    P1 = runner:start(Config, ?recv_tmo),
    runner:send_term(P1,{CNode,
                         erlang:get_cookie(),
                         node(),
                         get_group(Config)}),
    {term, X} = runner:get_term(P1, 10000),
    true = is_integer(X),
    {term, {Ret,ETimedout,ETimedout}} = runner:get_term(P1, 10000),
    true = (Ret < 0),
    runner:recv_eot(P1).

-define(EI_DIST_LOW, 6).
-define(EI_DIST_HIGH, 6).

%% An OTP-23 or 24 node may connect assuming 5 or higher.
-define(EI_DIST_LOWEST_ASSUMED, 5).


%% Check send with timeouts.
ei_send_tmo(Config) when is_list(Config) ->
    register(ei_send_tmo_1,self()),
    do_one_send(Config,self(),c_node_send_tmo_1),
    do_one_send(Config,ei_send_tmo_1,c_node_send_tmo_2),
    ok.


do_one_send(Config,From,CNode) ->
    {_,Host} = split(node()),
    P1 = runner:start(Config, ?send_tmo),
    runner:send_term(P1,{CNode,
                         erlang:get_cookie(),
                         node(),
                         get_group(Config)}),
    {term, X} = runner:get_term(P1, 10000),
    true = is_integer(X),
    CNode1 = join(CNode,Host),
    Term1 = {hej,[hopp,{i,[lingon,"skogen"]}]},
    {test,CNode1} ! {From,1,Term1},
    ok = receive
             Term1 ->
                 ok
         after 2000 ->
                   error
         end,
    {term, 0} = runner:get_term(P1, 10000),
    runner:recv_eot(P1).

ei_send_failure_tmo(Config) when is_list(Config) ->
    register(ei_send_tmo_1,self()),
    do_one_send_failure(Config,self(),cccc1,c_nod_send_tmo_3),
    do_one_send_failure(Config,ei_send_tmo_1,cccc2,c_nod_send_tmo_4),
    ok.

do_one_send_failure(Config,From,FakeName,CName) ->
    {_,Host} = split(node()),
    OurName = join(FakeName,Host),
    Node = join(CName,Host),
    LSocket = case gen_tcp:listen(0, [{active, false}, {packet,2}]) of
                  {ok, Socket} ->
                      Socket;
                  Else ->
                      exit(Else)
              end,
    EpmdSocket = epmd_register(OurName, LSocket),
    P3 = runner:start(Config, ?send_tmo),
    Cookie = kaksmula_som_ingen_bryr_sig_om,
    runner:send_term(P3,{CName,
                         Cookie,
                         OurName,
                         default}),
    SocketB = case gen_tcp:accept(LSocket) of
                  {ok, Socket1} ->
                      Socket1;
                  Else2 ->
                      exit(Else2)
              end,
    {hidden,Node} = recv_name(SocketB),  % See 1)
    send_status(SocketB, ok),
    MyChallengeB = gen_challenge(),
    send_challenge(SocketB, OurName, MyChallengeB),
    HisChallengeB = recv_challenge_reply(SocketB,
                                         MyChallengeB,
                                         Cookie),
    DigestB = gen_digest(HisChallengeB,Cookie),
    send_challenge_ack(SocketB, DigestB),
    inet:setopts(SocketB, [{active, false},
                           {packet, 4}]),
    {term, X} = runner:get_term(P3, 10000),
    true = is_integer(X),
    Message = [112,term_to_binary({6,self(),'',test}),
               term_to_binary({From,50000,
                               {app,["lapp",{sa,["att",du,{slapp,
                                                           sitta}]}]}})],
    gen_tcp:send(SocketB,Message),

    %% At this point the test program starts sending messages (max 50000). Since
    %% we're not receiving, eventually the send buffer fills up. Then no more 
    %% sending is possible and select() times out. The number of messages sent
    %% before this happens is returned in Iters. The timeout value for get_term/2
    %% must be large enough so there's time for the select() to time out and
    %% the test program to return the error tuple (below).

    {term,{Res,ETO,Iters,ETO}} = runner:get_term(P3, 20000),
    runner:recv_eot(P3),
    true = ((Res < 0) and (Iters > 0)),
    gen_tcp:close(SocketB),
    gen_tcp:close(EpmdSocket),
    ok.


%% Check accept with timeouts.
ei_connect_unreachable_tmo() -> [{require, test_host_not_reachable}].

ei_connect_unreachable_tmo(Config) when is_list(Config) ->
    DummyNode = make_and_check_dummy(),
    P = runner:start(Config, ?connect_tmo),
    runner:send_term(P,{c_nod_connect_tmo_1,
                        kaksmula_som_ingen_bryr_sig_om,
                        DummyNode,
                        default}),
    {term,{-3,ETimedout,ETimedout}} = runner:get_term(P, 10000),
    runner:recv_eot(P),
    ok.

ei_connect_tmo(Config) when is_list(Config) ->
    Flags = ?COMPULSORY_DFLAGS bor ?DFLAG_MANDATORY_25_DIGEST,

    P2 = runner:start(Config, ?connect_tmo),
    runner:send_term(P2,{c_nod_connect_tmo_2,
                         erlang:get_cookie(),
                         node(),
                         get_group(Config)}),
    {term, X} = runner:get_term(P2, 10000),
    runner:recv_eot(P2),
    true = is_integer(X),

    %% Aborted handshake test...
    {_,Host} = split(node()),
    OurName = join(cccc,Host),
    Node = join(c_nod_connect_tmo_3,Host),
    LSocket = case gen_tcp:listen(0, [{active, false}, {packet,2}]) of
                  {ok, Socket} ->
                      Socket;
                  Else ->
                      exit(Else)
              end,
    EpmdSocket = epmd_register(OurName, LSocket),
    P3 = runner:start(Config, ?connect_tmo),
    Cookie = kaksmula_som_ingen_bryr_sig_om,
    runner:send_term(P3,{c_nod_connect_tmo_3,
                         Cookie,
                         OurName,
                         get_group(Config)}),
    SocketB = case gen_tcp:accept(LSocket) of
                  {ok, Socket1} ->
                      Socket1;
                  Else2 ->
                      exit(Else2)
              end,
    {hidden,Node} = recv_name(SocketB),  % See 1)
    send_status(SocketB, ok),
    MyChallengeB = gen_challenge(),
    send_challenge(SocketB, OurName, MyChallengeB, Flags),
    _HisChallengeB = recv_challenge_reply(SocketB,
                                          MyChallengeB,
                                          Cookie),
    {term,{-1,ETimedout,ETimedout}} = runner:get_term(P3, 10000),
    runner:recv_eot(P3),
    gen_tcp:close(SocketB),
    gen_tcp:close(EpmdSocket),
    ok.


%% Check accept with timeouts.
ei_accept_tmo(Config) when is_list(Config) ->
    [begin
         io:format("Test assumed ver=~p\n",
                   [AssumedVer]),
         do_ei_accept_tmo(Config, AssumedVer)
     end
     || AssumedVer <- lists:seq(?EI_DIST_LOWEST_ASSUMED, ?EI_DIST_HIGH)],
    ok.

do_ei_accept_tmo(Config, AssumedVer) ->
    Flags = ?COMPULSORY_DFLAGS bor ?DFLAG_MANDATORY_25_DIGEST,

    P = runner:start(Config, ?accept_tmo),
    runner:send_term(P,{c_nod_som_ingen_kontaktar_1,
                        kaksmula_som_ingen_bryr_sig_om,
                        get_group(Config)}),
    {term,{-1,ETimedout,ETimedout}} = runner:get_term(P, 10000),
    runner:recv_eot(P),

    P2 = runner:start(Config, ?accept_tmo),
    runner:send_term(P2,{c_nod_som_vi_kontaktar_1,
                         erlang:get_cookie(),
                         get_group(Config)}),
    receive after 1000 -> ok end,
    CNode1 = make_node(c_nod_som_vi_kontaktar_1),
    {ignored,CNode1} ! tjenare,
    {term, X} = runner:get_term(P2, 10000),
    runner:recv_eot(P2),
    true = is_integer(X),

    normal_accept(Config, AssumedVer, Flags),

    ok.

normal_accept(Config, AssumedVer, Flags) ->
    P = runner:start(Config, ?accept_tmo),
    runner:send_term(P,{c_nod_som_vi_kontaktar_2,
                         erlang:get_cookie(),
                         get_group(Config)}),
    receive after 1000 -> ok end,
    CNode2 = make_node(c_nod_som_vi_kontaktar_2),
    {NA,NB} = split(CNode2),
    {_,Host} = split(node()),
    OurName = join(ccc,Host),
    {port,PortNo,?EI_DIST_HIGH} = erl_epmd:port_please(NA,NB),
    {ok, SocketA} = gen_tcp:connect(atom_to_list(NB),PortNo,
                                    [{active,false},
                                     {packet,2}]),
    send_name(SocketA, OurName, AssumedVer, Flags),
    ok = recv_status(SocketA),
    {hidden,_Node,HisChallengeA} = recv_challenge(SocketA), % See 1)
    _OurChallengeA = gen_challenge(),
    _OurDigestA = gen_digest(HisChallengeA, erlang:get_cookie()),
    %% Dont do the last two steps of the connection setup...
    %% send_challenge_reply(SocketA, OurChallengeA, OurDigestA),
    %% ok = recv_challenge_ack(SocketA, OurChallengeA, erlang:get_cookie()),
    {term, {-1,ETimedout,ETimedout}} = runner:get_term(P, 10000),
    runner:recv_eot(P),
    gen_tcp:close(SocketA).

normal_connect(Config, Flags) ->
    {_,Host} = split(node()),
    OurName = join(cccc,Host),
    Node = join(c_nod_connect_tmo_3,Host),
    LSocket = case gen_tcp:listen(0, [{active, false}, {packet,2}]) of
                  {ok, Socket} ->
                      Socket;
                  Else ->
                      exit(Else)
              end,
    EpmdSocket = epmd_register(OurName, LSocket),
    P3 = runner:start(Config, ?connect_tmo),
    Cookie = kaksmula_som_ingen_bryr_sig_om,
    runner:send_term(P3, {c_nod_connect_tmo_3,
                          Cookie,
                          OurName,
                          get_group(Config)}),
    SocketB = case gen_tcp:accept(LSocket) of
                  {ok, Socket1} ->
                      Socket1;
                  Else2 ->
                      exit(Else2)
              end,
    {hidden,Node} = recv_name(SocketB),
    send_status(SocketB, ok),
    MyChallengeB = gen_challenge(),
    send_challenge(SocketB, OurName, MyChallengeB, Flags),
    gen_tcp:close(SocketB),
    gen_tcp:close(EpmdSocket),
    ok.

make_node(X) ->
    list_to_atom(atom_to_list(X) ++ "@" ++ 
                 hd(tl(string:tokens(atom_to_list(node()),"@")))).


make_and_check_dummy() ->
    % First check that the host has an ip and is *not* reachable
    HostNotReachable = ct:get_config(test_host_not_reachable),
    case gen_tcp:connect(HostNotReachable, 23, [{active,false}],5000) of
        {error,timeout} -> ok;
        {error,ehostunreach} -> ok
    end,

    list_to_atom("dummy@"++HostNotReachable).

%% Test that erl_interface sets the appropriate distributions flags.
ei_dflags(Config) ->
    AssumedVer = 5,
    OurVer = 6,

    %% Test compatibility with OTP 24 and earlier.
    normal_connect(Config, ?COMPULSORY_DFLAGS),
    normal_accept(Config, AssumedVer, ?COMPULSORY_DFLAGS),
    normal_accept(Config, OurVer, ?COMPULSORY_DFLAGS),

    %% Test compatibility with future versions.
    normal_connect(Config, ?DFLAG_MANDATORY_25_DIGEST bor ?DFLAGS_MANDATORY_26),
    normal_accept(Config, AssumedVer, ?DFLAG_MANDATORY_25_DIGEST bor ?DFLAGS_MANDATORY_26),
    normal_accept(Config, OurVer, ?DFLAG_MANDATORY_25_DIGEST bor ?DFLAGS_MANDATORY_26),

    ok.

%%
%% Stolen from the erl_distribution_wb_test in kernel
%% To be able to do partial handshakes...
%%

-define(to_port(Socket, Data),
        case inet_tcp:send(Socket, Data) of
            {error, closed} ->
                self() ! {tcp_closed, Socket},
                {error, closed};
            R ->
                R
        end).

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

%%
%% Handshake utilities
%%

%%
%% MD5 hashing
%%

%% This is no proper random number, but that is not really important in 
%% this test
gen_challenge() ->
    {_,_,N} = os:timestamp(),
    N.

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
        {error, _} -> ?shutdown(could_not_send_status);
        _ -> true
    end.


recv_status(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, [$s|StrStat]} ->
            list_to_atom(StrStat);
        Bad ->
            exit(Bad)
    end.

send_challenge(Socket, Node, Challenge) ->
    DefaultFlags = ?COMPULSORY_DFLAGS bor ?DFLAG_MANDATORY_25_DIGEST,
    send_challenge(Socket, Node, Challenge, DefaultFlags).

send_challenge(Socket, Node, Challenge, Flags) ->
    NodeName = atom_to_binary(Node, latin1),
    NameLen = byte_size(NodeName),
    Creation = erts_internal:get_creation(),
    ?to_port(Socket, [$N,
                      << Flags:64,
                         Challenge:32,
                         Creation:32,
                         NameLen:16>>,
                      NodeName]).

recv_challenge(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok,[$N, F7,F6,F5,F4,F3,F2,F1,F0, CA3,CA2,CA1,CA0,
             _Cr3,_Cr2,_Cr1,_Cr0, NL1,NL0 | Rest]} ->
            <<Flags:64>> = <<F7,F6,F5,F4,F3,F2,F1,F0>>,
            verify_flags(Flags),
            Type = flags_to_type(Flags),
            NameLen = ?u16(NL1,NL0),
            {NodeName,_} = lists:split(NameLen, Rest),
            Node = list_to_atom(NodeName),
            Challenge = ?u32(CA3,CA2,CA1,CA0),
            %%Creation = ?u32(Cr3,Cr2,Cr1,Cr0),
            %%true = (Creation =/= 0),
            {Type,Node,Challenge};

        _ ->
            ?shutdown(no_node)
    end.

flags_to_type(Flags) ->
    case Flags band ?DFLAG_PUBLISHED of
        0 ->
            hidden;
        _ ->
            normal
    end.

%send_challenge_reply(Socket, Challenge, Digest) ->
%    ?to_port(Socket, [$r,?int32(Challenge),Digest]).

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
        Other ->
            ?shutdown({recv_challenge_reply,Other})
    end.

send_challenge_ack(Socket, Digest) ->
    ?to_port(Socket, [$a,Digest]).

%recv_challenge_ack(Socket, ChallengeB, CookieA) ->
%    case gen_tcp:recv(Socket, 0) of
%        {ok,[$a | SumB]} when length(SumB) == 16 ->
%            SumA = gen_digest(ChallengeB, CookieA),
%            if SumB == SumA ->
%                   ok;
%               true ->
%                   ?shutdown(bad_challenge_ack)
%            end;
%        _ ->
%            ?shutdown(bad_challenge_ack)
%    end.

send_name(Socket, MyNode, AssumedVer, Flags0) ->
    Flags = Flags0 bor?DFLAG_HANDSHAKE_23,
    NodeName = atom_to_binary(MyNode, latin1),
    if AssumedVer =:= 5 ->
            ?to_port(Socket, [$n,?int16(?EI_DIST_HIGH),?int32(Flags),NodeName]);
       AssumedVer >= 6 ->
            Creation = erts_internal:get_creation(),
            ?to_port(Socket, [$N,
                              <<Flags:64,
                                Creation:32,
                                (byte_size(NodeName)):16>>,
                              NodeName])
    end.

recv_name(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok,[$N, F7,F6,F5,F4,F3,F2,F1,F0, _Cr3,_Cr2,_Cr1,_Cr0, NL1, NL0 | Rest]} ->
            {OtherNode, _Residue} = lists:split(?u16(NL1,NL0), Rest),
            <<Flags:64>> = <<F7,F6,F5,F4,F3,F2,F1,F0>>,
            verify_flags(Flags),
            Type = flags_to_type(Flags),
            {Type, list_to_atom(OtherNode)};
        Res ->
            ?shutdown({no_node,Res})
    end.

%%
%% tell_name is for old handshake
%%
%tell_name(Socket, MyNode0, Version) ->
%    MyNode = atom_to_list(MyNode0),
%    {ok, {{Ip1,Ip2,Ip3,Ip4}, _}} = inet:sockname(Socket),
%    ?to_port(Socket, [$h,?int16(Version),Ip1,Ip2,Ip3,Ip4] ++ MyNode).

%%
%% The communication with EPMD follows
%%
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
                                  ?int16(?EI_DIST_HIGH),
                                  ?int16(?EI_DIST_LOW),
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
                [$y, 0, Cr1,Cr0] ->
                    {alive, Socket, ?u16(Cr1,Cr0)};
                [$v, 0, Cr3,Cr2,Cr1,Cr0] ->
                    {alive, Socket, ?u32(Cr3,Cr2,Cr1,Cr0)};
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


epmd_register(NodeName, ListenSocket) ->
    {ok,{_,TcpPort}} = inet:sockname(ListenSocket),
    case do_register_node(NodeName, TcpPort) of
        {alive, Socket, _Creation} ->
            Socket;
        Other ->
            exit(Other)
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

%% Build a nodename
join(Name,Host) ->
    list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Host)).

get_epmd_port() ->
    case init:get_argument(epmd_port) of
        {ok, [[PortStr|_]|_]} when is_list(PortStr) ->
            list_to_integer(PortStr);
        error ->
            4369 % Default epmd port
    end.
