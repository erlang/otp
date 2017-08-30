%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

-export([all/0, suite/0,
         init_per_testcase/2, end_per_testcase/2,
         framework_check/1, ei_accept_tmo/1, ei_connect_tmo/1, ei_send_tmo/1,
	 ei_connect_tmo/0,
         ei_recv_tmo/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() -> 
    [framework_check, ei_accept_tmo, ei_connect_tmo,
     ei_send_tmo, ei_recv_tmo].

init_per_testcase(_Case, Config) ->
    % test if platform is vxworks_simso
    {_,Host} = split(node()),
    Bool = case atom_to_list(Host) of
               [$v,$x,$s,$i,$m | _] -> true;
               _ -> false
           end,
    [{vxsim,Bool}|Config].

end_per_testcase(_Case, _Config) ->
    ok.

%% Check the framework.
framework_check(Config) when is_list(Config) ->
    %%dbg:tracer(),
    %%dbg:p(self()),
    P = runner:start(?framework_check),
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
    P1 = runner:start(?recv_tmo),
    runner:send_term(P1,{CNode,
                         erlang:get_cookie(),
                         node()}),
    {term, X} = runner:get_term(P1, 10000),
    true = is_integer(X),
    CNode1 = join(CNode,Host),
    Term1 = {hej,[hopp,{i,[lingon,"skogen"]}]},
    {test,CNode1} ! Term1,
    {term, Term1} = runner:get_term(P1, 10000),
    runner:recv_eot(P1).

do_one_recv_failure(Config,CNode) ->
    P1 = runner:start(?recv_tmo),
    runner:send_term(P1,{CNode,
                         erlang:get_cookie(),
                         node()}),
    {term, X} = runner:get_term(P1, 10000),
    true = is_integer(X),
    {term, {Ret,ETimedout,ETimedout}} = runner:get_term(P1, 10000),
    true = (Ret < 0),
    runner:recv_eot(P1).


%% Check send with timeouts.
ei_send_tmo(Config) when is_list(Config) ->
    %dbg:tracer(),
    %dbg:p(self()),
    VxSim = proplists:get_value(vxsim, Config),
    register(ei_send_tmo_1,self()),
    do_one_send(Config,self(),c_node_send_tmo_1),
    do_one_send(Config,ei_send_tmo_1,c_node_send_tmo_2),
    do_one_send_failure(Config,self(),cccc1,c_nod_send_tmo_3,VxSim),
    do_one_send_failure(Config,ei_send_tmo_1,cccc2,c_nod_send_tmo_4,VxSim),
    ok.


do_one_send(Config,From,CNode) ->
    {_,Host} = split(node()),
    P1 = runner:start(?send_tmo),
    runner:send_term(P1,{CNode,
                         erlang:get_cookie(),
                         node()}),
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

do_one_send_failure(Config,From,FakeName,CName,VxSim) ->
    {_,Host} = split(node()),
    OurName = join(FakeName,Host),
    Node = join(CName,Host),
    LSocket = case gen_tcp:listen(0, [{active, false}, {packet,2}]) of
                  {ok, Socket} ->
                      Socket;
                  Else ->
                      exit(Else)
              end,
    EpmdSocket = register(OurName, LSocket, 1, 5),
    P3 = runner:start(?send_tmo),
    Cookie = kaksmula_som_ingen_bryr_sig_om,
    runner:send_term(P3,{CName,
                         Cookie,
                         OurName}),
    SocketB = case gen_tcp:accept(LSocket) of
                  {ok, Socket1} ->
                      Socket1;
                  Else2 ->
                      exit(Else2)
              end,
    {hidden,Node,5} = recv_name(SocketB),  % See 1)
    send_status(SocketB, ok),
    MyChallengeB = gen_challenge(),
    send_challenge(SocketB, OurName, MyChallengeB, 5),
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

    Res0 = if VxSim == false ->
                  {term,{Res,ETO,Iters,ETO}} = runner:get_term(P3, 20000),
                  Res;
              true ->   % relax the test for vxsim
                  case runner:get_term(P3, 20000) of
                      {term,{Res,ETO,Iters,ETO}} ->
                          Res;
                      {term,{Res,_,Iters,_ETO}} -> % EIO?
                          Res
                  end
           end,
    runner:recv_eot(P3),
    true = ((Res0 < 0) and (Iters > 0)),
    gen_tcp:close(SocketB),
    gen_tcp:close(EpmdSocket),
    ok.


%% Check accept with timeouts.
ei_connect_tmo() -> [{require, test_host_not_reachable}].

ei_connect_tmo(Config) when is_list(Config) ->
    %dbg:tracer(),
    %dbg:p(self()),
    VxSim = proplists:get_value(vxsim, Config),
    DummyNode = make_and_check_dummy(),
    P = runner:start(?connect_tmo),
    runner:send_term(P,{c_nod_connect_tmo_1,
                        kaksmula_som_ingen_bryr_sig_om,
                        DummyNode}),
    ETimedout =
    if VxSim == false ->
           {term,{-3,ETO,ETO}} = runner:get_term(P, 10000),
           ETO;
       true ->				% relax the test for vxsim
           case runner:get_term(P, 10000) of
               {term,{-3,ETO,ETO}} ->
                   ETO;
               {term,{-1,_,ETO}} ->	% EHOSTUNREACH = ok
                   ETO
           end
    end,
    runner:recv_eot(P),
    P2 = runner:start(?connect_tmo),
    runner:send_term(P2,{c_nod_connect_tmo_2,
                         erlang:get_cookie(),
                         node()}),
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
    EpmdSocket = register(OurName, LSocket, 1, 5),
    P3 = runner:start(?connect_tmo),
    Cookie = kaksmula_som_ingen_bryr_sig_om,
    runner:send_term(P3,{c_nod_connect_tmo_3,
                         Cookie,
                         OurName}),
    SocketB = case gen_tcp:accept(LSocket) of
                  {ok, Socket1} ->
                      Socket1;
                  Else2 ->
                      exit(Else2)
              end,
    {hidden,Node,5} = recv_name(SocketB),  % See 1)
    send_status(SocketB, ok),
    MyChallengeB = gen_challenge(),
    send_challenge(SocketB, OurName, MyChallengeB, 5),
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
    %%dbg:tracer(),
    %%dbg:p(self()),
    P = runner:start(?accept_tmo),
    runner:send_term(P,{c_nod_som_ingen_kontaktar_1,
                        kaksmula_som_ingen_bryr_sig_om}),
    {term,{-1,ETimedout,ETimedout}} = runner:get_term(P, 10000),
    runner:recv_eot(P),
    P2 = runner:start(?accept_tmo),
    runner:send_term(P2,{c_nod_som_vi_kontaktar_1,
                         erlang:get_cookie()}),
    receive after 1000 -> ok end,
    CNode1 = make_node(c_nod_som_vi_kontaktar_1),
    {ignored,CNode1} ! tjenare,
    {term, X} = runner:get_term(P2, 10000),
    runner:recv_eot(P2),
    true = is_integer(X),
    P3 = runner:start(?accept_tmo),
    runner:send_term(P3,{c_nod_som_vi_kontaktar_2,
                         erlang:get_cookie()}),
    receive after 1000 -> ok end,
    CNode2 = make_node(c_nod_som_vi_kontaktar_2),
    {NA,NB} = split(CNode2),
    {_,Host} = split(node()),
    OurName = join(ccc,Host),
    {port,PortNo,_} = erl_epmd:port_please(NA,NB),
    {ok, SocketA} = gen_tcp:connect(atom_to_list(NB),PortNo,
                                    [{active,false},
                                     {packet,2}]),
    send_name(SocketA,OurName,5),
    ok = recv_status(SocketA),
    {hidden,_Node,5,HisChallengeA} = recv_challenge(SocketA), % See 1)
    _OurChallengeA = gen_challenge(),
    _OurDigestA = gen_digest(HisChallengeA, erlang:get_cookie()),
    %% Dont do the last two steps of the connection setup...
    %% send_challenge_reply(SocketA, OurChallengeA, OurDigestA),
    %% ok = recv_challenge_ack(SocketA, OurChallengeA, erlang:get_cookie()),
    {term, {-1,ETimedout,ETimedout}} = runner:get_term(P3, 10000),
    runner:recv_eot(P3),
    gen_tcp:close(SocketA),
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

-define(DFLAG_PUBLISHED,1).
-define(DFLAG_ATOM_CACHE,2).
-define(DFLAG_EXTENDED_REFERENCES,4).
-define(DFLAG_EXTENDED_PIDS_PORTS,16#100).
-define(DFLAG_NEW_FLOATS,16#800).
-define(DFLAG_DIST_MONITOR,8).

%% From R9 and forward extended references is compulsory
%% From 14 and forward new float is compulsory
-define(COMPULSORY_DFLAGS, (?DFLAG_EXTENDED_REFERENCES bor ?DFLAG_EXTENDED_PIDS_PORTS bor ?DFLAG_NEW_FLOATS)).

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
%% The differrent stages of the MD5 handshake
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
        _ ->
            ?shutdown(no_node)
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

send_name(Socket, MyNode0, Version) ->
    send_name(Socket, MyNode0, Version, ?COMPULSORY_DFLAGS).
send_name(Socket, MyNode0, Version, Flags) ->
    MyNode = atom_to_list(MyNode0),
    ?to_port(Socket, [$n,?int16(Version),?int32(Flags)] ++ 
             MyNode).

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
               0 -> hidden;
               _ -> normal
           end,
    {Type, list_to_atom(OtherNode), 
     ?u16(VersionA,VersionB)};
get_name(Data) ->
    ?shutdown(Data).

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
