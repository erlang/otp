%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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
-module(ei_tmo_SUITE).

-include("test_server.hrl").
-include_lib("kernel/include/inet.hrl").
-include("ei_tmo_SUITE_data/ei_tmo_test_cases.hrl").

-define(dummy_host,test01).

-export([all/1, init_per_testcase/2, fin_per_testcase/2,
	 framework_check/1, ei_accept_tmo/1, ei_connect_tmo/1, ei_send_tmo/1,
	 ei_recv_tmo/1]).

all(suite) -> [framework_check,ei_accept_tmo,ei_connect_tmo,
	       ei_send_tmo,ei_recv_tmo].

init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?t:minutes(1)),
    % test if platform is vxworks_simso
    ?line {_,Host} = split(node()),
    Bool = case atom_to_list(Host) of
		[$v,$x,$s,$i,$m | _] -> true;
		_ -> false
	    end,
    [{vxsim,Bool},{watchdog, Dog}|Config].

fin_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

framework_check(doc) ->
    ["Check the framework."];
framework_check(suite) ->
    [];
framework_check(Config) when is_list(Config) ->
    %%dbg:tracer(),
    %%dbg:p(self()),
    ?line P = runner:start(?framework_check),
    ?line runner:send_term(P,{hello,world}),
    ?line {term, {hello,world}} = runner:get_term(P),
    ?line runner:recv_eot(P),
    ok.


ei_recv_tmo(doc) ->
    ["Check recv with timeouts."];
ei_recv_tmo(suite) ->
    [];
ei_recv_tmo(Config) when is_list(Config) ->
    ?line do_one_recv(c_node_recv_tmo_1),
    ?line do_one_recv_failure(c_node_recv_tmo_2),
    ok.


do_one_recv(CNode) ->
    ?line {_,Host} = split(node()),
    ?line P1 = runner:start(?recv_tmo),
    ?line runner:send_term(P1,{CNode,
			       erlang:get_cookie(),
			       node()}),
    ?line {term, X} = runner:get_term(P1, 10000),
    ?line true = is_integer(X),
    ?line CNode1 = join(CNode,Host),
    ?line Term1 = {hej,[hopp,{i,[lingon,"skogen"]}]},
    ?line {test,CNode1} ! Term1,
    ?line {term, Term1} = runner:get_term(P1, 10000),
    ?line runner:recv_eot(P1).
    
do_one_recv_failure(CNode) ->
    ?line P1 = runner:start(?recv_tmo),
    ?line runner:send_term(P1,{CNode,
			       erlang:get_cookie(),
			       node()}),
    ?line {term, X} = runner:get_term(P1, 10000),
    ?line true = is_integer(X),
    ?line {term, {Ret,ETimedout,ETimedout}} = runner:get_term(P1, 10000),
    ?line true = (Ret < 0),
    ?line runner:recv_eot(P1).
    

ei_send_tmo(doc) ->
    ["Check send with timeouts."];
ei_send_tmo(suite) ->
    [];
ei_send_tmo(Config) when is_list(Config) ->
    %dbg:tracer(),
    %dbg:p(self()),
    VxSim = ?config(vxsim, Config),
    ?line register(ei_send_tmo_1,self()),
    ?line do_one_send(self(),c_node_send_tmo_1),
    ?line do_one_send(ei_send_tmo_1,c_node_send_tmo_2),
    ?line do_one_send_failure(self(),cccc1,c_nod_send_tmo_3,VxSim),
    ?line do_one_send_failure(ei_send_tmo_1,cccc2,c_nod_send_tmo_4,VxSim),
    ok.
    

do_one_send(From,CNode) ->
    ?line {_,Host} = split(node()),
    ?line P1 = runner:start(?send_tmo),
    ?line runner:send_term(P1,{CNode,
			      erlang:get_cookie(),
			      node()}),
    ?line {term, X} = runner:get_term(P1, 10000),
    ?line true = is_integer(X),
    ?line CNode1 = join(CNode,Host),
    ?line Term1 = {hej,[hopp,{i,[lingon,"skogen"]}]},
    ?line {test,CNode1} ! {From,1,Term1},
    ?line ok = receive
		   Term1 ->
		       ok
	       after 2000 ->
		       error
	       end,
    ?line {term, 0} = runner:get_term(P1, 10000),
    ?line runner:recv_eot(P1).

do_one_send_failure(From,FakeName,CName,VxSim) ->
    ?line {_,Host} = split(node()),
    ?line OurName = join(FakeName,Host),
    ?line Node = join(CName,Host),
    ?line LSocket = case gen_tcp:listen(0, [{active, false}, {packet,2}]) of
			{ok, Socket} ->
			    ?line Socket;
			Else ->
			    ?line exit(Else)
		    end,
    ?line EpmdSocket = register(OurName, LSocket, 1, 5),
    ?line P3 = runner:start(?send_tmo),
    ?line Cookie = kaksmula_som_ingen_bryr_sig_om,
    ?line runner:send_term(P3,{CName,
			      Cookie,
			      OurName}),
    ?line SocketB = case gen_tcp:accept(LSocket) of
		  {ok, Socket1} ->
		      ?line Socket1;
		  Else2 ->
		      ?line exit(Else2)
	      end,
    ?line {hidden,Node,5} = recv_name(SocketB),  % See 1)
    ?line send_status(SocketB, ok),
    ?line MyChallengeB = gen_challenge(),
    ?line send_challenge(SocketB, OurName, MyChallengeB, 5),
    ?line HisChallengeB = recv_challenge_reply(
			    SocketB, 
			    MyChallengeB, 
			    Cookie),
    ?line DigestB = gen_digest(HisChallengeB,Cookie),
    ?line send_challenge_ack(SocketB, DigestB),
    ?line inet:setopts(SocketB, [{active, false},
			  {packet, 4}]),   
    ?line {term, X} = runner:get_term(P3, 10000),
    ?line true = is_integer(X),
    ?line Message = [112,term_to_binary({6,self(),'',test}),
		     term_to_binary({From,10000,
				     {app,["lapp",{sa,["att",du,{slapp,
								 sitta}]}]}})],
    ?line gen_tcp:send(SocketB,Message),

    %% At this point the test program starts sending messages (max 10000). Since 
    %% we're not receiving, eventually the send buffer fills up. Then no more 
    %% sending is possible and select() times out. The number of messages sent
    %% before this happens is returned in Iters. The timeout value for get_term/2
    %% must be large enough so there's time for the select() to time out and
    %% the test program to return the error tuple (below).
    Res0 =
	if VxSim == false ->
		?line {term,{Res,ETO,Iters,ETO}} = runner:get_term(P3, 20000),
		Res;	   
	   true ->				% relax the test for vxsim
		?line case runner:get_term(P3, 20000) of
			  {term,{Res,ETO,Iters,ETO}} -> 
			      Res;
			  {term,{Res,_,Iters,ETO}} -> % EIO?
			      Res
		      end
	end,
    ?line runner:recv_eot(P3),
    ?line true = ((Res0 < 0) and (Iters > 0)),
    ?line gen_tcp:close(SocketB),
    ?line gen_tcp:close(EpmdSocket),
    ok.
    

ei_connect_tmo(doc) ->
    ["Check accept with timeouts."];
ei_connect_tmo(suite) ->
    [];
ei_connect_tmo(Config) when is_list(Config) ->
    %dbg:tracer(),
    %dbg:p(self()),
    VxSim = ?config(vxsim, Config),
    DummyNode = make_and_check_dummy(),
    ?line P = runner:start(?connect_tmo),
    ?line runner:send_term(P,{c_nod_connect_tmo_1,
			      kaksmula_som_ingen_bryr_sig_om,
			      DummyNode}),
    ETimedout =
	if VxSim == false ->
		?line {term,{-3,ETO,ETO}} = runner:get_term(P, 10000),
		?line ETO;
	   true ->				% relax the test for vxsim
		?line case runner:get_term(P, 10000) of
			  {term,{-3,ETO,ETO}} -> 
			      ?line ETO;
			  {term,{-1,_,ETO}} ->	% EHOSTUNREACH = ok
			      ?line ETO
		      end
	end,
    ?line runner:recv_eot(P),
    ?line P2 = runner:start(?connect_tmo),
    ?line runner:send_term(P2,{c_nod_connect_tmo_2,
			       erlang:get_cookie(),
			       node()}),
    ?line {term, X} = runner:get_term(P2, 10000),
    ?line runner:recv_eot(P2),
    ?line true = is_integer(X),
    %% Aborted handshake test...
    ?line {_,Host} = split(node()),
    ?line OurName = join(cccc,Host),
    ?line Node = join(c_nod_connect_tmo_3,Host),
    ?line LSocket = case gen_tcp:listen(0, [{active, false}, {packet,2}]) of
			{ok, Socket} ->
			    ?line Socket;
			Else ->
			    ?line exit(Else)
		    end,
    ?line EpmdSocket = register(OurName, LSocket, 1, 5),
    ?line P3 = runner:start(?connect_tmo),
    ?line Cookie = kaksmula_som_ingen_bryr_sig_om,
    ?line runner:send_term(P3,{c_nod_connect_tmo_3,
			      Cookie,
			      OurName}),
    ?line SocketB = case gen_tcp:accept(LSocket) of
		  {ok, Socket1} ->
		      ?line Socket1;
		  Else2 ->
		      ?line exit(Else2)
	      end,
    ?line {hidden,Node,5} = recv_name(SocketB),  % See 1)
    ?line send_status(SocketB, ok),
    ?line MyChallengeB = gen_challenge(),
    ?line send_challenge(SocketB, OurName, MyChallengeB, 5),
    ?line HisChallengeB = recv_challenge_reply(
			    SocketB, 
			    MyChallengeB, 
			    Cookie),
    ?line {term,{-1,ETimedout,ETimedout}} = runner:get_term(P3, 10000),
    ?line runner:recv_eot(P3),
    ?line gen_tcp:close(SocketB),
    ?line gen_tcp:close(EpmdSocket),
    ok.
    

ei_accept_tmo(doc) ->
    ["Check accept with timeouts."];
ei_accept_tmo(suite) ->
    [];
ei_accept_tmo(Config) when is_list(Config) ->
    %%dbg:tracer(),
    %%dbg:p(self()),
    ?line P = runner:start(?accept_tmo),
    ?line runner:send_term(P,{c_nod_som_ingen_kontaktar_1,
			      kaksmula_som_ingen_bryr_sig_om}),
    ?line {term,{-1,ETimedout,ETimedout}} = runner:get_term(P, 10000),
    ?line runner:recv_eot(P),
    ?line P2 = runner:start(?accept_tmo),
    ?line runner:send_term(P2,{c_nod_som_vi_kontaktar_1,
			      erlang:get_cookie()}),
    ?line receive after 1000 -> ok end,
    ?line CNode1 = make_node(c_nod_som_vi_kontaktar_1),
    ?line {ignored,CNode1} ! tjenare,
    ?line {term, X} = runner:get_term(P2, 10000),
    ?line runner:recv_eot(P2),
    ?line true = is_integer(X),
    ?line P3 = runner:start(?accept_tmo),
    ?line runner:send_term(P3,{c_nod_som_vi_kontaktar_2,
			      erlang:get_cookie()}),
    ?line receive after 1000 -> ok end,
    ?line CNode2 = make_node(c_nod_som_vi_kontaktar_2),
    ?line {NA,NB} = split(CNode2),
    ?line {_,Host} = split(node()),
    ?line OurName = join(ccc,Host),
    ?line {port,PortNo,_} = erl_epmd:port_please(NA,NB),
    ?line {ok, SocketA} = gen_tcp:connect(atom_to_list(NB),PortNo,
					  [{active,false},
					   {packet,2}]),
    ?line send_name(SocketA,OurName,5),
    ?line ok = recv_status(SocketA),
    ?line {hidden,Node,5,HisChallengeA} = recv_challenge(SocketA), % See 1)
    ?line OurChallengeA = gen_challenge(),
    ?line OurDigestA = gen_digest(HisChallengeA, erlang:get_cookie()),
    %% Dont do the last two steps of the connection setup...
    %% send_challenge_reply(SocketA, OurChallengeA, OurDigestA),
    %% ok = recv_challenge_ack(SocketA, OurChallengeA, erlang:get_cookie()),
    ?line {term, {-1,ETimedout,ETimedout}} = runner:get_term(P3, 10000),
    ?line runner:recv_eot(P3),
    ?line gen_tcp:close(SocketA),
    ok.

make_node(X) ->
    list_to_atom(atom_to_list(X) ++ "@" ++ 
		 hd(tl(string:tokens(atom_to_list(node()),"@")))).


make_and_check_dummy() ->
    % First check that the host has an ip and is *not* reachable
    ?line case gen_tcp:connect(?dummy_host,23,[{active,false}],5000) of
	      {error,timeout} -> ok;
	      {error,ehostunreach} -> ok
	  end,

    list_to_atom("dummy@"++atom_to_list(?dummy_host)).

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
    {ok, {{Ip1,Ip2,Ip3,Ip4}, _}} = inet:sockname(Socket),
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
%% tell_name is for old handshake
%%
tell_name(Socket, MyNode0, Version) ->
    MyNode = atom_to_list(MyNode0),
    {ok, {{Ip1,Ip2,Ip3,Ip4}, _}} = inet:sockname(Socket),
    ?to_port(Socket, [$h,?int16(Version),Ip1,Ip2,Ip3,Ip4] ++ 
             MyNode).

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
	{alive, Socket, Creation} ->
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

%% Build a simple distribution message
build_message(Cookie) ->
    [$?,term_to_binary({6,self(),Cookie,rex}),term_to_binary(plupp)].

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
	    Header = erlang:binary_to_term(B1),
	    Siz = size(term_to_binary(Header)),
	    {_,B2} = erlang:split_binary(B1,Siz),
	    Message = case (catch erlang:binary_to_term(B2)) of
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

get_epmd_port() ->
    case init:get_argument(epmd_port) of
        {ok, [[PortStr|_]|_]} when is_list(PortStr) ->
            list_to_integer(PortStr);
        error ->
            4369 % Default epmd port
    end.
