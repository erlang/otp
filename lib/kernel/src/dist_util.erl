%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%%%----------------------------------------------------------------------
%%% Purpose : The handshake of a streamed distribution connection
%%%           in a separate file to make it usable for other
%%%           distribution protocols.
%%%----------------------------------------------------------------------

-module(dist_util).

%%-compile(export_all).
-export([handshake_we_started/1, handshake_other_started/1,
	 start_timer/1, setup_timer/2, 
	 reset_timer/1, cancel_timer/1,
	 shutdown/3, shutdown/4]).

-import(error_logger,[error_msg/2]).

-include("dist_util.hrl").
-include("dist.hrl").

-ifdef(DEBUG).
-define(shutdown_trace(A,B), io:format(A,B)).
-else.
-define(shutdown_trace(A,B), noop).
-endif.

-define(to_port(FSend, Socket, Data),
	case FSend(Socket, Data) of
	    {error, closed} ->
		self() ! {tcp_closed, Socket},
	        {error, closed};
	    R ->
	        R
        end).


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

-record(tick, {read = 0,
	       write = 0,
	       tick = 0,
	       ticked = 0
	       }).

remove_flag(Flag, Flags) ->
    case Flags band Flag of
	0 ->
	    Flags;
	_ ->
	    Flags - Flag
    end.

adjust_flags(ThisFlags, OtherFlags) ->
    case (?DFLAG_PUBLISHED band ThisFlags) band OtherFlags of
	0 ->
	    {remove_flag(?DFLAG_PUBLISHED, ThisFlags),
	     remove_flag(?DFLAG_PUBLISHED, OtherFlags)};
	_ ->
	    {ThisFlags, OtherFlags}
    end.

publish_flag(hidden, _) ->
    0;
publish_flag(_, OtherNode) ->
    case net_kernel:publish_on_node(OtherNode) of
	true ->
	    ?DFLAG_PUBLISHED;
	_ ->
	    0
    end.

make_this_flags(RequestType, OtherNode) ->
    publish_flag(RequestType, OtherNode) bor
	%% The parenthesis below makes the compiler generate better code.
	(?DFLAG_EXPORT_PTR_TAG bor
	 ?DFLAG_EXTENDED_PIDS_PORTS bor
	 ?DFLAG_EXTENDED_REFERENCES bor
	 ?DFLAG_DIST_MONITOR bor
	 ?DFLAG_FUN_TAGS bor
	 ?DFLAG_DIST_MONITOR_NAME bor
	 ?DFLAG_HIDDEN_ATOM_CACHE bor
	 ?DFLAG_NEW_FUN_TAGS bor
	 ?DFLAG_BIT_BINARIES bor
	 ?DFLAG_NEW_FLOATS bor
	 ?DFLAG_UNICODE_IO bor
	 ?DFLAG_DIST_HDR_ATOM_CACHE bor
	 ?DFLAG_SMALL_ATOM_TAGS bor
	 ?DFLAG_UTF8_ATOMS bor
	 ?DFLAG_MAP_TAG bor
	 ?DFLAG_BIG_CREATION).

handshake_other_started(#hs_data{request_type=ReqType}=HSData0) ->
    {PreOtherFlags,Node,Version} = recv_name(HSData0),
    PreThisFlags = make_this_flags(ReqType, Node),
    {ThisFlags, OtherFlags} = adjust_flags(PreThisFlags,
					   PreOtherFlags),
    HSData = HSData0#hs_data{this_flags=ThisFlags,
			     other_flags=OtherFlags,
			     other_version=Version,
			     other_node=Node,
			     other_started=true},
    check_dflags(HSData),
    is_allowed(HSData),
    ?debug({"MD5 connection from ~p (V~p)~n",
	    [Node, HSData#hs_data.other_version]}),
    mark_pending(HSData),
    {MyCookie,HisCookie} = get_cookies(Node),
    ChallengeA = gen_challenge(),
    send_challenge(HSData, ChallengeA),
    reset_timer(HSData#hs_data.timer),
    ChallengeB = recv_challenge_reply(HSData, ChallengeA, MyCookie),
    send_challenge_ack(HSData, gen_digest(ChallengeB, HisCookie)),
    ?debug({dist_util, self(), accept_connection, Node}),
    connection(HSData);

handshake_other_started(OldHsData) when element(1,OldHsData) =:= hs_data ->
    handshake_other_started(convert_old_hsdata(OldHsData)).


%%
%% check if connecting node is allowed to connect
%% with allow-node-scheme
%%
is_allowed(#hs_data{other_node = Node, 
		    allowed = Allowed} = HSData) ->
    case lists:member(Node, Allowed) of
	false when Allowed =/= [] ->
	    send_status(HSData, not_allowed),
	    error_msg("** Connection attempt from "
		      "disallowed node ~w ** ~n", [Node]),
	    ?shutdown2(Node, {is_allowed, not_allowed});
	_ -> true
    end.

%%
%% Check that both nodes can handle the same types of extended
%% node containers. If they can not, abort the connection.
%%
check_dflags(#hs_data{other_node = Node,
                      other_flags = OtherFlags,
                      other_started = OtherStarted} = HSData) ->

    Mandatory = [{?DFLAG_EXTENDED_REFERENCES, "EXTENDED_REFERENCES"},
                 {?DFLAG_EXTENDED_PIDS_PORTS, "EXTENDED_PIDS_PORTS"},
                 {?DFLAG_UTF8_ATOMS, "UTF8_ATOMS"}],
    Missing = lists:filtermap(fun({Bit, Str}) ->
                                      case Bit band OtherFlags of
                                          Bit -> false;
                                          0 -> {true, Str}
                                      end
                              end,
                              Mandatory),
    case Missing of
        [] ->
            ok;
        _ ->
	    case OtherStarted of
		true ->
		    send_status(HSData, not_allowed),
		    Dir = "from",
		    How = "rejected";
	        _ ->
		    Dir = "to",
		    How = "aborted"
	    end,
	    error_msg("** ~w: Connection attempt ~s node ~w ~s "
		      "since it cannot handle ~p."
		      "**~n", [node(), Dir, Node, How, Missing]),
	    ?shutdown2(Node, {check_dflags_failed, Missing})
    end.


%% No nodedown will be sent if we fail before this process has
%% succeeded to mark the node as pending.

mark_pending(#hs_data{kernel_pid=Kernel,
		      other_node=Node,
		      this_node=MyNode}=HSData) ->
    case do_mark_pending(Kernel, MyNode, Node,
			 (HSData#hs_data.f_address)(HSData#hs_data.socket,
						    Node),
			 HSData#hs_data.other_flags) of
	ok ->
	    send_status(HSData, ok),
	    reset_timer(HSData#hs_data.timer);

	ok_pending ->
	    send_status(HSData, ok_simultaneous),
	    reset_timer(HSData#hs_data.timer);

	nok_pending ->
	    send_status(HSData, nok),
	    ?shutdown(Node);
	    
	up_pending ->
	    %% Check if connection is still alive, no
	    %% implies that the connection is no longer pending
	    %% due to simultaneous connect
	    do_alive(HSData),

	    %% This can happen if the other node goes down,
	    %% and goes up again and contact us before we have
	    %% detected that the socket was closed. 
	    wait_pending(Kernel),
	    reset_timer(HSData#hs_data.timer);

	already_pending ->
	    %% FIXME: is this a case ?
	    ?debug({dist_util,self(),mark_pending,already_pending,Node}),
	    ?shutdown(Node)
    end.


%%
%% Marking pending and negotiating away 
%% simultaneous connection problems
%%

wait_pending(Kernel) ->
    receive
	{Kernel, pending} ->
	    ?trace("wait_pending returned for pid ~p.~n", 
		   [self()]),
	    ok
    end.

do_alive(#hs_data{other_node = Node} = HSData) ->
    send_status(HSData, alive),
    case recv_status(HSData) of
	true  -> true;
	false -> ?shutdown(Node)
    end.
    
do_mark_pending(Kernel, MyNode, Node, Address, Flags) ->
    Kernel ! {self(), {accept_pending,MyNode,Node,Address,
		       publish_type(Flags)}},
    receive
	{Kernel,{accept_pending,Ret}} ->
	    ?trace("do_mark_pending(~p,~p,~p,~p) -> ~p~n",
		   [Kernel,Node,Address,Flags,Ret]),
	    Ret
    end.

is_pending(Kernel, Node) ->
    Kernel ! {self(), {is_pending, Node}},
    receive
	{Kernel, {is_pending, Reply}} -> Reply
    end.
    
%%
%% This will tell the net_kernel about the nodedown as it
%% recognizes the exit signal.
%% The termination of this process does also imply that the Socket
%% is closed in a controlled way by inet_drv.
%%

-spec shutdown(atom(), non_neg_integer(), term()) -> no_return().

shutdown(Module, Line, Data) ->
    shutdown(Module, Line, Data, shutdown).

-spec shutdown(atom(), non_neg_integer(), term(), term()) -> no_return().

shutdown(_Module, _Line, _Data, Reason) ->
    ?shutdown_trace("Net Kernel 2: shutting down connection "
		    "~p:~p, data ~p,reason ~p~n",
		    [_Module,_Line, _Data, Reason]),
    flush_down(),
    exit(Reason).
%% Use this line to debug connection.  
%% Set net_kernel verbose = 1 as well.
%%    exit({Reason, ?MODULE, _Line, _Data, erlang:timestamp()}).


flush_down() ->
    receive
	{From, get_status} ->
	    From ! {self(), get_status, error},
	    flush_down()
    after 0 ->
	    ok
    end.

handshake_we_started(#hs_data{request_type=ReqType,
			      other_node=Node}=PreHSData) ->
    PreThisFlags = make_this_flags(ReqType, Node),
    HSData = PreHSData#hs_data{this_flags=PreThisFlags},
    send_name(HSData),
    recv_status(HSData),
    {PreOtherFlags,ChallengeA} = recv_challenge(HSData),
    {ThisFlags,OtherFlags} = adjust_flags(PreThisFlags, PreOtherFlags),
    NewHSData = HSData#hs_data{this_flags = ThisFlags,
			       other_flags = OtherFlags, 
			       other_started = false}, 
    check_dflags(NewHSData),
    MyChallenge = gen_challenge(),
    {MyCookie,HisCookie} = get_cookies(Node),
    send_challenge_reply(NewHSData,MyChallenge,
			 gen_digest(ChallengeA,HisCookie)),
    reset_timer(NewHSData#hs_data.timer),
    recv_challenge_ack(NewHSData, MyChallenge, MyCookie),
    connection(NewHSData);

handshake_we_started(OldHsData) when element(1,OldHsData) =:= hs_data ->
    handshake_we_started(convert_old_hsdata(OldHsData)).

convert_old_hsdata({hs_data, KP, ON, TN, S, T, TF, A, OV, OF, OS, FS, FR,
		    FS_PRE, FS_POST, FG, FA, MFT, MFG, RT}) ->
    #hs_data{
       kernel_pid = KP, other_node = ON, this_node = TN, socket = S, timer = T,
       this_flags = TF, allowed = A, other_version = OV, other_flags = OF,
       other_started = OS, f_send = FS, f_recv = FR, f_setopts_pre_nodeup = FS_PRE,
       f_setopts_post_nodeup = FS_POST, f_getll = FG, f_address = FA,
       mf_tick = MFT, mf_getstat = MFG, request_type = RT}.


%% --------------------------------------------------------------
%% The connection has been established.
%% --------------------------------------------------------------

connection(#hs_data{other_node = Node,
		    socket = Socket,
		    f_address = FAddress,
		    f_setopts_pre_nodeup = FPreNodeup,
		    f_setopts_post_nodeup = FPostNodeup}= HSData) ->
    cancel_timer(HSData#hs_data.timer),
    PType = publish_type(HSData#hs_data.other_flags), 
    case FPreNodeup(Socket) of
	ok -> 
	    do_setnode(HSData), % Succeeds or exits the process.
	    Address = FAddress(Socket,Node),
	    mark_nodeup(HSData,Address),
	    case FPostNodeup(Socket) of
		ok ->
		    con_loop({HSData#hs_data.kernel_pid,
			      Node,
			      Socket,
			      PType,
			      HSData#hs_data.mf_tick,
			      HSData#hs_data.mf_getstat,
			      HSData#hs_data.mf_setopts,
			      HSData#hs_data.mf_getopts},
			     #tick{});
		_ ->
		    ?shutdown2(Node, connection_setup_failed)
	    end;
	_ ->
	    ?shutdown(Node)
    end.

%% Generate a message digest from Challenge number and Cookie	
gen_digest(Challenge, Cookie) when is_integer(Challenge), is_atom(Cookie) ->
    erlang:md5([atom_to_list(Cookie)|integer_to_list(Challenge)]).

%% ---------------------------------------------------------------
%% Challenge code
%% gen_challenge() returns a "random" number
%% ---------------------------------------------------------------
gen_challenge() ->
    A = erlang:phash2([erlang:node()]),
    B = erlang:monotonic_time(),
    C = erlang:unique_integer(),
    {D,_}   = erlang:statistics(reductions),
    {E,_}   = erlang:statistics(runtime),
    {F,_}   = erlang:statistics(wall_clock),
    {G,H,_} = erlang:statistics(garbage_collection),
    %% A(8) B(16) C(16)
    %% D(16),E(8), F(16) G(8) H(16)
    ( ((A bsl 24) + (E bsl 16) + (G bsl 8) + F) bxor
      (B + (C bsl 16)) bxor 
      (D + (H bsl 16)) ) band 16#ffffffff.

%%
%% Get the cookies for a node from auth
%%    
get_cookies(Node) ->
    case auth:get_cookie(Node) of
	X when is_atom(X) ->
	    {X,X}
%	{Y,Z} when is_atom(Y), is_atom(Z) ->
%	    {Y,Z};
%	_ ->
%	    erlang:error("Corrupt cookie database")
    end.    

%% No error return; either succeeds or terminates the process.
do_setnode(#hs_data{other_node = Node, socket = Socket, 
		    other_flags = Flags, other_version = Version,
		    f_getll = GetLL}) ->
    case GetLL(Socket) of
	{ok,Port} ->
	    ?trace("setnode(md5,~p ~p ~p)~n", 
		   [Node, Port, {publish_type(Flags), 
				 '(', Flags, ')', 
				 Version}]),
	    case (catch 
		  erlang:setnode(Node, Port, 
				 {Flags, Version, '', ''})) of
		{'EXIT', {system_limit, _}} ->
		    error_msg("** Distribution system limit reached, "
			      "no table space left for node ~w ** ~n",
			      [Node]),
		    ?shutdown(Node);
		{'EXIT', Other} ->
		    exit(Other);
		_Else ->
		    ok
	    end;
	_ ->
	    error_msg("** Distribution connection error, "
		      "could not get low level port for node ~w ** ~n",
		      [Node]),
	    ?shutdown(Node)
    end.

mark_nodeup(#hs_data{kernel_pid = Kernel, 
		     other_node = Node, 
		     other_flags = Flags,
		     other_started = OtherStarted}, 
	    Address) ->
    Kernel ! {self(), {nodeup,Node,Address,publish_type(Flags),
		       true}},
    receive
	{Kernel, inserted} ->
	    ok;
	{Kernel, bad_request} ->
	    TypeT = case OtherStarted of
		       true ->
			   "accepting connection";
		       _ ->
			   "initiating connection"
		   end,
	    error_msg("Fatal: ~p was not allowed to "
		      "send {nodeup, ~p} to kernel when ~s~n",
		      [self(), Node, TypeT]),
	    ?shutdown(Node)
    end.

con_loop({Kernel, Node, Socket, Type, MFTick, MFGetstat, MFSetOpts, MFGetOpts}=ConData,
	 Tick) ->
    receive
	{tcp_closed, Socket} ->
	    ?shutdown2(Node, connection_closed);
	{Kernel, disconnect} ->
	    ?shutdown2(Node, disconnected);
	{Kernel, aux_tick} ->
	    case MFGetstat(Socket) of
		{ok, _, _, PendWrite} ->
		    send_tick(Socket, PendWrite, MFTick);
		_ ->
		    ignore_it
	    end,
	    con_loop(ConData, Tick);
	{Kernel, tick} ->
	    case send_tick(Socket, Tick, Type, 
			   MFTick, MFGetstat) of
		{ok, NewTick} ->
		    con_loop(ConData, NewTick);
		{error, not_responding} ->
 		    error_msg("** Node ~p not responding **~n"
 			      "** Removing (timedout) connection **~n",
 			      [Node]),
 		    ?shutdown2(Node, net_tick_timeout);
		_Other ->
		    ?shutdown2(Node, send_net_tick_failed)
	    end;
	{From, get_status} ->
	    case MFGetstat(Socket) of
		{ok, Read, Write, _} ->
		    From ! {self(), get_status, {ok, Read, Write}},
		    con_loop(ConData, Tick);
		_ ->
		    ?shutdown2(Node, get_status_failed)
	    end;
	{From, Ref, {setopts, Opts}} ->
	    Ret = case MFSetOpts of
		      undefined -> {error, enotsup};
		      _ -> MFSetOpts(Socket, Opts)
		  end,
	    From ! {Ref, Ret},
	    con_loop(ConData, Tick);
	{From, Ref, {getopts, Opts}} ->
	    Ret = case MFGetOpts of
		      undefined -> {error, enotsup};
		      _ -> MFGetOpts(Socket, Opts)
		  end,
	    From ! {Ref, Ret},
	    con_loop(ConData, Tick)
    end.


%% ------------------------------------------------------------
%% Misc. functions.
%% ------------------------------------------------------------

send_name(#hs_data{socket = Socket, this_node = Node, 
		   f_send = FSend, 
		   this_flags = Flags,
		   other_version = Version}) ->
    ?trace("send_name: node=~w, version=~w\n",
	   [Node,Version]),
    ?to_port(FSend, Socket, 
	     [$n, ?int16(Version), ?int32(Flags), atom_to_list(Node)]).

send_challenge(#hs_data{socket = Socket, this_node = Node, 
			other_version = Version, 
			this_flags = Flags,
			f_send = FSend},
	       Challenge ) ->
    ?trace("send: challenge=~w version=~w\n",
	   [Challenge,Version]),
    ?to_port(FSend, Socket, [$n,?int16(Version), ?int32(Flags),
			     ?int32(Challenge), 
			     atom_to_list(Node)]).

send_challenge_reply(#hs_data{socket = Socket, f_send = FSend}, 
		     Challenge, Digest) ->
    ?trace("send_reply: challenge=~w digest=~p\n",
	   [Challenge,Digest]),
    ?to_port(FSend, Socket, [$r,?int32(Challenge),Digest]).

send_challenge_ack(#hs_data{socket = Socket, f_send = FSend}, 
		   Digest) ->
    ?trace("send_ack: digest=~p\n", [Digest]),
    ?to_port(FSend, Socket, [$a,Digest]).


%%
%% Get the name of the other side.
%% Close the connection if invalid data.
%% The IP address sent is not interesting (as in the old
%% tcp_drv.c which used it to detect simultaneous connection
%% attempts).
%%
recv_name(#hs_data{socket = Socket, f_recv = Recv}) ->
    case Recv(Socket, 0, infinity) of
	{ok,Data} ->
	    get_name(Data);
	_ ->
	    ?shutdown(no_node)
    end.

get_name([$n,VersionA, VersionB, Flag1, Flag2, Flag3, Flag4 | OtherNode]) ->
    {?u32(Flag1, Flag2, Flag3, Flag4), list_to_atom(OtherNode), 
     ?u16(VersionA,VersionB)};
get_name(Data) ->
    ?shutdown(Data).

publish_type(Flags) ->
    case Flags band ?DFLAG_PUBLISHED of
	0 ->
	    hidden;
	_ ->
	    normal
    end.

%% wait for challenge after connect
recv_challenge(#hs_data{socket=Socket,other_node=Node,
			other_version=Version,f_recv=Recv}) ->
    case Recv(Socket, 0, infinity) of
	{ok,[$n,V1,V0,Fl1,Fl2,Fl3,Fl4,CA3,CA2,CA1,CA0 | Ns]} ->
	    Flags = ?u32(Fl1,Fl2,Fl3,Fl4),
	    try {list_to_existing_atom(Ns),?u16(V1,V0)} of
		{Node,Version} ->
		    Challenge = ?u32(CA3,CA2,CA1,CA0),
		    ?trace("recv: node=~w, challenge=~w version=~w\n",
			   [Node, Challenge,Version]),
		    {Flags,Challenge};
		_ ->
		    ?shutdown2(no_node, {recv_challenge_failed, no_node, Ns})
	    catch
		error:badarg ->
		    ?shutdown2(no_node, {recv_challenge_failed, no_node, Ns})
	    end;
	Other ->
	    ?shutdown2(no_node, {recv_challenge_failed, Other})
    end.


%%
%% wait for challenge response after send_challenge
%%
recv_challenge_reply(#hs_data{socket = Socket, 
			      other_node = NodeB,
			      f_recv = FRecv}, 
		     ChallengeA, Cookie) ->
    case FRecv(Socket, 0, infinity) of
	{ok,[$r,CB3,CB2,CB1,CB0 | SumB]} when length(SumB) =:= 16 ->
	    SumA = gen_digest(ChallengeA, Cookie),
	    ChallengeB = ?u32(CB3,CB2,CB1,CB0),
	    ?trace("recv_reply: challenge=~w digest=~p\n",
		   [ChallengeB,SumB]),
	    ?trace("sum = ~p\n", [SumA]),
	    case list_to_binary(SumB) of
		SumA ->
		    ChallengeB;
		_ ->
		    error_msg("** Connection attempt from "
			      "disallowed node ~w ** ~n", [NodeB]),
		    ?shutdown2(NodeB, {recv_challenge_reply_failed, bad_cookie})
	    end;
	Other ->
	    ?shutdown2(no_node, {recv_challenge_reply_failed, Other})
    end.

recv_challenge_ack(#hs_data{socket = Socket, f_recv = FRecv, 
			    other_node = NodeB}, 
		   ChallengeB, CookieA) ->
    case FRecv(Socket, 0, infinity) of
	{ok,[$a|SumB]} when length(SumB) =:= 16 ->
	    SumA = gen_digest(ChallengeB, CookieA),
	    ?trace("recv_ack: digest=~p\n", [SumB]),
	    ?trace("sum = ~p\n", [SumA]),
	    case list_to_binary(SumB) of
		SumA ->
		    ok;
		_ ->
		    error_msg("** Connection attempt to "
			      "disallowed node ~w ** ~n", [NodeB]),
		    ?shutdown2(NodeB, {recv_challenge_ack_failed, bad_cookie})
	    end;
	Other ->
	    ?shutdown2(NodeB, {recv_challenge_ack_failed, Other})
    end.

recv_status(#hs_data{kernel_pid = Kernel, socket = Socket, 
		     other_node = Node, f_recv = Recv} = HSData) ->
    case Recv(Socket, 0, infinity) of
	{ok, [$s|StrStat]} ->
	    Stat = list_to_atom(StrStat),
	    ?debug({dist_util,self(),recv_status, Node, Stat}),
	    case Stat of
		not_allowed -> ?shutdown2(Node, {recv_status_failed, not_allowed});
		nok  -> 
		    %% wait to be killed by net_kernel
		    receive
		    after infinity -> ok
		    end;
		alive -> 
		    Reply = is_pending(Kernel, Node),
		    ?debug({is_pending,self(),Reply}),
		    send_status(HSData, Reply),
		    if not Reply ->
			    ?shutdown(Node);
		       Reply ->
			    Stat
		    end;
		_ -> Stat
	    end;
	Error ->
	    ?debug({dist_util,self(),recv_status_error, 
		Node, Error}),
	    ?shutdown2(Node, {recv_status_failed, Error})
    end.


send_status(#hs_data{socket = Socket, other_node = Node,
		    f_send = FSend}, Stat) ->
    ?debug({dist_util,self(),send_status, Node, Stat}),
    case FSend(Socket, [$s | atom_to_list(Stat)]) of
	{error, _} ->
	    ?shutdown(Node);
	_ -> 
	    true
    end.
    
    

%%
%% Send a TICK to the other side.
%%
%% This will happen every 15 seconds (by default) 
%% The idea here is that every 15 secs, we write a little 
%% something on the connection if we haven't written anything for 
%% the last 15 secs.
%% This will ensure that nodes that are not responding due to 
%% hardware errors (Or being suspended by means of ^Z) will 
%% be considered to be down. If we do not want to have this  
%% we must start the net_kernel (in erlang) without its 
%% ticker process, In that case this code will never run 

%% And then every 60 seconds we also check the connection and 
%% close it if we havn't received anything on it for the 
%% last 60 secs. If ticked == tick we havn't received anything 
%% on the connection the last 60 secs. 

%% The detection time interval is thus, by default, 45s < DT < 75s 

%% A HIDDEN node is always (if not a pending write) ticked if 
%% we haven't read anything as a hidden node only ticks when it receives 
%% a TICK !! 
	
send_tick(Socket, Tick, Type, MFTick, MFGetstat) ->
    #tick{tick = T0,
	  read = Read,
	  write = Write,
	  ticked = Ticked} = Tick,
    T = T0 + 1,
    T1 = T rem 4,
    case MFGetstat(Socket) of
	{ok, Read, _, _} when  Ticked =:= T ->
	    {error, not_responding};
	{ok, Read, W, Pend} when Type =:= hidden ->
	    send_tick(Socket, Pend, MFTick),
	    {ok, Tick#tick{write = W + 1,
			   tick = T1}};
	{ok, Read, Write, Pend} ->
	    send_tick(Socket, Pend, MFTick),
	    {ok, Tick#tick{write = Write + 1,
			   tick = T1}};
	{ok, R, Write, Pend} ->
	    send_tick(Socket, Pend, MFTick),
	    {ok, Tick#tick{write = Write + 1,
			   read = R,
			   tick = T1,
			   ticked = T}};
	{ok, Read, W, _} ->
	    {ok, Tick#tick{write = W,
			   tick = T1}};
	{ok, R, W, _} ->
	    {ok, Tick#tick{write = W,
			   read = R,
			   tick = T1,
			   ticked = T}};
	Error ->
	    Error
    end.

send_tick(Socket, 0, MFTick) ->
    MFTick(Socket);
send_tick(_, _Pend, _) ->
    %% Dont send tick if pending write.
    ok.

%% ------------------------------------------------------------
%% Connection setup timeout timer.
%% After Timeout milliseconds this process terminates
%% which implies that the owning setup/accept process terminates.
%% The timer is reset before every network operation during the
%% connection setup !
%% ------------------------------------------------------------

start_timer(Timeout) ->
    spawn_link(?MODULE, setup_timer, [self(), Timeout*?trace_factor]).

setup_timer(Pid, Timeout) ->
    receive
	{Pid, reset} ->
	    setup_timer(Pid, Timeout)
    after Timeout ->
	    ?trace("Timer expires ~p, ~p~n",[Pid, Timeout]),
	    ?shutdown2(timer, setup_timer_timeout)
    end.

reset_timer(Timer) ->
    Timer ! {self(), reset},
    ok.

cancel_timer(Timer) ->
    unlink(Timer),
    exit(Timer, shutdown).

