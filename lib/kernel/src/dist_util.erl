%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1999-2025. All Rights Reserved.
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
-moduledoc false.

%%-compile(export_all).
-export([handshake_we_started/1, handshake_other_started/1,
         strict_order_flags/0, rejectable_flags/0,
	 start_timer/1, setup_timer/2, 
	 reset_timer/1, cancel_timer/1,
         is_node_name/1, split_node/1, is_allowed/2,
	 shutdown/3, shutdown/4,
         net_ticker_spawn_options/0]).

-import(error_logger,[error_msg/2]).

-include("dist_util.hrl").
-include("dist.hrl").

-ifdef(DEBUG).
-define(shutdown_trace(A,B), io:format(A,B)).
-else.
-define(shutdown_trace(A,B), noop).
-endif.


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

dflag2str(?DFLAG_PUBLISHED) ->
    "PUBLISHED";
dflag2str(?DFLAG_ATOM_CACHE) ->
    "ATOM_CACHE";
dflag2str(?DFLAG_EXTENDED_REFERENCES) ->
    "EXTENDED_REFERENCES";
dflag2str(?DFLAG_DIST_MONITOR) ->
    "DIST_MONITOR";
dflag2str(?DFLAG_FUN_TAGS) ->
    "FUN_TAGS";
dflag2str(?DFLAG_DIST_MONITOR_NAME) ->
    "DIST_MONITOR_NAME";
dflag2str(?DFLAG_HIDDEN_ATOM_CACHE) ->
    "HIDDEN_ATOM_CACHE";
dflag2str(?DFLAG_NEW_FUN_TAGS) ->
    "NEW_FUN_TAGS";
dflag2str(?DFLAG_EXTENDED_PIDS_PORTS) ->
    "EXTENDED_PIDS_PORTS";
dflag2str(?DFLAG_EXPORT_PTR_TAG) ->
    "EXPORT_PTR_TAG";
dflag2str(?DFLAG_BIT_BINARIES) ->
    "BIT_BINARIES";
dflag2str(?DFLAG_NEW_FLOATS) ->
    "NEW_FLOATS";
dflag2str(?DFLAG_UNICODE_IO) ->
    "UNICODE_IO";
dflag2str(?DFLAG_DIST_HDR_ATOM_CACHE) ->
    "DIST_HDR_ATOM_CACHE";
dflag2str(?DFLAG_SMALL_ATOM_TAGS) ->
    "SMALL_ATOM_TAGS";
dflag2str(?DFLAG_UTF8_ATOMS) ->
    "UTF8_ATOMS";
dflag2str(?DFLAG_MAP_TAG) ->
    "MAP_TAG";
dflag2str(?DFLAG_BIG_CREATION) ->
    "BIG_CREATION";
dflag2str(?DFLAG_SEND_SENDER) ->
    "SEND_SENDER";
dflag2str(?DFLAG_BIG_SEQTRACE_LABELS) ->
    "BIG_SEQTRACE_LABELS";
dflag2str(?DFLAG_EXIT_PAYLOAD) ->
    "EXIT_PAYLOAD";
dflag2str(?DFLAG_FRAGMENTS) ->
    "FRAGMENTS";
dflag2str(?DFLAG_HANDSHAKE_23) ->
    "HANDSHAKE_23";
dflag2str(?DFLAG_UNLINK_ID) ->
    "UNLINK_ID";
dflag2str(?DFLAG_MANDATORY_25_DIGEST) ->
    "MANDATORY_25_DIGEST";
dflag2str(?DFLAG_SPAWN) ->
    "SPAWN";
dflag2str(?DFLAG_NAME_ME) ->
    "NAME_ME";
dflag2str(?DFLAG_V4_NC) ->
    "V4_NC";
dflag2str(?DFLAG_ALIAS) ->
    "ALIAS";
dflag2str(?DFLAG_LOCAL_EXT) ->
    "LOCAL_EXT";
dflag2str(?DFLAG_ALTACT_SIG) ->
    "ALTACT_SIG";
dflag2str(Other) ->
    lists:flatten(io_lib:format("UNKNOWN<~.16.0B>", [Other])).


adjust_flags(ThisFlags, OtherFlags) ->
    ThisFlags band OtherFlags.

publish_flag(_, NameMeFlg, _) when (NameMeFlg band ?DFLAG_NAME_ME) =/= 0 ->
    ?DFLAG_NAME_ME;
publish_flag(hidden, _, _) ->
    0;
publish_flag(_, _, OtherNode) ->
    case net_kernel:publish_on_node(OtherNode) of
	true ->
	    ?DFLAG_PUBLISHED;
	_ ->
	    0
    end.

name_type(Flags) ->
    case (Flags band ?DFLAG_NAME_ME) of
        0 ->
            static;
        ?DFLAG_NAME_ME ->
            dynamic
    end.

%% Sync with dist.c
-record(erts_dflags, {
          default,      % flags erts prefers
          mandatory,    % flags erts needs
          addable,      % flags local dist implementation is allowed to add
          rejectable,   % flags local dist implementation is allowed to reject
          strict_order  % flags for features needing strict order delivery
}).

-spec strict_order_flags() -> integer().
strict_order_flags() ->
    EDF = erts_internal:get_dflags(),
    EDF#erts_dflags.strict_order.

-spec rejectable_flags() -> integer().
rejectable_flags() ->
    EDF = erts_internal:get_dflags(),
    EDF#erts_dflags.rejectable.

make_this_flags(RequestType, AddFlags, RejectFlags, OtherNode,
                #erts_dflags{}=EDF, NameMeFlg) ->
    case RejectFlags band (bnot EDF#erts_dflags.rejectable) of
        0 -> ok;
        Rerror -> exit({"Rejecting non rejectable flags", Rerror})
    end,
    case AddFlags band (bnot EDF#erts_dflags.addable) of
        0 -> ok;
        Aerror -> exit({"Adding non addable flags", Aerror})
    end,
    Flgs0 = EDF#erts_dflags.default,
    Flgs1 = Flgs0 bor publish_flag(RequestType, NameMeFlg, OtherNode),
    Flgs2 = Flgs1 bor AddFlags,
    Flgs2 band (bnot RejectFlags).

handshake_other_started(#hs_data{request_type=ReqType,
                                 add_flags=AddFlgs0,
                                 reject_flags=RejFlgs0,
                                 require_flags=ReqFlgs0}=HSData0) ->
    AddFlgs = convert_flags(AddFlgs0),
    RejFlgs = convert_flags(RejFlgs0),
    ReqFlgs = convert_flags(ReqFlgs0),
    {PreOtherFlags0,NodeOrHost,Creation} = recv_name(HSData0),
    PreOtherFlags = expand_mandatory_25_flag(PreOtherFlags0),
    EDF = erts_internal:get_dflags(),
    PreThisFlags = make_this_flags(ReqType, AddFlgs, RejFlgs, NodeOrHost, EDF,
                                   PreOtherFlags),
    HSData1 = HSData0#hs_data{this_flags=PreThisFlags,
                              other_flags=PreOtherFlags,
                              other_node=NodeOrHost,
                              other_started=true,
                              other_creation=Creation,
                              add_flags=AddFlgs,
                              reject_flags=RejFlgs,
                              require_flags=ReqFlgs},
    check_dflags(HSData1, EDF),
    ?debug({"MD5 connection from ~p~n", [NodeOrHost]}),
    {AcceptedPending, HSData2} = mark_pending(HSData1),
    Node = HSData2#hs_data.other_node,
    Cookie = auth:get_cookie(Node),
    ChallengeA = gen_challenge(),
    send_challenge(HSData2, ChallengeA),
    reset_timer(HSData2#hs_data.timer),
    HSData3 = recv_complement(HSData2),
    check_dflags(HSData3, EDF),
    ChosenFlags = adjust_flags(HSData3#hs_data.this_flags,
                               HSData3#hs_data.other_flags),
    HSData4 = HSData3#hs_data{this_flags = ChosenFlags,
                              other_flags = ChosenFlags},
    ChallengeB = recv_challenge_reply(HSData4, ChallengeA, Cookie),
    case AcceptedPending of
        up_pending -> wait_pending(HSData4);
        _ -> continue
    end,
    send_challenge_ack(HSData4, gen_digest(ChallengeB, Cookie)),
    ?debug({dist_util, self(), accept_connection, Node}),
    connection(HSData4);

handshake_other_started(OldHsData) when element(1,OldHsData) =:= hs_data ->
    handshake_other_started(convert_old_hsdata(OldHsData)).

expand_mandatory_25_flag(Flags) ->
    if
        Flags band ?DFLAG_MANDATORY_25_DIGEST =/= 0 ->
            %% From OTP 25, the single flag ?DFLAG_MANDATORY_25_DIGEST can
            %% replace all the flags in ?MANDATORY_DFLAGS_25.
            Flags bor ?MANDATORY_DFLAGS_25;
        true ->
            Flags
    end.

%%
%% Check mandatory flags...
%%
check_dflags(#hs_data{other_node = Node,
                      other_flags = OtherFlags,
                      other_started = OtherStarted,
                      require_flags = RequiredFlags,
                      other_creation = OtherCreation} = HSData,
             #erts_dflags{}=EDF) ->

    Mask = case OtherCreation of
               undefined ->
                   %% Old 'send_name' without creation and high flag bits
                   %% Only check low 32 flag bits for now
                   (1 bsl 32) - 1;
               _ ->
                   bnot 0
           end,
    Mandatory = Mask band (EDF#erts_dflags.mandatory bor RequiredFlags),
    Missing = check_mandatory(Mandatory, OtherFlags, []),
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

check_mandatory(0, _OtherFlags, Missing) ->
    Missing;
check_mandatory(Mandatory, OtherFlags, Missing) ->
    Left = Mandatory band (Mandatory - 1),   % clear lowest set bit
    DFlag = Mandatory bxor Left,             % only lowest set bit
    NewMissing = case DFlag band OtherFlags of
                     0 ->
                         %% Mandatory and missing...
                         [dflag2str(DFlag) | Missing];
                     _ ->
                         %% Mandatory and present...
                         Missing
                 end,
    check_mandatory(Left, OtherFlags, NewMissing).
                    

%% No nodedown will be sent if we fail before this process has
%% succeeded to mark the node as pending.

mark_pending(#hs_data{kernel_pid=Kernel,
		      other_node=Node,
		      this_node=MyNode}=HSData) ->
    KernelReply = do_mark_pending(Kernel, MyNode, Node,
                                  HSData#hs_data.other_flags),
    {KernelReply,
     case KernelReply of
	ok ->
	    send_status(HSData, ok),
	    reset_timer(HSData#hs_data.timer),
            HSData;

        {ok, DynNodeName, Creation} ->
            HSData1 = HSData#hs_data{other_node = DynNodeName,
                                     other_creation = Creation},
            send_status(HSData1, named),
	    reset_timer(HSData1#hs_data.timer),
            HSData1;

	ok_pending ->
	    send_status(HSData, ok_simultaneous),
	    reset_timer(HSData#hs_data.timer),
            HSData;

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
            %% It can also happen if the old connection went down silently,
            %% without us knowing, a lost TCP FIN or RST packet for example.

            %% Continue handshake to verify cookie and then wait for old
            %% connection to die.
	    reset_timer(HSData#hs_data.timer),
            HSData;

	already_pending ->
	    %% FIXME: is this a case ?
	    ?debug({dist_util,self(),mark_pending,already_pending,Node}),
	    ?shutdown(Node)
        end
    }.



%%
%% Tell net_kernel we are waiting for old connection to die.
%%
wait_pending(#hs_data{kernel_pid=Kernel,
		      other_node=Node}) ->
    Kernel ! {self(), {wait_pending, Node}},
    receive
	{Kernel, pending} ->
	    ?trace("wait_pending returned for pid ~p.~n", 
		   [self()]),
	    ok
    end.

do_alive(#hs_data{other_node = Node} = HSData) ->
    send_status(HSData, alive),
    case recv_status_reply(HSData) of
	true  -> true;
	false -> ?shutdown(Node)
    end.
    
do_mark_pending(Kernel, MyNode, Node, Flags) ->
    Kernel ! {self(), {accept_pending,MyNode,Node,
		       publish_type(Flags)}},
    receive
	{Kernel,{accept_pending,Ret}} ->
	    ?trace("do_mark_pending(~p,~p,~p,~p) -> ~p~n",
		   [Kernel, MyNode, Node, Flags, Ret]),
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
    exit(Reason).
%% Use this line to debug connection.  
%% Set net_kernel verbose = 1 as well.
%%    exit({Reason, {?MODULE, _Line, _Data, erlang:timestamp()}}).

handshake_we_started(#hs_data{request_type=ReqType,
                              this_node=MyNode,
			      other_node=Node,
                              add_flags=AddFlgs0,
                              reject_flags=RejFlgs0,
                              require_flags=ReqFlgs0}=PreHSData) ->
    AddFlgs = convert_flags(AddFlgs0),
    RejFlgs = convert_flags(RejFlgs0),
    ReqFlgs = convert_flags(ReqFlgs0),
    EDF = erts_internal:get_dflags(),
    {NameMeFlg, NameToSend} =
        case node() of
            nonode@nohost ->
                {node, "undefined", Host} = split_node(MyNode),
                {?DFLAG_NAME_ME, Host};

            _ ->
                {0, MyNode}
        end,
    PreThisFlags = make_this_flags(ReqType, AddFlgs, RejFlgs, Node, EDF, NameMeFlg),
    HSData = PreHSData#hs_data{this_node = NameToSend,
                               this_flags = PreThisFlags,
                               add_flags = AddFlgs,
                               reject_flags = RejFlgs,
                               require_flags = ReqFlgs},
    send_name(HSData),
    HSData1 = recv_status(HSData),
    {PreOtherFlags0, ChallengeA, Creation} = recv_challenge(HSData1),
    PreOtherFlags = expand_mandatory_25_flag(PreOtherFlags0),
    ChosenFlags = adjust_flags(PreThisFlags, PreOtherFlags),
    HSData2 = HSData1#hs_data{this_flags = ChosenFlags,
			       other_flags = ChosenFlags,
			       other_started = false,
                               other_creation = Creation},
    check_dflags(HSData2, EDF),
    ChallengeB = gen_challenge(),
    Cookie = auth:get_cookie(Node),
    send_challenge_reply(HSData2, ChallengeB,
                         gen_digest(ChallengeA, Cookie)),
    reset_timer(HSData2#hs_data.timer),
    recv_challenge_ack(HSData2, ChallengeB, Cookie),
    connection(HSData2);

handshake_we_started(OldHsData) when element(1,OldHsData) =:= hs_data ->
    handshake_we_started(convert_old_hsdata(OldHsData)).

convert_old_hsdata(OldHsData) ->
    OHSDL = tuple_to_list(OldHsData),
    NoMissing = tuple_size(#hs_data{}) - tuple_size(OldHsData),
    true = NoMissing > 0,
    list_to_tuple(OHSDL ++ lists:duplicate(NoMissing, undefined)).

convert_flags(Flags) when is_integer(Flags) ->
    Flags;
convert_flags(_Undefined) ->
    0.

%% --------------------------------------------------------------
%% The connection has been established.
%% --------------------------------------------------------------

-record(state, {kernel          :: pid(),
                node            :: node(),
                tick_intensity  :: 4..1000,
                socket          :: term(),
                publish_type    :: 'hidden' | 'normal',
                handle          :: erlang:dist_handle(),
                f_tick          :: function(),
                f_getstat       :: function() | 'undefined',
                f_setopts       :: function() | 'undefined',
                f_getopts       :: function() | 'undefined'}).

connection(#hs_data{other_node = Node,
		    socket = Socket,
		    f_address = FAddress,
		    f_setopts_pre_nodeup = FPreNodeup,
		    f_setopts_post_nodeup = FPostNodeup}= HSData) ->
    cancel_timer(HSData#hs_data.timer),
    PType = publish_type(HSData#hs_data.other_flags), 
    case FPreNodeup(Socket) of
	ok -> 
	    {DHandle,NamedMe} = do_setnode(HSData), % Succeeds or exits the process.
	    Address = FAddress(Socket,Node),
	    TickIntensity = mark_nodeup(HSData,Address,NamedMe),
	    case FPostNodeup(Socket) of
		ok ->
                    case HSData#hs_data.f_handshake_complete of
                        undefined -> ok;
                        HsComplete -> HsComplete(Socket, Node, DHandle)
                    end,
		    con_loop(#state{kernel = HSData#hs_data.kernel_pid,
                                    node = Node,
                                    socket = Socket,
                                    tick_intensity = TickIntensity,
                                    publish_type = PType,
                                    handle = DHandle,
                                    f_tick = HSData#hs_data.mf_tick,
                                    f_getstat = HSData#hs_data.mf_getstat,
                                    f_setopts = HSData#hs_data.mf_setopts,
                                    f_getopts = HSData#hs_data.mf_getopts},
			     #tick{});
		Error1 ->
		    ?shutdown2(
                       {Node, Socket},
                       {f_setopts_post_nodeup_failed, Error1})
	    end;
	Error2 ->
	    ?shutdown2({Node, Socket}, {f_setopts_pre_nodeup_failed, Error2})
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

%% No error return; either succeeds or terminates the process.
do_setnode(#hs_data{other_node = Node, socket = Socket, 
                    this_node = MyNode,
		    other_flags = Flags,
		    f_getll = GetLL,
                    other_creation = Creation}=HSData) ->
    case GetLL(Socket) of
	{ok,Port} ->
            NamedMe = case node() of
                          nonode@nohost ->
                              dynamic = name_type(HSData#hs_data.this_flags),
                              erlang:setnode(MyNode, HSData#hs_data.this_creation),
                              true;
                          MyNode ->
                              false
                      end,
	    ?trace("setnode: node=~p port=~p flags=~p(~p) creation=~p~n",
		   [Node, Port, Flags, publish_type(Flags), Creation]),

            MyNode = node(), % ASSERT
            try
                DHandle = erlang:setnode(Node, Port, {Flags, Creation}),
                {DHandle, NamedMe}
            catch
                error:system_limit ->
		    error_msg("** Distribution system limit reached, "
			      "no table space left for node ~w ** ~n",
			      [Node]),
		    ?shutdown({Node, Socket});
                error:Other:Stacktrace ->
                    exit({Other, Stacktrace})
	    end;
	_ ->
	    error_msg("** Distribution connection error, "
		      "could not get low level port for node ~w ** ~n",
		      [Node]),
	    ?shutdown({Node, Socket})
    end.

mark_nodeup(#hs_data{kernel_pid = Kernel, 
		     other_node = Node, 
		     other_flags = Flags,
		     other_started = OtherStarted}, 
	    Address, NamedMe) ->
    Kernel ! {self(), {nodeup,Node,Address,publish_type(Flags),NamedMe}},
    receive
	{Kernel, inserted, TickIntensity} ->
	    TickIntensity;
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

getstat(DHandle, _Socket, undefined) ->
    erlang:dist_get_stat(DHandle);
getstat(_DHandle, Socket, MFGetstat) ->
    MFGetstat(Socket).

con_loop(#state{kernel = Kernel, node = Node,
                socket = Socket, handle = DHandle,
                f_getstat = MFGetstat, f_setopts = MFSetOpts,
                f_getopts = MFGetOpts} = ConData,
	 Tick) ->
    receive
	{tcp_closed, Socket} ->
	    ?shutdown2({Node, Socket}, tcp_closed);
	{Kernel, disconnect} ->
	    ?shutdown2({Node, Socket}, disconnected);
	{Kernel, aux_tick} ->
	    case getstat(DHandle, Socket, MFGetstat) of
		{ok, _, _, PendWrite} ->
		    send_aux_tick(ConData, PendWrite);
		_ ->
		    ignore_it
	    end,
	    con_loop(ConData, Tick);
	{Kernel, tick} ->
	    case send_tick(ConData, Tick) of
		{ok, NewTick} ->
		    con_loop(ConData, NewTick);
		{error, not_responding} ->
 		    error_msg("** Node ~p not responding **~n"
 			      "** Removing (timedout) connection **~n",
 			      [Node]),
		    ?shutdown2({Node, Socket}, net_tick_timeout);
		Error1 ->
		    ?shutdown2({Node, Socket}, {send_net_tick_failed, Error1})
	    end;
	{From, get_status} ->
	    case getstat(DHandle, Socket, MFGetstat) of
		{ok, Read, Write, _} ->
		    From ! {self(), get_status, {ok, Read, Write}},
		    con_loop(ConData, Tick);
		Error2 ->
		    ?shutdown2({Node, Socket}, {get_status_failed, Error2})
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
		   this_flags = Flags}) ->
    NameBin = to_binary(Node),
    Creation = case name_type(Flags) of
                   static -> erts_internal:get_creation();
                   dynamic -> 0
               end,
    NameLen = byte_size(NameBin),
    ?trace("send_name: 'N' node=~p creation=~w\n",
           [Node, Creation]),
    to_port(FSend, Socket,
            [<<$N, Flags:64, Creation:32, NameLen:16>>, NameBin]).

to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, latin1);
to_binary(List) when is_list(List) ->
    list_to_binary(List).

send_challenge(#hs_data{socket = Socket, this_node = Node, 
			this_flags = ThisFlags,
			f_send = FSend},
	       Challenge ) ->
    %% Reply with new 'N' message
    Creation = erts_internal:get_creation(),
    NodeName = atom_to_binary(Node, latin1),
    NameLen = byte_size(NodeName),
    ?trace("send: 'N' challenge=~w creation=~w\n",
           [Challenge,Creation]),
    to_port(FSend, Socket,
            [<<$N,ThisFlags:64, Challenge:32, Creation:32, NameLen:16>>,
             NodeName]).

send_challenge_reply(#hs_data{socket = Socket, f_send = FSend}, 
		     Challenge, Digest) ->
    ?trace("send_reply: challenge=~w digest=~p\n",
	   [Challenge,Digest]),
    to_port(FSend, Socket, [$r,?int32(Challenge),Digest]).

send_challenge_ack(#hs_data{socket = Socket, f_send = FSend}, 
		   Digest) ->
    ?trace("send_ack: digest=~p\n", [Digest]),
    to_port(FSend, Socket, [$a,Digest]).


%%
%% Receive first handshake message sent from connecting side.
%% Close the connection if invalid data.
%%
recv_name(#hs_data{socket = Socket, f_recv = Recv} = HSData) ->
    case Recv(Socket, 0, infinity) of
        {ok, [$n | _] = Data} ->
            recv_name_old(HSData, Data);
        {ok, [$N | _] = Data} ->
            recv_name_new(HSData, Data);
	Other ->
	    ?shutdown2({no_node, Socket}, {recv_name_failed, Other})
    end.

%% OTP 25.3:
%% We accept old 'send_name' messages ($n) used by OTP-22 and older.
%% OTP-23 or OTP-24 nodes, not using epmd, may send the old message
%% to us if they do not know our OTP version.
%% Hence, we accept the old 'send_name' and the accompanying
%% 'send_complement', but we do not send them ourself.
%% This was removed prematurely in OTP-25.0,
%% therefore versions 25.0 to 25.2.* are broken in this regard.
%% Can be safely removed in OTP-27.0.
recv_name_old(HSData,
             [$n, V1, V0, F3, F2, F1, F0 | Node] = Data) ->
    <<_Version:16>> = <<V1,V0>>,
    <<Flags:32>> = <<F3,F2,F1,F0>>,
    ?trace("recv_name: 'n' node=~p version=~w\n", [Node, _Version]),
    case is_node_name(Node) of
        true ->
            check_allowed(HSData, Node),
            {Flags, list_to_atom(Node), undefined};
        false ->
            ?shutdown(Data)
    end.

recv_name_new(HSData,
             [$N, F7,F6,F5,F4,F3,F2,F1,F0, Cr3,Cr2,Cr1,Cr0,
              NL1, NL0 | Rest] = Data) ->
    <<Flags:64>> = <<F7,F6,F5,F4,F3,F2,F1,F0>>,
    <<Creation:32>> = <<Cr3,Cr2,Cr1,Cr0>>,
    <<NameLen:16>> = <<NL1,NL0>>,
    {Name, _Residue} = lists:split(NameLen, Rest),
    ?trace("recv_name: 'N' name=~p creation=~w\n", [Name, Creation]),
    case is_name_ok(Name, Flags) of
        true ->
            check_allowed(HSData, Name),
            NodeOrHost = case name_type(Flags) of
                             static ->
                                 list_to_atom(Name);
                             dynamic ->
                                 %% Keep host name as string
                                 Name
                         end,
            {Flags, NodeOrHost, Creation};
        false ->
            ?shutdown({name, Data})
    end.

is_node_name(NodeName) ->
    is_name_ok(NodeName, 0).

is_name_ok(NodeOrHost, Flags) ->
    case {string:split(NodeOrHost, "@", all), name_type(Flags)} of
        {[Name,Host], static} ->
            (not string:is_empty(Host))
                andalso (not string:is_empty(Name));

        {[Host], dynamic} ->
            not string:is_empty(Host);

        _ ->
            false
    end.

split_node(Node) ->
    Split = string:split(listify(Node), "@", all),
    case Split of
        [Name,Host] ->
            case string:is_empty(Name) of
                true ->
                    Split;
                false ->
                    case string:is_empty(Host) of
                        true ->
                            {name,Name};
                        false ->
                            {node,Name,Host}
                    end
            end;
        [Host] ->
            case string:is_empty(Host) of
                true ->
                    Split;
                false ->
                    {host,Host}
            end;
        _ ->
            {error, {invalid_node, Node}}
    end.

%% Check if connecting node is allowed to connect
%% with allow-node-scheme.  An empty allowed list
%% allows all nodes.
%%
check_allowed(#hs_data{allowed = []}, _Node) ->
    ok;
check_allowed(#hs_data{allowed = Allowed} = HSData, Node) ->
    case is_allowed(Node, Allowed) of
        true ->
            ok;
        false ->
	    send_status(HSData#hs_data{other_node = Node}, not_allowed),
	    error_msg("** Connection attempt from "
		      "disallowed node ~s ** ~n", [Node]),
	    ?shutdown2(Node, {is_allowed, not_allowed})
    end.

%% The allowed list can contain node names, host names
%% or names before '@', in atom or list form:
%% [node@host.example.org, "host.example.org", "node@"].
%% An empty allowed list allows no nodes.
%%
%% Allow a node that matches any entry in the allowed list.
%% Also allow allowed entries as node to match, not from
%% this module; here the node has to be a valid name.
%%
is_allowed(_Node, []) ->
    false;
is_allowed(Node, [Node|_Allowed]) ->
    %% Just an optimization
    true;
is_allowed(Node, [AllowedNode|Allowed]) ->
    case split_node(AllowedNode) of
        {node,AllowedName,AllowedHost} ->
            %% Allowed node name
            case split_node(Node) of
                {node,AllowedName,AllowedHost} ->
                    true;
                _ ->
                    is_allowed(Node, Allowed)
            end;
        {host,AllowedHost} ->
            %% Allowed host name
            case split_node(Node) of
                {node,_,AllowedHost} ->
                    %% Matching Host part
                    true;
                {host,AllowedHost} ->
                    %% Host matches Host
                    true;
                _ ->
                    is_allowed(Node, Allowed)
            end;
        {name,AllowedName} ->
            %% Allowed name before '@'
            case split_node(Node) of
                {node,AllowedName,_} ->
                    %% Matching Name part
                    true;
                {name,AllowedName} ->
                    %% Name matches Name
                    true;
                _ ->
                    is_allowed(Node, Allowed)
            end;
        _ ->
            is_allowed(Node, Allowed)
    end.

listify(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
listify(Node) when is_list(Node) ->
    Node;
listify(Bin) when is_binary(Bin) ->
    binary_to_list(Bin).


publish_type(Flags) ->
    case Flags band ?DFLAG_PUBLISHED of
	0 ->
	    hidden;
	_ ->
	    normal
    end.

%% wait for challenge after connect
recv_challenge(#hs_data{socket=Socket, f_recv=Recv}=HSData) ->
    case Recv(Socket, 0, infinity) of
        {ok,[$N | _]=Msg} ->
            recv_challenge_new(HSData, Msg);
	Other ->
            ?shutdown2({no_node, Socket}, {recv_challenge_failed, Other})
    end.

recv_challenge_new(#hs_data{other_node=Node},
                   [$N,
                    F7,F6,F5,F4,F3,F2,F1,F0,
                    Ch3,Ch2,Ch1,Ch0,
                    Cr3,Cr2,Cr1,Cr0,
                    NL1,NL0 | Rest] = Msg) ->
    <<Flags:64>> = <<F7,F6,F5,F4,F3,F2,F1,F0>>,
    <<Challenge:32>> = <<Ch3,Ch2,Ch1,Ch0>>,
    <<Creation:32>> = <<Cr3,Cr2,Cr1,Cr0>>,
    <<NameLen:16>> = <<NL1,NL0>>,
    {Ns, _Residue} =
        try
            lists:split(NameLen, Rest)
        catch
            error:badarg ->
                ?shutdown2(no_node, {recv_challenge_failed, no_node, Msg})
        end,
    ?trace("recv: 'N' node=~p, challenge=~w creation=~w\n",
           [Ns, Challenge, Creation]),

    try list_to_existing_atom(Ns) of
        Node ->
            {Flags, Challenge, Creation};
        _ ->
            ?shutdown2(no_node, {recv_challenge_failed, no_node, Ns})
    catch
        error:badarg ->
            ?shutdown2(no_node, {recv_challenge_failed, no_node, Ns})
    end;
recv_challenge_new(_, Other) ->
    ?shutdown2(no_node, {recv_challenge_failed, Other}).


%% See comment for recv_name_old
recv_complement(#hs_data{socket = Socket,
                         f_recv = Recv,
                         other_flags = Flags,
                         other_creation = undefined} = HSData) ->
    case Recv(Socket, 0, infinity) of
        {ok, [$c, F7,F6,F5,F4, Cr3,Cr2,Cr1,Cr0]} ->
            <<FlagsHigh:32>> = <<F7,F6,F5,F4>>,
            <<Creation:32>> = <<Cr3,Cr2,Cr1,Cr0>>,
            ?trace("recv_complement: creation=~w\n", [Creation]),
            HSData#hs_data{other_creation = Creation,
                           other_flags = Flags bor (FlagsHigh bsl 32)};
        Other ->
            ?shutdown2(no_node, {recv_complement_failed, Other})
    end;
recv_complement(HSData) ->
    HSData.


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
		    error_msg("** Connection attempt from node ~w rejected."
                              " Invalid challenge reply. **~n", [NodeB]),
		    ?shutdown2(NodeB, {recv_challenge_reply_failed, bad_cookie})
	    end;
	Other ->
	    ?shutdown2({no_node, Socket},
                       {recv_challenge_reply_failed, Other})
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
		    error_msg("** Connection attempt to node ~w cancelled."
                              " Invalid challenge ack. **~n", [NodeB]),
		    ?shutdown2(
                       {NodeB, Socket}, {recv_challenge_ack_failed, bad_cookie})
	    end;
	Other ->
	    ?shutdown2(
               {NodeB, Socket}, {recv_challenge_ack_failed, Other})
    end.

recv_status(
  #hs_data{
     kernel_pid = Kernel,
     socket = Socket,
     this_flags = MyFlgs,
     other_node = Node,
     f_recv = Recv} = HSData) ->
    case Recv(Socket, 0, infinity) of
        {ok, "snamed:"++Rest} ->
            <<NameLen:16,
              NodeName:NameLen/binary,
              Creation:32,
              _/binary>> = list_to_binary(Rest),
            dynamic = name_type(MyFlgs),
            {node, _Name, Host} = split_node(NodeName),
            Host = HSData#hs_data.this_node,
            HSData#hs_data{this_node = binary_to_atom(NodeName, utf8),
                           this_creation = Creation};

	{ok, [$s|Stat]} ->
	    ?debug({dist_util,self(),recv_status, Node, Stat}),
	    case {Stat, name_type(MyFlgs)} of
                {"not_allowed", _} ->
                    ?shutdown2(
                       {Node, Socket}, {recv_status_failed, not_allowed});
                {_, dynamic} ->
                    ?shutdown2(
                       {Node, Socket}, {recv_status_failed, unexpected, Stat});
                _ ->
                    continue
            end,
            case Stat of
                "nok"  ->
                    %% wait to be killed by net_kernel
                    receive
                    after infinity -> ok
                    end;
                "alive" ->
                    Reply = is_pending(Kernel, Node),
                    ?debug({is_pending,self(),Reply}),
                    send_status(HSData, Reply),
                    if not Reply ->
                            ?shutdown({Node, Socket});
                       Reply ->
                            HSData
                    end;
                "ok" -> HSData;
                "ok_simultaneous" -> HSData;
                Other ->
                    ?shutdown2(
                       {Node, Socket}, {recv_status_failed, unknown, Other})
            end;

	Error ->
	    ?debug({dist_util, self(), recv_status_error, Node, Error}),
	    ?shutdown2({Node, Socket}, {recv_status_failed, Error})
    end.


recv_status_reply(#hs_data{socket = Socket,
                           other_node = Node,
                           f_recv = Recv}) ->
    case Recv(Socket, 0, infinity) of
	{ok, [$s|Stat]} ->
	    ?debug({dist_util,self(),recv_status, Node, Stat}),
            case Stat of
                "true" -> true;
                "false" -> false;
                Other ->
                    ?shutdown2(
                       {Node, Socket}, {recv_status_failed, unexpected, Other})
            end;

	Error ->
	    ?debug({dist_util,self(),recv_status_error,
		Node, Error}),
	    ?shutdown2({Node, Socket}, {recv_status_failed, Error})
    end.


send_status(#hs_data{socket = Socket,
                     other_node = Node,
                     other_creation = Creation,
		    f_send = FSend},
            named) ->
    ?debug({dist_util, self(), send_status, Node}),
    NameBin = atom_to_binary(Node, utf8),
    NameLen = byte_size(NameBin),
    case FSend(Socket, [$s, "named:",
                       <<NameLen:16, NameBin/binary>>,
                       <<Creation:32>>]) of
	{error, _} = Error->
	    ?shutdown2({Node, Socket}, {send_status_failed, Error});
	_ ->
	    ok
    end;
send_status(#hs_data{socket = Socket, other_node = Node,
		    f_send = FSend}, Stat) ->
    ?debug({dist_util,self(),send_status, Node, Stat}),
    case FSend(Socket, [$s | atom_to_list(Stat)]) of
	{error, _} = Error ->
	    ?shutdown2({Node, Socket}, {send_status_failed, Error});
	_ ->
	    ok
    end.

to_port(FSend, Socket, Data) ->
    case FSend(Socket, Data) of
        {error, closed} ->
            self() ! {tcp_closed, Socket},
            ok;
        {error, _} = Error ->
            ?shutdown2(Socket, {f_send_failed, Error});
        ok ->
            ok
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
%% close it if we haven't received anything on it for the 
%% last 60 secs. If ticked == tick we haven't received anything 
%% on the connection the last 60 secs. 

%% The detection time interval is thus, by default, 45s < DT < 75s 

%% A HIDDEN node is always ticked if we haven't read anything
%% as a (primitive) hidden node only ticks when it receives a TICK !!
	
send_tick(#state{handle = DHandle, socket = Socket,
                 tick_intensity = TickIntensity,
                 publish_type = Type, f_tick = MFTick,
                 f_getstat = MFGetstat}, Tick) ->
    #tick{tick = T0,
	  read = Read,
	  write = Write,
	  ticked = Ticked0} = Tick,
    T = T0 + 1,
    T1 = T rem TickIntensity,
    case getstat(DHandle, Socket, MFGetstat) of
	{ok, Read, _, _} when Ticked0 =:= T ->
	    {error, not_responding};

        {ok, R, W1, Pend} ->
            RDiff = R - Read,
            W2 = case need_to_tick(Type, RDiff, W1-Write, Pend) of
                     true ->
                         MFTick(Socket),
                         W1 + 1;
                     false ->
                         W1
                 end,

            Ticked1 = case RDiff of
                          0 -> Ticked0;
                          _ -> T
                      end,

            {ok, Tick#tick{write = W2,
                           tick = T1,
                           read = R,
                           ticked = Ticked1}};

	Error ->
	    Error
    end.

need_to_tick(_, _, 0, 0) ->       % nothing written and empty send queue
    true;
need_to_tick(_, _, 0, false) ->   % nothing written and empty send queue
    true;
need_to_tick(hidden, 0, _, _) ->  % nothing read from hidden
    true;
need_to_tick(_, _, _, _) ->
    false.

send_aux_tick(#state{publish_type = normal}, Pend) when Pend /= false,
                                                        Pend /= 0 ->
    ok; %% Dont send tick if pending write.
send_aux_tick(#state{socket = Socket, f_tick = MFTick}, _Pend) ->
    MFTick(Socket).

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
	    ?shutdown2({timer, Pid}, setup_timer_timeout)
    end.

reset_timer(Timer) ->
    Timer ! {self(), reset},
    ok.

cancel_timer(Timer) ->
    unlink(Timer),
    exit(Timer, cancel_setup_timer).

net_ticker_spawn_options() ->
    Opts = application:get_env(kernel, net_ticker_spawn_options, []),
    [link, {priority, max} | cleanup_net_ticker_spawn_options(Opts)].

cleanup_net_ticker_spawn_options([]) ->
    [];
cleanup_net_ticker_spawn_options([link|Opts]) ->
    cleanup_net_ticker_spawn_options(Opts);
cleanup_net_ticker_spawn_options([{priority, _}|Opts]) ->
    cleanup_net_ticker_spawn_options(Opts);
cleanup_net_ticker_spawn_options([monitor|Opts]) ->
    cleanup_net_ticker_spawn_options(Opts);
cleanup_net_ticker_spawn_options([{monitor, _}|Opts]) ->
    cleanup_net_ticker_spawn_options(Opts);
cleanup_net_ticker_spawn_options([Opt|Opts]) ->
    [Opt|cleanup_net_ticker_spawn_options(Opts)].

