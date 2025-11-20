%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025-2025. All Rights Reserved.
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

-module(socket_sctp_traffic_client).

-include("socket_sctp_traffic_lib.hrl").

-export([
         start/2, start/3, start/4,
         stop/1,
         start_run/2
        ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(MSG(Pid, Info), {?MODULE, (Pid), (Info)}).

-define(TIMEOUT,        5000).
-define(STATUS_TIMEOUT, 5000).

-define(DEFAULT_OPTS,         #{debug => false}).
-define(DEFAULT_TRAFFIC_DATA, [<<"FOOBAR">>]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(ID, ServerSA) ->
    start(ID, ServerSA, ?DEFAULT_TRAFFIC_DATA, ?DEFAULT_OPTS).

start(ID, ServerSA, TrafficData) when is_list(TrafficData) ->
    start(ID, ServerSA, TrafficData, ?DEFAULT_OPTS);
start(ID, ServerSA, Opts) when is_map(Opts) ->
    start(ID, ServerSA, ?DEFAULT_TRAFFIC_DATA, Opts).

start(ID, ServerSA, TrafficData, Opts0)
  when is_integer(ID) andalso (ID > 0) andalso
       is_map(ServerSA) andalso
       is_list(TrafficData) andalso (length(TrafficData) > 0) andalso
       is_map(Opts0) ->
    Opts   = (maps:merge(?DEFAULT_OPTS, Opts0))#{parent => self()},
    Client = {Pid, MRef} =
        spawn_monitor(fun() ->
                              init(ID, ServerSA, TrafficData, Opts)
                      end),
    receive
        {'DOWN', MRef, process, Pid, Reason} ->
            {error, Reason};

        ?MSG(Pid, {started, PortNo, AssocID}) ->
            {ok, {Client, {PortNo, AssocID}}}

    after ?TIMEOUT ->
            ?ERROR("Client start timeout"),
            exit(Pid, kill),
            {error, timeout}
    end.

stop(Pid) when is_pid(Pid) ->
    Pid ! ?MSG(self(), stop),
    receive
        {'DOWN', _MRef, process, Pid, {result, C, _} = RESULT} ->
            if
                (C =:= 0) ->
                    ok;
                true ->
                    RESULT
            end;
        {'DOWN', _MRef, process, Pid, _} ->
            ok
    after ?TIMEOUT ->
            ?ERROR("Client stop timeout"),
            exit(Pid, kill),
            ok
    end.


start_run(Pid, RunTime)
  when is_pid(Pid) andalso is_integer(RunTime) andalso (RunTime > 0) ->
    Pid ! ?MSG(self(), {start_run, RunTime}),
    ok.

        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(ID,
     #{family := Domain} = ServerSA,
     TrafficData,
     #{parent := Parent} = Opts) ->

    set_debug(Opts),
    ?SET_SNAME(?F("client[~w]", [ID])),

    ?DBG("try find local (~w) addr", [Domain]),
    Addr =
        case ?WHICH_LOCAL_ADDR(Domain) of
            {ok, A} ->
                A;
            {error, AReason} ->
                ?ERROR("Failed find local addr:"
                       "~n   ~p", [AReason]),
                exit({which_local_addr, AReason})
        end,
    
    ?DBG("try open socket"),
    Sock =
        case socket:open(Domain, seqpacket, sctp) of
            {ok, S} ->
                S;
            {error, OReason} ->
                ?ERROR("Failed open socket"
                       "~n   ~p", [OReason]),
                exit({open, OReason})
        end,

    ?DBG("try bind"),
    SockAddr = #{family => Domain,
                 addr   => Addr,
                 port   => 0},
    case socket:bind(Sock, SockAddr) of
        ok ->
            ok;
        {error, BReason} ->
            ?ERROR("Failed bind: "
                   "~n   SockAddr: ~p"
                   "~n   Reason:   ~p", [BReason]),
	    (catch socket:close(Sock)),
            exit({bind, BReason})
    end,

    ?DBG("try setopt events"),
    case socket:setopt(Sock, ?MK_SCTP_SOCKOPT(events), ?SCTP_EVENTS(true)) of
        ok ->
            ok;
        {error, SReason} ->
            ?ERROR("Failed setopt 'events': "
                   "~n   ~p", [SReason]),
	    (catch socket:close(Sock)),
            exit({setopt, events, SReason})
    end,

    ?DBG("try sockname (get port number)"),
    Port =
        case socket:sockname(Sock) of
            {ok, #{port := P}} ->
                P;
            {error, PReason} ->
                ?ERROR("Failed sockname: "
                       "~n   ~p", [PReason]),
                (catch socket:close(Sock)),
                exit({sockname, PReason})
        end,

    ?DBG("try connect to server:"
         "~n   Server SockAddr: ~p", [ServerSA]),
    case socket:connect(Sock, ServerSA) of
	ok ->
            ?DBG("connect ok"),
	    ok;
        {error, CReason} ->
	    ?ERROR("Failed connect: "
                   "~n   Server SockAddr: ~p"
                   "~n   Reason:          ~p", [CReason]),
	    (catch socket:close(Sock)),
            exit({connect, CReason})
    end,
    
    ?DBG("await connect confirmation"),
    AssocID =
	case socket:recvmsg(Sock) of
	    {ok, #{flags        := Flags,
                   addr         := SA,
                   notification := #{type     := assoc_change,
				     state    := comm_up,
				     assoc_id := AID}}} ->
		?DBG("received expected connect confirmation:"
                     "~n   Flags:   ~p"
                     "~n   SA:      ~p"
                     "~n   AssocID: ~p",
                     [Flags, SA, AID]),
		AID;
	    {ok, #{flags        := Flags,
                   addr         := SA,
                   notification := Notif}} ->
		?ERROR("Received unexpected notification: "
                       "~n   Flags: ~p"
                       "~n   SA:    ~p"
                       "~n   Notif: ~p",
                       [Flags, SA, Notif]),
                (catch socket:close(Sock)),
                exit({unexpected_notif, Flags, SA, Notif});
            {ok, Msg} ->
 		?ERROR("Received unexpected msg: "
                       "~n   ~p", [Msg]),
                (catch socket:close(Sock)),
		exit({unexpected_msg, Msg});
	    {error, RReason} ->
		?ERROR("Unexpected recvmsg failure: "
                       "~n   ~p", [RReason]),
                (catch socket:close(Sock)),
		exit({unexpected_recvmsg_failure, RReason})
	end,

    ?DBG("monitor parent"),
    MRef = erlang:monitor(process, Parent),

    ?DBG("started - inform parent (port number ~p, assoc id ~w)",
         [Port, AssocID]),
    Parent ! ?MSG(self(), {started, Port, AssocID}),

    ?DBG("init done"),
    loop(Opts#{parent_mref => MRef,
               msgs_data   => TrafficData,
               mode        => ready,
               sock        => Sock,
               port        => Port,
               assoc_id    => AssocID,
               stream      => 3,
               select      => undefined,
               status      => start_status_timer(),
               %% We do nothing until we get the 'start' order
               %% (except check socket status)
               run         => undefined,
               seq         => 1,
               cnt         => 0,
               msg         => undefined}).


loop(#{mode        := ready = Mode,
       sock        := Sock,
       assoc_id    := AssocID,
       parent      := Parent,
       parent_mref := MRef,
       run         := undefined} = State) ->
    receive
        ?MSG(Parent, {start_run, RunTime}) ->
            ?INFO("start run: ~w", [RunTime]),
            TRef = start_run_timer(RunTime),
            loop(State#{run => TRef, mode => send});

        {'DOWN', MRef, process, Parent, Reason} ->
            ?ERROR("Unexpected parent termination: "
                   "~n   ~p"
                   "~nwhen"
                   "~n   Mode: ~p", [Reason, Mode]),
            (catch socket:close(Sock)),
            exit({parent, Reason, Mode});

        {timeout, _OldTRef, status_check} ->
            ?INFO("Socket Status: "
                  "~n   ~p", [?WHICH_SCTP_STATUS(Sock, AssocID)]),
            loop(State#{status => start_status_timer()});


        ?MSG(Parent, stop) ->
            ?INFO("Received stop command"),
            graceful_assoc_shutdown(Sock, AssocID),
            (socket:close(Sock)),
            exit(normal)

    end;

%% Time to (try to) send a request
loop(#{mode      := send,
       msgs_data := [MsgData|MsgsData],
       sock      := Sock,
       assoc_id  := AssocID,
       stream    := Stream,
       select    := undefined,
       cnt       := Cnt,
       seq       := Seq,
       msg       := undefined} = State) ->
    ?DBG("try (build message and) send"),
    SRI     = #{assoc_id => AssocID, stream => Stream},
    CtrlSRI = #{level => sctp,
                type  => sndrcv,
                value => SRI},
    Msg = #{iov  => [?MK_REQUEST(Seq, MsgData)],
            ctrl => [CtrlSRI]},
    case socket:sendmsg(Sock, Msg, nowait) of
        ok ->
            ?DBG("sent -> enter recv mode"),
            loop(State#{msgs_data => lists:append(MsgsData, [MsgData]),
                        mode      => recv});

        {select, SelectInfo} ->
            ?DBG("select"),
            loop(State#{msgs_data => lists:append(MsgsData, [MsgData]),
                        select    => SelectInfo,
                        msg       => Msg});

        {error, Reason} ->
            ?ERROR("Failed sending request: "
                   "~n   Cnt:    ~w"
                   "~n   Seq:    ~w"
                   "~n   Reason: ~p", [Cnt, Seq, Reason]),
            exit({sendmsg, Reason})
    end;
loop(#{mode     := send,
       sock     := Sock,
       assoc_id := _AssocID,
       stream   := _Stream,
       select   := undefined,
       cnt      := Cnt,
       seq      := Seq,
       msg      := Msg} = State) ->
    ?DBG("try send"),
    case socket:sendmsg(Sock, Msg, nowait) of
        ok ->
            ?DBG("sent -> enter recv mode"),
            loop(State#{mode => recv,
                        msg  => undefined});

        {select, SelectInfo} ->
            ?DBG("select"),
            loop(State#{select => SelectInfo});

        {error, Reason} ->
            ?ERROR("Failed sending request: "
                   "~n   Cnt:    ~w"
                   "~n   Seq:    ~w"
                   "~n   Reason: ~p", [Cnt, Seq, Reason]),
            exit({sendmsg, Reason})
    end;

loop(#{mode     := recv,
       sock     := Sock,
       assoc_id := AssocID,
       select   := undefined,
       cnt      := Cnt,
       seq      := Seq} = State) ->
    ?DBG("try recv"),
    case socket:recvmsg(Sock, nowait) of
        {ok, #{flags := _,
               addr  := _,
               iov   := [Data],
               ctrl  := [#{level := sctp,
                           type  := sndrcv,
                           value := #{assoc_id := AssocID,
                                      stream   := _}}]}} ->
            ?DBG("data message received - verify"),
            case verify_reply(Seq, Data) of
                ok ->
                    loop(State#{mode => send,
                                cnt  => Cnt + 1,
                                seq  => next_seq(Seq)});
                {error, Reason} ->
                    ?ERROR("Received invalid message: "
                           "~n   Cnt:    ~p"
                           "~n   Reason: ~p", [Cnt, Reason]),
                    exit({recvmsg, verification, Reason})
            end;
        
        {ok, #{notification := #{type     := shutdown_event,
                                 assoc_id := AssocID}}} ->
            ?ERROR("Received unexpected shutdown event for assoc ~w",
                   [AssocID]),
            exit(unexpected_shutdown);

        {select, SelectInfo} ->
            ?DBG("select"),
            loop(State#{select => SelectInfo});

        {error, Reason} ->
            ?ERROR("Failed sending request: "
                   "~n   Cnt:    ~w"
                   "~n   Seq:    ~w"
                   "~n   Reason: ~p", [Cnt, Seq, Reason]),
            exit({sendmsg, Reason})
    end;

loop(#{mode        := Mode,
       parent      := Parent,
       parent_mref := MRef,
       sock        := Sock,
       assoc_id    := AssocID,
       select      := {select_info, _, SelectHandle} = SelectInfo,
       cnt         := Cnt,
       seq         := Seq} = State) ->
    ?DBG("await select message"),
    receive
        {'DOWN', MRef, process, Parent, Reason} ->
            ?ERROR("Unexpected parent termination: "
                   "~n   ~p"
                   "~nwhen"
                   "~n   Mode: ~p", [Reason, Mode]),
            (catch socket:close(Sock)),
            exit({parent, Reason, Mode});

        {'$socket', Sock, select, SelectHandle} ->
            ?DBG("received select message"),
            loop(State#{select => undefined});

        {timeout, _OldTRef, run} ->
            ?INFO("Received run timeout when: "
                  "~n   Mode:          ~p"
                  "~n   Cnt:           ~p"
                  "~n   Seq:           ~p"
                  "~n   Socket Status: ~p",
                  [Mode, Cnt, Seq, ?WHICH_SCTP_STATUS(Sock, AssocID)]),
            socket:cancel(Sock, SelectInfo),
            graceful_assoc_shutdown(Sock, AssocID),
            (socket:close(Sock)),
            exit({result, Cnt, Mode});

        {timeout, _OldTRef, status_check} ->
            ?INFO("Socket Status: "
                  "~n   ~p", [?WHICH_SCTP_STATUS(Sock, AssocID)]),
            loop(State#{status => start_status_timer()});


        ?MSG(Parent, stop) ->
            ?INFO("Received stop command"),
            socket:cancel(Sock, SelectInfo),
            graceful_assoc_shutdown(Sock, AssocID),
            (socket:close(Sock)),
            exit({result, Cnt, Mode})

    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify_reply(Seq,
             <<(?PROTO_TAG):32,
               (?REPLY_TAG):32,
               (Seq):32,
               PayloadSz:32,
               CheckSum:32,
               Payload:PayloadSz/binary>>) ->
    case erlang:crc32(Payload) of
        CheckSum ->
            ok;
        BadCheckSum ->
            {error, {bad_checksum, CheckSum, BadCheckSum}}
    end;
verify_reply(_, _) ->
    {error, bad_reply}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

graceful_assoc_shutdown(Sock, AssocID) ->
    EofSRI = #{assoc_id     => AssocID,
               stream       => 0,
               ssn          => 0,
               flags        => [eof],
               ppid         => 0,
               context      => 0,
               time_to_live => 0,
               tsn          => 0,
               cum_tsn      => 0},
    EofMsg = #{iov  => [],
               ctrl => [#{level => sctp,
                          type  => sndrcv,
                          value => EofSRI}]},
    case socket:sendmsg(Sock, EofMsg) of
        ok ->
            ok;
        {error, Reason} ->
            ?ERROR("Failed sending (graceful shutdown) eof message:"
                   "~n   ~p", [Reason]),
            error
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_run_timer(Time) ->
    start_timer(Time, run).

start_status_timer() ->
    start_timer(?STATUS_TIMEOUT, status_check).

start_timer(Time, Msg) ->
    erlang:start_timer(Time, self(), Msg).

%% stop_timer(TRef) ->
%%     erlang:cancel_timer(TRef).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_seq(CurrSeq) when CurrSeq < 16#FFFFFFFF ->
    CurrSeq;
next_seq(_) ->
    1.


set_debug(#{debug := Debug}) when is_boolean(Debug) ->
    ?SET_DEBUG(Debug);
set_debug(_) ->
    ?SET_DEBUG(false).

