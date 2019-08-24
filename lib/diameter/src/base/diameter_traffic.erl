%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2019. All Rights Reserved.
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
%% Implements the handling of incoming and outgoing Diameter messages
%% except CER/CEA, DWR/DWA and DPR/DPA. That is, the messages that a
%% diameter client sends and receives.
%%

-module(diameter_traffic).

%% towards diameter
-export([send_request/4]).

%% towards diameter_watchdog
-export([receive_message/5]).

%% towards diameter_peer_fsm and diameter_watchdog
-export([incr/4,
         incr_error/4,
         incr_rc/4]).

%% towards diameter_service
-export([make_recvdata/1,
         peer_up/1,
         peer_down/1]).

%% towards diameter_dist
-export([request_info/1]).

%% internal
-export([send/1,    %% send from remote node
         request/1, %% process request in handler process
         init/1]).  %% monitor process start

-include_lib("diameter/include/diameter.hrl").
-include("diameter_internal.hrl").

-define(LOGX(Reason, T), begin ?LOG(Reason, T), x({Reason, T}) end).

-define(RELAY, ?DIAMETER_DICT_RELAY).
-define(BASE,  ?DIAMETER_DICT_COMMON).  %% Note: the RFC 3588 dictionary

-define(DEFAULT(V, Def), if V == undefined -> Def; true -> V end).

%% Table containing outgoing entries that live and die with
%% peer_up/down. The name is historic, since the table used to contain
%% information about outgoing requests for which an answer has yet to
%% be received.
-define(REQUEST_TABLE, diameter_request).

%% Record diameter:call/4 options are parsed into.
-record(options,
        {peers = []     :: [diameter:peer_ref()],
         filter = none  :: diameter:peer_filter(),
         extra = []     :: list(),
         timeout = 5000 :: 0..16#FFFFFFFF,  %% for outgoing requests
         detach = false :: boolean()}).

%% Term passed back to receive_message/5 with every incoming message.
-record(recvdata,
        {peerT        :: ets:tid(),
         service_name :: diameter:service_name(),
         apps         :: [#diameter_app{}],
         sequence     :: diameter:sequence(),
         counters     :: boolean(),
         codec        :: #{decode_format := diameter:decode_format(),
                           avp_dictionaries => nonempty_list(module()),
                           string_decode := boolean(),
                           strict_arities => diameter:strict_arities(),
                           strict_mbit := boolean(),
                           incoming_maxlen := diameter:message_length()}}).
%% Note that incoming_maxlen is currently handled in diameter_peer_fsm,
%% so that any message exceeding the maximum is discarded. Retain the
%% option in case we want to extend the values and semantics.

%% Record stored in diameter_request for each outgoing request.
-record(request,
        {ref        :: reference(),         %% used to receive answer
         caller     :: pid() | undefined,   %% calling process
         handler    :: pid(),               %% request process
         peer       :: undefined | {pid(), #diameter_caps{}},
         caps       :: undefined,           %% no longer used
         packet     :: #diameter_packet{} | undefined}). %% of request

%% ---------------------------------------------------------------------------
%% make_recvdata/1
%% ---------------------------------------------------------------------------

make_recvdata([SvcName, PeerT, Apps, SvcOpts | _]) ->
    #{sequence := {_,_} = Mask, spawn_opt := Opts, traffic_counters := B}
        = SvcOpts,
    {Opts, #recvdata{service_name = SvcName,
                     peerT = PeerT,
                     apps = Apps,
                     sequence = Mask,
                     counters = B,
                     codec = maps:with([decode_format,
                                        avp_dictionaries,
                                        string_decode,
                                        strict_arities,
                                        strict_mbit,
                                        ordered_encode,
                                        incoming_maxlen],
                                       SvcOpts)}}.

%% ---------------------------------------------------------------------------
%% peer_up/1
%% ---------------------------------------------------------------------------

%% Start a process that dies with peer_down/1, on which request
%% processes can monitor. There is no other process that dies with
%% peer_down since failover doesn't imply the loss of transport in the
%% case of a watchdog transition into state SUSPECT.
peer_up(TPid) ->
    proc_lib:start(?MODULE, init, [TPid]).

init(TPid) ->
    ets:insert(?REQUEST_TABLE, {TPid, self()}),
    proc_lib:init_ack(self()),
    proc_lib:hibernate(erlang, exit, [{shutdown, TPid}]).

%% ---------------------------------------------------------------------------
%% peer_down/1
%% ---------------------------------------------------------------------------

peer_down(TPid) ->
    [{_, Pid}] = ets:lookup(?REQUEST_TABLE, TPid),
    ets:delete(?REQUEST_TABLE, TPid),
    Pid ! ok,  %% make it die
    Pid.

%% ---------------------------------------------------------------------------
%% incr/4
%% ---------------------------------------------------------------------------

incr(Dir, #diameter_packet{header = H}, TPid, AppDict) ->
    incr(Dir, H, TPid, AppDict);

incr(Dir, #diameter_header{} = H, TPid, AppDict) ->
    incr(TPid, {msg_id(H, AppDict), Dir}).

%% ---------------------------------------------------------------------------
%% incr_error/4
%% ---------------------------------------------------------------------------

%% Identify messages using the application dictionary, not the encode
%% dictionary, which may differ in the case of answer-message.
incr_error(Dir, T, Pid, {_MsgDict, AppDict}) ->
    incr_error(Dir, T, Pid, AppDict);

%% Decoded message without errors.
incr_error(recv, #diameter_packet{errors = []}, _, _) ->
    ok;

incr_error(recv = D, #diameter_packet{header = H}, TPid, AppDict) ->
    incr_error(D, H, TPid, AppDict);

%% Encoded message with errors and an identifiable header ...
incr_error(send = D, {_, _, #diameter_header{} = H}, TPid, AppDict) ->
    incr_error(D, H, TPid, AppDict);

%% ... or not.
incr_error(send = D, {_,_}, TPid, _) ->
    incr_error(D, unknown, TPid);

incr_error(Dir, #diameter_header{} = H, TPid, AppDict) ->
    incr_error(Dir, msg_id(H, AppDict), TPid);

incr_error(Dir, Id, TPid, _) ->
    incr_error(Dir, Id, TPid).

incr_error(Dir, Id, TPid) ->
    incr(TPid, {Id, Dir, error}).

%% ---------------------------------------------------------------------------
%% incr_rc/4
%% ---------------------------------------------------------------------------

-spec incr_rc(send|recv, Pkt, TPid, DictT)
   -> Counter
    | Reason
 when Pkt :: #diameter_packet{},
      TPid :: pid(),
      DictT :: module() | {MsgDict :: module(),
                           AppDict :: module(),
                           CommonDict:: module()},
      Counter :: {'Result-Code', integer()}
               | {'Experimental-Result', integer(), integer()},
      Reason :: atom().

incr_rc(Dir, Pkt, TPid, {MsgDict, AppDict, Dict0}) ->
    incr_rc(Dir, Pkt, TPid, MsgDict, AppDict, Dict0);

incr_rc(Dir, Pkt, TPid, Dict0) ->
    incr_rc(Dir, Pkt, TPid, Dict0, Dict0, Dict0).

%% incr_rc/6

incr_rc(Dir, Pkt, TPid, MsgDict, AppDict, Dict0) ->
    try get_result(Dir, MsgDict, Dict0, Pkt) of
        false ->
            unknown;
        Avp ->
            incr_result(Dir, Avp, Pkt, TPid, AppDict)
    catch
        exit: {E,_} when E == no_result_code;
                         E == invalid_error_bit ->
            incr(TPid, {msg_id(Pkt#diameter_packet.header, AppDict), Dir, E}),
            E
    end.

%% ---------------------------------------------------------------------------
%% receive_message/5
%%
%% Handle an incoming Diameter message in a watchdog process.
%% ---------------------------------------------------------------------------

-spec receive_message(pid(), Route, #diameter_packet{}, module(), RecvData)
   -> pid()     %% request handler
    | boolean() %% answer, known request or not
    | discard   %% request discarded
 when Route :: {Handler, RequestRef, TPid}
             | Ack,
      RecvData :: {[SpawnOpt], #recvdata{}},
      SpawnOpt :: term(),
      Handler :: pid(),
      RequestRef :: reference(),
      TPid :: pid(),
      Ack :: boolean().

receive_message(TPid, Route, Pkt, Dict0, RecvData) ->
    #diameter_packet{header = #diameter_header{is_request = R}} = Pkt,
    recv(R, Route, TPid, Pkt, Dict0, RecvData).

%% recv/6

%% Incoming request ...
recv(true, Ack, TPid, Pkt, Dict0, T)
  when is_boolean(Ack) ->
    {Opts, RecvData} = T,
    AppT = find_app(TPid, Pkt, RecvData),
    ack(Ack, TPid, spawn_request(AppT, Opts, Ack, TPid, Pkt, Dict0, RecvData));

%% ... answer to known request ...
recv(false, {Pid, Ref, TPid}, _, Pkt, Dict0, _) ->
    Pid ! {answer, Ref, TPid, Dict0, Pkt},
    true;

%% Note that failover could have happened prior to this message being
%% received and triggering failback. That is, both a failover message
%% and answer may be on their way to the handler process. In the worst
%% case the request process gets notification of the failover and
%% sends to the alternate peer before an answer arrives, so it's
%% always the case that we can receive more than one answer after
%% failover. The first answer received by the request process wins,
%% any others are discarded.

%% ... or not.
recv(false, false, TPid, Pkt, _, _) ->
    ?LOG(discarded, Pkt#diameter_packet.header),
    incr(TPid, {{unknown, 0}, recv, discarded}),
    false.

%% spawn_request/7

spawn_request(false, _, _, _, _, _, _) ->  %% no transport
    discard;

%% An MFA should return the pid() of a process in which the argument
%% fun in applied, or the atom 'discard' if the fun is not applied.
%% The latter results in an acknowledgment back to the transport
%% process when appropriate, to ensure that send/recv callbacks can
%% count outstanding requests. Acknowledgement is implicit if the
%% handler process dies (in a handle_request callback for example).
spawn_request(AppT, {M,F,A}, Ack, TPid, Pkt, Dict0, RecvData) ->
    %% Term to pass to request/1 in an appropriate process. Module
    %% diameter_dist implements callbacks.
    ReqT = {Pkt, AppT, Ack, TPid, Dict0, RecvData},
    apply(M, F, [ReqT | A]);

%% A spawned process acks implicitly when it dies, so there's no need
%% to handle 'discard'.
spawn_request(AppT, Opts, Ack, TPid, Pkt, Dict0, RecvData) ->
    spawn_opt(fun() ->
                      recv_request(Ack, TPid, Pkt, Dict0, RecvData, AppT)
              end,
              Opts).

%% request_info/1
%%
%% Limited request information for diameter_dist.

request_info({Pkt, _AppT, _Ack, _TPid, _Dict0, RecvData} = _ReqT) ->
    {RecvData#recvdata.service_name, Pkt#diameter_packet.bin}.

%% request/1
%%
%% Called from a handler process chosen by a transport spawn_opt MFA
%% to process an incoming request.

request({Pkt, AppT, Ack, TPid, Dict0, RecvData} = _ReqT) ->
    ack(Ack, TPid, recv_request(Ack, TPid, Pkt, Dict0, RecvData, AppT)).

%% ack/3

ack(Ack, TPid, RC) ->
    RC == discard
        andalso Ack
        andalso (TPid ! {send, false}),
    RC.

%% ---------------------------------------------------------------------------
%% recv_request/6
%% ---------------------------------------------------------------------------

-spec recv_request(Ack :: boolean(),
                   TPid :: pid(),
                   #diameter_packet{},
                   Dict0 :: module(),
                   #recvdata{},
                   AppT :: {#diameter_app{}, #diameter_caps{}}
                         | #diameter_caps{}) %% no suitable app
   -> ok        %% answer was sent
    | discard.  %% or not

recv_request(Ack, TPid, Pkt, Dict0, RecvData, AppT) ->
    Ack andalso (TPid ! {handler, self()}),
    case AppT of
        {#diameter_app{id = Aid, dictionary = AppDict} = App, Caps} ->
            Count = RecvData#recvdata.counters,
            Count andalso incr(recv, Pkt, TPid, AppDict),
            DecPkt = decode(Aid, AppDict, RecvData, Pkt),
            Count andalso incr_error(recv, DecPkt, TPid, AppDict),
            send_A(recv_R(App, TPid, Dict0, Caps, RecvData, DecPkt),
                   TPid,
                   App,
                   Dict0,
                   RecvData,
                   DecPkt,
                   Caps);
        #diameter_caps{} = Caps ->
            %%   DIAMETER_APPLICATION_UNSUPPORTED   3007
            %%      A request was sent for an application that is not
            %%      supported.
            RC = 3007,
            DecPkt = diameter_codec:collect_avps(Pkt),
            send_answer(answer_message(RC, Dict0, Caps, DecPkt),
                        TPid,
                        Dict0,
                        Dict0,
                        Dict0,
                        RecvData,
                        DecPkt,
                        [[]])
    end.

%% find_app/3
%%
%% Lookup the application of a received Diameter request on the node
%% on which it's received.

find_app(TPid,
         #diameter_packet{header = #diameter_header{application_id = Id}},
         #recvdata{peerT = PeerT,
                   apps = Apps}) ->
    diameter_service:find_incoming_app(PeerT, TPid, Id, Apps).

%% decode/4

decode(Id, Dict, #recvdata{codec = Opts}, Pkt) ->
    errors(Id, diameter_codec:decode(Id, Dict, Opts, Pkt)).

%% send_A/7

send_A([T | Fs], TPid, App, Dict0, RecvData, DecPkt, Caps) ->
    send_A(T, TPid, App, Dict0, RecvData, DecPkt, Caps, Fs);

send_A(discard = No, _, _, _, _, _, _) ->
    No.

%% recv_R/6

%% Answer errors ourselves ...
recv_R(#diameter_app{options = [_, {request_errors, E} | _]},
       _TPid,
       Dict0,
       _Caps,
       _RecvData,
       #diameter_packet{errors = [RC|_]})  %% a detected 3xxx is hd
  when E == answer, Dict0 /= ?BASE orelse 3 == RC div 1000;
       E == answer_3xxx, 3 == RC div 1000 ->
    [{answer_message, rc(RC)}, []];

%% ... or make a handle_request callback. Note that
%% Pkt#diameter_packet.msg = undefined in the 3001 case.
recv_R(App,
       TPid,
       _Dict0,
       Caps,
       #recvdata{service_name = SvcName},
       Pkt) ->
    request_cb(cb(App, handle_request, [Pkt, SvcName, {TPid, Caps}]),
               App,
               [],
               []).

rc({N,_}) ->
    N;
rc(N) ->
    N.

%% errors/1
%%
%% Look for additional errors in a decoded message, prepending the
%% errors field with the first detected error. It's odd/unfortunate
%% that 501[15] aren't protocol errors. With RFC 3588 this means that
%% a handle_request callback has to formulate the answer. With RFC
%% 6733 it's acceptable for 5xxx to be sent in an answer-message.

%%   DIAMETER_INVALID_MESSAGE_LENGTH 5015
%%      This error is returned when a request is received with an invalid
%%      message length.

errors(_, #diameter_packet{header = #diameter_header{length = Len} = H,
                           bin = Bin,
                           errors = Es}
          = Pkt)
  when Len < 20;
       0 /= Len rem 4;
       8*Len /= bit_size(Bin) ->
    ?LOG(invalid_message_length, {H, bit_size(Bin)}),
    Pkt#diameter_packet{errors = [5015 | Es]};

%%   DIAMETER_UNSUPPORTED_VERSION       5011
%%      This error is returned when a request was received, whose version
%%      number is unsupported.

errors(_, #diameter_packet{header = #diameter_header{version = V} = H,
                           errors = Es}
          = Pkt)
  when V /= ?DIAMETER_VERSION ->
    ?LOG(unsupported_version, H),
    Pkt#diameter_packet{errors = [5011 | Es]};

%%   DIAMETER_COMMAND_UNSUPPORTED       3001
%%      The Request contained a Command-Code that the receiver did not
%%      recognize or support.  This MUST be used when a Diameter node
%%      receives an experimental command that it does not understand.

errors(Id, #diameter_packet{header = #diameter_header{is_proxiable = P} = H,
                            msg = M,
                            errors = Es}
           = Pkt)
  when ?APP_ID_RELAY /= Id, undefined == M;  %% don't know the command
       ?APP_ID_RELAY == Id, not P ->         %% command isn't proxiable
    ?LOG(command_unsupported, H),
    Pkt#diameter_packet{errors = [3001 | Es]};

%%   DIAMETER_INVALID_HDR_BITS          3008
%%      A request was received whose bits in the Diameter header were
%%      either set to an invalid combination, or to a value that is
%%      inconsistent with the command code's definition.

errors(_, #diameter_packet{header = #diameter_header{is_request = true,
                                                     is_error = true}
                                  = H,
                            errors = Es}
          = Pkt) ->
    ?LOG(invalid_hdr_bits, H),
    Pkt#diameter_packet{errors = [3008 | Es]};

%% Green.
errors(_, Pkt) ->
    Pkt.

%% request_cb/4

%% A reply may be an answer-message, constructed either here or by
%% the handle_request callback. The header from the incoming request
%% is passed into the encode so that it can retrieve the relevant
%% command code in this case. It will also then ignore Dict and use
%% the base encoder.
request_cb({reply, _Ans} = T, _App, EvalPktFs, EvalFs) ->
    [T, EvalPktFs | EvalFs];

%% An 3xxx result code, for which the E-bit is set in the header.
request_cb({protocol_error, RC}, _App, EvalPktFs, EvalFs)
  when 3 == RC div 1000 ->
    [{answer_message, RC}, EvalPktFs | EvalFs];

request_cb({answer_message, RC} = T, _App, EvalPktFs, EvalFs)
  when 3 == RC div 1000;
       5 == RC div 1000 ->
    [T, EvalPktFs | EvalFs];

%% RFC 3588 says we must reply 3001 to anything unrecognized or
%% unsupported. 'noreply' is undocumented (and inappropriately named)
%% backwards compatibility for this, protocol_error the documented
%% alternative.
request_cb(noreply, _App, EvalPktFs, EvalFs) ->
    [{answer_message, 3001}, EvalPktFs | EvalFs];

%% Relay a request to another peer. This is equivalent to doing an
%% explicit call/4 with the message in question except that (1) a loop
%% will be detected by examining Route-Record AVP's, (3) a
%% Route-Record AVP will be added to the outgoing request and (3) the
%% End-to-End Identifier will default to that in the
%% #diameter_header{} without the need for an end_to_end_identifier
%% option.
%%
%% relay and proxy are similar in that they require the same handling
%% with respect to Route-Record and End-to-End identifier. The
%% difference is that a proxy advertises specific applications, while
%% a relay advertises the relay application. If a callback doesn't
%% want to distinguish between the cases in the callback return value
%% then 'resend' is a neutral alternative.
%%
request_cb({A, Opts}, #diameter_app{id = Id}, EvalPktFs, EvalFs)
  when A == relay, Id == ?APP_ID_RELAY;
       A == proxy, Id /= ?APP_ID_RELAY;
       A == resend ->
    [{call, Opts}, EvalPktFs | EvalFs];

request_cb(discard = No, _, _, _) ->
    No;

request_cb({eval_packet, RC, F}, App, Fs, EvalFs) ->
    request_cb(RC, App, [F|Fs], EvalFs);

request_cb({eval, RC, F}, App, EvalPktFs, Fs) ->
    request_cb(RC, App, EvalPktFs, [F|Fs]);

request_cb(T, App, _, _) ->
    ?ERROR({invalid_return, T, handle_request, App}).

%% send_A/8

send_A({reply, Ans}, TPid, App, Dict0, RecvData, Pkt, _Caps, Fs) ->
    AppDict = App#diameter_app.dictionary,
    MsgDict = msg_dict(AppDict, Dict0, Ans),
    send_answer(Ans,
                TPid,
                MsgDict,
                AppDict,
                Dict0,
                RecvData,
                Pkt,
                Fs);

send_A({call, Opts}, TPid, App, Dict0, RecvData, Pkt, Caps, Fs) ->
    AppDict = App#diameter_app.dictionary,
    case resend(Opts, Caps, Pkt, App, Dict0, RecvData) of
        #diameter_packet{bin = Bin} = Ans -> %% answer: reset hop by hop id
            #diameter_packet{header = #diameter_header{hop_by_hop_id = Id},
                             transport_data = TD}
                = Pkt,
            Reset = diameter_codec:hop_by_hop_id(Id, Bin),
            MsgDict = msg_dict(AppDict, Dict0, Ans),
            send_answer(Ans#diameter_packet{bin = Reset,
                                            transport_data = TD},
                        TPid,
                        MsgDict,
                        AppDict,
                        Dict0,
                        RecvData#recvdata.counters,
                        Fs);
        RC ->
            send_answer(answer_message(RC, Dict0, Caps, Pkt),
                        TPid,
                        Dict0,
                        AppDict,
                        Dict0,
                        RecvData,
                        Pkt,
                        Fs)
    end;

%% RFC 3588 only allows 3xxx errors in an answer-message. RFC 6733
%% added the possibility of setting 5xxx.

send_A({answer_message, RC} = T, TPid, App, Dict0, RecvData, Pkt, Caps, Fs) ->
    Dict0 /= ?BASE orelse 3 == RC div 1000
        orelse ?ERROR({invalid_return, T, handle_request, App}),
    send_answer(answer_message(RC, Dict0, Caps, Pkt),
                TPid,
                Dict0,
                App#diameter_app.dictionary,
                Dict0,
                RecvData,
                Pkt,
                Fs).

%% send_answer/8

%% Skip the setting of Result-Code and Failed-AVP's below. This is
%% undocumented and shouldn't be relied on.
send_answer([Ans], TPid, MsgDict, AppDict, Dict0, RecvData, Pkt, Fs)
  when [] == Pkt#diameter_packet.errors ->
    send_answer(Ans, TPid, MsgDict, AppDict, Dict0, RecvData, Pkt, Fs);
send_answer([Ans], TPid, MsgDict, AppDict, Dict0, RecvData, Pkt0, Fs) ->
    Pkt = Pkt0#diameter_packet{errors = []},
    send_answer(Ans, TPid, MsgDict, AppDict, Dict0, RecvData, Pkt, Fs);

send_answer(Ans, TPid, MsgDict, AppDict, Dict0, RecvData, DecPkt, Fs) ->
    Pkt = encode({MsgDict, AppDict},
                 TPid,
                 RecvData#recvdata.codec,
                 make_answer_packet(Ans, DecPkt, MsgDict, Dict0)),
    send_answer(Pkt,
                TPid,
                MsgDict,
                AppDict,
                Dict0,
                RecvData#recvdata.counters,
                Fs).

%% send_answer/7

send_answer(Pkt, TPid, MsgDict, AppDict, Dict0, Count, [EvalPktFs | EvalFs]) ->
    eval_packet(Pkt, EvalPktFs),
    Count andalso begin
                      incr(send, Pkt, TPid, AppDict),
                      incr_rc(send, Pkt, TPid, MsgDict, AppDict, Dict0)
                  end,
    send(TPid, z(Pkt), _Route = self()),
    lists:foreach(fun diameter_lib:eval/1, EvalFs).

%% msg_dict/3
%%
%% Return the dictionary defining the message grammar in question: the
%% application dictionary or the common dictionary.

msg_dict(AppDict, Dict0, [Msg]) ->
    msg_dict(AppDict, Dict0, Msg);

msg_dict(AppDict, Dict0, Msg) ->
    choose(is_answer_message(Msg, Dict0), Dict0, AppDict).

%% Incoming, not yet decoded.
is_answer_message(#diameter_packet{header = #diameter_header{} = H,
                                   msg = undefined},
                  Dict0) ->
    is_answer_message([H], Dict0);

is_answer_message(#diameter_packet{msg = Msg}, Dict0) ->
    is_answer_message(Msg, Dict0);

%% Message sent as a header/avps list.
is_answer_message([#diameter_header{is_request = R, is_error = E} | _], _) ->
    E andalso not R;

%% Message sent as a map or tagged avp/value list.
is_answer_message([Name | _], _) ->
    Name == 'answer-message';

%% Message sent as a record.
is_answer_message(Rec, Dict) ->
    try
        'answer-message' == Dict:rec2msg(element(1,Rec))
    catch
        error:_ -> false
    end.

%% resend/6

resend(Opts, Caps, Pkt, App, Dict0, RecvData) ->
    resend(is_loop(Dict0, Caps, Pkt), Opts, Caps, Pkt, App, Dict0, RecvData).

%% resend/7

%%   DIAMETER_LOOP_DETECTED             3005
%%      An agent detected a loop while trying to get the message to the
%%      intended recipient.  The message MAY be sent to an alternate peer,
%%      if one is available, but the peer reporting the error has
%%      identified a configuration problem.

resend(true, _Opts, _Caps, _Pkt, _App, _Dict0, _RecvData) ->
    3005;

%% 6.1.8.  Relaying and Proxying Requests
%%
%%   A relay or proxy agent MUST append a Route-Record AVP to all requests
%%   forwarded.  The AVP contains the identity of the peer the request was
%%   received from.

resend(false,
       Opts,
       #diameter_caps{origin_host = {_,OH}},
       #diameter_packet{header = Hdr0,
                        avps = Avps},
       App,
       Dict0,
       #recvdata{service_name = SvcName,
                 sequence = Mask}) ->
    Route = #diameter_avp{data = {Dict0, 'Route-Record', OH}},
    Seq = diameter_session:sequence(Mask),
    Hdr = Hdr0#diameter_header{hop_by_hop_id = Seq},
    Msg = [Hdr | Avps ++ [Route]],
    case send_request(SvcName, App, Msg, Opts) of
        #diameter_packet{} = Ans ->
            Ans;
        _ ->
            3002  %% DIAMETER_UNABLE_TO_DELIVER.
    end.
%% The incoming request is relayed with the addition of a
%% Route-Record. Note the requirement on the return from call/4 below,
%% which places a requirement on the value returned by the
%% handle_answer callback of the application module in question.
%%
%% Note that there's nothing stopping the request from being relayed
%% back to the sender. A pick_peer callback may want to avoid this but
%% a smart peer might recognize the potential loop and choose another
%% route. A less smart one will probably just relay the request back
%% again and force us to detect the loop. A pick_peer that wants to
%% avoid this can specify filter to avoid the possibility.
%% Eg. {neg, {host, OH} where #diameter_caps{origin_host = {OH, _}}.
%%
%% RFC 6.3 says that a relay agent does not modify Origin-Host but
%% says nothing about a proxy. Assume it should behave the same way.

%% is_loop/3

is_loop(Dict0,
        #diameter_caps{origin_host = {OH,_}},
        #diameter_packet{avps = Avps}) ->
    {Code, _Flags, Vid} = Dict0:avp_header('Route-Record'),
    is_loop(Code, Vid, OH, Avps).

%% is_loop/4
%%
%% Is there a Route-Record AVP with our Origin-Host?

is_loop(Code, Vid, Bin, [#diameter_avp{code = Code,
                                       vendor_id = Vid,
                                       data = Bin}
                         | _]) ->
    true;

is_loop(_, _, _, []) ->
    false;

is_loop(Code, Vid, OH, [_ | Avps])
  when is_binary(OH) ->
    is_loop(Code, Vid, OH, Avps);

is_loop(Code, Vid, OH, Avps) ->
    is_loop(Code, Vid, list_to_binary(OH), Avps).

%% select_error/3
%%
%% Extract the first appropriate RC or {RC, #diameter_avp{}}
%% pair from an errors list, along with any leading #diameter_avp{}.
%%
%% RFC 6733:
%%
%%  7.5.  Failed-AVP AVP
%%
%%   The Failed-AVP AVP (AVP Code 279) is of type Grouped and provides
%%   debugging information in cases where a request is rejected or not
%%   fully processed due to erroneous information in a specific AVP.  The
%%   value of the Result-Code AVP will provide information on the reason
%%   for the Failed-AVP AVP.  A Diameter answer message SHOULD contain an
%%   instance of the Failed-AVP AVP that corresponds to the error
%%   indicated by the Result-Code AVP.  For practical purposes, this
%%   Failed-AVP would typically refer to the first AVP processing error
%%   that a Diameter node encounters.
%%
%% 3xxx can only be set in an answer setting the E-bit. RFC 6733 also
%% allows 5xxx, RFC 3588 doesn't.

select_error(E, Es, Dict0) ->
    select(E, Es, Dict0, []).

%% select/4

select(E, [{RC, _} = T | Es], Dict0, Avps) ->
    select(E, RC, T, Es, Dict0, Avps);

select(E, [#diameter_avp{} = A | Es], Dict0, Avps) ->
    select(E, Es, Dict0, [A | Avps]);

select(E, [RC | Es], Dict0, Avps) ->
    select(E, RC, RC, Es, Dict0, Avps);

select(_, [], _, Avps) ->
    Avps.

%% select/6

select(E, RC, T, _, Dict0, Avps)
  when E, 3000 =< RC, RC < 4000;                 %% E-bit with 3xxx
       E, ?BASE /= Dict0, 5000 =< RC, RC < 6000; %% E-bit with 5xxx
       not E, RC < 3000 orelse 4000 =< RC ->     %% no E-bit
    [T | Avps];

select(E, _, _, Es, Dict0, Avps) ->
    select(E, Es, Dict0, Avps).

%% eval_packet/2

eval_packet(Pkt, Fs) ->
    lists:foreach(fun(F) -> diameter_lib:eval([F,Pkt]) end, Fs).

%% make_answer_packet/4

%% Use decode errors to set Result-Code and/or Failed-AVP unless the
%% the errors field has been explicitly set. Unfortunately, the
%% default value is the empty list rather than 'undefined' so use the
%% atom 'false' for "set nothing". (This is historical and changing
%% the default value would impact anyone expecting relying on the old
%% default.)

make_answer_packet(#diameter_packet{header = Hdr,
                                    msg = Msg,
                                    errors = Es,
                                    transport_data = TD},
                   #diameter_packet{header = Hdr0,
                                    errors = Es0},
                   MsgDict,
                   Dict0) ->
    #diameter_packet{header = make_answer_header(Hdr0, Hdr),
                     msg = reset(Msg, Es0, Es, MsgDict, Dict0),
                     transport_data = TD};

%% Binaries and header/avp lists are sent as-is.
make_answer_packet(Bin, #diameter_packet{transport_data = TD}, _, _)
  when is_binary(Bin) ->
    #diameter_packet{bin = Bin,
                     transport_data = TD};
make_answer_packet([#diameter_header{} | _] = Msg,
                   #diameter_packet{transport_data = TD},
                   _,
                   _) ->
    #diameter_packet{msg = Msg,
                     transport_data = TD};

make_answer_packet(Msg,
                   #diameter_packet{header = Hdr,
                                    errors = Es,
                                    transport_data = TD},
                   MsgDict,
                   Dict0) ->
    #diameter_packet{header = make_answer_header(Hdr, undefined),
                     msg = reset(Msg, [], Es, MsgDict, Dict0),
                     transport_data = TD}.

%% make_answer_header/2

%% A reply message clears the R and T flags and retains the P flag.
%% The E flag will be set at encode. 6.2 of 3588 requires the same P
%% flag on an answer as on the request. A #diameter_packet{} returned
%% from a handle_request callback can circumvent this by setting its
%% own header values.
make_answer_header(ReqHdr, Hdr) ->
    Hdr0 = ReqHdr#diameter_header{version = ?DIAMETER_VERSION,
                                  is_request = false,
                                  is_error = undefined,
                                  is_retransmitted = false},
    fold_record(Hdr0, Hdr).

%% reset/5

reset(Msg, [_|_] = Es0, [] = Es, MsgDict, Dict0) ->
    reset(Msg, Es, Es0, MsgDict, Dict0);

reset(Msg, _, Es, _, _)
  when Es == false;
       Es == [] ->
    Msg;

reset(Msg, _, Es, MsgDict, Dict0) ->
    E = is_answer_message(Msg, Dict0),
    reset(Msg, select_error(E, Es, Dict0), choose(E, Dict0, MsgDict)).

%% reset/4
%%
%% Set Result-Code and/or Failed-AVP (maybe). Only RC and {RC, AVP}
%% are the result of decode. AVP or {RC, [AVP]} can be set in an
%% answer for encode, as a convenience for injecting additional AVPs
%% into Failed-AVP; eg. 5001 = DIAMETER_AVP_UNSUPPORTED.

reset(Msg, [], _) ->
    Msg;

reset(Msg, [{RC, As} | Avps], Dict)
  when is_list(As) ->
    reset(Msg, [RC | As ++ Avps], Dict);

reset(Msg, [{RC, Avp} | Avps], Dict) ->
    reset(Msg, [RC, Avp | Avps], Dict);

reset(Msg, [#diameter_avp{} | _] = Avps, Dict) ->
    set(Msg, failed_avp(Msg, Avps, Dict), Dict);

reset(Msg, [RC | Avps], Dict) ->
    set(Msg, rc(Msg, RC, Dict) ++ failed_avp(Msg, Avps, Dict), Dict).

%% set/3

%% Reply as name/values list ...
set([Name|As], Avps, _)
  when is_map(As) ->
    [Name | maps:merge(As, maps:from_list(Avps))];
set([_|_] = Ans, Avps, _) ->
    Ans ++ Avps;  %% Values nearer tail take precedence.

%% ... or record.
set(Rec, Avps, Dict) ->
    Dict:'#set-'(Avps, Rec).

%% rc/3
%%
%% Turn the result code into a list if its optional and only set it if
%% the arity is 1 or {0,1}. In other cases (which probably shouldn't
%% exist in practice) we can't know what's appropriate.

rc([MsgName | _], RC, Dict) ->
    K = 'Result-Code',
    case Dict:avp_arity(MsgName, K) of
        1     -> [{K, RC}];
        {0,1} -> [{K, [RC]}];
        _     -> []
    end;

rc(Rec, RC, Dict) ->
    rc([Dict:rec2msg(element(1, Rec))], RC, Dict).

%% failed_avp/3

failed_avp(_, [] = No, _) ->
    No;

failed_avp(Msg, [_|_] = Avps, Dict) ->
    [failed(Msg, [{'AVP', Avps}], Dict)].

%% failed/3

failed(Msg, FailedAvp, Dict) ->
    RecName = msg2rec(Msg, Dict),
    try
        Dict:'#info-'(RecName, {index, 'Failed-AVP'}), %% assert existence
        {'Failed-AVP', [FailedAvp]}
    catch
        error: _ ->
            Avps = values(Msg, 'AVP', Dict),
            A = #diameter_avp{name = 'Failed-AVP',
                              value = FailedAvp},
            {'AVP', [A|Avps]}
    end.

%% msg2rec/2

%% Message as name/values list ...
msg2rec([MsgName | _], Dict) ->
    Dict:msg2rec(MsgName);

%% ... or record.
msg2rec(Rec, _) ->
    element(1, Rec).

%% values/2

%% Message as name/values list ...
values([_ | Avps], F, _) ->
    if is_map(Avps) ->
            maps:get(F, Avps, []);
       is_list(Avps) ->
            proplists:get_value(F, Avps, [])
    end;

%% ... or record.
values(Rec, F, Dict) ->
    Dict:'#get-'(F, Rec).

%% 3.  Diameter Header
%%
%%       E(rror)     - If set, the message contains a protocol error,
%%                     and the message will not conform to the ABNF
%%                     described for this command.  Messages with the 'E'
%%                     bit set are commonly referred to as error
%%                     messages.  This bit MUST NOT be set in request
%%                     messages.  See Section 7.2.

%% 3.2.  Command Code ABNF specification
%%
%%    e-bit            = ", ERR"
%%                       ; If present, the 'E' bit in the Command
%%                       ; Flags is set, indicating that the answer
%%                       ; message contains a Result-Code AVP in
%%                       ; the "protocol error" class.

%% 7.1.3.  Protocol Errors
%%
%%    Errors that fall within the Protocol Error category SHOULD be treated
%%    on a per-hop basis, and Diameter proxies MAY attempt to correct the
%%    error, if it is possible.  Note that these and only these errors MUST
%%    only be used in answer messages whose 'E' bit is set.

%% Thus, only construct answers to protocol errors. Other errors
%% require an message-specific answer and must be handled by the
%% application.

%% 6.2.  Diameter Answer Processing
%%
%%    When a request is locally processed, the following procedures MUST be
%%    applied to create the associated answer, in addition to any
%%    additional procedures that MAY be discussed in the Diameter
%%    application defining the command:
%%
%%    -  The same Hop-by-Hop identifier in the request is used in the
%%       answer.
%%
%%    -  The local host's identity is encoded in the Origin-Host AVP.
%%
%%    -  The Destination-Host and Destination-Realm AVPs MUST NOT be
%%       present in the answer message.
%%
%%    -  The Result-Code AVP is added with its value indicating success or
%%       failure.
%%
%%    -  If the Session-Id is present in the request, it MUST be included
%%       in the answer.
%%
%%    -  Any Proxy-Info AVPs in the request MUST be added to the answer
%%       message, in the same order they were present in the request.
%%
%%    -  The 'P' bit is set to the same value as the one in the request.
%%
%%    -  The same End-to-End identifier in the request is used in the
%%       answer.
%%
%%    Note that the error messages (see Section 7.3) are also subjected to
%%    the above processing rules.

%% 7.3.  Error-Message AVP
%%
%%    The Error-Message AVP (AVP Code 281) is of type UTF8String.  It MAY
%%    accompany a Result-Code AVP as a human readable error message.  The
%%    Error-Message AVP is not intended to be useful in real-time, and
%%    SHOULD NOT be expected to be parsed by network entities.

%% answer_message/4

answer_message(RC,
               Dict0,
               #diameter_caps{origin_host  = {OH,_},
                              origin_realm = {OR,_}},
               #diameter_packet{avps = Avps,
                                errors = Es}) ->
    ['answer-message', {'Origin-Host', OH},
                       {'Origin-Realm', OR},
                       {'Result-Code', RC}
     | session_id(Dict0, Avps)
       ++ failed_avp(RC, Es)
       ++ proxy_info(Dict0, Avps)].

session_id(Dict0, Avps) ->
    {Code, _, Vid} = Dict0:avp_header('Session-Id'),
    try
        #diameter_avp{data = Bin} = find_avp(Code, Vid, Avps),
        [{'Session-Id', [Bin]}]
    catch
        error: _ ->
            []
    end.

%% Note that this should only match 5xxx result codes currently but
%% don't bother distinguishing this case.
failed_avp(RC, [{RC, Avp} | _]) ->
    [{'Failed-AVP', [{'AVP', [Avp]}]}];
failed_avp(RC, [_ | Es]) ->
    failed_avp(RC, Es);
failed_avp(_, [] = No) ->
    No.

proxy_info(Dict0, Avps) ->
    {Code, _, Vid} = Dict0:avp_header('Proxy-Info'),
    [{'AVP', [A#diameter_avp{value = undefined}
              || [#diameter_avp{code = C, vendor_id = I} = A | _]
                     <- Avps,
                 C == Code,
                 I == Vid]}].

%% find_avp/3

%% Grouped ...
find_avp(Code, VId, [[#diameter_avp{code = Code, vendor_id = VId} | _] = As
                     | _]) ->
    As;

%% ... or not.
find_avp(Code, VId, [#diameter_avp{code = Code, vendor_id = VId} = A | _]) ->
    A;

find_avp(Code, VId, [_ | Avps]) ->
    find_avp(Code, VId, Avps).

%% 7.  Error Handling
%%
%%    There are certain Result-Code AVP application errors that require
%%    additional AVPs to be present in the answer.  In these cases, the
%%    Diameter node that sets the Result-Code AVP to indicate the error
%%    MUST add the AVPs.  Examples are:
%%
%%    -  An unrecognized AVP is received with the 'M' bit (Mandatory bit)
%%       set, causes an answer to be sent with the Result-Code AVP set to
%%       DIAMETER_AVP_UNSUPPORTED, and the Failed-AVP AVP containing the
%%       offending AVP.
%%
%%    -  An AVP that is received with an unrecognized value causes an
%%       answer to be returned with the Result-Code AVP set to
%%       DIAMETER_INVALID_AVP_VALUE, with the Failed-AVP AVP containing the
%%       AVP causing the error.
%%
%%    -  A command is received with an AVP that is omitted, yet is
%%       mandatory according to the command's ABNF.  The receiver issues an
%%       answer with the Result-Code set to DIAMETER_MISSING_AVP, and
%%       creates an AVP with the AVP Code and other fields set as expected
%%       in the missing AVP.  The created AVP is then added to the Failed-
%%       AVP AVP.
%%
%%    The Result-Code AVP describes the error that the Diameter node
%%    encountered in its processing.  In case there are multiple errors,
%%    the Diameter node MUST report only the first error it encountered
%%    (detected possibly in some implementation dependent order).  The
%%    specific errors that can be described by this AVP are described in
%%    the following section.

%% 7.5.  Failed-AVP AVP
%%
%%    The Failed-AVP AVP (AVP Code 279) is of type Grouped and provides
%%    debugging information in cases where a request is rejected or not
%%    fully processed due to erroneous information in a specific AVP.  The
%%    value of the Result-Code AVP will provide information on the reason
%%    for the Failed-AVP AVP.
%%
%%    The possible reasons for this AVP are the presence of an improperly
%%    constructed AVP, an unsupported or unrecognized AVP, an invalid AVP
%%    value, the omission of a required AVP, the presence of an explicitly
%%    excluded AVP (see tables in Section 10), or the presence of two or
%%    more occurrences of an AVP which is restricted to 0, 1, or 0-1
%%    occurrences.
%%
%%    A Diameter message MAY contain one Failed-AVP AVP, containing the
%%    entire AVP that could not be processed successfully.  If the failure
%%    reason is omission of a required AVP, an AVP with the missing AVP
%%    code, the missing vendor id, and a zero filled payload of the minimum
%%    required length for the omitted AVP will be added.

%% incr_result/5
%%
%% Increment a stats counter for result codes in incoming and outgoing
%% answers.

%% Message sent as a header/avps list.
incr_result(send = Dir,
            Avp,
            #diameter_packet{msg = [#diameter_header{} = H | _]},
            TPid,
            AppDict) ->
    incr_result(Dir, Avp, H, [], TPid, AppDict);

%% Incoming or outgoing. Outgoing with encode errors never gets here
%% since encode fails.
incr_result(Dir, Avp, Pkt, TPid, AppDict) ->
    #diameter_packet{header = H, errors = Es}
        = Pkt,
    incr_result(Dir, Avp, H, Es, TPid, AppDict).

%% incr_result/6

incr_result(Dir, Avp, Hdr, Es, TPid, AppDict) ->
    Id = msg_id(Hdr, AppDict),
    %% Could be {relay, 0}, in which case the R-bit is redundant since
    %% only answers are being counted. Let it be however, so that the
    %% same tuple is in both send/recv and result code counters.

    %% Count incoming decode errors.
    send == Dir orelse [] == Es orelse incr_error(Dir, Id, TPid, AppDict),

    Ctr = rcc(Avp),
    incr(TPid, {Id, Dir, Ctr}),
    Ctr.

%% msg_id/2

msg_id(#diameter_packet{header = H}, AppDict) ->
    msg_id(H, AppDict);

%% Only count on known keys so as not to be vulnerable to attack:
%% there are 2^32 (application ids) * 2^24 (command codes) = 2^56
%% pairs for an attacker to choose from.
msg_id(Hdr, AppDict) ->
    {Aid, Code, R} = Id = diameter_codec:msg_id(Hdr),
    case AppDict:id() of
        ?APP_ID_RELAY ->
            {relay, R};
        A ->
            unknown(A /= Aid orelse '' == AppDict:msg_name(Code, 0 == R), Id)
    end.

unknown(true, {_, _, R}) ->
    {unknown, R};
unknown(false, Id) ->
    Id.

%% No E-bit: can't be 3xxx.
is_result(RC, false, _Dict0) ->
    RC < 3000 orelse 4000 =< RC;

%% E-bit in RFC 3588: only 3xxx.
is_result(RC, true, ?BASE) ->
    3000 =< RC andalso RC < 4000;

%% E-bit in RFC 6733: 3xxx or 5xxx.
is_result(RC, true, _) ->
    3000 =< RC andalso RC < 4000
        orelse
        5000 =< RC andalso RC < 6000.

%% incr/2

incr(TPid, Counter) ->
    diameter_stats:incr(Counter, TPid, 1).

%% rcc/1

rcc(#diameter_avp{name = 'Result-Code' = Name, value = V}) ->
    {Name, head(V)};

rcc(#diameter_avp{name = 'Experimental-Result', value = V}) ->
    head(V).

%% head/1

head([V|_]) ->
    V;
head(V) ->
    V.

%% rcv/1

rcv(#diameter_avp{name = N, value = V}) ->
    rcv(N, head(V)).

%% rcv/2

rcv('Experimental-Result', {_,_,N}) ->
    N;

rcv('Result-Code', N) ->
    N.

%% get_result/4

%% Message sent as binary: no checks or counting.
get_result(_, _, _, #diameter_packet{header = undefined}) ->
    false;

get_result(Dir, MsgDict, Dict0, Pkt) ->
    Avp = get_result(MsgDict, msg(Dir, Pkt)),
    Hdr = Pkt#diameter_packet.header,
    %% Exit on a missing result code or inappropriate value.
    Avp == false
        andalso ?LOGX(no_result_code, {MsgDict, Dir, Hdr}),
    E = Hdr#diameter_header.is_error,
    is_result(rcv(Avp), E, Dict0)
        orelse ?LOGX(invalid_error_bit, {MsgDict, Dir, Hdr, Avp}),
    Avp.

%% RFC 3588, 7.6:
%%
%%   All Diameter answer messages defined in vendor-specific
%%   applications MUST include either one Result-Code AVP or one
%%   Experimental-Result AVP.

%% msg/2

msg(Dir, #diameter_packet{header = H,
                          avps = As,
                          msg = Msg})
  when Dir == recv;         %% decoded incoming
       Msg == undefined ->  %% relayed outgoing
    [H|As];

msg(_, #diameter_packet{msg = Msg}) ->
    Msg.

%% get_result/2

get_result(Dict, Msg) ->
    try
        [throw(A) || N <- ['Result-Code', 'Experimental-Result'],
                     #diameter_avp{} = A <- [get_avp(Dict, N, Msg)],
                     is_integer(catch rcv(A))],
        false
    catch
        #diameter_avp{} = A ->
            A
    end.

x(T) ->
    exit(T).

%% ---------------------------------------------------------------------------
%% send_request/4
%%
%% Handle an outgoing Diameter request.
%% ---------------------------------------------------------------------------

send_request(SvcName, AppOrAlias, Msg, Options)
  when is_list(Options) ->
    Rec = make_options(Options),
    Ref = make_ref(),
    Caller = {self(), Ref},
    ReqF = fun() ->
                   exit({Ref, send_R(SvcName, AppOrAlias, Msg, Rec, Caller)})
           end,
    try spawn_monitor(ReqF) of
        {_, MRef} ->
            recv_A(MRef, Ref, Rec#options.detach, false)
    catch
        error: system_limit = E ->
            {error, E}
    end.
%% The R in send_R is because Diameter request are usually given short
%% names of the form XXR. (eg. CER, DWR, etc.) Similarly, answers have
%% names of the form XXA.

%% Don't rely on gen_server:call/3 for the timeout handling since it
%% makes no guarantees about not leaving a reply message in the
%% mailbox if we catch its exit at timeout. It currently *can* do so,
%% which is also undocumented.

recv_A(MRef, _, true, true) ->
    erlang:demonitor(MRef, [flush]),
    ok;

recv_A(MRef, Ref, Detach, Sent) ->
    receive
        Ref ->  %% send has been attempted
            recv_A(MRef, Ref, Detach, true);
        {'DOWN', MRef, process, _, Reason} ->
            answer_rc(Reason, Ref, Sent)
    end.

%% send_R/5 has returned ...
answer_rc({Ref, Ans}, Ref, _) ->
    Ans;

%% ... or not. Note that failure/encode are documented return values.
answer_rc(_, _, Sent) ->
    {error, choose(Sent, failure, encode)}.

%% send_R/5
%%
%% In the process spawned for the outgoing request.

send_R(SvcName, AppOrAlias, Msg, CallOpts, Caller) ->
    case pick_peer(SvcName, AppOrAlias, Msg, CallOpts) of
        {{_,_} = Transport, SvcOpts} ->
            send_request(Transport, SvcOpts, Msg, CallOpts, Caller, SvcName);
        {error, _} = No ->
            No
    end.

%% make_options/1

make_options(Options) ->
    make_opts(Options, [], false, [], none, 5000).

%% Do our own recursion since this is faster than a lists:foldl/3
%% setting elements in an #options{} accumulator.

make_opts([], Peers, Detach, Extra, Filter, Tmo) ->
    #options{peers = lists:reverse(Peers),
             detach = Detach,
             extra = Extra,
             filter = Filter,
             timeout = Tmo};

make_opts([{timeout, Tmo} | Rest], Peers, Detach, Extra, Filter, _)
  when is_integer(Tmo), 0 =< Tmo ->
    make_opts(Rest, Peers, Detach, Extra, Filter, Tmo);

make_opts([{filter, F} | Rest], Peers, Detach, Extra, none, Tmo) ->
    make_opts(Rest, Peers, Detach, Extra, F, Tmo);
make_opts([{filter, F} | Rest], Peers, Detach, Extra, {all, Fs}, Tmo) ->
    make_opts(Rest, Peers, Detach, Extra, {all, [F|Fs]}, Tmo);
make_opts([{filter, F} | Rest], Peers, Detach, Extra, F0, Tmo) ->
    make_opts(Rest, Peers, Detach, Extra, {all, [F0, F]}, Tmo);

make_opts([{extra, L} | Rest], Peers, Detach, Extra, Filter, Tmo)
  when is_list(L) ->
    make_opts(Rest, Peers, Detach, Extra ++ L, Filter, Tmo);

make_opts([detach | Rest], Peers, _, Extra, Filter, Tmo) ->
    make_opts(Rest, Peers, true, Extra, Filter, Tmo);

make_opts([{peer, TPid} | Rest], Peers, Detach, Extra, Filter, Tmo)
  when is_pid(TPid) ->
    make_opts(Rest, [TPid | Peers], Detach, Extra, Filter, Tmo);

make_opts([T | _], _, _, _, _, _) ->
    ?ERROR({invalid_option, T}).

%% ---------------------------------------------------------------------------
%% send_request/6
%% ---------------------------------------------------------------------------

%% Send an outgoing request in its dedicated process.
%%
%% Note that both encode of the outgoing request and of the received
%% answer happens in this process. It's also this process that replies
%% to the caller. The service process only handles the state-retaining
%% callbacks.
%%
%% The module field of the #diameter_app{} here includes any extra
%% arguments passed to diameter:call/4.

send_request({{TPid, _Caps} = TC, App}
             = Transport,
             #{sequence := Mask, traffic_counters := Count}
             = SvcOpts,
             Msg0,
             CallOpts,
             Caller,
             SvcName) ->
    Pkt = make_prepare_packet(Mask, Msg0),

    case prepare(cb(App, prepare_request, [Pkt, SvcName, TC]), []) of
        [Msg | Fs] ->
            ReqPkt = make_request_packet(Msg, Pkt),
            EncPkt = encode(App#diameter_app.dictionary,
                            TPid,
                            SvcOpts,
                            ReqPkt),
            eval_packet(EncPkt, Fs),
            T = send_R(ReqPkt,
                       EncPkt,
                       Transport,
                       CallOpts,
                       Caller,
                       Count,
                       SvcName),
            Ans = recv_answer(SvcName, App, CallOpts, T),
            handle_answer(SvcName, Count, SvcOpts, App, Ans);
        {discard, Reason} ->
            {error, Reason};
        discard ->
            {error, discarded};
        {error, Reason} ->
            ?ERROR({invalid_return, Reason, prepare_request, App})
    end.

%% prepare/2

prepare({send, Msg}, Fs) ->
    [Msg | Fs];

prepare({eval_packet, RC, F}, Fs) ->
    prepare(RC, [F|Fs]);

prepare({discard, _Reason} = RC, _) ->
    RC;

prepare(discard = RC, _) ->
    RC;

prepare(Reason, _) ->
    {error, Reason}.

%% make_prepare_packet/2
%%
%% Turn an outgoing request as passed to call/4 into a diameter_packet
%% record in preparation for a prepare_request callback.

make_prepare_packet(_, Bin)
  when is_binary(Bin) ->
    #diameter_packet{header = diameter_codec:decode_header(Bin),
                     bin = Bin};

make_prepare_packet(Mask, #diameter_packet{msg = [#diameter_header{} = Hdr
                                                  | Avps]}
                          = Pkt) ->
    Pkt#diameter_packet{msg = [make_prepare_header(Mask, Hdr) | Avps]};

make_prepare_packet(Mask, #diameter_packet{header = Hdr} = Pkt) ->
    Pkt#diameter_packet{header = make_prepare_header(Mask, Hdr)};

make_prepare_packet(Mask, [#diameter_header{} = Hdr | Avps]) ->
    #diameter_packet{msg = [make_prepare_header(Mask, Hdr) | Avps]};

make_prepare_packet(Mask, Msg) ->
    #diameter_packet{header = make_prepare_header(Mask, undefined),
                     msg = Msg}.

%% make_prepare_header/2

make_prepare_header(Mask, undefined) ->
    Seq = diameter_session:sequence(Mask),
    #diameter_header{version = ?DIAMETER_VERSION,
                     end_to_end_id = Seq,
                     hop_by_hop_id = Seq};

make_prepare_header(Mask, #diameter_header{version = V,
                                           end_to_end_id = EI,
                                           hop_by_hop_id = HI}
                          = H)
  when EI == undefined;
       HI == undefined ->
    Id = diameter_session:sequence(Mask),
    H#diameter_header{version = ?DEFAULT(V, ?DIAMETER_VERSION),
                      end_to_end_id = ?DEFAULT(EI, Id),
                      hop_by_hop_id = ?DEFAULT(HI, Id)};

make_prepare_header(_, #diameter_header{version = undefined} = H) ->
    H#diameter_header{version = ?DIAMETER_VERSION};

make_prepare_header(_, #diameter_header{} = H) ->
    H;

make_prepare_header(_, T) ->
    ?ERROR({invalid_header, T}).

%% make_request_packet/2
%%
%% Reconstruct a diameter_packet from the return value of
%% prepare_request or prepare_retransmit callback.

make_request_packet(Bin, _)
  when is_binary(Bin) ->
    make_prepare_packet(false, Bin);

make_request_packet(#diameter_packet{msg = [#diameter_header{} | _]}
                    = Pkt,
                    _) ->
    Pkt;

%% Returning a diameter_packet with no header from a prepare_request
%% or prepare_retransmit callback retains the header passed into it.
%% This is primarily so that the end to end and hop by hop identifiers
%% are retained.
make_request_packet(#diameter_packet{header = Hdr} = Pkt,
                    #diameter_packet{header = Hdr0}) ->
    Pkt#diameter_packet{header = fold_record(Hdr0, Hdr)};

make_request_packet(Msg, Pkt) ->
    Pkt#diameter_packet{msg = Msg}.

%% make_retransmit_packet/1

make_retransmit_packet(#diameter_packet{msg = [#diameter_header{} = Hdr
                                               | Avps]}
                       = Pkt) ->
    Pkt#diameter_packet{msg = [make_retransmit_header(Hdr) | Avps]};

make_retransmit_packet(#diameter_packet{header = Hdr} = Pkt) ->
    Pkt#diameter_packet{header = make_retransmit_header(Hdr)}.

%% make_retransmit_header/1

make_retransmit_header(Hdr) ->
    Hdr#diameter_header{is_retransmitted = true}.

%% fold_record/2
%%
%% Replace elements in the first record by those in the second that
%% differ from undefined.

fold_record(Rec0, undefined) ->
    Rec0;
fold_record(Rec0, Rec) ->
    list_to_tuple(fold(tuple_to_list(Rec0), tuple_to_list(Rec))).

fold([], []) ->
    [];
fold([H | T0], [undefined | T]) ->
    [H | fold(T0, T)];
fold([_ | T0], [H | T]) ->
    [H | fold(T0, T)].

%% send_R/6

send_R(ReqPkt,
       EncPkt,
       {{TPid, _Caps} = TC, #diameter_app{dictionary = AppDict}},
       #options{timeout = Timeout},
       {Pid, Ref},
       Count,
       SvcName) ->
    Req = #request{ref = Ref,
                   caller = Pid,
                   handler = self(),
                   peer = TC,
                   packet = ReqPkt},

    Count andalso incr(send, EncPkt, TPid, AppDict),
    {TRef, MRef} = zend_requezt(TPid, EncPkt, Req, SvcName, Timeout),
    Pid ! Ref,  %% tell caller a send has been attempted
    {TRef, MRef, Req}.

%% recv_answer/4

recv_answer(SvcName, App, CallOpts, {TRef, MRef, #request{ref = Ref}
                                                 = Req}) ->
    %% Matching on TRef below ensures we ignore messages that pertain
    %% to a previous transport prior to failover. The answer message
    %% includes the pid of the transport on which it was received,
    %% which may not be the last peer to which we've transmitted.
    receive
        {answer = A, Ref, TPid, Dict0, Pkt} ->  %% Answer from peer
            {A, #request{} = erase(TPid), Dict0, Pkt};
        {timeout = Reason, TRef, _} ->        %% No timely reply
            {error, Req, Reason};
        {'DOWN', MRef, process, _, _} when false /= MRef -> %% local peer_down
            failover(SvcName, App, Req, CallOpts);
        {failover, TRef} ->                   %% local or remote peer_down
            failover(SvcName, App, Req, CallOpts)
    end.

%% failover/4

failover(SvcName, App, Req, CallOpts) ->
    resend_request(pick_peer(SvcName, App, Req, CallOpts),
                   Req,
                   CallOpts,
                   SvcName).

%% handle_answer/5

handle_answer(SvcName, _, _, App, {error, Req, Reason}) ->
    #request{packet = Pkt,
             peer = {_TPid, _Caps} = TC}
        = Req,
    cb(App, handle_error, [Reason, msg(Pkt), SvcName, TC]);

handle_answer(SvcName,
              Count,
              SvcOpts,
              #diameter_app{id = Id,
                            dictionary = AppDict,
                            options = [{answer_errors, AE} | _]}
              = App,
              {answer, Req, Dict0, Pkt}) ->
    MsgDict = msg_dict(AppDict, Dict0, Pkt),
    DecPkt = errors(Id, diameter_codec:decode({MsgDict, AppDict},
                                              SvcOpts,
                                              Pkt)),
    #request{peer = {TPid, _}}
        = Req,

    answer(answer(DecPkt, TPid, MsgDict, AppDict, Dict0, Count),
           SvcName,
           App,
           AE,
           Req).

%% answer/6

answer(DecPkt, TPid, MsgDict, AppDict, Dict0, Count) ->
    Count andalso incr(recv, DecPkt, TPid, AppDict),
    try get_result(recv, MsgDict, Dict0, DecPkt) of
        Avp ->
            Count andalso false /= Avp
                  andalso incr_result(recv, Avp, DecPkt, TPid, AppDict),
            DecPkt
    catch
        exit: {no_result_code, _} ->
            %% RFC 6733 requires one of Result-Code or
            %% Experimental-Result, but the decode will have
            %% detected a missing AVP. If both are optional in
            %% the dictionary then this isn't a decode error:
            %% just continue on.
            DecPkt;
        exit: {invalid_error_bit, {_, _, _, Avp}} ->
            #diameter_packet{errors = Es}
                = DecPkt,
            E = {5004, Avp},
            DecPkt#diameter_packet{errors = [E|Es]}
    end.

%% answer/5

answer(#diameter_packet{errors = Es}
       = Pkt,
       SvcName,
       App,
       AE,
       #request{peer = {_TPid, _Caps} = TC,
                packet = P})
  when callback == AE;
       [] == Es ->
    cb(App, handle_answer, [Pkt, msg(P), SvcName, TC]);

answer(#diameter_packet{header = H}, SvcName, _, AE, _) ->
    handle_error(H, SvcName, AE).

%% handle_error/3

-spec handle_error(_, _, _) -> no_return().  %% silence dialyzer

handle_error(Hdr, SvcName, report) ->
    MFA = {?MODULE, handle_answer, [SvcName, Hdr]},
    diameter_lib:warning_report(errors, MFA),
    handle_error(Hdr, SvcName, discard);

handle_error(Hdr, SvcName, discard) ->
    x({answer_errors, {SvcName, Hdr}}).

%% Note that we don't check that the application id in the answer's
%% header is what we expect. (TODO: Does the rfc says anything about
%% this?)

%% Note that failover starts a new timer and that expiry of an old
%% timer value is ignored. This means that an answer could be accepted
%% from a peer after timeout in the case of failover.

%% resend_request/4

resend_request({{{TPid, _Caps} = TC, App}, SvcOpts},
               Req0,
               #options{timeout = Timeout}
               = CallOpts,
               SvcName) ->
    case
        undefined == get(TPid)
        andalso prepare_retransmit(TC, App, Req0, SvcName)
    of
        [ReqPkt | Fs] ->
            AppDict = App#diameter_app.dictionary,
            EncPkt = encode(AppDict, TPid, SvcOpts, ReqPkt),
            eval_packet(EncPkt, Fs),
            Req = Req0#request{peer = TC,
                               packet = ReqPkt},
            ?LOG(retransmission, EncPkt#diameter_packet.header),
            incr(TPid, {msg_id(EncPkt, AppDict), send, retransmission}),
            {TRef, MRef} = zend_requezt(TPid, EncPkt, Req, SvcName, Timeout),
            recv_answer(SvcName, App, CallOpts, {TRef, MRef, Req});
        false ->
            {error, Req0, timeout};
        {discard, Reason} ->
            {error, Req0, Reason};
        discard ->
            {error, Req0, discarded};
        {error, T} ->
            ?ERROR({invalid_return, T, prepare_retransmit, App})
    end;

resend_request(_, Req, _, _) ->  %% no alternate peer
    {error, Req, failover}.

%% pick_peer/4

%% Retransmission after failover: call-specific arguments have already
%% been appended in App.
pick_peer(SvcName,
          App,
          #request{packet = #diameter_packet{msg = Msg}},
          CallOpts) ->
    pick_peer(SvcName, App, Msg, CallOpts#options{extra = []});

pick_peer(_, _, undefined, _) ->
    {error, no_connection};

pick_peer(SvcName,
          AppOrAlias,
          Msg,
          #options{peers = TPids, filter = Filter, extra = Xtra}) ->
    X = {fun(D) -> get_destination(D, Msg) end, Filter, Xtra, TPids},
    case diameter_service:pick_peer(SvcName, AppOrAlias, X) of
        false ->
            {error, no_connection};
        T ->
            T
    end.

msg(#diameter_packet{msg = undefined, bin = Bin}) ->
    Bin;
msg(#diameter_packet{msg = Msg}) ->
    Msg.

%% encode/4

%% Note that prepare_request can return a diameter_packet containing a
%% header or transport_data. Even allow the returned record to contain
%% an encoded binary. This isn't the usual case and doesn't properly
%% support retransmission but is useful for test.

encode(Dict, TPid, Opts, Pkt)
  when is_atom(Dict) ->
    encode({Dict, Dict}, TPid, Opts, Pkt);

%% A message to be encoded.
encode(DictT, TPid, Opts, #diameter_packet{bin = undefined} = Pkt) ->
    {Dict, AppDict} = DictT,
    try
        diameter_codec:encode(Dict, Opts, Pkt)
    catch
        exit: {diameter_codec, encode, T} = Reason ->
            incr_error(send, T, TPid, AppDict),
            exit(Reason)
    end;

%% An encoded binary: just send.
encode(_, _, _, #diameter_packet{} = Pkt) ->
    Pkt.

%% zend_requezt/5
%%
%% Strip potentially large record fields that aren't used by the
%% processes the records can be send to, possibly on a remote node.

zend_requezt(TPid, Pkt, Req, SvcName, Timeout) ->
    put(TPid, Req),
    send_request(TPid, z(Pkt), Req, SvcName, Timeout).

%% send_request/5

send_request(TPid, #diameter_packet{bin = Bin} = Pkt, Req, _SvcName, Timeout)
  when node() == node(TPid) ->
    Seqs = diameter_codec:sequence_numbers(Bin),
    TRef = erlang:start_timer(Timeout, self(), TPid),
    send(TPid, Pkt, _Route = {self(), Req#request.ref, Seqs}),
    {TRef, _MRef = peer_monitor(TPid, TRef)};

%% Send using a remote transport: spawn a process on the remote node
%% to relay the answer.
send_request(TPid, #diameter_packet{} = Pkt, Req, SvcName, Timeout) ->
    TRef = erlang:start_timer(Timeout, self(), TPid),
    T = {TPid, Pkt, z(Req), SvcName, Timeout, TRef},
    spawn(node(TPid), ?MODULE, send, [T]),
    {TRef, false}.

%% z/1
%%
%% Avoid sending potentially large terms unnecessarily. The records
%% themselves are retained since they're sent between nodes in send/1
%% and changing what's sent causes upgrade issues.

z(#request{ref = Ref, handler = Pid}) ->
    #request{ref = Ref,
             handler = Pid};

z(#diameter_packet{header = H, bin = Bin, transport_data = T}) ->
    #diameter_packet{header = H,
                     bin = Bin,
                     transport_data = T}.

%% send/1

send({TPid, Pkt, #request{handler = Pid} = Req0, SvcName, Timeout, TRef}) ->
    Req = Req0#request{handler = self()},
    recv(TPid, Pid, TRef, zend_requezt(TPid, Pkt, Req, SvcName, Timeout)).

%% recv/4
%%
%% Relay an answer from a remote node.

recv(TPid, Pid, TRef, {LocalTRef, MRef}) ->
    receive
        {answer, _, _, _, _} = A ->
            Pid ! A;
        {'DOWN', MRef, process, _, _} ->
            Pid ! {failover, TRef};
        {failover = T, LocalTRef} ->
            Pid ! {T, TRef};
        T ->
            exit({timeout, LocalTRef, TPid} = T)
    end.

%% send/3

send(Pid, Pkt, Route) ->
    Pid ! {send, Pkt, Route}.

%% prepare_retransmit/4

prepare_retransmit({_TPid, _Caps} = TC, App, Req, SvcName) ->
    Pkt = make_retransmit_packet(Req#request.packet),

    case prepare(cb(App, prepare_retransmit, [Pkt, SvcName, TC]), []) of
        [Msg | Fs] ->
            [make_request_packet(Msg, Pkt) | Fs];
        No ->
            No
    end.

%% When sending a binary, it's up to prepare_retransmit to modify it
%% accordingly.

%% peer_monitor/2

peer_monitor(TPid, TRef) ->
    case ets:lookup(?REQUEST_TABLE, TPid) of %% at peer_up/1
        [{_, MPid}] ->
            monitor(process, MPid);
        [] ->  %% transport has gone down
            self() ! {failover, TRef},
            false
    end.

%% get_destination/2

get_destination(Dict, Msg) ->
    [str(get_avp_value(Dict, D, Msg)) || D <- ['Destination-Realm',
                                               'Destination-Host']].

%% A DiameterIdentity has length at least one, so an empty list is not
%% a Realm/Host.
str([]) ->
    undefined;
str(T) ->
    T.

%% get_avp/3
%%
%% Find an AVP in a message in one of the decoded formats, or as a
%% header/avps list. There are only four AVPs that are extracted here:
%% Result-Code and Experimental-Result in order when constructing
%% counter keys, and Destination-Host/Realm when selecting a next-hop
%% peer. Experimental-Result is the only of type Grouped, and is given
%% special treatment in order to return the value as a record.

%% Messages will be header/avps list as a relay and the only AVP's we
%% look for are in the common dictionary. This is required since the
%% relay dictionary doesn't inherit the common dictionary (which maybe
%% it should).
get_avp(?RELAY, Name, Msg) ->
    get_avp(?BASE, Name, Msg);

%% Message as header/avps list.
get_avp(Dict, Name, [#diameter_header{} | Avps]) ->
    try
        {Code, _, Vid} = Dict:avp_header(Name),
        A = find_avp(Code, Vid, Avps),
        avp_decode(Dict, Name, ungroup(A))
    catch
        {diameter_gen, _} ->  %% faulty Grouped AVP
            undefined;
        error: _ ->
            undefined
    end;

%% Message as name/values list ...
get_avp(_, Name, [_MsgName | Avps]) ->
    case find(Name, Avps) of
        {_, V} ->
            #diameter_avp{name = Name, value = value(Name, V)};
        _ ->
            undefined
    end;

%% ... or record.
get_avp(Dict, Name, Rec) ->
    try Dict:'#get-'(Name, Rec) of
        V ->
            #diameter_avp{name = Name, value = value(Name, V)}
    catch
        error:_ ->
            undefined
    end.

value('Experimental-Result' = N, #{'Vendor-Id' := Vid,
                                   'Experimental-Result-Code' := RC}) ->
    {N, Vid, RC};
value('Experimental-Result' = N, [{'Experimental-Result-Code', RC},
                                  {'Vendor-Id', Vid}]) ->
    {N, Vid, RC};
value('Experimental-Result' = N, [{'Vendor-Id', Vid},
                                  {'Experimental-Result-Code', RC}]) ->
    {N, Vid, RC};
value(_, V) ->
    V.

%% find/2

find(Key, Map)
  when is_map(Map) ->
    maps:find(Key, Map);

find(Key, List)
  when is_list(List) ->
    lists:keyfind(Key, 1, List).

%% get_avp_value/3

get_avp_value(Dict, Name, Msg) ->
    case get_avp(Dict, Name, Msg) of
        #diameter_avp{value = V} ->
            V;
        undefined = No ->
            No
    end.

%% ungroup/1

ungroup([Avp|_]) ->
    Avp;
ungroup(Avp) ->
    Avp.

%% avp_decode/3

%% Ensure Experimental-Result is decoded as record, since this format
%% is used for counter keys.
avp_decode(Dict, 'Experimental-Result' = N, #diameter_avp{data = Bin}
                                            = Avp)
  when is_binary(Bin) ->
    {V,_} = Dict:avp(decode, Bin, N, decode_opts(Dict)),
    Avp#diameter_avp{name = N, value = V};

avp_decode(Dict, Name, #diameter_avp{value = undefined,
                                     data = Bin}
                       = Avp)
  when is_binary(Bin) ->
    V = Dict:avp(decode, Bin, Name, decode_opts(Dict)),
    Avp#diameter_avp{name = Name, value = V};

avp_decode(_, Name, #diameter_avp{} = Avp) ->
    Avp#diameter_avp{name = Name}.

%% cb/3

cb(#diameter_app{module = [_|_] = M}, F, A) ->
    eval(M, F, A).

eval([M|X], F, A) ->
    apply(M, F, A ++ X).

choose(true, X, _)  -> X;
choose(false, _, X) -> X.

%% Decode options sufficient for AVP extraction.
decode_opts(Dict) ->
    #{decode_format => record,
      string_decode => false,
      strict_mbit => false,
      failed_avp => false,
      module => Dict,
      app_dictionary => Dict}.
