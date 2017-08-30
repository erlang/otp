%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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
-export([receive_message/4]).

%% towards diameter_peer_fsm and diameter_watchdog
-export([incr/4,
         incr_error/4,
         incr_rc/4]).

%% towards diameter_service
-export([make_recvdata/1,
         peer_up/1,
         peer_down/1,
         pending/1]).

%% towards ?MODULE
-export([send/1]).  %% send from remote node

-include_lib("diameter/include/diameter.hrl").
-include("diameter_internal.hrl").

-define(LOGX(Reason, T), begin ?LOG(Reason, T), x({Reason, T}) end).

-define(RELAY, ?DIAMETER_DICT_RELAY).
-define(BASE,  ?DIAMETER_DICT_COMMON).  %% Note: the RFC 3588 dictionary

-define(DEFAULT_TIMEOUT, 5000).  %% for outgoing requests
-define(DEFAULT_SPAWN_OPTS, []).

%% Table containing outgoing requests for which a reply has yet to be
%% received.
-define(REQUEST_TABLE, diameter_request).

%% Workaround for dialyzer's lack of understanding of match specs.
-type match(T)
   :: T | '_' | '$1' | '$2' | '$3' | '$4'.

%% Record diameter:call/4 options are parsed into.
-record(options,
        {filter = none  :: diameter:peer_filter(),
         extra = []     :: list(),
         timeout = ?DEFAULT_TIMEOUT :: 0..16#FFFFFFFF,
         detach = false :: boolean()}).

%% Term passed back to receive_message/4 with every incoming message.
-record(recvdata,
        {peerT        :: ets:tid(),
         service_name :: diameter:service_name(),
         apps         :: [#diameter_app{}],
         sequence     :: diameter:sequence(),
         codec        :: [{string_decode, boolean()}
                          | {strict_mbit, boolean()}
                          | {incoming_maxlen, diameter:message_length()}]}).
%% Note that incoming_maxlen is currently handled in diameter_peer_fsm,
%% so that any message exceeding the maximum is discarded. Retain the
%% option in case we want to extend the values and semantics.

%% Record stored in diameter_request for each outgoing request.
-record(request,
        {ref        :: match(reference()),  %% used to receive answer
         caller     :: match(pid()),        %% calling process
         handler    :: match(pid()),        %% request process
         transport  :: match(pid()),        %% peer process
         caps       :: match(#diameter_caps{}),     %% of connection
         packet     :: match(#diameter_packet{})}). %% of request

%% ---------------------------------------------------------------------------
%% # make_recvdata/1
%% ---------------------------------------------------------------------------

make_recvdata([SvcName, PeerT, Apps, SvcOpts | _]) ->
    {_,_} = Mask = proplists:get_value(sequence, SvcOpts),
    #recvdata{service_name = SvcName,
              peerT = PeerT,
              apps = Apps,
              sequence = Mask,
              codec = [T || {K,_} = T <- SvcOpts,
                            lists:member(K, [string_decode,
                                             incoming_maxlen,
                                             strict_mbit])]}.

%% ---------------------------------------------------------------------------
%% peer_up/1
%% ---------------------------------------------------------------------------

%% Insert an element that is used to detect whether or not there has
%% been a failover when inserting an outgoing request.
peer_up(TPid) ->
    ets:insert(?REQUEST_TABLE, {TPid}).

%% ---------------------------------------------------------------------------
%% peer_down/1
%% ---------------------------------------------------------------------------

peer_down(TPid) ->
    ets:delete(?REQUEST_TABLE, TPid),
    failover(TPid).

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
   -> {Counter, non_neg_integer()}
    | Reason
 when Pkt :: #diameter_packet{},
      TPid :: pid(),
      DictT :: module() | {MsgDict :: module(),
                           AppDict :: module(),
                           CommonDict:: module()},
      Counter :: {'Result-Code', integer()}
               | {'Experimental-Result', integer(), integer()},
      Reason :: atom().

incr_rc(Dir, Pkt, TPid, {_, AppDict, _} = DictT) ->
    try
        incr_result(Dir, Pkt, TPid, DictT)
    catch
        exit: {E,_} when E == no_result_code;
                         E == invalid_error_bit ->
            incr(TPid, {msg_id(Pkt#diameter_packet.header, AppDict), Dir, E}),
            E
    end;

incr_rc(Dir, Pkt, TPid, Dict0) ->
    incr_rc(Dir, Pkt, TPid, {Dict0, Dict0, Dict0}).

%% ---------------------------------------------------------------------------
%% pending/1
%% ---------------------------------------------------------------------------

pending(TPids) ->
    MatchSpec = [{{'$1',
                   #request{caller = '$2',
                            handler = '$3',
                            transport = '$4',
                            _ = '_'},
                   '_'},
                  [?ORCOND([{'==', T, '$4'} || T <- TPids])],
                  [{{'$1', [{{caller, '$2'}},
                            {{handler, '$3'}},
                            {{transport, '$4'}}]}}]}],

    try
        ets:select(?REQUEST_TABLE, MatchSpec)
    catch
        error: badarg -> []  %% service has gone down
    end.

%% ---------------------------------------------------------------------------
%% # receive_message/4
%%
%% Handle an incoming Diameter message.
%% ---------------------------------------------------------------------------

%% Handle an incoming Diameter message in the watchdog process. This
%% used to come through the service process but this avoids that
%% becoming a bottleneck.

receive_message(TPid, {Pkt, NPid}, Dict0, RecvData) ->
    NPid ! {diameter, incoming(TPid, Pkt, Dict0, RecvData)};

receive_message(TPid, Pkt, Dict0, RecvData) ->
    incoming(TPid, Pkt, Dict0, RecvData).

%% incoming/4

incoming(TPid, Pkt, Dict0, RecvData)
  when is_pid(TPid) ->
    #diameter_packet{header = #diameter_header{is_request = R}} = Pkt,
    recv(R,
         (not R) andalso lookup_request(Pkt, TPid),
         TPid,
         Pkt,
         Dict0,
         RecvData).

%% recv/6

%% Incoming request ...
recv(true, false, TPid, Pkt, Dict0, T) ->
    try
        {request, spawn_request(TPid, Pkt, Dict0, T)}
    catch
        error: system_limit = E ->  %% discard
            ?LOG(error, E),
            discard
    end;

%% ... answer to known request ...
recv(false, #request{ref = Ref, handler = Pid} = Req, _, Pkt, Dict0, _) ->
    Pid ! {answer, Ref, Req, Dict0, Pkt},
    {answer, Pid};

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
    discard.

%% spawn_request/4

spawn_request(TPid, Pkt, Dict0, {Opts, RecvData}) ->
    spawn_request(TPid, Pkt, Dict0, Opts, RecvData);
spawn_request(TPid, Pkt, Dict0, RecvData) ->
    spawn_request(TPid, Pkt, Dict0, ?DEFAULT_SPAWN_OPTS, RecvData).

spawn_request(TPid, Pkt, Dict0, Opts, RecvData) ->
    spawn_opt(fun() -> recv_request(TPid, Pkt, Dict0, RecvData) end, Opts).

%% ---------------------------------------------------------------------------
%% recv_request/4
%% ---------------------------------------------------------------------------

recv_request(TPid,
             #diameter_packet{header = #diameter_header{application_id = Id}}
             = Pkt,
             Dict0,
             #recvdata{peerT = PeerT,
                       apps = Apps,
                       codec = Opts}
             = RecvData) ->
    diameter_codec:setopts([{common_dictionary, Dict0} | Opts]),
    send_A(recv_R(diameter_service:find_incoming_app(PeerT, TPid, Id, Apps),
                  TPid,
                  Pkt,
                  Dict0,
                  RecvData),
           TPid,
           Dict0,
           RecvData).

%% recv_R/5

recv_R({#diameter_app{id = Id, dictionary = AppDict} = App, Caps},
       TPid,
       Pkt0,
       Dict0,
       RecvData) ->
    incr(recv, Pkt0, TPid, AppDict),
    Pkt = errors(Id, diameter_codec:decode(Id, AppDict, Pkt0)),
    incr_error(recv, Pkt, TPid, AppDict),
    {Caps, Pkt, App, recv_R(App, TPid, Dict0, Caps, RecvData, Pkt)};
%% Note that the decode is different depending on whether or not Id is
%% ?APP_ID_RELAY.

%%   DIAMETER_APPLICATION_UNSUPPORTED   3007
%%      A request was sent for an application that is not supported.

recv_R(#diameter_caps{}
       = Caps,
       _TPid,
       #diameter_packet{errors = Es}
       = Pkt,
       _Dict0,
       _RecvData) ->
    {Caps, Pkt#diameter_packet{avps = collect_avps(Pkt),
                               errors = [3007 | Es]}};

recv_R(false = No, _, _, _, _) ->  %% transport has gone down
    No.

collect_avps(Pkt) ->
    case diameter_codec:collect_avps(Pkt) of
        {_Error, Avps} ->
            Avps;
        Avps ->
            Avps
    end.

%% recv_R/6

%% Answer errors ourselves ...
recv_R(#diameter_app{options = [_, {request_errors, E} | _]},
       _TPid,
       Dict0,
       _Caps,
       _RecvData,
       #diameter_packet{errors = [RC|_]})  %% a detected 3xxx is hd
  when E == answer, (Dict0 /= ?BASE orelse 3 == RC div 1000);
       E == answer_3xxx, 3 == RC div 1000 ->
    {{answer_message, rc(RC)}, [], []};

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
    {T, EvalPktFs, EvalFs};

%% An 3xxx result code, for which the E-bit is set in the header.
request_cb({protocol_error, RC}, _App, EvalPktFs, EvalFs)
  when 3 == RC div 1000 ->
    {{answer_message, RC}, EvalPktFs, EvalFs};

request_cb({answer_message, RC} = T, _App, EvalPktFs, EvalFs)
  when 3 == RC div 1000;
       5 == RC div 1000 ->
    {T, EvalPktFs, EvalFs};

%% RFC 3588 says we must reply 3001 to anything unrecognized or
%% unsupported. 'noreply' is undocumented (and inappropriately named)
%% backwards compatibility for this, protocol_error the documented
%% alternative.
request_cb(noreply, _App, EvalPktFs, EvalFs) ->
    {{answer_message, 3001}, EvalPktFs, EvalFs};

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
    {{call, Opts}, EvalPktFs, EvalFs};

request_cb(discard = No, _, _, _) ->
    No;

request_cb({eval_packet, RC, F}, App, Fs, EvalFs) ->
    request_cb(RC, App, [F|Fs], EvalFs);

request_cb({eval, RC, F}, App, EvalPktFs, Fs) ->
    request_cb(RC, App, EvalPktFs, [F|Fs]);

request_cb(T, App, _, _) ->
    ?ERROR({invalid_return, T, handle_request, App}).

%% send_A/4

send_A({Caps, Pkt}, TPid, Dict0, _RecvData) ->  %% unsupported application
    #diameter_packet{errors = [RC|_]} = Pkt,
    send_A(answer_message(RC, Caps, Dict0, Pkt),
           TPid,
           {Dict0, Dict0},
           Pkt,
           [],
           []);

send_A({Caps, Pkt, App, {T, EvalPktFs, EvalFs}}, TPid, Dict0, RecvData) ->
    send_A(answer(T, Caps, Pkt, App, Dict0, RecvData),
           TPid,
           {App#diameter_app.dictionary, Dict0},
           Pkt,
           EvalPktFs,
           EvalFs);

send_A(_, _, _, _) ->
    ok.

%% send_A/6

send_A(T, TPid, {AppDict, Dict0} = DictT0, ReqPkt, EvalPktFs, EvalFs) ->
    {MsgDict, Pkt} = reply(T, TPid, DictT0, EvalPktFs, ReqPkt),
    incr(send, Pkt, TPid, AppDict),
    incr_rc(send, Pkt, TPid, {MsgDict, AppDict, Dict0}),  %% count outgoing
    send(TPid, Pkt),
    lists:foreach(fun diameter_lib:eval/1, EvalFs).

%% answer/6

answer({reply, Ans}, _Caps, _Pkt, App, Dict0, _RecvData) ->
    {msg_dict(App#diameter_app.dictionary, Dict0, Ans), Ans};

answer({call, Opts}, Caps, Pkt, App, Dict0, RecvData) ->
    #diameter_caps{origin_host = {OH,_}}
        = Caps,
    #diameter_packet{avps = Avps}
        = Pkt,
    {Code, _Flags, Vid} = Dict0:avp_header('Route-Record'),
    resend(is_loop(Code, Vid, OH, Dict0, Avps),
           Opts,
           Caps,
           Pkt,
           App,
           Dict0,
           RecvData);

%% RFC 3588 only allows 3xxx errors in an answer-message. RFC 6733
%% added the possibility of setting 5xxx.
answer({answer_message, RC} = T, Caps, Pkt, App, Dict0, _RecvData)  ->
    Dict0 /= ?BASE orelse 3 == RC div 1000
        orelse ?ERROR({invalid_return, T, handle_request, App}),
    answer_message(RC, Caps, Dict0, Pkt).

%% msg_dict/3
%%
%% Return the dictionary defining the message grammar in question: the
%% application dictionary or the common dictionary.

msg_dict(AppDict, Dict0, [Msg])
  when is_list(Msg);
       is_tuple(Msg) ->
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

%% Message sent as a tagged avp/value list.
is_answer_message([Name | _], _) ->
    Name == 'answer-message';

%% Message sent as a record.
is_answer_message(Rec, Dict) ->
    try
        'answer-message' == Dict:rec2msg(element(1,Rec))
    catch
        error:_ -> false
    end.

%% answer_message/4

answer_message(RC,
               #diameter_caps{origin_host  = {OH,_},
                              origin_realm = {OR,_}},
               Dict0,
               Pkt) ->
    {Dict0, answer_message(OH, OR, RC, Dict0, Pkt)}.

%% resend/7

%%   DIAMETER_LOOP_DETECTED             3005
%%      An agent detected a loop while trying to get the message to the
%%      intended recipient.  The message MAY be sent to an alternate peer,
%%      if one is available, but the peer reporting the error has
%%      identified a configuration problem.

resend(true, _Opts, Caps, Pkt, _App, Dict0, _RecvData) ->
    answer_message(3005, Caps, Dict0, Pkt);

%% 6.1.8.  Relaying and Proxying Requests
%%
%%   A relay or proxy agent MUST append a Route-Record AVP to all requests
%%   forwarded.  The AVP contains the identity of the peer the request was
%%   received from.

resend(false,
       Opts,
       #diameter_caps{origin_host = {_,OH}}
       = Caps,
       #diameter_packet{header = Hdr0,
                        avps = Avps}
       = Pkt,
       App,
       Dict0,
       #recvdata{service_name = SvcName,
                 sequence = Mask}) ->
    Route = #diameter_avp{data = {Dict0, 'Route-Record', OH}},
    Seq = diameter_session:sequence(Mask),
    Hdr = Hdr0#diameter_header{hop_by_hop_id = Seq},
    Msg = [Hdr, Route | Avps],  %% reordered at encode
    resend(send_request(SvcName, App, Msg, Opts), Caps, Dict0, Pkt).
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

%% resend/4
%%
%% Relay a reply to a relayed request.

%% Answer from the peer: reset the hop by hop identifier.
resend(#diameter_packet{bin = B}
       = Pkt,
       _Caps,
       _Dict0,
       #diameter_packet{header = #diameter_header{hop_by_hop_id = Id},
                        transport_data = TD}) ->
    Pkt#diameter_packet{bin = diameter_codec:hop_by_hop_id(Id, B),
                        transport_data = TD};
%% TODO: counters

%% Or not: DIAMETER_UNABLE_TO_DELIVER.
resend(_, Caps, Dict0, Pkt) ->
    answer_message(3002, Caps, Dict0, Pkt).

%% is_loop/5
%%
%% Is there a Route-Record AVP with our Origin-Host?

is_loop(Code,
        Vid,
        Bin,
        _Dict0,
        [#diameter_avp{code = Code, vendor_id = Vid, data = Bin} | _]) ->
    true;

is_loop(_, _, _, _, []) ->
    false;

is_loop(Code, Vid, OH, Dict0, [_ | Avps])
  when is_binary(OH) ->
    is_loop(Code, Vid, OH, Dict0, Avps);

is_loop(Code, Vid, OH, Dict0, Avps) ->
    is_loop(Code, Vid, Dict0:avp(encode, OH, 'Route-Record'), Dict0, Avps).

%% reply/5

%% Local answer ...
reply({MsgDict, Ans}, TPid, {AppDict, Dict0}, Fs, ReqPkt) ->
    local(Ans, TPid, {MsgDict, AppDict, Dict0}, Fs, ReqPkt);

%% ... or relayed.
reply(#diameter_packet{} = Pkt, _TPid, {AppDict, Dict0}, Fs, _ReqPkt) ->
    eval_packet(Pkt, Fs),
    {msg_dict(AppDict, Dict0, Pkt), Pkt}.

%% local/5
%%
%% Send a locally originating reply.

%% Skip the setting of Result-Code and Failed-AVP's below. This is
%% undocumented and shouldn't be relied on.
local([Msg], TPid, DictT, Fs, ReqPkt)
  when is_list(Msg);
       is_tuple(Msg) ->
    local(Msg, TPid, DictT, Fs, ReqPkt#diameter_packet{errors = []});

local(Msg, TPid, {MsgDict, AppDict, Dict0}, Fs, ReqPkt) ->
    Pkt = encode({MsgDict, AppDict},
                 TPid,
                 reset(make_answer_packet(Msg, ReqPkt), MsgDict, Dict0),
                 Fs),
    {MsgDict, Pkt}.

%% reset/3

%% Header/avps list: send as is.
reset(#diameter_packet{msg = [#diameter_header{} | _]} = Pkt, _, _) ->
    Pkt;

%% No errors to set or errors explicitly ignored.
reset(#diameter_packet{errors = Es} = Pkt, _, _)
  when Es == [];
       Es == false ->
    Pkt;

%% Otherwise possibly set Result-Code and/or Failed-AVP.
reset(#diameter_packet{msg = Msg, errors = Es} = Pkt, Dict, Dict0) ->
    {RC, Failed} = select_error(Msg, Es, Dict0),
    Pkt#diameter_packet{msg = reset(Msg, Dict, RC, Failed)}.

%% select_error/3
%%
%% Extract the first appropriate RC or {RC, #diameter_avp{}}
%% pair from an errors list, and accumulate all #diameter_avp{}.
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

select_error(Msg, Es, Dict0) ->
    {RC, Avps} = lists:foldl(fun(T,A) -> select(T, A, Dict0) end,
                             {is_answer_message(Msg, Dict0), []},
                             Es),
    {RC, lists:reverse(Avps)}.

%% Only integer() and {integer(), #diameter_avp{}} are the result of
%% decode. #diameter_avp{} can only be set in a reply for encode.

select(#diameter_avp{} = A, {RC, As}, _) ->
    {RC, [A|As]};

select(_, {RC, _} = Acc, _)
  when is_integer(RC) ->
    Acc;

select({RC, #diameter_avp{} = A}, {IsAns, As} = Acc, Dict0)
  when is_integer(RC) ->
    case is_result(RC, IsAns, Dict0) of
        true  -> {RC, [A|As]};
        false -> Acc
    end;

select(RC, {IsAns, As} = Acc, Dict0)
  when is_boolean(IsAns), is_integer(RC) ->
    case is_result(RC, IsAns, Dict0) of
        true  -> {RC, As};
        false -> Acc
    end.

%% reset/4

reset(Msg, Dict, RC, Avps) ->
    FailedAVP = failed_avp(Msg, Avps, Dict),
    ResultCode = rc(Msg, RC, Dict),
    set(set(Msg, FailedAVP, Dict), ResultCode, Dict).

%% eval_packet/2

eval_packet(Pkt, Fs) ->
    lists:foreach(fun(F) -> diameter_lib:eval([F,Pkt]) end, Fs).

%% make_answer_packet/2

%% Use decode errors to set Result-Code and/or Failed-AVP unless the
%% the errors field has been explicitly set. Unfortunately, the
%% default value is the empty list rather than 'undefined' so use the
%% atom 'false' for "set nothing". (This is historical and changing
%% the default value would require modules including diameter.hrl to
%% be recompiled.)
make_answer_packet(#diameter_packet{errors = []}
                   = Pkt,
                   #diameter_packet{errors = [_|_] = Es}
                   = ReqPkt) ->
    make_answer_packet(Pkt#diameter_packet{errors = Es}, ReqPkt);

%% A reply message clears the R and T flags and retains the P flag.
%% The E flag will be set at encode. 6.2 of 3588 requires the same P
%% flag on an answer as on the request. A #diameter_packet{} returned
%% from a handle_request callback can circumvent this by setting its
%% own header values.
make_answer_packet(#diameter_packet{header = Hdr,
                                    msg = Msg,
                                    errors = Es,
                                    transport_data = TD},
                   #diameter_packet{header = ReqHdr}) ->
    Hdr0 = ReqHdr#diameter_header{version = ?DIAMETER_VERSION,
                                  is_request = false,
                                  is_error = undefined,
                                  is_retransmitted = false},
    #diameter_packet{header = fold_record(Hdr0, Hdr),
                     msg = Msg,
                     errors = Es,
                     transport_data = TD};

%% Binaries and header/avp lists are sent as-is.
make_answer_packet(Bin, #diameter_packet{transport_data = TD})
  when is_binary(Bin) ->
    #diameter_packet{bin = Bin,
                     transport_data = TD};
make_answer_packet([#diameter_header{} | _] = Msg,
                   #diameter_packet{transport_data = TD}) ->
    #diameter_packet{msg = Msg,
                     transport_data = TD};

%% Otherwise, preserve transport_data.
make_answer_packet(Msg, #diameter_packet{transport_data = TD} = Pkt) ->
    make_answer_packet(#diameter_packet{msg = Msg, transport_data = TD}, Pkt).

%% Reply as name and tuple list ...
set([_|_] = Ans, Avps, _) ->
    Ans ++ Avps;  %% Values nearer tail take precedence.

%% ... or record.
set(Rec, Avps, Dict) ->
    Dict:'#set-'(Avps, Rec).

%% rc/3
%%
%% Turn the result code into a list if its optional and only set it if
%% the arity is 1 or {0,1}. In other cases (which probably shouldn't
%% exist in practise) we can't know what's appropriate.

rc(_, B, _)
  when is_boolean(B) ->
    [];

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

failed_avp(Rec, Avps, Dict) ->
    [failed(Rec, [{'AVP', Avps}], Dict)].

%% Reply as name and tuple list ...
failed([MsgName | Values], FailedAvp, Dict) ->
    RecName = Dict:msg2rec(MsgName),
    try
        Dict:'#info-'(RecName, {index, 'Failed-AVP'}),
        {'Failed-AVP', [FailedAvp]}
    catch
        error: _ ->
            Avps = proplists:get_value('AVP', Values, []),
            A = #diameter_avp{name = 'Failed-AVP',
                              value = FailedAvp},
            {'AVP', [A|Avps]}
    end;

%% ... or record.
failed(Rec, FailedAvp, Dict) ->
    try
        RecName = element(1, Rec),
        Dict:'#info-'(RecName, {index, 'Failed-AVP'}),
        {'Failed-AVP', [FailedAvp]}
    catch
        error: _ ->
            Avps = Dict:'get-'('AVP', Rec),
            A = #diameter_avp{name = 'Failed-AVP',
                              value = FailedAvp},
            {'AVP', [A|Avps]}
    end.

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

%% answer_message/5

answer_message(OH, OR, RC, Dict0, #diameter_packet{avps = Avps,
                                                   errors = Es}) ->
    {Code, _, Vid} = Dict0:avp_header('Session-Id'),
    ['answer-message', {'Origin-Host', OH},
                       {'Origin-Realm', OR},
                       {'Result-Code', RC}]
        ++ session_id(Code, Vid, Dict0, Avps)
        ++ failed_avp(RC, Es).

session_id(Code, Vid, Dict0, Avps)
  when is_list(Avps) ->
    try
        #diameter_avp{data = Bin} = find_avp(Code, Vid, Avps),
        [{'Session-Id', [Dict0:avp(decode, Bin, 'Session-Id')]}]
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
            #diameter_packet{msg = [#diameter_header{} = H | _]}
            = Pkt,
            TPid,
            DictT) ->
    incr_res(Dir, Pkt#diameter_packet{header = H}, TPid, DictT);

%% Outgoing message as binary: don't count. (Sending binaries is only
%% partially supported.)
incr_result(send, #diameter_packet{header = undefined = No}, _, _) ->
    No;

%% Incoming or outgoing. Outgoing with encode errors never gets here
%% since encode fails.
incr_result(Dir, Pkt, TPid, DictT) ->
    incr_res(Dir, Pkt, TPid, DictT).

incr_res(Dir,
         #diameter_packet{header = #diameter_header{is_error = E}
                          = Hdr,
                          errors = Es}
         = Pkt,
         TPid,
         DictT) ->
    {MsgDict, AppDict, Dict0} = DictT,

    Id = msg_id(Hdr, AppDict),
    %% Could be {relay, 0}, in which case the R-bit is redundant since
    %% only answers are being counted. Let it be however, so that the
    %% same tuple is in both send/recv and result code counters.

    %% Count incoming decode errors.
    recv /= Dir orelse [] == Es orelse incr_error(Dir, Id, TPid, AppDict),

    %% Exit on a missing result code.
    T = rc_counter(MsgDict, Dir, Pkt),
    T == false andalso ?LOGX(no_result_code, {MsgDict, Dir, Hdr}),
    {Ctr, RC, Avp} = T,

    %% Or on an inappropriate value.
    is_result(RC, E, Dict0)
        orelse ?LOGX(invalid_error_bit, {MsgDict, Dir, Hdr, Avp}),

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

%% rc_counter/3

%% RFC 3588, 7.6:
%%
%%   All Diameter answer messages defined in vendor-specific
%%   applications MUST include either one Result-Code AVP or one
%%   Experimental-Result AVP.

rc_counter(Dict, Dir, #diameter_packet{header = H,
                                       avps = As,
                                       msg = Msg})
  when Dir == recv;         %% decoded incoming
       Msg == undefined ->  %% relayed outgoing
    rc_counter(Dict, [H|As]);

rc_counter(Dict, _, #diameter_packet{msg = Msg}) ->
    rc_counter(Dict, Msg).

rc_counter(Dict, Msg) ->
    rcc(get_result(Dict, Msg)).

rcc(#diameter_avp{name = 'Result-Code' = Name, value = N} = A)
  when is_integer(N) ->
    {{Name, N}, N, A};

rcc(#diameter_avp{name = 'Result-Code' = Name, value = [N|_]} = A)
  when is_integer(N) ->
    {{Name, N}, N, A};

rcc(#diameter_avp{name = 'Experimental-Result', value = {_,_,N} = T} = A)
  when is_integer(N) ->
    {T, N, A};

rcc(#diameter_avp{name = 'Experimental-Result', value = [{_,_,N} = T|_]} = A)
  when is_integer(N) ->
    {T, N, A};

rcc(_) ->
    false.

%% get_result/2

get_result(Dict, Msg) ->
    try
        [throw(A) || N <- ['Result-Code', 'Experimental-Result'],
                     #diameter_avp{} = A <- [get_avp(Dict, N, Msg)]]
    of
        [] -> false
    catch
        #diameter_avp{} = A -> A
    end.

x(T) ->
    exit(T).

%% ---------------------------------------------------------------------------
%% # send_request/4
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

send_R(SvcName, AppOrAlias, Msg, Opts, Caller) ->
    case pick_peer(SvcName, AppOrAlias, Msg, Opts) of
        {Transport, Mask, SvcOpts} ->
            diameter_codec:setopts(SvcOpts),
            send_request(Transport, Mask, Msg, Opts, Caller, SvcName);
        {error, _} = No ->
            No
    end.

%% make_options/1

make_options(Options) ->
    lists:foldl(fun mo/2, #options{}, Options).

mo({timeout, T}, Rec)
  when is_integer(T), 0 =< T ->
    Rec#options{timeout = T};

mo({filter, F}, #options{filter = none} = Rec) ->
    Rec#options{filter = F};
mo({filter, F}, #options{filter = {all, Fs}} = Rec) ->
    Rec#options{filter = {all, [F | Fs]}};
mo({filter, F}, #options{filter = F0} = Rec) ->
    Rec#options{filter = {all, [F0, F]}};

mo({extra, L}, #options{extra = X} = Rec)
  when is_list(L) ->
    Rec#options{extra = X ++ L};

mo(detach, Rec) ->
    Rec#options{detach = true};

mo(T, _) ->
    ?ERROR({invalid_option, T}).

%% ---------------------------------------------------------------------------
%% # send_request/6
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

send_request({TPid, Caps, App}
             = Transport,
             Mask,
             Msg,
             Opts,
             Caller,
             SvcName) ->
    Pkt = make_prepare_packet(Mask, Msg),

    send_R(cb(App, prepare_request, [Pkt, SvcName, {TPid, Caps}]),
           Pkt,
           Transport,
           Opts,
           Caller,
           SvcName,
           []).

%% send_R/7

send_R({send, Msg}, Pkt, Transport, Opts, Caller, SvcName, Fs) ->
    send_R(make_request_packet(Msg, Pkt),
           Transport,
           Opts,
           Caller,
           SvcName,
           Fs);

send_R({discard, Reason} , _, _, _, _, _, _) ->
    {error, Reason};

send_R(discard, _, _, _, _, _, _) ->
    {error, discarded};

send_R({eval_packet, RC, F}, Pkt, T, Opts, Caller, SvcName, Fs) ->
    send_R(RC, Pkt, T, Opts, Caller, SvcName, [F|Fs]);

send_R(E, _, {_, _, App}, _, _, _, _) ->
    ?ERROR({invalid_return, E, prepare_request, App}).

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

make_prepare_packet(Mask, Msg) ->
    make_prepare_packet(Mask, #diameter_packet{msg = Msg}).

%% make_prepare_header/2

make_prepare_header(Mask, undefined) ->
    Seq = diameter_session:sequence(Mask),
    make_prepare_header(#diameter_header{end_to_end_id = Seq,
                                         hop_by_hop_id = Seq});

make_prepare_header(Mask, #diameter_header{end_to_end_id = undefined,
                                           hop_by_hop_id = undefined}
                          = H) ->
    Seq = diameter_session:sequence(Mask),
    make_prepare_header(H#diameter_header{end_to_end_id = Seq,
                                          hop_by_hop_id = Seq});

make_prepare_header(Mask, #diameter_header{end_to_end_id = undefined} = H) ->
    Seq = diameter_session:sequence(Mask),
    make_prepare_header(H#diameter_header{end_to_end_id = Seq});

make_prepare_header(Mask, #diameter_header{hop_by_hop_id = undefined} = H) ->
    Seq = diameter_session:sequence(Mask),
    make_prepare_header(H#diameter_header{hop_by_hop_id = Seq});

make_prepare_header(_, Hdr) ->
    make_prepare_header(Hdr).

%% make_prepare_header/1

make_prepare_header(#diameter_header{version = undefined} = Hdr) ->
    make_prepare_header(Hdr#diameter_header{version = ?DIAMETER_VERSION});

make_prepare_header(#diameter_header{} = Hdr) ->
    Hdr;

make_prepare_header(T) ->
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

%% make_retransmit_packet/2

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

fold_record(undefined, R) ->
    R;
fold_record(Rec, R) ->
    diameter_lib:fold_tuple(2, Rec, R).

%% send_R/6

send_R(Pkt0,
       {TPid, Caps, #diameter_app{dictionary = AppDict} = App},
       Opts,
       {Pid, Ref},
       SvcName,
       Fs) ->
    Pkt = encode(AppDict, TPid, Pkt0, Fs),

    #options{timeout = Timeout}
        = Opts,

    Req = #request{ref = Ref,
                   caller = Pid,
                   handler = self(),
                   transport = TPid,
                   caps = Caps,
                   packet = Pkt0},

    incr(send, Pkt, TPid, AppDict),
    TRef = send_request(TPid, Pkt, Req, SvcName, Timeout),
    Pid ! Ref,  %% tell caller a send has been attempted
    handle_answer(SvcName,
                  App,
                  recv_A(Timeout, SvcName, App, Opts, {TRef, Req})).

%% recv_A/5

recv_A(Timeout, SvcName, App, Opts, {TRef, #request{ref = Ref} = Req}) ->
    %% Matching on TRef below ensures we ignore messages that pertain
    %% to a previous transport prior to failover. The answer message
    %% includes the #request{} since it's not necessarily Req; that
    %% is, from the last peer to which we've transmitted.
    receive
        {answer = A, Ref, Rq, Dict0, Pkt} ->  %% Answer from peer
            {A, Rq, Dict0, Pkt};
        {timeout = Reason, TRef, _} ->        %% No timely reply
            {error, Req, Reason};
        {failover, TRef} ->       %% Service says peer has gone down
            retransmit(pick_peer(SvcName, App, Req, Opts),
                       Req,
                       Opts,
                       SvcName,
                       Timeout)
    end.

%% handle_answer/3

handle_answer(SvcName, App, {error, Req, Reason}) ->
    handle_error(App, Req, Reason, SvcName);

handle_answer(SvcName,
              #diameter_app{dictionary = AppDict,
                            id = Id}
              = App,
              {answer, Req, Dict0, Pkt}) ->
    MsgDict = msg_dict(AppDict, Dict0, Pkt),
    handle_A(errors(Id, diameter_codec:decode({MsgDict, AppDict}, Pkt)),
             SvcName,
             MsgDict,
             Dict0,
             App,
             Req).

%% We don't really need to do a full decode if we're a relay and will
%% just resend with a new hop by hop identifier, but might a proxy
%% want to examine the answer?

handle_A(Pkt, SvcName, Dict, Dict0, App, #request{transport = TPid} = Req) ->
    AppDict = App#diameter_app.dictionary,

    incr(recv, Pkt, TPid, AppDict),

    try
        incr_result(recv, Pkt, TPid, {Dict, AppDict, Dict0}) %% count incoming
    of
        _ -> answer(Pkt, SvcName, App, Req)
    catch
        exit: {no_result_code, _} ->
            %% RFC 6733 requires one of Result-Code or
            %% Experimental-Result, but the decode will have detected
            %% a missing AVP. If both are optional in the dictionary
            %% then this isn't a decode error: just continue on.
            answer(Pkt, SvcName, App, Req);
        exit: {invalid_error_bit, {_, _, _, Avp}} ->
            #diameter_packet{errors = Es}
                = Pkt,
            E = {5004, Avp},
            answer(Pkt#diameter_packet{errors = [E|Es]}, SvcName, App, Req)
    end.

%% answer/4

answer(Pkt,
       SvcName,
       #diameter_app{module = ModX,
                     options = [{answer_errors, AE} | _]},
       Req) ->
    a(Pkt, SvcName, ModX, AE, Req).

-spec a(_, _, _) -> no_return().  %% silence dialyzer

a(#diameter_packet{errors = Es}
  = Pkt,
  SvcName,
  ModX,
  AE,
  #request{transport = TPid,
           caps = Caps,
           packet = P})
  when [] == Es;
       callback == AE ->
    cb(ModX, handle_answer, [Pkt, msg(P), SvcName, {TPid, Caps}]);

a(Pkt, SvcName, _, AE, _) ->
    a(Pkt#diameter_packet.header, SvcName, AE).

a(Hdr, SvcName, report) ->
    MFA = {?MODULE, handle_answer, [SvcName, Hdr]},
    diameter_lib:warning_report(errors, MFA),
    a(Hdr, SvcName, discard);

a(Hdr, SvcName, discard) ->
    x({answer_errors, {SvcName, Hdr}}).

%% Note that we don't check that the application id in the answer's
%% header is what we expect. (TODO: Does the rfc says anything about
%% this?)

%% Note that failover starts a new timer and that expiry of an old
%% timer value is ignored. This means that an answer could be accepted
%% from a peer after timeout in the case of failover.

%% retransmit/5

retransmit({{_,_,App} = Transport, _, _}, Req, Opts, SvcName, Timeout) ->
    try retransmit(Transport, Req, SvcName, Timeout) of
        T -> recv_A(Timeout, SvcName, App, Opts, T)
    catch
        ?FAILURE(Reason) -> {error, Req, Reason}
    end;

retransmit(_, Req, _, _, _) ->  %% no alternate peer
    {error, Req, failover}.

%% pick_peer/4

%% Retransmission after failover: call-specific arguments have already
%% been appended in App.
pick_peer(SvcName,
          App,
          #request{packet = #diameter_packet{msg = Msg}},
          Opts) ->
    pick_peer(SvcName, App, Msg, Opts#options{extra = []});

pick_peer(_, _, undefined, _) ->
    {error, no_connection};

pick_peer(SvcName,
          AppOrAlias,
          Msg,
          #options{filter = Filter, extra = Xtra}) ->
    pick(diameter_service:pick_peer(SvcName,
                                    AppOrAlias,
                                    {fun(D) -> get_destination(D, Msg) end,
                                     Filter,
                                     Xtra})).

pick(false) ->
    {error, no_connection};

pick(T) ->
    T.

%% handle_error/4

handle_error(App,
             #request{packet = Pkt,
                      transport = TPid,
                      caps = Caps},
             Reason,
             SvcName) ->
    cb(App, handle_error, [Reason, msg(Pkt), SvcName, {TPid, Caps}]).

msg(#diameter_packet{msg = undefined, bin = Bin}) ->
    Bin;
msg(#diameter_packet{msg = Msg}) ->
    Msg.

%% encode/4

encode(Dict, TPid, Pkt, Fs) ->
    P = encode(Dict, TPid, Pkt),
    eval_packet(P, Fs),
    P.

%% encode/2

%% Note that prepare_request can return a diameter_packet containing a
%% header or transport_data. Even allow the returned record to contain
%% an encoded binary. This isn't the usual case and doesn't properly
%% support retransmission but is useful for test.

encode(Dict, TPid, Pkt)
  when is_atom(Dict) ->
    encode({Dict, Dict}, TPid, Pkt);

%% A message to be encoded.
encode(DictT, TPid, #diameter_packet{bin = undefined} = Pkt) ->
    {Dict, AppDict} = DictT,
    try
        diameter_codec:encode(Dict, Pkt)
    catch
        exit: {diameter_codec, encode, T} = Reason ->
            incr_error(send, T, TPid, AppDict),
            exit(Reason)
    end;

%% An encoded binary: just send.
encode(_, _, #diameter_packet{} = Pkt) ->
    Pkt.

%% send_request/5

send_request(TPid, #diameter_packet{bin = Bin} = Pkt, Req, _SvcName, Timeout)
  when node() == node(TPid) ->
    Seqs = diameter_codec:sequence_numbers(Bin),
    TRef = erlang:start_timer(Timeout, self(), TPid),
    Entry = {Seqs, Req, TRef},

    %% Ensure that request table is cleaned even if we receive an exit
    %% signal. An alternative would be to simply trap exits, but
    %% callbacks are applied in this process, and these could possibly
    %% be expecting the prevailing behaviour.
    Self = self(),
    spawn(fun() -> diameter_lib:wait([Self]), erase_request(Entry) end),

    store_request(Entry, TPid),
    send(TPid, Pkt),
    TRef;

%% Send using a remote transport: spawn a process on the remote node
%% to relay the answer.
send_request(TPid, #diameter_packet{} = Pkt, Req, SvcName, Timeout) ->
    TRef = erlang:start_timer(Timeout, self(), TPid),
    T = {TPid, Pkt, Req, SvcName, Timeout, TRef},
    spawn(node(TPid), ?MODULE, send, [T]),
    TRef.

%% send/1

send({TPid, Pkt, #request{handler = Pid} = Req0, SvcName, Timeout, TRef}) ->
    Req = Req0#request{handler = self()},
    recv(TPid, Pid, TRef, send_request(TPid, Pkt, Req, SvcName, Timeout)).

%% recv/4
%%
%% Relay an answer from a remote node.

recv(TPid, Pid, TRef, LocalTRef) ->
    receive
        {answer, _, _, _, _} = A ->
            Pid ! A;
        {failover = T, LocalTRef} ->
            Pid ! {T, TRef};
        T ->
            exit({timeout, LocalTRef, TPid} = T)
    end.

%% send/2

send(Pid, Pkt) ->  %% Strip potentially large message terms.
    #diameter_packet{header = H,
                     bin = Bin,
                     transport_data = T}
        = Pkt,
    Pid ! {send, #diameter_packet{header = H,
                                  bin = Bin,
                                  transport_data = T}}.

%% retransmit/4

retransmit({TPid, Caps, App}
           = Transport,
           #request{packet = Pkt0}
           = Req,
           SvcName,
           Timeout) ->
    have_request(Pkt0, TPid)     %% Don't failover to a peer we've
        andalso ?THROW(timeout), %% already sent to.

    Pkt = make_retransmit_packet(Pkt0),

    retransmit(cb(App, prepare_retransmit, [Pkt, SvcName, {TPid, Caps}]),
               Transport,
               Req#request{packet = Pkt},
               SvcName,
               Timeout,
               []).

retransmit({send, Msg},
           Transport,
           #request{packet = Pkt}
           = Req,
           SvcName,
           Timeout,
           Fs) ->
    resend_request(make_request_packet(Msg, Pkt),
                   Transport,
                   Req,
                   SvcName,
                   Timeout,
                   Fs);

retransmit({discard, Reason}, _, _, _, _, _) ->
    ?THROW(Reason);

retransmit(discard, _, _, _, _, _) ->
    ?THROW(discarded);

retransmit({eval_packet, RC, F}, Transport, Req, SvcName, Timeout, Fs) ->
    retransmit(RC, Transport, Req, SvcName, Timeout, [F|Fs]);

retransmit(T, {_, _, App}, _, _, _, _) ->
    ?ERROR({invalid_return, T, prepare_retransmit, App}).

resend_request(Pkt0,
               {TPid, Caps, #diameter_app{dictionary = AppDict}},
               Req0,
               SvcName,
               Tmo,
               Fs) ->
    Pkt = encode(AppDict, TPid, Pkt0, Fs),

    Req = Req0#request{transport = TPid,
                       packet = Pkt0,
                       caps = Caps},

    ?LOG(retransmission, Pkt#diameter_packet.header),
    incr(TPid, {msg_id(Pkt, AppDict), send, retransmission}),
    TRef = send_request(TPid, Pkt, Req, SvcName, Tmo),
    {TRef, Req}.

%% store_request/2

store_request(T, TPid) ->
    ets:insert(?REQUEST_TABLE, T),
    ets:member(?REQUEST_TABLE, TPid)
        orelse begin
                   {_Seqs, _Req, TRef} = T,
                   self() ! {failover, TRef}  %% failover/1 may have missed
               end.

%% lookup_request/2
%%
%% Note the match on both the key and transport pid. The latter is
%% necessary since the same Hop-by-Hop and End-to-End identifiers are
%% reused in the case of retransmission.

lookup_request(Msg, TPid) ->
    Seqs = diameter_codec:sequence_numbers(Msg),
    Spec = [{{Seqs, #request{transport = TPid, _ = '_'}, '_'},
             [],
             ['$_']}],
    case ets:select(?REQUEST_TABLE, Spec) of
        [{_, Req, _}] ->
            Req;
        [] ->
            false
    end.

%% erase_request/1

erase_request(T) ->
    ets:delete_object(?REQUEST_TABLE, T).

%% match_requests/1

match_requests(TPid) ->
    Pat = {'_', #request{transport = TPid, _ = '_'}, '_'},
    ets:select(?REQUEST_TABLE, [{Pat, [], ['$_']}]).

%% have_request/2

have_request(Pkt, TPid) ->
    Seqs = diameter_codec:sequence_numbers(Pkt),
    Pat = {Seqs, #request{transport = TPid, _ = '_'}, '_'},
    '$end_of_table' /= ets:select(?REQUEST_TABLE, [{Pat, [], ['$_']}], 1).

%% ---------------------------------------------------------------------------
%% # failover/1-2
%% ---------------------------------------------------------------------------

failover(TPid)
  when is_pid(TPid) ->
    lists:foreach(fun failover/1, match_requests(TPid));
%% Note that a request process can store its request after failover
%% notifications are sent here: store_request/2 sends the notification
%% in that case.

%% Failover as a consequence of peer_down/1: inform the
%% request process.
failover({_, Req, TRef}) ->
    #request{handler = Pid,
             packet = #diameter_packet{msg = M}}
        = Req,
    M /= undefined andalso (Pid ! {failover, TRef}).
%% Failover is not performed when msg = binary() since sending
%% pre-encoded binaries is only partially supported. (Mostly for
%% test.)

%% get_destination/2

get_destination(Dict, Msg) ->
    [str(get_avp_value(Dict, D, Msg)) || D <- ['Destination-Realm',
                                               'Destination-Host']].

%% This is not entirely correct. The avp could have an arity 1, in
%% which case an empty list is a DiameterIdentity of length 0 rather
%% than the list of no values we treat it as by mapping to undefined.
%% This behaviour is documented.
str([]) ->
    undefined;
str(T) ->
    T.

%% get_avp/3
%%
%% Find an AVP in a message of one of three forms:
%%
%% - a message record (as generated from a .dia spec) or
%% - a list of an atom message name followed by 2-tuple, avp name/value pairs.
%% - a list of a #diameter_header{} followed by #diameter_avp{} records,
%%
%% In the first two forms a dictionary module is used at encode to
%% identify the type of the AVP and its arity in the message in
%% question. The third form allows messages to be sent as is, without
%% a dictionary, which is needed in the case of relay agents, for one.

%% Messages will be header/avps list as a relay and the only AVP's we
%% look for are in the common dictionary. This is required since the
%% relay dictionary doesn't inherit the common dictionary (which maybe
%% it should).
get_avp(?RELAY, Name, Msg) ->
    get_avp(?BASE, Name, Msg);

%% Message as a header/avps list.
get_avp(Dict, Name, [#diameter_header{} | Avps]) ->
    try
        {Code, _, VId} = Dict:avp_header(Name),
        find_avp(Code, VId, Avps)
    of
        A ->
            (avp_decode(Dict, Name, ungroup(A)))#diameter_avp{name = Name}
    catch
        error: _ ->
            undefined
    end;

%% Outgoing message as a name/values list.
get_avp(_, Name, [_MsgName | Avps]) ->
    case lists:keyfind(Name, 1, Avps) of
        {_, V} ->
            #diameter_avp{name = Name, value = V};
        _ ->
            undefined
    end;

%% Message is typically a record but not necessarily.
get_avp(Dict, Name, Rec) ->
    try
        #diameter_avp{name = Name, value = Dict:'#get-'(Name, Rec)}
    catch
        error:_ ->
            undefined
    end.

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

avp_decode(Dict, Name, #diameter_avp{value = undefined,
                                     data = Bin}
                       = Avp) ->
    try Dict:avp(decode, Bin, Name) of
        V ->
            Avp#diameter_avp{value = V}
    catch
        error:_ ->
            Avp
    end;
avp_decode(_, _, #diameter_avp{} = Avp) ->
    Avp.

cb(#diameter_app{module = [_|_] = M}, F, A) ->
    eval(M, F, A);
cb([_|_] = M, F, A) ->
    eval(M, F, A).

eval([M|X], F, A) ->
    apply(M, F, A ++ X).

choose(true, X, _)  -> X;
choose(false, _, X) -> X.
