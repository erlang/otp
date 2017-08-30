%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Test encoding/decoding of the sample call flows Megaco/H.248
%%----------------------------------------------------------------------
%% megaco_call_flow_test:pretty_text().
%% megaco_call_flow_test:compact_text().
%% megaco_call_flow_test:bin().
%% megaco_call_flow_test:asn1_ber().
%% megaco_call_flow_test:asn1_per().
%% megaco_call_flow_test:erl_dist().
%% megaco_call_flow_test:compressed_erl_dist().
%% megaco_call_flow_test:gnuplot_gif().
%% megaco_call_flow_test:gnuplot_size_gif().
%% megaco_call_flow_test:gnuplot_enc_time_gif().
%% megaco_call_flow_test:gnuplot_dec_time_gif().
%% megaco_call_flow_test:gnuplot_code_time_gif().
%%----------------------------------------------------------------------

-module(megaco_call_flow_test).

-compile(export_all).
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include("megaco_test_lib.hrl").

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).

%% Test server callbacks
init_per_testcase(Case, Config) ->
    megaco_test_lib:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->
    megaco_test_lib:end_per_testcase(Case, Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

all() -> 
    [{group, text}, {group, binary}].

groups() -> 
    [{text, [], [pretty, compact]},
     {flex, [], [pretty_flex, compact_flex]},
     {binary, [], [bin, ber, per]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

pretty(suite) ->
    [];
pretty(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    pretty_text().

compact(suite) ->
    [];
compact(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    compact_text().

pretty_flex(suite) ->
    [];
pretty_flex(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    pretty_flex().

compact_flex(suite) ->
    [];
compact_flex(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    compact_flex().

bin(suite) ->
    [];
bin(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    bin().

ber(suite) ->
    [];
ber(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    asn1_ber().

per(suite) ->
    [];
per(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    asn1_per().

standard_erl(suite) ->
    [];
standard_erl(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    standard_erl().

compressed_erl(suite) ->
    [];
compressed_erl(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    compressed_erl().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(PP(Name, Val), #'PropertyParm'{name = Name, value = [Val]}).
-define(SP(Name, Val), #'StatisticsParameter'{statName = Name, statValue = [Val]}) .

names() ->
    [
     msg1, msg2, msg3, msg4, msg5a, msg5b, msg6, msg7, msg9, msg10,
     msg11, msg12, msg13, msg14, msg15, msg16, msg17a, msg17b,
     msg18a, msg18b, msg18c, msg18d, msg19a, msg19b, msg20, msg21,
     msg22a, msg22b, msg23a, msg23b
    ].

%% In this example, MG1 has the IP address 124.124.124.222, MG2 is
%% 125.125.125.111, and the MGC is 123.123.123.4. The default Megaco port
%% is 55555 for all three.

-define(DEFAULT_PORT, 55555).
-define(MG1_MID_NO_PORT, {ip4Address,
                          #'IP4Address'{address = [124, 124, 124, 222]}}).
-define(MG1_MID, {ip4Address, #'IP4Address'{address = [124, 124, 124, 222],
                                            portNumber = ?DEFAULT_PORT}}).
-define(MG2_MID, {ip4Address, #'IP4Address'{address = [125, 125, 125, 111],
                                            portNumber = ?DEFAULT_PORT}}).
-define(MGC_MID, {ip4Address, #'IP4Address'{address = [123, 123, 123, 4],
                                            portNumber = ?DEFAULT_PORT}}).

-define(A4444, ["11111111", "00000000", "00000000"]).
-define(A4445, ["11111111", "00000000", "11111111"]).
-define(A5555, ["11111111", "11111111", "00000000"]).
-define(A5556, ["11111111", "11111111", "11111111"]).

%% -define(A4444, ["00000000"]).
%% -define(A4445, ["00001111"]).
%% -define(A5555, ["11110000"]).
%% -define(A5556, ["11111111"]).

request(Mid, TransId, ContextId, CmdReq) when is_list(CmdReq) ->
    Actions = [#'ActionRequest'{contextId = ContextId,
                                commandRequests = CmdReq}],
    Req = {transactions,
           [{transactionRequest,
             #'TransactionRequest'{transactionId = TransId,
                                   actions = Actions}}]},
    #'MegacoMessage'{mess = #'Message'{version = 1,
                                       mId = Mid,
                                       messageBody = Req}}.

reply(Mid, TransId, ContextId, CmdReply) when is_list(CmdReply) ->
    Actions = [#'ActionReply'{contextId = ContextId,
                              commandReply = CmdReply}],
    Req = {transactions,
           [{transactionReply,
             #'TransactionReply'{transactionId = TransId,
                                 transactionResult = {actionReplies, Actions}}}]},
    #'MegacoMessage'{mess = #'Message'{version = 1,
                                       mId = Mid,
                                       messageBody = Req}}.

%%----------------------------------------------------------------------
%% 1. An MG registers with an MGC using the ServiceChange command:
%%
%% MG1 to MGC:
%% "MEGACO/1 [124.124.124.222]
%% Transaction = 9998 {
%%     Context = - {
%%         ServiceChange = ROOT {Services {
%%             Method=Restart,
%%             ServiceChangeAddress=55555, Profile=ResGW/1}
%%         }
%%     }
%% }
%%----------------------------------------------------------------------

msg1() ->
    msg1(?MG1_MID_NO_PORT).
msg1(Mid) ->
    Address = {portNumber, ?DEFAULT_PORT},
    Profile = #'ServiceChangeProfile'{profileName = "resgw",
                                      version = 1},
    Parm = #'ServiceChangeParm'{serviceChangeMethod = restart,
                                serviceChangeAddress = Address,
                                serviceChangeReason  = ["901 mg cold boot"],
                                %% BUGBUG: Mandatory reason missing in spec
                                serviceChangeProfile = Profile},
    Req = #'ServiceChangeRequest'{terminationID = [?megaco_root_termination_id],
                                  serviceChangeParms = Parm},
    CmdReq = #'CommandRequest'{command = {serviceChangeReq, Req}},
    request(Mid, 9998, ?megaco_null_context_id, [CmdReq]).

%%----------------------------------------------------------------------
%%
%% 2. The MGC sends a reply:
%%
%% MGC to MG1:
%% MEGACO/1 [123.123.123.4]:55555
%% Reply = 9998 {
%%    Context = - {ServiceChange = ROOT {
%%      Services {ServiceChangeAddress=55555, Profile=ResGW/1} } }
%% }
%%----------------------------------------------------------------------

msg2() ->
    msg2(?MGC_MID).
msg2(Mid) ->
    Address = {portNumber, ?DEFAULT_PORT},
    Profile = #'ServiceChangeProfile'{profileName = "resgw",
                                      version = 1},
    Parm = #'ServiceChangeResParm'{serviceChangeAddress = Address,
                                   serviceChangeProfile = Profile},
    Reply = #'ServiceChangeReply'{terminationID = [?megaco_root_termination_id],
                                  serviceChangeResult = {serviceChangeResParms,
                                                         Parm}},
    reply(Mid, 9998, ?megaco_null_context_id, [{serviceChangeReply, Reply}]).

%%----------------------------------------------------------------------
%% 3. The MGC programs a Termination in the NULL context. The
%%    terminationId is A4444, the streamId is 1, the requestId in the
%%    Events descriptor is 2222. The mId is the identifier of the sender
%%    of this message, in this case, it is the IP address and port
%%    [123.123.123.4]:55555. Mode for this stream is set to
%%    SendReceive. "al" is the analog line supervision package.
%%
%% MGC to MG1:
%% MEGACO/1 [123.123.123.4]:55555
%% Transaction = 9999 {
%%     Context = - {
%%         Modify = A4444 {
%%             Media { Stream = 1 {
%%                      LocalControl {
%%                          Mode = SendReceive,
%%                          tdmc/gain=2,  ; in dB,
%%                          tdmc/ec=G165
%%                      },
%%                      Local {
%% v=0
%% c=IN IP4 $
%% m=audio $ RTP/AVP 0
%% a=fmtp:PCMU VAD=X-NNVAD ; special voice activity
%%                         ; detection algorithm
%%                      }
%%                  }
%%             },
%%             Events = 2222 {al/of}
%%         }
%%     }
%% }
%%----------------------------------------------------------------------

msg3() ->
    msg3(?MGC_MID).
msg3(Mid) ->
    msg3(Mid, ?A4444).
msg3(Mid, Tid) ->
    Gain = ?PP("tdmc/gain", "2"),
    %% Ec   = ?PP("tdmc/ec", "G165"),
    Ec   = ?PP("tdmc/ec", "g165"), %% BUGBUG: should be case insensitive
    LCD = #'LocalControlDescriptor'{streamMode = sendRecv,
                                    propertyParms = [Gain, Ec]},
    V = ?PP("v", "0"),
    C = ?PP("c", "IN IP4 $ "),
    M = ?PP("m", "audio $ RTP/AVP 0"),
    A = ?PP("a", "fmtp:PCMU VAD=X-NNVAD"),
    LD = #'LocalRemoteDescriptor'{propGrps = [[V, C, M, A]]},
    Parms = #'StreamParms'{localControlDescriptor = LCD,
                           localDescriptor = LD},
    StreamDesc = #'StreamDescriptor'{streamID = 1,
                                     streamParms = Parms},
    MediaDesc = #'MediaDescriptor'{streams = {multiStream, [StreamDesc]}},
    ReqEvent = #'RequestedEvent'{pkgdName = "al/of",
                                 evParList = []},
    EventsDesc = #'EventsDescriptor'{requestID = 2222,
                                     eventList = [ReqEvent]},
    AmmReq = #'AmmRequest'{terminationID = [#megaco_term_id{id = Tid}],
                           descriptors = [{mediaDescriptor, MediaDesc},
                                          {eventsDescriptor, EventsDesc}]},
    CmdReq = #'CommandRequest'{command = {modReq, AmmReq}},
    request(Mid, 9999, ?megaco_null_context_id, [CmdReq]).

%%----------------------------------------------------------------------
%% The dialplan script could have been loaded into the MG previously.
%% Its function would be to wait for the OffHook, turn on dialtone and
%% start collecting DTMF digits. However in this example, we use the
%% digit map, which is put into place after the offhook is detected (step
%% 5 below).
%%
%%
%% Note that the embedded EventsDescriptor could have been used to
%% combine steps 3 and 4 with steps 8 and 9, eliminating steps 6 and 7.
%%
%%
%% 4. The MG1 accepts the Modify with this reply:
%%
%% MG1 to MGC:
%% MEGACO/1 [124.124.124.222]:55555
%% Reply = 9999 {
%%    Context = - {Modify = A4444}
%% }
%%----------------------------------------------------------------------

msg4() ->
    msg4(?MG1_MID).
msg4(Mid) ->
    msg4(Mid, ?A4444).
msg4(Mid, Tid) ->
    Reply = #'AmmsReply'{terminationID = [#megaco_term_id{id = Tid}]},
    reply(Mid, 9999, ?megaco_null_context_id, [{modReply, Reply}]).

%%----------------------------------------------------------------------
%% 5. A similar exchange happens between MG2 and the MGC, resulting in an
%%    idle Termination called A5555.
%%----------------------------------------------------------------------

msg5a() ->
    msg5a(?MGC_MID).
msg5a(Mid) ->
    msg3(Mid, ?A4444).

msg5b() ->
    msg5b(?MG2_MID).
msg5b(Mid) ->
    msg4(Mid, ?A5555).

%%----------------------------------------------------------------------
%% A.1.2 Collecting Originator Digits and Initiating Termination
%%
%% The following builds upon the previously shown conditions.  It
%% illustrates the transactions from the Media Gateway Controller and
%% originating Media Gateway (MG1) to get the originating Termination
%% (A4444) through the stages of digit collection required to initiate a
%% connection to the terminating Media Gateway (MG2).
%%
%% 6. MG1 detects an offhook event from User 1 and reports it to the
%%    Media Gateway Controller via the Notify Command.
%%
%% MG1 to MGC:
%% MEGACO/1 [124.124.124.222]:55555
%% Transaction = 10000 {
%%    Context = - {
%%        Notify = A4444 {ObservedEvents =2222 {
%%          19990729T22000000:al/of}}
%%    }
%% }
%%----------------------------------------------------------------------

msg6() ->
    msg6(?MG1_MID).
msg6(Mid) ->
    TimeStamp = #'TimeNotation'{date = "19990729",
                                time = "22000000"},
    Event = #'ObservedEvent'{eventName = "al/of",
                             timeNotation = TimeStamp,
                             eventParList = []},
    Desc = #'ObservedEventsDescriptor'{requestId = 2222,
                                       observedEventLst = [Event]},
    NotifyReq = #'NotifyRequest'{terminationID = [#megaco_term_id{id = ?A4444}],
                                 observedEventsDescriptor = Desc},
    CmdReq = #'CommandRequest'{command = {notifyReq, NotifyReq}},
    request(Mid, 10000, ?megaco_null_context_id, [CmdReq]).

%%----------------------------------------------------------------------
%% 7. And the Notify is acknowledged.
%%
%% MGC to MG1:
%% MEGACO/1 [123.123.123.4]:55555
%% Reply = 10000 {
%%     Context = - {Notify = A4444}
%% }
%%----------------------------------------------------------------------

msg7() ->
    msg7(?MGC_MID).
msg7(Mid) ->
    Reply = #'NotifyReply'{terminationID = [#megaco_term_id{id = ?A4444}]},
    reply(Mid, 10000, ?megaco_null_context_id, [{notifyReply, Reply}]).

%%----------------------------------------------------------------------
%% 8. The MGC Modifies the termination to play dial tone, and to look for
%%    digits now. There is also an embedded event to stop dialtone upon
%%    detection of the first digit. dd is the DTMF Detection package, and
%%    ce is the completion event.
%%----------------------------------------------------------------------

%% BUGBUG: Example missing in spec

%%----------------------------------------------------------------------
%% 9.
%%
%% MGC to MG1:
%% MEGACO/1 [123.123.123.4]:55555
%% Transaction = 10001 {
%%     Context = - {
%%         Modify = A4444 {
%%             Events = 2223 {
%%                 al/on, dd/ce {DigitMap=Dialplan0}
%%             },
%%             Signals {cg/dt},
%%             DigitMap= Dialplan0{
%% (0S| 00S|[1-7]xLxx|8Lxxxxxxx|#xxxxxxx|*xx|9L1xxxxxxxxxx|9L011x.S)}
%%         }
%%     }
%% }
%%----------------------------------------------------------------------

msg9() ->
    msg9(?MGC_MID).
msg9(Mid) ->
    Name = "dialplan00",
    Body = "(0s| 00s|[1-7]xlxx|8lxxxxxxx|#xxxxxxx|*xx|9l1xxxxxxxxxx|9l011x.s)",
    Value = #'DigitMapValue'{digitMapBody = Body},
    On = #'RequestedEvent'{pkgdName = "al/on", evParList = []},
    Action = #'RequestedActions'{eventDM = {digitMapName, Name}},
    Ce = #'RequestedEvent'{pkgdName = "dd/ce",
                           eventAction = Action,
                           evParList = []},
    EventsDesc = #'EventsDescriptor'{requestID = 2223,
                                     eventList = [On, Ce]},
    Signal = #'Signal'{signalName = "cg/rt", sigParList = []},
    DigMapDesc = #'DigitMapDescriptor'{digitMapName = Name,
                                       digitMapValue = Value},
    AmmReq = #'AmmRequest'{terminationID = [#megaco_term_id{id = ?A4444}],
                           descriptors = [{eventsDescriptor, EventsDesc},
                                          {signalsDescriptor, [{signal, Signal}]},
                                          {digitMapDescriptor, DigMapDesc}]},
    CmdReq = #'CommandRequest'{command = {modReq, AmmReq}},
    request(Mid, 10001, ?megaco_null_context_id, [CmdReq]).

%%----------------------------------------------------------------------
%% 10. And the Modify is acknowledged.
%%
%% MG1 to MGC:
%% MEGACO/1 [124.124.124.222]:55555
%% Reply = 10001 {
%%     Context = - {Modify = A4444}
%% }
%%----------------------------------------------------------------------

msg10() ->
    msg10(?MG1_MID).
msg10(Mid) ->
    Reply = #'AmmsReply'{terminationID = [#megaco_term_id{id = ?A4444}]},
    reply(Mid, 10001, ?megaco_null_context_id, [{modReply, Reply}]).

%%----------------------------------------------------------------------
%% 11. Next, digits are accumulated by MG1 as they are dialed by User 1.
%%     Dialtone is stopped upon detection of the first digit, using the
%%     embedded event in step 8. When an appropriate match is made of
%%     collected digits against the currently programmed Dialplan for
%%     A4444, another Notify is sent to the Media Gateway Controller.
%%
%% MG1 to MGC:
%% MEGACO/1 [124.124.124.222]:55555
%% Transaction = 10002 {
%%    Context = - {
%%        Notify = A4444 {ObservedEvents =2223 {
%%          19990729T22010001:dd/ce{ds="916135551212"}}}
%%    }
%% }
%%----------------------------------------------------------------------

msg11() ->
    msg11(?MG1_MID).
msg11(Mid) ->
    TimeStamp = #'TimeNotation'{date = "19990729",
                                time = "22010001"},
    Parm = #'EventParameter'{eventParameterName = "ds",
                             value = ["916135551212"]},
    %% BUGBUG: Quoted string or safe char?
    Event = #'ObservedEvent'{eventName = "dd/ce",
                             timeNotation = TimeStamp,
                             eventParList = [Parm]},
    Desc = #'ObservedEventsDescriptor'{requestId = 2223,
                                       observedEventLst = [Event]},
    NotifyReq = #'NotifyRequest'{terminationID = [#megaco_term_id{id = ?A4444}],
                                 observedEventsDescriptor = Desc},
    CmdReq = #'CommandRequest'{command = {notifyReq, NotifyReq}},
    request(Mid, 10002, ?megaco_null_context_id, [CmdReq]).

%%----------------------------------------------------------------------
%% 12. And the Notify is acknowledged.
%%
%% MGC to MG1:
%% MEGACO/1 [123.123.123.4]:55555
%% Reply = 10002 {
%%     Context = - {Notify = A4444}
%% }
%%----------------------------------------------------------------------

msg12() ->
    msg12(?MGC_MID).
msg12(Mid) ->
    Reply = #'NotifyReply'{terminationID = [#megaco_term_id{id = ?A4444}]},
    reply(Mid, 10002, ?megaco_null_context_id, [{notifyReply, Reply}]).

%%----------------------------------------------------------------------
%% 13. The controller then analyses the digits and determines that a
%%     connection needs to be made from MG1 to MG2. Both the TDM
%%     termination A4444, and an RTP termination are added to a new
%%     context in MG1. Mode is ReceiveOnly since Remote descriptor values
%%     are not yet specified. Preferred codecs are in the MGC's preferred
%%     order of choice.
%%
%% MGC to MG1:
%% MEGACO/1 [123.123.123.4]:55555
%% Transaction = 10003 {
%%     Context = $ {
%%        Add = A4444,
%%        Add = $ {
%%            Media {
%%              Stream = 1 {
%%                   LocalControl {
%%                       Mode = ReceiveOnly,
%%
%%                       nt/jit=40, ; in ms
%%                   },
%%                   Local {
%% v=0
%% c=IN IP4 $
%% m=audio $ RTP/AVP 4
%% a=ptime:30
%% v=0
%% c=IN IP4 $
%% m=audio $ RTP/AVP 0
%%                   }
%%              }
%%           }
%%        }
%%     }
%% }
%%
%% NOTE - The MGC states its prefrred parameter values as a series of
%% sdp blocks in Local. The MG fills in the Local Descriptor in the
%% Reply.
%%----------------------------------------------------------------------

msg13() ->
    msg13(?MGC_MID).
msg13(Mid) ->
    AmmReq = #'AmmRequest'{terminationID = [#megaco_term_id{id = ?A4444}],
                           descriptors    = []},
    CmdReq = #'CommandRequest'{command = {addReq, AmmReq}},
    Jit = ?PP("nt/jit", "40"),
    LCD = #'LocalControlDescriptor'{streamMode = recvOnly,
                                    propertyParms = [Jit]},
    V = ?PP("v", "0"),
    C = ?PP("c", "IN IP4 $ "),
    M = ?PP("m", "audio $ RTP/AVP 4"),
    A = ?PP("a", "ptime:30"),
    V2 = ?PP("v", "0"),
    C2 = ?PP("c", "IN IP4 $ "),
    M2 = ?PP("m", "audio $ RTP/AVP 0"),
    LD = #'LocalRemoteDescriptor'{propGrps = [[V, C, M, A], [V2, C2, M2]]},
    Parms = #'StreamParms'{localControlDescriptor = LCD,
                           localDescriptor = LD},
    StreamDesc = #'StreamDescriptor'{streamID = 1,
                                     streamParms = Parms},
    MediaDesc = #'MediaDescriptor'{streams = {multiStream, [StreamDesc]}},
    ChooseTid = #megaco_term_id{contains_wildcards = true,
                                id = [[?megaco_choose]]},
    AmmReq2 = #'AmmRequest'{terminationID = [ChooseTid],
                            descriptors   = [{mediaDescriptor, MediaDesc}]},
    CmdReq2 = #'CommandRequest'{command = {addReq, AmmReq2}},
    request(Mid, 10003, ?megaco_choose_context_id, [CmdReq, CmdReq2]).

%%----------------------------------------------------------------------
%% 14. MG1 acknowledges the new Termination and fills in the Local IP
%%     address and UDP port. It also makes a choice for the codec based
%%     on the MGC preferences in Local. MG1 sets the RTP port to 2222.
%%
%% MEGACO/1 [124.124.124.222]:55555
%% Reply = 10003 {
%%    Context = 2000 {
%%       Add = A4444,
%%       Add=A4445{
%%          Media {
%%              Stream = 1 {
%%                  Local {
%% v=0
%% c=IN IP4 124.124.124.222
%% m=audio 2222 RTP/AVP 4
%% a=ptime:30
%% a=recvonly
%%                  } ; RTP profile for G.723 is 4
%%              }
%%          }
%%       }
%%    }
%% }
%%----------------------------------------------------------------------

msg14() ->
    msg14(?MG1_MID).
msg14(Mid) ->
    V = ?PP("v", "0"),
    C = ?PP("c", "IN IP4 124.124.124.222"),
    M = ?PP("m", "audio 2222 RTP/AVP 4"),
    A = ?PP("a", "a=ptime:30"),
    A2= ?PP("a", "recvonly"),
    LD = #'LocalRemoteDescriptor'{propGrps = [[V, C, M, A, A2]]},
    Parms = #'StreamParms'{localDescriptor = LD},
    StreamDesc = #'StreamDescriptor'{streamID = 1,
                                     streamParms = Parms},
    MediaDesc = #'MediaDescriptor'{streams = {multiStream, [StreamDesc]}},
    Reply = #'AmmsReply'{terminationID = [#megaco_term_id{id = ?A4444}]},
    Reply2 = #'AmmsReply'{terminationID = [#megaco_term_id{id = ?A4445}],
                          terminationAudit = [{mediaDescriptor, MediaDesc}]},
    reply(Mid, 10003, 2000, [{addReply, Reply}, {addReply, Reply2}]).

%%----------------------------------------------------------------------
%% 15. The MGC will now associate A5555 with a new Context on MG2, and
%%     establish an RTP Stream (i.e, A5556 will be assigned), SendReceive
%%     connection through to the originating user, User 1. The MGC also
%%     sets ring on A5555.
%%
%% MGC to MG2:
%% MEGACO/1 [123.123.123.4]:55555
%% Transaction = 50003 {
%%     Context = $ {
%%        Add = A5555  { Media {
%%             Stream = 1 {
%%                  LocalControl {Mode = SendReceive} }},
%%             Signals {al/ri}
%%             },
%%        Add  = $ {Media {
%%             Stream = 1 {
%%                  LocalControl {
%%                     Mode = SendReceive,
%%                     nt/jit=40 ; in ms
%%                  },
%%                  Local {
%% v=0
%% c=IN IP4 $
%% m=audio $ RTP/AVP 4
%% a=ptime:30
%%                  },
%%                  Remote {
%% v=0
%% c=IN IP4 124.124.124.222
%% m=audio 2222 RTP/AVP 4
%% a=ptime:30
%%                  } ; RTP profile for G.723 is 4
%%              }
%%           }
%%       }
%%    }
%% }
%%----------------------------------------------------------------------

msg15() ->
    msg15(?MGC_MID).
msg15(Mid) ->
    LCD = #'LocalControlDescriptor'{streamMode = sendRecv,
                                    propertyParms = []},
    Parms = #'StreamParms'{localControlDescriptor = LCD},
    StreamDesc = #'StreamDescriptor'{streamID = 1,
                                     streamParms = Parms},
    MediaDesc = #'MediaDescriptor'{streams = {multiStream, [StreamDesc]}},
    Signal = #'Signal'{signalName = "al/ri",
                       sigParList = []},
    AmmReq = #'AmmRequest'{terminationID = [#megaco_term_id{id = ?A5555}],
                           descriptors   = [{mediaDescriptor, MediaDesc},
                                            {signalsDescriptor, [{signal, Signal}]}]},
    CmdReq = #'CommandRequest'{command = {addReq, AmmReq}},
    Jit = ?PP("nt/jit", "40"),
    LCD2 = #'LocalControlDescriptor'{streamMode = sendRecv,
                                     propertyParms = [Jit]},
    V = ?PP("v", "0"),
    C = ?PP("c", "IN IP4 $ "),
    M = ?PP("m", "audio $ RTP/AVP 4"),
    A = ?PP("a", "ptime:30"),
    LD2 = #'LocalRemoteDescriptor'{propGrps = [[V, C, M, A]]},
    V2 = ?PP("v", "0"),
    C2 = ?PP("c", "IN IP4 124.124.124.222"),
    M2 = ?PP("m", "audio 2222 RTP/AVP 4"),
    RD2 = #'LocalRemoteDescriptor'{propGrps = [[V2, C2, M2]]},
    Parms2 = #'StreamParms'{localControlDescriptor = LCD2,
                            localDescriptor = LD2,
                            remoteDescriptor = RD2},
    StreamDesc2 = #'StreamDescriptor'{streamID = 1,
                                      streamParms = Parms2},
    MediaDesc2 = #'MediaDescriptor'{streams = {multiStream, [StreamDesc2]}},
    ChooseTid = #megaco_term_id{contains_wildcards = true,
                                id = [[?megaco_choose]]},
    AmmReq2 = #'AmmRequest'{terminationID = [ChooseTid],
                            descriptors   = [{mediaDescriptor, MediaDesc2}]},
    CmdReq2 = #'CommandRequest'{command = {addReq, AmmReq2}},
    request(Mid, 50003, ?megaco_choose_context_id, [CmdReq, CmdReq2]).

%%----------------------------------------------------------------------
%% 16. This is acknowledged. The stream port number is different from the
%%     control port number. In this case it is 1111 (in the SDP).
%%
%% MG2 to MGC:
%% MEGACO/1 [124.124.124.222]:55555
%% Reply = 50003 {
%%    Context = 5000 {
%%       Add = A5556{
%%          Media {
%%             Stream = 1 {
%%                 Local {
%% v=0
%% c=IN IP4 125.125.125.111
%% m=audio 1111 RTP/AVP 4
%% }
%%             } ; RTP profile for G723 is 4
%%          }
%%        }
%%    }
%% }
%%----------------------------------------------------------------------

msg16() ->
    msg16(?MG2_MID).
msg16(Mid) ->
    V = ?PP("v", "0"),
    C = ?PP("c", "IN IP4 125.125.125.111"),
    M = ?PP("m", "audio 1111 RTP/AVP 4"),
    LD = #'LocalRemoteDescriptor'{propGrps = [[V, C, M]]},
    Parms = #'StreamParms'{localDescriptor = LD},
    StreamDesc = #'StreamDescriptor'{streamID = 1,
                                     streamParms = Parms},
    MediaDesc = #'MediaDescriptor'{streams = {multiStream, [StreamDesc]}},
    Reply = #'AmmsReply'{terminationID = [#megaco_term_id{id = ?A5556}],
                         terminationAudit = [{mediaDescriptor, MediaDesc}]},
    reply(Mid, 50003, 5000, [{addReply, Reply}]).

%%----------------------------------------------------------------------
%% 17. The above IPAddr and UDPport need to be given to MG1 now.
%%
%% MGC to MG1:
%% MEGACO/1 [123.123.123.4]:55555
%% Transaction = 10005 {
%%   Context = 2000 {
%%     Modify = A4444 {
%%       Signals {cg/rt}
%%     },
%%     Modify = A4445 {
%%        Media {
%%             Stream = 1 {
%%                 Remote {
%% v=0
%% c=IN IP4 125.125.125.111
%% m=audio 1111 RTP/AVP 4
%%                 }
%%             } ; RTP profile for G723 is 4
%%         }
%%     }
%%   }
%% }
%%----------------------------------------------------------------------

msg17a() ->
    msg17a(?MGC_MID).
msg17a(Mid) ->
    Signal = #'Signal'{signalName = "cg/rt", sigParList = []},
    AmmReq = #'AmmRequest'{terminationID = [#megaco_term_id{id = ?A4444}],
                           descriptors   = [{signalsDescriptor, [{signal, Signal}]}]},
    CmdReq = #'CommandRequest'{command = {modReq, AmmReq}},

    V = ?PP("v", "0"),
    C = ?PP("c", "IN IP4 125.125.125.111"),
    M = ?PP("m", "audio 1111 RTP/AVP 4"),
    RD2 = #'LocalRemoteDescriptor'{propGrps = [[V, C, M]]},
    Parms2 = #'StreamParms'{remoteDescriptor = RD2},
    StreamDesc2 = #'StreamDescriptor'{streamID = 1,
                                      streamParms = Parms2},
    MediaDesc2 = #'MediaDescriptor'{streams = {multiStream, [StreamDesc2]}},
    AmmReq2 = #'AmmRequest'{terminationID = [#megaco_term_id{id = ?A4445}],
                            descriptors   = [{mediaDescriptor, MediaDesc2}]},
    CmdReq2 = #'CommandRequest'{command = {modReq, AmmReq2}},
    request(Mid, 10005, 2000, [CmdReq, CmdReq2]).

%%----------------------------------------------------------------------
%% MG1 to MGC:
%% MEGACO/1 [124.124.124.222]:55555
%% Reply = 10005 {
%%    Context = 2000 {Modify = A4444, Modify = A4445}
%% }
%%----------------------------------------------------------------------

msg17b() ->
    msg17b(?MG1_MID).
msg17b(Mid) ->
    Reply = #'AmmsReply'{terminationID = [#megaco_term_id{id = ?A4444}]},
    Reply2 = #'AmmsReply'{terminationID = [#megaco_term_id{id = ?A4445}]},
    reply(Mid, 10005, 2000, [{modReply, Reply}, {modReply, Reply2}]).

%%----------------------------------------------------------------------
%% 18. The two gateways are now connected and User 1 hears the
%%     RingBack. The MG2 now waits until User2 picks up the receiver and
%%     then the two-way call is established.
%%
%% MG2 to MGC:
%% MEGACO/1 [125.125.125.111]:55555
%% Transaction = 50005 {
%%    Context = 5000 {
%%        Notify = A5555 {ObservedEvents =1234 {
%%          19990729T22020002:al/of}}
%%    }
%% }
%%----------------------------------------------------------------------

msg18a() ->
    msg18a(?MG2_MID).
msg18a(Mid) ->
    TimeStamp = #'TimeNotation'{date = "19990729",
                                time = "22020002"},
    Event = #'ObservedEvent'{eventName = "al/of",
                             timeNotation = TimeStamp,
                             eventParList = []},
    Desc = #'ObservedEventsDescriptor'{requestId = 1234,
                                       observedEventLst = [Event]},
    NotifyReq = #'NotifyRequest'{terminationID = [#megaco_term_id{id = ?A5555}],
                                 observedEventsDescriptor = Desc},
    CmdReq = #'CommandRequest'{command = {notifyReq, NotifyReq}},
    request(Mid, 50005, 5000, [CmdReq]).

%%----------------------------------------------------------------------
%% MGC to MG2:
%% MEGACO/1 [123.123.123.4]:55555
%% Reply = 50005 {
%%     Context = - {Notify = A5555}
%% }
%%----------------------------------------------------------------------

msg18b() ->
    msg18b(?MGC_MID).
msg18b(Mid) ->
    Reply = #'NotifyReply'{terminationID = [#megaco_term_id{id = ?A5555}]},
    reply(Mid, 50005, ?megaco_null_context_id, [{notifyReply, Reply}]).

%%----------------------------------------------------------------------
%% MGC to MG2:
%% MEGACO/1 [123.123.123.4]:55555
%% Transaction = 50006 {
%%    Context = 5000 {
%%       Modify = A5555 {
%%          Events = 1235 {al/on},
%%          Signals { } ; to turn off ringing
%%       }
%%    }
%% }
%%----------------------------------------------------------------------

msg18c() ->
    msg18c(?MGC_MID).
msg18c(Mid) ->
    On = #'RequestedEvent'{pkgdName = "al/on", evParList = []},
    EventsDesc = #'EventsDescriptor'{requestID = 1235,
                                     eventList = [On]},
    AmmReq = #'AmmRequest'{terminationID = [#megaco_term_id{id = ?A5555}],
                           descriptors   = [{eventsDescriptor, EventsDesc},
                                            {signalsDescriptor, []}]},
    CmdReq = #'CommandRequest'{command = {modReq, AmmReq}},
    request(Mid, 50006, 5000, [CmdReq]).

%%----------------------------------------------------------------------
%% MG2 to MGC:
%% MEGACO/1 [125.125.125.111]:55555
%% Reply = 50006 {
%%  Context = 5000 {Modify = A4445}
%% }
%%----------------------------------------------------------------------

msg18d() ->
    msg18d(?MG2_MID).
msg18d(Mid) ->
    Reply = #'AmmsReply'{terminationID = [#megaco_term_id{id = ?A4445}]},
    reply(Mid, 50006, 5000, [{modReply, Reply}]).

%%----------------------------------------------------------------------
%% 19. Change mode on MG1 to SendReceive, and stop the ringback.
%%
%% MGC to MG1:
%% MEGACO/1 [123.123.123.4]:55555
%% Transaction = 10006 {
%%    Context = 2000 {
%%       Modify = A4445 {
%%          Media {
%%             Stream = 1 {
%%                LocalControl {
%%                   Mode=SendReceive
%%                }
%%             }
%%          }
%%       },
%%       Modify = A4444 {
%%          Signals { }
%%       }
%%    }
%% }
%%----------------------------------------------------------------------

msg19a() ->
    msg19a(?MGC_MID).
msg19a(Mid) ->
    LCD = #'LocalControlDescriptor'{streamMode = sendRecv,
                                    propertyParms = []},
    Parms = #'StreamParms'{localControlDescriptor = LCD},
    StreamDesc = #'StreamDescriptor'{streamID = 1,
                                     streamParms = Parms},
    MediaDesc = #'MediaDescriptor'{streams = {multiStream, [StreamDesc]}},
    AmmReq = #'AmmRequest'{terminationID = [#megaco_term_id{id = ?A4445}],
                           descriptors   = [{mediaDescriptor, MediaDesc}]},
    CmdReq = #'CommandRequest'{command = {modReq, AmmReq}},
    AmmReq2 = #'AmmRequest'{terminationID = [#megaco_term_id{id = ?A4444}],
                            descriptors   = [{signalsDescriptor, []}]},
    CmdReq2 = #'CommandRequest'{command = {modReq, AmmReq2}},
    request(Mid, 10006, 2000, [CmdReq, CmdReq2]).

%%----------------------------------------------------------------------
%% MG1 to MGC:
%% MEGACO/1 [124.124.124.222]:55555
%% Reply = 10006 {
%%    Context = 2000 {Modify = A4445, Modify = A4444}}
%%----------------------------------------------------------------------

msg19b() ->
    msg19b(?MG1_MID).
msg19b(Mid) ->
    Reply = #'AmmsReply'{terminationID = [#megaco_term_id{id = ?A4445}]},
    Reply2= #'AmmsReply'{terminationID = [#megaco_term_id{id = ?A4444}]},
    reply(Mid, 10006, 2000, [{modReply, Reply}, {modReply, Reply2}]).

%%----------------------------------------------------------------------
%% 20. The MGC decides to Audit the RTP termination on MG2.
%%
%% MEGACO/1 [123.123.123.4]:55555
%% Transaction = 50007 {
%%    Context = - {AuditValue = A5556{
%%       Audit{Media, DigitMap, Events, Signals, Packages, Statistics }}
%%    }
%% }
%%----------------------------------------------------------------------

msg20() ->
    msg20(?MGC_MID).
msg20(Mid) ->
    Tokens = [mediaToken, eventsToken, signalsToken,
              digitMapToken, statsToken, packagesToken],
    AuditDesc = #'AuditDescriptor'{auditToken = Tokens},
    Req = #'AuditRequest'{terminationID = #megaco_term_id{id = ?A5556},
                          auditDescriptor = AuditDesc},
    CmdReq = #'CommandRequest'{command = {auditValueRequest, Req}},
    request(Mid, 50007, ?megaco_null_context_id, [CmdReq]).

%%----------------------------------------------------------------------
%% 21. The MG2 replies. An RTP termination has no events nor signals, so
%%     these are left out in the reply .
%%
%% MEGACO/1 [125.125.125.111]:55555
%% Reply = 50007 {
%%    Context = - {
%% AuditValue = A5556 {
%%           Media {
%%              Stream = 1 {
%%                  LocalControl { Mode = SendReceive,
%%                     nt/jit=40 },
%%                  Local {
%% v=0
%% c=IN IP4 125.125.125.111
%% m=audio 1111 RTP/AVP  4
%% a=ptime:30
%%                 },
%%                  Remote {
%% v=0
%% c=IN IP4 124.124.124.222
%% m=audio 2222 RTP/AVP  4
%% a=ptime:30
%%                  } } },
%%           Packages {nt-1, rtp-1},
%%           Statistics { rtp/ps=1200,  ; packets sent
%%                        nt/os=62300, ; octets sent
%%                        rtp/pr=700, ; packets received
%%                        nt/or=45100, ; octets received
%%                        rtp/pl=0.2,  ; % packet loss
%%                        rtp/jit=20,
%%                        rtp/delay=40 } ; avg latency
%%        }
%%     }
%% }
%%----------------------------------------------------------------------

msg21() ->
    msg21(?MG2_MID).
msg21(Mid) ->
    Jit = ?PP("nt/jit", "40"),
    LCD = #'LocalControlDescriptor'{streamMode = sendRecv,
                                    propertyParms = [Jit]},
    LDV = ?PP("v", "0"),
    LDC = ?PP("c", "IN IP4 125.125.125.111"),
    LDM = ?PP("m", "audio 1111 RTP/AVP  4"),
    LDA = ?PP("a", "ptime:30"),
    LD  = #'LocalRemoteDescriptor'{propGrps = [[LDV, LDC, LDM, LDA]]},
    RDV = ?PP("v", "0"),
    RDC = ?PP("c", "IN IP4 124.124.124.222"),
    RDM = ?PP("m", "audio 2222 RTP/AVP  4"),
    RDA = ?PP("a", "ptime:30"),
    RD  = #'LocalRemoteDescriptor'{propGrps = [[RDV, RDC, RDM, RDA]]},
    StreamParms = #'StreamParms'{localControlDescriptor = LCD,
                                 localDescriptor = LD,
                                 remoteDescriptor = RD},
    StreamDesc = #'StreamDescriptor'{streamID = 1,
                                     streamParms = StreamParms},
    Media = #'MediaDescriptor'{streams = {multiStream, [StreamDesc]}},
    PackagesItem  = #'PackagesItem'{packageName = "nt",
                                    packageVersion = 1},
    PackagesItem2 = #'PackagesItem'{packageName = "rtp",
                                    packageVersion = 1},
    Stat  = ?SP("rtp/ps","1200"),
    Stat2 = ?SP("nt/os","62300"),
    Stat3 = ?SP("rtp/pr","700"),
    Stat4 = ?SP("nt/or","45100"),
    Stat5 = ?SP("rtp/pl","0.2"),
    Stat6 = ?SP("rtp/jit","20"),
    Stat7 = ?SP("rtp/delay","40"),
    Statistics = [Stat, Stat2, Stat3, Stat4, Stat5, Stat6, Stat7],
    Audits = [{mediaDescriptor, Media},
              {packagesDescriptor, [PackagesItem, PackagesItem2]},
              {statisticsDescriptor, Statistics}],
    Reply = {auditResult, #'AuditResult'{terminationID = #megaco_term_id{id = ?A5556},
					 terminationAuditResult = Audits}},
    reply(Mid, 50007, ?megaco_null_context_id, [{auditValueReply, Reply}]).

%%----------------------------------------------------------------------
%% 22. When the MGC receives an onhook signal from one of the MGs, it
%% brings down the call. In this example, the user at MG2 hangs up first.
%%
%% MG2 to MGC:
%% MEGACO/1 [125.125.125.111]:55555
%% Transaction = 50008 {
%%    Context = 5000 {
%%        Notify = A5555 {ObservedEvents =1235 {
%%           19990729T24020002:al/on}
%%        }
%%    }
%% }
%%----------------------------------------------------------------------

msg22a() ->
    msg22a(?MG2_MID).
msg22a(Mid) ->
    TimeStamp = #'TimeNotation'{date = "19990729",
                                time = "24020002"},
    Event = #'ObservedEvent'{eventName = "al/on",
                             timeNotation = TimeStamp,
                             eventParList = []},
    Desc = #'ObservedEventsDescriptor'{requestId = 1235,
                                       observedEventLst = [Event]},
    NotifyReq = #'NotifyRequest'{terminationID = [#megaco_term_id{id = ?A5555}],
                                 observedEventsDescriptor = Desc},
    CmdReq = #'CommandRequest'{command = {notifyReq, NotifyReq}},
    request(Mid, 50008, 5000, [CmdReq]).

%%----------------------------------------------------------------------
%% MGC to MG2:
%% MEGACO/1 [123.123.123.4]:55555
%% Reply = 50008 {
%%     Context = - {Notify = A5555}
%% }
%%----------------------------------------------------------------------

msg22b() ->
    msg22b(?MGC_MID).
msg22b(Mid) ->
    Reply = #'NotifyReply'{terminationID = [#megaco_term_id{id = ?A5555}]},
    reply(Mid, 50008, ?megaco_null_context_id, [{notifyReply, Reply}]).

%%----------------------------------------------------------------------
%% 23. The MGC now sends both MGs a Subtract to take down the call. Only
%%     the subtracts to MG2 are shown here. Each termination has its own
%%     set of statistics that it gathers. An MGC may not need to request
%%     both to be returned. A5555 is a physical termination, and A5556 is
%%     an RTP termination.
%%
%% MGC to MG2:
%%
%% MEGACO/1 [123.123.123.4]:55555
%% Transaction = 50009 {
%%    Context = 5000 {
%%       Subtract = A5555 {Audit{Statistics}},
%%       Subtract = A5556 {Audit{Statistics}}
%%    }
%% }
%%----------------------------------------------------------------------

msg23a() ->
    msg23a(?MGC_MID).
msg23a(Mid) ->
    CommonAuditDesc = #'AuditDescriptor'{auditToken = [statsToken]},
    SubReq = #'SubtractRequest'{terminationID = [#megaco_term_id{id = ?A5555}],
                                auditDescriptor = CommonAuditDesc},
    SubReq2 = #'SubtractRequest'{terminationID = [#megaco_term_id{id = ?A5556}],
                                 auditDescriptor = CommonAuditDesc},
    CmdReq = #'CommandRequest'{command = {subtractReq, SubReq}},
    CmdReq2 = #'CommandRequest'{command = {subtractReq, SubReq2}},
    request(Mid, 50009, 5000, [CmdReq, CmdReq2]).
%%
%%----------------------------------------------------------------------
%% MG2 to MGC:
%%
%% MEGACO/1 [125.125.125.111]:55555
%% Reply = 50009 {
%%    Context = 5000 {
%%      Subtract = A5555 {
%%           Statistics {
%%              nt/os=45123, ; Octets Sent
%%              nt/dur=40 ; in seconds
%%              }
%%        },
%%        Subtract = A5556 {
%%           Statistics {
%%              rtp/ps=1245, ; packets sent
%%              nt/os=62345, ; octets sent
%%              rtp/pr=780, ; packets received
%%              nt/or=45123, ; octets received
%%              rtp/pl=10, ;  % packets lost
%%              rtp/jit=27,
%%              rtp/delay=48 ; average latency
%%           }
%%        }
%%    }
%% }
%%----------------------------------------------------------------------

msg23b() ->
    msg23b(?MG2_MID).
msg23b(Mid) ->
    Stat11 = ?SP("nt/os","45123"),
    Stat12 = ?SP("nt/dur", "40"),
    Stats1 = [Stat11, Stat12],
    Reply1 = #'AmmsReply'{terminationID = [#megaco_term_id{id = ?A5555}],
                          terminationAudit = [{statisticsDescriptor, Stats1}]},
    Stat21 = ?SP("rtp/ps","1245"),
    Stat22 = ?SP("nt/os", "62345"),
    Stat23 = ?SP("rtp/pr", "780"),
    Stat24 = ?SP("nt/or", "45123"),
    Stat25 = ?SP("rtp/pl", "10"),
    Stat26 = ?SP("rtp/jit", "27"),
    Stat27 = ?SP("rtp/delay","48"),
    Stats2 = [Stat21, Stat22, Stat23, Stat24, Stat25, Stat26, Stat27],
    Reply2 = #'AmmsReply'{terminationID = [#megaco_term_id{id = ?A5556}],
                          terminationAudit = [{statisticsDescriptor, Stats2}]},
    reply(Mid, 50009, 5000, [{subtractReply, Reply1}, {subtractReply, Reply2}]).

%%----------------------------------------------------------------------
%% 24. The MGC now sets up both MG1 and MG2 to be ready to detect the
%%     next off-hook event. See step 1. Note that this could be the
%%     default state of a termination in the null context, and if this
%%     were the case, no message need be sent from the MGC to the
%%     MG. Once a termination returns to the null context, it goes back
%%     to the default termination values for that termination.
%%----------------------------------------------------------------------

%% BUGBUG: Example missing in spec

%%----------------------------------------------------------------------
%% Testing
%%----------------------------------------------------------------------

messages() ->
    [{Slogan, catch ?MODULE:Slogan()} || Slogan <- names()].

encoders() ->
    [
     {megaco_pretty_text_encoder,  [], 	         []},
     {megaco_compact_text_encoder, [], 	         []},
     {megaco_binary_encoder,       [], 	         [native]},
     {megaco_ber_encoder,          [],           [native]},
     {megaco_per_encoder,          [], 	         [native]},
     {megaco_erl_dist_encoder,     [],      	 []},
     {megaco_erl_dist_encoder,     [compressed], [compressed]}
    ].


pretty_mod({Mod, Opt, _Opt2}) ->
    case Mod of
	megaco_pretty_text_encoder   when Opt == [flex] -> pretty_flex;
	megaco_compact_text_encoder  when Opt == [flex] -> compact_flex;
	megaco_pretty_text_encoder  -> pretty_text;
	megaco_compact_text_encoder -> compact_text;
	megaco_binary_encoder       -> asn1_ber;
	megaco_ber_encoder          -> asn1_ber_old;
	megaco_per_encoder          -> asn1_per;
	megaco_erl_dist_encoder when Opt == []           -> standard_erl;
	megaco_erl_dist_encoder when Opt == [compressed] -> compressed_erl;
	Ugly                        -> Ugly
    end.

%%----------------------------------------------------------------------
%% Run specific encoder for all test cases
%%----------------------------------------------------------------------

pretty_text() ->
    Default = [],
    Encoder = {megaco_pretty_text_encoder, Default, Default},
    All = [encode(Slogan, Msg, Encoder) || {Slogan, Msg} <- messages()],
    compute_res(All).

compact_text() ->
    Default = [],
    Encoder = {megaco_compact_text_encoder, Default, Default},
    All = [encode(Slogan, Msg, Encoder) || {Slogan, Msg} <- messages()],
    compute_res(All).

pretty_flex() ->
    Default = [flex],
    Encoder = {megaco_pretty_text_encoder, Default, Default},
    All = [encode(Slogan, Msg, Encoder) || {Slogan, Msg} <- messages()],
    compute_res(All).

compact_flex() ->
    Default = [flex],
    Encoder = {megaco_compact_text_encoder, Default, Default},
    All = [encode(Slogan, Msg, Encoder) || {Slogan, Msg} <- messages()],
    compute_res(All).

bin() ->
    Default = [],
    Native = [native],
    Encoder = {megaco_binary_encoder, Default, Native},
    All = [encode(Slogan, Msg, Encoder) || {Slogan, Msg} <- messages()],
    compute_res(All).

asn1_ber() ->
    Default = [],
    Native = [native],
    Encoder = {megaco_ber_encoder, Default, Native},
    All = [encode(Slogan, Msg, Encoder) || {Slogan, Msg} <- messages()],
    compute_res(All).

asn1_per() ->
    Default = [],
    Native = [native],
    Encoder = {megaco_per_encoder, Default, Native},
    All = [encode(Slogan, Msg, Encoder) || {Slogan, Msg} <- messages()],
    compute_res(All).

standard_erl() ->
    Config = [],
    Encoder = {megaco_erl_dist_encoder, Config, Config},
    All = [encode(Slogan, Msg, Encoder) || {Slogan, Msg} <- messages()],
    compute_res(All).

compressed_erl() ->
    Config = [compressed],
    Encoder = {megaco_erl_dist_encoder, Config, Config},
    All = [encode(Slogan, Msg, Encoder) || {Slogan, Msg} <- messages()],
    compute_res(All).

encode(Slogan, DecodedMsg, {Mod, Opt, _Opt2} = _Encoder) ->
    Main = "==================================================",
    Sub  = "--------------------------------------------------",
    case catch Mod:encode_message(Opt, DecodedMsg) of
        {ok, EncodedMsg} when is_binary(EncodedMsg) ->
            Sz = size(EncodedMsg),
            ok = io:format("~w ~s ~w bytes~n",
                           [Slogan, Main, Sz]),
            catch io:format("~n~s~n", [(catch binary_to_list(EncodedMsg))]),
            case catch Mod:decode_message(Opt, EncodedMsg) of
                {ok, ReDecodedMsg} ->
                    fmt(Slogan, DecodedMsg, ReDecodedMsg);
                {error, {Line, _, Reason}} when is_integer(Line)->
                    ?ERROR([{Slogan, Line, decode_failed}, {error,  Reason}, {encoded, EncodedMsg}, DecodedMsg]),
                    io:format("~n~w <ERROR> #~w: ~w~n",
                              [Slogan, Line, Reason]);
                Other ->
                    ?ERROR([{Slogan, 0, decode_failed}, Other, {encoded, EncodedMsg}, DecodedMsg]),
                    io:format("~n~w <ERROR> ~w~n", [Slogan, Other])
            end,
            Sz;
        Other ->
            ?ERROR([{Slogan, encode_failed}, Other, DecodedMsg]),
            ok = io:format("~w ~s~n~p~n<ERROR> ~s~n~p~n",
                           [Slogan, Main, DecodedMsg, Sub, Other]),
            {Slogan, {encode_message, Other}}
    end.

fmt(_Slogan, Msg, Msg) ->
    ok;
fmt(Slogan, {'MegacoMessage', A, {'Message', V, MID, {transactions, [{T, Old}]}}},
    {'MegacoMessage', A, {'Message', V, MID, {transactions, [{T, New}]}}}) ->
    fmt(Slogan, Old, New);
fmt(Slogan, Old, New) ->
    PrettyOld = lists:flatten(io_lib:format("~p", [Old])),
    PrettyNew = lists:flatten(io_lib:format("~p", [New])),
    fmt_diff(Slogan, Old, New, PrettyOld, PrettyNew, []).

fmt_diff(Slogan, Old, New, [H |  OldRest], [H |  NewRest], Common) ->
    fmt_diff(Slogan, Old, New, OldRest, NewRest, [H | Common]);
fmt_diff(Slogan, Old, New, OldRest, NewRest, Common) ->
    RevCommon = lists:reverse(Common),
    ?ERROR([{Slogan, decode_mismatch}, {old, Old}, {new, New}]),
    Sub = "--------------------------------------------------",
    io:format("~n~w COMMON ~s~n~s~n", [Slogan, Sub, RevCommon]),
    io:format("~n~w OLD    ~s~n~s~n", [Slogan, Sub, OldRest]),
    io:format("~n~w NEW    ~s~n~s~n", [Slogan, Sub, NewRest]).

compute_res(All) ->
    compute_res(All, [], 0).

compute_res([H | T], Bad, Sum) when is_integer(H) ->
    compute_res(T, Bad, Sum + H);
compute_res([H | T], Bad, Sum) ->
    compute_res(T, [H | Bad], Sum);
compute_res([], Bad, Sum) ->
    ok = io:format("#bytes: ~w; errors: ~p~n", [Sum, Bad]).

%%----------------------------------------------------------------------
%% Compute sizes of encoded messages
%%----------------------------------------------------------------------

msg_sizes() ->
    Encoders = encoders(),
    msg_sizes(Encoders).

%% Returns a list of {MessageSlogan, MessageSizes} where
%% MessageSizes is the result from msg_sizes/2
msg_sizes(Encoders) ->
    [{S, msg_sizes(Msg, Encoders)} ||  {S, Msg} <- messages()].

%% Returns a list of {Encoder, Res} tuples
%% where Res either is the message size (integer)
%% or an error atom
msg_sizes(DecodedMsg, Encoders) ->
    [abs_msg_size(DecodedMsg, E) || E <- Encoders].

abs_msg_size(DecodedMsg, {Mod, Opt, _Opt2} = Encoder) ->
    case catch Mod:encode_message(Opt, DecodedMsg) of
        {ok, EncodedMsg} when is_binary(EncodedMsg) ->
	    {Encoder, size(EncodedMsg)};
        Error ->
            {Encoder, {bad_encoder, Error}}
    end.

%%----------------------------------------------------------------------
%% Compute time for encoding messages
%%----------------------------------------------------------------------

encoding_times() ->
    Encoders = encoders(),
    encoding_times(Encoders).

%% Returns a list of {MessageSlogan, EncodingTimes} where
%% EncodingTimes is the result from encoding_times/2
encoding_times(Encoders) ->
    [{Slogan, encoding_times(Msg, Encoders)} ||  {Slogan, Msg} <- messages()].

%% Returns a list of {Encoder, Res} tuples
%% where Res either is the encoding time (integer)
%% or an error atom
encoding_times(DecodedMsg, Encoders) ->
    [{E, encoding_time(encoding_msg(DecodedMsg, E))} || E <- Encoders].

encoding_msg(DecodedMsg, {Mod, Opt, Opt2} = Encoder) ->
    {ok, EncodedMsg}  = Mod:encode_message(Opt,  DecodedMsg),
    {ok, DecodedMsg2} = Mod:decode_message(Opt2, EncodedMsg),
    {Encoder, DecodedMsg2}.

encoding_time({{Mod, _Opt, Opt2} = Encoder, DecodedMsg}) ->
    meter(fun() -> {ok, _} = Mod:encode_message(Opt2, DecodedMsg) end, Encoder).

%%----------------------------------------------------------------------
%% Compute time for decoding messages
%%----------------------------------------------------------------------

decoding_times() ->
    Decoders = encoders(),
    decoding_times(Decoders).

%% Returns a list of {MessageSlogan, DecodingTimes} where
%% DecodingTimes is the result from decoding_times/2
decoding_times(Decoders) ->
    [{Slogan, decoding_times(Msg, Decoders)} ||  {Slogan, Msg} <- messages()].

%% Returns a list of {Decoder, Res} tuples
%% where Res either is the decoding time (integer)
%% or an error atom
decoding_times(DecodedMsg, Encoders) ->
    [{E, decoding_time(decoding_msg(DecodedMsg, E))} || E <- Encoders].

decoding_msg(DecodedMsg, {Mod, Opt, _Opt2} = Encoder) ->
    {ok, EncodedMsg} = Mod:encode_message(Opt, DecodedMsg),
    {Encoder, EncodedMsg}.

decoding_time({{Mod, _Opt, Opt2} = Encoder, EncodedMsg}) ->
    meter(fun() -> {ok, _} = Mod:decode_message(Opt2, EncodedMsg) end, Encoder).

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------

coding_times() ->
    Coders = encoders(),
    coding_times(Coders).

%% Returns a list of {MessageSlogan, DecodingTimes} where
%% DecodingTimes is the result from decoding_times/2
coding_times(Coders) ->
    [{Slogan, coding_times(Msg, Coders)} ||  {Slogan, Msg} <- messages()].

%% Returns a list of {Decoder, Res} tuples
%% where Res either is the decoding time (integer)
%% or an error atom
coding_times(DecodedMsg, Coders) ->
    [{E, coding_time(coding_msg(DecodedMsg, E))} || E <- Coders].

coding_msg(DecodedMsg, {Mod, Opt, _Opt2} = Encoder) ->
    {ok, EncodedMsg} = Mod:encode_message(Opt, DecodedMsg),
    {Encoder, EncodedMsg}.

coding_time({{Mod, _Opt, Opt2} = Encoder, EncodedMsg}) ->
    Fun = fun() ->
		  {ok, DecodedMsg} = Mod:decode_message(Opt2, EncodedMsg),
		  {ok, _}          = Mod:encode_message(Opt2, DecodedMsg)
	  end,
    meter(Fun, Encoder).

%%----------------------------------------------------------------------
%% Return size statistics as term
%%----------------------------------------------------------------------

size_stat() ->
    Encoders = encoders(),
    MsgSizes = msg_sizes(Encoders),
    stat(Encoders, MsgSizes).

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------

gnuplot_gif() ->
    [
     {size,     gnuplot_size_gif()},
     {encoding, gnuplot_enc_time_gif()},
     {decoding, gnuplot_dec_time_gif()},
     {coding,   gnuplot_code_time_gif()}
    ].

%%----------------------------------------------------------------------
%% Generate GIF picture from size statistics with gnuplot
%%----------------------------------------------------------------------

gnuplot_size_gif() ->
    {ok, _Cwd} = file:get_cwd(),
    TmpDir = "megaco_encoded_size.tmp",
    GifFile = "megaco_encoded_size.gif",
    Header =
        ["set title \"Size comparison of Megaco/H.248 encoding formats\"\n",
         "set timestamp top\n",
         "set terminal gif\n",
         "set xlabel \"Test cases from Appendix A\"\n",
         "set ylabel \"Message size in bytes\"\n",
         "set rmargin 10\n",
         "set key left top Left\n",
         "set output \"", GifFile, "\"\n\n"],
    Encoders = encoders(),
    Stat = msg_sizes(Encoders),
    BatchFile = gnuplot_dir(TmpDir, Header, Encoders, Stat),
    Cmd = "cd " ++ TmpDir ++ "; gnuplot " ++ BatchFile,
    os:cmd(Cmd),
    ok = io:format("~n~s~nxv ~s~n", [Cmd, filename:join([TmpDir, GifFile])]),
    stat(Encoders, Stat).

%%----------------------------------------------------------------------
%% Return encoding time statistics as term
%%----------------------------------------------------------------------

encoding_times_stat() ->
    Encoders = encoders(),
    EncodingTimes = encoding_times(Encoders),
    stat(Encoders, EncodingTimes).

%%----------------------------------------------------------------------
%% Return encoding time statistics as term
%%----------------------------------------------------------------------

decoding_times_stat() ->
    Encoders = encoders(),
    DecodingTimes = decoding_times(Encoders),
    stat(Encoders, DecodingTimes).

%%----------------------------------------------------------------------
%% Return encoding time statistics as term
%%----------------------------------------------------------------------

coding_times_stat() ->
    Encoders = encoders(),
    CodingTimes = coding_times(Encoders),
    stat(Encoders, CodingTimes).

%%----------------------------------------------------------------------
%% Generate GIF picture from encoding time statistics with gnuplot
%%----------------------------------------------------------------------

gnuplot_enc_time_gif() ->
    {ok, _Cwd} = file:get_cwd(),
    TmpDir = "megaco_encoding_time.tmp",
    GifFile = "megaco_encoding_time.gif",
    Header =
        ["set title \"Encoding time comparison of Megaco/H.248 encoding formats\"\n",
         "set timestamp top\n",
         "set terminal gif\n",
         "set xlabel \"Test cases from Appendix A\"\n",
         "set ylabel \"Time for encoding in micro seconds\"\n",
         "set rmargin 10\n",
         "set key left top Left\n",
         "set output \"", GifFile, "\"\n\n"],
    Encoders = encoders(),
    Stat = encoding_times(Encoders),
    BatchFile = gnuplot_dir(TmpDir, Header, Encoders, Stat),
    Cmd = "cd " ++ TmpDir ++ "; gnuplot " ++ BatchFile,
    os:cmd(Cmd),
    ok = io:format("~n~s~nxv ~s~n", [Cmd, filename:join([TmpDir, GifFile])]),
    stat(Encoders, Stat).

%%----------------------------------------------------------------------
%% Generate GIF picture from decoding time statistics with gnuplot
%%----------------------------------------------------------------------

gnuplot_dec_time_gif() ->
    {ok, _Cwd} = file:get_cwd(),
    TmpDir = "megaco_decoding_time.tmp",
    GifFile = "megaco_decoding_time.gif",
    Header =
        ["set title \"Decoding time comparison of Megaco/H.248 encoding formats\"\n",
         "set timestamp top\n",
         "set terminal gif\n",
         "set xlabel \"Test cases from Appendix A\"\n",
         "set ylabel \"Time for decoding in micro seconds\"\n",
         "set rmargin 10\n",
         "set key left top Left\n",
         "set output \"", GifFile, "\"\n\n"],
    Encoders = encoders(),
    Stat = decoding_times(Encoders),
    BatchFile = gnuplot_dir(TmpDir, Header, Encoders, Stat),
    Cmd = "cd " ++ TmpDir ++ "; gnuplot " ++ BatchFile,
    os:cmd(Cmd),
    ok = io:format("~n~s~nxv ~s~n", [Cmd, filename:join([TmpDir, GifFile])]),
    stat(Encoders, Stat).

%%----------------------------------------------------------------------
%% Generate GIF picture from decoding time statistics with gnuplot
%%----------------------------------------------------------------------

gnuplot_code_time_gif() ->
    {ok, _Cwd} = file:get_cwd(),
    TmpDir = "megaco_coding_time.tmp",
    GifFile = "megaco_coding_time.gif",
    Header =
        ["set title \"Encoding + decoding time comparison of Megaco/H.248 encoding formats\"\n",
         "set timestamp top\n",
         "set terminal gif\n",
         "set xlabel \"Test cases from Appendix A\"\n",
         "set ylabel \"Time for encoding+decoding in micro seconds\"\n",
         "set rmargin 10\n",
         "set key left top Left\n",
         "set output \"", GifFile, "\"\n\n"],
    Encoders = encoders(),
    Stat = coding_times(Encoders),
    BatchFile = gnuplot_dir(TmpDir, Header, Encoders, Stat),
    Cmd = "cd " ++ TmpDir ++ "; gnuplot " ++ BatchFile,
    os:cmd(Cmd),
    ok = io:format("~n~s~nxv ~s~n", [Cmd, filename:join([TmpDir, GifFile])]),
    stat(Encoders, Stat).

%%----------------------------------------------------------------------
%% Encode asn.1 messages
%%----------------------------------------------------------------------

gen_byte_msg(Msg, {Mod, Opt, _Opt2} = _Encoder) ->
    {ok, EncodedMsg} = Mod:encode_message(Opt, Msg),
    EncodedMsg.

%%----------------------------------------------------------------------
%% Gen the C header file content as a binary
%%----------------------------------------------------------------------

gen_header_file_binary([]) ->
    ok;
gen_header_file_binary([{S, B}| Rest]) ->
    file:write_file(atom_to_list(S), B),
    gen_header_file_binary(Rest).

%%----------------------------------------------------------------------
%% Generate headerfile for asn.1 BER test in C
%%----------------------------------------------------------------------

gen_ber_header() ->
    Encoder = {megaco_ber_encoder, [], []},
    L = [{S, gen_byte_msg(Msg, Encoder)} ||  {S, Msg} <- messages()],
    gen_header_file_binary(L).

%%----------------------------------------------------------------------
%% Generate headerfile for asn.1 BER test in C
%%----------------------------------------------------------------------
gen_ber_bin_header() ->
    Encoder = {megaco_ber_encoder, [], []},
    L = [{S, gen_byte_msg(Msg, Encoder)} ||  {S, Msg} <- messages()],
    gen_header_file_binary(L).

%%----------------------------------------------------------------------
%% Generate headerfile for asn.1 PER test in C
%%----------------------------------------------------------------------
gen_per_header() ->
    Encoder = {megaco_per_encoder, [], []},
    L = [{S, gen_byte_msg(Msg, Encoder)} ||  {S, Msg} <- messages()],
    gen_header_file_binary(L).

%%----------------------------------------------------------------------
%% Execute a fun a number of times and return avg in millis
%%----------------------------------------------------------------------

meter(Fun, Encoder) ->
    MaxTime = timer:seconds(5),
    Rep = 2,
    M = pretty_mod(Encoder),
    io:format("~p:\t", [M]),
    Times  = [single_meter(Fun, MaxTime, {M, N}) || N <- lists:seq(1, Rep)],
    Min = lists:min(Times),
    Max = lists:max(Times),
    Median = lists:nth((Rep + 1) div 2, lists:sort(Times)),
    io:format("(median=~p, diff=~p)~n", [Median, Max - Min]),
    Median.

single_meter(Fun, MaxTime, Tag) ->
    Pid = spawn_link(?MODULE, single_meter, [self(), Fun, MaxTime, Tag]),
    receive
	{meter, Pid, Time} ->
	    io:format("~p \t", [Time]),
	    Time;
	{'EXIT', Pid, Reason} ->
	    {bad_result, Reason}
    end.

single_meter(Parent, Fun, Expected, _Tag) ->
    erlang:statistics(runtime),
    erlang:send_after(Expected, self(), return_count),
    Count = count(Fun, 1),
    {_, Actual} = erlang:statistics(runtime),
    %% Diff = Actual - Expected,
    Micros = trunc((Actual / Count) * 1000),
    Parent ! {meter, self(), Micros},
    unlink(Parent),
    exit(normal).

count(Fun, N) ->
    Fun(),
    receive
	return_count ->
	    N
    after 0 ->
	    count(Fun, N + 1)
    end.

stat(Encoders, Stat) ->
    Fun =
	fun({_Mod, _Opt, _Opt2} = E) ->
		List = lists:flatten([[Val || {E2, Val} <- Info, E2 == E] ||
					 {_Slogan, Info} <- Stat]),
		Max = lists:max(List),
		Min = lists:min(List),
		case catch lists:sum(List) of
		    {'EXIT', _} ->
			{E, bad, List};
		    Sum when is_integer(Sum) ->
			N   = length(List),
			{E, [{min, Min}, {avg, Sum div N}, {max, Max}]}
		end
	end,
    lists:map(Fun, Encoders).

gnuplot_dir(Dir, Header, Encoders, Stat) ->
    file:make_dir(Dir),
    {Names, Arrows} = gnuplot_data(Encoders, Dir, Stat, 1, [], []),
    [H | T] = [lists:concat(["\"", N, "\" with linespoints"]) || N <- Names],
    Plots = [[", ", Plot] || Plot <- T],
    IoList = [Header, Arrows, "\nplot ", H, Plots, "\n\nshow output\n"],
    Bin = list_to_binary(IoList),
    BatchFile = "gnuplot.batch",
    file:write_file(filename:join(Dir, BatchFile), Bin),
    BatchFile.

gnuplot_data([], _Dir, _Stat, _Pos, Names, Arrows) ->
    {lists:reverse(Names), lists:reverse(Arrows)};
gnuplot_data([{Mod, _Opt, _Opt2} = E | Encoders], Dir, Stat, Pos, Names, Arrows) ->
    Plot = fun({Msg, List}, {N, AccSz}) ->
                   {value, {_, Sz}} = lists:keysearch(E, 1, List),
                   ActualSz =
                       if
                           is_integer(Sz) ->
                               Sz;
                           true        ->
                               ok = io:format("<ERROR> ~p(~p) -> ~p~n",
                                              [Mod, Msg, Sz]),
                               0
                       end,
                   Acc = {N + 1, [ActualSz | AccSz]},
                   {lists:concat([N + 1, " ", ActualSz, "\n"]), Acc}
           end,
    {Data, {N, Sizes}} = lists:mapfoldl(Plot, {0, []}, Stat),
    Min = lists:min(Sizes),
    Max = lists:max(Sizes),
    Sum = lists:sum(Sizes),
    Avg = Sum div N,
    Len = length(Stat),
    Pretty = pretty_mod(E),
    Name = lists:concat([Pretty, " (", Min, ",", Avg, ",", Max, ")"]),
    file:write_file(filename:join(Dir, Name), list_to_binary(Data)),
    %%"plot \"-\" title \"", E, "\" with linespoints\n", Data, "e\n",
    Arrow =
	lists:concat(["set arrow from 1,", Avg,
		      " to ", Len, ", ", Avg,
		      " nohead lt ", Pos, "\n",
		      "set label \" ", Avg, " (avg)\" at ", Len, ",", Avg + 10, "\n"]),
    gnuplot_data(Encoders, Dir, Stat, Pos + 1, [Name | Names], [Arrow | Arrows]).
