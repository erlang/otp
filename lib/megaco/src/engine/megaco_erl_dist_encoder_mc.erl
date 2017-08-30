%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
%% Purpose: Externalize/internalize Megaco/H.248 messages
%%----------------------------------------------------------------------

-module(megaco_erl_dist_encoder_mc).

-behaviour(megaco_edist_compress).

-export([
	 encode/1, encode/2, 
         decode/1, decode/2
	]).


-include("megaco_message_internal.hrl").
-include_lib("megaco/src/app/megaco_internal.hrl").


%%----------------------------------------------------------------------
%% Megaco compress a Megaco record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------

encode(M) -> 
    e(M, 1).

encode(M, Vsn) ->
    ?d("encode -> entry with"
       "~n   M:   ~p"
       "~n   Vsn: ~p", [M, Vsn]),
    Result = e(M, Vsn),
    ?d("encode -> "
       "~n   Result: ~p", [Result]),
    Result.

decode(M) -> 
    d(M, 1).

decode(M, Vsn) ->
    ?d("decode -> entry with"
       "~n   M:   ~p"
       "~n   Vsn: ~p", [M, Vsn]),
    Result = d(M, Vsn),
    ?d("decode -> "
       "~n   Result: ~p", [Result]),
    Result.


el(L,  V)  when is_list(L) -> [e(T, V) || T <- L];
el(L, _V)                  -> L.
dl(L,  V)  when is_list(L) -> [d(T, V) || T <- L];
dl(L, _V)                  -> L.

ell(L,  V) when is_list(L) -> [el(T, V) || T <- L];
ell(L, _V)                 -> L.
dll(L,  V) when is_list(L) -> [dl(T, V) || T <- L];
dll(L, _V)                 -> L.

e(asn1_NOVALUE, _) ->
    {1};
e('NULL', _V) ->
    {2};
e(sendRecv, _V) ->
    {3};
e(recvOnly, _V) ->
    {4};
e(restart, _V) ->
    {5};
e(mediaToken, _V) ->
    {6};
e(eventsToken, _V) ->
    {7};
e(signalsToken, _V) ->
    {8};
e(digitMapToken, _V) ->
    {9};
e(statsToken, _V) ->
    {10};
e(packagesToken, _V) ->
    {11};
e(h221, _V) ->
    {12};
e(h223, _V) ->
    {13};
e(h226, _V) ->
    {14};
e(v76, _V) ->
    {15};

e({'MegacoMessage', asn1_NOVALUE, {'Message', 1 = V, Mid, Body}}, _) ->
    {20, e(Mid, V), e(Body, V)};
e({'MegacoMessage', asn1_NOVALUE, {'Message', 2 = V, Mid, Body}}, _) ->
    {21, e(Mid, V), e(Body, V)};
e({'MegacoMessage', asn1_NOVALUE, {'Message', V, Mid, Body}}, _) ->
    {22, V, e(Mid, V), e(Body, V)};
e({'MegacoMessage', AuthHeader, {'Message', 1 = V, Mid, Body}}, _) ->
    {23, e(AuthHeader, V), V, e(Mid, V), e(Body, V)};
e({'MegacoMessage', AuthHeader, {'Message', 2 = V, Mid, Body}}, _) ->
    {24, e(AuthHeader, V), V, e(Mid, V), e(Body, V)};
e({'MegacoMessage', AuthHeader, {'Message', V, Mid, Body}}, _) ->
    {25, V, e(AuthHeader, V), V, e(Mid, V), e(Body, V)};
e({'MegacoMessage', AuthHeader, Mess}, V) ->
    {26, e(AuthHeader, V), e(Mess, V)};
e({'Message', V, Mid, Body}, _) ->
    {27, V, e(Mid, V), e(Body, V)};

e({domainName, {'DomainName', Name, asn1_NOVALUE}}, _V) ->
    {30, Name};
e({domainName, {'DomainName', Name, PortNumber}}, _V) ->
    {31, Name, PortNumber};
e({domainName, N}, V) ->
    {32, e(N, V)};
e({'DomainName', Name, asn1_NOVALUE}, _V) ->
    {33, Name};
e({'DomainName', Name, PortNumber}, _V) ->
    {34, Name, PortNumber};
e({ip4Address, {'IP4Address', Addr, asn1_NOVALUE}}, _V) ->
    {35, Addr};
e({ip4Address, {'IP4Address', Addr, PortNumber}}, _V) ->
    {36, Addr, PortNumber};
e({ip4Address, A}, V) ->
    {37, e(A, V)};
e({'IP4Address', Addr, asn1_NOVALUE}, _V) ->
    {38, Addr};
e({'IP4Address', Addr, PortNumber}, _V) ->
    {39, Addr, PortNumber};
e({ip6Address, {'IP6Address', Addr, asn1_NOVALUE}}, _V) ->
    {40, Addr};
e({ip6Address, {'IP6Address', Addr, PortNumber}}, _V) ->
    {41, Addr, PortNumber};
e({ip6Address, A}, V) ->
    {42, e(A, V)};
e({'IP6Address', Addr, asn1_NOVALUE}, _V) ->
    {43, Addr};
e({'IP6Address', Addr, PortNumber}, _V) ->
    {44, Addr, PortNumber};

e({transactions, [Transaction]}, V) ->
    {50, e(Transaction, V)};
e({transactions, Transactions}, V) ->
    {51, el(Transactions, V)};
e({messageError, {'ErrorDescriptor', EC, asn1_NOVALUE}}, _V) ->
    {52, EC};
e({messageError, {'ErrorDescriptor', EC, ET}}, _V) ->
    {53, EC, ET};
e({messageError, Error}, V) ->
    {54, e(Error, V)};
e({transactionRequest, {'TransactionRequest', TransId, Actions}}, V) ->
    {55, TransId, el(Actions, V)};
e({transactionPending, {'TransactionPending', TransId}}, _V) ->
    {56, TransId};
e({transactionReply, {'TransactionReply', TransId, asn1_NOVALUE, TransRes}}, V) ->
    {57, TransId, e(TransRes, V)};
e({transactionReply, {'TransactionReply', TransId, 'NULL', TransRes}}, V) ->
    {58, TransId, e(TransRes, V)};
e({transactionReply, {'TransactionReply', TransId, ImmAckReq, TransRes}}, V) ->
    {59, TransId, e(ImmAckReq, V), e(TransRes, V)};
e({transactionResponseAck, T}, V) ->
    {60, el(T, V)};
e({'TransactionAck', FirstAck, asn1_NOVALUE}, _V) ->
    {61, FirstAck};
e({'TransactionAck', FirstAck, LastAck}, _V) ->
    {62, FirstAck, LastAck};

e({'ErrorDescriptor', EC, asn1_NOVALUE}, _V) ->
    {70, EC};
e({'ErrorDescriptor', EC, ET}, _V) ->
    {71, EC, ET};

e({'ActionRequest', Cid, CtxReq, CtxAAR, [CmdReq]}, V) ->
    {80, Cid, e(CtxReq, V), e(CtxAAR, V), e(CmdReq, V)};
e({'ActionRequest', Cid, CtxReq, CtxAAR, CmdReqs}, V) ->
    {81, Cid, e(CtxReq, V), e(CtxAAR, V), el(CmdReqs, V)};

e({'ContextRequest', P, E, T}, V) when V < 3 ->
    {90, e(P, V), e(E, V), el(T, V)};
e({'ContextRequest', P, E, T, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {91, e(P, V), e(E, V), el(T, V)};
e({'ContextRequest', P, E, T, IC, asn1_NOVALUE, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {92, e(P, V), e(E, V), el(T, V), e(IC, V)};
e({'ContextRequest', P, E, T, IC, CP, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {93, e(P, V), e(E, V), el(T, V), e(IC, V), el(CP, V)};
e({'ContextRequest', P, E, T, IC, CP, CL}, V) 
  when V >= 3 ->
    {94, e(P, V), e(E, V), el(T, V), e(IC, V), el(CP, V), el(CL, V)};

e({'ContextAttrAuditRequest', P, E, T}, V) when V < 3 ->
    {100, e(P, V), e(E, V), e(T, V)};
e({'ContextAttrAuditRequest', P, E, T, 
   asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {101, e(P, V), e(E, V), e(T, V)};
e({'ContextAttrAuditRequest', P, E, T, 
   IC, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {102, e(P, V), e(E, V), e(T, V), 
     e(IC, V)};
e({'ContextAttrAuditRequest', P, E, T, 
   IC, CPA, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {103, e(P, V), e(E, V), e(T, V), 
     e(IC, V), el(CPA, V)};
e({'ContextAttrAuditRequest', P, E, T, 
   IC, CPA, SP, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {104, e(P, V), e(E, V), e(T, V), 
     e(IC, V), el(CPA, V), e(SP, V)};
e({'ContextAttrAuditRequest', P, E, T, 
   IC, CPA, SP, SE, asn1_NOVALUE, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {105, e(P, V), e(E, V), e(T, V), 
     e(IC, V), el(CPA, V), e(SP, V), e(SE, V)};
e({'ContextAttrAuditRequest', P, E, T, 
   IC, CPA, SP, SE, SIC, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {106, e(P, V), e(E, V), e(T, V), 
     e(IC, V), el(CPA, V), e(SP, V), e(SE, V), e(SIC, V)};
e({'ContextAttrAuditRequest', P, E, T, 
   IC, CPA, SP, SE, SIC, SL}, V) 
  when V >= 3 ->
    {107, e(P, V), e(E, V), e(T, V), 
     e(IC, V), el(CPA, V), e(SP, V), e(SE, V), e(SIC, V), e(SL, V)};

e({'CommandRequest', Cmd, asn1_NOVALUE, asn1_NOVALUE}, V) ->
    {110, e(Cmd, V)};
e({'CommandRequest', Cmd, 'NULL', asn1_NOVALUE}, V) ->
    {111, e(Cmd, V)};
e({'CommandRequest', Cmd, asn1_NOVALUE, 'NULL'}, V) ->
    {112, e(Cmd, V)};
e({'CommandRequest', Cmd, 'NULL', 'NULL'}, V) ->
    {113, e(Cmd, V)};
e({'CommandRequest', Cmd, Opt, WR}, V) ->
    {114, e(Cmd, V), e(Opt, V), e(WR, V)};

e({'TopologyRequest', From, To, Dir}, 1 = V) ->
    {120, e(From, V), e(To, V), e(Dir, V)};
e({'TopologyRequest', From, To, Dir, SID}, 2 = V) ->
    {121, e(From, V), e(To, V), e(Dir, V), e(SID, V)};
e({'TopologyRequest', From, To, Dir, SID, asn1_NOVALUE}, V) when (V >= 3) ->
    {122, e(From, V), e(To, V), e(Dir, V), e(SID, V)};
e({'TopologyRequest', From, To, Dir, SID, TDE}, V) when (V >= 3) ->
    {123, e(From, V), e(To, V), e(Dir, V), e(SID, V), e(TDE, V)};

e({modReq, {'AmmRequest', TID, []}}, V) ->
    {130, el(TID, V)};
e({modReq, {'AmmRequest', TID, [Desc]}}, V) ->
    {131, el(TID, V), e(Desc, V)};
e({modReq, {'AmmRequest', TID, Descs}}, V) ->
    {132, el(TID, V), el(Descs, V)};
e({addReq, {'AmmRequest', TID, []}}, V) ->
    {133, el(TID, V)};
e({addReq, {'AmmRequest', TID, [Desc]}}, V) ->
    {134, el(TID, V), e(Desc, V)};
e({addReq, {'AmmRequest', TID, Descs}}, V) ->
    {135, el(TID, V), el(Descs, V)};
e({'AmmRequest', TID, Descs}, V) ->
    {136, el(TID, V), el(Descs, V)};

e({subtractReq, {'SubtractRequest', TID, asn1_NOVALUE}}, V) ->
    {140, el(TID, V)};
e({subtractReq, {'SubtractRequest', TID, AudDesc}}, V) ->
    {141, el(TID, V), e(AudDesc, V)};
e({'SubtractRequest', TID, asn1_NOVALUE}, V) ->
    {142, el(TID, V)};
e({'SubtractRequest', TID, AudDesc}, V) ->
    {143, el(TID, V), e(AudDesc, V)};

e({auditValueRequest, AR}, V) ->
    {150, e(AR, V)};

e({'AuditRequest', TID, AudDesc}, V) when V < 3 ->
    {160, e(TID, V), e(AudDesc, V)};
e({'AuditRequest', TID, AudDesc, asn1_NOVALUE}, V) when V >= 3 ->
    {161, e(TID, V), e(AudDesc, V)};
e({'AuditRequest', TID, AudDesc, TIDs}, V) when V >= 3 ->
    {162, e(TID, V), e(AudDesc, V), el(TIDs, V)};

e({actionReplies, [AR]}, V) ->
    {170, e(AR, V)};
e({actionReplies, ARs}, V) ->
    {171, el(ARs, V)};

e({'ActionReply', CID, asn1_NOVALUE, asn1_NOVALUE, [CmdRep]}, V) ->
    {180, CID, e(CmdRep, V)};
e({'ActionReply', CID, asn1_NOVALUE, asn1_NOVALUE, CmdRep}, V) ->
    {181, CID, el(CmdRep, V)};
e({'ActionReply', CID, asn1_NOVALUE, CtxRep, [CmdRep]}, V) ->
    {182, CID, e(CtxRep, V), e(CmdRep, V)};
e({'ActionReply', CID, asn1_NOVALUE, CtxRep, CmdRep}, V) ->
    {183, CID, e(CtxRep, V), el(CmdRep, V)};
e({'ActionReply', CID, ED, asn1_NOVALUE, [CmdRep]}, V) ->
    {184, CID, e(ED, V), e(CmdRep, V)};
e({'ActionReply', CID, ED, asn1_NOVALUE, CmdRep}, V) ->
    {185, CID, e(ED, V), el(CmdRep, V)};
e({'ActionReply', CID, ED, CtxRep, [CmdRep]}, V) ->
    {186, CID, e(ED, V), e(CtxRep, V), e(CmdRep, V)};
e({'ActionReply', CID, ED, CtxRep, CmdRep}, V) ->
    {187, CID, e(ED, V), e(CtxRep, V), el(CmdRep, V)};

e({'AuditDescriptor', asn1_NOVALUE}, 1 = _V) ->
    {190};
e({'AuditDescriptor', AT}, 1 = V) ->
    {191, el(AT, V)};
e({'AuditDescriptor', asn1_NOVALUE, asn1_NOVALUE}, V) when V >= 2 ->
    {192};
e({'AuditDescriptor', AT, APT}, V) 
  when is_list(AT) andalso is_list(APT) andalso (V >= 2) ->
    {193, el(AT, V), el(APT, V)};
e({'AuditDescriptor', AT, APT}, V) 
  when is_list(APT) andalso (V >= 2) ->
    {194, e(AT, V), el(APT, V)};
e({'AuditDescriptor', AT, APT}, V) 
  when is_list(AT) andalso (V >= 2) ->
    {195, el(AT, V), e(APT, V)};
e({'AuditDescriptor', AT, APT}, V) when (V >= 2) ->
    {196, e(AT, V), e(APT, V)};

e({notifyReq, {'NotifyRequest', TID, OED, asn1_NOVALUE}}, V) ->
    {200, el(TID, V), e(OED, V)};
e({notifyReq, {'NotifyRequest', TID, OED, ED}}, V) ->
    {201, el(TID, V), e(OED, V), e(ED, V)};
e({'NotifyRequest', TID, OED}, V) ->
    {202, el(TID, V), e(OED, V)};
e({'NotifyRequest', TID, OED, ED}, V) ->
    {203, el(TID, V), e(OED, V), e(ED, V)};

e({'ObservedEventsDescriptor', RID, OEL}, V) ->
    {210, RID, el(OEL, V)};

e({'ObservedEvent', EN, SID, EPL, TN}, V) ->
    {220, EN, e(SID, V), el(EPL, V), e(TN, V)};

e({'EventParameter', "type", ["est"], asn1_NOVALUE}, _V) ->
    {230};
e({'EventParameter', "type", [Val], asn1_NOVALUE}, _V) ->
    {231, Val};
e({'EventParameter', "type", Val, asn1_NOVALUE}, _V) ->
    {232, Val};
e({'EventParameter', "Generalcause", ["NR"], asn1_NOVALUE}, _V) ->
    {233};
e({'EventParameter', "Generalcause", ["UR"], asn1_NOVALUE}, _V) ->
    {234};
e({'EventParameter', "Generalcause", ["FT"], asn1_NOVALUE}, _V) ->
    {235};
e({'EventParameter', "Generalcause", ["FP"], asn1_NOVALUE}, _V) ->
    {236};
e({'EventParameter', "Generalcause", ["IW"], asn1_NOVALUE}, _V) ->
    {237};
e({'EventParameter', "Generalcause", ["UN"], asn1_NOVALUE}, _V) ->
    {238};
e({'EventParameter', "Generalcause", [Val], asn1_NOVALUE}, _V) ->
    {239, Val};
e({'EventParameter', "Generalcause", Val, asn1_NOVALUE}, _V) ->
    {240, Val};
e({'EventParameter', "Failurecause", [Val], asn1_NOVALUE}, _V) ->
    {241, Val};
e({'EventParameter', "Failurecause", Val, asn1_NOVALUE}, _V) ->
    {242, Val};
e({'EventParameter', EPN, Val, asn1_NOVALUE}, _V) ->
    {243, EPN, Val};
e({'EventParameter', EPN, Val, EI}, _V) ->
    {244, EPN, Val, EI};

e({serviceChangeReq, {'ServiceChangeRequest', TID, SCPs}}, V) ->
    {260, el(TID, V), e(SCPs, V)};
e({serviceChangeReq, SCR}, V) ->
    {261, e(SCR, V)};
e({'ServiceChangeRequest', TID, SCPs}, V) ->
    {262, el(TID, V), e(SCPs, V)};

e({serviceChangeReply, {'ServiceChangeReply', TID, SCR}}, V) ->
    {270, el(TID, V), e(SCR, V)};
e({serviceChangeReply, SCR}, V) ->
    {271, e(SCR, V)};
e({'ServiceChangeReply', TID, SCR}, V) -> %% KOLLA
    {272, el(TID, V), e(SCR, V)};

e({mediaDescriptor, {'MediaDescriptor', TSD, S}}, V) ->
    {280, e(TSD, V), e(S, V)};
e({mediaDescriptor, MD}, V) ->
    {281, e(MD, V)};
e({'MediaDescriptor', TSD, S}, V) ->
    {282, e(TSD, V), e(S, V)};

e({oneStream, S}, V) ->
    {290, e(S, V)};
e({multiStream, S}, V) ->
    {291, el(S, V)};
e({'StreamDescriptor', SID, SP}, V) ->
    {292, e(SID, V), e(SP, V)};

e({'StreamParms', LCD, asn1_NOVALUE, asn1_NOVALUE}, V) when V < 3 ->
    {300, e(LCD, V)};
e({'StreamParms', LCD, LD, asn1_NOVALUE}, V) when V < 3 ->
    {301, e(LCD, V), e(LD, V)};
e({'StreamParms', LCD, LD, RD}, V) when V < 3 ->
    {302, e(LCD, V), e(LD, V), e(RD, V)};

e({'StreamParms', LCD, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {303, e(LCD, V)};
e({'StreamParms', LCD, LD, asn1_NOVALUE, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {304, e(LCD, V), e(LD, V)};
e({'StreamParms', LCD, LD, RD, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {305, e(LCD, V), e(LD, V), e(RD, V)};
e({'StreamParms', LCD, LD, RD, SD}, V) 
  when V >= 3 ->
    {306, e(LCD, V), e(LD, V), e(RD, V), el(SD, V)};

e({'LocalControlDescriptor', SM, RV, RG, PP}, V) ->
    {310, e(SM, V), e(RV, V), e(RG, V), el(PP, V)};

e({'PropertyParm', "v", [Val], asn1_NOVALUE}, _V) ->
    {320, Val};
e({'PropertyParm', "v", Val, asn1_NOVALUE}, _V) ->
    {321, Val};
e({'PropertyParm', "o", [Val], asn1_NOVALUE}, _V) ->
    {332, Val};
e({'PropertyParm', "o", Val, asn1_NOVALUE}, _V) ->
    {333, Val};
e({'PropertyParm', "s", [Val], asn1_NOVALUE}, _V) ->
    {334, Val};
e({'PropertyParm', "s", Val, asn1_NOVALUE}, _V) ->
    {335, Val};
e({'PropertyParm', "i", [Val], asn1_NOVALUE}, _V) ->
    {336, Val};
e({'PropertyParm', "i", Val, asn1_NOVALUE}, _V) ->
    {337, Val};
e({'PropertyParm', "u", [Val], asn1_NOVALUE}, _V) ->
    {338, Val};
e({'PropertyParm', "u", Val, asn1_NOVALUE}, _V) ->
    {339, Val};
e({'PropertyParm', "e", [Val], asn1_NOVALUE}, _V) ->
    {340, Val};
e({'PropertyParm', "e", Val, asn1_NOVALUE}, _V) ->
    {341, Val};
e({'PropertyParm', "p", [Val], asn1_NOVALUE}, _V) ->
    {342, Val};
e({'PropertyParm', "p", Val, asn1_NOVALUE}, _V) ->
    {343, Val};
e({'PropertyParm', "c", [Val], asn1_NOVALUE}, _V) ->
    {344, Val};
e({'PropertyParm', "c", Val, asn1_NOVALUE}, _V) ->
    {345, Val};
e({'PropertyParm', "b", [Val], asn1_NOVALUE}, _V) ->
    {346, Val};
e({'PropertyParm', "b", Val, asn1_NOVALUE}, _V) ->
    {347, Val};
e({'PropertyParm', "z", [Val], asn1_NOVALUE}, _V) ->
    {348, Val};
e({'PropertyParm', "z", Val, asn1_NOVALUE}, _V) ->
    {349, Val};
e({'PropertyParm', "k", [Val], asn1_NOVALUE}, _V) ->
    {350, Val};
e({'PropertyParm', "k", Val, asn1_NOVALUE}, _V) ->
    {351, Val};
e({'PropertyParm', "a", [Val], asn1_NOVALUE}, _V) ->
    {352, Val};
e({'PropertyParm', "a", Val, asn1_NOVALUE}, _V) ->
    {353, Val};
e({'PropertyParm', "t", [Val], asn1_NOVALUE}, _V) ->
    {354, Val};
e({'PropertyParm', "t", Val, asn1_NOVALUE}, _V) ->
    {355, Val};
e({'PropertyParm', "r", [Val], asn1_NOVALUE}, _V) ->
    {356, Val};
e({'PropertyParm', "r", Val, asn1_NOVALUE}, _V) ->
    {357, Val};
e({'PropertyParm', "m", [Val], asn1_NOVALUE}, _V) ->
    {358, Val};
e({'PropertyParm', "m", Val, asn1_NOVALUE}, _V) ->
    {359, Val};
e({'PropertyParm', "nt/jit", [Val], asn1_NOVALUE}, _V) ->
    {360, Val};
e({'PropertyParm', "nt/jit", Val, asn1_NOVALUE}, _V) ->
    {361, Val};
e({'PropertyParm', "tdmc/ec", ["on"], asn1_NOVALUE}, _V) ->
    {362};
e({'PropertyParm', "tdmc/ec", ["off"], asn1_NOVALUE}, _V) ->
    {363};
e({'PropertyParm', "tdmc/gain", ["automatic"], asn1_NOVALUE}, _V) ->
    {364};
e({'PropertyParm', "tdmc/gain", [Val], asn1_NOVALUE}, _V) ->
    {365, Val};
e({'PropertyParm', "tdmc/gain", Val, asn1_NOVALUE}, _V) ->
    {366, Val};
e({'PropertyParm', "maxNumberOfContexts", [Val], asn1_NOVALUE}, _V) ->
    {367, Val};
e({'PropertyParm', "maxNumberOfContexts", Val, asn1_NOVALUE}, _V) ->
    {368, Val};
e({'PropertyParm', "maxTerminationsPerContext", [Val], asn1_NOVALUE}, _V) ->
    {369, Val};
e({'PropertyParm', "maxTerminationsPerContext", Val, asn1_NOVALUE}, _V) ->
    {370, Val};
e({'PropertyParm', "normalMGExecutionTime", [Val], asn1_NOVALUE}, _V) ->
    {371, Val};
e({'PropertyParm', "normalMGExecutionTime", Val, asn1_NOVALUE}, _V) ->
    {372, Val};
e({'PropertyParm', "normalMGCExecutionTime", [Val], asn1_NOVALUE}, _V) ->
    {373, Val};
e({'PropertyParm', "normalMGCExecutionTime", Val, asn1_NOVALUE}, _V) ->
    {374, Val};
e({'PropertyParm', "MGProvisionalResponseTimerValue", [Val], asn1_NOVALUE}, _V) ->
    {375, Val};
e({'PropertyParm', "MGProvisionalResponseTimerValue", Val, asn1_NOVALUE}, _V) ->
    {376, Val};
e({'PropertyParm', "MGCProvisionalResponseTimerValue", [Val], asn1_NOVALUE}, _V) ->
    {377, Val};
e({'PropertyParm', "MGCProvisionalResponseTimerValue", Val, asn1_NOVALUE}, _V) ->
    {378, Val};
e({'PropertyParm', N, [Val], asn1_NOVALUE}, _V) ->
    {379, N, Val};
e({'PropertyParm', N, Val, asn1_NOVALUE}, _V) ->
    {380, N, Val};
e({'PropertyParm', N, Val, EI}, _V) ->
    {381, N, Val, EI};

e({'LocalRemoteDescriptor', [[PG]]}, V) ->
    {400, e(PG, V)};
e({'LocalRemoteDescriptor', [PG]}, V) ->
    {401, el(PG, V)};
e({'LocalRemoteDescriptor', PG}, V) ->
    {402, ell(PG, V)};

e({'TerminationStateDescriptor', PP, EBC, SS}, V) ->
    {410, el(PP, V), e(EBC, V), e(SS, V)};

e({eventsDescriptor, {'EventsDescriptor', RID, [E]}}, V) ->
    {420, e(RID, V), e(E, V)};
e({eventsDescriptor, {'EventsDescriptor', RID, EL}}, V) ->
    {421, e(RID, V), el(EL, V)};
e({eventsDescriptor, ED}, V) ->
    {422, e(ED, V)};
e({'EventsDescriptor', RID, [E]}, V) ->
    {423, e(RID, V), e(E, V)};
e({'EventsDescriptor', RID, EL}, V) ->
    {424, e(RID, V), el(EL, V)};

e({'RequestedEvent', PN, SID, EA, EPL}, V) ->
    {425, PN, e(SID, V), e(EA, V), el(EPL, V)};

e({'RegulatedEmbeddedDescriptor', SED, SD}, V) ->
    {430, e(SED, V), el(SD, V)};

e({notifyImmediate, NI}, V) ->
    {435, e(NI, V)};
e({notifyRegulated, NR}, V) ->
    {436, e(NR, V)};
e({neverNotify, NN}, V) ->
    {437, e(NN, V)};

e({'RequestedActions', KA, EDM, SE, SD}, V) ->
    {440, e(KA, V), e(EDM, V), e(SE, V), e(SD, V)};

e({'RequestedActions', KA, EDM, SE, SD, asn1_NOVALUE, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {441, e(KA, V), e(EDM, V), e(SE, V), e(SD, V)};
e({'RequestedActions', KA, EDM, SE, SD, NB, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {442, e(KA, V), e(EDM, V), e(SE, V), e(SD, V), e(NB, V)};
e({'RequestedActions', KA, EDM, SE, SD, NB, RED}, V) 
  when V >= 3 ->
    {443, e(KA, V), e(EDM, V), e(SE, V), e(SD, V), e(NB, V), e(RED, V)};

e({'SecondEventsDescriptor', RID, [E]}, V) ->
    {450, e(RID, V), e(E, V)};
e({'SecondEventsDescriptor', RID, EL}, V) ->
    {451, e(RID, V), el(EL, V)};

e({'SecondRequestedEvent', PN, SID, EA, EPL}, V) ->
    {460, PN, e(SID, V), e(EA, V), e(EPL, V)};

e({'SecondRequestedActions', KA, EDM, SD}, V) ->
    {470, e(KA, V), e(EDM, V), e(SD, V)};

e({'SecondRequestedActions', KA, EDM, SD, asn1_NOVALUE, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {471, e(KA, V), e(EDM, V), e(SD, V)};
e({'SecondRequestedActions', KA, EDM, SD, NB, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {472, e(KA, V), e(EDM, V), e(SD, V), e(NB, V)};
e({'SecondRequestedActions', KA, EDM, SD, NB, RED}, V) 
  when V >= 3 ->
    {473, e(KA, V), e(EDM, V), e(SD, V), e(NB, V), e(RED, V)};

e({'EventSpec', EN, SID, EPL}, V) ->
    {480, EN, e(SID, V), el(EPL, V)};

e({'SeqSigList', ID, SL}, V) ->
    {490, ID, el(SL, V)};

e({signalsDescriptor, S}, V) ->
    {500, el(S, V)};
e({signal, S}, V) ->
    {510, e(S, V)};

e({'Signal', SN, SID, ST, D, NC, KA, SPL}, V) ->
    {520, SN, e(SID, V), e(ST, V), e(D, V), e(NC, V), e(KA, V), el(SPL, V)};

e({'Signal', SN, SID, ST, D, NC, KA, SPL, 
   asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {521, SN, e(SID, V), e(ST, V), e(D, V), e(NC, V), e(KA, V), el(SPL, V)};
e({'Signal', SN, SID, ST, D, NC, KA, SPL, 
   SD, asn1_NOVALUE, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {522, SN, e(SID, V), e(ST, V), e(D, V), e(NC, V), e(KA, V), el(SPL, V),
     e(SD, V)};
e({'Signal', SN, SID, ST, D, NC, KA, SPL, 
   SD, RID, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {523, SN, e(SID, V), e(ST, V), e(D, V), e(NC, V), e(KA, V), el(SPL, V),
     e(SD, V), e(RID, V)};
e({'Signal', SN, SID, ST, D, NC, KA, SPL, 
   SD, RID, IsD}, V) 
  when V >= 3 ->
    {524, SN, e(SID, V), e(ST, V), e(D, V), e(NC, V), e(KA, V), el(SPL, V),
     e(SD, V), e(RID, V), e(IsD, V)};

e({'SigParameter', SPN, Val, asn1_NOVALUE}, _V) ->
    {530, SPN, Val};
e({'SigParameter', SPN, Val, EI}, _V) ->
    {531, SPN, Val, EI};

e({modemDescriptor, MD}, V) ->
    {550, e(MD, V)};
e({'ModemDescriptor', MTL, MPL, asn1_NOVALUE}, _V) ->
    {551, MTL, MPL};
e({'ModemDescriptor', MTL, MPL, NSD}, _V) ->
    {552, MTL, MPL, NSD};

e({digitMapDescriptor, {'DigitMapDescriptor', DMN, DMV}}, V) ->
    {560, DMN, e(DMV, V)};
e({digitMapDescriptor, DMD}, V) ->
    {561, e(DMD, V)};
e({'DigitMapDescriptor', DMN, DMV}, V) ->
    {562, DMN, e(DMV, V)};

e({'DigitMapValue', Start, Stop, Long, DMB}, 1 = V) ->
    {570, e(Start, V), e(Stop, V), e(Long, V), DMB};
e({'DigitMapValue', Start, Stop, Long, DMB, Dur}, V) when V >= 2 ->
    {571, e(Start, V), e(Stop, V), e(Long, V), DMB, e(Dur, V)};

e({'ServiceChangeParm', M, A, Ver, Prof, R, D, Id, asn1_NOVALUE, asn1_NOVALUE}, V) ->
    {580, e(M, V), e(A, V), e(Ver, V), e(Prof, V), R, e(D, V), e(Id, V)};
e({'ServiceChangeParm', M, A, Ver, Prof, R, D, Id, TS, asn1_NOVALUE}, V) ->
    {581, e(M, V), e(A, V), e(Ver, V), e(Prof, V), R, e(D, V), e(Id, V), 
     e(TS, V)};
e({'ServiceChangeParm', M, A, Ver, Prof, R, D, Id, TS, NSD}, V) ->
    {582, e(M, V), e(A, V), e(Ver, V), e(Prof, V), R, e(D, V), e(Id, V), 
     e(TS, V), NSD};

e({'ServiceChangeParm', M, A, Ver, Prof, R, D, Id, TS, NSD, asn1_NOVALUE}, V) 
  when V == 2 ->
    {583, e(M, V), e(A, V), e(Ver, V), e(Prof, V), R, e(D, V), e(Id, V),
     e(TS, V), NSD};
e({'ServiceChangeParm', M, A, Ver, Prof, R, D, Id, TS, NSD, Info}, V) 
  when V == 2 ->
    {584, e(M, V), e(A, V), e(Ver, V), e(Prof, V), R, e(D, V), e(Id, V),
     e(TS, V), NSD, e(Info, V)};

e({'ServiceChangeParm', M, A, Ver, Prof, R, D, Id, TS, NSD, 
   asn1_NOVALUE, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {585, e(M, V), e(A, V), e(Ver, V), e(Prof, V), R, e(D, V), e(Id, V),
     e(TS, V), NSD};
e({'ServiceChangeParm', M, A, Ver, Prof, R, D, Id, TS, NSD, Info, 
   asn1_NOVALUE}, V)  
  when V >= 3 ->
    {586, e(M, V), e(A, V), e(Ver, V), e(Prof, V), R, e(D, V), e(Id, V), 
     e(TS, V), e(TS, V), NSD, e(Info, V)};
e({'ServiceChangeParm', M, A, Ver, Prof, R, D, Id, TS, NSD, Info, Flag}, V)  
  when V >= 3 ->
    {587, e(M, V), e(A, V), e(Ver, V), e(Prof, V), R, e(D, V), e(Id, V), 
     e(TS, V), NSD, e(Info, V), e(Flag, V)};

e({serviceChangeResParms, {'ServiceChangeResParm', Id, A, Ver, Prof, TS}}, V) ->
    {590, Id, e(A, V), Ver, e(Prof, V), TS};
e({serviceChangeResParms, SCRP}, V) ->
    {591, e(SCRP, V)};
e({'ServiceChangeResParm', Id, A, Ver, Prof, TS}, V) ->
    {592, Id, e(A, V), Ver, e(Prof, V), TS};

e({portNumber, N}, _V) ->
    {600, N};

e({'TimeNotation', D, T}, _V) ->
    {610, D, T};

e({'ServiceChangeProfile', N, Ver}, _V) ->
    {620, N, Ver};

e({digitMapName, N}, _V) ->
    {630, N};

e({megaco_term_id, false, Id}, _V) ->
    {640, Id};
e({megaco_term_id, true, [[$*]]}, _V) ->
    {641};
e({megaco_term_id, true, [[$$]]}, _V) ->
    {642};
e({megaco_term_id, true, Id}, _V) ->
    {643, Id};
e({'TerminationID', W, ID}, _V) ->
    {644, W, ID};

e({modReply, {'AmmsReply', TID, asn1_NOVALUE}}, V) ->
    {650, el(TID, V)};
e({modReply, {'AmmsReply', TID, [TA]}}, V) ->
    {651, el(TID, V), e(TA, V)};
e({modReply, {'AmmsReply', TID, TA}}, V) when is_list(TA) ->
    {652, el(TID, V), el(TA, V)};
e({modReply, R}, V) ->
    {653, e(R, V)};

e({moveReply, AR}, V) ->
    {655, e(AR, V)};

e({addReply, {'AmmsReply', TID, asn1_NOVALUE}}, V) ->
    {660, el(TID, V)};
e({addReply, {'AmmsReply', TID, [TA]}}, V) ->
    {661, el(TID, V), e(TA, V)};
e({addReply, {'AmmsReply', TID, TA}}, V) when is_list(TA) ->
    {662, el(TID, V), el(TA, V)};
e({addReply, R}, V) ->
    {663, e(R, V)};

e({subtractReply, {'AmmsReply', TID, asn1_NOVALUE}}, V) ->
    {670, el(TID, V)};
e({subtractReply, {'AmmsReply', TID, [TA]}}, V) ->
    {671, el(TID, V), e(TA, V)};
e({subtractReply, {'AmmsReply', TID, TA}}, V) when is_list(TA) ->
    {672, el(TID, V), el(TA, V)};
e({subtractReply, R}, V) ->
    {673, e(R, V)};

e({'AmmsReply', TID, asn1_NOVALUE}, V) ->
    {680, el(TID, V)};
e({'AmmsReply', TID, [TA]}, V) ->
    {681, el(TID, V), e(TA, V)};
e({'AmmsReply', TID, TA}, V) when is_list(TA) ->
    {682, el(TID, V), el(TA, V)};

e({notifyReply, {'NotifyReply', TID, asn1_NOVALUE}}, V) ->
    {690, el(TID, V)};
e({notifyReply, {'NotifyReply', TID, ED}}, V) ->
    {691, el(TID, V), e(ED, V)};
e({notifyReply, R}, V) ->
    {692, e(R, V)};
e({'NotifyReply', TID, asn1_NOVALUE}, V) ->
    {693, el(TID, V)};
e({'NotifyReply', TID, ED}, V) ->
    {694, el(TID, V), e(ED, V)};

e({auditValueReply, AVR}, V) ->
    {700, e(AVR, V)};

e({contextAuditResult, TIDs}, V) ->
    {705, el(TIDs, V)};

e({auditResult, {'AuditResult', TID, [TAR]}}, V) ->
    {710, e(TID, V), e(TAR, V)};
e({auditResult, {'AuditResult', TID, TAR}}, V) ->
    {711, e(TID, V), el(TAR, V)};
e({auditResult, AR}, V) ->
    {712, e(AR, V)};
e({'AuditResult', TID, [TAR]}, V) ->
    {713, e(TID, V), e(TAR, V)};
e({'AuditResult', TID, TAR}, V) ->
    {714, e(TID, V), el(TAR, V)};

e({auditResultTermList, {'TermListAuditResult', TIDs, [TAR]}}, V) ->
    {715, el(TIDs, V), e(TAR, V)};
e({auditResultTermList, {'TermListAuditResult', TIDs, TAR}}, V) ->
    {716, el(TIDs, V), el(TAR, V)};

e({packagesDescriptor, PsD}, V) ->
    {720, el(PsD, V)};

e({'PackagesItem', "g", 1}, _V) ->
    {730};
e({'PackagesItem', "tonegen", 1}, _V) ->
    {731};
e({'PackagesItem', "tonedet", 1}, _V) ->
    {732};
e({'PackagesItem', "tg", 1}, _V) ->
    {733};
e({'PackagesItem', "dd", 1}, _V) ->
    {734};
e({'PackagesItem', "cg", 1}, _V) ->
    {735};
e({'PackagesItem', "cd", 1}, _V) ->
    {736};
e({'PackagesItem', "al", 1}, _V) ->
    {737};
e({'PackagesItem', "ct", 1}, _V) ->
    {738};
e({'PackagesItem', "nt", 1}, _V) ->
    {739};
e({'PackagesItem', "rtp", 1}, _V) ->
    {740};
e({'PackagesItem', "tdmc", 1}, _V) ->
    {741};
e({'PackagesItem', Name, Ver}, _V) ->
    {742, Name, Ver};

e({emptyDescriptors, AD}, V) ->
    {760, e(AD, V)};

e({statisticsDescriptor, [SD]}, V) ->
    {770, e(SD, V)};
e({statisticsDescriptor, SsD}, V) ->
    {771, el(SsD, V)};

e({'StatisticsParameter', Name, asn1_NOVALUE}, _V) ->
    {780, Name};
e({'StatisticsParameter', Name, Value}, _V) ->
    {781, Name, Value};

e({'MuxDescriptor', MT, TL, asn1_NOVALUE}, V) ->
    {800, e(MT, V), el(TL, V)};
e({'MuxDescriptor', MT, TL, NSD}, V) ->
    {801, e(MT, V), el(TL, V), NSD};

e({indAudPackagesDescriptor, {'IndAudPackagesDescriptor', N, Ver}}, V) 
  when (V >= 2) ->
    {900, N, Ver};
e({indAudPackagesDescriptor, IAPD}, V) 
  when (V >= 2) ->
    {900, e(IAPD, V)};
e({'IndAudPackagesDescriptor', N, Ver}, V) 
  when (V >= 2) ->
    {901, N, Ver};

e({indAudStatisticsDescriptor, {'IndAudStatisticsDescriptor', N}}, V) 
  when (V >= 2) ->
    {910, N};
e({indAudStatisticsDescriptor, IASD}, V) 
  when (V >= 2) ->
    {911, e(IASD, V)};
e({'IndAudStatisticsDescriptor', N}, V) 
  when (V >= 2) ->
    {912, N};

e({indAudDigitMapDescriptor, {'IndAudDigitMapDescriptor', DMN}}, V) 
  when (V >= 2) ->
    {920, DMN};
e({indAudDigitMapDescriptor, IADMD}, V) 
  when (V >= 2) ->
    {921, e(IADMD, V)};
e({'IndAudDigitMapDescriptor', DMN}, V) 
  when (V >= 2) ->
    {922, DMN};

e({indAudSignalsDescriptor, {seqSigList, IASD}}, V)  
  when (V >= 2) ->
    {930, e(IASD, V)};
e({indAudSignalsDescriptor, {signal, IAS}}, V)  
  when (V >= 2) ->
    {931, e(IAS, V)};

e({'IndAudSeqSigList', Id, SL}, V)  
  when (V >= 2) ->
    {940, Id, e(SL, V)};

e({'IndAudSignal', N, SID}, 2 = V)  ->
    {950, N, e(SID, V)};
e({'IndAudSignal', N, SID, asn1_NOVALUE}, V)  
  when (V >= 3) ->
    {951, N, e(SID, V)};
e({'IndAudSignal', N, SID, RID}, V)  
  when (V >= 3) ->
    {952, N, e(SID, V), e(RID, V)};

e({indAudEventBufferDescriptor, {'IndAudEventBufferDescriptor', EN, SID}}, V)  
  when (V >= 2) ->
    {960, EN, e(SID, V)};
e({indAudEventBufferDescriptor, IAEBD}, V)  
  when (V >= 2) ->
    {961, e(IAEBD, V)};
e({'IndAudEventBufferDescriptor', EN, SID}, V)  
  when (V >= 2) ->
    {962, EN, e(SID, V)};

e({indAudEventsDescriptor, {'IndAudEventsDescriptor', RID, N, SID}}, V)  
  when (V >= 2) ->
    {970, e(RID, V), N, e(SID, V)};
e({indAudEventsDescriptor, IAED}, V)  
  when (V >= 2) ->
    {971, e(IAED, V)};
e({'IndAudEventsDescriptor', RID, N, SID}, V)  
  when (V >= 2) ->
    {972, e(RID, V), N, e(SID, V)};

e({indAudMediaDescriptor, {'IndAudMediaDescriptor', TSD, S}}, V) when V >= 2 ->
    {980, e(TSD, V), e(S, V)};
e({indAudMediaDescriptor, IAMD}, V) when V >= 2 ->
    {981, e(IAMD, V)};
e({'IndAudMediaDescriptor', TSD, S}, V) when V >= 2 ->
    {982, e(TSD, V), e(S, V)};

e({'IndAudTerminationStateDescriptor', PP, EBC, SS}, 2 = V) ->
    {990, el(PP, V), e(EBC, V), e(SS, V)};
e({'IndAudTerminationStateDescriptor', PP, EBC, SS, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {991, el(PP, V), e(EBC, V), e(SS, V)};
e({'IndAudTerminationStateDescriptor', PP, EBC, SS, SSS}, V) 
  when V >= 3 ->
    {992, el(PP, V), e(EBC, V), e(SS, V), e(SSS, V)};

e({'IndAudStreamDescriptor', SID, SP}, V) ->
    {1000, e(SID, V), e(SP, V)};

e({'IndAudStreamParms', LCD, asn1_NOVALUE, asn1_NOVALUE}, 2 = V) ->
    {1010, e(LCD, V)};
e({'IndAudStreamParms', LCD, LD, RD}, 2 = V) ->
    {1011, e(LCD, V), e(LD, V), e(RD, V)};
e({'IndAudStreamParms', LCD, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {1012, e(LCD, V)};
e({'IndAudStreamParms', LCD, LD, asn1_NOVALUE, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {1013, e(LCD, V), e(LD, V)};
e({'IndAudStreamParms', LCD, LD, RD, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {1014, e(LCD, V), e(LD, V), e(RD, V)};
e({'IndAudStreamParms', LCD, LD, RD, SD}, V) 
  when V >= 3 ->
    {1015, e(LCD, V), e(LD, V), e(RD, V), e(SD, V)};

e({'IndAudLocalControlDescriptor', SM, RV, RG, asn1_NOVALUE}, 2 = V) ->
    {1020, e(SM, V), e(RV, V), e(RG, V)};
e({'IndAudLocalControlDescriptor', SM, RV, RG, PP}, 2 = V) when is_list(PP) ->
    {1021, e(SM, V), e(RV, V), e(RG, V), el(PP, V)};
e({'IndAudLocalControlDescriptor', SM, RV, RG, asn1_NOVALUE, asn1_NOVALUE}, V) 
  when V >= 3 ->
    {1022, e(SM, V), e(RV, V), e(RG, V)};
e({'IndAudLocalControlDescriptor', SM, RV, RG, PP, asn1_NOVALUE}, V) 
  when is_list(PP) andalso (V >= 3) ->
    {1023, e(SM, V), e(RV, V), e(RG, V), el(PP, V)};
e({'IndAudLocalControlDescriptor', SM, RV, RG, PP, SMS}, V) 
  when is_list(PP) andalso (V >= 3) ->
    {1024, e(SM, V), e(RV, V), e(RG, V), el(PP, V), e(SMS, V)};

e({'IndAudPropertyParm', N}, 2 = _V) ->
    {1030, N};
e({'IndAudPropertyParm', N, asn1_NOVALUE}, V) when V >= 3 ->
    {1031, N};
e({'IndAudPropertyParm', N, PP}, V) when V >= 3 ->
    {1032, N, e(PP, V)};

e(oneway, _V) ->
    {1100};
e(bothway, _V) ->
    {1101};
e(isolate, _V) ->
    {1102};
e(onewayexternal, _V) ->
    {1103};
e(onewayboth, _V) ->
    {1104};

e(T, _V) ->
    %% io:format("e(~w) -> ~nT: ~w~n", [_V, T]),
    T.


d({1}, _) ->
    asn1_NOVALUE;
d({2}, _V) ->
    'NULL';
d({3}, _V) ->
    sendRecv;
d({4}, _V) ->
    recvOnly;
d({5}, _V) ->
    restart;
d({6}, _) ->
    mediaToken;
d({7}, _) ->
    eventsToken;
d({8}, _) ->
    signalsToken;
d({9}, _) ->
    digitMapToken;
d({10}, _) ->
    statsToken;
d({11}, _) ->
    packagesToken;
d({12}, _V) ->
    h221;
d({13}, _V) ->
    h223;
d({14}, _V) ->
    h226;
d({15}, _V) ->
    v76;

d({20, Mid, Body}, _) ->
    {'MegacoMessage', asn1_NOVALUE, {'Message', 1, d(Mid, 1), d(Body, 1)}};
d({21, Mid, Body}, _) ->
    {'MegacoMessage', asn1_NOVALUE, {'Message', 2, d(Mid, 2), d(Body, 2)}};
d({22, V, Mid, Body}, _) ->
    {'MegacoMessage', asn1_NOVALUE, {'Message', V, d(Mid, V), d(Body, V)}};
d({23, AuthHeader, Mid, Body}, _) ->
    {'MegacoMessage', d(AuthHeader, 1), {'Message', 1, d(Mid, 1), d(Body, 1)}};
d({24, AuthHeader, Mid, Body}, _) ->
    {'MegacoMessage', d(AuthHeader, 2), {'Message', 2, d(Mid, 2), d(Body, 2)}};
d({25, V, AuthHeader, Mid, Body}, _) ->
    {'MegacoMessage', d(AuthHeader, V), {'Message', V, d(Mid, V), d(Body, V)}};
d({26, AuthHeader, Mess}, V) ->
    {'MegacoMessage', d(AuthHeader, V), d(Mess, V)};
d({27, V, Mid, Body}, _) ->
    {'Message', V, d(Mid, V), d(Body, V)};

d({30, Name}, _V) ->
    {domainName, {'DomainName', Name, asn1_NOVALUE}};
d({31, Name, PortNumber}, _V) ->
    {domainName, {'DomainName', Name, PortNumber}};
d({32, N}, V) ->
    {domainName, d(N, V)};
d({33, Name}, _V) ->
    {'DomainName', Name, asn1_NOVALUE};
d({34, Name, PortNumber}, _V) ->
    {'DomainName', Name, PortNumber};
d({35, Addr}, _V) ->
    {ip4Address, {'IP4Address', Addr, asn1_NOVALUE}};
d({36, Addr, PortNumber}, _V) ->
    {ip4Address, {'IP4Address', Addr, PortNumber}};
d({37, A}, V) ->
    {ip4Address, d(A, V)};
d({38, Addr}, _V) ->
    {'IP4Address', Addr, asn1_NOVALUE};
d({39, Addr, PortNumber}, _V) ->
    {'IP4Address', Addr, PortNumber};
d({40, Addr}, _V) ->
    {ip6Address, {'IP6Address', Addr, asn1_NOVALUE}};
d({41, Addr, PortNumber}, _V) ->
    {ip6Address, {'IP6Address', Addr, PortNumber}};
d({42, A}, V) ->
    {ip6Address, d(A, V)};
d({43, Addr}, _V) ->
    {'IP6Address', Addr, asn1_NOVALUE};
d({44, Addr, PortNumber}, _V) ->
    {'IP6Address', Addr, PortNumber};

d({50, Transaction}, V) ->
    {transactions, [d(Transaction, V)]};
d({51, Transactions}, V) ->
    {transactions, dl(Transactions, V)};
d({52, EC}, _V) ->
    {messageError, {'ErrorDescriptor', EC, asn1_NOVALUE}};
d({53, EC, ET}, _V) ->
    {messageError, {'ErrorDescriptor', EC, ET}};
d({54, Error}, V) ->
    {messageError, d(Error, V)};
d({55, TransId, Actions}, V) ->
    {transactionRequest, {'TransactionRequest', TransId, dl(Actions, V)}};
d({56, TransId}, _V) ->
    {transactionPending, {'TransactionPending', TransId}};
d({57, TransId, TransRes}, V) ->
    {transactionReply, {'TransactionReply', TransId, asn1_NOVALUE, d(TransRes, V)}};
d({58, TransId, TransRes}, V) ->
    {transactionReply, {'TransactionReply', TransId, 'NULL', d(TransRes, V)}};
d({59, TransId, ImmAckReq, TransRes}, V) ->
    {transactionReply, {'TransactionReply', TransId, d(ImmAckReq, V), d(TransRes, V)}};
d({60, T}, V) ->
    {transactionResponseAck, dl(T, V)};
d({61, FirstAck}, _V) ->
    {'TransactionAck', FirstAck, asn1_NOVALUE};
d({62, FirstAck, LastAck}, _V) ->
    {'TransactionAck', FirstAck, LastAck};

d({70, EC}, _V) ->
    {'ErrorDescriptor', EC, asn1_NOVALUE};
d({71, EC, ET}, _V) ->
    {'ErrorDescriptor', EC, ET};

d({80, Cid, CtxReq, CtxAAR, CmdReq}, V) ->
    {'ActionRequest', Cid, d(CtxReq, V), d(CtxAAR, V), [d(CmdReq, V)]};
d({81, Cid, CtxReq, CtxAAR, CmdReqs}, V) ->
    {'ActionRequest', Cid, d(CtxReq, V), d(CtxAAR, V), dl(CmdReqs, V)};

d({90, P, E, T}, V) ->
    {'ContextRequest', d(P, V), d(E, V), dl(T, V)};
d({91, P, E, T}, V) ->
    {'ContextRequest', d(P, V), d(E, V), dl(T, V), 
     asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE};
d({92, P, E, T, IC}, V) ->
    {'ContextRequest', d(P, V), d(E, V), dl(T, V), 
     d(IC, V), asn1_NOVALUE, asn1_NOVALUE};
d({93, P, E, T, IC, CP}, V) ->
    {'ContextRequest', d(P, V), d(E, V), dl(T, V), 
     d(IC, V), dl(CP, V), asn1_NOVALUE};
d({94, P, E, T, IC, CP, CL}, V) ->
    {'ContextRequest', d(P, V), d(E, V), dl(T, V), 
     d(IC, V), dl(CP, V), dl(CL, V)};

d({100, P, E, T}, V) ->
    {'ContextAttrAuditRequest', d(P, V), d(E, V), d(T, V)};
d({101, P, E, T}, V) ->
    {'ContextAttrAuditRequest', d(P, V), d(E, V), d(T, V), 
   asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE};
d({102, P, E, T, IC}, V) ->
    {'ContextAttrAuditRequest', d(P, V), d(E, V), d(T, V), 
     d(IC, V), asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE};
d({103, P, E, T, IC, CPA}, V) ->
    {'ContextAttrAuditRequest', d(P, V), d(E, V), d(T, V), 
     d(IC, V), dl(CPA, V), asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE};
d({104, P, E, T, IC, CPA, SP}, V) ->
    {'ContextAttrAuditRequest', d(P, V), d(E, V), d(T, V), 
     d(IC, V), dl(CPA, V), d(SP, V), asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE};
d({105, P, E, T, IC, CPA, SP, SE}, V) ->
    {'ContextAttrAuditRequest', d(P, V), d(E, V), d(T, V), 
     d(IC, V), dl(CPA, V), d(SP, V), d(SE, V), asn1_NOVALUE, asn1_NOVALUE};
d({106, P, E, T, IC, CPA, SP, SE, SIC}, V) ->
    {'ContextAttrAuditRequest', d(P, V), d(E, V), d(T, V), 
     d(IC, V), dl(CPA, V), d(SP, V), d(SE, V), d(SIC, V), asn1_NOVALUE};
d({107, P, E, T, IC, CPA, SP, SE, SIC, SL}, V) ->
    {'ContextAttrAuditRequest', d(P, V), d(E, V), d(T, V), 
     d(IC, V), dl(CPA, V), d(SP, V), d(SE, V), d(SIC, V), d(SL, V)};

d({110, Cmd}, V) ->
    {'CommandRequest', d(Cmd, V), asn1_NOVALUE, asn1_NOVALUE};
d({111, Cmd}, V) ->
    {'CommandRequest', d(Cmd, V), 'NULL', asn1_NOVALUE};
d({112, Cmd}, V) ->
    {'CommandRequest', d(Cmd, V), asn1_NOVALUE, 'NULL'};
d({113, Cmd}, V) ->
    {'CommandRequest', d(Cmd, V), 'NULL', 'NULL'};
d({114, Cmd, Opt, WR}, V) ->
    {'CommandRequest', d(Cmd, V), d(Opt, V), d(WR, V)};

d({120, From, To, Dir}, 1 = V) ->
    {'TopologyRequest', d(From, V), d(To, V), d(Dir, V)};
d({121, From, To, Dir, SID}, 2 = V) ->
    {'TopologyRequest', d(From, V), d(To, V), d(Dir, V), d(SID, V)};
d({122, From, To, Dir, SID}, V) when (V >= 3) ->
    {'TopologyRequest', d(From, V), d(To, V), d(Dir, V), d(SID, V), asn1_NOVALUE};
d({123, From, To, Dir, SID, TDE}, V) when (V >= 3) ->
    {'TopologyRequest', d(From, V), d(To, V), d(Dir, V), d(SID, V), d(TDE, V)};

d({130, TID}, V) ->
    {modReq, {'AmmRequest', dl(TID, V), []}};
d({131, TID, Desc}, V) ->
    {modReq, {'AmmRequest', dl(TID, V), [d(Desc, V)]}};
d({132, TID, Descs}, V) ->
    {modReq, {'AmmRequest', dl(TID, V), dl(Descs, V)}};
d({133, TID}, V) ->
    {addReq, {'AmmRequest', dl(TID, V), []}};
d({134, TID, Desc}, V) ->
    {addReq, {'AmmRequest', dl(TID, V), [d(Desc, V)]}};
d({135, TID, Descs}, V) ->
    {addReq, {'AmmRequest', dl(TID, V), dl(Descs, V)}};
d({136, TID, Descs}, V) ->
    {'AmmRequest', dl(TID, V), dl(Descs, V)};

d({140, TID}, V) ->
    {subtractReq, {'SubtractRequest', dl(TID, V), asn1_NOVALUE}};
d({141, TID, AudDesc}, V) ->
    {subtractReq, {'SubtractRequest', dl(TID, V), d(AudDesc, V)}};
d({142, TID}, V) ->
    {'SubtractRequest', dl(TID, V), asn1_NOVALUE};
d({143, TID, AudDesc}, V) ->
    {'SubtractRequest', dl(TID, V), d(AudDesc, V)};

d({150, AR}, V) ->
    {auditValueRequest, d(AR, V)};

d({160, TID, AudDesc}, V) when V < 3 ->
    {'AuditRequest', d(TID, V), d(AudDesc, V)};
d({161, TID, AudDesc}, V) when V >= 3 ->
    {'AuditRequest', d(TID, V), d(AudDesc, V), asn1_NOVALUE};
d({162, TID, AudDesc, TIDs}, V) when V >= 3 ->
    {'AuditRequest', d(TID, V), d(AudDesc, V), dl(TIDs, V)};

d({170, AR}, V) ->
    {actionReplies, [d(AR, V)]};
d({171, ARs}, V) ->
    {actionReplies, dl(ARs, V)};

d({180, CID, CmdRep}, V) ->
    {'ActionReply', CID, asn1_NOVALUE, asn1_NOVALUE, [d(CmdRep, V)]};
d({181, CID, CmdRep}, V) ->
    {'ActionReply', CID, asn1_NOVALUE, asn1_NOVALUE, dl(CmdRep, V)};
d({182, CID, CtxRep, CmdRep}, V) ->
    {'ActionReply', CID, asn1_NOVALUE, d(CtxRep, V), [d(CmdRep, V)]};
d({183, CID, CtxRep, CmdRep}, V) ->
    {'ActionReply', CID, asn1_NOVALUE, d(CtxRep, V), dl(CmdRep, V)};
d({184, CID, ED, CmdRep}, V) ->
    {'ActionReply', CID, d(ED, V), asn1_NOVALUE, [d(CmdRep, V)]};
d({185, CID, ED, CmdRep}, V) ->
    {'ActionReply', CID, d(ED, V), asn1_NOVALUE, dl(CmdRep, V)};
d({186, CID, ED, CtxRep, CmdRep}, V) ->
    {'ActionReply', CID, d(ED, V), d(CtxRep, V), [d(CmdRep, V)]};
d({187, CID, ED, CtxRep, CmdRep}, V) ->
    {'ActionReply', CID, d(ED, V), d(CtxRep, V), dl(CmdRep, V)};

d({190}, 1 = _V) ->
    {'AuditDescriptor', asn1_NOVALUE};
d({191, AT}, 1 = V) ->
    {'AuditDescriptor', dl(AT, V)};
d({192}, V) when (V >= 2) ->
    {'AuditDescriptor', asn1_NOVALUE, asn1_NOVALUE};
d({193, AT, APT}, V) when is_list(AT) andalso is_list(APT) andalso (V >= 2) ->
    {'AuditDescriptor', dl(AT, V), dl(APT, V)};
d({194, AT, APT}, V) when is_list(APT) andalso (V >= 2) ->
    {'AuditDescriptor', d(AT, V), dl(APT, V)};
d({195, AT, APT}, V) when is_list(AT) andalso (V >= 2) ->
    {'AuditDescriptor', dl(AT, V), d(APT, V)};
d({196, AT, APT}, V) when (V >= 2) ->
    {'AuditDescriptor', d(AT, V), d(APT, V)};

d({200, TID, OED}, V) ->
    {notifyReq, {'NotifyRequest', dl(TID, V), d(OED, V), asn1_NOVALUE}};
d({201, TID, OED, ED}, V) ->
    {notifyReq, {'NotifyRequest', dl(TID, V), d(OED, V), d(ED, V)}};
d({202, TID, OED}, V) ->
    {'NotifyRequest', dl(TID, V), d(OED, V), asn1_NOVALUE};
d({203, TID, OED, ED}, V) ->
    {'NotifyRequest', dl(TID, V), d(OED, V), d(ED, V)};

d({210, RID, OEL}, V) ->
    {'ObservedEventsDescriptor', RID, dl(OEL, V)};

d({220, EN, SID, EPL, TN}, V) ->
    {'ObservedEvent', EN, d(SID, V), dl(EPL, V), d(TN, V)};

d({230}, _V) ->
    {'EventParameter', "type", ["est"], asn1_NOVALUE};
d({231, Val}, _V) ->
    {'EventParameter', "type", [Val], asn1_NOVALUE};
d({232, Val}, _V) ->
    {'EventParameter', "type", Val, asn1_NOVALUE};
d({233}, _V) ->
    {'EventParameter', "Generalcause", ["NR"], asn1_NOVALUE};
d({234}, _V) ->
    {'EventParameter', "Generalcause", ["UR"], asn1_NOVALUE};
d({235}, _V) ->
    {'EventParameter', "Generalcause", ["FT"], asn1_NOVALUE};
d({236}, _V) ->
    {'EventParameter', "Generalcause", ["FP"], asn1_NOVALUE};
d({237}, _V) ->
    {'EventParameter', "Generalcause", ["IW"], asn1_NOVALUE};
d({238}, _V) ->
    {'EventParameter', "Generalcause", ["UN"], asn1_NOVALUE};
d({239, Val}, _V) ->
    {'EventParameter', "Generalcause", [Val], asn1_NOVALUE};
d({240, Val}, _V) ->
    {'EventParameter', "Generalcause", Val, asn1_NOVALUE};
d({241, Val}, _V) ->
    {'EventParameter', "Failurecause", [Val], asn1_NOVALUE};
d({242, Val}, _V) ->
    {'EventParameter', "Failurecause", Val, asn1_NOVALUE};
d({243, EPN, Val}, _V) ->
    {'EventParameter', EPN, Val, asn1_NOVALUE};
d({244, EPN, Val, EI}, _V) ->
    {'EventParameter', EPN, Val, EI};

d({260, TID, SCPs}, V) ->
    {serviceChangeReq, {'ServiceChangeRequest', dl(TID, V), d(SCPs, V)}};
d({261, SCR}, V) ->
    {serviceChangeReq, d(SCR, V)};
d({262, TID, SCPs}, V) ->
    {'ServiceChangeRequest', dl(TID, V), d(SCPs, V)};

d({270, TID, SCR}, V) ->
    {serviceChangeReply, {'ServiceChangeReply', dl(TID, V), d(SCR, V)}};
d({271, SCR}, V) ->
    {serviceChangeReply, d(SCR, V)};
d({272, TID, SCR}, V) -> %% KOLLA
    {'ServiceChangeReply', dl(TID, V), d(SCR, V)};

d({280, TSD, S}, V) ->
    {mediaDescriptor, {'MediaDescriptor', d(TSD, V), d(S, V)}};
d({281, MD}, V) ->
    {mediaDescriptor, d(MD, V)};
d({282, TSD, S}, V) ->
    {'MediaDescriptor', d(TSD, V), d(S, V)};

d({290, S}, V) ->
    {oneStream, d(S, V)};
d({291, S}, V) ->
    {multiStream, dl(S, V)};
d({292, SID, SP}, V) ->
    {'StreamDescriptor', d(SID, V), d(SP, V)};

d({300, LCD}, V) ->
    {'StreamParms', d(LCD, V), asn1_NOVALUE, asn1_NOVALUE};
d({301, LCD, LD}, V) ->
    {'StreamParms', d(LCD, V), d(LD, V), asn1_NOVALUE};
d({302, LCD, LD, RD}, V) ->
    {'StreamParms', d(LCD, V), d(LD, V), d(RD, V)};

d({303, LCD}, V)  
  when V >= 3 ->
    {'StreamParms', d(LCD, V), asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE};
d({304, LCD, LD}, V)  
  when V >= 3 ->
    {'StreamParms', d(LCD, V), d(LD, V), asn1_NOVALUE, asn1_NOVALUE};
d({305, LCD, LD, RD}, V)  
  when V >= 3 ->
    {'StreamParms', d(LCD, V), d(LD, V), d(RD, V), asn1_NOVALUE};
d({306, LCD, LD, RD, SD}, V)  
  when V >= 3 ->
    {'StreamParms', d(LCD, V), d(LD, V), d(RD, V), dl(SD, V)};

d({310, SM, RV, RG, PP}, V) ->
    {'LocalControlDescriptor', d(SM, V), d(RV, V), d(RG, V), dl(PP, V)};

d({320, Val}, _V) ->
    {'PropertyParm', "v", [Val], asn1_NOVALUE};
d({321, Val}, _V) ->
    {'PropertyParm', "v", Val, asn1_NOVALUE};
d({332, Val}, _V) ->
    {'PropertyParm', "o", [Val], asn1_NOVALUE};
d({333, Val}, _V) ->
    {'PropertyParm', "o", Val, asn1_NOVALUE};
d({334, Val}, _V) ->
    {'PropertyParm', "s", [Val], asn1_NOVALUE};
d({335, Val}, _V) ->
    {'PropertyParm', "s", Val, asn1_NOVALUE};
d({336, Val}, _V) ->
    {'PropertyParm', "i", [Val], asn1_NOVALUE};
d({337, Val}, _V) ->
    {'PropertyParm', "i", Val, asn1_NOVALUE};
d({338, Val}, _V) ->
    {'PropertyParm', "u", [Val], asn1_NOVALUE};
d({339, Val}, _V) ->
    {'PropertyParm', "u", Val, asn1_NOVALUE};
d({340, Val}, _V) ->
    {'PropertyParm', "e", [Val], asn1_NOVALUE};
d({341, Val}, _V) ->
    {'PropertyParm', "e", Val, asn1_NOVALUE};
d({342, Val}, _V) ->
    {'PropertyParm', "p", [Val], asn1_NOVALUE};
d({343, Val}, _V) ->
    {'PropertyParm', "p", Val, asn1_NOVALUE};
d({344, Val}, _V) ->
    {'PropertyParm', "c", [Val], asn1_NOVALUE};
d({345, Val}, _V) ->
    {'PropertyParm', "c", Val, asn1_NOVALUE};
d({346, Val}, _V) ->
    {'PropertyParm', "b", [Val], asn1_NOVALUE};
d({347, Val}, _V) ->
    {'PropertyParm', "b", Val, asn1_NOVALUE};
d({348, Val}, _V) ->
    {'PropertyParm', "z", [Val], asn1_NOVALUE};
d({349, Val}, _V) ->
    {'PropertyParm', "z", Val, asn1_NOVALUE};
d({350, Val}, _V) ->
    {'PropertyParm', "k", [Val], asn1_NOVALUE};
d({351, Val}, _V) ->
    {'PropertyParm', "k", Val, asn1_NOVALUE};
d({352, Val}, _V) ->
    {'PropertyParm', "a", [Val], asn1_NOVALUE};
d({353, Val}, _V) ->
    {'PropertyParm', "a", Val, asn1_NOVALUE};
d({354, Val}, _V) ->
    {'PropertyParm', "t", [Val], asn1_NOVALUE};
d({355, Val}, _V) ->
    {'PropertyParm', "t", Val, asn1_NOVALUE};
d({356, Val}, _V) ->
    {'PropertyParm', "r", [Val], asn1_NOVALUE};
d({357, Val}, _V) ->
    {'PropertyParm', "r", Val, asn1_NOVALUE};
d({358, Val}, _V) ->
    {'PropertyParm', "m", [Val], asn1_NOVALUE};
d({359, Val}, _V) ->
    {'PropertyParm', "m", Val, asn1_NOVALUE};
d({360, Val}, _V) ->
    {'PropertyParm', "nt/jit", [Val], asn1_NOVALUE};
d({361, Val}, _V) ->
    {'PropertyParm', "nt/jit", Val, asn1_NOVALUE};
d({362}, _V) ->
    {'PropertyParm', "tdmc/ec", ["on"], asn1_NOVALUE};
d({363}, _V) ->
    {'PropertyParm', "tdmc/ec", ["off"], asn1_NOVALUE};
d({364}, _V) ->
    {'PropertyParm', "tdmc/gain", ["automatic"], asn1_NOVALUE};
d({365, Val}, _V) ->
    {'PropertyParm', "tdmc/gain", [Val], asn1_NOVALUE};
d({366, Val}, _V) ->
    {'PropertyParm', "tdmc/gain", Val, asn1_NOVALUE};
d({367, Val}, _V) ->
    {'PropertyParm', "maxNumberOfContexts", [Val], asn1_NOVALUE};
d({368, Val}, _V) ->
    {'PropertyParm', "maxNumberOfContexts", Val, asn1_NOVALUE};
d({369, Val}, _V) ->
    {'PropertyParm', "maxTerminationsPerContext", [Val], asn1_NOVALUE};
d({370, Val}, _V) ->
    {'PropertyParm', "maxTerminationsPerContext", Val, asn1_NOVALUE};
d({371, Val}, _V) ->
    {'PropertyParm', "normalMGExecutionTime", [Val], asn1_NOVALUE};
d({372, Val}, _V) ->
    {'PropertyParm', "normalMGExecutionTime", Val, asn1_NOVALUE};
d({373, Val}, _V) ->
    {'PropertyParm', "normalMGCExecutionTime", [Val], asn1_NOVALUE};
d({374, Val}, _V) ->
    {'PropertyParm', "normalMGCExecutionTime", Val, asn1_NOVALUE};
d({375, Val}, _V) ->
    {'PropertyParm', "MGProvisionalResponseTimerValue", [Val], asn1_NOVALUE};
d({376, Val}, _V) ->
    {'PropertyParm', "MGProvisionalResponseTimerValue", Val, asn1_NOVALUE};
d({377, Val}, _V) ->
    {'PropertyParm', "MGCProvisionalResponseTimerValue", [Val], asn1_NOVALUE};
d({378, Val}, _V) ->
    {'PropertyParm', "MGCProvisionalResponseTimerValue", Val, asn1_NOVALUE};
d({379, N, Val}, _V) ->
    {'PropertyParm', N, [Val], asn1_NOVALUE};
d({380, N, Val}, _V) ->
    {'PropertyParm', N, Val, asn1_NOVALUE};
d({381, N, Val, EI}, _V) ->
    {'PropertyParm', N, Val, EI};

d({400, PG}, V) ->
    {'LocalRemoteDescriptor', [[d(PG, V)]]};
d({401, PG}, V) ->
    {'LocalRemoteDescriptor', [dl(PG, V)]};
d({402, PG}, V) ->
    {'LocalRemoteDescriptor', dll(PG, V)};

d({410, PP, EBC, SS}, V) ->
    {'TerminationStateDescriptor', dl(PP, V), d(EBC, V), d(SS, V)};

d({420, RID, E}, V) ->
    {eventsDescriptor, {'EventsDescriptor', d(RID, V), [d(E, V)]}};
d({421, RID, EL}, V) ->
    {eventsDescriptor, {'EventsDescriptor', d(RID, V), dl(EL, V)}};
d({422, ED}, V) ->
    {eventsDescriptor, d(ED, V)};
d({423, RID, E}, V) ->
    {'EventsDescriptor', d(RID, V), [d(E, V)]};
d({424, RID, EL}, V) ->
    {'EventsDescriptor', d(RID, V), dl(EL, V)};

d({425, PN, SID, EA, EPL}, V) ->
    {'RequestedEvent', PN, d(SID, V), d(EA, V), dl(EPL, V)};

d({430, SED, SD}, V) ->
    {'RegulatedEmbeddedDescriptor', d(SED, V), dl(SD, V)};

d({435, NI}, V) ->
    {notifyImmediate, d(NI, V)};
d({436, NR}, V) ->
    {notifyRegulated, d(NR, V)};
d({437, NN}, V) ->
    {neverNotify, d(NN, V)};

d({440, KA, EDM, SE, SD}, V) ->
    {'RequestedActions', d(KA, V), d(EDM, V), d(SE, V), d(SD, V)};
d({441, KA, EDM, SE, SD}, V)  
  when V >= 3 ->
    {'RequestedActions', d(KA, V), d(EDM, V), d(SE, V), d(SD, V), 
     asn1_NOVALUE, asn1_NOVALUE};
d({442, KA, EDM, SE, SD, NB}, V)  
  when V >= 3 ->
    {'RequestedActions', d(KA, V), d(EDM, V), d(SE, V), d(SD, V), 
     d(NB, V), asn1_NOVALUE};
d({443, KA, EDM, SE, SD, NB, RED}, V)  
  when V >= 3 ->
    {'RequestedActions', d(KA, V), d(EDM, V), d(SE, V), d(SD, V), 
     d(NB, V), d(RED, V)};

d({450, RID, E}, V) ->
    {'SecondEventsDescriptor', d(RID, V), [d(E, V)]};
d({451, RID, EL}, V) ->
    {'SecondEventsDescriptor', d(RID, V), dl(EL, V)};

d({460, PN, SID, EA, EPL}, V) ->
    {'SecondRequestedEvent', PN, d(SID, V), d(EA, V), d(EPL, V)};

d({470, KA, EDM, SD}, V) ->
    {'SecondRequestedActions', d(KA, V), d(EDM, V), d(SD, V)};
d({471, KA, EDM, SD}, V) 
  when V >= 3 ->
    {'SecondRequestedActions', d(KA, V), d(EDM, V), d(SD, V), 
     asn1_NOVALUE, asn1_NOVALUE};
d({472, KA, EDM, SD, NB}, V) 
  when V >= 3 ->
    {'SecondRequestedActions', d(KA, V), d(EDM, V), d(SD, V), 
     d(NB, V), asn1_NOVALUE};
d({473, KA, EDM, SD, NB, RED}, V) 
  when V >= 3 ->
    {'SecondRequestedActions', d(KA, V), d(EDM, V), d(SD, V), 
     d(NB, V), d(RED, V)};

d({480, EN, SID, EPL}, V) ->
    {'EventSpec', EN, d(SID, V), dl(EPL, V)};

d({490, ID, SL}, V) ->
    {'SeqSigList', ID, dl(SL, V)};

d({500, S}, V) ->
    {signalsDescriptor, dl(S, V)};

d({510, S}, V) ->
    {signal, d(S, V)};

d({520, SN, SID, ST, D, NC, KA, SPL}, V) ->
    {'Signal', 
     SN, d(SID, V), d(ST, V), d(D, V), d(NC, V), d(KA, V), dl(SPL, V)};
d({521, SN, SID, ST, D, NC, KA, SPL}, V) 
  when V >= 3 ->
    {'Signal', 
     SN, d(SID, V), d(ST, V), d(D, V), d(NC, V), d(KA, V), dl(SPL, V),
     asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE};
d({522, SN, SID, ST, D, NC, KA, SPL, SD}, V) 
  when V >= 3 ->
    {'Signal', 
     SN, d(SID, V), d(ST, V), d(D, V), d(NC, V), d(KA, V), dl(SPL, V),
     d(SD, V), asn1_NOVALUE, asn1_NOVALUE};
d({523, SN, SID, ST, D, NC, KA, SPL, SD, RID}, V) 
  when V >= 3 ->
    {'Signal', 
     SN, d(SID, V), d(ST, V), d(D, V), d(NC, V), d(KA, V), dl(SPL, V),
     d(SD, V), d(RID, V), asn1_NOVALUE};
d({524, SN, SID, ST, D, NC, KA, SPL, SD, RID, IsD}, V) 
  when V >= 3 ->
    {'Signal', 
     SN, d(SID, V), d(ST, V), d(D, V), d(NC, V), d(KA, V), dl(SPL, V),
     d(SD, V), d(RID, V), d(IsD, V)};

d({530, SPN, Val}, _V) ->
    {'SigParameter', SPN, Val, asn1_NOVALUE};
d({531, SPN, Val, EI}, _V) ->
    {'SigParameter', SPN, Val, EI};

d({550, MD}, V) ->
    {modemDescriptor, d(MD, V)};
d({551, MTL, MPL}, _V) ->
    {'ModemDescriptor', MTL, MPL, asn1_NOVALUE};
d({552, MTL, MPL, NSD}, _V) ->
    {'ModemDescriptor', MTL, MPL, NSD};

d({560, DMN, DMV}, V) ->
    {digitMapDescriptor, {'DigitMapDescriptor', DMN, d(DMV, V)}};
d({561, DMD}, V) ->
    {digitMapDescriptor, d(DMD, V)};
d({562, DMN, DMV}, V) ->
    {'DigitMapDescriptor', DMN, d(DMV, V)};

d({570, Start, Stop, Long, DMB}, 1 = V) ->
    {'DigitMapValue', d(Start, V), d(Stop, V), d(Long, V), DMB};
d({571, Start, Stop, Long, DMB, Dur}, V) when V >= 2 ->
    {'DigitMapValue', d(Start, V), d(Stop, V), d(Long, V), DMB, d(Dur, V)};

d({580, M, A, Ver, Prof, R, D, Id}, V) ->
    {'ServiceChangeParm', 
     d(M, V), d(A, V), d(Ver, V), d(Prof, V), R, d(D, V), d(Id, V), 
     asn1_NOVALUE, asn1_NOVALUE};
d({581, M, A, Ver, Prof, R, D, Id, TS}, V) ->
    {'ServiceChangeParm', 
     d(M, V), d(A, V), d(Ver, V), d(Prof, V), R, d(D, V), d(Id, V), 
     d(TS, V), asn1_NOVALUE};
d({582, M, A, Ver, Prof, R, D, Id, TS, NSD}, V) ->
    {'ServiceChangeParm', 
     d(M, V), d(A, V), d(Ver, V), d(Prof, V), R, d(D, V), d(Id, V), 
     d(TS, V), NSD};

d({583, M, A, Ver, Prof, R, D, Id, TS, NSD}, V)  
  when V == 2 ->
    {'ServiceChangeParm', 
     d(M, V), d(A, V), d(Ver, V), d(Prof, V), R, d(D, V), d(Id, V), 
     d(TS, V), NSD, asn1_NOVALUE};
d({584, M, A, Ver, Prof, R, D, Id, TS, NSD, Info}, V)  
  when V == 2 ->
    {'ServiceChangeParm', 
     d(M, V), d(A, V), d(Ver, V), d(Prof, V), R, d(D, V), d(Id, V), 
     d(TS, V), NSD, d(Info, V)};

d({585, M, A, Ver, Prof, R, D, Id, TS, NSD}, V) 
  when V >= 3 ->
    {'ServiceChangeParm', 
     d(M, V), d(A, V), d(Ver, V), d(Prof, V), R, d(D, V), d(Id, V), 
     d(TS, V), NSD, asn1_NOVALUE, asn1_NOVALUE};
d({586, M, A, Ver, Prof, R, D, Id, TS, NSD, Info}, V) 
  when V >= 3 ->
    {'ServiceChangeParm', 
     d(M, V), d(A, V), d(Ver, V), d(Prof, V), R, d(D, V), d(Id, V), 
     d(TS, V), NSD, d(Info, V), asn1_NOVALUE};
d({587, M, A, Ver, Prof, R, D, Id, TS, NSD, Info, Flag}, V) 
  when V >= 3 ->
    {'ServiceChangeParm', 
     d(M, V), d(A, V), d(Ver, V), d(Prof, V), R, d(D, V), d(Id, V), 
     d(TS, V), NSD, d(Info, V), d(Flag, V)};

d({590, Id, A, Ver, Prof, TS}, V) ->
    {serviceChangeResParms, {'ServiceChangeResParm', Id, d(A, V), Ver, d(Prof, V), TS}};
d({591, SCRP}, V) ->
    {serviceChangeResParms, d(SCRP, V)};
d({592, Id, A, Ver, Prof, TS}, V) ->
    {'ServiceChangeResParm', Id, d(A, V), Ver, d(Prof, V), TS};

d({600, N}, _V) ->
    {portNumber, N};

d({610, D, T}, _V) ->
    {'TimeNotation', D, T};

d({620, N, Ver}, _V) ->
    {'ServiceChangeProfile', N, Ver};

d({630, N}, _) ->
    {digitMapName, N};

d({640, Id}, _V) ->
    {megaco_term_id, false, Id};
d({641}, _V) ->
    {megaco_term_id, true, [[$*]]};
d({642}, _V) ->
    {megaco_term_id, true, [[$$]]};
d({643, Id}, _V) ->
    {megaco_term_id, true, Id};
d({644, W, ID}, _V) ->
    {'TerminationID', W, ID};

d({650, TID}, V) ->
    {modReply, {'AmmsReply', dl(TID, V), asn1_NOVALUE}};
d({651, TID, TA}, V) ->
    {modReply, {'AmmsReply', dl(TID, V), [d(TA, V)]}};
d({652, TID, TA}, V) ->
    {modReply, {'AmmsReply', dl(TID, V), dl(TA, V)}};
d({653, R}, V) ->
    {modReply, d(R, V)};

d({655, AR}, V) ->
    {moveReply, d(AR, V)};

d({660, TID}, V) ->
    {addReply, {'AmmsReply', dl(TID, V), asn1_NOVALUE}};
d({661, TID, TA}, V) ->
    {addReply, {'AmmsReply', dl(TID, V), [d(TA, V)]}};
d({662, TID, TA}, V) ->
    {addReply, {'AmmsReply', dl(TID, V), dl(TA, V)}};
d({663, R}, V) ->
    {addReply, d(R, V)};

d({670, TID}, V) ->
    {subtractReply, {'AmmsReply', dl(TID, V), asn1_NOVALUE}};
d({671, TID, TA}, V) ->
    {subtractReply, {'AmmsReply', dl(TID, V), [d(TA, V)]}};
d({672, TID, TA}, V) ->
    {subtractReply, {'AmmsReply', dl(TID, V), dl(TA, V)}};
d({673, R}, V) ->
    {subtractReply, d(R, V)};

d({680, TID}, V) ->
    {'AmmsReply', dl(TID, V), asn1_NOVALUE};
d({681, TID, TA}, V) ->
    {'AmmsReply', dl(TID, V), [d(TA, V)]};
d({682, TID, TA}, V) ->
    {'AmmsReply', dl(TID, V), dl(TA, V)};

d({690, TID}, V) ->
    {notifyReply, {'NotifyReply', dl(TID, V), asn1_NOVALUE}};
d({691, TID, ED}, V) ->
    {notifyReply, {'NotifyReply', dl(TID, V), d(ED, V)}};
d({692, R}, V) ->
    {notifyReply, d(R, V)};
d({693, TID}, V) ->
    {'NotifyReply', dl(TID, V), asn1_NOVALUE};
d({694, TID, ED}, V) ->
    {'NotifyReply', dl(TID, V), d(ED, V)};

d({700, AVR}, V) ->
    {auditValueReply, d(AVR, V)};

d({705, TIDs}, V) ->
    {contextAuditResult, dl(TIDs, V)};

d({710, TID, TAR}, V) ->
    {auditResult, {'AuditResult', d(TID, V), [d(TAR, V)]}};
d({711, TID, TAR}, V) ->
    {auditResult, {'AuditResult', d(TID, V), dl(TAR, V)}};
d({712, AR}, V) ->
    {auditResult, d(AR, V)};
d({713, TID, TAR}, V) ->
    {'AuditResult', d(TID, V), [d(TAR, V)]};
d({714, TID, TAR}, V) ->
    {'AuditResult', d(TID, V), dl(TAR, V)};

d({715, TIDs, [TAR]}, V) ->
    {auditResultTermList, {'TermListAuditResult', dl(TIDs, V), [d(TAR, V)]}};
d({716, TIDs, TAR}, V) ->
    {auditResultTermList, {'TermListAuditResult', dl(TIDs, V), dl(TAR, V)}};

d({720, PsD}, V) ->
    {packagesDescriptor, dl(PsD, V)};

d({730}, _V) ->
    {'PackagesItem', "g", 1};
d({731}, _V) ->
    {'PackagesItem', "tonegen", 1};
d({732}, _V) ->
    {'PackagesItem', "tonedet", 1};
d({733}, _V) ->
    {'PackagesItem', "tg", 1};
d({734}, _V) ->
    {'PackagesItem', "dd", 1};
d({735}, _V) ->
    {'PackagesItem', "cg", 1};
d({736}, _V) ->
    {'PackagesItem', "cd", 1};
d({737}, _V) ->
    {'PackagesItem', "al", 1};
d({738}, _V) ->
    {'PackagesItem', "ct", 1};
d({739}, _V) ->
    {'PackagesItem', "nt", 1};
d({740}, _V) ->
    {'PackagesItem', "rtp", 1};
d({741}, _V) ->
    {'PackagesItem', "tdmc", 1};
d({742, Name, Ver}, _V) ->
    {'PackagesItem', Name, Ver};

d({760, AD}, V) ->
    {emptyDescriptors, d(AD, V)};

d({770, SD}, V) ->
    {statisticsDescriptor, [d(SD, V)]};
d({771, SsD}, V) ->
    {statisticsDescriptor, dl(SsD, V)};

d({780, Name}, _V) ->
    {'StatisticsParameter', Name, asn1_NOVALUE};
d({781, Name, Value}, _V) ->
    {'StatisticsParameter', Name, Value};

d({800, MT, TL}, V) ->
    {'MuxDescriptor', d(MT, V), dl(TL, V), asn1_NOVALUE};
d({801, MT, TL, NSD}, V) ->
    {'MuxDescriptor', d(MT, V), dl(TL, V), NSD};

d({900, N, Ver}, V) when (V >= 2) ->
    {indAudPackagesDescriptor, {'IndAudPackagesDescriptor', N, Ver}};
d({900, IAPD}, V) when (V >= 2) ->
    {indAudPackagesDescriptor, d(IAPD, V)};
d({901, N, Ver}, V) when (V >= 2) ->
    {'IndAudPackagesDescriptor', N, Ver};

d({910, N}, V) when (V >= 2) ->
    {indAudStatisticsDescriptor, {'IndAudStatisticsDescriptor', N}};
d({911, IASD}, V) when (V >= 2) ->
    {indAudStatisticsDescriptor, d(IASD, V)};
d({912, N}, V) when (V >= 2) ->
    {'IndAudStatisticsDescriptor', N};

d({920, DMN}, V) when (V >= 2) ->
    {indAudDigitMapDescriptor, {'IndAudDigitMapDescriptor', DMN}};
d({921, IADMD}, V) when (V >= 2) ->
    {indAudDigitMapDescriptor, d(IADMD, V)};
d({922, DMN}, V) when (V >= 2) ->
    {'IndAudDigitMapDescriptor', DMN};

d({930, IASD}, V) when (V >= 2) ->
    {indAudSignalsDescriptor, {seqSigList, d(IASD, V)}};
d({931, IAS}, V) when (V >= 2) ->
    {indAudSignalsDescriptor, {signal, d(IAS, V)}};

d({940, Id, SL}, V) when (V >= 2) ->
    {'IndAudSeqSigList', Id, d(SL, V)};

d({950, N, SID}, 2 = V) ->
    {'IndAudSignal', N, d(SID, V)};
d({951, N, SID}, V) when (V >= 3) ->
    {'IndAudSignal', N, d(SID, V), asn1_NOVALUE};
d({952, N, SID, RID}, V) when (V >= 3) ->
    {'IndAudSignal', N, d(SID, V), d(RID, V)};

d({960, EN, SID}, V) when (V >= 2) ->
    {indAudEventBufferDescriptor, 
     {'IndAudEventBufferDescriptor', EN, d(SID, V)}};
d({961, IAEBD}, V) when (V >= 2) ->
    {indAudEventBufferDescriptor, d(IAEBD, V)};
d({962, EN, SID}, V) when (V >= 2) ->
    {'IndAudEventBufferDescriptor', EN, d(SID, V)};

d({970, RID, N, SID}, V) when (V >= 2) ->
    {indAudEventsDescriptor, 
     {'IndAudEventsDescriptor', d(RID, V), N, d(SID, V)}};
d({971, IAED}, V) when (V >= 2) ->
    {indAudEventsDescriptor, d(IAED, V)};
d({972, RID, N, SID}, V) when (V >= 2) ->
    {'IndAudEventsDescriptor', d(RID, V), N, d(SID, V)};

d({980, TSD, S}, V) when (V >= 2) ->
    {indAudMediaDescriptor, {'IndAudMediaDescriptor', d(TSD, V), d(S, V)}};
d({981, IAMD}, V) when (V >= 2) ->
    {indAudMediaDescriptor, d(IAMD, V)};
d({982, TSD, S}, V) when (V >= 2) ->
    {'IndAudMediaDescriptor', d(TSD, V), d(S, V)};

d({990, PP, EBC, SS}, 2 = V) ->
    {'IndAudTerminationStateDescriptor', dl(PP, V), d(EBC, V), d(SS, V)};
d({991, PP, EBC, SS}, V) when V >= 3 ->
    {'IndAudTerminationStateDescriptor', dl(PP, V), d(EBC, V), d(SS, V), 
     asn1_NOVALUE};
d({992, PP, EBC, SS, SSS}, V) when V >= 3 ->
    {'IndAudTerminationStateDescriptor', dl(PP, V), d(EBC, V), d(SS, V),
     d(SSS, V)};

d({1000, SID, SP}, V) ->
    {'IndAudStreamDescriptor', d(SID, V), d(SP, V)};

d({1010, LCD}, 2 = V) ->
    {'IndAudStreamParms', d(LCD, V), asn1_NOVALUE, asn1_NOVALUE};
d({1011, LCD, LD, RD}, 2 = V) ->
    {'IndAudStreamParms', d(LCD, V), d(LD, V), d(RD, V)};
d({1012, LCD}, V) when V >= 3 ->
    {'IndAudStreamParms', d(LCD, V), asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE};
d({1013, LCD, LD}, V) when V >= 3 ->
    {'IndAudStreamParms', d(LCD, V), d(LD, V), asn1_NOVALUE, asn1_NOVALUE};
d({1014, LCD, LD, RD}, V) when V >= 3 ->
    {'IndAudStreamParms', d(LCD, V), d(LD, V), d(RD, V), asn1_NOVALUE};
d({1015, LCD, LD, RD, SD}, V) when V >= 3 ->
    {'IndAudStreamParms', d(LCD, V), d(LD, V), d(RD, V), d(SD, V)};

d({1020, SM, RV, RG}, 2 = V) ->
    {'IndAudLocalControlDescriptor', 
     d(SM, V), d(RV, V), d(RG, V), asn1_NOVALUE};
d({1021, SM, RV, RG, PP}, 2 = V) 
  when is_list(PP) ->
    {'IndAudLocalControlDescriptor', d(SM, V), d(RV, V), d(RG, V), dl(PP, V)};
d({1022, SM, RV, RG}, V) when (V >= 3) ->
    {'IndAudLocalControlDescriptor', 
     d(SM, V), d(RV, V), d(RG, V), asn1_NOVALUE, asn1_NOVALUE};
d({1023, SM, RV, RG, PP}, V) 
  when is_list(PP) andalso (V >= 3) ->
    {'IndAudLocalControlDescriptor', d(SM, V), d(RV, V), d(RG, V), dl(PP, V),
     asn1_NOVALUE};
d({1024, SM, RV, RG, PP, SMS}, V) 
  when is_list(PP) andalso (V >= 3) ->
    {'IndAudLocalControlDescriptor', d(SM, V), d(RV, V), d(RG, V), dl(PP, V),
     d(SMS, V)};

d({1030, N}, 2 = _V) ->
    {'IndAudPropertyParm', N};
d({1031, N}, V) when V >= 3 ->
    {'IndAudPropertyParm', N, asn1_NOVALUE};
d({1032, N, PP}, V) when V >= 3 ->
    {'IndAudPropertyParm', N, d(PP, V)};

d({1100}, _V) ->
    oneway;
d({1101}, _V) ->
    bothway;
d({1102}, _V) ->
    isolate;
d({1103}, _V) ->
    onewayexternal;
d({1104}, _V) ->
    onewayboth;

d(T, _V) ->
    %% io:format("d(~w) -> ~nT: ~w~n", [_V, T]),
    T.


%% i(F, A) ->
%%     %% i(get(dbg), F, A).
%%     i(true, F, A).

%% i(true, F, A) ->
%%     io:format("DBG:" ++ F ++ "~n", A);
%% i(_, _, _) ->
%%     ok.

