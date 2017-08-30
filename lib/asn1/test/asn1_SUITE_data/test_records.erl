%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2017. All Rights Reserved.
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

-module(test_records).

-export(['check_record_names_OTP-5812'/1]).

-define(line,put(test_server_loc,{?MODULE,?LINE}),).

-include("NBAP-PDU-Descriptions.hrl").
-include("NBAP-PDU-Contents.hrl").
-include("NBAP-Containers.hrl").
-include("NBAP-CommonDataTypes.hrl").
-include("NBAP-IEs.hrl").




'check_record_names_OTP-5812'(Msg) ->
    io:format("Msg: ~n~p~n",[Msg]),
    check_record_names(Msg).

check_record_names({initiatingMessage,
		    #'InitiatingMessage'{procedureID = ProcedureID,
					 criticality = _Criticality,
					 messageDiscriminator = _MessageDisc,
					 transactionID = _TransactionID,
					 value = Value}}) ->
    
    ok = check_record_ProcedureID(ProcedureID),
    ok = check_record_Value(Value).

check_record_ProcedureID(#'ProcedureID'{}) ->
    ok;
check_record_ProcedureID(_) -> false.

check_record_Value(#'ResourceStatusIndication'{protocolIEs = ProtocolIEs}) ->
    ok = check_record_ProtocolIEs(ProtocolIEs);
check_record_Value(_) -> false.

check_record_ProtocolIEs([#'ProtocolIE-Field'{value =IndicationType}|_]) ->
    ok = check_record_NFResourceStatusInd(IndicationType);
check_record_ProtocolIEs(_) -> false.

check_record_NFResourceStatusInd({'no-Failure',#'No-Failure-ResourceStatusInd'{'local-Cell-InformationList'=[LCIPF]}}) ->
    'check_record_NFResourceStatusInd_ProtocolIE-Field'(LCIPF);
check_record_NFResourceStatusInd(_) -> false.

'check_record_NFResourceStatusInd_ProtocolIE-Field'(#'ProtocolIE-Field'{value=LCI}) ->
    ok = check_record_LCInfoResourceStatusInd(LCI);
'check_record_NFResourceStatusInd_ProtocolIE-Field'(_) -> false.

check_record_LCInfoResourceStatusInd(#'Local-Cell-InformationItem-ResourceStatusInd'{commonChannelsCapacityConsumptionLaw=[CCCCL],dedicatedChannelsCapacityConsumptionLaw=[DCCCL],'iE-Extensions' = [LCIRE]}) ->
    ok = check_record_CCCCL(CCCCL),
    ok = check_record_DCCCL(DCCCL),
    ok = check_record_LCIRE(LCIRE).

check_record_CCCCL(#'CommonChannelsCapacityConsumptionLaw_SEQOF'{}) -> 
    ok;
check_record_CCCCL(_) -> false.

check_record_DCCCL(#'DedicatedChannelsCapacityConsumptionLaw_SEQOF'{}) ->
    ok;
check_record_DCCCL(_) -> false.
check_record_LCIRE(#'ProtocolExtensionField'{}) ->
    ok;
check_record_LCIRE(_) -> false.
