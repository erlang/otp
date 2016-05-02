%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(beam_compiler_4).
-export([beam_compiler_4/0]).

%% From Ulf Wiger.

beam_compiler_4() ->
    ok.

selected_alarm(_Env, Parameters) ->
    UnWebParam = x,
    [Time, Severity, Info, Cause, 
     Type, Sender, Name, FaultId] = bar:foo(),

    %% The following case generates no test heap instructions,
    %% but the test heap generated before the call to bar:foo(),
    %% will be moved to here.  And the number of words to allocate
    %% will be ridicously big (> 65535) and will not fit in a 16 big
    %% word.

    SenderLink = case Name of
		     "pchScheduledConnectionAlarm" ->
			 Sender;
		     "pchVpVcAlarm" ->
			 Sender;
		     "pchSystemMemoryAlarm" ->
			 Sender;
		     "uniProtocolRestartFailureAlarm" ->
			 Sender;
		     "spvcScheduledConnectionAlarm" ->
			 Sender;
		     "eqmSwitchCoreInterfaceAlarm" ->
			 Sender;
		     "atmPhysicalLayerAlarm" ->
			 Sender;
		     "atmBufferCongestionAlarm" ->
			 Sender;
		     "sdhLinkAlarm" ->
			 Sender;
		     "sdhFarEndAlarm" ->
			 Sender;
		     "sdhUpperQosAlarm" ->
			 Sender;
		     "sdhLowerQosAlarm" ->
			 Sender;
		     "eqmSwitchCoreAlarm" ->
			 Sender;
		     "eqmEtAlarm" ->
			 Sender;
		     "eqmHwIdentityFaultAlarm" ->
			 Sender;
		     "eqmOperatorBlockedEquipmentAlarm" ->
			 Sender;
		     "pdh34PathAlarm" ->
			 Sender;
		     "pdh34ObservationAlarm" ->
			 Sender;
		     "pdh34UpperQosAlarm" ->
			 Sender;
		     "pdh34LowerQosAlarm" ->
			 Sender;
		     "nsySynchRefAlarm" ->
			 Sender;
		     "nsySynchRefBlockedAlarm" ->
			 Sender;
		     "nsySynchNodeHoldoverAlarm" ->
			 Sender;
		     "nsySynchNodeNotWorkingAlarm" ->
			 Sender;
		     "eqmAtbAlarm" ->
			 Sender;
		     "eqmCbEtAlarm" ->
			 Sender;
		     "eqmCpAlarm" ->
			 Sender;
		     "eqmCpInterfaceAlarm" ->
			 Sender;
		     "eqmCbClkAlarm" ->
			 Sender;
		     "eqmCbClkInterfaceAlarm" ->
			 Sender;
		     "eqmCbClkVelocityAlarm" ->
			 Sender;
		     "eqmCbClkPhaseDiffAlarm" ->
			 Sender;
		     "eqmHwNotFoundAlarm" ->
			 Sender;
		     "eqmPduAlarm" ->
			 Sender;
		     "eqmFanAlarm" ->
			 Sender;
		     "eqmLocAlarm" ->
			 Sender;
		     "eqmCustomerDefined1Alarm" ->
			 Sender;
		     "eqmCustomerDefined2Alarm" ->
			 Sender;
		     "eqmCustomerDefined3Alarm" ->
			 Sender;
		     "eqmCustomerDefined4Alarm" ->
			 Sender;
		     "eqmOperatorBlockedLinkAlarm" ->
			 Sender;
		     "eqmPowerFilterAlarm" ->
			 Sender;
		     "eqmCbAbrAlarm" ->
			 Sender;
		     "eqmAlarmCutOffAlarm" ->
			 Sender;
		     OtherAlarm ->
			 Sender
		 end,

    %% The testHeap instruction generated here will move up to before
    %% the case.

    bar:foo("<TABLE>
                           <TR><TD ALIGN=LEFT>Fault id:
                               <TD>" ++ FaultId ++ "
                           <TR><TD ALIGN=LEFT>Name:
                               <TD>" ++ Name ++ "
                           <TR><TD ALIGN=LEFT>Sender:
                               <TD>" ++ SenderLink ++ "
                           <TR><TD ALIGN=LEFT>Class:
                               <TD>" ++ Type ++ "
                           <TR><TD ALIGN=LEFT>Cause:
                               <TD>" ++ Cause ++ "
                           <TR><TD ALIGN=LEFT>Severity:
                               <TD>" ++ Severity ++ "
                           <TR><TD ALIGN=LEFT>Information:
                               <TD>" ++ Info ++ "
                           <TR><TD ALIGN=LEFT>Time:
                               <TD>" ++ Time).
