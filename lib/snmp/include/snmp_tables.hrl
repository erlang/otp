%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

%%-----------------------------------------------------------------
%% This file contains the record definitions of the agent SNMP MIB
%% tables to be used when creating the tables in Mnesia, for users
%% that want to store this data in Mnesia rather than config files
%% and ets/dets.
%%-----------------------------------------------------------------

-include("SNMPv2-TM.hrl").
-include("SNMPv2-TC.hrl").

-record(snmpCommunityTable,
	{snmpCommunityIndex,
	 snmpCommunityName,
	 snmpCommunitySecurityName,
	 snmpCommunityContextEngineID,
	 snmpCommunityContextName = "",
	 snmpCommunityTransportTag,
	 snmpCommunityStorageType = ?StorageType_nonVolatile,
	 snmpCommunityStatus = ?RowStatus_active}).

-record(snmpNotifyTable,
	{snmpNotifyName,
	 snmpNotifyTag,
	 snmpNotifyType,
	 snmpNotifyStorageType = ?StorageType_nonVolatile,
	 snmpNotifyRowStatus = ?RowStatus_active}).

-record(snmpTargetAddrTable,
	{snmpTargetAddrName,
	 snmpTargetAddrTDomain = ?snmpUDPDomain,
	 snmpTargetAddrTAddress,
	 snmpTargetAddrTimeout = 1500,
	 snmpTargetAddrRetryCount = 3,
	 snmpTargetAddrTagList,
	 snmpTargetAddrParams,
	 snmpTargetAddrStorageType = ?StorageType_nonVolatile,
	 snmpTargetAddrRowStatus = ?RowStatus_active,
	 snmpTargetAddrEngineId = "",   % not SNMP accessible
	 snmpTargetAddrTMask = [],      % part of ext table
	 snmpTargetAddrMMS = 2048}).    % part of ext table

-record(snmpTargetParamsTable,
	{snmpTargetParamsName,
	 snmpTargetParamsMPModel,
	 snmpTargetParamsSecurityModel,
	 snmpTargetParamsSecurityName,
	 snmpTargetParamsSecurityLevel,
	 snmpTargetParamsStorageType = ?StorageType_nonVolatile,
	 snmpTargetParamsRowStatus = ?RowStatus_active}).

-record(usmUserTable,
	{key, % {usmUserEngineID, usmUserName}
	 usmUserSecurityName,
	 usmUserCloneFrom,
	 usmUserAuthProtocol,
	 usmUserAuthKeyChange,
	 usmUserOwnAuthKeyChange,
	 usmUserPrivProtocol,
	 usmUserPrivKeyChange,
	 usmUserOwnPrivKeyChange,
	 usmUserPublic,
	 usmUserStorageType = ?StorageType_nonVolatile,
	 usmUserStatus = ?RowStatus_active,
         authKey,   % not SNMP accessible
         privKey}). % not SNMP accessible

-record(vacmSecurityToGroupTable,
	{key, % {vacmSecurityModel, vacmSecurityName}
	 vacmGroupName,
	 vacmSecurityToGroupStorageType = ?StorageType_nonVolatile,
	 vacmSecurityToGroupStatus = ?RowStatus_active}).

-record(vacmAccessTable,
	{key, % {vacmGroupName, vacmAccessContextPrefix,
	      %  vacmSecurityModel, vacmAccessSecurityLevel}
	 vacmAccessContextMatch,
	 vacmAccessReadViewName,
	 vacmAccessWriteViewName,
	 vacmAccessNotifyViewName,
	 vacmAccessStorageType = ?StorageType_nonVolatile,
	 vacmAccessStatus = ?RowStatus_active}).

-record(vacmViewTreeFamilyTable,
	{key, % {vacmViewTreeFamilyViewName, vacmViewTreeFamilySubtree}
	 vacmViewTreeFamilyMask,
	 vacmViewTreeFamilyType,
	 vacmViewTreeFamilyStorageType = ?StorageType_nonVolatile,
	 vacmViewTreeFamilyStatus = ?RowStatus_active}).


