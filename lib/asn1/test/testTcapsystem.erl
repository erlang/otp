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
%%
-module(testTcapsystem).

-export([compile/2]).

compile(Config, Options) ->
    Fs = [filename:join("tcapsystem", M) ||
	     M <- ["DialoguePDUs.asn",
		   "MAP-ApplicationContexts.asn",
		   "MAP-BS-Code.asn",
		   "MAP-CallHandlingOperations.asn",
		   "MAP-CH-DataTypes.asn",
		   "MAP-CommonDataTypes.asn",
		   "MAP-DialogueInformation.asn",
		   "MAP-ER-DataTypes.asn",
		   "MAP-Errors.asn",
		   "MAP-ExtensionDataTypes.asn",
		   "MAP-GR-DataTypes.asn",
		   "MAP-Group-Call-Operations.asn",
		   "MAP-LCS-DataTypes.asn",
		   "MAP-LocationServiceOperations.asn",
		   "MAP-MobileServiceOperations.asn",
		   "MAP-MS-DataTypes.asn",
		   "MAP-OM-DataTypes.asn",
		   "MAP-OperationAndMaintenanceOperations.asn",
		   "MAP-Protocol.asn",
		   "MAP-SecureTransportOperations.asn",
		   "MAP-ShortMessageServiceOperations.asn",
		   "MAP-SM-DataTypes.asn",
		   "MAP-SS-Code.asn",
		   "MAP-SS-DataTypes.asn",
		   "MAP-ST-DataTypes.asn",
		   "MAP-SupplementaryServiceOperations.asn",
		   "MAP-TS-Code.asn",
		   "MobileDomainDefinitions.asn",
		   "Remote-Operations-Generic-ROS-PDUs.asn",
		   "Remote-Operations-Information-Objects.asn",
		   "Remote-Operations-Useful-Definitions.asn",
		   "TCAP-Examples.asn",
		   "TCAPMessages.asn",
		   "TCAP-Tools.asn",
		   "TC-Notation-Extensions.asn",
		   "UnidialoguePDUs.asn"]],
    asn1_test_lib:compile_all(Fs, Config, Options).
