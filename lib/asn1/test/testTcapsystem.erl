%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%
-module(testTcapsystem).

-export([compile/2]).

-include_lib("test_server/include/test_server.hrl").

compile(Config, Options) ->
    [asn1_test_lib:compile(filename:join([tcapsystem, M]), Config, Options)
     || M <- ["DialoguePDUs.asn",
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
    ok.
