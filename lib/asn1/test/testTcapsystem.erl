%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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

-export([compile/3]).

-include_lib("test_server/include/test_server.hrl").



compile(Config,Rules,Opt) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line DataDir2 = filename:join([DataDir,tcapsystem]),

    ?line ok = asn1ct:compile(filename:join([DataDir2,"DialoguePDUs.asn"]),[Rules,{outdir,OutDir}]++Opt),
    ?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-ApplicationContexts.asn"]),[Rules,{outdir,OutDir}]++Opt),
    ?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-BS-Code.asn"]),[Rules,{outdir,OutDir}]++Opt),
    	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-CallHandlingOperations.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-CH-DataTypes.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-CommonDataTypes.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-DialogueInformation.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-ER-DataTypes.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-Errors.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-ExtensionDataTypes.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-GR-DataTypes.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-Group-Call-Operations.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-LCS-DataTypes.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-LocationServiceOperations.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-MobileServiceOperations.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-MS-DataTypes.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-OM-DataTypes.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-OperationAndMaintenanceOperations.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-Protocol.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-SecureTransportOperations.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-ShortMessageServiceOperations.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-SM-DataTypes.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-SS-Code.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-SS-DataTypes.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-ST-DataTypes.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-SupplementaryServiceOperations.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MAP-TS-Code.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"MobileDomainDefinitions.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"Remote-Operations-Generic-ROS-PDUs.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"Remote-Operations-Information-Objects.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"Remote-Operations-Useful-Definitions.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"TCAP-Examples.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"TCAPMessages.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"TCAP-Tools.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"TC-Notation-Extensions.asn"]),[Rules,{outdir,OutDir}]++Opt),
	?line ok = asn1ct:compile(filename:join([DataDir2,"UnidialoguePDUs.asn"]),[Rules,{outdir,OutDir}]++Opt).


