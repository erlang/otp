%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
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

-module(testInfObj).

-export([compile/3,main/1,compile_RANAPfiles/3]).

-include_lib("test_server/include/test_server.hrl").

-record('InitiatingMessage',{procedureCode,criticality,value}).
-record('InitiatingMessage2',{procedureCode,criticality,value}).
-record('Iu-ReleaseCommand',{first,second}).

compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),

    ?line ok = asn1ct:compile(DataDir ++ 
			      "RANAPextract1",[Rules,{outdir,OutDir}]++Options),
    ?line ok = asn1ct:compile(DataDir ++ "InfObj",[Rules,{outdir,OutDir}]++Options),
    %% test case for OTP-4792 optional open type
    ?line ok = asn1ct:compile(DataDir ++ "MAP-ExtensionDataTypes",[Rules,{outdir,OutDir}]++Options),
    %% OTP-6707
    ?line ok = asn1ct:compile(DataDir ++ "Objects",[Rules,{outdir,OutDir}]++Options),
    %% OTP-6717
    ?line ok = asn1ct:compile(DataDir ++ "INAPv2extract",[Rules,{outdir,OutDir}]++Options).


compile_RANAPfiles(Config,Rules,Options) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),

    ?line ok = asn1ct:compile(DataDir ++ "RANAP-CommonDataTypes",
			      [Rules,{outdir,OutDir}]++Options),
    ?line ok = asn1ct:compile(DataDir ++ "RANAP-Constants",
			      [Rules,{outdir,OutDir}]++Options),
    ?line ok = asn1ct:compile(DataDir ++ "RANAP-Containers",
			      [Rules,{outdir,OutDir}]++Options),
    ?line ok = asn1ct:compile(DataDir ++ "RANAP-IEs",
			      [Rules,{outdir,OutDir}]++Options),
    ?line ok = asn1ct:compile(DataDir ++ "RANAP-PDU-Contents",
			      [Rules,{outdir,OutDir}]++Options),
    ?line ok = asn1ct:compile(DataDir ++ "RANAP-PDU-Descriptions",
			      [Rules,{outdir,OutDir}]++Options).


main(_Erule) ->
    Val1 = #'InitiatingMessage'{procedureCode=1,
				criticality=ignore,
				value=#'Iu-ReleaseCommand'{
				  first=13,
				  second=true}},
    ?line {ok,Bytes1} = 
	asn1_wrapper:encode('RANAPextract1','InitiatingMessage',Val1),
    
    ?line {ok,{'InitiatingMessage',1,ignore,{'Iu-ReleaseCommand',13,true}}}=
	asn1_wrapper:decode('RANAPextract1','InitiatingMessage',Bytes1),
    
    ?line {ok,Bytes2} =
	asn1_wrapper:encode('InfObj','InitiatingMessage',Val1),
    
    ?line {ok,Val1} =
	asn1_wrapper:decode('InfObj','InitiatingMessage',Bytes2),

    Val2 = Val1#'InitiatingMessage'{procedureCode=2},
    
    ?line {error,_R1} =
	asn1_wrapper:encode('InfObj','InitiatingMessage',Val2),
    

    %% Test case for OTP-4275
    Val3 = #'InitiatingMessage2'{procedureCode=3,
				 criticality=reject,
				 value=#'Iu-ReleaseCommand'{
				   first=13,
				   second=true}},

    ?line {ok,Bytes3} = 
	asn1_wrapper:encode('RANAPextract1','InitiatingMessage2',Val3),

    
    ?line {ok,{'InitiatingMessage2',3,reject,{'Iu-ReleaseCommand',13,true}}}=
	asn1_wrapper:decode('RANAPextract1','InitiatingMessage2',Bytes3).
    
