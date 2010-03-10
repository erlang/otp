%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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
-module(testTCAP).

-export([compile/3,test/2,compile_asn1config/3,test_asn1config/0]).

-include_lib("test_server/include/test_server.hrl").



compile(Config,Rules,Opt) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    
    ?line ok = asn1ct:compile(DataDir ++ "Remote-Operations-Information-Objects",[Rules,{outdir,OutDir}]++Opt),
%    ?line ok = asn1ct:compile(DataDir ++ "Remote-Operations-Generic-ROS-PDUs",[Rules,{outdir,OutDir}]++Opt),
%    ?line ok = asn1ct:compile(DataDir ++ "Remote-Operations-Useful-Definitions",[Rules,{outdir,OutDir}]++Opt),
    ?line ok = asn1ct:compile(DataDir ++ "TCAPMessages",[Rules,{outdir,OutDir}]++Opt),
    ?line ok = asn1ct:compile(DataDir ++ "TCAPMessages-simple",[Rules,{outdir,OutDir}]++Opt),
    ?line ok = asn1ct:compile(DataDir ++ "TCAPPackage",[Rules,{outdir,OutDir}]++Opt),
    ?line compile:file(filename:join([DataDir,"TCAPPackage_msg"]),[{i,OutDir},{outdir,OutDir}]).

compile_asn1config(Config,Rules,Opt) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    
    ?line ok = asn1ct:compile(DataDir ++ "TCAPPackage",
			      [Rules,{outdir,OutDir},{i,DataDir}]++Opt).

test(Erule,_Config) when Erule==ber;Erule==ber_bin;Erule==ber_bin_v2 ->
%    ?line OutDir = ?config(priv_dir,Config),
    %% testing OTP-4798, open type encoded with indefinite length
    ?line {ok,_Res} = asn1_wrapper:decode('TCAPMessages-simple','MessageType', val_OTP_4798(Erule)),
    %% testing OTP-4799, absent optional open type
    ?line {ok,_Res2} = asn1_wrapper:decode('TCAPMessages-simple','MessageType',val_OTP_4799(Erule)),
    %% testing vance shipley's problems. Parameterized object sets.
    ?line Val3 = 'TCAPPackage_msg':val('PackageType',unidirectional),
    ?line {ok,Bytes3} = asn1_wrapper:encode('TCAPPackage','PackageType',Val3),
    ?line {ok,Res3} = asn1_wrapper:decode('TCAPPackage','PackageType',Bytes3),
    ?line ok = 'TCAPPackage_msg':check_result('PackageType',unidirectional,Res3),
%%    ?line io:format("Res3:~n~p~n~n",[Res3]),
    
    ?line Val4 = 'TCAPPackage_msg':val('PackageType',abort),
    ?line {ok,Bytes4} = asn1_wrapper:encode('TCAPPackage','PackageType',Val4),
    ?line {ok,Res4} = asn1_wrapper:decode('TCAPPackage','PackageType',Bytes4),
    ?line ok = 'TCAPPackage_msg':check_result('PackageType',abort,Res4),
%%    ?line io:format("Res4:~n~p~n~n",[Res4]),

    ?line Val5 = 'TCAPPackage_msg':val('PackageType',response),
    ?line {ok,Bytes5} = asn1_wrapper:encode('TCAPPackage','PackageType',Val5),
    ?line {ok,Res5} = asn1_wrapper:decode('TCAPPackage','PackageType',Bytes5),
    ?line ok = 'TCAPPackage_msg':check_result('PackageType',response,Res5).
%%    ?line io:format("Res5:~n~p~n~n",[Res5]).

val_OTP_4798(ber) ->
    [100,129,176,73,4,57,3,17,80,107,42,40,40,6,7,0,17,134,5,1,1,1,160,29,97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,14,2,162,3,2,1,0,163,5,161,3,2,1,0,108,128,162,120,2,1,0,48,115,2,1,56,48,128,48,34,4,16,203,87,215,196,217,93,235,90,64,131,106,145,39,26,25,236,4,4,197,241,81,112,4,8,78,225,34,196,215,212,200,0,48,34,4,16,145,125,27,67,42,144,6,161,207,112,55,75,200,191,191,28,4,4,226,219,242,123,4,8,72,46,130,28,206,178,168,0,48,34,4,16,1,8,20,29,70,160,218,160,125,188,244,174,113,115,253,245,4,4,26,5,90,160,4,8,252,75,149,98,153,224,140,0,0,0,0,0];
val_OTP_4798(_) ->
    <<100,129,176,73,4,57,3,17,80,107,42,40,40,6,7,0,17,134,5,1,1,1,160,29,97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,14,2,162,3,2,1,0,163,5,161,3,2,1,0,108,128,162,120,2,1,0,48,115,2,1,56,48,128,48,34,4,16,203,87,215,196,217,93,235,90,64,131,106,145,39,26,25,236,4,4,197,241,81,112,4,8,78,225,34,196,215,212,200,0,48,34,4,16,145,125,27,67,42,144,6,161,207,112,55,75,200,191,191,28,4,4,226,219,242,123,4,8,72,46,130,28,206,178,168,0,48,34,4,16,1,8,20,29,70,160,218,160,125,188,244,174,113,115,253,245,4,4,26,5,90,160,4,8,252,75,149,98,153,224,140,0,0,0,0,0>>.

val_OTP_4799(ber) ->
    [100,16,73,4,41,182,36,0,108,8,163,6,2,1,29,2,1,27];
val_OTP_4799(_) ->
    <<100,16,73,4,41,182,36,0,108,8,163,6,2,1,29,2,1,27>>.

test_asn1config() ->
    ?line Val = 'TCAPPackage_msg':val('PackageType',queryWithPerm),
    ?line {ok,B} = asn1_wrapper:encode('TCAPPackage','PackageType',Val),
    ?line {ok,ExMsg}='TCAPPackage':decode_PackageType(list_to_binary(B)),
    ?line {_,{_,_,_,{Key,ExVal}}}=ExMsg,
    ?line {ok,_Parts}='TCAPPackage':decode_part(Key,ExVal),
    
    ?line Val2 = 'TCAPPackage_msg':val('TransactionPDU'),
    ?line {ok,B2} = 'TCAPPackage':encode('TransactionPDU',Val2),
    ?line {ok,ExMsg2}='TCAPPackage':decode_TransactionPDU(list_to_binary(B2)),
    ?line {_,_,_,{Key2,ExVal2}}=ExMsg2,
    ?line {ok,_Parts2}='TCAPPackage':decode_part(Key2,ExVal2),

    ?line Val3 = 'TCAPPackage_msg':val('PackageType',response),
    ?line {ok,B3} = asn1_wrapper:encode('TCAPPackage','PackageType',Val3),
    ?line {ok,ExMsg3}='TCAPPackage':decode_PackageType(list_to_binary(B3)),
    ?line {_,{_,_,_,{Key3,ExVal3}}}=ExMsg3,
    ?line {ok,_Parts3}='TCAPPackage':decode_part(Key3,ExVal3).
    
