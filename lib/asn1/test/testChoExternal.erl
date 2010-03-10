%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(testChoExternal).


-export([compile/3]).
-export([external/1]).

-include_lib("test_server/include/test_server.hrl").
-include("External.hrl").



compile(Config,Rules,Optimize) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "ChoExternal",[Rules,{outdir,OutDir}]++Optimize).



external(_Rules) ->

    ?line {ok,Bytes11} = asn1_wrapper:encode('ChoExternal','ChoXCho',{'ChoXCho',{boolCho,true}}),
    ?line {ok,{boolCho,true}} = asn1_wrapper:decode('ChoExternal','ChoXCho',lists:flatten(Bytes11)),
    
    
    ?line {ok,Bytes12} = asn1_wrapper:encode('ChoExternal','ChoXCho',{'ChoXCho',{intCho,77}}),
    ?line {ok,{intCho,77}} = asn1_wrapper:decode('ChoExternal','ChoXCho',lists:flatten(Bytes12)),
    
    
    
    ?line {ok,Bytes21} = asn1_wrapper:encode('ChoExternal','ChoXBool',{'ChoXBool',{xbool,true}}),
    ?line {ok,{xbool,true}} = asn1_wrapper:decode('ChoExternal','ChoXBool',lists:flatten(Bytes21)),
    
    
    ?line {ok,Bytes22} = asn1_wrapper:encode('ChoExternal','ChoXBool',{'ChoXBool',{xboolImp,true}}),
    ?line {ok,{xboolImp,true}} = asn1_wrapper:decode('ChoExternal','ChoXBool',lists:flatten(Bytes22)),
    
    
    ?line {ok,Bytes23} = asn1_wrapper:encode('ChoExternal','ChoXBool',{'ChoXBool',{xboolExp,true}}),
    ?line {ok,{xboolExp,true}} = asn1_wrapper:decode('ChoExternal','ChoXBool',lists:flatten(Bytes23)),
    
    
    
    ?line {ok,Bytes31} = asn1_wrapper:encode('ChoExternal','NT',{os,"kalle"}),
    ?line {ok,{os,"kalle"}} = asn1_wrapper:decode('ChoExternal','NT',lists:flatten(Bytes31)),
    
    ?line {ok,Bytes32} = asn1_wrapper:encode('ChoExternal','Exp',{os,"kalle"}),
    ?line {ok,{os,"kalle"}} = asn1_wrapper:decode('ChoExternal','Exp',lists:flatten(Bytes32)),
    
    ?line {ok,Bytes33} = asn1_wrapper:encode('ChoExternal','NTNT',{os,"kalle"}),
    ?line {ok,{os,"kalle"}} = asn1_wrapper:decode('ChoExternal','NTNT',lists:flatten(Bytes33)),
    
    ?line {ok,Bytes34} = asn1_wrapper:encode('ChoExternal','NTExp',{os,"kalle"}),
    ?line {ok,{os,"kalle"}} = asn1_wrapper:decode('ChoExternal','NTExp',lists:flatten(Bytes34)),
    
    ?line {ok,Bytes35} = asn1_wrapper:encode('ChoExternal','ExpNT',{os,"kalle"}),
    ?line {ok,{os,"kalle"}} = asn1_wrapper:decode('ChoExternal','ExpNT',lists:flatten(Bytes35)),
    
    ?line {ok,Bytes36} = asn1_wrapper:encode('ChoExternal','ExpExp',{os,"kalle"}),
    ?line {ok,{os,"kalle"}} = asn1_wrapper:decode('ChoExternal','ExpExp',lists:flatten(Bytes36)),
    
    
    
    
    
    ?line {ok,Bytes41} = asn1_wrapper:encode('ChoExternal','XNTNT',{os,"kalle"}),
    ?line {ok,{os,"kalle"}} = asn1_wrapper:decode('ChoExternal','XNTNT',lists:flatten(Bytes41)),
    
    ?line {ok,Bytes42} = asn1_wrapper:encode('ChoExternal','XNTExp',{os,"kalle"}),
    ?line {ok,{os,"kalle"}} = asn1_wrapper:decode('ChoExternal','XNTExp',lists:flatten(Bytes42)),
    
    ?line {ok,Bytes43} = asn1_wrapper:encode('ChoExternal','XExpNT',{os,"kalle"}),
    ?line {ok,{os,"kalle"}} = asn1_wrapper:decode('ChoExternal','XExpNT',lists:flatten(Bytes43)),
    
    ?line {ok,Bytes44} = asn1_wrapper:encode('ChoExternal','XExpExp',{os,"kalle"}),
    ?line {ok,{os,"kalle"}} = asn1_wrapper:decode('ChoExternal','XExpExp',lists:flatten(Bytes44)),
    
    ok.

