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
-module(testChoTypeRefCho).

-export([compile/3]).
-export([choice/1]).

-include_lib("test_server/include/test_server.hrl").



compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "ChoTypeRefCho",[Rules,{outdir,OutDir}]++Options).



choice(_Rules) ->
    
    ?line {ok,Bytes11} = asn1_wrapper:encode('ChoTypeRefCho','ChoTRcho',{choCho,{choInt,88}}),
    ?line {ok,{choCho,{choInt,88}}} = 
	asn1_wrapper:decode('ChoTypeRefCho','ChoTRcho',lists:flatten(Bytes11)),
    
    ?line {ok,Bytes12} = asn1_wrapper:encode('ChoTypeRefCho','ChoTRcho',{choChoE,{choInt,88}}),
    ?line {ok,{choChoE,{choInt,88}}} = 
	asn1_wrapper:decode('ChoTypeRefCho','ChoTRcho',lists:flatten(Bytes12)),
    
    ?line {ok,Bytes13} = asn1_wrapper:encode('ChoTypeRefCho','ChoTRcho',{'choCho-E',{choInt,88}}),
    ?line {ok,{'choCho-E',{choInt,88}}} = 
	asn1_wrapper:decode('ChoTypeRefCho','ChoTRcho',lists:flatten(Bytes13)),
    
    ?line {ok,Bytes14} = asn1_wrapper:encode('ChoTypeRefCho','ChoTRcho',{'choChoE-E',{choInt,88}}),
    ?line {ok,{'choChoE-E',{choInt,88}}} = 
	asn1_wrapper:decode('ChoTypeRefCho','ChoTRcho',lists:flatten(Bytes14)),
    
    

    ?line {ok,Bytes21} = asn1_wrapper:encode('ChoTypeRefCho','ChoChoInline',{bool1,true}),
    ?line {ok,{bool1,true}} = 
	asn1_wrapper:decode('ChoTypeRefCho','ChoChoInline',lists:flatten(Bytes21)),
    
    ?line {ok,Bytes22} = asn1_wrapper:encode('ChoTypeRefCho','ChoChoInline',{'choCho',{bool,true}}),
    ?line {ok,{'choCho',{bool,true}}} = 
	asn1_wrapper:decode('ChoTypeRefCho','ChoChoInline',lists:flatten(Bytes22)),
    
    ?line {ok,Bytes23} = asn1_wrapper:encode('ChoTypeRefCho','ChoChoInline',{'choCho',{octStr,"kk"}}),
    ?line {ok,{'choCho',{octStr,"kk"}}} = 
	asn1_wrapper:decode('ChoTypeRefCho','ChoChoInline',lists:flatten(Bytes23)),
    
    ?line {ok,Bytes24} = asn1_wrapper:encode('ChoTypeRefCho','ChoChoInline',{'choCho',{int,55}}),
    ?line {ok,{'choCho',{int,55}}} = 
	asn1_wrapper:decode('ChoTypeRefCho','ChoChoInline',lists:flatten(Bytes24)),
    
    
    
    
    
    ok.
