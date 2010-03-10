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
-module(testChoTypeRefPrim).

-export([compile/3]).
-export([prim/1]).

-include_lib("test_server/include/test_server.hrl").



compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "ChoTypeRefPrim",[Rules,{outdir,OutDir}]++Options).



prim(_Rules) ->
    
    ?line {ok,Bytes11} = asn1_wrapper:encode('ChoTypeRefPrim','ChoTR',{bool,true}),
    ?line {ok,{bool,true}} = asn1_wrapper:decode('ChoTypeRefPrim','ChoTR',lists:flatten(Bytes11)),
    
    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('ChoTypeRefPrim','ChoTR',{octStr,[11,12,13,14,15,16,17]}),
    ?line {ok,{octStr,[11,12,13,14,15,16,17]}} = 
	asn1_wrapper:decode('ChoTypeRefPrim','ChoTR',lists:flatten(Bytes12)),
    
    ?line {ok,Bytes13} = asn1_wrapper:encode('ChoTypeRefPrim','ChoTR',{int,233}),
    ?line {ok,{int,233}} = asn1_wrapper:decode('ChoTypeRefPrim','ChoTR',lists:flatten(Bytes13)),
    
    ?line {ok,Bytes14} = 
	asn1_wrapper:encode('ChoTypeRefPrim','ChoTR',{octStr,"Stringing in the rain"}),
    ?line {ok,{octStr,"Stringing in the rain"}} = 
	asn1_wrapper:decode('ChoTypeRefPrim','ChoTR',lists:flatten(Bytes14)),
    
    
    
    ?line {ok,Bytes21} = asn1_wrapper:encode('ChoTypeRefPrim','ChoTR2',{'octStr',"A string"}),
    ?line {ok,{'octStr',"A string"}} = 
	asn1_wrapper:decode('ChoTypeRefPrim','ChoTR2',lists:flatten(Bytes21)),
    
    ?line {ok,Bytes22} = asn1_wrapper:encode('ChoTypeRefPrim','ChoTR2',{'octStrI',"A string"}),
    ?line {ok,{'octStrI',"A string"}} = 
	asn1_wrapper:decode('ChoTypeRefPrim','ChoTR2',lists:flatten(Bytes22)),
    
    ?line {ok,Bytes23} = asn1_wrapper:encode('ChoTypeRefPrim','ChoTR2',{'octStrE',"A string"}),
    ?line {ok,{'octStrE',"A string"}} = 
	asn1_wrapper:decode('ChoTypeRefPrim','ChoTR2',lists:flatten(Bytes23)),
    
    ?line {ok,Bytes24} = asn1_wrapper:encode('ChoTypeRefPrim','ChoTR2',{'octStr-I',"A string"}),
    ?line {ok,{'octStr-I',"A string"}} = 
	asn1_wrapper:decode('ChoTypeRefPrim','ChoTR2',lists:flatten(Bytes24)),
    
    ?line {ok,Bytes25} = asn1_wrapper:encode('ChoTypeRefPrim','ChoTR2',{'octStrI-I',"A string"}),
    ?line {ok,{'octStrI-I',"A string"}} = 
	asn1_wrapper:decode('ChoTypeRefPrim','ChoTR2',lists:flatten(Bytes25)),
    
    ?line {ok,Bytes26} = asn1_wrapper:encode('ChoTypeRefPrim','ChoTR2',{'octStrE-I',"A string"}),
    ?line {ok,{'octStrE-I',"A string"}} = 
	asn1_wrapper:decode('ChoTypeRefPrim','ChoTR2',lists:flatten(Bytes26)),
    
    ?line {ok,Bytes27} = asn1_wrapper:encode('ChoTypeRefPrim','ChoTR2',{'octStr-E',"A string"}),
    ?line {ok,{'octStr-E',"A string"}} = 
	asn1_wrapper:decode('ChoTypeRefPrim','ChoTR2',lists:flatten(Bytes27)),
    
    ?line {ok,Bytes28} = asn1_wrapper:encode('ChoTypeRefPrim','ChoTR2',{'octStrI-E',"A string"}),
    ?line {ok,{'octStrI-E',"A string"}} = 
	asn1_wrapper:decode('ChoTypeRefPrim','ChoTR2',lists:flatten(Bytes28)),
    
    ?line {ok,Bytes29} = asn1_wrapper:encode('ChoTypeRefPrim','ChoTR2',{'octStrE-E',"A string"}),
    ?line {ok,{'octStrE-E',"A string"}} = 
	asn1_wrapper:decode('ChoTypeRefPrim','ChoTR2',lists:flatten(Bytes29)),
    
    
    ok.
