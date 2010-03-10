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
-module(testChoOptional).


-export([compile/3]).
-export([optional/1]).

%-include("ChoOptional.hrl").
-include_lib("test_server/include/test_server.hrl").
-include("External.hrl").


-record('Seq1',{bool, int = asn1_NOVALUE, cho = asn1_NOVALUE}).
-record('Seq2',{int = asn1_NOVALUE, cho = asn1_NOVALUE, bool}).
-record('Seq3',{cho = asn1_NOVALUE, int = asn1_NOVALUE, bool}).


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "ChoOptional",[Rules,{outdir,OutDir}]++Options).



optional(_Rules) ->
    
    ?line {ok,Bytes11} = asn1_wrapper:encode('ChoOptional','Seq1',#'Seq1'{bool = true}),
    ?line {ok,{'Seq1',true,asn1_NOVALUE,asn1_NOVALUE}} = 
	asn1_wrapper:decode('ChoOptional','Seq1',lists:flatten(Bytes11)),
    
    ?line {ok,Bytes12} = asn1_wrapper:encode('ChoOptional','Seq1',#'Seq1'{bool = true,
								    int = 233}),
    ?line {ok,{'Seq1',true,233,asn1_NOVALUE}} = 
	asn1_wrapper:decode('ChoOptional','Seq1',lists:flatten(Bytes12)),
    
    ?line {ok,Bytes13} = asn1_wrapper:encode('ChoOptional','Seq1',#'Seq1'{bool = true,
								    cho = {vsCho,"Vs Str"}}),
    ?line {ok,{'Seq1',true,asn1_NOVALUE,{vsCho,"Vs Str"}}} = 
	asn1_wrapper:decode('ChoOptional','Seq1',lists:flatten(Bytes13)),
    
    ?line {ok,Bytes14} = 
	asn1_wrapper:encode('ChoOptional','Seq1',#'Seq1'{bool = true,
						   cho = {ocStrCho,"Oct Str"}}),
    ?line {ok,{'Seq1',true,asn1_NOVALUE,{ocStrCho,"Oct Str"}}} = 
	asn1_wrapper:decode('ChoOptional','Seq1',lists:flatten(Bytes14)),
    
    
    
    ?line {ok,Bytes21} = asn1_wrapper:encode('ChoOptional','Seq2',#'Seq2'{bool = true}),
    ?line {ok,{'Seq2',asn1_NOVALUE,asn1_NOVALUE,true}} = 
	asn1_wrapper:decode('ChoOptional','Seq2',lists:flatten(Bytes21)),
    
    ?line {ok,Bytes22} = asn1_wrapper:encode('ChoOptional','Seq2',#'Seq2'{bool = true,
								    int = 233}),
    ?line {ok,{'Seq2',233,asn1_NOVALUE,true}} = 
	asn1_wrapper:decode('ChoOptional','Seq2',lists:flatten(Bytes22)),
    
    ?line {ok,Bytes23} = asn1_wrapper:encode('ChoOptional','Seq2',#'Seq2'{bool = true,
								    cho = {vsCho,"Vs Str"}}),
    ?line {ok,{'Seq2',asn1_NOVALUE,{vsCho,"Vs Str"},true}} = 
	asn1_wrapper:decode('ChoOptional','Seq2',lists:flatten(Bytes23)),
    
    ?line {ok,Bytes24} = 
	asn1_wrapper:encode('ChoOptional','Seq2',#'Seq2'{bool = true,
						   cho = {ocStrCho,"Oct Str"}}),
    ?line {ok,{'Seq2',asn1_NOVALUE,{ocStrCho,"Oct Str"},true}} = 
	asn1_wrapper:decode('ChoOptional','Seq2',lists:flatten(Bytes24)),
    
    
    
    ?line {ok,Bytes31} = asn1_wrapper:encode('ChoOptional','Seq3',#'Seq3'{bool = true}),
    ?line {ok,{'Seq3',asn1_NOVALUE,asn1_NOVALUE,true}} = 
	asn1_wrapper:decode('ChoOptional','Seq3',lists:flatten(Bytes31)),
    
    ?line {ok,Bytes32} = asn1_wrapper:encode('ChoOptional','Seq3',#'Seq3'{bool = true,
								    int = 233}),
    ?line {ok,{'Seq3',asn1_NOVALUE,233,true}} = 
	asn1_wrapper:decode('ChoOptional','Seq3',lists:flatten(Bytes32)),
    
    ?line {ok,Bytes33} = asn1_wrapper:encode('ChoOptional','Seq3',#'Seq3'{bool = true,
								    cho = {vsCho,"Vs Str"}}),
    ?line {ok,{'Seq3',{vsCho,"Vs Str"},asn1_NOVALUE,true}} = 
	asn1_wrapper:decode('ChoOptional','Seq3',lists:flatten(Bytes33)),
    
    ?line {ok,Bytes34} = 
	asn1_wrapper:encode('ChoOptional','Seq3',#'Seq3'{bool = true,
						   cho = {ocStrCho,"Oct Str"}}),
    ?line {ok,{'Seq3',{ocStrCho,"Oct Str"},asn1_NOVALUE,true}} = 
	asn1_wrapper:decode('ChoOptional','Seq3',lists:flatten(Bytes34)),
    
    
    
    ok.
