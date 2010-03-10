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
-module(testSeqDefault).

-include("External.hrl").
-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('SeqDef1',{bool1 = asn1_DEFAULT, int1, seq1 = asn1_DEFAULT}).
-record('SeqDef1Imp',{bool1 = asn1_DEFAULT, int1, seq1 = asn1_DEFAULT}).
-record('SeqDef1Exp',{bool1 = asn1_DEFAULT, int1, seq1 = asn1_DEFAULT}).
-record('SeqDef2',{seq2 = asn1_DEFAULT, bool2 = asn1_DEFAULT, int2}).
-record('SeqDef2Imp',{seq2 = asn1_DEFAULT, bool2 = asn1_DEFAULT, int2}).
-record('SeqDef2Exp',{seq2 = asn1_DEFAULT, bool2, int2}).
-record('SeqDef3',{bool3 = asn1_DEFAULT, seq3 = asn1_DEFAULT, int3 = asn1_DEFAULT}).
-record('SeqDef3Imp',{bool3 = asn1_DEFAULT, seq3 = asn1_DEFAULT, int3 = asn1_DEFAULT}).
-record('SeqDef3Exp',{bool3 = asn1_DEFAULT, seq3 = asn1_DEFAULT, int3 = asn1_DEFAULT}).
-record('SeqIn',{boolIn, intIn}).


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SeqDefault",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    
    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SeqDefault','SeqDef1',#'SeqDef1'{bool1 = true,
							int1 = 15,
							seq1 = #'SeqIn'{boolIn = true,
									intIn = 66}}), 
    ?line {ok,{'SeqDef1',true,15,{'SeqIn',true,66}}} = 
	asn1_wrapper:decode('SeqDefault','SeqDef1',lists:flatten(Bytes11)),
    
    
    ?line {ok,Bytes12} = asn1_wrapper:encode('SeqDefault','SeqDef1',#'SeqDef1'{int1 = 15}), 
    ?line {ok,{'SeqDef1',true,15,{'SeqIn',asn1_NOVALUE,12}}} = 
	asn1_wrapper:decode('SeqDefault','SeqDef1',lists:flatten(Bytes12)),
    
    
    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SeqDefault','SeqDef2',#'SeqDef2'{bool2 = true,
							int2 = 15,
							seq2 = #'SeqIn'{boolIn = true,
									intIn = 66}}), 
    ?line {ok,{'SeqDef2',{'SeqIn',true,66},true,15}} = 
	asn1_wrapper:decode('SeqDefault','SeqDef2',lists:flatten(Bytes21)),
    
    
    ?line {ok,Bytes22} = asn1_wrapper:encode('SeqDefault','SeqDef2',#'SeqDef2'{int2 = 15}), 
    ?line {ok,{'SeqDef2',{'SeqIn',asn1_NOVALUE,12},true,15}} = 
	asn1_wrapper:decode('SeqDefault','SeqDef2',lists:flatten(Bytes22)),
    
    
    
    ?line {ok,Bytes31} = 
	asn1_wrapper:encode('SeqDefault','SeqDef3',#'SeqDef3'{bool3 = true,
							int3 = 15,
							seq3 = #'SeqIn'{boolIn = true,
									intIn = 66}}), 
    ?line {ok,{'SeqDef3',true,{'SeqIn',true,66},15}} = 
	asn1_wrapper:decode('SeqDefault','SeqDef3',lists:flatten(Bytes31)),
    
    
    ?line {ok,Bytes32} = asn1_wrapper:encode('SeqDefault','SeqDef3',#'SeqDef3'{int3 = 15}), 
    ?line {ok,{'SeqDef3',true,{'SeqIn',asn1_NOVALUE,12},15}} = 
	asn1_wrapper:decode('SeqDefault','SeqDef3',lists:flatten(Bytes32)),
    
    
    
    
    
    ?line {ok,Bytes41} = 
	asn1_wrapper:encode('SeqDefault','SeqDef1Imp',#'SeqDef1Imp'{bool1 = true,
							      int1 = 15,
							      seq1 = #'SeqIn'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SeqDef1Imp',true,15,{'SeqIn',true,66}}} = 
	asn1_wrapper:decode('SeqDefault','SeqDef1Imp',lists:flatten(Bytes41)),
    
    
    ?line {ok,Bytes42} = asn1_wrapper:encode('SeqDefault','SeqDef1Imp',#'SeqDef1Imp'{int1 = 15}), 
    ?line {ok,{'SeqDef1Imp',true,15,{'SeqIn',asn1_NOVALUE,12}}} = 
	asn1_wrapper:decode('SeqDefault','SeqDef1Imp',lists:flatten(Bytes42)),
    
    
    ?line {ok,Bytes51} = 
	asn1_wrapper:encode('SeqDefault','SeqDef2Imp',#'SeqDef2Imp'{bool2 = true,
							      int2 = 15,
							      seq2 = #'SeqIn'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SeqDef2Imp',{'SeqIn',true,66},true,15}} = 
	asn1_wrapper:decode('SeqDefault','SeqDef2Imp',lists:flatten(Bytes51)),
    
    
    ?line {ok,Bytes52} = asn1_wrapper:encode('SeqDefault','SeqDef2Imp',#'SeqDef2Imp'{int2 = 15}), 
    ?line {ok,{'SeqDef2Imp',{'SeqIn',asn1_NOVALUE,12},true,15}} = 
	asn1_wrapper:decode('SeqDefault','SeqDef2Imp',lists:flatten(Bytes52)),
    
    
    
    ?line {ok,Bytes61} = 
	asn1_wrapper:encode('SeqDefault','SeqDef3Imp',#'SeqDef3Imp'{bool3 = true,
							      int3 = 15,
							      seq3 = #'SeqIn'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SeqDef3Imp',true,{'SeqIn',true,66},15}} = 
	asn1_wrapper:decode('SeqDefault','SeqDef3Imp',lists:flatten(Bytes61)),
    
    
    ?line {ok,Bytes62} = asn1_wrapper:encode('SeqDefault','SeqDef3Imp',#'SeqDef3Imp'{int3 = 15}), 
    ?line {ok,{'SeqDef3Imp',true,{'SeqIn',asn1_NOVALUE,12},15}} = 
	asn1_wrapper:decode('SeqDefault','SeqDef3Imp',lists:flatten(Bytes62)),
    
    
    
    
    
    
    ?line {ok,Bytes71} = 
	asn1_wrapper:encode('SeqDefault','SeqDef1Exp',#'SeqDef1Exp'{bool1 = true,
							      int1 = 15,
							      seq1 = #'SeqIn'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SeqDef1Exp',true,15,{'SeqIn',true,66}}} = 
	asn1_wrapper:decode('SeqDefault','SeqDef1Exp',lists:flatten(Bytes71)),
    
    
    ?line {ok,Bytes72} = asn1_wrapper:encode('SeqDefault','SeqDef1Exp',#'SeqDef1Exp'{int1 = 15}), 
    ?line {ok,{'SeqDef1Exp',true,15,{'SeqIn',asn1_NOVALUE,12}}} = 
	asn1_wrapper:decode('SeqDefault','SeqDef1Exp',lists:flatten(Bytes72)),
    
    
    ?line {ok,Bytes81} = 
	asn1_wrapper:encode('SeqDefault','SeqDef2Exp',#'SeqDef2Exp'{bool2 = true,
							      int2 = 15,
							      seq2 = #'SeqIn'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SeqDef2Exp',{'SeqIn',true,66},true,15}} = 
	asn1_wrapper:decode('SeqDefault','SeqDef2Exp',lists:flatten(Bytes81)),
    
    
    ?line {ok,Bytes82} = asn1_wrapper:encode('SeqDefault','SeqDef2Exp',#'SeqDef2Exp'{int2 = 15,
									       bool2 = true}), 
    ?line {ok,{'SeqDef2Exp',{'SeqIn',asn1_NOVALUE,12},true,15}} = 
	asn1_wrapper:decode('SeqDefault','SeqDef2Exp',lists:flatten(Bytes82)),
    
    
    
    ?line {ok,Bytes91} = 
	asn1_wrapper:encode('SeqDefault','SeqDef3Exp',#'SeqDef3Exp'{bool3 = true,
							      int3 = 15,
							      seq3 = #'SeqIn'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SeqDef3Exp',true,{'SeqIn',true,66},15}} = 
	asn1_wrapper:decode('SeqDefault','SeqDef3Exp',lists:flatten(Bytes91)),
    
    
    ?line {ok,Bytes92} = asn1_wrapper:encode('SeqDefault','SeqDef3Exp',#'SeqDef3Exp'{int3 = 15}), 
    ?line {ok,{'SeqDef3Exp',true,{'SeqIn',asn1_NOVALUE,12},15}} = 
	asn1_wrapper:decode('SeqDefault','SeqDef3Exp',lists:flatten(Bytes92)),
    
    
    
    
    ok.
