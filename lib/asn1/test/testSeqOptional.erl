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
-module(testSeqOptional).

-include("External.hrl").
-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('SeqOpt1',{bool1 = asn1_NOVALUE, int1, seq1 = asn1_NOVALUE}).
-record('SeqOpt1Imp',{bool1 = asn1_NOVALUE, int1, seq1 = asn1_NOVALUE}).
-record('SeqOpt1Exp',{bool1 = asn1_NOVALUE, int1, seq1 = asn1_NOVALUE}).
-record('SeqOpt2',{seq2 = asn1_NOVALUE, bool2, int2}).
-record('SeqOpt2Imp',{seq2 = asn1_NOVALUE, bool2, int2}).
-record('SeqOpt2Exp',{seq2 = asn1_NOVALUE, bool2, int2}).
-record('SeqOpt3',{bool3 = asn1_NOVALUE, seq3 = asn1_NOVALUE, int3 = asn1_NOVALUE}).
-record('SeqOpt3Imp',{bool3 = asn1_NOVALUE, seq3 = asn1_NOVALUE, int3 = asn1_NOVALUE}).
-record('SeqOpt3Exp',{bool3 = asn1_NOVALUE, seq3 = asn1_NOVALUE, int3 = asn1_NOVALUE}).
-record('SeqIn',{boolIn, intIn}).
-record('SeqChoOpt',{int, cho = asn1_NOVALUE}).


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SeqOptional",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    
    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SeqOptional','SeqOpt1',#'SeqOpt1'{bool1 = true,
							int1 = 15,
							seq1 = #'SeqIn'{boolIn = true,
									intIn = 66}}), 
    ?line {ok,{'SeqOpt1',true,15,{'SeqIn',true,66}}} = 
	asn1_wrapper:decode('SeqOptional','SeqOpt1',lists:flatten(Bytes11)),
    
    
    ?line {ok,Bytes12} = asn1_wrapper:encode('SeqOptional','SeqOpt1',#'SeqOpt1'{int1 = 15}), 
    ?line {ok,{'SeqOpt1',asn1_NOVALUE,15,asn1_NOVALUE}} = 
	asn1_wrapper:decode('SeqOptional','SeqOpt1',lists:flatten(Bytes12)),
    
    
    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SeqOptional','SeqOpt2',#'SeqOpt2'{bool2 = true,
							int2 = 15,
							seq2 = #'SeqIn'{boolIn = true,
									intIn = 66}}), 
    ?line {ok,{'SeqOpt2',{'SeqIn',true,66},true,15}} = 
	asn1_wrapper:decode('SeqOptional','SeqOpt2',lists:flatten(Bytes21)),
    
    
    ?line {ok,Bytes22} = asn1_wrapper:encode('SeqOptional','SeqOpt2',#'SeqOpt2'{int2 = 15,
									  bool2 = true}), 
    ?line {ok,{'SeqOpt2',asn1_NOVALUE,true,15}} = 
	asn1_wrapper:decode('SeqOptional','SeqOpt2',lists:flatten(Bytes22)),
    
    
    
    ?line {ok,Bytes31} = 
	asn1_wrapper:encode('SeqOptional','SeqOpt3',#'SeqOpt3'{bool3 = true,
							int3 = 15,
							seq3 = #'SeqIn'{boolIn = true,
									intIn = 66}}), 
    ?line {ok,{'SeqOpt3',true,{'SeqIn',true,66},15}} = 
	asn1_wrapper:decode('SeqOptional','SeqOpt3',lists:flatten(Bytes31)),
    
    
    ?line {ok,Bytes32} = asn1_wrapper:encode('SeqOptional','SeqOpt3',#'SeqOpt3'{int3 = 15}), 
    ?line {ok,{'SeqOpt3',asn1_NOVALUE,asn1_NOVALUE,15}} = 
	asn1_wrapper:decode('SeqOptional','SeqOpt3',lists:flatten(Bytes32)),
    
    
    
    
    
    ?line {ok,Bytes41} = 
	asn1_wrapper:encode('SeqOptional','SeqOpt1Imp',#'SeqOpt1Imp'{bool1 = true,
							      int1 = 15,
							      seq1 = #'SeqIn'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SeqOpt1Imp',true,15,{'SeqIn',true,66}}} = 
	asn1_wrapper:decode('SeqOptional','SeqOpt1Imp',lists:flatten(Bytes41)),
    
    
    ?line {ok,Bytes42} = asn1_wrapper:encode('SeqOptional','SeqOpt1Imp',#'SeqOpt1Imp'{int1 = 15}), 
    ?line {ok,{'SeqOpt1Imp',asn1_NOVALUE,15,asn1_NOVALUE}} = 
	asn1_wrapper:decode('SeqOptional','SeqOpt1Imp',lists:flatten(Bytes42)),
    
    
    ?line {ok,Bytes51} = 
	asn1_wrapper:encode('SeqOptional','SeqOpt2Imp',#'SeqOpt2Imp'{bool2 = true,
							      int2 = 15,
							      seq2 = #'SeqIn'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SeqOpt2Imp',{'SeqIn',true,66},true,15}} = 
	asn1_wrapper:decode('SeqOptional','SeqOpt2Imp',lists:flatten(Bytes51)),
    
    
    ?line {ok,Bytes52} = asn1_wrapper:encode('SeqOptional','SeqOpt2Imp',#'SeqOpt2Imp'{int2 = 15,
									        bool2 = true}), 
    ?line {ok,{'SeqOpt2Imp',asn1_NOVALUE,true,15}} = 
	asn1_wrapper:decode('SeqOptional','SeqOpt2Imp',lists:flatten(Bytes52)),
    
    
    
    ?line {ok,Bytes61} = 
	asn1_wrapper:encode('SeqOptional','SeqOpt3Imp',#'SeqOpt3Imp'{bool3 = true,
							      int3 = 15,
							      seq3 = #'SeqIn'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SeqOpt3Imp',true,{'SeqIn',true,66},15}} = 
	asn1_wrapper:decode('SeqOptional','SeqOpt3Imp',lists:flatten(Bytes61)),
    
    
    ?line {ok,Bytes62} = asn1_wrapper:encode('SeqOptional','SeqOpt3Imp',#'SeqOpt3Imp'{int3 = 15}), 
    ?line {ok,{'SeqOpt3Imp',asn1_NOVALUE,asn1_NOVALUE,15}} = 
	asn1_wrapper:decode('SeqOptional','SeqOpt3Imp',lists:flatten(Bytes62)),
    
    
    
    
    
    
    ?line {ok,Bytes71} = 
	asn1_wrapper:encode('SeqOptional','SeqOpt1Exp',#'SeqOpt1Exp'{bool1 = true,
							      int1 = 15,
							      seq1 = #'SeqIn'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SeqOpt1Exp',true,15,{'SeqIn',true,66}}} = 
	asn1_wrapper:decode('SeqOptional','SeqOpt1Exp',lists:flatten(Bytes71)),
    
    
    ?line {ok,Bytes72} = asn1_wrapper:encode('SeqOptional','SeqOpt1Exp',#'SeqOpt1Exp'{int1 = 15}), 
    ?line {ok,{'SeqOpt1Exp',asn1_NOVALUE,15,asn1_NOVALUE}} = 
	asn1_wrapper:decode('SeqOptional','SeqOpt1Exp',lists:flatten(Bytes72)),
    
    
    ?line {ok,Bytes81} = 
	asn1_wrapper:encode('SeqOptional','SeqOpt2Exp',#'SeqOpt2Exp'{bool2 = true,
							      int2 = 15,
							      seq2 = #'SeqIn'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SeqOpt2Exp',{'SeqIn',true,66},true,15}} = 
	asn1_wrapper:decode('SeqOptional','SeqOpt2Exp',lists:flatten(Bytes81)),
    
    
    ?line {ok,Bytes82} = asn1_wrapper:encode('SeqOptional','SeqOpt2Exp',#'SeqOpt2Exp'{int2 = 15,
									       bool2 = true}), 
    ?line {ok,{'SeqOpt2Exp',asn1_NOVALUE,true,15}} = 
	asn1_wrapper:decode('SeqOptional','SeqOpt2Exp',lists:flatten(Bytes82)),
    
    
    
    ?line {ok,Bytes91} = 
	asn1_wrapper:encode('SeqOptional','SeqOpt3Exp',#'SeqOpt3Exp'{bool3 = true,
							      int3 = 15,
							      seq3 = #'SeqIn'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SeqOpt3Exp',true,{'SeqIn',true,66},15}} = 
	asn1_wrapper:decode('SeqOptional','SeqOpt3Exp',lists:flatten(Bytes91)),
    
    
    ?line {ok,Bytes92} = asn1_wrapper:encode('SeqOptional','SeqOpt3Exp',#'SeqOpt3Exp'{int3 = 15}), 
    ?line {ok,{'SeqOpt3Exp',asn1_NOVALUE,asn1_NOVALUE,15}} = 
	asn1_wrapper:decode('SeqOptional','SeqOpt3Exp',lists:flatten(Bytes92)),
    
    
    
    ?line {ok,Bytes101} = 
	asn1_wrapper:encode('SeqOptional','SeqChoOpt',#'SeqChoOpt'{int = 15,
							     cho = {boolC,true}}), 
    ?line {ok,{'SeqChoOpt',15,{boolC,true}}} = 
	asn1_wrapper:decode('SeqOptional','SeqChoOpt',lists:flatten(Bytes101)),
    
    
    ?line {ok,Bytes102} = asn1_wrapper:encode('SeqOptional','SeqChoOpt',#'SeqChoOpt'{int = 15}), 
    ?line {ok,{'SeqChoOpt',15,asn1_NOVALUE}} = 
	asn1_wrapper:decode('SeqOptional','SeqChoOpt',lists:flatten(Bytes102)),
    
    
    
    
    ok.
