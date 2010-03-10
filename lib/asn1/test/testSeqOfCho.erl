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
-module(testSeqOfCho).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('SeqChoDef',{bool1, int1, seq1 = asn1_DEFAULT}).
-record('SeqChoOpt',{bool1, int1, seq1 = asn1_NOVALUE}).
-record('SeqChoEmbDef',{bool1, int1, seq1 = asn1_DEFAULT}).
-record('SeqChoEmbOpt',{bool1, int1, seq1 = asn1_NOVALUE}).
-record('SeqOfChoEmbDef_SEQOF',{bool1, int1, seq1 = asn1_DEFAULT}).
-record('SeqOfChoEmbOpt_SEQOF',{bool1, int1, seq1 = asn1_NOVALUE}).



compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SeqOfCho",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    
    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SeqOfCho','SeqChoDef',#'SeqChoDef'{bool1 = true,
							  int1 = 17}),
    ?line {ok,{'SeqChoDef',true,17,[]}} = 
	asn1_wrapper:decode('SeqOfCho','SeqChoDef',lists:flatten(Bytes11)),
    
    
    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('SeqOfCho','SeqChoDef',#'SeqChoDef'{bool1 = true,
							  int1 = 17,
							  seq1 = [{boolIn,true},
								  {intIn,25}]}),
    ?line {ok,{'SeqChoDef',true,17,[{boolIn,true},{intIn,25}]}} = 
	asn1_wrapper:decode('SeqOfCho','SeqChoDef',lists:flatten(Bytes12)),
    
    
    
    ?line {ok,Bytes15} = 
	asn1_wrapper:encode('SeqOfCho','SeqChoOpt',#'SeqChoOpt'{bool1 = true,
							  int1 = 17}),
    ?line {ok,{'SeqChoOpt',true,17,asn1_NOVALUE}} = 
	asn1_wrapper:decode('SeqOfCho','SeqChoOpt',lists:flatten(Bytes15)),
    
    
    ?line {ok,Bytes16} = 
	asn1_wrapper:encode('SeqOfCho','SeqChoOpt',#'SeqChoOpt'{bool1 = true,
							  int1 = 17,
							  seq1 = [{boolIn,true},
								  {intIn,25}]}),
    ?line {ok,{'SeqChoOpt',true,17,[{boolIn,true},{intIn,25}]}} = 
	asn1_wrapper:decode('SeqOfCho','SeqChoOpt',lists:flatten(Bytes16)),
    
    
    
    
    
    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SeqOfCho','SeqChoEmbDef',#'SeqChoEmbDef'{bool1 = true,
								int1 = 17}),
    ?line {ok,{'SeqChoEmbDef',true,17,[]}} = 
	asn1_wrapper:decode('SeqOfCho','SeqChoEmbDef',lists:flatten(Bytes21)),
    
    
    ?line {ok,Bytes22} = 
	asn1_wrapper:encode('SeqOfCho','SeqChoEmbDef',#'SeqChoEmbDef'{bool1 = true,
								int1 = 17,
								seq1 = [{boolIn,true},
									{intIn,25}]}),
    ?line {ok,{'SeqChoEmbDef',true,17,[{boolIn,true},{intIn,25}]}} = 
	asn1_wrapper:decode('SeqOfCho','SeqChoEmbDef',lists:flatten(Bytes22)),
    
    
    
    ?line {ok,Bytes25} = 
	asn1_wrapper:encode('SeqOfCho','SeqChoEmbOpt',#'SeqChoEmbOpt'{bool1 = true,
								int1 = 17}),
    ?line {ok,{'SeqChoEmbOpt',true,17,asn1_NOVALUE}} = 
	asn1_wrapper:decode('SeqOfCho','SeqChoEmbOpt',lists:flatten(Bytes25)),
    
    
    ?line {ok,Bytes26} = 
	asn1_wrapper:encode('SeqOfCho','SeqChoEmbOpt',#'SeqChoEmbOpt'{bool1 = true,
								int1 = 17,
								seq1 = [{boolIn,true},
									{intIn,25}]}),
    ?line {ok,{'SeqChoEmbOpt',true,17,[{boolIn,true},{intIn,25}]}} = 
	asn1_wrapper:decode('SeqOfCho','SeqChoEmbOpt',lists:flatten(Bytes26)),
    
    
    
    
    
    
    ?line {ok,Bytes31} = 
	asn1_wrapper:encode('SeqOfCho','SeqOfChoEmbDef',[#'SeqOfChoEmbDef_SEQOF'{bool1 = true,
									   int1 = 17}]),
    ?line {ok,[{'SeqOfChoEmbDef_SEQOF',true,17,[]}]} = 
	asn1_wrapper:decode('SeqOfCho','SeqOfChoEmbDef',lists:flatten(Bytes31)),
    
    
    ?line {ok,Bytes32} = 
	asn1_wrapper:encode('SeqOfCho','SeqOfChoEmbDef',
		      [#'SeqOfChoEmbDef_SEQOF'{bool1 = true,
					       int1 = 17,
					       seq1 = [{boolIn,true},
						       {intIn,25}]}]),
    ?line {ok,[{'SeqOfChoEmbDef_SEQOF',true,17,[{boolIn,true},{intIn,25}]}]} = 
	asn1_wrapper:decode('SeqOfCho','SeqOfChoEmbDef',lists:flatten(Bytes32)),
    
    
    
    ?line {ok,Bytes35} = 
	asn1_wrapper:encode('SeqOfCho','SeqOfChoEmbOpt',[#'SeqOfChoEmbOpt_SEQOF'{bool1 = true,
									   int1 = 17}]),
    ?line {ok,[{'SeqOfChoEmbOpt_SEQOF',true,17,asn1_NOVALUE}]} = 
	asn1_wrapper:decode('SeqOfCho','SeqOfChoEmbOpt',lists:flatten(Bytes35)),
    
    
    ?line {ok,Bytes36} = 
	asn1_wrapper:encode('SeqOfCho','SeqOfChoEmbOpt',
		      [#'SeqOfChoEmbOpt_SEQOF'{bool1 = true,
					       int1 = 17,
					       seq1 = [{boolIn,true},
						       {intIn,25}]}]),
    ?line {ok,[{'SeqOfChoEmbOpt_SEQOF',true,17,[{boolIn,true},{intIn,25}]}]} = 
	asn1_wrapper:decode('SeqOfCho','SeqOfChoEmbOpt',lists:flatten(Bytes36)),
    
    
    
    
    ok.


