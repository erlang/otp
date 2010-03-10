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
-module(testSeqTypeRefSeq).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('Seq1',{bool1, int1, seq1}).
-record('Seq2',{seq2, bool2, int2}).
-record('Seq3',{bool3, seq3, int3}).
-record('Seq4',{seq41, seq42, seq43}).
-record('SeqIn',{boolIn, intIn}).
-record('SeqS1',{boolS1, intS1, seqS1}).
-record('SeqS1_seqS1',{boolIn, intIn}).
-record('SeqS2',{seqS2, boolS2, intS2}).
-record('SeqS2_seqS2',{boolIn, intIn}).
-record('SeqS3',{boolS3, seqS3, intS3}).
-record('SeqS3_seqS3',{boolIn, intIn}).
-record('SeqSTag',{seqS1, seqS2, seqS3}).
-record('SeqSTag_seqS1',{b1, i1}).
-record('SeqSTag_seqS2',{b2, i2}).
-record('SeqSTag_seqS3',{b3, i3}).
-record('SeqTRseq',{seqSeq, seqSeqI, seqSeqE, 'seqSeq-I', 'seqSeqI-I', 'seqSeqE-I', 'seqSeq-E', 'seqSeqI-E', 'seqSeqE-E'}).
-record('SeqSeq',{seqInt, seqOs}).
-record('SeqSeqImp',{seqInt, seqOs}).
-record('SeqSeqExp',{seqInt, seqOs}).



compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SeqTypeRefSeq",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    
    
    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SeqTypeRefSeq','Seq1',#'Seq1'{bool1 = true,
						     int1 = 15,
						     seq1 = #'SeqIn'{boolIn = true,
								     intIn = 66}}), 
    ?line {ok,{'Seq1',true,15,{'SeqIn',true,66}}} = 
	asn1_wrapper:decode('SeqTypeRefSeq','Seq1',lists:flatten(Bytes11)),
    
    
    
    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('SeqTypeRefSeq','Seq2',#'Seq2'{seq2 = #'SeqIn'{boolIn = true,
								     intIn = 66},
						     bool2 = true,
						     int2 = 15}),    
    ?line {ok,{'Seq2',{'SeqIn',true,66},true,15}} = 
	asn1_wrapper:decode('SeqTypeRefSeq','Seq2',lists:flatten(Bytes12)),
    
    
    ?line {ok,Bytes13} = 
	asn1_wrapper:encode('SeqTypeRefSeq','Seq3',#'Seq3'{bool3 = true,
						     seq3 = #'SeqIn'{boolIn = true,
								     intIn = 66},
						     int3 = 15}),    
    ?line {ok,{'Seq3',true,{'SeqIn',true,66},15}} = 
	asn1_wrapper:decode('SeqTypeRefSeq','Seq3',lists:flatten(Bytes13)),
    
    
    
    ?line {ok,Bytes14} = 
	asn1_wrapper:encode('SeqTypeRefSeq','Seq4',#'Seq4'{seq41 = #'SeqIn'{boolIn = true,
								      intIn = 66},
						     seq42 = #'SeqIn'{boolIn = true,
								      intIn = 66},
						     seq43 = #'SeqIn'{boolIn = true,
								      intIn = 66}}),    
    ?line {ok,{'Seq4',{'SeqIn',true,66},{'SeqIn',true,66},{'SeqIn',true,66}}} = 
	asn1_wrapper:decode('SeqTypeRefSeq','Seq4',lists:flatten(Bytes14)),
    
    
    
    
    
    
    
    
    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SeqTypeRefSeq','SeqS1',#'SeqS1'{boolS1 = true,
						       intS1 = 15,
						       seqS1 = #'SeqS1_seqS1'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SeqS1',true,15,{'SeqS1_seqS1',true,66}}} = 
	asn1_wrapper:decode('SeqTypeRefSeq','SeqS1',lists:flatten(Bytes21)),
    
    
    ?line {ok,Bytes22} = 
	asn1_wrapper:encode('SeqTypeRefSeq','SeqS2',#'SeqS2'{seqS2 = #'SeqS2_seqS2'{boolIn = true,
									      intIn = 66},
						       boolS2 = true,
						       intS2 = 15}),    
    ?line {ok,{'SeqS2',{'SeqS2_seqS2',true,66},true,15}} = 
	asn1_wrapper:decode('SeqTypeRefSeq','SeqS2',lists:flatten(Bytes22)),
    
    
    
    ?line {ok,Bytes23} = 
	asn1_wrapper:encode('SeqTypeRefSeq','SeqS3',#'SeqS3'{boolS3 = true,
						       seqS3 = #'SeqS3_seqS3'{boolIn = true,
									      intIn = 66},
						       intS3 = 15}),    
    ?line {ok,{'SeqS3',true,{'SeqS3_seqS3',true,66},15}} = 
	asn1_wrapper:decode('SeqTypeRefSeq','SeqS3',lists:flatten(Bytes23)),
    
    
    
    
    
    
    ?line {ok,Bytes31} = 
	asn1_wrapper:encode('SeqTypeRefSeq','SeqSTag',#'SeqSTag'{seqS1 = #'SeqSTag_seqS1'{b1 = true,
										    i1 = 11},
							   seqS2 = #'SeqSTag_seqS2'{b2 = true,
										    i2 = 22},
							   seqS3 = #'SeqSTag_seqS3'{b3 = true,
										    i3 = 33}}),    
    ?line {ok,{'SeqSTag',{'SeqSTag_seqS1',true,11},
	       {'SeqSTag_seqS2',true,22},
	       {'SeqSTag_seqS3',true,33}}} = 
	asn1_wrapper:decode('SeqTypeRefSeq','SeqSTag',lists:flatten(Bytes31)),
    
    
    
    
    
    ?line {ok,Bytes41} = 
	asn1_wrapper:encode('SeqTypeRefSeq','SeqTRseq',
		      #'SeqTRseq'{'seqSeq' = #'SeqSeq'{seqOs = "A1",
						       seqInt = 2},
				  'seqSeqI' = #'SeqSeq'{seqOs = "A2",
							seqInt = 2},
				  'seqSeqE' = #'SeqSeq'{seqOs = "A3",
							seqInt = 2},
				  'seqSeq-I' = #'SeqSeqImp'{seqOs = "A4",
							    seqInt = 2},
				  'seqSeqI-I' = #'SeqSeqImp'{seqOs = "A5",
							     seqInt = 2},
				  'seqSeqE-I' = #'SeqSeqImp'{seqOs = "A6",
							     seqInt = 2},
				  'seqSeq-E' = #'SeqSeqExp'{seqOs = "A7",
							    seqInt = 2},
				  'seqSeqI-E' = #'SeqSeqExp'{seqOs = "A8",
							     seqInt = 2},
				  'seqSeqE-E' = #'SeqSeqExp'{seqOs = "A9",
							     seqInt = 2}}),
    ?line {ok,{'SeqTRseq',{'SeqSeq',2,"A1"},
      {'SeqSeq',2,"A2"},
      {'SeqSeq',2,"A3"},
      {'SeqSeqImp',2,"A4"},
      {'SeqSeqImp',2,"A5"},
      {'SeqSeqImp',2,"A6"},
      {'SeqSeqExp',2,"A7"},
      {'SeqSeqExp',2,"A8"},
      {'SeqSeqExp',2,"A9"}}} = 
	asn1_wrapper:decode('SeqTypeRefSeq','SeqTRseq',lists:flatten(Bytes41)),
    
    ok.
