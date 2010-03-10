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
-module(testChoTypeRefSeq).

-export([compile/3]).
-export([seq/1]).

-include_lib("test_server/include/test_server.hrl").

-record('ChoSeq',{seqInt, seqOs}).
-record('ChoSeqImp',{seqInt, seqOs}).
-record('ChoSeqExp',{seqInt, seqOs}).


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "ChoTypeRefSeq",[Rules,{outdir,OutDir}]++Options).



seq(_Rules) ->
    
    ?line {ok,Bytes1} = 
	asn1_wrapper:encode('ChoTypeRefSeq','ChoTRseq',
		      {choSeq,#'ChoSeq'{seqInt = 88,
					seqOs = "A string"}}),
    ?line {ok,{choSeq,{'ChoSeq',88,"A string"}}} = 
	asn1_wrapper:decode('ChoTypeRefSeq','ChoTRseq',lists:flatten(Bytes1)),
    
    
    ?line {ok,Bytes2} = 
	asn1_wrapper:encode('ChoTypeRefSeq','ChoTRseq',
		      {choSeqI,#'ChoSeq'{seqInt = 88,
					 seqOs = "A string"}}),
    ?line {ok,{choSeqI,{'ChoSeq',88,"A string"}}} = 
	asn1_wrapper:decode('ChoTypeRefSeq','ChoTRseq',lists:flatten(Bytes2)),
    
    
    ?line {ok,Bytes3} = 
	asn1_wrapper:encode('ChoTypeRefSeq','ChoTRseq',
		      {choSeqE,#'ChoSeq'{seqInt = 88,
					 seqOs = "A string"}}),
    ?line {ok,{choSeqE,{'ChoSeq',88,"A string"}}} = 
	asn1_wrapper:decode('ChoTypeRefSeq','ChoTRseq',lists:flatten(Bytes3)),
    
    
    ?line {ok,Bytes4} = 
	asn1_wrapper:encode('ChoTypeRefSeq','ChoTRseq',
		      {'choSeq-I',#'ChoSeqImp'{seqInt = 88,
					       seqOs = "A string"}}),
    ?line {ok,{'choSeq-I',{'ChoSeqImp',88,"A string"}}} = 
	asn1_wrapper:decode('ChoTypeRefSeq','ChoTRseq',lists:flatten(Bytes4)),
    
    
    ?line {ok,Bytes5} = 
	asn1_wrapper:encode('ChoTypeRefSeq','ChoTRseq',
		      {'choSeqI-I',#'ChoSeqImp'{seqInt = 88,
						seqOs = "A string"}}),
    ?line {ok,{'choSeqI-I',{'ChoSeqImp',88,"A string"}}} = 
	asn1_wrapper:decode('ChoTypeRefSeq','ChoTRseq',lists:flatten(Bytes5)),
    
    
    ?line {ok,Bytes6} = 
	asn1_wrapper:encode('ChoTypeRefSeq','ChoTRseq',
		      {'choSeqE-I',#'ChoSeqImp'{seqInt = 88,
						seqOs = "A string"}}),
    ?line {ok,{'choSeqE-I',{'ChoSeqImp',88,"A string"}}} = 
	asn1_wrapper:decode('ChoTypeRefSeq','ChoTRseq',lists:flatten(Bytes6)),
    
    
    ?line {ok,Bytes7} = 
	asn1_wrapper:encode('ChoTypeRefSeq','ChoTRseq',
		      {'choSeq-E',#'ChoSeqExp'{seqInt = 88,
					       seqOs = "A string"}}),
    ?line {ok,{'choSeq-E',{'ChoSeqExp',88,"A string"}}} = 
	asn1_wrapper:decode('ChoTypeRefSeq','ChoTRseq',lists:flatten(Bytes7)),
    
    
    ?line {ok,Bytes8} = 
	asn1_wrapper:encode('ChoTypeRefSeq','ChoTRseq',
		      {'choSeqI-E',#'ChoSeqExp'{seqInt = 88,
						seqOs = "A string"}}),
    ?line {ok,{'choSeqI-E',{'ChoSeqExp',88,"A string"}}} = 
	asn1_wrapper:decode('ChoTypeRefSeq','ChoTRseq',lists:flatten(Bytes8)),
    
    
    ?line {ok,Bytes9} = 
	asn1_wrapper:encode('ChoTypeRefSeq','ChoTRseq',
		      {'choSeqE-E',#'ChoSeqExp'{seqInt = 88,
						seqOs = "A string"}}),
    ?line {ok,{'choSeqE-E',{'ChoSeqExp',88,"A string"}}} = 
	asn1_wrapper:decode('ChoTypeRefSeq','ChoTRseq',lists:flatten(Bytes9)),
    
    
    ok.
