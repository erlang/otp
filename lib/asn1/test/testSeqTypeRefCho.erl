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
-module(testSeqTypeRefCho).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").
-include("External.hrl").

-record('SeqTRcho',{seqCho, seqChoE, 'seqCho-E', 'seqChoE-E'}).


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SeqTypeRefCho",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    

    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SeqTypeRefCho','SeqTRcho',
		      #'SeqTRcho'{'seqCho' = {choOs,"A string 1"},
				  'seqChoE' = {choOs,"A string 3"},
				  'seqCho-E' = {choOs,"A string 7"},
				  'seqChoE-E' = {choOs,"A string 9"}}),
    ?line {ok,{'SeqTRcho',{choOs,"A string 1"},{choOs,"A string 3"},{choOs,"A string 7"},{choOs,"A string 9"}}} = 
	asn1_wrapper:decode('SeqTypeRefCho','SeqTRcho',lists:flatten(Bytes11)),



    ok.
