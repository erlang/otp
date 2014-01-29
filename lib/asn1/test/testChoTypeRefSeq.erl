%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2012. All Rights Reserved.
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

-export([seq/1]).

-include_lib("test_server/include/test_server.hrl").

-record('ChoSeq', {seqInt, seqOs}).
-record('ChoSeqImp', {seqInt, seqOs}).
-record('ChoSeqExp', {seqInt, seqOs}).

seq(_Rules) ->
    roundtrip('ChoTRseq', {choSeq,#'ChoSeq'{seqInt=88,
					    seqOs = <<"A string">>}}),
    roundtrip('ChoTRseq', {choSeqI,#'ChoSeq'{seqInt=88,
					     seqOs = <<"A string">>}}),
    roundtrip('ChoTRseq', {choSeqE,#'ChoSeq'{seqInt=88,
					     seqOs = <<"A string">>}}),
    roundtrip('ChoTRseq', {'choSeq-I',#'ChoSeqImp'{seqInt=88,
						   seqOs = <<"A string">>}}),
    roundtrip('ChoTRseq', {'choSeqI-I',#'ChoSeqImp'{seqInt=88,
						    seqOs = <<"A string">>}}),
    roundtrip('ChoTRseq', {'choSeqE-I',#'ChoSeqImp'{seqInt=88,
						    seqOs = <<"A string">>}}),
    roundtrip('ChoTRseq', {'choSeq-E',#'ChoSeqExp'{seqInt=88,
						   seqOs = <<"A string">>}}),
    roundtrip('ChoTRseq', {'choSeqI-E',#'ChoSeqExp'{seqInt=88,
						    seqOs = <<"A string">>}}),
    roundtrip('ChoTRseq', {'choSeqE-E',#'ChoSeqExp'{seqInt=88,
						    seqOs = <<"A string">>}}),
    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip('ChoTypeRefSeq', Type, Value).
