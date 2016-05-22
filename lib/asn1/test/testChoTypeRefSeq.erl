%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%%
-module(testChoTypeRefSeq).

-export([seq/1]).

-include_lib("common_test/include/ct.hrl").

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
