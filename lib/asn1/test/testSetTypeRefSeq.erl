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
-module(testSetTypeRefSeq).

-export([main/1]).

-include_lib("common_test/include/ct.hrl").

-record('SetTRseq',{setSeq, setSeqI, setSeqE, 'setSeq-I', 'setSeqI-I', 'setSeqE-I', 'setSeq-E', 'setSeqI-E', 'setSeqE-E'}).
-record('SetSeq',{seqInt, seqOs}).
-record('SetSeqImp',{seqInt, seqOs}).
-record('SetSeqExp',{seqInt, seqOs}).

main(_Rules) ->
    roundtrip('SetTRseq',
		      #'SetTRseq'{'setSeq' = #'SetSeq'{seqOs = <<"A1">>,
						       seqInt = 2},
				  'setSeqI' = #'SetSeq'{seqOs = <<"A2">>,
							seqInt = 2},
				  'setSeqE' = #'SetSeq'{seqOs = <<"A3">>,
							seqInt = 2},
				  'setSeq-I' = #'SetSeqImp'{seqOs = <<"A4">>,
							    seqInt = 2},
				  'setSeqI-I' = #'SetSeqImp'{seqOs = <<"A5">>,
							     seqInt = 2},
				  'setSeqE-I' = #'SetSeqImp'{seqOs = <<"A6">>,
							     seqInt = 2},
				  'setSeq-E' = #'SetSeqExp'{seqOs = <<"A7">>,
							    seqInt = 2},
				  'setSeqI-E' = #'SetSeqExp'{seqOs = <<"A8">>,
							     seqInt = 2},
				  'setSeqE-E' = #'SetSeqExp'{seqOs = <<"A9">>,
							     seqInt = 2}}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SetTypeRefSeq', T, V).
