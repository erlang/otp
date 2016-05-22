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
-module(testSeqTypeRefSet).

-export([main/1]).

-include_lib("common_test/include/ct.hrl").

-record('SeqTRset',{seqSet, seqSetI, seqSetE, 'seqSet-I', 'seqSetI-I', 'seqSetE-I', 'seqSet-E', 'seqSetI-E', 'seqSetE-E'}).
-record('SeqSet',{setInt, setOs}).
-record('SeqSetImp',{setInt, setOs}).
-record('SeqSetExp',{setInt, setOs}).



main(_Rules) ->
    roundtrip('SeqTRset',
	      #'SeqTRset'{seqSet=#'SeqSet'{setInt=2,setOs = <<"A1">>},
			  seqSetI=#'SeqSet'{setInt=2,setOs = <<"A2">>},
			  seqSetE=#'SeqSet'{setInt=2,setOs = <<"A3">>},
			  'seqSet-I'=#'SeqSetImp'{setInt=2,setOs = <<"A4">>},
			  'seqSetI-I'=#'SeqSetImp'{setInt=2,setOs = <<"A5">>},
			  'seqSetE-I'=#'SeqSetImp'{setInt=2,setOs = <<"A6">>},
			  'seqSet-E'=#'SeqSetExp'{setInt=2,setOs = <<"A7">>},
			  'seqSetI-E'=#'SeqSetExp'{setInt=2,setOs = <<"A8">>},
			  'seqSetE-E'=#'SeqSetExp'{setInt=2,setOs = <<"A9">>}}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SeqTypeRefSet', T, V).
