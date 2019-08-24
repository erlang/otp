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
-module(testSeqTypeRefPrim).
-export([main/1]).

-include_lib("common_test/include/ct.hrl").

-record('SeqTR',{octStr, octStrI, octStrE, 'octStr-I', 'octStrI-I', 'octStrE-I', 'octStr-E', 'octStrI-E', 'octStrE-E'}).

main(_Rules) ->
    roundtrip('SeqTR',
	      #'SeqTR'{'octStr' = <<"A string 1">>,
		       'octStrI' = <<"A string 2">>,
		       'octStrE' = <<"A string 3">>,
		       'octStr-I' = <<"A string 4">>,
		       'octStrI-I' = <<"A string 5">>,
		       'octStrE-I' = <<"A string 6">>,
		       'octStr-E' = <<"A string 7">>,
		       'octStrI-E' = <<"A string 8">>,
		       'octStrE-E' = <<"A string 9">>}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SeqTypeRefPrim', T, V).
