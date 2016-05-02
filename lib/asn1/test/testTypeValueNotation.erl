%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
-module(testTypeValueNotation).

-export([main/2]).

-record('Seq', {octstr, int, bool, enum, bitstr, null, oid, vstr}).

main(_Rule, _Option) ->
    Value = #'Seq'{octstr = <<1,2,3,4>>,
		   int = 12,
		   bool = true,
		   enum = a,
		   bitstr = <<2#1010:4>>,
		   null = 'NULL',
		   oid = {1, 2, 55},
		   vstr = "Hello World"},
    roundtrip('Seq', Value).

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SeqTypeRefPrim', T, V).
