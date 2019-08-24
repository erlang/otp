%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
-module(testDoubleEllipses).

-export([main/1]).

-include_lib("common_test/include/ct.hrl").

-record('Seq',{a, c}).
-record('SeqV1',{a, b}).
-record('SeqV2',{a, b ,c}).
-record('SeqAlt',{a,d,b,e,c,f,g}).
-record('SeqAltV2',{a,d,b,e,h,i,c,f,g}).

-record('Set',{a, c}).
-record('SetV1',{a, b}).
-record('SetV2',{a, b ,c}).
-record('SetAlt',{a,d,b,e,c,f,g}).
-record('SetAltV2',{a,d,b,e,h,i,c,f,g}).

main(_Rules) ->
    roundtrip('Seq', #'Seq'{a=10,c=true}),
    roundtrip('SeqV1', #'SeqV1'{a=10,b=false}),
    roundtrip('SeqV2', #'SeqV2'{a=10,b=false,c=true}),
    roundtrip('SeqAlt',
	      #'SeqAlt'{a=10,d=12,b = <<2#1010:4>>,
			e=true,c=false,f=14,g=16}),
    roundtrip('SeqAltV2',
	      #'SeqAltV2'{a=10,d=12,
			  b = <<2#1010:4>>,
			  e=true,h="PS",i=13,c=false,f=14,g=16}),
    
    roundtrip('Set', #'Set'{a=10,c=true}),
    roundtrip('SetV1', #'SetV1'{a=10,b=false}),
    roundtrip('SetV2', #'SetV2'{a=10,b=false,c=true}),
    roundtrip('SetAlt',
	      #'SetAlt'{a=10,d=12,
			b = <<2#1010:4>>,
			e=true,c=false,f=14,g=16}),
    roundtrip('SetAltV2',
	      #'SetAltV2'{a=10,d=12,
			  b = <<2#1010:4>>,
			  e=true,h="PS",i=13,c=false,f=14,g=16}),

    roundtrip('SeqDoubleEmpty1',
	      {'SeqDoubleEmpty1'}),
    roundtrip('SeqDoubleEmpty2',
	      {'SeqDoubleEmpty2',true,42}),
    roundtrip('SeqDoubleEmpty2',
	      {'SeqDoubleEmpty2',true,asn1_NOVALUE}),

    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('DoubleEllipses', T, V).
