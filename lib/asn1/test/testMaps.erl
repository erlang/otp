%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
-module(testMaps).

-export([main/1]).

main(_) ->
    M = 'Maps',
    true = M:maps(),

    true = M:xy1() =:= #{x=>42,y=>17},
    true = M:xy2() =:= #{x=>0,y=>0},
    true = M:xy3() =:= #{x=>0,y=>999},
    true = M:s1() =:= #{xy=>#{x=>100,y=>100}},

    roundtrip('XY', M:xy1()),
    roundtrip('XY', M:xy2()),
    roundtrip('XY', M:xy3()),
    roundtrip('XY', #{}, #{x=>0,y=>0}),

    roundtrip('S', M:s1()),
    roundtrip('S', #{}, #{xy=>#{x=>100,y=>100}}),
    roundtrip('S', #{os=><<1,2,3>>}, #{xy=>#{x=>100,y=>100},
                                       os=><<1,2,3>>}),

    ok.

roundtrip(Type, Value) ->
    roundtrip(Type, Value, Value).

roundtrip(Type, Value, Expected) ->
    asn1_test_lib:roundtrip('Maps', Type, Value, Expected).
