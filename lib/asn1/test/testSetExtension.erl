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
-module(testSetExtension).
-include("External.hrl").
-export([main/1]).

-include_lib("common_test/include/ct.hrl").

-record('SetExt1',{}).
-record('SetExt2',{bool, int}).
-record('SetExt3',{bool, int}).
-record('SetExt4',{bool, int}).

main(_Rules) ->
    roundtrip('SetExt1', #'SetExt1'{}),
    roundtrip('SetExt2', #'SetExt2'{bool=true,int=99}),
    roundtrip('SetExt2', #'SetExt2'{bool=true,int=99}),
    roundtrip('SetExt3', #'SetExt3'{bool=true,int=99}),
    roundtrip('SetExt3', #'SetExt3'{bool=true,int=99}),
    roundtrip('SetExt4', #'SetExt4'{bool=true,int=99}),
    roundtrip('SetExt4', #'SetExt4'{bool=true,int=99}),
    roundtrip('SetExt1', #'SetExt1'{}),
    roundtrip('SetExt2', #'SetExt2'{bool=true,int=99}),
    roundtrip('SetExt2', #'SetExt2'{bool=true,int=99}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SetExtension', T, V).


