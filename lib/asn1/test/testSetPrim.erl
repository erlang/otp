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
-module(testSetPrim).

-export([main/1]).

-include_lib("common_test/include/ct.hrl").

-record('Set',{bool, boolCon, boolPri, boolApp, boolExpCon, boolExpPri, boolExpApp}).
-record('Empty',{}).

main(_Rules) ->
    roundtrip('Set',
	      #'Set'{bool=true,boolCon=true,boolPri=true,boolApp=true,
		     boolExpCon=true,boolExpPri=true,boolExpApp=true}),
    roundtrip('Set',
	      #'Set'{bool=false,boolCon=false,boolPri=false,boolApp=false,
		     boolExpCon=false,boolExpPri=false,boolExpApp=false}),
    roundtrip('Set',
	      #'Set'{bool=false,boolCon=true,boolPri=false,boolApp=true,
		     boolExpCon=false,boolExpPri=true,boolExpApp=false}),
    roundtrip('Empty', #'Empty'{}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SetPrim', T, V).
