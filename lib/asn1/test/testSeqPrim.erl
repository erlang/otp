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
-module(testSeqPrim).

-export([main/1]).

-include_lib("common_test/include/ct.hrl").

-record('Seq',{bool, boolCon, boolPri, boolApp, boolExpCon, boolExpPri, boolExpApp}).
-record('Empty',{}).
-record('Big', {os1,os2,os3}).

main(_Rules) ->
    roundtrip('Seq', #'Seq'{bool=true,boolCon=true,boolPri=true,boolApp=true,
			    boolExpCon=true,boolExpPri=true,boolExpApp=true}),
    roundtrip('Seq', #'Seq'{bool=false,boolCon=false,boolPri=false,
			    boolApp=false,boolExpCon=false,
			    boolExpPri=false,boolExpApp=false}),
    roundtrip('Seq', #'Seq'{bool=false,boolCon=true,boolPri=false,boolApp=true,
			    boolExpCon=false,boolExpPri=true,boolExpApp=false}),
    roundtrip('Empty', #'Empty'{}),
    roundtrip('Big', #'Big'{os1=list_to_binary(lists:duplicate(120, 16#A5)),
			    os2=list_to_binary(lists:duplicate(128, 16#A7)),
			    os3=list_to_binary(lists:duplicate(17777, 16#F5))}),
    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip('SeqPrim', Type, Value).
