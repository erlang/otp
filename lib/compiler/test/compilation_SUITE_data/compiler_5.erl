%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(compiler_5).           
-export([compiler_5/0]).

compiler_5() ->
    f0(),
    f1(),
    f2(),
    ok.

%% compiler treats records with 1 and 2 fields differently... 
-record(nil, {}).
-record(foo, {hello=1}).
-record(bar, {hello=2,there=3}).

f0() ->
    R1 = #nil{},
    R2 = R1#nil{}, %% stupid code, but compiler shouldn't crash
    R1 = R2,
    ok.

f1() ->
    R1 = #foo{},
    R2 = R1#foo{}, %% stupid code, but compiler shouldn't crash
    R1 = R2,
    ok.

f2() ->
    R1 = #bar{},
    R2 = R1#bar{}, %% stupid code, but compiler shouldn't crash
    R1 = R2,
    ok.
