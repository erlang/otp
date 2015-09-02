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
-module(beam_compiler_2).
-export([beam_compiler_2/0]).

beam_compiler_2() ->
    ok.

-record(foo,{a,b}).

try_me() ->
    try_me({foo,x,z},{foo,y,z}).

try_me(X,Y) ->
    f(X#foo.a =/= Y#foo.a,X#foo.b =/= X#foo.b).

f(A,B) ->
    A.

