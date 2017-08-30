%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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

-module(diameter_transport).

%%
%% This module implements a transport start function that
%% evaluates its config argument.
%%

%% Transport start functions
-export([start/3,
         select/3,
         eval/3]).

%% start/3

%% Call a start function in this module ...
start(T, Svc, {F,A}) ->
    start(T, Svc, {?MODULE, F, [A]});

%% ... or some other.
start(T, Svc, F) ->
    diameter_lib:eval([F, T, Svc]).

%% select/3
%%
%% A start function that whose config argument is expected to return a
%% new start function.

select(T, Svc, F) ->
    start(T, Svc, diameter_lib:eval([F, T, Svc])).

%% eval/3
%%
%% A start function that simply evaluates its config argument.

eval(_, _, F) ->
    diameter_lib:eval(F).
