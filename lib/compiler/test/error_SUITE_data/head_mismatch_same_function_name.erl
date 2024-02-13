%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2024. All Rights Reserved.
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
-module(head_mismatch_same_function_name).

-export([foo/1, foo/2]).

foo({A, B}) ->
    A + B;
foo(Other, State) ->
    {Other, State}.
