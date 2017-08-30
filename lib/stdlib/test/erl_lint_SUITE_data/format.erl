%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
-module(format).

-export([f/1]).

%%% There will be warnings at level 2 and 3.

f(F) ->
    io:format("~", F),				%2
    io:format("~", [F]),			%1
    io:format(a, b),				%1
    io:format(a, "abc"),			%1
    io:format(a, [a | "abc"]),			%2
    io:format(4,5,6,7),				%1

    io:format("la cucaracha~n"),
    io:format(""),
    io:format("~p ~p~n", [F]),			%1
    io:format("~p~n", [F]),
    io:format("~m"),				%1
    io:format(F, "~p", []),			%1
    io:format("~x~n", [F]),			%1
    io:format("~p~n", F),			%2
    io:format(F, [3]),				%2

    io:format("~p", a),				%1
    io:format("~p~", [F]),			%1
    io:format("~p ~p", [F, 4 | 7]),		%1
    io:format("~14p", [F]),
    io:format("~*p", [a, F]),			%no type checking
    io:format("~*p", [14, F]),

    io:fwrite("~p", []),			%1
    io_lib:format("~p", []),			%1
    foo:format("~p", []),
    io:format(),				%1

    ok.
