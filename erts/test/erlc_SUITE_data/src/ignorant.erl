%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021-2022. All Rights Reserved.
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

%% This module is ignorant about any features and thus use 'ifn',
%% 'while' and 'until' as ordinary atoms.

-module(ignorant).

-export([foo/0,
         frob/1,
         bar/0,
         until/1,
         baz/1]).

until(X) ->
    until(X).

foo() ->
    [ifn, while, until].

frob(while) -> false.

bar() ->
    [until, while].

baz(ifn) ->
    {true, 'ifnot'}.
