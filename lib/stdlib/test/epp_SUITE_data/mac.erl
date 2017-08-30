%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
-module(mac).

-compile(export_all).

-define(A, ?A + ?A).

-define(a, ?a + ?a).
-define(b, x + ?c(2)).
-define(c(Y), x + Y).
-define(d(X), X X).

bar() ->
    1 ?d(?d(?d(?d(?d(?d(?d(+1))))))).

foo1() ->
    ?a.

foo2() ->
    ?b.

foo3() ->
    ?A.

-define( this, ?that).
-define( that, ?this).

talkAbout()->
    ?this==?that.
