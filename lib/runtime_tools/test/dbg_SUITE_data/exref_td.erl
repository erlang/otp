%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
-module(exref_td).

-export([a/1,b/1,c/1,d/1,e/1,my_analyse/1]).

a("edcba") ->
    true;
a(_) ->
    lists:reverse("abcde").

b(_) ->
    a(nil).

c(_) ->
    nonexistentmodule:f().

d(_) ->
    lists:nonexistentfunction().

e(_) ->
    [a(x),b(x),c(x)].

localfuncnotcalled() ->
    true.

localfunccalledbyf(A) ->
    A.

f() ->
    lists:filter(fun localfunccalledbyf/1,"aaabba"),
    F = fun localfuncnotcalled/0.

my_analyse(Graph) -> ok.
