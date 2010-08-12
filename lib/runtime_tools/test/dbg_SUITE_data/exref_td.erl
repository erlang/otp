%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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
