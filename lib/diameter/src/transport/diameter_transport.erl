%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
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
