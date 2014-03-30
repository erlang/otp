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
-module(expand_test).

-export([a_fun_name/1,
	 a_less_fun_name/1,
	 b_comes_after_a/1,
         expand0arity_entirely/0]).

a_fun_name(X) ->
    X.

a_less_fun_name(X) ->
    X.

b_comes_after_a(X) ->
    X.

expand0arity_entirely () ->
    ok.
