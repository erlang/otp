%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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

