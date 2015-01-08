%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015. All Rights Reserved.
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

-module(dialyzer_test).
-export([?MODULE/0,turtle/0,test/1,huge/1]).

-record(turtle, {a,b,c}).
-record(tortoise, {a,b,c}).

?MODULE() ->
    [{a,b,c}].

turtle() ->
    #turtle{a=1,b=2,c=3}.

test(T) ->
    {T#tortoise.a,T#tortoise.b}.

-file("dialyzer_test", 100000000).

huge(X) ->
    #turtle{a=42,b=100,c=511},
    X#tortoise.a.
