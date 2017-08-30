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
%%
%%----------------------------------------------------------------------
%% File    : client.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module(client).

-export([produce/0,init/3,call/0]).

-define(SERVER,{rmod_random_impl,
		list_to_atom("babbis@"++hd(tl(string:tokens(atom_to_list(node()),"@"))))}).
-define(CLIENTMOD,'rmod_random').

produce() ->
    ?CLIENTMOD:produce(?SERVER).


init(Seed1, Seed2, Seed3) ->
    io:format("Init..."),
    ?CLIENTMOD:init(?SERVER,Seed1, Seed2, Seed3),
    io:format("ok\n").


call() ->
    init(1,2,3),
    produce(0).


produce(10) ->
    ok;
produce(Ctr) ->
    N = produce(),
    io:format("Random~p = ~p\n",[Ctr,N]),
    produce(Ctr+1).
