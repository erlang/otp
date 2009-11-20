%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
%%
% Start Erlang with : erl -sname <your name> -setcookie <your cookie>

-module(test).

-export([start/0,exec/0]).


start() ->
    io:format("Starting server~n"),
    rmod_random:oe_create([],{local,'rmod_random_impl'}).

exec() ->
    io:format("Running client~n"),
    OutPut = os:cmd("client"),
    io:format("~s",[OutPut]).









