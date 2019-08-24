%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2018. All Rights Reserved.
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
%%%-------------------------------------------------------------------
%%% File    : sudoku.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : Sudoku
%%%
%%% Created : 13 Mar 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------

-module(sudoku).

-export([go/0, start/0]).


-include("sudoku.hrl").

start() -> 
    spawn_link(fun() -> init(halt) end).
go() -> 
    spawn_link(fun() -> init(keep) end).

init(Halt) ->
    ?TC(sudoku_gui:new(self())),
    receive {gfx, GFX} -> ok end,
    case sudoku_game:init(GFX) of
	Halt -> erlang:halt();
	Stop -> exit(Stop)
    end.

tc(Fun,Mod,Line) ->
    case timer:tc(erlang, apply, [Fun,[]]) of
        {_,{'EXIT',Reason}} -> exit(Reason);
        {T,R} ->
            io:format("~p:~p: Time: ~p\n", [Mod, Line, T]),
            R
    end.
