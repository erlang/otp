%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
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
%%%-------------------------------------------------------------------
%%% File    : sudoku.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : Sudoku
%%%
%%% Created : 13 Mar 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------

-module(sudoku).

-export([go/0]).

-compile(export_all).

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
