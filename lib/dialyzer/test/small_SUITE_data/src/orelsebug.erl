%%%-------------------------------------------------------------------
%%% File    : orelsebug.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description :
%%%
%%% Created : 14 Nov 2006 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(orelsebug).

-export([t/1, t1/1]).

t(Format) when is_list(Format) ->
   t1(Format).

t1(Format) when is_list(Format) orelse is_binary(Format) ->
   Format.
