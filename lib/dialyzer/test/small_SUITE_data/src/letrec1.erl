%%%-------------------------------------------------------------------
%%% File    : letrec1.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description :
%%%
%%% Created :  9 Mar 2007 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(letrec1).

-export([t/1]).

t(Opts) ->
  [Opt || Opt <- Opts, Opt =/= compressed].
