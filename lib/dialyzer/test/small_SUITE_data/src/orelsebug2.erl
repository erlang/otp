%%%-------------------------------------------------------------------
%%% File    : orelsebug2.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description :
%%%
%%% Created : 21 Nov 2006 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(orelsebug2).

-export([t/1]).

-record(eventdata, {
	  expires
	 }).

t(L) ->
  L2 = [E1 || E1 <- L, E1#eventdata.expires == x
			orelse E1#eventdata.expires == y],

  case L2 of
    [_E] -> x;
    [] ->  y
  end.
