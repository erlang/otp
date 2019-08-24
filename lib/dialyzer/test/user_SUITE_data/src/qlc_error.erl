%% -*- erlang-indent-level: 2 -*-
%% $Id: qlc_error.erl,v 1.1 2008/12/17 09:53:52 mikpe Exp $

%% @author Daniel Luna <luna@update.uu.se>
%% @copyright 2006 Daniel Luna
%%
%% @doc
%%

-module(qlc_error).
-export([fix/0]).
-include_lib("stdlib/include/qlc.hrl").

fix() ->
  qlc:eval(qlc:q([I || I <- []])).
