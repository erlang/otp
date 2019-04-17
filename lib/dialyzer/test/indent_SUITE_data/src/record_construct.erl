-module(record_construct).
-export([t_loc/0, t_opa/0, t_rem/0]).

-record(r_loc, {a = gazonk :: integer(), b = 42 :: atom()}).

t_loc() ->
  #r_loc{}.

-record(r_opa, {a                 :: atom(),
		b = gb_sets:new() :: gb_sets:set(),
		c = 42            :: boolean(),
		d,	% untyped on purpose
		e = false         :: boolean()}).

t_opa() ->
  #r_opa{}.

-record(r_rem, {a = gazonk :: string()}).

t_rem() ->
  #r_rem{}.
