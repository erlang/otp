%% With the patch introduced to avoid false warnings in
%% user_SUITE_data/src/wpc_hlines.erl we can unfortunately no longer precisely
%% catch problems like this one... The refinement procedure is still enough to
%% keep some of the details, nevertheless.

-module(higher_order_discrepancy).

-export([test/1]).

test(X) ->
    F =
	case X of
	    1 -> fun f/1;
	    2 -> fun g/1
	end,
    F(foo).

f(foo) -> ok.
g(bar) -> ok.
