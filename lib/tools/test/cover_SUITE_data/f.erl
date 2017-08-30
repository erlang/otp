-module(f).
-export([f1/0,f2/0,call_f2_when_isolated/0]).

f1() ->
    f1_line1,
    f1_line2.

f2() ->
    f2_line1,
    f2_line2.

call_f2_when_isolated() ->
    [Other] = nodes(),
    net_kernel:disconnect(Other),
    do_call_f2_when_isolated().

do_call_f2_when_isolated() ->
    case nodes() of
	[] ->
	    f2();
	_ ->
	    timer:sleep(100),
	    do_call_f2_when_isolated()
    end.
