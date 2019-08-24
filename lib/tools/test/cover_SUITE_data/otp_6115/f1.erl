-module(f1).
-export([non_tail_call_f2_wait/0]).

non_tail_call_f2_wait() ->
    f2:wait(),
    im_back.
