-module(anno).

%% OTP-14131

-export([t1/0, t2/0, t3/0]).

t1() ->
    A = erl_parse:anno_from_term({attribute, 1, module, my_test}),
    compile:forms([A], []).

t2() ->
    A = erl_parse:new_anno({attribute, 1, module, my_test}),
    compile:forms([A], []).

t3() ->
    A = erl_parse:new_anno({attribute, 1, module, my_test}),
    T = erl_parse:anno_to_term(A),
    {attribute, 1, module, my_test} = T.
