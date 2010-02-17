-module(lib_no_lines).

-export([do_error/0, do_exit/0, do_hang/0, do_throw/0]).

do_error() ->
    io:format("Here comes a badmatch error...~n"),
    [] = lists:seq(1,2),
    ok.

do_exit() ->
    io:format("Here comes a byebye exit...~n"),
    exit(byebye),
    ok.

do_hang() ->
    io:format("Here comes a hang...~n"),
    receive after infinity -> ok end,
    ok.

do_throw() ->
    io:format("Here comes a throw...~n"),
    throw(catch_me_if_u_can),
    ok.
