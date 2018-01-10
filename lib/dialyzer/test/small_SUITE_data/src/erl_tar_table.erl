-module(erl_tar_table).

%% OTP-14860, PR 1670.

-export([t/0, v/0, x/0]).

t() ->
    {ok, ["file"]} = erl_tar:table("table.tar").

v() ->
    {ok, [{_,_,_,_,_,_,_}]} = erl_tar:table("table.tar", [verbose]).

x() ->
    {ok, ["file"]} = erl_tar:table("table.tar", []).
