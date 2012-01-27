-module(my_upgrade_test).

-export([version/0]).
-export([do_local/0]).
-export([do_real_local/0]).
-export([make_fun_exp/0]).
-export([make_fun_local/0]).


version() ->
    ?VERSION.

do_local() ->
    version().

do_real_local() ->
    local_version().

local_version() ->
    ?VERSION.

make_fun_exp() ->
    fun() -> ?MODULE:version() end.

make_fun_local() ->
    fun() -> local_version() end.
