-module(on_load).
-export([?MODULE/0]).

-on_load(do_on_load/0).

%% Only test that the compiler is able to compile a module
%% with an on_load attribute. (There will be more thorough tests
%% of the functionality in code_SUITE in the Kernel application.)

?MODULE() ->
    ok.

do_on_load() ->
    local_function(),
    ok.

local_function() ->
    ok.
