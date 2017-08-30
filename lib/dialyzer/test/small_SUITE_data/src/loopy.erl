%% ERL-157, OTP-13653.
%% Would cause Dialyzer to go into an infinite loop.

-module(loopy).

-export([loop/1]).


-spec loop(Args) -> ok when
      Args :: [{Module, Args}],
      Module :: module(),
      Args :: any().
loop([{Module, Args} | Rest]) ->
    Module:init(Args),
    loop(Rest);
loop([]) ->
    ok.
