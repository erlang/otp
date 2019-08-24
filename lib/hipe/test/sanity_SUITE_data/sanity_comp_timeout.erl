%%% -*- erlang-indent-level: 2 -*-
%%%----------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Tests that when the native code compilation times out or gets killed
%%% for some other reason, the parent process does not also get killed.
%%%
%%% Problem discovered by Bjorn G. on 1/12/2003 and fixed by Kostis.
%%%----------------------------------------------------------------------

-module(sanity_comp_timeout).

-export([test/0, to_llvm/0]).

test() ->
  ok = write_dummy_mod(),
  error_logger:tty(false), % disable printouts of error reports
  Self = self(),           % get the parent process
  c:c(dummy_mod, [native, {hipe, [{timeout, 1}]}]), % This will kill the process
  Self = self(),           % make sure the parent process stays the same
  ok.

to_llvm() -> false.

write_dummy_mod() ->
  Prog = <<"-module(dummy_mod).\n-export([test/0]).\ntest() -> ok.\n">>,
  ok = file:write_file("dummy_mod.erl", Prog).

