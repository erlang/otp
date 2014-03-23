%%% -*- erlang-indent-level: 2 -*-
%%%----------------------------------------------------------------------
%%% Author: Per Gustafsson
%%%
%%% Checks that HiPE's concurrent compilation does not leave any zombie
%%% processes around after compilation has finished.
%%%
%%% This was a bug reported on erlang-bugs (Oct 25, 2007).
%%%----------------------------------------------------------------------

-module(sanity_no_zombies).

-export([test/0, to_llvm/0]).

test() ->
  L = length(processes()),
  hipe:c(?MODULE, [concurrent_comp]),   % force concurrent compilation
  L = length(processes()),
  ok.

to_llvm() -> false.
