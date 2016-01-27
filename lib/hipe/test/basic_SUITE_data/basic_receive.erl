%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains code examples that test correct handling of receives.
%%%-------------------------------------------------------------------
-module(basic_receive).

-export([test/0]).

test() ->
  ok = test_wait_timeout(),
  ok = test_double_timeout(),
  ok = test_reschedule(),
  ok.

%%--------------------------------------------------------------------

test_wait_timeout() ->
  receive after 42 -> ok end.

%%--------------------------------------------------------------------

test_double_timeout() ->
  self() ! foo,
  self() ! another_foo,
  receive
    non_existent -> weird
  after 0 -> timeout
  end,
  receive
    foo -> ok
  after 1000 -> timeout
  end.

%%--------------------------------------------------------------------
%% Check that RESCHEDULE returns from BIFs work.

test_reschedule() ->
  erts_debug:set_internal_state(available_internal_state, true),
  First = self(),
  Second = spawn(fun() -> doit(First) end),
  receive
    Second -> ok
  end,
  receive
  after 42 -> ok
  end,
  erts_debug:set_internal_state(hipe_test_reschedule_resume, Second),
  ok.

doit(First) ->
  First ! self(),
  erts_debug:set_internal_state(hipe_test_reschedule_suspend, 1).

%%--------------------------------------------------------------------
