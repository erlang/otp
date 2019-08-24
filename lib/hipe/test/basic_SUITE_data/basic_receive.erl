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
  ok = test_recv_mark(),
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
%% Check that we cannot cause a recv_mark,recv_set pair to misbehave and
%% deadlock the process.

test_recv_mark() ->
  ok = test_recv_mark(fun disturber_nop/0),
  ok = test_recv_mark(fun disturber_receive/0),
  ok = test_recv_mark(fun disturber_other/0),
  ok = test_recv_mark(fun disturber_recurse/0),
  ok = test_recv_mark_after(self(), fun disturber_after_recurse/0, false),
  ok = test_recv_mark(fun disturber_other_recurse/0),
  ok = test_recv_mark(fun disturber_other_after/0),
  ok = test_recv_mark_nested().

test_recv_mark(Disturber) ->
  Ref = make_ref(),
  self() ! Ref,
  Disturber(),
  receive Ref -> ok
  after 0 -> error(failure)
  end.

disturber_nop() -> ok.

disturber_receive() ->
  self() ! message,
  receive message -> ok end.

disturber_other() ->
  Ref = make_ref(),
  self() ! Ref,
  receive Ref -> ok end.

disturber_recurse() ->
  aborted = (catch test_recv_mark(fun() -> throw(aborted) end)),
  ok.

test_recv_mark_after(Recipient, Disturber, IsInner) ->
  Ref = make_ref(),
  Recipient ! Ref,
  Disturber(),
  receive
      Ref -> ok
  after 0 ->
          case IsInner of
              true -> expected;
              false -> error(failure)
          end
  end.

disturber_after_recurse() ->
  NoOp = fun() -> ok end,
  BlackHole = spawn(NoOp),
  expected = test_recv_mark_after(BlackHole, NoOp, true),
  ok.

disturber_other_recurse() ->
  aborted = (catch disturber_other_recurse(fun() -> throw(aborted) end)),
  ok.

disturber_other_recurse(InnerD) ->
  Ref = make_ref(),
  self() ! Ref,
  InnerD(),
  receive Ref -> ok
  after 0 -> error(failure)
  end.

disturber_other_after() ->
  BlackHole = spawn(fun() -> ok end),
  Ref = make_ref(),
  BlackHole ! Ref,
  receive Ref -> error(imposible)
  after 0 -> ok
  end.

test_recv_mark_nested() ->
  Ref1 = make_ref(),
  self() ! Ref1,
  begin
    Ref2 = make_ref(),
    self() ! Ref2,
    receive Ref2 -> ok end
  end,
  receive Ref1 -> ok
  after 0 -> error(failure)
  end.

%%--------------------------------------------------------------------
