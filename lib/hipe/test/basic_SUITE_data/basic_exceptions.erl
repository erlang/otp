%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains tests that raise exceptions and catch them.
%%%-------------------------------------------------------------------
-module(basic_exceptions).

-export([test/0, test_catches/0]).

%% functions used as arguments to spawn/3
-export([bad_guy/2]).

test() ->
  ok = test_catch_exit(42),
  ok = test_catch_throw(42),
  ok = test_catch_element(),
  ok = test_catch_crash(),
  ok = test_catch_empty(),
  ok = test_catch_merge(),
  ok = test_catches_merged(),
  ok = test_pending_errors(),
  ok = test_bad_fun_call(),
  ok.

%%--------------------------------------------------------------------
%% Written in 2001 by Erik Johansson.

test_catches() ->
  ExitBar = {'EXIT', bar},
  L1 = [ExitBar, ok, ExitBar, {ok, ExitBar}],
  L1 = [t1(), t2(), t3(), t4()],
  badarith = (catch element(1, element(2, t5(a, b)))),
  L2 = [42, ExitBar, ExitBar, {no_exception, ok}],
  L2 = [t5(21, 21), t6(), t7(), t8()],
  ok.

t1() ->
  catch foo().

t2() ->
  V = (catch ok()),
  s(),
  V.

t3() ->
  V = (catch foo()),
  V.

t4() ->
  V1 = ok(),
  V2 = (catch foo()),
  {V1, V2}.

t5(A, B) ->
  catch A + B.

t6() ->
  catch {no_exception, ok(), foo()}.

t7() ->
  catch {no_exception, foo(), ok()}.

t8() ->
  catch {no_exception, ok()}.

foo() ->
  s(),
  exit(bar).

ok() -> s(), ok.

s() -> nada.

%%--------------------------------------------------------------------

test_catch_exit(N) ->
  {'EXIT', N}  = (catch exit(N)),
  {'EXIT', 42} = (catch exit(42)),
  42 = try exit(N) catch exit:R1 -> R1 end,
  42 = try exit(42) catch exit:R2 -> R2 end,
  ok.

%%--------------------------------------------------------------------

test_catch_throw(N) ->
  N  = (catch throw(N)),
  42 = (catch throw(42)),
  42 = try throw(N) catch throw:R1 -> R1 end,
  42 = try throw(42) catch throw:R2 -> R2 end,
  ok.

%%--------------------------------------------------------------------

test_catch_element() ->
  'EXIT' = test_catch_element([]),
  'EXIT' = test_catch_element(42),
  ok.

test_catch_element(N) ->
  element(1, catch element(N, {1,2,3,4,5,6,7,8,9,10,11})).

%%--------------------------------------------------------------------

-define(try_match(E),
        catch ?MODULE:non_existing(),
	{'EXIT', {{badmatch, nomatch}, _}} = (catch E = no_match())).

test_catch_crash() ->
  ?try_match(a),
  ?try_match(42),
  ?try_match({a, b, c}),
  ?try_match([]),
  ?try_match(1.0),
  ok.

no_match() -> nomatch.

%% small_test() ->
%%   catch ?MODULE:non_existing(),
%%   io:format("Before\n",[]),
%%   hipe_bifs:show_nstack(self()),
%%   io:format("After\n",[]),
%%   garbage_collect().

%%--------------------------------------------------------------------
%% Tests whether the HiPE compiler optimizes catches in a way that
%% does not result in an infinite loop.
%%--------------------------------------------------------------------

test_catch_empty() ->
  badmatch().

badmatch() ->
  Big = ret_big(),
  Float = ret_float(),
  catch a = Big,
  catch b = Float,
  ok = case Big of Big -> ok end,
  ok = case Float of Float -> ok end,
  ok.

ret_big() ->
  329847987298478924982978248748729829487298292982972978239874.

ret_float() ->
  3.1415927.

%%--------------------------------------------------------------------
%% Test that shows how BEAM can merge catch-end blocks that belong to
%% different catch-start instructions. Written by Richard Carlsson.
%%--------------------------------------------------------------------

test_catch_merge() ->
  merge(get(whatever)).

merge(foo=X) ->
  catch f(X),
  catch g(X);
merge(X) ->
  catch f(X),
  catch g(X).

f(_) -> ok.

g(_) -> ok.

%%--------------------------------------------------------------------
%% Written by Tobias Lindahl.

test_catches_merged() ->
  {'EXIT', _} = merged_catches(foo),
  {'EXIT', {badarith, _}} = merged_catches(bar),
  {'EXIT', _} = merged_catches(baz),
  ok.

merged_catches(X) ->
  case X of
    foo -> catch fail1(0);
    bar -> catch {catch(1 = X), fail2(0)};
    baz -> catch fail3(0)
  end.

fail1(X) -> 1/X.

fail2(X) -> 1/X.

fail3(X) -> 1/X.

%%--------------------------------------------------------------------
%% Taken from exception_SUITE.erl
%%--------------------------------------------------------------------

test_pending_errors() ->
  error_logger:tty(false),    % disable printouts of error reports
  pending_errors().

%% Test various exceptions, in the presence of a previous error
%% suppressed in a guard.
pending_errors() ->
  pending(e_badmatch, {badmatch, b}),
  pending(x, function_clause),
  pending(e_case, {case_clause, xxx}),
  pending(e_if, if_clause),
  %% pending(e_badarith, badarith),
  %% pending(e_undef, undef),
  pending(e_timeoutval, timeout_value),
  %% pending(e_badarg, badarg),
  %% pending(e_badarg_spawn, badarg),
  ok.

bad_guy(pe_badarith, Other) when Other+1 =:= 0 -> % badarith (suppressed)
  ok;
bad_guy(pe_badarg, Other) when length(Other) > 0 -> % badarg (suppressed)
  ok;
bad_guy(_, e_case) ->
  case xxx() of
    ok -> ok
  end;                                        % case_clause
bad_guy(_, e_if) ->
  B = b(),
  if
    a == B -> ok
  end;                                        % if_clause
%% bad_guy(_, e_badarith) ->
%%   1+b;                                        % badarith
bad_guy(_, e_undef) ->
  non_existing_module:foo();                  % undef
bad_guy(_, e_timeoutval) ->
  receive
  after gazonk -> ok                          % timeout_value
  end;
bad_guy(_, e_badarg) ->
  node(xxx);                                  % badarg
bad_guy(_, e_badarg_spawn) ->
  spawn({}, {}, {});                          % badarg
bad_guy(_, e_badmatch) ->
  a = b().                                    % badmatch

xxx() -> xxx.

b() -> b.

pending(Arg, Expected) ->
  pending(pe_badarith, Arg, Expected),
  pending(pe_badarg, Arg, Expected).

pending(First, Second, Expected) ->
  pending_catched(First, Second, Expected),
  pending_exit_message([First, Second], Expected).

pending_catched(First, Second, Expected) ->
  %% ok = io:format("Catching bad_guy(~p, ~p)\n", [First, Second]),
  case catch bad_guy(First, Second) of
    {'EXIT', Reason} ->
      pending(Reason, bad_guy, [First, Second], Expected);
    Other ->
      exit({not_exit, Other})
  end.

pending_exit_message(Args, Expected) ->
  %% ok = io:format("Trapping exits from spawn_link(~p, ~p, ~p)\n",
  %%                [?MODULE, bad_guy, Args]),
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, bad_guy, Args),
  receive
    {'EXIT', Pid, Reason} ->
      pending(Reason, bad_guy, Args, Expected);
    Other ->
      exit({unexpected_message, Other})
  after 10000 ->
      exit(timeout)
  end,
  process_flag(trap_exit, false).

%% pending({badarg, [{erlang,Bif,BifArgs},{?MODULE,Func,Arity}|_]},
%%      Func, Args, _Code)
%%   when atom(Bif), list(BifArgs), length(Args) =:= Arity ->
%%     ok;
pending({badarg,Trace}, _, _, _) when is_list(Trace) ->
  ok;
%% pending({undef,[{non_existing_module,foo,[]}|_]}, _, _, _) ->
%%   ok;
pending({undef,Trace}, _, _, _)  when is_list(Trace) ->
  ok;
%% pending({function_clause,[{?MODULE,Func,Args}|_]}, Func, Args, _Code) ->
%%   ok;
pending({function_clause,Trace}, _, _, _) when is_list(Trace) ->
  ok;
%% pending({Code,[{?MODULE,Func,Arity}|_]}, Func, Args, Code)
%%   when length(Args) =:= Arity ->
%%     ok;
pending({Code,Trace}, _, _, Code) when is_list(Trace) ->
  ok;
pending(Reason, _Function, _Args, _Code) ->
  exit({bad_exit_reason, Reason}).

%%--------------------------------------------------------------------
%% Taken from fun_SUITE.erl
%%
%% Checks correct exception throwing when calling a bad fun.
%%--------------------------------------------------------------------

test_bad_fun_call() ->
  ok = bad_call_fc(42),
  ok = bad_call_fc(xx),
  ok = bad_call_fc({}),
  ok = bad_call_fc({1}),
  ok = bad_call_fc({1,2,3}),
  ok = bad_call_fc({1,2,3}),
  ok = bad_call_fc({1,2,3,4}),
  ok = bad_call_fc({1,2,3,4,5,6}),
  ok = bad_call_fc({1,2,3,4,5}),
  ok = bad_call_fc({1,2}),
  ok.

bad_call_fc(Fun) ->
  Args = [some, stupid, args],
  Res = (catch Fun(Fun(Args))),
  case Res of
    {'EXIT', {{badfun, Fun} ,_Where}} ->
      ok; %% = io:format("~p(~p) -> ~p\n", [Fun, Args, Res]);
    Other ->
      io:format("~p(~p) -> ~p\n", [Fun, Args, Res]),
      exit({bad_result, Other})
  end.
