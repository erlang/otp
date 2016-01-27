%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains code examples that exhibited bugs in the BEAM compiler.
%%%-------------------------------------------------------------------
-module(basic_bugs_beam).

-export([test/0]).

%% the following is needed for the test_weird_message
-export([loop/1]).
%% the following are needed for the test_catch_bug
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

test() ->
  ok = test_fp_basic_blocks(),
  ok = test_weird_message(),
  ok = test_catch_bug(),
  ok.

%%--------------------------------------------------------------------
%% Test which shows that BEAM's splitting of basic blocks should take
%% into account that arithmetic operations implemented as BIFs can
%% also cause exceptions and thus calls to BIFs should end basic blocks.
%%
%% Investigated and fixed in the beginning of April 2004.
%%--------------------------------------------------------------------

test_fp_basic_blocks() ->
  ok = t1(),
  ok = t2().

t1() ->
  X = (catch bad_arith1(2.0, 1.7)),
  case X of
    {'EXIT', {badarith, _}} ->
      ok;
    _ ->
      error
  end.

bad_arith1(X, Y) when is_float(X) ->
  X1 = X * 1.7e+308,
  X2 = X1 + 1.0,
  Y1 = Y * 2,
  {X2, Y1}.

%% Similarly, it is not kosher to have anything that can fail inside
%% the fp block since it will throw the exception before the fp
%% exception and we will get the same problems.

t2() ->
  case catch bad_arith2(2.0, []) of
    {'EXIT', {badarith, _}} ->
      ok;
    _ ->
      error
  end.

bad_arith2(X, Y) when is_float(X) ->
  X1 = X * 1.7e+308,
  Y1 = element(1, Y),
  {X1 + 1.0, Y1}.

%%--------------------------------------------------------------------
%% Sending 'test' to this process should return 'ok'.  But:
%%
%% 1> MOD:test().
%% Weird: received true
%% timeout
%%
%% Surprisingly, the message has been bound to the value of 'ena'
%% in the record! The problem was visible in the .S file.
%%--------------------------------------------------------------------

-record(state, {ena = true}).

test_weird_message() ->
  P = spawn_link(?MODULE, loop, [#state{}]),
  P ! {msg, self()},
  receive
    What -> What
  after 42 -> timeout
  end.

loop(S) ->
  receive
    _ when S#state.ena == false ->
        io:format("Weird: ena is false\n");
        % loop(S);
    {msg, Pid} ->
        Pid ! ok;
        % loop(S);
    Other ->
        io:format("Weird: received ~p\n", [Other])
        % loop(S)
  end.

%%--------------------------------------------------------------------
%% This was posted on the Erlang mailing list as a question:
%%
%%   Given the module below and the function call
%%     "catch_bug:start_link(foo)."
%%   from the Erlang shell, why does Erlang crash with "Catch not found"?
%%
%% The BEAM compiler was generating wrong code for this case;
%% this was fixed in R9C-0.  Native code generation was OK.
%%--------------------------------------------------------------------

test_catch_bug() ->
  ignore = start_link(foo),
  ok.

start_link(Param) ->
  gen_server:start_link(?MODULE, Param, []).

init(Param) ->
  process_flag(trap_exit, true),
  (catch begin
           dummy(Param),
           (catch exit(bar))
         end
  ),
  ignore.

dummy(_) -> ok.

%% gen_server callbacks below
handle_call(_Call, _From, State) -> {noreply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

