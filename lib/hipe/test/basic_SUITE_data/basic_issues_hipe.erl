%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains code examples that exhibited crashes in the HiPE compiler.
%%%-------------------------------------------------------------------
-module(basic_issues_hipe).

-export([test/0]).

%% functions that need to be exported so that they are retained.
-export([auth/4]).

test() ->
  ok = test_dominance_trees(),
  ok = test_merged_const(),
  ok = test_var_pair(),
  ok = test_bif_fails(),
  ok = test_find_catches(),
  ok = test_heap_allocate_trim(),
  ok.

%%--------------------------------------------------------------------
%% This is taken from a file sent to us by Martin Bjorklund @ Nortel
%% on 14th November 2004. The problem was in the SSA unconvert pass.
%%
%% No tests here; we simply check that the HiPE compiler does not go
%% into an infinite loop when compiling strange functions like this.
%%--------------------------------------------------------------------

auth(_, A, B, C) ->
  auth(A, B, C, []).

%%--------------------------------------------------------------------
%% Exposed a crash in the generation of dominance trees used in SSA.
%%--------------------------------------------------------------------

-record(state, {f}).

test_dominance_trees() ->
  {ok, true} = doit(true, #state{f = true}),
  ok.

doit(Foo, S) ->
  Fee = case Foo of
	  Bar when Bar == S#state.f; Bar == [] -> true;
	  _ -> false
	end,
  {ok, Fee}.

%%--------------------------------------------------------------------
%% Checks that the merging of constants in the constant table uses the
%% appropriate comparison function for this.
%%--------------------------------------------------------------------

test_merged_const() ->
  Const1 = {'', 1.0000},
  Const2 = {'', 1},
  match(Const1, Const2).

match(A, A) ->
  error;
match(_A, _B) ->
  ok.

%%--------------------------------------------------------------------
%% Checks that the HiPE compiler does not get confused by constant
%% data structures similar to the internal compiler data structures.
%%--------------------------------------------------------------------

test_var_pair() ->
  ok = var_pair([gazonk]).

var_pair([_|_]) ->
  var_pair({var, some_atom});
var_pair(_) ->
  ok.

%%--------------------------------------------------------------------
%% This module was causing the HiPE compiler to crash in January 2007.
%% The culprit was an "optimization" of the BEAM compiler: postponing
%% the save of x variables when BIFs cannot fail.  This was fixed on
%% February 1st, by making the HiPE compiler use the same functions
%% as the BEAM compiler for deciding whether a BIF fails.
%%--------------------------------------------------------------------

test_bif_fails() ->
  [42] = bif_fails_in_catch([42]),
  true = bif_fails_in_try([42]),
  ok.

bif_fails_in_catch(X) ->
  case catch get(gazonk) of
    _ -> X
  end.

bif_fails_in_try(X) ->
  try
    true = X =/= []
  catch
    _ -> nil(X)
  end.

nil(_) -> [].

%%--------------------------------------------------------------------
%% Test that resulted in a native code compiler crash in the code of
%% hipe_icode_exceptions:find_catches/1 when compiling find_catches/2.
%%--------------------------------------------------------------------

test_find_catches() ->
  42 = find_catches(a, false),
  ok.

find_catches(X, Y) ->
  case X of
    a when Y =:= true ->
      catch id(X),
      X;
    b when Y =:= true ->
      catch id(X),
      X;
    a ->
      catch id(X),
      42;
    b ->
      catch id(X),
      42
  end.

id(X) -> X.

%%--------------------------------------------------------------------
%% Date: Dec 28, 2007
%%
%% This is a test adapted from the file sent to the Erlang mailing
%% list by Eranga Udesh. The file did not compile because of problems
%% with the heap_allocate instruction and stack trimming.
%%--------------------------------------------------------------------

test_heap_allocate_trim() ->
  {abandon, 42} = get_next_retry(a, 42),
  ok.

get_next_retry(Error, Count) ->
  case catch pair(retry_scheme, {Error, Count}) of
    _ ->
      case pair(Error, Count) of
        _ -> {abandon, Count}
      end
  end.

pair(A, B) -> {A, B}.
