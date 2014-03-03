%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains code examples that exhibited bugs in the HiPE compiler.
%%%-------------------------------------------------------------------
-module(basic_bugs_hipe).

-export([test/0]).

test() ->
  ok = test_ets_bifs(),
  ok = test_bit_shift(),
  ok = test_R12B5_seg_fault(),
  ok.

%%--------------------------------------------------------------------
%% From: Bjorn Gustavsson
%%
%% This code, if HiPE compiled, crashed like this (on SPARC)
%%
%% (gdb) where
%% #0 fullsweep_heap (p=0x2c60dc, new_sz=610, objv=0xffbee8b4, nobj=3)
%%    at beam/ggc.c:1060
%% #1 0x7ff24 in erts_garbage_collect (p=0x2c60dc, need=2, objv=0x1128fc, ...)
%%    at beam/ggc.c:1648
%% #2 0xab6fc in hipe_mode_switch (p=0x2c60dc, cmd=704512, reg=0x1128fc)
%%    at hipe/hipe_mode_switch.c:180
%% #3 0x8e27c in process_main () at beam/beam_emu.c:3314
%% #4 0x31338 in erl_start (argc=9, argv=0xffbeed5c) at beam/erl_init.c:936
%% #5 0x2d9f4 in main (argc=9, argv=0xffbeed5c) at sys/unix/erl_main.c:28
%%
%% A guess at what could be the problem: From R8, many ets BIFs trap
%% to other ets BIFs with a *different* arity (i.e. they have more or
%% less arguments).  I have probably forgotten to mention that subtle
%% change.
%%--------------------------------------------------------------------

test_ets_bifs() ->
  Seed = {1032, 15890, 22716},
  put(random_seed, Seed),
  do_random_test().

do_random_test() ->
  OrdSet = ets:new(xxx, [ordered_set]),
  Set = ets:new(xxx, []),
  do_n_times(fun() ->
		 Key = create_random_string(25),
		 Value = create_random_tuple(25),
		 ets:insert(OrdSet, {Key, Value}),
		 ets:insert(Set, {Key, Value})
	     end, 5000),
  %% io:format("~nData inserted~n"),
  do_n_times(fun() ->
		 I = random:uniform(25),
		 Key = create_random_string(I) ++ '_',
		 L1 = ets_match_object(OrdSet, {Key, '_'}),
		 L2 = lists:sort(ets_match_object(Set, {Key, '_'})),
		 case L1 == L2 of
		   false ->
		     %% io:format("~p != ~p~n", [L1, L2]),
		     exit({not_eq, L1, L2});
		   true ->
		     ok
		 end
	     end,
	     2000),
  %% io:format("~nData matched~n"),
  ets:match_delete(OrdSet, '_'),
  ets:match_delete(Set, '_'),
  ok.

create_random_string(0) ->
  [];
create_random_string(OfLength) ->
  C = case random:uniform(2) of
        1 -> (random:uniform($Z - $A + 1) - 1) + $A;
        _ -> (random:uniform($z - $a + 1) - 1) + $a
      end,
  [C | create_random_string(OfLength - 1)].

create_random_tuple(OfLength) ->
  list_to_tuple([list_to_atom([X]) || X <- create_random_string(OfLength)]).

ets_match_object(Tab,Expr) ->
  case random:uniform(2) of
    1 -> ets:match_object(Tab,Expr);
    _ -> match_object_chunked(Tab,Expr)
  end.

match_object_chunked(Tab,Expr) ->
  match_object_chunked_collect(ets:match_object(Tab, Expr,
                                                random:uniform(1999) + 1)).

match_object_chunked_collect('$end_of_table') ->
  [];
match_object_chunked_collect({Results, Continuation}) ->
  Results ++ match_object_chunked_collect(ets:match_object(Continuation)).

do_n_times(_, 0) ->
  ok;
do_n_times(Fun, N) ->
  Fun(),
  case N rem 1000 of
    0 -> ok; %% WAS: io:format(".");
    _ -> ok
  end,
  do_n_times(Fun, N - 1).

%%--------------------------------------------------------------------
%% From: Niclas Pehrsson
%% Date: Apr 20, 2006
%%
%% We found something weird with the bit shifting in HiPE. It seems
%% that bsr in some cases shifts the bits in the wrong way...
%%
%% Fixed about 10 mins afterwards; was a bug in constant propagation.
%%--------------------------------------------------------------------

test_bit_shift() ->
  1 = plain_shift(),                 %  1
  6 = length_list_plus(),            %  6
  0 = shift_length_list(),           %  0
  1 = shift_length_list_plus(),      %  1
  1 = shift_length_list_plus2(),     %  1
  24 = shift_length_list_plus_bsl(), % 24
  1 = shift_fun(),                   %  1
  %% {1, 6, 0, 1, 1, 24, 1} = {A, B, C, D, E, F, G},
  ok.

plain_shift() ->
  6 bsr 2.

length_list() ->
  length([0,0]).

length_list_plus() ->
  length([0,0]) + 4.

shift_length_list() ->
  length([0,0]) bsr 2.

shift_length_list_plus() ->
  (length([0,0]) + 4) bsr 2.

shift_length_list_plus_bsl() ->
  (length([0,0]) + 4) bsl 2.

shift_length_list_plus2() ->
  N = length([0,0]) + 4,
  N bsr 2.

shift_fun() ->
  (length_list() + 4) bsr 2.

%%-----------------------------------------------------------------------
%% From: Sergey S, mid January 2009.
%%
%%   While I was playing with +native option, I run into a bug in HiPE
%%   which leads to segmentation fault using +native and Erlang R12B-5.
%%
%%   Eshell V5.6.5  (abort with ^G)
%%   1> crash:test().
%%   # Some message to be printed here each loop iteration
%%   Segmentation fault
%%
%% Diagnozed and fixed by Mikael Petterson (22 Jan 2009):
%%
%%   I've analysed the recently posted HiPE bug report on erlang-bugs
%%   <http://www.erlang.org/pipermail/erlang-bugs/2009-January/001162.html>.
%%   The segfault is caused by memory corruption, which in turn is caused
%%   by RTL removing an update of the HP (heap pointer) register due to
%%   what looks like broken liveness information.
%%-----------------------------------------------------------------------

test_R12B5_seg_fault() ->
  _ = spawn(fun() -> init() end),
  ok.

init() ->
  repeat(5, fun() -> void end),
  receive after infinity -> ok end.

repeat(0, _) ->
  ok;
repeat(N, Fun) ->
  %% io:format("# Some message to be printed here each loop iteration\n"),
  Fun(),
  repeat(N - 1, Fun).
