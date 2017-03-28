%%% -*- erlang-indent-level: 2 -*-
%%%----------------------------------------------------------------------
%%% Author: Kostis Sagonas
%%%
%%% Contains code examples that exhibited bugs in the HiPE compiler.
%%%----------------------------------------------------------------------
-module(basic_bugs_hipe).

-export([test/0]).

test() ->
  ok = test_ets_bifs(),
  ok = test_szar_bug(),
  ok = test_bit_shift(),
  ok = test_match_big_list(),
  ok = test_unsafe_bsl(),
  ok = test_unsafe_bsr(),
  ok = test_R12B5_seg_fault(),
  ok = test_switch_neg_int(),
  ok = test_icode_range_anal(),
  ok = test_icode_range_call(),
  ok.

%%-----------------------------------------------------------------------
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
%%-----------------------------------------------------------------------

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
	     end, 2000),
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

%%-----------------------------------------------------------------------
%% From: Jozsef Berces (PR/ECZ)
%% Date: Feb 19, 2004
%%
%% Program which was added to the testsuite as a result of another bug
%% report involving tuples as funs.  Thanks God, these are no longer
%% supported, but the following is a good test for testing calling
%% native code funs from BEAM code (lists:map, lists:filter, ...).
%%-----------------------------------------------------------------------

test_szar_bug() ->
  ["A","B","C"] = smartconcat([], "H'A, H'B, H'C"),
  ok.

smartconcat(B, L) ->
  LL = tokenize(L, $,),
  NewlineDel = fun (X) -> killcontrol(X) end,
  StripFun = fun (X) -> string:strip(X) end,
  LL2 = lists:map(NewlineDel, lists:map(StripFun, LL)),
  EmptyDel = fun(X) ->
		 case string:len(X) of
		   0 -> false;
		   _ -> true
		 end
	     end,
  LL3 = lists:filter(EmptyDel, LL2),
  HexFormat = fun(X, Acc) ->
		  case string:str(X, "H'") of
		    1 ->
		      case checkhex(string:substr(X, 3)) of
			{ok, Y} ->
			  {Y, Acc};
			_ ->
			  {X, Acc + 1}
		      end;
                    _ ->
		      {X, Acc + 1}
		  end
	      end,
  {LL4,_Ret} = lists:mapfoldl(HexFormat, 0, LL3),
  lists:append(B, lists:sublist(LL4, lists:max([0, 25 - length(B)]))).

checkhex(L) ->
  checkhex(L, "").

checkhex([H | T], N) when H >= $0, H =< $9 ->
  checkhex(T, [H | N]);
checkhex([H | T], N) when H >= $A, H =< $F ->
  checkhex(T, [H | N]);
checkhex([H | T], N) when H =< 32 ->
  checkhex(T, N);
checkhex([_ | _], _) ->
  {error, ""};
checkhex([], N) ->
  {ok, lists:reverse(N)}.

killcontrol([C | S]) when C < 32 ->
  killcontrol(S);
killcontrol([C | S]) ->
  [C | killcontrol(S)];
killcontrol([]) ->
  [].

tokenize(L, C) ->
  tokenize(L, C, [], []).

tokenize([C | T], C, A, B) ->
  case A of
   [] ->
      tokenize(T, C, [], B);
    _ ->
      tokenize(T, C, [], [lists:reverse(A) | B])
  end;
tokenize([H | T], C, A, B) ->
  tokenize(T, C, [H | A], B);
tokenize(_, _, [], B) ->
  lists:reverse(B);
tokenize(_, _, A, B) ->
  lists:reverse([lists:reverse(A) | B]).

%%-----------------------------------------------------------------------
%% From: Niclas Pehrsson
%% Date: Apr 20, 2006
%%
%% We found something weird with the bit shifting in HiPE. It seems
%% that bsr in some cases shifts the bits in the wrong way...
%%
%% Fixed about 10 mins afterwards; was a bug in constant propagation.
%%-----------------------------------------------------------------------

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
%% From: Igor Goryachev
%% Date: June 15, 2006
%%
%% I have experienced a different behaviour and possibly a weird result
%% while playing with matching a big list on x86 and x86_64 machines.
%%-----------------------------------------------------------------------

-define(BIG_LIST,
	["uid", "nickname", "n_family", "n_given", "email_pref",
          "tel_home_number", "tel_cellular_number", "adr_home_country",
          "adr_home_locality", "adr_home_region", "url", "gender", "bday",
          "constitution", "height", "weight", "hair", "routine", "smoke",
          "maritalstatus", "children", "independence", "school_number",
          "school_locality", "school_title", "school_period", "org_orgname",
          "title", "adr_work_locality", "photo_type", "photo_binval"]).

test_match_big_list() ->
  case create_tuple_with_big_const_list() of
    {selected, ?BIG_LIST, _} -> ok;
    _ -> weird
  end.

create_tuple_with_big_const_list() ->
  {selected, ?BIG_LIST, [{"test"}]}.

%%-----------------------------------------------------------------------
%% In October 2006 the HiPE compiler acquired more type-driven
%% optimisations of arithmetic operations. One of these, the
%% transformation of bsl to a pure fixnum bsl fixnum -> fixnum version
%% (unsafe_bsl), was incorrectly performed even when the result
%% wouldn't be a fixnum. The error occurred for all backends, but the
%% only place known to break was hipe_arm:imm_to_am1/2. Some
%% immediates got broken on ARM, causing segmentation faults in
%% compiler_tests when HiPE recompiled itself.
%%-----------------------------------------------------------------------

test_unsafe_bsl() ->
  ok = bsl_check(bsl_test_cases()).

bsl_test_cases() ->
  [{16#FF, {16#FF, 0}},
   {16#F000000F, {16#FF, 2}}].

bsl_check([]) -> ok;
bsl_check([{X, Y}|Rest]) ->
  case imm_to_am1(X) of
    Y -> bsl_check(Rest);
    _ -> 'hipe_broke_bsl'
  end.

imm_to_am1(Imm) ->
  imm_to_am1(Imm band 16#FFFFFFFF, 16).
imm_to_am1(Imm, RotCnt) ->
  if Imm >= 0, Imm =< 255 -> {Imm, RotCnt band 15};
     true ->
      NewRotCnt = RotCnt - 1,
      if NewRotCnt =:= 0 -> []; % full circle, no joy
         true ->
          NewImm = (Imm bsr 2) bor ((Imm band 3) bsl 30),
          imm_to_am1(NewImm, NewRotCnt)
      end
  end.

%%-----------------------------------------------------------------------
%% Another transformation, namely that of bsr to a pure fixnum bsr
%% fixnum -> fixnum version (unsafe_bsr), failed to check for shifts
%% larger than the number of bits in fixnums. Such shifts should
%% return zero, but instead they became plain machine-level shift
%% instructions.  Machines often only consider the low-order bits of
%% the shift count, so machine-level shifts larger than the word size
%% do not match the Erlang semantics.
%%-----------------------------------------------------------------------

test_unsafe_bsr() ->
  ok = bsr_check(bsr_test_cases()).

bsr_test_cases() ->
  [{16#FF, 4, 16#0F},
   {16#FF, 64, 0}].

bsr_check([]) -> ok;
bsr_check([{X, Y, Z}|Rest]) ->
  case do_bsr(X, Y) of
    Z -> bsr_check(Rest);
    _ -> 'hipe_broke_bsr'
  end.

do_bsr(X, Y) ->
  (X band 16#FFFF) bsr (Y band 16#FFFF).

%%-----------------------------------------------------------------------
%% From: Sergey S, mid January 2009.
%%
%%   While I was playing with +native option, I run into a bug in HiPE
%%   which leads to segmentation fault using +native and Erlang R12B-5.
%%
%%   Eshell V5.6.5
%%   1> crash:test().
%%   # Some message to be printed here each loop iteration
%%   Segmentation fault
%%
%% Diagnosed and fixed by Mikael Pettersson (22 Jan 2009):
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

%%-----------------------------------------------------------------------
%% From: Jon Meredith
%% Date: July 9, 2009
%%
%% Binary search key tables are sorted by the loader based on the
%% runtime representations of the keys as unsigned words.  However,
%% the code generated for the binary search used signed comparisons.
%% That worked for atoms and non-negative fixnums, but not for
%% negative fixnums.  Fixed by Mikael Pettersson July 10, 2009.
%%-----------------------------------------------------------------------

test_switch_neg_int() ->
  ok = f(-80, 8).

f(10, -1) -> ok;
f(X, Y) ->
  Y = g(X),
  f(X + 10, Y - 1).

g(X) -> % g(0) should be 0 but became -1
  case X of
      0 -> 0;
    -10 -> 1;
    -20 -> 2;
    -30 -> 3;
    -40 -> 4;
    -50 -> 5;
    -60 -> 6;
    -70 -> 7;
    -80 -> 8;
      _ -> -1
  end.

%%-----------------------------------------------------------------------
%% From: Paul Guyot
%% Date: Jan 31, 2011
%%
%%   There is a bug in HiPE compilation with the comparison of floats
%%   with integers. This bug happens in functions f/1 and g/2 below.
%%   BEAM will evaluate f_eq(42) and f_eq(42.0) to true, while HiPE
%%   will evaluate them to false.
%%
%% The culprit was the Icode range analysis which was buggy. (On the
%% other hand, HiPE properly evaluated these calls to true if passed
%% the option 'no_icode_range'.)  Fixed by Kostis Sagonas.
%% --------------------------------------------------------------------

test_icode_range_anal() ->
  true = f_eq(42),
  true = f_eq(42.0),
  false = f_ne(42),
  false = f_ne(42.0),
  false = f_eq_ex(42),
  false = f_eq_ex(42.0),
  true = f_ne_ex(42),
  true = f_ne_ex(42.0),
  false = f_gt(42),
  false = f_gt(42.0),
  true = f_le(42),
  true = f_le(42.0),
  zero_test = g(0, test),
  zero_test = g(0.0, test),
  non_zero_test = g(42, test),
  other = g(42, other),
  ok.

f_eq(X) ->
  Y = X / 2,
  Y == 21.

f_ne(X) ->
  Y = X / 2,
  Y /= 21.

f_eq_ex(X) ->
  Y = X / 2,
  Y =:= 21.

f_ne_ex(X) ->
  Y = X / 2,
  Y =/= 21.

f_gt(X) ->
  Y = X / 2,
  Y > 21.

f_le(X) ->
  Y = X / 2,
  Y =< 21.

g(X, Z) ->
  Y = X / 2,
  case Z of
    test when Y == 0 -> zero_test;
    test -> non_zero_test;
    other -> other
  end.

%%-----------------------------------------------------------------------
%% From: Rich Neswold
%% Date: Oct 5, 2016
%%
%% The following was a bug in the HiPE compiler's range analysis. The
%% function range_client/2 below would would not stop when N reached 0,
%% but keep recursing into the second clause forever.
%%
%% The problem turned out to be in hipe_icode_range:analyse_call/2,
%% which would note update the argument ranges of the callee if the
%% result of the call was ignored.
%% -----------------------------------------------------------------------
-define(TIMEOUT, 42).

test_icode_range_call() ->
    Self = self(),
    Client = spawn_link(fun() -> range_client(Self, 4) end),
    range_server(4, Client).

range_server(0, _Client) ->
    receive
        stopping -> ok;
        {called_with, 0} -> error(failure)
    after ?TIMEOUT -> error(timeout)
    end;
range_server(N, Client) ->
    receive
        {called_with, N} ->
            Client ! proceed
    after ?TIMEOUT -> error(timeout)
    end,
    range_server(N-1, Client). % tailcall (so the bug does not affect it)

range_client(Server, 0) ->
    Server ! stopping;
range_client(Server, N) ->
    Server ! {called_with, N},
    receive proceed -> ok end,
    range_client(Server, N - 1), % non-tailrecursive call with ignored result
    ok.
