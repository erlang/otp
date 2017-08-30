%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
%% Contains three cases of bugs that were reported for R12B
%%--------------------------------------------------------------------
-module(bs_bugs_R12).

-export([test/0]).

test() ->
  ok = test_beam_bug(),
  ok = test_v3_codegen(),
  ok = test_hipe_bug(),
  ok.

%%--------------------------------
%% First test case: a bug in BEAM
%%--------------------------------
test_beam_bug() ->
  lists:foreach(fun (_) -> ok = run(100) end, [1,2,3,4]).

%% For testing - runs scanner N number of times with same input
run(N) ->
  lists:foreach(fun(_) -> scan(<<"region:whatever">>, []) end, lists:seq(1, N)).

scan(<<>>, TokAcc) ->
  lists:reverse(['$thats_all_folks$' | TokAcc]);
scan(<<D, Z, Rest/binary>>, TokAcc)
  when (D =:= $D orelse D =:= $d) and
       ((Z =:= $\s) or (Z =:= $() or (Z =:= $))) ->
  scan(<<Z, Rest/binary>>, ['AND' | TokAcc]);
scan(<<D>>, TokAcc) when (D =:= $D) or (D =:= $d) ->
  scan(<<>>, ['AND' | TokAcc]);
scan(<<N, Z, Rest/binary>>, TokAcc)
  when (N =:= $N orelse N =:= $n) and
       ((Z =:= $\s) or (Z =:= $() or (Z =:= $))) ->
  scan(<<Z, Rest/binary>>, ['NOT' | TokAcc]);
scan(<<C, Rest/binary>>, TokAcc) when (C >= $A) and (C =< $Z);
                                      (C >= $a) and (C =< $z);
                                      (C >= $0) and (C =< $9) ->
  case Rest of
    <<$:, R/binary>> ->
      scan(R, [{'FIELD', C} | TokAcc]);
    _ ->
      scan(Rest, [{'KEYWORD', C} | TokAcc])
  end.

%%---------------------------------------------------
%% Second test case: an internal error in v3_codegen
%% Reported by Mateusz Berezecki on 19/1/2008
%%---------------------------------------------------
-define(S, {42, 4242, 4711}).
-define(R, <<90,164,116>>).

test_v3_codegen() ->
  _ = random:seed(?S),
  B0 = gen_bit(120, <<>>),
  B1 = set_bit(B0, 5),
  B2 = clr_bit(B1, 5),
  ?R = set_bit(B2, 5),
  ok.

gen_bit(0, Acc) -> Acc;
gen_bit(N, Acc) when is_integer(N), N > 0 ->
  gen_bit(N-1, <<Acc/bits, (random:uniform(2)-1):1>>).

%% sets bit K in the Bitmap
set_bit(<<_Start:32/unsigned-little-integer, Bitmap/bits>>, K)
  when is_integer(K), 0 < K, K =< bit_size(Bitmap) ->
  Before = K-1,
  After = bit_size(Bitmap) - K,
  <<BeforeBits:Before/bits, _:1, AfterBits:After/bits>> = Bitmap,
  <<BeforeBits/bits, 1:1, AfterBits/bits>>.

%% clears bit K in the Bitmap
clr_bit(<<_Start:32/unsigned-little-integer, Bitmap/bits>>, K)
  when is_integer(K), 0 < K, K =< bit_size(Bitmap) ->
  Before = K-1,
  After = bit_size(Bitmap) - K,
  <<BeforeBits:Before/bits, _:1, AfterBits:After/bits>> = Bitmap,
  <<BeforeBits/bits, 0:1, AfterBits/bits>>.

%%--------------------------------------------------------------------
%% Third test case: a bug in HiPE
%% Reported by Steve Vinoski on 1/3/2008
%%
%% Below find the results of compiling and running the example code at
%% the bottom of this message. Using "c" to compile gives the right
%% answer; using "hipe:c" gives the wrong answer. This is with R12B-1.
%%
%% Within the code, on the second instance of function check/2 you'll
%% find a commented-out guard. If you uncomment the guard, then the
%% code works correctly with both "c" and "hipe:c".
%%---------------------------------------------------------------------

test_hipe_bug() ->
  String = "2006/10/02/Linux-Journal",
  Binary = list_to_binary(String),
  StringToMatch = "200x/" ++ String ++ " ",
  BinaryToMatch = list_to_binary(StringToMatch),
  {ok, Binary} = match(BinaryToMatch),
  ok.

match(<<>>) ->
  nomatch;
match(Bin) ->
  <<Front:16/binary, Tail/binary>> = Bin,
  case Front of
    <<_:3/binary,"x/",Y:4/binary,$/,M:2/binary,$/,D:2/binary,$/>> ->
      case check(Tail) of
	{ok, Match} ->
          {ok, <<Y/binary,$/,M/binary,$/,D/binary,$/,Match/binary>>};
	{nomatch, Skip} ->
          {skip, Skip+size(Front)};
	_ ->
	  wrong_answer
      end;
    _ ->
      nomatch
  end.

check(Bin) ->
  check(Bin, 0).
check(<<$ , _/binary>>, 0) ->
  {nomatch, 0};
check(Bin, Len) ->  %when Len < size(Bin) ->
  case Bin of
    <<Front:Len/binary, $ , _/binary>> ->
      {ok, Front};
    <<_:Len/binary, $., _/binary>> ->
      {nomatch, Len};
    _ ->
      check(Bin, Len+1)
  end.
