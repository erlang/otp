%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
%% Tests that basic cases of binary pattern matching work
%%--------------------------------------------------------------------
-module(bs_pmatch).

-export([test/0]).

test() ->
  %% construct some binaries
  Bin42 = <<42>>,
  Bin = <<12,17,42,0,0,0>>,
  BinSS = <<0,1,0,0,0>>,
  %% do some pattern matching
  ok = pm_const(Bin42),
  <<17,42,0,0,0>> = pm_tail(Bin),
  42 = pm_little(<<0:1,42:7>>),
  42 = pm_rec(Bin),
  30 = pm_rec_acc(<<1,2,3,4,5,6,7,8,9,10>>, 0),
  42 = pm_binary_tuple(Bin42),
  -1 = pm_with_illegal_float(),
  %% do some pattern matching with bound segments
  ok = pm_bound_var(),
  ok = pm_bound_tail(),
  %% do some tests with floating point numbers
  ok = pm_float(),
  ok = pm_float_little(),
  %% do some pattern matching with segments of unknown sizes
  {<<17>>, <<42,0,0,0>>} = pm_body_s(Bin, 1),
  {<<17>>, <<42,0,0,0>>} = pm_body_ss(Bin, 1, 4),
  {<<45>>, <<>>} = pm_size_split(<<1:16,45>>),
  {<<45>>, <<46,47>>} = pm_size_split(<<1:16,45,46,47>>),
  {<<45,46>>, <<47>>} = pm_size_split(<<2:16,45,46,47>>),
  {<<45,46>>, <<47>>} = pm_size_split_2(2, <<2:16,45,46,47>>),
  {'EXIT',{function_clause,_}} = (catch pm_size_split_2(42, <<2:16,45,46,47>>)),
  {<<45,46,47>>, <<48>>} = pm_sizes_split(<<16:8,3:16,45,46,47,48>>),
  <<"cdef">> = pm_skip_segment(<<2:8, "abcdef">>),
  -1 = pm_double_size_in_head(BinSS),
  -1 = pm_double_size_in_body(BinSS),
  %% and finally some cases which were problematic for various reasons
  ok = pm_bigs(),
  ok = pm_sean(),
  ok = pm_bin8(<<1,2,3,4,5,6,7,8>>),
  ok = pm_bs_match_string(),
  ok = pm_till_gc(),
  ok.

%%--------------------
%% Test cases below
%%--------------------

pm_const(<<42>>) ->
  ok.

pm_tail(<<12, Bin/binary>>) ->
  Bin.

pm_little(<<_:1, X:15/little>>) ->
  {wrong, X};
pm_little(<<_:1, X:7/little>>) ->
  X.

pm_rec(<<12, Bin/binary>>) ->
  pm_rec(Bin);
pm_rec(<<17, Word:4/little-signed-integer-unit:8>>) ->
  Word.

pm_rec_acc(<<_:4, A:4, Rest/binary>>, Acc) ->
 case Rest of
   <<X, Y, 9, NewRest/binary>> ->
     pm_rec_acc(NewRest, X+Y+Acc);
   <<X, 5, NewRest/binary>> ->
     pm_rec_acc(NewRest, X+Acc);
   <<2, NewRest/binary>> ->
     pm_rec_acc(NewRest, 1+Acc);
   <<NewRest/binary>> ->
     pm_rec_acc(NewRest, A+Acc)
  end;
pm_rec_acc(<<>>, Acc) ->
  Acc.

pm_binary_tuple(<<X>>) ->
  X;
pm_binary_tuple({Y, Z}) ->
  Y + Z.

pm_with_illegal_float() ->
  Bin = <<-1:64>>,       % create a binary which is illegal as float
  pm_float_integer(Bin). % try to match it out as a float

pm_float_integer(<<F:64/float>>) -> F;
pm_float_integer(<<I:64/integer-signed>>) -> I.

%%--------------------------------------------------------------------
%% Some tests with bound variables in segments

pm_bound_var() ->
  ok = pm_bound_var(42, 13, <<42,13>>),
  no = pm_bound_var(42, 13, <<42,255>>),
  no = pm_bound_var(42, 13, <<154,255>>),
  ok.

pm_bound_var(A, B, <<A:8, B:8>>) -> ok;
pm_bound_var(_, _, _) -> no.

pm_bound_tail() ->
  ok = pm_bound_tail(<<>>, <<13,14>>),
  ok = pm_bound_tail(<<2,3>>, <<1,1,2,3>>),
  no = pm_bound_tail(<<2,3>>, <<1,1,2,7>>),
  no = pm_bound_tail(<<2,3>>, <<1,1,2,3,4>>),
  no = pm_bound_tail(<<2,3>>, <<>>),
  ok.

pm_bound_tail(T, <<_:16, T/binary>>) -> ok;
pm_bound_tail(_, _) -> no.

%%--------------------------------------------------------------------
%% Floating point tests

pm_float() ->
  F = f1(),
  G = f_one(),
  G = match_float(<<63,128,0,0>>, 32, 0),
  G = match_float(<<63,240,0,0,0,0,0,0>>, 64, 0),
  fcmp(F, match_float(<<F:32/float>>, 32, 0)),
  fcmp(F, match_float(<<F:64/float>>, 64, 0)),
  fcmp(F, match_float(<<1:1,F:32/float,127:7>>, 32, 1)),
  fcmp(F, match_float(<<1:1,F:64/float,127:7>>, 64, 1)),
  fcmp(F, match_float(<<1:13,F:32/float,127:3>>, 32, 13)),
  fcmp(F, match_float(<<1:13,F:64/float,127:3>>, 64, 13)),
  ok.

fcmp(F1, F2) when (F1 - F2) / F2 < 0.0000001 -> ok.

match_float(Bin0, Fsz, I) ->
  Bin = make_sub_bin(Bin0),
  Bsz = bit_size(Bin),
  Tsz = Bsz - Fsz - I,
  <<_:I,F:Fsz/float,_:Tsz>> = Bin,
  F.

pm_float_little() ->
  F = f2(),
  G = f_one(),
  G = match_float_little(<<0,0,0,0,0,0,240,63>>, 64, 0),
  G = match_float_little(<<0,0,128,63>>, 32, 0),
  fcmp(F, match_float_little(<<F:32/float-little>>, 32, 0)),
  fcmp(F, match_float_little(<<F:64/float-little>>, 64, 0)),
  fcmp(F, match_float_little(<<1:1,F:32/float-little,127:7>>, 32, 1)),
  fcmp(F, match_float_little(<<1:1,F:64/float-little,127:7>>, 64, 1)),
  fcmp(F, match_float_little(<<1:13,F:32/float-little,127:3>>, 32, 13)),
  fcmp(F, match_float_little(<<1:13,F:64/float-little,127:3>>, 64, 13)),
  ok.

match_float_little(Bin0, Fsz, I) ->
  Bin = make_sub_bin(Bin0),
  Bsz = bit_size(Bin),
  Tsz = Bsz - Fsz - I,
  <<_:I, F:Fsz/float-little, _:Tsz>> = Bin,
  F.

make_sub_bin(Bin0) ->
  Sz = byte_size(Bin0),
  Bin1 = <<37,Bin0/binary,38,39>>,
  <<_:8,Bin:Sz/binary,_:8,_:8>> = Bin1,
  Bin.

f1() -> 3.1415.

f2() -> 2.7133.

f_one() -> 1.0.

%%--------------------------------------------------------------------
%% Some tests using size fields specified within the binary
pm_body_s(Bin, S1) ->
  <<12, B1:S1/binary, B2:4/binary>> = Bin, %% 4 is hard-coded
  {B1, B2}.

pm_body_ss(Bin, S1, S2) ->
  <<12, B1:S1/binary, B2:S2/binary>> = Bin,
  {B1, B2}.

pm_size_split(<<N:16, B:N/binary, T/binary>>) ->
  {B, T}.

pm_size_split_2(N, <<N:16, B:N/binary, T/binary>>) ->
  {B, T}.

pm_sizes_split(<<N0:8, N:N0, B:N/binary, T/binary>>) ->
  {B, T}.

pm_skip_segment(<<N:8, _:N/binary, T/binary>>) -> T.

%%--------------------------------------------------------------------
%% Some tests using multiple occurrences of size fields
pm_double_size_in_head(<<S:16, _:S/binary, _:S/binary, _/binary>>) ->
  -S.

pm_double_size_in_body(Bin) ->
  <<S:16, _:S/binary, _:S/binary, _/binary>> = Bin,
  -S.

%%--------------------------------------------------------------------
%% matching with 64-bit integers which become big nums
-define(BIG, 16#7fffffff7fffffff).

pm_bigs() ->
  <<X:64/little>> = <<?BIG:64/little>>,
  true = (X =:= big()),
  <<Y:64>> = <<?BIG:64>>,
  true = (Y =:= big()),
  ok.

big() -> ?BIG.

%%--------------------------------------------------------------------

pm_sean() ->
  small = sean1(<<>>),
  small = sean1(<<1>>),
  small = sean1(<<1,2>>),
  small = sean1(<<1,2,3>>),
  large = sean1(<<1,2,3,4>>),
  small = sean1(<<4>>),
  small = sean1(<<4,5>>),
  small = sean1(<<4,5,6>>),
  {'EXIT', {function_clause, _}} = (catch sean1(<<4,5,6,7>>)),
  ok.

sean1(<<B/binary>>) when byte_size(B) < 4 -> small;
sean1(<<1, _/binary>>) -> large.

%%--------------------------------------------------------------------
%% Crashed on SPARC due to a bug in linear scan register allocator
pm_bin8(<<A, B, C, D, E, F, G, H>>) ->
  10 = add4(A, B, C, D),
  26 = add4(E, F, G, H),
  ok.

add4(X, Y, Z, W) ->
  X + Y + Z + W.

%%--------------------------------------------------------------------
%% Cases that exposed bugs in the handling of bs_match_string with an
%% empty destination list. Reported on 2013/2/12 and fixed 2013/3/10.

pm_bs_match_string() ->
  Bin = <<42,42>>,
  Bin = pm_match_string_head(Bin),
  ok = (pm_match_string_fun())(Bin).

pm_match_string_head(<<42, _/bits>> = B) -> B.

pm_match_string_fun() ->
  fun (<<X, _/bits>>) when X =:= 42 -> ok end.

%%--------------------------------------------------------------------
%% Match a lot to force a garbage collection which exposed a bug

pm_till_gc() ->
  Bin = <<16#76543210:32>>,
  16#76543210 = pm_a_lot(Bin, 1000000),
  ok.

pm_a_lot(<<X:32>>, 0) ->
  X;
pm_a_lot(<<X:32>>, N) ->
  pm_a_lot(<<X:32>>, N-1).
