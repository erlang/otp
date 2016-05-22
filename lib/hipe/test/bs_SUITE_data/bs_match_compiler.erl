%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : bs_match_compiler.erl
%%% 
%%%-------------------------------------------------------------------
-module(bs_match_compiler).
-compile(nowarn_shadow_vars).

-export([test/0]).
-export([exported_id/1, exported_id/2]).  %% needed by a test

test() -> 
  Funs = [fun fun_shadow/0, fun int_float/0, fun otp_5269/0, fun null_fields/0,
	  fun wiger/0, fun bin_tail/0, fun save_restore/0,
	  fun partitioned_bs_match/0, fun function_clause/0, fun unit/0,
	  fun shared_sub_bins/0, fun bin_and_float/0, fun dec_subidentifiers/0,
	  fun skip_optional_tag/0, fun wfbm/0, fun degenerated_match/0,
	  fun bs_sum/0, fun coverage/0, fun multiple_uses/0, fun zero_label/0,
	  fun followed_by_catch/0, fun matching_meets_construction/0,
	  fun simon/0, fun matching_and_andalso/0,
	  fun otp_7188/0, fun otp_7233/0, fun otp_7240/0, fun otp_7498/0,
	  fun match_string/0, fun zero_width/0, fun bad_size/0, fun haystack/0,
	  fun cover_beam_bool/0, fun matched_out_size/0, fun follow_fail_br/0,
	  fun no_partition/0, fun calling_a_binary/0, fun binary_in_map/0,
	  fun match_string_opt/0, fun map_and_binary/0,
	  fun unsafe_branch_caching/0],
  lists:foreach(fun (F) -> ok = F() end, Funs).


%%--------------------------------------------------------------------
%% OTP-5270

fun_shadow() ->
  7 = fun_shadow_1(),
  7 = fun_shadow_2(8),
  7 = fun_shadow_3(),
  no = fun_shadow_4(8),
  ok.

fun_shadow_1() ->
  L = 8,
  F = fun(<<L:L,B:L>>) -> B end,
  F(<<16:8, 7:16>>).

fun_shadow_2(L) ->
  F = fun(<<L:L,B:L>>) -> B end,
  F(<<16:8, 7:16>>).

fun_shadow_3() ->
  L = 8,
  F = fun(<<L:L,B:L,L:L>>) -> B end,
  F(<<16:8, 7:16,16:16>>).

fun_shadow_4(L) ->
  F = fun(<<L:L,B:L,L:L>>) -> B;
	 (_) -> no end,
  F(<<16:8, 7:16,15:16>>).

%%--------------------------------------------------------------------
%% OTP-5323

int_float() ->
  <<103133.0:64/float>> = <<103133:64/float>>,
  <<103133:64/float>> = <<103133:64/float>>,
  ok.

%%--------------------------------------------------------------------
%% Stolen from erl_eval_SUITE and modified.
%% OTP-5269. Bugs in the bit syntax.

otp_5269() ->
  check(fun() -> L = 8, F = fun(<<A:L,B:A>>) -> B end, F(<<16:8, 7:16>>) end, 7),
  check(fun() -> L = 8, <<A:L,B:A>> = <<16:8, 7:16>>, B end, 7),
  check(fun() -> U = 8, (fun(<<U:U>>) -> U end)(<<32:8>>) end, 32),
  check(fun() -> U = 8, [U || <<U:U>> <- [<<32:8>>]] end, [32]),
  check(fun() -> [X || <<A:8, B:A>> <- [<<16:8,19:16>>],
		       <<X:8>> <- [<<B:8>>]] end,
	[19]),
  check(fun() -> A = 4, B = 28, _ = bit_size(<<13:(A+(X=B))>>), X end, 28),
  check(fun() ->
	    <<Size,B:Size/binary,Rest/binary>> = <<2,"AB","CD">>,
	    {Size,B,Rest}
	end, {2,<<"AB">>,<<"CD">>}),
  check(fun() -> X = 32, [X || <<X:X>> <- [<<1:32>>,<<2:32>>,<<3:8>>]] end,
	%% "binsize variable"          ^
	[1,2]),
  check(fun() ->
	    (fun (<<A:1/binary, B:8/integer, _C:B/binary>>) ->
		 case A of
		   B -> wrong;
		   _ -> ok
		 end
	     end)(<<1,2,3,4>>) end,
	ok),
  ok.

%%--------------------------------------------------------------------

null_fields() ->
  check(fun() ->
	    W = id(0),
	    F = fun(<<_:W>>) -> tail;
		   (<<>>) -> empty
		end,
	    F(<<>>)
	end, tail),
  check(fun() ->
	    F = fun(<<_/binary>>) -> tail;
		   (<<>>) -> empty
		end,
	    F(<<>>)
	end, tail),
  ok.

%%--------------------------------------------------------------------

wiger() ->
  ok1 = wcheck(<<3>>),
  ok2 = wcheck(<<1,2,3>>),
  ok3 = wcheck(<<4>>),
  {error,<<1,2,3,4>>} = wcheck(<<1,2,3,4>>),
  {error,<<>>} = wcheck(<<>>),
  ok.

wcheck(<<A>>) when A==3->
  ok1;
wcheck(<<_,_:2/binary>>) ->
  ok2;
wcheck(<<_>>) ->
  ok3;
wcheck(Other) ->
  {error,Other}.

%%--------------------------------------------------------------------

bin_tail() ->
  S = <<"abcde">>,
  $a = bin_tail_c(S, 0),
  $c = bin_tail_c(S, 2),
  $e = bin_tail_c(S, 4),
  {'EXIT',_} = (catch bin_tail_c(S, 5)),
  {'EXIT',_} = (catch bin_tail_c_var(S, 5)),

  $a = bin_tail_d(S, 0),
  $b = bin_tail_d(S, 8),
  $d = bin_tail_d(S, 3*8),
  {'EXIT',_} = (catch bin_tail_d_dead(S, 1)),
  {'EXIT',_} = (catch bin_tail_d_dead(S, 9)),
  {'EXIT',_} = (catch bin_tail_d_dead(S, 5*8)),
  {'EXIT',_} = (catch bin_tail_d_var(S, 1)),

  ok = bin_tail_e(<<2:2,0:1,1:5>>),
  ok = bin_tail_e(<<2:2,1:1,1:5,42:64>>),
  error = bin_tail_e(<<3:2,1:1,1:5,42:64>>),
  error = bin_tail_e(<<>>),
  ok.

bin_tail_c(Bin, Offset) ->
  Res = bin_tail_c_dead(Bin, Offset),
  <<_:Offset/binary,_,Tail/binary>> = Bin,
  {Res,Tail} = bin_tail_c_var(Bin, Offset),
  Res.

bin_tail_c_dead(Bin, Offset) ->
  <<_:Offset/binary,C,_/binary>> = Bin,
  C.

bin_tail_c_var(Bin, Offset) ->
  <<_:Offset/binary,C,Tail/binary>> = Bin,
  {C,Tail}.

bin_tail_d(Bin, BitOffset) ->
  Res = bin_tail_d_dead(Bin, BitOffset),
  <<_:BitOffset,_:8,Tail/binary>> = Bin,
  {Res,Tail} = bin_tail_d_var(Bin, BitOffset),
  Res.

bin_tail_d_dead(Bin, BitOffset) ->
  <<_:BitOffset,C,_/binary>> = Bin,
  C.

bin_tail_d_var(Bin, BitOffset) ->
  <<_:BitOffset,C,Tail/binary>> = Bin,
  {C,Tail}.

bin_tail_e(Bin) ->
  case bin_tail_e_dead(Bin) of
    ok ->
      <<_,Tail/binary>> = Bin,
      Tail = bin_tail_e_var(Bin),
      ok;
    error ->
      bin_tail_e_var(Bin)
  end.

bin_tail_e_dead(Bin) ->
  case Bin of
    %% The binary is aligned at the end; neither the bs_skip_bits2 nor
    %% bs_test_tail2 instructions are needed.
    <<2:2,_:1,1:5,_/binary>> -> ok;
    _ -> error
  end.

bin_tail_e_var(Bin) ->
  case Bin of
    %% The binary is aligned at the end; neither the bs_skip_bits2 nor
    %% bs_test_tail2 instructions are needed.
    <<2:2,_:1,1:5,Tail/binary>> -> Tail;
    _ -> error
  end.

%%--------------------------------------------------------------------

save_restore() ->
  0 = save_restore_1(<<0:2,42:6>>),
  {1,3456} = save_restore_1(<<1:2,3456:14>>),
  {2,7981234} = save_restore_1(<<2:2,7981234:30>>),
  {3,763967493838} = save_restore_1(<<0:2,763967493838:62>>),

  A = <<" x">>,
  B = <<".x">>,
  C = <<"-x">>,

  {" ",<<"x">>} = lll(A),
  {" ",<<"x">>} = mmm(A),
  {" ",<<"x">>} = nnn(A),
  {" ",<<"x">>} = ooo(A),

  {".",<<"x">>} = lll(B),
  {".",<<"x">>} = mmm(B),
  {".",<<"x">>} = nnn(B),
  {".",<<"x">>} = ooo(B),

  {"-",<<"x">>} = lll(C),
  {"-",<<"x">>} = mmm(C),
  {"-",<<"x">>} = nnn(C),
  {"-",<<"x">>} = ooo(C),

  Bin = <<-1:64>>,
  case bad_float_unpack_match(Bin) of
    -1 -> ok;
    _Other -> bad_return_value_probably_NaN
  end.

save_restore_1(Bin) ->
  case Bin of
    <<0:2,_:6>> -> 0;
    <<1:2,A:14>> -> {1,A};
    <<2:2,A:30>> -> {2,A};
    <<A:64>> -> {3,A}
  end.

lll(<<Char,         Tail/binary>>) -> {[Char],Tail}.

mmm(<<$.,$.,$.,     Tail/binary>>) -> Tail;
mmm(<<$\s,$-,$\s,   Tail/binary>>) -> Tail;
mmm(<<Char,         Tail/binary>>) -> {[Char],Tail}. %% Buggy Tail!

nnn(<<"...",        Tail/binary>>) -> Tail;
nnn(<<" - ",        Tail/binary>>) -> Tail;
nnn(<<Char,         Tail/binary>>) -> {[Char],Tail}. %% Buggy Tail!

ooo(<<" - ",        Tail/binary>>) -> Tail;
ooo(<<Char,         Tail/binary>>) -> {[Char],Tail}.

bad_float_unpack_match(<<F:64/float>>) -> F;
bad_float_unpack_match(<<I:64/integer-signed>>) -> I.

%%--------------------------------------------------------------------

partitioned_bs_match() ->
  <<1,2,3>> = partitioned_bs_match(blurf, <<42,1,2,3>>),
  error = partitioned_bs_match(10, <<7,8,15,13>>),
  error = partitioned_bs_match(100, {a,tuple,is,'not',a,binary}),
  ok = partitioned_bs_match(0, <<>>),
  fc(partitioned_bs_match, [-1,blurf],
     catch partitioned_bs_match(-1, blurf)),
  fc(partitioned_bs_match, [-1,<<1,2,3>>],
     catch partitioned_bs_match(-1, <<1,2,3>>)),
  {17,<<1,2,3>>} = partitioned_bs_match_2(1, <<17,1,2,3>>),
  {7,<<1,2,3>>} = partitioned_bs_match_2(7, <<17,1,2,3>>),

  fc(partitioned_bs_match_2, [4,<<0:17>>],
     catch partitioned_bs_match_2(4, <<0:17>>)),

  anything = partitioned_bs_match_3(anything, <<42>>),
  ok = partitioned_bs_match_3(1, 2),
  ok.

partitioned_bs_match(_, <<42:8,T/binary>>) -> T;
partitioned_bs_match(N, _) when N > 0 -> error;
partitioned_bs_match(_, <<>>) -> ok.

partitioned_bs_match_2(1, <<B:8,T/binary>>) -> {B,T};
partitioned_bs_match_2(Len, <<_:8,T/binary>>) -> {Len,T}.

partitioned_bs_match_3(Var, <<_>>) -> Var;
partitioned_bs_match_3(1, 2) -> ok.

%%--------------------------------------------------------------------

function_clause()  ->
  ok = function_clause_1(<<0,7,0,7,42>>),
  fc(function_clause_1, [<<0,1,2,3>>],
     catch function_clause_1(<<0,1,2,3>>)),
  fc(function_clause_1, [<<0,1,2,3>>],
     catch function_clause_1(<<0,7,0,1,2,3>>)),
  
  ok = function_clause_2(<<0,7,0,7,42>>),
  ok = function_clause_2(<<255>>),
  ok = function_clause_2(<<13:4>>),
  fc(function_clause_2, [<<0,1,2,3>>],
     catch function_clause_2(<<0,1,2,3>>)),
  fc(function_clause_2, [<<0,1,2,3>>],
     catch function_clause_2(<<0,7,0,1,2,3>>)),
  ok.

function_clause_1(<<0:8,7:8,T/binary>>) ->
  function_clause_1(T);
function_clause_1(<<_:8>>) ->
  ok.

function_clause_2(<<0:8,7:8,T/binary>>) ->
  function_clause_2(T);
function_clause_2(<<_:8>>) ->
  ok;
function_clause_2(<<_:4>>) ->
  ok.

%%--------------------------------------------------------------------

unit() ->
  42 = peek1(<<42>>),
  43 = peek1(<<43,1,2>>),
  43 = peek1(<<43,1,2,(-1):1>>),
  43 = peek1(<<43,1,2,(-1):2>>),
  43 = peek1(<<43,1,2,(-1):7>>),

  99 = peek8(<<99>>),
  100 = peek8(<<100,101>>),
  fc(peek8, [<<100,101,0:1>>], catch peek8(<<100,101,0:1>>)),

  37484 = peek16(<<37484:16>>),
  37489 = peek16(<<37489:16,5566:16>>),
  fc(peek16, [<<8>>], catch peek16(<<8>>)),
  fc(peek16, [<<42:15>>], catch peek16(<<42:15>>)),
  fc(peek16, [<<1,2,3,4,5>>], catch peek16(<<1,2,3,4,5>>)),

  127 = peek7(<<127:7>>),
  100 = peek7(<<100:7,19:7>>),
  fc(peek7, [<<1,2>>], catch peek7(<<1,2>>)),
  ok.

peek1(<<B:8,_/bitstring>>) -> B.

peek7(<<B:7,_/binary-unit:7>>) -> B.

peek8(<<B:8,_/binary>>) -> B.

peek16(<<B:16,_/binary-unit:16>>) -> B.

%%--------------------------------------------------------------------

shared_sub_bins() ->
  {15,[<<>>,<<5>>,<<4,5>>,<<3,4,5>>,<<2,3,4,5>>]} = sum(<<1,2,3,4,5>>, [], 0),
  ok.

sum(<<B,T/binary>>, Acc, Sum) ->
  sum(T, [T|Acc], Sum+B);
sum(<<>>, Last, Sum) -> {Sum,Last}.

%%--------------------------------------------------------------------

bin_and_float() ->
  14.0 = bin_and_float(<<1.0/float,2.0/float,3.0/float>>, 0.0),
  ok.

bin_and_float(<<X/float,Y/float,Z/float,T/binary>>, Sum) when is_float(X),
							      is_float(Y),
							      is_float(Z) ->
  bin_and_float(T, Sum+X*X+Y*Y+Z*Z);
bin_and_float(<<>>, Sum) -> Sum.

%%--------------------------------------------------------------------

dec_subidentifiers() ->
  {[],<<1,2,3>>} =
    do_dec_subidentifiers(<<1:1,42:7,1:1,99:7,1,2,3>>, 0, [], 2),
  {[5389],<<1,2,3>>} =
    do_dec_subidentifiers(<<1:1,42:7,0:1,13:7,1,2,3>>, 0, [], 2),
  {[3,2,1],not_a_binary} = dec_subidentifiers(not_a_binary, any, [1,2,3], 0),
  ok.

do_dec_subidentifiers(Buffer, Av, Al, Len) -> 
  Res = dec_subidentifiers(Buffer, Av, Al, Len),
  Res = dec_subidentifiers2(Buffer, Av, Al, Len),
  Res = dec_subidentifiers4(Buffer, Av, Al, Len),
  Res = dec_subidentifiers3(Buffer, Av, Al, Len).

dec_subidentifiers(Buffer, _Av, Al, 0) -> 
  {lists:reverse(Al),Buffer}; 
dec_subidentifiers(<<1:1,H:7,T/binary>>, Av, Al, Len) -> 
  dec_subidentifiers(T, (Av bsl 7) bor H, Al, Len-1);
dec_subidentifiers(<<H,T/binary>>, Av, Al, Len) -> 
  dec_subidentifiers(T, 0, [((Av bsl 7) bor H)|Al], Len-1).

dec_subidentifiers2(<<Buffer/binary>>, _Av, Al, 0) ->
  {lists:reverse(Al),Buffer}; 
dec_subidentifiers2(<<1:1,H:7,T/binary>>, Av, Al, Len) -> 
  dec_subidentifiers2(T, (Av bsl 7) bor H, Al, Len-1); 
dec_subidentifiers2(<<H,T/binary>>, Av, Al, Len) -> 
  dec_subidentifiers2(T, 0, [((Av bsl 7) bor H)|Al], Len-1).

dec_subidentifiers3(Buffer, _Av, Al, 0) when is_binary(Buffer) ->
  {lists:reverse(Al),Buffer}; 
dec_subidentifiers3(<<1:1,H:7,T/binary>>, Av, Al, Len) -> 
  dec_subidentifiers3(T, (Av bsl 7) bor H, Al, Len-1); 
dec_subidentifiers3(<<H,T/binary>>, Av, Al, Len) -> 
  dec_subidentifiers3(T, 0, [((Av bsl 7) bor H)|Al], Len-1).

dec_subidentifiers4(<<1:1,H:7,T/binary>>, Av, Al, Len) when Len =/= 0 -> 
  dec_subidentifiers4(T, (Av bsl 7) bor H, Al, Len-1); 
dec_subidentifiers4(<<H,T/binary>>, Av, Al, Len) when Len =/= 0 -> 
  dec_subidentifiers4(T, 0, [((Av bsl 7) bor H)|Al], Len-1);
dec_subidentifiers4(Buffer, _Av, Al, 0) -> 
  {lists:reverse(Al),Buffer}.

%%--------------------------------------------------------------------

skip_optional_tag() ->
  {ok,<<>>} = skip_optional_tag(<<42>>, <<42>>),
  {ok,<<>>} = skip_optional_tag(<<42,1>>, <<42,1>>),
  {ok,<<1,2,3>>} = skip_optional_tag(<<42>>, <<42,1,2,3>>),
  missing = skip_optional_tag(<<2:3>>, blurf),
  ok.

skip_optional_tag(<<>>, Binary) ->
  {ok,Binary};
skip_optional_tag(<<Tag,RestTag/binary>>, <<Tag,Rest/binary>>) ->
  skip_optional_tag(RestTag, Rest);
skip_optional_tag(_, _) -> missing.

%%--------------------------------------------------------------------

-define(DATELEN, 16).

wfbm() ->
  %% check_for_dot_or_space and get_tail is from wfbm4 by Steve Vinoski,
  %% with modifications.
  {nomatch,0} = check_for_dot_or_space(<<" ">>),
  {nomatch,0} = check_for_dot_or_space(<<" abc">>),
  {ok,<<"abcde">>} = check_for_dot_or_space(<<"abcde 34555">>),
  {nomatch,0} = check_for_dot_or_space(<<".gurka">>),
  {nomatch,1} = check_for_dot_or_space(<<"g.urka">>),
  nomatch = get_tail(<<>>),
  {ok,<<"2007/10/23/blurf">>} = get_tail(<<"200x/2007/10/23/blurf ">>),
  {skip,?DATELEN+5} = get_tail(<<"200x/2007/10/23/blurf.">>),
  nomatch = get_tail(<<"200y.2007.10.23.blurf ">>),
  {'EXIT',_} = (catch get_tail({no,binary,at,all})),
  {'EXIT',_} = (catch get_tail(no_binary)),
  ok.

check_for_dot_or_space(Bin) ->
  check_for_dot_or_space(Bin, 0).

check_for_dot_or_space(<<$\s, _/binary>>, 0) ->
  {nomatch,0};
check_for_dot_or_space(Bin, Len) ->
  case Bin of
    <<Front:Len/binary, $\s, _/binary>> ->
      {ok,Front};
    <<_:Len/binary, $., _/binary>> ->
      {nomatch,Len};
    _ ->
      check_for_dot_or_space(Bin, Len+1)
  end.

get_tail(<<>>) ->
  nomatch;
get_tail(Bin) ->
  <<Front:?DATELEN/binary, Tail/binary>> = Bin,
  case Front of
    <<_:3/binary,"x/",Y:4/binary,$/,M:2/binary,$/,D:2/binary,$/>> ->
      case check_for_dot_or_space(Tail) of
	{ok,Match} ->
	  {ok,<<Y/binary,$/,M/binary,$/,D/binary,$/, Match/binary>>};
	{nomatch,Skip} -> {skip,?DATELEN + Skip}
      end;
    _ -> nomatch
  end.

%%--------------------------------------------------------------------

degenerated_match() ->
  error = degenerated_match_1(<<>>),
  1 = degenerated_match_1(<<1:1>>),
  2 = degenerated_match_1(<<42,43>>),

  error = degenerated_match_2(<<>>),
  no_split = degenerated_match_2(<<1,2>>),
  {<<1,2,3,4>>,<<5>>} = degenerated_match_2(<<1,2,3,4,5>>),
  ok.

degenerated_match_1(<<>>) -> error;
degenerated_match_1(Bin) -> byte_size(Bin).

degenerated_match_2(<<>>) -> error;
degenerated_match_2(Bin) ->
  case byte_size(Bin) > 4 of
    true -> split_binary(Bin, 4);
    false -> no_split
  end.

%%--------------------------------------------------------------------

bs_sum() ->
  0 = bs_sum_1([]),
  0 = bs_sum_1(<<>>),
  42 = bs_sum_1([42]),
  1 = bs_sum_1(<<1>>),
  10 = bs_sum_1([1,2,3,4]),
  15 = bs_sum_1(<<1,2,3,4,5>>),
  21 = bs_sum_1([1,2,3|<<4,5,6>>]),
  15 = bs_sum_1([1,2,3|{4,5}]),
  6 = bs_sum_1([1,2,3|zero]),
  6 = bs_sum_1([1,2,3|0]),
  7 = bs_sum_1([1,2,3|one]),

  fc(catch bs_sum_1({too,big,tuple})),
  fc(catch bs_sum_1([1,2,3|{too,big,tuple}])),

  [] = sneaky_alias(<<>>),
  [559,387655] = sneaky_alias(id(<<559:32,387655:32>>)),
  fc(sneaky_alias, [<<1>>], catch sneaky_alias(id(<<1>>))),
  fc(sneaky_alias, [[1,2,3,4]], catch sneaky_alias(lists:seq(1, 4))),
  ok.

bs_sum_1(<<H,T/binary>>) -> H+bs_sum_1(T);
bs_sum_1([H|T]) -> H+bs_sum_1(T);
bs_sum_1({A,B}=_Tuple=_AliasForNoGoodReason) -> A+B;
bs_sum_1(0) -> 0;
bs_sum_1(zero=_Zero) -> 0;
bs_sum_1(one) -> 1;
bs_sum_1([]) -> 0;
bs_sum_1(<<>>) -> 0.

sneaky_alias(<<>>=L) -> binary_to_list(L);
sneaky_alias(<<From:32,L/binary>>) -> [From|sneaky_alias(L)].

%%--------------------------------------------------------------------

coverage() ->
  0 = coverage_fold(fun(B, A) -> A+B end, 0, <<>>),
  6 = coverage_fold(fun(B, A) -> A+B end, 0, <<1,2,3>>),
  fc(catch coverage_fold(fun(B, A) -> A+B end, 0, [a,b,c])),

  {<<42.0:64/float>>,float} = coverage_build(<<>>, <<42>>, float),
  {<<>>,not_a_tuple} = coverage_build(<<>>, <<>>, not_a_tuple),
  {<<16#76,"abc",16#A9,"abc">>,{x,42,43}} =
    coverage_build(<<>>, <<16#7,16#A>>, {x,y,z}),

  [<<2>>,<<1>>] = coverage_bc(<<1,2>>, []),

  {x,<<"abc">>,z} = coverage_setelement(<<2,"abc">>, {x,y,z}),

  [42] = coverage_apply(<<42>>, [exported_id]),
  42 = coverage_external(<<42>>),

  do_coverage_bin_to_term_list([]),
  do_coverage_bin_to_term_list([lists:seq(0, 10),{a,b,c},<<23:42>>]),
  fc(coverage_bin_to_term_list, [<<0,0,0,7>>],
     catch do_coverage_bin_to_term_list_1(<<7:32>>)),

  <<>> = coverage_per_key(<<4:32>>),
  <<$a,$b,$c>> = coverage_per_key(<<7:32,"abc">>),

  ok.

coverage_fold(Fun, Acc, <<H,T/binary>>) ->
  IdFun = fun id/1,
  coverage_fold(Fun, Fun(IdFun(H), IdFun(Acc)), T);
coverage_fold(Fun, Acc, <<>>) when is_function(Fun, 2) -> Acc.

coverage_build(Acc0, <<H,T/binary>>, float) ->
  Float = id(<<H:64/float>>),
  Acc = <<Acc0/binary,Float/binary>>,
  coverage_build(Acc, T, float);
coverage_build(Acc0, <<H,T/binary>>, Tuple0) ->
  Str = id(<<H:(id(4)),(H-1):4,"abc">>),
  Acc = id(<<Acc0/bitstring,Str/bitstring>>),
  Tuple = setelement(2, setelement(3, Tuple0, 43), 42),
  if
    byte_size(Acc) > 0 ->
      coverage_build(Acc, T, Tuple)
  end;
coverage_build(Acc, <<>>, Tuple) -> {Acc,Tuple}.

coverage_bc(<<H,T/binary>>, Acc) ->
  B = << <<C:8>> || C <- [H] >>,
  coverage_bc(T, [B|Acc]);
coverage_bc(<<>>, Acc) -> Acc.

coverage_setelement(<<H,T1/binary>>, Tuple) when element(1, Tuple) =:= x ->
  setelement(H, Tuple, T1).

coverage_apply(<<H,T/binary>>, [F|Fs]) ->
  [?MODULE:F(H)|coverage_apply(T, Fs)];
coverage_apply(<<>>, []) -> [].

coverage_external(<<H,T/binary>>) ->
  ?MODULE:exported_id(T, T),
  H.

exported_id(I) -> id(I).

exported_id(_, _) -> ok.

do_coverage_bin_to_term_list(L) ->
  Bin = << <<(begin BinTerm = term_to_binary(Term),
		    <<(byte_size(BinTerm)):32,BinTerm/binary>> end)/binary>> ||
	  Term <- L >>,
  L = do_coverage_bin_to_term_list_1(Bin),
  L = do_coverage_bin_to_term_list_1(<<Bin/binary,7:32,"garbage">>),
  L = do_coverage_bin_to_term_list_1(<<7:32,"garbage",Bin/binary>>).   

do_coverage_bin_to_term_list_1(Bin) ->
  Res = coverage_bin_to_term_list(Bin),
  Res = coverage_bin_to_term_list(Bin, []),
  Res = coverage_bin_to_term_list_catch(Bin),
  Res = coverage_bin_to_term_list_catch(Bin, []).

coverage_bin_to_term_list(<<Sz:32,BinTerm:Sz/binary,T/binary>>) ->
  try binary_to_term(BinTerm) of
      Term -> [Term|coverage_bin_to_term_list(T)]
  catch
    error:badarg -> coverage_bin_to_term_list(T)
  end;
coverage_bin_to_term_list(<<>>) -> [].

coverage_bin_to_term_list(<<Sz:32,BinTerm:Sz/binary,T/binary>>, Acc) ->
  try binary_to_term(BinTerm) of
      Term -> coverage_bin_to_term_list(T, [Term|Acc])
  catch
    error:badarg -> coverage_bin_to_term_list(T, Acc)
  end;
coverage_bin_to_term_list(<<>>, Acc) -> lists:reverse(Acc).

coverage_bin_to_term_list_catch(<<Sz:32,BinTerm:Sz/binary,T/binary>>) ->
  case catch binary_to_term(BinTerm) of
    {'EXIT',_} -> coverage_bin_to_term_list_catch(T);
    Term -> [Term|coverage_bin_to_term_list_catch(T)]
  end;
coverage_bin_to_term_list_catch(<<>>) -> [].

coverage_bin_to_term_list_catch(<<Sz:32,BinTerm:Sz/binary,T/binary>>, Acc) ->
  case catch binary_to_term(BinTerm) of
    {'EXIT',_} -> coverage_bin_to_term_list_catch(T, Acc);
    Term -> coverage_bin_to_term_list_catch(T, [Term|Acc])
  end;
coverage_bin_to_term_list_catch(<<>>, Acc) -> lists:reverse(Acc).

coverage_per_key(<<BinSize:32,Bin/binary>> = B) ->
  true = (byte_size(B) =:= BinSize),
  Bin.

%%--------------------------------------------------------------------

multiple_uses() ->
  {344,62879,345,<<245,159,1,89>>} = multiple_uses_1(<<1,88,245,159,1,89>>),
  true = multiple_uses_2(<<0,0,197,18>>),
  <<42,43>> = multiple_uses_3(<<0,0,42,43>>, fun id/1),
  ok.

multiple_uses_1(<<X:16,Tail/binary>>) ->
  %% NOT OPTIMIZED: sub binary is matched or used in more than one place
  {Y,Z} = multiple_uses_match(Tail),
  {X,Y,Z,Tail}.

multiple_uses_2(<<_:16,Tail/binary>>) ->
  %% NOT OPTIMIZED: sub binary is matched or used in more than one place
  multiple_uses_cmp(Tail, Tail).

multiple_uses_3(<<_:16,Tail/binary>>, Fun) ->
  %% NOT OPTIMIZED: sub binary is used or returned
  Fun(Tail).

multiple_uses_match(<<Y:16,Z:16>>) -> {Y,Z}.

multiple_uses_cmp(<<Y:16>>, <<Y:16>>) -> true;
multiple_uses_cmp(<<_:16>>, <<_:16>>) -> false.

%%--------------------------------------------------------------------

zero_label() ->
  <<"nosemouth">> = read_pols(<<"FACE","nose","mouth">>),
  <<"CE">> = read_pols(<<"noFACE">>),
  ok.

read_pols(Data) ->
  <<PolygonType:4/binary,Rest/binary>> = Data,
  %% Intentional warning.
  _ = (PolygonType == <<"FACE">>) or (PolygonType == <<"PTCH">>),
  Rest.

%%--------------------------------------------------------------------

followed_by_catch() ->
  ok = handle(<<0,1,2,3,4,5>>).

-record(rec,{field}).
handle(<<>>) ->  ok;
handle(Msg) ->
  <<_DataLen:16, Rest/binary>> = Msg,
  case catch fooX:func() of
    [X] ->
      X#rec.field;
    _ ->
      ok
  end,
  handle(Rest).

%%--------------------------------------------------------------------

matching_meets_construction() ->
  Bin = id(<<"abc">>),
  Len = id(2),
  Tail0 = id(<<1,2,3,4,5>>),
  <<_:Len/binary,Tail/binary>> = Tail0,
  Res = <<Tail/binary,Bin/binary>>,
  <<3,4,5,"abc">> = Res,
  {'EXIT',{badarg,_}} = (catch matching_meets_construction_1(<<"Abc">>)),
  {'EXIT',{badarg,_}} = (catch matching_meets_construction_2(<<"Abc">>)),
  <<"Bbc">> = matching_meets_construction_3(<<"Abc">>),
  <<1,2>> = encode_octet_string(<<1,2,3>>, 2),
  ok.

matching_meets_construction_1(<<"A",H/binary>>) -> <<"B",H>>.

matching_meets_construction_2(<<"A",H/binary>>) -> <<"B",H/float>>.

matching_meets_construction_3(<<"A",H/binary>>) -> <<"B",H/binary>>.

encode_octet_string(<<OctetString/binary>>, Len) ->
  <<OctetString:Len/binary-unit:8>>.

%%--------------------------------------------------------------------

simon() ->
  one = simon(blurf, <<>>),
  two = simon(0, <<42>>),
  fc(simon, [17,<<1>>], catch simon(17, <<1>>)),
  fc(simon, [0,<<1,2,3>>], catch simon(0, <<1,2,3>>)),

  one = simon2(blurf, <<9>>),
  two = simon2(0, <<9,1>>),
  fc(simon2, [0,<<9,10,11>>], catch simon2(0, <<9,10,11>>)),
  ok.

simon(_, <<>>) -> one;
simon(0, <<_>>) -> two.

simon2(_, <<9>>) -> one;
simon2(0, <<_:16>>) -> two.

%%--------------------------------------------------------------------
%% OTP-7113: Crash in v3_codegen.

matching_and_andalso() ->
  ok = matching_and_andalso_1(<<1,2,3>>, 3),
  {'EXIT',{function_clause,_}} = (catch matching_and_andalso_1(<<1,2,3>>, -8)),
  {'EXIT',{function_clause,_}} = (catch matching_and_andalso_1(<<1,2,3>>, blurf)),
  {'EXIT',{function_clause,_}} = (catch matching_and_andalso_1(<<1,2,3>>, 19)),

  {"abc",<<"xyz">>} = matching_and_andalso_2("abc", <<"-xyz">>),
  {"abc",<<"">>} = matching_and_andalso_2("abc", <<($a-1)>>),
  {"abc",<<"">>} = matching_and_andalso_2("abc", <<($z+1)>>),
  {"abc",<<"">>} = matching_and_andalso_2("abc", <<($A-1)>>),
  {"abc",<<"">>} = matching_and_andalso_2("abc", <<($Z+1)>>),
  error = matching_and_andalso_2([], <<>>),
  error = matching_and_andalso_2([], <<$A>>),
  error = matching_and_andalso_2([], <<$Z>>),
  error = matching_and_andalso_2([], <<$a>>),
  error = matching_and_andalso_2([], <<$z>>),
  ok.

matching_and_andalso_1(<<Bitmap/binary>>, K)
  when is_integer(K) andalso size(Bitmap) >= K andalso 0 < K -> ok.

matching_and_andalso_2(Datetime, <<H,T/binary>>)
  when not ((H >= $a) andalso (H =< $z)) andalso
       not ((H >= $A) andalso (H =< $Z)) ->
  {Datetime,T};
matching_and_andalso_2(_, _) -> error.

%%--------------------------------------------------------------------
%% Thanks to Tomas Stejskal.

otp_7188() ->
  MP3 = <<84,65,71,68,117,154,105,232,107,121,0,0,0,0,0,0,0,0,0,0,
	  0,0,0,0,0,0,0,0,0,0,0,0,0,68,97,110,105,101,108,32,76,
	  97,110,100,97,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,66,
	  101,115,116,32,79,102,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	  0,0,0,0,0,0,50,48,48,48,50,48,48,48,32,45,32,66,101,115,
	  116,32,79,102,32,32,32,32,32,32,32,32,32,32,32,32,32,32,
	  32,32,12>>,
  {ok,{"ID3v1",
       [{title,<<68,117,154,105,232,107,121>>},
	{artist,<<"Daniel Landa">>},
	{album,<<"Best Of">>}]}} = parse_v1_or_v11_tag(MP3),
  ok.

parse_v1_or_v11_tag(<<"TAG", Title:30/binary,
		      Artist:30/binary, Album:30/binary,
		      _Year:4/binary, _Comment:28/binary,
		      0:8, Track:8, _Genre:8>>) ->
  {ok,
   {"ID3v1.1",
    [{track, Track}, {title, trim(Title)},
     {artist, trim(Artist)}, {album, trim(Album)}]}};
parse_v1_or_v11_tag(<<"TAG", Title:30/binary,
		      Artist:30/binary, Album:30/binary,
		      _Year:4/binary, _Comment:30/binary,
		      _Genre:8>>) ->
  {ok,
   {"ID3v1",
    [{title, trim(Title)},
     {artist, trim(Artist)},
     {album, trim(Album)}]}};
parse_v1_or_v11_tag(_) ->
    error.

trim(Bin) ->
  list_to_binary(trim_blanks(binary_to_list(Bin))).

trim_blanks(L) ->
  lists:reverse(skip_blanks_and_zero(lists:reverse(L))).

skip_blanks_and_zero([$\s|T]) ->
  skip_blanks_and_zero(T);
skip_blanks_and_zero([0|T]) ->
  skip_blanks_and_zero(T);
skip_blanks_and_zero(L) ->
  L.

%%--------------------------------------------------------------------
%% OTP-7233. Record and binary matching optimizations clashed.
%% Thanks to Vladimir Klebansky.

-record(rec_otp_7233, {key, val}).

otp_7233() ->
  otp_7233_1(#rec_otp_7233{key = <<"XXabcde">>,val=[{"xxxxxxxx",42}]}),
  [<<"XXabcde">>,{"xxxxxxxx",42}] = get(io_format),
  erase(io_format),
  otp_7233_1(#rec_otp_7233{key = <<"XXabcde">>,val=[]}),
  undefined = get(io_format),
  ok.

otp_7233_1(Rec) ->
  <<K:2/binary,_Rest:5/binary>> = Rec#rec_otp_7233.key,
  case K of
    <<"XX">> ->
      Value = Rec#rec_otp_7233.val,
      case lists:keysearch("xxxxxxxx", 1, Value) of
	{value,T} -> put(io_format, [Rec#rec_otp_7233.key,T]);
	false -> ok
      end;
    _ -> ok
  end.

%%--------------------------------------------------------------------

otp_7240() ->
  a = otp_7240_a(0, <<>>),
  b = otp_7240_a(1, 2),

  a = otp_7240_b(anything, <<>>),
  b = otp_7240_b(1, {x,y}),
  
  a = otp_7240_c(anything, <<>>),
  b = otp_7240_c(1, <<2>>),
  
  a = otp_7240_d(anything, <<>>),
  b = otp_7240_d(again, <<2>>),

  a = otp_7240_e(anything, <<>>),
  b = otp_7240_e(1, 41),

  a = otp_7240_f(anything, <<>>),
  b = otp_7240_f(1, {}),
    
  ok.

otp_7240_a(_, <<>>) -> a;
otp_7240_a(1, 2) -> b.

otp_7240_b(_, <<>>) -> a;
otp_7240_b(1, {_,_}) -> b.

otp_7240_c(_, <<>>) -> a;
otp_7240_c(1, <<2>>) -> b.

otp_7240_d(_, <<>>) -> a;
otp_7240_d(_, <<2>>) -> b.

otp_7240_e(_, <<>>) -> a;
otp_7240_e(1, B) when B < 42 -> b.

otp_7240_f(_, <<>>) -> a;
otp_7240_f(1, B) when is_tuple(B) -> b.

%%--------------------------------------------------------------------

otp_7498() ->
  <<1,2,3>> = otp_7498_foo(<<1,2,3>>, 0),
  <<2,3>> = otp_7498_foo(<<1,2,3>>, 1),
  <<1,2,3>> = otp_7498_foo(<<1,2,3>>, 2),

  <<1,2,3>> = otp_7498_bar(<<1,2,3>>, 0),
  <<2,3>> = otp_7498_bar(<<1,2,3>>, 1),
  <<1,2,3>> = otp_7498_bar(<<1,2,3>>, 2),
  <<>> = otp_7498_bar(<<>>, 2),
  <<1,2,3>> = otp_7498_bar(<<1,2,3>>, 3),
  ok.

otp_7498_foo(Bin, 0) ->
  otp_7498_foo(Bin, 42);
otp_7498_foo(<<_A, Rest/bitstring>>, 1) ->
  otp_7498_foo(Rest, 43);
otp_7498_foo(Bin, _I)  ->
  Bin.

otp_7498_bar(Bin, 0) ->
  otp_7498_bar(Bin, 42);
otp_7498_bar(<<_A, Rest/bitstring>>, 1) ->
  otp_7498_bar(Rest, 43);
otp_7498_bar(<<>>, 2) ->
  otp_7498_bar(<<>>, 44);
otp_7498_bar(Bin, _I)  ->
  Bin.

%%--------------------------------------------------------------------

match_string() ->
  %% To make sure that native endian really is handled correctly
  %% (i.e. that the compiler does not attempt to use bs_match_string/4
  %% instructions for native segments), running this test is not enough.
  %% Either examine the generated for do_match_string_native/1 or
  %% check the coverage for the v3_kernel module.
  case erlang:system_info(endian) of
    little ->
      do_match_string_native(<<$a,0,$b,0>>);
    big ->
      do_match_string_native(<<0,$a,0,$b>>)
  end,  
  do_match_string_big(<<0,$a,0,$b>>),
  do_match_string_little(<<$a,0,$b,0>>),

  do_match_string_big_signed(<<255,255>>),
  do_match_string_little_signed(<<255,255>>),

  plain = no_match_string_opt(<<"abc">>),
  strange = no_match_string_opt(<<$a:9,$b:9,$c:9>>),
  ok.

do_match_string_native(<<$a:16/native,$b:16/native>>) -> ok.

do_match_string_big(<<$a:16/big,$b:16/big>>) -> ok.

do_match_string_little(<<$a:16/little,$b:16/little>>) -> ok.

do_match_string_big_signed(<<(-1):16/signed>>) -> ok.

do_match_string_little_signed(<<(-1):16/little-signed>>) -> ok.

no_match_string_opt(<<"abc">>) -> plain;
no_match_string_opt(<<$a:9,$b:9,$c:9>>) -> strange.

%%--------------------------------------------------------------------
%% OTP-7591: A zero-width segment in matching would crash the compiler.

zero_width() ->
  <<Len:16/little, Str:Len/binary, 0:0>> = <<2, 0, $h, $i, 0:0>>,
  2 = Len,
  Str = <<"hi">>,
  %% Match sure that values that cannot fit in a segment will not match.
  case id(<<0:8>>) of
    <<256:8>> -> error;
    _ -> ok
  end.

%%--------------------------------------------------------------------
%% OTP_7650: A invalid size for binary segments could crash the compiler.

bad_size() ->
  Tuple = {a,b,c},
  {'EXIT',{{badmatch,<<>>},_}} = (catch <<32:Tuple>> = id(<<>>)),
  Binary = <<1,2,3>>,
  {'EXIT',{{badmatch,<<>>},_}} = (catch <<32:Binary>> = id(<<>>)),
  ok.

%%--------------------------------------------------------------------

haystack() ->
  <<0:10/unit:8>> = haystack_1(<<0:10/unit:8>>),
  [<<0:10/unit:8>>,
   <<0:20/unit:8>>] = haystack_2(<<1:8192>>),
  ok.

%% Used to crash the compiler.
haystack_1(Haystack) ->
  Subs = [10],
  [begin
     <<B:Y/binary>> = Haystack,
     B
   end || Y <- Subs],
  Haystack.

%% There would be an incorrect badmatch exception.
haystack_2(Haystack) ->
  Subs = [{687,10},{369,20}],
  [begin
     <<_:X/binary,B:Y/binary,_/binary>> = Haystack,
     B
   end || {X,Y} <- Subs].

fc({'EXIT',{function_clause,_}}) -> ok.

fc(Name, Args, {'EXIT',{function_clause,[{?MODULE,Name,Args,_}|_]}}) -> ok;
fc(Name, Args, {'EXIT',{function_clause,[{?MODULE,Name,Arity,_}|_]}})
  when length(Args) =:= Arity ->
  true = test_server:is_native(?MODULE).

%%--------------------------------------------------------------------
%% Cover the clause handling bs_context to binary in
%% beam_block:initialized_regs/2.
cover_beam_bool() ->
  ok = do_cover_beam_bool(<<>>, 3),
  <<19>> = do_cover_beam_bool(<<19>>, 2),
  <<42>> = do_cover_beam_bool(<<42>>, 1),
  <<17>> = do_cover_beam_bool(<<13,17>>, 0),
  ok.

do_cover_beam_bool(Bin, X) when X > 0 ->
  if
    X =:= 1; X =:= 2 ->
      Bin;
    true ->
      ok
  end;
do_cover_beam_bool(<<_,Bin/binary>>, X) ->
  do_cover_beam_bool(Bin, X+1).

%%--------------------------------------------------------------------

matched_out_size() ->
  {253,16#DEADBEEF} = mos_int(<<8,253,16#DEADBEEF:32>>),
  {6,16#BEEFDEAD} = mos_int(<<3,6:3,16#BEEFDEAD:32>>),
  {53,16#CAFEDEADBEEFCAFE} = mos_int(<<16,53:16,16#CAFEDEADBEEFCAFE:64>>),
  {23,16#CAFEDEADBEEFCAFE} = mos_int(<<5,23:5,16#CAFEDEADBEEFCAFE:64>>),

  {<<1,2,3>>,4} = mos_bin(<<3,1,2,3,4,3>>),
  {<<1,2,3,7>>,19,42} = mos_bin(<<4,1,2,3,7,19,4,42>>),
  <<1,2,3,7>> = mos_bin(<<4,1,2,3,7,"abcdefghij">>),
  ok.

mos_int(<<L,I:L,X:32>>) ->
  {I,X};
mos_int(<<L,I:L,X:64>>) ->
  {I,X}.

mos_bin(<<L,Bin:L/binary,X:8,L>>) ->
  L = byte_size(Bin),
  {Bin,X};
mos_bin(<<L,Bin:L/binary,X:8,L,Y:8>>) ->
  L = byte_size(Bin),
  {Bin,X,Y};
mos_bin(<<L,Bin:L/binary,"abcdefghij">>) ->
  L = byte_size(Bin),
  Bin.

%%--------------------------------------------------------------------

follow_fail_br() ->
  42 = ffb_1(<<0,1>>, <<0>>),
  8 = ffb_1(<<0,1>>, [a]),
  42 = ffb_2(<<0,1>>, <<0>>, 17),
  8 = ffb_2(<<0,1>>, [a], 0),
  ok.

ffb_1(<<_,T/bitstring>>, List) ->
  case List of
    <<_>> ->
      42;
    [_|_] ->
      %% The fail branch of the bs_start_match2 instruction pointing
      %% to here would be ignored, making the compiler incorrectly
      %% assume that the delayed sub-binary optimization was safe.
      bit_size(T)
  end.

ffb_2(<<_,T/bitstring>>, List, A) ->
  case List of
    <<_>> when A =:= 17 -> 42;
    [_|_] -> bit_size(T)
  end.

%%--------------------------------------------------------------------

no_partition() ->
  one = no_partition_1(<<"string">>, a1),
  {two,<<"string">>} = no_partition_1(<<"string">>, a2),
  {two,<<>>} = no_partition_1(<<>>, a2),
  {two,a} = no_partition_1(a, a2),
  three = no_partition_1(undefined, a3),
  {four,a,[]} = no_partition_1([a], a4),
  {five,a,b} = no_partition_1({a,b}, a5),

  one = no_partition_2(<<"string">>, a1),
  two = no_partition_2(<<"string">>, a2),
  two = no_partition_2(<<>>, a2),
  two = no_partition_2(a, a2),
  three = no_partition_2(undefined, a3),
  four = no_partition_2(42, a4),
  five = no_partition_2([], a5),
  six = no_partition_2(42.0, a6),
  ok.

no_partition_1(<<"string">>, a1) -> one;
no_partition_1(V, a2) -> {two,V};
no_partition_1(undefined, a3) -> three;
no_partition_1([H|T], a4) -> {four,H,T};
no_partition_1({A,B}, a5) -> {five,A,B}.

no_partition_2(<<"string">>, a1) -> one;
no_partition_2(_, a2) -> two;
no_partition_2(undefined, a3) -> three;
no_partition_2(42, a4) -> four;
no_partition_2([], a5) -> five;
no_partition_2(42.0, a6) -> six.

%%--------------------------------------------------------------------

calling_a_binary() ->
  [] = call_binary(<<>>, []),
  {'EXIT',{badarg,_}} = (catch call_binary(<<1>>, [])),
  {'EXIT',{badarg,_}} = (catch call_binary(<<1,2,3>>, [])),
  ok.

call_binary(<<>>, Acc) ->
  Acc;
call_binary(<<H,T/bits>>, Acc) ->
  T(<<Acc/binary,H>>).

%%--------------------------------------------------------------------

binary_in_map() ->
  ok = match_binary_in_map(#{key => <<42:8>>}),
  {'EXIT',{{badmatch,#{key := 1}},_}} =
    (catch match_binary_in_map(#{key => 1})),
  {'EXIT',{{badmatch,#{key := <<1023:16>>}},_}} =
    (catch match_binary_in_map(#{key => <<1023:16>>})),
  {'EXIT',{{badmatch,#{key := <<1:8>>}},_}} =
    (catch match_binary_in_map(#{key => <<1:8>>})),
  {'EXIT',{{badmatch,not_a_map},_}} =
    (catch match_binary_in_map(not_a_map)),
  ok.

match_binary_in_map(Map) ->
  case 8 of
    N ->
      #{key := <<42:N>>} = Map,
      ok
  end.

%%--------------------------------------------------------------------

match_string_opt() ->
  {x,<<1,2,3>>,{<<1>>,{v,<<1,2,3>>}}} = match_string_opt({<<1>>,{v,<<1,2,3>>}}),
  ok.

match_string_opt({<<1>>,{v,V}}=T) ->
  {x,V,T}.

%%--------------------------------------------------------------------
%% If 'bin_opt_info' was given the warning would lack filename and
%% line number.

map_and_binary() ->
  {<<"10">>,<<"37">>,<<"am">>} = do_map_and_binary(<<"10:37am">>),
  Map1 = #{time => "noon"},
  {ok,Map1} = do_map_and_binary(Map1),
  Map2 = #{hour => 8, min => 42},
  {8,42,Map2} = do_map_and_binary(Map2),
  ok.

do_map_and_binary(<<Hour:2/bytes, $:, Min:2/bytes, Rest/binary>>) ->
  {Hour, Min, Rest};
do_map_and_binary(#{time := _} = T) ->
  {ok, T};
do_map_and_binary(#{hour := Hour, min := Min} = T) ->
  {Hour, Min, T}.

%%--------------------------------------------------------------------
%% Unsafe caching of branch outcomes in beam_bsm would cause the
%% delayed creation of sub-binaries optimization to be applied even
%% when it was unsafe.

unsafe_branch_caching() ->
  <<>> = do_unsafe_branch_caching(<<42,1>>),
  <<>> = do_unsafe_branch_caching(<<42,2>>),
  <<>> = do_unsafe_branch_caching(<<42,3>>),
  <<17,18>> = do_unsafe_branch_caching(<<42,3,17,18>>),
  <<>> = do_unsafe_branch_caching(<<1,3,42,2>>),
  ok.

do_unsafe_branch_caching(<<Code/integer, Bin/binary>>) ->
  <<C1/integer, B1/binary>> = Bin,
  case C1 of
    X when X =:= 1 orelse X =:= 2 ->
      Bin2 = <<>>;
    _ ->
      Bin2 = B1
  end,
  case Code of
    1 -> do_unsafe_branch_caching(Bin2);
    _ -> Bin2
  end.

%%--------------------------------------------------------------------

check(F, R) ->
  R = F().

id(I) -> I.
