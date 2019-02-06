%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
%% Tests that basic cases of binary construction work
%%--------------------------------------------------------------------
-module(bs_construct).

-export([test/0]).

test() ->
  <<42>> = sz(8),
  <<42:8/little>> = sz_little(8),
  <<55>> = take_five(1, 3, 1, 7, 4),
  ok = bs5(),
  16#10000008 = bit_size(large_bin(1, 2, 3, 4)),
  ok = bad_ones(),
  ok = zero_width(),
  ok = not_used(),
  ok = bad_append(),
  ok = system_limit(),
  ok = bad_floats(),
  ok = huge_binaries(),
  ok.

%%--------------------------------------------------------------------
%% Taken from a bug report submitted by Dan Wallin (24 Oct 2003), the
%% following cases test construction of binaries whose segments have
%% sizes that are statically unknown.

sz(S) ->
  <<42:S>>.

sz_little(S) ->
  <<42:S/little>>.

take_five(A, Head, FB, C, Tail) ->
  <<A:Head, FB:1, C:Tail>>.

%%--------------------------------------------------------------------

bs5() ->
  Const = mk_constant(),
  Pairs = mk_pairs(),
  true = are_same(Const, Pairs),
  true = lists:all(fun ({B, L}) -> binary_to_list(B) =:= L end, Pairs),
  ok.

are_same(C, L) ->
  C =:= L.

mk_constant() ->
  [{<<213>>,[213]},
   {<<56>>,[56]},
   {<<1,2>>,[1,2]},
   {<<71>>,[71]},
   {<<8,1>>,[8,1]},
   {<<3,9>>,[3,9]},
   {<<9,3>>,[9,3]},
   {<<0,0,0,0>>,[0,0,0,0]},
   {<<62,0,0,0>>,[62,0,0,0]},
   {<<0,0,0,62>>,[0,0,0,62]},
   {<<138,99,0,147>>,[138,99,0,147]},
   {<<138,99,0,148>>,[138,99,0,148]},
   {<<147,0,99,138>>,[147,0,99,138]},
   {<<255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255>>,
    [255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255]},
   {<<13>>,[13]},
   {<<0,4,0,5>>,[0,4,0,5]},
   {<<129>>,[129]},
   {<<129>>,[129]},
   {<<1,2>>,[1,2]},
   {<<1>>,[1]},
   {<<4,3,1>>,[4,3,1]},
   {<<47>>,[47]},
   {<<>>,[]},
   {<<97,112,97>>,[97,112,97]},
   {<<46,110,142,77,45,204,233>>,[46,110,142,77,45,204,233]},
   {<<>>,[]}].

mk_pairs() ->
  L4 = [138,99,0,147],
  [{<<-43>>,[256-43]},
   {<<56>>,[56]},
   {<<1,2>>,[1,2]},
   {<<4:4,7:4>>,[4*16+7]},
   {<<1:5,1:11>>,[1*8,1]},
   {<<777:16/big>>,[3,9]},
   {<<777:16/little>>,[9,3]},
   {<<0.0:32/float>>,[0,0,0,0]},
   {<<0.125:32/float>>,[62,0,0,0]},
   {<<0.125:32/little-float>>,[0,0,0,62]},
   {<<57285702734876389752897683:32>>,L4},
   {<<57285702734876389752897684:32>>,[138,99,0,148]},
   {<<57285702734876389752897683:32/little>>,lists:reverse(L4)},
   {<<-1:17/unit:8>>,lists:duplicate(17,255)},
   {<<13>>,[13]},
   {<<4:8/unit:2,5:2/unit:8>>,[0,4,0,5]},
   {<<1:1,0:6,1:1>>,[129]},
   {<<1:1/little,0:6/little,1:1/little>>,[129]},
   {<<<<1,2>>/binary>>,[1,2]},
   {<<<<1,2>>:1/binary>>,[1]},
   {<<4,3,<<1,2>>:1/binary>>,[4,3,1]},
   {<<(256*45+47)>>,[47]},
   {<<57:0>>,[]},
   {<<"apa">>,"apa"},
   {<<1:3,"string",9:5>>,[46,110,142,77,45,204,233]},
   {<<>>,[]}].

%%--------------------------------------------------------------------
%% Constructs a big enough binary to have a bit size that needs a
%% bignum on 32-bit architectures

large_bin(X1, X2, X3, X4) ->
  Sz = 16#4000000,
  <<1, <<X1:Sz, X2:Sz, X3:Sz, X4:Sz>>/bits>>.

%%--------------------------------------------------------------------
%% Test construction of "bad" binaries

-define(FAIL(Expr), {'EXIT', {badarg, _}} = (catch Expr)).

bad_ones() ->
  PI = math:pi(),
  ?FAIL(<<PI>>),
  Bin12 = <<1,2>>,
  ?FAIL(<<Bin12>>),
  E = 2.71,
  ?FAIL(<<E/binary>>),
  Int = 24334,
  ?FAIL(<<Int/binary>>),
  BigInt = 24334344294788947129487129487219847,
  ?FAIL(<<BigInt/binary>>),
  Bin123 = <<1,2,3>>,
  ?FAIL(<<Bin123/float>>),
  ok.

%%--------------------------------------------------------------------
%% Taken from the emulator bs_construct_SUITE - seg faulted till 18.1

zero_width() ->
  Z = id(0),
  Small = id(42),
  Big = id(1 bsl 128),  % puts stuff on the heap
  <<>> = <<Small:Z>>,
  <<>> = <<Small:0>>,
  <<>> = <<Big:Z>>,
  <<>> = <<Big:0>>,
  ok.

id(X) -> X.

%%--------------------------------------------------------------------
%% Taken from bs_construct_SUITE. The test checks that constructed
%% binaries that are not used would still give a `badarg' exception.
%% Problem was that in native code one of them gave `badarith'.

not_used() ->
    ok = not_used1(3, <<"dum">>),
    {'EXIT',{badarg,_}} = (catch not_used1(42, "dum_string")),
    {'EXIT',{badarg,_}} = (catch not_used2(666, -2)),
    {'EXIT',{badarg,_}} = (catch not_used2(666, "bad_size")), % this one
    {'EXIT',{badarg,_}} = (catch not_used3(666)),
    ok.

not_used1(I, BinString) ->
    <<I:32,BinString/binary>>,
    ok.

not_used2(I, Sz) ->
    <<I:Sz>>,
    ok.

not_used3(I) ->
    <<I:(-8)>>,
    ok.

%%--------------------------------------------------------------------
%% Taken from bs_construct_SUITE.

bad_append() ->
    do_bad_append(<<127:1>>, fun append_unit_3/1),
    do_bad_append(<<127:2>>, fun append_unit_3/1),
    do_bad_append(<<127:17>>, fun append_unit_3/1),

    do_bad_append(<<127:3>>, fun append_unit_4/1),
    do_bad_append(<<127:5>>, fun append_unit_4/1),
    do_bad_append(<<127:7>>, fun append_unit_4/1),
    do_bad_append(<<127:199>>, fun append_unit_4/1),

    do_bad_append(<<127:7>>, fun append_unit_8/1),
    do_bad_append(<<127:9>>, fun append_unit_8/1),

    do_bad_append(<<0:8>>, fun append_unit_16/1),
    do_bad_append(<<0:15>>, fun append_unit_16/1),
    do_bad_append(<<0:17>>, fun append_unit_16/1),
    ok.

do_bad_append(Bin0, Appender) ->
    {'EXIT',{badarg,_}} = (catch Appender(Bin0)),

    Bin1 = id(<<0:3,Bin0/bitstring>>),
    <<_:3,Bin2/bitstring>> = Bin1,
    {'EXIT',{badarg,_}} = (catch Appender(Bin2)),

    %% Create a writable binary.
    Empty = id(<<>>),
    Bin3 = <<Empty/bitstring,Bin0/bitstring>>,
    {'EXIT',{badarg,_}} = (catch Appender(Bin3)),
    ok.

append_unit_3(Bin) ->
    <<Bin/binary-unit:3,0:1>>.

append_unit_4(Bin) ->
    <<Bin/binary-unit:4,0:1>>.

append_unit_8(Bin) ->
    <<Bin/binary,0:1>>.

append_unit_16(Bin) ->
    <<Bin/binary-unit:16,0:1>>.

%%--------------------------------------------------------------------
%% Taken from bs_construct_SUITE.

system_limit() ->
    WordSize = erlang:system_info(wordsize),
    BitsPerWord = WordSize * 8,
    {'EXIT',{system_limit,_}} =
	(catch <<0:(id(0)),42:(id(1 bsl BitsPerWord))>>),
    {'EXIT',{system_limit,_}} =
	(catch <<42:(id(1 bsl BitsPerWord)),0:(id(0))>>),
    {'EXIT',{system_limit,_}} =
	(catch <<(id(<<>>))/binary,0:(id(1 bsl 100))>>),

    %% Would fail to load.
    {'EXIT',{system_limit,_}} = (catch <<0:(1 bsl 67)>>),
    {'EXIT',{system_limit,_}} = (catch <<0:((1 bsl 64)+1)>>),
    case WordSize of
	4 ->
	    system_limit_32();
	8 ->
	    ok
    end.

system_limit_32() ->
    {'EXIT',{badarg,_}} = (catch <<42:(-1)>>),
    {'EXIT',{badarg,_}} = (catch <<42:(id(-1))>>),
    {'EXIT',{badarg,_}} = (catch <<42:(id(-389739873536870912))/unit:8>>),
    {'EXIT',{system_limit,_}} = (catch <<42:536870912/unit:8>>),
    {'EXIT',{system_limit,_}} = (catch <<42:(id(536870912))/unit:8>>),
    {'EXIT',{system_limit,_}} = (catch <<0:(id(8)),42:536870912/unit:8>>),
    {'EXIT',{system_limit,_}} =	(catch <<0:(id(8)),42:(id(536870912))/unit:8>>),

    %% The size would be silently truncated, resulting in a crash.
    {'EXIT',{system_limit,_}} = (catch <<0:(1 bsl 35)>>),
    {'EXIT',{system_limit,_}} = (catch <<0:((1 bsl 32)+1)>>),

    %% Would fail to load.
    {'EXIT',{system_limit,_}} = (catch <<0:(1 bsl 43)>>),
    {'EXIT',{system_limit,_}} = (catch <<0:((1 bsl 40)+1)>>),
    ok.

%%--------------------------------------------------------------------

bad_floats() ->
  WordSize = erlang:system_info(wordsize),
  BitsPerWord = WordSize * 8,
  {'EXIT',{badarg,_}} = (catch <<3.14:(id(33))/float>>),
  {'EXIT',{badarg,_}} = (catch <<3.14:(id(64 bor 32))/float>>),
  {'EXIT',{badarg,_}} = (catch <<3.14:(id((1 bsl 28) bor 32))/float>>),
  {'EXIT',{system_limit,_}} = (catch <<3.14:(id(1 bsl BitsPerWord))/float>>),
  ok.

%%--------------------------------------------------------------------
%% A bug in the implementation of binaries compared sizes in bits with sizes in
%% bytes, causing <<0:(id((1 bsl 31)-1))>> to fail to construct with
%% 'system_limit'.
%% <<0:(id((1 bsl 32)-1))>> was succeeding because the comparison was
%% (incorrectly) signed.

huge_binaries() ->
  case erlang:system_info(wordsize) of
    4 ->
      Old = erts_debug:set_internal_state(available_internal_state, true),
      case erts_debug:set_internal_state(binary, (1 bsl 29)-1) of
        false ->
          io:format("\nNot enough memory to create 512Mb binary\n",[]);
        Bin->
          huge_binaries_32(Bin)
      end,
      erts_debug:set_internal_state(available_internal_state, Old);

    8 -> ok
  end,
  garbage_collect(),
  id(<<0:(id((1 bsl 31)-1))>>),
  garbage_collect(),
  id(<<0:(id((1 bsl 30)-1))>>),
  garbage_collect(),
  ok.

huge_binaries_32(AlmostIllegal) ->
  %% Attempt construction of too large binary using bs_init/1 (which takes the
  %% number of bytes as an argument, which should be compared to the maximum
  %% size in bytes).
  {'EXIT',{system_limit,_}} = (catch <<0:32,AlmostIllegal/binary>>),
  %% Attempt construction of too large binary using bs_init/1 with a size in
  %% bytes that has the msb set (and would be negative if it was signed).
  {'EXIT',{system_limit,_}} =
    (catch <<0:8, AlmostIllegal/binary, AlmostIllegal/binary,
	     AlmostIllegal/binary, AlmostIllegal/binary,
	     AlmostIllegal/binary, AlmostIllegal/binary,
	     AlmostIllegal/binary, AlmostIllegal/binary>>),
  ok.
