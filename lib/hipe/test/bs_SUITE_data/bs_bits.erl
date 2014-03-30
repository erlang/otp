%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : bs_bits.erl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Purpose : Tests for bit stream operations including matching,
%%%           construction, binary_to_list and list_to_binary
%%% Created : 6 Sep 2006
%%%-------------------------------------------------------------------
-module(bs_bits).

-export([test/0]).

test() ->
  <<1:100>> = <<1:100>>,
  ok = match(7),
  ok = match(9),
  ok = match1(15),
  ok = match1(31),
  ok = horrid_match(),
  ok = test_bitstr(),
  ok = test_is_bitstr(<<1:1>>,<<8>>),
  ok = test_is_binary(<<1:1>>,<<8>>),
  ok = test_bitsize(),
  ok = asymmetric_tests(),
  ok = big_asymmetric_tests(),
  ok = bitstr_to_and_from_list(),
  ok = big_bitstr_to_and_from_list(),
  ok = send_and_receive(),
  ok = send_and_receive_alot(),
  ok.

match(N) ->
  <<0:N>> = <<0:N>>,
  ok.

match1(N) ->
  <<42:N/little>> = <<42:N/little>>,
  ok.

test_is_bitstr(Bitstr, Binary) ->
  true = is_bitstring(Bitstr),
  true = is_bitstring(Binary),
  ok = if is_bitstring(Bitstr) -> ok end,
  ok = if is_bitstring(Binary) -> ok end.

test_is_binary(Bitstr, Binary) ->
  false = is_binary(Bitstr),
  true = is_binary(Binary),
  ok = if is_binary(Bitstr) -> not_ok; true -> ok end,
  ok = if is_binary(Binary) -> ok end.

test_bitsize() ->
  101 = erlang:bit_size(<<1:101>>),
  1001 = erlang:bit_size(<<1:1001>>),
  80 = erlang:bit_size(<<1:80>>),
  800 = erlang:bit_size(<<1:800>>),
  Bin = <<0:16#1000000>>,
  BigBin = list_to_bitstring([Bin||_ <- lists:seq(1,16#10)]++[<<1:1>>]),
  16#10000001 = bit_size(BigBin),
  %% Only run these on computers with lots of memory
  %% HugeBin = list_to_bitstring([BigBin||_ <- lists:seq(1,16#10)]++[<<1:1>>]),
  %% 16#100000011 = bit_size(HugeBin),
  0 = erlang:bit_size(<<>>),
  ok.

horrid_match() ->
  <<1:4,B:24/bitstring>> = <<1:4,42:24/little>>,
  <<42:24/little>> = B,
  ok.

test_bitstr() ->
  <<1:7,B/bitstring>> = <<1:7,<<1:1,6>>/bitstring>>,
  <<1:1,6>> = B,
  B = <<1:1,6>>,
  ok.

asymmetric_tests() ->
  <<1:12>> = <<0,1:4>>,
  <<0,1:4>> = <<1:12>>,
  <<1:1,X/bitstring>> = <<128,255,0,0:2>>,
  <<1,254,0,0:1>> = X,
  X = <<1,254,0,0:1>>,
  <<1:1,X1:25/bitstring>> = <<128,255,0,0:2>>,
  <<1,254,0,0:1>> = X1,
  X1 = <<1,254,0,0:1>>,
  ok.

big_asymmetric_tests() ->
  <<1:875,1:12>> = <<1:875,0,1:4>>,
  <<1:875,0,1:4>> = <<1:875,1:12>>,
  <<1:1,X/bitstring>> = <<128,255,0,0:2,1:875>>,
  <<1,254,0,0:1,1:875>> = X,
  X = <<1,254,0,0:1,1:875>>,
  <<1:1,X1:900/bitstring>> = <<128,255,0,0:2,1:875>>,
  <<1,254,0,0:1,1:875>> = X1,
  X1 = <<1,254,0,0:1,1:875>>,
  ok.

bitstr_to_and_from_list() ->
  <<1:7>> = list_to_bitstring(bitstring_to_list(<<1:7>>)),
  <<1,2,3,4,1:1>> = list_to_bitstring(bitstring_to_list(<<1,2,3,4,1:1>>)),
  [1,2,3,4,<<1:1>>] = bitstring_to_list(<<1,2,3,4,1:1>>),
  <<1:1,1,2,3,4>> = list_to_bitstring([<<1:1>>,1,2,3,4]),
  [128,129,1,130,<<0:1>>] = bitstring_to_list(<<1:1,1,2,3,4>>),
  ok.

big_bitstr_to_and_from_list() ->
  <<1:800,2,3,4,1:1>> = list_to_bitstring(bitstring_to_list(<<1:800,2,3,4,1:1>>)),
  [1,2,3,4|_Rest1] = bitstring_to_list(<<1,2,3,4,1:800,1:1>>),
  <<1:801,1,2,3,4>> = list_to_bitstring([<<1:801>>,1,2,3,4]),
  ok.

send_and_receive() ->
  Bin = <<1,2:7>>,
  Pid = spawn(fun() -> receiver(Bin) end),
  Pid ! {self(),<<1:7,8:5,Bin/bitstring>>},
  receive
    ok ->
      ok
  end.

receiver(Bin) ->
  receive
    {Pid,<<1:7,8:5,Bin/bitstring>>} ->
      Pid ! ok
  end.

send_and_receive_alot() ->
  Bin = <<1:1000001>>,
  Pid = spawn(fun() -> receiver_alot(Bin) end),
  send_alot(100,Bin,Pid).

send_alot(N,Bin,Pid) when N > 0 ->
  Pid ! {self(),<<1:7,8:5,Bin/bitstring>>},
  receive
    ok ->
      ok
  end,
  send_alot(N-1,Bin,Pid);
send_alot(0,_Bin,Pid) ->
  Pid ! no_more,
  ok.

receiver_alot(Bin) ->
  receive
    {Pid,<<1:7,8:5,Bin/bitstring>>} ->
      Pid ! ok;
    no_more -> ok
  end,
  receiver_alot(Bin).
