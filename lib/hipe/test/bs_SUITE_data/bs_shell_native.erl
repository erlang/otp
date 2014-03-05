%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : bs_shell_native.erl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Purpose : Tests that the Erlang shell works well when in native
%%% Created : 6 Sep 2006
%%%-------------------------------------------------------------------
-module(bs_shell_native).

-export([prepare_for_test/0, test/0]).
%% These need to be exported so that we emulate calling them from the shell
-export([parse_and_eval/1, receiver/1, receiver_alot/1, send_alot/3]).

%% This makes sure the shell runs native code
prepare_for_test() ->
  lists:foreach(fun (M) -> {ok, M} = hipe:c(M) end, [erl_bits, erl_eval]).

test() ->
  ok = eval_bits_in_shell(),
  ok = eval_bin_comp_in_shell(),
  ok.

%%--------------------------------------------------------------------
%% Tests for bit stream operations including matching, construction
%% and binary_to_list, list_to_binary in the shell
eval_bits_in_shell() ->
  <<1:100>> = parse_and_eval("<<1:100>> = <<1:100>>."),
  ok = match(7),
  ok = match(9),
  ok = match1(15),
  ok = match1(31),
  ok = horrid_match(),
  ok = test_bitstr(),
  ok = test_bitsize(),
  ok = asymmetric_tests(),
  ok = big_asymmetric_tests(),
  ok = binary_to_and_from_list(),
  ok = big_binary_to_and_from_list(),
  ok = send_and_receive(),
  ok = send_and_receive_alot(),
  ok.

parse_and_eval(String) ->
  {ok, Toks, _} = erl_scan:string(String),
  {ok, Exprs} = erl_parse:parse_exprs(Toks),
  Bnds = erl_eval:new_bindings(),
  case erl_eval:exprs(Exprs, Bnds) of
    {value, V, _} ->
      V;
    V ->
      V
  end.

match(N) ->
  Str = "N =" ++ integer_to_list(N) ++ ", <<0:N>> = <<0:N>>.",
  <<0:N>> = parse_and_eval(Str),
  ok.

match1(N) ->
  Str = "N =" ++ integer_to_list(N) ++ ", <<42:N/little>> = <<42:N/little>>.",
  <<42:N/little>> = parse_and_eval(Str),
  ok.

test_bitsize() ->
  101 = parse_and_eval("101 = erlang:bit_size(<<1:101>>)."),
  1001 = parse_and_eval("1001 = erlang:bit_size(<<1:1001>>)."),
  80 = parse_and_eval("80 = erlang:bit_size(<<1:80>>)."),
  800 = parse_and_eval("800 = erlang:bit_size(<<1:800>>)."),
  S =
    "Bin = <<0:16#1000000>>,"
    "BigBin = list_to_bitstring([Bin||_ <- lists:seq(1,16#10)] ++ [<<1:1>>]),"
    "16#10000001 = erlang:bit_size(BigBin).",
  16#10000001 = parse_and_eval(S),
  %% Only run these on computers with lots of memory
  %% HugeBin = list_to_bitstring([BigBin||_ <- lists:seq(1,16#10)]++[<<1:1>>]),
  %% 16#100000011 = erlang:bit_size(HugeBin),
  0 = parse_and_eval("0 = erlang:bit_size(<<>>)."),
  ok.

horrid_match() ->
  S = "<<1:4,B:24/bitstring>> = <<1:4,42:24/little>>, <<42:24/little>> = B.",
  <<42:24/little>> = parse_and_eval(S),
  ok.

test_bitstr() ->
  S =
    "<<1:7,B/bitstring>> = <<1:7,<<1:1,6>>/bitstring>>,"
    "<<1:1,6>> = B,"
    "B = <<1:1,6>>.",
  <<1:1,6>> = parse_and_eval(S),
  ok.

asymmetric_tests() ->
  <<1:12>> = parse_and_eval("<<1:12>> = <<0,1:4>>."),
  <<0,1:4>> = parse_and_eval("<<0,1:4>> = <<1:12>>."),
  S1 =
    "<<1:1,X/bitstring>> = <<128,255,0,0:2>>,"
    "<<1,254,0,0:1>> = X,"
    "X = <<1,254,0,0:1>>.",
  <<1,254,0,0:1>> = parse_and_eval(S1),
  S2 =
    "<<1:1,X1:25/bitstring>> = <<128,255,0,0:2>>,"
    "<<1,254,0,0:1>> = X1,"
    "X1 = <<1,254,0,0:1>>.",
  <<1,254,0,0:1>> = parse_and_eval(S2),
  ok.

big_asymmetric_tests() ->
  <<1:875,1:12>> = parse_and_eval("<<1:875,1:12>> = <<1:875,0,1:4>>."),
  <<1:875,0,1:4>> = parse_and_eval("<<1:875,0,1:4>> = <<1:875,1:12>>."),
  S1 =
    "<<1:1,X/bitstring>> = <<128,255,0,0:2,1:875>>,"
    "<<1,254,0,0:1,1:875>> = X,"
    "X = <<1,254,0,0:1,1:875>>.",
  <<1,254,0,0:1,1:875>> = parse_and_eval(S1),
  S2 =
    "<<1:1,X1:900/bitstring>> = <<128,255,0,0:2,1:875>>,"
    "<<1,254,0,0:1,1:875>> = X1,"
    "X1 = <<1,254,0,0:1,1:875>>.",
  parse_and_eval(S2),
  ok.

binary_to_and_from_list() ->
  <<1:7>> = parse_and_eval("list_to_bitstring(bitstring_to_list(<<1:7>>))."),
  <<1,2,3,4,1:1>> = parse_and_eval("list_to_bitstring(bitstring_to_list(<<1,2,3,4,1:1>>))."),
  [1,2,3,4,<<1:1>>] = parse_and_eval("bitstring_to_list(<<1,2,3,4,1:1>>)."),
  <<1:1,1,2,3,4>> = parse_and_eval("list_to_bitstring([<<1:1>>,1,2,3,4])."),
  [128,129,1,130,<<0:1>>] = parse_and_eval("bitstring_to_list(<<1:1,1,2,3,4>>)."),
  ok.

big_binary_to_and_from_list() ->
  S1 = "erlang:list_to_bitstring(bitstring_to_list(<<1:800,2,3,4,1:1>>)).",
  <<1:800,2,3,4,1:1>> = parse_and_eval(S1),
  S2 = "erlang:bitstring_to_list(<<1,2,3,4,1:800,1:1>>).",
  [1,2,3,4|_Rest1] = parse_and_eval(S2),
  S3 = "erlang:list_to_bitstring([<<1:801>>,1,2,3,4]).",
  <<1:801,1,2,3,4>> = parse_and_eval(S3),
  ok.

send_and_receive() ->
  S =
    "Bin = <<1,2:7>>,"
    "Pid = spawn(fun() -> bs_shell_native:receiver(Bin) end),"
    "Pid ! {self(),<<1:7,8:5,Bin/bitstring>>},"
    "receive ok -> ok end.",
  parse_and_eval(S).

receiver(Bin) ->
  receive
    {Pid, <<1:7,8:5,Bin/bitstring>>} ->
      Pid ! ok
  end.

send_and_receive_alot() ->
  S =
    "Bin = <<1:1000001>>,"
    "Pid = spawn(fun() -> bs_shell_native:receiver_alot(Bin) end),"
    "bs_shell_native:send_alot(100,Bin,Pid).",
  parse_and_eval(S).

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
    {Pid, <<1:7,8:5,Bin/bitstring>>} ->
      Pid ! ok;
    no_more -> ok
  end,
  receiver_alot(Bin).

%%--------------------------------------------------------------------

eval_bin_comp_in_shell() ->
  ok = byte_aligned(),
  ok = bit_aligned(),
  ok = extended_byte_aligned(),
  ok = extended_bit_aligned(),
  ok = mixed(),
  ok.

byte_aligned() ->
  <<"abcdefg">> =
    parse_and_eval("<<\"abcdefg\">> = << <<(X+32)>> || <<X>> <= <<\"ABCDEFG\">> >>."),
  <<1:32/little,2:32/little,3:32/little,4:32/little>> =
    parse_and_eval("<<1:32/little,2:32/little,3:32/little,4:32/little>> =
    << <<X:32/little>> || <<X:32>> <= <<1:32,2:32,3:32,4:32>> >>."),
  <<1:32/little,2:32/little,3:32/little,4:32/little>> =
    parse_and_eval("<<1:32/little,2:32/little,3:32/little,4:32/little>> =
    << <<X:32/little>> || <<X:16>> <= <<1:16,2:16,3:16,4:16>> >>."),
  ok.

bit_aligned() ->
  <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> =
    parse_and_eval("<<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> =
    << <<(X+32):7>> || <<X>> <= <<\"ABCDEFG\">> >>."),
  <<"ABCDEFG">> =
    parse_and_eval("<<\"ABCDEFG\">> =
    << <<(X-32)>> || <<X:7>> <= <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> >>."),
 <<1:31/little,2:31/little,3:31/little,4:31/little>> =
    parse_and_eval("<<1:31/little,2:31/little,3:31/little,4:31/little>> =
    << <<X:31/little>> || <<X:31>> <= <<1:31,2:31,3:31,4:31>> >>."),
  <<1:31/little,2:31/little,3:31/little,4:31/little>> =
    parse_and_eval("<<1:31/little,2:31/little,3:31/little,4:31/little>> =
    << <<X:31/little>> || <<X:15>> <= <<1:15,2:15,3:15,4:15>> >>."),
  ok.

extended_byte_aligned() ->
  <<"abcdefg">> =
    parse_and_eval("<<\"abcdefg\">> = << <<(X+32)>> || X <- \"ABCDEFG\" >>."),
  "abcdefg" =
    parse_and_eval("\"abcdefg\" = [(X+32) || <<X>> <= <<\"ABCDEFG\">>]."),
  <<1:32/little,2:32/little,3:32/little,4:32/little>> =
    parse_and_eval("<<1:32/little,2:32/little,3:32/little,4:32/little>> =
    << <<X:32/little>> || X <- [1,2,3,4] >>."),
  [256,512,768,1024] =
    parse_and_eval("[256,512,768,1024] =
    [X || <<X:16/little>> <= <<1:16,2:16,3:16,4:16>>]."),
  ok.

extended_bit_aligned() ->
  <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> =
    parse_and_eval("<<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> =
    << <<(X+32):7>> || X <- \"ABCDEFG\" >>."),
  "ABCDEFG" =
    parse_and_eval("\"ABCDEFG\" = [(X-32) || <<X:7>> <=
<<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>>]."),
  <<1:31/little,2:31/little,3:31/little,4:31/little>> =
    parse_and_eval("<<1:31/little,2:31/little,3:31/little,4:31/little>> =
    << <<X:31/little>> || X <- [1,2,3,4] >>."),
  [256,512,768,1024] =
    parse_and_eval("[256,512,768,1024] =
    [X || <<X:15/little>> <= <<1:15,2:15,3:15,4:15>>]."),
  ok.

mixed() ->
  <<2,3,3,4,4,5,5,6>> =
    parse_and_eval("<<2,3,3,4,4,5,5,6>> =
    << <<(X+Y)>> || <<X>> <= <<1,2,3,4>>, <<Y>> <= <<1,2>> >>."),
  <<2,3,3,4,4,5,5,6>> =
  parse_and_eval("<<2,3,3,4,4,5,5,6>> =
    << <<(X+Y)>> || <<X>> <= <<1,2,3,4>>, Y <- [1,2] >>."),
  <<2,3,3,4,4,5,5,6>> =
  parse_and_eval("<<2,3,3,4,4,5,5,6>> =
    << <<(X+Y)>> || X <- [1,2,3,4], Y <- [1,2] >>."),
  [2,3,3,4,4,5,5,6] =
  parse_and_eval("[2,3,3,4,4,5,5,6] =
    [(X+Y) || <<X>> <= <<1,2,3,4>>, <<Y>> <= <<1,2>>]."),
  [2,3,3,4,4,5,5,6] =
  parse_and_eval("[2,3,3,4,4,5,5,6] =
    [(X+Y) || <<X>> <= <<1,2,3,4>>, Y <- [1,2]]."),
  <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =
  parse_and_eval("<<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =
    << <<(X+Y):3>> || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, <<Y:3>> <= <<1:3,2:3>> >>."),
  <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =
  parse_and_eval("<<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =
    << <<(X+Y):3>> || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, Y <- [1,2] >>."),
  <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =
  parse_and_eval("<<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =
    << <<(X+Y):3>> || X <- [1,2,3,4], Y <- [1,2] >>."),
  [2,3,3,4,4,5,5,6] =
  parse_and_eval("[2,3,3,4,4,5,5,6] =
    [(X+Y) || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, <<Y:3>> <= <<1:3,2:3>>]."),
  [2,3,3,4,4,5,5,6] =
  parse_and_eval("[2,3,3,4,4,5,5,6] =
    [(X+Y) || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, Y <- [1,2]]."),
  ok.
