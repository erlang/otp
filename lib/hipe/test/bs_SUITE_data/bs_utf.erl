%% -*- erlang-indent-level: 2 -*-
%%-------------------------------------------------------------------
%% Purpose: test support for UTF datatypes in binaries
%%
%% Most of it taken from emulator/test/bs_utf_SUITE.erl
%%-------------------------------------------------------------------

-module(bs_utf).

-export([test/0]).

-include_lib("common_test/include/ct.hrl").

test() ->
  ok = utf8_cm65(),
  ok = utf8_roundtrip(),
  ok = utf16_roundtrip(),
  ok = utf32_roundtrip(),
  %% The following were problematic for the LLVM backend
  ok = utf8_illegal_sequences(),
  ok = utf16_illegal_sequences(),
  ok = utf32_illegal_sequences(),
  ok.

%%-------------------------------------------------------------------
%% A test with construction and matching

utf8_cm65() ->
  <<65>> = b65utf8(),
  ok = m(<<65>>).

b65utf8() ->
  <<65/utf8>>.

m(<<65/utf8>>) ->
  ok.

%%-------------------------------------------------------------------

utf8_roundtrip() ->
  ok = utf8_roundtrip(0, 16#D7FF),
  ok = utf8_roundtrip(16#E000, 16#10FFFF),
  ok.

utf8_roundtrip(First, Last) when First =< Last ->
  Bin = int_to_utf8(First),
  Bin = id(<<First/utf8>>),
  Bin = id(<<(id(<<>>))/binary,First/utf8>>),
  Unaligned = id(<<3:2,First/utf8>>),
  <<_:2,Bin/binary>> = Unaligned,
  <<First/utf8>> = Bin,
  <<First/utf8>> = make_unaligned(Bin),
  utf8_roundtrip(First+1, Last);
utf8_roundtrip(_, _) ->
  ok.

%%-------------------------------------------------------------------

utf16_roundtrip() ->
  Big = fun utf16_big_roundtrip/1,
  Little = fun utf16_little_roundtrip/1,
  PidRefs = [spawn_monitor(fun() -> do_utf16_roundtrip(Fun) end) ||
	      Fun <- [Big,Little]],
  [receive {'DOWN', Ref, process, Pid, Reason} -> normal=Reason end ||
    {Pid, Ref} <- PidRefs],
  ok.

do_utf16_roundtrip(Fun) ->
  do_utf16_roundtrip(0, 16#D7FF, Fun),
  do_utf16_roundtrip(16#E000, 16#10FFFF, Fun).

do_utf16_roundtrip(First, Last, Fun) when First =< Last ->
  Fun(First),
  do_utf16_roundtrip(First+1, Last, Fun);
do_utf16_roundtrip(_, _, _) -> ok.

utf16_big_roundtrip(Char) ->
  Bin = id(<<Char/utf16>>),
  Bin = id(<<(id(<<>>))/binary,Char/utf16>>),
  Unaligned = id(<<3:2,Char/utf16>>),
  <<_:2,Bin/binary>> = Unaligned,
  <<Char/utf16>> = Bin,
  <<Char/utf16>> = make_unaligned(Bin),
  ok.

utf16_little_roundtrip(Char) ->
  Bin = id(<<Char/little-utf16>>),
  Bin = id(<<(id(<<>>))/binary,Char/little-utf16>>),
  Unaligned = id(<<3:2,Char/little-utf16>>),
  <<_:2,Bin/binary>> = Unaligned,
  <<Char/little-utf16>> = Bin,
  <<Char/little-utf16>> = make_unaligned(Bin),
  ok.

%%-------------------------------------------------------------------

utf32_roundtrip() ->
  Big = fun utf32_big_roundtrip/1,
  Little = fun utf32_little_roundtrip/1,
  PidRefs = [spawn_monitor(fun() -> do_utf32_roundtrip(Fun) end) ||
	      Fun <- [Big,Little]],
  [receive {'DOWN', Ref, process, Pid, Reason} -> normal=Reason end ||
    {Pid, Ref} <- PidRefs],
  ok.

do_utf32_roundtrip(Fun) ->
  do_utf32_roundtrip(0, 16#D7FF, Fun),
  do_utf32_roundtrip(16#E000, 16#10FFFF, Fun).

do_utf32_roundtrip(First, Last, Fun) when First =< Last ->
  Fun(First),
  do_utf32_roundtrip(First+1, Last, Fun);
do_utf32_roundtrip(_, _, _) -> ok.

utf32_big_roundtrip(Char) ->
  Bin = id(<<Char/utf32>>),
  Bin = id(<<(id(<<>>))/binary,Char/utf32>>),
  Unaligned = id(<<3:2,Char/utf32>>),
  <<_:2,Bin/binary>> = Unaligned,
  <<Char/utf32>> = Bin,
  <<Char/utf32>> = make_unaligned(Bin),
  ok.

utf32_little_roundtrip(Char) ->
  Bin = id(<<Char/little-utf32>>),
  Bin = id(<<(id(<<>>))/binary,Char/little-utf32>>),
  Unaligned = id(<<3:2,Char/little-utf32>>),
  <<_:2,Bin/binary>> = Unaligned,
  <<Char/little-utf32>> = Bin,
  <<Char/little-utf32>> = make_unaligned(Bin),
  ok.

%%-------------------------------------------------------------------

utf8_illegal_sequences() ->
  fail_range(16#10FFFF+1, 16#10FFFF+512), % Too large.
  fail_range(16#D800, 16#DFFF),	    % Reserved for UTF-16.

  %% Illegal first character.
  [fail(<<I,16#8F,16#8F,16#8F>>) || I <- lists:seq(16#80, 16#BF)],

  %% Short sequences.
  short_sequences(16#80, 16#10FFFF),

  %% Overlong sequences. (Using more bytes than necessary
  %% is not allowed.)
  overlong(0, 127, 2),
  overlong(128, 16#7FF, 3),
  overlong(16#800, 16#FFFF, 4),
  ok.

fail_range(Char, End) when Char =< End ->
  {'EXIT', _} = (catch <<Char/utf8>>),
  Bin = int_to_utf8(Char),
  fail(Bin),
  fail_range(Char+1, End);
fail_range(_, _) -> ok.

short_sequences(Char, End) ->
  Step = (End - Char) div erlang:system_info(schedulers) + 1,
  PidRefs = short_sequences_1(Char, Step, End),
  [receive {'DOWN', Ref, process, Pid, Reason} -> normal=Reason end ||
    {Pid, Ref} <- PidRefs],
  ok.

short_sequences_1(Char, Step, End) when Char =< End ->
  CharEnd = lists:min([Char+Step-1,End]),
  [spawn_monitor(fun() ->
		     %% io:format("~p - ~p\n", [Char, CharEnd]),
		     do_short_sequences(Char, CharEnd)
		 end)|short_sequences_1(Char+Step, Step, End)];
short_sequences_1(_, _, _) -> [].

do_short_sequences(Char, End) when Char =< End ->
  short_sequence(Char),
  do_short_sequences(Char+1, End);
do_short_sequences(_, _) -> ok.

short_sequence(I) ->
  case int_to_utf8(I) of
    <<S0:3/binary,_:8>> ->
      <<S1:2/binary,R1:8>> = S0,
      <<S2:1/binary,_:8>> = S1,
      fail(S0),
      fail(S1),
      fail(S2),
      fail(<<S2/binary,16#7F,R1,R1>>),
      fail(<<S1/binary,16#7F,R1>>),
      fail(<<S0/binary,16#7F>>);
    <<S0:2/binary,_:8>> ->
      <<S1:1/binary,R1:8>> = S0,
      fail(S0),
      fail(S1),
      fail(<<S0/binary,16#7F>>),
      fail(<<S1/binary,16#7F>>),
      fail(<<S1/binary,16#7F,R1>>);
    <<S:1/binary,_:8>> ->
      fail(S),
      fail(<<S/binary,16#7F>>)
  end.

overlong(Char, Last, NumBytes) when Char =< Last ->
  overlong(Char, NumBytes),
  overlong(Char+1, Last, NumBytes);
overlong(_, _, _) -> ok.

overlong(Char, NumBytes) when NumBytes < 5 ->
  case int_to_utf8(Char, NumBytes) of
    <<Char/utf8>>=Bin ->
      ?t:fail({illegal_encoding_accepted,Bin,Char});
    <<OtherChar/utf8>>=Bin ->
      ?t:fail({illegal_encoding_accepted,Bin,Char,OtherChar});
    _ -> ok
  end,
  overlong(Char, NumBytes+1);
overlong(_, _) -> ok.

fail(Bin) ->
  fail_1(Bin),
  fail_1(make_unaligned(Bin)).

fail_1(<<Char/utf8>> = Bin) ->
  ?t:fail({illegal_encoding_accepted, Bin, Char});
fail_1(_) -> ok.

%%-------------------------------------------------------------------

utf16_illegal_sequences() ->
  utf16_fail_range(16#10FFFF+1, 16#10FFFF+512), % Too large.
  utf16_fail_range(16#D800, 16#DFFF),		% Reserved for UTF-16.
  lonely_hi_surrogate(16#D800, 16#DFFF),
  leading_lo_surrogate(16#DC00, 16#DFFF),
  ok.

utf16_fail_range(Char, End) when Char =< End ->
  {'EXIT', _} = (catch <<Char/big-utf16>>),
  {'EXIT', _} = (catch <<Char/little-utf16>>),
  utf16_fail_range(Char+1, End);
utf16_fail_range(_, _) -> ok.

lonely_hi_surrogate(Char, End) when Char =< End ->
  BinBig = <<Char:16/big>>,
  BinLittle = <<Char:16/little>>,
  case {BinBig,BinLittle} of
    {<<Bad/big-utf16>>,_} ->
      ?t:fail({lonely_hi_surrogate_accepted,Bad});
    {_,<<Bad/little-utf16>>} ->
      ?t:fail({lonely_hi_surrogate_accepted,Bad});
    {_,_} ->
      ok
  end,
  lonely_hi_surrogate(Char+1, End);
lonely_hi_surrogate(_, _) -> ok.

leading_lo_surrogate(Char, End) when Char =< End ->
  leading_lo_surrogate(Char, 16#D800, 16#DFFF),
  leading_lo_surrogate(Char+1, End);
leading_lo_surrogate(_, _) -> ok.

leading_lo_surrogate(HiSurr, LoSurr, End) when LoSurr =< End ->
  BinBig = <<HiSurr:16/big,LoSurr:16/big>>,
  BinLittle = <<HiSurr:16/little,LoSurr:16/little>>,
  case {BinBig,BinLittle} of
    {<<Bad/big-utf16,_/bits>>,_} ->
      ?t:fail({leading_lo_surrogate_accepted,Bad});
    {_,<<Bad/little-utf16,_/bits>>} ->
      ?t:fail({leading_lo_surrogate_accepted,Bad});
    {_,_} ->
      ok
  end,
  leading_lo_surrogate(HiSurr, LoSurr+1, End);
leading_lo_surrogate(_, _, _) -> ok.

%%-------------------------------------------------------------------

utf32_illegal_sequences() ->
  utf32_fail_range(16#10FFFF+1, 16#10FFFF+512), % Too large.
  utf32_fail_range(16#D800, 16#DFFF),		% Reserved for UTF-16.
  utf32_fail_range(-100, -1),
  ok.

utf32_fail_range(Char, End) when Char =< End ->
  {'EXIT', _} = (catch <<Char/big-utf32>>),
  {'EXIT', _} = (catch <<Char/little-utf32>>),
  case {<<Char:32>>,<<Char:32/little>>} of
    {<<Unexpected/utf32>>,_} ->
      ?t:fail(Unexpected);
    {_,<<Unexpected/little-utf32>>} ->
      ?t:fail(Unexpected);
    {_,_} -> ok
  end,
  utf32_fail_range(Char+1, End);
utf32_fail_range(_, _) -> ok.

%%-------------------------------------------------------------------
%% This function intentionally allows construction of UTF-8 sequence
%% in illegal ranges.

int_to_utf8(I) when I =< 16#7F ->
  <<I>>;
int_to_utf8(I) when I =< 16#7FF ->
  B2 = I,
  B1 = (I bsr 6),
  <<1:1,1:1,0:1,B1:5,1:1,0:1,B2:6>>;
int_to_utf8(I) when I =< 16#FFFF ->
  B3 = I,
  B2 = (I bsr 6),
  B1 = (I bsr 12),
  <<1:1,1:1,1:1,0:1,B1:4,1:1,0:1,B2:6,1:1,0:1,B3:6>>;
int_to_utf8(I) when I =< 16#3FFFFF ->
  B4 = I,
  B3 = (I bsr 6),
  B2 = (I bsr 12),
  B1 = (I bsr 18),
  <<1:1,1:1,1:1,1:1,0:1,B1:3,1:1,0:1,B2:6,1:1,0:1,B3:6,1:1,0:1,B4:6>>;
int_to_utf8(I) when I =< 16#3FFFFFF ->
  B5 = I,
  B4 = (I bsr 6),
  B3 = (I bsr 12),
  B2 = (I bsr 18),
  B1 = (I bsr 24),
  <<1:1,1:1,1:1,1:1,1:1,0:1,B1:2,1:1,0:1,B2:6,1:1,0:1,B3:6,1:1,0:1,B4:6,
    1:1,0:1,B5:6>>.

%% int_to_utf8(I, NumberOfBytes) -> Binary.
%%  This function can be used to construct overlong sequences.
int_to_utf8(I, 1) ->
  <<I>>;
int_to_utf8(I, 2) ->
  B2 = I,
  B1 = (I bsr 6),
  <<1:1,1:1,0:1,B1:5,1:1,0:1,B2:6>>;
int_to_utf8(I, 3) ->
  B3 = I,
  B2 = (I bsr 6),
  B1 = (I bsr 12),
  <<1:1,1:1,1:1,0:1,B1:4,1:1,0:1,B2:6,1:1,0:1,B3:6>>;
int_to_utf8(I, 4) ->
  B4 = I,
  B3 = (I bsr 6),
  B2 = (I bsr 12),
  B1 = (I bsr 18),
  <<1:1,1:1,1:1,1:1,0:1,B1:3,1:1,0:1,B2:6,1:1,0:1,B3:6,1:1,0:1,B4:6>>.

%%-------------------------------------------------------------------

make_unaligned(Bin0) when is_binary(Bin0) ->
  Bin1 = <<0:3,Bin0/binary,31:5>>,
  Sz = byte_size(Bin0),
  <<0:3,Bin:Sz/binary,31:5>> = id(Bin1),
  Bin.

%%-------------------------------------------------------------------
%% Just to prevent compiler optimizations

id(X) -> X.
