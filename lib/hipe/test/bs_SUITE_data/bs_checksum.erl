%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
%% Code from Zoltan Toth that crashed the HiPE compiler (in R11B-3).
%% The problem was that the binary matching produces a pretty large
%% integer and we tried to find the range for this integer in a bad way.
%% Fixed on the same day -- 6th March 2007.
%%--------------------------------------------------------------------

-module(bs_checksum).

-export([test/0]).

test() ->
  "3389DAE361AF79B04C9C8E7057F60CC6" = checksum(<<42>>),
  ok.

checksum(Bin) ->
  Context = erlang:md5_init(),
  checksum(Context, Bin).

checksum(Context, <<>>) ->
  bin_to_hex(erlang:md5_final(Context));
checksum(Context, <<Bin:20480/binary,Rest/binary>>) ->
  checksum(erlang:md5_update(Context, Bin), Rest);
checksum(Context,Bin) ->
  checksum(erlang:md5_update(Context, Bin), <<>>).

bin_to_hex(Bin) ->
  lists:flatten([byte_to_hex(X) || X <- binary_to_list(Bin)]).

byte_to_hex(Byte) ->
  [int_to_hex(Byte div 16), int_to_hex(Byte rem 16)].

int_to_hex(Int) when Int < 10 -> $0 + Int;
int_to_hex(Int) when Int > 9 -> $A + Int - 10.
