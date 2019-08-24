%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Purpose : Checks that labels are handled properly from Core
%%% Created : 2 Nov 2004
%%%-------------------------------------------------------------------
-module(bs_orber).

-export([test/0]).

test() ->
  1 = dec_giop_message_header(<<1,1:32/little-integer>>),
  1 = dec_giop_message_header(<<0,1:32/big-integer>>),
  {2, 1} = dec_giop_message_header(<<2,1:32/little-integer>>),
  {3, 1} = dec_giop_message_header(<<3,1:32/big-integer>>),
  ok.

dec_giop_message_header(<<1:8, MessSize:32/little-integer>>) ->
  MessSize;
dec_giop_message_header(<<0:8, MessSize:32/big-integer>>) ->
  MessSize;
dec_giop_message_header(<<Flags:8, MessSize:32/little-integer>>) when
    ((Flags band 16#03) =:= 16#02) ->
  {Flags, MessSize};
dec_giop_message_header(<<Flags:8, MessSize:32/big-integer>>) ->
  {Flags, MessSize}.
