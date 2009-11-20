%%%
%%% %CopyrightBegin%
%%% 
%%% Copyright Ericsson AB 2006-2009. All Rights Reserved.
%%% 
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved online at http://www.erlang.org/.
%%% 
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%% 
%%% %CopyrightEnd%
%%%
%%%-------------------------------------------------------------------
%%% File    : hipe_rtl_binary_2.erl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Description : 
%%%
%%% Created : 5 Mar 2007 by Per Gustafsson <pergu@it.uu.se>
%%%-------------------------------------------------------------------
-module(hipe_rtl_binary).

-export([gen_rtl/7]).

gen_rtl(BsOP, Dst, Args, TrueLblName, FalseLblName, SysLimName, ConstTab) ->
  case type_of_operation(BsOP) of
    match ->
      {hipe_rtl_binary_match:gen_rtl(
	BsOP, Dst, Args, TrueLblName, FalseLblName),ConstTab};
    construct ->
      hipe_rtl_binary_construct:gen_rtl(
	BsOP, Dst, Args, TrueLblName, FalseLblName, SysLimName, ConstTab)
  end.

type_of_operation({bs_start_match,_}) -> match;
type_of_operation({{bs_start_match,_},_}) -> match;
type_of_operation({bs_get_binary,_,_}) -> match;
type_of_operation({bs_get_binary_all,_,_}) -> match;
type_of_operation({bs_get_binary_all_2,_,_}) -> match;
type_of_operation({bs_get_integer,_,_}) -> match;
type_of_operation({bs_get_float,_,_}) ->  match;
type_of_operation({bs_skip_bits,_}) -> match;
type_of_operation({bs_skip_bits_all,_,_}) ->  match;
type_of_operation({bs_test_tail,_}) -> match;
type_of_operation({bs_restore,_}) -> match;
type_of_operation({bs_save,_}) ->  match;
type_of_operation({bs_test_unit,_}) ->  match;
type_of_operation({bs_match_string,_,_}) ->  match;
type_of_operation(bs_context_to_binary) ->  match;
type_of_operation({bs_add,_}) -> construct;
type_of_operation({bs_add,_,_}) -> construct;
type_of_operation(bs_bits_to_bytes) -> construct;
type_of_operation(bs_bits_to_bytes2) -> construct;
type_of_operation({bs_init,_}) -> construct;
type_of_operation({bs_init,_,_}) -> construct;
type_of_operation({bs_init_bits,_}) -> construct;
type_of_operation({bs_init_bits,_,_}) -> construct;
type_of_operation({bs_put_binary,_,_}) -> construct;
type_of_operation({bs_put_binary_all,_}) -> construct;  
type_of_operation({bs_put_float,_,_,_}) -> construct;
type_of_operation({bs_put_integer,_,_,_}) -> construct;
type_of_operation({bs_put_string,_,_}) -> construct;  
type_of_operation({unsafe_bs_put_integer,_,_,_}) -> construct;
type_of_operation(bs_utf8_size) -> construct;
type_of_operation(bs_put_utf8) -> construct;
type_of_operation(bs_get_utf8) -> match;
type_of_operation(bs_utf16_size) -> construct;
type_of_operation({bs_put_utf16,_}) -> construct;
type_of_operation({bs_get_utf16,_}) -> match;
type_of_operation(bs_validate_unicode) -> construct;
type_of_operation(bs_validate_unicode_retract) -> match;
type_of_operation(bs_final) -> construct;
type_of_operation({bs_append,_,_,_,_}) -> construct;
type_of_operation({bs_private_append,_,_}) -> construct;
type_of_operation(bs_init_writable) -> construct.
