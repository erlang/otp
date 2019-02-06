%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------
%%% File    : hipe_rtl_binary.erl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Description : 
%%%
%%% Created : 5 Mar 2007 by Per Gustafsson <pergu@it.uu.se>
%%%-------------------------------------------------------------------
-module(hipe_rtl_binary).

-export([gen_rtl/7]).

-export([floorlog2/1, get_word_integer/4, make_size/3, make_size/4]).

%%--------------------------------------------------------------------

-define(BYTE_SHIFT, 3). %% Turn bits into bytes or vice versa
-define(BYTE_SIZE, 8).

%%--------------------------------------------------------------------

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
type_of_operation({bs_put_binary_all,_,_}) -> construct;
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Small utility functions:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_lbls(X) when X > 0 ->
  [hipe_rtl:mk_new_label()|create_lbls(X-1)];
create_lbls(0) ->
  [].

%%------------------------------------------------------------------------------
%% Utilities used by both hipe_rtl_binary_construct and hipe_rtl_binary_match
%%------------------------------------------------------------------------------

get_word_integer(Var, Register, SystemLimitLblName, FalseLblName) ->
  case hipe_rtl:is_imm(Var) of
    true ->
      TaggedVal = hipe_rtl:imm_value(Var),
      true = hipe_tagscheme:is_fixnum(TaggedVal),
      Val = hipe_tagscheme:fixnum_val(TaggedVal),
      if Val < 0 -> [hipe_rtl:mk_goto(FalseLblName)];
	true -> [hipe_rtl:mk_move(Register, hipe_rtl:mk_imm(Val))]
      end;
    false ->
      [EndLbl] = create_lbls(1),
      EndName = hipe_rtl:label_name(EndLbl),
      get_word_integer(Var, Register,SystemLimitLblName,  FalseLblName,
		       EndName, EndName, [EndLbl])
  end.

get_word_integer(Var, Register, SystemLimitLblName, FalseLblName, TrueLblName,
		 BigLblName, Tail) ->
  [FixnumLbl, NotFixnumLbl, BignumLbl, SuccessLbl] = create_lbls(4),
  [hipe_tagscheme:test_fixnum(Var, hipe_rtl:label_name(FixnumLbl),
			      hipe_rtl:label_name(NotFixnumLbl), 0.99),
   FixnumLbl,
   hipe_tagscheme:fixnum_ge(Var, hipe_rtl:mk_imm(hipe_tagscheme:mk_fixnum(0)),
			    hipe_rtl:label_name(SuccessLbl), FalseLblName,
			    0.99),
   SuccessLbl,
   hipe_tagscheme:untag_fixnum(Register, Var),
   hipe_rtl:mk_goto(TrueLblName),
   NotFixnumLbl,
   hipe_tagscheme:test_pos_bignum_arity(Var, 1, hipe_rtl:label_name(BignumLbl),
					FalseLblName, SystemLimitLblName, 0.99),
   BignumLbl,
   hipe_tagscheme:unsafe_get_one_word_pos_bignum(Register, Var),
   hipe_rtl:mk_goto(BigLblName) | Tail].

make_size(UnitImm, BitsVar, FailLblName) ->
  make_size(UnitImm, BitsVar, FailLblName, FailLblName).

make_size(1, BitsVar, OverflowLblName, FalseLblName) ->
  DstReg = hipe_rtl:mk_new_reg_gcsafe(),
  {get_word_integer(BitsVar, DstReg, OverflowLblName, FalseLblName), DstReg};
make_size(?BYTE_SIZE, BitsVar, OverflowLblName, FalseLblName) ->
  DstReg = hipe_rtl:mk_new_reg_gcsafe(),
  [FixnumLbl, BignumLbl] = create_lbls(2),
  WordBits = hipe_rtl_arch:word_size() * ?BYTE_SIZE,
  FixnumLblName = hipe_rtl:label_name(FixnumLbl),
  Tail = [BignumLbl,
	  hipe_rtl:mk_branch(DstReg, 'ltu',
			     hipe_rtl:mk_imm(1 bsl (WordBits - ?BYTE_SHIFT)),
			     FixnumLblName, OverflowLblName, 0.99),
	  FixnumLbl,
	  hipe_rtl:mk_alu(DstReg, DstReg, sll, hipe_rtl:mk_imm(?BYTE_SHIFT))],
  Code = get_word_integer(BitsVar, DstReg, OverflowLblName, FalseLblName,
			  FixnumLblName, hipe_rtl:label_name(BignumLbl), Tail),
  {Code, DstReg};
make_size(UnitImm, BitsVar, OverflowLblName, FalseLblName) ->
  DstReg = hipe_rtl:mk_new_reg_gcsafe(),
  UnitList = number2list(UnitImm),
  Code = multiply_code(UnitList, BitsVar, DstReg, OverflowLblName, FalseLblName),
  {Code, DstReg}.

multiply_code(List=[Head|_Tail], Variable, Result, OverflowLblName,
	      FalseLblName) ->
  Test = set_high(Head),
  Tmp1 = hipe_rtl:mk_new_reg(),
  SuccessLbl = hipe_rtl:mk_new_label(),
  Register = hipe_rtl:mk_new_reg(),
  Code = [hipe_rtl:mk_move(Result, hipe_rtl:mk_imm(0))|
	  get_word_integer(Variable, Register, OverflowLblName, FalseLblName)]
    ++
    [hipe_rtl:mk_alub(Tmp1, Register, 'and', hipe_rtl:mk_imm(Test),
		      eq, hipe_rtl:label_name(SuccessLbl),
		      OverflowLblName, 0.99),
     SuccessLbl],
  multiply_code(List, Register, Result, OverflowLblName, Tmp1, Code).

multiply_code([ShiftSize|Rest], Register, Result, OverflowLblName, Tmp1,
	      OldCode) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  Code =
    OldCode ++
    [hipe_rtl:mk_alu(Tmp1, Register, sll, hipe_rtl:mk_imm(ShiftSize)),
     hipe_rtl:mk_alub(Result, Tmp1, 'add', Result, not_overflow,
		      hipe_rtl:label_name(SuccessLbl), OverflowLblName, 0.99),
     SuccessLbl],
  multiply_code(Rest, Register, Result, OverflowLblName, Tmp1, Code);
multiply_code([], _Register, _Result, _OverflowLblName, _Tmp1, Code) ->
  Code.

set_high(X) ->
  WordBits = hipe_rtl_arch:word_size() * ?BYTE_SIZE,
  set_high(min(X, WordBits), WordBits, 0).

set_high(0, _, Y) ->
  Y;
set_high(X, WordBits, Y) ->
  set_high(X-1, WordBits, Y+(1 bsl (WordBits-X))).


number2list(X) when is_integer(X), X >= 0 ->
  number2list(X, []).

number2list(1, Acc) ->
  lists:reverse([0|Acc]);
number2list(0, Acc) ->
  lists:reverse(Acc);
number2list(X, Acc) ->
  F = floorlog2(X),
  number2list(X-(1 bsl F), [F|Acc]).

floorlog2(X) ->
  %% Double-precision floats do not have enough precision to make floorlog2
  %% exact for integers larger than 2^47.
  Approx = round(math:log(X)/math:log(2)-0.5),
  floorlog2_refine(X, Approx).

floorlog2_refine(X, Approx) ->
  if (1 bsl Approx) > X ->
      floorlog2_refine(X, Approx - 1);
     (1 bsl (Approx+1)) > X ->
      Approx;
     true ->
      floorlog2_refine(X, Approx + 1)
  end.
