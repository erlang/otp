%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2013. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%========================================================================
%%
%% Filename : hipe_tagscheme.erl
%% Note     : This is specific to Erlang 5.* (i.e. starting with R9).
%%
%% Modifications:
%%  020904: Happi - added support for external pids and ports.
%%     
%%========================================================================
%% $Id$
%%========================================================================

-module(hipe_tagscheme).

-export([mk_nil/0, mk_fixnum/1, mk_arityval/1, mk_non_value/0]).
-export([is_fixnum/1]).
-export([tag_tuple/2, tag_cons/2]).
-export([test_is_boxed/4, get_header/2]).
-export([test_nil/4, test_cons/4, test_flonum/4, test_fixnum/4,
	 test_tuple/4, test_atom/4, test_bignum/4, test_pos_bignum/4,
	 test_any_pid/4, test_any_port/4,
	 test_ref/4, test_fun/4, test_fun2/5, test_matchstate/4,
	 test_binary/4, test_bitstr/4, test_list/4,
	 test_integer/4, test_number/4, test_constant/4, test_tuple_N/5]).
-export([realtag_fixnum/2, tag_fixnum/2, realuntag_fixnum/2, untag_fixnum/2]).
-export([test_two_fixnums/3, test_fixnums/4, unsafe_fixnum_add/3,
	 unsafe_fixnum_sub/3,
	 fixnum_gt/5, fixnum_lt/5, fixnum_ge/5, fixnum_le/5, fixnum_val/1,
	 fixnum_mul/4,
	 fixnum_addsub/5, fixnum_andorxor/4, fixnum_not/2,
	 fixnum_bsr/3, fixnum_bsl/3]).
-export([unsafe_car/2, unsafe_cdr/2,
	 unsafe_constant_element/3, unsafe_update_element/3, element/6]).
-export([unsafe_closure_element/3]).
-export([mk_fun_header/0, tag_fun/2]).
-export([unsafe_untag_float/2, unsafe_tag_float/2]).
-export([mk_sub_binary/6,mk_sub_binary/7]).
-export([unsafe_mk_big/3, unsafe_load_float/3]).
-export([bignum_sizeneed/1,bignum_sizeneed_code/2, get_one_word_pos_bignum/3]).
-export([test_subbinary/3, test_heap_binary/3]).
-export([create_heap_binary/3, create_refc_binary/3, create_refc_binary/4]).
-export([create_matchstate/6, convert_matchstate/1, compare_matchstate/4]).
-export([get_field_from_term/3, get_field_from_pointer/3,
	 set_field_from_term/3, set_field_from_pointer/3,
	 extract_matchbuffer/2, extract_binary_bytes/2]).

-include("hipe_rtl.hrl").
-include("hipe_literals.hrl").

-ifdef(EFT_NATIVE_ADDRESS).
-export([if_fun_get_arity_and_address/5]).
-endif.

-undef(TAG_PRIMARY_BOXED).
-undef(TAG_IMMED2_MASK).
-undef(TAG_IMMED2_CATCH).
-undef(TAG_IMMED2_SIZE).

%%------------------------------------------------------------------------

-define(TAG_PRIMARY_SIZE,   2).
-define(TAG_PRIMARY_MASK,   16#3).
-define(TAG_PRIMARY_HEADER, 16#0).
-define(TAG_PRIMARY_LIST,   16#1).
-define(TAG_PRIMARY_BOXED,  16#2).
-define(TAG_PRIMARY_IMMED1, 16#3).

-define(TAG_IMMED1_SIZE,  4).
-define(TAG_IMMED1_MASK,  16#F).
-define(TAG_IMMED1_PID,   ((16#0 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_IMMED1)).
-define(TAG_IMMED1_PORT,  ((16#1 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_IMMED1)).
-define(TAG_IMMED1_IMMED2,((16#2 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_IMMED1)).
-define(TAG_IMMED1_SMALL, ((16#3 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_IMMED1)).

-define(TAG_IMMED2_SIZE,  6).
-define(TAG_IMMED2_MASK,  16#3F).
-define(TAG_IMMED2_ATOM,  ((16#0 bsl ?TAG_IMMED1_SIZE) bor ?TAG_IMMED1_IMMED2)).
-define(TAG_IMMED2_CATCH, ((16#1 bsl ?TAG_IMMED1_SIZE) bor ?TAG_IMMED1_IMMED2)).
-define(TAG_IMMED2_NIL,   ((16#3 bsl ?TAG_IMMED1_SIZE) bor ?TAG_IMMED1_IMMED2)).

-define(TAG_HEADER_ARITYVAL,((16#0 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_BIN_MATCHSTATE,   ((16#1 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_POS_BIG, ((16#2 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_NEG_BIG, ((16#3 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(BIG_SIGN_BIT,	     (16#1 bsl ?TAG_PRIMARY_SIZE)).
-define(TAG_HEADER_REF,     ((16#4 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_FUN,     ((16#5 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_FLOAT,   ((16#6 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_EXPORT,  ((16#7 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(BINARY_XXX_MASK,     (16#3 bsl ?TAG_PRIMARY_SIZE)).
-define(TAG_HEADER_REFC_BIN,((16#8 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_HEAP_BIN,((16#9 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_SUB_BIN, ((16#A bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_EXTERNAL_PID, ((16#C bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_EXTERNAL_PORT,((16#D bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_EXTERNAL_REF, ((16#E bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).

-define(TAG_HEADER_MASK, 16#3F).
-define(HEADER_ARITY_OFFS, 6).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mk_header(SZ,TAG) -> (SZ bsl ?HEADER_ARITY_OFFS) + TAG.
mk_arityval(SZ)	-> mk_header(SZ, ?TAG_HEADER_ARITYVAL).

size_from_header(Sz, Header) ->
  [hipe_rtl:mk_alu(Sz, Header, 'srl', hipe_rtl:mk_imm(?HEADER_ARITY_OFFS))].

mk_var_header(Header, Size, Tag) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  [hipe_rtl:mk_alu(Tmp, Size, sll, hipe_rtl:mk_imm(?HEADER_ARITY_OFFS)),
   hipe_rtl:mk_alu(Header, Tmp, 'add', hipe_rtl:mk_imm(Tag))].

mk_fixnum(X) -> (X bsl ?TAG_IMMED1_SIZE) + ?TAG_IMMED1_SMALL.

-define(NIL, ((-1 bsl ?TAG_IMMED2_SIZE) bor ?TAG_IMMED2_NIL)).
mk_nil()	-> ?NIL.
%% mk_atom(X)	-> (X bsl ?TAG_IMMED2_SIZE) + ?TAG_IMMED2_ATOM.
mk_non_value()	-> ?THE_NON_VALUE.

-spec is_fixnum(integer()) -> boolean().
is_fixnum(N) when is_integer(N) ->
  Bits = ?bytes_to_bits(hipe_rtl_arch:word_size()) - ?TAG_IMMED1_SIZE,
  (N =< ((1 bsl (Bits - 1)) - 1)) and (N >= -(1 bsl (Bits - 1))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(HEADER_EXPORT, mk_header(1, ?TAG_HEADER_EXPORT)).
-define(HEADER_FUN, mk_header(?ERL_FUN_SIZE-2, ?TAG_HEADER_FUN)).
-define(HEADER_PROC_BIN, mk_header(?PROC_BIN_WORDSIZE-1, ?TAG_HEADER_REFC_BIN)).
-define(HEADER_SUB_BIN, mk_header(?SUB_BIN_WORDSIZE-2, ?TAG_HEADER_SUB_BIN)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tag_boxed(Res, X) ->
  hipe_rtl:mk_alu(Res, X, 'add', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)).

%% tag_bignum(Res, X) -> tag_boxed(Res, X).
tag_flonum(Res, X) -> tag_boxed(Res, X).
tag_tuple(Res, X) -> tag_boxed(Res, X).

tag_cons(Res, X) ->
  hipe_rtl:mk_alu(Res, X, 'add', hipe_rtl:mk_imm(?TAG_PRIMARY_LIST)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Operations to test if an object has a known type T.

test_nil(X, TrueLab, FalseLab, Pred) ->
  hipe_rtl:mk_branch(X, eq, hipe_rtl:mk_imm(?NIL), TrueLab, FalseLab, Pred).

test_cons(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  Mask = hipe_rtl:mk_imm(?TAG_PRIMARY_MASK - ?TAG_PRIMARY_LIST),
  hipe_rtl:mk_alub(Tmp, X, 'and', Mask, 'eq', TrueLab, FalseLab, Pred).

test_is_boxed(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  Mask = hipe_rtl:mk_imm(?TAG_PRIMARY_MASK - ?TAG_PRIMARY_BOXED),
  hipe_rtl:mk_alub(Tmp, X, 'and', Mask, 'eq', TrueLab, FalseLab, Pred).

get_header(Res, X) ->
  hipe_rtl:mk_load(Res, X, hipe_rtl:mk_imm(-(?TAG_PRIMARY_BOXED))).

mask_and_compare(X, Mask, Value, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  [hipe_rtl:mk_alu(Tmp, X, 'and', hipe_rtl:mk_imm(Mask)),
   hipe_rtl:mk_branch(Tmp, 'eq', hipe_rtl:mk_imm(Value), TrueLab, FalseLab, Pred)].

test_immed1(X, Value, TrueLab, FalseLab, Pred) ->
  mask_and_compare(X, ?TAG_IMMED1_MASK, Value, TrueLab, FalseLab, Pred).

test_internal_pid(X, TrueLab, FalseLab, Pred) ->
  test_immed1(X, ?TAG_IMMED1_PID, TrueLab, FalseLab, Pred).

test_any_pid(X, TrueLab, FalseLab, Pred) ->
  NotInternalPidLab = hipe_rtl:mk_new_label(),
  [test_internal_pid(X, TrueLab, hipe_rtl:label_name(NotInternalPidLab), Pred),
   NotInternalPidLab,
   test_external_pid(X, TrueLab, FalseLab, Pred)].

test_external_pid(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  ExternalPidMask = ?TAG_HEADER_MASK,
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   mask_and_compare(Tmp, ExternalPidMask, ?TAG_HEADER_EXTERNAL_PID,
		    TrueLab, FalseLab, Pred)].

test_internal_port(X, TrueLab, FalseLab, Pred) ->
  test_immed1(X, ?TAG_IMMED1_PORT, TrueLab, FalseLab, Pred).

test_any_port(X, TrueLab, FalseLab, Pred) ->
  NotInternalPortLab = hipe_rtl:mk_new_label(),
  [test_internal_port(X, TrueLab, hipe_rtl:label_name(NotInternalPortLab), Pred),
   NotInternalPortLab,
   test_external_port(X, TrueLab, FalseLab, Pred)].

test_external_port(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  ExternalPortMask = ?TAG_HEADER_MASK,
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   mask_and_compare(Tmp, ExternalPortMask, ?TAG_HEADER_EXTERNAL_PORT,
		    TrueLab, FalseLab, Pred)].

test_fixnum(X, TrueLab, FalseLab, Pred) ->
  test_immed1(X, ?TAG_IMMED1_SMALL, TrueLab, FalseLab, Pred).

test_atom(X, TrueLab, FalseLab, Pred) ->
  mask_and_compare(X, ?TAG_IMMED2_MASK, ?TAG_IMMED2_ATOM,
		   TrueLab, FalseLab, Pred).

test_tuple(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp2 = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   hipe_rtl:mk_alub(Tmp2, Tmp, 'and', hipe_rtl:mk_imm(?TAG_HEADER_MASK), 'eq',
		    TrueLab, FalseLab, Pred)].

test_tuple_N(X, N, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   hipe_rtl:mk_branch(Tmp, 'eq', hipe_rtl:mk_imm(mk_arityval(N)),
		      TrueLab, FalseLab, Pred)].

test_ref(X, TrueLab, FalseLab, Pred) ->
  Hdr = hipe_rtl:mk_new_reg_gcsafe(),
  Tag = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  TwoThirdsTrueLab = hipe_rtl:mk_new_label(),
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Hdr, X),
   hipe_rtl:mk_alu(Tag, Hdr, 'and', hipe_rtl:mk_imm(?TAG_HEADER_MASK)),
   hipe_rtl:mk_branch(Tag, 'eq', hipe_rtl:mk_imm(?TAG_HEADER_REF),
		      TrueLab, hipe_rtl:label_name(TwoThirdsTrueLab), Pred),
   TwoThirdsTrueLab,
   hipe_rtl:mk_branch(Tag, 'eq', hipe_rtl:mk_imm(?TAG_HEADER_EXTERNAL_REF),
		      TrueLab, FalseLab, Pred)
  ].

-ifdef(EFT_NATIVE_ADDRESS).
test_closure(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   mask_and_compare(Tmp, ?TAG_HEADER_MASK, ?TAG_HEADER_FUN,
		    TrueLab, FalseLab, Pred)].
-endif.

test_fun(X, TrueLab, FalseLab, Pred) ->
  Hdr = hipe_rtl:mk_new_reg_gcsafe(),
  Tag = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  TwoThirdsTrueLab = hipe_rtl:mk_new_label(),
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Hdr, X),
   hipe_rtl:mk_alu(Tag, Hdr, 'and', hipe_rtl:mk_imm(?TAG_HEADER_MASK)),
   hipe_rtl:mk_branch(Tag, 'eq', hipe_rtl:mk_imm(?TAG_HEADER_FUN),
		      TrueLab, hipe_rtl:label_name(TwoThirdsTrueLab), Pred),
   TwoThirdsTrueLab,
   hipe_rtl:mk_branch(Tag, 'eq', hipe_rtl:mk_imm(?TAG_HEADER_EXPORT),
		      TrueLab, FalseLab, Pred)].

test_fun2(X, Arity, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  TFalse = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_call([Tmp], {erlang,is_function,2}, [X,Arity],
		    hipe_rtl:label_name(HalfTrueLab), FalseLab, 'not_remote'),
   HalfTrueLab,
   hipe_rtl:mk_load_atom(TFalse, 'false'),
   hipe_rtl:mk_branch(Tmp, 'ne', TFalse, TrueLab, FalseLab, Pred)].

flonum_header() ->
  mk_header(8 div hipe_rtl_arch:word_size(), ?TAG_HEADER_FLOAT).

test_flonum(X, TrueLab, FalseLab, Pred) ->
  HeaderFlonum = flonum_header(),
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   hipe_rtl:mk_branch(Tmp, 'eq', hipe_rtl:mk_imm(HeaderFlonum),
		      TrueLab, FalseLab, Pred)].

test_bignum(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  BigMask = ?TAG_HEADER_MASK - ?BIG_SIGN_BIT,
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   mask_and_compare(Tmp, BigMask, ?TAG_HEADER_POS_BIG,
		    TrueLab, FalseLab, Pred)].

test_pos_bignum(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  BigMask = ?TAG_HEADER_MASK,
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   mask_and_compare(Tmp, BigMask, ?TAG_HEADER_POS_BIG,
		    TrueLab, FalseLab, Pred)].

test_matchstate(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   mask_and_compare(Tmp, ?TAG_HEADER_MASK, ?TAG_HEADER_BIN_MATCHSTATE, 
		    TrueLab, FalseLab, Pred)].

test_bitstr(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  Mask = ?TAG_HEADER_MASK - ?BINARY_XXX_MASK,
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   mask_and_compare(Tmp, Mask, ?TAG_HEADER_REFC_BIN, TrueLab, FalseLab, Pred)].

test_binary(X, TrueLab, FalseLab, Pred) ->
  Tmp1 = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp2 = hipe_rtl:mk_new_reg_gcsafe(),
  IsBoxedLab = hipe_rtl:mk_new_label(),
  IsBitStrLab = hipe_rtl:mk_new_label(),
  IsSubBinLab =  hipe_rtl:mk_new_label(),
  Mask = ?TAG_HEADER_MASK - ?BINARY_XXX_MASK,
  [test_is_boxed(X, hipe_rtl:label_name(IsBoxedLab), FalseLab, Pred),
   IsBoxedLab,
   get_header(Tmp1, X),
   mask_and_compare(Tmp1, Mask, ?TAG_HEADER_REFC_BIN,
		    hipe_rtl:label_name(IsBitStrLab), FalseLab, Pred),
   IsBitStrLab,
   mask_and_compare(Tmp1, ?TAG_HEADER_MASK, ?TAG_HEADER_SUB_BIN,
		    hipe_rtl:label_name(IsSubBinLab), TrueLab, 0.5),
   IsSubBinLab,
   get_field_from_term({sub_binary, bitsize}, X, Tmp2),
   hipe_rtl:mk_branch(Tmp2, eq, hipe_rtl:mk_imm(0), TrueLab, FalseLab, Pred)].

test_list(X, TrueLab, FalseLab, Pred) ->
  Lab = hipe_rtl:mk_new_label(),
  [test_cons(X, TrueLab, hipe_rtl:label_name(Lab), 0.5),
   Lab,
   test_nil(X, TrueLab, FalseLab, Pred)].

test_integer(X, TrueLab, FalseLab, Pred) ->
  Lab = hipe_rtl:mk_new_label(),
  [test_fixnum(X, TrueLab, hipe_rtl:label_name(Lab), 0.5),
   Lab,
   test_bignum(X, TrueLab, FalseLab, Pred)].

test_number(X, TrueLab, FalseLab, Pred) ->
  Lab1 = hipe_rtl:mk_new_label(),
  Lab2 = hipe_rtl:mk_new_label(),
  Lab3 = hipe_rtl:mk_new_label(),
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  BigMask = ?TAG_HEADER_MASK - ?BIG_SIGN_BIT,
  HeaderFlonum = flonum_header(),
  [test_fixnum(X, TrueLab, hipe_rtl:label_name(Lab1), 0.5),
   Lab1,
   test_is_boxed(X, hipe_rtl:label_name(Lab2), FalseLab, 0.5),
   Lab2,
   get_header(Tmp, X),
   mask_and_compare(Tmp, BigMask, ?TAG_HEADER_POS_BIG,
		    TrueLab, hipe_rtl:label_name(Lab3), 0.5),
   Lab3,
   hipe_rtl:mk_branch(Tmp, 'eq', hipe_rtl:mk_imm(HeaderFlonum),
		      TrueLab, FalseLab, Pred)].

%% CONS, NIL, and TUPLE are not constants, everything else is
test_constant(X, TrueLab, FalseLab, Pred) ->
  Lab1 = hipe_rtl:mk_new_label(),
  Lab2 = hipe_rtl:mk_new_label(),
  Pred1 = 1-Pred,
  [test_cons(X, FalseLab, hipe_rtl:label_name(Lab1), Pred1),
   Lab1,
   test_nil(X, FalseLab, hipe_rtl:label_name(Lab2), Pred1),
   Lab2,
   test_tuple(X, FalseLab, TrueLab, Pred1)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tag_fixnum(DestVar, SrcReg) ->
  [hipe_rtl:mk_fixnumop(DestVar, SrcReg, tag)].
%% [hipe_rtl:mk_alu(DestVar, SrcReg, sll, hipe_rtl:mk_imm(?TAG_IMMED1_SIZE)),
%%  hipe_rtl:mk_alu(DestVar, DestVar, add, hipe_rtl:mk_imm(?TAG_IMMED1_SMALL))].

realtag_fixnum(DestVar, SrcReg) ->
  [hipe_rtl:mk_alu(DestVar, SrcReg, sll, hipe_rtl:mk_imm(?TAG_IMMED1_SIZE)),
   hipe_rtl:mk_alu(DestVar, DestVar, add, hipe_rtl:mk_imm(?TAG_IMMED1_SMALL))].

untag_fixnum(DestReg, SrcVar) ->
  hipe_rtl:mk_fixnumop(DestReg, SrcVar, untag).
%%  hipe_rtl:mk_alu(DestReg, SrcVar, 'sra', hipe_rtl:mk_imm(?TAG_IMMED1_SIZE)).

realuntag_fixnum(DestReg, SrcVar) ->
  hipe_rtl:mk_alu(DestReg, SrcVar, 'sra', hipe_rtl:mk_imm(?TAG_IMMED1_SIZE)).

fixnum_val(Fixnum) ->
  Fixnum bsr ?TAG_IMMED1_SIZE.

test_fixnums(Args, TrueLab, FalseLab, Pred) ->
  {Reg, Ands} = test_fixnums_1(Args, []),
  Ands ++ [test_fixnum(Reg, TrueLab, FalseLab, Pred)].

test_fixnums_1([Arg1, Arg2], Acc) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  {Tmp, lists:reverse([hipe_rtl:mk_alu(Tmp, Arg1, 'and', Arg2)|Acc])};
test_fixnums_1([Arg1, Arg2|Args], Acc) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  test_fixnums_1([Tmp|Args], [hipe_rtl:mk_alu(Tmp, Arg1, 'and', Arg2)|Acc]).

test_two_fixnums(Arg1, Arg2, FalseLab) ->
  TrueLab = hipe_rtl:mk_new_label(),
  case hipe_rtl:is_imm(Arg2) of
    true ->
      Value = hipe_rtl:imm_value(Arg2),
      case Value band ?TAG_IMMED1_MASK of
	?TAG_IMMED1_SMALL ->
	  [test_fixnum(Arg1, hipe_rtl:label_name(TrueLab), FalseLab, 0.99),
	   TrueLab];
	_ ->
	  [hipe_rtl:mk_goto(FalseLab)]
      end;
    false ->
      Tmp = hipe_rtl:mk_new_reg_gcsafe(),
      [hipe_rtl:mk_alu(Tmp, Arg1, 'and', Arg2),
       test_fixnum(Tmp, hipe_rtl:label_name(TrueLab), FalseLab, 0.99),
       TrueLab]
  end.

fixnum_cmp(Arg1, Arg2, TrueLab, FalseLab, Pred, CmpOp) ->
  hipe_rtl:mk_branch(Arg1, CmpOp, Arg2, TrueLab, FalseLab, Pred).

fixnum_gt(Arg1, Arg2, TrueLab, FalseLab, Pred) ->
  fixnum_cmp(Arg1, Arg2, TrueLab, FalseLab, Pred, gt).

fixnum_lt(Arg1, Arg2, TrueLab, FalseLab, Pred) ->
  fixnum_cmp(Arg1, Arg2, TrueLab, FalseLab, Pred, lt).

fixnum_ge(Arg1, Arg2, TrueLab, FalseLab, Pred) ->
  fixnum_cmp(Arg1, Arg2, TrueLab, FalseLab, Pred, ge).

fixnum_le(Arg1, Arg2, TrueLab, FalseLab, Pred) ->
  fixnum_cmp(Arg1, Arg2, TrueLab, FalseLab, Pred, le).

%% We know the answer will be a fixnum
unsafe_fixnum_add(Arg1, Arg2, Res) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  [hipe_rtl:mk_alu(Tmp, Arg2, sub, hipe_rtl:mk_imm(?TAG_IMMED1_SMALL)),
   hipe_rtl:mk_alu(Res, Arg1, add, Tmp)].

%% We know the answer will be a fixnum
unsafe_fixnum_sub(Arg1, Arg2, Res) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  [hipe_rtl:mk_alu(Tmp, Arg2, sub, hipe_rtl:mk_imm(?TAG_IMMED1_SMALL)),
   hipe_rtl:mk_alu(Res, Arg1, sub, Tmp)].

%%% (16X+tag)+((16Y+tag)-tag) = 16X+tag+16Y = 16(X+Y)+tag
%%% (16X+tag)-((16Y+tag)-tag) = 16X+tag-16Y = 16(X-Y)+tag
fixnum_addsub(AluOp, Arg1, Arg2, Res, OtherLab) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  %% XXX: Consider moving this test to the users of fixnum_addsub.
  case Arg1 =/= Res andalso Arg2 =/= Res of 
    true -> 
      %% Args differ from res.
      NoOverflowLab = hipe_rtl:mk_new_label(),
      [hipe_rtl:mk_alu(Tmp, Arg2, sub, hipe_rtl:mk_imm(?TAG_IMMED1_SMALL)),
       hipe_rtl:mk_alub(Res, Arg1, AluOp, Tmp, not_overflow,
			hipe_rtl:label_name(NoOverflowLab), 
			hipe_rtl:label_name(OtherLab), 0.99),
       NoOverflowLab];
    false ->
      %% At least one of the arguments is the same as Res.
      Tmp2 = hipe_rtl:mk_new_var(), % XXX: shouldn't this var be a reg?
      NoOverflowLab = hipe_rtl:mk_new_label(),
      [hipe_rtl:mk_alu(Tmp, Arg2, sub, hipe_rtl:mk_imm(?TAG_IMMED1_SMALL)),
       hipe_rtl:mk_alub(Tmp2, Arg1, AluOp, Tmp, not_overflow,
			hipe_rtl:label_name(NoOverflowLab), 
			hipe_rtl:label_name(OtherLab), 0.99),
       NoOverflowLab,
       hipe_rtl:mk_move(Res, Tmp2)]
  end.

%%% ((16X+tag) div 16) * ((16Y+tag)-tag) + tag = X*16Y+tag = 16(XY)+tag
fixnum_mul(Arg1, Arg2, Res, OtherLab) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  U1 = hipe_rtl:mk_new_reg_gcsafe(),
  U2 = hipe_rtl:mk_new_reg_gcsafe(),
  NoOverflowLab = hipe_rtl:mk_new_label(),
  [untag_fixnum(U1, Arg1),
   hipe_rtl:mk_alu(U2, Arg2, 'sub', hipe_rtl:mk_imm(?TAG_IMMED1_SMALL)),
   hipe_rtl:mk_alub(Tmp, U1, 'mul', U2, overflow, hipe_rtl:label_name(OtherLab),
		    hipe_rtl:label_name(NoOverflowLab), 0.01),
   NoOverflowLab,
   hipe_rtl:mk_alu(Res, Tmp, 'add', hipe_rtl:mk_imm(?TAG_IMMED1_SMALL))].

fixnum_andorxor(AluOp, Arg1, Arg2, Res) ->
  case AluOp of
    'xor' ->
      Tmp = hipe_rtl:mk_new_reg_gcsafe(),
      [hipe_rtl:mk_alu(Tmp, Arg1, 'xor', Arg2),	% clears tag :-(
       hipe_rtl:mk_alu(Res, Tmp, 'or', hipe_rtl:mk_imm(?TAG_IMMED1_SMALL))];
    _ -> hipe_rtl:mk_alu(Res, Arg1, AluOp, Arg2)
  end.

fixnum_not(Arg, Res) ->
  Mask = (-1 bsl ?TAG_IMMED1_SIZE),
  hipe_rtl:mk_alu(Res, Arg, 'xor', hipe_rtl:mk_imm(Mask)).

fixnum_bsr(Arg1, Arg2, Res) ->
  Tmp1 = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp2 = hipe_rtl:mk_new_reg_gcsafe(),
  [untag_fixnum(Tmp1, Arg2),
   hipe_rtl:mk_alu(Tmp2, Arg1, 'sra', Tmp1),
   hipe_rtl:mk_alu(Res, Tmp2, 'or', hipe_rtl:mk_imm(?TAG_IMMED1_SMALL))].

%% If someone knows how to make this better, please do.
fixnum_bsl(Arg1, Arg2, Res) ->
  Tmp1 = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp2 = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp3 = hipe_rtl:mk_new_reg_gcsafe(),
  [untag_fixnum(Tmp2, Arg2),
   hipe_rtl:mk_alu(Tmp1, Arg1, 'sub', hipe_rtl:mk_imm(?TAG_IMMED1_SMALL)),
   hipe_rtl:mk_alu(Tmp3, Tmp1, 'sll', Tmp2),
   hipe_rtl:mk_alu(Res, Tmp3, 'or', hipe_rtl:mk_imm(?TAG_IMMED1_SMALL))].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unsafe_car(Dst, Arg) ->
  hipe_rtl:mk_load(Dst, Arg, hipe_rtl:mk_imm(-(?TAG_PRIMARY_LIST))).

unsafe_cdr(Dst, Arg) ->
  WordSize = hipe_rtl_arch:word_size(),
  hipe_rtl:mk_load(Dst, Arg, hipe_rtl:mk_imm(-(?TAG_PRIMARY_LIST)+WordSize)).

unsafe_constant_element(Dst, Index, Tuple) ->	% Index is an immediate
  WordSize = hipe_rtl_arch:word_size(),
  Offset = -(?TAG_PRIMARY_BOXED) + WordSize * hipe_rtl:imm_value(Index),
  hipe_rtl:mk_load(Dst, Tuple, hipe_rtl:mk_imm(Offset)).

unsafe_update_element(Tuple, Index, Value) ->   % Index is an immediate
  WordSize = hipe_rtl_arch:word_size(),
  Offset = -(?TAG_PRIMARY_BOXED) + WordSize * hipe_rtl:imm_value(Index),
  hipe_rtl:mk_store(Tuple, hipe_rtl:mk_imm(Offset), Value).

%%% wrong semantics
%% unsafe_variable_element(Dst, Index, Tuple) -> % Index is an unknown fixnum
%%     %% Load word at (Tuple - 2) + ((Index >> 4) << 2).
%%     %% Offset = ((Index >> 4) << 2) - 2.
%%     %% Index = x..x1111 (fixnum tag is 2#1111).
%%     %% (Index >> 2) = 00x..x11 and ((Index >> 4) << 2) = 00x..x00.
%%     %% Therefore, ((Index >> 4) << 2) = (Index >> 2) - 3.
%%     %% So Offset = ((Index >> 4) << 2) - 2 = (Index >> 2) - (3 + 2).
%%     Tmp1 = hipe_rtl:mk_new_reg_gcsafe(),
%%     Tmp2 = hipe_rtl:mk_new_reg_gcsafe(),
%%     Shift = ?TAG_IMMED1_SIZE - 2,
%%     OffAdj = (?TAG_IMMED1_SMALL bsr Shift) + ?TAG_PRIMARY_BOXED,
%%     [hipe_rtl:mk_alu(Tmp1, Index, 'srl', hipe_rtl:mk_imm(Shift)),
%%      hipe_rtl:mk_alu(Tmp2, Tmp1, 'sub', hipe_rtl:mk_imm(OffAdj)),
%%      hipe_rtl:mk_load(Dst, Tuple, Tmp2)].

element(Dst, Index, Tuple, FailLabName, {tuple, A}, IndexInfo) ->
  FixnumOkLab = hipe_rtl:mk_new_label(),
  IndexOkLab = hipe_rtl:mk_new_label(),
  Ptr = hipe_rtl:mk_new_reg(), % offset from Tuple
  UIndex = hipe_rtl:mk_new_reg_gcsafe(),
  Arity = hipe_rtl:mk_imm(A),
  InvIndex = hipe_rtl:mk_new_reg_gcsafe(),
  Offset = hipe_rtl:mk_new_reg_gcsafe(),
  case IndexInfo of
    valid ->
      %% This is no branch, 1 load and 3 alus = 4 instr
      [untag_fixnum(UIndex, Index),
       hipe_rtl:mk_alu(Ptr, Tuple, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)),
       hipe_rtl:mk_alu(Offset, UIndex, 'sll', 
		       hipe_rtl:mk_imm(hipe_rtl_arch:log2_word_size())),
       hipe_rtl:mk_load(Dst, Ptr, Offset)];
    fixnums ->
      %% This is 1 branch, 1 load and 4 alus = 6 instr
      [untag_fixnum(UIndex, Index),
       hipe_rtl:mk_alu(Ptr, Tuple, 'sub',hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED))|
       gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset, UIndex, 
			FailLabName, IndexOkLab)];
    _ ->
      %% This is 3 branches, 1 load and 5 alus = 9 instr
      [test_fixnum(Index, hipe_rtl:label_name(FixnumOkLab),
		   FailLabName, 0.99),
       FixnumOkLab,
       untag_fixnum(UIndex, Index),
       hipe_rtl:mk_alu(Ptr, Tuple, 'sub',hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED))|
       gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset, UIndex, 
			FailLabName, IndexOkLab)]
  end;
element(Dst, Index, Tuple, FailLabName, tuple, IndexInfo) ->
  FixnumOkLab = hipe_rtl:mk_new_label(),
  IndexOkLab = hipe_rtl:mk_new_label(),
  Ptr = hipe_rtl:mk_new_reg(), % offset from Tuple
  Header = hipe_rtl:mk_new_reg_gcsafe(),
  UIndex = hipe_rtl:mk_new_reg_gcsafe(),
  Arity = hipe_rtl:mk_new_reg_gcsafe(),
  InvIndex = hipe_rtl:mk_new_reg_gcsafe(),
  Offset = hipe_rtl:mk_new_reg_gcsafe(),
  case IndexInfo of
    fixnums ->
      %% This is 1 branch, 2 loads and 5 alus = 8 instr
      [hipe_rtl:mk_alu(Ptr, Tuple, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)),
       hipe_rtl:mk_load(Header, Ptr, hipe_rtl:mk_imm(0)),
       untag_fixnum(UIndex, Index),
       hipe_rtl:mk_alu(Arity,Header,'srl',hipe_rtl:mk_imm(?HEADER_ARITY_OFFS))|
       gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset, UIndex, 
			FailLabName, IndexOkLab)];
    Num when is_integer(Num) ->
      %% This is 1 branch, 1 load and 3 alus = 5 instr
      [hipe_rtl:mk_alu(Ptr, Tuple, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED))|
       gen_element_tail(Dst, Ptr, InvIndex, hipe_rtl:mk_imm(Num), 
			Offset, UIndex, FailLabName, IndexOkLab)];
    _ ->
      %% This is 2 branches, 2 loads and 6 alus = 10 instr
      [test_fixnum(Index, hipe_rtl:label_name(FixnumOkLab), FailLabName, 0.99),
       FixnumOkLab,
       hipe_rtl:mk_alu(Ptr, Tuple, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)),
       hipe_rtl:mk_load(Header, Ptr, hipe_rtl:mk_imm(0)),
       untag_fixnum(UIndex, Index),
       hipe_rtl:mk_alu(Arity,Header,'srl',hipe_rtl:mk_imm(?HEADER_ARITY_OFFS))|
       gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset, UIndex, 
			FailLabName, IndexOkLab)]
  end;
element(Dst, Index, Tuple, FailLabName, unknown, IndexInfo) ->
  FixnumOkLab = hipe_rtl:mk_new_label(),
  BoxedOkLab = hipe_rtl:mk_new_label(),
  TupleOkLab = hipe_rtl:mk_new_label(),
  IndexOkLab = hipe_rtl:mk_new_label(),
  Ptr = hipe_rtl:mk_new_reg(), % offset from Tuple
  Header = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  UIndex = hipe_rtl:mk_new_reg_gcsafe(),
  Arity = hipe_rtl:mk_new_reg_gcsafe(),
  InvIndex = hipe_rtl:mk_new_reg_gcsafe(),
  Offset = hipe_rtl:mk_new_reg_gcsafe(),
  case IndexInfo of
    fixnums ->
      %% This is 3 branches, 2 loads and 5 alus = 10 instr
      [test_is_boxed(Tuple, hipe_rtl:label_name(BoxedOkLab),
		     FailLabName, 0.99),
       BoxedOkLab,
       hipe_rtl:mk_alu(Ptr, Tuple, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)),
       hipe_rtl:mk_load(Header, Ptr, hipe_rtl:mk_imm(0)),
       hipe_rtl:mk_alub(Tmp, Header, 'and', 
			hipe_rtl:mk_imm(?TAG_HEADER_MASK), 'eq',
			hipe_rtl:label_name(TupleOkLab), FailLabName, 0.99),
       TupleOkLab,
       untag_fixnum(UIndex, Index),
       hipe_rtl:mk_alu(Arity, Header, 'srl',
		       hipe_rtl:mk_imm(?HEADER_ARITY_OFFS))|
       gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset, 
			UIndex, FailLabName, IndexOkLab)];
    Num when is_integer(Num) ->
      %% This is 3 branches, 2 loads and 4 alus = 9 instr
      [test_is_boxed(Tuple, hipe_rtl:label_name(BoxedOkLab),
		     FailLabName, 0.99),
       BoxedOkLab,
       hipe_rtl:mk_alu(Ptr, Tuple, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)),
       hipe_rtl:mk_load(Header, Ptr, hipe_rtl:mk_imm(0)),
       hipe_rtl:mk_alub(Tmp, Header, 'and', 
			hipe_rtl:mk_imm(?TAG_HEADER_MASK), 'eq',
			hipe_rtl:label_name(TupleOkLab), FailLabName, 0.99),
       TupleOkLab,
       hipe_rtl:mk_alu(Arity, Header, 'srl', 
		       hipe_rtl:mk_imm(?HEADER_ARITY_OFFS))|
       gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset, 
			hipe_rtl:mk_imm(Num), FailLabName, IndexOkLab)];
    _ ->
      %% This is 4 branches, 2 loads, and 6 alus = 12 instr :(
      [test_fixnum(Index, hipe_rtl:label_name(FixnumOkLab),
		   FailLabName, 0.99),
       FixnumOkLab,      
       test_is_boxed(Tuple, hipe_rtl:label_name(BoxedOkLab),
		     FailLabName, 0.99),
       BoxedOkLab,
       hipe_rtl:mk_alu(Ptr, Tuple, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)),
       hipe_rtl:mk_load(Header, Ptr, hipe_rtl:mk_imm(0)),
       hipe_rtl:mk_alub(Tmp, Header, 'and', 
			hipe_rtl:mk_imm(?TAG_HEADER_MASK), 'eq',
			hipe_rtl:label_name(TupleOkLab), FailLabName, 0.99),
       TupleOkLab,
       untag_fixnum(UIndex, Index),
       hipe_rtl:mk_alu(Arity, Header, 'srl',
		       hipe_rtl:mk_imm(?HEADER_ARITY_OFFS))|
       gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset,
			UIndex, FailLabName, IndexOkLab)]
  end.

gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset, 
		 UIndex, FailLabName, IndexOkLab) ->
  %% now check that 1 <= UIndex <= Arity
  %% if UIndex < 1, then (Arity - UIndex) >= Arity
  %% if UIndex > Arity, then (Arity - UIndex) < 0, which is >=u Arity
  %% otherwise, 0 <= (Arity - UIndex) < Arity
  [hipe_rtl:mk_alu(InvIndex, Arity, 'sub', UIndex),
   hipe_rtl:mk_branch(InvIndex, 'geu', Arity, FailLabName,
		      hipe_rtl:label_name(IndexOkLab), 0.01),
   IndexOkLab,
   hipe_rtl:mk_alu(Offset, UIndex, 'sll',
                   hipe_rtl:mk_imm(hipe_rtl_arch:log2_word_size())),
   hipe_rtl:mk_load(Dst, Ptr, Offset)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unsafe_closure_element(Dst, Index, Closure) ->	% Index is an immediate
  Offset = -(?TAG_PRIMARY_BOXED)    %% Untag
    + ?EFT_ENV                      %% Field offset
                                    %% Index from 1 to N hence -1)
    + (hipe_rtl_arch:word_size() * (hipe_rtl:imm_value(Index)-1)),
  hipe_rtl:mk_load(Dst, Closure, hipe_rtl:mk_imm(Offset)).

mk_fun_header() ->
  hipe_rtl:mk_imm(?HEADER_FUN).

tag_fun(Res, X) ->
  tag_boxed(Res, X).

%% untag_fun(Res, X) ->
%%   hipe_rtl:mk_alu(Res, X, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)).

-ifdef(EFT_NATIVE_ADDRESS).
if_fun_get_arity_and_address(ArityReg, AddressReg, FunP, BadFunLab, Pred) ->
  %% EmuAddressPtrReg = hipe_rtl:mk_new_reg(),
  %% FEPtrReg = hipe_rtl:mk_new_reg(),
  %% ArityReg = hipe_rtl:mk_new_reg(),
  %% NumFreeReg = hipe_rtl:mk_new_reg(),
  %% RealArityReg = hipe_rtl:mk_new_reg(),
  TrueLab0 = hipe_rtl:mk_new_label(),
  %% TrueLab1 = hipe_rtl:mk_new_label(),
  IsFunCode = test_closure(FunP, hipe_rtl:label_name(TrueLab0), BadFunLab, Pred),
  GetArityCode =
    [TrueLab0,
     %% Funp->arity contains the arity
     hipe_rtl:mk_load(ArityReg, FunP,
		      hipe_rtl:mk_imm(-(?TAG_PRIMARY_BOXED)+
				      ?EFT_ARITY)),
     hipe_rtl:mk_load(AddressReg, FunP,
		      hipe_rtl:mk_imm(-(?TAG_PRIMARY_BOXED)+
				      ?EFT_NATIVE_ADDRESS))],
  IsFunCode ++ GetArityCode.
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Binary Code
%%

create_heap_binary(Base, Size, Dst) when is_integer(Size) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  WordSize = hipe_rtl_arch:word_size(),
  NoWords=(Size + 3*WordSize-1) div WordSize,
  NoBytes = NoWords*WordSize,
  HeapBinHeader = hipe_rtl:mk_imm(mk_header(NoWords-1, 
					    ?TAG_HEADER_HEAP_BIN)),
  [GetHPInsn,
   tag_boxed(Dst, HP),
   set_field_from_pointer({heap_bin, thing_word}, HP, HeapBinHeader),
   set_field_from_pointer({heap_bin, binsize}, HP, hipe_rtl:mk_imm(Size)),
   hipe_rtl:mk_alu(Base, HP, add, hipe_rtl:mk_imm(?HEAP_BIN_DATA)),
   hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(NoBytes)),
   PutHPInsn];

create_heap_binary(Base, Size, Dst) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  WordSize = hipe_rtl_arch:word_size(),
  Log2WordSize = hipe_rtl_arch:log2_word_size(),
  EvenWordSize = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp1 = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp2 = hipe_rtl:mk_new_reg_gcsafe(),
  Header = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp3 = hipe_rtl:mk_new_reg(), % offset from HP
  Tmp4 = hipe_rtl:mk_new_reg(), % offset from HP
  [GetHPInsn,
   hipe_rtl:mk_alu(Tmp1, Size, add, hipe_rtl:mk_imm(WordSize-1)),
   hipe_rtl:mk_alu(EvenWordSize, Tmp1, sra, hipe_rtl:mk_imm(Log2WordSize)),
   hipe_rtl:mk_alu(Tmp2, EvenWordSize, add, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Base, HP, add, hipe_rtl:mk_imm(?HEAP_BIN_DATA)),
   mk_var_header(Header, Tmp2, ?TAG_HEADER_HEAP_BIN),
   set_field_from_pointer({heap_bin, thing_word}, HP, Header),
   set_field_from_pointer({heap_bin, binsize}, HP, Size),
   tag_boxed(Dst, HP),
   hipe_rtl:mk_alu(Tmp3, HP, add, Size),
   hipe_rtl:mk_alu(Tmp4, Tmp3, add, hipe_rtl:mk_imm(3*WordSize-1)),
   hipe_rtl:mk_alu(HP, Tmp4, 'and', hipe_rtl:mk_imm(-WordSize)),
   PutHPInsn].

create_refc_binary(Base, Size, Dst) ->
  create_refc_binary(Base, Size, hipe_rtl:mk_imm(0), Dst).

create_refc_binary(Base, Size, Flags, Dst) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  ProcBinHeader = hipe_rtl:mk_imm(?HEADER_PROC_BIN),
  WordSize = hipe_rtl_arch:word_size(),
  Val = hipe_rtl:mk_new_reg(), % offset from Base
  [GetHPInsn,
   tag_boxed(Dst, HP),
   set_field_from_pointer({proc_bin, thing_word}, HP, ProcBinHeader),
   set_field_from_pointer({proc_bin, binsize}, HP, Size),
   heap_arch_spec(HP),
   hipe_rtl:mk_alu(Val, Base, sub, hipe_rtl:mk_imm(?BINARY_ORIG_BYTES)),
   set_field_from_pointer({proc_bin, val}, HP, Val),
   set_field_from_pointer({proc_bin, bytes}, HP, Base),
   set_field_from_pointer({proc_bin, flags}, HP, Flags),
   hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(?PROC_BIN_WORDSIZE*WordSize)),
   PutHPInsn].

heap_arch_spec(HP) ->
  Tmp1 = hipe_rtl:mk_new_reg(), % MSO state
  [hipe_rtl_arch:pcb_load(Tmp1, ?P_OFF_HEAP_FIRST),
   set_field_from_pointer({proc_bin, next}, HP, Tmp1),
   hipe_rtl_arch:pcb_store(?P_OFF_HEAP_FIRST, HP)].

test_heap_binary(Binary, TrueLblName, FalseLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp2 = hipe_rtl:mk_new_reg_gcsafe(),
  [get_header(Tmp1, Binary),
   hipe_rtl:mk_alu(Tmp2, Tmp1, 'and', hipe_rtl:mk_imm(?TAG_HEADER_MASK)),
   hipe_rtl:mk_branch(Tmp2, eq, hipe_rtl:mk_imm(?TAG_HEADER_HEAP_BIN), 
		      TrueLblName, FalseLblName)].

mk_sub_binary(Dst, ByteSize, ByteOffs, BitSize, BitOffs, Orig) -> 
  mk_sub_binary(Dst, ByteSize, ByteOffs, BitSize, BitOffs, 
		       hipe_rtl:mk_imm(0), Orig).

mk_sub_binary(Dst, ByteSize, ByteOffs, BitSize, BitOffs, 
		     Writable, Orig) ->
 {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  WordSize = hipe_rtl_arch:word_size(),
  [GetHPInsn,
   tag_boxed(Dst, HP),
   build_sub_binary(Dst, ByteSize, ByteOffs, BitSize, BitOffs, Writable, Orig),
   hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(?SUB_BIN_WORDSIZE*WordSize)),
   PutHPInsn].

build_sub_binary(Dst, ByteSize, ByteOffs, BitSize, BitOffs, 
		 Writable, Orig) ->
  Head = hipe_rtl:mk_imm(?HEADER_SUB_BIN),
  [set_field_from_term({sub_binary, thing_word}, Dst, Head),
   set_field_from_term({sub_binary, binsize}, Dst, ByteSize),
   set_field_from_term({sub_binary, offset}, Dst, ByteOffs),
   set_field_from_term({sub_binary, bitsize}, Dst, BitSize),
   set_field_from_term({sub_binary, bitoffset}, Dst, BitOffs),
   set_field_from_term({sub_binary, is_writable}, Dst, Writable),
   set_field_from_term({sub_binary, orig}, Dst, Orig)].

test_subbinary(Binary, TrueLblName, FalseLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp2 = hipe_rtl:mk_new_reg_gcsafe(),
  [get_header(Tmp1, Binary),
   hipe_rtl:mk_alu(Tmp2, Tmp1, 'and', hipe_rtl:mk_imm(?TAG_HEADER_MASK)),
   hipe_rtl:mk_branch(Tmp2, eq, hipe_rtl:mk_imm(?TAG_HEADER_SUB_BIN), TrueLblName, FalseLblName)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Float Code

unsafe_load_float(DstLo, DstHi, Src) ->
  WordSize = hipe_rtl_arch:word_size(),
  Offset1 = -(?TAG_PRIMARY_BOXED) + WordSize,
  Offset2 = Offset1 + 4, %% This should really be 4 and not WordSize
  case hipe_rtl_arch:endianess() of
    little ->
      [hipe_rtl:mk_load(DstLo, Src, hipe_rtl:mk_imm(Offset1), int32, unsigned),
       hipe_rtl:mk_load(DstHi, Src, hipe_rtl:mk_imm(Offset2), int32, unsigned)];
    big ->
      [hipe_rtl:mk_load(DstHi, Src, hipe_rtl:mk_imm(Offset1), int32, unsigned),
       hipe_rtl:mk_load(DstLo, Src, hipe_rtl:mk_imm(Offset2), int32, unsigned)]
  end. 

unsafe_untag_float(Dst, Src) ->
  Offset = -(?TAG_PRIMARY_BOXED) + hipe_rtl_arch:word_size(),
  [hipe_rtl:mk_fload(Dst, Src, hipe_rtl:mk_imm(Offset))].

unsafe_tag_float(Dst, Src) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  Head = hipe_rtl:mk_imm(flonum_header()),
  WordSize = hipe_rtl_arch:word_size(),
  [GetHPInsn,
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0), Head),
   hipe_rtl:mk_fstore(HP, hipe_rtl:mk_imm(WordSize), Src),
   tag_flonum(Dst, HP),
   hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(WordSize+8)),
   PutHPInsn].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% BigNum Code

unsafe_mk_big(Dst, Src, Signedness) ->
  WordSize = hipe_rtl_arch:word_size(),
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  PosHead = hipe_rtl:mk_imm(mk_header(1, ?TAG_HEADER_POS_BIG)),
  NegHead = hipe_rtl:mk_imm(mk_header(1, ?TAG_HEADER_NEG_BIG)),
  PosLabel = hipe_rtl:mk_new_label(),
  NegLabel = hipe_rtl:mk_new_label(),
  JoinLabel = hipe_rtl:mk_new_label(),
  PutHeaderCode = 
    case Signedness of
      unsigned ->
	[hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0*WordSize), PosHead)];
      signed ->
	[hipe_rtl:mk_branch(Src, ge, hipe_rtl:mk_imm(0), 
			    hipe_rtl:label_name(PosLabel), 
			    hipe_rtl:label_name(NegLabel)),
	 PosLabel,
	 hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0*WordSize), PosHead),
	 hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLabel)),
	 NegLabel,
	 hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0*WordSize), NegHead),
	 JoinLabel]
    end,
  RestCode = 
    [hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(1*WordSize), Src),
     tag_boxed(Dst, HP),
     hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(2*WordSize)),
     PutHPInsn],
  [GetHPInsn] ++ PutHeaderCode ++ RestCode.

get_one_word_pos_bignum(USize, Size, Fail) ->
  Header = hipe_rtl:mk_new_reg(),
  HalfLbl = hipe_rtl:mk_new_label(),
  HalfLblName = hipe_rtl:label_name(HalfLbl),
  WordSize = hipe_rtl_arch:word_size(),
  PosHead = hipe_rtl:mk_imm(mk_header(1, ?TAG_HEADER_POS_BIG)),
  [get_header(Header, Size),
   hipe_rtl:mk_branch(Header, eq, PosHead, HalfLblName, Fail),
   HalfLbl,
   hipe_rtl:mk_load(USize, Size, hipe_rtl:mk_imm(1*WordSize
						 -?TAG_PRIMARY_BOXED))].

-spec bignum_sizeneed(non_neg_integer()) -> non_neg_integer().

bignum_sizeneed(Size) ->
  WordSizeBits = hipe_rtl_arch:word_size() * 8,
  case is_fixnum(1 bsl Size) of
    true ->
      0;
    false ->
      ((Size + (WordSizeBits-1)) div WordSizeBits) + 1
  end.

bignum_sizeneed_code(SizeReg,FixNumLblName) ->
  WordSizeBits = hipe_rtl_arch:word_size() * 8,
  WordShifts = hipe_rtl_arch:log2_word_size() + 3,
  MaxFixNum = WordSizeBits - ?TAG_IMMED1_SIZE - 1,
  ResReg = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp1 = hipe_rtl:mk_new_reg_gcsafe(),
  BigLbl = hipe_rtl:mk_new_label(),
  Code =
    [hipe_rtl:mk_branch(SizeReg, le, hipe_rtl:mk_imm(MaxFixNum), 
			FixNumLblName, hipe_rtl:label_name(BigLbl)),
     BigLbl,
     hipe_rtl:mk_alu(Tmp1,SizeReg,add,hipe_rtl:mk_imm(WordSizeBits-1)),
     hipe_rtl:mk_alu(ResReg,Tmp1,srl,hipe_rtl:mk_imm(WordShifts)),
     hipe_rtl:mk_alu(ResReg,ResReg,add,hipe_rtl:mk_imm(1))],
  {ResReg,Code}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% MatchState Code

create_matchstate(Max, BinSize, Base, Offset, Orig, Ms) -> 
  WordSize = hipe_rtl_arch:word_size(),
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  ByteSize = (Max+1)*WordSize + ?MS_SAVEOFFSET,
  SizeInWords = ((ByteSize div WordSize) - 1),
  Header = hipe_rtl:mk_imm(mk_header(SizeInWords, ?TAG_HEADER_BIN_MATCHSTATE)),
  [GetHPInsn,
   hipe_rtl:mk_alu(Ms, HP, add, hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)),
   set_field_from_term({matchstate,thing_word}, Ms, Header),
   set_field_from_term({matchstate,{matchbuffer,orig}}, Ms, Orig),
   set_field_from_term({matchstate,{matchbuffer,base}}, Ms, Base),
   set_field_from_term({matchstate,{matchbuffer,binsize}}, Ms, BinSize),
   set_field_from_term({matchstate,{matchbuffer,offset}}, Ms, Offset),
   set_field_from_term({matchstate,{saveoffset, 0}}, Ms, Offset),
   hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(ByteSize)),
   PutHPInsn].

convert_matchstate(Ms) ->
  WordSize = hipe_rtl_arch:word_size(),
  Header = hipe_rtl:mk_new_reg_gcsafe(),
  TmpSize = hipe_rtl:mk_new_reg_gcsafe(),
  SavedOffset = hipe_rtl:mk_new_reg_gcsafe(),
  Orig = hipe_rtl:mk_new_reg_gcsafe(),
  BinSize = hipe_rtl:mk_new_reg_gcsafe(),
  ByteSize = hipe_rtl:mk_new_reg_gcsafe(),
  BitSize = hipe_rtl:mk_new_reg_gcsafe(),
  ByteOffset = hipe_rtl:mk_new_reg_gcsafe(),
  BitOffset = hipe_rtl:mk_new_reg_gcsafe(),
  SizeInWords = hipe_rtl:mk_new_reg_gcsafe(),
  Hole = hipe_rtl:mk_new_reg_gcsafe(),
  BigIntHeader = hipe_rtl:mk_new_reg_gcsafe(),
  [get_field_from_term({matchstate, {matchbuffer, orig}}, Ms, Orig),
   get_field_from_term({matchstate, {matchbuffer, binsize}}, Ms, BinSize),
   get_field_from_term({matchstate, {saveoffset, 0}}, Ms, SavedOffset),
   get_field_from_term({matchstate, thing_word}, Ms, Header),
   hipe_rtl:mk_alu(TmpSize, BinSize, sub, SavedOffset),
   hipe_rtl:mk_alu(BitSize, TmpSize, 'and', hipe_rtl:mk_imm(7)),
   hipe_rtl:mk_alu(BitOffset, SavedOffset, 'and', hipe_rtl:mk_imm(7)),
   hipe_rtl:mk_alu(ByteSize, TmpSize, srl, hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_alu(ByteOffset, SavedOffset, srl, hipe_rtl:mk_imm(3)),
   build_sub_binary(Ms, ByteSize, ByteOffset, BitSize, BitOffset, 
		    hipe_rtl:mk_imm(0), Orig), 
   size_from_header(SizeInWords, Header),
   hipe_rtl:mk_alu(Hole, SizeInWords, sub, hipe_rtl:mk_imm(?SUB_BIN_WORDSIZE)),
   mk_var_header(BigIntHeader, Hole, ?TAG_HEADER_POS_BIG),
   hipe_rtl:mk_store(Ms, hipe_rtl:mk_imm(?SUB_BIN_WORDSIZE*WordSize-?TAG_PRIMARY_BOXED),
		     BigIntHeader)].

compare_matchstate(Max, Ms, LargeEnough, TooSmall) ->
  WordSize = hipe_rtl_arch:word_size(),
  ByteSize = (Max+1)*WordSize + ?MS_SAVEOFFSET,
  SizeInWords = ((ByteSize div WordSize) - 1),
  Header = hipe_rtl:mk_imm(mk_header(SizeInWords, ?TAG_HEADER_BIN_MATCHSTATE)),
  RealHeader = hipe_rtl:mk_new_reg_gcsafe(),
   [hipe_rtl:mk_load(RealHeader, Ms, hipe_rtl:mk_imm(-?TAG_PRIMARY_BOXED)),
    hipe_rtl:mk_branch(RealHeader, ge, Header, LargeEnough, TooSmall)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Struct manipulation code

get_field_offset({matchstate, thing_word}) ->
  ?MS_THING_WORD;
get_field_offset({matchstate, matchbuffer}) ->
  ?MS_MATCHBUFFER;
get_field_offset({matchstate, {matchbuffer, _} = Field}) ->  
  ?MS_MATCHBUFFER + get_field_offset(Field);
get_field_offset({matchstate, {saveoffset, N}} = Field) ->  
  ?MS_SAVEOFFSET + N*get_field_size1(Field);
get_field_offset({sub_binary, thing_word}) ->
  ?SUB_BIN_THING_WORD;
get_field_offset({sub_binary, binsize}) ->
  ?SUB_BIN_BINSIZE;
get_field_offset({sub_binary, bitsize}) ->
  ?SUB_BIN_BITSIZE;
get_field_offset({sub_binary, offset}) ->
  ?SUB_BIN_OFFS;
get_field_offset({sub_binary, bitoffset}) ->
  ?SUB_BIN_BITOFFS;
get_field_offset({sub_binary, is_writable}) ->
  ?SUB_BIN_WRITABLE;
get_field_offset({sub_binary, orig}) ->
  ?SUB_BIN_ORIG;
get_field_offset({proc_bin, thing_word}) ->
  ?PROC_BIN_THING_WORD;
get_field_offset({proc_bin, binsize}) ->
  ?PROC_BIN_BINSIZE;
get_field_offset({proc_bin, next}) ->
  ?PROC_BIN_NEXT;
get_field_offset({proc_bin, val}) ->
  ?PROC_BIN_VAL;
get_field_offset({proc_bin, bytes}) ->
  ?PROC_BIN_BYTES;
get_field_offset({proc_bin, flags}) ->
  ?PROC_BIN_FLAGS;
get_field_offset({binary, orig_bytes}) ->
  ?BINARY_ORIG_BYTES;
get_field_offset({binary, orig_size}) ->
  ?BINARY_ORIG_SIZE;
get_field_offset({heap_bin, thing_word}) ->
  ?HEAP_BIN_THING_WORD;
get_field_offset({heap_bin, binsize}) ->
  ?HEAP_BIN_SIZE;
get_field_offset({heap_bin, {data, N}} = Field) ->
  ?HEAP_BIN_DATA+N*get_field_size1(Field);
get_field_offset({matchbuffer, offset}) ->
  ?MB_OFFSET;
get_field_offset({matchbuffer, orig}) ->  
  ?MB_ORIG;
get_field_offset({matchbuffer, base}) ->  
  ?MB_BASE;
get_field_offset({matchbuffer, binsize}) ->  
  ?MB_SIZE.

get_field_size(Field) ->
  size_to_atom(get_field_size1(Field)).

size_to_atom(Bytes) ->
  WordSize = hipe_rtl_arch:word_size(),
  case Bytes of
    WordSize -> word;
    4 -> int32;
    %%2 -> int16; So far there are no 2 byte fields
    1 -> byte
  end.

get_field_size1({matchstate, thing_word}) ->
  ?MS_THING_WORD_SIZE;
get_field_size1({matchstate, {matchbuffer, _} = Field}) ->  
  get_field_size1(Field);
get_field_size1({matchstate, {saveoffset, _N}}) ->  
  ?MS_SAVEOFFSET_SIZE;
get_field_size1({sub_binary, thing_word}) ->
  ?SUB_BIN_THING_WORD_SIZE;
get_field_size1({sub_binary, binsize}) ->
  ?SUB_BIN_BINSIZE_SIZE;
get_field_size1({sub_binary, bitsize}) ->
  ?SUB_BIN_BITSIZE_SIZE;
get_field_size1({sub_binary, offset}) ->
  ?SUB_BIN_OFFS_SIZE;
get_field_size1({sub_binary, bitoffset}) ->
  ?SUB_BIN_BITOFFS_SIZE;
get_field_size1({sub_binary, is_writable}) ->
  ?SUB_BIN_WRITABLE_SIZE;
get_field_size1({sub_binary, orig}) ->
  ?SUB_BIN_ORIG_SIZE;
get_field_size1({proc_bin, thing_word}) ->
  ?PROC_BIN_THING_WORD_SIZE;
get_field_size1({proc_bin, binsize}) ->
  ?PROC_BIN_BINSIZE_SIZE;
get_field_size1({proc_bin, next}) ->
  ?PROC_BIN_NEXT_SIZE;
get_field_size1({proc_bin, val}) ->
  ?PROC_BIN_VAL_SIZE;
get_field_size1({proc_bin, bytes}) ->
  ?PROC_BIN_BYTES_SIZE;
get_field_size1({proc_bin, flags}) ->
  ?PROC_BIN_FLAGS_SIZE;
get_field_size1({binary, orig_bytes}) ->
  ?BINARY_ORIG_BYTES_SIZE;
get_field_size1({binary, orig_size}) ->
  ?BINARY_ORIG_SIZE_SIZE;
get_field_size1({heap_bin, thing_word}) ->
  ?HEAP_BIN_THING_WORD_SIZE;
get_field_size1({heap_bin, binsize}) ->
  ?HEAP_BIN_SIZE_SIZE;
get_field_size1({heap_bin, {data, _}}) ->
  ?HEAP_BIN_DATA_SIZE;
get_field_size1({matchbuffer, offset}) ->
  ?MB_OFFSET_SIZE;
get_field_size1({matchbuffer, orig}) ->  
  ?MB_ORIG_SIZE;
get_field_size1({matchbuffer, base}) ->  
  ?MB_BASE_SIZE;
get_field_size1({matchbuffer, binsize}) ->  
  ?MB_SIZE_SIZE.

get_field_from_term(Struct, Term, Dst) ->
  Offset = hipe_rtl:mk_imm(get_field_offset(Struct) - ?TAG_PRIMARY_BOXED),
  Size = get_field_size(Struct),
  hipe_rtl:mk_load(Dst, Term, Offset, Size, unsigned). 

set_field_from_term(Struct, Term, Value) ->
  Offset = hipe_rtl:mk_imm(get_field_offset(Struct) - ?TAG_PRIMARY_BOXED),
  Size = get_field_size(Struct),
  hipe_rtl:mk_store(Term, Offset, Value, Size).

get_field_from_pointer(Struct, Term, Dst) ->
  Offset = hipe_rtl:mk_imm(get_field_offset(Struct)),
  Size = get_field_size(Struct),
  hipe_rtl:mk_load(Dst, Term, Offset, Size, unsigned). 

set_field_from_pointer(Struct, Term, Value) ->
  Offset = hipe_rtl:mk_imm(get_field_offset(Struct)),
  Size = get_field_size(Struct),
  hipe_rtl:mk_store(Term, Offset, Value, Size).
  
extract_matchbuffer(Mb, Ms) ->
  What = {matchstate, matchbuffer},
  Offset = hipe_rtl:mk_imm(get_field_offset(What) - ?TAG_PRIMARY_BOXED),
  hipe_rtl:mk_alu(Mb, Ms, add, Offset).

extract_binary_bytes(Binary, Base) ->
  Offset = hipe_rtl:mk_imm(get_field_offset({binary, orig_bytes})),
  hipe_rtl:mk_alu(Base, Binary, add, Offset). 
