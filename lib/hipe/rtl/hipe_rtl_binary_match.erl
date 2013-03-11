%%% -*- erlang-indent-level: 2 -*-
%%%
%%% %CopyrightBegin%
%%% 
%%% Copyright Ericsson AB 2007-2013. All Rights Reserved.
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
%%% File    : hipe_rtl_binary_match.erl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Description : 
%%%
%%% Created :  5 Mar 2007 by Per Gustafsson <pergu@it.uu.se>
%%%-------------------------------------------------------------------
-module(hipe_rtl_binary_match).

-export([gen_rtl/5]).

-import(hipe_tagscheme, [set_field_from_term/3, get_field_from_term/3]).

-include("hipe_literals.hrl").

%%--------------------------------------------------------------------

-define(MAX_BINSIZE, trunc(?MAX_HEAP_BIN_SIZE / hipe_rtl_arch:word_size()) + 2).
-define(BYTE_SHIFT, 3). %% Turn bits into bytes or vice versa
-define(LOW_BITS, 7). %% Three lowest bits set 
-define(BYTE_SIZE, 8).
-define(MAX_SMALL_BITS, (hipe_rtl_arch:word_size() * ?BYTE_SIZE - 5)).

%%--------------------------------------------------------------------

%% ----- bs_start_match -----
gen_rtl({bs_start_match, 0}, [Ms], [Binary], TrueLblName, FalseLblName) ->
  ReInitLbl = hipe_rtl:mk_new_label(),
  BinaryLbl = hipe_rtl:mk_new_label(),
  TestCode = 
    [hipe_rtl:mk_move(Ms,Binary),
     hipe_tagscheme:test_matchstate(Binary, 
				    hipe_rtl:label_name(ReInitLbl),
				    hipe_rtl:label_name(BinaryLbl),
				    0.99)],
  ReInitCode = reinit_matchstate(Ms, TrueLblName),  
  OrdinaryCode = make_matchstate(Binary, 0, Ms, TrueLblName, FalseLblName),
  [TestCode,[ReInitLbl|ReInitCode],[BinaryLbl|OrdinaryCode]];
gen_rtl({bs_start_match, Max}, [Ms], [Binary], TrueLblName, FalseLblName) ->
  MatchStateLbl = hipe_rtl:mk_new_label(),
  BinaryLbl = hipe_rtl:mk_new_label(),
  ReSizeLbl = hipe_rtl:mk_new_label(),
  ReInitLbl = hipe_rtl:mk_new_label(),
  TestCode = 
    [hipe_rtl:mk_move(Ms,Binary),
     hipe_tagscheme:test_matchstate(Binary, 
				    hipe_rtl:label_name(MatchStateLbl),
				    hipe_rtl:label_name(BinaryLbl),
				    0.99)],
  MatchStateTestCode = 
    [hipe_tagscheme:compare_matchstate(Max, Ms, 
				       hipe_rtl:label_name(ReInitLbl),
				       hipe_rtl:label_name(ReSizeLbl))],
  ReSizeCode = resize_matchstate(Ms, Max, TrueLblName),
  ReInitCode = reinit_matchstate(Ms, TrueLblName),  
  OrdinaryCode = make_matchstate(Binary, Max, Ms, TrueLblName, FalseLblName),
  [TestCode, [MatchStateLbl|MatchStateTestCode], [ReSizeLbl|ReSizeCode],
   [ReInitLbl|ReInitCode], [BinaryLbl|OrdinaryCode]];
gen_rtl({bs_start_match, _Max}, [], [Binary], TrueLblName, FalseLblName) ->
  MatchStateLbl = hipe_rtl:mk_new_label(),
  [hipe_tagscheme:test_bitstr(Binary, TrueLblName, 
			      hipe_rtl:label_name(MatchStateLbl), 0.99),
   MatchStateLbl, 
   hipe_tagscheme:test_matchstate(Binary, TrueLblName, FalseLblName, 0.99)];
gen_rtl({{bs_start_match, bitstr}, Max}, [Ms], [Binary],
	TrueLblName, FalseLblName) ->
  make_matchstate(Binary, Max, Ms, TrueLblName, FalseLblName);
gen_rtl({{bs_start_match, bitstr}, _Max}, [], [_Binary],
	TrueLblName, _FalseLblName) ->
  [hipe_rtl:mk_goto(TrueLblName)];
gen_rtl({{bs_start_match, ok_matchstate}, Max}, [Ms], [Binary],
	TrueLblName, FalseLblName) ->
  MatchStateLbl = hipe_rtl:mk_new_label(),
  BinaryLbl = hipe_rtl:mk_new_label(),
  TestCode = 
    [hipe_rtl:mk_move(Ms,Binary),
     hipe_tagscheme:test_matchstate(Binary, 
				    hipe_rtl:label_name(MatchStateLbl),
				    hipe_rtl:label_name(BinaryLbl),
				    0.99)],
  MatchStateCode = reinit_matchstate(Ms, TrueLblName),
  OrdinaryCode = make_matchstate(Binary, Max, Ms, TrueLblName, FalseLblName),
  TestCode ++ [MatchStateLbl|MatchStateCode] ++ [BinaryLbl|OrdinaryCode];
gen_rtl({{bs_start_match, ok_matchstate}, _Max}, [], [Binary],  
	TrueLblName, FalseLblName) ->
  MatchStateLbl = hipe_rtl:mk_new_label(),
  [hipe_tagscheme:test_bitstr(Binary, TrueLblName, 
			      hipe_rtl:label_name(MatchStateLbl), 0.99),
   MatchStateLbl, 
   hipe_tagscheme:test_matchstate(Binary, TrueLblName, FalseLblName, 0.99)];
%% ----- bs_get_integer -----
gen_rtl({bs_get_integer, 0, _Flags}, [Dst, NewMs], [Ms],
	TrueLblName, _FalseLblName) ->
  update_ms(NewMs, Ms) ++
    [hipe_rtl:mk_move(Dst, hipe_rtl:mk_imm(15)),
     hipe_rtl:mk_goto(TrueLblName)];
gen_rtl({bs_get_integer, Size, Flags}, [Dst, NewMs], Args,
	TrueLblName, FalseLblName) ->
  case is_illegal_const(Size) of
    true ->
      [hipe_rtl:mk_goto(FalseLblName)];
    false -> 
      Signed = signed(Flags),
      LittleEndian = littleendian(Flags),
      Aligned = aligned(Flags),
      UnSafe = unsafe(Flags),
      case Args of
	[Ms] ->
	  CCode = int_get_c_code(Dst, Ms, hipe_rtl:mk_imm(Size),
				 Flags, TrueLblName, FalseLblName),
	  update_ms(NewMs, Ms) ++
	    get_static_int(Dst, Ms, Size, CCode,
			   Signed, LittleEndian, Aligned, UnSafe,
			   TrueLblName, FalseLblName);
	[Ms, Arg] ->
	  {SizeCode1, SizeReg1} = make_size(Size, Arg, FalseLblName),
	  CCode = int_get_c_code(Dst, Ms, SizeReg1, Flags, 
				 TrueLblName, FalseLblName),
	  InCode = get_dynamic_int(Dst, Ms, SizeReg1, CCode, 
				   Signed, LittleEndian, Aligned, 
				   TrueLblName, FalseLblName),
	  update_ms(NewMs, Ms) ++ SizeCode1 ++ InCode
      end
  end;
%% ----- bs_get_float -----
gen_rtl({bs_get_float,Size,Flags}, [Dst1, NewMs], Args,
	TrueLblName, FalseLblName) ->
  case is_illegal_const(Size) of
    true ->
      [hipe_rtl:mk_goto(FalseLblName)];
    false -> 
      [hipe_rtl:mk_gctest(3)] ++
	case Args of
	  [Ms] ->
	    CCode = float_get_c_code(Dst1, Ms, hipe_rtl:mk_imm(Size), Flags, 
				     TrueLblName, FalseLblName),
	    update_ms(NewMs, Ms) ++ CCode;
	  [Ms, Arg]  ->
	    {SizeCode, SizeReg} = make_size(Size, Arg, FalseLblName),
	    CCode = float_get_c_code(Dst1, Ms, SizeReg, Flags, 
				     TrueLblName, FalseLblName),
	    update_ms(NewMs, Ms) ++ SizeCode ++ CCode
	end
  end;
%% ----- bs_get_binary_all -----
gen_rtl({bs_get_binary_all, Unit, _Flags}, [Dst], [Ms], 
	TrueLblName, FalseLblName) ->
  [hipe_rtl:mk_gctest(?SUB_BIN_WORDSIZE)] ++
    get_binary_all(Dst, Unit, Ms, TrueLblName,FalseLblName);
%% ----- bs_get_binary_all_2 -----
gen_rtl({bs_get_binary_all_2, Unit, _Flags}, [Dst, NewMs], [Ms], 
	TrueLblName, FalseLblName) ->
  [hipe_rtl:mk_gctest(?SUB_BIN_WORDSIZE)] ++
    update_ms(NewMs, Ms) ++
    get_binary_all(Dst, Unit, Ms, TrueLblName, FalseLblName);
%% ----- bs_get_binary -----
gen_rtl({bs_get_binary, Size, Flags}, [Dst, NewMs], Args,
	TrueLblName, FalseLblName) ->
  case is_illegal_const(Size) of
    true ->
      [hipe_rtl:mk_goto(FalseLblName)];
    false ->
      Unsafe = unsafe(Flags),
      case Args of
	[Ms] ->
	  SizeReg = hipe_rtl:mk_new_reg(),
	  SizeCode = [hipe_rtl:mk_move(SizeReg, hipe_rtl:mk_imm(Size))];
	[Ms, BitsVar] -> 
	  {SizeCode, SizeReg} = make_size(Size, BitsVar, FalseLblName)
      end,
      InCode = get_binary(Dst, Ms, SizeReg, Unsafe,
			  TrueLblName, FalseLblName),
      [hipe_rtl:mk_gctest(?SUB_BIN_WORDSIZE)] ++ 
	update_ms(NewMs, Ms) ++ SizeCode ++ InCode
  end;
%% ----- bs_get_utf8 -----
gen_rtl(bs_get_utf8, [Dst, NewMs], [Ms], TrueLblName, FalseLblName) ->
  update_ms(NewMs, Ms) ++ utf8_get_c_code(Dst, Ms, TrueLblName, FalseLblName);
%% ----- bs_get_utf16 -----
gen_rtl({bs_get_utf16, Flags}, [Dst, NewMs], [Ms], TrueLblName, FalseLblName) ->
  update_ms(NewMs, Ms) ++
    utf16_get_c_code(Flags, Dst, Ms, TrueLblName, FalseLblName);
%% ----- bs_validate_unicode_retract -----
gen_rtl(bs_validate_unicode_retract, [NewMs], [Src, Ms],
	TrueLblName, FalseLblName) ->
  update_ms(NewMs, Ms) ++
    validate_unicode_retract_c_code(Src, Ms, TrueLblName, FalseLblName);
%% ----- bs_test_tail -----
gen_rtl({bs_test_tail, NumBits}, [NewMs], [Ms], TrueLblName, FalseLblName) ->
  {[Offset,BinSize], ExCode} = extract_matchstate_vars([offset,binsize], Ms),
    update_ms(NewMs, Ms) ++ ExCode ++
    [add_to_offset(Offset, Offset, hipe_rtl:mk_imm(NumBits), FalseLblName),
     hipe_rtl:mk_branch(Offset, eq, BinSize, TrueLblName, FalseLblName)];
%% ----- bs_test_unit -----
gen_rtl({bs_test_unit, Unit}, [], [Ms], TrueLblName, FalseLblName) ->
  {[Offset, BinSize], ExCode} = extract_matchstate_vars([offset, binsize], Ms),
  SizeReg = hipe_rtl:mk_new_reg(),
  ExCode ++
    [hipe_rtl:mk_alu(SizeReg, BinSize, sub, Offset)|
    test_alignment_code(SizeReg, Unit, TrueLblName, FalseLblName)];
gen_rtl({bs_test_tail, NumBits}, [], [Ms], TrueLblName, FalseLblName) ->
  {[Offset, BinSize], ExCode} = extract_matchstate_vars([offset, binsize], Ms),
    ExCode ++
    [add_to_offset(Offset, Offset, hipe_rtl:mk_imm(NumBits), FalseLblName),
     hipe_rtl:mk_branch(Offset, eq, BinSize, TrueLblName, FalseLblName)];
%% ----- bs_skip_bits_all -----
gen_rtl({bs_skip_bits_all, Unit, _Flags}, Dst, [Ms],
	TrueLblName, FalseLblName) ->
  opt_update_ms(Dst, Ms) ++
    skip_bits_all(Unit, Ms, TrueLblName, FalseLblName);
%% ----- bs_skip_bits -----
gen_rtl({bs_skip_bits, Bits}, Dst, [Ms|Args], TrueLblName, FalseLblName) ->
  opt_update_ms(Dst, Ms) ++
    case Args of
      [] ->
	skip_bits2(Ms, hipe_rtl:mk_imm(Bits), TrueLblName, FalseLblName);
      [Arg] ->
	{SizeCode, SizeReg} = make_size(Bits, Arg, FalseLblName),
	InCode = skip_bits2(Ms, SizeReg, TrueLblName, FalseLblName),
	SizeCode ++ InCode
    end;
%% ----- bs_restore -----
gen_rtl({bs_restore, Slot}, [NewMs], [Ms], TrueLblName, _FalseLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg_gcsafe(),
  update_ms(NewMs, Ms) ++
    [get_field_from_term({matchstate, {saveoffset, Slot}}, Ms, Tmp1),
     set_field_from_term({matchstate, {matchbuffer, offset}}, Ms, Tmp1),
     hipe_rtl:mk_goto(TrueLblName)];
%% ----- bs_save -----
gen_rtl({bs_save, Slot}, [NewMs], [Ms], TrueLblName, _FalseLblName) ->
  {Offset, Instr} = extract_matchstate_var(offset, Ms),
  update_ms(NewMs, Ms) ++
    [Instr,
     set_field_from_term({matchstate, {saveoffset, Slot}}, Ms, Offset),
     hipe_rtl:mk_goto(TrueLblName)];
%% ----- bs_match_string -----
gen_rtl({bs_match_string, String, ByteSize}, Dst, [Ms],
	TrueLblName, FalseLblName) ->
  {[Offset, BinSize, Base], Instrs} =
    extract_matchstate_vars([offset, binsize, base], Ms),
  [SuccessLbl, ALbl, ULbl] = create_lbls(3),
  [NewOffset, BitOffset] = create_gcsafe_regs(2),
  Unit = hipe_rtl_arch:word_size() - 1,
  Loops = ByteSize div Unit,
  Init = 
    [Instrs,
     opt_update_ms(Dst, Ms),
     check_size(Offset, hipe_rtl:mk_imm(ByteSize*?BYTE_SIZE), BinSize,
		NewOffset, hipe_rtl:label_name(SuccessLbl), FalseLblName),
     SuccessLbl],
  SplitCode = 
    [hipe_rtl:mk_alub(BitOffset, Offset, 'and', hipe_rtl:mk_imm(?LOW_BITS), eq,
		      hipe_rtl:label_name(ALbl), hipe_rtl:label_name(ULbl))],
  Loops = ByteSize div Unit,
  SkipSize = Loops * Unit,
  {ACode1, UCode1} =
    case Loops of
      0 ->
	{[], []};
      _ ->
	create_loops(Loops, Unit, String, Base, 
		     Offset, BitOffset, FalseLblName)
    end,
  <<_:SkipSize/binary, RestString/binary>> = String,
  {ACode2, UCode2} =
    case ByteSize rem Unit of
      0 ->
	{[], []};
      Rem ->
	create_rests(Rem, RestString, Base, Offset, BitOffset, FalseLblName)
    end,
  GoTo = hipe_rtl:mk_goto(TrueLblName),
  End = case Dst of
	  [] -> [GoTo];
	  [NewMs] -> [update_offset(NewOffset, NewMs), GoTo]
	end,
  [Init, SplitCode, ALbl, ACode1, ACode2, End, ULbl, UCode1, UCode2, End];
%% ----- bs_context_to_binary -----
gen_rtl(bs_context_to_binary, [Bin], [Var], TrueLblName, _FalseLblName) ->
  MSLabel = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_move(Bin, Var),
   hipe_tagscheme:test_matchstate(Var, hipe_rtl:label_name(MSLabel), 
				  TrueLblName, 0.5),
   MSLabel,
   hipe_tagscheme:convert_matchstate(Bin),
   hipe_rtl:mk_goto(TrueLblName)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Calls to C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

int_get_c_code(Dst1, Ms, Size, Flags, TrueLblName, FalseLblName) ->
  make_int_gc_code(Size) ++
    get_c_code(bs_get_integer_2, Dst1, Ms, Size, Flags,
	       TrueLblName, FalseLblName).

float_get_c_code(Dst1, Ms, Size, Flags, TrueLblName, FalseLblName) ->
  get_c_code(bs_get_float_2, Dst1, Ms, Size, Flags, TrueLblName, FalseLblName).

get_c_code(Func, Dst1, Ms, Size, Flags, TrueLblName, FalseLblName) ->  
  SizeReg = hipe_rtl:mk_new_reg_gcsafe(),
  FlagsReg = hipe_rtl:mk_new_reg_gcsafe(),
  MatchBuf = hipe_rtl:mk_new_reg(),
  RetLabel = hipe_rtl:mk_new_label(),
  NonVal = hipe_rtl:mk_imm(hipe_tagscheme:mk_non_value()),
  [hipe_rtl:mk_move(SizeReg, Size),
   hipe_rtl:mk_move(FlagsReg, hipe_rtl:mk_imm(Flags)),
   hipe_tagscheme:extract_matchbuffer(MatchBuf, Ms),
   hipe_rtl_arch:call_bif([Dst1], Func, [SizeReg, FlagsReg, MatchBuf], 
			  hipe_rtl:label_name(RetLabel), FalseLblName), 
   RetLabel,
   hipe_rtl:mk_branch(Dst1, eq, NonVal, FalseLblName, TrueLblName, 0.01)].

utf8_get_c_code(Dst, Ms, TrueLblName, FalseLblName) ->
  MatchBuf = hipe_rtl:mk_new_reg(),
  NonVal = hipe_rtl:mk_imm(hipe_tagscheme:mk_non_value()),
  [hipe_tagscheme:extract_matchbuffer(MatchBuf, Ms),
   hipe_rtl_arch:call_bif([Dst], bs_get_utf8, [MatchBuf], [], []),
   hipe_rtl:mk_branch(Dst, eq, NonVal, FalseLblName, TrueLblName, 0.01)].

utf16_get_c_code(Flags, Dst, Ms, TrueLblName, FalseLblName) ->
  MatchBuf = hipe_rtl:mk_new_reg(),
  NonVal = hipe_rtl:mk_imm(hipe_tagscheme:mk_non_value()),
  FlagsReg = hipe_rtl:mk_new_reg_gcsafe(),
  [hipe_tagscheme:extract_matchbuffer(MatchBuf, Ms),
   hipe_rtl:mk_move(FlagsReg, hipe_rtl:mk_imm(Flags)),
   hipe_rtl_arch:call_bif([Dst], bs_get_utf16, [MatchBuf, FlagsReg], [], []),
   hipe_rtl:mk_branch(Dst, eq, NonVal, FalseLblName, TrueLblName, 0.01)].

validate_unicode_retract_c_code(Src, Ms, TrueLblName, FalseLblName) ->
  MatchBuf = hipe_rtl:mk_new_reg(),
  Zero = hipe_rtl:mk_imm(0),
  Tmp = hipe_rtl:mk_new_reg(),
  [hipe_tagscheme:extract_matchbuffer(MatchBuf, Ms),
   hipe_rtl_arch:call_bif([Tmp], bs_validate_unicode_retract,
			  [MatchBuf, Src], [], []),
   hipe_rtl:mk_branch(Tmp, eq, Zero, FalseLblName, TrueLblName, 0.01)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Int Code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_loops(Loops, Unit, String, Base, Offset, BitOffset, FalseLblName) -> 
  [Reg] = create_gcsafe_regs(1),
  AlignedFun = fun(Value) ->
		   [get_int_to_reg(Reg, Unit*?BYTE_SIZE, Base, Offset, 'srl', 
				   {unsigned, big}),
		    update_and_test(Reg, Unit, Offset, Value, FalseLblName)]
	       end,
  UnAlignedFun = fun(Value) ->
		     [get_unaligned_int_to_reg(Reg, Unit*?BYTE_SIZE, 
					       Base, Offset, BitOffset,
					       'srl', {unsigned, big})|
		      update_and_test(Reg, Unit, Offset, Value, FalseLblName)]
		 end,
  {create_loops(Loops, Unit, String, AlignedFun),
   create_loops(Loops, Unit, String, UnAlignedFun)}.
	
create_rests(Rem, String, Base, Offset, BitOffset, FalseLblName) ->
  [Reg] = create_gcsafe_regs(1),
  AlignedFun = fun(Value) ->
		   [get_int_to_reg(Reg, Rem*?BYTE_SIZE, Base, Offset, 'srl', 
				   {unsigned, big})|
		    just_test(Reg, Value, FalseLblName)]
	       end,
  UnAlignedFun = fun(Value) ->
		     [get_unaligned_int_to_reg(Reg, Rem*?BYTE_SIZE, 
					       Base, Offset, BitOffset,
					       'srl', {unsigned, big})|
		      just_test(Reg, Value, FalseLblName)]
		 end,
  {create_loops(1, Rem, String, AlignedFun),
   create_loops(1, Rem, String, UnAlignedFun)}.

create_loops(0, _Unit, _String, _IntFun) ->
  [];
create_loops(N, Unit, String, IntFun) ->
  {Value, RestString} = get_value(Unit,String),
  [IntFun(Value),
   create_loops(N-1, Unit, RestString, IntFun)].

update_and_test(Reg, Unit, Offset, Value, FalseLblName) ->
  [add_to_offset(Offset, Offset, hipe_rtl:mk_imm(Unit*?BYTE_SIZE), FalseLblName),
   just_test(Reg, Value, FalseLblName)].

just_test(Reg, Value, FalseLblName) ->
  [ContLbl] = create_lbls(1),
  [hipe_rtl:mk_branch(Reg, eq, hipe_rtl:mk_imm(Value), 
		      hipe_rtl:label_name(ContLbl), FalseLblName),
   ContLbl].

get_value(N,String) ->
  <<I:N/integer-unit:8, Rest/binary>> =  String,
  {I, Rest}.

make_int_gc_code(I) when is_integer(I) ->
  case hipe_tagscheme:bignum_sizeneed(I) of
    0 -> [];
    X when is_integer(X) -> [hipe_rtl:mk_gctest(X)]
  end;
make_int_gc_code(SReg) ->
  FixNumLbl = hipe_rtl:mk_new_label(),
  FixNumLblName = hipe_rtl:label_name(FixNumLbl),
  {ResReg,Code} = hipe_tagscheme:bignum_sizeneed_code(SReg, FixNumLblName),
  Code ++
    [hipe_rtl:mk_gctest(ResReg),
     hipe_rtl:mk_goto(FixNumLblName),
     FixNumLbl].

get_static_int(Dst1, Ms, Size, CCode, Signed, LittleEndian, Aligned, 
	       Unsafe, TrueLblName, FalseLblName) ->
  WordSize = hipe_rtl_arch:word_size(),
  case Size =< WordSize*?BYTE_SIZE of
    true ->
      case {Aligned, LittleEndian} of
	{true, false} ->
	  get_int_from_bin(Ms, Size, Dst1,Signed, LittleEndian, 
			   Unsafe, FalseLblName, TrueLblName);
	{true, true} ->
	  case Size rem ?BYTE_SIZE of
	    0 ->
	      get_int_from_bin(Ms, Size, Dst1, Signed, LittleEndian,
			       Unsafe, FalseLblName, TrueLblName);
	    _ ->
	      CCode
	  end;
	{false, false} ->
	  get_int_from_unaligned_bin(Ms, Size, Dst1, Signed, 
				     Unsafe, FalseLblName, TrueLblName);
	{false, true} ->
	  CCode
      end;
    false ->
      CCode
  end.

get_dynamic_int(Dst1, Ms, SizeReg, CCode, Signed, LittleEndian, true, 
		TrueLblName, FalseLblName) ->
  {Init, End} = make_dyn_prep(SizeReg, CCode),
  Init ++
    get_unknown_size_int(SizeReg, Ms, Dst1, Signed, LittleEndian, 
			 FalseLblName, TrueLblName) ++
    End;
get_dynamic_int(_Dst1, _Ms, _SizeReg, CCode, _Signed, _LittleEndian, false, 
		_TrueLblName, _FalseLblName) ->
  CCode.

get_int_from_bin(Ms, Size, Dst1, Signed, LittleEndian,
		 Unsafe, FalseLblName, TrueLblName) ->
  Shiftr = shift_type(Signed),
  Type = get_type(Signed, LittleEndian),
  NewOffset = hipe_rtl:mk_new_reg_gcsafe(),
  [SuccessLbl] = create_lbls(1),
  {[Base,Offset,BinSize], ExCode} =
    extract_matchstate_vars([base,offset,binsize], Ms),
  ExCode ++
    [check_size(Offset, hipe_rtl:mk_imm(Size), BinSize, NewOffset,
		Unsafe, hipe_rtl:label_name(SuccessLbl), FalseLblName),
     SuccessLbl] ++
    [update_offset(NewOffset, Ms)] ++
    get_int(Dst1, Size, Base, Offset, Shiftr, Type, TrueLblName).

get_int_from_unaligned_bin(Ms, Size, Dst1, Signed,
			   UnSafe, FalseLblName, TrueLblName)  ->
  Shiftr = shift_type(Signed),
  Type = get_type(Signed, false), 
  NewOffset = hipe_rtl:mk_new_reg_gcsafe(),
  [SuccessLbl] = create_lbls(1),
  {[Base,Offset,BinSize], ExCode} =
    extract_matchstate_vars([base,offset,binsize], Ms),
  ExCode ++
    [check_size(Offset, hipe_rtl:mk_imm(Size), BinSize, NewOffset,
		UnSafe, hipe_rtl:label_name(SuccessLbl), FalseLblName),
     SuccessLbl] ++
    [update_offset(NewOffset, Ms)] ++
    get_unaligned_int(Dst1, Size, Base, Offset, Shiftr, Type, TrueLblName).

get_unknown_size_int(SizeReg, Ms, Dst1, Signed, Little,
		     FalseLblName, TrueLblName) ->
  Shiftr = shift_type(Signed),
  Type = get_type(Signed, false),
  [NewOffset] = create_gcsafe_regs(1),
  [SuccessLbl] = create_lbls(1),
  {[Base,Offset,BinSize], ExCode} =
    extract_matchstate_vars([base,offset,binsize], Ms),
  ExCode ++
  [check_size(Offset, SizeReg, BinSize, NewOffset,
	      hipe_rtl:label_name(SuccessLbl), FalseLblName),
   SuccessLbl,
   update_offset(NewOffset, Ms)] ++
  case Little of
    true ->
      get_little_unknown_int(Dst1, Base, Offset, NewOffset, 
			     Shiftr, Type, TrueLblName);
    false ->
      get_big_unknown_int(Dst1, Base, Offset, NewOffset, 
			  Shiftr, Type, TrueLblName)
  end.

make_matchstate(Binary, Max, Ms, TrueLblName, FalseLblName) ->
  Base = hipe_rtl:mk_new_reg(),
  Orig = hipe_rtl:mk_new_var(),
  BinSize = hipe_rtl:mk_new_reg_gcsafe(),
  Offset = hipe_rtl:mk_new_reg_gcsafe(),
  Lbl = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_gctest(?MS_MIN_SIZE+Max),
   get_binary_bytes(Binary, BinSize, Base, Offset,
		    Orig, hipe_rtl:label_name(Lbl), FalseLblName),
   Lbl,
   hipe_tagscheme:create_matchstate(Max, BinSize, Base, Offset, Orig, Ms),
   hipe_rtl:mk_goto(TrueLblName)].

resize_matchstate(Ms, Max, TrueLblName) ->
  Base = hipe_rtl:mk_new_reg(),
  Orig = hipe_rtl:mk_new_var(),
  BinSize = hipe_rtl:mk_new_reg_gcsafe(),
  Offset = hipe_rtl:mk_new_reg_gcsafe(),
  [hipe_rtl:mk_gctest(?MS_MIN_SIZE+Max),
   get_field_from_term({matchstate, {matchbuffer, binsize}}, Ms, BinSize),
   get_field_from_term({matchstate, {matchbuffer, base}}, Ms, Base),
   get_field_from_term({matchstate, {matchbuffer, orig}}, Ms, Orig),
   get_field_from_term({matchstate, {matchbuffer, offset}}, Ms, Offset),
   hipe_tagscheme:create_matchstate(Max, BinSize, Base, Offset, Orig, Ms),
   hipe_rtl:mk_goto(TrueLblName)].

reinit_matchstate(Ms, TrueLblName) -> 
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  [get_field_from_term({matchstate, {matchbuffer, offset}}, Ms, Tmp),
   set_field_from_term({matchstate, {saveoffset, 0}}, Ms, Tmp),
   hipe_rtl:mk_goto(TrueLblName)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Binary Code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_binary_all(Dst1, 1, Ms, TrueLblName, _FalseLblName) ->
  [SizeReg] = create_gcsafe_regs(1),
  {[Offset,BinSize,Orig], ExCode} = 
    extract_matchstate_vars([offset,binsize,orig], Ms),
  MakeCode =
    [hipe_rtl:mk_alu(SizeReg, BinSize, sub, Offset)|
     construct_subbin(Dst1,SizeReg,Offset,Orig)] ++
    [update_offset(BinSize, Ms),
     hipe_rtl:mk_goto(TrueLblName)],
  ExCode ++ MakeCode;
get_binary_all(Dst1, Unit, Ms, TrueLblName, FalseLblName) ->
  [SizeReg] = create_gcsafe_regs(1),
  [SuccessLbl] = create_lbls(1),
  SLblName = hipe_rtl:label_name(SuccessLbl),
  {[Offset,BinSize,Orig], ExCode} = 
    extract_matchstate_vars([offset,binsize,orig], Ms),
  MakeCode =
    [hipe_rtl:mk_alu(SizeReg, BinSize, sub, Offset)|
     test_alignment_code(SizeReg,Unit,SLblName,FalseLblName)] ++
    [SuccessLbl|
     construct_subbin(Dst1,SizeReg,Offset,Orig)] ++
    [update_offset(BinSize, Ms),
     hipe_rtl:mk_goto(TrueLblName)],
  ExCode ++ MakeCode.

get_binary(Dst1, Ms, SizeReg, UnSafe, TrueLblName, FalseLblName) ->
  [SuccessLbl] = create_lbls(1),
  [EndOffset] = create_gcsafe_regs(1),
  {[Offset,BinSize,Orig], ExCode} = 
    extract_matchstate_vars([offset,binsize,orig], Ms),
  CheckCode =
    [check_size(Offset, SizeReg, BinSize, EndOffset, UnSafe,
		hipe_rtl:label_name(SuccessLbl), FalseLblName),
     SuccessLbl],
  MakeCode =
    construct_subbin(Dst1, SizeReg, Offset, Orig)
    ++ [update_offset(EndOffset, Ms),
	hipe_rtl:mk_goto(TrueLblName)],
  ExCode ++ CheckCode ++ MakeCode.

construct_subbin(Dst, Size, Offset, Orig) ->
  [BitOffset, ByteOffset, BitSize, ByteSize] = create_gcsafe_regs(4),
  [hipe_rtl:mk_alu(ByteSize, Size, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_alu(BitSize, Size, 'and', hipe_rtl:mk_imm(?LOW_BITS)),
   hipe_rtl:mk_alu(ByteOffset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_alu(BitOffset, Offset, 'and', hipe_rtl:mk_imm(?LOW_BITS)),
   hipe_tagscheme:mk_sub_binary(Dst, ByteSize, ByteOffset, 
				BitSize, BitOffset, Orig)].

%%%%%%%%%%%%%%%%%%%%%%%%% Skip Bits %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

skip_bits_all(1, Ms, TrueLblName, _FalseLblName) ->
  {[BinSize], ExCode} = extract_matchstate_vars([binsize], Ms),
  ExCode ++ [update_offset(BinSize,Ms), hipe_rtl:mk_goto(TrueLblName)];
skip_bits_all(Unit,Ms, TrueLblName, FalseLblName) ->
  [Size] = create_gcsafe_regs(1),
  [SuccessLbl] = create_lbls(1),
  SLblName = hipe_rtl:label_name(SuccessLbl),
  {[Offset,BinSize], ExCode} = extract_matchstate_vars([offset,binsize], Ms),
  ExCode ++
    [hipe_rtl:mk_alu(Size,BinSize,sub,Offset)]
    ++
    test_alignment_code(Size,Unit,SLblName,FalseLblName) ++
    [SuccessLbl,
     update_offset(BinSize,Ms),
     hipe_rtl:mk_goto(TrueLblName)].

test_alignment_code(Size, Unit, SLblName, FalseLblName) ->
  case Unit of
    1 -> [hipe_rtl:mk_goto(SLblName)];
    2 -> get_fast_test_code(Size,1,SLblName,FalseLblName);
    4 -> get_fast_test_code(Size,3,SLblName,FalseLblName);
    8 -> get_fast_test_code(Size,7,SLblName,FalseLblName);
    16 -> get_fast_test_code(Size,15,SLblName,FalseLblName);
    32 -> get_fast_test_code(Size,31,SLblName,FalseLblName);
    _ -> get_slow_test_code(Size,Unit,SLblName,FalseLblName)
  end.

get_fast_test_code(Size, AndTest, SLblName, FalseLblName) ->
  [Tmp] = create_gcsafe_regs(1),
  [hipe_rtl:mk_alub(Tmp, Size, 'and', hipe_rtl:mk_imm(AndTest),
		    'eq', SLblName, FalseLblName)].

%% This is really slow
get_slow_test_code(Size, Unit, SLblName, FalseLblName) ->
  [Tmp] = create_gcsafe_regs(1),
  [LoopLbl,Lbl1,Lbl2] = create_lbls(3),
  LoopLblName = hipe_rtl:label_name(LoopLbl),
  Lbl1Name = hipe_rtl:label_name(Lbl1),
  Lbl2Name = hipe_rtl:label_name(Lbl2),
  [hipe_rtl:mk_move(Tmp,Size),
   LoopLbl,
   hipe_rtl:mk_branch(Tmp, eq, hipe_rtl:mk_imm(0), SLblName, Lbl1Name),
   Lbl1,
   hipe_rtl:mk_branch(Tmp, lt, hipe_rtl:mk_imm(0), FalseLblName, Lbl2Name),
   Lbl2,
   hipe_rtl:mk_alu(Tmp,Tmp,sub,hipe_rtl:mk_imm(Unit)),
   hipe_rtl:mk_goto(LoopLblName)].

skip_bits2(Ms, NoOfBits, TrueLblName, FalseLblName) ->
  [NewOffset] = create_gcsafe_regs(1),
  [TempLbl] = create_lbls(1),
  {[Offset,BinSize], ExCode} = extract_matchstate_vars([offset,binsize], Ms),
  ExCode ++
    add_to_offset(NewOffset, NoOfBits, Offset, FalseLblName) ++
    [hipe_rtl:mk_branch(BinSize, 'ltu', NewOffset, FalseLblName, 
			hipe_rtl:label_name(TempLbl), 0.01),
     TempLbl,
     update_offset(NewOffset, Ms),
     hipe_rtl:mk_goto(TrueLblName)].

add_to_offset(Result, Extra, Original, FalseLblName) ->
  TrueLbl = hipe_rtl:mk_new_label(),
  %% Note: 'ltu' means 'unsigned overflow'.
  [hipe_rtl:mk_alub(Result, Extra, 'add', Original, 'ltu',
		    FalseLblName, hipe_rtl:label_name(TrueLbl)),
   TrueLbl].

%%%%%%%%%%%%%%%%%%%%%%% Code for start match %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_binary_bytes(Binary, BinSize, Base, Offset, Orig, 
		 TrueLblName, FalseLblName) ->
  [OrigOffset,BitSize,BitOffset] = create_gcsafe_regs(3),
  [SuccessLbl,SubLbl,OtherLbl,JoinLbl] = create_lbls(4),
  [hipe_tagscheme:test_bitstr(Binary, hipe_rtl:label_name(SuccessLbl),
			      FalseLblName, 0.99),
   SuccessLbl,
   get_field_from_term({sub_binary, binsize}, Binary, BinSize),
   hipe_rtl:mk_alu(BinSize, BinSize, sll, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_tagscheme:test_subbinary(Binary, hipe_rtl:label_name(SubLbl),
				 hipe_rtl:label_name(OtherLbl)),
   SubLbl,
   get_field_from_term({sub_binary, offset}, Binary, OrigOffset),
   hipe_rtl:mk_alu(Offset, OrigOffset, sll, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   get_field_from_term({sub_binary, bitoffset}, Binary, BitOffset),
   hipe_rtl:mk_alu(Offset, Offset, add, BitOffset),
   get_field_from_term({sub_binary, bitsize}, Binary, BitSize),
   hipe_rtl:mk_alu(BinSize, BinSize, add, Offset),
   hipe_rtl:mk_alu(BinSize, BinSize, add, BitSize),
   get_field_from_term({sub_binary, orig}, Binary, Orig),
   hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLbl)),
   OtherLbl,
   hipe_rtl:mk_move(Offset, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_move(Orig, Binary),
   JoinLbl] ++
    get_base(Orig,Base) ++
    [hipe_rtl:mk_goto(TrueLblName)].

%%%%%%%%%%%%%%%%%%%%%%%%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_base(Orig,Base) ->
  [HeapLbl,REFCLbl,EndLbl] = create_lbls(3),
  [hipe_tagscheme:test_heap_binary(Orig, hipe_rtl:label_name(HeapLbl),
				   hipe_rtl:label_name(REFCLbl)),
   HeapLbl,
   hipe_rtl:mk_alu(Base, Orig, 'add', hipe_rtl:mk_imm(?HEAP_BIN_DATA-2)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(EndLbl)),
   REFCLbl,
   hipe_rtl:mk_load(Base, Orig, hipe_rtl:mk_imm(?PROC_BIN_BYTES-2)),
   EndLbl].

extract_matchstate_var(binsize, Ms) ->
  BinSize = hipe_rtl:mk_new_reg_gcsafe(),
  {BinSize, 
   get_field_from_term({matchstate, {matchbuffer, binsize}}, Ms, BinSize)};
extract_matchstate_var(offset, Ms) ->
  Offset = hipe_rtl:mk_new_reg_gcsafe(),
  {Offset, 
  get_field_from_term({matchstate, {matchbuffer, offset}}, Ms, Offset)};
extract_matchstate_var(base, Ms) ->
  Base = hipe_rtl:mk_new_reg(),
  {Base, 
   get_field_from_term({matchstate, {matchbuffer, base}}, Ms, Base)};
extract_matchstate_var(orig, Ms) ->
  Orig = hipe_rtl:mk_new_var(),
  {Orig, 
   get_field_from_term({matchstate, {matchbuffer, orig}}, Ms, Orig)}.

extract_matchstate_vars(List, Ms) ->
  lists:unzip([extract_matchstate_var(Name, Ms) || Name <- List]).

check_size(Offset, Size, BinSize, Tmp1, ContLblName, FalseLblName) ->
  [add_to_offset(Tmp1, Offset, Size, FalseLblName),
   hipe_rtl:mk_branch(Tmp1, leu, BinSize, ContLblName, FalseLblName, 0.99)].

check_size(Offset, Size, _BinSize, Tmp1, true, ContLblName, _FalseLblName) ->
  [hipe_rtl:mk_alu(Tmp1, Offset, add, Size),
   hipe_rtl:mk_goto(ContLblName)];
check_size(Offset, Size, BinSize, Tmp1, false, ContLblName, FalseLblName) ->
  check_size(Offset, Size, BinSize, Tmp1, ContLblName, FalseLblName).

shift_type(true) ->
  sra;
shift_type(false) ->
  srl.

get_type(true, LittleEndian) ->
  {signed, endianess(LittleEndian)};
get_type(false, LittleEndian) ->
  {unsigned, endianess(LittleEndian)}.

endianess(true) ->
  little;
endianess(false) ->
  big.    

aligned(Flags) ->
  case Flags band ?BSF_ALIGNED of
    1 -> true;
    0 -> false
  end.

littleendian(Flags) ->
  case Flags band 2 of
    2 -> true;
    0 -> false
  end.

signed(Flags) ->
  case Flags band 4 of
    4 -> true;
    0 -> false
  end.

unsafe(Flags) ->
  case Flags band 16 of
    16 -> true;
    0 -> false
  end.

update_offset(NewOffset, Ms) ->
  set_field_from_term({matchstate, {matchbuffer, offset}}, Ms, NewOffset).

opt_update_ms([NewMs], OldMs) ->
  [hipe_rtl:mk_move(NewMs, OldMs)];
opt_update_ms([], _OldMs) ->
  [].

update_ms(NewMs, OldMs) ->
  [hipe_rtl:mk_move(NewMs, OldMs)].

create_lbls(0) ->
  [];
create_lbls(X) when X > 0 ->
  [hipe_rtl:mk_new_label()|create_lbls(X-1)].

make_dyn_prep(SizeReg, CCode) ->   
  [CLbl, SuccessLbl] = create_lbls(2),
  Init = [hipe_rtl:mk_branch(SizeReg, le, hipe_rtl:mk_imm(?MAX_SMALL_BITS),  
			     hipe_rtl:label_name(SuccessLbl), 
			     hipe_rtl:label_name(CLbl)),
	  SuccessLbl],
  End = [CLbl|CCode],
  {Init, End}.

%%------------------------------------------------------------------------
%% From hipe_rtl_binutil.erl
%%------------------------------------------------------------------------

get_unaligned_int(Dst1, Size, Base, Offset, Shiftr, Type, TrueLblName) ->
  [Reg] = create_regs(1),
  [get_maybe_unaligned_int_to_reg(Reg, Size, Base, Offset, Shiftr, Type),
   do_bignum_code(Size, Type, Reg, Dst1, TrueLblName)].

get_maybe_unaligned_int_to_reg(Reg, Size, Base, Offset, Shiftr, Type) ->
  [LowBits] = create_regs(1),
  [AlignedLbl, UnAlignedLbl, EndLbl] = create_lbls(3),
  [hipe_rtl:mk_alub(LowBits, Offset, 'and', hipe_rtl:mk_imm(?LOW_BITS),
		    eq, hipe_rtl:label_name(AlignedLbl), 
		    hipe_rtl:label_name(UnAlignedLbl)),
   AlignedLbl,
   get_int_to_reg(Reg, Size, Base, Offset, Shiftr, Type),
   hipe_rtl:mk_goto(hipe_rtl:label_name(EndLbl)),
   UnAlignedLbl,
   get_unaligned_int_to_reg(Reg, Size, Base, Offset, LowBits, Shiftr, Type),
   EndLbl].

get_unaligned_int_to_reg(Reg, Size, Base, Offset, LowBits, Shiftr, Type) ->
  [ByteOffset, ShiftBits, LoadDst, Tmp, TotBits] = create_gcsafe_regs(5),
  [MoreLbl, LessLbl, JoinLbl] = create_lbls(3),
  WordSize = hipe_rtl_arch:word_size(),
  MinLoad = (Size-1) div ?BYTE_SIZE +1,
  MaxLoad = MinLoad + 1,
  Code1 =
    [hipe_rtl:mk_alu(TotBits, LowBits, 'add', hipe_rtl:mk_imm(Size)),
     hipe_rtl:mk_alu(ByteOffset, Offset, 'srl', hipe_rtl:mk_imm(?BYTE_SHIFT))],
  Code2 =
    case {Size rem ?BYTE_SIZE, MinLoad} of
      {1, _} ->
	[load_bytes(LoadDst, Base, ByteOffset, Type, MinLoad),
	 hipe_rtl:mk_alu(ShiftBits, LowBits, 'add',
			 hipe_rtl:mk_imm((WordSize-MinLoad)*?BYTE_SIZE))];
      {_, WordSize} ->
	UnsignedBig = {unsigned, big},
	[hipe_rtl:mk_branch(TotBits, le, hipe_rtl:mk_imm(MinLoad*?BYTE_SIZE), 
			    hipe_rtl:label_name(LessLbl), 
			    hipe_rtl:label_name(MoreLbl)),
	 LessLbl,
	 load_bytes(LoadDst, Base, ByteOffset, Type, MinLoad),
	 hipe_rtl:mk_alu(ShiftBits, LowBits, 'add',
			 hipe_rtl:mk_imm((WordSize-MinLoad)*?BYTE_SIZE)),
	 hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLbl)),
	 MoreLbl,
	 load_bytes(LoadDst, Base, ByteOffset, UnsignedBig, MinLoad),
	 hipe_rtl:mk_alu(LoadDst, LoadDst, 'sll', LowBits),
	 load_bytes(Tmp, Base, ByteOffset, UnsignedBig, 1),
	 hipe_rtl:mk_alu(LowBits, hipe_rtl:mk_imm(?BYTE_SIZE), 'sub', LowBits),
	 hipe_rtl:mk_alu(Tmp, Tmp, 'srl', LowBits),
	 hipe_rtl:mk_alu(LoadDst, LoadDst, 'or', Tmp),
	 hipe_rtl:mk_move(ShiftBits, hipe_rtl:mk_imm(0)),
	 JoinLbl];
      {_, _} ->
	[load_bytes(LoadDst, Base, ByteOffset, Type, MaxLoad),
	 hipe_rtl:mk_alu(ShiftBits, LowBits, 'add',
			 hipe_rtl:mk_imm((WordSize-MaxLoad)*?BYTE_SIZE))]
    end,
  Code3 = 
    [hipe_rtl:mk_alu(Tmp, LoadDst, sll, ShiftBits),
     hipe_rtl:mk_alu(Reg, Tmp, Shiftr, 
		     hipe_rtl:mk_imm(WordSize*?BYTE_SIZE-Size))],
  Code1 ++ Code2 ++ Code3.

get_int(Dst1, Size, Base, Offset, Shiftr, Type, TrueLblName) ->
  [Reg] = create_gcsafe_regs(1),
  [get_int_to_reg(Reg, Size, Base, Offset, Shiftr, Type),
   do_bignum_code(Size, Type, Reg, Dst1, TrueLblName)].

get_int_to_reg(Reg, Size, Base, Offset, Shiftr, Type) ->
  [ByteOffset] = create_gcsafe_regs(1),
  Code1 =
    [hipe_rtl:mk_alu(ByteOffset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
     load_bytes(Reg, Base, ByteOffset, Type, ((Size-1) div ?BYTE_SIZE +1))],
  Code2 =
    case Size rem ?BYTE_SIZE  of
      0 ->
	[];
      _ ->
	[hipe_rtl:mk_alu(Reg, Reg, Shiftr, 
			 hipe_rtl:mk_imm(?BYTE_SIZE -Size rem ?BYTE_SIZE))]
    end,
  Code1 ++ Code2.
   
get_big_unknown_int(Dst1, Base, Offset, NewOffset,
		    Shiftr, Type, TrueLblName) ->
  [LoadDst, ByteOffset, Limit, Tmp, LowBits] = create_gcsafe_regs(5),
  [ContLbl, BackLbl, LoopLbl, TagLbl, LastLbl, EndLbl] = create_lbls(6),
  [hipe_rtl:mk_move(LoadDst, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_branch(NewOffset, ne, Offset, hipe_rtl:label_name(ContLbl), 
		      hipe_rtl:label_name(TagLbl), 0.99),
   ContLbl,
   hipe_rtl:mk_alu(Limit, NewOffset, sub, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Limit, Limit, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_alu(ByteOffset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   load_bytes(LoadDst, Base, ByteOffset, Type, 1),
   BackLbl,
   hipe_rtl:mk_branch(ByteOffset, le, Limit, hipe_rtl:label_name(LoopLbl), 
		      hipe_rtl:label_name(EndLbl)),
   LoopLbl,
   load_bytes(Tmp, Base, ByteOffset, {unsigned, big}, 1),
   hipe_rtl:mk_alu(LoadDst, LoadDst, sll, hipe_rtl:mk_imm(?BYTE_SIZE)),
   hipe_rtl:mk_alu(LoadDst, LoadDst, 'or', Tmp),
   hipe_rtl:mk_goto(hipe_rtl:label_name(BackLbl)),
   EndLbl,
   hipe_rtl:mk_alub(LowBits, NewOffset, 'and', hipe_rtl:mk_imm(?LOW_BITS), eq,
		    hipe_rtl:label_name(TagLbl), hipe_rtl:label_name(LastLbl)),
   LastLbl,
   hipe_rtl:mk_alu(LowBits, hipe_rtl:mk_imm(?BYTE_SIZE), 'sub', LowBits),
   hipe_rtl:mk_alu(LoadDst, LoadDst, Shiftr, LowBits),
   TagLbl] ++
    do_bignum_code(64, Type, LoadDst, Dst1, TrueLblName).

get_little_unknown_int(Dst1, Base, Offset, NewOffset,
		       Shiftr, Type, TrueLblName) ->
  [LoadDst, ByteOffset, Limit, ShiftReg, LowBits, Tmp] = create_gcsafe_regs(6),
  [ContLbl, BackLbl, LoopLbl, DoneLbl, TagLbl] = create_lbls(5),
  [hipe_rtl:mk_move(LoadDst, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_branch(NewOffset, ne, Offset, hipe_rtl:label_name(ContLbl), 
		      hipe_rtl:label_name(TagLbl), 0.99),
   ContLbl,
   hipe_rtl:mk_alu(Tmp, NewOffset, sub, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(ByteOffset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_alu(Limit, Tmp, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_move(ShiftReg, hipe_rtl:mk_imm(0)),
   BackLbl,
   hipe_rtl:mk_branch(ByteOffset, lt, Limit, 
		      hipe_rtl:label_name(LoopLbl), 
		      hipe_rtl:label_name(DoneLbl)),
   LoopLbl,
   load_bytes(Tmp, Base, ByteOffset, {unsigned, big}, 1),
   hipe_rtl:mk_alu(Tmp, Tmp, sll, ShiftReg),
   hipe_rtl:mk_alu(ShiftReg, ShiftReg, add, hipe_rtl:mk_imm(?BYTE_SIZE)),
   hipe_rtl:mk_alu(LoadDst, LoadDst, 'or', Tmp),
   hipe_rtl:mk_goto(hipe_rtl:label_name(BackLbl)),
   DoneLbl,
   hipe_rtl:mk_alu(LowBits, NewOffset, 'and', hipe_rtl:mk_imm(?LOW_BITS)),
   hipe_rtl:mk_alu(LowBits, hipe_rtl:mk_imm(?BYTE_SIZE), sub, LowBits),
   hipe_rtl:mk_alu(LowBits, LowBits, 'and', hipe_rtl:mk_imm(?LOW_BITS)),
   load_bytes(Tmp, Base, ByteOffset, Type, 1),
   hipe_rtl:mk_alu(Tmp, Tmp, Shiftr, LowBits),
   hipe_rtl:mk_alu(Tmp, Tmp, sll, ShiftReg),
   hipe_rtl:mk_alu(LoadDst, LoadDst, 'or', Tmp),
   TagLbl] ++
     do_bignum_code(64, Type, LoadDst, Dst1, TrueLblName).

do_bignum_code(Size, {Signedness,_}, Src, Dst1, TrueLblName)
  when is_integer(Size) ->
  case {Size > ?MAX_SMALL_BITS, Signedness} of
    {false, _} ->
      [hipe_tagscheme:tag_fixnum(Dst1, Src),
       hipe_rtl:mk_goto(TrueLblName)];
    {true, signed} ->
      make_int_gc_code(Size) ++
      signed_bignum(Dst1, Src, TrueLblName);
    {true, unsigned} ->
      make_int_gc_code(Size) ++
      unsigned_bignum(Dst1, Src, TrueLblName)
    end.

signed_bignum(Dst1, Src, TrueLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  BignumLabel = hipe_rtl:mk_new_label(),
  [hipe_tagscheme:realtag_fixnum(Dst1, Src),
   hipe_tagscheme:realuntag_fixnum(Tmp1, Dst1),
   hipe_rtl:mk_branch(Tmp1, eq, Src, TrueLblName, 
		      hipe_rtl:label_name(BignumLabel)),
   BignumLabel,
   hipe_tagscheme:unsafe_mk_big(Dst1, Src, signed),
   hipe_rtl:mk_goto(TrueLblName)].

unsigned_bignum(Dst1, Src, TrueLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg_gcsafe(),
  BignumLbl = hipe_rtl:mk_new_label(),
  BignumLblName = hipe_rtl:label_name(BignumLbl),
  NxtLbl = hipe_rtl:mk_new_label(),
  NxtLblName = hipe_rtl:label_name(NxtLbl),
  [hipe_rtl:mk_branch(Src, lt, hipe_rtl:mk_imm(0), BignumLblName, NxtLblName),
   NxtLbl,
   hipe_tagscheme:realtag_fixnum(Dst1, Src),
   hipe_tagscheme:realuntag_fixnum(Tmp1, Dst1),
   hipe_rtl:mk_branch(Tmp1, eq, Src, TrueLblName, BignumLblName),
   BignumLbl,
   hipe_tagscheme:unsafe_mk_big(Dst1, Src, unsigned),
   hipe_rtl:mk_goto(TrueLblName)].

load_bytes(Dst, Base, Offset, {Signedness, _Endianess},1) ->
  [hipe_rtl:mk_load(Dst, Base, Offset, byte, Signedness),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1))];
load_bytes(Dst, Base, Offset, {Signedness, Endianess},2) ->
  case Endianess of
    big ->
      hipe_rtl_arch:load_big_2(Dst, Base, Offset, Signedness);
    little ->
      hipe_rtl_arch:load_little_2(Dst, Base, Offset, Signedness)
  end;
load_bytes(Dst, Base, Offset, {Signedness, Endianess},3) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  case Endianess of
    big ->
      [hipe_rtl:mk_load(Dst, Base, Offset, byte, Signedness),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
       hipe_rtl:mk_alu(Dst, Dst, sll, hipe_rtl:mk_imm(8)),
       hipe_rtl:mk_load(Tmp1, Base, Offset, byte, unsigned),
       hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
       hipe_rtl:mk_alu(Dst, Dst, sll, hipe_rtl:mk_imm(8)),
       hipe_rtl:mk_load(Tmp1, Base, Offset, byte, unsigned),
       hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1))];
    little ->
      [hipe_rtl:mk_load(Dst, Base, Offset, byte, unsigned),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
       hipe_rtl:mk_load(Tmp1, Base, Offset, byte,unsigned),
       hipe_rtl:mk_alu(Tmp1, Tmp1, sll, hipe_rtl:mk_imm(8)),
       hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
       hipe_rtl:mk_load(Tmp1, Base, Offset, byte,Signedness),
       hipe_rtl:mk_alu(Tmp1, Tmp1, sll, hipe_rtl:mk_imm(16)),
       hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1))]
  end; 
load_bytes(Dst, Base, Offset, {Signedness, Endianess}, 4) ->
  case Endianess of
    big ->
      hipe_rtl_arch:load_big_4(Dst, Base, Offset, Signedness);
    little ->
      hipe_rtl_arch:load_little_4(Dst, Base, Offset, Signedness)
  end;

load_bytes(Dst, Base, Offset, {Signedness, Endianess}, X) when X > 1 ->
  [LoopLbl, EndLbl] = create_lbls(2),
  [Tmp1, Limit, TmpOffset] = create_regs(3),
  case Endianess of
    big ->
      [hipe_rtl:mk_alu(Limit, Offset, add, hipe_rtl:mk_imm(X)),
       hipe_rtl:mk_load(Dst, Base, Offset, byte, Signedness),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
       LoopLbl,
       hipe_rtl:mk_load(Tmp1, Base, Offset, byte, unsigned),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
       hipe_rtl:mk_alu(Dst, Dst, sll, hipe_rtl:mk_imm(8)),
       hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
       hipe_rtl:mk_branch(Offset, lt, Limit, hipe_rtl:label_name(LoopLbl),
			  hipe_rtl:label_name(EndLbl)),
       EndLbl];
    little ->
      [hipe_rtl:mk_alu(Limit, Offset, add, hipe_rtl:mk_imm(X)),
       hipe_rtl:mk_alu(TmpOffset, Limit, sub, hipe_rtl:mk_imm(1)),
       hipe_rtl:mk_load(Dst, Base, TmpOffset, byte, Signedness),
       LoopLbl,
       hipe_rtl:mk_alu(TmpOffset, TmpOffset, sub, hipe_rtl:mk_imm(1)),
       hipe_rtl:mk_load(Tmp1, Base, TmpOffset, byte, Signedness),
       hipe_rtl:mk_alu(Dst, Dst, sll, hipe_rtl:mk_imm(8)),
       hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
       hipe_rtl:mk_branch(Offset, lt, TmpOffset, hipe_rtl:label_name(LoopLbl),
			  hipe_rtl:label_name(EndLbl)),
       EndLbl,
       hipe_rtl:mk_move(Offset, Limit)]
  end.

create_regs(X) when X > 0 ->
  [hipe_rtl:mk_new_reg()|create_regs(X-1)];
create_regs(0) ->
  [].

create_gcsafe_regs(X) when X > 0 ->
  [hipe_rtl:mk_new_reg_gcsafe()|create_gcsafe_regs(X-1)];
create_gcsafe_regs(0) ->
  [].

first_part(Var, Register, FalseLblName) ->
  [SuccessLbl1, SuccessLbl2] = create_lbls(2),
  [hipe_tagscheme:test_fixnum(Var, hipe_rtl:label_name(SuccessLbl1),
			      FalseLblName, 0.99),
  SuccessLbl1,
  hipe_tagscheme:fixnum_ge(Var, hipe_rtl:mk_imm(hipe_tagscheme:mk_fixnum(0)), 
			   hipe_rtl:label_name(SuccessLbl2), FalseLblName, 0.99),
  SuccessLbl2,
  hipe_tagscheme:untag_fixnum(Register, Var)].

make_size(1, BitsVar, FalseLblName) ->
  [DstReg] = create_regs(1),
  {first_part(BitsVar, DstReg, FalseLblName), DstReg};
make_size(?BYTE_SIZE, BitsVar, FalseLblName) ->
  [DstReg] = create_regs(1),
  Code = 
    first_part(BitsVar, DstReg, FalseLblName) ++
    [hipe_rtl:mk_alu(DstReg, DstReg, sll, hipe_rtl:mk_imm(?BYTE_SHIFT))],
  {Code, DstReg};
make_size(UnitImm, BitsVar, FalseLblName) ->
  [DstReg] = create_regs(1),
  UnitList = number2list(UnitImm),
  Code = multiply_code(UnitList, BitsVar, DstReg, FalseLblName),
  {Code, DstReg}.

multiply_code(List=[Head|_Tail], Variable, Result, FalseLblName) ->
  Test = set_high(Head),
  Tmp1 = hipe_rtl:mk_new_reg(),
  SuccessLbl = hipe_rtl:mk_new_label(),
  Register = hipe_rtl:mk_new_reg(),
  Code = [hipe_rtl:mk_move(Result, hipe_rtl:mk_imm(0))|
	  first_part(Variable, Register, FalseLblName)]
	++
	 [hipe_rtl:mk_alub(Tmp1, Register, 'and', hipe_rtl:mk_imm(Test), 
			   eq, hipe_rtl:label_name(SuccessLbl), 
			   FalseLblName, 0.99),
	  SuccessLbl],
  multiply_code(List, Register, Result, FalseLblName, Tmp1, Code).

multiply_code([ShiftSize|Rest], Register, Result, FalseLblName, Tmp1, OldCode) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  Code =
    OldCode ++
    [hipe_rtl:mk_alu(Tmp1, Register, sll, hipe_rtl:mk_imm(ShiftSize)),
     hipe_rtl:mk_alub(Result, Tmp1, 'add', Result, not_overflow,
		      hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99),
     SuccessLbl],
  multiply_code(Rest, Register, Result, FalseLblName, Tmp1, Code);
multiply_code([], _Register, _Result, _FalseLblName, _Tmp1, Code) ->
  Code.

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
  round(math:log(X)/math:log(2)-0.5). 

set_high(X) ->
  set_high(X, 0).

set_high(0, Y) ->
  Y;
set_high(X, Y) ->
  set_high(X-1, Y+(1 bsl (27-X))).

is_illegal_const(Const) ->
  Const >=  1 bsl (hipe_rtl_arch:word_size() * ?BYTE_SIZE) orelse Const < 0.
