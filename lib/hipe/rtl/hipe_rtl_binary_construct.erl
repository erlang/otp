%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%
%% ====================================================================
%%  Module   : hipe_rtl_binary_construct
%%  Purpose  :  
%%  Notes    : 
%%  History  : Written mostly by Per Gustafsson
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_binary_construct).

-export([gen_rtl/7]).

-import(hipe_rtl_binary, [get_word_integer/4]).

%%-------------------------------------------------------------------------

-include("../main/hipe.hrl").
-include("hipe_rtl.hrl").
-include("hipe_literals.hrl").

-define(BYTE_SHIFT, hipe_rtl:mk_imm(3)). %% Turn bits into bytes or vice versa
-define(LOW_BITS, hipe_rtl:mk_imm(7)). %% Three lowest bits set
-define(LOW_BITS_INT, 7).
-define(BYTE_SIZE, 8).
-define(MAX_BINSIZE, ((1 bsl ((hipe_rtl_arch:word_size()*?BYTE_SIZE)-3)) - 1)).

%% -------------------------------------------------------------------------
%% The code is generated as a list of lists, it will be flattened later.
%% 

gen_rtl(BsOP, Dst, Args, TrueLblName, FalseLblName, SystemLimitLblName, ConstTab) ->
  %%io:format("~w, ~w, ~w~n", [BsOP, Args, Dst]),
  case BsOP of
    {bs_put_string, String, SizeInBytes} ->
      [NewOffset] = get_real(Dst),
      [Base, Offset] = Args,
      put_string(NewOffset, ConstTab, String, SizeInBytes, Base, Offset,
		 TrueLblName);
    _ ->
      Code =
	case BsOP of
	  {bs_init, Size, _Flags} ->
	    [] = Args,
	    [Dst0, Base, Offset] = Dst,
	    case is_illegal_const(Size bsl 3) of
	      true ->
		hipe_rtl:mk_goto(SystemLimitLblName);
	      false ->
		const_init2(Size, Dst0, Base, Offset, TrueLblName)
	    end;

	  {bs_init, _Flags} ->
	    [Size] = Args,
	    [Dst0, Base, Offset] = Dst,
	    var_init2(Size, Dst0, Base, Offset, TrueLblName,
		      SystemLimitLblName, FalseLblName);

	  {bs_init_bits, Size, _Flags} ->
	    [] = Args,
	    [Dst0, Base, Offset] = Dst,
	    case is_illegal_const(Size) of
	      true ->
		hipe_rtl:mk_goto(SystemLimitLblName);
	      false ->
		const_init_bits(Size, Dst0, Base, Offset, TrueLblName)
	    end;

	  {bs_init_bits, _Flags} ->
	    [Size] = Args,
	    [Dst0, Base, Offset] = Dst,
	    var_init_bits(Size, Dst0, Base, Offset, TrueLblName,
			  SystemLimitLblName, FalseLblName);

	  {bs_put_binary_all, Unit, _Flags} ->
	    [Src, Base, Offset] = Args,
	    [NewOffset] = get_real(Dst),
	    put_binary_all(NewOffset, Src, Base, Offset, Unit,
			   TrueLblName, FalseLblName);

	  {bs_put_binary, Size, _Flags} ->
	    case is_illegal_const(Size) of
	      true ->
		[hipe_rtl:mk_goto(FalseLblName)];
	      false ->
		[NewOffset] = get_real(Dst),
		case Args of
		  [Src, Base, Offset] ->
		    put_static_binary(NewOffset, Src, Size, Base, Offset,
				      TrueLblName, FalseLblName);
		  [Src, Bits, Base, Offset]  ->
		    {SizeCode, SizeReg} =
		      hipe_rtl_binary:make_size(Size, Bits, SystemLimitLblName,
						FalseLblName),
		    InCode = put_dynamic_binary(NewOffset, Src, SizeReg, Base,
						Offset, TrueLblName, FalseLblName),
		    SizeCode ++ InCode
		end
	    end;

	  {bs_put_float, Size, Flags, ConstInfo} ->
	    [NewOffset] = get_real(Dst),
	    Aligned = aligned(Flags),
	    LittleEndian = littleendian(Flags),
	   case is_illegal_const(Size) of
	     true ->
	     	[hipe_rtl:mk_goto(FalseLblName)];
	     false ->
	       case Args of
		 [Src, Base, Offset] ->
		   CCode = static_float_c_code(NewOffset, Src, Base, Offset, Size, Flags,
					       TrueLblName, FalseLblName),
		   put_float(NewOffset, Src, Base, Offset, Size, CCode, Aligned,
			     LittleEndian, ConstInfo, TrueLblName);
		 [Src, Bits, Base, Offset] ->
		   {SizeCode, SizeReg} =
		     hipe_rtl_binary:make_size(Size, Bits, SystemLimitLblName,
					       FalseLblName),
		   InCode = float_c_code(NewOffset, Src, Base, Offset, SizeReg,
					 Flags, TrueLblName, FalseLblName),
		   SizeCode ++ InCode
	       end
	   end;

	  {bs_put_integer, Size, Flags, ConstInfo} ->
	    Aligned = aligned(Flags),
	    LittleEndian = littleendian(Flags),
	    [NewOffset] = get_real(Dst),
	    case is_illegal_const(Size) of
	      true ->
		[hipe_rtl:mk_goto(FalseLblName)];
	      false ->
		case ConstInfo of
		  fail ->
		    [hipe_rtl:mk_goto(FalseLblName)];
		  _ ->
		    case Args of
		      [Src, Base, Offset] ->
			CCode = static_int_c_code(NewOffset, Src,
						  Base, Offset, Size,
						  Flags, TrueLblName,
						  FalseLblName),
			put_static_int(NewOffset, Src, Base, Offset, Size,
				       CCode, Aligned, LittleEndian, TrueLblName);
		      [Src, Bits, Base, Offset] ->
			{SizeCode, SizeReg} =
			  hipe_rtl_binary:make_size(Size, Bits,
						    SystemLimitLblName,
						    FalseLblName), 
			CCode = int_c_code(NewOffset, Src, Base,
					   Offset, SizeReg, Flags,
					   TrueLblName, FalseLblName),
			InCode =
			  put_dynamic_int(NewOffset, Src, Base, Offset,
					  SizeReg, CCode, Aligned,
					  LittleEndian, TrueLblName),
			SizeCode ++ InCode
		    end
		end
	    end;

	  {unsafe_bs_put_integer, 0, _Flags, _ConstInfo} ->
	    [NewOffset] = get_real(Dst),
	    case Args of
	      [_Src, _Base, Offset] ->
		[hipe_rtl:mk_move(NewOffset,Offset),
		 hipe_rtl:mk_goto(TrueLblName)];
	      [_Src, _Bits, _Base, Offset] ->
		[hipe_rtl:mk_move(NewOffset,Offset),
		 hipe_rtl:mk_goto(TrueLblName)]
	    end;

	  {unsafe_bs_put_integer, Size, Flags, ConstInfo} ->
	     case is_illegal_const(Size) of
	      true ->
		[hipe_rtl:mk_goto(FalseLblName)];
	      false ->
		 Aligned = aligned(Flags),
		 LittleEndian = littleendian(Flags),
		 [NewOffset] = get_real(Dst),
		 case ConstInfo of
		   fail ->
		     [hipe_rtl:mk_goto(FalseLblName)];
		   _ ->
		     case Args of
		       [Src, Base, Offset] ->
			 CCode = static_int_c_code(NewOffset, Src,
						   Base, Offset, Size,
						   Flags, TrueLblName,
						   FalseLblName),
			 put_unsafe_static_int(NewOffset, Src, Base,
					       Offset, Size,
					       CCode, Aligned, LittleEndian,
					       TrueLblName);
		       [Src, Bits, Base, Offset] ->
			 {SizeCode, SizeReg} =
			   hipe_rtl_binary:make_size(Size, Bits,
						     SystemLimitLblName,
						     FalseLblName),
			 CCode = int_c_code(NewOffset, Src, Base,
					    Offset, SizeReg, Flags,
					    TrueLblName, FalseLblName),
			 InCode =
			   put_unsafe_dynamic_int(NewOffset, Src, Base,
						  Offset, SizeReg, CCode,
						  Aligned, LittleEndian,
						  TrueLblName),
			 SizeCode ++ InCode
		     end
		 end
	     end;

	  bs_utf8_size ->
	    case Dst of
	      [_DstVar] ->
		[_Arg] = Args,
		[hipe_rtl:mk_call(Dst, bs_utf8_size, Args,
				  TrueLblName, [], not_remote)];
	      [] ->
		[hipe_rtl:mk_goto(TrueLblName)]
	    end;

	  bs_put_utf8 ->
	    [_Src, _Base, _Offset] = Args,
	    NewDsts = get_real(Dst),
	    [hipe_rtl:mk_call(NewDsts, bs_put_utf8, Args,
			      TrueLblName, FalseLblName, not_remote)];

	  bs_utf16_size ->
	    case Dst of
	      [_DstVar] ->
		[_Arg] = Args,
		[hipe_rtl:mk_call(Dst, bs_utf16_size, Args,
				  TrueLblName, [], not_remote)];
	      [] ->
		[hipe_rtl:mk_goto(TrueLblName)]
	    end;

	  {bs_put_utf16, Flags} ->
	    [_Src, _Base, _Offset] = Args,
	    NewDsts = get_real(Dst),
	    PrimOp =	% workaround for bif/primop arity restrictions
	      case littleendian(Flags) of
		false -> bs_put_utf16be;
		true -> bs_put_utf16le
	      end,
	    [hipe_rtl:mk_call(NewDsts, PrimOp, Args,
			      TrueLblName, FalseLblName, not_remote)];

	  bs_validate_unicode ->
	    [_Arg] = Args,
	    [hipe_rtl:mk_call([], bs_validate_unicode, Args,
			      TrueLblName, FalseLblName, not_remote)];

	  bs_final ->
	    Zero = hipe_rtl:mk_imm(0),
	    [Src, Offset] = Args,
	    [BitSize, ByteSize] = create_regs(2),
	    [ShortLbl, LongLbl] = create_lbls(2),
	    case Dst of
	      [DstVar] ->
		[hipe_rtl:mk_alub(BitSize, Offset, 'and', ?LOW_BITS, eq,
				  hipe_rtl:label_name(ShortLbl),
				  hipe_rtl:label_name(LongLbl)), ShortLbl,
		 hipe_rtl:mk_move(DstVar, Src),
		 hipe_rtl:mk_goto(TrueLblName),
		 LongLbl,
		 hipe_rtl:mk_alu(ByteSize, Offset, 'srl', ?BYTE_SHIFT),
		 hipe_tagscheme:mk_sub_binary(DstVar, ByteSize,
					      Zero, BitSize, Zero, Src),
		 hipe_rtl:mk_goto(TrueLblName)];
	      [] ->
		[hipe_rtl:mk_goto(TrueLblName)]
	    end;

	  bs_init_writable ->
	    Zero = hipe_rtl:mk_imm(0),
	    [Size] = Args,
	    [DstVar] = Dst,
	    [SizeReg] = create_regs(1),
	    [Base] = create_unsafe_regs(1),
	    [hipe_rtl:mk_gctest(?PROC_BIN_WORDSIZE + ?SUB_BIN_WORDSIZE),
	     get_word_integer(Size, SizeReg, SystemLimitLblName, FalseLblName),
	     allocate_writable(DstVar, Base, SizeReg, Zero, Zero),
	     hipe_rtl:mk_goto(TrueLblName)];

	  {bs_private_append, _U, _F} ->
	    [Size, Bin] = Args,
	    [DstVar, Base, Offset] = Dst,
	    [ProcBin] = create_vars(1),
	    [SubSize, SizeReg, EndSubSize, EndSubBitSize] = create_regs(4),
	    SubBinSize = {sub_binary, binsize},
	    [hipe_tagscheme:get_field_from_term({sub_binary, orig}, Bin, ProcBin),
	     hipe_tagscheme:get_field_from_term(SubBinSize, Bin, SubSize),
	     get_word_integer(Size, SizeReg, SystemLimitLblName, FalseLblName),
	     realloc_binary(SizeReg, ProcBin, Base),
	     calculate_sizes(Bin, SizeReg, Offset, EndSubSize, EndSubBitSize),
	     hipe_tagscheme:set_field_from_term(SubBinSize, Bin, EndSubSize),
	     hipe_tagscheme:set_field_from_term({sub_binary, bitsize}, Bin, EndSubBitSize),
	     hipe_rtl:mk_move(DstVar, Bin),
	     hipe_rtl:mk_goto(TrueLblName)]; 

	  {bs_append, _U, _F, Unit, _Bla} ->
	    [Size, Bin] = Args,
	    [DstVar, Base, Offset] = Dst,
	    [ProcBin] = create_vars(1),
	    [Flags, SizeReg, IsWritable, EndSubSize, EndSubBitSize] =
	      create_regs(5),
	    [ContLbl,ContLbl2,ContLbl3,ContLbl4,WritableLbl,NotWritableLbl] =
	      Lbls = create_lbls(6),
	    [ContLblName, ContLbl2Name, ContLbl3Name, ContLbl4Name,
	     Writable, NotWritable] =
	      [hipe_rtl:label_name(Lbl) || Lbl <- Lbls],
	    Zero = hipe_rtl:mk_imm(0),
	    SubIsWritable = {sub_binary, is_writable},
	    [hipe_rtl:mk_gctest(?SUB_BIN_WORDSIZE + ?PROC_BIN_WORDSIZE),
	     get_word_integer(Size, SizeReg, SystemLimitLblName, FalseLblName),
	     hipe_tagscheme:test_bitstr(Bin, ContLblName, FalseLblName, 0.99),
	     ContLbl,
	     hipe_tagscheme:test_subbinary(Bin,ContLbl2Name, NotWritable),
	     ContLbl2,
	     hipe_tagscheme:get_field_from_term(SubIsWritable, Bin, IsWritable),
	     hipe_rtl:mk_branch(IsWritable, 'ne', Zero,
				ContLbl3Name, NotWritable),
	     ContLbl3,
	     hipe_tagscheme:get_field_from_term({sub_binary, orig}, Bin, ProcBin),
	     hipe_tagscheme:get_field_from_term({proc_bin, flags}, ProcBin, Flags),
	     hipe_rtl:mk_alub(Flags, Flags, 'and',
			      hipe_rtl:mk_imm(?PB_IS_WRITABLE),
			      eq, NotWritable, ContLbl4Name, 0.01),
	     ContLbl4,
	     calculate_sizes(Bin, SizeReg, Offset, EndSubSize, EndSubBitSize),
	     is_divisible(Offset, Unit, Writable, FalseLblName),
	     WritableLbl,
	     hipe_tagscheme:set_field_from_term(SubIsWritable, Bin, Zero),
	     realloc_binary(SizeReg, ProcBin, Base),
	     hipe_tagscheme:mk_sub_binary(DstVar, EndSubSize, Zero,
					  EndSubBitSize, Zero,
					  hipe_rtl:mk_imm(1), ProcBin),
	     hipe_rtl:mk_goto(TrueLblName),
	     NotWritableLbl,
	     not_writable_code(Bin, SizeReg, DstVar, Base, Offset, Unit,
			       TrueLblName, FalseLblName)]
	end,
      {Code, ConstTab}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Code that is used in the append and init writeable functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_writable_code(Bin, SizeReg, Dst, Base, Offset, Unit,
		  TrueLblName, FalseLblName) ->
  [SrcBase] = create_unsafe_regs(1),
  [SrcOffset, SrcSize, TotSize, TotBytes, UsedBytes] = create_regs(5),
  [IncLbl,AllLbl] = Lbls = create_lbls(2),
  [IncLblName,AllLblName] = get_label_names(Lbls),
  [get_base_offset_size(Bin, SrcBase, SrcOffset, SrcSize, FalseLblName),
   hipe_rtl:mk_alu(TotSize, SrcSize, add, SizeReg),
   hipe_rtl:mk_alu(TotBytes, TotSize, add, ?LOW_BITS),
   hipe_rtl:mk_alu(TotBytes, TotBytes, srl, ?BYTE_SHIFT),
   hipe_rtl:mk_alu(UsedBytes, TotBytes, sll, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_branch(UsedBytes, geu, hipe_rtl:mk_imm(256),
		      AllLblName, IncLblName),
   IncLbl,
   hipe_rtl:mk_move(UsedBytes, hipe_rtl:mk_imm(256)),
   AllLbl,
   allocate_writable(Dst, Base, UsedBytes, TotBytes, TotSize),
   put_binary_all(Offset, Bin, Base, hipe_rtl:mk_imm(0), Unit,
		  TrueLblName, FalseLblName)].

allocate_writable(Dst, Base, UsedBytes, TotBytes, TotSize) ->
  Zero = hipe_rtl:mk_imm(0),
  [NextLbl] = create_lbls(1),
  [EndSubSize, EndSubBitSize, ProcBin] = create_regs(3),
  [hipe_rtl:mk_call([Base], bs_allocate, [UsedBytes],
		    hipe_rtl:label_name(NextLbl), [], not_remote),
   NextLbl,
   hipe_tagscheme:create_refc_binary(Base, TotBytes, 
				     hipe_rtl:mk_imm(?PB_IS_WRITABLE bor
						     ?PB_ACTIVE_WRITER),
				     ProcBin),
   hipe_rtl:mk_alu(EndSubSize, TotSize, srl, ?BYTE_SHIFT),
   hipe_rtl:mk_alu(EndSubBitSize, TotSize, 'and', ?LOW_BITS),
   hipe_tagscheme:mk_sub_binary(Dst, EndSubSize, Zero, EndSubBitSize,
				Zero, hipe_rtl:mk_imm(1), ProcBin)].

realloc_binary(SizeReg, ProcBin, Base) ->
  [NoReallocLbl, ReallocLbl, NextLbl, ContLbl] = Lbls = create_lbls(4),
  [NoReallocLblName, ReallocLblName, NextLblName, ContLblName] =
    [hipe_rtl:label_name(Lbl) || Lbl <- Lbls],
  [PBSize, Tmp, ByteSize, NewSize, Flags, ResultingSize, OrigSize,
   BinPointer] = create_regs(8),
  ProcBinSizeTag = {proc_bin, binsize},
  ProcBinFlagsTag = {proc_bin, flags},
  ProcBinValTag = {proc_bin, val},
  ProcBinBytesTag = {proc_bin, bytes},
  BinOrigSizeTag = {binary, orig_size},
  [hipe_tagscheme:get_field_from_term(ProcBinSizeTag, ProcBin, PBSize),
   hipe_rtl:mk_alu(Tmp, SizeReg, 'add', ?LOW_BITS),
   hipe_rtl:mk_alu(ByteSize, Tmp, 'srl', ?BYTE_SHIFT),
   hipe_rtl:mk_alu(ResultingSize, ByteSize, 'add', PBSize),
   hipe_tagscheme:set_field_from_term(ProcBinSizeTag, ProcBin, ResultingSize),
   hipe_tagscheme:get_field_from_term(ProcBinFlagsTag, ProcBin, Flags),
   hipe_rtl:mk_alu(Flags, Flags, 'or', hipe_rtl:mk_imm(?PB_ACTIVE_WRITER)),
   hipe_tagscheme:set_field_from_term(ProcBinFlagsTag, ProcBin, Flags),
   hipe_tagscheme:get_field_from_term(ProcBinValTag, ProcBin, BinPointer),
   hipe_tagscheme:get_field_from_pointer(BinOrigSizeTag, BinPointer, OrigSize),
   hipe_rtl:mk_branch(OrigSize, 'ltu', ResultingSize,
		      ReallocLblName, NoReallocLblName),
   NoReallocLbl,
   hipe_tagscheme:get_field_from_term(ProcBinBytesTag, ProcBin, Base),
   hipe_rtl:mk_goto(ContLblName),
   ReallocLbl,
   hipe_rtl:mk_alu(NewSize, ResultingSize, 'sll', hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_call([BinPointer], bs_reallocate, [BinPointer, NewSize],
		    NextLblName, [], not_remote),
   NextLbl,
   hipe_tagscheme:set_field_from_pointer(BinOrigSizeTag, BinPointer, NewSize),
   hipe_tagscheme:set_field_from_term(ProcBinValTag, ProcBin, BinPointer),
   hipe_tagscheme:extract_binary_bytes(BinPointer, Base),
   hipe_tagscheme:set_field_from_term(ProcBinBytesTag, ProcBin, Base),
   ContLbl].

calculate_sizes(Bin, SizeReg, Offset, EndSubSize, EndSubBitSize) ->
  [SubSize, SubBitSize, EndSize] = create_regs(3),
  [hipe_tagscheme:get_field_from_term({sub_binary, binsize}, Bin, SubSize),
   hipe_tagscheme:get_field_from_term({sub_binary, bitsize}, Bin, SubBitSize),
   hipe_rtl:mk_alu(Offset, SubSize, 'sll', ?BYTE_SHIFT),
   hipe_rtl:mk_alu(Offset, Offset, 'add', SubBitSize),
   hipe_rtl:mk_alu(EndSize, Offset, 'add', SizeReg),
   hipe_rtl:mk_alu(EndSubSize, EndSize, srl, ?BYTE_SHIFT),
   hipe_rtl:mk_alu(EndSubBitSize, EndSize, 'and', ?LOW_BITS)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Code that is used to create calls to beam functions
%%
%%  X_c_code/8, used for putting terms into binaries
%%
%%  X_get_c_code/10, used for getting terms from binaries
%%
%% - gen_test_sideffect_bs_call/4 is used to make a C-call that might
%%       fail but doesn't return an erlang value.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

static_float_c_code(NewOffset, Src, Base, Offset, Size, Flags,
		    TrueLblName, FalseLblName) ->
  [SizeReg] = create_regs(1),
  [hipe_rtl:mk_move(SizeReg, hipe_rtl:mk_imm(Size))|
   float_c_code(NewOffset, Src, Base, Offset, SizeReg, Flags,
		TrueLblName, FalseLblName)].

float_c_code(NewOffset, Src, Base, Offset, SizeReg, Flags,
	     TrueLblName, FalseLblName) ->
  put_c_code(bs_put_small_float, NewOffset, Src, Base, Offset, SizeReg, 
	     Flags, TrueLblName, FalseLblName).

static_int_c_code(NewOffset, Src, Base, Offset, Size, Flags,
		  TrueLblName, FalseLblName) ->
  [SizeReg] = create_regs(1),
  [hipe_rtl:mk_move(SizeReg, hipe_rtl:mk_imm(Size))|
   int_c_code(NewOffset, Src, Base, Offset, SizeReg, Flags,
	      TrueLblName, FalseLblName)].

int_c_code(NewOffset, Src, Base, Offset, SizeReg, Flags,
	   TrueLblName, FalseLblName) ->
  put_c_code(bs_put_big_integer, NewOffset, Src, Base, Offset, SizeReg,
	     Flags, TrueLblName, FalseLblName).

binary_c_code(NewOffset, Src, Base, Offset, Size, TrueLblName) ->
  PassedLbl = hipe_rtl:mk_new_label(),
  [SizeReg, FlagsReg] = create_regs(2),
  [hipe_rtl:mk_move(FlagsReg, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_move(SizeReg, Size),
   hipe_rtl:mk_call([], bs_put_bits, [Src, SizeReg, Base, Offset, FlagsReg],
		    hipe_rtl:label_name(PassedLbl), [], not_remote),
   PassedLbl,
   hipe_rtl:mk_alu(NewOffset, Offset, add, SizeReg),
   hipe_rtl:mk_goto(TrueLblName)].

put_c_code(Func, NewOffset, Src, Base, Offset, SizeReg, Flags,
	   TrueLblName, FalseLblName) ->
  PassedLbl = hipe_rtl:mk_new_label(),
  [FlagsReg] = create_regs(1),
  [hipe_rtl:mk_move(FlagsReg, hipe_rtl:mk_imm(Flags)),
   gen_test_sideffect_bs_call(Func, [Src, SizeReg, Base, Offset, FlagsReg],
			      hipe_rtl:label_name(PassedLbl), FalseLblName),
   PassedLbl,
   hipe_rtl:mk_alu(NewOffset, Offset, add, SizeReg),
   hipe_rtl:mk_goto(TrueLblName)].

gen_test_sideffect_bs_call(Name, Args, TrueLblName, FalseLblName) ->
  [Tmp1] = create_regs(1),
  RetLbl = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_call([Tmp1], Name, Args,
		    hipe_rtl:label_name(RetLbl), [], not_remote),
   RetLbl,
   hipe_rtl:mk_branch(Tmp1, eq, hipe_rtl:mk_imm(0),
		      FalseLblName, TrueLblName, 0.01)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Small utility functions:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_regs(X) when X > 0 ->
  [hipe_rtl:mk_new_reg_gcsafe()|create_regs(X-1)];
create_regs(0) ->
  [].

create_unsafe_regs(X) when X > 0 ->
  [hipe_rtl:mk_new_reg()|create_unsafe_regs(X-1)];
create_unsafe_regs(0) ->
  [].

create_vars(X) when X > 0 ->
  [hipe_rtl:mk_new_var()|create_vars(X-1)];
create_vars(0) ->
  [].

create_lbls(X) when X > 0 ->
  [hipe_rtl:mk_new_label()|create_lbls(X-1)];
create_lbls(0) ->
  [].

get_label_names(Lbls) ->
  [hipe_rtl:label_name(Lbl) || Lbl <- Lbls].

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

is_illegal_const(Const) ->
  Const >= (1 bsl (hipe_rtl_arch:word_size() * ?BYTE_SIZE)) orelse Const < 0.

get_real(Dst) ->
  case Dst of
    [_NewOffset] -> Dst;
    [] -> create_regs(1)
  end.

%%-----------------------------------------------------------------------------
%% Help functions implementing the bs operations in rtl code.
%%
%% The following functions are called from the translation switch:
%%
%% - put_string/7 creates code to copy a string to a binary
%%       starting at base+offset and ending at base+newoffset
%%
%% - const_init2/6 initializes the creation of a binary of constant size
%%
%% - var_init2/6 initializes the creation of a binary of variable size
%%
%% - get_int_from_unaligned_bin/11 creates code to extract a fixed
%%       size integer from a binary or makes a c-call if it does not
%%       conform to some certain rules.
%%
%% - get_unknown_size_int/11 creates code to extract a variable size
%%       byte-aligned integer from a binary or makes a c-call if it
%%       does not conform to some certain rules.
%%
%% - skip_no_of_bits/5 creates code to skip a variable amount of bits
%%       in a binary.
%%
%% - load_match_buffer/7 reloads the C-matchbuffer to RTL registers.
%%
%% - expand_runtime/4 creates code that calculates a maximal heap need
%%       before a binary match
%%-----------------------------------------------------------------------------

put_string(NewOffset, ConstTab, String, SizeInBytes, Base, Offset, TLName) ->
  [StringBase] = create_regs(1),
  {NewTab, Lbl} = hipe_consttab:insert_block(ConstTab, byte, String),
  {[hipe_rtl:mk_load_address(StringBase, Lbl, constant)|
    copy_string(StringBase, SizeInBytes, Base, Offset, NewOffset, TLName)],
   NewTab}.

const_init2(Size, Dst, Base, Offset, TrueLblName) ->
  Log2WordSize = hipe_rtl_arch:log2_word_size(),
  WordSize = hipe_rtl_arch:word_size(),
  NextLbl = hipe_rtl:mk_new_label(),
  case Size =< ?MAX_HEAP_BIN_SIZE of
    true ->
      [hipe_rtl:mk_gctest(((Size + 3*WordSize-1) bsr Log2WordSize)+?SUB_BIN_WORDSIZE),
       hipe_tagscheme:create_heap_binary(Base, Size, Dst),
       hipe_rtl:mk_move(Offset, hipe_rtl:mk_imm(0)),
       hipe_rtl:mk_goto(TrueLblName)];
    false ->
      ByteSize = hipe_rtl:mk_new_reg(),
      [hipe_rtl:mk_gctest(?PROC_BIN_WORDSIZE+?SUB_BIN_WORDSIZE),
       hipe_rtl:mk_move(Offset, hipe_rtl:mk_imm(0)),
       hipe_rtl:mk_move(ByteSize, hipe_rtl:mk_imm(Size)),
       hipe_rtl:mk_call([Base], bs_allocate, [ByteSize],
			hipe_rtl:label_name(NextLbl), [], not_remote),
       NextLbl,
       hipe_tagscheme:create_refc_binary(Base, ByteSize, Dst),
       hipe_rtl:mk_goto(TrueLblName)]
  end.

const_init_bits(Size, Dst, Base, Offset, TrueLblName) ->
  Log2WordSize = hipe_rtl_arch:log2_word_size(),
  WordSize = hipe_rtl_arch:word_size(),
  [NextLbl] = create_lbls(1),
  TmpDst = hipe_rtl:mk_new_var(),
  Zero = hipe_rtl:mk_imm(0),
  {ExtraSpace, SubBinCode} =
    case (Size rem ?BYTE_SIZE) =:= 0 of
      true ->
	{0, [hipe_rtl:mk_move(Dst, TmpDst)]};
      false ->
	{?SUB_BIN_WORDSIZE,
	 hipe_tagscheme:mk_sub_binary(Dst, hipe_rtl:mk_imm(Size bsr 3), Zero,
				      hipe_rtl:mk_imm(Size band ?LOW_BITS_INT),
				      Zero, TmpDst)}
    end,
  BaseBinCode =
    case Size =< (?MAX_HEAP_BIN_SIZE * 8) of
      true ->
	ByteSize = (Size + 7) div 8,
	[hipe_rtl:mk_gctest(((ByteSize + 3*WordSize-1) bsr Log2WordSize) + ExtraSpace),
	 hipe_tagscheme:create_heap_binary(Base, ByteSize, TmpDst),
	 hipe_rtl:mk_move(Offset, Zero)];
      false ->
	ByteSize = hipe_rtl:mk_new_reg(),
	[hipe_rtl:mk_gctest(?PROC_BIN_WORDSIZE+ExtraSpace),
	 hipe_rtl:mk_move(Offset, Zero),
	 hipe_rtl:mk_move(ByteSize, hipe_rtl:mk_imm((Size+7) bsr 3)),
	 hipe_rtl:mk_call([Base], bs_allocate, [ByteSize],
			  hipe_rtl:label_name(NextLbl), [], not_remote),
	 NextLbl,
	 hipe_tagscheme:create_refc_binary(Base, ByteSize, TmpDst)]
    end,
  [BaseBinCode, SubBinCode, hipe_rtl:mk_goto(TrueLblName)].

var_init2(Size, Dst, Base, Offset, TrueLblName, SystemLimitLblName, FalseLblName) ->
  Log2WordSize = hipe_rtl_arch:log2_word_size(),
  WordSize = hipe_rtl_arch:word_size(),
  [ContLbl, HeapLbl, REFCLbl, NextLbl] = create_lbls(4),
  [USize, Tmp] = create_unsafe_regs(2),
  [get_word_integer(Size, USize, SystemLimitLblName, FalseLblName),
   hipe_rtl:mk_branch(USize, leu, hipe_rtl:mk_imm(?MAX_BINSIZE),
		      hipe_rtl:label_name(ContLbl),
		      SystemLimitLblName),
   ContLbl,
   hipe_rtl:mk_move(Offset, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_branch(USize, leu, hipe_rtl:mk_imm(?MAX_HEAP_BIN_SIZE),
		      hipe_rtl:label_name(HeapLbl),
		      hipe_rtl:label_name(REFCLbl)),
   HeapLbl,
   hipe_rtl:mk_alu(Tmp, USize, add, hipe_rtl:mk_imm(3*WordSize-1)),
   hipe_rtl:mk_alu(Tmp, Tmp, srl, hipe_rtl:mk_imm(Log2WordSize)),
   hipe_rtl:mk_alu(Tmp, Tmp, add, hipe_rtl:mk_imm(?SUB_BIN_WORDSIZE)),
   hipe_rtl:mk_gctest(Tmp),
   hipe_tagscheme:create_heap_binary(Base, USize, Dst),
   hipe_rtl:mk_goto(TrueLblName),
   REFCLbl,
   hipe_rtl:mk_gctest(?PROC_BIN_WORDSIZE+?SUB_BIN_WORDSIZE),
   hipe_rtl:mk_call([Base], bs_allocate, [USize],
		    hipe_rtl:label_name(NextLbl), [], not_remote),
   NextLbl,
   hipe_tagscheme:create_refc_binary(Base, USize, Dst),
   hipe_rtl:mk_goto(TrueLblName)].

var_init_bits(Size, Dst, Base, Offset, TrueLblName, SystemLimitLblName, FalseLblName) ->
  [HeapLbl, REFCLbl, NextLbl, NoSubLbl, SubLbl,
   NoCreateSubBin, CreateSubBin, JoinLbl, JoinLbl2] = create_lbls(9),
  [USize, ByteSize, TotByteSize, OffsetBits] = create_regs(4),
  [TmpDst] = create_unsafe_regs(1),
  Log2WordSize = hipe_rtl_arch:log2_word_size(),
  WordSize = hipe_rtl_arch:word_size(),
  MaximumWords =
    erlang:max((?MAX_HEAP_BIN_SIZE + 3*WordSize) bsr Log2WordSize,
	       ?PROC_BIN_WORDSIZE) + ?SUB_BIN_WORDSIZE,
  Zero = hipe_rtl:mk_imm(0),
  [hipe_rtl:mk_gctest(MaximumWords),
   get_word_integer(Size, USize, SystemLimitLblName, FalseLblName),
   hipe_rtl:mk_alu(ByteSize, USize, srl, ?BYTE_SHIFT),
   hipe_rtl:mk_alub(OffsetBits, USize, 'and', ?LOW_BITS, eq,
		    hipe_rtl:label_name(NoSubLbl),
		    hipe_rtl:label_name(SubLbl)),
   NoSubLbl,
   hipe_rtl:mk_move(TotByteSize, ByteSize),
   hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLbl)),
   SubLbl,
   hipe_rtl:mk_alu(TotByteSize, ByteSize, 'add', hipe_rtl:mk_imm(1)),
   JoinLbl,
   hipe_rtl:mk_branch(TotByteSize, 'leu', hipe_rtl:mk_imm(?MAX_HEAP_BIN_SIZE),
		      hipe_rtl:label_name(HeapLbl),
		      hipe_rtl:label_name(REFCLbl)),
   HeapLbl,
   hipe_tagscheme:create_heap_binary(Base, TotByteSize, TmpDst),
   hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLbl2)),
   REFCLbl,
   hipe_rtl:mk_call([Base], bs_allocate, [TotByteSize],
		    hipe_rtl:label_name(NextLbl), [], not_remote),
   NextLbl,
   hipe_tagscheme:create_refc_binary(Base, TotByteSize, TmpDst),
   JoinLbl2,
   hipe_rtl:mk_move(Offset, Zero),
   hipe_rtl:mk_branch(OffsetBits, 'eq', Zero,
		      hipe_rtl:label_name(NoCreateSubBin),
		      hipe_rtl:label_name(CreateSubBin)),
   CreateSubBin,
   hipe_tagscheme:mk_sub_binary(Dst, ByteSize, Zero, OffsetBits, Zero, TmpDst),
   hipe_rtl:mk_goto(TrueLblName),
   NoCreateSubBin,
   hipe_rtl:mk_move(Dst, TmpDst),
   hipe_rtl:mk_goto(TrueLblName)].

put_binary_all(NewOffset, Src, Base, Offset, Unit, TLName, FLName) ->
  [SrcBase, SrcOffset, NumBits] = create_regs(3),
  [ContLbl] = create_lbls(1),
  CCode = binary_c_code(NewOffset, Src, Base, Offset, NumBits, TLName),
  AlignedCode = copy_aligned_bytes(SrcBase, SrcOffset, NumBits, Base, Offset,
				   NewOffset, TLName),
  [get_base_offset_size(Src, SrcBase, SrcOffset, NumBits,FLName),
   is_divisible(NumBits, Unit, hipe_rtl:label_name(ContLbl), FLName),
   ContLbl
   |test_alignment(SrcOffset, NumBits, Offset, AlignedCode, CCode)].

test_alignment(SrcOffset, NumBits, Offset, AlignedCode, CCode) ->
  [Tmp] = create_regs(1),
  [AlignedLbl, CLbl] = create_lbls(2),
   [hipe_rtl:mk_alu(Tmp, SrcOffset, 'or', NumBits),
   hipe_rtl:mk_alu(Tmp, Tmp, 'or', Offset),
   hipe_rtl:mk_alub(Tmp, Tmp, 'and', ?LOW_BITS, 'eq',
		    hipe_rtl:label_name(AlignedLbl),
		    hipe_rtl:label_name(CLbl)),
   AlignedLbl,
   AlignedCode,
   CLbl,
   CCode].
 
put_static_binary(NewOffset, Src, Size, Base, Offset, TLName, FLName) ->
  [SrcBase] = create_unsafe_regs(1),
  [SrcOffset, SrcSize] = create_regs(2),
    case Size of
      0 ->
	get_base_offset_size(Src, SrcBase, SrcOffset, SrcSize, FLName) ++
	[hipe_rtl:mk_move(NewOffset, Offset),
	 hipe_rtl:mk_goto(TLName)];
      _ ->
	SizeImm = hipe_rtl:mk_imm(Size),
	CCode = binary_c_code(NewOffset, Src, Base, Offset, SizeImm, TLName),
	AlignedCode = copy_aligned_bytes(SrcBase, SrcOffset, SizeImm, Base,
					 Offset, NewOffset, TLName),
	get_base_offset_size(Src, SrcBase, SrcOffset, SrcSize, FLName) ++
	  small_check(SizeImm, SrcSize, FLName) ++
	  test_alignment(SrcOffset, SizeImm, Offset, AlignedCode, CCode)
     end.

put_dynamic_binary(NewOffset, Src, SizeReg, Base, Offset, TLName, FLName) ->
  [SrcBase] = create_unsafe_regs(1), 
  [SrcOffset, SrcSize] = create_regs(2),
  CCode = binary_c_code(NewOffset, Src, Base, Offset, SizeReg, TLName),
  AlignedCode = copy_aligned_bytes(SrcBase, SrcOffset, SizeReg, Base, Offset,
				   NewOffset, TLName),
  get_base_offset_size(Src, SrcBase, SrcOffset, SrcSize, FLName) ++
    small_check(SizeReg, SrcSize, FLName) ++
    test_alignment(SrcOffset, SizeReg, Offset, AlignedCode, CCode).

put_float(NewOffset, Src, Base, Offset, 64, CCode, Aligned, LittleEndian,
	  ConstInfo, TrueLblName) ->
  [CLbl] = create_lbls(1),
  case {Aligned, LittleEndian} of
    {true, false} ->
      copy_float_big(Base, Offset, NewOffset, Src,
		     hipe_rtl:label_name(CLbl), TrueLblName, ConstInfo) ++
	[CLbl|CCode];
    {true, true} ->
      copy_float_little(Base, Offset, NewOffset, Src,
			hipe_rtl:label_name(CLbl), TrueLblName, ConstInfo) ++
	[CLbl|CCode];
    {false, _} ->
      CCode
  end;
put_float(_NewOffset, _Src, _Base, _Offset, _Size, CCode, _Aligned,
	  _LittleEndian, _ConstInfo, _TrueLblName) ->
  CCode.

put_static_int(NewOffset, Src, Base, Offset, Size, CCode, Aligned, 
	       LittleEndian, TrueLblName) ->
  {Init, End, UntaggedSrc} = make_init_end(Src, CCode, TrueLblName),
  case {Aligned, LittleEndian} of
    {true, true} ->
      Init ++
	copy_int_little(Base, Offset, NewOffset, Size, UntaggedSrc) ++
	End;
    {true, false} ->
      Init ++
	copy_int_big(Base, Offset, NewOffset, Size, UntaggedSrc) ++
	End;
    {false, true} ->
      CCode;
    {false, false} ->
      Init ++
	copy_offset_int_big(Base, Offset, NewOffset, Size, UntaggedSrc) ++
	End
  end.

put_unsafe_static_int(NewOffset, Src, Base, Offset, Size, CCode, Aligned,
		      LittleEndian, TrueLblName) ->
  {Init, End, UntaggedSrc} = make_init_end(Src, TrueLblName),
  case {Aligned, LittleEndian} of
    {true, true} ->
      Init ++
	copy_int_little(Base, Offset, NewOffset, Size, UntaggedSrc) ++
	End;
    {true, false} ->
      Init ++
	copy_int_big(Base, Offset, NewOffset, Size, UntaggedSrc) ++
	End;
    {false, true} ->
      CCode;
    {false, false} ->
      Init ++
	copy_offset_int_big(Base, Offset, NewOffset, Size, UntaggedSrc) ++
	End
  end.

put_dynamic_int(NewOffset, Src, Base, Offset, SizeReg, CCode, Aligned, 
		LittleEndian, TrueLblName) ->
  {Init, End, UntaggedSrc} = make_init_end(Src, CCode, TrueLblName),
  case Aligned of
    true ->
      case LittleEndian of
	true ->
	  Init ++
	    copy_int_little(Base, Offset, NewOffset, SizeReg, UntaggedSrc) ++
	    End;
	false ->
	  Init ++
	    copy_int_big(Base, Offset, NewOffset, SizeReg, UntaggedSrc) ++
	    End
	end;
    false ->
      CCode
  end.

put_unsafe_dynamic_int(NewOffset, Src, Base, Offset, SizeReg, CCode, Aligned, 
		       LittleEndian, TrueLblName) ->
  {Init, End, UntaggedSrc} = make_init_end(Src, TrueLblName),
  case Aligned of
    true ->
      case LittleEndian of
	true ->
	  Init ++
	    copy_int_little(Base, Offset, NewOffset, SizeReg, UntaggedSrc) ++
	    End;
	false ->
	  Init ++
	    copy_int_big(Base, Offset, NewOffset, SizeReg, UntaggedSrc) ++
	    End
	end;
    false ->
      CCode
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Help functions used by the above
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_init_end(Src, CCode, TrueLblName) ->
  [CLbl, SuccessLbl] = create_lbls(2),
  [UntaggedSrc] = create_regs(1),
  Init = [hipe_tagscheme:test_fixnum(Src, hipe_rtl:label_name(SuccessLbl),
				     hipe_rtl:label_name(CLbl), 0.99),
	  SuccessLbl,
	  hipe_tagscheme:untag_fixnum(UntaggedSrc,Src)],
  End = [hipe_rtl:mk_goto(TrueLblName), CLbl| CCode],
  {Init, End, UntaggedSrc}.

make_init_end(Src, TrueLblName) ->
  [UntaggedSrc] = create_regs(1),
  Init = [hipe_tagscheme:untag_fixnum(UntaggedSrc,Src)],
  End = [hipe_rtl:mk_goto(TrueLblName)],
  {Init, End, UntaggedSrc}.

get_base_offset_size(Binary, SrcBase, SrcOffset, SrcSize, FLName) ->
  [JoinLbl, EndLbl, SuccessLbl, SubLbl, OtherLbl, HeapLbl, REFCLbl] =
    Lbls = create_lbls(7),
  [JoinLblName, EndLblName, SuccessLblName, SubLblName,
   OtherLblName, HeapLblName, REFCLblName] = get_label_names(Lbls),
  [BitSize, BitOffset] = create_regs(2),
  [Orig] = create_vars(1),
  [hipe_tagscheme:test_bitstr(Binary, SuccessLblName, FLName, 0.99),
   SuccessLbl,
   hipe_tagscheme:get_field_from_term({sub_binary,binsize}, Binary, SrcSize),
   hipe_rtl:mk_alu(SrcSize, SrcSize, sll, ?BYTE_SHIFT),
   hipe_tagscheme:test_subbinary(Binary, SubLblName, OtherLblName),
   SubLbl,
   hipe_tagscheme:get_field_from_term({sub_binary,bitsize}, Binary, BitSize),
   hipe_tagscheme:get_field_from_term({sub_binary,offset}, Binary, SrcOffset),
   hipe_rtl:mk_alu(SrcSize, SrcSize, add, BitSize),
   hipe_tagscheme:get_field_from_term({sub_binary,bitoffset}, Binary, BitOffset),
   hipe_rtl:mk_alu(SrcOffset, SrcOffset, sll, ?BYTE_SHIFT),
   hipe_rtl:mk_alu(SrcOffset, SrcOffset, add, BitOffset),
   hipe_tagscheme:get_field_from_term({sub_binary,orig}, Binary, Orig),
   hipe_rtl:mk_goto(JoinLblName),
   OtherLbl,
   hipe_rtl:mk_move(SrcOffset, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_move(Orig, Binary),
   JoinLbl,
   hipe_tagscheme:test_heap_binary(Orig, HeapLblName, REFCLblName),
   HeapLbl,
   hipe_rtl:mk_alu(SrcBase, Orig, add, hipe_rtl:mk_imm(?HEAP_BIN_DATA-2)),
   hipe_rtl:mk_goto(EndLblName),
   REFCLbl,
   hipe_tagscheme:get_field_from_term({proc_bin,bytes}, Orig, SrcBase),
   EndLbl].

copy_aligned_bytes(CopyBase, CopyOffset, Size, Base, Offset, NewOffset, TrueLblName) ->
  [BaseDst, BaseSrc] = create_unsafe_regs(2),
  [Iter, Extra, BothOffset] = create_regs(3),
  initializations(BaseSrc, BaseDst, BothOffset, CopyOffset, Offset, CopyBase, Base) ++
    [hipe_rtl:mk_alu(Extra, Size, 'and', ?LOW_BITS),
     hipe_rtl:mk_alu(Iter, Size, srl, ?BYTE_SHIFT),
     hipe_rtl:mk_alu(NewOffset, Offset, 'add', Size)] ++
    easy_loop(BaseSrc, BaseDst, BothOffset, Iter, Extra, TrueLblName).

copy_string(StringBase, StringSize, BinBase, BinOffset, NewOffset, TrueLblName) ->
  [TmpOffset,BothOffset,InitOffs] = create_regs(3),
  [NewBinBase] = create_unsafe_regs(1),
  [EasyLbl, HardLbl] = create_lbls(2),
  [hipe_rtl:mk_alu(TmpOffset, BinOffset, srl, ?BYTE_SHIFT),
   hipe_rtl:mk_alu(NewBinBase, BinBase, add, TmpOffset),
   hipe_rtl:mk_move(BothOffset, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_alub(InitOffs, BinOffset, 'and', ?LOW_BITS, eq,
		    hipe_rtl:label_name(EasyLbl), hipe_rtl:label_name(HardLbl)),
   EasyLbl,
   hipe_rtl:mk_alu(NewOffset, BinOffset, add,
		   hipe_rtl:mk_imm(?bytes_to_bits(StringSize)))] ++
   easy_loop(StringBase, NewBinBase, BothOffset,
	     hipe_rtl:mk_imm(StringSize), hipe_rtl:mk_imm(0), TrueLblName) ++
    [HardLbl,
     hipe_rtl:mk_alu(NewOffset, BinOffset, add,
		     hipe_rtl:mk_imm(?bytes_to_bits(StringSize)))] ++
    hard_loop(StringBase, NewBinBase, BothOffset, hipe_rtl:mk_imm(StringSize),
	      InitOffs, TrueLblName).

small_check(SizeVar, CopySize, FalseLblName) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_branch(SizeVar, leu, CopySize,
		      hipe_rtl:label_name(SuccessLbl), FalseLblName),
   SuccessLbl].

easy_loop(BaseSrc, BaseDst, BothOffset, Iterations, Extra, TrueLblName) ->
  [Tmp1, Shift] = create_regs(2),
  [LoopLbl, TopLbl, EndLbl, ExtraLbl] = create_lbls(4),
  [TopLbl,
   hipe_rtl:mk_branch(BothOffset, ne, Iterations, hipe_rtl:label_name(LoopLbl),
		      hipe_rtl:label_name(EndLbl), 0.99),
   LoopLbl,
   hipe_rtl:mk_load(Tmp1, BaseSrc, BothOffset, byte, unsigned),
   hipe_rtl:mk_store(BaseDst, BothOffset, Tmp1, byte),
   hipe_rtl:mk_alu(BothOffset, BothOffset, add, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(TopLbl)),
   EndLbl,
   hipe_rtl:mk_branch(Extra, eq, hipe_rtl:mk_imm(0), TrueLblName,
		      hipe_rtl:label_name(ExtraLbl)),
   ExtraLbl,
   hipe_rtl:mk_load(Tmp1, BaseSrc, BothOffset, byte, unsigned),
   hipe_rtl:mk_alu(Shift, hipe_rtl:mk_imm(?BYTE_SIZE), sub, Extra),
   hipe_rtl:mk_alu(Tmp1, Tmp1, srl, Shift),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sll, Shift),
   hipe_rtl:mk_store(BaseDst, BothOffset, Tmp1, byte),
   hipe_rtl:mk_goto(TrueLblName)].

hard_loop(BaseSrc, BaseDst, BothOffset, Iterations,
	  InitOffset, TrueLblName) ->
  [Tmp1, Tmp2, OldByte, NewByte, SaveByte] = create_regs(5),
  [LoopLbl, EndLbl, TopLbl] = create_lbls(3),
  [hipe_rtl:mk_load(OldByte, BaseDst, BothOffset, byte, unsigned),
   hipe_rtl:mk_alu(Tmp1, hipe_rtl:mk_imm(?BYTE_SIZE), sub, InitOffset),
   TopLbl,
   hipe_rtl:mk_branch(BothOffset, ne, Iterations,
		      hipe_rtl:label_name(LoopLbl),
		      hipe_rtl:label_name(EndLbl)),
   LoopLbl,
   hipe_rtl:mk_load(NewByte, BaseSrc, BothOffset, byte, unsigned),
   hipe_rtl:mk_alu(Tmp2, NewByte, srl, InitOffset),
   hipe_rtl:mk_alu(SaveByte, OldByte, 'or', Tmp2),
   hipe_rtl:mk_store(BaseDst, BothOffset, SaveByte, byte),
   hipe_rtl:mk_alu(OldByte, NewByte, sll, Tmp1),
   hipe_rtl:mk_alu(BothOffset, BothOffset, 'add', hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(TopLbl)),
   EndLbl,
   hipe_rtl:mk_store(BaseDst, BothOffset, OldByte, byte),
   hipe_rtl:mk_goto(TrueLblName)].

initializations(BaseTmp1, BaseTmp2, BothOffset, CopyOffset, Offset, CopyBase, Base) ->
  [OffsetTmp1,OffsetTmp2] = create_regs(2),
  [hipe_rtl:mk_alu(OffsetTmp1, CopyOffset, srl, ?BYTE_SHIFT),
   hipe_rtl:mk_alu(OffsetTmp2, Offset, srl, ?BYTE_SHIFT),
   hipe_rtl:mk_alu(BaseTmp1, CopyBase, add, OffsetTmp1),
   hipe_rtl:mk_alu(BaseTmp2, Base, add, OffsetTmp2),
   hipe_rtl:mk_move(BothOffset, hipe_rtl:mk_imm(0))].

copy_int_little(Base, Offset, NewOffset, Size, Tmp1) when is_integer(Size) ->
  [Tmp2,TmpOffset] = create_regs(2),
  ByteSize = Size div ?BYTE_SIZE,
  [hipe_rtl:mk_alu(TmpOffset, Offset, srl, ?BYTE_SHIFT),
   hipe_rtl:mk_alu(Tmp2, hipe_rtl:mk_imm(ByteSize), 'add', TmpOffset)] ++

    little_loop(Tmp1, Tmp2, TmpOffset, Base) ++

    case Size band 7 of
      0 ->
	[hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(Size))];
      Bits ->
	[hipe_rtl:mk_alu(Tmp1, Tmp1, sll, hipe_rtl:mk_imm(?BYTE_SIZE-Bits)),
	 hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
	 hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(Size))]
    end;
copy_int_little(Base, Offset, NewOffset, Size, Tmp1) ->
  [Tmp2, Tmp3, Tmp4, TmpOffset] = create_regs(4), 
  [hipe_rtl:mk_alu(Tmp2, Size, srl, ?BYTE_SHIFT),
   hipe_rtl:mk_alu(TmpOffset, Offset, srl, ?BYTE_SHIFT),
   hipe_rtl:mk_alu(Tmp3, Tmp2, 'add', TmpOffset)] ++

    little_loop(Tmp1, Tmp3, TmpOffset, Base) ++

    [hipe_rtl:mk_alu(Tmp4, Size, 'and', ?LOW_BITS),
     hipe_rtl:mk_alu(Tmp4, hipe_rtl:mk_imm(?BYTE_SIZE), 'sub', Tmp4),
     hipe_rtl:mk_alu(Tmp1, Tmp1, sll, Tmp4),
     hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
     hipe_rtl:mk_alu(NewOffset, Offset, 'add', Size)].

little_loop(Tmp1, Tmp3, TmpOffset, Base) ->
  [BranchLbl, BodyLbl, EndLbl] = create_lbls(3),
  [BranchLbl,
   hipe_rtl:mk_branch(TmpOffset, 'ne', Tmp3,
		      hipe_rtl:label_name(BodyLbl),
		      hipe_rtl:label_name(EndLbl)),
   BodyLbl,
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
   hipe_rtl:mk_alu(Tmp1, Tmp1, 'sra', hipe_rtl:mk_imm(?BYTE_SIZE)),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, 'add', hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(BranchLbl)),
   EndLbl].

big_loop(Tmp1, Tmp3, TmpOffset, Base) ->
  [BranchLbl, BodyLbl, EndLbl] = create_lbls(3),
  [BranchLbl,
   hipe_rtl:mk_branch(TmpOffset, 'ne', Tmp3,
		      hipe_rtl:label_name(BodyLbl),
		      hipe_rtl:label_name(EndLbl)),
   BodyLbl,
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, 'sub', hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
   hipe_rtl:mk_alu(Tmp1, Tmp1, 'sra', hipe_rtl:mk_imm(?BYTE_SIZE)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(BranchLbl)),
   EndLbl].

copy_int_big(_Base, Offset, NewOffset, 0, _Tmp1) ->
  [hipe_rtl:mk_move(NewOffset, Offset)];
copy_int_big(Base, Offset, NewOffset, ?BYTE_SIZE, Tmp1) ->
  TmpOffset = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_alu(TmpOffset, Offset, 'srl', hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
   hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(8))];
copy_int_big(Base, Offset, NewOffset, 2*?BYTE_SIZE, Tmp1) ->
  TmpOffset = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_alu(TmpOffset, Offset, 'srl', hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, 'add', hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, sub, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Tmp1, Tmp1, 'sra', hipe_rtl:mk_imm(8)),
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
   hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(16))];
copy_int_big(Base, Offset, NewOffset, 3*?BYTE_SIZE, Tmp1) ->
  TmpOffset = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_alu(TmpOffset, Offset, srl, hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, add, hipe_rtl:mk_imm(2)),
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, sub, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sra, hipe_rtl:mk_imm(8)),
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, sub, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sra, hipe_rtl:mk_imm(8)),
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
   hipe_rtl:mk_alu(NewOffset, Offset, add, hipe_rtl:mk_imm(24))];
copy_int_big(Base, Offset,NewOffset, 4*?BYTE_SIZE, Tmp1) ->
  copy_big_word(Base, Offset, NewOffset, Tmp1);
copy_int_big(Base, Offset, NewOffset, Size, Tmp1) when is_integer(Size) ->
  [OldOffset, TmpOffset, Bits] = create_regs(3),
  ByteSize = (Size + 7) div ?BYTE_SIZE,
  case Size band 7 of
    0 ->
      [hipe_rtl:mk_alu(OldOffset, Offset, sra, hipe_rtl:mk_imm(3)),
       hipe_rtl:mk_alu(TmpOffset, OldOffset, add, hipe_rtl:mk_imm(ByteSize))];
    Rest ->
      [hipe_rtl:mk_alu(OldOffset, Offset, sra, hipe_rtl:mk_imm(3)),
       hipe_rtl:mk_alu(TmpOffset, OldOffset, add, hipe_rtl:mk_imm(ByteSize-1)),
       hipe_rtl:mk_alu(Bits, Tmp1, sll, hipe_rtl:mk_imm(?BYTE_SIZE-Rest)),
       hipe_rtl:mk_store(Base, TmpOffset, Bits, byte),
       hipe_rtl:mk_alu(Tmp1, Tmp1, sra, hipe_rtl:mk_imm(Rest))]
  end ++
    big_loop(Tmp1, OldOffset, TmpOffset, Base) ++
    [hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(Size))];
copy_int_big(Base, Offset, NewOffset, Size, Tmp1) ->
  Tmp2 = hipe_rtl:mk_new_reg(),
  Tmp3 = hipe_rtl:mk_new_reg(),
  Tmp4 = hipe_rtl:mk_new_reg(),
  Tmp5 = hipe_rtl:mk_new_reg(),
  Tmp6 = hipe_rtl:mk_new_reg(),
  TmpOffset = hipe_rtl:mk_new_reg(),
  EvenLbl = hipe_rtl:mk_new_label(),
  OddLbl = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_alu(Tmp2, Size, 'srl', hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_alu(Tmp3, Offset, 'srl', hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_alu(TmpOffset, Tmp2, 'add', Tmp3),
   hipe_rtl:mk_alub(Tmp4, Size, 'and', hipe_rtl:mk_imm(7), 'eq',
		    hipe_rtl:label_name(EvenLbl), hipe_rtl:label_name(OddLbl)),
   OddLbl,
   hipe_rtl:mk_alu(Tmp6, hipe_rtl:mk_imm(8), 'sub', Tmp4),
   hipe_rtl:mk_alu(Tmp5, Tmp1, 'sll', Tmp6),
   hipe_rtl:mk_store(Base, TmpOffset, Tmp5, byte),
   EvenLbl,
   hipe_rtl:mk_alu(Tmp1, Tmp1, srl, Tmp4)] ++
    big_loop(Tmp1, Tmp3, TmpOffset, Base) ++
    [hipe_rtl:mk_alu(NewOffset, Offset, 'add', Size)].

copy_big_word(Base, Offset, NewOffset, Word) ->
  TmpOffset = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_alu(TmpOffset, Offset, 'srl', hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, 'add', hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, 'sub', hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Word, Word, 'sra', hipe_rtl:mk_imm(8)),
   hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, 'sub', hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Word, Word, 'sra', hipe_rtl:mk_imm(8)),
   hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, 'sub', hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Word, Word, 'sra', hipe_rtl:mk_imm(8)),
   hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
   hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(32))].

copy_little_word(Base, Offset, NewOffset, Word) ->
  TmpOffset = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_alu(TmpOffset, Offset, 'srl', ?BYTE_SHIFT),
   hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, 'add', hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Word, Word, 'sra', hipe_rtl:mk_imm(?BYTE_SIZE)),
   hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, 'add', hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Word, Word, 'sra', hipe_rtl:mk_imm(?BYTE_SIZE)),
   hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, 'add', hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Word, Word, 'sra', hipe_rtl:mk_imm(?BYTE_SIZE)),
   hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
   hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(32))].

copy_offset_int_big(_Base, Offset, NewOffset, 0, _Tmp1) ->
  [hipe_rtl:mk_move(NewOffset, Offset)];
copy_offset_int_big(Base, Offset, NewOffset, Size, Tmp1)
  when is_integer(Size), Size > 0 ->
  Tmp2 = hipe_rtl:mk_new_reg(),
  Tmp3 = hipe_rtl:mk_new_reg(),
  Tmp4 = hipe_rtl:mk_new_reg(),
  Tmp5 = hipe_rtl:mk_new_reg(),
  Tmp6 = hipe_rtl:mk_new_reg(),
  Tmp7 = hipe_rtl:mk_new_reg(),
  Tmp8 = hipe_rtl:mk_new_reg(),
  Tmp9 = hipe_rtl:mk_new_reg(),
  OldByte = hipe_rtl:mk_new_reg(),
  TmpOffset = hipe_rtl:mk_new_reg(),
  BranchLbl = hipe_rtl:mk_new_label(),
  BodyLbl = hipe_rtl:mk_new_label(),
  EndLbl = hipe_rtl:mk_new_label(),
  NextLbl = hipe_rtl:mk_new_label(),
  WordSize = hipe_rtl_arch:word_size(),
  [hipe_rtl:mk_alu(Tmp2, Offset, 'and', ?LOW_BITS),
   hipe_rtl:mk_alu(Tmp3, Offset, srl, ?BYTE_SHIFT),
   hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(Size)),
   hipe_rtl:mk_alu(Tmp9, NewOffset, 'sub', hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(TmpOffset, Tmp9, srl, ?BYTE_SHIFT),
   hipe_rtl:mk_alu(Tmp4, NewOffset, 'and', ?LOW_BITS),
   hipe_rtl:mk_alu(Tmp6, hipe_rtl:mk_imm(?BYTE_SIZE), 'sub', Tmp4),
   hipe_rtl:mk_alu(Tmp6, Tmp6, 'and', ?LOW_BITS),
   hipe_rtl:mk_alu(Tmp4, hipe_rtl:mk_imm(?BYTE_SIZE), 'sub', Tmp6),
   hipe_rtl:mk_move(Tmp5, Tmp1),
   hipe_rtl:mk_alu(Tmp1, Tmp1, 'sll', Tmp6),
   hipe_rtl:mk_branch(TmpOffset, 'ne', Tmp3, hipe_rtl:label_name(NextLbl),
		      hipe_rtl:label_name(EndLbl)),
   NextLbl,
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
   hipe_rtl:mk_move(Tmp1, Tmp5),
   hipe_rtl:mk_alu(Tmp1, Tmp1, 'sra', Tmp4),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, 'sub', hipe_rtl:mk_imm(1)),
   BranchLbl,
   hipe_rtl:mk_branch(TmpOffset, 'ne', Tmp3, hipe_rtl:label_name(BodyLbl),
		      hipe_rtl:label_name(EndLbl)),
   BodyLbl,
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
   hipe_rtl:mk_alu(Tmp1, Tmp1, 'sra', hipe_rtl:mk_imm(?BYTE_SIZE)),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, 'sub', hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(BranchLbl)),
   EndLbl,
   hipe_rtl:mk_load(OldByte, Base, TmpOffset, byte, unsigned),
   hipe_rtl:mk_alu(Tmp8, hipe_rtl:mk_imm(?BYTE_SIZE), 'sub', Tmp2),
   hipe_rtl:mk_alu(OldByte, OldByte, 'srl', Tmp8),
   hipe_rtl:mk_alu(OldByte, OldByte, 'sll', Tmp8),
   hipe_rtl:mk_alu(Tmp7, Tmp2, 'add',
		   hipe_rtl:mk_imm(?bytes_to_bits(WordSize-1))),
   hipe_rtl:mk_alu(Tmp1, Tmp1, 'sll', Tmp7),
   hipe_rtl:mk_alu(Tmp1, Tmp1, 'srl', Tmp7),
   hipe_rtl:mk_alu(Tmp1, Tmp1, 'or', OldByte),
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte)].

copy_float_little(_Base, _Offset, _NewOffset, _Src, FalseLblName, _TrueLblName, fail) ->
  [hipe_rtl:mk_goto(FalseLblName)];
copy_float_little(Base, Offset, NewOffset, Src, _FalseLblName, TrueLblName, pass) ->
  FloatLo = hipe_rtl:mk_new_reg(),
  FloatHi = hipe_rtl:mk_new_reg(),
  TmpOffset = hipe_rtl:mk_new_reg(),
   hipe_tagscheme:unsafe_load_float(FloatLo, FloatHi, Src) ++
    copy_little_word(Base, Offset, TmpOffset, FloatLo) ++
    copy_little_word(Base, TmpOffset, NewOffset, FloatHi) ++
    [hipe_rtl:mk_goto(TrueLblName)];
copy_float_little(Base, Offset, NewOffset, Src, FalseLblName, TrueLblName, var) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  hipe_tagscheme:test_flonum(Src, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99) ++
    [SuccessLbl|copy_float_little(Base, Offset, NewOffset, Src, FalseLblName, TrueLblName, pass)].

copy_float_big(_Base, _Offset, _NewOffset, _Src, FalseLblName, _TrueLblName, fail) ->
  [hipe_rtl:mk_goto(FalseLblName)];
copy_float_big(Base, Offset, NewOffset, Src, _FalseLblName, TrueLblName,pass) ->
  FloatLo = hipe_rtl:mk_new_reg(),
  FloatHi = hipe_rtl:mk_new_reg(),
  TmpOffset = hipe_rtl:mk_new_reg(),
  hipe_tagscheme:unsafe_load_float(FloatLo, FloatHi, Src) ++
    copy_big_word(Base, Offset, TmpOffset, FloatHi) ++
    copy_big_word(Base, TmpOffset, NewOffset, FloatLo) ++
    [hipe_rtl:mk_goto(TrueLblName)];
copy_float_big(Base, Offset, NewOffset, Src, FalseLblName, TrueLblName, var) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  hipe_tagscheme:test_flonum(Src, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99) ++
    [SuccessLbl|copy_float_big(Base, Offset, NewOffset, Src, FalseLblName, TrueLblName, pass)].

is_divisible(_Dividend, 1, SuccLbl, _FailLbl) ->
  [hipe_rtl:mk_goto(SuccLbl)];
is_divisible(Dividend, Divisor, SuccLbl, FailLbl) ->
  Log2 = hipe_rtl_binary:floorlog2(Divisor),
  case Divisor =:= 1 bsl Log2 of
    true -> %% Divisor is a power of 2
      %% Test that the Log2-1 lowest bits are clear
      Mask = hipe_rtl:mk_imm(Divisor - 1),
      [Tmp] = create_regs(1),
      [hipe_rtl:mk_alub(Tmp, Dividend, 'and', Mask, eq, SuccLbl, FailLbl, 0.99)];
    false ->
      %% We need division, fall back to a primop
      [hipe_rtl:mk_call([], is_divisible, [Dividend, hipe_rtl:mk_imm(Divisor)],
			SuccLbl, FailLbl, not_remote)]
  end.
