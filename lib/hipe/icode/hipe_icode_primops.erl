%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Filename : 	hipe_icode_primops.erl
%%  Module   :	hipe_icode_primops
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-06-13 Erik Johansson (happi@it.uu.se): 
%%               Created.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_icode_primops).

-export([is_safe/1, fails/1, pp/2, type/1, type/2, arg_types/1]).

-include("hipe_icode.hrl").
-include("hipe_icode_primops.hrl").

%%---------------------------------------------------------------------

%% Note that 'unsafe_...' operations are generally "safe", i.e., it is
%% typically unsafe to use them unless you have extra information about
%% the call (e.g., if the types are known). However, if they have been
%% correctly introduced in the code, most of them are also OK to remove
%% if the result is not used.

-spec is_safe(icode_primop()) -> boolean().

is_safe('+') -> false;
is_safe('/') -> false;
is_safe('*') -> false;
is_safe('-') -> false;
is_safe('bsr') -> false;
is_safe('bsl') -> false;
is_safe('band') -> false;
is_safe('bor') -> false;
is_safe('bxor') -> false;
is_safe('bnot') -> false;
is_safe('div') -> false;
is_safe('rem') -> false;
is_safe(call_fun) -> false;
is_safe(check_get_msg) -> false;
is_safe(clear_timeout) -> false;
is_safe(cons) -> true;
%% is_safe(conv_to_float) -> false;
is_safe(extra_unsafe_add) -> true;
is_safe(extra_unsafe_sub) -> true;
is_safe(fcheckerror) -> false;
is_safe(fclearerror) -> false;
is_safe(fp_add) -> false;
is_safe(fp_div) -> false;
is_safe(fp_mul) -> false;
is_safe(fp_sub) -> false;
is_safe(mktuple) -> true;
is_safe(next_msg) -> false;
is_safe(redtest) -> false;
is_safe(select_msg) -> false;
is_safe(self) -> true;
is_safe(set_timeout) -> false;
is_safe(suspend_msg) -> false;
is_safe(unsafe_add) -> true;
is_safe(unsafe_band) -> true;
is_safe(unsafe_bnot) -> true;
is_safe(unsafe_bor) -> true;
is_safe(unsafe_bsl) -> true;
is_safe(unsafe_bsr) -> true;
is_safe(unsafe_bxor) -> true;
is_safe(unsafe_hd) -> true;
is_safe(unsafe_sub) -> true;
is_safe(unsafe_tag_float) -> true;
is_safe(unsafe_tl) -> true;
is_safe(unsafe_untag_float) -> true;
is_safe(#apply_N{}) -> false;
is_safe(#closure_element{}) -> true;
is_safe(#element{}) -> false;
%% is_safe(#gc_test{}) -> ???
is_safe({hipe_bs_primop, {bs_start_match, _}}) -> false;
is_safe({hipe_bs_primop, {{bs_start_match, bitstr}, _}}) -> true;
is_safe({hipe_bs_primop, {{bs_start_match, ok_matchstate}, _}}) -> false;
is_safe({hipe_bs_primop, {bs_get_binary, _, _}}) -> false;
is_safe({hipe_bs_primop, {bs_get_binary_all, _, _}}) -> false;
is_safe({hipe_bs_primop, {bs_get_binary_all_2, _, _}}) -> false;
is_safe({hipe_bs_primop, {bs_get_integer, _, _}}) -> false;
is_safe({hipe_bs_primop, {bs_get_float, _, _}}) -> false;
is_safe({hipe_bs_primop, {bs_skip_bits, _}}) -> false;
is_safe({hipe_bs_primop, {bs_skip_bits_all, _, _}}) -> false;
is_safe({hipe_bs_primop, {bs_test_tail, _}}) -> false;
is_safe({hipe_bs_primop, {bs_restore, _}}) -> true;
is_safe({hipe_bs_primop, {bs_save, _}}) -> true;
is_safe({hipe_bs_primop, {bs_add, _}}) -> false;
is_safe({hipe_bs_primop, {bs_add, _, _}}) -> false;
is_safe({hipe_bs_primop, bs_bits_to_bytes}) -> false;
is_safe({hipe_bs_primop, bs_bits_to_bytes2}) -> false;
is_safe({hipe_bs_primop, {bs_init, _}}) -> false;
is_safe({hipe_bs_primop, {bs_init, _, _}}) -> false;
is_safe({hipe_bs_primop, {bs_init_bits, _}}) -> false;
is_safe({hipe_bs_primop, {bs_init_bits, _, _}}) -> false;
is_safe({hipe_bs_primop, {bs_put_binary, _, _}}) -> false;
is_safe({hipe_bs_primop, {bs_put_binary_all, _, _}}) -> false;
is_safe({hipe_bs_primop, {bs_put_float, _, _, _}}) -> false;
is_safe({hipe_bs_primop, {bs_put_integer, _, _, _}}) -> false;
is_safe({hipe_bs_primop, {bs_put_string, _, _}}) -> false;  
is_safe({hipe_bs_primop, bs_put_utf8}) -> false;
is_safe({hipe_bs_primop, bs_utf8_size}) -> true;
is_safe({hipe_bs_primop, bs_get_utf8}) -> false;
is_safe({hipe_bs_primop, bs_utf16_size}) -> true;
is_safe({hipe_bs_primop, {bs_put_utf16, _}}) -> false;
is_safe({hipe_bs_primop, {bs_get_utf16, _}}) -> false;
is_safe({hipe_bs_primop, bs_validate_unicode}) -> false;
is_safe({hipe_bs_primop, bs_validate_unicode_retract}) -> false;
is_safe({hipe_bs_primop, {unsafe_bs_put_integer, _, _, _}}) -> false;
is_safe({hipe_bs_primop, bs_final}) -> true;
is_safe({hipe_bs_primop, bs_context_to_binary}) -> true;
is_safe({hipe_bs_primop, {bs_test_unit, _}}) -> false;
is_safe({hipe_bs_primop, {bs_match_string, _, _}}) -> false;
is_safe({hipe_bs_primop, {bs_append, _, _, _, _}}) -> false;
is_safe({hipe_bs_primop, {bs_private_append, _, _}}) -> false;
is_safe({hipe_bs_primop, bs_init_writable}) -> true;
is_safe(#mkfun{}) -> true;
is_safe(#unsafe_element{}) -> true;
is_safe(#unsafe_update_element{}) -> true;
is_safe(debug_native_called) -> false.


-spec fails(icode_funcall()) -> boolean().

fails('+') -> true;
fails('-') -> true;
fails('*') -> true;
fails('/') -> true;
fails('bnot') -> true;
fails('band') -> true;
fails('bor') -> true;
fails('bsl') -> true;
fails('bsr') -> true;
fails('bxor') -> true;
fails('div') -> true;
fails('rem') -> true;
fails(call_fun) -> true;
fails(check_get_msg) -> true;
fails(clear_timeout) -> false;
fails(cons) -> false;
fails(conv_to_float) -> true;
fails(extra_unsafe_add) -> false;
fails(extra_unsafe_sub) -> false;
fails(fcheckerror) -> true;
fails(fclearerror) -> false;
fails(fp_add) -> false;
fails(fp_div) -> false;
fails(fp_mul) -> false;
fails(fp_sub) -> false;
fails(mktuple) -> false;
fails(next_msg) -> false;
fails(redtest) -> false;
fails(select_msg) -> false;
fails(self) -> false;
fails(set_timeout) -> true;
fails(suspend_msg) -> false;
fails(unsafe_untag_float) -> false;
fails(unsafe_tag_float) -> false;
fails(unsafe_add) -> false;
fails(unsafe_band) -> false;
fails(unsafe_bnot) -> false;
fails(unsafe_bor) -> false;
fails(unsafe_bsl) -> false;
fails(unsafe_bsr) -> false;
fails(unsafe_bxor) -> false;
fails(unsafe_hd) -> false;
fails(unsafe_sub) -> false;
%% fails(unsafe_tag_float) -> false;
fails(unsafe_tl) -> false;
%% fails(unsafe_untag_float) -> false;
fails(#apply_N{}) -> true;
fails(#closure_element{}) -> false;
fails(#element{}) -> true;
%% fails(#gc_test{}) -> ???
fails({hipe_bs_primop, {bs_start_match, _}}) -> true;
fails({hipe_bs_primop, {{bs_start_match, bitstr}, _}}) -> true;
fails({hipe_bs_primop, {{bs_start_match, ok_matchstate}, _}}) -> true;
fails({hipe_bs_primop, {bs_get_binary, _, _}}) -> true;
fails({hipe_bs_primop, {bs_get_binary_all, _, _}}) -> true;
fails({hipe_bs_primop, {bs_get_binary_all_2, _, _}}) -> true;
fails({hipe_bs_primop, {bs_get_integer, _, _}}) -> true;
fails({hipe_bs_primop, {bs_get_float, _, _}}) -> true;
fails({hipe_bs_primop, {bs_skip_bits, _}}) -> true;
fails({hipe_bs_primop, {bs_skip_bits_all, _, _}}) -> true;
fails({hipe_bs_primop, {bs_test_tail, _}}) -> true;
fails({hipe_bs_primop, {bs_restore, _}}) -> false;
fails({hipe_bs_primop, {bs_save, _}}) -> false;
fails({hipe_bs_primop, bs_context_to_binary}) -> false;
fails({hipe_bs_primop, {bs_test_unit, _}}) -> true;
fails({hipe_bs_primop, {bs_match_string, _, _}}) -> true;
fails({hipe_bs_primop, {bs_add, _}}) -> true;
fails({hipe_bs_primop, {bs_add, _, _}}) -> true;
fails({hipe_bs_primop, bs_bits_to_bytes}) -> true;
fails({hipe_bs_primop, bs_bits_to_bytes2}) -> true;
fails({hipe_bs_primop, {bs_init, _}}) -> true;
fails({hipe_bs_primop, {bs_init, _, _}}) -> true;
fails({hipe_bs_primop, {bs_init_bits, _}}) -> true;
fails({hipe_bs_primop, {bs_init_bits, _, _}}) -> true;
fails({hipe_bs_primop, {bs_put_binary, _, _}}) -> true;
fails({hipe_bs_primop, {bs_put_binary_all, _, _}}) -> true;
fails({hipe_bs_primop, {bs_put_float, _, _, _}}) -> true;
fails({hipe_bs_primop, {bs_put_integer, _, _, _}}) -> true;
fails({hipe_bs_primop, {bs_put_string, _, _}}) -> true;  
fails({hipe_bs_primop, bs_put_utf8}) -> true;
fails({hipe_bs_primop, bs_utf8_size}) -> false;
fails({hipe_bs_primop, bs_get_utf8}) -> true;
fails({hipe_bs_primop, bs_utf16_size}) -> false;
fails({hipe_bs_primop, {bs_put_utf16, _}}) -> true;
fails({hipe_bs_primop, {bs_get_utf16, _}}) -> true;
fails({hipe_bs_primop, bs_validate_unicode}) -> true;
fails({hipe_bs_primop, bs_validate_unicode_retract}) -> true;
fails({hipe_bs_primop, {unsafe_bs_put_integer, _, _, _}}) -> true;
fails({hipe_bs_primop, bs_final}) -> false;
fails({hipe_bs_primop, {bs_append, _, _, _, _}}) -> true;
fails({hipe_bs_primop, {bs_private_append, _, _}}) -> true;
fails({hipe_bs_primop, bs_init_writable}) -> true;
fails(#mkfun{}) -> false;
fails(#unsafe_element{}) -> false;
fails(#unsafe_update_element{}) -> false;
fails(debug_native_called) -> false;
%% Apparently, we are calling fails/1 for all MFAs which are compiled.
%% This is weird and we should restructure the compiler to avoid
%% calling fails/1 for things that are not primops.
fails({M, F, A}) when is_atom(M), is_atom(F), is_integer(A), 0 =< A, A =< 255 ->
  %% Yes, we should move this.
  not erl_bifs:is_safe(M, F, A).

%%=====================================================================
%% Pretty printing
%%=====================================================================

-spec pp(io:device(), icode_primop()) -> 'ok'.

pp(Dev, Op) ->
  case Op of
    #apply_N{arity = N} ->
      io:format(Dev, "apply_N<~w>/", [N]);
    #closure_element{n = N} ->
      io:format(Dev, "closure_element<~w>", [N]);
    #element{} ->
      io:format(Dev, "element", []);
    #gc_test{need = N} ->
      io:format(Dev, "gc_test<~w>", [N]);
    {hipe_bs_primop, BsOp}  ->
      case BsOp of
	{bs_put_binary_all, Unit, Flags} ->
	  io:format(Dev, "bs_put_binary_all<~w, ~w>", [Unit,Flags]);
	{bs_put_binary, Size} ->
	  io:format(Dev, "bs_put_binary<~w>", [Size]);
	{bs_put_binary, Flags, Size} ->
	  io:format(Dev, "bs_put_binary<~w, ~w>", [Flags, Size]);
	{bs_put_float, Flags, Size, _ConstInfo} ->
	  io:format(Dev, "bs_put_float<~w, ~w>", [Flags, Size]);
	{bs_put_string, String, SizeInBytes} ->
	  io:format(Dev, "bs_put_string<~w, ~w>", [String, SizeInBytes]);
	{bs_put_integer, Bits, Flags, _ConstInfo} ->
	  io:format(Dev, "bs_put_integer<~w, ~w>", [Bits, Flags]);
	{unsafe_bs_put_integer, Bits, Flags, _ConstInfo} ->
	  io:format(Dev, "unsafe_bs_put_integer<~w, ~w>", [Bits, Flags]);
	{bs_skip_bits_all, Unit, Flags} ->
	  io:format(Dev, "bs_skip_bits_all<~w,~w>", [Unit, Flags]);
	{bs_skip_bits, Unit} ->
	  io:format(Dev, "bs_skip_bits<~w>", [Unit]);
	{bs_start_match, Max} ->
	  io:format(Dev, "bs_start_match<~w>", [Max]);
	{{bs_start_match, Type}, Max} ->
	  io:format(Dev, "bs_start_match<~w,~w>", [Type,Max]);
	{bs_match_string, String, SizeInBytes} ->
	  io:format(Dev, "bs_match_string<~w, ~w>", [String, SizeInBytes]);
	{bs_get_integer, Size, Flags} ->
	  io:format(Dev, "bs_get_integer<~w, ~w>", [Size, Flags]);
	{bs_get_float, Size, Flags} ->
	  io:format(Dev, "bs_get_float<~w, ~w>", [Size, Flags]);
	{bs_get_binary, Size, Flags} ->
	  io:format(Dev, "bs_get_binary<~w, ~w>", [Size, Flags]);
	{bs_get_binary_all, Unit, Flags} ->
	  io:format(Dev, "bs_get_binary_all<~w,~w>", [Unit, Flags]);
	{bs_get_binary_all_2, Unit, Flags} ->
	  io:format(Dev, "bs_get_binary_all<~w,~w>", [Unit, Flags]);
	{bs_test_tail, NumBits} ->
	  io:format(Dev, "bs_test_tail<~w>", [NumBits]);
	{bs_test_unit, Unit} ->
	  io:format(Dev, "bs_test_unit<~w>", [Unit]);
	bs_context_to_binary ->
	  io:format(Dev, "bs_context_to_binary", []);
	{bs_restore, Index} ->
	  io:format(Dev, "bs_restore<~w>", [Index]);
	{bs_save, Index} ->
	  io:format(Dev, "bs_save<~w>", [Index]);
	{bs_init, Size, Flags} ->
	  io:format(Dev, "bs_init<~w, ~w>", [Size, Flags]);
	{bs_init,Flags} ->
	  io:format(Dev, "bs_init<~w>", [Flags]);
	{bs_init_bits, Size, Flags} ->
	  io:format(Dev, "bs_init_bits<~w, ~w>", [Size, Flags]);
	{bs_init_bits, Flags} ->
	  io:format(Dev, "bs_init_bits<~w>", [Flags]);
	{bs_add, Unit} ->
	  io:format(Dev, "bs_add<~w>", [Unit]);
	{bs_add, Const, Unit} ->
	  io:format(Dev, "bs_add<~w, ~w>", [Const, Unit]);
	{bs_append, X, Y, Z, W} ->
	  io:format(Dev, "bs_append<~w, ~w, ~w, ~w>", [X, Y, Z, W]);
	{bs_private_append, U, Flags} ->
	  io:format(Dev, "bs_private_append<~w, ~w>", [U, Flags]);
	bs_bits_to_bytes ->
	  io:format(Dev, "bs_bits_to_bytes", []);
	bs_bits_to_bytes2 ->
	  io:format(Dev, "bs_bits_to_bytes2", []);
	bs_utf8_size ->
	  io:format(Dev, "bs_utf8_size", []);
	bs_put_utf8 ->
	  io:format(Dev, "bs_put_utf8", []);
	bs_get_utf8 ->
	  io:format(Dev, "bs_get_utf8", []);
	bs_utf16_size ->
	  io:format(Dev, "bs_utf16_size", []);
	{bs_put_utf16, Flags} ->
	  io:format(Dev, "bs_put_utf16<~w>", [Flags]);
	{bs_get_utf16, Flags} ->
	  io:format(Dev, "bs_get_utf16<~w>", [Flags]);
	bs_validate_unicode ->
	  io:format(Dev, "bs_validate_unicode", []);
	bs_validate_unicode_retract ->
	  io:format(Dev, "bs_validate_unicode_retract", []);
	bs_final ->
	  io:format(Dev, "bs_final", []);
	bs_final2 ->
	  io:format(Dev, "bs_final2", []);
	bs_init_writable ->
	  io:format(Dev, "bs_init_writable", [])
      end;
    #mkfun{mfa = {Mod, Fun, Arity}, magic_num = Unique, index = I} ->
      io:format(Dev, "mkfun<~w,~w,~w,~w,~w>", [Mod, Fun, Arity, Unique, I]);
    #unsafe_element{index = N} ->
      io:format(Dev, "unsafe_element<~w>", [N]);
    #unsafe_update_element{index = N} ->
      io:format(Dev, "unsafe_update_element<~w>", [N]);
    Fun when is_atom(Fun) ->
      io:format(Dev, "~w", [Fun])
  end.

%%=====================================================================
%% Type handling
%%=====================================================================

-spec type(icode_funcall(), [erl_types:erl_type()]) -> erl_types:erl_type().

type(Primop, Args) ->
  case Primop of
%%% -----------------------------------------------------
%%% Arithops
    '+' ->
      erl_bif_types:type(erlang, '+', 2, Args);
    '-' ->
      erl_bif_types:type(erlang, '-', 2, Args);
    '*' ->
      erl_bif_types:type(erlang, '*', 2, Args);
    '/' ->
      erl_bif_types:type(erlang, '/', 2, Args);
    'band' ->
      erl_bif_types:type(erlang, 'band', 2, Args);
    'bnot' ->
      erl_bif_types:type(erlang, 'bnot', 1, Args);
    'bor' ->
      erl_bif_types:type(erlang, 'bor', 2, Args);
    'bxor' ->
      erl_bif_types:type(erlang, 'bxor', 2, Args);
    'bsl' ->
      erl_bif_types:type(erlang, 'bsl', 2, Args);
    'bsr' ->
      erl_bif_types:type(erlang, 'bsr', 2, Args);
    'div' ->
      erl_bif_types:type(erlang, 'div', 2, Args);
    'rem' ->
      erl_bif_types:type(erlang, 'rem', 2, Args);
    extra_unsafe_add ->
      erl_bif_types:type(erlang, '+', 2, Args);
    unsafe_add ->
      erl_bif_types:type(erlang, '+', 2, Args);
    unsafe_bnot ->
      erl_bif_types:type(erlang, 'bnot', 1, Args);
    unsafe_bor ->
      erl_bif_types:type(erlang, 'bor', 2, Args);
    unsafe_band ->
      erl_bif_types:type(erlang, 'band', 2, Args);
    unsafe_bxor ->
      erl_bif_types:type(erlang, 'bxor', 2, Args);
    unsafe_sub ->
      erl_bif_types:type(erlang, '-', 2, Args);
%%% -----------------------------------------------------
%%% Lists
    cons ->
      [HeadType, TailType] = Args,
      erl_types:t_cons(HeadType, TailType);
    unsafe_hd ->
      [Type] = Args,
      case erl_types:t_is_cons(Type) of
	true -> erl_types:t_cons_hd(Type);
	false -> erl_types:t_none()
      end;
    unsafe_tl ->
      [Type] = Args,
      case erl_types:t_is_cons(Type) of
	true -> erl_types:t_cons_tl(Type);
	false -> erl_types:t_none()
      end;
%%% -----------------------------------------------------
%%% Tuples
    mktuple ->
      erl_types:t_tuple(Args);
    #element{} ->
      erl_bif_types:type(erlang, element, 2, Args);
    #unsafe_element{index = N} ->
      [Type] = Args,
      case erl_types:t_is_tuple(Type) of
	false ->
	  erl_types:t_none();
	true ->
	  Index = erl_types:t_from_term(N),
	  erl_bif_types:type(erlang, element, 2, [Index|Args])
      end;
    #unsafe_update_element{index = N} ->
      %% Same, same
      erl_bif_types:type(erlang, setelement, 3, [erl_types:t_integer(N)|Args]);
%%% -----------------------------------------------------
%%% Floats
    fclearerror ->
      erl_types:t_any();
    fcheckerror ->
      erl_types:t_any();
    unsafe_tag_float ->
      erl_types:t_float();
    %% These might look surprising, but the return is an untagged
    %% float and we have no type for untagged values.
    conv_to_float ->
      erl_types:t_any();
    unsafe_untag_float ->
      erl_types:t_any();
    fp_add ->
      erl_types:t_any();
    fp_sub ->
      erl_types:t_any();
    fp_mul ->
      erl_types:t_any();
    fp_div ->
      erl_types:t_any();
    fnegate ->
      erl_types:t_any();
%%% -----------------------------------------------------
%%% 
    {hipe_bs_primop, {bs_start_match, Max}} ->
      [Type] = Args,
      Init = 
	erl_types:t_sup(
	  erl_types:t_matchstate_present(Type),
	  erl_types:t_inf(erl_types:t_bitstr(1, 0), Type)),
      case erl_types:t_is_none(Init) of
	true -> 
	  erl_types:t_none();
	false -> 
	  erl_types:t_matchstate(Init, Max)
      end;
    {hipe_bs_primop, {{bs_start_match, _}, Max}} ->
      [Type] = Args,
      Init = 
	erl_types:t_sup(
	  erl_types:t_matchstate_present(Type),
	  erl_types:t_inf(erl_types:t_bitstr(1, 0), Type)),
      case erl_types:t_is_none(Init) of
	true -> 
	  erl_types:t_none();
	false -> 
	  erl_types:t_matchstate(Init, Max)
      end;
    {hipe_bs_primop, {bs_get_integer, Size, Flags}} ->
      Signed = Flags band 4,
      [MatchState|RestArgs] = Args,
      BinType = erl_types:t_matchstate_present(MatchState),
      case RestArgs of
	[] ->
	  NewBinType = match_bin(erl_types:t_bitstr(0, Size), BinType),
	  NewMatchState = 
	    erl_types:t_matchstate_update_present(NewBinType, MatchState),
	  Range =
	    case Signed of
	      0 ->
		UpperBound = inf_add(safe_bsl_1(Size), -1),
		erl_types:t_from_range(0, UpperBound);
	      4 ->
		Bound = safe_bsl_1(Size - 1),
		erl_types:t_from_range(inf_inv(Bound), inf_add(Bound, -1))
	    end,
	  erl_types:t_product([Range, NewMatchState]);
	[_Arg] ->
	  NewBinType = match_bin(erl_types:t_bitstr(Size, 0), BinType),
	  NewMatchState = 
	    erl_types:t_matchstate_update_present(NewBinType, MatchState),
	  erl_types:t_product([erl_types:t_integer(), NewMatchState])
      end;	     
    {hipe_bs_primop, {bs_get_float, Size, _Flags}} ->
      [MatchState|RestArgs] = Args,
      BinType = erl_types:t_matchstate_present(MatchState),
      NewBinType = 
	case RestArgs of
	  [] ->
	    match_bin(erl_types:t_bitstr(0,Size),BinType);
	  [_Arg] ->
	    erl_types:t_sup(match_bin(erl_types:t_bitstr(0, 32), BinType),
			    match_bin(erl_types:t_bitstr(0, 64), BinType))
	end,
      NewMatchState = erl_types:t_matchstate_update_present(NewBinType, MatchState),
      erl_types:t_product([erl_types:t_float(), NewMatchState]);
    {hipe_bs_primop, {bs_get_binary, Size, _Flags}} ->
      [MatchState|RestArgs] = Args,
      BinType = erl_types:t_matchstate_present(MatchState),
      case RestArgs of
	[] ->
	  NewBinType = match_bin(erl_types:t_bitstr(0, Size), BinType),
	  NewMatchState = erl_types:t_matchstate_update_present(NewBinType, MatchState),
	  erl_types:t_product([erl_types:t_bitstr(0,Size), NewMatchState]);
	[ArgType] ->
	  Posint = erl_types:t_inf(erl_types:t_non_neg_integer(), ArgType),
	  case erl_types:t_is_none(Posint) of
	    true -> 
	      erl_types:t_product([erl_types:t_none(),
				   erl_types:t_matchstate_update_present(
				     erl_types:t_none(),
				     MatchState)]);
	    false ->
	      OutBinType = 
		erl_types:t_bitstr(Size,erl_types:number_min(Posint)*Size),
	      NewBinType = match_bin(OutBinType,BinType),
	      NewMatchState = erl_types:t_matchstate_update_present(NewBinType, MatchState),
	      erl_types:t_product([OutBinType, NewMatchState])
	  end
      end;
    {hipe_bs_primop, {bs_get_binary_all, Unit, _Flags}} ->
      [MatchState] = Args,
      BinType = erl_types:t_matchstate_present(MatchState),
      erl_types:t_inf(BinType, erl_types:t_bitstr(Unit, 0));
    {hipe_bs_primop, {bs_get_binary_all_2, Unit, _Flags}} ->
      [MatchState] = Args,
      BinType = erl_types:t_matchstate_present(MatchState),
      erl_types:t_product(
	[erl_types:t_inf(BinType,erl_types:t_bitstr(Unit, 0)),
	 erl_types:t_matchstate_update_present(
	   erl_types:t_bitstr(0, 0), MatchState)]);
    {hipe_bs_primop, {bs_skip_bits_all, _Unit, _Flags}} ->
      [MatchState] = Args,
      erl_types:t_matchstate_update_present(erl_types:t_bitstr(0,0),MatchState);
    {hipe_bs_primop, {bs_skip_bits, Size}} ->
      [MatchState|RestArgs] = Args,
      BinType = erl_types:t_matchstate_present(MatchState),
      NewBinType = 
	case RestArgs of
	  [] ->
	    match_bin(erl_types:t_bitstr(0, Size), BinType);
	  [_Arg] ->
	    match_bin(erl_types:t_bitstr(Size, 0), BinType)
	end,
      erl_types:t_matchstate_update_present(NewBinType, MatchState);
    {hipe_bs_primop, {bs_save, Slot}} ->
      [MatchState] = Args,
      BinType = erl_types:t_matchstate_present(MatchState),
      erl_types:t_matchstate_update_slot(BinType, MatchState, Slot);
    {hipe_bs_primop, {bs_restore, Slot}} ->
      [MatchState] = Args,
      BinType = erl_types:t_matchstate_slot(MatchState, Slot),
      erl_types:t_matchstate_update_present(BinType, MatchState);
    {hipe_bs_primop, bs_context_to_binary} ->
      [Type] = Args,
      erl_types:t_sup(
	erl_types:t_subtract(Type, erl_types:t_matchstate()),
	erl_types:t_matchstate_slot(
	  erl_types:t_inf(Type, erl_types:t_matchstate()), 0));
    {hipe_bs_primop, {bs_match_string,_,Bytes}} ->
      [MatchState] = Args,
      BinType = erl_types:t_matchstate_present(MatchState),
      NewBinType = match_bin(erl_types:t_bitstr(0, Bytes*8), BinType),
      erl_types:t_matchstate_update_present(NewBinType, MatchState);
    {hipe_bs_primop, {bs_test_unit,Unit}} ->
      [MatchState] = Args,
      BinType = erl_types:t_matchstate_present(MatchState),
      NewBinType = erl_types:t_inf(erl_types:t_bitstr(Unit, 0), BinType),
      erl_types:t_matchstate_update_present(NewBinType, MatchState);
    {hipe_bs_primop, {bs_add, _, _}} ->
      erl_types:t_integer();
    {hipe_bs_primop, {bs_add, _}} ->
      erl_types:t_integer();
    {hipe_bs_primop, bs_bits_to_bytes} ->
      erl_types:t_integer();
    {hipe_bs_primop, bs_bits_to_bytes2} ->
      erl_types:t_integer();
    {hipe_bs_primop, {Name, Size, _Flags, _ConstInfo}} 
    when Name =:= bs_put_integer;
	 Name =:= bs_put_float ->
      case Args of
	[_SrcType, _Base, Type] ->
	  erl_types:t_bitstr_concat(Type, erl_types:t_bitstr(0, Size));
	[_SrcType,_BitsType, _Base, Type] ->
	  erl_types:t_bitstr_concat(Type, erl_types:t_bitstr(Size, 0))
      end;
    {hipe_bs_primop, {bs_put_binary, Size, _Flags}} ->
      case Args of
	[_SrcType, _Base, Type] ->
	  erl_types:t_bitstr_concat(Type, erl_types:t_bitstr(0, Size));
	[_SrcType, _BitsType, _Base, Type] ->
	  erl_types:t_bitstr_concat(Type, erl_types:t_bitstr(Size, 0))
      end;
    {hipe_bs_primop, {bs_put_binary_all, Unit, _Flags}} ->
      [SrcType0, _Base, Type] = Args,
      SrcType = erl_types:t_inf(erl_types:t_bitstr(Unit, 0), SrcType0),
      erl_types:t_bitstr_concat(SrcType,Type);
    {hipe_bs_primop, {bs_put_string, _, Size}} ->
      [_Base, Type] = Args,
      erl_types:t_bitstr_concat(Type, erl_types:t_bitstr(0, 8*Size));
    {hipe_bs_primop, bs_utf8_size} ->
      [_Arg] = Args,
      erl_types:t_from_range(1, 4);
    {hipe_bs_primop, bs_utf16_size} ->
      [_Arg] = Args,
      erl_types:t_from_range(2, 4);	% XXX: really 2 | 4
    {hipe_bs_primop, bs_final} ->
      [_Base, Type] = Args,
      Type;
    {hipe_bs_primop, {bs_init, Size, _Flags}} ->  
      erl_types:t_product(
	[erl_types:t_bitstr(0, Size*8),
	 erl_types:t_any(),
	 erl_types:t_bitstr(0, 0)]);
    {hipe_bs_primop, {bs_init, _Flags}} ->
      erl_types:t_product(
	[erl_types:t_binary(),
	 erl_types:t_any(),
	 erl_types:t_bitstr(0, 0)]);
    {hipe_bs_primop, {bs_init_bits, Size, _Flags}} ->  
      erl_types:t_product(
	[erl_types:t_bitstr(0, Size),
	 erl_types:t_any(),
	 erl_types:t_bitstr(0, 0)]);
    {hipe_bs_primop, {bs_init_bits, _Flags}} ->
      erl_types:t_product(
	[erl_types:t_bitstr(),
	 erl_types:t_any(),
	 erl_types:t_bitstr(0, 0)]);
    {hipe_bs_primop, {bs_private_append, _U, _Flags}} ->  
      erl_types:t_product(
	[erl_types:t_bitstr(),
	 erl_types:t_any(),
	 erl_types:t_bitstr()]);
    {hipe_bs_primop, {bs_append, _W, _R, _U, _Flags}} ->  
      erl_types:t_product(
	[erl_types:t_bitstr(),
	 erl_types:t_any(),
	 erl_types:t_bitstr()]);
    {hipe_bs_primop, bs_init_writable} ->
      erl_types:t_bitstr(0, 0);
    {hipe_bs_primop, _BsOp} ->
      erl_types:t_any();
%%% -----------------------------------------------------
%%% Funs
    #mkfun{mfa = {_M, _F, A}} ->
      %% Note that the arity includes the bound variables in args
      erl_types:t_fun(A - length(Args), erl_types:t_any());
    #apply_N{} ->
      erl_types:t_any();
    Op when Op =:= call_fun orelse Op =:= enter_fun ->
      [Fun0|TailArgs0] = lists:reverse(Args),
      TailArgs = lists:reverse(TailArgs0),
      Fun = erl_types:t_inf(erl_types:t_fun(), Fun0),
      case erl_types:t_is_fun(Fun) of
	true ->
	  case erl_types:t_fun_args(Fun) of
	    unknown ->
	      erl_types:t_any();
	    FunArgs ->
	      case check_fun_args(FunArgs, TailArgs) of
		ok ->
		  erl_types:t_fun_range(Fun);
		error ->
		  erl_types:t_none()
	      end
	  end;
	false ->
	  erl_types:t_none()
      end;
%%% -----------------------------------------------------
%%% Communication
    check_get_msg ->
      erl_types:t_any();
    clear_timeout ->
      erl_types:t_any();
    next_msg ->
      erl_types:t_any();
    select_msg ->
      erl_types:t_any();
    set_timeout ->
      erl_types:t_any();
    suspend_msg ->
      erl_types:t_any();
%%% -----------------------------------------------------
%%% Other
    #closure_element{} ->
      erl_types:t_any();
    redtest ->
      erl_types:t_any();
    debug_native_called ->
      erl_types:t_any();
    {M, F, A} ->
      erl_bif_types:type(M, F, A, Args)
  end.


-spec type(icode_funcall()) -> erl_types:erl_type().

type(Primop) ->
  case Primop of
%%% -----------------------------------------------------
%%% Arithops
    'bnot' ->
      erl_bif_types:type(erlang, 'bnot', 1);
    '+' ->
      erl_bif_types:type(erlang, '+', 2);
    '-' ->
      erl_bif_types:type(erlang, '-', 2);
    '*' ->
      erl_bif_types:type(erlang, '*', 2);
    '/' ->
      erl_bif_types:type(erlang, '/', 2);
    'div' ->
      erl_bif_types:type(erlang, 'div', 2);
    'rem' ->
      erl_bif_types:type(erlang, 'rem', 2);
    'band' ->
      erl_bif_types:type(erlang, 'band', 2);
    'bor' ->
      erl_bif_types:type(erlang, 'bor', 2);
    'bxor' ->
      erl_bif_types:type(erlang, 'bxor', 2);
    'bsr' ->
      erl_bif_types:type(erlang, 'bsr', 2);
    'bsl' ->
      erl_bif_types:type(erlang, 'bsl', 2);
    unsafe_add ->
      erl_bif_types:type(erlang, '+', 2);
    extra_unsafe_add ->
      erl_bif_types:type(erlang, '+', 2);
    unsafe_sub ->
      erl_bif_types:type(erlang, '-', 2);
    unsafe_bor ->
      erl_bif_types:type(erlang, 'bor', 2);
    unsafe_band ->
      erl_bif_types:type(erlang, 'band', 2);
    unsafe_bxor ->
      erl_bif_types:type(erlang, 'bxor', 2);
%%% -----------------------------------------------------
%%% Lists
    cons ->
      erl_types:t_cons();
    unsafe_hd ->
      erl_bif_types:type(erlang, hd, 1);
    unsafe_tl ->
      erl_bif_types:type(erlang, tl, 1);
%%% -----------------------------------------------------
%%% Tuples
    mktuple ->
      erl_types:t_tuple();
    #element{} ->
      erl_bif_types:type(erlang, element, 2);
    #unsafe_element{} ->
      erl_bif_types:type(erlang, element, 2);
    #unsafe_update_element{} ->
      erl_bif_types:type(erlang, setelement, 3);
%%% -----------------------------------------------------
%%% Floats
    fclearerror ->
      erl_types:t_any();
    fcheckerror ->
      erl_types:t_any();
    unsafe_tag_float ->
      erl_types:t_float();
    %% These might look surprising, but the return is an untagged
    %% float and we have no type for untagged values.
    conv_to_float ->
      erl_types:t_any();
    unsafe_untag_float ->
      erl_types:t_any();
    fp_add ->
      erl_types:t_any();
    fp_sub ->
      erl_types:t_any();
    fp_mul ->
      erl_types:t_any();
    fp_div ->
      erl_types:t_any();
    fnegate ->
      erl_types:t_any();
%%% -----------------------------------------------------
%%% Binaries    
    {hipe_bs_primop, bs_get_utf8} ->
      erl_types:t_product([erl_types:t_integer(), erl_types:t_matchstate()]);
    {hipe_bs_primop, {bs_get_utf16, _Flags}} ->
      erl_types:t_product([erl_types:t_integer(), erl_types:t_matchstate()]);
    {hipe_bs_primop, {bs_get_integer, _Size, _Flags}} ->
      erl_types:t_product([erl_types:t_integer(), erl_types:t_matchstate()]);
    {hipe_bs_primop, {bs_get_float, _, _}} ->
      erl_types:t_product([erl_types:t_float(), erl_types:t_matchstate()]);
    {hipe_bs_primop, {bs_get_binary, _, _}} ->
      erl_types:t_product([erl_types:t_bitstr(), erl_types:t_matchstate()]);
    {hipe_bs_primop, {bs_get_binary_all, _, _}} ->
      erl_types:t_bitstr();
    {hipe_bs_primop, {bs_get_binary_all_2, _, _}} ->
      erl_types:t_product([erl_types:t_bitstr(), erl_types:t_matchstate()]);
    {hipe_bs_primop, bs_final} ->
      erl_types:t_bitstr();
    {hipe_bs_primop, {bs_init, _, _}} ->
      erl_types:t_product([erl_types:t_binary(), erl_types:t_bitstr(),
			   erl_types:t_bitstr()]);
    {hipe_bs_primop, {bs_init, _}} ->
      erl_types:t_product([erl_types:t_binary(), erl_types:t_bitstr(),
			   erl_types:t_bitstr()]);
    {hipe_bs_primop, {bs_init_bits, Size, _}} ->
      erl_types:t_product([erl_types:t_bitstr(0, Size), erl_types:t_bitstr(),
			   erl_types:t_bitstr()]);
    {hipe_bs_primop, {bs_init_bits, _}} ->
      erl_types:t_product([erl_types:t_bitstr(), erl_types:t_bitstr(),
			   erl_types:t_bitstr()]);
    {hipe_bs_primop, {bs_add, _, _}} ->
      erl_types:t_integer();
    {hipe_bs_primop, {bs_add, _}} ->
      erl_types:t_integer();
    {hipe_bs_primop, bs_bits_to_bytes} ->
      erl_types:t_integer();
    {hipe_bs_primop, bs_bits_to_bytes2} ->
      erl_types:t_integer();
    {hipe_bs_primop, {bs_private_append, _U, _Flags}} ->  
      erl_types:t_product(
	[erl_types:t_bitstr(),
	 erl_types:t_any(),
	 erl_types:t_bitstr()]);
    {hipe_bs_primop, {bs_append, _W, _R, _U, _Flags}} ->  
      erl_types:t_product(
	[erl_types:t_bitstr(),
	 erl_types:t_any(),
	 erl_types:t_bitstr()]);
    {hipe_bs_primop, bs_init_writable} ->
      erl_types:t_bitstr();
    {hipe_bs_primop, _BsOp} ->
      erl_types:t_any();
%%% -----------------------------------------------------
%%% Funs
    #mkfun{} ->
      %% Note that the arity includes the bound variables in args
      erl_types:t_fun();
    #apply_N{} ->
      erl_types:t_any();
    call_fun ->
      erl_types:t_any();
    enter_fun ->
      erl_types:t_any();
%%% -----------------------------------------------------
%%% Communication
    check_get_msg ->
      erl_types:t_any();
    clear_timeout ->
      erl_types:t_any();
    next_msg ->
      erl_types:t_any();
    select_msg ->
      erl_types:t_any();
    set_timeout ->
      erl_types:t_any();
    suspend_msg ->
      erl_types:t_any();
%%% -----------------------------------------------------
%%% Other
    #closure_element{} ->
      erl_types:t_any();
    redtest ->
      erl_types:t_any();
    debug_native_called ->
      erl_types:t_any();
    {M, F, A} ->
      erl_bif_types:type(M, F, A)
  end.


%% =====================================================================
%% @doc
%% function arg_types returns a list of the demanded argument types for
%% a bif to succeed.

-spec arg_types(icode_funcall()) -> [erl_types:erl_type()] | 'unknown'.

arg_types(Primop) ->
  case Primop of
    {M, F, A} ->
      erl_bif_types:arg_types(M, F, A);
    #element{} ->
      [erl_types:t_pos_fixnum(), erl_types:t_tuple()];
    '+' ->
      erl_bif_types:arg_types(erlang, '+', 2);
    '-' ->
      erl_bif_types:arg_types(erlang, '-', 2);
    '*' ->
      erl_bif_types:arg_types(erlang, '*', 2);
    '/' ->
      erl_bif_types:arg_types(erlang, '/', 2);
    'band' ->
      erl_bif_types:arg_types(erlang, 'band', 2);
    'bnot' ->
      erl_bif_types:arg_types(erlang, 'bnot', 1);
    'bor' ->
      erl_bif_types:arg_types(erlang, 'bor', 2);
    'bxor' ->
      erl_bif_types:arg_types(erlang, 'bxor', 2);
    'bsl' ->
      erl_bif_types:arg_types(erlang, 'bsl', 2);
    'bsr' ->
      erl_bif_types:arg_types(erlang, 'bsr', 2);
    'div' ->
      erl_bif_types:arg_types(erlang, 'div', 2);
    'rem' ->
      erl_bif_types:arg_types(erlang, 'rem', 2);
    _ ->
      unknown    % safe approximation for all primops.
  end.

%%=====================================================================
%% Auxiliary functions
%%=====================================================================

check_fun_args([T1|Left1], [T2|Left2]) ->
  Inf = erl_types:t_inf(T1, T2),
  case erl_types:t_inf(Inf, T2) of
    Inf ->
      check_fun_args(Left1, Left2);
    _ ->
      error
  end;
check_fun_args([], []) ->
  ok;
check_fun_args(_, _) ->
  error.

match_bin(Pattern, Match) ->
  erl_types:t_bitstr_match(Pattern, Match).

-spec safe_bsl_1(non_neg_integer()) -> non_neg_integer() | 'pos_inf'.

safe_bsl_1(Shift) when Shift =< 128 -> 1 bsl Shift;
safe_bsl_1(_Shift) -> pos_inf.

%%
%% The following two functions are stripped-down versions of more
%% general functions that exist in hipe_icode_range.erl
%%

inf_inv(pos_inf) -> neg_inf;
inf_inv(Number) when is_integer(Number) -> -Number.

inf_add(pos_inf, _Number) -> pos_inf;
inf_add(Number1, Number2) when is_integer(Number1), is_integer(Number2) ->
  Number1 + Number2.
