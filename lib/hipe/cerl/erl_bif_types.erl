%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2010. All Rights Reserved.
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
%% =====================================================================
%% Type information for Erlang Built-in functions (implemented in C)
%%
%% Copyright (C) 2002 Richard Carlsson
%% Copyright (C) 2006 Richard Carlsson, Tobias Lindahl and Kostis Sagonas
%%
%% Author contact: richardc@it.uu.se, tobiasl@it.uu.se, kostis@it.uu.se
%% =====================================================================

-module(erl_bif_types).

%-define(BITS, (hipe_rtl_arch:word_size() * 8) - ?TAG_IMMED1_SIZE).
-define(BITS, 128). %This is only in bsl to convert answer to pos_inf/neg_inf.
-define(TAG_IMMED1_SIZE, 4).

-export([type/3, type/4, arg_types/3, 
	 is_known/3, structure_inspecting_args/3, infinity_add/2]).

-import(erl_types, [number_max/1,
		    number_min/1,
		    t_any/0,
		    t_arity/0,
		    t_atom/0,
		    t_atom/1,
		    t_atoms/1,
		    t_atom_vals/1,
		    t_binary/0,
		    t_bitstr/0,
		    t_boolean/0,
		    t_byte/0,
		    t_char/0,
		    t_cons/0,
		    t_cons/2,
		    t_cons_hd/1,
		    t_cons_tl/1,
		    t_constant/0,
		    t_fixnum/0,
		    t_non_neg_fixnum/0,
		    t_pos_fixnum/0,
		    t_float/0,
		    t_from_range/2,
		    t_from_term/1,
		    t_fun/0,
		    t_fun/2,
		    t_fun_args/1,
		    t_fun_range/1,
		    t_identifier/0,
		    t_inf/2,
		    t_integer/0,
		    t_integer/1,
		    t_non_neg_fixnum/0,
		    t_non_neg_integer/0,
		    t_pos_integer/0,
		    t_integers/1,
		    t_iodata/0,
		    t_iolist/0,
		    t_is_any/1,
		    t_is_atom/1,
		    t_is_binary/1,
		    t_is_bitstr/1,
		    t_is_boolean/1,
		    t_is_cons/1,
		    t_is_constant/1,
		    t_is_float/1,
		    t_is_float/1,
		    t_is_fun/1,
		    t_is_integer/1,
		    t_is_integer/1,
		    t_is_list/1,
		    t_is_nil/1,
		    t_is_none/1,
		    t_is_none_or_unit/1,
		    t_is_number/1,
		    t_is_pid/1,
		    t_is_port/1,
		    t_is_maybe_improper_list/1,
		    t_is_reference/1,
		    t_is_string/1,
		    t_is_subtype/2,
		    t_is_tuple/1,
		    t_list/0,
		    t_list/1,
		    t_list_elements/1,
		    t_list_termination/1,
		    t_mfa/0,
		    t_module/0,
		    t_nil/0,
		    t_node/0,
		    t_none/0,
		    t_nonempty_list/0,
		    t_nonempty_list/1,
		    t_number/0,
		    t_number_vals/1,
		    t_pid/0,
		    t_port/0,
		    t_maybe_improper_list/0,
		    t_reference/0,
		    t_string/0,
		    t_subtract/2,
		    t_sup/1,
		    t_sup/2,
		    t_tid/0,
		    t_timeout/0,
		    t_tuple/0,
		    t_tuple/1,
		    t_tuple_args/1,
		    t_tuple_size/1,
		    t_tuple_subtypes/1
		   ]).

-ifdef(DO_ERL_BIF_TYPES_TEST).
-export([test/0]).
-endif.

%%=============================================================================

-spec type(atom(), atom(), arity()) -> erl_types:erl_type().

type(M, F, A) ->
  type(M, F, A, any_list(A)).

%% Arguments should be checked for undefinedness, so we do not make
%% unnecessary overapproximations.

-spec type(atom(), atom(), arity(), [erl_types:erl_type()]) -> erl_types:erl_type().

%%-- binary -------------------------------------------------------------------
type(binary, at, 2, Xs) ->
  strict(arg_types(binary, at, 2), Xs, fun(_) -> t_integer() end);
type(binary, bin_to_list, Arity, Xs) when 1 =< Arity, Arity =< 3 ->
  strict(arg_types(binary, bin_to_list, Arity), Xs,
	 fun(_) -> t_list(t_integer()) end);
type(binary, compile_pattern, 1, Xs) ->
  strict(arg_types(binary, compile_pattern, 1), Xs,
	 fun(_) -> t_tuple([t_atom(bm),t_binary()]) end);
type(binary, copy, Arity, Xs)  when Arity =:= 1; Arity =:= 2 ->
  strict(arg_types(binary, copy, Arity), Xs,
	 fun(_) -> t_binary() end);
type(binary, decode_unsigned, Arity, Xs)  when Arity =:= 1; Arity =:= 2 ->
  strict(arg_types(binary, decode_unsigned, Arity), Xs,
	 fun(_) -> t_non_neg_integer() end);
type(binary, encode_unsigned, Arity, Xs)  when Arity =:= 1; Arity =:= 2 ->
  strict(arg_types(binary, encode_unsigned, Arity), Xs,
	 fun(_) -> t_binary() end);
type(binary, first, 1, Xs) ->
  strict(arg_types(binary, first, 1), Xs, fun(_) -> t_non_neg_integer() end);
type(binary, last, 1, Xs) ->
  strict(arg_types(binary, last, 1), Xs, fun(_) -> t_non_neg_integer() end);
type(binary, list_to_bin, 1, Xs) ->
  type(erlang, list_to_binary, 1, Xs);
type(binary, longest_common_prefix, 1, Xs) ->
  strict(arg_types(binary, longest_common_prefix, 1), Xs,
	 fun(_) -> t_integer() end);
type(binary, longest_common_suffix, 1, Xs) ->
  strict(arg_types(binary, longest_common_suffix, 1), Xs,
	 fun(_) -> t_integer() end);
type(binary, match, Arity, Xs) when Arity =:= 2; Arity =:= 3 ->
  strict(arg_types(binary, match, Arity), Xs,
	 fun(_) ->
	     t_sup(t_atom('nomatch'), t_binary_canonical_part())
	 end);
type(binary, matches, Arity, Xs) when Arity =:= 2; Arity =:= 3 ->
  strict(arg_types(binary, matches, Arity), Xs,
	 fun(_) -> t_list(t_binary_canonical_part()) end);
type(binary, part, 2, Xs) ->
  type(erlang, binary_part, 2, Xs);
type(binary, part, 3, Xs) ->
  type(erlang, binary_part, 3, Xs);
type(binary, referenced_byte_size, 1, Xs) ->
  strict(arg_types(binary, referenced_byte_size, 1), Xs,
	 fun(_) -> t_non_neg_integer() end);
%%-- code ---------------------------------------------------------------------
type(code, add_path, 1, Xs) ->
  strict(arg_types(code, add_path, 1), Xs,
	 fun (_) ->
	     t_sup(t_boolean(),
		   t_tuple([t_atom('error'), t_atom('bad_directory')]))
	 end);
type(code, add_patha, 1, Xs) ->
  type(code, add_path, 1, Xs);
type(code, add_paths, 1, Xs) ->
  strict(arg_types(code, add_paths, 1), Xs, fun(_) -> t_atom('ok') end);
type(code, add_pathsa, 1, Xs) ->
  type(code, add_paths, 1, Xs);
type(code, add_pathsz, 1, Xs) ->
  type(code, add_paths, 1, Xs);
type(code, add_pathz, 1, Xs) ->
  type(code, add_path, 1, Xs);
type(code, all_loaded, 0, _) ->
  t_list(t_tuple([t_atom(), t_code_loaded_fname_or_status()]));
type(code, compiler_dir, 0, _) ->
  t_string();
type(code, del_path, 1, Xs) ->
  strict(arg_types(code, del_path, 1), Xs,
	 fun (_) ->
	     t_sup(t_boolean(),
		   t_tuple([t_atom('error'), t_atom('bad_name')]))
	 end);
type(code, delete, 1, Xs) ->
  strict(arg_types(code, delete, 1), Xs, fun (_) -> t_boolean() end);
type(code, ensure_loaded, 1, Xs) ->
  type(code, load_file, 1, Xs);
type(code, get_chunk, 2, Xs) ->
  strict(arg_types(code, get_chunk, 2), Xs,
	 fun (_) -> t_sup(t_binary(), t_atom('undefined')) end);
type(code, get_object_code, 1, Xs) ->
  strict(arg_types(code, get_object_code, 1), Xs,
	 fun (_) ->
	     t_sup(t_tuple([t_atom(), t_binary(), t_string()]),
		   t_atom('error'))
	 end);
type(code, get_path, 0, _) ->
  t_list(t_string());
type(code, is_loaded, 1, Xs) ->
  strict(arg_types(code, is_loaded, 1), Xs,
	 fun (_) ->
	     t_sup([t_tuple([t_atom('file'), t_code_loaded_fname_or_status()]),
		    t_atom('false')])
	 end);
type(code, is_sticky, 1, Xs) ->
  strict(arg_types(code, is_sticky, 1), Xs, fun (_) -> t_boolean() end);
type(code, is_module_native, 1, Xs) ->
  strict(arg_types(code, is_module_native, 1), Xs,
	 fun (_) -> t_sup(t_boolean(), t_atom('undefined')) end);
type(code, lib_dir, 0, _) ->
  t_string();
type(code, lib_dir, 1, Xs) ->
  strict(arg_types(code, lib_dir, 1), Xs,
	 fun (_) ->
	     t_sup(t_string(),
 		   t_tuple([t_atom('error'), t_atom('bad_name')]))
 	 end);
type(code, load_abs, 1, Xs) ->
  strict(arg_types(code, load_abs, 1), Xs,
	 fun ([_File]) -> t_code_load_return(t_atom()) end);	% XXX: cheating
type(code, load_abs, 2, Xs) ->
  strict(arg_types(code, load_abs, 2), Xs,
	 fun ([_File,Mod]) -> t_code_load_return(Mod) end);
type(code, load_binary, 3, Xs) ->
  strict(arg_types(code, load_binary, 3), Xs,
	 fun ([Mod,_File,_Bin]) -> t_code_load_return(Mod) end);
type(code, load_file, 1, Xs) ->
  strict(arg_types(code, load_file, 1), Xs,
	 fun ([Mod]) -> t_code_load_return(Mod) end);
type(code, load_native_partial, 2, Xs) ->
  strict(arg_types(code, load_native_partial, 2), Xs,
	 fun ([Mod,_Bin]) -> t_code_load_return(Mod) end);
type(code, load_native_sticky, 3, Xs) ->
  strict(arg_types(code, load_native_sticky, 3), Xs,
        fun ([Mod,_Bin,_]) -> t_code_load_return(Mod) end);
type(code, module_md5, 1, Xs) ->
  strict(arg_types(code, module_md5, 1), Xs,
	 fun (_) -> t_sup(t_binary(), t_atom('undefined')) end);
type(code, make_stub_module, 3, Xs) ->
  strict(arg_types(code, make_stub_module, 3), Xs, fun ([Mod,_,_]) -> Mod end);
type(code, priv_dir, 1, Xs) ->
  strict(arg_types(code, priv_dir, 1), Xs,
	 fun (_) ->
	     t_sup(t_string(), t_tuple([t_atom('error'), t_atom('bad_name')]))
	 end);
type(code, purge, 1, Xs) ->
  type(code, delete, 1, Xs);
type(code, rehash, 0, _) -> t_atom('ok');
type(code, replace_path, 2, Xs) ->
  strict(arg_types(code, replace_path, 2), Xs,
	 fun (_) ->
	     t_sup([t_atom('true'),
		    t_tuple([t_atom('error'), t_atom('bad_name')]),
		    t_tuple([t_atom('error'), t_atom('bad_directory')]),
		    t_tuple([t_atom('error'),
			     t_tuple([t_atom('badarg'), t_any()])])])
	 end);
type(code, root_dir, 0, _) ->
  t_string();
type(code, set_path, 1, Xs) ->
  strict(arg_types(code, set_path, 1), Xs,
	 fun (_) ->
	     t_sup([t_atom('true'),
		    t_tuple([t_atom('error'), t_atom('bad_path')]),
		    t_tuple([t_atom('error'), t_atom('bad_directory')])])
	 end);
type(code, soft_purge, 1, Xs) ->
  type(code, delete, 1, Xs);
type(code, stick_mod, 1, Xs) ->
  strict(arg_types(code, stick_mod, 1), Xs, fun (_) -> t_atom('true') end);
type(code, unstick_mod, 1, Xs) ->
  type(code, stick_mod, 1, Xs);
type(code, which, 1, Xs) ->
  strict(arg_types(code, which, 1), Xs,
	 fun (_) ->
	     t_sup([t_code_loaded_fname_or_status(),
		    t_atom('non_existing')])
	 end);
%%-- erl_ddll -----------------------------------------------------------------
type(erl_ddll, demonitor, 1, Xs) ->
  type(erlang, demonitor, 1, Xs);
type(erl_ddll, format_error_int, 1, Xs) ->
  strict(arg_types(erl_ddll, format_error_int, 1), Xs,
	 fun (_) -> t_string() end);
type(erl_ddll, info, 2, Xs) ->
  strict(arg_types(erl_ddll, info, 2), Xs, fun (_) -> t_atom() end);
type(erl_ddll, loaded_drivers, 0, _) ->
  t_tuple([t_atom('ok'), t_list(t_string())]);
type(erl_ddll, monitor, 2, Xs) -> % return type is the same, though args are not
  type(erlang, monitor, 2, Xs);
type(erl_ddll, try_load, 3, Xs) ->
  strict(arg_types(erl_ddll, try_load, 3), Xs,
	 fun (_) ->
	     t_sup([t_tuple([t_atom('ok'), t_atom('already_loaded')]),
		    t_tuple([t_atom('ok'), t_atom('loaded')]),
		    t_tuple([t_atom('ok'),
			     t_atom('pending_driver'), t_reference()]),
		    t_tuple([t_atom('error'), t_atom('inconsistent')]),
		    t_tuple([t_atom('error'), t_atom('permanent')])])
	 end);
type(erl_ddll, try_unload, 2, Xs) ->
  strict(arg_types(erl_ddll, try_unload, 2), Xs,
	 fun (_) ->
	     t_sup([t_tuple([t_atom('ok'), t_atom('pending_process')]),
		    t_tuple([t_atom('ok'), t_atom('unloaded')]),
		    t_tuple([t_atom('ok'), t_atom('pending_driver')]),
		    t_tuple([t_atom('ok'),
			     t_atom('pending_driver'), t_reference()]),
		    t_tuple([t_atom('error'), t_atom('permanent')]),
		    t_tuple([t_atom('error'), t_atom('not_loaded')]),
		    t_tuple([t_atom('error'),
			     t_atom('not_loaded_by_this_process')])])
	 end);
%%-- erlang -------------------------------------------------------------------
type(erlang, halt, 0, _) -> t_none();
type(erlang, halt, 1, _) -> t_none();
type(erlang, exit, 1, _) -> t_none();
%% Note that exit/2 sends an exit signal to another process.
type(erlang, exit, 2, _) -> t_atom('true');
type(erlang, error, 1, _) -> t_none();
type(erlang, error, 2, _) -> t_none();
type(erlang, throw, 1, _) -> t_none();
type(erlang, hibernate, 3, _) -> t_none();
type(erlang, '==', 2, Xs = [X1, X2]) ->
  case t_is_atom(X1) andalso t_is_atom(X2) of
    true -> type(erlang, '=:=', 2, Xs);
    false ->
      case t_is_integer(X1) andalso t_is_integer(X2) of
	true -> type(erlang, '=:=', 2, Xs);
	false -> strict(Xs, t_boolean())
      end
  end;
type(erlang, '/=', 2, Xs = [X1, X2]) -> 
  case t_is_atom(X1) andalso t_is_atom(X2) of
    true -> type(erlang, '=/=', 2, Xs);
    false ->
      case t_is_integer(X1) andalso t_is_integer(X2) of
	true -> type(erlang, '=/=', 2, Xs);
	false -> strict(Xs, t_boolean())
      end
  end;
type(erlang, '=:=', 2, Xs = [Lhs, Rhs]) -> 
  Ans =
    case t_is_none(t_inf(Lhs, Rhs)) of
      true -> t_atom('false');
      false ->
	case t_is_atom(Lhs) andalso t_is_atom(Rhs) of
	  true ->
	    case {t_atom_vals(Lhs), t_atom_vals(Rhs)} of
	      {unknown, _} -> t_boolean();
	      {_, unknown} -> t_boolean();
	      {[X], [X]} -> t_atom('true');
	      {LhsVals, RhsVals} ->
		case lists:all(fun({X, Y}) -> X =/= Y end, 
			       [{X, Y} || X <- LhsVals, Y <- RhsVals]) of
		  true -> t_atom('false');
		  false -> t_boolean()
		end
	    end;
	  false ->
	    case t_is_integer(Lhs) andalso t_is_integer(Rhs) of
	      false -> t_boolean();
	      true ->
		case {t_number_vals(Lhs), t_number_vals(Rhs)} of
		  {[X], [X]} when is_integer(X) -> t_atom('true');
		  _ ->
		    LhsMax = number_max(Lhs),
		    LhsMin = number_min(Lhs),
		    RhsMax = number_max(Rhs),
		    RhsMin = number_min(Rhs),
		    Ans1 = (is_integer(LhsMin) 
			    andalso is_integer(RhsMax)
			    andalso (LhsMin > RhsMax)),
		    Ans2 = (is_integer(LhsMax) 
			    andalso is_integer(RhsMin)
			    andalso (RhsMin > LhsMax)),
		    case Ans1 orelse Ans2 of
		      true -> t_atom('false');
		      false -> t_boolean()
		    end
		end
	    end
	end
    end,
  strict(Xs, Ans);
type(erlang, '=/=', 2, Xs = [Lhs, Rhs]) ->
  Ans =
    case t_is_none(t_inf(Lhs, Rhs)) of
      true -> t_atom('true');
      false ->
	case t_is_atom(Lhs) andalso t_is_atom(Rhs) of
	  true ->
	    case {t_atom_vals(Lhs), t_atom_vals(Rhs)} of
	      {unknown, _} -> t_boolean();
	      {_, unknown} -> t_boolean();
	      {[Val], [Val]} -> t_atom('false');
	      {LhsVals, RhsVals} ->
		t_sup([t_from_term(X =/= Y) || X <- LhsVals, Y <- RhsVals])
	    end;
	  false ->
	    case t_is_integer(Lhs) andalso t_is_integer(Rhs) of
	      false -> t_boolean();
	      true ->
		LhsMax = number_max(Lhs),
		LhsMin = number_min(Lhs),
		RhsMax = number_max(Rhs),
		RhsMin = number_min(Rhs),
		Ans1 = (is_integer(LhsMin) andalso is_integer(RhsMax)
			andalso (LhsMin > RhsMax)),
		Ans2 = (is_integer(LhsMax) andalso is_integer(RhsMin)
			andalso (RhsMin > LhsMax)),
		case Ans1 orelse Ans2 of
		  true -> t_atom('true');
		  false -> 
		    if LhsMax =:= LhsMin, 
		       RhsMin =:= RhsMax, 
		       RhsMax =:= LhsMax -> t_atom('false');
		       true -> t_boolean()
		    end
		end
	    end
	end
    end,
  strict(Xs, Ans);
type(erlang, '>', 2, Xs = [Lhs, Rhs]) -> 
  Ans =
    case t_is_integer(Lhs) andalso t_is_integer(Rhs) of
      true ->
	LhsMax = number_max(Lhs),
	LhsMin = number_min(Lhs),
	RhsMax = number_max(Rhs),
	RhsMin = number_min(Rhs),
	T = t_atom('true'),
	F = t_atom('false'),
	if 
	  is_integer(LhsMin), is_integer(RhsMax), LhsMin > RhsMax -> T;
	  is_integer(LhsMax), is_integer(RhsMin), RhsMin >= LhsMax -> F;
	  true -> t_boolean()
	end;
      false -> t_boolean()
    end,
  strict(Xs, Ans);
type(erlang, '>=', 2, Xs = [Lhs, Rhs]) ->
  Ans =
    case t_is_integer(Lhs) andalso t_is_integer(Rhs) of
      true ->
	LhsMax = number_max(Lhs),
	LhsMin = number_min(Lhs),
	RhsMax = number_max(Rhs),
	RhsMin = number_min(Rhs),
	T = t_atom('true'),
	F = t_atom('false'),
	if 
	  is_integer(LhsMin), is_integer(RhsMax), LhsMin >= RhsMax -> T;
	  is_integer(LhsMax), is_integer(RhsMin), RhsMin > LhsMax -> F;
	  true -> t_boolean()
	end;
      false -> t_boolean()
    end,
  strict(Xs, Ans);
type(erlang, '<', 2, Xs = [Lhs, Rhs]) ->
  Ans =
    case t_is_integer(Lhs) andalso t_is_integer(Rhs) of
      true ->
	LhsMax = number_max(Lhs),
	LhsMin = number_min(Lhs),
	RhsMax = number_max(Rhs),
	RhsMin = number_min(Rhs),
	T = t_atom('true'),
	F = t_atom('false'),
	if 
	  is_integer(LhsMax), is_integer(RhsMin), LhsMax < RhsMin -> T;
	  is_integer(LhsMin), is_integer(RhsMax), RhsMax =< LhsMin -> F;
	  true -> t_boolean()
	end;
      false -> t_boolean()
    end,
  strict(Xs, Ans);
type(erlang, '=<', 2, Xs = [Lhs, Rhs]) ->
  Ans =
    case t_is_integer(Lhs) andalso t_is_integer(Rhs) of
      true ->
	LhsMax = number_max(Lhs),
	LhsMin = number_min(Lhs),
	RhsMax = number_max(Rhs),
	RhsMin = number_min(Rhs),
	T = t_atom('true'),
	F = t_atom('false'),
	if 
	  is_integer(LhsMax), is_integer(RhsMin), LhsMax =< RhsMin -> T;
	  is_integer(LhsMin), is_integer(RhsMax), RhsMax < LhsMin -> F;
	  true -> t_boolean()
	end;
      false -> t_boolean()
    end,
  strict(Xs, Ans);
type(erlang, '+', 1, Xs) ->
  strict(arg_types(erlang, '+', 1), Xs, 
	 fun ([X]) -> X end);
type(erlang, '-', 1, Xs) ->
  strict(arg_types(erlang, '-', 1), Xs, 
	 fun ([X]) -> 
	     case t_is_integer(X) of
	       true -> type(erlang, '-', 2, [t_integer(0), X]);
	       false -> X
	     end
	 end);
type(erlang, '!', 2, Xs) ->
  strict(arg_types(erlang, '!', 2), Xs, fun ([_, X2]) -> X2 end);
type(erlang, '+', 2, Xs) ->
  strict(arg_types(erlang, '+', 2), Xs,
	 fun ([X1, X2]) ->
	     case arith('+', X1, X2) of
	       {ok, T} -> T;
	       error ->
		 case t_is_float(X1) orelse t_is_float(X2) of
		   true -> t_float();
		   false -> t_number()
		 end
	     end
	 end);
type(erlang, '-', 2, Xs) ->
  strict(arg_types(erlang, '-', 2), Xs,
	 fun ([X1, X2]) ->
	     case arith('-', X1, X2) of
	       {ok, T} -> T;
	       error ->
		 case t_is_float(X1) orelse t_is_float(X2) of
		   true -> t_float();
		   false -> t_number()
		 end
	     end
	 end);
type(erlang, '*', 2, Xs) ->
  strict(arg_types(erlang, '*', 2), Xs,
	 fun ([X1, X2]) ->
	     case arith('*', X1, X2) of
	       {ok, T} -> T;
	       error ->
		 case t_is_float(X1) orelse t_is_float(X2) of
		   true -> t_float();
		   false -> t_number()
		 end
	     end
	 end);
type(erlang, '/', 2, Xs) ->
  strict(arg_types(erlang, '/', 2), Xs,
	 fun (_) -> t_float() end);
type(erlang, 'div', 2, Xs) ->
  strict(arg_types(erlang, 'div', 2), Xs,
	 fun ([X1, X2]) ->
	     case arith('div', X1, X2) of
	       error -> t_integer();
	       {ok, T} -> T
	     end
	 end);
type(erlang, 'rem', 2, Xs) ->
  strict(arg_types(erlang, 'rem', 2), Xs,
	 fun ([X1, X2]) ->
	     case arith('rem', X1, X2) of
	       error -> t_non_neg_integer();
	       {ok, T} -> T
	     end
	 end);
type(erlang, '++', 2, Xs) ->
  strict(arg_types(erlang, '++', 2), Xs,
	 fun ([X1, X2]) ->
	     case t_is_nil(X1) of
	       true  -> X2;    % even if X2 is not a list
	       false ->
		 case t_is_nil(X2) of
		   true  -> X1;
		   false ->
		     E1 = t_list_elements(X1),
		     case t_is_cons(X1) of
		       true -> t_cons(E1, X2);
		       false ->
			 t_sup(X2, t_cons(E1, X2))
		     end
		 end
	     end
	 end);
type(erlang, '--', 2, Xs) ->
  %% We don't know which elements (if any) in X2 will be found and
  %% removed from X1, even if they would have the same type. Thus, we
  %% must assume that X1 can remain unchanged. However, if we succeed,
  %% we know that X1 must be a proper list, but the result could
  %% possibly be empty even if X1 is nonempty.
  strict(arg_types(erlang, '--', 2), Xs,
	 fun ([X1, X2]) ->
	     case t_is_nil(X1) of
	       true  -> t_nil();
	       false ->
		 case t_is_nil(X2) of
		   true  -> X1;
		   false -> t_list(t_list_elements(X1))
		 end
	     end
	 end);
type(erlang, 'and', 2, Xs) ->
  strict(arg_types(erlang, 'and', 2), Xs, fun (_) -> t_boolean() end);
type(erlang, 'or', 2, Xs) ->
  strict(arg_types(erlang, 'or', 2), Xs, fun (_) -> t_boolean() end);
type(erlang, 'xor', 2, Xs) ->
  strict(arg_types(erlang, 'xor', 2), Xs, fun (_) -> t_boolean() end);
type(erlang, 'not', 1, Xs) ->
  strict(arg_types(erlang, 'not', 1), Xs, fun (_) -> t_boolean() end);
type(erlang, 'band', 2, Xs) ->
  strict(arg_types(erlang, 'band', 2), Xs,
	 fun ([X1, X2]) ->
	     case arith('band', X1, X2) of
	       error -> t_integer();
	       {ok, T} -> T
	     end
	 end);
%% The result is not wider than the smallest argument. We need to
%% kill any value-sets in the result.
%%  strict(arg_types(erlang, 'band', 2), Xs,
%%	 fun ([X1, X2]) -> t_sup(t_inf(X1, X2), t_byte()) end);
type(erlang, 'bor', 2, Xs) ->
  strict(arg_types(erlang, 'bor', 2), Xs,
	 fun ([X1, X2]) ->
	     case arith('bor', X1, X2) of
	       error -> t_integer();
	       {ok, T} -> T
	     end
	 end);
%% The result is not wider than the largest argument. We need to
%% kill any value-sets in the result.
%%  strict(arg_types(erlang, 'bor', 2), Xs,
%%	 fun ([X1, X2]) -> t_sup(t_sup(X1, X2), t_byte()) end);
type(erlang, 'bxor', 2, Xs) ->
  strict(arg_types(erlang, 'bxor', 2), Xs,
	 fun ([X1, X2]) ->
	     case arith('bxor', X1, X2) of
	       error -> t_integer();
	       {ok, T} -> T
	     end
	 end);
%% The result is not wider than the largest argument. We need to
%% kill any value-sets in the result.
%%  strict(arg_types(erlang, 'bxor', 2), Xs,
%%	 fun ([X1, X2]) -> t_sup(t_sup(X1, X2), t_byte()) end);
type(erlang, 'bsr', 2, Xs) ->
  strict(arg_types(erlang, 'bsr', 2), Xs,
	 fun ([X1, X2]) ->
	     case arith('bsr', X1, X2) of
	       error -> t_integer();
	       {ok, T} -> T
	     end
	 end);
%% If the first argument is unsigned (which is the case for
%% characters and bytes), the result is never wider. We need to kill
%% any value-sets in the result.
%%  strict(arg_types(erlang, 'bsr', 2), Xs,
%%	 fun ([X, _]) -> t_sup(X, t_byte()) end);
type(erlang, 'bsl', 2, Xs) ->
  strict(arg_types(erlang, 'bsl', 2), Xs,
	 fun ([X1, X2]) ->
	     case arith('bsl', X1, X2) of
	       error -> t_integer();
	       {ok, T} -> T
	     end
	 end);
%% Not worth doing anything special here.
%%  strict(arg_types(erlang, 'bsl', 2), Xs, fun (_) -> t_integer() end);
type(erlang, 'bnot', 1, Xs) ->
 strict(arg_types(erlang, 'bnot', 1), Xs,
	 fun ([X1]) ->
	     case arith('bnot', X1) of
	       error -> t_integer();
	       {ok, T} -> T
	     end
	 end);
%% This returns (-X)-1, so it often gives a negative result.
%%  strict(arg_types(erlang, 'bnot', 1), Xs, fun (_) -> t_integer() end);
type(erlang, abs, 1, Xs) ->
  strict(arg_types(erlang, abs, 1), Xs, fun ([X]) -> X end);
type(erlang, adler32, 1, Xs) ->
  strict(arg_types(erlang, adler32, 1), Xs, fun (_) -> t_adler32() end);
type(erlang, adler32, 2, Xs) ->
  strict(arg_types(erlang, adler32, 2), Xs, fun (_) -> t_adler32() end);
type(erlang, adler32_combine, 3, Xs) ->
  strict(arg_types(erlang, adler32_combine, 3), Xs,
	 fun (_) -> t_adler32() end);
type(erlang, append, 2, Xs) -> type(erlang, '++', 2, Xs); % alias
type(erlang, append_element, 2, Xs) ->
  strict(arg_types(erlang, append_element, 2), Xs, fun (_) -> t_tuple() end);
type(erlang, apply, 2, Xs) ->
  Fun = fun ([X, _Y]) -> 
	    case t_is_fun(X) of
	      true ->
		t_fun_range(X);
	      false ->
		t_any() 
	    end
	end,
  strict(arg_types(erlang, apply, 2), Xs, Fun);
type(erlang, apply, 3, Xs) ->
  strict(arg_types(erlang, apply, 3), Xs, fun (_) -> t_any() end);
type(erlang, atom_to_binary, 2, Xs) ->
  strict(arg_types(erlang, atom_to_binary, 2), Xs, fun (_) -> t_binary() end);
type(erlang, atom_to_list, 1, Xs) ->
  strict(arg_types(erlang, atom_to_list, 1), Xs, fun (_) -> t_string() end);
type(erlang, binary_part, 2, Xs) ->
  strict(arg_types(erlang, binary_part, 2), Xs, fun (_) -> t_binary() end);
type(erlang, binary_part, 3, Xs) ->
  strict(arg_types(erlang, binary_part, 3), Xs, fun (_) -> t_binary() end);
type(erlang, binary_to_atom, 2, Xs) ->
  strict(arg_types(erlang, binary_to_atom, 2), Xs, fun (_) -> t_atom() end);
type(erlang, binary_to_existing_atom, 2, Xs) ->
  type(erlang, binary_to_atom, 2, Xs);
type(erlang, binary_to_list, 1, Xs) ->
  strict(arg_types(erlang, binary_to_list, 1), Xs,
	 fun (_) -> t_list(t_byte()) end);
type(erlang, binary_to_list, 3, Xs) ->
  strict(arg_types(erlang, binary_to_list, 3), Xs,
	 fun (_) -> t_list(t_byte()) end);
type(erlang, binary_to_term, 1, Xs) ->
  strict(arg_types(erlang, binary_to_term, 1), Xs, fun (_) -> t_any() end);
type(erlang, binary_to_term, 2, Xs) ->
  strict(arg_types(erlang, binary_to_term, 2), Xs, fun (_) -> t_any() end);
type(erlang, bitsize, 1, Xs) ->	% XXX: TAKE OUT
  type(erlang, bit_size, 1, Xs);
type(erlang, bit_size, 1, Xs) ->
  strict(arg_types(erlang, bit_size, 1), Xs,
	 fun (_) -> t_non_neg_integer() end);
type(erlang, bitstr_to_list, 1, Xs) ->	% XXX: TAKE OUT
  type(erlang, bitstring_to_list, 1, Xs);
type(erlang, bitstring_to_list, 1, Xs) ->
  strict(arg_types(erlang, bitstring_to_list, 1), Xs,
	 fun (_) -> t_list(t_sup(t_byte(), t_bitstr())) end);
type(erlang, bump_reductions, 1, Xs) ->
  strict(arg_types(erlang, bump_reductions, 1), Xs,
	 fun (_) -> t_atom('true') end);
type(erlang, byte_size, 1, Xs) ->
  strict(arg_types(erlang, byte_size, 1), Xs,
	 fun (_) -> t_non_neg_integer() end);
type(erlang, call_on_load_function, 1, Xs) ->
  %% Internal BIF used by on_load.
  strict(arg_types(erlang, call_on_load_function, 1), Xs,
	 fun (_) -> t_any() end);
type(erlang, cancel_timer, 1, Xs) ->
  strict(arg_types(erlang, cancel_timer, 1), Xs,
	 fun (_) -> t_sup(t_integer(), t_atom('false')) end);
type(erlang, check_process_code, 2, Xs) ->
  strict(arg_types(erlang, check_process_code, 2), Xs,
	 fun (_) -> t_boolean() end);
type(erlang, concat_binary, 1, Xs) ->
  strict(arg_types(erlang, concat_binary, 1), Xs, fun (_) -> t_binary() end);
type(erlang, crc32, 1, Xs) ->
  strict(arg_types(erlang, crc32, 1), Xs, fun (_) -> t_crc32() end);
type(erlang, crc32, 2, Xs) ->
  strict(arg_types(erlang, crc32, 2), Xs, fun (_) -> t_crc32() end);
type(erlang, crc32_combine, 3, Xs) ->
  strict(arg_types(erlang, crc32_combine, 3), Xs, fun (_) -> t_crc32() end);
type(erlang, date, 0, _) ->
  t_date();
type(erlang, decode_packet, 3, Xs) ->
  strict(arg_types(erlang, decode_packet, 3), Xs,
	 fun (_) ->
	     t_sup([t_tuple([t_atom('ok'), t_packet(), t_binary()]),
		    t_tuple([t_atom('more'), t_sup([t_non_neg_integer(),
						    t_atom('undefined')])]),
		    t_tuple([t_atom('error'), t_any()])])
	 end);
type(erlang, delete_module, 1, Xs) ->
  strict(arg_types(erlang, delete_module, 1), Xs,
 	 fun (_) -> t_sup(t_atom('true'), t_atom('undefined')) end);
type(erlang, demonitor, 1, Xs) ->
  strict(arg_types(erlang, demonitor, 1), Xs, fun (_) -> t_atom('true') end);
%% TODO: overapproximation -- boolean only if 'info' is part of arg2 otherwise 'true'
type(erlang, demonitor, 2, Xs) ->
  strict(arg_types(erlang, demonitor, 2), Xs, fun (_) -> t_boolean() end);
type(erlang, disconnect_node, 1, Xs) ->
  strict(arg_types(erlang, disconnect_node, 1), Xs, fun (_) -> t_boolean() end);
type(erlang, display, 1, _) -> t_atom('true');
type(erlang, display_string, 1, Xs) ->
  strict(arg_types(erlang, display_string, 1), Xs, fun(_) -> t_atom('true') end);
type(erlang, display_nl, 0, _) ->
  t_atom('true');
type(erlang, dist_exit, 3, Xs) ->
  strict(arg_types(erlang, dist_exit, 3), Xs, fun (_) -> t_atom('true') end);
type(erlang, element, 2, Xs) ->
  strict(arg_types(erlang, element, 2), Xs,
	 fun ([X1, X2]) ->
	     case t_tuple_subtypes(X2) of
	       unknown -> t_any();
	       [_] ->
		 Sz = t_tuple_size(X2),
		 As = t_tuple_args(X2),
		 case t_number_vals(X1) of
		   unknown -> t_sup(As);
		   Ns when is_list(Ns) ->
		     Fun = fun 
			     (N, X) when is_integer(N), 1 =< N, N =< Sz ->
			       t_sup(X, lists:nth(N, As));
			     (_, X) ->
			       X
			   end,
		     lists:foldl(Fun, t_none(), Ns)
		 end;
	       Ts when is_list(Ts) ->
		 t_sup([type(erlang, element, 2, [X1, Y]) || Y <- Ts])
	     end
	 end);
type(erlang, erase, 0, _) -> t_any();
type(erlang, erase, 1, _) -> t_any();
type(erlang, external_size, 1, _) -> t_integer();
type(erlang, finish_after_on_load, 2, Xs) ->
  %% Internal BIF used by on_load.
  strict(arg_types(erlang, finish_after_on_load, 2), Xs,
	 fun (_) -> t_atom('true') end);
type(erlang, float, 1, Xs) ->
  strict(arg_types(erlang, float, 1), Xs, fun (_) -> t_float() end);
type(erlang, float_to_list, 1, Xs) ->
  strict(arg_types(erlang, float_to_list, 1), Xs, fun (_) -> t_string() end);
type(erlang, function_exported, 3, Xs) ->
  strict(arg_types(erlang, function_exported, 3), Xs,
	 fun (_) -> t_boolean() end);
type(erlang, fun_info, 1, Xs) ->
  strict(arg_types(erlang, fun_info, 1), Xs,
	 fun (_) -> t_list(t_tuple([t_atom(), t_any()])) end);
type(erlang, fun_info, 2, Xs) ->
  strict(arg_types(erlang, fun_info, 2), Xs,
	 fun (_) -> t_tuple([t_atom(), t_any()]) end);
type(erlang, fun_to_list, 1, Xs) ->
  strict(arg_types(erlang, fun_to_list, 1), Xs, fun (_) -> t_string() end);
type(erlang, garbage_collect, 0, _) -> t_atom('true');
type(erlang, garbage_collect, 1, Xs) ->
  strict(arg_types(erlang, garbage_collect, 1), Xs, fun (_) -> t_boolean() end);
type(erlang, get, 0, _) -> t_list(t_tuple(2));
type(erlang, get, 1, _) -> t_any();          % | t_atom('undefined')
type(erlang, get_cookie, 0, _) -> t_atom();  % | t_atom('nocookie')
type(erlang, get_keys, 1, _) -> t_list();
type(erlang, get_module_info, 1, Xs) ->
  strict(arg_types(erlang, get_module_info, 1), Xs,
	 fun (_) ->
	     t_list(t_tuple([t_atom(), t_list(t_tuple([t_atom(), t_any()]))]))
	 end);
type(erlang, get_module_info, 2, Xs) ->
  T_module_info_2_returns =
    t_sup([t_atom(),
	   t_list(t_tuple([t_atom(), t_any()])),
	   t_list(t_tuple([t_atom(), t_arity(), t_integer()]))]),
  strict(arg_types(erlang, get_module_info, 2), Xs,
	 fun ([Module, Item]) ->
	     case t_is_atom(Item) of
	       true -> 
		 case t_atom_vals(Item) of
		   ['module'] -> t_inf(t_atom(), Module);
		   ['imports'] -> t_nil();
		   ['exports'] -> t_list(t_tuple([t_atom(), t_arity()]));
		   ['functions'] -> t_list(t_tuple([t_atom(), t_arity()]));
		   ['attributes'] -> t_list(t_tuple([t_atom(), t_any()]));
		   ['compile'] -> t_list(t_tuple([t_atom(), t_any()]));
		   ['native_addresses'] -> % [{FunName, Arity, Address}]
		     t_list(t_tuple([t_atom(), t_arity(), t_integer()]));
		   List when is_list(List) ->
		     T_module_info_2_returns;
		   unknown ->
		     T_module_info_2_returns
		 end;
	       false ->
		 T_module_info_2_returns
	     end
	 end);
type(erlang, get_stacktrace, 0, _) ->
  t_list(t_tuple([t_atom(), t_atom(), t_sup([t_arity(), t_list()])]));
type(erlang, group_leader, 0, _) -> t_pid();
type(erlang, group_leader, 2, Xs) ->
  strict(arg_types(erlang, group_leader, 2), Xs,
	 fun (_) -> t_atom('true') end);
type(erlang, hash, 2, Xs) ->
  strict(arg_types(erlang, hash, 2), Xs, fun (_) -> t_integer() end);
type(erlang, hd, 1, Xs) ->
  strict(arg_types(erlang, hd, 1), Xs, fun ([X]) -> t_cons_hd(X) end);
type(erlang, integer_to_list, 1, Xs) ->
  strict(arg_types(erlang, integer_to_list, 1), Xs,
	 fun (_) -> t_string() end);
type(erlang, integer_to_list, 2, Xs) ->
  strict(arg_types(erlang, integer_to_list, 2), Xs,
	 fun (_) -> t_string() end);
type(erlang, info, 1, Xs) -> type(erlang, system_info, 1, Xs); % alias
type(erlang, iolist_size, 1, Xs) ->
  strict(arg_types(erlang, iolist_size, 1), Xs,
	 fun (_) -> t_non_neg_integer() end);
type(erlang, iolist_to_binary, 1, Xs) ->
  strict(arg_types(erlang, iolist_to_binary, 1), Xs,
	 fun (_) -> t_binary() end);
type(erlang, is_alive, 0, _) -> t_boolean();
type(erlang, is_atom, 1, Xs) ->   
  Fun = fun (X) -> check_guard(X, fun (Y) -> t_is_atom(Y) end, t_atom()) end,
  strict(arg_types(erlang, is_atom, 1), Xs, Fun);
type(erlang, is_binary, 1, Xs) ->
  Fun = fun (X) ->
	    check_guard(X, fun (Y) -> t_is_binary(Y) end, t_binary())
	end,
  strict(arg_types(erlang, is_binary, 1), Xs, Fun);
type(erlang, is_bitstr, 1, Xs) ->	% XXX: TAKE OUT
  type(erlang, is_bitstring, 1, Xs);
type(erlang, is_bitstring, 1, Xs) ->
  Fun = fun (X) ->
	    check_guard(X, fun (Y) -> t_is_bitstr(Y) end, t_bitstr())
	end,
  strict(arg_types(erlang, is_bitstring, 1), Xs, Fun);
type(erlang, is_boolean, 1, Xs) ->
  Fun = fun (X) ->
	    check_guard(X, fun (Y) -> t_is_boolean(Y) end, t_boolean())
	end,
  strict(arg_types(erlang, is_boolean, 1), Xs, Fun);
type(erlang, is_builtin, 3, Xs) ->
  strict(arg_types(erlang, is_builtin, 3), Xs, fun (_) -> t_boolean() end);
type(erlang, is_constant, 1, Xs) ->
  Fun = fun (X) ->
	    check_guard(X, fun (Y) -> t_is_constant(Y) end, t_constant())
	end,
  strict(arg_types(erlang, is_constant, 1), Xs, Fun);
type(erlang, is_float, 1, Xs) ->
  Fun = fun (X) ->
	    check_guard(X, fun (Y) -> t_is_float(Y) end, t_float())
	end,
  strict(arg_types(erlang, is_float, 1), Xs, Fun);
type(erlang, is_function, 1, Xs) ->
  Fun = fun (X) -> check_guard(X, fun (Y) -> t_is_fun(Y) end, t_fun()) end,
  strict(arg_types(erlang, is_function, 1), Xs, Fun);
type(erlang, is_function, 2, Xs) ->
  Fun = fun ([FunType, ArityType]) -> 
	    case t_number_vals(ArityType) of
	      unknown -> t_boolean();
	      [Val] -> 
		FunConstr = t_fun(any_list(Val), t_any()),
		Fun2 = fun (X) ->
			   t_is_subtype(X, FunConstr) andalso (not t_is_none(X))
		       end,
		check_guard_single(FunType, Fun2, FunConstr);
	      IntList when is_list(IntList) -> t_boolean() %% true?
	    end
	end,
  strict(arg_types(erlang, is_function, 2), Xs, Fun);
type(erlang, is_integer, 1, Xs) ->
  Fun = fun (X) ->
	    check_guard(X, fun (Y) -> t_is_integer(Y) end, t_integer())
	end,
  strict(arg_types(erlang, is_integer, 1), Xs, Fun);
type(erlang, is_list, 1, Xs) ->
  Fun = fun (X) ->
	    Fun2 = fun (Y) -> t_is_maybe_improper_list(Y) end,
	    check_guard(X, Fun2, t_maybe_improper_list())
	end,
  strict(arg_types(erlang, is_list, 1), Xs, Fun);
type(erlang, is_number, 1, Xs) ->
  Fun = fun (X) ->
	    check_guard(X, fun (Y) -> t_is_number(Y) end, t_number())
	end,
  strict(arg_types(erlang, is_number, 1), Xs, Fun);
type(erlang, is_pid, 1, Xs) ->
  Fun = fun (X) -> check_guard(X, fun (Y) -> t_is_pid(Y) end, t_pid()) end,
  strict(arg_types(erlang, is_pid, 1), Xs, Fun);
type(erlang, is_port, 1, Xs) ->
  Fun = fun (X) -> check_guard(X, fun (Y) -> t_is_port(Y) end, t_port()) end,
  strict(arg_types(erlang, is_port, 1), Xs, Fun);
type(erlang, is_process_alive, 1, Xs) ->
  strict(arg_types(erlang, is_process_alive, 1), Xs,
	 fun (_) -> t_boolean() end);
type(erlang, is_record, 2, Xs) ->
  Fun = fun ([X, Y]) ->
	    case t_is_tuple(X) of
	      false ->
		case t_is_none(t_inf(t_tuple(), X)) of
		  true -> t_atom('false');
		  false -> t_boolean()
		end;
	      true ->
		case t_tuple_subtypes(X) of
		  unknown -> t_boolean();
		  [Tuple] ->
		    case t_tuple_args(Tuple) of
		      %% any -> t_boolean();
		      [Tag|_] ->
			case t_is_atom(Tag) of
			  false ->
			    TagAtom = t_inf(Tag, t_atom()),
			    case t_is_none(TagAtom) of
			      true -> t_atom('false');
			      false -> t_boolean()
			    end;
			  true ->
			    case t_atom_vals(Tag) of
			      [RealTag] -> 
				case t_atom_vals(Y) of
				  [RealTag] -> t_atom('true');
				  _ -> t_boolean() 
				end;
			      _ -> t_boolean()
			    end
			end
		    end;
		  List when length(List) >= 2 ->
		    t_sup([type(erlang, is_record, 2, [T, Y]) || T <- List])
		end
	    end
	end,
  strict(arg_types(erlang, is_record, 2), Xs, Fun);
type(erlang, is_record, 3, Xs) ->
  Fun = fun ([X, Y, Z]) ->
	    Arity = t_number_vals(Z),
	    case t_is_tuple(X) of
	      false when length(Arity) =:= 1 ->
		[RealArity] = Arity,
		case t_is_none(t_inf(t_tuple(RealArity), X)) of
		  true -> t_atom('false');
		  false -> t_boolean()
		end;
	      false ->
		case t_is_none(t_inf(t_tuple(), X)) of
		  true -> t_atom('false');
		  false -> t_boolean()
		end;
	      true when length(Arity) =:= 1 ->
		[RealArity] = Arity,
		case t_tuple_subtypes(X) of
		  unknown -> t_boolean();
		  [Tuple] ->
		    case t_tuple_args(Tuple) of
		      %% any -> t_boolean();
		      Args when length(Args) =:= RealArity ->
			Tag = hd(Args),
			case t_is_atom(Tag) of
			  false ->
			    TagAtom = t_inf(Tag, t_atom()),
			    case t_is_none(TagAtom) of
			      true -> t_atom('false');
			      false -> t_boolean()
			    end;
			  true ->
			    case t_atom_vals(Tag) of
			      [RealTag] -> 
				case t_atom_vals(Y) of
				  [RealTag] -> t_atom('true');
				  _ -> t_boolean()
				end;
			      _ -> t_boolean()
			    end
			end;
		      Args when length(Args) =/= RealArity ->
			t_atom('false')
		    end;
		  [_, _|_] ->
		    t_boolean()
		end;
	      true ->
		t_boolean()
	    end
	end,
  strict(arg_types(erlang, is_record, 3), Xs, Fun);
type(erlang, is_reference, 1, Xs) ->
  Fun = fun (X) ->
	    check_guard(X, fun (Y) -> t_is_reference(Y) end, t_reference())
	end,
  strict(arg_types(erlang, is_reference, 1), Xs, Fun);
type(erlang, is_tuple, 1, Xs) ->
  Fun = fun (X) ->
	    check_guard(X, fun (Y) -> t_is_tuple(Y) end, t_tuple())
	end,
  strict(arg_types(erlang, is_tuple, 1), Xs, Fun);
type(erlang, length, 1, Xs) ->
  strict(arg_types(erlang, length, 1), Xs, fun (_) -> t_non_neg_fixnum() end);
type(erlang, link, 1, Xs) ->
  strict(arg_types(erlang, link, 1), Xs, fun (_) -> t_atom('true') end);
type(erlang, list_to_atom, 1, Xs) ->
  strict(arg_types(erlang, list_to_atom, 1), Xs, fun (_) -> t_atom() end);
type(erlang, list_to_binary, 1, Xs) ->
  strict(arg_types(erlang, list_to_binary, 1), Xs,
	 fun (_) -> t_binary() end);
type(erlang, list_to_bitstr, 1, Xs) ->
  type(erlang, list_to_bitstring, 1, Xs);
type(erlang, list_to_bitstring, 1, Xs) ->
  strict(arg_types(erlang, list_to_bitstring, 1), Xs,
	 fun (_) -> t_bitstr() end);
type(erlang, list_to_existing_atom, 1, Xs) ->
  strict(arg_types(erlang, list_to_existing_atom, 1), Xs,
	 fun (_) -> t_atom() end);
type(erlang, list_to_float, 1, Xs) ->
  strict(arg_types(erlang, list_to_float, 1), Xs, fun (_) -> t_float() end);
type(erlang, list_to_integer, 1, Xs) ->
  strict(arg_types(erlang, list_to_integer, 1), Xs,
	 fun (_) -> t_integer() end);
type(erlang, list_to_integer, 2, Xs) ->
  strict(arg_types(erlang, list_to_integer, 2), Xs,
	 fun (_) -> t_integer() end);
type(erlang, list_to_pid, 1, Xs) ->
  strict(arg_types(erlang, list_to_pid, 1), Xs, fun (_) -> t_pid() end);
type(erlang, list_to_tuple, 1, Xs) ->
  strict(arg_types(erlang, list_to_tuple, 1), Xs, fun (_) -> t_tuple() end);
type(erlang, load_module, 2, Xs) ->
  strict(arg_types(erlang, load_module, 2), Xs,
	 fun ([Mod,_Bin]) -> t_code_load_return(Mod) end);
type(erlang, load_nif, 2, Xs) ->
  strict(arg_types(erlang, load_nif, 2), Xs,
	 fun (_) ->
	     Reason = t_atoms(['load_failed', 'bad_lib', 'load',
			       'reload', 'upgrade', 'old_code']),
	     RsnPair = t_tuple([Reason, t_string()]),
	     t_sup(t_atom('ok'), t_tuple([t_atom('error'), RsnPair]))
	 end);
type(erlang, loaded, 0, _) ->
  t_list(t_atom());
type(erlang, localtime, 0, Xs) ->
  type(erlang, universaltime, 0, Xs);    % same
type(erlang, localtime_to_universaltime, 1, Xs) ->
  type(erlang, universaltime_to_localtime, 1, Xs);    % same
type(erlang, localtime_to_universaltime, 2, Xs) ->
  strict(arg_types(erlang, localtime_to_universaltime, 2), Xs, % typecheck
	 fun ([X,_]) -> type(erlang, localtime_to_universaltime, 1, [X]) end);
type(erlang, make_fun, 3, Xs) ->
  strict(arg_types(erlang, make_fun, 3), Xs, 
	 fun ([_, _, Arity]) -> 
	     case t_number_vals(Arity) of
	       [N] ->
		 case is_integer(N) andalso 0 =< N andalso N =< 255 of
		   true -> t_fun(N, t_any());
		   false -> t_none()
		 end;
	       _Other -> t_fun()
	     end
	 end);
type(erlang, make_ref, 0, _) -> t_reference();
type(erlang, make_tuple, 2, Xs) ->
  strict(arg_types(erlang, make_tuple, 2), Xs,
	 fun ([Int, _]) ->
	     case t_number_vals(Int) of
	       [N] when is_integer(N), N >= 0 -> t_tuple(N);
	       _Other -> t_tuple()
	     end
	 end);
type(erlang, make_tuple, 3, Xs) ->
  strict(arg_types(erlang, make_tuple, 3), Xs,
	 fun ([Int, _, _]) ->
	     case t_number_vals(Int) of
	       [N] when is_integer(N), N >= 0 -> t_tuple(N);
	       _Other -> t_tuple()
	     end
	 end);
type(erlang, match_spec_test, 3, Xs) ->
  strict(arg_types(erlang, match_spec_test, 3), Xs,
	 fun (_) -> t_sup(t_tuple([t_atom('ok'),
				   t_any(), % it can be any term
				   t_list(t_atom('return_trace')),
				   t_match_spec_test_errors()]),
			  t_tuple([t_atom('error'),
				   t_match_spec_test_errors()])) end);
type(erlang, md5, 1, Xs) ->
  strict(arg_types(erlang, md5, 1), Xs, fun (_) -> t_binary() end);
type(erlang, md5_final, 1, Xs) ->
  strict(arg_types(erlang, md5_final, 1), Xs, fun (_) -> t_binary() end);
type(erlang, md5_init, 0, _) -> t_binary();
type(erlang, md5_update, 2, Xs) ->
  strict(arg_types(erlang, md5_update, 2), Xs, fun (_) -> t_binary() end);
type(erlang, memory, 0, _) -> t_list(t_tuple([t_atom(), t_non_neg_fixnum()]));
type(erlang, memory, 1, Xs) ->
  strict(arg_types(erlang, memory, 1), Xs,
	 fun ([Type]) ->
	     case t_is_atom(Type) of
	       true -> t_non_neg_fixnum();
	       false ->
	         case t_is_list(Type) of
		   true -> t_list(t_tuple([t_atom(), t_non_neg_fixnum()]));
		   false ->
		     t_sup(t_non_neg_fixnum(),
		   	   t_list(t_tuple([t_atom(), t_non_neg_fixnum()])))
		 end
	     end
	 end);
type(erlang, module_loaded, 1, Xs) ->
  strict(arg_types(erlang, module_loaded, 1), Xs, fun (_) -> t_boolean() end);
type(erlang, monitor, 2, Xs) ->
  strict(arg_types(erlang, monitor, 2), Xs, fun (_) -> t_reference() end);
type(erlang, monitor_node, 2, Xs) ->
  strict(arg_types(erlang, monitor_node, 2), Xs,
	 fun (_) -> t_atom('true') end);
type(erlang, monitor_node, 3, Xs) ->
  strict(arg_types(erlang, monitor_node, 3), Xs,
	 fun (_) -> t_atom('true') end);
type(erlang, node, 0, _) -> t_node();
type(erlang, node, 1, Xs) ->
  strict(arg_types(erlang, node, 1), Xs, fun (_) -> t_node() end);
type(erlang, nodes, 0, _) -> t_list(t_node());
type(erlang, nodes, 1, Xs) ->
  strict(arg_types(erlang, nodes, 1), Xs, fun (_) -> t_list(t_node()) end);
type(erlang, now, 0, _) ->
  t_time();
type(erlang, open_port, 2, Xs) ->
  strict(arg_types(erlang, open_port, 2), Xs, fun (_) -> t_port() end);
type(erlang, phash, 2, Xs) ->
  strict(arg_types(erlang, phash, 2), Xs, fun (_) -> t_pos_integer() end);
type(erlang, phash2, 1, Xs) ->
  strict(arg_types(erlang, phash2, 1), Xs, fun (_) -> t_non_neg_integer() end);
type(erlang, phash2, 2, Xs) ->
  strict(arg_types(erlang, phash2, 2), Xs, fun (_) -> t_non_neg_integer() end);
type(erlang, pid_to_list, 1, Xs) ->
  strict(arg_types(erlang, pid_to_list, 1), Xs, fun (_) -> t_string() end);
type(erlang, port_call, 3, Xs) ->
  strict(arg_types(erlang, port_call, 3), Xs, fun (_) -> t_any() end);
type(erlang, port_close, 1, Xs) ->
  strict(arg_types(erlang, port_close, 1), Xs,
	 fun (_) -> t_atom('true') end);
type(erlang, port_command, 2, Xs) ->
  strict(arg_types(erlang, port_command, 2), Xs,
	 fun (_) -> t_atom('true') end);
type(erlang, port_command, 3, Xs) ->
  strict(arg_types(erlang, port_command, 3), Xs,
	 fun (_) -> t_boolean() end);
type(erlang, port_connect, 2, Xs) ->
  strict(arg_types(erlang, port_connect, 2), Xs,
	 fun (_) -> t_atom('true') end);
type(erlang, port_control, 3, Xs) ->
  strict(arg_types(erlang, port_control, 3), Xs,
	 fun (_) -> t_sup(t_string(), t_binary()) end);
type(erlang, port_get_data, 1, Xs) ->
  strict(arg_types(erlang, port_get_data, 1), Xs, fun (_) -> t_any() end);
type(erlang, port_info, 1, Xs) ->
  strict(arg_types(erlang, port_info, 1), Xs,
	 fun (_) -> t_sup(t_atom('undefined'), t_list()) end);
type(erlang, port_info, 2, Xs) ->
  strict(arg_types(erlang, port_info, 2), Xs,
	 fun ([_Port, Item]) ->
	     t_sup(t_atom('undefined'),
		   case t_atom_vals(Item) of
		     ['connected'] -> t_tuple([Item, t_pid()]);
		     ['id'] -> t_tuple([Item, t_integer()]);
		     ['input'] -> t_tuple([Item, t_integer()]);
		     ['links'] -> t_tuple([Item, t_list(t_pid())]);
		     ['name'] -> t_tuple([Item, t_string()]);
		     ['output'] -> t_tuple([Item, t_integer()]);
		     ['registered_name'] -> t_tuple([Item, t_atom()]);
		     List when is_list(List) ->
		       t_tuple([t_sup([t_atom(A) || A <- List]),
				t_sup([t_atom(), t_integer(),
				       t_pid(), t_list(t_pid()),
				       t_string()])]);
		     unknown ->
		       [_, PosItem] = arg_types(erlang, port_info, 2),
		       t_tuple([PosItem,
				t_sup([t_atom(), t_integer(),
				       t_pid(), t_list(t_pid()),
				       t_string()])])		       
		   end)
	 end);
type(erlang, port_to_list, 1, Xs) ->
  strict(arg_types(erlang, port_to_list, 1), Xs, fun (_) -> t_string() end);
type(erlang, ports, 0, _) -> t_list(t_port());
type(erlang, port_set_data, 2, Xs) ->
  strict(arg_types(erlang, port_set_data, 2), Xs,
	 fun (_) -> t_atom('true') end);
type(erlang, pre_loaded, 0, _) -> t_list(t_atom());
type(erlang, process_display, 2, _) -> t_atom('true');
type(erlang, process_flag, 2, Xs) ->
  T_process_flag_returns = t_sup([t_boolean(), t_atom(), t_non_neg_integer()]),
  strict(arg_types(erlang, process_flag, 2), Xs,
	 fun ([Flag, _Option]) ->
	     case t_is_atom(Flag) of
	       true ->
		 case t_atom_vals(Flag) of
		   ['error_handler'] -> t_atom();
		   ['min_heap_size'] -> t_non_neg_integer();
		   ['monitor_nodes'] -> t_boolean();
		   ['priority'] -> t_process_priority_level();
		   ['save_calls'] -> t_non_neg_integer();
		   ['trap_exit'] -> t_boolean();
		   List when is_list(List) ->
		     T_process_flag_returns;
		   unknown ->
		     T_process_flag_returns
		 end;
	       false -> % XXX: over-approximation if Flag is tuple
		 T_process_flag_returns
	     end
	 end);
type(erlang, process_flag, 3, Xs) ->
  strict(arg_types(erlang, process_flag, 3), Xs,
	 fun (_) -> t_non_neg_integer() end);
type(erlang, process_info, 1, Xs) ->
  strict(arg_types(erlang, process_info, 1), Xs,
	 fun (_) ->
	     t_sup(t_list(t_tuple([t_pinfo(), t_any()])),
		   t_atom('undefined'))
	 end);
type(erlang, process_info, 2, Xs) ->
  %% we define all normal return values: the return when the process exists
  %% t_nil() is the return for 'registered_name'; perhaps for more
  T_process_info_2_normal_returns =
    t_sup([t_tuple([t_pinfo_item(), t_any()]), t_nil()]),
  strict(arg_types(erlang, process_info, 2), Xs,
	 fun ([_Pid, InfoItem]) ->
	     Ret = case t_is_atom(InfoItem) of
		     true ->
		       case t_atom_vals(InfoItem) of
			 ['backtrace'] -> t_tuple([InfoItem, t_binary()]);
			 ['current_function'] -> t_tuple([InfoItem, t_mfa()]);
			 ['dictionary'] -> t_tuple([InfoItem, t_list()]);
			 ['error_handler'] -> t_tuple([InfoItem, t_atom()]);
			 ['garbage_collection'] ->
			   t_tuple([InfoItem, t_list()]);
			 ['group_leader'] -> t_tuple([InfoItem, t_pid()]);
			 ['heap_size'] ->
			   t_tuple([InfoItem, t_non_neg_integer()]);
			 ['initial_call'] -> t_tuple([InfoItem, t_mfa()]);
			 ['last_calls'] ->
			   t_tuple([InfoItem,
				    t_sup(t_atom('false'), t_list())]);
			 ['links'] -> t_tuple([InfoItem, t_list(t_pid())]);
			 ['memory'] ->
			   t_tuple([InfoItem, t_non_neg_integer()]);
			 ['message_binary'] -> t_tuple([InfoItem,  t_list()]);
			 ['message_queue_len'] ->
			   t_tuple([InfoItem, t_non_neg_integer()]);
			 ['messages'] -> t_tuple([InfoItem, t_list()]);
			 ['monitored_by'] ->
			   t_tuple([InfoItem,  t_list(t_pid())]);
			 ['monitors'] ->
			   t_tuple([InfoItem, 
				    t_list(t_sup(t_tuple([t_atom('process'),
							  t_pid()]),
						 t_tuple([t_atom('process'),
							  t_tuple([t_atom(),
								   t_atom()])])))]);
			 ['priority'] ->
			   t_tuple([InfoItem, t_process_priority_level()]);
			 ['reductions'] ->
			   t_tuple([InfoItem, t_non_neg_integer()]);
			 ['registered_name'] ->
			   t_sup(t_tuple([InfoItem, t_atom()]), t_nil());
			 ['sequential_trace_token'] ->
			   t_tuple([InfoItem, t_any()]); %% Underspecified
			 ['stack_size'] ->
			   t_tuple([InfoItem, t_non_neg_integer()]);
			 ['status'] ->
			   t_tuple([InfoItem, t_process_status()]);
			 ['suspending'] ->
			   t_tuple([InfoItem,
				    t_list(t_tuple([t_pid(),
						    t_non_neg_integer(),
						    t_non_neg_integer()]))]);
			 ['total_heap_size'] ->
			   t_tuple([InfoItem, t_non_neg_integer()]);
			 ['trap_exit'] ->
			   t_tuple([InfoItem, t_boolean()]);
			 List when is_list(List) ->
			   T_process_info_2_normal_returns;
			 unknown ->
			   T_process_info_2_normal_returns
		       end;
		     false ->
		       case t_is_list(InfoItem) of
			 true ->
			   t_list(t_tuple([t_pinfo_item(), t_any()]));
			 false ->
			   t_sup(T_process_info_2_normal_returns,
				 t_list(t_tuple([t_pinfo_item(), t_any()])))
		       end
		   end,
	       t_sup([Ret, t_atom('undefined')])
	 end);
type(erlang, processes, 0, _) -> t_list(t_pid());
type(erlang, purge_module, 1, Xs) ->
  strict(arg_types(erlang, purge_module, 1), Xs,
	 fun (_) -> t_atom('true') end);
type(erlang, put, 2, Xs) ->
  strict(arg_types(erlang, put, 2), Xs, fun (_) -> t_any() end);
type(erlang, raise, 3, _) -> t_none();
type(erlang, read_timer, 1, Xs) ->
  strict(arg_types(erlang, read_timer, 1), Xs,
	 fun (_) -> t_sup(t_non_neg_integer(), t_atom('false')) end);
type(erlang, ref_to_list, 1, Xs) ->
  strict(arg_types(erlang, ref_to_list, 1), Xs, fun (_) -> t_string() end);
type(erlang, register, 2, Xs) ->
  strict(arg_types(erlang, register, 2), Xs, fun (_) -> t_atom('true') end);
type(erlang, registered, 0, _) -> t_list(t_atom());
type(erlang, resume_process, 1, Xs) ->
  strict(arg_types(erlang, resume_process, 1), Xs,
	 fun (_) -> t_any() end); %% TODO: overapproximation -- fix this
type(erlang, round, 1, Xs) ->
  strict(arg_types(erlang, round, 1), Xs, fun (_) -> t_integer() end);
type(erlang, self, 0, _) -> t_pid();
type(erlang, send, 2, Xs) -> type(erlang, '!', 2, Xs);  % alias
type(erlang, send, 3, Xs) ->
  strict(arg_types(erlang, send, 3), Xs,
	 fun (_) -> t_sup(t_atom('ok'), t_sendoptions()) end);
type(erlang, send_after, 3, Xs) ->
  strict(arg_types(erlang, send_after, 3), Xs, fun (_) -> t_reference() end);
type(erlang, seq_trace, 2, Xs) ->
  strict(arg_types(erlang, seq_trace, 2), Xs,
	 fun (_) -> t_sup(t_seq_trace_info_returns(), t_tuple(5)) end);
type(erlang, seq_trace_info, 1, Xs) ->
  strict(arg_types(erlang, seq_trace_info, 1), Xs,
	 fun ([Item]) ->
	     case t_atom_vals(Item) of
	       ['label'] ->
		 t_sup(t_tuple([Item, t_non_neg_integer()]), t_nil());
	       ['serial'] -> 
		 t_sup(t_tuple([Item, t_tuple([t_non_neg_integer(),
					       t_non_neg_integer()])]),
		       t_nil());
	       ['send'] -> t_tuple([Item, t_boolean()]);
	       ['receive'] -> t_tuple([Item, t_boolean()]);
	       ['print'] -> t_tuple([Item, t_boolean()]);
	       ['timestamp'] -> t_tuple([Item, t_boolean()]);
	       List when is_list(List) ->
		 t_seq_trace_info_returns();
	       unknown ->
		 t_seq_trace_info_returns()
	     end
	 end);
type(erlang, seq_trace_print, 1, Xs) ->
  strict(arg_types(erlang, seq_trace_print, 1), Xs, fun (_) -> t_boolean() end);
type(erlang, seq_trace_print, 2, Xs) ->
  strict(arg_types(erlang, seq_trace_print, 2), Xs, fun (_) -> t_boolean() end);
type(erlang, set_cookie, 2, Xs) ->
  strict(arg_types(erlang, set_cookie, 2), Xs, fun (_) -> t_atom('true') end);
type(erlang, setelement, 3, Xs) ->
  strict(arg_types(erlang, setelement, 3), Xs,
	 fun ([X1, X2, X3]) ->
	     case t_tuple_subtypes(X2) of
	       unknown -> t_tuple();
	       [_] ->
		 Sz = t_tuple_size(X2),
		 As = t_tuple_args(X2),
		 case t_number_vals(X1) of
		   unknown ->
		     t_tuple([t_sup(X, X3) || X <- As]);
		   [N] when is_integer(N), 1 =< N, N =< Sz ->
		     t_tuple(list_replace(N, X3, As));
		   [N] when is_integer(N), N < 1 ->
		     t_none();
		   [N] when is_integer(N), N > Sz ->
		     t_none();
		   Ns ->
		     Fun = fun (N, XL) when is_integer(N), 1 =< N, N =< Sz ->
			       X = lists:nth(N, XL),
			       Y = t_sup(X, X3),
			       list_replace(N, Y, XL);
			       (_, XL) ->
			       XL
			   end,
		     t_tuple(lists:foldl(Fun, As, Ns))
		 end;
	       Ts when is_list(Ts) ->
		 t_sup([type(erlang, setelement, 3, [X1, Y, X3]) || Y <- Ts])
	     end
	 end);
type(erlang, setnode, 2, Xs) ->
  strict(arg_types(erlang, setnode, 2), Xs, fun (_) -> t_atom('true') end);
type(erlang, setnode, 3, Xs) ->
  strict(arg_types(erlang, setnode, 3), Xs, fun (_) -> t_atom('true') end);
type(erlang, size, 1, Xs) ->
  strict(arg_types(erlang, size, 1), Xs, fun (_) -> t_non_neg_integer() end);
type(erlang, spawn, 1, Xs) ->
  strict(arg_types(erlang, spawn, 1), Xs, fun (_) -> t_pid() end);
type(erlang, spawn, 2, Xs) ->
  strict(arg_types(erlang, spawn, 2), Xs, fun (_) -> t_pid() end);
type(erlang, spawn, 3, Xs) ->
  strict(arg_types(erlang, spawn, 3), Xs, fun (_) -> t_pid() end);
type(erlang, spawn, 4, Xs) ->
  strict(arg_types(erlang, spawn, 4), Xs, fun (_) -> t_pid() end);
type(erlang, spawn_link, 1, Xs) -> type(erlang, spawn, 1, Xs);  % same
type(erlang, spawn_link, 2, Xs) -> type(erlang, spawn, 2, Xs);  % same
type(erlang, spawn_link, 3, Xs) -> type(erlang, spawn, 3, Xs);  % same
type(erlang, spawn_link, 4, Xs) -> type(erlang, spawn, 4, Xs);  % same
type(erlang, spawn_opt, 1, Xs) ->
  strict(arg_types(erlang, spawn_opt, 1), Xs, 
	 fun ([Tuple]) ->
	     Fun = fun (TS) ->
		       [_, _, _, List] = t_tuple_args(TS),
		       t_spawn_opt_return(List)
		   end,
	     t_sup([Fun(TS) || TS <- t_tuple_subtypes(Tuple)])
	 end);
type(erlang, spawn_opt, 2, Xs) -> 
  strict(arg_types(erlang, spawn_opt, 2), Xs, 
	 fun ([_, List]) -> t_spawn_opt_return(List) end);
type(erlang, spawn_opt, 3, Xs) -> 
  strict(arg_types(erlang, spawn_opt, 3), Xs, 
	 fun ([_, _, List]) -> t_spawn_opt_return(List) end);
type(erlang, spawn_opt, 4, Xs) -> 
  strict(arg_types(erlang, spawn_opt, 4), Xs, 
	 fun ([_, _, _, List]) -> t_spawn_opt_return(List) end);
type(erlang, split_binary, 2, Xs) ->
  strict(arg_types(erlang, split_binary, 2), Xs,
	 fun (_) -> t_tuple([t_binary(), t_binary()]) end);
type(erlang, start_timer, 3, Xs) ->
  strict(arg_types(erlang, start_timer, 3), Xs, fun (_) -> t_reference() end);
type(erlang, statistics, 1, Xs) ->
  strict(arg_types(erlang, statistics, 1), Xs,
	 fun ([Type]) ->
	     T_statistics_1 = t_sup([t_non_neg_integer(),
				     t_tuple([t_non_neg_integer(),
					      t_non_neg_integer()]),
				     %% When called with the argument 'io'.
				     t_tuple([t_tuple([t_atom('input'),
						       t_non_neg_integer()]),
					      t_tuple([t_atom('output'),
						       t_non_neg_integer()])]),
				     t_tuple([t_non_neg_integer(),
					      t_non_neg_integer(),
					      t_non_neg_integer()])]),
	     case t_atom_vals(Type) of
	       ['context_switches'] ->
		 t_tuple([t_non_neg_integer(), t_integer(0)]);
	       ['exact_reductions'] ->
		 t_tuple([t_non_neg_integer(), t_non_neg_integer()]);
	       ['garbage_collection'] ->
		 t_tuple([t_non_neg_integer(),
			  t_non_neg_integer(),
			  t_integer(0)]);
	       ['io'] ->
		 t_tuple([t_tuple([t_atom('input'),  t_non_neg_integer()]),
			  t_tuple([t_atom('output'), t_non_neg_integer()])]);
	       ['reductions'] ->
		 t_tuple([t_non_neg_integer(), t_non_neg_integer()]);
	       ['run_queue'] ->
		 t_non_neg_integer();
	       ['runtime'] ->
		 t_tuple([t_non_neg_integer(), t_integer(0)]);
	       ['wall_clock'] ->
		 t_tuple([t_non_neg_integer(), t_integer(0)]);
	       List when is_list(List) ->
		 T_statistics_1;
	       unknown ->
		 T_statistics_1
	     end
	 end);
type(erlang, subtract, 2, Xs) -> type(erlang, '--', 2, Xs); % alias
type(erlang, suspend_process, 1, Xs) ->
  strict(arg_types(erlang, suspend_process, 1), Xs,
	 fun (_) -> t_atom('true') end);
type(erlang, suspend_process, 2, Xs) ->
  strict(arg_types(erlang, suspend_process, 2), Xs,
	 fun (_) -> t_boolean() end);
type(erlang, system_flag, 2, Xs) ->
  strict(arg_types(erlang, system_flag, 2), Xs,
	 fun ([Flag,_Value]) ->
	     %% this provides an overapproximation of all return values 
	     T_system_flag_2 = t_sup([t_boolean(),
				      t_integer(),
				      t_sequential_tracer(),
				      t_system_cpu_topology(),
				      t_system_multi_scheduling()]),
	     case t_is_atom(Flag) of
	       true ->
	         case t_atom_vals(Flag) of
		   ['backtrace_depth'] ->
		     t_non_neg_fixnum();
		   ['cpu_topology'] ->
		     t_system_cpu_topology();
		   ['debug_flags'] ->
		     t_atom('true');
		   ['display_items'] ->
		     t_non_neg_fixnum();
		   ['fullsweep_after'] ->
		     t_non_neg_fixnum();
		   ['min_heap_size'] ->
		     t_non_neg_fixnum();
		   ['multi_scheduling'] ->
		     t_system_multi_scheduling();
		   ['schedulers_online'] ->
		     t_pos_fixnum();
		   ['scheduler_bind_type'] ->
		     t_scheduler_bind_type_results();
		   ['sequential_tracer'] ->
		     t_sequential_tracer();
		   ['trace_control_word'] ->
		     t_integer();
		   List when is_list(List) ->
		     T_system_flag_2;
		   unknown ->
		     T_system_flag_2
		 end;
	       false ->
	       	 case t_is_integer(Flag) of	% SHOULD BE: t_is_fixnum
		   true ->
		     t_atom('true');
		   false ->
		     T_system_flag_2
		 end
	     end
	 end);
type(erlang, system_info, 1, Xs) ->
  strict(arg_types(erlang, system_info, 1), Xs,
	 fun ([Type]) ->
	     case t_is_atom(Type) of
	       true ->
		 case t_atom_vals(Type) of
		   ['allocated_areas'] ->
		     t_list(t_sup([t_tuple([t_atom(),t_non_neg_integer()]),
				   t_tuple([t_atom(),
					    t_non_neg_integer(),
					    t_non_neg_integer()])]));
		   ['allocator'] ->
		     t_tuple([t_sup([t_atom('undefined'),
				     t_atom('elib_malloc'),
				     t_atom('glibc')]),
			      t_list(t_integer()),
			      t_list(t_atom()),
			      t_list(t_tuple([t_atom(),
					      t_list(t_tuple([t_atom(),
							      t_any()]))]))]);
		   ['break_ignored'] ->
		     t_boolean();
		   ['cpu_topology'] ->
		     t_system_cpu_topology();
		   ['compat_rel'] ->
		     t_non_neg_fixnum();
		   ['creation'] ->
		     t_fixnum();
		   ['debug_compiled'] ->
		     t_boolean();
		   ['dist'] ->
		     t_binary();
		   ['dist_ctrl'] ->
		     t_list(t_tuple([t_atom(), t_sup([t_pid(), t_port])]));
		   ['elib_malloc'] ->
		     t_sup([t_atom('false'),
			    t_list(t_tuple([t_atom(), t_any()]))]);
		   ['endian'] ->
		     t_endian();
		   ['fullsweep_after'] ->
		     t_tuple([t_atom('fullsweep_after'), t_non_neg_integer()]);
		   ['garbage_collection'] ->
		     t_list();
		   ['global_heaps_size'] ->
		     t_non_neg_integer();
		   ['heap_sizes'] ->
		     t_list(t_integer());
		   ['heap_type'] ->
		     t_sup([t_atom('private'), t_atom('hybrid')]);
		   ['hipe_architecture'] ->
		     t_atoms(['amd64', 'arm', 'powerpc', 'ppc64',
			      'undefined', 'ultrasparc', 'x86']);
		   ['info'] ->
		     t_binary();
		   ['internal_cpu_topology'] -> %% Undocumented internal feature
		     t_internal_cpu_topology();
		   ['loaded'] ->
		     t_binary();
		   ['logical_processors'] ->
		     t_non_neg_fixnum();
		   ['machine'] ->
		     t_string();
		   ['multi_scheduling'] ->
		     t_system_multi_scheduling();
		   ['multi_scheduling_blockers'] ->
		     t_list(t_pid());
		   ['os_type'] ->
		     t_tuple([t_sup([t_atom('ose'),	% XXX: undocumented
				     t_atom('unix'),
				     t_atom('vxworks'),
				     t_atom('win32')]),
			      t_atom()]);
		   ['os_version'] ->
		     t_sup(t_tuple([t_non_neg_fixnum(),
				    t_non_neg_fixnum(),
				    t_non_neg_fixnum()]),
			   t_string());
		   ['process_count'] ->
		     t_non_neg_fixnum();
		   ['process_limit'] ->
		     t_non_neg_fixnum();
		   ['procs'] ->
		     t_binary();
		   ['scheduler_bindings'] ->
		     t_tuple();
		   ['scheduler_bind_type'] ->
		     t_scheduler_bind_type_results();
		   ['schedulers'] ->
		     t_pos_fixnum();
		   ['schedulers_online'] ->
		     t_pos_fixnum();
		   ['sequential_tracer'] ->
		     t_tuple([t_atom('sequential_tracer'),
			      t_sequential_tracer()]);
		   ['smp_support'] ->
		     t_boolean();
		   ['system_architecture'] ->
		     t_string();
		   ['system_version'] ->
		     t_string();
		   ['threads'] ->
		     t_boolean();
		   ['thread_pool_size'] ->
		     t_non_neg_fixnum();
		   ['trace_control_word'] ->
		     t_integer();
		   ['version'] ->
		     t_string();
		   ['wordsize'] ->
		     t_integers([4,8]);
		   List when is_list(List) ->
		     t_any(); %% gross overapproximation
		   unknown ->
		     t_any()
		 end;
	       false ->  %% This currently handles only {allocator, Alloc}
		 t_any() %% overapproximation as the return value might change
	     end
	 end);
type(erlang, system_monitor, 0, Xs) ->
  strict(arg_types(erlang, system_monitor, 0), Xs,
	 fun (_) -> t_system_monitor_settings() end);
type(erlang, system_monitor, 1, Xs) ->
  strict(arg_types(erlang, system_monitor, 1), Xs,
	 fun (_) -> t_system_monitor_settings() end);
type(erlang, system_monitor, 2, Xs) ->
  strict(arg_types(erlang, system_monitor, 2), Xs,
	 fun (_) -> t_system_monitor_settings() end);
type(erlang, system_profile, 0, _) ->
  t_system_profile_return();
type(erlang, system_profile, 2, Xs) ->
  strict(arg_types(erlang, system_profile, 2), Xs,
	 fun (_) -> t_system_profile_return() end); 
type(erlang, term_to_binary, 1, Xs) ->
  strict(arg_types(erlang, term_to_binary, 1), Xs, fun (_) -> t_binary() end);
type(erlang, term_to_binary, 2, Xs) ->
  strict(arg_types(erlang, term_to_binary, 2), Xs, fun (_) -> t_binary() end);
type(erlang, time, 0, _) ->
  t_tuple([t_non_neg_integer(), t_non_neg_integer(), t_non_neg_integer()]);
type(erlang, tl, 1, Xs) ->
  strict(arg_types(erlang, tl, 1), Xs, fun ([X]) -> t_cons_tl(X) end);
type(erlang, trace, 3, Xs) ->
  strict(arg_types(erlang, trace, 3), Xs, fun (_) -> t_integer() end);
type(erlang, trace_delivered, 1, Xs) ->
  strict(arg_types(erlang, trace_delivered, 1), Xs,
	 fun (_) -> t_reference() end);
type(erlang, trace_info, 2, Xs) ->
  strict(arg_types(erlang, trace_info, 2), Xs,
	 fun (_) ->
	     t_tuple([t_atom(),
		      t_sup([%% the following is info about a PID
			     t_list(t_atom()), t_pid(), t_port(),
			     %% the following is info about a func
			     t_atom('global'), t_atom('local'),
			     t_atom('false'), t_atom('true'),
			     t_list(), t_pid(), t_port(),
			     t_integer(),
			     t_list(t_tuple([t_atom(), t_any()])),
			     %% and this is the 'not found' value
			     t_atom('undefined')])])
	 end);
type(erlang, trace_pattern, 2, Xs) ->
  strict(arg_types(erlang, trace_pattern, 2), Xs,
	 fun (_) -> t_non_neg_fixnum() end); %% num of MFAs that match pattern
type(erlang, trace_pattern, 3, Xs) ->
  strict(arg_types(erlang, trace_pattern, 3), Xs,
	 fun (_) -> t_non_neg_fixnum() end); %% num of MFAs that match pattern
type(erlang, trunc, 1, Xs) ->
  strict(arg_types(erlang, trunc, 1), Xs, fun (_) -> t_integer() end);
type(erlang, tuple_size, 1, Xs) ->
  strict(arg_types(erlang, tuple_size, 1), Xs, fun (_) -> t_non_neg_integer() end);
type(erlang, tuple_to_list, 1, Xs) ->
  strict(arg_types(erlang, tuple_to_list, 1), Xs,
	 fun ([X]) ->
	     case t_tuple_subtypes(X) of
	       unknown -> t_list();
	       SubTypes -> 
		 Args = lists:flatten([t_tuple_args(ST) || ST <- SubTypes]),
		 %% Can be nil if the tuple can be {}
		 case lists:any(fun (T) ->
				    t_tuple_size(T) =:= 0
				end, SubTypes) of
		   true ->
		     %% Be careful here. If we had only {} we need to
		     %% keep the nil.
		     t_sup(t_nonempty_list(t_sup(Args)), t_nil());
		   false ->
		     t_nonempty_list(t_sup(Args))
		 end
	     end
	 end);
type(erlang, universaltime, 0, _) ->
  t_tuple([t_date(), t_time()]);
type(erlang, universaltime_to_localtime, 1, Xs) ->
  strict(arg_types(erlang, universaltime_to_localtime, 1), Xs,
	 fun ([T]) -> T end);
type(erlang, unlink, 1, Xs) ->
  strict(arg_types(erlang, unlink, 1), Xs, fun (_) -> t_atom('true') end);
type(erlang, unregister, 1, Xs) ->
  strict(arg_types(erlang, unregister, 1), Xs, fun (_) -> t_atom('true') end);
type(erlang, whereis, 1, Xs) ->
  strict(arg_types(erlang, whereis, 1), Xs,
	 fun (_) -> t_sup([t_pid(), t_port(), t_atom('undefined')]) end);
type(erlang, yield, 0, _) -> t_atom('true');
%%-- erl_prim_loader ----------------------------------------------------------
type(erl_prim_loader, get_file, 1, Xs) ->
  strict(arg_types(erl_prim_loader, get_file, 1), Xs,
	 fun (_) ->
	     t_sup(t_tuple([t_atom('ok'), t_binary(), t_string()]),
		   t_atom('error'))
	 end);
type(erl_prim_loader, get_path, 0, _) ->
  t_tuple([t_atom('ok'), t_list(t_string())]);
type(erl_prim_loader, set_path, 1, Xs) ->
  strict(arg_types(erl_prim_loader, set_path, 1), Xs,
	 fun (_) -> t_atom('ok') end);
%%-- error_logger -------------------------------------------------------------
type(error_logger, warning_map, 0, _) ->
  t_sup([t_atom('info'), t_atom('warning'), t_atom('error')]);
%%-- erts_debug ---------------------------------------------------------------
type(erts_debug, breakpoint, 2, Xs) ->
  strict(arg_types(erts_debug, breakpoint, 2), Xs, fun (_) -> t_fixnum() end);
type(erts_debug, disassemble, 1, Xs) ->
  strict(arg_types(erts_debug, disassemble, 1), Xs,
	 fun (_) -> t_sup([t_atom('false'),
			   t_atom('undef'),
			   t_tuple([t_integer(), t_binary(), t_mfa()])]) end);
type(erts_debug, dist_ext_to_term, 2, Xs) ->
  strict(arg_types(erts_debug, dist_ext_to_term, 2), Xs,
	 fun (_) -> t_any() end);
type(erts_debug, flat_size, 1, Xs) ->
  strict(arg_types(erts_debug, flat_size, 1), Xs, fun (_) -> t_integer() end);
type(erts_debug, lock_counters, 1, Xs) ->
  strict(arg_types(erts_debug, lock_counters, 1), Xs,
	 fun ([Arg]) ->
	     case t_is_atom(Arg) of
	       true ->
		 case t_atom_vals(Arg) of
		   ['enabled'] -> t_boolean();
		   ['info'] -> t_any();
		   ['clear'] -> t_atom(ok);
		   _ -> t_sup([t_boolean(), t_any(), t_atom('ok')])
		 end;
	       false ->
		 case t_is_tuple(Arg) of
		   true -> t_boolean();
		   false -> t_sup([t_boolean(), t_any(), t_atom('ok')])
		 end
	     end
	 end);
type(erts_debug, same, 2, Xs) ->
  strict(arg_types(erts_debug, same, 2), Xs, fun (_) -> t_boolean() end);
%%-- ets ----------------------------------------------------------------------
type(ets, all, 0, _) ->
  t_list(t_tab());
type(ets, delete, 1, Xs) ->
  strict(arg_types(ets, delete, 1), Xs, fun (_) -> t_atom('true') end);
type(ets, delete, 2, Xs) ->
  strict(arg_types(ets, delete, 2), Xs, fun (_) -> t_atom('true') end);
type(ets, delete_all_objects, 1, Xs) ->
  strict(arg_types(ets, delete_all_objects, 1), Xs,
	 fun (_) -> t_atom('true') end);
type(ets, delete_object, 2, Xs) ->
  strict(arg_types(ets, delete_object, 2), Xs, fun (_) -> t_atom('true') end);
type(ets, first, 1, Xs) ->
  strict(arg_types(ets, first, 1), Xs, fun (_) -> t_any() end);
type(ets, give_away, 3, Xs) ->
  strict(arg_types(ets, give_away, 3), Xs, fun (_) -> t_atom('true') end);
type(ets, info, 1, Xs) ->
  strict(arg_types(ets, info, 1), Xs,
	 fun (_) ->
	     t_sup(t_list(t_tuple([t_ets_info_items(), t_any()])),
		   t_atom('undefined'))
	 end);
type(ets, info, 2, Xs) ->
  strict(arg_types(ets, info, 2), Xs, fun (_) -> t_any() end);
type(ets, insert, 2, Xs) ->
  strict(arg_types(ets, insert, 2), Xs, fun (_) -> t_atom('true') end);
type(ets, insert_new, 2, Xs) ->
  strict(arg_types(ets, insert_new, 2), Xs, fun (_) -> t_boolean() end);
type(ets, is_compiled_ms, 1, Xs) ->
  strict(arg_types(ets, is_compiled_ms, 1), Xs, fun (_) -> t_boolean() end);
type(ets, last, 1, Xs) ->
  type(ets, first, 1, Xs);
type(ets, lookup, 2, Xs) ->
  strict(arg_types(ets, lookup, 2), Xs, fun (_) -> t_list(t_tuple()) end);
type(ets, lookup_element, 3, Xs) ->
  strict(arg_types(ets, lookup_element, 3), Xs, fun (_) -> t_any() end);
type(ets, match, 1, Xs) ->
  strict(arg_types(ets, match, 1), Xs, fun (_) -> t_matchres() end);
type(ets, match, 2, Xs) ->
  strict(arg_types(ets, match, 2), Xs, fun (_) -> t_list() end);
type(ets, match, 3, Xs) ->
  strict(arg_types(ets, match, 3), Xs, fun (_) -> t_matchres() end);
type(ets, match_object, 1, Xs) -> type(ets, match, 1, Xs);
type(ets, match_object, 2, Xs) -> type(ets, match, 2, Xs);
type(ets, match_object, 3, Xs) -> type(ets, match, 3, Xs);
type(ets, match_spec_compile, 1, Xs) ->
  strict(arg_types(ets, match_spec_compile, 1), Xs, fun (_) -> t_any() end);
type(ets, match_spec_run_r, 3, Xs) ->
  strict(arg_types(ets, match_spec_run_r, 3), Xs, fun (_) -> t_list() end);
type(ets, member, 2, Xs) ->
  strict(arg_types(ets, member, 2), Xs, fun (_) -> t_boolean() end);
type(ets, new, 2, Xs) ->
  strict(arg_types(ets, new, 2), Xs, fun (_) -> t_tab() end);
type(ets, next, 2, Xs) ->
  strict(arg_types(ets, next, 2), Xs,
	 %% t_any below stands for:  term() | '$end_of_table'
	 fun (_) -> t_any() end);
type(ets, prev, 2, Xs) -> type(ets, next, 2, Xs);
type(ets, rename, 2, Xs) ->
  strict(arg_types(ets, rename, 2), Xs, fun ([_, Name]) -> Name end);
type(ets, safe_fixtable, 2, Xs) ->
  strict(arg_types(ets, safe_fixtable, 2), Xs, fun (_) -> t_atom('true') end);
type(ets, select, 1, Xs) ->
  strict(arg_types(ets, select, 1), Xs, fun (_) -> t_matchres() end);
type(ets, select, 2, Xs) ->
  strict(arg_types(ets, select, 2), Xs, fun (_) -> t_list() end);
type(ets, select, 3, Xs) ->
  strict(arg_types(ets, select, 3), Xs, fun (_) -> t_matchres() end);
type(ets, select_count, 2, Xs) ->
  strict(arg_types(ets, select_count, 2), Xs,
	 fun (_) -> t_non_neg_fixnum() end);
type(ets, select_delete, 2, Xs) ->
  strict(arg_types(ets, select_delete, 2), Xs,
	 fun (_) -> t_non_neg_fixnum() end);
type(ets, select_reverse, 1, Xs) -> type(ets, select, 1, Xs);
type(ets, select_reverse, 2, Xs) -> type(ets, select, 2, Xs);
type(ets, select_reverse, 3, Xs) -> type(ets, select, 3, Xs);
type(ets, setopts, 2, Xs) ->
  strict(arg_types(ets, setopts, 2), Xs, fun (_) -> t_atom('true') end);
type(ets, slot, 2, Xs) ->
  strict(arg_types(ets, slot, 2), Xs,
	 fun (_) -> t_sup(t_list(t_tuple()), t_atom('$end_of_table')) end);
type(ets, update_counter, 3, Xs) ->
  strict(arg_types(ets, update_counter, 3), Xs, fun (_) -> t_integer() end);
type(ets, update_element, 3, Xs) ->
  strict(arg_types(ets, update_element, 3), Xs, fun (_) -> t_boolean() end);
%%-- file ---------------------------------------------------------------------
type(file, close, 1, Xs) ->
  strict(arg_types(file, close, 1), Xs, fun (_) -> t_file_return() end);
type(file, delete, 1, Xs) ->
  strict(arg_types(file, delete, 1), Xs, fun (_) -> t_file_return() end);
type(file, get_cwd, 0, _) ->
  t_sup(t_tuple([t_atom('ok'), t_string()]),
	t_tuple([t_atom('error'), t_file_posix_error()]));
type(file, make_dir, 1, Xs) ->
  strict(arg_types(file, make_dir, 1), Xs, fun (_) -> t_file_return() end);
type(file, open, 2, Xs) ->
  strict(arg_types(file, open, 2), Xs,
	 fun (_) ->
	     t_sup([t_tuple([t_atom('ok'), t_file_io_device()]),
		    t_tuple([t_atom('error'), t_file_posix_error()])])
	 end);
type(file, read_file, 1, Xs) ->
  strict(arg_types(file, read_file, 1), Xs,
	 fun (_) ->
	     t_sup([t_tuple([t_atom('ok'), t_binary()]),
		    t_tuple([t_atom('error'), t_file_posix_error()])])
	 end);
type(file, set_cwd, 1, Xs) ->
  strict(arg_types(file, set_cwd, 1), Xs, 
	 fun (_) -> t_sup(t_atom('ok'),
			  t_tuple([t_atom('error'), t_file_posix_error()]))
	 end);
type(file, write_file, 2, Xs) ->
  strict(arg_types(file, write_file, 2), Xs, fun (_) -> t_file_return() end);
%%-- gen_tcp ------------------------------------------------------------------
%% NOTE: All type information for this module added to avoid loss of precision
type(gen_tcp, accept, 1, Xs) ->
  strict(arg_types(gen_tcp, accept, 1), Xs, fun (_) -> t_gen_tcp_accept() end);
type(gen_tcp, accept, 2, Xs) ->
  strict(arg_types(gen_tcp, accept, 2), Xs, fun (_) -> t_gen_tcp_accept() end);
type(gen_tcp, connect, 3, Xs) ->
  strict(arg_types(gen_tcp, connect, 3), Xs,
	 fun (_) ->
	     t_sup(t_tuple([t_atom('ok'), t_socket()]),
		   t_tuple([t_atom('error'), t_inet_posix_error()]))
	 end);
type(gen_tcp, connect, 4, Xs) ->
  strict(arg_types(gen_tcp, connect, 4), Xs,
	 fun (_) ->
	     t_sup(t_tuple([t_atom('ok'), t_socket()]),
		   t_tuple([t_atom('error'), t_inet_posix_error()]))
	 end);
type(gen_tcp, listen, 2, Xs) ->
  strict(arg_types(gen_tcp, listen, 2), Xs,
	 fun (_) ->
	     t_sup(t_tuple([t_atom('ok'), t_socket()]),
		   t_tuple([t_atom('error'), t_inet_posix_error()]))
	 end);
type(gen_tcp, recv, 2, Xs) ->
  strict(arg_types(gen_tcp, recv, 2), Xs, fun (_) -> t_gen_tcp_recv() end);
type(gen_tcp, recv, 3, Xs) ->
  strict(arg_types(gen_tcp, recv, 3), Xs, fun (_) -> t_gen_tcp_recv() end);
type(gen_tcp, send, 2, Xs) ->
  strict(arg_types(gen_tcp, send, 2), Xs,
	 fun (_) ->
	     t_sup(t_atom('ok'),
		   t_tuple([t_atom('error'), t_inet_posix_error()]))
	 end);
type(gen_tcp, shutdown, 2, Xs) ->
  strict(arg_types(gen_tcp, shutdown, 2), Xs,
	 fun (_) ->
	     t_sup(t_atom('ok'),
		   t_tuple([t_atom('error'), t_inet_posix_error()]))
	 end);
%%-- gen_udp ------------------------------------------------------------------
%% NOTE: All type information for this module added to avoid loss of precision
type(gen_udp, open, 1, Xs) ->
  strict(arg_types(gen_udp, open, 1), Xs,
	 fun (_) ->
	     t_sup(t_tuple([t_atom('ok'), t_socket()]),
		   t_tuple([t_atom('error'), t_inet_posix_error()]))
	 end);
type(gen_udp, open, 2, Xs) ->
  strict(arg_types(gen_udp, open, 2), Xs,
	 fun (_) ->
	     t_sup(t_tuple([t_atom('ok'), t_socket()]),
		   t_tuple([t_atom('error'), t_inet_posix_error()]))
	 end);
type(gen_udp, recv, 2, Xs) ->
  strict(arg_types(gen_udp, recv, 2), Xs, fun (_) -> t_gen_udp_recv() end);
type(gen_udp, recv, 3, Xs) ->
  strict(arg_types(gen_udp, recv, 3), Xs, fun (_) -> t_gen_udp_recv() end);
type(gen_udp, send, 4, Xs) ->
  strict(arg_types(gen_udp, send, 4), Xs,
	 fun (_) ->
	     t_sup(t_atom('ok'),
		   t_tuple([t_atom('error'), t_sup(t_atom('not_owner'),
						   t_inet_posix_error())]))
	 end);
%%-- hipe_bifs ----------------------------------------------------------------
type(hipe_bifs, add_ref, 2, Xs) ->
  strict(arg_types(hipe_bifs, add_ref, 2), Xs, fun (_) -> t_nil() end);
type(hipe_bifs, alloc_data, 2, Xs) ->
  strict(arg_types(hipe_bifs, alloc_data, 2), Xs,
	 fun (_) -> t_integer() end); % address
type(hipe_bifs, array, 2, Xs) ->
  strict(arg_types(hipe_bifs, array, 2), Xs, fun (_) -> t_immarray() end);
type(hipe_bifs, array_length, 1, Xs) ->
  strict(arg_types(hipe_bifs, array_length, 1), Xs,
	 fun (_) -> t_non_neg_fixnum() end);
type(hipe_bifs, array_sub, 2, Xs) ->
  strict(arg_types(hipe_bifs, array_sub, 2), Xs, fun (_) -> t_immediate() end);
type(hipe_bifs, array_update, 3, Xs) ->
  strict(arg_types(hipe_bifs, array_update, 3), Xs,
	 fun (_) -> t_immarray() end);
type(hipe_bifs, atom_to_word, 1, Xs) ->
  strict(arg_types(hipe_bifs, atom_to_word, 1), Xs,
	 fun (_) -> t_integer() end);
type(hipe_bifs, bif_address, 3, Xs) ->
  strict(arg_types(hipe_bifs, bif_address, 3), Xs,
	 fun (_) -> t_sup(t_integer(), t_atom('false')) end);
type(hipe_bifs, bitarray, 2, Xs) ->
  strict(arg_types(hipe_bifs, bitarray, 2), Xs, fun (_) -> t_bitarray() end);
type(hipe_bifs, bitarray_sub, 2, Xs) ->
  strict(arg_types(hipe_bifs, bitarray_sub, 2), Xs, fun (_) -> t_boolean() end);
type(hipe_bifs, bitarray_update, 3, Xs) ->
  strict(arg_types(hipe_bifs, bitarray_update, 3), Xs,
	 fun (_) -> t_bitarray() end);
type(hipe_bifs, bytearray, 2, Xs) ->
  strict(arg_types(hipe_bifs, bytearray, 2), Xs, fun (_) -> t_bytearray() end);
type(hipe_bifs, bytearray_sub, 2, Xs) ->
  strict(arg_types(hipe_bifs, bytearray_sub, 2), Xs, fun (_) -> t_byte() end);
type(hipe_bifs, bytearray_update, 3, Xs) ->
  strict(arg_types(hipe_bifs, bytearray_update, 3), Xs,
	 fun (_) -> t_bytearray() end);
type(hipe_bifs, call_count_clear, 1, Xs) ->
  strict(arg_types(hipe_bifs, call_count_clear, 1), Xs,
	 fun (_) -> t_sup(t_non_neg_integer(), t_atom('false')) end);
type(hipe_bifs, call_count_get, 1, Xs) ->
  strict(arg_types(hipe_bifs, call_count_get, 1), Xs,
	 fun (_) -> t_sup(t_non_neg_integer(), t_atom('false')) end);
type(hipe_bifs, call_count_off, 1, Xs) ->
  strict(arg_types(hipe_bifs, call_count_off, 1), Xs,
	 fun (_) -> t_sup(t_non_neg_integer(), t_atom('false')) end);
type(hipe_bifs, call_count_on, 1, Xs) ->
  strict(arg_types(hipe_bifs, call_count_on, 1), Xs,
	 fun (_) -> t_sup(t_atom('true'), t_nil()) end);
type(hipe_bifs, check_crc, 1, Xs) ->
  strict(arg_types(hipe_bifs, check_crc, 1), Xs, fun (_) -> t_boolean() end);
type(hipe_bifs, enter_code, 2, Xs) ->
  strict(arg_types(hipe_bifs, enter_code, 2), Xs,
	 fun (_) -> t_tuple([t_integer(),
			     %% XXX: The tuple below contains integers and
			     %% is of size same as the length of the MFA list
			     t_sup(t_nil(), t_binary())]) end);
type(hipe_bifs, enter_sdesc, 1, Xs) ->
  strict(arg_types(hipe_bifs, enter_sdesc, 1), Xs, fun (_) -> t_nil() end);
type(hipe_bifs, find_na_or_make_stub, 2, Xs) ->
  strict(arg_types(hipe_bifs, find_na_or_make_stub, 2), Xs,
	 fun (_) -> t_integer() end); % address
type(hipe_bifs, fun_to_address, 1, Xs) ->
  strict(arg_types(hipe_bifs, fun_to_address, 1), Xs,
	 fun (_) -> t_integer() end);
%% type(hipe_bifs, get_emu_address, 1, Xs) ->
%%    strict(arg_types(hipe_bifs, get_emu_address, 1), Xs,
%%	   fun (_) -> t_integer() end); % address
type(hipe_bifs, get_rts_param, 1, Xs) ->
  strict(arg_types(hipe_bifs, get_rts_param, 1), Xs,
	 fun (_) -> t_sup(t_integer(), t_nil()) end);
type(hipe_bifs, invalidate_funinfo_native_addresses, 1, Xs) ->
  strict(arg_types(hipe_bifs, invalidate_funinfo_native_addresses, 1), Xs,
	 fun (_) -> t_nil() end);
type(hipe_bifs, make_fe, 3, Xs) ->
  strict(arg_types(hipe_bifs, make_fe, 3), Xs, fun (_) -> t_integer() end);
%% type(hipe_bifs, make_native_stub, 2, Xs) ->
%%    strict(arg_types(hipe_bifs, make_native_stub, 2), Xs,
%%	   fun (_) -> t_integer() end); % address
type(hipe_bifs, mark_referred_from, 1, Xs) ->
  strict(arg_types(hipe_bifs, mark_referred_from, 1), Xs,
	 fun (_) -> t_nil() end);
type(hipe_bifs, merge_term, 1, Xs) ->
  strict(arg_types(hipe_bifs, merge_term, 1), Xs, fun ([X]) -> X end);
type(hipe_bifs, patch_call, 3, Xs) ->
  strict(arg_types(hipe_bifs, patch_call, 3), Xs, fun (_) -> t_nil() end);
type(hipe_bifs, patch_insn, 3, Xs) ->
  strict(arg_types(hipe_bifs, patch_insn, 3), Xs, fun (_) -> t_nil() end);
type(hipe_bifs, primop_address, 1, Xs) ->
  strict(arg_types(hipe_bifs, primop_address, 1), Xs,
	 fun (_) -> t_sup(t_integer(), t_atom('false')) end);
type(hipe_bifs, redirect_referred_from, 1, Xs) ->
  strict(arg_types(hipe_bifs, redirect_referred_from, 1), Xs,
	 fun (_) -> t_nil() end);
type(hipe_bifs, ref, 1, Xs) ->
  strict(arg_types(hipe_bifs, ref, 1), Xs, fun (_) -> t_immarray() end);
type(hipe_bifs, ref_get, 1, Xs) ->
  strict(arg_types(hipe_bifs, ref_get, 1), Xs, fun (_) -> t_immediate() end);
type(hipe_bifs, ref_set, 2, Xs) ->
  strict(arg_types(hipe_bifs, ref_set, 2), Xs, fun (_) -> t_nil() end);
type(hipe_bifs, remove_refs_from, 1, Xs) ->
  strict(arg_types(hipe_bifs, remove_refs_from, 1), Xs,
	 fun (_) -> t_nil() end);
type(hipe_bifs, set_funinfo_native_address, 3, Xs) ->
  strict(arg_types(hipe_bifs, set_funinfo_native_address, 3), Xs,
	 fun (_) -> t_nil() end);
type(hipe_bifs, set_native_address, 3, Xs) ->
  strict(arg_types(hipe_bifs, set_native_address, 3), Xs,
	 fun (_) -> t_nil() end);
type(hipe_bifs, system_crc, 1, Xs) ->
  strict(arg_types(hipe_bifs, system_crc, 1), Xs, fun (_) -> t_crc32() end);
type(hipe_bifs, term_to_word, 1, Xs) ->
  strict(arg_types(hipe_bifs, term_to_word, 1), Xs,
	 fun (_) -> t_integer() end);
type(hipe_bifs, update_code_size, 3, Xs) ->
  strict(arg_types(hipe_bifs, update_code_size, 3), Xs,
	 fun (_) -> t_nil() end);
type(hipe_bifs, write_u8, 2, Xs) ->
  strict(arg_types(hipe_bifs, write_u8, 2), Xs, fun (_) -> t_nil() end);
type(hipe_bifs, write_u32, 2, Xs) ->
  strict(arg_types(hipe_bifs, write_u32, 2), Xs, fun (_) -> t_nil() end);
type(hipe_bifs, write_u64, 2, Xs) ->
  strict(arg_types(hipe_bifs, write_u64, 2), Xs, fun (_) -> t_nil() end);
%%-- io -----------------------------------------------------------------------
type(io, format, 1, Xs) ->
  strict(arg_types(io, format, 1), Xs, fun (_) -> t_atom('ok') end);
type(io, format, 2, Xs) ->
  strict(arg_types(io, format, 2), Xs, fun (_) -> t_atom('ok') end);
type(io, format, 3, Xs) ->
  strict(arg_types(io, format, 3), Xs, fun (_) -> t_atom('ok') end);
type(io, fwrite, 1, Xs) -> type(io, format, 1, Xs); % same
type(io, fwrite, 2, Xs) -> type(io, format, 2, Xs); % same
type(io, fwrite, 3, Xs) -> type(io, format, 3, Xs); % same
type(io, put_chars, 1, Xs) ->
  strict(arg_types(io, put_chars, 1), Xs, fun (_) -> t_atom('ok') end);
type(io, put_chars, 2, Xs) ->
  strict(arg_types(io, put_chars, 2), Xs, fun (_) -> t_atom('ok') end);
%%-- io_lib -------------------------------------------------------------------
type(io_lib, format, 2, Xs) ->
  strict(arg_types(io_lib, format, 2), Xs,
	 %% t_list() because the character list might be arbitrarily nested
	 fun (_) -> t_list(t_sup(t_char(), t_list())) end);
type(io_lib, fwrite, 2, Xs) -> type(io_lib, format, 2, Xs); % same
%%-- lists --------------------------------------------------------------------
type(lists, all, 2, Xs) ->
  strict(arg_types(lists, all, 2), Xs, 
	 fun ([F, L]) -> 
	     case t_is_nil(L) of
	       true -> t_atom('true');
	       false ->
		 El = t_list_elements(L),
		 case check_fun_application(F, [El]) of
		   ok -> 
		     case t_is_cons(L) of
		       true -> t_fun_range(F);
		       false -> 
			 %% The list can be empty.
			 t_sup(t_atom('true'), t_fun_range(F))
		     end;
		   error ->
		     case t_is_cons(L) of
		       true -> t_none();
		       false -> t_fun_range(F)
		     end
		 end
	     end
	 end);
type(lists, any, 2, Xs) ->
  strict(arg_types(lists, any, 2), Xs, 
	 fun ([F, L]) -> 
	     case t_is_nil(L) of
	       true -> t_atom('false');
	       false ->
		 El = t_list_elements(L),
		 case check_fun_application(F, [El]) of
		   ok -> 
		     case t_is_cons(L) of
		       true -> t_fun_range(F);
		       false -> 
			 %% The list can be empty
			 t_sup(t_atom('false'), t_fun_range(F))
		     end;
		   error ->
		     case t_is_cons(L) of
		       true -> t_none();
		       false -> t_fun_range(F)
		     end
		 end
	     end
	 end);
type(lists, append, 2, Xs) -> type(erlang, '++', 2, Xs);  % alias
type(lists, delete, 2, Xs) ->
  strict(arg_types(lists, delete, 2), Xs, 
	 fun ([_, List]) -> 
	     case t_is_cons(List) of
	       true -> t_cons_tl(List);
	       false -> List
	     end
	 end);
type(lists, dropwhile, 2, Xs) -> 
  strict(arg_types(lists, dropwhile, 2), Xs,
	 fun ([F, X]) -> 
	     case t_is_nil(X) of
	       true -> t_nil();
	       false ->
		 X1 = t_list_elements(X),
		 case check_fun_application(F, [X1]) of
		   ok ->
		     case t_atom_vals(t_fun_range(F)) of
		       ['true'] ->
			 case t_is_none(t_inf(t_list(), X)) of
			   true -> t_none();
			   false -> t_nil()
			 end;
		       ['false'] -> 
			 case t_is_none(t_inf(t_list(), X)) of
			   true -> t_none();
			   false -> X
			 end;
		       _ -> 
			 t_inf(t_cons_tl(t_inf(X, t_cons())), 
			       t_maybe_improper_list())
		     end;
		   error ->
		     case t_is_cons(X) of
		       true -> t_none();
		       false -> t_nil()
		     end
		 end
	     end
	 end);
type(lists, filter, 2, Xs) ->
  strict(arg_types(lists, filter, 2), Xs,
	 fun ([F, L]) -> 
	     case t_is_nil(L) of
	       true -> t_nil();
	       false ->
		 T = t_list_elements(L),
		 case check_fun_application(F, [T]) of
		   ok ->
		     case t_atom_vals(t_fun_range(F)) =:= ['false'] of
		       true -> t_nil();
		       false -> 
			 case t_atom_vals(t_fun_range(F)) =:= ['true'] of
			   true -> L;
			   false -> t_list(T)
			 end
		     end;
		   error ->
		     case t_is_cons(L) of
		       true -> t_none();
		       false -> t_nil()
		     end
		 end
	     end
	 end);
type(lists, flatten, 1, Xs) ->
  strict(arg_types(lists, flatten, 1), Xs,
	 fun ([L]) ->
	     case t_is_nil(L) of
	       true -> L;    % (nil has undefined elements)
	       false ->
		 %% Avoiding infinite recursion is tricky
		 X1 = t_list_elements(L),
		 case t_is_any(X1) of
		   true -> 
		     t_list();
		   false ->
		     X2 = type(lists, flatten, 1, [t_inf(X1, t_list())]),
		     t_sup(t_list(t_subtract(X1, t_list())),
			   X2)
		 end
	     end
	 end);
type(lists, flatmap, 2, Xs) ->
  strict(arg_types(lists, flatmap, 2), Xs,
	 fun ([F, List]) -> 
	     case t_is_nil(List) of
	       true -> t_nil();
	       false ->
		 case check_fun_application(F, [t_list_elements(List)]) of
		   ok -> 
		     case t_is_cons(List) of
		       true -> t_nonempty_list(t_list_elements(t_fun_range(F)));
		       false -> t_list(t_list_elements(t_fun_range(F)))
		     end;
		   error ->
		     case t_is_cons(List) of
		       true -> t_none();
		       false -> t_nil()
		     end
		 end
 	     end
	 end);
type(lists, foreach, 2, Xs) ->
  strict(arg_types(lists, foreach, 2), Xs,
	 fun ([F, List]) ->
	     case t_is_cons(List) of
	       true ->
		 case check_fun_application(F, [t_list_elements(List)]) of
		   ok -> t_atom('ok');
		   error -> t_none()
		 end;
	       false ->
		 t_atom('ok')
	     end
	 end);
type(lists, foldl, 3, Xs) ->
  strict(arg_types(lists, foldl, 3), Xs,
	 fun ([F, Acc, List]) ->
	     case t_is_nil(List) of
	       true -> Acc;
	       false ->
		 case check_fun_application(F, [t_list_elements(List), Acc]) of
		   ok ->
		     case t_is_cons(List) of
		       true -> t_fun_range(F);
		       false -> t_sup(t_fun_range(F), Acc)
		     end;
		   error ->
		     case t_is_cons(List) of
		       true -> t_none();
		       false -> Acc
		     end
		 end
	     end
	 end);
type(lists, foldr, 3, Xs) -> type(lists, foldl, 3, Xs);    % same
type(lists, keydelete, 3, Xs) ->
  strict(arg_types(lists, keydelete, 3), Xs, 
	 fun ([_, _, L]) ->
	     Term = t_list_termination(L),
	     t_sup(Term, erl_types:lift_list_to_pos_empty(L))
	 end);
type(lists, keyfind, 3, Xs) ->
  strict(arg_types(lists, keyfind, 3), Xs,
	 fun ([X, Y, Z]) ->
	     ListEs = t_list_elements(Z),
	     Tuple = t_inf(t_tuple(), ListEs),
	     case t_is_none(Tuple) of
	       true -> t_atom('false');
	       false ->
		 %% this BIF, contrary to lists:keysearch/3 does not
		 %% wrap its result in a 'value'-tagged tuple
		 Ret = t_sup(Tuple, t_atom('false')),
		 case t_is_any(X) of
		   true -> Ret;
		   false ->
		     case t_tuple_subtypes(Tuple) of
		       unknown -> Ret;
		       List ->
			 Keys = [type(erlang, element, 2, [Y, S])
				 || S <- List],
			 Infs = [t_inf(Key, X) || Key <- Keys],
			 case all_is_none(Infs) of
			   true -> t_atom('false');
			   false -> Ret
			 end
		     end
		 end
	     end
	 end);
type(lists, keymap, 3, Xs) ->
  strict(arg_types(lists, keymap, 3), Xs,
	 fun ([F, _I, L]) ->
	     case t_is_nil(L) of
	       true -> L;
	       false -> t_list(t_sup(t_fun_range(F), t_list_elements(L)))
	     end
	 end);
type(lists, keymember, 3, Xs) ->
  strict(arg_types(lists, keymember, 3), Xs,
	 fun ([X, Y, Z]) ->
	     ListEs = t_list_elements(Z),
	     Tuple = t_inf(t_tuple(), ListEs),
	     case t_is_none(Tuple) of
	       true -> t_atom('false');
	       false ->
		 case t_is_any(X) of
		   true -> t_boolean();
		   false ->
		     case t_tuple_subtypes(Tuple) of
		       unknown -> t_boolean();
		       List ->
			 Keys = [type(erlang, element, 2, [Y,S]) || S <- List],
			 Infs = [t_inf(Key, X) || Key <- Keys],
			 case all_is_none(Infs) of
			   true -> t_atom('false');
			   false -> t_boolean()
			 end
		     end
		 end
	     end
	 end);
type(lists, keymerge, 3, Xs) ->
  strict(arg_types(lists, keymerge, 3), Xs,
	 fun ([_I, L1, L2]) -> type(lists, merge, 2, [L1, L2]) end);
type(lists, keyreplace, 4, Xs) ->
  strict(arg_types(lists, keyreplace, 4), Xs,
	 fun ([_K, _I, L, T]) -> t_list(t_sup(t_list_elements(L), T)) end);
type(lists, keysearch, 3, Xs) ->
  strict(arg_types(lists, keysearch, 3), Xs,
	 fun ([X, Y, Z]) ->
	     ListEs = t_list_elements(Z),
	     Tuple = t_inf(t_tuple(), ListEs),
	     case t_is_none(Tuple) of
	       true -> t_atom('false');
	       false ->
		 Ret = t_sup(t_tuple([t_atom('value'), Tuple]), 
			     t_atom('false')),
		 case t_is_any(X) of
		   true -> Ret;
		   false ->
		     case t_tuple_subtypes(Tuple) of
		       unknown -> Ret;
		       List ->
			 Keys = [type(erlang, element, 2, [Y, S])
				 || S <- List],
			 Infs = [t_inf(Key, X) || Key <- Keys],
			 case all_is_none(Infs) of
			   true -> t_atom('false');
			   false -> Ret
			 end
		     end
		 end
	     end
	 end);
type(lists, keysort, 2, Xs) ->
  strict(arg_types(lists, keysort, 2), Xs, fun ([_, L]) -> L end);
type(lists, last, 1, Xs) ->
  strict(arg_types(lists, last, 1), Xs, fun ([L]) -> t_list_elements(L) end);
type(lists, map, 2, Xs) ->
  strict(arg_types(lists, map, 2), Xs,
	 fun ([F, L]) -> 
	     case t_is_nil(L) of
	       true -> L;
	       false ->
		 El = t_list_elements(L),
		 case t_is_cons(L) of
		   true ->
		     case check_fun_application(F, [El]) of
		       ok -> t_nonempty_list(t_fun_range(F));
		       error -> t_none()
		     end;
		   false ->
		     case check_fun_application(F, [El]) of
		       ok -> t_list(t_fun_range(F));
		       error -> t_nil()
		     end
		 end
	     end
	 end);
type(lists, mapfoldl, 3, Xs) ->
  strict(arg_types(lists, mapfoldl, 3), Xs,
	 fun ([F, Acc, List]) ->
	     case t_is_nil(List) of
	       true -> t_tuple([List, Acc]);
	       false ->
		 El = t_list_elements(List),
		 R = t_fun_range(F),
		 case t_is_cons(List) of
		   true ->
		     case check_fun_application(F, [El, Acc]) of
		       ok ->
			 Fun = fun (RangeTuple) ->
				   [T1, T2] = t_tuple_args(RangeTuple),
				   t_tuple([t_nonempty_list(T1), T2])
			       end,
			 t_sup([Fun(ST) || ST <- t_tuple_subtypes(R)]);
		       error -> 
			 t_none()
		     end;
		   false ->
		     case check_fun_application(F, [El, Acc]) of
		       ok ->
			 Fun = fun (RangeTuple) ->
				   [T1, T2] = t_tuple_args(RangeTuple),
				   t_tuple([t_list(T1), t_sup(Acc, T2)])
			       end,
			 t_sup([Fun(ST) || ST <- t_tuple_subtypes(R)]);
		       error ->
			 t_tuple([t_nil(), Acc])
		     end
		 end
	     end
	 end);
type(lists, mapfoldr, 3, Xs) -> type(lists, mapfoldl, 3, Xs);    % same
type(lists, max, 1, Xs) ->
  strict(arg_types(lists, max, 1), Xs, fun ([L]) -> t_list_elements(L) end);
type(lists, member, 2, Xs) ->
  strict(arg_types(lists, member, 2), Xs,
	 fun ([X, Y]) ->
	     Y1 = t_list_elements(Y),
	     case t_is_none(t_inf(Y1, X)) of
	       true -> t_atom('false');
	       false -> t_boolean()
	     end
	 end);
%% type(lists, merge, 1, Xs) ->
type(lists, merge, 2, Xs) ->
  strict(arg_types(lists, merge, 2), Xs,
	 fun ([L1, L2]) ->
	     case t_is_none(L1) of
	       true -> L2;
	       false ->
		 case t_is_none(L2) of
		   true -> L1;
		   false -> t_sup(L1, L2)
		 end
	     end
	 end);
%% type(lists, merge, 3, Xs) ->
%% type(lists, merge3, 3, Xs) ->
type(lists, min, 1, Xs) ->
  strict(arg_types(lists, min, 1), Xs, fun ([L]) -> t_list_elements(L) end);
type(lists, nth, 2, Xs) ->
  strict(arg_types(lists, nth, 2), Xs,
	 fun ([_, Y]) -> t_list_elements(Y) end);
type(lists, nthtail, 2, Xs) ->
  strict(arg_types(lists, nthtail, 2), Xs,
	 fun ([_, Y]) -> t_sup(Y, t_list()) end);
type(lists, partition, 2, Xs) ->
  strict(arg_types(lists, partition, 2), Xs,
	 fun ([F, L]) ->
	     case t_is_nil(L) of
	       true -> t_tuple([L,L]);
	       false ->
		 El = t_list_elements(L),
		 case check_fun_application(F, [El]) of
		   error -> 
		     case t_is_cons(L) of
		       true -> t_none();
		       false -> t_tuple([t_nil(), t_nil()])
		     end;
		   ok ->
		     case t_atom_vals(t_fun_range(F)) of
		       ['true'] -> t_tuple([L, t_nil()]);
		       ['false'] -> t_tuple([t_nil(), L]);
		       [_, _] ->
			 L2 = t_list(El),
			 t_tuple([L2, L2])
		     end
		 end
	     end
	 end);
type(lists, reverse, 1, Xs) ->
  strict(arg_types(lists, reverse, 1), Xs, fun ([X]) -> X end);
type(lists, reverse, 2, Xs) ->
  type(erlang, '++', 2, Xs);    % reverse-onto is just like append
type(lists, seq, 2, Xs) ->
  strict(arg_types(lists, seq, 2), Xs, fun (_) -> t_list(t_integer()) end);
type(lists, seq, 3, Xs) ->
  strict(arg_types(lists, seq, 3), Xs, fun (_) -> t_list(t_integer()) end);
type(lists, sort, 1, Xs) ->
  strict(arg_types(lists, sort, 1), Xs, fun ([X]) -> X end);
type(lists, sort, 2, Xs) ->
  strict(arg_types(lists, sort, 2), Xs,
	 fun ([F, L]) ->
	     R = t_fun_range(F),
	     case t_is_boolean(R) of
	       true -> L;
	       false ->
		 case t_is_nil(L) of
		   true -> t_nil();
		   false -> t_none()
		 end
	     end
	 end);
type(lists, split, 2, Xs) ->
  strict(arg_types(lists, split, 2), Xs,
	 fun ([_, L]) ->
	     case t_is_nil(L) of
	       true -> t_tuple([L, L]);
	       false ->
		 T = t_list_elements(L),
		 t_tuple([t_list(T), t_list(T)])
	     end
	 end);
type(lists, splitwith, 2, Xs) -> 
  T1 = type(lists, takewhile, 2, Xs),
  T2 = type(lists, dropwhile, 2, Xs),
  case t_is_none(T1) orelse t_is_none(T2) of
    true -> t_none();
    false -> t_tuple([T1, T2])
  end;
type(lists, subtract, 2, Xs) -> type(erlang, '--', 2, Xs);  % alias
type(lists, takewhile, 2, Xs) ->
  strict(arg_types(lists, takewhile, 2), Xs,
	 fun([F, L]) ->
	     case t_is_none(t_inf(t_list(), L)) of
	       false -> type(lists, filter, 2, Xs);
	       true ->
		 %% This works for non-proper lists as well.
		 El = t_list_elements(L),
		 type(lists, filter, 2, [F, t_list(El)])
	     end
	 end);
type(lists, usort, 1, Xs) -> type(lists, sort, 1, Xs); % same
type(lists, usort, 2, Xs) -> type(lists, sort, 2, Xs); % same
type(lists, unzip, 1, Xs) ->
  strict(arg_types(lists, unzip, 1), Xs, 
 	 fun ([Ps]) ->
	     case t_is_nil(Ps) of
	       true ->
		 t_tuple([t_nil(), t_nil()]);
	       false -> % Ps is a proper list of pairs
		 TupleTypes = t_tuple_subtypes(t_list_elements(Ps)),
		 lists:foldl(fun(Tuple, Acc) ->
				 [A, B] = t_tuple_args(Tuple),
				 t_sup(t_tuple([t_list(A), t_list(B)]), Acc)
			     end, t_none(), TupleTypes)
	     end
 	 end);
type(lists, unzip3, 1, Xs) ->
  strict(arg_types(lists, unzip3, 1), Xs, 
 	 fun ([Ts]) ->
	     case t_is_nil(Ts) of
	       true ->
		 t_tuple([t_nil(), t_nil(), t_nil()]);
	       false -> % Ps is a proper list of triples
		 TupleTypes = t_tuple_subtypes(t_list_elements(Ts)),
		 lists:foldl(fun(T, Acc) ->
				 [A, B, C] = t_tuple_args(T),
				 t_sup(t_tuple([t_list(A), 
						t_list(B), 
						t_list(C)]),
				       Acc)
			     end, t_none(), TupleTypes)
	     end
 	 end);
type(lists, zip, 2, Xs) ->
  strict(arg_types(lists, zip, 2), Xs,
	 fun ([As, Bs]) ->
	     case (t_is_nil(As) orelse t_is_nil(Bs)) of
	       true -> t_nil();
	       false ->
		 A = t_list_elements(As),
		 B = t_list_elements(Bs),
		 t_list(t_tuple([A, B]))
	     end
	 end);
type(lists, zip3, 3, Xs) ->
  strict(arg_types(lists, zip3, 3), Xs,
	 fun ([As, Bs, Cs]) ->
	     case (t_is_nil(As) orelse t_is_nil(Bs) orelse t_is_nil(Cs)) of
	       true -> t_nil();
	       false ->
		 A = t_list_elements(As),
		 B = t_list_elements(Bs),
		 C = t_list_elements(Cs),
		 t_list(t_tuple([A, B, C]))
	     end
	 end);
type(lists, zipwith, 3, Xs) ->
  strict(arg_types(lists, zipwith, 3), Xs,
	 fun ([F, _As, _Bs]) -> t_sup(t_list(t_fun_range(F)), t_nil()) end);
type(lists, zipwith3, 4, Xs) ->
  strict(arg_types(lists, zipwith3, 4), Xs,
	 fun ([F,_As,_Bs,_Cs]) -> t_sup(t_list(t_fun_range(F)), t_nil()) end);
%%-- math ---------------------------------------------------------------------
type(math, acos, 1, Xs) ->
  strict(arg_types(math, acos, 1), Xs, fun (_) -> t_float() end);
type(math, acosh, 1, Xs) ->
  strict(arg_types(math, acosh, 1), Xs, fun (_) -> t_float() end);
type(math, asin, 1, Xs) ->
  strict(arg_types(math, asin, 1), Xs, fun (_) -> t_float() end);
type(math, asinh, 1, Xs) ->
  strict(arg_types(math, asinh, 1), Xs, fun (_) -> t_float() end);
type(math, atan, 1, Xs) ->
  strict(arg_types(math, atan, 1), Xs, fun (_) -> t_float() end);
type(math, atan2, 2, Xs) ->
  strict(arg_types(math, atan2, 2), Xs, fun (_) -> t_float() end);
type(math, atanh, 1, Xs) ->
  strict(arg_types(math, atanh, 1), Xs, fun (_) -> t_float() end);
type(math, cos, 1, Xs) ->
  strict(arg_types(math, cos, 1), Xs, fun (_) -> t_float() end);
type(math, cosh, 1, Xs) ->
  strict(arg_types(math, cosh, 1), Xs, fun (_) -> t_float() end);
type(math, erf, 1, Xs) ->
  strict(arg_types(math, erf, 1), Xs, fun (_) -> t_float() end);
type(math, erfc, 1, Xs) ->
  strict(arg_types(math, erfc, 1), Xs, fun (_) -> t_float() end);
type(math, exp, 1, Xs) ->
  strict(arg_types(math, exp, 1), Xs, fun (_) -> t_float() end);
type(math, log, 1, Xs) ->
  strict(arg_types(math, log, 1), Xs, fun (_) -> t_float() end);
type(math, log10, 1, Xs) ->
  strict(arg_types(math, log10, 1), Xs, fun (_) -> t_float() end);
type(math, pi, 0, _) -> t_float();
type(math, pow, 2, Xs) ->
  strict(arg_types(math, pow, 2), Xs, fun (_) -> t_float() end);
type(math, sin, 1, Xs) ->
  strict(arg_types(math, sin, 1), Xs, fun (_) -> t_float() end);
type(math, sinh, 1, Xs) ->
  strict(arg_types(math, sinh, 1), Xs, fun (_) -> t_float() end);
type(math, sqrt, 1, Xs) ->
  strict(arg_types(math, sqrt, 1), Xs, fun (_) -> t_float() end);
type(math, tan, 1, Xs) ->
  strict(arg_types(math, tan, 1), Xs, fun (_) -> t_float() end);
type(math, tanh, 1, Xs) ->
  strict(arg_types(math, tanh, 1), Xs, fun (_) -> t_float() end);
%%-- net_kernel ---------------------------------------------------------------
type(net_kernel, dflag_unicode_io, 1, Xs) ->
  strict(arg_types(net_kernel, dflag_unicode_io, 1), Xs, 
	 fun (_) -> t_boolean() end); 
%%-- ordsets ------------------------------------------------------------------
type(ordsets, filter, 2, Xs) ->
  type(lists, filter, 2, Xs);
type(ordsets, fold, 3, Xs) ->
  type(lists, foldl, 3, Xs);
%%-- os -----------------------------------------------------------------------
type(os, getenv, 0, _) -> t_list(t_string());
type(os, getenv, 1, Xs) ->
  strict(arg_types(os, getenv, 1), Xs,
	 fun (_) -> t_sup(t_string(), t_atom('false')) end);
type(os, getpid, 0, _) -> t_string();
type(os, putenv, 2, Xs) ->
  strict(arg_types(os, putenv, 2), Xs, fun (_) -> t_atom('true') end);
type(os, timestamp, 0, _) ->
  t_time();
%%-- re -----------------------------------------------------------------------
type(re, compile, 1, Xs) ->
  strict(arg_types(re, compile, 1), Xs,
	 fun (_) ->
	     t_sup(t_tuple([t_atom('ok'), t_re_MP()]),
		   t_tuple([t_atom('error'), t_re_ErrorSpec()]))
	 end);
type(re, compile, 2, Xs) ->
  strict(arg_types(re, compile, 2), Xs,
	 fun (_) ->
	     t_sup(t_tuple([t_atom('ok'), t_re_MP()]),
		   t_tuple([t_atom('error'), t_re_ErrorSpec()]))
	 end);
type(re, run, 2, Xs) ->
  strict(arg_types(re, run, 2), Xs,
	 fun (_) ->
	     t_sup([t_tuple([t_atom('match'), t_re_Captured()]),
		    t_atom('nomatch'),
		    t_tuple([t_atom('error'), t_re_ErrorSpec()])])
	 end);
type(re, run, 3, Xs) ->
  strict(arg_types(re, run, 3), Xs,
	 fun (_) ->
	     t_sup([t_tuple([t_atom('match'), t_re_Captured()]),
		    t_atom('match'),
		    t_atom('nomatch'),
		    t_tuple([t_atom('error'), t_re_ErrorSpec()])])
	 end);
%%-- string -------------------------------------------------------------------
type(string, chars, 2, Xs) ->  % NOTE: added to avoid loss of information
  strict(arg_types(string, chars, 2), Xs, fun (_) -> t_string() end);
type(string, chars, 3, Xs) ->  % NOTE: added to avoid loss of information
  strict(arg_types(string, chars, 3), Xs,
	 fun ([Char, N, Tail]) ->
	     case t_is_nil(Tail) of
	       true ->
		 type(string, chars, 2, [Char, N]);
	       false ->
		 case t_is_string(Tail) of
		   true ->
		     t_string();
		   false ->
		     t_sup(t_sup(t_string(), Tail), t_cons(Char, Tail))
		 end
	     end
	 end);
type(string, concat, 2, Xs) -> % NOTE: added to avoid loss of information
  strict(arg_types(string, concat, 2), Xs, fun (_) -> t_string() end);
type(string, equal, 2, Xs) ->  % NOTE: added to avoid loss of information
  strict(arg_types(string, equal, 2), Xs, fun (_) -> t_boolean() end);
type(string, to_float, 1, Xs) ->
  strict(arg_types(string, to_float, 1), Xs,
	 fun (_) -> t_sup(t_tuple([t_float(), t_string()]),
			  t_tuple([t_atom('error'),
				   t_sup(t_atom('no_float'),
					 t_atom('not_a_list'))]))
	 end);
type(string, to_integer, 1, Xs) ->
  strict(arg_types(string, to_integer, 1), Xs,
	 fun (_) -> t_sup(t_tuple([t_integer(), t_string()]),
			  t_tuple([t_atom('error'),
				   t_sup(t_atom('no_integer'),
					 t_atom('not_a_list'))]))
	 end);
%%-- unicode ------------------------------------------------------------------
type(unicode, characters_to_binary, 2, Xs) ->
  strict(arg_types(unicode, characters_to_binary, 2), Xs,
	 fun (_) -> 
	     t_sup([t_binary(),
		    t_tuple([t_atom('error'), t_binary(), t_ML()]),
		    t_tuple([t_atom('incomplete'), t_binary(), t_ML()])])
		    end);
type(unicode, characters_to_list, 2, Xs) ->
  strict(arg_types(unicode, characters_to_list, 2), Xs,
	 fun (_) ->
	     t_sup([t_string(),
		    t_tuple([t_atom('error'), t_string(), t_ML()]),
		    t_tuple([t_atom('incomplete'), t_string(), t_ML()])])
	 end);
type(unicode, bin_is_7bit, 1, Xs) ->
  strict(arg_types(unicode, bin_is_7bit, 1), Xs, fun (_) -> t_boolean() end);

%%-----------------------------------------------------------------------------
type(M, F, A, Xs) when is_atom(M), is_atom(F),
		       is_integer(A), 0 =< A, A =< 255 ->
  strict(Xs, t_any()).  % safe approximation for all functions.


%%-----------------------------------------------------------------------------
%% Auxiliary functions
%%-----------------------------------------------------------------------------

strict(Xs, Ts, F) ->
  %% io:format("inf lists arg~n1:~p~n2:~p ~n", [Xs, Ts]),
  Xs1 = inf_lists(Xs, Ts),
  %% io:format("inf lists return ~p ~n", [Xs1]),
  case any_is_none_or_unit(Xs1) of
    true -> t_none();
    false -> F(Xs1)
  end.

strict(Xs, X) ->
  case any_is_none_or_unit(Xs) of
    true -> t_none();
    false -> X
  end.

inf_lists([X | Xs], [T | Ts]) ->
  [t_inf(X, T) | inf_lists(Xs, Ts)];
inf_lists([], []) ->
  [].

any_list(N) -> any_list(N, t_any()).

any_list(N, A) when N > 0 ->
  [A | any_list(N - 1, A)];
any_list(0, _) ->
  [].

list_replace(N, E, [X | Xs]) when N > 1 ->
  [X | list_replace(N - 1, E, Xs)];
list_replace(1, E, [_X | Xs]) ->
  [E | Xs].

any_is_none_or_unit(Ts) ->
  lists:any(fun erl_types:t_is_none_or_unit/1, Ts).

all_is_none(Ts) ->
  lists:all(fun erl_types:t_is_none/1, Ts).

check_guard([X], Test, Type) ->
  check_guard_single(X, Test, Type).

check_guard_single(X, Test, Type) ->
  case Test(X) of
    true -> t_atom('true');
    false ->
      case erl_types:t_is_opaque(X) of
	true -> t_none();
	false ->
	  case t_is_none(t_inf(Type, X)) of
	    true -> t_atom('false');
	    false -> t_boolean()
	  end
      end
  end.

%%-----------------------------------------------------------------------------
%% Functions for range analysis
%%-----------------------------------------------------------------------------

infinity_max([]) -> empty;
infinity_max([H|T]) ->
  if H =:= empty ->
      infinity_max(T);
     true ->
      lists:foldl(
	fun (Elem, Max) ->
	    Geq = infinity_geq(Elem, Max),
	    if not Geq orelse (Elem =:= empty) ->
		Max;
	       true ->
		Elem
	    end
	end,
	H,
	T)
  end. 

infinity_min([]) -> empty;
infinity_min([H|T]) ->
  if H =:= empty ->
      infinity_min(T);
     true ->
      lists:foldl(fun (Elem, Min) ->
		      Geq = infinity_geq(Elem, Min),
		      if Geq orelse (Elem =:= empty) ->
			  Min;
			 true ->
			  Elem
		      end
		  end,
		  H,
		  T)
  end.

-type inf_integer() :: 'neg_inf' | 'pos_inf' | integer().

-spec infinity_abs('pos_inf' | 'neg_inf') -> 'pos_inf'
		; (integer()) -> non_neg_integer().

infinity_abs(pos_inf) -> pos_inf;
infinity_abs(neg_inf) -> pos_inf;
infinity_abs(Number) when is_integer(Number) -> abs(Number).

%% span_zero(Range) ->
%%   infinity_geq(0, number_min(Range)) and infinity_geq(number_max(Range), 0).

infinity_inv(pos_inf) -> neg_inf;
infinity_inv(neg_inf) -> pos_inf;
infinity_inv(Number) when is_integer(Number) -> -Number.

infinity_band(neg_inf, Type2) -> Type2;
%% infinity_band(Type1, neg_inf) -> Type1;
infinity_band(pos_inf, Type2) -> Type2;
%% infinity_band(Type1, pos_inf) -> Type1;
infinity_band(Type1, Type2) when is_integer(Type1), is_integer(Type2) ->
  Type1 band Type2.

infinity_bor(neg_inf, _Type2) -> neg_inf;
%% infinity_bor(_Type1, neg_inf) -> neg_inf;
infinity_bor(pos_inf, _Type2) -> pos_inf;
%% infinity_bor(_Type1, pos_inf) -> pos_inf;
infinity_bor(Type1, Type2) when is_integer(Type1), is_integer(Type2) ->
  Type1 bor Type2.

infinity_div(pos_inf, pos_inf) -> [0, pos_inf];
infinity_div(pos_inf, neg_inf) -> [neg_inf, 0];
infinity_div(neg_inf, neg_inf) -> [0, pos_inf];
infinity_div(neg_inf, pos_inf) -> [neg_inf, 0];
infinity_div(pos_inf, Number) when is_integer(Number), Number > 0 -> pos_inf;
infinity_div(pos_inf, Number) when is_integer(Number), Number < 0 -> neg_inf;
infinity_div(neg_inf, Number) when is_integer(Number), Number > 0 -> neg_inf;
infinity_div(neg_inf, Number) when is_integer(Number), Number < 0 -> pos_inf;
infinity_div(Number, pos_inf) when is_integer(Number), Number >= 0 -> pos_inf;
infinity_div(Number, pos_inf) when is_integer(Number), Number < 0 -> neg_inf;
infinity_div(Number, neg_inf) when is_integer(Number), Number >= 0 -> neg_inf;
infinity_div(Number, neg_inf) when is_integer(Number), Number < 0 -> pos_inf;
infinity_div(Number1, Number2) when is_integer(Number1), is_integer(Number2) ->
  Number1 div Number2.

infinity_bsl(pos_inf, _) -> pos_inf;
infinity_bsl(neg_inf, _) -> neg_inf;
infinity_bsl(Number, pos_inf) when is_integer(Number), Number >= 0 -> pos_inf;
infinity_bsl(Number, pos_inf) when is_integer(Number) -> neg_inf;
infinity_bsl(Number, neg_inf) when is_integer(Number), Number >= 0 -> 0;
infinity_bsl(Number, neg_inf) when is_integer(Number) -> -1;
infinity_bsl(Number1, Number2) when is_integer(Number1), is_integer(Number2) ->
  Bits = ?BITS,
  if Number2 > (Bits * 2) -> infinity_bsl(Number1, pos_inf);
     Number2 < (-Bits * 2) -> infinity_bsl(Number1, neg_inf);
     true -> Number1 bsl Number2
  end.

infinity_geq(pos_inf, _) -> true;
infinity_geq(_, pos_inf) -> false;
infinity_geq(_, neg_inf) -> true;
infinity_geq(neg_inf, _) -> false;
infinity_geq(A, B) when is_integer(A), is_integer(B) -> A >= B.

-spec infinity_add(inf_integer(), inf_integer()) -> inf_integer().

infinity_add(pos_inf, _Number) -> pos_inf;
infinity_add(neg_inf, _Number) -> neg_inf;
infinity_add(_Number, pos_inf) -> pos_inf;
infinity_add(_Number, neg_inf) -> neg_inf;
infinity_add(Number1, Number2) when is_integer(Number1), is_integer(Number2) ->
  Number1 + Number2.

infinity_mult(neg_inf, Number) -> 
  Greater = infinity_geq(Number, 0), 
  if Greater -> neg_inf;
     true -> pos_inf
  end;
infinity_mult(pos_inf, Number) -> infinity_inv(infinity_mult(neg_inf, Number));
infinity_mult(Number, pos_inf) -> infinity_inv(infinity_mult(neg_inf, Number));
infinity_mult(Number, neg_inf) -> infinity_mult(neg_inf, Number);
infinity_mult(Number1, Number2) when is_integer(Number1), is_integer(Number2)->
  Number1 * Number2.

width({Min, Max}) -> infinity_max([width(Min), width(Max)]);
width(pos_inf) -> pos_inf;
width(neg_inf) -> pos_inf;
width(X) when is_integer(X), X >= 0 -> poswidth(X, 0);
width(X) when is_integer(X), X < 0 ->  negwidth(X, 0).

poswidth(X, N) ->
  case X < (1 bsl N) of
    true  -> N;
    false -> poswidth(X, N+1)
  end.

negwidth(X, N) ->
  case X >= (-1 bsl N) of
    true  -> N;
    false -> negwidth(X, N+1)
  end.

arith('bnot', X1) ->
  case t_is_integer(X1) of
    false -> error;
    true ->
      Min1 = number_min(X1),
      Max1 = number_max(X1),
      {ok, t_from_range(infinity_add(infinity_inv(Max1), -1),
			infinity_add(infinity_inv(Min1), -1))}
  end.

arith_mult(Min1, Max1, Min2, Max2) ->
  Tmp_list = [infinity_mult(Min1, Min2), infinity_mult(Min1, Max2),
	      infinity_mult(Max1, Min2), infinity_mult(Max1, Max2)],
  {infinity_min(Tmp_list), infinity_max(Tmp_list)}.

arith_div(_Min1, _Max1, 0, 0) ->
  %% Signal failure.
  {pos_inf, neg_inf};
arith_div(Min1, Max1, Min2, Max2) ->
  %% 0 is not an accepted divisor.
  NewMin2 = if Min2 =:= 0 -> 1;
	       true       -> Min2
	    end,
  NewMax2 = if Max2 =:= 0 -> -1;
	       true       -> Max2
	    end,
  Tmp_list = lists:flatten([infinity_div(Min1, NewMin2), 
			    infinity_div(Min1, NewMax2),
			    infinity_div(Max1, NewMin2), 
			    infinity_div(Max1, NewMax2)]),
  {infinity_min(Tmp_list), infinity_max(Tmp_list)}.

arith_rem(Min1, Max1, Min2, Max2) ->
  Min1_geq_zero = infinity_geq(Min1, 0),
  Max1_leq_zero = infinity_geq(0, Max1),
  Max_range2 = infinity_max([infinity_abs(Min2), infinity_abs(Max2)]),
  Max_range2_leq_zero = infinity_geq(0, Max_range2),
  New_min = 
    if Min1_geq_zero -> 0;
       Max_range2 =:= 0 -> 0;
       Max_range2_leq_zero -> infinity_add(Max_range2, 1);
       true -> infinity_add(infinity_inv(Max_range2), 1)
    end,
  New_max = 
    if Max1_leq_zero -> 0;
       Max_range2 =:= 0 -> 0;
       Max_range2_leq_zero -> infinity_add(infinity_inv(Max_range2), -1);
       true -> infinity_add(Max_range2, -1)
    end,
  {New_min, New_max}.

arith_bsl(Min1, Max1, Min2, Max2) ->
  case infinity_geq(Min1, 0) of
    true -> {infinity_bsl(Min1, Min2), infinity_bsl(Max1, Max2)};
    false ->
      case infinity_geq(Max1, 0) of
	true -> {infinity_bsl(Min1, Max2), infinity_bsl(Max1, Max2)};
	false -> {infinity_bsl(Min1, Max2), infinity_bsl(Max2, Min2)}
      end
  end.

arith_band_range_set({Min, Max}, [Int|IntList]) ->
  SafeAnd = lists:foldl(
	      fun (IntFromSet, SafeAndAcc) ->
		  IntFromSet bor SafeAndAcc
	      end,
	      Int,
	      IntList),
  {infinity_band(Min, SafeAnd), infinity_band(Max, SafeAnd)}.

arith_bor_range_set({Min, Max}, [Int|IntList]) ->
  SafeAnd = lists:foldl(
	      fun (IntFromSet, SafeAndAcc) ->
		  IntFromSet band SafeAndAcc
	      end,
	      Int,
	      IntList),
  {infinity_bor(Min, SafeAnd), infinity_bor(Max, SafeAnd)}.
	      
arith_band(X1, X2) ->
  L1 = t_number_vals(X1), 
  L2 = t_number_vals(X2),
  Min1 = number_min(X1),
  Max1 = number_max(X1),
  Min2 = number_min(X2),
  Max2 = number_max(X2),
  case {L1 =:= unknown, L2 =:= unknown} of
    {true, false} ->
      arith_band_range_set(arith_band_ranges(Min1, Max1, Min2, Max2), L2);
    {false, true} ->
      arith_band_range_set(arith_band_ranges(Min1, Max1, Min2, Max2), L1);
    {true, true}  ->
      arith_band_ranges(Min1, Max1, Min2, Max2)
  end.

arith_bor(X1, X2) ->
  L1 = t_number_vals(X1), 
  L2 = t_number_vals(X2),
  Min1 = number_min(X1),
  Max1 = number_max(X1),
  Min2 = number_min(X2),
  Max2 = number_max(X2),
  case {L1 =:= unknown, L2 =:= unknown} of
    {true, false} ->
      arith_bor_range_set(arith_bor_ranges(Min1, Max1, Min2, Max2), L2);
    {false, true} ->
      arith_bor_range_set(arith_bor_ranges(Min1, Max1, Min2, Max2), L1);
    {true, true}  ->
      arith_bor_ranges(Min1, Max1, Min2, Max2)
  end.

arith_band_ranges(Min1, Max1, Min2, Max2) ->
  Width = infinity_min([width({Min1, Max1}), width({Min2, Max2})]),
  Min =
    case infinity_geq(Min1, 0) orelse infinity_geq(Min2, 0) of
      true -> 0;
      false -> infinity_bsl(-1, Width)
    end,
  Max =
    case infinity_geq(Max1, 0) orelse infinity_geq(Max2, 0) of
      true -> infinity_add(infinity_bsl(1, Width), -1);
      false -> 0
    end,
  {Min, Max}.

arith_bor_ranges(Min1, Max1, Min2, Max2) ->
  Width = infinity_max([width({Min1, Max1}), width({Min2, Max2})]),
  Min =
    case infinity_geq(Min1, 0) andalso infinity_geq(Min2, 0) of
      true -> 0;
      false -> infinity_bsl(-1, Width)
    end,	  
  Max =
    case infinity_geq(Max1, 0) andalso infinity_geq(Max2, 0) of
      true -> infinity_add(infinity_bsl(1, Width), -1);
      false -> -1
    end,
  {Min, Max}.

arith(Op, X1, X2) ->
  %% io:format("arith ~p ~p ~p~n", [Op, X1, X2]),
  case t_is_integer(X1) andalso t_is_integer(X2) of
    false -> error;
    true ->
      L1 = t_number_vals(X1), 
      L2 = t_number_vals(X2),
      case (L1 =:= unknown) orelse (L2 =:= unknown) of
	true ->
	  Min1 = number_min(X1),
	  Max1 = number_max(X1),
	  Min2 = number_min(X2),
	  Max2 = number_max(X2),
	  {NewMin, NewMax} =
	    case Op of
	      '+'    -> {infinity_add(Min1, Min2), infinity_add(Max1, Max2)};
	      '-'    -> {infinity_add(Min1, infinity_inv(Max2)), 
			 infinity_add(Max1, infinity_inv(Min2))};
	      '*'    -> arith_mult(Min1, Max1, Min2, Max2);
	      'div'  -> arith_div(Min1, Max1, Min2, Max2);
	      'rem'  -> arith_rem(Min1, Max1, Min2, Max2);
	      'bsl'  -> arith_bsl(Min1, Max1, Min2, Max2);
	      'bsr'  -> NewMin2 = infinity_inv(Max2),
			NewMax2 = infinity_inv(Min2),
			arith_bsl(Min1, Max1, NewMin2, NewMax2);
	      'band' -> arith_band(X1, X2);
	      'bor'  -> arith_bor(X1, X2);
	      'bxor' -> arith_bor_ranges(Min1, Max1, Min2, Max2) %% overaprox.
	    end,
	  %% io:format("done arith ~p = ~p~n", [Op, {NewMin, NewMax}]),
	  {ok, t_from_range(NewMin, NewMax)};
	false ->
	  %% Some of these arithmetic operations might throw a system_limit
	  %% exception; for example, when trying to evaluate 1 bsl 100000000.
	  try case Op of
		'+'    -> [X + Y    || X <- L1, Y <- L2];
		'-'    -> [X - Y    || X <- L1, Y <- L2];
		'*'    -> [X * Y    || X <- L1, Y <- L2];
		'div'  -> [X div Y  || X <- L1, Y <- L2, Y =/= 0];
		'rem'  -> [X rem Y  || X <- L1, Y <- L2, Y =/= 0];
		'bsl'  -> [X bsl Y  || X <- L1, Y <- L2];
		'bsr'  -> [X bsr Y  || X <- L1, Y <- L2];
		'band' -> [X band Y || X <- L1, Y <- L2];
		'bor'  -> [X bor Y  || X <- L1, Y <- L2];
		'bxor' -> [X bxor Y || X <- L1, Y <- L2]
	      end of
	    AllVals ->
	      {ok, t_integers(ordsets:from_list(AllVals))}
	  catch
	    error:system_limit -> error
	  end
      end
  end.

%%=============================================================================

-spec arg_types(atom(), atom(), arity()) -> [erl_types:erl_type()] | 'unknown'.

%%------- binary --------------------------------------------------------------
arg_types(binary, at, 2) ->
  [t_binary(), t_non_neg_integer()];
arg_types(binary, bin_to_list, 1) ->
  [t_binary()];
arg_types(binary, bin_to_list, 2) ->
  [t_binary(), t_binary_part()];
arg_types(binary, bin_to_list, 3) ->
  [t_binary(), t_integer(), t_non_neg_integer()];
arg_types(binary, compile_pattern, 1) ->
  [t_sup(t_binary(), t_list(t_binary()))];
arg_types(binary, copy, 1) ->
  [t_binary()];
arg_types(binary, copy, 2) ->
  [t_binary(), t_non_neg_integer()];
arg_types(binary, decode_unsigned, 1) ->
  [t_binary()];
arg_types(binary, decode_unsigned, 2) ->
  [t_binary(), t_endian()];
arg_types(binary, encode_unsigned, 1) ->
  [t_non_neg_integer()];
arg_types(binary, encode_unsigned, 2) ->
  [t_non_neg_integer(), t_endian()];
arg_types(binary, first, 1) ->
  [t_binary()];
arg_types(binary, last, 1) ->
  [t_binary()];
arg_types(binary, list_to_bin, 1) ->
  arg_types(erlang, list_to_binary, 1);
arg_types(binary, longest_common_prefix, 1) ->
  [t_list(t_binary())];
arg_types(binary, longest_common_suffix, 1) ->
  [t_list(t_binary())];
arg_types(binary, match, 2) ->
  [t_binary(), t_binary_pattern()];
arg_types(binary, match, 3) ->
  [t_binary(), t_binary_pattern(), t_binary_options()];
arg_types(binary, matches, 2) ->
  [t_binary(), t_binary_pattern()];
arg_types(binary, matches, 3) ->
  [t_binary(), t_binary_pattern(), t_binary_options()];
arg_types(binary, part, 2) ->
  arg_types(erlang, binary_part, 2);
arg_types(binary, part, 3) ->
  arg_types(erlang, binary_part, 3);
arg_types(binary, referenced_byte_size, 1) ->
  [t_binary()];
%%------- code ----------------------------------------------------------------
arg_types(code, add_path, 1) ->
  [t_string()];
arg_types(code, add_patha, 1) ->
  arg_types(code, add_path, 1);
arg_types(code, add_paths, 1) ->
  [t_list(t_string())];
arg_types(code, add_pathsa, 1) ->
  arg_types(code, add_paths, 1);
arg_types(code, add_pathsz, 1) ->
  arg_types(code, add_paths, 1);
arg_types(code, add_pathz, 1) ->
  arg_types(code, add_path, 1);
arg_types(code, all_loaded, 0) ->
  [];
arg_types(code, compiler_dir, 0) ->
  [];
arg_types(code, del_path, 1) ->
  [t_sup(t_string(), t_atom())];  % OBS: doc differs from add_path/1 - why?
arg_types(code, delete, 1) ->
  [t_atom()];
arg_types(code, ensure_loaded, 1) ->
  arg_types(code, load_file, 1);
arg_types(code, get_chunk, 2) ->
  [t_binary(), t_string()];
arg_types(code, get_object_code, 1) ->
  [t_atom()];
arg_types(code, get_path, 0) ->
  [];
arg_types(code, is_loaded, 1) ->
  [t_atom()];
arg_types(code, is_sticky, 1) ->
  [t_atom()];
arg_types(code, is_module_native, 1) ->
  [t_atom()];
arg_types(code, lib_dir, 0) ->
  [];
arg_types(code, lib_dir, 1) ->
  [t_atom()];
arg_types(code, load_abs, 1) ->
  [t_string()];
arg_types(code, load_abs, 2) ->
  [t_code_loaded_fname_or_status(), t_atom()];
arg_types(code, load_binary, 3) ->
  [t_atom(), t_code_loaded_fname_or_status(), t_binary()];
arg_types(code, load_file, 1) ->
  [t_atom()];
arg_types(code, load_native_partial, 2) ->
  [t_atom(), t_binary()];
arg_types(code, load_native_sticky, 3) ->
  [t_atom(), t_binary(), t_sup(t_binary(), t_atom('false'))];
arg_types(code, module_md5, 1) ->
  [t_binary()];
arg_types(code, make_stub_module, 3) ->
  [t_atom(), t_binary(), t_tuple([t_list(), t_list()])];
arg_types(code, priv_dir, 1) ->
  [t_atom()];
arg_types(code, purge, 1) ->
  arg_types(code, delete, 1);
arg_types(code, rehash, 0) ->
  [];
arg_types(code, replace_path, 2) ->
  [t_atom(), t_string()];
arg_types(code, root_dir, 0) ->
  [];
arg_types(code, set_path, 1) ->
  [t_string()];
arg_types(code, soft_purge, 1) ->
  arg_types(code, delete, 1);
arg_types(code, stick_mod, 1) ->
  [t_atom()];
arg_types(code, unstick_mod, 1) ->
  arg_types(code, stick_mod, 1);
arg_types(code, which, 1) ->
  [t_atom()];
%%------- erl_ddll ------------------------------------------------------------
arg_types(erl_ddll, demonitor, 1) ->
  arg_types(erlang, demonitor, 1);
arg_types(erl_ddll, format_error_int, 1) ->
  [t_sup([t_atom('inconsistent'),
	  t_atom('linked_in_driver'),
	  t_atom('permanent'),
	  t_atom('not_loaded'),
	  t_atom('not_loaded_by_this_process'),
	  t_atom('not_pending'),
	  t_atom('already_loaded'),
	  t_atom('unloading')])];
arg_types(erl_ddll, info, 2) ->
  [t_sup([t_atom(), t_string()]),
   t_sup([t_atom('awaiting_load'),
	  t_atom('awaiting_unload'),
	  t_atom('driver_options'),
	  t_atom('linked_in_driver'),
	  t_atom('permanent'),
	  t_atom('port_count'),
	  t_atom('processes')])];
arg_types(erl_ddll, loaded_drivers, 0) ->
  [];
arg_types(erl_ddll, monitor, 2) ->
  [t_atom('driver'),
   t_tuple([t_atom(), t_sup([t_atom('loaded'), t_atom('unloaded')])])];
arg_types(erl_ddll, try_load, 3) ->
  [t_sup([t_atom(), t_string(), t_nonempty_list(t_sup([t_atom(), t_string()]))]),
   t_sup([t_atom(), t_string()]),
   t_list(t_sup([t_tuple([t_atom('driver_options'),
			  t_list(t_atom('kill_ports'))]),
		 t_tuple([t_atom('monitor'),
			  t_sup([t_atom('pending_driver'),
				 t_atom('pending')])]),
		 t_tuple([t_atom('reload'),
			  t_sup([t_atom('pending_driver'),
				 t_atom('pending')])])]))];
arg_types(erl_ddll, try_unload, 2) ->
  [t_sup([t_atom(), t_string(), t_nonempty_list(t_sup([t_atom(), t_string()]))]),
   t_list(t_sup([t_atom('kill_ports'),
		 t_tuple([t_atom('monitor'),
			  t_sup([t_atom('pending_driver'),
				 t_atom('pending')])])]))];
%%------- erlang --------------------------------------------------------------
arg_types(erlang, '!', 2) ->
  Pid = t_sup([t_pid(), t_port(), t_atom(),
	       t_tuple([t_atom(), t_node()])]),
  [Pid, t_any()];
arg_types(erlang, '==', 2) ->
  [t_any(), t_any()];
arg_types(erlang, '/=', 2) ->
  [t_any(), t_any()];
arg_types(erlang, '=:=', 2) ->
  [t_any(), t_any()];
arg_types(erlang, '=/=', 2) ->
  [t_any(), t_any()];
arg_types(erlang, '>', 2) ->
  [t_any(), t_any()];
arg_types(erlang, '>=', 2) ->
  [t_any(), t_any()];
arg_types(erlang, '<', 2) ->
  [t_any(), t_any()];
arg_types(erlang, '=<', 2) ->
  [t_any(), t_any()];
arg_types(erlang, '+', 1) ->
  [t_number()];
arg_types(erlang, '+', 2) ->
  [t_number(), t_number()];
arg_types(erlang, '++', 2) ->
  [t_list(), t_any()];
arg_types(erlang, '-', 1) ->
  [t_number()];
arg_types(erlang, '-', 2) ->
  [t_number(), t_number()];
arg_types(erlang, '--', 2) ->
  [t_list(), t_list()];
arg_types(erlang, '*', 2) ->
  [t_number(), t_number()];
arg_types(erlang, '/', 2) ->
  [t_number(), t_number()];
arg_types(erlang, 'div', 2) ->
  [t_integer(), t_integer()];
arg_types(erlang, 'rem', 2) ->
  [t_integer(), t_integer()];
arg_types(erlang, 'and', 2) ->
  [t_boolean(), t_boolean()];
arg_types(erlang, 'or', 2) ->
  [t_boolean(), t_boolean()];
arg_types(erlang, 'xor', 2) ->
  [t_boolean(), t_boolean()];
arg_types(erlang, 'not', 1) ->
  [t_boolean()];
arg_types(erlang, 'band', 2) ->
  [t_integer(), t_integer()];
arg_types(erlang, 'bor', 2) ->
  [t_integer(), t_integer()];
arg_types(erlang, 'bxor', 2) ->
  [t_integer(), t_integer()];
arg_types(erlang, 'bsr', 2) ->
  [t_integer(), t_integer()];
arg_types(erlang, 'bsl', 2) ->
  [t_integer(), t_integer()];
arg_types(erlang, 'bnot', 1) ->
  [t_integer()];
arg_types(erlang, abs, 1) ->
  [t_number()];
arg_types(erlang, adler32, 1) ->
  [t_iodata()];
arg_types(erlang, adler32, 2) ->
  [t_adler32(), t_iodata()];
arg_types(erlang, adler32_combine, 3) ->
  [t_adler32(), t_adler32(), t_non_neg_integer()];
arg_types(erlang, append, 2) ->
  arg_types(erlang, '++', 2);
arg_types(erlang, append_element, 2) ->
  [t_tuple(), t_any()];
arg_types(erlang, apply, 2) ->
  [t_sup(t_tuple([t_module(),
		  t_atom()]),
	 t_fun()),
   t_list()];
arg_types(erlang, apply, 3) ->
  [t_sup(t_atom(), t_tuple()), t_atom(), t_list()];
arg_types(erlang, atom_to_binary, 2) ->
  [t_atom(), t_encoding_a2b()];
arg_types(erlang, atom_to_list, 1) ->
  [t_atom()];
arg_types(erlang, binary_part, 2) ->
  [t_binary(), t_tuple([t_integer(),t_integer()])];
arg_types(erlang, binary_part, 3) ->
  [t_binary(), t_integer(), t_integer()];
arg_types(erlang, binary_to_atom, 2) ->
  [t_binary(), t_encoding_a2b()];
arg_types(erlang, binary_to_existing_atom, 2) ->
  arg_types(erlang, binary_to_atom, 2);
arg_types(erlang, binary_to_list, 1) ->
  [t_binary()];
arg_types(erlang, binary_to_list, 3) ->
  [t_binary(), t_pos_integer(), t_pos_integer()]; % I want fixnum, but cannot
arg_types(erlang, binary_to_term, 1) ->
  [t_binary()];
arg_types(erlang, binary_to_term, 2) ->
  [t_binary(), t_list(t_atom('safe'))];
arg_types(erlang, bitsize, 1) ->	% XXX: TAKE OUT
  arg_types(erlang, bit_size, 1);
arg_types(erlang, bit_size, 1) ->
  [t_bitstr()];
arg_types(erlang, bitstr_to_list, 1) ->	% XXX: TAKE OUT
  arg_types(erlang, bitstring_to_list, 1);
arg_types(erlang, bitstring_to_list, 1) ->
  [t_bitstr()];
arg_types(erlang, bump_reductions, 1) ->
  [t_pos_fixnum()];
arg_types(erlang, byte_size, 1) ->
  [t_binary()];
arg_types(erlang, call_on_load_function, 1) ->
  [t_atom()];
arg_types(erlang, cancel_timer, 1) ->
  [t_reference()];
arg_types(erlang, check_process_code, 2) ->
  [t_pid(), t_atom()];
arg_types(erlang, concat_binary, 1) ->
  [t_list(t_binary())];
arg_types(erlang, crc32, 1) ->
  [t_iodata()];
arg_types(erlang, crc32, 2) ->
  [t_crc32(), t_iodata()];
arg_types(erlang, crc32_combine, 3) ->
  [t_crc32(), t_crc32(), t_non_neg_integer()];
arg_types(erlang, date, 0) ->
  [];
arg_types(erlang, decode_packet, 3) ->
  [t_decode_packet_type(), t_binary(), t_list(t_decode_packet_option())];
arg_types(erlang, delete_module, 1) ->
  [t_atom()];
arg_types(erlang, demonitor, 1) ->
  [t_reference()];
arg_types(erlang, demonitor, 2) ->
  [t_reference(), t_list(t_atoms(['flush', 'info']))];
arg_types(erlang, disconnect_node, 1) ->
  [t_node()];
arg_types(erlang, display, 1) ->
  [t_any()];
arg_types(erlang, display_nl, 0) ->
  [];
arg_types(erlang, display_string, 1) ->
  [t_string()];
arg_types(erlang, dist_exit, 3) ->
  [t_pid(), t_dist_exit(), t_sup(t_pid(), t_port())];
arg_types(erlang, element, 2) ->
  [t_pos_fixnum(), t_tuple()];
arg_types(erlang, erase, 0) ->
  [];
arg_types(erlang, erase, 1) ->
  [t_any()];
arg_types(erlang, error, 1) ->
  [t_any()];
arg_types(erlang, error, 2) ->
  [t_any(), t_list()];
arg_types(erlang, exit, 1) ->
  [t_any()];
arg_types(erlang, exit, 2) ->
  [t_sup(t_pid(), t_port()), t_any()];
arg_types(erlang, external_size, 1) ->
  [t_any()]; % takes any term as input
arg_types(erlang, finish_after_on_load, 2) ->
  [t_atom(), t_boolean()];
arg_types(erlang, float, 1) ->
  [t_number()];
arg_types(erlang, float_to_list, 1) ->
  [t_float()];
arg_types(erlang, function_exported, 3) ->
  [t_atom(), t_atom(), t_arity()];
arg_types(erlang, fun_info, 1) ->
  [t_fun()];
arg_types(erlang, fun_info, 2) ->
  [t_fun(), t_atom()];
arg_types(erlang, fun_to_list, 1) ->
  [t_fun()];
arg_types(erlang, garbage_collect, 0) ->
  [];
arg_types(erlang, garbage_collect, 1) ->
  [t_pid()];
arg_types(erlang, get, 0) ->
  [];
arg_types(erlang, get, 1) ->
  [t_any()];
arg_types(erlang, get_cookie, 0) ->
  [];
arg_types(erlang, get_keys, 1) ->
  [t_any()];
arg_types(erlang, get_stacktrace, 0) ->
  [];
arg_types(erlang, get_module_info, 1) ->
  [t_atom()];
arg_types(erlang, get_module_info, 2) ->
  [t_atom(), t_module_info_2()];
arg_types(erlang, group_leader, 0) ->
  [];
arg_types(erlang, group_leader, 2) ->
  [t_pid(), t_pid()];
arg_types(erlang, halt, 0) ->
  [];
arg_types(erlang, halt, 1) ->
  [t_sup(t_non_neg_fixnum(), t_string())];
arg_types(erlang, hash, 2) ->
  [t_any(), t_integer()];
arg_types(erlang, hd, 1) ->
  [t_cons()];
arg_types(erlang, hibernate, 3) ->
  [t_atom(), t_atom(), t_list()];
arg_types(erlang, info, 1) ->
  arg_types(erlang, system_info, 1); % alias
arg_types(erlang, iolist_to_binary, 1) ->
  [t_sup(t_iolist(), t_binary())];
arg_types(erlang, iolist_size, 1) ->
  [t_sup(t_iolist(), t_binary())];
arg_types(erlang, integer_to_list, 1) ->
  [t_integer()];
arg_types(erlang, integer_to_list, 2) ->
  [t_integer(), t_from_range(2, 36)];
arg_types(erlang, is_alive, 0) ->
  [];
arg_types(erlang, is_atom, 1) ->
  [t_any()];
arg_types(erlang, is_binary, 1) ->
  [t_any()];
arg_types(erlang, is_bitstr, 1) ->	% XXX: TAKE OUT
  arg_types(erlang, is_bitstring, 1);
arg_types(erlang, is_bitstring, 1) ->
  [t_any()];
arg_types(erlang, is_boolean, 1) ->
  [t_any()];
arg_types(erlang, is_builtin, 3) ->
  [t_atom(), t_atom(), t_arity()];
arg_types(erlang, is_constant, 1) ->
  [t_any()];
arg_types(erlang, is_float, 1) ->
  [t_any()];
arg_types(erlang, is_function, 1) ->
  [t_any()];
arg_types(erlang, is_function, 2) ->
  [t_any(), t_arity()];
arg_types(erlang, is_integer, 1) ->
  [t_any()];
arg_types(erlang, is_list, 1) ->
  [t_any()];
arg_types(erlang, is_number, 1) ->
  [t_any()];
arg_types(erlang, is_pid, 1) ->
  [t_any()];
arg_types(erlang, is_port, 1) ->
  [t_any()];
arg_types(erlang, is_process_alive, 1) ->
  [t_pid()];
arg_types(erlang, is_record, 2) ->
  [t_any(), t_atom()];
arg_types(erlang, is_record, 3) ->
  [t_any(), t_atom(), t_pos_fixnum()];
arg_types(erlang, is_reference, 1) ->
  [t_any()];
arg_types(erlang, is_tuple, 1) ->
  [t_any()];
arg_types(erlang, length, 1) ->
  [t_list()];
arg_types(erlang, link, 1) ->
  [t_sup(t_pid(), t_port())];
arg_types(erlang, list_to_atom, 1) ->
  [t_string()];
arg_types(erlang, list_to_binary, 1) ->
  [t_iolist()];
arg_types(erlang, list_to_bitstr, 1) ->	% XXX: TAKE OUT
  arg_types(erlang, list_to_bitstring, 1);
arg_types(erlang, list_to_bitstring, 1) ->
  [t_iolist()];
arg_types(erlang, list_to_existing_atom, 1) ->
  [t_string()];
arg_types(erlang, list_to_float, 1) ->
  [t_list(t_byte())];
arg_types(erlang, list_to_integer, 1) ->
  [t_list(t_byte())];
arg_types(erlang, list_to_integer, 2) ->
  [t_list(t_byte()), t_from_range(2, 36)];
arg_types(erlang, list_to_pid, 1) ->
  [t_string()];
arg_types(erlang, list_to_tuple, 1) ->
  [t_list()];
arg_types(erlang, load_module, 2) ->
  [t_atom(), t_binary()];
arg_types(erlang, load_nif, 2) ->
  [t_string(), t_any()];
arg_types(erlang, loaded, 0) ->
  [];
arg_types(erlang, localtime, 0) ->
  [];
arg_types(erlang, localtime_to_universaltime, 1) ->
  [t_tuple([t_date(), t_time()])];
arg_types(erlang, localtime_to_universaltime, 2) ->
  arg_types(erlang, localtime_to_universaltime, 1) ++
    [t_sup(t_boolean(), t_atom('undefined'))];
arg_types(erlang, make_fun, 3) ->
  [t_atom(), t_atom(), t_arity()];
arg_types(erlang, make_ref, 0) ->
  [];
arg_types(erlang, make_tuple, 2) ->
  [t_non_neg_fixnum(), t_any()];  % the value 0 is OK as first argument
arg_types(erlang, make_tuple, 3) ->
  [t_non_neg_fixnum(), t_any(), t_list(t_tuple([t_pos_integer(), t_any()]))];
arg_types(erlang, match_spec_test, 3) ->
  [t_sup(t_list(), t_tuple()),
   t_any(),
   t_sup(t_atom('table'), t_atom('trace'))];
arg_types(erlang, md5, 1) ->
  [t_sup(t_iolist(), t_binary())];
arg_types(erlang, md5_final, 1) ->
  [t_binary()];
arg_types(erlang, md5_init, 0) ->
  [];
arg_types(erlang, md5_update, 2) ->
  [t_binary(), t_sup(t_iolist(), t_binary())];
arg_types(erlang, memory, 0) ->
  [];
arg_types(erlang, memory, 1) ->
  Arg = t_atoms(['total', 'processes', 'processes_used', 'system',
		 'atom', 'atom_used', 'binary', 'code', 'ets',
		 'maximum']),
  [t_sup(Arg, t_list(Arg))];
arg_types(erlang, module_loaded, 1) ->
  [t_atom()];
arg_types(erlang, monitor, 2) ->
  [t_atom(), t_sup([t_pid(), t_atom(), t_tuple([t_atom(), t_node()])])];
arg_types(erlang, monitor_node, 2) ->
  [t_node(), t_boolean()];
arg_types(erlang, monitor_node, 3) ->
  [t_node(), t_boolean(), t_list(t_atom('allow_passive_connect'))];
arg_types(erlang, node, 0) ->
  [];
arg_types(erlang, node, 1) ->
  [t_identifier()];
arg_types(erlang, nodes, 0) ->
  [];
arg_types(erlang, nodes, 1) ->
  NodesArg = t_atoms(['visible', 'hidden', 'connected', 'this', 'known']),
  [t_sup(NodesArg, t_list(NodesArg))];
arg_types(erlang, now, 0) ->
  [];
arg_types(erlang, open_port, 2) ->
  [t_sup(t_atom(), t_sup([t_tuple([t_atom('spawn'), t_string()]),
			  t_tuple([t_atom('spawn_driver'), t_string()]),
			  t_tuple([t_atom('spawn_executable'), t_string()]),
			  t_tuple([t_atom('fd'), t_integer(), t_integer()])])),
   t_list(t_sup(t_sup([t_atom('stream'),
		       t_atom('exit_status'),
		       t_atom('use_stdio'),
		       t_atom('nouse_stdio'),
		       t_atom('stderr_to_stdout'),
		       t_atom('in'),
		       t_atom('out'),
		       t_atom('binary'),
		       t_atom('eof'),
		       t_atom('hide')]),
		t_sup([t_tuple([t_atom('packet'), t_integer()]),
		       t_tuple([t_atom('line'), t_integer()]),
		       t_tuple([t_atom('cd'), t_string()]),
		       t_tuple([t_atom('env'), t_list(t_tuple(2))]), % XXX: More
		       t_tuple([t_atom('args'), t_list(t_string())]),
		       t_tuple([t_atom('arg0'), t_string()])])))];
arg_types(erlang, phash, 2) ->
  [t_any(), t_pos_integer()];
arg_types(erlang, phash2, 1) ->
  [t_any()];
arg_types(erlang, phash2, 2) ->
  [t_any(), t_pos_integer()];
arg_types(erlang, pid_to_list, 1) ->
  [t_pid()];
arg_types(erlang, port_call, 3) ->
  [t_sup(t_port(), t_atom()), t_integer(), t_any()];
arg_types(erlang, port_close, 1) ->
  [t_sup(t_port(), t_atom())];
arg_types(erlang, port_command, 2) ->
  [t_sup(t_port(), t_atom()), t_sup(t_iolist(), t_binary())];
arg_types(erlang, port_command, 3) ->
  [t_sup(t_port(), t_atom()),
   t_sup(t_iolist(), t_binary()),
   t_list(t_atoms(['force', 'nosuspend']))];
arg_types(erlang, port_connect, 2) ->
  [t_sup(t_port(), t_atom()), t_pid()];
arg_types(erlang, port_control, 3) ->
  [t_sup(t_port(), t_atom()), t_integer(), t_sup(t_iolist(), t_binary())];
arg_types(erlang, port_get_data, 1) ->
  [t_sup(t_port(), t_atom())];
arg_types(erlang, port_info, 1) ->
  [t_sup(t_port(), t_atom())];
arg_types(erlang, port_info, 2) ->
  [t_sup(t_port(), t_atom()),
   t_atoms(['registered_name', 'id', 'connected',
	    'links', 'name', 'input', 'output'])];
arg_types(erlang, port_to_list, 1) ->
  [t_port()];
arg_types(erlang, ports, 0) ->
  [];
arg_types(erlang, port_set_data, 2) ->
  [t_sup(t_port(), t_atom()), t_any()];
arg_types(erlang, pre_loaded, 0) ->
  [];
arg_types(erlang, process_display, 2) ->
  [t_pid(), t_atom('backtrace')];
arg_types(erlang, process_flag, 2) ->
  [t_sup([t_atom('trap_exit'), t_atom('error_handler'),
	  t_atom('min_heap_size'), t_atom('priority'), t_atom('save_calls'),
	  t_atom('monitor_nodes'), 			  % undocumented
	  t_tuple([t_atom('monitor_nodes'), t_list()])]), % undocumented
   t_sup([t_boolean(), t_atom(), t_non_neg_integer()])];
arg_types(erlang, process_flag, 3) ->
  [t_pid(), t_atom('save_calls'), t_non_neg_integer()];
arg_types(erlang, process_info, 1) ->
  [t_pid()];
arg_types(erlang, process_info, 2) ->
  [t_pid(), t_pinfo()];
arg_types(erlang, processes, 0) ->
  [];
arg_types(erlang, purge_module, 1) ->
  [t_atom()];
arg_types(erlang, put, 2) ->
  [t_any(), t_any()];
arg_types(erlang, raise, 3) ->
  [t_raise_errorclass(), t_any(), type(erlang, get_stacktrace, 0, [])];
arg_types(erlang, read_timer, 1) ->
  [t_reference()];
arg_types(erlang, ref_to_list, 1) ->
  [t_reference()];
arg_types(erlang, register, 2) ->
  [t_atom(), t_sup(t_port(), t_pid())];
arg_types(erlang, registered, 0) ->
  [];
arg_types(erlang, resume_process, 1) ->
  [t_pid()]; % intended for debugging only
arg_types(erlang, round, 1) ->
  [t_number()];
arg_types(erlang, self, 0) ->
  [];
arg_types(erlang, send, 2) ->
  arg_types(erlang, '!', 2);  % alias
arg_types(erlang, send, 3) ->
  arg_types(erlang, send, 2) ++ [t_list(t_sendoptions())];
arg_types(erlang, send_after, 3) ->
  [t_non_neg_integer(), t_sup(t_pid(), t_atom()), t_any()];
arg_types(erlang, seq_trace, 2) ->
  [t_atom(), t_sup([t_boolean(), t_tuple([t_fixnum(), t_fixnum()]), t_nil()])];
arg_types(erlang, seq_trace_info, 1) ->
  [t_seq_trace_info()];
arg_types(erlang, seq_trace_print, 1) ->
  [t_any()];
arg_types(erlang, seq_trace_print, 2) ->
  [t_sup(t_atom(), t_fixnum()), t_any()];
arg_types(erlang, set_cookie, 2) ->
  [t_node(), t_atom()];
arg_types(erlang, setelement, 3) ->
  [t_pos_integer(), t_tuple(), t_any()];
arg_types(erlang, setnode, 2) ->
  [t_atom(), t_integer()];
arg_types(erlang, setnode, 3) ->
  [t_atom(), t_port(), t_tuple(4)];
arg_types(erlang, size, 1) ->
  [t_sup(t_tuple(), t_binary())];
arg_types(erlang, spawn, 1) -> %% TODO: Tuple?
  [t_fun()];
arg_types(erlang, spawn, 2) -> %% TODO: Tuple?
  [t_node(), t_fun()];
arg_types(erlang, spawn, 3) -> %% TODO: Tuple?
  [t_atom(), t_atom(), t_list()];
arg_types(erlang, spawn, 4) -> %% TODO: Tuple?
  [t_node(), t_atom(), t_atom(), t_list()];
arg_types(erlang, spawn_link, 1) ->
  arg_types(erlang, spawn, 1);  % same
arg_types(erlang, spawn_link, 2) ->
  arg_types(erlang, spawn, 2);  % same
arg_types(erlang, spawn_link, 3) ->
  arg_types(erlang, spawn, 3);  % same
arg_types(erlang, spawn_link, 4) ->
  arg_types(erlang, spawn, 4);  % same
arg_types(erlang, spawn_opt, 1) ->
  [t_tuple([t_atom(), t_atom(), t_list(), t_list(t_spawn_options())])];
arg_types(erlang, spawn_opt, 2) ->
  [t_fun(), t_list(t_spawn_options())];
arg_types(erlang, spawn_opt, 3) ->
  [t_atom(), t_fun(), t_list(t_spawn_options())];
arg_types(erlang, spawn_opt, 4) ->
  [t_node(), t_atom(), t_list(), t_list(t_spawn_options())];
arg_types(erlang, split_binary, 2) ->
  [t_binary(), t_non_neg_integer()];
arg_types(erlang, start_timer, 3) ->
  [t_non_neg_integer(), t_sup(t_pid(), t_atom()), t_any()];
arg_types(erlang, statistics, 1) ->
  [t_sup([t_atom('context_switches'),
	  t_atom('exact_reductions'),
	  t_atom('garbage_collection'),
	  t_atom('io'),
	  t_atom('reductions'),
	  t_atom('run_queue'),
	  t_atom('runtime'),
	  t_atom('wall_clock')])];
arg_types(erlang, subtract, 2) ->
  arg_types(erlang, '--', 2);
arg_types(erlang, suspend_process, 1) ->
  [t_pid()];
arg_types(erlang, suspend_process, 2) ->
  [t_pid(), t_list(t_sup([t_atom('unless_suspending'),
			  t_atom('asynchronous')]))];
arg_types(erlang, system_flag, 2) ->
  [t_sup([t_atom('backtrace_depth'),
	  t_atom('cpu_topology'),
	  t_atom('debug_flags'),	% undocumented
	  t_atom('display_items'),	% undocumented
	  t_atom('fullsweep_after'),
	  t_atom('min_heap_size'),
	  t_atom('multi_scheduling'),
	  t_atom('schedulers_online'),
	  t_atom('scheduler_bind_type'),
	  %% Undocumented; used to implement (the documented) seq_trace module.
	  t_atom('sequential_tracer'),
	  t_atom('trace_control_word'),
	  %% 'internal_cpu_topology' is an undocumented internal feature.
	  t_atom('internal_cpu_topology'),
	  t_integer()]),
   t_sup([t_integer(),
	  %% 'cpu_topology'
	  t_system_cpu_topology(),
	  %% 'scheduler_bind_type'
	  t_scheduler_bind_type_args(),
	  %% Undocumented: the following is for 'debug_flags' that
	  %% takes any erlang term as flags and currently ignores it.
	  %% t_any(),	% commented out since it destroys the type signature
	  %%
	  %% Again undocumented; the following are for 'sequential_tracer'
	  t_sequential_tracer(),
	  %% The following two are for 'multi_scheduling'
	  t_atom('block'),
	  t_atom('unblock'),
	  %% The following is for 'internal_cpu_topology'
	  t_internal_cpu_topology()])];
arg_types(erlang, system_info, 1) ->
  [t_sup([t_atom(),                     % documented
	  t_tuple([t_atom(), t_any()]), % documented
	  t_tuple([t_atom(), t_atom(), t_any()])])];
arg_types(erlang, system_monitor, 0) ->
  [];
arg_types(erlang, system_monitor, 1) ->
  [t_system_monitor_settings()];
arg_types(erlang, system_monitor, 2) ->
  [t_pid(), t_system_monitor_options()];
arg_types(erlang, system_profile, 0) ->
  [];
arg_types(erlang, system_profile, 2) ->
  [t_sup([t_pid(), t_port(), t_atom('undefined')]),
   t_system_profile_options()];
arg_types(erlang, term_to_binary, 1) ->
  [t_any()];
arg_types(erlang, term_to_binary, 2) ->
  [t_any(), t_list(t_sup([t_atom('compressed'),
			  t_tuple([t_atom('compressed'), t_from_range(0, 9)]),
			  t_tuple([t_atom('minor_version'), t_integers([0, 1])])]))];
arg_types(erlang, throw, 1) ->
  [t_any()];
arg_types(erlang, time, 0) ->
  [];
arg_types(erlang, tl, 1) ->
  [t_cons()];
arg_types(erlang, trace, 3) ->
  [t_sup(t_pid(), t_sup([t_atom('existing'), t_atom('new'), t_atom('all')])),
   t_boolean(),
   t_list(t_sup(t_atom(), t_tuple(2)))];
arg_types(erlang, trace_delivered, 1) ->
  [t_sup(t_pid(), t_atom('all'))];
arg_types(erlang, trace_info, 2) ->
  [t_sup([%% the following two get info about a PID
	  t_pid(), t_atom('new'),
	  %% while the following two get info about a func
	  t_mfa(), t_atom('on_load')]),
   t_sup([%% the following are items about a PID
	  t_atom('flags'), t_atom('tracer'),
	  %% while the following are items about a func
	  t_atom('traced'), t_atom('match_spec'), t_atom('meta'),
	  t_atom('meta_match_spec'), t_atom('call_count'), t_atom('all')])];
arg_types(erlang, trace_pattern, 2) ->
  [t_sup(t_tuple([t_atom(), t_atom(), t_sup(t_arity(), t_atom('_'))]),
	 t_atom('on_load')),
   t_sup([t_boolean(), t_list(), t_atom('restart'), t_atom('pause')])];
arg_types(erlang, trace_pattern, 3) ->
  arg_types(erlang, trace_pattern, 2) ++
    [t_list(t_sup([t_atom('global'), t_atom('local'),
		   t_atom('meta'), t_tuple([t_atom('meta'), t_pid()]),
		   t_atom('call_count')]))];
arg_types(erlang, trunc, 1) ->
  [t_number()];
arg_types(erlang, tuple_size, 1) ->
  [t_tuple()];
arg_types(erlang, tuple_to_list, 1) ->
  [t_tuple()];
arg_types(erlang, universaltime, 0) ->
  [];
arg_types(erlang, universaltime_to_localtime, 1) ->
  [t_tuple([t_date(), t_time()])];
arg_types(erlang, unlink, 1) ->
  [t_sup(t_pid(), t_port())];
arg_types(erlang, unregister, 1) ->
  [t_atom()];
arg_types(erlang, whereis, 1) ->
  [t_atom()];
arg_types(erlang, yield, 0) ->
  [];
%%------- erl_prim_loader -----------------------------------------------------
arg_types(erl_prim_loader, get_file, 1) ->
  [t_sup(t_atom(), t_string())];
arg_types(erl_prim_loader, get_path, 0) ->
  [];
arg_types(erl_prim_loader, set_path, 1) ->
  [t_list(t_string())];
%%------- error_logger --------------------------------------------------------
arg_types(error_logger, warning_map, 0) ->
  [];
%%------- erts_debug ----------------------------------------------------------
arg_types(erts_debug, breakpoint, 2) ->
  [t_tuple([t_atom(), t_atom(), t_sup(t_integer(), t_atom('_'))]), t_boolean()];
arg_types(erts_debug, disassemble, 1) ->
  [t_sup(t_mfa(), t_integer())];
arg_types(erts_debug, dist_ext_to_term, 2) ->
  [t_tuple(), t_binary()];
arg_types(erts_debug, flat_size, 1) ->
  [t_any()];
arg_types(erts_debug, lock_counters, 1) ->
  [t_sup([t_atom(enabled),
	  t_atom(info),
	  t_atom(clear),
	  t_tuple([t_atom(copy_save), t_boolean()]),
	  t_tuple([t_atom(process_locks), t_boolean()])])];
arg_types(erts_debug, same, 2) ->
  [t_any(), t_any()];
%%------- ets -----------------------------------------------------------------
arg_types(ets, all, 0) ->
  [];
arg_types(ets, delete, 1) ->
  [t_tab()];
arg_types(ets, delete, 2) ->
  [t_tab(), t_any()];
arg_types(ets, delete_all_objects, 1) ->
  [t_tab()];
arg_types(ets, delete_object, 2) ->
  [t_tab(), t_tuple()];
arg_types(ets, first, 1) ->
  [t_tab()];
arg_types(ets, give_away, 3) ->
  [t_tab(), t_pid(), t_any()];
arg_types(ets, info, 1) ->
  [t_tab()];
arg_types(ets, info, 2) ->
  [t_tab(), t_ets_info_items()];
arg_types(ets, insert, 2) ->
  [t_tab(), t_sup(t_tuple(), t_list(t_tuple()))];
arg_types(ets, insert_new, 2) ->
  [t_tab(), t_sup(t_tuple(), t_list(t_tuple()))];
arg_types(ets, is_compiled_ms, 1) ->
  [t_any()];
arg_types(ets, last, 1) ->
  arg_types(ets, first, 1);
arg_types(ets, lookup, 2) ->
  [t_tab(), t_any()];
arg_types(ets, lookup_element, 3) ->
  [t_tab(), t_any(), t_pos_fixnum()];
arg_types(ets, match, 1) ->
  [t_any()];
arg_types(ets, match, 2) ->
  [t_tab(), t_match_pattern()];
arg_types(ets, match, 3) ->
  [t_tab(), t_match_pattern(), t_pos_fixnum()];
arg_types(ets, match_object, 1) ->
  arg_types(ets, match, 1);
arg_types(ets, match_object, 2) ->
  arg_types(ets, match, 2);
arg_types(ets, match_object, 3) ->
  arg_types(ets, match, 3);
arg_types(ets, match_spec_compile, 1) ->
  [t_matchspecs()];
arg_types(ets, match_spec_run_r, 3) ->
  [t_matchspecs(), t_any(), t_list()];
arg_types(ets, member, 2) ->
  [t_tab(), t_any()];
arg_types(ets, new, 2) ->
  [t_atom(), t_ets_new_options()];
arg_types(ets, next, 2) ->
  [t_tab(), t_any()];
arg_types(ets, prev, 2) ->
  [t_tab(), t_any()];
arg_types(ets, rename, 2) ->
  [t_atom(), t_atom()];
arg_types(ets, safe_fixtable, 2) ->
  [t_tab(), t_boolean()];
arg_types(ets, select, 1) ->
  [t_any()];
arg_types(ets, select, 2) ->
  [t_tab(), t_matchspecs()];
arg_types(ets, select, 3) ->
  [t_tab(), t_matchspecs(), t_pos_fixnum()];
arg_types(ets, select_count, 2) ->
  [t_tab(), t_matchspecs()];
arg_types(ets, select_delete, 2) ->
  [t_tab(), t_matchspecs()];
arg_types(ets, select_reverse, 1) ->
  arg_types(ets, select, 1);
arg_types(ets, select_reverse, 2) ->
  arg_types(ets, select, 2);
arg_types(ets, select_reverse, 3) ->
  arg_types(ets, select, 3);
arg_types(ets, slot, 2) ->
  [t_tab(), t_non_neg_fixnum()]; % 2nd arg can be 0
arg_types(ets, setopts, 2) ->
  Opt = t_sup(t_tuple([t_atom('heir'), t_pid(), t_any()]),
	      t_tuple([t_atom('heir'), t_atom('none')])),
  [t_tab(), t_sup(Opt, t_list(Opt))];
arg_types(ets, update_counter, 3) ->
  [t_tab(), t_any(), t_sup(t_integer(),
			   t_sup(t_tuple([t_integer(), t_integer()]),
				 t_tuple([t_integer(), t_integer(),
					  t_integer(), t_integer()])))];
arg_types(ets, update_element, 3) ->
  PosValue = t_tuple([t_integer(), t_any()]),
  [t_tab(), t_any(), t_sup(PosValue, t_list(PosValue))];
%%------- file ----------------------------------------------------------------
arg_types(file, close, 1) ->
  [t_file_io_device()];
arg_types(file, delete, 1) ->
  [t_file_name()];
arg_types(file, get_cwd, 0) ->
  [];
arg_types(file, make_dir, 1) ->
  [t_file_name()];
arg_types(file, open, 2) ->
  [t_file_name(), t_list(t_file_open_option())];
arg_types(file, read_file, 1) ->
  [t_file_name()];
arg_types(file, set_cwd, 1) ->
  [t_file_name()];
arg_types(file, write, 2) ->
  [t_file_io_device(), t_iodata()];
arg_types(file, write_file, 2) ->
  [t_file_name(), t_sup(t_binary(), t_list())];
%%------- gen_tcp -------------------------------------------------------------
arg_types(gen_tcp, accept, 1) ->
  [t_socket()];
arg_types(gen_tcp, accept, 2) ->
  [t_socket(), t_timeout()];
arg_types(gen_tcp, connect, 3) ->
  [t_gen_tcp_address(), t_gen_tcp_port(), t_list(t_gen_tcp_connect_option())];
arg_types(gen_tcp, connect, 4) ->
  arg_types(gen_tcp, connect, 3) ++ [t_timeout()];
arg_types(gen_tcp, listen, 2) ->
  [t_gen_tcp_port(), t_list(t_gen_tcp_listen_option())];
arg_types(gen_tcp, recv, 2) ->
  [t_socket(), t_non_neg_integer()];
arg_types(gen_tcp, recv, 3) ->
  arg_types(gen_tcp, recv, 2) ++ [t_timeout()];
arg_types(gen_tcp, send, 2) ->
  [t_socket(), t_packet()];
arg_types(gen_tcp, shutdown, 2) ->
  [t_socket(), t_sup([t_atom('read'), t_atom('write'), t_atom('read_write')])];
%%------- gen_udp -------------------------------------------------------------
arg_types(gen_udp, open, 1) ->
  [t_gen_tcp_port()];
arg_types(gen_udp, open, 2) ->
  [t_gen_tcp_port(), t_list(t_gen_udp_connect_option())];
arg_types(gen_udp, recv, 2) ->
  arg_types(gen_tcp, recv, 2);
arg_types(gen_udp, recv, 3) ->
  arg_types(gen_tcp, recv, 3);
arg_types(gen_udp, send, 4) ->
  [t_socket(), t_gen_tcp_address(), t_gen_tcp_port(), t_packet()];
%%------- hipe_bifs -----------------------------------------------------------
arg_types(hipe_bifs, add_ref, 2) ->
  [t_mfa(), t_tuple([t_mfa(),
		     t_integer(),
		     t_sup(t_atom('call'), t_atom('load_mfa')),
		     t_trampoline(),
		     t_sup(t_atom('remote'), t_atom('local'))])];
arg_types(hipe_bifs, alloc_data, 2) ->
  [t_integer(), t_integer()];
arg_types(hipe_bifs, array, 2) ->
  [t_non_neg_fixnum(), t_immediate()];
arg_types(hipe_bifs, array_length, 1) ->
  [t_immarray()];
arg_types(hipe_bifs, array_sub, 2) ->
  [t_immarray(), t_non_neg_fixnum()];
arg_types(hipe_bifs, array_update, 3) ->
  [t_immarray(), t_non_neg_fixnum(), t_immediate()];
arg_types(hipe_bifs, atom_to_word, 1) ->
  [t_atom()];
arg_types(hipe_bifs, bif_address, 3) ->
  [t_atom(), t_atom(), t_arity()];
arg_types(hipe_bifs, bitarray, 2) ->
  [t_non_neg_fixnum(), t_boolean()];
arg_types(hipe_bifs, bitarray_sub, 2) ->
  [t_bitarray(), t_non_neg_fixnum()];
arg_types(hipe_bifs, bitarray_update, 3) ->
  [t_bytearray(), t_non_neg_fixnum(), t_boolean()];
arg_types(hipe_bifs, bytearray, 2) ->
  [t_non_neg_fixnum(), t_byte()];
arg_types(hipe_bifs, bytearray_sub, 2) ->
  [t_bytearray(), t_non_neg_fixnum()];
arg_types(hipe_bifs, bytearray_update, 3) ->
  [t_bytearray(), t_non_neg_fixnum(), t_byte()];
arg_types(hipe_bifs, call_count_clear, 1) ->
  [t_mfa()];
arg_types(hipe_bifs, call_count_get, 1) ->
  [t_mfa()];
arg_types(hipe_bifs, call_count_off, 1) ->
  [t_mfa()];
arg_types(hipe_bifs, call_count_on, 1) ->
  [t_mfa()];
arg_types(hipe_bifs, check_crc, 1) ->
  [t_crc32()];
arg_types(hipe_bifs, enter_code, 2) ->
  [t_binary(), t_sup(t_nil(), t_tuple())];
arg_types(hipe_bifs, enter_sdesc, 1) ->
  [t_tuple([t_integer(), t_integer(), t_integer(), t_integer(), t_integer()])];
arg_types(hipe_bifs, find_na_or_make_stub, 2) ->
  [t_mfa(), t_boolean()];
arg_types(hipe_bifs, fun_to_address, 1) ->
  [t_mfa()];
%% arg_types(hipe_bifs, get_emu_address, 1) ->
%%   [t_mfa()];
arg_types(hipe_bifs, get_rts_param, 1) ->
  [t_fixnum()];
arg_types(hipe_bifs, invalidate_funinfo_native_addresses, 1) ->
  [t_list(t_mfa())];
arg_types(hipe_bifs, make_fe, 3) ->
  [t_integer(), t_atom(), t_tuple([t_integer(), t_integer(), t_integer()])];
%% arg_types(hipe_bifs, make_native_stub, 2) ->
%%   [t_integer(), t_arity()];
arg_types(hipe_bifs, mark_referred_from, 1) ->
  [t_mfa()];
arg_types(hipe_bifs, merge_term, 1) ->
  [t_any()];
arg_types(hipe_bifs, patch_call, 3) ->
  [t_integer(), t_integer(), t_trampoline()];
arg_types(hipe_bifs, patch_insn, 3) ->
  [t_integer(), t_integer(), t_insn_type()];
arg_types(hipe_bifs, primop_address, 1) ->
  [t_atom()];
arg_types(hipe_bifs, redirect_referred_from, 1) ->
  [t_mfa()];
arg_types(hipe_bifs, ref, 1) ->
  [t_immediate()];
arg_types(hipe_bifs, ref_get, 1) ->
  [t_hiperef()];
arg_types(hipe_bifs, ref_set, 2) ->
  [t_hiperef(), t_immediate()];
arg_types(hipe_bifs, remove_refs_from, 1) ->
  [t_mfa()];
arg_types(hipe_bifs, set_funinfo_native_address, 3) ->
  arg_types(hipe_bifs, set_native_address, 3);
arg_types(hipe_bifs, set_native_address, 3) ->
  [t_mfa(), t_integer(), t_boolean()];
arg_types(hipe_bifs, system_crc, 1) ->
  [t_crc32()];
arg_types(hipe_bifs, term_to_word, 1) ->
  [t_any()];
arg_types(hipe_bifs, update_code_size, 3) ->
  [t_atom(), t_sup(t_nil(), t_binary()), t_integer()];
arg_types(hipe_bifs, write_u8, 2) ->
  [t_integer(), t_byte()];
arg_types(hipe_bifs, write_u32, 2) ->
  [t_integer(), t_integer()];
arg_types(hipe_bifs, write_u64, 2) ->
  [t_integer(), t_integer()];
%%------- io ------------------------------------------------------------------
arg_types(io, format, 1) ->
  [t_io_format_string()];
arg_types(io, format, 2) ->
  [t_io_format_string(), t_list()];
arg_types(io, format, 3) ->
  [t_io_device(), t_io_format_string(), t_list()];
arg_types(io, fwrite, 1) ->
  arg_types(io, format, 1);
arg_types(io, fwrite, 2) ->
  arg_types(io, format, 2);
arg_types(io, fwrite, 3) ->
  arg_types(io, format, 3);
arg_types(io, put_chars, 1) ->
  [t_iodata()];
arg_types(io, put_chars, 2) ->
  [t_io_device(), t_iodata()];
%%------- io_lib --------------------------------------------------------------
arg_types(io_lib, format, 2) ->
  arg_types(io, format, 2);
arg_types(io_lib, fwrite, 2) ->
  arg_types(io_lib, format, 2);
%%------- lists ---------------------------------------------------------------
arg_types(lists, all, 2) ->
  [t_fun([t_any()], t_boolean()), t_list()];
arg_types(lists, any, 2) ->
  [t_fun([t_any()], t_boolean()), t_list()];
arg_types(lists, append, 2) -> 
  arg_types(erlang, '++', 2);  % alias
arg_types(lists, delete, 2) ->
  [t_any(), t_maybe_improper_list()];
arg_types(lists, dropwhile, 2) ->
  [t_fun([t_any()], t_boolean()), t_maybe_improper_list()];
arg_types(lists, filter, 2) ->
  [t_fun([t_any()], t_boolean()), t_list()];
arg_types(lists, flatten, 1) ->
  [t_list()];
arg_types(lists, flatmap, 2) ->
  [t_fun([t_any()], t_list()), t_list()];
arg_types(lists, foreach, 2) ->
  [t_fun([t_any()], t_any()), t_list()];
arg_types(lists, foldl, 3) ->
  [t_fun([t_any(), t_any()], t_any()), t_any(), t_list()];
arg_types(lists, foldr, 3) -> 
  arg_types(lists, foldl, 3);  % same
arg_types(lists, keydelete, 3) ->
  [t_any(), t_pos_fixnum(), t_maybe_improper_list()]; % t_list(t_tuple())];
arg_types(lists, keyfind, 3) ->
  arg_types(lists, keysearch, 3);
arg_types(lists, keymap, 3) ->
  [t_fun([t_any()], t_any()), t_pos_fixnum(), t_list(t_tuple())];
arg_types(lists, keymember, 3) ->
  [t_any(), t_pos_fixnum(), t_maybe_improper_list()]; % t_list(t_tuple());
arg_types(lists, keymerge, 3) ->
  [t_pos_fixnum(), t_list(t_tuple()), t_list(t_tuple())];
arg_types(lists, keyreplace, 4) ->
  [t_any(), t_pos_fixnum(), t_maybe_improper_list(), t_tuple()]; % t_list(t_tuple())];
arg_types(lists, keysearch, 3) ->
  [t_any(), t_pos_fixnum(), t_maybe_improper_list()]; % t_list(t_tuple())];
arg_types(lists, keysort, 2) ->
  [t_pos_fixnum(), t_list(t_tuple())];
arg_types(lists, last, 1) ->
  [t_nonempty_list()];
arg_types(lists, map, 2) ->
  [t_fun([t_any()], t_any()), t_list()];
arg_types(lists, mapfoldl, 3) ->
  [t_fun([t_any(), t_any()], t_tuple([t_any(), t_any()])), t_any(), t_list()];
arg_types(lists, mapfoldr, 3) -> 
  arg_types(lists, mapfoldl, 3); % same
arg_types(lists, max, 1) ->
  [t_nonempty_list()];
arg_types(lists, member, 2) ->
  [t_any(), t_list()];
%% arg_types(lists, merge, 1) ->
%%   [t_list(t_list())];
arg_types(lists, merge, 2) ->
  [t_list(), t_list()];
%% arg_types(lists, merge, 3) ->
%%   [t_fun([t_any(), t_any()], t_boolean()), t_list(), t_list()];
%% arg_types(lists, merge3, 3) ->
%%   [t_list(), t_list(), t_list()];
arg_types(lists, min, 1) ->
  [t_nonempty_list()];
arg_types(lists, nth, 2) ->
  [t_pos_fixnum(), t_nonempty_list()];
arg_types(lists, nthtail, 2) ->
  [t_non_neg_fixnum(), t_nonempty_list()];
arg_types(lists, partition, 2) ->
  arg_types(lists, filter, 2); % same
arg_types(lists, reverse, 1) ->
  [t_list()];
arg_types(lists, reverse, 2) ->
  [t_list(), t_any()];
arg_types(lists, seq, 2) ->
  [t_integer(), t_integer()];
arg_types(lists, seq, 3) ->
  [t_integer(), t_integer(), t_integer()];
arg_types(lists, sort, 1) ->
  [t_list()];
arg_types(lists, sort, 2) ->
  [t_fun([t_any(), t_any()], t_boolean()), t_list()];
arg_types(lists, split, 2) ->
  [t_non_neg_fixnum(), t_maybe_improper_list()]; % do not lie in 2nd arg
arg_types(lists, splitwith, 2) ->
  [t_fun([t_any()], t_boolean()), t_maybe_improper_list()];
arg_types(lists, subtract, 2) ->
  arg_types(erlang, '--', 2);  % alias
arg_types(lists, takewhile, 2) ->
  [t_fun([t_any()], t_boolean()), t_maybe_improper_list()];
arg_types(lists, usort, 1) ->
  arg_types(lists, sort, 1);   % same
arg_types(lists, usort, 2) ->
  arg_types(lists, sort, 2);
arg_types(lists, unzip, 1) ->
  [t_list(t_tuple(2))];
arg_types(lists, unzip3, 1) ->
  [t_list(t_tuple(3))];
arg_types(lists, zip, 2) ->
  [t_list(), t_list()];
arg_types(lists, zip3, 3) ->
  [t_list(), t_list(), t_list()];
arg_types(lists, zipwith, 3) ->
  [t_fun([t_any(), t_any()], t_any()), t_list(), t_list()];
arg_types(lists, zipwith3, 4) ->
  [t_fun([t_any(), t_any(), t_any()], t_any()), t_list(), t_list(), t_list()];
%%------- math ----------------------------------------------------------------
arg_types(math, acos, 1) ->
  [t_number()];
arg_types(math, acosh, 1) ->
  [t_number()];
arg_types(math, asin, 1) ->
  [t_number()];
arg_types(math, asinh, 1) ->
  [t_number()];
arg_types(math, atan, 1) ->
  [t_number()];
arg_types(math, atan2, 2) ->
  [t_number(), t_number()];
arg_types(math, atanh, 1) ->
  [t_number()];
arg_types(math, cos, 1) ->
  [t_number()];
arg_types(math, cosh, 1) ->
  [t_number()];
arg_types(math, erf, 1) ->
  [t_number()];
arg_types(math, erfc, 1) ->
  [t_number()];
arg_types(math, exp, 1) ->
  [t_number()];
arg_types(math, log, 1) ->
  [t_number()];
arg_types(math, log10, 1) ->
  [t_number()];
arg_types(math, pi, 0) ->
  [];
arg_types(math, pow, 2) ->
  [t_number(), t_number()];
arg_types(math, sin, 1) ->
  [t_number()];
arg_types(math, sinh, 1) ->
  [t_number()];
arg_types(math, sqrt, 1) ->
  [t_number()];
arg_types(math, tan, 1) ->
  [t_number()];
arg_types(math, tanh, 1) ->
  [t_number()];
%%-- net_kernel ---------------------------------------------------------------
arg_types(net_kernel, dflag_unicode_io, 1) ->
  [t_pid()];
%%------- ordsets -------------------------------------------------------------
arg_types(ordsets, filter, 2) ->
  arg_types(lists, filter, 2);
arg_types(ordsets, fold, 3) ->
  arg_types(lists, foldl, 3);
%%------- os ------------------------------------------------------------------
arg_types(os, getenv, 0) ->
  [];
arg_types(os, getenv, 1) ->
  [t_string()];
arg_types(os, getpid, 0) ->
  [];
arg_types(os, putenv, 2) ->
  [t_string(), t_string()];
arg_types(os, timestamp, 0) ->
  [];
%%-- re -----------------------------------------------------------------------
arg_types(re, compile, 1) ->
  [t_iodata()];
arg_types(re, compile, 2) ->
  [t_iodata(), t_list(t_re_compile_option())];
arg_types(re, run, 2) ->
  [t_iodata(), t_re_RE()];
arg_types(re, run, 3) ->
  [t_iodata(), t_re_RE(), t_list(t_re_run_option())];
%%------- string --------------------------------------------------------------
arg_types(string, chars, 2) ->
  [t_char(), t_non_neg_integer()];
arg_types(string, chars, 3) ->
  [t_char(), t_non_neg_integer(), t_any()];
arg_types(string, concat, 2) ->
  [t_string(), t_string()];
arg_types(string, equal, 2) ->
  [t_string(), t_string()];
arg_types(string, to_float, 1) ->
  [t_string()];
arg_types(string, to_integer, 1) ->
  [t_string()];
%%------- unicode -------------------------------------------------------------
arg_types(unicode, characters_to_binary, 2) ->
  [t_ML(), t_encoding()];
arg_types(unicode, characters_to_list, 2) ->
  [t_ML(), t_encoding()];
arg_types(unicode, bin_is_7bit, 1) ->
  [t_binary()];
%%-----------------------------------------------------------------------------
arg_types(M, F, A) when is_atom(M), is_atom(F),
			is_integer(A), 0 =< A, A =< 255 ->
  unknown.                     % safe approximation for all functions.


-spec is_known(atom(), atom(), arity()) -> boolean().

is_known(M, F, A) ->
  arg_types(M, F, A) =/= unknown.


-spec structure_inspecting_args(atom(), atom(), arity()) -> [1..255].

structure_inspecting_args(erlang, element, 2) -> [2];
structure_inspecting_args(erlang, is_atom, 1) -> [1];
structure_inspecting_args(erlang, is_boolean, 1) -> [1];
structure_inspecting_args(erlang, is_binary, 1) -> [1];
structure_inspecting_args(erlang, is_bitstring, 1) -> [1];
structure_inspecting_args(erlang, is_float, 1) -> [1];
structure_inspecting_args(erlang, is_function, 1) -> [1];
structure_inspecting_args(erlang, is_integer, 1) -> [1];
structure_inspecting_args(erlang, is_list, 1) -> [1];
structure_inspecting_args(erlang, is_number, 1) -> [1];
structure_inspecting_args(erlang, is_pid, 1) -> [1];
structure_inspecting_args(erlang, is_port, 1) -> [1];
structure_inspecting_args(erlang, is_reference, 1) -> [1];
structure_inspecting_args(erlang, is_tuple, 1) -> [1];
structure_inspecting_args(erlang, length, 1) -> [1];
%%structure_inspecting_args(erlang, setelement, 3) -> [2].
structure_inspecting_args(_, _, _) -> []. % XXX: assume no arg needs inspection


check_fun_application(Fun, Args) ->
  case t_is_fun(Fun) of
    true ->
      case t_fun_args(Fun) of
	unknown ->
	  case t_is_none_or_unit(t_fun_range(Fun)) of
	    true -> error;
	    false -> ok
	  end;
	FunDom when length(FunDom) =:= length(Args) ->
	  case any_is_none_or_unit(inf_lists(FunDom, Args)) of
	    true -> error;
	    false ->
	      case t_is_none_or_unit(t_fun_range(Fun)) of
		true -> error;
		false -> ok
	      end
	  end;
	_ -> error
      end;
    false ->
      error
  end.


%% =====================================================================
%% These are basic types that should probably be moved to erl_types
%% =====================================================================

t_socket() -> t_port(). % alias

t_ip_address() ->
  T_int16 = t_from_range(0,  16#FFFF),
  t_sup(t_tuple([t_byte(), t_byte(), t_byte(), t_byte()]),
	t_tuple([T_int16, T_int16, T_int16, T_int16,
		 T_int16, T_int16, T_int16, T_int16])).

%% =====================================================================
%% Some basic types used in various parts of the system
%% =====================================================================

t_date() ->
  t_tuple([t_pos_fixnum(), t_pos_fixnum(), t_pos_fixnum()]).

t_time() ->
  t_tuple([t_non_neg_fixnum(), t_non_neg_fixnum(), t_non_neg_fixnum()]).

t_packet() ->
  t_sup([t_binary(), t_iolist(), t_httppacket()]).

t_httppacket() ->
  t_sup([t_HttpRequest(), t_HttpResponse(),
	 t_HttpHeader(), t_atom('http_eoh'), t_HttpError()]).

t_endian() ->
  t_sup([t_atom('big'), t_atom('little')]).

%% =====================================================================
%% Types for the binary module
%% =====================================================================

t_binary_part() ->
  t_tuple([t_non_neg_integer(),t_integer()]).

t_binary_canonical_part() ->
  t_tuple([t_non_neg_integer(),t_non_neg_integer()]).

t_binary_pattern() ->
  t_sup([t_binary(),
	 t_list(t_binary()),
	 t_binary_compiled_pattern()]).

t_binary_compiled_pattern() ->
  t_tuple([t_atom('cp'),t_binary()]).

t_binary_options() ->
  t_list(t_tuple([t_atom('scope'),t_binary_part()])).

%% =====================================================================
%% HTTP types documented in R12B-4
%% =====================================================================

t_HttpRequest() ->
  t_tuple([t_atom('http_request'), t_HttpMethod(), t_HttpUri(), t_HttpVersion()]).

t_HttpResponse() ->
   t_tuple([t_atom('http_response'), t_HttpVersion(), t_integer(), t_string()]).

t_HttpHeader() ->
  t_tuple([t_atom('http_header'), t_integer(), t_HttpField(), t_any(), t_string()]).

t_HttpError() ->
  t_tuple([t_atom('http_error'), t_string()]).

t_HttpMethod() ->
  t_sup(t_HttpMethodAtom(), t_string()).

t_HttpMethodAtom() ->
  t_atoms(['OPTIONS', 'GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'TRACE']).

t_HttpUri() ->
  t_sup([t_atom('*'),
	 t_tuple([t_atom('absoluteURI'),
		  t_sup(t_atom('http'), t_atom('https')),
		  t_string(),
		  t_sup(t_non_neg_integer(), t_atom('undefined')),
		  t_string()]),
	 t_tuple([t_atom('scheme'), t_string(), t_string()]),
	 t_tuple([t_atom('abs_path'), t_string()]),
	 t_string()]).

t_HttpVersion() ->
  t_tuple([t_non_neg_integer(), t_non_neg_integer()]).

t_HttpField() ->
  t_sup(t_HttpFieldAtom(), t_string()).

t_HttpFieldAtom() ->
  t_atoms(['Cache-Control', 'Connection', 'Date', 'Pragma', 'Transfer-Encoding',
	   'Upgrade', 'Via', 'Accept', 'Accept-Charset', 'Accept-Encoding',
	   'Accept-Language', 'Authorization', 'From', 'Host',
	   'If-Modified-Since', 'If-Match', 'If-None-Match', 'If-Range',
	   'If-Unmodified-Since', 'Max-Forwards', 'Proxy-Authorization',
	   'Range', 'Referer', 'User-Agent', 'Age', 'Location',
	   'Proxy-Authenticate', 'Public', 'Retry-After', 'Server', 'Vary',
	   'Warning', 'Www-Authenticate', 'Allow', 'Content-Base',
	   'Content-Encoding', 'Content-Language', 'Content-Length',
	   'Content-Location', 'Content-Md5', 'Content-Range', 'Content-Type',
	   'Etag', 'Expires', 'Last-Modified', 'Accept-Ranges',
	   'Set-Cookie', 'Set-Cookie2', 'X-Forwarded-For', 'Cookie',
	   'Keep-Alive', 'Proxy-Connection']).

%% =====================================================================
%% These are used for the built-in functions of 'code'
%% =====================================================================

t_code_load_return(Mod) ->
  t_sup(t_tuple([t_atom('module'), case t_is_atom(Mod) of
				     true -> Mod;
				     false -> t_atom()
				   end]),
	t_tuple([t_atom('error'), t_code_load_error_rsn()])).

t_code_load_error_rsn() ->	% also used in erlang:load_module/2
  t_sup([t_atom('badfile'),
	 t_atom('nofile'),
	 t_atom('not_purged'),
	 t_atom('native_code'),
	 t_atom('on_load'),
	 t_atom('sticky_directory')]).	% only for the 'code' functions

t_code_loaded_fname_or_status() ->
  t_sup([t_string(), % filename
	 t_atom('preloaded'),
	 t_atom('cover_compiled')]).

%% =====================================================================
%% These are used for the built-in functions of 'erlang'
%% =====================================================================

t_adler32() ->
  t_non_neg_integer().

t_crc32() ->
  t_non_neg_integer().

t_decode_packet_option() ->
  t_sup([t_tuple([t_atom('packet_size'), t_non_neg_integer()]),
	 t_tuple([t_atom('line_length'), t_non_neg_integer()])]).

t_decode_packet_type() ->
  t_sup([t_inet_setoption_packettype(), t_atom('httph'), t_atom('httph_bin')]).

t_dist_exit() ->
  t_sup([t_atom('kill'), t_atom('noconnection'), t_atom('normal')]).

t_match_spec_test_errors() ->
  t_list(t_sup(t_tuple([t_atom('error'), t_string()]),
	       t_tuple([t_atom('warning'), t_string()]))).

t_module_info_2() ->
 t_sup([t_atom('module'),
	t_atom('imports'),
	t_atom('exports'),
	t_atom('functions'),
	t_atom('attributes'),
	t_atom('compile'),
	t_atom('native_addresses')]).

t_pinfo() ->
  t_sup([t_pinfo_item(), t_list(t_pinfo_item())]).

t_pinfo_item() ->
  t_sup([t_atom('backtrace'),
	 t_atom('current_function'),
	 t_atom('dictionary'),
	 t_atom('error_handler'),
	 t_atom('garbage_collection'),
	 t_atom('group_leader'),
	 t_atom('heap_size'),
	 t_atom('initial_call'),
	 t_atom('last_calls'),
	 t_atom('links'),
	 t_atom('memory'),
	 t_atom('message_binary'),     % for hybrid heap only
	 t_atom('message_queue_len'),
	 t_atom('messages'),
	 t_atom('monitored_by'),
	 t_atom('monitors'),
	 t_atom('priority'),
	 t_atom('reductions'),
	 t_atom('registered_name'),
	 t_atom('sequential_trace_token'),
	 t_atom('stack_size'),
	 t_atom('status'),
	 t_atom('suspending'),
	 t_atom('total_heap_size'),
	 t_atom('trap_exit')]).

t_process_priority_level() ->
  t_sup([t_atom('max'), t_atom('high'), t_atom('normal'), t_atom('low')]).

t_process_status() ->
  t_sup([t_atom('runnable'), t_atom('running'),
	 t_atom('suspended'), t_atom('waiting')]).

t_raise_errorclass() ->
  t_sup([t_atom('error'), t_atom('exit'), t_atom('throw')]).

t_sendoptions() ->
  t_sup(t_atom('noconnect'), t_atom('nosuspend')).

t_seq_trace_info() ->
  t_sup([t_atom('send'),
	 t_atom('receive'),
	 t_atom('print'),
	 t_atom('timestamp'),
	 t_atom('label'),
	 t_atom('serial')]).

%% XXX: Better if we also maintain correspondencies between infos and values
t_seq_trace_info_returns() ->
  Values = t_sup([t_non_neg_integer(), t_boolean(),
		  t_tuple([t_non_neg_integer(), t_non_neg_integer()])]),
  t_sup(t_tuple([t_seq_trace_info(), Values]), t_nil()).

t_sequential_tracer() ->
  t_sup([t_atom('false'), t_pid(), t_port()]).

t_spawn_options() ->
  t_sup([t_atom('link'),
	 t_atom('monitor'),
	 t_tuple([t_atom('priority'), t_process_priority_level()]),
	 t_tuple([t_atom('min_heap_size'), t_fixnum()]),
	 t_tuple([t_atom('fullsweep_after'), t_fixnum()])]).

t_spawn_opt_return(List) ->
  case t_is_none(t_inf(t_list(t_atom('monitor')), List)) of
    true -> t_pid();
    false -> t_sup(t_pid(), t_tuple([t_pid(), t_reference()]))
  end.

t_system_cpu_topology() ->
  t_sup(t_atom('undefined'), t_system_cpu_topology_level_entry_list()).

t_system_cpu_topology_level_entry_list() ->
  t_list(t_system_cpu_topology_level_entry()).

t_system_cpu_topology_level_entry() ->
  t_sup(t_tuple([t_system_cpu_topology_level_tag(),
		 t_system_cpu_topology_sublevel_entry()]),
	t_tuple([t_system_cpu_topology_level_tag(),
		 t_system_cpu_topology_info_list(),
		 t_system_cpu_topology_sublevel_entry()])).

t_system_cpu_topology_sublevel_entry() ->
  t_sup(t_system_cpu_topology_logical_cpu_id(),
	t_list(t_tuple())). % approximation

t_system_cpu_topology_level_tag() ->
  t_atoms(['core', 'node', 'processor', 'thread']).

t_system_cpu_topology_logical_cpu_id() ->
  t_tuple([t_atom('logical'), t_non_neg_fixnum()]).

t_system_cpu_topology_info_list() ->
  t_nil().  % it may be extended in the future

t_internal_cpu_topology() -> %% Internal undocumented type
  t_sup(t_list(t_tuple([t_atom('cpu'),
			t_non_neg_fixnum(),
			t_non_neg_fixnum(),
			t_non_neg_fixnum(),
			t_non_neg_fixnum(),
			t_non_neg_fixnum(),
			t_non_neg_fixnum()])),
	t_atom('undefined')).

t_scheduler_bind_type_args() ->
  t_sup([t_atom('default_bind'),
	 t_atom('no_node_processor_spread'),
	 t_atom('no_node_thread_spread'),
	 t_atom('no_spread'),
	 t_atom('processor_spread'),
	 t_atom('spread'),
	 t_atom('thread_spread'),
	 t_atom('thread_no_node_processor_spread'),
	 t_atom('unbound')]).
  
t_scheduler_bind_type_results() ->
  t_sup([t_atom('no_node_processor_spread'),
	 t_atom('no_node_thread_spread'),
	 t_atom('no_spread'),
	 t_atom('processor_spread'),
	 t_atom('spread'),
	 t_atom('thread_spread'),
	 t_atom('thread_no_node_processor_spread'),
	 t_atom('unbound')]).


t_system_monitor_settings() ->
  t_sup([t_atom('undefined'),
	 t_tuple([t_pid(), t_system_monitor_options()])]).

t_system_monitor_options() ->
  t_list(t_sup([t_atom('busy_port'),
		t_atom('busy_dist_port'),
		t_tuple([t_atom('long_gc'), t_integer()]),
		t_tuple([t_atom('large_heap'), t_integer()])])).

t_system_multi_scheduling() ->
  t_sup([t_atom('blocked'), t_atom('disabled'), t_atom('enabled')]).

t_system_profile_options() ->
  t_list(t_sup([t_atom('exclusive'),
		t_atom('runnable_ports'),
		t_atom('runnable_procs'),
		t_atom('scheduler')])).

t_system_profile_return() ->
  t_sup(t_atom('undefined'),
	t_tuple([t_sup(t_pid(), t_port()), t_system_profile_options()])).

%% =====================================================================
%% These are used for the built-in functions of 'ets'
%% =====================================================================

t_tab() ->
  t_sup(t_tid(), t_atom()).

t_match_pattern() ->
  t_sup(t_atom(), t_tuple()).

t_matchspecs() ->
  t_list(t_tuple([t_match_pattern(), t_list(), t_list()])).

t_matchres() ->
  t_sup(t_tuple([t_list(), t_any()]), t_atom('$end_of_table')).

%% From the 'ets' documentation
%%-----------------------------
%%   Option = Type | Access | named_table | {keypos,Pos}
%%          | {heir,pid(),HeirData} | {heir,none}
%%          | {write_concurrency,boolean()}
%%     Type = set | ordered_set | bag | duplicate_bag
%%   Access = public | protected | private
%%      Pos = integer()
%% HeirData = term()
t_ets_new_options() ->
  t_list(t_sup([t_atom('set'),
		t_atom('ordered_set'),
		t_atom('bag'),
		t_atom('duplicate_bag'),
		t_atom('public'),
		t_atom('protected'),
		t_atom('private'),
		t_atom('named_table'),
		t_tuple([t_atom('heir'), t_pid(), t_any()]),
		t_tuple([t_atom('heir'), t_atom('none')]),
		t_tuple([t_atom('keypos'), t_integer()]),
		t_tuple([t_atom('write_concurrency'), t_boolean()])])).

t_ets_info_items() ->
  t_sup([t_atom('fixed'),
	 t_atom('safe_fixed'),
	 t_atom('keypos'),
	 t_atom('memory'),
	 t_atom('name'),
	 t_atom('named_table'),
	 t_atom('node'),
	 t_atom('owner'),
	 t_atom('protection'),
	 t_atom('size'),
	 t_atom('type')]).

%% =====================================================================
%% These are used for the built-in functions of 'file'
%% =====================================================================

t_file_io_device() ->
  t_sup(t_pid(), t_tuple([t_atom('file_descriptor'), t_atom(), t_any()])).

t_file_name() ->
  t_sup([t_atom(),
	 t_string(),
	 %% DeepList = [char() | atom() | DeepList] -- approximation below
	 t_list(t_sup([t_atom(), t_string(), t_list()]))]).

t_file_open_option() ->
  t_sup([t_atom('read'),
	 t_atom('write'),
	 t_atom('append'),
	 t_atom('raw'),
	 t_atom('binary'),
	 t_atom('delayed_write'),
	 t_atom('read_ahead'),
	 t_atom('compressed'),
	 t_tuple([t_atom('delayed_write'),
		  t_pos_integer(), t_non_neg_integer()]),
	 t_tuple([t_atom('read_ahead'), t_pos_integer()])]).

%% This lists all Posix errors that can occur in file:*/* functions
t_file_posix_error() ->
  t_sup([t_atom('eacces'),
	 t_atom('eagain'),
	 t_atom('ebadf'),
	 t_atom('ebusy'),
	 t_atom('edquot'),
	 t_atom('eexist'),
	 t_atom('efault'),
	 t_atom('efbig'),
	 t_atom('eintr'),
	 t_atom('einval'),
	 t_atom('eio'),
	 t_atom('eisdir'),
	 t_atom('eloop'),
	 t_atom('emfile'),
	 t_atom('emlink'),
	 t_atom('enametoolong'),
	 t_atom('enfile'),
	 t_atom('enodev'),
	 t_atom('enoent'),
	 t_atom('enomem'),
	 t_atom('enospc'),
	 t_atom('enotblk'),
	 t_atom('enotdir'),
	 t_atom('enotsup'),
	 t_atom('enxio'),
	 t_atom('eperm'),
	 t_atom('epipe'),
	 t_atom('erofs'),
	 t_atom('espipe'),
	 t_atom('esrch'),
	 t_atom('estale'),
	 t_atom('exdev')]).

t_file_return() ->
  t_sup(t_atom('ok'), t_tuple([t_atom('error'), t_file_posix_error()])).

%% =====================================================================
%% These are used for the built-in functions of 'gen_tcp'
%% =====================================================================

t_gen_tcp_accept() ->
  t_sup(t_tuple([t_atom('ok'), t_socket()]),
	t_tuple([t_atom('error'), t_sup([t_atom('closed'),
					 t_atom('timeout'),
					 t_inet_posix_error()])])).

t_gen_tcp_address() ->
  t_sup([t_string(), t_atom(), t_ip_address()]).

t_gen_tcp_port() ->
  t_from_range(0, 16#FFFF).

t_gen_tcp_connect_option() ->
  t_sup([t_atom('list'),
	 t_atom('binary'),
	 t_tuple([t_atom('ip'), t_ip_address()]),
	 t_tuple([t_atom('port'), t_gen_tcp_port()]),
	 t_tuple([t_atom('fd'), t_integer()]),
	 t_atom('inet6'),
	 t_atom('inet'),
	 t_inet_setoption()]).

t_gen_tcp_listen_option() ->
  t_sup([t_atom('list'),
	 t_atom('binary'),
	 t_tuple([t_atom('backlog'), t_non_neg_integer()]),
	 t_tuple([t_atom('ip'), t_ip_address()]),
	 t_tuple([t_atom('fd'), t_integer()]),
	 t_atom('inet6'),
	 t_atom('inet'),
	 t_inet_setoption()]).

t_gen_tcp_recv() ->
  t_sup(t_tuple([t_atom('ok'), t_packet()]),
	t_tuple([t_atom('error'), t_sup([t_atom('closed'),
					 t_inet_posix_error()])])).

%% =====================================================================
%% These are used for the built-in functions of 'gen_udp'
%% =====================================================================

t_gen_udp_connect_option() ->
  t_sup([t_atom('list'),
	 t_atom('binary'),
	 t_tuple([t_atom('ip'), t_ip_address()]),
	 t_tuple([t_atom('fd'), t_integer()]),
	 t_atom('inet6'),
	 t_atom('inet'),
	 t_inet_setoption()]).

t_gen_udp_recv() ->
  t_sup(t_tuple([t_atom('ok'),
		 t_tuple([t_ip_address(),
			  t_gen_tcp_port(),
			  t_packet()])]),
	t_tuple([t_atom('error'),
		 t_sup(t_atom('not_owner'), t_inet_posix_error())])).

%% =====================================================================
%% These are used for the built-in functions of 'hipe_bifs'
%% =====================================================================

t_trampoline() ->
  t_sup(t_nil(), t_integer()).

t_immediate() ->
  t_sup([t_nil(), t_atom(), t_fixnum()]).

t_immarray() ->
  t_integer().	%% abstract data type

t_hiperef() ->
  t_immarray().

t_bitarray() ->
  t_bitstr().

t_bytearray() ->
  t_binary().

t_insn_type() ->
  t_sup([% t_atom('call'),
	 t_atom('load_mfa'),
	 t_atom('x86_abs_pcrel'),
	 t_atom('atom'),
	 t_atom('constant'),
	 t_atom('c_const'),
	 t_atom('closure')]).

%% =====================================================================
%% These are used for the built-in functions of 'inet'
%% =====================================================================

t_inet_setoption() ->
  t_sup([%% first the 2-tuple options
	 t_tuple([t_atom('active'), t_sup(t_boolean(), t_atom('once'))]),
	 t_tuple([t_atom('broadcast'), t_boolean()]),
	 t_tuple([t_atom('delay_send'), t_boolean()]),
	 t_tuple([t_atom('dontroute'), t_boolean()]),
	 t_tuple([t_atom('exit_on_close'), t_boolean()]),
	 t_tuple([t_atom('header'), t_non_neg_integer()]),
	 t_tuple([t_atom('keepalive'), t_boolean()]),
	 t_tuple([t_atom('nodelay'), t_boolean()]),
	 t_tuple([t_atom('packet'), t_inet_setoption_packettype()]),
	 t_tuple([t_atom('packet_size'), t_non_neg_integer()]),
	 t_tuple([t_atom('read_packets'), t_non_neg_integer()]),
	 t_tuple([t_atom('recbuf'), t_non_neg_integer()]),
	 t_tuple([t_atom('reuseaddr'), t_boolean()]),
	 t_tuple([t_atom('send_timeout'), t_non_neg_integer()]),
	 t_tuple([t_atom('sndbuf'), t_non_neg_integer()]),
	 t_tuple([t_atom('priority'), t_non_neg_integer()]),
	 t_tuple([t_atom('tos'), t_non_neg_integer()]),
	 %% and a 4-tuple option
	 t_tuple([t_atom('raw'),
		  t_non_neg_integer(),	% protocol level
		  t_non_neg_integer(),	% option number
		  t_binary()])]).	% actual option value

t_inet_setoption_packettype() ->
  t_sup([t_atom('raw'),
	 t_integers([0,1,2,4]),
	 t_atom('asn1'), t_atom('cdr'), t_atom('sunrm'),
	 t_atom('fcgi'), t_atom('tpkt'), t_atom('line'),
	 t_atom('http'),
	 t_atom('http_bin')]).	%% but t_atom('httph') is not needed

t_inet_posix_error() ->
  t_atom().  %% XXX: Very underspecified

%% =====================================================================
%% These are used for the built-in functions of 'io'
%% =====================================================================

t_io_device() ->
  t_sup(t_atom(), t_pid()).

%% The documentation in R11B-4 reads
%%	Format ::= atom() | string() | binary()
%% but the Format can also be a (deep) list, hence the type below
t_io_format_string() ->
  t_sup([t_atom(), t_list(), t_binary()]).

%% =====================================================================
%% These are used for the built-in functions of 're'; the functions
%% whose last name component starts with a capital letter are types
%% =====================================================================

t_re_MP() ->  %% it's supposed to be an opaque data type
  t_tuple([t_atom('re_pattern'), t_integer(), t_integer(), t_binary()]).

t_re_RE() ->
  t_sup(t_re_MP(), t_iodata()).

t_re_compile_option() ->
  t_sup([t_atoms(['anchored', 'caseless', 'dollar_endonly', 'dotall',
		  'extended', 'firstline', 'multiline', 'no_auto_capture',
		  'dupnames', 'ungreedy']),
	 t_tuple([t_atom('newline'), t_re_NLSpec()])]).

t_re_run_option() ->
  t_sup([t_atoms(['anchored', 'global', 'notbol', 'noteol', 'notempty']),
	 t_tuple([t_atom('offset'), t_integer()]),
	 t_tuple([t_atom('newline'), t_re_NLSpec()]),
	 t_tuple([t_atom('capture'), t_re_ValueSpec()]),
	 t_tuple([t_atom('capture'), t_re_ValueSpec(), t_re_Type()]),
	 t_re_compile_option()]).

t_re_ErrorSpec() ->
  t_tuple([t_string(), t_non_neg_integer()]).

t_re_Type() ->
  t_atoms(['index', 'list', 'binary']).

t_re_NLSpec() ->
  t_atoms(['cr', 'crlf', 'lf', 'anycrlf']).

t_re_ValueSpec() ->
  t_sup(t_atoms(['all', 'all_but_first', 'first', 'none']), t_re_ValueList()).

t_re_ValueList() ->
  t_list(t_sup([t_integer(), t_string(), t_atom()])).

t_re_Captured() ->
  t_list(t_sup(t_re_CapturedData(), t_list(t_re_CapturedData()))).

t_re_CapturedData() ->
  t_sup([t_tuple([t_integer(), t_integer()]), t_string(), t_binary()]).

%% =====================================================================
%% These are used for the built-in functions of 'unicode'
%% =====================================================================

t_ML() ->     % a binary or a possibly deep list of integers or binaries
  t_sup(t_list(t_sup([t_integer(), t_binary(), t_list()])), t_binary()).

t_encoding() ->
  t_atoms(['latin1', 'unicode', 'utf8', 'utf16', 'utf32']).

t_encoding_a2b() -> % for the 2nd arg of atom_to_binary/2 and binary_to_atom/2
  t_atoms(['latin1', 'unicode', 'utf8']).

%% =====================================================================
%% Some testing code for ranges below
%% =====================================================================

-ifdef(DO_ERL_BIF_TYPES_TEST).

test() ->
  put(hipe_target_arch, amd64),

  Bsl1 = type(erlang, 'bsl', 2, [t_from_range(1, 299), t_from_range(-4, 22)]),
  Bsl2 = type(erlang, 'bsl', 2),
  Bsl3 = type(erlang, 'bsl', 2, [t_from_range(1, 299), t_atom('pelle')]),
  io:format("Bsl ~p ~p ~p~n", [Bsl1, Bsl2, Bsl3]),

  Add1 = type(erlang, '+', 2, [t_from_range(1, 299), t_from_range(-4, 22)]),
  Add2 = type(erlang, '+', 2),
  Add3 = type(erlang, '+', 2, [t_from_range(1, 299), t_atom('pelle')]),
  io:format("Add ~p ~p ~p~n", [Add1, Add2, Add3]),

  Band1 = type(erlang, 'band', 2, [t_from_range(1, 29), t_from_range(34, 36)]),
  Band2 = type(erlang, 'band', 2),
  Band3 = type(erlang, 'band', 2, [t_from_range(1, 299), t_atom('pelle')]),
  io:format("band ~p ~p ~p~n", [Band1, Band2, Band3]),

  Bor1 = type(erlang, 'bor', 2, [t_from_range(1, 29), t_from_range(8, 11)]),
  Bor2 = type(erlang, 'bor', 2),
  Bor3 = type(erlang, 'bor', 2, [t_from_range(1, 299), t_atom('pelle')]),
  io:format("bor ~p ~p ~p~n", [Bor1, Bor2, Bor3]),
  
  io:format("inf_?"),
  pos_inf = infinity_max([1, 4, 51, pos_inf]),
  -12 = infinity_min([1, 142, -4, -12]),
  neg_inf = infinity_max([neg_inf]),

  io:format("width"),
  4 = width({7, 9}),
  pos_inf = width({neg_inf, 100}),
  pos_inf = width({1, pos_inf}),
  3 = width({-8, 7}),
  0 = width({-1, 0}),

  io:format("arith * "),
  Mult1 = t_from_range(0, 12),
  Mult2 = t_from_range(-21, 7),
  Mult1 = type(erlang, '*', 2, [t_from_range(2,3), t_from_range(0,4)]),
  Mult2 = type(erlang, '*', 2, [t_from_range(-7,-1), t_from_range(-1,3)]),
  ok.

-endif.
