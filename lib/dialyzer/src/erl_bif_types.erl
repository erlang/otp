%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2004-2010 held by the authors. All Rights Reserved.
%% Copyright Ericsson AB 2020-2025. All Rights Reserved.
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
%% @doc Type information for Erlang Built-in functions (implemented in C)
%% @copyright 2002 Richard Carlsson, 2006 Richard Carlsson, Tobias Lindahl
%% and Kostis Sagonas
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @author Tobias Lindahl <tobias.lindahl@gmail.com>
%% @author Kostis Sagonas <kostis@it.uu.se>

-module(erl_bif_types).
-moduledoc false.

-define(BITS, 128). %This is only in bsl to convert answer to pos_inf/neg_inf.
-export([type/3, type/4, arg_types/3,
	 is_known/3, infinity_add/2]).

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
		    t_cons/0,
		    t_cons/2,
		    t_cons_hd/1,
		    t_cons_tl/1,
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
		    t_is_any/1,
		    t_is_atom/1,
		    t_is_binary/1,
		    t_is_bitstr/1,
		    t_is_boolean/1,
		    t_is_cons/1,
		    t_is_float/1,
		    t_is_fun/1,
		    t_is_impossible/1,
		    t_is_integer/1,
		    t_is_nil/1,
		    t_is_none/1,
		    t_is_number/1,
		    t_is_pid/1,
		    t_is_port/1,
		    t_is_maybe_improper_list/1,
		    t_is_reference/1,
		    t_is_subtype/2,
		    t_is_tuple/1,
		    t_list/0,
		    t_list/1,
		    t_list_elements/1,
		    t_list_termination/1,
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
		    t_tuple/0,
		    t_tuple/1,
		    t_tuple_args/1,
		    t_tuple_size/1,
		    t_tuple_subtypes/1,
		    t_is_map/1,
		    t_map/0,
		    t_map/3,
		    t_map_def_key/1,
		    t_map_def_val/1,
		    t_map_get/2,
		    t_map_is_key/2,
		    t_map_entries/1,
		    t_map_put/2,
		    t_map_remove/2,
		    t_map_update/2,
		    t_map_pairwise_merge/3,
                    t_inf_lists/2
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

%%-- erlang -------------------------------------------------------------------
type(erlang, halt, 0, _) -> t_none();
type(erlang, halt, 1, _) -> t_none();
type(erlang, halt, 2, _) -> t_none();
type(erlang, exit, 1, _) -> t_none();
type(erlang, error, 1, _) -> t_none();
type(erlang, error, 2, _) -> t_none();
type(erlang, error, 3, _) -> t_none();
type(erlang, throw, 1, _) -> t_none();
type(erlang, '==', 2, Xs = [X1, X2]) ->
  case
    t_is_atom(X1) andalso t_is_atom(X2)
  of
    true -> type(erlang, '=:=', 2, Xs);
    false ->
      case t_is_integer(X1) andalso t_is_integer(X2) of
	true -> type(erlang, '=:=', 2, Xs);
	false -> strict2(Xs, t_boolean())
      end
  end;
type(erlang, '/=', 2, Xs = [X1, X2]) ->
  case
    t_is_atom(X1) andalso t_is_atom(X2)
  of
    true -> type(erlang, '=/=', 2, Xs);
    false ->
      case t_is_integer(X1) andalso t_is_integer(X2) of
	true -> type(erlang, '=/=', 2, Xs);
	false -> strict2(Xs, t_boolean())
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
	    case
              t_is_integer(Lhs) andalso t_is_integer(Rhs)
            of
	      false -> t_boolean();
	      true ->
		case
                  {t_number_vals(Lhs), t_number_vals(Rhs)}
                of
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
  strict2(Xs, Ans);
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
	    case
              t_is_integer(Lhs) andalso t_is_integer(Rhs)
            of
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
  strict2(Xs, Ans);
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
      false -> compare('>', Lhs, Rhs)
    end,
  strict2(Xs, Ans);
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
      false -> compare('>=', Lhs, Rhs)
    end,
  strict2(Xs, Ans);
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
      false -> compare('<', Lhs, Rhs)
    end,
  strict2(Xs, Ans);
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
      false -> compare('=<', Lhs, Rhs)
    end,
  strict2(Xs, Ans);
type(erlang, '+', 1, Xs) ->
  strict(erlang, '+', 1, Xs, fun ([X]) -> X end);
type(erlang, '-', 1, Xs) ->
  strict(erlang, '-', 1, Xs,
	 fun ([X]) ->
	     case t_is_integer(X) of
	       true -> type(erlang, '-', 2, [t_integer(0), X]);
	       false -> X
	     end
	 end);
type(erlang, '!', 2, Xs) ->
  strict(erlang, '!', 2, Xs, fun ([_, X2]) -> X2 end);
type(erlang, '+', 2, Xs) ->
  strict(erlang, '+', 2, Xs,
	 fun ([X1, X2]) ->
	     case arith('+', X1, X2) of
	       {ok, T} -> T;
	       error ->
		 case
                   t_is_float(X1) orelse t_is_float(X2)
                 of
		   true -> t_float();
		   false -> t_number()
		 end
	     end
	 end);
type(erlang, '-', 2, Xs) ->
  strict(erlang, '-', 2, Xs,
	 fun ([X1, X2]) ->
	     case arith('-', X1, X2) of
	       {ok, T} -> T;
	       error ->
		 case
                   t_is_float(X1) orelse t_is_float(X2)
                 of
		   true -> t_float();
		   false -> t_number()
		 end
	     end
	 end);
type(erlang, '*', 2, Xs) ->
  strict(erlang, '*', 2, Xs,
	 fun ([X1, X2]) ->
	     case arith('*', X1, X2) of
	       {ok, T} -> T;
	       error ->
		 case
                   t_is_float(X1) orelse t_is_float(X2)
                 of
		   true -> t_float();
		   false -> t_number()
		 end
	     end
	 end);
type(erlang, '/', 2, Xs) ->
  strict(erlang, '/', 2, Xs, fun (_) -> t_float() end);
type(erlang, 'div', 2, Xs) ->
  strict(erlang, 'div', 2, Xs,
	 fun ([X1, X2]) ->
	     case arith('div', X1, X2) of
	       error -> t_integer();
	       {ok, T} -> T
	     end
	 end);
type(erlang, 'rem', 2, Xs) ->
  strict(erlang, 'rem', 2, Xs,
	 fun ([X1, X2]) ->
	     case arith('rem', X1, X2) of
	       error -> t_non_neg_integer();
	       {ok, T} -> T
	     end
	 end);
type(erlang, '++', 2, Xs) ->
  strict(erlang, '++', 2, Xs,
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
  strict(erlang, '--', 2, Xs,
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
  strict(erlang, 'and', 2, Xs, fun (_) -> t_boolean() end);
type(erlang, 'or', 2, Xs) ->
  strict(erlang, 'or', 2, Xs, fun (_) -> t_boolean() end);
type(erlang, 'xor', 2, Xs) ->
  strict(erlang, 'xor', 2, Xs, fun (_) -> t_boolean() end);
type(erlang, 'not', 1, Xs) ->
  strict(erlang, 'not', 1, Xs, fun (_) -> t_boolean() end);
type(erlang, 'band', 2, Xs) ->
  strict(erlang, 'band', 2, Xs,
	 fun ([X1, X2]) ->
	     case arith('band', X1, X2) of
	       error -> t_integer();
	       {ok, T} -> T
	     end
	 end);
%% The result is not wider than the smallest argument. We need to
%% kill any value-sets in the result.
%%  strict(erlang, 'band', 2, Xs,
%%	 fun ([X1, X2]) -> t_sup(t_inf(X1, X2), t_byte()) end);
type(erlang, 'bor', 2, Xs) ->
  strict(erlang, 'bor', 2, Xs,
	 fun ([X1, X2]) ->
	     case arith('bor', X1, X2) of
	       error -> t_integer();
	       {ok, T} -> T
	     end
	 end);
%% The result is not wider than the largest argument. We need to
%% kill any value-sets in the result.
%%  strict(erlang, 'bor', 2, Xs,
%%	 fun ([X1, X2]) -> t_sup(t_sup(X1, X2), t_byte()) end);
type(erlang, 'bxor', 2, Xs) ->
  strict(erlang, 'bxor', 2, Xs,
	 fun ([X1, X2]) ->
	     case arith('bxor', X1, X2) of
	       error -> t_integer();
	       {ok, T} -> T
	     end
	 end);
%% The result is not wider than the largest argument. We need to
%% kill any value-sets in the result.
%%  strict(erlang, 'bxor', 2, Xs,
%%	 fun ([X1, X2]) -> t_sup(t_sup(X1, X2), t_byte()) end);
type(erlang, 'bsr', 2, Xs) ->
  strict(erlang, 'bsr', 2, Xs,
	 fun ([X1, X2]) ->
	     case arith('bsr', X1, X2) of
	       error -> t_integer();
	       {ok, T} -> T
	     end
	 end);
%% If the first argument is unsigned (which is the case for
%% characters and bytes), the result is never wider. We need to kill
%% any value-sets in the result.
%%  strict(erlang, 'bsr', 2, Xs,
%%	 fun ([X, _]) -> t_sup(X, t_byte()) end);
type(erlang, 'bsl', 2, Xs) ->
  strict(erlang, 'bsl', 2, Xs,
	 fun ([X1, X2]) ->
	     case arith('bsl', X1, X2) of
	       error -> t_integer();
	       {ok, T} -> T
	     end
	 end);
%% Not worth doing anything special here.
%%  strict(erlang, 'bsl', 2, Xs, fun (_) -> t_integer() end);
type(erlang, 'bnot', 1, Xs) ->
 strict(erlang, 'bnot', 1, Xs,
	 fun ([X1]) ->
	     case arith_bnot(X1) of
	       error -> t_integer();
	       {ok, T} -> T
	     end
	 end);
%% Guard bif, needs to be here.
type(erlang, abs, 1, Xs) ->
  strict(erlang, abs, 1, Xs,
         fun ([X1]) -> arith_abs(X1) end);
%% This returns (-X)-1, so it often gives a negative result.
%%  strict(erlang, 'bnot', 1, Xs, fun (_) -> t_integer() end);
type(erlang, append, 2, Xs) -> type(erlang, '++', 2, Xs); % alias
type(erlang, apply, 2, Xs) ->
  Fun = fun ([X, _Y]) ->
	    case t_is_fun(X) of
	      true ->
		t_fun_range(X);
	      false ->
		t_any()
	    end
	end,
  strict(erlang, apply, 2, Xs, Fun);
type(erlang, apply, 3, Xs) ->
  strict(erlang, apply, 3, Xs, fun (_) -> t_any() end);
%% Guard bif, needs to be here.
type(erlang, binary_part, 2, Xs) ->
  strict(erlang, binary_part, 2, Xs, fun (_) -> t_binary() end);
%% Guard bif, needs to be here.
type(erlang, binary_part, 3, Xs) ->
  strict(erlang, binary_part, 3, Xs, fun (_) -> t_binary() end);
%% Guard bif, needs to be here.
type(erlang, bit_size, 1, Xs) ->
  strict(erlang, bit_size, 1, Xs,
	 fun (_) -> t_non_neg_integer() end);
%% Guard bif, needs to be here.
type(erlang, byte_size, 1, Xs) ->
  strict(erlang, byte_size, 1, Xs,
	 fun (_) -> t_non_neg_integer() end);
%% Guard bif, needs to be here.
type(erlang, ceil, 1, Xs) ->
  strict(erlang, ceil, 1, Xs, fun (_) -> t_integer() end);
%% Guard bif, needs to be here.
%% Also much more expressive than anything you could write in a spec...
type(erlang, element, 2, Xs) ->
  strict(erlang, element, 2, Xs,
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
%% Guard bif, needs to be here.
type(erlang, float, 1, Xs) ->
  strict(erlang, float, 1, Xs, fun (_) -> t_float() end);
%% Guard bif, needs to be here.
type(erlang, floor, 1, Xs) ->
  strict(erlang, floor, 1, Xs, fun (_) -> t_integer() end);
%% Primop, needs to be somewhere.
type(erlang, build_stacktrace, 0, _) ->
  t_list(t_tuple([t_module(),
                  t_atom(),
                  t_sup([t_arity(),t_list()]),
                  t_list(t_sup([t_tuple([t_atom('error_info'),t_map()]),
                                t_tuple([t_atom('file'),t_string()]),
                                t_tuple([t_atom('line'),t_pos_integer()])]))]));
%% Guard bif, needs to be here.
type(erlang, hd, 1, Xs) ->
  strict(erlang, hd, 1, Xs, fun ([X]) -> t_cons_hd(X) end);
type(erlang, info, 1, Xs) -> type(erlang, system_info, 1, Xs); % alias
%% All type tests are guard BIF's and may be implemented in ways that
%% cannot be expressed in a type spec, why they are kept in erl_bif_types.
type(erlang, is_atom, 1, Xs) ->
  Fun = fun (X) ->
            check_guard(X, fun (Y) -> t_is_atom(Y) end,
                        t_atom())
        end,
  strict(erlang, is_atom, 1, Xs, Fun);
type(erlang, is_binary, 1, Xs) ->
  Fun = fun (X) ->
	    check_guard(X, fun (Y) -> t_is_binary(Y) end,
                        t_binary())
	end,
  strict(erlang, is_binary, 1, Xs, Fun);
type(erlang, is_bitstring, 1, Xs) ->
  Fun = fun (X) ->
	    check_guard(X, fun (Y) -> t_is_bitstr(Y) end,
                        t_bitstr())
	end,
  strict(erlang, is_bitstring, 1, Xs, Fun);
type(erlang, is_boolean, 1, Xs) ->
  Fun = fun (X) ->
	    check_guard(X, fun (Y) -> t_is_boolean(Y) end,
                        t_boolean())
	end,
  strict(erlang, is_boolean, 1, Xs, Fun);
type(erlang, is_float, 1, Xs) ->
  Fun = fun (X) ->
	    check_guard(X, fun (Y) -> t_is_float(Y) end,
                        t_float())
	end,
  strict(erlang, is_float, 1, Xs, Fun);
type(erlang, is_function, 1, Xs) ->
  Fun = fun (X) ->
            check_guard(X, fun (Y) -> t_is_fun(Y) end,
                        t_fun())
        end,
  strict(erlang, is_function, 1, Xs, Fun);
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
  strict(erlang, is_function, 2, Xs, Fun);
type(erlang, is_integer, 1, Xs) ->
  Fun = fun (X) ->
	    check_guard(X, fun (Y) -> t_is_integer(Y) end,
                        t_integer())
	end,
  strict(erlang, is_integer, 1, Xs, Fun);
type(erlang, is_list, 1, Xs) ->
  Fun = fun (X) ->
	    Fun2 = fun (Y) -> t_is_maybe_improper_list(Y) end,
	    check_guard(X, Fun2, t_maybe_improper_list())
	end,
  strict(erlang, is_list, 1, Xs, Fun);
type(erlang, is_map, 1, Xs) ->
  Fun = fun (X) ->
	    check_guard(X, fun (Y) -> t_is_map(Y) end,
	    t_map()) end,
  strict(erlang, is_map, 1, Xs, Fun);
type(erlang, is_map_key, 2, Xs) ->
  type(maps, is_key, 2, Xs);
type(erlang, is_number, 1, Xs) ->
  Fun = fun (X) ->
	    check_guard(X, fun (Y) -> t_is_number(Y) end,
                        t_number())
	end,
  strict(erlang, is_number, 1, Xs, Fun);
type(erlang, is_pid, 1, Xs) ->
  Fun = fun (X) ->
            check_guard(X, fun (Y) -> t_is_pid(Y) end,
                        t_pid())
        end,
  strict(erlang, is_pid, 1, Xs, Fun);
type(erlang, is_port, 1, Xs) ->
  Fun = fun (X) ->
            check_guard(X, fun (Y) -> t_is_port(Y) end,
                        t_port())
        end,
  strict(erlang, is_port, 1, Xs, Fun);
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
		      [Tag|_] -> check_record_tag(Tag, Y)
		    end;
		  List when length(List) >= 2 ->
		    t_sup([type(erlang, is_record, 2, [T, Y]) || T <- List])
		end
	    end
	end,
  strict(erlang, is_record, 2, Xs, Fun);
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
                        check_record_tag(hd(Args), Y);
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
  strict(erlang, is_record, 3, Xs, Fun);
type(erlang, is_reference, 1, Xs) ->
  Fun = fun (X) ->
	    check_guard(X, fun (Y) -> t_is_reference(Y) end,
                        t_reference())
	end,
  strict(erlang, is_reference, 1, Xs, Fun);
type(erlang, is_tuple, 1, Xs) ->
  Fun = fun (X) ->
	    check_guard(X, fun (Y) -> t_is_tuple(Y) end,
                        t_tuple())
	end,
  strict(erlang, is_tuple, 1, Xs, Fun);
%% Guard bif, needs to be here.
type(erlang, length, 1, Xs) ->
  strict(erlang, length, 1, Xs, fun (_) -> t_non_neg_fixnum() end);
%% Guard bif, needs to be here.
type(erlang, map_size, 1, Xs) ->
  type(maps, size, 1, Xs);
type(erlang, max, 2, Xs) ->
  strict(erlang, max, 2, Xs, fun([A, B]) -> t_sup(A, B) end);
%% Guard bif, needs to be here.
type(erlang, map_get, 2, Xs) ->
  type(maps, get, 2, Xs);
type(erlang, make_fun, 3, Xs) ->
  strict(erlang, make_fun, 3, Xs,
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
type(erlang, make_tuple, 2, Xs) ->
  strict(erlang, make_tuple, 2, Xs,
	 fun ([Int, _]) ->
	     case t_number_vals(Int) of
	       [N] when is_integer(N), N >= 0 -> t_tuple(N);
	       _Other -> t_tuple()
	     end
	 end);
type(erlang, make_tuple, 3, Xs) ->
  strict(erlang, make_tuple, 3, Xs,
	 fun ([Int, _, _]) ->
	     case t_number_vals(Int) of
	       [N] when is_integer(N), N >= 0 -> t_tuple(N);
	       _Other -> t_tuple()
	     end
	 end);
type(erlang, min, 2, Xs) ->
  strict(erlang, min, 2, Xs, fun([A, B]) -> t_sup(A, B) end);
type(erlang, nif_error, 1, Xs) ->
  %% this BIF and the next one are stubs for NIFs and never return
  strict(erlang, nif_error, 1, Xs, fun (_) -> t_any() end);
type(erlang, nif_error, 2, Xs) ->
  strict(erlang, nif_error, 2, Xs, fun (_) -> t_any() end);
%% Guard bif, needs to be here.
type(erlang, node, 0, _) -> t_node();
%% Guard bif, needs to be here.
type(erlang, node, 1, Xs) ->
  strict(erlang, node, 1, Xs, fun (_) -> t_node() end);
type(erlang, raise, 3, Xs) ->
  Ts = arg_types(erlang, raise, 3),
  Xs1 = t_inf_lists(Xs, Ts),
  case any_is_none_or_unit(Xs1) of
    true -> t_atom('badarg');
    false -> t_none()
  end;
%% Guard bif, needs to be here.
type(erlang, round, 1, Xs) ->
  strict(erlang, round, 1, Xs, fun (_) -> t_integer() end);
%% Guard bif, needs to be here.
type(erlang, self, 0, _) -> t_pid();
type(erlang, setelement, 3, Xs) ->
  strict(erlang, setelement, 3, Xs,
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
%% Guard bif, needs to be here.
type(erlang, size, 1, Xs) ->
  strict(erlang, size, 1, Xs, fun (_) -> t_non_neg_integer() end);
type(erlang, subtract, 2, Xs) -> type(erlang, '--', 2, Xs); % alias
type(erlang, system_info, 1, Xs) ->
  strict(erlang, system_info, 1, Xs,
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
		     t_list(t_tuple([t_atom(), t_sup([t_pid(), t_port()])]));
		   ['endian'] ->
		     t_endian();
		   ['fullsweep_after'] ->
		     t_tuple([t_atom('fullsweep_after'), t_non_neg_integer()]);
		   ['garbage_collection'] ->
		     t_list();
		   ['heap_sizes'] ->
		     t_list(t_integer());
		   ['heap_type'] ->
		     t_atom('private');
		   ['info'] ->
		     t_binary();
		   ['internal_cpu_topology'] -> %% Undocumented internal feature
		     t_internal_cpu_topology();
		   ['loaded'] ->
		     t_binary();
		   [P] when P == 'logical_processors'
		        orelse P == 'logical_processors_available'
		        orelse P == 'logical_processors_online' ->
		     t_sup([t_non_neg_fixnum(),t_atom('unknown')]);
		   ['machine'] ->
		     t_string();
		   ['multi_scheduling'] ->
		     t_system_multi_scheduling();
		   ['multi_scheduling_blockers'] ->
		     t_list(t_pid());
		   ['os_type'] ->
		     t_tuple([t_sup([t_atom('unix'),
				     t_atom('win32')]),
			      t_atom()]);
		   ['os_version'] ->
		     t_sup(t_tuple([t_non_neg_fixnum(),
				    t_non_neg_fixnum(),
				    t_non_neg_fixnum()]),
			   t_string());
                   ['otp_release'] ->
                     t_string();
		   ['port_parallelism'] ->
		     t_boolean();
		   ['port_count'] ->
		     t_non_neg_fixnum();
		   ['port_limit'] ->
		     t_non_neg_fixnum();
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
%% Guard bif, needs to be here.
type(erlang, tl, 1, Xs) ->
  strict(erlang, tl, 1, Xs, fun ([X]) -> t_cons_tl(X) end);
%% Guard bif, needs to be here.
type(erlang, trunc, 1, Xs) ->
  strict(erlang, trunc, 1, Xs, fun (_) -> t_integer() end);
%% Guard bif, needs to be here.
type(erlang, tuple_size, 1, Xs) ->
  strict(erlang, tuple_size, 1, Xs,
         fun (_) -> t_non_neg_integer() end);
type(erlang, tuple_to_list, 1, Xs) ->
  strict(erlang, tuple_to_list, 1, Xs,
	 fun ([X]) ->
	     case t_tuple_subtypes(X) of
	       unknown -> t_list();
	       SubTypes ->
                 Args = lists:append([t_tuple_args(ST) ||
                                       ST <- SubTypes]),
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
%%-- lists --------------------------------------------------------------------
type(lists, all, 2, Xs) ->
  strict(lists, all, 2, Xs,
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
  strict(lists, any, 2, Xs,
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
  strict(lists, delete, 2, Xs,
	 fun ([_, List]) ->
	     case t_is_cons(List) of
	       true -> t_cons_tl(List);
	       false -> List
	     end
	 end);
type(lists, dropwhile, 2, Xs) ->
  strict(lists, dropwhile, 2, Xs,
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
  strict(lists, filter, 2, Xs,
	 fun ([F, L]) ->
	     case t_is_nil(L) of
	       true -> t_nil();
	       false ->
		 T = t_list_elements(L),
		 case check_fun_application(F, [T]) of
		   ok ->
                     RangeVals = t_atom_vals(t_fun_range(F)),
		     case RangeVals =:= ['false'] of
		       true -> t_nil();
		       false ->
			 case RangeVals =:= ['true'] of
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
  strict(lists, flatten, 1, Xs,
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
		     t_sup(t_list(t_subtract(X1, t_list())), X2)
		 end
	     end
	 end);
type(lists, flatmap, 2, Xs) ->
  strict(lists, flatmap, 2, Xs,
	 fun ([F, List]) ->
	     case t_is_nil(List) of
	       true -> t_nil();
	       false ->
		 case
                   check_fun_application(F, [t_list_elements(List)])
                 of
		   ok ->
		     R = t_fun_range(F),
		     case t_is_nil(R) of
		       true -> t_nil();
		       false ->
			 Elems = t_list_elements(R),
			 case t_is_cons(List) of
			   true ->
			     case t_is_subtype(t_nil(), R) of
			       true -> t_list(Elems);
			       false -> t_nonempty_list(Elems)
			     end;
			   false -> t_list(Elems)
			 end
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
  strict(lists, foreach, 2, Xs,
	 fun ([F, List]) ->
	     case t_is_cons(List) of
	       true ->
		 case
                   check_fun_application(F, [t_list_elements(List)])
                 of
		   ok -> t_atom('ok');
		   error -> t_none()
		 end;
	       false ->
		 t_atom('ok')
	     end
	 end);
type(lists, foldl, 3, Xs) ->
  strict(lists, foldl, 3, Xs,
	 fun ([F, Acc, List]) ->
	     case t_is_nil(List) of
	       true -> Acc;
	       false ->
		 case
                   check_fun_application(F,
                                         [t_list_elements(List),Acc])
                 of
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
type(lists, foldr, 3, Xs) -> type(lists, foldl, 3, Xs);  % same
type(lists, keydelete, 3, Xs) ->
  strict(lists, keydelete, 3, Xs,
	 fun ([_, _, L]) ->
	     Term = t_list_termination(L),
	     t_sup(Term, erl_types:lift_list_to_pos_empty(L))
	 end);
type(lists, keyfind, 3, Xs) ->
  strict(lists, keyfind, 3, Xs,
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
			 case key_comparisons_fail(X, Y, List) of
			   true -> t_atom('false');
			   false -> Ret
			 end
		     end
		 end
	     end
	 end);
type(lists, keymap, 3, Xs) ->
  strict(lists, keymap, 3, Xs,
	 fun ([F, _I, L]) ->
	     case t_is_nil(L) of
	       true -> L;
	       false -> t_list(t_sup(t_fun_range(F),
                                     t_list_elements(L)))
	     end
	 end);
type(lists, keymember, 3, Xs) ->
  strict(lists, keymember, 3, Xs,
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
			 case key_comparisons_fail(X, Y, List) of
			   true -> t_atom('false');
			   false -> t_boolean()
			 end
		     end
		 end
	     end
	 end);
type(lists, keymerge, 3, Xs) ->
  strict(lists, keymerge, 3, Xs,
	 fun ([_I, L1, L2]) -> type(lists, merge, 2, [L1, L2]) end);
type(lists, keyreplace, 4, Xs) ->
  strict(lists, keyreplace, 4, Xs,
	 fun ([_K, _I, L, T]) ->
             t_list(t_sup(t_list_elements(L), T))
         end);
type(lists, keysearch, 3, Xs) ->
  strict(lists, keysearch, 3, Xs,
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
			 case key_comparisons_fail(X, Y, List) of
			   true -> t_atom('false');
			   false -> Ret
			 end
		     end
		 end
	     end
	 end);
type(lists, keysort, 2, Xs) ->
  strict(lists, keysort, 2, Xs, fun ([_, L]) -> L end);
type(lists, last, 1, Xs) ->
  strict(lists, last, 1, Xs,
         fun ([L]) -> t_list_elements(L) end);
type(lists, map, 2, Xs) ->
  strict(lists, map, 2, Xs,
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
  strict(lists, mapfoldl, 3, Xs,
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
type(lists, mapfoldr, 3, Xs) -> type(lists, mapfoldl, 3, Xs); % same
type(lists, max, 1, Xs) ->
  strict(lists, max, 1, Xs,
         fun ([L]) -> t_list_elements(L) end);
type(lists, member, 2, Xs) ->
  strict(lists, member, 2, Xs,
	 fun ([X, Y]) ->
	     Y1 = t_list_elements(Y),
	     case t_is_none(t_inf(Y1, X)) of
	       true -> t_atom('false');
	       false -> t_boolean()
	     end
	 end);
%% type(lists, merge, 1, Xs) ->
type(lists, merge, 2, Xs) ->
  strict(lists, merge, 2, Xs,
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
type(lists, min, 1, Xs) ->
  strict(lists, min, 1, Xs,
         fun ([L]) -> t_list_elements(L) end);
type(lists, nth, 2, Xs) ->
  strict(lists, nth, 2, Xs,
	 fun ([_, Y]) -> t_list_elements(Y) end);
type(lists, nthtail, 2, Xs) ->
  strict(lists, nthtail, 2, Xs,
	 fun ([_, Y]) -> t_sup(Y, t_list()) end);
type(lists, partition, 2, Xs) ->
  strict(lists, partition, 2, Xs,
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
  strict(lists, reverse, 1, Xs, fun ([X]) -> X end);
type(lists, reverse, 2, Xs) ->
  type(erlang, '++', 2, Xs);    % reverse-onto is just like append
type(lists, sort, 1, Xs) ->
  strict(lists, sort, 1, Xs, fun ([X]) -> X end);
type(lists, sort, 2, Xs) ->
  strict(lists, sort, 2, Xs,
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
  strict(lists, split, 2, Xs,
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
  strict(lists, takewhile, 2, Xs,
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
  strict(lists, unzip, 1, Xs,
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
  strict(lists, unzip3, 1, Xs,
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
  strict(lists, zip, 2, Xs,
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
  strict(lists, zip3, 3, Xs,
	 fun ([As, Bs, Cs]) ->
	     case
               (t_is_nil(As)
                orelse t_is_nil(Bs)
                orelse t_is_nil(Cs))
             of
	       true -> t_nil();
	       false ->
		 A = t_list_elements(As),
		 B = t_list_elements(Bs),
		 C = t_list_elements(Cs),
		 t_list(t_tuple([A, B, C]))
	     end
	 end);
type(lists, zipwith, 3, Xs) ->
  strict(lists, zipwith, 3, Xs,
	 fun ([F, _As, _Bs]) -> t_sup(t_list(t_fun_range(F)),
                                      t_nil()) end);
type(lists, zipwith3, 4, Xs) ->
  strict(lists, zipwith3, 4, Xs,
	 fun ([F,_As,_Bs,_Cs]) -> t_sup(t_list(t_fun_range(F)),
                                        t_nil()) end);

%%-- maps ---------------------------------------------------------------------
type(maps, from_keys, 2, Xs) ->
  strict(maps, from_keys, 2, Xs,
	 fun ([List, Value]) ->
	     case t_is_nil(List) of
	       true -> t_from_term(#{});
	       false -> t_map([], t_list_elements(List), Value)
	     end
	 end);
type(maps, from_list, 1, Xs) ->
  strict(maps, from_list, 1, Xs,
	 fun ([List]) ->
	     case t_is_nil(List) of
	       true -> t_from_term(#{});
	       false ->
		 T = t_list_elements(List),
		 case t_tuple_subtypes(T) of
		   unknown -> t_map();
		   Stypes when length(Stypes) >= 1 ->
		     t_sup([begin
			      [K, V] = t_tuple_args(Args),
			      t_map([], K, V)
			    end || Args <- Stypes])
		 end
	     end
	 end);
type(maps, get, 2, Xs) ->
  strict(maps, get, 2, Xs,
	 fun ([Key, Map]) ->
	     t_map_get(Key, Map)
	 end);
type(maps, is_key, 2, Xs) ->
  strict(maps, is_key, 2, Xs,
	 fun ([Key, Map]) ->
	     t_map_is_key(Key, Map)
	 end);
type(maps, merge, 2, Xs) ->
  strict(maps, merge, 2, Xs,
	 fun ([MapA, MapB]) ->
	     ADefK = t_map_def_key(MapA),
	     BDefK = t_map_def_key(MapB),
	     ADefV = t_map_def_val(MapA),
	     BDefV = t_map_def_val(MapB),
	     t_map(t_map_pairwise_merge(
		     fun(K, _,     _,  mandatory, V) -> {K, mandatory, V};
			(K, MNess, VA, optional, VB) -> {K, MNess, t_sup(VA,VB)}
		     end, MapA, MapB),
		   t_sup(ADefK, BDefK), t_sup(ADefV, BDefV))
	 end);
type(maps, put, 3, Xs) ->
  strict(maps, put, 3, Xs,
	 fun ([Key, Value, Map]) ->
	     t_map_put({Key, Value}, Map)
	 end);
type(maps, remove, 2, Xs) ->
  strict(maps, remove, 2, Xs,
         fun ([Key, Map]) ->
             t_map_remove(Key, Map)
         end);
type(maps, size, 1, Xs) ->
  strict(maps, size, 1, Xs,
	 fun ([Map]) ->
	     Mand = [E || E={_,mandatory,_} <- t_map_entries(Map)],
	     LowerBound = length(Mand),
	     case t_is_none(t_map_def_key(Map)) of
	       false -> t_from_range(LowerBound, pos_inf);
	       true ->
		 Opt = [E || E={_,optional,_} <- t_map_entries(Map)],
		 UpperBound = LowerBound + length(Opt),
		 t_from_range(LowerBound, UpperBound)
	     end
	 end);
type(maps, update, 3, Xs) ->
  strict(maps, update, 3, Xs,
	 fun ([Key, Value, Map]) ->
	     t_map_update({Key, Value}, Map)
	 end);

%%-----------------------------------------------------------------------------
type(M, F, A, Xs) when is_atom(M), is_atom(F),
		       is_integer(A), 0 =< A, A =< 255 ->
  strict(Xs, t_any()).  % safe approximation for all functions.


%%-----------------------------------------------------------------------------
%% Auxiliary functions
%%-----------------------------------------------------------------------------

strict(M, F, A, Xs, Fun) ->
  Ts = arg_types(M, F, A),
  Xs1 = t_inf_lists(Xs, Ts),
  case any_is_none_or_unit(Xs1) of
    true -> t_none();
    false -> Fun(Xs1)
  end.

strict2(Xs, X) ->
  case any_is_none_or_unit(Xs) of
    true -> t_none();
    false -> X
  end.

strict(Xs, X) ->
  case any_is_none_or_unit(Xs) of
    true -> t_none();
    false -> X
  end.

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
  lists:any(fun erl_types:t_is_impossible/1, Ts).

check_guard([X], Test, Type) ->
  check_guard_single(X, Test, Type).

check_guard_single(X, Test, Type) ->
  case Test(X) of
    true -> t_atom('true');
    false ->
      case t_is_none(t_inf(Type, X)) of
        true -> t_atom('false');
        false -> t_boolean()
      end
  end.

check_record_tag(Tag, Y) ->
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
infinity_bsl(0, pos_inf) -> 0;
infinity_bsl(Number, pos_inf) when is_integer(Number), Number > 0 -> pos_inf;
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
  try Number1 + Number2
  catch
    error:system_limit when Number1 < 0 -> neg_inf;
    error:system_limit -> pos_inf
  end.

infinity_mult(neg_inf, Number) ->
  Greater = infinity_geq(Number, 0),
  if Greater -> neg_inf;
     true -> pos_inf
  end;
infinity_mult(pos_inf, Number) -> infinity_inv(infinity_mult(neg_inf, Number));
infinity_mult(Number, pos_inf) -> infinity_inv(infinity_mult(neg_inf, Number));
infinity_mult(Number, neg_inf) -> infinity_mult(neg_inf, Number);
infinity_mult(Number1, Number2) when is_integer(Number1), is_integer(Number2)->
  try Number1 * Number2
  catch
    error:system_limit ->
      if (Number1 >= 0) =:= (Number2 >= 0) -> pos_inf;
         true -> neg_inf
      end
  end.

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

arith_bnot(X1) ->
  case t_is_integer(X1) of
    false -> error;
    true ->
      Min1 = number_min(X1),
      Max1 = number_max(X1),
      {ok, t_from_range(infinity_add(infinity_inv(Max1), -1),
			infinity_add(infinity_inv(Min1), -1))}
  end.

arith_abs(X1) ->
  case t_is_integer(X1) of
    false ->
      case t_is_float(X1) of
        true -> t_float();
        false -> t_number()
      end;
    true ->
      Min1 = number_min(X1),
      Max1 = number_max(X1),
      {NewMin, NewMax} =
        case infinity_geq(Min1, 0) of
          true -> {Min1, Max1};
          false ->
            NegMin1 = infinity_inv(Min1),
            NegMax1 = infinity_inv(Max1),
            case infinity_geq(Max1, 0) of
              true  -> {0, max(NegMin1, Max1)};
              false -> {NegMax1, NegMin1}
            end
        end,
      t_from_range(NewMin, NewMax)
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
  New_min =
    if Min1_geq_zero -> 0;
       Max_range2 =:= 0 -> 0;
       true -> infinity_add(infinity_inv(Max_range2), 1)
    end,
  New_max =
    if Max1_leq_zero -> 0;
       Max_range2 =:= 0 -> 0;
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
%% Comparison of terms
%%=============================================================================

compare(Op, Lhs, Rhs) ->
  case t_is_none(t_inf(Lhs, Rhs)) of
    false ->
      t_boolean();
    true ->
      case Op of
        '<' -> always_smaller(Lhs, Rhs);
        '>' -> always_smaller(Rhs, Lhs);
        '=<' -> always_smaller(Lhs, Rhs);
        '>=' -> always_smaller(Rhs, Lhs)
      end
  end.

always_smaller(Type1, Type2) ->
  {Min1, Max1} = type_ranks(Type1),
  {Min2, Max2} = type_ranks(Type2),
  if Max1 < Min2 -> t_atom('true');
     Min1 > Max2 -> t_atom('false');
     true        -> t_boolean()
  end.

type_ranks(Type) ->
  type_ranks(Type, 1, 0, 0, type_order()).

type_ranks(_Type, _I, Min, Max, []) -> {Min, Max};
type_ranks(Type, I, Min, Max, [TypeClass|Rest]) ->
  {NewMin, NewMax} =
    case t_is_none(t_inf(Type, TypeClass)) of
      true  -> {Min, Max};
      false -> case Min of
		 0 -> {I, I};
		 _ -> {Min, I}
	       end
    end,
  type_ranks(Type, I+1, NewMin, NewMax, Rest).

type_order() ->
  [t_number(), t_atom(), t_reference(), t_fun(), t_port(), t_pid(), t_tuple(),
   t_map(), t_list(), t_bitstr()].

key_comparisons_fail(X0, KeyPos, TupleList) ->
  X = erl_types:t_widen_to_number(X0),
  lists:all(fun(Tuple) ->
		Key = type(erlang, element, 2, [KeyPos, Tuple]),
		t_is_none(t_inf(Key, X))
	    end, TupleList).

%%=============================================================================

-spec arg_types(atom(), atom(), arity()) -> [erl_types:erl_type()] | 'unknown'.

%%------- erlang --------------------------------------------------------------
arg_types(erlang, '!', 2) ->
  Pid = t_sup([t_pid(), t_reference(), t_port(), t_atom(),
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
%% Guard bif, needs to be here.
arg_types(erlang, abs, 1) ->
  [t_number()];
arg_types(erlang, append, 2) ->
  arg_types(erlang, '++', 2);
arg_types(erlang, apply, 2) ->
  [t_sup(t_tuple([t_module(),
		  t_atom()]),
	 t_fun()),
   t_list()];
arg_types(erlang, apply, 3) ->
  [t_sup(t_atom(), t_tuple()), t_atom(), t_list()];
%% Guard bif, needs to be here.
arg_types(erlang, binary_part, 2) ->
  [t_binary(), t_tuple([t_non_neg_integer(), t_integer()])];
%% Guard bif, needs to be here.
arg_types(erlang, binary_part, 3) ->
  [t_binary(), t_non_neg_integer(), t_integer()];
%% Guard bif, needs to be here.
arg_types(erlang, bit_size, 1) ->
  [t_bitstr()];
%% Guard bif, needs to be here.
arg_types(erlang, byte_size, 1) ->
  [t_bitstr()];
%% Guard bif, needs to be here.
arg_types(erlang, ceil, 1) ->
  [t_number()];
arg_types(erlang, halt, 0) ->
  [];
arg_types(erlang, halt, 1) ->
  [t_sup([t_non_neg_fixnum(), t_atom('abort'), t_string()])];
arg_types(erlang, halt, 2) ->
  [t_sup([t_non_neg_fixnum(), t_atom('abort'), t_string()]),
   t_list(t_tuple([t_atom('flush'), t_boolean()]))];
arg_types(erlang, error, 1) ->
  [t_any()];
arg_types(erlang, error, 2) ->
  [t_any(), t_list()];
arg_types(erlang, error, 3) ->
  [t_any(), t_list(), t_list()];
arg_types(erlang, exit, 1) ->
  [t_any()];
%% Guard bif, needs to be here.
arg_types(erlang, element, 2) ->
  [t_pos_fixnum(), t_tuple()];
%% Guard bif, needs to be here.
arg_types(erlang, float, 1) ->
  [t_number()];
%% Guard bif, needs to be here.
arg_types(erlang, floor, 1) ->
  [t_number()];
%% Primop, needs to be somewhere.
arg_types(erlang, build_stacktrace, 0) ->
  [];
%% Guard bif, needs to be here.
arg_types(erlang, hd, 1) ->
  [t_cons()];
arg_types(erlang, info, 1) ->
  arg_types(erlang, system_info, 1); % alias
arg_types(erlang, is_atom, 1) ->
  [t_any()];
arg_types(erlang, is_binary, 1) ->
  [t_any()];
arg_types(erlang, is_bitstring, 1) ->
  [t_any()];
arg_types(erlang, is_boolean, 1) ->
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
arg_types(erlang, is_map, 1) ->
  [t_any()];
arg_types(erlang, is_map_key, 2) ->
  [t_any(), t_map()];
arg_types(erlang, is_number, 1) ->
  [t_any()];
arg_types(erlang, is_pid, 1) ->
  [t_any()];
arg_types(erlang, is_port, 1) ->
  [t_any()];
arg_types(erlang, is_record, 2) ->
  [t_any(), t_atom()];
arg_types(erlang, is_record, 3) ->
  [t_any(), t_atom(), t_non_neg_fixnum()];
arg_types(erlang, is_reference, 1) ->
  [t_any()];
arg_types(erlang, is_tuple, 1) ->
  [t_any()];
%% Guard bif, needs to be here.
arg_types(erlang, length, 1) ->
  [t_list()];
arg_types(erlang, max, 2) ->
  [t_any(), t_any()];
%% Guard bif, needs to be here.
arg_types(erlang, map_size, 1) ->
  [t_map()];
%% Guard bif, needs to be here.
arg_types(erlang, map_get, 2) ->
  [t_any(), t_map()];
arg_types(erlang, make_fun, 3) ->
  [t_atom(), t_atom(), t_arity()];
arg_types(erlang, make_tuple, 2) ->
  [t_non_neg_fixnum(), t_any()];  % the value 0 is OK as first argument
arg_types(erlang, make_tuple, 3) ->
  [t_non_neg_fixnum(), t_any(), t_list(t_tuple([t_pos_integer(), t_any()]))];
arg_types(erlang, min, 2) ->
  [t_any(), t_any()];
arg_types(erlang, nif_error, 1) ->
  [t_any()];
arg_types(erlang, nif_error, 2) ->
  [t_any(), t_list()];
%% Guard bif, needs to be here.
arg_types(erlang, node, 0) ->
  [];
%% Guard bif, needs to be here.
arg_types(erlang, node, 1) ->
  [t_identifier()];
arg_types(erlang, raise, 3) ->
  %% The types are based on the implementation of erlang:raise/3, not
  %% the documention. That is, those are the types for which
  %% erlang:raise/3 will raise an exception. If the arguments do not
  %% conform to those types, erlang:raise/3 will return 'badarg'.
  Class = t_sup([t_atom('error'), t_atom('exit'), t_atom('throw')]),
  InfoList = t_list(),                 %erlang:raise/3 does not check the contents of the list.
  ArityOrArgs = t_any(),               %erlang:raise/3 does not check the type.
  MFA = t_tuple([t_module(), t_atom(), ArityOrArgs]),
  MFAS = t_tuple([t_module(), t_atom(), ArityOrArgs, InfoList]),
  Fun = t_tuple([t_fun(), ArityOrArgs]),
  FunI = t_tuple([t_fun(), ArityOrArgs, InfoList]),
  Stack = t_list(t_sup([MFA, MFAS, Fun, FunI])),
  [Class, t_any(), Stack];
%% Guard bif, needs to be here.
arg_types(erlang, round, 1) ->
  [t_number()];
%% Guard bif, needs to be here.
arg_types(erlang, self, 0) ->
  [];
arg_types(erlang, setelement, 3) ->
  [t_pos_integer(), t_tuple(), t_any()];
%% Guard bif, needs to be here.
arg_types(erlang, size, 1) ->
  [t_sup(t_tuple(), t_binary())];
arg_types(erlang, subtract, 2) ->
  arg_types(erlang, '--', 2);
arg_types(erlang, system_info, 1) ->
  [t_sup([t_atom(),                     % documented
	  t_tuple([t_atom(), t_any()]), % documented
	  t_tuple([t_atom(), t_atom(), t_any()]),
	  t_tuple([t_atom(allocator_sizes), t_reference(), t_any()])])];
arg_types(erlang, throw, 1) ->
  [t_any()];
%% Guard bif, needs to be here.
arg_types(erlang, tl, 1) ->
  [t_cons()];
%% Guard bif, needs to be here.
arg_types(erlang, trunc, 1) ->
  [t_number()];
%% Guard bif, needs to be here.
arg_types(erlang, tuple_size, 1) ->
  [t_tuple()];
arg_types(erlang, tuple_to_list, 1) ->
  [t_tuple()];

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
%%------- maps ----------------------------------------------------------------
arg_types(maps, from_keys, 2) ->
  [t_list(), t_any()];
arg_types(maps, from_list, 1) ->
  [t_list(t_tuple(2))];
arg_types(maps, get, 2) ->
  [t_any(), t_map()];
arg_types(maps, is_key, 2) ->
  [t_any(), t_map()];
arg_types(maps, merge, 2) ->
  [t_map(), t_map()];
arg_types(maps, put, 3) ->
  [t_any(), t_any(), t_map()];
arg_types(maps, remove, 2) ->
  [t_any(), t_map()];
arg_types(maps, size, 1) ->
  [t_map()];
arg_types(maps, update, 3) ->
  [t_any(), t_any(), t_map()];
arg_types(M, F, A) when is_atom(M), is_atom(F),
			is_integer(A), 0 =< A, A =< 255 ->
  unknown.                     % safe approximation for all functions.


-spec is_known(module(), atom(), arity()) -> boolean().

is_known(M, F, A) ->
  arg_types(M, F, A) =/= unknown.

check_fun_application(Fun, Args) ->
  case t_is_fun(Fun) of
    true ->
      case t_fun_args(Fun) of
	unknown ->
	  case t_is_impossible(t_fun_range(Fun)) of
	    true -> error;
	    false -> ok
	  end;
	FunDom when length(FunDom) =:= length(Args) ->
	  case any_is_none_or_unit(t_inf_lists(FunDom, Args)) of
	    true -> error;
	    false ->
	      case t_is_impossible(t_fun_range(Fun)) of
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
%% Some basic types used in various parts of the system
%% =====================================================================

t_endian() ->
  t_sup(t_atom('big'), t_atom('little')).

%% =====================================================================
%% These are used for the built-in functions of 'erlang'
%% =====================================================================

t_sequential_tracer() ->
  t_sup([t_atom('false'), t_pid(), t_port()]).

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

t_scheduler_bind_type_results() ->
  t_sup([t_atom('no_node_processor_spread'),
	 t_atom('no_node_thread_spread'),
	 t_atom('no_spread'),
	 t_atom('processor_spread'),
	 t_atom('spread'),
	 t_atom('thread_spread'),
	 t_atom('thread_no_node_processor_spread'),
	 t_atom('unbound')]).

t_system_multi_scheduling() ->
  t_sup([t_atom('blocked'), t_atom('disabled'), t_atom('enabled')]).

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
