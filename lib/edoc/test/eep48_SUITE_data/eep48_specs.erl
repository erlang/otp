%% =====================================================================
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%
%% Copyright 2019-2021 Radek Szymczyszyn
%% Copyright Ericsson AB 2026. All Rights Reserved.
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
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% %CopyrightEnd%
%%
%% =====================================================================

%% @doc Fixtures for chunk specs.
%% @author Radek Szymczyszyn <lavrin@gmail.com>
%% @end
-module(eep48_specs).

-export([f_spec_type_without_name/1,
       f_spec_type_with_name/1,
       f_spec_types_mixed/2,
       f_spec_with_multiple_clauses/2,
       f_spec_with_multiple_clauses_one_fun_clause/2,
       f_spec_lhs_match_expr/1,
       f_spec_rhs_match_expr/1,
       f_spec_unnamed_pattern/1,
       f_spec_bounded_single_clause_fun/4,
       f_spec_bounded_multiple_clause_fun/4,
       f_spec_bounded_singleton_atom/2,
       f_spec_bounded_singleton_int/2,
       f_spec_rettype_constraint/0,
       f_spec_indirect_constraint/2,
       f_spec_arg_type_in_retval/2]).

-export_type([opaque_type/0,nominal_type/0]).

-opaque opaque_type() :: atom().

-nominal nominal_type() :: atom().

-spec f_spec_type_without_name(atom()) -> ok.
f_spec_type_without_name(Arg) -> ok.

-spec f_spec_type_with_name(Arg :: atom()) -> ok.
f_spec_type_with_name(Arg) -> ok.

-type my_type() :: atom().

-spec f_spec_types_mixed(my_type(), Arg2 :: tuple()) -> ok.
f_spec_types_mixed(Arg1, Arg2) -> ok.

-spec f_spec_with_multiple_clauses(my_type(), atom()) -> atoms;
                                  (string(), integer()) -> not_atoms.
f_spec_with_multiple_clauses(A1, A2) -> atoms;
f_spec_with_multiple_clauses(S, I) -> not_atoms.

-spec f_spec_with_multiple_clauses_one_fun_clause(my_type(), atom()) -> atoms;
                                                 (string(), integer()) -> not_atoms.
f_spec_with_multiple_clauses_one_fun_clause(A1, A2) ->
    if
        is_atom(A1), is_atom(A2) -> atoms;
        true -> not_atoms
    end.

-spec f_spec_lhs_match_expr(any()) -> ok.
f_spec_lhs_match_expr(Pattern = {A, B}) -> ok.

-spec f_spec_rhs_match_expr(any()) -> ok.
f_spec_rhs_match_expr({A, B} = Pattern) -> ok.

-spec f_spec_unnamed_pattern(any()) -> ok.
f_spec_unnamed_pattern({_, _}) -> ok.

-spec f_spec_bounded_single_clause_fun(my_type(), tuple(), string(), I) -> ok when
      I :: integer().
f_spec_bounded_single_clause_fun(A, T, S, I) -> ok.

-spec f_spec_bounded_multiple_clause_fun
        (my_type(), tuple(), string(), I) -> ok when I :: integer();
        (string(), integer(), list(), A) -> ok when A :: atom().
f_spec_bounded_multiple_clause_fun(A1, A2, A3, A4) -> ok.

-spec f_spec_bounded_singleton_atom(I, a) -> ok when
      I :: integer().
f_spec_bounded_singleton_atom(I, a) -> ok.

-spec f_spec_bounded_singleton_int(I, 1) -> ok when
      I :: integer().
f_spec_bounded_singleton_int(I, 1) -> ok.

-spec f_spec_rettype_constraint() -> R when R :: atom().
f_spec_rettype_constraint() -> ok.

-spec f_spec_indirect_constraint(A, B) -> ok when
      B :: [A].
f_spec_indirect_constraint(A, B) -> ok.

-spec f_spec_arg_type_in_retval(A, B) -> [A] when B :: atom().
f_spec_arg_type_in_retval(A, _B) -> [A].
