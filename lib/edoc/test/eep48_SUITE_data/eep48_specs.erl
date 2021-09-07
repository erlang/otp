%% @doc Fixtures for chunk specs.
-module(eep48_specs).

-export([]).

-export_type([opaque_type/0]).

-opaque opaque_type() :: atom().

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
