%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023. All Rights Reserved.
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
%% This module tests that beam_ssa_alias_opt:opt/2 correctly annotates
%% instructions with information about unique and aliased operands.
%%
-compile(no_ssa_opt_destructive_update).

-module(alias).

-export([transformable0/1,
	 transformable1/1,
	 transformable1b/1,
	 transformable2/1,
	 transformable3/1,
	 transformable4/1,
	 transformable5/1,
	 %% transformable6/1,
	 transformable7/1,
	 transformable8/1,
	 transformable9/1,
	 transformable10/1,
	 transformable11/1,
	 transformable12a/1,
	 transformable12b/1,
	 transformable13/1,
	 transformable14/1,
	 transformable15/1,
	 transformable16/1,
	 transformable17/1,
	 transformable18/2,
	 transformable19/1,
	 transformable20/1,
	 transformable21/1,
	 transformable22/1,
	 transformable23/1,
	 transformable24/1,
	 transformable25/1,
	 transformable26/1,

	 not_transformable1/2,
	 not_transformable2/1,
	 not_transformable3/1,
	 not_transformable4/1,
	 not_transformable5/1,

         bad_get_status_by_type/0,
         stacktrace0/0,
         stacktrace1/0,
         in_cons/0,
         make_fun/0,
         gh6925/0,
         binary_part_aliases/2,
         aliased_map_lookup_bif/1,
         aliased_map_lookup_instr/1,

         variables_in_put_tuple_unique_0/1,
         variables_in_put_tuple_unique_1/1,
         variables_in_put_tuple_unique_2/1,
         variables_in_put_tuple_unique_3/1,
         variables_in_put_tuple_unique_4/1,
         variables_in_put_tuple_unique_5/1,
         variables_in_put_tuple_unique_6/1,
         variables_in_put_tuple_aliased/1,

         aliased_tuple_element_bif/1,
         aliased_tuple_element_bif/2,
         aliased_tuple_element_instr/1,
         aliased_pair_hd_bif/1,
         aliased_pair_tl_bif/1,
         aliased_pair_hd_instr/1,
         aliased_pair_tl_instr/1,
         aliasing_after_tuple_extract/1,
         alias_after_pair_hd/1,
         alias_after_pair_tl/1,
         unique_pair/0,
         make_unique_pair/1,

         double_map_lookup/2,
         double_tuple_element/2,
         tuple_element_aliasing/0,
         tuple_element_from_tuple_with_existing_child/0,

         extract_tuple_element/0,

         update_record0/0,
         update_record1/0,

         live_past_call_triggers_aliasing/1,

         fuzz0/0, fuzz0/1,
         alias_after_phi/0,
         check_identifier_type/0]).

%% Trivial smoke test
transformable0(L) ->
%ssa% (A) when post_ssa_opt ->
%ssa% _ = call(fun transformable0/2, A, _) { aliased => [A] }.
    transformable0(L, <<>>).

transformable0([H|T], Acc) ->
%ssa% (_, A) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, A, _, _, _, B, ...) { aliased => [B], unique => [A], first_fragment_dies => true }.
    transformable0(T, <<Acc/binary, H:8>>);
transformable0([], Acc) ->
    Acc.

transformable1(L) ->
    transformable1(L, start).

transformable1(L, start) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, _, _, _, X, _) { aliased => [X], unique => [Arg1], first_fragment_dies => true }.
    transformable1(L, <<>>);
transformable1([H|T], Acc) ->
    transformable1(T, <<Acc/binary, H:8>>);
transformable1([], Acc) ->
    Acc.

transformable1b(L) ->
    transformable1b(L, start).

transformable1b([H|T], X) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% Phi = phi({Arg1, _}, {_, _}, ...),
%ssa% _ = bs_create_bin(append, _, Phi, _, _, _, X, _) { aliased => [X], unique => [Phi], first_fragment_dies => true }.
    Acc = case X of
	      start ->
		  <<>>;
	      _ ->
		  X
	  end,
    N = <<Acc/binary, H:8>>,
    transformable1b(T, N);
transformable1b([], Acc) ->
    Acc.

transformable2(L) ->
    transformable2(L, <<>>).

transformable2([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = bs_create_bin(append, _, Arg1, _, _, _, X, _) { aliased => [X], unique => [Arg1], first_fragment_dies => true },
%ssa% _ = call(fun transformable2/2, _, A).
    case ex:f() of
	true ->
	    transformable2(T, <<Acc/binary, H:8>>);
	false ->
	    transformable2(T, <<>>)
    end;
transformable2([], Acc) ->
    Acc.

transformable3(L) ->
    transformable3(L, <<>>).

transformable3([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, _, _, _, X, _) { aliased => [X], unique => [Arg1], first_fragment_dies => true }.
    R = case ex:f() of
	    true ->
		<<Acc/binary, H:8>>;
	    false ->
		<<>>
	end,
    transformable3(T, R);
transformable3([], Acc) ->
    Acc.

transformable4(L) ->
    transformable4(L, <<>>).

transformable4([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% Phi = phi({_, _}, {Arg1, _}, ...),
%ssa% _ = bs_create_bin(append, _, Phi, _, _, _, X, _) { aliased => [X], unique => [Phi], first_fragment_dies => true }.
    R = case ex:f() of
	    true ->
		Acc;
	    false ->
		<<>>
	end,
    transformable4(T, <<R/binary, H:8>>);
transformable4([], Acc) ->
    Acc.

%% Check that the alias analysis handles local functions.
transformable5(L) ->
    transformable5(L, <<>>).

transformable5([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, _, _, _, X, _) { aliased => [Arg1,X] }.

%% Although does_not_escape/1 does not let its argument escape, it is
%% live across the call and thus aliased in does_not_escape.

    does_not_escape(Acc),
    transformable5(T, <<Acc/binary, H:8>>);
transformable5([], Acc) ->
    Acc.

does_not_escape(_) ->
    ok.

%% Check that the analysis works when we have an appendable binary in
%% the head of a cons.
transformable7(L) ->
    transformable7(L, [<<>>|0]).

transformable7([H|T], [Acc|N]) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_hd(Arg1),
%ssa% _ = bs_create_bin(append, _, A, _, _, _, X, _) { aliased => [X], unique => [A], first_fragment_dies => true }.
    transformable7(T, [<<Acc/binary, H:8>>|N+1]);
transformable7([], Acc) ->
    Acc.

%% Check that the analysis works when we have an appendable binary in
%% just one of the clauses.
transformable8(L) ->
    transformable8(L, start).

transformable8(L, start) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, _, _, _, X, _) { aliased => [X], unique => [Arg1], first_fragment_dies => true }.
    transformable8(L, <<>>);
transformable8([H|T], Acc) ->
    transformable8b(T, <<Acc/binary, H:8>>);
transformable8([], Acc) ->
    Acc.

transformable8b(T, Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, ...) { unique => [Arg1], first_fragment_dies => true }.
    transformable8(T, <<Acc/binary, 16#ff:8>>).

%% Check that the analysis works across mutually recursive functions.
transformable9(L) ->
    transformable9a(L, <<>>).

transformable9a([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, _, _, _, _, _, _, _, X, _) { aliased => [X], unique => [Arg1], first_fragment_dies => true }.
    transformable9b(T, <<Acc/binary, 0:8, H:8>>);
transformable9a([], Acc) ->
    Acc.

transformable9b([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, _, _, _, _, _, _, _, X, _) { aliased => [X], unique => [Arg1], first_fragment_dies => true }.
    transformable9a(T, <<Acc/binary, 1:8, H:8>>);
transformable9b([], Acc) ->
    Acc.

%% Check that the analysis works for binaries embedded in a literal.
transformable10(L) ->
    transformable10(L, {<<>>,0}).

transformable10([H|T], {Acc,N}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tuple_element(Arg1, 0),
%ssa% _ = bs_create_bin(append, _, A, _, _, _, X, _) { aliased => [X], unique => [A], first_fragment_dies => true }.
    transformable10(T, {<<Acc/binary, H:8>>,N+1});
transformable10([], Acc) ->
    Acc.

%% Check that the analysis works across clauses
transformable11(L) ->
    transformable11(L, <<>>).

transformable11([H|T], Acc) when H =:= 0 ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = bs_create_bin(append, _, Arg1, ...) { unique => [Arg1], first_fragment_dies => true },
%ssa% _ = call(fun transformable11/2, _, A),
%ssa% B = bs_create_bin(append, _, Arg1, ...) { unique => [Arg1], first_fragment_dies => true },
%ssa% _ = call(fun transformable11/2, _, B).
    transformable11(T, <<Acc/binary, 0:8>>);
transformable11([_|T], Acc)->
    transformable11(T, <<Acc/binary, 1:8>>);
transformable11([], Acc) ->
    Acc.

% Broken, type analysis can't handle the list
transformable12a(L) ->
    transformable12(L, {<<>>}).

transformable12b(L) ->
    transformable12(L, [<<>>]).

%% The type analysis can't handle the list yet
transformable12([H|T], {Acc}) ->
%ssa% (_, _) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, A, _, _, _, B, _) { aliased => [B], unique => [A], first_fragment_dies => true },
%ssa% _ = bs_create_bin(append, _, C, _, _, _, D, _) { aliased => [D], unique => [C], first_fragment_dies => true }.
    transformable12([H|T], {<<Acc/binary,H:8>>});
transformable12([H|T], [Acc]) ->
    transformable12([H|T], [<<Acc/binary,H:8>>]);
transformable12([], {Acc}) ->
    Acc;
transformable12([], [Acc]) ->
    Acc.

%% Check binaries coming from another function.
transformable13(L) ->
    transformable13(L, make_empty_binary()).

transformable13([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, _, _, _, X, _) { aliased => [X], unique => [Arg1], first_fragment_dies => true }.
    transformable13(T, <<Acc/binary, H:8>>);
transformable13([], Acc) ->
    Acc.

make_empty_binary() ->
    <<>>.

%% Check binaries coming from other functions, with various levels of
%% nesting in the literals and how they are picked apart using
%% matching.
transformable14(L) ->
    {X} = make_wrapped_empty_binary(),
    transformable14(L, X).

transformable14([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, _, _, _, X, _) { aliased => [X], unique => [Arg1], first_fragment_dies => true }.
    transformable14(T, <<Acc/binary, H:8>>);
transformable14([], Acc) ->
    Acc.

make_wrapped_empty_binary() ->
    {<<>>}.

transformable15(L) ->
    {X} = make_wrapped_empty_binary(),
    Y = make_empty_binary(),
    transformable15(L, X, Y).

transformable15([A,B|T], Acc0, Acc1) ->
%ssa% (_, Arg1, Arg2) when post_ssa_opt ->
%ssa% A = bs_create_bin(append, _, Arg1, _, _, _, X, _) { aliased => [X], unique => [Arg1], first_fragment_dies => true },
%ssa% B = bs_create_bin(append, _, Arg2, _, _, _, Y, _) { aliased => [Y], unique => [Arg2], first_fragment_dies => true },
%ssa% _ = call(fun transformable15/3, _, A, B).

    transformable15(T, <<Acc0/binary, A:8>>, <<Acc1/binary, B:8>>);
transformable15([], Acc0, Acc1) ->
    {Acc0,Acc1}.

transformable16(L) ->
    X = make_wrapped_empty_binary(),
    Y = make_empty_binary(),
    transformable16(L, {X, Y}).

transformable16([A,B|T], {{Acc0}, Acc1}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tuple_element(Arg1, 0),
%ssa% B = get_tuple_element(A, 0),
%ssa% C = bs_create_bin(append, _, B, _, _, _, X, _) { aliased => [X], unique => [B], first_fragment_dies => true },
%ssa% D = get_tuple_element(Arg1, 1),
%ssa% E = bs_create_bin(append, _, D, _, _, _, Y, _) { aliased => [Y], unique => [D], first_fragment_dies => true },
%ssa% F = put_tuple(C),
%ssa% G = put_tuple(F, E),
%ssa% _ = call(fun transformable16/2, _, G).
    transformable16(T, {{<<Acc0/binary, A:8>>}, <<Acc1/binary, B:8>>});
transformable16([], {{Acc0}, Acc1}) ->
    {Acc0,Acc1}.

transformable17(L) ->
    transformable17(L, [0|<<>>]).

transformable17([H|T], [N|Acc]) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tl(Arg1),
%ssa% B = bs_create_bin(append, _, A, _, _, _, X, _) { aliased => [X], unique => [A], first_fragment_dies => true },
%ssa% C = put_list(_, B),
%ssa% _ = call(fun transformable17/2, _, C).
    transformable17(T, [N+1|<<Acc/binary, H:8>>]);
transformable17([], Acc) ->
    Acc.

%% Check that type information is used to figure out that {<<>>, X} is
%% not aliased.
transformable18(L, X) when is_integer(X), X < 256 ->
    transformable18b(L, {<<>>, X}).

transformable18b([H|T], {Acc,X}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tuple_element(Arg1, 0),
%ssa% B = bs_create_bin(append, _, A, _, _, _, X, _) { unique => [X,A], first_fragment_dies => true },
%ssa% C = put_tuple(B, _),
%ssa% _ = call(fun transformable18b/2, _, C).
    transformable18b(T, {<<Acc/binary, (H+X):8>>, X});
transformable18b([], {Acc,_}) ->
    Acc.

%% Check that the analysis works when the binary isn't embedded in a
%% tuple literal.
transformable19(L) ->
    X = case ex:foo() of
	    true ->
		4711;
	    false ->
		17
	end,
    transformable19b(L, {<<>>, X}).

transformable19b([H|T], {Acc,X}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tuple_element(Arg1, 0),

%ssa% B = bs_create_bin(append, _, A, _, _, _, X, _) { unique => [X, A], first_fragment_dies => true },
%ssa% C = put_tuple(B, _),
%ssa% _ = call(fun transformable19b/2, _, C).
    transformable19b(T, {<<Acc/binary, (H+X):8>>, X});
transformable19b([], {Acc,_}) ->
    Acc.

%% Check that the analysis works when the binary isn't embedded in a
%% list literal.
transformable20(L) ->
    X = case ex:foo() of
	    true ->
		4711;
	    false ->
		17
	end,
    transformable20b(L, [<<>>|X]).

transformable20b([H|T], [Acc|X]) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_hd(Arg1),
%ssa% B = bs_create_bin(append, _, A, _, _, _, X, _) { unique => [X, A], first_fragment_dies => true },
%ssa% C = put_list(B, _),
%ssa% _ = call(fun transformable20b/2, _, C).
    transformable20b(T, [<<Acc/binary, (H+X):8>>|X+1]);
transformable20b([], [Acc|_]) ->
    Acc.

%% Check that the analysis works when the binary is embedded in a
%% tuple literal returned from another function.
transformable21(L) ->
    transformable21(L, make_empty_binary_tuple()).

transformable21([H|T], {AccA,AccB}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tuple_element(Arg1, 0),
%ssa% B = bs_create_bin(append, _, A, _, _, _, X, _) { aliased => [X], unique => [A], first_fragment_dies => true },
%ssa% C = get_tuple_element(Arg1, 1),
%ssa% D = bs_create_bin(append, _, C, _, _, _, _, _) { unique => [C], first_fragment_dies => true },
%ssa% E = put_tuple(B, D),
%ssa% _ = call(fun transformable21/2, _, E).
    transformable21(T, {<<AccA/binary, H:8>>,<<AccB/binary, 17:8>>});
transformable21([], {AccA, AccB}) ->
    {AccA, AccB}.

make_empty_binary_tuple() ->
    {<<>>, <<>>}.

transformable22(L) ->
    transformable22(L, [<<>>|<<>>]).

transformable22([H|T], [AccA|AccB]) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_hd(Arg1),
%ssa% B = get_tl(Arg1),
%ssa% C = bs_create_bin(append, _, A, _, _, _, X, _) { aliased => [X], unique => [A], first_fragment_dies => true },
%ssa% D = bs_create_bin(append, _, B, _, _, _, _, _) { unique => [B], first_fragment_dies => true },
%ssa% E = put_list(C, D),
%ssa% _ = call(fun transformable22/2, _, E).
    transformable22(T, [<<AccA/binary, H:8>>|<<AccB/binary, 17:8>>]);
transformable22([], Acc) ->
    Acc.

%% As transformable21 but with a more complex embedded tuple
transformable23(L) ->
    transformable23(L, make_empty_binary_tuple_nested()).

transformable23([H|T], {AccA,{AccB},X}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tuple_element(Arg1, 0),
%ssa% B = bs_create_bin(append, _, A, _, _, _, X, _) { aliased => [X], unique => [A], first_fragment_dies => true },
%ssa% C = get_tuple_element(Arg1, 1),
%ssa% D = get_tuple_element(C, 0),
%ssa% E = bs_create_bin(append, _, D, _, _, _, _, _) { unique => [D], first_fragment_dies => true },
%ssa% F = put_tuple(E),
%ssa% G = put_tuple(B, F, _),
%ssa% _ = call(fun transformable23/2, _, G).
    transformable23(T, {<<AccA/binary, H:8>>,{<<AccB/binary, 17:8>>}, X});
transformable23([], {AccA, AccB, X}) ->
    {AccA, AccB, X}.

make_empty_binary_tuple_nested() ->
    {<<>>, {<<>>}, 47}.

transformable24(L) ->
    transformable24(L, {<<>>, ex:foo(),ex:foo()}).

transformable24([H|T], {Acc,X,Y}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% X = get_tuple_element(Arg1, 1),
%ssa% Acc = get_tuple_element(Arg1, 0),
%ssa% A = bs_create_bin(append, _, Acc, _, _, _, Sum, _) { unique => [Sum,Acc], first_fragment_dies => true },
%ssa% Y = get_tuple_element(Arg1, 2),
%% X is unique as it is known to be a number.
%ssa% B = put_tuple(A, X, Y) { aliased => [Y], unique => [A,X] },
%ssa% _ = call(fun transformable24/2, C, B) { aliased => [C], unique => [B] }.
    transformable24(T, {<<Acc/binary,(H+X):8>>,X,Y});
transformable24([], {Acc,_}) ->
    Acc.

%% Check that the update of more than one element of a tuple is
%% handled.
transformable25(L) ->
    transformable25(L, {<<>>,<<>>}).

transformable25([H|T], {AccA,AccB}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tuple_element(Arg1, 0),
%ssa% B = bs_create_bin(append, _, A, _, _, _, X, _) { aliased => [X], unique => [A], first_fragment_dies => true },
%ssa% C = get_tuple_element(Arg1, 1),
%ssa% D = bs_create_bin(append, _, C, _) { unique => [C], first_fragment_dies => true },
%ssa% E = put_tuple(B, D),
%ssa% _ = call(fun transformable25/2, _, E).
    transformable25(T, {<<AccA/binary, H:8>>,<<AccB/binary>>});
transformable25([], Acc) ->
    Acc.

%% Check that the update of more than two elements of a tuple is
%% handled (check the that inductive step of
%% beam_ssa_alias_opt:merge_arg_patches/1 works).
transformable26(L) ->
    transformable26(L, {<<>>,<<>>,<<>>}).

transformable26([H|T], {AccA,AccB,AccC}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tuple_element(Arg1, 0),
%ssa% B = bs_create_bin(append, _, A, _, _, _, X, _) { aliased => [X], unique => [A], first_fragment_dies => true },
%ssa% C = get_tuple_element(Arg1, 1),
%ssa% D = bs_create_bin(append, _, C, _, _, _, Y, _) { aliased => [Y], unique => [C], first_fragment_dies => true },
%ssa% E = get_tuple_element(Arg1, 2),
%ssa% F = bs_create_bin(append, _, E, _, _, _, Z, _) { aliased => [Z], unique => [E], first_fragment_dies => true },
%ssa% G = put_tuple(B, D, F),
%ssa% _ = call(fun transformable26/2, _, G).
    transformable26(T, {<<AccA/binary, H:8>>,
			<<AccB/binary, H:8>>,
			<<AccC/binary, H:8>>});
transformable26([], Acc) ->
    Acc.

%%
%% Check that we detect aliasing
%%

not_transformable1([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, _, _, _, X, _) { aliased => [Arg1, X], first_fragment_dies => true }.
    not_transformable1(T, <<Acc/binary, H:8>>);
not_transformable1([], Acc) ->
    Acc.

not_transformable2(L) ->
    not_transformable2(L, <<>>).

not_transformable2([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, _, _, _, X, _) { aliased => [Arg1, X], first_fragment_dies => true }.
    ex:escape(Acc),
    not_transformable2(T, <<Acc/binary, H:8>>);
not_transformable2([], Acc) ->
    Acc.

not_transformable3(L) ->
    not_transformable3(L, <<>>, []).

not_transformable3([H|T], Acc, Ls) ->
%ssa% (_, Arg1, _) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, _, _, _, X, _) { aliased => [Arg1, X], first_fragment_dies => false }.
    not_transformable3(T, <<Acc/binary, H:8>>, [Acc|Ls]);
not_transformable3([], Acc, Ls) ->
    {Acc, Ls}.

%% We randomly keep multiple references to the binary, so we should
%% detect aliasing.
not_transformable4(L) ->
    not_transformable4(L, [<<>>|[]]).

not_transformable4([H|T], X=[Acc|Ls]) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, X, _, _, _, Y, _) { aliased => [Y, X], first_fragment_dies => true }.
    Tmp = case ex:f() of
	      true ->
		  [Q|_] = X,
		  Q;
	      false ->
		  ok
	  end,
    T1 = [Tmp|Ls],
    not_transformable4(T, [<<Acc/binary, H:8>>|T1]);
not_transformable4([], Acc) ->
    Acc.

%% Check that the leak in the external call is detected despite the
%% mutual recursion.
not_transformable5(L) ->
    not_transformable5a(L, <<>>).

not_transformable5a([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, _, _, _, _, _, _, _, X, _) { aliased => [Arg1, X], first_fragment_dies => true }.
    not_transformable5b(T, <<Acc/binary, 0:8, H:8>>);
not_transformable5a([], Acc) ->
    Acc.

not_transformable5b([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, _, _, _, _, _, _, _, X, _) { aliased => [Arg1, X], first_fragment_dies => true }.
    ex:alias(Acc),
    not_transformable5a(T, <<Acc/binary, 1:8, H:8>>);
not_transformable5b([], Acc) ->
    Acc.

%% Reproducer for a bug in beam_ssa_alias:aa_get_status_by_type/2
%% where it would return the wrong alias/uniqe status for certain
%% combinations of returned types.
bad_get_status_by_type() ->
%ssa% () when post_ssa_opt ->
%ssa% A = call(fun bad_get_status_by_type_1/0),
%ssa% ret(A) { aliased => [A] }.
    bad_get_status_by_type_1().

bad_get_status_by_type_1() ->
    case e:foo() of
	a -> <<0:1>>;
	b -> X = e:bar(),
	     true = is_binary(X),
	     X
    end.

stacktrace0() ->
%ssa% () when post_ssa_opt ->
%ssa% X = call(fun transformable0/2, ...),
%ssa% ret(A) { unique => [A] },
%ssa% ret(A) { aliased => [A] },
%ssa% ret(_) { result_type => none }.
    X = transformable0(e:foo(), <<>>),
    try
	e:code_that_fails(),
	X
    catch
	_:_:Stacktrace ->
	    e:foo(Stacktrace),
	    X
    end.

stacktrace1() ->
%ssa% () when post_ssa_opt ->
%ssa% X = call(fun transformable0/2, ...),
%ssa% ret(A) { unique => [A] },
%ssa% ret('bad'),
%ssa% ret(_) { result_type => none }.
    try
	X = transformable0(e:foo(), <<>>),
	e:code_that_fails(),
	X
    catch
	_:_:Stacktrace ->
	    e:foo(Stacktrace),
	    bad
    end.

in_cons() ->
%ssa% () when post_ssa_opt ->
%ssa% A = call(fun in_cons_inner/1, ...),
%ssa% ret(A) { unique => [A] },
%ssa% ret(_) { result_type => none }.
    in_cons_inner([x|<<>>]).

in_cons_inner([x|B]) ->
    [x|<<B/binary,1:8>>].

%% The alias analysis did not consider values copied into the
%% environment which in turn led to unsafe private appends and
%% segfaults, GH-6890.
make_fun() ->
    make_fun([<<"hello">>], <<>>).

make_fun(List, Indent) ->
    lists:map(fun (X) -> make_fun1(X, Indent) end, List).

make_fun1(X, Indent) ->
%ssa% (_, A) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, A, ...) { aliased => [A], first_fragment_dies => true }.
    make_fun(X, <<Indent/binary,"    ">>).


%% Check that the alias analysis detects that the first fragment to
%% doesn't die with the second bs_create_bin, GH-6925.
gh6925() ->
%ssa% () when post_ssa_opt ->
%ssa% A = bs_create_bin(private_append, ...),
%ssa% B = bs_create_bin(append, ...) { first_fragment_dies => false }.
    A = << <<"x">> || true >>,
    B = <<A/binary, "z">>,
    {A, B}.

%% Check that bif:binary_part/3 is correctly flagged as an operation
%% which aliases its operands
binary_part_aliases(A, B) ->
%ssa% (_,_) when post_ssa_opt ->
%ssa% X = bif:binary_part(_, _, _),
%ssa% ret(X) {aliased => [X]}.
    binary_part(<<>>, A, B).

%% Check that as the map is aliased, the extracted value should also
%% be aliased.
aliased_map_lookup_bif(M) ->
%ssa% (M) when post_ssa_opt ->
%ssa% X = bif:map_get(a, M),
%ssa% ret(X) {aliased => [X]}.
    map_get(a, M).

%% Check that as the map is aliased, the extracted value should also
%% be aliased.
aliased_map_lookup_instr(M) ->
%ssa% (M) when post_ssa_opt ->
%ssa% X = get_map_element(M, a),
%ssa% ret(X) {aliased => [X]}.
    #{a:=X} = M,
    X.

%% Check that as the tuple is aliased, the extracted value should also
%% be aliased.
aliased_tuple_element_bif(T) ->
%ssa% (T) when post_ssa_opt ->
%ssa% X = bif:element(1, T),
%ssa% ret(X) {aliased => [X]}.
    element(1, T).

%% Check that as the tuple is aliased, the extracted value should also
%% be aliased.
aliased_tuple_element_instr(T) ->
%ssa% (T) when post_ssa_opt ->
%ssa% X = get_tuple_element(T, 0),
%ssa% ret(X) {aliased => [X]}.
    {X} = T,
    X.

%% Check that alias analysis doesn't crash when element is given a
%% non-constant index.
aliased_tuple_element_bif(T, I) ->
%ssa% (T, I) when post_ssa_opt ->
%ssa% X = bif:element(I, T),
%ssa% ret(X) {aliased => [X]}.
    element(I, T).

%% Check that as the pair is aliased, the extracted value should also
%% be aliased.
aliased_pair_hd_bif(Ls) ->
%ssa% (Ls) when post_ssa_opt ->
%ssa% X = bif:hd(Ls),
%ssa% ret(X) {aliased => [X]}.
    hd(Ls).

%% Check that as the pair is aliased, the extracted value should also
%% be aliased.
aliased_pair_tl_bif(Ls) ->
%ssa% (Ls) when post_ssa_opt ->
%ssa% X = bif:tl(Ls),
%ssa% ret(X) {aliased => [X]}.
    tl(Ls).

%% Check that as the pair is aliased, the extracted value should also
%% be aliased.
aliased_pair_hd_instr(Ls) ->
%ssa% (Ls) when post_ssa_opt ->
%ssa% X = get_hd(Ls),
%ssa% ret(X) {aliased => [X]}.
    [X|_] = Ls,
    X.

%% Check that as the pair is aliased, the extracted value should also
%% be aliased.
aliased_pair_tl_instr(Ls) ->
%ssa% (Ls) when post_ssa_opt ->
%ssa% X = get_tl(Ls),
%ssa% ret(X) {aliased => [X]}.
    [_|X] = Ls,
    X.

aliasing_after_tuple_extract(N) ->
    aliasing_after_tuple_extract(N, {<<>>, dummy}).

%% Check that both the tuple (Acc) and the extracted element (X) are
%% aliased.
aliasing_after_tuple_extract(0, Acc) ->
%ssa% (_,Acc) when post_ssa_opt ->
%ssa% X = get_tuple_element(Acc, 0) {aliased => [Acc]},
%ssa% _ = bs_create_bin(_,_,X,...) {aliased => [X]}.
    Acc;
aliasing_after_tuple_extract(N, Acc) ->
    {X,_} = Acc,
    aliasing_after_tuple_extract(N - 1, {<<X/bitstring, 1>>, Acc}).


%% Check that both the pair (Acc) and the extracted element (X) are
%% aliased.
alias_after_pair_hd(N) ->
    alias_after_pair_hd(N, [<<>>|dummy]).

alias_after_pair_hd(0, Acc) ->
    Acc;
alias_after_pair_hd(N, Acc) ->
%ssa% (_,Acc) when post_ssa_opt ->
%ssa% X = get_hd(Acc) {aliased => [Acc]},
%ssa% _ = bs_create_bin(_,_,X,...) {aliased => [X]}.
    [X|_] = Acc,
    alias_after_pair_hd(N - 1, [<<X/bitstring, 1>>|Acc]).

%% Check that both the pair (Acc) and the extracted element (X) are
%% aliased.
alias_after_pair_tl(N) ->
    alias_after_pair_tl(N, [dummy|<<>>]).

alias_after_pair_tl(0, Acc) ->
    Acc;
alias_after_pair_tl(N, Acc) ->
%ssa% (_,Acc) when post_ssa_opt ->
%ssa% X = get_tl(Acc) {aliased => [Acc]},
%ssa% _ = bs_create_bin(_,_,X,...) {aliased => [X]}.
    [_|X] = Acc,
    alias_after_pair_tl(N - 1, [Acc|<<X/bitstring, 1>>]).

make_unique_pair(X) when is_integer(X) ->
    [X|X].

%% No aliasing occurs as Pair dies and only plain values are
%% extracted.
unique_pair() ->
%ssa% () when post_ssa_opt ->
%ssa% P = call(fun make_unique_pair/1, ...),
%ssa% H = get_hd(P),
%ssa% T = get_tl(P),
%ssa% R = put_tuple(H, T, P) {unique => [H, P, T]},
%ssa% ret(R) {unique => [R]}.
    Pair = make_unique_pair(e:f()),
    H = hd(Pair),
    T = tl(Pair),
    {H, T, Pair}.

%% Check that although the map is unique, the extracted values should
%% always be aliased as we can't know if they are the same.
%%
double_map_lookup(A, B) ->
%ssa% (A, B) when post_ssa_opt ->
%ssa% X = bif:map_get(A, Map),
%ssa% Y = bif:map_get(B, Map),
%ssa% _ = put_tuple(X, Y) {aliased => [X,Y]}.
    Map = make_map(),
    X = map_get(A, Map),
    Y = map_get(B, Map),
    {X, Y}.

make_map() ->
    #{a=> <<>>, b=> <<>>}.

%% Check that although the tuple is unique, the extracted values should
%% always be aliased as we can't know if they are the same.
%%
double_tuple_element(A, B) ->
%ssa% (A, B) when post_ssa_opt ->
%ssa% X = bif:element(A, T),
%ssa% Y = bif:element(B, T),
%ssa% _ = put_tuple(X, Y) {aliased => [X,Y]}.
    T = make_empty_binary_tuple(),
    X = element(A, T),
    Y = element(B, T),
    {X, Y}.

%% Check that both T and X are aliased to prevent the append to be
%% rewritten to a private_append.
tuple_element_aliasing() ->
%ssa% () when post_ssa_opt ->
%ssa% T = call(fun make_empty_binary_tuple/0),
%ssa% X = get_tuple_element(T, 0) { aliased => [T]},
%ssa% Y = bs_create_bin(append, _, X, _, _, _, B, _) { aliased => [X] },
%ssa% Z = put_tuple(Y, T) {aliased => [T, Y] }.
    T = make_empty_binary_tuple(),
    X = element(1, T),
    Z = <<X/binary,1:8>>,
    {Z, T}.

%% Check that alias analysis doesn't crash when extracting an element
%% from a tuple which already has a derived value associated with it.
%% Test case found by Robin Morisset.
tuple_element_from_tuple_with_existing_child() ->
    [ 0 || _V1 <- erlang:memory(),
	   { maybe
		 error ?= _V1,
		 ok
	     end,
	     maybe
		 {<<_>>} ?= _V1,
		 ok
	     end } ].

%% Check that the same variable used twice in a put_tuple does not
%% trigger aliasing when the variable's type isn't boxed, but that we
%% do when the types are unknown. These tests need their own private
%% version of the identity function, as the type analysis will
%% otherwise determine that the result type of id/1 (as it is shared
%% between all tests) is {variables_in_put_tuple, any(), any()} and
%% the more specific type information will be lost.
-record(variables_in_put_tuple, {a=0,b=0}).

variables_in_put_tuple_unique_0(A) when is_atom(A) ->
%ssa% (A0) when post_ssa_opt ->
%ssa% T0 = put_tuple(_, A0, A0),
%ssa% T = call(_, T0),
%ssa% A = get_tuple_element(T, 1),
%ssa% B = get_tuple_element(T, 2),
%ssa% R = put_tuple(A, B) { unique => [B,A] },
%ssa% ret(R) { unique => [R] }.
    Id = fun(X) -> X end,
    #variables_in_put_tuple{a=X,b=Y} = Id(#variables_in_put_tuple{a=A,b=A}),
    {X,Y}.

variables_in_put_tuple_unique_1(A) when is_number(A) ->
%ssa% (A0) when post_ssa_opt ->
%ssa% T0 = put_tuple(_, A0, A0),
%ssa% T = call(_, T0),
%ssa% A = get_tuple_element(T, 1),
%ssa% B = get_tuple_element(T, 2),
%ssa% R = put_tuple(A, B) { unique => [B,A] },
%ssa% ret(R) { unique => [R] }.
    Id = fun(X) -> X end,
    #variables_in_put_tuple{a=X,b=Y} = Id(#variables_in_put_tuple{a=A,b=A}),
    {X,Y}.

variables_in_put_tuple_unique_2(A) when is_integer(A) ->
%ssa% (A0) when post_ssa_opt ->
%ssa% T0 = put_tuple(_, A0, A0),
%ssa% T = call(_, T0),
%ssa% A = get_tuple_element(T, 1),
%ssa% B = get_tuple_element(T, 2),
%ssa% R = put_tuple(A, B) { unique => [B,A] },
%ssa% ret(R) { unique => [R] }.
    Id = fun(X) -> X end,
    #variables_in_put_tuple{a=X,b=Y} = Id(#variables_in_put_tuple{a=A,b=A}),
    {X,Y}.

variables_in_put_tuple_unique_3(A) when is_float(A) ->
%ssa% (A0) when post_ssa_opt ->
%ssa% T0 = put_tuple(_, A0, A0),
%ssa% T = call(_, T0),
%ssa% A = get_tuple_element(T, 1),
%ssa% B = get_tuple_element(T, 2),
%ssa% R = put_tuple(A, B) { unique => [B,A] },
%ssa% ret(R) { unique => [R] }.
    Id = fun(X) -> X end,
    #variables_in_put_tuple{a=X,b=Y} = Id(#variables_in_put_tuple{a=A,b=A}),
    {X,Y}.

variables_in_put_tuple_unique_4(A) when is_pid(A) ->
%ssa% (A0) when post_ssa_opt ->
%ssa% T0 = put_tuple(_, A0, A0),
%ssa% T = call(_, T0),
%ssa% A = get_tuple_element(T, 1),
%ssa% B = get_tuple_element(T, 2),
%ssa% R = put_tuple(A, B) { unique => [B,A] },
%ssa% ret(R) { unique => [R] }.
    Id = fun(X) -> X end,
    #variables_in_put_tuple{a=X,b=Y} = Id(#variables_in_put_tuple{a=A,b=A}),
    {X,Y}.

variables_in_put_tuple_unique_5(A) when is_port(A) ->
%ssa% (A0) when post_ssa_opt ->
%ssa% T0 = put_tuple(_, A0, A0),
%ssa% T = call(_, T0),
%ssa% A = get_tuple_element(T, 1),
%ssa% B = get_tuple_element(T, 2),
%ssa% R = put_tuple(A, B) { unique => [B,A] },
%ssa% ret(R) { unique => [R] }.
    Id = fun(X) -> X end,
    #variables_in_put_tuple{a=X,b=Y} = Id(#variables_in_put_tuple{a=A,b=A}),
    {X,Y}.

variables_in_put_tuple_unique_6(A) when is_reference(A) ->
%ssa% (A0) when post_ssa_opt ->
%ssa% T0 = put_tuple(_, A0, A0),
%ssa% T = call(_, T0),
%ssa% A = get_tuple_element(T, 1),
%ssa% B = get_tuple_element(T, 2),
%ssa% R = put_tuple(A, B) { unique => [B,A] },
%ssa% ret(R) { unique => [R] }.
    Id = fun(X) -> X end,
    #variables_in_put_tuple{a=X,b=Y} = Id(#variables_in_put_tuple{a=A,b=A}),
    {X,Y}.

%% Unknown types, no aliasing of the tuple, but the elements should be
%% aliased.
variables_in_put_tuple_aliased(A) ->
%ssa% (A0) when post_ssa_opt ->
%ssa% T0 = put_tuple(_, A0, A0),
%ssa% T = call(_, T0),
%ssa% A = get_tuple_element(T, 1),
%ssa% B = get_tuple_element(T, 2),
%ssa% R = put_tuple(A, B) { aliased => [B,A] },
%ssa% ret(R) { unique => [R] }.
    Id = fun(X) -> X end,
    #variables_in_put_tuple{a=X,b=Y} = Id(#variables_in_put_tuple{a=A,b=A}),
    {X,Y}.

id(X) ->
    X.

%% Check that we don't unnecessarily flag a tuple as aliased just
%% because we extract a plain type from it.
generate_integer() ->
    case ex:f() of
        a ->
            1;
        b ->
            2
    end.

make_tuple() ->
    {generate_integer(), generate_integer()}.

extract_tuple_element() ->
%ssa% () when post_ssa_opt ->
%ssa% R = put_tuple(X, Y, Z)  {unique => [Z, Y, X]} ,
%ssa% ret(R) { unique => [R] }.
    {X,Y} = Z = make_tuple(),
    {X,Y,Z}.

-record(r0, {not_aliased=0,aliased=[]}).

update_record0() ->
    update_record0(ex:f(), #r0{}).

update_record0([Val|Ls], Acc=#r0{not_aliased=N}) ->
%ssa% (_, Rec) when post_ssa_opt ->
%ssa% _ = update_record(reuse, 3, Rec, 3, A, 2, NA) {unique => [Rec, NA], aliased => [A]}.
    R = Acc#r0{not_aliased=N+1,aliased=Val},
    update_record0(Ls, R);
update_record0([], Acc) ->
    Acc.

-record(r1, {not_aliased0=0,not_aliased1=[]}).

update_record1() ->
    update_record1(ex:f(), #r1{}).

update_record1([Val|Ls], Acc=#r1{not_aliased0=N0,not_aliased1=N1}) ->
%ssa% (_, Rec) when post_ssa_opt ->
%ssa% _ = update_record(reuse, 3, Rec, 3, NA0, 2, NA1) {unique => [Rec, NA1, NA0], source_dies => true}.
    R = Acc#r1{not_aliased0=N0+1,not_aliased1=[Val|N1]},
    update_record1(Ls, R);
update_record1([], Acc) ->
    Acc.

live_past_call_triggers_aliasing(A) ->
%%% As X lives past the call to id, X and Y alias each other.
%ssa% (A) when post_ssa_opt ->
%ssa% X = put_tuple(A),
%ssa% Y = call(fun id/1, X) { aliased => [X] },
%ssa% R = put_tuple(X, Y) { aliased => [X,Y] },
%ssa% ret(R) { unique => [R] }.
    X = {A},
    Y = id(X),
    {X,Y}.

%% Check that the alias analysis handles the case where the called
%% function only has a known return status for a result type which is
%% not present in the call. In the example the looked up type is a
%% #t_union{} but a status is only known for 'nil'.
fuzz0(_V0)  ->
    maybe
        [] ?= _V0
    else
        _  when ok ->
            ok;
        []  ->
            ok;
        _  ->
            _V0
    end.

fuzz0()  ->
    fuzz0(ok).

alias_after_phi() ->
    alias_after_phi({e:f(),e:f()}).

alias_after_phi(X) ->
%% Check that X is aliased after the Phi.
%ssa% (Arg0) when post_ssa_opt ->
%ssa% Phi = phi({_,_}, {Arg0,_}, ...),
%ssa% _ = get_tuple_element(Arg0, 0) {aliased => [Arg0]}.
    {A,B} = X,
    T = case e:f() of
	    1 ->
		X;
	    2 ->
		{e:f(),e:f()}
	end,
    {A,B} = T,
    {A,B,X}.

%% Check that the identifier type is considered plain and therefore
%% unique.
check_identifier_type() ->
    R = {case e:f() of
	     X when is_port(X) ->
		 X;
	     X when is_pid(X) ->
		 X
	 end},
    should_return_unique(R).

should_return_unique({X}) ->
%ssa% (_) when post_ssa_opt ->
%ssa% ret(R) { unique => [R] }.
    X.
