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

-compile(no_ssa_opt_private_append).

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
         gh6925/0]).

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
%ssa% _ = bs_create_bin(append, _, Arg1, _, _, _, X, _) { aliased => [X], unique => [Arg1], first_fragment_dies => true }.
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
%ssa% _ = bs_create_bin(append, _, A, _, _, _, B, _) { aliased => [B, A], first_fragment_dies => true },
%ssa% _ = bs_create_bin(append, _, C, _, _, _, D, _) { aliased => [D, C], first_fragment_dies => true }.
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

%% We should use type information to figure out that {<<>>, X} is not
%% aliased, but as of now we don't have the information at this pass,
%% nor do we track alias status at the sub-term level.
transformable18(L, X) when is_integer(X), X < 256 ->
    transformable18b(L, {<<>>, X}).

transformable18b([H|T], {Acc,X}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tuple_element(Arg1, 0),
%ssa% B = bs_create_bin(append, _, A, _, _, _, X, _) { aliased => [A], unique => [X], first_fragment_dies => true },
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
    transformable24(L, {<<>>, ex:foo()}).

transformable24([H|T], {Acc,X}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tuple_element(Arg1, 0),
%ssa% B = bs_create_bin(append, _, A, _, _, _, X, _) { aliased => [A], unique => [X], first_fragment_dies => true },
%ssa% C = put_tuple(B, _),
%ssa% _ = call(fun transformable24/2, _, C).
    transformable24(T, {<<Acc/binary, (H+X):8>>, X});
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
