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
%% This module tests that the beam_ssa_destructive_update pass
%% rewrites plain appends in bs_create_bin to private_append when
%% appropriate.
%%
-module(private_append).

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
	 transformable18/2,
	 transformable19/1,
	 transformable20/1,
	 transformable21/1,
	 transformable22/1,
	 transformable23/1,
	 transformable24/1,
	 transformable25/1,
	 transformable26/1,
	 transformable27/1,
	 transformable28/1,
	 transformable29/1,
	 transformable30/1,
	 transformable31a/1,
	 transformable31b/1,
         transformable32/0,
         transformable32/1,
         transformable33/0,

	 not_transformable1/2,
	 not_transformable2/1,
	 not_transformable3/1,
	 not_transformable4/1,
	 not_transformable5/1,
	 not_transformable6/1,
	 not_transformable7/1,
	 not_transformable8/1,
	 not_transformable9/1,
	 not_transformable10/1,
         not_transformable11/0,
         not_transformable12/1,
         not_transformable13/1,
         not_transformable14/0,
         not_transformable15/2,

         id/1,

         bs_create_bin_on_literal/0,

         crash_in_value_tracking/3,
         crash_in_value_tracking_inner/3]).

%% Trivial smoke test
transformable0(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% _ = call(fun transformable0/2, _, A).
    transformable0(L, <<>>).

transformable0([H|T], Acc) ->
%ssa% (_, Acc) when post_ssa_opt ->
%ssa% _ = bs_create_bin(private_append, _, Acc, ...).
    transformable0(T, <<Acc/binary, H:8>>);
transformable0([], Acc) ->
    Acc.

%% Check that the transform works when the binary is produced by a
%% non-trivial constructions. (transformable{1,2,3,4})
transformable1(L) ->
    transformable1(L, start).

transformable1(L, start) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% _ = call(fun transformable1/2, _, A),
%ssa% _ = bs_create_bin(private_append, _, Arg1, ...).
    transformable1(L, <<>>);
transformable1([H|T], Acc) ->
    transformable1(T, <<Acc/binary, H:8>>);
transformable1([], Acc) ->
    Acc.

transformable1b(L) ->
    transformable1b(L, start).

transformable1b([H|T], X) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% Phi = phi({Arg1, _}, {A, _}, ...),
%ssa% _ = bs_create_bin(private_append, _, Phi, ...).
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
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% _ = call(fun transformable2/2, _, A).
    transformable2(L, <<>>).

transformable2([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = bs_create_bin(private_append, _, Arg1, ...),
%ssa% _ = call(fun transformable2/2, _, A),
%ssa% B = bs_init_writable(_),
%ssa% _ = call(fun transformable2/2, _, B).
    case ex:f() of
	true ->
	    transformable2(T, <<Acc/binary, H:8>>);
	false ->
	    transformable2(T, <<>>)
    end;
transformable2([], Acc) ->
    Acc.

transformable3(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% _ = call(fun transformable3/2, _, A).
    transformable3(L, <<>>).

transformable3([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = bs_create_bin(private_append, _, Arg1, ...),
%ssa% B = bs_init_writable(_),
%ssa% Phi = phi({A, _}, {B, _}, ...),
%ssa% _ = call(fun transformable3/2, _, Phi).
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
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% _ = call(fun transformable4/2, _, A).
    transformable4(L, <<>>).

transformable4([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% Phi = phi({A, _}, {Arg1, _}, ...),
%ssa% B = bs_create_bin(private_append, _, Phi, ...),
%ssa% _ = call(fun transformable4/2, _, B).
    R = case ex:f() of
	    true ->
		Acc;
	    false ->
		<<>>
	end,
    transformable4(T, <<R/binary, H:8>>);
transformable4([], Acc) ->
    Acc.

%% Check that when a variable is live across a call it is considered
%% aliased. If the alias analysis is extended to track if an argument
%% is captured by the callee, that information could be fed back to
%% the caller. Until that is done, this test is expected to fail.
transformable5(L) ->
%ssa% xfail (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% _ = call(fun transformable5/2, _, A).
    transformable5(L, <<>>).

transformable5([H|T], Acc) ->
%ssa% xfail (_, Arg1) when post_ssa_opt ->
%ssa% A = bs_create_bin(private_append, _, Arg1, ...),
%ssa% _ = call(fun transformable5/2, _, A).
    does_not_escape(Acc),
    transformable5(T, <<Acc/binary, H:8>>);
transformable5([], Acc) ->
    Acc.

does_not_escape(_) ->
    ok.

%% Check that the transform works when we have an appendable binary in
%% the head of a cons.
transformable7(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% B = put_list(A, _),
%ssa% _ = call(fun transformable7/2, _, B).
    transformable7(L, [<<>>|0]).

transformable7([H|T], [Acc|N]) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_hd(Arg1),
%ssa% B = bs_create_bin(private_append, _, A, ...),
%ssa% C = put_list(B, _),
%ssa% _ = call(fun transformable7/2, _, C).
    transformable7(T, [<<Acc/binary, H:8>>|N+1]);
transformable7([], Acc) ->
    Acc.

%% Check that the transform works when we have an appendable binary in
%% just one of the clauses.
transformable8(L) ->
    transformable8(L, start).

transformable8(L, start) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% B = bs_create_bin(private_append, _, Arg1, ...),
%ssa% _ = call(fun transformable8b/2, _, B).
    transformable8(L, <<>>);
transformable8([H|T], Acc) ->
    transformable8b(T, <<Acc/binary, H:8>>);
transformable8([], Acc) ->
    Acc.

transformable8b(T, Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = bs_create_bin(private_append, _, Arg1, ...),
%ssa% _ = call(fun transformable8/2, _, B).
    transformable8(T, <<Acc/binary, 16#ff:8>>).

%% Check that the transform works across mutually recursive functions.
transformable9(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% _ = call(fun transformable9a/2, _, A).
    transformable9a(L, <<>>).

transformable9a([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = bs_create_bin(private_append, _, Arg1, ...),
%ssa% _ = call(fun transformable9b/2, _, B).
    transformable9b(T, <<Acc/binary, 0:8, H:8>>);
transformable9a([], Acc) ->
    Acc.

transformable9b([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = bs_create_bin(private_append, _, Arg1, ...),
%ssa% _ = call(fun transformable9a/2, _, B).
    transformable9a(T, <<Acc/binary, 1:8, H:8>>);
transformable9b([], Acc) ->
    Acc.

%% Check that the transform works for binaries embedded in a literal.
transformable10(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% B = put_tuple(A, ...),
%ssa% _ = call(fun transformable10/2, _, B).
    transformable10(L, {<<>>,0}).

transformable10([H|T], {Acc,N}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tuple_element(Arg1, 0),
%ssa% B = bs_create_bin(private_append, _, A, ...),
%ssa% C = put_tuple(B, _),
%ssa% _ = call(fun transformable10/2, _, C).
    transformable10(T, {<<Acc/binary, H:8>>,N+1});
transformable10([], Acc) ->
    Acc.

%% Check that the transform works across clauses
transformable11(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% _ = call(fun transformable11/2, _, A).
    transformable11(L, <<>>).

transformable11([H|T], Acc) when H =:= 0 ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = bs_create_bin(private_append, _, Arg1, ...),
%ssa% _ = call(fun transformable11/2, _, A),
%ssa% B = bs_create_bin(private_append, _, Arg1, ...),
%ssa% _ = call(fun transformable11/2, _, B).
    transformable11(T, <<Acc/binary, 0:8>>);
transformable11([_|T], Acc)->
    transformable11(T, <<Acc/binary, 1:8>>);
transformable11([], Acc) ->
    Acc.

transformable12a(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% B = put_tuple(A),
%ssa% _ = call(fun transformable12/2, _, B).
    transformable12(L, {<<>>}).

transformable12b(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% B = put_list(A, _),
%ssa% _ = call(fun transformable12/2, _, B).
    transformable12(L, [<<>>]).

%% The type analysis can't handle the list yet
transformable12([H|T], {Acc}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_hd(Arg1),
%ssa% B = bs_create_bin(private_append, _, A, ...),
%ssa% C = put_list(B, _),
%ssa% _ = call(fun transformable12/2, _, C),
%ssa% D = get_tuple_element(Arg1, 0),
%ssa% E = bs_create_bin(private_append, _, D, ...),
%ssa% F = put_tuple(E),
%ssa% _ = call(fun transformable12/2, _, F).
    transformable12([H|T], {<<Acc/binary,H:8>>});
transformable12([H|T], [Acc]) ->
    transformable12([H|T], [<<Acc/binary,H:8>>]);
transformable12([], {Acc}) ->
    Acc;
transformable12([], [Acc]) ->
    Acc.

%% Check binaries coming from another function.
transformable13(L) ->
%ssa% (Arg0) when post_ssa_opt ->
%ssa% A = call(fun make_empty_binary/0),
%ssa% _ = call(fun transformable13/2, _, A).
    transformable13(L, make_empty_binary()).

transformable13([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = bs_create_bin(private_append, _, Arg1, ...),
%ssa% _ = call(fun transformable13/2, _, A).
    transformable13(T, <<Acc/binary, H:8>>);
transformable13([], Acc) ->
    Acc.

make_empty_binary() ->
%ssa% () when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% ret(A).
    <<>>.

%% Check binaries coming from other functions, with various levels of
%% nesting in the literals and how they are picked apart using
%% matching.
transformable14(L) ->
%ssa% (Arg0) when post_ssa_opt ->
%ssa% A = call(fun make_wrapped_empty_binary/0),
%ssa% B = get_tuple_element(A, 0),
%ssa% _ = call(fun transformable14/2, _, B).
    {X} = make_wrapped_empty_binary(),
    transformable14(L, X).

transformable14([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = bs_create_bin(private_append, _, Arg1, ...),
%ssa% _ = call(fun transformable14/2, _, A).
    transformable14(T, <<Acc/binary, H:8>>);
transformable14([], Acc) ->
    Acc.

make_wrapped_empty_binary() ->
%ssa% () when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% B = put_tuple(A),
%ssa% ret(B).
    {<<>>}.

transformable15(L) ->
%ssa% (Arg0) when post_ssa_opt ->
%ssa% A = call(fun make_wrapped_empty_binary/0),
%ssa% B = call(fun make_empty_binary/0),
%ssa% C = get_tuple_element(A, 0),
%ssa% _ = call(fun transformable15/3, Arg0, C, B).
    {X} = make_wrapped_empty_binary(),
    Y = make_empty_binary(),
    transformable15(L, X, Y).

transformable15([A,B|T], Acc0, Acc1) ->
%ssa% (_, Arg1, Arg2) when post_ssa_opt ->
%ssa% A = bs_create_bin(private_append, _, Arg1, ...),
%ssa% B = bs_create_bin(private_append, _, Arg2, ...),
%ssa% _ = call(fun transformable15/3, _, A, B).

    transformable15(T, <<Acc0/binary, A:8>>, <<Acc1/binary, B:8>>);
transformable15([], Acc0, Acc1) ->
    {Acc0,Acc1}.

transformable16(L) ->
%ssa% (Arg0) when post_ssa_opt ->
%ssa% A = call(fun make_wrapped_empty_binary/0),
%ssa% B = call(fun make_empty_binary/0),
%ssa% C = put_tuple(A, B),
%ssa% _ = call(fun transformable16/2, Arg0, C).
    X = make_wrapped_empty_binary(),
    Y = make_empty_binary(),
    transformable16(L, {X, Y}).

transformable16([A,B|T], {{Acc0}, Acc1}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tuple_element(Arg1, 0),
%ssa% B = get_tuple_element(A, 0),
%ssa% C = bs_create_bin(private_append, _, B, ...),
%ssa% D = get_tuple_element(Arg1, 1),
%ssa% E = bs_create_bin(private_append, _, D, ...),
%ssa% F = put_tuple(C),
%ssa% G = put_tuple(F, E),
%ssa% _ = call(fun transformable16/2, _, G).
    transformable16(T, {{<<Acc0/binary, A:8>>}, <<Acc1/binary, B:8>>});
transformable16([], {{Acc0}, Acc1}) ->
    {Acc0,Acc1}.

%% Check that type information is used to figure out that {<<>>, X} is
%% not aliased.
transformable18(L, X) when is_integer(X), X < 256 ->
%ssa% (_, _) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% B = put_tuple(A, _),
%ssa% _ = call(fun transformable18b/2, _, B).
    transformable18b(L, {<<>>, X}).

transformable18b([H|T], {Acc,X}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tuple_element(Arg1, 0),
%ssa% B = bs_create_bin(private_append, _, A, ...),
%ssa% C = put_tuple(B, _),
%ssa% _ = call(fun transformable18b/2, _, C).
    transformable18b(T, {<<Acc/binary, (H+X):8>>, X});
transformable18b([], {Acc,_}) ->
    Acc.

%% Check that the conversion works when the binary isn't embedded in a
%% tuple literal.
transformable19(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% B = put_tuple(A, _),
%ssa% _ = call(fun transformable19b/2, _, B).
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
%ssa% B = bs_create_bin(private_append, _, A, ...),
%ssa% C = put_tuple(B, _),
%ssa% _ = call(fun transformable19b/2, _, C).
    transformable19b(T, {<<Acc/binary, (H+X):8>>, X});
transformable19b([], {Acc,_}) ->
    Acc.

%% Check that the conversion works when the binary isn't embedded in a
%% list literal.
transformable20(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% B = put_list(A, _),
%ssa% _ = call(fun transformable20b/2, _, B).
    X = case ex:foo() of
	    true ->
		4711;
	    false ->
		17
	end,
    transformable20b(L, [<<>>|X]). % XXXX

transformable20b([H|T], [Acc|X]) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_hd(Arg1),
%ssa% B = bs_create_bin(private_append, _, A, ...),
%ssa% C = put_list(B, _),
%ssa% _ = call(fun transformable20b/2, _, C).
    transformable20b(T, [<<Acc/binary, (H+X):8>>|X+1]);
transformable20b([], [Acc|_]) ->
    Acc.

%% Check that the conversion works when the binary is embedded in a
%% tuple literal returned from another function.
transformable21(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = call(fun make_empty_binary_tuple/0),
%ssa% _ = call(fun transformable21/2, _, A).
    transformable21(L, make_empty_binary_tuple()).

transformable21([H|T], {AccA,AccB}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tuple_element(Arg1, 0),
%ssa% B = bs_create_bin(private_append, _, A, ...),
%ssa% C = get_tuple_element(Arg1, 1),
%ssa% D = bs_create_bin(private_append, _, C, ...),
%ssa% E = put_tuple(B, D),
%ssa% _ = call(fun transformable21/2, _, E).
    transformable21(T, {<<AccA/binary, H:8>>,<<AccB/binary, 17:8>>});
transformable21([], {AccA, AccB}) ->
    {AccA, AccB}.

make_empty_binary_tuple() ->
%ssa% () when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% B = bs_init_writable(_),
%ssa% C = put_tuple(A, B),
%ssa% ret(C).
    {<<>>, <<>>}.

%% Check that the conversion works for the first element of the list.
%% Type analysis does not understand that the second field of the cons
%% isn't an improper list. As the private append variant of
%% bs_create_bin can't deal with non-bitstrings, check that we
%% conservatively refuse to rewrite the second element of the cons.
transformable22(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% C = put_list(A, _),
%ssa% _ = call(fun transformable22/2, _, C).
    transformable22(L, [<<>>|<<>>]).

transformable22([H|T], [AccA|AccB]) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_hd(Arg1),
%ssa% B = get_tl(Arg1),
%ssa% C = bs_create_bin(private_append, _, A, ...),
%ssa% D = bs_create_bin(append, _, B, ...),
%ssa% E = put_list(C, D),
%ssa% _ = call(fun transformable22/2, _, E).
    transformable22(T, [<<AccA/binary, H:8>>|<<AccB/binary, 17:8>>]);
transformable22([], Acc) ->
    Acc.

%% As transformable21 but with a more complex embedded tuple
transformable23(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = call(fun make_empty_binary_tuple_nested/0),
%ssa% _ = call(fun transformable23/2, _, A).
    transformable23(L, make_empty_binary_tuple_nested()).

transformable23([H|T], {AccA,{AccB},X}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tuple_element(Arg1, 0),
%ssa% B = bs_create_bin(private_append, _, A, ...),
%ssa% C = get_tuple_element(Arg1, 1),
%ssa% D = get_tuple_element(C, 0),
%ssa% E = bs_create_bin(private_append, _, D, ...),
%ssa% F = put_tuple(E),
%ssa% G = put_tuple(B, F, _),
%ssa% _ = call(fun transformable23/2, _, G).
    transformable23(T, {<<AccA/binary, H:8>>,{<<AccB/binary, 17:8>>}, X});
transformable23([], {AccA, AccB, X}) ->
    {AccA, AccB, X}.

make_empty_binary_tuple_nested() ->
%ssa% () when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% B = bs_init_writable(_),
%ssa% C = put_tuple(B),
%ssa% D = put_tuple(A, C, _),
%ssa% ret(D).
    {<<>>, {<<>>}, 47}.

transformable24(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% B = put_tuple(A, _),
%ssa% _ = call(fun transformable24/2, _, B).
    transformable24(L, {<<>>, ex:foo()}).

transformable24([H|T], {Acc,X}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tuple_element(Arg1, 0),
%ssa% B = bs_create_bin(private_append, _, A, ...),
%ssa% C = put_tuple(B, _),
%ssa% _ = call(fun transformable24/2, _, C).
    transformable24(T, {<<Acc/binary, (H+X):8>>, X});
transformable24([], {Acc,_}) ->
    Acc.

%% Check that the update of more than one element of a tuple is
%% handled.
transformable25(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% B = bs_init_writable(_),
%ssa% C = put_tuple(A, B),
%ssa% _ = call(fun transformable25/2, _, C).
    transformable25(L, {<<>>,<<>>}).

transformable25([H|T], {AccA,AccB}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tuple_element(Arg1, 0),
%ssa% B = bs_create_bin(private_append, _, A, ...),
%ssa% C = get_tuple_element(Arg1, 1),
%ssa% D = bs_create_bin(private_append, _, C, ...),
%ssa% E = put_tuple(B, D),
%ssa% _ = call(fun transformable25/2, _, E).
    transformable25(T, {<<AccA/binary, H:8>>,<<AccB/binary>>});
transformable25([], Acc) ->
    Acc.

%% Check that the update of more than two elements of a tuple is
%% handled (check the inductive step of
%% beam_ssa_alias_opt:merge_arg_patches/1 works).
transformable26(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% B = bs_init_writable(_),
%ssa% C = bs_init_writable(_),
%ssa% D = put_tuple(A, B, C),
%ssa% _ = call(fun transformable26/2, _, D).
    transformable26(L, {<<>>,<<>>,<<>>}).

transformable26([H|T], {AccA,AccB,AccC}) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tuple_element(Arg1, 0),
%ssa% B = bs_create_bin(private_append, _, A, ...),
%ssa% C = get_tuple_element(Arg1, 1),
%ssa% D = bs_create_bin(private_append, _, C, ...),
%ssa% E = get_tuple_element(Arg1, 2),
%ssa% F = bs_create_bin(private_append, _, E, ...),
%ssa% G = put_tuple(B, D, F),
%ssa% _ = call(fun transformable26/2, _, G).
    transformable26(T, {<<AccA/binary, H:8>>,
			<<AccB/binary, H:8>>,
			<<AccC/binary, H:8>>});
transformable26([], Acc) ->
    Acc.

%% Check that we allow the transform when we append a multiple of 8
%% bits.
transformable27(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% _ = call(fun transformable27/2, _, A).
    transformable27(L, <<>>).

transformable27([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(private_append, _, Arg1, ...).
    transformable27(T, <<Acc/binary, H:7, 1:1>>);
transformable27([], Acc) ->
    Acc.

transformable28(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% _ = call(fun transformable28/2, _, A).
    transformable28(L, <<>>).

transformable28([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(private_append, _, Arg1, ...).
    transformable28(T, <<Acc/binary, H:23/bitstring, 1:1>>);
transformable28([], Acc) ->
    Acc.

transformable29(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% _ = call(fun transformable29/2, _, A).
    transformable29(L, <<>>).

transformable29([A,B|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(private_append, _, Arg1, ...).
    transformable29(T, <<Acc/binary, A:23/bitstring, B/float, 1:9>>);
transformable29([], Acc) ->
    Acc.

%% As transformable22, but the accumulator is deconstructed in the
%% reverse order, tail-head instead of head-tail.
transformable30(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% C = put_list(A, _),
%ssa% _ = call(fun transformable30/2, _, C).
    transformable30(L, [<<>>|<<>>]).

transformable30([H|T], X) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% B = get_tl(Arg1),
%ssa% A = get_hd(Arg1),
%ssa% C = bs_create_bin(private_append, _, A, ...),
%ssa% D = bs_create_bin(append, _, B, ...),
%ssa% E = put_list(C, D),
%ssa% _ = call(fun transformable30/2, _, E).
    AccB = tl(X),
    AccA = hd(X),
    transformable30(T, [<<AccA/binary, H:8>>|<<AccB/binary, 17:8>>]);
transformable30([], Acc) ->
    Acc.

%% The private-append transform detects that the accumulator can be
%% transformed when the third argument to transformable31/3 is
%% 'b'. This test case checks that the transform doesn't try to
%% rewrite the {a,b} tuple (which lead to a compiler crash).
transformable31a(L) ->
    transformable31(L, <<>>, a).

transformable31b(L) ->
    transformable31(L, <<>>, b).

transformable31([H|T], Acc, a) when is_binary(Acc) ->
    transformable31(T, <<Acc/binary, H:8>>, a);
transformable31([_|T], _Acc, b) ->
    transformable31(T, {a,b}, b);
transformable31([], Acc, a) when is_binary(Acc) ->
    Acc;
transformable31([], Acc, b) when is_tuple(Acc) ->
    <<>>.

%% Check that we don't crash (Github issue #6847) while attempting to
%% patch the empty list, but also that the literal <<>> becomes a
%% bs_init_writable.
transformable32() ->
    <<(transformable32(ok))/binary>>.

transformable32(#{}) ->
%ssa% (_) when post_ssa_opt ->
%ssa% A = bs_init_writable(_),
%ssa% ret(A).
    [];
transformable32(_) ->
    <<>>.

%% Check that we don't crash (Github issue #6999) while attempting to
%% patch the empty list, but also that Dest is created with private_append.
transformable33() ->
%ssa% () when post_ssa_opt ->
%ssa% _ = bs_create_bin(private_append, ...).
    [F01] = [transformable33_inner(<<"0">>) || _ <- [1]],
    Dest = <<F01/binary>>,
    Dest.

transformable33_inner(V) ->
    << <<C>> || <<C:4>> <= V >>.

%% Check that calling an external function with append result doesn't
%% prevent private_append optimization.
transformable34() ->
    transformable34a(<<>>).

transformable34a(Acc) ->
%ssa% (Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(private_append, _, Arg1, ...).
    Value = <<Acc/binary, 1>>,
    ex:escape(Value).

% Should not be transformed as we can't know the alias status of Acc
not_transformable1([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, ...).
    not_transformable1(T, <<Acc/binary, H:8>>);
not_transformable1([], Acc) ->
    Acc.

% Should not be transformed as references to the binary can escape in
% ex:escape/1.
not_transformable2(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% _ = call(fun not_transformable2/2, _, <<>>).
    not_transformable2(L, <<>>).

not_transformable2([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, ...).
    ex:escape(Acc),
    not_transformable2(T, <<Acc/binary, H:8>>);
not_transformable2([], Acc) ->
    Acc.

% Should not be transformed as we create and preserve multiple
% references to the binary.
not_transformable3(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% _ = call(fun not_transformable3/3, _, <<>>, _).
    not_transformable3(L, <<>>, []).

not_transformable3([H|T], Acc, Ls) ->
%ssa% (_, Arg1, _) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, ...).
    not_transformable3(T, <<Acc/binary, H:8>>, [Acc|Ls]);
not_transformable3([], Acc, Ls) ->
    {Acc, Ls}.

%% We randomly keep multiple references to the binary.
not_transformable4(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% _ = call(fun not_transformable4/2, _, [<<>>]).
    not_transformable4(L, [<<>>|[]]).

not_transformable4([H|T], X=[Acc|Ls]) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, ...).
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
%ssa% (_) when post_ssa_opt ->
%ssa% _ = call(fun not_transformable5a/2, _, <<>>).
    not_transformable5a(L, <<>>).

not_transformable5a([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, ...).
    not_transformable5b(T, <<Acc/binary, 0:8, H:8>>);
not_transformable5a([], Acc) ->
    Acc.

not_transformable5b([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, ...).
    ex:alias(Acc),
    not_transformable5a(T, <<Acc/binary, 1:8, H:8>>);
not_transformable5b([], Acc) ->
    Acc.

%% Check that we're not trying to build binaries which are not a
%% multiple of 8 bits.
not_transformable6(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% _ = call(fun not_transformable6/2, _, <<>>).
    not_transformable6(L, <<>>).

not_transformable6([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, ...).
    not_transformable6(T, <<Acc/binary, H:1>>);
not_transformable6([], Acc) ->
    Acc.

not_transformable7(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% _ = call(fun not_transformable7/2, _, <<>>).
    not_transformable7(L, <<>>).

not_transformable7([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, ...).
    not_transformable7(T, <<Acc/binary, H:H>>);
not_transformable7([], Acc) ->
    Acc.

not_transformable8(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% _ = call(fun not_transformable8/2, _, <<>>).
    not_transformable8(L, <<>>).

not_transformable8([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, ...).
    not_transformable8(T, <<Acc/binary, H/float, 15:4>>);
not_transformable8([], Acc) ->
    Acc.

not_transformable9(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% _ = call(fun not_transformable9/2, _, <<>>).
    not_transformable9(L, <<>>).

not_transformable9([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, ...).
    not_transformable9(T, <<Acc/binary, H:15/binary, 15:4>>);
not_transformable9([], Acc) ->
    Acc.

not_transformable10(L) ->
%ssa% (_) when post_ssa_opt ->
%ssa% _ = call(fun not_transformable10/2, _, <<>>).
    not_transformable10(L, <<>>).

not_transformable10([H|T], Acc) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, _, Arg1, ...).
    not_transformable10(T, <<Acc/binary, H:15/bitstring>>);
not_transformable10([], Acc) ->
    Acc.

%% Check that we don't transform when we can't guarantee that the
%% first fragment to bs_create_bin is a bitstring.
not_transformable11() ->
%ssa% fail () when post_ssa_opt ->
%ssa% _ = bs_init_writable(_).
    not_transformable11(not_ok).

not_transformable11(X) ->
%ssa% (_) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, ...).
    <<(case ok of
           X -> <<>>;
           _ -> ok
       end)/bytes>>.

%% Check that we don't transform when we can't guarantee that the
%% first fragment to bs_create_bin is a bitstring.
not_transformable12(A) ->
%ssa% (_) when post_ssa_opt ->
%ssa% _ = bs_create_bin(append, ...).
    <<(case ok of
           A ->
               not is_alive();
           _  ->
               <<>>
       end)/bitstring>>.

%% The order of fields in the accumulator is swapped compared to
%% transformable7. Type analysis does not understand that the second
%% field of the cons isn't an improper list. As the private append
%% variant of bs_create_bin can't deal with non-bitstrings, check that
%% we conservatively refuse to rewrite this case.
not_transformable13(L) ->
    not_transformable13(L, [0|<<>>]).

not_transformable13([H|T], [N|Acc]) ->
%ssa% (_, Arg1) when post_ssa_opt ->
%ssa% A = get_tl(Arg1),
%ssa% B = bs_create_bin(append, _, A, ...),
%ssa% C = put_list(_, B),
%ssa% _ = call(fun not_transformable13/2, _, C).
    not_transformable13(T, [N+1|<<Acc/binary, H:8>>]);
not_transformable13([], Acc) ->
    Acc.

%% Check that we don't try to transform appends into private_appends
%% when the first fragment doesn't die with the
%% bs_create_bin. GH-6925.
not_transformable14() ->
%ssa% () when post_ssa_opt ->
%ssa% A = bs_create_bin(private_append, ...),
%ssa% B = bs_create_bin(append, ...).
    A = << <<"x">> || true >>,
    B = <<A/binary, "z">>,
    {A, B}.


%% Check that we don't crash in cases where tracking a value ends up
%% in operations through which we can't continue tracking. GH-7011.
not_transformable15(V, _) when V ->
%ssa% (_, _) when post_ssa_opt ->
%ssa% _ = bs_init_writable(_),
%ssa% B = call(fun not_transformable15/2, _, _),
%ssa% _ = bs_create_bin(private_append, _, B, ...).
    << ok || catch <<(not_transformable15(id(ok), ok))/binary>> >>;
not_transformable15(_, V) ->
    id(ok) bor V.

id(I) ->
    I.

%% Check that we don't try to private_append to something created by
%% bs_create_bin `append`, _, `<<>>`, ...
bs_create_bin_on_literal() ->
%ssa% () when post_ssa_opt ->
%ssa% X = bs_init_writable(_),
%ssa% Y = bs_create_bin(private_append, _, X, ...),
%ssa% Z = bs_create_bin(private_append, _, Y, ...),
%ssa% ret(Z).
    <<
      <<
	(maybe
	     2147483647 ?= ok
	 else
	     <<_>> ->
		 ok;
	     _ ->
		 <<>>
	 end)/bytes
      >>/binary
    >>.

%% Check that the beam_ssa_destructive_update pass doesn't crash, if
%% it, during initial value tracking, ends up in operations which do
%% not create bit strings. This can happen as the initial value
%% tracking in beam_ssa_destructive_update doesn't consider types. As
%% the decision to apply the private append transform is using type
%% information, tracking values into not type-compatible execution
%% paths is harmless.
crash_in_value_tracking_inner(_, 1.0, _) ->
%ssa% (_, _, _) when post_ssa_opt ->
%ssa% _ = bs_init_writable(_).
    (<<>>);
crash_in_value_tracking_inner(_V1, _, _) when _V1 ->
    _V1.

crash_in_value_tracking(_, _V0, _) ->
%ssa% (_, _, _) when post_ssa_opt ->
%ssa% _ = bs_create_bin(private_append, ...).
    ((<<((crash_in_value_tracking_inner(
            {#{#{ ok => ok || _ := _ <- ok} => ok},
             _V0, false, _V0, "Bo"}, _V0, ok)))/bytes>>) =/= ok).
