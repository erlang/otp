%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2023. All Rights Reserved.
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
-module(appendable).
-export([make_empty/0, t0/0, t1/0, t2/0, t3/0, t4/0,
	 t5/0, t6/0, t7/0, t8/1, t9/1, t10/1, t11/1, t12/0, t13/0]).

%% Check that just returning an empty bitstring is considered
%% appendable.
t0() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(<<>>) { result_type => {t_bitstring,_,true} }.
    A = <<>>,
    A.

%% Check that appending an unknown bitstring to a literal <<>> results
%% in an appendable bitstring.
t1() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(_) { result_type => {t_bitstring,_,true} }.
    A = <<>>,
    B = ex:f(),
    <<A/binary, B/binary>>.

%% Check that appending an integer to a literal <<>> results in an
%% appendable bitstring.
t2() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(_) { result_type => {t_bitstring,_,true} }.
    A = <<>>,
    B = ex:f(),
    <<A/binary, B:32>>.

%% Check that just returning an empty bitstring is considered
%% appendable.
t3() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(_) { result_type => {t_bitstring,_,true} }.
    A = make_empty(),
    A.

%% Check that appending an unknown bitstring to a value known to be
%% <<>> results in an appendable bitstring.
t4() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(_) { result_type => {t_bitstring,_,true} }.
    A = make_empty(),
    B = ex:f(),
    <<A/binary, B/binary>>.

%% Check that appending an integer to a value known results in an
%% appendable bitstring.
t5() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(_) { result_type => {t_bitstring,_,true} }.
    A = make_empty(),
    B = ex:f(),
    <<A/binary, B:32>>.

%% Check that the appendable flag is cleared when we don't append
%% things to the bitstring accumulator.
t6() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(_) { result_type => {t_bitstring,_,false} }.
    A = <<>>,
    B = ex:f(),
    <<B/binary, A/binary>>.

%% Check that the appendable flag is cleared when we don't append
%% things to the bitstring accumulator.
t7() ->
%ssa% () when post_ssa_opt ->
%ssa% ret(_) { result_type => {t_bitstring,_,false} }.
    A = ex:f(),
    <<17:8, A/binary>>.

%% More complicated recursive check for setting the appendable flag
t8(Ls) ->
%ssa% (_) when post_ssa_opt ->
%ssa% ret(_) { result_type => {t_bitstring,_,true} }.
    t8(<<>>, Ls).

t8(Acc, [H|T]) ->
    t8(<<Acc/binary, H:32>>, T);
t8(Acc, []) ->
    Acc.

%% More complicated recursive check for when the appendable flag
%% should not be set.
t9(Ls) ->
%ssa% (_) when post_ssa_opt ->
%ssa% ret(_) { result_type => {t_bitstring,_,false} }.
    t9(<<>>, Ls).

t9(Acc, [H|T]) ->
    t9(<<H:32, Acc/binary>>, T);
t9(Acc, []) ->
    Acc.

%% More complicated recursive check for when the appendable flag
%% should be set.
t10(Ls) ->
%ssa% (_) when post_ssa_opt ->
%ssa% ret(_) { result_type => {t_bitstring,_,true} }.
    t10(<<>>, Ls).

t10(Acc, [H|T]) ->
    case H rem 2 of
	0 ->
	    t10(<<Acc/binary, H:16>>, T);
	1 ->
	    t10(<<Acc/binary, H:32>>, T)
    end;
t10(Acc, []) ->
    Acc.

%% More complicated recursive check for when the appendable flag
%% should not be set.
t11(Ls) ->
%ssa% (_) when post_ssa_opt ->
%ssa% ret(_) { result_type => {t_bitstring,_,false} }.
    t11(<<>>, Ls).

t11(Acc, [H|T]) ->
    case H rem 2 of
	0 ->
	    t11(<<H:32, Acc/binary>>, T);
	1 ->
	    t11(<<Acc/binary, H:32>>, T)
    end;
t11(Acc, []) ->
    Acc.

make_empty() ->
    <<>>.

%% beam_ssa_type:type/5 had problems handling bs_create_bin when the
%% first fragment was typed as a #t_union{} containing an appendable
%% bit string. Check that we don't lose append=true.
t12() ->
%ssa% () when post_ssa_opt ->
%ssa% A = call(fun t12_inner/1, ...),
%ssa% ret(A) { result_type => {t_cons,_,{t_bitstring,8,true}} },
%ssa% ret(_) { result_type => none }.
    t12_inner([x|<<>>]).

t12_inner([x|B]) ->
    [x|<<B/binary,1:8>>].

%% Check that the compiler doesn't infer anything about the appendable
%% status of a bitstring from a comparison.
t13() ->
%ssa% () when post_ssa_opt ->
%ssa% B = bif:binary_part(A, 0, 0),
%ssa% C = bif:'=:='(B, A) { arg_types => #{ 0 => {t_bitstring,8,false}, 1 => {t_bitstring,256,true}} },
%ssa% _ = bs_create_bin(append, _, B, ...) { arg_types => #{ 2 => {t_bitstring,256,false} } }.
   <<(_V4 = binary_part(_V4 = <<0 || _ <- []>>, 0, 0))/bitstring>>.
