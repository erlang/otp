%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2022-2025. All Rights Reserved.
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
%% Purpose: Calculate tight bounds for integer operations.
%%
%% Reference:
%%
%%    Henry S. Warren, Jr. Hacker's Delight (2 ed). Addison Wesley -
%%    Pearson Education, Inc. Chapter 4. Arithmetic Bounds.
%%
%%
-module(beam_bounds).
-moduledoc false.
-export([bounds/2, bounds/3, relop/3, infer_relop_types/3,
         is_masking_redundant/2]).
-export_type([range/0]).

-type range() :: {integer(), integer()} |
                 {'-inf', integer()} |
                 {integer(), '+inf'} |
                 'any'.
-type range_result() :: range() | 'any' | 'none'.
-type relop() :: '<' | '=<' | '>' | '>='.
-type bool_result() :: 'true' | 'false' | 'maybe'.
-type op() :: atom().

%% Maximum size of integers in bits to keep ranges for.
-define(NUM_BITS, 128).

-spec bounds(op(), range()) -> range_result().

bounds('bnot', R0) ->
    case R0 of
        {A,B} ->
            R = {inf_add(inf_neg(B), -1), inf_add(inf_neg(A), -1)},
            normalize(R);
        _ ->
            any
    end;
bounds(abs, R) ->
    case R of
        {A,B} when is_integer(A), is_integer(B) ->
            Min = 0,
            Max = max(abs(A), abs(B)),
            {Min,Max};
        _ ->
            {0,'+inf'}
    end.

-spec bounds(op(), range(), range()) -> range_result().

bounds('+', R1, R2) ->
    case {R1,R2} of
        {{A,B}, {C,D}} when abs(A) bsr ?NUM_BITS =:= 0,
                            abs(B) bsr ?NUM_BITS =:= 0,
                            abs(C) bsr ?NUM_BITS =:= 0,
                            abs(D) bsr ?NUM_BITS =:= 0 ->
            normalize({A+C,B+D});
        {{'-inf',B}, {_C,D}} when abs(B) bsr ?NUM_BITS =:= 0,
                                  abs(D) bsr ?NUM_BITS =:= 0 ->
            normalize({'-inf',B+D});
        {{_A,B}, {'-inf',D}} when abs(B) bsr ?NUM_BITS =:= 0,
                                  abs(D) bsr ?NUM_BITS =:= 0 ->
            normalize({'-inf',B+D});
        {{A,'+inf'}, {C,_D}} when abs(A) bsr ?NUM_BITS =:= 0,
                                  abs(C) bsr ?NUM_BITS =:= 0 ->
            normalize({A+C,'+inf'});
        {{A,_B}, {C,'+inf'}} when abs(A) bsr ?NUM_BITS =:= 0,
                                  abs(C) bsr ?NUM_BITS =:= 0 ->
            normalize({A+C,'+inf'});
        {_, _} ->
            any
    end;
bounds('-', R1, R2) ->
    case {R1,R2} of
        {{A,B}, {C,D}} when abs(A) bsr ?NUM_BITS =:= 0,
                            abs(B) bsr ?NUM_BITS =:= 0,
                            abs(C) bsr ?NUM_BITS =:= 0,
                            abs(D) bsr ?NUM_BITS =:= 0 ->
            normalize({A-D,B-C});
        {{A,'+inf'}, {_C,D}} when abs(A) bsr ?NUM_BITS =:= 0,
                                  abs(D) bsr ?NUM_BITS =:= 0 ->
            normalize({A-D,'+inf'});
        {{_A,B}, {C,'+inf'}} when abs(B) bsr ?NUM_BITS =:= 0,
                                  abs(C) bsr ?NUM_BITS =:= 0 ->
            normalize({'-inf',B-C});
        {{'-inf',B}, {C,_D}} when abs(B) bsr ?NUM_BITS =:= 0,
                                  abs(C) bsr ?NUM_BITS =:= 0 ->
            normalize({'-inf',B-C});
        {{A,_B}, {'-inf',D}} when abs(A) bsr ?NUM_BITS =:= 0,
                                  abs(D) bsr ?NUM_BITS =:= 0 ->
            normalize({A-D,'+inf'});
        {_, _} ->
            any
    end;
bounds('*', R1, R2) ->
    case {R1,R2} of
        {{A,B}, {C,D}} when abs(A) bsr ?NUM_BITS =:= 0,
                            abs(B) bsr ?NUM_BITS =:= 0,
                            abs(C) bsr ?NUM_BITS =:= 0,
                            abs(D) bsr ?NUM_BITS =:= 0 ->
            All = [X * Y || X <- [A,B], Y <- [C,D]],
            Min = lists:min(All),
            Max = lists:max(All),
            normalize({Min,Max});
        {{A,'+inf'}, {C,'+inf'}} when abs(A) bsr ?NUM_BITS =:= 0, A >= 0,
                                      abs(C) bsr ?NUM_BITS =:= 0, C >= 0 ->
            {A*C,'+inf'};
        {{A,'+inf'}, {C,D}} when abs(A) bsr ?NUM_BITS =:= 0,
                                 abs(C) bsr ?NUM_BITS =:= 0,
                                 abs(D) bsr ?NUM_BITS =:= 0,
                                 C >= 0 ->
            {min(A*C, A*D),'+inf'};
        {{'-inf',B}, {C,D}} when abs(B) bsr ?NUM_BITS =:= 0,
                                 abs(C) bsr ?NUM_BITS =:= 0,
                                 abs(D) bsr ?NUM_BITS =:= 0,
                                 C >= 0 ->
            {'-inf',max(B*C, B*D)};
        {{A,B}, {'-inf',_}} when is_integer(A), is_integer(B) ->
            bounds('*', R2, R1);
        {{A,B}, {_,'+inf'}} when is_integer(A), is_integer(B) ->
            bounds('*', R2, R1);
        {_, _} ->
            any
    end;
bounds('div', R1, R2) ->
    div_bounds(R1, R2);
bounds('rem', R1, R2) ->
    rem_bounds(R1, R2);
bounds('band', R1, R2) ->
    case {R1,R2} of
        {{A,B}, {C,D}} when A bsr ?NUM_BITS =:= 0, A >= 0,
                            C bsr ?NUM_BITS =:= 0, C >= 0,
                            is_integer(B), is_integer(D) ->
            Min = min_band(A, B, C, D),
            Max = max_band(A, B, C, D),
            {Min,Max};
        {_, {C,D}} when is_integer(C), C >= 0 ->
            {0,D};
        {{A,B}, _} when is_integer(A), A >= 0 ->
            {0,B};
        {_, _} ->
            any
    end;
bounds('bor', R1, R2) ->
    case {R1,R2} of
        {{A,B}, {C,D}} when A =:= '-inf' orelse abs(A) bsr ?NUM_BITS =:= 0,
                            C =:= '-inf' orelse abs(C) bsr ?NUM_BITS =:= 0,
                            B =:= '+inf' orelse abs(B) bsr ?NUM_BITS =:= 0,
                            D =:= '+inf' orelse abs(D) bsr ?NUM_BITS =:= 0 ->
            Min = min_bor(A, B, C, D),
            Max = max_bor(A, B, C, D),
            normalize({Min,Max});
        {_, _} ->
            any
    end;
bounds('bxor', R1, R2) ->
    case {R1,R2} of
        {{A,B}, {C,D}} when A bsr ?NUM_BITS =:= 0, A >= 0,
                            C bsr ?NUM_BITS =:= 0, C >= 0,
                            is_integer(B), is_integer(D) ->
            Max = max_bxor(A, B, C, D),
            {0,Max};
        {_, _} ->
            any
    end;
bounds('bsr', R1, R2) ->
    case {R1,R2} of
        {{A,B}, {C,D}} when is_integer(C), C >= 0 ->
            Min = inf_min(inf_bsr(A, C), inf_bsr(A, D)),
            Max = inf_max(inf_bsr(B, C), inf_bsr(B, D)),
            normalize({Min,Max});
        {_, _} ->
            any
    end;
bounds('bsl', R1, R2) ->
    case {R1,R2} of
        {{A,B}, {C,D}} when A =:= '-inf' orelse abs(A) bsr ?NUM_BITS =:= 0,
                            B =:= '+inf' orelse abs(B) bsr ?NUM_BITS =:= 0 ->
            Min = inf_min(inf_bsl(A, C), inf_bsl(A, D)),
            Max = inf_max(inf_bsl(B, C), inf_bsl(B, D)),
            normalize({Min,Max});
        {_, _} ->
            any
    end;
bounds(max, R1, R2) ->
    {A,B} = expand(R1),
    {C,D} = expand(R2),
    normalize({inf_max(A, C),inf_max(B, D)});
bounds(min, R1, R2) ->
    {A,B} = expand(R1),
    {C,D} = expand(R2),
    normalize({inf_min(A, C),inf_min(B, D)}).

-spec relop(relop(), range(), range()) -> bool_result().

relop('<', {A,B}, {C,D}) ->
    case {inf_lt(B, C),inf_lt(A, D)} of
        {Bool,Bool} -> Bool;
        {_,_} -> 'maybe'
    end;
relop('=<', {A,B}, {C,D}) ->
    case {inf_le(B, C),inf_le(A, D)} of
        {Bool,Bool} -> Bool;
        {_,_} -> 'maybe'
    end;
relop('>=', {A,B}, {C,D}) ->
    case {inf_ge(B, C),inf_ge(A, D)} of
        {Bool,Bool} -> Bool;
        {_,_} -> 'maybe'
    end;
relop('>', {A,B}, {C,D}) ->
    case {inf_gt(B, C),inf_gt(A, D)} of
        {Bool,Bool} -> Bool;
        {_,_} -> 'maybe'
    end;
relop(_, _, _) ->
    'maybe'.

-spec infer_relop_types(relop(), range(), range()) -> any().

infer_relop_types(Op, {_,_}=Range1, {_,_}=Range2) ->
    case relop(Op, Range1, Range2) of
        'maybe' -> infer_relop_types_1(Op, Range1, Range2);
        true -> any;
        false -> none
    end;
infer_relop_types('<', {A,_}=R1, any) ->
    {R1, normalize({inf_add(A, 1), '+inf'})};
infer_relop_types('<', any, {_,D}=R2) ->
    {normalize({'-inf', inf_add(D, -1)}), R2};
infer_relop_types('=<', {A,_}=R1, any) ->
    {R1, normalize({A, '+inf'})};
infer_relop_types('=<', any, {_,D}=R2) ->
    {normalize({'-inf', D}), R2};
infer_relop_types('>=', {_,B}=R1, any) ->
    {R1, normalize({'-inf', B})};
infer_relop_types('>=', any, {C,_}=R2) ->
    {normalize({C, '+inf'}), R2};
infer_relop_types('>', {_,B}=R1, any) ->
    {R1, normalize({'-inf', inf_add(B, -1)})};
infer_relop_types('>', any, {C,_}=R2) ->
    {normalize({inf_add(C, 1), '+inf'}), R2};
infer_relop_types(_Op, _R1, _R2) ->
    any.

-spec is_masking_redundant(range(), integer()) -> boolean().

is_masking_redundant(_, -1) ->
    true;
is_masking_redundant({A,B}, M)
  when M band (M + 1) =:= 0,            %Is M + 1 a power of two?
       M > 0,
       is_integer(A), A >= 0,
       B band M =:= B ->
    true;
is_masking_redundant(_, _) ->
    false.

%%%
%%% Internal functions.
%%%

div_bounds({_,_}, {0,0}) ->
    %% Division by zero, don't try to do anything clever.
    any;
div_bounds({A,B}, {C,D}) when is_integer(A), is_integer(B),
                              is_integer(C), is_integer(D) ->
    Denominators = [min(C, D),max(C, D)|
                    %% Handle zero crossing for the denominator.
                    if
                        C < 0, 0 < D -> [-1, 1];
                        C =:= 0 -> [1];
                        D =:= 0 -> [-1];
                        true -> []
                    end],
    All = [X div Y || X <- [A,B],
                      Y <- Denominators,
                      Y =/= 0],
    Min = lists:min(All),
    Max = lists:max(All),
    normalize({Min,Max});
div_bounds({A,'+inf'}, {C,D}) when is_integer(C), C > 0, is_integer(D) ->
    Min = min(A div C, A div D),
    Max = '+inf',
    normalize({Min,Max});
div_bounds({'-inf',B}, {C,D}) when is_integer(C), C > 0, is_integer(D) ->
    Min = '-inf',
    Max = max(B div C, B div D),
    normalize({Min,Max});
div_bounds({A,B}, _) when is_integer(A), is_integer(B) ->
    Max = max(abs(A), abs(B)),
    Min = -Max,
    {Min,Max};
div_bounds(_, _) ->
    any.

rem_bounds({A,_}, {C,D}) when is_integer(C), is_integer(D), C > 0 ->
    Max = inf_add(D, -1),
    Min = if
              A =:= '-inf' -> -Max;
              A >= 0 -> 0;
              true -> -Max
          end,
    normalize({Min,Max});
rem_bounds(_, {C,D}) when is_integer(C), is_integer(D),
                     C =/= 0 orelse D =/= 0 ->
    Max = max(abs(C), abs(D)) - 1,
    Min = -Max,
    normalize({Min,Max});
rem_bounds({A,B}, _) ->
    %% The sign of the remainder is the same as the sign of the
    %% left-hand side operand; it does not depend on the sign of the
    %% right-hand side operand. Therefore, the range of the remainder
    %% is the range of the left-hand side operand extended to always
    %% include zero.
    Min = inf_min(0, A),
    Max = inf_max(0, B),
    normalize({Min,Max});
rem_bounds(_, _) ->
    any.

min_band(A, B, C, D) ->
    M = 1 bsl (upper_bit(A bor C) + 1),
    min_band(A, B, C, D, M).

min_band(A, _B, C, _D, 0) ->
    A band C;
min_band(A, B, C, D, M) ->
    if
        (bnot A) band (bnot C) band M =/= 0 ->
            case (A bor M) band -M of
                NewA when NewA =< B ->
                    min_band(NewA, B, C, D, 0);
                _ ->
                    case (C bor M) band -M of
                        NewC when NewC =< D ->
                            min_band(A, B, NewC, D, 0);
                        _ ->
                            min_band(A, B, C, D, M bsr 1)
                    end
            end;
        true ->
            min_band(A, B, C, D, M bsr 1)
    end.

max_band(A, B, C, D) ->
    M = 1 bsl upper_bit(B bxor D),
    max_band(A, B, C, D, M).

max_band(_A, B, _C, D, 0) ->
    B band D;
max_band(A, B, C, D, M) ->
    if
        B band (bnot D) band M =/= 0 ->
            case (B band (bnot M)) bor (M - 1) of
                NewB when NewB >= A ->
                    max_band(A, NewB, C, D, 0);
                _ ->
                    max_band(A, B, C, D, M bsr 1)
            end;
        (bnot B) band D band M =/= 0 ->
            case (D band (bnot M)) bor (M - 1) of
                NewD when NewD >= C ->
                    max_band(A, B, C, NewD, 0);
                _ ->
                    max_band(A, B, C, D, M bsr 1)
            end;
        true ->
            max_band(A, B, C, D, M bsr 1)
    end.

min_bor(A, B, C, D) ->
    case inf_lt(inf_min(A, C), 0) of
        true ->
            '-inf';
        false ->
            M = 1 bsl upper_bit(A bxor C),
            min_bor(A, B, C, D, M)
    end.

min_bor(A, _B, C, _D, 0) ->
    A bor C;
min_bor(A, B, C, D, M) ->
    if
        (bnot A) band C band M =/= 0 ->
            case (A bor M) band -M of
                NewA when NewA =< B ->
                    min_bor(NewA, B, C, D, 0);
                _ ->
                    min_bor(A, B, C, D, M bsr 1)
            end;
        A band (bnot C) band M =/= 0 ->
            case (C bor M) band -M of
                NewC when NewC =< D ->
                    min_bor(A, B, NewC, D, 0);
                _ ->
                    min_bor(A, B, C, D, M bsr 1)
            end;
        true ->
            min_bor(A, B, C, D, M bsr 1)
    end.

max_bor(A0, B, C0, D) ->
    A = inf_max(A0, 0),
    C = inf_max(C0, 0),
    case inf_max(B, D) of
        '+inf' ->
            '+inf';
        Max when Max < 0 ->
            %% Both B and D are negative. The intersection would be
            %% infinite.
            -1;
        _ ->
            %% At least one of B and D are positive. The intersection
            %% has a finite size.
            Intersection = B band D,
            M = 1 bsl upper_bit(Intersection),
            max_bor(Intersection, A, B, C, D, M)
    end.

max_bor(_Intersection, _A, B, _C, D, 0) ->
    B bor D;
max_bor(Intersection, A, B, C, D, M) ->
    if
        Intersection band M =/= 0 ->
            case (B - M) bor (M - 1) of
                NewB when NewB >= A ->
                    max_bor(Intersection, A, NewB, C, D, 0);
                _ ->
                    case (D - M) bor (M - 1) of
                        NewD when NewD >= C ->
                            max_bor(Intersection, A, B, C, NewD, 0);
                        _ ->
                            max_bor(Intersection, A, B, C, D, M bsr 1)
                    end
            end;
        true ->
            max_bor(Intersection, A, B, C, D, M bsr 1)
    end.

max_bxor(A, B, C, D) ->
    M = 1 bsl upper_bit(B band D),
    max_bxor(A, B, C, D, M).

max_bxor(_A, B, _C, D, 0) ->
    B bxor D;
max_bxor(A, B, C, D, M) ->
    if
        B band D band M =/= 0 ->
            case (B - M) bor (M - 1) of
                NewB when NewB >= A ->
                    max_bxor(A, NewB, C, D, M bsr 1);
                _ ->
                    case (D - M) bor (M - 1) of
                        NewD when NewD >= C ->
                            max_bxor(A, B, C, NewD, M bsr 1);
                        _ ->
                            max_bxor(A, B, C, D, M bsr 1)
                    end
            end;
        true ->
            max_bxor(A, B, C, D, M bsr 1)
    end.

upper_bit(Val) ->
    upper_bit_1(Val, 0).

upper_bit_1(Val0, N) ->
    case Val0 bsr 1 of
        0 -> N;
        Val -> upper_bit_1(Val, N + 1)
    end.

infer_relop_types_1('<', {A,B}, {C,D}) ->
    Left = normalize({A, clamp(inf_add(D, -1), A, B)}),
    Right = normalize({clamp(inf_add(A, 1), C, D), D}),
    {Left,Right};
infer_relop_types_1('=<', {A,B}, {C,D}) ->
    Left = normalize({A, clamp(D, A, B)}),
    Right = normalize({clamp(A, C, D), D}),
    {Left,Right};
infer_relop_types_1('>=', {A,B}, {C,D}) ->
    Left = normalize({clamp(C, A, B), B}),
    Right = normalize({C, clamp(B, C, D)}),
    {Left,Right};
infer_relop_types_1('>', {A,B}, {C,D}) ->
    Left = normalize({clamp(inf_add(C, 1), A, B), B}),
    Right = normalize({C,clamp(inf_add(B, -1), C, D)}),
    {Left,Right}.

%%%
%%% Handling of ranges.
%%%
%%% A range can begin with '-inf' OR end with '+inf'.
%%%
%%% Atoms are greater than all integers. Therefore, we don't
%%% need any special handling of '+inf'.
%%%

expand(any) -> {'-inf','+inf'};
expand({_,_}=R) -> R.

normalize({'-inf','-inf'}) ->
    {'-inf',-1};
normalize({'-inf','+inf'}) ->
    any;
normalize({'+inf','+inf'}) ->
    {0,'+inf'};
normalize({Min,Max}=T) ->
    true = inf_ge(Max, Min),
    T.

clamp(V, A, B) ->
    inf_min(inf_max(V, A), B).

inf_min(A, B) when A =:= '-inf'; B =:= '-inf' -> '-inf';
inf_min(A, B) when A =< B -> A;
inf_min(A, B) when A > B -> B.

inf_max('-inf', B) -> B;
inf_max(A, '-inf') -> A;
inf_max(A, B) when A >= B -> A;
inf_max(A, B) when A < B -> B.

inf_neg('-inf') -> '+inf';
inf_neg('+inf') -> '-inf';
inf_neg(N) -> -N.

inf_add(Int, N) when is_integer(Int) -> Int + N;
inf_add(Inf, _N) -> Inf.

inf_bsr('-inf', _S) ->
    '-inf';
inf_bsr('+inf', _S) ->
    '+inf';
inf_bsr(N, S0) when S0 =:= '-inf'; S0 < 0 ->
    S = inf_neg(S0),
    if
        S >= ?NUM_BITS, N < 0 -> '-inf';
        S >= ?NUM_BITS, N >= 0 -> '+inf';
        true -> N bsl S
    end;
inf_bsr(N, '+inf') ->
    if
        N < 0 -> -1;
        N >= 0 -> 0
    end;
inf_bsr(N, S) when S >= 0 ->
    N bsr S.

inf_bsl(N, S) ->
    inf_bsr(N, inf_neg(S)).

inf_lt(_, '-inf') -> false;
inf_lt('-inf', _) -> true;
inf_lt(A, B) -> A < B.

inf_ge(_, '-inf') -> true;
inf_ge('-inf', _) -> false;
inf_ge(A, B) -> A >= B.

inf_le(A, B) -> inf_ge(B, A).

inf_gt(A, B) -> inf_lt(B, A).
