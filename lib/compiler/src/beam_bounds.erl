%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2022. All Rights Reserved.
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
-export(['band'/2,'bor'/2,'bxor'/2]).

-type range() :: {integer(), integer()} | 'any'.

-spec 'band'(range(), range()) -> range().

'band'({A,B}, {C,D}) when A >= 0, A bsr 256 =:= 0, C >= 0, C bsr 256 =:= 0 ->
    Min = min_band(A, B, C, D),
    Max = max_band(A, B, C, D),
    {Min,Max};
'band'(_, {C,D}) when C >= 0 ->
    {0,D};
'band'({A,B}, _) when A >= 0 ->
    {0,B};
'band'(_, _) ->
    any.

-spec 'bor'(range(), range()) -> range().

'bor'({A,B}, {C,D}) when A >= 0, A bsr 256 =:= 0, C >= 0, C bsr 256 =:= 0 ->
    Min = min_bor(A, B, C, D),
    Max = max_bor(A, B, C, D),
    {Min,Max};
'bor'(_, _) ->
    any.

-spec 'bxor'(range(), range()) -> range().

'bxor'({A,B}, {C,D}) when A >= 0, A bsr 256 =:= 0, C >= 0, C bsr 256 =:= 0 ->
    Max = max_bxor(A, B, C, D),
    {0,Max};
'bxor'(_, _) ->
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
    M = 1 bsl upper_bit(A bxor C),
    min_bor(A, B, C, D, M).

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

max_bor(A, B, C, D) ->
    Intersection = B band D,
    M = 1 bsl upper_bit(Intersection),
    max_bor(Intersection, A, B, C, D, M).

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
