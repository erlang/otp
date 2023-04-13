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
%% This module tests that beam_ssa_alias_opt:to_private_append/3
%% rewrites plain appends in bs_create_bin to private_append when
%% appropriate.
%%
-module(tuple_inplace_checks).

-export([do0a/0, do0b/2, different_sizes/2, ambiguous_inits/1]).

-record(r, {a=0,b=0,c=0,tot=0}).

do0a() ->
    Ls = ex:f(),
    r0(Ls, #r{}).

do0b(A, B) ->
    Ls = ex:f(),
    r0(Ls, #r{a=A+B,b=A+B}).

r0([{Key,Val}|Updates], Acc=#r{a=A,b=B,c=C,tot=T}) ->
%ssa% xfail (_, Rec) when post_ssa_opt ->
%ssa% _ = update_record(inplace, 5, Rec, ...),
%ssa% _ = update_record(inplace, 5, Rec, ...),
%ssa% _ = update_record(inplace, 5, Rec, ...).
    R = case Key of
	    a -> Acc#r{a=Val + A, tot=T + Val};
	    b -> Acc#r{b=Val + B, tot=T + Val};
	    c -> Acc#r{c=Val + C, tot=T + Val}
	end,
    r0(Updates, R);
r0([], Acc) ->
    Acc.

%% Check that that the literal returned by make_ds(a) is rewritten to
%% a put_tuple but the result of make_ds(b) is left alone.
-record(ds,{a}).

make_ds(a) ->
%ssa% (K) when post_ssa_opt ->
%ssa% switch(K, Fail, [{a,IsA},{b,IsB}]),
%ssa% label IsB,
%ssa% ret({0,0}),
%ssa% label IsA,
%ssa% Rec = put_tuple(ds, 0),
%ssa% Tuple = put_tuple(0, 0, Rec),
%ssa% ret(Tuple).
    {0,0,#ds{a=0}};
make_ds(b) ->
    {0,0}.

%% Check that #ds{} is updated using update_record+inplace
work_ds([X|Rest], {A,B,C=#ds{a=F}}) ->
%ssa% (Ls, Acc) when post_ssa_opt ->
%ssa% Size = bif:tuple_size(Acc),
%ssa% switch(Size, Fail, [{2,_},{3,RecordLbl}]),
%ssa% label RecordLbl,
%ssa% Tuple = get_tuple_element(Acc, 2),
%ssa% _ = update_record(inplace, 2, Tuple, 2, _).
    work_ds(Rest, {A,B,C#ds{a=F+X}});
work_ds([X|Rest], {A,B}) ->
    work_ds(Rest, {A+X,B+X+X});
work_ds([], Acc) ->
    Acc.

different_sizes(L, K) ->
    {work_ds(L, make_ds(K)), work_ds(L, make_ds(K))}.

%% Check that both branches of ambiguous_make/1 are converted into
%% heap tuples.
-record(ar,{f}).

make_int() ->
    case e:f() of
	X when is_integer(X) ->
	    X
    end.

ambiguous_make(a) ->
%ssa% (X) when post_ssa_opt ->
%ssa% IsB = bif:'=:='(X, b),
%ssa% br(IsB, BLbl, ALbl),
%ssa% label BLbl,
%ssa% R0 = put_tuple(...),
%ssa% ret(R0),
%ssa% label ALbl,
%ssa% R1 = put_tuple(...),
%ssa% ret(R1).
    #ar{f=make_int()};
ambiguous_make(b) ->
    #ar{f=5}.

ambiguous([X|Rest], R=#ar{f=F}) ->
%ssa% (_, R) when post_ssa_opt ->
%ssa% _ = update_record(inplace, 2, R, ...).
    ambiguous(Rest, R#ar{f=F+X});
ambiguous([], Acc) ->
    Acc.

ambiguous_inits(L) ->
    X = ambiguous(L, ambiguous_make(a)),
    Y = ambiguous(L, ambiguous_make(b)),
    {X,Y}.
