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
%% TODO
%%
-module(tuple_inplace_checks).

-export([do0a/0, do0b/2, different_sizes/2, ambiguous_inits/1,
         update_record0/0, fc/0, track_update_record/1,
         gh8124_a/0, gh8124_b/0]).

-record(r, {a=0,b=0,c=0,tot=0}).
-record(r1, {a}).
-record(r2, {b}).

do0a() ->
    Ls = ex:f(),
    r0(Ls, #r{}).

do0b(A, B) ->
    Ls = ex:f(),
    r0(Ls, #r{a=A+B,b=A+B}).

r0([{Key,Val}|Updates], Acc=#r{a=A,b=B,c=C,tot=T}) ->
%ssa% (_, Rec) when post_ssa_opt ->
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


-record(r0, {not_aliased=0,aliased=[]}).

update_record0() ->
    update_record0(ex:f(), #r0{}).

update_record0([Val|Ls], Acc=#r0{not_aliased=N}) ->
%ssa% (_, Rec) when post_ssa_opt ->
%ssa% _ = update_record(inplace, 3, Rec, 3, A, 2, NA) {unique => [Rec, NA], aliased => [A]}.
    R = Acc#r0{not_aliased=N+1,aliased=Val},
    update_record0(Ls, R);
update_record0([], Acc) ->
    Acc.

%% Check that the reuse hint for update_record isn't used when the
%% result is used by a inplace update_record instruction.
-record(fc_r, {anno=#{},
	       is,
	       last}).

fc() ->
    fc0(ex:f(), []).

fc0([{L,#fc_r{}=Blk}|Bs], Acc0) ->
%ssa% (_, _) when post_ssa_opt ->
%ssa% _ = update_record(copy, 4, _, 3, _),
%ssa% _ = update_record(copy, 4, _, 3, _).
    case ex:f() of
        [Is] ->
            Acc = fc0(Acc0),
            fc0(Bs, [{L,Blk#fc_r{is=Is}}|Acc]);
        Is ->
            fc0(Bs, [{L,Blk#fc_r{is=Is}}|Acc0])
    end.

fc0([{L,Blk}|Acc]) ->
%ssa% (_) when post_ssa_opt ->
%ssa% _ = update_record(inplace, 4, _, 3, _).
    [{L,Blk#fc_r{is=x}}|Acc].

-record(outer, {a,b}).
-record(inner, {c,d,e}).

track_update_record(#outer{a=A}=Outer) ->
%ssa% (A0) when post_ssa_opt ->
%ssa% switch(X, _, [{0,Zero},{1,One},{2,Two},{3,Three},{4,Four}]),
%ssa% label Four,
%ssa% LitInner4 = put_tuple(inner, undefined, undefined, undefined),
%ssa% LitOuter40 = update_record(copy, 3, A0, 3, _, 2, LitInner4),
%ssa% LitOuter41 = update_record(inplace, 3, LitOuter40, 3, _),
%ssa% _ = call(fun track_update_record1/1, LitOuter41),
%ssa% label Three,
%ssa% LitInner0 = put_tuple(inner, undefined, undefined, undefined),
%ssa% LitOuter0 = update_record(copy, 3, A0, 3, _, 2, LitInner0),
%ssa% _ = call(fun track_update_record1/1, LitOuter0),
%ssa% label Two,
%ssa% LitInner1 = put_tuple(inner, c, undefined, undefined),
%ssa% LitOuter1 = put_tuple(outer, LitInner1, b),
%ssa% _ = call(fun track_update_record1/1, LitOuter1),
%ssa% label One,
%ssa% C = update_record(copy, 4, _, 2, _),
%ssa% D = update_record(copy, 3, A0, 3, _, 2, C),
%ssa% _ = call(fun track_update_record1/1, D),
%ssa% label Zero,
%ssa% A = update_record(copy, 4, _, 2, _),
%ssa% B = update_record(copy, 3, A0, 2, A),
%ssa% _ = call(fun track_update_record1/1, B).
    C = e:f(),
    case e:f() of
	0 ->
	    track_update_record1(Outer#outer{a=A#inner{c=C}});
	1 ->
	    track_update_record1(Outer#outer{a=A#inner{c=C}, b=e:f()});
	2 ->
	    track_update_record1(#outer{a=#inner{c=c}, b=b});
	3 ->
	    track_update_record1(Outer#outer{a=#inner{},b=e:f()});
	4 ->
	    Tmp0 = Outer#outer{a=#inner{},b=e:f()},
	    Tmp = Tmp0#outer{b=e:f()},
	    track_update_record1(Tmp)
    end.

track_update_record1(#outer{a=A}=Outer) ->
%ssa% (A) when post_ssa_opt ->
%ssa% B = update_record(inplace, 4, _, 3, _),
%ssa% R = update_record(inplace, 3, A, 2, B),
%ssa% ret(R).
    B = e:f(),
    Outer#outer{a=A#inner{d=B}}.

%% Check that update patches are correctly merged when we have nested
%% tuple updates.
gh8124_a_inner() ->
%ssa% () when post_ssa_opt ->
%ssa% Inner = put_tuple(r2, _),
%ssa% Outer = put_tuple(r1, Inner),
%ssa% ret(Outer).
    #r1{a = #r2{b = <<"value1">>}}.

gh8124_a() ->
    R1 = #r1{a=A} = gh8124_a_inner(),
    R1#r1{a = A#r2{b= <<"new value">>}}.

gh8124_b_inner() ->
%ssa% () when post_ssa_opt ->
%ssa% Inner = put_tuple(r, ...),
%ssa% Lst = put_list(Inner, _),
%ssa% ret(Lst).
    R = #r{a = <<"value1">>},
    [R].

gh8124_b() ->
    [R] = gh8124_b_inner(),
    R#r{a = <<"value 2">>}.

