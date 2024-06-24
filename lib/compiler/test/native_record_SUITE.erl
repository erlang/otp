%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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
-module(native_record_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
         local_basic/1,local_updates/1,non_atomic_names/1,
         external_records/1,any_record/1,
         matching/1,is_record_bif/1]).

-record #empty{}.
-record #a{x, y}.
-record #b{x=none, y=none, z=none}.
-record #c{x::integer, y=0::integer, z=[]}.
-record #d{f=3.1416, l=[a,b,c], t={a,b,c},
           m=#{a => 1}}.
-record #e{x=0.0}.

-record #order{zzzz=0, true=1, aaaa=2, wwww=3}.

%% Records with non-atomic names.
-record #div{attr=0}.
-record #rem{n=0}.
-record #Seq{elements=[]}.
-record #Point{x=0,y=0,z=0}.

-import_record(ext_records, [local,vector]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [{group,p}].

groups() ->
    [{p,test_lib:parallel(),
      [
       local_basic,
       local_updates,
       non_atomic_names,
       any_record,
       external_records,
       matching,
       is_record_bif
      ]}].

init_per_suite(Config) ->
    id(Config),
    test_lib:recompile(?MODULE),
    {module,ext_records} = code:ensure_loaded(ext_records),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

local_basic(_Config) ->
    ARec = id(#a{x=1, y=2}),
    ARec = id(#a{x=id(1), y=id(2)}),
    BRec = id(#b{}),
    BRec = id(#b{x=id(none), y=id(none), z=id(none)}),
    CRec = id(#c{x=42, y=100}),
    DRec = id(#d{f=3.0}),
    Order = id(#order{}),

    #d{l=[a,b,c],t={a,b,c}} = DRec,
    #d{m=#{a := 1}} = DRec,

    #order{zzzz=0, true=1, aaaa=2, wwww=3} = Order,

    empty = name(id(#empty{})),
    a = name(ARec),
    b = name(BRec),
    c = name(CRec),

    NameFun = fun(#a{}) -> a;
                 (#b{}) -> b
              end,
    a = NameFun(ARec),
    b = NameFun(BRec),

    %% Test errors when constructing or updating native records.
    ?assertError({badfield,foobar}, #b{foobar = some_value}),

    ?assertError({badrecord,not_a_record}, (not_a_record)#b{x=99}),
    ?assertError({badrecord,ARec}, ARec#b{x=99}),
    ?assertError({badfield,bad_field}, BRec#b{bad_field = some_value}),

    ?assertError({novalue,x}, #a{}),
    ?assertError({novalue,y}, #a{x=1}),
    ?assertError({novalue,x}, #a{y=1}),

    %% Test errors when accessing native records
    ?assertError({badfield,zoo}, BRec#b.zoo),
    ?assertError({badrecord,ARec}, ARec#b.x),
    ?assertError({badrecord,ARec}, ARec#non_existing_module:rec.x),

    true = is_int_ax(ARec),
    false = is_int_ax(id(#a{x=a,y=b})),

    try id(throw(ARec)) of
        _ ->
            error(should_fail)
    catch
        throw:#a{x=1, y=2} ->
            ok
    end,

    %% Test garbage collection.
    N = 10000,
    Seq = lists:seq(1, N),
    RecList = [#a{x=I,y=I*I} || I <- Seq],
    [] = [{I,R#a.x,R#a.y} || I <- Seq && R <- RecList,
                             not(R#a.x =:= I andalso R#a.y =:= I*I)],

    %% Cover v3_core:sanitize/1.
    ?assertError({badmatch, ARec}, (#a{}=[_]) = id(ARec)),

    ok.

name(#empty{}) -> empty;
name(#a{}) -> a;
name(#b{}) -> b;
name(#c{}) -> c;
name(#div{}) -> 'div';
name(#Seq{}) -> 'Seq'.

is_int_ax(A) ->
    Result = is_int_ax_guard_1(A),
    Result = is_int_ax_guard_2(A),
    Result = is_integer(A#a.x),
    Result.

is_int_ax_guard_1(A) when is_integer(A#a.x) -> true;
is_int_ax_guard_1(_) -> false.

is_int_ax_guard_2(#a{}=A) when is_integer(A#a.x) -> true;
is_int_ax_guard_2(_) -> false.

local_updates(_Config) ->
    R0 = id(#b{}),
    R0 = R0#b{},
    R1 = id(R0#b{x=foo}),
    #b{x=foo, y=none, z=none} = id(R1),
    #?MODULE:b{x=foo, y=none, z=none} = id(R1),
    foo = R1#b.x,
    none = R1#b.y,
    none = R1#b.z,

    R2 = id(R1#b{y=bar, z=baz}),
    R2 = id(R1#?MODULE:b{z=baz, y=bar}),
    #b{x=foo, y=bar, z=baz} = id(R2),
    foo = R2#b.x,
    bar = R2#b.y,
    baz = R2#b.z,

    foo = R2#?MODULE:b.x,
    bar = R2#?MODULE:b.y,
    baz = R2#?MODULE:b.z,

    N = 10000,
    #a{x=N,y=0} =
        lists:foldl(fun(_, #a{x=X,y=Y}=R) ->
                            R#a{x=X+1,y=Y-1}
                    end, #a{x=0,y=N}, lists:seq(1, N)),

    ok.

non_atomic_names(_Config) ->
    Div0 = id(#div{attr=42}),
    Seq0 = id(#Seq{elements=[1,2,3]}),

    'div' = name(Div0),
    'Seq' = name(Seq0),

    Div0 = id(#?MODULE:div{attr=42}),
    Seq0 = id(#?MODULE:Seq{elements=[1,2,3]}),

    42 = Div0#div.attr,
    42 = Div0#?MODULE:div.attr,

    [1,2,3] = Seq0#Seq.elements,
    [1,2,3] = Seq0#?MODULE:Seq.elements,

    Div = id(Div0#div{attr=99}),
    Seq = id(Seq0#Seq{elements=[10]}),

    Div = id(Div0#?MODULE:div{attr=99}),
    Seq = id(Seq0#?MODULE:Seq{elements=[10]}),

    99 = non_atomic_names_match(Div),
    [10] = non_atomic_names_match(Seq),
    100 = non_atomic_names_match(#rem{n=100}),

    Point = #Point{x=13,y=40,z=1},
    Point = #Point{x=13,y=40,z=1} = non_atomic_names_match(Point),

    ARecDiv0 = id(#a{x=#div{attr=7}, y=#div{attr=9}}),
    7 = ARecDiv0#a.x#div.attr,
    #div{attr=13} = id(ARecDiv0#a.x#div{attr=13}),

    DivRem0 = id(#div{attr=#rem{n=42}}),
    42 = DivRem0#div.attr#rem.n,

    DivRem = id(#?MODULE:div{attr=#?MODULE:rem{n=77}}),
    77 = DivRem#div.attr#rem.n,

    try id(throw(Div)) of
        _ ->
            error(should_fail)
    catch
        throw:#div{attr=99} ->
            ok
    end,

    try id(error(Seq)) of
        _ ->
            error(should_fail)
    catch
        error:#?MODULE:Seq{elements=[10]} ->
            ok
    end,

    ok.

non_atomic_names_match(R) ->
    case R of
        #div{attr=Attr} ->
            Attr;
        #?MODULE:Seq{elements=Es} ->
            Es;
        #?MODULE:rem{n=N} ->
            N;
        #Point{x=X,y=Y,z=Z}=Point when X =/= 0, Y =/= 0, Z =/= 0 ->
            Point
    end.

external_records(_Config) ->
    DefVector = id(#vector{}),
    DefVector = id(#ext_records:vector{}),

    #vector{x=10, y=1, z=5} = DefVector,
    #ext_records:vector{x=10, y=1, z=5} = DefVector,
    true = records:is_exported(DefVector),

    ExtLocal = ext_records:local([1,2,3], {a,b,c}),
    false = records:is_exported(ExtLocal),

    ?assertError({badrecord,{ext_records,local}}, #local{a=1, b=2}),
    ?assertError({badrecord,{ext_records,foreign}}, #ext_records:foreign{a=1, b=2}),

    ?assertError({badrecord,ExtLocal}, ExtLocal#local{a=42,b=99}),

    #local{} = ExtLocal,
    #ext_records:local{} = ExtLocal,

    case ExtLocal of
        #local{x=X, y=Y} ->
            error({should_fail,X,Y});
        _ ->
            ok
    end,

    ok.

any_record(_Config) ->
    {777,888} = get_any_xy(#a{x=777,y=888}),
    {77,88} = get_any_xy(#Point{x=77,y=88}),
    none = get_any_xy(#div{}),

    ARec0 = id(#a{x=1,y=0}),
    CRec0 = id(#c{x=1,y=0,z=[]}),

    #a{x=7,y=13} = ARec = update_any_xy(ARec0, 7, 13),
    #_{x=7,y=13} = ARec,

    #c{x=100,y=200} = CRec = update_any_xy(CRec0, 100, 200),
    #_{x=100,y=200} = CRec,

    ?assertError({badfield,x}, update_any_xy(id(#d{}), 0, 0)),
    ?assertError({badfield,y}, update_any_xy(id(#e{}), 0, 0)),

    {10,1} = get_any_xy(#ext_records:vector{}),
    {77,88} = get_any_xy(#ext_records:vector{x=77,y=88}),

    ExtLocal = ext_records:local(7, 13),
    ?assertError({badrecord,ExtLocal}, update_any_xy(ExtLocal, 1, 1)),

    none = get_any_xy(ext_records:local(1, 2)),

    ok.

get_any_xy(#_{x=X,y=Y}=R) ->
    X = R#_.x,
    Y = R#_.y,
    {X,Y};
get_any_xy(_) ->
    none.

update_any_xy(R, X, Y) ->
    R#_{x=X,y=Y}.

matching(_Config) ->
    a_origin = match_abc(id(#a{x=0,y=0})),
    {small_a,1,2} = match_abc(id(#a{x=1,y=2})),

    {int_b,6} = match_abc(id(#b{x=1,y=2,z=3})),
    b_none = match_abc(id(#b{})),
    {other,#b{x=atom,y=none,z=none}} = do_match_abc(id(#b{x=atom})),
    {xyz,atom,none,none} = do_match_abc_anon(id(#b{x=atom})),

    {c_list,[a,b,c]} = do_match_abc(id(#c{x=1, y=2, z=[a,b,c]})),
    {c,a,b,[]} = do_match_abc(id(#c{x=a, y=b, z=[]})),

    {other,#d{}} = match_abc(id(#d{})),
    {other,#empty{}} = match_abc(id(#empty{})),

    {other,#vector{}} = do_match_abc(id(#vector{})),
    {xyz,10,1,5} = do_match_abc_anon(id(#vector{})),

    {local,#local{}} = match_abc(ext_records:local(a, b)),

    ~"any" = match_bin(#b{y = <<3,"any">>}),
    none = match_bin(#b{y = <<99,"any">>}),
    none = match_bin(#b{y = <<>>}),
    none = match_bin(#b{y = a}),

    42 = match_bin(#b{z = #a{x = <<8,42>>, y = 0}}),
    none = match_bin(#b{z = #a{x = <<0,42>>, y = 0}}),
    none = match_bin(#b{z = #a{x = 0, y = 0}}),
    none = match_bin(#b{z = #b{}}),

    ok.

match_abc(R) ->
    Res = do_match_abc(R),
    Res = do_match_abc_anon(R),
    Res.

do_match_abc(#a{x=0, y=0}) ->
    a_origin;
do_match_abc(#b{x=X, y=Y, z=Z}) when is_integer(X+Y+Z) ->
    {int_b,X+Y+Z};
do_match_abc(#local{x=_,y=_}=Local) ->
    error({should_not_match,Local});
do_match_abc(#b{x=none, y=none, z=none}) ->
    b_none;
do_match_abc(#c{x=X, y=Y, z=Z}) when length(Z) =:= X + Y ->
    {c_list,Z};
do_match_abc(#a{x=X, y=Y}) when X + Y < 10 ->
    {small_a,X,Y};
do_match_abc(#c{x=X, y=Y, z=Z}) ->
    {c,X,Y,Z};
do_match_abc(#a{x=X, y=Y}) ->
    {a,X,Y};
do_match_abc(#local{}=Local) ->
    {local,Local};
do_match_abc(Other) when is_record(Other) ->
    {other,Other};
do_match_abc(_) ->
    none.

do_match_abc_anon(#a{x=0, y=0}) ->
    a_origin;
do_match_abc_anon(#b{x=X, y=Y, z=Z}) when is_integer(X+Y+Z) ->
    {int_b,X+Y+Z};
do_match_abc_anon(#local{x=_,y=_}=Local) ->
    error({should_not_match,Local});
do_match_abc_anon(#b{x=none, y=none, z=none}) ->
    b_none;
do_match_abc_anon(#c{x=X, y=Y, z=Z}) when length(Z) =:= X + Y ->
    {c_list,Z};
do_match_abc_anon(#a{x=X, y=Y}) when X + Y < 10 ->
    {small_a,X,Y};
do_match_abc_anon(#c{x=X, y=Y, z=Z}) ->
    {c,X,Y,Z};
do_match_abc_anon(#a{x=X, y=Y}) ->
    {a,X,Y};
do_match_abc_anon(#local{}=Local) ->
    {local,Local};
do_match_abc_anon(#_{x=X, y=Y, z=Z}) ->
    {xyz,X,Y,Z};
do_match_abc_anon(#_{x=X, y=Y}) ->
    {xy,X,Y};
do_match_abc_anon(Other) when is_record(Other) ->
    {other,Other};
do_match_abc_anon(_) ->
    none.

%% Cover split_record_pat/4 in v3_core.
match_bin(#b{y = <<N:8,S:N/binary>>}) ->
    S;
match_bin(#b{z = #a{x= <<N:8,Int:N>>}}) ->
    Int;
match_bin(_) ->
    none.

is_record_bif(Config) ->
    false = is_record(Config, empty),
    false = is_record(Config, ?MODULE, empty),
    false = is_record(Config, a),
    false = is_record(Config, ?MODULE, a),

    BR = id(#b{}),
    true = is_record(BR, b),
    true = is_record(BR, ?MODULE, b),

    Local = ext_records:local(a, b),
    true = is_record(Local, local),
    true = is_record(Local, ext_records, local),

    %% In guards.
    if
        is_record(Config, b) -> error(should_fail);
        true -> ok
    end,
    if
        is_record(BR, b) -> ok
    end,
    if
        is_record(BR, ?MODULE, b) -> ok
    end,
    if
        is_record(Local, local) -> ok
    end,
    if
        is_record(Local, ext_records, local) -> ok
    end,

    True = id(true),
    if
        True, is_record(BR, ?MODULE, b) -> ok
    end,
    if
        is_list(Config), is_record(BR, ?MODULE, b) -> ok
    end,
    if
        not is_record(Local, ext_records, local) -> error(should_fail);
        is_record(Local, ext_records, local), True -> ok
    end,

    %% Test calling the actual BIFs.
    true = is_record(BR, id(b)),
    true = is_record(BR, id(?MODULE), id(b)),
    false = is_record(BR, id(empty)),

    ok.

%%% Common utilities.

id(I) ->
    I.
