%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2018. All Rights Reserved.
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
-module(beam_type_SUITE).

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 integers/1,coverage/1,booleans/1,setelement/1,cons/1,
	 tuple/1,record_float/1,binary_float/1,float_compare/1,
	 arity_checks/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() ->
    [{p,[parallel],
      [integers,
       coverage,
       booleans,
       setelement,
       cons,
       tuple,
       record_float,
       binary_float,
       float_compare,
       arity_checks
      ]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

integers(_Config) ->
    a = do_integers_1(2#11000),
    b = do_integers_1(2#11001),

    a = do_integers_2(<<0:1>>),
    {'EXIT',{{case_clause,-1},_}} = (catch do_integers_2(<<1:1>>)),

    college = do_integers_3(),

    ok.

do_integers_1(B0) ->
    B = B0 band 1,
    case B band 15 of
	0 -> a;
	1 -> b
    end.

do_integers_2(Bin) ->
    <<B:1/signed>> = Bin,
    case B of
	0 -> a;
	1 -> b
    end.

do_integers_3() ->
    case try 0 after [] end of
	0 -> college;
	1 -> 0
    end.

coverage(_Config) ->
    {'EXIT',{badarith,_}} = (catch id(1) bsl 0.5),
    {'EXIT',{badarith,_}} = (catch id(2.0) bsl 2),
    {'EXIT',{badarith,_}} = (catch a + 0.5),
    {'EXIT',{badarith,_}} = (catch 2.0 * b),

    {'EXIT',{badarith,_}} = (catch id(42.0) / (1 bsl 2000)),

    id(id(42) band 387439739874298734983787934283479243879),
    id(-1 band id(13)),

    ok.

booleans(_Config) ->
    {'EXIT',{{case_clause,_},_}} = (catch do_booleans(42)),
    ok.

do_booleans(B) ->
    case is_integer(B) of
	yes -> yes;
	no -> no
    end.

setelement(_Config) ->
    T0 = id({a,42}),
    {a,_} = T0,
    {b,_} = setelement(1, T0, b),
    ok.

cons(_Config) ->
    [did] = cons(assigned, did),
    ok.

cons(assigned, Instrument) ->
    [Instrument] = [did].

tuple(_Config) ->
    {'EXIT',{{badmatch,{necessary}},_}} = (catch do_tuple()),
    ok.

do_tuple() ->
    {0, _} = {necessary}.

-record(x, {a}).

record_float(_Config) ->
    17.0 = record_float(#x{a={0}}, 1700),
    23.0 = record_float(#x{a={0}}, 2300.0),
    {'EXIT',{if_clause,_}} = (catch record_float(#x{a={1}}, 88)),
    {'EXIT',{if_clause,_}} = (catch record_float(#x{a={}}, 88)),
    {'EXIT',{if_clause,_}} = (catch record_float(#x{}, 88)),
    ok.

record_float(R, N0) ->
    N = N0 / 100,
    if element(1, R#x.a) =:= 0 ->
            N
    end.

binary_float(_Config) ->
    <<-1/float>> = binary_negate_float(<<1/float>>),
    ok.

binary_negate_float(<<Float/float>>) ->
    <<-Float/float>>.

float_compare(_Config) ->
    false = do_float_compare(-42.0),
    false = do_float_compare(-42),
    false = do_float_compare(0),
    false = do_float_compare(0.0),
    true = do_float_compare(42),
    true = do_float_compare(42.0),
    ok.

do_float_compare(X) ->
    %% ERL-433: Used to fail before OTP 20. Was accidentally fixed
    %% in OTP 20. Add a test case to ensure it stays fixed.

    Y = X + 1.0,
    case X > 0 of
        T when (T =:= nil) or (T =:= false) -> T;
        _T -> Y > 0
    end.

arity_checks(_Config) ->
    %% ERL-549: an unsafe optimization removed a test_arity instruction,
    %% causing the following to return 'broken' instead of 'ok'.
    ok = do_record_arity_check({rgb, 255, 255, 255, 1}),
    ok = do_tuple_arity_check({255, 255, 255, 1}).
 
-record(rgb, {r = 255, g = 255, b = 255}).

do_record_arity_check(RGB) when
        (element(2, RGB) >= 0), (element(2, RGB) =< 255),
        (element(3, RGB) >= 0), (element(3, RGB) =< 255),
        (element(4, RGB) >= 0), (element(4, RGB) =< 255) ->
    if
        element(1, RGB) =:= rgb, is_record(RGB, rgb) -> broken;
        true -> ok
    end.

do_tuple_arity_check(RGB) when is_tuple(RGB),
        (element(1, RGB) >= 0), (element(1, RGB) =< 255),
        (element(2, RGB) >= 0), (element(2, RGB) =< 255),
        (element(3, RGB) >= 0), (element(3, RGB) =< 255) ->
    case RGB of
        {255, _, _} -> broken;
        _ -> ok
    end.

id(I) ->
    I.
