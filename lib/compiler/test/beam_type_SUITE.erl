%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2016. All Rights Reserved.
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
	 tuple/1,record_float/1,binary_float/1]).

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
       binary_float
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

id(I) ->
    I.
