%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2024. All Rights Reserved.
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
-module(tuple_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 t_size/1, t_tuple_size/1, t_element/1, t_setelement/1,
	 t_insert_element/1, t_delete_element/1,
	 t_list_to_tuple/1, t_list_to_upper_boundry_tuple/1, t_tuple_to_list/1,
	 t_make_tuple_2/1, t_make_upper_boundry_tuple_2/1, t_make_tuple_3/1,
	 t_append_element/1, t_append_element_upper_boundry/1,
         build_and_match/1, tuple_with_case/1, tuple_in_guard/1,
         get_two_tuple_elements/1]).
-include_lib("common_test/include/ct.hrl").

%% Tests tuples and the BIFs:
%%
%% size(Tuple)
%% element/2
%% setelement/3
%% tuple_to_list/1
%% list_to_tuple/1
%% make_tuple/2
%%

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [build_and_match, t_size, t_tuple_size, t_list_to_tuple,
     t_list_to_upper_boundry_tuple,
     t_tuple_to_list, t_element, t_setelement,
     t_make_tuple_2, t_make_upper_boundry_tuple_2, t_make_tuple_3,
     t_append_element, t_append_element_upper_boundry,
     t_insert_element, t_delete_element,
     tuple_with_case, tuple_in_guard,
     get_two_tuple_elements].

groups() -> 
    [].

init_per_suite(Config) ->
    A0 = case application:start(sasl) of
	     ok -> [sasl];
	     _ -> []
	 end,
    A = case application:start(os_mon) of
	     ok -> [os_mon|A0];
	     _ -> A0
	 end,
    [{started_apps, A}|Config].

end_per_suite(Config) ->
    As = proplists:get_value(started_apps, Config),
    lists:foreach(fun (A) -> application:stop(A) end, As),
    Config.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


build_and_match(Config) when is_list(Config) ->
    {} = id({}),
    {1} = id({1}),
    {1, 2} = id({1, 2}),
    {1, 2, 3} = id({1, 2, 3}),
    {1, 2, 3, 4} = id({1, 2, 3, 4}),
    {1, 2, 3, 4, 5} = id({1, 2, 3, 4, 5}),
    {1, 2, 3, 4, 5, 6} = id({1, 2, 3, 4, 5, 6}),
    {1, 2, 3, 4, 5, 6} = id({1, 2, 3, 4, 5, 6}),
    {1, 2, 3, 4, 5, 6, 7} = id({1, 2, 3, 4, 5, 6, 7}),
    {1, 2, 3, 4, 5, 6, 7, 8} = id({1, 2, 3, 4, 5, 6, 7, 8}),
    ok.

%% Tests size(Tuple).

t_size(Config) when is_list(Config) ->
    0 = size({}),
    1 = size({a}),
    1 = size({{a}}),
    2 = size({{a}, {b}}),
    3 = size({1, 2, 3}),
    ok.

t_tuple_size(Config) when is_list(Config) ->
    0 = tuple_size(id({})),
    1 = tuple_size(id({a})),
    1 = tuple_size(id({{a}})),
    2 = tuple_size(id({{a},{b}})),
    3 = tuple_size(id({1,2,3})),

    %% Error cases.
    {'EXIT',{badarg,_}} = (catch tuple_size([])),
    {'EXIT',{badarg,_}} = (catch tuple_size(<<1,2,3>>)),
    error = ludicrous_tuple_size({a,b,c}),
    error = ludicrous_tuple_size([a,b,c]),
    ok.


ludicrous_tuple_size(T)
  when tuple_size(T) =:= 16#7777777777777777777777777777777777 -> ok;
ludicrous_tuple_size(_) -> error.

%% Tests element/2.

t_element(Config) when is_list(Config) ->
    a = element(1, {a}),
    a = element(1, {a, b}),

    List = lists:seq(1, 16384),
    Tuple = list_to_tuple(List),
    get_elements(List, Tuple, 1),

    get_literal_elements(Tuple),
    get_literal_tuple_elements(Tuple),

    {31,32, 63,64, 127,128, 255,256, 511,512, 1023,1024,
     2047,2048, 4095,4096, 8191,8192, 16383, 16384} =
        get_literal_tuple_element_pairs(Tuple),

    {'EXIT', {badarg, _}} = (catch element(0, id({a,b}))),
    {'EXIT', {badarg, _}} = (catch element(3, id({a,b}))),
    {'EXIT', {badarg, _}} = (catch element(1, id({}))),
    {'EXIT', {badarg, _}} = (catch element(1, id([a,b]))),
    {'EXIT', {badarg, _}} = (catch element(1, id(42))),
    {'EXIT', {badarg, _}} = (catch element(false, id({a,b}))),
    {'EXIT', {badarg, _}} = (catch element(id(1.5), id({a,b}))),

    %% Make sure that the loader does not reject the module when
    %% huge literal index values are used.
    {'EXIT', {badarg, _}} = (catch element((1 bsl 24)-1, id({a,b,c}))),
    {'EXIT', {badarg, _}} = (catch element(1 bsl 24, id({a,b,c}))),
    {'EXIT', {badarg, _}} = (catch element(1 bsl 32, id({a,b,c}))),
    {'EXIT', {badarg, _}} = (catch element(1 bsl 64, id({a,b,c}))),

    %% Test known tuple and unknown position.
    true = is_tuple(Tuple),
    {'EXIT', {badarg, _}} = catch element(id(false), Tuple),
    {'EXIT', {badarg, _}} = catch element(id(-1), Tuple),
    {'EXIT', {badarg, _}} = catch element(id(0), Tuple),
    {'EXIT', {badarg, _}} = catch element(id(1 bsl 64), Tuple),

    %% Test a known tuple and position that is a known integer.
    {'EXIT', {badarg, _}} = catch element(known_integer(-1), Tuple),
    {'EXIT', {badarg, _}} = catch element(known_integer(0), Tuple),
    {'EXIT', {badarg, _}} = catch element(known_integer(1 bsl 64), Tuple),
    {'EXIT', {badarg, _}} = catch element(known_integer(tuple_size(Tuple)+1), Tuple),

    %% Test unknown tuple and unknown position.
    {'EXIT', {badarg, _}} = catch element(id(false), id(Tuple)),
    {'EXIT', {badarg, _}} = catch element(id(-1), id(Tuple)),
    {'EXIT', {badarg, _}} = catch element(id(0), id(Tuple)),
    {'EXIT', {badarg, _}} = catch element(id(1 bsl 64), id(Tuple)),

    ok.

known_integer(I) when is_integer(I) ->
    I.

get_elements([Element|Rest], Tuple, Pos) ->
    Element = element(Pos, Tuple),
    get_elements(Rest, Tuple, Pos+1);
get_elements([], _Tuple, _Pos) ->
    ok.

get_literal_elements(Tuple) ->
    31 = element(31, Tuple),
    32 = element(32, Tuple),

    63 = element(63, Tuple),
    64 = element(64, Tuple),

    127 = element(127, Tuple),
    128 = element(128, Tuple),

    255 = element(255, Tuple),
    256 = element(256, Tuple),

    511 = element(511, Tuple),
    512 = element(512, Tuple),

    1023 = element(1023, Tuple),
    1024 = element(1024, Tuple),

    2047 = element(2047, Tuple),
    2048 = element(2048, Tuple),

    4095 = element(4095, Tuple),
    4096 = element(4096, Tuple),

    8191 = element(8191, Tuple),
    8192 = element(8192, Tuple),

    16383 = element(16383, Tuple),
    16384 = element(16384, Tuple),

    ok.

get_literal_tuple_elements(Tuple) when tuple_size(Tuple) =:= 16384 ->
    %% Since the tuple size is known, the element/2 calls will be
    %% rewritten to get_tuple_element instructions.

    31 = element(31, Tuple),
    32 = element(32, Tuple),

    63 = element(63, Tuple),
    64 = element(64, Tuple),

    127 = element(127, Tuple),
    128 = element(128, Tuple),

    255 = element(255, Tuple),
    256 = element(256, Tuple),

    511 = element(511, Tuple),
    512 = element(512, Tuple),

    1023 = element(1023, Tuple),
    1024 = element(1024, Tuple),

    2047 = element(2047, Tuple),
    2048 = element(2048, Tuple),

    4095 = element(4095, Tuple),
    4096 = element(4096, Tuple),

    8191 = element(8191, Tuple),
    8192 = element(8192, Tuple),

    16383 = element(16383, Tuple),
    16384 = element(16384, Tuple),

    ok.

get_literal_tuple_element_pairs(Tuple) when tuple_size(Tuple) =:= 16384 ->
    %% Since the tuple size is known, the element/2 calls will be
    %% rewritten to get_tuple_element instructions.

    {element(31, Tuple),
     element(32, Tuple),

     element(63, Tuple),
     element(64, Tuple),

     element(127, Tuple),
     element(128, Tuple),

     element(255, Tuple),
     element(256, Tuple),

     element(511, Tuple),
     element(512, Tuple),

     element(1023, Tuple),
     element(1024, Tuple),

     element(2047, Tuple),
     element(2048, Tuple),

     element(4095, Tuple),
     element(4096, Tuple),

     element(8191, Tuple),
     element(8192, Tuple),

     element(16383, Tuple),
     element(16384, Tuple)}.

%% Tests set_element/3.

t_setelement(Config) when is_list(Config) ->
    {x} = setelement(1, id({1}), x),
    {x,2} = setelement(1, id({1,2}), x),
    {1,x} = setelement(2, id({1,2}), x),

    Tuple = list_to_tuple(lists:duplicate(16385, x)),
    NewTuple = set_all_elements(Tuple, 1),
    NewTuple = list_to_tuple(lists:seq(1+7, 16385+7)),

    {'EXIT', {badarg, _}} = (catch setelement(0, {a, b}, x)),
    {'EXIT', {badarg, _}} = (catch setelement(3, {a, b}, x)),
    {'EXIT', {badarg, _}} = (catch setelement(1, {}, x)),
    {'EXIT', {badarg, _}} = (catch setelement(1, [a, b], x)),
    {'EXIT', {badarg, _}} = (catch setelement(1.5, {a, b}, x)),

    %% Nested setelement with literals.
    AnotherTuple = id({0,0,a,b,c}),
    {93748793749387837476555412,3.0,gurka,b,c} =
	setelement(1, setelement(2, setelement(3, AnotherTuple, gurka),
				 3.0), 93748793749387837476555412),

    NewNewTuple = set_literal_tuple_elements(NewTuple),
    verify_set_elements(1, NewNewTuple),

    ok.

set_all_elements(Tuple, Pos) when Pos =< size(Tuple) ->
    set_all_elements(setelement(Pos, Tuple, Pos+7), Pos+1);
set_all_elements(Tuple, Pos) when Pos > size(Tuple) ->
    Tuple.

set_literal_tuple_elements(Tuple0) when tuple_size(Tuple0) =:= 16385 ->
    %% Since the tuple size is known, the setelement/3 calls will be
    %% rewritten to set_tuple_element instructions.
    Tuple1 = setelement(16385, Tuple0, -16385),
    Tuple2 = setelement(16384, Tuple1, -16384),

    Tuple3 = setelement(8192, Tuple2, -8192),
    Tuple4 = setelement(8191, Tuple3, -8191),

    Tuple5 = setelement(4096, Tuple4, -4096),
    Tuple6 = setelement(4095, Tuple5, -4095),

    Tuple7 = setelement(2048, Tuple6, -2048),
    Tuple8 = setelement(2047, Tuple7, -2047),

    Tuple9 = setelement(1024, Tuple8, -1024),
    Tuple10 = setelement(1023, Tuple9, -1023),

    Tuple11 = setelement(512, Tuple10, -512),
    Tuple12 = setelement(511, Tuple11, -511),

    Tuple13 = setelement(256, Tuple12, -256),
    Tuple14 = setelement(255, Tuple13, -255),

    Tuple15 = setelement(128, Tuple14, -128),
    Tuple16 = setelement(127, Tuple15, -127),

    Tuple17 = setelement(64, Tuple16, -64),
    Tuple18 = setelement(63, Tuple17, -63),

    Tuple19 = setelement(32, Tuple18, -32),
    Tuple20 = setelement(31, Tuple19, -31),

    Tuple21 = setelement(16, Tuple20, -16),
    Tuple22 = setelement(15, Tuple21, -15),

    Tuple23 = setelement(8, Tuple22, -8),
    Tuple24 = setelement(7, Tuple23, -7),

    Tuple25 = setelement(4, Tuple24, -4),
    Tuple26 = setelement(3, Tuple25, -3),

    Tuple27 = setelement(2, Tuple26, -2),
    setelement(1, Tuple27, -1).

verify_set_elements(16385, Tuple) ->
    -16385 = element(16385, Tuple),
    ok;
verify_set_elements(N, Tuple) ->
    El = element(N, Tuple),
    if
        El =:= N + 7 ->
            true = not (is_power_of_two(N + 1) andalso is_power_of_two(N)),
            verify_set_elements(N + 1, Tuple);
        El =:= -N ->
            true = is_power_of_two(N + 1) orelse is_power_of_two(N),
            verify_set_elements(N + 1, Tuple);
        true ->
            io:format("element(~p) =:= ~p\n", [N,El]),
            ct:fail(bad_element)
    end.

is_power_of_two(N) ->
    N band (N - 1) =:= 0.

%% Tests list_to_tuple/1.

t_list_to_tuple(Config) when is_list(Config) ->
    {} = list_to_tuple([]),
    {a} = list_to_tuple([a]),
    {a, b} = list_to_tuple([a, b]),
    {a, b, c} = list_to_tuple([a, b, c]),
    {a, b, c, d} = list_to_tuple([a, b, c, d]),
    {a, b, c, d, e} = list_to_tuple([a, b, c, d, e]),

    Size  = 4096,
    Tuple = list_to_tuple(lists:seq(1, Size)),
    Size  = size(Tuple),

    {'EXIT', {badarg, _}} = (catch list_to_tuple(id({a,b}))),
    {'EXIT', {badarg, _}} = (catch list_to_tuple(id([a|b]))),
    {'EXIT', {badarg, _}} = (catch list_to_tuple(id([a|b]))),

    {'EXIT', {badarg,_}} = (catch list_to_tuple(lists:seq(1, 1 bsl 24))),
    ok.

t_list_to_upper_boundry_tuple(Config) when is_list(Config) ->
    sys_mem_cond_run(2048,
		    fun () ->
			    %% test upper boundary, 16777215 elements
			    MaxSize  = 1 bsl 24 - 1,
			    MaxTuple = list_to_tuple(lists:seq(1, MaxSize)),
			    MaxSize  = size(MaxTuple),
			    ok
		    end).

%% Tests tuple_to_list/1.

t_tuple_to_list(Config) when is_list(Config) ->
    [] = tuple_to_list({}),
    [a] = tuple_to_list({a}),
    [a, b] = tuple_to_list({a, b}),
    [a, b, c] = tuple_to_list({a, b, c}),
    [a, b, c, d] = tuple_to_list({a, b, c, d}),
    [a, b, c, d] = tuple_to_list({a, b, c, d}),

    Size = 4096,
    List = lists:seq(1, Size),
    Tuple = list_to_tuple(List),
    Size = size(Tuple),
    List = tuple_to_list(Tuple),

    {'EXIT', {badarg,_}} = (catch tuple_to_list(id(a))),
    {'EXIT', {badarg,_}} = (catch tuple_to_list(id(42))),

    ok.

%% Tests the make_tuple/2 BIF.
t_make_tuple_2(Config) when is_list(Config) ->
    t_make_tuple1([]),
    t_make_tuple1(42),
    t_make_tuple1(a),
    t_make_tuple1({}),
    t_make_tuple1({a}),
    t_make_tuple1(erlang:make_tuple(400, [])),

    {'EXIT', {badarg,_}} = (catch erlang:make_tuple(1 bsl 24, a)),

    {'EXIT', {badarg,_}} = (catch erlang:make_tuple(-1, a)),
    % 26 bits is the total header arity room (for now)
    {'EXIT', {badarg,_}} = (catch erlang:make_tuple(1 bsl 26 + 3, a)),
    % bignum
    {'EXIT', {badarg,_}} = (catch erlang:make_tuple(1 bsl 65 + 3, a)),
    ok.

t_make_upper_boundry_tuple_2(Config) when is_list(Config) ->
    sys_mem_cond_run(2048,
		     fun () ->
			     %% test upper boundary, 16777215 elements
			     t_make_tuple(1 bsl 24 - 1, a)
		     end).

t_make_tuple1(Element) ->
    lists:foreach(fun(Size) -> t_make_tuple(Size, Element) end,
		  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 255, 256, 511, 512, 999,
		   1000, 1023, 1024, 4095, 4096]).

t_make_tuple(Size, Element) ->
    Tuple = erlang:make_tuple(Size, Element),
    lists:foreach(fun(El) when El =:= Element ->
			  ok;
		     (Other) ->
			  ct:fail({got, Other, expected, Element})
		  end, tuple_to_list(Tuple)).

%% Tests the erlang:make_tuple/3 BIF.
t_make_tuple_3(Config) when is_list(Config) ->
    {}    = erlang:make_tuple(0, def, []),
    {def} = erlang:make_tuple(1, def, []),
    {a}   = erlang:make_tuple(1, def, [{1,a}]),

    {a,def,c,def,e} = erlang:make_tuple(5, def, [{5,e},{1,a},{3,c}]),
    {a,def,c,def,e} = erlang:make_tuple(5, def, [{1,blurf},{5,e},{3,blurf},{1,a},{3,c}]),
    MaxSize  = 1 bsl 16 - 1,
    MaxTuple = erlang:make_tuple(MaxSize, def, [{1,blurf},{5,e},{3,blurf},{1,a},{3,c}]),
    MaxSize  = size(MaxTuple),

    %% Error cases.
    {'EXIT',{badarg,_}} = (catch erlang:make_tuple(0, def, [{1,a}])),
    {'EXIT',{badarg,_}} = (catch erlang:make_tuple(5, def, [{-1,a}])),
    {'EXIT',{badarg,_}} = (catch erlang:make_tuple(5, def, [{0,a}])),
    {'EXIT',{badarg,_}} = (catch erlang:make_tuple(5, def, [{6,z}])),
    {'EXIT',{badarg,_}} = (catch erlang:make_tuple(a, def, [{6,z}])),
    {'EXIT',{badarg,_}} = (catch erlang:make_tuple(5, def, [{1,a}|b])),
    {'EXIT',{badarg,_}} = (catch erlang:make_tuple(5, def, [42])),
    {'EXIT',{badarg,_}} = (catch erlang:make_tuple(5, def, [[a,b,c]])),
    {'EXIT',{badarg,_}} = (catch erlang:make_tuple(5, def, non_list)),
    {'EXIT',{badarg,_}} = (catch erlang:make_tuple(1 bsl 24, def, [{5,e},{1,a},{3,c}])),

    ok.

%% Tests the erlang:insert_element/3 BIF.
t_insert_element(Config) when is_list(Config) ->
    {a}       = erlang:insert_element(1, {}, a),
    {{b,b},a} = erlang:insert_element(1, {a}, {b,b}),
    {a,b}     = erlang:insert_element(2, {a}, b),
    [b,def|_] = tuple_to_list(erlang:insert_element(1, erlang:make_tuple(1 bsl 20, def), b)),
    [def,b|_] = tuple_to_list(erlang:insert_element(2, erlang:make_tuple(1 bsl 20, def), b)),
    [def,b|_] = lists:reverse(tuple_to_list(erlang:insert_element(1 bsl 20, erlang:make_tuple(1 bsl 20, def), b))),
    [b,def|_] = lists:reverse(tuple_to_list(erlang:insert_element((1 bsl 20) + 1, erlang:make_tuple(1 bsl 20, def), b))),

    %% Error cases.
    {'EXIT',{badarg,_}} = (catch erlang:insert_element(1, [], a)),
    {'EXIT',{badarg,_}} = (catch erlang:insert_element(1, a, a)),
    {'EXIT',{badarg,_}} = (catch erlang:insert_element(0, {}, a)),
    {'EXIT',{badarg,_}} = (catch erlang:insert_element(0, {b,b,b,b,b}, a)),
    {'EXIT',{badarg,_}} = (catch erlang:insert_element(-1, {}, a)),
    {'EXIT',{badarg,_}} = (catch erlang:insert_element(2, {}, a)),
    {'EXIT',{badarg,_}} = (catch erlang:insert_element(6, {b,b,b,b}, a)),
    {'EXIT',{badarg,_}} = (catch erlang:insert_element(1 bsl 20, {b,b,b,b}, a)),
    ok.

%% Tests the erlang:delete_element/3 BIF.
t_delete_element(Config) when is_list(Config) ->
    {}        = erlang:delete_element(1, {a}),
    {{b,b},c} = erlang:delete_element(1, {a,{b,b},c}),
    {a,b}     = erlang:delete_element(2, {a,c,b}),
    [2,3|_]   = tuple_to_list(erlang:delete_element(1, list_to_tuple(lists:seq(1, 1 bsl 20)))),
    [1,3|_]   = tuple_to_list(erlang:delete_element(2, list_to_tuple(lists:seq(1, 1 bsl 20)))),
    [(1 bsl 20) - 1, (1 bsl 20) - 2 |_] = lists:reverse(tuple_to_list(erlang:delete_element(1 bsl 20, list_to_tuple(lists:seq(1, 1 bsl 20))))),
    [(1 bsl 20), (1 bsl 20) - 2 |_] = lists:reverse(tuple_to_list(erlang:delete_element((1 bsl 20) - 1, list_to_tuple(lists:seq(1, 1 bsl 20))))),

    %% Error cases.
    {'EXIT',{badarg,_}} = (catch erlang:delete_element(1, [])),
    {'EXIT',{badarg,_}} = (catch erlang:delete_element(1, a)),
    {'EXIT',{badarg,_}} = (catch erlang:delete_element(0, {})),
    {'EXIT',{badarg,_}} = (catch erlang:delete_element(-1, {})),
    {'EXIT',{badarg,_}} = (catch erlang:delete_element(1, {})),
    {'EXIT',{badarg,_}} = (catch erlang:delete_element(0, {b,b,b,b,b})),
    {'EXIT',{badarg,_}} = (catch erlang:delete_element(5, {b,b,b,b})),
    {'EXIT',{badarg,_}} = (catch erlang:delete_element(1 bsl 20, {b,c,b,b,b})),
    ok.


%% Tests the append_element/2 BIF.
t_append_element(Config) when is_list(Config) ->
    ok = t_append_element({}, 2048, 2048).

t_append_element_upper_boundry(Config) when is_list(Config) ->
    sys_mem_cond_run(2048,
		     fun () ->
			     %% test upper boundary, 16777215 elements
			     MaxSize  = 1 bsl 24 - 1,
			     MaxTuple = list_to_tuple(lists:seq(1, MaxSize)),
			     {'EXIT',{badarg,_}} = (catch erlang:append_element(MaxTuple, a)),
			     ok
		     end).

t_append_element(_Tuple, 0, _High) -> ok;
t_append_element(Tuple, N, High) ->
    NewTuple = erlang:append_element(Tuple, N),
    verify_seq(tuple_to_list(Tuple), High, N),
    t_append_element(NewTuple, N-1, High).

verify_seq([], High, High) -> ok;
verify_seq([High], High, High) -> ok;
verify_seq([High|T], High, Lower) ->
    verify_seq(T, High-1, Lower).

%% Tests that a case nested inside a tuple is ok.
%% (This is known to crash earlier versions of BEAM.)

tuple_with_case(Config) when is_list(Config) ->
    {reply, true} = tuple_with_case(),
    ok.

tuple_with_case() ->
    %% The following comments apply to the BEAM compiler.
    foo(),                              % Reset var count.
    {reply,                             % Compiler will choose {x,1} for tuple.
     case foo() of                      % Call will reset var count.
         {'EXIT', Reason} ->		% Case will return in {x,1} (first free).
             {error, Reason};           % but the tuple will be build in {x,1},
         _ ->                           % so case value is lost and a circular
             true                       % data element is built.
     end}.

foo() -> ignored.

%% Test to build a tuple in a guard.

tuple_in_guard(Config) when is_list(Config) ->
    Tuple1 = id({a,b}),
    Tuple2 = id({a,b,c}),
    if
	Tuple1 == {element(1, Tuple2),element(2, Tuple2)} ->
	    ok;
	true ->
	    ct:fail("failed")
    end,
    if
	Tuple2 == {element(1, Tuple2),element(2, Tuple2),
	    element(3, Tuple2)} ->
	    ok;
	true ->
	    ct:fail("failed")
    end,
    ok.

%% For BeamAsm, test that the tuple pointer is correctly reloaded after
%% a get_two_tuple_elements instruction that ovewrites the register holding
%% the tuple.
get_two_tuple_elements(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    GTTETestsFile = filename:join(DataDir, "get_two_tuple_elements"),

    %% Compile from Erlang source code.
    {ok,GTTEMod,Code1} = compile:file(GTTETestsFile, [no_ssa_opt_sink,binary,report,time]),
    {module,GTTEMod} = code:load_binary(GTTEMod, GTTEMod, Code1),
    GTTEMod:GTTEMod(),
    true = code:delete(GTTEMod),
    code:purge(GTTEMod),

    %% Compile from the pre-generated BEAM assembly code file. (In case that the
    %% compiler's code generation has changed.)
    {ok,GTTEMod,Code2} = compile:file(GTTETestsFile, [from_asm,binary,report,time]),
    {module,GTTEMod} = code:load_binary(GTTEMod, GTTEMod, Code2),
    GTTEMod:GTTEMod(),
    true = code:delete(GTTEMod),
    code:purge(GTTEMod),

    ok.

%% Use this function to avoid compile-time evaluation of an expression.
id(I) -> I.


sys_mem_cond_run(ReqSizeMB, TestFun) when is_integer(ReqSizeMB) ->
    case total_memory() of
	TotMem when is_integer(TotMem), TotMem >= ReqSizeMB ->
	    TestFun();
	TotMem when is_integer(TotMem) ->
	    {skipped, "Not enough memory ("++integer_to_list(TotMem)++" MB)"};
	undefined ->
	    {skipped, "Could not retrieve memory information"}
    end.


total_memory() ->
    %% Total memory in MB.
    try
	SMD = memsup:get_system_memory_data(),
        TM = proplists:get_value(
               available_memory, SMD,
               proplists:get_value(
                 total_memory, SMD,
                 proplists:get_value(
                   system_total_memory, SMD))),
        TM div (1024*1024)
    catch
	_ : _ ->
	    undefined
    end.
