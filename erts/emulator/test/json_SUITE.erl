-module(json_SUITE).

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2,

         t2j_basic_types/1, t2j_integers/1, t2j_lists/1,
         t2j_objects_proplist/1, t2j_objects_flatmap/1, t2j_objects_hamt/1,
         t2j_objects_atomkey/1, t2j_unicode/1, t2j_binaries/1, t2j_bigints/1,
         t2j_preencoded/1, t2j_use_nil/1, t2j_bufsize/1, t2j_errors/1,

         j2t_basic_types/1, j2t_integers/1, j2t_floats/1, j2t_lists/1,
         j2t_objects_proplist/1, j2t_objects_map/1, j2t_string/1,
         j2t_escapes/1, j2t_unicode/1, j2t_errors/1,

         random_round_trip/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

suite() -> [{ct_hooks,[ts_install_cth]},
            {timetrap,{minutes,1}}].

all() ->
    [t2j_basic_types, t2j_integers, t2j_lists, t2j_objects_proplist,
     t2j_objects_flatmap, t2j_objects_hamt, t2j_objects_atomkey, t2j_unicode,
     t2j_binaries, t2j_bigints, t2j_preencoded, t2j_use_nil, t2j_bufsize,
     t2j_errors, j2t_basic_types, j2t_integers, j2t_floats, j2t_lists,
     j2t_objects_proplist, j2t_objects_map, j2t_string, j2t_escapes,
     j2t_unicode, j2t_errors, random_round_trip].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

%% ts:run(emulator, json_SUITE, [ batch ]).

t2j_basic_types(Config) when is_list(Config) ->
    <<"true">> = erlang:term_to_json(true),
    <<"false">> = erlang:term_to_json(false),
    <<"null">> = erlang:term_to_json(null),
    <<"1">> = erlang:term_to_json(1),
    <<"1.1">> = erlang:term_to_json(1.1),
    <<"\"apple\"">> = erlang:term_to_json(<<"apple">>),
    ok.

t2j_integers(Config) when is_list(Config) ->
    <<"0">> = erlang:term_to_json(0),
    <<"1">> = erlang:term_to_json(1),
    <<"-1">> = erlang:term_to_json(-1),
    <<"9">> = erlang:term_to_json(9),
    <<"10">> = erlang:term_to_json(10),
    <<"99">> = erlang:term_to_json(99),
    <<"-100">> = erlang:term_to_json(-100),
    <<"1234">> = erlang:term_to_json(1234),
    <<"4321">> = erlang:term_to_json(4321),
    <<"5678">> = erlang:term_to_json(5678),
    <<"8765">> = erlang:term_to_json(8765),
    <<"9999">> = erlang:term_to_json(9999),
    <<"10000">> = erlang:term_to_json(10000),
    <<"100000">> = erlang:term_to_json(100000),
    <<"1000000">> = erlang:term_to_json(1000000),
    <<"10000000">> = erlang:term_to_json(10000000),
    <<"100000000">> = erlang:term_to_json(100000000),
    <<"1000000000">> = erlang:term_to_json(1000000000),
    <<"1234567890">> = erlang:term_to_json(1234567890),
    <<"2147483647">> = erlang:term_to_json(2147483647),
    <<"2147483648">> = erlang:term_to_json(2147483648),
    <<"-2147483647">> = erlang:term_to_json(-2147483647),
    <<"-2147483648">> = erlang:term_to_json(-2147483648),
    <<"-2147483649">> = erlang:term_to_json(-2147483649),
    << "576460752303423487">> = erlang:term_to_json(576460752303423487), % 2^59 - 1.
    <<"-576460752303423488">> = erlang:term_to_json(-576460752303423488), % - 2^59.

    <<"1011121314">> = erlang:term_to_json(1011121314),
    <<"1516171819">> = erlang:term_to_json(1516171819),
    <<"2021222324">> = erlang:term_to_json(2021222324),
    <<"2526272829">> = erlang:term_to_json(2526272829),
    <<"3031323334">> = erlang:term_to_json(3031323334),
    <<"3536373839">> = erlang:term_to_json(3536373839),
    <<"4041424344">> = erlang:term_to_json(4041424344),
    <<"4546474849">> = erlang:term_to_json(4546474849),
    <<"5051525354">> = erlang:term_to_json(5051525354),
    <<"5556575859">> = erlang:term_to_json(5556575859),
    <<"6061626364">> = erlang:term_to_json(6061626364),
    <<"6566676869">> = erlang:term_to_json(6566676869),
    <<"7071727374">> = erlang:term_to_json(7071727374),
    <<"7576777879">> = erlang:term_to_json(7576777879),
    <<"8081828384">> = erlang:term_to_json(8081828384),
    <<"8586878889">> = erlang:term_to_json(8586878889),
    <<"9091929394">> = erlang:term_to_json(9091929394),
    <<"9596979899">> = erlang:term_to_json(9596979899),
    <<"990001020304">> = erlang:term_to_json(990001020304),
    <<"990506070809">> = erlang:term_to_json(990506070809),
    ok.

t2j_lists(Config) when is_list(Config) ->
    <<"[]">> = erlang:term_to_json([]),
    <<"[true]">> = erlang:term_to_json([true]),
    <<"[1]">> = erlang:term_to_json([1]),
    <<"[0.5]">> = erlang:term_to_json([0.5]),
    <<"[1,2]">> = erlang:term_to_json([1, 2]),
    <<"[1,2,3]">> = erlang:term_to_json([1, 2, 3]),
    <<"[true,2,\"three\"]">> = erlang:term_to_json([true, 2, <<"three">>]),
    <<"[1,2,[3,4,5],6,[[7]],[8,[9,[10],11,12]]]">> =
        erlang:term_to_json([1, 2, [3, 4, 5], 6, [[7]], [8, [9, [10], 11, 12]]]),
    ok.

t2j_objects_proplist(Config) when is_list(Config) ->
    <<"{}">> = erlang:term_to_json({[]}),
    <<"{\"one\":1}">> = erlang:term_to_json({[{<<"one">>, 1}]}),
    <<"{\"one\":1,\"two\":2}">> = erlang:term_to_json({[{<<"one">>, 1}, {<<"two">>, 2}]}),
    <<"{\"one\":1,\"two\":2,\"three\":3}">> = erlang:term_to_json({[{<<"one">>, 1}, {<<"two">>, 2}, {<<"three">>, 3}]}),
    <<"{\"empty\":[]}">> = erlang:term_to_json({[{<<"empty">>, []}]}),
    <<"{\"list\":[1,2,3]}">> = erlang:term_to_json({[{<<"list">>, [1, 2, 3]}]}),
    <<"{\"empty\":{}}">> = erlang:term_to_json({[{<<"empty">>, {[]}}]}),
    <<"{\"object\":{\"1\":\"one\"}}">> = erlang:term_to_json({[{<<"object">>, {[{<<"1">>,<<"one">>}]}}]}),
    ok.

t2j_objects_flatmap(Config) when is_list(Config) ->
    <<"{}">> = erlang:term_to_json(#{}),
    <<"{\"a\":\"apple\"}">> = erlang:term_to_json(#{<<"a">> => <<"apple">>}),
    <<"{\"a\":\"apple\",\"b\":\"banana\"}">> = erlang:term_to_json(#{<<"a">> => <<"apple">>, <<"b">> => <<"banana">>}),
    ok.

t2j_objects_hamt(Config) when is_list(Config) ->
    %% The map implementation switches to HAMT at 32 elements.
    MakeMapFun =
        fun (Values) ->
                maps:from_list(
                  lists:map(fun (N) when is_integer(N) -> {integer_to_binary(N), N};
                                (B) when is_binary(B)  -> {B, B}
                            end, Values))
        end,
    %% This may cause the test to fail if maps:to_list visits the map elements
    %% in a different order than the code in enc_json_int.
    MakeJsonFun =
        fun (Map) ->
                list_to_binary(
                  [ ${,
                    lists:join($,,
                               lists:map(
                                 fun ({K, V}) -> [ encode_to_binary(K), $:, encode_natural(V) ] end,
                                 maps:to_list(Map))),
                    $}
                  ])
        end,

    V1 = lists:seq(1, 33),
    M1 = MakeMapFun(V1),
    J1 = MakeJsonFun(M1),
    %% io:format(user, ">> ~p\n", [ J1 ]),
    J1 = erlang:term_to_json(MakeMapFun(V1)),
    %% io:format(user, ">> ~p\n", [ J1A ]),
    %% MM1 = maps:from_list(mochijson2:decode(J1A, [ {format, proplist} ])),
    %% io:format(user, "x ~p\n  ~p\n", [ M1, MM1 ]),

    V2 = lists:seq(1, 128),
    M2 = MakeMapFun(V2),
    J2 = MakeJsonFun(M2),
    J2 = erlang:term_to_json(MakeMapFun(V2)),
    %% io:format(user, ">> ~p\n", [ J2 ]),
    %% MM2 = maps:from_list(mochijson2:decode(J2, [ {format, proplist} ])),
    %% io:format(user, "x ~p\n  ~p\n", [ M2, MM2 ]),

    ok.

t2j_objects_atomkey(Config) when is_list(Config) ->
    <<"{\"a\":\"apple\"}">> = erlang:term_to_json({[{a, <<"apple">>}]}),
    <<"{\"1\":\"one\"}">> = erlang:term_to_json({[{'1', <<"one">>}]}),
    Ole = <<"¡olé!"/utf8>>,
    <<"{\"¡olé!\":\"bravo!\"}"/utf8>> = erlang:term_to_json({[{Ole, <<"bravo!">>}]}),
    <<"{\"a\":\"apple\"}">> = erlang:term_to_json(#{a => <<"apple">>}),
    ok.

t2j_unicode(Config) when is_list(Config) ->
    <<"\"\\u0000\"">> = erlang:term_to_json(list_to_binary([0])),
    <<"\"\\b\"">> = erlang:term_to_json(<<"\b">>),
    <<"\"\\n\"">> = erlang:term_to_json(<<"\n">>),
    <<"\"\\r\"">> = erlang:term_to_json(<<"\r">>),
    <<"\"\\t\"">> = erlang:term_to_json(<<"\t">>),
    <<"\"\\\"\"">> = erlang:term_to_json(<<"\"">>),
    <<"\"\\\\\"">> = erlang:term_to_json(<<"\\">>),
    <<"\"say \\\"hello, world\\n\\\", he said\"">> =
        erlang:term_to_json(<<"say \"hello, world\n\", he said">>),
    %% <<"\"<html><p>★¡Héllŏ, wōrłd!★<br />◕‿-</p></html>\""/utf8>> =
    <<34,60,104,116,109,108,62,60,112,62,226,152,133,194,161,
      72,195,169,108,108,197,143,44,32,119,197,141,114,197,
      130,100,33,226,152,133,60,98,114,32,47,62,226,151,149,
      226,128,191,45,60,47,112,62,60,47,104,116,109,108,62,34>> =
        erlang:term_to_json(<<"<html><p>★¡Héllŏ, wōrłd!★<br />◕‿-</p></html>"/utf8>>),
    <<"\"\\u0001\"">> = erlang:term_to_json(list_to_binary([1])),
    <<"\"\\u0002\"">> = erlang:term_to_json(list_to_binary([2])),
    <<"\"\\u0003\"">> = erlang:term_to_json(list_to_binary([3])),
    <<"\"\\u0004\"">> = erlang:term_to_json(list_to_binary([4])),
    <<"\"\\u0005\"">> = erlang:term_to_json(list_to_binary([5])),
    <<"\"\\u0006\"">> = erlang:term_to_json(list_to_binary([6])),
    <<"\"\\u0007\"">> = erlang:term_to_json(list_to_binary([7])),
    <<"\"\\b\"">>     = erlang:term_to_json(list_to_binary([8])),
    <<"\"\\t\"">>     = erlang:term_to_json(list_to_binary([9])),
    <<"\"\\n\"">>     = erlang:term_to_json(list_to_binary([10])),
    <<"\"\\u000B\"">> = erlang:term_to_json(list_to_binary([11])),
    <<"\"\\u000C\"">> = erlang:term_to_json(list_to_binary([12])),
    <<"\"\\r\"">>     = erlang:term_to_json(list_to_binary([13])),
    <<"\"\\u000E\"">> = erlang:term_to_json(list_to_binary([14])),
    <<"\"\\u000F\"">> = erlang:term_to_json(list_to_binary([15])),
    <<"\"\\u0010\"">> = erlang:term_to_json(list_to_binary([16])),
    <<"\"\\u0011\"">> = erlang:term_to_json(list_to_binary([17])),
    <<"\"\\u0012\"">> = erlang:term_to_json(list_to_binary([18])),
    <<"\"\\u0013\"">> = erlang:term_to_json(list_to_binary([19])),
    <<"\"\\u0014\"">> = erlang:term_to_json(list_to_binary([20])),
    <<"\"\\u0015\"">> = erlang:term_to_json(list_to_binary([21])),
    <<"\"\\u0016\"">> = erlang:term_to_json(list_to_binary([22])),
    <<"\"\\u0017\"">> = erlang:term_to_json(list_to_binary([23])),
    <<"\"\\u0018\"">> = erlang:term_to_json(list_to_binary([24])),
    <<"\"\\u0019\"">> = erlang:term_to_json(list_to_binary([25])),
    <<"\"\\u001A\"">> = erlang:term_to_json(list_to_binary([26])),
    <<"\"\\u001B\"">> = erlang:term_to_json(list_to_binary([27])),
    <<"\"\\u001C\"">> = erlang:term_to_json(list_to_binary([28])),
    <<"\"\\u001D\"">> = erlang:term_to_json(list_to_binary([29])),
    <<"\"\\u001E\"">> = erlang:term_to_json(list_to_binary([30])),
    <<"\"\\u001F\"">> = erlang:term_to_json(list_to_binary([31])),
    <<"\" \"">>       = erlang:term_to_json(list_to_binary([32])),
    <<"\"~\"">>       = erlang:term_to_json(list_to_binary([126])),
    <<"\"\\u007F\"">> = erlang:term_to_json(list_to_binary([127])),
    <<34,194,128,34>> = erlang:term_to_json(unicode:characters_to_binary([ 16#80 ])),
    <<34,223,191,34>> = erlang:term_to_json(unicode:characters_to_binary([ 16#07FF ])),
    <<34,224,160,128,34>> = erlang:term_to_json(unicode:characters_to_binary([ 16#0800 ])),
    <<34,239,191,191,34>> = erlang:term_to_json(unicode:characters_to_binary([ 16#FFFF ])),
    <<34,240,144,128,128,34>> = erlang:term_to_json(unicode:characters_to_binary([ 16#10000 ])),
    <<34,244,143,191,191,34>> = erlang:term_to_json(unicode:characters_to_binary([ 16#10FFFF ])),
    %% Code point U+110000 is too high and should fail, but doesn't:
    <<34,244,144,128,128,34>> == erlang:term_to_json(<<16#F4, 16#90, 16#80, 16#80>>),
    %% Inputs that with characters in the range 0xF5 to 0xFF produce errors.
    ?assertError(badarg, erlang:term_to_json(<<16#F5, 16#80, 16#80, 16#80>>)),
    ?assertError(badarg, erlang:term_to_json(<<16#F6, 16#80, 16#80, 16#80>>)),
    ?assertError(badarg, erlang:term_to_json(<<16#F7, 16#BF, 16#BF, 16#BF>>)),
    ?assertError(badarg, erlang:term_to_json(<<16#F8, 16#80, 16#80, 16#80, 16#80>>)),
    ?assertError(badarg, erlang:term_to_json(<<16#F9, 16#80, 16#80, 16#80, 16#80>>)),
    ?assertError(badarg, erlang:term_to_json(<<16#FA, 16#80, 16#80, 16#80, 16#80>>)),
    ?assertError(badarg, erlang:term_to_json(<<16#FB, 16#80, 16#80, 16#80, 16#80>>)),
    ?assertError(badarg, erlang:term_to_json(<<16#FC, 16#80, 16#80, 16#80, 16#80, 16#80>>)),
    ?assertError(badarg, erlang:term_to_json(<<16#FD, 16#80, 16#80, 16#80, 16#80, 16#80>>)),
    ?assertError(badarg, erlang:term_to_json(<<16#FE, 16#80, 16#80, 16#80, 16#80, 16#80, 16#80>>)),
    ?assertError(badarg, erlang:term_to_json(<<16#FF, 16#80, 16#80, 16#80, 16#80, 16#80, 16#80, 16#80>>)),
    ok.

t2j_binaries(Config) when is_list(Config) ->
    %% Encode an aligned sub-binary.
    <<_:5/binary, Str1:5/binary, _:5/binary>> = <<"abcdefghijklmno">>,
    <<"\"fghij\"">> = erlang:term_to_json(Str1),

    %% Encode a non-aligned binary.
    <<_:4, Str2:5/binary, _:4>> = <<0:4, "hello", 0:4>>,
    <<"\"hello\"">> = erlang:term_to_json(Str2),

    %% Encode a long binary.
    Str3 = list_to_binary(lists:duplicate(10000, list_to_binary(lists:seq($a, $z)))),
    E3 = <<$", Str3/binary, $">>,
    A3 = erlang:term_to_json(Str3),
    ?assertEqual(size(E3), size(A3)),
    ?assertEqual(E3, A3),

    garbage_collect(),

    ok.

t2j_bigints(Config) when is_list(Config) ->
    <<"1208925819614629174706176">> = erlang:term_to_json(1 bsl 80),
    <<"-604462909807314587353088">> = erlang:term_to_json(- 1 bsl 79),
    ok.

t2j_preencoded(Config) when is_list(Config) ->
    <<"test">> = erlang:term_to_json({json, <<"test">>}),
    <<"[1]">> = erlang:term_to_json([{json, <<"1">>}]),
    <<"[1,2,3]">> = erlang:term_to_json([{json, <<"1,2,3">>}]),
    <<"[1,2,3]">> = erlang:term_to_json([1, {json, <<"2,3">>}]),
    <<"[1,2,3]">> = erlang:term_to_json([{json, <<"1,2">>}, 3]),
    <<"[1,2,3]">> = erlang:term_to_json([1, {json, <<"2">>}, 3]),
    <<"{test}">> = erlang:term_to_json({[{json, <<"test">>}]}),
    <<"{\"t\":test}">> = erlang:term_to_json({[{<<"t">>, {json, <<"test">>}}]}),
    <<"{\"a\":1,\"b\":2}">> = erlang:term_to_json({[{json, <<"\"a\":1">>}, {<<"b">>, 2}]}),
    <<"{\"a\":1,\"b\":2}">> = erlang:term_to_json({[{<<"a">>, 1}, {json, <<"\"b\":2">>}]}),
    <<"{test}">> = erlang:term_to_json(#{json => <<"test">>}),
    LongBinary = list_to_binary([$\", lists:duplicate(1000, <<"0123456789">>), $\"]),
    LongBinaryExpect = <<$[, LongBinary/binary, $]>>,
    LongBinaryActual = erlang:term_to_json([{json, LongBinary}]),
    ?assertEqual(LongBinaryExpect, LongBinaryActual),
    ok.

t2j_use_nil(Config) when is_list(Config) ->
    <<"null">> = erlang:term_to_json(nil, [ use_nil ]),
    <<"{\"obj\":null}">> = erlang:term_to_json({[ {<<"obj">>, nil} ]}, [ use_nil ]),
    <<"{\"obj\":null}">> = erlang:term_to_json(#{<<"obj">> => nil}, [ use_nil ]),
    <<"[null]">> = erlang:term_to_json([ nil ], [ use_nil ]),
    ok.

t2j_bufsize(Config) when is_list(Config) ->
    LongBinary = list_to_binary([$\", lists:duplicate(1000, <<"0123456789">>), $\"]),
    IntList = lists:seq(1, 100),
    IntListJson = list_to_binary([ $[, lists:join($,, [ integer_to_binary(N) || N <- IntList ]), $] ]),
    lists:foreach(
      fun(N) ->
              LongBinary = erlang:term_to_json({json, LongBinary}, [ {min_buf_size, N} ]),
              IntListJson = erlang:term_to_json(IntList,  [ {min_buf_size, N} ])
      end, lists:seq(1, 127, 2)),
    ok.

t2j_errors(Config) when is_list(Config) ->
    ?assertError(badarg, erlang:term_to_json(a)),
    ?assertError(badarg, erlang:term_to_json([ 1 | 2 ])),
    ?assertError(badarg, erlang:term_to_json({[true]})),
    ?assertError(badarg, erlang:term_to_json({[{<<"a">>}]})),
    ?assertError(badarg, erlang:term_to_json({[{<<"a">>, 1, 2}]})),
    ok.

encode_to_binary(I) when is_integer(I) -> [ $", integer_to_binary(I), $" ];
encode_to_binary(B) when is_binary(B)  -> [ $", B, $" ].

encode_natural(I) when is_integer(I) -> integer_to_binary(I);
encode_natural(B) when is_binary(B) -> [ $", B, $" ].

j2t_basic_types(Config) when is_list(Config) ->
    true = erlang:json_to_term(<<"true">>),
    false = erlang:json_to_term(<<"false">>),
    null = erlang:json_to_term(<<"null">>),
    nil = erlang:json_to_term(<<"null">>, [ use_nil ]),
    1 = erlang:json_to_term(<<"1">>),
    1.1 = erlang:json_to_term(<<"1.1">>),
    <<"apple">> = erlang:json_to_term(<<"\"apple\"">>),
    %% Whitespace skipping.
    true = erlang:json_to_term(<<" true ">>),
    false = erlang:json_to_term(<<" false ">>),
    null = erlang:json_to_term(<<" null ">>),
    %% Bad values.
    ?assertError(badarg, erlang:json_to_term("nul")),
    ?assertError(badarg, erlang:json_to_term("nulll")),
    ?assertError(badarg, erlang:json_to_term("tru")),
    ?assertError(badarg, erlang:json_to_term("truex")),
    ?assertError(badarg, erlang:json_to_term("fals")),
    ?assertError(badarg, erlang:json_to_term("falsex")),
    ok.

j2t_integers(Config) when is_list(Config) ->
    0 = erlang:json_to_term(<<"0">>),
    1 = erlang:json_to_term(<<"1">>),
    -1 = erlang:json_to_term(<<"-1">>),
    9 = erlang:json_to_term(<<"9">>),
    10 = erlang:json_to_term(<<"10">>),
    99 = erlang:json_to_term(<<"99">>),
    -100 = erlang:json_to_term(<<"-100">>),
    1234 = erlang:json_to_term(<<"1234">>),
    4321 = erlang:json_to_term(<<"4321">>),
    5678 = erlang:json_to_term(<<"5678">>),
    8765 = erlang:json_to_term(<<"8765">>),
    9999 = erlang:json_to_term(<<"9999">>),
    10000 = erlang:json_to_term(<<"10000">>),
    100000 = erlang:json_to_term(<<"100000">>),
    1000000 = erlang:json_to_term(<<"1000000">>),
    10000000 = erlang:json_to_term(<<"10000000">>),
    100000000 = erlang:json_to_term(<<"100000000">>),
    1000000000 = erlang:json_to_term(<<"1000000000">>),
    1234567890 = erlang:json_to_term(<<"1234567890">>),
    2147483647 = erlang:json_to_term(<<"2147483647">>),
    2147483648 = erlang:json_to_term(<<"2147483648">>),
    -2147483647 = erlang:json_to_term(<<"-2147483647">>),
    -2147483648 = erlang:json_to_term(<<"-2147483648">>),
    -2147483649 = erlang:json_to_term(<<"-2147483649">>),
    576460752303423487 = erlang:json_to_term(<<"576460752303423487">>), % 2^59 - 1.
    -576460752303423488 = erlang:json_to_term(<<"-576460752303423488">>), % - 2^59.
    0 = erlang:json_to_term(<<"-0">>),
    %% whitespace skipping
    1 = erlang:json_to_term(<<" 1 ">>),
    %% bad whitespace
    ?assertError(badarg, erlang:json_to_term(<<"1 2">>)),
    %% illegal integers
    ?assertError(badarg, erlang:json_to_term(<<"01">>)),
    ?assertError(badarg, erlang:json_to_term(<<"--1">>)),
    ?assertError(badarg, erlang:json_to_term(<<"1-">>)),
    ?assertError(badarg, erlang:json_to_term(<<"1-1">>)),
    ok.

j2t_floats(Config) when is_list(Config) ->
    0.0 = erlang:json_to_term(<<"0.0">>),
    1.0 = erlang:json_to_term(<<"1.0">>),
    1.0 = erlang:json_to_term(<<"1e0">>),
    10.0 = erlang:json_to_term(<<"10e0">>),
    10.0 = erlang:json_to_term(<<"1e1">>),
    0.1 = erlang:json_to_term(<<"1e-1">>),
    1.1 = erlang:json_to_term(<<"11e-1">>),
    11.0 = erlang:json_to_term(<<"1.1e1">>),
    0.5 = erlang:json_to_term(<<"5.0e-1">>),
    1.0e10 = erlang:json_to_term(<<"1e10">>),
    ?assertError(badarg, erlang:json_to_term(<<"0.">>)),
    ?assertError(badarg, erlang:json_to_term(<<"5.">>)),
    ?assertError(badarg, erlang:json_to_term(<<".5">>)),
    ?assertError(badarg, erlang:json_to_term(<<".5e1">>)),
    ?assertError(badarg, erlang:json_to_term(<<"5.e1">>)),
    ?assertError(badarg, erlang:json_to_term(<<"1.2.3">>)),
    ?assertError(badarg, erlang:json_to_term(<<"1e1e1">>)),
    ?assertError(badarg, erlang:json_to_term(<<"1e1.4">>)),
    ok.

j2t_lists(Config) when is_list(Config) ->
    [] = erlang:json_to_term(<<"[]">>),
    [true] = erlang:json_to_term(<<"[true]">>),
    [1] = erlang:json_to_term(<<"[1]">>),
    [0.5] = erlang:json_to_term(<<"[0.5]">>),
    [1, 2] = erlang:json_to_term(<<"[1,2]">>),
    [1, 2, 3] = erlang:json_to_term(<<"[1,2,3]">>),
    [true, 2, <<"three">>] = erlang:json_to_term(<<"[true,2,\"three\"]">>),
    [1, 2, [3, 4, 5], 6, [[7]], [8, [9, [10], 11, 12]]] =
        erlang:json_to_term(<<"[1,2,[3,4,5],6,[[7]],[8,[9,[10],11,12]]]">>),
    %% Whitespace skipping.
    [] = erlang:json_to_term(<<" [ ] ">>),
    [1] = erlang:json_to_term(<<" [ 1 ] ">>),
    [1, 2] = erlang:json_to_term(<<" [ 1 , 2 ] ">>),
    ok.

j2t_objects_proplist(Config) when is_list(Config) ->
    {[]} = erlang:json_to_term(<<"{}">>),
    {[{<<"one">>, 1}]} = erlang:json_to_term(<<"{\"one\":1}">>),
    {[{<<"one">>, 1}, {<<"two">>, 2}]} = erlang:json_to_term(<<"{\"one\":1,\"two\":2}">>),
    {[{<<"one">>, 1}, {<<"two">>, 2}, {<<"three">>, 3}]} = erlang:json_to_term(<<"{\"one\":1,\"two\":2,\"three\":3}">>),
    {[{<<"empty">>, []}]} = erlang:json_to_term(<<"{\"empty\":[]}">>),
    {[{<<"list">>, [1, 2, 3]}]} = erlang:json_to_term(<<"{\"list\":[1,2,3]}">>),
    {[{<<"empty">>, {[]}}]} = erlang:json_to_term(<<"{\"empty\":{}}">>),
    {[{<<"object">>, {[{<<"1">>,<<"one">>}]}}]} = erlang:json_to_term(<<"{\"object\":{\"1\":\"one\"}}">>),
    %% Whitespace skipping.
    {[]} = erlang:json_to_term(<<" { } ">>),
    {[{<<"a">>, 1}]} = erlang:json_to_term(<<" { \"a\" : 1 } ">>),
    {[{<<"a">>, 1}, {<<"b">>, 2}]} = erlang:json_to_term(<<" { \"a\" : 1 , \"b\" : 2 } ">>),
    ok.

j2t_objects_map(Config) when is_list(Config) ->
    #{} = erlang:json_to_term(<<"{}">>, [return_maps]),
    #{<<"one">> := 1} = erlang:json_to_term(<<"{\"one\":1}">>, [return_maps]),
    #{<<"one">> := 1, <<"two">> := 2} = erlang:json_to_term(<<"{\"one\":1,\"two\":2}">>, [return_maps]),
    #{<<"one">> := 1, <<"two">> := 2, <<"three">> := 3} = erlang:json_to_term(<<"{\"one\":1,\"two\":2,\"three\":3}">>, [return_maps]),
    #{<<"empty">> := []} = erlang:json_to_term(<<"{\"empty\":[]}">>, [return_maps]),
    #{<<"list">> := [1, 2, 3]} = erlang:json_to_term(<<"{\"list\":[1,2,3]}">>, [return_maps]),
    #{<<"empty">> := #{}} = erlang:json_to_term(<<"{\"empty\":{}}">>, [return_maps]),
    #{<<"object">> := #{<<"1">> := <<"one">>}} = erlang:json_to_term(<<"{\"object\":{\"1\":\"one\"}}">>, [return_maps]),
    #{<<"a">> := [ 1, #{}, 2, [], 3 ], <<"b">> := #{<<"four">> := 4}} =
        erlang:json_to_term(<<"{\"a\": [1, {}, 2, [], 3], \"b\":{\"four\": 4}}">>, [return_maps]),
    ok.

j2t_string(Config) when is_list(Config) ->
    %% Decode a long binary.
    L1 = list_to_binary(lists:duplicate(10000, [ <<"\\u058D", 214, 142>> ])),
    L2 = list_to_binary(lists:duplicate(10000, [ <<214, 141, 214, 142>> ])),
    ?assertEqual(L2, erlang:json_to_term(<<34, L1/binary, 34 >>)),
    ?assertEqual(<<"a", L2/binary>>, erlang:json_to_term(<<34, "a", L1/binary, 34>>)),
    ?assertEqual(<<"ab", L2/binary>>, erlang:json_to_term(<<34, "ab", L1/binary, 34>>)),
    ?assertEqual(<<"abc", L2/binary>>, erlang:json_to_term(<<34, "abc", L1/binary, 34>>)),
    ?assertEqual(<<"abcd", L2/binary>>, erlang:json_to_term(<<34, "abcd", L1/binary, 34>>)),
    ?assertEqual(<<"abcde", L2/binary>>, erlang:json_to_term(<<34, "abcde", L1/binary, 34>>)),
    ok.

j2t_escapes(Config) when is_list(Config) ->
    <<0>> = erlang:json_to_term(<<"\"\\u0000\"">>),
    <<"\b">> = erlang:json_to_term(<<"\"\\b\"">>),
    <<"\n">> = erlang:json_to_term(<<"\"\\n\"">>),
    <<"\r">> = erlang:json_to_term(<<"\"\\r\"">>),
    <<"\t">> = erlang:json_to_term(<<"\"\\t\"">>),
    <<"\"">> = erlang:json_to_term(<<"\"\\\"\"">>),
    <<"\\">> = erlang:json_to_term(<<"\"\\\\\"">>),
    <<"/">> = erlang:json_to_term(<<"\"\\/\"">>),
    [ 16#1, 16#7F, 16#80, 16#FD, 16#7FF, 16#800, 16#FFFF ] =
        unicode:characters_to_list(erlang:json_to_term(<<"\"\\u0001\\u007f\\u0080\\u00FD\\u07FF\\u0800\\uFffF\"">>), utf8),
    [ 16#3210, 16#7654, 16#BA98, 16#FEDC, 16#ABCD, 16#00EF ] =
        unicode:characters_to_list(erlang:json_to_term(<<"\"\\u3210\\u7654\\uba98\\ufedc\\uABCD\\u00EF\"">>), utf8),

    ?assertError(badarg, erlang:json_to_term(<<$", 0, $">>)),
    ?assertError(badarg, erlang:json_to_term(<<"\"\\x\"">>)),
    ?assertError(badarg, erlang:json_to_term(<<"\"\\u1\"">>)),
    ?assertError(badarg, erlang:json_to_term(<<"\"\\u12\"">>)),
    ?assertError(badarg, erlang:json_to_term(<<"\"\\u123\"">>)),
    ?assertEqual(<<225,136,180>>, erlang:json_to_term(<<"\"\\u1234\"">>)),

    ?assertEqual(<<" ">>, erlang:json_to_term(<<"\"\\u", $0, $0, $2, $0, "\"">>)),
    ?assertError(badarg, erlang:json_to_term(<<"\"\\u", $0, $0, $g, $0, "\"">>)),
    ?assertError(badarg, erlang:json_to_term(<<"\"\\u", $0, $0, 0, $0, "\"">>)),
    ?assertError(badarg, erlang:json_to_term(<<"\"\\u", $0, $0, 127, $0, "\"">>)),
    ?assertError(badarg, erlang:json_to_term(<<"\"\\u", $0, $0, 128, $0, "\"">>)),
    ?assertError(badarg, erlang:json_to_term(<<"\"\\u", $0, $0, 255, $0, "\"">>)),

    %% UTF-16 surrogate pairs for code points >= U+10000.
    ?assertEqual(<<240, 144, 128, 128>>, erlang:json_to_term(<<"\"\\uD800\\uDC00\"">>)), % U+10000.
    ?assertEqual(<<243, 191, 191, 191>>, erlang:json_to_term(<<"\"\\uDBBF\\uDFFF\"">>)), % U+FFFFF.
    ?assertEqual(<<244, 143, 191, 191>>, erlang:json_to_term(<<"\"\\uDBFF\\uDFFF\"">>)), % U+10FFFF.


    ?assertEqual(<<237,159,191>>, erlang:json_to_term(<<"\"\\uD7FF\"">>)),
    ?assertError(badarg, erlang:json_to_term(<<"\"\\uD800\"">>)),
    ?assertError(badarg, erlang:json_to_term(<<"\"\\uDFFF\"">>)),
    ?assertEqual(<<238,128,128>>, erlang:json_to_term(<<"\"\\uE000\"">>)),
    ok.

j2t_unicode(Config) when is_list(Config) ->
    %% Two-byte UTF-8.
    ?assertEqual([ 16#80 ], unicode:characters_to_list(erlang:json_to_term(<<$", 16#C2, 16#80, $">>))),
    ?assertEqual([ 16#7FF ], unicode:characters_to_list(erlang:json_to_term(<<$", 16#DF, 16#BF, $">>))),

    %% Three-byte UTF-8.
    ?assertEqual([ 16#800 ], unicode:characters_to_list(erlang:json_to_term(<<$", 16#E0, 16#A0, 16#80, $">>))),
    ?assertEqual([ 16#2640 ], unicode:characters_to_list(erlang:json_to_term(<<$", 16#E2, 16#99, 16#80, $">>))),
    ?assertEqual([ 16#FFFF ], unicode:characters_to_list(erlang:json_to_term(<<$", 16#EF, 16#BF, 16#BF, $">>))),

    %% Four-byte UTF-8.
    ?assertEqual([ 16#10000 ], unicode:characters_to_list(erlang:json_to_term(<<$", 16#F0, 16#90, 16#80, 16#80, $">>))),
    ?assertEqual([ 16#10FFFF ], unicode:characters_to_list(erlang:json_to_term(<<$", 16#F4, 16#8F, 16#BF, 16#BF, $">>))),

    %% Code point U+110000 is too high.
    ?assertError(badarg, erlang:json_to_term(<<$", 16#F4, 16#90, 16#80, 16#80, $">>)),
    %% 0xF5 - 0xF7 would start other 4-byte UTF-8 code points that are too high.
    ?assertError(badarg, erlang:json_to_term(<<$", 16#F5, 16#80, 16#80, 16#80, $">>)),
    ?assertError(badarg, erlang:json_to_term(<<$", 16#F7, 16#BF, 16#BF, 16#BF, $">>)),
    %% 0xF8 - 0xFB would start 5-byte code points, if they were allowed.
    ?assertError(badarg, erlang:json_to_term(<<$", 16#F8, 16#80, 16#80, 16#80, 16#80, $">>)),
    ?assertError(badarg, erlang:json_to_term(<<$", 16#FB, 16#80, 16#80, 16#80, 16#80, $">>)),
    %% 0xFC and 0xFD would start 6-byte code points, if they were allowed.
    ?assertError(badarg, erlang:json_to_term(<<$", 16#FC, 16#80, 16#80, 16#80, 16#80, 16#80, $">>)),
    ?assertError(badarg, erlang:json_to_term(<<$", 16#FD, 16#80, 16#80, 16#80, 16#80, 16#80, $">>)),
    %% 0xFE would start 7-byte code points, if it was allowed.
    ?assertError(badarg, erlang:json_to_term(<<$", 16#FE, 16#80, 16#80, 16#80, 16#80, 16#80, 16#80, $">>)),
    %% There's not really a sequence that follows the pattern that begins with 0xFF.
    ?assertError(badarg, erlang:json_to_term(<<$", 16#FF, 16#80, 16#80, 16#80, 16#80, 16#80, 16#80, 16#80, $">>)),

    %% U+FFFF, U+FFFE, and U+FFFD are allowed.
    ?assertEqual([ 16#FFFF, 16#FFFE, 16#FFFD ],
                 unicode:characters_to_list(
                   erlang:json_to_term(<<"\"\\uFFFF\\uFFFE\\uFFFD\"">>))),
    ?assertEqual([ 16#FFFF, 16#FFFE, 16#FFFD ],
                 unicode:characters_to_list(
                   erlang:json_to_term(<<"\"", (unicode:characters_to_binary([ 16#FFFF, 16#FFFE, 16#FFFD ]))/binary, "\"">>))),

    %% UTF-16 surrogate pairs.
    ?assertEqual(<<237,159,191>>, erlang:json_to_term(<<$", 16#ED, 16#9F, 16#BF, $">>)), % U+D7FF: OK, below range
    ?assertError(badarg,          erlang:json_to_term(<<$", 16#ED, 16#A0, 16#80, $">>)), % U+D800: fails
    ?assertError(badarg,          erlang:json_to_term(<<$", 16#ED, 16#BF, 16#BF, $">>)), % U+DFFF: fails
    ?assertEqual(<<238,128,128>>, erlang:json_to_term(<<$", 16#EE, 16#80, 16#80, $">>)), % U+E000: OK, above range

    %% Bad UTF-8.
    ?assertError(badarg, erlang:json_to_term(<<$", 16#80, $">>)), % bare continuation byte
    ?assertError(badarg, erlang:json_to_term(<<$", 16#BF, $">>)), % bare continuation byte
    ?assertError(badarg, erlang:json_to_term(<<$", 16#C0, $">>)), % illegal byte
    ?assertError(badarg, erlang:json_to_term(<<34, 16#C1, 16#81, 34>>)), % long encoding for "A"
    ?assertError(badarg, erlang:json_to_term(<<$", 16#E0, 16#9F, 16#BF, $">>)), % long three-byte encoding
    ?assertError(badarg, erlang:json_to_term(<<$", 16#F0, 16#8F, 16#BF, 16#BF, $">>)), % long four-byte encoding
    ?assertError(badarg, erlang:json_to_term(<<$", 16#C2, $">>)), % missing continuation byte
    ?assertError(badarg, erlang:json_to_term(<<$", 16#C2, 16#7F, $">>)), % bad continuation byte
    ok.

j2t_errors(Config) when is_list(Config) ->
    ?assertError(badarg, erlang:json_to_term(<<"">>)),
    ?assertError(badarg, erlang:json_to_term(<<"stuff">>)),
    ?assertError(badarg, erlang:json_to_term(<<"\"">>)),
    ?assertError(badarg, erlang:json_to_term(<<"{">>)),
    ?assertError(badarg, erlang:json_to_term(<<"}">>)),
    ?assertError(badarg, erlang:json_to_term(<<"[">>)),
    ?assertError(badarg, erlang:json_to_term(<<"]">>)),
    ?assertError(badarg, erlang:json_to_term(<<"{]">>)),
    ?assertError(badarg, erlang:json_to_term(<<"[}">>)),
    ?assertError(badarg, erlang:json_to_term(<<"[{]">>)),
    ?assertError(badarg, erlang:json_to_term(<<"{[}">>)),
    ?assertError(badarg, erlang:json_to_term(<<"[0,]">>)),
    ?assertError(badarg, erlang:json_to_term(<<"{\"k\":1,}">>)),
    ?assertError(badarg, erlang:json_to_term(<<"{0:1}">>)),
    ?assertError(badarg, erlang:json_to_term(<<"{true:1}">>)),
    ?assertError(badarg, erlang:json_to_term(<<"{false:1}">>)),
    ?assertError(badarg, erlang:json_to_term(<<"{null:1}">>)),
    ?assertError(badarg, erlang:json_to_term(<<"\"hello">>)),
    ok.

-record(gen_config, {object_type :: proplist | map}).

%% Keep calling the function `Fun' as long as it returns `true'.  When it
%% returns `false', return the number of times `Fun' was called.
do_while(Fun, Count) -> case Fun() of true -> do_while(Fun, Count + 1); false -> Count end.

random_round_trip(Config) when is_list(Config) ->
    TimeoutSecs = case os:getenv("JSON_TIME") of false -> 10; T -> list_to_integer(T) end,
    ct:timetrap(erlang:convert_time_unit(TimeoutSecs + 2, second, millisecond)),
    TimeLimit = erlang:monotonic_time() + erlang:convert_time_unit(TimeoutSecs, second, native),

    Count =
        do_while(
          fun () ->
                  {ObjectType, DecodeOpts} =
                      case rand:uniform() < 0.5 of
                          true  -> {proplist, []};
                          false -> {map, [ return_maps ]}
                      end,
                  EJson = gen_json(100, #gen_config{object_type = ObjectType}),
                  ?assertEqual(EJson, erlang:json_to_term(erlang:term_to_json(EJson), DecodeOpts)),
                  erlang:monotonic_time() < TimeLimit
          end, 0),
    ct:log(info, ?LOW_IMPORTANCE, "ran ~p loops in ~p sec\n", [ Count, TimeoutSecs ]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Generate a random UTF-8 binary.
gen_utf8(0, Acc) ->
    unicode:characters_to_binary(Acc, unicode);
gen_utf8(Len, Acc) ->
    case rand:uniform(16#FFFFFFFF) of
        R when R < 16#FF000000 -> gen_utf8(Len - 1, [ 16#20    + R rem (16#80 - 16#20)         | Acc ]);
        R when R < 16#FFF00000 -> gen_utf8(Len - 1, [ 16#80    + R rem (16#07FF - 16#80)       | Acc ]);
        R when R < 16#FFFF0000 -> case 16#800 + R rem (16#FFFF - 16#800) of
                                      %% Avoid surrogate pair range 0xD800 to 0xDFFF.
                                      CC when CC >= 16#D800 andalso CC < 16#E000 -> C = CC - 16#8000;
                                      C -> ok
                                  end,
                                  gen_utf8(Len - 1, [ C                                        | Acc ]);
        R                      -> gen_utf8(Len - 1, [ 16#10000 + R rem (16#10FFFF - 16#100000) | Acc ])
    end.

%% Generate a random unique binary for use as an object key.
gen_key(N) ->
    %% Generate a StrLen that is biased toward short lengths.
    StrLen = 3 + floor(100 * math:pow(rand:uniform(), 2.0)),
    %% Append a character to ensure that every key is unique.  We don't want
    %% the test to fail because of a key collision.
    gen_utf8(StrLen, [ N + 48 ]).

fold(_Fun, Acc={0, _}) -> Acc;
fold(Fun, Acc) -> fold(Fun, Fun(Acc)).

gen_value(N, Config, Depth) ->
    case rand:uniform(if Depth =:= 0 -> 2; true -> 16 end) of
        1 ->
            %% Return a list.
            fold(
              fun ({NIn, Acc}) ->
                      {NOut, Value} = gen_value(NIn, Config, Depth + 1),
                      {NOut, [ Value | Acc ]}
              end, {N, []});
        2 ->
            %% Return an object.
            case Config of
                #gen_config{object_type=proplist} ->
                    {NN, KVList} =
                        fold(
                          fun ({NIn, Acc}) ->
                                  {NOut, Value} = gen_value(NIn, Config, Depth + 1),
                                  {NOut, [ {gen_key(NIn), Value} | Acc ]}
                          end, {N, []}),
                    {NN, {KVList}};
                #gen_config{object_type=map} ->
                    fold(
                      fun ({NIn, Acc}) ->
                              {NOut, Value} = gen_value(NIn, Config, Depth + 1),
                              {NOut, maps:put(gen_key(NIn), Value, Acc)}
                      end, {N, maps:new()})
            end;
        R when R < 8 ->
            %% Return an integer.
            {N - 1, rand:uniform(100000)};
        R when R < 12 ->
            %% Return a float.
            {N - 1, trunc(rand:uniform() * 1.0e14) / 1.0e6};
        _ ->
            %% Return a string.
            {N - 1, gen_key(N)}
    end.

gen_json(N, Config) ->
    {0, V} = gen_value(N, Config, 0),
    V.
