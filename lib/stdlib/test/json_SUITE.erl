%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
%% % @format
%%
%%%----------------------------------------------------------------
%%% Purpose: Test suite for the 'json' module.
%%%-----------------------------------------------------------------

-module(json_SUITE).
-include_lib("stdlib/include/assert.hrl").

%% Test server specific exports
-export([all/0, suite/0, groups/0, init_per_group/2, end_per_group/2]).

%% Test cases must be exported.
-export([
    test_encode_atom/1,
    test_encode_integer/1,
    test_encode_float/1,
    test_encode_binary/1,
    test_encode_map/1,
    test_encode_list/1,
    test_encode_proplist/1,
    test_encode_escape_all/1,
    test_format_list/1,
    test_format_proplist/1,
    test_format_map/1,
    test_format_fun/1,
    test_decode_atoms/1,
    test_decode_numbers/1,
    test_decode_strings/1,
    test_decode_arrays/1,
    test_decode_objects/1,
    test_decode_whitespace/1,
    test_decode_api/1,
    test_decode_api_stream/1,
    test_json_test_suite/1,
    counterexamples/1,
    property_string_roundtrip/1,
    property_integer_roundtrip/1,
    property_float_roundtrip/1,
    property_object_roundtrip/1,
    property_escape_all/1,
    error_info/1
]).


-define(is_ws(X), X =:= $\s; X =:= $\t; X =:= $\r; X =:= $\n).

suite() ->
    [
        {ct_hooks, [ts_install_cth]},
        {timetrap, {minutes, 4}}
    ].

all() ->
    [
        {group, encode},
        {group, decode},
        {group, format},
        test_json_test_suite,
        {group, properties},
        counterexamples,
        error_info
    ].

groups() ->
    [
        {encode, [parallel], [
            test_encode_atom,
            test_encode_integer,
            test_encode_float,
            test_encode_binary,
            test_encode_map,
            test_encode_list,
            test_encode_proplist,
            test_encode_escape_all
        ]},
        {format, [parallel], [
            test_format_list,
            test_format_proplist,
            test_format_map,
            test_format_fun
        ]},

        {decode, [parallel], [
            test_decode_atoms,
            test_decode_numbers,
            test_decode_strings,
            test_decode_arrays,
            test_decode_objects,
            test_decode_whitespace,
            test_decode_api,
            test_decode_api_stream
        ]},
        {properties, [parallel], [
            property_string_roundtrip,
            property_integer_roundtrip,
            property_float_roundtrip,
            property_object_roundtrip,
            property_escape_all
        ]}
    ].

init_per_group(properties, Config) ->
    %% this works well in init_per_group as well as per suite
    %% we only want to skip this group, if the tool is not found
    ct_property_test:init_per_suite(Config);
init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) -> ok.

%%
%% Encoding tests
%%

test_encode_atom(_Config) ->
    ?assertEqual(<<"true">>, encode(true)),
    ?assertEqual(<<"false">>, encode(false)),
    ?assertEqual(<<"null">>, encode(null)),
    ?assertEqual(<<"\"json\"">>, encode(json)),
    ?assertEqual(<<"\"☃a\""/utf8>>, encode('☃a')).

test_encode_integer(_Config) ->
    ?assertEqual(<<"123">>, encode(123)),
    ?assertEqual(<<"-123">>, encode(-123)),
    ?assertEqual(<<"0">>, encode(0)).

test_encode_float(_Config) ->
    ?assertEqual(<<"0.0">>, encode(0.0)),
    ?assertEqual(<<"-0.0">>, encode(-0.0)),
    ?assertEqual(<<"99.99">>, encode(99.99)),
    ?assertEqual(<<"9.9e100">>, encode(9.9e100)),
    ?assertEqual(<<"9.9e-100">>, encode(9.9e-100)).

test_encode_binary(_Config) ->
    ?assertEqual(<<"\"hello world\"">>, encode(<<"hello world">>)),
    ?assertEqual(<<"\"hello\\nworld\"">>, encode(<<"hello\nworld">>)),
    ?assertEqual(<<"\"\\nhello\\nworld\\n\"">>, encode(<<"\nhello\nworld\n">>)),

    ?assertEqual(<<"\"\\\"\"">>, encode(<<"\"">>)),
    ?assertEqual(<<"\"\\\\\"">>, encode(<<"\\">>)),
    ?assertEqual(<<"\"\\b\"">>, encode(<<"\b">>)),
    ?assertEqual(<<"\"\\f\"">>, encode(<<"\f">>)),
    ?assertEqual(<<"\"\\n\"">>, encode(<<"\n">>)),
    ?assertEqual(<<"\"\\r\"">>, encode(<<"\r">>)),
    ?assertEqual(<<"\"\\t\"">>, encode(<<"\t">>)),
    ?assertEqual(<<"\"\\u0000\"">>, encode(<<"\0">>)),
    ?assertEqual(<<"\"\\u001F\"">>, encode(<<"\x1F">>)),

    ?assertEqual(<<"\"/\"">>, encode(<<"/">>)),

    ?assertEqual(<<"\"áéíóúàèìòùâêîôûãẽĩõũ\""/utf8>>, encode(<<"áéíóúàèìòùâêîôûãẽĩõũ"/utf8>>)),
    ?assertEqual(<<"\"☃a\""/utf8>>, encode(<<"☃a"/utf8>>)),

    ?assertError({unsupported_type, <<0:1>>}, encode(<<0:1>>)),

    %% invalid 1st byte
    ?assertError({invalid_byte, $\xa0}, encode(<<"\xa0\xa1">>)),

    ?assertEqual(<<"\"\xc3\xb1\"">>, encode(<<"\xc3\xb1">>)),
    %% invalid 2nd octet in 2-byte sequence
    ?assertError({invalid_byte, $\x28}, encode(<<"\xc3\x28">>)),

    ?assertEqual(<<"\"\xe2\x82\xa1\"">>, encode(<<"\xe2\x82\xa1">>)),
    %% invalid 2nd octet in 3-byte sequence
    ?assertError({invalid_byte, $\x28}, encode(<<"\xe2\x28\xa1">>)),
    %% invalid 3rd octet in 3-byte sequence
    ?assertError({invalid_byte, $\x28}, encode(<<"\xe2\x82\x28">>)),

    ?assertEqual(<<"\"\xf0\x90\x8c\xbc\"">>, encode(<<"\xf0\x90\x8c\xbc">>)),
    %% invalid 2nd octet in 4-byte sequence
    ?assertError({invalid_byte, $\x28}, encode(<<"\xf0\x28\x8c\xbc">>)),
    %% invalid 3rd octet in 4-byte sequence
    ?assertError({invalid_byte, $\x28}, encode(<<"\xf0\x90\x28\xbc">>)),
    %% invalid 4th octet in 4-byte sequence
    ?assertError({invalid_byte, $\x28}, encode(<<"\xf0\x28\x8c\x28">>)),

    %% too-long sequences
    ?assertError({invalid_byte, $\xf8}, encode(<<"\xf8\xa1\xa1\xa1\xa1">>)),
    ?assertError({invalid_byte, $\xfc}, encode(<<"\xfc\xa1\xa1\xa1\xa1\xa1">>)),

    %% overlong
    ?assertError({invalid_byte, $\xc0}, encode(<<"\xc0\x80">>)),
    ?assertError({invalid_byte, $\x80}, encode(<<"\xe0\x80\x80">>)),
    ?assertError({invalid_byte, $\x80}, encode(<<"\xf0\x80\x80\x80">>)),

    %% surrogate halves
    ?assertError({invalid_byte, $\xa0}, encode(<<"\xed\xa0\x80">>)),
    ?assertError({invalid_byte, $\xaf}, encode(<<"\xed\xaf\xbf">>)),
    ?assertError({invalid_byte, $\xb0}, encode(<<"\xed\xb0\x80">>)),
    ?assertError({invalid_byte, $\xbf}, encode(<<"\xed\xbf\xbf">>)).

test_encode_escape_all(_Config) ->
    ?assertEqual(<<"\"\\u2603a\"">>, encode_escape_all('☃a')),
    ?assertEqual(<<"\"\\u2603a\"">>, encode_escape_all(<<"☃a"/utf8>>)),
    ?assertEqual(<<"\"\\uD834\\uDD1Eb\"">>, encode_escape_all(<<"𝄞b"/utf8>>)),

    %% because of how /utf8 works, we always report 1st byte as invalid

    %% invalid 1st byte
    ?assertError({invalid_byte, $\xa0}, encode_escape_all(<<"\xa0\xa1">>)),

    ?assertEqual(<<"\"\\u00F1\"">>, encode_escape_all(<<"\xc3\xb1">>)),
    %% invalid 2nd octet in 2-byte sequence
    ?assertError({invalid_byte, $\xc3}, encode_escape_all(<<"\xc3\x28">>)),

    ?assertEqual(<<"\"\\u20A1\"">>, encode_escape_all(<<"\xe2\x82\xa1">>)),
    %% invalid 2nd octet in 3-byte sequence
    ?assertError({invalid_byte, $\xe2}, encode_escape_all(<<"\xe2\x28\xa1">>)),
    %% invalid 3rd octet in 3-byte sequence
    ?assertError({invalid_byte, $\xe2}, encode_escape_all(<<"\xe2\x82\x28">>)),

    ?assertEqual(<<"\"\\uD800\\uDF3C\"">>, encode_escape_all(<<"\xf0\x90\x8c\xbc">>)),
    %% invalid 2nd octet in 4-byte sequence
    ?assertError({invalid_byte, $\xf0}, encode_escape_all(<<"\xf0\x28\x8c\xbc">>)),
    %% invalid 3rd octet in 4-byte sequence
    ?assertError({invalid_byte, $\xf0}, encode_escape_all(<<"\xf0\x90\x28\xbc">>)),
    %% invalid 4th octet in 4-byte sequence
    ?assertError({invalid_byte, $\xf0}, encode_escape_all(<<"\xf0\x28\x8c\x28">>)),

    %% too-long sequences
    ?assertError({invalid_byte, $\xf8}, encode_escape_all(<<"\xf8\xa1\xa1\xa1\xa1">>)),
    ?assertError({invalid_byte, $\xfc}, encode_escape_all(<<"\xfc\xa1\xa1\xa1\xa1\xa1">>)),

    %% overlong
    ?assertError({invalid_byte, $\xc0}, encode_escape_all(<<"\xc0\x80">>)),
    ?assertError({invalid_byte, $\xe0}, encode_escape_all(<<"\xe0\x80\x80">>)),
    ?assertError({invalid_byte, $\xf0}, encode_escape_all(<<"\xf0\x80\x80\x80">>)),

    %% surrogate halves
    ?assertError({invalid_byte, $\xed}, encode_escape_all(<<"\xed\xa0\x80">>)),
    ?assertError({invalid_byte, $\xed}, encode_escape_all(<<"\xed\xaf\xbf">>)),
    ?assertError({invalid_byte, $\xed}, encode_escape_all(<<"\xed\xb0\x80">>)),
    ?assertError({invalid_byte, $\xed}, encode_escape_all(<<"\xed\xbf\xbf">>)).

test_encode_map(_Config) ->
    ?assertEqual(<<"{}">>, encode(#{})),
    ?assertEqual(<<"{\"foo\":\"bar\"}">>, encode(#{<<"foo">> => <<"bar">>})),
    ?assertEqual(<<"{\"foo\":\"bar\"}">>, encode(#{foo => bar})),
    ?assertEqual(<<"{\"42\":\"bar\"}">>, encode(#{42 => bar})),
    ?assertEqual(<<"{\"foo\":\"bar\"}">>, encode_checked(#{<<"foo">> => <<"bar">>})),
    ?assertEqual(<<"{\"foo\":\"bar\"}">>, encode_checked(#{foo => bar})),
    ?assertEqual(<<"{\"42\":\"bar\"}">>, encode_checked(#{42 => bar})),

    MultiKeyMap = #{<<"foo">> => <<"foo1">>, foo => <<"foo2">>},
    ?assertError({duplicate_key, <<"foo">>}, encode_checked(MultiKeyMap)),
    ?assertEqual(<<"{\"foo\":\"foo2\",\"foo\":\"foo1\"}">>, encode(MultiKeyMap)).

test_encode_list(_Config) ->
    ?assertEqual(<<"[]">>, encode([])),
    ?assertEqual(<<"[1,2,3]">>, encode([1, 2, 3])).

test_encode_proplist(_Config) ->
    ?assertError({unsupported_type, {a, 1}}, encode([{a, 1}])),
    ?assertEqual(<<"{\"a\":1}">>, encode_proplist([{a, 1}])),
    ?assertEqual(<<"{\"a\":1}">>, encode_proplist_checked([{a, 1}])),

    MultiKeyProplist = [{<<"foo">>, <<"foo1">>}, {foo, <<"foo2">>}],
    ?assertError({duplicate_key, foo}, encode_proplist_checked(MultiKeyProplist)),
    ?assertEqual(<<"{\"foo\":\"foo1\",\"foo\":\"foo2\"}">>, encode_proplist(MultiKeyProplist)).

encode(Term) -> iolist_to_binary(json:encode(Term)).

encode_escape_all(Term) ->
    Encode = fun
        (Binary, _) when is_binary(Binary) -> json:encode_binary_escape_all(Binary);
        (Other, Encode) -> json:encode_value(Other, Encode)
    end,
    iolist_to_binary(json:encode(Term, Encode)).

encode_checked(Term) ->
    Encode = fun
        (Map, Encode) when is_map(Map) -> json:encode_map_checked(Map, Encode);
        (Other, Encode) -> json:encode_value(Other, Encode)
    end,
    iolist_to_binary(json:encode(Term, Encode)).

encode_proplist(Term) ->
    Encode = fun
        ([{_, _} | _], Encode) -> json:encode_key_value_list(Term, Encode);
        (Other, Encode) -> json:encode_value(Other, Encode)
    end,
    iolist_to_binary(json:encode(Term, Encode)).

encode_proplist_checked(Term) ->
    Encode = fun
        ([{_, _} | _], Encode) -> json:encode_key_value_list_checked(Term, Encode);
        (Other, Encode) -> json:encode_value(Other, Encode)
    end,
    iolist_to_binary(json:encode(Term, Encode)).

%%
%% Formatting tests
%%

format(Term) -> iolist_to_binary(json:format(Term)).
format(Term, Arg) -> iolist_to_binary(json:format(Term, Arg)).

test_format_list(_Config) ->
    ?assertEqual(~"[]\n", format([])),

    List10 = ~'[1,2,3,4,5,6,7,8,9,10]\n',
    ?assertEqual(List10, format(lists:seq(1,10))),

    ListWithLists = ~"""
    [
      [1,2],
      [3,4]
    ]

    """,
    ?assertEqual(ListWithLists, format([[1,2],[3,4]])),

    ListWithListWithList = ~"""
    [
      [
        []
      ],
      [
        [3,4]
      ]
    ]

    """,
    ?assertEqual(ListWithListWithList, format([[[]],[[3,4]]])),

    ListWithMap = ~"""
    [
      { "key": 1 }
    ]

    """,
    ?assertEqual(ListWithMap, format([#{key => 1}])),

    ListList10 = ~"""
    [
        [1,2,3,4,5,
            6,7,8,9,
            10]
    ]

    """,
    ?assertEqual(ListList10, format([lists:seq(1,10)], #{indent => 4, max => 14})),

    ListString = ~"""
    [
       "foo",
       "bar",
       "baz"
    ]

    """,
    ?assertEqual(ListString, format([~"foo", ~"bar", ~"baz"], #{indent => 3})),
    ok.

test_format_proplist(_Config) ->
    Formatter = fun({kvlist, KVList}, Fun, State) ->
                        json:format_key_value_list(KVList, Fun, State);
                   ({kvlist_checked, KVList}, Fun, State) ->
                        json:format_key_value_list_checked(KVList, Fun, State);
                   (Other, Fun, State) ->
                        json:format_value(Other, Fun, State)
                end,

    ?assertEqual(~"""
                  {
                    "a": 1,
                    "b": "str"
                  }

                  """, format({kvlist, [{a, 1}, {b, ~"str"}]}, Formatter)),

    ?assertEqual(~"""
                  {
                    "a": 1,
                    "b": "str"
                  }

                  """, format({kvlist_checked, [{a, 1}, {b, ~"str"}]}, Formatter)),

    ?assertEqual(~"""
                  {
                    "10": 1.0,
                    "1.0": 10,
                    "a": "αβ",
                    "αβ": "a"
                  }

                  """, format({kvlist, [{10, 1.0},
                                        {1.0, 10},
                                        {a, ~"αβ"},
                                        {~"αβ", a}
                                       ]}, Formatter)),

    ?assertEqual(~"""
                  {
                    "10": 1.0,
                    "1.0": 10,
                    "a": "αβ",
                    "αβ": "a"
                  }

                  """, format({kvlist_checked, [{10, 1.0},
                                                {1.0, 10},
                                                {a, ~"αβ"},
                                                {~"αβ", a}
                                               ]}, Formatter)),

    ?assertEqual(~"""
                  {
                    "a": 1,
                    "b": {
                      "aa": 10,
                      "bb": 20
                    },
                    "c": "str"
                  }

                  """, format({kvlist, [{a, 1},
                                        {b, {kvlist, [{aa, 10}, {bb, 20}]}},
                                        {c, ~"str"}
                                       ]}, Formatter)),

    ?assertEqual(~"""
                  [{
                      "a1": 1,
                      "b1": [{
                          "a11": 1,
                          "b11": 2
                        },{
                          "a12": 3,
                          "b12": 4
                        }],
                      "c1": "str1"
                    },
                    {
                      "a2": 2,
                      "b2": [{
                          "a21": 5,
                          "b21": 6
                        },{
                          "a22": 7,
                          "b22": 8
                        }],
                      "c2": "str2"
                    }]

                  """, format([{kvlist, [{a1, 1},
                                         {b1, [{kvlist, [{a11, 1}, {b11, 2}]},
                                               {kvlist, [{a12, 3}, {b12, 4}]}
                                              ]},
                                         {c1, ~"str1"}
                                        ]},
                               {kvlist, [{a2, 2},
                                         {b2, [{kvlist, [{a21, 5}, {b21, 6}]}
                                              ,{kvlist, [{a22, 7}, {b22, 8}]}
                                              ]},
                                         {c2, ~"str2"}
                                        ]}
                              ], Formatter)),

    ?assertEqual(~"""
                  {
                    "a": 1,
                    "b": {
                      "aa": 10,
                      "bb": 20
                    },
                    "c": "str"
                  }

                  """, format({kvlist_checked, [{a, 1},
                                                {b, {kvlist_checked, [{aa, 10}, {bb,20}]}},
                                                {c, ~"str"}
                                               ]}, Formatter)),

    ?assertEqual(~"""
                  [{
                      "a1": 1,
                      "b1": [{
                          "a11": 1,
                          "b11": 2
                        },{
                          "a12": 3,
                          "b12": 4
                        }],
                      "c1": "str1"
                    },
                    {
                      "a2": 2,
                      "b2": [{
                          "a21": 5,
                          "b21": 6
                        },{
                          "a22": 7,
                          "b22": 8
                        }],
                      "c2": "str2"
                    }]

                  """, format([{kvlist_checked,
                                [{a1, 1},
                                 {b1, [{kvlist_checked, [{a11, 1}, {b11, 2}]},
                                       {kvlist_checked, [{a12, 3}, {b12, 4}]}
                                      ]},
                                 {c1, ~"str1"}
                                ]},
                               {kvlist_checked,
                                [{a2, 2},
                                 {b2, [{kvlist_checked, [{a21, 5}, {b21, 6}]}
                                      ,{kvlist_checked, [{a22, 7}, {b22, 8}]}
                                      ]},
                                 {c2, ~"str2"}
                                ]}
                              ], Formatter)),


    ?assertError({duplicate_key, a},
                 format({kvlist_checked, [{a, 1}, {b, ~"str"}, {a, 2}]}, Formatter)),

    %% on invalid input exact error is not specified
    ?assertError(_, format({kvlist, [{a, 1}, b]}, Formatter)),

    ?assertError(_, format({kvlist, x}, Formatter)),

    ?assertError(_, format({kvlist, [{#{}, 1}]}, Formatter)),

    ?assertError(_, format({kvlist_checked, [{a, 1}, b]}, Formatter)),

    ?assertError(_, format({kvlist_checked, x}, Formatter)),

    ?assertError(_, format({kvlist_checked, [{#{}, 1}]}, Formatter)),

    ok.

test_format_map(_Config) ->
    ?assertEqual(~'{}\n', format(#{})),
    ?assertEqual(~'{ "key": "val" }\n', format(#{key => val})),
    MapSingleMap = ~"""
    {
      "key1": { "key3": "val3" },
      "key2": 42
    }

    """,
    ?assertEqual(MapSingleMap, format(#{key1 => #{key3 => val3}, key2 => 42})),

    MapNestedMap = ~"""
    {
      "key1": {
        "key3": true,
        "key4": {
          "deep1": 4711,
          "deep2": "string"
        }
      },
      "key2": 42
    }

    """,
    ?assertEqual(MapNestedMap, format(#{key1 => #{key3 => true,
                                                  key4 => #{deep1 => 4711, deep2 => ~'string'}},
                                        key2 => 42})),
    MapIntList =  ~"""
    {
      "key1": [1,2,3,4,5],
      "key2": 42
    }

    """,
    ?assertEqual(MapIntList, format(#{key1 => lists:seq(1,5),
                                      key2 => 42})),

    MapObjList =  ~"""
    {
      "key1": [
        {
          "key3": true,
          "key4": [1,2,3,4,5]
        },
        {
          "key3": true,
          "key4": [1,2,3,4,5]
        }
      ],
      "key2": 42
    }

    """,
    ?assertEqual(MapObjList, format(#{key1 =>
                                          [#{key3 => true, key4 => lists:seq(1,5)},
                                           #{key3 => true, key4 => lists:seq(1,5)}],
                                      key2 => 42})),

    MapObjList2 =  ~"""
    {
     "key1": [
      {
       "key3": true,
       "key4": [1,2,
        3,4,5,6,7,8,
        9,10]
      },
      {
       "key3": true,
       "key_longer_name": [
        1,
        2,
        3
       ]
      }
     ],
     "key2": 42
    }

    """,
    ?assertEqual(MapObjList2, format(#{key1 =>
                                          [#{key3 => true, key4 => lists:seq(1,10)},
                                           #{key3 => true, key_longer_name => lists:seq(1,3)}],
                                      key2 => 42},
                                     #{indent => 1, max => 15}
                                   )),
    ok.


-record(rec, {a,b,c}).

test_format_fun(_Config) ->
    All = #{types => [[], #{}, true, false, null, #{foo => ~"baz"}],
            numbers => [1, -10, 0.0, -0.0, 2.0, -2.0],
            strings => [~"three", ~"åäö", ~"mixed_Ω"],
            user_data => #rec{a = 1, b = 2, c = 3}
           },
    Formatter = fun(#rec{a=A, b=B, c=C}, _Fun, _State) ->
                        List = [{type, rec}, {a, A}, {b, B}, {c, C}],
                        encode_proplist(List);
                   (Other, Fun, State) ->
                        json:format_value(Other, Fun, State)
                end,
    Formatted = ~"""
    {
      "numbers": [1,-10,0.0,-0.0,2.0,-2.0],
      "strings": [
        "three",
        "åäö",
        "mixed_Ω"
      ],
      "types": [
        [],
        {},
        true,
        false,
        null,
        { "foo": "baz" }
      ],
      "user_data": {"type":"rec","a":1,"b":2,"c":3}
    }

    """,
    ?assertEqual(Formatted, format(All, Formatter)),
    ok.


%%
%% Decoding tests
%%

test_decode_atoms(_Config) ->
    ?assertEqual(true, decode(<<"true">>)),
    ?assertEqual(false, decode(<<"false">>)),
    ?assertEqual(null, decode(<<"null">>)).

test_decode_numbers(_Config) ->
    ?assertError(unexpected_end, decode(<<"-">>)),
    ?assertError({invalid_byte, $-}, decode(<<"--1">>)),
    ?assertError({invalid_byte, $1}, json:decode(<<"01">>)),
    ?assertError({invalid_byte, $.}, decode(<<".1">>)),
    ?assertError(unexpected_end, decode(<<"1.">>)),
    ?assertError(unexpected_end, decode(<<"1e">>)),
    ?assertError(unexpected_end, decode(<<"1.0e+">>)),
    ?assertError({unexpected_sequence, <<"1.0e999">>}, decode(<<"1e999">>)),

    ?assertEqual(0, decode(<<"0">>)),
    ?assertEqual(1, decode(<<"1">>)),
    ?assertEqual(0, decode(<<"-0">>)),
    ?assertEqual(-1, decode(<<"-1">>)),
    ?assertEqual(0.0, decode(<<"0.0">>)),
    ?assertEqual(-0.0, decode(<<"-0.0">>)),
    ?assertEqual(0.1, decode(<<"0.1">>)),
    ?assertEqual(-0.1, decode(<<"-0.1">>)),
    ?assertEqual(0.0, decode(<<"0e0">>)),
    ?assertEqual(0.0, decode(<<"0E0">>)),
    ?assertEqual(1.0, decode(<<"1e0">>)),
    ?assertEqual(1.0, decode(<<"1E0">>)),
    ?assertEqual(1.0, decode(<<"1.0e0">>)),
    ?assertEqual(1.0, decode(<<"1e+0">>)),
    ?assertEqual(1.0, decode(<<"1.0e+0">>)),
    ?assertEqual(0.1e1, decode(<<"0.1e1">>)),
    ?assertEqual(0.1e-1, decode(<<"0.1e-1">>)),
    ?assertEqual(99.99e99, decode(<<"99.99e99">>)),
    ?assertEqual(-99.99e-99, decode(<<"-99.99e-99">>)),
    ?assertEqual(123456789.123456789e123, decode(<<"123456789.123456789e123">>)).

test_decode_strings(_Config) ->
    ?assertError(unexpected_end, decode(<<"\"">>)),
    ?assertError(unexpected_end, decode(<<"\"\\\"">>)),
    ?assertError({invalid_byte, $k}, decode(<<"\"\\k\"">>)),
    ?assertError({invalid_byte, $\r}, decode(<<"\"a\r\"">>)),
    ?assertError({invalid_byte, $\x80}, decode(<<"\"\x80\"">>)),
    ?assertError({invalid_byte, $\x1F}, decode(<<"\"\x1F\"">>)),
    ?assertError(unexpected_end, decode(<<"\"\\u2603\\\"">>)),
    ?assertError(unexpected_end, decode(<<"\"Here's a snowman for you: ☃. Good day!"/utf8>>)),
    ?assertError(unexpected_end, decode(<<"\"𝄞"/utf8>>)),
    ?assertError({unexpected_sequence, <<"\\ud8aa\\udcxx">>}, decode(<<"\"\\ud8aa\\udcxx\"">>)),
    ?assertError({unexpected_sequence, <<"\\ud8aa\\uda00">>}, decode(<<"\"\\ud8aa\\uda00\"">>)),
    ?assertError({unexpected_sequence, <<"\\uxxxx">>}, decode(<<"\"\\uxxxx\"">>)),

    ?assertEqual(<<"\"\\/\b\f\n\r\t">>, decode(<<"\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"">>)),
    ?assertEqual(<<"☃"/utf8>>, decode(<<"\"\\u2603\"">>)),
    ?assertEqual(<<"\x{2028}\x{2029}"/utf8>>, decode(<<"\"\\u2028\\u2029\"">>)),
    ?assertEqual(<<"𝄞"/utf8>>, decode(<<"\"\\uD834\\uDD1E\"">>)),
    ?assertEqual(<<"𝄞"/utf8>>, decode(<<"\"\\ud834\\udd1e\"">>)),
    ?assertEqual(<<"힙힙"/utf8>>, decode(<<"\"\\uD799\\uD799\"">>)),
    ?assertEqual(<<"✔"/utf8>>, decode(<<"\"✔\""/utf8>>)),

    %% test-case with & without \n to test copying & non-copying implementation

    %% invalid 1st byte
    ?assertError({invalid_byte, $\xa0}, decode(<<"\"\xa0\xa1\"">>)),
    ?assertError({invalid_byte, $\xa0}, decode(<<"\"\\n\xa0\xa1\"">>)),

    ?assertEqual(<<"\xc3\xb1">>, decode(<<"\"\xc3\xb1\"">>)),
    ?assertEqual(<<"\n\xc3\xb1">>, decode(<<"\"\\n\xc3\xb1\"">>)),
    %% invalid 2nd octet in 2-byte sequence
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\xc3\x28\"">>)),
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\\n\xc3\x28\"">>)),

    ?assertEqual(<<"\xe2\x82\xa1">>, decode(<<"\"\xe2\x82\xa1\"">>)),
    ?assertEqual(<<"\n\xe2\x82\xa1">>, decode(<<"\"\\n\xe2\x82\xa1\"">>)),
    %% invalid 2nd octet in 3-byte sequence
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\xe2\x28\xa1\"">>)),
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\\n\xe2\x28\xa1\"">>)),
    %% invalid 3rd octet in 3-byte sequence
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\xe2\x82\x28\"">>)),
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\\n\xe2\x82\x28\"">>)),

    ?assertEqual(<<"\xf0\x90\x8c\xbc">>, decode(<<"\"\xf0\x90\x8c\xbc\"">>)),
    ?assertEqual(<<"\n\xf0\x90\x8c\xbc">>, decode(<<"\"\\n\xf0\x90\x8c\xbc\"">>)),
    %% invalid 2nd octet in 4-byte sequence
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\xf0\x28\x8c\xbc\"">>)),
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\\n\xf0\x28\x8c\xbc\"">>)),
    %% invalid 3rd octet in 4-byte sequence
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\xf0\x90\x28\xbc\"">>)),
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\\n\xf0\x90\x28\xbc\"">>)),
    %% invalid 4th octet in 4-byte sequence
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\xf0\x28\x8c\x28\"">>)),
    ?assertError({invalid_byte, $\x28}, decode(<<"\"\\n\xf0\x28\x8c\x28\"">>)),

    %% too-long sequences
    ?assertError({invalid_byte, $\xf8}, decode(<<"\"\xf8\xa1\xa1\xa1\xa1\"">>)),
    ?assertError({invalid_byte, $\xf8}, decode(<<"\"\\n\xf8\xa1\xa1\xa1\xa1\"">>)),
    ?assertError({invalid_byte, $\xfc}, decode(<<"\"\xfc\xa1\xa1\xa1\xa1\xa1\"">>)),
    ?assertError({invalid_byte, $\xfc}, decode(<<"\"\\n\xfc\xa1\xa1\xa1\xa1\xa1\"">>)),

    %% overlong
    ?assertError({invalid_byte, $\xc0}, decode(<<"\"\xc0\x80\"">>)),
    ?assertError({invalid_byte, $\xc0}, decode(<<"\"\\n\xc0\x80\"">>)),
    ?assertError({invalid_byte, $\x80}, decode(<<"\"\xe0\x80\x80\"">>)),
    ?assertError({invalid_byte, $\x80}, decode(<<"\"\\n\xe0\x80\x80\"">>)),
    ?assertError({invalid_byte, $\x80}, decode(<<"\"\xf0\x80\x80\x80\"">>)),
    ?assertError({invalid_byte, $\x80}, decode(<<"\"\\n\xf0\x80\x80\x80\"">>)),

    %% surrogate halves
    ?assertError({invalid_byte, $\xa0}, decode(<<"\"\xed\xa0\x80\"">>)),
    ?assertError({invalid_byte, $\xa0}, decode(<<"\"\\n\xed\xa0\x80\"">>)),
    ?assertError({invalid_byte, $\xaf}, decode(<<"\"\xed\xaf\xbf\"">>)),
    ?assertError({invalid_byte, $\xaf}, decode(<<"\"\\n\xed\xaf\xbf\"">>)),
    ?assertError({invalid_byte, $\xb0}, decode(<<"\"\xed\xb0\x80\"">>)),
    ?assertError({invalid_byte, $\xb0}, decode(<<"\"\\n\xed\xb0\x80\"">>)),
    ?assertError({invalid_byte, $\xbf}, decode(<<"\"\xed\xbf\xbf\"">>)).

test_decode_arrays(_Config) ->
    ?assertError(unexpected_end, decode(<<"[">>)),
    ?assertError({invalid_byte, $,}, decode(<<"[,">>)),
    ?assertError({invalid_byte, $]}, decode(<<" ]">>)),
    ?assertError(unexpected_end, decode(<<"[1,">>)),

    ?assertEqual([], decode(<<"[]">>)),
    ?assertEqual([1, 2, 3], decode(<<"[1,2,3]">>)),
    ?assertEqual([<<"foo">>, <<"bar">>, <<"baz">>], decode(<<"[\"foo\", \"bar\", \"baz\"]">>)),
    ?assertEqual([#{<<"foo">> => <<"bar">>}], decode(<<"[{\"foo\":\"bar\"}]">>)).

test_decode_objects(_Config) ->
    ?assertError(unexpected_end, decode(<<"{">>)),
    ?assertError({invalid_byte, $,}, decode(<<"{,">>)),
    ?assertError({invalid_byte, $}}, decode(<<"{\"foo\"}">>)),
    ?assertError({invalid_byte, $}}, decode(<<"{\"foo\":}">>)),
    ?assertError({invalid_byte, $}}, decode(<<"{\"foo\":1,}">>)),

    ?assertEqual(#{}, decode(<<"{}">>)),
    ?assertEqual(#{<<"foo">> => <<"bar">>}, decode(<<"{\"foo\": \"bar\"}">>)),
    ?assertEqual(#{<<"foo">> => <<"bar">>}, decode(<<"{\"foo\" : \"bar\"}">>)),
    ?assertEqual(#{<<"foo">> => #{}}, decode(<<"{\"foo\":{}}">>)),
    ?assertEqual(#{<<"foo">> => []}, decode(<<"{\"foo\":[]}">>)),
    ?assertEqual(#{<<"foo">> => 1, <<"bar">> => 2}, decode(<<"{\"foo\":1,\"bar\":2}">>)).

test_decode_whitespace(_Config) ->
    ?assertError(unexpected_end, decode(<<"">>)),
    ?assertError(unexpected_end, decode(ews(<<" ">>))),

    ?assertEqual([], decode(ews(<<" [ ] ">>))),
    ?assertEqual(#{}, decode(ews(<<" { } ">>))),
    ?assertEqual([1, 2], decode(ews(<<" [ 1 , 2 ] ">>))),
    ?assertEqual(#{<<"foo">> => 1, <<"bar">> => 2}, decode(ews(<<" { \"foo\" : 1 , \"bar\" : 2 } ">>))).

%% add extra whitespace
ews(Str) ->
    unicode:characters_to_binary(string:replace(Str, <<" ">>, <<" \s\t\r\n">>, all)).

test_decode_api(_Config) ->
    put(history, []),
    ArrayStart = set_history1(array_start, fun({_, Int}) ->
        {[], Int + 1}
    end),
    ArrayPush = set_history2(array_push, fun(Val, {Acc, Int}) ->
        {[Val | Acc], Int + 1}
    end),
    ArrayFinish = set_history2(array_finish, fun({Acc, Int}, {OldAcc, _OldInt}) ->
        {lists:reverse(Acc), {OldAcc, Int + 1}}
    end),
    ObjectStart = set_history1(object_start, fun({_, Int}) ->
        {[], Int + 1}
    end),
    ObjectPush = set_history3(object_push, fun(Key, Val, {Acc, Int}) ->
        {[{Key, Val} | Acc], Int + 1}
    end),
    ObjectFinish = set_history2(object_finish, fun({Acc, Int}, {OldAcc, _OldInt}) ->
        {maps:from_list(Acc), {OldAcc, Int + 1}}
    end),
    Float = set_history1(float, fun binary_to_float/1),
    Integer = set_history1(integer, fun binary_to_integer/1),
    String = set_history1(string, fun binary_to_atom/1),
    Decoders = #{
        array_start => ArrayStart,
        array_push => ArrayPush,
        array_finish => ArrayFinish,
        object_start => ObjectStart,
        object_push => ObjectPush,
        object_finish => ObjectFinish,
        float => Float,
        integer => Integer,
        string => String,
        null => nil
    },

    Data = <<"{\"a\": [[], {}, true, false, null, {\"foo\": \"baz\"}], \"b\": [1, 2.0, \"three\", 0]}">>,
    {Decoded, Acc, <<>>} = json:decode(Data, {[], 0}, Decoders),
    ?assertEqual({[], 25}, Acc),
    Expected = #{a => [[], #{}, true, false, nil, #{foo => baz}], b => [1, 2.0, three, 0]},
    ?assertEqual(Expected, Decoded),
    ExpectedHistory =
        [
            {object_start, {[], 0}, {[], 1}},
            {string, <<"a">>, a},
            {array_start, {[], 1}, {[], 2}},
            {array_start, {[], 2}, {[], 3}},
            {array_finish, {{[], 3}, {[], 2}}, {[], {[], 4}}},
            {array_push, {[], {[], 4}}, {[[]], 5}},
            {object_start, {[[]], 5}, {[], 6}},
            {object_finish, {{[], 6}, {[[]], 5}}, {#{}, {[[]], 7}}},
            {array_push, {#{}, {[[]], 7}}, {[#{}, []], 8}},
            {array_push, {true, {[#{}, []], 8}}, {[true, #{}, []], 9}},
            {array_push, {false, {[true, #{}, []], 9}}, {[false, true, #{}, []], 10}},
            {array_push, {nil, {[false, true, #{}, []], 10}}, {[nil, false, true, #{}, []], 11}},
            {object_start, {[nil, false, true, #{}, []], 11}, {[], 12}},
            {string, <<"foo">>, foo},
            {string, <<"baz">>, baz},
            {object_push, {foo, baz, {[], 12}}, {[{foo, baz}], 13}},
            {object_finish, {{[{foo, baz}], 13}, {[nil, false, true, #{}, []], 11}},
                {#{foo => baz}, {[nil, false, true, #{}, []], 14}}},
            {array_push, {#{foo => baz}, {[nil, false, true, #{}, []], 14}}, {
                [#{foo => baz}, nil, false, true, #{}, []], 15
            }},
            {array_finish, {{[#{foo => baz}, nil, false, true, #{}, []], 15}, {[], 1}},
                {[[], #{}, true, false, nil, #{foo => baz}], {[], 16}}},
            {object_push, {a, [[], #{}, true, false, nil, #{foo => baz}], {[], 16}}, {
                [{a, [[], #{}, true, false, nil, #{foo => baz}]}], 17
            }},
            {string, <<"b">>, b},
            {array_start, {[{a, [[], #{}, true, false, nil, #{foo => baz}]}], 17}, {[], 18}},
            {integer, <<"1">>, 1},
            {array_push, {1, {[], 18}}, {[1], 19}},
            {float, <<"2.0">>, 2.0},
            {array_push, {2.0, {[1], 19}}, {[2.0, 1], 20}},
            {string, <<"three">>, three},
            {array_push, {three, {[2.0, 1], 20}}, {[three, 2.0, 1], 21}},
            {integer, <<"0">>, 0},
            {array_push, {0, {[three, 2.0, 1], 21}}, {[0, three, 2.0, 1], 22}},
            {array_finish, {{[0, three, 2.0, 1], 22}, {[{a, [[], #{}, true, false, nil, #{foo => baz}]}], 17}},
                {[1, 2.0, three, 0], {[{a, [[], #{}, true, false, nil, #{foo => baz}]}], 23}}},
            {object_push, {b, [1, 2.0, three, 0], {[{a, [[], #{}, true, false, nil, #{foo => baz}]}], 23}}, {
                [{b, [1, 2.0, three, 0]}, {a, [[], #{}, true, false, nil, #{foo => baz}]}], 24
            }},
            {object_finish, {{[{b, [1, 2.0, three, 0]}, {a, [[], #{}, true, false, nil, #{foo => baz}]}], 24}, {[], 0}},
                {#{a => [[], #{}, true, false, nil, #{foo => baz}], b => [1, 2.0, three, 0]}, {[], 25}}}
        ],
    ?assertEqual(ExpectedHistory, lists:reverse(get(history))).

set_history1(Ty, Fun) ->
    fun(Arg) -> set_history(Ty, Arg, Fun(Arg)) end.

set_history2(Ty, Fun) ->
    fun(Arg1, Arg2) -> set_history(Ty, {Arg1, Arg2}, Fun(Arg1, Arg2)) end.

set_history3(Ty, Fun) ->
    fun(Arg1, Arg2, Arg3) -> set_history(Ty, {Arg1, Arg2, Arg3}, Fun(Arg1, Arg2, Arg3)) end.

set_history(Ty, Acc, Res) ->
    History = get(history),
    Entry = {Ty, Acc, Res},
    put(history, [Entry | History]),
    Res.

test_decode_api_stream(_Config) ->
    Types = ~#{"types": [[], {}, true, false, null, {"foo": "baz"}],
               "numbers": [1, -10, 0.0, -0.0, 2.0, -2.0, 31e2, 31e-2, 0.31e2, -0.31e2, 0.13e-2],
               "strings": ["three", "åäö", "mixed_Ω"],
               "escaped": ["\\n", "\\u2603", "\\ud834\\uDD1E", "\\n\xc3\xb1"]
              }
             #,
    ok = stream_decode(Types),

    {12345, ok, B1} = json:decode(ews(~# 12345 "foo" #), ok, #{}),
    <<" \s\t\r\n", _/binary>> = B1,
    {<<"foo">>, ok, <<>>} = json:decode(B1, ok, #{}),

    Multiple = ~#12345 1.30 "String1" -0.31e2\n["an array"]12345\n#,
    ok = multi_stream_decode(Multiple),
    ok.


decode(Bin) ->
    try json:decode(Bin) of
        Result ->
            {Res, [], <<>>} = byte_loop(Bin),
            ?assertEqual(Result, Res, "Stream decode failed"),
            Result
    catch Class:Reason:ST ->
            ?assertError(Reason, byte_loop(Bin)),
            erlang:raise(Class, Reason, ST)
    end.

stream_decode(Str) ->
    {R1, [], <<>>} = byte_loop(Str),
    case json:decode(Str) of
        R1 ->
            ok;
        R2 ->
            io:format("~p ~p~n",[R1,R2]),
            error
    end.

multi_stream_decode(<<>>) ->
    ok;
multi_stream_decode(Strs) ->
    {R1, [], ContBin} = byte_loop(Strs),
    case json:decode(Strs, [], #{}) of
        {R1, [], ContBin} ->
            multi_stream_decode(ContBin);
        Other ->
            io:format("~p '~tp'~n~p~n", [R1,ContBin, Other]),
            error
    end.

byte_loop(Bin) ->
    {continue, State} = json:decode_start(<<>>, [], #{}),
    byte_loop(Bin, State, []).

byte_loop(<<Byte, Rest/binary>> = Orig, State0, Bytes) ->
    %% io:format("cont with '~s'  ~p~n",[lists:reverse([Byte|Bytes]), State0]),
    case json:decode_continue(<<Byte>>, State0) of
        {continue, State} ->
            byte_loop(Rest, State, [Byte|Bytes]);
        {Result, [], <<>>} ->
            %% trim to match the binary in return value
            case string:trim(Rest, leading) of
                <<>> ->
                    {Result, [], <<>>};
                _ when ?is_ws(Byte) ->
                    {Result, [], Orig};
                _ ->
                    {Result, [], Rest}
            end
    end;
byte_loop(<<>>, State, _Bytes) ->
    json:decode_continue(end_of_input, State).

%%
%% JSON SUITE tests
%%

test_json_test_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    TestFiles = filelib:wildcard("*.json", DataDir),
    Checker = fun(File) ->
        Type = test_type(File),
        {ok, Data} = file:read_file(filename:join(DataDir, File)),
        test_file(Type, File, Data)
    end,
    lists:foreach(Checker, TestFiles).

test_type("y_" ++ _) -> yes;
test_type("n_" ++ _) -> no;
test_type("i_number_double_huge_neg_exp.json") -> yes;
test_type("i_number_real_underflow.json") -> yes;
test_type("i_number_too_big_neg_int.json") -> yes;
test_type("i_number_too_big_pos_int.json") -> yes;
test_type("i_number_very_big_negative_int.json") -> yes;
test_type("i_structure_500_nested_arrays.json") -> yes;
test_type("i_" ++ _) -> no.

test_file(yes, File, Data) ->
    Parsed = decode(Data),
    ?assertEqual(Parsed, decode(iolist_to_binary(encode(Parsed))), File),
    ?assertEqual(Parsed, decode(iolist_to_binary(json:format(Parsed))), File);
test_file(no, File, Data) ->
    ?assertError(_, decode(Data), File).

error_info(_) ->
    L = [{decode, [~'["valid string", not_valid'], [allow_rename, unexplained]}],
    error_info_lib:test_error_info(json, L,  [allow_nyi]).


%%
%% Property tests
%%

property_string_roundtrip(Config) ->
    ct_property_test:quickcheck(json_prop:prop_string_roundtrip(), Config).

property_integer_roundtrip(Config) ->
    ct_property_test:quickcheck(json_prop:prop_integer_roundtrip(), Config).

property_float_roundtrip(Config) ->
    ct_property_test:quickcheck(json_prop:prop_float_roundtrip(), Config).

property_object_roundtrip(Config) ->
    ct_property_test:quickcheck(json_prop:prop_object_roundtrip(), [{numtests, 250} | Config]).

property_escape_all(Config) ->
    ct_property_test:quickcheck(json_prop:prop_escape_all(), Config).

counterexamples(_Config) ->
    Value = [[], 0],
    ?assertEqual(Value, decode_io(encode(Value))).

decode_io(IOList) -> decode(iolist_to_binary(IOList)).
