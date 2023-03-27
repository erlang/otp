-module(erl_desugar_interpolation_SUITE).

-export([all/0,suite/0]).

-export([empty/1, head/1, head_subs_no_tail/1, head_subs_tail/1,
         head_subs_cont_subs_tail/1, multiline_head_subs_cont_subs_tail/1,
         lots_of_subs/1, tilde_can_be_escaped/1,
         unicode_list_string_subs_in_list/1, utf8_binary_string_subs_in_list/1,
         unicode_list_string_subs_in_tuple/1, utf8_binary_string_subs_in_tuple/1,
         variable_subs/1, macro_subs/1, special_characters/1, all_types_subs/1,
         all_production_format_types_subs/1, floats_round_trip_and_are_the_same_between_lists_and_binaries/1,
         block_substitutions/1, function_call_substitutions/1,
         back_to_back_substitutions/1,
         homogenous_interpolations_inside_interpolations/1,
         heterogenous_interpolations_inside_interpolations/1]).

-include_lib("stdlib/include/assert.hrl").

suite() ->
    [{timetrap,{minutes,1}}].

all() ->
    [ empty, head, head_subs_no_tail, head_subs_tail,
      head_subs_cont_subs_tail, multiline_head_subs_cont_subs_tail,
      lots_of_subs, tilde_can_be_escaped,
      unicode_list_string_subs_in_list, utf8_binary_string_subs_in_list,
      unicode_list_string_subs_in_tuple, utf8_binary_string_subs_in_tuple,
      variable_subs, macro_subs, special_characters, all_types_subs,
      all_production_format_types_subs, floats_round_trip_and_are_the_same_between_lists_and_binaries,
      block_substitutions, function_call_substitutions,
      back_to_back_substitutions,
      homogenous_interpolations_inside_interpolations,
      heterogenous_interpolations_inside_interpolations
    ].

-define(assertEqualStr(A,B), ?assertEqual(A,lists:flatten(B))).

empty(Config) when is_list(Config) ->
  ?assertEqual(
    <<>>,
    bf""
  ),
  ?assertEqualStr(
    "",
    lf""
  ),
  ?assertEqual(
    <<>>,
    bd""
  ),
  ?assertEqualStr(
    "",
    ld""
  ).

head(Config) when is_list(Config) ->
  ?assertEqual(
    <<"A head"/utf8>>,
    bf"A head"
  ),
  ?assertEqualStr(
    "A head",
    lf"A head"
  ),
  ?assertEqual(
    <<"A head"/utf8>>,
    bd"A head"
  ),
  ?assertEqualStr(
    "A head",
    ld"A head"
  ).

head_subs_tail(Config) when is_list(Config) ->
  ?assertEqual(
    <<"Two plus two is 4!"/utf8>>,
    bf"Two plus two is ~2 + 2~!"
  ),
  ?assertEqualStr(
    "Two plus two is 4!",
    lf"Two plus two is ~2 + 2~!"
  ),
  ?assertEqual(
    <<"Two plus two is 4!"/utf8>>,
    bd"Two plus two is ~2 + 2~!"
  ),
  ?assertEqualStr(
    "Two plus two is 4!",
    ld"Two plus two is ~2 + 2~!"
  ).

head_subs_no_tail(Config) when is_list(Config) ->
  ?assertEqual(
    <<"Two plus two is 4"/utf8>>,
    bf"Two plus two is ~2 + 2~"
  ),
  ?assertEqualStr(
    "Two plus two is 4",
    lf"Two plus two is ~2 + 2~"
  ),
  ?assertEqual(
    <<"Two plus two is 4"/utf8>>,
    bd"Two plus two is ~2 + 2~"
  ),
  ?assertEqualStr(
    "Two plus two is 4",
    ld"Two plus two is ~2 + 2~"
  ).

head_subs_cont_subs_tail(Config) when is_list(Config) ->
  ?assertEqual(
    <<"Two plus two is 4, and three times three is 9!"/utf8>>,
    bf"Two plus two is ~2 + 2~, and three times three is ~3 * 3~!"
  ),
  ?assertEqualStr(
    "Two plus two is 4, and three times three is 9!",
    lf"Two plus two is ~2 + 2~, and three times three is ~3 * 3~!"
  ),
  ?assertEqual(
    <<"Two plus two is 4, and three times three is 9!"/utf8>>,
    bd"Two plus two is ~2 + 2~, and three times three is ~3 * 3~!"
  ),
  ?assertEqualStr(
    "Two plus two is 4, and three times three is 9!",
    ld"Two plus two is ~2 + 2~, and three times three is ~3 * 3~!"
  ).

multiline_head_subs_cont_subs_tail(Config) when is_list(Config) ->
  ?assertEqual(
    <<"Two plus two is 4,
       and three times three is 9!"/utf8>>,
    bf"Two plus two is ~2 + 2~,
       and three times three is ~3 * 3~!"
  ),
  ?assertEqualStr(
    "Two plus two is 4,
       and three times three is 9!",
    lf"Two plus two is ~2 + 2~,
       and three times three is ~3 * 3~!"
  ),
  ?assertEqual(
    <<"Two plus two is 4,
       and three times three is 9!"/utf8>>,
    bd"Two plus two is ~2 + 2~,
       and three times three is ~3 * 3~!"
  ),
  ?assertEqualStr(
    "Two plus two is 4,
       and three times three is 9!",
    ld"Two plus two is ~2 + 2~,
       and three times three is ~3 * 3~!"
  ).

lots_of_subs(Config) when is_list(Config) ->
  ?assertEqual(
    <<"A 4, B 6, C 81, D -6, E 0, F 8"/utf8>>,
    bf"A ~2 + 2~, B ~3 + 3~, C ~9 * 9~, D ~-6~, E ~0~, F ~2 * (2 + 2)~"
  ),
  ?assertEqualStr(
    "A 4, B 6, C 81, D -6, E 0, F 8",
    lf"A ~2 + 2~, B ~3 + 3~, C ~9 * 9~, D ~-6~, E ~0~, F ~2 * (2 + 2)~"
  ),
  ?assertEqual(
    <<"A 4, B 6, C 81, D -6, E 0, F 8"/utf8>>,
    bd"A ~2 + 2~, B ~3 + 3~, C ~9 * 9~, D ~-6~, E ~0~, F ~2 * (2 + 2)~"
  ),
  ?assertEqualStr(
    "A 4, B 6, C 81, D -6, E 0, F 8",
    ld"A ~2 + 2~, B ~3 + 3~, C ~9 * 9~, D ~-6~, E ~0~, F ~2 * (2 + 2)~"
  ).

tilde_can_be_escaped(Config) when is_list(Config) ->
  ?assertEqual(
    <<"Two plus t\~wo is ~4!"/utf8>>,
    bf"Two plus t\~wo is \~~2 + 2~!"
  ),
  ?assertEqualStr(
    "Two plus t\~wo is ~4!",
    lf"Two plus t\~wo is \~~2 + 2~!"
  ),
  ?assertEqual(
    <<"Two plus t\~wo is ~4!"/utf8>>,
    bd"Two plus t\~wo is \~~2 + 2~!"
  ),
  ?assertEqualStr(
    "Two plus t\~wo is ~4!",
    ld"Two plus t\~wo is \~~2 + 2~!"
  ).

unicode_list_string_subs_in_list(Config) when is_list(Config) ->
  ?assertEqual(
    <<"Emoji: 🙂👍"/utf8>>,
    bf"Emoji: ~[$🙂, $👍]~"
  ),
  ?assertEqualStr(
    "Emoji: 🙂👍",
    lf"Emoji: ~[$🙂, $👍]~"
  ),
  ?assertEqual(
    <<"Emoji: [128578,128077]"/utf8>>,
    bd"Emoji: ~[$🙂, $👍]~"
  ),
  ?assertEqualStr(
    "Emoji: [128578,128077]",
    ld"Emoji: ~[$🙂, $👍]~"
  ).

utf8_binary_string_subs_in_list(Config) when is_list(Config) ->
  ?assertEqual(
    <<"Emoji: 🙂👍"/utf8>>,
    bf"Emoji: ~<<"🙂👍"/utf8>>~"
  ),
  ?assertEqualStr(
    "Emoji: 🙂👍",
    lf"Emoji: ~<<"🙂👍"/utf8>>~"
  ),
  ?assertEqual(
    <<"Emoji: <<240,159,153,130,240,159,145,141>>"/utf8>>,
    bd"Emoji: ~<<"🙂👍"/utf8>>~"
  ),
  ?assertEqualStr(
    "Emoji: <<240,159,153,130,240,159,145,141>>",
    ld"Emoji: ~<<"🙂👍"/utf8>>~"
  ).

unicode_list_string_subs_in_tuple(Config) when is_list(Config) ->
  ?assertError(
    badarg,
    bf"Emoji: ~{a,[$🙂,$👍],c}~"
  ),
  ?assertError(
    badarg,
    lf"Emoji: ~{a,[$🙂,$👍],c}~"
  ),
  ?assertEqual(
    <<"Emoji: {a,[128578,128077],c}"/utf8>>,
    bd"Emoji: ~{a,[$🙂,$👍],c}~"
  ),
  ?assertEqualStr(
    "Emoji: {a,[128578,128077],c}",
    ld"Emoji: ~{a,[$🙂,$👍],c}~"
  ).

utf8_binary_string_subs_in_tuple(Config) when is_list(Config) ->
  ?assertError(
    badarg,
    bf"Emoji: ~{a,<<"🙂👍"/utf8>>,c}~"
  ),
  ?assertError(
    badarg,
    lf"Emoji: ~{a,<<"🙂👍"/utf8>>,c}~"
  ),
  ?assertEqual(
    <<"Emoji: {a,<<240,159,153,130,240,159,145,141>>,c}"/utf8>>,
    bd"Emoji: ~{a,<<"🙂👍"/utf8>>,c}~"
  ),
  ?assertEqualStr(
    "Emoji: {a,<<240,159,153,130,240,159,145,141>>,c}",
    ld"Emoji: ~{a,<<"🙂👍"/utf8>>,c}~"
  ).

variable_subs(Config) when is_list(Config) ->
  X = 2 + 2,
  ?assertEqual(
    <<"X: 4"/utf8>>,
    bf"X: ~X~"
  ),
  ?assertEqualStr(
    "X: 4",
    lf"X: ~X~"
  ),
  ?assertEqual(
    <<"X: 4"/utf8>>,
    bd"X: ~X~"
  ),
  ?assertEqualStr(
    "X: 4",
    ld"X: ~X~"
  ).

macro_subs(Config) when is_list(Config) ->
  ?assertEqual(
    <<"This module: erl_desugar_interpolation_SUITE"/utf8>>,
    bf"This module: ~?MODULE~"
  ),
  ?assertEqualStr(
    "This module: erl_desugar_interpolation_SUITE",
    lf"This module: ~?MODULE~"
  ),
  ?assertEqual(
    <<"This module: erl_desugar_interpolation_SUITE"/utf8>>,
    bd"This module: ~?MODULE~"
  ),
  ?assertEqualStr(
    "This module: erl_desugar_interpolation_SUITE",
    ld"This module: ~?MODULE~"
  ).

special_characters(Config) when is_list(Config) ->
  ?assertEqual(
    <<"\nstuff\tmore stuff: \neven more stuff"/utf8>>,
    bf"\nstuff\tmore stuff: ~<<"\n"/utf8>>~even more stuff"
  ),
  ?assertEqualStr(
    "\nstuff\tmore stuff: \neven more stuff",
    lf"\nstuff\tmore stuff: ~<<"\n"/utf8>>~even more stuff"
  ),
  ?assertEqual(
    <<"\nstuff\tmore stuff: <<10>>even more stuff"/utf8>>,
    bd"\nstuff\tmore stuff: ~<<"\n"/utf8>>~even more stuff"
  ),
  ?assertEqualStr(
    "\nstuff\tmore stuff: <<10>>even more stuff",
    ld"\nstuff\tmore stuff: ~<<"\n"/utf8>>~even more stuff"
  ).

back_to_back_substitutions(Config) when is_list(Config) ->
  ?assertEqual(
    <<"Here are several expression back-to-back 3my_atomfoo"/utf8>>,
    bf"Here are several expression back-to-back ~1+2~~my_atom~~<<"foo"/utf8>>~"
  ),
  ?assertEqualStr(
    "Here are several expression back-to-back 3my_atomfoo",
    lf"Here are several expression back-to-back ~1+2~~my_atom~~<<"foo"/utf8>>~"
  ),
  ?assertEqual(
    <<"Here are several expression back-to-back 3my_atom<<102,111,111>>"/utf8>>,
    bd"Here are several expression back-to-back ~1+2~~my_atom~~<<"foo"/utf8>>~"
  ),
  ?assertEqualStr(
    "Here are several expression back-to-back 3my_atom<<102,111,111>>",
    ld"Here are several expression back-to-back ~1+2~~my_atom~~<<"foo"/utf8>>~"
  ).

all_production_format_types_subs(Config) when is_list(Config) ->
  ?assertEqual(
    <<"integer: 2
       atom: foo
       list-string: a string
       binary-string: another string">>,
    bf"integer: ~1 + 1~
       atom: ~foo~
       list-string: ~"a string"~
       binary-string: ~<<"another string"/utf8>>~"
    ),
  ?assertEqualStr(
    "integer: 2
       atom: foo
       list-string: a string
       binary-string: another string",
    lf"integer: ~1 + 1~
       atom: ~foo~
       list-string: ~"a string"~
       binary-string: ~<<"another string"/utf8>>~"
    ),
  ?assertEqual(
    <<"integer: 2
       atom: foo
       list-string: [97,32,115,116,114,105,110,103]
       binary-string: <<97,110,111,116,104,101,114,32,115,116,114,105,110,103>>">>,
    bd"integer: ~1 + 1~
       atom: ~foo~
       list-string: ~"a string"~
       binary-string: ~<<"another string"/utf8>>~"
    ),
  ?assertEqualStr(
    "integer: 2
       atom: foo
       list-string: [97,32,115,116,114,105,110,103]
       binary-string: <<97,110,111,116,104,101,114,32,115,116,114,105,110,103>>",
    ld"integer: ~1 + 1~
       atom: ~foo~
       list-string: ~"a string"~
       binary-string: ~<<"another string"/utf8>>~"
    ).

floats_round_trip_and_are_the_same_between_lists_and_binaries(Config) when is_list(Config) ->
  ?assertError(
    badarg,
    bf"A float: ~1000000000000 + 0.1 + 0.2~"
  ),
  ?assertError(
    badarg,
    lf"A float: ~1000000000000 + 0.1 + 0.2~"
  ),
  ?assertEqual(
    <<"A float: 1000000000000.2999"/utf8>>,
    bd"A float: ~1000000000000 + 0.1 + 0.2~"
  ),
  ?assertEqualStr(
    "A float: 1000000000000.2999",
    ld"A float: ~1000000000000 + 0.1 + 0.2~"
  ),
  ?assertEqual(
     1000000000000 + 0.1 + 0.2,
     binary_to_float(string:trim(bd"~1000000000000 + 0.1 + 0.2~", both, [$"]))
  ),
  ?assertEqual(
     1000000000000 + 0.1 + 0.2,
     list_to_float(string:trim(ld"~1000000000000 + 0.1 + 0.2~", both, [$"]))
  ),
  ?assertEqual(
     binary_to_float(string:trim(bd"~1000000000000 + 0.1 + 0.2~", both, [$"])),
     list_to_float(string:trim(ld"~1000000000000 + 0.1 + 0.2~", both, [$"]))
  ).

all_types_subs(Config) when is_list(Config) ->
  ?assertError(
    badarg,
    bf"integer: ~1 + 1~
       float: ~2.0 * 2.0~
       atom: ~foo~
       port: ~open_port({spawn, ls}, [])~
       pid: ~self()~
       ref: ~make_ref()~
       bitstring: ~<<6:4>>~
       list: ~[1,2,3]~
       tuple: ~{1,b,3.0}~
       map: ~#{key => value}~
       function: ~fun (X) -> X + 1 end~"
    ),
  ?assertError(
    badarg,
    lf"integer: ~1 + 1~
       float: ~2.0 * 2.0~
       atom: ~foo~
       port: ~open_port({spawn, ls}, [])~
       pid: ~self()~
       ref: ~make_ref()~
       bitstring: ~<<6:4>>~
       list: ~[1,2,3]~
       tuple: ~{1,b,3.0}~
       map: ~#{key => value}~
       function: ~fun (X) -> X + 1 end~"
    ),
  InterpolatedBd =
    bd"integer: ~1 + 1~
       float: ~2.0 * 2.0~
       atom: ~foo~
       port: ~open_port({spawn, ls}, [])~
       pid: ~self()~
       ref: ~make_ref()~
       bitstring: ~<<6:4>>~
       list: ~[1,2,3]~
       tuple: ~{1,b,3.0}~
       map: ~#{key => value}~
       function: ~fun (X) -> X + 1 end~",
  ?assert(
    is_binary(InterpolatedBd)
  ),
  ?assertMatch(
    {match, _},
    re:run(
      InterpolatedBd,
      <<"integer: 2
       float: 4\\.0
       atom: foo
       port: #Port<.*>
       pid: <.*>
       ref: #Ref<.*>
       bitstring: <<6:4>>
       list: \\[1,2,3\\]
       tuple: \\{1,b,3\.0\\}
       map: #\\{key => value\\}
       function: #Fun<erl_desugar_interpolation_SUITE.*>">>,
      [multiline]
     ),
    lists:flatten(io_lib:format("Interpolation (binary, debug) result was:~n~ts~n", [InterpolatedBd]))
  ),
  InterpolatedLd =
    ld"integer: ~1 + 1~
       float: ~2.0 * 2.0~
       atom: ~foo~
       port: ~open_port({spawn, ls}, [])~
       pid: ~self()~
       ref: ~make_ref()~
       bitstring: ~<<6:4>>~
       list: ~[1,2,3]~
       tuple: ~{1,b,3.0}~
       map: ~#{key => value}~
       function: ~fun (X) -> X + 1 end~",
  ?assert(
    is_list(InterpolatedLd)
  ),
  ?assertMatch(
    {match, _},
    re:run(
      InterpolatedLd,
      "integer: 2
       float: 4\\.0
       atom: foo
       port: #Port<.*>
       pid: <.*>
       ref: #Ref<.*>
       bitstring: <<6:4>>
       list: \\[1,2,3\\]
       tuple: \\{1,b,3\.0\\}
       map: #\\{key => value\\}
       function: #Fun<erl_desugar_interpolation_SUITE.*>",
      [multiline]
     ),
    lists:flatten(io_lib:format("Interpolation (list, debug) result was:~n~ts~n", [InterpolatedLd]))
  ).

function_call_substitutions(Config) when is_list(Config) ->
  ?assertEqual(
    <<"erlang:is_binary(\"hi\") = false"/utf8>>,
    bf"erlang:is_binary(\"hi\") = ~erlang:is_binary("hi")~"
  ),
  ?assertEqualStr(
    "erlang:is_binary(\"hi\") = false",
    lf"erlang:is_binary(\"hi\") = ~erlang:is_binary("hi")~"
  ),
  ?assertEqual(
    <<"erlang:is_binary(\"hi\") = false"/utf8>>,
    bd"erlang:is_binary(\"hi\") = ~erlang:is_binary("hi")~"
  ),
  ?assertEqualStr(
    "erlang:is_binary(\"hi\") = false",
    ld"erlang:is_binary(\"hi\") = ~erlang:is_binary("hi")~"
  ).

block_substitutions(Config) when is_list(Config) ->
  ?assertEqual(
    <<"block value: 7"/utf8>>,
    bf"block value: ~begin A = 6, (fun (X) -> X + 1 end)(A) end~"
  ),
  ?assertEqualStr(
    "block value: 7",
    lf"block value: ~begin A = 6, (fun (X) -> X + 1 end)(A) end~"
  ),
  ?assertEqual(
    <<"block value: 7"/utf8>>,
    bd"block value: ~begin A = 6, (fun (X) -> X + 1 end)(A) end~"
  ),
  ?assertEqualStr(
    "block value: 7",
    ld"block value: ~begin A = 6, (fun (X) -> X + 1 end)(A) end~"
  ).

homogenous_interpolations_inside_interpolations(Config) when is_list(Config) ->
  ?assertEqual(
    <<"Yo dawg, I heard you like string interpolations, so I put a string in your string so you can interpolate while you interpolate"/utf8>>,
    bf"Yo dawg, I heard you like string interpolations, ~bf"so I put a ~bf"string in your"~ string"~ so you can interpolate while you interpolate"
  ),
  ?assertEqualStr(
    "Yo dawg, I heard you like string interpolations, so I put a string in your string so you can interpolate while you interpolate",
    lf"Yo dawg, I heard you like string interpolations, ~lf"so I put a ~lf"string in your"~ string"~ so you can interpolate while you interpolate"
  ),
  ?assertEqual(
    <<"Yo dawg, I heard you like string interpolations, <<115,111,32,73,32,112,117,116,32,97,32,60,60,49,49,53,44,49,49,54,44,49,49,52,44,49,48,53,44,49,49,48,44,49,48,51,44,51,50,44,49,48,53,44,49,49,48,44,51,50,44,49,50,49,44,49,49,49,44,49,49,55,44,49,49,52,62,62,32,115,116,114,105,110,103>> so you can interpolate while you interpolate"/utf8>>,
    bd"Yo dawg, I heard you like string interpolations, ~bd"so I put a ~bd"string in your"~ string"~ so you can interpolate while you interpolate"
  ),
  ?assertEqualStr(
    "Yo dawg, I heard you like string interpolations, [[115,111,32,73,32,112,117,116,32,97,32],[91,[[49,49,53],44,[49,49,54],44,[49,49,52],44,[49,48,53],44,[49,49,48],44,[49,48,51],44,[51,50],44,[49,48,53],44,[49,49,48],44,[51,50],44,[49,50,49],44,[49,49,49],44,[49,49,55],44,[49,49,52]],93],[32,115,116,114,105,110,103]] so you can interpolate while you interpolate",
    ld"Yo dawg, I heard you like string interpolations, ~ld"so I put a ~ld"string in your"~ string"~ so you can interpolate while you interpolate"
  ).

heterogenous_interpolations_inside_interpolations(Config) when is_list(Config) ->
  ?assertEqual(
    <<"Yo dawg, I heard you like string interpolations, so I put a string in your string so you can interpolate while you interpolate"/utf8>>,
    bf"Yo dawg, I heard you like string interpolations, ~lf"so I put a ~bd"string in your"~ string"~ so you can interpolate while you interpolate"
  ),
  ?assertEqualStr(
    "Yo dawg, I heard you like string interpolations, so I put a string in your string so you can interpolate while you interpolate",
    lf"Yo dawg, I heard you like string interpolations, ~bf"so I put a ~ld"string in your"~ string"~ so you can interpolate while you interpolate"
  ),
  ?assertEqual(
    <<"Yo dawg, I heard you like string interpolations, [[115,111,32,73,32,112,117,116,32,97,32],[60,60,[[49,49,53],44,[49,49,54],44,[49,49,52],44,[49,48,53],44,[49,49,48],44,[49,48,51],44,[51,50],44,[49,48,53],44,[49,49,48],44,[51,50],44,[49,50,49],44,[49,49,49],44,[49,49,55],44,[49,49,52]],62,62],[32,115,116,114,105,110,103]] so you can interpolate while you interpolate"/utf8>>,
    bd"Yo dawg, I heard you like string interpolations, ~ld"so I put a ~bf"string in your"~ string"~ so you can interpolate while you interpolate"
  ),
  ?assertEqualStr(
    "Yo dawg, I heard you like string interpolations, <<115,111,32,73,32,112,117,116,32,97,32,91,49,49,53,44,49,49,54,44,49,49,52,44,49,48,53,44,49,49,48,44,49,48,51,44,51,50,44,49,48,53,44,49,49,48,44,51,50,44,49,50,49,44,49,49,49,44,49,49,55,44,49,49,52,93,32,115,116,114,105,110,103>> so you can interpolate while you interpolate",
    ld"Yo dawg, I heard you like string interpolations, ~bd"so I put a ~lf"string in your"~ string"~ so you can interpolate while you interpolate"
  ).
