%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2017. All Rights Reserved.
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
%%%----------------------------------------------------------------
%%% Purpose: string test suite.
%%%-----------------------------------------------------------------
-module(string_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases must be exported.
-export([is_empty/1, length/1, to_graphemes/1,
         reverse/1, slice/1,
         equal/1,
         pad/1, trim/1, chomp/1, take/1,
         uppercase/1, lowercase/1, titlecase/1, casefold/1,
         to_integer/1,to_float/1,
         prefix/1, split/1, replace/1, find/1,
         lexemes/1, nth_lexeme/1, cd_gc/1, meas/1
        ]).

-export([len/1,old_equal/1,old_concat/1,chr_rchr/1,str_rstr/1]).
-export([span_cspan/1,substr/1,old_tokens/1,chars/1]).
-export([copies/1,words/1,strip/1,sub_word/1,left_right/1]).
-export([sub_string/1,centre/1, join/1]).
-export([old_to_integer/1,old_to_float/1]).
-export([to_upper_to_lower/1]).

%% Run tests when debugging them
-export([debug/0, time_func/4]).
-compile([nowarn_deprecated_function]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [{group, chardata}, {group, list_string}].

groups() ->
    [{chardata,
      [is_empty, length, to_graphemes,
       equal, reverse, slice,
       pad, trim, chomp, take,
       lexemes, nth_lexeme,
       to_integer, to_float,
       uppercase, lowercase, titlecase, casefold,
       prefix, find, split, replace, cd_gc,
       meas]},
     {list_string,
      [len, old_equal, old_concat, chr_rchr, str_rstr, span_cspan,
       substr, old_tokens, chars, copies, words, strip, sub_word,
       left_right, sub_string, centre, join, old_to_integer,
       old_to_float, to_upper_to_lower]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

debug() ->
    Config = [{data_dir, "./" ++ ?MODULE_STRING++"_data"}],
    [io:format("~p:~p~n",[Test,?MODULE:Test(Config)]) ||
        {_,Tests} <- groups(), Test <- Tests].

-define(TEST(B,C,D), test(?LINE,?FUNCTION_NAME,B,C,D, true)).

-define(TEST_NN(B,C,D),
        test(?LINE,?FUNCTION_NAME,B,C,D, false),
        test(?LINE,?FUNCTION_NAME,hd(C),[B|tl(C)],D, false)).


is_empty(_) ->
    ?TEST("", [], true),
    ?TEST([""|<<>>], [], true),
    ?TEST("a", [], false),
    ?TEST([""|<<$a>>], [], false),
    ?TEST(["",[<<>>]], [], true),
    ok.

length(_) ->
    %% invalid arg type
    {'EXIT',_} = (catch string:length({})),
    {'EXIT',_} = (catch string:length(foo)),
    %% Valid signs
    ?TEST("", [], 0),
    ?TEST([""|<<>>], [], 0),
    L = tuple_size(list_to_tuple(atom_to_list(?MODULE))),
    ?TEST(atom_to_list(?MODULE), [], L),
    ?TEST("Hello", [], 5),
    ?TEST("UC Ω ßð", [], 7),
    ?TEST(["abc"|<<"abc">>], [], 6),
    ?TEST(["abc",["def"]], [], 6),
    ?TEST([<<97/utf8, 778/utf8, 98/utf8>>, [776,111,776]], [], 3), %% åäö in nfd
    ok.

equal(_) ->
    %% invalid arg type
    {'EXIT',_} = (catch string:equal(1, 2)),
    {'EXIT',_} = (catch string:equal(1, 2, foo)),
    {'EXIT',_} = (catch string:equal(1, 2, true, foo)),

    ?TEST("", [<<"">>], true),
    ?TEST("Hello", ["Hello"], true),
    ?TEST("Hello", ["Hell"], false),
    ?TEST("Hello", ["Hello!"], false),
    ?TEST("Hello", [<<"Hello"/utf8>>], true),
    ?TEST("Hello", [<<"Mello"/utf8>>], false),
    ?TEST("Hello", [<<"Hello!"/utf8>>], false),
    ?TEST(["Hello",[" deep"]], ["Hello deep"], true),
    ?TEST(["Hello",[<<" deep"/utf8>>]], ["Hello deep"], true),
    ?TEST("Hello deep", [["Hello", [" deep"]]], true),
    ?TEST("Hello deep", [["Hello", [" d!eep"]]], false),
    ?TEST("Hello deep", [["Hello", [<<" deep"/utf8>>]]], true),
    false = string:equal("Åäö", [<<97/utf8, 778/utf8, 98/utf8>>, [776,111,776]]), %% nfc vs nfd

    %% case_insensitive_equal()
    ?TEST("", ["", true], true),
    ?TEST("a", ["b", true], false),
    ?TEST("", [<<>>, true], true),
    ?TEST("", [[<<>>,[]], true], true),
    ?TEST("", [[<<>>,[$a]], true], false),
    ?TEST("123", ["123", true], true),
    ?TEST("abc", ["abc", true], true),
    ?TEST([[],<<>>,"ABC"|<<>>], [["abc",[]], true], true),
    ?TEST("ABCa", ["abcå", true], false),
    ?TEST("åäö", [{norm,"åäö"}, true], true),
    ?TEST("ÅÄÖ", [{norm,"åäö"}, true], true),
    ?TEST("MICHAŁ", ["michał", true], true),
    ?TEST(["Mic",<<"HAŁ"/utf8>>], ["michał", true], true),
    ?TEST("ß SHARP S", ["ss sharp s", true], true),
    ?TEST("ẞ SHARP S", [[<<$ß/utf8, $\s>>,"SHARP S"], true], true),
    ?TEST("ẞ SHARP ß", ["ss sharp s", true], false),
    ?TEST(<<"İ I WITH DOT ABOVE"/utf8>>, ["i̇ i with dot above", true], true),
    %% These should be equivalent with the above
    true = string:equal(string:casefold(["Mic",<<"HAŁ"/utf8>>]), string:casefold("michał")),
    true = string:equal(string:casefold("ẞ SHARP S"), string:casefold([<<$ß/utf8, $\s>>,"SHARP S"])),
    false = string:equal(string:casefold("ẞ SHARP ß"), string:casefold("ss sharp s")),

    %% Normalization
    ?TEST_NN("", ["", true, none], true),
    ?TEST_NN("a", ["b", true, nfc], false),
    ?TEST_NN("a", ["b", true, nfd], false),
    ?TEST_NN("a", ["b", true, nfkc], false),
    ?TEST_NN("a", ["b", true, nfkd], false),

    ?TEST_NN("a", ["A", false, nfc], false),
    ?TEST_NN("a", ["A", false, nfd], false),
    ?TEST_NN([<<>>,"a"|<<>>], ["A", true, nfkc], true),
    ?TEST_NN(<<"a">>, ["A", true, nfkd], true),

    ?TEST_NN([$a, <<$b>>, [97,776]], ["abä", false, none], false),
    ?TEST_NN([$a, <<$b>>, [97,776]], ["abä", false, nfc], true),
    ?TEST_NN([$a, <<$b>>, [97,776]], ["abä", false, nfd], true),
    ?TEST_NN([$a, <<$b>>, [97,776]], ["abä", false, nfkc], true),
    ?TEST_NN([$a, <<$b>>, [97,776]], ["abä", false, nfkd], true),

    ?TEST_NN([$a, <<$b>>, [97,776]], ["abÄ", true, none], false),
    ?TEST_NN([$a, <<$b>>, [97,776]], ["abÄ", false, nfc], false),
    ?TEST_NN([$a, <<$b>>, [97,776]], ["abÄ", true, nfc], true),
    ?TEST_NN([$a, <<$b>>, [97,776]], ["abÄ", true, nfd], true),
    ?TEST_NN([$a, <<$b>>, [97,776]], ["abÄ", true, nfkc], true),
    ?TEST_NN([$a, <<$b>>, [97,776]], ["abÄ", true, nfkd], true),

    ?TEST_NN([$a, <<$b>>, "ホンダ"], ["abﾎﾝﾀﾞ", true, none], false),
    ?TEST_NN([$a, <<$b>>, "ホンダ"], ["abﾎﾝﾀﾞ", true, nfc], false),
    ?TEST_NN([$a, <<$b>>, "ホンダ"], ["abﾎﾝﾀﾞ", true, nfd], false),
    ?TEST_NN([$a, <<$b>>, "ホンダ"], ["abﾎﾝﾀﾞ", true, nfkc], true),
    ?TEST_NN([$a, <<$b>>, "ホンダ"], ["abﾎﾝﾀﾞ", true, nfkd], true),

    ?TEST_NN([$a, <<$b>>, "32"], ["ab３２", true, none], false),
    ?TEST_NN([$a, <<$b>>, "32"], ["ab３２", true, nfc], false),
    ?TEST_NN([$a, <<$b>>, "32"], ["ab３２", true, nfd], false),
    ?TEST_NN([$a, <<$b>>, "32"], ["ab３２", true, nfkc], true),
    ?TEST_NN([$a, <<$b>>, "32"], ["ab３２", true, nfkd], true),

    %% Coverage.
    ?TEST("", [<<"">>, false, nfc], true),
    ?TEST("", [<<"">>, true, nfc], true),

    ok.

to_graphemes(_) ->
    %% More tests are in unicode_util_SUITE.erl
    {'EXIT', _} = (catch unicode:characters_to_nfd_binary(["asdåäö", an_atom])),
    String = ["abc..åäö", $e, 788, <<"Ωµe`è"/utf8>>, "œŒþæÆħ§ß"],
    NFD = unicode:characters_to_nfd_list(String),
    [] = string:to_graphemes([]),
    [] = string:to_graphemes(<<>>),
    GCs = string:to_graphemes(String),
    true = erlang:length(GCs) =:= string:length(String),
    true = erlang:length(GCs) =:= erlang:length(string:to_graphemes(NFD)),
    true = erlang:length(GCs) =:=
        erlang:length(string:to_graphemes(unicode:characters_to_nfc_list(String))),
    ok.

reverse(_) ->
    {'EXIT',_} = (catch string:reverse(2)),
    Str1 = "Hello ",
    Str2 = "Ω ßð",
    Str3 = "åäö",
    ?TEST("", [], ""),
    ?TEST(Str1, [], lists:reverse(Str1)),
    ?TEST(Str2, [], lists:reverse(Str2)),
    ?TEST(Str3, [], lists:reverse(Str3)),
    true = string:reverse(Str3) =:= lists:reverse(string:to_graphemes(Str3)),
    ok.

slice(_) ->
    {'EXIT',_} = (catch string:slice(2, 2, 2)),
    {'EXIT',_} = (catch string:slice("asd", foo, 2)),
    {'EXIT',_} = (catch string:slice("asd", 2, -1)),
    ?TEST("", [3], ""),
    ?TEST("aåä", [1, 0], ""),
    ?TEST("aåä", [3], ""),
    ?TEST("aåäöbcd", [3], "öbcd"),
    ?TEST([<<"aå"/utf8>>,"äöbcd"], [3], "öbcd"),
    ?TEST([<<"aåä"/utf8>>,"öbcd"], [3], "öbcd"),
    ?TEST([<<"aåä"/utf8>>,"öbcd"], [3, infinity], "öbcd"),

    ?TEST("", [3, 2], ""),
    ?TEST("aåä", [3, 2], ""),
    ?TEST("aåäöbcd", [3,2], "öb"),
    ?TEST([<<"aå"/utf8>>,"äöbcd"], [3,3], "öbc"),
    ?TEST([<<"aåä"/utf8>>,"öbcd"], [3,10], "öbcd"),

    ok.

pad(_) ->
    Str = "Hallå",
    ?TEST(Str, [7], "Hallå  "),
    ?TEST(Str, [7, leading], "  Hallå"),
    ?TEST(Str, [4, both, $.], "Hallå"),
    ?TEST(Str, [10, both, $.], "..Hallå..."),
    ?TEST(Str, [10, leading, $.], ".....Hallå"),
    ?TEST(Str, [10, trailing, $.], "Hallå....."),
    ?TEST(Str++["f"], [10, trailing, $.], "Hallåf...."),
    ?TEST(Str++[" flåwer"], [10, trailing, $.], "Hallå flåwer"),
    ok.

trim(_) ->
    Str = "\t\s..Ha\s.llå..\t\n\r",
    ?TEST("", [], ""),
    ?TEST(Str, [both, "x"], Str),
    ?TEST(Str, [leading], "..Ha\s.llå..\t\n\r"),
    ?TEST(Str, [trailing], "\t\s..Ha\s.llå.."),
    ?TEST(Str, [], "..Ha .llå.."),
    ?TEST(".. ", [both, ""], ".. "),
    ?TEST([<<".. ">>], [both, ". "], ""),
    ?TEST(".. h.ej ..", [leading, ". "], "h.ej .."),
    ?TEST(".. h.ej ..", [trailing, ". "], ".. h.ej"),
    ?TEST(".. h.ej ..", [both, ". "], "h.ej"),
    ?TEST(["..", <<"h.ej">>, ".."], [both, ". "], "h.ej"),
    ?TEST([[], "..", " h.ej ", <<"..">>], [both, ". "], "h.ej"),
    ?TEST([<<>>,<<"..">>, " h.ej", <<" ..">>], [both, ". "], "h.ej"),
    ?TEST([<<>>,<<"..">>, " h.ej", <<" ..">>], [trailing, ". "], ".. h.ej"),
    ?TEST([<<"..  h.ej .">>, <<"..">>], [both, ". "], "h.ej"),
    ?TEST(["..h", ".e", <<"j..">>], [both, ". "], "h.ej"),
    ?TEST(["..h", <<".ejsa"/utf8>>, "n.."], [both, ". "], "h.ejsan"),
    %% Test that it behaves with graphemes (i.e. nfd tests are the hard part)
    ?TEST([1013,101,778,101,101], [trailing, [101]], [1013,101,778]),
    ?TEST("aaåaa", [both, "a"], "å"),
    ?TEST(["aaa",778,"äöoo"], [both, "ao"], "åäö"),
    ?TEST([<<"aaa">>,778,"äöoo"], [both, "ao"], "åäö"),
    ?TEST([<<"e">>,778,"åäöe", <<778/utf8>>], [both, [[$e,778]]], "åäö"),
    ?TEST([[<<"!v">>|<<204,128,$v,204,129>>]],[trailing, [[$v,769]]], [$!,$v,768]),
    ?TEST([[[<<"v">>|<<204,129,118,204,128,118>>],769,118,769]], [trailing, [[118,769]]], [$v,769,$v,768]),
    ?TEST([<<"vv">>|<<204,128,118,204,128>>], [trailing, [[118,768]]], "v"),
    ok.

chomp(_) ->
    Str = "åäö\na\r\nsd\n",
    Res = "åäö\na\r\nsd",
    ?TEST("", [], ""),
    ?TEST("\n", [], ""),
    ?TEST("str \t", [], "str \t"),
    ?TEST("str \t\n\r", [], "str \t\n\r"),
    ?TEST(Str, [], Res),
    ?TEST([Str,$\n], [], Res),
    ?TEST([Str|"\n"], [], Res),
    ?TEST([Str|<<"\n">>], [], Res),
    ?TEST([Str,$\r|<<"\n">>], [], Res),
    ?TEST([Str, <<$\r>>|"\n"], [], Res),
    ?TEST([<<$a,$\r>>,"\na\n"], [], "a\r\na"),
    ok.

take(_) ->
    Str = "\t\s..Ha\s.llå..\t\n\r",
    WS = "\t\s\n\r",
    Chars = lists:seq($a,$z)++lists:seq($A,$Z),
    %% complement=false, dir=leading
    ?TEST("", ["abc"], {"",""}),
    ?TEST(Str, ["x"], {[], Str}),
    ?TEST(Str, [WS], {"\t\s","..Ha\s.llå..\t\n\r"}),
    ?TEST(".. ", ["", false], {"", ".. "}),
    ?TEST([<<".. ">>], [". ", false, leading], {".. ", ""}),
    ?TEST(".. h.ej ..", [". ", false, leading], {".. ", "h.ej .."}),
    ?TEST(["..", <<"h.ej">>, ".."], [". ", false, leading], {"..", "h.ej.."}),
    ?TEST([[], "..", " h.ej ", <<"..">>], [". ", false, leading], {".. ","h.ej .."}),
    ?TEST([<<>>,<<"..">>, " h.ej", <<" ..">>], [". ", false, leading], {".. ", "h.ej .."}),
    ?TEST(["..h", <<".ejsa"/utf8>>, "n.."], [". ", false, leading], {"..", "h.ejsan.."}),
    ?TEST([[<<101,204,138,33>>]], [[[$e,778]]], {[$e,778], "!"}),
    %% Test that it behaves with graphemes (i.e. nfd tests are the hard part)
    ?TEST("aaåaa", ["a", false, leading], {"aa", "åaa"}),
    ?TEST(["aaa",778,"äöoo"], ["ao", false, leading], {"aa", "åäöoo"}),
    ?TEST([<<"aaa">>,778,"äöoo"], ["ao",false,leading], {"aa", "åäöoo"}),
    ?TEST([<<"e">>,778,"åäöe", <<778/utf8>>], [[[$e,778]], false, leading], {[$e,778],"åäöe"++[778]}),

    %% complement=true, dir=leading
    ?TEST("", ["abc", true], {"",""}),
    ?TEST(Str, ["x", true], {Str, []}),
    ?TEST(Str, [Chars, true], {"\t\s..","Ha\s.llå..\t\n\r"}),
    ?TEST(".. ", ["",true], {".. ", ""}),
    ?TEST([<<".. ">>], [Chars, true, leading], {".. ", ""}),
    ?TEST(".. h.ej ..", [Chars, true, leading], {".. ", "h.ej .."}),
    ?TEST(["..", <<"h.ej">>, ".."], [Chars, true, leading], {"..", "h.ej.."}),
    ?TEST([[], "..", " h.ej ", <<"..">>], [Chars, true, leading], {".. ","h.ej .."}),
    ?TEST([<<>>,<<"..">>, " h.ej", <<" ..">>], [Chars, true, leading], {".. ", "h.ej .."}),
    ?TEST(["..h", <<".ejsa"/utf8>>, "n.."], [Chars, true, leading], {"..", "h.ejsan.."}),
    %% Test that it behaves with graphemes (i.e. nfd tests are the hard part)
    ?TEST([101,778], [[[101, 779]], true], {[101,778], []}),
    ?TEST(["aaee",778,"äöoo"], [[[$e,778]], true, leading], {"aae", [$e,778|"äöoo"]}),
    ?TEST([<<"aae">>,778,"äöoo"], [[[$e,778]],true,leading], {"aa", [$e,778|"äöoo"]}),
    ?TEST([<<"e">>,778,"åäöe", <<778/utf8>>], [[[$e,778]], true, leading], {[], [$e,778]++"åäöe"++[778]}),

    %% complement=false, dir=trailing
    ?TEST(Str, ["", false, trailing], {Str, []}),
    ?TEST(Str, ["x", false, trailing], {Str, []}),
    ?TEST(Str, [WS, false,trailing], {"\t\s..Ha\s.llå..", "\t\n\r"}),
    ?TEST(".. h.ej ..", [". ", false, trailing], {".. h.ej", " .."}),
    ?TEST(["..", <<"h.ej">>, ".."], [". ", false, trailing], {"..h.ej", ".."}),
    ?TEST([[], "..", " h.ej ", <<"..">>], [". ", false, trailing], {".. h.ej", " .."}),
    ?TEST([<<>>,<<"..">>, " h.ej", <<" ..">>], [". ", false, trailing], {".. h.ej", " .."}),
    ?TEST(["..h", <<".ejsa"/utf8>>, "n.."], [". ", false, trailing], {"..h.ejsan", ".."}),
    ?TEST("aaåaa", ["a", false, trailing], {"aaå", "aa"}),
    ?TEST([<<"KMÐ¨"/utf8>>], [[1064], false, trailing], {"KMÐ¨",[]}),
    ?TEST([[<<"!\"">>|<<"\"">>]], ["\"", false, trailing], {"!", "\"\""}),
    ?TEST([<<$v>>, 769], [[[$v,769]], false, trailing], {"", [$v,769]}),
    ?TEST(["aaa",778,"äöoo"], ["ao", false, trailing], {"aaåäö", "oo"}),
    ?TEST([<<"aaa">>,778,"äöoo"], ["ao", false, trailing], {"aaåäö", "oo"}),
    ?TEST([<<"e">>,778,"åäöee", <<778/utf8>>], [[[$e,778]], false, trailing], {[$e,778|"åäöe"], [$e,778]}),

    %% complement=true, dir=trailing
    ?TEST("", ["abc", true, trailing], {"",""}),
    ?TEST(Str, ["x", true, trailing], {[], Str}),
    %?TEST(Str, [{norm,Chars}, true, trailing], {"\t\s..Ha\s.ll","å..\t\n\r"}),
    ?TEST(".. ", ["", true, trailing], {"", ".. "}),
    ?TEST([<<".. ">>], [Chars, true, trailing], {"", ".. "}),
    ?TEST(".. h.ej ..", [Chars, true, trailing], {".. h.ej", " .."}),
    ?TEST(["..", <<"h.ej">>, ".."], [Chars, true, trailing], {"..h.ej", ".."}),
    ?TEST([[], "..", " h.ej ", <<"..">>], [Chars, true, trailing], {".. h.ej"," .."}),
    ?TEST([<<>>,<<"..">>, " h.ej", <<" ..">>], [Chars, true, trailing], {".. h.ej"," .."}),
    ?TEST(["..h", <<".ejsa"/utf8>>, "n.."], [Chars, true, trailing], {"..h.ejsan", ".."}),
    ?TEST([[<<101,204,138,33>>]], [[[$e,778]], true, trailing], {[$e,778], "!"}),
    ?TEST([<<"Fa">>], [[$F], true, trailing], {"F", "a"}),
    ?TEST([[<<101,101,204,138>>,1045,778]], ["e", true, trailing], {"e", [101,778,1045,778]}),
    ?TEST([[<<101,101,204,138>>,<<1045/utf8,778/utf8>>]], ["e", true, trailing], {"e", [101,778,1045,778]}),
    ?TEST([[[118,769,118],<<204,129,118,204,129,120,204,128,118>>,768,120,768]],
          [[[118,769]], true, trailing], {[118,769,118,769,118,769],[120,768,118,768,120,768]}),
    ?TEST([[<<118,204,128,118>>|<<204,128,118,204,128,118,204,128,206,132,204,129,206,132,204,129>>]],
          [[[118,768]], true, trailing], {[118,768,118,768,118,768,118,768], [900,769,900,769]}),
    %% Test that it behaves with graphemes (i.e. nfd tests are the hard part)
    ?TEST(["aaee",778,"äöoo"], [[[$e,778]], true, trailing], {"aae"++[$e,778], "äöoo"}),
    ?TEST([<<"aae">>,778,"äöoo"], [[[$e,778]],true,trailing], {"aa"++[$e,778], "äöoo"}),
    ?TEST([<<"e">>,778,"åäöe", <<778/utf8>>], [[[$e,778]], true, trailing], {[$e,778]++"åäöe"++[778], []}),
    ?TEST([<<"e">>,778,"åäöe", <<778/utf8>>, $e, 779], [[[$e,778]], true, trailing],
          {[$e,778]++"åäöe"++[778], [$e,779]}),

    ok.


uppercase(_) ->
    ?TEST("", [], ""),
    ?TEST("123", [], "123"),
    ?TEST("abc", [], "ABC"),
    ?TEST("ABC", [], "ABC"),
    ?TEST("abcdefghiljklmnopqrstvxyzåäö",[], "ABCDEFGHILJKLMNOPQRSTVXYZÅÄÖ"),
    ?TEST("åäö", [], "ÅÄÖ"),
    ?TEST("ÅÄÖ", [], "ÅÄÖ"),
    ?TEST("Michał", [], "MICHAŁ"),
    ?TEST(["Mic",<<"hał"/utf8>>], [], "MICHAŁ"),
    ?TEST("ǉǇ", [], "ǇǇ"),
    ?TEST("Ǉǉ", [], "ǇǇ"),
    ?TEST("ß sharp s", [], "SS SHARP S"),
    ok.

lowercase(_) ->
    ?TEST("", [], ""),
    ?TEST("123", [], "123"),
    ?TEST("abc", [], "abc"),
    ?TEST("ABC", [], "abc"),
    ?TEST("åäö", [], "åäö"),
    ?TEST("ÅÄÖ", [], "åäö"),
    ?TEST("MICHAŁ", [], "michał"),
    ?TEST(["Mic",<<"HAŁ"/utf8>>], [], "michał"),
    ?TEST("ß SHARP S", [], "ß sharp s"),
    ?TEST("İ I WITH DOT ABOVE", [], "i̇ i with dot above"),
    ok.

titlecase(_) ->
    ?TEST("", [], ""),
    ?TEST("123", [], "123"),
    %% Titlecase is the same as uppercase for most chars
    [?TEST([C,$x], [], string:uppercase([C])++[$x]) ||
        C <-"abcdefghiljklmnopqrstvxyzåäö"],
    %% Example of a different mapping
    ?TEST("ǉusad", [],"ǈusad"),
    ?TEST("ǉǇ", [], "ǈǇ"),
    ?TEST("Ǉǉ", [], "ǈǉ"),
    ?TEST("ß sharp s", [], "Ss sharp s"),
    ok.

casefold(_) ->
    ?TEST("", [], ""),
    ?TEST("123", [], "123"),
    ?TEST("abc", [], "abc"),
    ?TEST("ABC", [], "abc"),
    ?TEST("åäö", [], "åäö"),
    ?TEST("ÅÄÖ", [], "åäö"),
    ?TEST("MICHAŁ", [], "michał"),
    ?TEST(["Mic",<<"HAŁ"/utf8>>], [], "michał"),
    ?TEST("ß SHARP S", [], "ss sharp s"),
    ?TEST("ẞ SHARP S", [], "ss sharp s"),
    ?TEST("İ I WITH DOT ABOVE", [], "i̇ i with dot above"),
    ok.


to_integer(_) ->
    ?TEST("", [], {error, no_integer}),
    ?TEST("-", [], {error, no_integer}),
    ?TEST("01", [], {1, ""}),
    ?TEST("1.53", [], {1, ".53"}),
    ?TEST("+01.53", [], {1, ".53"}),
    ?TEST("-1.53", [], {-1, ".53"}),
    ?TEST("-13#16FF", [], {-13, "#16FF"}),
    ?TEST("13xFF", [], {13, "xFF"}),
    ?TEST(["234", <<"3+4-234">>], [], {2343, "+4-234"}),
    ok.

to_float(_) ->
    ?TEST("", [], {error, no_float}),
    ?TEST("1.53", [], {1.53, ""}),
    ?TEST("+01.53foo", [], {1.53, "foo"}),
    ?TEST("-1.53foo", [], {-1.53, "foo"}),
    ?TEST("-1,53foo", [], {-1.53, "foo"}),
    ?TEST("-1,53e1foo", [], {-15.3, "foo"}),
    ?TEST("-1,53e-1", [], {-0.153, ""}),
    ?TEST("-1,53E-1+2", [], {-0.153, "+2"}),
    ?TEST(["-1,53", <<"E-1+2">>], [], {-0.153, "+2"}),
    ok.

prefix(_) ->
    ?TEST("", ["a"], nomatch),
    ?TEST("a", [""], "a"),
    ?TEST("a", [[[]]], "a"),
    ?TEST("a", [<<>>], "a"),
    ?TEST("a", [[<<>>]], "a"),
    ?TEST("a", [[[<<>>]]], "a"),
    ?TEST("b", ["a"], nomatch),
    ?TEST("a", ["a"], ""),
    ?TEST("å", ["a"], nomatch),
    ?TEST(["a",<<778/utf8>>], ["a"], nomatch),
    ?TEST([<<"a"/utf8>>,778], ["a"], nomatch),
    ?TEST("hejsan", [""], "hejsan"),
    ?TEST("hejsan", ["hej"], "san"),
    ?TEST("hejsan", ["hes"], nomatch),
    ?TEST(["h", "ejsan"], ["hej"], "san"),
    ?TEST(["h", "e", "jsan"], ["hej"], "san"),
    ?TEST(["h", "e", "san"], ["hej"], nomatch),
    ?TEST(["h", <<"ejsan">>], ["hej"], "san"),
    ?TEST(["h", <<"e">>, "jsan"], ["hej"], "san"),
    ?TEST(["h", "e", <<"jsan">>], ["hej"], "san"),
    ok.

split(_) ->
    Mod = fun(Res) ->
                  [lists:flatten(unicode:characters_to_nfc_list(io_lib:format("~ts", [Str])))
                   || Str <- Res] end,
    ?TEST("..", ["", leading], {Mod, [".."]}),
    ?TEST("..", ["..", leading], {Mod, [[],[]]}),
    ?TEST("abcd", ["..", leading], {Mod, ["abcd"]}),
    ?TEST("ab..bc", ["..", leading], {Mod, ["ab","bc"]}),
    ?TEST("ab..bc..cd", ["..", leading], {Mod, ["ab","bc..cd"]}),
    ?TEST("..ab", [".."], {Mod, [[],"ab"]}),
    ?TEST("ab..", ["..", leading], {Mod, ["ab",[]]}),
    ?TEST(["ab..bc..cd"], ["..", leading], {Mod, ["ab","bc..cd"]}),
    ?TEST(["ab","..bc..cd"], ["..", leading], {Mod, ["ab","bc..cd"]}),
    ?TEST(["ab",<<"..bc..cd">>], ["..", leading], {Mod, ["ab","bc..cd"]}),
    ?TEST(["ab.",".bc..cd"], ["..", leading], {Mod, ["ab","bc..cd"]}),
    ?TEST(["ab.",<<".bc..cd">>], ["..", leading], {Mod, ["ab","bc..cd"]}),
    ?TEST(["ab..","bc..cd"], ["..", leading], {Mod, ["ab","bc..cd"]}),
    ?TEST(["ab..",<<"bc..cd">>], ["..", leading], {Mod, ["ab","bc..cd"]}),
    ?TEST(["ab.","bc..cd"], ["..", leading], {Mod, ["ab.bc","cd"]}),
    ?TEST("ab...bc", ["..", leading], {Mod, ["ab",".bc"]}),

    ?TEST("..", ["", trailing], {Mod, [".."]}),
    ?TEST("..", ["..", trailing], {Mod, [[],[]]}),
    ?TEST("abcd", ["..", trailing], {Mod, ["abcd"]}),
    ?TEST("ab..bc", ["..", trailing], {Mod, ["ab","bc"]}),
    ?TEST("ab..bc..cd", ["..", trailing], {Mod, ["ab..bc","cd"]}),
    ?TEST("..ab", ["..", trailing], {Mod, [[],"ab"]}),
    ?TEST("ab..", ["..", trailing], {Mod, ["ab",[]]}),
    ?TEST(["ab..bc..cd"], ["..", trailing], {Mod, ["ab..bc","cd"]}),
    ?TEST(["ab","..bc..cd"], ["..", trailing], {Mod, ["ab..bc","cd"]}),
    ?TEST(["ab"|<<"a">>], ["a", trailing], {Mod, ["ab",[]]}),
    ?TEST(["ab",<<"..bc..cd">>], ["..", trailing], {Mod, ["ab..bc","cd"]}),
    ?TEST([<<"ab.">>,".bc..cd"], ["..", trailing], {Mod, ["ab..bc","cd"]}),
    ?TEST(["ab.",<<".bc..cd">>], ["..", trailing], {Mod, ["ab..bc","cd"]}),
    ?TEST(["ab..","bc..cd"], ["..", trailing], {Mod, ["ab..bc","cd"]}),
    ?TEST(["ab..",<<"bc..cd">>], ["..", trailing], {Mod, ["ab..bc","cd"]}),
    ?TEST(["ab.","bc..cd"], ["..", trailing], {Mod, ["ab.bc","cd"]}),
    ?TEST("ab...bc", ["..", trailing], {Mod, ["ab.","bc"]}),

    ?TEST("..", ["..", all], {Mod, [[],[]]}),
    ?TEST("abcd", ["..", all], {Mod, ["abcd"]}),
    ?TEST("a..b", ["..", all], {Mod, ["a","b"]}),
    ?TEST("a..b..c", ["..", all], {Mod, ["a","b","c"]}),
    ?TEST("a..", ["..", all], {Mod, ["a",[]]}),
    ?TEST(["a..b..c"], ["..", all], {Mod, ["a","b","c"]}),
    ?TEST(["a","..b..c"], ["..", all], {Mod, ["a","b","c"]}),
    ?TEST(["a",<<"..b..c">>], ["..", all], {Mod, ["a","b","c"]}),
    ?TEST(["a.",".b..c"], ["..", all], {Mod, ["a","b","c"]}),
    ?TEST(["a.",<<".b..c">>], ["..", all], {Mod, ["a","b","c"]}),
    ?TEST(["a..","b..c"], ["..", all], {Mod, ["a","b","c"]}),
    ?TEST(["a..",<<"b..c">>], ["..", all], {Mod, ["a","b","c"]}),
    ?TEST(["a.","b..c"], ["..", all], {Mod, ["a.b","c"]}),
    ?TEST("a...b", ["..", all], {Mod, ["a",".b"]}),

    %% Grapheme (split) tests
    ?TEST("aΩΩb", ["Ω", all], {Mod, ["a","","b"]}),
    ?TEST([<<"aae">>,778,"äöoo"], [[$e,778], leading], {Mod, ["aa","äöoo"]}),
    ?TEST([<<"aae">>,778,"äöoo"], [[$e,778], trailing], {Mod, ["aa","äöoo"]}),
    ?TEST([<<"aae">>,778,"äöoo"], [[$e,778], all], {Mod, ["aa","äöoo"]}),
    ?TEST([<<"aae">>,778,"öeeåäö"], ["e", leading], {Mod, [[$a, $a, $e,778,$ö],"eåäö"]}),
    ?TEST([<<"aae">>,778,"öeeåäö"], ["e", trailing], {Mod, [[$a, $a, $e,778,$ö, $e],"åäö"]}),
    ?TEST([<<"aae">>,778,"öeeåäö"], ["e", all], {Mod, [[$a, $a, $e,778,$ö],"", "åäö"]}),

    ok.

replace(_) ->
    ?TEST(["a..b.", [".c"]], ["xxx", "::"], "a..b..c"),
    ?TEST(["a..b.", [".c"]], ["..", "::"], "a::b..c"),
    ?TEST([<<"a..b.">>, [".c"]], ["..", "::", trailing], "a..b::c"),
    ?TEST(["a..b.", [".c"]], ["..", "::", all], "a::b::c"),
    ok.

cd_gc(_) ->
    [] = string:next_codepoint(""),
    [] = string:next_codepoint(<<>>),
    [] = string:next_codepoint([<<>>]),
    "abcd" = string:next_codepoint("abcd"),
    [$e,778] = string:next_codepoint([$e,778]),
    [$e|<<204,138>>] = string:next_codepoint(<<$e,778/utf8>>),
    [778|_] = string:next_codepoint(tl(string:next_codepoint(<<$e,778/utf8>>))),
    [0|<<128,1>>] = string:next_codepoint(<<0,128,1>>),
    {error,<<128,1>>} = string:next_codepoint(<<128,1>>),

    [] = string:next_grapheme(""),
    [] = string:next_grapheme(<<>>),
    [] = string:next_grapheme([<<>>]),
    "abcd" = string:next_grapheme("abcd"),
    [[$e,778]] = string:next_grapheme([$e,778]),
    [[$e,778]] = string:next_grapheme(<<$e,778/utf8>>),
    [0|<<128,1>>] = string:next_grapheme(<<0,128,1>>),
    {error,<<128,1>>} = string:next_grapheme(<<128,1>>),

    ok.


find(_) ->
    ?TEST(["h", "ejsan"], [""], "hejsan"),
    ?TEST(["h", "ejsan"], [<<>>], "hejsan"),
    ?TEST([], [""], ""),
    ?TEST([], ["hej"], nomatch),
    ?TEST(["h", "ejsan"], ["hej"], "hejsan"),
    ?TEST(["h", "e", "jsan"], ["hej"], "hejsan"),
    ?TEST(["xh", "e", "san"], ["hej"], nomatch),
    ?TEST([<<"xh">>, <<"ejsan">>], ["hej"], "hejsan"),
    ?TEST(["xh", <<"ejsan">>], ["hej"], "hejsan"),
    ?TEST(["xh", <<"e">>, "jsan"], ["hej"], "hejsan"),
    ?TEST(["xh", "e", <<"jsan">>], ["hej"], "hejsan"),
    ?TEST(["xh", "er", <<"ljsane">>, "rlang"], ["erl", leading], "erljsanerlang"),
    ?TEST("aΩΩb", ["Ω", leading], "ΩΩb"),
    ?TEST([<<"aae">>,778,"äöoo"], [[$e,778], leading], [$e,778]++"äöoo"),
    ?TEST([<<"aae">>,778,"öeeåäö"], ["e", leading], "eeåäö"),

    ?TEST(["h", "ejsan"], ["", trailing], "hejsan"),
    ?TEST([], ["", trailing], ""),
    ?TEST([], ["hej", trailing], nomatch),
    ?TEST(["h", "ejsan"], ["hej", trailing], "hejsan"),
    ?TEST(["h", "e", "jsan"], ["hej", trailing], "hejsan"),
    ?TEST(["xh", "e", "san"], ["hej", trailing], nomatch),
    ?TEST([<<"xh">>, <<"ejsan">>], ["hej", trailing], "hejsan"),
    ?TEST(["xh", <<"ejsan">>], ["hej", trailing], "hejsan"),
    ?TEST(["xh", <<"e">>, "jsan"], ["hej", trailing], "hejsan"),
    ?TEST(["xh", "e", <<"jsan">>], ["hej", trailing], "hejsan"),
    ?TEST(["xh", "er", <<"ljsane">>, "rlang"], ["erl", trailing], "erlang"),
    ?TEST("aΩΩb", ["Ω", trailing], "Ωb"),
    ?TEST([<<"aae">>,778,"äöoo"], [[$e,778], trailing], [$e,778]++"äöoo"),
    ?TEST([<<"aeae">>,778,"äö"], ["e", trailing], "eae"++[778,$ä,$ö]),

    ok.

lexemes(_) ->
    Mod = fun(Res) ->
                  [unicode:characters_to_nfc_list(io_lib:format("~ts", [Str]))|| Str <- Res]
          end,
    Res = ["Hej", "san", "Hopp", "san"],
    ?TEST("", [" ,."],  {Mod, []}),
    ?TEST("Hej san", [""],  {Mod, ["Hej san"]}),
    ?TEST("  ,., ", [" ,."],  {Mod, []}),
    ?TEST( "Hej san Hopp san", [" ,."], {Mod, Res}),
    ?TEST(" Hej san Hopp san ", [" ,."], {Mod, Res}),
    ?TEST(" Hej san, .Hopp san ", [" ,."], {Mod, Res}),

    ?TEST([" Hej san",", .Hopp san "], [" ,."], {Mod, Res}),
    ?TEST([" Hej sa","n, .Hopp san "], [" ,."], {Mod, Res}),
    ?TEST([" Hej san,"," .Hopp san "], [" ,."], {Mod, Res}),

    ?TEST([" Hej san",[", .Hopp san "]], [" ,."], {Mod, Res}),
    ?TEST([" Hej sa",["n, .Hopp san "]], [" ,."], {Mod, Res}),
    ?TEST([" Hej san,",[" .Hopp san "]], [" ,."], {Mod, Res}),

    ?TEST([" H",<<"ej san, .Hopp "/utf8>>, "san"], [" ,."], {Mod, Res}),
    ?TEST([" Hej san",<<", .Hopp "/utf8>>, "san"], [" ,."], {Mod, Res}),
    ?TEST([" Hej sa",<<"n, .Hopp"/utf8>>, " san"], [" ,."], {Mod, Res}),
    ?TEST([" Hej san,",<<" .Hopp s"/utf8>>, "an"], [" ,."], {Mod, Res}),
    ?TEST([" Hej san",[<<", .Hopp san "/utf8>>]], [" ,."], {Mod, Res}),
    ?TEST([" Hej sa",[<<"n, .Hopp san "/utf8>>]], [" ,."], {Mod, Res}),
    ?TEST([" Hej san,",[<<" .Hopp san "/utf8>>], <<"  ">>], [" ,."], {Mod, Res}),

    ?TEST(" Hej\r\nsan\nnl", ["\r\n\s"], {Mod, ["Hej\r\nsan", "nl"]}),

    ?TEST(["b1ec1e",778,"äöo21"], ["eo"], {Mod, ["b1",[$c,$1,$e,778,$ä,$ö],"21"]}),
    ?TEST([<<"b1ec1e">>,778,"äöo21"], ["eo"], {Mod, ["b1",[$c,$1,$e,778,$ä,$ö],"21"]}),
    %% Grapheme (split) tests
    Str10 = [[[<<"Ã·"/utf8>>,1101],<<"Ã«"/utf8>>|<<"\"">>]],
    ?TEST(Str10, [[1076]], {Mod, [unicode:characters_to_nfc_list(Str10)]}),
    ?TEST("a1Ωb1Ωc1", ["Ω"], {Mod, ["a1","b1","c1"]}),
    ?TEST([<<"aae">>,778,"äöoo"], [[[$e,778]]], {Mod, ["aa","äöoo"]}),
    ?TEST([<<"aae">>,778,"äöo21"], [[[$e,778],$o]], {Mod, ["aa","äö","21"]}),
    ?TEST([<<"aae">>,778,"öeeåäö"], ["e"], {Mod, [[$a, $a, $e,778,$ö],"åäö"]}),
    ok.

nth_lexeme(_) ->
    {'EXIT', _} = (catch string:nth_lexeme("test test", 0, [])),
    {'EXIT', _} = (catch string:nth_lexeme(<<"test test">>, 0, [])),
    ?TEST( "", [1, " ,."],  []),
    ?TEST( "Hej san", [1, ""],  "Hej san"),
    ?TEST( "  ,., ", [1, " ,."],  []),
    ?TEST( "  ,., ", [3, " ,."],  []),
    ?TEST("Hej san Hopp san", [1, " ,."], "Hej"),
    ?TEST("...Hej san Hopp san", [1, " ,."], "Hej"),
    ?TEST("Hej san Hopp san", [3, " ,."], "Hopp"),
    ?TEST(" Hej san Hopp san ", [3, " ,."], "Hopp"),
    ?TEST(" Hej san, .Hopp san ", [3, " ,."], "Hopp"),
    ?TEST("ab cd", [3, " "], ""),

    ?TEST([" Hej san",", .Hopp san "], [3, " ,."], "Hopp"),
    ?TEST([" Hej sa","n, .Hopp san "], [3, " ,."], "Hopp"),
    ?TEST([" Hej san,"," .Hopp san "], [3, " ,."], "Hopp"),

    ?TEST([" Hej san",[", .Hopp san "]], [3," ,."], "Hopp"),
    ?TEST([" Hej sa",["n, .Hopp san "]], [3, " ,."], "Hopp"),
    ?TEST([" Hej san,",[" .Hopp san "]], [3, " ,."], "Hopp"),

    ?TEST([" Hej san",<<", .Hopp "/utf8>>, "san"], [3, " ,."], "Hopp"),
    ?TEST([" Hej sa",<<"n, .Hopp"/utf8>>, " san"], [3, " ,."], "Hopp"),
    ?TEST([" Hej san,",<<" .Hopp s"/utf8>>, "an"], [3, " ,."], "Hopp"),
    ?TEST([" Hej san,",<<" .Hopp s"/utf8>>, "an"], [4, " ,."], "san"),
    ?TEST([" Hej san",[<<", .Hopp san "/utf8>>]], [3, " ,."], "Hopp"),
    ?TEST([" Hej sa",[<<"n, .Hopp san "/utf8>>]], [3, " ,."], "Hopp"),
    ?TEST([" Hej san,",[<<" .Hopp san "/utf8>>], <<"  ">>], [3, " ,."], "Hopp"),

    ?TEST(["b1ec1e",778,"äöo21"], [3,"eo"], "21"),
    ?TEST([<<"b1ec1e">>,778,"äöo21"], [3, "eo"], "21"),
    %% Grapheme (split) tests
    ?TEST("a1Ωb1Ωc1", [1, "Ω"], "a1"),
    ?TEST([<<"aae">>,778,"äöoo"], [2,[[$e,778]]], "äöoo"),
    ?TEST([<<"aae">>,778,"äöo21"], [2,[[$e,778],$o]], "äö"),
    ?TEST([<<"aae">>,778,"öeeåäö"], [2,"e"], "åäö"),
    ok.


meas(Config) ->
    Parent = self(),
    Exec = fun() ->
                   DataDir0 = proplists:get_value(data_dir, Config),
                   DataDir = filename:join(lists:droplast(filename:split(DataDir0))),
                   case proplists:get_value(profile, Config, false) of
                       false ->
                           do_measure(DataDir);
                       eprof ->
                           eprof:profile(fun() -> do_measure(DataDir) end, [set_on_spawn]),
                           eprof:stop_profiling(),
                           eprof:analyze(),
                           eprof:stop()
                   end,
                   Parent ! {test_done, self()},
                   normal
           end,
    ct:timetrap({minutes,2}),
    case ct:get_timetrap_info() of
        {_,{_,Scale}} when Scale > 1 ->
            {skip,{will_not_run_in_debug,Scale}};
        _ -> % No scaling, run at most 1.5 min
            Tester = spawn(Exec),
            receive {test_done, Tester} -> ok
            after 90000 ->
                    io:format("Timelimit reached stopping~n",[]),
                    exit(Tester, die)
            end,
            ok
    end.

do_measure(DataDir) ->
    File =  filename:join([DataDir,"unicode_util_SUITE_data","NormalizationTest.txt"]),
    io:format("File ~s ",[File]),
    {ok, Bin} = file:read_file(File),
    io:format("~p~n",[byte_size(Bin)]),
    Do = fun(Name, Func, Mode) ->
                 {N, Mean, Stddev, _} = time_func(Func, Mode, Bin, 20),
                 io:format("~15w ~6w ~6.2fms ±~5.2fms #~.2w gc included~n",
                           [Name, Mode, Mean/1000, Stddev/1000, N])
         end,
    Do2 = fun(Name, Func, Mode) ->
                  {N, Mean, Stddev, _} = time_func(Func, binary, <<>>, 20),
                  io:format("~15w ~6w ~6.2fms ±~5.2fms #~.2w gc included~n",
                            [Name, Mode, Mean/1000, Stddev/1000, N])
          end,
    io:format("----------------------~n"),

    Do(old_tokens, fun(Str) -> string:tokens(Str, [$\n,$\r]) end, list),
    Tokens = {lexemes, fun(Str) -> string:lexemes(Str, [$\n,$\r]) end},
    [Do(Name,Fun,Mode) || {Name,Fun} <- [Tokens], Mode <- [list, binary]],

    S0 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxy.....",
    S0B = <<"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxy.....">>,
    Do2(old_strip_l, repeat(fun() -> string:strip(S0, left, $x) end), list),
    Do2(trim_l,  repeat(fun() -> string:trim(S0, leading, [$x]) end), list),
    Do2(trim_l,  repeat(fun() -> string:trim(S0B, leading, [$x]) end), binary),
    Do2(old_strip_r, repeat(fun() -> string:strip(S0, right, $.) end), list),
    Do2(trim_t,  repeat(fun() -> string:trim(S0, trailing, [$.]) end), list),
    Do2(trim_t,  repeat(fun() -> string:trim(S0B, trailing, [$.]) end), binary),

    Do2(old_chr_sub, repeat(fun() -> string:sub_string(S0, string:chr(S0, $.)) end), list),
    Do2(old_str_sub, repeat(fun() -> string:sub_string(S0, string:str(S0, [$.])) end), list),
    Do2(find, repeat(fun() -> string:find(S0, [$.]) end), list),
    Do2(find, repeat(fun() -> string:find(S0B, [$.]) end), binary),
    Do2(old_str_sub2, repeat(fun() -> N = string:str(S0, "xy.."),
                        {string:sub_string(S0,1,N), string:sub_string(S0,N+4)} end), list),
    Do2(split, repeat(fun() -> string:split(S0, "xy..") end), list),
    Do2(split, repeat(fun() -> string:split(S0B, "xy..") end), binary),

    Do2(old_rstr_sub, repeat(fun() -> string:sub_string(S0, string:rstr(S0, [$y])) end), list),
    Do2(find_t, repeat(fun() -> string:find(S0, [$y], trailing) end), list),
    Do2(find_t, repeat(fun() -> string:find(S0B, [$y], trailing) end), binary),
    Do2(old_rstr_sub2, repeat(fun() -> N = string:rstr(S0, "y.."),
                         {string:sub_string(S0,1,N), string:sub_string(S0,N+3)} end), list),
    Do2(split_t, repeat(fun() -> string:split(S0, "y..", trailing) end), list),
    Do2(split_t, repeat(fun() -> string:split(S0B, "y..", trailing) end), binary),

    Do2(old_span, repeat(fun() -> N=string:span(S0, [$x, $y]),
                                  {string:sub_string(S0,1,N),string:sub_string(S0,N+1)}
                         end), list),
    Do2(take, repeat(fun() -> string:take(S0, [$x, $y]) end), list),
    Do2(take, repeat(fun() -> string:take(S0B, [$x, $y]) end), binary),

    Do2(old_cspan, repeat(fun() -> N=string:cspan(S0, [$.,$y]),
                                   {string:sub_string(S0,1,N),string:sub_string(S0,N+1)}
                          end), list),
    Do2(take_c, repeat(fun() -> string:take(S0, [$.,$y], true) end), list),
    Do2(take_c, repeat(fun() -> string:take(S0B, [$.,$y], true) end), binary),

    Do2(old_substr, repeat(fun() -> string:substr(S0, 21, 15) end), list),
    Do2(slice, repeat(fun() -> string:slice(S0, 20, 15) end), list),
    Do2(slice, repeat(fun() -> string:slice(S0B, 20, 15) end), binary),

    io:format("--~n",[]),
    NthTokens = {nth_lexemes, fun(Str) -> string:nth_lexeme(Str, 18000, [$\n,$\r]) end},
    [Do(Name,Fun,Mode) || {Name,Fun} <- [NthTokens], Mode <- [list, binary]],
    Do2(take_t, repeat(fun() -> string:take(S0, [$.,$y], false, trailing) end), list),
    Do2(take_t, repeat(fun() -> string:take(S0B, [$.,$y], false, trailing) end), binary),
    Do2(take_tc, repeat(fun() -> string:take(S0, [$x], true, trailing) end), list),
    Do2(take_tc, repeat(fun() -> string:take(S0B, [$x], true, trailing) end), binary),

    Length = {length, fun(Str) -> string:length(Str) end},
    [Do(Name,Fun,Mode) || {Name,Fun} <- [Length], Mode <- [list, binary]],

    Reverse = {reverse, fun(Str) -> string:reverse(Str) end},
    [Do(Name,Fun,Mode) || {Name,Fun} <- [Reverse], Mode <- [list, binary]],

    ok.

repeat(F) ->
    fun(_) -> repeat_1(F,20000) end.

repeat_1(F, N) when N > 0 ->
    F(),
    repeat_1(F, N-1);
repeat_1(_, _) ->
    erlang:garbage_collect(),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions

test(Line, Func, Str, Args, Res, Norm) ->
    %%io:format("~p: ~p ~w ~w~n",[Line, Func, Str, Args]),
    test_1(Line, Func, Str, [Str|norm(none,Args)], Res),
    %%io:format("~p: ~p bin ",[Line, Func]),
    test_1({Line,list}, Func, Str,
           [unicode:characters_to_list(Str)|norm(none,Args)], Res),
    Norm andalso
        test_1({Line,clist}, Func, Str,
               [unicode:characters_to_nfc_list(Str)|norm(nfc,Args)], Res),
    Norm andalso
        test_1({Line,dlist}, Func, Str,
               [unicode:characters_to_nfd_list(Str)|norm(nfd,Args)], Res),
    test_1({Line,bin}, Func, Str,
           [unicode:characters_to_binary(Str)|norm(none, Args)], Res),
    Norm andalso
        test_1({Line,cbin}, Func, Str,
               [unicode:characters_to_nfc_binary(Str)|norm(nfc,Args)], Res),
    Norm andalso
        test_1({Line,dbin}, Func, Str,
               [unicode:characters_to_nfd_binary(Str)|norm(nfd,Args)], Res),
    %%io:format("~n",[]),
    ok.

test_1(Line, Func, Str, Args, Exp) ->
    try
        Res = apply(string, Func, Args),
        check_types(Line, Func, Args, Res),
        case res(Res, Exp) of
            true -> ok;
            {Res1,Exp1} when is_tuple(Exp1) ->
                io:format("~p~n",[Args]),
                io:format("~p:~p: ~ts~w =>~n  :~w:~w~n",
                          [Func,Line, Str,Str,Res1,Exp1]),
                exit({error, Func});
            {Res1,Exp1} ->
                io:format("~p:~p: ~ts~w =>~n  :~ts~w:~ts~w~n",
                          [Func,Line, Str,Str, Res1,Res1, Exp1,Exp1]),
                exit({error, Func})
        end
    catch
        error:Exp ->
            ok;
        error:Reason:Stacktrace ->
            io:format("~p:~p: Crash ~p ~p~n",
                      [?MODULE,Line, Reason, Stacktrace]),
            exit({error, Func})
    end.

norm(Type, Args) ->
    Norm = case Type of
               nfc -> fun unicode:characters_to_nfc_list/1;
               nfd -> fun unicode:characters_to_nfd_list/1;
               none -> fun(Str) -> Str end
           end,
    lists:map(fun({norm,Str}) -> Norm(Str);
                 (Other) -> Other
              end, Args).

res(Str, Str) -> true;
res(Str, Exp) when is_list(Str), is_list(Exp) ->
    A = unicode:characters_to_nfc_list(Str),
    A==Exp orelse {A,Exp};
res(Str, Exp) when is_binary(Str), is_list(Exp) ->
    A = unicode:characters_to_nfc_list(Str),
    A==Exp orelse {A,Exp};
res(What, {Fun, Exp}) when is_function(Fun) ->
    Fun(What) == Exp orelse {Fun(What), Exp};
res({S1,S2}=S, {Exp1,Exp2}=E) -> %% For take
    case {res(S1,Exp1), res(S2,Exp2)} of
        {true, true} -> true;
        _ -> {S, E}
    end;
res(Int, Exp) ->
    Int == Exp orelse {Int, Exp}.


check_types(_Line, _Func, _Str, Res)
  when is_integer(Res); is_boolean(Res); Res =:= nomatch ->
    %% length or equal
    ok;
check_types(Line, Func, [S1,S2], Res)
  when Func =:= concat ->
    case check_types_1(type(S1),type(S2)) of
        ok ->
            case check_types_1(type(S1),type(Res)) of
                ok -> ok;
                {T1,T2} ->
                    io:format("Failed: ~p ~p ~p ~p~n",[Line, Func, T1, T2]),
                    io:format("  ~p ~p  => ~p~n", [S1, S2, Res]),
                    error
            end;
        _ -> ok
    end;
check_types(Line, Func, [Str|_], Res)  ->
    AddList = fun(mixed) -> mixed;
                 ({list,{list,_}}) -> {list, deep};
                 (R) ->
                      case lists:member(Func, [lexemes, tokens, split]) of
                          true -> {list, R};
                          false -> R
                      end
              end,
    try needs_check(Func) andalso (ok = check_types_1(AddList(type(Str)), type(Res))) of
        ok -> ok;
        false -> ok
    catch _:{badmatch, {T1,T2}} ->
            io:format("Failed: ~p ~p: ~p ~p~n",[Line, Func, T1, T2]),
            io:format("  ~p  => ~p~n", [Str, Res]),
            error;
          _:Reason:Stacktrace ->
            io:format("Crash: ~p in~n ~p~n",[Reason, Stacktrace]),
            io:format("Failed: ~p ~p: ~p => ~p~n", [Line, Func, Str, Res]),
            exit({Reason, Stacktrace})
    end.

check_types_1(T, T) ->
    ok;
check_types_1(Str, Res)
  when is_binary(Str), is_binary(Res) ->
    ok;
check_types_1({list, _},{list, undefined}) ->
    ok;
check_types_1({list, _},{list, codepoints}) ->
    ok;
check_types_1({list, {list, _}},{list, {list, codepoints}}) ->
    ok;
check_types_1(mixed,_) ->
    ok;
check_types_1({list, binary}, binary) ->
    ok;
check_types_1({list, binary}, {other, _, _}) -> %% take
    ok;
check_types_1({list, deep}, _) ->
    ok;
check_types_1({list, {list, deep}}, _) ->
    ok;
check_types_1(_, {error,_}) ->
    ok;
check_types_1(T1,T2) ->
    {T1,T2}.

type(Bin) when is_binary(Bin) ->
    binary;
type([]) ->
    {list, undefined};
type(List) when is_list(List) ->
    Deep = fun(L) when is_list(L) ->
                   lists:any(fun(C) -> is_list(C) orelse is_binary(C) end, L);
              (_) -> false
           end,
    case all(fun(C) -> not is_binary(C) end, List) of
        true ->
            case all(fun(C) -> is_integer(C) end, List) of
                true -> {list, codepoints};
                false ->
                    case [deep || L <- List, Deep(L)] of
                        [] -> {list, {list, codepoints}};
                        _ -> {list, deep}
                    end
            end;
        false ->
            case all(fun(C) -> is_binary(C) end, List) of
                true -> {list, binary};
                false -> mixed
            end
    end;
type({Number, String}) when is_number(Number) ->
    %% to_integer or to_float
    type(String);
type({Atom, _}=What) when is_atom(Atom) ->
    What;
type({R1,R2}) ->
    case {type(R1),type(R2)} of
        {T,T} -> T;
        {{list,undefined}, {list,codepoints}} -> {list,codepoints};
        {{list,codepoints}, {list,undefined}} -> {list,codepoints};
        {T1,T2} -> {other, T1,T2}
    end;
type(Other) ->
    {other, Other}.

all(_Check, []) ->
    true;
all(Check, [H|T]) ->
    Check(H) andalso all(Check,T);
all(Check, Bin) when is_binary(Bin) ->
    Check(Bin).

needs_check(reverse) -> false;
needs_check(pad) -> false;
needs_check(replace) -> false;
needs_check(_) -> true.

%%%% Timer stuff

time_func(Fun, Mode, Bin, Repeat) ->
    timer:sleep(100), %% Let emulator catch up and clean things before test runs
    Self = self(),
    Pid = spawn_link(fun() ->
                             Str = mode(Mode, Bin),
                             Self ! {self(),time_func(0,0,0, Fun, Str, undefined, Repeat)}
                     end),
    receive {Pid,Msg} -> Msg end.

time_func(N,Sum,SumSq, Fun, Str, _, Repeat) when N < Repeat ->
    {Time, Res} = timer:tc(fun() -> Fun(Str) end),
    time_func(N+1,Sum+Time,SumSq+Time*Time, Fun, Str, Res, Repeat);
time_func(N,Sum,SumSq, _, _, Res, _) ->
    Mean = round(Sum / N),
    Stdev = round(math:sqrt((SumSq - (Sum*Sum/N))/(N - 1))),
    {N, Mean, Stdev, Res}.

mode(binary, Bin) -> Bin;
mode(list, Bin) -> unicode:characters_to_list(Bin).

%%
%% Old string lists Test cases starts here.
%%

len(Config) when is_list(Config) ->
    0 = string:len(""),
    L = tuple_size(list_to_tuple(atom_to_list(?MODULE))),
    L = string:len(atom_to_list(?MODULE)),
    %% invalid arg type
    {'EXIT',_} = (catch string:len({})),
    ok.

old_equal(Config) when is_list(Config) ->
    true = string:equal("", ""),
    false = string:equal("", " "),
    true = string:equal("laban", "laban"),
    false = string:equal("skvimp", "skvump"),
    ok.

old_concat(Config) when is_list(Config) ->
    "erlang rules" = string:concat("erlang ", "rules"),
    "" = string:concat("", ""),
    "x" = string:concat("x", ""),
    "y" = string:concat("", "y"),
    %% invalid arg type
    {'EXIT',_} = (catch string:concat(hello, please)),
    ok.

chr_rchr(Config) when is_list(Config) ->
    {_,_,X} = erlang:timestamp(),
    0 = string:chr("", (X rem (255-32)) + 32),
    0 = string:rchr("", (X rem (255-32)) + 32),
    1 = string:chr("x", $x),
    1 = string:rchr("x", $x),
    1 = string:chr("xx", $x),
    2 = string:rchr("xx", $x),
    3 = string:chr("xyzyx", $z),
    3 = string:rchr("xyzyx", $z),
    %% invalid arg type
    {'EXIT',_} = (catch string:chr(hello, $h)),
    %% invalid arg type
    {'EXIT',_} = (catch string:chr("hello", h)),
    %% invalid arg type
    {'EXIT',_} = (catch string:rchr(hello, $h)),
    %% invalid arg type
    {'EXIT',_} = (catch string:rchr("hello", h)),
    ok.

str_rstr(Config) when is_list(Config) ->
    {_,_,X} = erlang:timestamp(),
    0 = string:str("", [(X rem (255-32)) + 32]),
    0 = string:rstr("", [(X rem (255-32)) + 32]),
    1 = string:str("x", "x"),
    1 = string:rstr("x", "x"),
    0 = string:str("hello", ""),
    0 = string:rstr("hello", ""),
    1 = string:str("xxxx", "xx"),
    3 = string:rstr("xxxx", "xx"),
    3 = string:str("xy z yx", " z"),
    3 = string:rstr("xy z yx", " z"),
    3 = string:str("aaab", "ab"),
    %% invalid arg type
    {'EXIT',_} = (catch string:str(hello, "he")),
    %% invalid arg type
    {'EXIT',_} = (catch string:str("hello", he)),
    %% invalid arg type
    {'EXIT',_} = (catch string:rstr(hello, "he")),
    %% invalid arg type
    {'EXIT',_} = (catch string:rstr("hello", he)),
    ok.

span_cspan(Config) when is_list(Config) ->
    0 = string:span("", "1"),
    0 = string:span("1", ""),
    0 = string:cspan("", "1"),
    1 = string:cspan("1", ""),
    1 = string:span("1  ", "1"),
    5 = string:span("  1  ", "12 "),
    6 = string:span("1231234", "123"),
    0 = string:cspan("1  ", "1"),
    1 = string:cspan("3 ", "12 "),
    6 = string:cspan("1231234", "4"),
    %% invalid arg type
    {'EXIT',_} = (catch string:span(1234, "1")),
    %% invalid arg type
    {'EXIT',_} = (catch string:span(1234, "1")),
    %% invalid arg type
    {'EXIT',_} = (catch string:cspan("1234", 1)),
    %% invalid arg type
    {'EXIT',_} = (catch string:cspan("1234", 4)),
    ok.


substr(Config) when is_list(Config) ->
    {'EXIT',_} = (catch string:substr("", 0)),
    [] = string:substr("", 1),
    {'EXIT',_} = (catch string:substr("", 2)),
    [] = string:substr("1", 2),
    {'EXIT',_} = (catch  string:substr("", 0, 1)),
    [] = string:substr("", 1, 1),
    [] = string:substr("", 1, 2),
    {'EXIT',_} = (catch string:substr("", 2, 2)),
    "1234" = string:substr("1234", 1),
    "1234" = string:substr("1234", 1, 4),
    "1234" = string:substr("1234", 1, 5),
    "23" = string:substr("1234", 2, 2),
    "4" = string:substr("1234", 4),
    "" = string:substr("1234", 4, 0),
    "4" = string:substr("1234", 4, 1),
    %% invalid arg type
    {'EXIT',_} = (catch string:substr(1234, 1)),
    %% invalid arg type
    {'EXIT',_} = (catch string:substr("1234", "1")),
    ok.

old_tokens(Config) when is_list(Config) ->
    [] = string:tokens("",""),
    [] = string:tokens("abc","abc"),
    ["abc"] = string:tokens("abc", ""),
    ["1","2 34","45","5","6","7"] = do_tokens("1,2 34,45;5,;6;,7", ";,"),

    %% invalid arg type
    {'EXIT',_} = (catch string:tokens('x,y', ",")),
    {'EXIT',_} = (catch string:tokens("x,y", ',')),
    ok.

do_tokens(S0, Sep0) ->
    [H|T] = Sep0,
    S = [replace_sep(C, T, H) || C <- S0],
    Sep = [H],
    io:format("~p ~p\n", [S0,Sep0]),
    io:format("~p ~p\n", [S,Sep]),

    Res = string:tokens(S0, Sep0),
    Res = string:tokens(Sep0++S0, Sep0),
    Res = string:tokens(S0++Sep0, Sep0),

    Res = string:tokens(S, Sep),
    Res = string:tokens(Sep++S, Sep),
    Res = string:tokens(S++Sep, Sep),

    Res.

replace_sep(C, Seps, New) ->
    case lists:member(C, Seps) of
	true -> New;
	false -> C
    end.

chars(Config) when is_list(Config) ->
    [] = string:chars($., 0),
    [] = string:chars($., 0, []),
    10 = erlang:length(string:chars(32, 10, [])),
    "aaargh" = string:chars($a, 3, "rgh"),
    %% invalid arg type
    {'EXIT',_} = (catch string:chars($x, [])),
    ok.

copies(Config) when is_list(Config) ->
    "" = string:copies("", 10),
    "" = string:copies(".", 0),
    "." = string:copies(".", 1),
    30 = erlang:length(string:copies("123", 10)),
    %% invalid arg type
    {'EXIT',_} = (catch string:copies("hej", -1)),
    {'EXIT',_} = (catch string:copies("hej", 2.0)),
    ok.

words(Config) when is_list(Config) ->
    1 = string:words(""),
    1 = string:words("", $,),
    1 = string:words("hello"),
    1 = string:words("hello", $,),
    1 = string:words("...", $.),
    2 = string:words("2.35", $.),
    100 = string:words(string:copies(". ", 100)),
    %% invalid arg type
    {'EXIT',_} = (catch string:chars(hej, 1)),
    %% invalid arg type
    {'EXIT',_} = (catch string:chars("hej", 1, " ")),
    ok.


strip(Config) when is_list(Config) ->
    "" = string:strip(""),
    "" = string:strip("", both),
    "" = string:strip("", both, $.),
    "hej" = string:strip("  hej  "),
    "hej  " = string:strip("  hej  ", left),
    "  hej" = string:strip("  hej  ", right),
    "  hej  " = string:strip("  hej  ", right, $.),
    "hej  hopp" = string:strip("  hej  hopp  ", both),
    %% invalid arg type
    {'EXIT',_} = (catch string:strip(hej)),
    %% invalid arg type
    {'EXIT',_} = (catch string:strip(" hej", up)),
    %% invalid arg type
    {'EXIT',_} = (catch string:strip(" hej", left, " ")),	% not good
    ok.

sub_word(Config) when is_list(Config) ->
    "" = string:sub_word("", 1),
    "" = string:sub_word("", 1, $,),
    {'EXIT',_} = (catch string:sub_word("1 2 3", 0)),
    "" = string:sub_word("1 2 3", 4),
    "llo th" = string:sub_word("but hello there", 2, $e),
    %% invalid arg type
    {'EXIT',_} = (catch string:sub_word('hello there', 1)),
    %% invalid arg type
    {'EXIT',_} = (catch string:sub_word("hello there", 1, "e")),
    ok.

left_right(Config) when is_list(Config) ->
    "" = string:left("", 0),
    "" = string:left("hej", 0),
    "" = string:left("hej", 0, $.),
    "" = string:right("", 0),
    "" = string:right("hej", 0),
    "" = string:right("hej", 0, $.),
    "123  " = string:left("123 ", 5),
    "  123" = string:right(" 123", 5),
    "123!!" = string:left("123!", 5, $!),
    "==123" = string:right("=123", 5, $=),
    "1" = string:left("123", 1, $.),
    "3" = string:right("123", 1, $.),
    %% invalid arg type
    {'EXIT',_} = (catch string:left(hello, 5)),
    %% invalid arg type
    {'EXIT',_} = (catch string:right(hello, 5)),
    %% invalid arg type
    {'EXIT',_} = (catch string:left("hello", 5, ".")),
    %% invalid arg type
    {'EXIT',_} = (catch string:right("hello", 5, ".")),
    ok.

sub_string(Config) when is_list(Config) ->
    {'EXIT',_} = (catch string:sub_string("", 0)),
    [] = string:sub_string("", 1),
    {'EXIT',_} = (catch string:sub_string("", 2)),
    [] = string:sub_string("1", 2),
    {'EXIT',_} = (catch string:sub_string("", 0, 1)),
    [] = string:sub_string("", 1, 1),
    [] = string:sub_string("", 1, 2),
    {'EXIT',_} = (catch string:sub_string("", 2, 2)),
    "1234" = string:sub_string("1234", 1),
    "1234" = string:sub_string("1234", 1, 4),
    "1234" = string:sub_string("1234", 1, 5),
    "23" = string:sub_string("1234", 2, 3),
    "4" = string:sub_string("1234", 4),
    "4" = string:sub_string("1234", 4, 4),
    "4" = string:sub_string("1234", 4, 5),
    %% invalid arg type
    {'EXIT',_} = (catch string:sub_string(1234, 1)),
    %% invalid arg type
    {'EXIT',_} = (catch string:sub_string("1234", "1")),
    ok.

centre(Config) when is_list(Config) ->
    "" = string:centre("", 0),
    "" = string:centre("1", 0),
    "" = string:centre("", 0, $-),
    "" = string:centre("1", 0, $-),
    "gd" = string:centre("agda", 2),
    "agda " = string:centre("agda", 5),
    " agda " = string:centre("agda", 6),
    "agda." = string:centre("agda", 5, $.),
    "--agda--" = string:centre("agda", 8, $-),
    "agda" = string:centre("agda", 4),
    %% invalid arg type
    {'EXIT',_} = (catch string:centre(hello, 10)),
    ok.

old_to_integer(Config) when is_list(Config) ->
    {1,""} = test_to_integer("1"),
    {1,""} = test_to_integer("+1"),
    {-1,""} = test_to_integer("-1"),
    {1,"="} = test_to_integer("1="),
    {7,"F"} = test_to_integer("7F"),
    {709,""} = test_to_integer("709"),
    {709,"*2"} = test_to_integer("709*2"),
    {0,"xAB"} = test_to_integer("0xAB"),
    {16,"#FF"} = test_to_integer("16#FF"),
    {error,no_integer} = test_to_integer(""),
    {error,no_integer} = test_to_integer("!1"),
    {error,no_integer} = test_to_integer("F1"),
    {error,badarg} = test_to_integer('23'),
    %% {3,[[]]} = test_to_integer([$3,[]]),
    %% {3,[hello]} = test_to_integer([$3,hello]),
    {error,badarg} = test_to_integer([$3,hello]),
    ok.

test_to_integer(Str) ->
    %% io:format("Checking ~p~n", [Str]),
    case string:to_integer(Str) of
	{error,_Reason} = Bad ->
	    {'EXIT',_} = (catch list_to_integer(Str)),
	    Bad;
	{F,_Rest} = Res ->
	    _ = integer_to_list(F),
	    Res
    end.

old_to_float(Config) when is_list(Config) ->
    {1.2,""} = test_to_float("1.2"),
    {1.2,""} = test_to_float("1,2"),
    {120.0,""} = test_to_float("1.2e2"),
    {120.0,""} = test_to_float("+1,2e2"),
    {-120.0,""} = test_to_float("-1.2e2"),
    {-120.0,""} = test_to_float("-1,2e+2"),
    {-1.2e-2,""} = test_to_float("-1.2e-2"),
    {1.2,"="} = test_to_float("1.2="),
    {7.9,"e"} = test_to_float("7.9e"),
    {7.9,"ee"} = test_to_float("7.9ee"),
    {7.9,"e+"} = test_to_float("7.9e+"),
    {7.9,"e-"} = test_to_float("7.9e-"),
    {7.9,"e++"} = test_to_float("7.9e++"),
    {7.9,"e--"} = test_to_float("7.9e--"),
    {7.9,"e+e"} = test_to_float("7.9e+e"),
    {7.9,"e-e"} = test_to_float("7.9e-e"),
    {7.9,"e+."} = test_to_float("7.9e+."),
    {7.9,"e-."} = test_to_float("7.9e-."),
    {7.9,"e+,"} = test_to_float("7.9e+,"),
    {7.9,"e-,"} = test_to_float("7.9e-,"),
    {error,no_float} = test_to_float(""),
    {error,no_float} = test_to_float("e1,0"),
    {error,no_float} = test_to_float("1;0"),
    {error,no_float} = test_to_float("1"),
    {error,no_float} = test_to_float("1e"),
    {error,no_float} = test_to_float("2."),
    {error,badarg} = test_to_float('2.3'),
    %{2.3,[[]]} = test_to_float([$2,$.,$3,[]]),
    {2.3,[]} = test_to_float([$2,$.,$3,[]]),
    %%{2.3,[hello]} = test_to_float([$2,$.,$3,hello]),
    {error, badarg} = test_to_float([$2,$.,$3,hello]),
    ok.

test_to_float(Str) ->
    %% io:format("Checking ~p~n", [Str]),
    case string:to_float(Str) of
	{error,_Reason} = Bad ->
	    {'EXIT',_} = (catch list_to_float(Str)),
	    Bad;
	{F,_Rest} = Res ->
	    _ = float_to_list(F),
	    Res
    end.

to_upper_to_lower(Config) when is_list(Config) ->
    "1234ABCDEFÅÄÖ=" = string_to_upper("1234abcdefåäö="),
    "éèíúùòóåäöabc()" = string_to_lower("ÉÈÍÚÙÒÓÅÄÖabc()"),
    All = lists:seq(0, 255),

    UC = string_to_upper(All),
    256 = erlang:length(UC),
    all_upper_latin1(UC, 0),

    LC = string_to_lower(All),
    all_lower_latin1(LC, 0),

    LC = string_to_lower(string_to_upper(LC)),
    LC = string_to_lower(string_to_upper(UC)),
    UC = string_to_upper(string_to_lower(LC)),
    UC = string_to_upper(string_to_lower(UC)),

    ok.

string_to_lower(Str) ->
    Res = string:to_lower(Str),
    Res = [string:to_lower(C) || C <- Str].

string_to_upper(Str) ->
    Res = string:to_upper(Str),
    Res = [string:to_upper(C) || C <- Str].

all_upper_latin1([C|T], C) when 0 =< C, C < $a;
				$z < C, C < 16#E0;
				C =:= 16#F7; C =:= 16#FF ->
    all_upper_latin1(T, C+1);
all_upper_latin1([H|T], C) when $a =< C, C =< $z;
				16#E0 =< C, C =< 16#F6;
				16#F8 =< C, C =< 16#FE ->
    H = C - 32,
    all_upper_latin1(T, C+1);
all_upper_latin1([], 256) -> ok.

all_lower_latin1([C|T], C) when 0 =< C, C < $A;
				$Z < C, C < 16#C0;
				C =:= 16#D7;
				16#DF =< C, C =< 255 ->
    all_lower_latin1(T, C+1);
all_lower_latin1([H|T], C) when $A =< C, C =< $Z;
				16#C0 =< C, C =< 16#F6;
				16#C8 =< C, C =< 16#DE ->
    % io:format("~p\n", [{H,C}]),
    H = C + 32,
    all_lower_latin1(T, C+1);
all_lower_latin1([], 256) -> ok.

join(Config) when is_list(Config) ->
    "erlang rules" = string:join(["erlang", "rules"], " "),
    "a,-,b,-,c" = string:join(["a", "b", "c"], ",-,"),
    "1234" = string:join(["1", "2", "3", "4"], ""),
    [] = string:join([], ""), % OTP-7231
    %% invalid arg type
    {'EXIT',_} = (catch string:join([apa], "")),
    ok.
