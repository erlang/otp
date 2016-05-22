%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
-export([len/1,equal/1,concat/1,chr_rchr/1,str_rstr/1]).
-export([span_cspan/1,substr/1,tokens/1,chars/1]).
-export([copies/1,words/1,strip/1,sub_word/1,left_right/1]).
-export([sub_string/1,centre/1, join/1]).
-export([to_integer/1,to_float/1]).
-export([to_upper_to_lower/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [len, equal, concat, chr_rchr, str_rstr, span_cspan,
     substr, tokens, chars, copies, words, strip, sub_word,
     left_right, sub_string, centre, join, to_integer,
     to_float, to_upper_to_lower].

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


init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%%
%% Test cases starts here.
%%

len(Config) when is_list(Config) ->
    0 = string:len(""),
    L = tuple_size(list_to_tuple(atom_to_list(?MODULE))),
    L = string:len(atom_to_list(?MODULE)),
    %% invalid arg type
    {'EXIT',_} = (catch string:len({})),
    ok.

equal(Config) when is_list(Config) ->
    true = string:equal("", ""),
    false = string:equal("", " "),
    true = string:equal("laban", "laban"),
    false = string:equal("skvimp", "skvump"),
    %% invalid arg type
    true = string:equal(2, 2),			% not good, should crash
    ok.

concat(Config) when is_list(Config) ->
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

tokens(Config) when is_list(Config) ->
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
    10 = length(string:chars(32, 10, [])),
    "aaargh" = string:chars($a, 3, "rgh"),
    %% invalid arg type
    {'EXIT',_} = (catch string:chars($x, [])),
    ok.

copies(Config) when is_list(Config) ->
    "" = string:copies("", 10),
    "" = string:copies(".", 0),
    "." = string:copies(".", 1),
    30 = length(string:copies("123", 10)),
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

to_integer(Config) when is_list(Config) ->
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
    {error,not_a_list} = test_to_integer('23'),
    {3,[[]]} = test_to_integer([$3,[]]),
    {3,[hello]} = test_to_integer([$3,hello]),
    ok.

test_to_integer(Str) ->
    io:format("Checking ~p~n", [Str]),
    case string:to_integer(Str) of
	{error,_Reason} = Bad ->
	    {'EXIT',_} = (catch list_to_integer(Str)),
	    Bad;
	{F,_Rest} = Res ->
	    _ = integer_to_list(F),
	    Res
    end.

to_float(Config) when is_list(Config) ->
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
    {error,not_a_list} = test_to_float('2.3'),
    {2.3,[[]]} = test_to_float([$2,$.,$3,[]]),
    {2.3,[hello]} = test_to_float([$2,$.,$3,hello]),
    ok.

test_to_float(Str) ->
    io:format("Checking ~p~n", [Str]),
    case string:to_float(Str) of
	{error,_Reason} = Bad ->
	    {'EXIT',_} = (catch list_to_float(Str)),
	    Bad;
	{F,_Rest} = Res ->
	    _ = float_to_list(F),
	    Res
    end.

to_upper_to_lower(Config) when is_list(Config) ->
    "1234ABCDEFÅÄÖ=" = string:to_upper("1234abcdefåäö="),
    "éèíúùòóåäöabc()" = string:to_lower("ÉÈÍÚÙÒÓÅÄÖabc()"),
    All = lists:seq(0, 255),

    UC = string:to_upper(All),
    256 = length(UC),
    all_upper_latin1(UC, 0),

    LC = string:to_lower(All),
    all_lower_latin1(LC, 0),

    LC = string:to_lower(string:to_upper(LC)),
    LC = string:to_lower(string:to_upper(UC)),
    UC = string:to_upper(string:to_lower(LC)),
    UC = string:to_upper(string:to_lower(UC)),
    ok.

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
    io:format("~p\n", [{H,C}]),
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
