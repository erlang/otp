%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2013. All Rights Reserved.
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
-include_lib("test_server/include/test_server.hrl").


% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).

% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

% Test cases must be exported.
-export([len/1,equal/1,concat/1,chr_rchr/1,str_rstr/1]).
-export([span_cspan/1,substr/1,tokens/1,chars/1]).
-export([copies/1,words/1,strip/1,sub_word/1,left_right/1]).
-export([sub_string/1,centre/1, join/1]).
-export([to_integer/1,to_float/1]).
-export([to_upper_to_lower/1]).
%%
%% all/1
%%
suite() -> [{ct_hooks,[ts_install_cth]}].

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
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%
% Test cases starts here.
%

len(suite) ->
    [];
len(doc) ->
    [];
len(Config) when is_list(Config) ->
    ?line 0 = string:len(""),
    ?line L = tuple_size(list_to_tuple(atom_to_list(?MODULE))),
    ?line L = string:len(atom_to_list(?MODULE)),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:len({})),
    ok.

equal(suite) ->
    [];
equal(doc) ->
    [];
equal(Config) when is_list(Config) ->
    ?line true = string:equal("", ""),
    ?line false = string:equal("", " "),
    ?line true = string:equal("laban", "laban"),
    ?line false = string:equal("skvimp", "skvump"),
    %% invalid arg type
    ?line true = string:equal(2, 2),			% not good, should crash
    ok.

concat(suite) ->
    [];
concat(doc) ->
    [];
concat(Config) when is_list(Config) ->
    ?line "erlang rules" = string:concat("erlang ", "rules"),
    ?line "" = string:concat("", ""),
    ?line "x" = string:concat("x", ""),
    ?line "y" = string:concat("", "y"),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:concat(hello, please)),
    ok.

chr_rchr(suite) ->
    [];
chr_rchr(doc) ->
    [];
chr_rchr(Config) when is_list(Config) ->
    {_,_,X} = erlang:timestamp(),
    ?line 0 = string:chr("", (X rem (255-32)) + 32),
    ?line 0 = string:rchr("", (X rem (255-32)) + 32),
    ?line 1 = string:chr("x", $x),
    ?line 1 = string:rchr("x", $x),
    ?line 1 = string:chr("xx", $x),
    ?line 2 = string:rchr("xx", $x),
    ?line 3 = string:chr("xyzyx", $z),
    ?line 3 = string:rchr("xyzyx", $z),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:chr(hello, $h)),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:chr("hello", h)),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:rchr(hello, $h)),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:rchr("hello", h)),
    ok.

str_rstr(suite) ->
    [];
str_rstr(doc) ->
    [];
str_rstr(Config) when is_list(Config) ->
    {_,_,X} = erlang:timestamp(),
    ?line 0 = string:str("", [(X rem (255-32)) + 32]),
    ?line 0 = string:rstr("", [(X rem (255-32)) + 32]),
    ?line 1 = string:str("x", "x"),
    ?line 1 = string:rstr("x", "x"),
    ?line 0 = string:str("hello", ""),
    ?line 0 = string:rstr("hello", ""),
    ?line 1 = string:str("xxxx", "xx"),
    ?line 3 = string:rstr("xxxx", "xx"),
    ?line 3 = string:str("xy z yx", " z"),
    ?line 3 = string:rstr("xy z yx", " z"),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:str(hello, "he")),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:str("hello", he)),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:rstr(hello, "he")),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:rstr("hello", he)),
    ok.

span_cspan(suite) ->
    [];
span_cspan(doc) ->
    [];
span_cspan(Config) when is_list(Config) ->
    ?line 0 = string:span("", "1"),
    ?line 0 = string:span("1", ""),
    ?line 0 = string:cspan("", "1"),
    ?line 1 = string:cspan("1", ""),
    ?line 1 = string:span("1  ", "1"),
    ?line 5 = string:span("  1  ", "12 "),
    ?line 6 = string:span("1231234", "123"),
    ?line 0 = string:cspan("1  ", "1"),
    ?line 1 = string:cspan("3 ", "12 "),
    ?line 6 = string:cspan("1231234", "4"),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:span(1234, "1")),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:span(1234, "1")),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:cspan("1234", 1)),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:cspan("1234", 4)),
    ok.


substr(suite) ->
    [];
substr(doc) ->
    [];
substr(Config) when is_list(Config) ->
    ?line {'EXIT',_} = (catch string:substr("", 0)), 
    ?line [] = string:substr("", 1), 
    ?line {'EXIT',_} = (catch string:substr("", 2)), 
    ?line [] = string:substr("1", 2), 
    ?line {'EXIT',_} = (catch  string:substr("", 0, 1)), 
    ?line [] = string:substr("", 1, 1), 
    ?line [] = string:substr("", 1, 2), 
    ?line {'EXIT',_} = (catch string:substr("", 2, 2)),
    ?line "1234" = string:substr("1234", 1), 
    ?line "1234" = string:substr("1234", 1, 4), 
    ?line "1234" = string:substr("1234", 1, 5), 
    ?line "23" = string:substr("1234", 2, 2), 
    ?line "4" = string:substr("1234", 4), 
    ?line "" = string:substr("1234", 4, 0), 
    ?line "4" = string:substr("1234", 4, 1), 
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:substr(1234, 1)),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:substr("1234", "1")),
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

chars(suite) ->
    [];
chars(doc) ->
    [];
chars(Config) when is_list(Config) ->
    ?line [] = string:chars($., 0),
    ?line [] = string:chars($., 0, []),
    ?line 10 = length(string:chars(32, 10, [])),
    ?line "aaargh" = string:chars($a, 3, "rgh"),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:chars($x, [])),
    ok.

copies(suite) ->
    [];
copies(doc) ->
    [];
copies(Config) when is_list(Config) ->
    ?line "" = string:copies("", 10),
    ?line "" = string:copies(".", 0),
    ?line "." = string:copies(".", 1),
    ?line 30 = length(string:copies("123", 10)),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:copies("hej", -1)),
    ?line {'EXIT',_} = (catch string:copies("hej", 2.0)),
    ok.

words(suite) ->
    [];
words(doc) ->
    [];
words(Config) when is_list(Config) ->
    ?line 1 = string:words(""),
    ?line 1 = string:words("", $,),
    ?line 1 = string:words("hello"),
    ?line 1 = string:words("hello", $,),
    ?line 1 = string:words("...", $.),
    ?line 2 = string:words("2.35", $.),
    ?line 100 = string:words(string:copies(". ", 100)),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:chars(hej, 1)),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:chars("hej", 1, " ")),
    ok.


strip(suite) ->
    [];
strip(doc) ->
    [];
strip(Config) when is_list(Config) ->
    ?line "" = string:strip(""),
    ?line "" = string:strip("", both),  
    ?line "" = string:strip("", both, $.),  
    ?line "hej" = string:strip("  hej  "),  
    ?line "hej  " = string:strip("  hej  ", left),
    ?line "  hej" = string:strip("  hej  ", right),
    ?line "  hej  " = string:strip("  hej  ", right, $.),
    ?line "hej  hopp" = string:strip("  hej  hopp  ", both),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:strip(hej)),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:strip(" hej", up)),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:strip(" hej", left, " ")),	% not good
    ok.

sub_word(suite) ->
    [];
sub_word(doc) ->
    [];
sub_word(Config) when is_list(Config) ->
    ?line "" = string:sub_word("", 1),
    ?line "" = string:sub_word("", 1, $,),
    ?line {'EXIT',_} = (catch string:sub_word("1 2 3", 0)),
    ?line "" = string:sub_word("1 2 3", 4),
    ?line "llo th" = string:sub_word("but hello there", 2, $e),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:sub_word('hello there', 1)),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:sub_word("hello there", 1, "e")),
    ok.

left_right(suite) ->
    [];
left_right(doc) ->
    [];
left_right(Config) when is_list(Config) ->
    ?line "" = string:left("", 0),
    ?line "" = string:left("hej", 0),
    ?line "" = string:left("hej", 0, $.),
    ?line "" = string:right("", 0),
    ?line "" = string:right("hej", 0),
    ?line "" = string:right("hej", 0, $.),
    ?line "123  " = string:left("123 ", 5),
    ?line "  123" = string:right(" 123", 5),
    ?line "123!!" = string:left("123!", 5, $!),
    ?line "==123" = string:right("=123", 5, $=),
    ?line "1" = string:left("123", 1, $.),
    ?line "3" = string:right("123", 1, $.),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:left(hello, 5)),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:right(hello, 5)),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:left("hello", 5, ".")),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:right("hello", 5, ".")),
    ok.

sub_string(suite) ->
    [];
sub_string(doc) ->
    [];
sub_string(Config) when is_list(Config) ->
    ?line {'EXIT',_} = (catch string:sub_string("", 0)), 
    ?line [] = string:sub_string("", 1), 
    ?line {'EXIT',_} = (catch string:sub_string("", 2)), 
    ?line [] = string:sub_string("1", 2), 
    ?line {'EXIT',_} = (catch string:sub_string("", 0, 1)), 
    ?line [] = string:sub_string("", 1, 1), 
    ?line [] = string:sub_string("", 1, 2), 
    ?line {'EXIT',_} = (catch string:sub_string("", 2, 2)),
    ?line "1234" = string:sub_string("1234", 1), 
    ?line "1234" = string:sub_string("1234", 1, 4), 
    ?line "1234" = string:sub_string("1234", 1, 5), 
    ?line "23" = string:sub_string("1234", 2, 3), 
    ?line "4" = string:sub_string("1234", 4), 
    ?line "4" = string:sub_string("1234", 4, 4), 
    ?line "4" = string:sub_string("1234", 4, 5), 
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:sub_string(1234, 1)),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:sub_string("1234", "1")),
    ok.

centre(suite) ->
    [];
centre(doc) ->
    [];
centre(Config) when is_list(Config) ->
    ?line "" = string:centre("", 0),
    ?line "" = string:centre("1", 0),
    ?line "" = string:centre("", 0, $-),
    ?line "" = string:centre("1", 0, $-),
    ?line "gd" = string:centre("agda", 2),
    ?line "agda " = string:centre("agda", 5),
    ?line " agda " = string:centre("agda", 6),
    ?line "agda." = string:centre("agda", 5, $.),
    ?line "--agda--" = string:centre("agda", 8, $-),
    ?line "agda" = string:centre("agda", 4),
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:centre(hello, 10)),
    ok.

to_integer(suite) ->
    [];
to_integer(doc) ->
    [];
to_integer(Config) when is_list(Config) ->
    ?line {1,""} = test_to_integer("1"),
    ?line {1,""} = test_to_integer("+1"),
    ?line {-1,""} = test_to_integer("-1"),
    ?line {1,"="} = test_to_integer("1="),
    ?line {7,"F"} = test_to_integer("7F"),
    ?line {709,""} = test_to_integer("709"),
    ?line {709,"*2"} = test_to_integer("709*2"),
    ?line {0,"xAB"} = test_to_integer("0xAB"),
    ?line {16,"#FF"} = test_to_integer("16#FF"),
    ?line {error,no_integer} = test_to_integer(""),
    ?line {error,no_integer} = test_to_integer("!1"),
    ?line {error,no_integer} = test_to_integer("F1"),
    ?line {error,not_a_list} = test_to_integer('23'),
    ?line {3,[[]]} = test_to_integer([$3,[]]),
    ?line {3,[hello]} = test_to_integer([$3,hello]),
    ok.

test_to_integer(Str) ->
    io:format("Checking ~p~n", [Str]),
    case string:to_integer(Str) of
	{error,_Reason} = Bad ->
	    ?line {'EXIT',_} = (catch list_to_integer(Str)),
	    Bad;
	{F,_Rest} = Res ->
	    ?line _ = integer_to_list(F),
	    Res
    end.

to_float(suite) ->
    [];
to_float(doc) ->
    [];
to_float(Config) when is_list(Config) ->
    ?line {1.2,""} = test_to_float("1.2"),
    ?line {1.2,""} = test_to_float("1,2"),
    ?line {120.0,""} = test_to_float("1.2e2"),
    ?line {120.0,""} = test_to_float("+1,2e2"),
    ?line {-120.0,""} = test_to_float("-1.2e2"),
    ?line {-120.0,""} = test_to_float("-1,2e+2"),
    ?line {-1.2e-2,""} = test_to_float("-1.2e-2"),
    ?line {1.2,"="} = test_to_float("1.2="),
    ?line {7.9,"e"} = test_to_float("7.9e"),
    ?line {7.9,"ee"} = test_to_float("7.9ee"),
    ?line {7.9,"e+"} = test_to_float("7.9e+"),
    ?line {7.9,"e-"} = test_to_float("7.9e-"),
    ?line {7.9,"e++"} = test_to_float("7.9e++"),
    ?line {7.9,"e--"} = test_to_float("7.9e--"),
    ?line {7.9,"e+e"} = test_to_float("7.9e+e"),
    ?line {7.9,"e-e"} = test_to_float("7.9e-e"),
    ?line {7.9,"e+."} = test_to_float("7.9e+."),
    ?line {7.9,"e-."} = test_to_float("7.9e-."),
    ?line {7.9,"e+,"} = test_to_float("7.9e+,"),
    ?line {7.9,"e-,"} = test_to_float("7.9e-,"),
    ?line {error,no_float} = test_to_float(""),
    ?line {error,no_float} = test_to_float("e1,0"),
    ?line {error,no_float} = test_to_float("1;0"),
    ?line {error,no_float} = test_to_float("1"),
    ?line {error,no_float} = test_to_float("1e"),
    ?line {error,no_float} = test_to_float("2."),
    ?line {error,not_a_list} = test_to_float('2.3'),
    ?line {2.3,[[]]} = test_to_float([$2,$.,$3,[]]),
    ?line {2.3,[hello]} = test_to_float([$2,$.,$3,hello]),
    ok.

test_to_float(Str) ->
    io:format("Checking ~p~n", [Str]),
    case string:to_float(Str) of
	{error,_Reason} = Bad ->
	    ?line {'EXIT',_} = (catch list_to_float(Str)),
	    Bad;
	{F,_Rest} = Res ->
	    ?line _ = float_to_list(F),
	    Res
    end.
	    
to_upper_to_lower(suite) ->
    [];
to_upper_to_lower(doc) ->
    [];
to_upper_to_lower(Config) when is_list(Config) ->
    ?line "1234ABCDEFÅÄÖ=" = string:to_upper("1234abcdefåäö="),
    ?line "éèíúùòóåäöabc()" = string:to_lower("ÉÈÍÚÙÒÓÅÄÖabc()"),
    ?line All = lists:seq(0, 255),

    ?line UC = string:to_upper(All),
    ?line 256 = length(UC),
    ?line all_upper_latin1(UC, 0),

    ?line LC = string:to_lower(All),
    ?line all_lower_latin1(LC, 0),

    ?line LC = string:to_lower(string:to_upper(LC)),
    ?line LC = string:to_lower(string:to_upper(UC)),
    ?line UC = string:to_upper(string:to_lower(LC)),
    ?line UC = string:to_upper(string:to_lower(UC)),
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

join(suite) ->
    [];
join(doc) ->
    [];
join(Config) when is_list(Config) ->
    ?line "erlang rules" = string:join(["erlang", "rules"], " "),
    ?line "a,-,b,-,c" = string:join(["a", "b", "c"], ",-,"),
    ?line "1234" = string:join(["1", "2", "3", "4"], ""),
    ?line [] = string:join([], ""), % OTP-7231
    %% invalid arg type
    ?line {'EXIT',_} = (catch string:join([apa], "")),
    ok.
