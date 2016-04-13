%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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

-module(erl_scan_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2,
	 init_per_group/2,end_per_group/2]).

-export([error_1/1, error_2/1, iso88591/1, otp_7810/1, otp_10302/1,
	 otp_10990/1, otp_10992/1, otp_11807/1]).

-import(lists, [nth/2,flatten/1]).
-import(io_lib, [print/1]).

%%
%% Define to run outside of test server
%%
%%-define(STANDALONE,1).

-ifdef(STANDALONE).
-compile(export_all).
-define(line, put(line, ?LINE), ).
-define(config(A,B),config(A,B)).
-define(t, test_server).
%% config(priv_dir, _) ->
%%     ".";
%% config(data_dir, _) ->
%%     ".".
-else.
-include_lib("common_test/include/ct.hrl").
-endif.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,20}}].

all() -> 
    [{group, error}, iso88591, otp_7810, otp_10302, otp_10990, otp_10992,
     otp_11807].

groups() -> 
    [{error, [], [error_1, error_2]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%% (OTP-2347)
error_1(Config) when is_list(Config) ->
    {error, _, _} = erl_scan:string("'a"),
    ok.

%% Checks that format_error works on the error cases.
error_2(Config) when is_list(Config) ->
    lists:foreach(fun check/1, error_cases()),
    ok.

error_cases() ->
    ["'a",
     "\"a",
     "'\\",
     "\"\\",
     "$",
     "$\\",
     "2.3e",
     "2.3e-",
     "91#9"
    ].

assert_type(N, integer) when is_integer(N) ->
    ok;
assert_type(N, atom) when is_atom(N) ->
    ok.

check(String) ->
    Error = erl_scan:string(String),
    check_error(Error, erl_scan).

%%% (This should be useful for all format_error functions.)
check_error({error, Info, EndLine}, Module0) ->
    {ErrorLine, Module, Desc} = Info,
    true = (Module == Module0),
    assert_type(EndLine, integer),
    assert_type(ErrorLine, integer),
    true = (ErrorLine =< EndLine),
    String = lists:flatten(Module0:format_error(Desc)),
    true = io_lib:printable_list(String).

%% Tests the support for ISO-8859-1 i.e Latin-1.
iso88591(Config) when is_list(Config) ->
    ok =
	case catch begin
		       %% Some atom and variable names
		       V1s = [$Á,$á,$é,$ë],
		       V2s = [$N,$ä,$r],
		       A1s = [$h,$ä,$r],
		       A2s = [$ö,$r,$e],
		       %% Test parsing atom and variable characters.
		       {ok,Ts1,_} = erl_scan_string(V1s ++ " " ++ V2s ++
							"\327" ++
							A1s ++ " " ++ A2s),
		       V1s = atom_to_list(element(3, nth(1, Ts1))),
		       V2s = atom_to_list(element(3, nth(2, Ts1))),
		       A1s = atom_to_list(element(3, nth(4, Ts1))),
		       A2s = atom_to_list(element(3, nth(5, Ts1))),
		       %% Test printing atoms
		       A1s = flatten(print(element(3, nth(4, Ts1)))),
		       A2s = flatten(print(element(3, nth(5, Ts1)))),
		       %% Test parsing and printing strings.
		       S1 = V1s ++ "\327" ++ A1s ++ "\250" ++ A2s,
		       S1s = "\"" ++ S1 ++ "\"",
		       {ok,Ts2,_} = erl_scan_string(S1s),
		       S1 = element(3, nth(1, Ts2)),
		       S1s = flatten(print(element(3, nth(1, Ts2)))),
		       ok				%It all worked
		   end of
	    {'EXIT',R} ->				%Something went wrong!
		{error,R};
	    ok -> ok				%Aok
	end.

%% OTP-7810. White spaces, comments, and more...
otp_7810(Config) when is_list(Config) ->
    ok = reserved_words(),
    ok = atoms(),
    ok = punctuations(),
    ok = comments(),
    ok = errors(),
    ok = integers(),
    ok = base_integers(),
    ok = floats(),
    ok = dots(),
    ok = chars(),
    ok = variables(),
    ok = eof(),
    ok = illegal(),
    ok = crashes(),

    ok = options(),
    ok = token_info(),
    ok = column_errors(),
    ok = white_spaces(),

    ok = unicode(),

    ok = more_chars(),
    ok = more_options(),
    ok = anno_info(),

    ok.

reserved_words() ->
    L = ['after', 'begin', 'case', 'try', 'cond', 'catch',
         'andalso', 'orelse', 'end', 'fun', 'if', 'let', 'of',
         'receive', 'when', 'bnot', 'not', 'div',
         'rem', 'band', 'and', 'bor', 'bxor', 'bsl', 'bsr',
         'or', 'xor'],
    [begin
         {RW, true} = {RW, erl_scan:reserved_word(RW)},
         S = atom_to_list(RW),
         Ts = [{RW,{1,1}}],
         test_string(S, Ts)
     end || RW <- L],
    ok.


atoms() ->
    test_string("a
                 b", [{atom,{1,1},a},{atom,{2,18},b}]),
    test_string("'a b'", [{atom,{1,1},'a b'}]),
		test_string("a", [{atom,{1,1},a}]),
		test_string("a@2", [{atom,{1,1},a@2}]),
		test_string([39,65,200,39], [{atom,{1,1},'AÈ'}]),
		test_string("ärlig östen", [{atom,{1,1},ärlig},{atom,{1,7},östen}]),
		{ok,[{atom,_,'$a'}],{1,6}} =
		    erl_scan_string("'$\\a'", {1,1}),
		test("'$\\a'"),
		ok.

punctuations() ->
    L = ["<<", "<-", "<=", "<", ">>", ">=", ">", "->", "--",
         "-", "++", "+", "=:=", "=/=", "=<", "=>", "==", "=", "/=",
         "/", "||", "|", ":=", "::", ":"],
    %% One token at a time:
    [begin
         W = list_to_atom(S),
         Ts = [{W,{1,1}}],
         test_string(S, Ts)
     end || S <- L],
    Three = ["/=:=", "<=:=", "==:=", ">=:="], % three tokens...
    No = Three ++ L,
    SL0 = [{S1++S2,{-length(S1),S1,S2}} ||
              S1 <- L,
              S2 <- L,
              not lists:member(S1++S2, No)],
    SL = family_list(SL0),
    %% Two tokens. When there are several answers, the one with
    %% the longest first token is chosen:
    %% [the special case "=<<" is among the tested ones]
    [begin
         W1 = list_to_atom(S1),
         W2 = list_to_atom(S2),
         Ts = [{W1,{1,1}},{W2,{1,-L2+1}}],
         test_string(S, Ts)
     end || {S,[{L2,S1,S2}|_]}  <- SL],

    PTs1 = [{'!',{1,1}},{'(',{1,2}},{')',{1,3}},{',',{1,4}},{';',{1,5}},
            {'=',{1,6}},{'[',{1,7}},{']',{1,8}},{'{',{1,9}},{'|',{1,10}},
            {'}',{1,11}}],
    test_string("!(),;=[]{|}", PTs1),

    PTs2 = [{'#',{1,1}},{'&',{1,2}},{'*',{1,3}},{'+',{1,4}},{'/',{1,5}},
            {':',{1,6}},{'<',{1,7}},{'>',{1,8}},{'?',{1,9}},{'@',{1,10}},
            {'\\',{1,11}},{'^',{1,12}},{'`',{1,13}},{'~',{1,14}}],
    test_string("#&*+/:<>?@\\^`~", PTs2),

    test_string(".. ", [{'..',{1,1}}]),
    test_string("1 .. 2",
                [{integer,{1,1},1},{'..',{1,3}},{integer,{1,6},2}]),
    test_string("...", [{'...',{1,1}}]),
    ok.

comments() ->
    test("a %%\n b"),
    {ok,[],1} = erl_scan_string("%"),
    test("a %%\n b"),
    {ok,[{atom,{1,1},a},{atom,{2,2},b}],{2,3}} =
        erl_scan_string("a %%\n b", {1,1}),
    {ok,[{atom,{1,1},a},{comment,{1,3},"%%"},{atom,{2,2},b}],{2,3}} =
        erl_scan_string("a %%\n b",{1,1}, [return_comments]),
    {ok,[{atom,{1,1},a},
         {white_space,{1,2}," "},
         {white_space,{1,5},"\n "},
         {atom,{2,2},b}],
     {2,3}} =
        erl_scan_string("a %%\n b",{1,1},[return_white_spaces]),
    {ok,[{atom,{1,1},a},
         {white_space,{1,2}," "},
         {comment,{1,3},"%%"},
         {white_space,{1,5},"\n "},
         {atom,{2,2},b}],
     {2,3}} = erl_scan_string("a %%\n b",{1,1},[return]),
    ok.

errors() ->
    {error,{1,erl_scan,{string,$',"qa"}},1} = erl_scan:string("'qa"), %'
    {error,{{1,1},erl_scan,{string,$',"qa"}},{1,4}} = %'
        erl_scan:string("'qa", {1,1}, []), %'
    {error,{1,erl_scan,{string,$","str"}},1} = %"
        erl_scan:string("\"str"), %"
    {error,{{1,1},erl_scan,{string,$","str"}},{1,5}} = %"
        erl_scan:string("\"str", {1,1}, []), %"
    {error,{1,erl_scan,char},1} = erl_scan:string("$"),
    {error,{{1,1},erl_scan,char},{1,2}} = erl_scan:string("$", {1,1}, []),
    test_string([34,65,200,34], [{string,{1,1},"AÈ"}]),
    test_string("\\", [{'\\',{1,1}}]),
    {'EXIT',_} =
        (catch {foo, erl_scan:string('$\\a', {1,1})}), % type error
    {'EXIT',_} =
        (catch {foo, erl_scan:tokens([], '$\\a', {1,1})}), % type error

    "{a,tuple}" = erl_scan:format_error({a,tuple}),
    ok.

integers() ->
    [begin
         I = list_to_integer(S),
         Ts = [{integer,{1,1},I}],
         test_string(S, Ts)
     end || S <- [[N] || N <- lists:seq($0, $9)] ++ ["2323","000"] ],
    ok.

base_integers() ->
    [begin
         B = list_to_integer(BS),
         I = erlang:list_to_integer(S, B),
         Ts = [{integer,{1,1},I}],
         test_string(BS++"#"++S, Ts)
     end || {BS,S} <- [{"2","11"}, {"5","23234"}, {"12","05a"},
                       {"16","abcdef"}, {"16","ABCDEF"}] ],

    {error,{1,erl_scan,{base,1}},1} = erl_scan:string("1#000"),
    {error,{{1,1},erl_scan,{base,1}},{1,2}} =
        erl_scan:string("1#000", {1,1}, []),

    test_string("12#bc", [{integer,{1,1},11},{atom,{1,5},c}]),

    [begin
         Str = BS ++ "#" ++ S,
         {error,{1,erl_scan,{illegal,integer}},1} =
             erl_scan:string(Str)
     end || {BS,S} <- [{"3","3"},{"15","f"}, {"12","c"}] ],

    {ok,[{integer,1,239},{'@',1}],1} = erl_scan_string("16#ef@"),
    {ok,[{integer,{1,1},239},{'@',{1,6}}],{1,7}} =
        erl_scan_string("16#ef@", {1,1}, []),
    {ok,[{integer,{1,1},14},{atom,{1,5},g@}],{1,7}} =
        erl_scan_string("16#eg@", {1,1}, []),

    ok.

floats() ->
    [begin
         F = list_to_float(FS),
         Ts = [{float,{1,1},F}],
         test_string(FS, Ts)
     end || FS <- ["1.0","001.17","3.31200","1.0e0","1.0E17",
                   "34.21E-18", "17.0E+14"]],
    test_string("1.e2", [{integer,{1,1},1},{'.',{1,2}},{atom,{1,3},e2}]),

    {error,{1,erl_scan,{illegal,float}},1} =
        erl_scan:string("1.0e400"),
    {error,{{1,1},erl_scan,{illegal,float}},{1,8}} =
        erl_scan:string("1.0e400", {1,1}, []),
    [begin
         {error,{1,erl_scan,{illegal,float}},1} = erl_scan:string(S),
         {error,{{1,1},erl_scan,{illegal,float}},{1,_}} =
             erl_scan:string(S, {1,1}, [])
     end || S <- ["1.14Ea"]],

    ok.

dots() ->
    Dot = [{".",    {ok,[{dot,1}],1}, {ok,[{dot,{1,1}}],{1,2}}},
           {". ",   {ok,[{dot,1}],1}, {ok,[{dot,{1,1}}],{1,3}}},
           {".\n",  {ok,[{dot,1}],2}, {ok,[{dot,{1,1}}],{2,1}}},
           {".%",   {ok,[{dot,1}],1}, {ok,[{dot,{1,1}}],{1,3}}},
           {".\210",{ok,[{dot,1}],1}, {ok,[{dot,{1,1}}],{1,3}}},
           {".% öh",{ok,[{dot,1}],1}, {ok,[{dot,{1,1}}],{1,6}}},
           {".%\n", {ok,[{dot,1}],2}, {ok,[{dot,{1,1}}],{2,1}}},
           {".$",   {error,{1,erl_scan,char},1},
	    {error,{{1,2},erl_scan,char},{1,3}}},
           {".$\\", {error,{1,erl_scan,char},1},
                    {error,{{1,2},erl_scan,char},{1,4}}},
           {".a",   {ok,[{'.',1},{atom,1,a}],1},
	    {ok,[{'.',{1,1}},{atom,{1,2},a}],{1,3}}}
          ],
    [begin
         R = erl_scan_string(S),
         R2 = erl_scan_string(S, {1,1}, [])
     end || {S, R, R2} <- Dot],

    {ok,[{dot,_}=T1],{1,2}} = erl_scan:string(".", {1,1}, text),
    [1, 1, "."] = token_info(T1),
    {ok,[{dot,_}=T2],{1,3}} = erl_scan:string(".%", {1,1}, text),
    [1, 1, "."] = token_info(T2),
    {ok,[{dot,_}=T3],{1,6}} =
        erl_scan:string(".% öh", {1,1}, text),
    [1, 1, "."] = token_info(T3),
    {error,{{1,2},erl_scan,char},{1,3}} = erl_scan:string(".$", {1,1}),
    {error,{{1,2},erl_scan,char},{1,4}} = erl_scan:string(".$\\", {1,1}),

    test_string(". ", [{dot,{1,1}}]),
    test_string(".  ", [{dot,{1,1}}]),
    test_string(".\n", [{dot,{1,1}}]),
    test_string(".\n\n", [{dot,{1,1}}]),
    test_string(".\n\r", [{dot,{1,1}}]),
    test_string(".\n\n\n", [{dot,{1,1}}]),
    test_string(".\210", [{dot,{1,1}}]),
    test_string(".%\n", [{dot,{1,1}}]),
    test_string(".a", [{'.',{1,1}},{atom,{1,2},a}]),

    test_string("%. \n. ", [{dot,{2,1}}]),
    {more,C} = erl_scan:tokens([], "%. ",{1,1}, return),
    {done,{ok,[{comment,{1,1},"%. "},
               {white_space,{1,4},"\n"},
               {dot,{2,1}}],
           {2,3}}, ""} =
        erl_scan_tokens(C, "\n. ", {1,1}, return), % any loc, any options

    [test_string(S, R) ||
        {S, R} <- [{".$\n",   [{'.',{1,1}},{char,{1,2},$\n}]},
                   {"$\\\n",  [{char,{1,1},$\n}]},
                   {"'\\\n'", [{atom,{1,1},'\n'}]},
                   {"$\n",    [{char,{1,1},$\n}]}] ],
    ok.

chars() ->
    [begin
         L = lists:flatten(io_lib:format("$\\~.8b", [C])),
         Ts = [{char,{1,1},C}],
         test_string(L, Ts)
     end || C <- lists:seq(0, 255)],

    %% Leading zeroes...
    [begin
         L = lists:flatten(io_lib:format("$\\~3.8.0b", [C])),
         Ts = [{char,{1,1},C}],
         test_string(L, Ts)
     end || C <- lists:seq(0, 255)],

    %% $\^\n now increments the line...
    [begin
         L = "$\\^" ++ [C],
         Ts = [{char,{1,1},C band 2#11111}],
         test_string(L, Ts)
     end || C <- lists:seq(0, 255)],

    [begin
         L = "$\\" ++ [C],
         Ts = [{char,{1,1},V}],
         test_string(L, Ts)
     end || {C,V} <- [{$n,$\n}, {$r,$\r}, {$t,$\t}, {$v,$\v},
                      {$b,$\b}, {$f,$\f}, {$e,$\e}, {$s,$\s},
                      {$d,$\d}]],

    EC = [$\n,$\r,$\t,$\v,$\b,$\f,$\e,$\s,$\d],
    Ds = lists:seq($0, $9),
    X = [$^,$n,$r,$t,$v,$b,$f,$e,$s,$d],
    New = [${,$x],
    No = EC ++ Ds ++ X ++ New,
    [begin
         L = "$\\" ++ [C],
         Ts = [{char,{1,1},C}],
         test_string(L, Ts)
     end || C <- lists:seq(0, 255) -- No],

    [begin
         L = "'$\\" ++ [C] ++ "'",
         Ts = [{atom,{1,1},list_to_atom("$"++[C])}],
         test_string(L, Ts)
     end || C <- lists:seq(0, 255) -- No],

    test_string("\"\\013a\\\n\"", [{string,{1,1},"\va\n"}]),

    test_string("'\n'", [{atom,{1,1},'\n'}]),
    test_string("\"\n\a\"", [{string,{1,1},"\na"}]),

    %% No escape
    [begin
         L = "$" ++ [C],
         Ts = [{char,{1,1},C}],
         test_string(L, Ts)
     end || C <- lists:seq(0, 255) -- (No ++ [$\\])],
    test_string("$\n", [{char,{1,1},$\n}]),

    {error,{{1,1},erl_scan,char},{1,4}} =
        erl_scan:string("$\\^",{1,1}),
    test_string("$\\\n", [{char,{1,1},$\n}]),
    %% Robert's scanner returns line 1:
    test_string("$\\\n", [{char,{1,1},$\n}]),
    test_string("$\n\n", [{char,{1,1},$\n}]),
    test("$\n\n"),
    ok.


variables() ->
    test_string("     \237_Aouåeiyäö", [{var,{1,7},'_Aouåeiyäö'}]),
    test_string("A_b_c@", [{var,{1,1},'A_b_c@'}]),
    test_string("V@2", [{var,{1,1},'V@2'}]),
    test_string("ABDÀ", [{var,{1,1},'ABDÀ'}]),
    test_string("Ärlig Östen", [{var,{1,1},'Ärlig'},{var,{1,7},'Östen'}]),
    ok.

eof() ->
    {done,{eof,1},eof} = erl_scan:tokens([], eof, 1),
    {more, C1} = erl_scan:tokens([],"    \n", 1),
    {done,{eof,2},eof} = erl_scan:tokens(C1, eof, 1),
    {more, C2} = erl_scan:tokens([], "abra", 1),
    %% An error before R13A.
    %% {done,Err={error,{1,erl_scan,scan},1},eof} =
    {done,{ok,[{atom,1,abra}],1},eof} =
        erl_scan_tokens(C2, eof, 1),

    %% With column.
    {more, C3} = erl_scan:tokens([],"    \n",{1,1}),
    {done,{eof,{2,1}},eof} = erl_scan:tokens(C3, eof, 1),
    {more, C4} = erl_scan:tokens([], "abra", {1,1}),
    %% An error before R13A.
    %% {done,{error,{{1,1},erl_scan,scan},{1,5}},eof} =
    {done,{ok,[{atom,_,abra}],{1,5}},eof} =
        erl_scan_tokens(C4, eof, 1),

    %% Robert's scanner returns "" as LeftoverChars;
    %% the R12B scanner returns eof as LeftoverChars: (eof is correct)
    {more, C5} = erl_scan:tokens([], "a", 1),
    %% An error before R13A.
    %% {done,{error,{1,erl_scan,scan},1},eof} =
    {done,{ok,[{atom,1,a}],1},eof} =
        erl_scan_tokens(C5,eof,1),

    %% With column.
    {more, C6} = erl_scan:tokens([], "a", {1,1}),
    %% An error before R13A.
    %% {done,{error,{1,erl_scan,scan},1},eof} =
    {done,{ok,[{atom,{1,1},a}],{1,2}},eof} =
        erl_scan_tokens(C6,eof,1),

    %% A dot followed by eof is special:
    {more, C} = erl_scan:tokens([], "a.", 1),
    {done,{ok,[{atom,1,a},{dot,1}],1},eof} = erl_scan_tokens(C,eof,1),
    {ok,[{atom,1,foo},{dot,1}],1} = erl_scan_string("foo."),

    %% With column.
    {more, CCol} = erl_scan:tokens([], "a.", {1,1}),
    {done,{ok,[{atom,{1,1},a},{dot,{1,2}}],{1,3}},eof} =
        erl_scan_tokens(CCol,eof,1),
    {ok,[{atom,{1,1},foo},{dot,{1,4}}],{1,5}} =
        erl_scan_string("foo.", {1,1}, []),

    ok.

illegal() ->
    Atom = lists:duplicate(1000, $a),
    {error,{1,erl_scan,{illegal,atom}},1} = erl_scan:string(Atom),
    {done,{error,{1,erl_scan,{illegal,atom}},1},". "} =
        erl_scan:tokens([], Atom++". ", 1),
    QAtom = "'" ++ Atom ++ "'",
    {error,{1,erl_scan,{illegal,atom}},1} = erl_scan:string(QAtom),
    {done,{error,{1,erl_scan,{illegal,atom}},1},". "} =
        erl_scan:tokens([], QAtom++". ", 1),
    Var = lists:duplicate(1000, $A),
    {error,{1,erl_scan,{illegal,var}},1} = erl_scan:string(Var),
    {done,{error,{1,erl_scan,{illegal,var}},1},". "} =
        erl_scan:tokens([], Var++". ", 1),
    Float = "1" ++ lists:duplicate(400, $0) ++ ".0",
    {error,{1,erl_scan,{illegal,float}},1} = erl_scan:string(Float),
    {done,{error,{1,erl_scan,{illegal,float}},1},". "} =
        erl_scan:tokens([], Float++". ", 1),
    String = "\"43\\x{aaaaaa}34\"",
    {error,{1,erl_scan,{illegal,character}},1} = erl_scan:string(String),
    {done,{error,{1,erl_scan,{illegal,character}},1},"34\". "} =
        %% Would be nice if `34\"' were skipped...
        %% Maybe, but then the LeftOverChars would not be the characters
        %% immediately following the end location of the error.
        erl_scan:tokens([], String++". ", 1),

    {error,{{1,1},erl_scan,{illegal,atom}},{1,1001}} =
        erl_scan:string(Atom, {1,1}),
    {done,{error,{{1,5},erl_scan,{illegal,atom}},{1,1005}},". "} =
        erl_scan:tokens([], "foo "++Atom++". ", {1,1}),
    {error,{{1,1},erl_scan,{illegal,atom}},{1,1003}} =
        erl_scan:string(QAtom, {1,1}),
    {done,{error,{{1,5},erl_scan,{illegal,atom}},{1,1007}},". "} =
        erl_scan:tokens([], "foo "++QAtom++". ", {1,1}),
    {error,{{1,1},erl_scan,{illegal,var}},{1,1001}} =
        erl_scan:string(Var, {1,1}),
    {done,{error,{{1,5},erl_scan,{illegal,var}},{1,1005}},". "} =
        erl_scan:tokens([], "foo "++Var++". ", {1,1}),
    {error,{{1,1},erl_scan,{illegal,float}},{1,404}} =
        erl_scan:string(Float, {1,1}),
    {done,{error,{{1,5},erl_scan,{illegal,float}},{1,408}},". "} =
        erl_scan:tokens([], "foo "++Float++". ", {1,1}),
    {error,{{1,4},erl_scan,{illegal,character}},{1,14}} =
        erl_scan:string(String, {1,1}),
    {done,{error,{{1,4},erl_scan,{illegal,character}},{1,14}},"34\". "} =
        erl_scan:tokens([], String++". ", {1,1}),
    ok.

crashes() ->
    {'EXIT',_} = (catch {foo, erl_scan:string([-1])}), % type error
    {'EXIT',_} = (catch {foo, erl_scan:string("$"++[-1])}),
    {'EXIT',_} = (catch {foo, erl_scan:string("$\\"++[-1])}),
    {'EXIT',_} = (catch {foo, erl_scan:string("$\\^"++[-1])}),
    {'EXIT',_} = (catch {foo, erl_scan:string([$",-1,$"],{1,1})}),
    {'EXIT',_} = (catch {foo, erl_scan:string("\"\\v"++[-1,$"])}), %$"
    {'EXIT',_} = (catch {foo, erl_scan:string([$",-1,$"])}),
    {'EXIT',_} = (catch {foo, erl_scan:string("% foo"++[-1])}),
    {'EXIT',_} =
         (catch {foo, erl_scan:string("% foo"++[-1],{1,1})}),

    {'EXIT',_} = (catch {foo, erl_scan:string([a])}), % type error
    {'EXIT',_} = (catch {foo, erl_scan:string("$"++[a])}),
    {'EXIT',_} = (catch {foo, erl_scan:string("$\\"++[a])}),
    {'EXIT',_} = (catch {foo, erl_scan:string("$\\^"++[a])}),
    {'EXIT',_} = (catch {foo, erl_scan:string([$",a,$"],{1,1})}),
    {'EXIT',_} = (catch {foo, erl_scan:string("\"\\v"++[a,$"])}), %$"
    {'EXIT',_} = (catch {foo, erl_scan:string([$",a,$"])}),
    {'EXIT',_} = (catch {foo, erl_scan:string("% foo"++[a])}),
    {'EXIT',_} =
         (catch {foo, erl_scan:string("% foo"++[a],{1,1})}),

    {'EXIT',_} = (catch {foo, erl_scan:string([3.0])}), % type error

    ok.

options() ->
    %% line and column are not options, but tested here
    {ok,[{atom,1,foo},{white_space,1," "},{comment,1,"% bar"}], 1} =
        erl_scan_string("foo % bar", 1, return),
    {ok,[{atom,1,foo},{white_space,1," "}],1} =
        erl_scan_string("foo % bar", 1, return_white_spaces),
    {ok,[{atom,1,foo},{comment,1,"% bar"}],1} =
        erl_scan_string("foo % bar", 1, return_comments),
    {ok,[{atom,17,foo}],17} =
        erl_scan_string("foo % bar", 17),
    {'EXIT',{function_clause,_}} =
        (catch {foo,
                erl_scan:string("foo % bar", {a,1}, [])}), % type error
    {ok,[{atom,_,foo}],{17,18}} =
        erl_scan_string("foo % bar", {17,9}, []),
    {'EXIT',{function_clause,_}} =
        (catch {foo,
                erl_scan:string("foo % bar", {1,0}, [])}), % type error
    {ok,[{foo,1}],1} =
        erl_scan_string("foo % bar",1, [{reserved_word_fun,
                                         fun(W) -> W =:= foo end}]),
    {'EXIT',{badarg,_}} =
        (catch {foo,
                erl_scan:string("foo % bar",1, % type error
                                [{reserved_word_fun,
                                  fun(W,_) -> W =:= foo end}])}),
    ok.

more_options() ->
    {ok,[{atom,_,foo}=T1],{19,20}} =
        erl_scan:string("foo", {19,17},[]),
    {19,17} = erl_scan:location(T1),
    {done,{ok,[{atom,_,foo}=T2,{dot,_}],{19,22}},[]} =
        erl_scan:tokens([], "foo. ", {19,17}, [bad_opt]), % type error
    {19,17} = erl_scan:location(T2),
    {ok,[{atom,_,foo}=T3],{19,20}} =
        erl_scan:string("foo", {19,17},[text]),
    {19,17} = erl_scan:location(T3),
    "foo" = erl_scan:text(T3),

    {ok,[{atom,_,foo}=T4],1} = erl_scan:string("foo", 1, [text]),
    1 = erl_scan:line(T4),
    1 = erl_scan:location(T4),
    "foo" = erl_scan:text(T4),

    ok.

token_info() ->
    {ok,[T1],_} = erl_scan:string("foo", {1,18}, [text]),
    {'EXIT',{badarg,_}} =
        (catch {foo, erl_scan:category(foo)}), % type error
    {'EXIT',{badarg,_}} =
        (catch {foo, erl_scan:symbol(foo)}), % type error
    atom = erl_scan:category(T1),
    foo = erl_scan:symbol(T1),

    {ok,[T2],_} = erl_scan:string("foo", 1, []),
    1 = erl_scan:line(T2),
    undefined = erl_scan:column(T2),
    undefined = erl_scan:text(T2),
    1 = erl_scan:location(T2),

    {ok,[T3],_} = erl_scan:string("=", 1, []),
    '=' = erl_scan:category(T3),
    '=' = erl_scan:symbol(T3),
    ok.

anno_info() ->
    {'EXIT',_} =
        (catch {foo,erl_scan:line(foo)}), % type error
    {ok,[{atom,_,foo}=T0],_} = erl_scan:string("foo", 19, [text]),
    19 = erl_scan:location(T0),
    19 = erl_scan:end_location(T0),

    {ok,[{atom,_,foo}=T3],_} = erl_scan:string("foo", {1,3}, [text]),
    1 = erl_scan:line(T3),
    3 = erl_scan:column(T3),
    {1,3} = erl_scan:location(T3),
    {1,6} = erl_scan:end_location(T3),
    "foo" = erl_scan:text(T3),

    {ok,[{atom,_,foo}=T4],_} = erl_scan:string("foo", 2, [text]),
    2 = erl_scan:line(T4),
    undefined = erl_scan:column(T4),
    2 = erl_scan:location(T4),
    "foo" = erl_scan:text(T4),

    {ok,[{atom,_,foo}=T5],_} = erl_scan:string("foo", {1,3}, []),
    1 = erl_scan:line(T5),
    3 = erl_scan:column(T5),
    {1,3} = erl_scan:location(T5),
    undefined = erl_scan:text(T5),

    ok.

column_errors() ->
    {error,{{1,1},erl_scan,{string,$',""}},{1,3}} = % $'
        erl_scan:string("'\\",{1,1}),
    {error,{{1,1},erl_scan,{string,$",""}},{1,3}} = % $"
        erl_scan:string("\"\\",{1,1}),

    {error,{{1,1},erl_scan,{string,$',""}},{1,2}} =  % $'
        erl_scan:string("'",{1,1}),
    {error,{{1,1},erl_scan,{string,$",""}},{1,2}} =  % $"
        erl_scan:string("\"",{1,1}),

    {error,{{1,1},erl_scan,char},{1,2}} =
        erl_scan:string("$",{1,1}),

    {error,{{1,2},erl_scan,{string,$',"1234567890123456"}},{1,20}} = %'
        erl_scan:string(" '12345678901234567", {1,1}),
    {error,{{1,2},erl_scan,{string,$',"123456789012345 "}}, {1,20}} = %'
        erl_scan:string(" '123456789012345\\s", {1,1}),
    {error,{{1,2},erl_scan,{string,$","1234567890123456"}},{1,20}} = %"
        erl_scan:string(" \"12345678901234567", {1,1}),
    {error,{{1,2},erl_scan,{string,$","123456789012345 "}}, {1,20}} = %"
        erl_scan:string(" \"123456789012345\\s", {1,1}),
    {error,{{1,2},erl_scan,{string,$',"1234567890123456"}},{2,1}} = %'
        erl_scan:string(" '12345678901234567\n", {1,1}),
    ok.

white_spaces() ->
    {ok,[{white_space,_,"\r"},
               {white_space,_,"   "},
               {atom,_,a},
               {white_space,_,"\n"}],
           _} = erl_scan_string("\r   a\n", {1,1}, return),
    test("\r   a\n"),
    L = "{\"a\nb\", \"a\\nb\",\nabc\r,def}.\n\n",
    {ok,[{'{',_},
               {string,_,"a\nb"},
               {',',_},
               {white_space,_," "},
               {string,_,"a\nb"},
               {',',_},
               {white_space,_,"\n"},
               {atom,_,abc},
               {white_space,_,"\r"},
               {',',_},
               {atom,_,def},
               {'}',_},
               {dot,_},
               {white_space,_,"\n"}],
           _} = erl_scan_string(L, {1,1}, return),
    test(L),
    test("\"\n\"\n"),
    test("\n\r\n"),
    test("\n\r"),
    test("\r\n"),
    test("\n\f"),
    [test(lists:duplicate(N, $\t)) || N <- lists:seq(1, 20)],
    [test([$\n|lists:duplicate(N, $\t)]) || N <- lists:seq(1, 20)],
    [test(lists:duplicate(N, $\s)) || N <- lists:seq(1, 20)],
    [test([$\n|lists:duplicate(N, $\s)]) || N <- lists:seq(1, 20)],
    test("\v\f\n\v "),
    test("\n\e\n\b\f\n\da\n"),
    ok.

unicode() ->
    {ok,[{char,1,83},{integer,1,45}],1} =
        erl_scan_string("$\\12345"), % not unicode

    {error,{1,erl_scan,{illegal,character}},1} =
        erl_scan:string([1089]),
    {error,{{1,1},erl_scan,{illegal,character}},{1,2}} =
        erl_scan:string([1089], {1,1}),
    {error,{1,erl_scan,{illegal,atom}},1} =
        erl_scan:string("'a"++[1089]++"b'", 1),
    {error,{{1,1},erl_scan,{illegal,atom}},{1,6}} =
        erl_scan:string("'a"++[1089]++"b'", {1,1}),
    test("\"a"++[1089]++"b\""),
    {ok,[{char,1,1}],1} =
        erl_scan_string([$$,$\\,$^,1089], 1),

    {error,{1,erl_scan,Error},1} =
        erl_scan:string("\"qa\x{aaa}", 1),
    "unterminated string starting with \"qa"++[2730]++"\"" =
        erl_scan:format_error(Error),
    {error,{{1,1},erl_scan,_},{1,11}} =
        erl_scan:string("\"qa\\x{aaa}",{1,1}),
    {error,{{1,1},erl_scan,{illegal,atom}},{1,12}} =
        erl_scan:string("'qa\\x{aaa}'",{1,1}),

    {ok,[{char,1,1089}],1} =
        erl_scan_string([$$,1089], 1),
    {ok,[{char,1,1089}],1} =
        erl_scan_string([$$,$\\,1089], 1),

    Qs = "$\\x{aaa}",
    {ok,[{char,1,$\x{aaa}}],1} =
        erl_scan_string(Qs, 1),
    {ok,[Q2],{1,9}} =
        erl_scan:string("$\\x{aaa}", {1,1}, [text]),
    [{category,char},{column,1},{line,1},{symbol,16#aaa},{text,Qs}] =
        token_info_long(Q2),

    U1 = "\"\\x{aaa}\"",
    {ok,[{string,_,[2730]}=T1],{1,10}} = erl_scan:string(U1, {1,1}, [text]),
    {1,1} = erl_scan:location(T1),
    "\"\\x{aaa}\"" = erl_scan:text(T1),
    {ok,[{string,1,[2730]}],1} = erl_scan_string(U1, 1),

    U2 = "\"\\x41\\x{fff}\\x42\"",
    {ok,[{string,1,[$\x41,$\x{fff},$\x42]}],1} = erl_scan_string(U2, 1),

    U3 = "\"a\n\\x{fff}\n\"",
    {ok,[{string,1,[$a,$\n,$\x{fff},$\n]}],3} = erl_scan_string(U3, 1),

    U4 = "\"\\^\n\\x{aaa}\\^\n\"",
    {ok,[{string,1,[$\n,$\x{aaa},$\n]}],3} = erl_scan_string(U4, 1),

    %% Keep these tests:
    test(Qs),
    test(U1),
    test(U2),
    test(U3),
    test(U4),

    Str1 = "\"ab" ++ [1089] ++ "cd\"",
    {ok,[{string,1,[$a,$b,1089,$c,$d]}],1} = erl_scan_string(Str1, 1),
    {ok,[{string,{1,1},[$a,$b,1089,$c,$d]}],{1,8}} =
        erl_scan_string(Str1, {1,1}),
    test(Str1),
    Comment = "%% "++[1089],
    {ok,[{comment,1,[$%,$%,$\s,1089]}],1} =
        erl_scan_string(Comment, 1, [return]),
    {ok,[{comment,{1,1},[$%,$%,$\s,1089]}],{1,5}} =
        erl_scan_string(Comment, {1,1}, [return]),
    ok.

more_chars() ->
    %% Due to unicode, the syntax has been incompatibly augmented:
    %% $\x{...}, $\xHH

    %% All kinds of tests...
    {ok,[{char,_,123}],{1,4}} =
        erl_scan_string("$\\{",{1,1}),
    {more, C1} = erl_scan:tokens([], "$\\{", {1,1}),
    {done,{ok,[{char,_,123}],{1,4}},eof} =
        erl_scan_tokens(C1, eof, 1),
    {ok,[{char,1,123},{atom,1,a},{'}',1}],1} =
        erl_scan_string("$\\{a}"),

    {error,{{1,1},erl_scan,char},{1,4}} =
        erl_scan:string("$\\x", {1,1}),
    {error,{{1,1},erl_scan,char},{1,5}} =
        erl_scan:string("$\\x{",{1,1}),
    {more, C3} = erl_scan:tokens([], "$\\x", {1,1}),
    {done,{error,{{1,1},erl_scan,char},{1,4}},eof} =
        erl_scan:tokens(C3, eof, 1),
    {error,{{1,1},erl_scan,char},{1,5}} =
        erl_scan:string("$\\x{",{1,1}),
    {more, C2} = erl_scan:tokens([], "$\\x{", {1,1}),
    {done,{error,{{1,1},erl_scan,char},{1,5}},eof} =
        erl_scan:tokens(C2, eof, 1),
    {error,{1,erl_scan,{illegal,character}},1} =
        erl_scan:string("$\\x{g}"),
    {error,{{1,1},erl_scan,{illegal,character}},{1,5}} =
        erl_scan:string("$\\x{g}", {1,1}),
    {error,{{1,1},erl_scan,{illegal,character}},{1,6}} =
        erl_scan:string("$\\x{}",{1,1}),

    test("\"\\{0}\""),
    test("\"\\x{0}\""),
    test("\'\\{0}\'"),
    test("\'\\x{0}\'"),

    {error,{{2,3},erl_scan,{illegal,character}},{2,6}} =
        erl_scan:string("\"ab \n $\\x{g}\"",{1,1}),
    {error,{{2,3},erl_scan,{illegal,character}},{2,6}} =
        erl_scan:string("\'ab \n $\\x{g}\'",{1,1}),

    test("$\\{34}"),
    test("$\\x{34}"),
    test("$\\{377}"),
    test("$\\x{FF}"),
    test("$\\{400}"),
    test("$\\x{100}"),
    test("$\\x{10FFFF}"),
    test("$\\x{10ffff}"),
    test("\"$\n \\{1}\""),
    {error,{1,erl_scan,{illegal,character}},1} =
        erl_scan:string("$\\x{110000}"),
    {error,{{1,1},erl_scan,{illegal,character}},{1,12}} =
        erl_scan:string("$\\x{110000}", {1,1}),

    {error,{{1,1},erl_scan,{illegal,character}},{1,4}} =
        erl_scan:string("$\\xfg", {1,1}),

    test("$\\xffg"),

    {error,{{1,1},erl_scan,{illegal,character}},{1,4}} =
        erl_scan:string("$\\xg", {1,1}),
    ok.

%% OTP-10302. Unicode characters scanner/parser.
otp_10302(Config) when is_list(Config) ->
    %% From unicode():
    {error,{1,erl_scan,{illegal,atom}},1} =
        erl_scan:string("'a"++[1089]++"b'", 1),
    {error,{{1,1},erl_scan,{illegal,atom}},{1,12}} =
        erl_scan:string("'qa\\x{aaa}'",{1,1}),

    {ok,[{char,1,1089}],1} = erl_scan_string([$$,1089], 1),
    {ok,[{char,1,1089}],1} = erl_scan_string([$$,$\\,1089],1),

    Qs = "$\\x{aaa}",
    {ok,[{char,1,2730}],1} = erl_scan_string(Qs, 1),
    {ok,[Q2],{1,9}} = erl_scan:string(Qs,{1,1},[text]),
    [{category,char},{column,1},{line,1},{symbol,16#aaa},{text,Qs}] =
        token_info_long(Q2),

    U1 = "\"\\x{aaa}\"",
    {ok,[T1],{1,10}} = erl_scan:string(U1, {1,1}, [text]),
    [{category,string},{column,1},{line,1},{symbol,[16#aaa]},{text,U1}] =
        token_info_long(T1),

    U2 = "\"\\x41\\x{fff}\\x42\"",
    {ok,[{string,1,[65,4095,66]}],1} = erl_scan_string(U2, 1),

    U3 = "\"a\n\\x{fff}\n\"",
    {ok,[{string,1,[97,10,4095,10]}],3} = erl_scan_string(U3, 1),

    U4 = "\"\\^\n\\x{aaa}\\^\n\"",
    {ok,[{string,1,[10,2730,10]}],3} = erl_scan_string(U4, 1,[]),

    Str1 = "\"ab" ++ [1089] ++ "cd\"",
    {ok,[{string,1,[97,98,1089,99,100]}],1} =
        erl_scan_string(Str1,1),
    {ok,[{string,{1,1},[97,98,1089,99,100]}],{1,8}} =
        erl_scan_string(Str1, {1,1}),

    OK1 = 16#D800-1,
    OK2 = 16#DFFF+1,
    OK3 = 16#FFFE-1,
    OK4 = 16#FFFF+1,
    OKL = [OK1,OK2,OK3,OK4],

    Illegal1 = 16#D800,
    Illegal2 = 16#DFFF,
    Illegal3 = 16#FFFE,
    Illegal4 = 16#FFFF,
    IllegalL = [Illegal1,Illegal2,Illegal3,Illegal4],

    [{ok,[{comment,1,[$%,$%,$\s,OK]}],1} =
         erl_scan_string("%% "++[OK], 1, [return]) ||
        OK <- OKL],
    {ok,[{comment,_,[$%,$%,$\s,OK1]}],{1,5}} =
        erl_scan_string("%% "++[OK1], {1,1}, [return]),
    [{error,{1,erl_scan,{illegal,character}},1} =
         erl_scan:string("%% "++[Illegal], 1, [return]) ||
        Illegal <- IllegalL],
    {error,{{1,1},erl_scan,{illegal,character}},{1,5}} =
        erl_scan:string("%% "++[Illegal1], {1,1}, [return]),

    [{ok,[],1} = erl_scan_string("%% "++[OK], 1, []) ||
        OK <- OKL],
    {ok,[],{1,5}} = erl_scan_string("%% "++[OK1], {1,1}, []),
    [{error,{1,erl_scan,{illegal,character}},1} =
         erl_scan:string("%% "++[Illegal], 1, []) ||
        Illegal <- IllegalL],
    {error,{{1,1},erl_scan,{illegal,character}},{1,5}} =
        erl_scan:string("%% "++[Illegal1], {1,1}, []),

    [{ok,[{string,{1,1},[OK]}],{1,4}} =
        erl_scan_string("\""++[OK]++"\"",{1,1}) ||
        OK <- OKL],
    [{error,{{1,2},erl_scan,{illegal,character}},{1,3}} =
         erl_scan:string("\""++[OK]++"\"",{1,1}) ||
        OK <- IllegalL],

    [{error,{{1,1},erl_scan,{illegal,character}},{1,2}} =
        erl_scan:string([Illegal],{1,1}) ||
        Illegal <- IllegalL],

    {ok,[{char,{1,1},OK1}],{1,3}} =
        erl_scan_string([$$,OK1],{1,1}),
    {error,{{1,1},erl_scan,{illegal,character}},{1,2}} =
        erl_scan:string([$$,Illegal1],{1,1}),

    {ok,[{char,{1,1},OK1}],{1,4}} =
        erl_scan_string([$$,$\\,OK1],{1,1}),
    {error,{{1,1},erl_scan,{illegal,character}},{1,4}} =
        erl_scan:string([$$,$\\,Illegal1],{1,1}),

    {ok,[{string,{1,1},[55295]}],{1,5}} =
        erl_scan_string("\"\\"++[OK1]++"\"",{1,1}),
    {error,{{1,2},erl_scan,{illegal,character}},{1,4}} =
        erl_scan:string("\"\\"++[Illegal1]++"\"",{1,1}),

    {ok,[{char,{1,1},OK1}],{1,10}} =
        erl_scan_string("$\\x{D7FF}",{1,1}),
    {error,{{1,1},erl_scan,{illegal,character}},{1,10}} =
        erl_scan:string("$\\x{D800}",{1,1}),

    %% Not erl_scan, but erl_parse.
    {integer,0,1} = erl_parse_abstract(1),
    Float = 3.14, {float,0,Float} = erl_parse_abstract(Float),
    {nil,0} = erl_parse_abstract([]),
    {bin,0,
     [{bin_element,0,{integer,0,1},default,default},
      {bin_element,0,{integer,0,2},default,default}]} =
        erl_parse_abstract(<<1,2>>),
    {cons,0,{tuple,0,[{atom,0,a}]},{atom,0,b}} =
        erl_parse_abstract([{a} | b]),
    {string,0,"str"} = erl_parse_abstract("str"),
    {cons,0,
     {integer,0,$a},
     {cons,0,{integer,0,55296},{string,0,"c"}}} =
        erl_parse_abstract("a"++[55296]++"c"),

    Line = 17,
    {integer,Line,1} = erl_parse_abstract(1, Line),
    Float = 3.14, {float,Line,Float} = erl_parse_abstract(Float, Line),
    {nil,Line} = erl_parse_abstract([], Line),
    {bin,Line,
     [{bin_element,Line,{integer,Line,1},default,default},
      {bin_element,Line,{integer,Line,2},default,default}]} =
        erl_parse_abstract(<<1,2>>, Line),
    {cons,Line,{tuple,Line,[{atom,Line,a}]},{atom,Line,b}} =
        erl_parse_abstract([{a} | b], Line),
    {string,Line,"str"} = erl_parse_abstract("str", Line),
    {cons,Line,
     {integer,Line,$a},
     {cons,Line,{integer,Line,55296},{string,Line,"c"}}} =
        erl_parse_abstract("a"++[55296]++"c", Line),

    Opts1 = [{line,17}],
    {integer,Line,1} = erl_parse_abstract(1, Opts1),
    Float = 3.14, {float,Line,Float} = erl_parse_abstract(Float, Opts1),
    {nil,Line} = erl_parse_abstract([], Opts1),
    {bin,Line,
     [{bin_element,Line,{integer,Line,1},default,default},
      {bin_element,Line,{integer,Line,2},default,default}]} =
        erl_parse_abstract(<<1,2>>, Opts1),
    {cons,Line,{tuple,Line,[{atom,Line,a}]},{atom,Line,b}} =
        erl_parse_abstract([{a} | b], Opts1),
    {string,Line,"str"} = erl_parse_abstract("str", Opts1),
    {cons,Line,
     {integer,Line,$a},
     {cons,Line,{integer,Line,55296},{string,Line,"c"}}} =
        erl_parse_abstract("a"++[55296]++"c", Opts1),

    [begin
         {integer,Line,1} = erl_parse_abstract(1, Opts2),
         Float = 3.14, {float,Line,Float} = erl_parse_abstract(Float, Opts2),
         {nil,Line} = erl_parse_abstract([], Opts2),
         {bin,Line,
          [{bin_element,Line,{integer,Line,1},default,default},
           {bin_element,Line,{integer,Line,2},default,default}]} =
             erl_parse_abstract(<<1,2>>, Opts2),
         {cons,Line,{tuple,Line,[{atom,Line,a}]},{atom,Line,b}} =
             erl_parse_abstract([{a} | b], Opts2),
         {string,Line,"str"} = erl_parse_abstract("str", Opts2),
         {string,Line,[97,1024,99]} =
             erl_parse_abstract("a"++[1024]++"c", Opts2)
     end || Opts2 <- [[{encoding,unicode},{line,Line}],
                      [{encoding,utf8},{line,Line}]]],

    {cons,0,
     {integer,0,97},
     {cons,0,{integer,0,1024},{string,0,"c"}}} =
        erl_parse_abstract("a"++[1024]++"c", [{encoding,latin1}]),
    ok.

%% OTP-10990. Floating point number in input string.
otp_10990(Config) when is_list(Config) ->
    {'EXIT',_} = (catch {foo, erl_scan:string([$",42.0,$"],1)}),
    ok.

%% OTP-10992. List of floats to abstract format.
otp_10992(Config) when is_list(Config) ->
    {cons,0,{float,0,42.0},{nil,0}} =
        erl_parse_abstract([42.0], [{encoding,unicode}]),
    {cons,0,{float,0,42.0},{nil,0}} =
        erl_parse_abstract([42.0], [{encoding,utf8}]),
    {cons,0,{integer,0,65},{cons,0,{float,0,42.0},{nil,0}}} =
        erl_parse_abstract([$A,42.0], [{encoding,unicode}]),
    {cons,0,{integer,0,65},{cons,0,{float,0,42.0},{nil,0}}} =
        erl_parse_abstract([$A,42.0], [{encoding,utf8}]),
    ok.

%% OTP-11807. Generalize erl_parse:abstract/2.
otp_11807(Config) when is_list(Config) ->
    {cons,0,{integer,0,97},{cons,0,{integer,0,98},{nil,0}}} =
        erl_parse_abstract("ab", [{encoding,none}]),
    {cons,0,{integer,0,-1},{nil,0}} =
        erl_parse_abstract([-1], [{encoding,latin1}]),
    ASCII = fun(I) -> I >= 0 andalso I < 128 end,
    {string,0,"xyz"} = erl_parse_abstract("xyz", [{encoding,ASCII}]),
    {cons,0,{integer,0,228},{nil,0}} =
        erl_parse_abstract([228], [{encoding,ASCII}]),
    {cons,0,{integer,0,97},{atom,0,a}} =
        erl_parse_abstract("a"++a, [{encoding,latin1}]),
    {'EXIT', {{badarg,bad},_}} = % minor backward incompatibility
         (catch erl_parse:abstract("string", [{encoding,bad}])),
   ok.

test_string(String, ExpectedWithCol) ->
    {ok, ExpectedWithCol, _EndWithCol} = erl_scan_string(String, {1, 1}, []),
    Expected = [ begin
                     {L,_C} = element(2, T),
                     setelement(2, T, L)
                 end
                    || T <- ExpectedWithCol ],
    {ok, Expected, _End} = erl_scan_string(String),
    test(String).

erl_scan_string(String) ->
    erl_scan_string(String, 1, []).

erl_scan_string(String, StartLocation) ->
    erl_scan_string(String, StartLocation, []).

erl_scan_string(String, StartLocation, Options) ->
    case erl_scan:string(String, StartLocation, Options) of
        {ok, Tokens, EndLocation} ->
            {ok, unopaque_tokens(Tokens), EndLocation};
        Else ->
            Else
    end.

erl_scan_tokens(C, S, L) ->
    erl_scan_tokens(C, S, L, []).

erl_scan_tokens(C, S, L, O) ->
    case erl_scan:tokens(C, S, L, O) of
        {done, {ok, Ts, End}, R} ->
            {done, {ok, unopaque_tokens(Ts), End}, R};
        Else ->
            Else
    end.

unopaque_tokens([]) ->
    [];
unopaque_tokens([Token|Tokens]) ->
    Attrs = element(2, Token),
    Term = erl_anno:to_term(Attrs),
    T = setelement(2, Token, Term),
    [T | unopaque_tokens(Tokens)].

erl_parse_abstract(Term) ->
    erl_parse_abstract(Term, []).

erl_parse_abstract(Term, Options) ->
    Abstr = erl_parse:abstract(Term, Options),
    unopaque_abstract(Abstr).

unopaque_abstract(Abstr) ->
    erl_parse:anno_to_term(Abstr).

%% test_string(String, Expected, StartLocation, Options) ->
%%     {ok, Expected, _End} = erl_scan:string(String, StartLocation, Options),
%%     test(String).

%% There are no checks of the tags...
test(String) ->
    %% io:format("Testing `~ts'~n", [String]),
    [{Tokens, End},
     {Wtokens, Wend},
     {Ctokens, Cend},
     {CWtokens, CWend},
     {CWtokens2, _}] =
        [scan_string_with_column(String, X) ||
            X <- [[],
                  [return_white_spaces],
                  [return_comments],
                  [return],
                  [return]]], % for white space compaction test

    {end1,End,Wend} = {end1,Wend,End},
    {end2,Wend,Cend} = {end2,Cend,Wend},
    {end3,Cend,CWend} = {end3,CWend,Cend},

    %% Test that the tokens that are common to two token lists are identical.
    {none,Tokens} = {none, filter_tokens(CWtokens, [white_space,comment])},
    {comments,Ctokens} =
        {comments,filter_tokens(CWtokens, [white_space])},
    {white_spaces,Wtokens} =
        {white_spaces,filter_tokens(CWtokens, [comment])},

    %% Use token attributes to extract parts from the original string,
    %% and check that the parts are identical to the token strings.
    {Line,Column} = test_decorated_tokens(String, CWtokens),
    {deco,{Line,Column},End} = {deco,End,{Line,Column}},

    %% Almost the same again: concat texts to get the original:
    Text = get_text(CWtokens),
    {text,Text,String} = {text,String,Text},

    %% Test that white spaces occupy less heap than the worst case.
    ok = test_white_space_compaction(CWtokens, CWtokens2),

    %% Test that white newlines are always first in text:
    WhiteTokens = select_tokens(CWtokens, [white_space]),
    ok = newlines_first(WhiteTokens),

    %% Line attribute only:
    [Simple,Wsimple,Csimple,WCsimple] = Simples =
        [element(2, erl_scan:string(String, 1, Opts)) ||
            Opts <- [[],
                     [return_white_spaces],
                     [return_comments],
                     [return]]],
    {consistent,true} = {consistent,consistent_attributes(Simples)},
    {simple_wc,WCsimple} = {simple_wc,simplify(CWtokens)},
    {simple,Simple} = {simple,filter_tokens(WCsimple, [white_space,comment])},
    {simple_c,Csimple} = {simple_c,filter_tokens(WCsimple, [white_space])},
    {simple_w,Wsimple} = {simple_w,filter_tokens(WCsimple, [comment])},

    %% Line attribute only, with text:
    [SimpleTxt,WsimpleTxt,CsimpleTxt,WCsimpleTxt] = SimplesTxt =
        [element(2, erl_scan:string(String, 1, [text|Opts])) ||
            Opts <- [[],
                     [return_white_spaces],
                     [return_comments],
                     [return]]],
    TextTxt = get_text(WCsimpleTxt),
    {text_txt,TextTxt,String} = {text_txt,String,TextTxt},
    {consistent_txt,true} =
        {consistent_txt,consistent_attributes(SimplesTxt)},
    {simple_txt,SimpleTxt} =
        {simple_txt,filter_tokens(WCsimpleTxt, [white_space,comment])},
    {simple_c_txt,CsimpleTxt} =
        {simple_c_txt,filter_tokens(WCsimpleTxt, [white_space])},
    {simple_w_txt,WsimpleTxt} =
        {simple_w_txt,filter_tokens(WCsimpleTxt, [comment])},

    ok.

test_white_space_compaction(Tokens, Tokens2) when Tokens =:= Tokens2 ->
    [WS, WS2] = [select_tokens(Ts, [white_space]) || Ts <- [Tokens, Tokens2]],
    test_wsc(WS, WS2).

test_wsc([], []) ->
    ok;
test_wsc([Token|Tokens], [Token2|Tokens2]) ->
    [Text, Text2] = [Text ||
                        Text <- [erl_scan:text(T) || T <- [Token, Token2]]],
    Sz = erts_debug:size(Text),
    Sz2 = erts_debug:size({Text, Text2}),
    IsCompacted = Sz2 < 2*Sz+erts_debug:size({a,a}),
    ToBeCompacted = is_compacted(Text),
    if
        IsCompacted =:= ToBeCompacted ->
            test_wsc(Tokens, Tokens2);
        true ->
            {compaction_error, Token}
    end.

is_compacted("\r") ->
    true;
is_compacted("\n\r") ->
    true;
is_compacted("\n\f") ->
    true;
is_compacted([$\n|String]) ->
      all_spaces(String)
    orelse
      all_tabs(String);
is_compacted(String) ->
      all_spaces(String)
    orelse
      all_tabs(String).

all_spaces(L) ->
    all_same(L, $\s).

all_tabs(L) ->
    all_same(L, $\t).

all_same(L, Char) ->
    lists:all(fun(C) -> C =:= Char end, L).

newlines_first([]) ->
    ok;
newlines_first([Token|Tokens]) ->
    Text = erl_scan:text(Token),
    Nnls = length([C || C <- Text, C =:= $\n]),
    OK = case Text of
             [$\n|_] ->
                 Nnls =:= 1;
             _ ->
                 Nnls =:= 0
         end,
    if
        OK -> newlines_first(Tokens);
        true -> OK
    end.

filter_tokens(Tokens, Tags) ->
    lists:filter(fun(T) -> not lists:member(element(1, T), Tags) end, Tokens).

select_tokens(Tokens, Tags) ->
    lists:filter(fun(T) -> lists:member(element(1, T), Tags) end, Tokens).

simplify([Token|Tokens]) ->
    Line = erl_scan:line(Token),
    [setelement(2, Token, erl_anno:new(Line)) | simplify(Tokens)];
simplify([]) ->
    [].

get_text(Tokens) ->
    lists:flatten(
      [T ||
          Token <- Tokens,
          (T = erl_scan:text(Token)) =/= []]).

test_decorated_tokens(String, Tokens) ->
    ToksAttrs = token_attrs(Tokens),
    test_strings(ToksAttrs, String, 1, 1).

token_attrs(Tokens) ->
    [{L,C,length(T),T} ||
        Token <- Tokens,
        ([C,L,T] = token_info(Token)) =/= []].

token_info(T) ->
    Column = erl_scan:column(T),
    Line = erl_scan:line(T),
    Text = erl_scan:text(T),
    [Column, Line, Text].

token_info_long(T) ->
    Column = erl_scan:column(T),
    Line = erl_scan:line(T),
    Text = erl_scan:text(T),
    Category = erl_scan:category(T),
    Symbol = erl_scan:symbol(T),
    [{category,Category},{column,Column},{line,Line},
     {symbol,Symbol},{text,Text}].

test_strings([], _S, Line, Column) ->
    {Line,Column};
test_strings([{L,C,Len,T}=Attr|Attrs], String0, Line0, Column0) ->
    {String1, Column1} = skip_newlines(String0, L, Line0, Column0),
    String = skip_chars(String1, C-Column1),
    {Str,Rest} = lists:split(Len, String),
    if
        Str =:= T ->
            {Line,Column} = string_newlines(T, L, C),
            test_strings(Attrs, Rest, Line, Column);
        true ->
            {token_error, Attr, Str}
    end.

skip_newlines(String, Line, Line, Column) ->
    {String, Column};
skip_newlines([$\n|String], L, Line, _Column) ->
    skip_newlines(String, L, Line+1, 1);
skip_newlines([_|String], L, Line, Column) ->
    skip_newlines(String, L, Line, Column+1).

skip_chars(String, 0) ->
    String;
skip_chars([_|String], N) ->
    skip_chars(String, N-1).

string_newlines([$\n|String], Line, _Column) ->
    string_newlines(String, Line+1, 1);
string_newlines([], Line, Column) ->
    {Line, Column};
string_newlines([_|String], Line, Column) ->
    string_newlines(String, Line, Column+1).

scan_string_with_column(String, Options0) ->
    Options = [text | Options0],
    StartLoc = {1, 1},
    {ok, Ts1, End1} = erl_scan:string(String, StartLoc, Options),
    TString = String ++ ". ",
    {ok,Ts2,End2} = scan_tokens(TString, Options, [], StartLoc),
    {ok, Ts3, End3} =
        scan_tokens_1({more, []}, TString, Options, [], StartLoc),
    {end_2,End2,End3} = {end_2,End3,End2},
    {EndLine1,EndColumn1} = End1,
    End2 = {EndLine1,EndColumn1+2},
    {ts_1,Ts2,Ts3} = {ts_1,Ts3,Ts2},
    Ts2 = Ts1 ++ [lists:last(Ts2)],

    %% Attributes are keylists, but have no text.
    {ok, Ts7, End7} = erl_scan:string(String, {1,1}, Options),
    {ok, Ts8, End8} = scan_tokens(TString, Options, [], {1,1}),
    {end1, End1} = {end1, End7},
    {end2, End2} = {end2, End8},
    Ts8 = Ts7 ++ [lists:last(Ts8)],
    {cons,true} = {cons,consistent_attributes([Ts1,Ts2,Ts3,Ts7,Ts8])},

    {Ts1, End1}.

scan_tokens(String, Options, Rs, Location) ->
    case erl_scan:tokens([], String, Location, Options) of
        {done, {ok,Ts,End}, ""} ->
            {ok, lists:append(lists:reverse([Ts|Rs])), End};
        {done, {ok,Ts,End}, Rest} ->
            scan_tokens(Rest, Options, [Ts|Rs], End)
    end.

scan_tokens_1({done, {ok,Ts,End}, ""}, "", _Options, Rs, _Location) ->
    {ok,lists:append(lists:reverse([Ts|Rs])),End};
scan_tokens_1({done, {ok,Ts,End}, Rest}, Cs, Options, Rs, _Location) ->
    scan_tokens_1({more,[]}, Rest++Cs, Options, [Ts|Rs], End);
scan_tokens_1({more, Cont}, [C | Cs], Options, Rs, Loc) ->
    R = erl_scan:tokens(Cont, [C], Loc, Options),
    scan_tokens_1(R, Cs, Options, Rs, Loc).

consistent_attributes([]) ->
    true;
consistent_attributes([Ts | TsL]) ->
    L = [T || T <- Ts, is_integer(element(2, T))],
    case L of
        [] ->
            TagsL = [[Tag || {Tag,_} <- defined(token_info_long(T))] ||
                        T <- Ts],
            case lists:usort(TagsL) of
                [_] ->
                    consistent_attributes(TsL);
                [] when Ts =:= [] ->
                    consistent_attributes(TsL);
                _ ->
                    Ts
            end;
        Ts ->
            consistent_attributes(TsL);
        _ ->
            Ts
    end.

defined(L) ->
    [{T,V} || {T,V} <- L, V =/= undefined].

family_list(L) ->
    sofs:to_external(family(L)).

family(L) ->
    sofs:relation_to_family(sofs:relation(L)).
