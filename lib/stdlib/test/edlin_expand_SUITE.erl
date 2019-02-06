%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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
-module(edlin_expand_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_testcase/2, end_per_testcase/2,
	 init_per_group/2,end_per_group/2]).
-export([normal/1, quoted_fun/1, quoted_module/1, quoted_both/1, erl_1152/1,
         erl_352/1, unicode/1]).

-include_lib("common_test/include/ct.hrl").

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [normal, quoted_fun, quoted_module, quoted_both, erl_1152, erl_352,
     unicode].

groups() -> 
    [].

init_per_suite(Config) ->
    (catch code:delete(expand_test)),
    (catch code:delete(expand_test1)),
    (catch code:delete('ExpandTestCaps')),
    (catch code:delete('ExpandTestCaps1')),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


normal(Config) when is_list(Config) ->
    {module,expand_test} = c:l(expand_test),
    %% These tests might fail if another module with the prefix
    %% "expand_" happens to also be loaded.
    {yes, "test:", []} = do_expand("expand_"),
    {no, [], []} = do_expand("expandXX_"),
    {no,[],
     [{"a_fun_name",1},
      {"a_less_fun_name",1},
      {"b_comes_after_a",1},
      {"expand0arity_entirely",0},
      {"module_info",0},
      {"module_info",1}]} = do_expand("expand_test:"),
    {yes,[],[{"a_fun_name",1},
	     {"a_less_fun_name",1}]} = do_expand("expand_test:a_"),
    {yes,"arity_entirely()",[]} = do_expand("expand_test:expand0"),
    ok.

%% Normal module name, some function names using quoted atoms.
quoted_fun(Config) when is_list(Config) ->
    {module,expand_test} = c:l(expand_test),
    {module,expand_test1} = c:l(expand_test1),
    %% should be no colon after test this time
    {yes, "test", []} = do_expand("expand_"),
    {no, [], []} = do_expand("expandXX_"),
    {no,[],[{"'#weird-fun-name'",1},
	    {"'Quoted_fun_name'",0},
	    {"'Quoted_fun_too'",0},
	    {"a_fun_name",1},
	    {"a_less_fun_name",1},
	    {"b_comes_after_a",1},
	    {"module_info",0},
	    {"module_info",1}]} = do_expand("expand_test1:"),
    {yes,"_",[]} = do_expand("expand_test1:a"),
    {yes,[],[{"a_fun_name",1},
	     {"a_less_fun_name",1}]} = do_expand("expand_test1:a_"),
    {yes,[],
     [{"'#weird-fun-name'",1},
      {"'Quoted_fun_name'",0},
      {"'Quoted_fun_too'",0}]} = do_expand("expand_test1:'"),
    {yes,"uoted_fun_",[]} = do_expand("expand_test1:'Q"),
    {yes,[],
     [{"'Quoted_fun_name'",0},
      {"'Quoted_fun_too'",0}]} = do_expand("expand_test1:'Quoted_fun_"),
    {yes,"weird-fun-name'(",[]} = do_expand("expand_test1:'#"),

    %% Since there is a module_info/1 as well as a module_info/0
    %% there should not be a closing parenthesis added.
    {yes,"(",[]} = do_expand("expand_test:module_info"),
    ok.

quoted_module(Config) when is_list(Config) ->
    {module,'ExpandTestCaps'} = c:l('ExpandTestCaps'),
    {yes, "Caps':", []} = do_expand("'ExpandTest"),
    {no,[],
     [{"a_fun_name",1},
      {"a_less_fun_name",1},
      {"b_comes_after_a",1},
      {"module_info",0},
      {"module_info",1}]} = do_expand("'ExpandTestCaps':"),
    {yes,[],[{"a_fun_name",1},
	     {"a_less_fun_name",1}]} = do_expand("'ExpandTestCaps':a_"),
    ok.

quoted_both(Config) when is_list(Config) ->
    {module,'ExpandTestCaps'} = c:l('ExpandTestCaps'),
    {module,'ExpandTestCaps1'} = c:l('ExpandTestCaps1'),
    %% should be no colon (or quote) after test this time
    {yes, "Caps", []} = do_expand("'ExpandTest"),
    {no,[],[{"'#weird-fun-name'",0},
	    {"'Quoted_fun_name'",0},
	    {"'Quoted_fun_too'",0},
	    {"a_fun_name",1},
	    {"a_less_fun_name",1},
	    {"b_comes_after_a",1},
	    {"module_info",0},
	    {"module_info",1}]} = do_expand("'ExpandTestCaps1':"),
    {yes,"_",[]} = do_expand("'ExpandTestCaps1':a"),
    {yes,[],[{"a_fun_name",1},
	     {"a_less_fun_name",1}]} = do_expand("'ExpandTestCaps1':a_"),
    {yes,[],
     [{"'#weird-fun-name'",0},
      {"'Quoted_fun_name'",0},
      {"'Quoted_fun_too'",0}]} = do_expand("'ExpandTestCaps1':'"),
    {yes,"uoted_fun_",[]} = do_expand("'ExpandTestCaps1':'Q"),
    {yes,[],
     [{"'Quoted_fun_name'",0},
      {"'Quoted_fun_too'",0}]} = do_expand("'ExpandTestCaps1':'Quoted_fun_"),
    {yes,"weird-fun-name'()",[]} = do_expand("'ExpandTestCaps1':'#"),
    ok.

%% Note: pull request #1152.
erl_1152(Config) when is_list(Config) ->
    "\n"++"foo"++"    "++[1089]++_ = do_format(["foo",[1089]]),
    ok.

erl_352(Config) when is_list(Config) ->
    erl_352_test(3, 3),

    erl_352_test(3, 75),
    erl_352_test(3, 76, [trailing]),
    erl_352_test(4, 74),
    erl_352_test(4, 75, [leading]),
    erl_352_test(4, 76, [leading, trailing]),

    erl_352_test(75, 3),
    erl_352_test(76, 3, [leading]),
    erl_352_test(74, 4),
    erl_352_test(75, 4, [leading]),
    erl_352_test(76, 4, [leading]),

    erl_352_test(74, 74, [leading]),
    erl_352_test(74, 75, [leading]),
    erl_352_test(74, 76, [leading, trailing]).

erl_352_test(PrefixLen, SuffixLen) ->
    erl_352_test(PrefixLen, SuffixLen, []).

erl_352_test(PrefixLen, SuffixLen, Dots) ->
    io:format("\nPrefixLen = ~w, SuffixLen = ~w\n", [PrefixLen, SuffixLen]),

    PrefixM = lists:duplicate(PrefixLen, $p),
    SuffixM = lists:duplicate(SuffixLen, $s),
    LM = [PrefixM ++ S ++ SuffixM || S <- ["1", "2"]],
    StrM = do_format(LM),
    check_leading(StrM, "", PrefixM, SuffixM, Dots),

    PrefixF = lists:duplicate(PrefixLen, $p),
    SuffixF = lists:duplicate(SuffixLen-2, $s),
    LF = [{PrefixF ++ S ++ SuffixF, 1} || S <- ["1", "2"]],
    StrF = do_format(LF),
    true = check_leading(StrF, "/1", PrefixF, SuffixF, Dots),

    ok.

check_leading(FormStr, ArityStr, Prefix, Suffix, Dots) ->
    List = string:tokens(FormStr, "\n "),
    io:format("~p\n", [List]),
    true = lists:all(fun(L) -> length(L) < 80 end, List),
    case lists:member(leading, Dots) of
        true ->
            true = lists:all(fun(L) ->
                                     {"...", Rest} = lists:split(3, L),
                                     check_trailing(Rest, ArityStr,
                                                    Suffix, Dots)
                             end, List);
        false ->
            true = lists:all(fun(L) ->
                                     {Prefix, Rest} =
                                         lists:split(length(Prefix), L),
                                     check_trailing(Rest, ArityStr,
                                                    Suffix, Dots)
                             end, List)
    end.

check_trailing([I|Str], ArityStr, Suffix, Dots) ->
    true = lists:member(I, [$1, $2]),
    case lists:member(trailing, Dots) of
        true ->
            {Rest, "..." ++ ArityStr} =
                lists:split(length(Str) - (3 + length(ArityStr)), Str),
            true = lists:prefix(Rest, Suffix);
        false ->
            {Rest, ArityStr} =
                lists:split(length(Str) - length(ArityStr), Str),
            Rest =:= Suffix
    end.

unicode(Config) when is_list(Config) ->
    {module,unicode_expand} = c:l('unicode_expand'),
    {no,[],[{"'кlирилли́ческий атом'",0},
            {"'кlирилли́ческий атом'",1},
            {"'кlирилли́ческий атомB'",1},
            {"module_info",0},
            {"module_info",1}]} = do_expand("unicode_expand:"),
    {yes,"рилли́ческий атом", []} = do_expand("unicode_expand:'кlи"),
    {yes,"еский атом", []} = do_expand("unicode_expand:'кlирилли́ч"),
    {yes,"(",[]} = do_expand("unicode_expand:'кlирилли́ческий атомB'"),
    "\n'кlирилли́ческий атом'/0   'кlирилли́ческий атом'/1   "
    "'кlирилли́ческий атомB'/1  \nmodule_info/0             "
    "module_info/1             \n" =
        do_format([{"'кlирилли́ческий атом'",0},
                   {"'кlирилли́ческий атом'",1},
                   {"'кlирилли́ческий атомB'",1},
                   {"module_info",0},
                   {"module_info",1}]),
    ok.

do_expand(String) ->
    edlin_expand:expand(lists:reverse(String)).

do_format(StringList) ->
    lists:flatten(edlin_expand:format_matches(StringList)).
