%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
-export([normal/1, quoted_fun/1, quoted_module/1, quoted_both/1]).

-include_lib("common_test/include/ct.hrl").

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [normal, quoted_fun, quoted_module, quoted_both].

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

do_expand(String) ->
    edlin_expand:expand(lists:reverse(String)).
