%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(edlin_expand_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([normal/1, quoted_fun/1, quoted_module/1, quoted_both/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

-include_lib("test_server/include/test_server.hrl").

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).

init_per_testcase(_Case, Config) ->
    ?line Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].
end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

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


normal(doc) ->
    [""];
normal(suite) ->
    [];
normal(Config) when is_list(Config) ->
    ?line {module,expand_test} = c:l(expand_test),
    % These tests might fail if another module with the prefix "expand_" happens
    % to also be loaded.
    ?line {yes, "test:", []} = edlin_expand:expand(lists:reverse("expand_")),
    ?line {no, [], []} = edlin_expand:expand(lists:reverse("expandXX_")),
    ?line {no,[],
	   [{"a_fun_name",1},
	    {"a_less_fun_name",1},
	    {"b_comes_after_a",1},
	    {"module_info",0},
	    {"module_info",1}]} = edlin_expand:expand(lists:reverse("expand_test:")),
    ?line {yes,[],[{"a_fun_name",1},
		   {"a_less_fun_name",1}]} = edlin_expand:expand(
					       lists:reverse("expand_test:a_")),
    ok.

quoted_fun(doc) ->
    ["Normal module name, some function names using quoted atoms"];
quoted_fun(suite) ->
    [];
quoted_fun(Config) when is_list(Config) ->
    ?line {module,expand_test} = c:l(expand_test),
    ?line {module,expand_test1} = c:l(expand_test1),
    %% should be no colon after test this time
    ?line {yes, "test", []} = edlin_expand:expand(lists:reverse("expand_")),
    ?line {no, [], []} = edlin_expand:expand(lists:reverse("expandXX_")),
    ?line {no,[],[{"'#weird-fun-name'",0},
		  {"'Quoted_fun_name'",0},
		  {"'Quoted_fun_too'",0},
		  {"a_fun_name",1},
		  {"a_less_fun_name",1},
		  {"b_comes_after_a",1},
		  {"module_info",0},
		  {"module_info",1}]} = edlin_expand:expand(
					  lists:reverse("expand_test1:")),
    ?line {yes,"_",[]} = edlin_expand:expand(
			   lists:reverse("expand_test1:a")),
    ?line {yes,[],[{"a_fun_name",1},
		   {"a_less_fun_name",1}]} = edlin_expand:expand(
					       lists:reverse("expand_test1:a_")),
    ?line {yes,[],
	   [{"'#weird-fun-name'",0},
	    {"'Quoted_fun_name'",0},
	    {"'Quoted_fun_too'",0}]} = edlin_expand:expand(
					 lists:reverse("expand_test1:'")),
    ?line {yes,"uoted_fun_",[]} = edlin_expand:expand(
				    lists:reverse("expand_test1:'Q")),
    ?line {yes,[],
	   [{"'Quoted_fun_name'",0},
	    {"'Quoted_fun_too'",0}]} = edlin_expand:expand(
					 lists:reverse("expand_test1:'Quoted_fun_")),
    ?line {yes,"weird-fun-name'(",[]} = edlin_expand:expand(
					  lists:reverse("expand_test1:'#")),
    ok.

quoted_module(doc) ->
    [""];
quoted_module(suite) ->
    [];
quoted_module(Config) when is_list(Config) ->
    ?line {module,'ExpandTestCaps'} = c:l('ExpandTestCaps'),
    ?line {yes, "Caps':", []} = edlin_expand:expand(lists:reverse("'ExpandTest")),
    ?line {no,[],
	   [{"a_fun_name",1},
	    {"a_less_fun_name",1},
	    {"b_comes_after_a",1},
	    {"module_info",0},
	    {"module_info",1}]} = edlin_expand:expand(lists:reverse("'ExpandTestCaps':")),
    ?line {yes,[],[{"a_fun_name",1},
		   {"a_less_fun_name",1}]} = edlin_expand:expand(
					       lists:reverse("'ExpandTestCaps':a_")),
    ok.

quoted_both(suite) ->
    [];
quoted_both(Config) when is_list(Config) ->
    ?line {module,'ExpandTestCaps'} = c:l('ExpandTestCaps'),
    ?line {module,'ExpandTestCaps1'} = c:l('ExpandTestCaps1'),
    %% should be no colon (or quote) after test this time
    ?line {yes, "Caps", []} = edlin_expand:expand(lists:reverse("'ExpandTest")),
    ?line {no,[],[{"'#weird-fun-name'",0},
		  {"'Quoted_fun_name'",0},
		  {"'Quoted_fun_too'",0},
		  {"a_fun_name",1},
		  {"a_less_fun_name",1},
		  {"b_comes_after_a",1},
		  {"module_info",0},
		  {"module_info",1}]} = edlin_expand:expand(
					  lists:reverse("'ExpandTestCaps1':")),
    ?line {yes,"_",[]} = edlin_expand:expand(
			   lists:reverse("'ExpandTestCaps1':a")),
    ?line {yes,[],[{"a_fun_name",1},
		   {"a_less_fun_name",1}]} = edlin_expand:expand(
					       lists:reverse("'ExpandTestCaps1':a_")),
    ?line {yes,[],
	   [{"'#weird-fun-name'",0},
	    {"'Quoted_fun_name'",0},
	    {"'Quoted_fun_too'",0}]} = edlin_expand:expand(
					 lists:reverse("'ExpandTestCaps1':'")),
    ?line {yes,"uoted_fun_",[]} = edlin_expand:expand(
				    lists:reverse("'ExpandTestCaps1':'Q")),
    ?line {yes,[],
	   [{"'Quoted_fun_name'",0},
	    {"'Quoted_fun_too'",0}]} = edlin_expand:expand(
					 lists:reverse("'ExpandTestCaps1':'Quoted_fun_")),
    ?line {yes,"weird-fun-name'(",[]} = edlin_expand:expand(
					  lists:reverse("'ExpandTestCaps1':'#")),
    ok.
