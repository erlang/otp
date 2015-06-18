%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2013. All Rights Reserved.
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

%%
-module(int_SUITE).
-include_lib("test_server/include/test_server.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([interpret/1, guards/1, interpretable/1]).
-export([ append_1/1, append_2/1, member/1, reverse/1]).

%% Default timetrap timeout (set in init_per_testcase)
-define(default_timeout, ?t:minutes(1)).

init_per_testcase(interpretable, Config) ->
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config];
init_per_testcase(_Case, Config) ->

    %% Interpret some existing and non-existing modules
    ?line DataDir = ?config(data_dir, Config),
    ?line {module, lists1} = int:i(filename:join([DataDir,lists1])),
    ?line {module, guards} = int:i(filename:join([DataDir,guards])),

    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

end_per_testcase(interpretable, Config) ->
    ?line Dog=?config(watchdog, Config),
    ?line test_server:timetrap_cancel(Dog),
    ok;
end_per_testcase(_Case, Config) ->

    %% Quit interpreting
    ?line ok = int:n(lists1),
    ?line ok = int:n(guards),

    ?line Dog=?config(watchdog, Config),
    ?line test_server:timetrap_cancel(Dog),
    ?line ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [interpret, guards, {group, list_suite}, interpretable].

groups() -> 
    [{list_suite, [], [{group, append}, reverse, member]},
     {append, [], [append_1, append_2]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


interpret(suite) ->
    [];
interpret(doc) ->
    ["Interpreting modules"];
interpret(Config) when is_list(Config) ->
    ?line int:n(int:interpreted()),

    %% Interpret some existing and non-existing modules
    ?line DataDir = ?config(data_dir, Config),
    ?line {module, lists1} = int:i(filename:join([DataDir,lists1])),
    ?line {module, ordsets1} = int:i(filename:join([DataDir,ordsets1])),
    ?line error = int:i(non_existent_module),

    %% Check that the interpreter has the right view.
    ?line ExpectedResult = lists:sort([lists1, ordsets1]),
    ?line Result = int:interpreted(),
    ?line ExpectedResult = lists:sort(Result),

    %% Uniterpret the modules.
    ?line ok = int:n(non_existent_module),
    ?line ok = int:n(lists1),
    ?line [ordsets1] = int:interpreted(),
    ?line ok = int:n("ordsets1"),
    ?line [] = int:interpreted(),

    ok.

guards(suite) ->
    [];
guards(doc) ->
    "Evaluate guards.";
guards(Config) when is_list(Config) ->
    ok = guards:guards().




append_1(suite) ->
    [];
append_1(doc) ->
    [];
append_1(Config) when is_list(Config) ->
    ?line test_server:format("In append_1~n"),
    ?line test_server:format("code:which(lists1)=~p~n",
			     [code:which(lists1)]),
    ?line test_server:format("lists1:append([a],[b])=~p~n",
			     [spawn_eval(lists1,append,[[a],[b]])]),

    ?line "abcdef"=spawn_eval(lists1,append,[["abc","def"]]),
    ?line [hej, du,[glade, [bagare]]]=
	spawn_eval(lists1,append,[[[hej], [du], [[glade, [bagare]]]]]),
    ?line [10, [elem]]=spawn_eval(lists1,append,[[[10], [[elem]]]]),
    ok.

append_2(suite) ->
    [];
append_2(doc) ->
    [];
append_2(Config) when is_list(Config) ->
    ?line test_server:format("In append_2~n"),
    ?line test_server:format("code:which(lists1)=~p~n",
			     [code:which(lists1)]),

    ?line "abcdef"=spawn_eval(lists1,append,["abc", "def"]),
    ?line [hej, du]=spawn_eval(lists1,append,[[hej], [du]]),
    ?line [10, [elem]]=spawn_eval(lists1,append,[[10], [[elem]]]),
    ok.

reverse(suite) ->
    [];
reverse(doc) ->
    [];
reverse(Config) when is_list(Config) ->
    ?line ok=reverse_test(0),
    ?line ok=reverse_test(1),
    ?line ok=reverse_test(2),
    ?line ok=reverse_test(537),
    ok.

reverse_test(0) ->
    case spawn_eval(lists1,reverse,[[]]) of
	[] ->
	    ok;
	_Other ->
	    error
    end;
reverse_test(Num) ->
    List=spawn_eval(lists1,reverse,
		    [['The Element'|lists1:duplicate(Num, 'Ele')]]),
    case spawn_eval(lists1,reverse,[List]) of
	['The Element'|_Rest] ->
	    ok;
	_Other ->
	    error
    end.

member(suite) ->
    [];
member(doc) ->
    ["Tests the lists1:member() implementation. The function "
     "is `non-blocking', and only processes 2000 elements "
     "at a time.",
     "This test case depends on lists1:reverse() to work, "
     "wich is tested in a separate test case."];
member(Config) when list(Config) ->
    ?line ok=member_test(0),
    ?line ok=member_test(1),
    ?line ok=member_test(100),
    ?line ok=member_test(537),
    ok.

member_test(0) ->
    case spawn_eval(lists1,member,['The Element', []]) of
	false ->
	    ok;
	true ->
	    {error, 'Found (!?)'}
    end;
member_test(Num) ->
    List=spawn_eval(lists1,reverse,
		    [['The Element'|spawn_eval(lists1,duplicate,
					       [Num, 'Elem'])]]),
    case spawn_eval(lists1,member,['The Element', List]) of
	true ->
	    ok;
	false ->
	    {error, not_found}
    end.

spawn_eval(M,F,A) ->
    Self = self(),
    spawn(fun() -> evaluator(Self, M,F,A) end),
    receive
	Result ->
	    Result
    end.

evaluator(Pid, M,F,A) ->
    Pid ! (catch apply(M,F,A)).

interpretable(suite) ->
    [];
interpretable(doc) ->
    ["Test int:interpretable/1"];
interpretable(Config) when is_list(Config) ->

    %% First make sure that 'lists1' is not loaded
    case code:is_loaded(lists1) of
	{file, _Loaded} ->
	    ?line code:purge(lists1),
	    ?line code:delete(lists1),
	    ?line code:purge(lists1);
	false -> ignore
    end,

    %% true
    ?line DataDir = filename:dirname(?config(data_dir, Config)),
    ?line true = code:add_patha(DataDir),
    ?line true = int:interpretable(lists1),
    ?line true = int:interpretable(filename:join([DataDir,lists1])),
    ?line true = code:del_path(DataDir),

    %% {error, no_src}
    ?line PrivDir = filename:join(?config(priv_dir, Config), ""),
    ?line {ok, _} = file:copy(filename:join([DataDir,"lists1.beam"]),
			      filename:join([PrivDir,"lists1.beam"])),
    ?line true = code:add_patha(PrivDir),

    ?line {error, no_src} = int:interpretable(lists1),
    ?line ok = file:delete(filename:join([PrivDir,"lists1.beam"])),

    %% {error, no_beam}
    Src = filename:join([PrivDir,"lists1.erl"]),
    ?line {ok, _} = file:copy(filename:join([DataDir,"lists1.erl"]),
			      Src),
    ?line {error, no_beam} = int:interpretable(Src),

    %% {error, no_debug_info}
    ?line {ok, _} = compile:file(Src, [{outdir,PrivDir}]),
    ?line {error, no_debug_info} = int:interpretable(Src),
    ?line {error, no_debug_info} = int:interpretable(lists1),
    ?line ok = file:delete(Src),
    ?line true = code:del_path(PrivDir),

    %% {error, badarg}
    ?line {error, badarg} = int:interpretable(pride),
    ?line {error, badarg} = int:interpretable("prejudice.erl"),

    %% {error, {app,App}}
    ?line {error, {app,_}} = int:interpretable(file),
    ?line {error, {app,_}} = int:interpretable(lists),
    ?line case int:interpretable(dbg_ieval) of
	      {error, {app,_}} ->
		  ok;
	      {error, badarg} ->
		  case code:which(dbg_ieval) of
		      cover_compiled ->
			  ok;
		      Other1 ->
			  ?line ?t:fail({unexpected_result, Other1})
		  end;
	      Other2 ->
		  ?line ?t:fail({unexpected_result, Other2})
	  end,

    ok.
