%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2017. All Rights Reserved.
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
-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([interpret/1, guards/1, interpretable/1]).
-export([ append_1/1, append_2/1, member/1, reverse/1]).

init_per_testcase(interpretable, Config) ->
    Config;
init_per_testcase(_Case, Config) ->

    %% Interpret some existing and non-existing modules
    DataDir = proplists:get_value(data_dir, Config),
    {module, lists1} = int:i(filename:join([DataDir,lists1])),
    {module, guards} = int:i(filename:join([DataDir,guards])),

    Config.

end_per_testcase(interpretable, _Config) ->
    ok;
end_per_testcase(_Case, Config) ->

    %% Quit interpreting
    ok = int:n(lists1),
    ok = int:n(guards),

    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [interpret, guards, {group, list_suite}, interpretable].

groups() -> 
    [{list_suite, [], [{group, append}, reverse, member]},
     {append, [], [append_1, append_2]}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    {ok,OldCwd} = file:get_cwd(),
    try
	ok = file:set_cwd(DataDir),
	make:all()
    after
	file:set_cwd(OldCwd)
    end,
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% Interpreting modules.
interpret(Config) when is_list(Config) ->
    int:n(int:interpreted()),

    %% Interpret some existing and non-existing modules
    DataDir = proplists:get_value(data_dir, Config),
    {module, lists1} = int:i(filename:join([DataDir,lists1])),
    {module, ordsets1} = int:i(filename:join([DataDir,ordsets1])),
    error = int:i(non_existent_module),

    %% Check that the interpreter has the right view.
    ExpectedResult = lists:sort([lists1, ordsets1]),
    Result = int:interpreted(),
    ExpectedResult = lists:sort(Result),

    %% Uniterpret the modules.
    ok = int:n(non_existent_module),
    ok = int:n(lists1),
    [ordsets1] = int:interpreted(),
    ok = int:n("ordsets1"),
    [] = int:interpreted(),

    ok.

%% Evaluate guards.
guards(Config) when is_list(Config) ->
    ok = guards:guards().

append_1(Config) when is_list(Config) ->
    io:format("In append_1~n"),
    io:format("code:which(lists1)=~p~n",
	      [code:which(lists1)]),
    io:format("lists1:append([a],[b])=~p~n",
	      [spawn_eval(lists1,append,[[a],[b]])]),

    "abcdef"=spawn_eval(lists1,append,[["abc","def"]]),
    [hej, du,[glade, [bagare]]]=
	spawn_eval(lists1,append,[[[hej], [du], [[glade, [bagare]]]]]),
    [10, [elem]]=spawn_eval(lists1,append,[[[10], [[elem]]]]),
    ok.

append_2(Config) when is_list(Config) ->
    io:format("In append_2~n"),
    io:format("code:which(lists1)=~p~n",
	      [code:which(lists1)]),

    "abcdef"=spawn_eval(lists1,append,["abc", "def"]),
    [hej, du]=spawn_eval(lists1,append,[[hej], [du]]),
    [10, [elem]]=spawn_eval(lists1,append,[[10], [[elem]]]),
    ok.

reverse(Config) when is_list(Config) ->
    ok=reverse_test(0),
    ok=reverse_test(1),
    ok=reverse_test(2),
    ok=reverse_test(537),
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

%% Tests the lists1:member() implementation. The function
%% is `non-blocking', and only processes 2000 elements
%% at a time.
%% This test case depends on lists1:reverse() to work,
%% which is tested in a separate test case.
member(Config) when list(Config) ->
    ok=member_test(0),
    ok=member_test(1),
    ok=member_test(100),
    ok=member_test(537),
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

%% Test int:interpretable/1.
interpretable(Config) when is_list(Config) ->

    %% First make sure that 'lists1' is not loaded
    case code:is_loaded(lists1) of
	{file, _Loaded} ->
	    code:purge(lists1),
	    code:delete(lists1),
	    code:purge(lists1);
	false -> ignore
    end,

    %% true
    DataDir = filename:dirname(proplists:get_value(data_dir, Config)),
    true = code:add_patha(DataDir),
    true = int:interpretable(lists1),
    true = int:interpretable(filename:join([DataDir,lists1])),
    true = code:del_path(DataDir),

    %% true (from source)
    PrivDir = filename:join(proplists:get_value(priv_dir, Config), ""),
    {ok, _} = file:copy(filename:join([DataDir,"lists1.beam"]),
			filename:join([PrivDir,"lists1.beam"])),
    true = code:add_patha(PrivDir),
    true = int:interpretable(lists1),
    ok = file:delete(filename:join([PrivDir,"lists1.beam"])),

    %% {error, no_beam}
    Src = filename:join([PrivDir,"lists1.erl"]),
    {ok, _} = file:copy(filename:join([DataDir,"lists1.erl"]),
			Src),
    {error, no_beam} = int:interpretable(Src),

    %% {error, no_debug_info}
    {ok, _} = compile:file(Src, [{outdir,PrivDir}]),
    {error, no_debug_info} = int:interpretable(Src),
    {error, no_debug_info} = int:interpretable(lists1),
    ok = file:delete(Src),
    true = code:del_path(PrivDir),

    %% {error, no_src}
    A1 = erl_anno:new(1),
    {ok, lists2, Binary} = compile:forms([{attribute,A1,module,lists2}], []),
    code:load_binary(lists2, "unknown", Binary),
    {error, no_src} = int:interpretable(lists2),

    %% {error, badarg}
    {error, badarg} = int:interpretable(pride),
    {error, badarg} = int:interpretable("prejudice.erl"),

    %% {error, {app,App}}
    case filename:basename(code:lib_dir(kernel)) of
	"kernel" ->
	    %% Development system (not installed). We are allowed
	    %% to interpret modules in kernel and stdlib
	    %% (at our own risk).
	    ok;
	"kernel-" ++ _ ->
	    %% Installed system. Certain applications (including
	    %% kernel and stdlib) cannot be interpreted.
	    {error, {app,_}} = int:interpretable(file),
	    {error, {app,_}} = int:interpretable(lists),
	    case int:interpretable(dbg_ieval) of
		{error, {app,_}} ->
		    ok;
		{error, badarg} ->
		    case code:which(dbg_ieval) of
			cover_compiled ->
			    ok;
			Other1 ->
			    ct:fail({unexpected_result, Other1})
		    end;
		Other2 ->
		    ct:fail({unexpected_result, Other2})
	    end
    end,
    ok.
