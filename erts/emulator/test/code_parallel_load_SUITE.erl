%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2013. All Rights Reserved.
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

-module(code_parallel_load_SUITE).
-export([
	all/0,
	suite/0,
	init_per_suite/1,
	end_per_suite/1,
	init_per_testcase/2,
	end_per_testcase/2
    ]).

-export([
	multiple_load_check_purge_repeat/1,
	many_load_distributed_only_once/1
    ]).

-define(model,       code_parallel_load_SUITE_model).
-define(interval,    50).
-define(number_of_processes, 160).
-define(passes, 4).


-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [
	multiple_load_check_purge_repeat,
	many_load_distributed_only_once
    ].


init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog=?t:timetrap(?t:minutes(3)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    SConf = ?config(save_config, Config),
    Pids  = proplists:get_value(purge_pids, SConf),

    case check_old_code(?model) of
	true -> check_and_purge_processes_code(Pids, ?model);
	_ ->    ok
    end,
    case erlang:delete_module(?model) of
	true -> check_and_purge_processes_code(Pids, ?model);
	_ ->    ok
    end,
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).


multiple_load_check_purge_repeat(_Conf) ->
    Ts    = [v1,v2,v3,v4,v5,v6],

    %% generate code that receives a token, code switches to new code
    %% then matches this token against a literal code token
    %% should be identical
    %% (smoke test for parallel code loading
    Codes = [{T, generate(?model, [], [
	    format("check(T) -> receive {_Pid, change, T1} -> "
		" ~w:check(T1)\n"
		" after 0 -> T = f(), check(T) end.\n", [?model]),
	    format("f() -> ~w.~n", [T])
	])} || T <- Ts],

    Pids = setup_code_changer(Codes),
    {save_config, [{purge_pids,Pids}]}.

setup_code_changer([{Token,Code}|Cs] = Codes) ->
    {module, ?model} = erlang:load_module(?model,Code),
    Pids = setup_checkers(Token,?number_of_processes),
    code_changer(Cs, Codes, ?interval,Pids,?passes),
    Pids.

code_changer(_, _, _, Pids, 0) ->
    [unlink(Pid) || Pid <- Pids],
    [exit(Pid, die) || Pid <- Pids],
    io:format("done~n"),
    ok;
code_changer([], Codes, T, Pids, Ps) ->
    code_changer(Codes, Codes, T, Pids, Ps - 1);
code_changer([{Token,Code}|Cs], Codes, T, Pids, Ps) ->
    receive after T ->
	    io:format("load code with token ~4w : pass ~4w~n", [Token, Ps]),
	    {module, ?model} = erlang:load_module(?model, Code),
	    % this is second time we call load_module for this module
	    % so it should have old code
	    [Pid ! {self(), change, Token} || Pid <- Pids],
	    % should we wait a moment or just blantantly try to check and purge repeatadly?
	    receive after 1 -> ok end,
	    ok = check_and_purge_processes_code(Pids, ?model),
	    code_changer(Cs, Codes, T, Pids, Ps)
    end.



many_load_distributed_only_once(_Conf) ->
    Ts = [<<"first version">>, <<"second version">>],

    [{Token1,Code1},{Token2, Code2}] = [{T, generate(?model, [], [
	    "check({<<\"second version\">> = V, Pid}) -> V = f(), Pid ! {self(), completed, V}, ok;\n" ++
	    format("check(T) -> receive {Pid, change, T1, B} -> "
		" Res = erlang:load_module(~w, B), Pid ! {self(), change, Res},\n"
		" ~w:check({T1, Pid})\n"
		" after 0 -> T = f(), check(T) end.\n", [?model, ?model]),
	    format("f() -> ~w.~n", [T])
	])} || T <- Ts],


    {module, ?model} = erlang:load_module(?model, Code1),
    Pids = setup_checkers(Token1,?number_of_processes),

    receive after 1000 -> ok end, % give 'em some time to spin up
    [Pid ! {self(), change, Token2, Code2} || Pid <- Pids],
    Loads = [receive {Pid, change, Res} -> Res end || Pid <- Pids],
    [receive {Pid, completed, Token2} -> ok end || Pid <- Pids],

    ok = ensure_only_one_load(Loads, 0),
    {save_config, [{purge_pids,Pids}]}.

ensure_only_one_load([], 1) -> ok;
ensure_only_one_load([], _) -> too_many_loads;
ensure_only_one_load([{module, ?model}|Loads], N) ->
    ensure_only_one_load(Loads, N + 1);
ensure_only_one_load([{error, not_purged}|Loads], N) ->
    ensure_only_one_load(Loads, N).
% no other return values are allowed from load_module


%% aux

setup_checkers(_,0) -> [];
setup_checkers(T,N) -> [spawn_link(fun() -> ?model:check(T) end) | setup_checkers(T, N-1)].

check_and_purge_processes_code(Pids, M) ->
    check_and_purge_processes_code(Pids, M, []).
check_and_purge_processes_code([], M, []) ->
    erlang:purge_module(M),
    ok;
check_and_purge_processes_code([], M, Pending) ->
    io:format("Processes ~w are still executing old code - retrying.~n", [Pending]),
    check_and_purge_processes_code(Pending, M, []);
check_and_purge_processes_code([Pid|Pids], M, Pending) ->
    case erlang:check_process_code(Pid, M) of
	false ->
	    check_and_purge_processes_code(Pids, M, Pending);
	true ->
	    check_and_purge_processes_code(Pids, M, [Pid|Pending])
    end.


generate(Module, Attributes, FunStrings) ->
    FunForms = function_forms(FunStrings),
    Forms    = [
	{attribute,1,module,Module},
	{attribute,2,export,[FA || {FA,_} <- FunForms]}
    ] ++ [{attribute, 3, A, V}|| {A, V} <- Attributes] ++
    [ Function || {_, Function} <- FunForms],
    {ok, Module, Bin} = compile:forms(Forms),
    Bin.


function_forms([]) -> [];
function_forms([S|Ss]) ->
    {ok, Ts,_} = erl_scan:string(S),
    {ok, Form} = erl_parse:parse_form(Ts),
    Fun   = element(3, Form),
    Arity = element(4, Form),
    [{{Fun,Arity}, Form}|function_forms(Ss)].

format(F,Ts) -> lists:flatten(io_lib:format(F, Ts)).
