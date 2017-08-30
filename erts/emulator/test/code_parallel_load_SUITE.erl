%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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

-module(code_parallel_load_SUITE).
-export([all/0,
         suite/0,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([multiple_load_check_purge_repeat/1,
         many_load_distributed_only_once/1]).

-define(model,       code_parallel_load_SUITE_model).
-define(interval,    50).
-define(number_of_processes, 160).
-define(passes, 4).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 4}}].

all() ->
    [ multiple_load_check_purge_repeat,
      many_load_distributed_only_once ].

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Config.

end_per_testcase(_Func, Config) ->
    SConf = proplists:get_value(save_config, Config),
    Pids  = proplists:get_value(purge_pids, SConf),

    case check_old_code(?model) of
	true -> check_and_purge_processes_code(Pids, ?model);
	_ ->    ok
    end,
    case erlang:delete_module(?model) of
	true -> check_and_purge_processes_code(Pids, ?model);
	_ ->    ok
    end,
    ok.

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
    Tag = make_ref(),
    N = request_cpc(Pids, M, Tag),
    ok = handle_cpc_responses(N, Tag, M),
    erlang:purge_module(M),
    ok.

request_cpc(Pid, M, Tag) when is_pid(Pid) ->
    erlang:check_process_code(Pid, M, [{async, {Tag, Pid}}]),
    1;
request_cpc(Pids, M, Tag) when is_list(Pids) ->
    request_cpc(Pids, M, Tag, 0).

request_cpc([], _M, _Tag, N) ->
    N;
request_cpc([Pid|Pids], M, Tag, N) ->
    request_cpc(Pids, M, Tag, N + request_cpc(Pid, M, Tag)).

handle_cpc_responses(0, _Tag, _Module) ->
    ok;
handle_cpc_responses(N, Tag, Module) ->
    receive
	{check_process_code, {Tag, _Pid}, false} ->
	    handle_cpc_responses(N-1, Tag, Module);
	{check_process_code, {Tag, Pid}, true} ->
	    1 = request_cpc(Pid, Module, Tag),
	    handle_cpc_responses(N, Tag, Module)
    end.

generate(Module, Attributes, FunStrings) ->
    FunForms = function_forms(FunStrings),
    Forms    = [
	{attribute,a(1),module,Module},
	{attribute,a(2),export,[FA || {FA,_} <- FunForms]}
    ] ++ [{attribute, a(3), A, V}|| {A, V} <- Attributes] ++
    [ Function || {_, Function} <- FunForms],
    {ok, Module, Bin} = compile:forms(Forms),
    Bin.

a(L) ->
    erl_anno:new(L).

function_forms([]) -> [];
function_forms([S|Ss]) ->
    {ok, Ts,_} = erl_scan:string(S),
    {ok, Form} = erl_parse:parse_form(Ts),
    Fun   = element(3, Form),
    Arity = element(4, Form),
    [{{Fun,Arity}, Form}|function_forms(Ss)].

format(F,Ts) -> lists:flatten(io_lib:format(F, Ts)).
