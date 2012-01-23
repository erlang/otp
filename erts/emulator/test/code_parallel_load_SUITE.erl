%% Copyright (C) 2012 Björn-Egil Dahlberg
%%
%% File:    code_parallel_load_SUITE.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2012-01-19
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
	parallel_load_check_purge_repeat/1
    ]).

-define(model,       my_model).
-define(interval,    1500).
-define(number_of_processes, 160).
-define(passes, 4).


-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [parallel_load_check_purge_repeat].


init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog=?t:timetrap(?t:minutes(3)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).


parallel_load_check_purge_repeat(_Conf) ->
    Ts    = [v1,v2,v3,v4,v5,v6],
    Codes = generate_codes(Ts),
    setup_code_changer(Codes),
    ok.

setup_code_changer([{Token,Code}|Cs] = Codes) ->
    {module, ?model} = erlang:load_module(?model,Code),
    Pids = setup_checkers(Token,?number_of_processes),
    code_changer(Cs, Codes, ?interval,Pids,?passes),
    ok.

code_changer(_, _, _, Pids, 0) ->
    [exit(Pid, normal) || Pid <- Pids],
    io:format("done~n"),
    ok;
code_changer([], Codes, T, Pids, Ps) ->
    code_changer(Codes, Codes, T, Pids, Ps - 1);
code_changer([{Token,Code}|Cs], Codes, T, Pids, Ps) ->
    receive after T ->
	    io:format("load code with token ~4w : pass ~4w~n", [Token, Ps]),
	    %code:load_binary(?model, ?model, Code),
	    {module, ?model} = erlang:load_module(?model, Code),
	    % this is second time we call load_module for this module
	    % so it should have old code
	    [Pid ! {self(), change, Token} || Pid <- Pids],
	    % should we wait a moment or just blantantly try to check and purge repeatadly?
	    receive after 1 -> ok end,
	    ok = check_and_purge_processes_code(Pids, ?model),
	    code_changer(Cs, Codes, T, Pids, Ps)
    end.

%% generate code that receives a token, code switches to new code
%% then matches this token against a literal code token
%% should be identical
%% (smoke test for parallel code loading
generate_codes([]) -> [];
generate_codes([T|Ts]) ->
    [{T, generate(?model, [], [
	    format("check(T) -> receive {_Pid, change, T1} -> "
		" ~w:check(T1)\n"
		" after 0 -> T = f(), check(T) end.\n", [?model]),
	    format("f() -> ~w.~n", [T])
	])} | generate_codes(Ts)].



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
