%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2010-2018. All Rights Reserved.
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

-module(rtt).
-compile([export_all, nowarn_export_all]).

%%  Modules or suites can be shortcuts, for example server expands to reltool_server_SUITE.
%%  
%%  t(Tests) run reltool testcases.
%%    Tests can be module, {module, test_case} or [module|{module,test_case}]

t() ->
    t(read_test_case()).
t(Test) ->
    t(Test, []).

t(Mod, TC) when is_atom(Mod), is_atom(TC) ->
    t({Mod,TC}, []);
t(all, Config) when is_list(Config) ->
    Fs = filelib:wildcard("reltool_*_SUITE.erl"),
    t([list_to_atom(filename:rootname(File)) || File <- Fs], Config);
t(Test,Config) when is_list(Config) ->
    Tests = resolve(Test),
    write_test_case(Test),
    Res = reltool_test_lib:run_test(Tests, Config),    
    append_test_case_info(Test, Res).

user() ->
    user(read_test_case()). 
user(Mod) ->
    t(Mod, [{user,step}]).
user(Mod,Tc) when is_atom(Tc) ->
    t({Mod,Tc}, [{user,step}]).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resolves the name of test suites and test cases
%% according to the alias definitions. Single atoms
%% are assumed to be the name of a test suite. 

resolve(Suite0) when is_atom(Suite0) ->
    case alias(Suite0) of
	Suite when is_atom(Suite) ->
	    {Suite, all};
	{Suite, Case} ->
	    {Suite, Case}
    end;
resolve({Suite0, Case}) when is_atom(Suite0), is_atom(Case) ->
    case alias(Suite0) of
	Suite when is_atom(Suite) ->
	    {Suite, Case};
	{Suite, Case2} ->
	    {Suite, Case2}
    end;
resolve(List) when is_list(List) ->
    [resolve(Case) || Case <- List].

alias(Suite) when is_atom(Suite) ->
    Str = atom_to_list(Suite),
    case {Str, lists:reverse(Str)} of
	{"reltool" ++ _, "ETIUS" ++ _} ->
	    Suite;
	_ -> 
	    list_to_atom("reltool_" ++ Str ++ "_SUITE")
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

config_fname() ->
    "reltool_test_case_config".

%% Read default config file
read_config() ->
    Fname = config_fname(),
    reltool_test_lib:log("Consulting file ~s...~n", [Fname]),
    case file:consult(Fname) of
        {ok, Config} ->
	    reltool_test_lib:log("Read config ~w~n", [Config]),
            Config;
        _Error ->
	    Config = reltool_test_lib:default_config(),
            reltool_test_lib:log("<>WARNING<> Using default config: ~w~n", [Config]),
            Config
    end.

%% Write new default config file
write_config(Config) when is_list(Config) ->
    Fname = config_fname(),
    {ok, Fd} = file:open(Fname, write),
    write_list(Fd, Config),
    file:close(Fd).

write_list(Fd, [H | T]) ->
    ok = io:format(Fd, "~p.~n",[H]),
    write_list(Fd, T);
write_list(_, []) ->
    ok.

test_case_fname() ->
    "reltool_test_case_info".

%% Read name of test case
read_test_case() ->
    Fname = test_case_fname(),
    case file:open(Fname, [read]) of
	{ok, Fd} ->
	    Res = io:read(Fd, []),
	    file:close(Fd),
	    case Res of
		{ok, TestCase} ->
		    reltool_test_lib:log("Using test case ~w from file ~s~n",
					 [TestCase, Fname]),
		    TestCase;
		{error, _} ->
		    default_test_case(Fname)
	    end;
	{error, _} ->
	    default_test_case(Fname)
    end.

default_test_case(Fname) ->
    TestCase = all, 
    reltool_test_lib:log("<>WARNING<> Cannot read file ~s, "
			 "using default test case: ~w~n",
			 [Fname, TestCase]),
    TestCase.

write_test_case(TestCase) ->
    Fname = test_case_fname(),
    {ok, Fd} = file:open(Fname, write),
    ok = io:format(Fd, "~p.~n",[TestCase]),
    file:close(Fd).

append_test_case_info(TestCase, TestCaseInfo) ->
    Fname = test_case_fname(),
    {ok, Fd} = file:open(Fname, [read, write]),
    ok = io:format(Fd, "~p.~n",[TestCase]),
    ok = io:format(Fd, "~p.~n",[TestCaseInfo]),
    file:close(Fd),
    TestCaseInfo.
