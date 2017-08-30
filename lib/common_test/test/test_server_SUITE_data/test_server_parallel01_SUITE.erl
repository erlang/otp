%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

%%%------------------------------------------------------------------
%%% Test Server self test. 
%%%------------------------------------------------------------------
-module(test_server_parallel01_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%% -------------------------------------------------------------------
%% Notes on parallel execution of test cases
%% -------------------------------------------------------------------
%%
%% A group nested under a parallel group will start executing in
%% parallel with previous (parallel) test cases (no matter what
%% properties the nested group has). Test cases are however never 
%% executed in parallel with the start or end conf case of the same
%% group! Because of this, the test_server_ctrl loop waits at
%% the end conf of a group for all parallel cases to finish
%% before the end conf case actually executes. This has the effect
%% that it's only after a nested group has finished that any
%% remaining parallel cases in the previous group get spawned (*). 
%% Example (all parallel cases):
%%
%% group1_init   |---->
%% group1_case1        | --------->
%% group1_case2        | --------------------------------->
%% group2_init         | ---->
%% group2_case1               | ------>
%% group2_case2               | ---------->
%% group2_end                              | --->
%% group1_case3                               (*)| ---->
%% group1_case4                               (*)| -->
%% group1_end                                              | --->
%%

all(doc) -> ["Test simple conf case structure, with and without nested cases"];
all(suite) -> 
    [
     {conf, [parallel], conf1_init, [conf1_tc1, conf1_tc2], conf1_end},

     {conf, [parallel], conf2_init, [conf2_tc1, conf2_tc2], conf2_end},

     {conf, [parallel], conf3_init, [conf3_tc1, conf3_tc1,
				     
				     {conf, [], 
				      conf4_init, [conf4_tc1, conf4_tc2], conf4_end},
				     
				     conf3_tc2], conf3_end},

     conf5,

     {conf, [parallel], conf7_init, [conf7_tc1, conf7_tc1,
				     
				     {conf, [parallel], 
				      conf8_init, [conf8_tc1, conf8_tc2], conf8_end},
				     
				     conf7_tc2], conf7_end}
     
    ].


%%---------- conf cases ----------

init_per_suite(Config) ->
    case ?config(data_dir,Config) of
	undefined -> exit(no_data_dir);
	_ -> ok
    end,    
    [{suite,init}|Config].
end_per_suite(Config) ->
    case ?config(data_dir,Config) of
	undefined -> exit(no_data_dir);
	_ -> ok
    end,    
    init = ?config(suite,Config),
    ok.

init_per_testcase(TC=conf1_tc1, Config) ->
    init = ?config(suite,Config),
    [{tc11,TC}|Config];
init_per_testcase(TC=conf1_tc2, Config) ->
    [{tc12,TC}|Config];
init_per_testcase(TC=conf2_tc1, Config) ->
    [{tc21,TC}|Config];
init_per_testcase(TC=conf2_tc2, Config) ->
    [{tc22,TC}|Config];
init_per_testcase(TC=conf3_tc1, Config) ->
    [{tc31,TC}|Config];
init_per_testcase(TC=conf3_tc2, Config) ->
    [{tc32,TC}|Config];
init_per_testcase(TC=conf4_tc1, Config) ->
    [{tc41,TC}|Config];
init_per_testcase(TC=conf4_tc2, Config) ->
    [{tc42,TC}|Config];
init_per_testcase(TC=conf5_tc1, Config) ->
    [{tc51,TC}|Config];
init_per_testcase(TC=conf5_tc2, Config) ->
    [{tc52,TC}|Config];
init_per_testcase(TC=conf6_tc1, Config) ->
    [{tc61,TC}|Config];
init_per_testcase(TC=conf6_tc2, Config) ->
    init = ?config(suite,Config),
    [{tc62,TC}|Config];
init_per_testcase(TC=conf7_tc1, Config) ->
    [{tc71,TC}|Config];
init_per_testcase(TC=conf7_tc2, Config) ->
    [{tc72,TC}|Config];
init_per_testcase(TC=conf8_tc1, Config) ->
    [{tc81,TC}|Config];
init_per_testcase(TC=conf8_tc2, Config) ->
    init = ?config(suite,Config),
    [{tc82,TC}|Config].

end_per_testcase(TC=conf1_tc1, Config) ->
    init = ?config(suite,Config),
    TC = ?config(tc11,Config),
    ok;
end_per_testcase(TC=conf1_tc2, Config) ->
    TC = ?config(tc12,Config),
    ok;
end_per_testcase(TC=conf2_tc1, Config) ->
    TC = ?config(tc21,Config),
    ok;
end_per_testcase(TC=conf2_tc2, Config) ->
    TC = ?config(tc22,Config),
    ok;
end_per_testcase(TC=conf3_tc1, Config) ->
    TC = ?config(tc31,Config),
    ok;
end_per_testcase(TC=conf3_tc2, Config) ->
    TC = ?config(tc32,Config),
    ok;
end_per_testcase(TC=conf4_tc1, Config) ->
    TC = ?config(tc41,Config),
    ok;
end_per_testcase(TC=conf4_tc2, Config) ->
    TC = ?config(tc42,Config),
    ok;
end_per_testcase(TC=conf5_tc1, Config) ->
    TC = ?config(tc51,Config),
    ok;
end_per_testcase(TC=conf5_tc2, Config) ->
    TC = ?config(tc52,Config),
    ok;
end_per_testcase(TC=conf6_tc1, Config) ->
    TC = ?config(tc61,Config),
    ok;
end_per_testcase(TC=conf6_tc2, Config) ->
    init = ?config(suite,Config),
    TC = ?config(tc62,Config),
    ok;
end_per_testcase(TC=conf7_tc1, Config) ->
    TC = ?config(tc71,Config),
    ok;
end_per_testcase(TC=conf7_tc2, Config) ->
    TC = ?config(tc72,Config),
    ok;
end_per_testcase(TC=conf8_tc1, Config) ->
    TC = ?config(tc81,Config),
    ok;
end_per_testcase(TC=conf8_tc2, Config) ->
    init = ?config(suite,Config),
    TC = ?config(tc82,Config),
    ok.

conf1_init(Config) when is_list(Config) ->
    test_server:comment(io_lib:format("~p",[now()])),
    [parallel] = ?config(tc_group_properties,Config),
    init = ?config(suite,Config),
    [{t0,now()},{cc1,conf1}|Config].
conf1_end(Config) ->
    %% check 2s & 3s < 4s
    Ms = timer:now_diff(now(),?config(t0,Config)),
    test_server:comment(io_lib:format("~p",[now()])),
    if Ms > 4000000 -> exit({bad_parallel_exec,Ms});
       Ms < 3000000 -> exit({bad_parallel_exec,Ms});
       true -> ok
    end.

conf2_init(Config) when is_list(Config) ->
    test_server:comment(io_lib:format("~p",[now()])),
    [parallel] = ?config(tc_group_properties,Config),
    [{t0,now()},{cc2,conf2}|Config].
conf2_end(Config) ->
    %% check 3s & 2s < 4s
    Ms = timer:now_diff(now(),?config(t0,Config)),
    test_server:comment(io_lib:format("~p",[now()])),
    if Ms > 4000000 -> exit({bad_parallel_exec,Ms});
       Ms < 3000000 -> exit({bad_parallel_exec,Ms});
       true -> ok
    end.

conf3_init(Config) when is_list(Config) ->
    test_server:comment(io_lib:format("~p",[now()])),
    [parallel] = ?config(tc_group_properties,Config),
    [{t0,now()},{cc3,conf3}|Config].
conf3_end(Config) ->
    %% check 6s & 6s & (2s & 3s) & 1s = ~6s
    Ms = timer:now_diff(now(),?config(t0,Config)),
    test_server:comment(io_lib:format("~p",[now()])),
    if Ms > 7000000 -> exit({bad_parallel_exec,Ms});
       Ms < 6000000 -> exit({bad_parallel_exec,Ms});
       true -> ok
    end.

conf4_init(Config) when is_list(Config) ->
    test_server:comment(io_lib:format("~p",[now()])),
    [] = ?config(tc_group_properties,Config),
    [{t0,now()},{cc4,conf4}|Config].
conf4_end(Config) ->
    %% check 2s & 3s >= 5s
    Ms = timer:now_diff(now(),?config(t0,Config)),
    test_server:comment(io_lib:format("~p",[now()])),
    if Ms > 6000000 -> exit({bad_parallel_exec,Ms});
       Ms < 5000000 -> exit({bad_parallel_exec,Ms});
       true -> ok
    end.

conf5_init(Config) when is_list(Config) ->
    test_server:comment(io_lib:format("~p",[now()])),
    [] = ?config(tc_group_properties,Config),
    [{t0,now()},{cc5,conf5}|Config].
conf5_end(Config) ->
    %% check 1s & 1s & (3s & 2s) & 1s = ~6s
    Ms = timer:now_diff(now(),?config(t0,Config)),
    test_server:comment(io_lib:format("~p",[now()])),
    if Ms > 7500000 -> exit({bad_parallel_exec,Ms});
       Ms < 6000000 -> exit({bad_parallel_exec,Ms});
       true -> ok
    end.

conf6_init(Config) when is_list(Config) ->
    test_server:comment(io_lib:format("~p",[now()])),
    [parallel] = ?config(tc_group_properties,Config),
    init = ?config(suite,Config),
    [{t0,now()},{cc6,conf6}|Config].
conf6_end(Config) ->
    %% check 3s & 2s < 5s
    Ms = timer:now_diff(now(),?config(t0,Config)),
    test_server:comment(io_lib:format("~p",[now()])),
    if Ms > 4500000 -> exit({bad_parallel_exec,Ms});
       Ms < 3000000 -> exit({bad_parallel_exec,Ms});
       true -> ok
    end.

conf5(suite) ->					% test specification
    [{conf, conf5_init, [conf5_tc1, conf5_tc1,

			 {conf, [parallel], conf6_init, [conf6_tc1, conf6_tc2], conf6_end},

			 conf5_tc2], conf5_end}].

conf7_init(Config) when is_list(Config) ->
    test_server:comment(io_lib:format("~p",[now()])),
    [parallel] = ?config(tc_group_properties,Config),
    [{t0,now()},{cc7,conf7}|Config].
conf7_end(Config) ->
    %% check 1s & 1s & (2s & 2s) & 1s = ~3s 
    Ms = timer:now_diff(now(),?config(t0,Config)),
    test_server:comment(io_lib:format("~p",[now()])),
    if Ms > 4000000 -> exit({bad_parallel_exec,Ms});
       Ms < 3000000 -> exit({bad_parallel_exec,Ms});
       true -> ok
    end.

conf8_init(Config) when is_list(Config) ->
    test_server:comment(io_lib:format("~p",[now()])),
    [parallel] = ?config(tc_group_properties,Config),
    init = ?config(suite,Config),
    [{t0,now()},{cc8,conf8}|Config].
conf8_end(Config) ->
    %% check 2s & 2s < 4s
    Ms = timer:now_diff(now(),?config(t0,Config)),
    test_server:comment(io_lib:format("~p",[now()])),
    if Ms > 3000000 -> exit({bad_parallel_exec,Ms});
       Ms < 2000000 -> exit({bad_parallel_exec,Ms});
       true -> ok
    end.


%%---------- test cases ----------

conf1_tc1(Config) when is_list(Config) ->
    case ?config(data_dir,Config) of
	undefined -> exit(no_data_dir);
	_ -> ok
    end,    
    init = ?config(suite,Config),
    conf1 = ?config(cc1,Config),
    conf1_tc1 = ?config(tc11,Config),
    timer:sleep(2000),
    test_server:comment(io_lib:format("~p",[now()])),
    ok.
conf1_tc2(Config) when is_list(Config) ->
    case ?config(priv_dir,Config) of
	undefined -> exit(no_priv_dir);
	_ -> ok
    end,    
    init = ?config(suite,Config),
    conf1 = ?config(cc1,Config),
    conf1_tc2 = ?config(tc12,Config),
    timer:sleep(3000),
    test_server:comment(io_lib:format("~p",[now()])),
    ok.

conf2_tc1(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    undefined = ?config(cc1,Config),
    undefined = ?config(tc11,Config),
    conf2 = ?config(cc2,Config),
    conf2_tc1 = ?config(tc21,Config),
    timer:sleep(3000),
    test_server:comment(io_lib:format("~p",[now()])),
    ok.
conf2_tc2(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    conf2 = ?config(cc2,Config),
    undefined = ?config(tc21,Config),
    conf2_tc2 = ?config(tc22,Config),
    timer:sleep(2000),
    test_server:comment(io_lib:format("~p",[now()])),
    ok.

conf3_tc1(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    undefined = ?config(cc2,Config),
    undefined = ?config(tc22,Config),
    conf3 = ?config(cc3,Config),
    conf3_tc1 = ?config(tc31,Config),
    timer:sleep(6000),
    test_server:comment(io_lib:format("~p",[now()])),
    ok.
conf3_tc2(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    conf3 = ?config(cc3,Config),
    undefined = ?config(cc4,Config),
    undefined = ?config(tc31,Config),
    undefined = ?config(tc41,Config),
    conf3_tc2 = ?config(tc32,Config),
    timer:sleep(1000),
    test_server:comment(io_lib:format("~p",[now()])),
    ok.

conf4_tc1(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    case ?config(data_dir,Config) of
	undefined -> exit(no_data_dir);
	_ -> ok
    end,    
    undefined = ?config(cc1,Config),
    undefined = ?config(cc2,Config),
    conf3 = ?config(cc3,Config),
    conf4 = ?config(cc4,Config),
    undefined = ?config(tc32,Config),
    conf4_tc1 = ?config(tc41,Config),
    timer:sleep(2000),
    test_server:comment(io_lib:format("~p",[now()])),
    ok.
conf4_tc2(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    case ?config(priv_dir,Config) of
	undefined -> exit(no_priv_dir);
	_ -> ok
    end,    
    conf3 = ?config(cc3,Config),
    conf4 = ?config(cc4,Config),
    undefined = ?config(tc41,Config),
    conf4_tc2 = ?config(tc42,Config),
    timer:sleep(3000),
    test_server:comment(io_lib:format("~p",[now()])),
    ok.

conf5_tc1(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    case ?config(data_dir,Config) of
	undefined -> exit(no_data_dir);
	_ -> ok
    end,    
    undefined = ?config(cc1,Config),
    undefined = ?config(cc2,Config),
    undefined = ?config(cc3,Config),
    undefined = ?config(cc4,Config),
    conf5 = ?config(cc5,Config),
    undefined = ?config(tc42,Config),
    conf5_tc1 = ?config(tc51,Config),
    timer:sleep(1000),
    test_server:comment(io_lib:format("~p",[now()])),
    ok.
conf5_tc2(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    case ?config(priv_dir,Config) of
	undefined -> exit(no_priv_dir);
	_ -> ok
    end,    
    conf5 = ?config(cc5,Config),
    undefined = ?config(cc6,Config),
    undefined = ?config(tc51,Config),
    undefined = ?config(tc62,Config),
    conf5_tc2 = ?config(tc52,Config),
    timer:sleep(1000),
    test_server:comment(io_lib:format("~p",[now()])),
    ok.

conf6_tc1(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    case ?config(data_dir,Config) of
	undefined -> exit(no_data_dir);
	_ -> ok
    end,    
    undefined = ?config(cc1,Config),
    undefined = ?config(cc2,Config),
    undefined = ?config(cc3,Config),
    undefined = ?config(cc4,Config),
    conf5 = ?config(cc5,Config),
    conf6 = ?config(cc6,Config),
    undefined = ?config(tc52,Config),
    conf6_tc1 = ?config(tc61,Config),
    timer:sleep(3000),
    test_server:comment(io_lib:format("~p",[now()])),
    ok.
conf6_tc2(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    case ?config(priv_dir,Config) of
	undefined -> exit(no_priv_dir);
	_ -> ok
    end,    
    conf5 = ?config(cc5,Config),
    conf6 = ?config(cc6,Config),
    undefined = ?config(tc61,Config),
    conf6_tc2 = ?config(tc62,Config),
    timer:sleep(2000),
    test_server:comment(io_lib:format("~p",[now()])),
    ok.

conf7_tc1(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    case ?config(data_dir,Config) of
	undefined -> exit(no_data_dir);
	_ -> ok
    end,    
    undefined = ?config(cc1,Config),
    undefined = ?config(cc2,Config),
    undefined = ?config(cc3,Config),
    undefined = ?config(cc4,Config),
    undefined = ?config(cc5,Config),
    undefined = ?config(cc6,Config),
    conf7 = ?config(cc7,Config),
    undefined = ?config(tc62,Config),
    conf7_tc1 = ?config(tc71,Config),
    timer:sleep(1000),
    test_server:comment(io_lib:format("~p",[now()])),
    ok.
conf7_tc2(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    case ?config(priv_dir,Config) of
	undefined -> exit(no_priv_dir);
	_ -> ok
    end,    
    conf7 = ?config(cc7,Config),
    undefined = ?config(cc8,Config),
    undefined = ?config(tc71,Config),
    undefined = ?config(tc82,Config),
    conf7_tc2 = ?config(tc72,Config),
    timer:sleep(1000),
    test_server:comment(io_lib:format("~p",[now()])),
    ok.

conf8_tc1(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    case ?config(data_dir,Config) of
	undefined -> exit(no_data_dir);
	_ -> ok
    end,    
    undefined = ?config(cc1,Config),
    undefined = ?config(cc2,Config),
    undefined = ?config(cc3,Config),
    undefined = ?config(cc4,Config),
    undefined = ?config(cc5,Config),
    undefined = ?config(cc6,Config),
    conf7 = ?config(cc7,Config),
    conf8 = ?config(cc8,Config),
    undefined = ?config(tc72,Config),
    conf8_tc1 = ?config(tc81,Config),
    timer:sleep(2000),
    test_server:comment(io_lib:format("~p",[now()])),
    ok.
conf8_tc2(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    case ?config(priv_dir,Config) of
	undefined -> exit(no_priv_dir);
	_ -> ok
    end,    
    conf7 = ?config(cc7,Config),
    conf8 = ?config(cc8,Config),
    undefined = ?config(tc81,Config),
    conf8_tc2 = ?config(tc82,Config),
    timer:sleep(2000),
    test_server:comment(io_lib:format("~p",[now()])),
    ok.
