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
-module(test_server_conf02_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all(doc) -> ["Test simple conf case structure, with and without nested cases"];
all(suite) -> 
    [
     {conf, conf1_init, [conf1_tc1, conf1_tc2], conf1_end},

     {conf, [], conf2_init, [conf2_tc1, conf2_tc2], conf2_end},

     {conf, conf3_init, [conf3_tc1, 

			 {conf, [], conf4_init, [conf4_tc1, conf4_tc2], conf4_end},

			 conf3_tc2], conf3_end},

     conf5 
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
    [{tc62,TC}|Config].

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
    ok.

conf1_init(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    [{cc1,conf1}|Config].
conf1_end(_Config) ->
    ok.

conf2_init(Config) when is_list(Config) ->
    [{cc2,conf2}|Config].
conf2_end(_Config) ->
    ok.

conf3_init(Config) when is_list(Config) ->
    [{cc3,conf3}|Config].
conf3_end(_Config) ->
    ok.

conf4_init(Config) when is_list(Config) ->
    [{cc4,conf4}|Config].
conf4_end(_Config) ->
    ok.

conf5_init(Config) when is_list(Config) ->
    [{cc5,conf5}|Config].
conf5_end(_Config) ->
    ok.

conf6_init(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    [{cc6,conf6}|Config].
conf6_end(_Config) ->
    ok.

conf5(suite) ->					% test specification
    [{conf, conf5_init, [conf5_tc1, 

			 {conf, [], conf6_init, [conf6_tc1, conf6_tc2], conf6_end},

			 conf5_tc2], conf5_end}].

%%---------- test cases ----------

conf1_tc1(Config) when is_list(Config) ->
    case ?config(data_dir,Config) of
	undefined -> exit(no_data_dir);
	_ -> ok
    end,    
    init = ?config(suite,Config),
    conf1 = ?config(cc1,Config),
    conf1_tc1 = ?config(tc11,Config),
    ok.
conf1_tc2(Config) when is_list(Config) ->
    case ?config(priv_dir,Config) of
	undefined -> exit(no_priv_dir);
	_ -> ok
    end,    
    init = ?config(suite,Config),
    conf1 = ?config(cc1,Config),
    conf1_tc2 = ?config(tc12,Config),
    ok.

conf2_tc1(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    undefined = ?config(cc1,Config),
    undefined = ?config(tc11,Config),
    conf2 = ?config(cc2,Config),
    conf2_tc1 = ?config(tc21,Config),
    ok.
conf2_tc2(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    conf2 = ?config(cc2,Config),
    undefined = ?config(tc21,Config),
    conf2_tc2 = ?config(tc22,Config),
    ok.

conf3_tc1(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    undefined = ?config(cc2,Config),
    undefined = ?config(tc22,Config),
    conf3 = ?config(cc3,Config),
    conf3_tc1 = ?config(tc31,Config),
    ok.
conf3_tc2(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    conf3 = ?config(cc3,Config),
    undefined = ?config(cc4,Config),
    undefined = ?config(tc31,Config),
    undefined = ?config(tc41,Config),
    conf3_tc2 = ?config(tc32,Config),
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
    ok.
