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
-module(test_server_shuffle01_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all(doc) -> ["Test simple conf case structure, with and without nested cases"];
all(suite) -> 
    [
     {conf, [shuffle], conf1_init, [conf1_tc1, conf1_tc2, conf1_tc3], conf1_end},

     {conf, [{shuffle,{1,2,3}}], conf2_init, [conf2_tc1, conf2_tc2, conf2_tc3], conf2_end},

     {conf, [shuffle], conf3_init, [conf3_tc1, conf3_tc2, conf3_tc3,
				    
				    {conf, [], conf4_init, 
				     [conf4_tc1, conf4_tc2], conf4_end}], 
                       conf3_end},

     conf5,

     {conf, [shuffle,{repeat,5},parallel], conf7_init, [conf7_tc1, 
				    
				    {conf, [{shuffle,{3,2,1}},{repeat,3}], 
				     conf8_init, [conf8_tc1, conf8_tc2, conf8_tc3], 
				     conf8_end},
				    
				    conf7_tc2, conf7_tc3], conf7_end}
     
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
init_per_testcase(TC=conf1_tc3, Config) ->
    [{tc13,TC}|Config];
init_per_testcase(TC=conf2_tc1, Config) ->
    [{tc21,TC}|Config];
init_per_testcase(TC=conf2_tc2, Config) ->
    [{tc22,TC}|Config];
init_per_testcase(TC=conf2_tc3, Config) ->
    [{tc23,TC}|Config];
init_per_testcase(TC=conf3_tc1, Config) ->
    [{tc31,TC}|Config];
init_per_testcase(TC=conf3_tc2, Config) ->
    [{tc32,TC}|Config];
init_per_testcase(TC=conf3_tc3, Config) ->
    [{tc33,TC}|Config];
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
init_per_testcase(TC=conf6_tc3, Config) ->
    [{tc63,TC}|Config];
init_per_testcase(TC=conf7_tc1, Config) ->
    [{tc71,TC}|Config];
init_per_testcase(TC=conf7_tc2, Config) ->
    [{tc72,TC}|Config];
init_per_testcase(TC=conf7_tc3, Config) ->
    [{tc73,TC}|Config];
init_per_testcase(TC=conf8_tc1, Config) ->
    [{tc81,TC}|Config];
init_per_testcase(TC=conf8_tc2, Config) ->
    init = ?config(suite,Config),
    [{tc82,TC}|Config];
init_per_testcase(TC=conf8_tc3, Config) ->
    [{tc83,TC}|Config].

end_per_testcase(TC=conf1_tc1, Config) ->
    init = ?config(suite,Config),
    TC = ?config(tc11,Config),
    ok;
end_per_testcase(TC=conf1_tc2, Config) ->
    TC = ?config(tc12,Config),
    ok;
end_per_testcase(TC=conf1_tc3, Config) ->
    TC = ?config(tc13,Config),
    ok;
end_per_testcase(TC=conf2_tc1, Config) ->
    TC = ?config(tc21,Config),
    ok;
end_per_testcase(TC=conf2_tc2, Config) ->
    TC = ?config(tc22,Config),
    ok;
end_per_testcase(TC=conf2_tc3, Config) ->
    TC = ?config(tc23,Config),
    ok;
end_per_testcase(TC=conf3_tc1, Config) ->
    TC = ?config(tc31,Config),
    ok;
end_per_testcase(TC=conf3_tc2, Config) ->
    TC = ?config(tc32,Config),
    ok;
end_per_testcase(TC=conf3_tc3, Config) ->
    TC = ?config(tc33,Config),
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
end_per_testcase(TC=conf6_tc3, Config) ->
    TC = ?config(tc63,Config),
    ok;
end_per_testcase(TC=conf7_tc1, Config) ->
    TC = ?config(tc71,Config),
    ok;
end_per_testcase(TC=conf7_tc2, Config) ->
    TC = ?config(tc72,Config),
    ok;
end_per_testcase(TC=conf7_tc3, Config) ->
    TC = ?config(tc73,Config),
    ok;
end_per_testcase(TC=conf8_tc1, Config) ->
    TC = ?config(tc81,Config),
    ok;
end_per_testcase(TC=conf8_tc2, Config) ->
    init = ?config(suite,Config),
    TC = ?config(tc82,Config),
    ok;
end_per_testcase(TC=conf8_tc3, Config) ->
    TC = ?config(tc83,Config),
    ok.


conf1_init(Config) when is_list(Config) ->
    init = ?config(suite,Config),
    [{shuffle,{_,_,_}}] = ?config(tc_group_properties,Config),
    test_server:comment("Shuffle (random seed)"),
    [{cc1,conf1}|Config].
conf1_end(_Config) ->
    ok.

conf2_init(Config) when is_list(Config) ->
    [{shuffle,{1,2,3}}] = ?config(tc_group_properties,Config),
    test_server:comment("Shuffle (user seed)"),
    [{cc2,conf2}|Config].
conf2_end(_Config) ->
    ok.

conf3_init(Config) when is_list(Config) ->
    [{shuffle,{_,_,_}}] = ?config(tc_group_properties,Config),
    test_server:comment("Shuffle (random)"),
    [{cc3,conf3}|Config].
conf3_end(_Config) ->
    ok.

conf4_init(Config) when is_list(Config) ->
    [] = ?config(tc_group_properties,Config),
    test_server:comment("No shuffle"),
    [{cc4,conf4}|Config].
conf4_end(_Config) ->
    ok.

conf5_init(Config) when is_list(Config) ->
    [] = ?config(tc_group_properties,Config),
    test_server:comment("No shuffle"),
    [{cc5,conf5}|Config].
conf5_end(_Config) ->
    ok.

conf6_init(Config) when is_list(Config) ->
    validate_shuffle(Config),
    test_server:comment("Shuffle (random)"),
    init = ?config(suite,Config),
    [{cc6,conf6}|Config].
conf6_end(_Config) ->
    ok.

conf5(suite) ->					% test specification
    [{conf, conf5_init, [conf5_tc1, 

			 {conf, [shuffle], conf6_init, 
			  [conf6_tc1, conf6_tc2, conf6_tc3], 
			  conf6_end},

			 conf5_tc2], conf5_end}].

conf7_init(Config) when is_list(Config) ->
    test_server:comment("Group 7, Shuffle (random seed)"),
    validate_shuffle(Config),
    [{cc7,conf7}|Config].
conf7_end(_Config) ->
    ok.

conf8_init(Config) when is_list(Config) ->
    test_server:comment("Group 8, Shuffle (user start seed)"),
    validate_shuffle(Config),
    init = ?config(suite,Config),
    [{cc8,conf8}|Config].
conf8_end(_Config) ->
    ok.

validate_shuffle(Config) ->
    case proplists:get_value(shuffle, ?config(tc_group_properties,Config)) of
	{_,_,_} ->
	    ok;
	Seed ->
	    %% Must be a valid seed.
	    _ = rand:seed_s(rand:export_seed_s(Seed))
    end.


%%---------- test cases ----------

conf1_tc1(Config) when is_list(Config) ->
    test_server:comment("Case 1"),
    case ?config(data_dir,Config) of
	undefined -> exit(no_data_dir);
	_ -> ok
    end,    
    init = ?config(suite,Config),
    conf1 = ?config(cc1,Config),
    conf1_tc1 = ?config(tc11,Config),
    ok.
conf1_tc2(Config) when is_list(Config) ->
    test_server:comment("Case 2"),
    case ?config(priv_dir,Config) of
	undefined -> exit(no_priv_dir);
	_ -> ok
    end,    
    init = ?config(suite,Config),
    conf1 = ?config(cc1,Config),
    conf1_tc2 = ?config(tc12,Config),
    ok.
conf1_tc3(suite) -> [];
conf1_tc3(_Config) ->
    test_server:comment("Case 3"),
    ok.
    
conf2_tc1(Config) when is_list(Config) ->
    test_server:comment("Case 1"),
    init = ?config(suite,Config),
    undefined = ?config(cc1,Config),
    conf2 = ?config(cc2,Config),
    conf2_tc1 = ?config(tc21,Config),
    ok.
conf2_tc2(Config) when is_list(Config) ->
    test_server:comment("Case 2"),
    init = ?config(suite,Config),
    conf2 = ?config(cc2,Config),
    conf2_tc2 = ?config(tc22,Config),
    ok.
conf2_tc3(suite) -> [];
conf2_tc3(_Config) ->    
    test_server:comment("Case 3"),
    ok.

conf3_tc1(Config) when is_list(Config) ->
    test_server:comment("Case 1"),
    init = ?config(suite,Config),
    undefined = ?config(cc2,Config),
    conf3 = ?config(cc3,Config),
    conf3_tc1 = ?config(tc31,Config),
    ok.
conf3_tc2(Config) when is_list(Config) ->
    test_server:comment("Case 2"),
    init = ?config(suite,Config),
    conf3 = ?config(cc3,Config),
    undefined = ?config(cc4,Config),
    conf3_tc2 = ?config(tc32,Config),
    ok.
conf3_tc3(suite) -> [];
conf3_tc3(_Config) ->
    test_server:comment("Case 3"),
    ok.

conf4_tc1(Config) when is_list(Config) ->
    test_server:comment("Case 1"),
    init = ?config(suite,Config),
    case ?config(data_dir,Config) of
	undefined -> exit(no_data_dir);
	_ -> ok
    end,    
    undefined = ?config(cc1,Config),
    undefined = ?config(cc2,Config),
    conf3 = ?config(cc3,Config),
    conf4 = ?config(cc4,Config),
    conf4_tc1 = ?config(tc41,Config),
    ok.
conf4_tc2(Config) when is_list(Config) ->
    test_server:comment("Case 2"),
    init = ?config(suite,Config),
    case ?config(priv_dir,Config) of
	undefined -> exit(no_priv_dir);
	_ -> ok
    end,    
    conf3 = ?config(cc3,Config),
    conf4 = ?config(cc4,Config),
    conf4_tc2 = ?config(tc42,Config),
    ok.

conf5_tc1(Config) when is_list(Config) ->
    test_server:comment("Case 1"),
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
    conf5_tc1 = ?config(tc51,Config),
    ok.
conf5_tc2(Config) when is_list(Config) ->
    test_server:comment("Case 2"),
    init = ?config(suite,Config),
    case ?config(priv_dir,Config) of
	undefined -> exit(no_priv_dir);
	_ -> ok
    end,    
    conf5 = ?config(cc5,Config),
    undefined = ?config(cc6,Config),
    conf5_tc2 = ?config(tc52,Config),
    ok.

conf6_tc1(Config) when is_list(Config) ->
    test_server:comment("Case 1"),
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
    conf6_tc1 = ?config(tc61,Config),
    ok.
conf6_tc2(Config) when is_list(Config) ->
    test_server:comment("Case 2"),
    init = ?config(suite,Config),
    case ?config(priv_dir,Config) of
	undefined -> exit(no_priv_dir);
	_ -> ok
    end,    
    conf5 = ?config(cc5,Config),
    conf6 = ?config(cc6,Config),
    conf6_tc2 = ?config(tc62,Config),
    ok.
conf6_tc3(suite) -> [];
conf6_tc3(_Config) ->
    test_server:comment("Case 3"),
    ok.

conf7_tc1(Config) when is_list(Config) ->
    test_server:comment("Case 1"),
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
    conf7_tc1 = ?config(tc71,Config),
    ok.
conf7_tc2(Config) when is_list(Config) ->
    test_server:comment("Case 2"),
    init = ?config(suite,Config),
    case ?config(priv_dir,Config) of
	undefined -> exit(no_priv_dir);
	_ -> ok
    end,    
    conf7 = ?config(cc7,Config),
    undefined = ?config(cc8,Config),
    conf7_tc2 = ?config(tc72,Config),
    ok.
conf7_tc3(suite) -> [];
conf7_tc3(_Config) ->
    test_server:comment("Case 3"),
    ok.

conf8_tc1(Config) when is_list(Config) ->
    test_server:comment("Case 1"),
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
    conf8_tc1 = ?config(tc81,Config),
    ok.
conf8_tc2(Config) when is_list(Config) ->
    test_server:comment("Case 2"),
    init = ?config(suite,Config),
    case ?config(priv_dir,Config) of
	undefined -> exit(no_priv_dir);
	_ -> ok
    end,    
    conf7 = ?config(cc7,Config),
    conf8 = ?config(cc8,Config),
    conf8_tc2 = ?config(tc82,Config),
    ok.
conf8_tc3(suite) -> [];
conf8_tc3(_Config) ->
    test_server:comment("Case 3"),
    ok.
