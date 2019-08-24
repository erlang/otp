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
-module(test_server_conf01_SUITE).
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

conf1_init(Config) when is_list(Config) ->
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
    conf1 = ?config(cc1,Config),
    ok.
conf1_tc2(Config) when is_list(Config) ->
    case ?config(priv_dir,Config) of
	undefined -> exit(no_priv_dir);
	_ -> ok
    end,    
    conf1 = ?config(cc1,Config),
    ok.

conf2_tc1(Config) when is_list(Config) ->
    undefined = ?config(cc1,Config),
    case ?config(data_dir,Config) of
	undefined -> exit(no_data_dir);
	_ -> ok
    end,    
    conf2 = ?config(cc2,Config),
    ok.
conf2_tc2(Config) when is_list(Config) ->
    case ?config(priv_dir,Config) of
	undefined -> exit(no_priv_dir);
	_ -> ok
    end,    
    conf2 = ?config(cc2,Config),
    ok.

conf3_tc1(Config) when is_list(Config) ->
    undefined = ?config(cc1,Config),
    undefined = ?config(cc2,Config),
    conf3 = ?config(cc3,Config),
    ok.
conf3_tc2(Config) when is_list(Config) ->
    conf3 = ?config(cc3,Config),
    undefined = ?config(cc4,Config),
    ok.

conf4_tc1(Config) when is_list(Config) ->
    case ?config(data_dir,Config) of
	undefined -> exit(no_data_dir);
	_ -> ok
    end,    
    undefined = ?config(cc1,Config),
    undefined = ?config(cc2,Config),
    conf3 = ?config(cc3,Config),
    conf4 = ?config(cc4,Config),
    ok.
conf4_tc2(Config) when is_list(Config) ->
    case ?config(priv_dir,Config) of
	undefined -> exit(no_priv_dir);
	_ -> ok
    end,    
    conf3 = ?config(cc3,Config),
    conf4 = ?config(cc4,Config),
    ok.

conf5_tc1(Config) when is_list(Config) ->
    case ?config(data_dir,Config) of
	undefined -> exit(no_data_dir);
	_ -> ok
    end,    
    undefined = ?config(cc1,Config),
    undefined = ?config(cc2,Config),
    undefined = ?config(cc3,Config),
    undefined = ?config(cc4,Config),
    conf5 = ?config(cc5,Config),
    ok.
conf5_tc2(Config) when is_list(Config) ->
    case ?config(priv_dir,Config) of
	undefined -> exit(no_priv_dir);
	_ -> ok
    end,    
    conf5 = ?config(cc5,Config),
    undefined = ?config(cc6,Config),
    ok.

conf6_tc1(Config) when is_list(Config) ->
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
    ok.
conf6_tc2(Config) when is_list(Config) ->
    case ?config(priv_dir,Config) of
	undefined -> exit(no_priv_dir);
	_ -> ok
    end,    
    conf5 = ?config(cc5,Config),
    conf6 = ?config(cc6,Config),
    ok.
    
