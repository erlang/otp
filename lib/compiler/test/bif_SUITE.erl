%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016. All Rights Reserved.
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
-module(bif_SUITE).

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 beam_validator/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() ->
    [{p,[parallel],
      [beam_validator
      ]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%% Cover code in beam_validator.

beam_validator(Config) ->
    [false,Config] = food(Config),

    true = is_number(42.0),
    false = is_port(Config),

    ok.

food(Curriculum) ->
    [try
	 is_bitstring(functions)
     catch _ ->
	     0
     end, Curriculum].
