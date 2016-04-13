%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
-module(erl_ext_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("erl_ext_SUITE_data/ext_test_cases.hrl").

-export([all/0, suite/0,
         compare_tuple/1,
         compare_list/1,
         compare_string/1,
         compare_list_string/1,
         compare_nc_ext/1]).

-import(runner, [get_term/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() -> 
    [compare_tuple, compare_list, compare_string,
     compare_list_string, compare_nc_ext].


compare_tuple(Config) when is_list(Config) ->
    P = runner:start(?compare_tuple),
    runner:recv_eot(P),
    ok.

compare_list(Config) when is_list(Config) ->
    P = runner:start(?compare_list),
    runner:recv_eot(P),
    ok.

compare_string(Config) when is_list(Config) ->
    P = runner:start(?compare_string),
    runner:recv_eot(P),
    ok.

compare_list_string(Config) when is_list(Config) ->
    P = runner:start(?compare_list_string),
    runner:recv_eot(P),
    ok.

compare_nc_ext(Config) when is_list(Config) ->
    P = runner:start(?compare_nc_ext),
    runner:recv_eot(P),
    ok.
