%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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

%% This is a wrapper suite, to make it possible to simply run all 
%% socket suites in one go.
%%

-module(socket_suites).

-include("kernel_test_lib.hrl").

-export([suite/0, all/0, groups/0]).
-export([
	 init_per_suite/1,    end_per_suite/1,
	 init_per_group/2,    end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.


init_per_group(GroupName, Config)
  when (GroupName =:= gen_tcp_api) orelse
       (GroupName =:= gen_tcp_echo) orelse
       (GroupName =:= gen_tcp_misc) orelse
       (GroupName =:= gen_udp) ->
    [{inet_backend, socket}|Config];
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

suite() ->
    [{ct_hooks, [ts_install_cth]},
     {timetrap, {minutes,1}}].

all() -> 
    %% This, together with the group init above, is what is required to
    %% run the inet_backend_socket group (and only this top grooup) of
    %% the gen_tcp_api, gen_tcp_echo, gen_tcp_misc, gen_udp test suites.
    ok = application:set_env(kernel, test_inet_backends, true),
    [
     {group, main},
     {group, api},
     {group, traffic},
     {group, ttest},
     {group, gen_tcp},

     {group, gen_tcp_api},
     {group, gen_tcp_echo},
     {group, gen_tcp_misc},
     {group, gen_udp}
    ].

groups() -> 
    [
     {main,           [], [{socket_SUITE,               all}]},
     {api,            [], [{socket_api_SUITE,           all}]},
     {traffic,        [], [{socket_traffic_SUITE,       all}]},
     {ttest,          [], [{socket_ttest_SUITE,         all}]},
     {gen_tcp,        [], [{gen_tcp_socket_SUITE,       all}]},

     {gen_tcp_api,    [], [{gen_tcp_api_SUITE,          all}]},
     {gen_tcp_echo,   [], [{gen_tcp_echo_SUITE,         all}]},
     {gen_tcp_misc,   [], [{gen_tcp_misc_SUITE,         all}]},
     {gen_udp,        [], [{gen_udp_SUITE,              all}]}
    ].
