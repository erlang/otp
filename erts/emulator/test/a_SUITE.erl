%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2017. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : a_SUITE.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : Misc tests that should be run first
%%%
%%% Created : 21 Aug 2006 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(a_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1,
	 leaked_processes/1, long_timers/1, pollset_size/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    [leaked_processes, long_timers, pollset_size].

%% Start some system servers now to avoid having them
%% reported as leaks.

init_per_suite(Config) when is_list(Config) ->
    %% Ensure inet_gethost_native port program started, in order to
    %% allow other suites to use it...
    inet_gethost_native:gethostbyname("localhost"),

    %% Start the timer server.
    timer:start(),

    Config.

end_per_suite(Config) when is_list(Config) ->
    Config.

leaked_processes(Config) when is_list(Config) ->
    Parent = self(),
    Go = make_ref(),
    spawn(fun () ->
                  Name = leaked_processes__process_holder,
                  true = register(Name, self()),
                  Ps = processes(),
                  Parent ! Go,
                  receive
                      {get_initial_processes, Pid} ->
                          Pid ! {initial_processes, Ps}
                  end
          end),
    receive Go -> ok end,
    {comment, "Testcase started! This test will run in parallel with the "
     "erts testsuite and ends in the z_SUITE:leaked_processes/1 testcase."}.

long_timers(Config) when is_list(Config) ->
    Dir = proplists:get_value(data_dir, Config),
    long_timers_test:start(Dir),
    {comment, "Testcase started! This test will run in parallel with the "
     "erts testsuite and ends in the z_SUITE:long_timers/1 testcase."}.

pollset_size(Config) when is_list(Config) ->
    Parent = self(),
    Go = make_ref(),
    spawn(fun () ->
                  Name = pollset_size_testcase_initial_state_holder,
                  true = register(Name, self()),
                  ChkIo = get_check_io_info(),
                  io:format("Initial: ~p~n", [ChkIo]),
                  Parent ! Go,
                  receive
                      {get_initial_check_io_result, Pid} ->
                          Pid ! {initial_check_io_result, ChkIo}
                  end
          end),
    receive Go -> ok end,
    {comment, "Testcase started! This test will run in parallel with the "
     "erts testsuite and ends in the z_SUITE:pollset_size/1 testcase."}.

%%
%% Internal functions...
%%

get_check_io_info() ->
    z_SUITE:get_check_io_info().
