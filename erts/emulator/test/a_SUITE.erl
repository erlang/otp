%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
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

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, long_timers/1, pollset_size/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [long_timers, pollset_size].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


long_timers(doc) ->
    [];
long_timers(suite) ->
    [];
long_timers(Config) when is_list(Config) ->
    Dir = ?config(data_dir, Config),
    ?line long_timers_test:start(Dir),
    ?line {comment,
	   "Testcase started! This test will run in parallel with the "
	   "erts testsuite and ends in the z_SUITE:long_timers testcase."}.

pollset_size(doc) ->
    [];
pollset_size(suite) ->
    [];
pollset_size(Config) when is_list(Config) ->
    %% Ensure inet_gethost_native port program started, in order to
    %% allow other suites to use it...
    inet_gethost_native:gethostbyname("localhost"),
    ?line Parent = self(),
    ?line Go = make_ref(),
    ?line spawn(fun () ->
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
    ?line receive Go -> ok end,
    ?line {comment,
	   "Testcase started! This test will run in parallel with the "
	   "erts testsuite and ends in the z_SUITE:pollset_size testcase."}.

%%
%% Internal functions...
%%

display_check_io(ChkIo) ->
    catch erlang:display('--- CHECK IO INFO ---'),
    catch erlang:display(ChkIo),
    catch erts_debug:set_internal_state(available_internal_state, true),
    NoOfErrorFds = (catch element(1, erts_debug:get_internal_state(check_io_debug))),
    catch erlang:display({'NoOfErrorFds', NoOfErrorFds}),
    catch erts_debug:set_internal_state(available_internal_state, false),
    catch erlang:display('--- CHECK IO INFO ---'),
    ok.

get_check_io_info() ->
    z_SUITE:get_check_io_info().


