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

-module(reltool_wx_SUITE).

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, 
	 init_per_suite/1, end_per_suite/1, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([start_all_windows/1, check_no_win_crash/0, wait_terminate/1]).

-include("reltool_test_lib.hrl").

%% Initialization functions.
init_per_suite(Config) ->
    reltool_test_lib:wx_init_per_suite(Config).

end_per_suite(Config) ->
    reltool_test_lib:wx_end_per_suite(Config).

init_per_testcase(Func,Config) ->
    reltool_test_lib:init_per_testcase(Func,Config).
end_per_testcase(Func,Config) -> 
    reltool_test_lib:end_per_testcase(Func,Config).

%% SUITE specification
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [start_all_windows].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% The test cases

%% Display all windows and see if something crashes
start_all_windows(TestInfo) when is_atom(TestInfo) ->
    reltool_test_lib:tc_info(TestInfo);
start_all_windows(_Config) ->
    {ok, SysPid} = ?msym({ok, _}, reltool:start([{trap_exit, false}])),
    erlang:monitor(process,SysPid),
    {ok, AppPid} = ?msym({ok, _}, reltool_sys_win:open_app(SysPid, stdlib)),
    erlang:monitor(process,AppPid),
    {ok, ModPid} = ?msym({ok, _}, reltool_app_win:open_mod(AppPid, escript)),
    erlang:monitor(process,ModPid),

    %% Let all windows get started
    timer:sleep(timer:seconds(10)),

    %% Test that server pid can be fetched, and that server is alive
    {ok, Server} = ?msym({ok,_}, reltool:get_server(SysPid)),
    ?m(true, erlang:is_process_alive(Server)),
    ?m({ok,{sys,[]}}, reltool:get_config(Server)),

    %% Terminate
    check_no_win_crash(),
    ?m(ok, reltool:stop(SysPid)),
    wait_terminate([{sys,SysPid},{app,AppPid},{mod,ModPid}]),

    ok.


%%%-----------------------------------------------------------------
%%% Internal functions
check_no_win_crash() ->
    receive {'DOWN',_,_,_,_} = Down ->
	    ct:log("Unexpected termination of window:~n~p",[Down]),
	    ct:fail("window crashed")
    after 0 ->
	    ok
    end.

wait_terminate([]) ->
    ok;
wait_terminate([{Win,P}|Rest]) ->
    receive
	{'DOWN',_,process,P,shutdown} ->
	    wait_terminate(Rest);
	{'DOWN',_,process,P,Reason} ->
	    ct:log("~p window terminated with unexpected reason:~n~p",
		   [Win,Reason]),
	    ct:fail("unexpected exit reason from window")
    end.
