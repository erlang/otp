%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2011. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%

-module(reltool_wx_SUITE).

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, 
	 init_per_suite/1, end_per_suite/1, 
	 init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

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
    {ok, AppPid} = ?msym({ok, _}, reltool_sys_win:open_app(SysPid, stdlib)),
    ?msym({ok, _}, reltool_app_win:open_mod(AppPid, escript)),
    timer:sleep(timer:seconds(10)),
    ?m(ok, reltool:stop(SysPid)),
    
    ok.
