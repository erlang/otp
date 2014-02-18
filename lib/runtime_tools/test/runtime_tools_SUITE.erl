%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2012. All Rights Reserved.
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
%%
-module(runtime_tools_SUITE).
-include_lib("test_server/include/test_server.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([app_file/1, appup_file/1, start_stop_app/1]).

%% Default timetrap timeout (set in init_per_testcase)
-define(default_timeout, ?t:minutes(1)).

init_per_testcase(_Case, Config) ->
    Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [app_file,
     appup_file,
     start_stop_app].

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


app_file(_Config) ->
    ?line ok = ?t:app_test(runtime_tools),
    ok.

appup_file(_Config) ->
    ok = ?t:appup_test(runtime_tools).

start_stop_app(_Config) ->
    ok = application:start(runtime_tools),
    Sup = whereis(runtime_tools_sup),
    true = is_pid(Sup),
    Ref = erlang:monitor(process,Sup),
    ok = application:stop(runtime_tools),
    receive {'DOWN', Ref, process, Sup, shutdown} -> ok end.
