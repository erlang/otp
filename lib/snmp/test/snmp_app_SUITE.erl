%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2019. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Verify the application specifics of the snmp application
%%----------------------------------------------------------------------
-module(snmp_app_SUITE).

%% Note: This directive should only be used in test suites.
-export([
        suite/0, all/0,
        init_per_suite/1,    end_per_suite/1,
        init_per_group/2,    end_per_group/2,
        init_per_testcase/2, end_per_testcase/2,

        app/0, app/1,
        appup/0, appup/1
        ]).


-include_lib("common_test/include/ct.hrl").
-include("snmp_test_lib.hrl").


%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    [
     app, 
     appup
    ].



%%
%% -----
%%

init_per_suite(Config) when is_list(Config) ->
    Config.

end_per_suite(Config) when is_list(Config) ->
    Config.



%%
%% -----
%%

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%
%% -----
%%

init_per_testcase(_Case, Config) when is_list(Config) ->
    Config.

end_per_testcase(_Case, Config) when is_list(Config) ->
    Config.



%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
app() ->
    [{doc, "Test that the snmp app file is ok"}].
app(Config) when is_list(Config) ->
    ok = test_server:app_test(snmp).


%%--------------------------------------------------------------------
appup() ->
    [{doc, "Test that the snmp appup file is ok"}].
appup(Config) when is_list(Config) ->
    ok = test_server:appup_test(snmp).
