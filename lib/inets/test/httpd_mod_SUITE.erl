%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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

%% 
%% ct:run("../inets_test", httpd_mod_SUITE).
-module(httpd_mod_SUITE).

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").
-include("inets_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    [
     {group, http},
     {group, https}
    ].

groups() ->
    [
     {http, [], all_version_groups()},
     {https, [], all_version_groups()}
     {http_1_1, [], []},
     {http_1_0, [], []},
     {http_0_9, [], []},
     {mod_alias, [], []},
     {mod_actions, [], []},
     {mod_security, [], []},
     {mod_auth, [], []},
     {mod_htaccess, [], []},
     {mod_cgi, [], []},
     {mod_esi, [], []},
     {mod_head, [], []},
     {configure, [], []}
    ].

all_version_groups ()->
    [
     {group, mod_alias},
     {group, mod_actions},
     {group, mod_security},
     {group, mod_auth},
     {group, mod_htaccess},
     {group, mod_cgi},
     {group, mod_esi},
     {group, mod_head}
    ].

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
