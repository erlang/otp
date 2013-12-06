%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2013-2013. All Rights Reserved.
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
