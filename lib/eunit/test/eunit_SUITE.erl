%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
-module(eunit_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 app_test/1,appup_test/1,eunit_test/1]).
	 
-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [app_test, appup_test, eunit_test].

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

app_test(Config) when is_list(Config) ->
    ok = ?t:app_test(eunit).

appup_test(Config) when is_list(Config) ->
    ok = ?t:appup_test(eunit).

eunit_test(Config) when is_list(Config) ->
    ok = file:set_cwd(code:lib_dir(eunit)),
    ok = eunit:test(eunit).

