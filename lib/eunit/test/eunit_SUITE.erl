%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
-module(eunit_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 app_test/1,appup_test/1,eunit_test/1,surefire_utf8_test/1,surefire_latin_test/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [app_test, appup_test, eunit_test, surefire_utf8_test, surefire_latin_test].

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

surefire_latin_test(Config) when is_list(Config) ->
    ok = file:set_cwd(proplists:get_value(priv_dir, Config, ".")),
	check_surefire(tlatin),
	ok.

surefire_utf8_test(Config) when is_list(Config) ->
    ok = file:set_cwd(proplists:get_value(priv_dir, Config, ".")),
	check_surefire(tutf8),
	ok.

check_surefire(Module) ->
	File = "TEST-"++atom_to_list(Module)++".xml",
	file:delete(File),
	% ignore test result, some fail on purpose
	eunit:test(Module, [{report,{eunit_surefire,[{dir,"."}]}}]),
	{ok, Bin} = file:read_file(File),
	[_|_] = unicode:characters_to_list(Bin, unicode),
	ok.