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

-module(ct_cth_prio_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").

suite() ->
    ([{timetrap, {minutes, 10}},
      {ct_hooks, [{empty_cth,[800],800},
		  {prio_cth,[1200]},{prio_cth,[1200,1050],1050}]}]).

%% Test server callback functions
init_per_suite(Config) ->
    [{ct_hooks, [{empty_cth,[700],700},
		 {prio_cth,[600,600]},
		 {prio_cth,[600,200],200}]}|Config].

end_per_suite(_Config) ->
    ok.

init_per_group(_G, Config) ->
    [{ct_hooks, [{empty_cth,[600],600},
		 {prio_cth,[900,900]},{prio_cth,[500,900],900}]}|Config].

end_per_group(_G, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [{group,test_group}].

groups() ->
    [{test_group,[],[test_case]}].

%% Test cases starts here.
test_case(Config) when is_list(Config) ->
    ok.
