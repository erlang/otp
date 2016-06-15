%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% 
%% Description:
%% Test suite for the Names Library module
%%
%%-----------------------------------------------------------------
-module(lname_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include_lib("orber/COSS/CosNaming/lname.hrl").

-define(default_timeout, test_server:minutes(3)).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).
-compile(export_all).

%%-----------------------------------------------------------------
%% Func: all/1
%% Args: 
%% Returns: 
%%-----------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [lname_component, lname].

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


%%-----------------------------------------------------------------
%% Init and cleanup functions.
%%-----------------------------------------------------------------

init_per_testcase(_Case, Config) ->
    Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    Dog = proplists:get_value(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.


%%-----------------------------------------------------------------
%% Test Case: name component handling tests
%% Description: 
%%-----------------------------------------------------------------
lname_component(_) ->
    create_test(),
    get_tests(),
    set_tests().

create_test() ->
    #'CosNaming_NameComponent'{} = lname_component:create(),
    ok.

get_tests() ->
    NC = #'CosNaming_NameComponent'{id="first", kind="apple"},
    NC1 = #'CosNaming_NameComponent'{id="", kind="apple"},
    NC2 = #'CosNaming_NameComponent'{id="first", kind=""},
    "first" = lname_component:get_id(NC),
    "apple" = lname_component:get_kind(NC),
    {'EXCEPTION', #'LNameComponent_NotSet'{}} =
	(catch lname_component:get_id(NC1)),
    {'EXCEPTION', #'LNameComponent_NotSet'{}} =
	(catch  lname_component:get_kind(NC2)),
    ok.

set_tests() ->
    NC = #'CosNaming_NameComponent'{id="first", kind="apple"},
    #'CosNaming_NameComponent'{id="second", kind="apple"} =
	lname_component:set_id(NC, "second"),
    #'CosNaming_NameComponent'{id="first", kind="pear"} =
	lname_component:set_kind(NC, "pear"),
    ok.

%%-----------------------------------------------------------------
%% Test Case: name handling tests
%% Description: 
%%-----------------------------------------------------------------
lname(_) ->
    Name = [#'CosNaming_NameComponent'{id="first", kind="apple"},
	    #'CosNaming_NameComponent'{id="last", kind="peach"},
	    #'CosNaming_NameComponent'{id="and", kind="plum"},
	    #'CosNaming_NameComponent'{id="always", kind="orange"}],
    insert_tests(Name), 
    get_tests(Name), 
    delete_tests(Name), 
    comparision_tests(Name),
    convertion_tests(Name).

insert_tests(Name) ->
    NC = #'CosNaming_NameComponent'{id="new", kind="pear"},
    [NC, #'CosNaming_NameComponent'{id="first", kind="apple"},
	   #'CosNaming_NameComponent'{id="last", kind="peach"},
	   #'CosNaming_NameComponent'{id="and", kind="plum"},
	   #'CosNaming_NameComponent'{id="always", kind="orange"}] =
	lname:insert_component(Name, 1, NC),
    [#'CosNaming_NameComponent'{id="first", kind="apple"},
	   #'CosNaming_NameComponent'{id="last", kind="peach"},
	   #'CosNaming_NameComponent'{id="and", kind="plum"},
	   #'CosNaming_NameComponent'{id="always", kind="orange"}, NC] =
	lname:insert_component(Name, 5, NC),
    [#'CosNaming_NameComponent'{id="first", kind="apple"},
	   #'CosNaming_NameComponent'{id="last", kind="peach"}, 
	   #'CosNaming_NameComponent'{id="and", kind="plum"}, NC,
	   #'CosNaming_NameComponent'{id="always", kind="orange"}] =
	lname:insert_component(Name, 4, NC),
    [#'CosNaming_NameComponent'{id="first", kind="apple"},
	   #'CosNaming_NameComponent'{id="last", kind="peach"}, NC,
	   #'CosNaming_NameComponent'{id="and", kind="plum"},
	   #'CosNaming_NameComponent'{id="always", kind="orange"}] =
	lname:insert_component(Name, 3, NC),
    {'EXCEPTION', #'LName_NoComponent'{}} =
	(catch lname:insert_component(Name, 6, NC)),
    {'EXCEPTION', #'LName_NoComponent'{}} =
	(catch lname:insert_component(Name, 0, NC)),
    {'EXCEPTION', #'LName_NoComponent'{}} =
	(catch lname:insert_component(Name, -2, NC)),
    ok.

get_tests(Name) -> 
    #'CosNaming_NameComponent'{id="first", kind="apple"} =
	lname:get_component(Name, 1),
    #'CosNaming_NameComponent'{id="always", kind="orange"} =
	lname:get_component(Name, 4),
    #'CosNaming_NameComponent'{id="and", kind="plum"} =
	lname:get_component(Name, 3),
    {'EXCEPTION', #'LName_NoComponent'{}} =
	(catch lname:get_component(Name, 5)),
    {'EXCEPTION', #'LName_NoComponent'{}} =
	(catch lname:get_component(Name, 0)),
    {'EXCEPTION', #'LName_NoComponent'{}} =
	(catch lname:get_component(Name, -2)),
    ok.

delete_tests(Name) ->
    [#'CosNaming_NameComponent'{id="last", kind="peach"},
	   #'CosNaming_NameComponent'{id="and", kind="plum"},
	   #'CosNaming_NameComponent'{id="always", kind="orange"}] =
	lname:delete_component(Name, 1),
    [#'CosNaming_NameComponent'{id="first", kind="apple"},
	   #'CosNaming_NameComponent'{id="last", kind="peach"},
	   #'CosNaming_NameComponent'{id="and", kind="plum"}] =
	lname:delete_component(Name, 4),
    [#'CosNaming_NameComponent'{id="first", kind="apple"},
	   #'CosNaming_NameComponent'{id="last", kind="peach"},
	   #'CosNaming_NameComponent'{id="always", kind="orange"}] =
	lname:delete_component(Name, 3),
    {'EXCEPTION', #'LName_NoComponent'{}} =
	(catch lname:delete_component(Name, 6)),
    {'EXCEPTION', #'LName_NoComponent'{}} =
	(catch lname:delete_component(Name, 0)),
    {'EXCEPTION', #'LName_NoComponent'{}} =
	(catch lname:delete_component(Name, -2)),
    ok.

comparision_tests(Name) ->
    true = lname:equal(Name, Name),
    false = lname:equal(Name, lname:delete_component(Name, 2)),
    true = lname:less_than(lname:delete_component(Name, 2), Name),
    false = lname:less_than(Name, Name),
    false = lname:less_than(Name, lname:delete_component(Name, 2)),
    ok.

convertion_tests(Name) ->
    Name = lname:from_idl_form(Name),
    Name = lname:to_idl_form(Name),
    ok.
