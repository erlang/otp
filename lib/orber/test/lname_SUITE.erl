%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% 
%% Description:
%% Test suite for the Names Library module
%%
%%-----------------------------------------------------------------
-module(lname_SUITE).

-include_lib("test_server/include/test_server.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include_lib("orber/COSS/CosNaming/lname.hrl").

-define(default_timeout, ?t:minutes(3)).

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
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.


%%-----------------------------------------------------------------
%% Test Case: name component handling tests
%% Description: 
%%-----------------------------------------------------------------
lname_component(doc) -> ["Description", "more description"];
lname_component(suite) -> [];
lname_component(_) ->
    create_test(),
    get_tests(),
    set_tests().

create_test() ->
    ?line #'CosNaming_NameComponent'{} = lname_component:create(),
    ok.

get_tests() ->
    NC = #'CosNaming_NameComponent'{id="first", kind="apple"},
    NC1 = #'CosNaming_NameComponent'{id="", kind="apple"},
    NC2 = #'CosNaming_NameComponent'{id="first", kind=""},
    ?line "first" = lname_component:get_id(NC),
    ?line "apple" = lname_component:get_kind(NC),
    ?line {'EXCEPTION', #'LNameComponent_NotSet'{}} =
	(catch lname_component:get_id(NC1)),
    ?line {'EXCEPTION', #'LNameComponent_NotSet'{}} =
	(catch  lname_component:get_kind(NC2)),
    ok.

set_tests() ->
    NC = #'CosNaming_NameComponent'{id="first", kind="apple"},
    ?line #'CosNaming_NameComponent'{id="second", kind="apple"} =
	lname_component:set_id(NC, "second"),
    ?line #'CosNaming_NameComponent'{id="first", kind="pear"} =
	lname_component:set_kind(NC, "pear"),
    ok.

%%-----------------------------------------------------------------
%% Test Case: name handling tests
%% Description: 
%%-----------------------------------------------------------------
lname(doc) -> ["Description", "more description"];
lname(suite) -> [];
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
    ?line [NC, #'CosNaming_NameComponent'{id="first", kind="apple"},
	   #'CosNaming_NameComponent'{id="last", kind="peach"},
	   #'CosNaming_NameComponent'{id="and", kind="plum"},
	   #'CosNaming_NameComponent'{id="always", kind="orange"}] =
	lname:insert_component(Name, 1, NC),
    ?line [#'CosNaming_NameComponent'{id="first", kind="apple"},
	   #'CosNaming_NameComponent'{id="last", kind="peach"},
	   #'CosNaming_NameComponent'{id="and", kind="plum"},
	   #'CosNaming_NameComponent'{id="always", kind="orange"}, NC] =
	lname:insert_component(Name, 5, NC),
    ?line [#'CosNaming_NameComponent'{id="first", kind="apple"},
	   #'CosNaming_NameComponent'{id="last", kind="peach"}, 
	   #'CosNaming_NameComponent'{id="and", kind="plum"}, NC,
	   #'CosNaming_NameComponent'{id="always", kind="orange"}] =
	lname:insert_component(Name, 4, NC),
    ?line [#'CosNaming_NameComponent'{id="first", kind="apple"},
	   #'CosNaming_NameComponent'{id="last", kind="peach"}, NC,
	   #'CosNaming_NameComponent'{id="and", kind="plum"},
	   #'CosNaming_NameComponent'{id="always", kind="orange"}] =
	lname:insert_component(Name, 3, NC),
    ?line {'EXCEPTION', #'LName_NoComponent'{}} =
	(catch lname:insert_component(Name, 6, NC)),
    ?line {'EXCEPTION', #'LName_NoComponent'{}} =
	(catch lname:insert_component(Name, 0, NC)),
    ?line {'EXCEPTION', #'LName_NoComponent'{}} =
	(catch lname:insert_component(Name, -2, NC)),
    ok.

get_tests(Name) -> 
    ?line #'CosNaming_NameComponent'{id="first", kind="apple"} =
	lname:get_component(Name, 1),
    ?line #'CosNaming_NameComponent'{id="always", kind="orange"} =
	lname:get_component(Name, 4),
    ?line #'CosNaming_NameComponent'{id="and", kind="plum"} =
	lname:get_component(Name, 3),
    ?line {'EXCEPTION', #'LName_NoComponent'{}} =
	(catch lname:get_component(Name, 5)),
    ?line {'EXCEPTION', #'LName_NoComponent'{}} =
	(catch lname:get_component(Name, 0)),
    ?line {'EXCEPTION', #'LName_NoComponent'{}} =
	(catch lname:get_component(Name, -2)),
    ok.

delete_tests(Name) ->
    ?line [#'CosNaming_NameComponent'{id="last", kind="peach"},
	   #'CosNaming_NameComponent'{id="and", kind="plum"},
	   #'CosNaming_NameComponent'{id="always", kind="orange"}] =
	lname:delete_component(Name, 1),
    ?line [#'CosNaming_NameComponent'{id="first", kind="apple"},
	   #'CosNaming_NameComponent'{id="last", kind="peach"},
	   #'CosNaming_NameComponent'{id="and", kind="plum"}] =
	lname:delete_component(Name, 4),
    ?line [#'CosNaming_NameComponent'{id="first", kind="apple"},
	   #'CosNaming_NameComponent'{id="last", kind="peach"},
	   #'CosNaming_NameComponent'{id="always", kind="orange"}] =
	lname:delete_component(Name, 3),
    ?line {'EXCEPTION', #'LName_NoComponent'{}} =
	(catch lname:delete_component(Name, 6)),
    ?line {'EXCEPTION', #'LName_NoComponent'{}} =
	(catch lname:delete_component(Name, 0)),
    ?line {'EXCEPTION', #'LName_NoComponent'{}} =
	(catch lname:delete_component(Name, -2)),
    ok.

comparision_tests(Name) ->
    ?line true = lname:equal(Name, Name),
    ?line false = lname:equal(Name, lname:delete_component(Name, 2)),
    ?line true = lname:less_than(lname:delete_component(Name, 2), Name),
    ?line false = lname:less_than(Name, Name),
    ?line false = lname:less_than(Name, lname:delete_component(Name, 2)),
    ok.

convertion_tests(Name) ->
    ?line Name = lname:from_idl_form(Name),
    ?line Name = lname:to_idl_form(Name),
    ok.
