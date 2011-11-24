%%-*-erlang-*-
%%----------------------------------------------------------------------
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2010. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : xmerl_sax_SUITE.erl
%% Created : 2009-06-01
%%----------------------------------------------------------------------
-module(xmerl_sax_SUITE).
-compile(export_all).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("test_server/include/test_server.hrl").
-include_lib("kernel/include/file.hrl").

%%======================================================================
%% External functions
%%======================================================================

%%----------------------------------------------------------------------
%% Initializations
%%----------------------------------------------------------------------

init_per_suite(doc) ->
    ["Starts the test suite"];
init_per_suite(Config) ->
    Config.
 
end_per_suite(doc) ->
    ["Stops the test suite"];
end_per_suite(Config) ->
    Config.
 

 
%% initialization before each testcase
init_per_testcase(_TestCase,Config) ->
    Config.
 
%% clean up after each testcase
end_per_testcase(_Func,_Config) ->
    ok.

%%----------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Test Case 
%% ID: ticket_8213
%% Description: Checks that end of document is checked properly when continuation fun is missing.
ticket_8213(suite) -> [];
ticket_8213(_Config) -> 
    ?line {ok,ok,[]} = xmerl_sax_parser:stream("<elem/>", [{event_fun, fun (_E,_,_) -> ok end}]).


%%----------------------------------------------------------------------
%% Test Case 
%% ID: ticket_8214
%% Description: Checks that attributes with default namespace don't get [] in NS field.
ticket_8214(suite) -> [];
ticket_8214(_Config) -> 
    ?line {ok,ok,[]} = 
	xmerl_sax_parser:stream("<elem attr='123' x:attr='234' xmlns='http://lshift.net/d' "
				"xmlns:x='http://lshift.net/x' />", 
				[{event_fun, fun ({startElement,"http://lshift.net/d","elem",
						   {[],"elem"},
						   [{[],[],"attr","123"},{"http://lshift.net/x","x","attr","234"}]},
						  _, _) ->ok;
						 ({startElement, _, "elem",_,_}, _,_) -> 
						     throw({test, "Error in startElement tuple"});
						 (_E,_,_) -> ok
					     end}]).

%%----------------------------------------------------------------------
%% Bug test cases
%%

%%----------------------------------------------------------------------
%% Test Suite 
%% 
all() -> 
    [{group, bugs}].

groups() -> 
    [{bugs, [], [ticket_8213, ticket_8214]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


