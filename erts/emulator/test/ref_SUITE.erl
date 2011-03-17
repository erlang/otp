%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2011. All Rights Reserved.
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

-module(ref_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2]).
-export([wrap_1/1]).

-export([loop_ref/1]).

-include_lib("test_server/include/test_server.hrl").

init_per_testcase(_, Config) ->
    ?line Dog=test_server:timetrap(test_server:minutes(2)),
    [{watchdog, Dog}|Config].

end_per_testcase(_, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [wrap_1].

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


wrap_1(doc) -> "Check that refs don't wrap around easily.";
wrap_1(Config) when is_list(Config) ->
    ?line spawn_link(?MODULE, loop_ref, [self()]),
    ?line receive
	      done ->
		  test_server:fail(wrapfast)
	  after 30000 ->
		  ok
	  end,
    ok.

loop_ref(Parent) ->
    Ref0 = make_ref(),
    loop_ref(Ref0, first, 0),
    Parent ! done.

loop_ref(R, R, _) -> ok;
loop_ref(R0, _, N) ->
    loop_ref(R0, make_ref(), N+1).
