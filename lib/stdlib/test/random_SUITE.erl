%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2011. All Rights Reserved.
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

-module(random_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([interval_1/1, seed0/1, seed/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

-include_lib("test_server/include/test_server.hrl").

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).

init_per_testcase(_Case, Config) ->
    ?line Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].
end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [interval_1, seed0, seed].

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


seed0(doc) ->
    ["Test that seed is set implicitly, and always the same."];
seed0(suite) ->
    [];
seed0(Config) when is_list(Config) ->
    ?line Self = self(),
    ?line _ = spawn(fun() -> Self ! random:uniform() end),
    ?line F1 = receive
		   Fa -> Fa
	  end,
    ?line _ = spawn(fun() -> random:seed(),
			     Self ! random:uniform() end),
    ?line F2 = receive
		   Fb -> Fb
	       end,
    ?line F1 = F2,
    ok.

seed(doc) ->
    ["Test that seed/1 and seed/3 is equivalent."];
seed(suite) ->
    [];
seed(Config) when is_list(Config) ->
    ?line Self = self(),
    ?line Seed = {S1, S2, S3} = now(),
    ?line _ = spawn(fun() ->
    	random:seed(S1,S2,S3),
    	Rands = lists:foldl(fun
	    (_, Out) -> [random:uniform(10000)|Out]
	end, [], lists:seq(1,100)),
    	Self ! {seed_test, Rands}
    end),
    ?line Rands1 = receive {seed_test, R1s} -> R1s end,
    ?line _ = spawn(fun() ->
    	random:seed(Seed),
    	Rands = lists:foldl(fun
	    (_, Out) -> [random:uniform(10000)|Out]
	end, [], lists:seq(1,100)),
    	Self ! {seed_test, Rands}
    end),
    ?line Rands2 = receive {seed_test, R2s} -> R2s end,
    ?line Rands1 = Rands2,
    ok.


interval_1(doc) ->
    ["Check that uniform/1 returns values within the proper interval."];
interval_1(suite) ->
    [];
interval_1(Config) when is_list(Config) ->
    ?line Top = 7,
    ?line N = 10,
    ?line check_interval(N, Top),
    ok.

check_interval(0, _) -> ok;
check_interval(N, Top) ->
    X = random:uniform(Top),
    if
	X < 1 ->
	    test_server:fail(too_small);
	X > Top ->
	    test_server:fail(too_large);
	true ->
	    ok
    end,
    check_interval(N-1, Top).
