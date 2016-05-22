%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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

-module(random_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_testcase/2, end_per_testcase/2,
	 init_per_group/2,end_per_group/2]).

-export([interval_1/1, seed0/1, seed/1]).


-include_lib("common_test/include/ct.hrl").

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

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


%% Test that seed is set implicitly, and always the same.
seed0(Config) when is_list(Config) ->
    Self = self(),
    _ = spawn(fun() -> Self ! random:uniform() end),
    F1 = receive
	     Fa -> Fa
	 end,
    _ = spawn(fun() -> random:seed(),
		       Self ! random:uniform() end),
    F2 = receive
	     Fb -> Fb
	 end,
    F1 = F2,
    ok.

%% Test that seed/1 and seed/3 are equivalent.
seed(Config) when is_list(Config) ->
    Self = self(),
    Seed = {S1, S2, S3} = erlang:timestamp(),
    _ = spawn(fun() ->
		      random:seed(S1,S2,S3),
		      Rands = lists:foldl(fun
					      (_, Out) -> [random:uniform(10000)|Out]
					 end, [], lists:seq(1,100)),
		      Self ! {seed_test, Rands}
	      end),
    Rands1 = receive {seed_test, R1s} -> R1s end,
    _ = spawn(fun() ->
		      random:seed(Seed),
		      Rands = lists:foldl(fun
					      (_, Out) -> [random:uniform(10000)|Out]
					 end, [], lists:seq(1,100)),
		      Self ! {seed_test, Rands}
	      end),
    Rands2 = receive {seed_test, R2s} -> R2s end,
    Rands1 = Rands2,
    ok.


%% Check that uniform/1 returns values within the proper interval.
interval_1(Config) when is_list(Config) ->
    Top = 7,
    N = 10,
    check_interval(N, Top),
    ok.

check_interval(0, _) -> ok;
check_interval(N, Top) ->
    X = random:uniform(Top),
    if
	X < 1 ->
	    ct:fail(too_small);
	X > Top ->
	    ct:fail(too_large);
	true ->
	    ok
    end,
    check_interval(N-1, Top).
