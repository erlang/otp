%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
-module(group_timetrap_3_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").


%%%-----------------------------------------------------------------
%%% CONFIG FUNCS
%%%-----------------------------------------------------------------

init_per_testcase(TestCase, Config) ->
    Info = case catch apply(?MODULE,TestCase,[]) of 
	       {'EXIT',_} -> [];
	       I -> I
	   end,
    ct:comment(io_lib:format("init( ~w ): ~p", [TestCase, Info])),    
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%------------------------------------------------------------------
%%% TEST DECLARATIONS
%%%------------------------------------------------------------------

groups() ->
    [{g1,[],[t11]},
     {g2,[],[t21]},
     {g3,[],[{group,g4},t31,{group,g5}]},

       {g4,[],[t41]},
       {g5,[],[t51]},

     {g6,[],[t61]},
     {g7,[],[{group,g8},t71,{group,g9}]},

       {g8,[],[t81]},
       {g9,[],[t91]},

     {g10,[],[t101]},
     {g11,[],[t111]}
    ].
     

all() -> 
    [t1,
     {group,g1},
     {group,g2},
     t2,
     {group,g3},
     t3,
     {group,g6},
     {group,g7},
     {group,g10},
     {group,g11}
    ].

%%%-----------------------------------------------------------------
%%% INFO FUNCS
%%%-----------------------------------------------------------------

suite() -> [{timetrap,1000}].

group(g1) -> [{timetrap,500}];

group(g2) -> [{timetrap,1500}];

group(g3) -> [{timetrap,500}];

group(g4) -> [{timetrap,250}];

group(g5) -> [{timetrap,1500}];

group(g6) -> [{timetrap,250}];

group(g7) -> [{timetrap,250}];

group(g8) -> [{timetrap,250}];

group(G) when G /= g11 -> [].

t3() -> [{timetrap,250}].

t61() -> [{timetrap,500}].

t71() -> [{timetrap,500}].

t81() -> [{timetrap,750}].

t91() -> [{timetrap,250}].

%%%------------------------------------------------------------------
%%% TEST CASES
%%%------------------------------------------------------------------

t1(_) ->
    ct:sleep(3000),
    exit(should_timeout).

t11(_) ->
    ct:sleep(750),
    exit(should_timeout).

t21(_) ->
    ct:sleep(3000),
    exit(should_timeout).

t2(_) ->
    ct:sleep(1250),
    exit(should_timeout).

t31(_) ->
    ct:sleep(750),
    exit(should_timeout).

t41(_) ->
    ct:sleep(350),
    exit(should_timeout).

t51(_) ->
    ct:sleep(2000),
    exit(should_timeout).

t3(_) ->
    ct:sleep(500),
    exit(should_timeout).

t61(_) ->
    ct:sleep(750),
    exit(should_timeout).

t71(_) ->
    ct:sleep(750),
    exit(should_timeout).

t81(_) ->
    ct:sleep(1000),
    exit(should_timeout).

t91(_) ->
    ct:sleep(350),
    exit(should_timeout).

t101(_) ->
    ct:sleep(1500),
    exit(should_timeout).

t111(_) ->
    ct:sleep(1500),
    exit(should_timeout).


