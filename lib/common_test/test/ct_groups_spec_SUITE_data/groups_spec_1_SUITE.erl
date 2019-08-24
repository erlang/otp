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
-module(groups_spec_1_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% INFO FUNCS
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,1000}].

group(_) ->
    [{timetrap,2000}].

%%--------------------------------------------------------------------
%% CONFIG FUNCS
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(G, Config) ->
    GrProps = proplists:get_value(tc_group_properties, Config),
    GrProps1 = proplists:delete(name, GrProps),
    ct:comment(io_lib:format("init( ~w ): ~p", [G, GrProps1])),
    ct:pal("init( ~w ): ~p", [G, GrProps1]),
    Config.

end_per_group(G, Config) ->
    GrProps = proplists:get_value(tc_group_properties, Config),
    GrProps1 = proplists:delete(name, GrProps),
    ct:comment(io_lib:format("end( ~w ): ~p", [G, GrProps1])),
    ct:pal("end( ~w ): ~p", [G, GrProps1]),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% TEST DECLARATIONS
%%--------------------------------------------------------------------

groups() ->
    [
     {g1,[],[t11,t12,t13]},
     {g2,[sequence],[t21,{group,g3},t22,{group,g4},t23]},
     {g3,[parallel],[t31,t32,t33]},
     {g4,[],[t41,{group,g5},t42]},
     {g5,[parallel],[t51,t52,t53,t54]}
    ].

all() -> 
    [
     {group,g1,default},
     {group,g1,[sequence]},
     {group,g1,[parallel],[]},
     
     {group,g2,[],[]},
     {group,g2,default,[{g3,[sequence]}]},
     {group,g2,[],[{g4,[sequence],[{g5,[sequence]}]},{g3,[sequence]}]}
    ].

%%-----------------------------------------------------------------
%% TEST CASES
%%-----------------------------------------------------------------

t11(_) ->
    ok.
t12(_) ->
    exit(crashes).
t13(_) ->
    ok.

t21(_) ->
    ok.
t22(_) ->
    exit(crashes).
t23(_) ->
    ok.

t31(_) ->
    ok.
t32(_) ->
    exit(crashes).
t33(_) ->
    ok.

t41(_) ->
    ok.	
t42(_) ->
    exit(crashes).

t51(_) ->
    ok.
t52(_) ->
    ct:sleep(3000).
t53(_) ->
    exit(crashes).
t54(_) ->
    ok.
