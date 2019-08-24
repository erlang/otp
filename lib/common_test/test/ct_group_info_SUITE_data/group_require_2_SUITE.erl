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
-module(group_require_2_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").


%%%-----------------------------------------------------------------
%%% CONFIG FUNCS
%%%-----------------------------------------------------------------

init_per_group(G, Config) ->
    GrProps = proplists:get_value(tc_group_properties, Config),
    _GrProps1 = proplists:delete(name, GrProps),
    Info = case catch group(G) of {'EXIT',_} -> []; I -> I end,
    ct:comment(io_lib:format("init( ~w ): ~p", [G, Info])),
    if Info /= [] -> verify_cfg(G); true -> ok end,
    Config.

end_per_group(G, Config) ->
    GrProps = proplists:get_value(tc_group_properties, Config),
    _GrProps1 = proplists:delete(name, GrProps),
    Info = case catch group(G) of {'EXIT',_} -> []; I -> I end,
    ct:comment(io_lib:format("end( ~w )", [G])),
    if Info /= [] -> verify_cfg(G); true -> ok end,
    ok.

init_per_testcase(t101, Config) ->
    Config;
init_per_testcase(t111, Config) ->
    Config;
init_per_testcase(TestCase, Config) ->
    GrProps = proplists:get_value(tc_group_properties, Config),
    GrProps1 = if GrProps == undefined  -> []; true -> GrProps end,
    verify_cfg(proplists:get_value(name, GrProps1)),
    if TestCase == t72 -> verify_cfg(TestCase); true -> ok end,
    Info = case catch apply(?MODULE,TestCase,[]) of 
	       {'EXIT',_} -> [];
	       I -> I
	   end,
    ct:comment(io_lib:format("init( ~w ): ~p", [TestCase, Info])),    
    Config.

end_per_testcase(t101, Config) ->
    ok;
end_per_testcase(t111, Config) ->
    ok;
end_per_testcase(TestCase, Config) ->
    GrProps = proplists:get_value(tc_group_properties, Config),
    GrProps1 = if GrProps == undefined  -> []; true -> GrProps end,
    verify_cfg(proplists:get_value(name, GrProps1)),
    if TestCase == t72 -> verify_cfg(TestCase); true -> ok end,
    ok.

verify_cfg(undefined) ->
    ok;
verify_cfg(Name) ->
    Key = list_to_atom(atom_to_list(Name) ++ "_cfg"),
    Alias = list_to_atom(atom_to_list(Name) ++ "_cfg_alias"),
    Val = list_to_atom(atom_to_list(Name) ++ "_cfg_val"),
    ct:pal("Reading ~p & ~p. Expecting ~p.", [Key,Alias,Val]),
    Val = ct:get_config(Key),
    Val = ct:get_config(Alias),
    suite_cfg_val = ct:get_config(suite_cfg),
    suite_cfg_val = ct:get_config(suite_cfg_alias).
    
    

%%%------------------------------------------------------------------
%%% TEST DECLARATIONS
%%%------------------------------------------------------------------

groups() ->
    [{g1,[],[t11]},
     {g2,[],[t21]},
     {g3,[],[t31]},
     {g4,[],[t41]},

     {g5,[],[{group,g6},t51,{group,g7}]},

       {g6,[],[t61]},
       {g7,[],[t71,t72]},

     {g8,[],[t81]},
     {g9,[],[t91]},
     {g10,[],[t101]},
     {g11,[],[t111]}
    ].
     

all() -> 
    [t1,
     {group,g1},
     {group,g2},
     {group,g3},
     {group,g4},
     {group,g5},
     {group,g8},
     {group,g9},
     {group,g10},
     {group,g11}
    ].

%%%-----------------------------------------------------------------
%%% INFO FUNCS
%%%-----------------------------------------------------------------

suite() -> [{require,suite_cfg},
	    {require,suite_cfg_alias,suite_cfg},
	    {require,common1},
	    {default_config,suite_cfg,suite_cfg_val},
	    {default_config,common1,common1_val}].

group(g1) -> [{require,g1_cfg},
	      {require,g1_cfg_alias,g1_cfg},
	      {default_config,g1_cfg,g1_cfg_val}];

group(g2) -> [{require,g2_cfg},
	      {require,g2_cfg_alias,g2_cfg},
	      {require,common1},
	      {require,common2},
	      {default_config,g2_cfg,g2_cfg_val},
	      {default_config,common1,common1_val},
	      {default_config,common2,common2_val}];

group(g3) -> [{require,g3_cfg},
	      {require,g3_cfg_alias,g3_cfg},
	      {require,common2},
	      {require,common2_alias,common2},
	      {default_config,g3_cfg,g3_cfg_val},
	      {default_config,common2,common2_val}];

group(g4) -> [{require,g4_cfg},
	      {require,g4_cfg_alias,g4_cfg},
	      {require,common2_alias,common3},
	      {default_config,g4_cfg,g4_cfg_val}];

group(g5) -> [{require,g5_cfg},
	      {require,g5_cfg_alias,g5_cfg},
	      {default_config,g5_cfg,g5_cfg_val}];

group(g6) -> [{require,g6_cfg},
	      {require,g6_cfg_alias,g6_cfg},
	      {default_config,g6_cfg,g6_cfg_val}];

group(g7) -> [{require,g7_cfg},
	      {require,g7_cfg_alias,g7_cfg},
	      {default_config,g7_cfg,g7_cfg_val}];

group(g8) -> [{require,non_existing}];

group(g9) -> [{require,g9_cfg},
	      {require,g9_cfg_alias,g9_cfg},
	      {default_config,g9_cfg,g9_cfg_val}];

group(G) when G /= g11 -> [].

t72() -> [{require,t72_cfg},
	  {require,t72_cfg_alias,t72_cfg},
	  {default_config,t72_cfg,t72_cfg_val}].

t91() -> [{require,non_existing}].
    

%%%------------------------------------------------------------------
%%% TEST CASES
%%%------------------------------------------------------------------

t1(_) ->
    suite_cfg_val = ct:get_config(suite_cfg).

t11(_) ->
    suite_cfg_val = ct:get_config(suite_cfg),
    suite_cfg_val = ct:get_config(suite_cfg_alias),
    g1_cfg_val = ct:get_config(g1_cfg),
    g1_cfg_val = ct:get_config(g1_cfg_alias).

t21(_) ->
    suite_cfg_val = ct:get_config(suite_cfg),
    g2_cfg_val = ct:get_config(g2_cfg),
    g2_cfg_val = ct:get_config(g2_cfg_alias),
    common1_val = ct:get_config(common1),
    common2_val = ct:get_config(common2).

t31(_) ->
    suite_cfg_val = ct:get_config(suite_cfg),
    g3_cfg_val = ct:get_config(g3_cfg),
    g3_cfg_val = ct:get_config(g3_cfg_alias),
    common2_val = ct:get_config(common2),
    common2_val = ct:get_config(common2_alias).

t41(_) ->
    exit(should_be_skipped).

t51(_) ->
    suite_cfg_val = ct:get_config(suite_cfg),
    g5_cfg_val = ct:get_config(g5_cfg),
    g5_cfg_val = ct:get_config(g5_cfg_alias).

t61(_) ->
    suite_cfg_val = ct:get_config(suite_cfg),
    g5_cfg_val = ct:get_config(g5_cfg),
    g5_cfg_val = ct:get_config(g5_cfg_alias),
    g6_cfg_val = ct:get_config(g6_cfg),
    g6_cfg_val = ct:get_config(g6_cfg_alias).

t71(_) ->
    suite_cfg_val = ct:get_config(suite_cfg),
    g5_cfg_val = ct:get_config(g5_cfg),
    g5_cfg_val = ct:get_config(g5_cfg_alias),
    g7_cfg_val = ct:get_config(g7_cfg),
    g7_cfg_val = ct:get_config(g7_cfg_alias).

t72(_) ->
    suite_cfg_val = ct:get_config(suite_cfg),
    g5_cfg_val = ct:get_config(g5_cfg),
    g5_cfg_val = ct:get_config(g5_cfg_alias),
    g7_cfg_val = ct:get_config(g7_cfg),
    g7_cfg_val = ct:get_config(g7_cfg_alias),
    t72_cfg_val = ct:get_config(t72_cfg).

t81(_) ->
    exit(should_be_skipped).

t91(_) ->
    exit(should_be_skipped).

t101(_) ->
    suite_cfg_val = ct:get_config(suite_cfg).

t111(_) ->
    suite_cfg_val = ct:get_config(suite_cfg).


