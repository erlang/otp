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
-module(systools_rc_SUITE).

-include_lib("test_server/include/test_server.hrl").
-include_lib("sasl/src/systools.hrl").
-export([all/0,groups/0,init_per_group/2,end_per_group/2, 
	 syntax_check/1, translate/1, translate_app/1,
	 translate_emulator_restarts/1]).

%%-----------------------------------------------------------------
%% erl -compile systools_rc_SUITE @i ../src/ @i ../../test_server/include/
%% c(systools_rc_SUITE, [{i, "../src"}, {i, "../../test_server/include"}]).
%%-----------------------------------------------------------------
all() -> 
    [syntax_check, translate, translate_app, translate_emulator_restarts].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


syntax_check(suite) -> [];
syntax_check(Config) when is_list(Config) ->
    PreApps =
	[#application{name = test,
		      description = "TEST",
		      vsn = "0.1",
		      modules = [{foo,1},{bar,1},{baz,1},{old_mod,1}],
		      regs = [],
		      mod = {sasl, []}},
	 #application{name = snmp,
		      description = "SNMP",
		      vsn = "1.0",
		      modules = [snmp],
		      regs = [],
		      mod = {snmp, []}}],
    Apps =
	[#application{name = test,
		      description = "TEST",
		      vsn = "1.0",
		      modules = [{foo,1},{bar,1},{baz,1},{new_mod,1}],
		      regs = [],
		      mod = {sasl, []}}],
    S1 = [
	  {update, bar, {advanced, extra}, brutal_purge, brutal_purge, []},
	  {update, foo, soft, soft_purge, soft_purge, [bar]},
	  {update, baz, 5000, soft, brutal_purge, brutal_purge, []},
	  {add_module, new_mod},
	  {remove_application, snmp}
	  ],
    ?line {ok, _} = systools_rc:translate_scripts([S1], Apps, PreApps),
    S2 = [
	  {apply, {m, f, [a]}},
	  {load_object_code, {tst, "1.0", [new_mod]}},
          point_of_no_return,
	  {update, bar, {advanced, extra}, brutal_purge, brutal_purge, []},
	  {update, foo, soft, soft_purge, soft_purge, [bar]},
	  {load, {new_mod, soft_purge, soft_purge}},
	  {remove, {old_mod, soft_purge, soft_purge}},
	  {purge, [m1, m2]},
	  {suspend, [m1]},
	  {code_change, [{m1, extra}]},
	  {resume, [m1]},
	  {stop, [m3,m4]},
	  {start, [m3,m4]},
	  {sync_nodes, id1, {m, f, [a]}},
	  {sync_nodes, id2, [cp1, cp2]},
	  {apply, {m,f,[a]}},
	  restart_new_emulator,
	  restart_emulator
	  ],
    ?line {ok, _} = systools_rc:translate_scripts([S2], Apps, []),
    S3 = [{apply, {m, f, a}}],
    ?line {error, _, _} = systools_rc:translate_scripts([S3], Apps, []),
    S3_1 = [{apply, {m, 3, a}}],
    ?line {error, _, _} = systools_rc:translate_scripts([S3_1], Apps, []),
    S4 = [{apply, {m, f}}],
    ?line {error, _, _} = systools_rc:translate_scripts([S4], Apps, []),
    S5 = [{load_object_code, hej}],
    ?line {error, _, _} = systools_rc:translate_scripts([S5], Apps, []),
    S6 = [{load_object_code, {342, "1.0", [foo]}}],
    ?line {error, _, _} = systools_rc:translate_scripts([S6], Apps, []),
    S7 = [{load_object_code, {tets, "1.0", foo}}],
    ?line {error, _, _} = systools_rc:translate_scripts([S7], Apps, []),
    S8 = [{suspend, [m1]}, point_of_no_return],
    ?line {error, _, _} = systools_rc:translate_scripts([S8], Apps, []),
    S9 = [{update, ba, {advanced, extra}, brutal_purge, brutal_purge, []}],
    ?line {error, _, _} = systools_rc:translate_scripts([S9], Apps, []),
    S10 = [{update, bar, {advanced, extra}, brutal_purge, brutal_purge, [baz]}],
    ?line {error, _, _} = systools_rc:translate_scripts([S10], Apps, []),
    S11 = [{update, bar, {advanced, extra}, brutal_purge, brutal_purge, [ba]}],
    ?line {error, _, _} = systools_rc:translate_scripts([S11], Apps, []),
    S12 = [{update, bar, advanced, brutal_purge, brutal_purge, []}],
    ?line {error, _, _} = systools_rc:translate_scripts([S12], Apps, []),
    S13 = [{update, bar, {advanced, extra}, rutal_purge, brutal_purge, [ba]}],
    ?line {error, _, _} = systools_rc:translate_scripts([S13], Apps, []),
    S14 = [{update, bar, {advanced, extra}, brutal_purge, rutal_purge, [ba]}],
    ?line {error, _, _} = systools_rc:translate_scripts([S14], Apps, []),
    S15 = [{update, bar, {advanced, extra}, brutal_purge, brutal_purge, ba}],
    ?line {error, _, _} = systools_rc:translate_scripts([S15], Apps, []),
    S16 = [{code_change, [module]}],
    ?line {error, _, _} = systools_rc:translate_scripts([S16], Apps, []),
    ?line ok.

translate(suite) -> [];
translate(Config) when is_list(Config) ->
    Apps =
	[#application{name = test,
		      description = "TEST",
		      vsn = "1.0",
		      modules = [{foo,1},{bar,1},{baz,1},
				 {x,1},{y,1},{z,1}],
		      regs = [],
		      mod = {sasl, []}}],
    %% Simple translation (1)
    Up1 = [{update, foo, soft, soft_purge, soft_purge, []}],
    ?line {ok, X1} = systools_rc:translate_scripts([Up1], Apps, []),
    ?line [{load_object_code, {test,"1.0",[foo]}},
	   point_of_no_return,
	   {suspend,[foo]},
	   {load,{foo,soft_purge,soft_purge}},
	   {resume,[foo]}] = X1,

    %% Simple translation (2)
    Up2 = [{update, foo, {advanced, extra}, soft_purge, soft_purge, []}],
    ?line {ok, X2} = systools_rc:translate_scripts([Up2], Apps, []),
    ?line [{load_object_code, {test,"1.0",[foo]}},
	   point_of_no_return,
	   {suspend,[foo]},
	   {load,{foo,soft_purge,soft_purge}},
	   {code_change, up, [{foo, extra}]},
	   {resume,[foo]}] = X2,

    ?line {ok, X22} = systools_rc:translate_scripts(dn,[Up2], Apps, []),
    ?line [{load_object_code, {test,"1.0",[foo]}},
	   point_of_no_return,
	   {suspend,[foo]},
	   {code_change, down, [{foo, extra}]},
	   {load,{foo,soft_purge,soft_purge}},
	   {resume,[foo]}] = X22,

    Up2a = [{update, foo, static, default, {advanced,extra},
	     soft_purge, soft_purge, []}],
    ?line {ok, X2a} = systools_rc:translate_scripts([Up2a], Apps, []),
    ?line [{load_object_code, {test,"1.0",[foo]}},
	   point_of_no_return,
	   {suspend,[foo]},
	   {load,{foo,soft_purge,soft_purge}},
	   {code_change, up, [{foo, extra}]},
	   {resume,[foo]}] = X2a,

    ?line {ok, X22a} = systools_rc:translate_scripts(dn,[Up2a], Apps, []),
    ?line [{load_object_code, {test,"1.0",[foo]}},
	   point_of_no_return,
	   {suspend,[foo]},
	   {load,{foo,soft_purge,soft_purge}},
	   {code_change, down, [{foo, extra}]},
	   {resume,[foo]}] = X22a,

    %% Simple dependency (1)
    Up3 = [{update, foo, soft, soft_purge, soft_purge, [bar]},
	   {update, bar, soft, soft_purge, soft_purge, []}],
    ?line {ok, X31} = systools_rc:translate_scripts([Up3], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,bar]}},
	   point_of_no_return,
	   {suspend,[foo,bar]},
	   {load,{bar,soft_purge,soft_purge}},
	   {load,{foo,soft_purge,soft_purge}},
	   {resume,[bar,foo]}] = X31,
    ?line {ok, X32} = systools_rc:translate_scripts(dn,[Up3], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,bar]}},
	   point_of_no_return,
	   {suspend,[foo,bar]},
	   {load,{foo,soft_purge,soft_purge}},
	   {load,{bar,soft_purge,soft_purge}},
	   {resume,[bar,foo]}] = X32,

    Up3a = [{update, foo, static, default, soft, soft_purge, soft_purge, [bar]},
	    {update, bar, static, default, soft, soft_purge, soft_purge, []}],
    ?line {ok, X3a1} = systools_rc:translate_scripts([Up3a], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,bar]}},
	   point_of_no_return,
	   {suspend,[foo, bar]},
	   {load,{bar,soft_purge,soft_purge}},
	   {load,{foo,soft_purge,soft_purge}},
	   {resume,[bar,foo]}] = X3a1,
    ?line {ok, X3a2} = systools_rc:translate_scripts(dn,[Up3a], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,bar]}},
	   point_of_no_return,
	   {suspend,[foo,bar]},
	   {load,{foo,soft_purge,soft_purge}},
	   {load,{bar,soft_purge,soft_purge}},
	   {resume,[bar,foo]}] = X3a2,

    %%  Simple dependency (2)
    Up4 = [{update, foo, soft, soft_purge, soft_purge, [bar]},
	   {update, bar, {advanced, []}, soft_purge, soft_purge, []}],
    ?line {ok, X4} = systools_rc:translate_scripts(up,[Up4], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,bar]}},
	   point_of_no_return,
	   {suspend,[foo,bar]},
	   {load,{bar,soft_purge,soft_purge}},
	   {load,{foo,soft_purge,soft_purge}},
	   {code_change,up,[{bar,[]}]},
	   {resume,[bar,foo]}] = X4,

    ?line {ok, X42} = systools_rc:translate_scripts(dn,[Up4], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,bar]}},
	   point_of_no_return,
	   {suspend,[foo,bar]},
	   {code_change,down,[{bar,[]}]},
	   {load,{foo,soft_purge,soft_purge}},
	   {load,{bar,soft_purge,soft_purge}},
	   {resume,[bar,foo]}] = X42,

    Up4a = [{update, foo, soft, soft_purge, soft_purge, [bar]},
	    {update, bar, static, infinity, {advanced, []},
	     soft_purge, soft_purge, []}],
    ?line {ok, X4a} = systools_rc:translate_scripts(up,[Up4a], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,bar]}},
	   point_of_no_return,
	   {suspend,[foo,{bar,infinity}]},
	   {load,{bar,soft_purge,soft_purge}},
	   {load,{foo,soft_purge,soft_purge}},
	   {code_change,up,[{bar,[]}]},
	   {resume,[bar,foo]}] = X4a,

    ?line {ok, X42a} = systools_rc:translate_scripts(dn,[Up4a], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,bar]}},
	   point_of_no_return,
	   {suspend,[foo,{bar,infinity}]},
	   {load,{foo,soft_purge,soft_purge}},
	   {load,{bar,soft_purge,soft_purge}},
	   {code_change,down,[{bar,[]}]},
	   {resume,[bar,foo]}] = X42a,

    Up4b = [{update, foo, soft, soft_purge, soft_purge, [bar]},
	    {update, bar, dynamic, infinity, {advanced, []},
	     soft_purge, soft_purge, []}],
    ?line {ok, X4b} = systools_rc:translate_scripts(up,[Up4b], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,bar]}},
	   point_of_no_return,
	   {suspend,[foo,{bar,infinity}]},
	   {load,{bar,soft_purge,soft_purge}},
	   {load,{foo,soft_purge,soft_purge}},
	   {code_change,up,[{bar,[]}]},
	   {resume,[bar,foo]}] = X4b,

    ?line {ok, X42b} = systools_rc:translate_scripts(dn,[Up4b], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,bar]}},
	   point_of_no_return,
	   {suspend,[foo,{bar,infinity}]},
	   {code_change,down,[{bar,[]}]},
	   {load,{foo,soft_purge,soft_purge}},
	   {load,{bar,soft_purge,soft_purge}},
	   {resume,[bar,foo]}] = X42b,

    %% More complex dependency
    Up5 = [{update, foo, soft, soft_purge, soft_purge, [bar, baz]},
	   {update, bar, {advanced, []}, soft_purge, soft_purge, []},
	   {update, baz, {advanced, baz}, soft_purge, soft_purge, [bar]}],
    ?line {ok, X5} = systools_rc:translate_scripts([Up5], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,baz,bar]}},
	   point_of_no_return,
	   {suspend,[foo,baz,bar]},
	   {load,{bar,soft_purge,soft_purge}},
	   {load,{baz,soft_purge,soft_purge}},
	   {load,{foo,soft_purge,soft_purge}},
	   {code_change,up,[{baz,baz},{bar,[]}]},
	   {resume,[bar,baz,foo]}] = X5,

    ?line {ok, X52} = systools_rc:translate_scripts(dn,[Up5], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,baz,bar]}},
	   point_of_no_return,
	   {suspend,[foo,baz,bar]},
	   {code_change,down,[{baz,baz},{bar,[]}]},
	   {load,{foo,soft_purge,soft_purge}},
	   {load,{baz,soft_purge,soft_purge}},
	   {load,{bar,soft_purge,soft_purge}},
	   {resume,[bar,baz,foo]}] = X52,

    Up5a = [{update, foo, dynamic, infinity, soft, soft_purge,
	     soft_purge, [bar, baz]},
	    {update, bar, static, 7000, {advanced, []}, soft_purge,
	     soft_purge, []},
	    {update, baz, dynamic, default, {advanced, baz}, soft_purge,
	     soft_purge, [bar]}],
    ?line {ok, X5a} = systools_rc:translate_scripts([Up5a], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,baz,bar]}},
	   point_of_no_return,
	   {suspend,[{foo,infinity},baz,{bar,7000}]},
	   {load,{bar,soft_purge,soft_purge}},
	   {load,{baz,soft_purge,soft_purge}},
	   {load,{foo,soft_purge,soft_purge}},
	   {code_change,up,[{baz,baz}, {bar,[]}]},
	   {resume,[bar,baz,foo]}] = X5a,

    ?line {ok, X52a} = systools_rc:translate_scripts(dn,[Up5a], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,baz,bar]}},
	   point_of_no_return,
	   {suspend,[{foo,infinity},baz,{bar,7000}]},
	   {code_change,down,[{baz,baz}]},
	   {load,{foo,soft_purge,soft_purge}},
	   {load,{baz,soft_purge,soft_purge}},
	   {load,{bar,soft_purge,soft_purge}},
	   {code_change,down,[{bar,[]}]},
	   {resume,[bar,baz,foo]}] = X52a,

    Up5b = [{update, foo, dynamic, infinity, soft, soft_purge,
	     soft_purge, [bar, baz]},
	    {update, bar, dynamic, 7000, {advanced, []}, soft_purge,
	     soft_purge, []},
	    {update, baz, static, default, {advanced, baz}, soft_purge,
	     soft_purge, [bar]}],
    ?line {ok, X5b} = systools_rc:translate_scripts([Up5b], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,baz,bar]}},
	   point_of_no_return,
	   {suspend,[{foo,infinity},baz,{bar,7000}]},
	   {load,{bar,soft_purge,soft_purge}},
	   {load,{baz,soft_purge,soft_purge}},
	   {load,{foo,soft_purge,soft_purge}},
	   {code_change,up,[{baz,baz},{bar,[]}]},
	   {resume,[bar,baz,foo]}] = X5b,

    ?line {ok, X52b} = systools_rc:translate_scripts(dn,[Up5b], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,baz,bar]}},
	   point_of_no_return,
	   {suspend,[{foo,infinity},baz,{bar,7000}]},
	   {code_change,down,[{bar,[]}]},
	   {load,{foo,soft_purge,soft_purge}},
	   {load,{baz,soft_purge,soft_purge}},
	   {load,{bar,soft_purge,soft_purge}},
	   {code_change,down,[{baz,baz}]},
	   {resume,[bar,baz,foo]}] = X52b,

    %% Simple circular dependency (1)
    Up6 = [{update, foo, soft, soft_purge, soft_purge, [bar]},
	   {update, bar, soft, soft_purge, soft_purge, [foo]}],
    ?line {ok, X61} = systools_rc:translate_scripts([Up6], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,bar]}},
	   point_of_no_return,
	   {suspend,[foo,bar]},
	   {load,{bar,soft_purge,soft_purge}},
	   {load,{foo,soft_purge,soft_purge}},
	   {resume,[bar,foo]}] = X61,
    ?line {ok, X62} = systools_rc:translate_scripts(dn,[Up6], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,bar]}},
	   point_of_no_return,
	   {suspend,[foo,bar]},
	   {load,{foo,soft_purge,soft_purge}},
	   {load,{bar,soft_purge,soft_purge}},
	   {resume,[bar,foo]}] = X62,

    %% Simple circular dependency (2)
    Up7 = [{update, foo, soft, soft_purge, soft_purge, [bar, baz]},
	   {update, bar, soft, soft_purge, soft_purge, [foo]},
	   {update, baz, soft, soft_purge, soft_purge, [bar]}],
    ?line {ok, X71} = systools_rc:translate_scripts([Up7], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,bar,baz]}},
	   point_of_no_return,
	   {suspend,[foo,bar,baz]},
	   {load,{baz,soft_purge,soft_purge}},
	   {load,{bar,soft_purge,soft_purge}},
	   {load,{foo,soft_purge,soft_purge}},
	   {resume,[baz, bar, foo]}] = X71,
    ?line {ok, X72} = systools_rc:translate_scripts(dn,[Up7], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,bar,baz]}},
	   point_of_no_return,
	   {suspend,[foo,bar,baz]},
	   {load,{foo,soft_purge,soft_purge}},
	   {load,{bar,soft_purge,soft_purge}},
	   {load,{baz,soft_purge,soft_purge}},
	   {resume,[baz,bar,foo]}] = X72,

    %% Complex circular dependencies, check only order
    %%
    Up8 = [{update, foo, soft, soft_purge, soft_purge, [baz]},
	   {update, y, soft, soft_purge, soft_purge, [bar]},
	   {update, x, soft, soft_purge, soft_purge, [y, z]},
	   {update, z, soft, soft_purge, soft_purge, [x]},
	   {update, bar, soft, soft_purge, soft_purge, [baz]},
	   {update, baz, soft, soft_purge, soft_purge, [bar]}],
    ?line {ok, X8} = systools_rc:translate_scripts([Up8], Apps, []),
    ?line {value, {suspend, Order}} = lists:keysearch(suspend, 1, X8),
    ?line Rest = case lists:reverse(Order) of
		     [bar, baz | R] -> R;
		     [baz, bar | R] -> R
		 end,
    ?line case Rest of
	      [y, z, x, foo] -> ok;
	      [y, x, z, foo] -> ok;
	      [foo, y, z, x] -> ok;
	      [foo, y, x, z] -> ok;
	      [y, foo, z, x] -> ok;
	      [y, foo, x, z] -> ok
	  end,

    %% Check that order among other instructions isn't changed
    Up9 = [{update, foo, soft, soft_purge, soft_purge, [baz]},
	   {apply, {m, f, [1]}},
	   {update, y, soft, soft_purge, soft_purge, [bar]},
	   {apply, {m, f, [2]}},
	   {update, x, soft, soft_purge, soft_purge, [y, z]},
	   {apply, {m, f, [3]}},
	   {update, z, soft, soft_purge, soft_purge, [x]},
	   {apply, {m, f, [4]}},
	   {update, bar, soft, soft_purge, soft_purge, [baz]},
	   {apply, {m, f, [5]}},
	   {update, baz, soft, soft_purge, soft_purge, [bar]},
	   {apply, {m, f, [6]}}],
    ?line {ok, X9} = systools_rc:translate_scripts([Up9], Apps, []),
    Other2 = [X || {apply, {m, f, [X]}} <- X9],
    ?line [1,2,3,4,5,6] = lists:sort(Other2),
    ?line ok.


translate_app(suite) -> [];
translate_app(Config) when is_list(Config) ->
    PreApps =
	[#application{name = test,
		      description = "TEST",
		      vsn = "1.0",
		      modules = [{foo,1},{bar,1},{baz,1}],
		      regs = [],
		      mod = {sasl, []}},
	 #application{name = pelle,
		      description = "PELLE",
		      vsn = "1.0",
		      modules = [pelle, kalle],
		      regs = [],
		      mod = {pelle, []}}],
    Apps =
	[#application{name = test,
		      description = "TEST",
		      vsn = "1.0",
		      modules = [{foo,1},{bar,1},{baz,1}],
		      regs = [],
		      mod = {sasl, []}}],
    %% Simple translation (1)
    Up1 = [{add_module, foo},
	   {add_module, bar}],
    ?line {ok, X1} = systools_rc:translate_scripts([Up1], Apps, []),
    ?line [{load_object_code,{test,"1.0",[foo,bar]}},
	   point_of_no_return,
	   {load,{foo,brutal_purge,brutal_purge}},
	   {load,{bar,brutal_purge,brutal_purge}}] = X1,

    %% Simple translation (2)
    Up2 = [{add_application, test}],
    ?line {ok, X2} = systools_rc:translate_scripts([Up2], Apps, []),
io:format("X2=~p~n", [X2]),
    ?line [{load_object_code,{test,"1.0",[foo,bar,baz]}},
	   point_of_no_return,
	   {load,{foo,brutal_purge,brutal_purge}},
	   {load,{bar,brutal_purge,brutal_purge}},
	   {load,{baz,brutal_purge,brutal_purge}},
	   {apply,{application,start,[test,permanent]}}] = X2,

    %% Simple translation (3)
    Up3 = [{remove_application, pelle}],
    ?line {ok, X3} = systools_rc:translate_scripts([Up3], Apps, PreApps),
    ?line [point_of_no_return,
	   {apply,{application,stop,[pelle]}},
	   {remove,{pelle,brutal_purge,brutal_purge}},
	   {remove,{kalle,brutal_purge,brutal_purge}},
	   {purge,[pelle,kalle]},
	   {apply,{application,unload,[pelle]}}] = X3,
    ?line ok.


translate_emulator_restarts(_Config) ->
    Apps =
	[#application{name = test,
		      description = "TEST",
		      vsn = "1.0",
		      modules = [{foo,1},{bar,1},{baz,1}],
		      regs = [],
		      mod = {sasl, []}},
	 #application{name = test,
		      description = "TEST2",
		      vsn = "1.0",
		      modules = [{x,1},{y,1},{z,1}],
		      regs = [],
		      mod = {sasl, []}}],
    %% restart_new_emulator
    Up1 = [{update, foo, soft, soft_purge, soft_purge, []},restart_new_emulator],
    ?line {ok, X1} = systools_rc:translate_scripts([Up1], Apps, []),
    ?line [restart_new_emulator,
	   {load_object_code, {test,"1.0",[foo]}},
	   point_of_no_return,
	   {suspend,[foo]},
	   {load,{foo,soft_purge,soft_purge}},
	   {resume,[foo]}] = X1,

    %% restart_emulator
    Up2 = [{update, foo, soft, soft_purge, soft_purge, []},restart_emulator],
    ?line {ok, X2} = systools_rc:translate_scripts([Up2], Apps, []),
    ?line [{load_object_code, {test,"1.0",[foo]}},
	   point_of_no_return,
	   {suspend,[foo]},
	   {load,{foo,soft_purge,soft_purge}},
	   {resume,[foo]},
	   restart_emulator] = X2,

    %% restart_emulator + restart_new_emulator
    Up3 = [{update, foo, soft, soft_purge, soft_purge, []},
	   restart_emulator,
	   restart_new_emulator],
    ?line {ok, X3} = systools_rc:translate_scripts([Up3], Apps, []),
    ?line [restart_new_emulator,
	   {load_object_code, {test,"1.0",[foo]}},
	   point_of_no_return,
	   {suspend,[foo]},
	   {load,{foo,soft_purge,soft_purge}},
	   {resume,[foo]},
	   restart_emulator] = X3,

    %% restart_emulator + restart_new_emulator
    Up4a = [{update, foo, soft, soft_purge, soft_purge, []},
	    restart_emulator,
	    restart_new_emulator],
    Up4b = [restart_new_emulator,
	    {update, x, soft, soft_purge, soft_purge, []},
	    restart_emulator,
	    restart_emulator],
    ?line {ok, X4} = systools_rc:translate_scripts([Up4a,Up4b], Apps, []),
    ?line [restart_new_emulator,
	   {load_object_code, {test,"1.0",[foo,x]}},
	   point_of_no_return,
	   {suspend,[foo]},
	   {load,{foo,soft_purge,soft_purge}},
	   {resume,[foo]},
	   {suspend,[x]},
	   {load,{x,soft_purge,soft_purge}},
	   {resume,[x]},
	   restart_emulator] = X4,

    %% only restart_new_emulator
    Up5 = [restart_new_emulator],
    ?line {ok, X5} = systools_rc:translate_scripts([Up5], Apps, []),
    ?line [restart_new_emulator,
	   point_of_no_return] = X5,

    %% only restart_emulator
    Up6 = [restart_emulator],
    ?line {ok, X6} = systools_rc:translate_scripts([Up6], Apps, []),
    ?line [point_of_no_return,
	   restart_emulator] = X6,

    ok.
