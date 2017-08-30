%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
-module(systools_rc_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("sasl/src/systools.hrl").
-export([all/0,groups/0,init_per_group/2,end_per_group/2, 
	 syntax_check/1, translate/1, translate_app/1,
	 translate_emulator_restarts/1,
	 translate_add_delete_module/1]).

all() -> 
    [syntax_check, translate, translate_app, translate_emulator_restarts,
     translate_add_delete_module].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


syntax_check(Config) when is_list(Config) ->
    PreApps =
	[#application{name = test,
		      description = "TEST",
		      vsn = "0.1",
		      modules = [foo,bar,baz,old_mod],
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
		      modules = [foo,bar,baz,new_mod],
		      regs = [],
		      mod = {sasl, []}}],
    S1 = [
	  {update, bar, {advanced, extra}, brutal_purge, brutal_purge, []},
	  {update, foo, soft, soft_purge, soft_purge, [bar]},
	  {update, baz, 5000, soft, brutal_purge, brutal_purge, []},
	  {add_module, new_mod},
	  {remove_application, snmp}
	 ],
    {ok, _} = systools_rc:translate_scripts([S1], Apps, PreApps),
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
    {ok, _} = systools_rc:translate_scripts([S2], Apps, []),
    S3 = [{apply, {m, f, a}}],
    {error, _, _} = systools_rc:translate_scripts([S3], Apps, []),
    S3_1 = [{apply, {m, 3, a}}],
    {error, _, _} = systools_rc:translate_scripts([S3_1], Apps, []),
    S4 = [{apply, {m, f}}],
    {error, _, _} = systools_rc:translate_scripts([S4], Apps, []),
    S5 = [{load_object_code, hej}],
    {error, _, _} = systools_rc:translate_scripts([S5], Apps, []),
    S6 = [{load_object_code, {342, "1.0", [foo]}}],
    {error, _, _} = systools_rc:translate_scripts([S6], Apps, []),
    S7 = [{load_object_code, {tets, "1.0", foo}}],
    {error, _, _} = systools_rc:translate_scripts([S7], Apps, []),
    S8 = [{suspend, [m1]}, point_of_no_return],
    {error, _, _} = systools_rc:translate_scripts([S8], Apps, []),
    S9 = [{update, ba, {advanced, extra}, brutal_purge, brutal_purge, []}],
    {error, _, _} = systools_rc:translate_scripts([S9], Apps, []),
    S10 = [{update, bar, {advanced, extra}, brutal_purge, brutal_purge, [baz]}],
    {error, _, _} = systools_rc:translate_scripts([S10], Apps, []),
    S11 = [{update, bar, {advanced, extra}, brutal_purge, brutal_purge, [ba]}],
    {error, _, _} = systools_rc:translate_scripts([S11], Apps, []),
    S12 = [{update, bar, advanced, brutal_purge, brutal_purge, []}],
    {error, _, _} = systools_rc:translate_scripts([S12], Apps, []),
    S13 = [{update, bar, {advanced, extra}, rutal_purge, brutal_purge, [ba]}],
    {error, _, _} = systools_rc:translate_scripts([S13], Apps, []),
    S14 = [{update, bar, {advanced, extra}, brutal_purge, rutal_purge, [ba]}],
    {error, _, _} = systools_rc:translate_scripts([S14], Apps, []),
    S15 = [{update, bar, {advanced, extra}, brutal_purge, brutal_purge, ba}],
    {error, _, _} = systools_rc:translate_scripts([S15], Apps, []),
    S16 = [{code_change, [module]}],
    {error, _, _} = systools_rc:translate_scripts([S16], Apps, []),
    ok.

translate(Config) when is_list(Config) ->
    Apps =
	[#application{name = test,
		      description = "TEST",
		      vsn = "1.0",
		      modules = [foo,bar,baz,
				 x,y,z],
		      regs = [],
		      mod = {sasl, []}}],
    %% Simple translation (1)
    Up1 = [{update, foo, soft, soft_purge, soft_purge, []}],
    {ok, X1} = systools_rc:translate_scripts([Up1], Apps, []),
    [{load_object_code, {test,"1.0",[foo]}},
     point_of_no_return,
     {suspend,[foo]},
     {load,{foo,soft_purge,soft_purge}},
     {resume,[foo]}] = X1,

    %% Simple translation (2)
    Up2 = [{update, foo, {advanced, extra}, soft_purge, soft_purge, []}],
    {ok, X2} = systools_rc:translate_scripts([Up2], Apps, []),
    [{load_object_code, {test,"1.0",[foo]}},
     point_of_no_return,
     {suspend,[foo]},
     {load,{foo,soft_purge,soft_purge}},
     {code_change, up, [{foo, extra}]},
     {resume,[foo]}] = X2,

    {ok, X22} = systools_rc:translate_scripts(dn,[Up2], Apps, []),
    [{load_object_code, {test,"1.0",[foo]}},
     point_of_no_return,
     {suspend,[foo]},
     {code_change, down, [{foo, extra}]},
     {load,{foo,soft_purge,soft_purge}},
     {resume,[foo]}] = X22,

    Up2a = [{update, foo, static, default, {advanced,extra},
	     soft_purge, soft_purge, []}],
    {ok, X2a} = systools_rc:translate_scripts([Up2a], Apps, []),
    [{load_object_code, {test,"1.0",[foo]}},
     point_of_no_return,
     {suspend,[foo]},
     {load,{foo,soft_purge,soft_purge}},
     {code_change, up, [{foo, extra}]},
     {resume,[foo]}] = X2a,

    {ok, X22a} = systools_rc:translate_scripts(dn,[Up2a], Apps, []),
    [{load_object_code, {test,"1.0",[foo]}},
     point_of_no_return,
     {suspend,[foo]},
     {load,{foo,soft_purge,soft_purge}},
     {code_change, down, [{foo, extra}]},
     {resume,[foo]}] = X22a,

    %% Simple dependency (1)
    Up3 = [{update, foo, soft, soft_purge, soft_purge, [bar]},
	   {update, bar, soft, soft_purge, soft_purge, []}],
    {ok, X31} = systools_rc:translate_scripts([Up3], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar]}},
     point_of_no_return,
     {suspend,[foo,bar]},
     {load,{bar,soft_purge,soft_purge}},
     {load,{foo,soft_purge,soft_purge}},
     {resume,[bar,foo]}] = X31,
    {ok, X32} = systools_rc:translate_scripts(dn,[Up3], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar]}},
     point_of_no_return,
     {suspend,[foo,bar]},
     {load,{foo,soft_purge,soft_purge}},
     {load,{bar,soft_purge,soft_purge}},
     {resume,[bar,foo]}] = X32,

    Up3a = [{update, foo, static, default, soft, soft_purge, soft_purge, [bar]},
	    {update, bar, static, default, soft, soft_purge, soft_purge, []}],
    {ok, X3a1} = systools_rc:translate_scripts([Up3a], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar]}},
     point_of_no_return,
     {suspend,[foo, bar]},
     {load,{bar,soft_purge,soft_purge}},
     {load,{foo,soft_purge,soft_purge}},
     {resume,[bar,foo]}] = X3a1,
    {ok, X3a2} = systools_rc:translate_scripts(dn,[Up3a], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar]}},
     point_of_no_return,
     {suspend,[foo,bar]},
     {load,{foo,soft_purge,soft_purge}},
     {load,{bar,soft_purge,soft_purge}},
     {resume,[bar,foo]}] = X3a2,

    %%  Simple dependency (2)
    Up4 = [{update, foo, soft, soft_purge, soft_purge, [bar]},
	   {update, bar, {advanced, []}, soft_purge, soft_purge, []}],
    {ok, X4} = systools_rc:translate_scripts(up,[Up4], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar]}},
     point_of_no_return,
     {suspend,[foo,bar]},
     {load,{bar,soft_purge,soft_purge}},
     {load,{foo,soft_purge,soft_purge}},
     {code_change,up,[{bar,[]}]},
     {resume,[bar,foo]}] = X4,

    {ok, X42} = systools_rc:translate_scripts(dn,[Up4], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar]}},
     point_of_no_return,
     {suspend,[foo,bar]},
     {code_change,down,[{bar,[]}]},
     {load,{foo,soft_purge,soft_purge}},
     {load,{bar,soft_purge,soft_purge}},
     {resume,[bar,foo]}] = X42,

    Up4a = [{update, foo, soft, soft_purge, soft_purge, [bar]},
	    {update, bar, static, infinity, {advanced, []},
	     soft_purge, soft_purge, []}],
    {ok, X4a} = systools_rc:translate_scripts(up,[Up4a], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar]}},
     point_of_no_return,
     {suspend,[foo,{bar,infinity}]},
     {load,{bar,soft_purge,soft_purge}},
     {load,{foo,soft_purge,soft_purge}},
     {code_change,up,[{bar,[]}]},
     {resume,[bar,foo]}] = X4a,

    {ok, X42a} = systools_rc:translate_scripts(dn,[Up4a], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar]}},
     point_of_no_return,
     {suspend,[foo,{bar,infinity}]},
     {load,{foo,soft_purge,soft_purge}},
     {load,{bar,soft_purge,soft_purge}},
     {code_change,down,[{bar,[]}]},
     {resume,[bar,foo]}] = X42a,

    Up4b = [{update, foo, soft, soft_purge, soft_purge, [bar]},
	    {update, bar, dynamic, infinity, {advanced, []},
	     soft_purge, soft_purge, []}],
    {ok, X4b} = systools_rc:translate_scripts(up,[Up4b], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar]}},
     point_of_no_return,
     {suspend,[foo,{bar,infinity}]},
     {load,{bar,soft_purge,soft_purge}},
     {load,{foo,soft_purge,soft_purge}},
     {code_change,up,[{bar,[]}]},
     {resume,[bar,foo]}] = X4b,

    {ok, X42b} = systools_rc:translate_scripts(dn,[Up4b], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar]}},
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
    {ok, X5} = systools_rc:translate_scripts([Up5], Apps, []),
    [{load_object_code,{test,"1.0",[foo,baz,bar]}},
     point_of_no_return,
     {suspend,[foo,baz,bar]},
     {load,{bar,soft_purge,soft_purge}},
     {load,{baz,soft_purge,soft_purge}},
     {load,{foo,soft_purge,soft_purge}},
     {code_change,up,[{baz,baz},{bar,[]}]},
     {resume,[bar,baz,foo]}] = X5,

    {ok, X52} = systools_rc:translate_scripts(dn,[Up5], Apps, []),
    [{load_object_code,{test,"1.0",[foo,baz,bar]}},
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
    {ok, X5a} = systools_rc:translate_scripts([Up5a], Apps, []),
    [{load_object_code,{test,"1.0",[foo,baz,bar]}},
     point_of_no_return,
     {suspend,[{foo,infinity},baz,{bar,7000}]},
     {load,{bar,soft_purge,soft_purge}},
     {load,{baz,soft_purge,soft_purge}},
     {load,{foo,soft_purge,soft_purge}},
     {code_change,up,[{baz,baz}, {bar,[]}]},
     {resume,[bar,baz,foo]}] = X5a,

    {ok, X52a} = systools_rc:translate_scripts(dn,[Up5a], Apps, []),
    [{load_object_code,{test,"1.0",[foo,baz,bar]}},
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
    {ok, X5b} = systools_rc:translate_scripts([Up5b], Apps, []),
    [{load_object_code,{test,"1.0",[foo,baz,bar]}},
     point_of_no_return,
     {suspend,[{foo,infinity},baz,{bar,7000}]},
     {load,{bar,soft_purge,soft_purge}},
     {load,{baz,soft_purge,soft_purge}},
     {load,{foo,soft_purge,soft_purge}},
     {code_change,up,[{baz,baz},{bar,[]}]},
     {resume,[bar,baz,foo]}] = X5b,

    {ok, X52b} = systools_rc:translate_scripts(dn,[Up5b], Apps, []),
    [{load_object_code,{test,"1.0",[foo,baz,bar]}},
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
    {ok, X61} = systools_rc:translate_scripts([Up6], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar]}},
     point_of_no_return,
     {suspend,[foo,bar]},
     {load,{bar,soft_purge,soft_purge}},
     {load,{foo,soft_purge,soft_purge}},
     {resume,[bar,foo]}] = X61,
    {ok, X62} = systools_rc:translate_scripts(dn,[Up6], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar]}},
     point_of_no_return,
     {suspend,[foo,bar]},
     {load,{foo,soft_purge,soft_purge}},
     {load,{bar,soft_purge,soft_purge}},
     {resume,[bar,foo]}] = X62,

    %% Simple circular dependency (2)
    Up7 = [{update, foo, soft, soft_purge, soft_purge, [bar, baz]},
	   {update, bar, soft, soft_purge, soft_purge, [foo]},
	   {update, baz, soft, soft_purge, soft_purge, [bar]}],
    {ok, X71} = systools_rc:translate_scripts([Up7], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar,baz]}},
     point_of_no_return,
     {suspend,[foo,bar,baz]},
     {load,{baz,soft_purge,soft_purge}},
     {load,{bar,soft_purge,soft_purge}},
     {load,{foo,soft_purge,soft_purge}},
     {resume,[baz, bar, foo]}] = X71,
    {ok, X72} = systools_rc:translate_scripts(dn,[Up7], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar,baz]}},
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
    {ok, X8} = systools_rc:translate_scripts([Up8], Apps, []),
    {value, {suspend, Order}} = lists:keysearch(suspend, 1, X8),
    Rest = case lists:reverse(Order) of
	       [bar, baz | R] -> R;
	       [baz, bar | R] -> R
	   end,
    case Rest of
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
    {ok, X9} = systools_rc:translate_scripts([Up9], Apps, []),
    Other2 = [X || {apply, {m, f, [X]}} <- X9],
    [1,2,3,4,5,6] = lists:sort(Other2),
    ok.


translate_app(Config) when is_list(Config) ->
    PreApps =
	[Test = #application{name = test,
			     description = "TEST",
			     vsn = "1.0",
			     modules = [foo,bar,baz],
			     regs = [],
			     mod = {sasl, []}},
	 #application{name = pelle,
		      description = "PELLE",
		      vsn = "1.0",
		      modules = [pelle, kalle],
		      regs = [],
		      mod = {pelle, []}}],
    Apps = [Test],
    %% Simple translation (1)
    Up1 = [{add_module, foo},
	   {add_module, bar}],
    {ok, X1} = systools_rc:translate_scripts([Up1], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar]}},
     point_of_no_return,
     {load,{foo,brutal_purge,brutal_purge}},
     {load,{bar,brutal_purge,brutal_purge}}] = X1,

    %% Simple translation (2)
    Up2 = [{add_application, test}],
    {ok, X2} = systools_rc:translate_scripts([Up2], Apps, []),
    io:format("X2=~p~n", [X2]),
    [{load_object_code,{test,"1.0",[foo,bar,baz]}},
     point_of_no_return,
     {load,{foo,brutal_purge,brutal_purge}},
     {load,{bar,brutal_purge,brutal_purge}},
     {load,{baz,brutal_purge,brutal_purge}},
     {apply,{application,start,[test,permanent]}}] = X2,

    %% Translate add_application with different restart types
    %%  permanent
    Up2_1 = [{add_application, test, permanent}],
    {ok, X2_1} = systools_rc:translate_scripts([Up2_1], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar,baz]}},
     point_of_no_return,
     {load,{foo,brutal_purge,brutal_purge}},
     {load,{bar,brutal_purge,brutal_purge}},
     {load,{baz,brutal_purge,brutal_purge}},
     {apply,{application,start,[test,permanent]}}] = X2_1,

    %%  transient
    Up2_2 = [{add_application, test, transient}],
    {ok, X2_2} = systools_rc:translate_scripts([Up2_2], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar,baz]}},
     point_of_no_return,
     {load,{foo,brutal_purge,brutal_purge}},
     {load,{bar,brutal_purge,brutal_purge}},
     {load,{baz,brutal_purge,brutal_purge}},
     {apply,{application,start,[test,transient]}}] = X2_2,

    %%  temporary
    Up2_3 = [{add_application, test, temporary}],
    {ok, X2_3} = systools_rc:translate_scripts([Up2_3], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar,baz]}},
     point_of_no_return,
     {load,{foo,brutal_purge,brutal_purge}},
     {load,{bar,brutal_purge,brutal_purge}},
     {load,{baz,brutal_purge,brutal_purge}},
     {apply,{application,start,[test,temporary]}}] = X2_3,

    %%  load
    Up2_4 = [{add_application, test, load}],
    {ok, X2_4} = systools_rc:translate_scripts([Up2_4], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar,baz]}},
     point_of_no_return,
     {load,{foo,brutal_purge,brutal_purge}},
     {load,{bar,brutal_purge,brutal_purge}},
     {load,{baz,brutal_purge,brutal_purge}},
     {apply,{application,load,[test]}}] = X2_4,

    %%  none
    Up2_5 = [{add_application, test, none}],
    {ok, X2_5} = systools_rc:translate_scripts([Up2_5], Apps, []),
    [{load_object_code,{test,"1.0",[foo,bar,baz]}},
     point_of_no_return,
     {load,{foo,brutal_purge,brutal_purge}},
     {load,{bar,brutal_purge,brutal_purge}},
     {load,{baz,brutal_purge,brutal_purge}}] = X2_5,

    %% Simple translation (3)
    Up3 = [{remove_application, pelle}],
    {ok, X3} = systools_rc:translate_scripts([Up3], Apps, PreApps),
    [point_of_no_return,
     {apply,{application,stop,[pelle]}},
     {remove,{pelle,brutal_purge,brutal_purge}},
     {remove,{kalle,brutal_purge,brutal_purge}},
     {purge,[pelle,kalle]},
     {apply,{application,unload,[pelle]}}] = X3,

    %% Simple translation (4)
    Up4 = [{restart_application, test}],
    {ok, X4} = systools_rc:translate_scripts([Up4], Apps, PreApps),
    [{load_object_code,{test,"1.0",[foo,bar,baz]}},
     point_of_no_return,
     {apply,{application,stop,[test]}},
     {remove,{foo,brutal_purge,brutal_purge}},
     {remove,{bar,brutal_purge,brutal_purge}},
     {remove,{baz,brutal_purge,brutal_purge}},
     {purge,[foo,bar,baz]},
     {load,{foo,brutal_purge,brutal_purge}},
     {load,{bar,brutal_purge,brutal_purge}},
     {load,{baz,brutal_purge,brutal_purge}},
     {apply,{application,start,[test,permanent]}}] = X4,

    %% Translate restart_application with different restart types
    %%  permanent
    {ok, X4_1} = systools_rc:translate_scripts([Up4],
					       [Test#application{type=permanent}],
					       [Test]),
    [{load_object_code,{test,"1.0",[foo,bar,baz]}},
     point_of_no_return,
     {apply,{application,stop,[test]}},
     {remove,{foo,brutal_purge,brutal_purge}},
     {remove,{bar,brutal_purge,brutal_purge}},
     {remove,{baz,brutal_purge,brutal_purge}},
     {purge,[foo,bar,baz]},
     {load,{foo,brutal_purge,brutal_purge}},
     {load,{bar,brutal_purge,brutal_purge}},
     {load,{baz,brutal_purge,brutal_purge}},
     {apply,{application,start,[test,permanent]}}] = X4_1,

    %%  transient
    {ok, X4_2} = systools_rc:translate_scripts([Up4],
					       [Test#application{type=transient}],
					       [Test]),
    [{load_object_code,{test,"1.0",[foo,bar,baz]}},
     point_of_no_return,
     {apply,{application,stop,[test]}},
     {remove,{foo,brutal_purge,brutal_purge}},
     {remove,{bar,brutal_purge,brutal_purge}},
     {remove,{baz,brutal_purge,brutal_purge}},
     {purge,[foo,bar,baz]},
     {load,{foo,brutal_purge,brutal_purge}},
     {load,{bar,brutal_purge,brutal_purge}},
     {load,{baz,brutal_purge,brutal_purge}},
     {apply,{application,start,[test,transient]}}] = X4_2,

    %%  temporary
    {ok, X4_3} = systools_rc:translate_scripts([Up4],
					       [Test#application{type=temporary}],
					       [Test]),
    [{load_object_code,{test,"1.0",[foo,bar,baz]}},
     point_of_no_return,
     {apply,{application,stop,[test]}},
     {remove,{foo,brutal_purge,brutal_purge}},
     {remove,{bar,brutal_purge,brutal_purge}},
     {remove,{baz,brutal_purge,brutal_purge}},
     {purge,[foo,bar,baz]},
     {load,{foo,brutal_purge,brutal_purge}},
     {load,{bar,brutal_purge,brutal_purge}},
     {load,{baz,brutal_purge,brutal_purge}},
     {apply,{application,start,[test,temporary]}}] = X4_3,

    %%  load
    {ok, X4_4} = systools_rc:translate_scripts([Up4],
					       [Test#application{type=load}],
					       [Test]),
    [{load_object_code,{test,"1.0",[foo,bar,baz]}},
     point_of_no_return,
     {apply,{application,stop,[test]}},
     {remove,{foo,brutal_purge,brutal_purge}},
     {remove,{bar,brutal_purge,brutal_purge}},
     {remove,{baz,brutal_purge,brutal_purge}},
     {purge,[foo,bar,baz]},
     {load,{foo,brutal_purge,brutal_purge}},
     {load,{bar,brutal_purge,brutal_purge}},
     {load,{baz,brutal_purge,brutal_purge}},
     {apply,{application,load,[test]}}] = X4_4,

    %%  none
    {ok, X4_5} = systools_rc:translate_scripts([Up4],
					       [Test#application{type=none}],
					       [Test]),
    [{load_object_code,{test,"1.0",[foo,bar,baz]}},
     point_of_no_return,
     {apply,{application,stop,[test]}},
     {remove,{foo,brutal_purge,brutal_purge}},
     {remove,{bar,brutal_purge,brutal_purge}},
     {remove,{baz,brutal_purge,brutal_purge}},
     {purge,[foo,bar,baz]},
     {load,{foo,brutal_purge,brutal_purge}},
     {load,{bar,brutal_purge,brutal_purge}},
     {load,{baz,brutal_purge,brutal_purge}}] = X4_5,

    ok.


translate_emulator_restarts(_Config) ->
    Apps =
	[#application{name = test,
		      description = "TEST",
		      vsn = "1.0",
		      modules = [foo,bar,baz],
		      regs = [],
		      mod = {sasl, []}},
	 #application{name = test,
		      description = "TEST2",
		      vsn = "1.0",
		      modules = [x,y,z],
		      regs = [],
		      mod = {sasl, []}}],
    %% restart_new_emulator
    Up1 = [{update, foo, soft, soft_purge, soft_purge, []},restart_new_emulator],
    {ok, X1} = systools_rc:translate_scripts([Up1], Apps, []),
    [restart_new_emulator,
     {load_object_code, {test,"1.0",[foo]}},
     point_of_no_return,
     {suspend,[foo]},
     {load,{foo,soft_purge,soft_purge}},
     {resume,[foo]}] = X1,

    %% restart_emulator
    Up2 = [{update, foo, soft, soft_purge, soft_purge, []},restart_emulator],
    {ok, X2} = systools_rc:translate_scripts([Up2], Apps, []),
    [{load_object_code, {test,"1.0",[foo]}},
     point_of_no_return,
     {suspend,[foo]},
     {load,{foo,soft_purge,soft_purge}},
     {resume,[foo]},
     restart_emulator] = X2,

    %% restart_emulator + restart_new_emulator
    Up3 = [{update, foo, soft, soft_purge, soft_purge, []},
	   restart_emulator,
	   restart_new_emulator],
    {ok, X3} = systools_rc:translate_scripts([Up3], Apps, []),
    [restart_new_emulator,
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
    {ok, X4} = systools_rc:translate_scripts([Up4a,Up4b], Apps, []),
    [restart_new_emulator,
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
    {ok, X5} = systools_rc:translate_scripts([Up5], Apps, []),
    [restart_new_emulator,
     point_of_no_return] = X5,

    %% only restart_emulator
    Up6 = [restart_emulator],
    {ok, X6} = systools_rc:translate_scripts([Up6], Apps, []),
    [point_of_no_return,
     restart_emulator] = X6,

    ok.

translate_add_delete_module(_Config) ->
    PreApps =
	[#application{name = test,
		      description = "TEST",
		      vsn = "0.1",
		      modules = [foo,bar,baz,old_mod],
		      regs = [],
		      mod = {sasl, []}}],
    Apps =
	[#application{name = test,
		      description = "TEST",
		      vsn = "1.0",
		      modules = [foo,bar,baz,new_mod],
		      regs = [],
		      mod = {sasl, []}}],
    S1 = [
	  {delete_module, old_mod},
	  {add_module, new_mod},
	  {load_module, foo}
	 ],
    {ok, X1} = systools_rc:translate_scripts([S1], Apps, PreApps),
    [{load_object_code,{test,"1.0",[new_mod,foo]}},
     point_of_no_return,
     {remove,{old_mod,brutal_purge,brutal_purge}},
     {purge,[old_mod]},
     {load,{new_mod,brutal_purge,brutal_purge}},
     {load,{foo,brutal_purge,brutal_purge}}] = X1,

    S2 = [
	  {delete_module, old_mod},
	  {add_module, new_mod, [foo]},
	  {load_module, foo}
	 ],
    {ok, X2} = systools_rc:translate_scripts([S2], Apps, PreApps),
    [{load_object_code,{test,"1.0",[new_mod,foo]}},
     point_of_no_return,
     {remove,{old_mod,brutal_purge,brutal_purge}},
     {purge,[old_mod]},
     {load,{foo,brutal_purge,brutal_purge}},
     {load,{new_mod,brutal_purge,brutal_purge}}] = X2,

    S3 = [
	  {delete_module, old_mod, [new_mod]},
	  {add_module, new_mod, [foo]},
	  {load_module, foo}
	 ],
    {ok, X3} = systools_rc:translate_scripts([S3], Apps, PreApps),
    [{load_object_code,{test,"1.0",[new_mod,foo]}},
     point_of_no_return,
     {load,{foo,brutal_purge,brutal_purge}},
     {load,{new_mod,brutal_purge,brutal_purge}},
     {remove,{old_mod,brutal_purge,brutal_purge}},
     {purge,[old_mod]}] = X3,

    ok.
