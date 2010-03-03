%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
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
-module(config_1_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [
     {timetrap, {seconds,10}},
     %% x1 doesn't exist in cfg-file!
     {require, x1, x},
     {require, gen_cfg2},
     {require, alias, gen_cfg3},
     %% x1 default value
     {x1, {x,suite}}
    ].

is_exported(Module, Function, Arity)->
    Exports = Module:module_info(exports),
    case lists:keyfind(Function, 1, Exports) of
	false->
	    false;
	{Function, Arity}->
	    true;
	{Function, _OtherArity}->
	    false
    end.

get_all_config()->
    case is_exported(ct_util, get_all_config, 0) of
	true->
	    {ct_util, ct_util:get_all_config()};
	false->
	    {ct_config, ct_config:get_all_config()}
    end.

init_per_suite(Config) ->
    {Module, Cfg} = get_all_config(),
    ct:pal("CONFIG (handled by ~p):~n~p", [Module, Cfg]),
    Config.

end_per_suite(_) ->
    ok.

all() -> [test1,test2,test3,test4,test5,test6,test7,test8].

init_per_testcase(_, Config) ->
    {Module, Cfg} = get_all_config(),
    ct:pal("CONFIG (handled by ~p):~n~p", [Module, Cfg]),
    Config.

end_per_testcase(_, _) ->
    ok.

test1(_) ->
    suite = ct:get_config(x1),
    [{a,x},{b,y}] = ct:get_config(gen_cfg2),
    [{v,w},{m,n}] = ct:get_config(alias),
    ok.

%% should get skipped
test2() ->
    [{timetrap, {seconds,2}},
     {require, x1, x},
     {x1, {x,test2}}].
test2(_) ->
    test2 = ct:get_config(x1),
    [{a,x},{b,y}] = ct:get_config(gen_cfg2),
    [{v,w},{m,n}] = ct:get_config(alias),
    ct:fail("Test should've been skipped, you shouldn't see this!"),
    ok.

test3() ->
    [{timetrap, {seconds,3}},
     {require, y1, y},
     {y1, {y,test3}}].
test3(_) ->
    suite = ct:get_config(x1),
    test3 = ct:get_config(y1),
    [{a,x},{b,y}] = ct:get_config(gen_cfg2),
    [{v,w},{m,n}] = ct:get_config(alias),
    ok.

%% should get skipped
test4() ->
    [{require,alias,something},
     {alias,{something,else}},
     {require, x1, x},
     {x1, {x,test4}}].
test4(_) ->
    ct:fail("Test should've been skipped, you shouldn't see this!"),
    ok.

test5() ->
    [{require,newalias,gen_cfg2}].
test5(_) ->
    A = [{a,x},{b,y}] = ct:get_config(newalias),
    A = ct:get_config(gen_cfg2),
    ok.

test6(_) ->
    undefined = ct:get_config(y1),
    ok.

test7() ->
    [{require, y1, y},
     {y1, {y,test6}}].
test7(_) ->
    suite = ct:get_config(x1),
    test6 = ct:get_config(y1),
    ok.

%% should get skipped
test8() ->
    [{require, x}].
test8(_) ->
    ok.