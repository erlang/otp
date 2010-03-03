%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(config_2_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [
     {timetrap, {seconds,10}}
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

all() -> [test1].

init_per_testcase(_, Config) ->
    {Module, Cfg} = get_all_config(),
    ct:pal("CONFIG (handled by ~p):~n~p", [Module, Cfg]),
    Config.

end_per_testcase(_, _) ->
    ok.

test1(_)->
    x = ct:get_config({gen_cfg2, a}),
    ok.
