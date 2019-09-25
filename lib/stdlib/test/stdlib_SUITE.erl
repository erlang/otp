%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%%----------------------------------------------------------------
%%% Purpose:Stdlib application test suite.
%%%-----------------------------------------------------------------
-module(stdlib_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1,
	 init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2,
         app_test/1, appup_test/1, assert_test/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [app_test, appup_test, assert_test].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Case, Config) ->
    Config.
end_per_testcase(_Case, _Config) ->
    ok.

%%
%% Test cases starts here.
%%
%% Application consistency test.
app_test(Config) when is_list(Config) ->
    test_server:app_test(stdlib),
    ok.

%% Test that appup allows upgrade from/downgrade to a maximum of one
%% major release back.
appup_test(_Config) ->
    appup_tests(stdlib,create_test_vsns(stdlib)).

appup_tests(_App,{[],[]}) ->
    {skip,"no previous releases available"};
appup_tests(App,{OkVsns0,NokVsns}) ->
    application:load(App),
    {_,_,Vsn} = lists:keyfind(App,1,application:loaded_applications()),
    AppupFileName = atom_to_list(App) ++ ".appup",
    AppupFile = filename:join([code:lib_dir(App),ebin,AppupFileName]),
    {ok,[{Vsn,UpFrom,DownTo}=AppupScript]} = file:consult(AppupFile),
    ct:log("~p~n",[AppupScript]),
    OkVsns =
	case OkVsns0 -- [Vsn] of
	    OkVsns0 ->
		OkVsns0;
	    Ok ->
		ct:log("Current version, ~p, is same as in previous release.~n"
		       "Removing this from the list of ok versions.",
		      [Vsn]),
		Ok
	end,
    ct:log("Testing that appup allows upgrade from these versions: ~p~n",
	   [OkVsns]),
    check_appup(OkVsns,UpFrom,{ok,[restart_new_emulator]}),
    check_appup(OkVsns,DownTo,{ok,[restart_new_emulator]}),
    ct:log("Testing that appup does not allow upgrade from these versions: ~p~n",
	   [NokVsns]),
    check_appup(NokVsns,UpFrom,error),
    check_appup(NokVsns,DownTo,error),
    ok.

create_test_vsns(App) ->
    ThisMajor = erlang:system_info(otp_release),
    FirstMajor = previous_major(ThisMajor),
    SecondMajor = previous_major(FirstMajor),
    Ok = app_vsn(App,[ThisMajor,FirstMajor]),
    Nok0 = app_vsn(App,[SecondMajor]),
    Nok = case Ok of
	       [Ok1|_] ->
		   [Ok1 ++ ",1" | Nok0]; % illegal
	       _ ->
		   Nok0
	   end,
    {Ok,Nok}.

previous_major("17") ->
    "r16b";
previous_major("r16b") ->
    "r15b";
previous_major(Rel) ->
    integer_to_list(list_to_integer(Rel)-1).

app_vsn(App,[R|Rs]) ->
    OldRel =
	case test_server:is_release_available(R) of
	    true ->
		{release,R};
	    false ->
		case ct:get_config({otp_releases,list_to_atom(R)}) of
		    undefined ->
			false;
		    Prog0 ->
			case os:find_executable(Prog0) of
			    false ->
				false;
			    Prog ->
				{prog,Prog}
			end
		end
	end,
    case OldRel of
	false ->
	    app_vsn(App,Rs);
	_ ->
	    {ok,N} = test_server:start_node(prevrel,peer,[{erl,[OldRel]}]),
	    _ = rpc:call(N,application,load,[App]),
	    As = rpc:call(N,application,loaded_applications,[]),
	    {_,_,V} = lists:keyfind(App,1,As),
	    test_server:stop_node(N),
	    [V|app_vsn(App,Rs)]
    end;
app_vsn(_App,[]) ->
    [].

check_appup([Vsn|Vsns],Instrs,Expected) ->
    case systools_relup:appup_search_for_version(Vsn, Instrs) of
	Expected -> check_appup(Vsns,Instrs,Expected);
	Other -> ct:fail({unexpected_result_for_vsn,Vsn,Other})
    end;
check_appup([],_,_) ->
    ok.


-include_lib("stdlib/include/assert.hrl").
-include_lib("stdlib/include/assert.hrl"). % test repeated inclusion

%% Assert macros test.
assert_test(_Config) ->
    ok = ?assert(true),
    {'EXIT',{{assert, _},_}} = (catch ?assert(false)),
    {'EXIT',{{assert, Info1},_}} = (catch ?assert(0)),
    {not_boolean,0} = lists:keyfind(not_boolean,1,Info1),

    ok = ?assertNot(false),
    {'EXIT',{{assert, _},_}} = (catch ?assertNot(true)),
    {'EXIT',{{assert, Info2},_}} = (catch ?assertNot(0)),
    {not_boolean,0} = lists:keyfind(not_boolean,1,Info2),

    ok = ?assertMatch({foo,_}, {foo,bar}),
    {'EXIT',{{assertMatch,_},_}} =
        (catch ?assertMatch({foo,_}, {foo})),

    ok = ?assertMatch({foo,N} when N > 0, {foo,1}),
    {'EXIT',{{assertMatch,_},_}} =
        (catch ?assertMatch({foo,N} when N > 0, {foo,0})),

    ok = ?assertNotMatch({foo,_}, {foo,bar,baz}),
    {'EXIT',{{assertNotMatch,_},_}} =
        (catch ?assertNotMatch({foo,_}, {foo,baz})),

    ok = ?assertNotMatch({foo,N} when N > 0, {foo,0}),
    {'EXIT',{{assertNotMatch,_},_}} =
        (catch ?assertNotMatch({foo,N} when N > 0, {foo,1})),

    ok = ?assertEqual(1.0, 1.0),
    {'EXIT',{{assertEqual,_},_}} = (catch ?assertEqual(1, 1.0)),

    ok = ?assertNotEqual(1, 1.0),
    {'EXIT',{{assertNotEqual,_},_}} = (catch ?assertNotEqual(1.0, 1.0)),

    ok = ?assertException(error, badarith, 1/0),
    ok = ?assertException(exit, foo, exit(foo)),
    ok = ?assertException(throw, foo, throw(foo)),
    ok = ?assertException(throw, {foo,_}, throw({foo,bar})),
    ok = ?assertException(throw, {foo,N} when N > 0, throw({foo,1})),
    {'EXIT',{{assertException,Why1},_}} =
        (catch ?assertException(error, badarith, 0/1)),
    true = lists:keymember(unexpected_success,1,Why1),
    {'EXIT',{{assertException,Why2},_}} =
        (catch ?assertException(error, badarith, 1/length(0))),
    true = lists:keymember(unexpected_exception,1,Why2),
    {'EXIT',{{assertException,Why3},_}} =
        (catch ?assertException(throw, {foo,N} when N > 0, throw({foo,0}))),
    true = lists:keymember(unexpected_exception,1,Why3),

    ok = ?assertNotException(throw, {foo,baz}, throw({foo,bar})),
    {'EXIT',{{assertNotException,Why4},_}} =
        (catch ?assertNotException(throw, {foo,bar}, throw({foo,bar}))),
    true = lists:keymember(unexpected_exception,1,Why4),

    ok = ?assertError(badarith, 1/0),
    ok = ?assertExit(foo, exit(foo)),
    ok = ?assertThrow(foo, throw(foo)),
    ok.
