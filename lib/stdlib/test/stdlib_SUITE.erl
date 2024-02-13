%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2024. All Rights Reserved.
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

-compile(r24).

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

init_per_testcase(appup_test, Config) ->
    %% We check if the test results were released using a version
    %% of Erlang/OTP that was a tagged version or not. On a non-tagged
    %% version this testcase most likely will fail.
    case file:read_file(
           filename:join(
             proplists:get_value(data_dir,Config), "otp_version_tickets")) of
        {ok,<<"DEVELOPMENT",_/binary>>} ->
            {skip, "This is a development version, test might fail "
             "because of incorrect version numbers"};
        {ok,S} ->
            Config
    end;
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
		ct:log("Removed current version ~p from the list of ok versions to test.",
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
    S = otp_vsns:read_state(),
    Rel = list_to_integer(erlang:system_info(otp_release)),
    AppStr = atom_to_list(App),
    Ok = ok_app_vsns(S, Rel, AppStr),
    Nok0 = nok_app_vsns(S, Rel, AppStr, hd(Ok)),
    Nok = case Ok of
              [Ok1|_] ->
                  [Ok1 ++ ",1" | Nok0]; % illegal
              _ ->
                  Nok0
          end,
    {Ok, Nok}.

ok_app_vsns(S, Rel, AppStr) ->
    AppVsns0 = get_rel_app_vsns(S, Rel-2, AppStr),
    AppVsns1 = get_rel_app_vsns(S, Rel-1, AppStr),
    AppVsns2 = try
                   get_rel_app_vsns(S, Rel, AppStr)
               catch
                   _:_ -> []
               end,
    lists:usort(AppVsns2 ++ AppVsns1 ++ AppVsns0).

nok_app_vsns(S, Rel, AppStr, EarliestOkVsn) ->
    AppVsns0 = get_rel_app_vsns(S, Rel-4, AppStr),
    AppVsns1 = get_rel_app_vsns(S, Rel-3, AppStr),
    %% Earliest OK version may exist in not OK versions
    %% as well if there were no application version bump
    %% between two releases, so we need to remove it
    %% if that is the case...
    lists:usort(AppVsns1 ++ AppVsns0) -- EarliestOkVsn.

get_rel_app_vsns(S, Rel, App) ->
    RelStr = integer_to_list(Rel),
    OtpVsns = otp_vsns:branch_vsns(S, "maint-"++RelStr),
    lists:map(fun (OtpVsn) ->
                      AppVsn = otp_vsns:app_vsn(S, OtpVsn, App),
                      [_, Vsn] = string:lexemes(AppVsn, "-"),
                      Vsn
              end, OtpVsns).

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
