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
%%% Kernel application test suite.
%%%-----------------------------------------------------------------
-module(kernel_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(r24).

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases must be exported.
-export([app_test/1, appup_test/1, refc/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,2}}].

all() -> 
    [app_test, appup_test, refc].

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
        {ok,_NotDev} ->
            Config
    end;
init_per_testcase(_Case, Config) ->
    Config.
end_per_testcase(_Case, _Config) ->
    ok.

%%
%% Test cases starts here.
%%
%% Tests the applications consistency.
app_test(Config) when is_list(Config) ->
    ok=test_server:app_test(kernel),
    ok.


%% Test that appup allows upgrade from/downgrade to a maximum of one
%% major release back.
appup_test(_Config) ->
    appup_tests(kernel,create_test_vsns(kernel)).

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

%%% Check that refc module handles the counters as expected
refc(_Config) ->
    Enable = fun(Enable) -> erlang:system_flag(scheduler_wall_time, Enable) end,
    IsOn = fun() -> undefined /= erlang:statistics(scheduler_wall_time) end,
    Tester = self(),
    Loop = fun Loop() ->
                   receive
                       die -> normal;
                       {apply, Bool} ->
                           Res = Enable(Bool),
                           Tester ! {self(), Res},
                           Loop()
                   end
           end,

    %% Counter should be 0
    false = Enable(false),

    false = Enable(true),
    true  = Enable(true),
    true  = Enable(false),
    true  = Enable(false),

    %% Counter should be 0
    false = IsOn(),

    P1 = spawn_link(Loop),
    P1 ! {apply, true},
    receive {P1, R1} -> false = R1 end,

    %% P1 has turned it on counter should be one
    true = IsOn(),
    true = Enable(true),
    true = Enable(false),
    true = IsOn(),

    P1 ! {apply, false},
    receive {P1, R2} -> true = R2 end,
    false = IsOn(),

    P1 ! {apply, true},
    receive {P1, R3} -> false = R3 end,
    true = IsOn(),
    true = Enable(false),


    P1 ! die,
    timer:sleep(100),
    false = IsOn(),
    false = Enable(false),

    P2 = spawn_link(Loop),
    P2 ! {apply, true},
    receive {P2, R4} -> false = R4 end,
    true = IsOn(),
    P2 ! {apply, true},
    receive {P2, R5} -> true = R5 end,
    true = IsOn(),

    P2 ! die,
    timer:sleep(100),
    false = IsOn(),

    ok.
