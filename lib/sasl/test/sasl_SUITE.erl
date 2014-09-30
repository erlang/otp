%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2014. All Rights Reserved.
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
-module(sasl_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0,groups/0,init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases must be exported.
-export([app_test/1,
	 appup_test/1,
	 log_mf_h_env/1]).

all() -> 
    [log_mf_h_env, app_test, appup_test].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Case, Config) ->
    Config.
end_per_testcase(_Case, _Config) ->
    ok.

app_test(Config) when is_list(Config) ->
    ?t:app_test(sasl, allow),
    ok.

%% Test that appup allows upgrade from/downgrade to a maximum of one
%% major release back.
appup_test(_Config) ->
    appup_tests(sasl,create_test_vsns(sasl)).

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


%% OTP-9185 - fail sasl start if some but not all log_mf_h env vars
%% are given.
log_mf_h_env(Config) ->
    PrivDir = ?config(priv_dir,Config),
    LogDir = filename:join(PrivDir,sasl_SUITE_log_dir),
    ok = file:make_dir(LogDir),
    application:stop(sasl),
    SaslEnv = application:get_all_env(sasl),
    lists:foreach(fun({E,_V}) -> application:unset_env(sasl,E) end, SaslEnv),

    ok = application:set_env(sasl,error_logger_mf_dir,LogDir),
    match_error(missing_config,application:start(sasl)),

    ok = application:set_env(sasl,error_logger_mf_maxbytes,"xx"),
    match_error(bad_config,application:start(sasl)),

    ok = application:set_env(sasl,error_logger_mf_maxbytes,50000),
    match_error(missing_config,application:start(sasl)),

    ok = application:set_env(sasl,error_logger_mf_maxfiles,"xx"),
    match_error(bad_config,application:start(sasl)),

    ok = application:set_env(sasl,error_logger_mf_maxfiles,2),
    ok = application:unset_env(sasl,error_logger_mf_dir),
    match_error(missing_config,application:start(sasl)),

    ok = application:set_env(sasl,error_logger_mf_dir,xx),
    match_error(bad_config,application:start(sasl)),

    ok = application:set_env(sasl,error_logger_mf_dir,LogDir),
    ok = application:start(sasl).


%%-----------------------------------------------------------------
%% Internal
match_error(Expected,{error,{bad_return,{_,{'EXIT',{Expected,{sasl,_}}}}}}) ->
    ok;
match_error(Expected,Actual) ->
    ?t:fail({unexpected_return,Expected,Actual}).
