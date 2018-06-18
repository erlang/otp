%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2018. All Rights Reserved.
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
-module(sasl_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([init_per_suite/1,end_per_suite/1]).
-export([all/0,groups/0,init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases must be exported.
-export([app_test/1,
	 appup_test/1,
	 log_mf_h_env/1,
	 log_file/1,
	 utc_log/1]).

all() -> 
    [log_mf_h_env, log_file, app_test, appup_test, utc_log].

groups() -> 
    [].

init_per_suite(Config) ->
    S = application:get_env(kernel,logger_sasl_compatible),
    application:set_env(kernel,logger_sasl_compatible,true),
    [{sasl_compatible,S}|Config].

end_per_suite(Config) ->
    case ?config(sasl_compatible,Config) of
        {ok,X} ->
            application:set_env(kernel,logger_sasl_compatible,X);
        undefined ->
            application:unset_env(kernel,logger_sasl_compatible)
    end.

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
    ok = filelib:ensure_dir(LogDir),
    application:stop(sasl),
    clear_env(sasl),

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

log_file(Config) ->
    PrivDir = ?config(priv_dir,Config),
    LogDir  = filename:join(PrivDir,sasl_SUITE_log_dir),
    File    = filename:join(LogDir, "file.log"),
    ok      = filelib:ensure_dir(File),
    application:stop(sasl),
    clear_env(sasl),

    _ = test_log_file(File, {file,File}),
    _ = test_log_file(File, {file,File,[write]}),

    ok = file:write_file(File, <<"=PROGRESS preserve me\n">>),
    <<"=PROGRESS preserve me\n",_/binary>> =
	test_log_file(File, {file,File,[append]}),

    ok = application:set_env(sasl,sasl_error_logger, tty,
			     [{persistent, false}]),
    ok = application:start(sasl).

test_log_file(File, Arg) ->
    ok = application:set_env(sasl, sasl_error_logger, Arg,
			     [{persistent, true}]),
    ok = application:start(sasl),
    application:stop(sasl),
    {ok,Bin} = file:read_file(File),
    ok = file:delete(File),
    Lines0 = binary:split(Bin, <<"\n">>, [trim_all,global]),
    Lines = [L || L <- Lines0,
		  binary:match(L, <<"=PROGRESS">>) =:= {0,9}],
    io:format("~p:\n~p\n", [Arg,Lines]),

    %% There must be at least four PROGRESS lines.
    if
	length(Lines) >= 4 -> ok;
	true -> ?t:fail()
    end,
    Bin.

%% Make a basic test of utc_log.
utc_log(Config) ->
    PrivDir = ?config(priv_dir, Config),
    LogDir = filename:join(PrivDir, sasl_SUITE_log_dir),
    Log = filename:join(LogDir, "utc.log"),
    ok = filelib:ensure_dir(Log),

    application:stop(sasl),
    clear_env(sasl),

    %% Test that the UTC marker gets added to PROGRESS lines
    %% when the utc_log configuration variable is set to true.
    ok = application:set_env(sasl, sasl_error_logger, {file,Log},
			     [{persistent,true}]),
    ok = application:set_env(sasl, utc_log, true, [{persistent,true}]),
    ok = application:start(sasl),
    application:stop(sasl),

    verify_utc_log(Log, true),

    %% Test that no UTC markers gets added to PROGRESS lines
    %% when the utc_log configuration variable is set to false.
    ok = application:set_env(sasl, utc_log, false, [{persistent,true}]),
    ok = application:start(sasl),
    application:stop(sasl),

    verify_utc_log(Log, false),

    %% Test that no UTC markers gets added to PROGRESS lines
    %% when the utc_log configuration variable is unset.
    ok = application:unset_env(sasl, utc_log, [{persistent,true}]),
    ok = application:start(sasl),
    application:stop(sasl),

    verify_utc_log(Log, false),

    %% Change back to the standard TTY error logger.
    ok = application:set_env(sasl,sasl_error_logger, tty,
			     [{persistent, false}]),
    ok = application:start(sasl).

verify_utc_log(Log, UTC) ->
    {ok,Bin} = file:read_file(Log),
    ok = file:delete(Log),

    Lines0 = binary:split(Bin, <<"\n">>, [trim_all,global]),
    Lines = [L || L <- Lines0,
		  binary:match(L, <<"=PROGRESS">>) =:= {0,9}],
    Setting = application:get_env(sasl, utc_log),
    io:format("utc_log ~p:\n~p\n", [Setting,Lines]),
    Filtered = [L || L <- Lines,
		     binary:match(L, <<" UTC ===">>) =:= nomatch],
    %% Filtered now contains all lines WITHOUT any UTC markers.
    case UTC of
	false ->
	    %% No UTC marker on the PROGRESS line.
	    Filtered = Lines;
	true ->
	    %% Each PROGRESS line must have an UTC marker.
	    [] = Filtered
    end,
    ok.


%%-----------------------------------------------------------------
%% Internal
match_error(Expected,{error,{bad_return,{_,{'EXIT',{Expected,{sasl,_}}}}}}) ->
    ok;
match_error(Expected,Actual) ->
    ?t:fail({unexpected_return,Expected,Actual}).

clear_env(App) ->
    [application:unset_env(App,Opt) || {Opt,_} <- application:get_all_env(App)],
    ok.
