%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2013. All Rights Reserved.
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


%% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).
-define(application, sasl).

%% Test server specific exports
-export([all/0,groups/0,init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases must be exported.
-export([app_test/1,
	 appup_test/1,
	 log_mf_h_env/1]).

all() -> 
    [app_test, appup_test, log_mf_h_env].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Case, Config) ->
    Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

app_test(Config) when is_list(Config) ->
    ?t:app_test(sasl, allow),
    ok.

%% Test that appup allows upgrade from/downgrade to a maximum of two
%% major releases back.
appup_test(_Config) ->
    application:load(sasl),
    {sasl,_,SaslVsn} = lists:keyfind(sasl,1,application:loaded_applications()),
    Ebin = filename:join(code:lib_dir(sasl),ebin),
    {ok,[{SaslVsn,UpFrom,DownTo}=Appup]} =
	file:consult(filename:join(Ebin,"sasl.appup")),
    ct:log("~p~n",[Appup]),
    {OkVsns,NokVsns} = create_test_vsns(SaslVsn),
    check_appup(OkVsns,UpFrom,{ok,[restart_new_emulator]}),
    check_appup(OkVsns,DownTo,{ok,[restart_new_emulator]}),
    check_appup(NokVsns,UpFrom,error),
    check_appup(NokVsns,DownTo,error),
    ok.

create_test_vsns(Current) ->
    [XStr,YStr|Rest] = string:tokens(Current,"."),
    X = list_to_integer(XStr),
    Y = list_to_integer(YStr),
    SecondMajor = vsn(X,Y-2),
    SecondMinor = SecondMajor ++ ".1.3",
    FirstMajor = vsn(X,Y-1),
    FirstMinor = FirstMajor ++ ".57",
    ThisMajor = vsn(X,Y),
    This =
	case Rest of
	    [] ->
		[];
	    ["1"] ->
		[ThisMajor];
	    _ ->
		ThisMinor = ThisMajor ++ ".1",
		[ThisMajor,ThisMinor]
	end,
    OkVsns = This ++ [FirstMajor, FirstMinor, SecondMajor, SecondMinor],

    ThirdMajor = vsn(X,Y-3),
    ThirdMinor = ThirdMajor ++ ".10.12",
    Illegal = ThisMajor ++ ",1",
    Newer1Major = vsn(X,Y+1),
    Newer1Minor = Newer1Major ++ ".1",
    Newer2Major = ThisMajor ++ "1",
    NokVsns = [ThirdMajor,ThirdMinor,
	       Illegal,
	       Newer1Major,Newer1Minor,
	       Newer2Major],
    {OkVsns,NokVsns}.

vsn(X,Y) ->
    integer_to_list(X) ++ "." ++ integer_to_list(Y).

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
