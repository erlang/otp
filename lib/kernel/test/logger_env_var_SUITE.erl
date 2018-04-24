%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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
-module(logger_env_var_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/src/logger_internal.hrl").

-define(all_vars,[{kernel,logger_dest},
                  {kernel,logger_level},
                  {kernel,logger_log_progress},
                  {kernel,logger_sasl_compatible},
                  {kernel,error_logger}]).

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Env = [{App,Key,application:get_env(App,Key)} || {App,Key} <- ?all_vars],
    Removed = cleanup(),
    [{env,Env},{logger,Removed}|Config].

end_per_suite(Config) ->
    [application:set_env(App,Key,Val) ||
        {App,Key,Val} <- ?config(env,Config),
        Val =/= undefined],
    Hs = ?config(logger,Config),
    [ok = logger:add_handler(Id,Mod,C) || {Id,Mod,C} <- Hs],
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(Case, Config) ->
    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,
    cleanup(),
    ok.

groups() ->
    [].

all() -> 
    [default,
     default_sasl_compatible,
     dest_tty,
     dest_tty_sasl_compatible,
     dest_false,
     dest_false_progress,
     dest_false_sasl_compatible,
     dest_silent,
     dest_silent_sasl_compatible,
     dest_file_old,
     dest_file,
     dest_disk_log,
     %% disk_log_vars, % or test this in logger_disk_log_SUITE?
     sasl_compatible_false,
     sasl_compatible_false_no_progress,
     sasl_compatible,
     bad_dest%% ,
     %% bad_level,
     %% bad_sasl_compatibility,
     %% bad_progress
    ].

default(Config) ->
    {ok,{_Log,Hs}} = setup(Config,?FUNCTION_NAME,
                              undefined,
                              undefined, % dest
                              undefined, % level
                              undefined, % sasl comp (default=false)
                              undefined), % progress (default=false)
    {logger_std_h,logger_std_h,StdC} = lists:keyfind(logger_std_h,1,Hs),
    true = is_pid(whereis(logger_std_h)),
    info = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,prefix_of,[beam,erlang,otp,sasl]}}} =
        lists:keyfind(domain,1,StdFilters),
    true = lists:keymember(stop_progress,1,StdFilters),
    false = lists:keymember(logger_simple,1,Hs),
    false = lists:keymember(sasl_h,1,Hs),
    false = is_pid(whereis(sasl_h)),
    ok.

default_sasl_compatible(Config) ->
    {ok,{_Log,Hs}} = setup(Config,?FUNCTION_NAME,
                              undefined,
                              undefined, % dest
                              undefined, % level
                              true, % sasl comp (default=false)
                              undefined), % progress (default=false)
    {logger_std_h,logger_std_h,StdC} = lists:keyfind(logger_std_h,1,Hs),
    true = is_pid(whereis(logger_std_h)),
    info = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,prefix_of,[beam,erlang,otp]}}} =
        lists:keyfind(domain,1,StdFilters),
    false = lists:keymember(stop_progress,1,StdFilters),
    false = lists:keymember(logger_simple,1,Hs),
    true = lists:keymember(sasl_h,1,Hs),
    true = is_pid(whereis(sasl_h)),
    ok.

dest_tty(Config) ->
    {ok,{_Log,Hs}} = setup(Config,?FUNCTION_NAME,
                              logger_dest,
                              tty, % dest
                              undefined, % level
                              undefined, % sasl comp (default=false)
                              undefined), % progress (default=false)
    {logger_std_h,logger_std_h,StdC} = lists:keyfind(logger_std_h,1,Hs),
    true = is_pid(whereis(logger_std_h)),
    info = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,prefix_of,[beam,erlang,otp,sasl]}}} =
        lists:keyfind(domain,1,StdFilters),
    true = lists:keymember(stop_progress,1,StdFilters),
    false = lists:keymember(logger_simple,1,Hs),
    false = lists:keymember(sasl_h,1,Hs),
    false = is_pid(whereis(sasl_h)),
    ok.

dest_tty_sasl_compatible(Config) ->
    {ok,{_Log,Hs}} = setup(Config,?FUNCTION_NAME,
                              logger_dest,
                              tty, % dest
                              undefined, % level
                              true, % sasl comp (default=false)
                              undefined), % progress (default=false)
    {logger_std_h,logger_std_h,StdC} = lists:keyfind(logger_std_h,1,Hs),
    true = is_pid(whereis(logger_std_h)),
    info = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,prefix_of,[beam,erlang,otp]}}} =
        lists:keyfind(domain,1,StdFilters),
    false = lists:keymember(stop_progress,1,StdFilters),
    false = lists:keymember(logger_simple,1,Hs),
    true = lists:keymember(sasl_h,1,Hs),
    true = is_pid(whereis(sasl_h)),
    ok.

dest_false(Config) ->
    {ok,{_Log,Hs}} = setup(Config,?FUNCTION_NAME,
                              logger_dest,
                              false, % dest
                              notice, % level
                              undefined, % sasl comp (default=false)
                              undefined), % progress (default=false)
    false = lists:keymember(logger_std_h,1,Hs),
    {logger_simple,logger_simple,SimpleC} = lists:keyfind(logger_simple,1,Hs),
    notice = maps:get(level,SimpleC),
    SimpleFilters = maps:get(filters,SimpleC),
    {domain,{_,{log,prefix_of,[beam,erlang,otp,sasl]}}} =
        lists:keyfind(domain,1,SimpleFilters),
    true = lists:keymember(stop_progress,1,SimpleFilters),
    false = lists:keymember(sasl_h,1,Hs),
    ok.

dest_false_progress(Config) ->
    {ok,{_Log,Hs}} = setup(Config,?FUNCTION_NAME,
                              logger_dest,
                              false, % dest
                              notice, % level
                              undefined, % sasl comp (default=false)
                              true), % progress (default=false)
    false = lists:keymember(logger_std_h,1,Hs),
    {logger_simple,logger_simple,SimpleC} = lists:keyfind(logger_simple,1,Hs),
    notice = maps:get(level,SimpleC),
    SimpleFilters = maps:get(filters,SimpleC),
    {domain,{_,{log,prefix_of,[beam,erlang,otp,sasl]}}} =
        lists:keyfind(domain,1,SimpleFilters),
    false = lists:keymember(stop_progress,1,SimpleFilters),
    false = lists:keymember(sasl_h,1,Hs),
    ok.

dest_false_sasl_compatible(Config) ->
    {ok,{_Log,Hs}} = setup(Config,?FUNCTION_NAME,
                              logger_dest,
                              false, % dest
                              notice, % level
                              true, % sasl comp (default=false)
                              undefined), % progress (default=false)
    false = lists:keymember(logger_std_h,1,Hs),
    {logger_simple,logger_simple,SimpleC} = lists:keyfind(logger_simple,1,Hs),
    notice = maps:get(level,SimpleC),
    SimpleFilters = maps:get(filters,SimpleC),
    {domain,{_,{log,prefix_of,[beam,erlang,otp]}}} =
        lists:keyfind(domain,1,SimpleFilters),
    false = lists:keymember(stop_progress,1,SimpleFilters),
    true = lists:keymember(sasl_h,1,Hs),
    true = is_pid(whereis(sasl_h)),
    ok.

dest_silent(Config) ->
    {ok,{_Log,Hs}} = setup(Config,?FUNCTION_NAME,
                              logger_dest,
                              silent, % dest
                              undefined, % level
                              undefined, % sasl comp (default=false)
                              undefined), % progress (default=false)
    false = lists:keymember(logger_std_h,1,Hs),
    false = lists:keymember(logger_simple,1,Hs),
    false = lists:keymember(sasl_h,1,Hs),
    ok.

dest_silent_sasl_compatible(Config) ->
    {ok,{_Log,Hs}} = setup(Config,?FUNCTION_NAME,
                              logger_dest,
                              silent, % dest
                              undefined, % level
                              true, % sasl comp (default=false)
                              undefined), % progress (default=false)
    false = lists:keymember(logger_std_h,1,Hs),
    false = lists:keymember(logger_simple,1,Hs),
    true = lists:keymember(sasl_h,1,Hs),
    true = is_pid(whereis(sasl_h)),
    ok.


dest_file_old(Config) ->
    {ok,{Log,_Hs}} = setup(Config,?FUNCTION_NAME,
                              error_logger,
                              file, % dest
                              undefined, % level
                              undefined, % sasl comp (default=false)
                              undefined), % progress (default=false)
    check_log(Log,
              file, % dest
              0), % progress in std logger
    ok.
    

dest_file(Config) ->
    {ok,{Log,_Hs}} = setup(Config,?FUNCTION_NAME,
                              logger_dest,
                              file, % dest
                              undefined, % level
                              undefined, % sasl comp (default=false)
                              undefined), % progress (default=false)
    check_log(Log,
              file, % dest
              0), % progress in std logger
    ok.
    

dest_disk_log(Config) ->
    {ok,{Log,_Hs}} = setup(Config,?FUNCTION_NAME,
                              logger_dest,
                              disk_log, % dest
                              undefined, % level
                              undefined, % sasl comp (default=false)
                              undefined), % progress (default=false)
    check_log(Log,
              disk_log, % dest
              0), % progress in std logger
    ok.
    

sasl_compatible_false(Config) ->
    {ok,{Log,_Hs}} = setup(Config,?FUNCTION_NAME,
                              logger_dest,
                              file, % dest
                              undefined, % level
                              false, % sasl comp
                              true), % progress
    check_log(Log,
              file, % dest
              4), % progress in std logger
    ok.

sasl_compatible_false_no_progress(Config) ->
    {ok,{Log,_Hs}} = setup(Config,?FUNCTION_NAME,
                              logger_dest,
                              file, % dest
                              undefined, % level
                              false, % sasl comp
                              false), % progress
    check_log(Log,
              file, % dest
              0), % progress in std logger
    ok.

sasl_compatible(Config) ->
    {ok,{Log,_Hs}} = setup(Config,?FUNCTION_NAME,
                              logger_dest,
                              file, % dest
                              undefined, % level
                              true, % sasl comp
                              undefined), % progress
    check_log(Log,
              file, % dest
              0), % progress in std logger
    ok.

bad_dest(Config) ->
    {error,{bad_config,{kernel,{logger_dest,baddest}}}} =
        setup(Config,?FUNCTION_NAME,
              logger_dest,
              baddest,
              undefined,
              undefined,
              undefined).

bad_level(Config) ->
    error =
        setup(Config,?FUNCTION_NAME,
              logger_dest,
              tty,
              badlevel,
              undefined,
              undefined).

bad_sasl_compatibility(Config) ->
    error =
        setup(Config,?FUNCTION_NAME,
              logger_dest,
              tty,
              info,
              badcomp,
              undefined).

bad_progress(Config) ->
    error =
        setup(Config,?FUNCTION_NAME,
              logger_dest,
              tty,
              info,
              undefined,
              badprogress).

%%%-----------------------------------------------------------------
%%% Internal
setup(Config,Func,DestVar,Dest,Level,SaslComp,Progress) ->
    ok = logger:add_handler(logger_simple,logger_simple,
                            #{filter_default=>log,
                              logger_simple=>#{buffer=>true}}),
    Dir = ?config(priv_dir,Config),
    File = lists:concat([?MODULE,"_",Func,".log"]),
    Log = filename:join(Dir,File),
    case Dest of
        undefined ->
            ok;
        F when F==file; F==disk_log ->
            application:set_env(kernel,DestVar,{Dest,Log});
        _ ->
            application:set_env(kernel,DestVar,Dest)
    end,
    case Level of
        undefined ->
            ok;
        _ ->
           application:set_env(kernel,logger_level,Level)
    end,
    case SaslComp of
        undefined ->
            ok;
        _ ->
            application:set_env(kernel,logger_sasl_compatible,SaslComp)
    end,
    case Progress of
        undefined ->
            ok;
        _ ->
            application:set_env(kernel,logger_log_progress,Progress)
    end,
    case logger:setup_standard_handler() of
        ok ->
            application:start(sasl),
            StdH = case Dest of
                       NoH when NoH==false; NoH==silent -> false;
                       _ -> true
                   end,
            StdH = is_pid(whereis(?STANDARD_HANDLER)),
            SaslH = if SaslComp -> true;
                       true -> false
                    end,
            SaslH = is_pid(whereis(sasl_h)),
            {ok,{Log,maps:get(handlers,logger:i())}};
        Error ->
            Error
    end.

check_log(Log,Dest,NumProgress) ->
    ok = logger:alert("dummy1"),
    ok = logger:debug("dummy1"),

    %% Check that there are progress reports (supervisor and
    %% application_controller) and an error report (the call above) in
    %% the log. There should not be any info reports yet.
    {ok,Bin1} = sync_and_read(Dest,Log),
    ct:log("Log content:~n~s",[Bin1]),
    match(Bin1,<<"PROGRESS REPORT">>,NumProgress),
    match(Bin1,<<"ALERT REPORT">>,1),
    match(Bin1,<<"INFO REPORT">>,0),
    match(Bin1,<<"DEBUG REPORT">>,0),

    %% Then stop sasl and see that the info report from
    %% application_controller is there
    ok = application:stop(sasl),
    {ok,Bin2} = sync_and_read(Dest,Log),
    ct:log("Log content:~n~s",[Bin2]),
    match(Bin2,<<"INFO REPORT">>,1),
    match(Bin1,<<"DEBUG REPORT">>,0),
    ok.

match(Bin,Pattern,0) ->
    nomatch = re:run(Bin,Pattern,[{capture,none}]);
match(Bin,Pattern,N) ->
    {match,M} = re:run(Bin,Pattern,[{capture,all},global]),
    N = length(M).

sync_and_read(disk_log,Log) ->
    logger_disk_log_h:disk_log_sync(?STANDARD_HANDLER),
    file:read_file(Log ++ ".1");
sync_and_read(file,Log) ->
    logger_std_h:filesync(?STANDARD_HANDLER),
    file:read_file(Log).

cleanup() ->
    application:stop(sasl),
    [application:unset_env(App,Key) || {App,Key} <- ?all_vars],
    #{handlers:=Hs0} = logger:i(),
    Hs = lists:keydelete(cth_log_redirect,1,Hs0),
    [ok = logger:remove_handler(Id) || {Id,_,_} <- Hs],
    Hs.
