%%
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
-module(logger_disk_log_h_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/src/logger_internal.hrl").
-include_lib("kernel/src/logger_olp.hrl").
-include_lib("kernel/src/logger_h_common.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("kernel/include/file.hrl").

-define(check_no_log, [] = test_server:messages_get()).

-define(check(Expected),
        receive {log,Expected} ->
                [] = test_server:messages_get()
        after 1000 ->
                ct:fail({report_not_received,
                         {line,?LINE},
                         {got,test_server:messages_get()}})
        end).

-define(msg,"Log from "++atom_to_list(?FUNCTION_NAME)++
            ":"++integer_to_list(?LINE)).
-define(bin(Msg), list_to_binary(Msg++"\n")).
-define(log_no(File,N), lists:concat([File,".",N])).
-define(domain,#{domain=>[?MODULE]}).

suite() ->
    [{timetrap,{seconds,30}},
     {ct_hooks,[logger_test_lib]}].

init_per_suite(Config) ->
    timer:start(),                              % to avoid progress report
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(TestHooksCase, Config) when
      TestHooksCase == write_failure;
      TestHooksCase == sync_failure ->
    case (fun() -> ?TEST_HOOKS_TAB == undefined end)() of
        true ->
            {skip,"Define the TEST_HOOKS macro to run this test"};
        false ->
            ct:print("********** ~w **********", [TestHooksCase]),
            Config
    end;
init_per_testcase(TestCase, Config) ->
    ct:print("********** ~w **********", [TestCase]),
    Config.

end_per_testcase(Case, Config) ->
    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,
    ok.

groups() ->
    [].

all() -> 
    [start_stop_handler,
     create_log,
     open_existing_log,
     disk_log_opts,
     default_formatter,
     logging,
     filter_config,
     errors,
     formatter_fail,
     config_fail,
     bad_input,
     reconfig,
     sync,
     disk_log_full,
     disk_log_wrap,
     disk_log_events,
     write_failure,
     sync_failure,
     op_switch_to_sync,
     op_switch_to_drop,
     op_switch_to_flush,
     limit_burst_disabled,
     limit_burst_enabled_one,
     limit_burst_enabled_period,
     kill_disabled,
     qlen_kill_new,
     %% qlen_kill_std,
     mem_kill_new,
     %% mem_kill_std,
     restart_after,
     handler_requests_under_load
    ].

start_stop_handler(_Config) ->
    ok = logger:add_handler(?MODULE, logger_disk_log_h, #{}),
    {error,{already_exist,?MODULE}} =
        logger:add_handler(?MODULE, logger_disk_log_h, #{}),
    true = is_pid(whereis(h_proc_name())),
    ok = logger:remove_handler(?MODULE),
    timer:sleep(500),
    undefined = whereis(h_proc_name()).
start_stop_handler(cleanup, _Config) ->
    logger:remove_handler(?MODULE).

create_log(Config) ->
    PrivDir = ?config(priv_dir,Config),
    %% test new handler
    Name1 = list_to_atom(lists:concat([?FUNCTION_NAME,"_A"])),
    LogFile1 = filename:join(PrivDir, Name1),
    ok = start_and_add(Name1, #{filter_default=>stop,
                                filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                                formatter=>{?MODULE,self()}},
                       #{file=>LogFile1}),
    logger:notice("hello", ?domain),
    logger_disk_log_h:filesync(Name1),
    ct:pal("Checking contents of ~p", [?log_no(LogFile1,1)]),
    try_read_file(?log_no(LogFile1,1), {ok,<<"hello\n">>}, 5000),
    
    %% test second handler
    Name2 = list_to_atom(lists:concat([?FUNCTION_NAME,"_B"])),
    DLName = lists:concat([?FUNCTION_NAME,"_B_log"]),
    LogFile2 = filename:join(PrivDir, DLName),
    ok = start_and_add(Name2, #{filter_default=>stop,
                                filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                                formatter=>{?MODULE,self()}},
                       #{file=>LogFile2}),
    logger:notice("dummy", ?domain),
    logger_disk_log_h:filesync(Name2),
    ct:pal("Checking contents of ~p", [?log_no(LogFile2,1)]),
    try_read_file(?log_no(LogFile2,1), {ok,<<"dummy\n">>}, 5000),

    remove_and_stop(Name1),    
    remove_and_stop(Name2),
    try_read_file(?log_no(LogFile1,1), {ok,<<"hello\ndummy\n">>}, 1),
    try_read_file(?log_no(LogFile2,1), {ok,<<"dummy\n">>}, 5000),
    ok.

open_existing_log(Config) ->
    PrivDir = ?config(priv_dir,Config),
    %% test new handler
    HName = ?FUNCTION_NAME,
    DLName = lists:concat([?FUNCTION_NAME,"_log"]),
    LogFile1 = filename:join(PrivDir, DLName),
    ok = start_and_add(HName, #{filter_default=>stop,
                                filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                                formatter=>{?MODULE,self()}},
                       #{file=>LogFile1}),
    logger:notice("one", ?domain),
    logger_disk_log_h:filesync(HName),
    ct:pal("Checking contents of ~p", [?log_no(LogFile1,1)]),
    try_read_file(?log_no(LogFile1,1), {ok,<<"one\n">>}, 5000),
    logger:notice("two", ?domain),
    ok = remove_and_stop(HName),
    try_read_file(?log_no(LogFile1,1), {ok,<<"one\ntwo\n">>}, 5000),

    logger:notice("two and a half", ?domain),

    ok = start_and_add(HName, #{filter_default=>stop,
                                filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                                formatter=>{?MODULE,self()}},
                       #{file=>LogFile1}),
    logger:notice("three", ?domain),
    logger_disk_log_h:filesync(HName),
    try_read_file(?log_no(LogFile1,1), {ok,<<"one\ntwo\nthree\n">>}, 5000),
    remove_and_stop(HName),
    try_read_file(?log_no(LogFile1,1), {ok,<<"one\ntwo\nthree\n">>}, 5000).

disk_log_opts(Config) ->
    Get = fun(Key, PL) -> proplists:get_value(Key, PL) end,
    PrivDir = ?config(priv_dir,Config),
    WName = list_to_atom(lists:concat([?FUNCTION_NAME,"_W"])),
    WFile = lists:concat([?FUNCTION_NAME,"_W_log"]),
    Size = length("12345"),
    ConfigW = #{filter_default=>stop,
                filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                formatter => {?MODULE,no_nl}},
    WFileFull = filename:join(PrivDir, WFile),
    DLOptsW = #{file => WFileFull,
                type => wrap,
                max_no_bytes => Size,
                max_no_files => 2},
    ok = start_and_add(WName, ConfigW, DLOptsW),
    WInfo1 = disk_log:info(WName),
    ct:log("Fullname = ~s", [WFileFull]),
    {WFileFull,wrap,{Size,2},1} = {Get(file,WInfo1),Get(type,WInfo1),
                                   Get(size,WInfo1),Get(current_file,WInfo1)},
    logger:notice("123", ?domain),
    logger_disk_log_h:filesync(WName),
    timer:sleep(500),
    1 = Get(current_file, disk_log:info(WName)),

    logger:notice("45", ?domain),
    logger_disk_log_h:filesync(WName),
    timer:sleep(500),
    1 = Get(current_file, disk_log:info(WName)),

    logger:notice("6", ?domain),
    logger_disk_log_h:filesync(WName),
    timer:sleep(500),
    2 = Get(current_file, disk_log:info(WName)),

    logger:notice("7890", ?domain),
    logger_disk_log_h:filesync(WName),
    timer:sleep(500),
    2 = Get(current_file, disk_log:info(WName)),

    HName1 = list_to_atom(lists:concat([?FUNCTION_NAME,"_H1"])),
    HFile1 = lists:concat([?FUNCTION_NAME,"_H1_log"]),
    ConfigH = #{filter_default=>stop,
                filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                formatter => {?MODULE,no_nl}},
    HFile1Full = filename:join(PrivDir, HFile1),
    DLOptsH1 = #{file => HFile1Full,
                 type => halt},
    ok = start_and_add(HName1, ConfigH, DLOptsH1),
    HInfo1 = disk_log:info(HName1),
    ct:log("Fullname = ~s", [HFile1Full]),
    {HFile1Full,halt,infinity} = {Get(file,HInfo1),Get(type,HInfo1),
                                  Get(size,HInfo1)},
    logger:notice("12345", ?domain),
    logger_disk_log_h:filesync(HName1),
    timer:sleep(500),
    1 = Get(no_written_items, disk_log:info(HName1)),

    HName2 = list_to_atom(lists:concat([?FUNCTION_NAME,"_H2"])),
    HFile2 = lists:concat([?FUNCTION_NAME,"_H2_log"]),
    HFile2Full = filename:join(PrivDir, HFile2),
    DLOptsH2 = DLOptsH1#{file => HFile2Full,
                         max_no_bytes => 1000},
    ok = start_and_add(HName2, ConfigH, DLOptsH2),
    HInfo3 = disk_log:info(HName2),
    ct:log("Fullname = ~s", [HFile2Full]),
    {HFile2Full,halt,1000} = {Get(file,HInfo3),Get(type,HInfo3),
                              Get(size,HInfo3)},
    
    remove_and_stop(WName),    
    remove_and_stop(HName1),
    remove_and_stop(HName2),    
    ok.

default_formatter(Config) ->
    PrivDir = ?config(priv_dir,Config),
    LogFile = filename:join(PrivDir,atom_to_list(?FUNCTION_NAME)),
    HandlerConfig = #{config => #{file=>LogFile},
                      filter_default=>log},
    ct:pal("Log: ~p", [LogFile]),
    ok = logger:add_handler(?MODULE, logger_disk_log_h, HandlerConfig),
    ok = logger:set_handler_config(?MODULE,formatter,
                                   {?DEFAULT_FORMATTER,?DEFAULT_FORMAT_CONFIG}),
    LogName = lists:concat([LogFile, ".1"]),
    logger:notice("dummy"),
    wait_until_written(LogName),
    {ok,Bin} = file:read_file(LogName),
    match = re:run(Bin, "=NOTICE REPORT====.*\ndummy", [{capture,none}]),
    ok.
default_formatter(cleanup, _Config) ->
    logger:remove_handler(?MODULE).

logging(Config) ->
    PrivDir = ?config(priv_dir,Config),
    %% test new handler
    Name = list_to_atom(lists:concat([?FUNCTION_NAME,"_1"])),
    LogFile = filename:join(PrivDir, Name),
    ok = start_and_add(Name, #{filter_default=>log,
                               formatter=>{?MODULE,self()}},
                       #{file => LogFile}),
    MsgFormatter = fun(Term) -> {"Term:~p",[Term]} end,
    logger:notice([{x,y}], #{report_cb => MsgFormatter}),
    logger:notice([{x,y}], #{}),
    ct:pal("Checking contents of ~p", [?log_no(LogFile,1)]),   
    try_read_file(?log_no(LogFile,1), {ok,<<"Term:[{x,y}]\n    x: y\n">>}, 5000).

logging(cleanup, _Config) ->
    Name = list_to_atom(lists:concat([?FUNCTION_NAME,"_1"])),
    remove_and_stop(Name).

filter_config(_Config) ->
    ok = logger:add_handler(?MODULE,logger_disk_log_h,#{}),
    {ok,#{config:=HConfig}=Config} = logger:get_handler_config(?MODULE),
    HConfig = maps:without([olp],HConfig),

    FakeFullHConfig = HConfig#{olp=>{regname,self(),erlang:make_ref()}},
    #{config:=HConfig} =
        logger_disk_log_h:filter_config(Config#{config=>FakeFullHConfig}),
    ok.

filter_config(cleanup,_Config) ->
    logger:remove_handler(?MODULE),
    ok.

errors(Config) ->
    PrivDir = ?config(priv_dir,Config),
    Name1 = list_to_atom(lists:concat([?FUNCTION_NAME,"_1"])),
    LogFile1 = filename:join(PrivDir,Name1),
    HandlerConfig = #{config=>#{file=>LogFile1},
                      filter_default=>log,
                      formatter=>{?MODULE,self()}},
    ok = logger:add_handler(Name1, logger_disk_log_h, HandlerConfig),
    {error,{already_exist,Name1}} =
        logger:add_handler(Name1, logger_disk_log_h, #{}),
 
    %%! TODO:
    %%! Check how bad log_opts are handled!

    {error,{illegal_config_change,
            logger_disk_log_h,
            #{type:=wrap},
            #{type:=halt}}} =
        logger:update_handler_config(Name1,
                                     config,
                                     #{type=>halt,
                                       file=>LogFile1}),

    {error,{illegal_config_change,
            logger_disk_log_h,
            #{file:=LogFile1},
            #{file:="newfilename"}}} =
        logger:update_handler_config(Name1,
                                     config,
                                     #{file=>"newfilename"}),

    %% Read-only fields may (accidentially) be included in the change,
    %% but it won't take effect
    {ok,C} = logger:get_handler_config(Name1),
    ok = logger:set_handler_config(Name1,config,#{olp=>dummyvalue}),
    {ok,C} = logger:get_handler_config(Name1),


    ok = logger:remove_handler(Name1),
    {error,{not_found,Name1}} = logger:remove_handler(Name1),
    ok.

errors(cleanup, _Config) ->
    Name1 = list_to_atom(lists:concat([?FUNCTION_NAME,"_1"])),
    _ = logger:remove_handler(Name1).

formatter_fail(Config) ->
    PrivDir = ?config(priv_dir,Config),
    Name = ?FUNCTION_NAME,
    LogFile = filename:join(PrivDir,Name),
    ct:pal("Log = ~p", [LogFile]),
    HandlerConfig = #{config => #{file=>LogFile},
                      filter_default=>stop,
                      filters=>?DEFAULT_HANDLER_FILTERS([?MODULE])},
    %% no formatter!
    logger:add_handler(Name, logger_disk_log_h, HandlerConfig),
    Pid = whereis(h_proc_name(Name)),
    true = is_pid(Pid),
    H = logger:get_handler_ids(),
    true = lists:member(Name,H),

    %% Formatter is added automatically
    {ok,#{formatter:={logger_formatter,_}}} = logger:get_handler_config(Name),
    logger:notice(M1=?msg,?domain),
    Got1 = try_match_file(?log_no(LogFile,1),"[0-9\\+\\-T:\\.]* notice: "++M1,5000),

    ok = logger:set_handler_config(Name,formatter,{nonexistingmodule,#{}}),
    logger:notice(M2=?msg,?domain),
    Got2 = try_match_file(?log_no(LogFile,1),
                          escape(Got1)++"[0-9\\+\\-T:\\.]* notice: FORMATTER CRASH: .*"++M2,
                          5000),

    ok = logger:set_handler_config(Name,formatter,{?MODULE,crash}),
    logger:notice(M3=?msg,?domain),
    Got3 = try_match_file(?log_no(LogFile,1),
                          escape(Got2)++"[0-9\\+\\-T:\\.]* notice: FORMATTER CRASH: .*"++M3,
                          5000),

    ok = logger:set_handler_config(Name,formatter,{?MODULE,bad_return}),
    logger:notice(?msg,?domain),
    try_match_file(?log_no(LogFile,1),
                   escape(Got3)++"FORMATTER ERROR: bad return value",
                   5000),

    %% Check that handler is still alive and was never dead
    Pid = whereis(h_proc_name(Name)),
    H = logger:get_handler_ids(),
    ok.

formatter_fail(cleanup,_Config) ->
    _ = logger:remove_handler(?FUNCTION_NAME),
    ok.

config_fail(_Config) ->
    {error,{handler_not_added,{invalid_config,logger_disk_log_h,#{bad:=bad}}}} =
        logger:add_handler(?MODULE,logger_disk_log_h,
                           #{config => #{bad => bad},
                             filter_default=>log,
                             formatter=>{?MODULE,self()}}),

    {error,{handler_not_added,{invalid_olp_levels,#{drop_mode_qlen:=1}}}} =
        logger:add_handler(?MODULE,logger_disk_log_h,
                           #{config => #{drop_mode_qlen=>1}}),
    {error,{handler_not_added,{invalid_olp_levels,#{sync_mode_qlen:=43,
                                                drop_mode_qlen:=42}}}} =
        logger:add_handler(?MODULE,logger_disk_log_h,
                           #{config => #{sync_mode_qlen=>43,
                                         drop_mode_qlen=>42}}),
    {error,{handler_not_added,{invalid_olp_levels,#{drop_mode_qlen:=43,
                                                flush_qlen:=42}}}} =
        logger:add_handler(?MODULE,logger_disk_log_h,
                           #{config => #{drop_mode_qlen=>43,
                                         flush_qlen=>42}}),

    ok = logger:add_handler(?MODULE,logger_disk_log_h,
                            #{filter_default=>log,
                              formatter=>{?MODULE,self()}}),
    %% can't change the disk log options for a log already in use
    {error,{illegal_config_change,logger_disk_log_h,_,_}} =
        logger:update_handler_config(?MODULE,config,
                                     #{max_no_files=>2}),
    %% incorrect values of OP params
    {ok,#{config := HConfig}} = logger:get_handler_config(?MODULE),
    {error,{invalid_olp_levels,_}} =
        logger:update_handler_config(?MODULE,config,
                                     HConfig#{sync_mode_qlen=>100,
                                              flush_qlen=>99}),
    %% invalid name of config parameter
    {error,{invalid_config,logger_disk_log_h,#{filesync_rep_int:=2000}}} =
        logger:update_handler_config(?MODULE, config,
                                     HConfig#{filesync_rep_int => 2000}),
    ok.
config_fail(cleanup,_Config) ->
    logger:remove_handler(?MODULE).

bad_input(_Config) ->
    {error,{badarg,{filesync,["BadType"]}}} =
        logger_disk_log_h:filesync("BadType").

reconfig(Config) ->
    Dir = ?config(priv_dir,Config),
    ok = logger:add_handler(?MODULE,
                            logger_disk_log_h,
                            #{filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()}}),
    #{%id := ?MODULE,
      sync_mode_qlen := ?SYNC_MODE_QLEN,
      drop_mode_qlen := ?DROP_MODE_QLEN,
      flush_qlen := ?FLUSH_QLEN,
      burst_limit_enable := ?BURST_LIMIT_ENABLE,
      burst_limit_max_count := ?BURST_LIMIT_MAX_COUNT,
      burst_limit_window_time := ?BURST_LIMIT_WINDOW_TIME,
      overload_kill_enable := ?OVERLOAD_KILL_ENABLE,
      overload_kill_qlen := ?OVERLOAD_KILL_QLEN,
      overload_kill_mem_size := ?OVERLOAD_KILL_MEM_SIZE,
      overload_kill_restart_after := ?OVERLOAD_KILL_RESTART_AFTER,
      cb_state :=
          #{handler_state :=
                #{log_opts := #{type := ?DISK_LOG_TYPE,
                                max_no_files := ?DISK_LOG_MAX_NO_FILES,
                                max_no_bytes := ?DISK_LOG_MAX_NO_BYTES,
                                file := DiskLogFile}},
            filesync_repeat_interval := ?FILESYNC_REPEAT_INTERVAL}} =
        logger_olp:info(h_proc_name()),
    {ok,#{config :=
              #{sync_mode_qlen := ?SYNC_MODE_QLEN,
                drop_mode_qlen := ?DROP_MODE_QLEN,
                flush_qlen := ?FLUSH_QLEN,
                burst_limit_enable := ?BURST_LIMIT_ENABLE,
                burst_limit_max_count := ?BURST_LIMIT_MAX_COUNT,
                burst_limit_window_time := ?BURST_LIMIT_WINDOW_TIME,
                overload_kill_enable := ?OVERLOAD_KILL_ENABLE,
                overload_kill_qlen := ?OVERLOAD_KILL_QLEN,
                overload_kill_mem_size := ?OVERLOAD_KILL_MEM_SIZE,
                overload_kill_restart_after := ?OVERLOAD_KILL_RESTART_AFTER,
                filesync_repeat_interval := ?FILESYNC_REPEAT_INTERVAL,
                file := DiskLogFile,
                max_no_files := ?DISK_LOG_MAX_NO_FILES,
                max_no_bytes := ?DISK_LOG_MAX_NO_BYTES,
                type := wrap} = HConfig0}} =
        logger:get_handler_config(?MODULE),

    HConfig1 = HConfig0#{sync_mode_qlen => 1,
                         drop_mode_qlen => 2,
                         flush_qlen => 3,
                         burst_limit_enable => false,
                         burst_limit_max_count => 10,
                         burst_limit_window_time => 10,
                         overload_kill_enable => true,
                         overload_kill_qlen => 100000,
                         overload_kill_mem_size => 10000000,
                         overload_kill_restart_after => infinity,
                         filesync_repeat_interval => no_repeat},
    ok = logger:set_handler_config(?MODULE, config, HConfig1),
    #{%id := ?MODULE,
      sync_mode_qlen := 1,
      drop_mode_qlen := 2,
      flush_qlen := 3,
      burst_limit_enable := false,
      burst_limit_max_count := 10,
      burst_limit_window_time := 10,
      overload_kill_enable := true,
      overload_kill_qlen := 100000,
      overload_kill_mem_size := 10000000,
      overload_kill_restart_after := infinity,
      cb_state := #{filesync_repeat_interval := no_repeat}} =
        logger_olp:info(h_proc_name()),
    {ok,#{config:=HConfig1}} = logger:get_handler_config(?MODULE),

    ok = logger:update_handler_config(?MODULE, config,
                                      #{flush_qlen => ?FLUSH_QLEN}),
    {ok,#{config:=C1}} = logger:get_handler_config(?MODULE),
    ct:log("C1: ~p",[C1]),
    C1 = HConfig1#{flush_qlen => ?FLUSH_QLEN},

    ok = logger:set_handler_config(?MODULE, config, #{sync_mode_qlen => 1}),
    {ok,#{config:=C2}} = logger:get_handler_config(?MODULE),
    ct:log("C2: ~p",[C2]),
    C2 = HConfig0#{sync_mode_qlen => 1},

    ok = logger:set_handler_config(?MODULE, config, #{drop_mode_qlen => 100}),
    {ok,#{config:=C3}} = logger:get_handler_config(?MODULE),
    ct:log("C3: ~p",[C3]),
    C3 = HConfig0#{drop_mode_qlen => 100},

    ok = logger:update_handler_config(?MODULE, config, #{sync_mode_qlen => 1}),
    {ok,#{config:=C4}} = logger:get_handler_config(?MODULE),
    ct:log("C4: ~p",[C4]),
    C4 = HConfig0#{sync_mode_qlen => 1,
                   drop_mode_qlen => 100},

    ok = logger:remove_handler(?MODULE),

    File = filename:join(Dir, "logfile"),
    ok = logger:add_handler(?MODULE,
                            logger_disk_log_h,
                            #{filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()},
                              config=>
                                  #{type => halt,
                                    max_no_files => 1,
                                    max_no_bytes => 1024,
                                    file => File}}),
    #{cb_state :=
          #{handler_state :=
                #{log_opts := #{type := halt,
                                max_no_files := 1,
                                max_no_bytes := 1024,
                                file := File}}}} =
        logger_olp:info(h_proc_name()),
    {ok,#{config :=
              #{type := halt,
                max_no_files := 1,
                max_no_bytes := 1024,
                file := File}=HaltHConfig} = Config2} =
        logger:get_handler_config(?MODULE),

    ok = logger:update_handler_config(?MODULE, level, notice),
    {ok,C5} = logger:get_handler_config(?MODULE),
    ct:log("C5: ~p",[C5]),
    C5 = Config2#{level => notice},

    ok = logger:set_handler_config(?MODULE, level, info),
    {ok,C6} = logger:get_handler_config(?MODULE),
    ct:log("C6: ~p",[C6]),
    C6 = Config2#{level => info},

    %% You are not allowed to actively set the write once fields
    %% (type, max_no_files, max_no_bytes, file) in runtime.
    {error, {illegal_config_change,_,_,_}} =
        logger:set_handler_config(?MODULE,config,#{type=>wrap}),
    {error, {illegal_config_change,_,_,_}} =
        logger:set_handler_config(?MODULE,config,#{max_no_files=>2}),
    {error, {illegal_config_change,_,_,_}} =
        logger:set_handler_config(?MODULE,config,#{max_no_bytes=>2048}),
    {error, {illegal_config_change,_,_,_}} =
        logger:set_handler_config(?MODULE,config,#{file=>"otherfile.log"}),
    {ok,C7} = logger:get_handler_config(?MODULE),
    ct:log("C7: ~p",[C7]),
    C7 = C6,

    %%  ... but if you don't specify the write once fields, then
    %%  set_handler_config shall NOT reset them to their default value
    ok = logger:set_handler_config(?MODULE,config,#{sync_mode_qlen=>1}),
    {ok,#{config:=C8}} = logger:get_handler_config(?MODULE),
    ct:log("C8: ~p",[C8]),
    C8 = HaltHConfig#{sync_mode_qlen=>1},
    ok.

reconfig(cleanup, _Config) ->
    logger:remove_handler(?MODULE).

sync(Config) ->
    Dir = ?config(priv_dir,Config),
    File = filename:join(Dir, ?FUNCTION_NAME),
    Log = lists:concat([File,".1"]),
    ok = logger:add_handler(?MODULE,
                            logger_disk_log_h,
                            #{config => #{file => File},
                              filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,nl}}),

    start_tracer([{logger_disk_log_h,disk_log_write,3},
                  {disk_log,sync,1}],
                 [{logger_disk_log_h,disk_log_write,<<"first\n">>},
                  {disk_log,sync}]),

    logger:notice("first", ?domain),
    %% wait for automatic disk_log_sync
    check_tracer(?FILESYNC_REPEAT_INTERVAL*2),
    
    %% check that if there's no repeated filesync active,
    %% a disk_log_sync is still performed when handler goes idle
    {ok,#{config := HConfig}} = logger:get_handler_config(?MODULE),
    HConfig1 = HConfig#{filesync_repeat_interval => no_repeat},
    ok = logger:update_handler_config(?MODULE, config, HConfig1),
    no_repeat = maps:get(filesync_repeat_interval,
                         maps:get(cb_state,logger_olp:info(h_proc_name()))),

    start_tracer([{logger_disk_log_h,disk_log_write,3},
                  {disk_log,sync,1}],
                 [{logger_disk_log_h,disk_log_write,<<"second\n">>},
                  {disk_log,sync},
                  {logger_disk_log_h,disk_log_write,<<"third\n">>},
                  {disk_log,sync}]),

    logger:notice("second", ?domain),
    timer:sleep(?IDLE_DETECT_TIME*2),
    logger:notice("third", ?domain),
    %% wait for automatic disk_log_sync
    check_tracer(?IDLE_DETECT_TIME*2),

    try_read_file(Log, {ok,<<"first\nsecond\nthird\n">>}, 1000),
    
    %% switch repeated filesync on and verify that the looping works
    SyncInt = 1000,
    WaitT = 4500,
    OneSync = {logger_h_common,handle_cast,repeated_filesync},
    %% receive 1 repeated_filesync per sec
    start_tracer([{{logger_h_common,handle_cast,2},
                   [{[repeated_filesync,'_'],[],[{message,{caller}}]}]}],
                 [OneSync || _ <- lists:seq(1, trunc(WaitT/SyncInt))]),

    HConfig2 = HConfig#{filesync_repeat_interval => SyncInt},
    ok = logger:update_handler_config(?MODULE, config, HConfig2),
                      
    SyncInt = maps:get(filesync_repeat_interval,
                       maps:get(cb_state,logger_olp:info(h_proc_name()))),
    timer:sleep(WaitT),
    HConfig3 = HConfig#{filesync_repeat_interval => no_repeat},
    ok = logger:update_handler_config(?MODULE, config, HConfig3),
    check_tracer(100),
    ok.
sync(cleanup,_Config) ->
    dbg:stop_clear(),
    logger:remove_handler(?MODULE).

disk_log_wrap(Config) ->
    Get = fun(Key, PL) -> proplists:get_value(Key, PL) end,
    Dir = ?config(priv_dir,Config),
    File = filename:join(Dir, ?FUNCTION_NAME),
    ct:pal("Log = ~p", [File]),
    MaxFiles = 3,
    MaxBytes = 5,
    ok = logger:add_handler(?MODULE,
                            logger_disk_log_h,
                            #{filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()},
                              config=>
                                  #{type => wrap,
                                    max_no_files => MaxFiles,
                                    max_no_bytes => MaxBytes,
                                    file => File}}),
    Info = disk_log:info(?MODULE),
    {File,wrap,{MaxBytes,MaxFiles},1} =
        {Get(file,Info),Get(type,Info),Get(size,Info),Get(current_file,Info)},
    Tester = self(),
    TraceFun = fun({trace,_,call,{Mod,Func,Details}}, Pid) ->
                       Pid ! {trace,Mod,Func,Details},
                       Pid
               end,
    {ok,_} = dbg:tracer(process, {TraceFun, Tester}),
    {ok,_} = dbg:p(whereis(h_proc_name()), [c]),
    {ok,_} = dbg:tp(logger_disk_log_h, handle_info, 3, []),

    Text = [34 + rand:uniform(126-34) || _ <- lists:seq(1,MaxBytes)],
    ct:pal("String = ~p (~w)", [Text, erts_debug:size(Text)]),
    %% fill first file
    lists:foreach(fun(N) ->
                          Log = lists:concat([File,".",N]),
                          logger:notice(Text, ?domain),
                          wait_until_written(Log),
                          ct:pal("N = ~w",
                                 [N = Get(current_file,
                                          disk_log:info(?MODULE))])
                  end, lists:seq(1,MaxFiles)),

    %% wait for trace messages
    timer:sleep(1000),
    dbg:stop_clear(),
    Received = lists:flatmap(fun({trace,_M,handle_info,
                                  [_,{disk_log,_Node,_Name,What},_]}) ->
                                     [{trace,What}];
                                ({log,_}) ->
                                     []
                             end, test_server:messages_get()),
    ct:pal("Trace =~n~p", [Received]),
    Received = [{trace,{wrap,0}} || _ <- lists:seq(1,MaxFiles-1)],
    ok.

disk_log_wrap(cleanup,_Config) ->
    dbg:stop_clear(),
    logger:remove_handler(?MODULE).

disk_log_full(Config) ->
    Dir = ?config(priv_dir,Config),
    File = filename:join(Dir, ?FUNCTION_NAME),
    ct:pal("Log = ~p", [File]),
    MaxBytes = 50,
    ok = logger:add_handler(?MODULE,
                            logger_disk_log_h,
                            #{filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()},
                              config=>
                                  #{type => halt,
                                    max_no_files => 1,
                                    max_no_bytes => MaxBytes,
                                    file => File}}),

    Tester = self(),
    TraceFun = fun({trace,_,call,{Mod,Func,Details}}, Pid) ->
                       Pid ! {trace,Mod,Func,Details},
                       Pid
               end,
    {ok,_} = dbg:tracer(process, {TraceFun, Tester}),
    {ok,_} = dbg:p(whereis(h_proc_name()), [c]),
    {ok,_} = dbg:tp(logger_disk_log_h, handle_info, 3, []),

    NoOfChars = 5,
    Text = [34 + rand:uniform(126-34) || _ <- lists:seq(1,NoOfChars)],
    [logger:notice(Text, ?domain) || _ <- lists:seq(1,trunc(MaxBytes/NoOfChars)+1)],

    %% wait for trace messages
    timer:sleep(2000),
    dbg:stop_clear(),
    Received = lists:flatmap(fun({trace,_M,handle_info,
                                  [_,{disk_log,_Node,_Name,What},_]}) ->
                                     [{trace,What}];
                                ({log,_}) ->
                                     []
                             end, test_server:messages_get()),
    ct:pal("Trace =~n~p", [Received]),

    %% The tail here could be an error_status notification, if the
    %% last write was synchronous, but in most cases it will not be
    [{trace,full}|_] = Received,
    %% [{trace,full},
    %%  {trace,{error_status,{error,{full,_}}}}] = Received,
    ok.
disk_log_full(cleanup, _Config) ->
    dbg:stop_clear(),
    logger:remove_handler(?MODULE).    

disk_log_events(_Config) ->
    Node = node(),
    Log = ?MODULE,
    ok = logger:add_handler(?MODULE,
                            logger_disk_log_h,
                            #{filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()}}),

    %% Events copied from disk_log API
    Events =
        [{disk_log, Node, Log, {wrap, 0}},
         {disk_log, Node, Log, {truncated, 0}},
         {disk_log, Node, Log, {read_only, 42}},
         {disk_log, Node, Log, {blocked_log, 42}},
         {disk_log, Node, Log, {format_external, 42}},
         {disk_log, Node, Log, full},
         {disk_log, Node, Log, {error_status, ok}}],
    
    Tester = self(),
    TraceFun = fun({trace,_,call,{Mod,Func,Details}}, Pid) ->
                       Pid ! {trace,Mod,Func,Details},
                       Pid
               end,
    {ok,_} = dbg:tracer(process, {TraceFun, Tester}),
    {ok,_} = dbg:p(whereis(h_proc_name()), [c]),
    {ok,_} = dbg:tp(logger_disk_log_h, handle_info, 3, []),
    
    [whereis(h_proc_name()) ! E || E <- Events],
    %% wait for trace messages
    timer:sleep(2000),
    dbg:stop_clear(),
    Received = lists:map(fun({trace,_M,handle_info,
                              [_,Got,_]}) -> Got
                         end, test_server:messages_get()),
    ct:pal("Trace =~n~p", [Received]),
    NoOfEvents = length(Events),
    NoOfEvents = length(Received),
    lists:foreach(fun(Event) ->
                          true = lists:member(Event, Received)
                  end, Received),
    ok.
disk_log_events(cleanup, _Config) ->
    dbg:stop_clear(),
    logger:remove_handler(?MODULE).    

write_failure(Config) ->
    Dir = ?config(priv_dir, Config),
    File = filename:join(Dir, ?FUNCTION_NAME),
    Log = lists:concat([File,".1"]),
    ct:pal("Log = ~p", [Log]),

    Node = start_h_on_new_node(Config, File),
    false = (undefined == rpc:call(Node, ets, whereis, [?TEST_HOOKS_TAB])),
    rpc:call(Node, ets, insert, [?TEST_HOOKS_TAB,{tester,self()}]),
    rpc:call(Node, ?MODULE, set_internal_log, [?MODULE,internal_log]),
    rpc:call(Node, ?MODULE, set_result, [disk_log_write,ok]),
    HState = rpc:call(Node, logger_olp, info, [h_proc_name(?STANDARD_HANDLER)]),
    LogOpts = maps:get(log_opts,
                       maps:get(handler_state,
                                maps:get(cb_state,HState))),
    ct:pal("LogOpts = ~p", [LogOpts]),
           
    %% ?check and ?check_no_log in this test only check for internal log events
    ok = log_on_remote_node(Node, "Logged1"),
    rpc:call(Node, logger_disk_log_h, filesync, [?STANDARD_HANDLER]),
    ?check_no_log, % no internal log when write ok

    SyncRepInt = case (fun() -> is_atom(?FILESYNC_REPEAT_INTERVAL) end)() of
                     true -> 5500;
                     false -> ?FILESYNC_REPEAT_INTERVAL + 500
                 end,

    try_read_file(Log, {ok,<<"Logged1\n">>}, SyncRepInt),

    rpc:call(Node, ?MODULE, set_result, [disk_log_write,{error,no_such_log}]),
    ok = log_on_remote_node(Node, "Cause simple error printout"),

    %% this should have caused an internal log
    ?check({error,{?STANDARD_HANDLER,log,LogOpts,{error,no_such_log}}}),

    ok = log_on_remote_node(Node, "No second error printout"),
    ?check_no_log, % but don't log same error twice

    rpc:call(Node, ?MODULE, set_result, [disk_log_write,
                                         {error,{full,?STANDARD_HANDLER}}]),
    ok = log_on_remote_node(Node, "Cause simple error printout"),
    %% this was a different error, so it should be logged
    ?check({error,{?STANDARD_HANDLER,log,LogOpts,
                   {error,{full,?STANDARD_HANDLER}}}}),

    rpc:call(Node, ?MODULE, set_result, [disk_log_write,ok]),
    ok = log_on_remote_node(Node, "Logged2"),
    rpc:call(Node, logger_disk_log_h, filesync, [?STANDARD_HANDLER]),
    ?check_no_log, % no internal log when write ok
    try_read_file(Log, {ok,<<"Logged1\nLogged2\n">>}, SyncRepInt),
    ok.
write_failure(cleanup, _Config) ->
    Nodes = nodes(),
    [test_server:stop_node(Node) || Node <- Nodes].


sync_failure(Config) ->
    Dir = ?config(priv_dir, Config),
    FileName = lists:concat([?MODULE,"_",?FUNCTION_NAME]),
    File = filename:join(Dir, FileName),


    Node = start_h_on_new_node(Config, File),
    false = (undefined == rpc:call(Node, ets, whereis, [?TEST_HOOKS_TAB])),
    rpc:call(Node, ets, insert, [?TEST_HOOKS_TAB,{tester,self()}]),
    rpc:call(Node, ?MODULE, set_internal_log, [?MODULE,internal_log]),
    rpc:call(Node, ?MODULE, set_result, [disk_log_sync,ok]),
    HState = rpc:call(Node, logger_olp, info, [h_proc_name(?STANDARD_HANDLER)]),
    LogOpts = maps:get(log_opts, maps:get(handler_state,
                                          maps:get(cb_state,HState))),
    
    SyncInt = 500,
    ok = rpc:call(Node, logger, update_handler_config,
                  [?STANDARD_HANDLER, config,
                   #{filesync_repeat_interval => SyncInt}]),
    Info = rpc:call(Node, logger_olp, info, [h_proc_name(?STANDARD_HANDLER)]),
    SyncInt = maps:get(filesync_repeat_interval, maps:get(cb_state, Info)),
    
    ok = log_on_remote_node(Node, "Logged1"),
    ?check_no_log,

    rpc:call(Node, ?MODULE, set_result, [disk_log_sync,{error,no_such_log}]),
    ok = log_on_remote_node(Node, "Cause simple error printout"),
    
    ?check({error,{?STANDARD_HANDLER,filesync,LogOpts,{error,no_such_log}}}),

    ok = log_on_remote_node(Node, "No second error printout"),
    ?check_no_log,

    rpc:call(Node, ?MODULE, set_result,
             [disk_log_sync,{error,{blocked_log,?STANDARD_HANDLER}}]),
    ok = log_on_remote_node(Node, "Cause simple error printout"),
    ?check({error,{?STANDARD_HANDLER,filesync,LogOpts,
                   {error,{blocked_log,?STANDARD_HANDLER}}}}),

    rpc:call(Node, ?MODULE, set_result, [disk_log_sync,ok]),
    ok = log_on_remote_node(Node, "Logged2"),
    ?check_no_log,
    ok.
sync_failure(cleanup, _Config) ->
    Nodes = nodes(),
    [test_server:stop_node(Node) || Node <- Nodes].

start_h_on_new_node(Config, File) ->
    {ok,_,Node} =
        logger_test_lib:setup(
          Config,
          [{logger,[{handler,default,logger_disk_log_h,
                     #{ config => #{ file => File }}}]}]),
    ok = rpc:call(Node,logger,set_handler_config,[?STANDARD_HANDLER,formatter,
                                                  {?MODULE,nl}]),
    Node.

log_on_remote_node(Node,Msg) ->
    _ = spawn_link(Node,
                   fun() -> erlang:group_leader(whereis(user),self()),
                            logger:notice(Msg)
                   end),
    ok.

%% functions for test hook macros to be called by rpc
set_internal_log(_Mod, _Func) ->
    ?set_internal_log({_Mod,_Func}).
set_result(_Op, _Result) ->
    ?set_result(_Op, _Result).
set_defaults() ->
    ?set_defaults().

%% internal log function that sends the term to the test case process
internal_log(Type, Term) ->
    [{tester,Tester}] = ets:lookup(?TEST_HOOKS_TAB, tester),
    Tester ! {log,{Type,Term}},
    logger:internal_log(Type, Term),
    ok.


%%%-----------------------------------------------------------------
%%% Overload protection tests

op_switch_to_sync(Config) ->
    {Log,HConfig,DLHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    NumOfReqs = 500,
    NewHConfig =
        HConfig#{config => DLHConfig#{sync_mode_qlen => 2,
                                      drop_mode_qlen => NumOfReqs+1,
                                      flush_qlen => 2*NumOfReqs,
                                      burst_limit_enable => false}},
    ok = logger:update_handler_config(?MODULE, NewHConfig),
    send_burst({n,NumOfReqs}, seq, {chars,79}, notice),
    Lines = count_lines(Log),
    NumOfReqs = Lines,
    ok = file_delete(Log),
    ok.
op_switch_to_sync(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

op_switch_to_drop() ->
    [{timetrap,{seconds,180}}].
op_switch_to_drop(Config) ->
    Test =
        fun() ->
                {Log,HConfig,DLHConfig} =
                    start_handler(?MODULE, ?FUNCTION_NAME, Config),
                NumOfReqs = 300,
                Procs = 2,
                Bursts = 10,
                NewHConfig =
                    HConfig#{config =>
                                 DLHConfig#{sync_mode_qlen => 1,
                                            drop_mode_qlen => 2,
                                            flush_qlen => Procs*NumOfReqs*Bursts,
                                            burst_limit_enable => false}},
                ok = logger:update_handler_config(?MODULE, NewHConfig),
                %% It sometimes happens that the handler either gets
                %% the requests in a slow enough pace so that dropping
                %% never occurs. Therefore, lets generate a number of
                %% bursts to increase the chance of message buildup.
                [send_burst({n,NumOfReqs}, {spawn,Procs,0}, {chars,79}, notice) ||
                    _ <- lists:seq(1, Bursts)],
                Logged = count_lines(Log),
                ok = stop_handler(?MODULE),
                ct:pal("Number of messages dropped = ~w (~w)",
                       [Procs*NumOfReqs*Bursts-Logged,Procs*NumOfReqs*Bursts]),
                true = (Logged < (Procs*NumOfReqs*Bursts)),
                true = (Logged > 0),
                _ = file_delete(Log),
                ok
        end,
    %% As it's tricky to get the timing right in only one go, we perform the
    %% test repeatedly, hoping that will generate a successful result.
    case repeat_until_ok(Test, 10) of
        {ok,{Failures,_Result}} ->
            ct:log("Failed ~w times before success!", [Failures]);
        {fails,Reason} ->
            ct:fail(Reason)
    end.    
op_switch_to_drop(cleanup, _Config) ->
    _  = stop_handler(?MODULE).

op_switch_to_flush() ->
    [{timetrap,{minutes,3}}].
op_switch_to_flush(Config) ->
    Test =
        fun() ->
                {Log,HConfig,DLHConfig} =
                    start_handler(?MODULE, ?FUNCTION_NAME, Config),
                
                %% NOTE: it's important that both async and sync
                %% requests have been queued when the flush happens
                %% (verify with coverage of flush_log_requests/2)
    
                NewHConfig =
                    HConfig#{config =>
                                 DLHConfig#{sync_mode_qlen => 2,
                                            %% disable drop mode
                                            drop_mode_qlen => 300,
                                            flush_qlen => 300,
                                            burst_limit_enable => false}},    
                ok = logger:update_handler_config(?MODULE, NewHConfig),
                NumOfReqs = 1500,
                Procs = 10,
                Bursts = 10,
                %% It sometimes happens that the handler either gets
                %% the requests in a slow enough pace so that flushing
                %% never occurs, or it gets all messages at once,
                %% causing all messages to get flushed (no dropping of
                %% sync messages gets tested). Therefore, lets
                %% generate a number of bursts to increase the chance
                %% of message buildup in some random fashion.
                [send_burst({n,NumOfReqs}, {spawn,Procs,0}, {chars,79}, notice) ||
                    _ <- lists:seq(1,Bursts)],
                Logged = count_lines(Log),
                ok= stop_handler(?MODULE),
                ct:pal("Number of messages flushed/dropped = ~w (~w)",
                       [NumOfReqs*Procs*Bursts-Logged,NumOfReqs*Procs*Bursts]),
                true = (Logged < (NumOfReqs*Procs*Bursts)),
                true = (Logged > 0),
                _ = file_delete(Log),
                ok
        end,
    %% As it's tricky to get the timing right in only one go, we perform the
    %% test repeatedly, hoping that will generate a successful result.
    case repeat_until_ok(Test, 10) of
        {ok,{Failures,_Result}} ->
            ct:log("Failed ~w times before success!", [Failures]);
        {fails,Reason} ->
            ct:fail(Reason)
    end.
op_switch_to_flush(cleanup, _Config) ->
    _  = stop_handler(?MODULE).


limit_burst_disabled(Config) ->
    {Log,HConfig,DLHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    NewHConfig =
        HConfig#{config => DLHConfig#{burst_limit_enable => false,
                                      burst_limit_max_count => 10,
                                      burst_limit_window_time => 2000,
                                      drop_mode_qlen => 200,
                                      flush_qlen => 300}},
    ok = logger:update_handler_config(?MODULE, NewHConfig),
    NumOfReqs = 100,
    send_burst({n,NumOfReqs}, seq, {chars,79}, notice),
    Logged = count_lines(Log),
    ct:pal("Number of messages logged = ~w", [Logged]),
    NumOfReqs = Logged,
    ok = file_delete(Log),
    ok.
limit_burst_disabled(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

limit_burst_enabled_one(Config) ->
    {Log,HConfig,DLHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    ReqLimit = 10,
    NewHConfig =
        HConfig#{config => DLHConfig#{burst_limit_enable => true,
                                      burst_limit_max_count => ReqLimit,
                                      burst_limit_window_time => 2000,
                                      drop_mode_qlen => 200,
                                      flush_qlen => 300}},
    ok = logger:update_handler_config(?MODULE, NewHConfig),
    NumOfReqs = 100,
    send_burst({n,NumOfReqs}, seq, {chars,79}, notice),
    Logged = count_lines(Log),
    ct:pal("Number of messages logged = ~w", [Logged]),
    ReqLimit = Logged,
    ok = file_delete(Log),
    ok.
limit_burst_enabled_one(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

limit_burst_enabled_period(Config) ->
    {Log,HConfig,DLHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    ReqLimit = 10,
    BurstTWin = 1000,
    NewHConfig =
        HConfig#{config => DLHConfig#{burst_limit_enable => true,
                                      burst_limit_max_count => ReqLimit,
                                      burst_limit_window_time => BurstTWin,
                                      drop_mode_qlen => 20000,
                                      flush_qlen => 20001}},
    ok = logger:update_handler_config(?MODULE, NewHConfig),
    
    Windows = 3,
    Sent = send_burst({t,BurstTWin*Windows}, seq, {chars,79}, notice),
    Logged = count_lines(Log),
    ct:pal("Number of messages sent = ~w~nNumber of messages logged = ~w",
           [Sent,Logged]),
    true = (Logged > (ReqLimit*Windows)) andalso
           (Logged < (ReqLimit*(Windows+2))),
    ok = file_delete(Log),
    ok.
limit_burst_enabled_period(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

kill_disabled(Config) ->
    {Log,HConfig,DLHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    NewHConfig =
        HConfig#{config=>DLHConfig#{overload_kill_enable=>false,
                                    overload_kill_qlen=>10,
                                    overload_kill_mem_size=>100}},
    ok = logger:update_handler_config(?MODULE, NewHConfig),
    NumOfReqs = 100,
    send_burst({n,NumOfReqs}, seq, {chars,79}, notice),
    Logged = count_lines(Log),
    ct:pal("Number of messages logged = ~w", [Logged]),
    ok = file_delete(Log),
    true = is_pid(whereis(h_proc_name())),
    ok.
kill_disabled(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

qlen_kill_new(Config) ->
    {Log,HConfig,DLHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    Pid0 = whereis(h_proc_name()),
    {_,Mem0} = process_info(Pid0, memory),
    RestartAfter = ?OVERLOAD_KILL_RESTART_AFTER,
    NewHConfig =
        HConfig#{config =>
                     DLHConfig#{overload_kill_enable=>true,
                                overload_kill_qlen=>10,
                                overload_kill_mem_size=>Mem0+50000,
                                overload_kill_restart_after=>RestartAfter}},
    ok = logger:update_handler_config(?MODULE, NewHConfig),
    MRef = erlang:monitor(process, Pid0),
    NumOfReqs = 100,
    Procs = 4,
    send_burst({n,NumOfReqs}, {spawn,Procs,0}, {chars,79}, notice),
    %% send_burst({n,NumOfReqs}, seq, {chars,79}, notice),
    receive
        {'DOWN', MRef, _, _, Info} ->
           case Info of
                {shutdown,{overloaded,QLen,Mem}} ->
                    ct:pal("Terminated with qlen = ~w, mem = ~w", [QLen,Mem]);
                killed ->
                    ct:pal("Slow shutdown, handler process was killed!", [])
            end,
            file_delete(Log),
            {ok,_} = wait_for_process_up(RestartAfter * 3),
            ok
    after
        5000 ->
            Info = logger_olp:info(h_proc_name()),
            ct:pal("Handler state = ~p", [Info]),
            ct:fail("Handler not dead! It should not have survived this!")
    end.
qlen_kill_new(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

mem_kill_new(Config) ->
    {Log,HConfig,DLHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    Pid0 = whereis(h_proc_name()),
    {_,Mem0} = process_info(Pid0, memory),
    RestartAfter = ?OVERLOAD_KILL_RESTART_AFTER,
    NewHConfig =
        HConfig#{config =>
                     DLHConfig#{overload_kill_enable=>true,
                                overload_kill_qlen=>50000,
                                overload_kill_mem_size=>Mem0+500,
                                overload_kill_restart_after=>RestartAfter}},
    ok = logger:update_handler_config(?MODULE, NewHConfig),
    MRef = erlang:monitor(process, Pid0),
    NumOfReqs = 100,
    Procs = 4,
    send_burst({n,NumOfReqs}, {spawn,Procs,0}, {chars,79}, notice),
    %% send_burst({n,NumOfReqs}, seq, {chars,79}, notice),
    receive
        {'DOWN', MRef, _, _, Info} ->
            case Info of
                {shutdown,{overloaded,QLen,Mem}} ->
                    ct:pal("Terminated with qlen = ~w, mem = ~w", [QLen,Mem]);
                killed ->
                    ct:pal("Slow shutdown, handler process was killed!", [])
            end,
            file_delete(Log),
            {ok,_} = wait_for_process_up(RestartAfter * 3),
            ok
    after
        5000 ->
            Info = logger_olp:info(h_proc_name()),
            ct:pal("Handler state = ~p", [Info]),
            ct:fail("Handler not dead! It should not have survived this!")
    end.
mem_kill_new(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

restart_after() ->
    [{timetrap,{minutes,2}}].
restart_after(Config) ->
    {Log,HConfig,DLHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    NewHConfig1 =
        HConfig#{config=>DLHConfig#{overload_kill_enable=>true,
                                    overload_kill_qlen=>4,
                                    overload_kill_restart_after=>infinity}},
    ok = logger:update_handler_config(?MODULE, NewHConfig1),
    MRef1 = erlang:monitor(process, whereis(h_proc_name())),
    %% kill handler
    send_burst({n,100}, {spawn,5,0}, {chars,79}, notice),
    receive
        {'DOWN', MRef1, _, _, _Reason1} ->
            file_delete(Log),
            error = wait_for_process_up(?OVERLOAD_KILL_RESTART_AFTER * 3),
            ok
    after
        5000 ->
            Info1 = logger_olp:info(h_proc_name()),
            ct:pal("Handler state = ~p", [Info1]),
            ct:fail("Handler not dead! It should not have survived this!")
    end,
    
    {Log,_,_} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    RestartAfter = ?OVERLOAD_KILL_RESTART_AFTER,
    NewHConfig2 =
        HConfig#{config=>DLHConfig#{overload_kill_enable=>true,
                                    overload_kill_qlen=>4,
                                    overload_kill_restart_after=>RestartAfter}},
    ok = logger:update_handler_config(?MODULE, NewHConfig2),
    Pid0 = whereis(h_proc_name()),
    MRef2 = erlang:monitor(process, Pid0),
    %% kill handler
    send_burst({n,100}, {spawn,5,0}, {chars,79}, notice),
    receive
        {'DOWN', MRef2, _, _, _Reason2} ->
            file_delete(Log),
            {ok,Pid1} = wait_for_process_up(RestartAfter * 3),
            false = (Pid1 == Pid0),
            ok
    after
        5000 ->
            Info2 = logger_olp:info(h_proc_name()),
            ct:pal("Handler state = ~p", [Info2]),
            ct:fail("Handler not dead! It should not have survived this!")
    end,
    ok.
restart_after(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

%% send handler requests (sync, info, reset, change_config)
%% during high load to verify that sync, dropping and flushing is
%% handled correctly.
handler_requests_under_load() ->
    [{timetrap,{minutes,5}}].
handler_requests_under_load(Config) ->
    {Log,HConfig,DLHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    NewHConfig =
        HConfig#{config => DLHConfig#{sync_mode_qlen => 2,
                                      drop_mode_qlen => 1000,
                                      flush_qlen => 2000,
                                      burst_limit_enable => false}},
    ok = logger:update_handler_config(?MODULE, NewHConfig),
    Pid = spawn_link(
            fun() -> send_requests(1,[{logger_disk_log_h,filesync,[?MODULE],[]},
                                      {logger_olp,info,[h_proc_name()],[]},
                                      {logger_olp,reset,[h_proc_name()],[]},
                                      {logger,update_handler_config,
                                       [?MODULE, config,
                                        #{overload_kill_enable => false}],
                                       []}])
            end),
    Procs = 100,
    Sent = Procs * send_burst({n,5000}, {spawn,Procs,10}, {chars,79}, notice),
    Pid ! {self(),finish},
    ReqResult = receive {Pid,Result} -> Result end,
    Logged = count_lines(Log),
    ct:pal("Number of messages sent = ~w~nNumber of messages logged = ~w",
           [Sent,Logged]),
    FindError = fun(Res) ->
                        [E || E <- Res,
                              is_tuple(E) andalso (element(1,E) == error)]
                end,
    Errors = [{Func,FindError(Res)} || {_,Func,_,Res} <- ReqResult],
    NoOfReqs = lists:foldl(fun({_,_,_,Res}, N) -> N + length(Res) end,
                           0, ReqResult),
    ct:pal("~w requests made. Errors: ~n~p", [NoOfReqs,Errors]),
    ok = file_delete(Log).
handler_requests_under_load(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

send_requests(TO, Reqs = [{Mod,Func,Args,Res}|Rs]) ->
    receive
        {From,finish} ->
            From ! {self(),Reqs}
    after
        TO ->
            Result = apply(Mod,Func,Args),
            send_requests(TO, Rs ++ [{Mod,Func,Args,[Result|Res]}])
    end.

%%%-----------------------------------------------------------------
%%% 
start_handler(Name, FuncName, Config) ->
    Dir = ?config(priv_dir,Config),
    File = filename:join(Dir, FuncName),
    ct:pal("Logging to ~tp", [File]),
    FullFile = lists:concat([File,".1"]),
    _ = file_delete(FullFile),
    ok = logger:add_handler(Name,
                            logger_disk_log_h,
                            #{config=>#{file => File,
                                        max_no_files => 1,
                                        max_no_bytes => 100000000},
                              filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([Name]),
                              formatter=>{?MODULE,op}}),
    {ok,HConfig = #{config := DLHConfig}} = logger:get_handler_config(Name),
    {FullFile,HConfig,DLHConfig}.
    
stop_handler(Name) ->
    ct:pal("Stopping handler ~p!", [Name]),
    logger:remove_handler(Name).

send_burst(NorT, Type, {chars,Sz}, Class) ->
    Text = [34 + rand:uniform(126-34) || _ <- lists:seq(1,Sz)],
    case NorT of
        {n,N} ->
            %% process_flag(priority, high),
            send_n_burst(N, Type, Text, Class),
            %% process_flag(priority, normal),
            N;
        {t,T} ->
            ct:pal("Sending messages sequentially for ~w ms", [T]),
            T0 = erlang:monotonic_time(millisecond),
            send_t_burst(T0, T, Text, Class, 0)
    end.

send_n_burst(0, _, _Text, _Class) ->
    ok;
send_n_burst(N, seq, Text, Class) ->
    ok = logger:Class(Text, ?domain),
    send_n_burst(N-1, seq, Text, Class);
send_n_burst(N, {spawn,Ps,TO}, Text, Class) ->
    ct:pal("~w processes each sending ~w messages", [Ps,N]),
    MRefs = [begin if TO == 0 -> ok; true -> timer:sleep(TO) end,
                   monitor(process,spawn_link(per_proc_fun(N,Text,Class,X)))
             end || X <- lists:seq(1,Ps)],
    lists:foreach(fun(MRef) ->
                          receive
                              {'DOWN', MRef, _, _, _} ->
                                  ok
                          end
                  end, MRefs),
    ct:pal("Message burst sent", []),
    ok.

send_t_burst(T0, T, Text, Class, N) ->
    T1 = erlang:monotonic_time(millisecond),
    if (T1-T0) > T ->
            N;
       true ->
            ok = logger:Class(Text, ?domain),
            send_t_burst(T0, T, Text, Class, N+1)
    end.

per_proc_fun(N,Text,Class,X) when X rem 2 == 0 ->
    fun() ->
            process_flag(priority,high),
            send_n_burst(N, seq, Text, Class)
    end;
per_proc_fun(N,Text,Class,_) ->
    fun() ->
            send_n_burst(N, seq, Text, Class)
    end.

%%%-----------------------------------------------------------------
%%% Formatter callback
%%% Using this to send the formatted string back to the test case
%%% process - so it can check for logged events.
format(_,bad_return) ->
    bad_return;
format(_,crash) ->
    erlang:error(formatter_crashed);
format(#{msg:={report,R},meta:=#{report_cb:=Fun}}=Log,Config) ->
    format(Log#{msg=>Fun(R)},Config);
format(#{msg:={string,String0}},no_nl) ->
    String = unicode:characters_to_list(String0),
    String;
format(#{msg:={string,String0}},nl) ->
    String = unicode:characters_to_list(String0),
    String++"\n";
format(#{msg:={string,String0}},op) ->
    String = unicode:characters_to_list(String0),
    String++"\n";
format(#{msg:={report,#{label:={supervisor,progress}}}},op) ->
    "";
format(#{msg:={report,#{label:={gen_server,terminate}}}},op) ->
    "";
format(#{msg:={report,#{label:={proc_lib,crash}}}},op) ->
    "";
format(#{msg:={F,A}},OpOrPid) when is_list(F), is_list(A) ->
    String = lists:flatten(io_lib:format(F,A)),
    if is_pid(OpOrPid) -> OpOrPid ! {log,String};
       true -> ok
    end,
    String++"\n";
format(#{msg:={string,String0}},Pid) ->
    String = unicode:characters_to_list(String0),
    Pid ! {log,String},
    String++"\n";
format(Msg,Tag) ->
    Error = {unexpected_format,Msg,Tag},
    erlang:display(Error),
    exit(Error).

start_and_add(Name, Config, LogOpts) ->
    HConfig = maps:get(config, Config, #{}),
    HConfig1 = maps:merge(HConfig, LogOpts),
    Config1 = Config#{config=>HConfig1},
    ct:pal("Adding handler ~w with: ~p", [Name,Config1]),
    ok = logger:add_handler(Name, logger_disk_log_h, Config1),
    Pid = whereis(h_proc_name(Name)),
    true = is_pid(Pid),
    Name = proplists:get_value(name, disk_log:info(Name)),
    ok.
 
remove_and_stop(Handler) ->
    ok = logger:remove_handler(Handler),
    timer:sleep(500),
    undefined = whereis(h_proc_name(Handler)),
    ok.

try_read_file(FileName, Expected, Time) ->
    try_read_file(FileName, Expected, Time, undefined).

try_read_file(FileName, Expected, Time, _) when Time > 0 ->
    case file:read_file(FileName) of
        Expected ->
            ok;
        Error = {error,_Reason} ->
            erlang:error(Error);
        SomethingElse ->
            ct:pal("try_read_file read unexpected: ~p~n", [SomethingElse]),
            timer:sleep(500),
            try_read_file(FileName, Expected, Time-500, SomethingElse)
    end;

try_read_file(_, _, _, Incorrect) ->
    ct:pal("try_read_file got incorrect pattern: ~p~n", [Incorrect]),
    erlang:error({error,not_matching_pattern,Incorrect}).

try_match_file(FileName, Pattern, Time) ->
    try_match_file(FileName, Pattern, Time, <<>>).

try_match_file(FileName, Pattern, Time, _) when Time > 0 ->
    case file:read_file(FileName) of
        {ok, Bin} ->
            case re:run(Bin,Pattern,[{capture,none}]) of
                match ->
                    unicode:characters_to_list(Bin);
                _ ->
                    timer:sleep(100),
                    try_match_file(FileName, Pattern, Time-100, Bin)
            end;
        Error ->
            erlang:error(Error)
    end;
try_match_file(_,Pattern,_,Incorrect) ->
    ct:pal("try_match_file did not match pattern: ~p~nGot: ~p~n",
           [Pattern,Incorrect]),
    erlang:error({error,not_matching_pattern,Pattern,Incorrect}).

count_lines(File) ->
    wait_until_written(File),
    count_lines1(File).

wait_until_written(File) ->
    wait_until_written(File, -1).

wait_until_written(File, Sz) ->
    timer:sleep(2000),
    case file:read_file_info(File) of
        {ok,#file_info{size = Sz}} ->
            timer:sleep(1000),
            case file:read_file_info(File) of
                {ok,#file_info{size = Sz}} ->
                    ok;
                {ok,#file_info{size = Sz1}} ->
                    wait_until_written(File, Sz1)
            end;
        {ok,#file_info{size = Sz1}} ->
            wait_until_written(File, Sz1)
    end.
    
count_lines1(File) ->
    {_,Dev} = file:open(File, [read]),
    Lines = count_lines2(Dev, 0),
    file:close(Dev),
    Lines.

count_lines2(Dev, LC) ->
    case file:read_line(Dev) of
        {ok,"Handler logger_disk_log_h_SUITE " ++_} ->
            %% Not counting handler info
            count_lines2(Dev,LC);
        {ok,_} ->
            count_lines2(Dev,LC+1);
        eof -> LC
    end.

repeat_until_ok(Fun, N) ->
    repeat_until_ok(Fun, 0, N, undefined).

repeat_until_ok(_Fun, Stop, Stop, Reason) ->
    {fails,Reason};

repeat_until_ok(Fun, C, Stop, FirstReason) ->
    if C > 0 -> timer:sleep(5000);
       true -> ok
    end,
    try Fun() of
        Result ->
            {ok,{C,Result}}
    catch
        _:Reason:Stack ->
            ct:pal("Test fails: ~p (~p)~n", [Reason,hd(Stack)]),
            if FirstReason == undefined ->
                    repeat_until_ok(Fun, C+1, Stop, {Reason,Stack});
               true ->
                    repeat_until_ok(Fun, C+1, Stop, FirstReason)
            end
    end.

start_tracer(Trace,Expected) ->
    Pid = self(),
    dbg:tracer(process,{fun tracer/2,{Pid,Expected}}),
    dbg:p(h_proc_name(),[c]),
    tpl(Trace),
    ok.

tpl([{M,F,A}|Trace]) ->
    tpl([{{M,F,A},c}|Trace]);
tpl([{{M,F,A},MS}|Trace]) ->
    {ok,Match} = dbg:tpl(M,F,A,MS),
    case lists:keyfind(matched,1,Match) of
        {_,_,1} ->
            ok;
        _ ->
            dbg:stop_clear(),
            throw({skip,"Can't trace "++atom_to_list(M)++":"++
                       atom_to_list(F)++"/"++integer_to_list(A)})
    end,
    tpl(Trace);
tpl([]) ->
    ok.

tracer({trace,_,call,{logger_h_common,handle_cast,[Op|_]},Caller},
       {Pid,[{Mod,Func,Op}|Expected]}) ->
    maybe_tracer_done(Pid,Expected,{Mod,Func,Op},Caller);
tracer({trace,_,call,{Mod=logger_disk_log_h,Func=disk_log_write,[_,_,Data]},Caller}, {Pid,[{Mod,Func,Data}|Expected]}) ->
    maybe_tracer_done(Pid,Expected,{Mod,Func,Data},Caller);
tracer({trace,_,call,{Mod,Func,_},Caller}, {Pid,[{Mod,Func}|Expected]}) ->
    maybe_tracer_done(Pid,Expected,{Mod,Func},Caller);
tracer({trace,_,call,Call,Caller}, {Pid,Expected}) ->
    ct:log("Tracer got unexpected: ~p~nCaller: ~p~nExpected: ~p~n",[Call,Caller,Expected]),
    Pid ! {tracer_got_unexpected,Call,Expected},
    {Pid,Expected}.

maybe_tracer_done(Pid,[],Got,Caller) ->
    ct:log("Tracer got: ~p~nCaller: ~p~n",[Got,Caller]),
    Pid ! tracer_done;
maybe_tracer_done(Pid,Expected,Got,Caller) ->
    ct:log("Tracer got: ~p~nCaller: ~p~n",[Got,Caller]),
    {Pid,Expected}.

check_tracer(T) ->
    receive
        tracer_done ->
            dbg:stop_clear(),
            ok;
        {tracer_got_unexpected,Got,Expected} ->
            dbg:stop_clear(),
            ct:fail({tracer_got_unexpected,Got,Expected})
    after T ->
            dbg:stop_clear(),
            ct:fail({timeout,tracer})
    end.

escape([$+|Rest]) ->
    [$\\,$+|escape(Rest)];
escape([H|T]) ->
    [H|escape(T)];
escape([]) ->
    [].

h_proc_name() ->
    h_proc_name(?MODULE).
h_proc_name(Name) ->
    list_to_atom(lists:concat([logger_disk_log_h,"_",Name])).

wait_for_process_up(T) ->
    wait_for_process_up(?MODULE, h_proc_name(), T).

wait_for_process_up(Name, RegName, T) ->
    N = (T div 500) + 1,
    wait_for_process_up1(Name, RegName, N).

wait_for_process_up1(_Name, _RegName, 0) ->
    error;
wait_for_process_up1(Name, RegName, N) ->
    timer:sleep(500),
    case whereis(RegName) of
        Pid when is_pid(Pid) ->
            case logger:get_handler_config(Name) of
                {ok,_} ->
                    %% ct:pal("Process ~p up (~p tries left)",[Name,N]),
                    {ok,Pid};
                _ ->
                    wait_for_process_up1(Name, RegName, N-1)
            end;
        undefined ->
            %% ct:pal("Waiting for process ~p (~p tries left)",[Name,N]),
            wait_for_process_up1(Name, RegName, N-1)
    end.

file_delete(Log) ->
   file:delete(Log).
