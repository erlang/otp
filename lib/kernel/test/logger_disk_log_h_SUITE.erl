-module(logger_disk_log_h_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/src/logger_internal.hrl").
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

-define(SYNC_REP_INT, if is_atom(?FILESYNC_REPEAT_INTERVAL) -> 5500;
                         true -> ?FILESYNC_REPEAT_INTERVAL + 500
                      end).

suite() ->
    [{timetrap,{seconds,30}}].

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
    if ?TEST_HOOKS_TAB == undefined ->
            {skip,"Define the TEST_HOOKS macro to run this test"};
       true ->
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
     errors,
     formatter_fail,
     config_fail,
     bad_input,
     info_and_reset,
     reconfig,
     disk_log_sync,
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
    true = is_pid(whereis(?MODULE)),
    ok = logger:remove_handler(?MODULE),
    timer:sleep(500),
    undefined = whereis(?MODULE).
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
    logger:info("hello", ?domain),
    logger_disk_log_h:disk_log_sync(Name1),
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
    logger:info("dummy", ?domain),
    logger_disk_log_h:disk_log_sync(Name2),
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
    logger:info("one", ?domain),
    logger_disk_log_h:disk_log_sync(HName),
    ct:pal("Checking contents of ~p", [?log_no(LogFile1,1)]),
    try_read_file(?log_no(LogFile1,1), {ok,<<"one\n">>}, 5000),
    logger:info("two", ?domain),
    ok = remove_and_stop(HName),
    try_read_file(?log_no(LogFile1,1), {ok,<<"one\ntwo\n">>}, 5000),

    logger:info("two and a half", ?domain),

    ok = start_and_add(HName, #{filter_default=>stop,
                                filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                                formatter=>{?MODULE,self()}},
                       #{file=>LogFile1}),
    logger:info("three", ?domain),
    logger_disk_log_h:disk_log_sync(HName),
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
    logger:info("123", ?domain),
    logger_disk_log_h:disk_log_sync(WName),
    timer:sleep(500),
    1 = Get(current_file, disk_log:info(WName)),

    logger:info("45", ?domain),
    logger_disk_log_h:disk_log_sync(WName),
    timer:sleep(500),
    1 = Get(current_file, disk_log:info(WName)),

    logger:info("6", ?domain),
    logger_disk_log_h:disk_log_sync(WName),
    timer:sleep(500),
    2 = Get(current_file, disk_log:info(WName)),

    logger:info("7890", ?domain),
    logger_disk_log_h:disk_log_sync(WName),
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
    logger:info("12345", ?domain),
    logger_disk_log_h:disk_log_sync(HName1),
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
    HConfig = #{disk_log_opts => #{file=>LogFile},
                filter_default=>log},
    ct:pal("Log: ~p", [LogFile]),
    ok = logger:add_handler(?MODULE, logger_disk_log_h, HConfig),
    ok = logger:set_handler_config(?MODULE,formatter,
                                   {?DEFAULT_FORMATTER,?DEFAULT_FORMAT_CONFIG}),
    LogName = lists:concat([LogFile, ".1"]),
    logger:info("dummy"),
    wait_until_written(LogName),
    {ok,Bin} = file:read_file(LogName),
    match = re:run(Bin, "=INFO REPORT====.*\ndummy", [{capture,none}]),
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
    MsgFormatter = fun(Term) -> {io_lib:format("Term:~p",[Term]),[]} end,
    logger:info([{x,y}], #{report_cb => MsgFormatter}),
    logger:info([{x,y}], #{}),
    ct:pal("Checking contents of ~p", [?log_no(LogFile,1)]),   
    try_read_file(?log_no(LogFile,1), {ok,<<"Term:[{x,y}]\n    x: y\n">>}, 5000).

logging(cleanup, _Config) ->
    Name = list_to_atom(lists:concat([?FUNCTION_NAME,"_1"])),
    remove_and_stop(Name).

errors(Config) ->
    PrivDir = ?config(priv_dir,Config),
    Name1 = list_to_atom(lists:concat([?FUNCTION_NAME,"_1"])),
    LogFile1 = filename:join(PrivDir,Name1),
    HConfig = #{disk_log_opts=>#{file=>LogFile1},
                filter_default=>log,
                formatter=>{?MODULE,self()}},
    ok = logger:add_handler(Name1, logger_disk_log_h, HConfig),
    {error,{already_exist,Name1}} =
        logger:add_handler(Name1, logger_disk_log_h, #{}),
 
    %%! TODO:
    %%! Check how bad log_opts are handled!

    {error,{illegal_config_change,_,_}} =
        logger:set_handler_config(Name1,
                                  disk_log_opts,
                                  #{file=>LogFile1,
                                    type=>halt}),
    {error,{illegal_config_change,_,_}} =
        logger:set_handler_config(Name1,id,new),

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
    HConfig = #{disk_log_opts => #{file=>LogFile},
                filter_default=>stop,
                filters=>?DEFAULT_HANDLER_FILTERS([?MODULE])},
    %% no formatter!
    logger:add_handler(Name, logger_disk_log_h, HConfig),
    Pid = whereis(Name),
    true = is_pid(Pid),
    {ok,#{handlers:=H}} = logger:get_logger_config(),
    true = lists:member(Name,H),

    %% Formatter is added automatically
    {ok,{_,#{formatter:={logger_formatter,_}}}} =
        logger:get_handler_config(Name),
    logger:info(M1=?msg,?domain),
    Got1 = try_match_file(?log_no(LogFile,1),"=INFO REPORT====.*\n"++M1,5000),

    ok = logger:set_handler_config(Name,formatter,{nonexistingmodule,#{}}),
    logger:info(M2=?msg,?domain),
    Got2 = try_match_file(?log_no(LogFile,1),
                          Got1++"=INFO REPORT====.*\nFORMATTER CRASH: .*"++M2,
                          5000),

    ok = logger:set_handler_config(Name,formatter,{?MODULE,crash}),
    logger:info(M3=?msg,?domain),
    Got3 = try_match_file(?log_no(LogFile,1),
                          Got2++"=INFO REPORT====.*\nFORMATTER CRASH: .*"++M3,
                          5000),

    ok = logger:set_handler_config(Name,formatter,{?MODULE,bad_return}),
    logger:info(?msg,?domain),
    try_match_file(?log_no(LogFile,1),
                   Got3++"FORMATTER ERROR: bad_return_value",
                   5000),

    %% Check that handler is still alive and was never dead
    Pid = whereis(Name),
    {ok,#{handlers:=H}} = logger:get_logger_config(),
    ok.

formatter_fail(cleanup,_Config) ->
    _ = logger:remove_handler(?FUNCTION_NAME),
    ok.

config_fail(_Config) ->
    {error,{handler_not_added,{invalid_config,logger_disk_log_h,{bad,bad}}}} =
        logger:add_handler(?MODULE,logger_disk_log_h,
                           #{logger_disk_log_h => #{bad => bad},
                             filter_default=>log,
                             formatter=>{?MODULE,self()}}),

    {error,{handler_not_added,{invalid_levels,{_,1,_}}}} =
        logger:add_handler(?MODULE,logger_disk_log_h,
                           #{logger_disk_log_h => #{drop_new_reqs_qlen=>1}}),
    {error,{handler_not_added,{invalid_levels,{43,42,_}}}} =
        logger:add_handler(?MODULE,logger_disk_log_h,
                           #{logger_disk_log_h => #{toggle_sync_qlen=>43,
                                                    drop_new_reqs_qlen=>42}}),
    {error,{handler_not_added,{invalid_levels,{_,43,42}}}} =
        logger:add_handler(?MODULE,logger_disk_log_h,
                           #{logger_disk_log_h => #{drop_new_reqs_qlen=>43,
                                                    flush_reqs_qlen=>42}}),

    ok = logger:add_handler(?MODULE,logger_disk_log_h,
                            #{filter_default=>log,
                              formatter=>{?MODULE,self()}}),
    %% can't change the disk log options for a log already in use
    {error,{illegal_config_change,_,_}} =
        logger:set_handler_config(?MODULE,disk_log_opts,
                                  #{max_no_files=>2}),
    %% can't change name of an existing handler
    {error,{illegal_config_change,_,_}} =
        logger:set_handler_config(?MODULE,id,bad),
    %% incorrect values of OP params  
    {error,{invalid_levels,_}} =
        logger:set_handler_config(?MODULE,logger_disk_log_h,
                                  #{toggle_sync_qlen=>100,
                                    flush_reqs_qlen=>99}),
    %% invalid name of config parameter
    {error,{invalid_config,logger_disk_log_h,{filesync_rep_int,2000}}} =
        logger:set_handler_config(?MODULE, logger_disk_log_h,
                                  #{filesync_rep_int => 2000}),
    ok.
config_fail(cleanup,_Config) ->
    logger:remove_handler(?MODULE).

bad_input(_Config) ->
    {error,{badarg,{disk_log_sync,["BadType"]}}} =
        logger_disk_log_h:disk_log_sync("BadType"),
    {error,{badarg,{info,["BadType"]}}} = logger_disk_log_h:info("BadType"),
    {error,{badarg,{reset,["BadType"]}}} = logger_disk_log_h:reset("BadType").

info_and_reset(_Config) ->
    ok = logger:add_handler(?MODULE,logger_disk_log_h,
                            #{filter_default=>log,
                              formatter=>{?MODULE,self()}}),
    #{id := ?MODULE} = logger_disk_log_h:info(?MODULE),
    ok = logger_disk_log_h:reset(?MODULE).
info_and_reset(cleanup,_Config) ->
    logger:remove_handler(?MODULE).

reconfig(Config) ->
    Dir = ?config(priv_dir,Config),
    ok = logger:add_handler(?MODULE,
                            logger_disk_log_h,
                            #{filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()}}),
    #{id := ?MODULE,
      toggle_sync_qlen := ?TOGGLE_SYNC_QLEN,
      drop_new_reqs_qlen := ?DROP_NEW_REQS_QLEN,
      flush_reqs_qlen := ?FLUSH_REQS_QLEN,
      enable_burst_limit := ?ENABLE_BURST_LIMIT,
      burst_limit_size := ?BURST_LIMIT_SIZE,
      burst_window_time := ?BURST_WINDOW_TIME,
      enable_kill_overloaded := ?ENABLE_KILL_OVERLOADED,
      handler_overloaded_qlen := ?HANDLER_OVERLOADED_QLEN,
      handler_overloaded_mem := ?HANDLER_OVERLOADED_MEM,
      handler_restart_after := ?HANDLER_RESTART_AFTER,
      filesync_repeat_interval := ?FILESYNC_REPEAT_INTERVAL,
      log_opts := #{type := ?DISK_LOG_TYPE,
                    max_no_files := ?DISK_LOG_MAX_NO_FILES,
                    max_no_bytes := ?DISK_LOG_MAX_NO_BYTES,
                    file := _DiskLogFile}} =
        logger_disk_log_h:info(?MODULE),

    ok = logger:set_handler_config(?MODULE, logger_disk_log_h,
                                   #{toggle_sync_qlen => 1,
                                     drop_new_reqs_qlen => 2,
                                     flush_reqs_qlen => 3,
                                     enable_burst_limit => false,
                                     burst_limit_size => 10,
                                     burst_window_time => 10,
                                     enable_kill_overloaded => true,
                                     handler_overloaded_qlen => 100000,
                                     handler_overloaded_mem => 10000000,
                                     handler_restart_after => never,
                                     filesync_repeat_interval => no_repeat}),
    #{id := ?MODULE,
      toggle_sync_qlen := 1,
      drop_new_reqs_qlen := 2,
      flush_reqs_qlen := 3,
      enable_burst_limit := false,
      burst_limit_size := 10,
      burst_window_time := 10,
      enable_kill_overloaded := true,
      handler_overloaded_qlen := 100000,
      handler_overloaded_mem := 10000000,
      handler_restart_after := never,
      filesync_repeat_interval := no_repeat} =
        logger_disk_log_h:info(?MODULE),

    ok = logger:remove_handler(?MODULE),

    File = filename:join(Dir, "logfile"),
    ok = logger:add_handler(?MODULE,
                            logger_disk_log_h,
                            #{filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()},
                              disk_log_opts=>
                                  #{type => halt,
                                    max_no_files => 1,
                                    max_no_bytes => 1024,
                                    file => File}}),
    #{log_opts := #{type := halt,
                    max_no_files := 1,
                    max_no_bytes := 1024,
                    file := File}} =
        logger_disk_log_h:info(?MODULE),
    ok.

reconfig(cleanup, _Config) ->
    logger:remove_handler(?MODULE).

disk_log_sync(Config) ->
    Dir = ?config(priv_dir,Config),
    File = filename:join(Dir, ?FUNCTION_NAME),
    Log = lists:concat([File,".1"]),
    ok = logger:add_handler(?MODULE,
                            logger_disk_log_h,
                            #{disk_log_opts => #{file => File},
                              filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,nl}}),

    start_tracer([{?MODULE,format,2},
                  {disk_log,blog,2},
                  {disk_log,sync,1}],
                 [{formatter,"first"},
                  {disk_log,blog},
                  {disk_log,sync}]),

    logger:info("first", ?domain),
    %% wait for automatic disk_log_sync
    check_tracer(?FILESYNC_REPEAT_INTERVAL*2),
    
    start_tracer([{?MODULE,format,2},
                  {disk_log,blog,2},
                  {disk_log,sync,1}],
                 [{formatter,"second"},
                  {formatter,"third"},
                  {disk_log,blog},
                  {disk_log,blog},
                  {disk_log,sync}]),
    %% two log requests in fast succession will make the handler skip
    %% an automatic disk log sync
    logger:info("second", ?domain),
    logger:info("third", ?domain),
    %% do explicit disk_log_sync
    logger_disk_log_h:disk_log_sync(?MODULE),
    check_tracer(100),

    %% check that if there's no repeated disk_log_sync active,
    %% a disk_log_sync is still performed when handler goes idle
    logger:set_handler_config(?MODULE, logger_disk_log_h,
                              #{filesync_repeat_interval => no_repeat}),
    no_repeat = maps:get(filesync_repeat_interval,
                         logger_disk_log_h:info(?MODULE)),

    start_tracer([{?MODULE,format,2},
                  {disk_log,blog,2},
                  {disk_log,sync,1}],
                 [{formatter,"fourth"},
                  {disk_log,blog},
                  {formatter,"fifth"},
                  {disk_log,blog},
                  {disk_log,sync}]),

    logger:info("fourth", ?domain),
    timer:sleep(?IDLE_DETECT_TIME_MSEC*2),
    logger:info("fifth", ?domain),
    %% wait for automatic disk_log_sync
    check_tracer(?IDLE_DETECT_TIME_MSEC*2),

    try_read_file(Log, {ok,<<"first\nsecond\nthird\nfourth\nfifth\n">>}, 1000),
    
    %% switch repeated disk_log_sync on and verify that the looping works
    SyncInt = 1000,
    WaitT = 4500,
    OneSync = {logger_disk_log_h,handle_cast,repeated_disk_log_sync},
    %% receive 1 initial repeated_disk_log_sync, then 1 per sec
    start_tracer([{logger_disk_log_h,handle_cast,2}],
                 [OneSync || _ <- lists:seq(1, 1 + trunc(WaitT/SyncInt))]),

    logger:set_handler_config(?MODULE, logger_disk_log_h,
                              #{filesync_repeat_interval => SyncInt}),
    SyncInt = maps:get(filesync_repeat_interval,
                       logger_disk_log_h:info(?MODULE)),
    timer:sleep(WaitT),
    logger:set_handler_config(?MODULE, logger_disk_log_h,
                              #{filesync_repeat_interval => no_repeat}),    
    check_tracer(100),
    ok.
disk_log_sync(cleanup,_Config) ->
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
                              disk_log_opts=>
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
    {ok,_} = dbg:p(whereis(?MODULE), [c]),
    {ok,_} = dbg:tp(logger_disk_log_h, handle_info, 2, []),

    Text = [34 + rand:uniform(126-34) || _ <- lists:seq(1,MaxBytes)],
    ct:pal("String = ~p (~w)", [Text, erts_debug:size(Text)]),
    %% fill first file
    lists:foreach(fun(N) ->
                          Log = lists:concat([File,".",N]),
                          logger:info(Text, ?domain),
                          wait_until_written(Log),
                          ct:pal("N = ~w",
                                 [N = Get(current_file,
                                          disk_log:info(?MODULE))])
                  end, lists:seq(1,MaxFiles)),

    %% wait for trace messages
    timer:sleep(1000),
    dbg:stop_clear(),
    Received = lists:flatmap(fun({trace,_M,handle_info,
                                  [{disk_log,_Node,_Name,What},_]}) ->
                                     [{trace,What}];
                                ({log,_}) ->
                                     []
                             end, test_server:messages_get()),
    ct:pal("Trace =~n~p", [Received]),
    Received = [{trace,{wrap,0}} || _ <- lists:seq(1,MaxFiles-1)],
    ok.

disk_log_wrap(cleanup,_Config) ->
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
                              disk_log_opts=>
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
    {ok,_} = dbg:p(whereis(?MODULE), [c]),
    {ok,_} = dbg:tp(logger_disk_log_h, handle_info, 2, []),

    NoOfChars = 5,
    Text = [34 + rand:uniform(126-34) || _ <- lists:seq(1,NoOfChars)],
    [logger:info(Text, ?domain) || _ <- lists:seq(1,trunc(MaxBytes/NoOfChars)+1)],

    %% wait for trace messages
    timer:sleep(2000),
    dbg:stop_clear(),
    Received = lists:flatmap(fun({trace,_M,handle_info,
                                  [{disk_log,_Node,_Name,What},_]}) ->
                                     [{trace,What}];
                                ({log,_}) ->
                                     []
                             end, test_server:messages_get()),
    ct:pal("Trace =~n~p", [Received]),
    [{trace,full},
     {trace,{error_status,{error,{full,_}}}}] = Received,
    ok.
disk_log_full(cleanup, _Config) ->
    logger:remove_handler(?MODULE).    

disk_log_events(Config) ->
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
    {ok,_} = dbg:p(whereis(?MODULE), [c]),
    {ok,_} = dbg:tp(logger_disk_log_h, handle_info, 2, []),
    
    [whereis(?MODULE) ! E || E <- Events],
    %% wait for trace messages
    timer:sleep(2000),
    dbg:stop_clear(),
    Received = lists:map(fun({trace,_M,handle_info,
                              [Got,_]}) -> Got
                         end, test_server:messages_get()),
    ct:pal("Trace =~n~p", [Received]),
    NoOfEvents = length(Events),
    NoOfEvents = length(Received),
    lists:foreach(fun(Event) ->
                          true = lists:member(Event, Received)
                  end, Received),
    ok.
disk_log_events(cleanup, _Config) ->
    logger:remove_handler(?MODULE).    

write_failure(Config) ->
    Dir = ?config(priv_dir, Config),
    File = filename:join(Dir, ?FUNCTION_NAME),
    Log = lists:concat([File,".1"]),
    ct:pal("Log = ~p", [Log]),

    Node = start_h_on_new_node(Config, ?FUNCTION_NAME, File),
    false = (undefined == rpc:call(Node, ets, whereis, [?TEST_HOOKS_TAB])),
    rpc:call(Node, ets, insert, [?TEST_HOOKS_TAB,{tester,self()}]),
    rpc:call(Node, ?MODULE, set_internal_log, [?MODULE,internal_log]),
    rpc:call(Node, ?MODULE, set_result, [disk_log_blog,ok]),
    HState = rpc:call(Node, logger_disk_log_h, info, [?STANDARD_HANDLER]),
    ct:pal("LogOpts = ~p", [LogOpts = maps:get(log_opts, HState)]),

    ok = log_on_remote_node(Node, "Logged1"),
    rpc:call(Node, logger_disk_log_h, disk_log_sync, [?STANDARD_HANDLER]),
    ?check_no_log,
    try_read_file(Log, {ok,<<"Logged1\n">>}, ?SYNC_REP_INT),

    rpc:call(Node, ?MODULE, set_result, [disk_log_blog,{error,no_such_log}]),
    ok = log_on_remote_node(Node, "Cause simple error printout"),

    ?check({error,{?STANDARD_HANDLER,log,LogOpts,{error,no_such_log}}}),
    
    ok = log_on_remote_node(Node, "No second error printout"),
    ?check_no_log,

    rpc:call(Node, ?MODULE, set_result, [disk_log_blog,
                                         {error,{full,?STANDARD_HANDLER}}]),
    ok = log_on_remote_node(Node, "Cause simple error printout"),
    ?check({error,{?STANDARD_HANDLER,log,LogOpts,
                   {error,{full,?STANDARD_HANDLER}}}}),

    rpc:call(Node, ?MODULE, set_result, [disk_log_blog,ok]),
    ok = log_on_remote_node(Node, "Logged2"),
    rpc:call(Node, logger_disk_log_h, disk_log_sync, [?STANDARD_HANDLER]),
    ?check_no_log,
    try_read_file(Log, {ok,<<"Logged1\nLogged2\n">>}, ?SYNC_REP_INT),
    ok.
write_failure(cleanup, _Config) ->
    Nodes = nodes(),
    [test_server:stop_node(Node) || Node <- Nodes].


sync_failure(Config) ->
    Dir = ?config(priv_dir, Config),
    FileName = lists:concat([?MODULE,"_",?FUNCTION_NAME]),
    File = filename:join(Dir, FileName),
    Log = lists:concat([File,".1"]),

    Node = start_h_on_new_node(Config, ?FUNCTION_NAME, File),
    false = (undefined == rpc:call(Node, ets, whereis, [?TEST_HOOKS_TAB])),
    rpc:call(Node, ets, insert, [?TEST_HOOKS_TAB,{tester,self()}]),
    rpc:call(Node, ?MODULE, set_internal_log, [?MODULE,internal_log]),
    rpc:call(Node, ?MODULE, set_result, [disk_log_sync,ok]),
    HState = rpc:call(Node, logger_disk_log_h, info, [?STANDARD_HANDLER]),
    LogOpts = maps:get(log_opts, HState),
    
    SyncInt = 500,
    ok = rpc:call(Node, logger, set_handler_config,
                  [?STANDARD_HANDLER, logger_disk_log_h,
                   #{filesync_repeat_interval => SyncInt}]),
    Info = rpc:call(Node, logger_disk_log_h, info, [?STANDARD_HANDLER]),
    SyncInt = maps:get(filesync_repeat_interval, Info),
    
    ok = log_on_remote_node(Node, "Logged1"),
    ?check_no_log,

    rpc:call(Node, ?MODULE, set_result, [disk_log_sync,{error,no_such_log}]),
    ok = log_on_remote_node(Node, "Cause simple error printout"),
    
    ?check({error,{?STANDARD_HANDLER,sync,LogOpts,{error,no_such_log}}}),

    ok = log_on_remote_node(Node, "No second error printout"),
    ?check_no_log,

    rpc:call(Node, ?MODULE, set_result,
             [disk_log_sync,{error,{blocked_log,?STANDARD_HANDLER}}]),
    ok = log_on_remote_node(Node, "Cause simple error printout"),
    ?check({error,{?STANDARD_HANDLER,sync,LogOpts,
                   {error,{blocked_log,?STANDARD_HANDLER}}}}),

    rpc:call(Node, ?MODULE, set_result, [disk_log_sync,ok]),
    ok = log_on_remote_node(Node, "Logged2"),
    ?check_no_log,
    ok.
sync_failure(cleanup, _Config) ->
    Nodes = nodes(),
    [test_server:stop_node(Node) || Node <- Nodes].

start_h_on_new_node(_Config, Func, File) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Dest =
        case os:type() of
            {win32,_} ->
                lists:concat([" {disk_log,\\\"",File,"\\\"}"]);
            _ ->
                lists:concat([" \'{disk_log,\"",File,"\"}\'"])
        end,
    Args = lists:concat([" -kernel ",logger_dest,Dest," -pa ",Pa]),
    NodeName = lists:concat([?MODULE,"_",Func]),
    ct:pal("Starting ~s with ~tp", [NodeName,Args]),
    {ok,Node} = test_server:start_node(NodeName, peer, [{args, Args}]),
    Pid = rpc:call(Node,erlang,whereis,[?STANDARD_HANDLER]),
    true = is_pid(Pid),
    ok = rpc:call(Node,logger,set_handler_config,[?STANDARD_HANDLER,formatter,
                                                  {?MODULE,nl}]),
    Node.

log_on_remote_node(Node,Msg) ->
    _ = spawn_link(Node,
                   fun() -> erlang:group_leader(whereis(user),self()),
                            logger:info(Msg)
                   end),
    ok.

%% functions for test hook macros to be called by rpc
set_internal_log(Mod, Func) ->
    ?set_internal_log({Mod,Func}).
set_result(Op, Result) ->
    ?set_result(Op, Result).
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
        HConfig#{logger_disk_log_h => DLHConfig#{toggle_sync_qlen => 2,
                                                 drop_new_reqs_qlen => NumOfReqs+1,
                                                 flush_reqs_qlen => 2*NumOfReqs,
                                                 enable_burst_limit => false}},
    ok = logger:set_handler_config(?MODULE, NewHConfig),
    send_burst({n,NumOfReqs}, seq, {chars,79}, info),
    Lines = count_lines(Log),
    ok = file:delete(Log),
    NumOfReqs = Lines,
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
                    HConfig#{logger_disk_log_h =>
                                 DLHConfig#{toggle_sync_qlen => 1,
                                            drop_new_reqs_qlen => 2,
                                            flush_reqs_qlen => Procs*NumOfReqs*Bursts,
                                            enable_burst_limit => false}},
                ok = logger:set_handler_config(?MODULE, NewHConfig),
                %% It sometimes happens that the handler either gets
                %% the requests in a slow enough pace so that dropping
                %% never occurs. Therefore, lets generate a number of
                %% bursts to increase the chance of message buildup.
                [send_burst({n,NumOfReqs}, {spawn,Procs,0}, {chars,79}, info) ||
                    _ <- lists:seq(1, Bursts)],
                Logged = count_lines(Log),
                ok= stop_handler(?MODULE),
                _ = file:delete(Log),
                ct:pal("Number of messages dropped = ~w (~w)",
                       [Procs*NumOfReqs*Bursts-Logged,Procs*NumOfReqs*Bursts]),
                true = (Logged < (Procs*NumOfReqs*Bursts)),
                true = (Logged > 0),
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
    [{timetrap,{seconds,180}}].
op_switch_to_flush(Config) ->
    Test =
        fun() ->
                {Log,HConfig,DLHConfig} =
                    start_handler(?MODULE, ?FUNCTION_NAME, Config),
                
                %% NOTE: it's important that both async and sync
                %% requests have been queued when the flush happens
                %% (verify with coverage of flush_log_requests/2)
    
                NewHConfig =
                    HConfig#{logger_disk_log_h =>
                                 DLHConfig#{toggle_sync_qlen => 2,
                                            %% disable drop mode
                                            drop_new_reqs_qlen => 300,
                                            flush_reqs_qlen => 300,
                                            enable_burst_limit => false}},    
                ok = logger:set_handler_config(?MODULE, NewHConfig),
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
                [send_burst({n,NumOfReqs}, {spawn,Procs,0}, {chars,79}, info) ||
                    _ <- lists:seq(1,Bursts)],
                Logged = count_lines(Log),
                ok= stop_handler(?MODULE),
                _ = file:delete(Log),
                ct:pal("Number of messages flushed/dropped = ~w (~w)",
                       [NumOfReqs*Procs*Bursts-Logged,NumOfReqs*Procs*Bursts]),
                true = (Logged < (NumOfReqs*Procs*Bursts)),
                true = (Logged > 0),
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
        HConfig#{logger_disk_log_h => DLHConfig#{enable_burst_limit => false,
                                                 burst_limit_size => 10,
                                                 burst_window_time => 2000,
                                                 drop_new_reqs_qlen => 200,
                                                 flush_reqs_qlen => 300}},
    ok = logger:set_handler_config(?MODULE, NewHConfig),
    NumOfReqs = 100,
    send_burst({n,NumOfReqs}, seq, {chars,79}, info),
    Logged = count_lines(Log),
    ct:pal("Number of messages logged = ~w", [Logged]),
    ok = file:delete(Log),
    NumOfReqs = Logged.
limit_burst_disabled(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

limit_burst_enabled_one(Config) ->
    {Log,HConfig,DLHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    ReqLimit = 10,
    NewHConfig =
        HConfig#{logger_disk_log_h => DLHConfig#{enable_burst_limit => true,
                                                 burst_limit_size => ReqLimit,
                                                 burst_window_time => 2000,
                                                 drop_new_reqs_qlen => 200,
                                                 flush_reqs_qlen => 300}},
    ok = logger:set_handler_config(?MODULE, NewHConfig),
    NumOfReqs = 100,
    send_burst({n,NumOfReqs}, seq, {chars,79}, info),
    Logged = count_lines(Log),
    ct:pal("Number of messages logged = ~w", [Logged]),
    ok = file:delete(Log),
    ReqLimit = Logged.
limit_burst_enabled_one(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

limit_burst_enabled_period(Config) ->
    {Log,HConfig,DLHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    ReqLimit = 10,
    BurstTWin = 1000,
    NewHConfig =
        HConfig#{logger_disk_log_h => DLHConfig#{enable_burst_limit => true,
                                                 burst_limit_size => ReqLimit,
                                                 burst_window_time => BurstTWin,
                                                 drop_new_reqs_qlen => 20000,
                                                 flush_reqs_qlen => 20001}},
    ok = logger:set_handler_config(?MODULE, NewHConfig),
    
    Windows = 3,
    Sent = send_burst({t,BurstTWin*Windows}, seq, {chars,79}, info),
    Logged = count_lines(Log),
    ct:pal("Number of messages sent = ~w~nNumber of messages logged = ~w",
           [Sent,Logged]),
    ok = file:delete(Log),
    true = (Logged > (ReqLimit*Windows)) andalso
           (Logged < (ReqLimit*(Windows+2))).
limit_burst_enabled_period(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

kill_disabled(Config) ->
    {Log,HConfig,DLHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    NewHConfig =
        HConfig#{logger_disk_log_h=>DLHConfig#{enable_kill_overloaded=>false,
                                               handler_overloaded_qlen=>10,
                                               handler_overloaded_mem=>100}},
    ok = logger:set_handler_config(?MODULE, NewHConfig),
    NumOfReqs = 100,
    send_burst({n,NumOfReqs}, seq, {chars,79}, info),
    Logged = count_lines(Log),
    ct:pal("Number of messages logged = ~w", [Logged]),
    ok = file:delete(Log),
    true = is_pid(whereis(?MODULE)),
    ok.
kill_disabled(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

qlen_kill_new(Config) ->
    {Log,HConfig,DLHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    Pid0 = whereis(?MODULE),
    {_,Mem0} = process_info(Pid0, memory),
    RestartAfter = ?HANDLER_RESTART_AFTER,
    NewHConfig =
        HConfig#{logger_disk_log_h =>
                     DLHConfig#{enable_kill_overloaded=>true,
                                handler_overloaded_qlen=>10,
                                handler_overloaded_mem=>Mem0+50000,
                                handler_restart_after=>RestartAfter}},
    ok = logger:set_handler_config(?MODULE, NewHConfig),
    MRef = erlang:monitor(process, Pid0),
    NumOfReqs = 100,
    Procs = 2,
    send_burst({n,NumOfReqs}, {spawn,Procs,0}, {chars,79}, info),
    %% send_burst({n,NumOfReqs}, seq, {chars,79}, info),
    receive
        {'DOWN', MRef, _, _, Info} ->
           case Info of
                {shutdown,{overloaded,?MODULE,QLen,Mem}} ->
                    ct:pal("Terminated with qlen = ~w, mem = ~w", [QLen,Mem]);
                killed ->
                    ct:pal("Slow shutdown, handler process was killed!", [])
            end,
            timer:sleep(RestartAfter + 2000),
            true = is_pid(whereis(?MODULE)),
            ok
    after
        5000 ->
            Info = logger_disk_log_h:info(?MODULE),
            ct:pal("Handler state = ~p", [Info]),
            ct:fail("Handler not dead! It should not have survived this!")
    end.
qlen_kill_new(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

mem_kill_new(Config) ->
    {Log,HConfig,DLHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    Pid0 = whereis(?MODULE),
    {_,Mem0} = process_info(Pid0, memory),
    RestartAfter = ?HANDLER_RESTART_AFTER,
    NewHConfig =
        HConfig#{logger_disk_log_h =>
                     DLHConfig#{enable_kill_overloaded=>true,
                                handler_overloaded_qlen=>50000,
                                handler_overloaded_mem=>Mem0+500,
                                handler_restart_after=>RestartAfter}},
    ok = logger:set_handler_config(?MODULE, NewHConfig),
    MRef = erlang:monitor(process, Pid0),
    NumOfReqs = 100,
    Procs = 2,
    send_burst({n,NumOfReqs}, {spawn,Procs,0}, {chars,79}, info),
    %% send_burst({n,NumOfReqs}, seq, {chars,79}, info),
    receive
        {'DOWN', MRef, _, _, Info} ->
            case Info of
                {shutdown,{overloaded,?MODULE,QLen,Mem}} ->
                    ct:pal("Terminated with qlen = ~w, mem = ~w", [QLen,Mem]);
                killed ->
                    ct:pal("Slow shutdown, handler process was killed!", [])
            end,
            timer:sleep(RestartAfter + 2000),
            true = is_pid(whereis(?MODULE)),
            ok
    after
        5000 ->
            Info = logger_disk_log_h:info(?MODULE),
            ct:pal("Handler state = ~p", [Info]),
            ct:fail("Handler not dead! It should not have survived this!")
    end.
mem_kill_new(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

restart_after(Config) ->
    {Log,HConfig,DLHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    NewHConfig1 =
        HConfig#{logger_disk_log_h=>DLHConfig#{enable_kill_overloaded=>true,
                                               handler_overloaded_qlen=>10,
                                               handler_restart_after=>never}},
    ok = logger:set_handler_config(?MODULE, NewHConfig1),
    MRef1 = erlang:monitor(process, whereis(?MODULE)),
    %% kill handler
    send_burst({n,100}, {spawn,2,0}, {chars,79}, info),
    receive
        {'DOWN', MRef1, _, _, _Info1} ->
            timer:sleep(?HANDLER_RESTART_AFTER + 1000),
            undefined = whereis(?MODULE),
            ok
    after
        5000 ->
            ct:fail("Handler not dead! It should not have survived this!")
    end,
    
    {Log,_,_} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    RestartAfter = ?HANDLER_RESTART_AFTER,
    NewHConfig2 =
        HConfig#{logger_disk_log_h=>DLHConfig#{enable_kill_overloaded=>true,
                                               handler_overloaded_qlen=>10,
                                               handler_restart_after=>RestartAfter}},
    ok = logger:set_handler_config(?MODULE, NewHConfig2),
    Pid0 = whereis(?MODULE),
    MRef2 = erlang:monitor(process, Pid0),
    %% kill handler
    send_burst({n,100}, {spawn,2,0}, {chars,79}, info),
    receive
        {'DOWN', MRef2, _, _, _Info2} ->
            timer:sleep(RestartAfter + 2000),
            Pid1 = whereis(?MODULE),
            true = is_pid(Pid1),
            false = (Pid1 == Pid0),
            ok
    after
        5000 ->
            ct:fail("Handler not dead! It should not have survived this!")
    end,
    ok.
restart_after(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

%% send handler requests (filesync, info, reset, change_config)
%% during high load to verify that sync, dropping and flushing is
%% handled correctly.
handler_requests_under_load() ->
    [{timetrap,{seconds,60}}].
handler_requests_under_load(Config) ->
    {Log,HConfig,DLHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    NewHConfig =
        HConfig#{logger_disk_log_h => DLHConfig#{toggle_sync_qlen => 2,
                                                 drop_new_reqs_qlen => 1000,
                                                 flush_reqs_qlen => 2000,
                                                 enable_burst_limit => false}},   
    ok = logger:set_handler_config(?MODULE, NewHConfig),
    Pid = spawn_link(fun() -> send_requests(?MODULE, 1, [{disk_log_sync,[]},
                                                         {info,[]},
                                                         {reset,[]},
                                                         {change_config,[]}])
                     end),
    Procs = 100,
    Sent = Procs * send_burst({n,5000}, {spawn,Procs,10}, {chars,79}, info),
    Pid ! {self(),finish},
    ReqResult = receive {Pid,Result} -> Result end,
    Logged = count_lines(Log),
    ct:pal("Number of messages sent = ~w~nNumber of messages logged = ~w",
           [Sent,Logged]),
    FindError = fun(Res) ->
                        [E || E <- Res,
                              is_tuple(E) andalso (element(1,E) == error)]
                end,
    Errors = [{Req,FindError(Res)} || {Req,Res} <- ReqResult],
    NoOfReqs = lists:foldl(fun({_,Res}, N) -> N + length(Res) end, 0, ReqResult),
    ct:pal("~w requests made. Errors: ~n~p", [NoOfReqs,Errors]),
    ok = file:delete(Log).
handler_requests_under_load(cleanup, Config) ->
    ok = stop_handler(?MODULE).

send_requests(HName, TO, Reqs = [{Req,Res}|Rs]) ->
    receive
        {From,finish} ->
            From ! {self(),Reqs}
    after
        TO ->
            Result =
                case Req of
                    change_config ->
                        logger:set_handler_config(HName, logger_disk_log_h,
                                                  #{enable_kill_overloaded =>
                                                        false});
                    Func ->
                        logger_disk_log_h:Func(HName)
                end,
            send_requests(HName, TO, Rs ++ [{Req,[Result|Res]}])
    end.

%%%-----------------------------------------------------------------
%%% 
start_handler(Name, FuncName, Config) ->
    Dir = ?config(priv_dir,Config),
    File = filename:join(Dir, FuncName),
    ct:pal("Logging to ~tp", [File]),
    ok = logger:add_handler(Name,
                            logger_disk_log_h,
                            #{disk_log_opts=>#{file => File,
                                               max_no_files => 1,
                                               max_no_bytes => 100000000},
                              filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([Name]),
                              formatter=>{?MODULE,op}}),
    {ok,{_,HConfig = #{logger_disk_log_h := DLHConfig}}} =
        logger:get_handler_config(Name),
    {lists:concat([File,".1"]),HConfig,DLHConfig}.
    
stop_handler(Name) ->
    ok = logger:remove_handler(Name),
    ct:pal("Handler ~p stopped!", [Name]).    

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
    PerProc = fun() -> 
                      send_n_burst(N, seq, Text, Class)
              end,
    MRefs = [begin if TO == 0 -> ok; true -> timer:sleep(TO) end,
                   monitor(process,spawn_link(PerProc)) end ||
                _ <- lists:seq(1,Ps)],
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
format(#{msg:={F,A}},Pid) when is_list(F), is_list(A) ->
    String = lists:flatten(io_lib:format(F,A)),
    Pid ! {log,String},
    String++"\n";
format(#{msg:={string,String0}},Pid) ->
    String = unicode:characters_to_list(String0),
    Pid ! {log,String},
    String++"\n";
format(Msg,Tag) ->
    Error = {unexpected_format,Msg,Tag},
    erlang:display(Error),
    exit(Error).

remove(Handler, LogName) ->
    logger_disk_log_h:remove(Handler, LogName),
    HState = #{log_names := Logs} = logger_disk_log_h:info(),
    false = maps:is_key(LogName, HState),
    false = lists:member(LogName, Logs),
    false = logger_config:exist(logger, LogName),
    {error,no_such_log} = disk_log:info(LogName),
    ok.

start_and_add(Name, Config, LogOpts) ->
    ct:pal("Adding handler ~w with: ~p",
           [Name,Config#{disk_log_opts=>LogOpts}]),
    ok = logger:add_handler(Name, logger_disk_log_h,
                            Config#{disk_log_opts=>LogOpts}),
    Pid = whereis(Name),
    true = is_pid(Pid),
    Name = proplists:get_value(name, disk_log:info(Name)),
    ok.
 
remove_and_stop(Handler) ->
    ok = logger:remove_handler(Handler),
    timer:sleep(500),
    undefined = whereis(Handler),
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
                {ok,#file_info{size = Sz1}} ->
                    ok;
                {ok,#file_info{size = Sz2}} ->
                    wait_until_written(File, Sz2)
            end;
        {ok,#file_info{size = Sz1}} ->
            wait_until_written(File, Sz1)
    end.
    
count_lines1(File) ->
    Counter = fun Cnt(Dev,LC) ->
                      case file:read_line(Dev) of
                          eof -> LC;
                          _   -> Cnt(Dev,LC+1)
                      end
              end,
    {_,Dev} = file:open(File, [read]),
    Lines = Counter(Dev, 0),
    file:close(Dev),
    Lines.

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
    dbg:p(whereis(?MODULE),[c]),
    dbg:p(Pid,[c]),
    tpl(Trace),
    ok.

tpl([{M,F,A}|Trace]) ->
    {ok,Match} = dbg:tpl(M,F,A,[]),
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

tracer({trace,_,call,{?MODULE,format,[#{msg:={string,Msg}}|_]}},
       {Pid,[{formatter,Msg}|Expected]}) ->
    maybe_tracer_done(Pid,Expected,{formatter,Msg});
tracer({trace,_,call,{logger_disk_log_h,handle_cast,[Op|_]}},
       {Pid,[{Mod,Func,Op}|Expected]}) ->
    maybe_tracer_done(Pid,Expected,{Mod,Func,Op});
tracer({trace,_,call,{Mod,Func,_}}, {Pid,[{Mod,Func}|Expected]}) ->
    maybe_tracer_done(Pid,Expected,{Mod,Func});
tracer({trace,_,call,Call}, {Pid,Expected}) ->
    Pid ! {tracer_got_unexpected,Call,Expected},
    {Pid,Expected}.

maybe_tracer_done(Pid,[],Got) ->
    ct:log("Tracer got: ~p~n",[Got]),
    Pid ! tracer_done;
maybe_tracer_done(Pid,Expected,Got) ->
    ct:log("Tracer got: ~p~n",[Got]),
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
            ct:fail({timeout,tracer})
    end.
