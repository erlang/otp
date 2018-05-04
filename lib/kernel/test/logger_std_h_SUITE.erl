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
-module(logger_std_h_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/src/logger_internal.hrl").
-include_lib("kernel/src/logger_h_common.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("kernel/include/file.hrl").

-define(check_no_log, [] = test_server:messages_get()).
-define(check(Expected),
        receive
            {log,Expected} ->
                [] = test_server:messages_get()
        after 5000 ->
                ct:fail({report_not_received,
                         {line,?LINE},
                         {expected,Expected},
                         {got,test_server:messages_get()}})
        end).

-define(msg,"Log from "++atom_to_list(?FUNCTION_NAME)++
            ":"++integer_to_list(?LINE)).
-define(bin(Msg), list_to_binary(Msg++"\n")).
-define(domain,#{domain=>[?MODULE]}).

-define(FILESYNC_REP_INT, if is_atom(?FILESYNC_REPEAT_INTERVAL) -> 5500;
                             true -> ?FILESYNC_REPEAT_INTERVAL + 500
                          end).

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    timer:start(),                              % to avoid progress report
    {ok,{?STANDARD_HANDLER,#{formatter:=OrigFormatter}}} =
        logger:get_handler_config(?STANDARD_HANDLER),
    [{formatter,OrigFormatter}|Config].

end_per_suite(Config) ->
    {OrigMod,OrigConf} = proplists:get_value(formatter,Config),
    logger:set_handler_config(?STANDARD_HANDLER,formatter,{OrigMod,OrigConf}),
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
    [add_remove_instance_tty,
     add_remove_instance_standard_io,
     add_remove_instance_standard_error,
     add_remove_instance_file1,
     add_remove_instance_file2,
     default_formatter,
     errors,
     formatter_fail,
     config_fail,
     crash_std_h_to_file,
     crash_std_h_to_disk_log,
     bad_input,
     info_and_reset,
     reconfig,
     file_opts,
     filesync,
     write_failure,
     sync_failure,
     op_switch_to_sync_file,
     op_switch_to_sync_tty,
     op_switch_to_drop_file,
     op_switch_to_drop_tty,
     op_switch_to_flush_file,
     op_switch_to_flush_tty,
     limit_burst_disabled,
     limit_burst_enabled_one,
     limit_burst_enabled_period,
     kill_disabled,
     qlen_kill_new,
     qlen_kill_std,
     mem_kill_new,
     mem_kill_std,
     restart_after,
     handler_requests_under_load
    ].

add_remove_instance_tty(_Config) ->
    {error,{handler_not_added,{invalid_config,logger_std_h,{type,tty}}}} =
        logger:add_handler(?MODULE,logger_std_h,
                           #{logger_std_h => #{type => tty},
                             filter_default=>log,
                             formatter=>{?MODULE,self()}}),
    ok.

add_remove_instance_standard_io(_Config) ->
    add_remove_instance_nofile(standard_io).
add_remove_instance_standard_io(cleanup,_Config) ->
    logger_std_h_remove().

add_remove_instance_standard_error(_Config) ->
    add_remove_instance_nofile(standard_error).
add_remove_instance_standard_error(cleanup,_Config) ->
    logger_std_h_remove().

add_remove_instance_file1(Config) ->
    Dir = ?config(priv_dir,Config),
    Log = filename:join(Dir,"stdlog1.txt"),
    Type = {file,Log},
    add_remove_instance_file(Log, Type).
add_remove_instance_file1(cleanup,_Config) ->
    logger_std_h_remove().

add_remove_instance_file2(Config) ->
    Dir = ?config(priv_dir,Config),
    Log = filename:join(Dir,"stdlog2.txt"),
    Type = {file,Log,[raw,append]},
    add_remove_instance_file(Log, Type).
add_remove_instance_file2(cleanup,_Config) ->
    logger_std_h_remove().

add_remove_instance_file(Log, Type) ->
    ok = logger:add_handler(?MODULE,
                            logger_std_h,
                            #{logger_std_h => #{type => Type},
                              filter_default=>stop,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()}}),
    Pid = whereis(?MODULE),
    true = is_pid(Pid),
    logger:info(M1=?msg,?domain),
    ?check(M1),
    B1 = ?bin(M1),
    try_read_file(Log, {ok,B1}, ?FILESYNC_REP_INT),
    ok = logger:remove_handler(?MODULE),
    timer:sleep(500),
    undefined = whereis(?MODULE),
    logger:info(?msg,?domain),
    ?check_no_log,
    try_read_file(Log, {ok,B1}, ?FILESYNC_REP_INT),
    ok.

default_formatter(_Config) ->
    ok = logger:set_handler_config(?STANDARD_HANDLER,formatter,
                                   {?DEFAULT_FORMATTER,?DEFAULT_FORMAT_CONFIG}),
    ct:capture_start(),
    logger:info(M1=?msg),
    timer:sleep(100),
    ct:capture_stop(),
    [Msg] = ct:capture_get(),
    match = re:run(Msg,"=INFO REPORT====.*\n"++M1,[{capture,none}]),
    ok.

errors(Config) ->
    Dir = ?config(priv_dir,Config),
    Log = filename:join(Dir,?FUNCTION_NAME),

    ok = logger:add_handler(?MODULE,logger_std_h,#{}),
    {error,{already_exist,?MODULE}} =
        logger:add_handler(?MODULE,logger_std_h,#{}),
    
    {error,{not_found,no_such_name}} = logger:remove_handler(no_such_name),

    ok = logger:remove_handler(?MODULE),
    {error,{not_found,?MODULE}} = logger:remove_handler(?MODULE),

    {error,
     {handler_not_added,
      {invalid_config,logger_std_h,{type,faulty_type}}}} =
        logger:add_handler(?MODULE,logger_std_h,
                           #{logger_std_h => #{type => faulty_type}}),

    NoDir = lists:concat(["/",?MODULE,"_dir"]),
    {error,
     {handler_not_added,{{open_failed,NoDir,eacces},_}}} =
        logger:add_handler(myh2,logger_std_h,
                           #{logger_std_h=>#{type=>{file,NoDir}}}),

    {error,
     {handler_not_added,{{open_failed,Log,_},_}}} =
        logger:add_handler(myh3,logger_std_h,
                           #{logger_std_h=>#{type=>{file,Log,[bad_file_opt]}}}),

    ok = logger:info(?msg).

errors(cleanup,_Config) ->
    logger:remove_handler(?MODULE).

formatter_fail(Config) ->
    Dir = ?config(priv_dir,Config),
    Log = filename:join(Dir,?FUNCTION_NAME),

    %% no formatter
    ok = logger:add_handler(?MODULE,
                            logger_std_h,
                            #{logger_std_h => #{type => {file,Log}},
                              filter_default=>stop,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE])}),
    Pid = whereis(?MODULE),
    true = is_pid(Pid),
    {ok,#{handlers:=H}} = logger:get_logger_config(),
    true = lists:member(?MODULE,H),

    %% Formatter is added automatically
    {ok,{_,#{formatter:={logger_formatter,_}}}} =
        logger:get_handler_config(?MODULE),
    logger:info(M1=?msg,?domain),
    Got1 = try_match_file(Log,"=INFO REPORT====.*\n"++M1,5000),

    ok = logger:set_handler_config(?MODULE,formatter,{nonexistingmodule,#{}}),
    logger:info(M2=?msg,?domain),
    Got2 = try_match_file(Log,
                          Got1++"=INFO REPORT====.*\nFORMATTER CRASH: .*"++M2,
                          5000),

    ok = logger:set_handler_config(?MODULE,formatter,{?MODULE,crash}),
    logger:info(M3=?msg,?domain),
    Got3 = try_match_file(Log,
                          Got2++"=INFO REPORT====.*\nFORMATTER CRASH: .*"++M3,
                          5000),

    ok = logger:set_handler_config(?MODULE,formatter,{?MODULE,bad_return}),
    logger:info(?msg,?domain),
    try_match_file(Log,
                   Got3++"FORMATTER ERROR: bad_return_value",
                   5000),

    %% Check that handler is still alive and was never dead
    Pid = whereis(?MODULE),
    {ok,#{handlers:=H}} = logger:get_logger_config(),

    ok.

formatter_fail(cleanup,_Config) ->
    logger:remove_handler(?MODULE).

config_fail(_Config) ->
    {error,{handler_not_added,{invalid_config,logger_std_h,{bad,bad}}}} =
        logger:add_handler(?MODULE,logger_std_h,
                           #{logger_std_h => #{bad => bad},
                             filter_default=>log,
                             formatter=>{?MODULE,self()}}),
    {error,{handler_not_added,{invalid_config,logger_std_h,
                               {restart_type,bad}}}} =
        logger:add_handler(?MODULE,logger_std_h,
                           #{logger_std_h => #{restart_type => bad},
                             filter_default=>log,
                             formatter=>{?MODULE,self()}}),
    {error,{handler_not_added,{invalid_levels,{_,1,_}}}} =
        logger:add_handler(?MODULE,logger_std_h,
                           #{logger_std_h => #{drop_new_reqs_qlen=>1}}),
    {error,{handler_not_added,{invalid_levels,{43,42,_}}}} =
        logger:add_handler(?MODULE,logger_std_h,
                           #{logger_std_h => #{toggle_sync_qlen=>43,
                                               drop_new_reqs_qlen=>42}}),
    {error,{handler_not_added,{invalid_levels,{_,43,42}}}} =
        logger:add_handler(?MODULE,logger_std_h,
                           #{logger_std_h => #{drop_new_reqs_qlen=>43,
                                               flush_reqs_qlen=>42}}),

    ok = logger:add_handler(?MODULE,logger_std_h,
                            #{filter_default=>log,
                              formatter=>{?MODULE,self()}}),
    {error,{illegal_config_change,_,_}} =
        logger:set_handler_config(?MODULE,logger_std_h,
                                  #{type=>{file,"file"}}),
    {error,{illegal_config_change,_,_}} =
        logger:set_handler_config(?MODULE,id,bad),
    {error,{invalid_levels,_}} =
        logger:set_handler_config(?MODULE,logger_std_h,
                                  #{toggle_sync_qlen=>100,
                                    flush_reqs_qlen=>99}),
    {error,{invalid_config,logger_std_h,{filesync_rep_int,2000}}} =
        logger:set_handler_config(?MODULE, logger_std_h,
                                  #{filesync_rep_int => 2000}),
    ok.

config_fail(cleanup,_Config) ->
    logger:remove_handler(?MODULE).

crash_std_h_to_file(Config) ->
    crash_std_h(Config,?FUNCTION_NAME,logger_dest,file).
crash_std_h_to_file(cleanup,_Config) ->
    crash_std_h(cleanup).

crash_std_h_to_disk_log(Config) ->
    crash_std_h(Config,?FUNCTION_NAME,logger_dest,disk_log).
crash_std_h_to_disk_log(cleanup,_Config) ->
    crash_std_h(cleanup).

crash_std_h(Config,Func,Var,Type) ->
    Dir = ?config(priv_dir,Config),
    File = lists:concat([?MODULE,"_",Func,".log"]),
    Log = filename:join(Dir,File),
    Pa = filename:dirname(code:which(?MODULE)),
    TypeAndLog =
        case os:type() of
            {win32,_} ->
                lists:concat([" {",Type,",\\\"",Log,"\\\"}"]);
            _ ->
                lists:concat([" \'{",Type,",\"",Log,"\"}\'"])
        end,
    Args = lists:concat([" -kernel ",Var,TypeAndLog," -pa ",Pa]),
    Name = lists:concat([?MODULE,"_",Func]),
    ct:pal("Starting ~p with ~tp", [Name,Args]),
    %% Start a node which prints kernel logs to the destination specified by Type
    {ok,Node} = test_server:start_node(Name, peer, [{args, Args}]),
    Pid = rpc:call(Node,erlang,whereis,[?STANDARD_HANDLER]),
    ok = rpc:call(Node,logger,set_handler_config,[?STANDARD_HANDLER,formatter,
                                                  {?MODULE,self()}]),
    ok = log_on_remote_node(Node,"dummy1"),
    ?check("dummy1"),
    {ok,Bin1} = sync_and_read(Node,Type,Log),
    <<"dummy1\n">> = binary:part(Bin1,{byte_size(Bin1),-7}),

    %% Kill the logger_std_h process
    exit(Pid, kill),

    %% Wait a bit, then check that it is gone
    timer:sleep(2000),
    undefined = rpc:call(Node,erlang,whereis,[?STANDARD_HANDLER]),

    %% Check that file is not empty
    {ok,Bin2} = sync_and_read(Node,Type,Log),
    <<"dummy1\n">> = binary:part(Bin2,{byte_size(Bin2),-7}),
    ok.

%% Can not use rpc:call here, since the code would execute on a
%% process with group_leader on this (the calling) node, and thus
%% logger would send the log event to the logger process here instead
%% of logging it itself.
log_on_remote_node(Node,Msg) ->
    _ = spawn_link(Node,
                   fun() -> erlang:group_leader(whereis(user),self()),
                            logger:info(Msg)
                   end),
    ok.


crash_std_h(cleanup) ->
    Nodes = nodes(),
    [test_server:stop_node(Node) || Node <- Nodes].

sync_and_read(Node,disk_log,Log) ->
    rpc:call(Node,logger_disk_log_h,disk_log_sync,[?STANDARD_HANDLER]),
    case file:read_file(Log ++ ".1") of
        {ok,<<>>} ->
            timer:sleep(5000),
            file:read_file(Log ++ ".1");
        Ok ->
            Ok
    end;
sync_and_read(Node,file,Log) ->
    rpc:call(Node,logger_std_h,filesync,[?STANDARD_HANDLER]),
    case file:read_file(Log) of
        {ok,<<>>} ->
            timer:sleep(5000),
            file:read_file(Log);
        Ok ->
            Ok
    end.

bad_input(_Config) ->
    {error,{badarg,{filesync,["BadType"]}}} = logger_std_h:filesync("BadType"),
    {error,{badarg,{info,["BadType"]}}} = logger_std_h:info("BadType"),
    {error,{badarg,{reset,["BadType"]}}} = logger_std_h:reset("BadType").


info_and_reset(_Config) ->
    #{id := ?STANDARD_HANDLER} = logger_std_h:info(?STANDARD_HANDLER),
    ok = logger_std_h:reset(?STANDARD_HANDLER).

reconfig(Config) ->
    Dir = ?config(priv_dir,Config),
    ok = logger:add_handler(?MODULE,
                            logger_std_h,
                            #{logger_std_h => #{type => standard_io},
                              filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()}}),
    #{id := ?MODULE,
      type := standard_io,
      file_ctrl_pid := FileCtrlPid, 
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
      filesync_repeat_interval := ?FILESYNC_REPEAT_INTERVAL} =
        logger_std_h:info(?MODULE),

    ok = logger:set_handler_config(?MODULE, logger_std_h,
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
      type := standard_io,
      file_ctrl_pid := FileCtrlPid, 
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
      filesync_repeat_interval := no_repeat} = logger_std_h:info(?MODULE),
    ok.

reconfig(cleanup, _Config) ->
    logger:remove_handler(?MODULE).


file_opts(Config) ->
    Dir = ?config(priv_dir,Config),
    Log = filename:join(Dir, lists:concat([?FUNCTION_NAME,".log"])),
    BadFileOpts = [raw],
    BadType = {file,Log,BadFileOpts},
    {error,{handler_not_added,{{open_failed,Log,enoent},_}}} =
        logger:add_handler(?MODULE, logger_std_h,
                           #{logger_std_h => #{type => BadType}}),

    OkFileOpts = [raw,append],
    OkType = {file,Log,OkFileOpts},
    ok = logger:add_handler(?MODULE,
                            logger_std_h,
                            #{logger_std_h => #{type => OkType},
                              filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()}}),

    #{type := OkType} = logger_std_h:info(?MODULE),
    logger:info(M1=?msg,?domain),
    ?check(M1),
    B1 = ?bin(M1),
    try_read_file(Log, {ok,B1}, ?FILESYNC_REP_INT),
    ok.
file_opts(cleanup, _Config) ->
    logger:remove_handler(?MODULE).


filesync(Config) ->
    Dir = ?config(priv_dir,Config),
    Log = filename:join(Dir, lists:concat([?FUNCTION_NAME,".log"])),
    Type = {file,Log},
    ok = logger:add_handler(?MODULE,
                            logger_std_h,
                            #{logger_std_h => #{type => Type},
                              filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()}}),
    Tester = self(),
    TraceFun = fun({trace,_,call,{Mod,Func,Details}}, Pid) ->
                       Pid ! {trace,Mod,Func,Details},
                       Pid;
                  ({trace,TPid,'receive',Received}, Pid) ->
                       Pid ! {trace,TPid,Received},
                       Pid
               end,
    {ok,_} = dbg:tracer(process, {TraceFun, Tester}),
    FileCtrlPid = maps:get(file_ctrl_pid , logger_std_h:info(?MODULE)),
    {ok,_} = dbg:p(FileCtrlPid, [c]),
    {ok,_} = dbg:tpl(logger_std_h, write_to_dev, 5, []),
    {ok,_} = dbg:tpl(logger_std_h, sync_dev, 4, []),
    {ok,_} = dbg:tp(file, datasync, 1, []),

    logger:info("first", ?domain),
    %% wait for automatic filesync
    timer:sleep(?FILESYNC_REP_INT),
    Expected1 = [{log,"first"}, {trace,logger_std_h,write_to_dev},
                 {trace,logger_std_h,sync_dev}, {trace,file,datasync}],
    
    logger:info("second", ?domain),
    %% do explicit filesync
    logger_std_h:filesync(?MODULE),
    %% a second filesync should be ignored
    logger_std_h:filesync(?MODULE),
    Expected2 =  [{log,"second"}, {trace,logger_std_h,write_to_dev},
                  {trace,logger_std_h,sync_dev}, {trace,file,datasync}],

    %% check that if there's no repeated filesync active,
    %% a filesync is still performed when handler goes idle
    logger:set_handler_config(?MODULE, logger_std_h,
                              #{filesync_repeat_interval => no_repeat}),
    no_repeat = maps:get(filesync_repeat_interval, logger_std_h:info(?MODULE)),
    logger:info("third", ?domain),
    timer:sleep(?IDLE_DETECT_TIME_MSEC*2),
    logger:info("fourth", ?domain),
    %% wait for automatic filesync
    timer:sleep(?IDLE_DETECT_TIME_MSEC*2),
    Expected3 = [{log,"third"}, {trace,logger_std_h,write_to_dev},
                 {log,"fourth"}, {trace,logger_std_h,write_to_dev},
                 {trace,logger_std_h,sync_dev}, {trace,file,datasync}],

    dbg:stop_clear(),

    %% verify that filesync has been performed as expected
    Received1 = lists:map(fun({trace,M,F,_}) -> {trace,M,F};
                             (Other) -> Other
                          end, test_server:messages_get()),
    ct:pal("Trace #1 =~n~p", [Received1]),
    Received1 = Expected1 ++ Expected2 ++ Expected3,

    try_read_file(Log, {ok,<<"first\nsecond\nthird\nfourth\n">>}, 1000),
    
    {ok,_} = dbg:tracer(process, {TraceFun, Tester}),
    {ok,_} = dbg:p(whereis(?MODULE), [c]),
    {ok,_} = dbg:tpl(logger_std_h, handle_cast, 2, []),

    %% switch repeated filesync on and verify that the looping works
    SyncInt = 1000,
    WaitT = 4500,
    logger:set_handler_config(?MODULE, logger_std_h,
                              #{filesync_repeat_interval => SyncInt}),
    SyncInt = maps:get(filesync_repeat_interval, logger_std_h:info(?MODULE)),
    timer:sleep(WaitT),
    logger:set_handler_config(?MODULE, logger_std_h,
                              #{filesync_repeat_interval => no_repeat}),    
    dbg:stop_clear(),

    Received2 = lists:map(fun({trace,_M,handle_cast,[Op,_]}) -> {trace,Op};
                             (Other) -> Other
                          end, test_server:messages_get()),
    ct:pal("Trace #2 =~n~p", [Received2]),
    OneSync = [{trace,repeated_filesync}],
    %% receive 1 initial repeated_filesync, then 1 per sec
    Received2 =
        lists:flatten([OneSync || _ <- lists:seq(1, 1 + trunc(WaitT/SyncInt))]),
    ok.
filesync(cleanup, _Config) ->
    logger:remove_handler(?MODULE).

write_failure(Config) ->
    Dir = ?config(priv_dir, Config),
    File = lists:concat([?MODULE,"_",?FUNCTION_NAME,".log"]),
    Log = filename:join(Dir, File),
    Node = start_std_h_on_new_node(Config, ?FUNCTION_NAME, Log),
    false = (undefined == rpc:call(Node, ets, whereis, [?TEST_HOOKS_TAB])),
    rpc:call(Node, ets, insert, [?TEST_HOOKS_TAB,{tester,self()}]),
    rpc:call(Node, ?MODULE, set_internal_log, [?MODULE,internal_log]),
    rpc:call(Node, ?MODULE, set_result, [file_write,ok]),

    ok = log_on_remote_node(Node, "Logged1"),
    rpc:call(Node, logger_std_h, filesync, [?STANDARD_HANDLER]),
    ?check_no_log,
    try_read_file(Log, {ok,<<"Logged1\n">>}, ?FILESYNC_REP_INT),

    rpc:call(Node, ?MODULE, set_result, [file_write,{error,terminated}]),
    ok = log_on_remote_node(Node, "Cause simple error printout"),

    ?check({error,{?STANDARD_HANDLER,write,Log,{error,terminated}}}),

    ok = log_on_remote_node(Node, "No second error printout"),
    ?check_no_log,

    rpc:call(Node, ?MODULE, set_result, [file_write,{error,eacces}]),
    ok = log_on_remote_node(Node, "Cause simple error printout"),
    ?check({error,{?STANDARD_HANDLER,write,Log,{error,eacces}}}),

    rpc:call(Node, ?MODULE, set_result, [file_write,ok]),
    ok = log_on_remote_node(Node, "Logged2"),
    rpc:call(Node, logger_std_h, filesync, [?STANDARD_HANDLER]),
    ?check_no_log,
    try_read_file(Log, {ok,<<"Logged1\nLogged2\n">>}, ?FILESYNC_REP_INT),
    ok.
write_failure(cleanup, _Config) ->
    Nodes = nodes(),
    [test_server:stop_node(Node) || Node <- Nodes].

sync_failure(Config) ->
    Dir = ?config(priv_dir, Config),
    File = lists:concat([?MODULE,"_",?FUNCTION_NAME,".log"]),
    Log = filename:join(Dir, File),
    Node = start_std_h_on_new_node(Config, ?FUNCTION_NAME, Log),
    false = (undefined == rpc:call(Node, ets, whereis, [?TEST_HOOKS_TAB])),
    rpc:call(Node, ets, insert, [?TEST_HOOKS_TAB,{tester,self()}]),
    rpc:call(Node, ?MODULE, set_internal_log, [?MODULE,internal_log]),
    rpc:call(Node, ?MODULE, set_result, [file_datasync,ok]),
    
    SyncInt = 500,
    ok = rpc:call(Node, logger, set_handler_config,
                  [?STANDARD_HANDLER, logger_std_h,
                   #{filesync_repeat_interval => SyncInt}]),
    Info = rpc:call(Node, logger_std_h, info, [?STANDARD_HANDLER]),
    SyncInt = maps:get(filesync_repeat_interval, Info),
    
    ok = log_on_remote_node(Node, "Logged1"),
    ?check_no_log,

    rpc:call(Node, ?MODULE, set_result, [file_datasync,{error,terminated}]),
    ok = log_on_remote_node(Node, "Cause simple error printout"),
    
    ?check({error,{?STANDARD_HANDLER,filesync,Log,{error,terminated}}}),

    ok = log_on_remote_node(Node, "No second error printout"),
    ?check_no_log,

    rpc:call(Node, ?MODULE, set_result, [file_datasync,{error,eacces}]),
    ok = log_on_remote_node(Node, "Cause simple error printout"),
    ?check({error,{?STANDARD_HANDLER,filesync,Log,{error,eacces}}}),

    rpc:call(Node, ?MODULE, set_result, [file_datasync,ok]),
    ok = log_on_remote_node(Node, "Logged2"),
    ?check_no_log,
    ok.
sync_failure(cleanup, _Config) ->
    Nodes = nodes(),
    [test_server:stop_node(Node) || Node <- Nodes].

start_std_h_on_new_node(_Config, Func, Log) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Dest =
        case os:type() of
            {win32,_} ->
                lists:concat([" {file,\\\"",Log,"\\\"}"]);
            _ ->
                lists:concat([" \'{file,\"",Log,"\"}\'"])
        end,
    Args = lists:concat([" -kernel ",logger_dest,Dest," -pa ",Pa]),
    Name = lists:concat([?MODULE,"_",Func]),
    ct:pal("Starting ~s with ~tp", [Name,Args]),
    {ok,Node} = test_server:start_node(Name, peer, [{args, Args}]),
    Pid = rpc:call(Node,erlang,whereis,[?STANDARD_HANDLER]),
    true = is_pid(Pid),
    ok = rpc:call(Node,logger,set_handler_config,[?STANDARD_HANDLER,formatter,
                                                  {?MODULE,nl}]),
    Node.

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

op_switch_to_sync_file(Config) ->
    {Log,HConfig,StdHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    NumOfReqs = 500,
    NewHConfig =
        HConfig#{logger_std_h => StdHConfig#{toggle_sync_qlen => 2,
                                             drop_new_reqs_qlen => NumOfReqs+1,
                                             flush_reqs_qlen => 2*NumOfReqs,
                                             enable_burst_limit => false}},
    ok = logger:set_handler_config(?MODULE, NewHConfig),
    %%    TRecvPid = start_op_trace(),
    send_burst({n,NumOfReqs}, seq, {chars,79}, info),
    Lines = count_lines(Log),
    ok = file:delete(Log),
    %% true = analyse_trace(TRecvPid,
    %%                      fun(Events) -> find_mode(async,Events) end),
    %% true = analyse_trace(TRecvPid,
    %%                      fun(Events) -> find_mode(sync,Events) end),
    %% true = analyse_trace(TRecvPid,
    %%                      fun(Events) -> find_switch(async,sync,Events) end),
    %% false = analyse_trace(TRecvPid,
    %%                       fun(Events) -> find_mode(drop,Events) end),
    %% false = analyse_trace(TRecvPid,
    %%                       fun(Events) -> find_mode(flush,Events) end),    
    %%    stop_op_trace(TRecvPid),
    NumOfReqs = Lines,
    ok.
op_switch_to_sync_file(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

op_switch_to_sync_tty(Config) ->
    {HConfig,StdHConfig} = start_handler(?MODULE, standard_io, Config),
    NumOfReqs = 500,
    NewHConfig =
        HConfig#{logger_std_h => StdHConfig#{toggle_sync_qlen => 3,
                                             drop_new_reqs_qlen => NumOfReqs+1,
                                             flush_reqs_qlen => 2*NumOfReqs,
                                             enable_burst_limit => false}},
    ok = logger:set_handler_config(?MODULE, NewHConfig),
    send_burst({n,NumOfReqs}, seq, {chars,79}, info),
    ok.
op_switch_to_sync_tty(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

op_switch_to_drop_file() ->
    [{timetrap,{seconds,180}}].
op_switch_to_drop_file(Config) ->
    Test =
        fun() ->
                {Log,HConfig,StdHConfig} =
                    start_handler(?MODULE, ?FUNCTION_NAME, Config),
                NumOfReqs = 300,
                Procs = 2,
                Bursts = 10,
                NewHConfig =
                    HConfig#{logger_std_h =>
                                 StdHConfig#{toggle_sync_qlen => 1,
                                             drop_new_reqs_qlen => 2,
                                             flush_reqs_qlen =>
                                                 Procs*NumOfReqs*Bursts,
                                             enable_burst_limit => false}},
                ok = logger:set_handler_config(?MODULE, NewHConfig),
                %% It sometimes happens that the handler gets the
                %% requests in a slow enough pace so that dropping
                %% never occurs. Therefore, lets generate a number of
                %% bursts to increase the chance of message buildup.
                [send_burst({n,NumOfReqs}, {spawn,Procs,0}, {chars,79}, info) ||
                    _ <- lists:seq(1, Bursts)],
                Logged = count_lines(Log),
                ok = stop_handler(?MODULE),
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
op_switch_to_drop_file(cleanup, _Config) ->
    _ = stop_handler(?MODULE).

op_switch_to_drop_tty(Config) ->
    {HConfig,StdHConfig} = start_handler(?MODULE, standard_io, Config),
    NumOfReqs = 300,
    Procs = 2,
    NewHConfig =
        HConfig#{logger_std_h => StdHConfig#{toggle_sync_qlen => 1,
                                             drop_new_reqs_qlen => 2,
                                             flush_reqs_qlen =>
                                                 Procs*NumOfReqs+1,
                                             enable_burst_limit => false}},
    ok = logger:set_handler_config(?MODULE, NewHConfig),
    send_burst({n,NumOfReqs}, {spawn,Procs,0}, {chars,79}, info),
    ok.
op_switch_to_drop_tty(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

op_switch_to_flush_file() ->
    [{timetrap,{seconds,180}}].
op_switch_to_flush_file(Config) ->
    Test =
        fun() ->
                {Log,HConfig,StdHConfig} =
                    start_handler(?MODULE, ?FUNCTION_NAME, Config),

                %% NOTE: it's important that both async and sync
                %% requests have been queued when the flush happens
                %% (verify with coverage of flush_log_requests/2)

                NewHConfig =
                    HConfig#{logger_std_h =>
                                 StdHConfig#{toggle_sync_qlen => 2,
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
                ok = stop_handler(?MODULE),
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
op_switch_to_flush_file(cleanup, _Config) ->
    _ = stop_handler(?MODULE).

op_switch_to_flush_tty(Config) ->
    {HConfig,StdHConfig} = start_handler(?MODULE, standard_io, Config),

    %% it's important that both async and sync requests have been queued
    %% when the flush happens (verify with coverage of flush_log_requests/2)

    NewHConfig =
        HConfig#{logger_std_h => StdHConfig#{toggle_sync_qlen => 2,
                                             %% disable drop mode
                                             drop_new_reqs_qlen => 100,
                                             flush_reqs_qlen => 100,
                                             enable_burst_limit => false}},   
    ok = logger:set_handler_config(?MODULE, NewHConfig),
    NumOfReqs = 1000,
    Procs = 100,
    send_burst({n,NumOfReqs}, {spawn,Procs,0}, {chars,79}, info),
    ok.
op_switch_to_flush_tty(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

limit_burst_disabled(Config) ->
    {Log,HConfig,StdHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    NewHConfig =
        HConfig#{logger_std_h => StdHConfig#{enable_burst_limit => false,
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
    {Log,HConfig,StdHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    ReqLimit = 10,
    NewHConfig =
        HConfig#{logger_std_h => StdHConfig#{enable_burst_limit => true,
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
    {Log,HConfig,StdHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    ReqLimit = 10,
    BurstTWin = 1000,
    NewHConfig =
        HConfig#{logger_std_h => StdHConfig#{enable_burst_limit => true,
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
    {Log,HConfig,StdHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
     NewHConfig =
        HConfig#{logger_std_h=>StdHConfig#{enable_kill_overloaded=>false,
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
    {_Log,HConfig,StdHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    Pid0 = whereis(?MODULE),
    {_,Mem0} = process_info(Pid0, memory),
    RestartAfter = ?HANDLER_RESTART_AFTER,
     NewHConfig =
        HConfig#{logger_std_h=>StdHConfig#{enable_kill_overloaded=>true,
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
            Info = logger_std_h:info(?MODULE),
            ct:pal("Handler state = ~p", [Info]),
            ct:fail("Handler not dead! It should not have survived this!")
    end.
qlen_kill_new(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

%% choke the standard handler on remote node to verify the termination
%% works as expected    
qlen_kill_std(_Config) ->
    %%! HERE
    %% Dir = ?config(priv_dir, Config),
    %% File = lists:concat([?MODULE,"_",?FUNCTION_NAME,".log"]),
    %% Log = filename:join(Dir, File),
    %% Node = start_std_h_on_new_node(Config, ?FUNCTION_NAME, Log),
    %% ok = rpc:call(Node, logger, set_handler_config,
    %%               [?STANDARD_HANDLER, logger_std_h,
    %%                #{enable_kill_overloaded=>true,
    %%                  handler_overloaded_qlen=>10,
    %%                  handler_overloaded_mem=>100000}]),
    {skip,"Not done yet"}.

mem_kill_new(Config) ->
    {_Log,HConfig,StdHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    Pid0 = whereis(?MODULE),
    {_,Mem0} = process_info(Pid0, memory),
    RestartAfter = ?HANDLER_RESTART_AFTER,
     NewHConfig =
        HConfig#{logger_std_h=>StdHConfig#{enable_kill_overloaded=>true,
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
            Info = logger_std_h:info(?MODULE),
            ct:pal("Handler state = ~p", [Info]),
            ct:fail("Handler not dead! It should not have survived this!")
    end.
mem_kill_new(cleanup, _Config) ->
    ok = stop_handler(?MODULE).
    
%% choke the standard handler on remote node to verify the termination
%% works as expected
mem_kill_std(_Config) ->
    {skip,"Not done yet"}.

restart_after(Config) ->
    {Log,HConfig,StdHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
     NewHConfig1 =
        HConfig#{logger_std_h=>StdHConfig#{enable_kill_overloaded=>true,
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
        HConfig#{logger_std_h=>StdHConfig#{enable_kill_overloaded=>true,
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
    {Log,HConfig,StdHConfig} =
        start_handler(?MODULE, ?FUNCTION_NAME, Config),
    NewHConfig =
        HConfig#{logger_std_h => StdHConfig#{toggle_sync_qlen => 2,
                                             drop_new_reqs_qlen => 1000,
                                             flush_reqs_qlen => 2000,
                                             enable_burst_limit => false}},   
    ok = logger:set_handler_config(?MODULE, NewHConfig),
    Pid = spawn_link(fun() -> send_requests(?MODULE, 1, [{filesync,[]},
                                                         {info,[]},
                                                         {reset,[]},
                                                         {change_config,[]}])
                     end),
    Sent = send_burst({t,10000}, seq, {chars,79}, info),
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
handler_requests_under_load(cleanup, _Config) ->
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
                        logger:set_handler_config(HName, logger_std_h,
                                                  #{enable_kill_overloaded =>
                                                        false});
                    Func ->
                        logger_std_h:Func(HName)
                end,
            send_requests(HName, TO, Rs ++ [{Req,[Result|Res]}])
    end.
            

%%%-----------------------------------------------------------------
%%% 
start_handler(Name, TTY, Config) when TTY == standard_io;
                                      TTY == standard_error->
    ok = logger:add_handler(Name,
                            logger_std_h,
                            #{logger_std_h => #{type => TTY},
                              filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([Name]),
                              formatter=>{?MODULE,op}}),
    {ok,{_,HConfig = #{logger_std_h := StdHConfig}}} =
        logger:get_handler_config(Name),
    {HConfig,StdHConfig};

start_handler(Name, FuncName, Config) ->
    Dir = ?config(priv_dir,Config),
    Log = filename:join(Dir, lists:concat([FuncName,".log"])),
    ct:pal("Logging to ~tp", [Log]),
    Type = {file,Log},
    ok = logger:add_handler(Name,
                            logger_std_h,
                            #{logger_std_h => #{type => Type},
                              filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([Name]),
                              formatter=>{?MODULE,op}}),
    {ok,{_,HConfig = #{logger_std_h := StdHConfig}}} =
        logger:get_handler_config(Name),
    {Log,HConfig,StdHConfig}.
    
stop_handler(Name) ->
    ok = logger:remove_handler(Name),
    ct:pal("Handler ~p stopped!", [Name]).    

count_lines(File) ->
    wait_until_written(File, -1),
    count_lines1(File).

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
    String++"\n".

add_remove_instance_nofile(Type) ->
    ok = logger:add_handler(?MODULE,logger_std_h,
                            #{logger_std_h => #{type => Type},
                              filter_default=>stop,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()}}),
    Pid = whereis(?MODULE),
    true = is_pid(Pid),
    group_leader(group_leader(),Pid), % to get printouts in test log
    logger:info(M1=?msg,?domain),
    ?check(M1),
    %% check that filesync doesn't do damage even if not relevant
    ok = logger_std_h:filesync(?MODULE),
    ok = logger:remove_handler(?MODULE),
    timer:sleep(500),
    undefined = whereis(?MODULE),
    logger:info(?msg,?domain),
    ?check_no_log,
    ok.

logger_std_h_remove() ->
    logger:remove_handler(?MODULE).
logger_std_h_remove(Id) ->
    logger:remove_handler(Id).

try_read_file(FileName, Expected, Time) when Time > 0 ->
    case file:read_file(FileName) of
        Expected ->
            ok;
        Error = {error,_Reason} ->
            ct:pal("Can't read ~tp: ~tp", [FileName,Error]),
            erlang:error(Error);
        Got ->
            ct:pal("try_read_file got ~tp", [Got]),
            timer:sleep(500),
            try_read_file(FileName, Expected, Time-500)
    end;
try_read_file(FileName, Expected, _) ->
    ct:pal("Missing pattern ~tp in ~tp", [Expected,FileName]),
    erlang:error({error,missing_expected_pattern}).

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
    

%%%-----------------------------------------------------------------
%%% 
start_op_trace() ->
    TraceFun = fun({trace,_,call,{_Mod,Func,Details}}, Pid) ->
                       Pid ! {trace_call,Func,Details},
                       Pid;
                  ({trace,_,return_from,{_Mod,Func,_},RetVal}, Pid) ->
                       Pid ! {trace_return,Func,RetVal},
                       Pid
               end,
    TRecvPid = spawn_link(fun() -> trace_receiver(5000) end),
    {ok,_} = dbg:tracer(process, {TraceFun, TRecvPid}),

    {ok,_} = dbg:p(whereis(?MODULE), [c]),
    {ok,_} = dbg:p(self(), [c]),

    MS1 = dbg:fun2ms(fun([_]) -> return_trace() end),
    {ok,_} = dbg:tp(logger_h_common, check_load, 1, MS1),

    {ok,_} = dbg:tpl(logger_h_common, flush_log_requests, 2, []),

    MS2 = dbg:fun2ms(fun([_,mode]) -> return_trace() end),
    {ok,_} = dbg:tpl(ets, lookup, 2, MS2),

    ct:pal("Tracing started!", []),
    TRecvPid.
    
stop_op_trace(TRecvPid) ->
    dbg:stop_clear(),
    unlink(TRecvPid),
    exit(TRecvPid, kill),
    ok.

find_mode(flush, Events) ->
    lists:any(fun({trace_call,flush_log_requests,[_,_]}) -> true;
                 (_) -> false
              end, Events);
find_mode(Mode, Events) ->
    lists:keymember([{mode,Mode}], 3, Events).

%% find_switch(_From, To, Events) ->
%%     try lists:foldl(fun({trace_return,check_load,{To,_,_,_}},
%%                         {trace_call,check_load,[#{mode := From}]}) ->
%%                             throw(match);
%%                        (Event, _) ->
%%                             Event
%%                     end, undefined, Events) of
%%         _ -> false
%%     catch
%%         throw:match -> true
%%     end.

analyse_trace(TRecvPid, TestFun) ->
    TRecvPid ! {test,self(),TestFun},
    receive
        {result,TRecvPid,Result} ->
            Result
    after
        60000 ->
            fails
    end.

trace_receiver(IdleT) ->
    Msgs = receive_until_idle(IdleT, 5, []),
    ct:pal("~w trace events generated", [length(Msgs)]),
    analyse(Msgs).

receive_until_idle(IdleT, WaitN, Msgs) ->
    receive
        Msg = {trace_call,_,_} ->
            receive_until_idle(IdleT, 5, [Msg | Msgs]);
        Msg = {trace_return,_,_} ->
            receive_until_idle(IdleT, 5, [Msg | Msgs])
    after
        IdleT ->
            if WaitN == 0 ->
                    Msgs;
               true ->
                    receive_until_idle(IdleT, WaitN-1, Msgs)
            end
    end.
          
analyse(Msgs) ->
    receive
        {test,From,TestFun} ->
            From ! {result,self(),TestFun(Msgs)},
            analyse(Msgs)
    end.
