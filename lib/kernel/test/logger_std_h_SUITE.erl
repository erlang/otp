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
-include_lib("kernel/src/logger_olp.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("kernel/include/file.hrl").

-define(check_no_log,
        begin
            timer:sleep(?IDLE_DETECT_TIME*2),
            [] = test_server:messages_get()
        end).
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

suite() ->
    [{timetrap,{seconds,30}},
     {ct_hooks,[logger_test_lib]}].

init_per_suite(Config) ->
    timer:start(),                              % to avoid progress report
    {ok,#{formatter:=OrigFormatter}} =
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

init_per_testcase(reopen_changed_log=TC, Config) ->
    case os:type() of
        {win32,_} ->
            {skip,"This test can only work with inodes, i.e. not on Windows"};
        _ ->
            ct:print("********** ~w **********", [TC]),
            Config
    end;
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
init_per_testcase(OPCase, Config) when
      OPCase == qlen_kill_new;
      OPCase == restart_after ->
    case re:run(erlang:system_info(system_version),
                "dirty-schedulers-TEST",
                [{capture,none}]) of
        match ->
            {skip,"Overload protection test skipped on dirty-schedulers-TEST"};
        nomatch ->
            ct:print("********** ~w **********", [OPCase]),
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
     add_remove_instance_file3,
     add_remove_instance_file4,
     default_formatter,
     filter_config,
     errors,
     formatter_fail,
     config_fail,
     crash_std_h_to_file,
     crash_std_h_to_disk_log,
     bad_input,
     reconfig,
     file_opts,
     relative_file_path,
     sync,
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
     handler_requests_under_load,
     recreate_deleted_log,
     reopen_changed_log,
     rotate_size,
     rotate_size_compressed,
     rotate_size_reopen,
     rotate_on_start_compressed,
     rotation_opts,
     rotation_opts_restart_handler
    ].

add_remove_instance_tty(_Config) ->
    {error,{handler_not_added,{invalid_config,logger_std_h,#{type:=tty}}}} =
        logger:add_handler(?MODULE,logger_std_h,
                           #{config => #{type => tty},
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

add_remove_instance_file3(_Config) ->
    Log = atom_to_list(?MODULE),
    StdHConfig = #{type=>file},
    add_remove_instance_file(Log, StdHConfig).
add_remove_instance_file3(cleanup,_Config) ->
    logger_std_h_remove().

add_remove_instance_file4(Config) ->
    Dir = ?config(priv_dir,Config),
    Log = filename:join(Dir,"stdlog4.txt"),
    StdHConfig = #{file=>Log,modes=>[]},
    add_remove_instance_file(Log, StdHConfig).
add_remove_instance_file4(cleanup,_Config) ->
    logger_std_h_remove().

add_remove_instance_file(Log, Type) when not is_map(Type) ->
    add_remove_instance_file(Log,#{type=>Type});
add_remove_instance_file(Log, StdHConfig) when is_map(StdHConfig) ->
    ok = logger:add_handler(?MODULE,
                            logger_std_h,
                            #{config => StdHConfig,
                              filter_default=>stop,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()}}),
    Pid = whereis(h_proc_name()),
    true = is_pid(Pid),
    logger:notice(M1=?msg,?domain),
    ?check(M1),
    B1 = ?bin(M1),
    try_read_file(Log, {ok,B1}, filesync_rep_int()),
    ok = logger:remove_handler(?MODULE),
    timer:sleep(500),
    undefined = whereis(h_proc_name()),
    logger:notice(?msg,?domain),
    ?check_no_log,
    try_read_file(Log, {ok,B1}, filesync_rep_int()),
    ok.

default_formatter(_Config) ->
    ok = logger:set_handler_config(?STANDARD_HANDLER,formatter,
                                   {?DEFAULT_FORMATTER,?DEFAULT_FORMAT_CONFIG}),
    ct:capture_start(),
    logger:notice(M1=?msg),
    timer:sleep(100),
    ct:capture_stop(),
    [Msg] = ct:capture_get(),
    match = re:run(Msg,"=NOTICE REPORT====.*\n"++M1,[{capture,none}]),
    ok.

filter_config(_Config) ->
    ok = logger:add_handler(?MODULE,logger_std_h,#{}),
    {ok,#{config:=HConfig}=Config} = logger:get_handler_config(?MODULE),
    HConfig = maps:without([olp],HConfig),

    FakeFullHConfig = HConfig#{olp=>{regname,self(),erlang:make_ref()}},
    #{config:=HConfig} =
        logger_std_h:filter_config(Config#{config=>FakeFullHConfig}),
    ok.

filter_config(cleanup,_Config) ->
    logger:remove_handler(?MODULE),
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
      {invalid_config,logger_std_h,#{type:=faulty_type}}}} =
        logger:add_handler(?MODULE,logger_std_h,
                           #{config => #{type => faulty_type}}),

    case os:type() of
        {win32,_} ->
            %% No use in testing file access on windows
            ok;
        _ ->
            NoDir = lists:concat(["/",?MODULE,"_dir"]),
            {error,
             {handler_not_added,{open_failed,NoDir,eacces}}} =
                logger:add_handler(myh2,logger_std_h,
                                   #{config=>#{type=>{file,NoDir}}})
    end,

    {error,
     {handler_not_added,
      {invalid_config,logger_std_h,#{modes:=bad_file_opt}}}} =
        logger:add_handler(myh3,logger_std_h,
                           #{config=>#{type=>{file,Log,bad_file_opt}}}),

    ok = logger:notice(?msg).

errors(cleanup,_Config) ->
    logger:remove_handler(?MODULE).

formatter_fail(Config) ->
    Dir = ?config(priv_dir,Config),
    Log = filename:join(Dir,?FUNCTION_NAME),

    %% no formatter
    ok = logger:add_handler(?MODULE,
                            logger_std_h,
                            #{config => #{type => {file,Log}},
                              filter_default=>stop,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE])}),
    Pid = whereis(h_proc_name()),
    true = is_pid(Pid),
    H = logger:get_handler_ids(),
    true = lists:member(?MODULE,H),

    %% Formatter is added automatically
    {ok,#{formatter:={logger_formatter,_}}} = logger:get_handler_config(?MODULE),
    logger:notice(M1=?msg,?domain),
    Got1 = try_match_file(Log,"[0-9\\+\\-T:\\.]* notice: "++M1,5000),

    ok = logger:set_handler_config(?MODULE,formatter,{nonexistingmodule,#{}}),
    logger:notice(M2=?msg,?domain),
    Got2 = try_match_file(Log,
                          escape(Got1)++"[0-9\\+\\-T:\\.]* notice: FORMATTER CRASH: .*"++M2,
                          5000),

    ok = logger:set_handler_config(?MODULE,formatter,{?MODULE,crash}),
    logger:notice(M3=?msg,?domain),
    Got3 = try_match_file(Log,
                          escape(Got2)++"[0-9\\+\\-T:\\.]* notice: FORMATTER CRASH: .*"++M3,
                          5000),

    ok = logger:set_handler_config(?MODULE,formatter,{?MODULE,bad_return}),
    logger:notice(?msg,?domain),
    try_match_file(Log,
                   escape(Got3)++"FORMATTER ERROR: bad return value",
                   5000),

    %% Check that handler is still alive and was never dead
    Pid = whereis(h_proc_name()),
    H = logger:get_handler_ids(),

    ok.

formatter_fail(cleanup,_Config) ->
    logger:remove_handler(?MODULE).

config_fail(_Config) ->
    {error,{handler_not_added,{invalid_config,logger_std_h,#{bad:=bad}}}} =
        logger:add_handler(?MODULE,logger_std_h,
                           #{config => #{bad => bad},
                             filter_default=>log,
                             formatter=>{?MODULE,self()}}),
    {error,{handler_not_added,{invalid_config,logger_std_h,
                               #{restart_type:=bad}}}} =
        logger:add_handler(?MODULE,logger_std_h,
                           #{config => #{restart_type => bad},
                             filter_default=>log,
                             formatter=>{?MODULE,self()}}),
    {error,{handler_not_added,{invalid_olp_levels,#{drop_mode_qlen:=1}}}} =
        logger:add_handler(?MODULE,logger_std_h,
                           #{config => #{drop_mode_qlen=>1}}),
    {error,{handler_not_added,{invalid_olp_levels,#{sync_mode_qlen:=43,
                                                    drop_mode_qlen:=42}}}} =
        logger:add_handler(?MODULE,logger_std_h,
                           #{config => #{sync_mode_qlen=>43,
                                         drop_mode_qlen=>42}}),
    {error,{handler_not_added,{invalid_olp_levels,#{drop_mode_qlen:=43,
                                                    flush_qlen:=42}}}} =
        logger:add_handler(?MODULE,logger_std_h,
                           #{config => #{drop_mode_qlen=>43,
                                         flush_qlen=>42}}),

    ok = logger:add_handler(?MODULE,logger_std_h,
                            #{filter_default=>log,
                              formatter=>{?MODULE,self()}}),
    {error,{illegal_config_change,logger_std_h,#{type:=_},#{type:=_}}} =
        logger:set_handler_config(?MODULE,config,
                                  #{type=>{file,"file"}}),

    {error,{invalid_olp_levels,_}} =
        logger:set_handler_config(?MODULE,config,
                                  #{sync_mode_qlen=>100,
                                    flush_qlen=>99}),
    {error,{invalid_config,logger_std_h,#{filesync_rep_int:=2000}}} =
        logger:set_handler_config(?MODULE, config,
                                  #{filesync_rep_int => 2000}),

    %% Read-only fields may (accidentially) be included in the change,
    %% but it won't take effect
    {ok,C} = logger:get_handler_config(?MODULE),
    ok = logger:set_handler_config(?MODULE,config,#{olp=>dummyvalue}),
    {ok,C} = logger:get_handler_config(?MODULE),

    ok.

config_fail(cleanup,_Config) ->
    logger:remove_handler(?MODULE).

crash_std_h_to_file(Config) ->
    Dir = ?config(priv_dir,Config),
    Log = filename:join(Dir,lists:concat([?MODULE,"_",?FUNCTION_NAME,".log"])),
    crash_std_h(Config,?FUNCTION_NAME,
                [{handler,default,logger_std_h,
                  #{ config => #{ type => {file, Log} }}}],
                file, Log).
crash_std_h_to_file(cleanup,_Config) ->
    crash_std_h(cleanup).

crash_std_h_to_disk_log(Config) ->
    Dir = ?config(priv_dir,Config),
    Log = filename:join(Dir,lists:concat([?MODULE,"_",?FUNCTION_NAME,".log"])),
    crash_std_h(Config,?FUNCTION_NAME,
                [{handler,default,logger_disk_log_h,
                  #{ config => #{ file => Log }}}],
                disk_log,Log).
crash_std_h_to_disk_log(cleanup,_Config) ->
    crash_std_h(cleanup).

crash_std_h(Config,Func,Var,Type,Log) ->
    Dir = ?config(priv_dir,Config),
    SysConfig = filename:join(Dir,lists:concat([?MODULE,"_",Func,".config"])),
    ok = file:write_file(SysConfig, io_lib:format("[{kernel,[{logger,~p}]}].",[Var])),
    Pa = filename:dirname(code:which(?MODULE)),
    Name = lists:concat([?MODULE,"_",Func]),
    Args = lists:concat([" -config ",filename:rootname(SysConfig)," -pa ",Pa]),
    ct:pal("Starting ~p with ~tp", [Name,Args]),
    %% Start a node which prints kernel logs to the destination specified by Type
    {ok,Node} = test_server:start_node(Name, peer, [{args, Args}]),
    HProcName =
        case Type of
            file -> ?name_to_reg_name(logger_std_h,?STANDARD_HANDLER);
            disk_log -> ?name_to_reg_name(logger_disk_log_h,?STANDARD_HANDLER)
        end,
    Pid = rpc:call(Node,erlang,whereis,[HProcName]),
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
    undefined = rpc:call(Node,erlang,whereis,[HProcName]),

    %% Check that file is not empty
    {ok,Bin2} = sync_and_read(Node,Type,Log),
    <<"dummy1\n">> = binary:part(Bin2,{byte_size(Bin2),-7}),
    ok.

%% Can not use rpc:call here, since the code would execute on a
%% process with group_leader on this (the calling) node, and thus
%% logger would send the log event to the logger process here instead
%% of logging it itself.
log_on_remote_node(Node,Msg) ->
    Pid = self(),
    _ = spawn_link(Node,
                   fun() -> erlang:group_leader(whereis(user),self()),
                            logger:notice(Msg),
                            Pid ! done
                   end),
    receive done -> ok end,
    ok.


crash_std_h(cleanup) ->
    Nodes = nodes(),
    [test_server:stop_node(Node) || Node <- Nodes].

sync_and_read(Node,disk_log,Log) ->
    rpc:call(Node,logger_disk_log_h,filesync,[?STANDARD_HANDLER]),
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
    {error,{badarg,{filesync,["BadType"]}}} = logger_std_h:filesync("BadType").

reconfig(Config) ->
    Dir = ?config(priv_dir,Config),
    ok = logger:add_handler(?MODULE,
                            logger_std_h,
                            #{config => #{type => standard_io},
                              filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()}}),
    #{%id := ?MODULE,
      cb_state:=#{handler_state := #{type := standard_io,
                                     file_ctrl_pid := FileCtrlPid},
                  filesync_repeat_interval := no_repeat},
      sync_mode_qlen := ?SYNC_MODE_QLEN,
      drop_mode_qlen := ?DROP_MODE_QLEN,
      flush_qlen := ?FLUSH_QLEN,
      burst_limit_enable := ?BURST_LIMIT_ENABLE,
      burst_limit_max_count := ?BURST_LIMIT_MAX_COUNT,
      burst_limit_window_time := ?BURST_LIMIT_WINDOW_TIME,
      overload_kill_enable := ?OVERLOAD_KILL_ENABLE,
      overload_kill_qlen := ?OVERLOAD_KILL_QLEN,
      overload_kill_mem_size := ?OVERLOAD_KILL_MEM_SIZE,
      overload_kill_restart_after := ?OVERLOAD_KILL_RESTART_AFTER} =
        logger_olp:info(h_proc_name()),

    {ok,
     #{config:=
           #{type := standard_io,
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
             filesync_repeat_interval := no_repeat} =
           DefaultHConf}}
        = logger:get_handler_config(?MODULE),

    ok = logger:set_handler_config(?MODULE, config,
                                   #{sync_mode_qlen => 1,
                                     drop_mode_qlen => 2,
                                     flush_qlen => 3,
                                     burst_limit_enable => false,
                                     burst_limit_max_count => 10,
                                     burst_limit_window_time => 10,
                                     overload_kill_enable => true,
                                     overload_kill_qlen => 100000,
                                     overload_kill_mem_size => 10000000,
                                     overload_kill_restart_after => infinity,
                                     filesync_repeat_interval => 5000}),
    #{%id := ?MODULE,
      cb_state := #{handler_state := #{type := standard_io,
                                       file_ctrl_pid := FileCtrlPid},
                    filesync_repeat_interval := no_repeat},
      sync_mode_qlen := 1,
      drop_mode_qlen := 2,
      flush_qlen := 3,
      burst_limit_enable := false,
      burst_limit_max_count := 10,
      burst_limit_window_time := 10,
      overload_kill_enable := true,
      overload_kill_qlen := 100000,
      overload_kill_mem_size := 10000000,
      overload_kill_restart_after := infinity} = logger_olp:info(h_proc_name()),

    {ok,#{config :=
              #{type := standard_io,
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
                filesync_repeat_interval := no_repeat} = HConf}} =
        logger:get_handler_config(?MODULE),

    ok = logger:update_handler_config(?MODULE, config,
                                      #{flush_qlen => ?FLUSH_QLEN}),
    {ok,#{config:=C1}} = logger:get_handler_config(?MODULE),
    ct:log("C1: ~p",[C1]),
    C1 = HConf#{flush_qlen => ?FLUSH_QLEN},

    ok = logger:set_handler_config(?MODULE, config, #{sync_mode_qlen => 1}),
    {ok,#{config:=C2}} = logger:get_handler_config(?MODULE),
    ct:log("C2: ~p",[C2]),
    C2 = DefaultHConf#{sync_mode_qlen => 1},

    ok = logger:set_handler_config(?MODULE, config, #{drop_mode_qlen => 100}),
    {ok,#{config:=C3}} = logger:get_handler_config(?MODULE),
    ct:log("C3: ~p",[C3]),
    C3 = DefaultHConf#{drop_mode_qlen => 100},

    ok = logger:update_handler_config(?MODULE, config, #{sync_mode_qlen => 1}),
    {ok,#{config:=C4}} = logger:get_handler_config(?MODULE),
    ct:log("C4: ~p",[C4]),
    C4 = DefaultHConf#{sync_mode_qlen => 1,
                       drop_mode_qlen => 100},

    ok = logger:remove_handler(?MODULE),

    File = filename:join(Dir,lists:concat([?FUNCTION_NAME,".log"])),
    ok = logger:add_handler(?MODULE,
                            logger_std_h,
                            #{config => #{type => {file,File}},
                              filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()}}),

    {ok,#{config:=#{filesync_repeat_interval:=FSI}=FileHConfig}} =
        logger:get_handler_config(?MODULE),
    ok = logger:update_handler_config(?MODULE,config,
                                      #{filesync_repeat_interval=>FSI+2000}),
    {ok,#{config:=C5}} = logger:get_handler_config(?MODULE),
    ct:log("C5: ~p",[C5]),
    C5 = FileHConfig#{filesync_repeat_interval=>FSI+2000},

    %% You are not allowed to actively set 'type' in runtime, since
    %% this is a write once field.
    {error, {illegal_config_change,logger_std_h,_,_}} =
        logger:set_handler_config(?MODULE,config,#{type=>standard_io}),
    {ok,#{config:=C6}} = logger:get_handler_config(?MODULE),
    ct:log("C6: ~p",[C6]),
    C6 = C5,

    %%  ... but if you don't specify 'type', then set_handler_config shall
    %%  NOT reset it to its default value
    ok = logger:set_handler_config(?MODULE,config,#{sync_mode_qlen=>1}),
    {ok,#{config:=C7}} = logger:get_handler_config(?MODULE),
    ct:log("C7: ~p",[C7]),
    C7 = FileHConfig#{sync_mode_qlen=>1},
    ok.

reconfig(cleanup, _Config) ->
    logger:remove_handler(?MODULE).


file_opts(Config) ->
    Dir = ?config(priv_dir,Config),
    Log = filename:join(Dir, lists:concat([?FUNCTION_NAME,".log"])),
    MissingOpts = [raw],
    Type1 = {file,Log,MissingOpts},
    ok = logger:add_handler(?MODULE, logger_std_h,
                            #{config => #{type => Type1}}),
    {ok,#{config:=#{type:=file,file:=Log,modes:=Modes1}}} =
        logger:get_handler_config(?MODULE),
    [append,delayed_write,raw] = lists:sort(Modes1),
    ok = logger:remove_handler(?MODULE),

    OkFileOpts = [raw,append],
    OkType = {file,Log,OkFileOpts},
    ok = logger:add_handler(?MODULE,
                            logger_std_h,
                            #{config => #{type => OkType}, % old format
                              filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()}}),

    ModOpts = [delayed_write|OkFileOpts],
    #{cb_state := #{handler_state := #{type:=file,
                                       file:=Log,
                                       modes:=ModOpts}}} =
        logger_olp:info(h_proc_name()),
    {ok,#{config := #{type:=file,
                      file:=Log,
                      modes:=ModOpts}}} = logger:get_handler_config(?MODULE),
    ok = logger:remove_handler(?MODULE),

    ok = logger:add_handler(?MODULE,
                            logger_std_h,
                            #{config => #{type => file,
                                          file => Log,
                                          modes => OkFileOpts}, % new format
                              filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()}}),

    #{cb_state := #{handler_state := #{type:=file,
                                       file:=Log,
                                       modes:=ModOpts}}} =
        logger_olp:info(h_proc_name()),
    {ok,#{config := #{type:=file,
                      file:=Log,
                      modes:=ModOpts}}} =
        logger:get_handler_config(?MODULE),
    logger:notice(M1=?msg,?domain),
    ?check(M1),
    B1 = ?bin(M1),
    try_read_file(Log, {ok,B1}, filesync_rep_int()),
    ok.
file_opts(cleanup, _Config) ->
    logger:remove_handler(?MODULE).

relative_file_path(_Config) ->
    {ok,Dir} = file:get_cwd(),
    AbsName1 = filename:join(Dir,?MODULE),
    ok = logger:add_handler(?MODULE,
                            logger_std_h,
                            #{config => #{type=>file},
                              filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()}}),
    #{cb_state := #{handler_state := #{file:=AbsName1}}} =
        logger_olp:info(h_proc_name()),
    {ok,#{config := #{file:=AbsName1}}} =
        logger:get_handler_config(?MODULE),
    ok = logger:remove_handler(?MODULE),

    RelName2 = filename:join(atom_to_list(?FUNCTION_NAME),
                             lists:concat([?FUNCTION_NAME,".log"])),
    AbsName2 = filename:join(Dir,RelName2),
    ok = logger:add_handler(?MODULE,
                            logger_std_h,
                            #{config => #{file => RelName2},
                              filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()}}),
    #{cb_state := #{handler_state := #{file:=AbsName2}}} =
        logger_olp:info(h_proc_name()),
    {ok,#{config := #{file:=AbsName2}}} =
        logger:get_handler_config(?MODULE),
    logger:notice(M1=?msg,?domain),
    ?check(M1),
    B1 = ?bin(M1),
    try_read_file(AbsName2, {ok,B1}, filesync_rep_int()),

    ok = file:set_cwd(".."),
    logger:notice(M2=?msg,?domain),
    ?check(M2),
    B20 = ?bin(M2),
    B2 = <<B1/binary,B20/binary>>,
    try_read_file(AbsName2, {ok,B2}, filesync_rep_int()),

    {error,_} = logger:update_handler_config(?MODULE,config,#{file=>RelName2}),
    ok = logger:update_handler_config(?MODULE,config,#{file=>AbsName2}),
    ok = file:set_cwd(Dir),
    ok = logger:update_handler_config(?MODULE,config,#{file=>RelName2}),
    ok.
relative_file_path(cleanup,_Config) ->
    logger:remove_handler(?MODULE).


sync(Config) ->
    Dir = ?config(priv_dir,Config),
    Log = filename:join(Dir, lists:concat([?FUNCTION_NAME,".log"])),
    Type = {file,Log},
    ok = logger:add_handler(?MODULE,
                            logger_std_h,
                            #{config => #{type => Type,
                                          file_check => 10000},
                              filter_default=>log,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,nl}}),

    %% check repeated filesync happens
    start_tracer([{logger_std_h, write_to_dev, 2},
                  {file, datasync, 1}],
                 [{logger_std_h, write_to_dev, <<"first\n">>},
                  {file,datasync}]),

    logger:notice("first", ?domain),
    %% wait for automatic filesync
    check_tracer(filesync_rep_int()*2),

    %% check that explicit filesync is only done once
    start_tracer([{logger_std_h, write_to_dev, 2},
                  {file, datasync, 1}],
                 [{logger_std_h, write_to_dev, <<"second\n">>},
                  {file,datasync},
                  {no_more,500}
                 ]),
    logger:notice("second", ?domain),
    %% do explicit sync
    logger_std_h:filesync(?MODULE),
    %% a second sync should be ignored
    logger_std_h:filesync(?MODULE),
    check_tracer(100),

    %% check that if there's no repeated filesync active,
    %% a filesync is still performed when handler goes idle
    ok = logger:update_handler_config(?MODULE, config,
                                      #{filesync_repeat_interval => no_repeat}),
    no_repeat = maps:get(filesync_repeat_interval,
                         maps:get(cb_state, logger_olp:info(h_proc_name()))),
    start_tracer([{logger_std_h, write_to_dev, 2},
                  {file, datasync, 1}],
                 [{logger_std_h, write_to_dev, <<"third\n">>},
                  {file,datasync},
                  {logger_std_h, write_to_dev, <<"fourth\n">>},
                  {file,datasync}]),
    logger:notice("third", ?domain),
    %% wait for automatic filesync
    timer:sleep(?IDLE_DETECT_TIME*2),
    logger:notice("fourth", ?domain),
    %% wait for automatic filesync
    check_tracer(?IDLE_DETECT_TIME*2),

    %% switch repeated filesync on and verify that the looping works
    SyncInt = 1000,
    WaitT = 4500,
    OneSync = {logger_h_common,handle_cast,repeated_filesync},
    %% receive 1 repeated_filesync per sec
    start_tracer([{{logger_h_common,handle_cast,2},
                   [{[repeated_filesync,'_'],[],[]}]}],
                 [OneSync || _ <- lists:seq(1, trunc(WaitT/SyncInt))]),

    ok = logger:update_handler_config(?MODULE, config,
                                      #{filesync_repeat_interval => SyncInt}),
    SyncInt = maps:get(filesync_repeat_interval,
                       maps:get(cb_state,logger_olp:info(h_proc_name()))),
    timer:sleep(WaitT),
    ok = logger:update_handler_config(?MODULE, config,
                                      #{filesync_repeat_interval => no_repeat}),
    check_tracer(100),
    ok.
sync(cleanup, _Config) ->
    dbg:stop_clear(),
    logger:remove_handler(?MODULE).

write_failure(Config) ->
    Dir = ?config(priv_dir, Config),
    File = lists:concat([?MODULE,"_",?FUNCTION_NAME,".log"]),
    Log = filename:join(Dir, File),
    Node = start_std_h_on_new_node(Config, Log),
    false = (undefined == rpc:call(Node, ets, whereis, [?TEST_HOOKS_TAB])),
    rpc:call(Node, ets, insert, [?TEST_HOOKS_TAB,{tester,self()}]),
    rpc:call(Node, ?MODULE, set_internal_log, [?MODULE,internal_log]),
    rpc:call(Node, ?MODULE, set_result, [file_write,ok]),

    ok = log_on_remote_node(Node, "Logged1"),
    rpc:call(Node, logger_std_h, filesync, [?STANDARD_HANDLER]),
    ?check_no_log,
    try_read_file(Log, {ok,<<"Logged1\n">>}, filesync_rep_int()),

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
    try_read_file(Log, {ok,<<"Logged1\nLogged2\n">>}, filesync_rep_int()),
    ok.
write_failure(cleanup, _Config) ->
    Nodes = nodes(),
    [test_server:stop_node(Node) || Node <- Nodes].

sync_failure(Config) ->
    Dir = ?config(priv_dir, Config),
    File = lists:concat([?MODULE,"_",?FUNCTION_NAME,".log"]),
    Log = filename:join(Dir, File),
    Node = start_std_h_on_new_node(Config, Log),
    false = (undefined == rpc:call(Node, ets, whereis, [?TEST_HOOKS_TAB])),
    rpc:call(Node, ets, insert, [?TEST_HOOKS_TAB,{tester,self()}]),
    rpc:call(Node, ?MODULE, set_internal_log, [?MODULE,internal_log]),
    rpc:call(Node, ?MODULE, set_result, [file_datasync,ok]),
    
    SyncInt = 500,
    ok = rpc:call(Node, logger, update_handler_config,
                  [?STANDARD_HANDLER, config,
                   #{filesync_repeat_interval => SyncInt}]),
    
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

start_std_h_on_new_node(Config, Log) ->
    {ok,_,Node} =
        logger_test_lib:setup(
          Config,
          [{logger,[{handler,default,logger_std_h,
                     #{ config => #{ type => {file,Log}}}}]}]),
    ok = rpc:call(Node,logger,set_handler_config,[?STANDARD_HANDLER,formatter,
                                                  {?MODULE,nl}]),
    Node.

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

op_switch_to_sync_file(Config) ->
    {Log,HConfig,StdHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    NumOfReqs = 500,
    NewHConfig =
        HConfig#{config => StdHConfig#{sync_mode_qlen => 2,
                                       drop_mode_qlen => NumOfReqs+1,
                                       flush_qlen => 2*NumOfReqs,
                                       burst_limit_enable => false}},
    ok = logger:update_handler_config(?MODULE, NewHConfig),
    %%    TRecvPid = start_op_trace(),
    send_burst({n,NumOfReqs}, seq, {chars,79}, notice),
    Lines = count_lines(Log),
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
    ok = file_delete(Log),
    ok.
op_switch_to_sync_file(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

op_switch_to_sync_tty(Config) ->
    {HConfig,StdHConfig} = start_handler(?MODULE, standard_io, Config),
    NumOfReqs = 500,
    NewHConfig =
        HConfig#{config => StdHConfig#{sync_mode_qlen => 3,
                                       drop_mode_qlen => NumOfReqs+1,
                                       flush_qlen => 2*NumOfReqs,
                                       burst_limit_enable => false}},
    ok = logger:update_handler_config(?MODULE, NewHConfig),
    send_burst({n,NumOfReqs}, seq, {chars,79}, notice),
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
                    HConfig#{config =>
                                 StdHConfig#{sync_mode_qlen => 1,
                                             drop_mode_qlen => 2,
                                             flush_qlen =>
                                                 Procs*NumOfReqs*Bursts,
                                             burst_limit_enable => false}},
                ok = logger:update_handler_config(?MODULE, NewHConfig),
                %% It sometimes happens that the handler gets the
                %% requests in a slow enough pace so that dropping
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
op_switch_to_drop_file(cleanup, _Config) ->
    _ = stop_handler(?MODULE).

op_switch_to_drop_tty(Config) ->
    {HConfig,StdHConfig} = start_handler(?MODULE, standard_io, Config),
    NumOfReqs = 300,
    Procs = 2,
    NewHConfig =
        HConfig#{config => StdHConfig#{sync_mode_qlen => 1,
                                       drop_mode_qlen => 2,
                                       flush_qlen =>
                                           Procs*NumOfReqs+1,
                                       burst_limit_enable => false}},
    ok = logger:update_handler_config(?MODULE, NewHConfig),
    send_burst({n,NumOfReqs}, {spawn,Procs,0}, {chars,79}, notice),
    ok.
op_switch_to_drop_tty(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

op_switch_to_flush_file() ->
    [{timetrap,{minutes,5}}].
op_switch_to_flush_file(Config) ->
    Test =
        fun() ->
                {Log,HConfig,StdHConfig} =
                    start_handler(?MODULE, ?FUNCTION_NAME, Config),

                %% NOTE: it's important that both async and sync
                %% requests have been queued when the flush happens
                %% (verify with coverage of flush_log_requests/2)

                NewHConfig =
                    HConfig#{config =>
                                 StdHConfig#{sync_mode_qlen => 2,
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
                ok = stop_handler(?MODULE),
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
op_switch_to_flush_file(cleanup, _Config) ->
    _ = stop_handler(?MODULE).

op_switch_to_flush_tty() ->
    [{timetrap,{minutes,5}}].
op_switch_to_flush_tty(Config) ->
    {HConfig,StdHConfig} = start_handler(?MODULE, standard_io, Config),

    %% it's important that both async and sync requests have been queued
    %% when the flush happens (verify with coverage of flush_log_requests/2)

    NewHConfig =
        HConfig#{config => StdHConfig#{sync_mode_qlen => 2,
                                       %% disable drop mode
                                       drop_mode_qlen => 100,
                                       flush_qlen => 100,
                                       burst_limit_enable => false}},
    ok = logger:update_handler_config(?MODULE, NewHConfig),
    NumOfReqs = 1000,
    Procs = 100,
    send_burst({n,NumOfReqs}, {spawn,Procs,0}, {chars,79}, notice),
    ok.
op_switch_to_flush_tty(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

limit_burst_disabled(Config) ->
    {Log,HConfig,StdHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    NewHConfig =
        HConfig#{config => StdHConfig#{burst_limit_enable => false,
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
    {Log,HConfig,StdHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    ReqLimit = 10,
    NewHConfig =
        HConfig#{config => StdHConfig#{burst_limit_enable => true,
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
    {Log,HConfig,StdHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    ReqLimit = 10,
    BurstTWin = 1000,
    NewHConfig =
        HConfig#{config => StdHConfig#{burst_limit_enable => true,
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
    {Log,HConfig,StdHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
     NewHConfig =
        HConfig#{config=>StdHConfig#{overload_kill_enable=>false,
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
    {Log,HConfig,StdHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    Pid0 = whereis(h_proc_name()),
    {_,Mem0} = process_info(Pid0, memory),
    RestartAfter = ?OVERLOAD_KILL_RESTART_AFTER,
     NewHConfig =
        HConfig#{config=>StdHConfig#{overload_kill_enable=>true,
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

%% choke the standard handler on remote node to verify the termination
%% works as expected    
qlen_kill_std(_Config) ->
    %%! HERE
    %% Dir = ?config(priv_dir, Config),
    %% File = lists:concat([?MODULE,"_",?FUNCTION_NAME,".log"]),
    %% Log = filename:join(Dir, File),
    %% Node = start_std_h_on_new_node(Config, ?FUNCTION_NAME, Log),
    %% ok = rpc:call(Node, logger, update_handler_config,
    %%               [?STANDARD_HANDLER, config,
    %%                #{overload_kill_enable=>true,
    %%                  overload_kill_qlen=>10,
    %%                  overload_kill_mem_size=>100000}]),
    {skip,"Not done yet"}.

mem_kill_new(Config) ->
    {Log,HConfig,StdHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
    Pid0 = whereis(h_proc_name()),
    {_,Mem0} = process_info(Pid0, memory),
    RestartAfter = ?OVERLOAD_KILL_RESTART_AFTER,
     NewHConfig =
        HConfig#{config=>StdHConfig#{overload_kill_enable=>true,
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
    
%% choke the standard handler on remote node to verify the termination
%% works as expected
mem_kill_std(_Config) ->
    {skip,"Not done yet"}.

restart_after() ->
    [{timetrap,{minutes,2}}].
restart_after(Config) ->
    {Log,HConfig,StdHConfig} = start_handler(?MODULE, ?FUNCTION_NAME, Config),
     NewHConfig1 =
        HConfig#{config=>StdHConfig#{overload_kill_enable=>true,
                                     overload_kill_qlen=>10,
                                     overload_kill_restart_after=>infinity}},
    ok = logger:update_handler_config(?MODULE, NewHConfig1),
    MRef1 = erlang:monitor(process, whereis(h_proc_name())),
    %% kill handler
    send_burst({n,100}, {spawn,4,0}, {chars,79}, notice),
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
        HConfig#{config=>StdHConfig#{overload_kill_enable=>true,
                                     overload_kill_qlen=>10,
                                     overload_kill_restart_after=>RestartAfter}},
    ok = logger:update_handler_config(?MODULE, NewHConfig2),
    Pid0 = whereis(h_proc_name()),
    MRef2 = erlang:monitor(process, Pid0),
    %% kill handler
    send_burst({n,100}, {spawn,4,0}, {chars,79}, notice),
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
    [{timetrap,{minutes,3}}].
handler_requests_under_load(Config) ->
    {Log,HConfig,StdHConfig} =
        start_handler(?MODULE, ?FUNCTION_NAME, Config),
    NewHConfig =
        HConfig#{config => StdHConfig#{sync_mode_qlen => 2,
                                       drop_mode_qlen => 1000,
                                       flush_qlen => 2000,
                                       burst_limit_enable => false}},
    ok = logger:update_handler_config(?MODULE, NewHConfig),
    Pid = spawn_link(
            fun() -> send_requests(1,[{logger_std_h,filesync,[?MODULE],[]},
                                      {logger_olp,info,[h_proc_name()],[]},
                                      {logger_olp,reset,[h_proc_name()],[]},
                                      {logger,update_handler_config,
                                       [?MODULE, config,
                                        #{overload_kill_enable => false}],
                                       []}])
            end),
    Sent = send_burst({t,10000}, seq, {chars,79}, notice),
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

recreate_deleted_log(Config) ->
    {Log,_HConfig,_StdHConfig} =
        start_handler(?MODULE, ?FUNCTION_NAME, Config),
    logger:notice("first",?domain),
    logger_std_h:filesync(?MODULE),
    ok = file:rename(Log,Log++".old"),
    logger:notice("second",?domain),
    logger_std_h:filesync(?MODULE),
    {ok,<<"first\n">>} = file:read_file(Log++".old"),
    {ok,<<"second\n">>} = file:read_file(Log),
    ok.
recreate_deleted_log(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

reopen_changed_log(Config) ->
    {Log,_HConfig,_StdHConfig} =
        start_handler(?MODULE, ?FUNCTION_NAME, Config),
    logger:notice("first",?domain),
    logger_std_h:filesync(?MODULE),
    ok = file:rename(Log,Log++".old"),
    ok = file:write_file(Log,""),
    logger:notice("second",?domain),
    logger_std_h:filesync(?MODULE),
    {ok,<<"first\n">>} = file:read_file(Log++".old"),
    {ok,<<"second\n">>} = file:read_file(Log),
    ok.
reopen_changed_log(cleanup, _Config) ->
    ok = stop_handler(?MODULE).

rotate_size(Config) ->
    {Log,_HConfig,_StdHConfig} =
        start_handler(?MODULE, ?FUNCTION_NAME, Config),
    ok = logger:update_handler_config(?MODULE,#{config=>#{max_no_bytes=>1000,
                                                          max_no_files=>2}}),

    Str = lists:duplicate(19,$a),
    [logger:notice(Str,?domain) || _ <- lists:seq(1,50)],
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=1000}} = file:read_file_info(Log),
    {error,enoent} = file:read_file_info(Log++".0"),

    logger:notice(Str,?domain),
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=0}} = file:read_file_info(Log),
    {ok,#file_info{size=1020}} = file:read_file_info(Log++".0"),
    {error,enoent} = file:read_file_info(Log++".1"),

    [logger:notice(Str,?domain) || _ <- lists:seq(1,51)],
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=0}} = file:read_file_info(Log),
    {ok,#file_info{size=1020}} = file:read_file_info(Log++".0"),
    {ok,#file_info{size=1020}} = file:read_file_info(Log++".1"),
    {error,enoent} = file:read_file_info(Log++".2"),

    [logger:notice(Str,?domain) || _ <- lists:seq(1,50)],
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=1000}} = file:read_file_info(Log),
    {ok,#file_info{size=1020}} = file:read_file_info(Log++".0"),
    {ok,#file_info{size=1020}} = file:read_file_info(Log++".1"),
    {error,enoent} = file:read_file_info(Log++".2"),

    logger:notice("bbbb",?domain),
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=0}} = file:read_file_info(Log),
    {ok,#file_info{size=1005}} = file:read_file_info(Log++".0"),
    {ok,#file_info{size=1020}} = file:read_file_info(Log++".1"),
    {error,enoent} = file:read_file_info(Log++".2"),

    ok.
rotate_size(cleanup,_Config) ->
    ok = stop_handler(?MODULE).

rotate_size_compressed(Config) ->
    {Log,_HConfig,_StdHConfig} =
        start_handler(?MODULE, ?FUNCTION_NAME, Config),
    ok = logger:update_handler_config(?MODULE,
                                      #{config=>#{max_no_bytes=>1000,
                                                  max_no_files=>2,
                                                  compress_on_rotate=>true}}),
    Str = lists:duplicate(19,$a),
    [logger:notice(Str,?domain) || _ <- lists:seq(1,50)],
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=1000}} = file:read_file_info(Log),
    {error,enoent} = file:read_file_info(Log++".0"),
    {error,enoent} = file:read_file_info(Log++".0.gz"),

    logger:notice(Str,?domain),
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=0}} = file:read_file_info(Log),
    {error,enoent} = file:read_file_info(Log++".0"),
    {ok,#file_info{size=35}} = file:read_file_info(Log++".0.gz"),
    {error,enoent} = file:read_file_info(Log++".1"),
    {error,enoent} = file:read_file_info(Log++".1.gz"),

    [logger:notice(Str,?domain) || _ <- lists:seq(1,51)],
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=0}} = file:read_file_info(Log),
    {error,enoent} = file:read_file_info(Log++".0"),
    {ok,#file_info{size=35}} = file:read_file_info(Log++".0.gz"),
    {error,enoent} = file:read_file_info(Log++".1"),
    {ok,#file_info{size=35}} = file:read_file_info(Log++".1.gz"),
    {error,enoent} = file:read_file_info(Log++".2"),
    {error,enoent} = file:read_file_info(Log++".2.gz"),

    [logger:notice(Str,?domain) || _ <- lists:seq(1,50)],
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=1000}} = file:read_file_info(Log),
    {error,enoent} = file:read_file_info(Log++".0"),
    {ok,#file_info{size=35}} = file:read_file_info(Log++".0.gz"),
    {error,enoent} = file:read_file_info(Log++".1"),
    {ok,#file_info{size=35}} = file:read_file_info(Log++".1.gz"),
    {error,enoent} = file:read_file_info(Log++".2"),
    {error,enoent} = file:read_file_info(Log++".2.gz"),

    logger:notice("bbbb",?domain),
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=0}} = file:read_file_info(Log),
    {error,enoent} = file:read_file_info(Log++".0"),
    {ok,#file_info{size=38}} = file:read_file_info(Log++".0.gz"),
    {error,enoent} = file:read_file_info(Log++".1"),
    {ok,#file_info{size=35}} = file:read_file_info(Log++".1.gz"),
    {error,enoent} = file:read_file_info(Log++".2"),
    {error,enoent} = file:read_file_info(Log++".2.gz"),

    ok.
rotate_size_compressed(cleanup,_Config) ->
    ok = stop_handler(?MODULE).

rotate_size_reopen(Config) ->
    {Log,_HConfig,_StdHConfig} =
        start_handler(?MODULE, ?FUNCTION_NAME, Config),
    ok = logger:update_handler_config(?MODULE,#{config=>#{max_no_bytes=>1000,
                                                          max_no_files=>2}}),

    Str = lists:duplicate(19,$a),
    [logger:notice(Str,?domain) || _ <- lists:seq(1,40)],
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=800}} = file:read_file_info(Log),

    {ok,HConfig} = logger:get_handler_config(?MODULE),
    ok = logger:remove_handler(?MODULE),
    ok = logger:add_handler(?MODULE,maps:get(module,HConfig),HConfig),
    {ok,#file_info{size=800}} = file:read_file_info(Log),

    [logger:notice(Str,?domain) || _ <- lists:seq(1,40)],
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=580}} = file:read_file_info(Log),
    {ok,#file_info{size=1020}} = file:read_file_info(Log++".0"),
    ok.
rotate_size_reopen(cleanup,_Config) ->
    ok = stop_handler(?MODULE).

%% Test that it is possible to start the handler when there
%% exists a large file that needs rotating at startup.
rotate_on_start_compressed() ->
    [{timetrap,{minutes,5}}].
rotate_on_start_compressed(Config) ->

    application:ensure_all_started(os_mon),

    case file_SUITE:disc_free(?config(priv_dir, Config)) of
        N when N >= 5 * (1 bsl 30), is_integer(N) ->
            ct:pal("Free disk: ~w KByte~n", [N]),
            Log = get_handler_log_name(rotate_on_start_compressed, Config),

            %% Write a 1 GB file to disk
            {ok, D} = file:open(Log,[write]),
            [file:write(D,<<0:(1024*1024*8)>>) || I <- lists:seq(1,1024)],
            file:close(D),

            NumOfReqs = 500,

            %% Start the handler that will compress and rotate the existing file
            ok = logger:add_handler(?MODULE,
                                    logger_std_h,
                                    #{config => #{sync_mode_qlen => 2,
                                                  drop_mode_qlen => NumOfReqs+1,
                                                  flush_qlen => 2*NumOfReqs,
                                                  burst_limit_enable => false,
                                                  max_no_bytes=>1048576,
                                                  max_no_files=>10,
                                                  compress_on_rotate=>true,
                                                  type => {file,Log}},
                                      filter_default=>stop,
                                      filters=>filter_only_this_domain(?MODULE),
                                      formatter=>{?MODULE,op}}),

            %% Wait for compression to start
            timer:sleep(50),

            %% We send a burst here in order to make sure that the
            %% compression has time to take place. The burst will
            %% trigger sync mode which means that there will be
            %% calls made to the file controller process which
            %% in turn means that when the burst is done the
            %% compression is done.
            send_burst({n,NumOfReqs}, seq, {chars,79}, notice),
            Lines = count_lines(Log),
            NumOfReqs = Lines,
            {ok,#file_info{size=1043656}} = file:read_file_info(Log++".0.gz"),
            ok;
        _ ->
            {skip,"Disk not large enough"}
    end.
rotate_on_start_compressed(cleanup,Config) ->
    application:stop(os_mon),
    application:stop(sasl),
    file:delete(get_handler_log_name(rotate_on_start_compressed, Config)),
    file:delete(get_handler_log_name(rotate_on_start_compressed, Config)++".0.gz"),
    ok = stop_handler(?MODULE).

rotation_opts(Config) ->
    {Log,_HConfig,StdHConfig} =
        start_handler(?MODULE, ?FUNCTION_NAME, Config),
    #{max_no_bytes:=infinity,
      max_no_files:=0,
      compress_on_rotate:=false} = StdHConfig,

    %% Test bad rotation config
    {error,{invalid_config,_,_}} =
        logger:update_handler_config(?MODULE,config,#{max_no_bytes=>0}),
    {error,{invalid_config,_,_}} =
        logger:update_handler_config(?MODULE,config,#{max_no_files=>infinity}),
    {error,{invalid_config,_,_}} =
        logger:update_handler_config(?MODULE,config,
                                     #{compress_on_rotate=>undefined}),


    %% Test good rotation config - start with no rotation
    Str = lists:duplicate(19,$a),
    [logger:notice(Str,?domain) || _ <- lists:seq(1,10)],
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=200}} = file:read_file_info(Log),
    [] = filelib:wildcard(Log++".*"),

    %% Turn on rotation, check that existing file is rotated since its
    %% size exceeds max_no_bytes
    ok = logger:update_handler_config(?MODULE,
                                      config,
                                      #{max_no_bytes=>100,
                                        max_no_files=>2}),
    timer:sleep(100), % give some time to execute config_changed
    {ok,#file_info{size=0}} = file:read_file_info(Log),
    Log0 = Log++".0",
    {ok,#file_info{size=200}} = file:read_file_info(Log0),
    [Log0] = filelib:wildcard(Log++".*"),

    %% Fill all logs
    [logger:notice(Str,?domain) || _ <- lists:seq(1,13)],
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=20}} = file:read_file_info(Log),
    {ok,#file_info{size=120}} = file:read_file_info(Log0),
    {ok,#file_info{size=120}} = file:read_file_info(Log++".1"),
    [_,_] = filelib:wildcard(Log++".*"),

    %% Extend size and count and check that nothing changes with existing files
    ok = logger:update_handler_config(?MODULE,
                                      config,
                                      #{max_no_bytes=>200,
                                        max_no_files=>3}),
    timer:sleep(100), % give some time to execute config_changed
    {ok,#file_info{size=20}} = file:read_file_info(Log),
    {ok,#file_info{size=120}} = file:read_file_info(Log0),
    {ok,#file_info{size=120}} = file:read_file_info(Log++".1"),
    [_,_] = filelib:wildcard(Log++".*"),

    %% Add more log events and see that extended size and count works
    [logger:notice(Str,?domain) || _ <- lists:seq(1,10)],
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=0}} = file:read_file_info(Log),
    {ok,#file_info{size=220}} = file:read_file_info(Log0),
    {ok,#file_info{size=120}} = file:read_file_info(Log++".1"),
    {ok,#file_info{size=120}} = file:read_file_info(Log++".2"),
    [_,_,_] = filelib:wildcard(Log++".*"),

    %% Reduce count and check that archive files that exceed the new
    %% count are moved
    ok = logger:update_handler_config(?MODULE,
                                      config,
                                      #{max_no_files=>1}),
    timer:sleep(100), % give some time to execute config_changed
    {ok,#file_info{size=0}} = file:read_file_info(Log),
    {ok,#file_info{size=220}} = file:read_file_info(Log0),
    [Log0] = filelib:wildcard(Log++".*"),

    %% Extend size and count again, and turn on compression. Check
    %% that archives are compressed
    ok = logger:update_handler_config(?MODULE,
                                      config,
                                      #{max_no_bytes=>100,
                                        max_no_files=>2,
                                        compress_on_rotate=>true}),
    timer:sleep(100), % give some time to execute config_changed
    {ok,#file_info{size=0}} = file:read_file_info(Log),
    Log0gz = Log0++".gz",
    {ok,#file_info{size=29}} = file:read_file_info(Log0gz),
    [Log0gz] = filelib:wildcard(Log++".*"),

    %% Fill all logs
    [logger:notice(Str,?domain) || _ <- lists:seq(1,13)],
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=20}} = file:read_file_info(Log),
    {ok,#file_info{size=29}} = file:read_file_info(Log0gz),
    {ok,#file_info{size=29}} = file:read_file_info(Log++".1.gz"),
    [_,_] = filelib:wildcard(Log++".*"),

    %% Reduce count and turn off compression. Check that archives that
    %% exceeds the new count are removed, and the rest are
    %% uncompressed.
    ok = logger:update_handler_config(?MODULE,
                                      config,
                                      #{max_no_files=>1,
                                        compress_on_rotate=>false}),
    timer:sleep(100), % give some time to execute config_changed
    {ok,#file_info{size=20}} = file:read_file_info(Log),
    {ok,#file_info{size=120}} = file:read_file_info(Log0),
    [Log0] = filelib:wildcard(Log++".*"),

    %% Check that config and handler state agree on the current rotation settings
    {ok,#{config:=#{max_no_bytes:=100,
                    max_no_files:=1,
                    compress_on_rotate:=false}}} =
         logger:get_handler_config(?MODULE),
    #{cb_state:=#{handler_state:=#{max_no_bytes:=100,
                                   max_no_files:=1,
                                   compress_on_rotate:=false}}} =
        logger_olp:info(h_proc_name()),
    ok.
rotation_opts(cleanup,_Config) ->
    ok = stop_handler(?MODULE).

rotation_opts_restart_handler(Config) ->
    {Log,_HConfig,_StdHConfig} =
        start_handler(?MODULE, ?FUNCTION_NAME, Config),
    ok = logger:update_handler_config(?MODULE,
                                      config,
                                      #{max_no_bytes=>100,
                                        max_no_files=>2}),

    %% Fill all logs
    Str = lists:duplicate(19,$a),
    [logger:notice(Str,?domain) || _ <- lists:seq(1,15)],
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=60}} = file:read_file_info(Log),
    {ok,#file_info{size=120}} = file:read_file_info(Log++".0"),
    {ok,#file_info{size=120}} = file:read_file_info(Log++".1"),
    [_,_] = filelib:wildcard(Log++".*"),

    %% Stop/start handler and turn off rotation. Check that archives are removed.
    {ok,#{config:=StdHConfig1}=HConfig1} = logger:get_handler_config(?MODULE),
    ok = logger:remove_handler(?MODULE),
    ok = logger:add_handler(
           ?MODULE,logger_std_h,
           HConfig1#{config=>StdHConfig1#{max_no_bytes=>infinity}}),
    timer:sleep(100),
    {ok,#file_info{size=60}} = file:read_file_info(Log),
    [] = filelib:wildcard(Log++".*"),

    %% Add some log events and check that file is no longer rotated.
    [logger:notice(Str,?domain) || _ <- lists:seq(1,10)],
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=260}} = file:read_file_info(Log),
    [] = filelib:wildcard(Log++".*"),

    %% Stop/start handler and trun on rotation. Check that file is rotated.
    {ok,#{config:=StdHConfig2}=HConfig2} = logger:get_handler_config(?MODULE),
    ok = logger:remove_handler(?MODULE),
    ok = logger:add_handler(
           ?MODULE,logger_std_h,
           HConfig2#{config=>StdHConfig2#{max_no_bytes=>100,
                                          max_no_files=>2}}),
    timer:sleep(100),
    {ok,#file_info{size=0}} = file:read_file_info(Log),
    {ok,#file_info{size=260}} = file:read_file_info(Log++".0"),
    [_] = filelib:wildcard(Log++".*"),

    %% Fill all logs
    [logger:notice(Str,?domain) || _ <- lists:seq(1,10)],
    logger_std_h:filesync(?MODULE),
    {ok,#file_info{size=80}} = file:read_file_info(Log),
    {ok,#file_info{size=120}} = file:read_file_info(Log++".0"),
    {ok,#file_info{size=260}} = file:read_file_info(Log++".1"),

    %% Stop/start handler, reduce count and turn on compression. Check
    %% that excess archives are removed, and the rest compressed.
    {ok,#{config:=StdHConfig3}=HConfig3} = logger:get_handler_config(?MODULE),
    ok = logger:remove_handler(?MODULE),
    ok = logger:add_handler(
           ?MODULE,logger_std_h,
           HConfig3#{config=>StdHConfig3#{max_no_bytes=>75,
                                          max_no_files=>1,
                                          compress_on_rotate=>true}}),
    timer:sleep(100),
    {ok,#file_info{size=0}} = file:read_file_info(Log),
    {ok,#file_info{size=29}} = file:read_file_info(Log++".0.gz"),
    [_] = filelib:wildcard(Log++".*"),

    %% Stop/start handler and turn off compression. Check that achives
    %% are decompressed.
    {ok,#{config:=StdHConfig4}=HConfig4} = logger:get_handler_config(?MODULE),
    ok = logger:remove_handler(?MODULE),
    ok = logger:add_handler(
           ?MODULE,logger_std_h,
           HConfig4#{config=>StdHConfig4#{compress_on_rotate=>false}}),
    timer:sleep(100),
    {ok,#file_info{size=0}} = file:read_file_info(Log),
    {ok,#file_info{size=80}} = file:read_file_info(Log++".0"),
    [_] = filelib:wildcard(Log++".*"),

    ok.
rotation_opts_restart_handler(cleanup,_Config) ->
    ok = stop_handler(?MODULE).

%%%-----------------------------------------------------------------
%%%
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
start_handler(Name, TTY, _Config) when TTY == standard_io;
                                       TTY == standard_error->
    ok = logger:add_handler(Name,
                            logger_std_h,
                            #{config => #{type => TTY},
                              filter_default=>stop,
                              filters=>filter_only_this_domain(Name),
                              formatter=>{?MODULE,op}}),
    {ok,HConfig = #{config := StdHConfig}} = logger:get_handler_config(Name),
    {HConfig,StdHConfig};

start_handler(Name, FuncName, Config) ->
    Log = get_handler_log_name(FuncName, Config),
    ct:pal("Logging to ~tp", [Log]),
    Type = {file,Log},
    _ = file_delete(Log),
    ok = logger:add_handler(Name,
                            logger_std_h,
                            #{config => #{type => Type},
                              filter_default=>stop,
                              filters=>filter_only_this_domain(Name),
                              formatter=>{?MODULE,op}}),
    {ok,HConfig = #{config := StdHConfig}} = logger:get_handler_config(Name),
    {Log,HConfig,StdHConfig}.

get_handler_log_name(FuncName, Config) ->
    Dir = ?config(priv_dir,Config),
    filename:join(Dir, lists:concat([FuncName,".log"])).

filter_only_this_domain(Name) ->
    [{remote_gl,{fun logger_filters:remote_gl/2,stop}},
     {domain,{fun logger_filters:domain/2,{log,super,[Name]}}}].

stop_handler(Name) ->
    R = logger:remove_handler(Name),
    ct:pal("Handler ~p stopped! Result: ~p", [Name,R]),
    R.

count_lines(File) ->
    wait_until_written(File, -1),
    count_lines1(File).

wait_until_written(File, Sz) ->
    timer:sleep(2000),
    case file:read_file_info(File) of
        {error,enoent} when Sz == -1 ->
            wait_until_written(File, Sz);
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
        {ok,"Handler logger_std_h_SUITE " ++_} ->
            %% Not counting handler info
            count_lines2(Dev,LC);
        {ok,_} ->
            count_lines2(Dev,LC+1);
        eof -> LC
    end.

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
    String++"\n".

add_remove_instance_nofile(Type) ->
    ok = logger:add_handler(?MODULE,logger_std_h,
                            #{config => #{type => Type},
                              filter_default=>stop,
                              filters=>?DEFAULT_HANDLER_FILTERS([?MODULE]),
                              formatter=>{?MODULE,self()}}),
    Pid = whereis(h_proc_name()),
    true = is_pid(Pid),
    group_leader(group_leader(),Pid), % to get printouts in test log
    logger:notice(M1=?msg,?domain),
    ?check(M1),
    %% check that sync doesn't do damage even if not relevant
    ok = logger_std_h:filesync(?MODULE),
    ok = logger:remove_handler(?MODULE),
    timer:sleep(500),
    undefined = whereis(h_proc_name()),
    logger:notice(?msg,?domain),
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

    {ok,_} = dbg:p(whereis(h_proc_name()), [c]),
    {ok,_} = dbg:p(self(), [c]),

    MS1 = dbg:fun2ms(fun([_]) -> return_trace() end),
    {ok,_} = dbg:tpl(logger_h_common, check_load, 1, MS1),

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

start_tracer(Trace,Expected) ->
    Pid = self(),
    FileCtrlPid = maps:get(file_ctrl_pid,
                           maps:get(handler_state,
                                    maps:get(cb_state,
                                             logger_olp:info(h_proc_name())))),
    dbg:tracer(process,{fun tracer/2,{Pid,Expected}}),
    dbg:p(whereis(h_proc_name()),[c]),
    dbg:p(FileCtrlPid,[c]),
    tpl(Trace),
    ok.

tpl([{M,F,A}|Trace]) ->
    tpl([{{M,F,A},[]}|Trace]);
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

tracer({trace,_,call,{logger_h_common,handle_cast,[Op|_]}},
       {Pid,[{Mod,Func,Op}|Expected]}) ->
    maybe_tracer_done(Pid,Expected,{Mod,Func,Op});
tracer({trace,_,call,{Mod=logger_std_h,Func=write_to_dev,[Data,_]}},
       {Pid,[{Mod,Func,Data}|Expected]}) ->
    maybe_tracer_done(Pid,Expected,{Mod,Func,Data});
tracer({trace,_,call,{Mod,Func,_}}, {Pid,[{Mod,Func}|Expected]}) ->
    maybe_tracer_done(Pid,Expected,{Mod,Func});
tracer({trace,_,call,Call}, {Pid,Expected}) ->
    ct:log("Tracer got unexpected: ~p~nExpected: ~p~n",[Call,Expected]),
    Pid ! {tracer_got_unexpected,Call,Expected},
    {Pid,Expected}.

maybe_tracer_done(Pid,[]=Expected,Got) ->
    ct:log("Tracer got: ~p~n",[Got]),
    Pid ! {tracer_done,0},
    {Pid,Expected};
maybe_tracer_done(Pid,[{no_more,T}]=Expected,Got) ->
    ct:log("Tracer got: ~p~n",[Got]),
    Pid ! {tracer_done,T},
    {Pid,Expected};
maybe_tracer_done(Pid,Expected,Got) ->
    ct:log("Tracer got: ~p~n",[Got]),
    {Pid,Expected}.

check_tracer(T) ->
    check_tracer(T,fun() -> ct:fail({timeout,tracer}) end).
check_tracer(T,TimeoutFun) ->
    receive
        {tracer_done,Delay} ->
            %% Possibly wait Delay ms to check that no unexpected
            %% traces are received
            check_tracer(Delay,fun() -> ok end);
        {tracer_got_unexpected,Got,Expected} ->
            dbg:stop_clear(),
            ct:fail({tracer_got_unexpected,Got,Expected})
    after T ->
            dbg:stop_clear(),
            TimeoutFun()
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
    ?name_to_reg_name(logger_std_h,Name).

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

filesync_rep_int() ->
    case (fun() -> is_atom(?FILESYNC_REPEAT_INTERVAL) end)() of
        true  -> 5500;
        false -> ?FILESYNC_REPEAT_INTERVAL + 500
    end.


file_delete(Log) ->
   file:delete(Log).
