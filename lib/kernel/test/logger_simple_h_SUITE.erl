%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2024. All Rights Reserved.
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
-module(logger_simple_h_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/src/logger_internal.hrl").

-import(logger_test_lib, [setup/2, log/3, sync_and_read/3]).

-define(check_no_log,[] = test_server:messages_get()).
-define(check(Expected),
        receive {log,Expected} ->
                [] = test_server:messages_get()
        after 1000 ->
                ct:fail({report_not_received,
                         {line,?LINE},
                         {expected,Expected},
                         {got,test_server:messages_get()}})
        end).

-define(str,"Log from "++atom_to_list(?FUNCTION_NAME)++
            ":"++integer_to_list(?LINE)).
-define(map_rep,#{function=>?FUNCTION_NAME, line=>?LINE}).
-define(keyval_rep,[{function,?FUNCTION_NAME}, {line,?LINE}]).

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Hs0 = logger:get_handler_config(),
    Hs = lists:keydelete(cth_log_redirect,1,Hs0),
    [ok = logger:remove_handler(Id) || {Id,_,_} <- Hs],
    Env = [{App,Key,application:get_env(App,Key)} ||
              {App,Key} <- [{kernel,logger_level}]],
    [{env,Env},{logger,Hs}|Config].

end_per_suite(Config) ->
    [application:set_env(App,Key,Val) || {App,Key,Val} <- ?config(env,Config)],
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
    ok.

groups() ->
    [].

all() -> 
    [start_stop,
     start_debug,
     start_crash,
     replace_default,
     replace_file,
     replace_disk_log
    ].

start_stop(_Config) ->
    undefined = whereis(logger_simple_h),
    register(logger_simple_h,self()),
    {error,_} = logger:add_handler(simple,
                                   logger_simple_h,
                                   #{filter_default=>log}),    
    unregister(logger_simple_h),
    ok = logger:add_handler(simple,logger_simple_h,#{filter_default=>log}),
    Pid = whereis(logger_simple_h),
    true = is_pid(Pid),
    ok = logger:remove_handler(simple),
    false = is_pid(whereis(logger_simple_h)),
    ok.
start_stop(cleanup,_Config) ->
    logger:remove_handler(simple).

%% Test that the simple logger works when debug level is used
start_debug(Config) ->

    ConfigFile = filename:join(proplists:get_value(priv_dir, Config), "sys.config"),
    LogFile = filename:join(proplists:get_value(priv_dir, Config), "file.log"),
    SysConfig = [{kernel,[{logger,[{handler,default,logger_std_h,
                                    #{config => #{file => LogFile}}}]}]}],
    ok = file:write_file(ConfigFile, io_lib:format("~p.",[SysConfig])),

    Run =
        fun(Args) ->
                CmdLine =
                    lists:flatten(
                      [ct:get_progname(), " ", Args,
                       " -config ", filename:rootname(ConfigFile),  " -noshell -s init stop"]),
                "" = os:cmd(CmdLine),
                {ok, Bin} = file:read_file(LogFile),
                Bin
        end,

    Output = Run(""),
    LogOutput = re:replace(unicode:characters_to_binary(Output),"\r\n","\n",[global]),
    ct:log("~ts",[LogOutput]),
    nomatch = re:run(LogOutput,"^=PROGRESS REPORT====",[global,multiline]),

    InfoOutput = Run("-kernel logger_level info"),
    InfoLogOutput = re:replace(unicode:characters_to_binary(InfoOutput),"\r\n","\n",[global]),
    ct:log("~ts",[InfoLogOutput]),
    {match,InfoNumReports} = re:run(InfoLogOutput,"^=PROGRESS REPORT====",[global,multiline]),

    %% Test that more progress reports are logged for info than default
    ?assert(0 < length(InfoNumReports)),

    DebugOutput = Run("-kernel logger_level debug"),
    DebugLogOutput = re:replace(unicode:characters_to_binary(DebugOutput),"\r\n","\n",[global]),
    ct:log("~ts",[DebugLogOutput]),
    {match,DebugNumReports} = re:run(DebugLogOutput,"^=PROGRESS REPORT====",[global,multiline]),

    %% Test that more progress reports are logged for debug than info
    ?assert(length(InfoNumReports) < length(DebugNumReports)),

    ok.

%% Test that the simple logger works during startup crash
start_crash(_Config) ->

    Output = os:cmd(ct:get_progname() ++ " -user baduser"),
    ErrorOutput = re:replace(unicode:characters_to_binary(Output),"\r\n","\n",[global]),
    ct:log("~ts",[ErrorOutput]),
    {match,[_]} = re:run(ErrorOutput,"(^=SUPERVISOR REPORT====| supervisor_report *\n)",[global,multiline]),
    {match,[_, _]} = re:run(ErrorOutput," crash_report *\n",[global]),
    {match,[_]} = re:run(ErrorOutput," std_info *\n",[global]),
    {match,[[CD]]} = re:run(ErrorOutput,"\nCrash dump is being written to: (.*)\\.\\.\\.done",
                            [{capture, all_but_first, binary}, global]),
    ok = file:delete(CD),
    ok.


%% This testcase just tests that it does not crash, the default handler prints
%% to stdout which we cannot read from in a detached slave.
replace_default(Config) ->

    {ok, _, Peer, Node} = logger_test_lib:setup(Config, [{logger, [{handler, default, undefined}]}]),
    log(Node, emergency, [?str]),
    log(Node, alert, [?str,[]]),
    log(Node, error, [?map_rep]),
    log(Node, info, [?keyval_rep]),
    log(Node, info, [?keyval_rep++[not_key_val]]),
    rpc:call(Node, error_logger, error_report, [some_type,?map_rep]),
    rpc:call(Node, error_logger, warning_report, ["some_type",?map_rep]),
    log(Node, critical, [?str,[?keyval_rep]]),
    log(Node, notice, [["fake",string,"line:",?LINE]]),

    ok = rpc:call(Node, logger, add_handlers, [kernel]),

    ok = peer:stop(Peer).

replace_file(Config) ->

    {ok, _, Peer, Node} = logger_test_lib:setup(Config, [{logger, [{handler, default, undefined}]}]),
    log(Node, emergency, [M1=?str]),
    log(Node, alert, [M2=?str,[]]),
    log(Node, error, [?map_rep]),
    log(Node, warning, [?keyval_rep]),
    log(Node, warning, [?keyval_rep++[not_key_val]]),
    log(Node, critical, [?str,[?keyval_rep]]),
    log(Node, notice, [["fake",string,"line:",?LINE]]),

    File = filename:join(proplists:get_value(priv_dir,Config),
                         atom_to_list(?FUNCTION_NAME)++".log"),

    ok = rpc:call(Node, logger, add_handlers,
                  [[{handler, default, logger_std_h,
                     #{ config => #{ type => {file, File} },
                        formatter => {?DEFAULT_FORMATTER,?DEFAULT_FORMAT_CONFIG}}}]]),

    {ok,Bin} = sync_and_read(Node, file, File),
    Lines = [unicode:characters_to_list(L) ||
                L <- binary:split(Bin,<<"\n">>,[global,trim])],
    ["=EMERGENCY REPORT===="++_,
     M1,
     "=ALERT REPORT===="++_,
     M2,
     "=ERROR REPORT===="++_,
     _,
     _,
     "=WARNING REPORT===="++_,
     _,
     _,
     "=WARNING REPORT===="++_,
     _,
     _,
     _,
     "=CRITICAL REPORT===="++_,
     _,
     _,
     "=NOTICE REPORT===="++_,
     _
    ] = Lines,
    ok = peer:stop(Peer).

replace_disk_log(Config) ->

    {ok, _, Peer, Node} = logger_test_lib:setup(Config, [{logger, [{handler, default, undefined}]}]),
    log(Node, emergency, [M1=?str]),
    log(Node, alert, [M2=?str,[]]),
    log(Node, error, [?map_rep]),
    log(Node, warning, [?keyval_rep]),
    log(Node, warning, [?keyval_rep++[not_key_val]]),
    log(Node, critical, [?str,[?keyval_rep]]),
    log(Node, notice, [["fake",string,"line:",?LINE]]),

    File = filename:join(proplists:get_value(priv_dir,Config),
                         atom_to_list(?FUNCTION_NAME)++".log"),

    ok = rpc:call(Node, logger, add_handlers,
                  [[{handler, default, logger_disk_log_h,
                     #{ config => #{ file => File },
                        formatter => {?DEFAULT_FORMATTER,?DEFAULT_FORMAT_CONFIG}}}]]),
    {ok,Bin} = sync_and_read(Node, disk_log, File),
    Lines = [unicode:characters_to_list(L) ||
                L <- binary:split(Bin,<<"\n">>,[global,trim])],
    ["=EMERGENCY REPORT===="++_,
     M1,
     "=ALERT REPORT===="++_,
     M2,
     "=ERROR REPORT===="++_,
     _,
     _,
     "=WARNING REPORT===="++_,
     _,
     _,
     "=WARNING REPORT===="++_,
     _,
     _,
     _,
     "=CRITICAL REPORT===="++_,
     _,
     _,
     "=NOTICE REPORT===="++_,
     _
    ] = Lines,
    ok = peer:stop(Peer).
