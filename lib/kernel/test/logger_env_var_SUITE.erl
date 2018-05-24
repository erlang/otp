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
-module(logger_env_var_SUITE).

-compile(export_all).

-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/src/logger_internal.hrl").

-import(logger_test_lib,[setup/2,log/3,sync_and_read/3]).

suite() ->
    [{timetrap,{seconds,60}},
     {ct_hooks,[logger_test_lib]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

groups() ->
    [{error_logger,[],[error_logger_tty,
                       error_logger_tty_sasl_compatible,
                       error_logger_false,
                       error_logger_false_progress,
                       error_logger_false_sasl_compatible,
                       error_logger_silent,
                       error_logger_silent_sasl_compatible,
                       error_logger_file]},
     {logger,[],[logger_file,
                 logger_file_sasl_compatible,
                 logger_file_log_progress,
                 logger_file_no_filter,
                 logger_file_no_filter_level,
                 logger_file_formatter,
                 logger_filters,
                 logger_filters_stop,
                 logger_module_level,
                 logger_disk_log,
                 logger_disk_log_formatter,
                 logger_undefined,
                 logger_many_handlers_default_first,
                 logger_many_handlers_default_last,
                 logger_many_handlers_default_last_broken_filter
                ]},
     {bad,[],[bad_error_logger,
              bad_level,
              bad_sasl_compatibility,
              bad_progress]}].

all() ->
    [default,
     default_sasl_compatible,
     sasl_compatible_false,
     sasl_compatible_false_no_progress,
     sasl_compatible,
     {group,bad},
     {group,error_logger},
     {group,logger}
    ].

default(Config) ->
    {ok,#{handlers:=Hs},_Node} = setup(Config,[]),
    {?STANDARD_HANDLER,logger_std_h,StdC} = lists:keyfind(?STANDARD_HANDLER,1,Hs),
    info = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[beam,erlang,otp,sasl]}}} =
        lists:keyfind(domain,1,StdFilters),
    true = lists:keymember(stop_progress,1,StdFilters),
    false = lists:keymember(simple,1,Hs),
    false = lists:keymember(sasl,1,Hs),
    ok.

default_sasl_compatible(Config) ->
    {ok,#{handlers:=Hs},_Node} = setup(Config,
                                       [{logger_sasl_compatible,true}]),
    {?STANDARD_HANDLER,logger_std_h,StdC} = lists:keyfind(?STANDARD_HANDLER,1,Hs),
    info = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[beam,erlang,otp]}}} =
        lists:keyfind(domain,1,StdFilters),
    false = lists:keymember(stop_progress,1,StdFilters),
    false = lists:keymember(simple,1,Hs),
    true = lists:keymember(sasl,1,Hs),
    ok.

error_logger_tty(Config) ->
    {ok,#{handlers:=Hs},_Node} = setup(Config,[{error_logger,tty}]),
    {?STANDARD_HANDLER,logger_std_h,StdC} = lists:keyfind(?STANDARD_HANDLER,1,Hs),
    info = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[beam,erlang,otp,sasl]}}} =
        lists:keyfind(domain,1,StdFilters),
    true = lists:keymember(stop_progress,1,StdFilters),
    false = lists:keymember(simple,1,Hs),
    false = lists:keymember(sasl,1,Hs),
    ok.

error_logger_tty_sasl_compatible(Config) ->
    {ok,#{handlers:=Hs},_Node} = setup(Config,
                                       [{error_logger,tty},
                                        {logger_sasl_compatible,true}]),
    {?STANDARD_HANDLER,logger_std_h,StdC} = lists:keyfind(?STANDARD_HANDLER,1,Hs),
    info = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[beam,erlang,otp]}}} =
        lists:keyfind(domain,1,StdFilters),
    false = lists:keymember(stop_progress,1,StdFilters),
    false = lists:keymember(simple,1,Hs),
    true = lists:keymember(sasl,1,Hs),
    ok.

error_logger_false(Config) ->
    {ok,#{handlers:=Hs,logger:=L},_Node} =
        setup(Config,
              [{error_logger,false},
               {logger_level,notice}]),
    false = lists:keymember(?STANDARD_HANDLER,1,Hs),
    {simple,logger_simple_h,SimpleC} = lists:keyfind(simple,1,Hs),
    info = maps:get(level,SimpleC),
    notice = maps:get(level,L),
    SimpleFilters = maps:get(filters,SimpleC),
    {domain,{_,{log,super,[beam,erlang,otp,sasl]}}} =
        lists:keyfind(domain,1,SimpleFilters),
    true = lists:keymember(stop_progress,1,SimpleFilters),
    false = lists:keymember(sasl,1,Hs),
    ok.

error_logger_false_progress(Config) ->
    {ok,#{handlers:=Hs,logger:=L},_Node} =
        setup(Config,
              [{error_logger,false},
               {logger_level,notice},
               {logger_progress_reports,log}]),
    false = lists:keymember(?STANDARD_HANDLER,1,Hs),
    {simple,logger_simple_h,SimpleC} = lists:keyfind(simple,1,Hs),
    info = maps:get(level,SimpleC),
    notice = maps:get(level,L),
    SimpleFilters = maps:get(filters,SimpleC),
    {domain,{_,{log,super,[beam,erlang,otp,sasl]}}} =
        lists:keyfind(domain,1,SimpleFilters),
    false = lists:keymember(stop_progress,1,SimpleFilters),
    false = lists:keymember(sasl,1,Hs),
    ok.

error_logger_false_sasl_compatible(Config) ->
    {ok,#{handlers:=Hs,logger:=L},_Node} =
        setup(Config,
              [{error_logger,false},
               {logger_level,notice},
               {logger_sasl_compatible,true}]),
    false = lists:keymember(?STANDARD_HANDLER,1,Hs),
    {simple,logger_simple_h,SimpleC} = lists:keyfind(simple,1,Hs),
    info = maps:get(level,SimpleC),
    notice = maps:get(level,L),
    SimpleFilters = maps:get(filters,SimpleC),
    {domain,{_,{log,super,[beam,erlang,otp]}}} =
        lists:keyfind(domain,1,SimpleFilters),
    false = lists:keymember(stop_progress,1,SimpleFilters),
    true = lists:keymember(sasl,1,Hs),
    ok.

error_logger_silent(Config) ->
    {ok,#{handlers:=Hs},_Node} = setup(Config,
                                       [{error_logger,silent}]),
    false = lists:keymember(?STANDARD_HANDLER,1,Hs),
    false = lists:keymember(simple,1,Hs),
    false = lists:keymember(sasl,1,Hs),
    ok.

error_logger_silent_sasl_compatible(Config) ->
    {ok,#{handlers:=Hs},_Node} = setup(Config,
                                       [{error_logger,silent},
                                        {logger_sasl_compatible,true}]),
    false = lists:keymember(?STANDARD_HANDLER,1,Hs),
    false = lists:keymember(simple,1,Hs),
    true = lists:keymember(sasl,1,Hs),
    ok.


error_logger_file(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,_Hs,Node} = setup(Config,
                          [{error_logger,{file,Log}}]),
    check_default_log(Node,Log,
                      file,% dest
                      0),% progress in std logger
    ok.


logger_file(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs},Node}
        = setup(Config,
                [{logger,
                  [{handler,?STANDARD_HANDLER,logger_std_h,
                    #{logger_std_h=>#{type=>{file,Log}}}}]}]),
    check_default_log(Node,Log,
                      file,% dest
                      0),% progress in std logger

    {?STANDARD_HANDLER,logger_std_h,StdC} = lists:keyfind(?STANDARD_HANDLER,1,Hs),
    info = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[beam,erlang,otp,sasl]}}} =
        lists:keyfind(domain,1,StdFilters),
    true = lists:keymember(stop_progress,1,StdFilters),
    false = lists:keymember(simple,1,Hs),
    false = lists:keymember(sasl,1,Hs),

    ok.

logger_file_sasl_compatible(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs},Node}
        = setup(Config,
                [{logger_sasl_compatible,true},
                 {logger,
                  [{handler,?STANDARD_HANDLER,logger_std_h,
                    #{logger_std_h=>#{type=>{file,Log}}}}]}]),
    check_default_log(Node,Log,
                      file,% dest
                      0),% progress in std logger

    {?STANDARD_HANDLER,logger_std_h,StdC} = lists:keyfind(?STANDARD_HANDLER,1,Hs),
    info = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[beam,erlang,otp]}}} =
        lists:keyfind(domain,1,StdFilters),
    false = lists:keymember(stop_progress,1,StdFilters),
    false = lists:keymember(simple,1,Hs),
    true = lists:keymember(sasl,1,Hs),

    ok.

logger_file_log_progress(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs},Node}
        = setup(Config,
                [{logger_progress_reports,log},
                 {logger,
                  [{handler,?STANDARD_HANDLER,logger_std_h,
                    #{logger_std_h=>#{type=>{file,Log}}}}]}]),
    check_default_log(Node,Log,
                      file,% dest
                      6),% progress in std logger

    {?STANDARD_HANDLER,logger_std_h,StdC} = lists:keyfind(?STANDARD_HANDLER,1,Hs),
    info = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[beam,erlang,otp,sasl]}}} =
        lists:keyfind(domain,1,StdFilters),
    false = lists:keymember(stop_progress,1,StdFilters),
    false = lists:keymember(simple,1,Hs),
    false = lists:keymember(sasl,1,Hs),

    ok.

logger_file_no_filter(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs},Node}
        = setup(Config,
                [{logger,
                  [{handler,?STANDARD_HANDLER,logger_std_h,
                    #{filter_default=>log,filters=>[],
                      logger_std_h=>#{type=>{file,Log}}}}]}]),
    check_default_log(Node,Log,
                      file,% dest
                      6),% progress in std logger

    {?STANDARD_HANDLER,logger_std_h,StdC} = lists:keyfind(?STANDARD_HANDLER,1,Hs),
    info = maps:get(level,StdC),
    [] = maps:get(filters,StdC),
    false = lists:keymember(simple,1,Hs),
    false = lists:keymember(sasl,1,Hs),

    ok.

logger_file_no_filter_level(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs},Node}
        = setup(Config,
                [{logger,
                  [{handler,?STANDARD_HANDLER,logger_std_h,
                    #{filters=>[],level=>error,
                      logger_std_h=>#{type=>{file,Log}}}}]}]),
    check_default_log(Node,Log,
                      file,% dest
                      0,% progress in std logger
                      error),% level

    {?STANDARD_HANDLER,logger_std_h,StdC} = lists:keyfind(?STANDARD_HANDLER,1,Hs),
    error = maps:get(level,StdC),
    [] = maps:get(filters,StdC),
    false = lists:keymember(simple,1,Hs),
    false = lists:keymember(sasl,1,Hs),

    ok.

logger_file_formatter(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs},Node}
        = setup(Config,
                [{logger,
                  [{handler,?STANDARD_HANDLER,logger_std_h,
                    #{filters=>[],
                      formatter=>{logger_formatter,#{}},
                      logger_std_h=>#{type=>{file,Log}}}}]}]),
    check_single_log(Node,Log,
                     file,% dest
                     6),% progress in std logger

    {?STANDARD_HANDLER,logger_std_h,StdC} = lists:keyfind(?STANDARD_HANDLER,1,Hs),
    info = maps:get(level,StdC),
    [] = maps:get(filters,StdC),
    false = lists:keymember(simple,1,Hs),
    false = lists:keymember(sasl,1,Hs),

    ok.

logger_filters(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs,logger:=Logger},Node}
        = setup(Config,
                [{logger_progress_reports,log},
                 {logger,
                  [{handler,?STANDARD_HANDLER,logger_std_h,
                    #{logger_std_h=>#{type=>{file,Log}}}},
                   {filters,log,[{stop_progress,{fun logger_filters:progress/2,stop}}]}
                  ]}]),
    check_default_log(Node,Log,
                      file,% dest
                      0),% progress in std logger

    {?STANDARD_HANDLER,logger_std_h,StdC} = lists:keyfind(?STANDARD_HANDLER,1,Hs),
    info = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[beam,erlang,otp,sasl]}}} =
        lists:keyfind(domain,1,StdFilters),
    false = lists:keymember(stop_progress,1,StdFilters),
    false = lists:keymember(simple,1,Hs),
    false = lists:keymember(sasl,1,Hs),
    LoggerFilters = maps:get(filters,Logger),
    true = lists:keymember(stop_progress,1,LoggerFilters),

    ok.

logger_filters_stop(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs,logger:=Logger},Node}
        = setup(Config,
                [{logger_progress_reports,log},
                 {logger,
                  [{handler,?STANDARD_HANDLER,logger_std_h,
                    #{filters=>[],
                      logger_std_h=>#{type=>{file,Log}}}},
                   {filters,stop,[{log_error,{fun logger_filters:level/2,{log,gt,info}}}]}
                  ]}]),
    check_default_log(Node,Log,
                      file,% dest
                      0,
                      notice),% progress in std logger

    {?STANDARD_HANDLER,logger_std_h,StdC} = lists:keyfind(?STANDARD_HANDLER,1,Hs),
    info = maps:get(level,StdC),
    [] = maps:get(filters,StdC),
    false = lists:keymember(simple,1,Hs),
    false = lists:keymember(sasl,1,Hs),
    LoggerFilters = maps:get(filters,Logger),
    true = lists:keymember(log_error,1,LoggerFilters),

    ok.

logger_module_level(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs,module_levels:=ModuleLevels},Node}
        = setup(Config,
                [{logger_progress_reports,log},
                 {logger,
                  [{handler,?STANDARD_HANDLER,logger_std_h,
                    #{logger_std_h=>#{type=>{file,Log}}}},
                   {module_level,error,[supervisor]}
                  ]}]),
    check_default_log(Node,Log,
                      file,% dest
                      3),% progress in std logger

    {?STANDARD_HANDLER,logger_std_h,StdC} = lists:keyfind(?STANDARD_HANDLER,1,Hs),
    info = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[beam,erlang,otp,sasl]}}} =
        lists:keyfind(domain,1,StdFilters),
    false = lists:keymember(stop_progress,1,StdFilters),
    false = lists:keymember(simple,1,Hs),
    false = lists:keymember(sasl,1,Hs),
    [{supervisor,error}] = ModuleLevels,
    ok.

logger_disk_log(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs},Node}
        = setup(Config,
                [{logger,
                  [{handler,?STANDARD_HANDLER,logger_disk_log_h,
                    #{disk_log_opts=>#{file=>Log}}}]}]),
    check_default_log(Node,Log,
                      disk_log,% dest
                      0),% progress in std logger

    {?STANDARD_HANDLER,logger_disk_log_h,StdC} = lists:keyfind(?STANDARD_HANDLER,1,Hs),
    info = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[beam,erlang,otp,sasl]}}} =
        lists:keyfind(domain,1,StdFilters),
    true = lists:keymember(stop_progress,1,StdFilters),
    false = lists:keymember(simple,1,Hs),
    false = lists:keymember(sasl,1,Hs),

    ok.

logger_disk_log_formatter(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs},Node}
        = setup(Config,
                [{logger,
                  [{handler,?STANDARD_HANDLER,logger_disk_log_h,
                    #{filters=>[],
                      formatter=>{logger_formatter,#{}},
                      disk_log_opts=>#{file=>Log}}}]}]),
    check_single_log(Node,Log,
                     disk_log,% dest
                     6),% progress in std logger

    {?STANDARD_HANDLER,logger_disk_log_h,StdC} = lists:keyfind(?STANDARD_HANDLER,1,Hs),
    info = maps:get(level,StdC),
    [] = maps:get(filters,StdC),
    false = lists:keymember(simple,1,Hs),
    false = lists:keymember(sasl,1,Hs),

    ok.

logger_undefined(Config) ->
    {ok,#{handlers:=Hs,logger:=L},_Node} =
        setup(Config,[{logger,[{handler,?STANDARD_HANDLER,undefined}]}]),
    false = lists:keymember(?STANDARD_HANDLER,1,Hs),
    {simple,logger_simple_h,SimpleC} = lists:keyfind(simple,1,Hs),
    info = maps:get(level,SimpleC),
    info = maps:get(level,L),
    SimpleFilters = maps:get(filters,SimpleC),
    {domain,{_,{log,super,[beam,erlang,otp,sasl]}}} =
        lists:keyfind(domain,1,SimpleFilters),
    true = lists:keymember(stop_progress,1,SimpleFilters),
    false = lists:keymember(sasl,1,Hs),
    ok.


%% Test that we can add multiple handlers with the default first
logger_many_handlers_default_first(Config) ->
    LogErr = file(Config,logger_many_handlers_default_first_error),
    LogInfo = file(Config,logger_many_handlers_default_first_info),

    logger_many_handlers(
      Config,[{logger,
               [{handler,?STANDARD_HANDLER,logger_std_h,
                 #{level=>error,
                   filters=>[],
                   formatter=>{logger_formatter,#{}},
                   logger_std_h=>#{type=>{file,LogErr}}}
                },
                {handler,info,logger_std_h,
                 #{level=>info,
                   filters=>[{level,{fun logger_filters:level/2,{stop,gteq,error}}}],
                   logger_std_h=>#{type=>{file,LogInfo}}}
                }
               ]}], LogErr, LogInfo, 6).

%% Test that we can add multiple handlers with the default last
logger_many_handlers_default_last(Config) ->
    LogErr = file(Config,logger_many_handlers_default_last_error),
    LogInfo = file(Config,logger_many_handlers_default_last_info),
    logger_many_handlers(
      Config,[{logger,
               [{handler,info,logger_std_h,
                 #{level=>info,
                   filters=>[{level,{fun logger_filters:level/2,{stop,gteq,error}}}],
                   logger_std_h=>#{type=>{file,LogInfo}}}
                },
                {handler,?STANDARD_HANDLER,logger_std_h,
                 #{level=>error,
                   filters=>[],
                   formatter=>{logger_formatter,#{}},
                   logger_std_h=>#{type=>{file,LogErr}}}
                }
               ]}], LogErr, LogInfo, 7).

%% Check that we can handle that an added logger has a broken filter
%% This used to cause a deadlock.
logger_many_handlers_default_last_broken_filter(Config) ->
    LogErr = file(Config,logger_many_handlers_default_first_broken_filter_error),
    LogInfo = file(Config,logger_many_handlers_default_first_broken_filter_info),

    logger_many_handlers(
      Config,[{logger,
               [{handler,info,logger_std_h,
                 #{level=>info,
                   filters=>[{broken,{fun logger_filters:level/2,broken_state}},
                             {level,{fun logger_filters:level/2,{stop,gteq,error}}}],
                   logger_std_h=>#{type=>{file,LogInfo}}}
                },
                {handler,?STANDARD_HANDLER,logger_std_h,
                 #{level=>error,
                   filters=>[],
                   formatter=>{logger_formatter,#{}},
                   logger_std_h=>#{type=>{file,LogErr}}}
                }
               ]}], LogErr, LogInfo, 7).

logger_many_handlers(Config, Env, LogErr, LogInfo, NumProgress) ->
    {ok,#{handlers:=Hs},Node} = setup(Config,Env),
    check_single_log(Node,LogErr,
                     file,% dest
                     0,% progress in std logger
                     error), % level
    ok = rpc:call(Node,logger_std_h,filesync,[info]),
    {ok, Bin} = file:read_file(LogInfo),
    ct:log("Log content:~n~s",[Bin]),
    match(Bin,<<"info:">>,NumProgress+1,info,info),
    match(Bin,<<"alert:">>,0,alert,info),

    ok.

sasl_compatible_false(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,_Hs,Node} = setup(Config,
                          [{error_logger,{file,Log}},
                           {logger_sasl_compatible,false},
                           {logger_progress_reports,log}]),
    check_default_log(Node,Log,
                      file,% dest
                      6),% progress in std logger
    ok.

sasl_compatible_false_no_progress(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,_Hs,Node} = setup(Config,
                          [{error_logger,{file,Log}},
                           {logger_sasl_compatible,false},
                           {logger_progress_reports,stop}]),
    check_default_log(Node,Log,
                      file,% dest
                      0),% progress in std logger
    ok.

sasl_compatible(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,_Hs,Node} = setup(Config,
                          [{error_logger,{file,Log}},
                           {sasl_compatible,true}]),
    check_default_log(Node,Log,
                      file,% dest
                      0),% progress in std logger
    ok.

bad_error_logger(Config) ->
    error = setup(Config,[{error_logger,baddest}]).

bad_level(Config) ->
    error = setup(Config,[{logger_level,badlevel}]).

bad_sasl_compatibility(Config) ->
    error = setup(Config,[{logger_sasl_compatible,badcomp}]).

bad_progress(Config) ->
    error = setup(Config,[{logger_progress_reports,badprogress}]).

%%%-----------------------------------------------------------------
%%% Internal
file(Config,Func) ->
    filename:join(proplists:get_value(priv_dir,Config),
                  lists:concat([Func,".log"])).

check_default_log(Node,Log,Dest,NumProgress) ->
    check_default_log(Node,Log,Dest,NumProgress,info).
check_default_log(Node,Log,Dest,NumProgress,Level) ->

    {ok,Bin1,Bin2} = check_log(Node,Log,Dest),

    match(Bin1,<<"PROGRESS REPORT">>,NumProgress,info,Level),
    match(Bin1,<<"ALERT REPORT">>,1,alert,Level),
    match(Bin1,<<"INFO REPORT">>,0,info,Level),
    match(Bin1,<<"DEBUG REPORT">>,0,debug,Level),

    match(Bin2,<<"INFO REPORT">>,1,info,Level),
    match(Bin2,<<"DEBUG REPORT">>,0,debug,Level),
    ok.

check_single_log(Node,Log,Dest,NumProgress) ->
    check_single_log(Node,Log,Dest,NumProgress,info).
check_single_log(Node,Log,Dest,NumProgress,Level) ->

    {ok,Bin1,Bin2} = check_log(Node,Log,Dest),

    match(Bin1,<<"info:">>,NumProgress,info,Level),
    match(Bin1,<<"alert:">>,1,alert,Level),
    match(Bin1,<<"debug:">>,0,debug,Level),

    match(Bin2,<<"info:">>,NumProgress+1,info,Level),
    match(Bin2,<<"debug:">>,0,debug,Level),

    ok.

check_log(Node,Log,Dest) ->

    ok = log(Node,alert,["dummy1"]),
    ok = log(Node,debug,["dummy1"]),

    %% Check that there are progress reports (supervisor and
    %% application_controller) and an error report (the call above) in
    %% the log. There should not be any info reports yet.
    {ok,Bin1} = sync_and_read(Node,Dest,Log),
    ct:log("Log content:~n~s",[Bin1]),

    %% Then stop sasl and see that the info report from
    %% application_controller is there
    ok = rpc:call(Node,application,stop,[sasl]),
    {ok,Bin2} = sync_and_read(Node,Dest,Log),
    ct:log("Log content:~n~s",[Bin2]),
    {ok,Bin1,Bin2}.

match(Bin,Pattern,0,_,_) ->
    nomatch = re:run(Bin,Pattern,[{capture,none}]);
match(Bin,Pattern,N,LogLevel,ConfLevel) ->
    case logger:compare_levels(LogLevel,ConfLevel) of
        lt -> match(Bin,Pattern,0,LogLevel,ConfLevel);
        _ ->
            {match,M} = re:run(Bin,Pattern,[{capture,all},global]),
            N = length(M)
    end.
