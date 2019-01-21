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
                 logger_many_handlers_default_last_broken_filter,
                 logger_proxy
                ]},
     {bad,[],[bad_error_logger,
              bad_level,
              bad_sasl_compatibility]}].

all() ->
    [default,
     default_sasl_compatible,
     sasl_compatible_false,
     sasl_compatible_false_no_progress,
     sasl_compatible,
     all_logger_level,
     {group,bad},
     {group,error_logger},
     {group,logger}
    ].

default(Config) ->
    {ok,#{primary:=P,handlers:=Hs,module_levels:=ML},_Node} = setup(Config,[]),
    notice = maps:get(level,P),
    #{module:=logger_std_h} = StdC = find(?STANDARD_HANDLER,Hs),
    all = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[otp,sasl]}}} = lists:keyfind(domain,1,StdFilters),
    false = exists(simple,Hs),
    false = exists(sasl,Hs),
    [] = ML,
    ok.

default_sasl_compatible(Config) ->
    {ok,#{primary:=P,handlers:=Hs,module_levels:=ML},_Node} =
        setup(Config,[{logger_sasl_compatible,true}]),
    info = maps:get(level,P),
    #{module:=logger_std_h} = StdC = find(?STANDARD_HANDLER,Hs),
    all = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[otp]}}} = lists:keyfind(domain,1,StdFilters),
    false = exists(simple,Hs),
    true = exists(sasl,Hs),
    [] = ML,
    ok.

error_logger_tty(Config) ->
    {ok,#{primary:=P,handlers:=Hs,module_levels:=ML},_Node} =
        setup(Config,[{error_logger,tty}]),
    notice = maps:get(level,P),
    #{module:=logger_std_h} = StdC = find(?STANDARD_HANDLER,Hs),
    all = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[otp,sasl]}}} = lists:keyfind(domain,1,StdFilters),
    false = exists(simple,Hs),
    false = exists(sasl,Hs),
    [] = ML,
    ok.

error_logger_tty_sasl_compatible(Config) ->
    {ok,#{primary:=P,handlers:=Hs,module_levels:=ML},_Node} = 
        setup(Config,
              [{error_logger,tty},
               {logger_sasl_compatible,true}]),
    info = maps:get(level,P),
    #{module:=logger_std_h} = StdC = find(?STANDARD_HANDLER,Hs),
    all = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[otp]}}} = lists:keyfind(domain,1,StdFilters),
    false = exists(simple,Hs),
    true = exists(sasl,Hs),
    [] = ML,
    ok.

error_logger_false(Config) ->
    {ok,#{handlers:=Hs,primary:=P,module_levels:=ML},_Node} =
        setup(Config,
              [{error_logger,false},
               {logger_level,notice}]),
    false = exists(?STANDARD_HANDLER,Hs),
    #{module:=logger_simple_h} = SimpleC = find(simple,Hs),
    all = maps:get(level,SimpleC),
    notice = maps:get(level,P),
    SimpleFilters = maps:get(filters,SimpleC),
    {domain,{_,{log,super,[otp,sasl]}}} = lists:keyfind(domain,1,SimpleFilters),
    false = exists(sasl,Hs),
    [] = ML,
    ok.

error_logger_false_progress(Config) ->
    {ok,#{handlers:=Hs,primary:=P,module_levels:=ML},_Node} =
        setup(Config,
              [{error_logger,false},
               {logger_level,notice}]),
    false = exists(?STANDARD_HANDLER,Hs),
    #{module:=logger_simple_h} = SimpleC = find(simple,Hs),
    all = maps:get(level,SimpleC),
    notice = maps:get(level,P),
    SimpleFilters = maps:get(filters,SimpleC),
    {domain,{_,{log,super,[otp,sasl]}}} = lists:keyfind(domain,1,SimpleFilters),
    false = exists(sasl,Hs),
    [] = ML,
    ok.

error_logger_false_sasl_compatible(Config) ->
    {ok,#{handlers:=Hs,primary:=P,module_levels:=ML},_Node} =
        setup(Config,
              [{error_logger,false},
               {logger_level,notice},
               {logger_sasl_compatible,true}]),
    false = exists(?STANDARD_HANDLER,Hs),
    #{module:=logger_simple_h} = SimpleC = find(simple,Hs),
    all = maps:get(level,SimpleC),
    info = maps:get(level,P),
    SimpleFilters = maps:get(filters,SimpleC),
    {domain,{_,{log,super,[otp]}}} = lists:keyfind(domain,1,SimpleFilters),
    true = exists(sasl,Hs),
    [] = ML,
    ok.

error_logger_silent(Config) ->
    {ok,#{handlers:=Hs},_Node} = setup(Config,
                                       [{error_logger,silent}]),
    false = exists(?STANDARD_HANDLER,Hs),
    false = exists(simple,Hs),
    false = exists(sasl,Hs),
    ok.

error_logger_silent_sasl_compatible(Config) ->
    {ok,#{handlers:=Hs},_Node} = setup(Config,
                                       [{error_logger,silent},
                                        {logger_sasl_compatible,true}]),
    false = exists(?STANDARD_HANDLER,Hs),
    false = exists(simple,Hs),
    true = exists(sasl,Hs),
    ok.


error_logger_file(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,_,Node} = setup(Config,
                        [{error_logger,{file,Log}}]),
    check_default_log(Node,Log,
                      file,% dest
                      0),% progress in std logger
    ok.


logger_file(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{primary:=P,handlers:=Hs,module_levels:=ML},Node}
        = setup(Config,
                [{logger,
                  [{handler,?STANDARD_HANDLER,logger_std_h,
                    #{config=>#{type=>{file,Log}}}}]}]),
    check_default_log(Node,Log,
                      file,% dest
                      0),% progress in std logger

    notice = maps:get(level,P),
    #{module:=logger_std_h} = StdC = find(?STANDARD_HANDLER,Hs),
    all = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[otp,sasl]}}} = lists:keyfind(domain,1,StdFilters),
    false = exists(simple,Hs),
    false = exists(sasl,Hs),
    [] = ML,
    ok.

logger_file_sasl_compatible(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{primary:=P,handlers:=Hs,module_levels:=ML},Node}
        = setup(Config,
                [{logger_sasl_compatible,true},
                 {logger,
                  [{handler,?STANDARD_HANDLER,logger_std_h,
                    #{config=>#{type=>{file,Log}}}}]}]),
    check_default_log(Node,Log,
                      file,% dest
                      0),% progress in std logger

    info = maps:get(level,P),
    #{module:=logger_std_h} = StdC = find(?STANDARD_HANDLER,Hs),
    all = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[otp]}}} = lists:keyfind(domain,1,StdFilters),
    false = exists(simple,Hs),
    true = exists(sasl,Hs),
    [] = ML,
    ok.

logger_file_log_progress(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{primary:=P,handlers:=Hs,module_levels:=ML},Node}
        = setup(Config,
                [{logger_level,info},
                 {logger,
                  [{handler,?STANDARD_HANDLER,logger_std_h,
                    #{config=>#{type=>{file,Log}}}}]}]),
    check_default_log(Node,Log,
                      file,% dest
                      6,% progress in std logger
                      info),

    info = maps:get(level,P),
    #{module:=logger_std_h} = StdC = find(?STANDARD_HANDLER,Hs),
    all = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[otp,sasl]}}} = lists:keyfind(domain,1,StdFilters),
    false = exists(simple,Hs),
    false = exists(sasl,Hs),
    [] = ML,
    ok.

logger_file_no_filter(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs},Node}
        = setup(Config,
                [{logger,
                  [{handler,?STANDARD_HANDLER,logger_std_h,
                    #{filter_default=>log,filters=>[],
                      config=>#{type=>{file,Log}}}}]}]),
    check_default_log(Node,Log,
                      file,% dest
                      6),% progress in std logger

    #{module:=logger_std_h} = StdC = find(?STANDARD_HANDLER,Hs),
    all = maps:get(level,StdC),
    [] = maps:get(filters,StdC),
    false = exists(simple,Hs),
    false = exists(sasl,Hs),

    ok.

logger_file_no_filter_level(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs},Node}
        = setup(Config,
                [{logger,
                  [{handler,?STANDARD_HANDLER,logger_std_h,
                    #{filters=>[],level=>error,
                      config=>#{type=>{file,Log}}}}]}]),
    check_default_log(Node,Log,
                      file,% dest
                      0,% progress in std logger
                      error),% level

    #{module:=logger_std_h} = StdC = find(?STANDARD_HANDLER,Hs),
    error = maps:get(level,StdC),
    [] = maps:get(filters,StdC),
    false = exists(simple,Hs),
    false = exists(sasl,Hs),

    ok.

logger_file_formatter(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs},Node}
        = setup(Config,
                [{logger,
                  [{handler,?STANDARD_HANDLER,logger_std_h,
                    #{filters=>[],
                      formatter=>{logger_formatter,#{}},
                      config=>#{type=>{file,Log}}}}]}]),
    check_single_log(Node,Log,
                     file,% dest
                     6),% progress in std logger

    #{module:=logger_std_h} = StdC = find(?STANDARD_HANDLER,Hs),
    all = maps:get(level,StdC),
    [] = maps:get(filters,StdC),
    false = exists(simple,Hs),
    false = exists(sasl,Hs),

    ok.

logger_filters(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs,primary:=P},Node}
        = setup(Config,
                [{logger_level,info},
                 {logger,
                  [{handler,?STANDARD_HANDLER,logger_std_h,
                    #{config=>#{type=>{file,Log}}}},
                   {filters,log,[{stop_progress,{fun logger_filters:progress/2,stop}}]}
                  ]}]),
    check_default_log(Node,Log,
                      file,% dest
                      0,% progress in std logger
                      info),

    #{module:=logger_std_h} = StdC = find(?STANDARD_HANDLER,Hs),
    all = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[otp,sasl]}}} = lists:keyfind(domain,1,StdFilters),
    false = exists(simple,Hs),
    false = exists(sasl,Hs),
    LoggerFilters = maps:get(filters,P),
    true = lists:keymember(stop_progress,1,LoggerFilters),

    ok.

logger_filters_stop(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs,primary:=P},Node}
        = setup(Config,
                [{logger_level,info},
                 {logger,
                  [{handler,?STANDARD_HANDLER,logger_std_h,
                    #{filters=>[],
                      config=>#{type=>{file,Log}}}},
                   {filters,stop,[{log_error,{fun logger_filters:level/2,{log,gt,info}}}]}
                  ]}]),
    check_default_log(Node,Log,
                      file,% dest
                      0,% progress in std logger
                      info),

    #{module:=logger_std_h} = StdC = find(?STANDARD_HANDLER,Hs),
    all = maps:get(level,StdC),
    [] = maps:get(filters,StdC),
    false = exists(simple,Hs),
    false = exists(sasl,Hs),
    LoggerFilters = maps:get(filters,P),
    true = lists:keymember(log_error,1,LoggerFilters),

    ok.

logger_module_level(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs,module_levels:=ModuleLevels},Node}
        = setup(Config,
                [{logger_level,info},
                 {logger,
                  [{handler,?STANDARD_HANDLER,logger_std_h,
                    #{config=>#{type=>{file,Log}}}},
                   {module_level,error,[supervisor]}
                  ]}]),
    check_default_log(Node,Log,
                      file,% dest
                      3,% progress in std logger
                      info),

    #{module:=logger_std_h} = StdC = find(?STANDARD_HANDLER,Hs),
    all = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[otp,sasl]}}} = lists:keyfind(domain,1,StdFilters),
    false = exists(simple,Hs),
    false = exists(sasl,Hs),
    [{supervisor,error}] = ModuleLevels,
    ok.

logger_disk_log(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs},Node}
        = setup(Config,
                [{logger,
                  [{handler,?STANDARD_HANDLER,logger_disk_log_h,
                    #{config=>#{file=>Log}}}]}]),
    check_default_log(Node,Log,
                      disk_log,% dest
                      0),% progress in std logger

    #{module:=logger_disk_log_h} = StdC = find(?STANDARD_HANDLER,Hs),
    all = maps:get(level,StdC),
    StdFilters = maps:get(filters,StdC),
    {domain,{_,{log,super,[otp,sasl]}}} = lists:keyfind(domain,1,StdFilters),
    false = exists(simple,Hs),
    false = exists(sasl,Hs),

    ok.

logger_disk_log_formatter(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,#{handlers:=Hs},Node}
        = setup(Config,
                [{logger,
                  [{handler,?STANDARD_HANDLER,logger_disk_log_h,
                    #{filters=>[],
                      formatter=>{logger_formatter,#{}},
                      config=>#{file=>Log}}}]}]),
    check_single_log(Node,Log,
                     disk_log,% dest
                     6),% progress in std logger

    #{module:=logger_disk_log_h} = StdC = find(?STANDARD_HANDLER,Hs),
    all = maps:get(level,StdC),
    [] = maps:get(filters,StdC),
    false = exists(simple,Hs),
    false = exists(sasl,Hs),

    ok.

logger_undefined(Config) ->
    {ok,#{handlers:=Hs,primary:=P},_Node} =
        setup(Config,[{logger,[{handler,?STANDARD_HANDLER,undefined}]}]),
    false = exists(?STANDARD_HANDLER,Hs),
    #{module:=logger_simple_h} = SimpleC = find(simple,Hs),
    all = maps:get(level,SimpleC),
    notice = maps:get(level,P),
    SimpleFilters = maps:get(filters,SimpleC),
    {domain,{_,{log,super,[otp,sasl]}}} = lists:keyfind(domain,1,SimpleFilters),
    false = exists(sasl,Hs),
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
                   config=>#{type=>{file,LogErr}}}
                },
                {handler,info,logger_std_h,
                 #{level=>info,
                   filters=>[{level,{fun logger_filters:level/2,{stop,gteq,error}}}],
                   config=>#{type=>{file,LogInfo}}}
                }
               ]},
              {logger_level,info}], LogErr, LogInfo, 6).

%% Test that we can add multiple handlers with the default last
logger_many_handlers_default_last(Config) ->
    LogErr = file(Config,logger_many_handlers_default_last_error),
    LogInfo = file(Config,logger_many_handlers_default_last_info),
    logger_many_handlers(
      Config,[{logger,
               [{handler,info,logger_std_h,
                 #{level=>info,
                   filters=>[{level,{fun logger_filters:level/2,{stop,gteq,error}}}],
                   config=>#{type=>{file,LogInfo}}}
                },
                {handler,?STANDARD_HANDLER,logger_std_h,
                 #{level=>error,
                   filters=>[],
                   formatter=>{logger_formatter,#{}},
                   config=>#{type=>{file,LogErr}}}
                }
               ]},
              {logger_level,info}], LogErr, LogInfo, 7).

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
                   config=>#{type=>{file,LogInfo}}}
                },
                {handler,?STANDARD_HANDLER,logger_std_h,
                 #{level=>error,
                   filters=>[],
                   formatter=>{logger_formatter,#{}},
                   config=>#{type=>{file,LogErr}}}
                }
               ]},
              {logger_level,info}], LogErr, LogInfo, 7).

logger_many_handlers(Config, Env, LogErr, LogInfo, NumProgress) ->
    {ok,_,Node} = setup(Config,Env),
    check_single_log(Node,LogErr,
                     file,% dest
                     0,% progress in std logger
                     error), % level
    ok = rpc:call(Node,logger_std_h,filesync,[info]),
    {ok, Bin} = file:read_file(LogInfo),
    ct:log("Log content:~n~s",[Bin]),
    match(Bin,<<"info:">>,NumProgress,info,info),
    match(Bin,<<"notice:">>,1,notice,info),
    match(Bin,<<"alert:">>,0,alert,info),

    ok.

logger_proxy(Config) ->
    %% assume current node runs with default settings
    DefOpts = logger_olp:get_opts(logger_proxy),
    {ok,_,Node} = setup(Config,
                        [{logger,[{proxy,#{sync_mode_qlen=>0,
                                           drop_mode_qlen=>2}}]}]),
    Expected = DefOpts#{sync_mode_qlen:=0,
                        drop_mode_qlen:=2},
    Expected = rpc:call(Node,logger_olp,get_opts,[logger_proxy]),
    Expected = rpc:call(Node,logger,get_proxy_config,[]),

    ok.

sasl_compatible_false(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,_,Node} = setup(Config,
                        [{error_logger,{file,Log}},
                         {logger_sasl_compatible,false},
                         {logger_level,info}]), % to get progress
    check_default_log(Node,Log,
                      file,% dest
                      6,% progress in std logger
                      info),
    ok.

sasl_compatible_false_no_progress(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,_,Node} = setup(Config,
                        [{error_logger,{file,Log}},
                         {logger_sasl_compatible,false}]),
    check_default_log(Node,Log,
                      file,% dest
                      0),% progress in std logger
    ok.

sasl_compatible(Config) ->
    Log = file(Config,?FUNCTION_NAME),
    {ok,_,Node} = setup(Config,
                        [{error_logger,{file,Log}},
                         {sasl_compatible,true}]),
    check_default_log(Node,Log,
                      file,% dest
                      0),% progress in std logger
    ok.

all_logger_level(Config) ->
    [all_logger_level(Config,Level) || Level <- [none,
                                                 emergency,
                                                 alert,
                                                 critical,
                                                 error,
                                                 warning,
                                                 notice,
                                                 info,
                                                 debug,
                                                 all]],
    ok.

all_logger_level(Config,Level) ->
    {ok,#{primary:=#{level:=Level}},Node} = setup(Config,[{logger_level,Level}]),
    true = test_server:stop_node(Node),
    ok.

bad_error_logger(Config) ->
    error = setup(Config,[{error_logger,baddest}]).

bad_level(Config) ->
    error = setup(Config,[{logger_level,badlevel}]).

bad_sasl_compatibility(Config) ->
    error = setup(Config,[{logger_sasl_compatible,badcomp}]).

%%%-----------------------------------------------------------------
%%% Internal
file(Config,Func) ->
    filename:join(proplists:get_value(priv_dir,Config),
                  lists:concat([Func,".log"])).

check_default_log(Node,Log,Dest,NumProgress) ->
    check_default_log(Node,Log,Dest,NumProgress,notice).
check_default_log(Node,Log,Dest,NumProgress,Level) ->

    {ok,Bin1,Bin2} = check_log(Node,Log,Dest),

    match(Bin1,<<"PROGRESS REPORT">>,NumProgress,info,Level),
    match(Bin1,<<"ALERT REPORT">>,1,alert,Level),
    match(Bin1,<<"INFO REPORT">>,0,notice,Level),
    match(Bin1,<<"DEBUG REPORT">>,0,debug,Level),

    match(Bin2,<<"INFO REPORT">>,1,notice,Level),
    match(Bin2,<<"DEBUG REPORT">>,0,debug,Level),
    ok.

check_single_log(Node,Log,Dest,NumProgress) ->
    check_single_log(Node,Log,Dest,NumProgress,notice).
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

find(Id,Handlers) ->
    case lists:search(fun(#{id:=Id0}) when Id0=:=Id-> true;
                         (_) -> false end,
                      Handlers) of
        {value,Config} ->
            Config;
        false ->
            false
    end.

exists(Id,Handlers) ->
    case find(Id,Handlers) of
        false ->
            false;
        _ ->
            true
    end.
