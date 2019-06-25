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
-module(logger_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/src/logger_internal.hrl").

-define(str,"Log from "++atom_to_list(?FUNCTION_NAME)++
            ":"++integer_to_list(?LINE)).
-define(map_rep,#{function=>?FUNCTION_NAME, line=>?LINE}).
-define(keyval_rep,[{function,?FUNCTION_NAME}, {line,?LINE}]).

-define(MY_LOC(N),#{mfa=>{?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY},
                    file=>?FILE, line=>?LINE-N}).

-define(TRY(X), my_try(fun() -> X end)).


suite() ->
    [{timetrap,{seconds,30}},
     {ct_hooks,[logger_test_lib]}].

init_per_suite(Config) ->
    case logger:get_handler_config(?STANDARD_HANDLER) of
        {ok,StdH} ->
            ok = logger:remove_handler(?STANDARD_HANDLER),
            [{default_handler,StdH}|Config];
        _ ->
            Config
    end.

end_per_suite(Config) ->
    case ?config(default_handler,Config) of
        #{module:=HMod} = HConfig ->
            ok = logger:add_handler(?STANDARD_HANDLER,HMod,HConfig);
        _ ->
            ok
    end.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    PC = logger:get_primary_config(),
    [{logger_config,PC}|Config].

end_per_testcase(Case, Config) ->
    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,
    ok.

groups() ->
    [].

all() -> 
    [start_stop,
     add_remove_handler,
     multiple_handlers,
     add_remove_filter,
     change_config,
     set_formatter,
     log_no_levels,
     log_all_levels_api,
     macros,
     set_level,
     set_module_level,
     set_application_level,
     cache_module_level,
     format_report,
     filter_failed,
     handler_failed,
     config_sanity_check,
     log_failed,
     emulator,
     via_logger_process,
     other_node,
     compare_levels,
     process_metadata,
     app_config,
     kernel_config,
     pretty_print].

start_stop(_Config) ->
    S = whereis(logger),
    true = is_pid(S),
    ok.

add_remove_handler(_Config) ->
    register(callback_receiver,self()),
    Hs0 = logger:get_handler_config(),
    {error,{not_found,h1}} = logger:get_handler_config(h1),
    ok = logger:add_handler(h1,?MODULE,#{}),
    [add] = test_server:messages_get(),
    Hs = logger:get_handler_config(),
    Hs0 = lists:filter(fun(#{id:=h1}) -> false; (_) -> true end, Hs),
    {ok,#{module:=?MODULE,level:=all,filters:=[],filter_default:=log}} = %defaults
        logger:get_handler_config(h1),
    ok = logger:set_handler_config(h1,filter_default,stop),
    [changing_config] = test_server:messages_get(),
    ?LOG_NOTICE("hello",[]),
    ok = check_no_log(),
    ok = logger:set_handler_config(h1,filter_default,log),
    [changing_config] = test_server:messages_get(),
    {ok,#{filter_default:=log}} = logger:get_handler_config(h1),
    ?LOG_NOTICE("hello",[]),
    ok = check_logged(notice,"hello",[],?MY_LOC(1)),
    ok = logger:remove_handler(h1),
    [remove] = test_server:messages_get(),
    Hs0 = logger:get_handler_config(),
    {error,{not_found,h1}} = logger:get_handler_config(h1),
    {error,{not_found,h1}} = logger:remove_handler(h1),
    logger:notice("hello",[]),
    ok = check_no_log(),
    ok.

add_remove_handler(cleanup,_Config) ->
    logger:remove_handler(h1),
    ok.

multiple_handlers(_Config) ->
    ok = logger:add_handler(h1,?MODULE,#{level=>notice,filter_default=>log}),
    ok = logger:add_handler(h2,?MODULE,#{level=>error,filter_default=>log}),
    ?LOG_ERROR("hello",[]),
    ok = check_logged(error,"hello",[],?MY_LOC(1)),
    ok = check_logged(error,"hello",[],?MY_LOC(2)),
    ?LOG_NOTICE("hello",[]),
    ok = check_logged(notice,"hello",[],?MY_LOC(1)),
    ok = check_no_log(),
    ok.

multiple_handlers(cleanup,_Config) ->
    logger:remove_handler(h1),
    logger:remove_handler(h2),
    ok.

add_remove_filter(_Config) ->
    ok = logger:add_handler(h1,?MODULE,#{level=>notice,filter_default=>log}),
    LF = {fun(Log,_) -> Log#{level=>error} end, []},
    ok = logger:add_primary_filter(lf,LF),
    {error,{already_exist,lf}} = logger:add_primary_filter(lf,LF),
    {error,{already_exist,lf}} = logger:add_primary_filter(lf,{fun(Log,_) ->
                                                                       Log
                                                               end, []}),
    ?LOG_NOTICE("hello",[]),
    ok = check_logged(error,"hello",[],?MY_LOC(1)),
    ok = check_no_log(),

    ok = logger:add_handler(h2,?MODULE,#{level=>notice,filter_default=>log}),
    HF = {fun(#{level:=error}=Log,_) ->
                  Log#{level=>mylevel};
             (_,_) ->
                  ignore
          end,
          []},
    ok = logger:add_handler_filter(h1,hf,HF),
    {error,{already_exist,hf}} = logger:add_handler_filter(h1,hf,HF),
    {error,{already_exist,hf}} = logger:add_handler_filter(h1,hf,{fun(Log,_) ->
                                                                          Log
                                                                  end, []}),
    ?LOG_NOTICE("hello",[]),
    ok = check_logged(mylevel,"hello",[],?MY_LOC(1)),
    ok = check_logged(error,"hello",[],?MY_LOC(2)),

    ok = logger:remove_primary_filter(lf),
    {error,{not_found,lf}} = logger:remove_primary_filter(lf),

    ?LOG_NOTICE("hello",[]),
    ok = check_logged(notice,"hello",[],?MY_LOC(1)),
    ok = check_logged(notice,"hello",[],?MY_LOC(2)),

    ?LOG_ERROR("hello",[]),
    ok = check_logged(mylevel,"hello",[],?MY_LOC(1)),
    ok = check_logged(error,"hello",[],?MY_LOC(2)),

    ok = logger:remove_handler_filter(h1,hf),
    {error,{not_found,hf}} = logger:remove_handler_filter(h1,hf),
    ?LOG_NOTICE("hello",[]),
    ok = check_logged(notice,"hello",[],?MY_LOC(1)),
    ok = check_logged(notice,"hello",[],?MY_LOC(2)),

    ?LOG_ERROR("hello",[]),
    ok = check_logged(error,"hello",[],?MY_LOC(1)),
    ok = check_logged(error,"hello",[],?MY_LOC(2)),
    ok.

add_remove_filter(cleanup,_Config) ->
    logger:remove_primary_filter(lf),
    logger:remove_handler(h1),
    logger:remove_handler(h2),
    ok.

change_config(_Config) ->
    %% Overwrite handler config - check that defaults are added
    {error,{not_found,h1}} = logger:set_handler_config(h1,#{}),
    ok = logger:add_handler(h1,?MODULE,#{level=>notice,custom=>custom}),
    {ok,#{module:=?MODULE,level:=notice,filter_default:=log,custom:=custom}} =
        logger:get_handler_config(h1),
    register(callback_receiver,self()),
    ok = logger:set_handler_config(h1,#{filter_default=>stop}),
    [changing_config] = test_server:messages_get(),
    {ok,#{module:=?MODULE,level:=all,filter_default:=stop}=C2} =
        logger:get_handler_config(h1),
    false = maps:is_key(custom,C2),
    {error,fail} = logger:set_handler_config(h1,#{conf_call=>fun() -> {error,fail} end}),
    {error,{attempting_syncronous_call_to_self,_}} =
        logger:set_handler_config(
          h1,#{conf_call=>fun() -> logger:set_handler_config(?MODULE,#{}) end}),
    ok =
        logger:set_handler_config(
          h1,#{conf_call=>fun() -> logger:set_module_level(?MODULE,debug) end}),
    {ok,C2} = logger:get_handler_config(h1),

    %% Change handler config: Single key
    {error,fail} = logger:set_handler_config(h1,conf_call,fun() -> {error,fail} end),
    ok = logger:set_handler_config(h1,custom,custom),
    [changing_config] = test_server:messages_get(),
    {ok,#{custom:=custom}=C3} = logger:get_handler_config(h1),
    C2 = maps:remove(custom,C3),

    %% Change handler config: Map
    ok = logger:update_handler_config(h1,#{custom=>new_custom}),
    [changing_config] = test_server:messages_get(),
    {ok,C4} = logger:get_handler_config(h1),
    C4 = C3#{custom:=new_custom},

    %% Change handler config: Id and module can not be changed
    {error,{illegal_config_change,Old,New}} =
        logger:set_handler_config(h1,id,newid),
    %% Check that only the faulty field is included in return
    [{id,h1}] = maps:to_list(Old),
    [{id,newid}] = maps:to_list(New),
    %% Check that both fields are included when both are changed
    {error,{illegal_config_change,
            #{id:=h1,module:=?MODULE},
            #{id:=newid,module:=newmodule}}} =
        logger:set_handler_config(h1,#{id=>newid,module=>newmodule}),

    %% Change primary config: Single key
    PConfig0 = logger:get_primary_config(),
    ok = logger:set_primary_config(level,warning),
    PConfig1 = logger:get_primary_config(),
    PConfig1 = PConfig0#{level:=warning},

    %% Change primary config: Map
    ok = logger:update_primary_config(#{level=>error}),
    PConfig2 = logger:get_primary_config(),
    PConfig2 = PConfig1#{level:=error},

    %% Overwrite primary config - check that defaults are added
    ok = logger:set_primary_config(#{filter_default=>stop}),
    #{level:=notice,filters:=[],filter_default:=stop}=PC1 =
        logger:get_primary_config(),
    3 = maps:size(PC1),
    %% Check that internal 'handlers' field has not been changed
    MS = [{{{?HANDLER_KEY,'$1'},'_','_'},[],['$1']}],
    HIds1 = lists:sort(ets:select(?LOGGER_TABLE,MS)), % dirty, internal data
    HIds2 = lists:sort(logger:get_handler_ids()),
    HIds1 = HIds2,

    %% Cleanup
    ok = logger:set_primary_config(PConfig0),
    [] = test_server:messages_get(),

    ok.

change_config(cleanup,Config) ->
    logger:remove_handler(h1),
    PC = ?config(logger_config,Config),
    logger:set_primary_config(PC),
    ok.

set_formatter(_Config) ->
    {error,{not_found,h1}}=logger:set_handler_config(h1,formatter,{?MODULE,[]}),
    ok = logger:add_handler(h1,?MODULE,#{level=>notice,filter_default=>log}),
    ok = logger:set_handler_config(h1,formatter,{?MODULE,[]}),
    logger:notice("hello",[]),
    receive
        {_Log,#{formatter:={?MODULE,[]}}} ->
            ok
    after 500 ->
            ct:fail({timeout,no_log,process_info(self(),messages)})
    end,
    ok.

set_formatter(cleanup,_Config) ->
    logger:remove_handler(h1),
    ok.

log_no_levels(_Config) ->
    ok = logger:add_handler(h1,?MODULE,#{level=>all,filter_default=>log}),
    logger:notice(M1=?map_rep),
    ok = check_logged(notice,M1,#{}),

    Levels = [emergency,alert,critical,error,warning,notice,info,debug],
    ok = logger:set_primary_config(level,none),
    [logger:Level(#{Level=>rep}) || Level <- Levels],
    ok = check_no_log(),
    
    ok = logger:set_primary_config(level,all),
    M2 = ?map_rep,
    ?LOG_NOTICE(M2),
    ok = check_logged(notice,M2,#{}),

    ok = logger:set_module_level(?MODULE,none),
    ?LOG_EMERGENCY(?map_rep),
    ?LOG_ALERT(?map_rep),
    ?LOG_CRITICAL(?map_rep),
    ?LOG_ERROR(?map_rep),
    ?LOG_WARNING(?map_rep),
    ?LOG_NOTICE(?map_rep),
    ?LOG_INFO(?map_rep),
    ?LOG_DEBUG(?map_rep),
    ok = check_no_log(),
    
    ok = logger:unset_module_level(?MODULE),
    logger:notice(M3=?map_rep),
    ok = check_logged(notice,M3,#{}),
    
    ok = logger:set_handler_config(h1,level,none),
    [logger:Level(#{Level=>rep}) || Level <- Levels],
    ok = check_no_log(),
    
    ok.
log_no_levels(cleanup,_Config) ->
    logger:remove_handler(h1),
    logger:set_primary_config(level,notice),
    logger:unset_module_level(?MODULE),
    ok.

log_all_levels_api(_Config) ->
    ok = logger:set_primary_config(level,all),
    ok = logger:add_handler(h1,?MODULE,#{level=>all,filter_default=>log}),
    test_api(emergency),
    test_api(alert),
    test_api(critical),
    test_api(error),
    test_api(warning),
    test_api(notice),
    test_api(info),
    test_api(debug),
    test_log_function(emergency),
    ok.

log_all_levels_api(cleanup,_Config) ->
    logger:remove_handler(h1),
    logger:set_primary_config(level,notice),
    ok.

macros(_Config) ->
    ok = logger:add_handler(h1,?MODULE,#{level=>all,filter_default=>log}),
    test_macros(emergency),
    test_log_macro(alert),
    ok.

macros(cleanup,_Config) ->
    logger:remove_handler(h1),
    logger:unset_module_level(?MODULE),
    ok.

set_level(_Config) ->
    ok = logger:add_handler(h1,?MODULE,#{level=>all,filter_default=>log}),
    logger:debug(?map_rep),
    ok = check_no_log(),
    logger:notice(M1=?map_rep),
    ok = check_logged(notice,M1,#{}),
    ok = logger:set_primary_config(level,debug),
    logger:debug(M2=?map_rep),
    ok = check_logged(debug,M2,#{}),
    ok.

set_level(cleanup,_Config) ->
    logger:remove_handler(h1),
    logger:set_primary_config(level,notice),
    ok.

set_module_level(_Config) ->
    [] = logger:get_module_level([?MODULE,other]),
    [] = logger:get_module_level(?MODULE),
    [] = logger:get_module_level(),

    ok = logger:add_handler(h1,?MODULE,#{level=>notice,filter_default=>log}),
    {error,{invalid_level,bad}} = logger:set_module_level(?MODULE,bad),
    {error,{not_a_list_of_modules,{bad}}} =
        logger:set_module_level({bad},warning),
    {error,{not_a_list_of_modules,[{bad}]}} =
        logger:set_module_level([{bad}],warning),
    ok = logger:set_module_level(?MODULE,warning),
    [{?MODULE,warning}] = logger:get_module_level([?MODULE,other]),
    [{?MODULE,warning}] = logger:get_module_level(?MODULE),
    [{?MODULE,warning}] = logger:get_module_level(),
    logger:notice(?map_rep,?MY_LOC(0)),
    ok = check_no_log(),
    logger:warning(M1=?map_rep,?MY_LOC(0)),
    ok = check_logged(warning,M1,?MY_LOC(1)),
    ok = logger:set_module_level(?MODULE,notice),
    [{?MODULE,notice}] = logger:get_module_level([?MODULE,other]),
    [{?MODULE,notice}] = logger:get_module_level(?MODULE),
    [{?MODULE,notice}] = logger:get_module_level(),
    logger:notice(M2=?map_rep,?MY_LOC(0)),
    ok = check_logged(notice,M2,?MY_LOC(1)),

    {error,{not_a_list_of_modules,{bad}}} = logger:unset_module_level({bad}),
    {error,{not_a_list_of_modules,[{bad}]}} = logger:unset_module_level([{bad}]),
    ok = logger:unset_module_level(?MODULE),
    [] = logger:get_module_level([?MODULE,other]),
    [] = logger:get_module_level(?MODULE),
    [] = logger:get_module_level(),

    ok = logger:set_module_level([m1,m2,m3],notice),
    [{m1,notice},{m2,notice},{m3,notice}] = logger:get_module_level(),
    ok = logger:unset_module_level(m2),
    [{m1,notice},{m3,notice}] = logger:get_module_level(),
    ok = logger:unset_module_level(),
    [] = logger:get_module_level(),

    ok.

set_module_level(cleanup,_Config) ->
    logger:remove_handler(h1),
    logger:unset_module_level(?MODULE),
    ok.

set_application_level(_Config) ->

    {error,{not_loaded,mnesia}} = logger:set_application_level(mnesia, warning),
    {error,{not_loaded,mnesia}} = logger:unset_application_level(mnesia),

    case application:load(mnesia) of
        ok ->
            {ok, Modules} = application:get_key(mnesia, modules),
            [] = logger:get_module_level(Modules),

            {error,{invalid_level,warn}} =
                logger:set_application_level(mnesia, warn),

            ok = logger:set_application_level(mnesia, debug),
            DebugModules = lists:sort([{M,debug} || M <- Modules]),
            DebugModules = lists:sort(logger:get_module_level(Modules)),

            ok = logger:set_application_level(mnesia, warning),

            WarnModules = lists:sort([{M,warning} || M <- Modules]),
            WarnModules = lists:sort(logger:get_module_level(Modules)),

            ok = logger:unset_application_level(mnesia),
            [] = logger:get_module_level(Modules);
        {error,{"no such file or directory","mnesia.app"}} ->
            {skip, "Cannot load mnesia, does not exist"}
    end.

set_application_level(cleanup,_Config) ->
    _ = logger:unset_application_level(mnesia),
    _ = application:unload(mnesia),
    ok.

cache_module_level(_Config) ->
    ok = logger:unset_module_level(?MODULE),
    [] = ets:lookup(?LOGGER_TABLE,?MODULE), %dirty - add API in logger_config?
    ?LOG_NOTICE(?map_rep),
    %% Caching is done asynchronously, so wait a bit for the update
    timer:sleep(100),
    [_] = ets:lookup(?LOGGER_TABLE,?MODULE), %dirty - add API in logger_config?
    ok = logger:unset_module_level(?MODULE),
    [] = ets:lookup(?LOGGER_TABLE,?MODULE), %dirty - add API in logger_config?
    ok.

cache_module_level(cleanup,_Config) ->
    logger:unset_module_level(?MODULE),
    ok.

format_report(_Config) ->
    {"~ts",["string"]} = logger:format_report("string"),
    {"~tp",[term]} = logger:format_report(term),
    {"~tp",[[]]} = logger:format_report([]),
    {"    ~tp: ~tp",[key,value]} = logger:format_report([{key,value}]),
    KeyVals = [{key1,value1},{key2,"value2"},{key3,[]}],
    KeyValRes =
        {"    ~tp: ~tp\n    ~tp: ~ts\n    ~tp: ~tp",
         [key1,value1,key2,"value2",key3,[]]} =
        logger:format_report(KeyVals),
    KeyValRes = logger:format_report(maps:from_list(KeyVals)),
    KeyValRes = logger:format_otp_report(#{label=>{?MODULE,test},report=>KeyVals}),
    {"    ~tp: ~tp\n    ~tp: ~tp",
     [label,{?MODULE,test},report,KeyVals]} =
        logger:format_report(#{label=>{?MODULE,test},report=>KeyVals}),

    {"    ~tp: ~tp\n    ~tp",[key1,value1,term]} =
        logger:format_report([{key1,value1},term]),

    {"    ~tp: ~tp\n    ~tp",[key1,value1,[]]} =
        logger:format_report([{key1,value1},[]]),

    {"~tp",[[]]} = logger:format_report([[],[],[]]),

    ok.

filter_failed(_Config) ->
    ok = logger:add_handler(h1,?MODULE,#{level=>notice,filter_default=>log}),

    %% Logger filters
    {error,{invalid_filter,_}} =
        logger:add_primary_filter(lf,{fun(_) -> ok end,args}),
    ok = logger:add_primary_filter(lf,
                                   {fun(_,_) ->
                                            erlang:error({badmatch,b})
                                    end,
                                    args}),
    #{filters:=[_]} = logger:get_primary_config(),
    ok = logger:notice(M1=?map_rep),
    ok = check_logged(notice,M1,#{}),
    {error,{not_found,lf}} = logger:remove_primary_filter(lf),

    ok = logger:add_primary_filter(lf,{fun(_,_) -> faulty_return end,args}),
    #{filters:=[_]} = logger:get_primary_config(),
    ok = logger:notice(M2=?map_rep),
    ok = check_logged(notice,M2,#{}),
    {error,{not_found,lf}} = logger:remove_primary_filter(lf),

    %% Handler filters
    {error,{not_found,h0}} =
        logger:add_handler_filter(h0,hf,{fun(_,_) -> ignore end,args}),
    {error,{not_found,h0}} = logger:remove_handler_filter(h0,hf),
    {error,{invalid_filter,_}} =
        logger:add_handler_filter(h1,hf,{fun(_) -> ok end,args}),
    ok = logger:add_handler_filter(h1,hf,
                                   {fun(_,_) ->
                                            erlang:error({badmatch,b})
                                    end,
                                    args}),
    {ok,#{filters:=[_]}} = logger:get_handler_config(h1),
    ok = logger:notice(M3=?map_rep),
    ok = check_logged(notice,M3,#{}),
    {error,{not_found,hf}} = logger:remove_handler_filter(h1,hf),

    ok = logger:add_handler_filter(h1,hf,{fun(_,_) -> faulty_return end,args}),
    {ok,#{filters:=[_]}} = logger:get_handler_config(h1),
    ok = logger:notice(M4=?map_rep),
    ok = check_logged(notice,M4,#{}),
    {error,{not_found,hf}} = logger:remove_handler_filter(h1,hf),

    ok.

filter_failed(cleanup,_Config) ->
    logger:remove_handler(h1),
    ok.

handler_failed(_Config) ->
    register(callback_receiver,self()),
    {error,{invalid_id,1}} = logger:add_handler(1,?MODULE,#{}),
    {error,{invalid_module,"nomodule"}} = logger:add_handler(h1,"nomodule",#{}),
    {error,{invalid_config,bad}} = logger:add_handler(h1,?MODULE,bad),
    {error,{invalid_filters,false}} =
        logger:add_handler(h1,?MODULE,#{filters=>false}),
    {error,{invalid_filter_default,true}} =
        logger:add_handler(h1,?MODULE,#{filter_default=>true}),
    {error,{invalid_formatter,[]}} =
        logger:add_handler(h1,?MODULE,#{formatter=>[]}),
    {error,{invalid_handler,_}} = logger:add_handler(h1,nomodule,#{filter_default=>log}),
    logger:notice(?map_rep),
    check_no_log(),
    H1 = logger:get_handler_config(),
    false = lists:search(fun(#{id:=h1}) -> true; (_) -> false end,H1),
    {error,{not_found,h1}} = logger:remove_handler(h1),

    ok = logger:add_handler(h2,?MODULE,
                            #{filter_default => log,
                              log_call => fun() ->
                                                  erlang:error({badmatch,b})
                                          end}),
    {error,{already_exist,h2}} = logger:add_handler(h2,othermodule,#{}),
    [add] = test_server:messages_get(),

    logger:notice(?map_rep),
    [remove] = test_server:messages_get(),
    H2 = logger:get_handler_config(),
    false = lists:search(fun(#{id:=h2}) -> true; (_) -> false end,H2),
    {error,{not_found,h2}} = logger:remove_handler(h2),

    CallAddHandler = fun() -> logger:add_handler(h2,?MODULE,#{}) end,
    CrashHandler = fun() -> erlang:error({badmatch,b}) end,
    KillHandler = fun() -> exit(self(), die) end,

    {error,{handler_not_added,{attempting_syncronous_call_to_self,_}}} =
        logger:add_handler(h1,?MODULE,#{add_call=>CallAddHandler}),
    {error,{handler_not_added,{callback_crashed,_}}} =
        logger:add_handler(h1,?MODULE,#{add_call=>CrashHandler}),
    {error,{handler_not_added,{logger_process_exited,_,die}}} =
        logger:add_handler(h1,?MODULE,#{add_call=>KillHandler}),

    check_no_log(),
    ok = logger:add_handler(h1,?MODULE,#{}),
    {error,{attempting_syncronous_call_to_self,_}} =
        logger:set_handler_config(h1,#{conf_call=>CallAddHandler}),
    {error,{callback_crashed,_}} =
        logger:set_handler_config(h1,#{conf_call=>CrashHandler}),
    {error,{logger_process_exited,_,die}} =
        logger:set_handler_config(h1,#{conf_call=>KillHandler}),

    {error,{attempting_syncronous_call_to_self,_}} =
        logger:set_handler_config(h1,conf_call,CallAddHandler),
    {error,{callback_crashed,_}} =
        logger:set_handler_config(h1,conf_call,CrashHandler),
    {error,{logger_process_exited,_,die}} =
        logger:set_handler_config(h1,conf_call,KillHandler),

    ok = logger:remove_handler(h1),
    [add,remove] = test_server:messages_get(),

    check_no_log(),
    ok = logger:add_handler(h1,?MODULE,#{rem_call=>CallAddHandler}),
    ok = logger:remove_handler(h1),
    ok = logger:add_handler(h1,?MODULE,#{rem_call=>CrashHandler}),
    ok = logger:remove_handler(h1),
    ok = logger:add_handler(h1,?MODULE,#{rem_call=>KillHandler}),
    ok = logger:remove_handler(h1),
    [add,add,add] = test_server:messages_get(),

    ok.

handler_failed(cleanup,_Config) ->
    logger:remove_handler(h1),
    logger:remove_handler(h2),
    ok.

config_sanity_check(_Config) ->
    %% Primary config
    {error,{invalid_config,bad}} = logger:set_primary_config(bad),
    {error,{invalid_filter_default,bad}} =
        logger:set_primary_config(filter_default,bad),
    {error,{invalid_level,bad}} = logger:set_primary_config(level,bad),
    {error,{invalid_filters,bad}} = logger:set_primary_config(filters,bad),
    {error,{invalid_filter,bad}} = logger:set_primary_config(filters,[bad]),
    {error,{invalid_filter,{_,_}}} =
        logger:set_primary_config(filters,[{id,bad}]),
    {error,{invalid_filter,{_,{_,_}}}} =
        logger:set_primary_config(filters,[{id,{bad,args}}]),
    {error,{invalid_filter,{_,{_,_}}}} =
        logger:set_primary_config(filters,[{id,{fun() -> ok end,args}}]),
    {error,{invalid_primary_config,{bad,bad}}} =
        logger:set_primary_config(bad,bad),

    %% Handler config
    {error,{not_found,h1}} = logger:set_handler_config(h1,a,b),
    ok = logger:add_handler(h1,?MODULE,#{}),
    {error,{invalid_filter_default,bad}} =
        logger:set_handler_config(h1,filter_default,bad),
    {error,{invalid_level,bad}} = logger:set_handler_config(h1,level,bad),
    {error,{invalid_filters,bad}} = logger:set_handler_config(h1,filters,bad),
    {error,{invalid_filter,bad}} = logger:set_handler_config(h1,filters,[bad]),
    {error,{invalid_filter,{_,_}}} =
        logger:set_handler_config(h1,filters,[{id,bad}]),
    {error,{invalid_filter,{_,{_,_}}}} =
        logger:set_handler_config(h1,filters,[{id,{bad,args}}]),
    {error,{invalid_filter,{_,{_,_}}}} =
        logger:set_handler_config(h1,filters,[{id,{fun() -> ok end,args}}]),
    {error,{invalid_formatter,bad}} =
        logger:set_handler_config(h1,formatter,bad),
    {error,{invalid_module,{bad}}} =
        logger:set_handler_config(h1,formatter,{{bad},cfg}),
    {error,{invalid_formatter_config,logger_formatter,bad}} =
        logger:set_handler_config(h1,formatter,{logger_formatter,bad}),
    {error,{invalid_formatter_config,logger_formatter,{bad,bad}}} =
        logger:set_handler_config(h1,formatter,{logger_formatter,#{bad=>bad}}),
    {error,{invalid_formatter_template,logger_formatter,bad}} =
        logger:set_handler_config(h1,formatter,{logger_formatter,
                                                #{template=>bad}}),
    {error,{invalid_formatter_template,logger_formatter,[1]}} =
        logger:set_handler_config(h1,formatter,{logger_formatter,
                                                #{template=>[1]}}),
    ok = logger:set_handler_config(h1,formatter,{logger_formatter,
                                                 #{template=>[]}}),
    {error,{invalid_formatter_config,logger_formatter,{single_line,bad}}} =
        logger:set_handler_config(h1,formatter,{logger_formatter,
                                                #{single_line=>bad}}),
    ok = logger:set_handler_config(h1,formatter,{logger_formatter,
                                                 #{single_line=>true}}),
    {error,{invalid_formatter_config,logger_formatter,{legacy_header,bad}}} =
        logger:set_handler_config(h1,formatter,{logger_formatter,
                                                #{legacy_header=>bad}}),
    ok = logger:set_handler_config(h1,formatter,{logger_formatter,
                                                 #{legacy_header=>true}}),
    {error,{invalid_formatter_config,logger_formatter,{report_cb,bad}}} =
        logger:set_handler_config(h1,formatter,{logger_formatter,
                                                #{report_cb=>bad}}),
    ok = logger:set_handler_config(h1,formatter,{logger_formatter,
                                                 #{report_cb=>fun(R) ->
                                                                      {"~p",[R]}
                                                              end}}),
    {error,{invalid_formatter_config,logger_formatter,{chars_limit,bad}}} =
        logger:set_handler_config(h1,formatter,{logger_formatter,
                                                #{chars_limit=>bad}}),
    ok = logger:set_handler_config(h1,formatter,{logger_formatter,
                                                 #{chars_limit=>unlimited}}),
    ok = logger:set_handler_config(h1,formatter,{logger_formatter,
                                                 #{chars_limit=>4}}),
    {error,{invalid_formatter_config,logger_formatter,{depth,bad}}} =
        logger:set_handler_config(h1,formatter,{logger_formatter,
                                                #{depth=>bad}}),
    ok = logger:set_handler_config(h1,formatter,{logger_formatter,
                                                 #{depth=>unlimited}}),
    ok = logger:set_handler_config(h1,formatter,{logger_formatter,
                                                 #{depth=>4}}),
    {error,{invalid_formatter_config,logger_formatter,{max_size,bad}}} =
        logger:set_handler_config(h1,formatter,{logger_formatter,
                                                #{max_size=>bad}}),
    ok = logger:set_handler_config(h1,formatter,{logger_formatter,
                                                 #{max_size=>unlimited}}),
    ok = logger:set_handler_config(h1,formatter,{logger_formatter,
                                                 #{max_size=>4}}),
    ok = logger:set_handler_config(h1,formatter,{module,config}),
    {error,{callback_crashed,{error,{badmatch,3},[{?MODULE,check_config,1,_}]}}} =
        logger:set_handler_config(h1,formatter,{?MODULE,crash}),
    ok = logger:set_handler_config(h1,custom,custom),

    %% Old utc parameter is no longer allowed (replaced by time_offset)
    {error,{invalid_formatter_config,logger_formatter,{utc,true}}} =
         logger:set_handler_config(h1,formatter,{logger_formatter,
                                                 #{utc=>true}}),
    {error,{invalid_formatter_config,logger_formatter,{time_offset,bad}}} =
        logger:set_handler_config(h1,formatter,{logger_formatter,
                                                #{time_offset=>bad}}),
    ok = logger:set_handler_config(h1,formatter,{logger_formatter,
                                                 #{time_offset=>0}}),
    ok = logger:set_handler_config(h1,formatter,{logger_formatter,
                                                 #{time_offset=>""}}),
    ok = logger:set_handler_config(h1,formatter,{logger_formatter,
                                                 #{time_offset=>"Z"}}),
    ok = logger:set_handler_config(h1,formatter,{logger_formatter,
                                                 #{time_offset=>"z"}}),
    ok = logger:set_handler_config(h1,formatter,{logger_formatter,
                                                 #{time_offset=>"-0:0"}}),
    ok = logger:set_handler_config(h1,formatter,{logger_formatter,
                                                 #{time_offset=>"+10:13"}}),

    {error,{invalid_formatter_config,logger_formatter,{time_offset,"+0"}}} =
        logger:set_handler_config(h1,formatter,{logger_formatter,
                                                #{time_offset=>"+0"}}),

    {error,{invalid_formatter_config,logger_formatter,{time_designator,bad}}} =
        logger:set_handler_config(h1,formatter,{logger_formatter,
                                                #{time_designator=>bad}}),
    {error,{invalid_formatter_config,logger_formatter,{time_designator,"s"}}} =
        logger:set_handler_config(h1,formatter,{logger_formatter,
                                                #{time_designator=>"s"}}),
    {error,{invalid_formatter_config,logger_formatter,{time_designator,0}}} =
        logger:set_handler_config(h1,formatter,{logger_formatter,
                                                #{time_designator=>0}}),
    ok = logger:set_handler_config(h1,formatter,{logger_formatter,
                                                 #{time_designator=>$\s}}),
    ok.

config_sanity_check(cleanup,_Config) ->
    logger:remove_handler(h1),
    ok.

log_failed(_Config) ->
    ok = logger:add_handler(h1,?MODULE,#{level=>notice,filter_default=>log}),
    {error,function_clause} = ?TRY(logger:log(bad,?map_rep)),
    {error,function_clause} = ?TRY(logger:log(notice,?map_rep,bad)),
    {error,function_clause} = ?TRY(logger:log(notice,fun() -> ?map_rep end,bad)),
    {error,function_clause} = ?TRY(logger:log(notice,fun() -> ?map_rep end,bad,#{})),
    {error,function_clause} = ?TRY(logger:log(notice,bad,bad,bad)),
    {error,function_clause} = ?TRY(logger:log(notice,bad,bad,#{})),
    check_no_log(),
    ok = logger:log(notice,M1=?str,#{}),
    check_logged(notice,M1,#{}),
    ok = logger:log(notice,M2=?map_rep,#{}),
    check_logged(notice,M2,#{}),
    ok = logger:log(notice,M3=?keyval_rep,#{}),
    check_logged(notice,M3,#{}),

    %% Should we check report input more thoroughly?
    ok = logger:log(notice,M4=?keyval_rep++[other,stuff,in,list],#{}),
    check_logged(notice,M4,#{}),

    %% This might break a handler since it is assumed to be a format
    %% string and args, so it depends how the handler protects itself
    %% against something like io_lib:format("ok","ok")
    ok = logger:log(notice,"ok","ok",#{}),
    check_logged(notice,"ok","ok",#{}),

    ok.

log_failed(cleanup,_Config) ->
    logger:remove_handler(h1),
    ok.

emulator(_Config) ->
    ok = logger:add_handler(h1,?MODULE,#{level=>notice,filter_default=>log,
                                         tc_proc=>self()}),
    Msg = "Error in process ~p on node ~p with exit value:~n~p~n",
    Error = {badmatch,4},
    Stack = [{module, function, 2, []}],
    Pid = spawn(?MODULE, generate_error, [Error, Stack]),
    check_logged(error, Msg, [Pid, node(), {Error, Stack}],
                 #{gl=>group_leader(),
                   error_logger=>#{tag=>error,emulator=>true}}),
    ok.

emulator(cleanup, _Config) ->
    logger:remove_handler(h1),
    ok.

generate_error(Error, Stack) ->
    erlang:raise(error, Error, Stack).

via_logger_process(Config) ->
    ok = logger:add_handler(h1,?MODULE,#{level=>notice,filter_default=>log,
                                         tc_proc=>self()}),

    %% Explicitly send a message to the logger process
    %% This is used by code_server, erl_prim_loader, init, prim_file, ...
    Msg = ?str,
    logger ! {log,error,Msg,[],#{}},
    check_logged(error, Msg, [], #{}),

    case os:type() of
        {win32,_} ->
            %% Skip this part on windows - cant change file mode"
            ok;
        _ ->
            %% This should trigger the same thing from erl_prim_loader
            Dir = filename:join(?config(priv_dir,Config),"dummydir"),
            ok = file:make_dir(Dir),
            ok = file:change_mode(Dir,8#0222),
            error = erl_prim_loader:list_dir(Dir),
            check_logged(error,
                         #{report=>"File operation error: eacces. Target: " ++
                               Dir ++". Function: list_dir. "},
                         #{pid=>self(),
                           gl=>group_leader(),
                           error_logger=>#{tag=>error_report,
                                           type=>std_error}}),
            ok
    end.

via_logger_process(cleanup, Config) ->
    Dir = filename:join(?config(priv_dir,Config),"dummydir"),
    _ = file:change_mode(Dir,8#0664),
    _ = file:del_dir(Dir),
    logger:remove_handler(h1),
    ok.

other_node(_Config) ->
    ok = logger:add_handler(h1,?MODULE,#{level=>notice,filter_default=>log,
                                         tc_proc=>self()}),
    {ok,Node} = test_server:start_node(?FUNCTION_NAME,slave,[]),
    rpc:call(Node,logger,error,[Msg=?str,#{}]),
    check_logged(error,Msg,#{}),
    ok.

other_node(cleanup,_Config) ->
    Nodes = nodes(),
    [test_server:stop_node(Node) || Node <- Nodes],
    logger:remove_handler(h1),
    ok.

compare_levels(_Config) ->
    Levels = [none,emergency,alert,critical,error,warning,notice,info,debug,all],
    ok = compare(Levels),
    {error,badarg} = ?TRY(logger:compare_levels(bad,bad)),
    {error,badarg} = ?TRY(logger:compare_levels({bad},notice)),
    {error,badarg} = ?TRY(logger:compare_levels(notice,"bad")),
    ok.

compare([L|Rest]) ->
    eq = logger:compare_levels(L,L),
    [gt = logger:compare_levels(L,L1) || L1 <- Rest],
    [lt = logger:compare_levels(L1,L) || L1 <- Rest],
    compare(Rest);
compare([]) ->
    ok.

process_metadata(_Config) ->
    undefined = logger:get_process_metadata(),
    {error,badarg} = ?TRY(logger:set_process_metadata(bad)),
    ok = logger:add_handler(h1,?MODULE,#{level=>notice,filter_default=>log}),
    Time = logger:timestamp(),
    ProcMeta = #{time=>Time,line=>0,custom=>proc},
    ok = logger:set_process_metadata(ProcMeta),
    S1 = ?str,
    ?LOG_NOTICE(S1,#{custom=>macro}),
    check_logged(notice,S1,#{time=>Time,line=>0,custom=>macro}),

    Time2 = logger:timestamp(),
    S2 = ?str,
    ?LOG_NOTICE(S2,#{time=>Time2,line=>1,custom=>macro}),
    check_logged(notice,S2,#{time=>Time2,line=>1,custom=>macro}),

    logger:notice(S3=?str,#{custom=>func}),
    check_logged(notice,S3,#{time=>Time,line=>0,custom=>func}),

    ProcMeta = logger:get_process_metadata(),
    ok = logger:update_process_metadata(#{custom=>changed,custom2=>added}),
    Expected = ProcMeta#{custom:=changed,custom2=>added},
    Expected = logger:get_process_metadata(),
    ok = logger:unset_process_metadata(),
    undefined = logger:get_process_metadata(),

    ok = logger:update_process_metadata(#{custom=>added_again}),
    {error,badarg} = ?TRY(logger:update_process_metadata(bad)),
    #{custom:=added_again} = logger:get_process_metadata(),

    ok.

process_metadata(cleanup,_Config) ->
    logger:remove_handler(h1),
    ok.

app_config(Config) ->
    %% Start a node with default configuration
    {ok,_,Node} = logger_test_lib:setup(Config,[]),

    App1Name = app1,
    App1 = {application, App1Name,
            [{description, "Test of app with logger config"},
             {applications, [kernel]}]},
    ok = rpc:call(Node,application,load,[App1]),
    ok = rpc:call(Node,application,set_env,
                  [App1Name,logger,[{handler,default,logger_std_h,#{}}]]),

    %% Try to add an own default handler
    {error,{bad_config,{handler,{app1,{already_exist,default}}}}} =
        rpc:call(Node,logger,add_handlers,[App1Name]),

    %% Add a different handler
    ok = rpc:call(Node,application,set_env,[App1Name,logger,
                                            [{handler,myh,logger_std_h,#{}}]]),
    ok = rpc:call(Node,logger,add_handlers,[App1Name]),

    {ok,#{filters:=DF}} = rpc:call(Node,logger,get_handler_config,[default]),
    {ok,#{filters:=[]}} = rpc:call(Node,logger,get_handler_config,[myh]),

    true = test_server:stop_node(Node),

    %% Start a node with no default handler, then add an own default handler
    {ok,#{handlers:=[#{id:=simple}]},Node} =
        logger_test_lib:setup(Config,[{logger,[{handler,default,undefined}]}]),

    ok = rpc:call(Node,application,load,[App1]),
    ok = rpc:call(Node,application,set_env,
                  [App1Name,logger,[{handler,default,logger_std_h,#{}}]]),
    ok = rpc:call(Node,logger,add_handlers,[App1Name]),

    #{handlers:=[#{id:=default,filters:=DF}]} =
        rpc:call(Node,logger,get_config,[]),

    true = test_server:stop_node(Node),

    %% Start a silent node, then add an own default handler
    {ok,#{handlers:=[]},Node} =
        logger_test_lib:setup(Config,[{error_logger,silent}]),

    {error,{bad_config,{handler,[{some,bad,config}]}}} =
        rpc:call(Node,logger,add_handlers,[[{some,bad,config}]]),
    ok = rpc:call(Node,logger,add_handlers,
                  [[{handler,default,logger_std_h,#{}}]]),

    #{handlers:=[#{id:=default,filters:=DF}]} =
        rpc:call(Node,logger,get_config,[]),

    ok.

%% This test case is maintly to see code coverage. Note that
%% logger_env_var_SUITE tests a lot of the same, and checks the
%% functionality more thoroughly, but since it all happens at node
%% start, it is not possible to see code coverage in that test.
kernel_config(Config) ->
    %% Start a node with simple handler only, then simulate kernel
    %% start by calling internally exported
    %% internal_init_logger(). This is to test all variants of kernel
    %% config, including bad config, and see the code coverage.
    {ok,#{handlers:=[#{id:=simple,filters:=DF}]}=LC,Node} =
        logger_test_lib:setup(Config,[{error_logger,false}]),

    %% Same once more, to get coverage
    ok = rpc:call(Node,logger,internal_init_logger,[]),
    ok = rpc:call(Node,logger,add_handlers,[kernel]),
    LC = rpc:call(Node,logger,get_config,[]),

    %% This shall mean the same as above, but using 'logger' parameter
    %% instead of 'error_logger'
    ok = rpc:call(Node,application,unset_env,[kernel,error_logger]),
    ok = rpc:call(Node,application,set_env,
                  [kernel,logger,[{handler,default,undefined}]]),
    ok = rpc:call(Node,logger,internal_init_logger,[]),
    ok = rpc:call(Node,logger,add_handlers,[kernel]),
    LC = rpc:call(Node,logger,get_config,[]),

    %% Silent
    ok = rpc:call(Node,application,unset_env,[kernel,logger]),
    ok = rpc:call(Node,application,set_env,[kernel,error_logger,silent]),
    ok = rpc:call(Node,logger,internal_init_logger,[]),
    ok = rpc:call(Node,logger,add_handlers,[kernel]),
    #{primary:=#{filter_default:=log,filters:=[]},
      handlers:=[],
      module_levels:=[]} = rpc:call(Node,logger,get_config,[]),

    %% Default
    ok = rpc:call(Node,application,unset_env,[kernel,error_logger]),
    ok = rpc:call(Node,application,unset_env,[kernel,logger]),
    ok = rpc:call(Node,logger,internal_init_logger,[]),
    ok = rpc:call(Node,logger,add_handlers,[kernel]),
    #{primary:=#{filter_default:=log,filters:=[]},
      handlers:=[#{id:=default,filters:=DF,config:=#{type:=standard_io}}],
      module_levels:=[]} = rpc:call(Node,logger,get_config,[]),

    %% error_logger=tty (same as default)
    ok = rpc:call(Node,logger,remove_handler,[default]),% so it can be added again
    ok = rpc:call(Node,application,set_env,[kernel,error_logger,tty]),
    ok = rpc:call(Node,application,unset_env,[kernel,logger]),
    ok = rpc:call(Node,logger,internal_init_logger,[]),
    ok = rpc:call(Node,logger,add_handlers,[kernel]),
    #{primary:=#{filter_default:=log,filters:=[]},
      handlers:=[#{id:=default,filters:=DF,config:=#{type:=standard_io}}],
      module_levels:=[]} = rpc:call(Node,logger,get_config,[]),

    %% error_logger={file,File}
    ok = rpc:call(Node,logger,remove_handler,[default]),% so it can be added again
    F = filename:join(?config(priv_dir,Config),
                      atom_to_list(?FUNCTION_NAME)++".log"),
    ok = rpc:call(Node,application,set_env,[kernel,error_logger,{file,F}]),
    ok = rpc:call(Node,application,unset_env,[kernel,logger]),
    ok = rpc:call(Node,logger,internal_init_logger,[]),
    ok = rpc:call(Node,logger,add_handlers,[kernel]),
    #{primary:=#{filter_default:=log,filters:=[]},
      handlers:=[#{id:=default,filters:=DF,
                   config:=#{type:=file,file:=F,modes:=Modes}}],
      module_levels:=[]} = rpc:call(Node,logger,get_config,[]),
    [append,delayed_write,raw] = lists:sort(Modes),


    %% Same, but using 'logger' parameter instead of 'error_logger'
    ok = rpc:call(Node,logger,remove_handler,[default]),% so it can be added again
    ok = rpc:call(Node,application,unset_env,[kernel,error_logger]),
    ok = rpc:call(Node,application,set_env,[kernel,logger,
                                            [{handler,default,logger_std_h,
                                              #{config=>#{type=>{file,F}}}}]]),
    ok = rpc:call(Node,logger,internal_init_logger,[]),
    ok = rpc:call(Node,logger,add_handlers,[kernel]),
    #{primary:=#{filter_default:=log,filters:=[]},
      handlers:=[#{id:=default,filters:=DF,
                   config:=#{type:=file,file:=F,modes:=Modes}}],
      module_levels:=[]} = rpc:call(Node,logger,get_config,[]),

    %% Same, but with type={file,File,Modes}
    ok = rpc:call(Node,logger,remove_handler,[default]),% so it can be added again
    ok = rpc:call(Node,application,unset_env,[kernel,error_logger]),
    M = [raw,write],
    ok = rpc:call(Node,application,set_env,[kernel,logger,
                                            [{handler,default,logger_std_h,
                                              #{config=>#{type=>{file,F,M}}}}]]),
    ok = rpc:call(Node,logger,internal_init_logger,[]),
    ok = rpc:call(Node,logger,add_handlers,[kernel]),
    #{primary:=#{filter_default:=log,filters:=[]},
      handlers:=[#{id:=default,filters:=DF,
                   config:=#{type:=file,file:=F,modes:=[delayed_write|M]}}],
      module_levels:=[]} = rpc:call(Node,logger,get_config,[]),

    %% Same, but with disk_log handler
    ok = rpc:call(Node,logger,remove_handler,[default]),% so it can be added again
    ok = rpc:call(Node,application,unset_env,[kernel,error_logger]),
    ok = rpc:call(Node,application,set_env,[kernel,logger,
                                            [{handler,default,logger_disk_log_h,
                                              #{config=>#{file=>F}}}]]),
    ok = rpc:call(Node,logger,internal_init_logger,[]),
    ok = rpc:call(Node,logger,add_handlers,[kernel]),
    #{primary:=#{filter_default:=log,filters:=[]},
      handlers:=[#{id:=default,filters:=DF,config:=#{file:=F}}],
      module_levels:=[]} = rpc:call(Node,logger,get_config,[]),

    %% Set primary filters and module level. No default handler.
    ok = rpc:call(Node,logger,remove_handler,[default]),% so it can be added again
    ok = rpc:call(Node,application,unset_env,[kernel,error_logger]),
    ok = rpc:call(Node,application,set_env,
                  [kernel,logger,[{handler,default,undefined},
                                  {filters,stop,[{f1,{fun(_,_) -> log end,ok}}]},
                                  {module_level,debug,[?MODULE]}]]),
    ok = rpc:call(Node,logger,internal_init_logger,[]),
    ok = rpc:call(Node,logger,add_handlers,[kernel]),
    #{primary:=#{filter_default:=stop,filters:=[_]},
      handlers:=[],
      module_levels:=[{?MODULE,debug}]} = rpc:call(Node,logger,get_config,[]),

    %% Bad config
    ok = rpc:call(Node,application,unset_env,[kernel,logger]),

    ok = rpc:call(Node,application,set_env,[kernel,error_logger,bad]),
    {error,{bad_config,{kernel,{error_logger,bad}}}} =
        rpc:call(Node,logger,internal_init_logger,[]),

    ok = rpc:call(Node,application,unset_env,[kernel,error_logger]),
    ok = rpc:call(Node,application,set_env,[kernel,logger_level,bad]),
    {error,{bad_config,{kernel,{logger_level,bad}}}} =
        rpc:call(Node,logger,internal_init_logger,[]),

    ok = rpc:call(Node,application,unset_env,[kernel,logger_level]),
    ok = rpc:call(Node,application,set_env,
                  [kernel,logger,[{filters,stop,[bad]}]]),
    {error,{bad_config,{kernel,{invalid_filters,[bad]}}}} =
        rpc:call(Node,logger,internal_init_logger,[]),

    ok = rpc:call(Node,application,set_env,
                  [kernel,logger,[{filters,stop,[bad]}]]),
    {error,{bad_config,{kernel,{invalid_filters,[bad]}}}} =
        rpc:call(Node,logger,internal_init_logger,[]),

    ok = rpc:call(Node,application,set_env,
                  [kernel,logger,[{filters,stop,[{f1,bad}]}]]),
    {error,{bad_config,{kernel,{invalid_filter,{f1,bad}}}}} =
        rpc:call(Node,logger,internal_init_logger,[]),

    ok = rpc:call(Node,application,set_env,
                  [kernel,logger,MF=[{filters,stop,[]},{filters,log,[]}]]),
    {error,{bad_config,{kernel,{multiple_filters,MF}}}} =
        rpc:call(Node,logger,internal_init_logger,[]),

    ok = rpc:call(Node,application,set_env,
                  [kernel,logger,[{module_level,bad,[?MODULE]}]]),
    {error,{bad_config,{kernel,{invalid_level,bad}}}} =
        rpc:call(Node,logger,internal_init_logger,[]),

    ok.

pretty_print(Config) ->
    ok = logger:add_handler(?FUNCTION_NAME,logger_std_h,#{}),
    ok = logger:set_module_level([module1,module2],debug),

    ct:capture_start(),
    logger:i(),
    ct:capture_stop(),
    I0 = ct:capture_get(),

    ct:capture_start(),
    logger:i(primary),
    ct:capture_stop(),
    IPrim = ct:capture_get(),

    ct:capture_start(),
    logger:i(handlers),
    ct:capture_stop(),
    IHs = ct:capture_get(),

    ct:capture_start(),
    logger:i(proxy),
    ct:capture_stop(),
    IProxy = ct:capture_get(),

    ct:capture_start(),
    logger:i(modules),
    ct:capture_stop(),
    IMs = ct:capture_get(),

    I02 = lists:append([IPrim,IHs,IProxy,IMs]),
    %% ct:log("~p~n",[I0]),
    %% ct:log("~p~n",[I02]),
    I0 = I02,

    ct:capture_start(),
    logger:i(handlers),
    ct:capture_stop(),
    IHs = ct:capture_get(),

    Ids = logger:get_handler_ids(),
    IHs2 =
        lists:append(
          [begin
               ct:capture_start(),
               logger:i(Id),
               ct:capture_stop(),
               [_|IH] = ct:capture_get(),
               IH
           end || Id <- Ids]),

    %% ct:log("~p~n",[IHs]),
    %% ct:log("~p~n",[["Handler configuration: \n"|IHs2]]),
    IHs = ["Handler configuration: \n"|IHs2],
    ok.

%%%-----------------------------------------------------------------
%%% Internal
check_logged(Level,Format,Args,Meta) ->
    do_check_logged(Level,{Format,Args},Meta).

check_logged(Level,Msg,Meta) when ?IS_REPORT(Msg) ->
    do_check_logged(Level,{report,Msg},Meta);
check_logged(Level,Msg,Meta) when ?IS_STRING(Msg) ->
    do_check_logged(Level,{string,Msg},Meta).

do_check_logged(Level,Msg0,Meta0) ->
    receive
        {#{level:=Level,msg:=Msg,meta:=Meta},_} ->
            check_msg(Msg0,Msg),
            check_maps(Meta0,Meta,meta)
    after 500 ->
            ct:fail({timeout,no_log,process_info(self(),messages)})
    end.

check_no_log() ->
    receive
        X -> ct:fail({got_unexpected_log,X})
    after 500 ->
            ok
    end.

check_msg(Msg,Msg) ->
    ok;
check_msg({report,Expected},{report,Got}) when is_map(Expected), is_map(Got) ->
    check_maps(Expected,Got,msg);
check_msg(Expected,Got) ->
    ct:fail({unexpected,msg,Expected,Got}).

check_maps(Expected,Got,What) ->
    case maps:merge(Got,Expected) of
        Got ->
            ok;
        _ ->
            ct:fail({unexpected,What,Expected,Got})
    end.

%% Handler
adding_handler(#{add_call:=Fun}) ->
    Fun();
adding_handler(Config) ->
    maybe_send(add),
    {ok,Config}.

removing_handler(#{rem_call:=Fun}) ->
    Fun();
removing_handler(_Config) ->
    maybe_send(remove),
    ok.
changing_config(_Old,#{conf_call:=Fun}) ->
    Fun();
changing_config(_Old,Config) ->
    maybe_send(changing_config),
    {ok,Config}.

maybe_send(Msg) ->
    case whereis(callback_receiver) of
        undefined -> ok;
        Pid -> Pid ! Msg
    end.

log(_Log,#{log_call:=Fun}) ->
    Fun();
log(Log,Config) ->
    TcProc  = maps:get(tc_proc,Config,self()),
    TcProc ! {Log,Config},
    ok.

test_api(Level) ->
    logger:Level(#{Level=>rep}),
    ok = check_logged(Level,#{Level=>rep},#{}),
    logger:Level(#{Level=>rep},#{my=>meta}),
    ok = check_logged(Level,#{Level=>rep},#{my=>meta}),
    logger:Level("~w: ~w",[Level,fa]),
    ok = check_logged(Level,"~w: ~w",[Level,fa],#{}),
    logger:Level("~w: ~w ~w",[Level,fa,meta],#{my=>meta}),
    ok = check_logged(Level,"~w: ~w ~w",[Level,fa,meta],#{my=>meta}),
    logger:Level(fun(x) -> {"~w: ~w ~w",[Level,fun_to_fa,meta]} end,x,
                 #{my=>meta}),
    ok = check_logged(Level,"~w: ~w ~w",[Level,fun_to_fa,meta],#{my=>meta}),
    logger:Level(fun(x) -> #{Level=>fun_to_r,meta=>true} end,x,
                     #{my=>meta}),
    ok = check_logged(Level,#{Level=>fun_to_r,meta=>true},#{my=>meta}),
    logger:Level(fun(x) -> <<"fun_to_s">> end,x,#{}),
    ok = check_logged(Level,<<"fun_to_s">>,#{}),
    logger:Level(F1=fun(x) -> {fun_to_bad} end,x,#{}),
    ok = check_logged(Level,"LAZY_FUN ERROR: ~tp; Returned: ~tp",
                      [{F1,x},{fun_to_bad}],#{}),
    logger:Level(F2=fun(x) -> erlang:error(fun_that_crashes) end,x,#{}),
    ok = check_logged(Level,"LAZY_FUN CRASH: ~tp; Reason: ~tp",
                      [{F2,x},{error,fun_that_crashes}],#{}),
    ok.

test_log_function(Level) ->
    logger:log(Level,#{Level=>rep}),
    ok = check_logged(Level,#{Level=>rep},#{}),
    logger:log(Level,#{Level=>rep},#{my=>meta}),
    ok = check_logged(Level,#{Level=>rep},#{my=>meta}),
    logger:log(Level,"~w: ~w",[Level,fa]),
    ok = check_logged(Level,"~w: ~w",[Level,fa],#{}),
    logger:log(Level,"~w: ~w ~w",[Level,fa,meta],#{my=>meta}),
    ok = check_logged(Level,"~w: ~w ~w",[Level,fa,meta],#{my=>meta}),
    logger:log(Level,fun(x) -> {"~w: ~w ~w",[Level,fun_to_fa,meta]} end,
               x, #{my=>meta}),
    ok = check_logged(Level,"~w: ~w ~w",[Level,fun_to_fa,meta],#{my=>meta}),
    logger:log(Level,fun(x) -> #{Level=>fun_to_r,meta=>true} end,
               x, #{my=>meta}),
    ok = check_logged(Level,#{Level=>fun_to_r,meta=>true},#{my=>meta}),
    logger:log(Level,fun(x) -> <<"fun_to_s">> end,x,#{}),
    ok = check_logged(Level,<<"fun_to_s">>,#{}),
    logger:log(Level,F1=fun(x) -> {fun_to_bad} end,x,#{}),
    ok = check_logged(Level,"LAZY_FUN ERROR: ~tp; Returned: ~tp",
                      [{F1,x},{fun_to_bad}],#{}),
    logger:log(Level,F2=fun(x) -> erlang:error(fun_that_crashes) end,x,#{}),
    ok = check_logged(Level,"LAZY_FUN CRASH: ~tp; Reason: ~tp",
                      [{F2,x},{error,fun_that_crashes}],#{}),
    ok.

test_macros(emergency=Level) ->
    ?LOG_EMERGENCY(#{Level=>rep}),
    ok = check_logged(Level,#{Level=>rep},?MY_LOC(1)),
    ?LOG_EMERGENCY(#{Level=>rep},#{my=>meta}),
    ok = check_logged(Level,#{Level=>rep},(?MY_LOC(1))#{my=>meta}),
    ?LOG_EMERGENCY("~w: ~w",[Level,fa]),
    ok = check_logged(Level,"~w: ~w",[Level,fa],?MY_LOC(1)),
    ?LOG_EMERGENCY("~w: ~w ~w",[Level,fa,meta],#{my=>meta}),
    ok = check_logged(Level,"~w: ~w ~w",[Level,fa,meta],(?MY_LOC(1))#{my=>meta}),
    ?LOG_EMERGENCY(fun(x) -> {"~w: ~w ~w",[Level,fun_to_fa,meta]} end,
                   x, #{my=>meta}),
    ok = check_logged(Level,"~w: ~w ~w",[Level,fun_to_fa,meta],
                      (?MY_LOC(3))#{my=>meta}),
    ?LOG_EMERGENCY(fun(x) -> #{Level=>fun_to_r,meta=>true} end, x, #{my=>meta}),
    ok = check_logged(Level,#{Level=>fun_to_r,meta=>true},
                      (?MY_LOC(2))#{my=>meta}),
    ?LOG_EMERGENCY(fun(x) -> <<"fun_to_s">> end,x,#{}),
    ok = check_logged(Level,<<"fun_to_s">>,?MY_LOC(1)),
    F1=fun(x) -> {fun_to_bad} end,
    ?LOG_EMERGENCY(F1,x,#{}),
    ok = check_logged(Level,"LAZY_FUN ERROR: ~tp; Returned: ~tp",
                      [{F1,x},{fun_to_bad}],#{}),
    F2=fun(x) -> erlang:error(fun_that_crashes) end,
    ?LOG_EMERGENCY(F2,x,#{}),
    ok = check_logged(Level,"LAZY_FUN CRASH: ~tp; Reason: ~tp",
                      [{F2,x},{error,fun_that_crashes}],#{}),
    ok.

test_log_macro(Level) ->
    ?LOG(Level,#{Level=>rep}),
    ok = check_logged(Level,#{Level=>rep},?MY_LOC(1)),
    ?LOG(Level,#{Level=>rep},#{my=>meta}),
    ok = check_logged(Level,#{Level=>rep},(?MY_LOC(1))#{my=>meta}),
    ?LOG(Level,"~w: ~w",[Level,fa]),
    ok = check_logged(Level,"~w: ~w",[Level,fa],?MY_LOC(1)),
    ?LOG(Level,"~w: ~w ~w",[Level,fa,meta],#{my=>meta}),
    ok = check_logged(Level,"~w: ~w ~w",[Level,fa,meta],(?MY_LOC(1))#{my=>meta}),
    ?LOG(Level,fun(x) -> {"~w: ~w ~w",[Level,fun_to_fa,meta]} end,
                   x, #{my=>meta}),
    ok = check_logged(Level,"~w: ~w ~w",[Level,fun_to_fa,meta],
                      (?MY_LOC(3))#{my=>meta}),
    ?LOG(Level,fun(x) -> #{Level=>fun_to_r,meta=>true} end, x, #{my=>meta}),
    ok = check_logged(Level,#{Level=>fun_to_r,meta=>true},
                      (?MY_LOC(2))#{my=>meta}),
    ?LOG(Level,fun(x) -> <<"fun_to_s">> end,x,#{}),
    ok = check_logged(Level,<<"fun_to_s">>,?MY_LOC(1)),
    F1=fun(x) -> {fun_to_bad} end,
    ?LOG(Level,F1,x,#{}),
    ok = check_logged(Level,"LAZY_FUN ERROR: ~tp; Returned: ~tp",
                      [{F1,x},{fun_to_bad}],#{}),
    F2=fun(x) -> erlang:error(fun_that_crashes) end,
    ?LOG(Level,F2,x,#{}),
    ok = check_logged(Level,"LAZY_FUN CRASH: ~tp; Reason: ~tp",
                      [{F2,x},{error,fun_that_crashes}],#{}),
    ok.

%%%-----------------------------------------------------------------
%%% Called by macro ?TRY(X)
my_try(Fun) ->
    try Fun() catch C:R -> {C,R} end.

check_config(crash) ->
    erlang:error({badmatch,3});
check_config(_) ->
    ok.
