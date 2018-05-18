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
    [{timetrap,{seconds,30}}].

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
        {HMod,HConfig} ->
            ok = logger:add_handler(?STANDARD_HANDLER,HMod,HConfig);
        _ ->
            ok
    end.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    {ok,LC} = logger:get_logger_config(),
    [{logger_config,LC}|Config].

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
     log_all_levels_api,
     macros,
     set_level,
     set_level_module,
     cache_level_module,
     format_report,
     filter_failed,
     handler_failed,
     config_sanity_check,
     log_failed,
     emulator,
     via_logger_process,
     other_node,
     compare_levels,
     process_metadata].

start_stop(_Config) ->
    S = whereis(logger),
    true = is_pid(S),
    ok.

add_remove_handler(_Config) ->
    register(callback_receiver,self()),
    #{handlers:=Hs0} = logger:i(),
    {error,{not_found,h1}} = logger:get_handler_config(h1),
    ok = logger:add_handler(h1,?MODULE,#{}),
    [add] = test_server:messages_get(),
    #{handlers:=Hs} = logger:i(),
    {value,_,Hs0} = lists:keytake(h1,1,Hs),
    {ok,{?MODULE,#{level:=info,filters:=[],filter_default:=log}}} = % defaults
        logger:get_handler_config(h1),
    ok = logger:set_handler_config(h1,filter_default,stop),
    [changing_config] = test_server:messages_get(),
    ?LOG_INFO("hello",[]),
    ok = check_no_log(),
    ok = logger:set_handler_config(h1,filter_default,log),
    [changing_config] = test_server:messages_get(),
    {ok,{?MODULE,#{filter_default:=log}}} = logger:get_handler_config(h1),
    ?LOG_INFO("hello",[]),
    ok = check_logged(info,"hello",[],?MY_LOC(1)),
    ok = logger:remove_handler(h1),
    [remove] = test_server:messages_get(),
    #{handlers:=Hs0} = logger:i(),
    {error,{not_found,h1}} = logger:get_handler_config(h1),
    {error,{not_found,h1}} = logger:remove_handler(h1),
    logger:info("hello",[]),
    ok = check_no_log(),
    ok.

add_remove_handler(cleanup,_Config) ->
    logger:remove_handler(h1),
    ok.

multiple_handlers(_Config) ->
    ok = logger:add_handler(h1,?MODULE,#{level=>info,filter_default=>log}),
    ok = logger:add_handler(h2,?MODULE,#{level=>error,filter_default=>log}),
    ?LOG_ERROR("hello",[]),
    ok = check_logged(error,"hello",[],?MY_LOC(1)),
    ok = check_logged(error,"hello",[],?MY_LOC(2)),
    ?LOG_INFO("hello",[]),
    ok = check_logged(info,"hello",[],?MY_LOC(1)),
    ok = check_no_log(),
    ok.

multiple_handlers(cleanup,_Config) ->
    logger:remove_handler(h1),
    logger:remove_handler(h2),
    ok.

add_remove_filter(_Config) ->
    ok = logger:add_handler(h1,?MODULE,#{level=>info,filter_default=>log}),
    LF = {fun(Log,_) -> Log#{level=>error} end, []},
    ok = logger:add_logger_filter(lf,LF),
    {error,{already_exist,lf}} = logger:add_logger_filter(lf,LF),
    {error,{already_exist,lf}} = logger:add_logger_filter(lf,{fun(Log,_) ->
                                                                      Log
                                                              end, []}),
    ?LOG_INFO("hello",[]),
    ok = check_logged(error,"hello",[],?MY_LOC(1)),
    ok = check_no_log(),

    ok = logger:add_handler(h2,?MODULE,#{level=>info,filter_default=>log}),
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
    ?LOG_INFO("hello",[]),
    ok = check_logged(mylevel,"hello",[],?MY_LOC(1)),
    ok = check_logged(error,"hello",[],?MY_LOC(2)),

    ok = logger:remove_logger_filter(lf),
    {error,{not_found,lf}} = logger:remove_logger_filter(lf),

    ?LOG_INFO("hello",[]),
    ok = check_logged(info,"hello",[],?MY_LOC(1)),
    ok = check_logged(info,"hello",[],?MY_LOC(2)),

    ?LOG_ERROR("hello",[]),
    ok = check_logged(mylevel,"hello",[],?MY_LOC(1)),
    ok = check_logged(error,"hello",[],?MY_LOC(2)),

    ok = logger:remove_handler_filter(h1,hf),
    {error,{not_found,hf}} = logger:remove_handler_filter(h1,hf),
    ?LOG_INFO("hello",[]),
    ok = check_logged(info,"hello",[],?MY_LOC(1)),
    ok = check_logged(info,"hello",[],?MY_LOC(2)),

    ?LOG_ERROR("hello",[]),
    ok = check_logged(error,"hello",[],?MY_LOC(1)),
    ok = check_logged(error,"hello",[],?MY_LOC(2)),
    ok.

add_remove_filter(cleanup,_Config) ->
    logger:remove_logger_filter(lf),
    logger:remove_handler(h1),
    logger:remove_handler(h2),
    ok.

change_config(_Config) ->
    %% Overwrite handler config - check that defaults are added
    ok = logger:add_handler(h1,?MODULE,#{level=>debug,custom=>custom}),
    {ok,{?MODULE,#{level:=debug,filter_default:=log,custom:=custom}}} =
        logger:get_handler_config(h1),
    register(callback_receiver,self()),
    ok = logger:set_handler_config(h1,#{filter_default=>stop}),
    [changing_config] = test_server:messages_get(),
    {ok,{?MODULE,#{level:=info,filter_default:=stop}=C2}} =
        logger:get_handler_config(h1),
    false = maps:is_key(custom,C2),
    {error,fail} = logger:set_handler_config(h1,#{conf_call=>fun() -> {error,fail} end}),
    {error,{attempting_syncronous_call_to_self,_}} =
        logger:set_handler_config(
          h1,#{conf_call=>fun() -> logger:set_handler_config(?MODULE,#{}) end}),
    ok =
        logger:set_handler_config(
          h1,#{conf_call=>fun() -> logger:set_module_level(?MODULE,debug) end}),
    {ok,{?MODULE,C2}} = logger:get_handler_config(h1),

    %% Change handler config: Single key
    {error,fail} = logger:set_handler_config(h1,conf_call,fun() -> {error,fail} end),
    ok = logger:set_handler_config(h1,custom,custom),
    [changing_config] = test_server:messages_get(),
    {ok,{?MODULE,#{custom:=custom}=C3}} = logger:get_handler_config(h1),
    C2 = maps:remove(custom,C3),

    %% Change handler config: Map
    ok = logger:update_handler_config(h1,#{custom=>new_custom}),
    [changing_config] = test_server:messages_get(),
    {ok,{_,C4}} = logger:get_handler_config(h1),
    C4 = C3#{custom:=new_custom},

    %% Change logger config: Single key
    {ok,LConfig0} = logger:get_logger_config(),
    ok = logger:set_logger_config(level,warning),
    {ok,LConfig1} = logger:get_logger_config(),
    LConfig1 = LConfig0#{level:=warning},

    %% Change logger config: Map
    ok = logger:update_logger_config(#{level=>error}),
    {ok,LConfig2} = logger:get_logger_config(),
    LConfig2 = LConfig1#{level:=error},

    %% Overwrite logger config - check that defaults are added
    ok = logger:set_logger_config(#{filter_default=>stop}),
    {ok,#{level:=info,filters:=[],filter_default:=stop}=LC1} =
        logger:get_logger_config(),
    3 = maps:size(LC1),
    %% Check that internal 'handlers' field has not been changed
    #{handlers:=HCs} = logger:i(),
    HIds1 = [Id || {Id,_,_} <- HCs],
    {ok,#{handlers:=HIds2}} = logger_config:get(?LOGGER_TABLE,logger),
    HIds1 = lists:sort(HIds2),

    %% Cleanup
    ok = logger:set_logger_config(LConfig0),
    [] = test_server:messages_get(),

    ok.

change_config(cleanup,Config) ->
    logger:remove_handler(h1),
    LC = ?config(logger_config,Config),
    logger:set_logger_config(LC),
    ok.

set_formatter(_Config) ->
    {error,{not_found,h1}}=logger:set_handler_config(h1,formatter,{?MODULE,[]}),
    ok = logger:add_handler(h1,?MODULE,#{level=>info,filter_default=>log}),
    ok = logger:set_handler_config(h1,formatter,{?MODULE,[]}),
    logger:info("hello",[]),
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

log_all_levels_api(_Config) ->
    ok = logger:set_logger_config(level,debug),
    ok = logger:add_handler(h1,?MODULE,#{level=>debug,filter_default=>log}),
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
    logger:set_logger_config(level,info),
    ok.

macros(_Config) ->
    ok = logger:set_module_level(?MODULE,debug),
    ok = logger:add_handler(h1,?MODULE,#{level=>debug,filter_default=>log}),
    test_macros(emergency),
    ok.

macros(cleanup,_Config) ->
    logger:remove_handler(h1),
    logger:unset_module_level(?MODULE),
    ok.

set_level(_Config) ->
    ok = logger:add_handler(h1,?MODULE,#{level=>debug,filter_default=>log}),
    logger:debug(?map_rep),
    ok = check_no_log(),
    logger:info(M1=?map_rep),
    ok = check_logged(info,M1,#{}),
    ok = logger:set_logger_config(level,debug),
    logger:debug(M2=?map_rep),
    ok = check_logged(debug,M2,#{}),
    ok.

set_level(cleanup,_Config) ->
    logger:remove_handler(h1),
    logger:set_logger_config(level,info),
    ok.

set_level_module(_Config) ->
    ok = logger:add_handler(h1,?MODULE,#{level=>info,filter_default=>log}),
    {error,{invalid_level,bad}} = logger:set_module_level(?MODULE,bad),
    {error,{not_a_module,{bad}}} = logger:set_module_level({bad},warning),
    ok = logger:set_module_level(?MODULE,warning),
    logger:info(?map_rep,?MY_LOC(0)),
    ok = check_no_log(),
    logger:warning(M1=?map_rep,?MY_LOC(0)),
    ok = check_logged(warning,M1,?MY_LOC(1)),
    ok = logger:set_module_level(?MODULE,info),
    logger:info(M2=?map_rep,?MY_LOC(0)),
    ok = check_logged(info,M2,?MY_LOC(1)),

    {error,{not_a_module,{bad}}} = logger:unset_module_level({bad}),
    ok = logger:unset_module_level(?MODULE),

    ok.

set_level_module(cleanup,_Config) ->
    logger:remove_handler(h1),
    logger:unset_module_level(?MODULE),
    ok.

cache_level_module(_Config) ->
    ok = logger:unset_module_level(?MODULE),
    [] = ets:lookup(logger,?MODULE), %dirty - add API in logger_config?
    ?LOG_INFO(?map_rep),
    %% Caching is done asynchronously, so wait a bit for the update
    timer:sleep(100),
    [_] = ets:lookup(logger,?MODULE), %dirty - add API in logger_config?
    ok = logger:unset_module_level(?MODULE),
    [] = ets:lookup(logger,?MODULE), %dirty - add API in logger_config?
    ok.

cache_level_module(cleanup,_Config) ->
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
    ok = logger:add_handler(h1,?MODULE,#{level=>info,filter_default=>log}),

    %% Logger filters
    {error,{invalid_filter,_}} =
        logger:add_logger_filter(lf,{fun(_) -> ok end,args}),
    ok = logger:add_logger_filter(lf,{fun(_,_) -> a=b end,args}),
    {ok,#{filters:=[_]}} = logger:get_logger_config(),
    ok = logger:info(M1=?map_rep),
    ok = check_logged(info,M1,#{}),
    {error,{not_found,lf}} = logger:remove_logger_filter(lf),

    ok = logger:add_logger_filter(lf,{fun(_,_) -> faulty_return end,args}),
    {ok,#{filters:=[_]}} = logger:get_logger_config(),
    ok = logger:info(M2=?map_rep),
    ok = check_logged(info,M2,#{}),
    {error,{not_found,lf}} = logger:remove_logger_filter(lf),

    %% Handler filters
    {error,{not_found,h0}} =
        logger:add_handler_filter(h0,hf,{fun(_,_) -> ignore end,args}),
    {error,{not_found,h0}} = logger:remove_handler_filter(h0,hf),
    {error,{invalid_filter,_}} =
        logger:add_handler_filter(h1,hf,{fun(_) -> ok end,args}),
    ok = logger:add_handler_filter(h1,hf,{fun(_,_) -> a=b end,args}),
    {ok,{?MODULE,#{filters:=[_]}}} = logger:get_handler_config(h1),
    ok = logger:info(M3=?map_rep),
    ok = check_logged(info,M3,#{}),
    {error,{not_found,hf}} = logger:remove_handler_filter(h1,hf),

    ok = logger:add_handler_filter(h1,hf,{fun(_,_) -> faulty_return end,args}),
    {ok,{?MODULE,#{filters:=[_]}}} = logger:get_handler_config(h1),
    ok = logger:info(M4=?map_rep),
    ok = check_logged(info,M4,#{}),
    {error,{not_found,hf}} = logger:remove_handler_filter(h1,hf),

    ok.

filter_failed(cleanup,_Config) ->
    logger:remove_handler(h1),
    ok.

handler_failed(_Config) ->
    register(callback_receiver,self()),
    {error,{invalid_id,1}} = logger:add_handler(1,?MODULE,#{}),
    {error,{invalid_module,"nomodule"}} = logger:add_handler(h1,"nomodule",#{}),
    {error,{invalid_handler_config,bad}} = logger:add_handler(h1,?MODULE,bad),
    {error,{invalid_filters,false}} =
        logger:add_handler(h1,?MODULE,#{filters=>false}),
    {error,{invalid_filter_default,true}} =
        logger:add_handler(h1,?MODULE,#{filter_default=>true}),
    {error,{invalid_formatter,[]}} =
        logger:add_handler(h1,?MODULE,#{formatter=>[]}),
    {error,{invalid_handler,_}} = logger:add_handler(h1,nomodule,#{filter_default=>log}),
    logger:info(?map_rep),
    check_no_log(),
    #{handlers:=H1} = logger:i(),
    false = lists:keymember(h1,1,H1),
    {error,{not_found,h1}} = logger:remove_handler(h1),

    ok = logger:add_handler(h2,?MODULE,#{filter_default=>log,log_call=>fun() -> a = b end}),
    {error,{already_exist,h2}} = logger:add_handler(h2,othermodule,#{}),
    [add] = test_server:messages_get(),

    logger:info(?map_rep),
    [remove] = test_server:messages_get(),
    #{handlers:=H2} = logger:i(),
    false = lists:keymember(h2,1,H2),
    {error,{not_found,h2}} = logger:remove_handler(h2),

    CallAddHandler = fun() -> logger:add_handler(h2,?MODULE,#{}) end,
    CrashHandler = fun() -> a = b end,
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
    %% Logger config
    {error,{invalid_filter_default,bad}} =
        logger:set_logger_config(filter_default,bad),
    {error,{invalid_level,bad}} = logger:set_logger_config(level,bad),
    {error,{invalid_filters,bad}} = logger:set_logger_config(filters,bad),
    {error,{invalid_filter,bad}} = logger:set_logger_config(filters,[bad]),
    {error,{invalid_filter,{_,_}}} =
        logger:set_logger_config(filters,[{id,bad}]),
    {error,{invalid_filter,{_,{_,_}}}} =
        logger:set_logger_config(filters,[{id,{bad,args}}]),
    {error,{invalid_filter,{_,{_,_}}}} =
        logger:set_logger_config(filters,[{id,{fun() -> ok end,args}}]),
    {error,{invalid_logger_config,{bad,bad}}} =
        logger:set_logger_config(bad,bad),

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
    {error,{invalid_formatter_config,logger_formatter,{template,bad}}} =
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
    ok = logger:add_handler(h1,?MODULE,#{level=>info,filter_default=>log}),
    {error,function_clause} = ?TRY(logger:log(bad,?map_rep)),
    {error,function_clause} = ?TRY(logger:log(info,?map_rep,bad)),
    {error,function_clause} = ?TRY(logger:log(info,fun() -> ?map_rep end,bad)),
    {error,function_clause} = ?TRY(logger:log(info,fun() -> ?map_rep end,bad,#{})),
    {error,function_clause} = ?TRY(logger:log(info,bad,bad,bad)),
    {error,function_clause} = ?TRY(logger:log(info,bad,bad,#{})),
    check_no_log(),
    ok = logger:log(info,M1=?str,#{}),
    check_logged(info,M1,#{}),
    ok = logger:log(info,M2=?map_rep,#{}),
    check_logged(info,M2,#{}),
    ok = logger:log(info,M3=?keyval_rep,#{}),
    check_logged(info,M3,#{}),

    %% Should we check report input more thoroughly?
    ok = logger:log(info,M4=?keyval_rep++[other,stuff,in,list],#{}),
    check_logged(info,M4,#{}),

    %% This might break a handler since it is assumed to be a format
    %% string and args, so it depends how the handler protects itself
    %% against something like io_lib:format("ok","ok")
    ok = logger:log(info,"ok","ok",#{}),
    check_logged(info,"ok","ok",#{}),

    ok.

log_failed(cleanup,_Config) ->
    logger:remove_handler(h1),
    ok.

emulator(_Config) ->
    ok = logger:add_handler(h1,?MODULE,#{level=>info,filter_default=>log,
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
    ok = logger:add_handler(h1,?MODULE,#{level=>info,filter_default=>log,
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
    ok = logger:add_handler(h1,?MODULE,#{level=>info,filter_default=>log,
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
    Levels = [emergency,alert,critical,error,warning,notice,info,debug],
    ok = compare(Levels),
    {error,badarg} = ?TRY(logger:compare_levels(bad,bad)),
    {error,badarg} = ?TRY(logger:compare_levels({bad},info)),
    {error,badarg} = ?TRY(logger:compare_levels(info,"bad")),
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
    ok = logger:add_handler(h1,?MODULE,#{level=>info,filter_default=>log}),
    Time = erlang:system_time(microsecond),
    ProcMeta = #{time=>Time,line=>0,custom=>proc},
    ok = logger:set_process_metadata(ProcMeta),
    S1 = ?str,
    ?LOG_INFO(S1,#{custom=>macro}),
    check_logged(info,S1,#{time=>Time,line=>0,custom=>macro}),

    Time2 = erlang:system_time(microsecond),
    S2 = ?str,
    ?LOG_INFO(S2,#{time=>Time2,line=>1,custom=>macro}),
    check_logged(info,S2,#{time=>Time2,line=>1,custom=>macro}),

    logger:info(S3=?str,#{custom=>func}),
    check_logged(info,S3,#{time=>Time,line=>0,custom=>func}),

    ProcMeta = logger:get_process_metadata(),
    ok = logger:update_process_metadata(#{custom=>changed,custom2=>added}),
    Expected = ProcMeta#{custom:=changed,custom2=>added},
    Expected = logger:get_process_metadata(),
    ok = logger:unset_process_metadata(),
    undefined = logger:get_process_metadata(),

    ok.

process_metadata(cleanup,_Config) ->
    logger:remove_handler(h1),
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
adding_handler(_Id,#{add_call:=Fun}) ->
    Fun();
adding_handler(_Id,Config) ->
    maybe_send(add),
    {ok,Config}.

removing_handler(_Id,#{rem_call:=Fun}) ->
    Fun();
removing_handler(_Id,_Config) ->
    maybe_send(remove),
    ok.
changing_config(_Id,_Old,#{conf_call:=Fun}) ->
    Fun();
changing_config(_Id,_Old,Config) ->
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

%%%-----------------------------------------------------------------
%%% Called by macro ?TRY(X)
my_try(Fun) ->
    try Fun() catch C:R -> {C,R} end.

check_config(crash) ->
    erlang:error({badmatch,3});
check_config(_) ->
    ok.
