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
-module(logger_simple_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/src/logger_internal.hrl").

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
    #{handlers:=Hs0} = logger:i(),
    Hs = lists:keydelete(cth_log_redirect,1,Hs0),
    [ok = logger:remove_handler(Id) || {Id,_,_} <- Hs],
    Env = [{App,Key,application:get_env(App,Key)} ||
              {App,Key} <- [{kernel,logger_dest},
                            {kernel,logger_level}]],
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
     get_buffer,
     replace_file,
     replace_disk_log
    ].

start_stop(_Config) ->
    undefined = whereis(logger_simple),
    register(logger_simple,self()),
    {error,_} = logger:add_handler(logger_simple,
                                   logger_simple,
                                   #{filter_default=>log}),    
    unregister(logger_simple),
    ok = logger:add_handler(logger_simple,logger_simple,#{filter_default=>log}),
    Pid = whereis(logger_simple),
    true = is_pid(Pid),
    ok = logger:remove_handler(logger_simple),
    false = is_pid(whereis(logger_simple)),
    ok.
start_stop(cleanup,_Config) ->
    logger:remove_handler(logger_simple).

get_buffer(_Config) ->
    %% Start simple without buffer
    ok = logger:add_handler(logger_simple,logger_simple,
                            #{filter_default=>log}), 
    logger:emergency(?str),
    logger:alert(?str,[]),
    logger:error(?map_rep),
    logger:info(?keyval_rep),
    {ok,[]} = logger_simple:get_buffer(), % no buffer
    ok = logger:remove_handler(logger_simple),

    %% Start with buffer
    ok = logger:add_handler(logger_simple,logger_simple,
                            #{filter_default=>log,
                              logger_simple=>#{buffer=>true}}),
    logger:emergency(M1=?str),
    logger:alert(M2=?str,[]),
    logger:error(M3=?map_rep),
    logger:info(M4=?keyval_rep),
    logger:info(M41=?keyval_rep++[not_key_val]),
    error_logger:error_report(some_type,M5=?map_rep),
    error_logger:warning_report("some_type",M6=?map_rep),
    logger:critical(M7=?str,[A7=?keyval_rep]),
    logger:notice(M8=["fake",string,"line:",?LINE]),
    {ok,Buffered1} = logger_simple:get_buffer(),
    [#{level:=emergency,msg:={string,M1}},
     #{level:=alert,msg:={M2,[]}},
     #{level:=error,msg:={report,M3}},
     #{level:=info,msg:={report,M4}},
     #{level:=info,msg:={report,M41}},
     #{level:=error,msg:={report,#{label:={error_logger,error_report},
                                   report:=M5}}},
     #{level:=warning,msg:={report,#{label:={error_logger,warning_report},
                                   report:=M6}}},
     #{level:=critical,msg:={M7,[A7]}},
     #{level:=notice,msg:={string,M8}}] = Buffered1,

    %% Keep logging - should not buffer any more
    logger:emergency(?str),
    logger:alert(?str,[]),
    logger:error(?map_rep),
    logger:info(?keyval_rep),
    {ok,[]} = logger_simple:get_buffer(),
    ok = logger:remove_handler(logger_simple),

    %% Fill buffer and drop
    ok = logger:add_handler(logger_simple,logger_simple,
                            #{filter_default=>log,
                              logger_simple=>#{buffer=>true}}),
    logger:emergency(M9=?str),
    M10=?str,
    [logger:info(M10) || _ <- lists:seq(1,8)],
    logger:error(M11=?str),
    logger:error(?str),
    logger:error(?str),
    {ok,Buffered3} = logger_simple:get_buffer(),
    11 = length(Buffered3),
    [#{level:=emergency,msg:={string,M9}},
     #{level:=info,msg:={string,M10}},
     #{level:=info,msg:={string,M10}},
     #{level:=info,msg:={string,M10}},
     #{level:=info,msg:={string,M10}},
     #{level:=info,msg:={string,M10}},
     #{level:=info,msg:={string,M10}},
     #{level:=info,msg:={string,M10}},
     #{level:=info,msg:={string,M10}},
     #{level:=error,msg:={string,M11}},
     #{level:=info,msg:={"Simple handler buffer full, dropped ~w messages",[2]}}]
        = Buffered3,
    ok.
get_buffer(cleanup,_Config) ->
    logger:remove_handler(logger_simple).

replace_file(Config) ->
    ok = logger:add_handler(logger_simple,logger_simple,
                            #{filter_default=>log,
                              logger_simple=>#{buffer=>true}}),
    logger:emergency(M1=?str),
    logger:alert(M2=?str,[]),
    logger:error(?map_rep),
    logger:info(?keyval_rep),
    undefined = whereis(?STANDARD_HANDLER),
    PrivDir = ?config(priv_dir,Config),
    File = filename:join(PrivDir,atom_to_list(?FUNCTION_NAME)++".log"),

    application:set_env(kernel,logger_dest,{file,File}),
    application:set_env(kernel,logger_level,info),

    ok = logger:setup_standard_handler(),
    true = is_pid(whereis(?STANDARD_HANDLER)),
    ok = logger_std_h:filesync(?STANDARD_HANDLER),
    {ok,Bin} = file:read_file(File),
    Lines = [unicode:characters_to_list(L) || 
                L <- binary:split(Bin,<<"\n">>,[global,trim])],
    ["=EMERGENCY REPORT===="++_,
     M1,
     "=ALERT REPORT===="++_,
     M2,
     "=ERROR REPORT===="++_,
     _,
     _,
     "=INFO REPORT===="++_,
     _,
     _] = Lines,
    ok.
replace_file(cleanup,_Config) ->
    logger:remove_handler(?STANDARD_HANDLER),
    logger:remove_handler(logger_simple).
    
replace_disk_log(Config) ->
    ok = logger:add_handler(logger_simple,logger_simple,
                            #{filter_default=>log,
                              logger_simple=>#{buffer=>true}}),
    logger:emergency(M1=?str),
    logger:alert(M2=?str,[]),
    logger:error(?map_rep),
    logger:info(?keyval_rep),
    undefined = whereis(?STANDARD_HANDLER),
    PrivDir = ?config(priv_dir,Config),
    File = filename:join(PrivDir,atom_to_list(?FUNCTION_NAME)),

    application:set_env(kernel,logger_dest,{disk_log,File}),
    application:set_env(kernel,logger_level,info),

    ok = logger:setup_standard_handler(),
    true = is_pid(whereis(?STANDARD_HANDLER)),
    ok = logger_disk_log_h:disk_log_sync(?STANDARD_HANDLER),
    {ok,Bin} = file:read_file(File++".1"),
    Lines = [unicode:characters_to_list(L) || 
                L <- binary:split(Bin,<<"\n">>,[global,trim])],
    ["=EMERGENCY REPORT===="++_,
     M1,
     "=ALERT REPORT===="++_,
     M2,
     "=ERROR REPORT===="++_,
     _,
     _,
     "=INFO REPORT===="++_,
     _,
     _|_] = Lines, % the tail might be an info report about opening the disk log
    ok.
replace_disk_log(cleanup,_Config) ->
    logger:remove_handler(?STANDARD_HANDLER),
    logger:remove_handler(logger_simple).

