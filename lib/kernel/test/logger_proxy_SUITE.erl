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
-module(logger_proxy_SUITE).

-compile(export_all).

%% -include_lib("common_test/include/ct.hrl").
%% -include_lib("kernel/include/logger.hrl").
%% -include_lib("kernel/src/logger_internal.hrl").

%% -define(str,"Log from "++atom_to_list(?FUNCTION_NAME)++
%%             ":"++integer_to_list(?LINE)).
%% -define(map_rep,#{function=>?FUNCTION_NAME, line=>?LINE}).
%% -define(keyval_rep,[{function,?FUNCTION_NAME}, {line,?LINE}]).

%% -define(MY_LOC(N),#{mfa=>{?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY},
%%                     file=>?FILE, line=>?LINE-N}).

%% -define(TRY(X), my_try(fun() -> X end)).


-define(HNAME,list_to_atom(lists:concat([?MODULE,"_",?FUNCTION_NAME]))).
-define(LOC,#{mfa=>{?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY},line=>?LINE}).
-define(ENSURE_TIME,5000).

suite() ->
    [{timetrap,{seconds,30}},
     {ct_hooks,[logger_test_lib]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
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
    [basic,
     emulator,
     remote,
     remote_emulator,
     config,
     restart_after,
     terminate].

%%%-----------------------------------------------------------------
%%% Test cases
basic(_Config) ->
    ok = logger:add_handler(?HNAME,?MODULE,#{config=>self()}),
    logger_proxy ! {log,notice,"Log from: ~p; ~p",[?FUNCTION_NAME,?LINE],L1=?LOC},
    ok = ensure(L1),
    logger_proxy ! {log,notice,[{test_case,?FUNCTION_NAME},{line,?LINE}],L2=?LOC},
    ok = ensure(L2),
    logger_proxy:log({remote,node(),{log,notice,
                                     "Log from: ~p; ~p",
                                     [?FUNCTION_NAME,?LINE],
                                     L3=?LOC}}),
    ok = ensure(L3),
    logger_proxy:log({remote,node(),{log,notice,
                                     [{test_case,?FUNCTION_NAME},
                                      {line,?LINE}],
                                     L4=?LOC}}),
    ok = ensure(L4),
    ok.
basic(cleanup,_Config) ->
    ok = logger:remove_handler(?HNAME).

emulator(_Config) ->
    ok = logger:add_handler(?HNAME,?MODULE,#{config=>self()}),
    Pid = spawn(fun() -> erlang:error(some_reason) end),
    ok = ensure(#{pid=>Pid}),
    ok.
emulator(cleanup,_Config) ->
    ok = logger:remove_handler(?HNAME).

remote(Config) ->
    {ok,_,Node} = logger_test_lib:setup(Config,[{logger,[{proxy,#{}}]}]),
    ok = logger:add_handler(?HNAME,?MODULE,#{config=>self()}),
    L1 = ?LOC, spawn(Node,fun() -> logger:notice("Log from ~p; ~p",[?FUNCTION_NAME,?LINE],L1) end),
    ok = ensure(L1),
    L2 = ?LOC, spawn(Node,fun() -> logger:notice([{test_case,?FUNCTION_NAME},{line,?LINE}],L2) end),
    ok = ensure(L2),
    ok.
remote(cleanup,_Config) ->
    ok = logger:remove_handler(?HNAME).

remote_emulator(Config) ->
    {ok,_,Node} = logger_test_lib:setup(Config,[{logger,[{proxy,#{}}]}]),
    ok = logger:add_handler(?HNAME,?MODULE,#{config=>self()}),
    Pid = spawn(Node,fun() -> erlang:error(some_reason) end),
    ok = ensure(#{pid=>Pid}),
    ok.
remote_emulator(cleanup,_Config) ->
    ok = logger:remove_handler(?HNAME).

config(_Config) ->
    C1 = #{sync_mode_qlen:=SQ,
           drop_mode_qlen:=DQ} = logger:get_proxy_config(),
    C1 = logger_olp:get_opts(logger_proxy),

    %% Update the existing config with these two values
    SQ1 = SQ+1,
    DQ1 = DQ+1,
    ok = logger:update_proxy_config(#{sync_mode_qlen=>SQ1,
                                      drop_mode_qlen=>DQ1}),
    C2 = logger:get_proxy_config(), % reads from ets table
    C2 = logger_olp:get_opts(logger_proxy), % ensure consistency with process opts
    C2 = C1#{sync_mode_qlen:=SQ1,
             drop_mode_qlen:=DQ1},

    %% Update the existing again with only one value
    SQ2 = SQ+2,
    ok = logger:update_proxy_config(#{sync_mode_qlen=>SQ2}),
    C3 = logger:get_proxy_config(),
    C3 = logger_olp:get_opts(logger_proxy),
    C3 = C2#{sync_mode_qlen:=SQ2},

    %% Set the config, i.e. merge with defaults
    ok = logger:set_proxy_config(#{sync_mode_qlen=>SQ1}),
    C4 = logger:get_proxy_config(),
    C4 = logger_olp:get_opts(logger_proxy),
    C4 = C1#{sync_mode_qlen:=SQ1},

    %% Reset to default
    ok = logger:set_proxy_config(#{}),
    C5 = logger:get_proxy_config(),
    C5 = logger_olp:get_opts(logger_proxy),
    C5 = logger_proxy:get_default_config(),

    %% Errors
    {error,{invalid_olp_config,_}} =
        logger:set_proxy_config(#{faulty_key=>1}),
    {error,{invalid_olp_config,_}} =
        logger:set_proxy_config(#{sync_mode_qlen=>infinity}),
    {error,{invalid_config,[]}} = logger:set_proxy_config([]),

    {error,{invalid_olp_config,_}} =
        logger:update_proxy_config(#{faulty_key=>1}),
    {error,{invalid_olp_config,_}} =
        logger:update_proxy_config(#{sync_mode_qlen=>infinity}),
    {error,{invalid_config,[]}} = logger:update_proxy_config([]),

    C5 = logger:get_proxy_config(),
    C5 = logger_olp:get_opts(logger_proxy),

    ok.
config(cleanup,_Config) ->
    _ = logger:set_logger_proxy(logger_proxy:get_default_config()),
    ok.

restart_after(_Config) ->
    Restart = 3000,
    ok = logger:update_proxy_config(#{overload_kill_enable => true,
                                      overload_kill_qlen => 10,
                                      overload_kill_restart_after => Restart}),
    Proxy = whereis(logger_proxy),
    Proxy = erlang:system_info(system_logger),
    ProxyConfig = logger:get_proxy_config(),
    ProxyConfig = logger_olp:get_opts(logger_proxy),

    Ref = erlang:monitor(process,Proxy),
    spawn(fun() ->
                  [logger_proxy ! {log,debug,
                                   [{test_case,?FUNCTION_NAME},
                                    {line,?LINE}],
                                   ?LOC} || _ <- lists:seq(1,100)]
          end),
    receive
        {'DOWN',Ref,_,_,_Reason} ->
            undefined = erlang:system_info(system_logger),
            timer:sleep(Restart),
            poll_restarted(10)
    after 5000 ->
            ct:fail(proxy_not_terminated)
    end,

    Proxy1 = whereis(logger_proxy),
    Proxy1 = erlang:system_info(system_logger),
    ProxyConfig = logger:get_proxy_config(),
    ProxyConfig = logger_olp:get_opts(logger_proxy),

    ok.
restart_after(cleanup,_Config) ->
    _ = logger:set_logger_proxy(logger_proxy:get_default_config()),
    ok.

%% Test that system_logger flag is set to logger process if
%% logger_proxy terminates for other reason than overloaded.
terminate(_Config) ->
    Logger = whereis(logger),
    Proxy = whereis(logger_proxy),
    Proxy = erlang:system_info(system_logger),
    ProxyConfig = logger:get_proxy_config(),
    ProxyConfig = logger_olp:get_opts(logger_proxy),

    Ref = erlang:monitor(process,Proxy),
    ok = logger_olp:stop(Proxy),
    receive
        {'DOWN',Ref,_,_,_Reason} ->
            Logger = erlang:system_info(system_logger),
            logger_proxy:restart(),
            poll_restarted(10)
    after 5000 ->
            ct:fail(proxy_not_terminated)
    end,

    Proxy1 = whereis(logger_proxy),
    Proxy1 = erlang:system_info(system_logger),
    ProxyConfig = logger:get_proxy_config(),
    ProxyConfig = logger_olp:get_opts(logger_proxy),

    ok.

%%%-----------------------------------------------------------------
%%% Internal functions

poll_restarted(0) ->
    ct:fail(proxy_not_restarted);
poll_restarted(N) ->
    timer:sleep(1000),
    case whereis(logger_proxy) of
        undefined ->
            poll_restarted(N-1);
        _Pid ->
            ok
    end.

%% Logger handler callback
log(#{meta:=Meta},#{config:=Pid}) ->
    Pid ! {logged,Meta}.

%% Check that the log from the logger callback function log/2 is received
ensure(Match) ->
    receive {logged,Meta} ->
            case maps:with(maps:keys(Match),Meta) of
                Match -> ok;
                _NoMatch -> {error,Match,Meta,test_server:messages_get()}
            end
    after ?ENSURE_TIME -> {error,Match,test_server:messages_get()}
    end.
