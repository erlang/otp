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
     config].

%%%-----------------------------------------------------------------
%%% Test cases
basic(_Config) ->
    ok = logger:add_handler(?HNAME,?MODULE,#{config=>self()}),
    logger_proxy ! {log,notice,"Log from: ~p; ~p",[?FUNCTION_NAME,?LINE],L1=?LOC},
    ok = ensure(L1),
    logger_proxy ! {log,notice,[{test_case,?FUNCTION_NAME},{line,?LINE}],L2=?LOC},
    ok = ensure(L2),
    logger_proxy:log(logger_server:get_proxy_ref(logger),
                     {remote,node(),{log,notice,
                                     "Log from: ~p; ~p",
                                     [?FUNCTION_NAME,?LINE],
                                     L3=?LOC}}),
    ok = ensure(L3),
    logger_proxy:log(logger_server:get_proxy_ref(logger),
                     {remote,node(),{log,notice,
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
    {skip,not_yet_implemented}.
config(cleanup,_Config) ->
    ok.

%%%-----------------------------------------------------------------
%%% Internal functions

%% Logger handler callback
log(#{meta:=Meta},#{config:=Pid}) ->
    Pid ! {logged,Meta}.

%% Check that the log from the logger callback function log/2 is received
ensure(Match) ->
    receive {logged,Meta} ->
            case maps:with(maps:keys(Match),Meta) of
                Match -> ok;
                NoMatch -> {error,Match,Meta,test_server:messages_get()}
            end
    after ?ENSURE_TIME -> {error,Match,test_server:messages_get()}
    end.
