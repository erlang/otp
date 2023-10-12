%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2022. All Rights Reserved.
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
-module(logger_test_lib).

-include_lib("kernel/src/logger_internal.hrl").
-include_lib("common_test/include/ct.hrl").

-export([setup/2, log/3, sync_and_read/3]).

setup(Config,Vars) ->
    Postfix = case proplists:get_value(postfix, Config) of
                  undefined -> "";
                  P -> ["_",P]
              end,
    TestCase = proplists:get_value(tc, Config),
    FuncStr = lists:concat([proplists:get_value(suite, Config), "_", TestCase | Postfix]),
    ConfigFileName = filename:join(proplists:get_value(priv_dir, Config), FuncStr),
    file:write_file(ConfigFileName ++ ".config", io_lib:format("[{kernel, ~p}].",[Vars])),
    Args = ["-boot", "start_sasl", "-kernel", "start_timer", "true", "-config", ConfigFileName],
    try
        {ok, Peer, Node} = ?CT_PEER(#{name => ?CT_PEER_NAME(TestCase),
            args => Args, connection => standard_io}),
        L = rpc:call(Node, logger, get_config, []),
        ct:log("~p",[L]),
        {ok, L, Peer, Node}
    catch
        exit:{boot_failed, Reason} ->
            ct:log("Failed to start node: ~p", [Reason]),
            error
    end.

log(Node, F, A) ->
    log(Node, logger, F, A).
log(Node, M, F, A) ->
    MD = #{ gl => rpc:call(Node, erlang, whereis, [logger]) },
    rpc:call(Node, M, F, A ++ [MD]).

sync_and_read(Node,disk_log,Log) ->
    rpc:call(Node,logger_disk_log_h,filesync,[?STANDARD_HANDLER]),
    file:read_file(Log ++ ".1");
sync_and_read(Node, file,Log) ->
    ok = rpc:call(Node,logger_std_h,filesync,[?STANDARD_HANDLER]),
    file:read_file(Log).
