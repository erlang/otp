%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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

-module(ct_run_SUITE).

-export([all/0, suite/0]).
-export([missing_argument/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks, [ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() ->
    [missing_argument].

missing_argument(Config) when is_list(Config) ->
    case os:find_executable("ct_run") of
        false ->
            {skip, "ct_run executable not found"};
        CtRun ->
            [missing_argument(CtRun, Opt)
             || Opt <- ["-sname", "-name", "-browser", "-ctname"]],
            ok
    end.

missing_argument(CtRun, Opt) ->
    Port = open_port({spawn_executable, CtRun},
                     [{args, [Opt]}, binary, exit_status, stderr_to_stdout]),
    Expected = iolist_to_binary(["ct_run: Missing argument to ", Opt, "\n"]),
    {1, Expected} = collect_port(Port, <<>>).

collect_port(Port, Acc) ->
    receive
        {Port, {data, Data}} -> collect_port(Port, <<Acc/binary, Data/binary>>);
        {Port, {exit_status, Status}} -> {Status, Acc}
    after 30000 ->
            ct:fail({timeout, Acc})
    end.

