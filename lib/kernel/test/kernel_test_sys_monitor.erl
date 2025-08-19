%% 
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2021-2025. All Rights Reserved.
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

-module(kernel_test_sys_monitor).

-export([start/0, stop/0,
         ping/0, ping/1,
         init/1]).

-define(NAME, ?MODULE).
-define(GSM,  kernel_test_global_sys_monitor).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    Parent = self(),
    proc_lib:start(?MODULE, init, [Parent]).

stop() ->
    case whereis(?NAME) of
        Pid when is_pid(Pid) ->
            Pid ! {?MODULE, self(), stop},
            receive
                {?MODULE, Pid, stop} ->
                    ok
            end;
        _ ->
            ok
    end.


ping(Node) when is_atom(Node) andalso (Node =/= node()) ->
    case rpc:call(Node, ?MODULE, ping, []) of
        {badrpc, nodedown} ->
            pang;
        Reply ->
            Reply
    end.

ping() ->
    case whereis(?NAME) of
        Pid when is_pid(Pid) ->
            Pid ! {?MODULE, self(), ping},
            receive
                {?MODULE, Pid, Reply} ->
                    Reply
            end;
        _ ->
            pang
    end.
        


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Parent) ->
    process_flag(priority, high),
    try register(?NAME, self()) of
        true ->
            await_synced(),
            ?GSM:log({?GSM:timestamp(), starting}),
            MonSettings = [
                           busy_port,
                           busy_dist_port,
                           {long_gc, 1000},
                           {long_schedule, 1000},
                           {large_heap, 8*1024*1024} % 8 MB
                          ],
            erlang:system_monitor(self(), MonSettings),
            proc_lib:init_ack(Parent, {ok, self()}),
            ?GSM:log({?GSM:timestamp(), started}),
            loop(#{parent => Parent})
    catch
        _:_:_ ->
            ?GSM:log({?GSM:timestamp(), already_started}),
            proc_lib:init_ack(Parent, {error, already_started}),
            exit(normal)
    end.
    

await_synced() ->
    case global:whereis_name(?GSM) of
        Pid when is_pid(Pid) ->
            ok;
        undefined ->
            global:sync(),
            receive after 1000 -> ok end,
            await_synced()
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loop(State) ->
    receive
        {monitor, Pid, Tag, Info} ->
            ?GSM:log({Pid, ?GSM:timestamp(), Tag, Info}),
            loop(State);

        {?MODULE, From, stop} ->
            ?GSM:log({?GSM:timestamp(), stopping}),
            From ! {?MODULE, self(), stop},
            exit(normal);

        {?MODULE, From, ping} ->
            ?GSM:log({?GSM:timestamp(), ping}),
            From ! {?MODULE, self(), pong},
            loop(State);

        _ ->
            loop(State)
    end.



