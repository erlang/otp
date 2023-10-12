%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2019-2019. All Rights Reserved.
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

-module(snmp_test_sys_monitor).

-export([start/0, stop/0,
         init/1]).

-define(NAME, ?MODULE).
-define(GSM,  snmp_test_global_sys_monitor).


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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Parent) ->
    process_flag(priority, high),
    try register(?NAME, self()) of
        true ->
            global:sync(),
            MonSettings = [
                           busy_port,
                           busy_dist_port,
                           {long_gc, 1000},
                           {long_schedule, 1000},
                           {large_heap, 8*1024*1024} % 8 MB
                          ],
            erlang:system_monitor(self(), MonSettings),
            ?GSM:log({erlang:timestamp(), starting}),
            proc_lib:init_ack(Parent, {ok, self()}),
            loop(#{parent => Parent})
    catch
        _:_:_ ->
            ?GSM:log({erlang:timestamp(), already_started}),
            proc_lib:init_ack(Parent, {error, already_started}),
            exit(normal)
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loop(State) ->
    receive
        {monitor, Pid, Tag, Info} ->
            ?GSM:log({Pid, erlang:timestamp(), Tag, Info}),
            loop(State);

        {?MODULE, From, stop} ->
            ?GSM:log({erlang:timestamp(), stopping}),
            From ! {?MODULE, self(), stop},
            exit(normal);

        _ ->
            loop(State)
    end.



