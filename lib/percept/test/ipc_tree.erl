%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%% 

-module(ipc_tree).
-export([go/1, init/2]).

go(N) ->
    start(N, self()),
    receive stop -> ok end.

start(Depth, ParentPid) ->
    spawn(?MODULE, init, [Depth, ParentPid]).

init(0, ParentPid) -> 
    workload(5000),
    ParentPid ! stop,
    ok;
init(Depth, ParentPid) ->
    Pid1 = spawn(?MODULE, init, [Depth - 1, self()]),
    Pid2 = spawn(?MODULE, init, [Depth - 1, self()]),
    main([Pid1,Pid2], ParentPid).

main(Pids, ParentPid) ->
    workload(5000),
    gather(Pids),
    ParentPid ! stop,
    ok.

gather([]) -> ok;
gather([_|Pids]) -> receive _ -> gather(Pids) end.

workload(0) -> ok;
workload(N) -> _ = math:sin(2), workload(N - 1).
