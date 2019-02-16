%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2018. All Rights Reserved.
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

%%

%%% Description: Example ssh server
-module(ssh_bench_dev_null).
-behaviour(ssh_server_channel).

-record(state, {
          cm,
          chid,
          n,
          sum = 0
	 }).

-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2]).

init([N]) -> {ok, #state{n=N}}.

handle_msg({ssh_channel_up, ChId, CM}, S) -> 
    {ok, S#state{cm = CM,
                 chid = ChId}}.



handle_ssh_msg({ssh_cm, CM, {data,ChId,0,Data}}, #state{n=N, sum=Sum0, cm=CM, chid=ChId} = S) -> 
    Sum = Sum0 + size(Data),
    if Sum == N ->
            %% Got all
            ssh_connection:send(CM, ChId, <<"READY">>),
            {ok, S#state{sum=Sum}};
       Sum < N ->
            %% Expects more
            {ok, S#state{sum=Sum}}
    end;
handle_ssh_msg({ssh_cm, _, {exit_signal,ChId,_,_,_}}, S) -> {stop, ChId, S};
handle_ssh_msg({ssh_cm, _, {exit_status,ChId,_}    }, S) -> {stop, ChId, S};
handle_ssh_msg({ssh_cm, _, _                       }, S) -> {ok, S}.

terminate(_, _) -> ok.
