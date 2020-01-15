%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2016-2018. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Supervisor for a procsses dispatching upd datagrams to
%% correct DTLS handler 
%%----------------------------------------------------------------------
-module(dtls_listener_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_child/1,
         lookup_listner/1,
         register_listner/2]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).
    
lookup_listner(0) ->
    undefined;
lookup_listner(Port) ->
    try ets:lookup(dtls_listener_sup, Port) of 
        [{Port, {Owner, Handler}}] ->
            case erlang:is_process_alive(Handler) of 
                true ->
                    case (Owner =/= undefined) andalso erlang:is_process_alive(Owner) of
                        true ->
                            {error, already_listening};
                        false ->
                            {ok, Handler}
                    end;
                false ->
                    ets:delete(dtls_listener_sup, Port),
                    undefined
            end;
        [] ->
            undefined
    catch _:_ ->
            undefined
    end.

register_listner(OwnerAndListner, Port) -> 
    ets:insert(dtls_listener_sup, {Port, OwnerAndListner}).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(_O) ->
    ets:new(dtls_listener_sup, [named_table, public, set]),
    RestartStrategy = simple_one_for_one,
    MaxR = 0,
    MaxT = 3600,
   
    Name = undefined, % As simple_one_for_one is used.
    StartFunc = {dtls_packet_demux, start_link, []},
    Restart = temporary, % E.g. should not be restarted
    Shutdown = 4000,
    Modules = [dtls_packet_demux],
    Type = worker,
    
    ChildSpec = {Name, StartFunc, Restart, Shutdown, Type, Modules},
    {ok, {{RestartStrategy, MaxR, MaxT}, [ChildSpec]}}.
