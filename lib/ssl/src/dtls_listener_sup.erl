%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2016-2021. All Rights Reserved.
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
         lookup_listener/2,
         register_listener/3]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

lookup_listener(IP, Port) ->
    try ets:lookup(dtls_listener_sup, {IP, Port}) of
        [] ->
            undefined;
        [{{IP, Port}, {Owner, Handler}}] ->
            case erlang:is_process_alive(Handler) of 
                true ->
                    case (Owner =/= undefined) andalso
                        erlang:is_process_alive(Owner) of
                        true ->
                            %% Trying to bind port that is already bound
                            {error, already_listening};
                        false ->
                            %% Re-open same listen socket when the handler
                            %% is dead.
                            {ok, Handler}
                    end;
                false ->
                    ets:delete(dtls_listener_sup, {IP, Port}),
                    undefined
            end
    catch _:_ ->
            undefined
    end.

register_listener(OwnerAndListner, IP, Port) ->
    ets:insert(dtls_listener_sup, {{IP, Port}, OwnerAndListner}).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(_) ->
    ets:new(dtls_listener_sup, [named_table, public, set]),    
    SupFlags = #{strategy  => simple_one_for_one, 
                 intensity =>   0,
                 period    => 3600
                },
    ChildSpecs = [#{id       => undefined,
                    start    => {dtls_packet_demux, start_link, []},
                    restart  => temporary, 
                    shutdown => 4000,
                    modules  => [dtls_packet_demux],
                    type     => worker
                   }],     
    {ok, {SupFlags, ChildSpecs}}.
