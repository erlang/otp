%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

-module(inets_service).


%% Starts service stand-alone
%% start_standalone(Config) ->  % {ok, Pid} | {error, Reason}
%%    <service>:start_link(Config).

-callback start_standalone(Config :: term()) ->
    {ok, pid()} | {error, Reason :: term()}.

%% Starts service as part of inets
%% start_service(Config) -> % {ok, Pid} | {error, Reason}
%%    <service_sup>:start_child(Config).

-callback start_service(Config :: term()) ->
    {ok, pid()} | {error, Reason :: term()}.

%% Stop service
%% stop_service(Pid) ->  % ok | {error, Reason}   
%%   <service_sup>:stop_child(maybe_map_pid_to_other_ref(Pid)).
%%
%% <service_sup>:stop_child(Ref) ->
%%    Id = id(Ref),
%%    case supervisor:terminate_child(?MODULE, Id) of
%%        ok ->
%%            supervisor:delete_child(?MODULE, Id);
%%        Error ->
%%            Error
%%    end.

-callback stop_service(Service :: term()) ->
    ok | {error, Reason :: term()}.

%% Returns list of running services. Services started as stand alone
%% are not listed 
%% services() -> % [{Service, Pid}] 
%% Exampel:
%% services() ->
%%   [{httpc, Pid} || {_, Pid, _, _} <- 
%%			supervisor:which_children(httpc_profile_sup)].

-callback services() ->
    [{Service :: term(), pid()}].

%% service_info() -> {ok, [{Property, Value}]} | {error, Reason}
%% ex: httpc:service_info() -> [{profile, ProfileName}] 
%%     httpd:service_info() -> [{host, Host}, {port, Port}]

-callback service_info(Service :: term()) ->
    {ok, [{Property :: term(), Value :: term()}]} | {error, Reason :: term()}.

