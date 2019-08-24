%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2018. All Rights Reserved.
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
%% Description: a gen_server implementing a simple
%% terminal (using the group module) for a CLI
%% over SSH

-module(ssh_server_channel).

%% API to server side channel that can be pluged into the erlang ssh daemeon
-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.

-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().

-callback handle_msg(Msg ::term(), State :: term()) ->
    {ok, State::term()} | {stop, ChannelId::ssh:channel_id(), State::term()}. 
-callback handle_ssh_msg(ssh_connection:event(),
			 State::term()) -> {ok, State::term()} |
					   {stop, ChannelId::ssh:channel_id(),
					    State::term()}.

%%% Internal API
-export([start_link/5,
         get_print_info/1
        ]).

start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec) ->
    ssh_client_channel:start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec).


get_print_info(Pid) ->
    ssh_client_channel:get_print_info(Pid).
