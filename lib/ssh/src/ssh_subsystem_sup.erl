%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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
%% Purpose: The ssh subsystem supervisor 
%%----------------------------------------------------------------------

-module(ssh_subsystem_sup).
-moduledoc false.

-behaviour(supervisor).

-include("ssh.hrl").

-export([start_link/5,
         start_channel/8,
         tcpip_fwd_supervisor/1
	]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Role, Address=#address{}, Id, Socket, Options) ->
    case supervisor:start_link(?MODULE, [Role, Address, Id, Socket, Options]) of
        {error, {shutdown, {failed_to_start_child, _, Error}}} ->
            {error,Error};
        Other ->
            Other
    end.

start_channel(Role, SupPid, ConnRef, Callback, Id, Args, Exec, Opts) ->
    ChannelSup = channel_supervisor(SupPid),
    ssh_channel_sup:start_child(Role, ChannelSup, ConnRef, Callback, Id, Args, Exec, Opts).

tcpip_fwd_supervisor(SubSysSup) ->
    find_child(tcpip_forward_acceptor_sup, SubSysSup).


%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([Role, Address, Id, Socket, Options]) ->
    SubSysSup = self(),
    SupFlags = #{strategy      => one_for_all,
                 auto_shutdown => any_significant,
                 intensity     =>    0,
                 period        => 3600
                },
    ChildSpecs = [#{id          => connection,
                    restart     => temporary,
                    type        => worker,
                    significant => true,
                    start       => {ssh_connection_handler,
                                    start_link,
                                    [Role, Address, Id, Socket,
                                     ?PUT_INTERNAL_OPT([
                                                        {subsystem_sup, SubSysSup}
                                                       ], Options)
                                    ]
                                   }
                   },
                  #{id      => channel_sup,
                    restart => temporary,
                    type    => supervisor,
                    start   => {ssh_channel_sup, start_link, [Options]}
                   },

                 #{id      => tcpip_forward_acceptor_sup,
                   restart => temporary,
                   type    => supervisor,
                   start   => {ssh_tcpip_forward_acceptor_sup, start_link, []}
                  }
                 ],
    {ok, {SupFlags,ChildSpecs}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
channel_supervisor(SubSysSup) -> find_child(channel_sup, SubSysSup).

find_child(Id, Sup) when is_pid(Sup) ->
    try
       {Id, Pid, _, _} = lists:keyfind(Id, 1, supervisor:which_children(Sup)),
       Pid
    catch
        exit:{no_proc,_} ->
            {error, no_proc};
        _:_ ->
            {error, {id_not_found,?MODULE,Id}}
    end.

