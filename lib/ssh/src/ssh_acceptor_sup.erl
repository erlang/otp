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
%% Purpose: The acceptor supervisor for ssh servers hangs under 
%%          ssh_system_sup.
%%----------------------------------------------------------------------

-module(ssh_acceptor_sup).
-moduledoc false.
-behaviour(supervisor).

-include("ssh.hrl").

-export([start_link/3,
         restart_child/2
        ]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(SystemSup, Address, Options) ->
    case supervisor:start_link(?MODULE, [SystemSup, Address, Options]) of
        {error, {shutdown, {failed_to_start_child, _, Error}}} ->
            {error,Error};
        Other ->
            Other
    end.

restart_child(AccSup, Address) ->
    supervisor:restart_child(AccSup, {?MODULE,Address}).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([SystemSup, Address, Options]) ->
    %% Initial start of ssh_acceptor_sup for this port
    SupFlags = #{strategy  => one_for_one, 
                 intensity =>   10,
                 period    => 3600
                },
    ChildSpecs = [#{id       => {?MODULE,Address},
                    start    => {ssh_acceptor, start_link, [SystemSup, Address, Options]},
                    restart  => transient % because a crashed listener could be replaced by a new one
                   }
                 ],
    {ok, {SupFlags,ChildSpecs}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
