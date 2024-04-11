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

-module(ssh_acceptor_subsup).
-moduledoc false.
-behaviour(supervisor).

-include("ssh.hrl").

-export([start_link/3,
         listen/2
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

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([SystemSup, Address=#address{port=Port}, Options]) ->
    ssh_lib:set_label(server, acceptor_subsup),
    %% Initial start of ssh_acceptor_sup for this port
    Self = self(),
    NumAcceptors = case ?GET_OPT(parallel_login, Options) of
                       true -> 1;
                       false -> 1;
                       N -> N
                   end,
    {Sock, Opts} = case ?GET_INTERNAL_OPT(lsocket, Options, undefined) of
        {LSock, SockOwner} ->
            %% A listening socket (or fd option) was provided in the ssh:daemon call
            case inet:sockname(LSock) of
                {ok, {_, Port}} ->
                    %% A usable, open LSock
                    spawn_link(fun() ->
                              SockOwner ! {request_control, LSock, Self, self()},
                              receive {its_yours, LSock} -> ok end,
                              [supervisor:start_child(Self, []) || _ <- lists:seq(1, NumAcceptors)],
                              unlink(Self)
                          end),
                    {LSock, Options};

                {error, _Error} ->
                    %% Not open, a restart
                    %% Allow gen_tcp:listen to fail 4 times if eaddrinuse (It is a bug fix):
                    case try_listen(Port, Options, 4) of
                        {ok, NewLSock} ->
                            spawn(fun() ->
                                      [supervisor:start_child(Self, []) || _ <- lists:seq(1, NumAcceptors)]
                                  end),
                            {NewLSock, ?DELETE_INTERNAL_OPT(lsocket, Options)};
                        {error, Error} ->
                            exit({error, Error})
                    end
            end;

        undefined ->
            %% No listening socket (nor fd option) was provided; open a listening socket:
            case listen(Port, Options) of
                {ok, LSock} ->
                    spawn(fun() ->
                              [supervisor:start_child(Self, []) || _ <- lists:seq(1, NumAcceptors)]
                          end),
                    {LSock, Options};
                {error, Error} ->
                    exit({error, Error})
            end
    end,
    SupFlags = #{strategy  => simple_one_for_one, 
                 intensity => 1000000,
                 period    => 1
                },
    ChildSpecs = [#{id => ssh_acceptor,
                    start => {ssh_acceptor, start_link, [SystemSup, Sock, Address, Opts]},
                    restart => transient}],
    {ok, {SupFlags,ChildSpecs}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================

listen(Port, Options) ->
    {_, Callback, _} = ?GET_OPT(transport, Options),
    SockOpts = ?GET_OPT(socket_options, Options) ++ [{active, false}, {reuseaddr, true}],
    case Callback:listen(Port, SockOpts) of
        {error, nxdomain} ->
            Callback:listen(Port, lists:delete(inet6, SockOpts));
        {error, enetunreach} ->
            Callback:listen(Port, lists:delete(inet6, SockOpts));
        {error, eafnosupport} ->
            Callback:listen(Port, lists:delete(inet6, SockOpts));
        Other ->
            Other
    end.

try_listen(Port, Opts, NtriesLeft) ->
    try_listen(Port, Opts, 1, NtriesLeft).

try_listen(Port, Opts, N, Nmax) ->
    case listen(Port, Opts) of
        {error,eaddrinuse} when N<Nmax ->
            timer:sleep(10*N), % Sleep 10, 20, 30,... ms
            try_listen(Port, Opts, N+1, Nmax);
        Other ->
            Other
    end.
