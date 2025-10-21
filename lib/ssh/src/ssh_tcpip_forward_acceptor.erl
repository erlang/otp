%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
%% Purpose: The supervisor for tcpip-forwarding acceptor
%%----------------------------------------------------------------------

-module(ssh_tcpip_forward_acceptor).
-moduledoc false.

-export([supervised_start/6,
         start_link/6]).

-include("ssh.hrl").

%%%----------------------------------------------------------------
supervised_start(FwdSup, {ListenAddrStr, ListenPort}, ConnectToAddr, ChanType, ChanCB, ConnPid) ->
    case get_fwd_listen_opts(ListenAddrStr) of
        {ok,Opts} ->
            %% start listening on Addr:BoundPort
            case gen_tcp:listen(ListenPort,
                                Opts ++ [binary, {reuseaddr,true}, {active,false}]) of
                {ok,LSock} ->
                    {ok,{_, TrueListenPort}} = inet:sockname(LSock),
                    ssh_tcpip_forward_acceptor_sup:start_child(FwdSup,
                                                               LSock,
                                                               {ListenAddrStr,TrueListenPort},
                                                               ConnectToAddr,
                                                               ChanType,
                                                               ChanCB,
                                                               ConnPid),
                    {ok, TrueListenPort};

                {error,Error} ->
                    {error,Error}
            end;

        {error,Error} ->
            {error,Error}
    end.


%%%----------------------------------------------------------------
start_link(LSock, {ListenAddrStr,ListenPort}, ConnectToAddr, ChanType, ChanCB, ConnPid) ->
    Pid = proc_lib:spawn_link(
            fun() ->
                    acceptor_loop(LSock, ListenAddrStr, ListenPort, ConnectToAddr, ChanType, ChanCB, ConnPid)
            end),
    {ok, Pid}.
    
%%%================================================================
%%%
%%% Internal
%%% 
acceptor_loop(LSock, ListenAddrStr, ListenPort, ConnectToAddr, ChanType, ChanCB, ConnPid) ->
    case gen_tcp:accept(LSock) of
        {ok, Sock} ->
            {ok, {RemHost,RemPort}} = inet:peername(Sock),
            RemHostBin = list_to_binary(encode_ip(RemHost)),
            Data = 
                case ConnectToAddr of
                    undefined ->
                        <<?STRING(ListenAddrStr), ?UINT32(ListenPort),
                          ?STRING(RemHostBin), ?UINT32(RemPort)>>;
                    {ConnectToHost, ConnectToPort} ->
                        <<?STRING(ConnectToHost), ?UINT32(ConnectToPort),
                          ?STRING(RemHostBin), ?UINT32(RemPort)>>
                end,
            case ssh_connection:open_channel(ConnPid, ChanType, Data, infinity) of
                {ok,ChId} ->
                    gen_tcp:controlling_process(Sock, ConnPid),
                    ConnPid ! {fwd_connect_received, Sock, ChId, ChanCB};
                _ ->
                    gen_tcp:close(Sock)
            end,
            acceptor_loop(LSock, ListenAddrStr, ListenPort, ConnectToAddr, ChanType, ChanCB, ConnPid);

        {error,closed} ->
            ok
    end.

%%%----------------------------------------------------------------
get_fwd_listen_opts(<<"">>         ) -> {ok, []};
get_fwd_listen_opts(<<"0.0.0.0">>  ) -> {ok, [inet]};
get_fwd_listen_opts(<<"::">>       ) -> {ok, [inet6]};
get_fwd_listen_opts(<<"localhost">>) -> {ok, [{ip,loopback}]};
get_fwd_listen_opts(AddrStr) ->
    case inet:getaddr(binary_to_list(AddrStr), inet) of
        {ok, Addr} -> {ok, [{ip,Addr}]};
        {error,Error} -> {error,Error}
    end.

%%%----------------------------------------------------------------
encode_ip(Addr) when is_tuple(Addr) ->
    case catch inet_parse:ntoa(Addr) of
	{'EXIT',_} -> false;
	A -> A
    end.
