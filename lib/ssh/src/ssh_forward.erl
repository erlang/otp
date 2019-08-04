%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2018. All Rights Reserved.
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

%%
%%----------------------------------------------------------------------
%% Purpose: Ssh channel for port forwarding.
%%----------------------------------------------------------------------

-module(ssh_forward).

-behaviour(ssh_server_channel).

-export([init/1, handle_cast/2, handle_msg/2, handle_ssh_msg/2, terminate/2]).

-export([set_socket/2]).

-export([dbg_trace/3]).

-include("ssh.hrl").
-include("ssh_connect.hrl").

-define(tcp_options,[binary,
                     {keepalive, true},
                     {active, false},
                     {nodelay, true},
                     {send_timeout, 5000},
                     {send_timeout_close, true}
                    ]).

-record(st, { address :: string()
            , port :: non_neg_integer()
            , channel :: ssh:channel_id()
            , cm :: pid()
            , socket :: port() | undefined
            , data :: binary() | undefined
            , init_socket :: reference() | undefined
            , role :: ssh:role()
            , mode :: direct | forwarded
            }).

set_socket(Pid, Sock) ->
    gen_server:cast(Pid, {set_socket, Sock}).

%%====================================================================
%% ssh_server_channel callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%%
%% Description: Initiates the CLI
%%--------------------------------------------------------------------

%% direct-tcpip
init({Role, ConnManager, Addr, Port, _OrigAddr, _OrigPort, ChannelId}) ->
    Self = self(),
    Pid = proc_lib:spawn_opt(fun () -> 
                                     init_socket(Self, binary_to_list(Addr), Port, ?tcp_options) 
                             end, []),
    Ref = monitor(process, Pid),
    {ok, #st{channel = ChannelId,
             cm = ConnManager,
             address = Addr,
             port = Port,
             init_socket = Ref,
             role = Role,
             mode = direct
            }};
%% forwarded-tcpip
init({Role, ConnManager, Addr, Port, ChannelId}) ->
    {ok, #st{channel = ChannelId,
             cm = ConnManager,
             address = Addr,
             port = Port,
             role = Role,
             mode = forwarded
            }}.

handle_cast({set_socket, Sock}, St = #st{cm = ConnManager, channel = Id, data = Data, mode = Mode}) ->
    case Mode of
        forwarded ->
            ok;
        direct ->
            ok = ssh_connection_handler:reply_request(ConnManager, open_confirmation, Id)
    end,
    case Data of
        undefined ->
            ok;
        _ ->
            ok = gen_tcp:send(Sock, Data)
    end,
    ok = inet:setopts(Sock, [{active, once}]),
    {noreply, St#st{socket = Sock, data = undefined}}.

handle_ssh_msg({ssh_cm, _, {data, _ChannelId, _, Data}}, St = #st{socket = undefined, data = undefined}) ->
    {ok, St#st{data = Data}};
handle_ssh_msg({ssh_cm, _, {data, _ChannelId, _, Data}}, St = #st{socket = undefined, data = Acc}) ->
    {ok, St#st{data = <<Acc/binary, Data/binary>>}};
handle_ssh_msg({ssh_cm, _, {data, ChannelId, _, Data}}, St = #st{socket = Sock}) ->
    case gen_tcp:send(Sock, Data) of
        ok ->
            {ok, St};
        {error, _} ->
            {stop, ChannelId, St}
    end;
handle_ssh_msg({ssh_cm, _, {eof, Id}}, St = #st{ channel = Id}) ->
    {stop, Id, St};
handle_ssh_msg({ssh_cm, _, {signal, Id, _}}, St = #st{ channel = Id}) ->
    {ok, St};
handle_ssh_msg({ssh_cm, _, {exit_signal, Id, _, _Error, _}}, St = #st{ channel = Id}) ->
    {stop, Id, St};
handle_ssh_msg({ssh_cm, _, {exit_status, Id, _Status}}, St = #st{ channel = Id}) ->
    {stop, Id, St}.

%%--------------------------------------------------------------------
%% Function: handle_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%
%% Description: Handles other channel messages
%%--------------------------------------------------------------------
handle_msg({'DOWN', Ref, process, _Pid, normal}, St = #st{init_socket = Ref}) ->
    {ok, St#st{init_socket = undefined}};
handle_msg({'DOWN', Ref, process, _Pid, Error}, St = #st{init_socket = Ref, channel = Id,
                                                         address = Addr, port = Port}) ->
    error_logger:error_msg("failed to open forwarded connection with ~s:~p ~p", [Addr, Port, Error]),
    {stop, Id, St};
handle_msg({tcp, Sock, Data}, St = #st{cm = ConnManager, channel = Id, socket = Sock}) ->
    ok = inet:setopts(Sock, [{active, once}]),
    ssh_connection:send(ConnManager, Id, Data),
    {ok, St};
handle_msg({tcp_closed, Sock}, St = #st{cm = ConnManager, channel = Id, socket = Sock}) ->
    ssh_connection:send_eof(ConnManager, Id),
    {stop, Id, St};
handle_msg({ssh_channel_up, Id, ConnManager}, St = #st{channel = Id, cm = ConnManager}) ->
    {ok,  St}.

%%--------------------------------------------------------------------
%% Function: terminate(Reasons, State) -> _
%%--------------------------------------------------------------------
terminate(_Reason, #st{socket = Sock}) ->
    catch gen_tcp:close(Sock),
    void.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

init_socket(Parent, Addr, Port, Opts) ->
    case gen_tcp:connect(Addr, Port, Opts) of
        {ok, Sock} ->
            ok = gen_tcp:controlling_process(Sock, Parent),
            set_socket(Parent, Sock);
        {error, Error} ->
            exit(Error)
    end.

%%%################################################################
%%%#
%%%# Tracing
%%%#

dbg_trace(points,         _,  _) -> [terminate];

dbg_trace(flags,  terminate,  _) -> [c];
dbg_trace(on,     terminate,  _) -> dbg:tp(?MODULE,  terminate, 2, x);
dbg_trace(off,    terminate,  _) -> dbg:ctpg(?MODULE, terminate, 2);
dbg_trace(format, terminate, {call, {?MODULE,terminate, [Reason, State]}}) ->
    ["Port Forward Terminating:\n",
     io_lib:format("Reason: ~p,~nState:~n~s", [Reason, wr_record(State)])
    ].

?wr_record(st).
