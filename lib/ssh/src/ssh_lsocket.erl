%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
%% Purpose: Produce listen sockets.
%%----------------------------------------------------------------------
-module(ssh_lsocket).
-moduledoc false.

-include("ssh.hrl").
-export([start_link/4, provide_lsocket/4, get_lsocket/3]).

-behaviour(ssh_dbg).
-export([ssh_dbg_trace_points/0, ssh_dbg_flags/1, ssh_dbg_on/1, ssh_dbg_off/1,
         ssh_dbg_format/2, ssh_dbg_format/3]).

get_lsocket(Host, Port, Options) ->
    try
        supervisor:start_child(ssh_lsocket_sup, [self(), Host, Port, Options])
    of
        {ok, LSocketProvider} ->
            receive
                Result = {_, {LSocketProvider, _}} ->
                    Result
            after
                ?DEFAULT_TIMEOUT ->
                    {error, LSocketProvider, no_response_from_lsocket_provider}
            end
    catch
        exit:{noproc, _} ->
            {error, {no_provider_pid, ssh_not_started}}
    end.

start_link(Caller, Host, Port, Options) ->
    {ok, proc_lib:spawn_link(?MODULE, provide_lsocket,
                             [Caller, Host, Port, Options])}.

provide_lsocket(Caller, _Host1, Port0, Options) ->
    ListenResult =
        try
            try_listen(Port0, Options, 4)
        of
            {ok, LSocket} ->
                {ok, {self(), LSocket}};
            {error, Details} ->
                {error, {self(), Details}}
        catch
            _Class:Exception ->
                {error, {self(), Exception}}
        end,
    case ListenResult of
        {ok, {_, LSocket1}} ->
            Caller ! ListenResult,
            wait_for_acceptor_sup(LSocket1, Options),
            ok;
        {error, {_, _}} ->
            Caller ! ListenResult
    end.

wait_for_acceptor_sup(ListenSocket, Options) ->
    receive
        {request_control, ListenSocket, AcceptorSup} ->
            ok = controlling_process(ListenSocket, AcceptorSup, Options),
            AcceptorSup ! {its_yours,ListenSocket},
            ok
    end.

controlling_process(ListenSocket, ReqPid, Options) ->
    {_, Callback, _} = ?GET_OPT(transport, Options),
    Callback:controlling_process(ListenSocket, ReqPid).

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

listen(Port, Options) ->
    {_, Callback, _} = ?GET_OPT(transport, Options),
    SockOpts = ?GET_OPT(socket_options, Options) ++ [{active, false}, {reuseaddr,true}],
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


%%%################################################################
%%%#
%%%# Tracing
%%%#

ssh_dbg_trace_points() -> [connections].

ssh_dbg_flags(connections) -> [c].

ssh_dbg_on(connections) ->
    dbg:tpl(?MODULE, provide_lsocket, 4, x),
    dbg:tpl(?MODULE, controlling_process, 3, x),
    dbg:tpl(?MODULE, try_listen, 4, x),
    dbg:tpl(?MODULE, wait_for_acceptor_sup, 2, x);
ssh_dbg_on(tcp) -> dbg:tpl(?MODULE, accept, 3, x).

ssh_dbg_off(connections) ->
    dbg:ctpl(?MODULE, provide_lsocket, 4),
    dbg:ctpl(?MODULE, controlling_process, 3),
    dbg:ctpl(?MODULE, try_listen, 4),
    dbg:ctpl(?MODULE, wait_for_acceptor_sup, 2);
ssh_dbg_off(tcp) -> dbg:ctpl(?MODULE, accept, 3).

ssh_dbg_format(Tracepoint, Event = {call, {?MODULE, Function, Args}}) ->
    [io_lib:format("~w:~w/~w> ~s", [?MODULE, Function, length(Args)] ++
                       ssh_dbg_comment(Tracepoint, Event))];
ssh_dbg_format(Tracepoint, Event = {return_from, {?MODULE,Function,Arity}, Ret}) ->
    [io_lib:format("~w:~w/~w returned ~W> ~s", [?MODULE, Function, Arity, Ret, 3] ++
                  ssh_dbg_comment(Tracepoint, Event))].

ssh_dbg_comment(connections, {call, {?MODULE, try_listen, [Port, _, N, Nmax]}}) ->
    [io_lib:format("listen retry on Port:~p [~p/~p]", [Port, N, Nmax])];
ssh_dbg_comment(connections, {call, {?MODULE, provide_lsocket, [_Parent, _Caller, _Ref, Host, Port, _UserOptions]}}) ->
    [io_lib:format("providing LSocket for Host: ~p Port: ~p", [Host, Port])];
ssh_dbg_comment(connections, {call, {?MODULE,controlling_process,[ListenSocket, ReqPid, _Options]}}) ->
    [io_lib:format("changing owner for ~p to ~p", [ListenSocket, ReqPid])];
ssh_dbg_comment(connections, {return_from, {?MODULE,controlling_process,3}, _Ret}) ->
    [io_lib:format("ownership changed", [])];
ssh_dbg_comment(connections, {call, {?MODULE, wait_for_acceptor_sup, [LSocket, _Options]}}) ->
    [io_lib:format("waiting for acceptor sup to pickup LSocket ~p", [LSocket])];
ssh_dbg_comment(connections, {return_from, {?MODULE,wait_for_acceptor_sup,2}, ok}) ->
    [io_lib:format("LSocket provided. Bye.", [])];
ssh_dbg_comment(_, _) ->
    [""].

ssh_dbg_format(tcp, {call, {?MODULE,listen, [Port,_Opts]}}, Stack) ->
    {skip, [{port,Port}|Stack]};
ssh_dbg_format(tcp, {return_from, {?MODULE,listen,2}, {ok,Sock}}, [{port,Port}|Stack]) ->
    {["TCP listener started\n",
      io_lib:format("Port: ~p~n"
                    "ListeningSocket: ~p~n", [Port,Sock])
     ],
     Stack};
ssh_dbg_format(tcp, {return_from, {?MODULE,listen,2}, Result}, [{port,Port}|Stack]) ->
    {["TCP listener start ERROR\n",
      io_lib:format("Port: ~p~n"
                    "Return: ~p~n", [Port,Result])
     ],
     Stack}.
