%  Copyright 2008 Konrad-Zuse-Zentrum für Informationstechnik Berlin
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%       http://www.apache.org/licenses/LICENSE-2.0
%
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
%%%-------------------------------------------------------------------
%%% File    : comm_layer.erl
%%% Author  : Thorsten Schuett <schuett@zib.de>
%%% Description : Public interface to Communication Layer.
%%%           Generic functions to send messages.
%%%           Distinguishes on runtime whether the destination is in the
%%%           same Erlang virtual machine (use ! for sending) or on a remote
%%%           site (use comm_port:send()).
%%%
%%% Created :  04 Feb 2008 by Thorsten Schuett <schuett@zib.de>
%%%-------------------------------------------------------------------
%% @author Thorsten Schuett <schuett@zib.de>
%% @copyright 2008 Konrad-Zuse-Zentrum für Informationstechnik Berlin
%% @version $Id $
-module(comm_layer_dir.comm_layer).

-author('schuett@zib.de').
-vsn('$Id: comm_layer.erl,v 1.1 2009/11/06 12:41:36 maria Exp $ ').

-export([start_link/0, send/2, this/0, here/1]).

-import(io).
-import(util).
-import(log).

-include("comm_layer.hrl").


% @TODO: should be ip
-type(process_id() :: {any(), integer(), pid()}).
%%====================================================================
%% public functions
%%====================================================================

%% @doc starts the communication port (for supervisor)
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
start_link() ->
    comm_port_sup:start_link().

%% @doc a process descriptor has to specify the erlang vm
%%      + the process inside. {IP address, port, pid}
%% @type process_id() = {inet:ip_address(), int(), pid()}.
%% @spec send(process_id(), term()) -> ok

send({{_IP1, _IP2, _IP3, _IP4} = _IP, _Port, _Pid} = Target, Message) ->
    {MyIP,MyPort} = comm_port:get_local_address_port(),
    %io:format("send: ~p:~p -> ~p:~p(~p) : ~p\n", [MyIP, MyPort, _IP, _Port, _Pid, Message]),
    IsLocal = (MyIP == _IP) and (MyPort == _Port),
    if
	IsLocal ->
	    ?LOG_MESSAGE(erlang:element(1, Message), byte_size(term_to_binary(Message))),
	    _Pid ! Message;
	true ->
	    comm_port:send(Target, Message)
    end;

send(Target, Message) ->
    log:log(error,"[ CC ] wrong call to cs_send:send: ~w ! ~w", [Target, Message]),
    log:log(error,"[ CC ] stacktrace: ~w", [util:get_stacktrace()]),
    ok.

%% @doc returns process descriptor for the calling process
-spec(this/0 :: () -> atom()).%process_id()).
this() ->
    here(self()).

-spec(here/1 :: (pid()) -> process_id()).
here(Pid) ->
    {LocalIP, LocalPort} = comm_port:get_local_address_port(),
    {LocalIP, LocalPort, Pid}.
