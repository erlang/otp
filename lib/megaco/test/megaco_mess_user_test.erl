%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%% Purpose: A fun implementation of user callbacks
%%----------------------------------------------------------------------

-module(megaco_mess_user_test).

-behaviour(megaco_user).

-export([
         handle_connect/2, handle_connect/3,
         handle_disconnect/3,
         handle_syntax_error/3,        handle_syntax_error/4, 
         handle_message_error/3,       handle_message_error/4, 
         handle_trans_request/3,       handle_trans_request/4, 
         handle_trans_long_request/3,  handle_trans_long_request/4,
         handle_trans_reply/4,         handle_trans_reply/5, 
         handle_trans_ack/4,           handle_trans_ack/5,
	 handle_unexpected_trans/3,    handle_unexpected_trans/4,
         handle_trans_request_abort/4, handle_trans_request_abort/5,
         handle_segment_reply/5,       handle_segment_reply/6
        ]).

-export([
	 start_proxy/0,
	 stop_proxy/0,
	 apply_proxy/1,
	 reply/3,

	 start_transport/2,
	 loop_transport/1, % Internal only
	 send_message/2,
	 resend_message/2
	]).

-include("megaco_test_lib.hrl").
-define(SERVER, ?MODULE).

start_proxy() ->
    yes = global:register_name(?SERVER, self()),
    Pid = megaco_test_lib:proxy_start(?MODULE),
    put(?MODULE, Pid),
    Pid.

stop_proxy() ->
    global:unregister_name(?SERVER),
    Pid = erase(?MODULE),
    unlink(Pid),
    exit(Pid, shutdown).

whereis_proxy() ->
    case get(?MODULE) of
	undefined ->
	    exit(no_server, ?MODULE);
	Pid when is_pid(Pid) ->
	    Pid
    end.

apply_proxy(Fun) ->
    Pid = whereis_proxy(),
    ?APPLY(Pid, Fun),
    ok.

reply(Mod, Line, Fun) when is_function(Fun) ->
    receive
	{?MODULE, Pid, UserCallback} ->
	    UserReply = Fun(UserCallback),
	    Pid ! {?MODULE, self(), UserReply},
	    UserReply;
	Other ->
	    megaco_test_lib:error(Other, Mod, Line),
	    {error, Other}
%%     after 1000 ->
%% 	       megaco_test_lib:error(timeout, Mod, Line),
%% 	       {error, timeout}
    end.
  
call(UserCallback) ->
    Request = {?MODULE, self(), UserCallback},
    case global:whereis_name(?SERVER) of
	undefined ->
	    exit({no_server, ?SERVER, Request});
        Pid when is_pid(Pid) ->
	    ?LOG("call[~p] -> bang request: "
		 "~n   ~p"
		 "~n", [Pid, Request]),
	    Pid ! Request,
	    call_await_reply(Pid)
    end.

call_await_reply(Pid) ->
    receive
	{?MODULE, Pid, UserReply} = _Reply ->
	    case UserReply of
		{ok, Good}   -> Good;
		{error, Bad} -> exit(Bad)
	    end;
	{'EXIT', Pid, Reason} = Bad ->
	    ?LOG("receive test case exit: ~p~n", [Bad]),
	    exit(Reason);
	{'EXIT', _, _Reason} = Bad ->
	    ?LOG("receive unknown exit: ~p~n", [Bad]),
	    call_await_reply(Pid);
	Bad ->
	    ?LOG("receive other: ~p~n", [Bad]),
	    exit(Bad)
    end.

%%----------------------------------------------------------------------
%% Megaco user callback
%%----------------------------------------------------------------------

%% -- handle_connect/2 --

handle_connect(ConnHandle, ProtocolVersion) ->
%%     io:format("~p~p[~p]: handle_connect -> entry with"
%% 	      "~n   ConnHandle:      ~p"
%% 	      "~n   ProtocolVersion: ~p"
%% 	      "~n", 
%%      	      [self(), ?MODULE, ?LINE, ConnHandle, ProtocolVersion]),
    call({connect, ConnHandle, ProtocolVersion, []}).

handle_connect(ConnHandle, ProtocolVersion, Extra) ->
%%     io:format(user,"~p~p[~p]: handle_connect -> entry with"
%% 	      "~n   ConnHandle:      ~p"
%% 	      "~n   ProtocolVersion: ~p"
%% 	      "~n   Extra:           ~p"
%% 	      "~n", 
%%     	      [self(), ?MODULE, ?LINE, ConnHandle, ProtocolVersion, Extra]),
    call({connect, ConnHandle, ProtocolVersion, [Extra]}).


%% -- handle_disconnect/3 --

handle_disconnect(ConnHandle, ProtocolVersion, Reason) ->
    %%     io:format("~w:~w:~p:handle_disconnect -> entry with"
    %% 	      "~n   ConnHandle:      ~p"
    %% 	      "~n   ProtocolVersion: ~p"
    %% 	      "~n   Reason:          ~p"
    %% 	      "~n", 
    %% 	      [?MODULE, ?LINE, self(), ConnHandle, ProtocolVersion, Reason]),
    call({disconnect, ConnHandle, ProtocolVersion, [Reason]}).


%% -- handle_syntax_error/3,4 --

handle_syntax_error(ReceiveHandle, ProtocolVersion, ErrorDescriptor) ->
    call({syntax_error, ReceiveHandle, ProtocolVersion, [ErrorDescriptor]}).

handle_syntax_error(ReceiveHandle, ProtocolVersion, ErrorDescriptor, Extra) ->
    call({syntax_error, ReceiveHandle, ProtocolVersion, [ErrorDescriptor, Extra]}).


%% -- handle_message_error/3,4 --

handle_message_error(ConnHandle, ProtocolVersion, ErrorDescriptor) ->
    call({message_error, ConnHandle, ProtocolVersion, [ErrorDescriptor]}).

handle_message_error(ConnHandle, ProtocolVersion, ErrorDescriptor, Extra) ->
    call({message_error, ConnHandle, ProtocolVersion, [ErrorDescriptor, Extra]}).


%% -- handle_trans_request/3,4 --

handle_trans_request(ConnHandle, ProtocolVersion, ActionRequests) ->
    call({request, ConnHandle, ProtocolVersion, [ActionRequests]}).

handle_trans_request(ConnHandle, ProtocolVersion, ActionRequests, Extra) ->
    call({request, ConnHandle, ProtocolVersion, [ActionRequests, Extra]}).

%% -- handle_trans_long_request/3,4 --

handle_trans_long_request(ConnHandle, ProtocolVersion, RequestData) ->
    call({long_request, ConnHandle, ProtocolVersion, [RequestData]}).

handle_trans_long_request(ConnHandle, ProtocolVersion, RequestData, Extra) ->
    call({long_request, ConnHandle, ProtocolVersion, [RequestData, Extra]}).


%% -- handle_trans_relpy/4,5 --

handle_trans_reply(ConnHandle, ProtocolVersion, UserReply, UserData) ->
    call({reply, ConnHandle, ProtocolVersion, [UserReply, UserData]}).

handle_trans_reply(ConnHandle, ProtocolVersion, UserReply, UserData, Extra) ->
    call({reply, ConnHandle, ProtocolVersion, [UserReply, UserData, Extra]}).


%% -- handle_trans_relpy/4,5 --

handle_trans_ack(ConnHandle, ProtocolVersion, AckStatus, AckData) ->
    call({ack, ConnHandle, ProtocolVersion, [AckStatus, AckData]}).

handle_trans_ack(ConnHandle, ProtocolVersion, AckStatus, AckData, Extra) ->
    call({ack, ConnHandle, ProtocolVersion, [AckStatus, AckData, Extra]}).


%% -- handle_unexpected_trans/3,4 --

handle_unexpected_trans(ConnHandle, ProtocolVersion, Trans) ->
    call({unepected_trans, ConnHandle, ProtocolVersion, [Trans]}).

handle_unexpected_trans(ConnHandle, ProtocolVersion, Trans, Extra) ->
    call({unepected_trans, ConnHandle, ProtocolVersion, [Trans, Extra]}).


%% -- handle_trans_request_abort/4,5 --

handle_trans_request_abort(ConnHandle, ProtocolVersion, TransId, Pid) ->
    call({request_abort, ConnHandle, ProtocolVersion, [TransId, Pid]}).

handle_trans_request_abort(ConnHandle, ProtocolVersion, TransId, Pid, Extra) ->
    call({request_abort, ConnHandle, ProtocolVersion, [TransId, Pid, Extra]}).


%% -- handle_segment_reply/5,6 --

handle_segment_reply(ConnHandle, ProtocolVersion, TransId, SN, SC) ->
    call({segment_reply, ConnHandle, ProtocolVersion, [TransId, SN, SC]}).

handle_segment_reply(ConnHandle, ProtocolVersion, TransId, SN, SC, Extra) ->
    call({segment_reply, ConnHandle, ProtocolVersion, [TransId, SN, SC, Extra]}).


%%----------------------------------------------------------------------
%% The ultimate Megaco transport callback
%%----------------------------------------------------------------------

start_transport(MgReceiveHandle, MgcReceiveHandle) ->
    MgControlPid  = spawn_link(?MODULE, loop_transport, [self()]),
    MgSendHandle  = {MgcReceiveHandle, MgControlPid},
    MgcControlPid = spawn_link(?MODULE, loop_transport, [self()]),
    MgcSendHandle = {MgReceiveHandle, MgcControlPid},
    SendHandle    = {MgSendHandle, MgcSendHandle},
    {ok, MgControlPid, SendHandle}.

loop_transport(Parent) ->
    receive
	{'EXIT', _Pid, Reason} = Error ->
	    ok = io:format("transport stopped: ~p~n", [{Parent, Error}]),
	    exit(Reason)
    end.

send_message(Handles, Bin) ->
    ?LOG("send_message -> entry with"
	 "~n   Handles: ~p"
	 "~n", [Handles]),    
    case megaco_tc_controller:lookup(allow_send_message) of
	{value, ok} ->
	    do_send_message(Handles, Bin);
	{value, {fail, Reason}} ->
	    {error, Reason};
	{value, {cancel, Reason}} ->
	    {cancel, Reason};
	{value, {skip, Result}} ->
	    Result;
	false ->
	    do_send_message(Handles, Bin)
    end.

resend_message(Handles, Bin) ->
    ?LOG("resend_message -> entry with"
	 "~n   Handles: ~p"
	 "~n", [Handles]),    
    case megaco_tc_controller:lookup(allow_resend_message) of
	{value, ok} ->
	    do_send_message(Handles, Bin);
	{value, {fail, Reason}} ->
	    {error, Reason};
	{value, {cancel, Reason}} ->
	    {cancel, Reason};
	{value, {skip, Result}} ->
	    Result;
	false ->
	    do_send_message(Handles, Bin)
    end.

do_send_message({{RH, Pid} = LocalSendHandle, RemoteSendHandle}, Bin) ->
    ?LOG("do_send_message -> entry with"
	 "~n   RH:               ~p"
	 "~n   Pid:              ~p"
	 "~n   RemoteSendHandle: ~p"
	 "~n", [RH, Pid, RemoteSendHandle]),    
    SwappedSendHandle = {RemoteSendHandle, LocalSendHandle},
    case megaco_tc_controller:lookup(extra_transport_info) of
	{value, Extra} ->
	    megaco:receive_message(RH, Pid, SwappedSendHandle, Bin, Extra);
	_ ->
	    megaco:receive_message(RH, Pid, SwappedSendHandle, Bin)
    end.
