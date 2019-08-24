%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2019. All Rights Reserved.
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
%% Purpose: This module takes the role of the main megaco module for
%%          the transport module. It is used when  delivering 
%%          received messages. The purpose is to be able to do
%%          various forms of filtering before passing the message 
%%          the the megaco stack (by calling the megaco module).
%%          It can be controlled with the following flags:
%%          allow_recv_message - Shall the received message be 
%%                               delivered.
%%          extra_transport_info - Provide extra info from the transport
%%                                 module.
%%----------------------------------------------------------------------
-module(megaco_test_deliver).


%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("megaco/src/udp/megaco_udp.hrl").
-include("megaco_test_lib.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
	 process_received_message/4, process_received_message/5,
	 receive_message/4, receive_message/5
	]).



%%======================================================================
%% External functions
%%======================================================================

process_received_message(ReceiveHandle, ControlPid, SendHandle, BinMsg) ->
    i("process_received_message -> entry with"
      "~n   ReceiveHandle: ~p"
      "~n   ControlPid:    ~p"
      "~n   SendHandle:    ~p", 
      [ReceiveHandle, ControlPid, SendHandle]),
    case allow_recv_message() of
	true ->
	    i("process_received_message -> allowed recv msg"),
	    case extra_transport_info() of
		{value, Extra} ->
		    i("process_received_message -> extra_transport_info: "
		      "~n   Extra: ~p", [Extra]),
		    megaco:process_received_message(ReceiveHandle, ControlPid, 
						    SendHandle, BinMsg,
						    Extra);
		_ ->
		    i("process_received_message -> no extra_transport_info"),
		    megaco:process_received_message(ReceiveHandle, ControlPid, 
						    SendHandle, BinMsg)
	    end;
	false ->
	    i("process_received_message -> recv msg not allowed"),
	    ok;
	{false, Reason} ->
	    i("process_received_message -> recv msg not allowed"
	      "~n   Reason: ~p", [Reason]),
	    exit(Reason)
    end.
	    

process_received_message(ReceiveHandle, ControlPid, SendHandle, BinMsg, Extra) ->
    i("process_received_message -> entry with"
      "~n   ReceiveHandle: ~p"
      "~n   ControlPid:    ~p"
      "~n   SendHandle:    ~p"
      "~n   Extra:         ~p", 
      [ReceiveHandle, ControlPid, SendHandle, Extra]),
    case allow_recv_message() of
	true ->
	    i("process_received_message -> allowed recv msg"),
	    megaco:process_received_message(ReceiveHandle, ControlPid, 
					    SendHandle, BinMsg,
					    Extra);
	false ->
	    i("process_received_message -> recv msg not allowed"),
	    ok;
	{false, Reason} ->
	    i("process_received_message -> recv msg not allowed"
	      "~n   Reason: ~p", [Reason]),
	    exit(Reason)
    end.

receive_message(ReceiveHandle, ControlPid, SendHandle, BinMsg) ->
    i("receive_message -> entry with"
      "~n   ReceiveHandle: ~p"
      "~n   ControlPid:    ~p"
      "~n   SendHandle:    ~p", 
      [ReceiveHandle, ControlPid, SendHandle]),
    case allow_recv_message() of
	true ->
	    i("receive_message -> allowed recv msg"),
	    case extra_transport_info() of
		{value, Extra} ->
		    i("receive_message -> extra_transport_info: "
		      "~n   Extra: ~p", [Extra]),
		    megaco:receive_message(ReceiveHandle, ControlPid,
					   SendHandle, BinMsg, Extra);
		_ ->
		    i("receive_message -> no extra_transport_info"),
		    megaco:receive_message(ReceiveHandle, ControlPid, 
					   SendHandle, BinMsg)
	    end;
	false ->
	    i("receive_message -> recv msg not allowed"),
	    ok;
	{false, Reason} ->
	    i("receive_message -> recv msg not allowed"
	      "~n   Reason: ~p", [Reason]),
	    exit(Reason)
    end.

receive_message(ReceiveHandle, ControlPid, SendHandle, BinMsg, Extra) ->
    i("receive_message -> entry with"
      "~n   ReceiveHandle: ~p"
      "~n   ControlPid:    ~p"
      "~n   SendHandle:    ~p"
      "~n   Extra:         ~p", 
      [ReceiveHandle, ControlPid, SendHandle, Extra]),
    case allow_recv_message() of
	true ->
	    i("receive_message -> allowed recv msg"),
	    megaco:receive_message(ReceiveHandle, ControlPid,
				   SendHandle, BinMsg,
				   Extra);
	false ->
	    i("receive_message -> recv msg not allowed"),
	    ok;
	{false, Reason} ->
	    i("receive_message -> recv msg not allowed"
	      "~n   Reason: ~p", [Reason]),
	    exit(Reason)
    end.


%%======================================================================
%% Internal functions
%%======================================================================

allow_recv_message() ->
    case megaco_tc_controller:lookup(allow_recv_message) of
	{error, _} ->
	    true;
	{value, Else} ->
	    Else;
	false ->
	    true
    end.

extra_transport_info() ->
    case megaco_tc_controller:lookup(extra_transport_info) of
	{error, _} ->
	    false;
	Else ->
	    Else
    end.
    
i(F) ->
    i(F, []).

i(F, A) ->
    io:format("*** [~s] ~p ~w:" ++ F ++ "~n", [?FTS(), self(), ?MODULE | A]).
