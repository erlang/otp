%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% 
%% Description:
%%    This file handles the H.248 UDP connections.
%%
%%-----------------------------------------------------------------
-module(megaco_udp_server).

-behaviour(gen_server).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("megaco/src/udp/megaco_udp.hrl").
-include_lib("megaco/src/app/megaco_internal.hrl"). 


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([
	 start_link/1,
	 stop/1,

	 upgrade_receive_handle/2
	]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([
	 init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 code_change/3, 
	 terminate/2,

	 handle_received_message/5
	]).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start_link/1
%% Description: Starts the process that keeps track of an UDP 
%%              socket.
%%-----------------------------------------------------------------
start_link(Arg) ->
    gen_server:start_link(?MODULE, Arg, []).

%%-----------------------------------------------------------------
%% Func: stop/1
%% Description: Stops the process that keeps track of an UDP 
%%              socket.
%%-----------------------------------------------------------------
stop(Pid) ->
    call(Pid, stop).


upgrade_receive_handle(Pid, NewHandle) ->
    call(Pid, {upgrade_receive_handle, NewHandle}).

%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%% Description: Init funcion for the generic server
%%-----------------------------------------------------------------
init(Arg) ->
    ?udp_debug(Arg, "udp server starting", [self()]),
    {ok, Arg}.

%%-----------------------------------------------------------------
%% Func: terminate/2
%% Description: Termination function for the generic server
%%-----------------------------------------------------------------
terminate(Reason, State) ->
    ?udp_debug(State, "udp server terminating", [self(), Reason]),
    ok.

%%-----------------------------------------------------------------
%% Func: handle_call/3
%% Description: Handling call messages (really just stop and garbage)
%%-----------------------------------------------------------------
handle_call(stop, _From, UdpRec) ->
    Reply = do_stop(UdpRec),
    {stop, shutdown, Reply, UdpRec};
handle_call({upgrade_receive_handle, NewHandle}, _From, UdpRec) ->
    {reply, ok, UdpRec#megaco_udp{receive_handle = NewHandle}};
handle_call(Req, From, UdpRec) ->
    warning_msg("received unexpected request from ~p: "
		"~n~p", [From, Req]),
    {reply, {error, {invalid_request, Req}}, UdpRec}.

%%-----------------------------------------------------------------
%% Func: handle_cast/2
%% Description: Handling cast messages (really just stop and garbage)
%%-----------------------------------------------------------------
handle_cast(stop, UdpRec) ->
    do_stop(UdpRec),
    {stop, shutdown, UdpRec};
handle_cast(Msg, UdpRec) ->
    warning_msg("received unexpected message: "
		"~n~w", [Msg]),
    {noreply, UdpRec}.

%%-----------------------------------------------------------------
%% Func: handle_info/2
%% Description: Handling non call/cast messages. Incomming messages
%%              from the socket and exit codes.
%%-----------------------------------------------------------------
handle_info({udp, _UdpId, Ip, Port, Msg}, 
	    #megaco_udp{serialize = false} = UdpRec) ->
    #megaco_udp{socket = Socket, module = Mod, receive_handle = RH} = UdpRec,
    SH = megaco_udp:create_send_handle(Socket, Ip, Port), 
    MsgSize = size(Msg),
    incNumInMessages(SH),
    incNumInOctets(SH, MsgSize),
    case MsgSize of
	Sz when Sz < ?GC_MSG_LIMIT ->
	    apply(Mod, receive_message, [RH, self(), SH, Msg]);
	Sz ->
	    receive_message(Mod, RH, SH, Sz, Msg)
    end,
    inet:setopts(Socket, [{active, once}]),
    {noreply, UdpRec};
handle_info({udp, _UdpId, Ip, Port, Msg}, 
	    #megaco_udp{serialize = true} = UdpRec) ->
    #megaco_udp{socket = Socket, module = Mod, receive_handle = RH} = UdpRec,
    SH = megaco_udp:create_send_handle(Socket, Ip, Port), 
    MsgSize = size(Msg),
    incNumInMessages(SH),
    incNumInOctets(SH, MsgSize),
    process_received_message(Mod, RH, SH, Msg),
    inet:setopts(Socket, [{active, once}]),
    {noreply, UdpRec};
handle_info(Info, UdpRec) ->
    warning_msg("received unexpected info: "
		"~n~w", [Info]),
    {noreply, UdpRec}.


process_received_message(Mod, RH, SH, Msg) ->
    case (catch Mod:process_received_message(RH, self(), SH, Msg)) of
	ok ->
	    ok;
	Error ->
	    error_msg("failed processing received message: "
		      "~n~p", [Error]),
	    ok
    end.


receive_message(Mod, RH, SendHandle, Length, Msg) ->
    Opts = [link , {min_heap_size, ?HEAP_SIZE(Length)}],
    spawn_opt(?MODULE, handle_received_message,
               [Mod, RH, self(), SendHandle, Msg], Opts).


handle_received_message(Mod, RH, Parent, SH, Msg) ->
    Mod:process_received_message(RH, Parent, SH, Msg),
    unlink(Parent),
    exit(normal).
   

    
%%-----------------------------------------------------------------
%% Func: code_change/3
%% Descrition: Handles code change messages during upgrade.
%%-----------------------------------------------------------------
code_change(_Vsn, State, _Extra) ->
    {ok, State}.

do_stop(#megaco_udp{socket = Socket}) ->
    gen_udp:close(Socket).


%%-----------------------------------------------------------------
%% Func: incNumInMessages/1, incNumInOctets/2, incNumErrors/1
%% Description: SNMP counter increment functions
%%              
%%-----------------------------------------------------------------
incNumInMessages(SH) ->
    incCounter({SH, medGwyGatewayNumInMessages}, 1).

incNumInOctets(SH, NumOctets) ->
    incCounter({SH, medGwyGatewayNumInOctets}, NumOctets).

incCounter(Key, Inc) ->
    ets:update_counter(megaco_udp_stats, Key, Inc).

% incNumErrors(SH) ->
%     incCounter({SH, medGwyGatewayNumErrors}, 1).


%% info_msg(F, A) ->
%%     ?megaco_info("UDP server: " ++ F, A).

warning_msg(F, A) ->
    ?megaco_warning("UDP server: " ++ F, A).
  
error_msg(F, A) ->
    ?megaco_error("UDP server: " ++ F, A).
  

call(Pid, Req) ->
    gen_server:call(Pid, Req, infinity).
