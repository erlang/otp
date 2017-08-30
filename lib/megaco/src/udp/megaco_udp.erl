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
%% Purpose: Interface to the UDP transport module for Megaco/H.248
%%-----------------------------------------------------------------
-module(megaco_udp).

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/src/udp/megaco_udp.hrl").

-record(send_handle, {socket, addr, port}).


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([
	 start_transport/0, stop_transport/1,
	 open/2,
	 socket/1,
	 create_send_handle/3,
	 send_message/2,
	 close/1,
	 block/1,
	 unblock/1,

         upgrade_receive_handle/2
	]).

%% Statistics exports
-export([get_stats/0, get_stats/1, get_stats/2,
	 reset_stats/0, reset_stats/1]).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: get_stats/0, get_stats/1, get_stats/2
%% Description: Retreive statistics (counters) for TCP
%%-----------------------------------------------------------------
get_stats() ->
    megaco_stats:get_stats(megaco_udp_stats).

get_stats(SH) when is_record(SH, send_handle) ->
    megaco_stats:get_stats(megaco_udp_stats, SH).

get_stats(SH, Counter) 
  when is_record(SH, send_handle) andalso is_atom(Counter) ->
    megaco_stats:get_stats(megaco_udp_stats, SH, Counter).


%%-----------------------------------------------------------------
%% Func: reset_stats/0, reaet_stats/1
%% Description: Reset statistics (counters) for TCP
%%-----------------------------------------------------------------
reset_stats() ->
    megaco_stats:reset_stats(megaco_udp_stats).

reset_stats(SH) when is_record(SH, send_handle) ->
    megaco_stats:reset_stats(megaco_udp_stats, SH).



%%-----------------------------------------------------------------
%% Func: start_transport
%% Description: Starts the UDP transport service
%%-----------------------------------------------------------------
start_transport() ->
    (catch megaco_stats:init(megaco_udp_stats)),
    megaco_udp_sup:start_link().


%%-----------------------------------------------------------------
%% Func: stop_transport
%% Description: Stop the UDP transport service
%%-----------------------------------------------------------------
stop_transport(Pid) ->
    (catch unlink(Pid)), 
    stop_transport(Pid, shutdown).

stop_transport(Pid, Reason) ->
    exit(Pid, Reason).


%%-----------------------------------------------------------------
%% Func: open
%% Description: Function is used when opening an UDP socket
%%-----------------------------------------------------------------
open(SupPid, Options) ->
    Mand = [port, receive_handle],
    case parse_options(Options, #megaco_udp{}, Mand) of
	{ok, UdpRec} ->

	    %%------------------------------------------------------
	    %% Setup the socket
	    IpOpts = [binary, {reuseaddr, true}, {active, once} |
		      UdpRec#megaco_udp.options],

	    case (catch gen_udp:open(UdpRec#megaco_udp.port, IpOpts)) of
		{ok, Socket} ->
		    ?udp_debug(UdpRec, "udp open", []),
		    NewUdpRec = UdpRec#megaco_udp{socket = Socket},
		    case start_udp_server(SupPid, NewUdpRec) of
			{ok, ControlPid} ->
			    gen_udp:controlling_process(Socket, ControlPid),
			    {ok, Socket, ControlPid};
			{error, Reason} ->
			    Error = {error, {could_not_start_udp_server, Reason}},
			    ?udp_debug({socket, Socket}, "udp close", []),
			    gen_udp:close(Socket),
			    Error

		    end;
		{'EXIT', Reason} ->
		    Error = {error, {could_not_open_udp_port, Reason}},
		    ?udp_debug(UdpRec, "udp open exited", [Error]),
		    Error;
		{error, Reason} ->
		    Error = {error, {could_not_open_udp_port, Reason}},
		    ?udp_debug(UdpRec, "udp open failed", [Error]),
		    Error
	    end;
	{error, Reason} = Error ->
	    ?udp_debug(#megaco_udp{}, "udp open failed",
		       [Error, {options, Options}]),
	    {error, Reason}
    end.


%%-----------------------------------------------------------------
%% Func: socket
%% Description: Returns the inet socket
%%-----------------------------------------------------------------
socket(SH) when is_record(SH, send_handle) ->
    SH#send_handle.socket;
socket(Socket) ->
    Socket.


upgrade_receive_handle(Pid, NewHandle) 
  when is_pid(Pid) andalso is_record(NewHandle, megaco_receive_handle) ->
    megaco_udp_server:upgrade_receive_handle(Pid, NewHandle).


%%-----------------------------------------------------------------
%% Func: create_send_handle
%% Description: Function is used for creating the handle used when 
%%    sending data on the UDP socket
%%-----------------------------------------------------------------
create_send_handle(Socket, {_, _, _, _} = Addr, Port) ->
    do_create_send_handle(Socket, Addr, Port);
create_send_handle(Socket, Addr0, Port) ->
    {ok, Addr} = inet:getaddr(Addr0, inet),
    do_create_send_handle(Socket, Addr, Port).

do_create_send_handle(Socket, Addr, Port) ->
    %% If neccessary create snmp counter's
    SH = #send_handle{socket = Socket, addr = Addr, port = Port},
    maybe_create_snmp_counters(SH),
    SH.


maybe_create_snmp_counters(SH) ->
    Counters = [medGwyGatewayNumInMessages, 
		medGwyGatewayNumInOctets, 
		medGwyGatewayNumOutMessages, 
		medGwyGatewayNumOutOctets, 
		medGwyGatewayNumErrors],
    %% Only need to check one of them, since either all of them exist
    %% or none of them exist:
    Key = {SH, medGwyGatewayNumInMessages},
    case (catch ets:lookup(megaco_udp_stats, Key)) of
	[] ->
	    create_snmp_counters(SH, Counters);
	[_] ->
	    ok;
	_ ->
	    ok
    end.

create_snmp_counters(_SH, []) ->
    ok;
create_snmp_counters(SH, [Counter|Counters]) ->
    Key = {SH, Counter},
    ets:insert(megaco_udp_stats, {Key, 0}),
    create_snmp_counters(SH, Counters).


%%-----------------------------------------------------------------
%% Func: send_message
%% Description: Function is used for sending data on the UDP socket
%%-----------------------------------------------------------------
send_message(SH, Data) when is_record(SH, send_handle) ->
    #send_handle{socket = Socket, addr = Addr, port = Port} = SH,
    Res = gen_udp:send(Socket, Addr, Port, Data),
    case Res of
	ok ->
	    incNumOutMessages(SH),
	    incNumOutOctets(SH, size(Data));
	_ ->
	    ok
    end,
    Res;
send_message(SH, _Data) ->
    {error, {bad_send_handle, SH}}.


%%-----------------------------------------------------------------
%% Func: block
%% Description: Function is used for blocking incomming messages
%%              on the TCP socket
%%-----------------------------------------------------------------
block(SH) when is_record(SH, send_handle) ->
    block(SH#send_handle.socket);
block(Socket) ->
    ?udp_debug({socket, Socket}, "udp block", []),
    inet:setopts(Socket, [{active, false}]).
      

%%-----------------------------------------------------------------
%% Func: unblock
%% Description: Function is used for blocking incomming messages
%%              on the TCP socket
%%-----------------------------------------------------------------
unblock(SH) when is_record(SH, send_handle) ->
    unblock(SH#send_handle.socket);
unblock(Socket) ->
    ?udp_debug({socket, Socket}, "udp unblock", []),
    inet:setopts(Socket, [{active, once}]).


%%-----------------------------------------------------------------
%% Func: close
%% Description: Function is used for closing the UDP socket
%%-----------------------------------------------------------------
close(#send_handle{socket = Socket}) ->
    close(Socket);
close(Socket) ->
    ?udp_debug({socket, Socket}, "udp close", []),
    case erlang:port_info(Socket, connected) of
	{connected, ControlPid} ->
	    megaco_udp_server:stop(ControlPid);
	undefined ->
	    {error, already_closed}
    end.


%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start_udp_server/1
%% Description: Function is used for starting up a connection
%%              process
%%-----------------------------------------------------------------
start_udp_server(SupPid, UdpRec) ->
    megaco_udp_sup:start_child(SupPid, UdpRec).


%%-----------------------------------------------------------------
%% Func: parse_options
%% Description: Function that parses the options sent to the UDP 
%%              module.
%%-----------------------------------------------------------------
parse_options([{Tag, Val} | T], UdpRec, Mand) ->
    Mand2 = Mand -- [Tag],
    case Tag of
	port ->
	    parse_options(T, UdpRec#megaco_udp{port = Val}, Mand2);
	udp_options when is_list(Val) ->
	    parse_options(T, UdpRec#megaco_udp{options = Val}, Mand2);
	receive_handle ->
	    parse_options(T, UdpRec#megaco_udp{receive_handle = Val}, Mand2);
	module when is_atom(Val) ->
	    parse_options(T, UdpRec#megaco_udp{module = Val}, Mand2);
	serialize when (Val =:= true) orelse (Val =:= false) ->
	    parse_options(T, UdpRec#megaco_udp{serialize = Val}, Mand2);
        Bad ->
	    {error, {bad_option, Bad}}
    end;
parse_options([], UdpRec, []) ->
    {ok, UdpRec};
parse_options([], _UdpRec, Mand) ->
    {error, {missing_options, Mand}};
parse_options(BadList, _UdpRec, _Mand) ->
    {error, {bad_option_list, BadList}}.


%%-----------------------------------------------------------------
%% Func: incNumOutMessages/1, incNumOutOctets/2, incNumErrors/1
%% Description: SNMP counter increment functions
%%              
%%-----------------------------------------------------------------
incNumOutMessages(SH) ->
    incCounter({SH, medGwyGatewayNumOutMessages}, 1).

incNumOutOctets(SH, NumOctets) ->
    incCounter({SH, medGwyGatewayNumOutOctets}, NumOctets).

incCounter(Key, Inc) ->
    ets:update_counter(megaco_udp_stats, Key, Inc).

% incNumErrors(SH) ->
%     incCounter({SH, medGwyGatewayNumErrors}, 1).
