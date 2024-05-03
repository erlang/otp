%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2024. All Rights Reserved.
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
-moduledoc """
Interface module to UDP transport protocol for Megaco/H.248.

This module contains the public interface to the UDP/IP version transport
protocol for Megaco/H.248.
""".

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
-doc(#{equiv => get_stats/2}).
get_stats() ->
    megaco_stats:get_stats(megaco_udp_stats).

-doc(#{equiv => get_stats/2}).
get_stats(SH) when is_record(SH, send_handle) ->
    megaco_stats:get_stats(megaco_udp_stats, SH).

-doc """
get_stats(SendHandle, Counter) -> {ok, CounterStats} | {error, Reason}

Retreive the UDP related (SNMP) statistics counters.
""".
get_stats(SH, Counter) 
  when is_record(SH, send_handle) andalso is_atom(Counter) ->
    megaco_stats:get_stats(megaco_udp_stats, SH, Counter).


%%-----------------------------------------------------------------
%% Func: reset_stats/0, reaet_stats/1
%% Description: Reset statistics (counters) for TCP
%%-----------------------------------------------------------------
-doc(#{equiv => reset_stats/1}).
reset_stats() ->
    megaco_stats:reset_stats(megaco_udp_stats).

-doc """
reset_stats(SendHandle) -> void()

Reset all TCP related (SNMP) statistics counters.
""".
reset_stats(SH) when is_record(SH, send_handle) ->
    megaco_stats:reset_stats(megaco_udp_stats, SH).



%%-----------------------------------------------------------------
%% Func: start_transport
%% Description: Starts the UDP transport service
%%-----------------------------------------------------------------
-doc """
start_transport() -> {ok, TransportRef}

This function is used for starting the UDP/IP transport service. Use
exit(TransportRef, Reason) to stop the transport service.
""".
start_transport() ->
    (catch megaco_stats:init(megaco_udp_stats)),
    megaco_udp_sup:start_link().


%%-----------------------------------------------------------------
%% Func: stop_transport
%% Description: Stop the UDP transport service
%%-----------------------------------------------------------------
-doc false.
stop_transport(Pid) ->
    (catch unlink(Pid)), 
    stop_transport(Pid, shutdown).

stop_transport(Pid, Reason) ->
    exit(Pid, Reason).


%%-----------------------------------------------------------------
%% Func: open
%% Description: Function is used when opening an UDP socket
%%-----------------------------------------------------------------
-doc """
open(TransportRef, OptionList) -> {ok, Handle, ControlPid} | {error, Reason}

This function is used to open an UDP/IP socket.

- **`module`** - The option makes it possible for the user to provide their own
  callback module. The functions `receive_message/4` or
  `process_received_message/4` of this module is called when a new message is
  received. Which one depends on the size of the message:

  - **`small`** - receive_message

  - **`large`** - process_received_message

  Default value is _megaco_.

- **`inet_backend`** - Choose the inet-backend.

  This option make it possible to use a different inet-backend ('default',
  'inet' or 'socket').

  Default is `default` (system default).
""".
open(SupPid, Options) ->
    Mand = [port, receive_handle],
    case parse_options(Options, #megaco_udp{}, Mand) of
	{ok, #megaco_udp{port         = Port,
			 options      = Opts,
			 inet_backend = IB} = UdpRec} ->

	    %%------------------------------------------------------
	    %% Setup the socket
	    IpOpts =
                case IB of
                    default ->
                        [];
                    IB ->
                        [{inet_backend, IB}]
                end ++
                [binary, {reuseaddr, true}, {active, once} |
                 post_process_opts(IB, Opts)],
	    case (catch gen_udp:open(Port, IpOpts)) of
		{ok, Socket} ->
		    ?udp_debug(UdpRec, "udp open", []),
		    NewUdpRec = UdpRec#megaco_udp{socket = Socket},
		    case start_udp_server(SupPid, NewUdpRec) of
			{ok, ControlPid} ->
			    _ = gen_udp:controlling_process(Socket, ControlPid),
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


%% In some cases we must bind and therefor we must have the
%% ip (or ifaddr) option.
post_process_opts(socket = _IB, Opts) ->
    case os:type() of
	{win32, nt} ->
	    %% We must bind, and therefor we must provide a "proper" address.
	    %% Therefor...we need to figure out our domain.
	    post_process_opts(Opts);
	_ ->
	    Opts
    end;
post_process_opts(_IB, Opts) ->
    Opts.


%% Socket on Windows: We need the ip (or ifaddr) option
post_process_opts(Opts) ->
    case lists:keymember(ip, 1, Opts) orelse
	lists:keymember(ifaddr, 1, Opts) of
	true ->
	    %% No need to do anything, user has provided an address
	    Opts;
	false ->
	    %% We need to figure out a proper address and provide 
	    %% the ip option our selves.
	    post_process_opts2(Opts)
    end.
	
post_process_opts2(Opts) ->
    case lists:member(inet, Opts) of
	true ->
	    post_process_opts3(inet, Opts);
	false ->
	    case lists:member(inet6, Opts) of
		true ->
		    post_process_opts3(inet6, Opts);
		false ->
		    post_process_opts3(inet, Opts)
	    end
    end.

post_process_opts3(Domain, Opts) ->
    case net:getifaddrs(Domain) of
	{ok, IfAddrs} ->
	    post_process_opts4(Domain, IfAddrs, Opts);
	{error, _} ->
	    Opts
    end.

post_process_opts4(_Domain, [] = _IfAddrs, Opts) ->
    Opts;
post_process_opts4(inet,
		   [#{addr := #{family := inet,
				addr   := {A, B, _, _}}} | IfAddrs],
		   Opts)
  when (A =:= 127) orelse ((A =:= 169) andalso (B =:= 254)) ->
    post_process_opts4(inet, IfAddrs, Opts);
post_process_opts4(inet,
		   [#{addr   := #{family := inet,
				  addr   := Addr},
		      flags  := Flags} | IfAddrs],
		   Opts) ->
    case lists:member(up, Flags) of
	true ->
	    [{ip, Addr} | Opts];
	false ->
	    post_process_opts4(inet, IfAddrs, Opts)
    end;
post_process_opts4(inet6,
		   [#{addr := #{family := inet6,	
				addr   := {A, _, _, _, _, _, _, _}}} | IfAddrs],
		   Opts)
  when (A =:= 0) orelse (A =:= 16#fe80) ->
    post_process_opts4(inet6, IfAddrs, Opts);
post_process_opts4(inet6,
		   [#{addr  := #{family := inet6,
				 addr   := Addr},
		      flags := Flags} | IfAddrs],
		   Opts) ->
    %% The loopback should really have been covered above, but just in case...
    case lists:member(up, Flags) andalso (not lists:member(loopback, Flags)) of
	true ->
	    [{ip, Addr} | Opts];
	false ->
	    post_process_opts4(inet6, IfAddrs, Opts)
    end.

    

%%-----------------------------------------------------------------
%% Func: socket
%% Description: Returns the inet socket
%%-----------------------------------------------------------------
-doc """
socket(Handle) -> Socket

This function is used to convert a socket_handle() to a inet_socket().
inet_socket() is a plain socket, see the inet module for more info.
""".
socket(SH) when is_record(SH, send_handle) ->
    SH#send_handle.socket;
socket(Socket) ->
    Socket.


-doc """
upgrade_receive_handle(ControlPid, NewHandle) -> ok

Update the receive handle of the control process (e.g. after having changed
protocol version).

[](){: #stats }
""".
upgrade_receive_handle(Pid, NewHandle) 
  when is_pid(Pid) andalso is_record(NewHandle, megaco_receive_handle) ->
    megaco_udp_server:upgrade_receive_handle(Pid, NewHandle).


%%-----------------------------------------------------------------
%% Func: create_send_handle
%% Description: Function is used for creating the handle used when 
%%    sending data on the UDP socket
%%-----------------------------------------------------------------
-doc """
create_send_handle(Handle, Host, Port) -> send_handle()

Creates a send handle from a transport handle. The send handle is intended to be
used by megaco_udp:send_message/2.
""".
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
-doc """
send_message(SendHandle, Msg) -> ok

Sends a message on a socket. The send handle is obtained by
megaco_udp:create_send_handle/3. Increments the NumOutMessages and NumOutOctets
counters if message successfully sent. In case of a failure to send, the
NumErrors counter is _not_ incremented. This is done elsewhere in the megaco
app.
""".
send_message(SH, Data) when is_record(SH, send_handle) ->
    #send_handle{socket = Socket, addr = Addr, port = Port} = SH,
    Res = gen_udp:send(Socket, Addr, Port, Data),
    _ = case Res of
            ok ->
                incNumOutMessages(SH),
                incNumOutOctets(SH, byte_size(Data));
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
-doc """
block(Handle) -> ok

Stop receiving incoming messages on the socket.
""".
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
-doc """
unblock(Handle) -> ok

Starting to receive incoming messages from the socket again.

[](){: #upgrade_receive_handle }
""".
unblock(SH) when is_record(SH, send_handle) ->
    unblock(SH#send_handle.socket);
unblock(Socket) ->
    ?udp_debug({socket, Socket}, "udp unblock", []),
    inet:setopts(Socket, [{active, once}]).


%%-----------------------------------------------------------------
%% Func: close
%% Description: Function is used for closing the UDP socket
%%-----------------------------------------------------------------
-doc """
close(Handle) -> ok

This function is used for closing an active UDP socket.
""".
close(#send_handle{socket = Socket}) ->
    close(Socket);
close(Socket) ->
    ?udp_debug({socket, Socket}, "udp close", []),
    case inet:info(Socket) of
        #{owner := ControlPid} = _Info when is_pid(ControlPid) ->
	    (catch megaco_udp_server:stop(ControlPid));
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
	serialize when is_boolean(Val) ->
	    parse_options(T, UdpRec#megaco_udp{serialize = Val}, Mand2);
	inet_backend when (Val =:= default) orelse
                          (Val =:= inet) orelse
                          (Val =:= socket)  ->
	    parse_options(T, UdpRec#megaco_udp{inet_backend = Val}, Mand2);
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

%% incNumErrors(SH) ->
%%     incCounter({SH, medGwyGatewayNumErrors}, 1).

%%-----------------------------------------------------------------

%% formated_timestamp() ->
%%     format_timestamp(os:timestamp()).

%% format_timestamp(TS) ->
%%     megaco:format_timestamp(TS).

%% d(F) ->
%%     d(F, []).

%% d(F, A) ->
%%     io:format("*** [~s] ~p " ++ F ++ "~n", [formated_timestamp(), self() | A]).
