%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1999-2025. All Rights Reserved.
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
%% Purpose: 
%%      Interface the TPKT (TCP/IP) transport module for Megaco/H.248
%%
%%-----------------------------------------------------------------
-module(megaco_tcp).
-moduledoc """
Interface module to TPKT transport protocol for Megaco/H.248.

This module contains the public interface to the TPKT (TCP/IP) version transport
protocol for Megaco/H.248.

""".

-behaviour(gen_server).


%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-define(megaco_debug, true).
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/src/tcp/megaco_tcp.hrl"). 
-include_lib("megaco/src/app/megaco_internal.hrl"). 


-define(d1(F, A), ?d("~p " ++ F, [self()|A])).
-define(d2(F),    ?d1(F, [])).


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([
	 start_transport/0, %% Start TPKT transport service
	 stop_transport/1,  %% Stop TPKT transport service
	 listen/2,          %% Starts a new listener socket
	 connect/2,         %% Used on client side to connect server
	 socket/1,          %% Returns the inet socket
	 send_message/2,    %% Used to send data on connection
	 block/1,           %% Used to block the socket for incomming
	                    %% messages
	 unblock/1,         %% Used to unblock the node
	 close/1,           %% Used on both sides to close connection

	 upgrade_receive_handle/2
	]).

%% Statistics exports
-export([
	 get_stats/0, get_stats/1, get_stats/2,
	 reset_stats/0, reset_stats/1
	]).

%% -export([tcp_sockets/0]).

-export_type([
         handle/0,
         counter/0
        ]).


%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([
	 start_link/1,       %% Start TCP/IP net server
	 init/1,             %%
	 terminate/2, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2, 
	 code_change/3,
	 start_connection/2
	]).


-doc """
An opaque data type representing a TPKT connection.
""".
-opaque handle() :: inet:socket().

-doc """
Defines the different counters handled by this transport.
""".
-type counter() :: medGwyGatewayNumInMessages |
                   medGwyGatewayNumInOctets |
                   medGwyGatewayNumOutMessages |
                   medGwyGatewayNumOutOctets |
                   medGwyGatewayNumErrors.


%%-----------------------------------------------------------------
%% Server state record
%%-----------------------------------------------------------------
-record(state, {supervisor_pid, linkdb}).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: get_stats/0, get_stats/1, get_stats/2
%% Description: Retreive statistics (counters) for TCP
%%-----------------------------------------------------------------
-doc """
Get all counter values for all known connections.
""".

-spec get_stats() -> {ok, TotalStats} | {error, Reason} when
      TotalStats :: [{Handle, [{Counter, integer()}]}],
      Handle     :: handle(),
      Counter    :: counter(),
      Reason     :: term().

get_stats() ->
    megaco_stats:get_stats(megaco_tcp_stats).


-doc """
Get all counter values for a given (connection) handle.
""".

-spec get_stats(Handle) -> {ok, Stats} | {error, Reason} when
      Handle  :: handle(),
      Stats   :: [{Counter, integer()}],
      Counter :: counter(),
      Reason  :: term().
      
get_stats(Socket) ->
    megaco_stats:get_stats(megaco_tcp_stats, Socket).


-doc """
Get the value of a specific counter.
""".

-spec get_stats(Handle, Counter) -> {ok, integer()} | {error, Reason} when
      Handle  :: handle(),
      Counter :: counter(),
      Reason  :: term().

get_stats(Socket, Counter) ->
    megaco_stats:get_stats(megaco_tcp_stats, Socket, Counter).


%%-----------------------------------------------------------------
%% Func: reset_stats/0, reaet_stats/1
%% Description: Reset statistics (counters) for TCP
%%-----------------------------------------------------------------

-doc """
Reset all counters for all connections.
""".

-spec reset_stats() -> megaco:void().

reset_stats() ->
    megaco_stats:reset_stats(megaco_tcp_stats).


-doc """
Reset all counters for the given connection.
""".

-spec reset_stats(Handle) -> megaco:void() when
      Handle :: handle().

reset_stats(Socket) ->
    megaco_stats:reset_stats(megaco_tcp_stats, Socket).


%%-----------------------------------------------------------------
%% Func: start_transport/0
%% Description: Starts the TPKT transport service
%%-----------------------------------------------------------------
-doc """
This function is used for starting the TCP/IP transport service. Use
exit(TransportRef, Reason) to stop the transport service.
""".

-spec start_transport() -> {ok, TransportRef} when
      TransportRef :: pid().

start_transport() ->
    ?d2("start_transport -> entry"),
    (catch megaco_stats:init(megaco_tcp_stats)),
    megaco_tcp_sup:start_link().


%%-----------------------------------------------------------------
%% Func: stop_transport/1, 2
%% Description: Stop the TPKT transport service
%%-----------------------------------------------------------------
-doc false.
stop_transport(Pid) ->
    (catch unlink(Pid)), 
    stop_transport(Pid, shutdown).

stop_transport(Pid, Reason) ->
    ?d1("stop_transport -> entry with"
	"~n   Pid:    ~p"
	"~n   Reason: ~p", [Pid, Reason]),
    exit(Pid, Reason).


%%-----------------------------------------------------------------
%% Func: listen/2
%% Description: Starts new TPKT listener sockets
%%-----------------------------------------------------------------
-doc """
This function is used for starting new TPKT listening socket for TCP/IP. The
option list contains the socket definitions.

- **`inet_backend`** - Choose the inet-backend.

  This option make it possible to use a different inet-backend ('default',
  'inet' or 'socket').

  Default is `default` (system default).
""".

-spec listen(TransportRef, Options) -> ok when
      TransportRef :: pid() | RegName,
      RegName      :: atom(),
      Options      :: [Option],
      Option       :: {inet_backend,    default | inet | socket} |
                      {port,            inet:port_number()} |
                      {options,         list()} |
                      {receive_handle,  term()}.

listen(SupPid, Parameters) ->
    ?d1("listen -> entry with"
	"~n   SupPid:     ~p"
	"~n   Parameters: ~p", [SupPid, Parameters]),
    ProcList = supervisor:which_children(SupPid),
    case lists:keysearch(megaco_tcp, 1, ProcList) of
	{value, {_Name, Pid, _Type, _Modules}} ->
	    ?d1("listen -> found listener: "
		"~n   Pid: ~p", [Pid]),
	    call(Pid, {add_listener, Parameters});
	false ->
	    {error, no_tcp_server}
    end.	    


%%-----------------------------------------------------------------
%% Func: connect
%% Description: Function is used when opening an TCP socket 
%%              at the MG side when trying to connect an MGC
%%-----------------------------------------------------------------
-doc """
This function is used to open a TPKT connection.

- **`module`** - This option makes it possible for the user to provide their own
  callback module. The `receive_message/4` or `process_received_message/4`
  functions of this module is called when a new message is received. Which one
  is called depends on the size of the message;

  - **`small`** - receive_message

  - **`large`** - process_received_message

  Default value is _megaco_.

- **`inet_backend`** - Choose the inet-backend.

  This option make it possible to use a different inet-backend ('default',
  'inet' or 'socket').

  Default is `default` (system default).
""".

-spec connect(TransportRef, Opts) ->
          {ok, Handle, ControlPid} | {error, Reason} when
      TransportRef :: pid() | RegName,
      RegName      :: atom(),
      Opts         :: [Option],
      Option       :: {inet_backend, default | inet | socket} |
                      {host,           Host} |
                      {port,           PortNum} |
                      {options,        list()} |
                      {receive_handle, term()} |
                      {module,         atom()},
      Host         :: inet:socket_address() | inet:hostname(),
      PortNum      :: inet:port_number(),
      Handle       :: handle(),
      ControlPid   :: pid(),
      Reason       :: term().

connect(TransportRef, Opts) ->
    ?d1("connect -> entry with"
	"~n   TransportRef: ~p"
	"~n   Opts:         ~p", [TransportRef, Opts]),
    Mand = [host, port, receive_handle],
    case parse_options(Opts, #megaco_tcp{}, Mand) of
	{ok, Rec} ->

	    ?d1("connect -> options parsed: "
		"~n   Rec: ~p", [Rec]),

	    #megaco_tcp{host         = Host,
			port         = Port,
			options      = Options,
                        inet_backend = IB} = Rec,

	    %% When using 'socket on Windows':
	    %% Unless 'Options' contain the 'ip' option,
	    %% we *will* use our own value (selected from net:getifaddr/1).
	    %% If 'host' is a string, we need to check 'Options'
	    %% to see if 'local' is present (which does not, currently,
	    %% work on Windows)?
	    %% If not (local), we *assume* domain = 'inet'.

	    IpOpts =
                case IB of
                    default ->
                        [];
                    _ ->
                        [{inet_backend, IB}]
                end ++ [binary, {packet, tpkt}, {active, once} |
			post_process_opts(Host, IB, Options)], 

            %%------------------------------------------------------
            %% Connect the other side
	    ?d1("connect -> connect with: "
		"~n   Host:   ~p"
		"~n   Port:   ~p"
		"~n   IpOpts: ~p", [Host, Port, IpOpts]),
	    case (catch gen_tcp:connect(Host, Port, IpOpts)) of
		{ok, Socket} ->
		    ?d1("connect -> connected: "
			"~n   Socket: ~p", [Socket]),
                    %%----------------------------------------------
                    %% Socket up start a new control process
		    Rec2 = Rec#megaco_tcp{socket = Socket}, 
		    case start_connection(TransportRef, Rec2) of
			{ok, Pid} ->
			    ?d1("connect -> connection started: "
				"~n   Pid: ~p", [Pid]),
			    _ = gen_tcp:controlling_process(Socket, Pid),
			    ?d2("connect -> control transferred"),
			    {ok, Socket, Pid};
			{error, Reason} ->
			    ?d1("connect -> failed starting connection: "
				"~n   Reason: ~p", [Reason]),
			    {error, Reason}
		    end;

		{error, Reason} ->
		    ?d1("connect -> failed connecting: "
			"~n   Reason: ~p", [Reason]),
		    Error = {error, {gen_tcp_connect, Reason, {Host, Port, IpOpts}}},
		    ?tcp_debug(Rec, "tcp connect failed", [Error]),
		    Error;

		{'EXIT', _Reason} = Exit ->
		    ?d1("connect -> connect exited: "
			"~n   Exit: ~p", [Exit]),
		    Error = {error, {gen_tcp_connect, Exit, {Host, Port, IpOpts}}},
		    ?tcp_debug(Rec, "tcp connect failed", [Error]),
		    Error

	    end;

	{error, _Reason} = Error ->
	    ?d1("connect -> failed parsing options: "
		"~n   Error: ~p", [Error]),
	    ?tcp_debug(#megaco_tcp{}, "tcp connect failed",
		       [Error, {options, Opts}]),
	    Error
    end.


%% In some cases we must bind and therefor we must have the
%% ip (or ifaddr) option.
post_process_opts(Host, socket = _IB, Opts) ->
    case os:type() of
	{win32, nt} ->
	    %% We must bind, and therefor we must provide a "proper" address.
	    %% Therefor...we need to figure out our domain.
	    post_process_opts(Host, Opts);
	_ ->
	    Opts
    end;
post_process_opts(_Host, _IB, Opts) ->
    Opts.


%% Socket on Windows: We need the ip (or ifaddr) option
post_process_opts(Host, Opts) ->
    case lists:keymember(ip, 1, Opts) orelse
	lists:keymember(ifaddr, 1, Opts) of
	true ->
	    %% No need to do anything, user has provided an address
	    Opts;
	false ->
	    %% We need to figure out a proper address and provide 
	    %% the ip option our selves.
	    post_process_opts2(Host, Opts)
    end.
	
%% We do not have the ip (or ifaddr) option
post_process_opts2(Host, Opts)
  when is_tuple(Host) andalso (tuple_size(Host) =:= 4) ->
    post_process_opts3(inet, Opts);
post_process_opts2(Host, Opts)
  when is_tuple(Host) andalso (tuple_size(Host) =:= 8) ->
    post_process_opts3(inet6, Opts);
%% This works even if Host is 'undefined'
post_process_opts2(Host, Opts) when is_atom(Host) ->
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
    end;
post_process_opts2(Host, Opts) when is_list(Host) ->
    %% Either hostname (inet or inet6) or a path (local)
    case lists:member(inet, Opts) of
	true ->
	    post_process_opts3(inet, Opts);
	false ->
	    case lists:member(inet6, Opts) of
		true ->
		    post_process_opts3(inet6, Opts);
		false ->
		    case lists:member(local, Opts) of
			true ->
			    %% Not supported on windows,
			    %% so we leave it as is and... 
			    Opts;
			false ->
			    post_process_opts3(inet, Opts)
		    end
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
%% Func: send_message
%% Description: Function is used for sending data on the TCP socket
%%-----------------------------------------------------------------
-doc """
Sends a message on a TPKT connection.
""".

-spec send_message(Handle, Msg) -> ok when
      Handle :: handle(),
      Msg    :: binary() | iolist().

send_message(Socket, Msg) ->
    ?d1("send_message -> entry with"
	"~n   Socket:    ~p"
	"~n   size(Msg): ~p", [Socket, sz(Msg)]),
    {Size, NewMsg} = add_tpkt_header(Msg),
    Res = gen_tcp:send(Socket, NewMsg),
    _ = case Res of
            ok ->
                incNumOutMessages(Socket),
                incNumOutOctets(Socket, Size);
            _ ->
                ok
        end,
    Res.
	    
-ifdef(megaco_debug).
sz(Bin) when is_binary(Bin) ->
    byte_size(Bin);
sz(List) when is_list(List) ->
    length(List).
-endif.


%%-----------------------------------------------------------------
%% Func: block
%% Description: Function is used for blocking incomming messages
%%              on the TCP socket
%%-----------------------------------------------------------------
-doc """
Stop receiving incoming messages on the socket.
""".

-spec block(Handle) -> ok when
      Handle :: handle().

block(Socket) ->
    ?tcp_debug({socket, Socket}, "tcp block", []),
    inet:setopts(Socket, [{active, false}]).


%%-----------------------------------------------------------------
%% Func: unblock
%% Description: Function is used for blocking incomming messages
%%              on the TCP socket
%%-----------------------------------------------------------------
-doc """
Starting to receive incoming messages from the socket again.

""".

-spec unblock(Handle) -> ok when
      Handle :: handle().

unblock(Socket) ->
    ?tcp_debug({socket, Socket}, "tcp unblock", []),
    inet:setopts(Socket, [{active, once}]).


%%-----------------------------------------------------------------
%% Func: close
%% Description: Function is used for closing the TCP socket
%%-----------------------------------------------------------------
-doc """
This function is used for closing an active TPKT connection.
""".

-spec close(Handle) -> ok when
      Handle :: handle().

close(Socket) ->
    ?tcp_debug({socket, Socket}, "tcp close", []),
    gen_tcp:close(Socket).


%%-----------------------------------------------------------------
%% Func: socket
%% Description: Returns the inet socket
%%-----------------------------------------------------------------
-doc """
This function is used to convert a socket `handle()` to a inet `socket()`.
""".

-spec socket(Handle) -> Socket when
      Handle :: handle(),
      Socket :: inet:socket().

socket(Socket) ->
    Socket.

-doc """
Upgrade the receive handle of the control process (e.g. after having changed
protocol version).
""".
-spec upgrade_receive_handle(ControlPid, NewRecvHandle) -> ok when
      ControlPid    :: pid(),
      NewRecvHandle :: term().

upgrade_receive_handle(ControlPid, NewRecvHandle) 
  when is_pid(ControlPid) andalso
       is_record(NewRecvHandle, megaco_receive_handle) ->
    megaco_tcp_connection:upgrade_receive_handle(ControlPid, NewRecvHandle).


%%-----------------------------------------------------------------
%% Internal Interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start_link/1
%% Description: Starts the net server
%%-----------------------------------------------------------------
-doc false.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


%%-----------------------------------------------------------------
%% Func: start_connection
%% Description: Function is used for starting up a connection
%%              process
%%-----------------------------------------------------------------
-doc false.
start_connection(SupPid, #megaco_tcp{socket = Socket} = TcpRec) ->
    ?d1("start_connection -> entry with"
	"~n   SupPid: ~p" 
	"~n   Socket: ~p", [SupPid, Socket]),
    
    case connection_sup(SupPid) of
	{ok, ConnSupPid} ->
	    ?d1("start_connection -> found connection supervisor: "
		"~n   ConnSupPid: ~p", [ConnSupPid]),
	    ?tcp_debug(TcpRec, "tcp connect", []),
	    case create_connection(ConnSupPid, TcpRec) of
		{ok, Pid} ->
		    ?d1("start_connection -> started: "
			"~n   Pid: ~p", [Pid]),
		    ?tcp_debug(TcpRec, "connect handler started", [Pid]),
		    create_snmp_counters(Socket),
		    {ok, Pid};
		{error, Reason} ->
		    ?d1("start_connection -> failed starting: "
			"~n   Reason: ~p", [Reason]),
		    Error = {error, {controlling_process_not_started, Reason}},
		    ?tcp_debug(TcpRec, "tcp connect failed", [Error]),
		    Error
	    end;
	{error, _Reason} ->
	    ?d2("start_connection -> could not find connection supervisor"),
	    Error = {error, no_connection_supervisor},
	    ?tcp_debug(TcpRec, "tcp connect failed", [Error]),
	    Error
    end.

connection_sup(Pid) ->
    megaco_tcp_sup:which_connection_sup(Pid).

create_connection(Pid, Rec) ->
    megaco_tcp_connection_sup:start_child(Pid, Rec).

create_snmp_counters(Socket) ->
    Counters = [medGwyGatewayNumInMessages, 
                medGwyGatewayNumInOctets, 
                medGwyGatewayNumOutMessages, 
                medGwyGatewayNumOutOctets, 
                medGwyGatewayNumErrors],
    create_snmp_counters(Socket, Counters).

create_snmp_counters(_Socket, []) ->
    ok;
create_snmp_counters(Socket, [Counter|Counters]) ->
    Key = {Socket, Counter},
    ets:insert(megaco_tcp_stats, {Key, 0}),
    create_snmp_counters(Socket, Counters).


%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%% Description: Init funcion for the supervisor
%%-----------------------------------------------------------------
-doc false.
init({SupPid, _}) ->
    process_flag(trap_exit, true),
    {ok, #state{supervisor_pid = SupPid, linkdb = []}}.

%%-----------------------------------------------------------------
%% Func: terminate/1
%% Description: Termination function for the generic server
%%-----------------------------------------------------------------
-doc false.
terminate(_Reason, _State) ->
    ok.


%%-----------------------------------------------------------------
%% Internal Functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: start_tcp_listener/2
%% Description: Function which parses the list of transport layers
%%              to start 
%%-----------------------------------------------------------------
start_tcp_listener(P, State) ->
    ?d1("start_tcp_listener -> entry with"
	"~n   P: ~p", [P]),
    case setup(State#state.supervisor_pid, P) of
	{ok, Pid, Data} ->
	    ?d1("start_tcp_listener -> setup ok"
		"~n   Pid:  ~p"
		"~n   Data: ~p", [Pid, Data]),
	    link(Pid),
	    {reply, ok, 
	     State#state{linkdb=[{Pid, Data} | State#state.linkdb]}};
	{error, Reason} ->
	    ?d1("start_tcp_listener -> setup failed"
		"~n   Reason: ~p", [Reason]),
            DB       = State#state.linkdb,
            DBStatus = [{LPid, {LRec, LSock}, inet:info(LSock)} || 
                           {LPid, {LRec, LSock}} <- DB],
            Reply = {error, {could_not_start_listener, Reason, DBStatus}},
	    {reply, Reply, State}
    end.


%%-----------------------------------------------------------------
%% Func: handle_call/3
%% Description: Handling call messages (really just garbage)
%%-----------------------------------------------------------------
-doc false.
handle_call({add_listener, Parameters}, _From, State) ->
    ?d1("handle_call(add_listener) -> entry with"
	"~n   Parameters: ~p", [Parameters]),
    start_tcp_listener(Parameters, State);
handle_call(Req, From, State) ->
    warning_msg("received unexpected request from ~p: "
		"~n~w", [From, Req]),
    {noreply, State}.


%%------------------------------------------------------------
%% Func: handle_cast/2
%% Description: Handling cast messages (really just garbage)
%%------------------------------------------------------------
-doc false.
handle_cast(Msg, State) ->
    warning_msg("received unexpected message: "
		"~n~w", [Msg]),
    {noreply,  State}.


%%-----------------------------------------------------------------
%% Func: handle_info/2
%% Description: Handling non call/cast messages, eg exit messages
%%-----------------------------------------------------------------
-doc false.
handle_info({'EXIT', Pid, Reason}, State) when is_pid(Pid) ->
    %% Accept process died
    NewState = resetup(Pid, Reason, State),
    {noreply, NewState};
handle_info(Info, State) ->
    warning_msg("received unexpected info: "
		"~n~w", [Info]),
    {noreply,  State}.


%%-----------------------------------------------------------------
%% Func: code_change/3
%% Descrition: Handles code change messages during upgrade.
%%-----------------------------------------------------------------
-doc false.
code_change(_Vsn, State, _Extra) ->
    {ok, State}.


%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: setup/2
%% Description: Function is used when setting up an TCP listen 
%%              socket in the MGC
%%-----------------------------------------------------------------
setup(SupPid, Options) ->
    ?d1("setup -> entry with"
	"~n   SupPid:  ~p"
	"~n   Options: ~p", [SupPid, Options]),
    Mand = [port, receive_handle],
    case parse_options(Options, #megaco_tcp{}, Mand) of
	{ok, #megaco_tcp{port         = Port,
			 options      = Opts,
			 inet_backend = IB} = TcpRec} ->
    
	    ?d1("setup -> options parsed"
		"~n   TcpRec: ~p", [TcpRec]),

            %%------------------------------------------------------
            %% Setup the listen socket
	    IpOpts =
                case IB of
                    default ->
                        [];
                    _ ->
                        [{inet_backend, IB}]
                end ++
                [binary, {packet, tpkt}, {active, once}, {reuseaddr, true} |
		 post_process_opts(undefined, IB, Opts)],
	    ?d1("setup -> listen with: "
		"~n   Port:   ~p"
		"~n   IpOpts: ~p", [Port, IpOpts]),
	    case catch gen_tcp:listen(Port, IpOpts) of
		{ok, LSock} ->

		    ?d1("setup -> listen ok"
			"~n   Listen: ~p", [LSock]),

	            %%-----------------------------------------------
	            %% Startup the accept process that will wait for 
	            %% connect attempts
		    case start_accept(SupPid, TcpRec, LSock) of
			{ok, Pid} ->
			    ?d1("setup -> accept process started"
				"~n   Pid: ~p", [Pid]),
			    ?tcp_debug(TcpRec, "tcp listen setup", []),
			    {ok, Pid, {TcpRec, LSock}};

			{error, _Reason} = Error ->
			    ?d1("setup -> failed starting accept process"
				"~n   Error: ~p", [Error]),
			    ?tcp_debug(TcpRec, "tcp listen setup failed", 
				       [Error]),
			    Error
		    end;

		{error, Reason} ->
		    ?d1("setup -> listen failed"
			"~n   Reason: ~p", [Reason]),
		    Error = {error, {gen_tcp_listen, Reason, [Port, IpOpts]}},
		    ?tcp_debug(TcpRec, "tcp listen setup failed", [Error]),
		    Error;
		{'EXIT', _Reason} = Exit ->
		    ?d1("setup -> listen exited"
			"~n   Exit: ~p", [Exit]),
		    Error = {error, {gen_tcp_listen, Exit, [Port, IpOpts]}},
		    ?tcp_debug(TcpRec, "tcp listen setup failed", [Error]),
		    Error
	    end;
	{error, _Reason} = Error ->
	    ?d1("setup -> failed parsing options"
		"~n   Error: ~p", [Error]),
	    ?tcp_debug(#megaco_tcp{}, "tcp listen setup failed",
		       [Error, {options, Options}]),
	    Error
    end.
    

%%-----------------------------------------------------------------
%% Func: resetup
%% Description: Function is used when restarting the accept process
%%              if it died for some reason.
%%-----------------------------------------------------------------

resetup(Pid, Reason, State) ->
    ?d1("resetup -> entry with"
	"~n   Pid:    ~p"
	"~n   Reason: ~p", [Pid, Reason]),
    case lists:keysearch(Pid, 1, State#state.linkdb) of
	{value, {Pid, {TcpRec, Listener}}} ->
	    ?d1("resetup -> found accept process: "
		"~n   TcpRec:   ~p"
		"~n   Listener: ~p", [TcpRec, Listener]),
	    ?tcp_debug(TcpRec, "tcp listen resetup", [{error, Reason}]),
	    unlink(Pid),
	    warning_msg("received unexpected 'EXIT' signal "
			"from accept process ~p: "
			"~n~w", [Pid, Reason]),
	    case start_accept(State#state.supervisor_pid, TcpRec, Listener) of
		{ok, NewPid} ->
		    ?d1("resetup -> start new accept process ok: "
			"~n   NewPid: ~p", [NewPid]),
		    link(NewPid),
		    NewList = lists:keyreplace(Pid, 1, State#state.linkdb,
					       {NewPid, {TcpRec, Listener}}),
		    State#state{linkdb = NewList};
		{error, Reason} ->
		    ?d1("resetup -> failed starting new accept process: "
			"~n   :Reason ~p", [Reason]),
		    ?tcp_debug(TcpRec, 
			       "tcp listen resetup failed", [{error, Reason}]),
		    State
	    end;
	false ->
	    warning_msg("received unexpected 'EXIT' signal from ~p: "
			"~n~w", [Pid, Reason]),
	    State
    end.


%%-----------------------------------------------------------------
%% Func: start_accept
%% Description: Function is used for starting up an TCP accept
%%              process
%%-----------------------------------------------------------------
start_accept(SupPid, TcpRec, Listen) ->
    ?d1("start_accept -> entry with"
	"~n   SupPid: ~p"
	"~n   TcpRec: ~p"
	"~n   Reason: ~p", [SupPid, TcpRec, Listen]),
    case accept_sup(SupPid) of
	{ok, AcceptSupPid} ->
	    ?d1("start_accept -> found accept supervisor"
		"~n   AcceptSupPid: ~p", [AcceptSupPid]),
	    case create_acceptor(AcceptSupPid, TcpRec, SupPid, Listen) of
		{ok, Pid} ->
		    ?d1("start_accept -> accept process started"
			"~n   Pid: ~p", [Pid]),
		    {ok, Pid};
		{error, Reason} ->
		    ?d1("start_accept -> failed starting accept process: "
			"~n   Reason: ~p", [Reason]),
		    {error, {accept_not_started, Reason}}
	    end;
	{error, Reason} ->
	    ?d1("start_accept -> could not find acceept supervisor: "
		"~n   Reason: ~p", [Reason]),
	    {error, {no_tcp_accept_sup, Reason}}
    end.

accept_sup(Pid) ->
    megaco_tcp_sup:which_accept_sup(Pid).

create_acceptor(Pid, Rec, TopSup, Listen) ->
    megaco_tcp_accept_sup:start_child(Pid, Rec, TopSup, Listen).


%%-----------------------------------------------------------------
%% Func: add_tpkt_header
%% Description: Function is used to add the TPKT header
%%-----------------------------------------------------------------
add_tpkt_header(Data) when is_binary(Data) ->
    L = byte_size(Data) + 4,
    {L, [3, 0, ((L) bsr 8) band 16#ff, (L) band 16#ff ,Data]};
add_tpkt_header(IOList) when is_list(IOList) ->
    Binary = list_to_binary(IOList),
    L = byte_size(Binary) + 4,
    {L, [3, 0, ((L) bsr 8) band 16#ff, (L) band 16#ff , Binary]}.

%%-----------------------------------------------------------------
%% Func: parse_options
%% Description: Function that parses the options sent to the TCP 
%%              module.
%%-----------------------------------------------------------------
parse_options([{Tag, Val} | T], TcpRec, Mand) ->
    ?d1("parse_options -> entry with"
	"~n   Tag: ~p"
	"~n   Val: ~p", [Tag, Val]),
    Mand2 = Mand -- [Tag],
    case Tag of
	port ->
	    parse_options(T, TcpRec#megaco_tcp{port = Val}, Mand2);
	host ->
	    parse_options(T, TcpRec#megaco_tcp{host = Val}, Mand2);
	tcp_options when is_list(Val) ->
	    parse_options(T, TcpRec#megaco_tcp{options = Val}, Mand2);
	receive_handle ->
	    parse_options(T, TcpRec#megaco_tcp{receive_handle = Val}, Mand2);
	module when is_atom(Val) ->
	    parse_options(T, TcpRec#megaco_tcp{module = Val}, Mand2);
	serialize when (Val =:= true) orelse (Val =:= false) ->
	    parse_options(T, TcpRec#megaco_tcp{serialize = Val}, Mand2);
	inet_backend when (Val =:= default) orelse
                          (Val =:= inet) orelse
                          (Val =:= socket)  ->
	    parse_options(T, TcpRec#megaco_tcp{inet_backend = Val}, Mand2);
        Bad ->
	    ?d1("parse_options -> bad option: "
		"~n   Tag: ~p", [Tag]),
	    {error, {bad_option, Bad}}
    end;
parse_options([], TcpRec, []) ->
    ?d2("parse_options -> done"),
    {ok, TcpRec};
parse_options([], _TcpRec, Mand) ->
    ?d1("parse_options -> entry with"
	"~n   Mand: ~p", [Mand]),
    {error, {missing_options, Mand}};
parse_options(BadList, _TcpRec, _Mand) ->
    ?d1("parse_options -> entry with"
	"~n   BadList: ~p", [BadList]),
    {error, {bad_option_list, BadList}}.


%%-----------------------------------------------------------------
%% Func: incNumOutMessages/1, incNumOutOctets/2, incNumErrors/1
%% Description: SNMP counter increment functions
%%              
%%-----------------------------------------------------------------
incNumOutMessages(Socket) ->
    incCounter({Socket, medGwyGatewayNumOutMessages}, 1).

incNumOutOctets(Socket, NumOctets) ->
    incCounter({Socket, medGwyGatewayNumOutOctets}, NumOctets).

incCounter(Key, Inc) ->
    ets:update_counter(megaco_tcp_stats, Key, Inc).

% incNumErrors(Socket) ->
%     ets:update_counter(megaco_tcp_stats, 
% 		       {Socket, medGwyGatewayNumErrors}, 1).


%%-----------------------------------------------------------------


warning_msg(F, A) ->
    ?megaco_warning("TCP server: " ++ F, A).
  
%% error_msg(F, A) ->
%%     ?megaco_error("TCP server: " ++ F, A).


call(Pid, Req) ->
    gen_server:call(Pid, Req, infinity).


%%-----------------------------------------------------------------

%% formated_timestamp() ->
%%     format_timestamp(os:timestamp()).

%% format_timestamp(TS) ->
%%     megaco:format_timestamp(TS).

%% d(F) ->
%%     d(F, []).

%% d(F, A) ->
%%     io:format("*** [~s] ~p ~w " ++ F ++ "~n",
%%               [formated_timestamp(), self(), ?MODULE | A]).


%%-----------------------------------------------------------------

%% tcp_sockets() ->
%%     port_list("tcp_inet") ++ gen_tcp_socket:which_sockets().
                      

%% %% Return all ports having the name 'Name'
%% port_list(Name) ->
%%     lists:filter(
%%       fun(Port) ->
%%               case erlang:port_info(Port, name) of
%%                   {name, Name} -> true;
%%                   _ -> false
%%               end
%%       end, erlang:ports()).

