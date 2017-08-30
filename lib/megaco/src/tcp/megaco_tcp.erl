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
%% Purpose: 
%%      Interface the TPKT (TCP/IP) transport module for Megaco/H.248
%%
%%-----------------------------------------------------------------
-module(megaco_tcp).

-behaviour(gen_server).


%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
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
get_stats() ->
    megaco_stats:get_stats(megaco_tcp_stats).

get_stats(Socket) ->
    megaco_stats:get_stats(megaco_tcp_stats, Socket).

get_stats(Socket, Counter) ->
    megaco_stats:get_stats(megaco_tcp_stats, Socket, Counter).


%%-----------------------------------------------------------------
%% Func: reset_stats/0, reaet_stats/1
%% Description: Reset statistics (counters) for TCP
%%-----------------------------------------------------------------
reset_stats() ->
    megaco_stats:reset_stats(megaco_tcp_stats).

reset_stats(Socket) ->
    megaco_stats:reset_stats(megaco_tcp_stats, Socket).


%%-----------------------------------------------------------------
%% Func: start_transport/0
%% Description: Starts the TPKT transport service
%%-----------------------------------------------------------------
start_transport() ->
    ?d2("start_transport -> entry"),
    (catch megaco_stats:init(megaco_tcp_stats)),
    megaco_tcp_sup:start_link().


%%-----------------------------------------------------------------
%% Func: stop_transport/1, 2
%% Description: Stop the TPKT transport service
%%-----------------------------------------------------------------
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
connect(SupPid, Parameters) ->
    ?d1("connect -> entry with"
	"~n   SupPid:     ~p"
	"~n   Parameters: ~p", [SupPid, Parameters]),
    Mand = [host, port, receive_handle],
    case parse_options(Parameters, #megaco_tcp{}, Mand) of
	{ok, Rec} ->

	    ?d1("connect -> options parsed: "
		"~n   Rec: ~p", [Rec]),

	    #megaco_tcp{host    = Host,
			port    = Port,
			options = Options} = Rec,
	    
	    IpOpt = [binary, {packet, tpkt}, {active, once} | Options], 

            %%------------------------------------------------------
            %% Connect the other side
	    case (catch gen_tcp:connect(Host, Port, IpOpt)) of
		{ok, Socket} ->
		    ?d1("connect -> connected: "
			"~n   Socket: ~p", [Socket]),
                    %%----------------------------------------------
                    %% Socket up start a new control process
		    Rec2 = Rec#megaco_tcp{socket = Socket}, 
		    case start_connection(SupPid, Rec2) of
			{ok, Pid} ->
			    ?d1("connect -> connection started: "
				"~n   Pid: ~p", [Pid]),
			    gen_tcp:controlling_process(Socket, Pid),
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
		    Error = {error, {gen_tcp_connect, Reason}},
		    ?tcp_debug(Rec, "tcp connect failed", [Error]),
		    Error;

		{'EXIT', _Reason} = Exit ->
		    ?d1("connect -> connect exited: "
			"~n   Exit: ~p", [Exit]),
		    Error = {error, {gen_tcp_connect, Exit}},
		    ?tcp_debug(Rec, "tcp connect failed", [Error]),
		    Error

	    end;

	{error, _Reason} = Error ->
	    ?d1("connect -> failed parsing options: "
		"~n   Error: ~p", [Error]),
	    ?tcp_debug(#megaco_tcp{}, "tcp connect failed",
		       [Error, {options, Parameters}]),
	    Error
    end.


%%-----------------------------------------------------------------
%% Func: send_message
%% Description: Function is used for sending data on the TCP socket
%%-----------------------------------------------------------------
send_message(Socket, Data) ->
    ?d1("send_message -> entry with"
	"~n   Socket:     ~p"
	"~n   size(Data): ~p", [Socket, sz(Data)]),
    {Size, NewData} = add_tpkt_header(Data),
    Res = gen_tcp:send(Socket, NewData),
    case Res of
	ok ->
	    incNumOutMessages(Socket),
	    incNumOutOctets(Socket, Size);
	_ ->
	    ok
    end,
    Res.
	    
-ifdef(megaco_debug).
sz(Bin) when is_binary(Bin) ->
    size(Bin);
sz(List) when is_list(List) ->
    length(List).
-endif.


%%-----------------------------------------------------------------
%% Func: block
%% Description: Function is used for blocking incomming messages
%%              on the TCP socket
%%-----------------------------------------------------------------
block(Socket) ->
    ?tcp_debug({socket, Socket}, "tcp block", []),
    inet:setopts(Socket, [{active, false}]).


%%-----------------------------------------------------------------
%% Func: unblock
%% Description: Function is used for blocking incomming messages
%%              on the TCP socket
%%-----------------------------------------------------------------
unblock(Socket) ->
    ?tcp_debug({socket, Socket}, "tcp unblock", []),
    inet:setopts(Socket, [{active, once}]).


%%-----------------------------------------------------------------
%% Func: close
%% Description: Function is used for closing the TCP socket
%%-----------------------------------------------------------------
close(Socket) ->
    ?tcp_debug({socket, Socket}, "tcp close", []),
    gen_tcp:close(Socket).


%%-----------------------------------------------------------------
%% Func: socket
%% Description: Returns the inet socket
%%-----------------------------------------------------------------
socket(Socket) ->
    Socket.

upgrade_receive_handle(Pid, NewHandle) 
  when is_pid(Pid) andalso is_record(NewHandle, megaco_receive_handle) ->
    megaco_tcp_connection:upgrade_receive_handle(Pid, NewHandle).


%%-----------------------------------------------------------------
%% Internal Interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start_link/1
%% Description: Starts the net server
%%-----------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


%%-----------------------------------------------------------------
%% Func: start_connection
%% Description: Function is used for starting up a connection
%%              process
%%-----------------------------------------------------------------
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
init({SupPid, _}) ->
    process_flag(trap_exit, true),
    {ok, #state{supervisor_pid = SupPid}}.

%%-----------------------------------------------------------------
%% Func: terminate/1
%% Description: Termination function for the generic server
%%-----------------------------------------------------------------
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
	    {reply, {error, {could_not_start_listener, Reason}}, State}
    end.


%%-----------------------------------------------------------------
%% Func: handle_call/3
%% Description: Handling call messages (really just garbage)
%%-----------------------------------------------------------------
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
handle_cast(Msg, State) ->
    warning_msg("received unexpected message: "
		"~n~w", [Msg]),
    {noreply,  State}.


%%-----------------------------------------------------------------
%% Func: handle_info/2
%% Description: Handling non call/cast messages, eg exit messages
%%-----------------------------------------------------------------
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
	{ok, TcpRec} ->
    
	    ?d1("setup -> options parsed"
		"~n   TcpRec: ~p", [TcpRec]),

            %%------------------------------------------------------
            %% Setup the listen socket
	    IpOpts = [binary, {packet, tpkt}, {active, once},
		      {reuseaddr, true} | TcpRec#megaco_tcp.options],
	    case catch gen_tcp:listen(TcpRec#megaco_tcp.port, IpOpts) of
		{ok, Listen} ->

		    ?d1("setup -> listen ok"
			"~n   Listen: ~p", [Listen]),

	            %%-----------------------------------------------
	            %% Startup the accept process that will wait for 
	            %% connect attempts
		    case start_accept(SupPid, TcpRec, Listen) of
			{ok, Pid} ->

			    ?d1("setup -> accept process started"
				"~n   Pid: ~p", [Pid]),

			    ?tcp_debug(TcpRec, "tcp listen setup", []),
			    {ok, Pid, {TcpRec, Listen}};
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
		    Error = {error, {gen_tcp_listen, Reason}},
		    ?tcp_debug(TcpRec, "tcp listen setup failed", [Error]),
		    Error;
		{'EXIT', _Reason} = Exit ->
		    ?d1("setup -> listen exited"
			"~n   Exit: ~p", [Exit]),
		    Error = {error, {gen_tcp_listen, Exit}},
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
    L = size(Data) + 4,
    {L, [3, 0, ((L) bsr 8) band 16#ff, (L) band 16#ff ,Data]};
add_tpkt_header(IOList) when is_list(IOList) ->
    Binary = list_to_binary(IOList),
    L = size(Binary) + 4,
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
