%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2019. All Rights Reserved.
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
-module(snmpa_net_if).

-behaviour(snmpa_network_interface).

-export([start_link/4,
	 info/1, 
	 verbosity/2]).
-export([get_log_type/1,  set_log_type/2]).
-export([get_request_limit/1, set_request_limit/2]).
-export([system_continue/3, system_terminate/4, system_code_change/4]).
-export([init/5]).
-export([filter_reset/1]).

-include("snmp_types.hrl").
-include("snmpa_internal.hrl").
-include("snmpa_atl.hrl").
-include("snmp_debug.hrl").
-include("snmp_verbosity.hrl").

-record(state,
	{parent,
	 note_store,
	 master_agent,
	 transports = [],
%%	 usock,
%%	 usock_opts,
	 mpd_state,
	 log,
	 reqs  = [],
	 debug = false,
	 limit = infinity,
%%	 rcnt  = [],
	 filter}).
%%	 domain = snmpUDPDomain}).

-record(transport,
	{socket,
	 domain = snmpUDPDomain,
	 opts = [],
	 req_refs = []}).

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.

-define(DEFAULT_FILTER_MODULE, snmpa_net_if_filter).
-define(DEFAULT_FILTER_OPTS,   [{module, ?DEFAULT_FILTER_MODULE}]).

-define(ATL_SEQNO_INITIAL, 1).
-define(ATL_SEQNO_MAX,     2147483647).


%%%-----------------------------------------------------------------
%%% This module implements the default Network Interface part
%%% of the SNMP agent. It uses UDP, and read the agent.conf to find
%%% the UDP port.
%%%
%%% Opts = [Opt]
%%% Opt  = {verbosity,silence | log | debug} | 
%%%        {bind_to, bool()} |
%%%        {recbuf,integer()} |
%%%        {no_reuse, bool()} |
%%%        {req_limit, integer() | infinity}
%%%-----------------------------------------------------------------
start_link(Prio, NoteStore, MasterAgent, Opts) ->
    ?d("start_link -> entry with"
	"~n   Prio:        ~p"
	"~n   NoteStore:   ~p"
	"~n   MasterAgent: ~p"
	"~n   Opts:        ~p", [Prio, NoteStore, MasterAgent, Opts]),
    Args = [Prio, NoteStore, MasterAgent, self(), Opts],
    proc_lib:start_link(?MODULE, init, Args).

    
info(Pid) ->
    case call(Pid, info) of
	Info when is_list(Info) ->
	    Info;
	_ ->
	    []
    end.

verbosity(Pid, Verbosity) -> 
    Pid ! {verbosity, Verbosity}.

get_log_type(Pid) ->
    call(Pid, get_log_type).

set_log_type(Pid, NewType) ->
    call(Pid, {set_log_type, NewType}).

get_request_limit(Pid) ->
    call(Pid, get_request_limit).

set_request_limit(Pid, NewLimit) ->
    call(Pid, {set_request_limit, NewLimit}).

get_transports() ->
    {value, Transports} = snmp_framework_mib:intAgentTransports(get),
    Transports.

filter_reset(Pid) ->
    Pid ! filter_reset.


%%-----------------------------------------------------------------
%%-----------------------------------------------------------------

init(Prio, NoteStore, MasterAgent, Parent, Opts) ->
    ?d("init -> entry with"
      "~n   Prio:        ~p"
      "~n   NoteStore:   ~p"
      "~n   MasterAgent: ~p"
      "~n   Parent:      ~p"
      "~n   Opts:        ~p", [Prio, NoteStore, MasterAgent, Parent, Opts]),
    case (catch do_init(Prio, NoteStore, MasterAgent, Parent, Opts)) of
	{ok, State} ->
	    proc_lib:init_ack({ok, self()}),
	    try loop(State)
	    catch
		C:E:S when C =/= exit, E =/= shutdown ->
		    Fmt =
			"loop/1 EXCEPTION ~w:~w~n"
			"   ~p",
		    case C of
			exit ->
			    %% Externally killed, root cause is elsewhere
			    info_msg(Fmt, [C, E, S]);
			_ ->
			    error_msg(Fmt, [C, E, S])
		    end,
		    erlang:raise(C, E, S)
	    end;
	{error, Reason} ->
	    config_err("failed starting net-if: ~n~p", [Reason]),
	    proc_lib:init_ack({error, Reason});
	Error ->
	    config_err("failed starting net-if: ~n~p", [Error]),
	    proc_lib:init_ack({error, Error})
    end.

do_init(Prio, NoteStore, MasterAgent, Parent, Opts) ->
    process_flag(trap_exit, true),
    process_flag(priority, Prio),

    %% -- Verbosity --
    put(sname,nif),
    put(verbosity,get_verbosity(Opts)),
    ?vlog("starting",[]),

    %% -- Versions --
    Vsns = get_vsns(Opts),
    ?vdebug("vsns: ~w",[Vsns]),

    %% Flow control --
    Limit      = get_req_limit(Opts),
    ?vdebug("Limit: ~w", [Limit]),
    FilterOpts = get_filter_opts(Opts),
    FilterMod  = create_filter(FilterOpts),
    ?vdebug("FilterMod: ~w FilterOpts: ~p", [FilterMod,FilterOpts]),

    %% -- Audit trail log
    Log = create_log(),
    ?vdebug("Log: ~w",[Log]),

    DomainAddresses = get_transports(),
    ?vdebug("DomainAddresses: ~w",[DomainAddresses]),
    try
	[begin
	     SocketOpts = socket_opts(Domain, Address, Opts),
	     Socket = socket_open(Domain, SocketOpts),
	     active_once(Socket),
	     #transport{
		      socket = Socket,
		      domain = Domain,
		      opts   = SocketOpts}
	 end || {Domain, Address} <- DomainAddresses]
    of
	[] ->
	    ?vinfo("No transports configured: ~p", [DomainAddresses]),
	    {error, {no_transports,DomainAddresses}};
	Transports ->
	    MpdState = snmpa_mpd:init(Vsns),
	    init_counters(),
	    S = #state{parent       = Parent,
		       note_store   = NoteStore,
		       master_agent = MasterAgent, 
		       mpd_state    = MpdState,
		       transports   = Transports,
		       log          = Log,
		       limit        = Limit,
		       filter       = FilterMod},
	    ?vdebug("started with MpdState: ~p", [MpdState]),
	    {ok, S}
    catch
	Error ->
	    ?vinfo("Failed to initialize socket(s): ~p", [Error]),
	    {error, Error}
    end.


create_log() ->
    case ets:lookup(snmp_agent_table, audit_trail_log) of
	[] ->
	    {undefined, []};
	[{audit_trail_log, AtlOpts}] ->
	    ?vtrace("AtlOpts: ~p", [AtlOpts]),
	    Type   = get_atl_type(AtlOpts),
	    Dir    = get_atl_dir(AtlOpts),
	    Size   = get_atl_size(AtlOpts),
	    Repair = get_atl_repair(AtlOpts),
	    Name   = ?audit_trail_log_name,
	    File   = filename:absname(?audit_trail_log_file, Dir),
	    case get_atl_seqno(AtlOpts) of
		true ->
		    Initial  = ?ATL_SEQNO_INITIAL,
		    Max      = ?ATL_SEQNO_MAX, 
		    Module   = snmpa_agent, 
		    Function = increment_counter, 
		    Args     = [atl_seqno, Initial, Max], 
		    SeqNoGen = {Module, Function, Args}, 
		    case snmp_log:create(Name, File, 
					 SeqNoGen, Size, Repair, true) of
			{ok, Log} ->
			    ?vdebug("log created: ~w", [Log]),
			    {Log, Type};
			{error, Reason} ->
			    throw({error, {create_log, Reason}})
		    end;
		_ ->
		    case snmp_log:create(Name, File, Size, Repair, true) of
			{ok, Log} ->
			    ?vdebug("log created: ~w", [Log]),
			    {Log, Type};
			{error, Reason} ->
			    throw({error, {create_log, Reason}})
		    end
	    end
    end.


create_filter(Opts) when is_list(Opts) ->
    case get_filter_module(Opts) of
	?DEFAULT_FILTER_MODULE = Mod ->
	    Mod;
	Module ->
	    snmpa_network_interface_filter:verify(Module),
	    Module
    end;
create_filter(BadOpts) ->
    throw({error, {bad_filter_opts, BadOpts}}).


log({_, []}, _, _, _) ->
    ok;
log({Log, Types}, 'set-request', Packet, Address) ->
    case lists:member(write, Types) of
	true ->
	    snmp_log:log(Log, Packet, format_address(Address));
	false ->
	    ok
    end;
log({Log, Types}, _, Packet, Address) ->
     case lists:member(read, Types) of
	true ->
	    snmp_log:log(Log, Packet, format_address(Address));
	false ->
	    ok
    end.

format_address(Address) ->
    iolist_to_binary(snmp_conf:mk_addr_string(Address)).


socket_open(snmpUDPDomain = Domain, [IpPort | Opts]) ->
    case init:get_argument(snmp_fd) of
	{ok, [[FdStr]]} ->
	    Fd = list_to_integer(FdStr),
	    ?vdebug("socket_open(~p, [~p | ~p]) Fd: ~p",
		    [Domain, IpPort, Opts, Fd]),
	    gen_udp_open(0, [{fd, Fd} | Opts]);
	error ->
	    case init:get_argument(snmpa_fd) of
		{ok, [[FdStr]]} ->
		    Fd = list_to_integer(FdStr),
		    ?vdebug("socket_open(~p, [~p | ~p]) Fd: ~p",
			    [Domain, IpPort, Opts, Fd]),
		    gen_udp_open(0, [{fd, Fd} | Opts]);
		error ->
		    ?vdebug("socket_open(~p, [~p | ~p])",
			    [Domain, IpPort, Opts]),
		    gen_udp_open(IpPort, Opts)
	    end
    end;
socket_open(Domain, [IpPort | Opts])
  when Domain =:= transportDomainUdpIpv4;
       Domain =:= transportDomainUdpIpv6 ->
    ?vdebug("socket_open(~p, [~p | ~p])", [Domain, IpPort, Opts]),
    gen_udp_open(IpPort, Opts);
socket_open(Domain, Opts) ->
    throw({socket_open, Domain, Opts}).

gen_udp_open(IpPort, Opts) ->
    case gen_udp:open(IpPort, Opts) of
	{ok, Socket} ->
	    Socket;
	{error, Reason} ->
	    throw({udp_open, IpPort, Reason})
    end.



loop(#state{transports = Transports, limit = Limit, parent = Parent} = S) ->
    ?vdebug("loop(~p)", [S]),
    receive
	{udp, Socket, IpAddr, IpPort, Packet} = Msg when is_port(Socket) ->
	    ?vlog("got paket from ~w:~w on ~w", [IpAddr, IpPort, Socket]),
	    case lists:keyfind(Socket, #transport.socket, Transports) of
		#transport{socket = Socket, domain = Domain} = Transport ->
		    From =
			case Domain of
			    snmpUDPDomain ->
				{IpAddr, IpPort};
			    _ ->
				{Domain, {IpAddr, IpPort}}
			end,
		    loop(maybe_handle_recv(S, Transport, From, Packet));
		false ->
		    error_msg("Packet on unknown port: ~p", [Msg]),
		    loop(S)
	    end;

	{info, ReplyRef, Pid} ->
	    Info = get_info(S),
	    Pid ! {ReplyRef, Info, self()},
	    loop(S);

	%% response (to get/get_next/get_bulk/set requests)
	{snmp_response, Vsn, RePdu, Type, ACMData, To, Extra} ->
	    ?vlog("reply pdu: "
		  "~n   ~s", 
		  [?vapply(snmp_misc, format, [256, "~w", [RePdu]])]),
	    {_, ReqRef} = lists:keyfind(request_ref, 1, Extra),
	    case
		case
		    (Limit =/= infinity) andalso
		    select_transport_from_req_ref(ReqRef, Transports)
		of
		    false ->
			select_transport_from_domain(
			  address_to_domain(To),
			  Transports);
		    T ->
			T
		end
	    of
		false ->
		    error_msg(
		      "Can not find transport for response PDU to: ~s",
		      [format_address(To)]),
		    loop(S);
		Transport ->
		    NewS = update_req_counter_outgoing(S, Transport, ReqRef),
		    maybe_handle_reply_pdu(
		      NewS, Transport, Vsn, RePdu, Type, ACMData, To),
		    loop(NewS)
	    end;

	%% Traps/notification
	{send_pdu, Vsn, Pdu, MsgData, TDomAddrs} ->
	    ?vdebug("send pdu:~n"
		    "   Pdu:       ~p~n"
		    "   TDomAddrs: ~p", [Pdu, TDomAddrs]),
	    NewS =
		maybe_handle_send_pdu(
		  S, Vsn, Pdu, MsgData, TDomAddrs, undefined),
	    loop(NewS);

	%% We dont use the extra-info at this time, ...
	{send_pdu, Vsn, Pdu, MsgData, TDomAddrs, _ExtraInfo} ->
	    ?vdebug("send pdu:~n"
		    "   Pdu:        ~p~n"
		    "   TDomAddrs: ~p", [Pdu, TDomAddrs]),
	    NewS =
		maybe_handle_send_pdu(
		  S, Vsn, Pdu, MsgData, TDomAddrs, undefined),
	    loop(NewS);

	%% Informs
	{send_pdu_req, Vsn, Pdu, MsgData, TDomAddrs, From} ->
	    ?vdebug("send pdu request:~n"
		    "   Pdu:       ~p~n"
		    "   TDomAddrs: ~p~n"
		    "   From:      ~p",
		    [Pdu, TDomAddrs, toname(From)]),
	    NewS =
		maybe_handle_send_pdu(
		  S, Vsn, Pdu, MsgData, TDomAddrs, From),
	    loop(NewS);

	%% We dont use the extra-info at this time, ...
	{send_pdu_req, Vsn, Pdu, MsgData, TDomAddrs, From, _ExtraInfo} ->
	    ?vdebug("send pdu request:~n"
		    "   Pdu:       ~p~n"
		    "   TDomAddrs: ~p~n"
		    "   From:      ~p",
		    [Pdu, TDomAddrs, toname(From)]),
	    NewS =
		maybe_handle_send_pdu(
		  S, Vsn, Pdu, MsgData, TDomAddrs, From),
	    loop(NewS);

	%% Discovery Inform
	%% <BACKWARD-COMPAT>
	{send_discovery, Pdu, MsgData, To, From} ->
	    ?vdebug("received send discovery request: "
		    "~n   Pdu:  ~p"
		    "~n   To:   ~p"
		    "~n   From: ~p", 
		    [Pdu, To, toname(From)]),
	    NewS = handle_send_discovery(S, Pdu, MsgData, To, From),
	    loop(NewS);
	%% </BACKWARD-COMPAT>

	%% Discovery Inform
	{send_discovery, Pdu, MsgData, To, From, ExtraInfo} ->
	    ?vdebug("received send discovery request: "
		    "~n   Pdu:       ~p"
		    "~n   To:        ~p"
		    "~n   From:      ~p"
		    "~n   ExtraInfo: ~p", 
		    [Pdu, To, toname(From), ExtraInfo]),
	    NewS = handle_send_discovery(S, Pdu, MsgData, To, From),
	    loop(NewS);

	{discarded_pdu, _Vsn, ReqId, _ACMData, Variable, Extra} ->
	    ?vdebug("discard PDU: ~p - ~p - ~p",
		    [Variable, Extra, Transports]),
	    snmpa_mpd:discarded_pdu(Variable),
	    {_, ReqRef} = lists:keyfind(request_ref, 1, Extra),
	    if
		Limit =:= infinity ->
		    %% The incoming PDU was not registered
		    loop(update_req_counter_outgoing(S, false, ReqRef));
		true ->
		    case
			select_transport_from_req_ref(ReqRef, Transports)
		    of
			false ->
			    error_msg(
			      "Can not find transport for discarded PDU: ~p",
			      [ReqId]),
			    loop(S);
			Transport ->
			    loop(
			      update_req_counter_outgoing(
				S, Transport, ReqRef))
		    end
	    end;

	{get_log_type, ReplyRef, Pid} ->
	    ?vdebug("get log type: ~p", []),
	    {_, LogType} = S#state.log,
	    Pid ! {ReplyRef, {ok, LogType}, self()},
	    loop(S);

	{{set_log_type, NewType}, ReplyRef, Pid} ->
	    ?vdebug("set log type: ~p", [NewType]),
	    {NewState, Reply} = (catch handle_set_log_type(S, NewType)),
	    Pid ! {ReplyRef, Reply, self()},
	    loop(NewState);

	{get_request_limit, ReplyRef, Pid} ->
	    ?vdebug("get request limit: ~p", []),
	    Pid ! {ReplyRef, {ok, Limit}, self()},
	    loop(S);

	{{set_request_limit, NewLimit}, ReplyRef, Pid} ->
	    ?vdebug("set request limit: ~p", [NewLimit]),
	    {NewState, Reply} = (catch handle_set_request_limit(S, NewLimit)),
	    Pid ! {ReplyRef, Reply, self()},
	    loop(NewState);

	{disk_log, _Node, Log, Info} ->
	    ?vdebug("disk log event: ~p, ~p", [Log, Info]),
	    NewS = handle_disk_log(Log, Info, S),
	    loop(NewS);

	{verbosity, Verbosity} ->
	    ?vlog("verbosity: ~p -> ~p", [get(verbosity), Verbosity]),
	    put(verbosity, snmp_verbosity:validate(Verbosity)),
	    loop(S);

	filter_reset ->
	    reset_counters(),
	    loop(S);

	{'EXIT', Parent, Reason} ->
	    ?vlog("parent (~p) exited: "
		  "~n   ~p", [Parent, Reason]),
	    exit(Reason);

	{'EXIT', Socket, Reason} when is_port(Socket) ->
	    case lists:keyfind(Socket, #transport.socket, Transports) of
		#transport{
		  socket   = Socket,
		  domain   = Domain,
		  opts     = SocketOpts,
		  req_refs = ReqRefs} = Transport ->
		    try socket_open(Domain, SocketOpts) of
			NewSocket ->
			    error_msg(
			      "Socket ~p exited for reason"
			      "~n     ~p"
			      "~n     Re-opened (~p)",
			      [Socket, Reason, NewSocket]),
			    (length(ReqRefs) < Limit) andalso
				active_once(NewSocket),
			    S#state{
			      transports =
				  lists:keyreplace(
				    Socket, #transport.socket, Transports,
				    Transport#transport{socket = NewSocket})}
		    catch
			ReopenReason ->
			    error_msg(
			      "Socket ~p exited for reason"
			      "~n     ~p"
			      "~n     Re-open failed with reason"
			      "~n     ~p",
			      [Socket, Reason, ReopenReason]),
			    exit(ReopenReason)
		    end;
		false ->
		    error_msg(
		      "Exit message from port ~p for reason ~p~n",
		      [Socket, Reason]),
		    loop(S)
	    end;

	{'EXIT', Pid, Reason} when is_pid(Pid) ->
	    ?vlog("~p exited: "
		  "~n   ~p", [Pid, Reason]),
	    NewS = clear_reqs(Pid, S),
	    loop(NewS);

	{system, From, Msg} ->
	    ?vdebug("system event ~p from ~p", [Msg, From]),
	    sys:handle_system_msg(Msg, From, Parent, ?MODULE, [], S);

	_ ->
	    loop(S)
    end.


update_req_counter_incoming(
  #state{limit = infinity} = S,
  #transport{socket = Socket},
  _ReqRef) ->
    active_once(Socket), %% No limit so activate directly
    S; 
update_req_counter_incoming(
  #state{limit = Limit} = S,
  #transport{socket = Socket, req_refs = ReqRefs} = T,
  ReqRef) when length(ReqRefs) + 1 >= Limit ->
    %% Ok, one more and we are at the limit.
    %% Just make sure we are not already processing this one...
    case lists:member(ReqRef, ReqRefs) of
	false ->
	    %% We are at the limit, do _not_ activate socket
	    update_transport_req_refs(S, T, [ReqRef | ReqRefs]);
	true ->
	    active_once(Socket),
	    S
    end;
update_req_counter_incoming(
  #state{} = S,
  #transport{socket = Socket, req_refs = ReqRefs} = T,
  ReqRef) ->
    active_once(Socket),
    case lists:member(ReqRef, ReqRefs) of
	false ->
	    update_transport_req_refs(S, T, [ReqRef | ReqRefs]);
	true ->
	    S
    end.

update_req_counter_outgoing(
  #state{limit = infinity} = S,
  _Transport, _ReqRef) ->
    %% Already activated (in the incoming function)
    S;
update_req_counter_outgoing(
  #state{limit = Limit} = S,
  #transport{socket = Socket, req_refs = ReqRefs} = Transport,
  ReqRef) ->
    LengthReqRefs = length(ReqRefs),
    ?vtrace("update_req_counter_outgoing() -> entry with~n"
	    "   Limit:          ~w~n"
	    "   ReqRef:          ~w~n"
	    "   length(ReqRefs): ~w", [Limit, ReqRef, LengthReqRefs]),
    NewReqRefs = lists:delete(ReqRef, ReqRefs),
    (LengthReqRefs >= Limit) andalso (length(NewReqRefs) < Limit) andalso
	begin
	    ?vtrace("update_req_counter_outgoing -> "
		    "passed below limit: activate", []),
	    active_once(Socket)
	end,
    update_transport_req_refs(S, Transport, NewReqRefs).

update_transport_req_refs(
  #state{transports = Transports} = S,
  #transport{socket = Socket} = T,
  ReqRefs) ->
    S#state{
      transports =
	  lists:keyreplace(
	    Socket, #transport.socket, Transports,
	    T#transport{req_refs = ReqRefs})}.


maybe_handle_recv(
  #state{filter = FilterMod} = S,
  #transport{socket = Socket} = Transport,
  From, Packet) ->
    {From_1, From_2} = From,
    case
	try FilterMod:accept_recv(From_1, From_2)
	catch
	    Class:Exception:StackTrace ->
		error_msg(
		  "FilterMod:accept_recv(~p, ~p) crashed: ~w:~w~n    ~p",
		  [From_1, From_2, Class, Exception, StackTrace]),
		true
	end
    of
	false ->
	    %% Drop the received packet 
	    inc(netIfMsgInDrops),
	    active_once(Socket),
	    S;
	Other ->
	    case Other of
		true ->
		    ok;
		_ ->
		    error_msg(
		      "FilterMod:accept_recv(~p, ~p) returned: ~p",
		      [From_1,From_2,Other])
	    end,
	    handle_recv(S, Transport, From, Packet)
    end.

handle_recv(
  #state{mpd_state  = MpdState, note_store = NS, log = Log} = S,
  #transport{socket = Socket} = Transport,
  From, Packet) ->
    put(n1, erlang:monotonic_time(micro_seconds)),
    LogF =
	fun(Type, Data) ->
		log(Log, Type, Data, From)
	end,
    case (catch snmpa_mpd:process_packet(
		  Packet, From, MpdState, NS, LogF)) of
	{ok, _Vsn, Pdu, _PduMS, {discovery, ManagerEngineId}} ->
	    handle_discovery_response(
	      S, Transport, From, Pdu, ManagerEngineId);

	{ok, _Vsn, Pdu, _PduMS, discovery} ->
	    handle_discovery_response(
	      S, Transport, From, Pdu, undefined);

	{ok, Vsn, Pdu, PduMS, ACMData} ->
	    ?vlog("got pdu ~s", 
		  [?vapply(snmp_misc, format, [256, "~w", [Pdu]])]),
	    %% handle_recv_pdu(S, Transport, From, Vsn, Pdu, PduMS, ACMData);
	    maybe_handle_recv_pdu(
	      S, Transport, From, Vsn, Pdu, PduMS, ACMData);

	{discarded, Reason} ->
	    ?vlog("packet discarded for reason: ~s",
		  [?vapply(snmp_misc, format, [256, "~w", [Reason]])]),
	    active_once(Socket),
	    S;

	{discarded, Reason, ReportPacket} ->
	    ?vlog("sending report for reason: "
		"~n   ~s", 
		[?vapply(snmp_misc, format, [256, "~w", [Reason]])]),
	    (catch udp_send(Socket, From, ReportPacket)),
	    active_once(Socket),
	    S;
	
	{discovery, ReportPacket} ->
	    ?vlog("sending discovery report", []),
	    (catch udp_send(Socket, From, ReportPacket)),
	    active_once(Socket),
	    S;
	
	Error ->
	    error_msg("processing of received message failed: "
                      "~n   ~p", [Error]),
	    active_once(Socket),
	    S
    end.

handle_discovery_response(
  #state{reqs = Reqs} = S,
  #transport{socket = Socket},
  _From,
  #pdu{request_id = ReqId} = Pdu,
  ManagerEngineId) ->
    case lists:keyfind(ReqId, 1, S#state.reqs) of
	{ReqId, Pid} ->
	    active_once(Socket),
	    Pid ! {snmp_discovery_response_received, Pdu, ManagerEngineId},
	    %% XXX Strange... Reqs from this Pid should be reaped
	    %% at process exit by clear_reqs/2 so the following
	    %% should be redundant.
            NReqs = lists:keydelete(ReqId, 1, Reqs -- [{0, Pid}]), % ERIERL-427
	    S#state{reqs = NReqs};

        %% <OTP-16207>
        %% For some reason 'snmptrapd' response in stage 2 with request-id
        %% of zero.
        false when (ReqId =:= 0) ->
            DiscoReqs = [X|| {0, From1}     <- S#state.reqs,
                             {_, From2} = X <- S#state.reqs, From1 =:= From2],
            case (length(DiscoReqs) =:= 2) of
                true ->
                    [{_, Pid}, _] = DiscoReqs,
                    active_once(Socket),
                    Pid ! {snmp_discovery_response_received, Pdu,
                           ManagerEngineId},
                    NReqs = S#state.reqs -- DiscoReqs,
                    S#state{reqs = NReqs};
                false ->
                    S
            end;
        %% </OTP-16207>

	false ->
	    %% Ouch, timeout? resend?
	    S
    end.

maybe_handle_recv_pdu(
  #state{filter = FilterMod} = S,
  #transport{socket = Socket} = Transport,
  From, Vsn,
  #pdu{type = Type} = Pdu, PduMS, ACMData) ->
    {From_1, From_2} = From,
    case
	try FilterMod:accept_recv_pdu(From_1, From_2, Type)
	catch
	    Class:Exception:StackTrace ->
		error_msg(
		  "FilterMod:accept_recv_pdu(~p, ~p, ~p) crashed: ~w:~w~n"
		  "    ~p",
		  [From_1, From_2, Type, Class, Exception, StackTrace]),
		true
	end
    of
	false ->
	    inc(netIfPduInDrops),
	    active_once(Socket),
	    S;
	Other ->
	    case Other of
		true ->
		    ok;
		_ ->
		    error_msg(
		      "FilterMod:accept_recv_pdu(~p, ~p, ~p) returned: ~p",
		      [From_1,From_2,Type,Other])
	    end,
	    handle_recv_pdu(S, Transport, From, Vsn, Pdu, PduMS, ACMData)
    end.

handle_recv_pdu(
  #state{reqs = Reqs} = S,
  #transport{socket = Socket},
  From, Vsn,
  #pdu{type = 'get-response', request_id = ReqId} = Pdu,
  _PduMS, _ACMData) ->
    active_once(Socket),
    case lists:keyfind(ReqId, 1, Reqs) of
	{ReqId, Pid} ->
	    ?vdebug("handle_recv_pdu -> "
		    "~n   send response to receiver ~p", [Pid]),
	    Pid ! {snmp_response_received, Vsn, Pdu, From};
	false ->
	    ?vdebug("handle_recv_pdu -> "
		    "~n   No receiver available for response pdu", [])
    end,
    S;
handle_recv_pdu(
  #state{master_agent = Pid} = S,
  #transport{} = Transport,
  From, Vsn,
  #pdu{type = Type} = Pdu,
  PduMS, ACMData)
  when Type =:= 'set-request';
       Type =:= 'get-request';
       Type =:= 'get-next-request';
       Type =:= 'get-bulk-request' ->
    ?vtrace("handle_recv_pdu -> received request (~w)", [Type]),
    ReqRef = make_ref(),
    Extra = [{request_ref, ReqRef}],
    Pid ! {snmp_pdu, Vsn, Pdu, PduMS, ACMData, From, Extra},
    NewS = update_req_counter_incoming(S, Transport, ReqRef),
    ?vdebug("handle_recv_pdu -> ~p", [NewS]),
    NewS;
handle_recv_pdu(
  #state{master_agent = Pid} = S,
  #transport{socket = Socket},
  From, Vsn, Pdu, PduMS, ACMData) ->
    ?vtrace("handle_recv_pdu -> received other request", []),
    active_once(Socket),
    Pid ! {snmp_pdu, Vsn, Pdu, PduMS, ACMData, From, []},
    S.


maybe_handle_reply_pdu(
  #state{filter = FilterMod, transports = Transports} = S,
  #transport{} = Transport,
  Vsn,
  #pdu{} = Pdu,
  Type, ACMData, To) ->
    %%
    Addresses = [fix_filter_address(Transports, To)],
    case
	try
	    FilterMod:accept_send_pdu(Addresses, Type)
	catch
	    Class:Exception:StackTrace ->
		error_msg(
		  "FilterMod:accept_send_pdu(~p, ~p) crashed: ~w:~w~n    ~p",
		  [Addresses, Type, Class, Exception, StackTrace]),
		true
	end
    of
	false ->
	    inc(netIfPduOutDrops),
	    ok;
	Other ->
	    case Other of
		true ->
		    ok;
		_ ->
		    error_msg(
		      "FilterMod:accept_send_pdu(~p, ~p) returned: ~p",
		      [Addresses,Type,Other])
	    end,
	    handle_reply_pdu(S, Transport, Vsn, Pdu, Type, ACMData, To)
    end.

handle_reply_pdu(
  #state{log = Log} = S,
  #transport{} = Transport,
  Vsn,
  #pdu{} = Pdu,
  Type, ACMData, To) ->
    %%
    LogF =
	fun(Type2, Data) ->
		log(Log, Type2, Data, To)
	end,
    case (catch snmpa_mpd:generate_response_msg(Vsn, Pdu, Type,
						ACMData, LogF)) of
	{ok, Packet} ->
	    ?vinfo("time in agent: ~w mysec", [time_in_agent()]),
	    try maybe_udp_send_wo_log(S, Transport, To, Packet)
	    catch
		{Reason, Sz} ->
		    error_msg("Cannot send message "
			      "~n   size:   ~p"
			      "~n   reason: ~p"
			      "~n   pdu:    ~p",
			      [Sz, Reason, Pdu])
	    end;
	{discarded, Reason} ->
	    ?vlog("handle_reply_pdu -> "
		  "~n   reply discarded for reason: ~s", 
		[?vapply(snmp_misc, format, [256, "~w", [Reason]])]),
	    ok;
	{'EXIT', Reason} ->
	    user_err("failed generating response message: "
		     "~nPDU: ~p~n~p", [Pdu, Reason])
    end.



maybe_handle_send_pdu(
  #state{filter = FilterMod, transports = Transports} = S,
  Vsn, Pdu, MsgData, TDomAddrSecs, From) ->

    ?vtrace("maybe_handle_send_pdu -> entry with~n"
	    "   FilterMod: ~p~n"
	    "   TDomAddrSecs: ~p", [FilterMod, TDomAddrSecs]),

    DomAddrSecs = snmpa_mpd:process_taddrs(TDomAddrSecs),
    AddressesToFilter =
	case is_legacy_transports(Transports) of
	    true ->
		[fix_filter_legacy_mpd_address(DAS)
		 || DAS <- DomAddrSecs];
	    false ->
		[fix_filter_mpd_address(DAS)
		 || DAS <- DomAddrSecs]
	end,

    Type = pdu_type_of(Pdu),

    case
	try FilterMod:accept_send_pdu(AddressesToFilter, Type)
	catch
	    Class:Exception:StackTrace ->
		error_msg(
		  "FilterMod:accept_send_pdu(~p, ~p) crashed: ~w:~w~n    ~p",
		  [AddressesToFilter, Type, Class, Exception, StackTrace]),
		true
	end
    of
	false ->
	    inc(netIfPduOutDrops),
	    ok;
	true ->
	    handle_send_pdu(S, Vsn, Pdu, MsgData, DomAddrSecs, From);
	FilteredAddresses when is_list(FilteredAddresses) ->
	    FilteredDomAddrSecs =
		case is_legacy_transports(Transports) of
		    true ->
			[DAS ||
			    DAS <- DomAddrSecs,
			    lists:member(
				fix_filter_legacy_mpd_address(DAS),
				FilteredAddresses)];
		    false ->
			[DAS ||
			    DAS <- DomAddrSecs,
			    lists:member(
				fix_filter_mpd_address(DAS),
				FilteredAddresses)]
		end,
	    ?vtrace("maybe_handle_send_pdu -> FilteredDomAddrSecs:~n"
		    "   ~p", [FilteredDomAddrSecs]),
	    handle_send_pdu(S, Vsn, Pdu, MsgData, FilteredDomAddrSecs, From);
	Other ->
	    error_msg(
	      "FilterMod:accept_send_pdu(~p, ~p) returned: ~p",
	      [AddressesToFilter,Type,Other]),
	    handle_send_pdu(S, Vsn, Pdu, MsgData, DomAddrSecs, From)
    end.

handle_send_pdu(
  #state{note_store = NS} = S,
  Vsn, Pdu, MsgData, DomAddrSecs, From) ->
    %%
    ?vtrace("handle_send_pdu -> entry with~n"
	    "   Pdu:         ~p~n"
	    "   DomAddrSecs: ~p", [Pdu, DomAddrSecs]),

    case (catch snmpa_mpd:generate_msg(
		  Vsn, NS, Pdu, MsgData, DomAddrSecs)) of
	{ok, Addresses} ->
	    do_handle_send_pdu(S, Pdu, Addresses);
	{discarded, Reason} ->
	    ?vlog("handle_send_pdu -> "
		  "~n   PDU ~p not sent due to ~p", [Pdu, Reason]),
	    ok;
        {'EXIT', Reason} ->
            user_err("failed generating message: "
                     "~nPDU: ~p~n~p", [Pdu, Reason]),
            ok
    end,
    case From of
	undefined ->
	    S;
	Pid when is_pid(Pid) ->
	    ?vtrace("link to ~p and add to request list", [Pid]),
	    link(Pid),
	    NReqs =
		snmp_misc:keyreplaceadd(
		  Pid, 2, S#state.reqs, {Pdu#pdu.request_id, From}),
	    S#state{reqs = NReqs}
    end.


handle_send_discovery(
  #state{
     note_store = NS,
     log        = Log,
     reqs       = Reqs,
     transports = Transports} = S,
  #pdu{type = Type, request_id = ReqId} = Pdu,
  MsgData, To, From) when is_pid(From) ->
    
    ?vtrace("handle_send_discovery -> entry with"
	    "~n   Pdu:     ~p"
	    "~n   MsgData: ~p"
	    "~n   To:      ~p"
	    "~n   From:    ~p", [Pdu, MsgData, To, From]),

    case (catch snmpa_mpd:generate_discovery_msg(NS, Pdu, MsgData, To)) of
	{ok, {Domain, Address, Packet}} ->
	    case select_transport_from_domain(Domain, Transports) of
		false ->
		    error_msg(
		      "Can not find transport to: ~s",
		      [format_address(To)]),
		    S;
		#transport{socket = Socket} ->
		    log(Log, Type, Packet, {Domain, Address}),
		    udp_send(Socket, {Domain, Address}, Packet),
		    ?vtrace("handle_send_discovery -> sent (~w)", [ReqId]),
                    link(From),
		    NReqs  = snmp_misc:keyreplaceadd(From, 2, Reqs, {ReqId, From}),
                    NReqs2 = (NReqs -- [{0, From}]) ++ [{0, From}], % OTP-16207
		    S#state{reqs = NReqs2}
	    end;
	{discarded, Reason} ->
	    ?vlog("handle_send_discovery -> "
		  "~n   Discovery PDU ~p not sent due to ~p", [Pdu, Reason]),
	    S;
        {'EXIT', Reason} ->
            user_err("failed generating discovery message: "
                     "~n   PDU:    ~p"
		     "~n   Reason: ~p", [Pdu, Reason]),
            S
    end.


do_handle_send_pdu(S, #pdu{type = Type} = Pdu, Addresses) ->
    do_handle_send_pdu(S, Type, Pdu, Addresses);
do_handle_send_pdu(S, Trap, Addresses) ->
    do_handle_send_pdu(S, trappdu, Trap, Addresses).

do_handle_send_pdu(S, Type, Pdu, Addresses) ->
    try do_handle_send_pdu1(S, Type, Addresses)
    catch
	{Reason, Sz} ->
	    error_msg(
	      "Can not send message~n"
	      "   size:   ~p~n"
	      "   reason: ~p~n"
	      "   pdu:    ~p",
	      [Sz, Reason, Pdu])
    end.

do_handle_send_pdu1(S, Type, Addresses) ->
    lists:foreach(
      fun ({Domain, Address, Pkg}) when is_binary(Pkg) ->
	      do_handle_send_pdu2(S, Type, Domain, Address,
                                  Pkg, Pkg, "");
	  ({Domain, Address, {Pkg, LogPkg}}) when is_binary(Pkg) ->
	      do_handle_send_pdu2(S, Type, Domain, Address,
                                  Pkg, LogPkg, " encrypted")
      end,
      Addresses).

do_handle_send_pdu2(#state{transports = Transports} = S,
                    Type, Domain, Address, Pkg, LogPkg, EncrStr) ->
    ?vdebug("[~w] sending~s packet:"
            "~n   size: ~p"
            "~n   to:   ~p", [Domain, EncrStr, sz(Pkg), Address]),
    To = {Domain, Address},
    case select_transport_from_domain(Domain, Transports) of
	false ->
	    error_msg("Can not find transport: "
                      "~n   size: ~p"
                      "~n   to:   ~s",
                      [sz(Pkg), format_address(To)]);
	Transport ->
	    maybe_udp_send_w_log(S, Transport, To, Pkg, LogPkg, Type)
    end.


%% This function is used when logging has already been done!
maybe_udp_send_wo_log(
  #state{filter = FilterMod, transports = Transports},
  #transport{socket = Socket},
  To, Packet) ->
    {To_1, To_2} = fix_filter_address(Transports, To),
    case
	try FilterMod:accept_send(To_1, To_2)
	catch
	    Class:Exception:StackTrace ->
		error_msg(
		  "FilterMod:accept_send(~p, ~p) crashed: ~w:~w~n    ~p",
		  [To_1, To_2, Class, Exception, StackTrace]),
		true
	end
    of
	false ->
	    inc(netIfMsgOutDrops),
	    ok;
	Other ->
	    case Other of
		true ->
		    ok;
		_ ->
		    error_msg(
		      "FilterMod:accept_send(~p, ~p) returned: ~p",
		      [To_1,To_2,Other])
	    end,
	    udp_send(Socket, To, Packet)
    end.

maybe_udp_send_w_log(
  #state{log = Log, filter = FilterMod, transports = Transports},
  #transport{socket = Socket},
  To, Pkg, LogPkg, Type) ->
    {To_1, To_2} = fix_filter_address(Transports, To),
    case
	try FilterMod:accept_send(To_1, To_2)
	catch
	    Class:Exception:StackTrace ->
		error_msg(
		  "FilterMod:accept_send(~p, ~p) crashed for: ~w:~w~n    ~p",
		  [To_1, To_2, Class, Exception, StackTrace]),
		true
	end
    of
	false ->
	    inc(netIfMsgOutDrops),
	    ok;
	Other ->
	    case Other of
		true ->
		    ok;
		_ ->
		    error_msg(
		      "FilterMod:accept_send(~p, ~p) returned: ~p",
		      [To_1, To_2, Other])
	    end,
	    log(Log, Type, LogPkg, To),
	    udp_send(Socket, To, Pkg)
    end.

udp_send(Socket, To, B) ->
    {IpAddr, IpPort} =
	case To of
	    {Domain, Addr} when is_atom(Domain) ->
		Addr;
	    {_, P} = Addr when is_integer(P) ->
		Addr
	end,
    try gen_udp:send(Socket, IpAddr, IpPort, B) of
	{error, emsgsize} ->
	    %% From this message we cannot recover, so exit sending loop
	    throw({emsgsize, sz(B)});
	{error, ErrorReason} ->
	    error_msg("[error] cannot send message "
		      "(destination: ~p:~p, size: ~p, reason: ~p)",
		      [IpAddr, IpPort, sz(B), ErrorReason]);
	ok ->
	    ok
    catch
	error:ExitReason:StackTrace ->
	    error_msg("[exit] cannot send message "
		      "(destination: ~p:~p, size: ~p, reason: ~p, at: ~p)",
		      [IpAddr, IpPort, sz(B), ExitReason, StackTrace])
    end.

sz(L) when is_list(L) -> length(L);
sz(B) when is_binary(B) -> size(B);
sz(_) -> undefined.


handle_disk_log(_Log, {wrap, NoLostItems}, State) ->
    ?vlog("Audit Trail Log - wrapped: ~w previously logged items where lost", 
	  [NoLostItems]),
    State;
handle_disk_log(_Log, {truncated, NoLostItems}, State) ->
    ?vlog("Audit Trail Log - truncated: ~w items where lost when truncating", 
	  [NoLostItems]),
    State;
handle_disk_log(_Log, full, State) ->
    error_msg("Failure to write to Audit Trail Log (full)", []),
    State;
handle_disk_log(_Log, {error_status, ok}, State) ->
    State;
handle_disk_log(_Log, {error_status, {error, Reason}}, State) ->
    error_msg("Error status received from Audit Trail Log: "
	      "~n~p", [Reason]),
    State;
handle_disk_log(_Log, _Info, State) ->
    State.


clear_reqs(Pid, S) ->
    NReqs  = lists:keydelete(Pid, 2, S#state.reqs),
    NReqs2 = NReqs -- [{0, Pid}], % ERIERL-427
    S#state{reqs = NReqs2}.


toname(P) when is_pid(P) ->
    case process_info(P, registered_name) of
	{registered_name, Name} ->
	    Name;
	_ ->
	    P
    end;
toname(Else) ->
    Else.


active_once(Sock) ->
    ?vtrace("activate once", []),
    inet:setopts(Sock, [{active, once}]).


select_transport_from_req_ref(_, []) ->
    false;
select_transport_from_req_ref(
  ReqRef,
  [#transport{req_refs = ReqRefs} = Transport | Transports]) ->
    case lists:member(ReqRef, ReqRefs) of
	true ->
	    Transport;
	false ->
	    select_transport_from_req_ref(ReqRef, Transports)
    end.

select_transport_from_domain(Domain, Transports) when is_atom(Domain) ->
    Pos = #transport.domain,
    case lists:keyfind(Domain, Pos, Transports) of
	#transport{domain = Domain} = Transport ->
	    Transport;
	false when Domain == snmpUDPDomain ->
	    lists:keyfind(transportDomainUdpIpv4, Pos, Transports);
	false when Domain == transportDomainUdpIpv4 ->
	    lists:keyfind(snmpUDPDomain, Pos, Transports);
	false ->
	    false
    end.

address_to_domain({Domain, _Addr}) when is_atom(Domain) ->
    Domain;
address_to_domain({_Ip, Port}) when is_integer(Port) ->
    snmpUDPDomain.

%% If the agent uses legacy snmpUDPDomain e.g has not set
%% intAgentTransportDomain, then make sure
%% snmpa_network_interface_filter gets legacy arguments
%% to not break backwards compatibility.
%%
fix_filter_address(Transports, Address) ->
    case is_legacy_transports(Transports) of
	true ->
	    case Address of
		{Domain, Addr} when is_atom(Domain) ->
		    Addr;
		{_, IpPort} = Addr when is_integer(IpPort) ->
		    Addr
	    end;
	false ->
	    Address
    end.

is_legacy_transports([#transport{domain = snmpUDPDomain}]) ->
    true;
is_legacy_transports([#transport{} | _]) ->
    false.

fix_filter_legacy_mpd_address(Domain_Address_SecData) ->
    case Domain_Address_SecData of
	{{Domain, Addr}, _SecData} when is_atom(Domain) -> % v3
	    Addr;
	{Domain, Addr} when is_atom(Domain) -> % v1 & v2
	    Addr
    end.

fix_filter_mpd_address(Domain_Address_SecData) ->
    case Domain_Address_SecData of
	{{Domain, _Addr} = Address, _SecData} when is_atom(Domain) -> % v3
	    Address;
	{Domain, _Addr} = Address when is_atom(Domain) -> % v1 & v2
	    Address
    end.

%%%-----------------------------------------------------------------

handle_set_log_type(#state{log = {Log, OldValue}} = State, NewType) 
  when (Log /= undefined) ->
    NewValue =
	case NewType of
	    read ->
		[read];
	    write ->
		[write];
	    read_write ->
		[read,write];
	    _ ->
		throw({State, {error, {bad_atl_type, NewType}}})
	end,
    NewState = State#state{log = {Log, NewValue}},
    OldType =
	case {lists:member(read, OldValue),
	      lists:member(write, OldValue)} of
	    {true, true} ->
		read_write;
	    {true, false} ->
		read;
	    {false, true} ->
		write;
	    {false, false} ->
		throw({State, {error, {bad_atl_type, OldValue}}})
	end,
    {NewState, {ok, OldType}};
handle_set_log_type(State, _NewType) ->
    {State, {error, not_enabled}}.


handle_set_request_limit(#state{limit = OldLimit} = State, NewLimit) 
  when ((is_integer(NewLimit) andalso (NewLimit >= 0)) orelse 
	(NewLimit == infinity)) ->
    NewState = State#state{limit = NewLimit},
    {NewState, {ok, OldLimit}};
handle_set_request_limit(State, BadLimit) ->
    {State, {error, {bad_request_limit, BadLimit}}}.


%%%-----------------------------------------------------------------
%%% System messages
%%%-----------------------------------------------------------------
system_continue(_Parent, _Dbg, S) ->
    loop(S).

system_terminate(Reason, _Parent, _Dbg, #state{log = Log}) ->
    ?vlog("system-terminate -> entry with"
	  "~n   Reason: ~p", [Reason]),
    do_close_log(Log),
    exit(Reason).

system_code_change(OldState, _Module, _OldVsn, downgrade_to_pre_4_16) ->
    {OldLog, Type} = OldState#state.log, 
    NewLog   = snmp_log:downgrade(OldLog), 
    NewState = OldState#state{log = {NewLog, Type}}, 
    {ok, NewState};

system_code_change(OldState, _Module, _OldVsn, upgrade_from_pre_4_16) ->
    Initial  = ?ATL_SEQNO_INITIAL,
    Max      = ?ATL_SEQNO_MAX, 
    Module   = snmpa_agent, 
    Function = increment_counter, 
    Args     = [atl_seqno, Initial, Max], 
    SeqNoGen = {Module, Function, Args}, 
    {OldLog, Type} = OldState#state.log, 
    NewLog   = snmp_log:upgrade(OldLog, SeqNoGen), 
    NewState = OldState#state{log = {NewLog, Type}}, 
    {ok, NewState};

system_code_change(S, _Module, _OldVsn, _Extra) ->
    {ok, S}.

do_close_log({undefined, []}) ->
    ok;
do_close_log({Log, _}) ->
    (catch snmp_log:sync(Log)),
    (catch snmp_log:close(Log)),
    ok;
do_close_log(_) ->
    ok.


%%%-----------------------------------------------------------------
%%% DEBUG FUNCTIONS
%%%-----------------------------------------------------------------
time_in_agent() ->
    erlang:monotonic_time(micro_seconds) - get(n1).

%% ----------------------------------------------------------------

pdu_type_of(#pdu{type = Type}) ->
    Type;
pdu_type_of(TrapPdu) when is_record(TrapPdu, trappdu) ->
    trap.


%%-----------------------------------------------------------------
%% Counter functions
%%-----------------------------------------------------------------

init_counters() -> 
    F = fun(Counter) -> maybe_create_counter(Counter) end,
    lists:map(F, counters()).

reset_counters() -> 
    F = fun(Counter) -> init_counter(Counter) end,
    lists:map(F, counters()).

maybe_create_counter(Counter) ->
    case ets:lookup(snmp_agent_table, Counter) of
	[_] -> ok;
	_ -> init_counter(Counter)
    end.

init_counter(Counter) -> 
    ets:insert(snmp_agent_table, {Counter, 0}).

counters() ->
    [
     netIfMsgOutDrops,
     netIfMsgInDrops,
     netIfPduOutDrops,
     netIfPduInDrops
    ].

inc(Name) -> 
    ets:update_counter(snmp_agent_table, Name, 1).

get_counters() ->
    Counters = counters(),
    get_counters(Counters, []).

get_counters([], Acc) ->
    lists:reverse(Acc);
get_counters([Counter|Counters], Acc) ->
    case ets:lookup(snmp_agent_table, Counter) of
	[CounterVal] ->
	    get_counters(Counters, [CounterVal|Acc]);
	_ ->
	    get_counters(Counters, Acc)
    end.


%% ----------------------------------------------------------------

socket_opts(Domain, {IpAddr, IpPort}, Opts) ->
    [IpPort, % Picked off at socket open, separate argument
     binary
     |   case snmp_conf:tdomain_to_family(Domain) of
	     inet6 = Family ->
		 [Family, {ipv6_v6only, true}];
	     Family ->
		 [Family]
	 end ++
	 case get_bind_to_ip_address(Opts) of
	     true ->
		 [{ip, IpAddr}];
	     _ ->
		 []
	 end ++
	 case get_no_reuse_address(Opts) of
	     false ->
		 [{reuseaddr, true}];
	     _ ->
		 []
	 end ++
	 case get_recbuf(Opts) of
	     use_default ->
		 [];
	     Sz ->
		 [{recbuf, Sz}]
	 end ++
	 case get_sndbuf(Opts) of
	     use_default ->
		 [];
	     Sz ->
		 [{sndbuf, Sz}]
	 end] ++
        case get_extra_sock_opts(Opts) of
            ESO when is_list(ESO) ->
                ESO;
            BadESO ->
                error_msg("Invalid 'extra socket options' (=> ignored):"
                          "~n   ~p", [BadESO]),
                []
        end.


%% ----------------------------------------------------------------

get_atl_type(Opts) ->
    case snmp_misc:get_option(type, Opts, read_write) of
	read_write ->
	    [read,write];
	write ->
	    [write];
	read ->
	    [read]
    end.
	
get_atl_dir(Opts) ->
    snmp_misc:get_option(dir, Opts).

get_atl_size(Opts) ->
    snmp_misc:get_option(size, Opts).

get_atl_repair(Opts) ->
    snmp_misc:get_option(repair, Opts, true).

get_atl_seqno(Opts) ->
    snmp_misc:get_option(seqno, Opts, false).

get_verbosity(Opts) ->
    snmp_misc:get_option(verbosity, Opts, ?default_verbosity).

get_vsns(Opts) ->
    snmp_misc:get_option(versions, Opts, [v1, v2, v3]).

get_req_limit(O) ->
    snmp_misc:get_option(req_limit, O, infinity).

get_filter_opts(O) ->
    snmp_misc:get_option(filter, O, []).

get_filter_module(O) ->
    snmp_misc:get_option(module, O, ?DEFAULT_FILTER_MODULE).

get_recbuf(Opts) -> 
    snmp_misc:get_option(recbuf, Opts, use_default).

get_sndbuf(Opts) -> 
    snmp_misc:get_option(sndbuf, Opts, use_default).

get_no_reuse_address(Opts) -> 
    snmp_misc:get_option(no_reuse, Opts, false).

get_bind_to_ip_address(Opts) ->
    snmp_misc:get_option(bind_to, Opts, false).

get_extra_sock_opts(Opts) ->
    snmp_misc:get_option(extra_sock_opts, Opts, []).


%% ----------------------------------------------------------------

error_msg(F,A) -> 
    ?snmpa_error("NET-IF server: " ++ F, A).

info_msg(F,A) ->
    ?snmpa_info("NET-IF server: " ++ F, A).

%% --- 

user_err(F, A) ->
    snmpa_error:user_err(F, A).
 
config_err(F, A) ->
    snmpa_error:config_err(F, A).
 

%% ----------------------------------------------------------------

call(Pid, Req) ->
    ReplyRef = make_ref(),
    Pid ! {Req, ReplyRef, self()},
    receive 
	{ReplyRef, Reply, Pid} ->
	    Reply
    after 5000 ->
	    {error, timeout}
    end.

		       
%% ----------------------------------------------------------------

get_info(#state{transports = Transports, reqs = Reqs}) ->
    ProcSize = proc_mem(self()),
    Counters = get_counters(),
    [{reqs,           Reqs},
     {counters,       Counters},
     {process_memory, ProcSize}
     | [{port_info, get_port_info(Socket)}
	|| #transport{socket = Socket} <- Transports]].

proc_mem(P) when is_pid(P) ->
    case (catch erlang:process_info(P, memory)) of
	{memory, Sz} when is_integer(Sz) ->
	    Sz;
	_ ->
	    undefined
    end.
%% proc_mem(_) ->
%%     undefined.

get_port_info(Id) ->
    PortInfo = 
	case (catch erlang:port_info(Id)) of
	    PI when is_list(PI) ->
		[{port_info, PI}];
	    _ ->
		[]
	end,
    PortStatus = 
	case (catch prim_inet:getstatus(Id)) of
	    {ok, PS} ->
		[{port_status, PS}];
	    _ ->
		[]
	end,
    PortAct = 
	case (catch inet:getopts(Id, [active])) of
	    {ok, PA} ->
		[{port_act, PA}];
	    _ ->
		[]
	end,
    PortStats = 
	case (catch inet:getstat(Id)) of
	    {ok, Stat} ->
		[{port_stats, Stat}];
	    _ ->
		[]
	end,
    IfList = 
	case (catch inet:getif(Id)) of
	    {ok, IFs} ->
		[{interfaces, IFs}];
	    _ ->
		[]
	end,
    BufSz = 
	case (catch inet:getopts(Id, [recbuf, sndbuf])) of
	    {ok, Sz} ->
		[{buffer_size, Sz}];
	    _ ->
		[]
	end,
    [{socket, Id}] ++ 
	IfList ++ 
	PortStats ++ 
	PortInfo ++ 
	PortStatus ++ 
	PortAct ++ 
	BufSz.


%% ----------------------------------------------------------------
