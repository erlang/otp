%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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

-record(state, {parent, 
		note_store,
		master_agent, 
		usock, 
		usock_opts, 
		mpd_state, 
		log,
		reqs  = [], 
		debug = false,
		limit = infinity,
		rcnt  = [],
		filter}).

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

get_port() ->
    {value, UDPPort} = snmp_framework_mib:intAgentUDPPort(get),
    UDPPort.

get_address() ->
    {value, IPAddress} = snmp_framework_mib:intAgentIpAddress(get),
    IPAddress.

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
	    loop(State);
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

    %% -- Port and address --
    UDPPort = get_port(),
    ?vdebug("port: ~w",[UDPPort]),
    IPAddress = get_address(),
    ?vdebug("addr: ~w",[IPAddress]),

    %% -- Versions --
    Vsns = get_vsns(Opts),
    ?vdebug("vsns: ~w",[Vsns]),

    %% Flow control --
    Limit      = get_req_limit(Opts),
    ?vdebug("Limit: ~w", [Limit]),
    FilterOpts = get_filter_opts(Opts),
    FilterMod  = create_filter(FilterOpts),
    ?vdebug("FilterMod: ~w", [FilterMod]),

    %% -- Audit trail log
    Log = create_log(),
    ?vdebug("Log: ~w",[Log]),


    %% -- Socket --
    IPOpts1 = ip_opt_bind_to_ip_address(Opts, IPAddress),
    IPOpts2 = ip_opt_no_reuse_address(Opts),
    IPOpts3 = ip_opt_recbuf(Opts),
    IPOpts4 = ip_opt_sndbuf(Opts),
    IPOpts  = [binary | IPOpts1 ++ IPOpts2 ++ IPOpts3 ++ IPOpts4], 
    ?vdebug("open socket with options: ~w",[IPOpts]),
    case gen_udp_open(UDPPort, IPOpts) of
	{ok, Sock} ->
	    MpdState = snmpa_mpd:init(Vsns),
	    init_counters(), 
	    active_once(Sock),
	    S = #state{parent       = Parent,
		       note_store   = NoteStore,
		       master_agent = MasterAgent, 
		       mpd_state    = MpdState,
		       usock        = Sock, 
		       usock_opts   = IPOpts,
		       log          = Log,
		       limit        = Limit,
		       filter       = FilterMod},
	    ?vdebug("started with MpdState: ~p", [MpdState]),
	    {ok, S};
	{error, Reason} ->
	    ?vinfo("Failed to open UDP socket: ~p", [Reason]),
	    {error, {udp_open, UDPPort, Reason}}
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


log({_, []}, _, _, _, _) ->
    ok;
log({Log, Types}, 'set-request', Packet, Addr, Port) ->
    case lists:member(write, Types) of
	true ->
	    snmp_log:log(Log, Packet, Addr, Port);
	false ->
	    ok
    end;
log({Log, Types}, _, Packet, Addr, Port) ->
     case lists:member(read, Types) of
	true ->
	    snmp_log:log(Log, Packet, Addr, Port);
	false ->
	    ok
    end;
log(_, _, _, _, _) ->
    ok.
   
    
gen_udp_open(Port, Opts) ->
    case init:get_argument(snmp_fd) of
	{ok, [[FdStr]]} ->
	    Fd = list_to_integer(FdStr),
	    gen_udp:open(0, [{fd, Fd}|Opts]);
	error ->
	    case init:get_argument(snmpa_fd) of
		{ok, [[FdStr]]} ->
		    Fd = list_to_integer(FdStr),
		    gen_udp:open(0, [{fd, Fd}|Opts]);
		error ->
		    gen_udp:open(Port, Opts)
	    end
    end.


loop(S) ->
    receive
	{udp, _UdpId, Ip, Port, Packet} ->
	    ?vlog("got paket from ~w:~w",[Ip,Port]),
	    NewS = maybe_handle_recv(S, Ip, Port, Packet),
	    loop(NewS);

	{info, ReplyRef, Pid} ->
	    Info = get_info(S),
	    Pid ! {ReplyRef, Info, self()},
	    loop(S);

	%% response (to get/get_next/get_bulk/set requests)
	{snmp_response, Vsn, RePdu, Type, ACMData, Dest, []} ->
	    ?vlog("reply pdu: "
		  "~n   ~s", 
		  [?vapply(snmp_misc, format, [256, "~w", [RePdu]])]),
	    NewS = maybe_handle_reply_pdu(S, Vsn, RePdu, Type, ACMData, Dest),
	    loop(NewS);

	%% Traps/notification
	{send_pdu, Vsn, Pdu, MsgData, To} ->
	    ?vdebug("send pdu: "
		    "~n   Pdu: ~p"
		    "~n   To:  ~p", [Pdu, To]),
	    NewS = maybe_handle_send_pdu(S, Vsn, Pdu, MsgData, To, undefined),
	    loop(NewS);

	%% Informs
	{send_pdu_req, Vsn, Pdu, MsgData, To, From} ->
	    ?vdebug("send pdu request: "
		    "~n   Pdu:     ~p"
		    "~n   To:      ~p"
		    "~n   From:    ~p", 
		    [Pdu, To, toname(From)]),
	    NewS = maybe_handle_send_pdu(S, Vsn, Pdu, MsgData, To, From),
	    loop(NewS);

	%% Discovery Inform
	{send_discovery, Pdu, MsgData, To, From} ->
	    ?vdebug("received send discovery request: "
		    "~n   Pdu:  ~p"
		    "~n   To:   ~p"
		    "~n   From: ~p", 
		    [Pdu, To, toname(From)]),
	    NewS = handle_send_discovery(S, Pdu, MsgData, To, From),
	    loop(NewS);

	{discarded_pdu, _Vsn, ReqId, _ACMData, Variable, _Extra} ->
	    ?vdebug("discard PDU: ~p", [Variable]),
	    snmpa_mpd:discarded_pdu(Variable),
	    NewS = update_req_counter_outgoing(S, ReqId),
	    loop(NewS);

	{get_log_type, ReplyRef, Pid} ->
	    ?vdebug("get log type: ~p", []),
	    #state{log = {_, LogType}} = S,
	    Pid ! {ReplyRef, {ok, LogType}, self()},
	    loop(S);

	{{set_log_type, NewType}, ReplyRef, Pid} ->
	    ?vdebug("set log type: ~p", [NewType]),
	    {NewState, Reply} = (catch handle_set_log_type(S, NewType)),
	    Pid ! {ReplyRef, Reply, self()},
	    loop(NewState);

	{get_request_limit, ReplyRef, Pid} ->
	    ?vdebug("get request limit: ~p", []),
	    #state{limit = Limit} = S,
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

	{'EXIT', Parent, Reason} when Parent == S#state.parent ->
	    ?vlog("parent (~p) exited: "
		  "~n   ~p", [Parent, Reason]),
	    exit(Reason);

	{'EXIT', Port, Reason} when Port == S#state.usock ->
	    UDPPort = get_port(),
	    NewS = 
		case gen_udp_open(UDPPort, S#state.usock_opts) of
		    {ok, Id} ->
			error_msg("Port ~p exited for reason"
				  "~n   ~p"
				  "~n   Re-opened (~p)", [Port, Reason, Id]),
			S#state{usock = Id};
		    {error, ReopenReason} ->
			error_msg("Port ~p exited for reason"
				  "~n   ~p"
				  "~n   Re-open failed with reason"
				  "~n   ~p", 
				  [Port, Reason, ReopenReason]),
			ok
		end,
	    loop(NewS);

	{'EXIT', Port, Reason} when is_port(Port) ->
	    error_msg("Exit message from port ~p for reason ~p~n", 
		      [Port, Reason]),
	    loop(S);

	{'EXIT', Pid, Reason} ->
	    ?vlog("~p exited: "
		  "~n   ~p", [Pid, Reason]),
	    NewS = clear_reqs(Pid, S),
	    loop(NewS);

	{system, From, Msg} ->
	    ?vdebug("system event ~p from ~p", [Msg, From]),
	    sys:handle_system_msg(Msg, From, S#state.parent, ?MODULE, [], S);

	_ ->
	    loop(S)
    end.


update_req_counter_incomming(#state{limit = infinity, usock = Sock} = S, _) ->
    active_once(Sock), %% No limit so activate directly
    S; 
update_req_counter_incomming(#state{limit = Limit, 
				    rcnt  = RCnt, 
				    usock = Sock} = S, Rid) 
  when length(RCnt) + 1 == Limit ->
    %% Ok, one more and we are at the limit.
    %% Just make sure we are not already processing this one...
    case lists:member(Rid, RCnt) of
	false ->
	    %% We are at the limit, do _not_ activate socket
	    S#state{rcnt = [Rid|RCnt]};
	true ->
	    active_once(Sock),
	    S
    end;
update_req_counter_incomming(#state{rcnt  = RCnt, 
				    usock = Sock} = S, Rid) ->
    active_once(Sock),
    case lists:member(Rid, RCnt) of
	false ->
	    S#state{rcnt = [Rid|RCnt]};
	true ->
	    S
    end.


update_req_counter_outgoing(#state{limit = infinity} = S, _Rid) ->
    %% Already activated (in the incoming function)
    S;
update_req_counter_outgoing(#state{limit = Limit, 
				   rcnt  = RCnt, 
				   usock = Sock} = S, Rid) 
  when length(RCnt) == Limit ->
    ?vtrace("handle_req_counter_outgoing(~w) -> entry with"
	"~n   Rid:          ~w"
	"~n   length(RCnt): ~w", [Limit, Rid, length(RCnt)]),
    case lists:delete(Rid, RCnt) of
	NewRCnt when length(NewRCnt) < Limit ->
	    ?vtrace("update_req_counter_outgoing -> "
		"passed below limit: activate", []),
	    active_once(Sock),
	    S#state{rcnt = NewRCnt};
	_ ->
	    S
    end;
update_req_counter_outgoing(#state{limit = Limit, rcnt = RCnt} = S, 
			    Rid) ->
    ?vtrace("handle_req_counter_outgoing(~w) -> entry with"
	"~n   Rid:          ~w"
	"~n   length(RCnt): ~w", [Limit, Rid, length(RCnt)]),
    NewRCnt = lists:delete(Rid, RCnt),
    S#state{rcnt = NewRCnt}.
    

maybe_handle_recv(#state{filter = FilterMod} = S, 
		  Ip, Port, Packet) ->
    case (catch FilterMod:accept_recv(Ip, Port)) of
	false ->
	    %% Drop the received packet 
	    inc(netIfMsgInDrops),
	    S;
	_ ->
	    handle_recv(S, Ip, Port, Packet)
    end.

handle_discovery_response(_Ip, _Port, #pdu{request_id = ReqId} = Pdu, 
			  ManagerEngineId, 
			  #state{usock = Sock, reqs = Reqs} = S) ->
    case lists:keysearch(ReqId, 1, S#state.reqs) of
	{value, {_, Pid}} ->
	    active_once(Sock),
	    Pid ! {snmp_discovery_response_received, Pdu, ManagerEngineId},
	    NReqs = lists:keydelete(ReqId, 1, Reqs),
	    S#state{reqs = NReqs};
	_ ->
	    %% Ouch, timeout? resend?
	    S
    end.
	   
    
handle_recv(#state{usock      = Sock, 
		   mpd_state  = MpdState, 
		   note_store = NS,
		   log        = Log} = S, Ip, Port, Packet) ->
    put(n1, erlang:now()),
    LogF = fun(Type, Data) ->
		   log(Log, Type, Data, Ip, Port)
	   end,
    case (catch snmpa_mpd:process_packet(Packet, snmpUDPDomain, {Ip, Port}, 
					 MpdState, NS, LogF)) of
	{ok, _Vsn, Pdu, _PduMS, {discovery, ManagerEngineId}} ->
	    handle_discovery_response(Ip, Port, Pdu, ManagerEngineId, S);

	{ok, _Vsn, Pdu, _PduMS, discovery} ->
	    handle_discovery_response(Ip, Port, Pdu, undefined, S);

	{ok, Vsn, Pdu, PduMS, ACMData} ->
	    ?vlog("got pdu ~s", 
		  [?vapply(snmp_misc, format, [256, "~w", [Pdu]])]),
	    %% handle_recv_pdu(Ip, Port, Vsn, Pdu, PduMS, ACMData, S);
	    maybe_handle_recv_pdu(Ip, Port, Vsn, Pdu, PduMS, ACMData, S);

	{discarded, Reason} ->
	    ?vlog("packet discarded for reason: ~s",
		  [?vapply(snmp_misc, format, [256, "~w", [Reason]])]),
	    active_once(Sock),
	    S;

	{discarded, Reason, ReportPacket} ->
	    ?vlog("sending report for reason: "
		"~n   ~s", 
		[?vapply(snmp_misc, format, [256, "~w", [Reason]])]),
	    (catch udp_send(S#state.usock, Ip, Port, ReportPacket)),
	    active_once(Sock),
	    S;
	
	{discovery, ReportPacket} ->
	    ?vlog("sending discovery report", []),
	    (catch udp_send(S#state.usock, Ip, Port, ReportPacket)),
	    active_once(Sock),
	    S;
	
	Error ->
	    error_msg("processing of received message failed: "
                      "~n   ~p", [Error]),
	    active_once(Sock),
	    S
    end.
    
maybe_handle_recv_pdu(Ip, Port, Vsn, #pdu{type = Type} = Pdu, PduMS, ACMData, 
		      #state{usock = Sock, filter = FilterMod} = S) ->
    case (catch FilterMod:accept_recv_pdu(Ip, Port, Type)) of
	false ->
	    inc(netIfPduInDrops),
	    active_once(Sock),
	    ok;
	_ ->
	    handle_recv_pdu(Ip, Port, Vsn, Pdu, PduMS, ACMData, S)
    end;
maybe_handle_recv_pdu(Ip, Port, Vsn, Pdu, PduMS, ACMData, S) ->
    handle_recv_pdu(Ip, Port, Vsn, Pdu, PduMS, ACMData, S).

handle_recv_pdu(Ip, Port, Vsn, #pdu{type = 'get-response'} = Pdu, 
		_PduMS, _ACMData, #state{usock = Sock} = S) ->
    active_once(Sock),
    handle_response(Vsn, Pdu, {Ip, Port}, S),
    S;
handle_recv_pdu(Ip, Port, Vsn, #pdu{request_id = Rid, type = Type} = Pdu, 
		PduMS, ACMData, #state{master_agent = Pid} = S) 
  when ((Type =:= 'get-request') orelse
	(Type =:= 'get-next-request') orelse
	(Type =:= 'get-bulk-request')) ->
    ?vtrace("handle_recv_pdu -> received get (~w)", [Type]),
    Pid ! {snmp_pdu, Vsn, Pdu, PduMS, ACMData, {Ip, Port}, []},
    update_req_counter_incomming(S, Rid);
handle_recv_pdu(Ip, Port, Vsn, Pdu, PduMS, ACMData, 
		#state{usock = Sock, master_agent = Pid} = S) ->
    ?vtrace("handle_recv_pdu -> received other request", []),
    active_once(Sock),
    Pid ! {snmp_pdu, Vsn, Pdu, PduMS, ACMData, {Ip, Port}, []},
    S.


maybe_handle_reply_pdu(#state{filter = FilterMod} = S, Vsn, 
		       #pdu{request_id = Rid} = Pdu, 
		       Type, ACMData, {Ip, Port} = Dest) ->
    S1 = update_req_counter_outgoing(S, Rid),
    case (catch FilterMod:accept_send_pdu([{Ip, Port}], Type)) of
	false ->
	    inc(netIfPduOutDrops),
	    ok;
	_ ->
	    handle_reply_pdu(S1, Vsn, Pdu, Type, ACMData, Dest)
    end,
    S1.

handle_reply_pdu(#state{log    = Log,
			usock  = Sock, 
			filter = FilterMod}, 
		 Vsn, Pdu, Type, ACMData, {Ip, Port}) ->
    LogF = fun(Type2, Data) ->
		   log(Log, Type2, Data, Ip, Port)
	   end,
    case (catch snmpa_mpd:generate_response_msg(Vsn, Pdu, Type,
						ACMData, LogF)) of
	{ok, Packet} ->
	    ?vinfo("time in agent: ~w mysec", [time_in_agent()]),
	    maybe_udp_send(FilterMod, Sock, Ip, Port, Packet);
	{discarded, Reason} ->
	    ?vlog("handle_reply_pdu -> "
		  "~n   reply discarded for reason: ~s", 
		[?vapply(snmp_misc, format, [256, "~w", [Reason]])]),
	    ok;
	{'EXIT', Reason} ->
	    user_err("failed generating response message: "
		     "~nPDU: ~w~n~w", [Pdu, Reason])
    end.
    

process_taddrs(To) ->
    process_taddrs(To, []).

process_taddrs([], Acc) ->
    lists:reverse(Acc);
%% v3 
process_taddrs([{{_Domain, AddrAndPort}, _SecData}|T], Acc) ->
    process_taddrs(T, [AddrAndPort|Acc]);
%% v1 & v2
process_taddrs([{_Domain, AddrAndPort}|T], Acc) ->
    process_taddrs(T, [AddrAndPort|Acc]).


merge_taddrs(To1, To2) ->
    merge_taddrs(To1, To2, []).

merge_taddrs([], _To2, Acc) ->
    lists:reverse(Acc);
%% v3
merge_taddrs([{{_, AddrAndPort}, _} = H|To1], To2, Acc) ->
    case lists:member(AddrAndPort, To2) of
	true ->
	    merge_taddrs(To1, To2, [H|Acc]);
	false ->
	    merge_taddrs(To1, To2, Acc)
    end;
%% v1 & v2
merge_taddrs([{_, AddrAndPort} = H|To1], To2, Acc) ->
    case lists:member(AddrAndPort, To2) of
	true ->
	    merge_taddrs(To1, To2, [H|Acc]);
	false ->
	    merge_taddrs(To1, To2, Acc)
    end;
merge_taddrs([_Crap|To1], To2, Acc) ->
    merge_taddrs(To1, To2, Acc).


maybe_handle_send_pdu(#state{filter = FilterMod} = S,
		      Vsn, Pdu, MsgData, To0, From) ->

    ?vtrace("maybe_handle_send_pdu -> entry with"
	    "~n   FilterMod: ~p"
	    "~n   To0:       ~p", [FilterMod, To0]),

    To1 = snmpa_mpd:process_taddrs(To0),
    To2 = process_taddrs(To1),

    case (catch FilterMod:accept_send_pdu(To2, pdu_type_of(Pdu))) of
	false ->
	    inc(netIfPduOutDrops),
	    ok;
	true ->
	    handle_send_pdu(S, Vsn, Pdu, MsgData, To1, From);
	To3 when is_list(To3) ->
	    To4 = merge_taddrs(To1, To3),
	    ?vtrace("maybe_handle_send_pdu -> To4: "
		    "~n   ~p", [To4]),
	    handle_send_pdu(S, Vsn, Pdu, MsgData, To4, From);
	_ ->
	    handle_send_pdu(S, Vsn, Pdu, MsgData, To1, From)
    end.

handle_send_pdu(#state{note_store = NS} = S, Vsn, Pdu, MsgData, To, From) ->
    
    ?vtrace("handle_send_pdu -> entry with"
	    "~n   Pdu: ~p"
	    "~n   To:  ~p", [Pdu, To]),

    case (catch snmpa_mpd:generate_msg(Vsn, NS, Pdu, MsgData, To)) of
	{ok, Addresses} ->
	    handle_send_pdu(S, Pdu, Addresses);
	{discarded, Reason} ->
	    ?vlog("handle_send_pdu -> "
		  "~n   PDU ~p not sent due to ~p", [Pdu, Reason]),
	    ok;
        {'EXIT', Reason} ->
            user_err("failed generating message: "
                     "~nPDU: ~w~n~w", [Pdu, Reason]),
            ok
    end,
    case From of
	undefined ->
	    S;
	Pid ->
	    ?vtrace("link to ~p and add to request list", [Pid]),
	    link(Pid),
	    NReqs = snmp_misc:keyreplaceadd(Pid, 2, S#state.reqs, 
					    {Pdu#pdu.request_id, From}),
	    S#state{reqs = NReqs}
    end.


handle_send_discovery(#state{note_store = NS} = S, 
		      Pdu, MsgData, 
		      To, From) ->
    
    ?vtrace("handle_send_discovery -> entry with"
	    "~n   Pdu:     ~p"
	    "~n   MsgData: ~p"
	    "~n   To:      ~p"
	    "~n   From:    ~p", [Pdu, MsgData, To, From]),

    case (catch snmpa_mpd:generate_discovery_msg(NS, Pdu, MsgData, To)) of
	{ok, {Packet, {Ip, Port}}} ->
	    handle_send_discovery(S, Pdu, Packet, Ip, Port, From);
	{discarded, Reason} ->
	    ?vlog("handle_send_discovery -> "
		  "~n   Discovery PDU ~p not sent due to ~p", [Pdu, Reason]),
	    ok;
        {'EXIT', Reason} ->
            user_err("failed generating discovery message: "
                     "~n   PDU:    ~w"
		     "~n   Reason: ~w", [Pdu, Reason]),
            ok
    end.

handle_send_discovery(#state{log   = Log, 
			     usock = Sock,
			     reqs  = Reqs} = S, 
		      #pdu{type       = Type, 
			   request_id = ReqId}, 
		      Packet, Ip, Port, From) 
  when is_binary(Packet) ->
    log(Log, Type, Packet, Ip, Port),
    udp_send(Sock, Ip, Port, Packet),
    ?vtrace("handle_send_discovery -> sent (~w)", [ReqId]),
    NReqs = snmp_misc:keyreplaceadd(From, 2, Reqs, {ReqId, From}),
    S#state{reqs = NReqs}.


handle_send_pdu(S, #pdu{type = Type} = Pdu, Addresses) ->
    handle_send_pdu(S, Type, Pdu, Addresses);
handle_send_pdu(S, Trap, Addresses) ->
    handle_send_pdu(S, trappdu, Trap, Addresses).

handle_send_pdu(S, Type, Pdu, Addresses) ->
    case (catch handle_send_pdu1(S, Type, Addresses)) of
	{Reason, Sz} ->
	    error_msg("Cannot send message "
		      "~n   size:   ~p"
		      "~n   reason: ~p"
		      "~n   pdu:    ~p",
		      [Sz, Reason, Pdu]);
	_ ->
	    ok
    end.
	    
handle_send_pdu1(#state{log    = Log, 
			usock  = Sock,
			filter = FilterMod}, Type, Addresses) ->
    SendFun = 
	fun({snmpUDPDomain, {Ip, Port}, Packet}) when is_binary(Packet) ->
		?vdebug("sending packet:"
			"~n   size: ~p"
			"~n   to:   ~p:~p",
			[sz(Packet), Ip, Port]),
		maybe_udp_send(FilterMod, Log, Type, Sock, Ip, Port, Packet);

	   ({snmpUDPDomain, {Ip, Port}, {Packet, _LogData}}) when is_binary(Packet) ->
		?vdebug("sending encrypted packet:"
			"~n   size: ~p"
			"~n   to:   ~p:~p",
			[sz(Packet), Ip, Port]),
		maybe_udp_send(FilterMod, Log, Type, Sock, Ip, Port, Packet);

	   (_X) ->
		?vlog("** bad res: ~p", [_X]),
		ok
	end, 
    lists:foreach(SendFun, Addresses).


handle_response(Vsn, Pdu, From, S) ->
    case lists:keysearch(Pdu#pdu.request_id, 1, S#state.reqs) of
	{value, {_, Pid}} ->
	    ?vdebug("handle_response -> "
		    "~n   send response to receiver ~p", [Pid]),
	    Pid ! {snmp_response_received, Vsn, Pdu, From};
	_ ->
	    ?vdebug("handle_response -> "
		    "~n   No receiver available for response pdu", [])
    end.

maybe_udp_send(FilterMod, Sock, Ip, Port, Packet) ->
    case (catch FilterMod:accept_send(Ip, Port)) of
	false ->
	    inc(netIfMsgOutDrops),
	    ok;
	_ ->
	    (catch udp_send(Sock, Ip, Port, Packet))
    end.

maybe_udp_send(FilterMod, AtLog, Type, Sock, Ip, Port, Packet) ->
    case (catch FilterMod:accept_send(Ip, Port)) of
	false ->
	    inc(netIfMsgOutDrops),
	    ok;
	_ ->
	    log(AtLog, Type, Packet, Ip, Port),
	    (catch udp_send(Sock, Ip, Port, Packet))
    end.


udp_send(UdpId, AgentIp, UdpPort, B) ->
    case (catch gen_udp:send(UdpId, AgentIp, UdpPort, B)) of
	{error, emsgsize} ->
	    %% From this message we cannot recover, so exit sending loop
	    throw({emsgsize, sz(B)});
	{error, ErrorReason} ->
	    error_msg("[error] cannot send message "
		      "(destination: ~p:~p, size: ~p, reason: ~p)",
		      [AgentIp, UdpPort, sz(B), ErrorReason]);
	{'EXIT', ExitReason} ->
	    error_msg("[exit] cannot send message "
		      "(destination: ~p:~p, size: ~p, reason: ~p)",
		      [AgentIp, UdpPort, sz(B), ExitReason]);
	_ ->
	    ok
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
    NReqs = lists:keydelete(Pid, 2, S#state.reqs),
    S#state{reqs = NReqs}.


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
    subtr(erlang:now(), get(n1)).

subtr({X1,Y1,Z1}, {X1,Y1,Z2}) ->
    Z1 - Z2;
subtr({X1,Y1,Z1}, {X1,Y2,Z2}) ->
    ((Y1-Y2) * 1000000) + (Z1 - Z2);
subtr({X1,Y1,Z1}, {X2,Y2,Z2}) ->
    ((X1 - X2) * 1000000000000) + ((Y1 - Y2) * 1000000) + (Z1 - Z2).


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

ip_opt_bind_to_ip_address(Opts, Ip) ->
    case get_bind_to_ip_address(Opts) of
	true ->
	    [{ip, list_to_tuple(Ip)}];
	_ ->
	    []
    end.

ip_opt_no_reuse_address(Opts) ->
    case get_no_reuse_address(Opts) of
	false ->
	    [{reuseaddr, true}];
	_ ->
	    []
    end.

ip_opt_recbuf(Opts) ->
    case get_recbuf(Opts) of
	use_default ->
	    [];
	Sz ->
	    [{recbuf, Sz}]
    end.

ip_opt_sndbuf(Opts) ->
    case get_sndbuf(Opts) of
	use_default ->
	    [];
	Sz ->
	    [{sndbuf, Sz}]
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


%% ----------------------------------------------------------------

error_msg(F,A) -> 
    ?snmpa_error("NET-IF server: " ++ F, A).

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

get_info(#state{usock = Id, reqs = Reqs}) ->
    ProcSize = proc_mem(self()),
    PortInfo = get_port_info(Id),
    Counters = get_counters(),
    [{reqs,           Reqs}, 
     {counters,       Counters}, 
     {process_memory, ProcSize}, 
     {port_info,      PortInfo}].

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

% i(F) ->
%     i(F, []).

% i(F, A) ->
%     io:format("~p: " ++ F ++ "~n", [?MODULE|A]).

