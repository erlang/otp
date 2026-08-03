%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2004-2025. All Rights Reserved.
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
-moduledoc false.

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


%% Regarding Trap/Notification transport(s),
%% it *should* be possible to specify either:
%% 1) A fixed set of transport(s) used for sending.
%%    For instance, one for IPv4 and one for IPv6.
%% 2) A single one-shot ephemeral port.
%%    That is, a port is created, used once, and then closed.
%% 3) A pool of ephemeral ports, used for "a time"
%%    (a configurable number of sends, a set number of bytes
%%     or time based) thar is cycled through.
%% Though, we do *not* currently implement ephemeral sockets,
%% so case 2 and 3 are not possible at the moment.

%% Also, the request-reponder and trap-sender transport(s),
%% has different needs.
%% The trap-sender transport will send more data then it will receive.
%% Therefore, we should be able to specify;
%% bind_to, no_reuse_address, recbuf and sndbuf individually:
%% {intAgentTransports,
%%  [{transportDomainUdpIpv4, {{141,213,11,24},   PortInfo},
%%    req_responder, Opts},
%%   {transportDomainUdpIpv4, {{141,213,11,24},   PortInfo},
%%    trap_sender,   Opts},
%%   {transportDomainUdpIpv6, {{0,0,0,0,0,0,0,1}, Portinfo},
%%    req_responder, Opts},
%%   {transportDomainUdpIpv6, {{0,0,0,0,0,0,0,1}, Portinfo},
%%    trap_sender,   Opts}]}.
%% Opts is basically "socket options" for this transport (the same
%% options as for net-if).
%% Also, always give the option to provide a port range:
%%    Port     :: pos_integer() | system | range() || ranges()
%%                system => Let the system choose
%%                (0 is unfortunately already used as 'default',
%%                 so we can't use that for 'system').
%%    range()  :: {Min :: pos_integer(), Max :: pos_integer()} when Min < Max
%%    ranges() :: [pos_integer() | range()]
%%                Examples:
%%                      [{2000, 2004}]
%%                      [2000, 2001, 2002, 2003, 2004]
%%                      [2000, 2001, {2002, 2004}]
%%                      [{5000, 5100}, {6000, 6100}]

%% <EPHEMERAL-FOR-FUTUR-USE>
%% , *but*
%% may also contain the tuple, {ephemeral, EphmOpts}.
%% Ephm sockets are created on the fly, used for the specified
%% time (number of sends, number of bytes sent, used time, ...).
%% </EPHEMERAL-FOR-FUTUR-USE>

-record(state,
	{parent,
	 note_store,
	 master_agent,
	 transports  = [],
	 mpd_state,
	 log,
	 reqs  = [],
	 debug = false,
	 limit = infinity,
	 filter}).

-type transport_kind() :: req_responder | trap_sender.
-type port_info() :: non_neg_integer() |
                     system |
                     {pos_integer(), pos_integer()}.

%% <EPHEMERAL-FOR-FUTUR-USE>
%% How would 'ephemeral' effect this?
%% What kind of usage would we have for emphemeral ports?
%%    once (send and maybe receive reply (inform)) |
%%    {sends, pos_integer()} (number of sends) |
%%    {data,  pos_integer()} (number of bytes sent) |
%%    {alive_time, pos_integer()} (once used for the first time, alive time)
%% You can't combine a ephemeral info with a fixed port (pos_integer())
%% The port_info() is used when creating a port, even an ephemeral port.
%% But it must either be 'system' or a range in that (ephemeral) case.
%% Also, ephemeral transports are only allowed if kind = trap_sender
%% -type ephemeral() :: none |
%%                      once |
%%                      {sends,      pos_integer()} |
%%                      {data,       pos_integer()} |
%%                      {alive_time, pos_integer()}.

%% Note that since informs require confirmation,
%% an ephemeral socket cannot be removed immediately
%% when it has been "used up".
%% We need to keep it for some time to receive responses
%% and in case a resend is needed!.
%% </EPHEMERAL-FOR-FUTUR-USE>

-record(transport,
	{socket,
         mref,
         kind         = all :: all | transport_kind(),
	 domain       = snmpUDPDomain,
         address   :: inet:ip_address(),
         port_no   :: pos_integer(),
         port_info :: port_info(),
         %% <EPHEMERAL-FOR-FUTUR-USE>
         ephm         = none, %%  :: ephemeral(),
         ephm_info    = undefined, % Only used if ephm =/= none and once
         %% </EPHEMERAL-FOR-FUTUR-USE>
         inet_backend = [],
	 opts         = [],
	 req_refs     = [] % Not used for trap/notification transports
        }).

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
	{error, {udp_open, {open, PortNo, Reason}}} ->
            OEFilters = get_open_err_filters(Opts),
            Class =
                case lists:member(Reason, OEFilters) of
                    false ->
                        error;
                    true ->
                        info
            end,
            proc_lib:init_ack({error, {Class, udp_open, PortNo, Reason}});
	{error, Reason} ->
	    %% config_err("failed starting net-if: ~n~p", [Reason]),
	    proc_lib:init_fail({error, Reason}, {exit, normal});
	Error ->
	    %% config_err("failed starting net-if: ~n~p", [Error]),
	    proc_lib:init_fail({error, Error}, {exit, normal})
    end.

do_init(Prio, NoteStore, MasterAgent, Parent, Opts) ->
    process_flag(trap_exit, true),
    process_flag(priority,  Prio),

    %% -- Verbosity --
    put(sname,     nif),
    put(verbosity, get_verbosity(Opts)),
    ?vlog("starting",[]),

    %% -- Versions --
    Vsns = get_vsns(Opts),
    ?vdebug("vsns: ~w",[Vsns]),

    %% -- Flow control --
    Limit      = get_req_limit(Opts),
    ?vdebug("Limit: ~w", [Limit]),
    FilterOpts = get_filter_opts(Opts),
    FilterMod  = create_filter(FilterOpts),
    ?vdebug("FilterMod: ~w FilterOpts: ~p", [FilterMod,FilterOpts]),

    %% -- Audit trail log
    Log = create_log(),
    ?vdebug("Log: ~w",[Log]),

    RawTransports = get_transports(),
    ?vdebug("Raw Transports: "
            "~n   ~p", [RawTransports]),
    try
	[begin
             %% Any socket option not explicitly configured for the transport
             %% will be taken from the "global" socket options (which serve as
             %% default values).
             %% Also, note that Ephm are not actually used at this time.
	     {Ephm, IpAddr, PortInfo, InetBackend, SocketOpts} =
                 socket_opts(Domain, Address, RawSocketOpts, Opts),
             ?vtrace("socket opts processed:"
                     "~n      Ephm:         ~p"
                     "~n      Port Info:    ~p"
                     "~n      Inet Backend: ~p"
                     "~n      Socket Opts:  ~p",
                     [Ephm, PortInfo, InetBackend, SocketOpts]),
	     {Socket, IpPort}             = socket_open(Domain, PortInfo,
                                                        InetBackend,
                                                        SocketOpts),
             SockMRef = inet:monitor(Socket),
             ?vtrace("socket opened:"
                     "~n      Socket:   ~p"
                     "~n      Port No:  ~p"
                     "~n      Info:     ~p",
                     [Socket, IpPort, inet:info(Socket)]),

             %% Should we really do this here?
             %% If Kind =:= trap_sender, we only need to receive after 
             %% we have sent an inform!
	     active_once(Socket),
             ?vtrace("socket activated:"
                     "~n      Info: ~p", [inet:info(Socket)]),
	     #transport{
                socket       = Socket,
                mref         = SockMRef,
                kind         = Kind,
                domain       = Domain,
                %% We may not have explicitly specified the port ('system'
                %% or a range), so it could have been "generated".
                %% Also, shall we push this into the transport (handled by the
                %% FRAMEWORK MIB)? Would not work for ephemeral sockets.
                address      = IpAddr,
                port_no      = IpPort,
                port_info    = PortInfo,
                ephm         = Ephm,
                inet_backend = InetBackend,
                opts         = SocketOpts}
             %% We need to fix this also
	 end || {Domain, Address, Kind, RawSocketOpts} <- RawTransports]
    of
	[] ->
	    ?vinfo("No transports configured: ~p", [RawTransports]),
	    {error, {no_transports, RawTransports}};
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


socket_open(snmpUDPDomain = Domain, IpPort, InetBackend, Opts) ->
    ?vdebug("socket_open(~p) -> entry with"
            "~n   Port: ~p"
            "~n   Opts: ~p", [Domain, IpPort, Opts]),
    case init:get_argument(snmp_fd) of
	{ok, [[FdStr]]} ->
	    FD = list_to_integer(FdStr),
	    ?vdebug("socket_open(~p) -> open with fd - "
                    "(old) snmp_fd command line argument provided: "
                    "~n   FD:   ~p"
                    "~n   Port: ~p"
                    "~n   Opts: ~p", [Domain, FD, IpPort, Opts]),
	    gen_udp_open(0, InetBackend ++ [{fd, FD} | Opts]);
	error ->
	    case init:get_argument(snmpa_fd) of
		{ok, [[FdStr]]} ->
		    FD = list_to_integer(FdStr),
                    ?vdebug("socket_open(~p) -> open with fd - "
                            "snmpa_fd command line argument provided: "
                            "~n   FD:   ~p"
                            "~n   Port: ~p"
                            "~n   Opts: ~p", [Domain, FD, IpPort, Opts]),
		    gen_udp_open(0, InetBackend ++ [{fd, FD} | Opts]);
		error ->
		    ?vdebug("socket_open(~p) -> plain open"
                            "~n   Port: ~p"
                            "~n   Opts: ~p", [Domain, IpPort, Opts]),
		    gen_udp_open(IpPort, InetBackend ++ Opts)
	    end
    end;
socket_open(Domain, PortInfo, InetBackend, Opts)
  when (Domain =:= transportDomainUdpIpv4) orelse
       (Domain =:= transportDomainUdpIpv6) ->
    ?vdebug("socket_open(~p) -> entry with"
            "~n      PortInfo:    ~p"
            "~n      InetBackend: ~p"
            "~n      Opts:        ~p", [Domain, PortInfo, InetBackend, Opts]),
    gen_udp_open(PortInfo, InetBackend ++ Opts);
socket_open(Domain, PortInfo, InetBackend, Opts) ->
    ?vinfo("socket_open(~p) -> entry when invalid with"
           "~n   PortInfo:    ~p"
           "~n   InetBackend: ~p"
           "~n   Opts:        ~p", [Domain, PortInfo, InetBackend, Opts]),
    throw({socket_open, Domain, InetBackend, Opts}).


%% Make the system choose!
gen_udp_open(system, Opts) ->
    ?vtrace("gen_udp_open(system) -> entry"),
    case gen_udp:open(0, Opts) of
	{ok, Socket} ->
            case inet:port(Socket) of
                {ok, PortNo} ->
                    ?vtrace("gen_udp_open(system) -> created: "
                            "~n      ~p (~w)", [Socket, PortNo]),
                    {Socket, PortNo};
                {error, PReason} ->
                    (catch gen_udp:close(Socket)),
                    throw({udp_open, {port, PReason}})
            end;
	{error, OReason} ->
            throw({udp_open, {open, 0, OReason}})
    end;
%% This is for "future compat" since we cannot actually config '0'...
gen_udp_open(IpPort, Opts) when (IpPort =:= 0) ->
    ?vtrace("gen_udp_open(0) -> entry with"
            "~n   Opts: ~p", [Opts]),
    case gen_udp:open(IpPort, Opts) of
	{ok, Socket} ->
            case inet:port(Socket) of
                {ok, PortNo} ->
                    ?vtrace("gen_udp_open(0) -> created: "
                            "~n      ~p (~w)", [Socket, PortNo]),
                    {Socket, PortNo};
                {error, PReason} ->
                    (catch gen_udp:close(Socket)),
                    throw({udp_open, {port, PReason}})
            end;
	{error, Reason} ->
	    throw({udp_open, {open, IpPort, Reason}})
    end;
gen_udp_open(IpPort, Opts) when is_integer(IpPort) ->
    ?vtrace("gen_udp_open(~w) -> entry with"
            "~n   Opts: ~p", [IpPort, Opts]),
    case gen_udp:open(IpPort, Opts) of
	{ok, Socket} ->
            ?vtrace("gen_udp_open(~w) -> created: "
                    "~n      ~p", [Socket]),
	    {Socket, IpPort};
	{error, Reason} ->
	    throw({udp_open, {open, IpPort, Reason}})
    end;
%% A range is "pointless" if we allow reuseaddr...
%% ...but we leave that to the user...
gen_udp_open({Min, Max}, Opts) ->
    ?vtrace("gen_udp_open(~w,~w) -> entry", [Min, Max]),
    gen_udp_range_open(Min, Max, Opts);
%% A range is "pointless" if we allow reuseaddr...
%% ...but we leave that to the user...
gen_udp_open(Ranges, Opts) when is_list(Ranges) ->
    gen_udp_ranges_open(Ranges, Opts).

gen_udp_range_open(Min, Max, _Opts) when (Min > Max) ->
    ?vinfo("gen_udp_range_open -> entry when no available ports"),
    throw({udp_open, no_available_ports});
gen_udp_range_open(Min, Max, Opts) ->
    ?vtrace("gen_udp_range_open -> entry with"
            "~n      Min: ~w"
            "~n      Max: ~w", [Min, Max]),
    try gen_udp:open(Min, Opts) of
        {ok, Socket} ->
            ?vtrace("gen_udp_range_open(~w,~w) -> created: "
                    "~n      ~p", [Min, Max, Socket]),
            {Socket, Min};
        {error, eaddrinuse} ->
            ?vdebug("gen_udp_range_open(~w,~w) -> eaddrinuse"),
            gen_udp_range_open(Min+1, Max, Opts);
        {error, Reason} ->
            ?vdebug("gen_udp_range_open(~w,~w) -> ~w", [Reason]),
            throw({udp_open, {open, Min, Reason}})
    catch
        C:E:S ->
            ?vinfo("gen_udp_range_open(~w,~w) -> failed open socket: "
                   "~n      C: ~p"
                   "~n      E: ~p"
                   "~n      S: ~p", [Min, Max, C, E, S]),
            erlang:raise(C, E, S)
    end.

gen_udp_ranges_open([], _Opts) ->
    ?vinfo("gen_udp_ranges_open -> entry when no available ports"),
    throw({udp_open, no_available_ports});
gen_udp_ranges_open([PortNo|Ranges], Opts) when is_integer(PortNo) andalso
                                                (PortNo > 0) ->
    ?vtrace("gen_udp_ranges_open(~w) -> entry", [PortNo]),
    try gen_udp_open(PortNo, Opts) of
        {_Sock, PortNo} = SUCCESS when is_integer(PortNo) ->
            SUCCESS
    catch
        throw:{udp_open, _} ->
            gen_udp_ranges_open(Ranges, Opts)
    end;
gen_udp_ranges_open([{Min, Max}|Ranges], Opts) ->
    ?vtrace("gen_udp_ranges_open(~w,~w) -> entry", [Min, Max]),
    try gen_udp_range_open(Min, Max, Opts) of
        {_Sock, PortNo} = SUCCESS when is_integer(PortNo) ->
            SUCCESS
    catch
        throw:{udp_open, _} ->
            gen_udp_ranges_open(Ranges, Opts)
    end.


loop(#state{transports = Transports,
            limit      = Limit,
            parent     = Parent} = S) ->
    ?vdebug("loop(~p)", [S]),
    receive
        {udp, Socket, IpAddr, IpPort, Packet} = Msg ->
	    ?vlog("got packet from ~w:~w on ~w", [IpAddr, IpPort, Socket]),
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
		    error_msg("Packet on unknown socket: "
                              "~n   ~p", [Msg]),
		    loop(S)
	    end;

	{udp_error, Socket, Error} ->
	    ?vinfo("got udp-error on ~p: ~w", [Socket, Error]),
	    case lists:keyfind(Socket, #transport.socket, Transports) of
		#transport{socket = Socket} = Transport ->
		    loop(handle_udp_error(S, Transport, Error));
		false ->
		    loop(handle_udp_error_unknown(S, Socket, Error))
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
		    select_transport(ReqRef, Transports)
		of
		    false ->
			select_transport(address_to_domain(To), Type,
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
			select_transport(ReqRef, Transports)
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

        {'DOWN', _SockMRef, Type, Socket, Reason} when (Type =:= port) orelse
                                                       (Type =:= socket) ->
	    case lists:keyfind(Socket, #transport.socket, Transports) of
		#transport{
                   socket       = Socket,
                   domain       = Domain,
                   port_info    = PortInfo,
                   inet_backend = InetBackend,
                   opts         = SocketOpts,
                   req_refs     = ReqRefs} = Transport ->
		    try socket_open(Domain, PortInfo, InetBackend, SocketOpts) of
			{NewSocket, PortNo} ->
			    error_msg(
			      "Socket ~p exited for reason"
			      "~n     ~p"
			      "~n     Re-opened (~p, ~w)",
			      [Socket, Reason, NewSocket, PortNo]),
			    (length(ReqRefs) < Limit) andalso
				active_once(NewSocket),
                            NewSockMRef = inet:monitor(NewSocket),
			    S#state{
			      transports =
				  lists:keyreplace(
				    Socket, #transport.socket, Transports,
				    Transport#transport{socket  = NewSocket,
                                                        mref    = NewSockMRef,
                                                        port_no = PortNo})}
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
		      "Exit message from socket ~p for reason ~p~n",
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

handle_udp_error(S, #transport{socket = Socket,
                               kind   = Kind}, Error) ->
    try inet:sockname(Socket) of
        {ok, {IP, Port}} ->
            error_msg("UDP Error for transport: "
                      "~n      Socket: ~s"
                      "~n         Addr: ~p"
                      "~n         Port: ~p"
                      "~n      Kind:   ~p"
                      "~n      Error:  ~p",
		      [inet:socket_to_list(Socket), IP, Port, Kind, Error]);
        {error, _} ->
            error_msg("UDP Error for transport: "
                      "~n      Socket: ~p"
                      "~n      Kind:   ~p"
                      "~n      Error:  ~p", [Socket, Kind, Error])
    catch
        _:_:_ ->
            error_msg("UDP Error for transport: "
                      "~n      Socket: ~p"
                      "~n      Kind:   ~p"
                      "~n      Error:  ~p", [Socket, Kind, Error])
    end,
    S.

handle_udp_error_unknown(S, Socket, Error) ->
    try inet:sockname(Socket) of
        {ok, {IP, Port}} ->
            warning_msg("UDP Error for unknown transport: "
                        "~n      Socket: ~p (~p, ~p)"
                        "~n      Error:  ~p", [Socket, IP, Port, Error]);
        {error, _} ->
            warning_msg("UDP Error for unknown transport: "
                        "~n      Socket: ~p"
                        "~n      Error:  ~p", [Socket, Error])
    catch
        _:_:_ ->
            warning_msg("UDP Error for transport: "
                        "~n      Socket: ~p"
                        "~n      Error:  ~p", [Socket, Error])
    end,
    S.


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
	    "   Limit:           ~w~n"
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
            %% What if this is an expected (inform) reply
            %% on an ephemeral socket?
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
    active_once(Socket),
    case lists:keyfind(ReqId, 1, S#state.reqs) of
	{ReqId, Pid} ->
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



maybe_handle_send_pdu(#state{filter     = FilterMod,
                             transports = Transports} = S,
                       Vsn, Pdu, MsgData, TDomAddrSecs, From) ->
    
    ?vtrace("maybe_handle_send_pdu -> entry with"
	    "~n   FilterMod:    ~p"
	    "~n   TDomAddrSecs: ~p", [FilterMod, TDomAddrSecs]),

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
	    case select_transport(Domain, Type, Transports) of
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

%% Because of the ephemeral sockets used by some transports,
%% the list of transports may be update for each send...
do_handle_send_pdu1(S, _Type, []) ->
    S;
do_handle_send_pdu1(S, Type, [{Domain, Address, Pkg}|Addresses])
  when is_binary(Pkg) ->
    NewS = do_handle_send_pdu2(S, Type, Domain, Address,
                               Pkg, Pkg, ""),
    do_handle_send_pdu1(NewS, Type, Addresses);
do_handle_send_pdu1(S, Type, [{Domain, Address, {Pkg, LogPkg}}|Addresses])
  when is_binary(Pkg) ->
    NewS = do_handle_send_pdu2(S, Type, Domain, Address,
                               Pkg, LogPkg, " encrypted"),
    do_handle_send_pdu1(NewS, Type, Addresses).


do_handle_send_pdu2(#state{transports = Transports} = S,
                    Type, Domain, Address, Pkg, LogPkg, EncrStr) ->
    ?vdebug("[~w] sending~s packet:"
            "~n   size: ~p"
            "~n   to:   ~p", [Domain, EncrStr, sz(Pkg), Address]),
    To = {Domain, Address},
    case select_transport(Domain, Type, Transports) of
	false ->
	    error_msg("Transport not found: "
                      "~n   size: ~p"
                      "~n   to:   ~s",
                      [sz(Pkg), format_address(To)]),
            S;
	#transport{ephm = none} = Transport ->
            ?vtrace("do_handle_send_pdu2 -> transport(ephm = none) selected: "
                    "~n      ~p", [Transport]),
	    maybe_udp_send_w_log(S, Transport, To, Pkg, LogPkg, Type),
            S%;

            %% <EPHEMERAL-FOR-FUTUR-USE>
            %% #transport{} = Transport ->
            %% ?vtrace("do_handle_send_pdu2 -> transport selected: "
            %%         "~n      ~p", [Transport]),
	    %% case maybe_udp_send_w_log(S, Transport, To, Pkg, LogPkg, Type) of
            %%     {ok, Sz} -> % we actually sent something
            %%        maybe_update_ephm_transport(S, Transport, Type, Sz);
            %%     _ -> % Non-fatal error -> Nothing sent
            %%         S
            %% end
            %% </EPHEMERAL-FOR-FUTUR-USE>
    end.


%% <EPHEMERAL-FOR-FUTUR-USE>

%% For inform:
%% This will have a reply, so we cannot close it directly!
%% Also, we will resend (a couple of times), if we don't
%% get a reply in time.
%% So, how do we handle this transport in this case?
%% Shall we create a list of used sockets? Tagged with a key
%% so we can reuse the correct socket for a resend?

%%
%% Or shall we used the same socket for all the sends for this
%% trap/inform? That is, we send the trap/inform to a number of
%% targets (the Addresses list), and *that* is considered *one*
%% use?
%% That would mean that we would potentially need to wait for
%% replies from a large number of targets?
%% But it may be better in the long term, because we will not
%% use of so many sockets.
%%

%% maybe_update_ephm_transport(S, #transport{ephm = once} = _Transport,
%%                             'inform-request' = _Type, _Sz) ->
%%     S; % Figure out the above first!

%% %% Before we close the current socket, create the new.
%% %% This is done in case we fail to create a new socket
%% %% (if we first close the current and then fail to create
%% %%  the new, we are stuck).
%% %% If we fail to create the new socket, we keep the current.
%% %% Better then nothing!
%% maybe_update_ephm_transport(S, #transport{socket    = OldSocket,
%%                                           ephm      = once,
%%                                           port_info = PortInfo,
%%                                           opts      = Opts} = Transport,
%%                             _Type, _Sz) ->
%%     try
%%         begin
%%             {Socket, PortNo} = gen_udp_open(PortInfo, Opts),
%%             (catch gen_udp:close(OldSocket)),
%%             T2 = Transport#transport{socket  = Socket,
%%                                      port_no = PortNo},
%%             TS  = S#state.transports,
%%             TS2 = lists:keyreplace(OldSocket, #transport.socket, TS, T2),
%%             S#state{transports = TS2}
%%         end
%%     catch
%%         _:_:_ ->
%%             %% We need to identify which transport!
%%             error_msg("Failed creating new ephemeral socket for transport"),
%%             S
%%     end;

%% %% Note that we do not currently handle inform:s, as that adds a whole
%% %% set of issues. See above for more info.
%% maybe_update_ephm_transport(S, #transport{socket    = Socket,
%%                                           ephm      = {sends, MaxSends},
%%                                           ephm_info = NumSends,
%%                                           port_info = _PortInfo,
%%                                           opts      = _Opts} = Transport,
%%                             _Type, _Sz) when (MaxSends > NumSends) ->
%%     T2  = Transport#transport{ephm_info = NumSends + 1},
%%     TS  = S#state.transports,
%%     TS2 = lists:keyreplace(Socket, #transport.socket, TS, T2),
%%     S#state{transports = TS2};
%% maybe_update_ephm_transport(S, #transport{socket    = OldSocket,
%%                                           ephm      = {sends, _MaxSends},
%%                                           ephm_info = _NumSends,
%%                                           port_info = PortInfo,
%%                                           opts      = Opts} = Transport,
%%                             _Type, _Sz) ->
%%     try
%%         begin
%%             {Socket, PortNo} = gen_udp_open(PortInfo, Opts),
%%             (catch gen_udp:close(OldSocket)),
%%             T2 = Transport#transport{socket    = Socket,
%%                                      ephm_info = 0,
%%                                      port_no   = PortNo},
%%             TS  = S#state.transports,
%%             TS2 = lists:keyreplace(OldSocket, #transport.socket, TS, T2),
%%             S#state{transports = TS2}
%%         end
%%     catch
%%         _:_:_ ->
%%             %% We need to identify which transport!
%%             error_msg("Failed creating new ephemeral socket for transport"),
%%             S
%%     end;

%% %% Note that we do not currently handle inform:s, as that adds a whole
%% %% set of issues. See above for more info.
%% maybe_update_ephm_transport(S, #transport{socket    = Socket,
%%                                           ephm      = {data, MaxData},
%%                                           ephm_info = AccSent,
%%                                           port_info = _PortInfo,
%%                                           opts      = _Opts} = Transport,
%%                             _Type, Sz) when (MaxData > (AccSent + Sz)) ->
%%     T2 = Transport#transport{ephm_info = AccSent + Sz},
%%     TS  = S#state.transports,
%%     TS2 = lists:keyreplace(Socket, #transport.socket, TS, T2),
%%     S#state{transports = TS2};
%% maybe_update_ephm_transport(S, #transport{socket    = OldSocket,
%%                                           ephm      = {data, _MaxData},
%%                                           ephm_info = _AccSent,
%%                                           port_info = PortInfo,
%%                                           opts      = Opts} = Transport,
%%                             _Type, _Sz) ->
%%     try
%%         begin
%%             {Socket, PortNo} = gen_udp_open(PortInfo, Opts),
%%             (catch gen_udp:close(OldSocket)),
%%             T2 = Transport#transport{socket    = Socket,
%%                                      ephm_info = 0,
%%                                      port_no   = PortNo},
%%             TS  = S#state.transports,
%%             TS2 = lists:keyreplace(OldSocket, #transport.socket, TS, T2),
%%             S#state{transports = TS2}
%%         end
%%     catch
%%         _:_:_ ->
%%             %% We need to identify which transport!
%%             error_msg("Failed creating new ephemeral socket for transport"),
%%             S
%%     end;

%% %% Note that we do not currently handle inform:s, as that adds a whole
%% %% set of issues. See above for more info.
%% maybe_update_ephm_transport(S, #transport{socket    = Socket,
%%                                           ephm      = {alive_time, AliveTime},
%%                                           ephm_info = undefined} = Transport,
%%                             _Type, _Sz) ->
%%     AliveEnd = erlang:monotonic_time(micro_seconds) + AliveTime,
%%     T2 = Transport#transport{ephm_info = AliveEnd},
%%     TS  = S#state.transports,
%%     TS2 = lists:keyreplace(Socket, #transport.socket, TS, T2),
%%     S#state{transports = TS2};
%% maybe_update_ephm_transport(S, #transport{socket    = OldSocket,
%%                                           ephm      = {alive_time, _AliveTime},
%%                                           ephm_info = AliveEnd,
%%                                           port_info = PortInfo,
%%                                           opts      = Opts} = Transport,
%%                             _Type, _Sz) ->
%%     TS = erlang:monotonic_time(micro_seconds),
%%     if
%%         (TS > AliveEnd) ->
%%             try
%%                 begin
%%                     {Socket, PortNo} = gen_udp_open(PortInfo, Opts),
%%                     (catch gen_udp:close(OldSocket)),
%%                     T2 = Transport#transport{socket    = Socket,
%%                                              %% This will be set when the transport
%%                                              %% is first used
%%                                              ephm_info = undefined,
%%                                              port_no   = PortNo},
%%                     TS  = S#state.transports,
%%                     TS2 = lists:keyreplace(OldSocket, #transport.socket, TS, T2),
%%                     S#state{transports = TS2}
%%                 end
%%             catch
%%                 _:_:_ ->
%%                     %% We need to identify which transport!
%%                     error_msg("Failed creating new ephemeral socket for transport"),
%%                     S
%%             end;
%%         true ->
%%             S
%%     end;

%% maybe_update_ephm_transport(S, _Transport, _Type, _Sz) ->
%%     S.

%% </EPHEMERAL-FOR-FUTUR-USE>


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
		      [IpAddr, IpPort, sz(B), ErrorReason]),
            ok;
	ok ->
            %% For future use! Ephemeral ports!
	    {ok, byte_size(B)}
    catch
	error:ExitReason:StackTrace ->
	    error_msg("[exit] cannot send message "
		      "(destination: ~p:~p, size: ~p, reason: ~p, at: ~p)",
		      [IpAddr, IpPort, sz(B), ExitReason, StackTrace])
    end.

sz(L) when is_list(L) -> length(L);
sz(B) when is_binary(B) -> byte_size(B);
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
    ok = inet:setopts(Sock, [{active, once}]).


select_transport(_, []) ->
    false;
select_transport(ReqRef,
                 [#transport{req_refs = ReqRefs} = Transport | Transports]) ->
    case lists:member(ReqRef, ReqRefs) of
	true ->
	    Transport;
	false ->
	    select_transport(ReqRef, Transports)
    end.

select_transport(snmpUDPDomain = Domain, Type, Transports) ->
    ?vtrace("select_transport -> entry with"
            "~n      Domain:     ~p"
            "~n      Type:       ~p"
            "~n      Transports: ~p",
            [Domain, Type, Transports]),
    case select_transport2(Domain, Type, Transports) of
	#transport{} = Transport ->
            ?vtrace("select_transport -> selected: "
                    "~n      ~p",
                    [Transport]),
 	    Transport;
 	false ->
 	    select_transport2(transportDomainUdpIpv4, Type, Transports)
    end;
select_transport(Domain, Type, Transports)
  when is_atom(Domain) ->
    ?vtrace("select_transport -> entry with"
            "~n      Domain:     ~p"
            "~n      Type:       ~p"
            "~n      Transports: ~p",
            [Domain, Type, Transports]),
    case select_transport2(Domain, Type, Transports) of
 	#transport{} = Transport ->
            ?vtrace("select_transport -> selected: "
                    "~n      ~p",
                    [Transport]),
 	    Transport;
 	false when Domain =:= transportDomainUdpIpv4 ->
            ?vdebug("select_transport -> (~p) not found: "
                    "~n      try ~p", [Domain, snmpUDPDomain]),
	    select_transport2(snmpUDPDomain, Type, Transports);
 	false ->
 	    false
    end.


%% Two kinds of (pdu) type that we (the agent) will ever attempt to send:
%%   req-responder: 'get-response'
%%   trap-sender:   'inform-request'   |
%%                  'snmpv2-trap'      |
%%                  report             |
%%                  trap (which is a 'fake' type (#trappdu{}))
select_transport2(_Domain, _Type,
                  []) ->
    false;
select_transport2(snmpUDPDomain = Domain, _Type,
                  [#transport{domain = Domain} = Transport|_Transports]) ->
    Transport;
select_transport2(snmpUDPDomain = Domain, Type,
                  [_|Transports]) ->
    select_transport2(Domain, Type, Transports);
select_transport2(Domain, Type,
                  [#transport{domain = Domain,
                              kind   = Kind} = Transport|_Transports])
  when ((Type =:= 'inform-request') orelse
        (Type =:= 'snmpv2-trap') orelse
        (Type =:= report) orelse
        (Type =:= trap) orelse 
        (Type =:= trappdu)) andalso 
       ((Kind =:= all) orelse (Kind =:= trap_sender)) ->
    Transport;
select_transport2(Domain, Type,
                  [#transport{domain = Domain,
                              kind   = Kind} = Transport|_Transports])
  when (Type =:= 'get-response') andalso 
       ((Kind =:= all) orelse (Kind =:= req_responder)) ->
    Transport;
select_transport2(Domain, Type, [_|Transports]) ->
    select_transport2(Domain, Type, Transports).



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

system_terminate(Reason, _Parent, _Dbg, #state{log        = Log,
                                               transports = Transports}) ->
    ?vlog("system-terminate -> entry with"
	  "~n   Reason: ~p", [Reason]),
    %% Close all transports
    Close =
        fun(S) ->
                ?vlog("try close socket ~p", [S]),
                (catch gen_udp:close(S))
        end,
    _ = [Close(Socket) || #transport{socket = Socket} <- Transports],
    %% Close logs
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

%% This extracts the socket options from what is specified for
%% the transport, or if not, from the "global" socket options.
socket_opts(Domain, {IpAddr, PortInfo}, SocketOpts, DefaultOpts) ->
    ?vtrace("socket_opts -> entry with"
            "~n      Domain:      ~p"
            "~n      IpAddr:      ~p"
            "~n      PortInfo:    ~p"
            "~n      SocketOpts:  ~p"
            "~n      DefaultOpts: ~p",
            [Domain, IpAddr, PortInfo, SocketOpts, DefaultOpts]),
    {RequireBind, InetBackend} =
        case get_inet_backend(SocketOpts, DefaultOpts) of
            use_default ->
                {false, []};
            Backend when (Backend =:= inet) ->
                {false, [{inet_backend, Backend}]};
            Backend when (Backend =:= socket) ->
		{case os:type() of
		     {win32, nt} ->
			 true;
		     _ ->
			 false
		 end,
		 [{inet_backend, Backend}]}
        end,
    Opts =
        [binary |
         case snmp_conf:tdomain_to_family(Domain) of
             inet6 = Family ->
                 [Family, {ipv6_v6only, true}];
             Family ->
                 [Family]
         end ++
         case RequireBind orelse
	     get_bind_to_ip_address(SocketOpts, DefaultOpts) of
             true ->
                 [{ip, IpAddr}];
             _ ->
                 []
         end ++
         case get_no_reuse_address(SocketOpts, DefaultOpts) of
             false ->
                 [{reuseaddr, true}];
             _ ->
                 []
         end ++
         case get_recbuf(SocketOpts, DefaultOpts) of
             use_default ->
                 [];
             Sz ->
                 [{recbuf, Sz}]
         end ++
             case get_sndbuf(SocketOpts, DefaultOpts) of
                 use_default ->
                     [];
                 Sz ->
                     [{sndbuf, Sz}]
             end
        ] ++
        case get_extra_sock_opts(SocketOpts, DefaultOpts) of
            ESO when is_list(ESO) ->
                ESO;
            BadESO ->
                error_msg("Invalid 'extra socket options' (=> ignored):"
                          "~n   ~p", [BadESO]),
                []
        end,
    %% <EPHEMERAL-FOR-FUTUR-USE>
    %% Ephm = get_ephemeral(SocketOpts),
    %% {Ephm, PortInfo, Opts}.
    %% </EPHEMERAL-FOR-FUTUR-USE>
    {none, IpAddr, PortInfo, InetBackend, Opts}.


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

get_open_err_filters(O) ->
    case snmp_misc:get_option(open_err_filters, O, []) of
        Filters when is_list(Filters) ->
            Filters;
        Filter when is_atom(Filter) ->
            [Filter];
        _ ->
            []
    end.

get_recbuf(Opts, DefaultOpts) -> 
    get_socket_opt(recbuf, Opts, DefaultOpts, use_default).

get_sndbuf(Opts, DefaultOpts) -> 
    get_socket_opt(sndbuf, Opts, DefaultOpts, use_default).

get_bind_to_ip_address(Opts, DefaultOpts) ->
    get_socket_opt(bind_to, Opts, DefaultOpts, false).

get_no_reuse_address(Opts, DefaultOpts) -> 
    get_socket_opt(no_reuse, Opts, DefaultOpts, false).

get_extra_sock_opts(Opts, DefaultOpts) ->
    get_socket_opt(extra_sock_opts, Opts, DefaultOpts, []).

get_inet_backend(Opts, DefaultOpts) -> 
    get_socket_opt(inet_backend, Opts, DefaultOpts, use_default).

%% <EPHEMERAL-FOR-FUTUR-USE>
%% This is not really a socket option, but rather socket 'meta'
%% information. Its still put together with the actual socket
%% options.
%% get_ephemeral(SocketOpts) ->
%%     snmp_misc:get_option(ephemeral, SocketOpts, none).
%% </EPHEMERAL-FOR-FUTUR-USE>


get_socket_opt(Opt, Opts, DefaultOpts, DefaultVal) ->
    snmp_misc:get_option(Opt, Opts,
                         snmp_misc:get_option(Opt, DefaultOpts, DefaultVal)).
    


%% ----------------------------------------------------------------

%% error_msg(F) ->
%%     error_msg(F, []).

error_msg(F, A) -> 
    ?snmpa_error("NET-IF server: " ++ F, A).

warning_msg(F, A) -> 
    ?snmpa_warning("NET-IF server: " ++ F, A).

info_msg(F,A) ->
    ?snmpa_info("NET-IF server: " ++ F, A).

%% --- 

user_err(F, A) ->
    snmpa_error:user_err(F, A).
 
%% config_err(F, A) ->
%%     snmpa_error:config_err(F, A).
 

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
     {process_memory, ProcSize},
     {transport_info, [#{tdomain        => Domain,
                         taddress       => {Address, PortNo},
                         transport_kind => Kind,
                         port_info      => PortInfo,
                         opts           => Opts,
                         socket_info    => get_socket_info(Socket),
                         num_reqs       => length(Refs)} ||
                          #transport{socket    = Socket,
                                     domain    = Domain,
                                     address   = Address,
                                     port_no   = PortNo,
                                     port_info = PortInfo,
                                     opts      = Opts,
                                     kind      = Kind,
                                     req_refs  = Refs} <- Transports]}].

proc_mem(P) when is_pid(P) ->
    case (catch erlang:process_info(P, memory)) of
	{memory, Sz} when is_integer(Sz) ->
	    Sz;
	_ ->
	    undefined
    end.
%% proc_mem(_) ->
%%     undefined.

get_socket_info(Id) when is_port(Id) ->
    Info = inet:info(Id),
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
    [{socket, Id}, {info, Info}] ++ IfList ++ BufSz;
get_socket_info(Id) ->
    Info = inet:info(Id),

    %% Does not exist for 'socket'
    IfList = [],
	%% case (catch inet:getif(Id)) of
	%%     {ok, IFs} ->
	%% 	[{interfaces, IFs}];
	%%     _ ->
	%% 	[]
	%% end,

    BufSz = 
	case (catch inet:getopts(Id, [recbuf, sndbuf])) of
	    {ok, Sz} ->
		[{buffer_size, Sz}];
	    _ ->
		[]
	end,

    [{socket, Id}, {info, Info}] ++ IfList ++ BufSz.



%% ---------------------------------------------------------------
