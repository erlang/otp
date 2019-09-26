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

-ifndef(snmpm_net_if_mt).
-module(snmpm_net_if).
-endif.

-behaviour(gen_server).
-behaviour(snmpm_network_interface).


%% Network Interface callback functions
-export([
	 start_link/2, 
	 stop/1, 
	 send_pdu/6, % Backward compatibility
	 send_pdu/7, % Partly backward compatibility
	 send_pdu/8, % Backward compatibility

	 inform_response/4, 

	 note_store/2, 

	 info/1, 
 	 verbosity/2, 
 	 %% system_info_updated/2, 
 	 get_log_type/1, set_log_type/2,
	 filter_reset/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 code_change/3, terminate/2]).

-define(SNMP_USE_V3, true).
-include("snmp_types.hrl").
-include("snmpm_internal.hrl").
-include("snmpm_atl.hrl").
-include("snmp_debug.hrl").

%% -define(VMODULE,"NET_IF").
-include("snmp_verbosity.hrl").

-record(state,
	{
	  server,
	  note_store,
	  transports = [],
	  mpd_state,
	  log,
	  irb = auto, % auto | {user, integer()}
	  irgc,
	  filter
	 }).

-record(transport,
	{socket,
	 domain = snmpUDPDomain}).

-define(DEFAULT_FILTER_MODULE, snmpm_net_if_filter).
-define(DEFAULT_FILTER_OPTS,   [{module, ?DEFAULT_FILTER_MODULE}]).

-ifdef(snmp_debug).
-define(GS_START_LINK(Args),
	gen_server:start_link(?MODULE, Args, [{debug,[trace]}])).
-else.
-define(GS_START_LINK(Args),
	gen_server:start_link(?MODULE, Args, [])).
-endif.


-define(IRGC_TIMEOUT, timer:minutes(5)).

-define(ATL_SEQNO_INITIAL, 1).
-define(ATL_SEQNO_MAX,     2147483647).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link(Server, NoteStore) ->
    ?d("start_link -> entry with"
       "~n   Server:    ~p"
       "~n   NoteStore: ~p", [Server, NoteStore]),
    Args = [Server, NoteStore], 
    ?GS_START_LINK(Args).

stop(Pid) ->
    call(Pid, stop).

send_pdu(Pid, Pdu, Vsn, MsgData, Domain_or_Ip, Addr_or_Port) ->
    send_pdu(
      Pid, Pdu, Vsn, MsgData, Domain_or_Ip, Addr_or_Port, ?DEFAULT_EXTRA_INFO).

send_pdu(Pid, Pdu, Vsn, MsgData, Domain_or_Ip, Addr_or_Port, ExtraInfo)
  when is_record(Pdu, pdu) ->
    ?d("send_pdu -> entry with~n"
       "   Pid:       ~p~n"
       "   Pdu:       ~p~n"
       "   Vsn:       ~p~n"
       "   MsgData:   ~p~n"
       "   Domain/IP: ~p~n"
       "   Addr/Port: ~p",
       [Pid, Pdu, Vsn, MsgData, Domain_or_Ip, Addr_or_Port]),
    {Domain, Addr} = address(Domain_or_Ip, Addr_or_Port),
    cast(Pid, {send_pdu, Pdu, Vsn, MsgData, Domain, Addr, ExtraInfo}).

send_pdu(Pid, Pdu, Vsn, MsgData, Domain, Ip, Port, ExtraInfo) ->
    send_pdu(Pid, Pdu, Vsn, MsgData, Domain, {Ip, Port}, ExtraInfo).

note_store(Pid, NoteStore) ->
    call(Pid, {note_store, NoteStore}).

inform_response(Pid, Ref, Domain_or_Ip, Addr_or_Port) ->
    {Domain, Addr} = address(Domain_or_Ip, Addr_or_Port),
    cast(Pid, {inform_response, Ref, Domain, Addr}).

info(Pid) ->
    call(Pid, info).

verbosity(Pid, V) ->
    call(Pid, {verbosity, V}).

%% system_info_updated(Pid, What) ->
%%     call(Pid, {system_info_updated, What}).

get_log_type(Pid) ->
    call(Pid, get_log_type).

set_log_type(Pid, NewType) ->
    call(Pid, {set_log_type, NewType}).

filter_reset(Pid) ->
    cast(Pid, filter_reset).


%%%-------------------------------------------------------------------
%%% Multi-thread manager
%%%-------------------------------------------------------------------

-ifdef(snmpm_net_if_mt).

%% This function is called through the macro below to
%% (in the not multithreaded case) avoid creating the
%% Failer/4 fun, and to avoid calling the Worker through a fun
%% (now it shall not be a fun, just a code snippet).

worker(Worker, Failer, #state{log = Log} = State) ->
    Verbosity = get(verbosity),
    spawn_opt(
      fun () ->
	      try
		  put(sname, mnifw),
		  put(verbosity, Verbosity),
		  NewState =
		      case do_reopen_log(Log) of
			  Log ->
			      State;
			  NewLog ->
			      State#state{log = NewLog}
		      end,
		  Worker(NewState)
	      of
		  Result ->
		      %% Winds up in handle_info {'DOWN', ...}
		      erlang:exit({net_if_worker, Result})
	      catch
		  C:E:S ->
		      %% Winds up in handle_info {'DOWN', ...}
		      erlang:exit({net_if_worker, Failer, C, E, S})
	      end
      end,
      [monitor]).
-define(
   worker(S, Worker, Failer, State),
   begin
       worker(
	 fun (S) -> begin Worker end end,
	 begin Failer end,
	 (State))
   end).

-else.

-define(
   worker(S, Worker, _Failer, State),
   begin (S) = (State), begin Worker end end).

-endif.



%%%-------------------------------------------------------------------
%%% Callback functions from gen_server
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Server, NoteStore]) -> 
    ?d("init -> entry with"
       "~n   Server:    ~p"
       "~n   NoteStore: ~p", [Server, NoteStore]),
    try do_init(Server, NoteStore)
    catch
	{error, Reason} ->
	    {stop, Reason}
    end.

-ifdef(snmpm_net_if_mt).
%% This should really be protected, but it also needs to
%% be writable for the worker processes, so...
-define(inform_table_opts, [set, public, named_table, {keypos, 1}]).
-else.
-define(inform_table_opts, [set, protected, named_table, {keypos, 1}]).
-endif.
	    
do_init(Server, NoteStore) ->
    process_flag(trap_exit, true),

    %% -- Prio --
    {ok, Prio} = snmpm_config:system_info(prio),
    process_flag(priority, Prio),

    %% -- Create inform request table --
    ets:new(snmpm_inform_request_table, ?inform_table_opts),

    %% -- Verbosity -- 
    {ok, Verbosity} = snmpm_config:system_info(net_if_verbosity),
    put(sname, mnif),
    put(verbosity, Verbosity),
    ?vlog("starting", []),

    %% -- MPD --
    {ok, Vsns} = snmpm_config:system_info(versions),
    MpdState   = snmpm_mpd:init(Vsns),
    ?vdebug("MpdState: ~w", [MpdState]),

    %% -- Module dependent options --
    {ok, Opts} = snmpm_config:system_info(net_if_options),

    %% -- Inform response behaviour --
    {ok, IRB}  = snmpm_config:system_info(net_if_irb), 
    IrGcRef    = irgc_start(IRB), 

    %% Flow control --
    FilterOpts = get_opt(Opts, filter, []),
    FilterMod  = create_filter(FilterOpts),
    ?vdebug("FilterMod: ~w", [FilterMod]),

    %% -- Audit trail log ---
    {ok, ATL} = snmpm_config:system_info(audit_trail_log),
    Log = do_init_log(ATL),
    ?vdebug("Log: ~w", [Log]),

    {ok, DomainAddresses} = snmpm_config:system_info(transports),
    ?vdebug("DomainAddresses: ~w",[DomainAddresses]),
    CommonSocketOpts = common_socket_opts(Opts),
    BindTo = get_opt(Opts, bind_to,  false),
    case
	[begin
	     {IpPort, SocketOpts} =
		 socket_params(Domain, Address, BindTo, CommonSocketOpts),
	     Socket = socket_open(IpPort, SocketOpts),
	     #transport{socket = Socket, domain = Domain}
	 end || {Domain, Address} <- DomainAddresses]
    of
	[] ->
	    ?vinfo("No transports configured: ~p", [DomainAddresses]),
	    throw({error, {no_transports,DomainAddresses}});
	Transports ->
	    %% -- Initiate counters ---
	    init_counters(),

	    %% -- We are done ---
	    State = #state{
	      server     = Server,
	      note_store = NoteStore,
	      mpd_state  = MpdState,
	      transports = Transports,
	      log        = Log,
	      irb        = IRB,
	      irgc       = IrGcRef,
	      filter     = FilterMod},
	    ?vdebug("started", []),
	    {ok, State}
    end.

socket_open(IpPort, SocketOpts) ->
    ?vtrace("socket_open -> entry with~n"
	    "   IpPort:     ~p~n"
	    "   SocketOpts: ~p", [IpPort, SocketOpts]),
    case gen_udp:open(IpPort, SocketOpts) of
	{error, _} = Error ->
	    throw(Error);
	{ok, Socket} ->
	    Socket
    end.

socket_params(Domain, {IpAddr, IpPort} = Addr, BindTo, CommonSocketOpts) ->
    Family = snmp_conf:tdomain_to_family(Domain),
    SocketOpts =
	case Family of
	    inet6 ->
		[Family, {ipv6_v6only, true} | CommonSocketOpts];
	    Family ->
		[Family | CommonSocketOpts]
	end,
    case Family of
	inet ->
	    case init:get_argument(snmpm_fd) of
		{ok, [[FdStr]]} ->
		    Fd = list_to_integer(FdStr),
		    case BindTo of
			true ->
			    {IpPort, [{ip, IpAddr}, {fd, Fd} | SocketOpts]};
			_ ->
			    {0, [{fd, Fd} | SocketOpts]}
		    end;
		error ->
		    socket_params(SocketOpts, Addr, BindTo)
	    end;
	_ ->
	    socket_params(SocketOpts, Addr, BindTo)
    end.
%%
socket_params(SocketOpts, {IpAddr, IpPort}, BindTo) ->
    case BindTo of
	true ->
	    {IpPort, [{ip, IpAddr} | SocketOpts]};
	_ ->
	    {IpPort, SocketOpts}
    end.

common_socket_opts(Opts) ->
    [binary
     |   case get_opt(Opts, sndbuf, default) of
	     default ->
		 [];
	     Sz ->
		 [{sndbuf, Sz}]
	 end ++
	 case get_opt(Opts, recbuf, default) of
	     default ->
		 [];
	     Sz ->
		 [{recbuf, Sz}]
	 end ++
	 case get_opt(Opts, no_reuse, false) of
	     false ->
		 [{reuseaddr, true}];
	     _ ->
		 []
	 end ++
         case get_opt(Opts, extra_sock_opts, []) of
             ESO when is_list(ESO) ->
                 ESO;
             BadESO ->
                 error_msg("Invalid 'extra socket options' (=> ignored):"
                           "~n   ~p", [BadESO]),
                 []
         end].


create_filter(Opts) when is_list(Opts) ->
    case get_opt(Opts, module, ?DEFAULT_FILTER_MODULE) of
	?DEFAULT_FILTER_MODULE = Mod ->
	    Mod;
	Module ->
	    snmpm_network_interface_filter:verify(Module),
	    Module
    end;
create_filter(BadOpts) ->
    throw({error, {bad_filter_opts, BadOpts}}).


%% ----------------------------------------------------------------------
%%                         Audit Trail Logger
%% ----------------------------------------------------------------------

%% Open log
do_init_log(false) ->
    ?vtrace("do_init_log(false) -> entry", []),
    undefined;
do_init_log(true) ->
    ?vtrace("do_init_log(true) -> entry", []),
    {ok, Type}   = snmpm_config:system_info(audit_trail_log_type),
    {ok, Dir}    = snmpm_config:system_info(audit_trail_log_dir),
    {ok, Size}   = snmpm_config:system_info(audit_trail_log_size),
    {ok, Repair} = snmpm_config:system_info(audit_trail_log_repair),
    Name = ?audit_trail_log_name, 
    File = filename:absname(?audit_trail_log_file, Dir),
    case snmpm_config:system_info(audit_trail_log_seqno) of
	{ok, true} ->
	    Initial  = ?ATL_SEQNO_INITIAL,
	    Max      = ?ATL_SEQNO_MAX, 
	    Module   = snmpm_config, 
	    Function = increment_counter, 
	    Args     = [atl_seqno, Initial, Max], 
	    SeqNoGen = {Module, Function, Args}, 
	    case snmp_log:create(
		   Name, File, SeqNoGen, Size, Repair, true) of
		{ok, Log} ->
		    ?vdebug("log created: ~w", [Log]),
		    {Name, Log, Type};
		{error, Reason} ->
		    throw({error, {failed_create_audit_log, Reason}})
	    end;
	_ ->
	    case snmp_log:create(Name, File, Size, Repair, true) of
		{ok, Log} ->
		    ?vdebug("log created: ~w", [Log]),
		    {Name, Log, Type};
		{error, Reason} ->
		    throw({error, {failed_create_audit_log, Reason}})
	    end
    end.

-ifdef(snmpm_net_if_mt).
do_reopen_log(undefined) ->
    undefined;
do_reopen_log({Name, Log, Type}) ->
    case snmp_log:open(Name, Log) of
	{ok, NewLog} ->
	    {Name, NewLog, Type};
	{error, Reason} ->
	    warning_msg(
	      "NetIf worker ~p failed to open ATL:~n"
	      "   ~p", [self(), Reason]),
	    undefined
    end.
-endif.

%% Close log
do_close_log(undefined) ->
    ok;
do_close_log({_Name, Log, _Type}) ->
    (catch snmp_log:sync(Log)),
    (catch snmp_log:close(Log)),
    ok;
do_close_log(_) ->
    ok.

%% Log
logger(undefined, _Type, _Domain, _Addr) ->
    fun(_) ->
	    ok
    end;
logger({_Name, Log, Types}, Type, Domain, Addr) ->
    case lists:member(Type, Types) of
	true ->
	    AddrString =
		iolist_to_binary(snmp_conf:mk_addr_string({Domain, Addr})),
	    fun(Msg) ->
		    snmp_log:log(Log, Msg, AddrString)
	    end;
	false ->
	    fun(_) ->
		    ok
	    end
    end.


%%--------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({verbosity, Verbosity}, _From, State) ->
    ?vlog("received verbosity request", []),
    put(verbosity, Verbosity),
    {reply, ok, State};

handle_call(get_log_type, _From, State) ->
    ?vlog("received get-log-type request", []),
    Reply = (catch handle_get_log_type(State)),
    {reply, Reply, State};

handle_call({set_log_type, NewType}, _From, State) ->
    ?vlog("received set-log-type request with NewType = ~p", [NewType]),
    {NewState, Reply} = (catch handle_set_log_type(State, NewType)),
    {reply, Reply, NewState};

handle_call({note_store, Pid}, _From, State) ->
    ?vlog("received new note_store: ~w", [Pid]),
    {reply, ok, State#state{note_store = Pid}};

handle_call(stop, _From, State) ->
    ?vlog("received stop request", []),
    Reply = ok,
    {stop, normal, Reply, State};

handle_call(info, _From, State) ->
    ?vlog("received info request", []),
    Reply = get_info(State),
    {reply, Reply, State};

handle_call(Req, From, State) ->
    warning_msg("received unknown request (from ~p): ~n~p", [Req, From]),
    {reply, {error, {invalid_request, Req}}, State}.


%%--------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({send_pdu, Pdu, Vsn, MsgData, Domain, Addr, ExtraInfo},
	    State) ->
    ?vlog("received send_pdu message with~n"
	  "   Pdu:     ~p~n"
	  "   Vsn:     ~p~n"
	  "   MsgData: ~p~n"
	  "   Domain:  ~p~n"
	  "   Addr:    ~p", [Pdu, Vsn, MsgData, Domain, Addr]),
    maybe_process_extra_info(ExtraInfo),
    maybe_handle_send_pdu(Pdu, Vsn, MsgData, Domain, Addr, State),
    {noreply, State};

handle_cast({inform_response, Ref, Domain, Addr}, State) ->
    ?vlog("received inform_response message with~n"
	  "   Ref:    ~p~n"
	  "   Domain: ~p~n"
	  "   Addr:   ~p", [Ref, Domain, Addr]),
    handle_inform_response(Ref, Domain, Addr, State),
    {noreply, State};

handle_cast(filter_reset, State) ->
    ?vlog("received filter_reset message", []),
    reset_counters(),
    {noreply, State};

handle_cast(Msg, State) ->
    warning_msg("received unknown message: ~n~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(
  {udp, Socket, IpAddr, IpPort, Bytes},
  #state{transports = Transports} = State) ->
    Size = byte_size(Bytes),
    case lists:keyfind(Socket, #transport.socket, Transports) of
	#transport{socket = Socket, domain = Domain} ->
	    ?vlog("received ~w bytes from ~p:~p [~w]",
		  [Size, IpAddr, IpPort, Socket]),
	    maybe_handle_recv_msg(Domain, {IpAddr, IpPort}, Bytes, State),
	    {noreply, State};
	false ->
	    warning_msg("Received ~w bytes on unknown port: ~p from ~s",
			[Size, Socket, format_address({IpAddr, IpPort})]),
	    {noreply, State}
    end;

handle_info(inform_response_gc, State) ->
    ?vlog("received inform_response_gc message", []),
    State2 = handle_inform_response_gc(State),
    {noreply, State2};

handle_info({disk_log, _Node, Log, Info}, State) ->
    ?vlog("received disk_log message: "
	  "~n   Info: ~p", [Info]),
    State2 = handle_disk_log(Log, Info, State),
    {noreply, State2};

handle_info({'DOWN', _, _, _, _} = Info, State) ->
    handle_info_down(Info, State);

handle_info(Info, State) ->
    handle_info_unknown(Info, State).


handle_info_unknown(Info, State) ->
    warning_msg("received unknown info: ~n~p", [Info]),
    {noreply, State}.


-ifdef(snmpm_net_if_mt).
handle_info_down(
  {'DOWN', _MRef, process, _Pid,
   {net_if_worker, _Result}},
  State) ->
    ?vdebug("received DOWN message from net_if worker [~w]: "
	    "~n   Result: ~p", [_Pid, _Result]),
    {noreply, State};
handle_info_down(
  {'DOWN', _MRef, process, Pid,
   {net_if_worker, Failer, Class, Reason, Stacktrace} = _ExitStatus},
  State) ->
    ?vdebug("received DOWN message from net_if worker [~w]: "
	    "~n   ExitStatus: ~p", [Pid, _ExitStatus]),
    Failer(Pid, Class, Reason, Stacktrace),
    {noreply, State};
handle_info_down(Info, State) ->
    handle_info_unknown(Info, State).
-else.
handle_info_down(Info, State) ->
    handle_info_unknown(Info, State).
-endif.


%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, #state{log = Log, irgc = IrGcRef}) ->
    ?vdebug("terminate: ~p", [Reason]),
    irgc_stop(IrGcRef),
    %% Close logs
    do_close_log(Log),
    ok.


%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
 
code_change(_Vsn, State, _Extra) ->
    ?d("code_change -> entry with"
       "~n   Vsn:   ~p"
       "~n   State: ~p"
       "~n   Extra: ~p", [_Vsn, State, _Extra]),
    {ok, State}.

 
%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

maybe_handle_recv_msg(Domain, Addr, Bytes, State) ->
    ?worker(
       S, maybe_handle_recv_msg_mt(Domain, Addr, Bytes, S),
       fun (Pid, Class, Reason, Stacktrace) ->
	       warning_msg(
		 "Worker process (~p) terminated "
		 "while processing (incomming) message from %s:~n"
		 "~w:~w at ~p",
		 [Pid, snmp_conf:mk_addr_string({Domain, Addr}),
		  Class, Reason, Stacktrace])
       end,
       State).

maybe_handle_recv_msg_mt(
  Domain, Addr, Bytes,
  #state{filter = FilterMod, transports = Transports} = State) ->
    {Arg1, Arg2} = fix_filter_address(Transports, {Domain, Addr}),
    case (catch FilterMod:accept_recv(Arg1, Arg2)) of
	false ->
	    %% Drop the received packet 
	    inc(netIfMsgInDrops);
	_ ->
	    handle_recv_msg(Domain, Addr, Bytes, State)
    end,
    ok.
    

handle_recv_msg(Domain, Addr, Bytes, #state{server = Pid})
  when is_binary(Bytes) andalso (size(Bytes) =:= 0) ->
    Pid ! {snmp_error, {empty_message, Domain, Addr}, Domain, Addr};
%%
handle_recv_msg(
  Domain, Addr, Bytes,
  #state{
	  server     = Pid,
	  note_store = NoteStore,
	  mpd_state  = MpdState,
	  log        = Log} = State) ->
    Logger = logger(Log, read, Domain, Addr),
    case (catch snmpm_mpd:process_msg(
		  Bytes, Domain, Addr, MpdState, NoteStore, Logger)) of

	{ok, Vsn, Pdu, MS, ACM} ->
	    maybe_handle_recv_pdu(
	      Domain, Addr, Vsn, Pdu, MS, ACM, Logger, State);

	{discarded, Reason, Report} ->
	    ?vdebug("discarded: ~p", [Reason]),
	    ErrorInfo = {failed_processing_message, Reason},
	    Pid ! {snmp_error, ErrorInfo, Domain, Addr},
	    maybe_udp_send(Domain, Addr, Report, State);

	{discarded, Reason} ->
	    ?vdebug("discarded: ~p", [Reason]),
	    ErrorInfo = {failed_processing_message, Reason},
	    Pid ! {snmp_error, ErrorInfo, Domain, Addr};

	Error ->
	    error_msg("processing of received message failed: "
		      "~n   ~p", [Error])
    end.


maybe_handle_recv_pdu(
  Domain, Addr, Vsn, #pdu{type = Type} = Pdu, PduMS, ACM, Logger,
  #state{filter = FilterMod, transports = Transports} = State) ->
    {Arg1, Arg2} = fix_filter_address(Transports, {Domain, Addr}),
    case (catch FilterMod:accept_recv_pdu(Arg1, Arg2, Type)) of
	false ->
	    inc(netIfPduInDrops);
	_ ->
	    handle_recv_pdu(
	      Domain, Addr, Vsn, Pdu, PduMS, ACM, Logger, State)
    end;
maybe_handle_recv_pdu(
  Domain, Addr, Vsn, Trap, PduMS, ACM, Logger,
  #state{filter = FilterMod, transports = Transports} = State)
  when is_record(Trap, trappdu) ->
    {Arg1, Arg2} = fix_filter_address(Transports, {Domain, Addr}),
    case (catch FilterMod:accept_recv_pdu(Arg1, Arg2, trappdu)) of
	false ->
	    inc(netIfPduInDrops);
	_ ->
	    handle_recv_pdu(
	      Domain, Addr, Vsn, Trap, PduMS, ACM, Logger, State)
    end;
maybe_handle_recv_pdu(
  Domain, Addr, Vsn, Pdu, PduMS, ACM, Logger, State) ->
    handle_recv_pdu(Domain, Addr, Vsn, Pdu, PduMS, ACM, Logger, State).


handle_recv_pdu(
  Domain, Addr, Vsn,
  #pdu{type = 'inform-request'} = Pdu, _PduMS, ACM, Logger,
  #state{server = Pid, irb = IRB} = State) ->
    handle_inform_request(
      IRB, Pid, Vsn, Pdu, ACM, Domain, Addr, Logger, State);
handle_recv_pdu(
  Domain, Addr, _Vsn,
  #pdu{type = report} = Pdu, _PduMS, ok, _Logger,
  #state{server = Pid} = _State) ->
    ?vtrace("received report - ok", []),
    Pid ! {snmp_report, {ok, Pdu}, Domain, Addr};
handle_recv_pdu(
  Domain, Addr, _Vsn,
  #pdu{type = report} = Pdu, _PduMS, {error, ReqId, Reason}, _Logger,
  #state{server = Pid} = _State) ->
    ?vtrace("received report - error", []),
    Pid ! {snmp_report, {error, ReqId, Reason, Pdu}, Domain, Addr};
handle_recv_pdu(
  Domain, Addr, _Vsn,
  #pdu{type = 'snmpv2-trap'} = Pdu, _PduMS, _ACM, _Logger,
  #state{server = Pid} = _State) ->
    ?vtrace("received snmpv2-trap", []),
    Pid ! {snmp_trap, Pdu, Domain, Addr};
handle_recv_pdu(
  Domain, Addr, _Vsn, Trap, _PduMS, _ACM, _Logger,
  #state{server = Pid} = _State) when is_record(Trap, trappdu) ->
    ?vtrace("received trappdu", []),
    Pid ! {snmp_trap, Trap, Domain, Addr};
handle_recv_pdu(
  Domain, Addr, _Vsn, Pdu, _PduMS, _ACM, _Logger,
  #state{server = Pid} = _State) when is_record(Pdu, pdu) ->
    ?vtrace("received pdu", []),
    Pid ! {snmp_pdu, Pdu, Domain, Addr};
handle_recv_pdu(
  _Domain, _Addr, _Vsn, Pdu, _PduMS, ACM, _Logger, _State) ->
    ?vlog("received unexpected pdu: "
	  "~n   Pdu: ~p"
	  "~n   ACM: ~p", [Pdu, ACM]).


handle_inform_request(
  auto, Pid, Vsn, Pdu, ACM, Domain, Addr, Logger, State) ->
    ?vtrace("received inform-request (true)", []),
    Pid ! {snmp_inform, ignore, Pdu, Domain, Addr},
    RePdu = make_response_pdu(Pdu),
    maybe_send_inform_response(RePdu, Vsn, ACM, Domain, Addr, Logger, State);
handle_inform_request(
  {user, To}, Pid, Vsn, #pdu{request_id = ReqId} = Pdu, 
  ACM, Domain, Addr, _Logger, _State) ->
    ?vtrace("received inform-request (false)", []),

    Pid ! {snmp_inform, ReqId, Pdu, Domain, Addr},

    %% Before we go any further, we need to check that we have not
    %% already received this message (possible resend).

    Key = {ReqId, Domain, Addr},
    case ets:lookup(snmpm_inform_request_table, Key) of
	[_] ->
	    %% OK, we already know about this.  We assume this
	    %% is a resend. Either the agent is really eager or
	    %% the user has not answered yet. Bad user!
	    ok;
	[] ->
	    RePdu  = make_response_pdu(Pdu),
	    Expire = snmp_misc:now(ms) + To,
	    Rec    = {Key, Expire, {Vsn, ACM, RePdu}},
	    ets:insert(snmpm_inform_request_table, Rec)
    end.

handle_inform_response(Ref, Domain, Addr, State) ->
    ?worker(
       S, handle_inform_response_mt(Ref, Domain, Addr, S),
       fun (Pid, Class, Reason, Stacktrace) ->
	       warning_msg(
		 "Worker process (~p) terminated "
		 "while processing (outgoing) inform response for %s:~n"
		 "~w:~w at ~p",
		 [Pid, snmp_conf:mk_addr_string({Domain, Addr}),
		  Class, Reason, Stacktrace])
       end,
       State).

handle_inform_response_mt(Ref, Domain, Addr, State) ->
    Key = {Ref, Domain, Addr},
    case ets:lookup(snmpm_inform_request_table, Key) of
	[{Key, _, {Vsn, ACM, RePdu}}] ->
	    Logger = logger(State#state.log, read, Domain, Addr),
	    ets:delete(snmpm_inform_request_table, Key), 
	    maybe_send_inform_response(
	      RePdu, Vsn, ACM, Domain, Addr, Logger, State);
	[] ->
	    %% Already acknowledged, or the user was to slow to reply...
	    ok
    end,
    ok.

maybe_send_inform_response(
  RePdu, Vsn, ACM, Domain, Addr, Logger,
  #state{
	  server = Pid,
	  filter = FilterMod,
	  transports = Transports} = State) ->
    {Arg1, Arg2} = fix_filter_address(Transports, {Domain, Addr}),
    case (catch FilterMod:accept_send_pdu(
		  Arg1, Arg2, pdu_type_of(RePdu)))
    of
	false ->
	    inc(netIfPduOutDrops),
	    ok;
	_ ->
	    case snmpm_mpd:generate_response_msg(Vsn, RePdu, ACM, Logger) of
		{ok, Msg} ->
		    maybe_udp_send(Domain, Addr, Msg, State);
		{discarded, Reason} ->
		    ?vlog("failed generating response message:"
			  "~n   Reason: ~p", [Reason]),
		    ReqId     = RePdu#pdu.request_id,
		    ErrorInfo = {failed_generating_response, {RePdu, Reason}},
		    Pid ! {snmp_error, ReqId, ErrorInfo, Domain, Addr}
	    end
    end.
    
handle_inform_response_gc(#state{irb = IRB} = State) ->
    ets:safe_fixtable(snmpm_inform_request_table, true),
    do_irgc(ets:first(snmpm_inform_request_table), snmp_misc:now(ms)),
    ets:safe_fixtable(snmpm_inform_request_table, false),
    State#state{irgc = irgc_start(IRB)}.

%% We are deleting at the same time as we are traversing the table!!!
do_irgc('$end_of_table', _) ->
    ok;
do_irgc(Key, Now) ->
    Next = ets:next(snmpm_inform_request_table, Key),
    case ets:lookup(snmpm_inform_request_table, Key) of
        [{Key, BestBefore, _}] when BestBefore < Now ->
            ets:delete(snmpm_inform_request_table, Key);
        _ ->
            ok
    end,
    do_irgc(Next, Now).

irgc_start(auto) ->
    undefined;
irgc_start(_) ->
    erlang:send_after(?IRGC_TIMEOUT, self(), inform_response_gc).

irgc_stop(undefined) ->
    ok;
irgc_stop(Ref) ->
    (catch erlang:cancel_timer(Ref)).

maybe_handle_send_pdu(Pdu, Vsn, MsgData, Domain, Addr, State) ->
    ?worker(
       S, maybe_handle_send_pdu_mt(Pdu, Vsn, MsgData, Domain, Addr, S),
       fun (Pid, Class, Reason, Stacktrace) ->
	       warning_msg(
		 "Worker process (~p) terminated "
		 "while processing (outgoing) pdu for %s:~n"
		 "~w:~w at ~p",
		 [Pid, snmp_conf:mk_addr_string({Domain, Addr}),
		  Class, Reason, Stacktrace])
       end,
       State).

maybe_handle_send_pdu_mt(
  Pdu, Vsn, MsgData, Domain, Addr,
  #state{filter = FilterMod, transports = Transports} = State) ->
    {Arg1, Arg2} = fix_filter_address(Transports, {Domain, Addr}),
    case (catch FilterMod:accept_send_pdu(Arg1, Arg2, pdu_type_of(Pdu))) of
	false ->
	    inc(netIfPduOutDrops);
	_ ->
	    handle_send_pdu(Pdu, Vsn, MsgData, Domain, Addr, State)
    end,
    ok.

handle_send_pdu(
  Pdu, Vsn, MsgData, Domain, Addr,
  #state{
	  server     = Pid,
	  note_store = NoteStore,
	  log        = Log} = State) ->
    Logger = logger(Log, write, Domain, Addr),
    case (catch snmpm_mpd:generate_msg(
		  Vsn, NoteStore, Pdu, MsgData, Logger)) of
	{ok, Msg} ->
	    ?vtrace("handle_send_pdu -> message generated", []),
	    maybe_udp_send(Domain, Addr, Msg, State);
	{discarded, Reason} ->
	    ?vlog("PDU not sent: "
		  "~n   PDU:    ~p"
		  "~n   Reason: ~p", [Pdu, Reason]),
	    Pid ! {snmp_error, Pdu, Reason}
    end.


maybe_udp_send(
  Domain, Addr, Msg,
  #state{filter = FilterMod, transports = Transports}) ->
    To = {Domain, Addr},
    {Arg1, Arg2} = fix_filter_address(Transports, To),
    case (catch FilterMod:accept_send(Arg1, Arg2)) of
	false ->
	    inc(netIfMsgOutDrops),
	    ok;
	_ ->
	    case select_transport_from_domain(Domain, Transports) of
		false ->
		    error_msg(
		      "Can not find transport~n"
			"   size:   ~p~n"
		      "   to:     ~s",
		      [sz(Msg), format_address(To)]);
		#transport{socket = Socket} ->
		    udp_send(Socket, Addr, Msg)
	    end
    end.

udp_send(Sock, To, Msg) ->
    {IpAddr, IpPort} =
	case To of
	    {Domain, Addr} when is_atom(Domain) ->
		Addr;
	    {_, P} = Addr when is_integer(P) ->
		Addr
	end,
    try gen_udp:send(Sock, IpAddr, IpPort, Msg) of
	ok ->
	    ?vdebug("sent ~w bytes to ~w:~w [~w]", 
		    [sz(Msg), IpAddr, IpPort, Sock]),
	    ok;
	{error, Reason} ->
	    error_msg("failed sending message to ~p:~p:~n"
		      "   ~p",[IpAddr, IpPort, Reason])
    catch
	error:E:S ->
	    error_msg("failed sending message to ~p:~p:"
		      "~n   ~p"
		      "~n   ~p", [IpAddr, IpPort, E, S])
    end.

sz(B) when is_binary(B) ->
    byte_size(B);
sz(L) when is_list(L) ->
    length(L);
sz(_) ->
    undefined.


handle_disk_log(_Log, {wrap, NoLostItems}, State) ->
    ?vlog("Audit Trail Log - wrapped: ~w previously logged items where lost", 
	  [NoLostItems]),
    State;
handle_disk_log(_Log, {truncated, NoLostItems}, State) ->
    ?vlog("Audit Trail Log - truncated: ~w items where lost when truncating", 
	  [NoLostItems]),
    State;
handle_disk_log(_Log, full, State) ->
    error_msg("Failed to write to Audit Trail Log (full)", []),
    State;
handle_disk_log(_Log, {error_status, ok}, State) ->
    State;
handle_disk_log(_Log, {error_status, {error, Reason}}, State) ->
    error_msg("Error status received from Audit Trail Log: "
	      "~n~p", [Reason]),
    State;
handle_disk_log(_Log, _Info, State) ->
    State.


handle_get_log_type(#state{log = {_Log, Value}} = State) ->
    %% Just to make sure, check that ATL is actually enabled
    case snmpm_config:system_info(audit_trail_log) of
	{ok, true} ->
	    Type = 
		case {lists:member(read, Value), lists:member(write, Value)} of
		    {true, true} ->
			read_write;
		    {true, false} ->
			read;
		    {false, true} ->
			write;
		    {false, false} ->
			throw({State, {error, {bad_atl_type, Value}}})
		end,
	    {ok, Type};
	_ ->
	    {error, not_enabled}
    end;
handle_get_log_type(_State) ->
    {error, not_enabled}.
    
handle_set_log_type(#state{log = {Log, OldValue}} = State, NewType) ->
    %% Just to make sure, check that ATL is actually enabled
    case snmpm_config:system_info(audit_trail_log) of
	{ok, true} ->
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
	_ ->
	    {State, {error, not_enabled}}
    end;
handle_set_log_type(State, _NewType) ->
    {State, {error, not_enabled}}.


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

%% If the manager uses legacy snmpUDPDomain e.g has not set
%% {domain, _}, then make sure snmpm_network_interface_filter
%% gets legacy arguments to not break backwards compatibility.
%%
fix_filter_address(Transports, Address) ->
    DefaultDomain = snmpm_config:default_transport_domain(),
    case Transports of
	[#transport{domain = DefaultDomain}, DefaultDomain] ->
	    case Address of
		{Domain, Addr} when is_atom(Domain) ->
		    Addr;
		{_, IpPort} = Addr when is_integer(IpPort) ->
		    Addr
	    end;
	_ ->
	    Address
    end.

address(Domain, Addr) when is_atom(Domain) ->
    {Domain, Addr};
address(Ip, Port) when is_integer(Port) ->
    {snmpm_config:default_transport_domain(), {Ip, Port}}.

format_address(Address) ->
    iolist_to_binary(snmp_conf:mk_addr_string(Address)).

%% -------------------------------------------------------------------

make_response_pdu(#pdu{request_id = ReqId, varbinds = Vbs}) ->
    #pdu{type         = 'get-response', 
	 request_id   = ReqId, 
	 error_status = noError,
	 error_index  = 0, 
	 varbinds     = Vbs}.


%% ----------------------------------------------------------------

pdu_type_of(#pdu{type = Type}) ->
    Type;
pdu_type_of(TrapPdu) when is_record(TrapPdu, trappdu) ->
    trap.


%% -------------------------------------------------------------------

%% At this point this function is used during testing
maybe_process_extra_info(?DEFAULT_EXTRA_INFO) ->
    ok;
maybe_process_extra_info({?SNMPM_EXTRA_INFO_TAG, Fun}) 
  when is_function(Fun, 0) ->
    (catch Fun()),
    ok;
maybe_process_extra_info(_ExtraInfo) ->
    ok.
    

%% -------------------------------------------------------------------

%% info_msg(F, A) ->
%%     ?snmpm_info("NET-IF server: " ++ F, A).

warning_msg(F, A) ->
    ?snmpm_warning("NET-IF server: " ++ F, A).

error_msg(F, A) ->
    ?snmpm_error("NET-IF server: " ++ F, A).



%%%-------------------------------------------------------------------

% get_opt(Key, Opts) ->
%     ?vtrace("get option ~w", [Key]),
%     snmp_misc:get_option(Key, Opts).

get_opt(Opts, Key, Def) ->
    ?vtrace("get option ~w with default ~p", [Key, Def]),
    snmp_misc:get_option(Key, Opts, Def).


%% -------------------------------------------------------------------

get_info(#state{transports = Transports}) ->
    ProcSize = proc_mem(self()),
    [{process_memory, ProcSize}
     | [{port_info, get_port_info(Socket)}
	|| #transport{socket = Socket} <- Transports]].

proc_mem(P) when is_pid(P) ->
    case (catch erlang:process_info(P, memory)) of
	{memory, Sz} when is_integer(Sz) ->
	    Sz;
	_ ->
	    undefined
    end.


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
	case (catch inet:getopts(Id, [recbuf, sndbuf, buffer])) of
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


%%-----------------------------------------------------------------
%% Counter functions
%%-----------------------------------------------------------------
init_counters() ->
    F = fun(Counter) -> maybe_create_counter(Counter) end,
    lists:map(F, counters()).

reset_counters() ->
    F = fun(Counter) -> snmpm_config:reset_stats_counter(Counter) end,
    lists:map(F, counters()).

maybe_create_counter(Counter) ->
    snmpm_config:maybe_cre_stats_counter(Counter, 0).

counters() ->
    [
     netIfMsgOutDrops,
     netIfMsgInDrops,
     netIfPduOutDrops,
     netIfPduInDrops
    ].

inc(Name)    -> inc(Name, 1).
inc(Name, N) -> snmpm_config:incr_stats_counter(Name, N).


%% ----------------------------------------------------------------

call(Pid, Req) ->
    call(Pid, Req, infinity).

call(Pid, Req, Timeout) ->
    gen_server:call(Pid, Req, Timeout).

cast(Pid, Msg) ->
    gen_server:cast(Pid, Msg).
