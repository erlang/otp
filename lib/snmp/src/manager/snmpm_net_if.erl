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

-module(snmpm_net_if).

-behaviour(gen_server).
-behaviour(snmpm_network_interface).


%% Network Interface callback functions
-export([
	 start_link/2, 
	 stop/1, 
	 send_pdu/6, % Backward compatibillity
	 send_pdu/7,

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
	  sock, 
	  mpd_state,
	  log,
	  irb = auto, % auto | {user, integer()}
	  irgc,
	  filter
	 }).


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

send_pdu(Pid, Pdu, Vsn, MsgData, Addr, Port) ->
    send_pdu(Pid, Pdu, Vsn, MsgData, Addr, Port, undefined).

send_pdu(Pid, Pdu, Vsn, MsgData, Addr, Port, ExtraInfo) 
  when is_record(Pdu, pdu) ->
    ?d("send_pdu -> entry with"
       "~n   Pid:     ~p"
       "~n   Pdu:     ~p"
       "~n   Vsn:     ~p"
       "~n   MsgData: ~p"
       "~n   Addr:    ~p"
       "~n   Port:    ~p", [Pid, Pdu, Vsn, MsgData, Addr, Port]),
    cast(Pid, {send_pdu, Pdu, Vsn, MsgData, Addr, Port, ExtraInfo}).

note_store(Pid, NoteStore) ->
    call(Pid, {note_store, NoteStore}).

inform_response(Pid, Ref, Addr, Port) ->
    cast(Pid, {inform_response, Ref, Addr, Port}).

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
    case (catch do_init(Server, NoteStore)) of
	{error, Reason} ->
	    {stop, Reason};
	{ok, State} ->
	    {ok, State}
    end.
	    
do_init(Server, NoteStore) ->
    process_flag(trap_exit, true),

    %% -- Prio --
    {ok, Prio} = snmpm_config:system_info(prio),
    process_flag(priority, Prio),

    %% -- Create inform request table --
    ets:new(snmpm_inform_request_table,
	    [set, protected, named_table, {keypos, 1}]),

    %% -- Verbosity -- 
    {ok, Verbosity} = snmpm_config:system_info(net_if_verbosity),
    put(sname,mnif),
    put(verbosity,Verbosity),
    ?vlog("starting", []),

    %% -- MPD --
    {ok, Vsns} = snmpm_config:system_info(versions),
    MpdState   = snmpm_mpd:init(Vsns),

    %% -- Module dependent options --
    {ok, Opts} = snmpm_config:system_info(net_if_options),

    %% -- Inform response behaviour --
    {ok, IRB}  = snmpm_config:system_info(net_if_irb), 
    IrGcRef    = irgc_start(IRB), 

    %% -- Socket --
    SndBuf  = get_opt(Opts, sndbuf,   default),
    RecBuf  = get_opt(Opts, recbuf,   default),
    BindTo  = get_opt(Opts, bind_to,  false),
    NoReuse = get_opt(Opts, no_reuse, false),
    {ok, Port} = snmpm_config:system_info(port),
    {ok, Sock} = do_open_port(Port, SndBuf, RecBuf, BindTo, NoReuse),

    %% Flow control --
    FilterOpts = get_opt(Opts, filter, []),
    FilterMod  = create_filter(FilterOpts),
    ?vdebug("FilterMod: ~w", [FilterMod]),

    %% -- Audit trail log ---
    {ok, ATL} = snmpm_config:system_info(audit_trail_log),
    Log = do_init_log(ATL),

    %% -- Initiate counters ---
    init_counters(),
    
    %% -- We are done ---
    State = #state{server     = Server, 
		   note_store = NoteStore, 
		   mpd_state  = MpdState,
		   sock       = Sock, 
		   log        = Log,
		   irb        = IRB,
		   irgc       = IrGcRef,
		   filter     = FilterMod},
    ?vdebug("started", []),
    {ok, State}.


%% Open port 
do_open_port(Port, SendSz, RecvSz, BindTo, NoReuse) ->
    ?vtrace("do_open_port -> entry with"
	    "~n   Port:    ~p"
	    "~n   SendSz:  ~p"
	    "~n   RecvSz:  ~p"
	    "~n   BindTo:  ~p"
	    "~n   NoReuse: ~p", [Port, SendSz, RecvSz, BindTo, NoReuse]),
    IpOpts1 = bind_to(BindTo),
    IpOpts2 = no_reuse(NoReuse),
    IpOpts3 = recbuf(RecvSz),
    IpOpts4 = sndbuf(SendSz),
    IpOpts  = [binary | IpOpts1 ++ IpOpts2 ++ IpOpts3 ++ IpOpts4],
    OpenRes = 
	case init:get_argument(snmpm_fd) of
	    {ok, [[FdStr]]} ->
		Fd = list_to_integer(FdStr),
		gen_udp:open(0, [{fd, Fd}|IpOpts]);
	    error ->
		gen_udp:open(Port, IpOpts)
	end,
    case OpenRes of
	{error, _} = Error ->
	    throw(Error);
	OK ->
	    OK
    end.

bind_to(true) ->
    case snmpm_config:system_info(address) of
	{ok, Addr} when is_list(Addr) ->
	    [{ip, list_to_tuple(Addr)}];
	{ok, Addr} ->
	    [{ip, Addr}];
	_ ->
	    []
    end;
bind_to(_) ->
    [].

no_reuse(false) ->
    [{reuseaddr, true}];
no_reuse(_) ->
    [].

recbuf(default) ->
    [];
recbuf(Sz) ->
    [{recbuf, Sz}].

sndbuf(default) ->
    [];
sndbuf(Sz) ->
    [{sndbuf, Sz}].


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
	    case snmp_log:create(Name, File, 
				 SeqNoGen, Size, Repair, true) of
		{ok, Log} ->
		    ?vdebug("log created: ~w", [Log]),
		    {Log, Type};
		{error, Reason} ->
		    throw({error, {failed_create_audit_log, Reason}})
	    end;
	_ ->
	    case snmp_log:create(Name, File, Size, Repair, true) of
		{ok, Log} ->
		    {Log, Type};
		{error, Reason} ->
		    throw({error, {failed_create_audit_log, Reason}})
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

%% handle_call({system_info_updated, What}, _From, State) ->
%%     ?vlog("received system_info_updated request with What = ~p", [What]),
%%     {NewState, Reply} = handle_system_info_updated(State, What),
%%     {reply, Reply, NewState};

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
handle_cast({send_pdu, Pdu, Vsn, MsgData, Addr, Port, _ExtraInfo}, State) ->
    ?vlog("received send_pdu message with"
	  "~n   Pdu:     ~p"
	  "~n   Vsn:     ~p"
	  "~n   MsgData: ~p"
	  "~n   Addr:    ~p"
	  "~n   Port:    ~p", [Pdu, Vsn, MsgData, Addr, Port]),
    maybe_handle_send_pdu(Pdu, Vsn, MsgData, Addr, Port, State), 
    {noreply, State};

handle_cast({inform_response, Ref, Addr, Port}, State) ->
    ?vlog("received inform_response message with"
	  "~n   Ref:  ~p"
	  "~n   Addr: ~p"
	  "~n   Port: ~p", [Ref, Addr, Port]),
    handle_inform_response(Ref, Addr, Port, State), 
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
handle_info({udp, Sock, Ip, Port, Bytes}, #state{sock = Sock} = State) ->
    ?vlog("received ~w bytes from ~p:~p [~w]", [size(Bytes), Ip, Port, Sock]),
    maybe_handle_recv_msg(Ip, Port, Bytes, State),
    {noreply, State};

handle_info(inform_response_gc, State) ->
    ?vlog("received inform_response_gc message", []),
    State2 = handle_inform_response_gc(State),
    {noreply, State2};

handle_info({disk_log, _Node, Log, Info}, State) ->
    ?vlog("received disk_log message: "
	  "~n   Info: ~p", [Info]),
    State2 = handle_disk_log(Log, Info, State),
    {noreply, State2};

handle_info(Info, State) ->
    warning_msg("received unknown info: ~n~p", [Info]),
    {noreply, State}.


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


do_close_log({Log, _Type}) ->
    (catch snmp_log:sync(Log)),
    (catch snmp_log:close(Log)),
    ok;
do_close_log(_) ->
    ok.


%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
 
code_change({down, _Vsn}, OldState, downgrade_to_pre_4_14) ->
    ?d("code_change(down, downgrade_to_pre_4_14) -> entry with"
       "~n   OldState: ~p", [OldState]),
    #state{server     = Server,
	   note_store = NoteStore,
	   sock       = Sock,
	   mpd_state  = MpdState,
	   log        = {OldLog, Type}, 
	   irb        = IRB,
	   irgc       = IRGC} = OldState, 
    NewLog = snmp_log:downgrade(OldLog), 
    State = 
	{state, Server, NoteStore, Sock, MpdState, {NewLog, Type}, IRB, IRGC},
    {ok, State};

code_change({down, _Vsn}, OldState, downgrade_to_pre_4_16) ->
    ?d("code_change(down, downgrade_to_pre_4_16) -> entry with"
       "~n   OldState: ~p", [OldState]),
    {OldLog, Type} = OldState#state.log, 
    NewLog = snmp_log:downgrade(OldLog), 
    State  = OldState#state{log = {NewLog, Type}}, 
    {ok, State};

% upgrade
code_change(_Vsn, OldState, upgrade_from_pre_4_14) ->
    ?d("code_change(up, upgrade_from_pre_4_14) -> entry with"
       "~n   OldState: ~p", [OldState]), 
    {state, Server, NoteStore, Sock, MpdState, {OldLog, Type}, IRB, IRGC} = 
	OldState,
    NewLog = snmp_log:upgrade(OldLog), 
    State = #state{server     = Server,
		   note_store = NoteStore,
		   sock       = Sock,
		   mpd_state  = MpdState,
		   log        = {NewLog, Type}, 
		   irb        = IRB,
		   irgc       = IRGC,
		   filter     = ?DEFAULT_FILTER_MODULE},
    {ok, State};

code_change(_Vsn, OldState, upgrade_from_pre_4_16) ->
    ?d("code_change(up, upgrade_from_pre_4_16) -> entry with"
       "~n   OldState: ~p", [OldState]),
    {OldLog, Type} = OldState#state.log,
    NewLog = snmp_log:upgrade(OldLog), 
    State  = OldState#state{log = {NewLog, Type}}, 
    {ok, State};

code_change(_Vsn, State, _Extra) ->
    ?d("code_change -> entry with"
       "~n   Vsn:   ~p"
       "~n   State: ~p"
       "~n   Extra: ~p", [_Vsn, State, _Extra]),
    {ok, State}.

 
%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

maybe_handle_recv_msg(Addr, Port, Bytes, #state{filter = FilterMod} = State) ->
    case (catch FilterMod:accept_recv(Addr, Port)) of
	false ->
	    %% Drop the received packet 
	    inc(netIfMsgInDrops),
	    ok;
	_ ->
	    handle_recv_msg(Addr, Port, Bytes, State)
    end.
    

handle_recv_msg(Addr, Port, Bytes, #state{server = Pid}) 
  when is_binary(Bytes) andalso (size(Bytes) =:= 0) ->
    Pid ! {snmp_error, {empty_message, Addr, Port}, Addr, Port},
    ok;

handle_recv_msg(Addr, Port, Bytes, 
		#state{server     = Pid, 
		       note_store = NoteStore, 
		       mpd_state  = MpdState, 
		       sock       = Sock,
		       log        = Log} = State) ->
    Logger = logger(Log, read, Addr, Port),
    case (catch snmpm_mpd:process_msg(Bytes, snmpUDPDomain, Addr, Port, 
				      MpdState, NoteStore, Logger)) of

	{ok, Vsn, Pdu, MS, ACM} ->
	    maybe_handle_recv_pdu(Addr, Port, Vsn, Pdu, MS, ACM, 
				  Logger, State);

	{discarded, Reason, Report} ->
	    ?vdebug("discarded: ~p", [Reason]),
	    ErrorInfo = {failed_processing_message, Reason},
	    Pid ! {snmp_error, ErrorInfo, Addr, Port},
	    maybe_udp_send(State#state.filter, Sock, Addr, Port, Report),
	    ok;

	{discarded, Reason} ->
	    ?vdebug("discarded: ~p", [Reason]),
	    ErrorInfo = {failed_processing_message, Reason},
	    Pid ! {snmp_error, ErrorInfo, Addr, Port},
	    ok;

	Error ->
	    error_msg("processing of received message failed: "
		      "~n   ~p", [Error]),
	    ok
    end.


maybe_handle_recv_pdu(Addr, Port, 
		      Vsn, #pdu{type = Type} = Pdu, PduMS, ACM, 
		      Logger, 
		      #state{filter = FilterMod} = State) ->
    case (catch FilterMod:accept_recv_pdu(Addr, Port, Type)) of
	false ->
	    inc(netIfPduInDrops),
	    ok;
	_ ->
	    handle_recv_pdu(Addr, Port, Vsn, Pdu, PduMS, ACM, Logger, State)
    end;
maybe_handle_recv_pdu(Addr, Port, Vsn, Trap, PduMS, ACM, Logger, 
		      #state{filter = FilterMod} = State) 
  when is_record(Trap, trappdu) ->
    case (catch FilterMod:accept_recv_pdu(Addr, Port, trappdu)) of
	false ->
	    inc(netIfPduInDrops),
	    ok;
	_ ->
	    handle_recv_pdu(Addr, Port, Vsn, Trap, PduMS, ACM, Logger, State)
    end;
maybe_handle_recv_pdu(Addr, Port, Vsn, Pdu, PduMS, ACM, Logger, State) ->
    handle_recv_pdu(Addr, Port, Vsn, Pdu, PduMS, ACM, Logger, State).


handle_recv_pdu(Addr, Port, 
		Vsn, #pdu{type = 'inform-request'} = Pdu, _PduMS, ACM, 
		Logger, #state{server = Pid, irb = IRB} = State) ->
    handle_inform_request(IRB, Pid, Vsn, Pdu, ACM, 
			  Addr, Port, Logger, State);
handle_recv_pdu(Addr, Port, 
		_Vsn, #pdu{type = report} = Pdu, _PduMS, ok, 
		_Logger, 
		#state{server = Pid} = _State) ->
    ?vtrace("received report - ok", []),
    Pid ! {snmp_report, {ok, Pdu}, Addr, Port};
handle_recv_pdu(Addr, Port, 
		_Vsn, #pdu{type = report} = Pdu, _PduMS, 
		{error, ReqId, Reason}, 
		_Logger, 
		#state{server = Pid} = _State) ->
    ?vtrace("received report - error", []),
    Pid ! {snmp_report, {error, ReqId, Reason, Pdu}, Addr, Port};
handle_recv_pdu(Addr, Port, 
		_Vsn, #pdu{type = 'snmpv2-trap'} = Pdu, _PduMS, _ACM, 
		_Logger, 
		#state{server = Pid} = _State) ->
    ?vtrace("received snmpv2-trap", []),
    Pid ! {snmp_trap, Pdu, Addr, Port};
handle_recv_pdu(Addr, Port, 
		_Vsn, Trap, _PduMS, _ACM, 
		_Logger, 
		#state{server = Pid} = _State) when is_record(Trap, trappdu) ->
    ?vtrace("received trappdu", []),
    Pid ! {snmp_trap, Trap, Addr, Port};
handle_recv_pdu(Addr, Port, 
		_Vsn, Pdu, _PduMS, _ACM, 
		_Logger, 
		#state{server = Pid} = _State) when is_record(Pdu, pdu) ->
    ?vtrace("received pdu", []),
    Pid ! {snmp_pdu, Pdu, Addr, Port};
handle_recv_pdu(_Addr, _Port, _Vsn, Pdu, _PduMS, ACM, _Logger, _State) ->
    ?vlog("received unexpected pdu: "
	    "~n   Pdu: ~p"
	    "~n   ACM: ~p", [Pdu, ACM]).


handle_inform_request(auto, Pid, Vsn, Pdu, ACM, Addr, Port, Logger, State) ->
    ?vtrace("received inform-request (true)", []),
    Pid ! {snmp_inform, ignore, Pdu, Addr, Port},
    RePdu = make_response_pdu(Pdu),
    maybe_send_inform_response(RePdu, Vsn, ACM, Addr, Port, Logger, State);
handle_inform_request({user, To}, Pid, Vsn, #pdu{request_id = ReqId} = Pdu, 
		      ACM, Addr, Port, _Logger, _State) ->
    ?vtrace("received inform-request (false)", []),

    Pid ! {snmp_inform, ReqId, Pdu, Addr, Port},

    %% Before we go any further, we need to check that we have not
    %% already received this message (possible resend).

    Key = {ReqId, Addr, Port},
    case ets:lookup(snmpm_inform_request_table, Key) of
	[_] ->
	    %% OK, we already know about this.  We assume this
	    %% is a resend. Either the agent is really eager or
	    %% the user has not answered yet. Bad user!
	    ok;
	[] ->
	    RePdu  = make_response_pdu(Pdu),
	    Expire = t() + To, 
	    Rec    = {Key, Expire, {Vsn, ACM, RePdu}},
	    ets:insert(snmpm_inform_request_table, Rec)
    end.
	    
handle_inform_response(Ref, Addr, Port, State) ->
    Key = {Ref, Addr, Port},
    case ets:lookup(snmpm_inform_request_table, Key) of
	[{Key, _, {Vsn, ACM, RePdu}}] ->
	    Logger = logger(State#state.log, read, Addr, Port),
	    ets:delete(snmpm_inform_request_table, Key), 
	    maybe_send_inform_response(RePdu, Vsn, ACM, Addr, Port, 
				       Logger, State);
	[] ->
	    %% Already acknowledged, or the user was to slow to reply...
	    ok
    end,
    ok.

maybe_send_inform_response(RePdu, Vsn, ACM, Addr, Port, Logger, 
			   #state{server = Pid, 
				  sock   = Sock, 
				  filter = FilterMod}) ->
    case (catch FilterMod:accept_send_pdu(Addr, Port, pdu_type_of(RePdu))) of
	false ->
	    inc(netIfPduOutDrops),
	    ok;
	_ ->
	    case snmpm_mpd:generate_response_msg(Vsn, RePdu, ACM, Logger) of
		{ok, Msg} ->
		    maybe_udp_send(FilterMod, Sock, Addr, Port, Msg);
		{discarded, Reason} ->
		    ?vlog("failed generating response message:"
			  "~n   Reason: ~p", [Reason]),
		    ReqId     = RePdu#pdu.request_id,
		    ErrorInfo = {failed_generating_response, {RePdu, Reason}},
		    Pid ! {snmp_error, ReqId, ErrorInfo, Addr, Port},
		    ok
	    end
    end.
    
handle_inform_response_gc(#state{irb = IRB} = State) ->
    ets:safe_fixtable(snmpm_inform_request_table, true),
    do_irgc(ets:first(snmpm_inform_request_table), t()),
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


maybe_handle_send_pdu(Pdu, Vsn, MsgData, Addr, Port, 
		      #state{filter = FilterMod} = State) ->
    case (catch FilterMod:accept_send_pdu(Addr, Port, pdu_type_of(Pdu))) of
	false ->
	    inc(netIfPduOutDrops),
	    ok;
	_ ->
	    handle_send_pdu(Pdu, Vsn, MsgData, Addr, Port, State)
    end.

handle_send_pdu(Pdu, Vsn, MsgData, Addr, Port, 
		#state{server     = Pid, 
		       note_store = NoteStore, 
		       sock       = Sock, 
		       log        = Log,
		       filter     = FilterMod}) ->
    Logger = logger(Log, write, Addr, Port),
    case (catch snmpm_mpd:generate_msg(Vsn, NoteStore, 
				       Pdu, MsgData, Logger)) of
	{ok, Msg} ->
	    ?vtrace("handle_send_pdu -> message generated", []),
	    maybe_udp_send(FilterMod, Sock, Addr, Port, Msg);	    
	{discarded, Reason} ->
	    ?vlog("PDU not sent: "
		  "~n   PDU:    ~p"
		  "~n   Reason: ~p", [Pdu, Reason]),
	    Pid ! {snmp_error, Pdu, Reason},
	    ok
    end.


maybe_udp_send(FilterMod, Sock, Addr, Port, Msg) ->
    case (catch FilterMod:accept_send(Addr, Port)) of
	false ->
	    inc(netIfMsgOutDrops),
	    ok;
	_ ->
	    udp_send(Sock, Addr, Port, Msg)
    end.
	    
    
udp_send(Sock, Addr, Port, Msg) ->
    case (catch gen_udp:send(Sock, Addr, Port, Msg)) of
	ok ->
	    ?vdebug("sent ~w bytes to ~w:~w [~w]", 
		    [sz(Msg), Addr, Port, Sock]),
	    ok;
	{error, Reason} ->
	    error_msg("failed sending message to ~p:~p: "
		      "~n   ~p",[Addr, Port, Reason]);
	Error ->
	    error_msg("failed sending message to ~p:~p: "
		      "~n   ~p",[Addr, Port, Error])
    end.

sz(B) when is_binary(B) ->
    size(B);
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


%% mk_discovery_msg('version-3', Pdu, _VsnHdr, UserName) ->
%%     ScopedPDU = #scopedPdu{contextEngineID = "",
%% 			   contextName = "",
%% 			   data = Pdu},
%%     Bytes = snmp_pdus:enc_scoped_pdu(ScopedPDU),
%%     MsgID = get(msg_id),
%%     put(msg_id,MsgID+1),
%%     UsmSecParams = 
%% 	#usmSecurityParameters{msgAuthoritativeEngineID = "",
%% 			       msgAuthoritativeEngineBoots = 0,
%% 			       msgAuthoritativeEngineTime = 0,
%% 			       msgUserName = UserName,
%% 			       msgPrivacyParameters = "",
%% 			       msgAuthenticationParameters = ""},
%%     SecBytes = snmp_pdus:enc_usm_security_parameters(UsmSecParams),
%%     PduType = Pdu#pdu.type,
%%     Hdr = #v3_hdr{msgID = MsgID, 
%% 		  msgMaxSize = 1000,
%% 		  msgFlags = snmp_misc:mk_msg_flags(PduType, 0),
%% 		  msgSecurityModel = ?SEC_USM,
%% 		  msgSecurityParameters = SecBytes},
%%     Msg = #message{version = 'version-3', vsn_hdr = Hdr, data = Bytes},
%%     case (catch snmp_pdus:enc_message_only(Msg)) of
%% 	{'EXIT', Reason} ->
%% 	    error("Encoding error. Pdu: ~w. Reason: ~w",[Pdu, Reason]),
%% 	    error;
%% 	L when list(L) ->
%% 	    {Msg, L}
%%     end;
%% mk_discovery_msg(Version, Pdu, {Com, _, _, _, _}, UserName) ->
%%     Msg = #message{version = Version, vsn_hdr = Com, data = Pdu},
%%     case catch snmp_pdus:enc_message(Msg) of
%% 	{'EXIT', Reason} ->
%% 	    error("Encoding error. Pdu: ~w. Reason: ~w",[Pdu, Reason]),
%% 	    error;
%% 	L when list(L) -> 
%% 	    {Msg, L}
%%     end.


%% mk_msg('version-3', Pdu, {Context, User, EngineID, CtxEngineId, SecLevel}, 
%%        MsgData) ->
%%     %% Code copied from snmp_mpd.erl
%%     {MsgId, SecName, SecData} =
%% 	if
%% 	    tuple(MsgData), Pdu#pdu.type == 'get-response' ->
%% 		MsgData;
%% 	    true -> 
%% 		Md = get(msg_id),
%% 		put(msg_id, Md + 1),
%% 		{Md, User, []}
%% 	end,
%%     ScopedPDU = #scopedPdu{contextEngineID = CtxEngineId,
%% 			   contextName = Context,
%% 			   data = Pdu},
%%     ScopedPDUBytes = snmp_pdus:enc_scoped_pdu(ScopedPDU),

%%     PduType = Pdu#pdu.type,
%%     V3Hdr = #v3_hdr{msgID      = MsgId,
%% 		    msgMaxSize = 1000,
%% 		    msgFlags   = snmp_misc:mk_msg_flags(PduType, SecLevel),
%% 		    msgSecurityModel = ?SEC_USM},
%%     Message = #message{version = 'version-3', vsn_hdr = V3Hdr,
%% 		       data = ScopedPDUBytes},
%%     SecEngineID = case PduType of
%% 		      'get-response' -> snmp_framework_mib:get_engine_id();
%% 		      _ -> EngineID
%% 		  end,
%%     case catch snmp_usm:generate_outgoing_msg(Message, SecEngineID,
%% 					      SecName, SecData, SecLevel) of
%% 	{'EXIT', Reason} ->
%% 	    error("Encoding error. Pdu: ~w. Reason: ~w",[Pdu, Reason]),
%% 	    error;
%% 	{error, Reason} ->
%% 	    error("Encoding error. Pdu: ~w. Reason: ~w",[Pdu, Reason]),
%% 	    error;
%% 	Packet ->
%% 	    Packet
%%     end;
%% mk_msg(Version, Pdu, {Com, _User, _EngineID, _Ctx, _SecLevel}, _SecData) ->
%%     Msg = #message{version = Version, vsn_hdr = Com, data = Pdu},
%%     case catch snmp_pdus:enc_message(Msg) of
%% 	{'EXIT', Reason} ->
%% 	    error("Encoding error. Pdu: ~w. Reason: ~w",[Pdu, Reason]),
%% 	    error;
%% 	B when list(B) -> 
%% 	    B
%%     end.


%% handle_system_info_updated(#state{log = {Log, _OldType}} = State, 
%% 			   audit_trail_log_type = _What) ->
%%     %% Just to make sure, check that ATL is actually enabled
%%     case snmpm_config:system_info(audit_trail_log) of
%% 	{ok, true} ->
%% 	    {ok, Type} = snmpm_config:system_info(audit_trail_log_type),
%% 	    NewState = State#state{log = {Log, Type}},
%% 	    {NewState, ok};
%% 	_ ->
%% 	    {State, {error, {adt_not_enabled}}}
%%     end;
%% handle_system_info_updated(_State, _What) ->
%%     ok.

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

t() ->
    {A,B,C} = erlang:now(),
    A*1000000000+B*1000+(C div 1000).


%% -------------------------------------------------------------------

logger(undefined, _Type, _Addr, _Port) ->
    fun(_) ->
	    ok
    end;
logger({Log, Types}, Type, Addr, Port) ->
    case lists:member(Type, Types) of
	true ->
	    fun(Msg) ->
		    snmp_log:log(Log, Msg, Addr, Port)
	    end;
	false ->
	    fun(_) ->
		    ok
	    end
    end.


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

get_info(#state{sock = Id}) ->
    ProcSize = proc_mem(self()),
    PortInfo = get_port_info(Id),
    [{process_memory, ProcSize}, {port_info, PortInfo}].

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

%% get_counters() ->
%%     Counters = counters(),
%%     get_counters(Counters, []).

%% get_counters([], Acc) ->
%%     lists:reverse(Acc);
%% get_counters([Counter|Counters], Acc) ->
%%     case snmpm_config:get_stats_counter(Counter) of
%%         {ok, CounterVal} ->
%%             get_counters(Counters, [{Counter, CounterVal}|Acc]);
%%         _ ->
%%             get_counters(Counters, Acc)
%%     end.


%% ----------------------------------------------------------------

call(Pid, Req) ->
    call(Pid, Req, infinity).

call(Pid, Req, Timeout) ->
    gen_server:call(Pid, Req, Timeout).

cast(Pid, Msg) ->
    gen_server:cast(Pid, Msg).

