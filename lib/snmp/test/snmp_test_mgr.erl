%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2019. All Rights Reserved.
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

-module(snmp_test_mgr).


%%----------------------------------------------------------------------
%% This module implements a simple SNMP manager for Erlang. Its used
%% during by the agent test suite.
%%----------------------------------------------------------------------

%% c(snmp_test_mgr).
%% snmp_test_mgr:start().
%% snmp_test_mgr:g([[sysContact,0]]).

%% snmp_test_mgr:start([{engine_id, "mbjk's engine"}, v3, {agent, "clip"}, {mibs, ["../mibs/SNMPv2-MIB"]}]).

%% snmp_test_mgr:start([{engine_id, "agentEngine"}, {user, "iwl_test"}, {dir, "mgr_conf"}, {sec_level, authPriv}, v3, {agent, "clip"}]).

%% User interface
-export([start_link/1, start/1, stop/0, 
	 d/0, discovery/0, 
	 g/1, s/1, gn/1, gn/0, r/0, gb/3, rpl/1,
	 send_bytes/1,
	 expect/2,expect/3,expect/4,expect/6,get_response/2, 
	 receive_response/0,
	 purify_oid/1, 
	 oid_to_name/1, name_to_oid/1]).

%% Internal exports
-export([get_oid_from_varbind/1, 
	 var_and_value_to_varbind/2, flatten_oid/2, make_vb/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("snmp/include/snmp_types.hrl").
-include_lib("snmp/include/STANDARD-MIB.hrl").
-include("snmp_test_lib.hrl").
-include_lib("snmp/src/app/snmp_internal.hrl").

-record(state, {dbg         = true,
                quiet,
                parent,
                timeout     = 3500,
                print_traps = true,
                mini_mib,
                packet_server, 
                last_sent_pdu, 
                last_received_pdu}).

-define(SERVER, ?MODULE).
-define(PACK_SERV, snmp_test_mgr_misc).

start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Options, self()}, []).

start(Options) ->
    gen_server:start({local, ?SERVER}, ?MODULE, {Options, self()}, []).

stop() ->
    call(stop).

d() ->
    discovery().

discovery() ->
    call(discovery).

g(Oids) ->
    cast({get, Oids}).

%% VarsAndValues is: {PlainOid, o|s|i, Value} (unknown mibs) | {Oid, Value} 
s(VarsAndValues) ->
    cast({set, VarsAndValues}).

gn(Oids) when is_list(Oids) ->
    cast({get_next, Oids});
gn(N) when is_integer(N) ->
    cast({iter_get_next, N}).
gn() ->
    cast(iter_get_next).

r() ->
    cast(resend_pdu).

gb(NonRepeaters, MaxRepetitions, Oids) ->
    cast({bulk, {NonRepeaters, MaxRepetitions, Oids}}).

rpl(RespPdu) ->
    cast({response, RespPdu}).

send_bytes(Bytes) ->
    cast({send_bytes, Bytes}).

purify_oid(Oid) ->
    call({purify_oid, Oid}, 5000).

oid_to_name(Oid) ->
    call({oid_to_name, Oid}, 5000).

name_to_oid(Name) ->
    call({name_to_oid, Name}, 5000).


%%----------------------------------------------------------------------
%% Purpose: For writing test sequences
%% Args: Y=any (varbinds) | trap | timeout | VarBinds | ErrStatus
%% Returns: ok|{error, Id, Reason}
%%----------------------------------------------------------------------
expect(Id,Y) -> echo_errors(expect_impl(Id,Y)).
expect(Id,v2trap,VBs) -> echo_errors(expect_impl(Id,v2trap,VBs));
expect(Id,report,VBs) -> echo_errors(expect_impl(Id,report,VBs));
expect(Id,{inform, Reply},VBs) ->
    echo_errors(expect_impl(Id,{inform,Reply},VBs)).
expect(Id,Err,Idx,VBs) -> echo_errors(expect_impl(Id,Err,Idx,VBs)).
expect(Id,trap, Enterp, Generic, Specific, ExpectedVarbinds) ->
    echo_errors(expect_impl(Id,trap,Enterp,Generic,
			    Specific,ExpectedVarbinds)).

%%-----------------------------------------------------------------
%% Purpose: For writing test sequences
%%-----------------------------------------------------------------
get_response(Id, Vars) -> echo_errors(get_response_impl(Id, Vars)).

%%----------------------------------------------------------------------
%% Receives a response from the agent.
%% Returns: a PDU or {error, Reason}.
%% It doesn't receive traps though.
%%----------------------------------------------------------------------
receive_response() ->
    receive_response(get_timeout()).

receive_response(Timeout) ->
    d("await response within ~w ms",[Timeout]),
    receive
	{snmp_pdu, PDU} when is_record(PDU, pdu) ->
	    d("received PDU: ~n\t~p",[PDU]),
	    PDU
    after Timeout ->
	    d("response timeout",[]),
	    {error, timeout}
    end.


get_timeout() ->
    case get(receive_response_timeout) of
	Int when is_integer(Int) and (Int > 0) ->
	    Int;
	_ ->
	    get_timeout(os:type())
    end.

get_timeout(_)       -> 10000. % Trying to improve test results % 3500.

%%----------------------------------------------------------------------
%% Receives a trap from the agent.
%% Returns: TrapPdu|{error, Reason}
%%----------------------------------------------------------------------
receive_trap(Timeout) ->
    d("await trap within ~w ms",[Timeout]),
    receive
	{snmp_pdu, PDU} when is_record(PDU, trappdu) ->
	    d("received trap-PDU: ~n\t~p",[PDU]),
	    PDU
    after Timeout ->
	    d("trap timeout",[]),
	    {error, timeout}
    end.

%%----------------------------------------------------------------------
%% Options: List of
%%  {agent_udp, UDPPort},  {agent, Agent}
%%  Optional: 
%%  {community, String ("public" is default}, quiet,
%%  {mibs, List of Filenames}, {trap_udp, UDPPort (default 5000)},
%%----------------------------------------------------------------------
init({Options, CallerPid}) ->
    put(sname,     mgr),
    put(verbosity, debug), 
    ?SNMP_RAND_SEED(),
    %% rand:seed(exrop,
    %%           {erlang:phash2([node()]),
    %%            erlang:monotonic_time(),
    %%            erlang:unique_integer()}),
    case (catch is_options_ok(Options)) of
	true ->
	    put(debug, get_value(debug, Options, false)),
	    d("init -> (~p) extract options",[self()]),
 	    PacksDbg    = get_value(packet_server_debug, Options, false),
	    print("[~w] ~p -> PacksDbg: ~p~n", [?MODULE, self(), PacksDbg]),
	    RecBufSz    = get_value(recbuf,            Options, 1024),
	    print("[~w] ~p -> RecBufSz: ~p~n", [?MODULE, self(), RecBufSz]),
	    Mibs        = get_value(mibs,              Options, []),
	    print("[~w] ~p -> Mibs: ~p~n", [?MODULE, self(), Mibs]),
	    Udp         = get_value(agent_udp,         Options, 4000),
	    print("[~w] ~p -> Udp: ~p~n", [?MODULE, self(), Udp]),
	    User        = get_value(user,              Options, "initial"),
	    print("[~w] ~p -> User: ~p~n", [?MODULE, self(), User]),
	    EngineId    = get_value(engine_id,         Options, "agentEngine"),
	    print("[~w] ~p -> EngineId: ~p~n", [?MODULE, self(), EngineId]),
	    CtxEngineId = get_value(context_engine_id, Options, EngineId),
	    print("[~w] ~p -> CtxEngineId: ~p~n", [?MODULE, self(), CtxEngineId]),
	    TrapUdp     = get_value(trap_udp,          Options, 5000),
	    print("[~w] ~p -> TrapUdp: ~p~n", [?MODULE, self(), TrapUdp]),
	    Dir         = get_value(dir,               Options, "."),
	    print("[~w] ~p -> Dir: ~p~n", [?MODULE, self(), Dir]),
	    SecLevel    = get_value(sec_level,         Options, noAuthNoPriv),
	    print("[~w] ~p -> SecLevel: ~p~n", [?MODULE, self(), SecLevel]),
	    MiniMIB     = snmp_mini_mib:create(Mibs),
	    d("[~w] ~p -> MiniMIB: "
              "~n   ~p", [?MODULE, self(), MiniMIB]),
	    Version     = case lists:member(v2, Options) of
			      true -> 'version-2';
			      false -> 
				  case lists:member(v3, Options) of
				      true -> 'version-3';
				      false -> 'version-1'
				  end
			  end,
	    print("[~w] ~p -> Version: ~p~n", [?MODULE, self(), Version]),
	    Com = case Version of
		      'version-3' ->
			  get_value(context, Options, "");
		      _ ->
			  get_value(community, Options, "public")
		  end,
	    print("[~w] ~p -> Com: ~p~n", [?MODULE, self(), Com]),
	    VsnHdrD = 
		{Com, User, EngineId, CtxEngineId, mk_seclevel(SecLevel)},
	    print("[~w] ~p -> VsnHdrD: ~p~n", [?MODULE, self(), VsnHdrD]),
	    IpFamily = get_value(ipfamily, Options, inet),
	    print("[~w] ~p -> IpFamily: ~p~n", [?MODULE, self(), IpFamily]),
	    AgIp = case snmp_misc:assq(agent, Options) of
		       {value, Addr} when is_tuple(Addr) andalso 
                                          (size(Addr) =:= 4) andalso
                                          (IpFamily =:= inet) ->
                           print("[~w] ~p -> Addr: ~p~n",
                                 [?MODULE, self(), Addr]),
			   Addr;
		       {value, Addr} when is_tuple(Addr) andalso 
                                          (size(Addr) =:= 8) andalso
                                          (IpFamily =:= inet6) ->
                           print("[~w] ~p -> Addr: ~p~n",
                                 [?MODULE, self(), Addr]),
			   Addr;
		       {value, Host} when is_list(Host) ->
                           print("[~w] ~p -> Host: ~p~n",
                                 [?MODULE, self(), Host]),
			   {ok, Ip} = snmp_misc:ip(Host, IpFamily),
			   Ip
		   end,
	    print("[~w] ~p -> AgIp: ~p~n", [?MODULE, self(), AgIp]),
	    Quiet = lists:member(quiet, Options),
	    print("[~w] ~p -> Quiet: ~p~n", [?MODULE, self(), Quiet]),
	    PackServ =
		start_packet_server(
		  Quiet, Options, CallerPid, AgIp, Udp, TrapUdp,
		  VsnHdrD, Version, Dir, RecBufSz, PacksDbg, IpFamily),
	    d("init -> packet server: ~p",[PackServ]),
	    State = #state{parent        = CallerPid,
			   quiet         = Quiet,
			   mini_mib      = MiniMIB, 
			   packet_server = PackServ},
	    d("init -> done",[]),
	    {ok, State};
	
	{error, Reason} -> 
	    {stop,Reason}
    end.

start_packet_server(false, _Options, _CallerPid, AgIp, Udp, TrapUdp, 
		    VsnHdrD, Version, Dir, RecBufSz, PacksDbg, IpFamily) ->
    d("start_packet_server -> entry", []),
    ?PACK_SERV:start_link_packet(
       {msg, self()}, AgIp, Udp, TrapUdp,
       VsnHdrD, Version, Dir, RecBufSz, PacksDbg, IpFamily);
start_packet_server(true, Options, CallerPid, AgIp, Udp, TrapUdp, 
		    VsnHdrD, Version, Dir, RecBufSz, PacksDbg, IpFamily) ->
    Type =  get_value(receive_type, Options, pdu),
    d("start_packet_server -> entry with"
      "~n   CallerPid: ~p"
      "~n   when"
      "~n   Type:      ~p",[CallerPid, Type]),
    ?PACK_SERV:start_link_packet(
       {Type, CallerPid}, AgIp, Udp, TrapUdp,
       VsnHdrD, Version, Dir, RecBufSz, PacksDbg, IpFamily).

is_options_ok([{mibs,List}|Opts]) when is_list(List) ->
    is_options_ok(Opts);
is_options_ok([quiet|Opts])  ->
    is_options_ok(Opts);
is_options_ok([{agent,_}|Opts]) ->
    is_options_ok(Opts);
is_options_ok([{ipfamily,IpFamily}|Opts])
  when IpFamily =:= inet;
       IpFamily =:= inet6 ->
    is_options_ok(Opts);
is_options_ok([{agent_udp,Int}|Opts]) when is_integer(Int) ->
    is_options_ok(Opts);
is_options_ok([{trap_udp,Int}|Opts]) when is_integer(Int) ->
    is_options_ok(Opts);
is_options_ok([{community,List}|Opts]) when is_list(List) ->
    is_options_ok(Opts);
is_options_ok([{dir,List}|Opts]) when is_list(List) ->
    is_options_ok(Opts);
is_options_ok([{sec_level,noAuthNoPriv}|Opts]) ->
    is_options_ok(Opts);
is_options_ok([{sec_level,authNoPriv}|Opts]) ->
    is_options_ok(Opts);
is_options_ok([{sec_level,authPriv}|Opts]) ->
    is_options_ok(Opts);
is_options_ok([{context,List}|Opts]) when is_list(List) ->
    is_options_ok(Opts);
is_options_ok([{user,List}|Opts]) when is_list(List) ->
    is_options_ok(Opts);
is_options_ok([{engine_id,List}|Opts]) when is_list(List) ->
    is_options_ok(Opts);
is_options_ok([{context_engine_id,List}|Opts]) when is_list(List) ->
    is_options_ok(Opts);
is_options_ok([v1|Opts]) ->
    is_options_ok(Opts);
is_options_ok([v2|Opts]) ->
    is_options_ok(Opts);
is_options_ok([v3|Opts]) ->
    is_options_ok(Opts);
is_options_ok([{debug,Bool}|Opts]) ->
    case is_bool(Bool) of
	ok ->
	    is_options_ok(Opts);
	error ->
	    {error, {bad_option, debug, Bool}}
    end;
is_options_ok([{packet_server_debug,Bool}|Opts]) ->
    case is_bool(Bool) of
	ok ->
	    is_options_ok(Opts);
	error ->
	    {error, {bad_option, packet_server_debug, Bool}}
    end;
is_options_ok([{recbuf,Sz}|Opts]) when (0 < Sz) and (Sz =< 65535) ->
    is_options_ok(Opts);
is_options_ok([InvOpt|_]) ->
    {error,{invalid_option,InvOpt}};
is_options_ok([]) -> true.

is_bool(true)  -> ok;
is_bool(false) -> ok;
is_bool(_)     -> error.

mk_seclevel(noAuthNoPriv) -> 0;
mk_seclevel(authNoPriv) -> 1;
mk_seclevel(authPriv) -> 3.
    

handle_call({purify_oid, Oid}, _From, #state{mini_mib = MiniMib} = State) ->
    d("handle_call -> purify_oid for ~p",[Oid]),
    Reply = (catch purify_oid(Oid, MiniMib)),
    {reply, Reply, State};

handle_call({find_pure_oid, XOid}, _From, State) ->
    d("handle_call -> find_pure_oid for ~p",[XOid]),
    {reply, catch flatten_oid(XOid, State#state.mini_mib), State};

handle_call({oid_to_name, Oid}, _From, State) ->
    d("handle_call -> oid_to_name for Oid: ~p",[Oid]),
    Reply = 
	case lists:keysearch(Oid, 1, State#state.mini_mib) of
	    {value, {_Oid, Name, _Type}} ->
		{ok, Name};
	    false ->
		{error, {no_such_oid, Oid}}
	end,
    {reply, Reply, State};

handle_call({name_to_oid, Name}, _From, State) ->
    d("handle_call -> name_to_oid for Name: ~p",[Name]),
    Reply = 
	case lists:keysearch(Name, 2, State#state.mini_mib) of
	    {value, {Oid, _Name, _Type}} ->
		{ok, Oid};
	    false ->
		{error, {no_such_name, Name}}
	end,
    {reply, Reply, State};

handle_call(stop, _From, #state{mini_mib = MiniMIB} = State) ->
    d("handle_call -> stop request",[]),
    snmp_mini_mib:delete(MiniMIB),
    {stop, normal, ok, State#state{mini_mib = undefined}};

handle_call(discovery, _From, State) ->
    d("handle_call -> discovery",[]),
    {Reply, NewState} = execute_discovery(State),
    {reply, Reply, NewState}.
    

handle_cast({get, Oids}, State) ->
    d("handle_cast -> get request for ~p", [Oids]),
    {noreply, execute_request(get, Oids, State)};

handle_cast({set, VariablesAndValues}, State) ->
    d("handle_cast -> set request for ~p", [VariablesAndValues]),
    {noreply, execute_request(set, VariablesAndValues, State)};

handle_cast({get_next, Oids}, State) ->
    d("handle_cast -> get-next request for ~p", [Oids]),
    {noreply, execute_request(get_next, Oids, State)};

handle_cast(iter_get_next, State)
  when is_record(State#state.last_received_pdu, pdu) ->
    d("handle_cast -> iter_get_next request", []),
    PrevPDU = State#state.last_received_pdu,
    Oids    = [get_oid_from_varbind(Vb) || Vb <- PrevPDU#pdu.varbinds], 
    {noreply, execute_request(get_next, Oids, State)};

handle_cast(iter_get_next, State) ->
    ?PACK_SERV:error("[Iterated get-next] No Response PDU to "
		     "start iterating from.", []),
    {noreply, State};

handle_cast({iter_get_next, N}, State) ->
    d("handle_cast -> iter_get_next(~p) request",[N]),
    if
	is_record(State#state.last_received_pdu, pdu) ->
	    PDU = get_next_iter_impl(N, State#state.last_received_pdu,
				     State#state.mini_mib,
				     State#state.packet_server),
	    {noreply, State#state{last_received_pdu = PDU}};
	true ->
	    ?PACK_SERV:error("[Iterated get-next] No Response PDU to "
				"start iterating from.", []),
	    {noreply, State}
    end;

handle_cast(resend_pdu, #state{last_sent_pdu = PDU} = State) ->
    d("handle_cast -> resend_pdu request when"
      "~n   PDU = ~p", [PDU]),
    send_pdu(PDU#pdu{request_id = make_request_id()},
	     State#state.mini_mib,
	     State#state.packet_server),
    {noreply, State};

handle_cast({bulk, Args}, State) ->
    d("handle_bulk -> bulk request for ~p", [Args]),
    {noreply, execute_request(bulk, Args, State)};

handle_cast({response, RespPdu}, State) ->
    d("handle_cast -> response request with "
      "~n   ~p", [RespPdu]),
    ?PACK_SERV:send_pdu(RespPdu, State#state.packet_server),
    {noreply, State};

handle_cast({send_bytes, Bytes}, State) ->
    d("handle_cast -> send-bytes request for ~p bytes", [sizeOf(Bytes)]),
    ?PACK_SERV:send_bytes(Bytes, State#state.packet_server),
    {noreply, State};

handle_cast(Msg, State) ->
    d("handle_cast -> unknown message: "
      "~n   ~p", [Msg]),
    {noreply, State}.


handle_info({snmp_msg, Msg, Ip, Udp}, State) ->
    io:format("* Got PDU: ~s", [?PACK_SERV:format_hdr(Msg)]),
    PDU = ?PACK_SERV:get_pdu(Msg),
    echo_pdu(PDU, State#state.mini_mib),
    case PDU#pdu.type of
	'inform-request' ->
	    %% Generate a response...
	    RespPDU = PDU#pdu{type = 'get-response',
			      error_status = noError,
			      error_index = 0},
	    RespMsg = ?PACK_SERV:set_pdu(Msg, RespPDU),
	    ?PACK_SERV:send_msg(RespMsg, State#state.packet_server, Ip, Udp);
	_Else ->
	    ok
    end,
    {noreply, State#state{last_received_pdu = PDU}};

handle_info(Info, State) ->
    d("handle_info -> unknown info: "
      "~n   ~p", [Info]),
    {noreply, State}.


terminate(Reason, State) ->
    d("terminate -> with Reason: ~n\t~p",[Reason]),
    ?PACK_SERV:stop(State#state.packet_server).


%%----------------------------------------------------------------------
%% Returns: A new State
%%----------------------------------------------------------------------
execute_discovery(State) ->
    Pdu   = make_discovery_pdu(),
    Reply = ?PACK_SERV:send_discovery_pdu(Pdu, State#state.packet_server),
    {Reply, State#state{last_sent_pdu = Pdu}}.


execute_request(Operation, Data, State) ->
    case (catch make_pdu(Operation, Data, State#state.mini_mib)) of
	{error, {Format, Data2}} ->
	    report_error(State, Format, Data2),
	    State;
	{error, _Reason} -> 
	    State;
	PDU when is_record(PDU, pdu) ->
	    send_pdu(PDU, State#state.mini_mib, State#state.packet_server),
	    State#state{last_sent_pdu = PDU}
    end.
    
report_error(#state{quiet = true, parent = Pid}, Format, Args) ->
    Reason = lists:flatten(io_lib:format(Format, Args)),
    Pid ! {oid_error, Reason};
report_error(_, Format, Args) ->
    ?PACK_SERV:error(Format, Args).


get_oid_from_varbind(#varbind{oid = Oid}) -> Oid.

send_pdu(PDU, _MiniMIB, PackServ) ->
    ?PACK_SERV:send_pdu(PDU, PackServ).

%%----------------------------------------------------------------------
%% Purpose: Unnesting of oids like [myTable, 3, 4, "hej", 45] to
%%          [1,2,3,3,4,104,101,106,45]
%%----------------------------------------------------------------------

purify_oid([A|T], MiniMib) when is_atom(A) ->
    Oid2 = 
	case snmp_mini_mib:oid(MiniMib, A) of
	    false ->
		throw({error, {unknown_aliasname, A}});
	    Oid ->
		lists:flatten([Oid|T])
	end,
    {ok, verify_pure_oid(Oid2)};
purify_oid(L, _) when is_list(L) ->
    {ok, verify_pure_oid(lists:flatten(L))};
purify_oid(X, _) ->
    {error, {invalid_oid, X}}.
    
verify_pure_oid([]) -> 
    [];
verify_pure_oid([H | T]) when is_integer(H) and (H >= 0) ->
    [H | verify_pure_oid(T)];
verify_pure_oid([H | _]) ->
    throw({error, {not_pure_oid, H}}).
    
flatten_oid(XOid, DB)  ->
    Oid = case XOid of
	       [A|T] when is_atom(A) -> 
		   [remove_atom(A, DB)|T];
	       L when is_list(L) -> 
		   XOid;
	       Shit -> 
		   throw({error,
			  {"Invalid oid, not a list of integers: ~w", [Shit]}})
	   end,
    check_is_pure_oid(lists:flatten(Oid)).

remove_atom(AliasName, DB) when is_atom(AliasName) ->
    case snmp_mini_mib:oid(DB, AliasName) of
	false ->
	    throw({error, {"Unknown aliasname in oid: ~w", [AliasName]}});
	Oid -> 
	    Oid
    end;
remove_atom(X, _DB) -> 
    X.

%%----------------------------------------------------------------------
%% Throws if not a list of integers
%%----------------------------------------------------------------------
check_is_pure_oid([]) -> [];
check_is_pure_oid([X | T]) when is_integer(X) and (X >= 0) ->
    [X | check_is_pure_oid(T)];
check_is_pure_oid([X | _T]) ->
    throw({error, {"Invalid oid, it contains a non-integer: ~w", [X]}}).

get_next_iter_impl(0, PrevPDU, _MiniMIB, _PackServ) -> 
    PrevPDU;
get_next_iter_impl(N, PrevPDU, MiniMIB, PackServ) ->
    Oids = [get_oid_from_varbind(Vb) || Vb <- PrevPDU#pdu.varbinds],
    PDU  = make_pdu(get_next, Oids, MiniMIB),
    send_pdu(PDU, MiniMIB, PackServ),
    case receive_response() of
	{error, timeout} ->
	    io:format("(timeout)~n"),
	    get_next_iter_impl(N, PrevPDU, MiniMIB, PackServ);
	{error, _Reason} ->
	    PrevPDU;
	RPDU when is_record(RPDU, pdu) ->
	    io:format("(~w)", [N]),
	    echo_pdu(RPDU, MiniMIB),
	    get_next_iter_impl(N-1, RPDU, MiniMIB, PackServ)
    end.
	
%%--------------------------------------------------
%% Used to resend a PDU. Takes the old PDU and
%% generates a fresh one (with a new requestID).
%%--------------------------------------------------

make_pdu(set, VarsAndValues, MiniMIB) ->
    VBs = [var_and_value_to_varbind(VAV, MiniMIB) || VAV <- VarsAndValues],
    make_pdu_impl(set, VBs);
make_pdu(bulk, {NonRepeaters, MaxRepetitions, Oids}, MiniMIB) ->
    Foids = [flatten_oid(Oid, MiniMIB) || Oid <- Oids], 
    #pdu{type = 'get-bulk-request',
	 request_id   = make_request_id(),
	 error_status = NonRepeaters, 
	 error_index  = MaxRepetitions,
	 varbinds     = [make_vb(Oid) || Oid <- Foids]};
make_pdu(Operation, Oids, MiniMIB) ->
    Foids = [flatten_oid(Oid, MiniMIB) || Oid <- Oids], 
    make_pdu_impl(Operation, Foids).

make_pdu_impl(get, Oids) ->
    #pdu{type         = 'get-request',
	 request_id   = make_request_id(),
	 error_status = noError, 
	 error_index  = 0,
	 varbinds     = [make_vb(Oid) || Oid <- Oids]};

make_pdu_impl(get_next, Oids) ->
    #pdu{type         = 'get-next-request', 
	 request_id   = make_request_id(), 
	 error_status = noError, 
	 error_index  = 0,
	 varbinds     = [make_vb(Oid) || Oid <- Oids]};

make_pdu_impl(set, Varbinds) ->
    #pdu{type         = 'set-request', 
	 request_id   = make_request_id(),
	 error_status = noError, 
	 error_index  = 0, 
	 varbinds     = Varbinds}.

make_discovery_pdu() ->
    make_pdu_impl(get, []).

var_and_value_to_varbind({Oid, Type, Value}, MiniMIB) ->
    Oid2 = flatten_oid(Oid, MiniMIB), 
    #varbind{oid          = Oid2, 
	     variabletype = char_to_type(Type), 
	     value        = Value};
var_and_value_to_varbind({XOid, Value}, MiniMIB) ->
    Oid = flatten_oid(XOid, MiniMIB), 
    #varbind{oid          = Oid, 
	     variabletype = snmp_mini_mib:type(MiniMIB, Oid),
	     value        = Value}.

char_to_type(o) ->
    'OBJECT IDENTIFIER';
char_to_type(i) ->
    'INTEGER';
char_to_type(u) ->
    'Unsigned32';
char_to_type(g) -> % Gauge, Gauge32
    'Unsigned32';
char_to_type(s) ->
    'OCTET STRING'.

make_vb(Oid) ->
    #varbind{oid = Oid, variabletype = 'NULL', value = 'NULL'}.

make_request_id() ->
    snmp_test_mgr_counter_server:increment(mgr_request_id, 1, 1, 2147483647).

echo_pdu(PDU, MiniMIB) ->
    io:format("~s", [snmp_misc:format_pdu(PDU, MiniMIB)]).


%%----------------------------------------------------------------------
%% Test Sequence
%%----------------------------------------------------------------------
echo_errors({error, Id, {ExpectedFormat, ExpectedData}, {Format, Data}})->
    io:format("* Unexpected Behaviour * Id: ~w.~n"
	      "  Expected: " ++ ExpectedFormat ++ "~n"
	      "  Got:      " ++ Format ++ "~n", 
	      [Id] ++ ExpectedData ++ Data),
    {error, Id, {ExpectedFormat, ExpectedData}, {Format, Data}};
echo_errors(ok) -> ok;
echo_errors({ok, Val}) -> {ok, Val}.

get_response_impl(Id, ExpVars) ->
    ?PRINT2("await response ~w with"
            "~n   Expected Varbinds: ~p",
            [Id, ExpVars]),
    PureVars = find_pure_oids2(ExpVars),
    case receive_response() of
	#pdu{type         = 'get-response', 
	     error_status = noError, 
	     error_index  = 0,
	     varbinds     = VBs} ->
            ?PRINT2("received expected response pdu (~w) - match vars"
                    "~n   Expected VBs: ~p"
                    "~n   Received VBs: ~p",
                    [Id, PureVars, VBs]),
	    match_vars(Id, PureVars, VBs, []);

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2, 
	     error_index  = Index2} ->
            ?EPRINT2("received unexpected response pdu: ~w, ~w, ~w"
                     "~n   Received Error: ~p"
                     "~n   Received Index: ~p",
                     [Type2, Id, ReqId, Err2, Index2]),
	    {error, 
	     Id, 
	     {"Type: ~w, ErrStat: ~w, Idx: ~w, RequestId: ~w",
	      ['get-response', noError, 0, ReqId]},
	     {"Type: ~w, ErrStat: ~w, Idx: ~w", 
	      [Type2, Err2, Index2]}};

	{error, Reason} -> 
            ?EPRINT2("unexpected receive pdu error: ~w"
                     "~n   ~p", [Id, Reason]),
	    format_reason(Id, Reason)
    end.

    

%%----------------------------------------------------------------------
%% Returns: ok | {error, Id, {ExpectedFormat, ExpectedData}, {Format, Data}}
%%----------------------------------------------------------------------
expect_impl(Id, any) -> 
    ?PRINT2("await ~w pdu (any)", [Id]),
    case receive_response() of
	PDU when is_record(PDU, pdu) -> 
            ?PRINT2("received expected pdu (~w)", [Id]),
            ok;
	{error, Reason} ->
            ?EPRINT1("unexpected receive error: ~w"
                     "~n   ~p", [Id, Reason]),
            format_reason(Id, Reason)
    end;

expect_impl(Id, return) -> 
    ?PRINT2("await ~w pdu", [Id]),
    case receive_response() of
	PDU when is_record(PDU, pdu) ->
            ?PRINT2("received expected pdu (~w)", [Id]),
            {ok, PDU};
	{error, Reason} ->
            ?EPRINT1("unexpected receive error: ~w"
                     "~n   ~p", [Id, Reason]),
            format_reason(Id, Reason)
    end;

expect_impl(Id, trap) -> 
    ?PRINT2("await ~w trap", [Id]),
    case receive_trap(3500) of
	PDU when is_record(PDU, trappdu) ->
            ?PRINT2("received expected trap (~w)", [Id]),
            ok;
	{error, Reason} ->
            ?EPRINT1("unexpected receive error: ~w"
                     "~n   ~p", [Id, Reason]),
            format_reason(Id, Reason)
    end;

expect_impl(Id, timeout) -> 
    ?PRINT2("await ~w nothing", [Id]),
    receive
	X -> 
            ?EPRINT1("received unexpected message: ~w"
                     "~n   ~p",
                     [Id, X]),
	    {error, Id, {"Timeout", []}, {"Message ~w",  [X]}}
    after 3500 ->
	    ok
    end;

expect_impl(Id, Err) when is_atom(Err) ->
    ?PRINT2("await ~w with"
            "~n   Err: ~p",
            [Id, Err]),
    case receive_response() of
	#pdu{error_status = Err} -> 
            ?PRINT2("received pdu with expected error status (~w, ~w)",
                    [Id, Err]),
	    ok;

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2} ->
            ?EPRINT1("received pdu with unexpected error status: ~w, ~w, ~w"
                     "~n   Expected Error: ~p"
                     "~n   Received Error: ~p",
                     [Type2, Id, ReqId, Err, Err2]),
	    {error, Id, {"ErrorStatus: ~w, RequestId: ~w", [Err,ReqId]},
	     {"ErrorStatus: ~w", [Err2]}};

	{error, Reason} -> 
            ?EPRINT1("unexpected receive error: ~w"
                     "~n   ~p", [Id, Reason]),
	    format_reason(Id, Reason)
    end;

expect_impl(Id, ExpectedVarbinds) when is_list(ExpectedVarbinds) ->
    ?PRINT2("await ~w with"
            "~n   ExpectedVarbinds: ~p",
            [Id, ExpectedVarbinds]),
    PureVars = find_pure_oids(ExpectedVarbinds),
    case receive_response() of
	#pdu{type         = 'get-response', 
	     error_status = noError, 
	     error_index  = 0,
	     varbinds     = VBs} ->
            ?PRINT2("received expected response pdu (~w) - check varbinds"
                    "~n   Expected VBs: ~p"
                    "~n   Received VBs: ~p",
                    [Id, PureVars, VBs]),
	    check_vars(Id, PureVars, VBs);

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2, 
	     error_index  = Index2} ->
            ?EPRINT1("received unexpected pdu: ~w, ~w, ~w"
                     "~n   Received Error: ~p"
                     "~n   Received Index: ~p",
                     [Type2, Id, ReqId, Err2, Index2]),
	    {error, Id, {"Type: ~w, ErrStat: ~w, Idx: ~w, RequestId: ~w", 
			 ['get-response', noError, 0, ReqId]},
	     {"Type: ~w, ErrStat: ~w, Idx: ~w", [Type2, Err2, Index2]}};

	{error, Reason} -> 
            ?EPRINT1("unexpected receive error: ~w"
                     "~n   ~p", [Id, Reason]),
	    format_reason(Id, Reason)
    end.

expect_impl(Id, v2trap, ExpectedVarbinds) when is_list(ExpectedVarbinds) ->
    ?PRINT2("await v2 trap ~w with"
            "~n   ExpectedVarbinds: ~p",
            [Id, ExpectedVarbinds]),
    PureVars = find_pure_oids(ExpectedVarbinds),
    case receive_response() of
	#pdu{type         = 'snmpv2-trap', 
	     error_status = noError, 
	     error_index  = 0,
	     varbinds     = VBs} ->
            ?PRINT2("received expected v2 trap (~w) - check varbinds"
                    "~n   Expected VBs: ~p"
                    "~n   Received VBs: ~p",
                    [Id, PureVars, VBs]),
	    check_vars(Id, PureVars, VBs);

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2, 
	     error_index  = Index2} ->
            ?EPRINT1("received unexpected pdu: ~w, ~w, ~w"
                     "~n   Received Error: ~p"
                     "~n   Received Index: ~p",
                     [Type2, Id, ReqId, Err2, Index2]),
	    {error, Id, {"Type: ~w, ErrStat: ~w, Idx: ~w, RequestId: ~w", 
			 ['snmpv2-trap', noError, 0, ReqId]},
	     {"Type: ~w, ErrStat: ~w, Idx: ~w", [Type2, Err2, Index2]}};

	{error, Reason} -> 
            ?EPRINT1("unexpected receive error: ~w"
                     "~n   ~p", [Id, Reason]),
	    format_reason(Id, Reason)
    end;

expect_impl(Id, report, ExpectedVarbinds) when is_list(ExpectedVarbinds) ->
    ?PRINT2("await report ~w with"
            "~n   ExpectedVarbinds: ~p",
            [Id, ExpectedVarbinds]),
    PureVBs = find_pure_oids(ExpectedVarbinds),
    case receive_response() of
	#pdu{type         = 'report', 
	     error_status = noError, 
	     error_index  = 0,
	     varbinds     = VBs} ->
            ?PRINT2("received expected report (~w) - check varbinds"
                    "~n   Expected VBs: ~p"
                    "~n   Received VBs: ~p",
                    [Id, PureVBs, VBs]),
	    check_vars(Id, PureVBs, VBs);

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2, 
	     error_index  = Index2} ->
            ?EPRINT1("received unexpected pdu: ~w, ~w, ~w"
                     "~n   Received Error: ~p"
                     "~n   Received Index: ~p",
                     [Type2, Id, ReqId, Err2, Index2]),
	    {error, Id, {"Type: ~w, ErrStat: ~w, Idx: ~w, RequestId: ~w", 
			 [report, noError, 0, ReqId]},
	     {"Type: ~w, ErrStat: ~w, Idx: ~w", [Type2, Err2, Index2]}};

	{error, Reason} -> 
            ?EPRINT1("unexpected receive error: ~w"
                     "~n   ~p", [Id, Reason]),
	    format_reason(Id, Reason)
    end;

expect_impl(Id, {inform, Reply}, ExpectedVarbinds) 
  when is_list(ExpectedVarbinds) ->
    ?PRINT2("await inform ~w with"
            "~n   Reply:            ~p"
            "~n   ExpectedVarbinds: ~p",
            [Id, Reply, ExpectedVarbinds]),
    PureVBs = find_pure_oids(ExpectedVarbinds),
    Resp    = receive_response(),
    case Resp of
	#pdu{type         = 'inform-request', 
	     error_status = noError, 
	     error_index  = 0,
	     varbinds     = VBs} ->
            ?PRINT2("received inform (~w) - check varbinds"
                    "~n   Expected VBs: ~p"
                    "~n   Received VBs: ~p",
                    [Id, PureVBs, VBs]),
	    case check_vars(Id, PureVBs, VBs) of
		ok when (Reply == true) ->
                    ?PRINT2("varbinds ok (~w) - send ok inform response", [Id]),
		    RespPDU = Resp#pdu{type = 'get-response',
				       error_status = noError,
				       error_index = 0},
		    ?MODULE:rpl(RespPDU),
		    ok;
		ok when (element(1, Reply) == error) ->
                    ?PRINT2("varbinds ok (~w) - send error inform response", [Id]),
		    {error, Status, Index} = Reply,
		    RespPDU = Resp#pdu{type = 'get-response',
				       error_status = Status,
				       error_index = Index},
		    ?MODULE:rpl(RespPDU),
		    ok;
		ok when (Reply == false) ->
                    ?PRINT2("varbinds ok (~w) - don't send inform response", [Id]),
		    ok;
		Else ->
                    ?EPRINT1("unexpected varbinds (~w)", [Id]),
		    io:format("expect_impl(~w, inform) -> "
			      "~n   Else: ~p"
			      "~n", [Id, Else]),
		    Else
	    end;

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2, 
	     error_index  = Index2} ->
            ?EPRINT1("received unexpected pdu: ~w, ~w, ~w"
                     "~n   Received Error: ~p"
                     "~n   Received Index: ~p",
                     [Type2, Id, ReqId, Err2, Index2]),
	    {error, Id, {"Type: ~w, ErrStat: ~w, Idx: ~w, RequestId: ~w", 
			 ['inform-request', noError, 0, ReqId]},
	     {"Type: ~w, ErrStat: ~w, Idx: ~w", [Type2, Err2, Index2]}};

	{error, Reason} -> 
            ?EPRINT1("unexpected receive error: ~w"
                     "~n   ~p", [Id, Reason]),
	    format_reason(Id, Reason)
    end.

expect_impl(Id, Err, Index, any = _ExpectedVarbinds) ->
    ?PRINT2("await response ~w with"
            "~n   Err:              ~p"
            "~n   Index:            ~p"
            "~n   ExpectedVarbinds: ~p",
            [Id, Err, Index, _ExpectedVarbinds]),
    case receive_response() of
	#pdu{type         = 'get-response', 
	     error_status = Err, 
	     error_index  = Index} -> 
            ?PRINT2("received expected response pdu (~w, ~w, ~w)",
                    [Id, Err, Index]),
	    ok;

	#pdu{type         = 'get-response',
             error_status = Err} when (Index == any) -> 
            ?PRINT2("received expected response pdu (~w, ~w)",
                    [Id, Err]),
	    ok;

	#pdu{type         = 'get-response', 
	     request_id   = ReqId, 
	     error_status = Err, 
	     error_index  = Idx} when is_list(Index) ->
	    case lists:member(Idx, Index) of
		true -> 
                    ?PRINT2("received expected response pdu (~w, ~w, ~w)",
                            [Id, Err, Idx]),
		    ok;
		false ->
                    ?EPRINT1("received response pdu with unexpected index (~w, ~w):"
                             "~n   Expected Index: ~p"
                             "~n   Received Index: ~p",
                             [Id, Err, Index, Idx]),
		    {error, Id, {"ErrStat: ~w, Idx: ~w, RequestId: ~w", 
				 [Err, Index, ReqId]},
		     {"ErrStat: ~w, Idx: ~w", [Err, Idx]}}
	    end;

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2, 
	     error_index  = Index2} ->
            ?EPRINT1("received unexpected response pdu: ~w, ~w, ~w"
                     "~n   Expected Error: ~p"
                     "~n   Received Error: ~p"
                     "~n   Expected Index: ~p"
                     "~n   Received Index: ~p",
                     [Type2, Id, ReqId, Err, Err2, Index, Index2]),
	    {error, Id, {"Type: ~w, ErrStat: ~w, Idx: ~w, RequestId: ~w", 
			 ['get-response', Err, Index, ReqId]},
	     {"Type: ~w, ErrStat: ~w, Idx: ~w", [Type2, Err2, Index2]}};

	{error, Reason} -> 
	    format_reason(Id, Reason)
    end;

expect_impl(Id, Err, Index, ExpectedVarbinds) ->
    ?PRINT2("await response ~w with"
            "~n   Err:              ~p"
            "~n   Index:            ~p"
            "~n   ExpectedVarbinds: ~p",
            [Id, Err, Index, ExpectedVarbinds]),
    PureVBs = find_pure_oids(ExpectedVarbinds),
    case receive_response() of
	#pdu{type         = 'get-response', 
	     error_status = Err, 
	     error_index  = Index,
	     varbinds     = VBs} ->
            ?PRINT2("received expected response pdu (~w, ~w, ~w) - check varbinds"
                    "~n   Expected VBs: ~p"
                    "~n   Received VBs: ~p",
                    [Id, Err, Index, PureVBs, VBs]),
	    check_vars(Id, PureVBs, VBs);

	#pdu{type         = 'get-response', 
	     error_status = Err, 
	     varbinds     = VBs} when (Index == any) ->
            ?PRINT2("received expected response pdu (~w, ~w) - check varbinds"
                    "~n   Expected VBs: ~p"
                    "~n   Received VBs: ~p",
                    [Id, Err, PureVBs, VBs]),
	    check_vars(Id, PureVBs, VBs);

	#pdu{type         = 'get-response', 
	     request_id   = ReqId, 
	     error_status = Err, 
	     error_index  = Idx,
	     varbinds     = VBs} when is_list(Index) ->
	    case lists:member(Idx, Index) of
		true ->
                    ?PRINT2("received expected pdu (~w, ~w, ~w) - check varbinds"
                            "~n   Expected VBs: ~p"
                            "~n   Received VBs: ~p",
                            [Id, Err, Idx, PureVBs, VBs]),
		    check_vars(Id, PureVBs, VBs);
		false ->
                    ?EPRINT1("received response pdu with unexpected index (~w, ~w):"
                             "~n   Expected Index: ~p"
                             "~n   Received Index: ~p"
                             "~n   Expected VBs:   ~p"
                             "~n   Received VBs:   ~p",
                             [Id, Err, Index, Idx, PureVBs, VBs]),
		    {error,Id,
		     {"ErrStat: ~w, Idx: ~w, Varbinds: ~w, RequestId: ~w",
		      [Err,Index,PureVBs,ReqId]},
		     {"ErrStat: ~w, Idx: ~w, Varbinds: ~w",
		      [Err,Idx,VBs]}}
	    end;

	#pdu{type         = Type2, 
	     request_id   = ReqId, 
	     error_status = Err2, 
	     error_index  = Index2, 
	     varbinds     = VBs} ->
            ?EPRINT1("received unexpected response pdu: ~w, ~w, ~w"
                     "~n   Expected Error: ~p"
                     "~n   Received Error: ~p"
                     "~n   Expected Index: ~p"
                     "~n   Received Index: ~p"
                     "~n   Expected VBs:   ~p"
                     "~n   Received VBs:   ~p",
                     [Type2, Id, ReqId,
                      Err, Err2, Index, Index2, PureVBs, VBs]),
	    {error,Id,
	     {"Type: ~w, ErrStat: ~w, Idx: ~w, Varbinds: ~w, RequestId: ~w",
	      ['get-response',Err,Index,PureVBs,ReqId]},
	     {"Type: ~w, ErrStat: ~w Idx: ~w Varbinds: ~w",
	      [Type2,Err2,Index2,VBs]}};

	{error, Reason} ->
            ?EPRINT1("unexpected receive pdu error: ~w"
                     "~n   ~p", [Id, Reason]),
	    format_reason(Id, Reason)
    end.

expect_impl(Id, trap, Enterp, Generic, Specific, ExpectedVarbinds) ->
    ?PRINT2("await trap pdu ~w with"
            "~n   Enterprise:       ~p"
            "~n   Generic:          ~p"
            "~n   Specific:         ~p"
            "~n   ExpectedVarbinds: ~p",
            [Id, Enterp, Generic, Specific, ExpectedVarbinds]),
    PureE   = find_pure_oid(Enterp),
    PureVBs = find_pure_oids(ExpectedVarbinds),
    case receive_trap(3500) of
	#trappdu{enterprise    = PureE, 
		 generic_trap  = Generic,
		 specific_trap = Specific, 
		 varbinds      = VBs} ->
            ?PRINT2("received expected trap pdu - check varbinds"
                    "~n   Expected VBs: ~p"
                    "~n   Received VBs: ~p",
                    [PureVBs, VBs]),
	    check_vars(Id, PureVBs, VBs);

	#trappdu{enterprise    = Ent2, 
		 generic_trap  = G2,
		 specific_trap = Spec2, 
		 varbinds      = VBs} ->
            ?EPRINT1("received unexpected trap pdu: ~w"
                     "~n   Expected Enterprise: ~p"
                     "~n   Received Enterprise: ~p"
                     "~n   Expected Generic:    ~p"
                     "~n   Received Generic:    ~p"
                     "~n   Expected Specific:   ~p"
                     "~n   Received Specific:   ~p"
                     "~n   Expected VBs:        ~p"
                     "~n   Received VBs:        ~p",
                     [Id,
                      PureE, Ent2,
                      Generic, G2,
                      Specific, Spec2,
                      PureVBs, VBs]),
	    {error, Id,
	     {"Enterprise: ~w, Generic: ~w, Specific: ~w, Varbinds: ~w",
	      [PureE, Generic, Specific, ExpectedVarbinds]},
	     {"Enterprise: ~w, Generic: ~w, Specific: ~w, Varbinds: ~w",
	      [Ent2, G2, Spec2, VBs]}};

	{error, Reason} -> 
            ?EPRINT1("unexpected receive trap pdu error: ~w"
                     "~n   ~p", [Id, Reason]),
	    format_reason(Id, Reason)
    end.

format_reason(Id, Reason) ->
    {error, Id, {"?", []}, {"~w", [Reason]}}.


%%----------------------------------------------------------------------
%% Args: Id, ExpectedVarbinds, GotVarbinds
%% Returns: ok
%% Fails: if not ok
%%----------------------------------------------------------------------
check_vars(_Id,[], []) -> 
    ok;
check_vars(Id,Vars, []) ->
    {error, Id, {"More Varbinds (~w)", [Vars]}, {"Too few", []}};
check_vars(Id,[], Varbinds) ->
    {error,Id, {"Fewer Varbinds", []}, {"Too many (~w)", [Varbinds]}};
check_vars(Id,[{_XOid, any} | Vars], [#varbind{oid = _Oid} |Vbs]) ->
    check_vars(Id,Vars, Vbs);
check_vars(Id,[{Oid, Val} | Vars], [#varbind{oid = Oid, value = Val} |Vbs]) ->
    check_vars(Id,Vars, Vbs);
check_vars(Id,[{Oid, Val} | _], [#varbind{oid = Oid, value = Val2} |_]) ->
    {error, Id, {" Varbind: ~w = ~w", [Oid, Val]}, {"Value: ~w", [Val2]}};
check_vars(Id,[{Oid, _Val} | _], [#varbind{oid = Oid2, value = _Val2} |_]) ->
    {error, Id, {"Oid: ~w", [Oid]}, {"Oid: ~w", [Oid2]}}.

match_vars(Id, [Oid|T], [#varbind{oid = Oid, value = Value} | Vbs], Res) ->
    match_vars(Id, T, Vbs, [Value | Res]);
match_vars(_Id, [], [], Res) ->
    {ok, lists:reverse(Res)};
match_vars(Id, [Oid | _], [#varbind{oid = Oid2}], _Res) ->
    {error, Id, {" Oid: ~w", [Oid]}, {"Oid2: ~w", [Oid2]}};
match_vars(Id, Vars, [], _Res) ->
    {error, Id, {"More Varbinds (~w)", [Vars]}, {"Too few", []}};
match_vars(Id, [], Varbinds, _Res) ->
    {error,Id, {"Fewer Varbinds", []}, {"Too many (~w)", [Varbinds]}}.

    

find_pure_oids([]) -> [];
find_pure_oids([{XOid, Q}|T]) ->
    [{find_pure_oid(XOid), Q} | find_pure_oids(T)].

find_pure_oids2([]) -> [];
find_pure_oids2([XOid|T]) ->
    [find_pure_oid(XOid) | find_pure_oids2(T)].


%%----------------------------------------------------------------------
%% Returns: Oid
%% Fails: malformed oids
%%----------------------------------------------------------------------
find_pure_oid(XOid) ->
    case gen_server:call(?MODULE, {find_pure_oid, XOid}, infinity) of
	{error, {Format, Data}} ->
	    ok = io:format(Format, Data),
	    exit(malformed_oid);
	Oid when is_list(Oid) -> Oid
    end.

get_value(Opt, Opts, Default) ->
    case snmp_misc:assq(Opt,Opts) of
	{value, C} -> C;
	false -> Default
    end.


%%----------------------------------------------------------------------

call(Req) ->
    call(Req, infinity).

call(Req, To) ->
    gen_server:call(?SERVER, Req, To).

cast(Msg) ->
    gen_server:cast(?SERVER, Msg).


%%----------------------------------------------------------------------
%% Debug
%%----------------------------------------------------------------------

sizeOf(L) when is_list(L) ->
    length(lists:flatten(L));
sizeOf(B) when is_binary(B) ->
    size(B).

d(F, A) -> d(get(debug), F, A).

d(true, F, A) ->
    print(F, A);
d(_,_F,_A) -> 
    ok.

print(F, A) ->
    ?PRINT2("MGR " ++ F, A).

%% formated_timestamp() ->
%%     snmp_test_lib:formated_timestamp().
