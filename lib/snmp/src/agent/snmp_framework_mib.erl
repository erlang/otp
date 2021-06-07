%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2020. All Rights Reserved.
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
-module(snmp_framework_mib).

-include("snmp_types.hrl").
-include("STANDARD-MIB.hrl").

-define(VMODULE,"FRAMEWORK-MIB").
-include("snmp_verbosity.hrl").

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.


%%%-----------------------------------------------------------------
%%% This module implements the init- configure- and instrumentation-
%%% functions for the SNMP-FRAMEWORK-MIB.
%%%
%%% We also keep internal datastructures here, e.g. a table
%%% over all known contexts.
%%%-----------------------------------------------------------------
%% External exports
%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([init/0, configure/1]).
-export([intContextTable/1, intContextTable/3,
	 intAgentTransportDomain/1, intAgentTransports/1,
	 intAgentUDPPort/1, intAgentIpAddress/1,
	 snmpEngineID/1,
	 snmpEngineBoots/1,
	 snmpEngineTime/1,
	 snmpEngineMaxMessageSize/1,
	 get_engine_id/0, get_engine_max_message_size/0,
	 get_engine_boots/0, get_engine_time/0,
	 set_engine_boots/1, set_engine_time/1,
	 table_next/2, check_status/3]).
-export([which_trap_transport/1, which_req_transport/1, which_transport/2]).
-export([add_context/1, delete_context/1]).
-export([check_agent/2, check_context/1, order_agent/2]).


%%-----------------------------------------------------------------
%% Func: init/0
%% Purpose: Creates the tables and variables necessary for the SNMP
%%          mechanism to work properly.
%%          Note that this function won't destroy any old values.
%%          This function should be called only once.
%%-----------------------------------------------------------------
init() ->
    maybe_create_table(intContextTable),
    init_engine().


%%-----------------------------------------------------------------
%% Func: configure/1
%% Args: Dir is the directory with trailing dir_separator where
%%       the configuration files can be found.
%% Purpose: Reads the config-files for the internal tables, and
%%          inserts the data.  Makes sure that all old data in
%%          the tables are deleted, and the new data inserted.
%%          This function makes sure that all (and only) 
%%          config-file-data are in the tables. 
%% Returns: ok
%% Fails: exit(configuration_error)
%% PRE: init/1 has been successfully called
%%-----------------------------------------------------------------
configure(Dir) ->
    set_sname(),
    case snmpa_agent:get_agent_mib_storage() of
        mnesia ->
            ok;
        _ ->
	    case (catch do_configure(Dir)) of
		ok ->
		    ok;
		{error, Reason} ->
		    ?vinfo("configure error: ~p", [Reason]),
		    config_err("configure failed: ~p", [Reason]),
		    exit(configuration_error);
		Error ->
		    ?vinfo("configure failed: ~p", [Error]),
		    config_err("configure failed: ~p", [Error]),
		    exit(configuration_error)
	    end
    end,
    ok.

do_configure(Dir) ->
    ?vdebug("read internal config files",[]),
    Contexts = read_internal_config_files(Dir),
    ?vdebug("read agent config files",[]),
    Agent = read_agent(Dir),
    ?vdebug("initiate vars",[]),
    init_vars(Agent),
    %% Add default context, if not present.
    NContexts = [{""} | lists:delete({""}, Contexts)],
    ?vdebug("initiate tables",[]),
    init_tabs(NContexts),
    ok.

read_internal_config_files(Dir) ->
    ?vdebug("read context config file",[]),
    Gen    = fun gen_context/2,
    Order  = fun snmp_conf:no_order/2,
    Filter = fun snmp_conf:no_filter/1,
    Check  = fun(Entry, State) -> {check_context(Entry), State} end,
    [Ctxs] =
	snmp_conf:read_files
	  (Dir, [{"context.conf", Gen, Order, Check, Filter}]),
    Ctxs.

read_agent(Dir) ->
    ?vdebug("read agent config file", []),
    FileName = "agent.conf",
    File     = filename:join(Dir, FileName), 
    Conf0    =
	try
	    snmp_conf:read(File, fun order_agent/2, fun check_agent/2)
	catch
	    throw:{error, Reason} ->
		error({failed_reading_config_file, Dir, FileName, Reason})
	end,
    Mand =
	[{intAgentTransports,       mandatory},
	 {snmpEngineMaxMessageSize, mandatory},
	 {snmpEngineID,             mandatory}],
    {ok, Conf} = snmp_conf:check_mandatory(Conf0, Mand),
    Conf.


%%-----------------------------------------------------------------
%% Generate a context.conf file.
%%-----------------------------------------------------------------
gen_context(Dir, _Reason) ->
    config_err("missing context.conf file => generating a default file", []),
    File = filename:join(Dir, "context.conf"),
    case file:open(File, [write]) of
	{ok, Fid} ->
	    ok = io:format(Fid, "~s\n", [context_header()]),
	    ok = io:format(Fid, "%% The default context\n\"\".\n", []),
	    file:close(Fid),
	    [];
	{error, Reason} ->
            file:delete(File),
	    error({failed_creating_file, File, Reason})
    end.

context_header() ->
    {Y,Mo,D} = date(),
    {H,Mi,S} = time(),
    io_lib:format("%% This file was automatically generated by "
                  "snmp_config v~s  ~w-~2.2.0w-~2.2.0w "
                  "~2.2.0w:~2.2.0w:~2.2.0w\n",
                  [?version, Y, Mo, D, H, Mi, S]).


%%-----------------------------------------------------------------
%%  Context
%%  Context.
%%-----------------------------------------------------------------
check_context(Context) ->
    ?vtrace("check_context -> entry with"
        "~n   Context: ~p", [Context]),
    case (catch snmp_conf:check_string(Context)) of
	ok ->
	    {ok, {Context}};
	_ ->
	    error({invalid_context, Context})
    end.


%%-----------------------------------------------------------------
%%  Agent
%%  {Name, Value}.
%%-----------------------------------------------------------------
check_agent(Entry, undefined) ->
    check_agent(Entry, #{domain => snmp_target_mib:default_domain(),
                         port   => undefined});

%% <BACKWARD-COMPAT>
check_agent({intAgentTransportDomain, Domain},
            #{transports := T} = State) when is_list(T) andalso (T =/= []) ->
    ?vinfo("check_agent(intAgentTransportDomain) -> entry with"
           "~n      Domain: ~p"
           "~n   when"
           "~n      State:  ~p", [Domain, State]),
    error({transports_already_defined, T});
check_agent({intAgentTransportDomain, Domain}, State) ->
    ?vtrace("check_agent(intAgentTransportDomain) -> entry with"
            "~n      Domain: ~p", [Domain]),
    {snmp_conf:check_domain(Domain), State#{domain => Domain}};
%% </BACKWARD-COMPAT>

check_agent({intAgentUDPPort, NewPort}, #{port := OldPort})
  when is_integer(OldPort) ->
    ?vinfo("check_agent(intAgentUDPPort) -> entry with"
          "~n      New Port: ~p"
          "~n   when"
          "~n      Old Port: ~p", [NewPort, OldPort]),
    error({port_already_defined, OldPort});
check_agent({intAgentUDPPort, Port}, State) ->
    ?vtrace("check_agent(intAgentUDPPort) -> entry with"
            "~n      Port:  ~p"
            "~n   when"
            "~n      State: ~p", [Port, State]),
    ok = snmp_conf:check_port(Port),
    {ok, State#{port => Port}};

%% <BACKWARD-COMPAT>
check_agent({intAgentIpAddress, Ip}, #{port := undefined}) ->
    ?vinfo("check_agent(intAgentIpAddress) -> "
           "entry when port not defined with"
           "~n      Ip: ~p", [Ip]),
    error({missing_mandatory, intAgentUDPPort});
check_agent({intAgentIpAddress = _Tag, Ip} = _Entry, 
            #{transports := T} = _State) when (T =/= []) ->
    ?vinfo("check_agent(intAgentIpAddress) -> "
           "entry when transports already defined with"
           "~n      Ip:         ~p"
           "~n   when"
           "~n      Transports: ~p", [Ip, T]),
    error({transports_already_defined, T});
check_agent({intAgentIpAddress = Tag, Ip} = _Entry,
            #{domain := Domain, port := Port} = State) ->
    ?vtrace("check_agent(intAgentIpAddress) -> entry with"
            "~n      Ip:     ~p"
            "~n   when"
            "~n      Domain: ~p"
            "~n      Port:   ~p", [Ip, Domain, Port]),
    FixedIp = case snmp_conf:check_ip(Domain, Ip) of
                  ok ->
                      Ip;
                  {ok, FIp} ->
                      FIp
              end,
    T       = [{Domain, {FixedIp, Port}, all, []}],
    Rows    = [{Tag, FixedIp}, {intAgentTransports, T}],
    {Rows, State#{transports => T}};
%% </BACKWARD-COMPAT>

check_agent({intAgentTransports = _Tag, _Transports},
            #{transports := T} = _State) when (T =/= []) -> 
    ?vinfo("check_agent(intAgentTransports) -> "
           "entry when transports already defined with"
           "~n      T: ~p", [T]),
    error({transports_already_defined, T});   
check_agent({intAgentTransports = Tag, Transports}, #{port := Port} = State)
  when is_list(Transports) ->
    ?vtrace("check_agent(intAgentTransports) -> entry when"
            "~n      Port: ~p", [Port]),
    CheckAddress =
        fun(D, A, undefined) ->
                snmp_conf:check_address(D, A);
           (D, A, P) ->
                snmp_conf:check_address(D, A, P)
        end,
    CheckedTransports =
	[case Transport of
	     {Domain, Address} ->
                 ?vtrace("check_agent(intAgentTransports) -> check transport: "
                         "~n      Domain:  ~p"
                         "~n      Address: ~p", [Domain, Address]),
                 CheckedAddress =
                     case CheckAddress(Domain, Address, Port) of
                         ok ->
                             Address;
                         {ok, Address2} ->
                             Address2
                     end,
                 ?vtrace("check_agent(intAgentTransports) -> checked address: "
                         "~n      ~p", [CheckedAddress]),
                 {Domain, CheckedAddress, all, []};

	     {Domain, Address, Kind} when is_atom(Kind) ->
                 ?vtrace("check_agent(intAgentTransports) -> check transport: "
                         "~n      Domain:  ~p"
                         "~n      Address: ~p"
                         "~n      Kind:    ~p", [Domain, Address, Kind]),
                 ok = snmp_conf:check_transport_kind(Kind),
                 ?vtrace("check_agent(intAgentTransports) -> checked kind"),
                 case snmp_conf:check_transport_address(Domain, Address) of
                     true ->
                         ?vtrace("check_agent(intAgentTransports) -> "
                                 "checked transport address"),
                         {Domain, Address, Kind, []};
                     false ->
                         ?vinfo("check_agent(intAgentTransports) -> "
                                "invalid transport address: "
                                "~n      ~p", [Address]),
                         error({bad_transport_addr, Address})
                 end;

             {Domain, Address, Opts} when is_list(Opts) ->
                 ?vtrace("check_agent(intAgentTransports) -> check transport: "
                         "~n      Domain:  ~p"
                         "~n      Address: ~p"
                         "~n      Opts:    ~p", [Domain, Address, Opts]),
                 CheckedOpts = snmp_conf:check_transport_opts(Opts),
                 ?vtrace("check_agent(intAgentTransports) -> checked opts: "
                         "~n      ~p", [CheckedOpts]),
                 case snmp_conf:check_transport_address(Domain, Address) of
                     true ->
                         ?vtrace("check_agent(intAgentTransports) -> "
                                 "checked transport address"),
                         {Domain, Address, all, CheckedOpts};
                     false ->
                         ?vinfo("check_agent(intAgentTransports) -> "
                                "invalid transport address: "
                                "~n      ~p", [Address]),
                         error({bad_transport_addr, Address})
                 end;

             {Domain, Address, Kind, Opts} ->
                 ?vtrace("check_agent(intAgentTransports) -> check transport: "
                         "~n      Domain:  ~p"
                         "~n      Address: ~p"
                         "~n      Kind:    ~p"
                         "~n      Opts:    ~p", [Domain, Address, Kind, Opts]),
                 ok = snmp_conf:check_transport_kind(Kind),
                 ?vtrace("check_agent(intAgentTransports) -> checked kind"),
                 CheckedOpts = snmp_conf:check_transport_opts(Opts),
                 ?vtrace("check_agent(intAgentTransports) -> checked opts: "
                         "~n      ~p", [CheckedOpts]),
                 case snmp_conf:check_transport_address(Domain, Address) of
                     true ->
                         ?vtrace("check_agent(intAgentTransports) -> "
                                 "checked transport address"),
                         {Domain, Address, Kind, CheckedOpts};
                     false ->
                         ?vinfo("check_agent(intAgentTransports) -> "
                                "invalid transport address: "
                                "~n      ~p", [Address]),
                         error({bad_transport_addr, Address})
                 end;

	     _ ->
                 ?vinfo("check_agent(intAgentTransports) -> invalid transport:"
                        "~n      ~p", [Transport]),
                 error({bad_transport, Transport})
	 end
	 || Transport <- Transports],
    validate_transports(CheckedTransports),
    ?vtrace("check_agent(intAgentTransports) -> checked transports"),
    {{ok, {Tag, CheckedTransports}}, State};
check_agent(Entry, State) ->
    ?vtrace("check_agent -> entry when"
            "~n      Entry: ~p", [Entry]),
    {check_agent(Entry), State}.

%% Basically this is intended to check that there are no
%% inconsistencies (between transports). Such as specifying
%% both an old style transport (Kind = all) and transports
%% with specified Kind:s (rep_responser or trap_sender).
validate_transports(Transports) ->
    validate_transports(Transports, false).

validate_transports([] = _Transports, _All) ->
    ok;
validate_transports([{_Domain, _Addr, all, _Opts} | Transports], _All) ->
    validate_transports(Transports, true);
validate_transports([{_Domain, _Addr, Kind, _Opts} | _Transports], true)
  when (Kind =/= all) ->
    error({bad_transport_kind, Kind});
validate_transports([{_Domain, _Addr, _Kind, _Opts} | Transports], All) ->
    validate_transports(Transports, All).



%% This one is kept for backwards compatibility
check_agent({intAgentMaxPacketSize, Value}) -> 
    snmp_conf:check_packet_size(Value);
check_agent({snmpEngineMaxMessageSize, Value}) -> 
    snmp_conf:check_packet_size(Value);
check_agent({snmpEngineID, Value}) -> 
    snmp_conf:check_string(Value);
check_agent(X) -> 
    error({invalid_agent_attribute, X}).

%% Ordering function to sort intAgentTransportDomain first
%% hence before intAgentIpAddress.  Sort other entries on the key.
%% Note that neither of these are required!
-dialyzer({nowarn_function, order_agent/2}).
order_agent(EntryA, EntryB) ->
    snmp_conf:keyorder(
      1, EntryA, EntryB,
      [intAgentTransportDomain, intAgentUDPPort | sort]).



maybe_create_table(Name) ->
    case snmpa_local_db:table_exists(db(Name)) of
	true -> 
	    ok;
	_ -> 
	    ?vtrace("create table: ~w",[Name]),
	    snmpa_local_db:table_create(db(Name))
    end.

init_vars(Vars) ->
    lists:map(fun init_var/1, Vars).

init_var({Var, Val}) ->
    ?vtrace("init var: "
	    "~n   set ~w to ~w", [Var, Val]),
    snmp_generic:variable_set(db(Var), Val).    

init_tabs(Contexts) ->
    ?vdebug("create context table",[]),
    snmpa_local_db:table_delete(db(intContextTable)),
    snmpa_local_db:table_create(db(intContextTable)),
    init_context_table(Contexts).
    
init_context_table([Row | T]) ->
    Context = element(1, Row),
    Key = [length(Context) | Context],
    ?vtrace("create intContextTable table row for: ~w",[Key]),
    snmpa_local_db:table_create_row(db(intContextTable), Key, Row),
    init_context_table(T);
init_context_table([]) -> true.


table_cre_row(Tab, Key, Row) ->
    snmpa_mib_lib:table_cre_row(db(Tab), Key, Row).

table_del_row(Tab, Key) ->
    snmpa_mib_lib:table_del_row(db(Tab), Key).


%% FIXME: does not work with mnesia
add_context(Ctx) ->
    case (catch check_context(Ctx)) of
	{ok, Row} ->
	    Context = element(1, Row),
	    Key = [length(Context) | Context],
	    case table_cre_row(intContextTable, Key, Row) of
		true ->
		    {ok, Key};
		false ->
		    {error, create_failed}
	    end;
	{error, Reason} ->
	    {error, Reason};
	Error ->
	    {error, Error}
    end.

%% FIXME: does not work with mnesia
delete_context(Key) ->
    case table_del_row(intContextTable, Key) of
        true ->
            ok;
        false ->
            {error, delete_failed}
    end.

   
%%-----------------------------------------------------------------
%% Instrumentation functions
%% Retreive functions are also used internally by the agent, so
%% don't change the interface without changing those functions.
%% Note that if these functions implementations are changed,
%% an error can make the agent crash, as no error detection is
%% performed for the internal data.
%% These functions cannot use the default functions as is, because
%% the default functions rely on that the mib is loaded, and
%% these functions must work even if the OTP-FRAMEWORK-MIB isn't loaded.
%% So we hardcode the information necessary for the functions
%% called by the default functions in snmp_generic. This info is
%% normally provided by the mib compiler, and inserted into 
%% snmpa_symbolic_store at load mib time.
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% None if the int* objects are defined in any MIB.  
%%
%% intContextTable keeps all
%% known contexts internally, but is ordered as an SNMP table.  It
%% could be defined as:
%%   intContextTable OBJECT-TYPE
%%       SYNTAX       SEQUENCE OF IntContextEntry
%%       MAX-ACCESS   not-accessible
%%       STATUS       current
%%       DESCRIPTION "A table of locally available contexts."
%%       ::= { xx }
%%   
%%   intContextEntry OBJECT-TYPE
%%       SYNTAX       IntContextEntry
%%       MAX-ACCESS   not-accessible
%%       STATUS       current
%%       DESCRIPTION "Information about a particular context."
%%       INDEX       {
%%                     intContextName
%%                   }
%%       ::= { intContextTable 1 }
%%   
%%   IntContextEntry ::= SEQUENCE
%%       {
%%           intContextName SnmpAdminString
%%       }
%%   
%%   intContextName  OBJECT-TYPE
%%       SYNTAX       SnmpAdminString (SIZE(0..32))
%%       MAX-ACCESS   read-only
%%       STATUS       current
%%       DESCRIPTION "A human readable name identifying a particular
%%                    context at a particular SNMP entity.
%%   
%%                    The empty contextName (zero length) represents the
%%                    default context.
%%                   "
%%       ::= { intContextEntry 1 }
%%-----------------------------------------------------------------

%% Op == new | delete
intContextTable(Op) ->
    snmp_generic:table_func(Op, db(intContextTable)).

%% Op == get get_next  -- READ only table
intContextTable(get, RowIndex, Cols) ->
    get(intContextTable, RowIndex, Cols);
intContextTable(get_next, RowIndex, Cols) ->
    next(intContextTable, RowIndex, Cols);
intContextTable(Op, Arg1, Arg2) ->
    snmp_generic:table_func(Op, Arg1, Arg2, db(intContextTable)).

%% FIXME: exported, not used by agent, not documented - remove?
table_next(Name, RestOid) ->
    snmp_generic:table_next(db(Name), RestOid).

%% FIXME: exported, not used by agent, not documented - remove?
%% FIXME: does not work with mnesia
check_status(Name, Indexes, StatusNo) ->
    case snmpa_local_db:table_get_element(db(Name), Indexes, StatusNo) of
	{value, ?'RowStatus_active'} -> true;
	_ -> false
    end.

db(intContextTable) -> {intContextTable, volatile};
db(X) -> snmpa_agent:db(X).

fa(intContextTable) -> 1.
 
foi(intContextTable) -> 1.

noc(intContextTable) -> 1.
 
next(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_next(db(Name), RowIndex, Cols,
                                   fa(Name), foi(Name), noc(Name)).
 
get(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_get(db(Name), RowIndex, Cols, foi(Name)).

%% Op == new | delete | get
intAgentUDPPort(Op) ->
    snmp_generic:variable_func(Op, db(intAgentUDPPort)).

intAgentIpAddress(Op) ->
    snmp_generic:variable_func(Op, db(intAgentIpAddress)).

intAgentTransportDomain(Op) ->
    snmp_generic:variable_func(Op, db(intAgentTransportDomain)).

intAgentTransports(Op) ->
    snmp_generic:variable_func(Op, db(intAgentTransports)).


which_trap_transport(Domain) when (Domain =:= snmpUDPDomain) ->
    case which_transport(Domain, all) of
        {value, _} = VALUE ->
            VALUE;
        false ->
            which_transport(transportDomainUdpIpv4, all)
    end;
which_trap_transport(Domain) ->
    case which_transport(Domain, trap_sender) of
        {value, _} = VALUE ->
            VALUE;
        false ->
            which_transport(Domain, all)
    end.

which_req_transport(Domain) ->
    which_transport(Domain, req_responder).

which_transport(Domain, Kind) ->
    {value, Transports} = intAgentTransports(get),
    which_transport(Domain, Kind, Transports).

which_transport(_Domain, _Kind, []) ->
    false;
which_transport(Domain, Kind,
                [{Domain, _Addr, _Kind, _Opts} = Transport|_Transports])
  when (Kind =:= all) ->
    {value, Transport};
which_transport(Domain, Kind,
                [{Domain, _Addr, Kind, _Opts} = Transport|_Transports]) ->
    {value, Transport};
which_transport(Domain, Kind, [_Transport|Transports]) ->
    which_transport(Domain, Kind, Transports).

    

snmpEngineID(print) ->
    VarAndValue = [{snmpEngineID, snmpEngineID(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);
snmpEngineID(Op) ->
    snmp_generic:variable_func(Op, db(snmpEngineID)).

snmpEngineMaxMessageSize(print) ->
    VarAndValue = [{snmpEngineMaxMessageSize, snmpEngineMaxMessageSize(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);
snmpEngineMaxMessageSize(Op) ->
    snmp_generic:variable_func(Op, db(snmpEngineMaxMessageSize)).

snmpEngineBoots(print) ->
    VarAndValue = [{snmpEngineBoots, snmpEngineBoots(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);
snmpEngineBoots(Op) ->
    snmp_generic:variable_func(Op, db(snmpEngineBoots)).

snmpEngineTime(print) ->
    VarAndValue = [{snmpEngineTime, snmpEngineTime(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);
snmpEngineTime(get) ->
    {value, get_engine_time()}.

init_engine() ->
    case snmp_generic:variable_get(db(snmpEngineBoots)) of
	{value, Val} when Val < 2147483647 ->
	    snmp_generic:variable_set(db(snmpEngineBoots), Val+1);
	{value, _} ->
	    ok;
	undefined ->
	    snmp_generic:variable_set(db(snmpEngineBoots), 1)
    end,
    reset_engine_base().
    

reset_engine_base() ->
    ets:insert(snmp_agent_table, {snmp_engine_base, snmp_misc:now(sec)}).

get_engine_id() ->
    {value, EngineID} = snmpEngineID(get),
    EngineID.

get_engine_max_message_size() ->
    {value, MPS} = snmpEngineMaxMessageSize(get),
    MPS.

get_engine_time() ->
    [{_, EngineBase}] = ets:lookup(snmp_agent_table, snmp_engine_base),
    snmp_misc:now(sec) - EngineBase.
    
get_engine_boots() ->
    {value, Val} = snmpEngineBoots(get),
    Val.
    
set_engine_boots(Boots) ->
    snmp_generic:variable_func(set, Boots, db(snmpEngineBoots)).
    
set_engine_time(Time) ->
    Base = snmp_misc:now(sec) - Time,
    ets:insert(snmp_agent_table, {snmp_engine_base, Base}).


set_sname() ->
    set_sname(get(sname)).

set_sname(undefined) ->
    put(sname,conf);
set_sname(_) -> %% Keep it, if already set.
    ok.

%% ------------------------------------------------------------------

error(Reason) ->
    throw({error, Reason}).

config_err(F, A) ->
    snmpa_error:config_err("[FRAMEWORK-MIB]: " ++ F, A).

