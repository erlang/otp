%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
-module(snmp_standard_mib).

%%%-----------------------------------------------------------------
%%% This module implements the configure- and reinit-functions
%%% for the STANDARD-MIB and SNMPv2-MIB.
%%%-----------------------------------------------------------------

-include("snmp_types.hrl").
-include("STANDARD-MIB.hrl").

-define(VMODULE,"STANDARD-MIB").
-include("snmp_verbosity.hrl").

-define(enabled, 1).
-define(disabled, 2).

%% External exports
-export([configure/1, reconfigure/1, reset/0, sys_up_time/0, sys_up_time/1,
	 snmp_enable_authen_traps/1, snmp_enable_authen_traps/2,
	 sys_object_id/1, sys_object_id/2, sys_or_table/3,
	 variable_func/1, variable_func/2,
	 inc/1, inc/2]).
-export([dummy/1, snmp_set_serial_no/1, snmp_set_serial_no/2]).
-export([add_agent_caps/2, del_agent_caps/1, get_agent_caps/0]).
-export([check_standard/1]).


%%-----------------------------------------------------------------
%% Func: configure/1
%% Args: Dir is the directory with trailing dir_separator where
%%       the configuration files can be found.
%% Purpose: Reads the config-files for the standard mib, and
%%          inserts the data.  Persistent data that is already
%%          present is *not* changed!  (use reconfigure for that)
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
configure(Dir) ->
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
    end.

do_configure(Dir) ->
    case snmpa_agent:get_agent_mib_storage() of
        mnesia ->
            ok;
        _ ->
            Standard = read_standard(Dir),
            lists:map(fun maybe_create_persistent_var/1, Standard)
    end,
    snmpa_local_db:variable_set({next_sys_or_index, volatile}, 1),
    %% sysORTable is always volatile
    snmp_generic:table_func(new, {sysORTable, volatile}),
    ok.


%%-----------------------------------------------------------------
%% Func: reconfigure/1
%% Args: Dir is the directory with trailing dir_separator where
%%       the configuration files can be found.
%% Purpose: Reads the config-files for the standard mib, and
%%          inserts the data.  Persistent data that is already
%%          present is deleted.  Makes sure the config file
%%          data is used.
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
reconfigure(Dir) ->
    set_sname(),
    case (catch do_reconfigure(Dir)) of
	ok ->
	    ok;
	{error, Reason} ->
	    ?vinfo("reconfigure error: ~p", [Reason]),
	    config_err("reconfigure failed: ~p", [Reason]),
	    exit(configuration_error);
	Error ->
	    ?vinfo("reconfigure failed: ~p", [Error]),
	    config_err("reconfigure failed: ~p", [Error]),
	    exit(configuration_error)
    end.

do_reconfigure(Dir) ->
    Standard = read_standard(Dir),
    lists:map(fun create_persistent_var/1, Standard),
    snmpa_local_db:variable_set({next_sys_or_index, volatile}, 1),
    snmp_generic:table_func(new, {sysORTable, volatile}),
    ok.


%%-----------------------------------------------------------------
%% Func: read_standard/1
%% Args: Dir is the directory with trailing dir_separator where
%%       the configuration files can be found.
%% Purpose: Reads th standard configuration file.
%% Returns: A list of standard variables
%% Fails: If an error occurs, the process will die with Reason
%%        configuration_error.
%%-----------------------------------------------------------------
read_standard(Dir) ->
    ?vdebug("check standard config file",[]),
    Gen    = fun(_) -> ok end,
    Filter = fun(Standard) -> sort_standard(Standard) end,
    Check  = fun(Entry) -> check_standard(Entry) end,
    [Standard] = 
	snmp_conf:read_files(Dir, [{Gen, Filter, Check, "standard.conf"}]), 
    Standard.


%%-----------------------------------------------------------------
%% Make sure that each mandatory standard attribute is present, and
%% provide default values for the other non-present attributes.
%%-----------------------------------------------------------------
sort_standard(L) ->
    Mand = [{sysContact, {value, ""}},
	    {sysDescr, {value, ""}},
	    {sysLocation, {value, ""}},
	    {sysName, {value, ""}},
	    {sysObjectID, mandatory},
	    {sysServices, mandatory},
	    {snmpEnableAuthenTraps, mandatory}],
    {ok, L2} = snmp_conf:check_mandatory(L, Mand),
    lists:keysort(1, L2).


%%-----------------------------------------------------------------
%%  Standard
%%  {Name, Value}.
%%-----------------------------------------------------------------
check_standard({sysDescr,    Value}) -> snmp_conf:check_string(Value);
check_standard({sysObjectID, Value}) -> snmp_conf:check_oid(Value);
check_standard({sysContact,  Value}) -> snmp_conf:check_string(Value);
check_standard({sysName,     Value}) -> snmp_conf:check_string(Value);
check_standard({sysLocation, Value}) -> snmp_conf:check_string(Value);
check_standard({sysServices, Value}) -> snmp_conf:check_integer(Value);
check_standard({snmpEnableAuthenTraps, Value}) ->
    Atoms = [{enabled,  ?snmpEnableAuthenTraps_enabled},
	     {disabled, ?snmpEnableAuthenTraps_disabled}],
    {ok, Val} = snmp_conf:check_atom(Value, Atoms),
    {ok, {snmpEnableAuthenTraps, Val}};
check_standard({Attrib, _Value}) -> error({unknown_attribute, Attrib});
check_standard(X) -> error({invalid_standard_specification, X}).


%%-----------------------------------------------------------------
%% Func: reset/0
%% Purpose: Resets all counters (sets them to 0).
%%-----------------------------------------------------------------
reset() ->
    snmpa_mpd:reset().

maybe_create_persistent_var({Var, Val}) ->
    VarDB = db(Var), 
    case snmp_generic:variable_get(VarDB) of
	{value, _} -> ok;
	_ -> snmp_generic:variable_set(VarDB, Val)
    end.

create_persistent_var({Var, Val}) ->
    snmp_generic:variable_set(db(Var), Val).

variable_func(_Op) -> ok.

variable_func(get, Name) ->
    [{_, Val}] = ets:lookup(snmp_agent_table, Name),
    {value, Val}.
    

%%-----------------------------------------------------------------
%%  inc(VariableName) increments the variable (Counter) in
%%  the local mib. (e.g. snmpInPkts)
%%-----------------------------------------------------------------
inc(Name) -> inc(Name, 1).
inc(Name, N) -> ets:update_counter(snmp_agent_table, Name, N).

%%-----------------------------------------------------------------
%% This is the instrumentation function for sysUpTime.
%%-----------------------------------------------------------------
sys_up_time() ->
    snmpa:sys_up_time().

sys_up_time(get) ->
    {value, snmpa:sys_up_time()}.

%%-----------------------------------------------------------------
%% This is the instrumentation function for snmpEnableAuthenTraps
%%-----------------------------------------------------------------
snmp_enable_authen_traps(new) ->
    snmp_generic:variable_func(new, db(snmpEnableAuthenTraps));

snmp_enable_authen_traps(delete) ->
    ok;

snmp_enable_authen_traps(get) ->
    snmp_generic:variable_func(get, db(snmpEnableAuthenTraps)).

snmp_enable_authen_traps(set, NewVal) ->
    snmp_generic:variable_func(set, NewVal, db(snmpEnableAuthenTraps)).

%%-----------------------------------------------------------------
%% This is the instrumentation function for sysObjectId
%%-----------------------------------------------------------------
sys_object_id(new) ->
    snmp_generic:variable_func(new, db(sysObjectID));

sys_object_id(delete) ->
    ok;

sys_object_id(get) ->
    snmp_generic:variable_func(get, db(sysObjectID)).

sys_object_id(set, NewVal) ->
    snmp_generic:variable_func(set, NewVal, db(sysObjectID)).

%%-----------------------------------------------------------------
%% This is a dummy instrumentation function for objects like
%% snmpTrapOID, that is accessible-for-notify, with different
%% values each time.  This function will only be called with
%% new/delete.
%%-----------------------------------------------------------------
dummy(_Op) -> ok.

%%-----------------------------------------------------------------
%% This is the instrumentation function for snmpSetSerialNo.
%% It is always volatile.
%%-----------------------------------------------------------------
snmp_set_serial_no(new) ->
    snmp_generic:variable_func(new, {snmpSetSerialNo, volatile}),
    {A1,A2,A3} = erlang:now(),
    random:seed(A1,A2,A3),
    Val = random:uniform(2147483648) - 1,
    snmp_generic:variable_func(set, Val, {snmpSetSerialNo, volatile});

snmp_set_serial_no(delete) ->
    ok;

snmp_set_serial_no(get) ->
    snmp_generic:variable_func(get, {snmpSetSerialNo, volatile}).

snmp_set_serial_no(is_set_ok, NewVal) ->
    case snmp_generic:variable_func(get, {snmpSetSerialNo, volatile}) of
	{value, NewVal} -> noError;
	_ -> inconsistentValue
    end;
snmp_set_serial_no(set, NewVal) ->
    snmp_generic:variable_func(set, (NewVal + 1) rem 2147483648,
			       {snmpSetSerialNo, volatile}).

%%-----------------------------------------------------------------
%% This is the instrumentation function for sysOrTable
%%-----------------------------------------------------------------
sys_or_table(Op, RowIndex, Cols) ->
    snmp_generic:table_func(Op, RowIndex, Cols, {sysORTable, volatile}).

add_agent_caps(Oid, Descr) when is_list(Oid) andalso is_list(Descr) ->
    {value, Next} = snmpa_local_db:variable_get({next_sys_or_index, volatile}),
    snmpa_local_db:variable_set({next_sys_or_index, volatile}, Next+1),
    SysUpTime = sys_up_time(),
    Row = {Next, Oid, Descr, SysUpTime},
    snmpa_local_db:table_create_row({sysORTable, volatile}, [Next], Row),
    snmpa_local_db:variable_set({sysORLastChange, volatile}, SysUpTime),
    Next.

del_agent_caps(Index) ->
    snmpa_local_db:table_delete_row({sysORTable, volatile}, [Index]),
    snmpa_local_db:variable_set({sysORLastChange, volatile}, sys_up_time()).

get_agent_caps() ->
    snmpa_local_db:match({sysORTable, volatile}, {'$1', '$2', '$3', '$4'}).


db(Var) -> snmpa_agent:db(Var).


%% -----

set_sname() ->
    set_sname(get(sname)).

set_sname(undefined) ->
    put(sname,conf);
set_sname(_) -> %% Keep it, if already set.
    ok.

error(Reason) ->
    throw({error, Reason}).

config_err(F, A) ->
    snmpa_error:config_err("[STANDARD-MIB]: " ++ F, A).
