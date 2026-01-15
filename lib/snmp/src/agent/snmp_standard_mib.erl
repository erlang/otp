%% 
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
-module(snmp_standard_mib).
-moduledoc """
Instrumentation Functions for STANDARD-MIB and SNMPv2-MIB

The module `snmp_standard_mib` implements the instrumentation functions for the
STANDARD-MIB and SNMPv2-MIB, and functions for configuring the database.

The configuration files are described in the SNMP User's Manual.
""".

%%%-----------------------------------------------------------------
%%% This module implements the configure- and reinit-functions
%%% for the STANDARD-MIB and SNMPv2-MIB.
%%%-----------------------------------------------------------------

-include("snmp_types.hrl").
-include("STANDARD-MIB.hrl").

-include("snmpa_internal.hrl").
-define(VMODULE,"STANDARD-MIB").
-include("snmp_verbosity.hrl").

-define(enabled, 1).
-define(disabled, 2).

%% External exports
%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([configure/1, reconfigure/1, reset/0, sys_up_time/0, sys_up_time/1,
	 snmp_enable_authen_traps/1, snmp_enable_authen_traps/2,
	 sys_object_id/1, sys_object_id/2, sys_or_table/3,
	 variable_func/1, variable_func/2,
	 inc/1, inc/2]).
-export([sysDescr/1, sysContact/1, sysName/1, sysLocation/1, 
	 sysServices/1, sysUpTime/1, snmpEnableAuthenTraps/1, 
	 sysObjectID/1, 
	 snmpInPkts/1, snmpOutPkts/1, 
	 snmpInBadVersions/1, 
	 snmpInBadCommunityNames/1, snmpInBadCommunityUses/1, 
	 snmpInASNParseErrs/1, 
	 snmpInTooBigs/1, 
	 snmpInNoSuchNames/1, snmpInBadValues/1,
	 snmpInReadOnlys/1, snmpInGenErrs/1, 
	 snmpInTotalReqVars/1, snmpInTotalSetVars/1, 
	 snmpInGetRequests/1, snmpInSetRequests/1, 
	 snmpInGetNexts/1, 
	 snmpInGetResponses/1, snmpInTraps/1,
	 snmpOutTooBigs/1,
	 snmpOutNoSuchNames/1,
	 snmpOutBadValues/1, 
	 snmpOutGenErrs/1,
	 snmpOutGetRequests/1, snmpOutSetRequests/1, 
	 snmpOutGetNexts/1,
	 snmpOutGetResponses/1,
	 snmpOutTraps/1]).
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

-doc """
This function is called from the supervisor at system start-up.

Inserts all data in the configuration files into the database and destroys all
old rows with StorageType `volatile`. The rows created from the configuration
file will have StorageType `nonVolatile`.

All `snmp` counters are set to zero.

If an error is found in the configuration file, it is reported using the
function `config_err/2` of the error report module, and the function fails with
the reason `configuration_error`.

`ConfDir` is a string which points to the directory where the configuration
files are found.

The configuration file read is: `standard.conf`.
""".
-spec configure(ConfDir) -> snmp:void() when
      ConfDir :: string().

configure(ConfDir) ->
    case (catch do_configure(ConfDir)) of
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

-doc """
Inserts all data in the configuration files into the database and destroys all
old data, including the rows with StorageType `nonVolatile`. The rows created
from the configuration file will have StorageType `nonVolatile`.

Thus, the data in the SNMP-STANDARD-MIB and SNMPv2-MIB, after this function has
been called, is from the configuration files.

All `snmp` counters are set to zero.

If an error is found in the configuration file, it is reported using the
function `config_err/2` of the error report module, and the function fails with
the reason `configuration_error`.

`ConfDir` is a string which points to the directory where the configuration
files are found.

The configuration file read is: `standard.conf`.
""".
-spec reconfigure(ConfDir) -> snmp:void() when
      ConfDir :: string().

reconfigure(ConfDir) ->
    set_sname(),
    case (catch do_reconfigure(ConfDir)) of
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
%% Purpose: Reads the standard configuration file.
%% Returns: A list of standard variables
%% Fails: If an error occurs, the process will die with Reason
%%        configuration_error.
%%        This file is mandatory, as it contains mandatory 
%%        config options for which there are no default values. 
%%-----------------------------------------------------------------
read_standard(Dir) ->
    ?vdebug("check standard config file",[]),
    FileName = "standard.conf",
    Gen =
	fun (D, Reason) ->
		throw(
		  {error,
		   {failed_reading_config_file,
		    D, FileName, list_dir(Dir), Reason}})
	end,
    Order  = fun snmp_conf:no_order/2,
    Check  = fun (Entry, State) -> {check_standard(Entry), State} end,
    Filter = fun sort_standard/1,
    [Standard] = 
	snmp_conf:read_files(Dir, [{FileName, Gen, Order, Check, Filter}]),
    Standard.

list_dir(Dir) ->
    case file:list_dir(Dir) of
	{ok, Files} ->
	    Files;
	Error ->
	    Error
    end.


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
-doc false.
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

-doc "Resets all `snmp` counters to 0.".
-spec reset() -> snmp:void().

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

-doc false.
variable_func(_Op) -> ok.

-doc false.
variable_func(get, Name) ->
    [{_, Val}] = ets:lookup(snmp_agent_table, Name),
    {value, Val}.
    

%%-----------------------------------------------------------------
%%  inc(VariableName) increments the variable (Counter) in
%%  the local mib. (e.g. snmpInPkts)
%%-----------------------------------------------------------------

-doc(#{equiv => inc/2}).
-spec inc(Name) -> snmp:void() when
      Name :: atom().

inc(Name) -> inc(Name, 1).

-doc "Increments a variable in the MIB with `N`, or one if `N` is not specified.".
-spec inc(Name, N) -> snmp:void() when
      Name :: atom(),
      N    :: integer().

inc(Name, N) -> ets:update_counter(snmp_agent_table, Name, N).


-doc false.
sysDescr(print) ->
    VarAndValue = [{sysDescr,  sysDescr(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);
    
sysDescr(get) ->
    VarDB = db(sysDescr), 
    snmp_generic:variable_get(VarDB).
    

-doc false.
sysContact(print) ->
    VarAndValue = [{sysContact,  sysContact(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);

sysContact(get) ->
    VarDB = db(sysContact), 
    snmp_generic:variable_get(VarDB).


-doc false.
sysName(print) ->
    VarAndValue = [{sysName,  sysName(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);

sysName(get) ->
    VarDB = db(sysName), 
    snmp_generic:variable_get(VarDB).


-doc false.
sysLocation(print) ->
    VarAndValue = [{sysLocation,  sysLocation(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);

sysLocation(get) ->
    VarDB = db(sysLocation), 
    snmp_generic:variable_get(VarDB).


-doc false.
sysServices(print) ->
    VarAndValue = [{sysServices,  sysServices(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);

sysServices(get) ->
    VarDB = db(sysServices), 
    snmp_generic:variable_get(VarDB).


-doc false.
snmpInPkts(print) ->
    gen_counter(print, snmpInPkts);
snmpInPkts(get) ->
    gen_counter(get, snmpInPkts).


-doc false.
snmpOutPkts(print) ->
    gen_counter(print, snmpOutPkts);
snmpOutPkts(get) ->
    gen_counter(get, snmpOutPkts).


-doc false.
snmpInASNParseErrs(print) ->
    gen_counter(print, snmpInASNParseErrs);
snmpInASNParseErrs(get) ->
    gen_counter(get, snmpInASNParseErrs).
    

-doc false.
snmpInBadCommunityNames(print) ->
    gen_counter(print, snmpInBadCommunityNames);
snmpInBadCommunityNames(get) ->
    gen_counter(get, snmpInBadCommunityNames).
    

-doc false.
snmpInBadCommunityUses(print) ->
    gen_counter(print, snmpInBadCommunityUses);

snmpInBadCommunityUses(get) ->
    gen_counter(get, snmpInBadCommunityUses).
    

-doc false.
snmpInBadVersions(print) ->
    gen_counter(print, snmpInBadVersions);
snmpInBadVersions(get) ->
    gen_counter(get, snmpInBadVersions).
    

-doc false.
snmpInTooBigs(print) ->
    gen_counter(print, snmpInTooBigs);
snmpInTooBigs(get) ->
    gen_counter(get, snmpInTooBigs).
    

-doc false.
snmpInNoSuchNames(print) ->
    gen_counter(print, snmpInNoSuchNames);
snmpInNoSuchNames(get) ->
    gen_counter(get, snmpInNoSuchNames).
    

-doc false.
snmpInBadValues(print) ->
    gen_counter(print, snmpInBadValues);
snmpInBadValues(get) ->
    gen_counter(get, snmpInBadValues).
    

-doc false.
snmpInReadOnlys(print) ->
    gen_counter(print, snmpInReadOnlys);
snmpInReadOnlys(get) ->
    gen_counter(get, snmpInReadOnlys).
    

-doc false.
snmpInGenErrs(print) ->
    gen_counter(print, snmpInGenErrs);
snmpInGenErrs(get) ->
    gen_counter(get, snmpInGenErrs).
    

-doc false.
snmpInTotalReqVars(print) ->
    gen_counter(print, snmpInTotalReqVars);
snmpInTotalReqVars(get) ->
    gen_counter(get, snmpInTotalReqVars).
    

-doc false.
snmpInTotalSetVars(print) ->
    gen_counter(print, snmpInTotalSetVars);
snmpInTotalSetVars(get) ->
    gen_counter(get, snmpInTotalSetVars).
    

-doc false.
snmpInGetRequests(print) ->
    gen_counter(print, snmpInGetRequests);
snmpInGetRequests(get) ->
    gen_counter(get, snmpInGetRequests).
    

-doc false.
snmpInSetRequests(print) ->
    gen_counter(print, snmpInSetRequests);
snmpInSetRequests(get) ->
    gen_counter(get, snmpInSetRequests).
    

-doc false.
snmpInGetNexts(print) ->
    gen_counter(print, snmpInGetNexts);
snmpInGetNexts(get) ->
    gen_counter(get, snmpInGetNexts).
    

-doc false.
snmpInGetResponses(print) ->
    gen_counter(print, snmpInGetResponses);
snmpInGetResponses(get) ->
    gen_counter(get, snmpInGetResponses).
    

-doc false.
snmpInTraps(print) ->
    gen_counter(print, snmpInTraps);
snmpInTraps(get) ->
    gen_counter(get, snmpInTraps).
    

-doc false.
snmpOutTooBigs(print) ->
    gen_counter(print, snmpOutTooBigs);
snmpOutTooBigs(get) ->
    gen_counter(get, snmpOutTooBigs).
    

-doc false.
snmpOutNoSuchNames(print) ->
    gen_counter(print, snmpOutNoSuchNames);
snmpOutNoSuchNames(get) ->
    gen_counter(get, snmpOutNoSuchNames).
    

-doc false.
snmpOutBadValues(print) ->
    gen_counter(print, snmpOutBadValues);
snmpOutBadValues(get) ->
    gen_counter(get, snmpOutBadValues).
    

-doc false.
snmpOutGenErrs(print) ->
    gen_counter(print, snmpOutGenErrs);
snmpOutGenErrs(get) ->
    gen_counter(get, snmpOutGenErrs).
    

-doc false.
snmpOutGetRequests(print) ->
    gen_counter(print, snmpOutGetRequests);
snmpOutGetRequests(get) ->
    gen_counter(get, snmpOutGetRequests).
    

-doc false.
snmpOutSetRequests(print) ->
    gen_counter(print, snmpOutSetRequests);
snmpOutSetRequests(get) ->
    gen_counter(get, snmpOutSetRequests).
    

-doc false.
snmpOutGetNexts(print) ->
    gen_counter(print, snmpOutGetNexts);
snmpOutGetNexts(get) ->
    gen_counter(get, snmpOutGetNexts).
    

-doc false.
snmpOutGetResponses(print) ->
    gen_counter(print, snmpOutGetResponses);
snmpOutGetResponses(get) ->
    gen_counter(get, snmpOutGetResponses).
    

-doc false.
snmpOutTraps(print) ->
    gen_counter(print, snmpOutTraps);
snmpOutTraps(get) ->
    gen_counter(get, snmpOutTraps).
    

gen_counter(print, Counter) ->
    Val         = gen_counter(get, Counter), 
    VarAndValue = [{Counter, Val}],
    snmpa_mib_lib:print_variables(VarAndValue);
    
gen_counter(get, Counter) ->
    variable_func(get, Counter).


%%-----------------------------------------------------------------
%% This is the instrumentation function for sysUpTime.
%%-----------------------------------------------------------------

-doc "Gets the system up time in hundredth of a second.".
-spec sys_up_time() -> Time when
      Time :: integer().

sys_up_time() ->
    snmpa:sys_up_time().


-doc false.
sysUpTime(print) ->
    sys_up_time(print);
sysUpTime(get) ->
    sys_up_time(get).

-doc false.
sys_up_time(print) ->
    VarAndValue = [{sysUpTime,  sys_up_time(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);

sys_up_time(get) ->
    {value, snmpa:sys_up_time()}.


%%-----------------------------------------------------------------
%% This is the instrumentation function for snmpEnableAuthenTraps
%%-----------------------------------------------------------------

-doc false.
snmpEnableAuthenTraps(print) ->
    snmp_enable_authen_traps(print);
snmpEnableAuthenTraps(get) ->
    snmp_enable_authen_traps(get).


-doc false.
snmp_enable_authen_traps(print) ->
    VarAndValue = [{snmpEnableAuthenTraps,  snmp_enable_authen_traps(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);
    
snmp_enable_authen_traps(new) ->
    snmp_generic:variable_func(new, db(snmpEnableAuthenTraps));

snmp_enable_authen_traps(delete) ->
    ok;

snmp_enable_authen_traps(get) ->
    snmp_generic:variable_func(get, db(snmpEnableAuthenTraps)).

-doc false.
snmp_enable_authen_traps(set, NewVal) ->
    snmp_generic:variable_func(set, NewVal, db(snmpEnableAuthenTraps)).


%%-----------------------------------------------------------------
%% This is the instrumentation function for sysObjectID
%%-----------------------------------------------------------------
-doc false.
sysObjectID(print) ->
    sys_object_id(print);
sysObjectID(get) ->
    sys_object_id(get).

-doc false.
sys_object_id(print) ->
    VarAndValue = [{sysObjectID,  sys_object_id(get)}],
    snmpa_mib_lib:print_variables(VarAndValue);

sys_object_id(new) ->
    snmp_generic:variable_func(new, db(sysObjectID));

sys_object_id(delete) ->
    ok;

sys_object_id(get) ->
    snmp_generic:variable_func(get, db(sysObjectID)).

-doc false.
sys_object_id(set, NewVal) ->
    snmp_generic:variable_func(set, NewVal, db(sysObjectID)).


%%-----------------------------------------------------------------
%% This is a dummy instrumentation function for objects like
%% snmpTrapOID, that is accessible-for-notify, with different
%% values each time.  This function will only be called with
%% new/delete.
%%-----------------------------------------------------------------
-doc false.
dummy(_Op) -> ok.


%%-----------------------------------------------------------------
%% This is the instrumentation function for snmpSetSerialNo.
%% It is always volatile.
%%-----------------------------------------------------------------
-doc false.
snmp_set_serial_no(new) ->
    snmp_generic:variable_func(new, {snmpSetSerialNo, volatile}),
    ?SNMP_RAND_SEED(),
    %% rand:seed(exrop,
    %%           {erlang:phash2([node()]),
    %%            erlang:monotonic_time(),
    %%            erlang:unique_integer()}),
    Val = rand:uniform(2147483648) - 1,
    snmp_generic:variable_func(set, Val, {snmpSetSerialNo, volatile});

snmp_set_serial_no(delete) ->
    ok;

snmp_set_serial_no(get) ->
    snmp_generic:variable_func(get, {snmpSetSerialNo, volatile}).

-doc false.
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
-doc false.
sys_or_table(Op, RowIndex, Cols) ->
    snmp_generic:table_func(Op, RowIndex, Cols, {sysORTable, volatile}).

-doc false.
add_agent_caps(Oid, Descr) when is_list(Oid) andalso is_list(Descr) ->
    {value, Next} = snmpa_local_db:variable_get({next_sys_or_index, volatile}),
    snmpa_local_db:variable_set({next_sys_or_index, volatile}, Next+1),
    SysUpTime = sys_up_time(),
    Row = {Next, Oid, Descr, SysUpTime},
    snmpa_local_db:table_create_row({sysORTable, volatile}, [Next], Row),
    snmpa_local_db:variable_set({sysORLastChange, volatile}, SysUpTime),
    Next.

-doc false.
del_agent_caps(Index) ->
    snmpa_local_db:table_delete_row({sysORTable, volatile}, [Index]),
    snmpa_local_db:variable_set({sysORLastChange, volatile}, sys_up_time()).

-doc false.
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
    snmpa_error:config_err("[STANDARD-MIB] " ++ F, A).
