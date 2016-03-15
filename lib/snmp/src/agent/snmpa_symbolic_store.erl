%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(snmpa_symbolic_store).

%%----------------------------------------------------------------------
%% This module implements a multipurpose symbolic store.
%% 1) For internal and use from application: aliasname_to_value/1.
%%    If this was stored in the mib, deadlock would occur.
%% 2 table_info/1. Getting information about a table. Used by default 
%%    implementation of tables.
%% 3) variable_info/1. Used by default implementation of variables.
%% 4) notification storage. Used by snmpa_trap.
%% There is one symbolic store per node and it uses the ets table
%% snmp_agent_table, owned by the snmpa_supervisor.
%%
%%----------------------------------------------------------------------

-include_lib("kernel/include/file.hrl").
-include("snmp_types.hrl").
-include("snmp_debug.hrl").
-include("snmp_verbosity.hrl").
-include("SNMPv2-MIB.hrl").


%% API
-export([start_link/2, 
	 stop/0,
	 info/0, 
	 backup/1, 
	 aliasname_to_oid/1, oid_to_aliasname/1, 
	 add_aliasnames/2, delete_aliasnames/1,
	 which_aliasnames/0,
	 which_tables/0, 
	 which_variables/0, 
	 enum_to_int/2, int_to_enum/2, 
	 table_info/1, add_table_infos/2, delete_table_infos/1,
	 variable_info/1, add_variable_infos/2, delete_variable_infos/1,
	 get_notification/1, set_notification/2, 
	 delete_notifications/1, which_notifications/0,
	 add_types/2, delete_types/1]).

%% API (for quick access to the db, note that this is only reads).
-export([get_db/0,
	 aliasname_to_oid/2, oid_to_aliasname/2, 
	 enum_to_int/3, int_to_enum/3]).


%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-export([verbosity/1]).

-define(SERVER, ?MODULE).

-ifdef(snmp_debug).
-define(GS_START_LINK(Prio, Opts),
        gen_server:start_link({local, ?SERVER}, ?MODULE, [Prio, Opts], 
			      [{debug,[trace]}])).
-else.
-define(GS_START_LINK(Prio, Opts),
        gen_server:start_link({local, ?SERVER}, ?MODULE, [Prio, Opts], [])).
-endif.
  
-record(state,  {module, db, backup}).
-record(symbol, {key, mib_name, info}).


%%-----------------------------------------------------------------
%% Func: start_link/1
%% Args: Prio is priority of mib-server
%%       Opts is a list of options
%% Purpose: starts the mib server synchronized
%% Returns: {ok, Pid} | {error, Reason}
%%-----------------------------------------------------------------
start_link(Prio, Opts) ->
    ?d("start_link -> entry with"
	"~n   Prio: ~p"
	"~n   Opts: ~p", [Prio,Opts]),
    ?GS_START_LINK(Prio,Opts).

stop() ->
    call(stop).

info() ->
    call(info).

backup(BackupDir) ->
    call({backup, BackupDir}).


%%----------------------------------------------------------------------
%% Returns: Db
%%----------------------------------------------------------------------

get_db() ->
    call(get_db).

which_module() ->
    call(which_module).


%%----------------------------------------------------------------------
%% Returns: {value, Oid} | false
%%----------------------------------------------------------------------
aliasname_to_oid(Aliasname) ->
    call({aliasname_to_oid, Aliasname}).

oid_to_aliasname(OID) ->
    call({oid_to_aliasname, OID}).

int_to_enum(TypeOrObjName, Int) ->
    call({int_to_enum,TypeOrObjName,Int}).

enum_to_int(TypeOrObjName, Enum) ->
    call({enum_to_int,TypeOrObjName,Enum}).

add_types(MibName, Types) ->
    cast({add_types, MibName, Types}).

delete_types(MibName) ->
    cast({delete_types, MibName}).

add_aliasnames(MibName, MEs) ->
    cast({add_aliasnames, MibName, MEs}).

delete_aliasnames(MibName) ->
    cast({delete_aliasname, MibName}).

which_aliasnames() ->
    call(which_aliasnames).

which_tables() ->
    call(which_tables).

which_variables() ->
    call(which_variables).


%%----------------------------------------------------------------------
%% Returns: false|{value, Info}
%%----------------------------------------------------------------------
table_info(TableName) ->
    call({table_info, TableName}).


%%----------------------------------------------------------------------
%% Returns: false|{value, Info}
%%----------------------------------------------------------------------
variable_info(VariableName) ->
    call({variable_info, VariableName}).

add_table_infos(MibName, TableInfos) ->
    cast({add_table_infos, MibName, TableInfos}).

delete_table_infos(MibName) ->
    cast({delete_table_infos, MibName}).

add_variable_infos(MibName, VariableInfos) ->
    cast({add_variable_infos, MibName, VariableInfos}).

delete_variable_infos(MibName) ->
    cast({delete_variable_infos, MibName}).


%%-----------------------------------------------------------------
%% Store traps
%%-----------------------------------------------------------------
%% A notification is stored as {Key, Value}, where
%% Key is the symbolic trap name, and Value is 
%% a #trap record.
%%-----------------------------------------------------------------
%% Returns: {value, Val} | undefined
%%-----------------------------------------------------------------
get_notification(Key) ->
    call({get_notification, Key}).
set_notification(Trap, MibName) ->
    call({set_notification, MibName, Trap}).
delete_notifications(MibName) ->
    call({delete_notifications, MibName}).
which_notifications() ->
    call(which_notifications).


verbosity(Verbosity) -> 
    cast({verbosity,Verbosity}).


%%----------------------------------------------------------------------
%% DB access (read) functions: Returns: {value, Oid} | false
%%----------------------------------------------------------------------

aliasname_to_oid(Db, Aliasname) ->
    Mod = which_module(), 
    aliasname_to_oid(Mod, Db, Aliasname).

aliasname_to_oid(Mod, Db, Aliasname) ->
    case Mod:read(Db, {alias, Aliasname}) of
	{value,#symbol{info = {Oid, _Enums}}} -> {value, Oid};
	false -> false
    end.

oid_to_aliasname(Db, Oid) ->
    Mod = which_module(), 
    oid_to_aliasname(Mod, Db, Oid).

oid_to_aliasname(Mod, Db, Oid) ->
    case Mod:read(Db, {oid, Oid}) of
	{value,#symbol{info = Aliasname}} -> {value, Aliasname};
	_ -> false
    end.

which_notifications(Mod, Db) ->
    Pattern = #symbol{key = {trap, '_'}, _ = '_'},
    Symbols = Mod:match_object(Db, Pattern),
    [{Name, Mib, Rec} || #symbol{key      = {trap, Name}, 
				 mib_name = Mib, 
				 info     = Rec} <- Symbols].

which_aliasnames(Mod, Db) ->
    Pattern = #symbol{key = {alias, '_'}, _ = '_'},
    Symbols = Mod:match_object(Db, Pattern),
    [Alias || #symbol{key = {alias, Alias}} <- Symbols].

which_tables(Mod, Db) ->
    Pattern = #symbol{key = {table_info, '_'}, _ = '_'},
    Symbols = Mod:match_object(Db, Pattern),
    [Name || #symbol{key = {table_info, Name}} <- Symbols].

which_variables(Mod, Db) ->
    Pattern = #symbol{key = {variable_info, '_'}, _ = '_'},
    Symbols = Mod:match_object(Db, Pattern),
    [Name || #symbol{key = {variable_info, Name}} <- Symbols].


int_to_enum(Db, TypeOrObjName, Int) ->
    Mod = which_module(), 
    int_to_enum(Mod, Db, TypeOrObjName, Int).

int_to_enum(Mod, Db, TypeOrObjName, Int) ->
    case Mod:read(Db, {alias, TypeOrObjName}) of
	{value,#symbol{info = {_Oid, Enums}}} ->
	    case lists:keysearch(Int, 2, Enums) of
		{value, {Enum, _Int}} -> {value, Enum};
		false -> false
	    end;
	false -> % Not an Aliasname ->
	    case Mod:read(Db, {type, TypeOrObjName}) of
		{value,#symbol{info = Enums}} ->
		    case lists:keysearch(Int, 2, Enums) of
			{value, {Enum, _Int}} -> {value, Enum};
			false -> false
		    end;
		false ->
		    false
	    end
    end.

enum_to_int(Db, TypeOrObjName, Enum) ->
    Mod = which_module(), 
    enum_to_int(Mod, Db, TypeOrObjName, Enum).

enum_to_int(Mod, Db, TypeOrObjName, Enum) ->
    case Mod:read(Db, {alias, TypeOrObjName}) of
	{value,#symbol{info = {_Oid, Enums}}} ->
	    case lists:keysearch(Enum, 1, Enums) of
		{value, {_Enum, Int}} -> {value, Int};
		false -> false
	    end;
	false -> % Not an Aliasname
	    case Mod:read(Db, {type, TypeOrObjName}) of
		{value,#symbol{info = Enums}} ->
		    case lists:keysearch(Enum, 1, Enums) of
			{value, {_Enum, Int}} -> {value, Int};
			false -> false
		    end;
		false ->
		    false
	    end
    end.


%%----------------------------------------------------------------------
%% DB access (read) functions: Returns: false|{value, Info}
%%----------------------------------------------------------------------

table_info(Mod, Db, TableName) ->
    case Mod:read(Db, {table_info, TableName}) of
	{value,#symbol{info = Info}} -> {value, Info};
	false -> false
    end.


%%----------------------------------------------------------------------
%% DB access (read) functions: Returns: false|{value, Info}
%%----------------------------------------------------------------------
variable_info(Mod, Db, VariableName) ->
    case Mod:read(Db, {variable_info, VariableName}) of
	{value,#symbol{info = Info}} -> {value, Info};
	false -> false
    end.


%%----------------------------------------------------------------------
%% Implementation
%%----------------------------------------------------------------------

init([Prio, Opts]) ->
    ?d("init -> entry with"
	"~n   Prio: ~p"
	"~n   Opts: ~p", [Prio,Opts]),
    case (catch do_init(Prio, Opts)) of
	{ok, State} ->
	    {ok, State};
	Error ->
	    config_err("failed starting symbolic-store: ~n~p", [Error]),
	    {stop, {error, Error}}
    end.

do_init(Prio, Opts) ->
    process_flag(priority, Prio),
    process_flag(trap_exit, true),
    put(sname,ss),
    put(verbosity,get_verbosity(Opts)),
    ?vlog("starting",[]),
    MibStorage = get_mib_storage(Opts),
    Mod        = snmp_misc:get_option(module,  MibStorage), 
    MsOpts     = snmp_misc:get_option(options, MibStorage, []), 
    
    %% type = bag solves the problem with import and multiple
    %% object/type definitions.
    case Mod:open(?MODULE, symbol, record_info(fields, symbol), bag, MsOpts) of
	{ok, Db} ->
	    S  = #state{module = Mod, db = Db},
	    ?vdebug("started",[]),
	    {ok, S};
	{error, _} = ERROR ->
	    ERROR
    end.


handle_call(get_db, _From, #state{db = DB} = S) ->
    ?vlog("get db",[]),
    {reply, DB, S};

handle_call(which_module, _From, #state{module = Mod} = S) ->
    ?vlog("which module",[]),
    {reply, Mod, S};

handle_call({table_info, TableName}, _From, 
	    #state{module = Mod, db = DB} = S) ->
    ?vlog("table info: ~p",[TableName]),
    Res = table_info(Mod, DB, TableName),
    ?vdebug("table info result: ~p",[Res]),
    {reply, Res, S};

handle_call({variable_info, VariableName}, _From, 
	    #state{module = Mod, db = DB} = S) ->
    ?vlog("variable info: ~p",[VariableName]),
    Res = variable_info(Mod, DB, VariableName),
    ?vdebug("variable info result: ~p",[Res]),
    {reply, Res, S};

handle_call({aliasname_to_oid, Aliasname}, _From, 
	    #state{module = Mod, db = DB} = S) ->
    ?vlog("aliasname to oid: ~p",[Aliasname]),
    Res = aliasname_to_oid(Mod, DB, Aliasname),
    ?vdebug("aliasname to oid result: ~p",[Res]),
    {reply, Res, S};

handle_call({oid_to_aliasname, Oid}, _From, 
	    #state{module = Mod, db = DB} = S) ->
    ?vlog("oid to aliasname: ~p",[Oid]),
    Res = oid_to_aliasname(Mod, DB, Oid),
    ?vdebug("oid to aliasname result: ~p",[Res]),
    {reply, Res, S};

handle_call(which_aliasnames, _From, 
	    #state{module = Mod, db = DB} = S) ->
    ?vlog("which aliasnames",[]),
    Res = which_aliasnames(Mod, DB),
    ?vdebug("which aliasnames: ~p",[Res]),
    {reply, Res, S};

handle_call(which_tables, _From, 
	    #state{module = Mod, db = DB} = S) ->
    ?vlog("which tables",[]),
    Res = which_tables(Mod, DB),
    ?vdebug("which tables: ~p",[Res]),
    {reply, Res, S};

handle_call(which_variables, _From, 
	    #state{module = Mod, db = DB} = S) ->
    ?vlog("which variables",[]),
    Res = which_variables(Mod, DB),
    ?vdebug("which variables: ~p",[Res]),
    {reply, Res, S};

handle_call({enum_to_int, TypeOrObjName, Enum}, _From, 
	    #state{module = Mod, db = DB} = S) ->
    ?vlog("enum to int: ~p, ~p",[TypeOrObjName,Enum]),
    Res = enum_to_int(Mod, DB, TypeOrObjName, Enum),
    ?vdebug("enum to int result: ~p",[Res]),
    {reply, Res, S};

handle_call({int_to_enum, TypeOrObjName, Int}, _From, 
	    #state{module = Mod, db = DB} = S) ->
    ?vlog("int to enum: ~p, ~p",[TypeOrObjName,Int]),
    Res = int_to_enum(Mod, DB, TypeOrObjName, Int),
    ?vdebug("int to enum result: ~p",[Res]),
    {reply, Res, S};

handle_call({set_notification, MibName, Trap}, _From, 
	    #state{module = Mod, db = DB} = S) ->
    ?vlog("set notification:"
	  "~n   ~p~n   ~p", [MibName,Trap]),
    set_notif(Mod, DB, MibName, Trap),
    {reply, true, S};

handle_call({delete_notifications, MibName}, _From, 
	    #state{module = Mod, db = DB} = S) ->
    ?vlog("delete notification: ~p",[MibName]),
    delete_notif(Mod, DB, MibName),
    {reply, true, S};

handle_call(which_notifications, _From, 
	    #state{module = Mod, db = DB} = S) ->
    ?vlog("which notifications", []),
    Reply = which_notifications(Mod, DB), 
    {reply, Reply, S};

handle_call({get_notification, Key}, _From, 
	    #state{module = Mod, db = DB} = S) ->
    ?vlog("get notification: ~p",[Key]),
    Res = get_notif(Mod, DB, Key),
    ?vdebug("get notification result: ~p",[Res]),
    {reply, Res, S};

handle_call(info, _From, #state{module = Mod, db = DB} = S) ->
    ?vlog("info",[]),
    Info = get_info(Mod, DB), 
    {reply, Info, S};

handle_call({backup, BackupDir}, From, #state{module = Mod, db = DB} = S) ->
    ?vlog("info to ~p",[BackupDir]),
    Pid = self(),
    V   = get(verbosity),
    case file:read_file_info(BackupDir) of
	{ok, #file_info{type = directory}} ->
	    BackupServer = 
		erlang:spawn_link(
		  fun() ->
			  put(sname, albs),
			  put(verbosity, V),
			  Dir   = filename:join([BackupDir]), 
			  Reply = Mod:backup(DB, Dir),
			  Pid ! {backup_done, Reply},
			  unlink(Pid)
		  end),	
	    ?vtrace("backup server: ~p", [BackupServer]),
	    {noreply, S#state{backup = {BackupServer, From}}};
	{ok, _} ->
	    {reply, {error, not_a_directory}, S};
	Error ->
	    {reply, Error, S}
    end;

handle_call(stop, _From, S) -> 
    ?vlog("stop",[]),
    {stop, normal, ok, S};

handle_call(Req, _From, S) -> 
    info_msg("received unknown request: ~n~p", [Req]),
    Reply = {error, {unknown, Req}}, 
    {reply, Reply, S}.


handle_cast({add_types, MibName, Types}, #state{module = Mod, db = DB} = S) ->
    ?vlog("add types for ~p:",[MibName]),
    F = fun(#asn1_type{assocList = Alist, aliasname = Name}) ->
		case snmp_misc:assq(enums, Alist) of
		    {value, Es} ->
			?vlog("add type~n   ~p -> ~p",[Name,Es]),
			Rec = #symbol{key      = {type, Name}, 
				      mib_name = MibName, 
				      info     = Es},
			Mod:write(DB, Rec);
		    false -> done
		end
	end,
    lists:foreach(F, Types),
    {noreply, S};

handle_cast({delete_types, MibName}, #state{module = Mod, db = DB} = S) ->
    ?vlog("delete types: ~p",[MibName]),
    Pattern = #symbol{key = {type, '_'}, mib_name = MibName, info = '_'},
    Mod:match_delete(DB, Pattern),
    {noreply, S};

handle_cast({add_aliasnames, MibName, MEs}, 
	    #state{module = Mod, db = DB} = S) ->
    ?vlog("add aliasnames for ~p:",[MibName]),
    F = fun(#me{aliasname = AN, oid = Oid, asn1_type = AT}) ->
		Enums =
		    case AT of
			#asn1_type{assocList = Alist} -> 
			    case lists:keysearch(enums, 1, Alist) of
				{value, {enums, Es}} -> Es;
				_ -> []
			    end;
			_ -> []
		    end,
		write_alias(Mod, AN, DB, Enums, MibName, Oid)
	end,
    lists:foreach(F, MEs),
    {noreply, S};

handle_cast({delete_aliasname, MibName}, #state{module = Mod, db = DB} = S) ->
    ?vlog("delete aliasname: ~p",[MibName]),
    Pattern1 = #symbol{key = {alias, '_'}, mib_name = MibName, info = '_'},
    Mod:match_delete(DB, Pattern1),
    Pattern2 = #symbol{key = {oid, '_'}, mib_name = MibName, info = '_'},
    Mod:match_delete(DB, Pattern2),
    {noreply, S};

handle_cast({add_table_infos, MibName, TableInfos}, 
	    #state{module = Mod, db = DB} = S) ->
    ?vlog("add table infos for ~p:",[MibName]),
    F = fun({Name, TableInfo}) ->
		?vlog("add table info~n   ~p -> ~p",
		      [Name, TableInfo]),
		Rec = #symbol{key      = {table_info, Name}, 
			      mib_name = MibName, 
			      info     = TableInfo},
		Mod:write(DB, Rec)
	end,
    lists:foreach(F, TableInfos),
    {noreply, S};

handle_cast({delete_table_infos, MibName}, 
	    #state{module = Mod, db = DB} = S) ->
    ?vlog("delete table infos: ~p",[MibName]),
    Pattern = #symbol{key = {table_info, '_'}, mib_name = MibName, info = '_'},
    Mod:match_delete(DB, Pattern),
    {noreply, S};

handle_cast({add_variable_infos, MibName, VariableInfos}, 
	    #state{module = Mod, db = DB} = S) ->
    ?vlog("add variable infos for ~p:",[MibName]),
    F = fun({Name, VariableInfo}) ->
		?vlog("add variable info~n   ~p -> ~p",
		      [Name,VariableInfo]),
		Rec = #symbol{key      = {variable_info, Name},
			      mib_name = MibName,
			      info     = VariableInfo},
		Mod:write(DB, Rec)
	end,
    lists:foreach(F, VariableInfos),
    {noreply, S};

handle_cast({delete_variable_infos, MibName}, 
	    #state{module = Mod, db = DB} = S) ->
    ?vlog("delete variable infos: ~p",[MibName]),
    Pattern = #symbol{key      = {variable_info,'_'}, 
		      mib_name = MibName, 
		      info     = '_'},
    Mod:match_delete(DB, Pattern),
    {noreply, S};

handle_cast({verbosity,Verbosity}, State) ->
    ?vlog("verbosity: ~p -> ~p",[get(verbosity),Verbosity]),
    put(verbosity,snmp_verbosity:validate(Verbosity)),
    {noreply, State};
    
handle_cast(Msg, S) ->
    info_msg("received unknown message: ~n~p", [Msg]),
    {noreply, S}.
    

handle_info({'EXIT', Pid, Reason}, #state{backup = {Pid, From}} = S) ->
    ?vlog("backup server (~p) exited for reason ~n~p", [Pid, Reason]),
    gen_server:reply(From, {error, Reason}),
    {noreply, S#state{backup = undefined}};

handle_info({'EXIT', Pid, Reason}, S) ->
    %% The only other processes we should be linked to are
    %% either the master agent or our supervisor, so die...
    {stop, {received_exit, Pid, Reason}, S};

handle_info({backup_done, Reply}, #state{backup = {_, From}} = S) ->
    ?vlog("backup done:"
	  "~n   Reply: ~p", [Reply]),
    gen_server:reply(From, Reply),
    {noreply, S#state{backup = undefined}};

handle_info(Info, S) ->
    info_msg("received unknown info: ~n~p", [Info]),    
    {noreply, S}.


terminate(Reason, #state{module = Mod, db = DB}) ->
    ?vlog("terminate: ~p", [Reason]),
    Mod:close(DB).


%%----------------------------------------------------------
%% Code change
%%----------------------------------------------------------

% downgrade
%% code_change({down, _Vsn}, #state{db = DB, backup = B}, downgrade_to_pre_4_7) ->
%%     ?d("code_change(down) -> entry", []),
%%     stop_backup_server(B),
%%     S = {state, DB},
%%     {ok, S};

%% % upgrade
%% code_change(_Vsn, S, upgrade_from_pre_4_7) ->
%%     ?d("code_change(up) -> entry", []),
%%     {state, DB} = S,
%%     S1 = #state{db = DB},
%%     {ok, S1};

code_change(_Vsn, S, _Extra) ->
    ?d("code_change -> entry [do nothing]", []),
    {ok, S}.


%% stop_backup_server(undefined) ->
%%     ok;
%% stop_backup_server({Pid, _}) when is_pid(Pid) ->
%%     exit(Pid, kill).


    
%%-----------------------------------------------------------------
%% Trap operations (write, read, delete)
%%-----------------------------------------------------------------
%% A notification is stored as {Key, Value}, where
%% Key is the symbolic trap name, and Value is 
%% a #trap or a #notification record.
%%-----------------------------------------------------------------
%% Returns: {value, Value} | undefined
%%-----------------------------------------------------------------
get_notif(Mod, Db, Key) ->
    case Mod:read(Db, {trap, Key}) of
	{value,#symbol{info = Value}} -> {value, Value};
	false -> undefined
    end.

set_notif(Mod, Db, MibName, Trap) when is_record(Trap, trap) ->
    #trap{trapname = Name} = Trap,
    Rec = #symbol{key = {trap, Name}, mib_name = MibName, info = Trap},
    %% convert old v1 trap to oid
    Oid = case Trap#trap.enterpriseoid of
	      ?snmp ->
		  ?snmpTraps ++ [Trap#trap.specificcode + 1];
	      Oid0 ->
		  Oid0 ++ [0, Trap#trap.specificcode]
	  end,
    write_alias(Mod, Name, Db, MibName, Oid),
    Mod:write(Db, Rec);
set_notif(Mod, Db, MibName, Trap) ->
    #notification{trapname = Name, oid = Oid} = Trap,
    Rec = #symbol{key = {trap, Name}, mib_name = MibName, info = Trap},
    write_alias(Mod, Name, Db, MibName, Oid),
    Mod:write(Db, Rec).

delete_notif(Mod, Db, MibName) ->
    Pattern = #symbol{key = {trap, '_'}, mib_name = MibName, info = '_'},
    Mod:match_delete(Db, Pattern).


write_alias(Mod, AN, DB, MibName, Oid) ->
    write_alias(Mod, AN, DB, [], MibName, Oid).

write_alias(Mod, AN, DB, Enums, MibName, Oid) ->
    ?vlog("add alias~n   ~p -> {~p,~p}",[AN, Oid, Enums]),
    Rec1 = #symbol{key      = {alias, AN}, 
		   mib_name = MibName, 
		   info     = {Oid,Enums}},
    Mod:write(DB, Rec1),
    ?vlog("add oid~n   ~p -> ~p",[Oid, AN]),
    Rec2 = #symbol{key      = {oid, Oid}, 
		   mib_name = MibName, 
		   info     = AN},
    Mod:write(DB, Rec2).


%% -------------------------------------

get_info(Mod, DB) ->
    ProcSize = proc_mem(self()),
    DbMemory = Mod:info(DB, memory), 
    [{process_memory, ProcSize}, {db_memory, DbMemory}].

proc_mem(P) when is_pid(P) ->
    case (catch erlang:process_info(P, memory)) of
	{memory, Sz} when is_integer(Sz) ->
	    Sz;
	_ ->
	    undefined
    end.


%% -------------------------------------

get_verbosity(L) -> 
    snmp_misc:get_option(verbosity, L, ?default_verbosity).

get_mib_storage(L) -> 
    snmp_misc:get_option(mib_storage, L).


%% -------------------------------------

call(Req) ->
    call(Req, infinity).

call(Req, Timeout) ->
    gen_server:call(?SERVER, Req, Timeout).

cast(Msg) ->
    gen_server:cast(?SERVER, Msg).


%% ----------------------------------------------------------------

info_msg(F, A) ->
    error_logger:info_msg("~w: " ++ F ++ "~n", [?MODULE|A]).

config_err(F, A) ->
    snmpa_error:config_err(F, A).
 
