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
-module(snmpa_local_db).

-include_lib("kernel/include/file.hrl").
-include("snmpa_internal.hrl").
-include("snmp_types.hrl").
-include("STANDARD-MIB.hrl").

%% -define(VMODULE, "LDB").
-include("snmp_verbosity.hrl").


%% External exports
-export([start_link/3, start_link/4, stop/0, info/0, verbosity/1]).
-export([dump/0, backup/1, 
	 register_notify_client/2, unregister_notify_client/1]).
-export([table_func/2, table_func/4,
	 variable_get/1, variable_set/2, variable_delete/1, variable_inc/2,
	 table_create/1, table_exists/1, table_delete/1,
         table_create_row/3, table_create_row/4, table_construct_row/4,
	 table_delete_row/2,
	 table_get_row/2,
         table_get_element/3, table_get_elements/4,
	 table_set_elements/3, table_set_status/7,
         table_next/2,
	 table_max_col/2,
	 table_get/1]).

-export([get_elements/2]).

-export([match/2]).

%% Debug exports
-export([print/0, print/1, print/2]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
	 code_change/3]).


-define(BACKUP_TAB, snmpa_local_backup).
-define(DETS_TAB,   snmpa_local_db1).
-define(ETS_TAB,    snmpa_local_db2).
-define(SERVER,     ?MODULE).

-record(state, {dets, ets, notify_clients = [], backup}).
-record(dets,  {tab, shadow}).

%% -define(snmp_debug,true).
-include("snmp_debug.hrl").


-ifdef(snmp_debug).
-define(GS_START_LINK(Prio, Dir, DbInitError, Opts),
        gen_server:start_link({local, ?SERVER}, ?MODULE, 
			      [Prio, Dir, DbInitError, Opts],
                              [{debug,[trace]}])).
-else.
-define(GS_START_LINK(Prio, Dir, DbInitError, Opts),
        gen_server:start_link({local, ?SERVER}, ?MODULE, 
			      [Prio, Dir, DbInitError, Opts], 
			      [])).
-endif.
 

%%%-----------------------------------------------------------------
%%% Implements a general database in which its possible
%%% to store variables and tables. Provide functions for
%%% tableaccess by row or by element, and for next.
%%%
%%% Opts = [Opt]
%%% Opt = {auto_repair, false | true | true_verbose} |
%%%       {verbosity,silence | log | debug}
%%%-----------------------------------------------------------------
start_link(Prio, DbDir, Opts) when is_list(Opts) ->
    start_link(Prio, DbDir, terminate, Opts).

start_link(Prio, DbDir, DbInitError, Opts) when is_list(Opts) ->
    ?d("start_link -> entry with"
	"~n   Prio:        ~p"
	"~n   DbDir:       ~p"
	"~n   DbInitError: ~p"
	"~n   Opts:        ~p", [Prio, DbDir, DbInitError, Opts]),
    ?GS_START_LINK(Prio, DbDir, DbInitError, Opts).

stop() ->
    call(stop).

register_notify_client(Client,Module) ->
    call({register_notify_client,Client,Module}).


unregister_notify_client(Client) ->
    call({unregister_notify_client,Client}).

backup(BackupDir) ->
    call({backup, BackupDir}).

dump() ->
    call(dump).

info() ->
    call(info).

verbosity(Verbosity) ->
    cast({verbosity,Verbosity}).


%%%-----------------------------------------------------------------

init([Prio, DbDir, DbInitError, Opts]) ->
    ?d("init -> entry with"
	"~n   Prio:        ~p"
	"~n   DbDir:       ~p"
	"~n   DbInitError: ~p"
	"~n   Opts:        ~p", [Prio, DbDir, DbInitError, Opts]),
    case (catch do_init(Prio, DbDir, DbInitError, Opts)) of
	{ok, State} ->
	    ?vdebug("started",[]),
	    {ok, State};
	{error, Reason} ->
	    config_err("failed starting local-db: ~n~p", [Reason]),
	    {stop, {error, Reason}};
	Error ->
	    config_err("failed starting local-db: ~n~p", [Error]),
	    {stop, {error, Error}}
    end.

do_init(Prio, DbDir, DbInitError, Opts) ->
    process_flag(priority, Prio),
    process_flag(trap_exit, true),
    put(sname,     get_opt(sname, Opts, ldb)),
    put(verbosity, get_opt(verbosity, Opts, ?default_verbosity)),
    ?vlog("starting",[]),
    Dets = dets_open(DbDir, DbInitError, Opts),
    Ets  = ets:new(?ETS_TAB, [set, protected]),
    ?vdebug("started",[]),
    put(started,   snmp_misc:formated_timestamp()),
    {ok, #state{dets = Dets, ets = Ets}}.

dets_open(DbDir, DbInitError, Opts) ->
    Name     = ?DETS_TAB, 
    Filename = dets_filename(Name, DbDir),
    case file:read_file_info(Filename) of
	{ok, _} ->
	    %% File exists
	    case do_dets_open(Name, Filename, Opts) of
		{ok, Dets} ->
		    ?vdebug("dets open done",[]),
		    Shadow = ets:new(snmp_local_db1_shadow, [set, protected]),
		    dets:to_ets(Dets, Shadow),
		    ?vtrace("shadow table created and populated",[]),
		    #dets{tab = Dets, shadow = Shadow};
		{error, Reason1} ->
                    user_err("Corrupt local database: ~p", [Filename]),
		    case DbInitError of
			terminate ->
			    throw({error, {failed_open_dets, Reason1}});
			_ ->
			    Saved = Filename ++ ".saved",
			    file:rename(Filename, Saved),
			    case do_dets_open(Name, Filename, Opts) of
				{ok, Dets} ->
				    Shadow = ets:new(snmp_local_db1_shadow, 
						     [set, protected]),
				    #dets{tab = Dets, shadow = Shadow};
				{error, Reason2} ->
				    user_err("Could not create local "
					     "database: ~p"
					     "~n   ~p"
					     "~n   ~p", 
					     [Filename, Reason1, Reason2]),
				    throw({error, {failed_open_dets, Reason2}})
			    end
		    end
	    end;
	_ ->
	    case DbInitError of
		create_db_and_dir ->
		    ok = filelib:ensure_dir(Filename);
		_ ->
		    ok
	    end,
	    case do_dets_open(Name, Filename, Opts) of
		{ok, Dets} ->
		    ?vdebug("dets open done",[]),
		    Shadow = ets:new(snmp_local_db1_shadow, [set, protected]),
		    ?vtrace("shadow table created",[]),
		    #dets{tab = Dets, shadow = Shadow};
		{error, Reason} ->
		    user_err("Could not create local database ~p"
			     "~n   ~p", [Filename, Reason]),
		    throw({error, {failed_open_dets, Reason}})
	    end
    end.

do_dets_open(Name, Filename, Opts) ->
    Repair   = get_opt(repair, Opts, true),
    AutoSave = get_opt(auto_save, Opts, 5000),
    Args = [{auto_save, AutoSave},
	    {file,      Filename},
	    {repair,    Repair}],
    dets:open_file(Name, Args).

    
dets_filename(Name, Dir) when is_atom(Name) ->
    dets_filename(atom_to_list(Name), Dir);
dets_filename(Name, Dir) ->
    filename:join(dets_filename1(Dir), Name).
    
dets_filename1([])  -> ".";
dets_filename1(Dir) -> Dir.


%%-----------------------------------------------------------------
%% Interface functions.
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Functions for debugging.
%%-----------------------------------------------------------------
print()          -> call(print).
print(Table)     -> call({print,Table,volatile}).
print(Table, Db) -> call({print,Table,Db}).

variable_get({Name, Db}) ->
    call({variable_get, Name, Db});
variable_get(Name) ->
    call({variable_get, Name, volatile}).

variable_set({Name, Db}, Val) ->
    call({variable_set, Name, Db, Val});
variable_set(Name, Val) ->
    call({variable_set, Name, volatile, Val}).

variable_inc({Name, Db}, N) ->
    cast({variable_inc, Name, Db, N});
variable_inc(Name, N) ->
    cast({variable_inc, Name, volatile, N}).

variable_delete({Name, Db}) ->
    call({variable_delete, Name, Db});
variable_delete(Name) ->
    call({variable_delete, Name, volatile}).


table_create({Name, Db}) ->
    call({table_create, Name, Db});
table_create(Name) ->
    call({table_create, Name, volatile}).

table_exists({Name, Db}) ->
    call({table_exists, Name, Db});
table_exists(Name) ->
    call({table_exists, Name, volatile}).

table_delete({Name, Db}) ->
    call({table_delete, Name, Db});
table_delete(Name) ->
    call({table_delete, Name, volatile}).

table_delete_row({Name, Db}, RowIndex) ->
    call({table_delete_row, Name, Db, RowIndex});
table_delete_row(Name, RowIndex) ->
    call({table_delete_row, Name, volatile, RowIndex}).

table_get_row({Name, Db}, RowIndex) ->
    call({table_get_row, Name, Db, RowIndex});
table_get_row(Name, RowIndex) ->
    call({table_get_row, Name, volatile, RowIndex}).

table_get_element({Name, Db}, RowIndex, Col) ->
    call({table_get_element, Name, Db, RowIndex, Col});
table_get_element(Name, RowIndex, Col) ->
    call({table_get_element, Name, volatile, RowIndex, Col}).

table_set_elements({Name, Db}, RowIndex, Cols) ->
    call({table_set_elements, Name, Db, RowIndex, Cols});
table_set_elements(Name, RowIndex, Cols) ->
    call({table_set_elements, Name, volatile, RowIndex, Cols}).

table_next({Name, Db}, RestOid) ->
    call({table_next, Name, Db, RestOid});
table_next(Name, RestOid) ->
    call({table_next, Name, volatile, RestOid}).

table_max_col({Name, Db}, Col) ->
    call({table_max_col, Name, Db, Col});
table_max_col(Name, Col) ->
    call({table_max_col, Name, volatile, Col}).

table_create_row({Name, Db}, RowIndex, Row) ->
    call({table_create_row, Name, Db,RowIndex, Row});
table_create_row(Name, RowIndex, Row) ->
    call({table_create_row, Name, volatile, RowIndex, Row}).
table_create_row(NameDb, RowIndex, Status, Cols) ->
    Row = table_construct_row(NameDb, RowIndex, Status, Cols),
    table_create_row(NameDb, RowIndex, Row).

match({Name, Db}, Pattern) ->
    call({match, Name, Db, Pattern});    
match(Name, Pattern) ->
    call({match, Name, volatile, Pattern}).


table_get(Table) ->
    table_get(Table, [], []).

table_get(Table, Idx, Acc) ->
    case table_next(Table, Idx) of
	endOfTable ->
            lists:reverse(Acc);
	NextIdx ->
	    case table_get_row(Table, NextIdx) of
		undefined ->
		    {error, {failed_get_row, NextIdx, lists:reverse(Acc)}};
		Row ->
		    NewAcc = [{NextIdx, Row}|Acc],
		    table_get(Table, NextIdx, NewAcc)
	    end
    end.


%%-----------------------------------------------------------------
%% Implements the variable functions.
%%-----------------------------------------------------------------
handle_call({variable_get, Name, Db}, _From, State) -> 
    ?vlog("variable get: ~p [~p]",[Name, Db]),
    {reply, lookup(Db, Name, State), State};

handle_call({variable_set, Name, Db, Val}, _From, State) -> 
    ?vlog("variable ~p set [~p]: "
	  "~n   Val:  ~p",[Name, Db, Val]),
    {reply, insert(Db, Name, Val, State), State};

handle_call({variable_delete, Name, Db}, _From, State) -> 
    ?vlog("variable delete: ~p [~p]",[Name, Db]),
    {reply, delete(Db, Name, State), State};


%%-----------------------------------------------------------------
%% Implements the table functions.
%%-----------------------------------------------------------------
%% Entry in ets for a tablerow:
%% Key = {<tableName>, <(flat) list of indexes>}
%% Val = {{Row}, <Prev>, <Next>}
%% Where Prev and Next = <list of indexes>; "pointer to prev/next"
%% Each table is a double linked list, with a head-element, with
%% direct access to each individual element.
%% Head-el: Key = {<tableName>, first}
%% Operations:
%% table_create_row(<tableName>, <list of indexes>, <row>)   O(n)
%% table_delete_row(<tableName>, <list of indexes>)          O(1)
%% get(<tableName>, <list of indexes>, Col)            O(1)
%% set(<tableName>, <list of indexes>, Col, Val)       O(1)
%% next(<tableName>, <list of indexes>)   if Row exist O(1), else O(n)
%%-----------------------------------------------------------------
handle_call({table_create, Name, Db}, _From, State) ->
    ?vlog("table create: ~p [~p]",[Name, Db]),
    catch handle_delete(Db, Name, State),
    {reply, insert(Db, {Name, first}, {undef, first, first}, State), State};

handle_call({table_exists, Name, Db}, _From, State) ->
    ?vlog("table exist: ~p [~p]",[Name, Db]),
    Res =
	case lookup(Db, {Name, first}, State) of
	    {value, _} -> true;
	    undefined -> false
	end,
    ?vdebug("table exist result: "
	    "~n   ~p",[Res]),
    {reply, Res, State};

handle_call({table_delete, Name, Db}, _From, State) ->
    ?vlog("table delete: ~p [~p]",[Name, Db]),
    catch handle_delete(Db, Name, State),
    {reply, true, State};

handle_call({table_create_row, Name, Db, Indexes, Row}, _From, State) ->
    ?vlog("table create row [~p]: "
	  "~n   Name:    ~p"
	  "~n   Indexes: ~p"
	  "~n   Row:     ~p",[Db, Name, Indexes, Row]),
    Res = 
	case catch handle_create_row(Db, Name, Indexes, Row, State) of
	    {'EXIT', _} -> false;
	    _ -> true
	end,
    ?vdebug("table create row result: "
	    "~n   ~p",[Res]),
    {reply, Res, State};

handle_call({table_delete_row, Name, Db, Indexes}, _From, State) ->
    ?vlog("table delete row [~p]: "
	  "~n   Name:    ~p"
	  "~n   Indexes: ~p", [Db, Name, Indexes]),
    Res = 
	case catch handle_delete_row(Db, Name, Indexes, State) of
	    {'EXIT', _} -> false;
	    _ -> true
	end,
    ?vdebug("table delete row result: "
	    "~n   ~p",[Res]),
    {reply, Res, State};

handle_call({table_get_row, Name, Db, Indexes}, _From, State) -> 
    ?vlog("table get row [~p]: "
	  "~n   Name:    ~p"
	  "~n   Indexes: ~p",[Db, Name, Indexes]),
    Res = case lookup(Db, {Name, Indexes}, State) of
	      undefined -> 
		  undefined;
	      {value, {Row, _Prev, _Next}} -> 
		  Row
	  end,
    ?vdebug("table get row result: "
	    "~n   ~p",[Res]),
    {reply, Res, State};

handle_call({table_get_element, Name, Db, Indexes, Col}, _From, State) ->
    ?vlog("table ~p get element [~p]: "
	  "~n   Indexes: ~p"
	  "~n   Col:     ~p", [Name, Db, Indexes, Col]),
    Res = case lookup(Db, {Name, Indexes}, State) of
	      undefined -> undefined;
	      {value, {Row, _Prev, _Next}} -> {value, element(Col, Row)}
	  end,
    ?vdebug("table get element result: "
	    "~n   ~p",[Res]),
    {reply, Res, State};

handle_call({table_set_elements, Name, Db, Indexes, Cols}, _From, State) ->
    ?vlog("table ~p set element [~p]: "
	  "~n   Indexes: ~p"
	  "~n   Cols:    ~p", [Name, Db, Indexes, Cols]),
    Res = 
	case catch handle_set_elements(Db, Name, Indexes, Cols, State) of
	    {'EXIT', _} -> false;
	    _ -> true
	end,
    ?vdebug("table set element result: "
	    "~n   ~p",[Res]),
    {reply, Res, State};

handle_call({table_next, Name, Db, []}, From, State) ->
    ?vlog("table next: ~p [~p]",[Name, Db]),
    handle_call({table_next, Name, Db, first}, From, State);
    
handle_call({table_next, Name, Db, Indexes}, _From, State) ->
    ?vlog("table ~p next [~p]: "
	  "~n   Indexes: ~p",[Name, Db, Indexes]),
    Res = case lookup(Db, {Name, Indexes}, State) of
	      {value, {_Row, _Prev, Next}} -> 
		  if 
		      Next =:= first -> endOfTable;
		      true -> Next
		  end;
	      undefined -> 
		  table_search_next(Db, Name, Indexes, State)
	  end,
    ?vdebug("table next result: "
	    "~n   ~p",[Res]),
    {reply, Res, State};

handle_call({table_max_col, Name, Db, Col}, _From, State) ->
    ?vlog("table ~p max col [~p]: "
	  "~n   Col: ~p",[Name, Db, Col]),
    Res = table_max_col(Db, Name, Col, 0, first, State),
    ?vdebug("table max col result: "
	    "~n   ~p",[Res]),
    {reply, Res, State};

handle_call({match, Name, Db, Pattern}, _From, State) ->
    ?vlog("match ~p [~p]:"
	"~n   Pat: ~p", [Name, Db, Pattern]),
    L1 = match(Db, Name, Pattern, State),
    {reply, lists:delete([undef], L1), State};

%% This check (that there is no backup already in progress) is also 
%% done in the master agent process, but just in case a user issues 
%% a backup call to this process directly, we add a similar check here. 
handle_call({backup, BackupDir}, From, 
	    #state{backup = undefined, dets = Dets} = State) ->
    ?vlog("backup: ~p",[BackupDir]),
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
			  #dets{tab = Tab} = Dets, 
			  Reply = handle_backup(Tab, Dir),
			  Pid ! {backup_done, Reply},
			  unlink(Pid)
		  end),	
	    ?vtrace("backup server: ~p", [BackupServer]),
	    {noreply, State#state{backup = {BackupServer, From}}};
	{ok, _} ->
	    {reply, {error, not_a_directory}, State};
	Error ->
	    {reply, Error, State}
    end;

handle_call({backup, _BackupDir}, _From, #state{backup = Backup} = S) ->
    ?vinfo("backup already in progress: ~p", [Backup]),
    {reply, {error, backup_in_progress}, S};

handle_call(dump, _From, #state{dets = Dets} = State) ->
    ?vlog("dump",[]),
    dets_sync(Dets),
    {reply, ok, State};

handle_call(info, _From, #state{dets = Dets, ets = Ets} = State) ->
    ?vlog("info",[]),
    Info = get_info(Dets, Ets),
    {reply, Info, State};

handle_call(print, _From, #state{dets = Dets, ets = Ets} = State) ->
    ?vlog("print",[]),
    L1 = ets:tab2list(Ets),
    L2 = dets_match_object(Dets, '_'),
    {reply, {{ets, L1}, {dets, L2}}, State};

handle_call({print, Table, Db}, _From, State) ->
    ?vlog("print: ~p [~p]", [Table, Db]),
    L = match(Db, Table, '$1', State),
    {reply, lists:delete([undef], L), State};

handle_call({register_notify_client, Client, Module}, _From, State) ->
    ?vlog("register_notify_client: "
	"~n   Client: ~p"
	"~n   Module: ~p", [Client, Module]),
    Nc = State#state.notify_clients,
    case lists:keysearch(Client,1,Nc) of
	{value,{Client,Mod}} ->
	    ?vlog("register_notify_client: already registered to: ~p",
		  [Module]),
	    {reply, {error,{already_registered,Mod}}, State};
	false ->
	    {reply, ok, State#state{notify_clients = [{Client,Module}|Nc]}}
    end;

handle_call({unregister_notify_client, Client}, _From, State) ->
    ?vlog("unregister_notify_client: ~p",[Client]),
    Nc = State#state.notify_clients,
    case lists:keysearch(Client,1,Nc) of
	{value,{Client,_Module}} ->
	    Nc1 = lists:keydelete(Client,1,Nc),
	    {reply, ok, State#state{notify_clients = Nc1}};
	false ->
	    ?vlog("unregister_notify_client: not registered",[]),
	    {reply, {error,not_registered}, State}
    end;

handle_call(stop, _From, State) ->
    ?vlog("stop",[]),
    {stop, normal, stopped, State};

handle_call(Req, _From, State) ->
    warning_msg("received unknown request: ~n~p", [Req]),
    Reply = {error, {unknown, Req}}, 
    {reply, Reply, State}.


handle_cast({variable_inc, Name, Db, N}, State) ->
    ?vlog("variable ~p inc"
	  "~n   N: ~p", [Name, N]),
    M = case lookup(Db, Name, State) of
	    {value, Val} -> Val;
	    _ -> 0 
	end,
    insert(Db, Name, (M+N) rem 4294967296, State),
    {noreply, State};
    
handle_cast({verbosity,Verbosity}, State) ->
    ?vlog("verbosity: ~p -> ~p",[get(verbosity),Verbosity]),
    put(verbosity,?vvalidate(Verbosity)),
    {noreply, State};
    
handle_cast(Msg, State) ->
    warning_msg("received unknown message: ~n~p", [Msg]),
    {noreply, State}.
    

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

handle_info(Info, State) ->
    warning_msg("received unknown info: ~n~p", [Info]),
    {noreply, State}.


terminate(Reason, State) ->
    ?vlog("terminate: ~p", [Reason]),
    close(State).


%%----------------------------------------------------------
%% Code change
%%----------------------------------------------------------

%% downgrade
%% 
code_change({down, _Vsn}, S1, downgrade_to_pre_4_11) ->
    #state{dets = D} = S1,
    #dets{tab = Dets, shadow = Shadow} = D, 
    ets:delete(Shadow), 
    S2 = S1#state{dets = Dets},
    {ok, S2};

%% upgrade
%% 
code_change(_Vsn, S1, upgrade_from_pre_4_11) ->
    #state{dets = D} = S1,
    Shadow = ets:new(snmp_local_db1_shadow, [set, protected]),
    dets:to_ets(D, Shadow),
    Dets =  #dets{tab = D, shadow = Shadow}, 
    S2 = S1#state{dets = Dets},
    {ok, S2};

code_change(_Vsn, State, _Extra) ->
    {ok, State}.



%%----------------------------------------------------------
%% Backup
%%----------------------------------------------------------

handle_backup(D, BackupDir) ->
    %% First check that we do not wrote to the corrent db-dir...
    ?vtrace("handle_backup -> entry with"
	"~n   D:         ~p"
	"~n   BackupDir: ~p", [D, BackupDir]),
    case dets:info(D, filename) of
	undefined ->
	    ?vinfo("handle_backup -> no file to backup", []),
	    {error, no_file};
	Filename ->
	    ?vinfo("handle_backup -> file to backup: ~n   ~p", [Filename]),
	    case filename:dirname(Filename) of
		BackupDir ->
		    ?vinfo("handle_backup -> backup dir and db dir the same", 
			   []),
		    {error, db_dir};
		_ ->
		    case file:read_file_info(BackupDir) of
			{ok, #file_info{type = directory}} ->
			    ?vdebug("handle_backup -> backup dir ok", []),
			    %% All well so far...
			    Type = dets:info(D, type),
			    KP   = dets:info(D, keypos),
			    dets_backup(D, 
					filename:basename(Filename), 
					BackupDir, Type, KP);
			{ok, _} ->
			    ?vinfo("handle_backup -> backup dir not a dir", 
				   []),
			    {error, not_a_directory};
			Error ->
			    ?vinfo("handle_backup -> Error: ~p", [Error]),
			    Error
		    end
	    end
    end.

dets_backup(D, Filename, BackupDir, Type, KP) ->
    ?vtrace("dets_backup -> entry with"
	    "~n   D:         ~p"
	    "~n   Filename:  ~p"
	    "~n   BackupDir: ~p", [D, Filename, BackupDir]),
    BackupFile = filename:join(BackupDir, Filename),
    ?vtrace("dets_backup -> "
	    "~n   BackupFile: ~p", [BackupFile]),
    Opts = [{file, BackupFile}, {type, Type}, {keypos, KP}], 
    case dets:open_file(?BACKUP_TAB, Opts) of
	{ok, B} ->
	    F = fun(Arg) -> 
			dets_backup(Arg, start, D, B)
		end,
	    ?vtrace("dets_backup -> fix table", []),
	    dets:safe_fixtable(D, true),
	    ?vtrace("dets_backup -> copy table", []),
	    Res = dets:init_table(?BACKUP_TAB, F, [{format, bchunk}]),
	    ?vtrace("dets_backup -> unfix table", []),
	    dets:safe_fixtable(D, false),
	    ?vtrace("dets_backup -> Res: ~p", [Res]),
	    Res;
	Error ->
	    ?vinfo("dets_backup -> open_file failed: "
		   "~n   ~p", [Error]),
	    Error
    end.


dets_backup(close, _Cont, _D, B) ->
    dets:close(B),
    ok;
dets_backup(read, Cont1, D, B) ->
    case dets:bchunk(D, Cont1) of
	{error, _} = ERROR ->
	    ERROR;
	'$end_of_table' ->
	    dets:close(B),
	    end_of_input;
	{Cont2, Data} ->
	    F = fun(Arg) ->
			dets_backup(Arg, Cont2, D, B)
		end,
	    {Data, F}
    end.


%%-----------------------------------------------------------------
%% All handle_ functions exists so we can catch the call to
%% them, because we don't want to crash the server if a user
%% forgets to call e.g. table_create.
%%-----------------------------------------------------------------
%% Find larger element and insert before.
handle_create_row(Db, Name, Indexes, Row, State) ->		
    case table_find_first_after_maybe_same(Db, Name, Indexes, State) of
	{{Name, Next}, {NRow, NPrev, NNext}} ->
	    {value, {PRow, PPrev, _PNext}} = lookup(Db, {Name, NPrev}, State),
	    if 
		Next =:= NPrev ->
		    % Insert before first
		    insert(Db, {Name, NPrev}, {PRow, Indexes, Indexes}, State);
		true ->
		    insert(Db, {Name, NPrev}, {PRow, PPrev, Indexes}, State),
		    insert(Db, {Name, Next}, {NRow, Indexes, NNext}, State)
	    end,
	    insert(Db, {Name, Indexes}, {Row, NPrev, Next}, State);
	{same_row, {Prev, Next}} ->
	    insert(Db, {Name, Indexes}, {Row, Prev, Next}, State)
    end.

handle_delete_row(Db, Name, Indexes, State) ->
    {value, {_, Prev, Next}} = lookup(Db, {Name, Indexes}, State),
    {value, {PRow, PPrev, Indexes}} = lookup(Db, {Name, Prev}, State),
    insert(Db, {Name, Prev}, {PRow, PPrev, Next}, State),
    {value, {NRow, Indexes, NNext}} = lookup(Db, {Name, Next}, State),
    insert(Db, {Name, Next}, {NRow, Prev, NNext}, State),
    delete(Db, {Name, Indexes}, State).

handle_set_elements(Db, Name, Indexes, Cols, State) ->
    {value, {Row, Prev, Next}} = lookup(Db, {Name, Indexes}, State),
    NewRow = set_new_row(Cols, Row),
    insert(Db, {Name, Indexes}, {NewRow, Prev, Next}, State).

set_new_row([{Col, Val} | Cols], Row) ->
    set_new_row(Cols, setelement(Col, Row, Val));
set_new_row([], Row) ->
    Row.

handle_delete(Db, Name, State) ->
    {value, {_, _, Next}} = lookup(Db, {Name, first}, State),
    delete(Db, {Name, first}, State),
    handle_delete(Db, Name, Next, State).
handle_delete(_Db, _Name, first, _State) -> true;
handle_delete(Db, Name, Indexes, State) ->
    {value, {_, _, Next}} = lookup(Db, {Name, Indexes}, State),
    delete(Db, {Name, Indexes}, State),
    handle_delete(Db, Name, Next, State).

%%-----------------------------------------------------------------
%% Implementation of next.
%%-----------------------------------------------------------------
table_search_next(Db, Name, Indexes, State) ->
    case catch table_find_first_after(Db, Name, Indexes, State) of
	{{Name, Key}, {_, _, _Next}} ->
	    case Key of
		first -> endOfTable;
		_ -> Key
	    end;
	{'EXIT', _} -> endOfTable
    end.

table_find_first_after(Db, Name, Indexes, State) ->
    {value, {_Row, _Prev, Next}} = lookup(Db, {Name, first}, State),
    table_loop(Db, Name, Indexes, Next, State).

table_loop(Db, Name, _Indexes, first, State) -> 
    {value, FirstVal} = lookup(Db, {Name, first}, State),
    {{Name, first}, FirstVal};

table_loop(Db, Name, Indexes, Cur, State) -> 
    {value, {Row, Prev, Next}} = lookup(Db, {Name, Cur}, State),
    if
	Cur > Indexes ->
	    {{Name, Cur}, {Row, Prev, Next}};
	true ->
	    table_loop(Db, Name, Indexes, Next, State)
    end.
    
table_find_first_after_maybe_same(Db, Name, Indexes, State) ->
    {value, {_Row, _Prev, Next}} = lookup(Db, {Name, first}, State),
    table_loop2(Db, Name, Indexes, Next, State).

table_loop2(Db, Name, _Indexes, first, State) -> 
    {value, FirstVal} = lookup(Db, {Name, first}, State),
    {{Name, first}, FirstVal};

table_loop2(Db, Name, Indexes, Cur, State) -> 
    {value, {Row, Prev, Next}} = lookup(Db, {Name, Cur}, State),
    if
	Cur > Indexes ->
	    {{Name, Cur}, {Row, Prev, Next}};
	Cur =:= Indexes ->
	    {same_row, {Prev, Next}};
	true ->
	    table_loop2(Db, Name, Indexes, Next, State)
    end.
    
%%-----------------------------------------------------------------
%% Implementation of max.
%% The value in a column could be noinit or undefined,
%% so we must check only those with an integer.
%%-----------------------------------------------------------------
table_max_col(Db, Name, Col, Max, Indexes, State) ->
    case lookup(Db, {Name, Indexes}, State) of
	{value, {Row, _Prev, Next}} -> 
	    if 
		Next =:= first -> 
		    if 
			is_integer(element(Col, Row)) andalso 
			(element(Col, Row) > Max) -> 
			    element(Col, Row);
			true ->
			    Max
		    end;
		is_integer(element(Col, Row)) andalso 
		(element(Col, Row) > Max) -> 
		    table_max_col(Db,Name, Col,element(Col, Row),Next, State);
		true -> 
		    table_max_col(Db, Name, Col, Max, Next, State)
	    end;
	undefined -> table_search_next(Db, Name, Indexes, State)
    end.
    
%%-----------------------------------------------------------------
%% Interface to Pets.
%%-----------------------------------------------------------------
insert(volatile, Key, Val, #state{ets = Ets}) -> 
    ?vtrace("insert(volatile) -> entry with"
	    "~n   Key: ~p"
	    "~n   Val: ~p",
	    [Key,Val]),
    ets:insert(Ets, {Key, Val}),
    true;
insert(persistent, Key, Val, #state{dets = Dets, notify_clients = NC}) -> 
    ?vtrace("insert(persistent) -> entry with"
	    "~n   Key: ~p"
	    "~n   Val: ~p",
	    [Key,Val]),
    case dets_insert(Dets, {Key, Val}) of
	ok ->
	    notify_clients(insert,NC),
	    true;
	{error, Reason} ->
	    error_msg("DETS (persistent) insert failed for {~w,~w}: ~n~w", 
		      [Key, Val, Reason]),
	    false
    end;
insert(permanent, Key, Val, #state{dets = Dets, notify_clients = NC}) -> 
    ?vtrace("insert(permanent) -> entry with"
	    "~n   Key: ~p"
	    "~n   Val: ~p",
	    [Key,Val]),
    case dets_insert(Dets, {Key, Val}) of
	ok ->
	    notify_clients(insert,NC),
	    true;
	{error, Reason} ->
	    error_msg("DETS (permanent) insert failed for {~w,~w}: ~n~w", 
		      [Key, Val, Reason]),
	    false
    end;
insert(UnknownDb, Key, Val, _) ->
    error_msg("Tried to insert ~w = ~w into unknown db ~w", 
	      [Key, Val, UnknownDb]),
    false.

delete(volatile, Key, State) -> 
    ets:delete(State#state.ets, Key),
    true;
delete(persistent, Key, #state{dets = Dets, notify_clients = NC}) -> 
    case dets_delete(Dets, Key) of
	ok ->
	    notify_clients(delete,NC),
	    true;
	{error, Reason} ->
	    error_msg("DETS (persistent) delete failed for ~w: ~n~w", 
		      [Key, Reason]),
	    false
    end;
delete(permanent, Key, #state{dets = Dets, notify_clients = NC}) -> 
    case dets_delete(Dets, Key) of
	ok ->
	    notify_clients(delete,NC),
	    true;
	{error, Reason} ->
	    error_msg("DETS (permanent) delete failed for ~w: ~n~w", 
		      [Key, Reason]),
	    false
    end;
delete(UnknownDb, Key, _) ->
    error_msg("Tried to delete ~w from unknown db ~w", 
	      [Key, UnknownDb]),
    false.


match(volatile, Name, Pattern, #state{ets = Ets}) ->
    ets:match(Ets, {{Name,'_'},{Pattern,'_','_'}});
match(persistent, Name, Pattern, #state{dets = Dets}) ->
    dets_match(Dets, {{Name,'_'},{Pattern,'_','_'}});
match(permanent, Name, Pattern, #state{dets = Dets}) ->
    dets_match(Dets, {{Name,'_'},{Pattern,'_','_'}});
match(UnknownDb, Name, Pattern, _) ->
    error_msg("Tried to match [~p,~p] from unknown db ~w", 
	      [Name, Pattern, UnknownDb]),
    [].

lookup(volatile, Key, #state{ets = Ets}) ->
    case ets:lookup(Ets, Key) of
	[{_, Val}] -> 
	    {value, Val};
	[] -> 
	    undefined
    end;
lookup(persistent, Key, #state{dets = Dets}) ->
    case dets_lookup(Dets, Key) of
	[{_, Val}] -> 
	    {value, Val};
	[] -> 
	    undefined
    end;
lookup(permanent, Key, #state{dets = Dets}) ->
    case dets_lookup(Dets, Key) of
	[{_, Val}] -> 
	    {value, Val};
	[] -> 
	    undefined
    end;
lookup(UnknownDb, Key, _) ->
    error_msg("Tried to lookup ~w in unknown db ~w", [Key, UnknownDb]),
    false.

close(#state{dets = Dets, ets = Ets}) -> 
    ets:delete(Ets),
    dets_close(Dets).


%%-----------------------------------------------------------------
%% Notify clients interface
%%-----------------------------------------------------------------
notify_clients(Event, Clients) ->
    F = fun(Client) -> notify_client(Client, Event) end,
    lists:foreach(F, Clients).

notify_client({Client,Module}, Event) ->
    (catch Module:notify(Client,Event)).


%%------------------------------------------------------------------
%%  Constructs a row with first elements the own part of RowIndex,
%%  and last element RowStatus. All values are stored "as is", i.e.
%%  dynamic key values are stored without length first.
%%  RowIndex is a list of the
%%  first elements. RowStatus is needed, because the
%%  provided value may not be stored, e.g. createAndGo
%%  should be active.
%%  Returns a tuple of values for the row. If a value
%%  isn't specified in the Col list, then the
%%  corresponding value will be noinit.
%%------------------------------------------------------------------
table_construct_row(Name, RowIndex, Status, Cols) ->
    #table_info{nbr_of_cols = LastCol, index_types = Indexes,
		defvals = Defs, status_col = StatusCol,
		first_own_index = FirstOwnIndex, not_accessible = NoAccs} =
	snmp_generic:table_info(Name),
    ?vtrace(
       "table_construct_row Indexes: ~p~n"
       "    RowIndex: ~p",
       [Indexes, RowIndex]),
    Keys = snmp_generic:split_index_to_keys(Indexes, RowIndex),
    OwnKeys = snmp_generic:get_own_indexes(FirstOwnIndex, Keys),
    Row = OwnKeys ++ snmp_generic:table_create_rest(length(OwnKeys) + 1,
						    LastCol, StatusCol,
						    Status, Cols, NoAccs),
    L = snmp_generic:init_defaults(Defs, Row),
    list_to_tuple(L).


table_get_elements(NameDb, RowIndex, Cols, _FirstOwnIndex) ->
    get_elements(Cols, table_get_row(NameDb, RowIndex)).

get_elements(_Cols, undefined) -> 
    undefined;
get_elements([Col | Cols], Row) when is_tuple(Row) and (size(Row) >= Col) ->
    [element(Col, Row) | get_elements(Cols, Row)];
get_elements([], _Row) -> 
    [];
get_elements(Cols, Row) ->
    erlang:error({bad_arguments, Cols, Row}).


%%----------------------------------------------------------------------
%% This should/could be a generic function, but since Mnesia implements
%% its own and this version still is local_db dependent, it's not generic yet.
%%----------------------------------------------------------------------
%% createAndGo
table_set_status(NameDb, RowIndex, ?'RowStatus_createAndGo', StatusCol, Cols, 
		 ChangedStatusFunc, _ConsFunc) ->
    case table_create_row(NameDb, RowIndex, ?'RowStatus_active', Cols) of
	true -> snmp_generic:try_apply(ChangedStatusFunc,
				       [NameDb, ?'RowStatus_createAndGo',
					RowIndex, Cols]);
	_ -> {commitFailed, StatusCol}
    end;

%%------------------------------------------------------------------
%% createAndWait - set status to notReady, and try to 
%% make row consistent.
%%------------------------------------------------------------------
table_set_status(NameDb, RowIndex, ?'RowStatus_createAndWait', StatusCol, Cols, 
		 ChangedStatusFunc, ConsFunc) ->
    case table_create_row(NameDb, RowIndex, ?'RowStatus_notReady', Cols) of
	true -> 
	    case snmp_generic:try_apply(ConsFunc, [NameDb, RowIndex, Cols]) of
		{noError, 0} ->
		    snmp_generic:try_apply(ChangedStatusFunc, 
					   [NameDb, ?'RowStatus_createAndWait',
					    RowIndex, Cols]);
		Error -> Error
	    end;
	_ -> {commitFailed, StatusCol}
    end;
    
%% destroy
table_set_status(NameDb, RowIndex, ?'RowStatus_destroy', _StatusCol, Cols,
		 ChangedStatusFunc, _ConsFunc) ->
    case snmp_generic:try_apply(ChangedStatusFunc,
				[NameDb, ?'RowStatus_destroy',
				 RowIndex, Cols]) of
	{noError, 0} ->
	    table_delete_row(NameDb, RowIndex),
	    {noError, 0};
	Error -> Error
    end;

%% Otherwise, active or notInService
table_set_status(NameDb, RowIndex, Val, _StatusCol, Cols,
		 ChangedStatusFunc, ConsFunc) ->
    snmp_generic:table_set_cols(NameDb, RowIndex, Cols, ConsFunc),
    snmp_generic:try_apply(ChangedStatusFunc, [NameDb, Val, RowIndex, Cols]).

table_func(new, NameDb) ->
    case table_exists(NameDb) of
	true -> ok;
	_ -> table_create(NameDb)
    end;

table_func(delete, _NameDb) ->
    ok.

table_func(get, RowIndex, Cols, NameDb) ->
    TableInfo = snmp_generic:table_info(NameDb),
    snmp_generic:handle_table_get(NameDb, RowIndex, Cols,
				  TableInfo#table_info.first_own_index);

%%------------------------------------------------------------------
%% Returns: List of endOfTable | {NextOid, Value}.
%% Implements the next operation, with the function
%% handle_table_next. Next should return the next accessible
%% instance, which cannot be a key.
%%------------------------------------------------------------------
table_func(get_next, RowIndex, Cols, NameDb) ->
    #table_info{first_accessible = FirstCol, first_own_index = FOI,
		nbr_of_cols = LastCol} = snmp_generic:table_info(NameDb),
    snmp_generic:handle_table_next(NameDb, RowIndex, Cols,
				   FirstCol, FOI, LastCol);

%%-----------------------------------------------------------------
%% This function must only be used by tables with a RowStatus col!
%% Other tables must check if row exist themselves.
%%-----------------------------------------------------------------
table_func(is_set_ok, RowIndex, Cols, NameDb) ->
    snmp_generic:table_try_row(NameDb, nofunc, RowIndex, Cols);

%%------------------------------------------------------------------
%% Cols is here a list of {ColumnNumber, NewValue}
%% This function must only be used by tables with a RowStatus col!
%% Other tables should use table_set_cols/3,4.
%%------------------------------------------------------------------
table_func(set, RowIndex, Cols, NameDb) ->
    snmp_generic:table_set_row(NameDb,
			       nofunc,
			       fun snmp_generic:table_try_make_consistent/3,
			       RowIndex,
			       Cols);

table_func(undo, _RowIndex, _Cols, _NameDb) ->
    {noError, 0}.



%%------------------------------------------------------------------
%% This functions retrieves option values from the Options list.
%%------------------------------------------------------------------
get_opt(Key, Opts, Def) ->
    snmp_misc:get_option(Key, Opts, Def).


%%------------------------------------------------------------------

get_info(Dets, Ets) ->
    ProcSize = proc_mem(self()),
    DetsSz   = dets_size(Dets),
    EtsSz    = ets_size(Ets),
    [{process_memory, ProcSize},
     {db_memory, [{persistent, DetsSz}, {volatile, EtsSz}]}].

proc_mem(P) when is_pid(P) ->
    case (catch erlang:process_info(P, memory)) of
	{memory, Sz} ->
	    Sz;
	_ ->
	    undefined
    end.
%% proc_mem(_) ->
%%     undefined.

dets_size(#dets{tab = Tab, shadow = Shadow}) ->
    TabSz = 
	case (catch dets:info(Tab, file_size)) of
	    Sz when is_integer(Sz) ->
		Sz;
	    _ ->
		undefined
	end,
    ShadowSz = ets_size(Shadow),
    [{tab, TabSz}, {shadow, ShadowSz}].

ets_size(T) ->
    case (catch ets:info(T, memory)) of
	Sz when is_integer(Sz) ->
	    Sz;
	_ ->
	    undefined
    end.

%%------------------------------------------------------------------

%% info_msg(F, A) ->
%%     ?snmpa_info("Local DB server: " ++ F, A).

warning_msg(F, A) ->
    ?snmpa_warning("Local DB server: " ++ F, A).

error_msg(F, A) ->
    ?snmpa_error("Local DB server: " ++ F, A).

%% --- 

user_err(F, A) ->
    snmpa_error:user_err(F, A).

config_err(F, A) ->
    snmpa_error:config_err(F, A).


%% ----------------------------------------------------------------

call(Req) ->
    gen_server:call(?SERVER, Req, infinity).

cast(Msg) ->
    gen_server:cast(?SERVER, Msg).


%% ----------------------------------------------------------------
%% DETS wrapper functions
%% The purpose of these fuctions is basically to hide the shadow 
%% table.
%% Changes are made both in dets and in the shadow table.
%% Reads are made from the shadow table.
%% ----------------------------------------------------------------

dets_sync(#dets{tab = Dets}) ->
    dets:sync(Dets).
    
dets_insert(#dets{tab = Tab, shadow = Shadow}, Data) ->
    ets:insert(Shadow, Data),
    dets:insert(Tab, Data).

dets_delete(#dets{tab = Tab, shadow = Shadow}, Key) ->
    ets:delete(Shadow, Key),
    dets:delete(Tab, Key).

dets_match(#dets{shadow = Shadow}, Pat) ->
    ets:match(Shadow, Pat).

dets_match_object(#dets{shadow = Shadow}, Pat) ->
    ets:match_object(Shadow, Pat).

dets_lookup(#dets{shadow = Shadow}, Key) ->
    ets:lookup(Shadow, Key).

dets_close(#dets{tab = Tab, shadow = Shadow}) ->
    ets:delete(Shadow),
    dets:close(Tab).
