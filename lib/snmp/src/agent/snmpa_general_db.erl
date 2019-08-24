%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
-module(snmpa_general_db).


%%%-----------------------------------------------------------------
%%% This module implements a very simple "generic" MIB database 
%%% interface. It contains the most common functions performed.
%%% It is generic in the sense that it implements both an interface
%%% to mnesia and ets.
%%%
%%% Note that this module has nothing to do with the snmp_generic
%%% and snmp_generic_mnesia modules.
%%%-----------------------------------------------------------------

-export([open/5, close/1, read/2, write/2, delete/1, delete/2]).
-export([sync/1, backup/2]).
-export([match_object/2, match_delete/2]).
-export([tab2list/1, info/1, info/2]).


-define(VMODULE,"GDB").
-include("snmp_verbosity.hrl").


%% ---------------------------------------------------------------
%% open(Info,Name,RecName,Attr,Type) -> term()
%% Info    -> ets | {ets, Dir} | 
%%            {dets, Dir} | {dets, Dir, Action} |
%%            {mnesia, Nodes} | {mnesia, Nodes, Action}
%% Name    -> atom()
%% RecName -> Name of the record to store
%% Attr    -> Attributes of the record stored in the table
%% Type    -> set | bag
%% Dir     -> string()
%% Nodes   -> [node()]
%% Action  -> keep | clear
%% 
%% Open or create a database table. In the mnesia/dets case, 
%% where the table is stored on disc, it could be of interest
%% to clear the table. This is controlled by the Action parameter.
%% An empty node list ([]), is traslated into a list containing
%% only the own node.
%% ---------------------------------------------------------------
open({mnesia,Nodes,clear}, Name, RecName, Attr, Type) when is_list(Nodes) ->
    ?vtrace("[mnesia] open ~p database ~p for ~p on ~p; clear",
	    [Type, Name, RecName, Nodes]),
    mnesia_open(mnesia_table_check(Name), Nodes, RecName, Attr, Type, clear);
open({mnesia,Nodes,_}, Name, RecName, Attr, Type) ->
    ?vtrace("[mnesia] open ~p database ~p for ~p on ~p; keep",
	    [Type, Name, RecName, Nodes]),
    open({mnesia,Nodes}, Name, RecName, Attr, Type);
open({mnesia,Nodes}, Name, RecName, Attr, Type) when is_list(Nodes) ->
    ?vtrace("[mnesia] open ~p database ~p for ~p on ~p",
	    [Type, Name, RecName, Nodes]),
    mnesia_open(mnesia_table_check(Name), Nodes, RecName, Attr, Type, keep);

open({dets, Dir, Action}, Name, _RecName, _Attr, Type) ->
    dets_open(Name, dets_filename(Name, Dir), Type, Action);
open({dets, Dir}, Name, _RecName, _Attr, Type) ->
    dets_open(Name, dets_filename(Name, Dir), Type, keep);

%% This function creates the ets table 
open(ets, Name, _RecName, _Attr, Type) ->
    ?vtrace("[ets] open ~p database ~p", [Type, Name]),
    Ets = ets:new(Name, [Type, protected, {keypos, 2}]),
    {ets, Ets, undefined};
open({ets, Dir}, Name, _RecName, _Attr, Type) ->
    ets_open(Name, Dir, keep, Type);
open({ets, Dir, Action}, Name, _RecName, _Attr, Type) ->
    ets_open(Name, Dir, Action, Type).

ets_open(Name, Dir, keep, Type) ->
    ?vtrace("[ets] open ~p database ~p", [Type, Name]),
    File = filename:join(Dir, atom_to_list(Name) ++ ".db"),
    case file:read_file_info(File) of
	{ok, _} ->
	    case ets:file2tab(File) of
		{ok, Tab} ->
		    {ets, Tab, File};
		{error, Reason} ->
		    user_err("failed converting file to (ets-) tab: "
			     "File: ~p"
			     "~n~p", [File, Reason]),
		    Ets = ets:new(Name, [Type, protected, {keypos, 2}]),
		    {ets, Ets, File}
	    end;
	{error, _} ->
	    Ets = ets:new(Name, [Type, protected, {keypos, 2}]),
	    {ets, Ets, File}
    end;
ets_open(Name, Dir, clear, Type) ->
    File = filename:join(Dir, atom_to_list(Name) ++ ".db"),
    Ets  = ets:new(Name, [Type, protected, {keypos, 2}]),
    {ets, Ets, File}.
    
	    

mnesia_open({table_exist,Name},_Nodes,_RecName,_Attr,_Type,clear) ->
    ?vtrace("[mnesia] database ~p already exists; clear content",[Name]),
    F = fun() -> mnesia:clear_table(Name) end,
    case mnesia:transaction(F) of
	{aborted,Reason} ->
	    exit({aborted,Reason});
	{atomic,_} ->
	    {mnesia,Name}
    end;
mnesia_open({table_exist,Name},_Nodes,_RecName,_Attr,_Type,keep) ->
    ?vtrace("[mnesia] database ~p already exists; keep content",[Name]),
    {mnesia,Name};
mnesia_open({no_table,Name},[],Type,RecName,Attr,Action) ->
    mnesia_open({no_table,Name},[node()],Type,RecName,Attr,Action);
mnesia_open({no_table,Name},Nodes,RecName,Attr,Type,_) ->
    ?vtrace("[mnesia] no database ~p: create for ~p of type ~p",
	    [Name,RecName,Type]),
    %% Ok, we assume that this means that the table does not exist
    Args = [{record_name,RecName}, {attributes,Attr},
	    {type,Type}, {disc_copies,Nodes}],
    case mnesia:create_table(Name,Args) of
	{atomic,ok} ->
	    {mnesia,Name};
	{aborted,Reason} ->
	    %% ?vinfo("[mnesia] aborted: ~p", [Reason]),
	    exit({failed_create_mnesia_table,Reason})
    end.


mnesia_table_check(Name) ->
    ?vtrace("[mnesia] check existens of database ~p",[Name]),
    case (catch mnesia:table_info(Name,type)) of
	{'EXIT', _Reason} ->
	    {no_table, Name};
	_ ->
	    {table_exist, Name}
    end.
    

dets_open(Name, File, Type, Action) ->
    ?vtrace("[dets] open database ~p (~p)", [Name, Action]),
    N = dets_open1(Name, File, Type),
    dets_open2(N, Action).

dets_open1(Name, File, Type) ->
    ?vtrace("[dets] open database ~p of type ~p",[Name, Type]),
    {ok, N} = dets:open_file(Name, [{file, File}, {type, Type}, {keypos, 2}]),
    N.
    
dets_open2(N, clear) ->
    dets:match_delete(N,'_'),
    {dets, N};
dets_open2(N, _) ->
    {dets, N}.

%% dets_table_check(Name, Dir) ->
%%     Filename = dets_filename(Name, Dir),
%%     ?vtrace("[dets] check existens of database: "
%% 	"~n   ~p -> ~s"
%% 	"~n   ~p"
%% 	"~n   ~p"
%% 	, 
%% 	[Name, Filename, file:list_dir(Dir), file:read_file_info(Filename)]),
%%     case (catch dets:info(Filename, size)) of
%% 	{'EXIT', Reason} ->
%% 	    {no_table, Name, Filename};
%% 	undefined -> %% Introduced in R8
%% 	    {no_table, Name, Filename};
%% 	_ ->
%% 	    {table_exist, Name, Filename}
%%     end.
    

dets_filename(Name, Dir) ->
    Dir1 = dets_filename1(Dir),
    Dir2 = string:strip(Dir1, right, $/),
    io_lib:format("~s/~p.dat", [Dir2, Name]).
    
dets_filename1([])  -> ".";
dets_filename1(Dir) -> Dir.


%% ---------------------------------------------------------------
%% close(DbRef) -> 
%% DbRef -> term()
%% 
%% Close the database. This does nothing in the mnesia case, but
%% deletes the table in the ets case.
%% ---------------------------------------------------------------
close({mnesia,_}) ->
    ?vtrace("[mnesia] close database: NO ACTION",[]),
    ok;
close({dets, Name}) ->
    ?vtrace("[dets] close database ~p",[Name]),
    dets:close(Name);
close({ets, Name, undefined}) ->
    ?vtrace("[ets] close (delete) table ~p",[Name]),
    ets:delete(Name);
close({ets, Name, File}) ->
    ?vtrace("[ets] close (delete) table ~p",[Name]),
    write_ets_file(Name, File),
    ets:delete(Name).


%% ---------------------------------------------------------------
%% read(DbRef,Key) -> false | {value,Rec}
%% DbRef -> term()
%% Rec   -> tuple()
%% 
%% Retrieve a record from the database.
%% ---------------------------------------------------------------
read({mnesia, Name}, Key) ->
    ?vtrace("[mnesia] read (dirty) from database ~p: ~p",[Name,Key]),
    case (catch mnesia:dirty_read(Name,Key)) of
	[Rec|_] -> {value,Rec};
	_ -> false
    end;
read({dets, Name}, Key) ->
    ?vtrace("[dets] read from table ~p: ~p",[Name,Key]),
    case dets:lookup(Name, Key) of
	[Rec|_] -> {value, Rec};
	_ -> false
    end;
read({ets, Name, _}, Key) ->
    ?vtrace("[ets] read from table ~p: ~p",[Name,Key]),
    case ets:lookup(Name, Key) of
	[Rec|_] -> {value, Rec};
	_ -> false
    end.
    

%% ---------------------------------------------------------------
%% write(DbRef,Rec) -> ok
%% DbRef -> term()
%% Rec   -> tuple()
%% 
%% Write a record to the database.
%% ---------------------------------------------------------------
write({mnesia, Name}, Rec) -> 
    ?vtrace("[mnesia] write to database ~p",[Name]),
    F = fun() -> mnesia:write(Name, Rec, write) end,
    case mnesia:transaction(F) of
	{aborted, Reason} ->
	    exit({aborted, Reason});
	{atomic,_} ->
	    ok
    end;
write({dets, Name}, Rec) ->
    ?vtrace("[dets] write to table ~p",[Name]),
    dets:insert(Name, Rec);
write({ets, Name, _}, Rec) ->
    ?vtrace("[ets] write to table ~p",[Name]),
    ets:insert(Name, Rec).


%% ---------------------------------------------------------------
%% delete(DbRef) -> 
%% DbRef -> term()
%% 
%% Delete the database. 
%% ---------------------------------------------------------------
delete({mnesia, Name}) ->
    ?vtrace("[mnesia] delete database: ~p",[Name]),
    mnesia:delete_table(Name);
delete({dets, Name}) ->
    ?vtrace("[dets] delete database ~p",[Name]),
    File = dets:info(Name, filename),
    case dets:close(Name) of
	ok ->
	    file:delete(File);
	Error ->
	    Error
    end;
delete({ets, Name, undefined}) ->
    ?vtrace("[dets] delete table ~p",[Name]),
    ets:delete(Name);
delete({ets, Name, File}) ->
    ?vtrace("[dets] delete table ~p",[Name]),
    file:delete(File),
    ets:delete(Name).


%% ---------------------------------------------------------------
%% delete(DbRef, Key) -> ok
%% DbRef -> term()
%% Key   -> term()
%% 
%% Delete a record from the database.
%% ---------------------------------------------------------------
delete({mnesia, Name}, Key) -> 
    ?vtrace("[mnesia] delete from database ~p: ~p", [Name, Key]),
    F = fun() -> mnesia:delete(Name, Key, write) end,
    case mnesia:transaction(F) of
	{aborted,Reason} ->
	    exit({aborted,Reason});
	{atomic,_} ->
	    ok
    end;
delete({dets, Name}, Key) ->
    ?vtrace("[dets] delete from table ~p: ~p", [Name, Key]),
    dets:delete(Name, Key);
delete({ets, Name, _}, Key) ->
    ?vtrace("[ets] delete from table ~p: ~p", [Name, Key]),
    ets:delete(Name, Key).


%% ---------------------------------------------------------------
%% match_object(DbRef,Pattern) -> [tuple()]
%% DbRef -> term()
%% Pattern -> tuple()
%% 
%% Search the database for records witch matches the pattern.
%% ---------------------------------------------------------------
match_object({mnesia, Name}, Pattern) ->
    ?vtrace("[mnesia] match_object in ~p of ~p",[Name, Pattern]),
    F = fun() -> mnesia:match_object(Name, Pattern, read) end,
    case mnesia:transaction(F) of
	{aborted, Reason} ->
	    exit({aborted, Reason});
	 {atomic, Recs} ->
	    Recs
    end;
match_object({dets, Name}, Pattern) ->
    ?vtrace("[dets] match_object in ~p of ~p",[Name, Pattern]),
    dets:match_object(Name, Pattern);
match_object({ets, Name, _}, Pattern) ->
    ?vtrace("[ets] match_object in ~p of ~p",[Name, Pattern]),
    ets:match_object(Name, Pattern).
    

%% ---------------------------------------------------------------
%% match_delete(DbRef,Pattern) -> 
%% DbRef -> term()
%% Pattern -> tuple()
%% 
%% Search the database for records witch matches the pattern and 
%% deletes them from the database.
%% ---------------------------------------------------------------
match_delete({mnesia, Name}, Pattern) -> 
    ?vtrace("[mnesia] match_delete in ~p with pattern ~p",[Name,Pattern]),
    F = fun() -> 
		Recs = mnesia:match_object(Name, Pattern, read),
		lists:foreach(fun(Rec) -> 
				      mnesia:delete_object(Name, Rec, write)
			      end, Recs),
		Recs
	end,
    case mnesia:transaction(F) of
	{aborted, Reason} ->
	    exit({aborted, Reason});
	{atomic,R} ->
	    R
    end;
match_delete({dets, Name}, Pattern) -> 
    ?vtrace("[dets] match_delete in ~p with pattern ~p",[Name,Pattern]),
    Recs = dets:match_object(Name, Pattern),
    dets:match_delete(Name, Pattern),
    Recs;
match_delete({ets, Name, _}, Pattern) -> 
    ?vtrace("[ets] match_delete in ~p with pattern ~p",[Name,Pattern]),
    Recs = ets:match_object(Name, Pattern),
    ets:match_delete(Name, Pattern),
    Recs.


%% ---------------------------------------------------------------
%% tab2list(DbRef) -> [tuple()]
%% DbRef -> term()
%% 
%% Return all records in the table in the form of a list.
%% ---------------------------------------------------------------
tab2list({mnesia, Name}) ->
    ?vtrace("[mnesia] tab2list -> list of ~p", [Name]),
    match_object({mnesia, Name}, mnesia:table_info(Name, wild_pattern));
tab2list({dets, Name}) ->
    ?vtrace("[dets] tab2list -> list of ~p", [Name]),
    match_object({dets, Name}, '_');
tab2list({ets, Name, _}) ->
    ?vtrace("[ets] tab2list -> list of ~p", [Name]),
    ets:tab2list(Name).



%% ---------------------------------------------------------------
%% info(Db) -> taglist()
%% info(Db, Item) -> Info
%% Db   -> term()
%% Item -> atom()
%% tablist() -> [{key(),value()}]
%% key() -> atom()
%% value() -> term()
%% 
%% Retrieve table information.
%% ---------------------------------------------------------------
info({mnesia, Name}) ->
    case (catch mnesia:table_info(Name, all)) of
	Info when is_list(Info) ->
	    Info;
	{'EXIT', {aborted, Reason}} ->
	    {error, Reason};
	Else ->
	    {error, Else}
    end;
info({dets, Name}) ->
    dets:info(Name);
info({ets, Name, _}) ->
    case ets:info(Name) of
	undefined ->
	    [];
	L ->
	    L
    end.


info({mnesia, Name}, Item) ->
    case (catch mnesia:table_info(Name, Item)) of
	{'EXIT', {aborted, Reason}} ->
	    {error, Reason};
	Info ->
	    Info
    end;
info({dets, Name}, memory) ->
    dets:info(Name, file_size);
info({dets, Name}, Item) ->
    dets:info(Name, Item);
info({ets, Name, _}, Item) ->
    ets:info(Name, Item).


%% ---------------------------------------------------------------
%% sync(Db) -> ok | {error, Reason}
%% Db     -> term()
%% Reason -> term()
%% 
%% Dump table to disc (if possible)
%% ---------------------------------------------------------------

sync({mnesia, _}) ->
    ok;
sync({dets, Name}) ->
    dets:sync(Name);
sync({ets, _Name, undefined}) ->
    ok;
sync({ets, Name, File}) ->
    write_ets_file(Name, File).


%% ---------------------------------------------------------------
%% backup(Db, BackupDir) -> ok | {error, Reason}
%% Db     -> term()
%% Reason -> term()
%% 
%% Make a backup copy of the DB (only valid for det and ets)
%% ---------------------------------------------------------------

backup({mnesia, _}, _) ->
    ok;
backup({dets, Name}, BackupDir) ->
    case dets:info(Name, filename) of
	undefined ->
	    {error, no_file};
	Filename ->
	    case filename:dirname(Filename) of
		BackupDir ->
		    {error, db_dir};
		_ ->
		    Type = dets:info(Name, type),
		    KP   = dets:info(Name, keypos),
		    dets_backup(Name, 
				filename:basename(Filename), 
				BackupDir, Type, KP)
	    end
    end;
backup({ets, _Name, undefined}, _BackupDir) ->
    ok;
backup({ets, Name, File}, BackupDir) ->
    Filename = filename:basename(File),
    case filename:join(BackupDir, Filename) of
	File ->
	    %% Oups: backup-dir and db-dir the same
	    {error, db_dir};
	BackupFile ->
	    write_ets_file(Name, BackupFile)
    end.

	      
dets_backup(Name, Filename, BackupDir, Type, KP) ->
    ?vtrace("dets_backup -> entry with"
	    "~n   Name:      ~p"
	    "~n   Filename:  ~p"
	    "~n   BackupDir: ~p"
	    "~n   Type:      ~p"
	    "~n   KP:        ~p", [Name, Filename, BackupDir, Type, KP]),
    BackupFile = filename:join(BackupDir, Filename),
    ?vtrace("dets_backup -> "
	    "~n   BackupFile: ~p", [BackupFile]),
    Backup = list_to_atom(atom_to_list(Name) ++ "_backup"),
    Opts   = [{file, BackupFile}, {type, Type}, {keypos, KP}], 
    case dets:open_file(Backup, Opts) of
	{ok, B} ->
	    ?vtrace("dets_backup -> create fun", []),
	    F = fun(Arg) -> 
			dets_backup(Arg, start, Name, B)
		end,
	    dets:safe_fixtable(Name, true),
	    Res = dets:init_table(Backup, F, [{format, bchunk}]),
	    dets:safe_fixtable(Name, false),
	    ?vtrace("dets_backup -> Res: ~p", [Res]),
	    Res;
	Error ->
	    ?vinfo("dets_backup -> open_file failed: "
		   "~n   ~p", [Error]),
	    Error
    end.


dets_backup(close, _Cont, _Name, B) ->
    dets:close(B),
    ok;
dets_backup(read, Cont1, Name, B) ->
    case dets:bchunk(Name, Cont1) of
	{Cont2, Data} ->
	    F = fun(Arg) ->
			dets_backup(Arg, Cont2, Name, B)
		end,
	    {Data, F};
	'$end_of_table' ->
	    dets:close(B),
	    end_of_input;
	Error ->
	    Error
    end.


%%----------------------------------------------------------------------

write_ets_file(Name, File) ->
    TmpFile = File ++ ".tmp",
    case ets:tab2file(Name, TmpFile) of
	ok ->
	    case file:rename(TmpFile, File) of
		ok ->
		    ok;
		Else ->
		    user_err("Warning: could not move file ~p"
			     " (~p)", [File, Else])
	    end;
	{error, Reason} ->
	    user_err("Warning: could not save file ~p (~p)",
		     [File, Reason])
    end.


%%----------------------------------------------------------------------

user_err(F, A) ->
    snmpa_error:user_err(F, A).
