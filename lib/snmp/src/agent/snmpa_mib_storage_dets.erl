%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2019. All Rights Reserved.
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
-module(snmpa_mib_storage_dets).

-behaviour(snmpa_mib_storage).


%%%-----------------------------------------------------------------
%%% This module implements the snmpa_mib_storage behaviour. 
%%% It uses dets for storage. 
%%%-----------------------------------------------------------------

-export([
	 open/5, 
	 close/1, 
	 read/2, 
	 write/2, 
	 delete/1, 
	 delete/2, 
	 match_object/2, 
	 match_delete/2, 
	 tab2list/1, 
	 info/1, info/2, 
	 sync/1, 
	 backup/2
	]).


-define(VMODULE, "MS-DETS").
-include("snmp_verbosity.hrl").

-record(tab, {id, rec_name}).


%% ---------------------------------------------------------------
%% open
%% 
%% Open or create a mib-storage (dets) table. 
%% 
%% Opts - A list of implementation dependent options
%%        dets_open_options() = [dets_open_option()]
%%        dets_open_option()  = {dir,       filename()} | 
%%                              {action,    keep | clear} | 
%%                              {auto_save, default | pos_integer()} |
%%                              {repair,    force | boolean()}
%%                              
%% ---------------------------------------------------------------

open(Name, RecName, _Fields, Type, Opts) ->
    Dir      = snmp_misc:get_option(dir,       Opts), 
    Action   = snmp_misc:get_option(action,    Opts, keep), 
    AutoSave = snmp_misc:get_option(auto_save, Opts, default), 
    Repair   = snmp_misc:get_option(repair,    Opts, false), 
    File     = dets_filename(Name, Dir), 
    OpenOpts = [{file,   File}, 
		{type,   Type}, 
		{keypos, 2}, 
		{repair, Repair}] ++ 
	case AutoSave of
	    default ->
		[];
	    _ ->
		[{auto_save, AutoSave}]
	end,
    case dets:open_file(Name, OpenOpts) of
	{ok, ID} when (Action =:= keep) ->
	    {ok, #tab{id = ID, rec_name = RecName}};
	{ok, ID} when (Action =:= clear) ->
	    dets:match_delete(ID, '_'),
	    {ok, #tab{id = ID, rec_name = RecName}};
	{error, Reason} ->
	    {error, {dets_open, Reason}}
    end.

dets_filename(Name, Dir) ->
    Dir1 = dets_filename1(Dir),
    Dir2 = string:strip(Dir1, right, $/),
    io_lib:format("~s/~p.dat", [Dir2, Name]).
    
dets_filename1([])  -> ".";
dets_filename1(Dir) -> Dir.


%% ---------------------------------------------------------------
%% close
%% 
%% Close the table. 
%% ---------------------------------------------------------------

close(#tab{id = ID}) ->
    ?vtrace("close database ~p", [ID]),
    dets:close(ID).


%% ---------------------------------------------------------------
%% read
%% 
%% Retrieve a record from the database table.
%% ---------------------------------------------------------------

read(#tab{id = ID}, Key) ->
    ?vtrace("read from table ~p: ~p", [ID, Key]),
    case dets:lookup(ID, Key) of
	[Rec|_] -> {value, Rec};
	_ -> false
    end.
    

%% ---------------------------------------------------------------
%% write
%% 
%% Write a record to the database table.
%% ---------------------------------------------------------------

write(#tab{id = ID, rec_name = RecName}, Rec) 
  when (is_tuple(Rec) andalso (element(1, Rec) =:= RecName)) ->
    ?vtrace("write to table ~p", [ID]),
    dets:insert(ID, Rec).


%% ---------------------------------------------------------------
%% delete
%% 
%% Delete the database table. 
%% ---------------------------------------------------------------

delete(#tab{id = ID}) ->
    ?vtrace("delete database ~p", [ID]),
    File = dets:info(ID, filename),
    case dets:close(ID) of
	ok ->
	    file:delete(File);
	Error ->
	    Error
    end.


%% ---------------------------------------------------------------
%% delete
%% 
%% Delete a record from the database table.
%% ---------------------------------------------------------------

delete(#tab{id = ID}, Key) ->
    ?vtrace("delete from table ~p: ~p", [ID, Key]),
    dets:delete(ID, Key).


%% ---------------------------------------------------------------
%% match_object
%% 
%% Search the database table for records witch matches the pattern.
%% ---------------------------------------------------------------

match_object(#tab{id = ID}, Pattern) ->
    ?vtrace("match_object in ~p of ~p", [ID, Pattern]),
    dets:match_object(ID, Pattern).
    

%% ---------------------------------------------------------------
%% match_delete
%% 
%% Search the database table for records witch matches the 
%% pattern and deletes them from the database table.
%% ---------------------------------------------------------------

match_delete(#tab{id = ID}, Pattern) -> 
    ?vtrace("match_delete in ~p with pattern ~p", [ID, Pattern]),
    Recs = dets:match_object(ID, Pattern),
    dets:match_delete(ID, Pattern),
    Recs.


%% ---------------------------------------------------------------
%% tab2list
%% 
%% Return all records in the table in the form of a list.
%% ---------------------------------------------------------------

tab2list(#tab{id = ID} = Tab) ->
    ?vtrace("tab2list -> list of ~p", [ID]),
    match_object(Tab, '_').


%% ---------------------------------------------------------------
%% info
%% 
%% Retrieve implementation dependent mib-storage table 
%% information.
%% ---------------------------------------------------------------

info(#tab{id = ID}) ->
    ?vtrace("info -> info of ~p", [ID]),
    dets:info(ID).


info(TabId, all = _Item) ->
    info(TabId);
info(#tab{id = ID}, memory = _Item) ->
    ?vtrace("info on ~p (~w)", [ID, _Item]),
    dets:info(ID, file_size);
info(#tab{id = ID}, Item) ->
    ?vtrace("info on ~p (~w)", [ID, Item]),
    dets:info(ID, Item).


%% ---------------------------------------------------------------
%% sync
%% 
%% Dump mib-storage table to disc (if it has a disk component)
%% ---------------------------------------------------------------

sync(#tab{id = ID}) ->
    ?vtrace("sync -> sync ~p", [ID]),
    dets:sync(ID).


%% ---------------------------------------------------------------
%% backup
%% 
%% Make a backup copy of the mib-storage table. 
%% ---------------------------------------------------------------

backup(#tab{id = ID}, BackupDir) ->
    ?vtrace("backup -> backup of ~p to ~p", [ID, BackupDir]),
    case dets:info(ID, filename) of
	undefined ->
	    {error, no_file};
	Filename ->
	    case filename:dirname(Filename) of
		BackupDir ->
		    {error, db_dir};
		_ ->
		    Type = dets:info(ID, type),
		    KP   = dets:info(ID, keypos),
		    dets_backup(ID, 
				filename:basename(Filename), 
				BackupDir, Type, KP)
	    end
    end.

	      
dets_backup(ID, Filename, BackupDir, Type, KP) ->
    ?vtrace("dets_backup -> entry with"
	    "~n   ID:        ~p"
	    "~n   Filename:  ~p"
	    "~n   BackupDir: ~p"
	    "~n   Type:      ~p"
	    "~n   KP:        ~p", [ID, Filename, BackupDir, Type, KP]),
    BackupFile = filename:join(BackupDir, Filename),
    ?vtrace("dets_backup -> "
	    "~n   BackupFile: ~p", [BackupFile]),
    Backup = list_to_atom(atom_to_list(ID) ++ "_backup"),
    Opts   = [{file, BackupFile}, {type, Type}, {keypos, KP}], 
    case dets:open_file(Backup, Opts) of
	{ok, B} ->
	    ?vtrace("dets_backup -> create fun", []),
	    F = fun(Arg) -> 
			dets_backup(Arg, start, ID, B)
		end,
	    dets:safe_fixtable(ID, true),
	    Res = dets:init_table(Backup, F, [{format, bchunk}]),
	    dets:safe_fixtable(ID, false),
	    ?vtrace("dets_backup -> Res: ~p", [Res]),
	    Res;
	Error ->
	    ?vinfo("dets_backup -> open_file failed: "
		   "~n   ~p", [Error]),
	    Error
    end.

dets_backup(close, _Cont, _ID, B) ->
    dets:close(B),
    ok;
dets_backup(read, Cont1, ID, B) ->
    case dets:bchunk(ID, Cont1) of
        {error, _} = ERROR ->
            ERROR;
	'$end_of_table' ->
	    dets:close(B),
	    end_of_input;
	{Cont2, Data} ->
	    F = fun(Arg) ->
			dets_backup(Arg, Cont2, ID, B)
		end,
	    {Data, F}
    end.


%%----------------------------------------------------------------------

%% user_err(F, A) ->
%%     snmpa_error:user_err(F, A).
