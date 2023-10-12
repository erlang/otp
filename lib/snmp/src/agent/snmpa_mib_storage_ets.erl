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

-module(snmpa_mib_storage_ets).

-behaviour(snmpa_mib_storage).

%%%-----------------------------------------------------------------
%%% This module implements the snmpa_mib_storage behaviour. 
%%% It uses ets for storage. 
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


-define(VMODULE,"MS-ETS").
-include("snmp_verbosity.hrl").

-record(tab, {id, rec_name, file, checksum = false}).


%% ---------------------------------------------------------------
%% open
%% 
%% Open or create an ets table. 
%% Possibly also read data from a (specified) file (mirror) and 
%% populate the table from that (the dir option). 
%% 
%% Opts    - A list of implementation dependent options
%%           ets_open_options() = [ets_open_option()]
%%           ets_open_option()  = {dir,      filename()} | 
%%                                {action,   keep | clear} | 
%%                                {checksum, boolean()}
%% 
%% The RecName and Fields arguments are not used in this 
%% implementation. 
%% 
%% ---------------------------------------------------------------

%% This function creates the ets table 
open(Name, RecName, _Fields, Type, Opts) ->
    ?vtrace("open table ~p", [Name]),
    case lists:keysearch(dir, 1, Opts) of
	{value, {dir, Dir}} ->
	    Action   = snmp_misc:get_option(action,   Opts, keep),
	    Checksum = snmp_misc:get_option(checksum, Opts, false),
	    ?vtrace("open ~p database ~p - check if file exist", [Type, Name]),
	    File = filename:join(Dir, atom_to_list(Name) ++ ".db"),
	    case file:read_file_info(File) of
		{ok, _} ->
		    ?vdebug("open ~p database ~p - file exist - try reading", 
			    [Type, Name]),
		    case ets:file2tab(File, [{verify, Checksum}]) of
			{ok, ID} ->
			    ?vtrace("open ~p database ~p - "
				    "data read from file", [Type, Name]),
			    {ok, #tab{id       = ID, 
				      rec_name = RecName, 
				      file     = File, 
				      checksum = Checksum}};
			{error, Reason} when (Action =:= keep) ->
			    ?vinfo("open ~p database ~p - "
				   "failed reading from file (keep): "
				   "~n   ~p", 
				   [Type, Name, Reason]),
			    {error, {file2tab, Reason}};
			{error, Reason} ->
			    ?vlog("open ~p database ~p - "
				  "failed reading from file (clear): "
				  "~n   ~p", [Type, Name, Reason]),
			    user_err("Warning: could not read file - "
				     "create new (empty): "
				     "~n   File:   ~p"
				     "~n   Reason: ~p", [File, Reason]), 
			    ID = ets:new(Name, [Type, protected, {keypos, 2}]),
			    write_ets_file(ID, File, Checksum), 
			    {ok, #tab{id       = ID, 
				      rec_name = RecName, 
				      file     = File, 
				      checksum = Checksum}}
		    end;
		{error, enoent} ->
		    %% No such file - create it
		    ?vdebug("open ~p database ~p - "
			    "file does *not* exist - create", 
			    [Type, Name]),
		    ID = ets:new(Name, [Type, protected, {keypos, 2}]),
		    write_ets_file(ID, File, Checksum), 
		    {ok, #tab{id       = ID, 
			      rec_name = RecName, 
			      file     = File, 
			      checksum = Checksum}};
		{error, Reason} when (Action =:= keep) ->
		    ?vinfo("open ~p database ~p - "
			   "failed reading file info (keep): "
			   "~n   ~p", 
			   [Type, Name, Reason]),
		    {error, {read_file_info, Reason}};
		{error, Reason} ->
		    ?vlog("open ~p database ~p - "
			  "failed reading file info (clear): "
			  "~n   ~p", 
			  [Type, Name, Reason]),
		    user_err("Warning: could not read file info - "
			     "create new file: "
			     "~n   File:   ~p"
			     "~n   Reason: ~p", [File, Reason]), 
		    ID = ets:new(Name, [Type, protected, {keypos, 2}]),
		    write_ets_file(ID, File, Checksum), 
		    {ok, #tab{id       = ID, 
			      rec_name = RecName, 
			      file     = File, 
			      checksum = Checksum}}
	    end;
	false ->
	    ?vdebug("open ~p database ~p - ok", [Type, Name]),
	    ID = ets:new(Name, [Type, protected, {keypos, 2}]),
	    {ok, #tab{id = ID, rec_name = RecName}}
    end.


%% ---------------------------------------------------------------
%% close
%% 
%% Close the mib-storage table. 
%% We will delete the table and if there is a file component, 
%% will also be written to file. 
%% ---------------------------------------------------------------
close(#tab{id = ID, file = undefined}) ->
    ?vtrace("close (delete) table ~p", [ID]),
    ets:delete(ID);
close(#tab{id = ID, file = File, checksum = Checksum}) ->
    ?vtrace("close (delete) table ~p", [ID]),
    write_ets_file(ID, File, Checksum),
    ets:delete(ID).


%% ---------------------------------------------------------------
%% read
%% 
%% Retrieve a record from the mib-storage table.
%% ---------------------------------------------------------------

read(#tab{id = ID}, Key) ->
    ?vtrace("read from table ~p: ~p", [ID, Key]),
    case ets:lookup(ID, Key) of
	[Rec|_] -> {value, Rec};
	_ -> false
    end.
    

%% ---------------------------------------------------------------
%% write
%% 
%% Write a record to the mib-storage table.
%% ---------------------------------------------------------------

%% This is a very crude guard test is used instead of: is_record(Rec, RecName)
write(#tab{id = ID, rec_name = RecName}, Rec) 
  when (is_tuple(Rec) andalso (element(1, Rec) =:= RecName)) ->
    ?vtrace("write to table ~p", [ID]),
    ets:insert(ID, Rec),
    ok.


%% ---------------------------------------------------------------
%% delete
%% 
%% Delete the mib-storage table. 
%% ---------------------------------------------------------------
delete(#tab{id = ID, file = undefined}) ->
    ?vtrace("delete table ~p", [ID]),
    ets:delete(ID);
delete(#tab{id = ID, file = File}) ->
    ?vtrace("delete table ~p", [ID]),
    file:delete(File),
    ets:delete(ID).


%% ---------------------------------------------------------------
%% delete
%% 
%% Delete a record from the mib-storage table.
%% ---------------------------------------------------------------
delete(#tab{id = ID}, Key) ->
    ?vtrace("delete from table ~p: ~p", [ID, Key]),
    ets:delete(ID, Key),
    ok.
        


%% ---------------------------------------------------------------
%% match_object
%% 
%% Search the mib-storage table for records witch matches 
%% the pattern.
%% ---------------------------------------------------------------

match_object(#tab{id = ID}, Pattern) ->
    ?vtrace("match_object in ~p of ~p", [ID, Pattern]),
    ets:match_object(ID, Pattern).
    

%% ---------------------------------------------------------------
%% match_delete
%% 
%% Search the mib-storage table for records witch matches 
%% the pattern and deletes them from the table.
%% ---------------------------------------------------------------
 
match_delete(#tab{id = ID}, Pattern) -> 
    ?vtrace("match_delete in ~p with pattern ~p", [ID, Pattern]),
    Recs = ets:match_object(ID, Pattern),
    ets:match_delete(ID, Pattern),
    Recs.


%% ---------------------------------------------------------------
%% tab2list
%% 
%% Return all records in the mib-storage table in the form 
%% of a list.
%% ---------------------------------------------------------------

tab2list(#tab{id = ID}) ->
    ?vtrace("tab2list -> list of ~p", [ID]),
    ets:tab2list(ID).



%% ---------------------------------------------------------------
%% info/1,2
%% 
%% Retrieve implementation dependent mib-storage table 
%% information.
%% ---------------------------------------------------------------
info(#tab{id = ID}) ->
    ?vtrace("info on ~p", [ID]),
    case ets:info(ID) of
	undefined ->
	    [];
	L ->
	    L
    end.


info(TabId, all = _Item) ->
    info(TabId);
info(#tab{id = ID}, Item) ->
    ?vtrace("info on ~p", [ID]),
    ets:info(ID, Item).


%% ---------------------------------------------------------------
%% sync
%% 
%% Dump mib-storage table to disc (if there is a file compionent)
%% ---------------------------------------------------------------

sync(#tab{file = undefined}) ->
    ok;
sync(#tab{id = ID, file = File, checksum = Checksum}) ->
    ?vtrace("sync ~p", [ID]),
    write_ets_file(ID, File, Checksum).


%% ---------------------------------------------------------------
%% backup
%% 
%% Make a backup copy of the mib-storage table. Only valid id
%% there is a file component. 
%% ---------------------------------------------------------------

backup(#tab{file = undefined}, _BackupDir) ->
    ok;
backup(#tab{id = ID, file = File, checksum = Checksum}, BackupDir) ->
    ?vtrace("backup ~p to ~p", [ID, BackupDir]),
    Filename = filename:basename(File),
    case filename:join(BackupDir, Filename) of
	File ->
	    %% Oups: backup-dir and db-dir the same
	    {error, db_dir};
	BackupFile ->
	    write_ets_file(ID, BackupFile, Checksum)
    end.

	      
%%----------------------------------------------------------------------

write_ets_file(ID, File, Checksum) when (Checksum =:= true) ->
    do_write_ets_file(ID, File, [{extended_info, [md5sum]}]);
write_ets_file(ID, File, Checksum) when (Checksum =:= false) ->
    do_write_ets_file(ID, File, []).

do_write_ets_file(ID, File, Options) ->
    TmpFile = File ++ ".tmp",
    case ets:tab2file(ID, TmpFile, Options) of
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
