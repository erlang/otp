%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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
-module(snmpa_mib_storage_mnesia).


-behaviour(snmpa_mib_storage).

%%%-----------------------------------------------------------------
%%% This module implements the snmpa_mib_storage behaviour. 
%%% It uses mnesia for storage. 
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


-define(VMODULE,"MS-MNESIA").
-include("snmp_verbosity.hrl").

-record(tab, {id}).


%% ---------------------------------------------------------------
%% open
%% 
%% Open or create a mnesia table. 
%% 
%% Opts    - A list of implementation dependent options
%%           mnesia_open_options() = [mnesia_open_option()]
%%           mnesia_open_option()  = {action, keep | clear} | 
%%                                   {nodes,  [node()]}
%% 
%% ---------------------------------------------------------------

open(Name, RecName, Fields, Type, Opts) ->
    ?vtrace("open ~p table ~p for record ~p",
	    [Type, Name, RecName]),
    Action  = get_action(Opts), 
    Nodes   = get_nodes(Opts), 
    case table_exists(Name) of
	true when (Action =:= keep) -> 
	    ?vtrace("open table ~p - exist (keep)", [Name]),
	    {ok, #tab{id = Name}};
	true when (Action =:= clear) -> 
	    ?vtrace("open table ~p - exist (clear)", [Name]),
	    F = fun() -> mnesia:clear_table(Name) end,
	    case mnesia:transaction(F) of
		{aborted, Reason} ->
		    {error, {clear, Reason}};
		{atomic, _} ->
		    {ok, #tab{id = Name}}
	    end;
	false ->
	    ?vtrace("open table ~p - does not exist", [Name]),
	    Args = [{record_name, RecName}, 
		    {attributes,  Fields},
		    {type,        Type}, 
		    {disc_copies, Nodes}],
	    case mnesia:create_table(Name, Args) of
		{atomic, ok} ->
		    ?vtrace("open table ~p - ok", [Name]),
		    {ok, #tab{id = Name}};
		{aborted, Reason} ->
		    ?vinfo("open table ~p - aborted"
			   "~n   Reason: ~p", [Name, Reason]),
		    {error, {create, Reason}}
	    end
    end.

table_exists(Name) ->
    case (catch mnesia:table_info(Name, type)) of
	{'EXIT', _Reason} ->
	    false;
	_ ->
	    true
    end.
   

%% ---------------------------------------------------------------
%% close
%% 
%% Close the mib-storage table. 
%% This does nothing in the mnesia case.
%% ---------------------------------------------------------------

close(_) ->
    ?vtrace("close mib-storage - ignore",[]),
    ok.


%% ---------------------------------------------------------------
%% read
%% 
%% Retrieve a record from the mib-storage table.
%% ---------------------------------------------------------------

read(#tab{id = ID}, Key) ->
    ?vtrace("read (dirty) from database ~p: ~p", [ID, Key]),
    case (catch mnesia:dirty_read(ID, Key)) of
	[Rec|_] -> {value,Rec};
	_ -> false
    end.
    

%% ---------------------------------------------------------------
%% write
%% 
%% Write a record to the mib-storage table.
%% ---------------------------------------------------------------

write(#tab{id = ID}, Rec) -> 
    ?vtrace("write to database ~p", [ID]),
    F = fun() -> mnesia:write(ID, Rec, write) end,
    case mnesia:transaction(F) of
	{aborted, _Reason} = ABORTED ->
	    {error, ABORTED};
	{atomic,_} ->
	    ok
    end.


%% ---------------------------------------------------------------
%% delete
%% 
%% Delete the mib-storage table. 
%% ---------------------------------------------------------------

delete(#tab{id = ID}) ->
    ?vtrace("delete database: ~p", [ID]),
    mnesia:delete_table(ID).


%% ---------------------------------------------------------------
%% delete
%% 
%% Delete a record from the mib-storage table.
%% ---------------------------------------------------------------

delete(#tab{id = ID}, Key) -> 
    ?vtrace("delete from database ~p: ~p", [ID, Key]),
    F = fun() -> mnesia:delete(ID, Key, write) end,
    case mnesia:transaction(F) of
	{aborted, _Reason} = ABORTED ->
	    {error, ABORTED};
	{atomic, _} ->
	    ok
    end.


%% ---------------------------------------------------------------
%% match_object
%% 
%% Search the mib-storage table for records witch matches 
%% the pattern.
%% ---------------------------------------------------------------

match_object(#tab{id = ID}, Pattern) ->
    ?vtrace("match_object in ~p of ~p", [ID, Pattern]),
    F = fun() -> mnesia:match_object(ID, Pattern, read) end,
    case mnesia:transaction(F) of
	{aborted, _Reason} = ABORTED ->
	    {error, ABORTED};
	{atomic, Rs} ->
	    Rs
    end.
    

%% ---------------------------------------------------------------
%% match_delete
%% 
%% Search the mib-storage table for records witch matches 
%% the pattern and deletes them from the table.
%% ---------------------------------------------------------------

match_delete(#tab{id = ID}, Pattern) -> 
    ?vtrace("match_delete in ~p with pattern ~p", [ID, Pattern]),
    F = fun() -> 
		Recs = mnesia:match_object(ID, Pattern, read),
		lists:foreach(fun(Rec) -> 
				      mnesia:delete_object(ID, Rec, write)
			      end, Recs),
		Recs
	end,
    case mnesia:transaction(F) of
	{aborted, _Reason} = ABORTED ->
	    {error, ABORTED};
	{atomic, Rs} ->
	    Rs
    end.


%% ---------------------------------------------------------------
%% tab2list
%% 
%% Return all records in the mib-storage table in the form of 
%% a list.
%% ---------------------------------------------------------------

tab2list(#tab{id = ID} = Tab) ->
    ?vtrace("tab2list -> list of ~p", [ID]),
    match_object(Tab, mnesia:table_info(ID, wild_pattern)).


%% ---------------------------------------------------------------
%% info
%% 
%% Retrieve implementation dependent mib-storage table 
%% information.
%% ---------------------------------------------------------------

info(#tab{id = ID}) ->
    case (catch mnesia:table_info(ID, all)) of
	Info when is_list(Info) ->
	    Info;
	{'EXIT', {aborted, Reason}} ->
	    {error, Reason}
    end.


info(#tab{id = ID}, Item) ->
    mnesia:table_info(ID, Item).


%% ---------------------------------------------------------------
%% sync
%% 
%% Ignore
%% ---------------------------------------------------------------

sync(_) ->
    ok.


%% ---------------------------------------------------------------
%% backup
%% 
%% Ignore. Mnesia handles its own backups. 
%% ---------------------------------------------------------------

backup(_, _) ->
    ok.

	      
%%----------------------------------------------------------------------

get_action(Opts) ->
    snmp_misc:get_option(action, Opts, keep). 

get_nodes(Opts) ->
    case snmp_misc:get_option(nodes, Opts, erlang:nodes()) of
	[] ->
	    [node()];
	Nodes when is_list(Nodes) ->
	    Nodes;
	all ->
	    erlang:nodes();
	visible ->
	    erlang:nodes(visible);
	connected ->
	    erlang:nodes(connected);
	db_nodes -> 
	    try mnesia:system_info(db_nodes) of
		DbNodes when is_list(DbNodes) ->
		    DbNodes;
		_ ->
		    erlang:nodes()
	    catch 
		_:_ ->
		    erlang:nodes()
	    end
    end.
	    
%% user_err(F, A) ->
%%     snmpa_error:user_err(F, A).
