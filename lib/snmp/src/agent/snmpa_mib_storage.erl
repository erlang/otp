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

-module(snmpa_mib_storage).

-export_type([
	      mib_storage_fields/0, 
	      mib_storage_table_type/0, 
	      mib_storage_table_id/0
	     ]).


%%% ----------------------------------------------------------------
%%% This behaviour module defines the API for the mib-storage. 
%%% This is how the agent stores its internal mib-data 
%%% (symbolic-store and mib-server). 
%%%-----------------------------------------------------------------

-type mib_storage_fields()     :: [atom()].
-type mib_storage_table_type() :: set | bag. 
-type mib_storage_table_id()   :: term().


%% ---------------------------------------------------------------
%% open
%% 
%% Open or create a mib-storage table. 
%% If any extra info needs to be communicated to the implementor
%% (of the behaviour), this is done using the *Options* argument. 
%% ---------------------------------------------------------------

%% Options is callback module dependant

-callback open(Name    :: atom(), 
	       RecName :: atom(), 
	       Fields  :: mib_storage_fields(), 
	       Type    :: mib_storage_table_type(), 
	       Options :: list()) ->
    {ok, TabId :: mib_storage_table_id()} | {error, Reason :: term()}.


%% ---------------------------------------------------------------
%% close
%% 
%% Close the mib-storage table. What this does is up to the 
%% implementor (when using mnesia it may be a no-op but for ets 
%% it may actually delete the table). 
%% ---------------------------------------------------------------

-callback close(TabId :: mib_storage_table_id()) ->
    term().


%% ---------------------------------------------------------------
%% read/2
%% 
%% Retrieve a record from the mib-storage table.
%% ---------------------------------------------------------------

-callback read(TabId :: mib_storage_table_id(), 
	       Key   :: term()) ->
    false | {value, Record :: tuple()}.


%% ---------------------------------------------------------------
%% write/2
%% 
%% Write a record to the mib-storage table.
%% ---------------------------------------------------------------

-callback write(TabId  :: mib_storage_table_id(), 
		Record :: tuple()) ->
    ok | {error, Reason :: term()}.


%% ---------------------------------------------------------------
%% delete/1
%% 
%% Delete the mib-storage table. 
%% ---------------------------------------------------------------

-callback delete(TabId :: mib_storage_table_id()) ->
    snmp:void().


%% ---------------------------------------------------------------
%% delete/2
%% 
%% Delete a record from the mib-storage table.
%% ---------------------------------------------------------------

-callback delete(TabId :: mib_storage_table_id(), 
		 Key   :: term()) ->
    ok | {error, Reason :: term()}.


%% ---------------------------------------------------------------
%% match_object
%% 
%% Search the mib-storage table for records which matches 
%% the pattern.
%% ---------------------------------------------------------------

-callback match_object(TabId   :: mib_storage_table_id(), 
		       Pattern :: ets:match_pattern()) ->
    {ok, Recs :: [tuple()]} | {error, Reason :: term()}.


%% ---------------------------------------------------------------
%% match_delete
%% 
%% Search the mib-storage table for records which matches the 
%% pattern and deletes them from the database and return the 
%5 deleted records.
%% ---------------------------------------------------------------

-callback match_delete(TabId   :: mib_storage_table_id(), 
		       Pattern :: ets:match_pattern()) ->
    {ok, Recs :: [tuple()]} | {error, Reason :: term()}.


%% ---------------------------------------------------------------
%% tab2list
%% 
%% Return all records in the table in the form of a list.
%% ---------------------------------------------------------------

-callback tab2list(TabId :: mib_storage_table_id()) ->
    [tuple()].


%% ---------------------------------------------------------------
%% info/1,2
%% 
%% Retrieve implementation dependent mib-storage table 
%% information.
%% ---------------------------------------------------------------

-callback info(TabId :: mib_storage_table_id()) ->
    Info :: term().

-callback info(TabId :: mib_storage_table_id(), Item :: atom()) ->
    Info :: term().


%% ---------------------------------------------------------------
%% sync
%% 
%% Dump mib-storage table to disc (if it has a disk component). 
%% ---------------------------------------------------------------

-callback sync(TabId :: mib_storage_table_id()) ->
    snmp:void().


%% ---------------------------------------------------------------
%% backup
%% 
%% Make a backup copy of the mib-storage table. 
%% ---------------------------------------------------------------

-callback backup(TabId :: mib_storage_table_id(), 
		 Dir   :: file:filename()) ->
    ok | {error, Reason :: term()}.

