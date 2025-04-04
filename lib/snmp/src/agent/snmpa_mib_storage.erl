%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2013-2025. All Rights Reserved.
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
-moduledoc """
Behaviour module for the SNMP agent mib storage.

This module defines the behaviour of the SNMP agent mib storage.

The mib storage is used by the agent to store internal mib- related information.
The mib storage module is used by several entities, not just the mib-server.

A `snmpa_mib_storage` compliant module must export the following functions:

- [open/5](`c:snmpa_mib_storage:open/5`)
- [close/1](`c:snmpa_mib_storage:close/1`)
- [read/2](`c:snmpa_mib_storage:read/2`)
- [write/2](`c:snmpa_mib_storage:write/2`)
- [delete/1](`c:snmpa_mib_storage:delete/1`)
- [delete/2](`c:snmpa_mib_storage:delete/2`)
- [match_object/2](`c:snmpa_mib_storage:match_object/2`)
- [match_delete/2](`c:snmpa_mib_storage:match_delete/2`)
- [tab2list/1](`c:snmpa_mib_storage:tab2list/1`)
- [info/1](`c:snmpa_mib_storage:info/1`)
- [info/2](`c:snmpa_mib_storage:info/2`)
- [sync/1](`c:snmpa_mib_storage:sync/1`)
- [backup/2](`c:snmpa_mib_storage:backup/2`)

The semantics of them and their exact signatures are explained below.
""".
-moduledoc(#{since => "OTP R16B01"}).

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

%% Options is callback module dependent

-doc """
Create or open a mib storage table.

Note that the `RecordName` and `Fields` arguments my not be used in all
implementations (they are actually only needed for mnesia-based
implementations).

Note also that the `Options` argument comes from the `options` config option of
the mib-storage config option, and is passed on as is.
""".
-doc(#{since => <<"OTP R16B01">>}).
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

-doc """
Close the mib-storage table.
""".
-doc(#{since => <<"OTP R16B01">>}).
-callback close(TabId :: mib_storage_table_id()) ->
    term().


%% ---------------------------------------------------------------
%% read/2
%% 
%% Retrieve a record from the mib-storage table.
%% ---------------------------------------------------------------

-doc """
Read a record from the mib-storage table.
""".
-doc(#{since => <<"OTP R16B01">>}).
-callback read(TabId :: mib_storage_table_id(), 
	       Key   :: term()) ->
    false | {value, Record :: tuple()}.


%% ---------------------------------------------------------------
%% write/2
%% 
%% Write a record to the mib-storage table.
%% ---------------------------------------------------------------

-doc """
Write a record to the mib-storage table.
""".
-doc(#{since => <<"OTP R16B01">>}).
-callback write(TabId  :: mib_storage_table_id(), 
		Record :: tuple()) ->
    ok | {error, Reason :: term()}.


%% ---------------------------------------------------------------
%% delete/1
%% 
%% Delete the mib-storage table. 
%% ---------------------------------------------------------------

-doc """
Delete an entire mib-storage table.
""".
-doc(#{since => <<"OTP R16B01">>}).
-callback delete(TabId :: mib_storage_table_id()) ->
    snmp:void().


%% ---------------------------------------------------------------
%% delete/2
%% 
%% Delete a record from the mib-storage table.
%% ---------------------------------------------------------------

-doc """
Delete a record from the mib-storage table.
""".
-doc(#{since => <<"OTP R16B01">>}).
-callback delete(TabId :: mib_storage_table_id(), 
		 Key   :: term()) ->
    ok | {error, Reason :: term()}.


%% ---------------------------------------------------------------
%% match_object
%% 
%% Search the mib-storage table for records which matches 
%% the pattern.
%% ---------------------------------------------------------------

-doc """
Search the mib-storage table for record that match the specified pattern.
""".
-doc(#{since => <<"OTP R16B01">>}).
-callback match_object(TabId   :: mib_storage_table_id(), 
		       Pattern :: ets:match_pattern()) ->
    Recs :: [tuple()] | {error, Reason :: term()}.


%% ---------------------------------------------------------------
%% match_delete
%% 
%% Search the mib-storage table for records which matches the 
%% pattern and deletes them from the database and return the 
%5 deleted records.
%% ---------------------------------------------------------------

-doc """
Search the mib-storage table for record that match the specified pattern and
then delete them. The records deleted are also returned.
""".
-doc(#{since => <<"OTP R16B01">>}).
-callback match_delete(TabId   :: mib_storage_table_id(), 
		       Pattern :: ets:match_pattern()) ->
    Recs :: [tuple()] | {error, Reason :: term()}.


%% ---------------------------------------------------------------
%% tab2list
%% 
%% Return all records in the table in the form of a list.
%% ---------------------------------------------------------------

-doc """
Return all records in the mib-storage table in the form of a list.
""".
-doc(#{since => <<"OTP R16B01">>}).
-callback tab2list(TabId :: mib_storage_table_id()) ->
    [tuple()].


%% ---------------------------------------------------------------
%% info/1,2
%% 
%% Retrieve implementation dependent mib-storage table 
%% information.
%% ---------------------------------------------------------------

-doc """
Retrieve implementation dependent mib-storage table information.
""".
-doc(#{since => <<"OTP R16B01">>}).
-callback info(TabId :: mib_storage_table_id()) ->
    Info :: term().

-doc(#{since => <<"OTP R16B01">>}).
-callback info(TabId :: mib_storage_table_id(), Item :: atom()) ->
    Info :: term().


%% ---------------------------------------------------------------
%% sync
%% 
%% Dump mib-storage table to disc (if it has a disk component). 
%% ---------------------------------------------------------------

-doc """
Synchronize the mib-storage table.

What this means, if anything, is implementation dependent.
""".
-doc(#{since => <<"OTP R16B01">>}).
-callback sync(TabId :: mib_storage_table_id()) ->
    snmp:void().


%% ---------------------------------------------------------------
%% backup
%% 
%% Make a backup copy of the mib-storage table. 
%% ---------------------------------------------------------------

-doc """
Perform a backup of the mib-storage table.

What this means, if anything, is implementation dependent.
""".
-doc(#{since => <<"OTP R16B01">>}).
-callback backup(TabId :: mib_storage_table_id(), 
		 Dir   :: file:filename()) ->
    ok | {error, Reason :: term()}.

