%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
%%
-module(asn1ct_table).

%% Table abstraction module for ASN.1 compiler

-export([new/1]).
-export([new_reuse/1]).
-export([exists/1]).
-export([size/1]).
-export([insert/2]).
-export([lookup/2]).
-export([match/2]).
-export([to_list/1]).
-export([delete/1]).


%% Always create a new table.
new(Table) ->
    undefined = get(Table),			%Assertion.
    TableId = ets:new(Table, []),
    put(Table, TableId).

%% Only create it if it doesn't exist yet.
new_reuse(Table) ->
    not exists(Table) andalso new(Table).

exists(Table) -> get(Table) =/= undefined.

size(Table) -> ets:info(get(Table), size).

insert(Table, Tuple) -> ets:insert(get(Table), Tuple).

lookup(Table, Key) -> ets:lookup(get(Table), Key).

match(Table, MatchSpec) -> ets:match(get(Table), MatchSpec).

to_list(Table) -> ets:tab2list(get(Table)).

%% Deleting tables is no longer strictly necessary since each compilation
%% runs in separate process, but it will reduce memory consumption
%% especially when many compilations are run in parallel.

delete(Tables) when is_list(Tables) ->
    [delete(T) || T <- Tables],
    true;
delete(Table) when is_atom(Table) ->
    case erase(Table) of
        undefined ->
            true;
        TableId ->
            ets:delete(TableId)
    end.
