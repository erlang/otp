%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%
-module(asn1ct_table).

%% Table abstraction module for ASN.1 compiler

-export([new/2]).
-export([new_reuse/2]).
-export([exists/1]).
-export([size/1]).
-export([insert/2]).
-export([lookup/2]).
-export([match/2]).
-export([to_list/1]).
-export([delete/1]). % TODO: Remove (since we run in a separate process)


%% Always creates a new table
new(Table, Options) ->
    case ets:info(Table) of
        undefined ->
            ets:new(Table, Options);
        _  ->
            delete(Table),
            ets:new(Table, Options)
    end.

new_reuse(Table, Options) ->
    not exists(Table) andalso new(Table, Options).

exists(Table) -> ets:info(Table) =/= undefined.

size(Table) -> ets:info(Table, size).

insert(Table, Tuple) -> ets:insert(Table, Tuple).

lookup(Table, Key) -> ets:lookup(Table, Key).

match(Table, MatchSpec) -> ets:match(Table, MatchSpec).

to_list(Table) -> ets:tab2list(Table).

delete(Tables) when is_list(Tables) ->
    [delete(T) || T <- Tables],
    true;
delete(Table) when is_atom(Table) ->
    exists(Table) andalso ets:delete(Table).
