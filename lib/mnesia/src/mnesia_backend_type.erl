%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2015. All Rights Reserved.
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
%% Behaviour definition for mnesia backend types.
%%

%%%header_doc_include

-module(mnesia_backend_type).

-export([behaviour_info/1]).

%%%header_doc_include

%%%impl_doc_include

%% Note that mnesia considers all callbacks mandatory!!
%%
behaviour_info(callbacks) ->
    [
     {add_aliases, 1},         % (Aliases) -> ok
     {check_definition, 4},    % (TypeAlias, Tab, Nodes, Properties) -> ok
     {close_table, 2},         % (TypeAlias, Tab) -> ok
     {create_table, 3},        % (TypeAlias, Tab, Properties) -> ok
     {delete, 3},              % (TypeAlias, Tab, Key) -> true | ok
     {delete_table, 2},        % (TypeAlias, Tab) -> ok
     {first, 2},               % (TypeAlias, Tab) -> Key::Term | '$end_of_table'
     {fixtable, 3},            % (TypeAlias, Tab, Bool) -> ok | true
     {last, 2},                % (TypeAlias, Tab) -> Key::Term | '$end_of_table'
     {index_is_consistent,3},  % (TypeAlias, IxTag, Bool) -> ok
     {init_backend, 0},        % () -> ok
     {info, 3},                % (TypeAlias, Tab, Item) -> Term
     {insert, 3},              % (TypeAlias, Tab, Object) -> ok
     {lookup, 3},              % (TypeAlias, Tab, Key) -> [Objects]
     {is_index_consistent,2},  % (TypeAlias, IxTag) -> Bool
     {load_table, 4},          % (TypeAlias, Tab, Reason, CsList) -> ok
     {match_delete, 3},        % (TypeAlias, Tab, Pattern) -> ok
     {next, 3},                % (TypeAlias, Tab, Key) -> Key::Term | '$end_of_table'
     {prev, 3},                % (TypeAlias, Tab, Key) -> Key::Term | '$end_of_table'
     {receiver_first_message, 4}, % (Sender, FirstMsg, Alias, Tab) -> {Size, State}
     {receive_data, 5},        % (Data, Alias, Name, Sender, State) -> {more, State} | {{more, Msg}, State}
     {receive_done, 4},        % (Alias, Tab, Sender, State) -> ok
     {real_suffixes, 0},       % () -> [FileSuffix]
     {remove_aliases, 1},      % (Aliases) -> ok
     {repair_continuation, 2}, % (Continuation, MatchSpec) -> Continuation
     {select, 1},              % (Continuation) -> {[Match], Continuation'} | '$end_of_table'
     {select, 3},              % (TypeAlias, Tab, Pattern) -> {[Match], Continuation'} | '$end_of_table'
     {select, 4},              % (TypeAlias, Tab, MatchSpec, Limit) {[Match], Continuation'} | '$end_of_table'
     {sender_init, 4},         % (TypeAlias, Tab, LoadReason, Pid) ->
					% {standard, Init(), Chunk()} | {Init(), Chunk()}
     {semantics, 2},           % (TypeAlias, storage | types | index_fun | index_types) ->
					% ram_copies | disc_copies, set | ordered_set | bag, fun(), ordered | bag
     {slot, 3},                % (TypeAlias, Tab, Pos) -> '$end_of_table' | Objects | {error, Reason}
     {sync_close_table, 2},    % (TypeAlias, Tab) -> ok
     {tmp_suffixes, 0},        % () -> [FileSuffix]
     {update_counter, 4},      % (TypeAlias, Tab, Counter, Val) -> NewVal
     {validate_key, 6},        % (TypeAlias, Tab, RecName, Arity, Type, Key) -> {RecName, Arity, Type}
     {validate_record, 6}      % (TypeAlias, Tab, RecName, Arity, Type, Obj) -> {RecName, Arity, Type}
    ].

%%%impl_doc_include

%% -type tab() :: atom().
%% -type alias() :: atom().
%% -type rec_name() :: atom().
%% -type type() :: set | bag | ordered_set.
%% -type proplist() :: [{atom(), any()}].
%% -type key() :: any().
%% -type db_object() :: tuple().

%% -type matchspec() :: ets:match_spec().
%% -type limit() :: integer() | infinity.

%% -type cont_fun() :: any().
%% -type cont() :: '$end_of_table' | cont_fun().

%% -callback check_definition(alias(), tab(), [node()], proplist()) -> ok.
%% -callback create_table(alias(), tab(), proplist()) -> tab().
%% -callback load_table(alias(), tab(), any()) -> ok.
%% -callback delete_table(alias(), tab()) -> ok.
%% -callback first(alias(), tab()) -> key().
%% -callback last(alias(), tab()) -> key().
%% -callback next(alias(), tab(), key()) -> key().
%% -callback prev(alias(), tab(), key()) -> key().
%% -callback insert(alias(), tab(), db_object()) -> ok.
%% -callback lookup(alias(), tab(), key()) -> [db_object()].
%% -callback delete(alias(), tab(), key()) -> ok.
%% -callback update_counter(alias(), tab(), key(), integer()) -> integer().
%% -callback select(cont()) -> {list(), cont()}.
%% -callback select(alias(), tab(), matchspec()) -> list() | '$end_of_table'.
%% -callback select(alias(), tab(), matchspec(), limit()) -> {list(), cont()}.
%% -callback slot(alias(), tab(), integer()) -> key().
%% -callback validate_key(alias(), tab(), rec_name(), arity(), type(), key()) ->
%%     {rec_name(), arity(), type()}.
%% -callback validate_record(alias(),tab(),rec_name(),arity(),type(),db_obj()) ->
%%     {rec_name(), arity(), type()}.
%% -callback repair_continuation(cont(), matchspec()) -> cont().
