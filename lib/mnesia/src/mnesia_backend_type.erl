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
     {add_aliases, 1},         % (Aliases)
     {check_definition, 4},    % (TypeAlias, Tab, Nodes, Properties)
     {create_table, 3},        % (TypeAlias, Tab, Properties)
     {delete, 3},              % (TypeAlias, Tab, Key)
     {delete_table, 2},        % (TypeAlias, Tab)
     {load_table, 4},          % (TypeAlias, Tab, Reason, CsList)
     {close_table, 2},         % (TypeAlias, Tab)
     {sync_close_table, 2},    % (TypeAlias, Tab)
     {first, 2},               % (TypeAlias, Tab)
     {fixtable, 3},            % (TypeAlias, Tab, Bool)
     {index_is_consistent,3},  % (TypeAlias, IxTag, Bool)
     {init_backend, 0},        % ()
     {info, 3},                % (TypeAlias, Tab, Item)
     {insert, 3},              % (TypeAlias, Tab, Object)
     {is_index_consistent,2},  % (TypeAlias, IxTag)
     {last, 2},                % (TypeAlias, Tab)
     {lookup, 3},              % (TypeAlias, Tab, Key)
     {match_delete, 3},        % (TypeAlias, Tab, Pattern)
     {next, 3},                % (TypeAlias, Tab, Key)
     {prev, 3},                % (TypeAlias, Tab, Key)
     {real_suffixes, 0},       % ()
     {remove_aliases, 1},      % (Aliases)
     {repair_continuation, 2}, % (Continuation, MatchSpec)
     {select, 1},              % (Continuation)
     {select, 3},              % (TypeAlias, Tab, Pattern)
     {select, 4},              % (TypeAlias, Tab, MatchSpec, Limit)
     {semantics, 2},           % (TypeAlias, storage | types | index_fun)
     {slot, 3},                % (TypeAlias, Tab, Pos)
     {tmp_suffixes, 0},        % ()
     {update_counter, 4},      % (TypeAlias, Tab, Counter, Val)
     {validate_key, 6},        % (TypeAlias, Tab, RecName, Arity, Type, Key)
     {validate_record, 6}      % (TypeAlias, Tab, RecName, Arity, Type, Obj)
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
