%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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

-module(mnesia_external_backend_test).

-export([init_per_testcase/2, end_per_testcase/2,
         init_per_group/2, end_per_group/2,
         suite/0, all/0, groups/0]).

-export([
    schema_merge_should_work_when_node_is_removed_from_the_cluster_and_later_rejoins/1
]).

-include("mnesia_test_lib.hrl").

-record(some_rec, {some_id :: atom(), some_int :: number(), some_string :: string()}).

all() -> [
    schema_merge_should_work_when_node_is_removed_from_the_cluster_and_later_rejoins
].

groups() ->
    [].

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

suite() -> [{ct_hooks,[{ts_install_cth,[{nodenames,3}]}]}].

schema_merge_should_work_when_node_is_removed_from_the_cluster_and_later_rejoins(Config) when is_list(Config) ->
    [N1, N2] = ?acquire_nodes(2, Config),

    ?match({atomic,ok}, mnesia:create_table(table, [
        {type, set},
        {record_name, some_rec},
        {attributes, record_info(fields, some_rec)},
        {ext_ets, [N1, N2]}
    ])),

    ?match([], mnesia_test_lib:kill_mnesia([N2])),

    ?match({atomic, ok}, mnesia:del_table_copy(schema, N2)),

    ?match([], mnesia_test_lib:start_mnesia([N2])).
