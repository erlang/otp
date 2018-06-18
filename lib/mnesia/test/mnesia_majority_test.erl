%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2018. All Rights Reserved.
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
-module(mnesia_majority_test).
-author('ulf.wiger@erlang-solutions.com').
-export([init_per_testcase/2, end_per_testcase/2,
         all/0]).

-export([write/1, wread/1, delete/1, clear_table/1, frag/1,
         change_majority/1, frag_change_majority/1
        ]).

-include("mnesia_test_lib.hrl").

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() ->
    [
     write
     , wread
     , delete
     , clear_table
     , frag
     , change_majority
     , frag_change_majority
    ].

write(suite) -> [];
write(Config) when is_list(Config) ->
    [N1, N2, N3] = ?acquire_nodes(3, Config),
    Tab = t,
    Schema = [{name, Tab}, {ram_copies, [N1,N2,N3]}, {majority,true}],
    ?match({atomic, ok}, mnesia:create_table(Schema)),
    ?match({[ok,ok,ok],[]},
	   rpc:multicall([N1,N2,N3], mnesia, wait_for_tables, [[Tab], 3000])),
    ?match({atomic,ok},
	   mnesia:transaction(fun() -> mnesia:write({t,1,a}) end)),
    mnesia_test_lib:kill_mnesia([N3]),
    ?match({atomic,ok},
	   mnesia:transaction(fun() -> mnesia:write({t,1,a}) end)),
    mnesia_test_lib:kill_mnesia([N2]),
    ?match({aborted,{no_majority,Tab}},
	   mnesia:transaction(fun() -> mnesia:write({t,1,a}) end)).

wread(suite) -> [];
wread(Config) when is_list(Config) ->
    [N1, N2] = ?acquire_nodes(2, Config),
    Tab = t,
    Schema = [{name, Tab}, {ram_copies, [N1,N2]}, {majority,true}],
    ?match({atomic, ok}, mnesia:create_table(Schema)),
    ?match({[ok,ok],[]},
	   rpc:multicall([N1,N2], mnesia, wait_for_tables, [[Tab], 3000])),
    ?match({atomic,[]},
	   mnesia:transaction(fun() -> mnesia:read(t,1,write) end)),
    mnesia_test_lib:kill_mnesia([N2]),
    ?match({aborted,{no_majority,Tab}},
	   mnesia:transaction(fun() -> mnesia:read(t,1,write) end)).

delete(suite) -> [];
delete(Config) when is_list(Config) ->
    [N1, N2] = ?acquire_nodes(2, Config),
    Tab = t,
    Schema = [{name, Tab}, {ram_copies, [N1,N2]}, {majority,true}],
    ?match({atomic, ok}, mnesia:create_table(Schema)),
    ?match({[ok,ok],[]},
	   rpc:multicall([N1,N2], mnesia, wait_for_tables, [[Tab], 3000])),
    %% works as expected with majority of nodes present
    ?match({atomic,ok},
	   mnesia:transaction(fun() -> mnesia:write({t,1,a}) end)),
    ?match({atomic,ok},
	   mnesia:transaction(fun() -> mnesia:delete({t,1}) end)),
    ?match({atomic,[]},
	   mnesia:transaction(fun() -> mnesia:read({t,1}) end)),
    %% put the record back
    ?match({atomic,ok},
	   mnesia:transaction(fun() -> mnesia:write({t,1,a}) end)),
    ?match({atomic,[{t,1,a}]},
	   mnesia:transaction(fun() -> mnesia:read({t,1}) end)),
    mnesia_test_lib:kill_mnesia([N2]),
    ?match({aborted,{no_majority,Tab}},
	   mnesia:transaction(fun() -> mnesia:delete({t,1}) end)).

clear_table(suite) -> [];
clear_table(Config) when is_list(Config) ->
    [N1, N2] = ?acquire_nodes(2, Config),
    Tab = t,
    Schema = [{name, Tab}, {ram_copies, [N1,N2]}, {majority,true}],
    ?match({atomic, ok}, mnesia:create_table(Schema)),
    ?match({[ok,ok],[]},
	   rpc:multicall([N1,N2], mnesia, wait_for_tables, [[Tab], 3000])),
    %% works as expected with majority of nodes present
    ?match({atomic,ok},
	   mnesia:transaction(fun() -> mnesia:write({t,1,a}) end)),
    ?match({atomic,ok}, mnesia:clear_table(t)),
    ?match({atomic,[]},
	   mnesia:transaction(fun() -> mnesia:read({t,1}) end)),
    %% put the record back
    ?match({atomic,ok},
	   mnesia:transaction(fun() -> mnesia:write({t,1,a}) end)),
    ?match({atomic,[{t,1,a}]},
	   mnesia:transaction(fun() -> mnesia:read({t,1}) end)),
    mnesia_test_lib:kill_mnesia([N2]),
    ?match({aborted,{no_majority,Tab}}, mnesia:clear_table(t)).

frag(suite) -> [];
frag(Config) when is_list(Config) ->
    [N1] = ?acquire_nodes(1, Config),
    Tab = t,
    Schema = [
	      {name, Tab}, {ram_copies, [N1]},
	      {majority,true},
	      {frag_properties, [{n_fragments, 2}]}
	     ],
    ?match({atomic, ok}, mnesia:create_table(Schema)),
    ?match(true, mnesia:table_info(t, majority)),
    ?match(true, mnesia:table_info(t_frag2, majority)).

change_majority(suite) -> [];
change_majority(Config) when is_list(Config) ->
    [N1,N2] = ?acquire_nodes(2, Config),
    Tab = t,
    Schema = [
	      {name, Tab}, {ram_copies, [N1,N2]},
	      {majority,false}
	     ],
    ?match({atomic, ok}, mnesia:create_table(Schema)),
    ?match(false, mnesia:table_info(t, majority)),
    ?match({atomic, ok},
	   mnesia:change_table_majority(t, true)),
    ?match(true, mnesia:table_info(t, majority)),
    ?match(ok,
	   mnesia:activity(transaction, fun() ->
						mnesia:write({t,1,a})
					end)),
    mnesia_test_lib:kill_mnesia([N2]),
    ?match({'EXIT',{aborted,{no_majority,_}}},
	   mnesia:activity(transaction, fun() ->
						mnesia:write({t,1,a})
					end)).

frag_change_majority(suite) -> [];
frag_change_majority(Config) when is_list(Config) ->
    [N1,N2] = ?acquire_nodes(2, Config),
    Tab = t,
    Schema = [
	      {name, Tab}, {ram_copies, [N1,N2]},
	      {majority,false},
	      {frag_properties,
	       [{n_fragments, 2},
		{n_ram_copies, 2},
		{node_pool, [N1,N2]}]}
	     ],
    ?match({atomic, ok}, mnesia:create_table(Schema)),
    ?match(false, mnesia:table_info(t, majority)),
    ?match(false, mnesia:table_info(t_frag2, majority)),
    ?match({aborted,{bad_type,t_frag2}},
	   mnesia:change_table_majority(t_frag2, true)),
    ?match({atomic, ok},
	   mnesia:change_table_majority(t, true)),
    ?match(true, mnesia:table_info(t, majority)),
    ?match(true, mnesia:table_info(t_frag2, majority)),
    ?match(ok,
	   mnesia:activity(transaction, fun() ->
						mnesia:write({t,1,a})
					end, mnesia_frag)),
    mnesia_test_lib:kill_mnesia([N2]),
    ?match({'EXIT',{aborted,{no_majority,_}}},
	   mnesia:activity(transaction, fun() ->
						mnesia:write({t,1,a})
					end, mnesia_frag)).
