%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(mnesia_trans_access_test).
-author('hakan@erix.ericsson.se').

-export([init_per_testcase/2, end_per_testcase/2,
         init_per_group/2, end_per_group/2,
         all/0, groups/0]).

-export([write/1, read/1, wread/1, delete/1, delete_object/1,
         match_object/1, select/1, select14/1, all_keys/1, transaction/1,
         basic_nested/1, mix_of_nested_activities/1,
         nested_trans_both_ok/1, nested_trans_child_dies/1,
         nested_trans_parent_dies/1, nested_trans_both_dies/1,
         index_match_object/1, index_read/1,index_write/1,
         index_update_set/1, index_update_bag/1,
         add_table_index_ram/1, add_table_index_disc/1,
         add_table_index_disc_only/1, create_live_table_index_ram/1,
         create_live_table_index_disc/1,
         create_live_table_index_disc_only/1, del_table_index_ram/1,
         del_table_index_disc/1, del_table_index_disc_only/1,
         idx_schema_changes_ram/1, idx_schema_changes_disc/1,
         idx_schema_changes_disc_only/1]).

-export([do_nested/1]).

-include("mnesia_test_lib.hrl").

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

-define(receive_messages(Msgs), mnesia_recovery_test:receive_messages(Msgs, ?FILE, ?LINE)).

% First Some debug logging
-define(dgb, true).
-ifdef(dgb).
-define(dl(X, Y), ?verbose("**TRACING: " ++ X ++ "**~n", Y)).
-else.
-define(dl(X, Y), ok).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() ->
    [write, read, wread, delete, delete_object,
     match_object, select, select14, all_keys, transaction,
     {group, nested_activities}, {group, index_tabs},
     {group, index_lifecycle}].

groups() ->
    [{nested_activities, [],
      [basic_nested, {group, nested_transactions},
       mix_of_nested_activities]},
     {nested_transactions, [],
      [nested_trans_both_ok, nested_trans_child_dies,
       nested_trans_parent_dies, nested_trans_both_dies]},
     {index_tabs, [],
      [index_match_object, index_read, {group, index_update},
       index_write]},
     {index_update, [],
      [index_update_set, index_update_bag]},
     {index_lifecycle, [],
      [add_table_index_ram, add_table_index_disc,
       add_table_index_disc_only, create_live_table_index_ram,
       create_live_table_index_disc,
       create_live_table_index_disc_only, del_table_index_ram,
       del_table_index_disc, del_table_index_disc_only,
       {group, idx_schema_changes}]},
     {idx_schema_changes, [],
      [idx_schema_changes_ram, idx_schema_changes_disc,
       idx_schema_changes_disc_only]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% Write records

write(suite) -> [];
write(Config) when is_list(Config) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config),
    Tab = write,
    Schema = [{name, Tab}, {attributes, [k, v]}, {ram_copies, [Node1]}],
    ?match({atomic, ok},  mnesia:create_table(Schema)),

    ?match({aborted, {bad_type, _}},
	   mnesia:transaction(fun() -> mnesia:write([]) end)),
    ?match({aborted, {bad_type, _}},
	   mnesia:transaction(fun() -> mnesia:write({Tab, 2}) end)),
    ?match({aborted, _},
	   mnesia:transaction(fun() -> mnesia:write({foo, 2}) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write({Tab, 1, 2}) end)),

    ?match({'EXIT', {aborted, no_transaction}},  mnesia:write({Tab, 1, 2})),
    ?verify_mnesia(Nodes, []).

%% Read records

read(suite) -> [];
read(Config) when is_list(Config) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config),
    Tab = read,
    Schema = [{name, Tab}, {type, bag}, {attributes, [k, v]}, {ram_copies, [Node1]}],
    ?match({atomic, ok},  mnesia:create_table(Schema)),

    OneRec = {Tab, 1, 2},
    TwoRec = {Tab, 1, 3},
    ?match({aborted, {bad_type, _}},
	   mnesia:transaction(fun() -> mnesia:read([]) end)),
    ?match({aborted, {bad_type, _}},
	   mnesia:transaction(fun() -> mnesia:read({Tab}) end)),
    ?match({aborted, {bad_type, _}}
	   ,  mnesia:transaction(fun() -> mnesia:read(OneRec) end)),
    ?match({atomic, []},
	   mnesia:transaction(fun() -> mnesia:read({Tab, 1}) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(OneRec) end)),
    ?match({atomic, [OneRec]},
	   mnesia:transaction(fun() -> mnesia:read({Tab, 1}) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(TwoRec) end)),
    ?match({atomic, [OneRec, TwoRec]},
	   mnesia:transaction(fun() -> mnesia:read({Tab, 1}) end)),

    ?match({'EXIT', {aborted, no_transaction}},  mnesia:read({Tab, 1})),
    ?verify_mnesia(Nodes, []).

%% Read records and set write lock

wread(suite) -> [];
wread(Config) when is_list(Config) ->
    [_N1,N2] = Nodes = ?acquire_nodes(2, Config),
    Tab = wread,
    Schema = [{name, Tab}, {type, set}, {attributes, [k, v]}, {ram_copies, Nodes}],
    ?match({atomic, ok},  mnesia:create_table(Schema)),

    OneRec = {Tab, 1, 2},
    TwoRec = {Tab, 1, 3},
    ?match({aborted, {bad_type, _}},
	   mnesia:transaction(fun() -> mnesia:wread([]) end)),
    ?match({aborted, {bad_type, _}},
	   mnesia:transaction(fun() -> mnesia:wread({Tab}) end)),
    ?match({aborted, {bad_type, _}}
	   ,  mnesia:transaction(fun() -> mnesia:wread(OneRec) end)),

    ?match({atomic, []},
	   mnesia:transaction(fun() -> mnesia:wread({Tab, 1}) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(OneRec) end)),

    ?match({atomic, [OneRec]},
	   mnesia:transaction(fun() -> mnesia:wread({Tab, 1}) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(TwoRec) end)),
    ?match({atomic, [TwoRec]},
	   mnesia:transaction(fun() -> mnesia:wread({Tab, 1}) end)),

    ?match({'EXIT', {aborted, no_transaction}},  mnesia:wread({Tab, 1})),

    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(Tab, {Tab, 42, a}, sticky_write) end)),
    ?match({atomic, [{Tab,42, a}]},
	   rpc:call(N2, mnesia, transaction, [fun() -> mnesia:wread({Tab, 42}) end])),
    ?verify_mnesia(Nodes, []).

%% Delete record

delete(suite) -> [];
delete(Config) when is_list(Config) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config),
    Tab = delete,
    Schema = [{name, Tab}, {type, bag}, {attributes, [k, v]}, {ram_copies, [Node1]}],
    ?match({atomic, ok},  mnesia:create_table(Schema)),

    ?match({aborted, {bad_type, _}},
	   mnesia:transaction(fun() -> mnesia:delete([]) end)),
    ?match({aborted, {bad_type, _}},
	   mnesia:transaction(fun() -> mnesia:delete({Tab}) end)),
    ?match({aborted, {bad_type, _}}
	   ,  mnesia:transaction(fun() -> mnesia:delete({Tab, 1, 2}) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:delete({Tab, 1}) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write({Tab, 1, 2}) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:delete({Tab, 1}) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write({Tab, 1, 2}) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write({Tab, 1, 2}) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:delete({Tab, 1}) end)),

    ?match({'EXIT', {aborted, no_transaction}},  mnesia:delete({Tab, 1})),
    ?verify_mnesia(Nodes, []).

%% Delete matching record

delete_object(suite) -> [];
delete_object(Config) when is_list(Config) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config),
    Tab = delete_object,
    Schema = [{name, Tab}, {type, bag}, {attributes, [k, v]}, {ram_copies, [Node1]}],
    ?match({atomic, ok},  mnesia:create_table(Schema)),

    OneRec = {Tab, 1, 2},
    ?match({aborted, {bad_type, _}},
	   mnesia:transaction(fun() -> mnesia:delete_object([]) end)),
    ?match({aborted, {bad_type, _}},
	   mnesia:transaction(fun() -> mnesia:delete_object({Tab}) end)),
    ?match({aborted, {bad_type, _}},
	   mnesia:transaction(fun() -> mnesia:delete_object({Tab, 1}) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:delete_object(OneRec) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(OneRec) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:delete_object(OneRec) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(OneRec) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(OneRec) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:delete_object(OneRec) end)),

    ?match({'EXIT', {aborted, no_transaction}},  mnesia:delete_object(OneRec)),

    ?match({aborted, {bad_type, Tab, _}},
	   mnesia:transaction(fun() -> mnesia:delete_object({Tab, {['_']}, 21}) end)),
    ?match({aborted, {bad_type, Tab, _}},
	   mnesia:transaction(fun() -> mnesia:delete_object({Tab, {['$5']}, 21}) end)),

    ?verify_mnesia(Nodes, []).

%% Read matching records

match_object(suite) -> [];
match_object(Config) when is_list(Config) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config),
    Tab = match,
    Schema = [{name, Tab}, {attributes, [k, v]}, {ram_copies, [Node1]}],
    ?match({atomic, ok},  mnesia:create_table(Schema)),

    OneRec = {Tab, 1, 2},
    OnePat = {Tab, '$1', 2},
    ?match({atomic, []},
	   mnesia:transaction(fun() -> mnesia:match_object(OnePat) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(OneRec) end)),
    ?match({atomic, [OneRec]},
	   mnesia:transaction(fun() -> mnesia:match_object(OnePat) end)),

    ?match({aborted, _},
	   mnesia:transaction(fun() -> mnesia:match_object({foo, '$1', 2}) end)),
    ?match({aborted, _},
	   mnesia:transaction(fun() -> mnesia:match_object({[], '$1', 2}) end)),

    ?match({'EXIT', {aborted, no_transaction}},  mnesia:match_object(OnePat)),
    ?verify_mnesia(Nodes, []).

%% select
select(suite) -> [];
select(Config) when is_list(Config) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config),
    Tab = match,
    Schema = [{name, Tab}, {attributes, [k, v]}, {ram_copies, [Node1]}],
    ?match({atomic, ok},  mnesia:create_table(Schema)),

    OneRec = {Tab, 1, 2},
    TwoRec = {Tab, 2, 3},
    OnePat = [{{Tab, '$1', 2}, [], ['$_']}],
    ?match({atomic, []},
	   mnesia:transaction(fun() -> mnesia:select(Tab, OnePat) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(OneRec) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(TwoRec) end)),
    ?match({atomic, [OneRec]},
	   mnesia:transaction(fun() -> mnesia:select(Tab, OnePat) end)),

    ?match({aborted, _},
	   mnesia:transaction(fun() -> mnesia:select(Tab, {match, '$1', 2}) end)),
    ?match({aborted, _},
	   mnesia:transaction(fun() -> mnesia:select(Tab, [{'_', [], '$1'}]) end)),

    ?match({'EXIT', {aborted, no_transaction}},  mnesia:select(Tab, OnePat)),
    ?verify_mnesia(Nodes, []).


%% more select
select14(suite) -> [];
select14(Config) when is_list(Config) ->
    [Node1,Node2] = Nodes = ?acquire_nodes(2, Config),
    Tab1 = select14_ets,
    Tab2 = select14_dets,
    Tab3 = select14_remote,
    Tab4 = select14_remote_dets,
    Schemas = [[{name, Tab1}, {attributes, [k, v]}, {ram_copies, [Node1]}],
	       [{name, Tab2}, {attributes, [k, v]}, {disc_only_copies, [Node1]}],
	       [{name, Tab3}, {attributes, [k, v]}, {ram_copies, [Node2]}],
	       [{name, Tab4}, {attributes, [k, v]}, {disc_only_copies, [Node2]}]],
    [?match({atomic, ok},  mnesia:create_table(Schema)) || Schema <- Schemas],

    %% Some Helpers
    Trans = fun(Fun) -> mnesia:transaction(Fun) end,
    Dirty = fun(Fun) -> mnesia:async_dirty(Fun) end,
    LoopHelp = fun('$end_of_table',_) -> [];
		  ({Recs,Cont},Fun) ->
		       Sel = mnesia:select(Cont),
		       Recs ++ Fun(Sel, Fun)
	       end,
    Loop = fun(Table,Pattern) ->
		   Sel = mnesia:select(Table, Pattern, 1, read),
		   Res = LoopHelp(Sel,LoopHelp),
		   case mnesia:table_info(Table, type) of
		       ordered_set -> Res;
		       _ -> lists:sort(Res)
		   end
	   end,
    Test =
	fun(Tab) ->
		OneRec = {Tab, 1, 2},
		TwoRec = {Tab, 2, 3},
		OnePat = [{{Tab, '$1', 2}, [], ['$_']}],
		All = [OneRec,TwoRec],
		AllPat = [{'_', [], ['$_']}],

		?match({atomic, []}, Trans(fun() -> Loop(Tab, OnePat) end)),
		?match({atomic, ok},   mnesia:transaction(fun() -> mnesia:write(OneRec) end)),
		?match({atomic, ok},   mnesia:transaction(fun() -> mnesia:write(TwoRec) end)),
		?match({atomic, [OneRec]}, Trans(fun() -> Loop(Tab, OnePat) end)),
		?match({atomic, All}, Trans(fun() -> Loop(Tab, AllPat) end)),

		{atomic,{_, ContOne}} = Trans(fun() -> mnesia:select(Tab, OnePat, 1, read) end),
		?match({aborted, wrong_transaction}, Trans(fun() -> mnesia:select(ContOne) end)),
		?match('$end_of_table',              Dirty(fun() -> mnesia:select(ContOne) end)),

		{atomic,{_, ContAll}} = Trans(fun() -> mnesia:select(Tab, AllPat, 1, read) end),
		?match({aborted, wrong_transaction}, Trans(fun() -> mnesia:select(ContAll) end)),
		?match({[_], _},                     Dirty(fun() -> mnesia:select(ContAll) end)),

		?match({aborted, _}, Trans(fun() -> mnesia:select(Tab, {match, '$1', 2},1,read) end)),
		?match({aborted, _}, Trans(fun() -> mnesia:select(Tab, [{'_', [], '$1'}],1,read) end)),
		?match({aborted, _}, Trans(fun() -> mnesia:select(sune) end)),
		?match({'EXIT', {aborted, no_transaction}},  mnesia:select(Tab, OnePat,1,read)),
		?match({aborted, {badarg,sune}},
		       Trans(fun() -> mnesia:select(sune) end))
	end,
    Test(Tab1),
    Test(Tab2),
    Test(Tab3),
    Test(Tab4),
    ?verify_mnesia(Nodes, []).


%% Pick all keys from table

all_keys(suite) ->[];
all_keys(Config) when is_list(Config) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config),
    Tab = all_keys,
    Schema = [{name, Tab}, {type, bag}, {attributes, [k, v]}, {ram_copies, [Node1]}],
    ?match({atomic, ok},  mnesia:create_table(Schema)),

    Write = fun() -> mnesia:write({Tab, 14, 4}) end,
    AllKeys = fun() -> mnesia:all_keys(Tab) end,

    ?match({atomic, []},  mnesia:transaction(AllKeys)),

    ?match({atomic, ok},  mnesia:transaction(Write)),
    ?match({atomic, [14]},  mnesia:transaction(AllKeys)),

    ?match({atomic, ok},  mnesia:transaction(Write)),
    ?match({atomic, [14]},  mnesia:transaction(AllKeys)),

    ?match({aborted, _},
	   mnesia:transaction(fun() -> mnesia:all_keys(foo) end)),
    ?match({aborted, _},
	   mnesia:transaction(fun() -> mnesia:all_keys([]) end)),

    ?match({'EXIT', {aborted, no_transaction}},  mnesia:all_keys(Tab)),
    ?verify_mnesia(Nodes, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Use and misuse transactions

transaction(suite) -> [];
transaction(Config) when is_list(Config) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config),
    ?match({atomic, ali_baba}, mnesia:transaction(fun() -> ali_baba end)),
    ?match({aborted, _}, mnesia:transaction(no_fun)),
    ?match({aborted, _}, mnesia:transaction(?MODULE, no_fun, [foo])),

    {success, [A, B, C, D, E, F, G, H]} =
        ?start_activities(lists:duplicate(8, Node1)),
    ?start_transactions([A, B, C, D, E, F, G, H]),

    A ! fun() -> mnesia:abort(abort_bad_trans) end,
    ?match_receive({A, {aborted, abort_bad_trans}}),

    B ! fun() -> erlang:error(exit_here) end,
    ?match_receive({B, {aborted, _}}),

    C ! fun() -> throw(throw_bad_trans) end,
    ?match_receive({C, {aborted, {throw, throw_bad_trans}}}),

    D ! fun() -> exit(exit_bad_trans) end,
    ?match_receive({D, {aborted, exit_bad_trans}}),

    E ! fun() -> exit(normal) end,
    ?match_receive({E, {aborted, normal}}),

    F ! fun() -> exit(abnormal) end,
    ?match_receive({F, {aborted, abnormal}}),

    G ! fun() -> exit(G, abnormal) end,
    ?match_receive({'EXIT', G, abnormal}),

    H ! fun() -> exit(H, kill) end,
    ?match_receive({'EXIT', H, killed}),

    ?match({atomic, ali_baba},
           mnesia:transaction(fun() -> ali_baba end, infinity)),
    ?match({atomic, ali_baba}, mnesia:transaction(fun() -> ali_baba end, 1)),
    ?match({atomic, ali_baba}, mnesia:transaction(fun() -> ali_baba end, 0)),
    ?match({aborted, Reason8} when element(1, Reason8) == badarg, mnesia:transaction(fun() -> ali_baba end, -1)),
    ?match({aborted, Reason1} when element(1, Reason1) == badarg, mnesia:transaction(fun() -> ali_baba end, foo)),
    Fun = fun() ->
		  ?match(true, mnesia:is_transaction()),
		  ?match({atomic, ok},
			 mnesia:transaction(fun() -> ?match(true, mnesia:is_transaction()),ok end)), ok end,
    ?match({atomic, ok}, mnesia:transaction(Fun)),
    ?verify_mnesia(Nodes, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ensure that nested transactions behave correctly
%% We create a particular table that is used by this test only
-record(ntab,  {a,  b}).
basic_nested(doc) -> ["Test the basic functionality of nested transactions"];
basic_nested(suite) -> [];
basic_nested(Config) when is_list(Config) ->
    Nodes = ?acquire_nodes(3, Config),
    Args =  [{ram_copies,  Nodes},
	     {attributes,  record_info(fields,  ntab)}],
    ?match({atomic,  ok},  mnesia:create_table(ntab,  Args)),
    do_nested(top),
    case mnesia_test_lib:diskless(Config) of
	false ->
	    lists:foreach(fun(N) ->
				  ?match({atomic,  ok},
					 mnesia:change_table_copy_type(ntab,  N,  disc_only_copies))
			  end,  Nodes),
	    do_nested(top);
	true ->
	    skip
    end,
    ?verify_mnesia(Nodes, []).

do_nested(How) ->
    F1 = fun() ->
		mnesia:write(#ntab{a= 1}),
		mnesia:write(#ntab{a= 2})
	end,
    F2 = fun() ->
		 mnesia:read({ntab,  1})
	 end,
    ?match({atomic,  ok},  mnesia:transaction(F1)),
    ?match({atomic,  _},  mnesia:transaction(F2)),

    ?match({atomic,  {aborted,  _}},
	   mnesia:transaction(fun() -> n_f1(),
				mnesia:transaction(fun() -> n_f2() end)
		       end)),

    ?match({atomic,  {aborted,  _}},
	   mnesia:transaction(fun() -> n_f1(),
				mnesia:transaction(fun() -> n_f3() end)
		       end)),
    ?match({atomic,  {atomic,  [#ntab{a = 5}]}},
	   mnesia:transaction(fun() -> mnesia:write(#ntab{a = 5}),
				       mnesia:transaction(fun() -> n_f4() end)
			      end)),
    Cyclic = fun() -> mnesia:abort({cyclic,a,a,a,a,a}) end,  %% Ugly
    NodeNotR = fun() -> mnesia:abort({node_not_running, testNode}) end,

    TestAbort = fun(Fun) ->
			case get(restart_counter) of
			    undefined ->
				put(restart_counter, 1),
				Fun();
			    _ ->
				erase(restart_counter),
				ok
			end
		end,

    ?match({atomic,{atomic,ok}},
	   mnesia:transaction(fun()->mnesia:transaction(TestAbort,
							[Cyclic])end)),

    ?match({atomic,{atomic,ok}},
	   mnesia:transaction(fun()->mnesia:transaction(TestAbort,
							[NodeNotR])end)),

    %% Now try the restart thingie
    case How of
	top ->
	    Pids = [spawn(?MODULE,  do_nested,  [{spawned,  self()}]),
		    spawn(?MODULE,  do_nested,  [{spawned,  self()}]),
		    spawn(?MODULE,  do_nested,  [{spawned,  self()}]),
		    spawn(?MODULE,  do_nested,  [{spawned,  self()}])],
	    ?match({info, _, _}, mnesia_tm:get_info(2000)),
	    lists:foreach(fun(P) -> receive
					{P,  ok} -> ok
				    end
			  end,  Pids),
	    ?match([],  [Tab || Tab <- ets:all(), mnesia_trans_store == ets:info(Tab, name)]);

	{spawned,  Pid} ->
	    ?match({info, _, _}, mnesia_tm:get_info(2000)),
	    Pid ! {self(),  ok},
	    exit(normal)
    end.


n_f1() ->
    mnesia:read({ntab,  1}),
    mnesia:write(#ntab{a = 3}).

n_f2() ->
    mnesia:write(#ntab{a = 4}),
    erlang:error(exit_here).

n_f3() ->
    mnesia:write(#ntab{a = 4}),
    throw(funky).

n_f4() ->
    mnesia:read({ntab,  5}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nested_trans_both_ok(suite) -> [];
nested_trans_both_ok(Config) when is_list(Config) ->
    nested_transactions(Config, ok, ok).

nested_trans_child_dies(suite) -> [];
nested_trans_child_dies(Config) when is_list(Config) ->
    nested_transactions(Config, abort, ok).

nested_trans_parent_dies(suite) -> [];
nested_trans_parent_dies(Config) when is_list(Config) ->
    nested_transactions(Config, ok, abort).

nested_trans_both_dies(suite) -> [];
nested_trans_both_dies(Config) when is_list(Config) ->
    nested_transactions(Config, abort, abort).

nested_transactions(Config, Child, Father) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Tab = nested_trans,

    Def =
	case mnesia_test_lib:diskless(Config) of
	    true ->
		[{name, Tab}, {ram_copies, Nodes}];
	    false ->
		[{name, Tab}, {ram_copies, [Node1]},
		 {disc_copies, [Node2]}, {disc_only_copies, [Node3]}]
	end,

    ?match({atomic, ok}, mnesia:create_table(Def)),
    ?match(ok, mnesia:dirty_write({Tab, father, not_updated})),
    ?match(ok, mnesia:dirty_write({Tab, child, not_updated})),

    ChildOk = fun() -> mnesia:write({Tab, child, updated}) end,
    ChildAbort = fun() ->
			 mnesia:write({Tab, child, updated}),
			 erlang:error(exit_here)
		 end,

    Child_Fun =   % Depending of test case
	case Child of
	    ok -> ChildOk;
	    abort -> ChildAbort
	end,

    FatherOk = fun() -> mnesia:transaction(Child_Fun),
			mnesia:write({Tab, father, updated})
	       end,

    FatherAbort = fun() -> mnesia:transaction(Child_Fun),
			   mnesia:write({Tab, father, updated}),
			   erlang:error(exit_here)
		  end,

    {FatherRes, ChildRes} = % Depending of test case
	case Father of
	    ok -> ?match({atomic, ok}, mnesia:transaction(FatherOk)),
		  case Child of
		      ok -> {[{Tab, father, updated}], [{Tab, child, updated}]};
		      _ -> {[{Tab, father, updated}], [{Tab, child, not_updated}]}
		  end;
	    abort -> ?match({aborted, _}, mnesia:transaction(FatherAbort)),
		     {[{Tab, father, not_updated}], [{Tab, child, not_updated}]}
	end,

    %% Syncronize things!!
    ?match({atomic, ok}, mnesia:sync_transaction(fun() -> mnesia:write({Tab, sync, sync}) end)),

    ?match(ChildRes, rpc:call(Node1, mnesia, dirty_read, [{Tab, child}])),
    ?match(ChildRes, rpc:call(Node2, mnesia, dirty_read, [{Tab, child}])),
    ?match(ChildRes, rpc:call(Node3, mnesia, dirty_read, [{Tab, child}])),

    ?match(FatherRes, rpc:call(Node1, mnesia, dirty_read, [{Tab, father}])),
    ?match(FatherRes, rpc:call(Node2, mnesia, dirty_read, [{Tab, father}])),
    ?match(FatherRes, rpc:call(Node3, mnesia, dirty_read, [{Tab, father}])),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mix_of_nested_activities(doc) ->
    ["Verify that dirty operations in a transaction are handled like ",
     "normal transactions"];
mix_of_nested_activities(suite) ->   [];
mix_of_nested_activities(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Tab = tab,

    Def =
	case mnesia_test_lib:diskless(Config) of
	    true -> [{ram_copies, Nodes}];
	    false ->
		[{ram_copies, [Node1]},
		 {disc_copies, [Node2]},
		 {disc_only_copies, [Node3]}]
	end,

    ?match({atomic, ok}, mnesia:create_table(Tab, [{type,bag}|Def])),
    Activities = [transaction, sync_transaction,
		  ets, async_dirty, sync_dirty],
    %% Make a test for all 3000 combinations
    Tests = [[A,B,C,D,E] ||
		A <- Activities,
		B <- Activities,
		C <- Activities,
		D <- Activities,
		E <- Activities],
    Foreach =
	fun(Test,No) ->
		Result = lists:reverse(Test),
		?match({No,Result},{No,catch apply_op({Tab,No},Test)}),
		No+1
	end,
    lists:foldl(Foreach, 0, Tests),
    ?verify_mnesia(Nodes, []).

apply_op(Oid,[Type]) ->
    check_res(Type,mnesia:Type(fun() -> [Type|read_op(Oid)] end));
apply_op(Oid = {Tab,Key},[Type|Next]) ->
    check_res(Type,mnesia:Type(fun() ->
				       Prev = read_op(Oid),
				       mnesia:write({Tab,Key,[Type|Prev]}),
				       apply_op(Oid,Next)
			       end)).

check_res(transaction, {atomic,Res}) ->
    Res;
check_res(sync_transaction, {atomic,Res}) ->
    Res;
check_res(async_dirty, Res) when is_list(Res) ->
    Res;
check_res(sync_dirty, Res) when is_list(Res) ->
    Res;
check_res(ets, Res) when is_list(Res) ->
    Res;
check_res(Type,Res) ->
    ?match({bug, bug},{Type,Res}).

read_op(Oid) ->
    case lists:reverse(mnesia:read(Oid)) of
	[] -> [];
	[{_,_,Ops}|_] ->
	    Ops
    end.


%% Read matching records by using an index

index_match_object(suite) -> [];
index_match_object(Config) when is_list(Config) ->
    [Node1, Node2] = Nodes = ?acquire_nodes(2, Config),
    Tab = index_match_object,
    Schema = [{name, Tab}, {attributes, [k, v, e]}, {ram_copies, [Node1]}],
    ?match({atomic, ok},  mnesia:create_table(Schema)),
    ValPos = 3,
    BadValPos = ValPos + 2,
    ?match({atomic, ok},  mnesia:add_table_index(Tab, ValPos)),

    ?match({atomic, []},
	   mnesia:transaction(fun() -> mnesia:index_match_object({Tab, '$1', 2}, ValPos) end)),
    OneRec = {Tab, {1, 1}, 2, {1, 1}},
    OnePat = {Tab, '$1', 2, '_'},
    BadPat = {Tab, '$1', '$2', '_'},  %% See ref guide

    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(OneRec) end)),

    Imatch = fun(Patt, Pos) ->
		     mnesia:transaction(fun() -> lists:sort(mnesia:index_match_object(Patt, Pos)) end)
	     end,
    ?match({atomic, [OneRec]}, Imatch(OnePat, ValPos)),
    ?match({aborted, _}, Imatch(OnePat, BadValPos)),
    ?match({aborted, _}, Imatch({foo, '$1', 2, '_'}, ValPos)),
    ?match({aborted, _}, Imatch({[], '$1', 2, '_'}, ValPos)),
    ?match({aborted, _}, Imatch(BadPat, ValPos)),
    ?match({'EXIT', {aborted, no_transaction}},  mnesia:index_match_object(OnePat, ValPos)),

    Another = {Tab, {3,1}, 2, {4,4}},
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(Another) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write({Tab, {4, 4}, 3, {4, 4}}) end)),

    ?match({atomic, [OneRec]}, Imatch({Tab, {1,1}, 2, {1,1}}, ValPos)),
    ?match({atomic, [OneRec]}, Imatch({Tab, {1,1}, 2, '$1'}, ValPos)),
    ?match({atomic, [OneRec]}, Imatch({Tab, '$1', 2, {1,1}}, ValPos)),
    ?match({atomic, [OneRec]}, Imatch({Tab, '$1', 2, '$1'}, ValPos)),
    ?match({atomic, [OneRec]}, Imatch({Tab, {1, '$1'}, 2, '_'}, ValPos)),
    ?match({atomic, [OneRec]}, Imatch({Tab, {'$2', '$1'}, 2, {'_', '$1'}}, ValPos)),
    ?match({atomic, [OneRec, Another]}, Imatch({Tab, '_', 2, '_'}, ValPos)),

    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write({Tab, 4, 5, {7, 4}}) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write({Tab, 7, 5, {7, 5}}) end)),

    ?match({atomic, [{Tab, 4, 5, {7, 4}}]}, Imatch({Tab, '$1', 5, {'_', '$1'}}, ValPos)),

    ?match({atomic, [OneRec]}, rpc:call(Node2, mnesia, transaction,
					[fun() ->
						lists:sort(mnesia:index_match_object({Tab, {1,1}, 2,
										      {1,1}}, ValPos))
					 end])),
    ?verify_mnesia(Nodes, []).

%% Read records by using an index

index_read(suite) -> [];
index_read(Config) when is_list(Config) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config),
    Tab = index_read,
    Schema = [{name, Tab}, {attributes, [k, v]}, {ram_copies, [Node1]}],
    ?match({atomic, ok},  mnesia:create_table(Schema)),
    ValPos = 3,
    BadValPos = ValPos + 1,
    ?match({atomic, ok},  mnesia:add_table_index(Tab, ValPos)),

    OneRec = {Tab, 1, 2},
    ?match({atomic, []},
	   mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(OneRec) end)),
    ?match({atomic, [OneRec]},
	   mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end)),
    ?match({aborted, _},
	   mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, BadValPos) end)),
    ?match({aborted, _},
	   mnesia:transaction(fun() -> mnesia:index_read(foo, 2, ValPos) end)),
    ?match({aborted, _},
	   mnesia:transaction(fun() -> mnesia:index_read([], 2, ValPos) end)),

    ?match({'EXIT', {aborted, no_transaction}},  mnesia:index_read(Tab, 2, ValPos)),
    ?verify_mnesia(Nodes, []).

index_update_set(suite) -> [];
index_update_set(Config)when is_list(Config) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config),
    Tab = index_test,
    Schema = [{name, Tab}, {attributes, [k, v1, v2, v3]}, {ram_copies, [Node1]}],
    ?match({atomic, ok},  mnesia:create_table(Schema)),
    ValPos = v1,
    ValPos2 = v3,
    ?match({atomic, ok},  mnesia:add_table_index(Tab, ValPos)),

    Pat1 = {Tab, '$1',  2,   '$2', '$3'},
    Pat2 = {Tab, '$1', '$2', '$3', '$4'},

    Rec1 = {Tab, 1, 2, 3, 4},
    Rec2 = {Tab, 2, 2, 13, 14},
    Rec3 = {Tab, 1, 12, 13, 14},
    Rec4 = {Tab, 4, 2, 13, 14},

    ?match({atomic, []},
	   mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(Rec1) end)),
    ?match({atomic, [Rec1]},
	   mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end)),

    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(Rec2) end)),
    {atomic, R1} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec1, Rec2], lists:sort(R1)),

    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(Rec3) end)),
    {atomic, R2} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec2], lists:sort(R2)),
    ?match({atomic, [Rec2]},
	   mnesia:transaction(fun() -> mnesia:index_match_object(Pat1, ValPos) end)),

    {atomic, R3} = mnesia:transaction(fun() -> mnesia:match_object(Pat2) end),
    ?match([Rec3, Rec2], lists:sort(R3)),

    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(Rec4) end)),
    {atomic, R4} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec2, Rec4], lists:sort(R4)),

    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:delete({Tab, 4}) end)),
    ?match({atomic, [Rec2]},
	   mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end)),

    ?match({atomic, ok}, mnesia:del_table_index(Tab, ValPos)),
    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:write(Rec4) end)),
    ?match({atomic, ok}, mnesia:add_table_index(Tab, ValPos)),
    ?match({atomic, ok}, mnesia:add_table_index(Tab, ValPos2)),

    {atomic, R5} = mnesia:transaction(fun() -> mnesia:match_object(Pat2) end),
    ?match([Rec3, Rec2, Rec4], lists:sort(R5)),

    {atomic, R6} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec2, Rec4], lists:sort(R6)),

    ?match({atomic, []},
	   mnesia:transaction(fun() -> mnesia:index_read(Tab, 4, ValPos2) end)),
    {atomic, R7} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 14, ValPos2) end),
    ?match([Rec3, Rec2, Rec4], lists:sort(R7)),

    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:write(Rec1) end)),
    {atomic, R8} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec1, Rec2, Rec4], lists:sort(R8)),
    ?match({atomic, [Rec1]},
	   mnesia:transaction(fun() -> mnesia:index_read(Tab, 4, ValPos2) end)),
    {atomic, R9} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 14, ValPos2) end),
    ?match([Rec2, Rec4], lists:sort(R9)),

    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:delete_object(Rec2) end)),
    {atomic, R10} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec1, Rec4], lists:sort(R10)),
    ?match({atomic, [Rec1]},
	   mnesia:transaction(fun() -> mnesia:index_read(Tab, 4, ValPos2) end)),
    ?match({atomic, [Rec4]},
	   mnesia:transaction(fun() -> mnesia:index_read(Tab, 14, ValPos2) end)),

    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:delete({Tab, 4}) end)),
    {atomic, R11} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec1], lists:sort(R11)),
    ?match({atomic, [Rec1]},mnesia:transaction(fun() -> mnesia:index_read(Tab, 4, ValPos2) end)),
    ?match({atomic, []},mnesia:transaction(fun() -> mnesia:index_read(Tab, 14, ValPos2) end)),

    ?verify_mnesia(Nodes, []).

index_update_bag(suite) -> [];
index_update_bag(Config)when is_list(Config) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config),
    Tab = index_test,
    Schema = [{name, Tab},
	      {type, bag},
	      {attributes, [k, v1, v2, v3]},
	      {ram_copies, [Node1]}],
    ?match({atomic, ok},  mnesia:create_table(Schema)),
    ValPos = v1,
    ValPos2 = v3,

    ?match({atomic, ok},  mnesia:add_table_index(Tab, ValPos)),

    Pat1 = {Tab, '$1',  2,   '$2', '$3'},
    Pat2 = {Tab, '$1', '$2', '$3', '$4'},

    Rec1 = {Tab, 1, 2, 3, 4},
    Rec2 = {Tab, 2, 2, 13, 14},
    Rec3 = {Tab, 1, 12, 13, 14},
    Rec4 = {Tab, 4, 2, 13, 4},
    Rec5 = {Tab, 1, 2, 234, 14},

    %% Simple Index
    ?match({atomic, []},
	   mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end)),
    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(Rec1) end)),
    ?match({atomic, [Rec1]},
	   mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end)),

    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:delete_object(Rec5) end)),
    ?match({atomic, [Rec1]},
	   mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end)),

    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(Rec2) end)),
    {atomic, R1} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec1, Rec2], lists:sort(R1)),

    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(Rec3) end)),
    {atomic, R2} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec1, Rec2], lists:sort(R2)),

    {atomic, R3} = mnesia:transaction(fun() -> mnesia:index_match_object(Pat1, ValPos) end),
    ?match([Rec1, Rec2], lists:sort(R3)),

    {atomic, R4} = mnesia:transaction(fun() -> mnesia:match_object(Pat2) end),
    ?match([Rec1, Rec3, Rec2], lists:sort(R4)),

    ?match({atomic, ok},
	   mnesia:transaction(fun() -> mnesia:write(Rec4) end)),
    {atomic, R5} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec1, Rec2, Rec4], lists:sort(R5)),

    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:delete({Tab, 4}) end)),
    {atomic, R6} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec1, Rec2], lists:sort(R6)),

    %% OTP-6587  Needs some whitebox testing to see that the index table is cleaned correctly

    [IPos] = mnesia_lib:val({Tab,index}),
    ITab = mnesia_lib:val({index_test,{index, IPos}}),
    io:format("~n Index ~p @ ~p => ~p ~n~n",[IPos,ITab, ets:tab2list(ITab)]),
    %?match([{2,1},{2,2},{12,1}], lists:keysort(1,ets:tab2list(ITab))),

    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:write(Rec5) end)),
    {atomic, R60} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec1,Rec5,Rec2], lists:sort(R60)),

    %?match([{2,1},{2,2},{12,1}], lists:keysort(1,ets:tab2list(ITab))),

    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:delete_object(Rec3) end)),
    {atomic, R61} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec1,Rec5,Rec2], lists:sort(R61)),
    {atomic, R62} = mnesia:transaction(fun() -> mnesia:index_read(Tab,12, ValPos) end),
    ?match([], lists:sort(R62)),
    %% ?match([{2,1},{2,2}], lists:keysort(1,ets:tab2list(ITab))),

    %% reset for rest of testcase
    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:write(Rec3) end)),
    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:delete_object(Rec5) end)),
    {atomic, R6} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec1, Rec2], lists:sort(R6)),
    %% OTP-6587

    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:delete_object(Rec1) end)),
    ?match({atomic, [Rec2]},
	   mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end)),
    {atomic, R7} = mnesia:transaction(fun() -> mnesia:match_object(Pat2) end),
    ?match([Rec3, Rec2], lists:sort(R7)),

    %% Two indexies
    ?match({atomic, ok}, mnesia:del_table_index(Tab, ValPos)),
    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:write(Rec1) end)),
    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:write(Rec4) end)),
    ?match({atomic, ok}, mnesia:add_table_index(Tab, ValPos)),
    ?match({atomic, ok}, mnesia:add_table_index(Tab, ValPos2)),

    {atomic, R8} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec1, Rec2, Rec4], lists:sort(R8)),

    {atomic, R9} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 4, ValPos2) end),
    ?match([Rec1, Rec4], lists:sort(R9)),
    {atomic, R10} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 14, ValPos2) end),
    ?match([Rec3, Rec2], lists:sort(R10)),

    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:write(Rec5) end)),
    {atomic, R11} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec1, Rec5, Rec2, Rec4], lists:sort(R11)),
    {atomic, R12} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 4, ValPos2) end),
    ?match([Rec1, Rec4], lists:sort(R12)),
    {atomic, R13} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 14, ValPos2) end),
    ?match([Rec5, Rec3, Rec2], lists:sort(R13)),

    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:delete_object(Rec1) end)),
    {atomic, R14} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec5, Rec2, Rec4], lists:sort(R14)),
    ?match({atomic, [Rec4]},
	   mnesia:transaction(fun() -> mnesia:index_read(Tab, 4, ValPos2) end)),
    {atomic, R15} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 14, ValPos2) end),
    ?match([Rec5, Rec3, Rec2], lists:sort(R15)),

    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:delete_object(Rec5) end)),
    {atomic, R16} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec2, Rec4], lists:sort(R16)),
    ?match({atomic, [Rec4]}, mnesia:transaction(fun()->mnesia:index_read(Tab, 4, ValPos2) end)),
    {atomic, R17} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 14, ValPos2) end),
    ?match([Rec3, Rec2], lists:sort(R17)),

    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:write(Rec1) end)),
    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:delete({Tab, 1}) end)),
    {atomic, R18} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 2, ValPos) end),
    ?match([Rec2, Rec4], lists:sort(R18)),
    ?match({atomic, [Rec4]}, mnesia:transaction(fun()->mnesia:index_read(Tab, 4, ValPos2) end)),
    {atomic, R19} = mnesia:transaction(fun() -> mnesia:index_read(Tab, 14, ValPos2) end),
    ?match([Rec2], lists:sort(R19)),

    ?verify_mnesia(Nodes, []).


index_write(suite) -> [];
index_write(doc) -> ["See ticket OTP-8072"];
index_write(Config)when is_list(Config) ->
    Nodes = ?acquire_nodes(1, Config),
    mnesia:create_table(a, [{index, [val]}]),
    mnesia:create_table(counter, []),

    CreateIfNonExist =
	fun(Index) ->
		case mnesia:index_read(a, Index, 3) of
		    [] ->
			Id = mnesia:dirty_update_counter(counter, id, 1),
			New = {a, Id, Index},
			mnesia:write(New),
			New;
		    [Found] ->
			Found
		end
	end,

    Trans = fun(A) ->
		    mnesia:transaction(CreateIfNonExist, [A])
		    %% This works better most of the time
		    %% And it is allowed to fail since it's dirty
		    %% mnesia:async_dirty(CreateIfNonExist, [A])
	    end,

    Self = self(),
    Update = fun() ->
		     Res = lists:map(Trans, lists:seq(1,10)),
		     Self ! {self(), Res}
	     end,

    Pids = [spawn(Update) || _ <- lists:seq(1,5)],

    Gather = fun(Pid, Acc) -> receive {Pid, Res} -> [Res|Acc] end end,
    Results = lists:foldl(Gather, [], Pids),
    Expected = hd(Results),
    Check = fun(Res) -> ?match(Expected, Res) end,
    lists:foreach(Check, Results),
    ?verify_mnesia(Nodes, []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Add and drop indecies


add_table_index_ram(suite) -> [];
add_table_index_ram(Config) when is_list(Config) ->
    add_table_index(Config, ram_copies).

add_table_index_disc(suite) -> [];
add_table_index_disc(Config) when is_list(Config) ->
    add_table_index(Config, disc_copies).

add_table_index_disc_only(suite) -> [];
add_table_index_disc_only(Config) when is_list(Config) ->
    add_table_index(Config, disc_only_copies).

%% Add table index

add_table_index(Config, Storage) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config),
    Tab = add_table_index,
    Schema = [{name, Tab}, {attributes, [k, v]}, {Storage, [Node1]}],
    ?match({atomic, ok}, mnesia:create_table(Schema)),
    ValPos = 3,
    BadValPos = ValPos + 1,
    ?match({aborted, Reason41 } when element(1, Reason41) == bad_type,
           mnesia:add_table_index(Tab, BadValPos)),
    ?match({aborted,Reason42 } when element(1, Reason42) == bad_type,
           mnesia:add_table_index(Tab, 2)),
    ?match({aborted, Reason43 } when element(1, Reason43) == bad_type,
           mnesia:add_table_index(Tab, 1)),
    ?match({aborted, Reason44 } when element(1, Reason44) == bad_type,
           mnesia:add_table_index(Tab, 0)),
    ?match({aborted, Reason45 } when element(1, Reason45) == bad_type,
           mnesia:add_table_index(Tab, -1)),
    ?match({atomic, ok}, mnesia:add_table_index(Tab, ValPos)),
    ?match({aborted, Reason46 } when element(1, Reason46) == already_exists,
           mnesia:add_table_index(Tab, ValPos)),

    NestedFun = fun() ->
                        ?match({aborted, nested_transaction},
                               mnesia:add_table_index(Tab, ValPos)),
                        ok
                end,
    ?match({atomic, ok}, mnesia:transaction(NestedFun)),
    ?verify_mnesia(Nodes, []).

create_live_table_index_ram(suite) -> [];
create_live_table_index_ram(Config) when is_list(Config) ->
    create_live_table_index(Config, ram_copies).

create_live_table_index_disc(suite) -> [];
create_live_table_index_disc(Config) when is_list(Config) ->
    create_live_table_index(Config, disc_copies).

create_live_table_index_disc_only(suite) -> [];
create_live_table_index_disc_only(Config) when is_list(Config) ->
    create_live_table_index(Config, disc_only_copies).

create_live_table_index(Config, Storage) ->
    [N1,N2,N3] = Nodes = ?acquire_nodes(3, Config),
    Tab = create_live_table_index,
    Schema = [{name, Tab}, {attributes, [k, v]}, {Storage, Nodes}],
    ?match({atomic, ok}, mnesia:create_table(Schema)),
    ValPos = 3,
    mnesia:dirty_write({Tab, 1, 2}),

    Fun = fun() -> mnesia:write({Tab, 2, 2}) end,
    ?match({atomic, ok}, mnesia:transaction(Fun)),
    ?match({atomic, ok}, mnesia:add_table_index(Tab, ValPos)),
    IRead = fun() -> lists:sort(mnesia:index_read(Tab, 2, ValPos)) end,
    ?match({atomic, [{Tab, 1, 2},{Tab, 2, 2}]}, mnesia:transaction(IRead)),
    ?match({atomic, ok}, mnesia:del_table_index(Tab, ValPos)),

    %% Bug when adding index when table is still unloaded
    %% By setting load order we hopefully will trigger the bug
    mnesia:change_table_copy_type(Tab, N2, ram_copies),
    mnesia:change_table_copy_type(Tab, N3, ram_copies),
    ?match({atomic,ok}, mnesia:change_table_copy_type(schema, N2, ram_copies)),
    ?match({atomic,ok}, mnesia:change_table_copy_type(schema, N3, ram_copies)),

    Create = fun(N) ->
		     TabN = list_to_atom("tab_" ++ integer_to_list(N)),
		     Def = [{ram_copies, Nodes}, {load_order, N}],
		     mnesia:create_table(TabN, Def)
	     end,

    ?match([{atomic,ok}|_], [Create(N) || N <- lists:seq(1,50)]),

    ?match([], mnesia_test_lib:stop_mnesia([N2,N3])),
    Ext = [{schema, ?BACKEND}],
    ?match(ok, rpc:call(N2, mnesia, start, [[{extra_db_nodes,[N1]}|Ext]])),
    ?match(ok, rpc:call(N3, mnesia, start, [[{extra_db_nodes,[N1]}|Ext]])),

    ?match({atomic, ok}, mnesia:add_table_index(Tab, ValPos)),

    ?match({atomic, [{Tab, 1, 2},{Tab, 2, 2}]}, mnesia:transaction(IRead)),
    ?match({atomic, [{Tab, 1, 2},{Tab, 2, 2}]},
	   rpc:call(N2, mnesia, transaction, [IRead])),

    ?verify_mnesia(Nodes, []).

%% Drop table index

del_table_index_ram(suite) ->[];
del_table_index_ram(Config) when is_list(Config) ->
    del_table_index(Config, ram_copies).

del_table_index_disc(suite) ->[];
del_table_index_disc(Config) when is_list(Config) ->
    del_table_index(Config, disc_copies).

del_table_index_disc_only(suite) ->[];
del_table_index_disc_only(Config) when is_list(Config) ->
    del_table_index(Config, disc_only_copies).

del_table_index(Config, Storage) ->
    [Node1] = Nodes = ?acquire_nodes(1, Config),
    Tab = del_table_index,
    Schema = [{name, Tab}, {attributes, [k, v]}, {Storage, [Node1]}],
    ?match({atomic, ok}, mnesia:create_table(Schema)),
    ValPos = 3,
    BadValPos = ValPos + 1,
    ?match({atomic, ok}, mnesia:add_table_index(Tab, ValPos)),
    ?match({aborted,Reason} when element(1, Reason) == no_exists,
           mnesia:del_table_index(Tab, BadValPos)),
    ?match({atomic, ok}, mnesia:del_table_index(Tab, ValPos)),

    ?match({aborted,Reason1} when element(1, Reason1) == no_exists,
           mnesia:del_table_index(Tab, ValPos)),
    NestedFun =
        fun() ->
                ?match({aborted, nested_transaction},
                       mnesia:del_table_index(Tab, ValPos)),
                ok
        end,
    ?match({atomic, ok}, mnesia:transaction(NestedFun)),
    ?verify_mnesia(Nodes, []).


idx_schema_changes_ram(suite) -> [];
idx_schema_changes_ram(Config) when is_list(Config) ->
    idx_schema_changes(Config, ram_copies).
idx_schema_changes_disc(suite) -> [];
idx_schema_changes_disc(Config) when is_list(Config) ->
    idx_schema_changes(Config, disc_copies).
idx_schema_changes_disc_only(suite) -> [];
idx_schema_changes_disc_only(Config) when is_list(Config) ->
    idx_schema_changes(Config, disc_only_copies).

idx_schema_changes(Config, Storage) ->
    [N1, N2] = Nodes = ?acquire_nodes(2, Config),
    Tab = index_schema_changes,
    Idx = 3,
    Schema = [{name, Tab}, {index, [Idx]}, {attributes, [k, v]}, {Storage, Nodes}],
    ?match({atomic, ok}, mnesia:create_table(Schema)),

    {Storage1, Storage2} =
	case Storage of
	    disc_only_copies ->
		{ram_copies, disc_copies};
	    disc_copies ->
		{disc_only_copies, ram_copies};
	    ram_copies ->
		{disc_copies, disc_only_copies}
	end,

    Write = fun(N) ->
		    mnesia:write({Tab, N, N+50})
	    end,

    [mnesia:sync_transaction(Write, [N]) || N <- lists:seq(1, 10)],
    ?match([{Tab, 1, 51}], rpc:call(N1, mnesia, dirty_index_read, [Tab, 51, Idx])),
    ?match([{Tab, 1, 51}], rpc:call(N2, mnesia, dirty_index_read, [Tab, 51, Idx])),

    ?match({atomic, ok}, mnesia:change_table_copy_type(Tab, N1, Storage1)),

    ?match({atomic, ok}, rpc:call(N1, mnesia, sync_transaction, [Write, [17]])),
    ?match({atomic, ok}, rpc:call(N2, mnesia, sync_transaction, [Write, [18]])),

    ?match([{Tab, 17, 67}], rpc:call(N2, mnesia, dirty_index_read, [Tab, 67, Idx])),
    ?match([{Tab, 18, 68}], rpc:call(N1, mnesia, dirty_index_read, [Tab, 68, Idx])),

    ?match({atomic, ok}, mnesia:del_table_copy(Tab, N1)),
    ?match({atomic, ok}, rpc:call(N1, mnesia, sync_transaction, [Write, [11]])),
    ?match({atomic, ok}, rpc:call(N2, mnesia, sync_transaction, [Write, [12]])),

    ?match([{Tab, 11, 61}], rpc:call(N2, mnesia, dirty_index_read, [Tab, 61, Idx])),
    ?match([{Tab, 12, 62}], rpc:call(N1, mnesia, dirty_index_read, [Tab, 62, Idx])),

    ?match({atomic, ok}, mnesia:move_table_copy(Tab, N2, N1)),
    ?match({atomic, ok}, rpc:call(N1, mnesia, sync_transaction, [Write, [19]])),
    ?match({atomic, ok}, rpc:call(N2, mnesia, sync_transaction, [Write, [20]])),

    ?match([{Tab, 19, 69}], rpc:call(N2, mnesia, dirty_index_read, [Tab, 69, Idx])),
    ?match([{Tab, 20, 70}], rpc:call(N1, mnesia, dirty_index_read, [Tab, 70, Idx])),

    ?match({atomic, ok}, mnesia:add_table_copy(Tab, N2, Storage)),
    ?match({atomic, ok}, rpc:call(N1, mnesia, sync_transaction, [Write, [13]])),
    ?match({atomic, ok}, rpc:call(N2, mnesia, sync_transaction, [Write, [14]])),

    ?match([{Tab, 13, 63}], rpc:call(N2, mnesia, dirty_index_read, [Tab, 63, Idx])),
    ?match([{Tab, 14, 64}], rpc:call(N1, mnesia, dirty_index_read, [Tab, 64, Idx])),

    ?match({atomic, ok}, mnesia:change_table_copy_type(Tab, N2, Storage2)),

    ?match({atomic, ok}, rpc:call(N1, mnesia, sync_transaction, [Write, [15]])),
    ?match({atomic, ok}, rpc:call(N2, mnesia, sync_transaction, [Write, [16]])),

    ?match([{Tab, 15, 65}], rpc:call(N2, mnesia, dirty_index_read, [Tab, 65, Idx])),
    ?match([{Tab, 16, 66}], rpc:call(N1, mnesia, dirty_index_read, [Tab, 66, Idx])),

    ?verify_mnesia(Nodes, []).
