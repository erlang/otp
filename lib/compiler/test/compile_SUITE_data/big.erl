%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(big).
-compile([export_all]).

compiler_1() -> ok.

-define(log(Format,Args),mnesia_test_lib:log(Format,Args,?FILE,?LINE)).
-define(warning(Format,Args),?log("<WARNING> " ++ Format,Args)).
-define(error(Format,Args),
	mnesia_test_lib:note_error(Format,Args,?FILE,?LINE),
	?log("<ERROR> " ++ Format,Args)).

-define(match(ExpectedRes,Expr),
	fun() ->
	       AcTuAlReS = (catch (Expr)),
	       case AcTuAlReS of
		   ExpectedRes ->
		       ?log("ok, result as expected: ~p~n",[AcTuAlReS]),
		       {success,AcTuAlReS};
		   _ ->
		       ?error("actual result was: ~p~n",[AcTuAlReS]),
		       {fail,AcTuAlReS}
	       end
       end()).

-define(match_inverse(NotExpectedRes,Expr),
	fun() ->
	       AcTuAlReS = (catch (Expr)),
	       case AcTuAlReS of
		   NotExpectedRes ->
		       ?error("actual result was: ~p~n",[AcTuAlReS]),
		       {fail,AcTuAlReS};
		   _ ->
		       ?log("ok, result as expected: ~p~n",[AcTuAlReS]),
		       {success,AcTuAlReS}
	       end
       end()).

-define(match_receive(ExpectedMsg),
	?match(ExpectedMsg,mnesia_test_lib:pick_msg())).

%% ExpectedMsgs must be completely bound
-define(match_multi_receive(ExpectedMsgs),
	fun() ->
		TmPeXpCtEdMsGs = lists:sort(ExpectedMsgs),
		?match(TmPeXpCtEdMsGs,
		       lists:sort(lists:map(fun(_) ->
						    mnesia_test_lib:pick_msg()
					    end,
					    TmPeXpCtEdMsGs)))
	end()).

-define(setup(), mnesia_test_lib:setup(?FILE,?LINE)).

-define(start_activities(Nodes),
	fun() ->
		AcTiViTyPiDs =
		    lists:map(fun(Node) ->
				      spawn_link(Node,
						 mnesia_test_lib,
						 activity_evaluator,
						 [self()])
			      end,
			      Nodes),
		?match_multi_receive(AcTiViTyPiDs)
	end()).

-define(start_transactions(Pids),
	?match_multi_receive(lists:map(fun(Pid) ->
					       Pid ! begin_trans,
					       {Pid,begin_trans}
				       end,
				       Pids))).

-define(acquire_nodes(N,Nodes),
	mnesia_test_lib:acquire_nodes(N,Nodes,?FILE,?LINE)).



%%% Copyright (C) 1996, Ellemtel Telecommunications Systems Laboratories
%%% Author: Hakan Mattsson  hakan@erix.ericsson.se
%%% Purpose: Evil usage of the API
%%% 
%%% Invoke all functions in the API and try to cover all legal uses
%%% cases as well the illegal dito. This is a complement to the
%%% other more explicit test cases. 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% show/0
%%% 
%%%    Prints out the complete test case structure
%%%
%%% show/1
%%% 
%%%    Prints out parts of the test case structure
%%%
%%% test/0
%%% 
%%%    Run the complete test suite.
%%%    Reads Nodes from nodes.profile and starts them if neccessary.
%%%    Kills Mnesia and wipes out the Mnesia directories as a starter.
%%%
%%% test/1
%%%
%%%    Run parts of the test suite.
%%%    Reads Nodes from nodes.profile and starts them if neccessary.
%%%    Kills Mnesia and wipes out the Mnesia directories as a starter.
%%%
%%% test/2
%%% 
%%%    Run parts of the test suite on the given Nodes,
%%%    assuming that the nodes are up and running.
%%%    Kills Mnesia and wipes out the Mnesia directories as a starter.
%%%
%%% test/3
%%% 
%%%    Run parts of the test suite on permutations of the given Nodes,
%%%    assuming that the nodes are up and running. Uses test/2.
%%%    Kills Mnesia and wipes out the Mnesia directories as a starter.
%%%
%%% See the module mnesia_test_lib for further information.
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show() -> mnesia_test_lib:show([{?MODULE,all}]).
show(TestCases) -> mnesia_test_lib:show([{?MODULE,TestCases}]).
test() -> mnesia_test_lib:test([{?MODULE,all}]).
test(TestCases) ->  mnesia_test_lib:test([{?MODULE,TestCases}]).
test(TestCases,Nodes) -> mnesia_test_lib:test([{?MODULE,TestCases}],Nodes).
test(TestCases,Nodes,Config) -> mnesia_test_lib:test([{?MODULE,TestCases}],Nodes,Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

old_all(suite) ->
    [
     system_info, table_info, error_description,
     db_node_lifecycle, start_and_stop, transaction, checkpoint, backup,
     table_lifecycle, replica_management, replica_location, index_lifecycle,
     trans_access, dirty_access, table_sync, snmp_access, debug_support
    ].

trans_access(suite) ->
    [ {mnesia_dirty_access_test,all} ].

dirty_access(suite) ->
     [ {mnesia_trans_access_test,all} ].

%% Get meta info about Mnesia
system_info(suite) -> [];
system_info(Nodes) ->
    ?match(yes,mnesia:system_info(is_running)),
    ?match(Nodes,mnesia:system_info(db_nodes)),
    ?match(Nodes,mnesia:system_info(running_db_nodes)),
    ?match(true,mnesia:system_info(have_disc)),
    ?match(A when atom(A),mnesia:system_info(debug)),
    ?match(L when list(L),mnesia:system_info(directory)),
    ?match(L when list(L),mnesia:system_info(log_version)),
    ?match({_,_},mnesia:system_info(schema_version)),
    ?match(L when list(L),mnesia:system_info(tables)),
    ?match(L when list(L),mnesia:system_info(local_tables)),
    ?match(L when list(L),mnesia:system_info(held_locks)),
    ?match(L when list(L),mnesia:system_info(lock_queue)),
    ?match(L when list(L),mnesia:system_info(transactions)),
    ?match(I when integer(I),mnesia:system_info(transaction_failures)),
    ?match(I when integer(I),mnesia:system_info(transaction_commits)),
    ?match(I when integer(I),mnesia:system_info(transaction_restarts)),
    ?match(L when list(L),mnesia:system_info(checkpoints)),
    ?match(A when atom(A),mnesia:system_info(backup_module)),
    ?match(true,mnesia:system_info(auto_repair)),
    ?match({_,_},mnesia:system_info(dump_log_interval)),
    ?match(A when atom(A),mnesia:system_info(dump_log_update_in_place)),
    ?match(I when integer(I),mnesia:system_info(transaction_log_writes)),
    ?match({'EXIT',{aborted,badarg}},mnesia:system_info(ali_baba)),
    done.

%% Get meta info about table
table_info(suite) -> [];
table_info(Nodes) ->
    [Node1,Node2,Node3] = ?acquire_nodes(3,Nodes),
  
    Tab = table_info,
    Type = bag,
    ValPos = 3,
    Attrs = [k,v],
    Arity = length(Attrs) +1,
    Schema = [{name,Tab},{type,Type},{attributes,Attrs},{index,[ValPos]},
	      {disc_only_copies,[Node1]},{ram_copies,[Node2]},{disc_copies,[Node3]}],
    ?match({atomic,ok}, mnesia:create_table(Schema)),

    Size = 10,
    Keys = lists:seq(1,Size),
    Records = [{Tab,A,7} || A <- Keys],
    lists:foreach(fun(Rec) -> ?match(ok,mnesia:dirty_write(Rec)) end,Records),
    ?match(Mem when integer(Mem),mnesia:table_info(Tab,memory)),
    ?match(Size,mnesia:table_info(Tab,size)),
    ?match(Type,mnesia:table_info(Tab,type)),
    ?match([Node3],mnesia:table_info(Tab,disc_copies)),
    ?match([Node2],mnesia:table_info(Tab,ram_copies)),
    ?match([Node1],mnesia:table_info(Tab,disc_only_copies)),
    Read = [Node1,Node2,Node3],
    ?match(true,lists:member(mnesia:table_info(Tab,where_to_read),Read)),
    Write = lists:sort([Node1,Node2,Node3]),
    ?match(Write,lists:sort(mnesia:table_info(Tab,where_to_write))),
    WriteLock = lists:sort([Node2,Node3]),
    ?match([ValPos],mnesia:table_info(Tab,index)),
    ?match(Arity,mnesia:table_info(Tab,arity)),
    ?match(Attrs,mnesia:table_info(Tab,attributes)),
    ?match({Tab,'_','_'},mnesia:table_info(Tab,wild_pattern)),
    ?match({atomic,Attrs}, mnesia:transaction(fun() ->
	     mnesia:table_info(Tab,attributes) end)),    

    done.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Add and drop db nodes

db_node_lifecycle(suite) -> [];
db_node_lifecycle(Nodes) ->
    [Node1,Node2] = ?acquire_nodes(2,Nodes),
    Tab = db_node_lifecycle,

    Schema = [{name,Tab},{ram_copies,[Node1,Node2]}],
    ?match({atomic,ok}, mnesia:create_table(Schema)),
    ?match({aborted,active}, rpc:call(Node1,mnesia,del_db_node,[Node2])),

    ?match([], mnesia_test_lib:stop_mnesia(Nodes)),
    ?match(ok, mnesia:delete_schema(Nodes)),
    ?match({error,_}, mnesia:create_schema(foo)),
    ?match({error,_}, mnesia:create_schema([foo])),
    ?match({error,_}, mnesia:create_schema([foo@bar])),
    ?match({error,_}, mnesia:start()),

    ?match(ok, mnesia:create_schema(Nodes)),
    ?match([],mnesia_test_lib:start_mnesia(Nodes)),
    ?match({atomic,ok}, rpc:call(Node1,mnesia,del_db_node,[Node2])),
    ?match({aborted,no_exists}, rpc:call(Node1,mnesia,del_db_node,[Node2])),
    ?match({aborted,no_exists}, rpc:call(Node1,mnesia,del_db_node,[foo])),
    ?match({aborted,no_exists}, rpc:call(Node1,mnesia,del_db_node,[foo@bar])),

    ?match([], mnesia_test_lib:stop_mnesia([Node2])),
    ?match(ok,mnesia:delete_schema([Node2])),
    AddFun = fun() -> ?match({aborted,nested_transaction},
			     mnesia:add_db_node(Node2)), ok end,
    ?match({atomic,ok},rpc:call(Node1,mnesia,transaction,[AddFun])),
    DelFun = fun() -> ?match({aborted,nested_transaction},
			     mnesia:del_db_node(Node2)), ok end,
    ?match({atomic,ok},rpc:call(Node1,mnesia,transaction,[DelFun])),

    ?match({atomic,ok}, rpc:call(Node1,mnesia,add_db_node,[Node2])),
    ?match({aborted,already_exists}, rpc:call(Node1,mnesia,add_db_node,[Node2])),
    ?match([],mnesia_test_lib:start_mnesia([Node2])),
    ?match({atomic,ok}, mnesia:create_table(Schema)),
    done.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start and stop the system

start_and_stop(suite) -> [];
start_and_stop(Nodes) ->
    [Node1] = ?acquire_nodes(1,Nodes),
    
    ?match(stopped, rpc:call(Node1,mnesia,stop,[])),
    ?match(stopped, rpc:call(Node1,mnesia,stop,[])),
    ?match({started,_}, rpc:call(Node1,mnesia,start,[])),
    ?match({started,_}, rpc:call(Node1,mnesia,start,[])),
    ?match(stopped, rpc:call(Node1,mnesia,stop,[])),
    ?match([],mnesia_test_lib:start_mnesia(Nodes)),
    done.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Checkpoints and backup management

checkpoint(suite) -> [];
checkpoint(Nodes) ->
    OneNode = ?acquire_nodes(1,Nodes),
    checkpoint(OneNode,Nodes),
    TwoNodes = ?acquire_nodes(2,Nodes),
    checkpoint(TwoNodes,Nodes).

checkpoint(TabNodes,Nodes) ->
    [Node1] = ?acquire_nodes(1,TabNodes),
    CreateTab = fun(Type,N,Ns) ->
			Tab0 = lists:concat(["local_checkpoint_",Type,N]),
			Tab = list_to_atom(Tab0),
			Schema = [{name,Tab},{Type,Ns}],
			?match({atomic,ok},mnesia:delete_table(Tab)),
			?match({atomic,ok},mnesia:create_table(Schema)),
			Tab
		end,
    CreateTabs = fun(Type) ->
			 CreateTab(Type,1,hd(TabNodes)),
			 CreateTab(Type,2,TabNodes),
			 CreateTab(Type,3,lists:last(TabNodes))
		 end,
    Types = [ram_copies,disc_copies,disc_only_copies],
    Tabs = lists:append(lists:map(CreateTabs,Types)),
    Recs = lists:sort([{T,N,N} || T <- Tabs,N <- lists:seq(1,10)]),
    lists:foreach(fun(R) -> ?match(ok,mnesia:dirty_write(R)) end,Recs),
    
    CpName = a_checkpoint_name,
    MinArgs = [{name,CpName},{min,Tabs},{allow_remote,false}],
    ?match({ok,CpName,[Node1]},
	   rpc:call(Node1,mnesia,activate_checkpoint,[MinArgs])),
    ?match(ok,rpc:call(Node1,mnesia,deactivate_checkpoint,[CpName])),
    
    MaxArgs = [{name,CpName},{max,Tabs},{allow_remote,true}],
    ?match({ok,CpName,[Node1]},
	   rpc:call(Node1,mnesia,activate_checkpoint,[MaxArgs])),
    ?match(ok,rpc:call(Node1,mnesia,deactivate_checkpoint,[CpName])),
    
    Args = [{name,CpName},{min,Tabs},{allow_remote,false}],
    ?match({ok,CpName,[Node1]},
	   rpc:call(Node1,mnesia,activate_checkpoint,[Args])),
    Recs2 = lists:sort([{T,K,0} || {T,K,_} <- Recs]),
    lists:foreach(fun(R) -> ?match(ok,mnesia:dirty_write(R)) end,Recs2),
    ?match({atomic,ok},rpc:call(Node1,mnesia,deactivate_checkpoint,[CpName])),
    
    ?match({error,no_exists},mnesia:deactivate_checkpoint(CpName)),
    ?match({error,badarg},mnesia:activate_checkpoint(foo)),
    ?match({error,badarg},mnesia:activate_checkpoint([{foo,foo}])),
    ?match({error,badarg},mnesia:activate_checkpoint([{max,foo}])),
    ?match({error,badarg},mnesia:activate_checkpoint([{min,foo}])),
    ?match({error,no_exists},mnesia:activate_checkpoint([{min,[foo@bar]}])),
    ?match({error,badarg},mnesia:activate_checkpoint([{allow_remote,foo}])),
    
    Fun = fun(Tab) -> ?match({atomic,ok},mnesia:delete_table(Tab)) end,
    lists:foreach(Fun,Tabs),
    done.

backup(suite) ->
    [
     backup_schema, restore_schema, backup_checkpoint, restore_tables
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Use and misuse transactions

transaction(suite) -> [];
transaction(Nodes) ->
    [Node1] = ?acquire_nodes(1,Nodes),
    ?match({atomic,ali_baba}, mnesia:transaction(fun() -> ali_baba end)),
    ?match({aborted,_},  mnesia:transaction(no_fun)),
    ?match({aborted,_},  mnesia:transaction(?MODULE,no_fun,[foo])),
    
    {success,[A,B,C,D,E,F,G,H]} = ?start_activities(lists:duplicate(8,Node1)),
    ?start_transactions([A,B,C,D,E,F,G,H]),
    
    A ! fun() -> mnesia:abort(abort_bad_trans) end,
    ?match_receive({A,{aborted,abort_bad_trans}}),
    
    B ! fun() -> 1 = 2 end,
    ?match_receive({B,{aborted,_}}),
    
    C ! fun() -> throw(throw_bad_trans) end,
    ?match_receive({C,{aborted,{throw,throw_bad_trans}}}),
    
    D ! fun() -> exit(exit_bad_trans) end,
    ?match_receive({D,{aborted,exit_bad_trans}}),
    
    E ! fun() -> exit(normal) end,
    ?match_receive({E,{aborted,normal}}),
    
    F ! fun() -> exit(abnormal) end,
    ?match_receive({F,{aborted,abnormal}}),
    
    G ! fun() -> exit(G,abnormal) end,
    ?match_receive({'EXIT',G,abnormal}),
    
    H ! fun() -> exit(H,kill) end,
    ?match_receive({'EXIT',H,killed}),
    
    ?match({atomic,ali_baba},
	   mnesia:transaction(fun() -> ali_baba end,infinity)),
    ?match({atomic,ali_baba},mnesia:transaction(fun() -> ali_baba end,1)),
    ?match({atomic,ali_baba},mnesia:transaction(fun() -> ali_baba end,0)),
    ?match({atomic,ali_baba},mnesia:transaction(fun() -> ali_baba end,-1)),
    ?match({atomic,ali_baba},mnesia:transaction(fun() -> ali_baba end,foo)),
    Fun = fun() -> ?match({aborted,nested_transaction},
			  mnesia:transaction(fun() -> ok end)), ok end,
    ?match({atomic,ok},mnesia:transaction(Fun)),
    done.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create and delete tables

%% Get meta info about table

replica_location(suite) -> [];
replica_location(Nodes) ->
    [Node1,Node2,Node3] = ?acquire_nodes(3,Nodes),
    Tab = replica_location,

    %% Create three replicas
    Schema = [{name,Tab},{disc_only_copies,[Node1]},
	      {ram_copies,[Node2]},{disc_copies,[Node3]}],
    ?match({atomic,ok}, mnesia:create_table(Schema)),
    mnesia_test_lib:verify_replica_location(Tab,[Node1],[Node2],[Node3],Nodes),
    
    %% Delete one replica
    ?match({atomic,ok}, mnesia:del_table_copy(Tab, Node2)),
    mnesia_test_lib:verify_replica_location(Tab,[Node1],[],[Node3],Nodes),

    %% Move one replica
    ?match({atomic,ok}, mnesia:move_table_copy(Tab, Node1, Node2)),
    mnesia_test_lib:verify_replica_location(Tab,[Node2],[],[Node3],Nodes),

    %% Change replica type
    ?match({atomic,ok}, mnesia:change_table_copy_type(Tab, Node2,ram_copies)),
    mnesia_test_lib:verify_replica_location(Tab,[],[Node2],[Node3],Nodes),

    done.

table_lifecycle(suite) -> [];
table_lifecycle(Nodes) ->
    [Node1,Node2] = ?acquire_nodes(2,Nodes),

    ?match({atomic,ok}, mnesia:create_table([{type,bag},
					     {ram_copies,[Node1]},
					     {attributes,[rajtan,tajtan]},
					     {name,order_of_args}])),
    ?match([],mnesia:dirty_read({order_of_args,4711})),
    ?match({atomic,ok}, mnesia:create_table([{name,already_exists},
					    {ram_copies,[Node1]}])),
    ?match({aborted,already_exists},
	   mnesia:create_table([{name,already_exists},{ram_copies,[Node1]}])),
    ?match({aborted,not_a_db_node},
	   mnesia:create_table([{name,no_node},{ram_copies,[foo]}])),
    ?match({aborted,not_a_db_node},
	   mnesia:create_table([{name,no_host},{ram_copies,[foo@bar]}])),
    ?match({aborted,badarg},
	   mnesia:create_table([{name,zero_arity},{attributes,[]}])),
    ?match({aborted,badarg}, mnesia:create_table([])),
    ?match({aborted,badarg}, mnesia:create_table(atom)),
    ?match({aborted,badarg},
	   mnesia:create_table({cstruct,table_name_as_atom})),
    ?match({aborted,bad_type},
	   mnesia:create_table([{name,no_host},{ram_copies,foo}])),
    ?match({aborted,bad_type},
	   mnesia:create_table([{name,no_host},{disc_only_copies,foo}])),
    ?match({aborted,bad_type},
	   mnesia:create_table([{name,no_host},{disc_copies,foo}])),
    
    CreateFun =
	fun() -> ?match({aborted,nested_transaction},
			mnesia:create_table([{name,nested_trans}])), ok
	end,
    ?match({atomic,ok},mnesia:transaction(CreateFun)),
    ?match({atomic,ok},mnesia:create_table([{name,remote_tab},
					    {ram_copies,[Node2]}])),
    
    ?match({atomic,ok}, mnesia:create_table([{name,a_brand_new_tab},
					    {ram_copies,[Node1]}])),
    ?match([],mnesia:dirty_read({a_brand_new_tab,4711})),
    ?match({atomic,ok}, mnesia:delete_table(a_brand_new_tab)),
    ?match({'EXIT',{aborted,no_exists}},
	   mnesia:dirty_read({a_brand_new_tab,4711})),
    ?match({aborted,no_exists}, mnesia:delete_table(a_brand_new_tab)),
    ?match({aborted,badarg}, mnesia:create_table([])),
    
    ?match({atomic,ok}, mnesia:create_table([{name,nested_del_trans},
					     {ram_copies,[Node1]}])),
    DeleteFun = fun() -> ?match({aborted,nested_transaction},
				mnesia:delete_table(nested_del_trans)), ok end,
    ?match({atomic,ok}, mnesia:transaction(DeleteFun)),
    
    ?match({aborted,bad_type},
	   mnesia:create_table([{name,create_with_index},{index,2}])),
    ?match({aborted,bad_index},
	   mnesia:create_table([{name,create_with_index},{index,[-1]}])),
    ?match({aborted,bad_index},
	   mnesia:create_table([{name,create_with_index},{index,[0]}])),
    ?match({aborted,bad_index},
	   mnesia:create_table([{name,create_with_index},{index,[1]}])),
    ?match({aborted,bad_index},
	   mnesia:create_table([{name,create_with_index},{index,[2]}])),
    ?match({atomic,ok},
	   mnesia:create_table([{name,create_with_index},{index,[3]},
			       {ram_copies,[Node1]}])),
    done.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Add, drop and move replicas, change storage types
%% Change table layout (only arity change supported)

replica_management(suite) -> [];
replica_management(Nodes) ->
    %% add_table_copy/3, del_table_copy/2, move_table_copy/3,
    %% change_table_copy_type/3, transform_table/3
    
    [Node1,Node2,Node3] = ?acquire_nodes(3,Nodes),
    
    Tab = replica_management,
    Attrs = [k,v],

    %% 
    %% Add, delete and change replicas
    %%
    ?match({atomic,ok},
	   mnesia:create_table([{name,Tab},{attributes,Attrs},
				{ram_copies,[Node1]}])),
    mnesia_test_lib:verify_replica_location(Tab,[],[Node1],[],Nodes),
    %% R - - 
    ?match({aborted,combine_error},
	   mnesia:add_table_copy(Tab, Node2, disc_copies)),
    ?match({aborted,combine_error},
	   mnesia:change_table_copy_type(Tab, Node1, disc_copies)),
    ?match({atomic,ok}, mnesia:del_table_copy(Tab,Node1)),
    mnesia_test_lib:verify_replica_location(Tab,[],[],[],Nodes),
    %% - - -
    ?match({aborted,no_exists},
	   mnesia:add_table_copy(Tab, Node3, ram_copies)),

    ?match({atomic,ok}, mnesia:create_table([{name,Tab},
					     {attributes,Attrs},
					     {disc_copies,[Node1]}])),
    mnesia_test_lib:verify_replica_location(Tab,[],[],[Node1],Nodes),
    %% D - -
    ?match({aborted,badarg},
	   mnesia:add_table_copy(Tab, Node2, bad_storage_type)),
    ?match({atomic,ok}, mnesia:add_table_copy(Tab, Node2, disc_only_copies)),
    mnesia_test_lib:verify_replica_location(Tab,[Node2],[],[Node1],Nodes),
    %% D DO -
    ?match({atomic,ok}, mnesia:add_table_copy(Tab, Node3, ram_copies)),
    mnesia_test_lib:verify_replica_location(Tab,[Node2],[Node3],[Node1],Nodes),
    %% D DO R
    ?match({atomic,ok},
	   mnesia:change_table_copy_type(Tab, Node1, disc_only_copies)),
    mnesia_test_lib:verify_replica_location(Tab,[Node1,Node2],[Node3],[],Nodes),
    %% DO DO R
    ?match({aborted,already_exists},
	   mnesia:add_table_copy(Tab, Node3, ram_copies)),
    ?match({atomic,ok}, mnesia:del_table_copy(Tab, Node1)),
    mnesia_test_lib:verify_replica_location(Tab,[Node2],[Node3],[],Nodes),
    %% - DO R
    ?match({aborted,_}, mnesia:del_table_copy(Tab, Node1)),
    ?match({atomic,ok}, mnesia:add_table_copy(Tab, Node1, disc_copies)),
    mnesia_test_lib:verify_replica_location(Tab,[Node2],[Node3],[Node1],Nodes),
    %% D DO R
    ?match({atomic,ok},
	   mnesia:change_table_copy_type(Tab, Node3, disc_only_copies)),
    mnesia_test_lib:verify_replica_location(Tab,[Node2,Node3],[],[Node1],Nodes),
    %% D DO DO
    ?match({atomic,ok}, mnesia:del_table_copy(Tab, Node2)),
    mnesia_test_lib:verify_replica_location(Tab,[Node3],[],[Node1],Nodes),
    %% D - DO
    ?match({aborted,already_exists},
	   mnesia:change_table_copy_type(Tab, Node1, disc_copies)),
    
    %% 
    %% Move replica
    %% 
    ?match({atomic,ok}, mnesia:move_table_copy(Tab,Node1,Node2)),
    mnesia_test_lib:verify_replica_location(Tab,[Node3],[],[Node2],Nodes),
    %% - D DO
    ?match({aborted,_}, mnesia:move_table_copy(Tab,Node1,Node2)),
    ?match([], mnesia_test_lib:stop_mnesia([Node3])),
    mnesia_test_lib:verify_replica_location(Tab,[Node3],[],[Node2],
					    Nodes -- [Node3]),
    %% - D DO
    ?match({atomic,ok}, mnesia:move_table_copy(Tab,Node3,Node1)),
    mnesia_test_lib:verify_replica_location(Tab,[Node1],[],[Node2],
					    Nodes -- [Node3]),
    %% DO D -
    ?match([],mnesia_test_lib:start_mnesia([Node3])),
    mnesia_test_lib:verify_replica_location(Tab,[Node1],[],[Node2],Nodes),
    %% DO D -
    
    %% 
    %% Transformer
    %%
    
    NewAttrs = Attrs ++ [extra],
    Transformer =
	fun(Rec) -> list_to_tuple(tuple_to_list(Rec) ++ [initial_value]) end,
    ?match({atomic,ok}, mnesia:transform_table(Tab, Transformer,NewAttrs)),
    ?match({atomic,ok},  mnesia:transform_table(Tab, fun(R) -> R end, Attrs)),
    ?match({aborted,bad_type}, mnesia:transform_table(Tab, Transformer, 0)),
    ?match({aborted,bad_type}, mnesia:transform_table(Tab, Transformer, -1)),
    ?match({aborted,badarg}, mnesia:transform_table(Tab, Transformer, [])),
    ?match({aborted,bad_type}, mnesia:transform_table(Tab, no_fun, NewAttrs)),
    
    NestedFun =
	fun() ->
		?match({aborted,_},
		       mnesia:move_table_copy(Tab,Node1,Node2)),
		?match({aborted,_},
		       mnesia:add_table_copy(Tab,Node1,ram_copies)),
		?match({aborted,_},
		       mnesia:del_table_copy(Tab,Node1)),
		T = fun(_) -> 4711 end,
		?match({aborted,_},
		       mnesia:transform_table(Tab,Transformer, T)),
		ok
	end,
    ?match({atomic,ok},mnesia:transaction(NestedFun)),
    done.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Add and drop indecies

index_lifecycle(suite) ->
    [ add_table_index, create_live_table_index, del_table_index ].

%% Add table index

add_table_index(suite) -> [];
add_table_index(Nodes) ->
    [Node1] = ?acquire_nodes(1,Nodes),
    Tab = add_table_index,
    Schema = [{name,Tab},{attributes,[k,v]},{ram_copies,[Node1]}],
    ?match({atomic,ok}, mnesia:create_table(Schema)),
    ValPos = 3,
    BadValPos = ValPos + 1,
    ?match({aborted,bad_index}, mnesia:add_table_index(Tab,BadValPos)),
    ?match({aborted,bad_index}, mnesia:add_table_index(Tab,2)),
    ?match({aborted,bad_index}, mnesia:add_table_index(Tab,1)),
    ?match({aborted,bad_index}, mnesia:add_table_index(Tab,0)),
    ?match({aborted,bad_index}, mnesia:add_table_index(Tab,-1)),
    ?match({atomic,ok}, mnesia:add_table_index(Tab,ValPos)),
    ?match({aborted,already_exists}, mnesia:add_table_index(Tab,ValPos)),
    
    NestedFun = fun() ->
			?match({aborted,nested_transaction},
			       mnesia:add_table_index(Tab,ValPos)),
			
			ok
		end,
    ?match({atomic,ok},mnesia:transaction(NestedFun)),
    done.

create_live_table_index(suite) -> [];
create_live_table_index(Nodes) ->
    [Node1] = ?acquire_nodes(1,Nodes),
    Tab = create_live_table_index,
    Schema = [{name,Tab},{attributes,[k,v]},{ram_copies,[Node1]}],
    ?match({atomic,ok}, mnesia:create_table(Schema)),
    ValPos = 3,
    mnesia:dirty_write({Tab,1,2}),
    
    Fun = fun() ->
		  ?match(ok, mnesia:write({Tab,2,2})),	
		  ok
	  end,
    ?match({atomic,ok},mnesia:transaction(Fun)),
    ?match({atomic,ok}, mnesia:add_table_index(Tab,ValPos)),
    done.

%% Drop table index

del_table_index(suite) ->[];
del_table_index(Nodes) ->
    [Node1] = ?acquire_nodes(1,Nodes),
    Tab = del_table_index,
    Schema = [{name,Tab},{attributes,[k,v]},{ram_copies,[Node1]}],
    ?match({atomic,ok}, mnesia:create_table(Schema)),
    ValPos = 3,
    BadValPos = ValPos + 1,
    ?match({atomic,ok}, mnesia:add_table_index(Tab,ValPos)),
    ?match({aborted,no_exists},
	   mnesia:del_table_index(Tab,BadValPos)),
    ?match({atomic,ok}, mnesia:del_table_index(Tab,ValPos)),
    
    NestedFun =
	fun() ->
		?match({aborted,nested_transaction},
		       mnesia:del_table_index(Tab,ValPos)),
		ok
	end,
    ?match({atomic,ok},mnesia:transaction(NestedFun)),
    done.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Syncronize table with log or disc
%% 
table_sync(suite) -> 
    [ dump_tables, dump_log, change_dump_log_config, wait_for_tables, force_load_table ].

%% Dump ram tables on disc
dump_tables(suite) -> [];
dump_tables(Nodes) ->
    [Node1,Node2] = ?acquire_nodes(2,Nodes),
    Tab = dump_tables,
    Schema = [{name,Tab},{attributes,[k,v]},{ram_copies,[Node2]}],
    ?match({atomic,ok}, mnesia:create_table(Schema)),

    %% Dump 10 records
    Size = 10,
    Keys = lists:seq(1,Size),
    Records = [{Tab,A,7} || A <- Keys],
    lists:foreach(fun(Rec) -> ?match(ok,mnesia:dirty_write(Rec)) end,Records),
    AllKeys = fun() -> lists:sort(mnesia:all_keys(Tab)) end,
    
    ?match({atomic,Keys}, mnesia:transaction(AllKeys)),
    ?match(ok, mnesia:dump_tables(Tab)),

    %% Delete one record
    ?match(ok,mnesia:dirty_delete({Tab,5})),
    Keys2 = lists:delete(5,Keys),
    ?match({atomic,Keys2}, mnesia:transaction(AllKeys)),

    %% Check that all 10 is restored after a stop
    ?match([], mnesia_test_lib:stop_mnesia([Node1,Node2])),
    ?match([],mnesia_test_lib:start_mnesia([Node1,Node2])),
    ?match(ok,mnesia:wait_for_tables([Tab],infinity)),
    ?match({atomic,Keys}, mnesia:transaction(AllKeys)),

    ?match(ok, mnesia:dump_tables([foo])),
    done.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Make Mnesia table accessible via SNMP

snmp_access(suite) ->
    [
     snmp_open_table, snmp_close_table,
     snmp_get_row, snmp_get_next_index, snmp_get_mnesia_key
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check that the debug support has not decayed

debug_support(suite) ->
    [  info, schema, schema, kill, lkill ].

