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

-module(snmpa_mib_data_ttln).

%%%-----------------------------------------------------------------
%%% 
%%%           THIS FILE IS JUST A PLACE HOLDER - IGNORE
%%% 
%%%-----------------------------------------------------------------


%%%-----------------------------------------------------------------
%%% 
%%%        TTLN - TupleTreeListNodes
%%% 
%%% This module implements the MIB internal data structures.
%%% An MIB Data Structure consists of three items; an ets-table,
%%% a tree and a list of registered subagents.
%%% The subagent information is consequently duplicated. It resides
%%% both in the tree and in the list.
%%% The ets-table contains all data associated with each variable,
%%% table, tableentry and tablecolumn in the MIB.
%%% The tree contains information of the Oids in the MIB.
%%%
%%% When a mib is loaded, the tree is built from the plain list
%%% in the binary file.
%%% 
%%%-----------------------------------------------------------------

-include("snmp_types.hrl").
-include("snmp_debug.hrl").

-define(VMODULE,"MDATA_TTLN").
-include("snmp_verbosity.hrl").

-behaviour(snmpa_mib_data).

-define(MIB_DATA, snmpa_mib_data).
-define(MIB_NODE, snmpa_mib_node).
-define(MIB_TREE, snmpa_mib_tree).
-define(DUMMY_TREE_GENERATION, 1).
-define(DEFAULT_TREE, {tree,{undefined_node},internal}).


%%%-----------------------------------------------------------------
%%% Table of contents
%%% =================
%%% 1. Interface
%%% 2. Implementation of tree access
%%% 3. Tree building functions
%%% 4. Tree merging
%%% 5. Tree deletion routines
%%% 6. Functions for subagent handling
%%% 7. Misc functions
%%%-----------------------------------------------------------------


%%----------------------------------------------------------------------
%% data_db is an database containing loaded mibs as:
%%    {MibName = atom(), Symbolic = ?, FullFileName = string()}
%%    it is either ets or mnesia
%% tree_db is a database containing _one_ record with the tree!
%% (the reason for this is part to get replication and part out of convenience)
%% ref_tree is the root node, without any subagent.
%% tree is the root node (same as ref_tree but with the subagents added).
%% subagents is a list of {SAPid, Oid}
%%----------------------------------------------------------------------
-record(mib_data, {mib_db,  % table of #mib_info
		   node_db, % table of #node_info
		   tree_db, % table of #tree
		   tree,    % The actual tree
		   subagents = []}).

-record(mib_info,  {name, symbolic, file_name}).
-record(node_info, {oid, mib_name, me}).


%% API
-export([new/0, new/1, sync/1, close/1, 
	 load_mib/4, unload_mib/4, which_mibs/1, whereis_mib/2, 
	 info/1, info/2,
	 dump/1, dump/2, 
	 backup/2, 
	 lookup/2, next/3, which_mib/2, 
	 register_subagent/3, unregister_subagent/2]).

%% Internal exports
-export([code_change/2]).


%%-----------------------------------------------------------------
%% A tree is represented as a N-tuple, where each element is a
%% node. A node is:
%% 1) {tree, Tree, Info} where Info can be {table, Id}, {table_entry, Id}
%%                                        or perhaps 'internal'
%% 2) undefined_node  (memory optimization (instead of {node, undefined}))
%% 3) {node, Info} where Info can be {subagent, Pid}, {variable, Id}, 
%%                                   {table_column, Id}
%% Id is {MibName, MibEntry}
%% The over all root is represented as {tree, Tree, internal}.
%%
%% tree() = {tree, nodes(), tree_info()}
%% nodes() = [tree() | node() | undefined_node]
%% node() = {node, node_info()}
%% tree_info() = {table, Id} | {table_entry, Id} | internal
%% node_info() = {subagent, Pid} | {variable, Id} | {table_colum, Id}
%%-----------------------------------------------------------------

-type tree_generation() :: non_neg_integer().
-type tree()            :: #tree{}.
-type tree_nodes()      :: [tree_node()].
-type tree_node()       :: tree() | 
			   tree_node_elem() | 
			   tree_node_empty().
-type tree_node_elem()  :: {node, tree_node_info()}.
-type tree_node_info()  :: {subagent,     Pid :: pid()} | 
			   {variable,     Id  :: non_neg_integer()} | 
			   {table_column, Id  :: non_neg_integer()}.
-type tree_node_empty() :: {undefined_node, N :: pos_integer()}.
-type tree_info()       :: {table,       Id :: non_neg_integer()} | 
			   {table_entry, Id :: non_neg_integer()} | 
			   internal.


%% This record is what is stored in the database. The 'tree' part
%% is described above...
-record(mtree, 
	{
	  generation = ?DUMMY_TREE_GENERATION :: tree_generation(), 
	  root       = ?DEFAULT_TREE          :: tree()
	}).

-record(tree, 
	{
	  %% The number of nodes is *not* actually the length of the
	  %% nodes list. Since the undefined-node(s) can be collapsed
	  %% into {undefined_node, N} we need to keep track of the 
	  %% actual size some other way (so that we dont have the 
	  %% traverse the nodes every time we want to check an index).
	  num_nodes :: non_neg_integer(), 
	  nodes     :: tree_nodes(), 
	  tree_info :: tree_info()
	}).




%%%======================================================================
%%% 1. Interface
%%%======================================================================

%%-----------------------------------------------------------------
%% Func: new/0, new/1
%% Returns: A representation of mib data.
%%-----------------------------------------------------------------
new() ->
    new(ets).

%% Where -> A list of nodes where the tables will be created
new(Storage) ->
    %% First we must check if there is already something to read
    %% If a database already exists, then the tree structure has to be read
    ?vtrace("open (mib) database",[]),
    MibDb = snmpa_general_db:open(Storage, ?MIB_DATA,
				  mib_info,
				  record_info(fields, mib_info), set),
    ?vtrace("open (mib) node database",[]),
    NodeDb = snmpa_general_db:open(Storage, ?MIB_NODE,
				   node_info,
				   record_info(fields, node_info), set),
    ?vtrace("open (mib) tree database",[]),
    TreeDb = snmpa_general_db:open(Storage, ?MIB_TREE,
				   tree,
				   record_info(fields, mtree), set),
    MTree = 
	case snmpa_general_db:read(TreeDb, ?DUMMY_TREE_GENERATION) of
	    false ->
		T = #mtree{},
		snmpa_general_db:write(TreeDb, T),
		T;
	    {value, T} ->
		T
	end,
    install_mibs(MibDb, NodeDb),
    #mib_data{mib_db   = MibDb, 
	      node_db  = NodeDb,
	      tree_db  = TreeDb, 
	      mtree    = MTree}.


%%----------------------------------------------------------------------
%% Returns: new mib data | {error, Reason}
%%----------------------------------------------------------------------
load_mib(MibData,FileName,MeOverride,TeOverride) 
  when is_record(MibData,mib_data) andalso is_list(FileName) -> 
    ?vlog("load mib file: ~p",[FileName]),
    ActualFileName = filename:rootname(FileName, ".bin") ++ ".bin",
    MibName = list_to_atom(filename:basename(FileName, ".bin")),
    (catch do_load_mib(MibData, ActualFileName, MibName, 
                       MeOverride, TeOverride)).

do_load_mib(MibData, ActualFileName, MibName, MeOverride, TeOverride) ->
    ?vtrace("do_load_mib -> entry with"
        "~n  ActualFileName: ~s"
        "~n  MibName:        ~p",[ActualFileName, MibName]),
    #mib_data{mib_db  = MibDb, 
	      node_db = NodeDb, 
	      %% tree_db = TreeDb,
	      tree    = Tree} = MibData,
    verify_not_loaded(MibDb, MibName),
    ?vtrace("do_load_mib -> already loaded mibs:"
	"~n   ~p",[loaded(MibDb)]),
    Mib = do_read_mib(ActualFileName),
    ?vtrace("do_load_mib -> read mib ~s",[Mib#mib.name]),
    NonInternalMes = 
        lists:filter(fun(ME) -> maybe_drop_me(ME) end, Mib#mib.mes),
    OldRoot = Tree#tree.root,
    T = build_tree(NonInternalMes, MibName),
    ?d("load_mib -> "
	"~n   OldRoot: ~p"
	"~n   T:       ~p", [OldRoot, T]),
    case (catch merge_nodes(T, OldRoot)) of
	{error_merge_nodes, Node1, Node2} ->
	    ?vlog("error merging nodes:"
		"~n~p~nand~n~p", [Node1,Node2]),
	    {error, oid_conflict};
	NewRoot when is_tuple(NewRoot) andalso (element(1,NewRoot) =:= tree) ->
 	    ?d("load_mib -> "
 		"~n   NewRoot: ~p", [NewRoot]),
	    Symbolic = not lists:member(no_symbolic_info, Mib#mib.misc),
	    case (catch check_notif_and_mes(TeOverride, MeOverride, Symbolic, 
					    Mib#mib.traps, NonInternalMes)) of
		true ->
		    install_mes(NodeDb, MibName, NonInternalMes),
		    install_mib(MibDb, Symbolic, Mib, 
				MibName, ActualFileName, NonInternalMes),
		    ?vtrace("installed mib ~s", [Mib#mib.name]),
		    Tree2 = Tree#tree{root = NewRoot},
		    %% snmpa_general_db:write(TreeDb, Tree2), %% Store later?
		    {ok, MibData#mib_data{tree = Tree2}};
		Else -> 
		    Else
	    end
    end.


verify_not_loaded(Db, Name) ->
    case snmpa_general_db:read(Db, Name) of
        {value, #mib_info{name = Name}} -> 
            throw({error, 'already loaded'});
        false ->
            ok
    end.

do_read_mib(ActualFileName) ->
    case snmp_misc:read_mib(ActualFileName) of
        {error, Reason} -> 
            ?vlog("Failed reading mib file ~p with reason: ~p",
                [ActualFileName,Reason]),
            throw({error, Reason});
        {ok, Mib} ->
            Mib
    end.

%% The Tree DB is handled in a special way since it can be very large.
sync(#mib_data{mib_db  = M, 
	       node_db = N, 
	       tree_db = T, tree = Tree, subagents = []}) ->
    snmpa_general_db:sync(M),
    snmpa_general_db:sync(N),
    snmpa_general_db:write(T, Tree),
    snmpa_general_db:sync(T);
sync(#mib_data{mib_db  = M, 
	       node_db = N, 
	       tree_db = T, tree = Tree, subagents = SAs}) ->

    snmpa_general_db:sync(M),
    snmpa_general_db:sync(N),

    %% Ouch. Since the subagent info is dynamic we do not 
    %% want to store the tree containing subagent info. So, we 
    %% have to create a tmp tree without those and store it.

    case delete_subagents(Tree, SAs) of
	{ok, TreeWithoutSAs} ->
	    snmpa_general_db:write(T, TreeWithoutSAs),
	    snmpa_general_db:sync(T);
	Error ->
	    Error
    end.

delete_subagents(Tree, []) ->
    {ok, Tree};
delete_subagents(Tree0, [{_, Oid}|SAs]) ->
    case (catch delete_subagent(Tree0, Oid)) of
	{tree, _Tree, _Info} = Tree1 ->
	    delete_subagents(Tree1, SAs);
	_Error ->
	    {error, {'invalid oid', Oid}} 
    end.

%%----------------------------------------------------------------------
%% (OTP-3601)
%%----------------------------------------------------------------------
check_notif_and_mes(TeOverride,MeOverride,Symbolic,Traps,MEs) ->
    ?vtrace("check notifications and mib entries",[]),
    check_notifications(TeOverride,Symbolic,Traps),
    check_mes(MeOverride,MEs).

check_notifications(true, _Symbolic, _Traps) ->
    ?vtrace("trapentry override = true => skip check",[]),
    true;
check_notifications(_, Symbolic, Traps) -> 
    check_notifications(Symbolic, Traps).

check_notifications(true, Traps) ->
    check_notifications(Traps);
check_notifications(_, _) -> true.

check_notifications([]) -> true;
check_notifications([#trap{trapname = Key} = Trap | Traps]) ->
    ?vtrace("check notification [trap] with Key: ~p",[Key]),
    case snmpa_symbolic_store:get_notification(Key) of
	{value, Trap} -> check_notifications(Traps);
	{value,    _} -> throw({error, {'trap already defined', Key}});
	undefined     -> check_notifications(Traps)
    end;
check_notifications([#notification{trapname = Key} = Notif | Traps]) ->
    ?vtrace("check notification [notification] with Key: ~p",[Key]),
    case snmpa_symbolic_store:get_notification(Key) of
	{value, Notif} -> 
	    check_notifications(Traps);
	{value,     _} -> 
	    throw({error, {'notification already defined', Key}});
	undefined      -> 
	    check_notifications(Traps)
    end;
check_notifications([Crap | Traps]) ->
    ?vlog("skipped check of: ~n~p",[Crap]),
    check_notifications(Traps).

check_mes(true,_) ->
    ?vtrace("mibentry override = true => skip check",[]),
    true; 
check_mes(_,MEs) ->
    check_mes(MEs).

check_mes([]) -> true;
check_mes([#me{aliasname = Name, oid = Oid1} | MEs]) ->
    ?vtrace("check mib entries with aliasname: ~p",[Name]),
    case snmpa_symbolic_store:aliasname_to_oid(Name) of
	{value, Oid1} -> 
	    check_mes(MEs);
	{value, Oid2} -> 
	    ?vinfo("~n   expecting '~p'~n   but found '~p'",[Oid1, Oid2]),
	    throw({error, {'mibentry already defined', Name}});
	false -> 
	    check_mes(MEs)
    end;
check_mes([Crap | MEs]) ->
    ?vlog("skipped check of: ~n~p",[Crap]),
    check_mes(MEs).
    


%%----------------------------------------------------------------------
%% Returns: new mib data | {error, Reason}
%%----------------------------------------------------------------------
unload_mib(MibData, FileName, _, _) when is_list(FileName) -> 
    MibName = list_to_atom(filename:basename(FileName, ".bin")),
    (catch do_unload_mib(MibData, MibName)).

do_unload_mib(MibData, MibName) ->
    ?vtrace("do_unload_mib -> entry with"
	"~n   MibName: ~p", [MibName]),
    #mib_data{mib_db  = MibDb, 
	      node_db = NodeDb, 
	      %% tree_db = TreeDb, 
	      tree    = Tree} = MibData,
    #mib_info{symbolic = Symbolic} = verify_loaded(MibDb, MibName),
    NewRoot = delete_mib_from_tree(MibName, Tree#tree.root),
    MEs = uninstall_mes(NodeDb, MibName),
    uninstall_mib(MibDb, Symbolic, MibName, MEs),
    NewMibData = MibData#mib_data{tree = Tree#tree{root = NewRoot}},
    {ok, NewMibData}.

verify_loaded(Db, Name) ->
    case snmpa_general_db:read(Db, Name) of
        {value, MibInfo} ->
            MibInfo;
        false ->
            throw({error, 'not loaded'})
    end.
    
    
close(#mib_data{mib_db = MibDb, node_db = NodeDb, tree_db = TreeDb}) ->
    snmpa_general_db:close(MibDb),
    snmpa_general_db:close(NodeDb),
    snmpa_general_db:close(TreeDb),
    ok.

register_subagent(#mib_data{tree = T} = MibData, Oid, Pid) ->
    case insert_subagent(Oid, T#tree.root) of
	{error, Reason} -> 
	    {error, Reason};
	NewRootTree ->
	    SAs = [{Pid, Oid} | MibData#mib_data.subagents],
	    T2 = T#tree{root = NewRootTree},
	    MibData#mib_data{tree = T2, subagents = SAs}
    end.


%%----------------------------------------------------------------------
%% Purpose: Get a list of all loaded mibs
%% Returns: [{Name, File}]
%%----------------------------------------------------------------------

which_mibs(#mib_data{mib_db = Db}) ->
    Mibs = snmpa_general_db:tab2list(Db),
    [{Name, File} || #mib_info{name = Name, file_name = File} <- Mibs].


%%----------------------------------------------------------------------
%% Purpose: Get a list of all loaded mibs
%% Returns: [{Name, File}]
%%----------------------------------------------------------------------

whereis_mib(#mib_data{mib_db = Db}, Name) ->
    case snmpa_general_db:read(Db, Name) of
        {value, #mib_info{file_name = File}} ->
	    {ok, File};
	false ->
	    {error, not_found}
    end.


%%----------------------------------------------------------------------
%% Purpose: Deletes SA with Pid from all subtrees it handles.
%% Returns: NewMibData.
%%----------------------------------------------------------------------
unregister_subagent(MibData, Pid) when is_pid(Pid) ->
    SAs = MibData#mib_data.subagents,
    case lists:keysearch(Pid, 1, SAs) of
	false -> MibData;
	{value, {Pid, Oid}} ->
	    % we should never get an error since Oid is found in MibData.
	    {ok, NewMibData, _DeletedSA} = unregister_subagent(MibData, Oid),
	    % continue if the same Pid handles other mib subtrees.
	    unregister_subagent(NewMibData, Pid)
    end;

%%----------------------------------------------------------------------
%% Purpose: Deletes one unique subagent. 
%% Returns: {error, Reason} | {ok, NewMibData, DeletedSubagentPid}
%%----------------------------------------------------------------------
unregister_subagent(#mib_data{tree = T} = MibData, Oid) when is_list(Oid) ->
    case catch delete_subagent(T#tree.root, Oid) of
	{tree, Tree, Info} ->
	    OldSAs = MibData#mib_data.subagents,
	    {value, {Pid, _Oid}} = lists:keysearch(Oid, 2, OldSAs),
	    SAs = lists:keydelete(Oid, 2, OldSAs),
	    T2 = T#tree{root = {tree, Tree, Info}},
	    {ok, 
	     MibData#mib_data{tree = T2, subagents = SAs},
	     Pid};
	_ ->
	    {error, {'invalid oid', Oid}}
    end.

%%----------------------------------------------------------------------
%% Purpose: To inpect memory usage, loaded mibs, registered subagents
%%----------------------------------------------------------------------
info(MibData) ->
    ?vtrace("retrieve info",[]),
    #mib_data{mib_db = MibDb, node_db = NodeDb, tree_db = TreeDb, 
	      tree = Tree, subagents = SAs} = MibData,
    LoadedMibs = old_format(snmpa_general_db:tab2list(MibDb)),
    TreeSize = snmp_misc:mem_size(Tree),
    {memory, ProcSize} = erlang:process_info(self(),memory),
    MibDbSize  = snmpa_general_db:info(MibDb,  memory),
    NodeDbSize = snmpa_general_db:info(NodeDb, memory),
    TreeDbSize = snmpa_general_db:info(TreeDb, memory),
    [{loaded_mibs, LoadedMibs}, {subagents, SAs}, {tree_size_bytes, TreeSize},
     {process_memory, ProcSize}, 
     {db_memory, [{mib,MibDbSize},{node,NodeDbSize},{tree,TreeDbSize}]}].

info(#mib_data{mib_db = MibDb}, loaded_mibs) ->
    Mibs = snmpa_general_db:tab2list(MibDb),
    [filename:rootname(FN, ".bin") || #mib_info{file_name = FN} <- Mibs];
info(#mib_data{tree = Tree}, tree_size_bytes) ->
    snmp_misc:mem_size(Tree);
info(_, process_memory) ->
    {memory, ProcSize} = erlang:process_info(self(),memory),
    ProcSize;
info(#mib_data{mib_db = MibDb, node_db = NodeDb, tree_db = TreeDb}, 
     db_memory) ->
    MibDbSize  = snmpa_general_db:info(MibDb,  memory),
    NodeDbSize = snmpa_general_db:info(NodeDb, memory),
    TreeDbSize = snmpa_general_db:info(TreeDb, memory),
    [{mib,MibDbSize},{node,NodeDbSize},{tree,TreeDbSize}];
info(#mib_data{subagents = SAs}, subagents) ->
    SAs.

old_format(LoadedMibs) ->
    ?vtrace("convert mib info to old format",[]),
    [{N,S,F} || #mib_info{name=N,symbolic=S,file_name=F} <- LoadedMibs].

    
%%----------------------------------------------------------------------
%% A total dump for debugging.
%%----------------------------------------------------------------------
dump(#mib_data{mib_db = MibDb, node_db = NodeDb, tree = Tree}) ->
    (catch io:format("MIB-tables:~n~p~n~n", 
		     [snmpa_general_db:tab2list(MibDb)])),
    (catch io:format("MIB-entries:~n~p~n~n", 
		     [snmpa_general_db:tab2list(NodeDb)])),
    (catch io:format("Tree:~n~p~n", [Tree])), % good luck reading it!
    ok.

dump(#mib_data{mib_db = MibDb, node_db = NodeDb, tree = Tree}, File) ->
    case file:open(File,[write]) of
	{ok, Fd} ->
	    io:format(Fd,"~s~n", 
		      [snmp:date_and_time_to_string(snmp:date_and_time())]),
	    (catch io:format(Fd,"MIB-tables:~n~p~n~n",
			     [snmpa_general_db:tab2list(MibDb)])),
	    (catch io:format(Fd, "MIB-entries:~n~p~n~n", 
			     [snmpa_general_db:tab2list(NodeDb)])),
	    io:format(Fd,"Tree:~n~p~n", [Tree]), % good luck reading it!
	    file:close(Fd),
	    ok;
	{error,Reason} ->
	    ?vinfo("~n   Failed opening file '~s' for reason ~p",
		   [File,Reason]),
	    {error,Reason}
    end.


backup(#mib_data{mib_db = M, node_db = N, tree_db = T}, BackupDir) ->
    MRes = snmpa_general_db:backup(M, BackupDir),
    NRes = snmpa_general_db:backup(N, BackupDir),
    TRes = snmpa_general_db:backup(T, BackupDir),
    handle_backup_res([{mib_db, MRes}, {node_db, NRes}, {tree_db, TRes}]).

handle_backup_res(Res) ->
    handle_backup_res(Res, []).

handle_backup_res([], []) ->
    ok;
handle_backup_res([], Err) ->
    {error, lists:reverse(Err)};
handle_backup_res([{_, ok}|Res], Err) ->
    handle_backup_res(Res, Err);
handle_backup_res([{Tag, {error, Reason}}|Res], Err) ->
    handle_backup_res(Res, [{Tag, Reason}|Err]);
handle_backup_res([{Tag, Error}|Res], Err) ->
    handle_backup_res(Res, [{Tag, Error}|Err]).


%%%======================================================================
%%% 2. Implementation of tree access
%%%    lookup and next.
%%%======================================================================


which_mib(#mib_data{tree = T} = D, Oid) ->
    ?vtrace("which_mib -> entry with"
	    "~n   Oid: ~p",[Oid]),	    
    case (catch find_node(D, T#tree.root, Oid, [])) of
	{variable, _ME, Mib} ->
	    ?vtrace("which_mib -> variable:"
		"~n   Mib: ~p", [Mib]),	    
	    {ok, Mib};
	{table, _EntryME, _, Mib} ->
	    ?vtrace("which_mib -> table:"
		"~n   Mib: ~p", [Mib]),	    
	    {ok, Mib};
	{subagent, SubAgentPid, _SANextOid} ->
	    ?vtrace("which_mib -> subagent:"
		"~n   SubAgentPid: ~p", [SubAgentPid]),	    
	    {error, {subagent, SubAgentPid}};
	{false, ErrorCode} -> 
	    ?vtrace("which_mib -> false:"
		"~n   ErrorCode: ~p",[ErrorCode]),	    
	    {error, ErrorCode};
	false -> 
	    ?vtrace("which_mib -> false",[]),	    
	    {error, noSuchObject};
	{'EXIT', R} -> 
	    ?vtrace("which_mib -> exit:"
		"~n   R:  ~p",[R]),	    
	    {error, noSuchObject}
    end.


%%-----------------------------------------------------------------
%% Func: lookup/2
%% Purpose: Finds the mib entry corresponding to the Oid. If it is a
%%          variable, the Oid must be <Oid for var>.0 and if it is
%%          a table, Oid must be <table>.<entry>.<col>.<any>
%% Returns: {variable, MibEntry} |
%%          {table_column, MibEntry, TableEntryOid} |
%%          {subagent, SubAgentPid, SAOid} |
%%          {false, Reason}
%%-----------------------------------------------------------------
lookup(#mib_data{tree = T} = D, Oid) ->
     ?vtrace("lookup -> entry with"
 	"~n   Oid: ~p",[Oid]),	    
    case (catch find_node(D, T#tree.root, Oid, [])) of
	{variable, ME, _Mib} when is_record(ME, me) -> 
	    ?vtrace("lookup -> variable:"
		"~n   ME: ~p",[ME]),	    
	    {variable, ME};
	{table, EntryME, {ColME, TableEntryOid}, _Mib} ->
	    ?vtrace("lookup -> table:"
		"~n   EntryME:          ~p"
		"~n   ColME:            ~p"
		"~n   RevTableEntryOid: ~p",
		[EntryME, ColME, TableEntryOid]),	    
	    MFA = EntryME#me.mfa,
	    RetME = ColME#me{mfa = MFA},
	    {table_column, RetME, TableEntryOid};
	{subagent, SubAgentPid, SANextOid} ->
	    ?vtrace("lookup -> subagent:"
		"~n   SubAgentPid: ~p"
		"~n   SANextOid:   ~p", [SubAgentPid, SANextOid]),	    
	    {subagent, SubAgentPid, SANextOid};
	{false, ErrorCode} -> 
	    ?vtrace("lookup -> false:"
		"~n   ErrorCode: ~p",[ErrorCode]),	    
	    {false, ErrorCode};
	false -> 
	    ?vtrace("lookup -> false",[]),	    
	    {false, noSuchObject};
	{'EXIT', R} -> 
	    ?vtrace("lookup -> exit:"
		"~n   R:  ~p",[R]),	    
	    {false, noSuchObject}
    end.


find_node(D, {tree, Tree, {table, _}}, RestOfOid, RevOid) ->
     ?vtrace("find_node(tree,table) -> entry with"
 	"~n   RestOfOid: ~p"
 	"~n   RevOid:    ~p",[RestOfOid, RevOid]),
    find_node(D, {tree, Tree, internal}, RestOfOid, RevOid);
find_node(D, {tree, Tree, {table_entry, _}}, RestOfOid, RevOid) ->
     ?vtrace("find_node(tree,table_entry) -> entry with"
 	"~n   RestOfOid: ~p"
 	"~n   RevOid:    ~p",[RestOfOid, RevOid]),
    #mib_data{node_db = Db} = D,
    Oid = lists:reverse(RevOid),
    case snmpa_general_db:read(Db, Oid) of
	{value, #node_info{me = ME, mib_name = Mib}} ->
	    case find_node(D, {tree, Tree, internal}, RestOfOid, RevOid) of
		{false, ErrorCode} -> {false, ErrorCode};
		Val -> {table, ME, Val, Mib}
	    end;
	false ->
	    ?vinfo("find_node -> could not find table_entry ME with"
		"~n   RevOid:    ~p"
		"~n   when"
		"~n   RestOfOid: ~p", 
		[RevOid, RestOfOid]),
	    false
    end;
find_node(D, {tree, Tree, _Internal}, [Int | RestOfOid], RevOid) ->
    ?vtrace("find_node(tree) -> entry with"
	"~n   Int:       ~p"
	"~n   RestOfOid: ~p"
	"~n   RevOid:    ~p",[Int, RestOfOid, RevOid]),
    find_node(D, element(Int+1, Tree), RestOfOid, [Int | RevOid]);
find_node(D, {node, {table_column, _}}, RestOfOid, [ColInt | RevOid]) ->
     ?vtrace("find_node(tree,table_column) -> entry with"
 	"~n   RestOfOid: ~p"
 	"~n   ColInt:    ~p"
 	"~n   RevOid:    ~p",[RestOfOid, ColInt, RevOid]),
    #mib_data{node_db = Db} = D,
    Oid = lists:reverse([ColInt | RevOid]),
    case snmpa_general_db:read(Db, Oid) of
	{value, #node_info{me = ME}} ->
	    {ME, lists:reverse(RevOid)}; 
	false ->
	    X = snmpa_general_db:read(Db, lists:reverse([ColInt | RevOid])),
	    ?vinfo("find_node -> could not find table_column ME with"
		"~n   RevOid: ~p"
		"~n   trying  [~p|~p]"
		"~n   X:      ~p", 
		[RevOid, [ColInt | RevOid], X]),
	    false
    end;
find_node(D, {node, {variable, _MibName}}, [0], RevOid) ->
     ?vtrace("find_node(tree,variable,[0]) -> entry with"
 	"~n   RevOid:    ~p",[RevOid]),
    #mib_data{node_db = Db} = D,
    Oid = lists:reverse(RevOid),
    %% {value, #node_info{me = ME}} = snmpa_general_db:read(Db, Oid),
    case snmpa_general_db:read(Db, Oid) of
	{value, #node_info{me = ME, mib_name = Mib}} ->
	    {variable, ME, Mib};
	false ->
	    ?vinfo("find_node -> could not find variable ME with"
		"~n   RevOid: ~p", [RevOid]),
	    false
    end;
find_node(_D, {node, {variable, _MibName}}, [], _RevOid) ->
    ?vtrace("find_node(tree,variable,[]) -> entry",[]),
    {false, noSuchObject};
find_node(_D, {node, {variable, _MibName}}, _, _RevOid) ->
    ?vtrace("find_node(tree,variable) -> entry",[]),
    {false, noSuchInstance};
find_node(D, {node, subagent}, _RestOfOid, SARevOid) ->
    ?vtrace("find_node(tree,subagent) -> entry with"
 	"~n   SARevOid:    ~p",[SARevOid]),
    #mib_data{subagents = SAs} = D,
    SAOid = lists:reverse(SARevOid),
    case lists:keysearch(SAOid, 2, SAs) of
	{value, {SubAgentPid, SAOid}} ->
	    {subagent, SubAgentPid, SAOid};
	false ->
	    ?vinfo("find_node -> could not find subagent with"
		"~n   SAOid: ~p"
		"~n   SAs:   ~p", [SAOid, SAs]),
	    false
    end;    
find_node(_D, Node, _RestOfOid, _RevOid) ->
    ?vtrace("find_node -> failed:~n~p",[Node]),
    {false, noSuchObject}.


%%-----------------------------------------------------------------
%% Func: next/3
%% Purpose: Finds the lexicographically next oid.
%% Returns: endOfMibView |
%%          {subagent, SubAgentPid, SAOid} |
%%          {variable, MibEntry, VarOid} |
%%          {table, TableOid, TableRestOid, MibEntry}
%% If a variable is returnes, it is in the MibView.
%% If a table or subagent is returned, it *may* be in the MibView.
%%-----------------------------------------------------------------
next(#mib_data{tree = T} = D, Oid, MibView) ->
    case catch next_node(D, T#tree.root, Oid, [], MibView) of
	false -> endOfMibView;
	Else -> Else
    end.

%%-----------------------------------------------------------------
%% This function is used as long as we have any Oid left. Take
%% one integer at a time from the Oid, and traverse the tree
%% accordingly. When the Oid is empty, call find_next.
%% Returns: {subagent, SubAgentPid, SAOid} |
%%          false |
%%          {variable, MibEntry, VarOid} |
%%          {table, TableOid, TableRestOid, MibEntry}
%%-----------------------------------------------------------------
next_node(_D, undefined_node, _Oid, _RevOidSoFar, _MibView) ->
    ?vtrace("next_node(undefined_node) -> entry", []),
    false;

next_node(_D, {tree, Tree, {table_entry, _Id}}, [Int | _Oid], 
	  _RevOidSoFar, _MibView)
  when Int+1 > size(Tree) ->
    ?vtrace("next_node(tree,table_entry) -> entry when not found whith"
	"~n   Int:        ~p"
	"~n   size(Tree): ~p", [Int, size(Tree)]),
    false;
next_node(D, {tree, Tree, {table_entry, _MibName}},
	  Oid, RevOidSoFar, MibView) ->
    ?vtrace("next_node(tree,table_entry) -> entry when"
	"~n   size(Tree):  ~p"
	"~n   Oid:         ~p"
	"~n   RevOidSoFar: ~p"
	"~n   MibView:     ~p", [size(Tree), Oid, RevOidSoFar, MibView]),
    OidSoFar = lists:reverse(RevOidSoFar),
    case snmpa_acm:is_definitely_not_in_mib_view(OidSoFar, MibView) of
	true -> 
	    ?vdebug("next_node(tree,table_entry) -> not in mib view",[]),
	    false;
	_ -> 
	    #mib_data{node_db = Db} = D,
	    case snmpa_general_db:read(Db, OidSoFar) of
		false ->
		    ?vinfo("next_node -> could not find table_entry with"
			"~n   OidSoFar: ~p", [OidSoFar]),
		    false;
		{value, #node_info{me = ME}} ->
		    ?vtrace("next_node(tree,table_entry) -> found: ~n   ~p",
			[ME]),
		    {table, OidSoFar, Oid, ME}
	    end
    end;

next_node(D, {tree, Tree, _Info}, [Int | RestOfOid], RevOidSoFar, MibView) 
  when (Int < size(Tree)) andalso (Int >= 0) ->
    ?vtrace("next_node(tree) -> entry when"
	"~n   size(Tree):  ~p"
	"~n   Int:         ~p"
	"~n   RestOfOid:   ~p"
	"~n   RevOidSoFar: ~p"
	"~n   MibView:     ~p", 
	[size(Tree), Int, RestOfOid, RevOidSoFar, MibView]),
    case next_node(D, element(Int+1,Tree), 
		   RestOfOid, [Int|RevOidSoFar], MibView) of
	false -> 
	    find_next(D, {tree, Tree, _Info}, Int+1, RevOidSoFar, MibView);
	Else -> 
	    Else
    end;
%% no solution
next_node(D, {tree, Tree, _Info}, [], RevOidSoFar, MibView) ->
    ?vtrace("next_node(tree,[]) -> entry when"
	"~n   size(Tree):  ~p"
	"~n   RevOidSoFar: ~p"
	"~n   MibView:     ~p", 
	[size(Tree), RevOidSoFar, MibView]),
    find_next(D, {tree, Tree, _Info}, 0, RevOidSoFar, MibView);
next_node(_D, {tree, Tree, _Info}, _RestOfOid, _RevOidSoFar, _MibView) ->
    ?vtrace("next_node(tree) -> entry when"
	"~n   size(Tree):  ~p", [size(Tree)]),
    false;

next_node(D, {node, subagent}, Oid, RevOidSoFar, MibView) ->
    ?vtrace("next_node(node,subagent) -> entry when"
	"~n   Oid:         ~p"
	"~n   RevOidSoFar: ~p"
	"~n   MibView:     ~p", 
	[Oid, RevOidSoFar, MibView]),
    OidSoFar = lists:reverse(RevOidSoFar),
    case snmpa_acm:is_definitely_not_in_mib_view(OidSoFar, MibView) of
	true -> 
	    false;
	_ -> 
	    #mib_data{subagents = SAs} = D,
	    case lists:keysearch(OidSoFar, 2, SAs) of
		{value, {SubAgentPid, OidSoFar}} ->
		    {subagent, SubAgentPid, OidSoFar};
		_ ->
		    ?vinfo("next_node -> could not find subagent with"
			"~n   OidSoFar: ~p"
			"~n   SAs:      ~p", [OidSoFar, SAs]),
		    false
	    end
    end;
    
next_node(D, {node, {variable, _MibName}}, [], RevOidSoFar, MibView) ->
    ?vtrace("next_node(node,variable,[]) -> entry when"
	"~n   RevOidSoFar: ~p"
	"~n   MibView:     ~p", 
	[RevOidSoFar, MibView]),
    OidSoFar = lists:reverse([0 | RevOidSoFar]),
    case snmpa_acm:validate_mib_view(OidSoFar, MibView) of
	true ->
	    #mib_data{node_db = Db} = D,
	    case snmpa_general_db:read(Db, lists:reverse(RevOidSoFar)) of
		false ->
		    ?vinfo("next_node -> could not find variable with"
			"~n   RevOidSoFar: ~p", [RevOidSoFar]),
		    false;
		{value, #node_info{me = ME}} ->
		    {variable, ME, OidSoFar}
	    end;
	_ -> 
	    false
    end;

next_node(_D, {node, {variable, _MibName}}, _Oid, _RevOidSoFar, _MibView) ->
    ?vtrace("next_node(node,variable) -> entry", []),
    false.

%%-----------------------------------------------------------------
%% This function is used to find the first leaf from where we
%% are.
%% Returns: {subagent, SubAgentPid, SAOid} |
%%          false |
%%          {variable, MibEntry, VarOid} |
%%          {table, TableOid, TableRestOid, MibEntry}
%% PRE: This function must always be called with a {internal, Tree}
%%      node.
%%-----------------------------------------------------------------
find_next(D, {tree, Tree, internal}, Idx, RevOidSoFar, MibView) 
  when Idx < size(Tree) ->
    case find_next(D, element(Idx+1, Tree), 0, [Idx| RevOidSoFar], MibView) of
	false -> 
	    find_next(D, {tree, Tree, internal}, Idx+1, RevOidSoFar, MibView);
	Other -> 
	    Other
    end;
find_next(_D, {tree, _Tree, internal}, _Idx, _RevOidSoFar, _MibView) ->
    false;
find_next(_D, undefined_node, _Idx, _RevOidSoFar, _MibView) ->
    false;
find_next(D, {tree, Tree, {table, _MibName}}, Idx, RevOidSoFar, MibView) ->
    find_next(D, {tree, Tree, internal}, Idx, RevOidSoFar, MibView);
find_next(D, {tree, _Tree, {table_entry, _MibName}}, _Index,
	  RevOidSoFar, MibView) ->
    OidSoFar = lists:reverse(RevOidSoFar),
    case snmpa_acm:is_definitely_not_in_mib_view(OidSoFar, MibView) of
	true -> 
	    false;
	_ -> 
	    #mib_data{node_db = Db} = D,
	    case snmpa_general_db:read(Db, OidSoFar) of
		false ->
		    ?vinfo("find_next -> could not find table_entry ME with"
			"~n   OidSoFar: ~p", [OidSoFar]),
		    false;
		{value, #node_info{me = ME}} ->
		    {table, OidSoFar, [], ME}
	    end
    end;
find_next(D, {node, {variable, _MibName}}, _Idx, RevOidSoFar, MibView) ->
    OidSoFar = lists:reverse([0 | RevOidSoFar]),
    case snmpa_acm:validate_mib_view(OidSoFar, MibView) of
	true -> 
	    #mib_data{node_db = Db} = D,
	    case snmpa_general_db:read(Db, lists:reverse(RevOidSoFar)) of
		false ->
		    ?vinfo("find_next -> could not find variable with"
			"~n   RevOidSoFar: ~p", [RevOidSoFar]),
		    false;
		{value, #node_info{me = ME}} ->
		    {variable, ME, OidSoFar}
	    end;
	_ -> 
	    false
    end;
find_next(D, {node, subagent}, _Idx, RevOidSoFar, MibView) ->
    OidSoFar = lists:reverse(RevOidSoFar),
    case snmpa_acm:is_definitely_not_in_mib_view(OidSoFar, MibView) of
	true -> 
	    false;
	_ -> 
	    #mib_data{subagents = SAs} = D,
	    case lists:keysearch(OidSoFar, 2, SAs) of
		{value, {SubAgentPid, OidSoFar}} ->
		    {subagent, SubAgentPid, OidSoFar};
		false ->
		    ?vinfo("find_node -> could not find subagent with"
			"~n   OidSoFar: ~p"
			"~n   SAs:      ~p", [OidSoFar, SAs]),
		    false
	    end
    end.

%%%======================================================================
%%% 3. Tree building functions
%%%    Used when loading mibs.
%%%======================================================================

build_tree(Mes, MibName) ->
    ?d("build_tree -> "
	"~n   Mes: ~p", [Mes]),
    {ListTree, []}  = build_subtree([], Mes, MibName),
    {tree, convert_tree(ListTree), internal}.

%%----------------------------------------------------------------------
%% Purpose: Builds the tree where all oids have prefix equal to LevelPrefix.
%% Returns: {Tree, RestMes}
%% RestMes are Mes that should not be in this subtree.
%% The Tree is a temporary and simplified data structure that is easy to
%% convert to the final tuple tree used by the MIB process.
%% A Node is represented as in the final tree.
%% The tree is not represented as a N-tuple, but as an Index-list.
%% Example: Temporary: [{1, Node1}, {3, Node3}]
%%          Final:     {Node1, undefined_node, Node3}
%% Pre: Mes are sorted on oid.
%%----------------------------------------------------------------------
build_subtree(LevelPrefix, [Me | Mes], MibName) ->
    ?vtrace("build subtree -> ~n"
	   "   oid:         ~p~n"
	   "   LevelPrefix: ~p~n"
	   "   MibName:     ~p", [Me#me.oid, LevelPrefix, MibName]),
    EType = Me#me.entrytype,
    ?vtrace("build subtree -> EType = ~p",[EType]),
    case in_subtree(LevelPrefix, Me) of
	above ->
	    ?vtrace("build subtree -> above",[]),
	    {[], [Me|Mes]};
	{node, Index} ->
	    ?vtrace("build subtree -> node at ~p",[Index]),
	    {Tree, RestMes} = build_subtree(LevelPrefix, Mes, MibName),
	    {[{Index, {node, {EType, MibName}}} | Tree], RestMes};
	{subtree, Index, NewLevelPrefix} ->
	    ?vtrace("build subtree -> subtree at"
		    "~n   ~w with ~w",
		   [Index, NewLevelPrefix]),
	    {BelowTree, RestMes} = 
		build_subtree(NewLevelPrefix, Mes, MibName),
	    {CurTree, RestMes2} = 
		build_subtree(LevelPrefix, RestMes, MibName),
	    {[{Index, {tree, BelowTree, {EType,MibName}}}| CurTree], RestMes2};
	{internal_subtree, Index, NewLevelPrefix} ->
	    ?vtrace("build subtree -> internal_subtree at"
		    "~n   ~w with ~w",
		   [Index,NewLevelPrefix]),
	    {BelowTree, RestMes} =
		build_subtree(NewLevelPrefix, [Me | Mes], MibName),
	    {CurTree, RestMes2} =
		build_subtree(LevelPrefix, RestMes, MibName),
	    {[{Index, {tree, BelowTree, internal}} | CurTree], RestMes2}
    end;

build_subtree(_LevelPrefix, [], _MibName) -> 
    ?vtrace("build subtree -> done", []),
    {[], []}.

%%--------------------------------------------------
%% Purpose: Determine how/if/where Me should be inserted in subtree
%%          with LevelPrefix. This function does not build any tree, only 
%%          determinses what should be done (by build subtree).
%% Returns:
%% above - Indicating that this ME should _not_ be in this subtree.
%% {node, Index} - yes, construct a node with index Index on this level
%% {internal_subtree, Index, NewLevelPrefix} - yes, there should be an
%%   internal subtree at this index.
%% {subtree, Index, NewLevelPrefix} - yes, construct a subtree with 
%%   NewLevelPrefix and insert this on current level in position Index.
%%--------------------------------------------------
in_subtree(LevelPrefix, Me) ->
    case lists:prefix(LevelPrefix, Me#me.oid) of
	true when length(Me#me.oid) > length(LevelPrefix) ->
	    classify_how_in_subtree(LevelPrefix, Me);
	_ ->
	    above
    end.

%%--------------------------------------------------
%% See comment about in_subtree/2.  This function takes care of all cases
%% where the ME really should be in _this_ subtree (not above).
%%--------------------------------------------------
classify_how_in_subtree(LevelPrefix, Me) 
  when (length(Me#me.oid) =:= (length(LevelPrefix) + 1)) ->
    Oid = Me#me.oid,
    case node_or_subtree(Me#me.entrytype) of
	subtree ->
	    {subtree, lists:last(Oid), Oid};
	node ->
	    {node, lists:last(Oid)}
    end;

classify_how_in_subtree(LevelPrefix, Me) 
  when (length(Me#me.oid) > (length(LevelPrefix) + 1)) ->
    L1 = length(LevelPrefix) + 1,
    Oid = Me#me.oid,
    {internal_subtree, lists:nth(L1, Oid), lists:sublist(Oid, 1, L1)}.

%%--------------------------------------------------
%% Determines how to treat different kinds om MEs in the tree building process.
%% Pre: all internal nodes have been removed.
%%--------------------------------------------------
node_or_subtree(table) -> subtree;
node_or_subtree(table_entry) -> subtree;
node_or_subtree(variable) -> node;
node_or_subtree(table_column) -> node.

%%--------------------------------------------------
%% Purpose: (Recursively) Converts a temporary tree (see above) to a final tree.
%% If input is a ListTree, output is a TupleTree.
%% If input is a Node, output is the same Node.
%% Pre: All Indexes are >= 0.
%%--------------------------------------------------
convert_tree({Index, {tree, Tree, Info}}) when Index >= 0 ->
    L = lists:map(fun convert_tree/1, Tree),
    {Index, {tree, dict_list_to_tuple(L), Info}};
convert_tree({Index, {node, Info}}) when Index >= 0 ->
    {Index, {node, Info}};
convert_tree(Tree) when is_list(Tree) ->
    L = lists:map(fun convert_tree/1, Tree),
    dict_list_to_tuple(L).

%%----------------------------------------------------------------------
%% Purpose: Converts a single level (that is non-recursively) from
%%          the temporary indexlist to the N-tuple.
%% Input: A list of {Index, Data}.
%% Output: A tuple where element Index is Data.
%%----------------------------------------------------------------------
dict_list_to_tuple(L) ->
    L2 = lists:keysort(1, L),
    list_to_tuple(integrate_indexes(0, L2)).

%%----------------------------------------------------------------------
%% Purpose: Helper function for dict_list_to_tuple/1.
%%          Converts an indexlist to a N-list.
%% Input: A list of {Index, Data}.
%% Output: A (usually longer, never shorter) list where element Index is Data.
%% Example: [{1,hej}, {3, sven}] will give output 
%% [undefined_node, hej, undefined_node, sven].
%% Initially CurIndex should be 0.
%%----------------------------------------------------------------------
integrate_indexes(CurIndex, [{CurIndex, Data} | T]) ->
    [Data | integrate_indexes(CurIndex + 1, T)];
integrate_indexes(_Index, []) ->
    [];
integrate_indexes(CurIndex, L) ->
    [undefined_node | integrate_indexes(CurIndex + 1, L)].

%%%======================================================================
%%% 4. Tree merging
%%%    Used by: load mib, insert subagent.
%%%======================================================================

%%----------------------------------------------------------------------
%% Arg: Two root nodes (that is to be merged).
%% Returns: A new root node where the nodes have been merger to one.
%%----------------------------------------------------------------------
merge_nodes(Same, Same) -> 
    Same;
merge_nodes(Node, undefined_node) -> 
    Node;
merge_nodes(undefined_node, Node) -> 
    Node;
merge_nodes({tree, Tree1, internal}, {tree, Tree2, internal}) ->
    {tree, merge_levels(tuple_to_list(Tree1),tuple_to_list(Tree2)), internal};
merge_nodes(Node1, Node2) ->
    throw({error_merge_nodes, Node1, Node2}).

%%----------------------------------------------------------------------
%% Arg: Two levels to be merged.
%%      Here, a level is represented as a list of nodes. A list is easier
%%      to extend than a tuple.
%% Returns: The resulting, merged level tuple.
%%----------------------------------------------------------------------
merge_levels(Level1, Level2) when length(Level1) =:= length(Level2) ->
    MergeNodes = fun(N1, N2) -> merge_nodes(N1, N2) end,
    list_to_tuple(snmp_misc:multi_map(MergeNodes, [Level1, Level2]));
merge_levels(Level1, Level2) when length(Level1) > length(Level2) ->
    merge_levels(Level1, Level2 ++ 
		 undefined_nodes_list(length(Level1) - length(Level2)));
merge_levels(Level1, Level2) when length(Level1) < length(Level2) ->
    merge_levels(Level2, Level1).

undefined_nodes_list(N) -> lists:duplicate(N, undefined_node).


%%%======================================================================
%%% 5. Tree deletion routines
%%%    (for unload mib)
%%%======================================================================

%%----------------------------------------------------------------------
%% Purpose:  Actually kicks of the tree reconstruction.
%% Returns: {list of removed MEs, NewTree}
%%----------------------------------------------------------------------
delete_mib_from_tree(MibName, {tree, Tree, internal}) ->
    case delete_tree(Tree, MibName) of
	[] -> 
	    {tree, {undefined_node}, internal}; % reduce
	LevelList -> 
	    {tree, list_to_tuple(LevelList), internal}
    end.

%%----------------------------------------------------------------------
%% Purpose: Deletes all nodes associated to MibName from this level and
%%          all levels below.
%%          If the new level does not contain information (that is, no 
%%          other mibs use it) anymore the empty list is returned.
%% Returns: {MEs, The new level represented as a list}
%%----------------------------------------------------------------------
delete_tree(Tree, MibName) when is_tuple(Tree) ->
    NewLevel = delete_nodes(tuple_to_list(Tree), MibName, []),
    case lists:filter(fun drop_undefined_nodes/1,NewLevel) of
	[] -> [];
	_A_perhaps_shorted_list ->
	    NewLevel  % some other mib needs this level
    end.
    
%%----------------------------------------------------------------------
%% Purpose: Nodes belonging to MibName are removed from the tree.
%%          Recursively deletes sub trees to this node.
%% Returns: {MEs, NewNodesList}
%%----------------------------------------------------------------------
delete_nodes([], _MibName, AccNodes) ->
    lists:reverse(AccNodes);

delete_nodes([{node, {variable, MibName}}|T], MibName, AccNodes) ->
    delete_nodes(T, MibName, [undefined_node | AccNodes]);

delete_nodes([{node, {table_column, MibName}}|T], MibName, AccNodes) ->
    delete_nodes(T, MibName, [undefined_node | AccNodes]);

delete_nodes([{tree, _Tree, {table, MibName}}|T], MibName, AccNodes) ->
    delete_nodes(T, MibName, [undefined_node | AccNodes]);

delete_nodes([{tree, _Tree, {table_entry, MibName}}|T], MibName, AccNodes) ->
    delete_nodes(T, MibName, [undefined_node | AccNodes]);

delete_nodes([{tree, Tree, Info}|T], MibName, AccNodes) ->
    case delete_tree(Tree, MibName) of
	[] -> % tree completely deleted
	    delete_nodes(T, MibName, [undefined_node | AccNodes]);
	LevelList ->
	    delete_nodes(T, MibName, 
			 [{tree, list_to_tuple(LevelList), Info} | AccNodes])
    end;

delete_nodes([NodeToKeep|T], MibName, AccNodes) ->
    delete_nodes(T, MibName, [NodeToKeep | AccNodes]).

drop_undefined_nodes(undefined_node) -> false;
drop_undefined_nodes(_) -> true.


%%%======================================================================
%%% 6. Functions for subagent handling
%%%======================================================================

%%----------------------------------------------------------------------
%% Returns: A new Root|{error, reason}
%%----------------------------------------------------------------------
insert_subagent(Oid, OldRoot) ->
    ListTree = build_tree_for_subagent(Oid),
    case catch convert_tree(ListTree) of
	{'EXIT', _Reason} ->
	    {error, 'cannot construct tree from oid'};
	Level when is_tuple(Level) ->
	    T = {tree, Level, internal},
	    case catch merge_nodes(T, OldRoot) of
		{error_merge_nodes, _Node1, _Node2} ->
		    {error, oid_conflict};
		NewRoot when is_tuple(NewRoot) andalso 
			     (element(1, NewRoot) =:= tree) ->
		    NewRoot
	    end
    end.

build_tree_for_subagent([Index]) ->
    [{Index, {node, subagent}}];

build_tree_for_subagent([Index | T]) ->
    [{Index, {tree, build_tree_for_subagent(T), internal}}].

%%----------------------------------------------------------------------
%% Returns: A new tree where the subagent at Oid (2nd arg) has been deleted.
%%----------------------------------------------------------------------
delete_subagent({tree, Tree, Info}, [Index]) ->
    {node, subagent} = element(Index+1, Tree),
    {tree, setelement(Index+1, Tree, undefined_node), Info};
delete_subagent({tree, Tree, Info}, [Index | TI]) ->
    {tree, setelement(Index+1, Tree,
		      delete_subagent(element(Index+1, Tree), TI)), Info}.

%%%======================================================================
%%% 7. Misc functions
%%%======================================================================

%%----------------------------------------------------------------------
%% Installs the mibs found in the database when starting the agent.
%% Basically calls the instrumentation functions for all non-internal
%% mib-entries
%%----------------------------------------------------------------------
install_mibs(MibDb, NodeDb) ->
    MibNames = loaded(MibDb),
    ?vtrace("install_mibs -> found following mibs in database: ~n"
	"~p", [MibNames]),
    install_mibs2(NodeDb, MibNames).

install_mibs2(_, []) ->
    ok;
install_mibs2(NodeDb, [MibName|MibNames]) ->
    Pattern = #node_info{oid = '_', mib_name = MibName, me = '_'},
    Nodes = snmpa_general_db:match_object(NodeDb, Pattern),
    MEs = [ME || #node_info{me = ME} <- Nodes],
    ?vtrace("install_mibs2 -> installing ~p MEs for mib ~p", 
	[length(MEs),MibName]),
    NewF = fun(ME) -> call_instrumentation(ME, new) end,
    lists:foreach(NewF, MEs),
    install_mibs2(NodeDb, MibNames).
    
    
%%----------------------------------------------------------------------
%% Does all side effect stuff during load_mib.
%%----------------------------------------------------------------------
install_mib(Db, Symbolic, Mib, MibName, FileName, NonInternalMes) ->
    ?vdebug("install_mib -> entry with"
	    "~n   Symbolic: ~p"
	    "~n   MibName:  ~p"
	    "~n   FileName: ~p", [Symbolic, MibName, FileName]),
    Rec = #mib_info{name = MibName, symbolic = Symbolic, file_name = FileName},
    snmpa_general_db:write(Db, Rec),
    install_mib2(Symbolic, MibName, Mib),
    NewF = fun(ME) -> call_instrumentation(ME, new) end,
    lists:foreach(NewF, NonInternalMes).

install_mib2(true, MibName, Mib) ->
    #mib{table_infos    = TabInfos,
	 variable_infos = VarInfos,
	 mes            = MEs,
	 asn1_types     = ASN1Types,
	 traps          = Traps} = Mib,
    snmpa_symbolic_store:add_table_infos(MibName, TabInfos),
    snmpa_symbolic_store:add_variable_infos(MibName, VarInfos),
    snmpa_symbolic_store:add_aliasnames(MibName, MEs),
    snmpa_symbolic_store:add_types(MibName, ASN1Types),
    SetF = fun(Trap) -> 
		   snmpa_symbolic_store:set_notification(Trap, MibName)
	   end,
    lists:foreach(SetF, Traps);
install_mib2(_, _, _) ->
    ok.

install_mes(_Db, _MibName, []) ->
    ok;
install_mes(Db, MibName, [ME|MEs]) ->
    Node = #node_info{oid = ME#me.oid, mib_name = MibName, me = ME},
    snmpa_general_db:write(Db, Node),
    install_mes(Db, MibName, MEs).


%%----------------------------------------------------------------------
%% Does all side effect stuff during unload_mib.
%%----------------------------------------------------------------------
uninstall_mib(Db, Symbolic, MibName, MEs) ->
    ?vtrace("uninstall_mib -> entry with"
	"~n   Db:       ~p"
	"~n   Symbolic: ~p"
	"~n   MibName:  ~p", [Db, Symbolic, MibName]),
    Res = snmpa_general_db:delete(Db, MibName),
    ?vtrace("uninstall_mib -> (mib) db delete result: ~p", [Res]),
    uninstall_mib2(Symbolic, MibName),
    DelF = fun(ME) -> call_instrumentation(ME, delete) end,
    lists:foreach(DelF, MEs).

uninstall_mib2(true, MibName) ->
    snmpa_symbolic_store:delete_table_infos(MibName),
    snmpa_symbolic_store:delete_variable_infos(MibName),
    snmpa_symbolic_store:delete_aliasnames(MibName),
    snmpa_symbolic_store:delete_types(MibName),
    snmpa_symbolic_store:delete_notifications(MibName);
uninstall_mib2(_, _) ->
    ok.

uninstall_mes(Db, MibName) ->
    Pattern = #node_info{oid = '_', mib_name = MibName, me = '_'},
    snmpa_general_db:match_delete(Db, Pattern).


%%----------------------------------------------------------------------
%% Create a list of the names of all the loaded mibs
%%----------------------------------------------------------------------
loaded(Db) ->
    [N || #mib_info{name = N} <- snmpa_general_db:tab2list(Db)].
    

%%----------------------------------------------------------------------
%% Calls MFA-instrumentation with 'new' or 'delete' operation.
%%----------------------------------------------------------------------
call_instrumentation(#me{entrytype = variable, mfa={M,F,A}}, Operation) ->
    ?vtrace("call instrumentation with"
	    "~n   entrytype: variable"
	    "~n   MFA:       {~p,~p,~p}"
	    "~n   Operation: ~p",
	    [M,F,A,Operation]),
    catch apply(M, F, [Operation | A]);
call_instrumentation(#me{entrytype = table_entry, mfa={M,F,A}}, Operation) ->
    ?vtrace("call instrumentation with"
	    "~n   entrytype: table_entry"
	    "~n   MFA:       {~p,~p,~p}"
	    "~n   Operation: ~p",
	    [M,F,A,Operation]),
    catch apply(M, F, [Operation | A]);
call_instrumentation(_ShitME, _Operation) ->
    done.


maybe_drop_me(#me{entrytype = internal}) -> false;
maybe_drop_me(#me{entrytype = group}) -> false;
maybe_drop_me(#me{imported = true}) -> false;
maybe_drop_me(_) -> true.


%%----------------------------------------------------------------------
%% Code change functions
%%----------------------------------------------------------------------

code_change(down, State) ->
    ?d("code_change(down) -> entry",[]),
    State;

code_change(up, State) ->
    ?d("code_change(up)",[]),
    State;

code_change(_Vsn, State) ->
    State.

