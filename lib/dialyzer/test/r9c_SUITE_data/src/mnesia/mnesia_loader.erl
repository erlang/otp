%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: mnesia_loader.erl,v 1.2 2010/03/04 13:54:19 maria Exp $
%%% Purpose : Loads tables from local disc or from remote node

-module(mnesia_loader).

%% Mnesia internal stuff
-export([disc_load_table/2,
	 net_load_table/4,
	 send_table/3]).

-export([old_node_init_table/6]). %% Spawned old node protocol conversion hack
-export([spawned_receiver/8]).    %% Spawned lock taking process

-import(mnesia_lib, [set/2, fatal/2, verbose/2, dbg_out/2]).

-include("mnesia.hrl").

val(Var) ->
    case ?catch_val(Var) of
	{'EXIT', Reason} -> mnesia_lib:other_val(Var, Reason);
	Value -> Value
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Load a table from local disc

disc_load_table(Tab, Reason) ->
    Storage =  val({Tab, storage_type}),
    Type = val({Tab, setorbag}),
    dbg_out("Getting table ~p (~p) from disc: ~p~n",
	    [Tab, Storage, Reason]),
    ?eval_debug_fun({?MODULE, do_get_disc_copy},
		    [{tab, Tab},
		     {reason, Reason},
		     {storage, Storage},
		     {type, Type}]),
    do_get_disc_copy2(Tab, Reason, Storage, Type).

do_get_disc_copy2(Tab, _Reason, Storage, _Type) when Storage == unknown ->
    verbose("Local table copy of ~p has recently been deleted, ignored.~n",
	    [Tab]),
    {loaded, ok};  %% ?
do_get_disc_copy2(Tab, Reason, Storage, Type) when Storage == disc_copies ->
    %% NOW we create the actual table
    Repair = mnesia_monitor:get_env(auto_repair),
    Args = [{keypos, 2}, public, named_table, Type],
    case Reason of
	{dumper, _} -> %% Resources already allocated
	    ignore;
	_ ->
	    mnesia_monitor:mktab(Tab, Args),
	    Count = mnesia_log:dcd2ets(Tab, Repair),
	    case ets:info(Tab, size) of
		X when X < Count * 4 ->
		    ok = mnesia_log:ets2dcd(Tab);
		_ ->
		    ignore
	    end
    end,
    mnesia_index:init_index(Tab, Storage),
    snmpify(Tab, Storage),
    set({Tab, load_node}, node()),
    set({Tab, load_reason}, Reason),
    {loaded, ok};

do_get_disc_copy2(Tab, Reason, Storage, Type) when Storage == ram_copies ->
    Args = [{keypos, 2}, public, named_table, Type],
    case Reason of
	{dumper, _} -> %% Resources already allocated
	    ignore;
	_ ->
	    mnesia_monitor:mktab(Tab, Args),
	    Fname = mnesia_lib:tab2dcd(Tab),
	    Datname = mnesia_lib:tab2dat(Tab),
	    Repair = mnesia_monitor:get_env(auto_repair),
	    case mnesia_monitor:use_dir() of
		true ->
		    case mnesia_lib:exists(Fname) of
			true -> mnesia_log:dcd2ets(Tab, Repair);
			false ->
			    case mnesia_lib:exists(Datname) of
				true ->
				    mnesia_lib:dets_to_ets(Tab, Tab, Datname,
							   Type, Repair, no);
				false ->
				    false
			    end
		    end;
		false ->
		    false
	    end
    end,
    mnesia_index:init_index(Tab, Storage),
    snmpify(Tab, Storage),
    set({Tab, load_node}, node()),
    set({Tab, load_reason}, Reason),
    {loaded, ok};

do_get_disc_copy2(Tab, Reason, Storage, Type) when Storage == disc_only_copies ->
    Args = [{file, mnesia_lib:tab2dat(Tab)},
	    {type, mnesia_lib:disk_type(Tab, Type)},
	    {keypos, 2},
	    {repair, mnesia_monitor:get_env(auto_repair)}],
    case Reason of
	{dumper, _} ->
	    mnesia_index:init_index(Tab, Storage),
	    snmpify(Tab, Storage),
	    set({Tab, load_node}, node()),
	    set({Tab, load_reason}, Reason),
	    {loaded, ok};
	_ ->
	    case mnesia_monitor:open_dets(Tab, Args) of
		{ok, _} ->
		    mnesia_index:init_index(Tab, Storage),
		    snmpify(Tab, Storage),
		    set({Tab, load_node}, node()),
		    set({Tab, load_reason}, Reason),
		    {loaded, ok};
		{error, Error} ->
		    {not_loaded, {"Failed to create dets table", Error}}
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Load a table from a remote node
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Receiver                             Sender
%% --------                             ------
%% Grab schema lock on table
%%                                      Determine table size
%% Create empty pre-grown table
%%                                      Grab read lock on table
%%                                      Let receiver subscribe on updates done on sender node
%%                                      Disable rehashing of table
%%                                      Release read lock on table
%%                                      Send table to receiver in chunks
%%
%%                                      Grab read lock on table
%% Block dirty updates
%%                                      Update wherabouts
%%
%%                                      Cancel the update subscription
%% Process the subscription events
%% Optionally dump to disc
%% Unblock dirty updates
%%                                      Release read lock on table
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(MAX_TRANSFER_SIZE, 7500).
-define(MAX_RAM_FILE_SIZE, 1000000).
-define(MAX_RAM_TRANSFERS, (?MAX_RAM_FILE_SIZE div ?MAX_TRANSFER_SIZE) + 1).
-define(MAX_NOPACKETS, 20).

net_load_table(Tab, Reason, Ns, Cs)
        when Reason == {dumper,add_table_copy} ->
    try_net_load_table(Tab, Reason, Ns, Cs);
net_load_table(Tab, Reason, Ns, _Cs) ->
    try_net_load_table(Tab, Reason, Ns, val({Tab, cstruct})).

try_net_load_table(Tab, _Reason, [], _Cs) ->
    verbose("Copy failed. No active replicas of ~p are available.~n", [Tab]),
    {not_loaded, none_active};
try_net_load_table(Tab, Reason, Ns, Cs) ->
    Storage = mnesia_lib:cs_to_storage_type(node(), Cs),
    do_get_network_copy(Tab, Reason, Ns, Storage, Cs).

do_get_network_copy(Tab, _Reason, _Ns, unknown, _Cs) ->
    verbose("Local table copy of ~p has recently been deleted, ignored.~n", [Tab]),
    {not_loaded, storage_unknown};
do_get_network_copy(Tab, Reason, Ns, Storage, Cs) ->
    [Node | Tail] = Ns,
    dbg_out("Getting table ~p (~p) from node ~p: ~p~n",
	    [Tab, Storage, Node, Reason]),
    ?eval_debug_fun({?MODULE, do_get_network_copy},
		    [{tab, Tab}, {reason, Reason},
		     {nodes, Ns}, {storage, Storage}]),
    mnesia_controller:start_remote_sender(Node, Tab, self(), Storage),
    put(mnesia_table_sender_node, {Tab, Node}),
    case init_receiver(Node, Tab, Storage, Cs, Reason) of
	ok ->
	    set({Tab, load_node}, Node),
	    set({Tab, load_reason}, Reason),
	    mnesia_controller:i_have_tab(Tab),
	    dbg_out("Table ~p copied from ~p to ~p~n", [Tab, Node, node()]),
	    {loaded, ok};
	Err = {error, _} when element(1, Reason) == dumper ->
	    {not_loaded,Err};
	restart ->
	    try_net_load_table(Tab, Reason, Tail, Cs);
	down ->
	    try_net_load_table(Tab, Reason, Tail, Cs)
    end.

snmpify(Tab, Storage) ->
    do_snmpify(Tab, val({Tab, snmp}), Storage).

do_snmpify(_Tab, [], _Storage) ->
    ignore;
do_snmpify(Tab, Us, Storage) ->
    Snmp = mnesia_snmp_hook:create_table(Us, Tab, Storage),
    set({Tab, {index, snmp}}, Snmp).

%% Start the recieiver
%% Sender should be started first, so we don't have the schema-read
%% lock to long (or get stuck in a deadlock)
init_receiver(Node, Tab, Storage, Cs, Reason) ->
    receive
	{SenderPid, {first, TabSize}} ->
	    spawn_receiver(Tab,Storage,Cs,SenderPid,
			   TabSize,false,Reason);
	{SenderPid, {first, TabSize, DetsData}} ->
	    spawn_receiver(Tab,Storage,Cs,SenderPid,
			   TabSize,DetsData,Reason);
	%% Protocol conversion hack
	{copier_done, Node} ->
	    dbg_out("Sender of table ~p crashed on node ~p ~n", [Tab, Node]),
	    down(Tab, Storage)
    end.


table_init_fun(SenderPid) ->
    PConv = mnesia_monitor:needs_protocol_conversion(node(SenderPid)),
    MeMyselfAndI = self(),
    fun(read) ->
	    Receiver =
		if
		    PConv == true ->
			MeMyselfAndI ! {actual_tabrec, self()},
			MeMyselfAndI; %% Old mnesia
		    PConv == false -> self()
		end,
	    SenderPid ! {Receiver, more},
	    get_data(SenderPid, Receiver)
    end.


%% Add_table_copy get's it's own locks.
spawn_receiver(Tab,Storage,Cs,SenderPid,TabSize,DetsData,{dumper,add_table_copy}) ->
    Init = table_init_fun(SenderPid),
    case do_init_table(Tab,Storage,Cs,SenderPid,TabSize,DetsData,self(), Init) of
	Err = {error, _} ->
	    SenderPid ! {copier_done, node()},
	    Err;
	Else ->
	    Else
    end;

spawn_receiver(Tab,Storage,Cs,SenderPid,
	       TabSize,DetsData,Reason) ->
    %% Grab a schema lock to avoid deadlock between table_loader and schema_commit dumping.
    %% Both may grab tables-locks in different order.
    Load = fun() ->
		   {_,Tid,Ts} = get(mnesia_activity_state),
		   mnesia_locker:rlock(Tid, Ts#tidstore.store,
				       {schema, Tab}),
		   Init = table_init_fun(SenderPid),
		   Pid = spawn_link(?MODULE, spawned_receiver,
				    [self(),Tab,Storage,Cs,
				     SenderPid,TabSize,DetsData,
				     Init]),
		   put(mnesia_real_loader, Pid),
		   wait_on_load_complete(Pid)
	   end,
    Res = case mnesia:transaction(Load, 20) of
	      {'atomic', {error,Result}} when element(1,Reason) == dumper ->
		  SenderPid ! {copier_done, node()},
		  {error,Result};
	      {'atomic', {error,Result}} ->
		  SenderPid ! {copier_done, node()},
		  fatal("Cannot create table ~p: ~p~n",
			[[Tab, Storage], Result]);
	      {'atomic', Result} -> Result;
	      {aborted, nomore} ->
		  SenderPid ! {copier_done, node()},
		  restart;
	      {aborted, _ } ->
		  SenderPid ! {copier_done, node()},
		  down  %% either this node or sender is dying
	  end,
    unlink(whereis(mnesia_tm)),  %% Avoid late unlink from tm
    Res.

spawned_receiver(ReplyTo,Tab,Storage,Cs,
		 SenderPid,TabSize,DetsData, Init) ->
    process_flag(trap_exit, true),
    Done = do_init_table(Tab,Storage,Cs,
			 SenderPid,TabSize,DetsData,
			 ReplyTo, Init),
    ReplyTo ! {self(),Done},
    unlink(ReplyTo),
    unlink(whereis(mnesia_controller)),
    exit(normal).

wait_on_load_complete(Pid) ->
    receive
	{Pid, Res} ->
	    Res;
	{'EXIT', Pid, Reason} ->
	    exit(Reason);
	Else ->
	    Pid ! Else,
	    wait_on_load_complete(Pid)
    end.

tab_receiver(Node, Tab, Storage, Cs, PConv, OrigTabRec) ->
    receive
	{SenderPid, {no_more, DatBin}} when PConv == false ->
	    finish_copy(Storage,Tab,Cs,SenderPid,DatBin,OrigTabRec);

	%% Protocol conversion hack
	{SenderPid, {no_more, DatBin}} when pid(PConv) ->
	    PConv ! {SenderPid, no_more},
	    receive
		{old_init_table_complete, ok} ->
		    finish_copy(Storage, Tab, Cs, SenderPid, DatBin,OrigTabRec);
		{old_init_table_complete, Reason} ->
		    Msg = "OLD: [d]ets:init table failed",
		    dbg_out("~s: ~p: ~p~n", [Msg, Tab, Reason]),
		    down(Tab, Storage)
	    end;

	{actual_tabrec, Pid} ->
	    tab_receiver(Node, Tab, Storage, Cs, Pid,OrigTabRec);

	{SenderPid, {more, [Recs]}} when pid(PConv) ->
	    PConv ! {SenderPid, {more, Recs}}, %% Forward Msg to OldNodes
	    tab_receiver(Node, Tab, Storage, Cs, PConv,OrigTabRec);

	{'EXIT', PConv, Reason} ->  %% [d]ets:init process crashed
	    Msg = "Receiver crashed",
	    dbg_out("~s: ~p: ~p~n", [Msg, Tab, Reason]),
	    down(Tab, Storage);

	%% Protocol conversion hack
	{copier_done, Node} ->
	    dbg_out("Sender of table ~p crashed on node ~p ~n", [Tab, Node]),
	    down(Tab, Storage);

	{'EXIT', Pid, Reason} ->
	    handle_exit(Pid, Reason),
	    tab_receiver(Node, Tab, Storage, Cs, PConv,OrigTabRec)
    end.

create_table(Tab, TabSize, Storage, Cs) ->
    if
	Storage == disc_only_copies ->
	    mnesia_lib:lock_table(Tab),
	    Tmp = mnesia_lib:tab2tmp(Tab),
	    Size = lists:max([TabSize, 256]),
	    Args = [{file, Tmp},
		    {keypos, 2},
%%		    {ram_file, true},
		    {estimated_no_objects, Size},
		    {repair, mnesia_monitor:get_env(auto_repair)},
		    {type, mnesia_lib:disk_type(Tab, Cs#cstruct.type)}],
	    file:delete(Tmp),
	    case mnesia_lib:dets_sync_open(Tab, Args) of
		{ok, _} ->
		    mnesia_lib:unlock_table(Tab),
		    {Storage, Tab};
		Else ->
		    mnesia_lib:unlock_table(Tab),
		    Else
	    end;
	(Storage == ram_copies) or (Storage == disc_copies) ->
	    Args = [{keypos, 2}, public, named_table, Cs#cstruct.type],
	    case mnesia_monitor:unsafe_mktab(Tab, Args) of
		Tab ->
		    {Storage, Tab};
		Else ->
		    Else
	    end
    end.

do_init_table(Tab,Storage,Cs,SenderPid,
	      TabSize,DetsInfo,OrigTabRec,Init) ->
    case create_table(Tab, TabSize, Storage, Cs) of
	{Storage,Tab} ->
	    %% Debug info
	    Node = node(SenderPid),
	    put(mnesia_table_receiver, {Tab, Node, SenderPid}),
	    mnesia_tm:block_tab(Tab),
	    PConv = mnesia_monitor:needs_protocol_conversion(Node),

	    case init_table(Tab,Storage,Init,PConv,DetsInfo,SenderPid) of
		ok ->
		    tab_receiver(Node,Tab,Storage,Cs,PConv,OrigTabRec);
		Reason ->
		    Msg = "[d]ets:init table failed",
		    dbg_out("~s: ~p: ~p~n", [Msg, Tab, Reason]),
		    down(Tab, Storage)
	    end;
	Error ->
	    Error
    end.

make_table_fun(Pid, TabRec) ->
    fun(close) ->
	    ok;
       (read) ->
	    get_data(Pid, TabRec)
    end.

get_data(Pid, TabRec) ->
    receive
	{Pid, {more, Recs}} ->
	    Pid ! {TabRec, more},
	    {Recs, make_table_fun(Pid,TabRec)};
	{Pid, no_more} ->
	    end_of_input;
	{copier_done, Node} ->
	    case node(Pid) of
		Node ->
		    {copier_done, Node};
		_ ->
		    get_data(Pid, TabRec)
	    end;
	{'EXIT', Pid, Reason} ->
	    handle_exit(Pid, Reason),
	    get_data(Pid, TabRec)
    end.

init_table(Tab, disc_only_copies, Fun, false, DetsInfo,Sender) ->
    ErtsVer = erlang:system_info(version),
    case DetsInfo of
	{ErtsVer, DetsData}  ->
	    Res = (catch dets:is_compatible_bchunk_format(Tab, DetsData)),
	    case Res of
		{'EXIT',{undef,[{dets,_,_}|_]}} ->
		    Sender ! {self(), {old_protocol, Tab}},
		    dets:init_table(Tab, Fun);  %% Old dets version
		{'EXIT', What} ->
		    exit(What);
		false ->
		    Sender ! {self(), {old_protocol, Tab}},
		    dets:init_table(Tab, Fun);  %% Old dets version
		true ->
		    dets:init_table(Tab, Fun, [{format, bchunk}])
	    end;
	Old when Old /= false ->
	    Sender ! {self(), {old_protocol, Tab}},
	    dets:init_table(Tab, Fun);  %% Old dets version
	_ ->
	    dets:init_table(Tab, Fun)
    end;
init_table(Tab, _, Fun, false, _DetsInfo,_) ->
    case catch ets:init_table(Tab, Fun) of
	true ->
	    ok;
	{'EXIT', Else} -> Else
    end;
init_table(Tab, Storage, Fun, true, _DetsInfo, Sender) ->  %% Old Nodes
    spawn_link(?MODULE, old_node_init_table,
	       [Tab, Storage, Fun, self(), false, Sender]),
    ok.

old_node_init_table(Tab, Storage, Fun, TabReceiver, DetsInfo,Sender) ->
    Res = init_table(Tab, Storage, Fun, false, DetsInfo,Sender),
    TabReceiver ! {old_init_table_complete, Res},
    unlink(TabReceiver),
    ok.

finish_copy(Storage,Tab,Cs,SenderPid,DatBin,OrigTabRec) ->
    TabRef = {Storage, Tab},
    subscr_receiver(TabRef, Cs#cstruct.record_name),
    case handle_last(TabRef, Cs#cstruct.type, DatBin) of
	ok ->
	    mnesia_index:init_index(Tab, Storage),
	    snmpify(Tab, Storage),
	    %% OrigTabRec must not be the spawned tab-receiver
	    %% due to old protocol.
	    SenderPid ! {OrigTabRec, no_more},
	    mnesia_tm:unblock_tab(Tab),
	    ok;
	{error, Reason} ->
	    Msg = "Failed to handle last",
	    dbg_out("~s: ~p: ~p~n", [Msg, Tab, Reason]),
	    down(Tab, Storage)
    end.

subscr_receiver(TabRef = {_, Tab}, RecName) ->
    receive
	{mnesia_table_event, {Op, Val, _Tid}} ->
	    if
		Tab == RecName ->
		    handle_event(TabRef, Op, Val);
		true ->
		    handle_event(TabRef, Op, setelement(1, Val, RecName))
	    end,
	    subscr_receiver(TabRef, RecName);

	{'EXIT', Pid, Reason} ->
	    handle_exit(Pid, Reason),
	    subscr_receiver(TabRef, RecName)
    after 0 ->
	    ok
    end.

handle_event(TabRef, write, Rec) ->
    db_put(TabRef, Rec);
handle_event(TabRef, delete, {_Tab, Key}) ->
    db_erase(TabRef, Key);
handle_event(TabRef, delete_object, OldRec) ->
    db_match_erase(TabRef, OldRec);
handle_event(TabRef, clear_table, {_Tab, _Key}) ->
    db_match_erase(TabRef, '_').

handle_last({disc_copies, Tab}, _Type, nobin) ->
    Ret = mnesia_log:ets2dcd(Tab),
    Fname = mnesia_lib:tab2dat(Tab),
    case mnesia_lib:exists(Fname) of
	true ->  %% Remove old .DAT files.
	    file:delete(Fname);
	false ->
	    ok
    end,
    Ret;

handle_last({disc_only_copies, Tab}, Type, nobin) ->
    case mnesia_lib:swap_tmp_files([Tab]) of
	[] ->
	    Args = [{file, mnesia_lib:tab2dat(Tab)},
		    {type, mnesia_lib:disk_type(Tab, Type)},
		    {keypos, 2},
		    {repair, mnesia_monitor:get_env(auto_repair)}],
	    mnesia_monitor:open_dets(Tab, Args),
	    ok;
	L when list(L) ->
	    {error, {"Cannot swap tmp files", Tab, L}}
    end;

handle_last({ram_copies, _Tab}, _Type, nobin) ->
    ok;
handle_last({ram_copies, Tab}, _Type, DatBin) ->
    case mnesia_monitor:use_dir() of
	true ->
	    mnesia_lib:lock_table(Tab),
	    Tmp = mnesia_lib:tab2tmp(Tab),
	    ok = file:write_file(Tmp, DatBin),
	    ok = file:rename(Tmp, mnesia_lib:tab2dcd(Tab)),
	    mnesia_lib:unlock_table(Tab),
	    ok;
	false ->
	    ok
    end.

down(Tab, Storage) ->
    case Storage of
	ram_copies ->
	    catch ?ets_delete_table(Tab);
	disc_copies ->
	    catch ?ets_delete_table(Tab);
	disc_only_copies ->
	    mnesia_lib:cleanup_tmp_files([Tab])
    end,
    mnesia_checkpoint:tm_del_copy(Tab, node()),
    mnesia_controller:sync_del_table_copy_whereabouts(Tab, node()),
    mnesia_tm:unblock_tab(Tab),
    flush_subcrs(),
    down.

flush_subcrs() ->
    receive
	{mnesia_table_event, _} ->
	    flush_subcrs();

	{'EXIT', Pid, Reason} ->
	    handle_exit(Pid, Reason),
	    flush_subcrs()
    after 0 ->
	    done
    end.

db_erase({ram_copies, Tab}, Key) ->
    true = ?ets_delete(Tab, Key);
db_erase({disc_copies, Tab}, Key) ->
    true = ?ets_delete(Tab, Key);
db_erase({disc_only_copies, Tab}, Key) ->
    ok = dets:delete(Tab, Key).

db_match_erase({ram_copies, Tab} , Pat) ->
    true = ?ets_match_delete(Tab, Pat);
db_match_erase({disc_copies, Tab} , Pat) ->
    true = ?ets_match_delete(Tab, Pat);
db_match_erase({disc_only_copies, Tab}, Pat) ->
    ok = dets:match_delete(Tab, Pat).

db_put({ram_copies, Tab}, Val) ->
    true = ?ets_insert(Tab, Val);
db_put({disc_copies, Tab}, Val) ->
    true = ?ets_insert(Tab, Val);
db_put({disc_only_copies, Tab}, Val) ->
    ok = dets:insert(Tab, Val).

%% This code executes at the remote site where the data is
%% executes in a special copier process.

calc_nokeys(Storage, Tab) ->
    %% Calculate #keys per transfer
    Key = mnesia_lib:db_first(Storage, Tab),
    Recs = mnesia_lib:db_get(Storage, Tab, Key),
    BinSize = size(term_to_binary(Recs)),
    (?MAX_TRANSFER_SIZE div BinSize) + 1.

send_table(Pid, Tab, RemoteS) ->
    case ?catch_val({Tab, storage_type}) of
	{'EXIT', _} ->
	    {error, {no_exists, Tab}};
	unknown ->
	    {error, {no_exists, Tab}};
	Storage ->
	    %% Send first
	    TabSize = mnesia:table_info(Tab, size),
	    Pconvert = mnesia_monitor:needs_protocol_conversion(node(Pid)),
	    KeysPerTransfer = calc_nokeys(Storage, Tab),
	    ChunkData = dets:info(Tab, bchunk_format),

	    UseDetsChunk =
		Storage == RemoteS andalso
		Storage == disc_only_copies andalso
		ChunkData /= undefined andalso
		Pconvert == false,
	    if
		UseDetsChunk == true ->
		    DetsInfo = erlang:system_info(version),
		    Pid ! {self(), {first, TabSize, {DetsInfo, ChunkData}}};
		true  ->
		    Pid ! {self(), {first, TabSize}}
	    end,

	    %% Debug info
	    put(mnesia_table_sender, {Tab, node(Pid), Pid}),
	    {Init, Chunk} = reader_funcs(UseDetsChunk, Tab, Storage, KeysPerTransfer),

	    SendIt = fun() ->
			     prepare_copy(Pid, Tab, Storage),
			     send_more(Pid, 1, Chunk, Init(), Tab, Pconvert),
			     finish_copy(Pid, Tab, Storage, RemoteS)
		     end,

	    case catch SendIt() of
		receiver_died ->
		    cleanup_tab_copier(Pid, Storage, Tab),
		    unlink(whereis(mnesia_tm)),
		    ok;
		{_, receiver_died} ->
		    unlink(whereis(mnesia_tm)),
		    ok;
		{'atomic', no_more} ->
		    unlink(whereis(mnesia_tm)),
		    ok;
		Reason ->
		    cleanup_tab_copier(Pid, Storage, Tab),
		    unlink(whereis(mnesia_tm)),
		    {error, Reason}
	    end
    end.

prepare_copy(Pid, Tab, Storage) ->
    Trans =
	fun() ->
		mnesia:write_lock_table(Tab),
		mnesia_subscr:subscribe(Pid, {table, Tab}),
		update_where_to_write(Tab, node(Pid)),
		mnesia_lib:db_fixtable(Storage, Tab, true),
		ok
	end,
    case mnesia:transaction(Trans) of
	{'atomic', ok} ->
	    ok;
	{aborted, Reason} ->
	    exit({tab_copier_prepare, Tab, Reason})
    end.

update_where_to_write(Tab, Node) ->
    case val({Tab, access_mode}) of
	read_only ->
	    ignore;
	read_write ->
	    Current = val({current, db_nodes}),
	    Ns =
		case lists:member(Node, Current) of
		    true -> Current;
		    false -> [Node | Current]
		end,
	    update_where_to_write(Ns, Tab, Node)
    end.

update_where_to_write([], _, _) ->
    ok;
update_where_to_write([H|T], Tab, AddNode) ->
    rpc:call(H,  mnesia_controller, call,
	     [{update_where_to_write, [add, Tab, AddNode], self()}]),
    update_where_to_write(T, Tab, AddNode).

send_more(Pid, N, Chunk, DataState, Tab, OldNode) ->
    receive
	{NewPid, more} ->
	    case send_packet(N - 1, NewPid, Chunk, DataState, OldNode) of
		New when integer(New) ->
		    New - 1;
		NewData ->
		    send_more(NewPid, ?MAX_NOPACKETS, Chunk, NewData, Tab, OldNode)
	    end;
	{_NewPid, {old_protocol, Tab}} ->
	    Storage =  val({Tab, storage_type}),
	    {Init, NewChunk} =
		reader_funcs(false, Tab, Storage, calc_nokeys(Storage, Tab)),
	    send_more(Pid, 1, NewChunk, Init(), Tab, OldNode);

	{copier_done, Node} when Node == node(Pid)->
	    verbose("Receiver of table ~p crashed on ~p (more)~n", [Tab, Node]),
	    throw(receiver_died)
    end.

reader_funcs(UseDetsChunk, Tab, Storage, KeysPerTransfer) ->
    case UseDetsChunk of
	false ->
	    {fun() -> mnesia_lib:db_init_chunk(Storage, Tab, KeysPerTransfer) end,
	     fun(Cont) -> mnesia_lib:db_chunk(Storage, Cont) end};
	true ->
	    {fun() -> dets_bchunk(Tab, start) end,
	     fun(Cont) -> dets_bchunk(Tab, Cont) end}
    end.

dets_bchunk(Tab, Chunk) -> %% Arrg
    case dets:bchunk(Tab, Chunk) of
	{Cont, Data} -> {Data, Cont};
	Else -> Else
    end.

send_packet(N, Pid, _Chunk, '$end_of_table', OldNode) ->
    case OldNode of
	true -> ignore; %% Old nodes can't handle the new no_more
	false ->  Pid ! {self(), no_more}
    end,
    N;
send_packet(N, Pid, Chunk, {[], Cont}, OldNode) ->
    send_packet(N, Pid, Chunk, Chunk(Cont), OldNode);
send_packet(N, Pid, Chunk, {Recs, Cont}, OldNode) when N < ?MAX_NOPACKETS ->
    case OldNode of
	true -> Pid ! {self(), {more, [Recs]}}; %% Old need's wrapping list
	false -> Pid ! {self(), {more, Recs}}
    end,
    send_packet(N+1, Pid, Chunk, Chunk(Cont), OldNode);
send_packet(_N, _Pid, _Chunk, DataState, _OldNode) ->
    DataState.

finish_copy(Pid, Tab, Storage, RemoteS) ->
    RecNode = node(Pid),
    DatBin = dat2bin(Tab, Storage, RemoteS),
    Trans =
	fun() ->
		mnesia:read_lock_table(Tab),
		A = val({Tab, access_mode}),
		mnesia_controller:sync_and_block_table_whereabouts(Tab, RecNode, RemoteS, A),
		cleanup_tab_copier(Pid, Storage, Tab),
		mnesia_checkpoint:tm_add_copy(Tab, RecNode),
		Pid ! {self(), {no_more, DatBin}},
		receive
		    {Pid, no_more} -> % Dont bother about the spurious 'more' message
			no_more;
		    {copier_done, Node} when Node == node(Pid)->
			verbose("Tab receiver ~p crashed (more): ~p~n", [Tab, Node]),
			receiver_died
		end
	end,
    mnesia:transaction(Trans).

cleanup_tab_copier(Pid, Storage, Tab) ->
    mnesia_lib:db_fixtable(Storage, Tab, false),
    mnesia_subscr:unsubscribe(Pid, {table, Tab}).

dat2bin(Tab, ram_copies, ram_copies) ->
    mnesia_lib:lock_table(Tab),
    Res = file:read_file(mnesia_lib:tab2dcd(Tab)),
    mnesia_lib:unlock_table(Tab),
    case Res of
	{ok, DatBin} -> DatBin;
	_ -> nobin
    end;
dat2bin(_Tab, _LocalS, _RemoteS) ->
    nobin.

handle_exit(Pid, Reason) when node(Pid) == node() ->
    exit(Reason);
handle_exit(_Pid, _Reason) ->  %% Not from our node, this will be handled by
    ignore.                  %% mnesia_down soon.
