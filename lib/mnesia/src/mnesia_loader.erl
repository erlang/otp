%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2018. All Rights Reserved.
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
%%% Purpose : Loads tables from local disc or from remote node

-module(mnesia_loader).

%% Mnesia internal stuff
-export([disc_load_table/2,
	 net_load_table/4,
	 send_table/3]).

-export([spawned_receiver/8]).    %% Spawned lock taking process

-import(mnesia_lib, [set/2, fatal/2, verbose/2, dbg_out/2]).

-include("mnesia.hrl").

%% Local function in order to avoid external function call
val(Var) ->
    case ?catch_val_and_stack(Var) of
	{'EXIT', Stacktrace} -> mnesia_lib:other_val(Var, Stacktrace);
	Value -> Value
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Load a table from local disc

disc_load_table(Tab, Reason) ->
    Storage =  val({Tab, storage_type}),
    Type = val({Tab, setorbag}),
    dbg_out("Getting table ~tp (~p) from disc: ~tp~n",
	    [Tab, Storage, Reason]),
    ?eval_debug_fun({?MODULE, do_get_disc_copy},
		    [{tab, Tab},
		     {reason, Reason},
		     {storage, Storage},
		     {type, Type}]),
    do_get_disc_copy2(Tab, Reason, Storage, Type).

do_get_disc_copy2(Tab, _Reason, Storage, _Type) when Storage == unknown ->
    verbose("Local table copy of ~tp has recently been deleted, ignored.~n",
	    [Tab]),
    {not_loaded, storage_unknown};
do_get_disc_copy2(Tab, Reason, Storage, Type) when Storage == disc_copies ->
    %% NOW we create the actual table
    Repair = mnesia_monitor:get_env(auto_repair),
    StorageProps = val({Tab, storage_properties}),
    EtsOpts = proplists:get_value(ets, StorageProps, []),
    Args = [{keypos, 2}, public, named_table, Type | EtsOpts],
    case Reason of
	{dumper, DR} when is_atom(DR) -> %% Resources already allocated
	    ignore;
	_ ->
	    mnesia_monitor:mktab(Tab, Args),
	    _Count = mnesia_log:dcd2ets(Tab, Repair),
	    case mnesia_monitor:get_env(dump_disc_copies_at_startup)
		andalso mnesia_dumper:needs_dump_ets(Tab) of
		true ->
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
    StorageProps = val({Tab, storage_properties}),
    EtsOpts = proplists:get_value(ets, StorageProps, []),
    Args = [{keypos, 2}, public, named_table, Type | EtsOpts],
    case Reason of
	{dumper, DR} when is_atom(DR) ->
            ignore; %% Resources already allocated
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
    StorageProps = val({Tab, storage_properties}),
    DetsOpts = proplists:get_value(dets, StorageProps, []),

    Args = [{file, mnesia_lib:tab2dat(Tab)},
	    {type, mnesia_lib:disk_type(Tab, Type)},
	    {keypos, 2},
	    {repair, mnesia_monitor:get_env(auto_repair)} 
	    | DetsOpts],
    case Reason of
	{dumper, DR} when is_atom(DR) ->
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
    end;

do_get_disc_copy2(Tab, Reason, Storage = {ext, Alias, Mod}, _Type) ->
    Cs = val({Tab, cstruct}),
    case mnesia_monitor:unsafe_create_external(Tab, Alias, Mod, Cs) of
	ok ->
	    ok = ext_load_table(Mod, Alias, Tab, Reason),
	    mnesia_index:init_index(Tab, Storage),
	    set({Tab, load_node}, node()),
	    set({Tab, load_reason}, Reason),
	    {loaded, ok};
	Other ->
	    {not_loaded, Other}
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

net_load_table(Tab, {dumper,{add_table_copy, _}}=Reason, Ns, Cs) ->
    try_net_load_table(Tab, Reason, Ns, Cs);
net_load_table(Tab, Reason, Ns, _Cs) ->
    try_net_load_table(Tab, Reason, Ns, val({Tab, cstruct})).

try_net_load_table(Tab, _Reason, [], _Cs) ->
    verbose("Copy failed. No active replicas of ~tp are available.~n", [Tab]),
    {not_loaded, none_active};
try_net_load_table(Tab, Reason, Ns, Cs) ->
    Storage = mnesia_lib:cs_to_storage_type(node(), Cs),
    do_get_network_copy(Tab, Reason, Ns, Storage, Cs).

do_get_network_copy(Tab, _Reason, _Ns, unknown, _Cs) ->
    verbose("Local table copy of ~tp has recently been deleted, ignored.~n", [Tab]),
    {not_loaded, storage_unknown};
do_get_network_copy(Tab, Reason, Ns, Storage, Cs) ->
    [Node | Tail] = Ns,
    case lists:member(Node,val({current, db_nodes})) of
	true ->
	    dbg_out("Getting table ~tp (~p) from node ~p: ~tp~n",
		    [Tab, Storage, Node, Reason]),
	    ?eval_debug_fun({?MODULE, do_get_network_copy},
			    [{tab, Tab}, {reason, Reason},
			     {nodes, Ns}, {storage, Storage}]),
	    case init_receiver(Node, Tab, Storage, Cs, Reason) of
		ok ->
		    set({Tab, load_node}, Node),
		    set({Tab, load_reason}, Reason),
		    mnesia_controller:i_have_tab(Tab),
		    dbg_out("Table ~tp copied from ~p to ~p~n", [Tab, Node, node()]),
		    {loaded, ok};
		Err = {error, _} when element(1, Reason) == dumper ->
		    {not_loaded,Err};
		restart ->
		    try_net_load_table(Tab, Reason, Tail ++ [Node], Cs);
		down ->
		    try_net_load_table(Tab, Reason, Tail, Cs)
	    end;
	false ->
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
init_receiver(Node, Tab, Storage, Cs, Reas={dumper,{add_table_copy, Tid}}) ->
    rpc:call(Node, mnesia_lib, set, [{?MODULE, active_trans}, Tid]),
    case start_remote_sender(Node, Tab, Storage) of
	{SenderPid, TabSize, DetsData} ->
	    start_receiver(Tab,Storage,Cs,SenderPid,TabSize,DetsData,Reas);
	Else ->
	    Else
    end;
init_receiver(Node, Tab,Storage,Cs,Reason) ->
    %% Grab a schema lock to avoid deadlock between table_loader and schema_commit dumping.
    %% Both may grab tables-locks in different order.
    Load =
	fun() ->
		{_,Tid,Ts} = get(mnesia_activity_state),
		mnesia_locker:rlock(Tid, Ts#tidstore.store, {schema, Tab}),
		%% Check that table still exists
		Active = val({Tab, active_replicas}),
		%% Check that we havn't loaded it already
		case val({Tab,where_to_read}) == node() of
		    true -> ok;
		    _ ->
			%% And that sender still got a copy
			%% (something might have happend while
			%% we where waiting for the lock)
			true = lists:member(Node, Active),
			{SenderPid, TabSize, DetsData} =
			    start_remote_sender(Node,Tab,Storage),
			Init = table_init_fun(SenderPid, Storage),
			Args = [self(),Tab,Storage,Cs,SenderPid,
				TabSize,DetsData,Init],
			Pid = spawn_link(?MODULE, spawned_receiver, Args),
			put(mnesia_real_loader, Pid),
			wait_on_load_complete(Pid)
		end
	end,
    Res =
	case mnesia:transaction(Load, 20) of
	    {atomic, {error,Result}} when
		  element(1,Reason) == dumper ->
		{error,Result};
	    {atomic, {error,Result}} ->
		fatal("Cannot create table ~tp: ~tp~n",
		      [[Tab, Storage], Result]);
	    {atomic,  Result} -> Result;
	    {aborted, nomore} -> restart;
	    {aborted, _Reas} ->
		verbose("Receiver failed on ~tp from ~p:~nReason: ~tp~n",
			[Tab,Node,_Reas]),
		down  %% either this node or sender is dying
	end,
    unlink(whereis(mnesia_tm)),  %% Avoid late unlink from tm
    Res.

start_remote_sender(Node,Tab,Storage) ->
    mnesia_controller:start_remote_sender(Node, Tab, self(), Storage),
    put(mnesia_table_sender_node, {Tab, Node}),
    receive
	{SenderPid, {first, _} = Msg}
	  when is_pid(SenderPid), element(1, Storage) == ext ->
	    {ext, Alias, Mod} = Storage,
	    {Sz, Data} = Mod:receiver_first_message(SenderPid, Msg, Alias, Tab),
	    {SenderPid, Sz, Data};
	{SenderPid, {first, TabSize}} =_M1 ->
	    {SenderPid, TabSize, false};
	{SenderPid, {first, TabSize, DetsData}} ->
	    {SenderPid, TabSize, DetsData};
	%% Protocol conversion hack
	{copier_done, Node} ->
	    verbose("Sender of table ~tp crashed on node ~p ~n", [Tab, Node]),
	    down(Tab, Storage)
    end.

table_init_fun(SenderPid, Storage) ->
    fun(read) ->
	    Receiver = self(),
	    SenderPid ! {Receiver, more},
	    get_data(SenderPid, Receiver, Storage);
       (close) ->
	    ok
    end.

%% Add_table_copy get's it's own locks.
start_receiver(Tab,Storage,Cs,SenderPid,TabSize,DetsData,{dumper,{add_table_copy,_}}) ->
    Init = table_init_fun(SenderPid, Storage),
    case do_init_table(Tab,Storage,Cs,SenderPid,TabSize,DetsData,self(), Init) of
	Err = {error, _} ->
	    SenderPid ! {copier_done, node()},
	    Err;
	Else ->
	    Else
    end.

spawned_receiver(ReplyTo,Tab,Storage,Cs, SenderPid,TabSize,DetsData, Init) ->
    process_flag(trap_exit, true),
    Done = do_init_table(Tab,Storage,Cs,
			 SenderPid,TabSize,DetsData,
			 ReplyTo, Init),
    try
        ReplyTo ! {self(),Done},
        unlink(ReplyTo),
        unlink(whereis(mnesia_controller))
    catch _:_ -> ok %% avoid error reports when stopping down mnesia
    end,
    exit(normal).

wait_on_load_complete(Pid) ->
    receive
	{Pid, Res} ->
	    Res;
	{'EXIT', Pid, Reason} ->
	    error(Reason);
	Else ->
	    Pid ! Else,
	    wait_on_load_complete(Pid)
    end.

do_init_table(Tab,Storage,Cs,SenderPid,
	      TabSize,DetsInfo,OrigTabRec,Init) ->
    case create_table(Tab, TabSize, Storage, Cs) of
	{Storage,Tab} ->
	    %% Debug info
	    Node = node(SenderPid),
	    put(mnesia_table_receiver, {Tab, Node, SenderPid}),
	    mnesia_tm:block_tab(Tab),
	    case init_table(Tab,Storage,Init,DetsInfo,SenderPid) of
		ok ->
		    tab_receiver(Node,Tab,Storage,Cs,OrigTabRec);
		Reason ->
		    Msg = "[d]ets:init table failed",
		    verbose("~ts: ~tp: ~tp~n", [Msg, Tab, Reason]),
		    down(Tab, Storage)
	    end;
	Error ->
	    Error
    end.

create_table(Tab, TabSize, Storage, Cs) ->
    StorageProps = val({Tab, storage_properties}),
    if
	Storage == disc_only_copies ->
	    mnesia_lib:lock_table(Tab),
	    Tmp = mnesia_lib:tab2tmp(Tab),
	    Size = lists:max([TabSize, 256]),
	    DetsOpts = lists:keydelete(estimated_no_objects, 1,
				       proplists:get_value(dets, StorageProps, [])),
	    Args = [{file, Tmp},
		    {keypos, 2},
%%		    {ram_file, true},
		    {estimated_no_objects, Size},
		    {repair, mnesia_monitor:get_env(auto_repair)},
		    {type, mnesia_lib:disk_type(Tab, Cs#cstruct.type)}
		    | DetsOpts],
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
	    EtsOpts = proplists:get_value(ets, StorageProps, []),
	    Args = [{keypos, 2}, public, named_table, Cs#cstruct.type | EtsOpts],
	    case mnesia_monitor:unsafe_mktab(Tab, Args) of
		Tab ->
		    {Storage, Tab};
		Else ->
		    Else
	    end;
        element(1, Storage) == ext ->
            {_, Alias, Mod} = Storage,
            case mnesia_monitor:unsafe_create_external(Tab, Alias, Mod, Cs) of
                ok ->
                    {Storage, Tab};
                Else ->
                    Else
            end
    end.

tab_receiver(Node, Tab, Storage, Cs, OrigTabRec) ->
    receive
	{SenderPid, {no_more, DatBin}} ->
	    finish_copy(Storage,Tab,Cs,SenderPid,DatBin,OrigTabRec);

	%% Protocol conversion hack
	{copier_done, Node} ->
	    verbose("Sender of table ~tp crashed on node ~p ~n", [Tab, Node]),
	    down(Tab, Storage);

	{'EXIT', Pid, Reason} ->
	    handle_exit(Pid, Reason),
	    tab_receiver(Node, Tab, Storage, Cs, OrigTabRec)
    end.

make_table_fun(Pid, TabRec, Storage) ->
    fun(close) ->
	    ok;
       ({read, Msg}) ->
	    Pid ! {TabRec, Msg},
	    get_data(Pid, TabRec, Storage);
       (read) ->
	    get_data(Pid, TabRec, Storage)
    end.

get_data(Pid, TabRec, Storage) ->
    receive
	{Pid, {more_z, CompressedRecs}} when is_binary(CompressedRecs) ->
	    maybe_reply(Pid, {TabRec, more}, Storage),
	    {zlib_uncompress(CompressedRecs),
	     make_table_fun(Pid, TabRec, Storage)};
	{Pid, {more, Recs}} ->
	    maybe_reply(Pid, {TabRec, more}, Storage),
	    {Recs, make_table_fun(Pid, TabRec, Storage)};
	{Pid, no_more} ->
	    end_of_input;
	{copier_done, Node} ->
	    case node(Pid) of
		Node ->
		    {copier_done, Node};
		_ ->
		    get_data(Pid, TabRec, Storage)
	    end;
	{'EXIT', Pid, Reason} ->
	    handle_exit(Pid, Reason),
	    get_data(Pid, TabRec, Storage)
    end.

maybe_reply(_, _, {ext, _, _}) ->
    ignore;
maybe_reply(Pid, Msg, _) ->
    Pid ! Msg.

ext_init_table(Alias, Mod, Tab, Fun, State, Sender) ->
    ok = ext_load_table(Mod, Alias, Tab, {net_load, node(Sender)}),
    ext_init_table(read, Alias, Mod, Tab, Fun, State, Sender).

ext_load_table(Mod, Alias, Tab, Reason) ->
    CS = val({Tab, cstruct}),
    Mod:load_table(Alias, Tab, Reason, mnesia_schema:cs2list(CS)).


ext_init_table(Action, Alias, Mod, Tab, Fun, State, Sender) ->
    case Fun(Action) of
	{copier_done, Node} ->
	    verbose("Receiver of table ~tp crashed on ~p (more)~n", [Tab, Node]),
	    down(Tab, {ext,Alias,Mod});
	{Data, NewFun} ->
	    case Mod:receive_data(Data, Alias, Tab, Sender, State) of
		{more, NewState} ->
		    ext_init_table({read, more}, Alias, Mod,
				   Tab, NewFun, NewState, Sender);
		{{more,Msg}, NewState} ->
		    ext_init_table({read, Msg}, Alias, Mod,
				   Tab, NewFun, NewState, Sender)
	    end;
	end_of_input ->
	    Mod:receive_done(Alias, Tab, Sender, State),
	    ok = Fun(close)
    end.

init_table(Tab, {ext,Alias,Mod}, Fun, State, Sender) ->
    ext_init_table(Alias, Mod, Tab, Fun, State, Sender);
init_table(Tab, disc_only_copies, Fun, DetsInfo,Sender) ->
    ErtsVer = erlang:system_info(version),
    case DetsInfo of
	{ErtsVer, DetsData}  ->
	    try dets:is_compatible_bchunk_format(Tab, DetsData) of
		false ->
		    Sender ! {self(), {old_protocol, Tab}},
		    dets:init_table(Tab, Fun);  %% Old dets version
		true ->
		    dets:init_table(Tab, Fun, [{format, bchunk}])
	    catch
		error:{undef,[{dets,_,_,_}|_]} ->
		    Sender ! {self(), {old_protocol, Tab}},
		    dets:init_table(Tab, Fun);  %% Old dets version
		error:What ->
		    What
	    end;
	Old when Old /= false ->
	    Sender ! {self(), {old_protocol, Tab}},
	    dets:init_table(Tab, Fun);  %% Old dets version
	_ ->
	    dets:init_table(Tab, Fun)
    end;
init_table(Tab, _, Fun, _DetsInfo,_) ->
    try
	true = ets:init_table(Tab, Fun),
	ok
    catch _:Else:Stacktrace -> {Else, Stacktrace}
    end.


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
	    verbose("~ts: ~tp: ~tp~n", [Msg, Tab, Reason]),
	    down(Tab, Storage)
    end.

subscr_receiver(TabRef = {_, Tab}, RecName) ->
    receive
	{mnesia_table_event, {Op, Val, _Tid}}
	  when element(1, Val) =:= Tab ->
	    if
		Tab == RecName ->
		    handle_event(TabRef, Op, Val);
		true ->
		    handle_event(TabRef, Op, setelement(1, Val, RecName))
	    end,
	    subscr_receiver(TabRef, RecName);

	{mnesia_table_event, {Op, Val, _Tid}} when element(1, Val) =:= schema ->
	    %% clear_table is faked via two schema events
	    %% a schema record delete and a write
	    case Op of
		delete -> handle_event(TabRef, clear_table, {Tab, all});
		_ -> ok
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
    mnesia_lib:dets_sync_close(Tab),
    Tmp = mnesia_lib:tab2tmp(Tab),
    Dat = mnesia_lib:tab2dat(Tab),
    case file:rename(Tmp, Dat) of
	ok ->
	    StorageProps = val({Tab, storage_properties}),
	    DetsOpts = proplists:get_value(dets, StorageProps, []),

	    Args = [{file, mnesia_lib:tab2dat(Tab)},
		    {type, mnesia_lib:disk_type(Tab, Type)},
		    {keypos, 2},
		    {repair, mnesia_monitor:get_env(auto_repair)} | DetsOpts],
	    mnesia_monitor:open_dets(Tab, Args),
	    ok;
	{error, Reason} ->
	    {error, {"Cannot swap tmp files", Tab, Reason}}
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
    end;

handle_last(_Storage, _Type, nobin) ->
    ok.

down(Tab, Storage) ->
    case Storage of
	ram_copies ->
	    ?SAFE(?ets_delete_table(Tab));
	disc_copies ->
	    ?SAFE(?ets_delete_table(Tab));
	disc_only_copies ->
	    TmpFile = mnesia_lib:tab2tmp(Tab),
	    mnesia_lib:dets_sync_close(Tab),
	    file:delete(TmpFile);
        {ext, Alias, Mod} ->
	    catch Mod:close_table(Alias, Tab),
            catch Mod:delete_table(Alias, Tab)
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
    ok = dets:delete(Tab, Key);
db_erase({{ext, Alias, Mod}, Tab}, Key) ->
    ok = Mod:delete(Alias, Tab, Key).

db_match_erase({ram_copies, Tab} , Pat) ->
    true = ?ets_match_delete(Tab, Pat);
db_match_erase({disc_copies, Tab} , Pat) ->
    true = ?ets_match_delete(Tab, Pat);
db_match_erase({disc_only_copies, Tab}, Pat) ->
    ok = dets:match_delete(Tab, Pat);
db_match_erase({{ext, Alias, Mod}, Tab}, Pat) ->
    % "ets style" is to return true
    % "dets style" is to return N | { error, Reason }
    %   or sometimes ok (?)
    % be nice and accept both
    case Mod:match_delete(Alias, Tab, Pat) of
      N when is_integer (N) -> ok;
      true -> ok;
      ok -> ok
    end.

db_put({ram_copies, Tab}, Val) ->
    true = ?ets_insert(Tab, Val);
db_put({disc_copies, Tab}, Val) ->
    true = ?ets_insert(Tab, Val);
db_put({disc_only_copies, Tab}, Val) ->
    ok = dets:insert(Tab, Val);
db_put({{ext, Alias, Mod}, Tab}, Val) ->
    ok = Mod:insert(Alias, Tab, Val).

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
	    do_send_table(Pid, Tab, Storage, RemoteS)
    end.

do_send_table(Pid, Tab, Storage, RemoteS) ->
    {Init, Chunk} =
	case Storage of
	    {ext, Alias, Mod} ->
		case Mod:sender_init(Alias, Tab, RemoteS, Pid) of
		    {standard, I, C} ->
			Pid ! {self(), {first, Mod:info(Alias, Tab, size)}},
			{I, C};
		    {_, _} = Res ->
			Res
		end;
	    Storage ->
		%% Send first
		TabSize = mnesia:table_info(Tab, size),
		KeysPerTransfer = calc_nokeys(Storage, Tab),
		ChunkData = dets:info(Tab, bchunk_format),

		UseDetsChunk =
		    Storage == RemoteS andalso
		    Storage == disc_only_copies andalso
		    ChunkData /= undefined,
		if
		    UseDetsChunk == true ->
			DetsInfo = erlang:system_info(version),
			Pid ! {self(), {first, TabSize, {DetsInfo, ChunkData}}};
		    true  ->
			Pid ! {self(), {first, TabSize}}
		end,
		{_I, _C} =
		    reader_funcs(UseDetsChunk, Tab, Storage, KeysPerTransfer)
	end,
    %% Debug info
    put(mnesia_table_sender, {Tab, node(Pid), Pid}),

    SendIt = fun() ->
		     NeedLock = need_lock(Tab),
		     {atomic, ok} = prepare_copy(Pid, Tab, Storage, NeedLock),
		     send_more(Pid, 1, Chunk, Init(), Tab, Storage),
		     finish_copy(Pid, Tab, Storage, RemoteS, NeedLock)
	     end,

    try SendIt() of
        {_, receiver_died} -> ok;
        {atomic, no_more} ->  ok
    catch
        throw:receiver_died ->
            cleanup_tab_copier(Pid, Storage, Tab),
            ok;
        error:Reason:Stacktrace -> %% Prepare failed
            cleanup_tab_copier(Pid, Storage, Tab),
            {error, {tab_copier, Tab, {Reason, Stacktrace}}}
    after
        unlink(whereis(mnesia_tm))
    end.

prepare_copy(Pid, Tab, Storage, NeedLock) ->
    Trans =
	fun() ->
		NeedLock andalso mnesia:lock_table(Tab, load),
		mnesia_subscr:subscribe(Pid, {table, Tab}),
		update_where_to_write(Tab, node(Pid)),
		mnesia_lib:db_fixtable(Storage, Tab, true),
		ok
	end,
    mnesia:transaction(Trans).


need_lock(Tab) ->
    case ?catch_val({?MODULE, active_trans}) of
	#tid{} = Tid ->
	    %% move_table_copy grabs it's own table-lock
	    %% do not deadlock with it
	    mnesia_lib:unset({?MODULE, active_trans}),
	    case mnesia_locker:get_held_locks(Tab) of
		[{write, Tid}|_] -> false;
		_Locks -> true
	    end;
	_ ->
	    true
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

send_more(Pid, N, Chunk, DataState, Tab, Storage) ->
    receive
	{NewPid, more} ->
	    case send_packet(N - 1, NewPid, Chunk, DataState) of
		New when is_integer(New) ->
		    New - 1;
		NewData ->
		    send_more(NewPid, ?MAX_NOPACKETS, Chunk, NewData,
			      Tab, Storage)
	    end;
	{NewPid, {more, Msg}} when element(1, Storage) == ext ->
	    {ext, Alias, Mod} = Storage,
	    {NewChunk, NewState} =
		Mod:sender_handle_info(Msg, Alias, Tab, NewPid, DataState),
	    case send_packet(N - 1, NewPid, NewChunk, NewState) of
		New when is_integer(New) ->
		    New -1;
		NewData ->
		    send_more(NewPid, N, NewChunk, NewData, Tab,
			      Storage)
	    end;
	{_NewPid, {old_protocol, Tab}} ->
	    Storage =  val({Tab, storage_type}),
	    {Init, NewChunk} =
		reader_funcs(false, Tab, Storage, calc_nokeys(Storage, Tab)),
	    send_more(Pid, 1, NewChunk, Init(), Tab, Storage);

	{copier_done, Node} when Node == node(Pid)->
	    verbose("Receiver of table ~tp crashed on ~p (more)~n", [Tab, Node]),
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

zlib_compress(Data, Level) ->
    BinData = term_to_binary(Data),
    Z = zlib:open(),
    zlib:deflateInit(Z, Level),
    Bs = zlib:deflate(Z, BinData, finish),
    zlib:deflateEnd(Z),
    zlib:close(Z),
    list_to_binary(Bs).

zlib_uncompress(Data) when is_binary(Data) ->
    binary_to_term(zlib:uncompress(Data)).

compression_level() ->
    NoCompression = 0,
    case ?catch_val(send_compressed) of
	{'EXIT', _} ->
	    mnesia_lib:set(send_compressed, NoCompression),
	    NoCompression;
	Val -> Val
    end.

send_packet(N, Pid, _Chunk, '$end_of_table') ->
    Pid ! {self(), no_more},
    N;
send_packet(N, Pid, Chunk, {[], Cont}) ->
    send_packet(N, Pid, Chunk, Chunk(Cont));
send_packet(N, Pid, Chunk, {Recs, Cont}) when N < ?MAX_NOPACKETS ->
    case compression_level() of
	0 ->
	    Pid ! {self(), {more, Recs}};
	Level ->
	    Pid ! {self(), {more_z, zlib_compress(Recs, Level)}}
    end,
    send_packet(N+1, Pid, Chunk, Chunk(Cont));
send_packet(_N, _Pid, _Chunk, DataState) ->
    DataState.

finish_copy(Pid, Tab, Storage, RemoteS, NeedLock) ->
    RecNode = node(Pid),
    DatBin = dat2bin(Tab, Storage, RemoteS),
    Node = node(Pid),
    Trans =
	fun() ->
		NeedLock andalso mnesia:read_lock_table(Tab),
                %% Check that receiver is still alive
                receive {copier_done, Node} ->
                        throw(receiver_died)
                after 0 -> ok
                end,
		A = val({Tab, access_mode}),
		mnesia_controller:sync_and_block_table_whereabouts(Tab, RecNode, RemoteS, A),
		cleanup_tab_copier(Pid, Storage, Tab),
		mnesia_checkpoint:tm_add_copy(Tab, RecNode),
		Pid ! {self(), {no_more, DatBin}},
		receive
		    {Pid, no_more} -> % Dont bother about the spurious 'more' message
			no_more;
		    {copier_done, Node} ->
			verbose("Tab receiver ~tp crashed (more): ~p~n", [Tab, Node]),
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
    error(Reason);
handle_exit(_Pid, _Reason) ->  %% Not from our node, this will be handled by
    ignore.                  %% mnesia_down soon.
