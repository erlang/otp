-module(mnesia_ec).

-include("mnesia.hrl").

-import(mnesia_lib, [important/2, warning/2, dbg_out/2, verbose/2]).

-export([lock/4, write/5, delete/5, delete_object/5, read/5, match_object/5, all_keys/4,
         first/3, last/3, prev/4, next/4, index_match_object/6, index_read/6, table_info/4,
         select/5]).
-export([start/0, init/1]).

-record(prep,
        {protocol = async_ec,
         %% async_ec | sync_ec
         records = [],
         prev_tab = [], % initiate to a non valid table name
         prev_types,
         prev_snmp}).
-record(state, {supervisor}).

val(Var) ->
    case ?catch_val_and_stack(Var) of
        {'EXIT', Stacktrace} ->
            mnesia_lib:other_val(Var, Stacktrace);
        Value ->
            Value
    end.

has_var(Pat) ->
    mnesia:has_var(Pat).

start() ->
    mnesia_monitor:start_proc(?MODULE, ?MODULE, init, [self()]).

init(Parent) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    process_flag(message_queue_data, off_heap),
    case val(debug) of
        Debug when Debug /= debug, Debug /= trace ->
            ignore;
        _ ->
            mnesia_subscr:subscribe(whereis(mnesia_event), {table, schema})
    end,
    proc_lib:init_ack(Parent, {ok, self()}),
    doit_loop(#state{supervisor = Parent}).

doit_loop(#state{} = State) ->
    receive
        {From, {async_ec, Tid, Commit, Tab}} ->
            dbg_out("received async_ec: ~p~n", [{From, {async_ec, Tid, Commit, Tab}}]),
            spawn(fun() -> receive_msg(Tid, Commit, Tab, {rcv, async}) end),
            doit_loop(State);
        {From, {sync_ec, Tid, Commit, Tab}} ->
            dbg_out("received sync_ec: ~p~n", [{From, {sync_ec, Tid, Commit, Tab}}]),
            receive_msg(Tid, Commit, Tab, {rcv, {sync, From}}),
            doit_loop(State);
        {'EXIT', Pid, Reason} ->
            handle_exit(Pid, Reason, State);
        Msg ->
            verbose("** ERROR ** ~p got unexpected message: ~tp~n", [?MODULE, Msg]),
            doit_loop(State)
    end.

%% mnesia_access API
%%
lock({SyncMode, _Pid}, _Ts, _LockItem, _LockKind)
    when SyncMode =:= sync_ec orelse SyncMode =:= async_ec ->
    [];
lock({SyncMode, _}, _Ts, _LockItem, _LockKind) ->
    mnesia:abort({bad_type, SyncMode}).

write({SyncMode, _Pid}, _Ts, Tab, Val, _LockKind)
    when is_atom(Tab)
         andalso Tab /= schema
         andalso is_tuple(Val)
         andalso tuple_size(Val) > 2
         andalso (SyncMode =:= sync_ec orelse SyncMode =:= async_ec) ->
    do_ec_write(SyncMode, Tab, Val);
write({SyncMode, _}, _Ts, Tab, Val, LockKind) ->
    mnesia:abort({bad_type, SyncMode, Tab, Val, LockKind}).

delete({SyncMode, _Pid}, _Ts, Tab, Key, _LockKind)
    when is_atom(Tab)
         andalso Tab /= schema
         andalso (SyncMode =:= sync_ec orelse SyncMode =:= async_ec) ->
    do_ec_delete(SyncMode, Tab, Key);
delete({SyncMode, _}, _Ts, Tab, _Key, _LockKind) ->
    mnesia:abort({bad_type, SyncMode, Tab}).

delete_object({SyncMode, _Pid}, _Ts, Tab, Val, _LockKind)
    when is_atom(Tab)
         andalso Tab /= schema
         andalso is_tuple(Val)
         andalso tuple_size(Val) > 2
         andalso (SyncMode =:= async_ec orelse SyncMode =:= sync_ec) ->
    case has_var(Val) of
        false ->
            do_ec_delete_object(SyncMode, Tab, Val);
        true ->
            mnesia:abort({bad_type, Tab, Val})
    end;
delete_object({SyncMode, _}, _Ts, Tab, _Key, _LockKind) ->
    mnesia:abort({bad_type, SyncMode, Tab}).

read({SyncMode, _Pid}, _Ts, Tab, Key, _LockKind)
    when is_atom(Tab)
         andalso Tab /= schema
         andalso (SyncMode =:= sync_ec orelse SyncMode =:= async_ec) ->
    ec_read(Tab, Key);
read({SyncMode, _}, _Ts, Tab, _Key, _LockKind) ->
    mnesia:abort({bad_type, SyncMode, Tab}).

match_object({SyncMode, _Pid}, _Ts, Tab, Pat, _LockKind)
    when is_atom(Tab)
         andalso Tab /= schema
         andalso is_tuple(Pat)
         andalso tuple_size(Pat) > 2
         andalso (SyncMode =:= async_ec orelse SyncMode =:= sync_ec) ->
    ec_match_object(Tab, Pat);
match_object({SyncMode, _}, _Ts, Tab, Pat, _LockKind) ->
    mnesia:abort({bad_type, SyncMode, Tab, Pat}).

all_keys({SyncMode, _Pid}, _Ts, Tab, _LockKind)
    when is_atom(Tab)
         andalso Tab /= schema
         andalso (SyncMode =:= sync_ec orelse SyncMode =:= async_ec) ->
    Mod = get_crdt_module(Tab),
    Mod:db_all_keys(Tab);
all_keys({SyncMode, _}, _Ts, Tab, _LockKind) ->
    mnesia:abort({bad_type, SyncMode, Tab}).

first({SyncMode, _Pid}, _Ts, Tab)
    when is_atom(Tab)
         andalso Tab /= schema
         andalso (SyncMode =:= sync_ec orelse SyncMode =:= async_ec) ->
    ec_first(Tab);
first({SyncMode, _}, _Ts, Tab) ->
    mnesia:abort({bad_type, SyncMode, Tab}).

last({SyncMode, _Pid}, _Ts, Tab)
    when is_atom(Tab)
         andalso Tab /= schema
         andalso (SyncMode =:= sync_ec orelse SyncMode =:= async_ec) ->
    ec_last(Tab);
last({SyncMode, _}, _Ts, Tab) ->
    mnesia:abort({bad_type, SyncMode, Tab}).

prev({SyncMode, _Pid}, _Ts, Tab, Key)
    when is_atom(Tab)
         andalso Tab /= schema
         andalso (SyncMode =:= sync_ec orelse SyncMode =:= async_ec) ->
    ec_prev(Tab, Key);
prev({SyncMode, _}, _Ts, Tab, _) ->
    mnesia:abort({bad_type, SyncMode, Tab}).

next({SyncMode, _Pid}, _Ts, Tab, Key)
    when is_atom(Tab)
         andalso Tab /= schema
         andalso (SyncMode =:= sync_ec orelse SyncMode =:= async_ec) ->
    ec_next(Tab, Key);
next({SyncMode, _}, _Ts, Tab, _) ->
    mnesia:abort({bad_type, SyncMode, Tab}).

index_match_object({SyncMode, _Pid}, _Ts, Tab, Pat, Attr, _LockKind)
    when is_atom(Tab)
         andalso Tab /= schema
         andalso is_tuple(Pat)
         andalso tuple_size(Pat) > 2
         andalso (SyncMode =:= async_ec orelse SyncMode =:= sync_ec) ->
    ec_index_match_object(Tab, Pat, Attr);
index_match_object({SyncMode, _}, _Ts, Tab, Pat, _Attr, _LockKind) ->
    mnesia:abort({bad_type, SyncMode, Tab, Pat}).

index_read({SyncMode, _}, _Ts, Tab, Key, Attr, _LockKind)
    when is_atom(Tab)
         andalso Tab /= schema
         andalso (SyncMode =:= async_ec orelse SyncMode =:= sync_ec) ->
    Pos = mnesia_schema:attr_tab_to_pos(Tab, Attr),
    Mod = get_crdt_module(Tab),
    case has_var(Key) of
        false ->
            Mod:index_read(Tab, Key, Pos);
        true ->
            mnesia:abort({bad_type, Tab, Attr, Key})
    end;
index_read({SyncMode, _}, _Ts, Tab, _Key, _Attr, _LockKind) ->
    mnesia:abort({bad_type, SyncMode, Tab}).

select({SyncMode, _Pid}, _Ts, Tab, Spec, _LockKind)
    when SyncMode =:= sync_ec orelse SyncMode =:= async_ec ->
    ec_select(Tab, Spec);
select({SyncMode, _}, _Ts, _Tab, _Spec, _LockKind) ->
    mnesia:abort({bad_type, SyncMode}).

table_info({SyncMode, _Pid} = Tid, Ts, Tab, Item)
    when SyncMode =:= sync_ec orelse SyncMode =:= async_ec ->
    mnesia:table_info(Tid, Ts, Tab, Item);
table_info({SyncMode, _}, _Ts, _Tab, _Item) ->
    mnesia:abort({bad_type, SyncMode}).

-spec get_crdt_module(mnesia:table()) -> module().
get_crdt_module(Tab) ->
    case val({Tab, setorbag}) of
        AWSet when AWSet =:= pawset orelse AWSet =:= pawbag ->
            mnesia_pawset;
        RWSet when RWSet =:= prwset orelse RWSet =:= prwbag ->
            mnesia_prwset;
        _ ->
            error({bad_ec_tab_type, Tab})
    end.

%% Private functions, copied or modified from mnesia.erl and mnesia_tm.erl

%% =============== prepare and send ===============
do_ec_write(SyncMode, Tab, Val)
    when is_atom(Tab), Tab /= schema, is_tuple(Val), tuple_size(Val) > 2 ->
    {_, _, _} = mnesia_lib:validate_record(Tab, Val),
    Oid = {Tab, element(2, Val)},
    ec(SyncMode, {Oid, Val, write});
do_ec_write(_SyncMode, Tab, Val) ->
    mnesia:abort({bad_type, Tab, Val}).

do_ec_delete(SyncMode, Tab, Key) when is_atom(Tab), Tab /= schema ->
    Oid = {Tab, Key},
    ec(SyncMode, {Oid, Oid, delete});
do_ec_delete(_SyncMode, Tab, _Key) ->
    mnesia:abort({bad_type, Tab}).

do_ec_delete_object(SyncMode, Tab, Val)
    when is_atom(Tab), Tab /= schema, is_tuple(Val), tuple_size(Val) > 2 ->
    Oid = {Tab, element(2, Val)},
    ec(SyncMode, {Oid, Val, delete_object});
do_ec_delete_object(_SyncMode, Tab, Val) ->
    mnesia:abort({bad_type, Tab, Val}).

ec(Protocol, Item) ->
    {{Tab, Key}, _Val, _Op} = Item,
    Tid = {ec, self()},
    Prep = prepare_items(Tid, Tab, Key, [Item], #prep{protocol = Protocol}),
    CR = Prep#prep.records,
    dbg_out("ec: ~p~n", [CR]),
    case Protocol of
        async_ec ->
            ReadNode = val({Tab, where_to_read}),
            {WaitFor, FirstRes} = async_send_ec(Tid, CR, Tab, ReadNode),
            rec_ec(WaitFor, FirstRes);
        sync_ec ->
            %% Send commit records to the other involved nodes,
            %% and wait for all nodes to complete
            {WaitFor, FirstRes} = sync_send_ec(Tid, CR, Tab, []),
            rec_ec(WaitFor, FirstRes)
    end.

%% @doc @returns a prep record with all items in reverse order
prepare_items(Tid, Tab, Key, Items, Prep) ->
    Types = val({Tab, where_to_commit}),
    case Types of
        [] ->
            mnesia:abort({no_exists, Tab});
        {blocked, _} ->
            unblocked = req({unblock_me, Tab}),
            prepare_items(Tid, Tab, Key, Items, Prep);
        _ ->
            Snmp = val({Tab, snmp}),
            Recs2 = do_prepare_items(Tid, Tab, Key, Types, Snmp, Items, Prep#prep.records),
            Prep#prep{records = Recs2,
                      prev_tab = Tab,
                      prev_types = Types,
                      prev_snmp = Snmp}
    end.

req(R) ->
    case whereis(?MODULE) of
        undefined ->
            {error, {node_not_running, node()}};
        Pid ->
            Ref = make_ref(),
            Pid ! {{self(), Ref}, R},
            rec(Pid, Ref)
    end.

rec(Pid, Ref) ->
    receive
        {?MODULE, Ref, Reply} ->
            Reply;
        {'EXIT', Pid, _} ->
            {error, {node_not_running, node()}}
    end.

do_prepare_items(Tid, Tab, Key, Types, Snmp, Items, Recs) ->
    Recs2 = prepare_snmp(Tid, Tab, Key, Types, Snmp, Items, Recs), % May exit
    Recs3 = prepare_nodes(Tid, Types, Items, Recs2, normal),
    verbose("do prepare_items Rec3: ~p ~p ~p ~p~n", [Tid, Types, Items, Recs2]),
    prepare_ts(Recs3).

prepare_snmp(_Tid, _Tab, _Key, _Types, [], _Items, Recs) ->
    Recs;
prepare_snmp(Tid, Tab, Key, Types, Us, Items, Recs) ->
    if Key /= '_' ->
           {_Oid, _Val, Op} = hd(Items),
           SnmpOid = mnesia_snmp_hook:key_to_oid(Tab, Key, Us), % May exit
           prepare_nodes(Tid, Types, [{Op, Tab, Key, SnmpOid}], Recs, snmp);
       Key == '_' ->
           prepare_nodes(Tid, Types, [{clear_table, Tab}], Recs, snmp)
    end.

%% Returns a list of commit records
prepare_nodes(Tid, [{Node, Storage} | Rest], Items, C, Kind) ->
    {Rec, C2} = pick_node(Tid, Node, C, []),
    Rec2 = prepare_node(Node, Storage, Items, Rec, Kind),
    [Rec2 | prepare_nodes(Tid, Rest, Items, C2, Kind)];
prepare_nodes(_Tid, [], _Items, CommitRecords, _Kind) ->
    CommitRecords.

pick_node(Tid, Node, [Rec | Rest], Done) ->
    if Rec#commit.node == Node ->
           {Rec, Done ++ Rest};
       true ->
           pick_node(Tid, Node, Rest, [Rec | Done])
    end;
pick_node({ec, _}, Node, [], Done) ->
    {#commit{decision = presume_commit, node = Node}, Done}.

prepare_node(Node, Storage, [Item | Items], #commit{ext = Ext0} = Rec, Kind)
    when Kind == snmp ->
    Rec2 =
        case lists:keytake(snmp, 1, Ext0) of
            false ->
                Rec#commit{ext = [{snmp, [Item]} | Ext0]};
            {_, {snmp, Snmp}, Ext} ->
                Rec#commit{ext = [{snmp, [Item | Snmp]} | Ext]}
        end,
    prepare_node(Node, Storage, Items, Rec2, Kind);
prepare_node(Node, Storage, [Item | Items], Rec, Kind) when Kind /= schema ->
    Rec2 =
        case Storage of
            ram_copies ->
                Rec#commit{ram_copies = [Item | Rec#commit.ram_copies]};
            disc_copies ->
                Rec#commit{disc_copies = [Item | Rec#commit.disc_copies]};
            disc_only_copies ->
                Rec#commit{disc_only_copies = [Item | Rec#commit.disc_only_copies]};
            {ext, Alias, Mod} ->
                Ext0 = Rec#commit.ext,
                case lists:keytake(ext_copies, 1, Ext0) of
                    false ->
                        Rec#commit{ext = [{ext_copies, [{{ext, Alias, Mod}, Item}]} | Ext0]};
                    {_, {_, EC}, Ext} ->
                        Rec#commit{ext = [{ext_copies, [{{ext, Alias, Mod}, Item} | EC]} | Ext]}
                end
        end,
    prepare_node(Node, Storage, Items, Rec2, Kind);
prepare_node(_Node, _Storage, [], Rec, _Kind) ->
    Rec.

-spec prepare_ts([#commit{}]) -> [#commit{}].
prepare_ts(Recs) ->
    {Node, Ts} = mnesia_causal:send_msg(),
    do_prepare_ts(lists:reverse(Recs), Node, Ts).

%% Returns a list of commit record, with node and ts set
-spec do_prepare_ts([#commit{}], node(), mnesia_causal:vclock()) ->
                       [#commit{sender :: atom()}].
do_prepare_ts([Hd | Tl], Node, Ts) ->
    % we only add ts once, since we consider all copies in a commit as a whole
    Commit = Hd#commit{sender = Node, ts = Ts},
    [Commit | do_prepare_ts(Tl, Node, Ts)];
do_prepare_ts([], _Node, _Ts) ->
    [].

do_update_ts(ram_copies, Copy, Ts) ->
    add_time(Copy, Ts);
do_update_ts(disc_copies, Copy, Ts) ->
    add_time(Copy, Ts);
do_update_ts(disc_only_copies, Copy, Ts) ->
    add_time(Copy, Ts);
do_update_ts({ext, _Alias, _Mod}, ExtCopy, Ts) ->
    add_time(ExtCopy, Ts).

add_time({Oid, Val, Op}, Ts) ->
    {Oid, erlang:append_element(Val, Ts), Op};
add_time({ExtInfo, {Oid, Val, Op}}, Ts) ->
    {ExtInfo, {Oid, erlang:append_element(Val, Ts), Op}}.

receive_msg(Tid, Commit, _Tab, local) ->
    mnesia_causal:deliver_one(Commit),
    do_ec(Tid, Commit);
receive_msg(Tid, Commit, Tab, {rcv, async}) ->
    Deliverable = mnesia_causal:rcv_msg(Tid, Commit, Tab),
    dbg_out("found async_ec devliverable commits: ~p~n", [Deliverable]),
    lists:foreach(fun({Tid1, Commit1, Tab1}) ->
                     do_async_ec(Tid1, mnesia_tm:new_cr_format(Commit1), Tab1)
                  end,
                  Deliverable);
receive_msg(Tid, Commit, Tab, {rcv, {sync, From}}) ->
    Deliverable = mnesia_causal:rcv_msg(Tid, Commit, Tab, From),
    dbg_out("found sync_ec devliverable commits: ~p~n", [Deliverable]),
    lists:foreach(fun({Tid1, Commit1 = #commit{}, Tab1, From1}) ->
                     do_sync_ec(From1, Tid1, mnesia_tm:new_cr_format(Commit1), Tab1)
                  end,
                  Deliverable).

sync_send_ec(Tid, [Head | Tail], Tab, WaitFor) ->
    Node = Head#commit.node,
    if Node == node() ->
           % if the node we want to deliver to is local, we deliver it directly
           {WF, _} = sync_send_ec(Tid, Tail, Tab, WaitFor),
           Res = receive_msg(Tid, Head, Tab, local),
           {WF, Res};
       true ->
           % otherwise we need to send it and wait for ack
           {?MODULE, Node} ! {self(), {sync_ec, Tid, Head, Tab}},
           sync_send_ec(Tid, Tail, Tab, [Node | WaitFor])
    end;
sync_send_ec(_Tid, [], _Tab, WaitFor) ->
    {WaitFor, {'EXIT', {aborted, {node_not_running, WaitFor}}}}.

%% @returns {WaitFor, Res}
async_send_ec(_Tid, _Committs, Tab, nowhere) ->
    {[], {'EXIT', {aborted, {no_exists, Tab}}}};
async_send_ec(Tid, Commits, Tab, ReadNode) ->
    async_send_ec(Tid, Commits, Tab, ReadNode, [], ok).

async_send_ec(Tid, [Head | Tail], Tab, ReadNode, WaitFor, Res) ->
    dbg_out("async_send_ec Nodes: ~p~n", [[Head | Tail]]),
    Node = Head#commit.node,
    if ReadNode == Node, Node == node() ->
           NewRes = receive_msg(Tid, Head, Tab, local),
           async_send_ec(Tid, Tail, Tab, ReadNode, WaitFor, NewRes);
       ReadNode == Node ->
           % if the readnode is not local, we need to send it to the readnode and
           % _wait_ for it, note the sync_ec
           % this might happen when we are not a mnemia node
           {?MODULE, Node} ! {self(), {sync_ec, Tid, Head, Tab}},
           NewRes = {'EXIT', {aborted, {node_not_running, Node}}},
           async_send_ec(Tid, Tail, Tab, ReadNode, [Node | WaitFor], NewRes);
       true ->
           {?MODULE, Node} ! {self(), {async_ec, Tid, Head, Tab}},
           dbg_out("sending ~p to ~p~n", [{async_ec, Tid, Head, Tab}, {?MODULE, Node}]),
           async_send_ec(Tid, Tail, Tab, ReadNode, WaitFor, Res)
    end;
async_send_ec(_Tid, [], _Tab, _ReadNode, WaitFor, Res) ->
    {WaitFor, Res}.

rec_ec([Node | Tail], Res) when Node /= node() ->
    NewRes = get_ec_reply(Node, Res),
    rec_ec(Tail, NewRes);
rec_ec([], Res) ->
    Res.

get_ec_reply(Node, Res) ->
    receive
        {?MODULE, Node, {'EXIT', Reason}} ->
            {'EXIT', {aborted, {badarg, Reason}}};
        {?MODULE, Node, {ec_res, ok}} ->
            case Res of
                {'EXIT', {aborted, {node_not_running, _Node}}} ->
                    ok;
                _ ->
                    %% Prioritize bad results, but node_not_running
                    Res
            end;
        {?MODULE, Node, {ec_res, Reply}} ->
            Reply;
        {mnesia_down, Node} ->
            case get(mnesia_activity_state) of
                {_, Tid, _Ts} when element(1, Tid) == tid ->
                    %% Hmm dirty called inside a transaction, to avoid
                    %% hanging transaction we need to restart the transaction
                    mnesia:abort({node_not_running, Node});
                _ ->
                    %% It's ok to ignore mnesia_down's since we will make
                    %% the replicas consistent again when Node is started
                    Res
            end
    after 1000 ->
        case lists:member(Node, val({current, db_nodes})) of
            true ->
                get_ec_reply(Node, Res);
            false ->
                Res
        end
    end.

%%% =============== Receiving and update ===================
do_async_ec(Tid, Commit, _Tab) ->
    ?eval_debug_fun({?MODULE, async_ec, pre}, [{tid, Tid}]),
    do_ec(Tid, Commit),
    ?eval_debug_fun({?MODULE, async_ec, post}, [{tid, Tid}]).

do_sync_ec(From, Tid, Commit, _Tab) ->
    ?eval_debug_fun({?MODULE, sync_ec, pre}, [{tid, Tid}]),
    Res = do_ec(Tid, Commit),
    ?eval_debug_fun({?MODULE, sync_ec, post}, [{tid, Tid}]),
    From ! {?MODULE, node(), {ec_res, Res}}.

do_ec(Tid, Commit) when Commit#commit.schema_ops == [] ->
    mnesia_log:log(Commit),
    do_commit(Tid, Commit).

do_commit(Tid, C) ->
    do_commit(Tid, C, optional).

do_commit(Tid, C, DumperMode) ->
    mnesia_dumper:update(Tid, C#commit.schema_ops, DumperMode),
    R = mnesia_tm:do_snmp(Tid, proplists:get_value(snmp, C#commit.ext, [])),
    R2 = do_update(Tid, ram_copies, C#commit.ram_copies, C#commit.ts, R),
    R3 = do_update(Tid, disc_copies, C#commit.disc_copies, C#commit.ts, R2),
    R4 = do_update(Tid, disc_only_copies, C#commit.disc_only_copies, C#commit.ts, R3),
    R5 = do_update_ext(Tid, C#commit.ext, C#commit.ts, R4),
    mnesia_subscr:report_activity(Tid),
    R5.

%% This could/should be optimized
do_update_ext(_Tid, [], _Ts, OldRes) ->
    OldRes;
do_update_ext(Tid, Ext, Ts, OldRes) ->
    case lists:keyfind(ext_copies, 1, Ext) of
        false ->
            OldRes;
        {_, Ops} ->
            Do = fun({{ext, _, _} = Storage, Op}, R) -> do_update(Tid, Storage, [Op], Ts, R) end,
            lists:foldl(Do, OldRes, Ops)
    end.

%% Update the items
do_update(Tid, Storage, [Op1 | Ops], Ts, OldRes) ->
    Op = do_update_ts(Storage, Op1, Ts),
    try do_update_op(Tid, Storage, Op) of
        ok ->
            do_update(Tid, Storage, Ops, Ts, OldRes);
        NewRes ->
            do_update(Tid, Storage, Ops, Ts, NewRes)
    catch
        _:Reason:ST ->
            %% This may only happen when we recently have
            %% deleted our local replica, changed storage_type
            %% or transformed table
            %% BUGBUG: Updates may be lost if storage_type is changed.
            %%         Determine actual storage type and try again.
            %% BUGBUG: Updates may be lost if table is transformed.
            verbose("do_update in ~w failed: ~tp -> {'EXIT', ~tp}~n", [Tid, Op, {Reason, ST}]),
            do_update(Tid, Storage, Ops, Ts, OldRes)
    end;
do_update(_Tid, _Storage, [], _Ts, Res) ->
    Res.

do_update_op(Tid, Storage, {{Tab, K}, Obj, write}) ->
    commit_write(?catch_val({Tab, commit_work}), Tid, Storage, Tab, K, Obj, undefined),
    Mod = get_crdt_module(Tab),
    Mod:db_put(Storage, Tab, Obj);
do_update_op(Tid, Storage, {{Tab, K}, Obj, delete}) ->
    % note here parameter is Obj rather than Val, this is mostly better since we
    % can always extract the key from the object
    % we send Obj instead of Key for processing
    commit_delete(?catch_val({Tab, commit_work}), Tid, Storage, Tab, K, Obj, undefined),
    Mod = get_crdt_module(Tab),
    Mod:db_erase(Storage, Tab, Obj);
do_update_op(Tid, Storage, {{Tab, K}, {RecName, Incr}, update_counter}) ->
    {NewObj, OldObjs} =
        try
            NewVal = mnesia_lib:db_update_counter(Storage, Tab, K, Incr),
            true = is_integer(NewVal) andalso NewVal >= 0,
            {{RecName, K, NewVal}, [{RecName, K, NewVal - Incr}]}
        catch
            error:_ when Incr > 0 ->
                New = {RecName, K, Incr},
                mnesia_lib:db_put(Storage, Tab, New),
                {New, []};
            error:_ ->
                Zero = {RecName, K, 0},
                mnesia_lib:db_put(Storage, Tab, Zero),
                {Zero, []}
        end,
    commit_update(?catch_val({Tab, commit_work}), Tid, Storage, Tab, K, NewObj, OldObjs),
    element(3, NewObj);
do_update_op(Tid, Storage, {{Tab, Key}, Obj, delete_object}) ->
    commit_del_object(?catch_val({Tab, commit_work}), Tid, Storage, Tab, Key, Obj),
    Mod = get_crdt_module(Tab),
    Mod:db_match_erase(Storage, Tab, Obj);
do_update_op(Tid, Storage, {{Tab, Key}, Obj, clear_table}) ->
    commit_clear(?catch_val({Tab, commit_work}), Tid, Storage, Tab, Key, Obj),
    mnesia_lib:db_match_erase(Storage, Tab, Obj).

commit_write([], _, _, _, _, _, _) ->
    ok;
commit_write([{checkpoints, CpList} | R], Tid, Storage, Tab, K, Obj, Old) ->
    mnesia_checkpoint:tm_retain(Tid, Tab, K, write, CpList),
    commit_write(R, Tid, Storage, Tab, K, Obj, Old);
commit_write([H | R], Tid, Storage, Tab, K, Obj, Old) when element(1, H) == subscribers ->
    mnesia_subscr:report_table_event(H, Tab, Tid, Obj, write, Old),
    commit_write(R, Tid, Storage, Tab, K, Obj, Old);
commit_write([H | R], Tid, Storage, Tab, K, Obj, Old) when element(1, H) == index ->
    mnesia_index:add_index(H, Storage, Tab, K, Obj, Old),
    commit_write(R, Tid, Storage, Tab, K, Obj, Old).

commit_update([], _, _, _, _, _, _) ->
    ok;
commit_update([{checkpoints, CpList} | R], Tid, Storage, Tab, K, Obj, _) ->
    Old = mnesia_checkpoint:tm_retain(Tid, Tab, K, write, CpList),
    commit_update(R, Tid, Storage, Tab, K, Obj, Old);
commit_update([H | R], Tid, Storage, Tab, K, Obj, Old)
    when element(1, H) == subscribers ->
    mnesia_subscr:report_table_event(H, Tab, Tid, Obj, write, Old),
    commit_update(R, Tid, Storage, Tab, K, Obj, Old);
commit_update([H | R], Tid, Storage, Tab, K, Obj, Old) when element(1, H) == index ->
    mnesia_index:add_index(H, Storage, Tab, K, Obj, Old),
    commit_update(R, Tid, Storage, Tab, K, Obj, Old).

commit_delete([], _, _, _, _, _, _) ->
    ok;
commit_delete([{checkpoints, CpList} | R], Tid, Storage, Tab, K, Obj, _) ->
    Old = mnesia_checkpoint:tm_retain(Tid, Tab, K, delete, CpList),
    commit_delete(R, Tid, Storage, Tab, K, Obj, Old);
commit_delete([H | R], Tid, Storage, Tab, K, Obj, Old)
    when element(1, H) == subscribers ->
    mnesia_subscr:report_table_event(H, Tab, Tid, Obj, delete, Old),
    commit_delete(R, Tid, Storage, Tab, K, Obj, Old);
commit_delete([H | R], Tid, Storage, Tab, K, Obj, Old) when element(1, H) == index ->
    mnesia_index:delete_index(H, Storage, Tab, K),
    commit_delete(R, Tid, Storage, Tab, K, Obj, Old).

commit_del_object([], _, _, _, _, _) ->
    ok;
commit_del_object([{checkpoints, CpList} | R], Tid, Storage, Tab, K, Obj) ->
    mnesia_checkpoint:tm_retain(Tid, Tab, K, delete_object, CpList),
    commit_del_object(R, Tid, Storage, Tab, K, Obj);
commit_del_object([H | R], Tid, Storage, Tab, K, Obj) when element(1, H) == subscribers ->
    mnesia_subscr:report_table_event(H, Tab, Tid, Obj, delete_object),
    commit_del_object(R, Tid, Storage, Tab, K, Obj);
commit_del_object([H | R], Tid, Storage, Tab, K, Obj) when element(1, H) == index ->
    mnesia_index:del_object_index(H, Storage, Tab, K, Obj),
    commit_del_object(R, Tid, Storage, Tab, K, Obj).

commit_clear([], _, _, _, _, _) ->
    ok;
commit_clear([{checkpoints, CpList} | R], Tid, Storage, Tab, K, Obj) ->
    mnesia_checkpoint:tm_retain(Tid, Tab, K, clear_table, CpList),
    commit_clear(R, Tid, Storage, Tab, K, Obj);
commit_clear([H | R], Tid, Storage, Tab, K, Obj) when element(1, H) == subscribers ->
    mnesia_subscr:report_table_event(H, Tab, Tid, Obj, clear_table, undefined),
    commit_clear(R, Tid, Storage, Tab, K, Obj);
commit_clear([H | R], Tid, Storage, Tab, K, Obj) when element(1, H) == index ->
    mnesia_index:clear_index(H, Tab, K, Obj),
    commit_clear(R, Tid, Storage, Tab, K, Obj).

%% =============== read operations ===============

ec_rpc(Tab, M, F, Args) ->
    Node = val({Tab, where_to_read}),
    do_ec_rpc(Tab, Node, M, F, Args).

do_ec_rpc(_Tab, nowhere, _, _, Args) ->
    mnesia:abort({no_exists, Args});
do_ec_rpc(_Tab, Local, M, F, Args) when Local =:= node() ->
    try
        apply(M, F, Args)
    catch
        Res ->
            Res;
        _:_ ->
            mnesia:abort({badarg, Args})
    end;
do_ec_rpc(Tab, Node, M, F, Args) ->
    case mnesia_rpc:call(Node, M, F, Args) of
        {badrpc, Reason} ->
            timer:sleep(20), %% Do not be too eager, and can't use yield on SMP
            %% Sync with mnesia_monitor
            _ = try
                    sys:get_status(mnesia_monitor)
                catch
                    _:_ ->
                        ok
                end,
            case mnesia_controller:call({check_w2r, Node, Tab}) % Sync
            of
                NewNode when NewNode =:= Node ->
                    ErrorTag = mnesia_lib:dirty_rpc_error_tag(Reason),
                    mnesia:abort({ErrorTag, Args});
                NewNode ->
                    case get(mnesia_activity_state) of
                        {_Mod, Tid, _Ts} when is_record(Tid, tid) ->
                            %% In order to perform a consistent
                            %% retry of a transaction we need
                            %% to acquire the lock on the NewNode.
                            %% In this context we do neither know
                            %% the kind or granularity of the lock.
                            %% --> Abort the transaction
                            mnesia:abort({node_not_running, Node});
                        {error, {node_not_running, _}} ->
                            %% Mnesia is stopping
                            mnesia:abort({no_exists, Args});
                        _ ->
                            %% Splendid! A dirty retry is safe
                            %% 'Node' probably went down now
                            %% Let mnesia_controller get broken link message first
                            do_ec_rpc(Tab, NewNode, M, F, Args)
                    end
            end;
        Other ->
            Other
    end.

ec_read(Tab, Key) ->
    Mod = get_crdt_module(Tab),
    ec_rpc(Tab, Mod, db_get, [Tab, Key]).

ec_select(Tab, Spec) ->
    Mod = get_crdt_module(Tab),
    ec_rpc(Tab, Mod, remote_ec_select, [Tab, Spec]).

ec_first(Tab) ->
    Mod = get_crdt_module(Tab),
    ec_rpc(Tab, Mod, db_first, [Tab]).

ec_last(Tab) ->
    Mod = get_crdt_module(Tab),
    ec_rpc(Tab, Mod, db_last, [Tab]).

ec_prev(Tab, Key) ->
    Mod = get_crdt_module(Tab),
    ec_rpc(Tab, Mod, db_prev_key, [Tab, Key]).

ec_next(Tab, Key) ->
    Mod = get_crdt_module(Tab),
    ec_rpc(Tab, Mod, db_next_key, [Tab, Key]).

-spec ec_match_object(Tab, Pattern) -> [Record]
    when Tab :: mnesia:table(),
         Pattern :: tuple(),
         Record :: tuple().
ec_match_object(Tab, Pat)
    when is_atom(Tab), Tab /= schema, is_tuple(Pat), tuple_size(Pat) > 2 ->
    Mod = get_crdt_module(Tab),
    ec_rpc(Tab, Mod, remote_match_object, [Tab, Pat]);
ec_match_object(Tab, Pat) ->
    mnesia:abort({bad_type, Tab, Pat}).

ec_index_match_object(Tab, Pat, Attr)
    when is_atom(Tab), Tab /= schema, is_tuple(Pat), tuple_size(Pat) > 2 ->
    Mod = get_crdt_module(Tab),
    case mnesia_schema:attr_tab_to_pos(Tab, Attr) of
        {_} ->
            ec_match_object(Tab, Pat);
        Pos when Pos =< tuple_size(Pat) ->
            case has_var(element(2, Pat)) of
                false ->
                    ec_match_object(Tab, Pat);
                true ->
                    Elem = element(Pos, Pat),
                    case has_var(Elem) of
                        false ->
                            ec_rpc(Tab, Mod, index_match_object, [Tab, Pat, Pos]);
                        true ->
                            mnesia:abort({bad_type, Tab, Attr, Elem})
                    end
            end;
        BadPos ->
            mnesia:abort({bad_type, Tab, BadPos})
    end;
ec_index_match_object(Tab, Pat, _Attr) ->
    mnesia:abort({bad_type, Tab, Pat}).

%% =============== stop operations ===============

handle_exit(Pid, _Reason, State) when node(Pid) /= node() ->
    %% We got exit from a remote fool
    doit_loop(State);
handle_exit(Pid, _Reason, State) when Pid == State#state.supervisor ->
    %% Our supervisor has died, time to stop
    do_stop(State).

do_stop(#state{}) ->
    exit(shutdown).
