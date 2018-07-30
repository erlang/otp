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
%%     $Id: mnesia_lib.erl,v 1.3 2009/07/01 15:45:40 kostis Exp $
%% This module contains all sorts of various which doesn't fit
%% anywhere else. Basically everything is exported.

-module(mnesia_lib).

-include("mnesia.hrl").
-include_lib("kernel/include/file.hrl").

-export([core_file/0]).

-export([
	 active_tables/0,
	 add/2,
	 add_list/2,
	 all_nodes/0,
%%	 catch_val/1,
	 cleanup_tmp_files/1,
	 copy_file/2,
	 copy_holders/1,
	 coredump/0,
	 coredump/1,
	 create_counter/1,
	 cs_to_nodes/1,
	 cs_to_storage_type/2,
	 dets_to_ets/6,
	 db_chunk/2,
	 db_init_chunk/1,
	 db_init_chunk/2,
	 db_init_chunk/3,
	 db_erase/2,
	 db_erase/3,
	 db_erase_tab/1,
	 db_erase_tab/2,
	 db_first/1,
	 db_first/2,
	 db_last/1,
	 db_last/2,
	 db_fixtable/3,
	 db_get/2,
	 db_get/3,
	 db_match_erase/2,
	 db_match_erase/3,
	 db_match_object/2,
	 db_match_object/3,
	 db_next_key/2,
	 db_next_key/3,
	 db_prev_key/2,
	 db_prev_key/3,
	 db_put/2,
	 db_put/3,
	 db_select/2,
	 db_select/3,
	 db_slot/2,
	 db_slot/3,
	 db_update_counter/3,
	 db_update_counter/4,
	 dbg_out/2,
	 del/2,
	 dets_sync_close/1,
	 dets_sync_open/2,
	 dets_sync_open/3,
	 dir/0,
	 dir/1,
	 dir_info/0,
	 dirty_rpc_error_tag/1,
	 dist_coredump/0,
	 disk_type/1,
	 disk_type/2,
	 elems/2,
	 ensure_loaded/1,
	 error/2,
	 error_desc/1,
	 etype/1,
	 exists/1,
	 fatal/2,
	 get_node_number/0,
	 fix_error/1,
	 important/2,
	 incr_counter/1,
	 incr_counter/2,
	 intersect/2,
	 is_running/0,
	 is_running/1,
	 is_running_remote/0,
	 is_string/1,
	 key_search_delete/3,
	 key_search_all/3,
	 last_error/0,
	 local_active_tables/0,
	 lock_table/1,
	 mkcore/1,
	 not_active_here/1,
	 other_val/2,
	 pad_name/3,
	 random_time/2,
	 read_counter/1,
	 readable_indecies/1,
	 remote_copy_holders/1,
	 report_fatal/2,
	 report_system_event/1,
	 running_nodes/0,
	 running_nodes/1,
	 schema_cs_to_storage_type/2,
	 search_delete/2,
	 set/2,
	 set_counter/2,
	 set_local_content_whereabouts/1,
	 set_remote_where_to_read/1,
	 set_remote_where_to_read/2,
	 show/1,
	 show/2,
	 sort_commit/1,
	 storage_type_at_node/2,
	 swap_tmp_files/1,
	 tab2dat/1,
	 tab2dmp/1,
	 tab2tmp/1,
	 tab2dcd/1,
	 tab2dcl/1,
	 to_list/1,
	 union/2,
	 uniq/1,
	 unlock_table/1,
	 unset/1,
	 update_counter/2,
	 val/1,
	 vcore/0,
	 vcore/1,
	 verbose/2,
	 view/0,
	 view/1,
	 view/2,
	 warning/2,

	 is_debug_compiled/0,
	 activate_debug_fun/5,
	 deactivate_debug_fun/3,
	 eval_debug_fun/4,
	 scratch_debug_fun/0
	]).


search_delete(Obj, List) ->
    search_delete(Obj, List, [], none).
search_delete(Obj, [Obj|Tail], Ack, _Res) ->
    search_delete(Obj, Tail, Ack, Obj);
search_delete(Obj, [H|T], Ack, Res) ->
    search_delete(Obj, T, [H|Ack], Res);
search_delete(_, [], Ack, Res) ->
    {Res, Ack}.

key_search_delete(Key, Pos, TupleList) ->
    key_search_delete(Key, Pos, TupleList, none, []).
key_search_delete(Key, Pos, [H|T], _Obj, Ack) when element(Pos, H) == Key ->
    key_search_delete(Key, Pos, T, H, Ack);
key_search_delete(Key, Pos, [H|T], Obj, Ack) ->
    key_search_delete(Key, Pos, T, Obj, [H|Ack]);
key_search_delete(_, _, [], Obj, Ack) ->
    {Obj, Ack}.

key_search_all(Key, Pos, TupleList) ->
    key_search_all(Key, Pos, TupleList, []).
key_search_all(Key, N, [H|T], Ack) when element(N, H) == Key ->
    key_search_all(Key, N, T, [H|Ack]);
key_search_all(Key, N, [_|T], Ack) ->
    key_search_all(Key, N, T, Ack);
key_search_all(_, _, [], Ack) -> Ack.

intersect(L1, L2) ->
    L2 -- (L2 -- L1).

elems(I, [H|T]) ->
    [element(I, H) | elems(I, T)];
elems(_, []) ->
    [].

%%  sort_commit see to that checkpoint info is always first in
%%  commit_work structure the other info don't need to be sorted.
sort_commit(List) ->
    sort_commit2(List, []).

sort_commit2([{checkpoints, ChkpL}| Rest], Acc) ->
    [{checkpoints, ChkpL}| Rest] ++ Acc;
sort_commit2([H | R], Acc) ->
    sort_commit2(R, [H | Acc]);
sort_commit2([], Acc) -> Acc.

is_string([H|T]) ->
    if
	0 =< H, H < 256, integer(H)  -> is_string(T);
	true -> false
    end;
is_string([]) -> true.

%%%

union([H|L1], L2) ->
    case lists:member(H, L2) of
	true -> union(L1, L2);
	false -> [H | union(L1, L2)]
    end;
union([], L2) -> L2.

uniq([]) ->
    [];
uniq(List) ->
    [H|T] = lists:sort(List),
    uniq1(H, T, []).

uniq1(H, [H|R], Ack) ->
    uniq1(H, R, Ack);
uniq1(Old, [H|R], Ack) ->
    uniq1(H, R, [Old|Ack]);
uniq1(Old, [], Ack) ->
    [Old| Ack].

to_list(X) when list(X) -> X;
to_list(X) -> atom_to_list(X).

all_nodes() ->
    Ns = mnesia:system_info(db_nodes) ++
	mnesia:system_info(extra_db_nodes),
    mnesia_lib:uniq(Ns).

running_nodes() ->
    running_nodes(all_nodes()).

running_nodes(Ns) ->
    {Replies, _BadNs} = rpc:multicall(Ns, ?MODULE, is_running_remote, []),
    [N || {GoodState, N} <- Replies, GoodState == true].

is_running_remote() ->
    IsRunning = is_running(),
    {IsRunning == yes, node()}.

is_running(Node) when atom(Node) ->
    case rpc:call(Node, ?MODULE, is_running, []) of
	{badrpc, _} -> no;
	X -> X
    end.

is_running() ->
    case ?catch_val(mnesia_status) of
	{'EXIT', _} -> no;
	running -> yes;
	starting -> starting;
	stopping -> stopping
    end.

show(X) ->
    show(X, []).
show(F, A) ->
    io:format(user, F, A).


pad_name([Char | Chars], Len, Tail) ->
    [Char | pad_name(Chars, Len - 1, Tail)];
pad_name([], Len, Tail) when Len =< 0 ->
    Tail;
pad_name([], Len, Tail) ->
    [$ | pad_name([], Len - 1, Tail)].

%% Some utility functions .....
active_here(Tab) ->
    case val({Tab, where_to_read}) of
	Node when Node == node() -> true;
	_ -> false
    end.

not_active_here(Tab) ->
    not active_here(Tab).

exists(Fname) ->
    case file:open(Fname, [raw,read]) of
	{ok, F} ->file:close(F), true;
	_ -> false
    end.

dir() -> mnesia_monitor:get_env(dir).

dir(Fname) ->
    filename:join([dir(), to_list(Fname)]).

tab2dat(Tab) ->  %% DETS files
    dir(lists:concat([Tab, ".DAT"])).

tab2tmp(Tab) ->
    dir(lists:concat([Tab, ".TMP"])).

tab2dmp(Tab) ->  %% Dumped ets tables
    dir(lists:concat([Tab, ".DMP"])).

tab2dcd(Tab) ->  %% Disc copies data
    dir(lists:concat([Tab, ".DCD"])).

tab2dcl(Tab) ->  %% Disc copies log
    dir(lists:concat([Tab, ".DCL"])).

storage_type_at_node(Node, Tab) ->
    search_key(Node, [{disc_copies, val({Tab, disc_copies})},
		      {ram_copies, val({Tab, ram_copies})},
		      {disc_only_copies, val({Tab, disc_only_copies})}]).

cs_to_storage_type(Node, Cs) ->
    search_key(Node, [{disc_copies, Cs#cstruct.disc_copies},
		      {ram_copies, Cs#cstruct.ram_copies},
		      {disc_only_copies, Cs#cstruct.disc_only_copies}]).

schema_cs_to_storage_type(Node, Cs) ->
    case cs_to_storage_type(Node, Cs) of
	unknown when Cs#cstruct.name == schema -> ram_copies;
	Other -> Other
    end.


search_key(Key, [{Val, List} | Tail]) ->
    case lists:member(Key, List) of
	true -> Val;
	false -> search_key(Key, Tail)
    end;
search_key(_Key, []) ->
    unknown.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ops, we've got some global variables here :-)

%% They are
%%
%%   {Tab, setorbag}, -> set | bag
%%   {Tab, storage_type}       -> disc_copies |ram_copies | unknown (**)
%%   {Tab, disc_copies}        -> node list  (from schema)
%%   {Tab, ram_copies}, -> node list  (from schema)
%%   {Tab, arity}, -> number
%%   {Tab, attributes}, -> atom list
%%   {Tab, wild_pattern}, -> record tuple with '_'s
%%   {Tab, {index, Pos}}       -> ets table
%%   {Tab, index}              -> integer list
%%   {Tab, cstruct}            -> cstruct  structure
%%

%%   The following fields are dynamic according to the
%%   the current node/table situation

%%   {Tab, where_to_write}      -> node list
%%   {Tab, where_to_read}       -> node | nowhere
%%
%%   {schema, tables}                    -> tab list
%%   {schema, local_tables}              -> tab list  (**)
%%
%%   {current, db_nodes}                  -> node list
%%
%%   dir                                  -> directory path (**)
%%   mnesia_status                        -> status | running | stopping (**)
%%   (**) ==   (Different on all nodes)
%%

val(Var) ->
    case ?catch_val(Var) of
	{'EXIT', _ReASoN_} -> mnesia_lib:other_val(Var, _ReASoN_);
	_VaLuE_ -> _VaLuE_
    end.

set(Var, Val) ->
    ?ets_insert(mnesia_gvar, {Var, Val}).

unset(Var) ->
    ?ets_delete(mnesia_gvar, Var).

other_val(Var, Other) ->
    case Var of
	{_, where_to_read} -> nowhere;
	{_, where_to_write} -> [];
	{_, active_replicas} -> [];
	_ ->
	    pr_other(Var, Other)
    end.

pr_other(Var, Other) ->
    Why =
	case is_running() of
	    no -> {node_not_running, node()};
	    _ -> {no_exists, Var}
	end,
    verbose("~p (~p) val(mnesia_gvar, ~w) -> ~p ~p ~n",
	    [self(), process_info(self(), registered_name),
	     Var, Other, Why]),
    case Other of
	{badarg, [{ets, lookup_element, _}|_]} ->
	    exit(Why);
	_ ->
	    erlang:error(Why)
    end.

%% Some functions for list valued variables
add(Var, Val) ->
    L = val(Var),
    set(Var, [Val | lists:delete(Val, L)]).

add_list(Var, List) ->
    L = val(Var),
    set(Var, union(L, List)).

del(Var, Val) ->
    L = val(Var),
    set(Var, lists:delete(Val, L)).

%% This function is needed due to the fact
%% that the application_controller enters
%% a deadlock now and then. ac is implemented
%% as a rather naive server.
ensure_loaded(Appl) ->
    case application_controller:get_loaded(Appl) of
	{true, _} ->
	    ok;
	false ->
	    case application:load(Appl) of
		ok ->
		    ok;
		{error, {already_loaded, Appl}} ->
		    ok;
		{error, Reason} ->
		    {error, {application_load_error, Reason}}
	    end
    end.

local_active_tables() ->
    Tabs = val({schema, local_tables}),
    lists:zf(fun(Tab) -> active_here(Tab) end, Tabs).

active_tables() ->
    Tabs = val({schema, tables}),
    F = fun(Tab) ->
		case val({Tab, where_to_read}) of
		    nowhere -> false;
		    _ -> {true, Tab}
		end
	end,
    lists:zf(F, Tabs).

etype(X) when integer(X) -> integer;
etype([]) -> nil;
etype(X) when list(X) -> list;
etype(X) when tuple(X) -> tuple;
etype(X) when atom(X) -> atom;
etype(_) -> othertype.

remote_copy_holders(Cs) ->
    copy_holders(Cs) -- [node()].

copy_holders(Cs) when Cs#cstruct.local_content == false ->
    cs_to_nodes(Cs);
copy_holders(Cs) when Cs#cstruct.local_content == true ->
    case lists:member(node(), cs_to_nodes(Cs)) of
	true -> [node()];
	false -> []
    end.


set_remote_where_to_read(Tab) ->
    set_remote_where_to_read(Tab, []).

set_remote_where_to_read(Tab, Ignore) ->
    Active = val({Tab, active_replicas}),
    Valid =
	case mnesia_recover:get_master_nodes(Tab) of
	    [] ->  Active;
	    Masters -> mnesia_lib:intersect(Masters, Active)
	end,
    Available = mnesia_lib:intersect(val({current, db_nodes}), Valid -- Ignore),
    DiscOnlyC = val({Tab, disc_only_copies}),
    Prefered  = Available -- DiscOnlyC,
    if
	Prefered /= [] ->
	    set({Tab, where_to_read}, hd(Prefered));
	Available /= [] ->
	    set({Tab, where_to_read}, hd(Available));
	true ->
	    set({Tab, where_to_read}, nowhere)
    end.

%%% Local only
set_local_content_whereabouts(Tab) ->
    add({schema, local_tables}, Tab),
    add({Tab, active_replicas}, node()),
    set({Tab, where_to_write}, [node()]),
    set({Tab, where_to_read}, node()).

%%% counter routines

create_counter(Name) ->
    set_counter(Name, 0).

set_counter(Name, Val) ->
    ?ets_insert(mnesia_gvar, {Name, Val}).

incr_counter(Name) ->
    ?ets_update_counter(mnesia_gvar, Name, 1).

incr_counter(Name, I) ->
    ?ets_update_counter(mnesia_gvar, Name, I).

update_counter(Name, Val) ->
    ?ets_update_counter(mnesia_gvar, Name, Val).

read_counter(Name) ->
    ?ets_lookup_element(mnesia_gvar, Name, 2).

cs_to_nodes(Cs) ->
    Cs#cstruct.disc_only_copies ++
    Cs#cstruct.disc_copies ++
    Cs#cstruct.ram_copies.

dist_coredump() ->
    dist_coredump(all_nodes()).
dist_coredump(Ns) ->
    {Replies, _} = rpc:multicall(Ns, ?MODULE, coredump, []),
    Replies.

coredump() ->
    coredump({crashinfo, {"user initiated~n", []}}).
coredump(CrashInfo) ->
    Core = mkcore(CrashInfo),
    Out = core_file(),
    important("Writing Mnesia core to file: ~p...~p~n", [Out, CrashInfo]),
    file:write_file(Out, Core),
    Out.

core_file() ->
    Integers = tuple_to_list(date()) ++ tuple_to_list(time()),
    Fun = fun(I) when I < 10 -> ["_0", I];
	     (I) -> ["_", I]
	  end,
    List = lists:append([Fun(I) || I <- Integers]),
    filename:absname(lists:concat(["MnesiaCore.", node()] ++ List)).

mkcore(CrashInfo) ->
%   dbg_out("Making a Mnesia core dump...~p~n", [CrashInfo]),
    Nodes = [node() |nodes()],
    TidLocks = (catch ets:tab2list(mnesia_tid_locks)),
    Core = [
	    CrashInfo,
	    {time, {date(), time()}},
	    {self, catch process_info(self())},
	    {nodes, catch rpc:multicall(Nodes, ?MODULE, get_node_number, [])},
	    {applications, catch lists:sort(application:loaded_applications())},
	    {flags, catch init:get_arguments()},
	    {code_path, catch code:get_path()},
	    {code_loaded, catch lists:sort(code:all_loaded())},
	    {etsinfo, catch ets_info(ets:all())},

	    {version, catch mnesia:system_info(version)},
	    {schema, catch ets:tab2list(schema)},
	    {gvar, catch ets:tab2list(mnesia_gvar)},
	    {master_nodes, catch mnesia_recover:get_master_node_info()},

	    {processes, catch procs()},
	    {relatives, catch relatives()},
	    {workers, catch workers(mnesia_controller:get_workers(2000))},
	    {locking_procs, catch locking_procs(TidLocks)},

	    {held_locks, catch mnesia:system_info(held_locks)},
	    {tid_locks, TidLocks},
	    {lock_queue, catch mnesia:system_info(lock_queue)},
	    {load_info, catch mnesia_controller:get_info(2000)},
	    {trans_info, catch mnesia_tm:get_info(2000)},

	    {schema_file, catch file:read_file(tab2dat(schema))},
	    {dir_info, catch dir_info()},
	    {logfile, catch {ok, read_log_files()}}
	   ],
    term_to_binary(Core).

procs() ->
    Fun = fun(P) -> {P, (catch lists:zf(fun proc_info/1, process_info(P)))} end,
    lists:map(Fun, processes()).

proc_info({registered_name, Val}) -> {true, Val};
proc_info({message_queue_len, Val}) -> {true, Val};
proc_info({status, Val}) -> {true, Val};
proc_info({current_function, Val}) -> {true, Val};
proc_info(_) -> false.

get_node_number() ->
    {node(), self()}.

read_log_files() ->
    [{F, catch file:read_file(F)} || F <- mnesia_log:log_files()].

dir_info() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = dir(),
    [{cwd, Cwd, file:read_file_info(Cwd)},
     {mnesia_dir, Dir, file:read_file_info(Dir)}] ++
    case file:list_dir(Dir) of
	{ok, Files} ->
	    [{mnesia_file, F, catch file:read_file_info(dir(F))} || F <- Files];
	Other ->
	    [Other]
    end.

ets_info([H|T]) ->
    [{table, H, ets:info(H)} | ets_info(T)];
ets_info([]) -> [].

relatives() ->
    Info = fun(Name) ->
		   case whereis(Name) of
		       undefined -> false;
		       Pid -> {true, {Name, Pid, catch process_info(Pid)}}
		   end
	   end,
    lists:zf(Info, mnesia:ms()).

workers({workers, Loader, Sender, Dumper}) ->
    Info = fun({Name, Pid}) ->
		   case Pid of
		       undefined -> false;
		       Pid -> {true, {Name, Pid, catch process_info(Pid)}}
		   end
	   end,
    lists:zf(Info, [{loader, Loader}, {sender, Sender}, {dumper, Dumper}]).

locking_procs(LockList) when list(LockList) ->
    Tids = [element(1, Lock) || Lock <- LockList],
    UT = uniq(Tids),
    Info = fun(Tid) ->
		   Pid = Tid#tid.pid,
		   case node(Pid) == node() of
		       true ->
			   {true, {Pid, catch process_info(Pid)}};
		       _ ->
			   false
		   end
	   end,
    lists:zf(Info, UT).

view() ->
    Bin = mkcore({crashinfo, {"view only~n", []}}),
    vcore(Bin).

%% Displays a Mnesia file on the tty. The file may be repaired.
view(File) ->
    case suffix([".DAT", ".RET", ".DMP", ".TMP"], File) of
	true ->
	    view(File, dat);
	false ->
	    case suffix([".LOG", ".BUP", ".ETS"], File) of
		true ->
		    view(File, log);
		false ->
		    case lists:prefix("MnesiaCore.", File) of
			true ->
			    view(File, core);
			false ->
			    {error, "Unknown file name"}
		    end
	    end
    end.

view(File, dat) ->
    dets:view(File);
view(File, log) ->
    mnesia_log:view(File);
view(File, core) ->
    vcore(File).

suffix(Suffixes, File) ->
    Fun = fun(S) -> lists:suffix(S, File) end,
    lists:any(Fun, Suffixes).

%% View a core file

vcore() ->
    Prefix = lists:concat(["MnesiaCore.", node()]),
    Filter = fun(F) -> lists:prefix(Prefix, F) end,
    {ok, Cwd} = file:get_cwd(),
    case file:list_dir(Cwd) of
	{ok, Files}->
	    CoreFiles = lists:sort(lists:zf(Filter, Files)),
	    show("Mnesia core files: ~p~n", [CoreFiles]),
	    vcore(lists:last(CoreFiles));
	Error ->
	    Error
    end.

vcore(Bin) when binary(Bin) ->
    Core = binary_to_term(Bin),
    Fun = fun({Item, Info}) ->
		  show("***** ~p *****~n", [Item]),
		  case catch vcore_elem({Item, Info}) of
		      {'EXIT', Reason} ->
			  show("{'EXIT', ~p}~n", [Reason]);
		      _ -> ok
		  end
	  end,
    lists:foreach(Fun, Core);

vcore(File) ->
    show("~n***** Mnesia core: ~p *****~n", [File]),
    case file:read_file(File) of
	{ok, Bin} ->
	    vcore(Bin);
	_ ->
	    nocore
    end.

vcore_elem({schema_file, {ok, B}}) ->
    Fname = "/tmp/schema.DAT",
    file:write_file(Fname, B),
    dets:view(Fname),
    file:delete(Fname);

vcore_elem({logfile, {ok, BinList}}) ->
    Fun = fun({F, Info}) ->
		  show("----- logfile: ~p -----~n", [F]),
		  case Info of
		      {ok, B} ->
			  Fname = "/tmp/mnesia_vcore_elem.TMP",
			  file:write_file(Fname, B),
			  mnesia_log:view(Fname),
			  file:delete(Fname);
		      _ ->
			  show("~p~n", [Info])
		  end
	  end,
    lists:foreach(Fun, BinList);

vcore_elem({crashinfo, {Format, Args}}) ->
    show(Format, Args);
vcore_elem({gvar, L}) ->
    show("~p~n", [lists:sort(L)]);
vcore_elem({transactions, Info}) ->
    mnesia_tm:display_info(user, Info);

vcore_elem({_Item, Info}) ->
    show("~p~n", [Info]).

fix_error(X) ->
    set(last_error, X), %% for debugabililty
    case X of
	{aborted, Reason} -> Reason;
	{abort, Reason} -> Reason;
	Y when atom(Y) -> Y;
	{'EXIT', {_Reason, {Mod, _, _}}} when atom(Mod) ->
	    save(X),
	    case atom_to_list(Mod) of
		[$m, $n, $e|_] -> badarg;
		_ -> X
	    end;
	_ -> X
    end.

last_error() ->
    val(last_error).

%% The following is a list of possible mnesia errors and what they
%% actually mean

error_desc(nested_transaction) -> "Nested transactions are not allowed";
error_desc(badarg) -> "Bad or invalid argument, possibly bad type";
error_desc(no_transaction) -> "Operation not allowed outside transactions";
error_desc(combine_error)  -> "Table options were ilegally combined";
error_desc(bad_index)  -> "Index already exists or was out of bounds";
error_desc(already_exists) -> "Some schema option we try to set is already on";
error_desc(index_exists)-> "Some ops cannot  be performed on tabs with index";
error_desc(no_exists)-> "Tried to perform op on non-existing (non alive) item";
error_desc(system_limit) -> "Some system_limit was exhausted";
error_desc(mnesia_down) -> "A transaction involving objects at some remote "
                           "node which died while transaction was executing"
                           "*and* object(s) are no longer available elsewhere"
                           "in the network";
error_desc(not_a_db_node) -> "A node which is non existant in "
                              "the schema was mentioned";
error_desc(bad_type)            -> "Bad type on some provided arguments";
error_desc(node_not_running)    -> "Node not running";
error_desc(truncated_binary_file) -> "Truncated binary in file";
error_desc(active)     -> "Some delete ops require that "
                           "all active objects are removed";
error_desc(illegal) -> "Operation not supported on object";
error_desc({'EXIT', Reason}) ->
    error_desc(Reason);
error_desc({error, Reason}) ->
    error_desc(Reason);
error_desc({aborted, Reason}) ->
    error_desc(Reason);
error_desc(Reason) when tuple(Reason), size(Reason) > 0 ->
    setelement(1, Reason, error_desc(element(1, Reason)));
error_desc(Reason) ->
    Reason.

dirty_rpc_error_tag(Reason) ->
    case Reason of
	{'EXIT', _} -> badarg;
	no_variable -> badarg;
	_           -> no_exists
    end.

fatal(Format, Args) ->
    catch set(mnesia_status, stopping),
    Core = mkcore({crashinfo, {Format, Args}}),
    report_fatal(Format, Args, Core),
    timer:sleep(10000), % Enough to write the core dump to disc?
    mnesia:lkill(),
    exit(fatal).

report_fatal(Format, Args) ->
    report_fatal(Format, Args, nocore).

report_fatal(Format, Args, Core) ->
    report_system_event({mnesia_fatal, Format, Args, Core}),
    catch exit(whereis(mnesia_monitor), fatal).

%% We sleep longer and longer the more we try
%% Made some testing and came up with the following constants
random_time(Retries, _Counter0) ->
%    UpperLimit = 2000,
%    MaxIntv = trunc(UpperLimit * (1-(4/((Retries*Retries)+4)))),
    UpperLimit = 500,
    Dup = Retries * Retries,
    MaxIntv = trunc(UpperLimit * (1-(50/((Dup)+50)))),

    case get(random_seed) of
	undefined ->
	    {X, Y, Z} = erlang:now(), %% time()
	    random:seed(X, Y, Z),
	    Time = Dup + random:uniform(MaxIntv),
	    %%	    dbg_out("---random_test rs ~w max ~w val ~w---~n", [Retries, MaxIntv, Time]),
	    Time;
	_ ->
	    Time = Dup + random:uniform(MaxIntv),
	    %%	    dbg_out("---random_test rs ~w max ~w val ~w---~n", [Retries, MaxIntv, Time]),
	    Time
    end.

report_system_event(Event0) ->
    Event = {mnesia_system_event, Event0},
    report_system_event(catch_notify(Event), Event),
    case ?catch_val(subscribers) of
	{'EXIT', _} -> ignore;
	Pids -> lists:foreach(fun(Pid) -> Pid ! Event end, Pids)
    end,
    ok.

catch_notify(Event) ->
    case whereis(mnesia_event) of
	undefined ->
	    {'EXIT', {badarg, {mnesia_event, Event}}};
	Pid ->
	    gen_event:notify(Pid, Event)
    end.

report_system_event({'EXIT', Reason}, Event) ->
    Mod = mnesia_monitor:get_env(event_module),
    case mnesia_sup:start_event() of
	{ok, Pid} ->
	    link(Pid),
	    gen_event:call(mnesia_event, Mod, Event, infinity),
	    unlink(Pid),

            %% We get an exit signal if server dies
            receive
                {'EXIT', Pid, _Reason} ->
                    {error, {node_not_running, node()}}
            after 0 ->
		    gen_event:stop(mnesia_event),
                    ok
            end;

	Error ->
	    Msg = "Mnesia(~p): Cannot report event ~p: ~p (~p)~n",
	    error_logger:format(Msg, [node(), Event, Reason, Error])
    end;
report_system_event(_Res, _Event) ->
    ignore.

%% important messages are reported regardless of debug level
important(Format, Args) ->
    save({Format, Args}),
    report_system_event({mnesia_info, Format, Args}).

%% Warning messages are reported regardless of debug level
warning(Format, Args) ->
    save({Format, Args}),
    report_system_event({mnesia_warning, Format, Args}).

%% error messages are reported regardless of debug level
error(Format, Args) ->
    save({Format, Args}),
    report_system_event({mnesia_error, Format, Args}).

%% verbose messages are reported if debug level == debug or verbose
verbose(Format, Args) ->
    case mnesia_monitor:get_env(debug) of
	none ->    save({Format, Args});
	verbose -> important(Format, Args);
	debug ->   important(Format, Args);
	trace ->   important(Format, Args)
    end.

%% debug message are display if debug level == 2
dbg_out(Format, Args) ->
    case mnesia_monitor:get_env(debug) of
	none ->    ignore;
	verbose -> save({Format, Args});
	_ ->  report_system_event({mnesia_info, Format, Args})
    end.

%% Keep the last 10 debug print outs
save(DbgInfo) ->
    catch save2(DbgInfo).

save2(DbgInfo) ->
    Key = {'$$$_report', current_pos},
    P =
	case ?ets_lookup_element(mnesia_gvar, Key, 2) of
	    30 -> -1;
	    I -> I
	end,
    set({'$$$_report', current_pos}, P+1),
    set({'$$$_report', P+1}, {date(), time(), DbgInfo}).

copy_file(From, To) ->
    case file:open(From, [raw, binary, read]) of
	{ok, F} ->
	    case file:open(To, [raw, binary, write]) of
		{ok, T} ->
		    Res = copy_file_loop(F, T, 8000),
		    file:close(F),
		    file:close(T),
		    Res;
		{error, Reason} ->
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

copy_file_loop(F, T, ChunkSize) ->
    case file:read(F, ChunkSize) of
	{ok, {0, _}} ->
	    ok;
	{ok, {_, Bin}} ->
	    file:write(T, Bin),
	    copy_file_loop(F, T, ChunkSize);
	{ok, Bin} ->
	    file:write(T, Bin),
	    copy_file_loop(F, T, ChunkSize);
	eof ->
	    ok;
	{error, Reason} ->
	    {error, Reason}
    end.


%%%%%%%%%%%%
%% versions of all the lowlevel db funcs that determine whether we
%% shall go to disc or ram to do the actual operation.

db_get(Tab, Key) ->
    db_get(val({Tab, storage_type}), Tab, Key).
db_get(ram_copies, Tab, Key) -> ?ets_lookup(Tab, Key);
db_get(disc_copies, Tab, Key) -> ?ets_lookup(Tab, Key);
db_get(disc_only_copies, Tab, Key) -> dets:lookup(Tab, Key).

db_init_chunk(Tab) ->
    db_init_chunk(val({Tab, storage_type}), Tab, 1000).
db_init_chunk(Tab, N) ->
    db_init_chunk(val({Tab, storage_type}), Tab, N).

db_init_chunk(disc_only_copies, Tab, N) ->
    dets:select(Tab, [{'_', [], ['$_']}], N);
db_init_chunk(_, Tab, N) ->
    ets:select(Tab, [{'_', [], ['$_']}], N).

db_chunk(disc_only_copies, State) ->
    dets:select(State);
db_chunk(_, State) ->
    ets:select(State).

db_put(Tab, Val) ->
    db_put(val({Tab, storage_type}), Tab, Val).

db_put(ram_copies, Tab, Val) -> ?ets_insert(Tab, Val), ok;
db_put(disc_copies, Tab, Val) -> ?ets_insert(Tab, Val), ok;
db_put(disc_only_copies, Tab, Val) -> dets:insert(Tab, Val).

db_match_object(Tab, Pat) ->
    db_match_object(val({Tab, storage_type}), Tab, Pat).
db_match_object(Storage, Tab, Pat) ->
    db_fixtable(Storage, Tab, true),
    Res = catch_match_object(Storage, Tab, Pat),
    db_fixtable(Storage, Tab, false),
    case Res of
	{'EXIT', Reason} -> exit(Reason);
	_ -> Res
    end.

catch_match_object(disc_only_copies, Tab, Pat) ->
    catch dets:match_object(Tab, Pat);
catch_match_object(_, Tab, Pat) ->
    catch ets:match_object(Tab, Pat).

db_select(Tab, Pat) ->
    db_select(val({Tab, storage_type}), Tab, Pat).

db_select(Storage, Tab, Pat) ->
    db_fixtable(Storage, Tab, true),
    Res = catch_select(Storage, Tab, Pat),
    db_fixtable(Storage, Tab, false),
    case Res of
	{'EXIT', Reason} -> exit(Reason);
	_ -> Res
    end.

catch_select(disc_only_copies, Tab, Pat) ->
    dets:select(Tab, Pat);
catch_select(_, Tab, Pat) ->
    ets:select(Tab, Pat).

db_fixtable(ets, Tab, Bool) ->
    ets:safe_fixtable(Tab, Bool);
db_fixtable(ram_copies, Tab, Bool) ->
    ets:safe_fixtable(Tab, Bool);
db_fixtable(disc_copies, Tab, Bool) ->
    ets:safe_fixtable(Tab, Bool);
db_fixtable(dets, Tab, Bool) ->
    dets:safe_fixtable(Tab, Bool);
db_fixtable(disc_only_copies, Tab, Bool) ->
    dets:safe_fixtable(Tab, Bool).

db_erase(Tab, Key) ->
    db_erase(val({Tab, storage_type}), Tab, Key).
db_erase(ram_copies, Tab, Key) -> ?ets_delete(Tab, Key), ok;
db_erase(disc_copies, Tab, Key) -> ?ets_delete(Tab, Key), ok;
db_erase(disc_only_copies, Tab, Key) -> dets:delete(Tab, Key).

db_match_erase(Tab, Pat) ->
    db_match_erase(val({Tab, storage_type}), Tab, Pat).
db_match_erase(ram_copies, Tab, Pat) -> ?ets_match_delete(Tab, Pat), ok;
db_match_erase(disc_copies, Tab, Pat) -> ?ets_match_delete(Tab, Pat), ok;
db_match_erase(disc_only_copies, Tab, Pat) -> dets:match_delete(Tab, Pat).

db_first(Tab) ->
    db_first(val({Tab, storage_type}), Tab).
db_first(ram_copies, Tab) -> ?ets_first(Tab);
db_first(disc_copies, Tab) -> ?ets_first(Tab);
db_first(disc_only_copies, Tab) -> dets:first(Tab).

db_next_key(Tab, Key) ->
    db_next_key(val({Tab, storage_type}), Tab, Key).
db_next_key(ram_copies, Tab, Key) -> ?ets_next(Tab, Key);
db_next_key(disc_copies, Tab, Key) -> ?ets_next(Tab, Key);
db_next_key(disc_only_copies, Tab, Key) -> dets:next(Tab, Key).

db_last(Tab) ->
    db_last(val({Tab, storage_type}), Tab).
db_last(ram_copies, Tab) -> ?ets_last(Tab);
db_last(disc_copies, Tab) -> ?ets_last(Tab);
db_last(disc_only_copies, Tab) -> dets:first(Tab). %% Dets don't have order

db_prev_key(Tab, Key) ->
    db_prev_key(val({Tab, storage_type}), Tab, Key).
db_prev_key(ram_copies, Tab, Key) -> ?ets_prev(Tab, Key);
db_prev_key(disc_copies, Tab, Key) -> ?ets_prev(Tab, Key);
db_prev_key(disc_only_copies, Tab, Key) -> dets:next(Tab, Key). %% Dets don't have order

db_slot(Tab, Pos) ->
    db_slot(val({Tab, storage_type}), Tab, Pos).
db_slot(ram_copies, Tab, Pos) -> ?ets_slot(Tab, Pos);
db_slot(disc_copies, Tab, Pos) -> ?ets_slot(Tab, Pos);
db_slot(disc_only_copies, Tab, Pos) -> dets:slot(Tab, Pos).

db_update_counter(Tab, C, Val) ->
    db_update_counter(val({Tab, storage_type}), Tab, C, Val).
db_update_counter(ram_copies, Tab, C, Val) ->
    ?ets_update_counter(Tab, C, Val);
db_update_counter(disc_copies, Tab, C, Val) ->
    ?ets_update_counter(Tab, C, Val);
db_update_counter(disc_only_copies, Tab, C, Val) ->
    dets:update_counter(Tab, C, Val).

db_erase_tab(Tab) ->
    db_erase_tab(val({Tab, storage_type}), Tab).
db_erase_tab(ram_copies, Tab) -> ?ets_delete_table(Tab);
db_erase_tab(disc_copies, Tab) -> ?ets_delete_table(Tab);
db_erase_tab(disc_only_copies, _Tab) -> ignore.

%% assuming that Tab is a valid ets-table
dets_to_ets(Tabname, Tab, File, Type, Rep, Lock) ->
    {Open, Close} = mkfuns(Lock),
    case Open(Tabname, [{file, File}, {type, disk_type(Tab, Type)},
			{keypos, 2}, {repair, Rep}]) of
	{ok, Tabname} ->
	    Res = dets:to_ets(Tabname, Tab),
	    Close(Tabname),
	    trav_ret(Res, Tab);
	Other ->
	    Other
    end.

trav_ret(Tabname, Tabname) -> loaded;
trav_ret(Other, _Tabname) -> Other.

mkfuns(yes) ->
    {fun(Tab, Args) -> dets_sync_open(Tab, Args) end,
     fun(Tab) -> dets_sync_close(Tab) end};
mkfuns(no) ->
    {fun(Tab, Args) -> dets:open_file(Tab, Args) end,
     fun(Tab) -> dets:close(Tab) end}.

disk_type(Tab) ->
    disk_type(Tab, val({Tab, setorbag})).

disk_type(_Tab, ordered_set) ->
    set;
disk_type(_, Type) ->
    Type.

dets_sync_open(Tab, Ref, File) ->
    Args = [{file, File},
	    {keypos, 2},
	    {repair, mnesia_monitor:get_env(auto_repair)},
	    {type, disk_type(Tab)}],
    dets_sync_open(Ref, Args).

lock_table(Tab) ->
    global:set_lock({{mnesia_table_lock, Tab}, self()}, [node()], infinity).
%    dbg_out("dets_sync_open: ~p ~p~n", [T, self()]),

unlock_table(Tab) ->
    global:del_lock({{mnesia_table_lock, Tab}, self()}, [node()]).
%    dbg_out("unlock_table: ~p ~p~n", [T, self()]),

dets_sync_open(Tab, Args) ->
    lock_table(Tab),
    case dets:open_file(Tab, Args) of
	{ok, Tab} ->
	    {ok, Tab};
	Other ->
	    dets_sync_close(Tab),
	    Other
    end.

dets_sync_close(Tab) ->
    catch dets:close(Tab),
    unlock_table(Tab),
    ok.

cleanup_tmp_files([Tab | Tabs]) ->
    dets_sync_close(Tab),
    file:delete(tab2tmp(Tab)),
    cleanup_tmp_files(Tabs);
cleanup_tmp_files([]) ->
    ok.

%% Returns a list of bad tables
swap_tmp_files([Tab | Tabs]) ->
    dets_sync_close(Tab),
    Tmp = tab2tmp(Tab),
    Dat = tab2dat(Tab),
    case file:rename(Tmp, Dat) of
	ok ->
	    swap_tmp_files(Tabs);
	_ ->
	    file:delete(Tmp),
	    [Tab | swap_tmp_files(Tabs)]
    end;
swap_tmp_files([]) ->
    [].

readable_indecies(Tab) ->
    val({Tab, index}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Managing conditional debug functions
%%
%% The main idea with the debug_fun's is to allow test programs
%% to control the internal behaviour of Mnesia. This is needed
%% to make the test programs independent of system load, swapping
%% and other circumstances that may affect the behaviour of Mnesia.
%%
%% First should calls to ?eval_debug_fun be inserted at well
%% defined places in Mnesia's code. E.g. in critical situations
%% of startup, transaction commit, backups etc.
%%
%% Then compile Mnesia with the compiler option 'debug'.
%%
%% In test programs ?activate_debug_fun should be called
%% in order to bind a fun to the debug identifier stated
%% in the call to ?eval_debug_fun.
%%
%% If eval_debug_fun finds that the fun is activated it
%% invokes the fun as NewContext = Fun(PreviousContext, EvalContext)
%% and replaces the PreviousContext with the NewContext.
%% The initial context of a debug_fun is given as argument to
%% activate_debug_fun.

-define(DEBUG_TAB, mnesia_debug).
-record(debug_info, {id, function, context, file, line}).

scratch_debug_fun() ->
    dbg_out("scratch_debug_fun(): ~p~n", [?DEBUG_TAB]),
    (catch ?ets_delete_table(?DEBUG_TAB)),
    ?ets_new_table(?DEBUG_TAB, [set, public, named_table, {keypos, 2}]).

activate_debug_fun(FunId, Fun, InitialContext, File, Line) ->
    Info = #debug_info{id = FunId,
		       function = Fun,
		       context = InitialContext,
		       file = File,
		       line = Line
		      },
    update_debug_info(Info).

update_debug_info(Info) ->
    case catch ?ets_insert(?DEBUG_TAB, Info) of
	{'EXIT', _} ->
	    scratch_debug_fun(),
	    ?ets_insert(?DEBUG_TAB, Info);
	_ ->
	    ok
    end,
    dbg_out("update_debug_info(~p)~n", [Info]),
    ok.

deactivate_debug_fun(FunId, _File, _Line) ->
    catch ?ets_delete(?DEBUG_TAB, FunId),
    ok.

eval_debug_fun(FunId, EvalContext, EvalFile, EvalLine) ->
    case catch ?ets_lookup(?DEBUG_TAB, FunId) of
	[] ->
	    ok;
	[Info] ->
	    OldContext = Info#debug_info.context,
	    dbg_out("~s(~p): ~w "
		    "activated in ~s(~p)~n  "
		    "eval_debug_fun(~w, ~w)~n",
		    [filename:basename(EvalFile), EvalLine, Info#debug_info.id,
		     filename:basename(Info#debug_info.file), Info#debug_info.line,
		     OldContext, EvalContext]),
	    Fun = Info#debug_info.function,
	    NewContext = Fun(OldContext, EvalContext),

	    case catch ?ets_lookup(?DEBUG_TAB, FunId) of
		[Info] when NewContext /= OldContext ->
		    NewInfo = Info#debug_info{context = NewContext},
		    update_debug_info(NewInfo);
		_ ->
		    ok
	    end;
	{'EXIT', _} -> ok
    end.

-ifdef(debug).
    is_debug_compiled() -> true.
-else.
    is_debug_compiled() -> false.
-endif.
