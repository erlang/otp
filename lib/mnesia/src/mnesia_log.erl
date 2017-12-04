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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% This module administers three kinds of log files:
%%
%% 1 The transaction log
%%   mnesia_tm appends to the log (via mnesia_log) at the
%%   end of each transaction (or dirty write) and
%%   mnesia_dumper reads the log and performs the ops in
%%   the dat files. The dump_log is done  at startup and
%%   at intervals controlled by the user.
%%
%% 2 The mnesia_down log
%%   mnesia_tm appends to the log (via mnesia_log) when it
%%   realizes that mnesia goes up or down on another node.
%%   mnesia_init reads the log (via mnesia_log) at startup.
%%
%% 3 The backup log
%%   mnesia_schema produces one tiny log when the schema is
%%   initially created. mnesia_schema also reads the log
%%   when the user wants tables (possibly incl the schema)
%%   to be restored. mnesia_log appends to the log when the
%%   user wants to produce a real backup.
%%
%%   The actual access to the backup media is performed via the
%%   mnesia_backup module for both read and write. mnesia_backup
%%   uses the disk_log (*), BUT the user may write an own module
%%   with the same interface as mnesia_backup and configure
%%   Mnesia so the alternate module performs the actual accesses
%%   to the backup media. This means that the user may put the
%%   backup on medias that Mnesia does not know about possibly on
%%   hosts where Erlang is not running.
%%
%% All these logs have to some extent a common structure.
%% They are all using the disk_log module (*) for the basic
%% file structure. The disk_log has a repair feature that
%% can be used to skip erroneous log records if one comes to
%% the conclusion that it is more important to reuse some
%% of the log records than the risque of obtaining inconsistent
%% data. If the data becomes inconsistent it is solely up to the
%% application to make it consistent again. The automatic
%% reparation of the disk_log is very powerful, but use it
%% with extreme care.
%%
%% First in all Mnesia's log file is a mnesia log header.
%% It contains a list with a log_header record as single
%% element. The structure of the log_header may never be
%% changed since it may be written to very old backup files.
%% By holding this record definition stable we can be
%% able to comprahend backups from timepoint 0. It also
%% allows us to use the backup format as an interchange
%% format between Mnesia releases.
%%
%% An op-list is a list of tuples with arity 3. Each tuple
%% has this structure: {Oid, Recs, Op} where Oid is the tuple
%% {Tab, Key}, Recs is a (possibly empty) list of records and
%% Op is an atom.
%%
%% The log file structure for the transaction log is as follows.
%%
%%    After the mnesia log section follows an extended record section
%%    containing op-lists. There are several values that Op may
%%    have, such as write, delete, update_counter, delete_object,
%%    and replace. There is no special end of section marker.
%%
%%    +-----------------+
%%    | mnesia log head |
%%    +-----------------+
%%    | extended record |
%%    | section         |
%%    +-----------------+
%%
%% The log file structure for the mnesia_down log is as follows.
%%
%%    After the mnesia log section follows a mnesia_down section
%%    containg lists with yoyo records as single element.
%%
%%    +-----------------+
%%    | mnesia log head |
%%    +-----------------+
%%    | mnesia_down     |
%%    | section         |
%%    +-----------------+
%%
%% The log file structure for the backup log is as follows.
%%
%%    After the mnesia log section follows a schema section
%%    containing record lists. A record list is a list of tuples
%%    where {schema, Tab} is interpreted as a delete_table(Tab) and
%%    {schema, Tab, CreateList} are interpreted as create_table.
%%
%%    The record section also contains record lists. In this section
%%    {Tab, Key} is interpreted as delete({Tab, Key}) and other tuples
%%    as write(Tuple). There is no special end of section marker.
%%
%%    +-----------------+
%%    | mnesia log head |
%%    +-----------------+
%%    | schema section  |
%%    +-----------------+
%%    | record section  |
%%    +-----------------+
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(mnesia_log).

-export([
	 append/2,
	 backup/1,
	 backup/2,
	 backup_checkpoint/2,
	 backup_checkpoint/3,
	 backup_log_header/0,
	 backup_master/2,
	 chunk_decision_log/1,
	 chunk_decision_tab/1,
	 chunk_log/1,
	 chunk_log/2,
	 close_decision_log/0,
	 close_decision_tab/0,
	 close_log/1,
	 unsafe_close_log/1,
	 confirm_log_dump/1,
	 confirm_decision_log_dump/0,
	 previous_log_file/0,
	 previous_decision_log_file/0,
	 latest_log_file/0,
	 decision_log_version/0,
	 decision_log_file/0,
	 decision_tab_file/0,
	 decision_tab_version/0,
	 dcl_version/0,
	 dcd_version/0,
	 ets2dcd/1,
	 ets2dcd/2,
	 dcd2ets/1,
	 dcd2ets/2,
	 init/0,
	 init_log_dump/0,
	 log/1,
	 slog/1,
	 log_decision/1,
	 log_files/0,
	 open_decision_log/0,
	 trans_log_header/0,
	 open_decision_tab/0,
	 dcl_log_header/0,
	 dcd_log_header/0,
	 open_log/4,
	 open_log/6,
	 prepare_decision_log_dump/0,
	 prepare_log_dump/1,
	 save_decision_tab/1,
	 purge_all_logs/0,
	 purge_some_logs/0,
	 stop/0,
	 tab_copier/3,
	 version/0,
	 view/0,
	 view/1,
	 write_trans_log_header/0
	]).


-compile({no_auto_import,[error/2]}).

-include("mnesia.hrl").
-import(mnesia_lib, [val/1, dir/1]).
-import(mnesia_lib, [exists/1, fatal/2, error/2, dbg_out/2]).

trans_log_header() -> log_header(trans_log, version()).
backup_log_header() -> log_header(backup_log, "1.2").
decision_log_header() -> log_header(decision_log, decision_log_version()).
decision_tab_header() -> log_header(decision_tab, decision_tab_version()).
dcl_log_header() -> log_header(dcl_log, dcl_version()).
dcd_log_header() -> log_header(dcd_log, dcd_version()).

log_header(Kind, Version) ->
    #log_header{log_version=Version,
		log_kind=Kind,
		mnesia_version=mnesia:system_info(version),
		node=node(),
		now=erlang:timestamp()}.

version() -> "4.3".

decision_log_version() -> "3.0".

decision_tab_version() -> "1.0".

dcl_version() -> "1.0".
dcd_version() -> "1.0".

append(Log, Bin) when is_binary(Bin) ->
    disk_log:balog(Log, Bin);
append(Log, Term) ->
    disk_log:alog(Log, Term).

%% Synced append
sappend(Log, Bin) when is_binary(Bin) ->
    ok = disk_log:blog(Log, Bin);
sappend(Log, Term) ->
    ok = disk_log:log(Log, Term).

%% Write commit records to the latest_log
log(C) ->
    case need_log(C) andalso mnesia_monitor:use_dir() of
        true ->
	    if
		is_record(C, commit) ->
		    append(latest_log, strip_snmp(C));
		true ->
		    %% Either a commit record as binary
		    %% or some decision related info
		    append(latest_log, C)
	    end,
	    mnesia_dumper:incr_log_writes();
	false ->
	    ignore
    end.

%% Synced

slog(C) ->
    case need_log(C) andalso mnesia_monitor:use_dir() of
        true ->
	    if
		is_record(C, commit) ->
		    sappend(latest_log, strip_snmp(C));
		true ->
		    %% Either a commit record as binary
		    %% or some decision related info
		    sappend(latest_log, C)
	    end,
	    mnesia_dumper:incr_log_writes();
	false ->
	    ignore
    end.

need_log(#commit{disc_copies=[], disc_only_copies=[], schema_ops=[], ext=Ext}) ->
    lists:keymember(ext_copies, 1, Ext);
need_log(_) -> true.

strip_snmp(#commit{ext=[]}=CR) -> CR;
strip_snmp(#commit{ext=Ext}=CR) ->
    CR#commit{ext=lists:keydelete(snmp, 1, Ext)}.

%% Stuff related to the file LOG

%% Returns a list of logfiles. The oldest is first.
log_files() -> [previous_log_file(),
		latest_log_file(),
		decision_tab_file()
	       ].

latest_log_file() -> dir(latest_log_name()).

previous_log_file() -> dir("PREVIOUS.LOG").

decision_log_file() -> dir(decision_log_name()).

decision_tab_file() -> dir(decision_tab_name()).

previous_decision_log_file() -> dir("PDECISION.LOG").

latest_log_name() -> "LATEST.LOG".

decision_log_name() -> "DECISION.LOG".

decision_tab_name() -> "DECISION_TAB.LOG".

init() ->
    case mnesia_monitor:use_dir() of
	true ->
	    Prev = previous_log_file(),
	    verify_no_exists(Prev),

	    Latest = latest_log_file(),
	    verify_no_exists(Latest),

	    Header = trans_log_header(),
	    open_log(latest_log, Header, Latest);
	false ->
	    ok
    end.

verify_no_exists(Fname) ->
    case exists(Fname) of
	false ->
	    ok;
	true ->
	    fatal("Log file exists: ~tp~n", [Fname])
    end.

open_log(Name, Header, Fname) ->
    Exists = exists(Fname),
    open_log(Name, Header, Fname, Exists).

open_log(Name, Header, Fname, Exists) ->
    Repair = mnesia_monitor:get_env(auto_repair),
    open_log(Name, Header, Fname, Exists, Repair).

open_log(Name, Header, Fname, Exists, Repair) ->
    case Name == previous_log of
	true ->
	    open_log(Name, Header, Fname, Exists, Repair, read_only);
	false ->
	    open_log(Name, Header, Fname, Exists, Repair, read_write)
    end.

open_log(Name, Header, Fname, Exists, Repair, Mode) ->
    Args = [{file, Fname}, {name, Name}, {repair, Repair}, {mode, Mode}],
%%    io:format("~p:open_log: ~tp ~tp~n", [?MODULE, Name, Fname]),
    case mnesia_monitor:open_log(Args) of
	{ok, Log} when Exists == true ->
	    Log;
	{ok, Log} ->
	    write_header(Log, Header),
	    Log;
	{repaired, Log, _, {badbytes, 0}} when Exists == true ->
	    Log;
	{repaired, Log, _, {badbytes, 0}} ->
	    write_header(Log, Header),
	    Log;
	{repaired, Log, _Recover, BadBytes} ->
	    mnesia_lib:important("Data may be missing, log ~tp repaired: Lost ~p bytes~n",
				 [Fname, BadBytes]),
	    Log;
	{error, Reason = {file_error, _Fname, emfile}} ->
	    fatal("Cannot open log file ~tp: ~tp~n", [Fname, Reason]);
	{error, Reason} when Repair == true ->
	    file:delete(Fname),
	    mnesia_lib:important("Data may be missing, Corrupt logfile deleted: ~tp, ~tp ~n",
				 [Fname, Reason]),
	    %% Create a new
	    open_log(Name, Header, Fname, false, false, read_write);
	{error, Reason} ->
	    fatal("Cannot open log file ~tp: ~tp~n", [Fname, Reason])
    end.

write_header(Log, Header) ->
    append(Log, Header).

write_trans_log_header() ->
    write_header(latest_log, trans_log_header()).

stop() ->
    case mnesia_monitor:use_dir() of
        true ->
	    close_log(latest_log);
	false ->
	    ok
    end.

close_log(Log) ->
%%    io:format("mnesia_log:close_log ~p~n", [Log]),
%%    io:format("mnesia_log:close_log ~p~n", [Log]),
    case disk_log:sync(Log) of
	ok -> ok;
	{error, {read_only_mode, Log}} ->
	    ok;
	{error, Reason} ->
	    mnesia_lib:important("Failed syncing ~tp to_disk reason ~tp ~n",
				 [Log, Reason])
    end,
    mnesia_monitor:close_log(Log).

unsafe_close_log(Log) ->
%%    io:format("mnesia_log:close_log ~p~n", [Log]),
    mnesia_monitor:unsafe_close_log(Log).


purge_some_logs() ->
    mnesia_monitor:unsafe_close_log(latest_log),
    _ = file:delete(latest_log_file()),
    _ = file:delete(decision_tab_file()),
    ok.

purge_all_logs() ->
    _ = file:delete(previous_log_file()),
    _ = file:delete(latest_log_file()),
    _ = file:delete(decision_tab_file()),
    ok.

%% Prepare dump by renaming the open logfile if possible
%% Returns a tuple on the following format: {Res, OpenLog}
%% where OpenLog is the file descriptor to log file, ready for append
%% and Res is one of the following: already_dumped, needs_dump or {error, Reason}
prepare_log_dump(InitBy) ->
    Diff = mnesia_dumper:get_log_writes() -
           mnesia_lib:read_counter(trans_log_writes_prev),
    if
	Diff == 0, InitBy /= startup ->
	    already_dumped;
	true ->
	    case mnesia_monitor:use_dir() of
		true ->
		    Prev = previous_log_file(),
		    prepare_prev(Diff, InitBy, Prev, exists(Prev));
		false ->
		    already_dumped
	    end
    end.

prepare_prev(Diff, _, _, true) ->
    {needs_dump, Diff};
prepare_prev(Diff, startup, Prev, false) ->
    Latest = latest_log_file(),
    case exists(Latest) of
	true ->
	    case file:rename(Latest, Prev) of
		ok ->
		    {needs_dump, Diff};
		{error, Reason} ->
		    {error, Reason}
	    end;
	false ->
	    already_dumped
    end;
prepare_prev(Diff, _InitBy, Prev, false) ->
    Head = trans_log_header(),
    case mnesia_monitor:reopen_log(latest_log, Prev, Head) of
	ok ->
	    {needs_dump, Diff};
	{error, Reason} ->
	    Latest = latest_log_file(),
	    {error, {"Cannot rename log file",
		     [Latest, Prev, Reason]}}
    end.

%% Init dump and return PrevLogFileDesc or exit.
init_log_dump() ->
    Fname = previous_log_file(),
    open_log(previous_log, trans_log_header(), Fname),
    start.


chunk_log(Cont) ->
    chunk_log(previous_log, Cont).

chunk_log(_Log, eof) ->
    eof;
chunk_log(Log, Cont) ->
    case disk_log:chunk(Log, Cont) of
	{error, Reason} ->
	    fatal("Possibly truncated ~tp file: ~tp~n",
		  [Log, Reason]);
	{C2, Chunk, _BadBytes} ->
	    %% Read_only case, should we warn about the bad log file?
	    %% BUGBUG Should we crash if Repair == false ??
	    %% We got to check this !!
	    mnesia_lib:important("~tp repaired, lost ~p bad bytes~n", [Log, _BadBytes]),
	    {C2, Chunk};
	Other ->
	    Other
    end.

%% Confirms the dump by closing prev log and delete the file
confirm_log_dump(Updates) ->
    case mnesia_monitor:close_log(previous_log) of
	ok ->
	    file:delete(previous_log_file()),
	    mnesia_lib:incr_counter(trans_log_writes_prev, Updates),
	    dumped;
	{error, Reason} ->
	    {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Decision log

open_decision_log() ->
    Latest = decision_log_file(),
    open_log(decision_log, decision_log_header(), Latest),
    start.

prepare_decision_log_dump() ->
    Prev = previous_decision_log_file(),
    prepare_decision_log_dump(exists(Prev), Prev).

prepare_decision_log_dump(false, Prev) ->
    Head = decision_log_header(),
    case mnesia_monitor:reopen_log(decision_log, Prev, Head) of
	ok ->
	    prepare_decision_log_dump(true, Prev);
	{error, Reason} ->
	    fatal("Cannot rename decision log file ~tp -> ~tp: ~tp~n",
		     [decision_log_file(), Prev, Reason])
    end;
prepare_decision_log_dump(true, Prev) ->
    open_log(previous_decision_log, decision_log_header(), Prev),
    start.

chunk_decision_log(Cont) ->
    %% dbg_out("chunk log ~p~n", [Cont]),
    chunk_log(previous_decision_log, Cont).

%% Confirms dump of the decision log
confirm_decision_log_dump() ->
    case mnesia_monitor:close_log(previous_decision_log) of
	ok ->
	    file:delete(previous_decision_log_file());
	{error, Reason} ->
	    fatal("Cannot confirm decision log dump: ~tp~n",
		  [Reason])
    end.

save_decision_tab(Decisions) ->
    Log = decision_tab,
    Tmp = mnesia_lib:dir("DECISION_TAB.TMP"),
    file:delete(Tmp),
    open_log(Log, decision_tab_header(), Tmp),
    append(Log, Decisions),
    close_log(Log),
    TabFile = decision_tab_file(),
    ok = file:rename(Tmp, TabFile).

open_decision_tab() ->
    TabFile = decision_tab_file(),
    open_log(decision_tab, decision_tab_header(), TabFile),
    start.

close_decision_tab() ->
    close_log(decision_tab).

chunk_decision_tab(Cont) ->
    %% dbg_out("chunk tab ~p~n", [Cont]),
    chunk_log(decision_tab, Cont).

close_decision_log() ->
    close_log(decision_log).

log_decision(Decision) ->
    append(decision_log, Decision).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Debug functions

view() ->
    lists:foreach(fun(F) -> view(F) end, log_files()).

view(File) ->
    mnesia_lib:show("*****  ~tp ***** ~n", [File]),
    case exists(File) of
	false ->
	    nolog;
	true ->
	    N = view_only,
	    Args = [{file, File}, {name, N}, {mode, read_only}],
	    case disk_log:open(Args) of
		{ok, N} ->
		    view_file(start, N);
		{repaired, _, _, _} ->
		    view_file(start, N);
		{error, Reason} ->
		    error("Cannot open log ~tp: ~tp~n", [File, Reason])
	    end
    end.

view_file(C, Log) ->
    case disk_log:chunk(Log, C) of
	{error, Reason} ->
	    error("** Possibly truncated FILE ~tp~n", [Reason]),
	    error;
	eof ->
	    disk_log:close(Log),
	    eof;
	{C2, Terms, _BadBytes} ->
	    dbg_out("Lost ~p bytes in ~tp ~n", [_BadBytes, Log]),
	    lists:foreach(fun(X) -> mnesia_lib:show("~tp~n", [X]) end,
			  Terms),
	    view_file(C2, Log);
	{C2, Terms} ->
	    lists:foreach(fun(X) -> mnesia_lib:show("~tp~n", [X]) end,
			  Terms),
	    view_file(C2, Log)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Backup

-record(backup_args, {name, module, opaque, scope, prev_name, tables, cookie}).

backup(Opaque) ->
    backup(Opaque, []).

backup(Opaque, Mod) when is_atom(Mod) ->
    backup(Opaque, [{module, Mod}]);
backup(Opaque, Args) when is_list(Args) ->
    %% Backup all tables with max redundancy
    CpArgs = [{ram_overrides_dump, false}, {max, val({schema, tables})}],
    case mnesia_checkpoint:activate(CpArgs) of
	{ok, Name, _Nodes} ->
	    Res = backup_checkpoint(Name, Opaque, Args),
	    mnesia_checkpoint:deactivate(Name),
	    Res;
	{error, Reason} ->
	    {error, Reason}
    end.

backup_checkpoint(Name, Opaque) ->
    backup_checkpoint(Name, Opaque, []).

backup_checkpoint(Name, Opaque, Mod) when is_atom(Mod) ->
    backup_checkpoint(Name, Opaque, [{module, Mod}]);
backup_checkpoint(Name, Opaque, Args) when is_list(Args) ->
    DefaultMod = mnesia_monitor:get_env(backup_module),
    B = #backup_args{name = Name,
		     module = DefaultMod,
		     opaque = Opaque,
		     scope = global,
		     tables = all,
		     prev_name = Name},
    case check_backup_args(Args, B) of
	{ok, B2} ->
	    %% Decentralized backup
	    %% Incremental

	    Self = self(),
	    Pid = spawn_link(?MODULE, backup_master, [Self, B2]),
	    receive
		{Pid, Self, Res} -> Res
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

check_backup_args([Arg | Tail], B) ->
    try check_backup_arg_type(Arg, B) of
	B2 ->
	    check_backup_args(Tail, B2)
    catch error:_ ->
	    {error, {badarg, Arg}}
    end;

check_backup_args([], B) ->
    {ok, B}.

check_backup_arg_type(Arg, B) ->
    case Arg of
	{scope, global} ->
	    B#backup_args{scope = global};
	{scope, local} ->
	    B#backup_args{scope = local};
	{module, Mod} ->
	    Mod2 = mnesia_monitor:do_check_type(backup_module, Mod),
	    B#backup_args{module = Mod2};
	{incremental, Name} ->
	    B#backup_args{prev_name = Name};
	{tables, Tabs} when is_list(Tabs) ->
	    B#backup_args{tables = Tabs}
    end.

backup_master(ClientPid, B) ->
    process_flag(trap_exit, true),
    try do_backup_master(B) of
	Res ->
	    ClientPid ! {self(), ClientPid, Res}
    catch _:Reason ->
	    ClientPid ! {self(), ClientPid, {error, {'EXIT', Reason}}}
    end,
    unlink(ClientPid),
    exit(normal).

do_backup_master(B) ->
    Name = B#backup_args.name,
    B2 = safe_apply(B, open_write, [B#backup_args.opaque]),
    B3 = safe_write(B2, [backup_log_header()]),
    case mnesia_checkpoint:tables_and_cookie(Name) of
	{ok, AllTabs, Cookie} ->
	    Tabs = select_tables(AllTabs, B3),
	    B4 = B3#backup_args{cookie = Cookie},
	    %% Always put schema first in backup file
	    B5 = backup_schema(B4, Tabs),
	    B6 = lists:foldl(fun backup_tab/2, B5, Tabs -- [schema]),
	    safe_apply(B6, commit_write, [B6#backup_args.opaque]),
	    ok;
	{error, Reason} ->
	    abort_write(B3, {?MODULE, backup_master}, [B], {error, Reason})
    end.

select_tables(AllTabs, B) ->
    Tabs =
	case B#backup_args.tables of
	    all -> AllTabs;
	    SomeTabs when is_list(SomeTabs) -> SomeTabs
	end,
    case B#backup_args.scope of
	global ->
	    Tabs;
	local ->
	    Name = B#backup_args.name,
	    [T || T <- Tabs, mnesia_checkpoint:most_local_node(Name, T) == {ok, node()}]
    end.

safe_write(B, []) ->
    B;
safe_write(B, Recs) ->
    safe_apply(B, write, [B#backup_args.opaque, Recs]).

backup_schema(B, Tabs) ->
    case lists:member(schema, Tabs) of
	true ->
	    backup_tab(schema, B);
	false ->
	    Defs = [{schema, T, mnesia_schema:get_create_list(T)} || T <- Tabs],
	    safe_write(B, Defs)
    end.

safe_apply(B, write, [_, Items]) when Items == [] ->
    B;
safe_apply(B, What, Args) ->
    Abort = abort_write_fun(B, What, Args),
    receive
	{'EXIT', Pid, R} -> Abort({'EXIT', Pid, R})
    after 0 ->
	    Mod = B#backup_args.module,
	    try apply(Mod, What, Args) of
		{ok, Opaque} -> B#backup_args{opaque=Opaque};
		{error, R} -> Abort(R)
	    catch _:R -> Abort(R)
	    end
    end.

-spec abort_write_fun(_, _, _) -> fun((_) -> no_return()).
abort_write_fun(B, What, Args) ->
    fun(R) -> abort_write(B, What, Args, R) end.

abort_write(B, What, Args, Reason) ->
    Mod = B#backup_args.module,
    Opaque = B#backup_args.opaque,
    dbg_out("Failed to perform backup. M=~p:F=~tp:A=~tp -> ~tp~n",
	    [Mod, What, Args, Reason]),
    try {ok, _Res} = apply(Mod, abort_write, [Opaque]) of
        _ -> throw({error, Reason})
    catch _:Other ->
	    error("Failed to abort backup. ~p:~tp~tp -> ~tp~n",
		  [Mod, abort_write, [Opaque], Other]),
	    throw({error, Reason})
    end.

backup_tab(Tab, B) ->
    Name = B#backup_args.name,
    case mnesia_checkpoint:most_local_node(Name, Tab) of
	{ok, Node} when Node == node() ->
	    tab_copier(self(), B, Tab);
	{ok, Node} ->
	    RemoteB = B,
	    Pid = spawn_link(Node, ?MODULE, tab_copier, [self(), RemoteB, Tab]),
	    RecName = val({Tab, record_name}),
	    tab_receiver(Pid, B, Tab, RecName, 0);
	{error, Reason} ->
	    abort_write(B, {?MODULE, backup_tab}, [Tab, B], {error, Reason})
    end.

tab_copier(Pid, B, Tab) when is_record(B, backup_args) ->
    %% Intentional crash at exit
    Name = B#backup_args.name,
    PrevName = B#backup_args.prev_name,
    {FirstName, FirstSource} = select_source(Tab, Name, PrevName),

    ?eval_debug_fun({?MODULE, tab_copier, pre}, [{name, Name}, {tab, Tab}]),
    Res = handle_more(Pid, B, Tab, FirstName, FirstSource, Name),
    ?eval_debug_fun({?MODULE, tab_copier, post}, [{name, Name}, {tab, Tab}]),

    handle_last(Pid, Res).

select_source(Tab, Name, PrevName) ->
    if
	Tab == schema ->
	    %% Always full backup of schema
	    {Name, table};
	Name == PrevName ->
	    %% Full backup
	    {Name, table};
	true ->
	    %% Wants incremental backup
	    case mnesia_checkpoint:most_local_node(PrevName, Tab) of
		{ok, Node} when Node == node() ->
		    %% Accept incremental backup
		    {PrevName, retainer};
		_ ->
		    %% Do a full backup anyway
		    dbg_out("Incremental backup escalated to full backup: ~tp~n", [Tab]),
		    {Name, table}
	    end
    end.

handle_more(Pid, B, Tab, FirstName, FirstSource, Name) ->
    Acc = {0, B},
    case {mnesia_checkpoint:really_retain(Name, Tab),
	  mnesia_checkpoint:really_retain(FirstName, Tab)} of
	{true, true} ->
	    Acc2 = iterate(B, FirstName, Tab, Pid, FirstSource, latest, first, Acc),
	    iterate(B, Name, Tab, Pid, retainer, checkpoint, last, Acc2);
	{false, false}->
	    %% Put the dumped file in the backup
	    %% instead of the ram table. Does
	    %% only apply to ram_copies.
	    iterate(B, Name, Tab, Pid, retainer, checkpoint, last, Acc);
	Bad ->
	    Reason = {"Checkpoints for incremental backup must have same "
		      "setting of ram_overrides_dump",
		      Tab, Name, FirstName, Bad},
	    abort_write(B, {?MODULE, backup_tab}, [Tab, B], {error, Reason})
    end.

handle_last(Pid, {_Count, B}) when Pid == self() ->
    B;
handle_last(Pid, _Acc) ->
    unlink(Pid),
    Pid ! {self(), {last, {ok, dummy}}},
    exit(normal).

iterate(B, Name, Tab, Pid, Source, Age, Pass, Acc) ->
    Fun =
	if
	    Pid == self() ->
		RecName = val({Tab, record_name}),
		fun(Recs, A) -> copy_records(RecName, Tab, Recs, A) end;
	    true ->
		fun(Recs, A) -> send_records(Pid, Tab, Recs, Pass, A) end
	end,
    case mnesia_checkpoint:iterate(Name, Tab, Fun, Acc, Source, Age) of
	{ok, Acc2} ->
	    Acc2;
	{error, Reason} ->
	    R = {error, {"Tab copier iteration failed", Reason}},
	    abort_write(B, {?MODULE, iterate}, [self(), B, Tab], R)
    end.

copy_records(_RecName, _Tab, [], Acc) ->
    Acc;
copy_records(RecName, Tab, Recs, {Count, B}) ->
    Recs2 = rec_filter(B, Tab, RecName, Recs),
    B2 = safe_write(B, Recs2),
    {Count + 1, B2}.

send_records(Pid, Tab, Recs, Pass, {Count, B}) ->
    receive
	{Pid, more, Count} ->
	    if
		Pass == last, Recs == [] ->
		    {Count, B};
		true ->
		    Next = Count + 1,
		    Pid ! {self(), {more, Next, Recs}},
		    {Next, B}
	    end;
	Msg ->
	    exit({send_records_unexpected_msg, Tab, Msg})
    end.

tab_receiver(Pid, B, Tab, RecName, Slot) ->
    Pid ! {self(), more, Slot},
    receive
	{Pid, {more, Next, Recs}} ->
	    Recs2 = rec_filter(B, Tab, RecName, Recs),
	    B2 = safe_write(B, Recs2),
	    tab_receiver(Pid, B2, Tab, RecName, Next);

	{Pid, {last, {ok,_}}} ->
	    B;

	{'EXIT', Pid, {error, R}} ->
	    Reason = {error, {"Tab copier crashed", R}},
	    abort_write(B, {?MODULE, remote_tab_sender}, [self(), B, Tab], Reason);
	{'EXIT', Pid, R} ->
	    Reason = {error, {"Tab copier crashed", {'EXIT', R}}},
	    abort_write(B, {?MODULE, remote_tab_sender}, [self(), B, Tab], Reason);
	Msg ->
	    R = {error, {"Tab receiver got unexpected msg", Msg}},
	    abort_write(B, {?MODULE, remote_tab_sender}, [self(), B, Tab], R)
    end.

rec_filter(B, schema, _RecName, Recs) ->
    try mnesia_bup:refresh_cookie(Recs, B#backup_args.cookie)
    catch throw:{error, _Reason} ->
	    %% No schema table cookie
	    Recs
    end;
rec_filter(_B, Tab, Tab, Recs) ->
    Recs;
rec_filter(_B, Tab, _RecName, Recs) ->
    [setelement(1, Rec, Tab) || Rec <- Recs].

ets2dcd(Tab) ->
    ets2dcd(Tab, dcd).

ets2dcd(Tab, Ftype) ->
    Fname =
	case Ftype of
	    dcd -> mnesia_lib:tab2dcd(Tab);
	    dmp -> mnesia_lib:tab2dmp(Tab)
	end,
    TmpF = mnesia_lib:tab2tmp(Tab),
    file:delete(TmpF),
    Log  = open_log({Tab, ets2dcd}, dcd_log_header(), TmpF, false),
    mnesia_lib:db_fixtable(ram_copies, Tab, true),
    ok   = ets2dcd(mnesia_lib:db_init_chunk(ram_copies, Tab, 1000), Tab, Log),
    mnesia_lib:db_fixtable(ram_copies, Tab, false),
    close_log(Log),
    ok = file:rename(TmpF, Fname),
    %% Remove old log data which is now in the new dcd.
    %% No one else should be accessing this file!
    file:delete(mnesia_lib:tab2dcl(Tab)),
    ok.

ets2dcd('$end_of_table', _Tab, _Log) ->
    ok;
ets2dcd({Recs, Cont}, Tab, Log) ->
    ok = disk_log:log_terms(Log, Recs),
    ets2dcd(mnesia_lib:db_chunk(ram_copies, Cont), Tab, Log).

dcd2ets(Tab) ->
    dcd2ets(Tab, mnesia_monitor:get_env(auto_repair)).

dcd2ets(Tab, Rep) ->
    Dcd = mnesia_lib:tab2dcd(Tab),
    case mnesia_lib:exists(Dcd) of
	true ->
	    Log = open_log({Tab, dcd2ets}, dcd_log_header(), Dcd,
			   true, Rep, read_only),
	    Data = chunk_log(Log, start),
	    ok = insert_dcdchunk(Data, Log, Tab),
	    close_log(Log),
	    load_dcl(Tab, Rep);
	false -> %% Handle old dets files, and conversion from disc_only to disc.
	    Fname = mnesia_lib:tab2dat(Tab),
	    Type = val({Tab, setorbag}),
	    case mnesia_lib:dets_to_ets(Tab, Tab, Fname, Type, Rep, yes) of
		loaded ->
		    ets2dcd(Tab),
		    file:delete(Fname),
		    0;
		{error, Error} ->
		    erlang:error({"Failed to load table from disc", [Tab, Error]})
	    end
    end.

insert_dcdchunk({Cont, [LogH | Rest]}, Log, Tab)
  when is_record(LogH, log_header),
       LogH#log_header.log_kind == dcd_log,
       LogH#log_header.log_version >= "1.0" ->
    insert_dcdchunk({Cont, Rest}, Log, Tab);

insert_dcdchunk({Cont, Recs}, Log, Tab) ->
    true = ets:insert(Tab, Recs),
    insert_dcdchunk(chunk_log(Log, Cont), Log, Tab);
insert_dcdchunk(eof, _Log, _Tab) ->
    ok.

load_dcl(Tab, Rep) ->
    FName = mnesia_lib:tab2dcl(Tab),
    case mnesia_lib:exists(FName) of
	true ->
	    Name = {load_dcl,Tab},
	    open_log(Name,
		     dcl_log_header(),
		     FName,
		     true,
		     Rep,
		     read_only),
	    FirstChunk = chunk_log(Name, start),
            N = insert_logchunk(FirstChunk, Name, 0),
	    close_log(Name),
	    N;
	false ->
	    0
    end.

insert_logchunk({C2, Recs}, Tab, C) ->
    N = add_recs(Recs, C),
    insert_logchunk(chunk_log(Tab, C2), Tab, C+N);
insert_logchunk(eof, _Tab, C) ->
    C.

add_recs([{{Tab, _Key}, Val, write} | Rest], N) ->
    true = ets:insert(Tab, Val),
    add_recs(Rest, N+1);
add_recs([{{Tab, Key}, _Val, delete} | Rest], N) ->
    true = ets:delete(Tab, Key),
    add_recs(Rest, N+1);
add_recs([{{Tab, _Key}, Val, delete_object} | Rest], N) ->
    true = ets:match_delete(Tab, Val),
    add_recs(Rest, N+1);
add_recs([{{Tab, Key}, Val, update_counter} | Rest], N) ->
    {RecName, Incr} = Val,
    try
	CounterVal = ets:update_counter(Tab, Key, Incr),
	true = (CounterVal >= 0)
    catch
	error:_ when Incr < 0 ->
	    Zero = {RecName, Key, 0},
	    true = ets:insert(Tab, Zero);
	error:_ ->
	    Zero = {RecName, Key, Incr},
	    true = ets:insert(Tab, Zero)
    end,
    add_recs(Rest, N+1);
add_recs([LogH|Rest], N)
  when is_record(LogH, log_header),
       LogH#log_header.log_kind == dcl_log,
       LogH#log_header.log_version >= "1.0" ->
    add_recs(Rest, N);
add_recs([{{Tab, _Key}, _Val, clear_table} | Rest], N) ->
    Size = ets:info(Tab, size),
    true = ets:delete_all_objects(Tab),
    add_recs(Rest, N+Size);
add_recs([], N) ->
    N.
