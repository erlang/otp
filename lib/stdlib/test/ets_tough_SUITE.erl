%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
-module(ets_tough_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
         ex1/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% gen_server behavior.
-behavior(gen_server).
-export([init/1,terminate/2,handle_call/3,handle_cast/2,
         handle_info/2,code_change/3]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,5}}].

all() -> 
    [ex1].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



-define(DEBUG(X),debug_disabled).
%%-define(DEBUG(X),X).
-define(GLOBAL_PARAMS,ets_tough_SUITE_global_params).

init_per_testcase(_Func, Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    ets:delete(?GLOBAL_PARAMS).


ex1(Config) when is_list(Config) ->
    ets:new(?GLOBAL_PARAMS,[named_table,public]),
    ets:insert(?GLOBAL_PARAMS,{a,set}),
    ets:insert(?GLOBAL_PARAMS,{b,set}),
    ex1_sub(Config),
    ets:insert(?GLOBAL_PARAMS,{a,ordered_set}),
    ets:insert(?GLOBAL_PARAMS,{b,set}),
    ex1_sub(Config),
    ets:insert(?GLOBAL_PARAMS,{a,ordered_set}),
    ets:insert(?GLOBAL_PARAMS,{b,ordered_set}),
    ex1_sub(Config).




ex1_sub(Config) ->
    {A,B} = prep(Config),
    N = 
	case proplists:get_value(ets_tough_SUITE_iters,Config) of
	    undefined ->
		5000;
	    Other -> 
		Other
	end,
    {NewA,NewB} = run({A,B},N),
    _Gurkor = lists:keysearch(gurka,1,ets:all()),
    (catch stop(NewA)),
    (catch stop(NewB)),
    ok.

prep(Config) ->
    rand:seed(exsplus),
    put(dump_ticket,none),
    DumpDir = filename:join(proplists:get_value(priv_dir,Config), "ets_tough"),
    file:make_dir(DumpDir),
    put(dump_dir,DumpDir),
    process_flag(trap_exit,true),
    {ok, A} = start(a),
    {ok, B} = start(b),
    {A,B}.

run({A,B},N) ->
    run(A,B,0,N).

run(A,B,N,N) ->
    {A,B};
run(A,B,N,M) ->
    eat_msgs(),
    Op = random_operation(),
    ?DEBUG(io:format("~w: ",[N])),
    case catch operate(Op,A,B) of
	{'EXIT',Reason} ->
	    io:format("\nFAILURE on ~w: ~w, reason: ~w\n",[N,Op,Reason]),
	    exit(failed);
	{new_a,NewA} ->
	    run(NewA,B,N+1,M);
	_ ->
	    run(A,B,N+1,M)
    end.

eat_msgs() ->
    receive
	_Anything ->
	    eat_msgs()
    after 0 ->
	    ok
    end.

operate(get,A,B) ->
    case random_key() of
	1 ->
	    Class = random_class(),
	    AnsA = lists:sort(dget_class(A,Class,all)),
	    AnsB = lists:sort(dget_class(B,Class,all)),
	    ?DEBUG(io:format("get_class ~w (~w)\n",[Class,AnsA])),
	    AnsA = AnsB;
	_Other ->
	    Class = random_class(),
	    Key = random_key(),
	    AnsA = dget(A,Class,Key),
	    AnsB = dget(B,Class,Key),
	    ?DEBUG(io:format("get ~w,~w (~w)\n",[Class,Key,AnsA])),
	    AnsA = AnsB
    end;

operate(put,A,B) ->
    Class = random_class(),
    Key = random_key(),
    Value = random_value(),
    AnsA = dput(A,Class,Key,Value),
    AnsB = dput(B,Class,Key,Value),
    ?DEBUG(io:format("put ~w,~w=~w (~w)\n",[Class,Key,Value,AnsA])),
    AnsA = AnsB;

operate(erase,A,B) ->
    case random_key() of
	1 ->
	    Class = random_class(),
	    AnsA = derase_class(A,Class),
	    AnsB = derase_class(B,Class),
	    ?DEBUG(io:format("erase_class ~w\n",[Class])),
	    AnsA = AnsB;
	_Other ->
	    Class = random_class(),
	    Key = random_key(),
	    AnsA = derase(A,Class,Key),
	    AnsB = derase(B,Class,Key),
	    ?DEBUG(io:format("erase ~w,~w (~w)\n",[Class,Key,AnsA])),
	    AnsA = AnsB
    end;

operate(dirty_get,A,_B) ->
    Class = random_class(),
    Key = random_key(),
    %% only try dirty get on the b-side (which is never dumping)
    AnsA = dget(A,Class,Key),
    AnsB = dirty_dget(b,Class,Key),
    ?DEBUG(io:format("dirty_get ~w,~w (~w)\n",[Class,Key,AnsA])),
    AnsA = AnsB;

operate(dump,A,_B) ->
    case get(dump_ticket) of
	{dump_more,Ticket} ->
	    Units = random_key(),
	    NewTicket = ddump_next(A,Units,Ticket),
	    put(dump_ticket,NewTicket),
	    _Result = case NewTicket of
			  done -> done;
			  _ ->    dump_more
		      end,
	    ?DEBUG(io:format("dump ~w (~w)\n",[Units,_Result]));
	_ ->
	    DumpDir = get(dump_dir),
	    case random_key() of
		1 ->
		    ?DEBUG(io:format("start_dump\n",[])),
		    NewTicket = ddump_first(A,DumpDir),
		    put(dump_ticket,NewTicket);
		2 ->
		    ?DEBUG(io:format("dump_and_restore\n",[])),
		    {dump_more,NewTicket} = ddump_first(A,DumpDir),
		    done = ddump_next(A,1000000,NewTicket),
		    stop(A),
		    {ok, NewA} = start(a,DumpDir),
		    {new_a,NewA};
		_ ->
		    ?DEBUG(io:format("idle\n",[])),
		    ok
	    end
    end.

random_operation() ->
    Ops = {get,put,erase,dirty_get,dump},
    random_element(Ops).

random_class() ->
    Classes = {foo,bar,tomat,gurka},
    random_element(Classes).

random_key() ->
    rand:uniform(8).

random_value() ->
    case rand:uniform(5) of
	1 -> ok;
	2 -> {data,random_key()};
	3 -> {foo,bar,random_class()};
	4 -> rand:uniform(1000);
	5 -> {recursive,random_value()}
    end.

random_element(T) ->
    I = rand:uniform(tuple_size(T)),
    element(I,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DEFINITIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(NAMED_TABLES,true).
-define(DB_NAME_KEY, {'$db_name'}).
-define(LIST_OF_CLASSES_KEY,{'$list_of_classes'}).
-define(DUMPING_FLAG_KEY,{'$dumping_flag'}).
-define(DUMP_DIRECTORY_KEY,{'$dump_directory'}).
-define(ERASE_MARK(Key),{{{'$erased'},Key}}).
-define(ets_new,ets:new).
-define(ets_lookup,ets:lookup).
-define(ets_insert,ets:insert).    % erlang:db_put
-define(ets_delete,ets:delete).    % erlang:db_erase
-define(ets_first,ets:first).      % erlang:db_first
-define(ets_next,ets:next).        % erlang:db_next_key
-define(ets_info,ets:info).        % erlang:db_info

%%% INTERFACE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% start(DbName) -> Pid | {error,Reason}
%%%
%%% Starts the ets table database with name DbName

start(DbName) ->
    case gen_server:start_link(ets_tough_SUITE,{DbName,no_dump_dir},[]) of
	{ok,Pid} when is_pid(Pid) ->
	    {ok, Pid};
	Other ->
	    Other
    end.

%%% start(DbName,DumpDir) -> Pid | {error,Reason}
%%%
%%% Starts the ets table database with name DbName, and reads a dump
%%% from DumpDir when it starts.

start(DbName,DumpDir) ->
    case gen_server:start_link(ets_tough_SUITE,
			       {DbName,{dump_dir,DumpDir}},[]) of
	{ok,Pid} when is_pid(Pid) ->
	    {ok, Pid};
	Other ->
	    Other
    end.

%%% stop(ServerPid) -> {'EXIT',shutdown}
%%%
%%% Shuts down the ets table database

stop(ServerPid) ->
    gen_server:call(ServerPid,stop).

%%% dget(ServerPid,Class,Key) -> {value,Value} | undefined
%%%
%%% Returns a value identified by Class,Key from the database, or
%%% 'undefined' if there is no such value.

dget(ServerPid,Class,Key) ->
    gen_server:call(ServerPid,{handle_lookup,Class,Key}).

%%% dirty_dget(DbName,Class,Key) -> {value,Value} | undefined
%%%
%%% This is looks up the value directly in the ets table
%%% to avoid message passing. Several databases may be started,
%%% so the admin table must be registered.

dirty_dget(DbName,Class,Key) ->
    Admin = admin_table_name(DbName),
    case catch(?ets_lookup(Admin,Class)) of
	[{_Class,[Tab|_Tabs]}] ->
	    case ?ets_lookup(Tab,Key) of
		[{_Key,Value}] ->
		    {value,Value};
		_ ->
		    undefined
	    end;
	_ ->
	    undefined
    end.

%%% dput(ServerPid,Class,Key,Value) -> undefined | {value,OldValue}
%%%
%%% Inserts the given Value to be identified by Class,Key. Any prevoius
%%% value is returned, or otherwise 'undefined'.

dput(ServerPid,Class,Key,Value) ->
    gen_server:call(ServerPid,{handle_insert,Class,Key,Value}).

%%% derase(ServerPid,Class,Key) -> undefined | {value,OldValue}
%%%
%%% Erases any value identified by Class,Key

derase(ServerPid,Class,Key) ->
    gen_server:call(ServerPid,{handle_delete,Class,Key}).

%%% dget_class(ServerPid,Class,Condition) -> KeyList
%%%
%%% Returns a list of keys where the instance match Condition.
%%% Condition = 'all' returns all keys in the class.
%%% The condition is supplied as Condition = {Mod, Fun, ExtraArgs},
%%% where the instance will be prepended to ExtraArgs before each
%%% call is made.

dget_class(ServerPid,Class,Condition) ->
    gen_server:call(ServerPid,
		    {handle_get_class,Class,Condition},infinity).

%%% derase_class(ServerPid,Class) -> ok
%%%
%%% Erases a whole class, identified by Class

derase_class(ServerPid,Class) ->
    gen_server:call(ServerPid,{handle_delete_class,Class}, infinity).

%%% ddump_first(ServerPid,DumpDir) -> {dump_more,Ticket} | already_dumping
%%%
%%% Starts dumping the database. This call redirects all database updates
%%% to temporary tables, so that exactly the same database image will be
%%% written to disk as is in memory when this function is called.
%%% The returned Ticket is to be used with ddump_next/2

ddump_first(ServerPid,DumpDir) ->
    gen_server:call(ServerPid,{handle_dump_first,DumpDir}, infinity).

%%% ddump_next(ServerPid,Count,Ticket) -> {dump_more,Ticket} | done
%%%
%%% Dumps the database. This function performs Count units of dump work.
%%% Higher value of Count makes the entire dump operation more efficient,
%%% but blocks the database for longer periods of time.
%%% If there is still more work to be done, a new Ticket is returned,
%%% or 'done' otherwise.

ddump_next(ServerPid,Count,Ticket) ->
    gen_server:call(ServerPid,{handle_dump_next,Ticket,Count},150000).

%%% PRIVATE HANDLER FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Admin
%%% -----
%%%
%%% The database has a main administrative table Admin. It always contains
%%% these four items:
%%%
%%%    {{'$db_name'},Name}
%%%    {{'$list_of_classes'},ListOfClasses}
%%%    {{'$dumping_flag'},BoolDumping}
%%%    {{'$dump_directory'},Dir}
%%%
%%% The ListOfClasses is simply a list of all Classes that has ever been
%%% inserted in the database. It's used to know which tables to dump.
%%% The dump flag is 'true' while dump is in progress, to make it
%%% impossible to start a new dump before an old dump is completed.
%%%
%%% For each class there is an entry of the form
%%%
%%%    {Class,ListOfTables}
%%%
%%% Where the ListOfTables is the list of class tables (see below)
%%%
%%% Class Tables
%%% ------------
%%%
%%% The class tables are common ets tables that have the actual user
%%% data stored in them.
%%%
%%% Normally there is only one class table, Mtab (main table).
%%% When dumping is initiated, each class is syncronously given a 
%%% temporary table, Ttab, where all updates are stored. Reads are 
%%% directed to the Ttab first, and only if not found there, Mtab is
%%% consulted.
%%%
%%% Writes always go to the first table in the table sequence. This
%%% ensures that the dump algorithm can enumerate the entries in the
%%% other tables, without risk of being disrupted.
%%%
%%% When the dumping to disk is completed, it's time to write back
%%% whatever updates that came into the Ttab to Mtab. To do this, a
%%% third table is needed, Utab, to handle all updates while Ttab is
%%% being copied to Mtab. When all of Ttab is copied, Ttab is thrown
%%% away, and the whole process is repeated with Utab as Ttab until
%%% eventually nobody wrote to Utab while Ttab was copied (clean run).
%%%
%%% There is no _guarantee_ that this will ever happen, but unless there
%%% is a constant (and quite high frequency) stream of updates to a
%%% particular class, this should work.
%%%
%%% (It is possible to make this failsafe, by copying the elements in
%%% Mtab to Ttab. This is probably a lot more expensive, though)
%%%
%%% Erasure during dump
%%% -------------------
%%%
%%% Erasing need special attention when a single class has several 
%%% tables. It really boils down to a number of cases:
%%% 
%%% - element does not exist in Ttab.
%%%     A special erase record is written, {{{'$erased'},Key}} which
%%%     is hopefully different from all other keys used by the user.
%%% - element exists in Ttab
%%%     The element is deleted, and erase record is written
%%% - element does not exist in Ttab, but there is an erase record
%%%     fine, do nothing
%%% - element exist in Ttab, and there is an erase record
%%%     This happens when a record is deleted from Ttab, then written
%%%     back again. Erase records are not looked for when inserting
%%%     new data (and that's not necessary)
%%%
%%% Then when Ttab should be copied to Mtab:
%%%
%%% - found an element
%%%     Usual case, just copy
%%% - found erase record
%%%     Check if there is an element with the same key as the erase
%%%     record. If so it has been written later than the erasure, so
%%%     the erasure is obsolete. Otherwise erase the record from Mtab.
%%%
%%% Delete Class
%%% ------------
%%%
%%% A slight problem is deleting an entire class while dumping is in
%%% progress. For consitency, all user visible traces of the class must
%%% be deleted, while dumping must not be affected. On top of that, the
%%% deleted class may well be recreated while dumping is still going on,
%%% and entries added.
%%%
%%% This is solved by having the dump algorithm keep track of the table
%%% identifiers of the tables to dump, rather than asking the admin table
%%% (since the class might be deleted there). The dump algorithm will
%%% itself take care of deleting the tables used in the dumping, while the
%%% normal database interface deletes the "first table", the table that is
%%% currently accepting all write operations.


init({DbName,DumpDir}) ->
    case DumpDir of
	no_dump_dir ->
	    Admin = make_admin_table(DbName),
	    ?ets_insert(Admin,{?LIST_OF_CLASSES_KEY,[]}),
	    init2(DbName,Admin);
	{dump_dir,Dir} ->
	    case load_dump(DbName,Dir) of
		{ok,Admin} ->
		    ?ets_insert(Admin,{?DUMP_DIRECTORY_KEY,Dir}),
		    init2(DbName,Admin);
		_ ->
		    cant_load_dump
	    end
    end.

init2(DbName,Admin) ->
    ?ets_insert(Admin,{?DUMPING_FLAG_KEY,false}),
    ?ets_insert(Admin,{?DB_NAME_KEY,DbName}),
    {ok, Admin}.

terminate(_Reason,_Admin) ->
    ok.

handle_call({handle_lookup,Class,Key},_From,Admin) ->
    %% Lookup tables to search in
    Reply =
	case ?ets_lookup(Admin,Class) of
	    [] ->
		undefined; %% no such class => no such record
	    [{_,TabList}] ->
		{_,Ans} = table_lookup(TabList, Key),
		Ans
	end,
    {reply,Reply,Admin};

handle_call({handle_insert,Class,Key,Value},_From,Admin) ->
    %% Lookup in which table to write
    Reply = 
	case ?ets_lookup(Admin,Class) of
	    [] ->
		%% undefined class, let's create it
		Mtab = make_db_table(db_name(Admin),Class),
		?ets_insert(Admin,{Class,[Mtab]}),
		[{_,Classes}] = ?ets_lookup(Admin,?LIST_OF_CLASSES_KEY),
		?ets_insert(Admin,{?LIST_OF_CLASSES_KEY,[Class|Classes]}),
		?ets_insert(Mtab, {Key, Value}),
		undefined;
	    [{_,[Tab|Tabs]}] ->
		{_,Old} = table_lookup([Tab|Tabs], Key),
		?ets_insert(Tab, {Key, Value}),
		Old
	end,
    {reply,Reply,Admin};

handle_call({handle_delete,Class,Key},_From,Admin) ->
    %% Lookup in which table to write
    Reply =
	case ?ets_lookup(Admin, Class) of
	    [] ->
		undefined; %% no such class, but delete is happy anyway
	    [{_,[Tab]}] ->
		%% When there is only one table, simply deleting is enough
		{_,Old} = table_lookup(Tab,Key),
		?ets_delete(Tab,Key),
		Old;
	    [{_,[Tab|Tabs]}] ->
		%% When there are more tables, we have to write a delete
		%% record into the first one, so that nobody goes looking
		%% for this entry in some other table
		{_,Old} = table_lookup([Tab|Tabs],Key),
		?ets_insert(Tab, {?ERASE_MARK(Key), erased}),
		?ets_delete(Tab,Key),
		Old
	end,
    {reply,Reply,Admin};

handle_call({handle_get_class,Class,Cond},_From,Admin) ->
    Reply =
	case ?ets_lookup(Admin,Class) of     % Lookup tables to search in
	    [] ->
		[];          % no such class
	    [{_,TabList}] ->
		table_lookup_batch(TabList, Class, Cond)  % get class data
	end,
    {reply,Reply,Admin};

handle_call({handle_delete_class,Class},_From,Admin) ->
    Reply =
	case ?ets_lookup(Admin, Class) of
	    [] ->
		ok;     % no such class, but delete_class is happy anyway
	    [{_,[Tab|_Tabs]}] ->
		%% Always delete the first table (the one we're writing into)
		%% In case we're dumping, the rest of the tables will be
		%% taken care of by the dump algorithm.
		?ets_delete(Tab),
		[{_, Classes}] = ?ets_lookup(Admin, ?LIST_OF_CLASSES_KEY),
		NewClasses = lists:delete(Class, Classes),
		?ets_insert(Admin, {?LIST_OF_CLASSES_KEY, NewClasses}),
		?ets_delete(Admin, Class),
		ok
	end,
    {reply,Reply,Admin};

handle_call({handle_dmodify,Application},_From,Admin) ->
    [{_, Classes}] = ?ets_lookup(Admin, ?LIST_OF_CLASSES_KEY),
    modify(Application, Classes, Admin),
    {reply,ok,Admin};

handle_call({handle_dump_first,DumpDir},_From,Admin) ->
    case ?ets_lookup(Admin,?DUMPING_FLAG_KEY) of
	[{_,true}] ->
	    {reply,already_dumping,Admin};
	_ ->
	    phys_remove_ok(DumpDir),
	    [{_,Classes}] = ?ets_lookup(Admin,?LIST_OF_CLASSES_KEY),
	    Tables = dump_prepare_classes(Classes,Admin),
	    ?ets_insert(Admin,{?DUMPING_FLAG_KEY,true}),
	    %% this is the new dir for dumping:
	    ?ets_insert(Admin,{?DUMP_DIRECTORY_KEY,DumpDir}),
	    handle_dump_next({[{admin,Classes}|Tables]},0,Admin)
    end;

%% All done, good work!
handle_call({handle_dump_next,Ticket,Count},_From,Admin) ->
    handle_dump_next(Ticket,Count,Admin);

handle_call(stop,_From,Admin) ->
    ?ets_delete(Admin), % Make sure table is gone before reply is sent.
    {stop, normal, ok, []}.

handle_cast(_Req, Admin) ->
    {noreply, Admin}.

handle_info({'EXIT',_Pid,_Reason},Admin) ->
    {stop,normal,Admin}.

code_change(_OldVsn, StateData, _Extra) ->
    {ok, StateData}.

handle_delete(Class, Key, Admin) ->
    handle_call({handle_delete,Class,Key},from,Admin).

handle_insert(Class, Key, Value, Admin) ->
    handle_call({handle_insert,Class,Key,Value},from,Admin).

handle_lookup(Class, Key, Admin) ->
    handle_call({handle_lookup,Class,Key},from,Admin).


handle_dump_next({[]},_Count,Admin) ->
    [{_Key,DumpDir}] = ?ets_lookup(Admin,?DUMP_DIRECTORY_KEY),
    phys_ok_dump(DumpDir),
    ?ets_insert(Admin,{?DUMPING_FLAG_KEY,false}),
    {reply,done,Admin};

%% No more operations, return to user asking for more
handle_dump_next(Ticket,0,Admin) ->
    {reply,{dump_more,Ticket},Admin};

%% Dump the admin table. Costs one dump-work unit.
handle_dump_next({[{admin,Classes}|Tables]},Count,Admin) ->
    [{_Key,DumpDir}] = ?ets_lookup(Admin,?DUMP_DIRECTORY_KEY),
    DumpData = phys_init_dump(admin,DumpDir,0),
    phys_dump({?LIST_OF_CLASSES_KEY,Classes},DumpData),
    phys_finish_dump(DumpData),
    handle_dump_next({Tables},Count-1,Admin);

%% Pick out a class and start dumping it
handle_dump_next({[{Class,Mtab}|Tables]},Count,Admin) ->
    ?DEBUG(io:format("DUMP CLASS ~w\n",[Class])),
    [{_Key,DumpDir}] = ?ets_lookup(Admin,?DUMP_DIRECTORY_KEY),
    DumpData = phys_init_dump(Class,DumpDir,length(Tables)+1),
    First = ?ets_first(Mtab),
    handle_dump_next({Class,Tables,Mtab,First,DumpData},Count,Admin);

%% All keys in this class have been written to disk, now we have to
%% copy all items from temporary Ttab to main Mtab
handle_dump_next({Class,Tables,Stab,'$end_of_table',DumpData},Count,Admin) ->
    phys_finish_dump(DumpData),
    ?DEBUG(io:format("Cleaning up temporary table in ~p\n",[Class])),
    case ?ets_lookup(Admin,Class) of
	[{Key,[Utab,Mtab]}] ->
	    Ttab = make_db_table(db_name(Admin),Class),
	    ?ets_insert(Admin,{Key,[Ttab,Utab,Mtab]}),
	    First = ?ets_first(Utab),
	    handle_dump_next({3,Class,Tables,Utab,First,Mtab},Count,Admin);
	_Other ->
	    %% Class deleted (and maybe recreated) while dumping, no need to 
	    %% bring this one up to date. Just discard late additions.
	    ?ets_delete(Stab),
	    handle_dump_next({Tables},Count,Admin)
    end;

%% Dumping one key to disk. Costs one dump-work unit.
handle_dump_next({Class,Tables,Tab,Key,DumpData},Count,Admin) ->
    [KeyVal] = ?ets_lookup(Tab,Key),
    phys_dump(KeyVal,DumpData),
    NextKey = ?ets_next(Tab,Key),
    handle_dump_next({Class,Tables,Tab,NextKey,DumpData},Count-1,Admin);

%% Done copying elements from Ttab to Mtab
%% check if Utab is empty and go on with next class, or
%% make Utab the current Ttab, and run again
%% ... will this ever end? ;-)
handle_dump_next({3,Class,Tables,Stab,'$end_of_table',Dtab},Count,Admin) ->
    case ?ets_lookup(Admin,Class) of
	[{Key,[Ttab,Utab,Mtab]}] ->
	    case ?ets_info(Ttab,size) of
		0 ->
		    ?ets_insert(Admin,{Key,[Mtab]}),
		    ?ets_delete(Ttab),
		    ?ets_delete(Utab),
		    handle_dump_next({Tables},Count,Admin);
		_Work ->
		    ?DEBUG(io:format("Switching direction in ~p\n",[Class])),
		    %% Which is faster, deleting all the entries
		    %% in a table, or deleting it and create a new?
		    ?ets_delete(Utab),
		    Ntab = make_db_table(db_name(Admin),Class),
		    ?ets_insert(Admin,{Key,[Ntab,Ttab,Mtab]}),
		    First = ?ets_first(Ttab),
		    handle_dump_next({3,Class,Tables,Ttab,First,Mtab},
				     Count,Admin)
	    end;
	_Other ->
	    %% Class deleted (and maybe recreated) while dumping, no need to 
	    %% bring this one up to date. Just discard late additions.
	    ?ets_delete(Stab),
	    ?ets_delete(Dtab),
	    handle_dump_next({Tables},Count,Admin)
    end;

%% Copy one key from Ttab to Mtab
%% costs one dump-work unit
handle_dump_next({3,Class,Tables,Stab,Key,Dtab},Count,Admin) ->
    copy_dump_entry(Stab,Key,Dtab),
    NextKey = ?ets_next(Stab,Key),
    handle_dump_next({3,Class,Tables,Stab,NextKey,Dtab},Count-1,Admin).

%%% INTERNAL HELPER FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% admin_table_name(DbName) -> Name
%%%
%%% Returns the name of the admin table of the table DbName

admin_table_name(DbName) ->
    list_to_atom(lists:append(atom_to_list(DbName),"#admin")).

%%% make_admin_table(DbName) -> EtsAdminTable
%%%
%%% Creates and registers an ETS Admin table

make_admin_table(DbName) ->
    ?ets_new(admin_table_name(DbName),[named_table,protected,db_type(DbName)]).

%%% make_db_table(DbName,Name) -> EtsTable
%%%
%%% Creates an ETS database table

make_db_table(DbName, Name) ->
    ?ets_new(Name,[protected,db_type(DbName)]).

db_name(Admin) ->
    ets:lookup_element(Admin,?DB_NAME_KEY,2).

db_type(DbName) ->
    case ets:lookup(?GLOBAL_PARAMS, DbName) of
	[] ->
	    set;
	[{DbName,X}] ->
	    X
    end.

%%% table_lookup(Table,Key) -> 
%%% table_lookup(TableList,Key) ->
%%%    {def,{value,Value}} | {undef,undefined} | (erased,undefined}
%%%
%%% Looks up key in the table and returns it value, or undefined
%%% if there is no such key.
%%% If a list of tables is given, they are searched one after another
%%% for a matching key, until one is found. The search is discontinued
%%% if a record telling that the key was deleted is found.

table_lookup([], _Key) ->
    {undef,undefined};
table_lookup([Table|Tables], Key) ->
    case table_lookup(Table,Key) of
	{_,undefined} ->
	    case ?ets_lookup(Table,?ERASE_MARK(Key)) of
		[] ->
		    table_lookup(Tables,Key);
		_Definition ->
		    %% The element has been deleted, don't look further!
		    %% Pretend we never saw anything..
		    {erased,undefined}
	    end;
	Answer ->
	    Answer
    end;
table_lookup(Table, Key) ->
    case ?ets_lookup(Table,Key) of
	[] ->
	    {undef,undefined};
	[{_Key,Value}] ->
	    {def,{value,Value}}
    end.

%%% table_lookup_batch(Tables, Class, Cond) -> KeyList
%%%
%%% Extract the keys from a table or a table group.
%%% If a condition is supplied, it is on the form {Mod, Fun, ExtraArgs}
%%% and returns {true,Key} or false when called using
%%% apply(Mod, Fun, [Instance|ExtraArgs]).
%%% Instance is, for historic reasons, {{Class, Key}, Value} when the function
%%% is called. Cond = 'all' can be used to get all keys from a class.

table_lookup_batch([],_Class,_Cond) ->
    [];
table_lookup_batch([Table|Tables],Class,Cond) ->
    table_lookup_batch([],Tables,Table,ets:first(Table),Class,Cond,[]).

table_lookup_batch(_Passed,[],_,'$end_of_table',_Class,_Cond,Ack) ->
    Ack;
table_lookup_batch(Passed,[NewTable|Tables],Table,'$end_of_table',
		   Class,Cond,Ack) ->
    table_lookup_batch(lists:append(Passed,[Table]),Tables,
		       NewTable,ets:first(NewTable),Class,Cond,Ack);
table_lookup_batch(Passed,Tables,Table,?ERASE_MARK(Key),Class,Cond,Ack) ->
    table_lookup_batch(Passed,Tables,Table,?ets_next(Table,?ERASE_MARK(Key)),
		       Class,Cond,Ack);

table_lookup_batch(Passed,Tables,Table,Key,Class,Cond,Ack) ->
    NewAck =
	case table_lookup(Passed,Key) of
	    {undef,undefined} ->
		[{_Key,Value}] = ?ets_lookup(Table,Key),
		case Cond of    % are there any conditions?
		    all ->
			[Key|Ack];
		    {M, F, A} ->
			%% apply the condition test.
			%% Applications need keys to consist of
			%% {class, primkey}, so we make it that way
			case apply(M, F, [{{Class, Key}, Value}|A]) of
			    {true, Key} -> [Key|Ack];
			    false ->       Ack
			end
		end;
	    _Other -> 
		%% Already processed (or erased) key
		%% {def,{value,Value}} ->
		%% {erased,undefined} ->
		Ack
	end,
    table_lookup_batch(Passed,Tables,Table,?ets_next(Table,Key),
		       Class,Cond,NewAck).

%%% modify(Application, ClassList, Admin) -> ok.
%%%
%%% This function modifies each element of the classes

modify(_Application, [], _Admin) ->
    ok;
modify(Application, [Class|Classes], Admin) ->
    ?DEBUG(io:format("modifying class ~p\n", [Class])),
    [{_,Tables}] = ?ets_lookup(Admin, Class),
    modify_class(Application, Class, table_lookup_batch(Tables, Class, all),
		 Admin),
    modify(Application, Classes, Admin).

modify_class(_Application, _Class, [], _Admin) ->
    ok;
modify_class({Mod, Fun, ExtraArgs}, Class, [Key|Keys], Admin) ->
    {ok, {{value, Value}, _Admin}} = handle_lookup(Class, Key, Admin),
    %% The applications think that a key consists of {class, primkey},
    %% so let them.
    case apply(Mod,Fun,[{{Class, Key}, Value}|ExtraArgs]) of
	{ok,{{NewClass, NewKey}, NewValue}} ->   % The item is modified.
	    %% remove old instance, insert new
	    %% JALI could be optimized (we don't care about previous values),
	    %% but ets_delete/insert is *not* enough
	    handle_delete(Class, Key, Admin),
	    handle_insert(NewClass, NewKey, NewValue, Admin);
	true ->                           % The item should be left as it is.
	    ok;
	false ->                          % The item should be removed!
	    %% JALI could be optimized (we don't care about previous values),
	    %% but ets_delete is *not* enough
	    handle_delete(Class, Key, Admin)
    end,
    modify_class({Mod, Fun, ExtraArgs}, Class, Keys, Admin).

%%% dump_prepare_classes(Classes,Admin) -> ok
%%%
%%% Create a Ttab for each class, and insert 
%%% the new table order in Admin

dump_prepare_classes(Classes,Admin) ->
    ?DEBUG(io:format("DUMP CLASSES ~w\n",[Classes])),
    dump_prepare_classes(Classes,Admin,[]).

dump_prepare_classes([],_Admin,Ack) ->
    Ack;
dump_prepare_classes([Class|Classes],Admin,Ack) ->
    [{_Class,[Mtab]}] = ?ets_lookup(Admin,Class),
    %% Only one table => we can prepare for dumping
    %% In case there are several tables defined, dumping is
    %% already (still) in progress for this class (database inconsistent)
    Ttab = make_db_table(db_name(Admin),Class),
    ?ets_insert(Admin,{Class,[Ttab,Mtab]}),
    dump_prepare_classes(Classes,Admin,lists:append(Ack,[{Class,Mtab}])).

%%% copy_dump_entry(SourceTable,Key,DestinationTable) -> NobodyCares
%%%
%%% Copies Key from SourceTable to DestinationTable.
%%% If Key is an erase record, then the corresponding entry is deleted
%%% from DestinationTable, if it should be (see Erasure during dump, above)

copy_dump_entry(Stab,Key,Dtab) ->
    ?DEBUG(io:format("Copying key ~p\n",[Key])),
    case ?ets_lookup(Stab,Key) of
	[{?ERASE_MARK(RealKey),_}] ->
	    %% Only erase if the entry RealKey hasn't been written again
	    case ?ets_lookup(Stab,RealKey) of
		[] ->
		    %% No, it hasn't: we should delete
		    ?DEBUG(io:format("Erasing: ~p\n",[RealKey])),
		    ?ets_delete(Dtab,RealKey);
		_Definition ->
		    %% It has, don't erase. In this case the new value
		    %% has already or will soon be written to Dtab
		    ok
	    end;
	[KeyVal] ->
	    ?DEBUG(io:format("Forwarding: ~p\n",[KeyVal])),
	    ?ets_insert(Dtab,KeyVal)
    end.

%%% DUMP LOADING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_dump(DbName,DumpDir) ->
    case phys_load_dump_ok(DumpDir) of
	ok ->
	    Admin = make_admin_table(DbName),
	    ?ets_insert(Admin,{?DB_NAME_KEY,DbName}),
	    case phys_load_table(DumpDir,0,Admin) of
		ok ->
		    load_dump2(DumpDir,Admin);
		Other ->
		    load_dump_failed(Admin,[]),
		    {error,{load_dump1,Other}}
	    end;
	Other ->
	    {error,{load_dump2,Other}}
    end.

load_dump2(DumpDir,Admin) ->
    case ?ets_lookup(Admin,?LIST_OF_CLASSES_KEY) of
	[{_Key,Classes}] ->
	    case load_dump_tables(DumpDir,Admin,Classes) of
		ok ->
		    {ok, Admin};
		Other ->
		    io:format("Dumping failed: ~p\n",[Other]),
		    load_dump_failed(Admin,Classes)
	    end;
	Other ->
	    io:format("Dumping failed2: ~p\n",[Other]),
	    load_dump_failed(Admin,[])
    end.

load_dump_failed(Admin,[]) ->
    ?ets_delete(Admin),
    {error,load_dump_failed};
load_dump_failed(Admin,[Class|Classes]) ->
    case ?ets_lookup(Admin,Class) of
	[{_Key,[Tab]}] ->
	    ?ets_delete(Tab);
	_ ->
	    ok
    end,
    load_dump_failed(Admin,Classes).

load_dump_tables(_DumpDir,_Admin,[]) ->
    ok;
load_dump_tables(DumpDir,Admin,[Class|Classes]) ->
    Mtab = make_db_table(db_name(Admin),Class),
    ?ets_insert(Admin,{Class,[Mtab]}),
    Num = length(Classes)+1,
    case phys_load_table(DumpDir,Num,Mtab) of
	ok ->
	    load_dump_tables(DumpDir,Admin,Classes);
	Other ->
	    {error,{load_dump_failed,Other}}
    end.

%%% FILE ACCESS LAYER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% phys_init_dump(Class,DumpDir) -> DumpData

phys_init_dump(Class,DumpDir,Num) ->
    ?DEBUG(io:format("Opened ~p for writing\n",[Class])),
    FileName = [DumpDir,"/etsdump.",integer_to_list(Num)],
    {tag1,{ok,Fd}} = {tag1,file:open(FileName,write)},
    {Class,Fd}.

%%% phys_finish_dump(DumpData) -> NobodyCares

phys_finish_dump({_Class,Fd}) ->
    ?DEBUG(io:format("Closed ~p\n",[_Class])),
    phys_dump_term(Fd,ok),
    file:close(Fd), % JALI: OTP P1D returns true instead of ok, so no check
    ok.

%%% phys_dump(KeyVal,DumpData) -> NobodyCares

phys_dump({Key,Val},{_Class,Fd}) ->
    ?DEBUG(io:format("To disk (~p.dump): {~p,~p}\n",[_Class,Key,Val])),
    phys_dump_term(Fd,{Key,Val}),
    ok.

phys_dump_term(Fd,Term) ->
    Bin = binary_to_list(term_to_binary(Term)),
    {tag2,ok} = {tag2,io:put_chars(Fd,encode32(length(Bin)))},
    {tag3,ok} = {tag3,io:put_chars(Fd,Bin)}.

%%% phys_ok_dump(DumpDir) -> NobodyCares

phys_ok_dump(DumpDir) ->
    ?DEBUG(io:format("Ok:ing dump dir ~s\n",[DumpDir])),
    FileName = [DumpDir,"/ok"],
    {tag4,{ok,Fd}} = {tag4,file:open(FileName,write)},
    {tag5,ok} = {tag5,io:format(Fd,"ok.\n",[])},
    file:close(Fd), % JALI: OTP P1D returns true instead of ok, so no check
    ok.

phys_remove_ok(DumpDir) ->
    ?DEBUG(io:format("Removing any Ok in dump dir ~s\n",[DumpDir])),
    FileName = [DumpDir,"/ok"],
    %% don't care if delete returns ok, file probably doesn't exist
    file:delete(FileName),
    ok.

phys_load_dump_ok(DumpDir) ->
    FileName = [DumpDir,"/ok"],
    case file:consult(FileName) of
	{ok,[ok]} ->
	    ok;
	Other ->
	    {error,{consult_error,Other}}
    end.

phys_load_table(DumpDir,N,Tab) ->
    ?DEBUG(io:format("LOAD TABLE ~w\n",[N])),
    FileName = [DumpDir,"/etsdump.",integer_to_list(N)],
    case file:open(FileName,read) of
	{ok,Fd} ->
	    phys_load_entries(Fd,Tab);
	Other ->
	    {error,{open_error,Other}}
    end.

phys_load_entries(Fd,Tab) ->
    case phys_read_len(Fd) of
	{ok,Len} ->
	    case phys_read_entry(Fd,Len) of
		{ok,ok} ->
		    ok;
		{ok,{Key,Val}} ->
		    ?ets_insert(Tab,{Key,Val}),
		    phys_load_entries(Fd,Tab);
		Other ->
		    {error,{read_len,Other}}
	    end;
	Other ->
	    {error,{read_len2,Other}}
    end.

phys_read_len(Fd) ->
    case io:get_chars(Fd,'',4) of
	[A,B,C,D] ->
	    {ok,decode32(A,B,C,D)};
	Other ->
	    {error,{decode,Other}}
    end.

phys_read_entry(Fd,Len) ->
    case io:get_chars(Fd,'',Len) of
	L when is_list(L), length(L) == Len ->
	    {ok,binary_to_term(list_to_binary(L))};
	Other ->
	    {error,{read_term,Other}}
    end.

encode32(N) ->
    [(N bsr 24) band 255, 
     (N bsr 16) band 255, 
     (N bsr 8) band 255,
     N band 255].

decode32(A,B,C,D) ->
    (A bsl 24) bor (B bsl 16) bor (C bsl 8) bor D.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
