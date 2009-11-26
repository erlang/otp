%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(lcnt).
-behaviour(gen_server).
-author("Björn-Egil Dahlberg").

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
	]).

%% start/stop
-export([
	start/0,
	stop/0
	]).

%% gen_server call api
-export([
	raw/0,
	collect/0,
	collect/1,
	clear/0,
	clear/1,
	conflicts/0,
	conflicts/1,
	locations/0,
	locations/1,
	inspect/1,
	inspect/2,
	information/0,
	swap_pid_keys/0,
	% set options
	set/1,
	set/2,

	load/1,
	save/1
	]).

%% convenience
-export([
	apply/3,
	apply/2,
	help/0
    ]).

-define(version, "1.0").

-record(state, {
	locks      = [],
	duration   = 0
    }).


-record(stats, {
	file,
	line,
	tries,
	colls,
	time,	  % us
	nt	  % #timings collected
    }).

-record(lock, {
	name,
	id,
	type,
	stats = []

    }).

-record(print, {
	name,
	id,
	type,
	entry,
	tries,
	colls,
	cr,     % collision ratio
	time,
	dtr     % time duration ratio
    }).

%% -------------------------------------------------------------------- %%
%%
%% start/stop/init
%%
%% -------------------------------------------------------------------- %%

start()  -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).
stop()   -> gen_server:cast(?MODULE, stop).
init([]) -> {ok, #state{ locks = [], duration = 0 } }.

%% -------------------------------------------------------------------- %%
%%
%% API implementation
%%
%% -------------------------------------------------------------------- %%

clear()              -> erts_debug:lock_counters(clear).
clear(Node)          -> rpc:call(Node, erts_debug, lock_counters, [clear]).
collect()            -> gen_server:call(?MODULE, {collect, erts_debug:lock_counters(info)}).
collect(Node)        -> gen_server:call(?MODULE, {collect, rpc:call(Node, erts_debug, lock_counters, [info])}).
locations()          -> gen_server:call(?MODULE, {locations,[]}).
locations(Opts)      -> gen_server:call(?MODULE, {locations, Opts}).
conflicts()          -> gen_server:call(?MODULE, {conflicts, []}).
conflicts(Opts)      -> gen_server:call(?MODULE, {conflicts, Opts}).
inspect(Lock)        -> gen_server:call(?MODULE, {inspect, Lock, []}).
inspect(Lock, Opts)  -> gen_server:call(?MODULE, {inspect, Lock, Opts}).
information()        -> gen_server:call(?MODULE, information).
swap_pid_keys()      -> gen_server:call(?MODULE, swap_pid_keys).
raw()                -> gen_server:call(?MODULE, raw).
set(Option, Value)   -> gen_server:call(?MODULE, {set, Option, Value}).
set({Option, Value}) -> gen_server:call(?MODULE, {set, Option, Value}).
save(Filename)       -> gen_server:call(?MODULE, {save, Filename}, infinity).
load(Filename)       -> start(), gen_server:call(?MODULE, {load, Filename}, infinity).

%% -------------------------------------------------------------------- %%
%%
%% convenience implementation
%%
%% -------------------------------------------------------------------- %%

apply(M,F,As) when is_atom(M), is_atom(F), is_list(As) ->
    lcnt:start(),
    lcnt:clear(),
    Res = erlang:apply(M,F,As),
    lcnt:collect(),
    Res.

apply(Fun, As) when is_function(Fun) ->
    lcnt:start(),
    lcnt:clear(),
    Res = erlang:apply(Fun, As),
    lcnt:collect(),
    Res.

help() ->
    Help =
    "lcnt:conflicts() -> ok\n"
    "lcnt:conflicts(Opts) -> ok\n"
    "  Returns a list of internal lock counters.\n"
    "    Opts      = [Opt]\n"
    "    Opt       = {sort, Sort} | {threshold, Threshold} | {print, PrintOpts} | {max_locks, MaxLocks} {combine, bool()}, {location, bool()}\n"
    "    Sort      = name | id | type | tries | colls | ratio | time | entry\n"
    "    Threshold = {tries, integer()} | {colls, integer()} | {time, integer()}\n"
    "    PrintOpts = [PrintOpt | {PrintOpt, Width}]\n"
    "    PrintOpt  = name | id | type | entry | tries | colls | ratio | time | duration\n"
    "    Width     = integer()\n"
    "    MaxLocks  = integer()\n",
    io:format("~s", [Help]),
    ok.

%% -------------------------------------------------------------------- %%
%%
%% handle_call
%%
%% -------------------------------------------------------------------- %%

% printing

handle_call({conflicts, InOpts}, _From, #state{ locks = Locks } = State) when is_list(InOpts) ->
    Default = [
	{sort,       time},
	{reverse,    false},
	{print,      [name,id,tries,colls,ratio,time,duration]},
	{max_locks,  20},
	{combine,    true},
	{thresholds, [{tries, 0}, {colls, 0}, {time, 0}] },
	{locations,  false}],
    Opts       = options(InOpts, Default),
    Combos     = combine_classes(Locks, proplists:get_value(combine, Opts)),
    Printables = locks2print(Combos, State#state.duration),
    Filtered   = filter_print(Printables, Opts),

    print_lock_information(Filtered, proplists:get_value(print, Opts)),

    {reply, ok, State};

handle_call(information, _From, State) ->
    print_state_information(State),
    {reply, ok, State};

handle_call({locations, InOpts}, _From, #state{ locks = Locks } = State) when is_list(InOpts) ->
    Default = [
	{sort,       time},
	{reverse,    false},
	{print,      [name,id,tries,colls,ratio,time,duration]},
	{max_locks,  20},
	{combine,    true},
	{thresholds, [{tries, 0}, {colls, 0}, {time, 0}] },
	{locations,  false}],

    Opts = options(InOpts, Default),
    Printables = filter_print([#print{
	    name  = term2string("~w", [Names]),
	    entry = term2string("~p:~p", [Stats#stats.file, Stats#stats.line]),
	    colls = Stats#stats.colls,
	    tries = Stats#stats.tries,
	    cr    = percent(Stats#stats.colls, Stats#stats.tries),
	    time  = Stats#stats.time,
	    dtr   = percent(Stats#stats.time, State#state.duration)
	} || {Stats, Names} <- combine_locations(Locks) ], Opts),

    print_lock_information(Printables, proplists:get_value(print, Opts)),

    {reply, ok, State};

handle_call({inspect, Lockname, InOpts}, _From, #state{ duration = Duration, locks = Locks } = State) when is_list(InOpts) ->
    Default = [
	{sort,       time},
	{reverse,    false},
	{print,      [name,id,tries,colls,ratio,time,duration]},
	{max_locks,  20},
	{combine,    false},
	{thresholds, [{tries, 0}, {colls, 0}, {time, 0}] },
	{locations,  false}],

    Opts      = options(InOpts, Default),
    Filtered  = filter_locks(Lockname, Locks),
    IDs       = case {proplists:get_value(full_id, Opts), proplists:get_value(combine, Opts)} of
	{true, true} -> locks_ids(Filtered);
	_            -> []
    end,
    Combos    =  combine_classes(Filtered, proplists:get_value(combine, Opts)),
    case proplists:get_value(locations, Opts) of
	true ->
	    lists:foreach(fun
		(L) ->
		    IdString = case proplists:get_value(full_id, Opts) of
			true -> term2string(proplists:get_value(L#lock.name, IDs, L#lock.id));
			_    -> term2string(L#lock.id)
		    end,
		    print("lock: " ++ term2string(L#lock.name)),
		    print("id:   " ++ IdString),
		    print("type: " ++ term2string(L#lock.type)),
		    Combined = [Stats || {Stats,_} <- combine_locations(L#lock.stats)],
		    Ps = stats2print(Combined, Duration),
		    Opts1 = options([{{print, [entry, name,id,tries,colls,ratio,time,duration]},
		    print_lock_information(filter_print(Ps, Opts), proplists:get_value(print, Opts))
		end, Combos);
	_ ->
	    print_lock_information(locks2print(Combos, Duration), proplists:get_value(print, Opts))
    end,
    {reply, ok, State};

handle_call(raw, _From, #state{ locks = Locks} = State)->
    {reply, Locks, State};

% collecting
handle_call({collect, Data}, _From, State)->
    {reply, ok, data2state(Data, State)};

% manipulate
handle_call(swap_pid_keys, _From, #state{ locks = Locks } = State)->
    SwappedLocks = lists:map(fun
	(L) when L#lock.name =:= port_lock; L#lock.type =:= proclock ->
	    L#lock{ id = L#lock.name, name = L#lock.id };
	(L) ->
	    L
    end, Locks),

    {reply, ok, State#state{ locks = SwappedLocks}};

% settings
handle_call({set, data, Data}, _From, State)->
    {reply, ok, data2state(Data, State)};

% file operations
handle_call({load, Filename}, _From, State) ->
    case file:read_file(Filename) of
	{ok, Binary} ->
	    case binary_to_term(Binary) of
		{?version, Statelist} ->
		    {reply, ok, list2state(Statelist)};
		{Version, _} ->
		    {reply, {error, {mismatch, Version, ?version}}, State}
	    end;
	Error ->
	    {reply, {error, Error}, State}
    end;

handle_call({save, Filename}, _From, State) ->
    Binary = term_to_binary({?version, state2list(State)}),
    case file:write_file(Filename, Binary) of
	ok ->
	    {reply, ok, State};
	Error ->
	    {reply, {error, Error}, State}
    end;


handle_call(Command, _From, State) ->
    {reply, {error, {undefined, Command}}, State}.

%% -------------------------------------------------------------------- %%
%%
%% handle_cast
%%
%% -------------------------------------------------------------------- %%

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------- %%
%%
%% handle_info
%%
%% -------------------------------------------------------------------- %%

handle_info(_Info, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------- %%
%%
%% termination
%%
%% -------------------------------------------------------------------- %%

terminate(_Reason, _State) ->
    ok.

%% -------------------------------------------------------------------- %%
%%
%% code_change
%%
%% -------------------------------------------------------------------- %%

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -------------------------------------------------------------------- %%
%%
%% AUX
%%
%% -------------------------------------------------------------------- %%

% summate

summate_locks(Locks) -> summate_locks(Locks, #stats{ tries = 0, colls = 0, time = 0, nt = 0}).
summate_locks([], Stats) -> Stats;
summate_locks([L|Ls], #stats{ tries = Tries, colls = Colls, time = Time, nt = Nt}) ->
    S = summate_stats(L#lock.stats),
    summate_locks(Ls, #stats{ tries = Tries + S#stats.tries, colls = Colls + S#stats.colls, time = Time + S#stats.time, nt = Nt + S#stats.nt}).

summate_stats(Stats) -> summate_stats(Stats, #stats{ tries = 0, colls = 0, time = 0, nt = 0}).
summate_stats([], Stats) -> Stats;
summate_stats([S|Ss], #stats{ tries = Tries, colls = Colls, time = Time, nt = Nt}) ->
    summate_stats(Ss, #stats{ tries = Tries + S#stats.tries, colls = Colls + S#stats.colls, time = Time + S#stats.time, nt = Nt + S#stats.nt}).


%% manipulators

filter_locks({Lockname, Ids}, Locks) when is_list(Ids) ->
    [ L || L <- Locks, L#lock.name == Lockname, lists:member(L#lock.id, Ids)];
filter_locks({Lockname, Id}, Locks) ->
    [ L || L <- Locks, L#lock.name == Lockname, L#lock.id == Id ];
filter_locks(Lockname, Locks) ->
    [ L || L <- Locks, L#lock.name == Lockname ].
% order of processing
% 2. cut thresholds
% 3. sort locks
% 4. max length of locks

filter_print(PLs, Opts) ->
    TLs = threshold_locks(PLs, proplists:get_value(thresholds, Opts, [])),
    SLs =      sort_locks(TLs, proplists:get_value(sort,       Opts, time)),
    CLs =       cut_locks(SLs, proplists:get_value(max_locks,  Opts, none)),
	    reverse_locks(CLs, proplists:get_value(reverse,    Opts, false)).

sort_locks(Locks, Type)   -> lists:reverse(sort_locks0(Locks, Type)).
sort_locks0(Locks, name)  -> lists:keysort(#print.name, Locks);
sort_locks0(Locks, id)    -> lists:keysort(#print.id, Locks);
sort_locks0(Locks, type)  -> lists:keysort(#print.type, Locks);
sort_locks0(Locks, tries) -> lists:keysort(#print.tries, Locks);
sort_locks0(Locks, colls) -> lists:keysort(#print.colls, Locks);
sort_locks0(Locks, ratio) -> lists:keysort(#print.cr, Locks);
sort_locks0(Locks, time)  -> lists:keysort(#print.time, Locks);
sort_locks0(Locks, _)     -> sort_locks0(Locks, time).

% cut locks not above certain thresholds
threshold_locks(Locks, Thresholds) ->
    Tries = proplists:get_value(tries, Thresholds, -1),
    Colls = proplists:get_value(colls, Thresholds, -1),
    Time  = proplists:get_value(time,  Thresholds, -1),
    [ L || L <- Locks, L#print.tries > Tries, L#print.colls > Colls, L#print.time > Time].

cut_locks(Locks, N) when is_integer(N), N > 0 -> lists:sublist(Locks, N);
cut_locks(Locks, _) -> Locks.

%% reversal
reverse_locks(Locks, true) -> lists:reverse(Locks);
reverse_locks(Locks, _) -> Locks.

%% combine_locations
%% In:
%%	Locations :: [#lock{}] | [#stats{}]
%% Out:
%%	[{{File,Line}, #stats{}, [Lockname]}]


combine_locations(Locations)    -> gb_trees:values(combine_locations(Locations, gb_trees:empty())).
combine_locations([], Tree) -> Tree;
combine_locations([S|_] = Stats, Tree) when is_record(S, stats) ->
    combine_locations(Stats, undefined, Tree);
combine_locations([#lock{ stats = Stats, name = Name}|Ls], Tree)  ->
    combine_locations(Ls, combine_locations(Stats, Name, Tree)).

combine_locations([], _, Tree) -> Tree;
combine_locations([S|Ss], Name, Tree) when is_record(S, stats)->
    Key  = {S#stats.file, S#stats.line},
    Tree1 = case gb_trees:lookup(Key, Tree) of
	none ->
	    gb_trees:insert(Key, {S, [Name]}, Tree);
	{value, {C, Names}} ->
	    NewNames = case lists:member(Name, Names) of
		true -> Names;
		_    -> [Name | Names]
	    end,
	    gb_trees:update(Key, {
		C#stats{
		    tries = C#stats.tries + S#stats.tries,
		    colls = C#stats.colls + S#stats.colls,
		    time  = C#stats.time  + S#stats.time,
		    nt    = C#stats.nt    + S#stats.nt
		}, NewNames}, Tree)
    end,
    combine_locations(Ss, Name, Tree1).

%% combines all statistics for a class (name) lock
%% id's are translated to #id's.

combine_classes(Locks, true) ->  combine_classes1(Locks, gb_trees:empty());
combine_classes(Locks, _) -> Locks.

combine_classes1([], Tree) ->  gb_trees:values(Tree);
combine_classes1([L|Ls], Tree) ->
    Key = L#lock.name,
    case gb_trees:lookup(Key, Tree) of
	none ->
	    combine_classes1(Ls, gb_trees:insert(Key, L#lock{ id = 1 }, Tree));
	{value, C} ->
	    combine_classes1(Ls, gb_trees:update(Key, C#lock{
		id    = C#lock.id    + 1,
		stats = L#lock.stats ++ C#lock.stats
	    }, Tree))
    end.

locks_ids(Locks) -> locks_ids(Locks, []).
locks_ids([], Out) -> Out;
locks_ids([#lock{ name = Key } = L|Ls], Out) ->
    case proplists:get_value(Key, Out) of
	undefined ->
	    locks_ids(Ls, [{Key, [L#lock.id] } | Out]);
	Ids ->
	    locks_ids(Ls, [{Key, [L#lock.id | Ids] } | proplists:delete(Key,Out)])
    end.

stats2print(Stats, Duration) ->
    lists:map(fun
	(S) ->
	    #print{
		entry = term2string("~p:~p", [S#stats.file, S#stats.line]),
		colls = S#stats.colls,
		tries = S#stats.tries,
		cr    = percent(S#stats.colls, S#stats.tries),
		time  = S#stats.time,
		dtr   = percent(S#stats.time,  Duration)
	    }
	end, Stats).

locks2print(Locks, Duration) ->
    lists:map( fun
	(L) ->
	    Tries = lists:sum([T || #stats{ tries = T} <- L#lock.stats]),
	    Colls = lists:sum([C || #stats{ colls = C} <- L#lock.stats]),
	    Time  = lists:sum([T || #stats{ time  = T} <- L#lock.stats]),
	    Cr    = percent(Colls, Tries),
	    Dtr   = percent(Time,  Duration),
	    #print{
		name  = L#lock.name,
		id    = L#lock.id,
		type  = L#lock.type,
		tries = Tries,
		colls = Colls,
		cr    = Cr,
		time  = Time,
		dtr   = Dtr
	    }
	end, Locks).

%% state making

data2state(Data, State) ->
    Duration = time2us(proplists:get_value(duration, Data)),
    Rawlocks = proplists:get_value(locks, Data),
    Locks    = locks2records(Rawlocks),
    State#state{
	duration = Duration,
	locks    = Locks
    }.

% [{name, id, type, [{{file, line}, {tries, colls, {s, ns, n}}}]
locks2records(Locks) ->
    [  #lock{
	    name  = Name,
	    id    = Id,
	    type  = Type,
	    stats = [ #stats{
		file  = File,
		line  = Line,
		tries = Tries,
		colls = Colls,
		time  = time2us({S, Ns}),
		nt    = N
	    } || {{File, Line}, {Tries, Colls, {S, Ns, N}}} <- Stats]
	} ||  {Name, Id, Type, Stats} <- Locks
    ].


%% serializer

state_default(Field) -> proplists:get_value(Field, state2list(#state{})).

state2list(State) ->
    [_|Values] = tuple_to_list(State),
    lists:zipwith(fun
	(locks, Locks) -> {locks, [lock2list(Lock) || Lock <- Locks]};
	(X, Y) -> {X,Y}
    end, record_info(fields, state), Values).

list2state(List) -> list2state(record_info(fields, state), List, [state]).
list2state([], _, Out) -> list_to_tuple(lists:reverse(Out));
list2state([locks|Fs], List, Out) ->
    Locks = [ list2lock(Lock) || Lock <- proplists:get_value(locks, List, [])],
    list2state(Fs, List, [Locks|Out]);
list2state([F|Fs], List, Out) -> list2state(Fs, List, [proplists:get_value(F, List, state_default(F))|Out]).

lock_default(Field) -> proplists:get_value(Field, lock2list(#lock{})).

lock2list(Lock) ->
    [_|Values] = tuple_to_list(Lock),
    lists:zip(record_info(fields, lock), Values).

list2lock(List) -> list2lock(record_info(fields, lock), List, [lock]).
list2lock([], _, Out) -> list_to_tuple(lists:reverse(Out));
list2lock([F|Fs], List, Out) -> list2lock(Fs, List, [proplists:get_value(F, List, lock_default(F))|Out]).

%% printing

%% print_lock_information
%% In:
%%	Locks :: [#lock{}]
%%	Print :: [Type | {Type, integer()}]
%%
%% Out:
%%	ok

print_lock_information(Locks, Print) ->
    print_header(Print),
    lists:foreach(fun
	(L) ->
	    print_lock(L, Print)
    end, Locks),
    ok.

print_header(Opts) ->
    Header = #print{
	name  = "lock",
	id    = "id",
	type  = "type",
	entry = "location",
	tries = "#tries",
	colls = "#collisions",
	cr    = "collisions [%]",
	time  = "time [us]",
	dtr   = "duration [%]"
    },
    Divider = #print{
	name  = lists:duplicate(1 + length(Header#print.name),  45),
	id    = lists:duplicate(1 + length(Header#print.id),    45),
	type  = lists:duplicate(1 + length(Header#print.type),  45),
	entry = lists:duplicate(1 + length(Header#print.entry), 45),
	tries = lists:duplicate(1 + length(Header#print.tries), 45),
	colls = lists:duplicate(1 + length(Header#print.colls), 45),
	cr    = lists:duplicate(1 + length(Header#print.cr),    45),
	time  = lists:duplicate(1 + length(Header#print.time),  45),
	dtr   = lists:duplicate(1 + length(Header#print.dtr),   45)
    },
    print_lock(Header, Opts),
    print_lock(Divider, Opts),
    ok.


print_lock(L, Opts) -> print_lock(L, Opts, []).
print_lock(_, [],  Formats) -> print(strings(lists:reverse(Formats)));
print_lock(L, [Opt|Opts], Formats) ->
    case Opt of
	id            -> print_lock(L, Opts, [{space, 18, s(L#print.id)   } | Formats]);
	{id, W}       -> print_lock(L, Opts, [{space,  W, s(L#print.id)   } | Formats]);
	type          -> print_lock(L, Opts, [{space, 18, s(L#print.type) } | Formats]);
	{type, W}     -> print_lock(L, Opts, [{space,  W, s(L#print.type) } | Formats]);
	entry         -> print_lock(L, Opts, [{space, 30, s(L#print.entry)} | Formats]);
	{entry, W}    -> print_lock(L, Opts, [{space,  W, s(L#print.entry)} | Formats]);
	name          -> print_lock(L, Opts, [{space, 25, s(L#print.name) } | Formats]);
	{name, W}     -> print_lock(L, Opts, [{space,  W, s(L#print.name) } | Formats]);
	tries         -> print_lock(L, Opts, [{space, 12, s(L#print.tries)} | Formats]);
	{tries, W}    -> print_lock(L, Opts, [{space,  W, s(L#print.tries)} | Formats]);
	colls         -> print_lock(L, Opts, [{space, 14, s(L#print.colls)} | Formats]);
	{colls, W}    -> print_lock(L, Opts, [{space,  W, s(L#print.colls)} | Formats]);
	ratio         -> print_lock(L, Opts, [{space, 20, s(L#print.cr)   } | Formats]);
	{ratio, W}    -> print_lock(L, Opts, [{space,  W, s(L#print.cr)   } | Formats]);
	time          -> print_lock(L, Opts, [{space, 15, s(L#print.time) } | Formats]);
	{time, W}     -> print_lock(L, Opts, [{space,  W, s(L#print.time) } | Formats]);
	duration      -> print_lock(L, Opts, [{space, 20, s(L#print.dtr)  } | Formats]);
	{duration, W} -> print_lock(L, Opts, [{space,  W, s(L#print.dtr)  } | Formats]);
	_        -> print_lock(L, Opts, Formats)
    end.

print_state_information(#state{ locks = Locks} = State) ->
    Stats = summate_locks(Locks),
    print("information:"),
    print(kv("#locks",          s(length(Locks)))),
    print(kv("duration",        s(State#state.duration) ++ " us" ++ " (" ++ s(State#state.duration/1000000) ++ " s)")),
    print("\nsummated stats:"),
    print(kv("#tries",          s(Stats#stats.tries))),
    print(kv("#colls",          s(Stats#stats.colls))),
    print(kv("wait time",       s(Stats#stats.time) ++ " us" ++ " ( " ++ s(Stats#stats.time/1000000) ++ " s)")),
    print(kv("percent of duration", s(Stats#stats.time/State#state.duration*100) ++ " %")),
    ok.

%% AUX

time2us({S, Ns}) -> round(S*1000000 + Ns/1000).

percent(_,0) -> 0.0;
percent(T,N) -> T/N*100.

options(Opts, Default) when is_list(Default) ->
    options1(proplists:unfold(Opts), Default).
options1([], Defaults) -> Defaults;
options1([{Key, Value}|Opts], Defaults) ->
    case proplists:get_value(Key, Defaults) of
	undefined -> options1(Opts, [{Key, Value} | Defaults]);
	_         -> options1(Opts, [{Key, Value} | proplists:delete(Key, Defaults)])
    end.

%%% AUX STRING FORMATTING

print(String) -> io:format("~s~n", [String]).

kv(Key, Value) -> kv(Key, Value, 20).
kv(Key, Value, Offset) -> term2string(term2string("~~~ps : ~~s", [Offset]),[Key, Value]).

s(T) when is_float(T) -> term2string("~.4f", [T]);
s(T) when is_list(T)  -> term2string("~s", [T]);
s(T)                  -> term2string("~p", [T]).

strings(Strings) -> strings(Strings, []).
strings([], Out) -> Out;
strings([{space,  N,      S} | Ss], Out) -> strings(Ss, Out ++ term2string(term2string("~~~ps", [N]), [S]));
strings([{format, Format, S} | Ss], Out) -> strings(Ss, Out ++ term2string(Format, [S]));
strings([S|Ss], Out) -> strings(Ss, Out ++ term2string("~s", [S])).


term2string({M,F,A}) when is_atom(M), is_atom(F), is_integer(A) -> term2string("~p:~p/~p", [M,F,A]);
term2string(Term) -> term2string("~w", [Term]).
term2string(Format, Terms) -> lists:flatten(io_lib:format(Format, Terms)).
