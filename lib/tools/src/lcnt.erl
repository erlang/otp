%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

-module(lcnt).
-behaviour(gen_server).
-author("Björn-Egil Dahlberg").

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% start/stop
-export([start/0,
         stop/0]).

%% erts_debug:lcnt_xxx api
-export([rt_mask/0,
         rt_mask/1,
         rt_mask/2,
         rt_collect/0,
         rt_collect/1,
         rt_clear/0,
         rt_clear/1,
         rt_opt/1,
         rt_opt/2]).


%% gen_server call api
-export([raw/0,
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
         histogram/1,
         histogram/2,
         information/0,
         swap_pid_keys/0,
         % set options
         set/1,
         set/2,

         load/1,
         save/1]).

%% convenience
-export([apply/3,
         apply/2,
         apply/1,
         all_conflicts/0,
         all_conflicts/1,
         pid/2, pid/3,
         port/1, port/2]).

-define(version, "1.0").

-record(state, {
	locks      = [],
	duration   = 0
    }).

-record(stats, {
	file  :: atom(),
	line  :: non_neg_integer() | 'undefined',
	tries :: non_neg_integer(),
	colls :: non_neg_integer(),
	time  :: non_neg_integer(), % us
	nt    :: non_neg_integer(), % #timings collected
	hist  :: tuple() | 'undefined'  % histogram
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
	dtr,    % time duration ratio
	%% new
	hist    % log2 histogram of lock wait_time
    }).



%% -------------------------------------------------------------------- %%
%%
%% start/stop/init
%%
%% -------------------------------------------------------------------- %%

start()  -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).
stop()   -> gen_server:call(?MODULE, stop, infinity).
init([]) -> {ok, #state{ locks = [], duration = 0 } }.

start_internal() ->
    case start() of
        {ok,_} -> ok;
        {error, {already_started,_}} -> ok;
        Error -> Error
    end.

%% -------------------------------------------------------------------- %%
%%
%% API erts_debug:lcnt_xxx
%%
%% -------------------------------------------------------------------- %%

rt_mask(Node, Categories) when is_atom(Node), is_list(Categories) ->
    rpc:call(Node, lcnt, rt_mask, [Categories]).

rt_mask(Node) when is_atom(Node) ->
    rpc:call(Node, lcnt, rt_mask, []);

rt_mask(Categories) when is_list(Categories) ->
    case erts_debug:lcnt_control(copy_save) of
        false ->
            erts_debug:lcnt_control(mask, Categories);
        true ->
            {error, copy_save_enabled}
    end.

rt_mask() ->
    erts_debug:lcnt_control(mask).

rt_collect(Node) ->
    rpc:call(Node, lcnt, rt_collect, []).
rt_collect() ->
    erts_debug:lcnt_collect().

rt_clear(Node) ->
    rpc:call(Node, lcnt, rt_clear, []).
rt_clear() ->
    erts_debug:lcnt_clear().

rt_opt(Node, Arg) ->
    rpc:call(Node, lcnt, rt_opt, [Arg]).

%% Compatibility shims for the "process/port_locks" options mentioned in the
%% manual.
rt_opt({process_locks, Enable}) ->
    toggle_category(process, Enable);
rt_opt({port_locks, Enable}) ->
    toggle_category(io, Enable);

rt_opt({Type, NewVal}) ->
    PreviousVal = erts_debug:lcnt_control(Type),
    erts_debug:lcnt_control(Type, NewVal),
    PreviousVal.

toggle_category(Category, true) ->
    PreviousMask = erts_debug:lcnt_control(mask),
    erts_debug:lcnt_control(mask, [Category | PreviousMask]),
    lists:member(Category, PreviousMask);

toggle_category(Category, false) ->
    PreviousMask = erts_debug:lcnt_control(mask),
    erts_debug:lcnt_control(mask, lists:delete(Category, PreviousMask)),
    lists:member(Category, PreviousMask).

%% -------------------------------------------------------------------- %%
%%
%% API implementation
%%
%% -------------------------------------------------------------------- %%

clear()              -> rt_clear().
clear(Node)          -> rt_clear(Node).
collect()            -> call({collect, rt_collect()}).
collect(Node)        -> call({collect, rt_collect(Node)}).

locations()          -> call({locations,[]}).
locations(Opts)      -> call({locations, Opts}).
conflicts()          -> call({conflicts, []}).
conflicts(Opts)      -> call({conflicts, Opts}).
inspect(Lock)        -> call({inspect, Lock, []}).
inspect(Lock, Opts)  -> call({inspect, Lock, Opts}).
histogram(Lock)      -> call({histogram, Lock, []}).
histogram(Lock, Opts)-> call({histogram, Lock, Opts}).
information()        -> call(information).
swap_pid_keys()      -> call(swap_pid_keys).
raw()                -> call(raw).
set(Option, Value)   -> call({set, Option, Value}).
set({Option, Value}) -> call({set, Option, Value}).
save(Filename)       -> call({save, Filename}).
load(Filename)       -> call({load, Filename}).

call(Msg) ->
    ok = start_internal(),
    gen_server:call(?MODULE, Msg, infinity).

%% -------------------------------------------------------------------- %%
%%
%% convenience implementation
%%
%% -------------------------------------------------------------------- %%

apply(M,F,As) when is_atom(M), is_atom(F), is_list(As) ->
    apply(fun() ->
        erlang:apply(M,F,As)
    end).

apply(Fun) when is_function(Fun) ->
    lcnt:apply(Fun, []).

apply(Fun, As) when is_function(Fun) ->
    Opt = lcnt:rt_opt({copy_save, true}),
    lcnt:clear(),
    Res = erlang:apply(Fun, As),
    lcnt:collect(),
    %% _ is bound to silence a dialyzer warning; it used to fail silently and
    %% we don't want to change the error semantics.
    _ = lcnt:rt_opt({copy_save, Opt}),
    Res.

all_conflicts() -> all_conflicts(time).
all_conflicts(Sort) ->
    conflicts([{max_locks, none}, {thresholds, []},{combine,false}, {sort, Sort}, {reverse, true}]).

pid(Id, Serial) -> pid(node(), Id, Serial).
pid(Node, Id, Serial) when is_atom(Node) ->
    Header   = <<131,103,100>>,
    String   = atom_to_list(Node),
    L        = length(String),
    binary_to_term(list_to_binary([Header, bytes16(L), String, bytes32(Id), bytes32(Serial),0])).

port(Id) -> port(node(), Id).
port(Node, Id ) when is_atom(Node) ->
    Header   = <<131,102,100>>,
    String   = atom_to_list(Node),
    L        = length(String),
    binary_to_term(list_to_binary([Header, bytes16(L), String, bytes32(Id), 0])).

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
    Flocks     = filter_locks_type(Locks, proplists:get_value(type, Opts)),
    Combos     = combine_classes(Flocks, proplists:get_value(combine, Opts)),
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
	{print,      [name,entry,tries,colls,ratio,time,duration]},
	{max_locks,  20},
	{combine,    true},
	{thresholds, [{tries, 0}, {colls, 0}, {time, 0}] },
	{locations,  true}],

    Opts = options(InOpts, Default),
    Printables = filter_print([#print{
	    name  = string_names(Names),
	    entry = term2string("~tp:~p", [Stats#stats.file, Stats#stats.line]),
	    colls = Stats#stats.colls,
	    tries = Stats#stats.tries,
	    cr    = percent(Stats#stats.colls, Stats#stats.tries),
	    time  = Stats#stats.time,
	    dtr   = percent(Stats#stats.time, State#state.duration)
	} || {Stats, Names} <- combine_locations(Locks) ], Opts),

    print_lock_information(Printables, proplists:get_value(print, Opts)),

    {reply, ok, State};

handle_call({inspect, Lockname, InOpts}, _From, #state{ duration=Duration, locks=Locks } = State) when is_list(InOpts) ->
    Default = [
	{sort,       time},
	{reverse,    false},
	{print,      [name,id,tries,colls,ratio,time,duration,histogram]},
	{max_locks,  20},
	{combine,    false},
	{thresholds, []},
	{locations,  false}],

    Opts      = options(InOpts, Default),
    Filtered  = filter_locks(Locks, Lockname),
    IDs       = case {proplists:get_value(full_id, Opts), proplists:get_value(combine, Opts)} of
	{true, true} -> locks_ids(Filtered);
	_            -> []
    end,
    Combos = combine_classes(Filtered, proplists:get_value(combine, Opts)),
    case proplists:get_value(locations, Opts) of
	true ->
	    lists:foreach(fun
		    (#lock{ name = Name, id = Id, type = Type, stats =  Stats })  ->
			IdString = case proplists:get_value(full_id, Opts) of
			    true -> term2string(proplists:get_value(Name, IDs, Id));
			    _    -> term2string(Id)
			end,
			Combined = [CStats || {CStats,_} <- combine_locations(Stats)],
			case Combined of
			    [] ->
				ok;
			    _  ->
				print("lock: " ++ term2string(Name)),
				print("id:   " ++ IdString),
				print("type: " ++ term2string(Type)),
				Ps = stats2print(Combined, Duration),
				Opts1 = options([{print, [entry, tries,colls,ratio,time,duration,histogram]},
					{thresholds, [{tries, -1}, {colls, -1}, {time, -1}]}], Opts),
				print_lock_information(filter_print(Ps, Opts1), proplists:get_value(print, Opts1))
			end
		end, Combos);
	_ ->
	    Print = filter_print(locks2print(Combos, Duration), Opts),
	    print_lock_information(Print, proplists:get_value(print, Opts))
    end,
    {reply, ok, State};

%% histogram

handle_call({histogram, Lockname, InOpts}, _From, #state{ duration=Duration, locks=Locks} = State)->
    Default = [
	{sort,       time},
	{reverse,    false},
	{print,      [name,id,tries,colls,ratio,time,duration,histogram]},
	{max_locks,  20},
	{combine,    true},
	{thresholds, []},
	{locations,  false}],

    Opts     = options(InOpts, Default),
    Filtered = filter_locks(Locks, Lockname),
    Combos   = combine_classes(Filtered, proplists:get_value(combine, Opts)),
    lists:foreach(fun
	    (#lock{ stats = Stats }=L) ->
		SumStats = summate_stats(Stats),
		Opts1 = options([{print, [name,id,tries,colls,ratio,time,duration]},
			{thresholds, [{tries, -1}, {colls, -1}, {time, -1}]}], Opts),
		Prints = locks2print([L], Duration),
		print_lock_information(Prints, proplists:get_value(print, Opts1)),
		print_full_histogram(SumStats#stats.hist)
	end, Combos),

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

handle_call({set, duration, Duration}, _From, State)->
    {reply, ok, State#state{ duration = Duration}};

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

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Command, _From, State) ->
    {reply, {error, {undefined, Command}}, State}.

%% -------------------------------------------------------------------- %%
%%
%% handle_cast
%%
%% -------------------------------------------------------------------- %%

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
summate_locks([L|Ls], #stats{ tries = Tries, colls = Colls, time = Time, nt = Nt, hist = Hist}) ->
    S = summate_stats(L#lock.stats),
    summate_locks(Ls, #stats{
	    tries = Tries + S#stats.tries,
	    colls = Colls + S#stats.colls,
	    time  = Time + S#stats.time,
	    nt    = Nt + S#stats.nt,
	    hist  = summate_histogram(Hist, S#stats.hist)
	}).

summate_stats(Stats) -> summate_stats(Stats, #stats{ tries = 0, colls = 0, time = 0, nt = 0}).
summate_stats([], Stats) -> Stats;
summate_stats([S|Ss], #stats{ tries = Tries, colls = Colls, time = Time, nt = Nt, hist = Hist}) ->
    summate_stats(Ss, #stats{
	    tries = Tries + S#stats.tries,
	    colls = Colls + S#stats.colls,
	    time  = Time + S#stats.time,
	    nt    = Nt + S#stats.nt,
	    hist  = summate_histogram(Hist, S#stats.hist)
	}).

%% first call is undefined
summate_histogram(Tup,undefined) when is_tuple(Tup) -> Tup;
summate_histogram(undefined,Tup) when is_tuple(Tup) -> Tup;
summate_histogram(Hs1,Hs2) ->
    list_to_tuple([ A + B || {A,B} <- lists:zip(tuple_to_list(Hs1),tuple_to_list(Hs2))]).

%% manipulators
filter_locks_type(Locks, undefined) -> Locks;
filter_locks_type(Locks, all) -> Locks;
filter_locks_type(Locks, Types) when is_list(Types) ->
    [ L || L <- Locks, lists:member(L#lock.type, Types)];
filter_locks_type(Locks, Type) ->
    [ L || L <- Locks, L#lock.type =:= Type].

filter_locks(Locks, {Lockname, Ids}) when is_list(Ids) ->
    [ L || L <- Locks, L#lock.name =:= Lockname, lists:member(L#lock.id, Ids)];
filter_locks(Locks, {Lockname, Id}) ->
    [ L || L <- Locks, L#lock.name =:= Lockname, L#lock.id =:= Id ];
filter_locks(Locks, Lockname) ->
    [ L || L <- Locks, L#lock.name =:= Lockname ].
% order of processing
% 2. cut thresholds
% 3. sort locks
% 4. max length of locks

filter_print(PLs, Opts) ->
    TLs = threshold_locks(PLs, proplists:get_value(thresholds,  Opts, [])),
    SLs =      sort_locks(TLs, proplists:get_value(sort,        Opts, time)),
    CLs =       cut_locks(SLs, proplists:get_value(max_locks,   Opts, none)),
	    reverse_locks(CLs, proplists:get_value(reverse, Opts, false)).

sort_locks(Locks, name)  -> reverse_sort_locks(#print.name,  Locks);
sort_locks(Locks, id)    -> reverse_sort_locks(#print.id,    Locks);
sort_locks(Locks, type)  -> reverse_sort_locks(#print.type,  Locks);
sort_locks(Locks, tries) -> reverse_sort_locks(#print.tries, Locks);
sort_locks(Locks, colls) -> reverse_sort_locks(#print.colls, Locks);
sort_locks(Locks, ratio) -> reverse_sort_locks(#print.cr,    Locks);
sort_locks(Locks, time)  -> reverse_sort_locks(#print.time,  Locks);
sort_locks(Locks, _)     -> sort_locks(Locks, time).

reverse_sort_locks(Ix, Locks) ->
    lists:reverse(lists:keysort(Ix, Locks)).

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


%%
string_names([]) -> "";
string_names(Names) -> string_names(Names, []).
string_names([Name], Strings) -> strings(lists:reverse([term2string(Name) | Strings]));
string_names([Name|Names],Strings) -> string_names(Names, [term2string(Name) ++ ","|Strings]).

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
	undefined -> locks_ids(Ls, [{Key, [L#lock.id]}|Out]);
	Ids ->       locks_ids(Ls, [{Key, [L#lock.id|Ids]}|proplists:delete(Key,Out)])
    end.

stats2print(Stats, Duration) ->
    lists:map(fun
	(S) ->
	    #print{entry = term2string("~tp:~p", [S#stats.file, S#stats.line]),
		   colls = S#stats.colls,
		   tries = S#stats.tries,
		   cr    = percent(S#stats.colls, S#stats.tries),
		   time  = S#stats.time,
		   dtr   = percent(S#stats.time,  Duration),
		   hist  = format_histogram(S#stats.hist)}
	end, Stats).

locks2print(Locks, Duration) ->
    lists:map( fun
	(L) ->
	    #stats{tries = Tries,
		   colls = Colls,
		   time  = Time,
		   hist  = Hist} = summate_stats(L#lock.stats),
	    Cr  = percent(Colls, Tries),
	    Dtr = percent(Time,  Duration),
	    #print{name  = L#lock.name,
		   id    = L#lock.id,
		   type  = L#lock.type,
		   tries = Tries,
		   colls = Colls,
		   hist  = format_histogram(Hist),
		   cr    = Cr,
		   time  = Time,
		   dtr   = Dtr}
	end, Locks).


format_histogram(Tup) when is_tuple(Tup) ->
    Vs   = tuple_to_list(Tup),
    Max  = lists:max(Vs),
    case Max of
	0 -> string_histogram(Vs);
	_ -> string_histogram([case V of 0 -> 0; _ -> V/Max end || V <- Vs])
    end.

string_histogram(Vs) ->
    [$||histogram_values_to_string(Vs,$|)].

histogram_values_to_string([0|Vs],End) ->
    [$\s|histogram_values_to_string(Vs,End)];
histogram_values_to_string([V|Vs],End) when V > 0.66 ->
    [$X|histogram_values_to_string(Vs,End)];
histogram_values_to_string([V|Vs],End) when V > 0.33 ->
    [$x|histogram_values_to_string(Vs,End)];
histogram_values_to_string([_|Vs],End) ->
    [$.|histogram_values_to_string(Vs,End)];
histogram_values_to_string([],End) ->
    [End].

%% state making

data2state(Data, State) ->
    Duration = time2us(proplists:get_value(duration, Data)),
    Rawlocks = proplists:get_value(locks, Data),
    Locks    = locks2records(Rawlocks),
    State#state{
	duration = Duration,
	locks    = Locks
    }.

locks2records([{Name, Id, Type, Stats}|Locks]) ->
    [#lock{name  = Name,
	   id    = clean_id_creation(Id),
	   type  = Type,
	   stats = stats2record(Stats)}|locks2records(Locks)];
locks2records([]) -> [].

%% new stats with histogram
stats2record([{{File,Line},{Tries,Colls,{S,Ns,N}},Hist}|Stats]) ->
    [#stats{file  = File,
	    line  = Line,
	    hist  = Hist,
	    tries = Tries,
	    colls = Colls,
	    time  = time2us({S, Ns}),
	    nt    = N} | stats2record(Stats)];
%% old stats without histogram
stats2record([{{File,Line},{Tries,Colls,{S,Ns,N}}}|Stats]) ->
    [#stats{file  = File,
	    line  = Line,
	    hist  = {},
	    tries = Tries,
	    colls = Colls,
	    time  = time2us({S, Ns}),
	    nt    = N} | stats2record(Stats)];
stats2record([]) -> [].


clean_id_creation(Id) when is_pid(Id) ->
    Bin = term_to_binary(Id),
    <<H:3/binary, Rest/binary>> = Bin,
    <<131, PidTag, AtomTag>> = H,
    LL = atomlen_bits(AtomTag),
    CL = creation_bits(PidTag),
    <<L:LL, Node:L/binary, Ids:8/binary, _Creation/binary>> = Rest,
    Bin2 = list_to_binary([H, <<L:LL>>, Node, Ids, <<0:CL>>]),
    binary_to_term(Bin2);
clean_id_creation(Id) when is_port(Id) ->
    Bin = term_to_binary(Id),
    <<H:3/binary, Rest/binary>> = Bin,
    <<131, PortTag, AtomTag>> = H,
    LL = atomlen_bits(AtomTag),
    CL = creation_bits(PortTag),
    <<L:LL, Node:L/binary, Ids:4/binary, _Creation/binary>> = Rest,
    Bin2 = list_to_binary([H, <<L:LL>>, Node, Ids, <<0:CL>>]),
    binary_to_term(Bin2);
clean_id_creation(Id) ->
    Id.

-define(PID_EXT, $g).
-define(NEW_PID_EXT, $X).
-define(PORT_EXT, $f).
-define(NEW_PORT_EXT, $Y).
-define(ATOM_EXT, $d).
-define(SMALL_ATOM_EXT, $s).
-define(ATOM_UTF8_EXT, $v).
-define(SMALL_ATOM_UTF8_EXT, $w).

atomlen_bits(?ATOM_EXT) -> 16;
atomlen_bits(?SMALL_ATOM_EXT) -> 8;
atomlen_bits(?ATOM_UTF8_EXT) -> 16;
atomlen_bits(?SMALL_ATOM_UTF8_EXT) -> 8.

creation_bits(?PID_EXT) -> 8;
creation_bits(?NEW_PID_EXT) -> 32;
creation_bits(?PORT_EXT) -> 8;
creation_bits(?NEW_PORT_EXT) -> 32.

%% serializer

state_default(Field) -> proplists:get_value(Field, state2list(#state{})).

state2list(State) ->
    [_|Values] = tuple_to_list(State),
    lists:zipwith(fun
	(locks, Locks) -> {locks, [lock2list(Lock) || Lock <- Locks]};
	(X, Y) -> {X,Y}
    end, record_info(fields, state), Values).

lock_default(Field) -> proplists:get_value(Field, lock2list(#lock{})).

lock2list(Lock) ->
    [_|Values] = tuple_to_list(Lock),
    lists:zip(record_info(fields, lock), Values).


list2state(List) ->
    list_to_tuple([state|list2state(record_info(fields, state), List)]).
list2state([], _) -> [];
list2state([locks|Fs], List) ->
    Locks = [list2lock(Lock) || Lock <- proplists:get_value(locks, List, [])],
    [Locks|list2state(Fs,List)];
list2state([F|Fs], List) ->
    [proplists:get_value(F, List, state_default(F))|list2state(Fs, List)].

list2lock(Ls) ->
    list_to_tuple([lock|list2lock(record_info(fields, lock), Ls)]).

list2lock([],_) -> [];
list2lock([stats=F|Fs], Ls) ->
    Stats = stats2stats(proplists:get_value(F, Ls, lock_default(F))),
    [Stats|list2lock(Fs, Ls)];
list2lock([F|Fs], Ls) ->
    [proplists:get_value(F, Ls, lock_default(F))|list2lock(Fs, Ls)].

%% process old stats (hack)
%% old stats had no histograms
%% in future versions stats should be serialized as a list, not a record

stats2stats([]) -> [];
stats2stats([Stat|Stats]) ->
    Sz = record_info(size, stats),
    [stat2stat(Stat,Sz)|stats2stats(Stats)].

stat2stat(Stat,Sz) when tuple_size(Stat) =:= Sz -> Stat;
stat2stat(Stat,_) ->
    %% assume no histogram at the end
    list_to_tuple(tuple_to_list(Stat) ++ [{0}]).

%% printing

%% print_lock_information
%% In:
%%	Locks :: [#lock{}]
%%	Print :: [Type | {Type, integer()}]
%%
%% Out:
%%	ok

auto_print_width(Locks, Print) ->
    % iterate all lock entries to save all max length values
    % these are records, so we do a little tuple <-> list smashing
    R = lists:foldl(fun
	(L, Max) ->
		list_to_tuple(lists:reverse(lists:foldl(fun
		    ({print,print}, Out) -> [print|Out];
		    ({Str, Len}, Out)    -> [erlang:min(erlang:max(length(s(Str))+1,Len),80)|Out]
		end, [], lists:zip(tuple_to_list(L), tuple_to_list(Max)))))
	end, #print{ id=4, type=5, entry=5, name=6, tries=8, colls=13, cr=16, time=11, dtr=14, hist=20 },
	Locks),
    % Setup the offsets for later pruning
    Offsets = [
	{id, R#print.id},
	{name, R#print.name},
	{type, R#print.type},
	{entry, R#print.entry},
	{tries, R#print.tries},
	{colls, R#print.colls},
	{ratio, R#print.cr},
	{time, R#print.time},
	{duration, R#print.dtr},
	{histogram, R#print.hist}
    ],
    % Prune offsets to only allow specified print options
    lists:foldr(fun
	    ({Type, W}, Out) -> [{Type, W}|Out];
	    (Type, Out)      -> [proplists:lookup(Type, Offsets)|Out]
	end, [], Print).

print_lock_information(Locks, Print) ->
    % remake Print to autosize entries
    AutoPrint = auto_print_width(Locks, Print),
    print_header(AutoPrint),
    lists:foreach(fun
	(L) ->
	    print_lock(L, AutoPrint)
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
	dtr   = "duration [%]",
	hist  = "histogram [log2(us)]"
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
	dtr   = lists:duplicate(1 + length(Header#print.dtr),   45),
	hist  = lists:duplicate(1 + length(Header#print.hist),  45)
    },
    print_lock(Header, Opts),
    print_lock(Divider, Opts),
    ok.


print_lock(L, Opts) ->
    print(strings(format_lock(L, Opts))).

format_lock(_, []) -> [];
format_lock(L, [Opt|Opts]) ->
    case Opt of
	id             -> [{space, 25, s(L#print.id)   } | format_lock(L, Opts)];
	{id, W}        -> [{space,  W, s(L#print.id)   } | format_lock(L, Opts)];
	type           -> [{space, 18, s(L#print.type) } | format_lock(L, Opts)];
	{type, W}      -> [{space,  W, s(L#print.type) } | format_lock(L, Opts)];
	entry          -> [{space, 30, s(L#print.entry)} | format_lock(L, Opts)];
	{entry, W}     -> [{space,  W, s(L#print.entry)} | format_lock(L, Opts)];
	name           -> [{space, 22, s(L#print.name) } | format_lock(L, Opts)];
	{name, W}      -> [{space,  W, s(L#print.name) } | format_lock(L, Opts)];
	tries          -> [{space, 12, s(L#print.tries)} | format_lock(L, Opts)];
	{tries, W}     -> [{space,  W, s(L#print.tries)} | format_lock(L, Opts)];
	colls          -> [{space, 14, s(L#print.colls)} | format_lock(L, Opts)];
	{colls, W}     -> [{space,  W, s(L#print.colls)} | format_lock(L, Opts)];
	ratio          -> [{space, 20, s(L#print.cr)   } | format_lock(L, Opts)];
	{ratio, W}     -> [{space,  W, s(L#print.cr)   } | format_lock(L, Opts)];
	time           -> [{space, 15, s(L#print.time) } | format_lock(L, Opts)];
	{time, W}      -> [{space,  W, s(L#print.time) } | format_lock(L, Opts)];
	duration       -> [{space, 20, s(L#print.dtr)  } | format_lock(L, Opts)];
	{duration, W}  -> [{space,  W, s(L#print.dtr)  } | format_lock(L, Opts)];
	histogram      -> [{space, 20, s(L#print.hist) } | format_lock(L, Opts)];
	{histogram, W} -> [{left,  W - length(s(L#print.hist)) - 1, s(L#print.hist)} | format_lock(L, Opts)];
	_              -> format_lock(L, Opts)
    end.

print_state_information(#state{locks = Locks} = State) ->
    Stats = summate_locks(Locks),
    print("information:"),
    print(kv("#locks",          s(length(Locks)))),
    print(kv("duration",        s(State#state.duration) ++ " us" ++ " (" ++ s(State#state.duration/1000000) ++ " s)")),
    print("\nsummated stats:"),
    print(kv("#tries",          s(Stats#stats.tries))),
    print(kv("#colls",          s(Stats#stats.colls))),
    print(kv("wait time",       s(Stats#stats.time) ++ " us" ++ " ( " ++ s(Stats#stats.time/1000000) ++ " s)")),
    print(kv("percent of duration", s(percent(Stats#stats.time, State#state.duration)) ++ " %")),
    ok.


print_full_histogram(T) when is_tuple(T) ->
    Vs = tuple_to_list(T),
    Max = lists:max(Vs),
    W = 60,
    print_full_histogram(0,Vs,Max,W).

print_full_histogram(_,[],_,_) -> ok;
print_full_histogram(Ix,[V|Vs],0,W) ->
    io:format("~2w = log2 : ~8w |~n", [Ix,V]),
    print_full_histogram(Ix+1,Vs,0,W);
print_full_histogram(Ix,[V|Vs],Max,W) ->
    io:format("~2w = log2 : ~8w | ~s~n", [Ix,V,lists:duplicate(trunc(W*(V/Max)), $#)]),
    print_full_histogram(Ix+1,Vs,Max,W).


%% AUX

time2us({S, Ns}) -> S*1000000 + (Ns div 1000).

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

print(String) -> io:format("~ts~n", [String]).

kv(Key, Value) -> kv(Key, Value, 20).
kv(Key, Value, Offset) -> term2string(term2string("~~~ps : ~~s", [Offset]),[Key, Value]).

s(T) when is_float(T) -> term2string("~.4f", [T]);
s(T) when is_list(T)  -> term2string("~ts", [T]);
s(T)                  -> term2string(T).

strings(Strings) -> strings(Strings, []).
strings([], Out) -> Out;
strings([{space,  N,      S} | Ss], Out) -> strings(Ss, Out ++ term2string(term2string("~~~ws", [N]), [S]));
strings([{left,   N,      S} | Ss], Out) -> strings(Ss, Out ++ term2string(term2string(" ~~s~~~ws", [N]), [S,""]));
strings([S|Ss], Out) -> strings(Ss, Out ++ term2string("~ts", [S])).


term2string({M,F,A}) when is_atom(M), is_atom(F), is_integer(A) -> term2string("~p:~p/~p", [M,F,A]);
term2string(Term) when is_port(Term) ->
    %  ex #Port<6442.816>
    case term_to_binary(Term) of
        <<_:2/binary, ?SMALL_ATOM_UTF8_EXT, L:8, Node:L/binary, Ids:32, _/binary>> ->
            term2string("#Port<~ts.~w>", [Node, Ids]);
        <<_:2/binary, ?ATOM_UTF8_EXT, L:16, Node:L/binary, Ids:32, _/binary>> ->
            term2string("#Port<~ts.~w>", [Node, Ids]);
        <<_:2/binary, ?ATOM_EXT, L:16, Node:L/binary, Ids:32, _/binary>> ->
            term2string("#Port<~s.~w>", [Node, Ids])
    end;
term2string(Term) when is_pid(Term) ->
    %  ex <0.80.0>
    case  term_to_binary(Term) of
        <<_:2/binary, ?SMALL_ATOM_UTF8_EXT, L:8, Node:L/binary, Ids:32, Serial:32,  _/binary>> ->
            term2string("<~ts.~w.~w>", [Node, Ids, Serial]);
        <<_:2/binary, ?ATOM_UTF8_EXT, L:16, Node:L/binary, Ids:32, Serial:32,  _/binary>> ->
            term2string("<~ts.~w.~w>", [Node, Ids, Serial]);
        <<_:2/binary, ?ATOM_EXT, L:16, Node:L/binary, Ids:32, Serial:32,  _/binary>> ->
            term2string("<~s.~w.~w>", [Node, Ids, Serial])
    end;
term2string(Term) -> term2string("~w", [Term]).
term2string(Format, Terms) -> lists:flatten(io_lib:format(Format, Terms)).

%%% AUX id binary

bytes16(Value) ->
    B0 =  Value band 255,
    B1 = (Value bsr 8) band 255,
    <<B1, B0>>.

bytes32(Value) ->
    B0 =  Value band 255,
    B1 = (Value bsr  8) band 255,
    B2 = (Value bsr 16) band 255,
    B3 = (Value bsr 24) band 255,
    <<B3, B2, B1, B0>>.
