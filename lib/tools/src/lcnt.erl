%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2025. All Rights Reserved.
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
-moduledoc """
A runtime system Lock Profiling tool.

The `lcnt` module is used to profile the internal ethread locks in the Erlang
Runtime System. With `lcnt` enabled, internal counters in the runtime system are
updated each time a lock is taken. The counters stores information about the
number of acquisition tries and the number of collisions that has occurred
during the acquisition tries. The counters also record the waiting time a lock
has caused for a blocked thread when a collision has occurred.

The data produced by the lock counters will give an estimate on how well the
runtime system will behave from a parallelizable view point for the scenarios
tested. This tool was mainly developed to help Erlang runtime developers iron
out potential and generic bottlenecks.

Locks in the emulator are named after what type of resource they protect and
where in the emulator they are initialized, those are lock 'classes'. Most of
those locks are also instantiated several times, and given unique identifiers,
to increase locking granularity. Typically an instantiated lock protects a
disjunct set of the resource, for example ets tables, processes or ports. In
other cases it protects a specific range of a resource, for example `pix_lock`
which protects index to process mappings, and is given a unique number within
the class. A unique lock in `lcnt` is referenced by a name (class) and an
identifier: `{Name, Id}`.

Some locks in the system are static and protects global resources, for example
`bif_timers` and the `run_queue` locks. Other locks are dynamic and not
necessarily long lived, for example process locks and ets-table locks. The
statistics data from short lived locks can be stored separately when the locks
are deleted. This behavior is by default turned off to save memory but can be
turned on via `lcnt:rt_opt({copy_save, true})`. The `lcnt:apply/1,2,3` functions
enables this behavior during profiling.

## See Also

[LCNT User's Guide](lcnt_chapter.md)
""".
-moduledoc(#{since => "OTP R13B04"}).
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

-doc """
Starts the lock profiler server.

The server only act as a medium for the user and performs filtering
and printing of data collected by `lcnt:collect/1`.
""".
-doc(#{since => <<"OTP R13B04">>}).
-spec start() -> {'ok', Pid} | {'error', {'already_started', Pid}} when
      Pid :: pid().

start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).

-doc "Stops the lock profiler server.".
-doc(#{since => <<"OTP R13B04">>}).
-spec stop() -> 'ok'.

stop()-> gen_server:stop(?MODULE, normal, infinity).

-doc false.
init([]) -> {ok, #state{ locks = [], duration = 0 } }.

-dialyzer({no_match, start_internal/0}).
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

-doc """
Sets the lock category mask according to `Categories` on node `Node`.

This call will fail if the `copy_save` option is enabled; see
[`lcnt:rt_opt/2`](`rt_opt/2`).

Valid categories are:

- `allocator`
- `db` (ETS tables)
- `debug`
- `distribution`
- `generic`
- `io`
- `process`
- `scheduler`

This list is subject to change at any time, as is the category any given lock
belongs to.
""".
-doc(#{group => <<"Internal runtime lock counter controllers">>,
       since => <<"OTP 20.1">>}).
-spec rt_mask(Node, Categories) ->  'ok' | {'error', 'copy_save_enabled'} when
      Node :: node(),
      Categories :: [category_atom()].

rt_mask(Node, Categories) when is_atom(Node), is_list(Categories) ->
    rpc:call(Node, lcnt, rt_mask, [Categories]).

-type category_atom() :: atom().

-doc """
rt_mask(Arg)

Sets the current lock category mask for the current node or
retrieves the current mask for a remote node.

If `Arg` is an atom, it is assumed to be a node, and this
call returns the current lock category mask for node `Arg`.

If `Arg` is a list, this call is equivalent to
[`rt_mask(node(), Arg)`](`rt_mask/2`).
""".
-doc(#{group => <<"Internal runtime lock counter controllers">>,
       since => <<"OTP 20.1">>}).
-spec rt_mask(Node) -> [category_atom()] when
                  Node :: node();
             (Categories) -> 'ok' | {'error', 'copy_save_enabled'} when
                  Categories :: [category_atom()].

rt_mask(Node) when is_atom(Node) ->
    rpc:call(Node, lcnt, rt_mask, []);
rt_mask(Categories) when is_list(Categories) ->
    case erts_debug:lcnt_control(copy_save) of
        false ->
            erts_debug:lcnt_control(mask, Categories);
        true ->
            {error, copy_save_enabled}
    end.

-doc """
Return the current category mask for the current node.
""".
-doc(#{group => <<"Internal runtime lock counter controllers">>,
       since => <<"OTP 20.1">>}).
-spec rt_mask() -> [category_atom()].

rt_mask() ->
    erts_debug:lcnt_control(mask).

-type lock_counter_data() :: term().

-doc "Returns a list of raw lock counter data.".
-doc(#{group => <<"Internal runtime lock counter controllers">>,
       since => <<"OTP R13B04">>}).
-spec rt_collect(Node) -> [lock_counter_data()] when
      Node :: node().

rt_collect(Node) ->
    rpc:call(Node, lcnt, rt_collect, []).

-doc #{equiv => rt_collect(node())}.
-doc(#{group => <<"Internal runtime lock counter controllers">>,
       since => <<"OTP R13B04">>}).
-spec rt_collect() -> [lock_counter_data()].

rt_collect() ->
    erts_debug:lcnt_collect().

-doc """
Clear the internal counters.

Equivalent to [`lcnt:clear(Node)`](`clear/1`).
""".
-doc(#{group => <<"Internal runtime lock counter controllers">>,
       since => <<"OTP R13B04">>}).
-spec rt_clear(Node) -> 'ok' when
      Node :: node().

rt_clear(Node) ->
    rpc:call(Node, lcnt, rt_clear, []).

-doc #{equiv => rt_clear(node())}.
-doc(#{group => <<"Internal runtime lock counter controllers">>,
       since => <<"OTP R13B04">>}).
-spec rt_clear() -> 'ok'.

rt_clear() ->
    erts_debug:lcnt_clear().

-doc """
Sets a single option on node `Node`.

Option description:

- **`{copy_save, boolean()}`** - Retains the statistics of destroyed locks.  
  Default: `false`

  > #### Warning {: .warning }
  >
  > This option will use a lot of memory when enabled, which must be reclaimed
  > with [`lcnt:rt_clear/0,1`](`lcnt:rt_clear/1`). Note that it makes no
  > distinction between locks that  were destroyed and locks for which counting
  > was disabled, so enabling this option will disable changes to the lock
  > category mask.

- **`{process_locks, boolean()}`** - Profile process locks, equal to adding
  `process` to the lock category mask; see `lcnt:rt_mask/2`.  
  Default: `true`
""".
-doc(#{group => <<"Internal runtime lock counter controllers">>,
       since => <<"OTP R13B04">>}).
-spec rt_opt(Node, Option) -> boolean() when
      Node :: node(),
      Option :: {Type, Value :: boolean()},
      Type :: 'copy_save' | 'process_locks'.

rt_opt(Node, Arg) ->
    rpc:call(Node, lcnt, rt_opt, [Arg]).

-doc #{equiv => rt_opt(node(), {Type, Value})}.
-doc(#{group => <<"Internal runtime lock counter controllers">>,
       since => <<"OTP R13B04">>}).
-spec rt_opt(Option) -> boolean() when
      Option :: {Type, Value :: boolean()},
      Type :: 'copy_save' | 'process_locks'.

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

-doc #{equiv => clear(node())}.
-doc(#{since => <<"OTP R13B04">>}).
-spec clear() -> 'ok'.

clear() -> rt_clear().

-doc """
Clears the internal lock statistics from the runtime system.

This clears the data in the runtime system but not in server.  All
counters for static locks are zeroed, all dynamic locks currently
alive are zeroed and all saved locks now destroyed are removed. It
also resets the duration timer.

""".
-doc(#{since => <<"OTP R13B04">>}).
-spec clear(Node) -> 'ok' when
      Node :: node().

clear(Node) -> rt_clear(Node).

-doc #{equiv => collect(node())}.
-doc(#{since => <<"OTP R13B04">>}).
-spec collect() -> 'ok'.

collect() -> call({collect, rt_collect()}).

-doc """
Collects lock statistics from the runtime system.

The function starts a server if it is not already started. It then
populates the server with lock statistics.  If the server held any
lock statistics data before the collect then that data is lost.

""".
-doc(#{since => <<"OTP R13B04">>}).
-spec collect(Node) -> 'ok' when
      Node :: node().

collect(Node) -> call({collect, rt_collect(Node)}).

-doc #{equiv => locations([])}.
-doc(#{since => <<"OTP R13B04">>}).
-spec locations() -> 'ok'.

locations() -> call({locations,[]}).

-doc """
Prints a list of internal lock counters by source code locations.

For option description, see [`lcnt:inspect/2`](`inspect/2`).
""".
-doc(#{since => <<"OTP R13B04">>}).
-spec locations(Options) -> 'ok' when
      Options :: [option()].

locations(Opts) -> call({locations, Opts}).

-doc #{equiv => conflicts([])}.
-doc(#{since => <<"OTP R13B04">>}).
-spec conflicts() -> 'ok'.

conflicts() -> call({conflicts, []}).

-type sort() :: 'colls' | 'entry' | 'id' | 'name' | 'ratio' | 'time' |
                'tries' | 'type'.

-type threshold() :: {'colls', non_neg_integer()}
                   | {'time', non_neg_integer()}
                   | {'tries', non_neg_integer()}.

-type print() :: 'colls' | 'duration' | 'entry' | 'id' | 'name' |
                 'ratio' | 'time' | 'tries' | 'type'.

-type option() :: {'sort', Sort :: sort()}
                | {'reverse', boolean()}
                | {'locations', boolean()}
                | {'thresholds', Thresholds :: [threshold()]}
                | {'print',
                   PrintOptions :: [print() | {print(), non_neg_integer()}]}
                | {'max_locks', MaxLocks :: non_neg_integer() | 'none'}
                | {'combine', boolean()}.

-doc """
Prints a list of internal locks and its statistics.

For option description, see [`lcnt:inspect/2`](`inspect/2`).
""".
-doc(#{since => <<"OTP R13B04">>}).
-spec conflicts(Options) -> 'ok' when
      Options :: [option()].

conflicts(Opts)      -> call({conflicts, Opts}).

-doc #{equiv => inspect(Lock, [])}.
-doc(#{since => <<"OTP R13B04">>}).
-spec inspect(Lock) -> 'ok' when
      Lock :: Name | {Name, Id | [Id]},
      Name :: atom() | pid() | port(),
      Id :: atom() | integer() | pid() | port().

inspect(Lock) -> call({inspect, Lock, []}).

-doc """
Prints a list of internal lock counters for a specific lock.

Lock `Name` and `Id` for ports and processes are interchangeable with the use of
[`lcnt:swap_pid_keys/0`](`swap_pid_keys/0`) and is the reason why `t:pid/0` and
`t:port/0` options can be used in both `Name` and `Id` space. Both pids and
ports are special identifiers with stripped creation and can be recreated with
[`lcnt:pid/2,3`](`pid/3`) and [`lcnt:port/1,2`](`port/2`).

Option description:

- **`{combine, boolean()}`** - Combine the statistics from different instances
  of a lock class.  
  Default: `true`

- **`{locations, boolean()}`** - Print the statistics by source file and line
  numbers.  
  Default: `false`

- **`{max_locks, MaxLocks}`** - Maximum number of locks printed or no limit with
  `none`.  
  Default: `20`

- **`{print, PrintOptions}`** - Printing options:

  - **`name`** - Named lock or named set of locks (classes). The same name used
    for initializing the lock in the VM.

  - **`id`** - Internal id for set of locks, not always unique. This could be
    table name for ets tables (db_tab), port id for ports, integer identifiers
    for allocators, etc.

  - **`type`** - Type of lock: `rw_mutex`, `mutex`, `spinlock`, `rw_spinlock` or
    `proclock`.

  - **`entry`** - In combination with `{locations, true}` this option prints the
    lock operations source file and line number entry-points along with
    statistics for each entry.

  - **`tries`** - Number of acquisitions of this lock.

  - **`colls`** - Number of collisions when a thread tried to acquire this lock.
    This is when a trylock is EBUSY, a write try on read held rw_lock, a try
    read on write held rw_lock, a thread tries to lock an already locked lock.
    (Internal states supervises this.)

  - **`ratio`** - The ratio between the number of collisions and the number of
    tries (acquisitions) in percentage.

  - **`time`** - Accumulated waiting time for this lock. This could be greater
    than actual wall clock time, it is accumulated for all threads. Trylock
    conflicts does not accumulate time.

  - **`duration`** - Percentage of accumulated waiting time of wall clock time.
    This percentage can be higher than 100% since accumulated time is from all
    threads.

  Default: `[name,id,tries,colls,ratio,time,duration]`

- **`{reverse, boolean()}`** - Reverses the order of sorting.  
  Default: `false`

- **`{sort, Sort}`** - Column sorting orders.  
  Default: `time`

- **`{thresholds, Thresholds}`** - Filtering thresholds. Anything values above
  the threshold value are passed through.  
  Default: `[{tries, 0}, {colls, 0}, {time, 0}]`
""".
-doc(#{since => <<"OTP R13B04">>}).
-spec inspect(Lock, Options) -> 'ok' when
      Lock :: Name | {Name, Id | [Id]},
      Name :: atom() | pid() | port(),
      Id :: atom() | integer() | pid() | port(),
      Options :: [option()].

inspect(Lock, Opts)  -> call({inspect, Lock, Opts}).

-doc false.
histogram(Lock)      -> call({histogram, Lock, []}).
-doc false.
histogram(Lock, Opts)-> call({histogram, Lock, Opts}).

-doc """
Prints `lcnt` server state and generic information about collected lock
statistics.
""".
-doc(#{since => <<"OTP R13B04">>}).
-spec information() -> 'ok'.

information()        -> call(information).

-doc "Swaps places on `Name` and `Id` space for ports and processes.".
-doc(#{since => <<"OTP R13B04">>}).
-spec swap_pid_keys() -> 'ok'.

swap_pid_keys()      -> call(swap_pid_keys).

-doc false.
raw()                -> call(raw).
-doc false.
set(Option, Value)   -> call({set, Option, Value}).
-doc false.
set({Option, Value}) -> call({set, Option, Value}).

-doc "Saves the collected data to file.".
-doc(#{since => <<"OTP R13B04">>}).
-spec save(Filename) -> 'ok' when
      Filename :: file:filename().

save(Filename)       -> call({save, Filename}).

-doc "Restores previously saved data to the server.".
-doc(#{since => <<"OTP R13B04">>}).
-spec load(Filename) -> 'ok' when
      Filename :: file:filename().

load(Filename)       -> call({load, Filename}).

call(Msg) ->
    ok = start_internal(),
    gen_server:call(?MODULE, Msg, infinity).

%% -------------------------------------------------------------------- %%
%%
%% convenience implementation
%%
%% -------------------------------------------------------------------- %%

-doc #{equiv => apply(fun() -> erlang:apply(Module, Function, Args) end)}.
-doc(#{group => <<"Convenience functions">>,
       since => <<"OTP R13B04">>}).
-spec apply(Module, Function, Args) -> term() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].

apply(M, F, As) when is_atom(M), is_atom(F), is_list(As) ->
    apply(fun() ->
                  erlang:apply(M, F, As)
          end).

-doc #{equiv => apply(Fun, [])}.
-doc(#{group => <<"Convenience functions">>,
       since => <<"OTP R13B04">>}).
-spec apply(Fun) -> term() when
      Fun :: fun().

apply(Fun) when is_function(Fun) ->
    lcnt:apply(Fun, []).

-doc """
Sets up lock counters, applies `Fun` with `Args`, and cleans up.

Clears the lock counters and then setups the instrumentation to save all
destroyed locks. After setup the function is called, passing the elements in
`Args` as arguments. When the function returns the statistics are immediately
collected to the server. After the collection the instrumentation is returned to
its previous behavior. The result of the applied function is returned.

> #### Warning {: .warning }
>
> This function should only be used for micro-benchmarks; it sets `copy_save` to
> `true` for the duration of the call, which can quickly lead to running out of
> memory.
""".
-doc(#{group => <<"Convenience functions">>,since => <<"OTP R13B04">>}).
-spec apply(Fun, Args) -> term() when
      Fun :: fun(),
      Args :: [term()].

apply(Fun, As) when is_function(Fun) ->
    Opt = lcnt:rt_opt({copy_save, true}),
    lcnt:clear(),
    Res = erlang:apply(Fun, As),
    lcnt:collect(),
    %% _ is bound to silence a dialyzer warning; it used to fail silently and
    %% we don't want to change the error semantics.
    _ = lcnt:rt_opt({copy_save, Opt}),
    Res.

-doc false.
all_conflicts() -> all_conflicts(time).
-doc false.
all_conflicts(Sort) ->
    conflicts([{max_locks, none}, {thresholds, []},{combine,false}, {sort, Sort}, {reverse, true}]).

-doc #{equiv => pid(node(), Id, Serial)}.
-doc(#{group => <<"Convenience functions">>,since => <<"OTP R13B04">>}).
-spec pid(Id, Serial) -> pid() when
      Id :: integer(),
      Serial :: integer().

pid(Id, Serial) -> pid(node(), Id, Serial).

-doc "Creates a process id with creation 0.".
-doc(#{group => <<"Convenience functions">>,since => <<"OTP R13B04">>}).
-spec pid(Node, Id, Serial) -> pid() when
      Node :: node(),
      Id :: integer(),
      Serial :: integer().

pid(Node, Id, Serial) when is_atom(Node) ->
    Header   = <<131,103,100>>,
    String   = atom_to_list(Node),
    L        = length(String),
    binary_to_term(list_to_binary([Header, bytes16(L), String, bytes32(Id), bytes32(Serial),0])).

-doc #{equiv => port(node(), Id)}.
-doc(#{group => <<"Convenience functions">>,since => <<"OTP R13B04">>}).
-spec port(Id) -> port() when
      Id :: integer().

port(Id) -> port(node(), Id).

-doc "Creates a port id with creation 0.".
-doc(#{group => <<"Convenience functions">>,since => <<"OTP R13B04">>}).
-spec port(Node, Id) -> port() when
      Node :: node(),
      Id :: integer().

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

-doc false.
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

handle_call(Command, _From, State) ->
    {reply, {error, {undefined, Command}}, State}.

%% -------------------------------------------------------------------- %%
%%
%% handle_cast
%%
%% -------------------------------------------------------------------- %%

-doc false.
handle_cast(_, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------- %%
%%
%% handle_info
%%
%% -------------------------------------------------------------------- %%

-doc false.
handle_info(_Info, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------- %%
%%
%% termination
%%
%% -------------------------------------------------------------------- %%

-doc false.
terminate(_Reason, _State) ->
    ok.

%% -------------------------------------------------------------------- %%
%%
%% code_change
%%
%% -------------------------------------------------------------------- %%

-doc false.
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
    list_to_tuple([A + B || A <- tuple_to_list(Hs1) && B <- tuple_to_list(Hs2)]).

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
    <<L:LL, Node:L/binary, Ids:8/binary, _Creation/binary>> = Rest,
    Bin2 = list_to_binary([H, <<L:LL>>, Node, Ids, <<0:CL>>]),
    binary_to_term(Bin2);
clean_id_creation(Id) ->
    Id.

-define(PID_EXT, $g).
-define(NEW_PID_EXT, $X).
-define(V4_PORT_EXT, $x).
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
creation_bits(?V4_PORT_EXT) -> 32.

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
%%	Print :: [Type | {Type, non_neg_integer()}]
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
