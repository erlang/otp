%%
%%
%% Copyright WhatsApp Inc. and its affiliates. All rights reserved.
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
%%
%%-------------------------------------------------------------------
%%
%% @author Maxim Fedorov <maximfca@gmail.com>
%% Erlang Process Tracing profiler.
%%
-module(tprof).

%% API
-export([
    start/0,
    start/1,
    start_link/0,
    start_link/1,
    stop/0,
    set_pattern/3,
    clear_pattern/3,
    get_trace_map/0,
    enable_trace/1, enable_trace/2,
    disable_trace/1, disable_trace/2,
    pause/0,
    continue/0,
    restart/0,
    collect/0,
    %% ad-hoc profiling
    profile/1, profile/2, profile/3, profile/4,
    %% Analysis API
    inspect/1, inspect/3,
    format/1, format/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-compile([warn_missing_spec]).

%% typedefs for easier digestion

%% Configures the type of profiling.
-type start_options() :: #{type => trace_type()}.

%% Trace type
- type trace_type() :: call_count | call_time | call_memory.

%% Trace spec: module() or '_', function or '_', arity or '_'
-type trace_pattern() :: {module(), Fun :: atom(), arity() | '_'}.

%% Trace map: accumulated view of multiple trace patterns
-type trace_map() :: #{module() => [{Fun :: atom(), arity()}]}.

%% Single trace_info call with associated module/function/arity
-type trace_info() :: {module(), Fun :: atom(), Arity :: non_neg_integer(),
    [{pid(), Count :: pos_integer(), Measurement :: pos_integer()}]}.

%% Combined report for a single function (one or all processes).
-type profile_line() :: {module(), Function :: {atom(), arity()},
    Count :: pos_integer(), Measurement :: pos_integer(), MeasurementPerCall :: non_neg_integer(), Percent :: float()}.

%% Single profiling attempt result.
-type profile_result() :: {trace_type(), TotalMeasurement :: non_neg_integer(), [profile_line()]}.

%% Convenience type used to sort the profiling results.
-type column() :: module | function | calls | measurement | measurement_per_call | percent.

%% Sort by
-type sort_by() :: column() | {column(), ascending} | {column(), descending}.

%% Selected options allowed for enable/disable trace
-type trace_options() :: #{
    set_on_spawn => boolean()
}.

%% Convenience type to define which processes to trace
-type rootset() :: [process()] |   %% list of pids/registered names
    processes |
    existing_processes |
    new_processes.

-type profile_options() :: #{
    type => trace_type(),                           %% the type of profiling
    timeout => timeout(),                           %% stop profiling after the timeout
    pattern => trace_pattern() | [trace_pattern()], %% list of patterns to trace
    set_on_spawn => boolean(),                      %% trace spawned processes or not (true by default)
    rootset => rootset(),                           %% extra processes to trace
    report => return | process | total | {process, sort_by()} | {total, sort_by()},   %% print or return results
    device => io:device(),                          %% device to report to
    registered => false | {local, atom()}           %% register the profiler process (to detect concurrent attempts)
}.

%%--------------------------------------------------------------------
%% Server-aided API
-spec start() -> {'ok', Pid} | {'error', Reason} when Pid :: pid(), Reason :: {'already_started', Pid}.
start() ->
    start(#{}).

-spec start(start_options()) -> {'ok', Pid} | {'error', Reason}
        when Pid :: pid(), Reason :: {'already_started', Pid}.
start(Config) when is_map(Config) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Starts the process and links it to the caller.
-spec start_link() -> {'ok', Pid} | {'error', Reason} when Pid :: pid(), Reason :: {'already_started', Pid}.
start_link() ->
    start_link(#{}).

-spec start_link(start_options()) -> {'ok', Pid} | {'error', Reason}
        when Pid :: pid(), Reason :: {'already_started', Pid}.
start_link(Config) when is_map(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

-spec set_pattern(module(), atom(), arity() | '_') -> ok | {error, {trace_pattern, trace_pattern()}}.
set_pattern(Mod, Fun, Arity) ->
    gen_server:call(?MODULE, {set_pattern, Mod, Fun, Arity}, infinity).

%% @doc Stops tracing all or specific function patterns.
-spec clear_pattern(module(), atom(), arity() | '_') -> ok.
clear_pattern(Mod, Fun, Arity) ->
    gen_server:call(?MODULE, {clear_pattern, Mod, Fun, Arity}, infinity).

%% @doc Returns current trace map.
-spec get_trace_map() -> trace_map().
get_trace_map() ->
    gen_server:call(?MODULE, get_trace_map).

%% @doc Returns statistics for current trace map.
-spec collect() -> {trace_type(), [trace_info()]}.
collect() ->
    gen_server:call(?MODULE, collect, infinity).

%% Process identified by a PID or a registered name.
-type process() :: pid() | atom().

%% @doc Shortcut for erlang:trace/3 BIF touching only memory tracing flags.
%%      Returns number of successful operations, and list of those unsuccessful
%%      if the list was supplied. By default applies set_on_spawn flag.
-spec enable_trace(Spec) -> non_neg_integer()
    when Spec :: pid() |
        processes |
        new_processes |
        existing_processes |
        {children | all_children, process()};
    ([process()]) -> non_neg_integer() | {non_neg_integer(), [process()]}.
enable_trace(Rootset) ->
    enable_trace(Rootset, #{set_on_spawn => true}).

-spec enable_trace(Spec, trace_options()) -> non_neg_integer()
    when Spec :: pid() |
        processes |
        new_processes |
        existing_processes |
        {children | all_children, process()};
    ([process()], trace_options()) -> non_neg_integer() | {non_neg_integer(), [process()]}.
enable_trace(Procs, Options) when Procs =:= processes; Procs =:= new_processes; Procs =:= existing_processes ->
    erlang:trace(Procs, true, trace_options(Options));
enable_trace({Children, PidOrName}, Options) when Children =:= children; Children =:= all_children ->
    Pids = children(Children, PidOrName),
    toggle_trace(Pids, true, trace_options(Options), 0, []);
enable_trace(Pid, Options) when is_pid(Pid); is_atom(Pid) ->
    toggle_process_trace(Pid, true, trace_options(Options));
enable_trace(List, Options) when is_list(List) ->
    toggle_trace(List, true, trace_options(Options), 0, []).

-spec disable_trace(Spec) -> non_neg_integer()
    when Spec :: pid() |
        processes |
        new_processes |
        existing_processes |
        {children | all_children, process()};
    ([process()]) -> non_neg_integer() | {non_neg_integer(), [process()]}.
disable_trace(Rootset) ->
    disable_trace(Rootset, #{set_on_spawn => true}).

-spec disable_trace(Spec, trace_options()) -> non_neg_integer()
    when Spec :: pid() |
        processes |
        new_processes |
        existing_processes |
        {children | all_children, process()};
    ([process()], trace_options()) -> non_neg_integer() | {non_neg_integer(), [process()]}.
disable_trace(Procs, Options) when Procs =:= processes; Procs =:= new_processes; Procs =:= existing_processes ->
    erlang:trace(Procs, false, trace_options(Options));
disable_trace({Children, PidOrName}, Options) when Children =:= children; Children =:= all_children ->
    Pids = children(Children, PidOrName),
    toggle_trace(Pids, false, trace_options(Options), 0, []);
disable_trace(Pid, Options) when is_pid(Pid); is_atom(Pid) ->
    toggle_process_trace(Pid, false, trace_options(Options));
disable_trace(List, Options) when is_list(List) ->
    toggle_trace(List, false, trace_options(Options), 0, []).

%% @doc Pauses tracing for the entire trace_map
-spec pause() -> ok | not_running.
pause() ->
    gen_server:call(?MODULE, pause, infinity).

%% @doc Continues paused tracing.
-spec continue() -> ok | not_paused.
continue() ->
    gen_server:call(?MODULE, continue, infinity).

%% @doc Restarts tracing, clearing current statistics. Profiling could be
%%      running or paused.
-spec restart() -> ok.
restart() ->
    gen_server:call(?MODULE, restart, infinity).

%%--------------------------------------------------------------------
%% Common API

%% @doc Shortcut to transform raw collected data by process, sorted by percent.
-spec inspect({trace_type(), [trace_info()]}) -> #{pid() | all => profile_result()}.
inspect(Profile) ->
    inspect(Profile, process, percent).

%% @doc Transforms raw collected data into shape suitable for analysis and printing.
-spec inspect({trace_type(), [trace_info()]}, process | total, sort_by()) ->
    #{pid() | all => profile_result()}.
inspect({Type, Profile}, process, SortBy) ->
    maps:map(
        fun (_Pid, {Total, Stats}) ->
            {Type, Total, inspect_sort(Stats, SortBy)}
        end, inspect_processes(Profile, #{}));
inspect({Type, Profile}, total, SortBy) ->
    GrandTotal = lists:sum([Mem || {_M, _F, _A, Mems} <- Profile, {_P, _C, Mem} <- Mems]),
    TotalStats = [inspect_total(M, F, A, GrandTotal, Mems) || {M, F, A, Mems} <- Profile],
    #{all => {Type, GrandTotal, inspect_sort(TotalStats, SortBy)}}.

%% @doc Formats inspect()-ed totals and per-function data
-spec format(#{pid() | all => profile_result()}) -> ok.
format(Inspected) ->
    format_impl([], Inspected).

-spec format(io:device(), #{pid() | all => profile_result()}) -> ok.
format(IoDevice, Inspected) ->
    format_impl(IoDevice, Inspected).

%%--------------------------------------------------------------------
%% Ad-hoc API

%% @doc Runs the function/MFA with heap tracing enabled.
-spec profile(fun(() -> term())) -> ok | {term(), [trace_info()]}.
profile(Fun) when is_function(Fun) ->
    profile(Fun, #{}).

-spec profile(fun(() -> term()), profile_options()) -> ok | {term(), [trace_info()]}.
profile(Fun, Options) when is_function(Fun) ->
    do_profile(Fun, Options).

-spec profile(module(), Fun :: atom(), Args :: [term()]) -> ok | {term(), [trace_info()]}.
profile(Module, Function, Args) when is_atom(Module), is_atom(Function), is_list(Args) ->
    profile(Module, Function, Args, #{}).

-spec profile(module(), Fun :: atom(), Args :: [term()], profile_options()) -> ok | {term(), [trace_info()]}.
profile(Module, Function, Args, Options) when is_atom(Module), is_atom(Function), is_list(Args) ->
    do_profile({Module, Function, Args}, Options).

%%--------------------------------------------------------------------
%% gen_server implementation
-record(tprof_state, {
    type = call_count :: trace_type(),
    trace_map = #{} :: trace_map(),
    paused = false :: boolean(),
    ad_hoc = undefined :: undefined |
        {pid(), Timer :: reference() | false, Patterns :: [trace_pattern()],
            RootSet :: rootset(), ReplyTo :: gen_server:from()}
}).

-type state() :: #tprof_state{}.

-spec init(start_options()) -> {ok, state()}.
init(Config) ->
    Type = maps:get(type, Config, call_count),
    false = erlang:process_flag(trap_exit, true), %% need this for reliable terminate/2 call
    {ok, #tprof_state{type=Type}}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply | noreply, term(), state()}.
handle_call({set_pattern, M, F, A}, _From, #tprof_state{trace_map = Map, type = Type} = State) ->
    {Reply, NewMap} = enable_pattern(M, F, A, Map, Type),
    {reply, Reply, State#tprof_state{trace_map = NewMap}};
handle_call({clear_pattern, M, F, A}, _From, #tprof_state{trace_map = Map, type = Type} = State) ->
    {Ret, NewMap} = disable_pattern(M, F, A, Map, Type),
    {reply, Ret, State#tprof_state{trace_map = NewMap}};
handle_call(get_trace_map, _From, #tprof_state{trace_map = Map} = State) ->
    {reply, Map, State};
handle_call(pause, _From, #tprof_state{paused = true} = State) ->
    {reply, not_running, State};
handle_call(pause, _From, #tprof_state{trace_map = Map, paused = false, type = Type} = State) ->
    foreach(Map, pause, Type),
    {reply, ok, State#tprof_state{paused = true}};
handle_call(continue, _From, #tprof_state{paused = false} = State) ->
    {reply, running, State};
handle_call(continue, _From, #tprof_state{trace_map = Map, type = Type} = State) ->
    foreach(Map, true, Type),
    {reply, ok, State#tprof_state{paused = false}};
handle_call(restart, _From, #tprof_state{trace_map = Map, type = Type} = State) ->
    foreach(Map, restart, Type),
    {reply, ok, State#tprof_state{paused = false}};
handle_call(collect, _From, #tprof_state{trace_map = Map, type = Type} = State) ->
    {reply, collect(Map, Type), State};
handle_call({profile, What, Options}, From, #tprof_state{ad_hoc = undefined, trace_map = Map, type = Type} = State) ->
    %% ad-hoc profile routed via gen_server to handle 'EXIT' signal
    {Pid, Timer, Patterns, RootSet, NewMap} = ad_hoc_run(What, Options, Map, Type),
    {noreply, State#tprof_state{ad_hoc = {Pid, Timer, Patterns, RootSet, From}, trace_map = NewMap}};
handle_call({profile, _What, _Options}, _From, State) ->
    {reply, {error, running}, State}.

-spec handle_cast(term(), state()) -> no_return().
handle_cast(_Req, _State) ->
    erlang:error(notsup).

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'EXIT', Pid, Reason}, #tprof_state{ad_hoc = {Pid, Timer, Patterns, RootSet, From},
    trace_map = Map, type = Type} = State) ->
    _ = disable_trace(RootSet),
    Profile = collect(Map, Type),
    gen:reply(From, {Reason, Profile}),
    Timer =/= false andalso erlang:cancel_timer(Timer),
    {noreply, State#tprof_state{ad_hoc = undefined, trace_map = disable_patterns(Patterns, Map, Type)}};

handle_info({cancel, Pid}, #tprof_state{ad_hoc = {Pid, _Timer, Patterns, RootSet, From},
    trace_map = Map, type = Type} = State) ->
    _ = disable_trace(RootSet),
    Profile = collect(Map, Type),
    gen:reply(From, {{'EXIT', timeout}, Profile}),
    {noreply, State#tprof_state{ad_hoc = undefined, trace_map = disable_patterns(Patterns, Map, Type)}}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #tprof_state{trace_map = Map, type = Type}) ->
    clear_pattern(Map, Type),
    ok.

%%--------------------------------------------------------------------
%% Internal implementation

-include_lib("kernel/include/logger.hrl").

%% Add the trace of the specified module to the accumulator
collect_trace(Mod, FunList, Acc, Type) ->
    {Fail, Ret} = lists:foldl(
        fun ({Fun, Arity}, {Fail, Prev}) ->
            case combine_trace(erlang:trace_info({Mod, Fun, Arity}, Type)) of
                skip ->
                    {Fail, Prev};
                fail ->
                    {true, Prev};
                Tr ->
                    {Fail, [{Mod, Fun, Arity, Tr} | Prev]}
            end
        end, {false, Acc}, FunList),
    %% module may have been hot-code reloaded, or tracing was broken by something else
    Fail andalso begin
        ?LOG_WARNING(
            "tprof encountered an error tracing module ~s, was it reloaded or untraced?",
            [Mod])
        end,
    Ret.

%% It is possible that due to hot code reload event
%% some function is no longer traced, while it was supposed to.
%% Reinstating tracing automatically is wrong thing to do, because
%% statistics won't be correct anyway. Hence the warning in the user
%% guide, guarding against hot code reload while tracing.
combine_trace({_, false}) -> fail;
combine_trace({call_count, 0}) -> skip;
combine_trace({call_count, Num}) -> [{all, Num, Num}];
combine_trace({call_time, Times}) ->
    case [{Pid, Calls, S * 1000000 + Us} || {Pid, Calls, S, Us} <- Times] of
        [] ->
            skip;
        NonZero ->
            NonZero
    end;
combine_trace({call_memory, Mem}) ->
    case [{Pid, Calls, Words} || {Pid, Calls, Words} <- Mem, Words > 0] of
        [] ->
            skip;
        NonZero ->
            NonZero
    end.

%% Inspection: iterate over collected traces, return map of
%%  #{Pid => [{M, {F, A}, Calls, Measurement, PerCall, Percentage}], unsorted.
inspect_processes([], Acc) ->
    maps:map(
        fun (_Pid, {Total, Lines}) ->
            {Total, [{M, {F, A}, Calls, Measurement, PerCall, divide(Measurement * 100, Total)}
                || {M, F, A, Calls, Measurement, PerCall} <- Lines]}
        end, Acc);
inspect_processes([{_M, _F, _A, []} | Tail], Acc) ->
    inspect_processes(Tail, Acc);
inspect_processes([{M, F, A, [{Pid, Calls, Mem} | TripletTail]} | Tail], Acc) ->
    ProfLine = {M, F, A, Calls, Mem, divide(Mem, Calls)},
    inspect_processes([{M, F, A, TripletTail} | Tail],
        maps:update_with(Pid, fun ({Grand, L}) -> {Grand + Mem, [ProfLine | L]} end, {Mem, [ProfLine]}, Acc)).

%% Inspection: remove Pid information from the Profile, return list of
%%  [{M, F, A, TotalCalls, Measurement, PerCall, Percentage}]
inspect_total(M, F, A, GrandTotal, Mems) ->
    {TC, TM} = lists:foldl(
        fun ({_Pid, Calls, Mem}, {TotalCalls, TotalMem}) ->
            {TotalCalls + Calls, TotalMem + Mem}
        end, {0, 0}, Mems),
    {M, {F, A}, TC, TM, divide(TM, TC), divide(TM * 100, GrandTotal)}.

divide(_,0) -> 0.0;
divide(T,N) -> T/N.

%% Returns "sort by" column index
column(module) -> {1, ascending};
column(function) -> {2, ascending};
column(calls) -> {3, ascending};
column(measurement) -> {4, ascending};
column(measurement_per_call) -> {5, ascending};
column(percent) -> {6, ascending}.

%% Sorts by column name, ascending/descending
inspect_sort(Profile, undefined) ->
    Profile;
inspect_sort(Profile, {Column, ascending}) when is_integer(Column) ->
    lists:keysort(Column, Profile);
inspect_sort(Profile, {Column, descending}) when is_integer(Column) ->
    lists:reverse(lists:keysort(Column, Profile));
inspect_sort(Profile, {Column, Direction}) when is_atom(Column) ->
    {Col, _Skip} = column(Column),
    inspect_sort(Profile, {Col, Direction});
inspect_sort(Profile, Column) when is_atom(Column) ->
    inspect_sort(Profile, column(Column)).

%% Formats the inspected profile to the Device, which could be [] meaning
%%  default output.
format_impl(Device, Empty) when Empty =:= #{} ->
    format_out(Device, "Trace is empty~n", []);
format_impl(Device, #{all := {Type, Total, Inspected}}) ->
    format_each(Device, Type, Total, Inspected);
format_impl(Device, Inspected) when is_map(Inspected) ->
    %% grab the total-total words
    GrandTotal = maps:fold(fun (_Pid, {_Type, Total, _Profile}, Acc) -> Acc + Total end, 0, Inspected),
    %% per-process printout
    maps:foreach(
        fun(Pid, {Type, Total, Each}) ->
            format_out(Device, "~n****** Process ~w  --  ~.2f% of total *** ~n",
                [Pid, divide(100 * Total, GrandTotal)]),
            format_each(Device, Type, Total, Each)
        end, Inspected).

format_each(Device, call_count, _Total, Inspected) ->
    %% viewport size
    %% Viewport = case io:columns() of {ok, C} -> C; _ -> 80 end,
    %% layout: module and fun/arity columns are resizable, the rest are not
    %% convert all lines to strings
    {Widths, Lines} = lists:foldl(
        fun ({Mod, {F, A}, Calls, _Value, _VPC, Percent}, {Widths, Ln}) ->
            Line = [lists:flatten(io_lib:format("~tw:~tw/~w", [Mod, F, A])),
                integer_to_list(Calls), float_to_list(Percent, [{decimals, 2}])],
            NewWidths = [erlang:max(Old, New) || {Old, New} <- lists:zip([string:length(L) || L <- Line], Widths)],
            {NewWidths, [Line | Ln]}
        end, {[0, 5, 5], []}, Inspected),
    %% figure our max column widths according to viewport (cut off module/funArity)
    FilteredWidths = Widths,
    %% figure out formatting line
    Fmt = lists:flatten(io_lib:format("~~.~wts  ~~~ws  [~~~ws]~~n", FilteredWidths)),
    %% print using this format
    format_out(Device, Fmt, ["FUNCTION", "CALLS", "%"]),
    [format_out(Device, Fmt, Line) || Line <- lists:reverse(Lines)],
    format_out(Device, Fmt, [" ", " ", "100.0"]);
format_each(Device, call_time, Total, Inspected) ->
    format_labelled(Device, "TIME (μs)", Total, Inspected);
format_each(Device, call_memory, Total, Inspected) ->
    format_labelled(Device, "WORDS", Total, Inspected).

format_labelled(Device, Label, Total, Inspected) ->
    %% viewport size
    %% Viewport = case io:columns() of {ok, C} -> C; _ -> 80 end,
    %% layout: module and fun/arity columns are resizable, the rest are not
    %% convert all lines to strings
    {Widths, Lines} = lists:foldl(
        fun ({Mod, {F, A}, Calls, Value, VPC, Percent}, {Widths, Ln}) ->
            Line = [lists:flatten(io_lib:format("~tw:~tw/~w", [Mod, F, A])),
                integer_to_list(Calls), integer_to_list(Value),
                float_to_list(VPC, [{decimals, 2}]),
                float_to_list(Percent, [{decimals, 2}])],
            NewWidths = [erlang:max(Old, New) || {Old, New} <- lists:zip([string:length(L) || L <- Line], Widths)],
            {NewWidths, [Line | Ln]}
        end, {[0, 5, length(Label), 8, 5], []}, Inspected),
    %% figure our max column widths according to viewport (cut off module/funArity)
    FilteredWidths = Widths,
    %% figure out formatting line
    Fmt = lists:flatten(io_lib:format("~~.~wts  ~~~ws  ~~~wts  ~~~ws  [~~~ws]~~n", FilteredWidths)),
    %% print using this format
    format_out(Device, Fmt, ["FUNCTION", "CALLS", Label, "PER CALL", "%"]),
    [format_out(Device, Fmt, Line) || Line <- lists:reverse(Lines)],
    format_out(Device, Fmt, [" ", " ", integer_to_list(Total), " ", "100.0"]).

%% format implementation that uses [] as a way to tell "default output"
format_out([], Fmt, Args) ->
    io:format(Fmt, Args);
format_out(Device, Fmt, Args) ->
    io:format(Device, Fmt, Args).

%% pattern collapse code
enable_pattern('_', '_', '_', _Acc, Type) ->
    %% need to re-trace everything, probably some new modules were loaded
    %% discard any existing trace pattern
    lists:foldl(
        fun({Mod, _}, {Total, Acc}) ->
            Plus = erlang:trace_pattern({Mod, '_', '_'}, true, [Type]),
            {Total + Plus, Acc#{Mod => Mod:module_info(functions)}}
        end, {0, #{}}, code:all_loaded());
enable_pattern(Mod, '_', '_', Acc, Type) ->
    %% code may have been hot-loaded, redo the trace
    case erlang:trace_pattern({Mod, '_', '_'}, true, [Type]) of
        0 ->
            {{error, {trace_pattern, Mod, '_', '_'}}, Acc};
        Traced ->
            {Traced, Acc#{Mod => Mod:module_info(functions)}}
    end;
enable_pattern(Mod, Fun, '_', Acc, Type) ->
    case erlang:trace_pattern({Mod, Fun, '_'}, true, [Type]) of
        0 ->
            {{error, {trace_pattern, Mod, Fun, '_'}}, Acc};
        Traced ->
            Added = [{F, A} || {F, A} <- Mod:module_info(functions), F =:= Fun],
            NewMap = maps:update_with(Mod,
                fun (FAs) ->
                    Added ++ [{F, A} || {F, A} <- FAs, F =/= Fun]
                end, Added, Acc),
            {Traced, NewMap}
    end;
enable_pattern(Mod, Fun, Arity, Acc, Type) ->
    case erlang:trace_pattern({Mod, Fun, Arity}, true, [Type]) of
        0 ->
            {{error, {trace_pattern, Mod, Fun, Arity}}, Acc};
        1 ->
            {1, maps:update_with(Mod,
                fun (FAs) -> [{Fun, Arity} | FAs -- [{Fun, Arity}]] end, [{Fun, Arity}], Acc)}
    end.

%% pattern collapse code for un-tracing
disable_pattern('_', '_', '_', _Acc, Type) ->
    Traced = erlang:trace_pattern({'_', '_', '_'}, false, [Type]),
    {Traced, #{}};
disable_pattern(Mod, '_', '_', Acc, Type) when is_map_key(Mod, Acc) ->
    Traced = erlang:trace_pattern({Mod, '_', '_'}, false, [Type]),
    {Traced, maps:remove(Mod, Acc)};
disable_pattern(Mod, Fun, '_', Acc, Type) when is_map_key(Mod, Acc) ->
    Traced = erlang:trace_pattern({Mod, Fun, '_'}, false, [Type]),
    {Traced, maps:update_with(Mod,
        fun (FAs) -> [{F, A} || {F, A} <- FAs, F =/= Fun] end, Acc)};
disable_pattern(Mod, Fun, Arity, Acc, Type) when is_map_key(Mod, Acc) ->
    Traced = erlang:trace_pattern({Mod, Fun, Arity}, false, [Type]),
    {Traced, maps:update_with(Mod, fun (FAs) -> FAs -- [{Fun, Arity}] end, Acc)};
disable_pattern(Mod, Fun, Arity, Acc, _Type) ->
    {{error, {not_traced, Mod, Fun, Arity}}, Acc}.

disable_patterns(Patterns, Map, Type) ->
    lists:foldl(
        fun ({M, F, A}, Acc) -> {_, New} = disable_pattern(M, F, A, Acc, Type), New end,
        Map,
        Patterns
    ).

%% ad-hoc profiler implementation
do_profile(What, Options) ->
    %% start a new tprof server, potentially registered to a new name
    Pid = start_result(start_internal(Options)),
    try
        {Ret, Profile} = gen_server:call(Pid, {profile, What, Options}, infinity),
        return_profile(maps:get(report, Options, process), Profile, Ret,
            maps:get(device, Options, []))
    after
        gen_server:stop(Pid)
    end.

start_internal(Options) ->
    case maps:get(registered, Options, {local, ?MODULE}) of
        false -> gen_server:start_link(?MODULE, Options, []);
        {local, Name} -> gen_server:start_link({local, Name}, ?MODULE, Options, [])
    end.

start_result({ok, Pid}) -> Pid;
start_result({error, Reason}) -> erlang:error(Reason).

return_profile(return, Profile, Ret, _Device) ->
    {Ret, Profile};
return_profile(process, Profile, Ret, Device) ->
    return_profile({process, percent}, Profile, Ret, Device);
return_profile(total, Profile, Ret, Device) ->
    return_profile({total, percent}, Profile, Ret, Device);
return_profile({Agg, Sort}, Profile, _Ret, Device) ->
    format_impl(Device, inspect(Profile, Agg, Sort)).

%% @doc clears tracing for the entire trace map passed
-spec clear_pattern(trace_map(), trace_type()) -> ok.
clear_pattern(Existing, Type) ->
    maps:foreach(
        fun (Mod, FunArity) ->
            [erlang:trace_pattern({Mod, F, A}, false, [Type]) || {F, A} <- FunArity]
        end, Existing).

trace_options(#{set_on_spawn := false}) ->
    [call, silent];
trace_options(_) ->
    [call, silent, set_on_spawn].

children(Children, PidOrName) when is_atom(PidOrName) ->
    case erlang:whereis(PidOrName) of
        undefined -> [];
        Pid -> children(Children, Pid)
    end;
children(children, Pid) when is_pid(Pid) ->
    [P || P <- erlang:processes(), erlang:process_info(P, parent) =:= {parent, Pid}];
children(all_children, Pid) when is_pid(Pid) ->
    %% build a process tree (could use a digraph too)
    Tree = maps:groups_from_list(
        fun (P) ->
            case erlang:process_info(P, parent) of
                undefined -> undefined;
                {parent, Parent} -> Parent
            end
        end, erlang:processes()),
    select_pids(Tree, Pid).

select_pids(Tree, Pid) ->
    case maps:find(Pid, Tree) of
        error -> [];
        {ok, Children} ->
            Children ++ lists:concat([select_pids(Tree, C) || C <- Children])
    end.

toggle_process_trace(Pid, On, Flags) when is_pid(Pid) ->
    try
        1 = erlang:trace(Pid, On, Flags)
    catch _:_ ->
        0
    end;
toggle_process_trace(Name, On, Flags) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            0;
        Pid ->
            toggle_process_trace(Pid, On, Flags)
    end.

toggle_trace([], _On, _Flags, Success, []) ->
    Success;
toggle_trace([], _On, _Flags, Success, Failure) ->
    {Success, lists:reverse(Failure)};
toggle_trace([Pid | Tail], On, Flags, Success, Failure) when is_pid(Pid) ->
    {NS, NF} =
        try
            1 = erlang:trace(Pid, On, Flags),
            {Success + 1, Failure}
        catch _:_ ->
            {Success, [Pid | Failure]}
        end,
    toggle_trace(Tail, On, Flags, NS, NF);
toggle_trace([Name | Tail], On, Flags, Success, Failure) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            toggle_trace(Tail, On, Flags, Success, [Name | Failure]);
        Pid ->
            {NS, NF} =
                try
                    1 = erlang:trace(Pid, On, Flags),
                    {Success + 1, Failure}
                catch _:_ ->
                    {Success, [Name | Failure]}
                end,
            toggle_trace(Tail, On, Flags, NS, NF)
    end.

%% @doc Collects memory tracing data (usable for inspect()) for
%%      all traced functions.
-spec collect(trace_map(), trace_type()) -> {trace_type(), [trace_info()]}.
collect(Pattern, Type) ->
    {Type, maps:fold(fun(K, V, Acc) -> collect_trace(K, V, Acc, Type) end, [], Pattern)}.

foreach(Map, Action, Type) ->
    maps:foreach(
        fun (Mod, Funs) ->
            [erlang:trace_pattern({Mod, F, A}, Action, [Type]) || {F, A} <- Funs]
        end, Map).

ad_hoc_run(What, Options, Map, Type) ->
    %% add missing patterns
    Patterns = make_list(maps:get(pattern, Options, {'_', '_', '_'})),
    NewMap = lists:foldl(
        fun({M, F, A}, Acc) ->
            {_, NewMap} = enable_pattern(M, F, A, Acc, Type),
            NewMap
        end, Map, Patterns),
    %% check whether spawned processes are also traced
    OnSpawn = maps:get(set_on_spawn, Options, true),
    %% enable tracing for items in the rootset
    RootSet = maps:get(rootset, Options, []),
    _ = enable_trace(RootSet), %% ignore errors when setting up rootset trace
    %% spawn a separate process to run the user-supplied MFA
    %% if RootSet is 'processes' or 'new_processes', skip the trace flags
    Flags = trace_flags(RootSet, OnSpawn),
    Pid = spawn_profiled(What, Flags),
    %% start timer to terminate the function being profiled if it takes too long
    %%  to complete
    Timer = is_map_key(timeout, Options) andalso
        erlang:send_after(maps:get(timeout, Options), self(), {cancel, Pid}),
    {Pid, Timer, Patterns, RootSet, NewMap}.

trace_flags(processes, _) -> [];
trace_flags(new_processes, _) -> [];
trace_flags(_, true) -> [call, silent, set_on_spawn];
trace_flags(_, false) -> [call, silent].

make_list({M, F, A}) -> [{M, F, A}];
make_list(List) -> List.

spawn_profiled(Fun, Flags) when is_function(Fun) ->
    spawn_link(
        fun() ->
            Flags =/= [] andalso begin 1 = erlang:trace(self(), true, Flags) end,
            Ret = catch Fun(),
            Flags =/= [] andalso begin 1 = erlang:trace(self(), false, Flags) end,
            exit(Ret)
        end);
spawn_profiled({M, F, A}, Flags) ->
    spawn_link(
        fun() ->
            Flags =/= [] andalso begin 1 = erlang:trace(self(), true, Flags) end,
            Ret = catch erlang:apply(M, F, A),
            Flags =/= [] andalso begin 1 = erlang:trace(self(), false, Flags) end,
            exit(Ret)
        end).
