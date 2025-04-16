%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
%% Purpose: Profile a system in order to figure out where the 
%% time goes.
%%

-module(eprof).
-moduledoc """
A Time Profiling Tool for Erlang

The module `eprof` provides a set of functions for time profiling of Erlang
programs to find out how the execution time is used. The profiling is done using
the Erlang trace BIFs. Tracing of local function calls for a specified set of
processes is enabled when profiling is begun, and disabled when profiling is
stopped.

When using Eprof, expect a slowdown in program execution.
""".
-behaviour(gen_server).

-export([start/0,
	 stop/0,
	 dump/0, dump_data/0,
	 start_profiling/1, start_profiling/2, start_profiling/3,
	 profile/1, profile/2, profile/3, profile/4, profile/5, profile/6,
	 stop_profiling/0,
	 analyze/0, analyze/1, analyze/2, analyze/4,
	 log/1]).

%% Internal exports 
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).


-record(bpd, {
	n   = 0,                 % number of total calls
	us  = 0,                 % sum of uS for all calls
	p   = gb_trees:empty(),  % tree of {Pid, {Mfa, {Count, Us}}}
	mfa = []                 % list of {Mfa, {Count, Us}}
    }).

-define(default_options, [{set_on_spawn, true}]).
-define(default_pattern, {'_','_','_'}).

-record(state, {
	profiling  = false,
	pattern    = ?default_pattern,
	rootset    = [],
	trace_opts = [],
	fd         = undefined,
	start_ts   = undefined,
	reply      = undefined,
	bpd        = #bpd{}
    }).



%% -------------------------------------------------------------------- %%
%%
%% API
%%
%% -------------------------------------------------------------------- %%

-doc "Starts the Eprof server which holds the internal state of the collected data.".
-spec start() -> {'ok', Pid} | {'error', Reason} when
      Pid :: pid(),
      Reason :: {'already_started', Pid}.

start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).

-doc "Stops the Eprof server.".
-spec stop() -> 'stopped'.

stop()  -> gen_server:call(?MODULE, stop, infinity).

-doc #{equiv => analyze(procs, [])}.

-doc(#{since => <<"OTP R14B">>}).
-spec analyze() -> 'ok' | 'nothing_to_analyze'.

analyze() ->
    analyze(procs).

-type analyze_type() :: 'procs' | 'total'.

-doc """
If `TypeOpts` is an atom, it is assumed to be a module name, and this
call is equivalent to [`analyze(TypeOpts, [])`](`analyze/2`).

Otherwise, if `TypeOpts` is a list, it assumed to be a list of options, and this
call is equivalent to [`analyze(procs, TypeOpts)`](`analyze/2`).
""".
-doc(#{since => <<"OTP R14B">>}).
-spec analyze(TypeOpts) -> 'ok' | 'nothing_to_analyze' when
      TypeOpts :: analyze_type().

analyze(Type) when is_atom(Type) ->
    analyze(Type, []);
analyze(Opts) when is_list(Opts) ->
    analyze(procs, Opts).

-doc """
Call this function when profiling has been stopped to display the results.

If `Type` is `procs`, the time spent in each function is shown separately
for each profiled process.

If `Type` is `total`, the time spent in each function is shown combined
for each profiled process.

Time is shown as percentage of total time and as absolute time in micro seconds.
""".
-doc(#{since => <<"OTP R14B">>}).
-spec analyze(Type, Options) -> 'ok' | 'nothing_to_analyze' when
      Type :: analyze_type(),
      Options :: [Option],
      Option :: {'filter', Filter}
              | {'sort', Sort},
      Filter :: [{'calls', non_neg_integer()} | {'time', float()}],
      Sort :: 'time' | 'calls' | 'mfa'.

analyze(Type, Opts) when is_list(Opts) ->
    gen_server:call(?MODULE, {analyze, Type, Opts}, infinity).

-doc """
profile(FunRootset)

If `FunRootset` is a fun, this call is equivalent to
[`profile([], FunRootset)`](`profile/2`).

If `FunRootset` is a list, it is assumed to be a `Rootset`, and this
call is equivalent to [`start_profiling(Rootset)`](`start_profiling/1`).
""".
-spec profile(Fun) -> {'ok', Value} | {'error', Reason} when
                  Fun :: fun(() -> term()),
                  Value :: term(),
                  Reason :: term();
             (Rootset) -> 'profiling' | {'error', Reason} when
                  Rootset :: [atom() | pid()],
                  Reason :: term().

%% odd duck, should only been start_profiling/1
profile(Rootset) when is_list(Rootset) ->
    start_profiling(Rootset);
profile(Fun) when is_function(Fun) ->
    profile([], Fun).

-doc """
profile(Arg1, Arg2)

If `Arg1` is a fun and `Arg2` is list, this call is equivalent to
[`profile([], Arg1, {'_','_','_'}, Arg2)`](`profile/4`).

If `Arg1` is a list and `Arg2` is a fun, this call is equivalent to
[`profile(Arg1, Arg2, {'_','_','_'}, Arg1)`](`profile/4`).

""".
-spec profile(Fun, Options) -> {'ok', Value} | {'error', Reason} when
                  Fun :: fun(() -> term()),
                  Options :: ['set_on_spawn' | {'set_on_spawn', boolean()}],
                  Value :: term(),
                  Reason :: term();
             (Rootset, Fun) -> {'ok', Value} | {'error', Reason} when
                  Rootset :: [atom() | pid()],
                  Fun :: fun(() -> term()),
                  Value :: term(),
                  Reason :: term().

profile(Fun, Opts) when is_function(Fun), is_list(Opts) ->
    profile([], erlang, apply, [Fun, []], ?default_pattern, Opts);
profile(Rootset, Fun) when is_list(Rootset), is_function(Fun) ->
    profile(Rootset, Fun, ?default_pattern).

%% Subtype of erlang:trace_pattern_mfa().
-type trace_pattern_mfa() :: {atom(), atom(), arity() | '_'}.

-doc #{equiv => profile(Rootset, Fun, Pattern, [{set_on_spawn, true}])}.
-doc #{since => <<"OTP R14B">>}.
-spec profile(Rootset, Fun, Pattern) -> {'ok', Value} | {'error', Reason} when
                  Rootset :: [atom() | pid()],
                  Fun :: fun(() -> term()),
                  Pattern :: trace_pattern_mfa(),
                  Value :: term(),
                  Reason :: term().

profile(Rootset, Fun, Pattern) when is_list(Rootset), is_function(Fun) ->
    profile(Rootset, Fun, Pattern, ?default_options).

-doc """
profile(Rootset, Arg1, Arg2, Arg3)

This function spawns a process that applies a fun or an an function,
and then starts profiling for the spawned proceses as well as the
processes in `Rootset` (and any new processes spawned from them).

If `Arg1` is a fun, `Arg2` is expected to be a trace pattern, and
`Arg3` a list of options. In that case, this call is equivalent to:

[`profile(Rootset, erlang, apply, [Arg1, []], Arg2, Arg3)`](`profile/6`)

If `Arg1` is an atom, `Arg1` is assumed to be a module name, `Arg2` the
name of the function in that module, and `Arg3` a list of arguments to
be used when calling that function. In that case, this call is equivalent
to:

[`profile(Rootset, Arg1, Arg2, Arg3, {'_','_','_'}, [{set_on_spawn, true}])`](`profile/6`)
""".
-spec profile(Rootset, Module, Function, Args) ->
                     {'ok', Value} | {'error', Reason} when
                  Rootset :: [atom() | pid()],
                  Module :: module(),
                  Function :: atom(),
                  Args :: [term()],
                  Value :: term(),
                  Reason :: term();
             (Rootset, Fun, Pattern, Options) ->
                     {'ok', Value} | {'error', Reason} when
                  Rootset :: [atom() | pid()],
                  Fun :: fun(() -> term()),
                  Pattern :: trace_pattern_mfa(),
                  Options :: ['set_on_spawn' | {'set_on_spawn', boolean()}],
                  Value :: term(),
                  Reason :: term().

profile(Rootset, Fun, Pattern, Options)
  when is_list(Rootset), is_function(Fun), is_list(Options) ->
    profile(Rootset, erlang, apply, [Fun,[]], Pattern, Options);
profile(Rootset, M, F, A)
  when is_list(Rootset), is_atom(M), is_atom(F), is_list(A) ->
    profile(Rootset, M, F, A, ?default_pattern).

-doc #{equiv => profile(Rootset, Module, Function, Args, Pattern,
                        [{set_on_spawn, true}])}.
-doc #{since => <<"OTP R14B">>}.
-spec profile(Rootset, Module, Function, Args, Pattern) ->
                     {'ok', Value} | {'error', Reason} when
                  Rootset :: [atom() | pid()],
                  Module :: module(),
                  Function :: atom(),
                  Args :: [term()],
                  Pattern :: trace_pattern_mfa(),
                  Value :: term(),
                  Reason :: term().

profile(Rootset, M, F, A, Pattern)
  when is_list(Rootset), is_atom(M), is_atom(F), is_list(A) ->
    profile(Rootset, M, F, A, Pattern, ?default_options).

-doc """
This function spawns a process `P` that [`apply(Module, Function,
Args)`](`apply/3`), and then starts profiling for `P` and the
processes in `Rootset` (and any new processes spawned from them).

`Rootset` is a list of pids and registered names.

Information about activity in any profiled process is stored in the Eprof
database.

If tracing could be enabled for `P` and all processes in `Rootset`, the function
returns `{ok,Value}` when `Fun()`/`apply` returns with the value `Value`, or
`{error,Reason}` if `Fun()`/`apply` fails with exit reason `Reason`. Otherwise
it returns `{error, Reason}` immediately.

The `set_on_spawn` option will active call time tracing for all processes
spawned by processes in the rootset. This is the default behaviour.

The programmer must ensure that the function given as argument is truly
synchronous and that no work continues after the function has returned a value.
""".
-doc(#{since => <<"OTP R16B01">>}).
-spec profile(Rootset, Module, Function, Args, Pattern, Options) ->
                     {'ok', Value} | {'error', Reason} when
                  Rootset :: [atom() | pid()],
                  Module :: module(),
                  Function :: atom(),
                  Args :: [term()],
                  Pattern :: trace_pattern_mfa(),
                  Options :: ['set_on_spawn' | {'set_on_spawn', boolean()}],
                  Value :: term(),
                  Reason :: term().

%% Returns when M:F/A has terminated
profile(Rootset, M, F, A, Pattern, Options) ->
    ok = start_internal(),
    gen_server:call(?MODULE, {profile_start, Rootset, Pattern, {M,F,A}, Options}, infinity).

-doc false.
dump() -> 
    gen_server:call(?MODULE, dump, infinity).

-doc false.
dump_data() ->
    gen_server:call(?MODULE, dump_data, infinity).

-doc """
Call this function to ensure that the results displayed by
[`analyze/0,1,2`](`analyze/0`) are printed to the file `File` as well as to the
screen.
""".
-spec log(File) -> 'ok' when
      File :: atom() | file:filename().

log(File) ->
    gen_server:call(?MODULE, {logfile, File}, infinity).

-doc #{equiv => start_profiling(Rootset, {'_','_','_'})}.
-spec start_profiling(Rootset) -> 'profiling' | {'error', Reason} when
      Rootset :: [atom() | pid()],
      Reason :: term().

%% Does not block
start_profiling(Rootset) ->
    start_profiling(Rootset, ?default_pattern).

-doc #{equiv => start_profiling(Rootset, Pattern, {'_','_','_'})}.
-doc(#{since => <<"OTP R14B">>}).
-spec start_profiling(Rootset, Pattern) -> 'profiling' | {'error', Reason} when
      Rootset :: [atom() | pid()],
      Pattern :: trace_pattern_mfa(),
      Reason :: term().

start_profiling(Rootset, Pattern) ->
    start_profiling(Rootset, Pattern, ?default_options).

-doc """
Starts profiling for the processes in `Rootset` (and any new processes spawned
from them).

Information about activity in any profiled process is stored in the
Eprof database.

`Rootset` is a list of pids and registered names.

The function returns `profiling` if tracing could be enabled for all processes
in `Rootset`, or `error` otherwise.

A pattern can be selected to narrow the profiling. For instance a specific
module can be selected, and only the code executed in that module will be
profiled.

The `set_on_spawn` option will active call time tracing for all processes
spawned by processes in the rootset. This is the default behaviour.
""".
-doc(#{since => <<"OTP R16B01">>}).
-spec start_profiling(Rootset, Pattern, Options) ->
                             'profiling' | {'error', Reason} when
      Rootset :: [atom() | pid()],
      Pattern :: trace_pattern_mfa(),
      Options :: ['set_on_spawn' | {'set_on_spawn', boolean()}],
      Reason :: term().

start_profiling(Rootset, Pattern, Options) ->
    ok = start_internal(),
    gen_server:call(?MODULE, {profile_start, Rootset, Pattern, undefined, Options}, infinity).

-doc "Stops profiling started with `start_profiling/1` or `profile/1`.".
-spec stop_profiling() -> 'profiling_stopped' | 'profiling_already_stopped'.

stop_profiling() ->
    gen_server:call(?MODULE, profile_stop, infinity).


%% -------------------------------------------------------------------- %%
%%
%% init
%%
%% -------------------------------------------------------------------- %%

-doc false.
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%% -------------------------------------------------------------------- %%
%%
%% handle_call
%%
%% -------------------------------------------------------------------- %%

%% analyze

-doc false.
handle_call(
  {analyze, _, _}, _,
  #state{ bpd = #bpd{ p = {0,nil}, us = 0, n = 0 } } = S) ->
    {reply, nothing_to_analyze, S};

handle_call({analyze, procs, Opts}, _, #state{ bpd = Bpd, fd = Fd } = S)
  when is_record(Bpd, bpd) ->
    {reply, analyze(Fd, procs, Opts, Bpd), S};

handle_call({analyze, total, Opts}, _, #state{ bpd = Bpd, fd = Fd } = S)
  when is_record(Bpd, bpd) ->
    {reply, analyze(Fd, total, Opts, Bpd), S};

handle_call({analyze, Type, _Opts}, _, S) ->
    {reply, {error, {undefined, Type}}, S};

%% profile

handle_call({profile_start, _Rootset, _Pattern, _MFA, _Opts}, _From, #state{ profiling = true } = S) ->
    {reply, {error, already_profiling}, S};

handle_call({profile_start, Rootset, Pattern, {M,F,A}, Opts}, From, #state{fd = Fd } = S) ->

    ok = set_pattern_trace(false, S#state.pattern),
    _  = set_process_trace(false, S#state.rootset, S#state.trace_opts),

    Topts = get_trace_options(Opts),
    Pid   = setup_profiling(M,F,A),

    case set_process_trace(true, [Pid|Rootset], Topts) of
	true ->
	    ok = set_pattern_trace(true, Pattern),
	    T0 = erlang:timestamp(),
	    ok = execute_profiling(Pid),
	    {noreply, #state{
		    profiling  = true,
		    rootset    = [Pid|Rootset],
		    start_ts   = T0,
		    reply      = From,
		    fd         = Fd,
		    trace_opts = Topts,
		    pattern    = Pattern
		}};
	{false, FailedPid} ->
	    exit(Pid, eprof_kill),
	    {reply, {error, {set_process_trace_failed, FailedPid}}, #state{ fd = Fd}}
    end;

handle_call({profile_start, Rootset, Pattern, undefined, Opts}, From, #state{ fd = Fd } = S) ->

    ok    = set_pattern_trace(false, S#state.pattern),
    true  = set_process_trace(false, S#state.rootset, S#state.trace_opts),
    Topts = get_trace_options(Opts),

    case set_process_trace(true, Rootset, Topts) of
	true ->
	    T0 = erlang:timestamp(),
	    ok = set_pattern_trace(true, Pattern),
	    {reply, profiling, #state{
		    profiling  = true,
		    rootset    = Rootset,
		    start_ts   = T0,
		    reply      = From,
		    fd         = Fd,
		    trace_opts = Topts,
		    pattern    = Pattern
		}};
	{false, FailedPid} ->
	    {reply, {error, {set_process_trace_failed, FailedPid}}, #state{ fd = Fd }}
    end;

handle_call(profile_stop, _From, #state{ profiling = false } = S) ->
    {reply, profiling_already_stopped, S};

handle_call(profile_stop, _From, #state{ profiling = true } = S) ->

    ok  = set_pattern_trace(pause, S#state.pattern),
    Bpd = collect_bpd(),
    _   = set_process_trace(false, S#state.rootset, S#state.trace_opts),
    ok  = set_pattern_trace(false, S#state.pattern),

    {reply, profiling_stopped, S#state{
	profiling  = false,
	rootset    = [],
	trace_opts = [],
	pattern    = ?default_pattern,
	bpd        = Bpd
    }};

%% logfile
handle_call({logfile, File}, _From, #state{ fd = OldFd } = S) ->
    case file:open(File, [write, {encoding, utf8}]) of
	{ok, Fd} ->
	    case OldFd of
		undefined -> ok;
		OldFd -> ok = file:close(OldFd)
	    end,
	    {reply, ok, S#state{fd = Fd}};
	Error ->
	    {reply, Error, S}
    end;

handle_call(dump, _From, #state{ bpd = Bpd } = S) when is_record(Bpd, bpd) ->
    {reply, gb_trees:to_list(Bpd#bpd.p), S};

handle_call(dump_data, _, #state{ bpd = #bpd{} = Bpd } = S)
  when is_record(Bpd, bpd) ->
    {reply, Bpd, S};

handle_call(stop, _FromTag, S) ->
    {stop, normal, stopped, S}.

%% -------------------------------------------------------------------- %%
%%
%% handle_cast
%%
%% -------------------------------------------------------------------- %%

-doc false.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% -------------------------------------------------------------------- %%
%%
%% handle_info
%%
%% -------------------------------------------------------------------- %%

-doc false.
handle_info({'EXIT', _, normal}, S) ->
    {noreply, S};
handle_info({'EXIT', _, eprof_kill}, S) ->
    {noreply, S};
handle_info({'EXIT', _, Reason}, #state{ reply = FromTag } = S) ->

    _  = set_process_trace(false, S#state.rootset, S#state.trace_opts),
    ok = set_pattern_trace(false, S#state.pattern),

    gen_server:reply(FromTag, {error, Reason}),
    {noreply, S#state{
	profiling  = false,
	rootset    = [],
	trace_opts = [],
	pattern    = ?default_pattern
    }};

% check if Pid is spawned process?
handle_info({_Pid, {answer, Result}}, #state{ reply = {From,_} = FromTag} = S) ->

    ok   = set_pattern_trace(pause, S#state.pattern),
    Bpd  = collect_bpd(),
    _    = set_process_trace(false, S#state.rootset, S#state.trace_opts),
    ok   = set_pattern_trace(false, S#state.pattern),

    catch unlink(From),
    gen_server:reply(FromTag, {ok, Result}),
    {noreply, S#state{
	profiling  = false,
	rootset    = [],
	trace_opts = [],
	pattern    = ?default_pattern,
	bpd        = Bpd
    }}.

%% -------------------------------------------------------------------- %%
%%
%% termination
%%
%% -------------------------------------------------------------------- %%

-doc false.
terminate(_Reason, #state{ fd = undefined }) ->
    ok = set_pattern_trace(false, ?default_pattern),
    ok;
terminate(_Reason, #state{ fd = Fd }) ->
    ok = file:close(Fd),
    ok = set_pattern_trace(false, ?default_pattern),
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
%% AUX Functions
%%
%% -------------------------------------------------------------------- %%

setup_profiling(M,F,A) ->
    spawn_link(fun() -> spin_profile(M,F,A) end).

spin_profile(M, F, A) ->
    receive
	{Pid, execute} ->
	    Pid ! {self(), {answer, erlang:apply(M,F,A)}}
    end.

execute_profiling(Pid) ->
    Pid ! {self(), execute},
    ok.


get_trace_options([]) ->
    [call];
get_trace_options([{set_on_spawn, true}|Opts]) ->
    [set_on_spawn | get_trace_options(Opts)];
get_trace_options([set_on_spawn|Opts]) ->
    [set_on_spawn | get_trace_options(Opts)];
get_trace_options([_|Opts]) ->
    get_trace_options(Opts).


set_pattern_trace(Flag, Pattern) ->
    erlang:system_flag(multi_scheduling, block),
    erlang:trace_pattern(on_load, Flag, [call_time]),
    erlang:trace_pattern(Pattern, Flag, [call_time]),
    erlang:system_flag(multi_scheduling, unblock),
    ok.

set_process_trace(_, [], _) -> true;
set_process_trace(Flag, [Pid|Pids], Options) when is_pid(Pid) ->
    try
	erlang:trace(Pid, Flag, Options),
	set_process_trace(Flag, Pids, Options)
    catch
	_:_ ->
	    {false, Pid}
    end;
set_process_trace(Flag, [Name|Pids], Options) when is_atom(Name) ->
    case whereis(Name) of
	undefined ->
	    set_process_trace(Flag, Pids, Options);
	Pid ->
	    set_process_trace(Flag, [Pid|Pids], Options)
    end.

collect_bpd() ->
    collect_bpd([M || M <- [element(1, Mi) || Mi <- code:all_loaded()], M =/= ?MODULE]).

collect_bpd(Ms) when is_list(Ms) ->
    collect_bpdf(collect_mfas(Ms)).

collect_mfas(Ms) ->
    lists:foldl(fun
	    (M, Mfas) ->
		Mfas ++ [{M, F, A} || {F, A} <- M:module_info(functions)]
	end, [], Ms).

collect_bpdf(Mfas) ->
    collect_bpdf(Mfas, #bpd{}).
collect_bpdf([], Bpd) ->
    Bpd;
collect_bpdf([Mfa|Mfas], #bpd{n = N, us = Us, p = Tree, mfa = Code } = Bpd) ->
    case erlang:trace_info(Mfa, call_time) of
	{call_time, []} ->
	    collect_bpdf(Mfas, Bpd);
	{call_time, Data} when is_list(Data) ->
	    {CTn, CTus, CTree} = collect_bpdfp(Mfa, Tree, Data),
	    collect_bpdf(Mfas, Bpd#bpd{
		    n   = CTn  + N,
		    us  = CTus + Us,
		    p   = CTree,
		    mfa = [{Mfa, {CTn, CTus}}|Code]
		});
	{call_time, false} ->
	    collect_bpdf(Mfas, Bpd);
	{call_time, _Other} ->
	    collect_bpdf(Mfas, Bpd)
    end.

collect_bpdfp(Mfa, Tree, Data) ->
     lists:foldl(fun
	({Pid, Ni, Si, Usi}, {PTno, PTuso, To}) ->
	    Time = Si * 1000000 + Usi,
	    Ti1  = case gb_trees:lookup(Pid, To) of
		none ->
		    gb_trees:enter(Pid, [{Mfa, {Ni, Time}}], To);
		{value, Pmfas} ->
		    gb_trees:enter(Pid, [{Mfa, {Ni, Time}}|Pmfas], To)
	    end,
	    {PTno + Ni, PTuso + Time, Ti1}
    end, {0,0, Tree}, Data).


-doc false.
analyze(Fd, procs, Opts, #bpd{ p = Ps, us = Tus }) ->
    lists:foreach(
      fun
          ({Pid, Mfas}) ->
              {Pn, Pus} =  sum_bp_total_n_us(Mfas),
              format(
                Fd,
                "~n****** Process ~w    -- ~s % of profiled time *** ~n",
                [Pid, s("~.2f", [100.0*divide(Pus, Tus)])]),
              print_bp_mfa(Mfas, {Pn,Pus}, Fd, Opts),
              ok
      end, gb_trees:to_list(Ps));
analyze(Fd, total, Opts, #bpd{ mfa = Mfas, n = Tn, us = Tus } ) ->
    print_bp_mfa(Mfas, {Tn, Tus}, Fd, Opts).

%% manipulators
sort_mfa(Bpfs, mfa) when is_list(Bpfs) ->
    lists:sort(fun
	    ({A,_}, {B,_}) when A < B -> true;
	    (_, _) -> false
	end, Bpfs);
sort_mfa(Bpfs, time) when is_list(Bpfs) ->
    lists:sort(fun
	    ({_,{_,A}}, {_,{_,B}}) when A < B -> true;
	    (_, _) -> false
	end, Bpfs);
sort_mfa(Bpfs, calls) when is_list(Bpfs) ->
    lists:sort(fun
	    ({_,{A,_}}, {_,{B,_}}) when A < B -> true;
	    (_, _) -> false
	end, Bpfs);
sort_mfa(Bpfs, _) when is_list(Bpfs) -> sort_mfa(Bpfs, time).

filter_mfa(Bpfs, Ts) when is_list(Ts) ->
    filter_mfa(Bpfs, [], proplists:get_value(calls, Ts, 0), proplists:get_value(time, Ts, 0));
filter_mfa(Bpfs, _) -> Bpfs.
filter_mfa([], Out, _, _) -> lists:reverse(Out);
filter_mfa([{_, {C, T}}=Bpf|Bpfs], Out, Ct, Tt) when C >= Ct, T >= Tt -> filter_mfa(Bpfs, [Bpf|Out], Ct, Tt);
filter_mfa([_|Bpfs], Out, Ct, Tt) -> filter_mfa(Bpfs, Out, Ct, Tt).

sum_bp_total_n_us(Mfas) ->
    lists:foldl(fun ({_, {Ci,Usi}}, {Co, Uso}) -> {Co + Ci, Uso + Usi} end, {0,0}, Mfas).

%% strings and format

string_bp_mfa(Mfas, Tus) -> string_bp_mfa(Mfas, Tus, {0,0,0,0,0}, []).
string_bp_mfa([], _, Ws, Strings) -> {Ws, lists:reverse(Strings)};
string_bp_mfa([{Mfa, {Count, Time}}|Mfas], Tus, {MfaW, CountW, PercW, TimeW, TpCW}, Strings) ->
	Smfa   = s(Mfa),
	Scount = s(Count),
	Stime  = s(Time),
	Sperc  = s("~.2f", [100*divide(Time,Tus)]),
	Stpc   = s("~.2f", [divide(Time,Count)]),

	string_bp_mfa(Mfas, Tus, {
		erlang:max(MfaW,  string:length(Smfa)),
		erlang:max(CountW,string:length(Scount)),
		erlang:max(PercW, string:length(Sperc)),
		erlang:max(TimeW, string:length(Stime)),
		erlang:max(TpCW,  string:length(Stpc))
	    }, [[Smfa, Scount, Sperc, Stime, Stpc] | Strings]).

print_bp_mfa(Mfas, {Tn, Tus}, Fd, Opts) ->
    Fmfas = filter_mfa(sort_mfa(Mfas, proplists:get_value(sort, Opts)), proplists:get_value(filter, Opts)),
    {{MfaW, CountW, PercW, TimeW, TpCW}, Strs} = string_bp_mfa(Fmfas, Tus),
    TnStr    = s(Tn),
    TusStr   = s(Tus),
    TuspcStr = s("~.2f", [divide(Tus,Tn)]),
    Ws = {erlang:max(string:length("FUNCTION"), MfaW),
          lists:max([string:length("CALLS"), CountW, string:length(TnStr)]),
          erlang:max(string:length("      %"), PercW),
          lists:max([string:length("TIME"), TimeW, string:length(TusStr)]),
          lists:max([string:length("uS / CALLS"), TpCW, string:length(TuspcStr)])},
    format(Fd, Ws, ["FUNCTION", "CALLS", "      %", "TIME", "uS / CALLS"]),
    format(Fd, Ws, ["--------", "-----", "-------", "----", "----------"]),
    lists:foreach(fun (String) -> format(Fd, Ws, String) end, Strs),
    format(Fd, Ws, [lists:duplicate(N,$-)||N <- tuple_to_list(Ws)]),
    format(Fd, Ws, ["Total:", TnStr, "100.00%", TusStr, TuspcStr]),
    ok.

s({M,F,A}) -> s("~w:~tw/~w",[M,F,A]);
s(Term) -> s("~tp", [Term]).
s(Format, Terms) -> lists:flatten(io_lib:format(Format, Terms)).


format(Fd, {MfaW, CountW, PercW, TimeW, TpCW}, Strings) ->
    format(Fd, s("~~.~wts  ~~~ws  ~~~ws  ~~~ws  [~~~ws]~~n", [MfaW, CountW, PercW, TimeW, TpCW]), Strings);
format(undefined, Format, Strings) ->
    io:format(Format, Strings),
    ok;
format(Fd, Format, Strings) ->
    io:format(Fd, Format, Strings),
    io:format(Format, Strings),
    ok.

divide(_,0) -> 0.0;
divide(T,N) -> T/N.

-dialyzer({no_match, start_internal/0}).
start_internal() ->
    case start() of
        {ok, _} -> ok;
        {error, {already_started,_}} -> ok;
        Error -> Error
    end.
