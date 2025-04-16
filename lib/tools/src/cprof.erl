%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2002-2025. All Rights Reserved.
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
-module(cprof).
-moduledoc """
A simple Call Count Profiling Tool using breakpoints for minimal runtime
performance impact.

The `cprof` module is used to profile a program to find out how many times
different functions are called. To minimize runtime performance impact,
breakpoints containing counters are used.

Since breakpoints are used there is no need for special compilation of the
modules to be profiled. These breakpoints can only be set on BEAM code, so
BIFs cannot be call-count traced.

The size of the call counters is the host machine word size. One bit is used
when pausing the counter, so the maximum counter value for a 32-bit host
is 2,147,483,647.

The profiling result is delivered as a term containing a sorted list of entries,
one per module. Each module entry contains a sorted list of functions. The
sorting order in both cases is of decreasing call count.

Call count tracing is lightweight compared to other forms of tracing,
such as `m:eprof` or `m:fprof`, since no trace messages have to be
generated. Some measurements indicates that the performance degradation is
about 10 percent.

For more information and some examples, see the
[User's Guide for `cprof`](cprof_chapter.md).
""".

%% Call count profiling tool.

-export ([start/0, start/1, start/2, start/3,
	  stop/0, stop/1, stop/2, stop/3,
	  restart/0, restart/1, restart/2, restart/3,
	  pause/0, pause/1, pause/2, pause/3,
	  analyse/0, analyse/1, analyse/2,
	  analyze/0, analyze/1, analyze/2]).



-doc """
Start call count tracing for all functions in all modules, and also for all
functions in modules to be loaded.

This is equivalent to
[`start('_', '_', '_') + start({on_load})`](`start/3`).
""".
-spec start() -> non_neg_integer().

start() ->
    tr({'_','_','_'}, true) + tr(on_load, true).

-doc """
If `FuncSpec` is an atom, it is assumed to be a module name, and
this call is equivalent to [`start(FuncSpec, '_', '_')`](`start/3`).

If `FuncSpec` is an MFA tuple, `{Module, Name, Arity`}, this call
is equivalent to [`start(Module, Name, Arity)`](`start/3`).

If `FuncSpec` is tuple `{FS}`, `FS` is the first argument to
`erlang:trace_pattern/3`. For example, if `FuncSpec` is `{on_load}`,
call counters will be set to zero and running for all functions in
modules to be loaded.
""".
-spec start(FuncSpec) -> non_neg_integer() when
      FuncSpec :: (Mod :: module()) | mfa() | {FS :: term()}.

start({_,_,_} = MFA) ->
    tr(MFA, true);
start({FuncSpec}) ->
    tr(FuncSpec, true);
start(M) ->
    tr({M,'_','_'}, true).

-doc #{equiv => start(Mod, Func, '_')}.
-spec start(Mod, Func) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom().

start(M, F) ->
    tr({M,F,'_'}, true).

-doc """
Start call count tracing for matching functions in matching modules.

Set call count breakpoints on the matching functions that has no call count
breakpoints. Call counters are set to zero and running for all matching
functions.

Return the number of matching functions that has call count breakpoints.
""".
-spec start(Mod, Func, Arity) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom(),
      Arity :: arity().

start(M,F,A) ->
    tr({M,F,A}, true).



-doc """
Stop call count tracing for all functions in all modules, and also for all
functions in modules to be loaded.

This is equivalent to
[`stop('_', '_', '_') + stop({on_load})`](`stop/3`).
""".
-spec stop() -> non_neg_integer().

stop() ->
    tr({'_','_','_'}, false) + tr(on_load, false).

-doc """
If `FuncSpec` is an atom, it is assumed to be a module name, and
this call is equivalent to [`stop(FuncSpec, '_', '_')`](`stop/3`).

If `FuncSpec` is an MFA tuple, `{Module, Name, Arity`}, this call
is equivalent to [`stop(Module, Name, Arity)`](`stop/3`).

If `FuncSpec` is tuple `{FS}`, `FS` is the first argument to
`erlang:trace_pattern/3`. For example, if `FuncSpec` is `{on_load}`,
call counters be disabled for all functions in modules to be loaded.
""".
-spec stop(FuncSpec) -> non_neg_integer() when
      FuncSpec :: (Mod :: module()) | mfa() | {FS :: term()}.

stop({_,_,_} = MFA) ->
    tr(MFA, false);
stop({FuncSpec}) ->
    tr(FuncSpec, false);
stop(M) ->
    tr({M,'_','_'}, false).

-doc #{equiv => stop(Mod, Func, '_')}.
-spec stop(Mod, Func) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom().

stop(M, F) ->
    tr({M,F,'_'}, false).

-doc """
Stop call count tracing for matching functions in matching modules.

Remove call count breakpoints from the matching functions that has call count
breakpoints.

Return the number of matching functions that can have call count breakpoints,
which is the same as [`start/*`](`start/3`) with the same arguments would have
returned.
""".
-spec stop(Mod, Func, Arity) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom(),
      Arity :: arity().

stop(M,F,A) ->
    tr({M,F,A}, false).


-doc #{equiv => restart('_', '_', '_')}.
-spec restart() -> non_neg_integer().

restart() ->
    tr({'_','_','_'}, restart).

-doc """
If `FuncSpec` is an atom, it is assumed to be a module name, and
this call is equivalent to [`restart(FuncSpec, '_', '_')`](`restart/3`).

If `FuncSpec` is an MFA tuple, `{Module, Name, Arity`}, this call
is equivalent to [`restart(Module, Name, Arity)`](`restart/3`).

If `FuncSpec` is tuple `{FS}`, `FS` is the first argument to
`erlang:trace_pattern/3`. For example, if `FuncSpec` is `{on_load}`,
call counters will be set to zero and running for all functions in
modules to be loaded.
""".
-spec restart(FuncSpec) -> non_neg_integer() when
      FuncSpec :: (Mod :: module()) | mfa() | {FS :: term()}.

restart({_,_,_} = MFA) ->
    tr(MFA, restart);
restart({FuncSpec}) ->
    tr(FuncSpec, restart);
restart(M) ->
    tr({M,'_','_'}, restart).

-doc #{equiv => restart(Mod, Func, '_')}.
-spec restart(Mod, Func) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom().

restart(M, F) ->
    tr({M,F,'_'}, restart).

-doc """
Restart call counters for the matching functions in matching modules that are
call-count traced.

The call counters for all matching functions that has call count breakpoints
are set to zero and running.

Return the number of matching functions that can have call count breakpoints,
the same as [`start/*`](`start/3`) with the same arguments would have
returned.
""".
-spec restart(Mod, Func, Arity) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom(),
      Arity :: arity().

restart(M,F,A) ->
    tr({M,F,A}, restart).


-doc """
Pause call count tracing for all functions in all modules and stop it for all
functions in modules to be loaded.

This call is equivalent to
[`pause('_', '_', '_') + stop({on_load})`](`pause/3`).
""".
-spec pause() -> non_neg_integer().

pause() ->
    tr({'_','_','_'}, pause) + tr(on_load, false).

-doc """
If `FuncSpec` is an atom, it is assumed to be a module name, and
this call is equivalent to [`pause(FuncSpec, '_', '_')`](`pause/3`).

If `FuncSpec` is an MFA tuple, `{Module, Name, Arity`}, this call
is equivalent to [`pause(Module, Name, Arity)`](`pause/3`).

If `FuncSpec` is tuple `{FS}`, `FS` is the first argument to
`erlang:trace_pattern/3`. For example, if `FuncSpec` is `{on_load}`,
call counters will be paused for all functions in modules to be loaded.
""".
-spec pause(FuncSpec) -> non_neg_integer() when
      FuncSpec :: (Mod :: module()) | mfa() | {FS :: term()}.

pause({_,_,_} = MFA) ->
    tr(MFA, pause);
pause({FuncSpec}) ->
    tr(FuncSpec, pause);
pause(M) ->
    tr({M,'_','_'}, pause).

-doc #{equiv => pause(Mod, Func, '_')}.
-spec pause(Mod, Func) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom().

pause(M, F) ->
    tr({M,F,'_'}, pause).

-doc """
Pause call counters for matching functions in matching modules.

The call counters for all matching functions that have call count breakpoints
are paused at their current count.

Return the number of matching functions that can have call count breakpoints,
the same as [`start/*`](`start/3`) with the same arguments would have
returned.
""".
-spec pause(Mod, Func, Arity) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom(),
      Arity :: arity().

pause(M, F, A) ->
    tr({M,F,A}, pause).


-type mod_analysis_list() :: [mod_analysis()].
-type mod_analysis() :: {Mod :: module(),
                         ModCallCount :: non_neg_integer(),
                         FuncAnalysisList :: func_analysis_list()}.
-type func_analysis_list() :: [{mfa(), FuncCallCount :: non_neg_integer()}].

-doc(#{equiv => analyse(1)}).
-spec analyse() -> {AllCallCount :: non_neg_integer(),
                    ModAnalysisList :: mod_analysis_list()}.

analyse() ->
    analyse(1).

-doc """
analyse(ModLimit)

Collect call counters for one or more modules.

If `ModLimit` is a module name (an atom), this call is equivalent to
[`analyse(ModLimit, 1)`](`analyse/2`).

If `ModLimit` is an integer, this function calls
[`analyse(Module, ModLimit)`](`analyse/2`) for each `Module` that is
currently loaded (except the `cprof` module itself).
The result from those calls are returned in a list.

""".
-spec analyse(Limit) -> {AllCallCount :: non_neg_integer(),
                         ModAnalysisList :: mod_analysis_list()} when
                  Limit :: non_neg_integer();
             (Mod) -> ModAnalysis :: mod_analysis() when
                  Mod :: module().

analyse(Limit) when is_integer(Limit) ->
    L0 = [analyse(element(1, Mod), Limit) || Mod <- code:all_loaded()],
    L1 = [{C,M,Lm} || {M,C,Lm} <- L0, C > 0, M =/= ?MODULE],
    N = lists:foldl(fun ({C,_,_}, Q) -> Q+C end, 0, L1),
    L = [{M,C,Lm} || {C,M,Lm} <- lists:reverse(lists:sort(L1))],
    {N,L};
analyse(M) when is_atom(M) ->
    analyse(M, 1).

-doc """
analyse(Module, Limit)

Collects and analyses all call counters for module `Module`.

This function returns:

```
{Module, ModuleCount, FuncAnalysisList}
```

where `FuncAnalysisList` is a list of tuples, one for each function:

```
{{Module, FunctionName, Arity}, FuncCallCount}
```

If call counters are still running while `analyse/0,1,2` is executing, the result
could be inconsistent. This happens if the process executing `analyse/0,1,2`
is scheduled out so some other process can increment the counters that are
being analysed. Calling [`pause()`](`pause/0`) before analysing takes care of
that problem.

All functions with a `FuncCallCount` lower than `Limit` are excluded from
`FuncAnalysisList`. They are still included in `ModCallCount`, though.
""".
-spec analyse(Mod, Limit) -> ModAnalysis :: mod_analysis() when
      Mod :: module(),
      Limit :: non_neg_integer().

-dialyzer({no_improper_lists, analyse/2}).
analyse(M, Limit) when is_atom(M), is_integer(Limit) ->
    L0 = [begin
	      MFA = {M,F,A},
	      {_,C} = erlang:trace_info(MFA, call_count),
	      [C|MFA]
	  end || {F,A} <- M:module_info(functions)],
    L1 = [X || [C|_]=X <- L0, is_integer(C)],
    N = lists:foldl(fun ([C|_], Q) -> Q+C end, 0, L1),
    L2 = [X || [C|_]=X <- L1, C >= Limit],
    L = [{MFA,C} || [C|MFA] <- lists:reverse(lists:sort(L2))],
    {M,N,L}.


-doc false.
analyze() ->
    analyse().

-doc false.
analyze(X) ->
    analyse(X).

-doc false.
analyze(X, Y) ->
    analyse(X, Y).


tr(FuncSpec, State) ->
    erlang:trace_pattern(FuncSpec, State, [call_count]).
