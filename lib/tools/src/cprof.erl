%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2024. All Rights Reserved.
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
different functions are called. Breakpoints similar to local call trace, but
containing a counter, are used to minimise runtime performance impact.

Since breakpoints are used there is no need for special compilation of any
module to be profiled. For now these breakpoints can only be set on BEAM code so
BIFs cannot be call count traced.

The size of the call counters is the host machine word size. One bit is used
when pausing the counter, so the maximum counter value for a 32-bit host
is 2147483647.

The profiling result is delivered as a term containing a sorted list of entries,
one per module. Each module entry contains a sorted list of functions. The
sorting order in both cases is of decreasing call count.

Call count tracing is very lightweight compared to other forms of tracing since
no trace message has to be generated. Some measurements indicates performance
degradation in the vicinity of 10 percent.

## See Also

`m:eprof`(3), `m:fprof`(3), erlang(3), [User's Guide](cprof_chapter.md)
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
functions in modules to be loaded. This is the same as
`(start({'_','_','_'})+start({on_load}))`.

See also [`start/1..3`](`start/1`) below.
""".
-spec start() -> non_neg_integer().

start() ->
    tr({'_','_','_'}, true) + tr(on_load, true).

-doc(#{equiv => start/3}).
-spec start(FuncSpec) -> non_neg_integer() when
      FuncSpec :: (Mod :: module()) | mfa() | {FS :: term()}.

start({_,_,_} = MFA) ->
    tr(MFA, true);
start({FuncSpec}) ->
    tr(FuncSpec, true);
start(M) ->
    tr({M,'_','_'}, true).

-doc(#{equiv => start/3}).
-spec start(Mod, Func) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom().

start(M,F) ->
    tr({M,F,'_'}, true).

-doc """
Start call count tracing for matching functions in matching modules. The `FS`
argument can be used to specify the first argument to `erlang:trace_pattern/3`,
for example `on_load`.

Set call count breakpoints on the matching functions that has no call count
breakpoints. Call counters are set to zero and running for all matching
functions.

Return the number of matching functions that has got call count breakpoints.
""".
-spec start(Mod, Func, Arity) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom(),
      Arity :: arity().

start(M,F,A) ->
    tr({M,F,A}, true).



-doc """
Stop call count tracing for all functions in all modules, and also for all
functions in modules to be loaded. This is the same as
`(stop({'_','_','_'})+stop({on_load}))`.

See also [`stop/1..3`](`stop/1`) below.
""".
-spec stop() -> non_neg_integer().

stop() ->
    tr({'_','_','_'}, false) + tr(on_load, false).

-doc(#{equiv => stop/3}).
-spec stop(FuncSpec) -> non_neg_integer() when
      FuncSpec :: (Mod :: module()) | mfa() | {FS :: term()}.

stop({_,_,_} = MFA) ->
    tr(MFA, false);
stop({FuncSpec}) ->
    tr(FuncSpec, false);
stop(M) ->
    tr({M,'_','_'}, false).

-doc(#{equiv => stop/3}).
-spec stop(Mod, Func) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom().

stop(M,F) ->
    tr({M,F,'_'}, false).

-doc """
Stop call count tracing for matching functions in matching modules. The `FS`
argument can be used to specify the first argument to `erlang:trace_pattern/3`,
for example `on_load`.

Remove call count breakpoints from the matching functions that has call count
breakpoints.

Return the number of matching functions that can have call count breakpoints,
the same as [`start/0..3`](`start/0`) with the same arguments would have
returned.
""".
-spec stop(Mod, Func, Arity) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom(),
      Arity :: arity().

stop(M,F,A) ->
    tr({M,F,A}, false).


-doc(#{equiv => restart/3}).
-spec restart() -> non_neg_integer().

restart() ->
    tr({'_','_','_'}, restart).

-doc(#{equiv => restart/3}).
-spec restart(FuncSpec) -> non_neg_integer() when
      FuncSpec :: (Mod :: module()) | mfa() | {FS :: term()}.

restart({_,_,_} = MFA) ->
    tr(MFA, restart);
restart({FuncSpec}) ->
    tr(FuncSpec, restart);
restart(M) ->
    tr({M,'_','_'}, restart).

-doc(#{equiv => restart/3}).
-spec restart(Mod, Func) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom().

restart(M,F) ->
    tr({M,F,'_'}, restart).

-doc """
Restart call counters for the matching functions in matching modules that are
call count traced. The `FS` argument can be used to specify the first argument
to `erlang:trace_pattern/3`.

The call counters for all matching functions that has got call count breakpoints
are set to zero and running.

Return the number of matching functions that can have call count breakpoints,
the same as [`start/0..3`](`start/0`) with the same arguments would have
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
functions in modules to be loaded. This is the same as
`(pause({'_','_','_'})+stop({on_load}))`.

See also [`pause/1..3`](`pause/1`) below.
""".
-spec pause() -> non_neg_integer().

pause() ->
    tr({'_','_','_'}, pause) + tr(on_load, false).

-doc(#{equiv => pause/3}).
-spec pause(FuncSpec) -> non_neg_integer() when
      FuncSpec :: (Mod :: module()) | mfa() | {FS :: term()}.

pause({_,_,_} = MFA) ->
    tr(MFA, pause);
pause({FuncSpec}) ->
    tr(FuncSpec, pause);
pause(M) ->
    tr({M,'_','_'}, pause).

-doc(#{equiv => pause/3}).
-spec pause(Mod, Func) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom().

pause(M,F) ->
    tr({M,F,'_'}, pause).

-doc """
Pause call counters for matching functions in matching modules. The `FS`
argument can be used to specify the first argument to `erlang:trace_pattern/3`.

The call counters for all matching functions that has got call count breakpoints
are paused at their current count.

Return the number of matching functions that can have call count breakpoints,
the same as [`start/0..3`](`start/0`) with the same arguments would have
returned.
""".
-spec pause(Mod, Func, Arity) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom(),
      Arity :: arity().

pause(M,F,A) ->
    tr({M,F,A}, pause).



-type mod_analysis_list() :: [mod_analysis()].
-type mod_analysis() :: {Mod :: module(),
                         ModCallCount :: non_neg_integer(),
                         FuncAnalysisList :: func_analysis_list()}.
-type func_analysis_list() :: [{mfa(), FuncCallCount :: non_neg_integer()}].

-doc(#{equiv => analyse/2}).
-spec analyse() -> {AllCallCount :: non_neg_integer(),
                    ModAnalysisList :: mod_analysis_list()}.

analyse() ->
    analyse(1).

-doc(#{equiv => analyse/2}).
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
Collects and analyses the call counters presently in the node for either module
`Mod`, or for all modules (except `cprof` itself), and returns:

- **`FuncAnalysisList`** - A list of tuples, one for each function in a module,
  in decreasing `FuncCallCount` order.

- **`ModCallCount`** - The sum of `FuncCallCount` values for all functions in
  module `Mod`.

- **`AllCallCount`** - The sum of `ModCallCount` values for all modules
  concerned in `ModAnalysisList`.

- **`ModAnalysisList`** - A list of tuples, one for each module except `cprof`,
  in decreasing `ModCallCount` order.

If call counters are still running while `analyse/0..2` is executing, you might
get an inconsistent result. This happens if the process executing `analyse/0..2`
gets scheduled out so some other process can increment the counters that are
being analysed, Calling [`pause()`](`pause/0`) before analysing takes care of
the problem.

If the `Mod` argument is given, the result contains a `ModAnalysis` tuple for
module `Mod` only, otherwise the result contains one `ModAnalysis` tuple for all
modules returned from [`code:all_loaded()`](`code:all_loaded/0`) except `cprof`
itself.

All functions with a `FuncCallCount` lower than `Limit` are excluded from
`FuncAnalysisList`. They are still included in `ModCallCount`, though. The
default value for `Limit` is `1`.
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
