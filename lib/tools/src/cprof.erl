%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2020. All Rights Reserved.
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

%% Call count profiling tool.

-export ([start/0, start/1, start/2, start/3,
	  stop/0, stop/1, stop/2, stop/3,
	  restart/0, restart/1, restart/2, restart/3,
	  pause/0, pause/1, pause/2, pause/3,
	  analyse/0, analyse/1, analyse/2,
	  analyze/0, analyze/1, analyze/2]).



-spec start() -> non_neg_integer().

start() ->
    tr({'_','_','_'}, true) + tr(on_load, true).

-spec start(FuncSpec) -> non_neg_integer() when
      FuncSpec :: (Mod :: module()) | mfa() | {FS :: term()}.

start({_,_,_} = MFA) ->
    tr(MFA, true);
start({FuncSpec}) ->
    tr(FuncSpec, true);
start(M) ->
    tr({M,'_','_'}, true).

-spec start(Mod, Func) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom().

start(M,F) ->
    tr({M,F,'_'}, true).

-spec start(Mod, Func, Arity) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom(),
      Arity :: arity().

start(M,F,A) ->
    tr({M,F,A}, true).



-spec stop() -> non_neg_integer().

stop() ->
    tr({'_','_','_'}, false) + tr(on_load, false).

-spec stop(FuncSpec) -> non_neg_integer() when
      FuncSpec :: (Mod :: module()) | mfa() | {FS :: term()}.

stop({_,_,_} = MFA) ->
    tr(MFA, false);
stop({FuncSpec}) ->
    tr(FuncSpec, false);
stop(M) ->
    tr({M,'_','_'}, false).

-spec stop(Mod, Func) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom().

stop(M,F) ->
    tr({M,F,'_'}, false).

-spec stop(Mod, Func, Arity) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom(),
      Arity :: arity().

stop(M,F,A) ->
    tr({M,F,A}, false).


-spec restart() -> non_neg_integer().

restart() ->
    tr({'_','_','_'}, restart).

-spec restart(FuncSpec) -> non_neg_integer() when
      FuncSpec :: (Mod :: module()) | mfa() | {FS :: term()}.

restart({_,_,_} = MFA) ->
    tr(MFA, restart);
restart({FuncSpec}) ->
    tr(FuncSpec, restart);
restart(M) ->
    tr({M,'_','_'}, restart).

-spec restart(Mod, Func) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom().

restart(M,F) ->
    tr({M,F,'_'}, restart).

-spec restart(Mod, Func, Arity) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom(),
      Arity :: arity().

restart(M,F,A) ->
    tr({M,F,A}, restart).


-spec pause() -> non_neg_integer().

pause() ->
    tr({'_','_','_'}, pause) + tr(on_load, false).

-spec pause(FuncSpec) -> non_neg_integer() when
      FuncSpec :: (Mod :: module()) | mfa() | {FS :: term()}.

pause({_,_,_} = MFA) ->
    tr(MFA, pause);
pause({FuncSpec}) ->
    tr(FuncSpec, pause);
pause(M) ->
    tr({M,'_','_'}, pause).

-spec pause(Mod, Func) -> non_neg_integer() when
      Mod :: module(),
      Func :: atom().

pause(M,F) ->
    tr({M,F,'_'}, pause).

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

-spec analyse() -> {AllCallCount :: non_neg_integer(),
                    ModAnalysisList :: mod_analysis_list()}.

analyse() ->
    analyse(1).

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



analyze() ->
    analyse().

analyze(X) ->
    analyse(X).

analyze(X, Y) ->
    analyse(X, Y).



tr(FuncSpec, State) ->
    erlang:trace_pattern(FuncSpec, State, [call_count]).
