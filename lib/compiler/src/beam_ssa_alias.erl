%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023. All Rights Reserved.
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

-module(beam_ssa_alias).

-export([opt/2]).

-import(lists, [foldl/3, reverse/1]).

-include("beam_ssa_opt.hrl").
-include("beam_types.hrl").

%% -define(DEBUG, true).

-ifdef(DEBUG).
-define(DP(FMT, ARGS), io:format(FMT, ARGS)).
-define(DP(FMT), io:format(FMT)).
-else.
-define(DP(FMT, ARGS), skip).
-define(DP(FMT), skip).
-endif.

%% A code location refering to either the #b_set{} defining a variable
%% or the terminator of a block.
-type kill_loc() :: #b_var{} | {terminator, beam_ssa:label()}.

%% Map a code location to the set of variables which die at that
%% location.
-type kill_set() :: #{ kill_loc() => sets:set(#b_var{}) }.

-type kills_map() :: #{ func_id() => kill_set() }.

%% Record holding the liveness information for a code location.
-record(liveness_st, {
                      in = sets:new([{version,2}]) :: sets:set(#b_var{}),
                      out = sets:new([{version,2}]) :: sets:set(#b_var{})
                     }).

%%%
%%% Optimization pass which calculates the alias status of values and
%%% uses the results to transform the code.
%%%
-spec opt(st_map(), func_info_db()) -> {st_map(), func_info_db()}.

opt(StMap0, FuncDb0) ->
    %% Ignore functions which are not in the function db (never
    %% called) or are stubs for nifs.
    Funs = [ F || F <- maps:keys(StMap0),
                  is_map_key(F, FuncDb0), not is_nif(F, StMap0)],
    Liveness = liveness(Funs, StMap0),
    _KillsMap = killsets(Liveness, StMap0),
    {StMap0, FuncDb0}.

%%%
%%% Calculate liveness for each function using the standard iterative
%%% fixpoint method.
%%%

-spec liveness([func_id()], st_map()) ->
          [{func_id(), #{func_id() => {beam_ssa:label(), #liveness_st{}}}}].

liveness([F|Funs], StMap) ->
    Liveness = liveness_fun(F, StMap),
    [{F,Liveness}|liveness(Funs, StMap)];
liveness([], _StMap) ->
    [].

liveness_fun(F, StMap0) ->
    #opt_st{ssa=SSA} = map_get(F, StMap0),
    State0 = maps:from_list([ {Lbl, #liveness_st{}} || {Lbl,_} <- SSA]),
    liveness_blks_fixp(reverse(SSA), State0, false).

liveness_blks_fixp(_SSA, State0, State0) ->
    State0;
liveness_blks_fixp(SSA, State0, _Old) ->
    State = liveness_blks(SSA, State0),
    liveness_blks_fixp(SSA, State, State0).

liveness_blks([{Lbl,Blk}|Blocks], State0) ->
    OutOld = get_live_out(Lbl, State0),
    Defs = blk_defs(Blk),
    Uses = blk_effective_uses(Blk),
    In = sets:union(Uses, sets:subtract(OutOld, Defs)),
    Out = successor_live_ins(Blk, State0),
    liveness_blks(Blocks, set_block_liveness(Lbl, In, Out, State0));
liveness_blks([], State0) ->
    State0.

get_live_in(Lbl, State) ->
    #liveness_st{in=In} = map_get(Lbl, State),
    In.

get_live_out(Lbl, State) ->
    #liveness_st{out=Out} = map_get(Lbl, State),
    Out.

set_block_liveness(Lbl, In, Out, State) ->
    L = map_get(Lbl, State),
    State#{Lbl => L#liveness_st{in=In,out=Out}}.

successor_live_ins(Blk, State) ->
    foldl(fun(Lbl, Acc) ->
                  sets:union(Acc, get_live_in(Lbl, State))
          end, sets:new([{version,2}]), beam_ssa:successors(Blk)).

blk_defs(#b_blk{is=Is}) ->
    foldl(fun(#b_set{dst=Dst}, Acc) ->
                  sets:add_element(Dst, Acc)
          end, sets:new([{version,2}]), Is).

blk_effective_uses(#b_blk{is=Is,last=Last}) ->
    %% We can't use beam_ssa:used/1 on the whole block as it considers
    %% a use after a def a use and that will derail the liveness
    %% calculation.
    blk_effective_uses([Last|reverse(Is)], sets:new([{version,2}])).

blk_effective_uses([I|Is], Uses0) ->
    Uses = case I of
               #b_set{dst=Dst} ->
                   %% The uses after the def do not count
                   sets:del_element(Dst, Uses0);
               _ -> % A terminator, no defs
                   Uses0
           end,
    LocalUses = sets:from_list(beam_ssa:used(I), [{version,2}]),
    blk_effective_uses(Is, sets:union(Uses, LocalUses));
blk_effective_uses([], Uses) ->
    Uses.

%%%
%%% Predicate to check if a function is the stub for a nif.
%%%
-spec is_nif(func_id(), st_map()) -> boolean().

is_nif(F, StMap) ->
    #opt_st{ssa=[{0,#b_blk{is=Is}}|_]} = map_get(F, StMap),
    case Is of
        [#b_set{op=nif_start}|_] ->
            true;
        _ -> false
    end.

%%%
%%% Calculate the killset for all functions in the liveness
%%% information.
%%%
-spec killsets([{func_id(),
                 #{func_id() => {beam_ssa:label(), #liveness_st{}}}}],
               st_map()) -> kills_map().

killsets(Liveness, StMap) ->
    maps:from_list([{F, kills_fun(F, StMap, Live)} || {F, Live} <- Liveness]).

%%%
%%% Calculate the killset for a function. The killset allows us to
%%% look up the variables that die at a code location.
%%%
kills_fun(Fun, StMap, Liveness) ->
    #opt_st{ssa=SSA} = map_get(Fun, StMap),
    kills_fun1(SSA, #{}, Liveness).

kills_fun1([{Lbl,Blk}|Blocks], KillsMap0, Liveness) ->
    KillsMap = kills_block(Lbl, Blk, map_get(Lbl, Liveness), KillsMap0),
    kills_fun1(Blocks, KillsMap, Liveness);
kills_fun1([], KillsMap, _) ->
    KillsMap.

kills_block(Lbl, #b_blk{is=Is,last=Last}, #liveness_st{out=Out}, KillsMap0) ->
    kills_is([Last|reverse(Is)], Out, KillsMap0, Lbl).

kills_is([I|Is], Live0, KillsMap0, Blk) ->
    {Live, Key} = case I of
                      #b_set{dst=Dst} ->
                          {sets:del_element(Dst, Live0), Dst};
                      _ ->
                          {Live0, {terminator, Blk}}
                  end,
    Uses = sets:from_list(beam_ssa:used(I), [{version,2}]),
    RemainingUses = sets:union(Live0, Uses),
    Killed = sets:subtract(RemainingUses, Live0),
    KillsMap = KillsMap0#{Key => Killed},
    kills_is(Is, sets:union(Live, Killed), KillsMap, Blk);
kills_is([], _, KillsMap, _) ->
    KillsMap.
