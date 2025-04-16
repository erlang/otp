%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
%% This module provides a database storing the sharing status for the
%% variables in a function. The database is represented as a graph
%% (beam_digraph) where the nodes are variables and values. Edges are
%% extraction and embedding operations. Nodes are labeled with the
%% sharing status of the variable and edges are labeled with the type
%% of extraction/embedding operation.
%%

-module(beam_ssa_ss).
-moduledoc false.

-compile({inline,[add_edge/4, add_vertex/3]}).

-export([add_var/3,
         derive_from/3,
         embed_in/3,
         extract/4,
         forward_status/2,
         get_status/2,
         initialize_in_args/1,
         meet_in_args/1,
         merge/2,
         merge_in_args/3,
         new/0,
         new/3,
         phi/4,
         prune/3,
         prune_by_add/2,
         set_call_result/4,
         set_status/3,
         size/1,
         variables/1]).

-include("beam_ssa.hrl").
-include("beam_types.hrl").
-include("beam_ssa_alias_debug.hrl").

-ifdef(PROVIDE_DUMP).
-export([dump/1]).
-endif.

-import(lists, [foldl/3]).

-define(ARGS_DEPTH_LIMIT, 4).
-define(SS_DEPTH_LIMIT, 30).

-ifdef(DEBUG_SS).
-define(DP(FMT, ARGS), io:format(FMT, ARGS)).
-define(DP(FMT), io:format(FMT)).
-else.
-define(DP(FMT, ARGS), skip).
-define(DP(FMT), skip).
-endif.

-ifdef(SS_EXTRA_ASSERTS).
-define(assert_state(State), assert_state(State)).
-define(ASSERT(Assert), Assert).
-else.
-define(assert_state(State), State).
-define(ASSERT(Assert), skip).
-endif.

-type sharing_state() :: any(). % A beam_digraph graph.
-type sharing_status() :: 'unique' | 'aliased' | 'no_info'.
-type element() :: 'hd' | 'tl' | non_neg_integer().

-spec add_var(beam_ssa:b_var(), sharing_status(), sharing_state()) ->
          sharing_state().
add_var(V, Status, State) ->
    ?DP("Adding variable ~p as ~p~n", [V,Status]),
    ?assert_state(add_vertex(State, V, Status)).

add_vertex(State, V, Status) ->
    ?ASSERT(case V of
                #b_var{} -> ok;
                plain -> ok;
                #b_literal{} -> ok
            end),
    beam_digraph:add_vertex(State, V, Status).

add_edge(State, Src, Dst, Lbl) ->
    ?ASSERT(begin
                case Src of
                    #b_var{} -> ok;
                    plain -> ok;
                    #b_literal{} -> ok
                end,
                #b_var{} = Dst,
                case Lbl of
                    {extract,Element} when is_integer(Element);
                                           Element =:= hd ; Element =:= tl ->
                        ok;
                    embed ->
                        ok;
                    {embed,Element} when is_integer(Element);
                                         Element =:= hd ; Element =:= tl ->
                        ok
                end
            end),
    beam_digraph:add_edge(State, Src, Dst, Lbl).

-spec derive_from(beam_ssa:b_var(), beam_ssa:b_var(), sharing_state()) ->
          sharing_state().
derive_from(Dst, Src, State) ->
    ?DP("Deriving ~p from ~p~nSS:~n~s~n", [Dst, Src, dump(State)]),
    ?assert_state(State),
    case {beam_digraph:vertex(State, Dst, unique),
          beam_digraph:vertex(State, Src, plain)} of
        {_,plain} ->
            State;
        {aliased,_} ->
            %% Nothing to do, already aliased.
            State;
        {_,aliased} ->
            %% The source is aliased, the destination will become aliased.
            ?assert_state(set_status(Dst, aliased, State));
        {_,_} ->
            case has_out_edges(Src, State) of
                true ->
                    %% The source has already been embedded in a term,
                    %% both destination and source will be aliased.
                    ?assert_state(set_status(Src, aliased,
                                             set_status(Dst, aliased, State)));
                false ->
                    %% Source is not aliased and has not been embedded
                    %% in a term, record that it now is.
                    State1 = case beam_digraph:has_vertex(State, Dst) of
                                 true ->
                                     State;
                                 false ->
                                     beam_ssa_ss:add_var(Dst, unique, State)
                             end,
                    ?assert_state(add_edge(State1, Src, Dst, embed))
            end
    end.

-spec embed_in(beam_ssa:b_var(), [{element(),beam_ssa:b_var()}],
               sharing_state()) -> sharing_state().
embed_in(Dst, Elements, State0) ->
    ?DP("Embedding ~p into ~p~nSS:~n~s~n", [Elements, Dst, dump(State0)]),
    ?assert_state(State0),
    ?ASSERT(assert_variable_exists(Dst, State0)),
    foldl(fun({Element,Src}, Acc) ->
                  add_embedding(Dst, Src, Element, Acc)
          end, State0, Elements).

add_embedding(Dst, Src, Element, State0) ->
    ?DP("add_embedding(~p, ~p, ~p, ...)~n", [Dst,Src,Element]),
    %% Create a node for literals and plain values as they are not in
    %% the graph. The node is needed to record the status of the
    %% element.
    State1 = case Src of
                 plain ->
                     beam_digraph:add_vertex(State0, Src, unique);
                 #b_literal{} ->
                     beam_digraph:add_vertex(State0, Src, unique);
                 _ ->
                     case beam_digraph:has_vertex(State0, Src) of
                         true ->
                             State0;
                         false ->
                             beam_digraph:add_vertex(State0, Src, unique)
                     end
             end,

    %% Create the edge, this is done regardless of the aliasing status
    %% as it is how the status of an element can be looked up.
    ?ASSERT(case Element of
                hd -> ok;
                tl -> ok;
                E when is_integer(E), E >= 0 -> ok
            end),
    State = ?assert_state(add_edge(State1, Src, Dst, {embed,Element})),

    %% If the variable being embedded ends up with more than one
    %% out-edge, the source will be aliased, unless it is a plain value.
    case beam_digraph:out_edges(State, Src) of
        [_] ->
            State;
        _ when Src =/= plain ->
            case Src of
                #b_literal{} ->
                    State;
                _ ->
                    set_status(Src, aliased, State)
            end;
        _ when Src =:= plain ->
            State
    end.

-spec extract(beam_ssa:b_var(), beam_ssa:b_var(), element(),
              sharing_state()) -> sharing_state().
extract(Dst, Src, Element, State) ->
    ?DP("Extracting ~p[~p] into ~p~n", [Src,Element,Dst]),
    ?assert_state(State),
    case beam_digraph:vertex(State, Src, plain) of
        aliased ->
            %% The pair/tuple is aliased, so what is extracted will be aliased.
            ?assert_state(set_status(Dst, aliased, State));
        unique ->
            %% The aggregate is unique, but elements can be aliased.
            OutEdges = beam_digraph:out_edges(State, Src),
            ?ASSERT(true = is_integer(Element)
                    orelse (Element =:= hd) orelse (Element =:= tl)),
            ?DP("dst: ~p, src: ~p, e: ~p, out-edges: ~p~n",
                [Dst, Src, Element, OutEdges]),
            extract_element(Dst, Src, Element, OutEdges, State);
        no_info ->
            ?assert_state(set_status(Dst, no_info, State));
        plain ->
            %% Extracting from a plain value is not possible, but the
            %% alias analysis pass can sometimes encounter it when no
            %% type information is available. Conservatively set the
            %% result to aliased.
            ?assert_state(set_status(Dst, aliased, State))
    end.

%% Note that extract_element/5 will never be given an out edge with a
%% plain 'embed' label as the source would have been aliased if it was
%% live past the point where it was embedded.
extract_element(Dst, _Src, Element, [{_,Old,{extract,Element}}|_], State) ->
    %% This element is already extracted, the old extracted value
    %% becomes aliased as well as Dst.
    ?assert_state(set_status(Old, aliased, set_status(Dst, aliased, State)));
extract_element(Dst, Src, Element, [{_,_,{extract,_}}|Rest], State) ->
    %% This is a recorded extraction of another element.
    extract_element(Dst, Src, Element, Rest, State);
extract_element(Dst, Src, Element, [], State0) ->
    %% This element has not been extracted before and Src is not
    %% aliased (checked in extract/4). It could be that we're about to
    %% extract an element which is known to be aliased.
    ?DP("  the element has not been extracted so far~n"),
    State1 = beam_ssa_ss:add_var(Dst, unique, State0),
    State = ?assert_state(add_edge(State1, Src, Dst, {extract,Element})),
    extract_status_for_element(Element, Src, Dst, State).

extract_status_for_element(Element, Src, Dst, State0) ->
    ?DP("    extract_status_for_element(~p, ~p)~n", [Element, Src]),
    InEdges = beam_digraph:in_edges(State0, Src),
    ?DP("    in-edges: ~p~n", [InEdges]),
    Embeddings = [Var || {Var,_,{embed,SE}} <- InEdges, Element =:= SE],
    Extracts = [Ex || {_,_,{extract,_}}=Ex <- InEdges],
    case {Embeddings,Extracts} of
        {[],[]} ->
            %% Nothing found, the status will be aliased.
            ?DP("    no known source~n"),
            ?DP("    status of ~p will be aliased~n", [Dst]),
            ?assert_state(set_status(Dst, aliased, State0));
        {[Var],[]} ->
            %% The element which is looked up is an embedding.
            ?DP("    found embedding~n"),
            ?DP("    the source is ~p~n", [Var]),
            ?DP("    SS~n~s~n", [dump(State0)]),
            ?DP("    status ~p~n", [beam_digraph:vertex(State0, Var, unique)]),
            State = set_status(Dst, beam_digraph:vertex(State0, Var, unique),
                               State0),
            ?DP("    Returned SS~n~s~n", [dump(State)]),
            ?assert_state(State);
        {[], [{Aggregate,_Dst,{extract,E}}]} ->
            %% We are trying extract from an extraction.
            S = get_status_of_extracted_element(Aggregate, [E,Element], State0),
            ?DP("    status of ~p will be ~p~n", [Dst, S]),
            ?assert_state(set_status(Dst, S, State0))
    end.


get_status_of_extracted_element(Aggregate, [First|Rest]=Elements, State) ->
    ?DP("    ~s(~p, ~p, ...)~n", [?FUNCTION_NAME, Aggregate, Elements]),
    %% This clause will only be called when there is a chain of
    %% extracts from unique aggregates. This implies that when the
    %% chain is traced backwards, no aliased aggregates will be found,
    %% but in case that invariant is ever broken, assert.
    ?ASSERT(unique = beam_digraph:vertex(State, Aggregate, unique)),
    ?DP("      aggregate is unique~n"),
    InEdges = beam_digraph:in_edges(State, Aggregate),
    Embeddings = [Src || {Src,_,{embed,E}} <- InEdges, First =:= E],
    Extracts = [{Src,E} || {Src,_,{extract,E}} <- InEdges],
    ?DP("      embeddings ~p~n", [Embeddings]),
    ?DP("      extracts ~p~n", [Extracts]),
    case {Embeddings,Extracts} of
        {[Embedding],[]} ->
            get_status_of_extracted_element(Embedding, Rest, State);
        {[],[{Extract,E}]} ->
            get_status_of_extracted_element(Extract, [E|Elements], State);
        {[],[]} ->
            aliased
    end;
get_status_of_extracted_element(Aggregate, [], State) ->
    ?DP("    ~s(~p, [], ...)~n", [?FUNCTION_NAME, Aggregate]),
    S = beam_digraph:vertex(State, Aggregate, unique),
    ?DP("      bottomed out, status is ~p~n", [S]),
    S.

%% A cut-down version of merge/2 which only considers variables in
%% Main and whether they have been aliased in Other.
-spec forward_status(sharing_state(), sharing_state()) -> sharing_state().
forward_status(Main, Other) ->
    ?DP("Forwarding state~n"),
    ?assert_state(Main),
    ?assert_state(Other),
    R = beam_digraph:foldv(
          Main,
          fun(#b_var{}=V, S, Acc) when S =/= aliased ->
                  maybe
                      true ?= beam_digraph:has_vertex(Other, V),
                      aliased ?= get_status(V, Other),
                      set_status(V, aliased, Acc)
                  else
                      _ ->
                          Acc
                  end;
             (_, _, Acc) ->
                  Acc
          end, Main),
    ?assert_state(R).


-spec get_status(beam_ssa:b_var(), sharing_state()) ->
          sharing_status().
get_status(V=#b_var{}, State) ->
    beam_digraph:vertex(State, V, unique).

-spec merge(sharing_state(), sharing_state()) -> sharing_state().
merge(StateA, StateB) ->
    ?DP("Merging states~n"),
    ?assert_state(StateA),
    ?assert_state(StateB),
    SizeA = beam_digraph:no_vertices(StateA),
    SizeB = beam_digraph:no_vertices(StateB),
    %% Always merge the smaller state into the larger.
    {Small,Large} = if SizeA < SizeB ->
                            {StateA,StateB};
                       true ->
                            {StateB,StateA}
                    end,
    ?DP("Merging Small into Large~nLarge:~n"),
    ?DP("Small:~n"),
    ?DP(dump(Small)),
    ?DP("Large:~n"),
    ?DP(dump(Large)),
    R = merge(Large, Small, beam_digraph:vertices(Small),
              sets:new(), sets:new()),
    ?assert_state(R).

merge(Dest, Source, [{V,VStatus}|Vertices], Edges0, Forced) ->

    Edges = accumulate_edges(V, Source, Edges0),
    DestStatus = beam_digraph:vertex(Dest, V, false),
    case {DestStatus,VStatus} of
        {Status,Status} ->
            %% Same status in both states.
            merge(Dest, Source, Vertices, Edges, Forced);
        {false,Status} ->
            %% V does not exist in Dest.
            merge(add_vertex(Dest, V, Status),
                  Source, Vertices, Edges, Forced);
        {unique,aliased} ->
            %% Alias in Dest.
            merge(set_status(V, aliased, Dest), Source,
                  Vertices, Edges, Forced);
        {aliased,unique} ->
            %% V has to be revisited and non-aliased copied parts will
            %% be aliased.
            merge(Dest, Source, Vertices, Edges, sets:add_element(V, Forced));
        {aliased,no_info} ->
            %% Nothing to do.
            merge(Dest, Source, Vertices, Edges, Forced);
        {no_info,aliased} ->
            %% Alias in Dest.
            merge(set_status(V, aliased, Dest), Source,
                  Vertices, Edges, Forced)
    end;
merge(Dest0, _Source, [], Edges, Forced) ->
    merge1(Dest0, _Source, sets:to_list(Edges),
           sets:new(), Forced).

merge1(Dest0, Source, [{plain,To,Lbl}|Edges], Fixups, Forced) ->
    ?DP("  Adding edge ~p -> ~p, lbl: ~p~n", [plain,To,Lbl]),
    Dest = add_edge(Dest0, plain, To, Lbl),
    merge1(Dest, Source, Edges, Fixups, Forced);
merge1(Dest0, Source, [{From,To,Lbl}=Edge|Edges], Fixups, Forced) ->
    ?DP("  Adding edge ~p -> ~p, lbl: ~p~n", [From,To,Lbl]),
    OutEdges = beam_digraph:out_edges(Dest0, From),
    case {ordsets:is_element(Edge, OutEdges),ordsets:is_empty(OutEdges)} of
        {true,_} ->
            ?DP("   Already exists~n"),
            merge1(Dest0, Source, Edges, Fixups, Forced);
        {false,true} ->
            ?DP("   Edge is new~n"),
            Dest = add_edge(Dest0, From, To, Lbl),
            merge1(Dest, Source, Edges, Fixups, Forced);
        {false,false} ->
            ?DP("   There are already edges from the node.~n"),
            ?DP("   lbl: ~p, out_edges: ~p~n", [Lbl,OutEdges]),
            case lists:keyfind(Lbl, 3, OutEdges) of
                false ->
                    ?DP("   There are no edges with the same label.~n"),
                    Dest = add_edge(Dest0, From, To, Lbl),
                    merge1(Dest, Source, Edges, Fixups, Forced);
                _ ->
                    ?DP("   There is at least one edge with the same label.~n"),
                    %% Add the edge, but alias all with the same label.
                    Dest = add_edge(Dest0, From, To, Lbl),
                    merge1(Dest, Source, Edges,
                           sets:add_element({alias,From,Lbl}, Fixups), Forced)
            end
    end;
merge1(Dest, _Source, [], Fixups, Forced) ->
    merge2(Dest, _Source, sets:to_list(Fixups), Forced).

merge2(Dest0, _Source, [{alias,From,Lbl}|Fixups], Forced) ->
    OutEdges = beam_digraph:out_edges(Dest0, From),
    Dest = foldl(fun({#b_var{},To,L}, Acc) when L =:= Lbl ->
                         set_status(To, aliased, Acc);
                    (_, Acc) ->
                         Acc
                 end, Dest0, OutEdges),
    merge2(Dest, _Source, Fixups, Forced);
merge2(Dest0, _Source, [], Forced) ->
    Dest = foldl(fun set_alias/2,
                 Dest0,
                 [get_alias_edges(V, Dest0) || V <- sets:to_list(Forced)]),
    ?assert_state(Dest).

accumulate_edges(V, State, Edges0) ->
    InEdges = beam_digraph:in_edges(State, V),
    OutEdges = beam_digraph:out_edges(State, V),
    foldl(fun sets:add_element/2,
          foldl(fun sets:add_element/2, Edges0, InEdges),
          OutEdges).

-spec new() -> sharing_state().
new() ->
    beam_digraph:new().

-spec phi(beam_ssa:b_var(), [beam_ssa:b_var()],
          sharing_state(), non_neg_integer())
         -> sharing_state().
phi(Dst, Args, State0, Cnt) ->
    ?assert_state(State0),
    ?DP("** phi **~n~s~n", [dump(State0)]),
    ?DP("  dst: ~p~n", [Dst]),
    ?DP("  args: ~p~n", [Args]),
    Structure = foldl(fun(Arg, Acc) ->
                              merge_in_arg(Arg, Acc, ?ARGS_DEPTH_LIMIT, State0)
                      end, no_info, Args),
    ?DP("  structure: ~p~n", [Structure]),
    new([Dst], [Structure], Cnt, State0).

%%%
%%% Throws `too_deep` if the depth of sharing state value chains
%%% exceeds SS_DEPTH_LIMIT.
%%%
-spec prune(sets:set(beam_ssa:b_var()),
            sets:set(beam_ssa:b_var()),
            sharing_state()) -> sharing_state().
prune(LiveVars, Killed, State) ->
    ?assert_state(State),
    ?DP("** pruning **~n~s~n", [dump(State)]),
    ?DP("pruning to: ~p~n", [sets:to_list(LiveVars)]),
    ?DP("killed: ~p~n", [sets:to_list(Killed)]),
    case sets:is_empty(LiveVars) of
        false ->
            Work = [{?SS_DEPTH_LIMIT,K} || K <- sets:to_list(Killed)],
            R = prune(Work, Killed, LiveVars, State),
            ?DP("Result:~n~s~n", [dump(R)]),
            ?assert_state(R);
        true ->
            R = new(),
            ?DP("Result (nothing killed):~n~s~n", [dump(R)]),
            R
        end.

prune([{0,_}|_], _, _, _) ->
    throw(too_deep);
prune([{Depth,V}|Work], Killed, LiveVars, State0) ->
    case is_safe_to_prune(V, LiveVars, State0) of
        true ->
            State = beam_digraph:del_vertex(State0, V),
            Ins = [{Depth - 1, I}
                   || I <- beam_digraph:in_neighbours(State0, V)],
            prune(Ins++Work, Killed, LiveVars, State);
        false ->
            prune(Work, Killed, LiveVars, State0)
    end;
prune([], _Killed, _LiveVars, State) ->
    State.

is_safe_to_prune(V, LiveVars, State) ->
    case sets:is_element(V, LiveVars) of
        true ->
            false;
        false ->
            %% Safe to prune if all out-neighbours are safe-to-prune.
            case beam_digraph:out_neighbours(State, V) of
                [] ->
                    true;
                Outs ->
                    lists:all(fun(X) ->
                                      is_safe_to_prune(X, LiveVars, State)
                              end, Outs)
            end
    end.

%%%
%%% As prune/3, but doing the pruning by rebuilding the surviving
%%% state from scratch.
%%%
%%% Throws `too_deep` if the depth of sharing state value chains
%%% exceeds SS_DEPTH_LIMIT.
%%%
-spec prune_by_add(sets:set(beam_ssa:b_var()), sharing_state())
                  -> sharing_state().
prune_by_add(LiveVars, State) ->
    ?assert_state(State),
    ?DP("Pruning to ~p~n", [sets:to_list(LiveVars)]),
    ?DP("~s~n", [dump(State)]),
    ?DP("Vertices: ~p~n", [beam_digraph:vertices(State)]),
    R = prune_by_add([{0,V} || V <- sets:to_list(LiveVars),
                               beam_digraph:has_vertex(State, V)],
                     [], new(), State),
    ?DP("Pruned result~n~s~n", [dump(R)]),
    ?assert_state(R).

prune_by_add([{Depth0,V}|Wanted], Edges, New0, Old) ->
    ?DP("Looking at ~p~n", [V]),
    ?DP("Curr:~n~s~n", [dump(New0)]),
    ?DP("Wanted: ~p~n", [Wanted]),
    case beam_digraph:has_vertex(New0, V) of
        true ->
            %% This variable is already added.
            prune_by_add(Wanted, Edges, New0, Old);
        false when Depth0 < ?SS_DEPTH_LIMIT ->
            %% This variable has to be kept. Add it to the new graph.
            New = add_vertex(New0, V, beam_digraph:vertex(Old, V)),
            %% Add all incoming edges to this node.
            InEdges = beam_digraph:in_edges(Old, V),
            Depth = Depth0 + 1,
            InNodes = [{Depth, From} || {From,_,_} <:- InEdges],
            prune_by_add(InNodes ++ Wanted, InEdges ++ Edges, New, Old);
        false ->
            %% We're in too deep, give up. This case will probably
            %% never be hit as it would require a previous prune/3
            %% application which doesn't hit the depth limit and for
            %% it to remove more than half of the nodes to trigger the
            %% use of prune_by_add/2, and in a later iteration trigger
            %% the depth limit. As it cannot be definitely ruled out,
            %% take the hit against the coverage statistics, as the
            %% handling code in beam_ssa_alias is tested.
            throw(too_deep)
    end;
prune_by_add([], Edges, New0, _Old) ->
    foldl(fun({Src,Dst,Lbl}, New) ->
                  add_edge(New, Src, Dst, Lbl)
          end, New0, Edges).

-spec set_call_result(beam_ssa:b_var(), call_in_arg_status(),
                      sharing_state(), non_neg_integer()) ->
          {sharing_state(),non_neg_integer()}.
set_call_result(Dst, aliased, SS0, Cnt0) ->
    {set_alias([Dst], SS0), Cnt0};
set_call_result(_Dst, unique, SS0, Cnt0) ->
    {SS0, Cnt0};
set_call_result(_Dst, no_info, SS0, Cnt0) ->
    {SS0, Cnt0};
set_call_result(Dst, {unique,Elements}, SS0, Cnt0) ->
    maps:fold(fun(Idx, Element, {SS,Cnt}) ->
                      V = #b_var{name=Cnt},
                      SS1 = add_var(V, unique, SS),
                      ?ASSERT(case Idx of
                                  hd -> ok;
                                  tl -> ok;
                                  E when is_integer(E), E >= 0 -> ok
                              end),
                      SS2 = beam_digraph:add_edge(SS1, V, Dst, {embed,Idx}),
                      set_call_result(V, Element, SS2, Cnt+1)
              end, {SS0,Cnt0}, Elements).

-spec set_status(beam_ssa:b_var(), sharing_status(), sharing_state()) ->
          sharing_state().
set_status(#b_var{}=V, Status, State0) ->
    %% Aliasing a variable does the following:
    %%
    %% * All extracted elements become aliased (out-edges).
    %%
    %% * If the variable contains embeddings of other variables
    %%   (in-edge label is 'embed' or {embed,_}), the embedded
    %%   variables become aliased.
    %%
    %% * If the variable is extracted from another variable (it has
    %%   in-edges matching {extract,_}), the aliasing does not change
    %%   the status of the source variables.
    %%
    %% * If the variable is embedded in a term (out-edge label is
    %%   'embed' or {embed,_}), the status of the variable in which it
    %%   is embedded remains unchanged.

    ?DP("Setting status of ~p to ~p~n", [V,Status]),
    case beam_digraph:vertex(State0, V, unique) of
        Status ->
            %% Status is unchanged.
            State0;
        unique when Status =:= aliased ->
            State = add_vertex(State0, V, Status),
            set_alias(get_alias_edges(V, State), State);
        no_info when Status =/= no_info ->
            add_vertex(State0, V, Status);
        unique when Status =:= no_info ->
            %% Can only be used safely for newly created variables.
            add_vertex(State0, V, Status)
    end.

set_alias([#b_var{}=V|Vars], State0) ->
    %% TODO: fold into the above
    case beam_digraph:vertex(State0, V, unique) of
        aliased ->
            set_alias(Vars, State0);
        _ ->
            State = add_vertex(State0, V, aliased),
            set_alias(get_alias_edges(V, State) ++ Vars, State)
    end;
set_alias([], State) ->
    State.

get_alias_edges(V, State) ->
    OutEdges = [To
                || {#b_var{},To,Kind} <- beam_digraph:out_edges(State, V),
                   case Kind of
                       {embed,_} -> false;
                       _ -> true
                   end],
    EmbedEdges = [Src
                  || {#b_var{}=Src,_,Lbl} <- beam_digraph:in_edges(State, V),
                     case Lbl of
                         embed -> true;
                         {embed,_} -> true;
                         _ -> false
                     end],
    EmbedEdges ++ OutEdges.

-spec size(sharing_state()) -> non_neg_integer().
size(State) ->
    beam_digraph:no_vertices(State).

-spec variables(sharing_state()) -> [beam_ssa:b_var()].
variables(State) ->
    [V || {V,_Lbl} <:- beam_digraph:vertices(State)].

-type call_in_arg_status() :: no_info
                            | unique
                            | aliased
                            | {unique, term()}.
-type call_in_arg_info() :: [call_in_arg_status()].
-spec initialize_in_args(['unique' | 'aliased']) -> call_in_arg_info().
initialize_in_args(ArgInfo) ->
    ArgInfo.

-spec meet_in_args([call_in_arg_status()]) -> call_in_arg_status().
meet_in_args(Args) ->
    ?DP("meet_in_args(~p)~n", [Args]),
    case Args of
        [Arg] ->
            Arg;
        [Arg|Rest] ->
            meet_in_args(Arg, Rest)
    end.

meet_in_args(aliased, _) ->
    aliased;
meet_in_args(_Status, [aliased|_Rest]) ->
    aliased;
meet_in_args(unique, [unique|Rest]) ->
    meet_in_args(unique, Rest);
meet_in_args({unique,A}, [{unique,B}|Rest]) ->
    meet_in_args({unique,meet_in_args_elems(A, B)}, Rest);
meet_in_args({unique,_}=A, [unique|Rest]) ->
    meet_in_args(A, Rest);
meet_in_args(unique, [{unique,_}=U|Rest]) ->
    meet_in_args(U, Rest);
meet_in_args(no_info, [Status|Rest]) ->
    meet_in_args(Status, Rest);
meet_in_args(Status, [no_info|Rest]) ->
    meet_in_args(Status, Rest);
meet_in_args(Status, []) ->
    ?DP("meet_in_args(...)->~p.~n", [Status]),
    Status.

meet_in_args_elems(A, B) ->
    SizeA = map_size(A),
    SizeB = map_size(B),
    if SizeA < SizeB ->
            meet_in_args_elems1(A, B);
       true ->
            meet_in_args_elems1(B, A)
    end.

meet_in_args_elems1(Small, Large) ->
    meet_in_args_elems1(maps:to_list(Small), Large, #{}).

meet_in_args_elems1([{E,Status0}|Elems], Large0, Result0) ->
    {Status,Large} = case Large0 of
                         #{E:=OtherStatus} ->
                             {meet_in_args(Status0, [OtherStatus]),
                              maps:remove(E,Large0)};
                         #{} ->
                             {Status0,Large0}
                     end,
    Result = Result0#{E=>Status},
    meet_in_args_elems1(Elems, Large, Result);
meet_in_args_elems1([], Large, Result) ->
    maps:merge(Large, Result).

-spec merge_in_args([beam_ssa:b_var()], call_in_arg_info(), sharing_state())
                   -> call_in_arg_info().
merge_in_args([Arg|Args], [ArgStatus|Status], State) ->
    ?DP("  merge_in_arg: ~p~n  current: ~p~n  SS:~n~s.~n",
        [Arg, ArgStatus, dump(State)]),
    Info = merge_in_arg(Arg, ArgStatus, ?ARGS_DEPTH_LIMIT, State),
    [Info|merge_in_args(Args, Status, State)];
merge_in_args([], [], _State) ->
    [].

merge_in_arg(_, aliased, _, _State) ->
    aliased;
merge_in_arg(plain, _, _, _State) ->
    unique;
merge_in_arg(#b_var{}=V, Status, 0, State) ->
    %% We will not traverse this argument further, this means that no
    %% element-level aliasing info will be kept for this element.
    case {Status, get_status(V, State)} of
        {S,S} ->
            S;
        {no_info,S} ->
            S;
        {S,no_info} ->
            S;
        {_,aliased} ->
            aliased
    end;
merge_in_arg(#b_var{}=V, unique, _Cutoff, State) ->
    case beam_digraph:vertex(State, V, unique) of
        no_info ->
            unique;
        S ->
            S
    end;
merge_in_arg(#b_var{}=V, Status, Cutoff, State) ->
    case beam_digraph:vertex(State, V, unique) of
        aliased ->
            aliased;
        unique ->
            InEdges = beam_digraph:in_edges(State, V),
            Elements = case Status of
                           {unique,Es} -> Es;
                           no_info -> #{}
                       end,
            merge_elements(InEdges, Elements, Cutoff, State);
        no_info ->
            Status
    end;
merge_in_arg(#b_literal{}, Status, 0, _State) ->
    %% We have reached the cutoff while traversing a larger construct,
    %% as we're not looking deeper down into the structure we stop
    %% constructing detailed information.
    Status;
merge_in_arg(#b_literal{val=[Hd|Tl]}, Status, Cutoff, State) ->
    {HdS,TlS,Elements0} = case Status of
                              {unique,#{hd:=HdS0,tl:=TlS0}=All} ->
                                  {HdS0,TlS0,All};
                              {unique,TupleElems} ->
                                  {no_info,no_info,TupleElems};
                              unique -> {unique,unique,#{}};
                              no_info -> {no_info,no_info,#{}}
                          end,
    Elements =
        Elements0#{hd=>merge_in_arg(#b_literal{val=Hd}, HdS, Cutoff-1, State),
                   tl=>merge_in_arg(#b_literal{val=Tl}, TlS, Cutoff-1, State)},
    {unique,Elements};
merge_in_arg(#b_literal{val=[]}, Status, _, _State) ->
    Status;
merge_in_arg(#b_literal{val=T}, unique, _Cutoff, _State) when is_tuple(T) ->
    %% The uniqe status cannot be safely upgraded to a more detailed
    %% info.
    unique;
merge_in_arg(#b_literal{val=T}, Status, Cutoff, State) when is_tuple(T) ->
    SrcElements = tuple_to_list(T),
    OrigElements = case Status of
                       {unique,TupleElems} ->
                           TupleElems;
                       no_info -> #{}
                   end,
    Elements = merge_tuple_elems(SrcElements, OrigElements, Cutoff, State),
    {unique,Elements};
merge_in_arg(#b_literal{val=_Lit}, {unique,_}, _Cutoff, _State) ->
    ?ASSERT(true = (is_atom(_Lit) orelse is_number(_Lit) orelse is_map(_Lit)
                    orelse is_bitstring(_Lit) orelse is_function(_Lit))),
    unique;
merge_in_arg(#b_literal{val=_Lit}, no_info, _, _State) ->
    ?ASSERT(true = (is_atom(_Lit) orelse is_number(_Lit) orelse is_map(_Lit)
                    orelse is_bitstring(_Lit) orelse is_function(_Lit))),
    unique;
merge_in_arg(#b_literal{val=_Lit}, unique, _, _State) ->
    ?ASSERT(true = (is_atom(_Lit) orelse is_number(_Lit) orelse is_map(_Lit)
                    orelse is_bitstring(_Lit) orelse is_function(_Lit))),
    unique.

merge_tuple_elems(SrcElements, OrigElements, Cutoff, State) ->
    merge_tuple_elems(SrcElements, 0, OrigElements, Cutoff-1, State).

merge_tuple_elems([S|SrcElements], Idx, OrigElements0, Cutoff, State) ->
    case OrigElements0 of
        #{Idx:=Status} ->
            E = merge_in_arg(#b_literal{val=S}, Status, Cutoff, State),
            OrigElements = OrigElements0#{Idx=>E},
            merge_tuple_elems(SrcElements, Idx+1, OrigElements, Cutoff, State);
        #{} ->
            E = merge_in_arg(#b_literal{val=S}, no_info, Cutoff, State),
            OrigElements = OrigElements0#{Idx=>E},
            merge_tuple_elems(SrcElements, Idx+1, OrigElements, Cutoff, State)
    end;
merge_tuple_elems([], _, Elements, _, _) ->
    Elements.

merge_elements([], Elements, _, _State) when map_size(Elements) =:= 0 ->
    unique;
merge_elements([], Elements, _, _State) when is_map(Elements) ->
    {unique,Elements};
merge_elements([], no_info, _, _State) ->
    %% We're lacking sub-element info, but all embedded elements were
    %% unique.
    unique;
merge_elements([{Src,_,{embed,Idx}}|Rest], Elements0, Cutoff, State) when
      is_map(Elements0) ->
    Old = maps:get(Idx, Elements0, no_info),
    New = merge_in_arg(Src, Old, Cutoff-1, State),
    Elements = Elements0#{Idx=>New},
    merge_elements(Rest, Elements, Cutoff, State);
merge_elements([{_Src,_,embed}|Rest], _Elements0, Cutoff, State) ->
    %% We don't know where this element is embedded.  Src will always
    %% be unique as otherwise merge_in_arg/4 will not bother merging
    %% the in-edges.
    ?ASSERT(true = get_status(_Src, State) =:= unique
            orelse get_status(_Src, State) =:= no_info),
    merge_elements(Rest, no_info, Cutoff, State);
merge_elements([{Src,V,{extract,E}}], Elements, Cutoff, State) ->
    ?DP("Looking for an embedding of the ~p element from ~p~n", [E, Src]),
    InEdges = beam_digraph:in_edges(State, Src),
    case [Other || {Other,_,{embed,EdgeOp}} <- InEdges, EdgeOp =:= E] of
        [Next] ->
            ?DP("Found: ~p~n", [Next]),
            merge_in_arg(Next, {unique,Elements}, Cutoff, State);
        [] ->
            get_status(V, State)
    end.

-spec new([beam_ssa:b_var()], call_in_arg_info(), non_neg_integer()) ->
          sharing_state().
new(Args, ArgsInfo, Cnt) ->
    ?DP("new args: ~p, args-info: ~p, cnt: ~p~n", [Args, ArgsInfo, Cnt]),
    R = {SS,_} = new(Args, ArgsInfo, Cnt, new()),
    ?assert_state(SS),
    R.

new([A|As], [S|Stats], Cnt, SS)
  when S =:= aliased; S =:= unique; S =:= no_info ->
    new(As, Stats, Cnt, add_var(A, S, SS));
new([A|As], [{unique,Elements}|Stats], Cnt0, SS0) ->
    SS1 = add_var(A, unique, SS0),
    {SS,Cnt} = init_elements(Elements, A, Cnt0, SS1),
    new(As, Stats, Cnt, SS);
new([], [], Cnt, SS) ->
    {SS,Cnt}.

init_elements(Elements, V, Cnt0, SS0) ->
    maps:fold(fun(Idx, Status, {SS,Cnt}) ->
                      init_element(Idx, Status, V, Cnt, SS)
              end, {SS0,Cnt0}, Elements).

init_element(Idx, Status, Child, Cnt0, SS0) ->
    ElemV = #b_var{name=Cnt0},
    {SS1,Cnt} = new([ElemV], [Status], Cnt0+1, SS0),
    ?ASSERT(case Idx of
                hd -> ok;
                tl -> ok;
                I when is_integer(I), I >= 0 -> ok
            end),
    SS = beam_digraph:add_edge(SS1, ElemV, Child, {embed,Idx}),
    {SS,Cnt}.

%% Internal helpers

-spec has_out_edges(beam_ssa:b_var(), sharing_state()) -> boolean().
has_out_edges(V, State) ->
    beam_digraph:out_edges(State, V) =/= [].

%% Debug support below

-ifdef(SS_EXTRA_ASSERTS).

-spec assert_state(sharing_state()) -> sharing_state().

assert_state(State) ->
    assert_bad_edges(State),
    assert_aliased_parent_implies_aliased(State),
    assert_embedded_in_aliased_implies_aliased(State),
    assert_multiple_embeddings_force_aliasing(State),
    assert_multiple_extractions_force_aliasing(State),
    State.

%% Check that we don't have edges between non-existing nodes
assert_bad_edges(State) ->
    [{assert_variable_exists(F, State), assert_variable_exists(T, State)}
     || {F,T,_} <:- beam_digraph:edges(State)].


%% Check that extracted and embedded elements of an aliased variable
%% are aliased.
assert_aliased_parent_implies_aliased(State) ->
    [assert_apia(A, State) || {A,aliased} <- beam_digraph:vertices(State)].

assert_apia(Parent, State) ->
    Children = [Child
                || {_,Child,Info} <:- beam_digraph:out_edges(State, Parent),
                   case Info of
                       {extract,_} -> true;
                       embed -> true;
                       {embed,_} -> false
                   end],
    [case beam_digraph:vertex(State, Child, unique) of
         aliased ->
             ok;
         _ ->
             io:format("Expected ~p to be aliased as is derived from ~p.~n"
                       "state: ~s", [Child, Parent, dump(State)]),
             throw(assertion_failure)
     end || Child <- Children].

%% Check that elements which are embedded twice or more times are
%% aliased.
assert_embedded_in_aliased_implies_aliased(State) ->
    [assert_eiaia(A, State) || {A,aliased} <- beam_digraph:vertices(State)].

assert_eiaia(Embedder, State) ->
    NotAliased = [ Src
                   || {Src,_,embed} <- beam_digraph:in_edges(State, Embedder),
                      beam_digraph:vertex(State, Src, unique) =/= aliased,
                      beam_digraph:vertex(State, Src, unique) =/= no_info],
    case NotAliased of
        [] ->
            State;
        _ ->
            io:format("Expected ~p to be aliased as"
                      " they are embedded in aliased values.~n~s.~n",
                      [NotAliased, dump(State)]),
            throw(assertion_failure)
    end.

%% Check that elements which are embedded twice or more times are
%% aliased.
assert_multiple_embeddings_force_aliasing(State) ->
    [assert_mefa(V, State) || {V,unique} <- beam_digraph:vertices(State)].

assert_mefa(V, State) ->
    NotAliased = [ B || {B,_,embed} <- beam_digraph:out_edges(State, V),
                        beam_digraph:vertex(State, B, unique) =/= aliased],
    case NotAliased of
        [_,_|_] ->
            io:format("Expected ~p to be aliased in:~n~s.~n", [V, dump(State)]),
            throw(assertion_failure);
        _ ->
            State
    end.

%% Check that elements which are extracted twice are aliased.
assert_multiple_extractions_force_aliasing(State) ->
    [assert_mxfa(V, State) || {V,_} <:- beam_digraph:vertices(State)].

assert_mxfa(V, State) ->
    %% Build a map of the extracted values keyed by element.
    Extracted = foldl(
                  fun({_,Other,{extract,Elem}}, Acc)
                        when is_integer(Elem); Elem =:= hd; Elem =:= tl ->
                          Acc#{Elem=>[Other|maps:get(Elem, Acc, [])]};
                     ({_,_,embed}, Acc) ->
                          Acc;
                     ({_,_,{embed,_}}, Acc) ->
                          Acc
                  end, #{}, beam_digraph:out_edges(State, V)),
    Bad = maps:fold(
            fun(_Elem, [_,_|_]=Vars, Acc) ->
                    [X || X <- Vars,
                          beam_digraph:vertex(State, X, unique) =/= aliased]
                        ++ Acc;
               (_, _, Acc) ->
                    Acc
            end, [], Extracted),
    case Bad of
        [] ->
            State;
        _ ->
            io:format("~p should be aliased~nstate:~n~s.~n", [V, dump(State)]),
            throw(assertion_failure)
    end.

assert_variable_exists(plain, State) ->
    case beam_digraph:has_vertex(State, plain) of
        false ->
            io:format("Expected ~p in~n ~s.~n", [plain, dump(State)]),
            throw(assertion_failure);
        _ ->
            case beam_digraph:vertex(State, plain, unique) of
                unique -> State;
                Other ->
                    io:format("Expected plain to be unique, was ~p, in:~n~p~n",
                              [Other, dump(State)]),
                    throw(assertion_failure)
            end
    end;
assert_variable_exists(#b_literal{}, State) ->
    State;
assert_variable_exists(#b_var{}=V, State) ->
    case beam_digraph:has_vertex(State, V) of
        false ->
            io:format("Expected ~p in~n~s.~n", [V, dump(State)]),
            throw(assertion_failure);
        _ ->
            State
    end.

-endif.

-ifdef(PROVIDE_DUMP).
-spec dump(sharing_state()) -> iolist().
dump(State) ->
    Ls = lists:enumerate(0, beam_digraph:vertices(State)),
    V2Id = #{ V=>Id || {Id,{V,_}} <- Ls },
    [
     "digraph G {\n",
     [ io_lib:format("  ~p [label=\"~p:~p\",shape=\"box\"]~n",
                     [Id,V,beam_digraph:vertex(State, V)])
       || V:=Id <- V2Id],
     [ io_lib:format("  ~p -> ~p [label=\"~p\"]~n",
                     [maps:get(From, V2Id, plain),
                      maps:get(To, V2Id),
                      Lbl])
       || {From,To,Lbl} <- beam_digraph:edges(State)],
     "}\n"].
-endif.
