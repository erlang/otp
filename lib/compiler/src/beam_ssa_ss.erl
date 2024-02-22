%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2024. All Rights Reserved.
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
         prune/2,
         set_call_result/4,
         set_status/3,
         variables/1]).

-include("beam_ssa.hrl").
-include("beam_types.hrl").

-import(lists, [foldl/3]).

-define(ARGS_DEPTH_LIMIT, 4).

%% -define(DEBUG, true).

-ifdef(DEBUG).
-define(DP(FMT, ARGS), io:format(FMT, ARGS)).
-define(DP(FMT), io:format(FMT)).
-define(DBG(STMT), STMT).
-else.
-define(DP(FMT, ARGS), skip).
-define(DP(FMT), skip).
-define(DBG(STMT), skip).
-endif.


%% Uncomment the following to check that all invariants for the state
%% hold. These checks are expensive and not enabled by default.

%% -define(EXTRA_ASSERTS, true).

-ifdef(EXTRA_ASSERTS).
-define(assert_state(State), assert_state(State)).
-define(ASSERT(Assert), Assert).
-else.
-define(assert_state(State), State).
-define(ASSERT(Assert), skip).
-endif.

-type sharing_state() :: any(). % A beam_digraph graph.
-type sharing_status() :: 'unique' | 'aliased'.
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
    ?DP("Deriving ~p from ~p~nSS:~p~n", [Dst,Src,State]),
    ?assert_state(State),
    ?ASSERT(assert_variable_exists(Dst, State)),
    ?ASSERT(assert_variable_exists(Src, State)),
    case {beam_digraph:vertex(State, Dst),beam_digraph:vertex(State, Src)} of
        {aliased,_} ->
            %% Nothing to do, already aliased. This can happen when
            %% handling Phis, no propagation to the source should be
            %% done.
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
                    ?assert_state(add_edge(State, Src, Dst, embed))
            end
    end.

-spec embed_in(beam_ssa:b_var(), [{element(),beam_ssa:b_var()}],
               sharing_state()) -> sharing_state().
embed_in(Dst, Elements, State0) ->
    ?DP("Embedding ~p into ~p~nSS:~p~n", [Elements,Dst,State0]),
    ?assert_state(State0),
    ?ASSERT(assert_variable_exists(Dst, State0)),
    ?ASSERT([assert_variable_exists(Src, State0)
             || {#b_var{},Src} <- Elements]),
    foldl(fun({Element,Src}, Acc) ->
                  add_embedding(Dst, Src, Element, Acc)
          end, State0, Elements).

add_embedding(Dst, Src, Element, State0) ->
    ?DP("add_embedding(~p, ~p, ~p, ...)~n", [Dst,Src,Element]),

    %% Create a node for literals as it isn't in the graph.
    State1 = case Src of
                 plain ->
                     beam_digraph:add_vertex(State0, Src, unique);
                 #b_literal{} ->
                     beam_digraph:add_vertex(State0, Src, unique);
                 _ ->
                     State0
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
    ?ASSERT(assert_variable_exists(Dst, State)),
    ?ASSERT(assert_variable_exists(Src, State)),

    case beam_digraph:vertex(State, Src) of
        aliased ->
            %% The pair/tuple is aliased, so what is extracted will be aliased.
            ?assert_state(set_status(Dst, aliased, State));
        unique ->
            %% The aggregate is unique, but elements can be aliased.
            OutEdges = beam_digraph:out_edges(State, Src),
            ?ASSERT(true = is_integer(Element)
                    orelse (Element =:= hd) orelse (Element =:= tl)),
            extract_element(Dst, Src, Element, OutEdges, State)
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
    State = ?assert_state(add_edge(State0, Src, Dst, {extract,Element})),
    extract_status_for_element(Element, Src, Dst, State).

extract_status_for_element(Element, Src, Dst, State) ->
    ?DP("    extract_status_for_element(~p, ~p)~n", [Element, Src]),
    InEdges = beam_digraph:in_edges(State, Src),
    extract_status_for_element(InEdges, Element, Src, Dst, State).

extract_status_for_element([{N,_,{embed,Element}}|_InEdges],
                           Element, _Src, Dst, State0) ->
    ?DP("    found new source ~p~n", [N]),
    ?DP("    SS ~p~n", [State0]),
    ?DP("    status ~p~n", [beam_digraph:vertex(State0, N)]),
    State = set_status(Dst, beam_digraph:vertex(State0, N), State0),
    ?DP("    Returned SS ~p~n", [State]),
    ?assert_state(State);
extract_status_for_element([{N,_,{extract,SrcElement}}|InEdges],
                           Element, Src, Dst, State0) ->
    ?DP("    found source: ~p[~p]~n", [N,SrcElement]),
    Origs = [Var || {Var,_,{embed,SE}} <- beam_digraph:in_edges(State0, N),
                    SrcElement =:= SE],
    ?DP("    original var: ~p~n", [Origs]),
    case Origs of
        [] ->
            ?DP("    no known source~n"),
            extract_status_for_element(InEdges, Element, Src, Dst, State0);
        [Orig] ->
            extract_status_for_element(Element, Orig, Dst, State0)
    end;
extract_status_for_element([_Edge|InEdges], Element, Src, Dst, State) ->
    ?DP("    ignoring in-edge ~p~n", [_Edge]),
    extract_status_for_element(InEdges, Element, Src, Dst, State);
extract_status_for_element([], _Element, _Src, Dst, State) ->
    %% Nothing found, the status will be aliased.
    ?DP("    status of ~p will be aliased~n", [Dst]),
    ?assert_state(set_status(Dst, aliased, State)).

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
    beam_digraph:vertex(State, V).

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
    ?DBG(dump(Small)),
    ?DP("Large:~n"),
    ?DBG(dump(Large)),
    R = merge(Large, Small, beam_digraph:vertices(Small),
              sets:new([{version,2}]), sets:new([{version,2}])),
    ?assert_state(R).

merge(Dest, Source, [{V,VStatus}|Vertices], Edges0, Forced) ->

    Edges = accumulate_edges(V, Source, Edges0),
    DestStatus = case beam_digraph:has_vertex(Dest, V) of
                     true ->
                         beam_digraph:vertex(Dest, V);
                     false ->
                         false
                 end,
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
            merge(Dest, Source, Vertices, Edges, sets:add_element(V, Forced))
    end;
merge(Dest0, _Source, [], Edges, Forced) ->
    merge1(Dest0, _Source, sets:to_list(Edges),
           sets:new([{version,2}]), Forced).

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

-spec prune(sets:set(beam_ssa:b_var()), sharing_state()) -> sharing_state().
prune(LiveVars, State) ->
    ?assert_state(State),
    ?DP("Pruning to ~p~n", [sets:to_list(LiveVars)]),
    ?DBG(dump(State)),
    R = prune(sets:to_list(LiveVars), [], new(), State),
    ?DP("Pruned result~n"),
    ?DBG(dump(R)),
    ?assert_state(R).

prune([V|Wanted], Edges, New0, Old) ->
    case beam_digraph:has_vertex(New0, V) of
        true ->
            %% This variable is alread added.
            prune(Wanted, Edges, New0, Old);
        false ->
            %% This variable has to be kept. Add it to the new graph.
            New = add_vertex(New0, V, beam_digraph:vertex(Old, V)),
            %% Add all incoming edges to this node.
            InEdges = beam_digraph:in_edges(Old, V),
            InNodes = [From || {From,_,_} <- InEdges],
            prune(InNodes ++ Wanted, InEdges ++ Edges, New, Old)
    end;
prune([], Edges, New0, _Old) ->
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
    ?ASSERT(assert_variable_exists(V, State0)),
    case beam_digraph:vertex(State0, V) of
        Status ->
            %% Status is unchanged.
            State0;
        unique when Status =:= aliased ->
            State = add_vertex(State0, V, Status),
            set_alias(get_alias_edges(V, State), State)
    end.

set_alias([#b_var{}=V|Vars], State0) ->
    %% TODO: fold into the above
    case beam_digraph:vertex(State0, V) of
        aliased ->
            set_alias(Vars, State0);
        _ ->
            State = add_vertex(State0, V, aliased),
            set_alias(get_alias_edges(V, State) ++ Vars, State)
    end;
set_alias([], State) ->
    State.

get_alias_edges(V, State) ->
    OutEdges = [To || {#b_var{},To,_} <- beam_digraph:out_edges(State, V)],
    EmbedEdges = [Src
                  || {#b_var{}=Src,_,Lbl} <- beam_digraph:in_edges(State, V),
                     case Lbl of
                         embed -> true;
                         {embed,_} -> true;
                         _ -> false
                     end],
    EmbedEdges ++ OutEdges.

-spec variables(sharing_state()) -> [beam_ssa:b_var()].
variables(State) ->
    %% TODO: Sink this beam_digraph to avoid splitting the list?
    [V || {V,_Lbl} <- beam_digraph:vertices(State)].

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
    ?DP("  merge_in_arg: ~p~n  current: ~p~n  SS: ~p.~n",
        [Arg,ArgStatus,State]),
    Info = merge_in_arg(Arg, ArgStatus, ?ARGS_DEPTH_LIMIT, State),
    [Info|merge_in_args(Args, Status, State)];
merge_in_args([], [], _State) ->
    [].

merge_in_arg(_, aliased, _, _State) ->
    aliased;
merge_in_arg(plain, _, _, _State) ->
    unique;
merge_in_arg(#b_var{}=V, _Status, 0, State) ->
    %% We will not traverse this argument further, this means that no
    %% element-level aliasing info will be kept for this element.
    get_status(V, State);
merge_in_arg(#b_var{}=V, Status, Cutoff, State) ->
    case beam_digraph:vertex(State, V) of
        aliased ->
            aliased;
        unique ->
            InEdges = beam_digraph:in_edges(State, V),
            Elements = case Status of
                           unique -> #{};
                           {unique,Es} -> Es;
                           no_info -> #{}
                       end,
            merge_elements(InEdges, Elements, Cutoff, State)
    end;
merge_in_arg(#b_literal{}, _, 0, _State) ->
    %% We have reached the cutoff while traversing a larger construct,
    %% as we're not looking deeper down into the structure we indicate
    %% that we have no information.
    no_info;
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
merge_in_arg(#b_literal{val=T}, Status, Cutoff, State) when is_tuple(T) ->
    SrcElements = tuple_to_list(T),
    OrigElements = case Status of
                       {unique,TupleElems} ->
                           TupleElems;
                       unique -> #{};
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
    %% be unique as otherwise erge_in_arg/4 will not bother merging
    %% the in-edges.
    ?ASSERT(unique = get_status(_Src, State)),
    merge_elements(Rest, no_info, Cutoff, State);
merge_elements([{_,V,{extract,_}}|_Rest], _Elements0, _, State) ->
    %% For now we don't try to derive the structure of this argument
    %% further.
    %% TODO: Revisit the decision above.
    get_status(V, State).

-spec new([beam_ssa:b_var()], call_in_arg_info(), non_neg_integer()) ->
          sharing_state().
new(Args, ArgsInfo, Cnt) ->
    ?DP("new args: ~p, args-info: ~p, cnt: ~p~n", [Args, ArgsInfo, Cnt]),
    R = {SS,_} = new(Args, ArgsInfo, Cnt, new()),
    ?assert_state(SS),
    R.

new([A|As], [S0|Stats], Cnt, SS)
  when S0 =:= aliased; S0 =:= unique; S0 =:= no_info ->
    S = case S0 of
            no_info -> unique;
            _ -> S0
        end,
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

-ifdef(EXTRA_ASSERTS).

-spec assert_state(sharing_state()) -> sharing_state().

assert_state(State) ->
    assert_aliased_parent_implies_aliased(State),
    assert_embedded_in_aliased_implies_aliased(State),
    assert_multiple_embeddings_force_aliasing(State),
    assert_multiple_extractions_force_aliasing(State),
    State.

%% Check that extracted and embedded elements of an aliased variable
%% are aliased.
assert_aliased_parent_implies_aliased(State) ->
    [assert_apia(A, State) || {A,aliased} <- beam_digraph:vertices(State)].

assert_apia(Parent, State) ->
    Children = [Child
                || {_,Child,Info} <- beam_digraph:out_edges(State, Parent),
                   case Info of
                       {extract,_} -> true;
                       embed -> true;
                       {embed,_} -> false
                   end],
    [case beam_digraph:vertex(State, Child) of
         aliased ->
             ok;
         _ ->
             io:format("Expected ~p to be aliased as is derived from ~p.~n"
                       "state: ~p", [Child, Parent, State]),
             throw(assertion_failure)
     end || Child <- Children].

%% Check that elements which are embedded twice or more times are
%% aliased.
assert_embedded_in_aliased_implies_aliased(State) ->
    [assert_eiaia(A, State) || {A,aliased} <- beam_digraph:vertices(State)].

assert_eiaia(Embedder, State) ->
    NotAliased = [ Src
                   || {Src,_,embed} <- beam_digraph:in_edges(State, Embedder),
                      beam_digraph:vertex(State, Src) =/= aliased],
    case NotAliased of
        [] ->
            State;
        _ ->
            io:format("Expected ~p to be aliased as"
                      " they are embedded in aliased values.~n~p.~n",
                      [NotAliased, State]),
            throw(assertion_failure)
    end.

%% Check that elements which are embedded twice or more times are
%% aliased.
assert_multiple_embeddings_force_aliasing(State) ->
    [assert_mefa(V, State) || {V,unique} <- beam_digraph:vertices(State)].

assert_mefa(V, State) ->
    NotAliased = [ B || {B,_,embed} <- beam_digraph:out_edges(State, V),
                        beam_digraph:vertex(State, B) =/= aliased],
    case NotAliased of
        [_,_|_] ->
            io:format("Expected ~p in ~p to be aliased.~n", [V,State]),
            throw(assertion_failure);
        _ ->
            State
    end.

%% Check that elements which are extracted twice are aliased.
assert_multiple_extractions_force_aliasing(State) ->
    [assert_mxfa(V, State) || {V,_} <- beam_digraph:vertices(State)].

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
                    [X || X <- Vars, beam_digraph:vertex(State, X) =/= aliased]
                        ++ Acc;
               (_, _, Acc) ->
                    Acc
            end, [], Extracted),
    case Bad of
        [] ->
            State;
        _ ->
            io:format("~p should be aliased~nstate:~p.~n", [V,State]),
            throw(assertion_failure)
    end.

assert_variable_exists(plain, State) ->
    case beam_digraph:has_vertex(State, plain) of
        false ->
            io:format("Expected ~p in ~p.~n", [plain,State]),
            throw(assertion_failure);
        _ ->
            case beam_digraph:vertex(State, plain) of
                unique -> State;
                Other ->
                    io:format("Expected plain in ~p to be unique,"
                              " was ~p.~n", [State,Other]),
                    throw(assertion_failure)
            end
    end;
assert_variable_exists(#b_literal{}, State) ->
    State;
assert_variable_exists(#b_var{}=V, State) ->
    case beam_digraph:has_vertex(State, V) of
        false ->
            io:format("Expected ~p in ~p.~n", [V,State]),
            throw(assertion_failure);
        _ ->
            State
    end.

-endif.
-ifdef(DEBUG).

dump(State) ->
    io:format("~p~n", [State]).

-endif.
