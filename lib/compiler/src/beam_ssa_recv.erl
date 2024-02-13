%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2024. All Rights Reserved.
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

-module(beam_ssa_recv).
-moduledoc false.
-export([format_error/1, module/2]).

%%%
%%% In code such as:
%%%
%%%    Ref = make_ref(),        %Or erlang:monitor(process, Pid)
%%%      .
%%%      .
%%%      .
%%%    receive
%%%       {Ref,Reply} -> Reply
%%%    end.
%%%
%%% we know that none of the messages that exist in the message queue
%%% before the call to make_ref/0 can be matched out in the receive
%%% statement. Therefore we can avoid going through the entire message
%%% queue if we introduce four new instructions (here written as BIFs
%%% in pseudo-Erlang):
%%%
%%%    OpaqueMarker = 'reserve_marker'(),
%%%    Ref = make_ref(),
%%%    'bind_marker'(OpaqueMarker, Ref),
%%%      .
%%%      .
%%%      .
%%%    'use_marker'(Ref),
%%%    receive
%%%       {Ref,Reply} ->
%%%           'clear_marker'(Ref),
%%%           Reply
%%%    end.
%%%
%%% The reserve_marker/0 instruction returns an opaque term representing the
%%% current message queue position (this is never visible to the end user).
%%%
%%% To use a marker it must first be bound to a term using bind_marker/2. This
%%% essentially renames the marker and all subsequent operations will use the
%%% bound term instead.
%%%
%%% Having separate "reserve" and "bind" steps may seem verbose but is
%%% necessary to avoid races in monitor/2 and friends: if the marker is not
%%% created before the call, we may get a 'DOWN' message after monitor/2
%%% returns but before the marker is created.
%%%
%%% The use_marker/1 instruction is used before suitable receive loops to start
%%% the search at the previously saved position.
%%%
%%% When the marker is no longer needed we can use clear_marker/1 to remove
%%% it. This is not strictly required as the number of active markers are
%%% limited and they may be overwritten as new ones are created, but being a
%%% good citizen reduces the risk of overwriting a marker that would otherwise
%%% have been used later on.
%%%
%%% It's important to note that use_marker/1 and clear_marker/1 do nothing when
%%% given anything other than a currently active marker. This lets us apply the
%%% optimization even when a given receive _doesn't always_ have a suitable
%%% reference to work with, for example:
%%%
%%%    first_function(Pid, Foo) ->
%%%        OpaqueMarker = 'reserve_marker'(),
%%%        Ref = make_ref(),
%%%        'bind_marker'(OpaqueMarker, Ref),
%%%        Pid ! {self(), Ref, Foo},
%%%        await_result(Ref).
%%%
%%%    second_function(Pid, Foo) ->
%%%        {Bar, Tag} = foo:bar(Foo),
%%%        Pid ! {self(), Tag, Bar},
%%%        await_result(Tag).
%%%
%%%    await_result(Tag) ->
%%%        'use_marker'(Tag),
%%%        receive
%%%            {Tag, Result} -> Result
%%%        end.
%%%
%%% Because use_marker/1 ignores terms that haven't been bound to a marker, the
%%% optimization is applied when await_result/1 is reached through
%%% first_function/2, but not second_function/2. If these instructions required
%%% markers (active or not) we would have to disable the optimization in cases
%%% like this.
%%%

-include("beam_ssa.hrl").

-import(lists, [foldl/3, member/2, search/2]).

%% Psuedo-block for representing function returns in the block graph. Body
%% calls add an edge returning _from_ this block.
-define(RETURN_BLOCK, -1).
-define(ENTRY_BLOCK, 0).

-spec format_error(term()) -> nonempty_string().

format_error(OptInfo) ->
    format_opt_info(OptInfo).

-record(scan, { graph=beam_digraph:new(),
                module :: #{ beam_ssa:b_local() => {beam_ssa:block_map(),
                                                    [beam_ssa:b_var()],
                                                    [beam_ssa:b_var()]} },
                recv_candidates=#{},
                ref_candidates=#{} }).

-spec module(Module, Options) -> Result when
      Module :: beam_ssa:b_module(),
      Options :: [compile:option()],
      Result :: {ok, beam_ssa:b_module(), list()}.

module(#b_module{}=Mod0, Opts) ->
    case scan(Mod0) of
        #scan{}=Scan ->
            %% Figure out where to place marker creation, usage, and clearing
            %% by walking through the module-wide graph.
            {Markers, Uses, Clears} = plan(Scan),

            %% If we have any markers we must have uses and vice versa. If
            %% there any any clears, we must have markers.
            true = (Markers =/= #{}) =:= (Uses =/= #{}), %Assertion.
            true = (Clears =:= #{}) orelse (Markers =/= #{}), %Assertion.

            Mod = optimize(Mod0, Markers, Uses, Clears),

            Ws = case proplists:get_bool(recv_opt_info, Opts) of
                     true -> collect_opt_info(Mod);
                     false -> []
                 end,

            {ok, Mod, Ws};
        none ->
            %% No peek_message instructions; just skip it all.
            {ok, Mod0, []}
    end.

scan(#b_module{body=Fs}) ->
    %% Quickly collect all peek_message instructions in this module,
    %% allowing us to avoid the expensive building of the module-wide
    %% graph of all blocks if there are no receives in this module.
    case scan_peek_message(Fs) of
        [_|_]=Rs0 ->
            Rs = maps:from_list(Rs0),
            ModMap = foldl(fun(#b_function{bs=Blocks,args=Args}=F, Acc) ->
                                   FuncId = get_func_id(F),
                                   Rets = scan_rets(Blocks),
                                   Acc#{ FuncId => {Blocks, Args, Rets} }
                           end, #{}, Fs),
            foldl(fun(F, Scan0) ->
                          FuncId = get_func_id(F),

                          Scan = scan_add_vertex({FuncId, ?ENTRY_BLOCK}, Scan0),
                          scan_function(FuncId, F, Scan)
                  end,
                  #scan{ module = ModMap, recv_candidates = Rs }, Fs);
        [] ->
            none
    end.

scan_peek_message([#b_function{bs=Bs}=F | Fs]) ->
    case scan_peek_message_bs(maps:to_list(Bs)) of
        [] ->
            scan_peek_message(Fs);
        [_ | _] = Rs ->
            FuncId = get_func_id(F),
            [{FuncId, Rs} | scan_peek_message(Fs)]
    end;
scan_peek_message([]) ->
    [].

scan_rets(Blocks) ->
    Rets = maps:fold(fun(_K, #b_blk{last=#b_ret{arg=#b_var{}=RetVal}}, Acc) ->
                             gb_sets:add_element(RetVal, Acc);
                        (_K, _V, Acc) ->
                             Acc
                     end, gb_sets:new(), Blocks),
    gb_sets:to_list(Rets).

scan_peek_message_bs([{Lbl, Blk} | Bs]) ->
    case Blk of
        #b_blk{is=[#b_set{op=peek_message}=I | _]} ->
            [{Lbl, I} | scan_peek_message_bs(Bs)];
        #b_blk{} ->
            scan_peek_message_bs(Bs)
    end;
scan_peek_message_bs([]) ->
    [].

get_func_id(#b_function{anno=Anno}) ->
    {_,Name,Arity} = maps:get(func_info, Anno),
    #b_local{name=#b_literal{val=Name},arity=Arity}.

scan_function(FuncId, #b_function{bs=Blocks}, State) ->
    scan_bs(beam_ssa:rpo(Blocks), Blocks, FuncId, State).

scan_bs([Lbl | Lbls], Blocks, FuncId, State0) ->
    #b_blk{is=Is} = Blk = map_get(Lbl, Blocks),
    State = scan_is(Is, Blk, Lbl, Blocks, FuncId, State0),
    scan_bs(Lbls, Blocks, FuncId, State);
scan_bs([], _Blocks, _FuncId, State) ->
    State.

scan_is([#b_set{op={succeeded,body}}], Blk, Lbl, _Blocks, FuncId, State) ->
    #b_br{bool=#b_var{},succ=Succ} = Blk#b_blk.last, %Assertion.

    %% Orphaned markers get recycled very quickly so there's little harm in
    %% ignoring exceptions. Clearing specific markers on exceptions requires us
    %% to add try/catch everywhere to clear markers before rethrowing, and that
    %% isn't worth the bother.
    scan_add_edge({FuncId, Lbl}, {FuncId, Succ}, State);
scan_is([#b_set{op=new_try_tag,dst=Dst}], Blk, Lbl, _Blocks, FuncId, State) ->
    %% This never throws but the failure label points at a landingpad, so we'll
    %% ignore that branch.
    #b_br{bool=Dst,succ=Succ} = Blk#b_blk.last, %Assertion.
    scan_add_edge({FuncId, Lbl}, {FuncId, Succ}, State);
scan_is([#b_set{op=call,dst=Dst,args=[#b_remote{} | _]}=Call,
         #b_set{op={succeeded,body}}],
        #b_blk{last=#b_br{succ=Succ}}, Lbl, Blocks, FuncId, State0) ->
    case Blocks of
        #{ Succ := #b_blk{is=[],last=#b_ret{arg=Dst}}} ->
            %% Tail call, skip adding a successor edge to prevent
            %% clearing the marker needlessly.
            si_remote_call(Call, Lbl, Succ, Blocks, FuncId, State0);
        #{} ->
            State = si_remote_call(Call, Lbl, Succ, Blocks, FuncId, State0),
            scan_add_edge({FuncId, Lbl}, {FuncId, Succ}, State)
    end;
scan_is([#b_set{op=call,args=[#b_remote{} | _]}=Call | Is],
        Blk, Lbl, Blocks, FuncId, State0) ->
    %% Remote call that always succeeds.
    State = si_remote_call(Call, Lbl, Lbl, Blocks, FuncId, State0),
    scan_is(Is, Blk, Lbl, Blocks, FuncId, State);
scan_is([#b_set{op=call,dst=Dst,args=[#b_local{} | _]}=Call,
         #b_set{op={succeeded,body},args=[Dst]}],
        #b_blk{last=#b_br{succ=Succ}},
        Lbl, Blocks, FuncId, State0) ->
    case Blocks of
        #{ Succ := #b_blk{is=[],last=#b_ret{arg=Dst}}} ->
            %% Tail call, skip adding a successor edge to prevent
            %% clearing the marker needlessly.
            scan_add_call(Call, Lbl, Succ, FuncId, State0);
        #{} ->
            State = scan_add_call(Call, Lbl, Succ, FuncId, State0),
            scan_add_edge({FuncId, Lbl}, {FuncId, Succ}, State)
    end;
scan_is([_I | Is], Blk, Lbl, Blocks, FuncId, State) ->
    scan_is(Is, Blk, Lbl, Blocks, FuncId, State);
scan_is([], #b_blk{last=#b_ret{}}, Lbl, _Blocks, FuncId, State) ->
    scan_add_edge({FuncId, Lbl}, {FuncId, ?RETURN_BLOCK}, State);
scan_is([], Blk, Lbl, _Blocks, FuncId, State) ->
    foldl(fun(Succ, Acc) ->
                  scan_add_edge({FuncId, Lbl}, {FuncId, Succ}, Acc)
          end, State, beam_ssa:successors(Blk)).

%% Adds an edge to the callee, with argument/parameter translation to let us
%% follow specific references.
scan_add_call(Call, CallLbl, SuccLbl, Caller, #scan{module=ModMap}=State0) ->
    #b_set{dst=Dst,args=[#b_local{}=Callee | Args]} = Call,
    #{ Callee := {_Blocks, Params, Rets} } = ModMap,

    {CallTranslation, CallInverse} =
        scan_translate_call(Args, Params, #{}, #{}),
    State = scan_add_edge({Caller, CallLbl},
                          {Callee, ?ENTRY_BLOCK},
                          {CallTranslation, CallInverse, Args},
                          State0),

    {RetTranslation, RetInverse} =
        scan_translate_return(Rets, Dst, CallTranslation),
    scan_add_edge({Callee, ?RETURN_BLOCK},
                  {Caller, SuccLbl},
                  {RetTranslation, RetInverse, Params},
                  State).

scan_translate_call([Arg | Args], [Param | Params], ArgToParams, ParamToArgs) ->
    scan_translate_call(Args, Params,
                        ArgToParams#{ Arg => Param },
                        ParamToArgs#{ Param => Arg });
scan_translate_call([], [], ArgToParams, ParamToArgs) ->
    {ArgToParams, ParamToArgs}.

scan_translate_return(Rets, Dst, CallerToCallee0) ->
    CallerToCallee = CallerToCallee0#{ Dst => Rets },
    CalleeToCaller = scan_translate_return_1(Rets, Dst, #{}),
    {CalleeToCaller, CallerToCallee}.

scan_translate_return_1([Ret | Rets], Dst, CalleeToCaller) ->
    scan_translate_return_1(Rets, Dst, CalleeToCaller#{ Ret => Dst });
scan_translate_return_1([], _Dst, CalleeToCaller) ->
    CalleeToCaller.

scan_add_edge(From, To, State) ->
    scan_add_edge(From, To, branch, State).

scan_add_edge(From, To, Label, State0) ->
    State = scan_add_vertex(To, scan_add_vertex(From, State0)),

    Graph = beam_digraph:add_edge(State#scan.graph, From, To, Label),

    State#scan{graph=Graph}.

scan_add_vertex(Vertex, #scan{graph=Graph0}=State) ->
    case beam_digraph:has_vertex(Graph0, Vertex) of
        true ->
            State;
        false ->
            Graph = beam_digraph:add_vertex(Graph0, Vertex),
            State#scan{graph=Graph}
    end.

si_remote_call(#b_set{anno=Anno,dst=Dst,args=Args}=Call,
               CalledAt, ValidAfter, Blocks, FuncId, State) ->
    case si_remote_call_1(Dst, Args, ValidAfter, Blocks) of
        {makes_ref, ExtractedAt, Ref} ->
            #scan{ref_candidates=Candidates0} = State,

            MakeRefs0 = maps:get(FuncId, Candidates0, []),
            MakeRef = {Anno, CalledAt, Dst, ExtractedAt, Ref},

            Candidates = Candidates0#{ FuncId => [MakeRef | MakeRefs0] },

            State#scan{ref_candidates=Candidates};
        uses_ref ->
            #scan{recv_candidates=Candidates0} = State,

            UseRefs0 = maps:get(FuncId, Candidates0, []),
            UseRef = {CalledAt, Call},

            Candidates = Candidates0#{ FuncId => [UseRef | UseRefs0] },

            State#scan{recv_candidates=Candidates};
        no ->
            State
    end.

si_remote_call_1(Dst, [Callee | Args], Lbl, Blocks) ->
    MFA = case Callee of
              #b_remote{mod=#b_literal{val=Mod},
                        name=#b_literal{val=Func},
                        arity=Arity} ->
                  {Mod, Func, Arity};
              _ ->
                  none
          end,
    case MFA of
        {erlang,alias,A} when is_integer(A), 0 =< A, A =< 1 ->
            {makes_ref, Lbl, Dst};
        {erlang,demonitor,2} ->
            case Args of
                [_MRef, #b_literal{val=[flush]}] ->
                    %% If the monitor fired prior to this call, 'flush' will
                    %% yank out the 'DOWN' message from the queue. Since we
                    %% want the receive optimization to trigger for that as
                    %% well, we'll treat it as a receive candidate.
                    uses_ref;
                [_MRef, _Options] ->
                    no
            end;
        {erlang,make_ref,0} ->
            {makes_ref, Lbl, Dst};
        {erlang,monitor,A} when is_integer(A), 2 =< A, A =< 3 ->
            {makes_ref, Lbl, Dst};
        {erlang,spawn_monitor,A} when is_integer(A), 1 =< A, A =< 4 ->
            RPO = beam_ssa:rpo([Lbl], Blocks),
            si_ref_in_tuple(RPO, Blocks, Dst);
        {erlang,spawn_request,A} when is_integer(A), 1 =< A, A =< 5 ->
            {makes_ref, Lbl, Dst};
        _ ->
            %% As an aside, spawn_opt/2-5 is trivially supported by handling it
            %% like spawn_monitor/1-4, but this is not forward-compatible as
            %% the return value may be expanded some day. We'll leave it be
            %% until we're okay with setting it in stone.
            no
    end.

si_ref_in_tuple([Lbl | Lbls], Blocks, Tuple) ->
    #b_blk{is=Is} = map_get(Lbl, Blocks),
    case si_ref_in_tuple_is(Is, Tuple) of
        {yes, Ref} -> {makes_ref, Lbl, Ref};
        no -> si_ref_in_tuple(Lbls, Blocks, Tuple)
    end;
si_ref_in_tuple([], _Blocks, _Tuple) ->
    no.

si_ref_in_tuple_is([#b_set{op=get_tuple_element,dst=Ref,
                           args=[#b_var{}=Tuple,Pos]} | Is],
                   Tuple) ->
    case Pos of
        #b_literal{val=1} -> {yes, Ref};
        _ -> si_ref_in_tuple_is(Is, Tuple)
    end;
si_ref_in_tuple_is([_I | Is], Tuple) ->
    si_ref_in_tuple_is(Is, Tuple);
si_ref_in_tuple_is([], _Tuple) ->
    no.

%% Plans our optimizations given the module-wide graph and reference/receive
%% candidates.
plan(Scan) ->
    #scan{ ref_candidates = RefCandidates,
           recv_candidates = ReceiveCandidates,
           module = ModMap,
           graph = Graph } = Scan,

    %% For all blocks that create new references, mark said references as
    %% available in all their successors.
    RefMap0 = propagate_references(RefCandidates, Graph),

    %% For all receive loops, check whether any of the available references are
    %% matched in all clauses.
    Uses = plan_uses(ReceiveCandidates, RefMap0, ModMap),

    %% Limit the reference map to the references that are actually used.
    RefMap = intersect_uses(Uses, RefMap0, Graph),

    %% Reserve and bind markers when we create a reference that we know will be
    %% used.
    Markers = plan_markers(RefCandidates, RefMap),

    %% Clear markers whenever we jump to a block that doesn't precede a use.
    Clears = plan_clears(RefMap, Graph),

    {Markers, Uses, Clears}.

%% Builds a map containing reachable references on a per-vertex basis.
propagate_references(Candidates, G) ->
    Roots = maps:fold(fun(FuncId, MakeRefs, Acc) ->
                              [begin
                                   {_, _, _, ExtractedAt, Ref} = MakeRef,
                                   Vertex = {FuncId, ExtractedAt},
                                   {Vertex, Ref}
                               end || MakeRef <- MakeRefs] ++ Acc
                      end, [], Candidates),
    propagate_references_1(Roots, G, #{}).

propagate_references_1([{Vertex, Ref} | VRefs], G, Acc0) ->
    Refs = maps:get(Vertex, Acc0, sets:new([{version, 2}])),
    Acc = case sets:is_element(Ref, Refs) of
              true ->
                  %% Already visited
                  Acc0;
              false ->
                  Acc1 = Acc0#{ Vertex => sets:add_element(Ref, Refs) },
                  Next = pr_successors(beam_digraph:out_edges(G, Vertex), Ref),
                  propagate_references_1(Next, G, Acc1)
          end,
    propagate_references_1(VRefs, G, Acc);
propagate_references_1([], _G, Acc) ->
    Acc.

pr_successors([{_From, To, branch} | Edges], Ref) ->
    [{To, Ref} | pr_successors(Edges, Ref)];
pr_successors([{{_, FromLbl}, To, {Translation, _Inverse, Args}} | Edges],
              Ref) ->
    case Translation of
        #{ Ref := #b_var{}=Param } ->
            %% If we return a function parameter, we must ignore the return
            %% edge to avoid leaking markers to functions that lack them.
            %% Consider the following:
            %%
            %%    t(NotMarker) ->
            %%        id(NotMarker),
            %%        receive NotMarker -> ok end.
            %%    g() ->
            %%        id(make_ref()).
            %%    id(I) -> I.
            %%
            %% Since id/1 receives a potential marker from at least one source,
            %% its argument is always treated as a marker. Propagating this
            %% back to all callers means that `NotMarker` will be treated as a
            %% marker after the call to id/1, enabling the optimization in the
            %% following receive. This would not be dangerous but it's a
            %% pessimization we'd rather avoid.
            case (FromLbl =/= ?RETURN_BLOCK orelse
                  not member(Ref, Args)) of
                true ->
                    [{To, Param} | pr_successors(Edges, Ref)];
                false ->
                    pr_successors(Edges, Ref)
            end;
        #{} ->
            pr_successors(Edges, Ref)
    end;
pr_successors([], _Ref) ->
    [].

%% Returns the starting vertex of all suitable receive loops, together with the
%% references we can use to jumpstart them.
plan_uses(Candidates, RefMap, ModMap) ->
    maps:fold(fun(FuncId, Receives, Acc) ->
                      #{ FuncId := {Blocks, _Params, _Rets} } = ModMap,
                      case plan_uses_1(Receives, FuncId, Blocks, RefMap) of
                          [_|_]=Uses -> Acc#{ FuncId => Uses };
                          [] -> Acc
                      end
              end, #{}, Candidates).

plan_uses_1([{Lbl, I} | Receives], FuncId, Blocks, RefMap) ->
    case RefMap of
        #{ {FuncId, Lbl} := Refs } ->
            case search(fun(Ref) ->
                                pu_is_ref_used(I, Ref, Lbl, Blocks)
                        end, sets:to_list(Refs)) of
                {value, Ref} ->
                    Use = {Lbl, I, Ref},
                    [Use | plan_uses_1(Receives, FuncId, Blocks, RefMap)];
                false ->
                    plan_uses_1(Receives, FuncId, Blocks, RefMap)
            end;
        #{} ->
            plan_uses_1(Receives, FuncId, Blocks, RefMap)
    end;
plan_uses_1([], _FuncId, _Blocks, _RefMap) ->
    [].

%% Checks whether `Ref` matches a part of the `Msg` in all clauses of the given
%% receive.
pu_is_ref_used(#b_set{op=call,args=[Callee | Args]}, Ref, _Lbl, _Blocks) ->
    MFA = case Callee of
              #b_remote{mod=#b_literal{val=Mod},
                        name=#b_literal{val=Func},
                        arity=Arity} ->
                  {Mod, Func, Arity};
              _ ->
                  none
          end,
    case MFA of
        {erlang,demonitor,2} ->
            [MRef | _] = Args,
            MRef =:= Ref;
        _ ->
            false
    end;
pu_is_ref_used(#b_set{op=peek_message,dst=Msg}=I, Ref, Lbl, Blocks) ->
    #b_blk{is=[I | _]} = Blk = map_get(Lbl, Blocks), %Assertion.
    Vs = #{Msg=>message,Ref=>ref,ref=>Ref,ref_matched=>false},
    case pu_is_ref_used_last(Blk, Vs, Blocks) of
        used -> true;
        not_used -> false;
        done -> false
    end.

pu_is_ref_used_last(#b_blk{last=Last}=Blk, Vs, Blocks) ->
    SuccVs = case Last of
                 #b_br{bool=#b_var{}=Bool,succ=Succ,fail=Fail} ->
                     case Vs of
                         #{Bool:={is_ref,Matched}} ->
                             [{Succ,Vs#{ref_matched:=Matched}},
                              {Fail,Vs#{ref_matched:=not Matched}}];
                         #{} ->
                             [{Succ,Vs},{Fail,Vs}]
                     end;
                 _ ->
                     [{Succ,Vs} || Succ <- beam_ssa:successors(Blk)]
             end,
    %% The receive loop must be terminated before returning.
    [_|_] = SuccVs,                             %Assertion.
    pu_ref_used_in(SuccVs, Blocks).

pu_ref_used_in([{L,Vs0}|Ls], Blocks) ->
    case pu_is_ref_used_in_1(L, Vs0, Blocks) of
        not_used ->
            not_used;
        used ->
            case pu_ref_used_in(Ls, Blocks) of
                done -> used;
                Result -> Result
            end;
        done ->
            pu_ref_used_in(Ls, Blocks)
    end;
pu_ref_used_in([], _) ->
    done.

pu_is_ref_used_in_1(L, Vs0, Blocks) ->
    #b_blk{is=Is} = Blk = map_get(L, Blocks),
    case pu_is_ref_used_is(Is, Vs0) of
        #{}=Vs ->
            pu_is_ref_used_last(Blk, Vs, Blocks);
        Result ->
            Result
    end.

pu_is_ref_used_is([#b_set{op={bif,Bif},args=Args,dst=Dst}=I|Is],
                  Vs0) ->
    if
        Bif =:= '=:='; Bif =:= '==' ->
            case pu_is_ref_msg_comparison(Args, Vs0) of
                true ->
                    Vs = Vs0#{Dst=>{is_ref,true}},
                    pu_is_ref_used_is(Is, Vs);
                false ->
                    pu_is_ref_used_is(Is, Vs0)
            end;
        true ->
            Vs = pu_update_vars(I, Vs0),
            pu_is_ref_used_is(Is, Vs)
    end;
pu_is_ref_used_is([#b_set{op=remove_message}|_], Vs) ->
    case Vs of
        #{ref_matched:=true} ->
            used;
        #{ref_matched:=false} ->
            not_used
    end;
pu_is_ref_used_is([#b_set{op=recv_next}|_], _Vs) ->
    done;
pu_is_ref_used_is([#b_set{op=wait_timeout}|_], _Vs) ->
    done;
pu_is_ref_used_is([#b_set{}=I|Is], Vs0) ->
    %% The receive loop must be terminated before reaching any side-effecting
    %% instructions.
    true = beam_ssa:no_side_effect(I),          %Assertion.
    Vs = pu_update_vars(I, Vs0),
    pu_is_ref_used_is(Is, Vs);
pu_is_ref_used_is([], Vs) ->
    Vs.

pu_update_vars(#b_set{args=Args,dst=Dst}, Vs) ->
    Vars = [V || #b_var{}=V <- Args],
    All = lists:all(fun(Var) ->
                            case Vs of
                                #{Var:=message} -> true;
                                #{} -> false
                            end
                    end, Vars),
    case {Vars,All} of
        {[_|_],true} -> Vs#{Dst=>message};
        {_,_} -> Vs
    end.

%% Returns whether Args denotes a comparison between the reference and message
%% or part of the message.
pu_is_ref_msg_comparison([#b_var{}=V1,#b_var{}=V2], Vs) ->
    case Vs of
        #{V1:=ref,V2:=message} -> true;
        #{V1:=message,V2:=ref} -> true;
        #{} -> false
    end;
pu_is_ref_msg_comparison(_, _) ->
    false.

%% Takes the map of all references available at a given block, and limits it to
%% those that are actually used in (all clauses of) a succeeding receive.
intersect_uses(UsageMap, RefMap, Graph) ->
    Roots = maps:fold(fun(FuncId, Uses, Acc) ->
                              [begin
                                   Vertex = {FuncId, Lbl},
                                   {Vertex, Ref}
                               end || {Lbl, _I, Ref} <- Uses] ++ Acc
                      end, [], UsageMap),
    intersect_uses_1(Roots, RefMap, Graph, #{}).

intersect_uses_1([{Vertex, Ref} | Vs], RefMap, Graph, Acc0) ->
    PossibleRefs = maps:get(Vertex, RefMap, sets:new([{version, 2}])),
    ActiveRefs0 = maps:get(Vertex, Acc0, sets:new([{version, 2}])),
    Acc = case {sets:is_element(Ref, PossibleRefs),
                sets:is_element(Ref, ActiveRefs0)} of
              {true, false} ->
                  %% This block lies between reference creation and the receive
                  %% block, add it to the intersection.
                  Edges = beam_digraph:in_edges(Graph, Vertex),
                  Next = iu_predecessors(Edges, Ref),
                  ActiveRefs = sets:add_element(Ref, ActiveRefs0),
                  intersect_uses_1(Next, RefMap, Graph,
                                   Acc0#{ Vertex => ActiveRefs });
              {false, _} ->
                  %% This block does not succeed the creation of the
                  %% reference. Ignore it.
                  Acc0;
              {_, true} ->
                  %% We've already handled this block, move on.
                  Acc0
          end,
    intersect_uses_1(Vs, RefMap, Graph, Acc);
intersect_uses_1([], _RefMap, _Graph, Acc) ->
    Acc.

iu_predecessors([{From, _To, branch} | Edges], Ref) ->
    [{From, Ref} | iu_predecessors(Edges, Ref)];
iu_predecessors([{From, _To, {_Translation, Inverse, _Args}} | Edges], Ref) ->
    case Inverse of
        #{ Ref := #b_var{}=Arg } ->
            [{From, Arg} | iu_predecessors(Edges, Ref)];
        #{ Ref := [_|_]=Rets } ->
            [{From, Ret} || Ret <- Rets] ++ iu_predecessors(Edges, Ref);
        #{} ->
            %% `Ref` is not a function argument (created in first block), is
            %% not a return value, or it was passed as a literal on this call.
            %% Either way we'll ignore it.
            iu_predecessors(Edges, Ref)
    end;
iu_predecessors([], _Ref) ->
    [].

%% Returns all candidates that are known to be used in at least one receive.
plan_markers(Candidates, UsageMap) ->
    maps:fold(fun(FuncId, MakeRefs, Acc) ->
                      case plan_markers_1(MakeRefs, FuncId, UsageMap) of
                          [_|_]=Marks -> Acc#{ FuncId => Marks };
                          [] -> Acc
                      end
              end, #{}, Candidates).

plan_markers_1(MakeRefs0, FuncId, UsageMap) ->
    [Marker || {_, _, _, ExtractedAt, Ref}=Marker <- MakeRefs0,
               case UsageMap of
                   #{ {FuncId, ExtractedAt} := Refs } ->
                       sets:is_element(Ref, Refs);
                   #{} ->
                       false
               end].

plan_clears(UsageMap, Graph) ->
    maps:fold(fun({FuncId, _}=Vertex, ActiveRefs, Acc) ->
                      Edges = beam_digraph:out_edges(Graph, Vertex),
                      case plan_clears_1(Edges, ActiveRefs, UsageMap) of
                          [_|_]=Clears ->
                              Clears0 = maps:get(FuncId, Acc, []),
                              Acc#{ FuncId => Clears ++ Clears0 };
                          [] ->
                              Acc
                      end
              end, #{}, maps:iterator(UsageMap, ordered)).

plan_clears_1([{From, To, branch} | Edges], ActiveRefs, UsageMap) ->
    %% Clear all references that are no longer active on the `To` block.
    ToRefs = maps:get(To, UsageMap, sets:new([{version, 2}])),
    Refs = sets:subtract(ActiveRefs, ToRefs),

    {FuncId, FromLbl} = From,
    {FuncId, ToLbl} = To,

    Clears = [{FromLbl, ToLbl, Ref} || Ref <- sets:to_list(Refs)],
    Clears ++ plan_clears_1(Edges, ActiveRefs, UsageMap);
plan_clears_1([{_From, _To, {_, _, _}} | Edges],
              ActiveRefs, UsageMap) ->
    %% We don't need to clear references on calls and returns: those we haven't
    %% passed will remain valid after we return, and those we do pass will be
    %% cleared by the caller/callee when necessary.
    plan_clears_1(Edges, ActiveRefs, UsageMap);
plan_clears_1([], _ActiveRefs, _UsageMap) ->
    [].

optimize(#b_module{body=Fs0}=Mod, Markers, Uses, Clears) ->
    Fs = [optimize_1(F, Markers, Uses, Clears) || F <- Fs0],
    Mod#b_module{body=Fs}.

optimize_1(#b_function{bs=Blocks0,cnt=Count0}=F, Markers, Uses, Clears) ->
    FuncId = get_func_id(F),

    {Blocks1, Count1} = insert_markers(maps:get(FuncId, Markers, []),
                                       Blocks0, Count0),
    {Blocks2, Count2} = insert_uses(maps:get(FuncId, Uses, []),
                                    Blocks1, Count1),
    {Blocks, Count} = insert_clears(maps:get(FuncId, Clears, []),
                                    Blocks2, Count2),

    F#b_function{bs=Blocks,cnt=Count}.

insert_markers([{Anno, CreatedAt, Dst, ExtractedAt, Ref} | Markers],
               Blocks0, Count0) ->
    {MarkerVar, Blocks1, Count1} =
        insert_reserve(CreatedAt, Dst, Anno, Blocks0, Count0),
    {Blocks, Count} =
        insert_bind(ExtractedAt, Ref, MarkerVar, Blocks1, Count1),
    insert_markers(Markers, Blocks, Count);
insert_markers([], Blocks, Count) ->
    {Blocks, Count}.

insert_reserve(Lbl, Dst, Anno, Blocks0, Count0) ->
    #{ Lbl := #b_blk{is=Is0}=Blk } = Blocks0,

    Var = #b_var{name=Count0},
    Count = Count0 + 1,

    Reserve = #b_set{anno=Anno,op=recv_marker_reserve,args=[],dst=Var},

    Is = insert_reserve_is(Is0, Reserve, Dst),
    Blocks = Blocks0#{ Lbl := Blk#b_blk{is=Is} },

    {Var, Blocks, Count}.

insert_reserve_is([#b_set{dst=Var} | _]=Is, Reserve, Var) ->
    [Reserve | Is];
insert_reserve_is([I | Is], Reserve, Var) ->
    [I | insert_reserve_is(Is, Reserve, Var)].

insert_bind(Lbl, Ref, Marker, Blocks0, Count0) ->
    #{ Lbl := #b_blk{is=Is0,last=Last}=Blk } = Blocks0,

    Ignored = #b_var{name=Count0},
    Count = Count0 + 1,

    Bind = #b_set{ op=recv_marker_bind,
                   args=[Marker,Ref],
                   dst=Ignored },

    Is = insert_bind_is(Is0, Bind, Last),
    Blocks = Blocks0#{ Lbl := Blk#b_blk{is=Is} },

    {Blocks, Count}.

insert_bind_is([#b_set{}, #b_set{op={succeeded,_}}]=Is, Bind, _Last) ->
    [Bind | Is];
insert_bind_is([#b_set{op=call,dst=Ret}]=Is, Bind, #b_ret{arg=Ret}) ->
    [Bind | Is];
insert_bind_is([#b_set{op=new_try_tag}]=Is, Bind, _Last) ->
    [Bind | Is];
insert_bind_is([#b_set{op=Op}=I | Is], Bind, Last) ->
    true = Op =/= bs_put,                       %Assertion.
    [I | insert_bind_is(Is, Bind, Last)];
insert_bind_is([], Bind, _Last) ->
    [Bind].

insert_uses([{_Lbl, #b_set{op=call}, _Ref} | Uses], Blocks, Count) ->
    %% The callee uses the marker internally. There's no need to emit a use
    %% here.
    insert_uses(Uses, Blocks, Count);
insert_uses([{Lbl, #b_set{op=peek_message}=Peek0, Ref} | Uses],
            Blocks0, Count) ->
    #{ Lbl := #b_blk{is=Is0}=Blk } = Blocks0,

    [Peek0 | Is] = Is0,                         %Assertion.
    Peek = Peek0#b_set{args=[Ref]},

    Blocks = Blocks0#{ Lbl := Blk#b_blk{is=[Peek | Is]} },
    insert_uses(Uses, Blocks, Count);
insert_uses([], Blocks, Count) ->
    {Blocks, Count}.

insert_clears(Clears0, Blocks0, Count0) ->
    {Insertions, Count} = insert_clears_1(Clears0, Count0, []),
    beam_ssa:insert_on_edges(Insertions, Blocks0, Count).

insert_clears_1([{From, To, Ref} | Clears], Count0, Acc) ->
    Ignored = #b_var{name=Count0},
    Count = Count0 + 1,

    Clear = #b_set{op=recv_marker_clear,args=[Ref],dst=Ignored},
    insert_clears_1(Clears, Count, [{From, To, [Clear]} | Acc]);
insert_clears_1([], Count, Acc) ->
    {Acc, Count}.

%%%
%%% +recv_opt_info
%%%

collect_opt_info(#b_module{body=Fs}) ->
    coi_1(Fs, []).

coi_1([#b_function{args=Args,bs=Blocks}=F | Fs], Acc0) ->
    Lbls = beam_ssa:rpo(Blocks),
    Where = beam_ssa:get_anno(location, F, []),
    {Defs, _} = foldl(fun(Var, {Defs0, Index0}) ->
                              Defs = Defs0#{ Var => {parameter, Index0}},
                              Index = Index0 + 1,
                              {Defs, Index}
                      end, {#{}, 1}, Args),
    Acc = coi_bs(Lbls, Blocks, Where, Defs, Acc0),
    coi_1(Fs, Acc);
coi_1([], Acc) ->
    Acc.

coi_bs([Lbl | Lbls], Blocks, Where, Defs0, Ws0) ->
    #{ Lbl := #b_blk{is=Is,last=Last} } = Blocks,
    {Defs, Ws} = coi_is(Is, Last, Blocks, Where, Defs0, Ws0),
    coi_bs(Lbls, Blocks, Where, Defs, Ws);
coi_bs([], _Blocks, _Where, _Defs, Ws) ->
    Ws.

coi_is([#b_set{anno=Anno,op=peek_message,args=[#b_var{}]=Args } | Is],
       Last, Blocks, Where, Defs, Ws) ->
    [Creation] = coi_creations(Args, Blocks, Defs), %Assertion.
    Warning = make_warning({used_receive_marker, Creation}, Anno, Where),
    coi_is(Is, Last, Blocks, Where, Defs, [Warning | Ws]);
coi_is([#b_set{anno=Anno,op=peek_message,args=[#b_literal{}] } | Is],
       Last, Blocks, Where, Defs, Ws) ->

    %% Is this a selective receive?
    #b_br{succ=NextMsg} = Last,
    #{ NextMsg := #b_blk{is=NextIs} } = Blocks,
    Info = case NextIs of
               [#b_set{op=remove_message} | _] -> matches_any_message;
               _ -> unoptimized_selective_receive
           end,

    Warning = make_warning(Info, Anno, Where),
    coi_is(Is, Last, Blocks, Where, Defs, [Warning | Ws]);
coi_is([#b_set{anno=Anno,op=recv_marker_reserve} | Is],
       Last, Blocks, Where, Defs, Ws) ->
    Warning = make_warning(reserved_receive_marker, Anno, Where),
    coi_is(Is, Last, Blocks, Where, Defs, [Warning | Ws]);
coi_is([#b_set{anno=Anno,op=call,dst=Dst,args=[#b_local{} | Args] }=I | Is],
       Last, Blocks, Where, Defs0, Ws0) ->
    Defs = Defs0#{ Dst => I },
    Ws = [make_warning({passed_marker, Creation}, Anno, Where)
          || #b_set{}=Creation <- coi_creations(Args, Blocks, Defs)] ++ Ws0,
    coi_is(Is, Last, Blocks, Where, Defs, Ws);
coi_is([#b_set{dst=Dst}=I | Is], Last, Blocks, Where, Defs0, Ws) ->
    Defs = Defs0#{ Dst => I },
    coi_is(Is, Last, Blocks, Where, Defs, Ws);
coi_is([], _Last, _Blocks, _Where, Defs, Ws) ->
    {Defs, Ws}.

coi_creations([Var | Vars], Blocks, Defs) ->
    case Defs of
        #{ Var := #b_set{op=call,dst=Dst,args=Args}=Call } ->
            case si_remote_call_1(Dst, Args, ?ENTRY_BLOCK, Blocks) of
                {makes_ref, _, _} ->
                    [Call | coi_creations(Vars, Blocks, Defs)];
                _ ->
                    coi_creations(Vars, Blocks, Defs)
            end;
        #{ Var := #b_set{op=get_tuple_element,args=[Tuple|_]}} ->
            coi_creations([Tuple | Vars], Blocks, Defs);
        #{ Var := {parameter, _}=Parameter } ->
            [Parameter | coi_creations(Vars, Blocks, Defs)];
        #{} ->
            coi_creations(Vars, Blocks, Defs)
    end;
coi_creations([], _Blocks, _Defs) ->
    [].

make_warning(Term, Anno, Where) ->
    {File, Line} =
        case maps:get(location, Anno, Where) of
            {_, _} = Location -> Location;
            _ -> {"no_file", none}
        end,
    {File,[{Line,?MODULE,Term}]}.

format_opt_info(matches_any_message) ->
    "INFO: receive matches any message, this is always fast";
format_opt_info({passed_marker, Creation}) ->
    io_lib:format("INFO: passing reference ~ts",
                  [format_ref_creation(Creation)]);
format_opt_info({used_receive_marker, Creation}) ->
    io_lib:format("OPTIMIZED: all clauses match reference ~ts",
                  [format_ref_creation(Creation)]);
format_opt_info(reserved_receive_marker) ->
    "OPTIMIZED: reference used to mark a message queue position";
format_opt_info(unoptimized_selective_receive) ->
    "NOT OPTIMIZED: all clauses do not match a suitable reference".

format_ref_creation({parameter, Index}) ->
    io_lib:format("in function parameter ~w", [Index]);
format_ref_creation(#b_set{op=call,anno=Anno,args=[Callee|_]}) ->
    #b_remote{name=#b_literal{val=F},arity=A} = Callee,
    {File, Line} = maps:get(location, Anno, {"",1}),
    io_lib:format("created by ~p/~p at ~ts:~w", [F, A, File, Line]).
