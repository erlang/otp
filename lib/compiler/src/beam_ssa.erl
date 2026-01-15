%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2018-2025. All Rights Reserved.
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
%% Purpose: Type definitions and utilities for the SSA format.

-module(beam_ssa).
-moduledoc false.
-export([add_anno/3,get_anno/2,get_anno/3,
         between/4,
         clobbers_xregs/1,def/2,def_unused/3,
         definitions/2,
         dominators/2,dominators_from_predecessors/2,common_dominators/3,
         eval_instr/1,
         flatmapfold_instrs/4,
         fold_blocks/4,
         fold_instrs/4,
         insert_on_edges/3,
         is_loop_header/1,
         linearize/1,
         mapfold_blocks/4,
         mapfold_instrs/4,
         merge_blocks/2,
         normalize/1,
         no_side_effect/1,
         can_be_guard_bif/3,
         predecessors/1,
         rename_vars/3,
         rpo/1,rpo/2,
         split_blocks_before/4,split_blocks_after/4,
         successors/1,successors/2,
         trim_unreachable/1,
         used/1,uses/2]).

-export_type([b_module/0,b_function/0,b_blk/0,b_set/0,
              b_ret/0,b_br/0,b_switch/0,terminator/0,
              b_var/0,b_literal/0,b_remote/0,b_local/0,
              value/0,argument/0,label/0,
              var_name/0,literal_value/0,
              op/0,anno/0,block_map/0,dominator_map/0,
              rename_map/0,rename_proplist/0,usage_map/0,
              definition_map/0,predecessor_map/0]).

-include("beam_ssa.hrl").

-type b_module()   :: #b_module{}.
-type b_function() :: #b_function{}.
-type b_blk()      :: #b_blk{}.
-type b_set()      :: #b_set{}.

-type b_br()       :: #b_br{}.
-type b_ret()      :: #b_ret{}.
-type b_switch()   :: #b_switch{}.
-type terminator() :: b_br() | b_ret() | b_switch().

-type construct()  :: b_module() | b_function() | b_blk() | b_set() |
                      terminator().

-type b_var()      :: #b_var{}.
-type b_literal()  :: #b_literal{}.
-type b_remote()   :: #b_remote{}.
-type b_local()    :: #b_local{}.

-type value()      :: b_var() | b_literal().
-type phi_value()  :: {value(),label()}.
-type argument()   :: value() | b_remote() | b_local() | phi_value().
-type label()      :: non_neg_integer().

-type var_name()   :: atom() | non_neg_integer().

-type literal_value() :: atom() | integer() | float() | list() |
                         nil() | tuple() | map() | binary() | fun().

-type op()   :: {'bif',atom()} |
                {'float',float_op()} |
                {'succeeded', 'guard' | 'body'} |
                prim_op() |
                cg_prim_op().

-type anno() :: #{atom() := any()}.

-type block_map() :: #{label():=b_blk()}.
-type dominator_map() :: #{label():=[label()]}.
-type numbering_map() :: #{label():=non_neg_integer()}.
-type usage_map() :: #{b_var():=[{label(),b_set() | terminator()}]}.
-type definition_map() :: #{b_var():=b_set()}.
-type predecessor_map() :: #{label():=[label()]}.
-type rename_map() :: #{b_var():=value()}.
-type rename_proplist() :: [{b_var(),value()}].

%% Note: By default, dialyzer will collapse this type to atom().
%% To avoid the collapsing, change the value of SET_LIMIT to 50 in the
%% file erl_types.erl in the dialyzer application.

-type prim_op() :: 'bs_create_bin' |
                   'bs_extract' | 'bs_ensure' | 'bs_get_tail' | 'bs_init_writable' |
                   'bs_match' | 'bs_start_match' | 'bs_test_tail' |
                   'build_stacktrace' |
                   'call' | 'catch_end' |
                   'executable_line' | 'extract' |
                   'get_hd' | 'get_map_element' | 'get_tl' | 'get_tuple_element' |
                   'has_map_field' |
                   'is_nonempty_list' | 'is_tagged_tuple' |
                   'kill_try_tag' |
                   'landingpad' |
                   'make_fun' | 'match_fail' | 'new_try_tag' |
                   'nif_start' |
                   'peek_message' | 'phi' | 'put_list' | 'put_map' | 'put_tuple' |
                   'raw_raise' |
                   'recv_marker_bind' |
                   'recv_marker_clear' |
                   'recv_marker_reserve' |
                   'recv_next' | 'remove_message' | 'resume' |
                   'update_tuple' | 'update_record' |
                   'wait_timeout'.

-type float_op() :: 'checkerror' | 'clearerror' | 'convert' | 'get' | 'put' |
                    '+' | '-' | '*' | '/'.

%% Primops only used internally during code generation.
-type cg_prim_op() :: 'bs_ensured_get' |
                      'bs_ensured_match_string' |
                      'bs_ensured_skip' |
                      'bs_get' | 'bs_get_position' | 'bs_match_string' |
                      'bs_restore' | 'bs_save' | 'bs_set_position' | 'bs_skip' |
                      'copy' | 'match_fail' | 'put_tuple_arity' |
                      'set_tuple_element' | 'succeeded' |
                      'update_record'.

-import(lists, [foldl/3,mapfoldl/3,member/2,reverse/1,reverse/2,sort/1]).

-spec add_anno(Key, Value, Construct0) -> Construct when
      Key :: atom(),
      Value :: any(),
      Construct0 :: construct(),
      Construct :: construct().

add_anno(Key, Val, #b_function{anno=Anno}=Bl) ->
    Bl#b_function{anno=Anno#{Key=>Val}};
add_anno(Key, Val, #b_blk{anno=Anno}=Bl) ->
    Bl#b_blk{anno=Anno#{Key=>Val}};
add_anno(Key, Val, #b_set{anno=Anno}=Bl) ->
    Bl#b_set{anno=Anno#{Key=>Val}};
add_anno(Key, Val, #b_br{anno=Anno}=Bl) ->
    Bl#b_br{anno=Anno#{Key=>Val}};
add_anno(Key, Val, #b_ret{anno=Anno}=Bl) ->
    Bl#b_ret{anno=Anno#{Key=>Val}};
add_anno(Key, Val, #b_switch{anno=Anno}=Bl) ->
    Bl#b_switch{anno=Anno#{Key=>Val}}.

-spec get_anno(atom(), construct()) -> any().

get_anno(Key, Construct) ->
    map_get(Key, get_anno(Construct)).

-spec get_anno(atom(), construct(), any()) -> any().

get_anno(Key, Construct, Default) ->
    maps:get(Key, get_anno(Construct), Default).

get_anno(#b_function{anno=Anno}) -> Anno;
get_anno(#b_blk{anno=Anno}) -> Anno;
get_anno(#b_set{anno=Anno}) -> Anno;
get_anno(#b_br{anno=Anno}) -> Anno;
get_anno(#b_ret{anno=Anno}) -> Anno;
get_anno(#b_switch{anno=Anno}) -> Anno.

%% clobbers_xregs(#b_set{}) -> true|false.
%%  Test whether the instruction invalidates all X registers.

-spec clobbers_xregs(b_set()) -> boolean().

clobbers_xregs(#b_set{op=Op}) ->
    case Op of
        bs_init_writable -> true;
        build_stacktrace -> true;
        call -> true;
        landingpad -> true;
        peek_message -> true;
        raw_raise -> true;
        wait_timeout -> true;
        _ -> false
    end.

%% no_side_effect(#b_set{}) -> true|false.
%%  Test whether this instruction has no side effect and thus is safe
%%  not to execute if its value is not used. Note that even if `true`
%%  is returned, the instruction could still be impure (e.g. bif:get).

-spec no_side_effect(b_set()) -> boolean().

no_side_effect(#b_set{op=Op}) ->
    case Op of
        {bif,_} -> true;
        {float,get} -> true;
        bs_create_bin -> true;
        bs_init_writable -> true;
        bs_extract -> true;
        bs_match -> true;
        bs_start_match -> true;
        bs_test_tail -> true;
        bs_get_tail -> true;
        build_stacktrace -> true;
        extract -> true;
        get_hd -> true;
        get_tl -> true;
        get_map_element -> true;
        get_tuple_element -> true;
        has_map_field -> true;
        is_nonempty_list -> true;
        is_tagged_tuple -> true;
        make_fun -> true;
        match_fail -> true;
        phi -> true;
        put_map -> true;
        put_list -> true;
        put_tuple -> true;
        raw_raise -> true;
        {succeeded,guard} -> true;
        update_record -> true;
        update_tuple -> true;
        _ -> false
    end.

-spec can_be_guard_bif(atom(), atom(), integer()) -> boolean().

can_be_guard_bif(M, F, A) ->
    case {M,F,A} of
        {erlang, binary_to_atom, 1} -> true;
        {erlang, binary_to_atom, 2} -> true;
        {erlang, binary_to_existing_atom, 1} -> true;
        {erlang, binary_to_existing_atom, 2} -> true;
        {erlang, list_to_atom, 1} -> true;
        {erlang, list_to_existing_atom, 1} -> true;
        {_,_,_} -> false
    end.



%% insert_on_edges(Insertions, BlockMap, Count) -> {BlockMap, Count}.
%%  Inserts instructions on the specified normal edges. It will not work on
%%  exception edges.
%%
%%  That is, `[{12, 34, [CallInstr]}]` will insert `CallInstr` on all jumps
%%  from block 12 to block 34.
-spec insert_on_edges(Insertions, Blocks, Count) -> Result when
    Insertions :: [{From, To, Is}],
    From :: label(),
    To :: label(),
    Is :: [b_set()],
    Blocks :: block_map(),
    Count :: label(),
    Result :: {block_map(), label()}.

insert_on_edges(Insertions, Blocks, Count) when is_map(Blocks) ->
    %% Sort insertions to simplify the handling of duplicates.
    insert_on_edges_1(sort(Insertions), Blocks, Count).

insert_on_edges_1([{_, ?EXCEPTION_BLOCK, _} | _], _, _) ->
    %% Internal error; we can't run code on specific exception edges without
    %% adding try/catch everywhere. Passes must avoid this.
    error(unsafe_edge);
insert_on_edges_1([{From, To, IsA}, {From, To, IsB} | Insertions],
                  Blocks, Count) ->
    %% Join duplicate insertions into the same block so we won't have to track
    %% which edges we've already inserted code on.
    insert_on_edges_1([{From, To, IsA ++ IsB} | Insertions], Blocks, Count);
insert_on_edges_1([{From, To, Is} | Insertions], Blocks0, Count0) ->
    #b_blk{last=FromLast0} = FromBlk0 = map_get(From, Blocks0),
    #b_blk{is=ToIs0} = ToBlk0 = map_get(To, Blocks0),

    EdgeLbl = Count0,
    Count = Count0 + 1,

    FromLast = insert_on_edges_reroute(FromLast0, To, EdgeLbl),
    FromBlk = FromBlk0#b_blk{last=FromLast},

    {EdgeIs0, ToIs} = insert_on_edges_is(ToIs0, From, EdgeLbl, []),
    EdgeIs = EdgeIs0 ++ Is,

    Br = #b_br{bool=#b_literal{val=true},
               succ=To,
               fail=To},

    EdgeBlk = #b_blk{is=EdgeIs,last=Br},
    ToBlk = ToBlk0#b_blk{is=ToIs},

    Blocks1 = Blocks0#{ EdgeLbl => EdgeBlk,
                        From := FromBlk,
                        To := ToBlk },
    Blocks = update_phi_labels([To], From, EdgeLbl, Blocks1),

    insert_on_edges_1(Insertions, Blocks, Count);
insert_on_edges_1([], Blocks, Count) ->
    {Blocks, Count}.

insert_on_edges_reroute(#b_switch{fail=Fail0,list=List0}=Sw, Old, New) ->
    Fail = rename_label(Fail0, Old, New),
    List = [{Value, rename_label(Dst, Old, New)} || {Value, Dst} <:- List0],
    Sw#b_switch{fail=Fail,list=List};
insert_on_edges_reroute(#b_br{succ=Succ0,fail=Fail0}=Br, Old, New) ->
    Succ = rename_label(Succ0, Old, New),
    Fail = rename_label(Fail0, Old, New),
    Br#b_br{succ=Succ,fail=Fail}.

insert_on_edges_is([#b_set{op=bs_extract}=I | Is], FromLbl, EdgeLbl, EdgeIs) ->
    %% Bit-syntax instructions span across edges, so we must hoist them into
    %% the edge block to avoid breaking them.
    %%
    %% This is safe because we *KNOW* that there are no other edges leading to
    %% this block.
    insert_on_edges_is(Is, FromLbl, EdgeLbl, [I | EdgeIs]);
insert_on_edges_is(ToIs0, FromLbl, EdgeLbl, EdgeIs) ->
    case ToIs0 of
        [#b_set{op=landingpad} | _] ->
            %% We can't run code on specific exception edges without adding
            %% try/catch everywhere. Passes must avoid this.
            error(unsafe_edge);
        _ ->
            ToIs = update_phi_labels_is(ToIs0, FromLbl, EdgeLbl),
            {reverse(EdgeIs), ToIs}
    end.

%% is_loop_header(#b_set{}) -> true|false.
%%  Test whether this instruction is a loop header.

-spec is_loop_header(b_set()) -> boolean().

is_loop_header(#b_set{op=wait_timeout,args=[Args]}) ->
    case Args of
        #b_literal{val=0} ->
            %% Never jumps back to peek_message
            false;
        _ ->
            true
    end;
is_loop_header(#b_set{op=Op}) ->
    Op =:= peek_message.

-spec predecessors(Blocks) -> Result when
      Blocks :: block_map(),
      Result :: predecessor_map().

predecessors(Blocks) ->
    P0 = [{S,L} || L := Blk <- Blocks,
                   S <- successors(Blk)],
    P1 = sofs:relation(P0),
    P2 = sofs:rel2fam(P1),
    P3 = sofs:to_external(P2),
    P = [{0,[]}|P3],
    maps:from_list(P).

-spec successors(b_blk()) -> [label()].

successors(#b_blk{last=Terminator}) ->
    case Terminator of
        #b_br{bool=#b_literal{val=true},succ=Succ} ->
            [Succ];
        #b_br{bool=#b_literal{val=false},fail=Fail} ->
            [Fail];
        #b_br{succ=Succ,fail=Fail} ->
            [Fail,Succ];
        #b_switch{fail=Fail,list=List} ->
            [Fail|[L || {_,L} <:- List]];
        #b_ret{} ->
            []
    end.

%% normalize(Instr0) -> Instr.
%%  Normalize instructions to help optimizations.
%%
%%  For commutative operators (such as '+' and 'or'), always
%%  place a variable operand before a literal operand.
%%
%%  Normalize #b_br{} to one of the following forms:
%%
%%    #b_br{b_literal{val=true},succ=Label,fail=Label}
%%    #b_br{b_var{},succ=Label1,fail=Label2} where Label1 =/= Label2
%%
%%  Simplify a #b_switch{} with a literal argument to a #b_br{}.
%%
%%  Simplify a #b_switch{} with a variable argument and an empty
%%  switch list to a #b_br{}.

-spec normalize(b_set() | terminator()) ->
          b_set() | terminator().

normalize(#b_set{anno=Anno0,op={bif,Bif},args=Args}=Set) ->
    case {is_commutative(Bif),Args} of
        {true, [#b_literal{}=Lit,#b_var{}=Var]} ->
            Anno = case Anno0 of
                       #{arg_types := ArgTypes0} ->
                           case ArgTypes0 of
                               #{1 := Type} ->
                                   Anno0#{arg_types => #{0 => Type}};
                               #{} ->
                                   Anno0#{arg_types => #{}}
                           end;
                       #{} ->
                           Anno0
                   end,
            Set#b_set{anno=Anno,args=[Var,Lit]};
        {_, _} ->
            Set
    end;
normalize(#b_set{}=Set) ->
    Set;
normalize(#b_br{}=Br) ->
    case Br of
        #b_br{bool=Bool,succ=Same,fail=Same} ->
            case Bool of
                #b_literal{val=true} ->
                    Br;
                _ ->
                    Br#b_br{bool=#b_literal{val=true}}
            end;
        #b_br{bool=#b_literal{val=true},succ=Succ} ->
            Br#b_br{fail=Succ};
        #b_br{bool=#b_literal{val=false},fail=Fail} ->
            Br#b_br{bool=#b_literal{val=true},succ=Fail};
        #b_br{} ->
            Br
    end;
normalize(#b_switch{arg=Arg,fail=Fail,list=List}=Sw) ->
    case Arg of
        #b_literal{} ->
            normalize_switch(Arg, List, Fail);
        #b_var{} when List =:= [] ->
            #b_br{bool=#b_literal{val=true},succ=Fail,fail=Fail};
        #b_var{} ->
            Sw#b_switch{list=sort(List)}
    end;
normalize(#b_ret{}=Ret) ->
    Ret.

normalize_switch(Val, [{Val,L}|_], _Fail) ->
    #b_br{bool=#b_literal{val=true},succ=L,fail=L};
normalize_switch(Val, [_|T], Fail) ->
    normalize_switch(Val, T, Fail);
normalize_switch(_Val, [], Fail) ->
    #b_br{bool=#b_literal{val=true},succ=Fail,fail=Fail}.

-spec successors(label(), block_map()) -> [label()].

successors(L, Blocks) ->
    successors(map_get(L, Blocks)).

-spec def(Ls, Blocks) -> Def when
      Ls :: [label()],
      Blocks :: block_map(),
      Def :: ordsets:ordset(b_var()).

def(Ls, Blocks) when is_map(Blocks) ->
    Blks = [map_get(L, Blocks) || L <- Ls],
    def_1(Blks, []).

-spec def_unused(Ls, Used, Blocks) -> {Def,Unused} when
      Ls :: [label()],
      Used :: ordsets:ordset(b_var()),
      Blocks :: block_map(),
      Def :: ordsets:ordset(b_var()),
      Unused :: ordsets:ordset(b_var()).

def_unused(Ls, Unused, Blocks) when is_map(Blocks) ->
    Blks = [map_get(L, Blocks) || L <- Ls],
    Preds = sets:from_list(Ls),
    def_unused_1(Blks, Preds, [], Unused).

%% dominators(Labels, BlockMap) -> {Dominators,Numbering}.
%%  Calculate the dominator tree, returning a map where each entry
%%  in the map is a list that gives the path from that block to
%%  the top of the dominator tree. (Note that the suffixes of the
%%  paths are shared with each other, which make the representation
%%  of the dominator tree highly memory-efficient.)
%%
%%  The implementation is based on:
%%
%%     http://www.hipersoft.rice.edu/grads/publications/dom14.pdf
%%     Cooper, Keith D.; Harvey, Timothy J; Kennedy, Ken (2001).
%%        A Simple, Fast Dominance Algorithm.

-spec dominators(Labels, Blocks) -> Result when
      Labels :: [label()],
      Blocks :: block_map(),
      Result :: {dominator_map(), numbering_map()}.
dominators(Labels, Blocks) when is_map(Blocks) ->
    Preds = predecessors(Blocks),
    dominators_from_predecessors(Labels, Preds).

-spec dominators_from_predecessors(Labels, Preds) -> Result when
      Labels :: [label()],
      Preds :: predecessor_map(),
      Result :: {dominator_map(), numbering_map()}.
dominators_from_predecessors(Top0, Preds) when is_map(Preds) ->
    Df = maps:from_list(number(Top0, 0)),
    [{0,[]}|Top] = [{L,map_get(L, Preds)} || L <- Top0],

    %% The flow graph for an Erlang function is reducible, and
    %% therefore one traversal in reverse postorder is sufficient.
    Acc = #{0=>[0]},
    {dominators_1(Top, Df, Acc),Df}.

%% common_dominators([Label], Dominators, Numbering) -> [Label].
%%  Calculate the common dominators for the given list of blocks
%%  and Dominators and Numbering as returned from dominators/1.

-spec common_dominators([label()], dominator_map(), numbering_map()) -> [label()].
common_dominators(Ls, Dom, Numbering) when is_map(Dom) ->
    Doms = [map_get(L, Dom) || L <- Ls],
    dom_intersection(Doms, Numbering).


%% eval_instr(Instr) -> #b_literal{} | any | failed.
%%  Attempt to evaluate a BIF instruction. Returns a `#b_literal{}`
%%  record if evaluation succeeded, `failed` if an exception was
%%  raised, or `any` if the arguments were not literals or the BIF is
%%  not pure.

-spec eval_instr(b_set()) -> b_literal() | 'any' | 'failed'.

eval_instr(#b_set{op={bif,Bif},args=Args}) ->
    LitArgs = case Args of
                  [#b_literal{val=Arg1}] ->
                      [Arg1];
                  [#b_literal{val=Arg1},#b_literal{val=Arg2}] ->
                      [Arg1,Arg2];
                  [#b_literal{val=Arg1},#b_literal{val=Arg2},#b_literal{val=Arg3}] ->
                      [Arg1,Arg2,Arg3];
                  _ ->
                      none
              end,
    case LitArgs of
        none ->
            any;
        _ ->
            Arity = length(LitArgs),
            case erl_bifs:is_pure(erlang, Bif, Arity) of
                true ->
                    try apply(erlang, Bif, LitArgs) of
                        Result ->
                            #b_literal{val=Result}
                    catch error:_->
                            failed
                    end;
                false ->
                    any
            end
    end;
eval_instr(_) ->
    any.

-spec fold_instrs(Fun, Labels, Acc0, Blocks) -> any() when
      Fun :: fun((b_set()|terminator(), any()) -> any()),
      Labels :: [label()],
      Acc0 :: any(),
      Blocks :: block_map().

fold_instrs(Fun, Labels, Acc0, Blocks) when is_map(Blocks) ->
    fold_instrs_1(Labels, Fun, Blocks, Acc0).

%% mapfold_blocks(Fun, [Label], Acc, BlockMap) -> {BlockMap,Acc}.
%%  Like mapfold_instrs but at the block level to support lookahead
%%  and scope-dependent transformations.

-spec mapfold_blocks(Fun, Labels, Acc, Blocks) -> Result when
      Fun :: fun((label(), b_blk(), any()) -> {b_blk(), any()}),
      Labels :: [label()],
      Acc :: any(),
      Blocks :: block_map(),
      Result :: {block_map(), any()}.
mapfold_blocks(Fun, Labels, Acc, Blocks) when is_map(Blocks) ->
    foldl(fun(Lbl, A) ->
                  mapfold_blocks_1(Fun, Lbl, A)
          end, {Blocks, Acc}, Labels).

mapfold_blocks_1(Fun, Lbl, {Blocks0, Acc0}) ->
    Block0 = map_get(Lbl, Blocks0),
    {Block, Acc} = Fun(Lbl, Block0, Acc0),
    Blocks = Blocks0#{Lbl:=Block},
    {Blocks, Acc}.

-spec mapfold_instrs(Fun, Labels, Acc0, Blocks0) -> {Blocks,Acc} when
      Fun :: fun((b_set()|terminator(), any()) -> any()),
      Labels :: [label()],
      Acc0 :: any(),
      Acc :: any(),
      Blocks0 :: block_map(),
      Blocks :: block_map().

mapfold_instrs(Fun, Labels, Acc0, Blocks) when is_map(Blocks) ->
    mapfold_instrs_1(Labels, Fun, Blocks, Acc0).

-spec flatmapfold_instrs(Fun, Labels, Acc0, Blocks0) -> {Blocks,Acc} when
      Fun :: fun((b_set()|terminator(), any()) -> any()),
      Labels :: [label()],
      Acc0 :: any(),
      Acc :: any(),
      Blocks0 :: block_map(),
      Blocks :: block_map().

flatmapfold_instrs(Fun, Labels, Acc0, Blocks) when is_map(Blocks) ->
    flatmapfold_instrs_1(Labels, Fun, Blocks, Acc0).

-type fold_fun() :: fun((label(), b_blk(), any()) -> any()).

%% fold_blocks(Fun, [Label], Acc0, Blocks) -> Acc.  Fold over all blocks
%%  from a given set of labels in a reverse postorder traversal of the
%%  block graph; that is, first visit a block, then visit its successors.

-spec fold_blocks(Fun, Labels, Acc0, Blocks) -> any() when
      Fun :: fold_fun(),
      Labels :: [label()],
      Acc0 :: any(),
      Blocks :: #{label():=b_blk()}.

fold_blocks(Fun, Labels, Acc0, Blocks) when is_map(Blocks) ->
    fold_blocks_1(Labels, Fun, Blocks, Acc0).

%% linearize(Blocks) -> [{BlockLabel,#b_blk{}}].
%%  Linearize the intermediate representation of the code.
%%  Unreachable blocks will be discarded, and phi nodes will
%%  be adjusted so that they no longer refers to discarded
%%  blocks or to blocks that no longer are predecessors of
%%  the phi node block.

-spec linearize(Blocks) -> Linear when
      Blocks :: block_map(),
      Linear :: [{label(),b_blk()}].

linearize(Blocks) when is_map(Blocks) ->
    Seen = sets:new(),
    {Linear0,_} = linearize_1([0], Blocks, Seen, []),
    Linear = fix_phis(Linear0, #{}),
    Linear.

-spec rpo(Blocks) -> [Label] when
      Blocks :: block_map(),
      Label :: label().

rpo(Blocks) ->
    rpo([0], Blocks).

-spec rpo(From, Blocks) -> Labels when
      From :: [label()],
      Blocks :: block_map(),
      Labels :: [label()].

rpo(From, Blocks) when is_map(Blocks) ->
    Seen = sets:new(),
    {Ls,_} = rpo_1(From, Blocks, Seen, []),
    Ls.

%% between(From, To, Preds, Blocks) -> RPO
%%  Returns all the blocks between `From` and `To` in reverse post-order. This
%%  is most efficient when `From` dominates `To`, as it won't visit any
%%  unnecessary blocks in that case.

-spec between(From, To, Preds, Blocks) -> Labels when
      From :: label(),
      To :: label(),
      Preds :: predecessor_map(),
      Blocks :: block_map(),
      Labels :: [label()].

between(From, To, Preds, Blocks) when is_map(Preds), is_map(Blocks) ->
    %% Gather the predecessors of `To` and then walk forward from `From`,
    %% skipping the blocks that don't precede `To`.
    %%
    %% As an optimization we initialize the predecessor set with `From` to stop
    %% gathering once seen since we're only interested in the blocks in between.
    %% Uninteresting blocks can still be added if `From` doesn't dominate `To`,
    %% but that has no effect on the final result.
    Filter = between_make_filter([To], Preds, sets:from_list([From])),
    {Paths, _} = between_rpo([From], Blocks, Filter, []),

    Paths.

-spec rename_vars(Rename, [label()], block_map()) -> block_map() when
      Rename :: rename_map() | rename_proplist().

rename_vars(Rename, Labels, Blocks) when is_list(Rename) ->
    rename_vars(maps:from_list(Rename), Labels, Blocks);
rename_vars(Rename, Labels, Blocks) when is_map(Rename), is_map(Blocks) ->
    Preds = sets:from_list(Labels),
    F = fun(#b_set{op=phi,args=Args0}=Set) ->
                Args = rename_phi_vars(Args0, Preds, Rename),
                normalize(Set#b_set{args=Args});
           (#b_set{args=Args0}=Set) ->
                Args = [rename_var(A, Rename) || A <- Args0],
                normalize(Set#b_set{args=Args});
           (#b_switch{arg=Bool}=Sw) ->
                normalize(Sw#b_switch{arg=rename_var(Bool, Rename)});
           (#b_br{bool=Bool}=Br) ->
                normalize(Br#b_br{bool=rename_var(Bool, Rename)});
           (#b_ret{arg=Arg}=Ret) ->
                normalize(Ret#b_ret{arg=rename_var(Arg, Rename)})
        end,
    map_instrs_1(Labels, F, Blocks).

%% split_blocks(Labels, Predicate, Blocks0, Count0) -> {Blocks,Count}.
%%  Call Predicate(Instruction) for each instruction in the given
%%  blocks. If Predicate/1 returns true, split the block before this
%%  instruction. Note that this function won't create a new empty
%%  block if the predicate returns true for the first instruction in a
%%  block.

-spec split_blocks_before(Labels, Pred, Blocks0, Count0) -> {Blocks,Count} when
      Labels :: [label()],
      Pred :: fun((b_set()) -> boolean()),
      Blocks :: block_map(),
      Count0 :: label(),
      Blocks0 :: block_map(),
      Blocks :: block_map(),
      Count :: label().

split_blocks_before(Ls, P, Blocks, Count) when is_map(Blocks) ->
    split_blocks_1(Ls, P, fun split_blocks_before_is/3, Blocks, Count).

%% split_blocks_after(Labels, Predicate, Blocks0, Count0) -> {Blocks,Count}.
%%  Call Predicate(Instruction) for each instruction in the given
%%  blocks. If Predicate/1 returns true, split the block after this
%%  instruction.

-spec split_blocks_after(Labels, Pred, Blocks0, Count0) -> {Blocks,Count} when
      Labels :: [label()],
      Pred :: fun((b_set()) -> boolean()),
      Blocks :: block_map(),
      Count0 :: label(),
      Blocks0 :: block_map(),
      Blocks :: block_map(),
      Count :: label().

split_blocks_after(Ls, P, Blocks, Count) when is_map(Blocks) ->
    split_blocks_1(Ls, P, fun split_blocks_after_is/3, Blocks, Count).

-spec trim_unreachable(SSA0) -> SSA when
      SSA0 :: block_map() | [{label(),b_blk()}],
      SSA :: block_map() | [{label(),b_blk()}].

%% trim_unreachable(Blocks0) -> Blocks.
%%  Remove all unreachable blocks. Adjust all phi nodes so
%%  they don't refer to blocks that has been removed or no
%%  no longer branch to the phi node in question.

trim_unreachable(Blocks) when is_map(Blocks) ->
    %% Could perhaps be optimized if there is any need.
    maps:from_list(linearize(Blocks));
trim_unreachable([_|_]=Blocks) ->
    trim_unreachable_1(Blocks, sets:from_list([0])).

-spec used(b_blk() | b_set() | terminator()) -> [b_var()].

used(#b_blk{is=Is,last=Last}) ->
    used_1([Last|Is], ordsets:new());
used(#b_br{bool=#b_var{}=V}) ->
    [V];
used(#b_ret{arg=#b_var{}=V}) ->
    [V];
used(#b_set{op=phi,args=Args}) ->
    ordsets:from_list([V || {#b_var{}=V,_} <- Args]);
used(#b_set{args=Args}) ->
    ordsets:from_list(used_args(Args));
used(#b_switch{arg=#b_var{}=V}) ->
    [V];
used(_) -> [].

-spec definitions(Labels :: [label()], Blocks :: block_map()) -> definition_map().
definitions(Labels, Blocks) ->
    fold_instrs(fun(#b_set{ dst = Var }=I, Acc) ->
                            Acc#{Var => I};
                       (_Terminator, Acc) ->
                            Acc
                    end, Labels, #{}, Blocks).

%% uses(Labels, BlockMap) -> UsageMap
%%  Traverse the blocks given by labels and builds a usage map
%%  with variables as keys and a list of labels-instructions
%%  tuples as values.
-spec uses(Labels, Blocks) -> usage_map() when
      Labels :: [label()],
      Blocks :: block_map().
uses(Labels, Blocks) when is_map(Blocks) ->
    fold_blocks(fun fold_uses_block/3, Labels, #{}, Blocks).

fold_uses_block(Lbl, #b_blk{is=Is,last=Last}, UseMap0) ->
    F = fun(I, UseMap) ->
                foldl(fun(Var, Acc) ->
                              Uses0 = maps:get(Var, Acc, []),
                              Uses = [{Lbl, I} | Uses0],
                              maps:put(Var, Uses, Acc)
                      end, UseMap, used(I))
        end,
    F(Last, foldl(F, UseMap0, Is)).

-spec merge_blocks([label()], block_map()) -> block_map().

merge_blocks(Labels, Blocks) ->
    Preds = predecessors(Blocks),

    %% We must traverse the blocks in reverse postorder to avoid
    %% embedding succeeded:guard instructions into the middle of
    %% blocks when this function is called from beam_ssa_bool.
    merge_blocks_1(Labels, Preds, Blocks).

%%%
%%% Internal functions.
%%%

is_commutative('and') -> true;
is_commutative('or') -> true;
is_commutative('xor') -> true;
is_commutative('band') -> true;
is_commutative('bor') -> true;
is_commutative('bxor') -> true;
is_commutative('+') -> true;
is_commutative('*') -> true;
is_commutative('=:=') -> true;
is_commutative('==') -> true;
is_commutative('=/=') -> true;
is_commutative('/=') -> true;
is_commutative(_) -> false.

def_unused_1([#b_blk{is=Is,last=Last}|Bs], Preds, Def0, Unused0) ->
    Unused1 = ordsets:subtract(Unused0, used(Last)),
    {Def,Unused} = def_unused_is(Is, Preds, Def0, Unused1),
    def_unused_1(Bs, Preds, Def, Unused);
def_unused_1([], _Preds, Def, Unused) ->
    {ordsets:from_list(Def), Unused}.

def_unused_is([#b_set{op=phi,dst=Dst,args=Args}|Is],
            Preds, Def0, Unused0) ->
    Def = [Dst|Def0],
    %% We must be careful to only include variables that will
    %% be used when arriving from one of the predecessor blocks
    %% in Preds.
    Unused1 = [V || {#b_var{}=V,L} <- Args, sets:is_element(L, Preds)],
    Unused = ordsets:subtract(Unused0, ordsets:from_list(Unused1)),
    def_unused_is(Is, Preds, Def, Unused);
def_unused_is([#b_set{dst=Dst}=I|Is], Preds, Def0, Unused0) ->
    Def = [Dst|Def0],
    Unused = ordsets:subtract(Unused0, used(I)),
    def_unused_is(Is, Preds, Def, Unused);
def_unused_is([], _Preds, Def, Unused) ->
    {Def,Unused}.

def_1([#b_blk{is=Is}|Bs], Def0) ->
    Def = def_is(Is, Def0),
    def_1(Bs, Def);
def_1([], Def) ->
    ordsets:from_list(Def).

def_is([#b_set{dst=Dst}|Is], Def) ->
    def_is(Is, [Dst|Def]);
def_is([], Def) -> Def.

dominators_1([{L,Preds}|Ls], Df, Doms) ->
    DomPreds = [map_get(P, Doms) || P <- Preds, is_map_key(P, Doms)],
    Dom = [L|dom_intersection(DomPreds, Df)],
    dominators_1(Ls, Df, Doms#{L=>Dom});
dominators_1([], _Df, Doms) -> Doms.

dom_intersection([S], _Df) ->
    S;
dom_intersection([S|Ss], Df) ->
    dom_intersection(S, Ss, Df).

dom_intersection([0]=S, [_|_], _Df) ->
    %% No need to continue. (We KNOW that all sets end in [0].)
    S;
dom_intersection(S1, [S2|Ss], Df) ->
    dom_intersection(dom_intersection_1(S1, S2, Df), Ss, Df);
dom_intersection(S, [], _Df) -> S.

dom_intersection_1([E1|Es1]=Set1, [E2|Es2]=Set2, Df) ->
    %% Blocks are numbered in the order they are found in
    %% reverse postorder.
    #{E1:=Df1,E2:=Df2} = Df,
    if
        Df1 > Df2 ->
            dom_intersection_2(Es1, Set2, Df, Df2);
        Df2 > Df1 ->
            dom_intersection_2(Es2, Set1, Df, Df1);
        true ->                                  %Set1 == Set2
            %% The common suffix of the sets is the intersection.
            Set1
    end.

dom_intersection_2([E1|Es1]=Set1, [_|Es2]=Set2, Df, Df2) ->
    %% Blocks are numbered in the order they are found in
    %% reverse postorder.
    #{E1:=Df1} = Df,
    if
        Df1 > Df2 ->
            dom_intersection_2(Es1, Set2, Df, Df2);
        Df2 > Df1 ->
            dom_intersection_2(Es2, Set1, Df, Df1);  %switch arguments
        true ->                                  %Set1 == Set2
            %% The common suffix of the sets is the intersection.
            Set1
    end.

number([L|Ls], N) ->
    [{L,N}|number(Ls, N+1)];
number([], _) -> [].

fold_blocks_1([L|Ls], Fun, Blocks, Acc0) ->
    Block = map_get(L, Blocks),
    Acc = Fun(L, Block, Acc0),
    fold_blocks_1(Ls, Fun, Blocks, Acc);
fold_blocks_1([], _, _, Acc) -> Acc.

fold_instrs_1([L|Ls], Fun, Blocks, Acc0) ->
    #b_blk{is=Is,last=Last} = map_get(L, Blocks),
    Acc1 = foldl(Fun, Acc0, Is),
    Acc = Fun(Last, Acc1),
    fold_instrs_1(Ls, Fun, Blocks, Acc);
fold_instrs_1([], _, _, Acc) -> Acc.

mapfold_instrs_1([L|Ls], Fun, Blocks0, Acc0) ->
    #b_blk{is=Is0,last=Last0} = Block0 = map_get(L, Blocks0),
    {Is,Acc1} = mapfoldl(Fun, Acc0, Is0),
    {Last,Acc} = Fun(Last0, Acc1),
    Block = Block0#b_blk{is=Is,last=Last},
    Blocks = Blocks0#{L:=Block},
    mapfold_instrs_1(Ls, Fun, Blocks, Acc);
mapfold_instrs_1([], _, Blocks, Acc) ->
    {Blocks,Acc}.

flatmapfold_instrs_1([L|Ls], Fun, Blocks0, Acc0) ->
    #b_blk{is=Is0,last=Last0} = Block0 = map_get(L, Blocks0),
    {Is,Acc1} = flatmapfoldl(Fun, Acc0, Is0),
    {[Last],Acc} = Fun(Last0, Acc1),
    Block = Block0#b_blk{is=Is,last=Last},
    Blocks = Blocks0#{L:=Block},
    flatmapfold_instrs_1(Ls, Fun, Blocks, Acc);
flatmapfold_instrs_1([], _, Blocks, Acc) ->
    {Blocks,Acc}.

linearize_1([L|Ls], Blocks, Seen0, Acc0) ->
    case sets:is_element(L, Seen0) of
        true ->
            linearize_1(Ls, Blocks, Seen0, Acc0);
        false ->
            Seen1 = sets:add_element(L, Seen0),
            Block = map_get(L, Blocks),
            Successors = successors(Block),
            {Acc,Seen} = linearize_1(Successors, Blocks, Seen1, Acc0),
            linearize_1(Ls, Blocks, Seen, [{L,Block}|Acc])
    end;
linearize_1([], _, Seen, Acc) ->
    {Acc,Seen}.

fix_phis([{L,Blk0}|Bs], S) ->
    Blk = case Blk0 of
              #b_blk{is=[#b_set{op=phi}|_]=Is0} ->
                  Is = fix_phis_1(Is0, L, S),
                  Blk0#b_blk{is=Is};
              #b_blk{} ->
                  Blk0
          end,
    Successors = successors(Blk),
    [{L,Blk}|fix_phis(Bs, S#{L=>Successors})];
fix_phis([], _) -> [].

fix_phis_1([#b_set{op=phi,args=Args0}=I|Is], L, S) ->
    Args = [{Val,Pred} || {Val,Pred} <:- Args0,
                          is_successor(L, Pred, S)],
    [I#b_set{args=Args}|fix_phis_1(Is, L, S)];
fix_phis_1(Is, _, _) -> Is.

is_successor(L, Pred, S) ->
    case S of
        #{Pred:=Successors} ->
            member(L, Successors);
        #{} ->
            %% This block has been removed.
            false
    end.

trim_unreachable_1([{L,Blk0}|Bs], Seen0) ->
    Blk = trim_phis(Blk0, Seen0),
    case sets:is_element(L, Seen0) of
        false ->
            trim_unreachable_1(Bs, Seen0);
        true ->
            case successors(Blk) of
                [] ->
                    [{L,Blk}|trim_unreachable_1(Bs, Seen0)];
                [Next] ->
                    Seen = sets:add_element(Next, Seen0),
                    [{L,Blk}|trim_unreachable_1(Bs, Seen)];
                [_|_]=Successors ->
                    Seen = sets:union(Seen0, sets:from_list(Successors)),
                    [{L,Blk}|trim_unreachable_1(Bs, Seen)]
            end
    end;
trim_unreachable_1([], _) -> [].

trim_phis(#b_blk{is=[#b_set{op=phi}|_]=Is0}=Blk, Seen) ->
    Is = trim_phis_1(Is0, Seen),
    Blk#b_blk{is=Is};
trim_phis(Blk, _Seen) -> Blk.

trim_phis_1([#b_set{op=phi,args=Args0}=I|Is], Seen) ->
    Args = [P || {_,L}=P <:- Args0, sets:is_element(L, Seen)],
    [I#b_set{args=Args}|trim_phis_1(Is, Seen)];
trim_phis_1(Is, _Seen) -> Is.

between_make_filter([L | Ls], Preds, Acc0) ->
    case sets:is_element(L, Acc0) of
        true ->
            between_make_filter(Ls, Preds, Acc0);
        false ->
            Next = map_get(L, Preds),
            Acc1 = sets:add_element(L, Acc0),

            Acc = between_make_filter(Next, Preds, Acc1),
            between_make_filter(Ls, Preds, Acc)
    end;
between_make_filter([], _Preds, Acc) ->
    Acc.

between_rpo([L | Ls], Blocks, Filter0, Acc0) ->
    case sets:is_element(L, Filter0) of
        true ->
            Block = map_get(L, Blocks),
            Filter1 = sets:del_element(L, Filter0),

            Successors = successors(Block),
            {Acc, Filter} = between_rpo(Successors, Blocks, Filter1, Acc0),
            between_rpo(Ls, Blocks, Filter, [L | Acc]);
        false ->
            between_rpo(Ls, Blocks, Filter0, Acc0)
    end;
between_rpo([], _, Filter, Acc) ->
    {Acc, Filter}.

rpo_1([L|Ls], Blocks, Seen0, Acc0) ->
    case sets:is_element(L, Seen0) of
        true ->
            rpo_1(Ls, Blocks, Seen0, Acc0);
        false ->
            Block = map_get(L, Blocks),
            Seen1 = sets:add_element(L, Seen0),
            Successors = successors(Block),
            {Acc,Seen} = rpo_1(Successors, Blocks, Seen1, Acc0),
            rpo_1(Ls, Blocks, Seen, [L|Acc])
    end;
rpo_1([], _, Seen, Acc) ->
    {Acc,Seen}.

rename_var(#b_var{}=Old, Rename) ->
    case Rename of
        #{Old:=New} -> New;
        #{} -> Old
    end;
rename_var(#b_remote{mod=Mod0,name=Name0}=Remote, Rename) ->
    Mod = rename_var(Mod0, Rename),
    Name = rename_var(Name0, Rename),
    Remote#b_remote{mod=Mod,name=Name};
rename_var(Old, _) -> Old.

rename_phi_vars([{Var,L}|As], Preds, Ren) ->
    case sets:is_element(L, Preds) of
        true ->
            [{rename_var(Var, Ren),L}|rename_phi_vars(As, Preds, Ren)];
        false ->
            [{Var,L}|rename_phi_vars(As, Preds, Ren)]
    end;
rename_phi_vars([], _, _) -> [].

map_instrs_1([L|Ls], Fun, Blocks0) ->
    #b_blk{is=Is0,last=Last0} = Blk0 = map_get(L, Blocks0),
    Is = [Fun(I) || I <- Is0],
    Last = Fun(Last0),
    Blk = Blk0#b_blk{is=Is,last=Last},
    Blocks = Blocks0#{L:=Blk},
    map_instrs_1(Ls, Fun, Blocks);
map_instrs_1([], _, Blocks) -> Blocks.

flatmapfoldl(F, Accu0, [Hd|Tail]) ->
    {R,Accu1} = F(Hd, Accu0),
    {Rs,Accu2} = flatmapfoldl(F, Accu1, Tail),
    {R++Rs,Accu2};
flatmapfoldl(_, Accu, []) -> {[],Accu}.

split_blocks_1([L|Ls], P, Split, Blocks0, Count0) ->
    #b_blk{is=Is0} = Blk = map_get(L, Blocks0),
    case Split(Is0, P, []) of
        {yes,Bef,Aft} ->
            NewLbl = Count0,
            Count = Count0 + 1,
            Br = #b_br{bool=#b_literal{val=true},succ=NewLbl,fail=NewLbl},
            BefBlk = Blk#b_blk{is=Bef,last=Br},
            NewBlk = Blk#b_blk{is=Aft},
            Blocks1 = Blocks0#{L:=BefBlk,NewLbl=>NewBlk},
            Successors = successors(NewBlk),
            Blocks = update_phi_labels(Successors, L, NewLbl, Blocks1),
            split_blocks_1([NewLbl|Ls], P, Split, Blocks, Count);
        no ->
            split_blocks_1(Ls, P, Split, Blocks0, Count0)
    end;
split_blocks_1([], _, _, Blocks, Count) ->
    {Blocks,Count}.

split_blocks_before_is([I|Is], P, []) ->
    split_blocks_before_is(Is, P, [I]);
split_blocks_before_is([I|Is], P, Acc) ->
    case P(I) of
        true ->
            {yes,reverse(Acc),[I|Is]};
        false ->
            split_blocks_before_is(Is, P, [I|Acc])
    end;
split_blocks_before_is([], _, _) -> no.

split_blocks_after_is([I|Is], P, Acc) ->
    case P(I) of
        true ->
            {yes,reverse(Acc, [I]),Is};
        false ->
            split_blocks_after_is(Is, P, [I|Acc])
    end;
split_blocks_after_is([], _, _) -> no.

update_phi_labels_is([#b_set{op=phi,args=Args0}=I0|Is], Old, New) ->
    Args = [{Arg,rename_label(Lbl, Old, New)} || {Arg,Lbl} <:- Args0],
    I = I0#b_set{args=Args},
    [I|update_phi_labels_is(Is, Old, New)];
update_phi_labels_is(Is, _, _) -> Is.

rename_label(Old, Old, New) -> New;
rename_label(Lbl, _Old, _New) -> Lbl.

used_args([#b_var{}=V|As]) ->
    [V|used_args(As)];
used_args([#b_remote{mod=Mod,name=Name}|As]) ->
    used_args([Mod,Name|As]);
used_args([_|As]) ->
    used_args(As);
used_args([]) -> [].

used_1([H|T], Used0) ->
    Used = ordsets:union(used(H), Used0),
    used_1(T, Used);
used_1([], Used) -> Used.


%%% Merge blocks.

merge_blocks_1([L|Ls], Preds0, Blocks0) ->
    case Preds0 of
        #{L:=[P]} ->
            #{P:=Blk0,L:=Blk1} = Blocks0,
            case is_merge_allowed(L, Blk0, Blk1) of
                true ->
                    #b_blk{is=Is0} = Blk0,
                    #b_blk{is=Is1} = Blk1,
                    verify_merge_is(Is1),
                    Is = merge_fix_succeeded(Is0 ++ Is1, Blk1),
                    Blk = Blk1#b_blk{is=Is},
                    Blocks1 = maps:remove(L, Blocks0),
                    Blocks2 = Blocks1#{P:=Blk},
                    Successors = successors(Blk),
                    Blocks = update_phi_labels(Successors, L, P, Blocks2),
                    Preds = merge_update_preds(Successors, L, P, Preds0),
                    merge_blocks_1(Ls, Preds, Blocks);
                false ->
                    merge_blocks_1(Ls, Preds0, Blocks0)
            end;
        #{} ->
            merge_blocks_1(Ls, Preds0, Blocks0)
    end;
merge_blocks_1([], _Preds, Blocks) -> Blocks.

%% Since we process the candidates in reverse postorder it is necessary
%% to update the predecessors. Reverse postorder is necessary to ensure
%% that merge_fix_succeeded/2 will find and remove all succeeded:guard
%% not followed by a two-way branch.
merge_update_preds([L|Ls], From, To, Preds0) ->
    case Preds0 of
        #{L := [P]} ->
            Preds = Preds0#{L := [rename_label(P, From, To)]},
            merge_update_preds(Ls, From, To, Preds);
        #{} ->
            %% More than one predecessor, so this block will not be
            %% merged. Updating the predecessors is not needed and
            %% updating would waste a lot of time if there are many
            %% predecessors.
            merge_update_preds(Ls, From, To, Preds0)
    end;
merge_update_preds([], _, _, Preds) -> Preds.

merge_fix_succeeded(Is, #b_blk{last=#b_br{succ=Succ,fail=Fail}}) when Succ =/= Fail ->
    %% This is a two-way branch. There is no need look at the instructions.
    Is;
merge_fix_succeeded([_|_]=Is0, #b_blk{}) ->
    %% Not a two-way branch.
    case reverse(Is0) of
        [#b_set{op={succeeded,guard},args=[Dst]},#b_set{dst=Dst}|Is] ->
            %% This succeeded:guard instruction MUST be followed by a
            %% two-way branch. It is not, which means that its result
            %% will never be used. Therefore, the instruction and
            %% succeeded:guard must be removed.
            %%
            %% We remove those instructions for the benefit of the
            %% beam_ssa_bool pass. When called from beam_ssa_opt there
            %% should be no such instructions left.
            reverse(Is);
        _ ->
            Is0
    end;
merge_fix_succeeded(Is, _Blk) -> Is.

verify_merge_is([#b_set{op=Op}|_]) ->
    %% The merged block has only one predecessor, so it should not have any phi
    %% nodes.
    true = Op =/= phi;                          %Assertion.
verify_merge_is(_) ->
    ok.

is_merge_allowed(?EXCEPTION_BLOCK, #b_blk{}, #b_blk{}) ->
    false;
is_merge_allowed(_L, #b_blk{is=[#b_set{op=landingpad} | _]}, #b_blk{}) ->
    false;
is_merge_allowed(_L, #b_blk{}, #b_blk{is=[#b_set{op=landingpad} | _]}) ->
    false;
is_merge_allowed(L, #b_blk{}=Blk1, #b_blk{is=[#b_set{}=I|_]}=Blk2) ->
    not is_loop_header(I) andalso
        is_merge_allowed_1(L, Blk1, Blk2);
is_merge_allowed(L, Blk1, Blk2) ->
    is_merge_allowed_1(L, Blk1, Blk2).

is_merge_allowed_1(L, #b_blk{last=#b_br{}}=Blk, #b_blk{is=Is}) ->
    %% The predecessor block must have exactly one successor (L) for
    %% the merge to be safe.
    case successors(Blk) of
        [L] ->
            case Is of
                [#b_set{op=phi,args=[_]}|_] ->
                    %% The type optimizer pass must have been
                    %% turned off, since it would have removed this
                    %% redundant phi node. Refuse to merge the blocks
                    %% to ensure that this phi node remains at the
                    %% beginning of a block.
                    false;
                _ ->
                    true
            end;
        [_|_] ->
            false
    end;
is_merge_allowed_1(_, #b_blk{last=#b_switch{}}, #b_blk{}) ->
    false.

%% update_phi_labels([BlockLabel], Old, New, Blocks0) -> Blocks.
%%  In the given blocks, replace label Old in with New in all
%%  phi nodes. This is useful after merging or splitting
%%  blocks.

update_phi_labels([L|Ls], Old, New, Blocks0) ->
    case Blocks0 of
        #{L:=#b_blk{is=[#b_set{op=phi}|_]=Is0}=Blk0} ->
            Is = update_phi_labels_is(Is0, Old, New),
            Blk = Blk0#b_blk{is=Is},
            Blocks = Blocks0#{L:=Blk},
            update_phi_labels(Ls, Old, New, Blocks);
        #{L:=#b_blk{}} ->
            %% No phi nodes in this block.
            update_phi_labels(Ls, Old, New, Blocks0)
    end;
update_phi_labels([], _, _, Blocks) -> Blocks.
