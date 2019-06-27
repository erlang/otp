%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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
-export([add_anno/3,get_anno/2,get_anno/3,
         clobbers_xregs/1,def/2,def_used/2,
         definitions/1,
         dominators/1,common_dominators/3,
         flatmapfold_instrs_rpo/4,
         fold_po/3,fold_po/4,fold_rpo/3,fold_rpo/4,
         fold_instrs_rpo/4,
         linearize/1,
         mapfold_blocks_rpo/4,
         mapfold_instrs_rpo/4,
         normalize/1,
         no_side_effect/1,
         predecessors/1,
         rename_vars/3,
         rpo/1,rpo/2,
         split_blocks/3,
         successors/1,successors/2,
         trim_unreachable/1,
         update_phi_labels/4,used/1,
         uses/1,uses/2]).

-export_type([b_module/0,b_function/0,b_blk/0,b_set/0,
              b_ret/0,b_br/0,b_switch/0,terminator/0,
              b_var/0,b_literal/0,b_remote/0,b_local/0,
              value/0,argument/0,label/0,
              var_name/0,var_base/0,literal_value/0,
              op/0,anno/0,block_map/0,dominator_map/0,
              rename_map/0,rename_proplist/0,usage_map/0,
              definition_map/0]).

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

-type var_name()   :: var_base() | {var_base(),non_neg_integer()}.
-type var_base()   :: atom() | non_neg_integer().

-type literal_value() :: atom() | integer() | float() | list() |
                         nil() | tuple() | map() | binary().

-type op()   :: {'bif',atom()} | {'float',float_op()} | prim_op() | cg_prim_op().
-type anno() :: #{atom() := any()}.

-type block_map() :: #{label():=b_blk()}.
-type dominator_map() :: #{label():=[label()]}.
-type numbering_map() :: #{label():=non_neg_integer()}.
-type usage_map() :: #{b_var():=[{label(),b_set() | terminator()}]}.
-type definition_map() :: #{b_var():=b_set()}.
-type rename_map() :: #{b_var():=value()}.
-type rename_proplist() :: [{b_var(),value()}].

%% Note: By default, dialyzer will collapse this type to atom().
%% To avoid the collapsing, change the value of SET_LIMIT to 50 in the
%% file erl_types.erl in the hipe application.

-type prim_op() :: 'bs_add' | 'bs_extract' | 'bs_get_tail' |
                   'bs_init' | 'bs_init_writable' |
                   'bs_match' | 'bs_put' | 'bs_start_match' | 'bs_test_tail' |
                   'bs_utf16_size' | 'bs_utf8_size' | 'build_stacktrace' |
                   'call' | 'catch_end' |
                   'extract' |
                   'get_hd' | 'get_map_element' | 'get_tl' | 'get_tuple_element' |
                   'has_map_field' |
                   'is_nonempty_list' | 'is_tagged_tuple' |
                   'kill_try_tag' |
                   'landingpad' |
                   'make_fun' | 'new_try_tag' |
                   'peek_message' | 'phi' | 'put_list' | 'put_map' | 'put_tuple' |
                   'raw_raise' | 'recv_next' | 'remove_message' | 'resume' |
                   'succeeded' |
                   'timeout' |
                   'wait' | 'wait_timeout'.

-type float_op() :: 'checkerror' | 'clearerror' | 'convert' | 'get' | 'put' |
                    '+' | '-' | '*' | '/'.

%% Primops only used internally during code generation.
-type cg_prim_op() :: 'bs_get' | 'bs_get_position' | 'bs_match_string' |
                      'bs_restore' | 'bs_save' | 'bs_set_position' | 'bs_skip' |
                      'copy' | 'put_tuple_arity' | 'put_tuple_element' |
                      'put_tuple_elements' | 'set_tuple_element'.

-import(lists, [foldl/3,keyfind/3,mapfoldl/3,member/2,reverse/1,umerge/1]).

-spec add_anno(Key, Value, Construct) -> Construct when
      Key :: atom(),
      Value :: any(),
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

-spec get_anno(atom(), construct(),any()) -> any().

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
        make_fun -> true;
        peek_message -> true;
        raw_raise -> true;
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
        bs_init -> true;
        bs_extract -> true;
        bs_match -> true;
        bs_start_match -> true;
        bs_test_tail -> true;
        bs_get_tail -> true;
        bs_put -> true;
        extract -> true;
        get_hd -> true;
        get_tl -> true;
        get_map_element -> true;
        get_tuple_element -> true;
        has_map_field -> true;
        is_nonempty_list -> true;
        is_tagged_tuple -> true;
        make_fun -> true;
        put_map -> true;
        put_list -> true;
        put_tuple -> true;
        succeeded -> true;
        _ -> false
    end.

-spec predecessors(Blocks) -> #{BlockNumber:=[Predecessor]} when
      Blocks :: block_map(),
      BlockNumber :: label(),
      Predecessor :: label().

predecessors(Blocks) ->
    P0 = [{S,L} || {L,Blk} <- maps:to_list(Blocks),
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
            [Fail|[L || {_,L} <- List]];
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

normalize(#b_set{op={bif,Bif},args=Args}=Set) ->
    case {is_commutative(Bif),Args} of
        {false,_} ->
            Set;
        {true,[#b_literal{}=Lit,#b_var{}=Var]} ->
            Set#b_set{args=[Var,Lit]};
        {true,_} ->
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
            case keyfind(Arg, 1, List) of
                false ->
                    #b_br{bool=#b_literal{val=true},succ=Fail,fail=Fail};
                {Arg,L} ->
                    #b_br{bool=#b_literal{val=true},succ=L,fail=L}
            end;
        #b_var{} when List =:= [] ->
            #b_br{bool=#b_literal{val=true},succ=Fail,fail=Fail};
        #b_var{} ->
            Sw
    end;
normalize(#b_ret{}=Ret) ->
    Ret.

-spec successors(label(), block_map()) -> [label()].

successors(L, Blocks) ->
    successors(map_get(L, Blocks)).

-spec def(Ls, Blocks) -> Def when
      Ls :: [label()],
      Blocks :: block_map(),
      Def :: ordsets:ordset(var_name()).

def(Ls, Blocks) ->
    Top = rpo(Ls, Blocks),
    Blks = [map_get(L, Blocks) || L <- Top],
    def_1(Blks, []).

-spec def_used(Ls, Blocks) -> {Def,Used} when
      Ls :: [label()],
      Blocks :: block_map(),
      Def :: ordsets:ordset(var_name()),
      Used :: ordsets:ordset(var_name()).

def_used(Ls, Blocks) ->
    Top = rpo(Ls, Blocks),
    Blks = [map_get(L, Blocks) || L <- Top],
    Preds = cerl_sets:from_list(Top),
    def_used_1(Blks, Preds, [], []).

%% dominators(BlockMap) -> {Dominators,Numbering}.
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

-spec dominators(Blocks) -> Result when
      Blocks :: block_map(),
      Result :: {dominator_map(), numbering_map()}.
dominators(Blocks) ->
    Preds = predecessors(Blocks),
    Top0 = rpo(Blocks),
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
common_dominators(Ls, Dom, Numbering) ->
    Doms = [map_get(L, Dom) || L <- Ls],
    dom_intersection(Doms, Numbering).

-spec fold_instrs_rpo(Fun, From, Acc0, Blocks) -> any() when
      Fun :: fun((b_blk()|terminator(), any()) -> any()),
      From :: [label()],
      Acc0 :: any(),
      Blocks :: block_map().

fold_instrs_rpo(Fun, From, Acc0, Blocks) ->
    Top = rpo(From, Blocks),
    fold_instrs_rpo_1(Top, Fun, Blocks, Acc0).

%% Like mapfold_instrs_rpo but at the block level to support lookahead and
%% scope-dependent transformations.
-spec mapfold_blocks_rpo(Fun, From, Acc, Blocks) -> Result when
      Fun :: fun((label(), b_blk(), any()) -> {b_blk(), any()}),
      From :: [label()],
      Acc :: any(),
      Blocks :: block_map(),
      Result :: {block_map(), any()}.
mapfold_blocks_rpo(Fun, From, Acc, Blocks) ->
    Successors = rpo(From, Blocks),
    foldl(fun(Lbl, A) ->
                  mapfold_blocks_rpo_1(Fun, Lbl, A)
          end, {Blocks, Acc}, Successors).

mapfold_blocks_rpo_1(Fun, Lbl, {Blocks0, Acc0}) ->
    Block0 = map_get(Lbl, Blocks0),
    {Block, Acc} = Fun(Lbl, Block0, Acc0),
    Blocks = Blocks0#{Lbl:=Block},
    {Blocks, Acc}.

-spec mapfold_instrs_rpo(Fun, From, Acc0, Blocks0) -> {Blocks,Acc} when
      Fun :: fun((b_blk()|terminator(), any()) -> any()),
      From :: [label()],
      Acc0 :: any(),
      Acc :: any(),
      Blocks0 :: block_map(),
      Blocks :: block_map().

mapfold_instrs_rpo(Fun, From, Acc0, Blocks) ->
    Top = rpo(From, Blocks),
    mapfold_instrs_rpo_1(Top, Fun, Blocks, Acc0).

-spec flatmapfold_instrs_rpo(Fun, From, Acc0, Blocks0) -> {Blocks,Acc} when
      Fun :: fun((b_blk()|terminator(), any()) -> any()),
      From :: [label()],
      Acc0 :: any(),
      Acc :: any(),
      Blocks0 :: block_map(),
      Blocks :: block_map().

flatmapfold_instrs_rpo(Fun, From, Acc0, Blocks) ->
    Top = rpo(From, Blocks),
    flatmapfold_instrs_rpo_1(Top, Fun, Blocks, Acc0).

-type fold_fun() :: fun((label(), b_blk(), any()) -> any()).

%% fold_rpo(Fun, [Label], Acc0, Blocks) -> Acc.
%%  Fold over all blocks a reverse postorder traversal of the block
%%  graph; that is, first visit a block, then visit its successors.

-spec fold_rpo(Fun, Acc0, Blocks) -> any() when
      Fun :: fold_fun(),
      Acc0 :: any(),
      Blocks :: #{label():=b_blk()}.

fold_rpo(Fun, Acc0, Blocks) ->
    fold_rpo(Fun, [0], Acc0, Blocks).

%% fold_rpo(Fun, [Label], Acc0, Blocks) -> Acc.  Fold over all blocks
%%  reachable from a given set of labels in a reverse postorder
%%  traversal of the block graph; that is, first visit a block, then
%%  visit its successors.

-spec fold_rpo(Fun, Labels, Acc0, Blocks) -> any() when
      Fun :: fold_fun(),
      Labels :: [label()],
      Acc0 :: any(),
      Blocks :: #{label():=b_blk()}.

fold_rpo(Fun, From, Acc0, Blocks) ->
    Top = rpo(From, Blocks),
    fold_rpo_1(Top, Fun, Blocks, Acc0).

%% fold_po(Fun, Acc0, Blocks) -> Acc.
%%  Fold over all blocks in a postorder traversal of the block graph;
%%  that is, first visit all successors of block, then the block
%%  itself.

-spec fold_po(Fun, Acc0, Blocks) -> any() when
      Fun :: fold_fun(),
      Acc0 :: any(),
      Blocks :: #{label():=b_blk()}.

%% fold_po(Fun, From, Acc0, Blocks) -> Acc.
%%  Fold over the blocks reachable from the block numbers given
%%  by From in a postorder traversal of the block graph.

fold_po(Fun, Acc0, Blocks) ->
    fold_po(Fun, [0], Acc0, Blocks).

-spec fold_po(Fun, Labels, Acc0, Blocks) -> any() when
      Fun :: fold_fun(),
      Labels :: [label()],
      Acc0 :: any(),
      Blocks :: block_map().

fold_po(Fun, From, Acc0, Blocks) ->
    Top = reverse(rpo(From, Blocks)),
    fold_rpo_1(Top, Fun, Blocks, Acc0).

%% linearize(Blocks) -> [{BlockLabel,#b_blk{}}].
%%  Linearize the intermediate representation of the code.
%%  Unreachable blocks will be discarded, and phi nodes will
%%  be adjusted so that they no longer refers to discarded
%%  blocks or to blocks that no longer are predecessors of
%%  the phi node block.

-spec linearize(Blocks) -> Linear when
      Blocks :: block_map(),
      Linear :: [{label(),b_blk()}].

linearize(Blocks) ->
    Seen = cerl_sets:new(),
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

rpo(From, Blocks) ->
    Seen = cerl_sets:new(),
    {Ls,_} = rpo_1(From, Blocks, Seen, []),
    Ls.

-spec rename_vars(Rename, [label()], block_map()) -> block_map() when
      Rename :: rename_map() | rename_proplist().

rename_vars(Rename, From, Blocks) when is_list(Rename) ->
    rename_vars(maps:from_list(Rename), From, Blocks);
rename_vars(Rename, From, Blocks) when is_map(Rename)->
    Top = rpo(From, Blocks),
    Preds = cerl_sets:from_list(Top),
    F = fun(#b_set{op=phi,args=Args0}=Set) ->
                Args = rename_phi_vars(Args0, Preds, Rename),
                Set#b_set{args=Args};
           (#b_set{args=Args0}=Set) ->
                Args = [rename_var(A, Rename) || A <- Args0],
                Set#b_set{args=Args};
           (#b_switch{arg=Bool}=Sw) ->
                Sw#b_switch{arg=rename_var(Bool, Rename)};
           (#b_br{bool=Bool}=Br) ->
                Br#b_br{bool=rename_var(Bool, Rename)};
           (#b_ret{arg=Arg}=Ret) ->
                Ret#b_ret{arg=rename_var(Arg, Rename)}
        end,
    map_instrs_1(Top, F, Blocks).

%% split_blocks(Predicate, Blocks0, Count0) -> {Blocks,Count}.
%%  Call Predicate(Instruction) for each instruction in all
%%  blocks. If Predicate/1 returns true, split the block
%%  before this instruction.

-spec split_blocks(Pred, Blocks0, Count0) -> {Blocks,Count} when
      Pred :: fun((b_set()) -> boolean()),
      Blocks :: block_map(),
      Count0 :: beam_ssa:label(),
      Blocks0 :: block_map(),
      Blocks :: block_map(),
      Count :: beam_ssa:label().

split_blocks(P, Blocks, Count) ->
    Ls = beam_ssa:rpo(Blocks),
    split_blocks_1(Ls, P, Blocks, Count).

-spec trim_unreachable(Blocks0) -> Blocks when
      Blocks0 :: block_map(),
      Blocks :: block_map().

%% trim_unreachable(Blocks0) -> Blocks.
%%  Remove all unreachable blocks. Adjust all phi nodes so
%%  they don't refer to blocks that has been removed or no
%%  no longer branch to the phi node in question.

trim_unreachable(Blocks) ->
    %% Could perhaps be optimized if there is any need.
    maps:from_list(linearize(Blocks)).

%% update_phi_labels([BlockLabel], Old, New, Blocks0) -> Blocks.
%%  In the given blocks, replace label Old in with New in all
%%  phi nodes. This is useful after merging or splitting
%%  blocks.

-spec update_phi_labels(From, Old, New, Blocks0) -> Blocks when
      From :: [label()],
      Old :: label(),
      New :: label(),
      Blocks0 :: block_map(),
      Blocks :: block_map().

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

-spec used(b_blk() | b_set() | terminator()) -> [var_name()].

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

-spec definitions(Blocks :: block_map()) -> definition_map().
definitions(Blocks) ->
    fold_instrs_rpo(fun(#b_set{ dst = Var }=I, Acc) ->
                            Acc#{Var => I};
                       (_Terminator, Acc) ->
                            Acc
                    end, [0], #{}, Blocks).

-spec uses(Blocks :: block_map()) -> usage_map().
uses(Blocks) ->
    uses([0], Blocks).

-spec uses(From, Blocks) -> usage_map() when
      From :: [label()],
      Blocks :: block_map().
uses(From, Blocks) ->
    fold_rpo(fun fold_uses_block/3, From, #{}, Blocks).

fold_uses_block(Lbl, #b_blk{is=Is,last=Last}, UseMap0) ->
    F = fun(I, UseMap) ->
                foldl(fun(Var, Acc) ->
                              Uses0 = maps:get(Var, Acc, []),
                              Uses = [{Lbl, I} | Uses0],
                              maps:put(Var, Uses, Acc)
                      end, UseMap, used(I))
        end,
    F(Last, foldl(F, UseMap0, Is)).

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

def_used_1([#b_blk{is=Is,last=Last}|Bs], Preds, Def0, UsedAcc) ->
    {Def,Used} = def_used_is(Is, Preds, Def0, used(Last)),
    case Used of
        [] ->
            def_used_1(Bs, Preds, Def, UsedAcc);
        [_|_] ->
            def_used_1(Bs, Preds, Def, [Used|UsedAcc])
    end;
def_used_1([], _Preds, Def0, UsedAcc) ->
    Def = ordsets:from_list(Def0),
    Used = umerge(UsedAcc),
    {Def,Used}.

def_used_is([#b_set{op=phi,dst=Dst,args=Args}|Is],
            Preds, Def0, Used0) ->
    Def = [Dst|Def0],
    %% We must be careful to only include variables that will
    %% be used when arriving from one of the predecessor blocks
    %% in Preds.
    Used1 = [V || {#b_var{}=V,L} <- Args, cerl_sets:is_element(L, Preds)],
    Used = ordsets:union(ordsets:from_list(Used1), Used0),
    def_used_is(Is, Preds, Def, Used);
def_used_is([#b_set{dst=Dst}=I|Is], Preds, Def0, Used0) ->
    Def = [Dst|Def0],
    Used = ordsets:union(used(I), Used0),
    def_used_is(Is, Preds, Def, Used);
def_used_is([], _Preds, Def, Used) ->
    {Def,Used}.

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

dom_intersection(S1, [S2|Ss], Df) ->
    dom_intersection(dom_intersection_1(S1, S2, Df), Ss, Df);
dom_intersection(S, [], _Df) -> S.

dom_intersection_1([E1|Es1]=Set1, [E2|Es2]=Set2, Df) ->
    %% Blocks are numbered in the order they are found in
    %% reverse postorder.
    #{E1:=Df1,E2:=Df2} = Df,
    if Df1 > Df2 ->
            dom_intersection_1(Es1, Set2, Df);
       Df2 > Df1 ->
            dom_intersection_1(Es2, Set1, Df);  %switch arguments!
       true ->                                  %Set1 == Set2
            %% The common suffix of the sets is the intersection.
            Set1
    end.

number([L|Ls], N) ->
    [{L,N}|number(Ls, N+1)];
number([], _) -> [].

fold_rpo_1([L|Ls], Fun, Blocks, Acc0) ->
    Block = map_get(L, Blocks),
    Acc = Fun(L, Block, Acc0),
    fold_rpo_1(Ls, Fun, Blocks, Acc);
fold_rpo_1([], _, _, Acc) -> Acc.

fold_instrs_rpo_1([L|Ls], Fun, Blocks, Acc0) ->
    #b_blk{is=Is,last=Last} = map_get(L, Blocks),
    Acc1 = foldl(Fun, Acc0, Is),
    Acc = Fun(Last, Acc1),
    fold_instrs_rpo_1(Ls, Fun, Blocks, Acc);
fold_instrs_rpo_1([], _, _, Acc) -> Acc.

mapfold_instrs_rpo_1([L|Ls], Fun, Blocks0, Acc0) ->
    #b_blk{is=Is0,last=Last0} = Block0 = map_get(L, Blocks0),
    {Is,Acc1} = mapfoldl(Fun, Acc0, Is0),
    {Last,Acc} = Fun(Last0, Acc1),
    Block = Block0#b_blk{is=Is,last=Last},
    Blocks = Blocks0#{L:=Block},
    mapfold_instrs_rpo_1(Ls, Fun, Blocks, Acc);
mapfold_instrs_rpo_1([], _, Blocks, Acc) ->
    {Blocks,Acc}.

flatmapfold_instrs_rpo_1([L|Ls], Fun, Blocks0, Acc0) ->
    #b_blk{is=Is0,last=Last0} = Block0 = map_get(L, Blocks0),
    {Is,Acc1} = flatmapfoldl(Fun, Acc0, Is0),
    {[Last],Acc} = Fun(Last0, Acc1),
    Block = Block0#b_blk{is=Is,last=Last},
    Blocks = Blocks0#{L:=Block},
    flatmapfold_instrs_rpo_1(Ls, Fun, Blocks, Acc);
flatmapfold_instrs_rpo_1([], _, Blocks, Acc) ->
    {Blocks,Acc}.

linearize_1([L|Ls], Blocks, Seen0, Acc0) ->
    case cerl_sets:is_element(L, Seen0) of
        true ->
            linearize_1(Ls, Blocks, Seen0, Acc0);
        false ->
            Seen1 = cerl_sets:add_element(L, Seen0),
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
    Args = [{Val,Pred} || {Val,Pred} <- Args0,
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

rpo_1([L|Ls], Blocks, Seen0, Acc0) ->
    case cerl_sets:is_element(L, Seen0) of
        true ->
            rpo_1(Ls, Blocks, Seen0, Acc0);
        false ->
            Block = map_get(L, Blocks),
            Seen1 = cerl_sets:add_element(L, Seen0),
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
    case cerl_sets:is_element(L, Preds) of
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

split_blocks_1([L|Ls], P, Blocks0, Count0) ->
    #b_blk{is=Is0} = Blk = map_get(L, Blocks0),
    case split_blocks_is(Is0, P, []) of
        {yes,Bef,Aft} ->
            NewLbl = Count0,
            Count = Count0 + 1,
            Br = #b_br{bool=#b_literal{val=true},succ=NewLbl,fail=NewLbl},
            BefBlk = Blk#b_blk{is=Bef,last=Br},
            NewBlk = Blk#b_blk{is=Aft},
            Blocks1 = Blocks0#{L:=BefBlk,NewLbl=>NewBlk},
            Successors = successors(NewBlk),
            Blocks = update_phi_labels(Successors, L, NewLbl, Blocks1),
            split_blocks_1([NewLbl|Ls], P, Blocks, Count);
        no ->
            split_blocks_1(Ls, P, Blocks0, Count0)
    end;
split_blocks_1([], _, Blocks, Count) ->
    {Blocks,Count}.

split_blocks_is([I|Is], P, []) ->
    split_blocks_is(Is, P, [I]);
split_blocks_is([I|Is], P, Acc) ->
    case P(I) of
        true ->
            {yes,reverse(Acc),[I|Is]};
        false ->
            split_blocks_is(Is, P, [I|Acc])
    end;
split_blocks_is([], _, _) -> no.

update_phi_labels_is([#b_set{op=phi,args=Args0}=I0|Is], Old, New) ->
    Args = [{Arg,rename_label(Lbl, Old, New)} || {Arg,Lbl} <- Args0],
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
