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
-export([add_anno/3,get_anno/2,
         clobbers_xregs/1,def/2,def_used/2,dominators/1,
         flatmapfold_instrs_rpo/4,
         fold_po/3,fold_po/4,fold_rpo/3,fold_rpo/4,
         fold_instrs_rpo/4,
         linearize/1,
         mapfold_instrs_rpo/4,
         normalize/1,
         predecessors/1,
         rename_vars/3,
         rpo/1,rpo/2,
         split_blocks/3,
         successors/1,successors/2,
         trim_unreachable/1,
         update_phi_labels/4,used/1]).

-export_type([b_module/0,b_function/0,b_blk/0,b_set/0,
              b_ret/0,b_br/0,b_switch/0,terminator/0,
              b_var/0,b_literal/0,b_remote/0,b_local/0,
              value/0,argument/0,label/0,
              var_name/0,var_base/0,literal_value/0,
              op/0,anno/0,block_map/0]).

-include("beam_ssa.hrl").

-type b_module()   :: #b_module{}.
-type b_function() :: #b_function{}.
-type b_blk()      :: #b_blk{}.
-type b_set()      :: #b_set{}.

-type b_br()       :: #b_br{}.
-type b_ret()      :: #b_ret{}.
-type b_switch()   :: #b_switch{}.
-type terminator() :: b_br() | b_ret() | b_switch().

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

%% Note: By default, dialyzer will collapse this type to atom().
%% To avoid the collapsing, change the value of SET_LIMIT to 50 in the
%% file erl_types.erl in the hipe application.

-type prim_op() :: 'bs_add' | 'bs_extract' | 'bs_init' | 'bs_init_writable' |
                   'bs_match' | 'bs_put' | 'bs_start_match' | 'bs_test_tail' |
                   'bs_utf16_size' | 'bs_utf8_size' | 'build_stacktrace' |
                   'call' | 'catch_end' | 'context_to_binary' |
                   'extract' |
                   'get_hd' | 'get_map_element' | 'get_tl' | 'get_tuple_element' |
                   'has_map_field' |
                   'is_nonempty_list' | 'is_tagged_tuple' |
                   'kill_try_tag' |
                   'landingpad' |
                   'make_fun' | 'new_try_tag' |
                   'peek_message' | 'phi' | 'put_list' | 'put_map' | 'put_tuple' |
                   'raw_raise' | 'recv_next' | 'remove_message' | 'resume' |
                   'set_tuple_element' | 'succeeded' |
                   'timeout' |
                   'wait' | 'wait_timeout'.

-type float_op() :: 'checkerror' | 'clearerror' | 'convert' | 'get' | 'put' |
                    '+' | '-' | '*' | '/'.

%% Primops only used internally during code generation.
-type cg_prim_op() :: 'bs_get' | 'bs_match_string' | 'bs_restore' | 'bs_skip' |
'copy' | 'put_tuple_arity' | 'put_tuple_element'.

-import(lists, [foldl/3,keyfind/3,mapfoldl/3,member/2,reverse/1]).

-spec add_anno(Key, Value, Construct) -> Construct when
      Key :: atom(),
      Value :: any(),
      Construct :: b_function() | b_blk() | b_set() | terminator().

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

-spec get_anno(atom(), b_blk()|b_set()|terminator()) -> any().

get_anno(Key, Construct) ->
    maps:get(Key, get_anno(Construct)).

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
    successors(maps:get(L, Blocks)).

-spec def(Ls, Blocks) -> Def when
      Ls :: [label()],
      Blocks :: block_map(),
      Def :: ordsets:ordset(var_name()).

def(Ls, Blocks) ->
    Top = rpo(Ls, Blocks),
    Blks = [maps:get(L, Blocks) || L <- Top],
    def_1(Blks, []).

-spec def_used(Ls, Blocks) -> {Def,Used} when
      Ls :: [label()],
      Blocks :: block_map(),
      Def :: ordsets:ordset(var_name()),
      Used :: ordsets:ordset(var_name()).

def_used(Ls, Blocks) ->
    Top = rpo(Ls, Blocks),
    Blks = [maps:get(L, Blocks) || L <- Top],
    Preds = gb_sets:from_list(Top),
    def_used_1(Blks, Preds, [], gb_sets:empty()).

-spec dominators(Blocks) -> Result when
      Blocks :: block_map(),
      Result :: #{label():=ordsets:ordset(label())}.

dominators(Blocks) ->
    Preds = predecessors(Blocks),
    Top0 = rpo(Blocks),
    Top = [{L,maps:get(L, Preds)} || L <- Top0],

    %% The flow graph for an Erlang function is reducible, and
    %% therefore one traversal in reverse postorder is sufficient.
    iter_dominators(Top, #{}).

-spec fold_instrs_rpo(Fun, From, Acc0, Blocks) -> any() when
      Fun :: fun((b_blk()|terminator(), any()) -> any()),
      From :: [label()],
      Acc0 :: any(),
      Blocks :: block_map().

fold_instrs_rpo(Fun, From, Acc0, Blocks) ->
    Top = rpo(From, Blocks),
    fold_instrs_rpo_1(Top, Fun, Blocks, Acc0).

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
    Seen = gb_sets:empty(),
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
    Seen = gb_sets:empty(),
    {Ls,_} = rpo_1(From, Blocks, Seen, []),
    Ls.

-spec rename_vars(Rename, [label()], block_map()) -> block_map() when
      Rename :: [{var_name(),value()}] | #{var_name():=value()}.

rename_vars(Rename, From, Blocks) when is_list(Rename) ->
    rename_vars(maps:from_list(Rename), From, Blocks);
rename_vars(Rename, From, Blocks) when is_map(Rename)->
    Top = rpo(From, Blocks),
    Preds = gb_sets:from_list(Top),
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
used(#b_br{bool=#b_var{name=V}}) ->
    [V];
used(#b_ret{arg=#b_var{name=V}}) ->
    [V];
used(#b_set{op=phi,args=Args}) ->
    ordsets:from_list([V || {#b_var{name=V},_} <- Args]);
used(#b_set{args=Args}) ->
    ordsets:from_list(used_args(Args));
used(#b_switch{arg=#b_var{name=V}}) ->
    [V];
used(_) -> [].

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

def_used_1([#b_blk{is=Is,last=Last}|Bs], Preds, Def0, Used0) ->
    {Def,Used1} = def_used_is(Is, Preds, Def0, Used0),
    Used = gb_sets:union(gb_sets:from_list(used(Last)), Used1),
    def_used_1(Bs, Preds, Def, Used);
def_used_1([], _Preds, Def, Used) ->
    {ordsets:from_list(Def),gb_sets:to_list(Used)}.

def_used_is([#b_set{op=phi,dst=#b_var{name=Dst},args=Args}|Is],
            Preds, Def0, Used0) ->
    Def = [Dst|Def0],
    %% We must be careful to only include variables that will
    %% be used when arriving from one of the predecessor blocks
    %% in Preds.
    Used1 = [V || {#b_var{name=V},L} <- Args,
                  gb_sets:is_member(L, Preds)],
    Used = gb_sets:union(gb_sets:from_list(Used1), Used0),
    def_used_is(Is, Preds, Def, Used);
def_used_is([#b_set{dst=#b_var{name=Dst}}=I|Is], Preds, Def0, Used0) ->
    Def = [Dst|Def0],
    Used = gb_sets:union(gb_sets:from_list(used(I)), Used0),
    def_used_is(Is, Preds, Def, Used);
def_used_is([], _Preds, Def, Used) ->
    {Def,Used}.

def_1([#b_blk{is=Is}|Bs], Def0) ->
    Def = def_is(Is, Def0),
    def_1(Bs, Def);
def_1([], Def) ->
    ordsets:from_list(Def).

def_is([#b_set{dst=#b_var{name=Dst}}|Is], Def) ->
    def_is(Is, [Dst|Def]);
def_is([], Def) -> Def.

iter_dominators([{0,[]}|Ls], _Doms) ->
    Dom = [0],
    iter_dominators(Ls, #{0=>Dom});
iter_dominators([{L,Preds}|Ls], Doms) ->
    DomPreds = [maps:get(P, Doms) || P <- Preds, maps:is_key(P, Doms)],
    Dom = ordsets:add_element(L, ordsets:intersection(DomPreds)),
    iter_dominators(Ls, Doms#{L=>Dom});
iter_dominators([], Doms) -> Doms.

fold_rpo_1([L|Ls], Fun, Blocks, Acc0) ->
    Block = maps:get(L, Blocks),
    Acc = Fun(L, Block, Acc0),
    fold_rpo_1(Ls, Fun, Blocks, Acc);
fold_rpo_1([], _, _, Acc) -> Acc.

fold_instrs_rpo_1([L|Ls], Fun, Blocks, Acc0) ->
    #b_blk{is=Is,last=Last} = maps:get(L, Blocks),
    Acc1 = foldl(Fun, Acc0, Is),
    Acc = Fun(Last, Acc1),
    fold_instrs_rpo_1(Ls, Fun, Blocks, Acc);
fold_instrs_rpo_1([], _, _, Acc) -> Acc.

mapfold_instrs_rpo_1([L|Ls], Fun, Blocks0, Acc0) ->
    #b_blk{is=Is0,last=Last0} = Block0 = maps:get(L, Blocks0),
    {Is,Acc1} = mapfoldl(Fun, Acc0, Is0),
    {Last,Acc} = Fun(Last0, Acc1),
    Block = Block0#b_blk{is=Is,last=Last},
    Blocks = maps:put(L, Block, Blocks0),
    mapfold_instrs_rpo_1(Ls, Fun, Blocks, Acc);
mapfold_instrs_rpo_1([], _, Blocks, Acc) ->
    {Blocks,Acc}.

flatmapfold_instrs_rpo_1([L|Ls], Fun, Blocks0, Acc0) ->
    #b_blk{is=Is0,last=Last0} = Block0 = maps:get(L, Blocks0),
    {Is,Acc1} = flatmapfoldl(Fun, Acc0, Is0),
    {[Last],Acc} = Fun(Last0, Acc1),
    Block = Block0#b_blk{is=Is,last=Last},
    Blocks = maps:put(L, Block, Blocks0),
    flatmapfold_instrs_rpo_1(Ls, Fun, Blocks, Acc);
flatmapfold_instrs_rpo_1([], _, Blocks, Acc) ->
    {Blocks,Acc}.

linearize_1([L|Ls], Blocks, Seen0, Acc0) ->
    case gb_sets:is_member(L, Seen0) of
        true ->
            linearize_1(Ls, Blocks, Seen0, Acc0);
        false ->
            Seen1 = gb_sets:insert(L, Seen0),
            Block = maps:get(L, Blocks),
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
    case gb_sets:is_member(L, Seen0) of
        true ->
            rpo_1(Ls, Blocks, Seen0, Acc0);
        false ->
            Block = maps:get(L, Blocks),
            Seen1 = gb_sets:insert(L, Seen0),
            Successors = successors(Block),
            {Acc,Seen} = rpo_1(Successors, Blocks, Seen1, Acc0),
            rpo_1(Ls, Blocks, Seen, [L|Acc])
    end;
rpo_1([], _, Seen, Acc) ->
    {Acc,Seen}.

rename_var(#b_var{name=Old}=V, Rename) ->
    case Rename of
        #{Old:=New} -> New;
        #{} -> V
    end;
rename_var(#b_remote{mod=Mod0,name=Name0}=Remote, Rename) ->
    Mod = rename_var(Mod0, Rename),
    Name = rename_var(Name0, Rename),
    Remote#b_remote{mod=Mod,name=Name};
rename_var(Old, _) -> Old.

rename_phi_vars([{Var,L}|As], Preds, Ren) ->
    case gb_sets:is_member(L, Preds) of
        true ->
            [{rename_var(Var, Ren),L}|rename_phi_vars(As, Preds, Ren)];
        false ->
            [{Var,L}|rename_phi_vars(As, Preds, Ren)]
    end;
rename_phi_vars([], _, _) -> [].

map_instrs_1([L|Ls], Fun, Blocks0) ->
    #b_blk{is=Is0,last=Last0} = Blk0 = maps:get(L, Blocks0),
    Is = [Fun(I) || I <- Is0],
    Last = Fun(Last0),
    Blk = Blk0#b_blk{is=Is,last=Last},
    Blocks = maps:put(L, Blk, Blocks0),
    map_instrs_1(Ls, Fun, Blocks);
map_instrs_1([], _, Blocks) -> Blocks.

flatmapfoldl(F, Accu0, [Hd|Tail]) ->
    {R,Accu1} = F(Hd, Accu0),
    {Rs,Accu2} = flatmapfoldl(F, Accu1, Tail),
    {R++Rs,Accu2};
flatmapfoldl(_, Accu, []) -> {[],Accu}.

split_blocks_1([L|Ls], P, Blocks0, Count0) ->
    #b_blk{is=Is0} = Blk = maps:get(L, Blocks0),
    case split_blocks_is(Is0, P, []) of
        {yes,Bef,Aft} ->
            NewLbl = Count0,
            Count = Count0 + 1,
            Br = #b_br{bool=#b_literal{val=true},succ=NewLbl,fail=NewLbl},
            BefBlk = Blk#b_blk{is=Bef,last=Br},
            NewBlk = Blk#b_blk{is=Aft},
            Blocks1 = Blocks0#{L:=BefBlk,NewLbl=>NewBlk},
            Successors = beam_ssa:successors(NewBlk),
            Blocks = beam_ssa:update_phi_labels(Successors, L, NewLbl, Blocks1),
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

used_args([#b_var{name=V}|As]) ->
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
