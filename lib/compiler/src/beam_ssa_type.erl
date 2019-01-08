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

-module(beam_ssa_type).
-export([opt/2]).

-include("beam_ssa.hrl").
-import(lists, [all/2,any/2,droplast/1,foldl/3,last/1,member/2,
                reverse/1,sort/1]).

-define(UNICODE_INT, #t_integer{elements={0,16#10FFFF}}).

-record(d, {ds :: #{beam_ssa:b_var():=beam_ssa:b_set()},
            ls :: #{beam_ssa:label():=type_db()},
            once :: cerl_sets:set(beam_ssa:b_var()),
            sub :: #{beam_ssa:b_var():=beam_ssa:value()}
           }).

-define(ATOM_SET_SIZE, 5).

%% Records that represent type information.
-record(t_atom, {elements=any :: 'any' | [atom()]}).
-record(t_integer, {elements=any :: 'any' | {integer(),integer()}}).
-record(t_bs_match, {type :: type()}).
-record(t_tuple, {size=0 :: integer(),
                  exact=false :: boolean(),
                  elements=[] :: [any()]
                 }).

-type type() :: 'any' | 'none' |
                #t_atom{} | #t_integer{} | #t_bs_match{} | #t_tuple{} |
                {'binary',pos_integer()} | 'cons' | 'float' | 'list' | 'map' | 'nil' |'number'.
-type type_db() :: #{beam_ssa:var_name():=type()}.

-spec opt([{Label0,Block0}], Args) -> [{Label,Block}] when
      Label0 :: beam_ssa:label(),
      Block0 :: beam_ssa:b_blk(),
      Args :: [beam_ssa:b_var()],
      Label :: beam_ssa:label(),
      Block :: beam_ssa:b_blk().

opt(Linear, Args) ->
    UsedOnce = used_once(Linear, Args),
    Ts = maps:from_list([{V,any} || #b_var{}=V <- Args]),
    FakeCall = #b_set{op=call,args=[#b_remote{mod=#b_literal{val=unknown},
                                              name=#b_literal{val=unknown},
                                              arity=0}]},
    Defs = maps:from_list([{Var,FakeCall#b_set{dst=Var}} ||
                              #b_var{}=Var <- Args]),
    D = #d{ds=Defs,ls=#{0=>Ts,?BADARG_BLOCK=>#{}},
           once=UsedOnce,sub=#{}},
    opt_1(Linear, D).

opt_1([{L,Blk}|Bs], #d{ls=Ls}=D) ->
    case Ls of
        #{L:=Ts} ->
            opt_2(L, Blk, Bs, Ts, D);
        #{} ->
            %% This block is never reached. Discard it.
            opt_1(Bs, D)
    end;
opt_1([], #d{}) -> [].

opt_2(L, #b_blk{is=Is0}=Blk0, Bs, Ts, #d{sub=Sub}=D0) ->
    case Is0 of
        [#b_set{op=call,dst=Dst,
                args=[#b_remote{mod=#b_literal{val=Mod},
                                name=#b_literal{val=Name}}=Rem|Args0]}=I0] ->
            case erl_bifs:is_exit_bif(Mod, Name, length(Args0)) of
                true ->
                    %% This call will never reach the successor block.
                    %% Rewrite the terminator to a 'ret', and remove
                    %% all type information for this label. That will
                    %% simplify the phi node in the former successor.
                    Args = simplify_args(Args0, Sub, Ts),
                    I = I0#b_set{args=[Rem|Args]},
                    Ret = #b_ret{arg=Dst},
                    Blk = Blk0#b_blk{is=[I],last=Ret},
                    Ls = maps:remove(L, D0#d.ls),
                    D = D0#d{ls=Ls},
                    [{L,Blk}|opt_1(Bs, D)];
                false ->
                    opt_3(L, Blk0, Bs, Ts, D0)
            end;
        _ ->
            opt_3(L, Blk0, Bs, Ts, D0)
    end.

opt_3(L, #b_blk{is=Is0,last=Last0}=Blk0, Bs, Ts0,
      #d{ds=Ds0,ls=Ls0,sub=Sub0}=D0) ->
    {Is,Ts,Ds,Sub} = opt_is(Is0, Ts0, Ds0, Ls0, Sub0, []),
    D1 = D0#d{ds=Ds,sub=Sub},
    Last1 = simplify_terminator(Last0, Sub, Ts),
    Last = opt_terminator(Last1, Ts, Ds),
    D = update_successors(Last, Ts, D1),
    Blk = Blk0#b_blk{is=Is,last=Last},
    [{L,Blk}|opt_1(Bs, D)].

simplify_terminator(#b_br{bool=Bool}=Br, Sub, Ts) ->
    Br#b_br{bool=simplify_arg(Bool, Sub, Ts)};
simplify_terminator(#b_switch{arg=Arg}=Sw, Sub, Ts) ->
    Sw#b_switch{arg=simplify_arg(Arg, Sub, Ts)};
simplify_terminator(#b_ret{arg=Arg}=Ret, Sub, Ts) ->
    Ret#b_ret{arg=simplify_arg(Arg, Sub, Ts)}.

opt_is([#b_set{op=phi,dst=Dst,args=Args0}=I0|Is],
       Ts0, Ds0, Ls, Sub0, Acc) ->
    %% Simplify the phi node by removing all predecessor blocks that no
    %% longer exists or no longer branches to this block.
    Args = [{simplify_arg(Arg, Sub0, Ts0),From} ||
               {Arg,From} <- Args0, maps:is_key(From, Ls)],
    case all_same(Args) of
        true ->
            %% Eliminate the phi node if there is just one source
            %% value or if the values are identical.
            [{Val,_}|_] = Args,
            Sub = Sub0#{Dst=>Val},
            opt_is(Is, Ts0, Ds0, Ls, Sub, Acc);
        false ->
            I = I0#b_set{args=Args},
            Ts = update_types(I, Ts0, Ds0),
            Ds = Ds0#{Dst=>I},
            opt_is(Is, Ts, Ds, Ls, Sub0, [I|Acc])
    end;
opt_is([#b_set{args=Args0,dst=Dst}=I0|Is],
       Ts0, Ds0, Ls, Sub0, Acc) ->
    Args = simplify_args(Args0, Sub0, Ts0),
    I1 = beam_ssa:normalize(I0#b_set{args=Args}),
    case simplify(I1, Ts0) of
        #b_set{}=I2 ->
            I = beam_ssa:normalize(I2),
            Ts = update_types(I, Ts0, Ds0),
            Ds = Ds0#{Dst=>I},
            opt_is(Is, Ts, Ds, Ls, Sub0, [I|Acc]);
        #b_literal{}=Lit ->
            Sub = Sub0#{Dst=>Lit},
            opt_is(Is, Ts0, Ds0, Ls, Sub, Acc);
        #b_var{}=Var ->
            Sub = Sub0#{Dst=>Var},
            opt_is(Is, Ts0, Ds0, Ls, Sub, Acc)
    end;
opt_is([], Ts, Ds, _Ls, Sub, Acc) ->
    {reverse(Acc),Ts,Ds,Sub}.

simplify(#b_set{op={bif,'and'},args=Args}=I, Ts) ->
    case is_safe_bool_op(Args, Ts) of
        true ->
            case Args of
                [_,#b_literal{val=false}=Res] -> Res;
                [Res,#b_literal{val=true}] -> Res;
                _ -> eval_bif(I, Ts)
            end;
        false ->
            I
    end;
simplify(#b_set{op={bif,'or'},args=Args}=I, Ts) ->
    case is_safe_bool_op(Args, Ts) of
        true ->
            case Args of
                [Res,#b_literal{val=false}] -> Res;
                [_,#b_literal{val=true}=Res] -> Res;
                _ -> eval_bif(I, Ts)
            end;
        false ->
            I
    end;
simplify(#b_set{op={bif,element},args=[#b_literal{val=Index},Tuple]}=I, Ts) ->
    case t_tuple_size(get_type(Tuple, Ts)) of
        {_,Size} when is_integer(Index), 1 =< Index, Index =< Size ->
            I#b_set{op=get_tuple_element,args=[Tuple,#b_literal{val=Index-1}]};
        _ ->
            eval_bif(I, Ts)
    end;
simplify(#b_set{op={bif,hd},args=[List]}=I, Ts) ->
    case get_type(List, Ts) of
        cons ->
            I#b_set{op=get_hd};
        _ ->
            eval_bif(I, Ts)
    end;
simplify(#b_set{op={bif,tl},args=[List]}=I, Ts) ->
    case get_type(List, Ts) of
        cons ->
            I#b_set{op=get_tl};
        _ ->
            eval_bif(I, Ts)
    end;
simplify(#b_set{op={bif,size},args=[Term]}=I, Ts) ->
    case get_type(Term, Ts) of
        #t_tuple{} ->
            simplify(I#b_set{op={bif,tuple_size}}, Ts);
        _ ->
            eval_bif(I, Ts)
    end;
simplify(#b_set{op={bif,tuple_size},args=[Term]}=I, Ts) ->
    case get_type(Term, Ts) of
        #t_tuple{size=Size,exact=true} ->
            #b_literal{val=Size};
        _ ->
            I
    end;
simplify(#b_set{op={bif,'=='},args=Args}=I, Ts) ->
    Types = get_types(Args, Ts),
    EqEq = case {meet(Types),join(Types)} of
               {none,any} -> true;
               {#t_integer{},#t_integer{}} -> true;
               {float,float} -> true;
               {{binary,_},_} -> true;
               {#t_atom{},_} -> true;
               {_,_} -> false
           end,
    case EqEq of
        true ->
            simplify(I#b_set{op={bif,'=:='}}, Ts);
        false ->
            eval_bif(I, Ts)
    end;
simplify(#b_set{op={bif,'=:='},args=[Same,Same]}, _Ts) ->
    #b_literal{val=true};
simplify(#b_set{op={bif,'=:='},args=Args}=I, Ts) ->
    case meet(get_types(Args, Ts)) of
        none -> #b_literal{val=false};
        _ -> eval_bif(I, Ts)
    end;
simplify(#b_set{op={bif,Op},args=Args}=I, Ts) ->
    Types = get_types(Args, Ts),
    case is_float_op(Op, Types) of
        false ->
            eval_bif(I, Ts);
        true ->
            AnnoArgs = [anno_float_arg(A) || A <- Types],
            eval_bif(beam_ssa:add_anno(float_op, AnnoArgs, I), Ts)
    end;
simplify(#b_set{op=get_tuple_element,args=[Tuple,#b_literal{val=0}]}=I, Ts) ->
    case get_type(Tuple, Ts) of
        #t_tuple{elements=[First]} ->
            #b_literal{val=First};
        #t_tuple{} ->
            I
    end;
simplify(#b_set{op=is_nonempty_list,args=[Src]}=I, Ts) ->
    case get_type(Src, Ts) of
        any ->  I;
        list -> I;
        cons -> #b_literal{val=true};
        _ ->    #b_literal{val=false}
    end;
simplify(#b_set{op=is_tagged_tuple,
                args=[Src,#b_literal{val=Size},#b_literal{val=Tag}]}=I, Ts) ->
    case get_type(Src, Ts) of
        #t_tuple{exact=true,size=Size,elements=[Tag]} ->
            #b_literal{val=true};
        #t_tuple{exact=true,size=ActualSize,elements=[]} ->
            if
                Size =/= ActualSize ->
                    #b_literal{val=false};
                true ->
                    I
            end;
        #t_tuple{exact=false} ->
            I;
        any ->
            I;
        _ ->
            #b_literal{val=false}
    end;
simplify(#b_set{op=put_list,args=[#b_literal{val=H},
                                  #b_literal{val=T}]}, _Ts) ->
    #b_literal{val=[H|T]};
simplify(#b_set{op=put_tuple,args=Args}=I, _Ts) ->
    case make_literal_list(Args) of
        none -> I;
        List -> #b_literal{val=list_to_tuple(List)}
    end;
simplify(#b_set{op=update_tuple,args=[_Kind, Tuple | Updates]}=I, Ts) ->
    case get_type(Tuple, Ts) of
        #t_tuple{size=Size} ->
            simplify_update_tuple(Updates, I, Size);
        _ ->
            I
    end;
simplify(#b_set{op=succeeded,args=[#b_literal{}]}, _Ts) ->
    #b_literal{val=true};
simplify(#b_set{op=wait_timeout,args=[#b_literal{val=infinity}]}=I, _Ts) ->
    I#b_set{op=wait,args=[]};
simplify(I, _Ts) -> I.

simplify_update_tuple([#b_literal{val=N}, _Value | Updates], I, Size)
  when is_integer(N), N =< Size, N >= 1 ->
    simplify_update_tuple(Updates, I, Size);
simplify_update_tuple([_|_], #b_set{args=[Kind | _Args]}=I, _Size) ->
    #b_literal{val=can_fail} = Kind,            %Assertion.
    I;
simplify_update_tuple([], #b_set{args=[_Kind | Args]}=I, _Size) ->
    I#b_set{args=[#b_literal{val=cannot_fail} | Args]}.

make_literal_list(Args) ->
    make_literal_list(Args, []).

make_literal_list([#b_literal{val=H}|T], Acc) ->
    make_literal_list(T, [H|Acc]);
make_literal_list([_|_], _) ->
    none;
make_literal_list([], Acc) ->
    reverse(Acc).

is_safe_bool_op(Args, Ts) ->
    [T1,T2] = get_types(Args, Ts),
    t_is_boolean(T1) andalso t_is_boolean(T2).

all_same([{H,_}|T]) ->
    all(fun({E,_}) -> E =:= H end, T).

eval_bif(#b_set{op={bif,Bif},args=Args}=I, Ts) ->
    Arity = length(Args),
    case erl_bifs:is_pure(erlang, Bif, Arity) of
        false ->
            I;
        true ->
            case make_literal_list(Args) of
                none ->
                    case get_types(Args, Ts) of
                        [any] ->
                            I;
                        [Type] ->
                            case will_succeed(Bif, Type) of
                                yes ->
                                    #b_literal{val=true};
                                no ->
                                    #b_literal{val=false};
                                maybe ->
                                    I
                            end;
                        _ ->
                            I
                    end;
                LitArgs ->
                    try apply(erlang, Bif, LitArgs) of
                        Val -> #b_literal{val=Val}
                    catch
                        error:_ -> I
                    end

            end
    end.

simplify_args(Args, Sub, Ts) ->
    [simplify_arg(Arg, Sub, Ts) || Arg <- Args].

simplify_arg(#b_var{}=Arg0, Sub, Ts) ->
    case sub_arg(Arg0, Sub) of
        #b_literal{}=LitArg ->
            LitArg;
        #b_var{}=Arg ->
            Type = get_type(Arg, Ts),
            case get_literal_from_type(Type) of
                none -> Arg;
                #b_literal{}=Lit -> Lit
            end
    end;
simplify_arg(#b_remote{mod=Mod,name=Name}=Rem, Sub, Ts) ->
    Rem#b_remote{mod=simplify_arg(Mod, Sub, Ts),
                 name=simplify_arg(Name, Sub, Ts)};
simplify_arg(Arg, _Sub, _Ts) -> Arg.

sub_arg(#b_var{}=Old, Sub) ->
    case Sub of
        #{Old:=New} -> New;
        #{} -> Old
    end.

is_float_op('-', [float]) ->
    true;
is_float_op('/', [_,_]) ->
    true;
is_float_op(Op, [float,_Other]) ->
    is_float_op_1(Op);
is_float_op(Op, [_Other,float]) ->
    is_float_op_1(Op);
is_float_op(_, _) -> false.

is_float_op_1('+') -> true;
is_float_op_1('-') -> true;
is_float_op_1('*') -> true;
is_float_op_1(_) -> false.

anno_float_arg(float) -> float;
anno_float_arg(_) -> convert.

opt_terminator(#b_br{bool=#b_literal{}}=Br, _Ts, _Ds) ->
    beam_ssa:normalize(Br);
opt_terminator(#b_br{bool=#b_var{}=V}=Br, Ts, Ds) ->
    #{V:=Set} = Ds,
    case Set of
        #b_set{op={bif,'=:='},args=[Bool,#b_literal{val=true}]} ->
            case t_is_boolean(get_type(Bool, Ts)) of
                true ->
                    %% Bool =:= true   ==>  Bool
                    simplify_not(Br#b_br{bool=Bool}, Ts, Ds);
                false ->
                    Br
            end;
        #b_set{} ->
            simplify_not(Br, Ts, Ds)
    end;
opt_terminator(#b_switch{arg=#b_literal{}}=Sw, _Ts, _Ds) ->
    beam_ssa:normalize(Sw);
opt_terminator(#b_switch{arg=#b_var{}=V}=Sw0, Ts, Ds) ->
    Type = get_type(V, Ts),
    case Type of
        #t_integer{elements={_,_}=Range} ->
            simplify_switch_int(Sw0, Range);
        _ ->
            case t_is_boolean(Type) of
                true ->
                    case simplify_switch_bool(Sw0, Ts, Ds) of
                        #b_br{}=Br ->
                            opt_terminator(Br, Ts, Ds);
                        Sw ->
                            beam_ssa:normalize(Sw)
                    end;
                false ->
                    beam_ssa:normalize(Sw0)
            end
    end;
opt_terminator(#b_ret{}=Ret, _Ts, _Ds) -> Ret.

update_successors(#b_br{bool=#b_literal{val=true},succ=S}, Ts, D) ->
    update_successor(S, Ts, D);
update_successors(#b_br{bool=#b_var{}=Bool,succ=Succ,fail=Fail}, Ts0, D0) ->
    case cerl_sets:is_element(Bool, D0#d.once) of
        true ->
            %% This variable is defined in this block and is only
            %% referenced by this br terminator. Therefore, there is
            %% no need to include the type database passed on to the
            %% successors of this block.
            Ts = maps:remove(Bool, Ts0),
            D = update_successor(Fail, Ts, D0),
            SuccTs = infer_types_br(Bool, Ts, D0),
            update_successor(Succ, SuccTs, D);
        false ->
            D = update_successor_bool(Bool, false, Fail, Ts0, D0),
            SuccTs = infer_types_br(Bool, Ts0, D0),
            update_successor_bool(Bool, true, Succ, SuccTs, D)
    end;
update_successors(#b_switch{arg=#b_var{}=V,fail=Fail,list=List}, Ts, D0) ->
    case cerl_sets:is_element(V, D0#d.once) of
        true ->
            %% This variable is defined in this block and is only
            %% referenced by this switch terminator. Therefore, there is
            %% no need to include it in the type database passed on to
            %% the successors of this block.
            D = update_successor(Fail, Ts, D0),
            F = fun({Val,S}, A) ->
                        SuccTs0 = infer_types_switch(V, Val, Ts, D),
                        SuccTs = maps:remove(V, SuccTs0),
                        update_successor(S, SuccTs, A)
                end,
            foldl(F, D, List);
        false ->
            D = update_successor(Fail, Ts, D0),
            F = fun({Val,S}, A) ->
                        SuccTs = infer_types_switch(V, Val, Ts, D),
                        update_successor(S, SuccTs, A)
                end,
            foldl(F, D, List)
        end;
update_successors(#b_ret{}, _Ts, D) -> D.

update_successor_bool(#b_var{}=Var, BoolValue, S, Ts, D) ->
    case t_is_boolean(get_type(Var, Ts)) of
        true ->
            update_successor(S, Ts#{Var:=t_atom(BoolValue)}, D);
        false ->
            %% The `br` terminator is preceeded by an instruction that
            %% does not produce a boolean value, such a `new_try_tag`.
            update_successor(S, Ts, D)
    end.

update_successor(?BADARG_BLOCK, _Ts, #d{}=D) ->
    %% We KNOW that no variables are used in the ?BADARG_BLOCK,
    %% so there is no need to update the type information. That
    %% can be a huge timesaver for huge functions.
    D;
update_successor(S, Ts0, #d{ls=Ls}=D) ->
    case Ls of
        #{S:=Ts1} ->
            Ts = join_types(Ts0, Ts1),
            D#d{ls=Ls#{S:=Ts}};
        #{} ->
            D#d{ls=Ls#{S=>Ts0}}
    end.

update_types(#b_set{op=Op,dst=Dst,args=Args}, Ts, Ds) ->
    T = type(Op, Args, Ts, Ds),
    Ts#{Dst=>T}.

type(phi, Args, Ts, _Ds) ->
    Types = [get_type(A, Ts) || {A,_} <- Args],
    join(Types);
type({bif,'band'}, Args, Ts, _Ds) ->
    band_type(Args, Ts);
type({bif,Bif}, Args, Ts, _Ds) ->
    case bif_type(Bif, Args) of
        number ->
            arith_op_type(Args, Ts);
        Type ->
            Type
    end;
type(bs_init, [#b_literal{val=Type}|Args], _Ts, _Ds) ->
    case {Type,Args} of
        {new,[_,#b_literal{val=Unit}]} ->
            {binary,Unit};
        {append,[_,_,#b_literal{val=Unit}]} ->
            {binary,Unit};
        {private_append,[_,_,#b_literal{val=Unit}]} ->
            {binary,Unit}
    end;
type(bs_extract, [Ctx], Ts, _Ds) ->
    #t_bs_match{type=Type} = get_type(Ctx, Ts),
    Type;
type(bs_match, Args, _Ts, _Ds) ->
    #t_bs_match{type=bs_match_type(Args)};
type(bs_get_tail, _Args, _Ts, _Ds) ->
    {binary, 1};
type(call, [#b_remote{mod=#b_literal{val=Mod},
                      name=#b_literal{val=Name}}|Args], _Ts, _Ds) ->
    case {Mod,Name,Args} of
        {math,_,_} ->
            case is_math_bif(Name, length(Args)) of
                false -> any;
                true -> float
            end;
        {_,_,_} ->
            case erl_bifs:is_exit_bif(Mod, Name, length(Args)) of
                true -> none;
                false -> any
            end
    end;
type(is_nonempty_list, [_], _Ts, _Ds) ->
    t_boolean();
type(is_tagged_tuple, [_,#b_literal{},#b_literal{}], _Ts, _Ds) ->
    t_boolean();
type(put_map, _Args, _Ts, _Ds) ->
    map;
type(put_list, _Args, _Ts, _Ds) ->
    cons;
type(put_tuple, Args, _Ts, _Ds) ->
    case Args of
        [#b_literal{val=First}|_] ->
            #t_tuple{exact=true,size=length(Args),elements=[First]};
        _ ->
            #t_tuple{exact=true,size=length(Args)}
    end;
type(succeeded, [#b_var{}=Src], Ts, Ds) ->
    case maps:get(Src, Ds) of
        #b_set{op={bif,Bif},args=BifArgs} ->
            Types = get_types(BifArgs, Ts),
            case {Bif,Types} of
                {BoolOp,[T1,T2]} when BoolOp =:= 'and'; BoolOp =:= 'or' ->
                    case t_is_boolean(T1) andalso t_is_boolean(T2) of
                        true -> t_atom(true);
                        false -> t_boolean()
                    end;
                {byte_size,[{binary,_}]} ->
                    t_atom(true);
                {bit_size,[{binary,_}]} ->
                    t_atom(true);
                {map_size,[map]} ->
                    t_atom(true);
                {'not',[Type]} ->
                    case t_is_boolean(Type) of
                        true -> t_atom(true);
                        false -> t_boolean()
                    end;
                {size,[{binary,_}]} ->
                    t_atom(true);
                {tuple_size,[#t_tuple{}]} ->
                    t_atom(true);
                {_,_} ->
                    t_boolean()
            end;
        #b_set{op=get_hd} ->
            t_atom(true);
        #b_set{op=get_tl} ->
            t_atom(true);
        #b_set{op=get_tuple_element} ->
            t_atom(true);
        #b_set{op=wait} ->
            t_atom(false);
        #b_set{} ->
            t_boolean()
    end;
type(update_tuple, [_Kind, Src | Updates0], Ts, _Ds) ->
    F = fun F([Pos, Value | Updates], #t_tuple{size=Size}=T) ->
                case get_type(Pos, Ts) of
                    #t_integer{elements={MinIndex,_}} when MinIndex > 1 ->
                        %% First element can't be overwritten, so we bump the
                        %% size and leave it at that.
                        F(Updates, T#t_tuple{size=max(MinIndex, Size)});
                    #t_integer{elements={1,1}} ->
                        %% First element is overwritten, set its value if we
                        %% know it.
                        Es = case get_literal_from_type(get_type(Value, Ts)) of
                                 #b_literal{val=Val} -> [Val];
                                 none -> []
                             end,
                        F(Updates, T#t_tuple{size=max(1, Size), elements=Es});
                    #t_integer{elements={1,_}} ->
                        %% First element *could* be overwritten, so we need to
                        %% wipe it before bumping the size.
                        F(Updates, T#t_tuple{size=max(1, Size),
                                             elements=[]});
                    _ ->
                        F(Updates, T#t_tuple{elements=[]})
                end;
            F([], #t_tuple{}=T) ->
                T
        end,
    Type = case get_type(Src, Ts) of
               #t_tuple{}=T -> T;
               _ -> #t_tuple{}
           end,
    F(Updates0, Type);
type(_, _, _, _) -> any.

arith_op_type(Args, Ts) ->
    Types = get_types(Args, Ts),
    foldl(fun(#t_integer{}, unknown) -> t_integer();
             (#t_integer{}, number) -> number;
             (#t_integer{}, float) -> float;
             (#t_integer{}, #t_integer{}) -> t_integer();
             (float, unknown) -> float;
             (float, #t_integer{}) -> float;
             (float, number) -> float;
             (number, unknown) -> number;
             (number, #t_integer{}) -> number;
             (number, float) -> float;
             (any, _) -> number;
             (Same, Same) -> Same;
             (_, _) -> none
          end, unknown, Types).

%% will_succeed(TestOperation, Type) -> yes|no|maybe.
%%  Test whether TestOperation applied to an argument of type Type
%%  will succeed.  Return yes, no, or maybe.
%%
%%  Type is a type as described in the comment for verified_type/1 at
%%  the very end of this file, but it will *never* be 'any'.

will_succeed(is_atom, Type) ->
    case Type of
        #t_atom{} -> yes;
        _ -> no
    end;
will_succeed(is_binary, Type) ->
    case Type of
        {binary,U} when U rem 8 =:= 0 -> yes;
        {binary,_} -> maybe;
        _ -> no
    end;
will_succeed(is_bitstring, Type) ->
    case Type of
        {binary,_} -> yes;
        _ -> no
    end;
will_succeed(is_boolean, Type) ->
    case Type of
        #t_atom{elements=any} ->
            maybe;
        #t_atom{elements=Es} ->
            case t_is_boolean(Type) of
                true ->
                    yes;
                false ->
                    case any(fun is_boolean/1, Es) of
                        true -> maybe;
                        false -> no
                    end
            end;
        _ ->
            no
    end;
will_succeed(is_float, Type) ->
    case Type of
        float -> yes;
        number -> maybe;
        _ -> no
    end;
will_succeed(is_integer, Type) ->
    case Type of
        #t_integer{} -> yes;
        number -> maybe;
        _ -> no
    end;
will_succeed(is_list, Type) ->
    case Type of
        list -> yes;
        cons -> yes;
        _ -> no
    end;
will_succeed(is_map, Type) ->
    case Type of
        map -> yes;
        _ -> no
    end;
will_succeed(is_number, Type) ->
    case Type of
        float -> yes;
        #t_integer{} -> yes;
        number -> yes;
        _ -> no
    end;
will_succeed(is_tuple, Type) ->
    case Type of
        #t_tuple{} -> yes;
        _ -> no
    end;
will_succeed(_, _) -> maybe.


band_type([Other,#b_literal{val=Int}], Ts) when is_integer(Int) ->
    band_type_1(Int, Other, Ts);
band_type([_,_], _) -> t_integer().

band_type_1(Int, OtherSrc, Ts) ->
    Type = band_type_2(Int, 0),
    OtherType = get_type(OtherSrc, Ts),
    meet(Type, OtherType).

band_type_2(N, Bits) when Bits < 64 ->
    case 1 bsl Bits of
        P when P =:= N + 1 ->
            t_integer(0, N);
        P when P > N + 1 ->
            t_integer();
        _ ->
            band_type_2(N, Bits+1)
    end;
band_type_2(_, _) ->
    %% Negative or large positive number. Give up.
    t_integer().

bs_match_type([#b_literal{val=Type}|Args]) ->
    bs_match_type(Type, Args).

bs_match_type(binary, Args) ->
    [_,_,_,#b_literal{val=U}] = Args,
    {binary,U};
bs_match_type(float, _) ->
    float;
bs_match_type(integer, Args) ->
    case Args of
        [_,
         #b_literal{val=Flags},
         #b_literal{val=Size},
         #b_literal{val=Unit}] when Size * Unit < 64 ->
            NumBits = Size * Unit,
            case member(unsigned, Flags) of
                true ->
                    t_integer(0, (1 bsl NumBits)-1);
                false ->
                    %% Signed integer. Don't bother.
                    t_integer()
            end;
        [_|_] ->
            t_integer()
    end;
bs_match_type(skip, _) ->
    any;
bs_match_type(string, _) ->
    any;
bs_match_type(utf8, _) ->
    ?UNICODE_INT;
bs_match_type(utf16, _) ->
    ?UNICODE_INT;
bs_match_type(utf32, _) ->
    ?UNICODE_INT.

simplify_switch_int(#b_switch{list=List0}=Sw, {Min,Max}) ->
    List1 = sort(List0),
    Vs = [V || {#b_literal{val=V},_} <- List1],
    case eq_ranges(Vs, Min, Max) of
        true ->
            {_,LastL} = last(List1),
            List = droplast(List1),
            Sw#b_switch{fail=LastL,list=List};
        false ->
            Sw
    end.

eq_ranges([H], H, H) -> true;
eq_ranges([H|T], H, Max) -> eq_ranges(T, H+1, Max);
eq_ranges(_, _, _) -> false.

simplify_switch_bool(#b_switch{arg=B,list=List0}=Sw, Ts, Ds) ->
    List = sort(List0),
    case List of
        [{#b_literal{val=false},Fail},{#b_literal{val=true},Succ}] ->
            simplify_not(#b_br{bool=B,succ=Succ,fail=Fail}, Ts, Ds);
        [_|_] ->
            Sw
    end.

simplify_not(#b_br{bool=#b_var{}=V,succ=Succ,fail=Fail}=Br0, Ts, Ds) ->
    case Ds of
        #{V:=#b_set{op={bif,'not'},args=[Bool]}} ->
            case t_is_boolean(get_type(Bool, Ts)) of
                true ->
                    Br = Br0#b_br{bool=Bool,succ=Fail,fail=Succ},
                    beam_ssa:normalize(Br);
                false ->
                    Br0
            end;
        #{} ->
            Br0
    end.

%%%
%%% Calculate the set of variables that are only used once in the
%%% block that they are defined in. That will allow us to discard type
%%% information for variables that will never be referenced by the
%%% successor blocks, potentially improving compilation times.
%%%

used_once(Linear, Args) ->
    Map0 = used_once_1(reverse(Linear), #{}),
    Map = maps:without(Args, Map0),
    cerl_sets:from_list(maps:keys(Map)).

used_once_1([{L,#b_blk{is=Is,last=Last}}|Bs], Uses0) ->
    Uses = used_once_2([Last|reverse(Is)], L, Uses0),
    used_once_1(Bs, Uses);
used_once_1([], Uses) -> Uses.

used_once_2([I|Is], L, Uses0) ->
    Uses = used_once_uses(beam_ssa:used(I), L, Uses0),
    case I of
        #b_set{dst=Dst} ->
            case Uses of
                #{Dst:=[L]} ->
                    used_once_2(Is, L, Uses);
                #{} ->
                    used_once_2(Is, L, maps:remove(Dst, Uses))
            end;
        _ ->
            used_once_2(Is, L, Uses)
    end;
used_once_2([], _, Uses) -> Uses.

used_once_uses([V|Vs], L, Uses) ->
    case Uses of
        #{V:=Us} ->
            used_once_uses(Vs, L, Uses#{V:=[L|Us]});
        #{} ->
            used_once_uses(Vs, L, Uses#{V=>[L]})
    end;
used_once_uses([], _, Uses) -> Uses.


get_types(Values, Ts) ->
    [get_type(Val, Ts) || Val <- Values].
-spec get_type(beam_ssa:value(), type_db()) -> type().

get_type(#b_var{}=V, Ts) ->
    #{V:=T} = Ts,
    T;
get_type(#b_literal{val=Val}, _Ts) ->
    if
        is_atom(Val) ->
            t_atom(Val);
        is_float(Val) ->
            float;
        is_integer(Val) ->
            t_integer(Val);
        is_list(Val), Val =/= [] ->
            cons;
        is_map(Val) ->
            map;
        Val =:= {} ->
            #t_tuple{exact=true};
        is_tuple(Val) ->
            #t_tuple{exact=true,size=tuple_size(Val),
                     elements=[element(1, Val)]};
        Val =:= [] ->
            nil;
        true ->
            any
    end.

infer_types_switch(V, Lit, Ts, #d{ds=Ds}) ->
    Types = infer_type({bif,'=:='}, [V, Lit], Ds),
    meet_types(Types, Ts).

infer_types_br(#b_var{}=V, Ts, #d{ds=Ds}) ->
    #{V:=#b_set{op=Op,args=Args}} = Ds,
    Types = infer_type(Op, Args, Ds),
    meet_types(Types, Ts).

infer_type({bif,element}, [#b_literal{val=Pos},#b_var{}=Tuple], _Ds) ->
    if
        is_integer(Pos), 1 =< Pos ->
            [{Tuple,#t_tuple{size=Pos}}];
        true ->
            []
    end;
infer_type({bif,'=:='}, [#b_var{}=Src,#b_literal{}=Lit], Ds) ->
    Def = maps:get(Src, Ds),
    Type = get_type(Lit, #{}),
    [{Src,Type}|infer_tuple_size(Def, Lit) ++
         infer_first_element(Def, Lit)];
infer_type({bif,Bif}, [#b_var{}=Src]=Args, _Ds) ->
    case inferred_bif_type(Bif, Args) of
        any -> [];
        T -> [{Src,T}]
    end;
infer_type({bif,is_map_key}, [_,#b_var{}=Src], _Ds) ->
    [{Src,map}];
infer_type({bif,map_get}, [_,#b_var{}=Src], _Ds) ->
    [{Src,map}];
infer_type(bs_start_match, [#b_var{}=Bin], _Ds) ->
    [{Bin,{binary,1}}];
infer_type(is_nonempty_list, [#b_var{}=Src], _Ds) ->
    [{Src,cons}];
infer_type(is_tagged_tuple, [#b_var{}=Src,#b_literal{val=Size},
                             #b_literal{val=Tag}], _Ds) ->
    [{Src,#t_tuple{exact=true,size=Size,elements=[Tag]}}];
infer_type(succeeded, [#b_var{}=Src], Ds) ->
    #b_set{op=Op,args=Args} = maps:get(Src, Ds),
    infer_type(Op, Args, Ds);
infer_type(_Op, _Args, _Ds) ->
    [].

%% bif_type(Name, Args) -> Type
%%  Return the return type for the guard BIF or operator Name with
%%  arguments Args.
%%
%%  Note that that the following BIFs are handle elsewhere:
%%
%%     band/2

bif_type(abs, [_]) -> number;
bif_type(bit_size, [_]) -> t_integer();
bif_type(byte_size, [_]) -> t_integer();
bif_type(ceil, [_]) -> t_integer();
bif_type(float, [_]) -> float;
bif_type(floor, [_]) -> t_integer();
bif_type(is_map_key, [_,_]) -> t_boolean();
bif_type(length, [_]) -> t_integer();
bif_type(map_size, [_]) -> t_integer();
bif_type(node, []) -> #t_atom{};
bif_type(node, [_]) -> #t_atom{};
bif_type(round, [_]) -> t_integer();
bif_type(size, [_]) -> t_integer();
bif_type(trunc, [_]) -> t_integer();
bif_type(tuple_size, [_]) -> t_integer();
bif_type('bnot', [_]) -> t_integer();
bif_type('bor', [_,_]) -> t_integer();
bif_type('bsl', [_,_]) -> t_integer();
bif_type('bsr', [_,_]) -> t_integer();
bif_type('bxor', [_,_]) -> t_integer();
bif_type('div', [_,_]) -> t_integer();
bif_type('rem', [_,_]) -> t_integer();
bif_type('/', [_,_]) -> float;
bif_type(Name, Args) ->
    Arity = length(Args),
    case erl_internal:new_type_test(Name, Arity) orelse
        erl_internal:bool_op(Name, Arity) orelse
        erl_internal:comp_op(Name, Arity) of
        true ->
            t_boolean();
        false ->
            case erl_internal:arith_op(Name, Arity) of
                true -> number;
                false -> any
            end
    end.

inferred_bif_type(is_atom, [_]) -> t_atom();
inferred_bif_type(is_binary, [_]) -> {binary,8};
inferred_bif_type(is_bitstring, [_]) -> {binary,1};
inferred_bif_type(is_boolean, [_]) -> t_boolean();
inferred_bif_type(is_float, [_]) -> float;
inferred_bif_type(is_integer, [_]) -> t_integer();
inferred_bif_type(is_list, [_]) -> list;
inferred_bif_type(is_map, [_]) -> map;
inferred_bif_type(is_number, [_]) -> number;
inferred_bif_type(is_tuple, [_]) -> #t_tuple{};
inferred_bif_type(abs, [_]) -> number;
inferred_bif_type(bit_size, [_]) -> {binary,1};
inferred_bif_type(byte_size, [_]) -> {binary,1};
inferred_bif_type(ceil, [_]) -> number;
inferred_bif_type(float, [_]) -> number;
inferred_bif_type(floor, [_]) -> number;
inferred_bif_type(hd, [_]) -> cons;
inferred_bif_type(length, [_]) -> list;
inferred_bif_type(map_size, [_]) -> map;
inferred_bif_type(round, [_]) -> number;
inferred_bif_type(trunc, [_]) -> number;
inferred_bif_type(tl, [_]) -> cons;
inferred_bif_type(tuple_size, [_]) -> #t_tuple{};
inferred_bif_type(_, _) -> any.

infer_tuple_size(#b_set{op={bif,tuple_size},args=[#b_var{}=Tuple]},
                 #b_literal{val=Size}) when is_integer(Size) ->
    [{Tuple,#t_tuple{exact=true,size=Size}}];
infer_tuple_size(_, _) -> [].

infer_first_element(#b_set{op=get_tuple_element,
                           args=[#b_var{}=Tuple,#b_literal{val=0}]},
                    #b_literal{val=First}) ->
    [{Tuple,#t_tuple{size=1,elements=[First]}}];
infer_first_element(_, _) -> [].

is_math_bif(cos, 1) -> true;
is_math_bif(cosh, 1) -> true;
is_math_bif(sin, 1) -> true;
is_math_bif(sinh, 1) -> true;
is_math_bif(tan, 1) -> true;
is_math_bif(tanh, 1) -> true;
is_math_bif(acos, 1) -> true;
is_math_bif(acosh, 1) -> true;
is_math_bif(asin, 1) -> true;
is_math_bif(asinh, 1) -> true;
is_math_bif(atan, 1) -> true;
is_math_bif(atanh, 1) -> true;
is_math_bif(erf, 1) -> true;
is_math_bif(erfc, 1) -> true;
is_math_bif(exp, 1) -> true;
is_math_bif(log, 1) -> true;
is_math_bif(log2, 1) -> true;
is_math_bif(log10, 1) -> true;
is_math_bif(sqrt, 1) -> true;
is_math_bif(atan2, 2) -> true;
is_math_bif(pow, 2) -> true;
is_math_bif(ceil, 1) -> true;
is_math_bif(floor, 1) -> true;
is_math_bif(fmod, 2) -> true;
is_math_bif(pi, 0) -> true;
is_math_bif(_, _) -> false.

join_types(Ts0, Ts1) ->
    if
        map_size(Ts0) < map_size(Ts1) ->
            join_types_1(maps:keys(Ts0), Ts1, Ts0);
        true ->
            join_types_1(maps:keys(Ts1), Ts0, Ts1)
    end.

join_types_1([V|Vs], Ts0, Ts1) ->
    case {Ts0,Ts1} of
        {#{V:=Same},#{V:=Same}} ->
            join_types_1(Vs, Ts0, Ts1);
        {#{V:=T0},#{V:=T1}} ->
            case join(T0, T1) of
                T1 ->
                    join_types_1(Vs, Ts0, Ts1);
                T ->
                    join_types_1(Vs, Ts0, Ts1#{V:=T})
            end;
        {#{},#{V:=_}} ->
            join_types_1(Vs, Ts0, Ts1)
    end;
join_types_1([], Ts0, Ts1) ->
    maps:merge(Ts0, Ts1).

join([T1,T2|Ts]) ->
    join([join(T1, T2)|Ts]);
join([T]) -> T.

get_literal_from_type(#t_atom{elements=[Atom]}) ->
    #b_literal{val=Atom};
get_literal_from_type(#t_integer{elements={Int,Int}}) ->
    #b_literal{val=Int};
get_literal_from_type(nil) ->
    #b_literal{val=[]};
get_literal_from_type(_) -> none.

t_atom() ->
    #t_atom{elements=any}.

t_atom(Atom) when is_atom(Atom) ->
    #t_atom{elements=[Atom]}.

t_boolean() ->
    #t_atom{elements=[false,true]}.

t_integer() ->
    #t_integer{elements=any}.

t_integer(Int) when is_integer(Int) ->
    #t_integer{elements={Int,Int}}.

t_integer(Min, Max) when is_integer(Min), is_integer(Max) ->
    #t_integer{elements={Min,Max}}.

t_is_boolean(#t_atom{elements=[F,T]}) ->
    F =:= false andalso T =:= true;
t_is_boolean(#t_atom{elements=[B]}) ->
    is_boolean(B);
t_is_boolean(_) -> false.

t_tuple_size(#t_tuple{size=Size,exact=false}) ->
    {at_least,Size};
t_tuple_size(#t_tuple{size=Size,exact=true}) ->
    {exact,Size};
t_tuple_size(_) ->
    none.

%% join(Type1, Type2) -> Type
%%  Return the "join" of Type1 and Type2. The join is a more general
%%  type than Type1 and Type2. For example:
%%
%%     join(#t_integer{elements=any}, #t_integer=elements={0,3}}) ->
%%          #t_integer{}
%%
%%  The join for two different types result in 'any', which is
%%  the top element for our type lattice:
%%
%%     join(#t_integer{}, map) -> any

-spec join(type(), type()) -> type().

join(T, T) ->
    verified_type(T);
join(none, T) ->
    verified_type(T);
join(T, none) ->
    verified_type(T);
join(any, _) -> any;
join(_, any) -> any;
join(#t_atom{elements=[_|_]=Set1}, #t_atom{elements=[_|_]=Set2}) ->
    Set = ordsets:union(Set1, Set2),
    case ordsets:size(Set) of
        Size when Size =< ?ATOM_SET_SIZE ->
            #t_atom{elements=Set};
        _Size ->
            #t_atom{elements=any}
    end;
join(#t_atom{elements=any}=T, #t_atom{elements=[_|_]}) -> T;
join(#t_atom{elements=[_|_]}, #t_atom{elements=any}=T) -> T;
join({binary,U1}, {binary,U2}) ->
    {binary,gcd(U1, U2)};
join(#t_integer{}, #t_integer{}) -> t_integer();
join(list, cons) -> list;
join(cons, list) -> list;
join(nil, cons) -> list;
join(cons, nil) -> list;
join(nil, list) -> list;
join(list, nil) -> list;
join(#t_integer{}, float) -> number;
join(float, #t_integer{}) -> number;
join(#t_integer{}, number) -> number;
join(number, #t_integer{}) -> number;
join(float, number) -> number;
join(number, float) -> number;
join(#t_tuple{size=Sz,exact=Exact1}, #t_tuple{size=Sz,exact=Exact2}) ->
    Exact = Exact1 and Exact2,
    #t_tuple{size=Sz,exact=Exact};
join(#t_tuple{size=Sz1}, #t_tuple{size=Sz2}) ->
    #t_tuple{size=min(Sz1, Sz2)};
join(_T1, _T2) ->
    %%io:format("~p ~p\n", [_T1,_T2]),
    any.

gcd(A, B) ->
    case A rem B of
        0 -> B;
        X -> gcd(B, X)
    end.

meet_types([{V,T0}|Vs], Ts) ->
    #{V:=T1} = Ts,
    T = meet(T0, T1),
    meet_types(Vs, Ts#{V:=T});
meet_types([], Ts) -> Ts.

meet([T1,T2|Ts]) ->
    meet([meet(T1, T2)|Ts]);
meet([T]) -> T.

%% meet(Type1, Type2) -> Type
%%  Return the "meet" of Type1 and Type2. The meet is a narrower
%%  type than Type1 and Type2. For example:
%%
%%     meet(#t_integer{elements=any}, #t_integer{elements={0,3}}) ->
%%          #t_integer{elements={0,3}}
%%
%%  The meet for two different types result in 'none', which is
%%  the bottom element for our type lattice:
%%
%%     meet(#t_integer{}, map) -> none

-spec meet(type(), type()) -> type().

meet(T, T) ->
    verified_type(T);
meet(#t_atom{elements=[_|_]=Set1}, #t_atom{elements=[_|_]=Set2}) ->
    case ordsets:intersection(Set1, Set2) of
        [] ->
            none;
        [_|_]=Set ->
            #t_atom{elements=Set}
    end;
meet(#t_atom{elements=[_|_]}=T, #t_atom{elements=any}) ->
    T;
meet(#t_atom{elements=any}, #t_atom{elements=[_|_]}=T) ->
    T;
meet(#t_integer{elements={_,_}}=T, #t_integer{elements=any}) ->
    T;
meet(#t_integer{elements=any}, #t_integer{elements={_,_}}=T) ->
    T;
meet(#t_integer{elements={Min1,Max1}},
     #t_integer{elements={Min2,Max2}}) ->
    #t_integer{elements={max(Min1, Min2),min(Max1, Max2)}};
meet(#t_integer{}=T, number) -> T;
meet(float=T, number) -> T;
meet(number, #t_integer{}=T) -> T;
meet(number, float=T) -> T;
meet(list, cons) -> cons;
meet(list, nil) -> nil;
meet(cons, list) -> cons;
meet(nil, list) -> nil;
meet(#t_tuple{}=T1, #t_tuple{}=T2) ->
    meet_tuples(T1, T2);
meet({binary,U1}, {binary,U2}) ->
    {binary,max(U1, U2)};
meet(any, T) ->
    verified_type(T);
meet(T, any) ->
    verified_type(T);
meet(_, _) ->
    %% Inconsistent types. There will be an exception at runtime.
    none.

meet_tuples(#t_tuple{elements=[E1]}, #t_tuple{elements=[E2]})
  when E1 =/= E2 ->
    none;
meet_tuples(#t_tuple{size=Sz1,exact=true},
            #t_tuple{size=Sz2,exact=true}) when Sz1 =/= Sz2 ->
    none;
meet_tuples(#t_tuple{size=Sz1,exact=Ex1,elements=Es1},
            #t_tuple{size=Sz2,exact=Ex2,elements=Es2}) ->
    Size = max(Sz1, Sz2),
    Exact = Ex1 or Ex2,
    Es = case {Es1,Es2} of
             {[],[_|_]} -> Es2;
             {[_|_],[]} -> Es1;
             {_,_} -> Es1
         end,
    #t_tuple{size=Size,exact=Exact,elements=Es}.

%% verified_type(Type) -> Type
%%  Returns the passed in type if it is one of the defined types.
%%  Crashes if there is anything wrong with the type.
%%
%%  Here are all possible types:
%%
%%  any                  Any Erlang term (top element for the type lattice).
%%
%%  #t_atom{}            Any atom or some specific atoms.
%%  {binary,Unit}        Binary/bitstring aligned to unit Unit.
%%  float                Floating point number.
%%  #t_integer{}         Integer
%%  list                 Empty or nonempty list.
%%  map                  Map.
%%  nil                  Empty list.
%%  cons                 Cons (nonempty list).
%%  number               A number (float or integer).
%%  #t_tuple{}           Tuple.
%%
%%  none                 No type (bottom element for the type lattice).

-spec verified_type(T) -> T when
      T :: type().

verified_type(any=T) -> T;
verified_type(none=T) -> T;
verified_type(#t_atom{elements=any}=T) -> T;
verified_type(#t_atom{elements=[_|_]}=T) -> T;
verified_type({binary,U}=T) when is_integer(U) -> T;
verified_type(#t_integer{elements=any}=T) -> T;
verified_type(#t_integer{elements={Min,Max}}=T)
  when is_integer(Min), is_integer(Max) -> T;
verified_type(list=T) -> T;
verified_type(map=T) -> T;
verified_type(nil=T) -> T;
verified_type(cons=T) -> T;
verified_type(number=T) -> T;
verified_type(#t_tuple{}=T) -> T;
verified_type(float=T) -> T.
