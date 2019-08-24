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
-export([opt_start/4, opt_continue/4, opt_finish/3]).

-include("beam_ssa_opt.hrl").
-import(lists, [all/2,any/2,droplast/1,foldl/3,last/1,member/2,
                keyfind/3,reverse/1,reverse/2,
                sort/1,split/2]).

-define(UNICODE_INT, #t_integer{elements={0,16#10FFFF}}).

-record(d,
        {ds :: #{beam_ssa:b_var():=beam_ssa:b_set()},
         ls :: #{beam_ssa:label():=type_db()},
         once :: cerl_sets:set(beam_ssa:b_var()),
         func_id :: func_id(),
         func_db :: func_info_db(),
         sub = #{} :: #{beam_ssa:b_var():=beam_ssa:value()},
         ret_type = [] :: [type()]}).

-define(ATOM_SET_SIZE, 5).

%% Records that represent type information.
-record(t_atom, {elements=any :: 'any' | [atom()]}).
-record(t_integer, {elements=any :: 'any' | {integer(),integer()}}).
-record(t_bs_match, {type :: type()}).
-record(t_tuple, {size=0 :: integer(),
                  exact=false :: boolean(),
                  %% Known element types (1-based index), unknown elements are
                  %% are assumed to be 'any'.
                  elements=#{} :: #{ non_neg_integer() => type() }}).

-type type() :: 'any' | 'none' |
                #t_atom{} | #t_integer{} | #t_bs_match{} | #t_tuple{} |
                {'binary',pos_integer()} | 'cons' | 'float' | 'list' | 'map' | 'nil' | 'number'.
-type type_db() :: #{beam_ssa:var_name():=type()}.

-spec opt_start(Linear, Args, Anno, FuncDb) -> {Linear, FuncDb} when
      Linear :: [{non_neg_integer(), beam_ssa:b_blk()}],
      Args :: [beam_ssa:b_var()],
      Anno :: beam_ssa:anno(),
      FuncDb :: func_info_db().
opt_start(Linear, Args, Anno, FuncDb) ->
    %% This is the first run through the module, so our arg_types can be
    %% incomplete as we may not have visited all call sites at least once.
    Ts = maps:from_list([{V,any} || #b_var{}=V <- Args]),
    opt_continue_1(Linear, Args, get_func_id(Anno), Ts, FuncDb).

-spec opt_continue(Linear, Args, Anno, FuncDb) -> {Linear, FuncDb} when
      Linear :: [{non_neg_integer(), beam_ssa:b_blk()}],
      Args :: [beam_ssa:b_var()],
      Anno :: beam_ssa:anno(),
      FuncDb :: func_info_db().
opt_continue(Linear, Args, Anno, FuncDb) ->
    Id = get_func_id(Anno),
    case FuncDb of
        #{ Id := #func_info{exported=false,arg_types=ArgTypes} } ->
            %% This is a local function and we're guaranteed to have visited
            %% every call site at least once, so we know that the parameter
            %% types are at least as narrow as the join of all argument types.
            Ts = join_arg_types(Args, ArgTypes, Anno),
            opt_continue_1(Linear, Args, Id, Ts, FuncDb);
        #{} ->
            %% We can't infer the parameter types of exported functions, nor
            %% the ones where module-level optimization is disabled, but
            %% running the pass again could still help other functions.
            Ts = maps:from_list([{V,any} || #b_var{}=V <- Args]),
            opt_continue_1(Linear, Args, Id, Ts, FuncDb)
    end.

join_arg_types(Args, ArgTypes, Anno) ->
    %% We suppress type optimization for parameters that have already been
    %% optimized by another pass, as they may have done things we have no idea
    %% how to interpret and running them over could generate incorrect code.
    ParamTypes = maps:get(parameter_type_info, Anno, #{}),
    Ts0 = join_arg_types_1(Args, ArgTypes, #{}),
    maps:fold(fun(Arg, _V, Ts) ->
                      maps:put(Arg, any, Ts)
              end, Ts0, ParamTypes).

join_arg_types_1([Arg | Args], [TM | TMs], Ts) when map_size(TM) =/= 0 ->
    join_arg_types_1(Args, TMs, Ts#{ Arg => join(maps:values(TM))});
join_arg_types_1([Arg | Args], [_TM | TMs], Ts) ->
    join_arg_types_1(Args, TMs, Ts#{ Arg => any });
join_arg_types_1([], [], Ts) ->
    Ts.

-spec opt_continue_1(Linear, Args, Id, Ts, FuncDb) -> Result when
      Linear :: [{non_neg_integer(), beam_ssa:b_blk()}],
      Args :: [beam_ssa:b_var()],
      Id :: func_id(),
      Ts :: type_db(),
      FuncDb :: func_info_db(),
      Result :: {Linear, FuncDb}.
opt_continue_1(Linear0, Args, Id, Ts, FuncDb0) ->
    UsedOnce = used_once(Linear0, Args),
    FakeCall = #b_set{op=call,args=[#b_remote{mod=#b_literal{val=unknown},
                                              name=#b_literal{val=unknown},
                                              arity=0}]},
    Defs = maps:from_list([{Var,FakeCall#b_set{dst=Var}} ||
                              #b_var{}=Var <- Args]),

    D = #d{ func_db=FuncDb0,
            func_id=Id,
            ds=Defs,
            ls=#{0=>Ts,?BADARG_BLOCK=>#{}},
            once=UsedOnce },

    {Linear, FuncDb, NewRet} = opt(Linear0, D, []),

    case FuncDb of
        #{ Id := Entry0 } ->
            Entry = Entry0#func_info{ret_type=NewRet},
            {Linear, FuncDb#{ Id := Entry }};
        #{} ->
            %% Module-level optimizations have been turned off for this
            %% function.
            {Linear, FuncDb}
    end.

-spec opt_finish(Args, Anno, FuncDb) -> {Anno, FuncDb} when
      Args :: [beam_ssa:b_var()],
      Anno :: beam_ssa:anno(),
      FuncDb :: func_info_db().
opt_finish(Args, Anno, FuncDb) ->
    Id = get_func_id(Anno),
    case FuncDb of
        #{ Id := #func_info{exported=false,arg_types=ArgTypes} } ->
            ParamInfo0 = maps:get(parameter_type_info, Anno, #{}),
            ParamInfo = opt_finish_1(Args, ArgTypes, ParamInfo0),
            {Anno#{ parameter_type_info => ParamInfo }, FuncDb};
        #{} ->
            {Anno, FuncDb}
    end.

opt_finish_1([Arg | Args], [TypeMap | TypeMaps], ParamInfo)
  when is_map_key(Arg, ParamInfo); %% See join_arg_types/3
       map_size(TypeMap) =:= 0 ->
    opt_finish_1(Args, TypeMaps, ParamInfo);
opt_finish_1([Arg | Args], [TypeMap | TypeMaps], ParamInfo0) ->
    case join(maps:values(TypeMap)) of
        any ->
            opt_finish_1(Args, TypeMaps, ParamInfo0);
        none ->
            %% This function will never be called. Pretend that we don't
            %% know the type for this argument.
            opt_finish_1(Args, TypeMaps, ParamInfo0);
        JoinedType ->
            JoinedType = verified_type(JoinedType),
            ParamInfo = ParamInfo0#{ Arg => validator_anno(JoinedType) },
            opt_finish_1(Args, TypeMaps, ParamInfo)
    end;
opt_finish_1([], [], ParamInfo) ->
    ParamInfo.

validator_anno(#t_tuple{size=Size,exact=Exact,elements=Elements0}) ->
    Elements = maps:fold(fun(Index, Type, Acc) ->
                                 Key = beam_validator:type_anno(integer, Index),
                                 Acc#{ Key => validator_anno(Type) }
                         end, #{}, Elements0),
    beam_validator:type_anno(tuple, Size, Exact, Elements);
validator_anno(#t_integer{elements={Same,Same}}) ->
    beam_validator:type_anno(integer, Same);
validator_anno(#t_integer{}) ->
    beam_validator:type_anno(integer);
validator_anno(float) ->
    beam_validator:type_anno(float);
validator_anno(#t_atom{elements=[Val]}) ->
    beam_validator:type_anno(atom, Val);
validator_anno(#t_atom{}=A) ->
    case t_is_boolean(A) of
        true -> beam_validator:type_anno(bool);
        false -> beam_validator:type_anno(atom)
    end;
validator_anno(T) ->
    beam_validator:type_anno(T).

get_func_id(Anno) ->
    #{func_info:={_Mod, Name, Arity}} = Anno,
    #b_local{name=#b_literal{val=Name}, arity=Arity}.

opt([{L,Blk}|Bs], #d{ls=Ls}=D, Acc) ->
    case Ls of
        #{L:=Ts} ->
            opt_1(L, Blk, Bs, Ts, D, Acc);
        #{} ->
            %% This block is never reached. Discard it.
            opt(Bs, D, Acc)
    end;
opt([], D, Acc) ->
    #d{func_db=FuncDb,ret_type=NewRet} = D,
    {reverse(Acc), FuncDb, NewRet}.

opt_1(L, #b_blk{is=Is0,last=Last0}=Blk0, Bs, Ts0,
      #d{ds=Ds0,sub=Sub0,func_db=Fdb0}=D0, Acc) ->
    case opt_is(Is0, Ts0, Ds0, Fdb0, D0, Sub0, []) of
        {Is,Ts,Ds,Fdb,Sub} ->
            D1 = D0#d{ds=Ds,sub=Sub,func_db=Fdb},
            Last1 = simplify_terminator(Last0, Sub, Ts, Ds),
            Last = opt_terminator(Last1, Ts, Ds),
            D = update_successors(Last, Ts, D1),
            Blk = Blk0#b_blk{is=Is,last=Last},
            opt(Bs, D, [{L,Blk}|Acc]);
        {no_return,Ret,Is,Ds,Fdb,Sub} ->
            %% This call will never reach the successor block.
            %% Rewrite the terminator to a 'ret', and remove
            %% all type information for this label. That can
            %% potentially narrow the type of the phi node
            %% in the former successor.
            Ls = maps:remove(L, D0#d.ls),
            RetType = join([none|D0#d.ret_type]),
            D = D0#d{ds=Ds,ls=Ls,sub=Sub,
                     func_db=Fdb,ret_type=[RetType]},
            Blk = Blk0#b_blk{is=Is,last=Ret},
            opt(Bs, D, [{L,Blk}|Acc])
    end.

simplify_terminator(#b_br{bool=Bool}=Br, Sub, Ts, _Ds) ->
    Br#b_br{bool=simplify_arg(Bool, Sub, Ts)};
simplify_terminator(#b_switch{arg=Arg}=Sw, Sub, Ts, _Ds) ->
    Sw#b_switch{arg=simplify_arg(Arg, Sub, Ts)};
simplify_terminator(#b_ret{arg=Arg}=Ret, Sub, Ts, Ds) ->
    %% Reducing the result of a call to a literal (fairly common for 'ok')
    %% breaks tail call optimization.
    case Ds of
        #{ Arg := #b_set{op=call}} -> Ret;
        #{} -> Ret#b_ret{arg=simplify_arg(Arg, Sub, Ts)}
    end.

opt_is([#b_set{op=phi,dst=Dst,args=Args0}=I0|Is],
       Ts0, Ds0, Fdb, #d{ls=Ls}=D, Sub0, Acc) ->
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
            opt_is(Is, Ts0, Ds0, Fdb, D, Sub, Acc);
        false ->
            I = I0#b_set{args=Args},
            Ts = update_types(I, Ts0, Ds0),
            Ds = Ds0#{Dst=>I},
            opt_is(Is, Ts, Ds, Fdb, D, Sub0, [I|Acc])
    end;
opt_is([#b_set{op=call,args=Args0,dst=Dst}=I0|Is],
       Ts0, Ds0, Fdb0, D, Sub0, Acc) ->
    Args = simplify_args(Args0, Sub0, Ts0),
    I1 = beam_ssa:normalize(I0#b_set{args=Args}),
    {Ts1,Ds,Fdb,I2} = opt_call(I1, D, Ts0, Ds0, Fdb0),
    case {map_get(Dst, Ts1),Is} of
        {Type,[#b_set{op=succeeded}]} when Type =/= none ->
            %% This call instruction is inside a try/catch
            %% block. Don't attempt to simplify it.
            opt_is(Is, Ts1, Ds, Fdb, D, Sub0, [I2|Acc]);
        {none,[#b_set{op=succeeded}]} ->
            %% This call instruction is inside a try/catch
            %% block, but we know it will never return and
            %% later optimizations may try to exploit that.
            %%
            %% For example, if we have an expression that
            %% either returns this call or a tuple, we know
            %% that the expression always returns a tuple
            %% and can turn a later element/3 into
            %% get_tuple_element.
            %%
            %% This is sound but difficult to validate in a
            %% meaningful way as try/catch currently forces
            %% us to maintain the illusion that the success
            %% block is reachable even when its not, so we
            %% disable the optimization to keep things
            %% simple.
            Ts = Ts1#{ Dst := any },
            opt_is(Is, Ts, Ds, Fdb, D, Sub0, [I2|Acc]);
        {none,_} ->
            %% This call never returns. The rest of the
            %% instructions will not be executed.
            Ret = #b_ret{arg=Dst},
            {no_return,Ret,reverse(Acc, [I2]),Ds,Fdb,Sub0};
        {_,_} ->
            case simplify_call(I2) of
                #b_set{}=I ->
                    opt_is(Is, Ts1, Ds, Fdb, D, Sub0, [I|Acc]);
                #b_literal{}=Lit ->
                    Sub = Sub0#{Dst=>Lit},
                    Ts = maps:remove(Dst, Ts1),
                    opt_is(Is, Ts, Ds0, Fdb, D, Sub, Acc);
                #b_var{}=Var ->
                    Ts = maps:remove(Dst, Ts1),
                    Sub = Sub0#{Dst=>Var},
                    opt_is(Is, Ts, Ds0, Fdb, D, Sub, Acc)
            end
    end;
opt_is([#b_set{op=succeeded,args=[Arg],dst=Dst}=I],
       Ts0, Ds0, Fdb, D, Sub0, Acc) ->
    case Ds0 of
        #{ Arg := #b_set{op=call} } ->
            %% The success check of a call is part of exception handling and
            %% must not be optimized away. We still have to update its type
            %% though.
            Ts = update_types(I, Ts0, Ds0),
            Ds = Ds0#{Dst=>I},

            opt_is([], Ts, Ds, Fdb, D, Sub0, [I|Acc]);
        #{} ->
            Args = simplify_args([Arg], Sub0, Ts0),
            Type = type(succeeded, Args, Ts0, Ds0),
            case get_literal_from_type(Type) of
                #b_literal{}=Lit ->
                    Sub = Sub0#{Dst=>Lit},
                    opt_is([], Ts0, Ds0, Fdb, D, Sub, Acc);
                none ->
                    Ts = Ts0#{Dst=>Type},
                    Ds = Ds0#{Dst=>I},
                    opt_is([], Ts, Ds, Fdb, D, Sub0, [I|Acc])
            end
    end;
opt_is([#b_set{args=Args0,dst=Dst}=I0|Is],
       Ts0, Ds0, Fdb, D, Sub0, Acc) ->
    Args = simplify_args(Args0, Sub0, Ts0),
    I1 = beam_ssa:normalize(I0#b_set{args=Args}),
    case simplify(I1, Ts0) of
        #b_set{}=I2 ->
            I = beam_ssa:normalize(I2),
            Ts = update_types(I, Ts0, Ds0),
            Ds = Ds0#{Dst=>I},
            opt_is(Is, Ts, Ds, Fdb, D, Sub0, [I|Acc]);
        #b_literal{}=Lit ->
            Sub = Sub0#{Dst=>Lit},
            opt_is(Is, Ts0, Ds0, Fdb, D, Sub, Acc);
        #b_var{}=Var ->
            case Is of
                [#b_set{op=succeeded,dst=SuccDst,args=[Dst]}] ->
                    %% We must remove this 'succeeded' instruction.
                    Sub = Sub0#{Dst=>Var,SuccDst=>#b_literal{val=true}},
                    opt_is([], Ts0, Ds0, Fdb, D, Sub, Acc);
                _ ->
                    Sub = Sub0#{Dst=>Var},
                    opt_is(Is, Ts0, Ds0, Fdb, D, Sub, Acc)
            end
    end;
opt_is([], Ts, Ds, Fdb, _D, Sub, Acc) ->
    {reverse(Acc), Ts, Ds, Fdb, Sub}.

simplify_call(#b_set{op=call,args=[#b_remote{}=Rem|Args]}=I) ->
    case Rem of
        #b_remote{mod=#b_literal{val=Mod},
                  name=#b_literal{val=Name}} ->
            case erl_bifs:is_pure(Mod, Name, length(Args)) of
                true ->
                    simplify_remote_call(Mod, Name, Args, I);
                false ->
                    I
            end;
         #b_remote{} ->
            I
    end;
simplify_call(I) -> I.

%% Simplify a remote call to a pure BIF.
simplify_remote_call(erlang, '++', [#b_literal{val=[]},Tl], _I) ->
    Tl;
simplify_remote_call(erlang, setelement,
                     [#b_literal{val=Pos},
                      #b_literal{val=Tuple},
                      #b_var{}=Value], I)
  when is_integer(Pos), 1 =< Pos, Pos =< tuple_size(Tuple) ->
    %% Position is a literal integer and the shape of the
    %% tuple is known.
    Els0 = [#b_literal{val=El} || El <- tuple_to_list(Tuple)],
    {Bef,[_|Aft]} = split(Pos - 1, Els0),
    Els = Bef ++ [Value|Aft],
    I#b_set{op=put_tuple,args=Els};
simplify_remote_call(Mod, Name, Args0, I) ->
    case make_literal_list(Args0) of
        none ->
            I;
        Args ->
            %% The arguments are literals. Try to evaluate the BIF.
            try apply(Mod, Name, Args) of
                Val ->
                    case cerl:is_literal_term(Val) of
                        true ->
                            #b_literal{val=Val};
                        false ->
                            %% The value can't be expressed as a literal
                            %% (e.g. a pid).
                            I
                    end
            catch
                _:_ ->
                    %% Failed. Don't bother trying to optimize
                    %% the call.
                    I
            end
    end.

opt_call(#b_set{dst=Dst,args=[#b_local{}=Callee|Args]}=I0, D, Ts0, Ds0, Fdb0) ->
    {Ts, Ds, I} = opt_local_call(I0, Ts0, Ds0, Fdb0),
    case Fdb0 of
        #{ Callee := #func_info{exported=false,arg_types=ArgTypes0}=Info } ->
            %% Update the argument types of *this exact call*, the types
            %% will be joined later when the callee is optimized.
            CallId = {D#d.func_id, Dst},
            ArgTypes = update_arg_types(Args, ArgTypes0, CallId, Ts0),

            Fdb = Fdb0#{ Callee => Info#func_info{arg_types=ArgTypes} },
            {Ts, Ds, Fdb, I};
        #{} ->
            %% We can't narrow the argument types of exported functions as they
            %% can receive anything as part of an external call.
            {Ts, Ds, Fdb0, I}
    end;
opt_call(#b_set{dst=Dst}=I, _D, Ts0, Ds0, Fdb) ->
    Ts = update_types(I, Ts0, Ds0),
    Ds = Ds0#{ Dst => I },
    {Ts, Ds, Fdb, I}.

opt_local_call(#b_set{dst=Dst,args=[Id|_]}=I0, Ts0, Ds0, Fdb) ->
    Type = case Fdb of
               #{ Id := #func_info{ret_type=[T]} } -> T;
               #{} -> any
           end,
    I = case Type of
            any -> I0;
            none -> I0;
            _ -> beam_ssa:add_anno(result_type, validator_anno(Type), I0)
        end,
    Ts = Ts0#{ Dst => Type },
    Ds = Ds0#{ Dst => I },
    {Ts, Ds, I}.

update_arg_types([Arg | Args], [TypeMap0 | TypeMaps], CallId, Ts) ->
    %% Match contexts are treated as bitstrings when optimizing arguments, as
    %% we don't yet support removing the "bs_start_match3" instruction.
    NewType = case get_type(Arg, Ts) of
                  #t_bs_match{} -> {binary, 1};
                  Type -> Type
              end,
    TypeMap = TypeMap0#{ CallId => NewType },
    [TypeMap | update_arg_types(Args, TypeMaps, CallId, Ts)];
update_arg_types([], [], _CallId, _Ts) ->
    [].

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
simplify(#b_set{op={bif,element},args=[#b_literal{val=Index},Tuple]}=I0, Ts) ->
    case t_tuple_size(get_type(Tuple, Ts)) of
        {_,Size} when is_integer(Index), 1 =< Index, Index =< Size ->
            I = I0#b_set{op=get_tuple_element,
                         args=[Tuple,#b_literal{val=Index-1}]},
            simplify(I, Ts);
        _ ->
            eval_bif(I0, Ts)
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
simplify(#b_set{op={bif,'=:='},args=[A1,_A2]=Args}=I, Ts) ->
    [T1,T2] = get_types(Args, Ts),
    case meet(T1, T2) of
        none ->
            #b_literal{val=false};
        _ ->
            case {t_is_boolean(T1),T2} of
                {true,#t_atom{elements=[true]}} ->
                    %% Bool =:= true  ==>  Bool
                    A1;
                {_,_} ->
                    eval_bif(I, Ts)
            end
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
simplify(#b_set{op=get_tuple_element,args=[Tuple,#b_literal{val=N}]}=I, Ts) ->
    case get_type(Tuple, Ts) of
        #t_tuple{size=Size,elements=Es} when Size > N ->
            ElemType = get_element_type(N + 1, Es),
            case get_literal_from_type(ElemType) of
                #b_literal{}=Lit -> Lit;
                none -> I
            end;
        none ->
            %% Will never be executed because of type conflict.
            %% #b_literal{val=ignored};
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
                args=[Src,#b_literal{val=Size},#b_literal{}=Tag]}=I, Ts) ->
    simplify_is_record(I, get_type(Src, Ts), Size, Tag, Ts);
simplify(#b_set{op=put_list,args=[#b_literal{val=H},
                                  #b_literal{val=T}]}, _Ts) ->
    #b_literal{val=[H|T]};
simplify(#b_set{op=put_tuple,args=Args}=I, _Ts) ->
    case make_literal_list(Args) of
        none -> I;
        List -> #b_literal{val=list_to_tuple(List)}
    end;
simplify(#b_set{op=wait_timeout,args=[#b_literal{val=0}]}, _Ts) ->
    #b_literal{val=true};
simplify(#b_set{op=wait_timeout,args=[#b_literal{val=infinity}]}=I, _Ts) ->
    I#b_set{op=wait,args=[]};
simplify(I, _Ts) -> I.

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
opt_terminator(#b_br{bool=#b_var{}}=Br, Ts, Ds) ->
    simplify_not(Br, Ts, Ds);
opt_terminator(#b_switch{arg=#b_literal{}}=Sw, _Ts, _Ds) ->
    beam_ssa:normalize(Sw);
opt_terminator(#b_switch{arg=#b_var{}=V}=Sw, Ts, Ds) ->
    case get_type(V, Ts) of
        any ->
            beam_ssa:normalize(Sw);
        Type ->
            beam_ssa:normalize(opt_switch(Sw, Type, Ts, Ds))
    end;
opt_terminator(#b_ret{}=Ret, _Ts, _Ds) -> Ret.


opt_switch(#b_switch{fail=Fail,list=List0}=Sw0, Type, Ts, Ds) ->
    List = prune_switch_list(List0, Fail, Type, Ts),
    Sw1 = Sw0#b_switch{list=List},
    case Type of
        #t_integer{elements={_,_}=Range} ->
            simplify_switch_int(Sw1, Range);
        #t_atom{elements=[_|_]} ->
            case t_is_boolean(Type) of
                true ->
                    #b_br{} = Br = simplify_switch_bool(Sw1, Ts, Ds),
                    opt_terminator(Br, Ts, Ds);
                false ->
                    simplify_switch_atom(Type, Sw1)
            end;
        _ ->
            Sw1
    end.

prune_switch_list([{_,Fail}|T], Fail, Type, Ts) ->
    prune_switch_list(T, Fail, Type, Ts);
prune_switch_list([{Arg,_}=Pair|T], Fail, Type, Ts) ->
    case meet(get_type(Arg, Ts), Type) of
        none ->
            %% Different types. This value can never match.
            prune_switch_list(T, Fail, Type, Ts);
        _ ->
            [Pair|prune_switch_list(T, Fail, Type, Ts)]
    end;
prune_switch_list([], _, _, _) -> [].

update_successors(#b_br{bool=#b_literal{val=true},succ=S}, Ts, D) ->
    update_successor(S, Ts, D);
update_successors(#b_br{bool=#b_var{}=Bool,succ=Succ,fail=Fail}, Ts0, D0) ->
    case cerl_sets:is_element(Bool, D0#d.once) of
        true ->
            %% This variable is defined in this block and is only
            %% referenced by this br terminator. Therefore, there is
            %% no need to include it in the type database passed on to
            %% the successors of this block.
            Ts = maps:remove(Bool, Ts0),
            {SuccTs,FailTs} = infer_types_br(Bool, Ts, D0),
            D = update_successor(Fail, FailTs, D0),
            update_successor(Succ, SuccTs, D);
        false ->
            {SuccTs,FailTs} = infer_types_br(Bool, Ts0, D0),
            D = update_successor_bool(Bool, false, Fail, FailTs, D0),
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
            %% V can not be equal to any of the values in List at the fail
            %% block.
            FailTs = subtract_sw_list(V, List, Ts),
            D = update_successor(Fail, FailTs, D0),
            F = fun({Val,S}, A) ->
                        SuccTs = infer_types_switch(V, Val, Ts, D),
                        update_successor(S, SuccTs, A)
                end,
            foldl(F, D, List)
    end;
update_successors(#b_ret{arg=Arg}, Ts, D) ->
    FuncId = D#d.func_id,
    case D#d.ds of
        #{ Arg := #b_set{op=call,args=[FuncId | _]} } ->
            %% Returning a call to ourselves doesn't affect our own return
            %% type.
            D;
        #{} ->
            RetType = join([get_type(Arg, Ts) | D#d.ret_type]),
            D#d{ret_type=[RetType]}
    end.

subtract_sw_list(V, List, Ts) ->
    Ts#{ V := sub_sw_list_1(get_type(V, Ts), List, Ts) }.

sub_sw_list_1(Type, [{Val,_}|T], Ts) ->
    ValType = get_type(Val, Ts),
    sub_sw_list_1(subtract(Type, ValType), T, Ts);
sub_sw_list_1(Type, [], _Ts) ->
    Type.

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
type(bs_init, _Args, _Ts, _Ds) ->
    {binary, 1};
type(bs_extract, [Ctx], Ts, _Ds) ->
    #t_bs_match{type=Type} = get_type(Ctx, Ts),
    Type;
type(bs_match, Args, _Ts, _Ds) ->
    #t_bs_match{type=bs_match_type(Args)};
type(bs_get_tail, _Args, _Ts, _Ds) ->
    {binary, 1};
type(call, [#b_remote{mod=#b_literal{val=Mod},
                      name=#b_literal{val=Name}}|Args], Ts, _Ds) ->
    case {Mod,Name,Args} of
        {erlang,setelement,[Pos,Tuple,Arg]} ->
            case {get_type(Pos, Ts),get_type(Tuple, Ts)} of
                {#t_integer{elements={Index,Index}},
                 #t_tuple{elements=Es0,size=Size}=T} ->
                    %% This is an exact index, update the type of said element
                    %% or return 'none' if it's known to be out of bounds.
                    Es = set_element_type(Index, get_type(Arg, Ts), Es0),
                    case T#t_tuple.exact of
                        false ->
                            T#t_tuple{size=max(Index, Size),elements=Es};
                        true when Index =< Size ->
                            T#t_tuple{elements=Es};
                        true ->
                            none
                    end;
                {#t_integer{elements={Min,_}}=IntType,
                 #t_tuple{elements=Es0,size=Size}=T} ->
                    %% Remove type information for all indices that
                    %% falls into the range of the integer.
                    Es = remove_element_info(IntType, Es0),
                    case T#t_tuple.exact of
                        false ->
                            T#t_tuple{elements=Es,size=max(Min, Size)};
                        true when Min =< Size ->
                            T#t_tuple{elements=Es,size=Size};
                        true ->
                            none
                    end;
                {_,#t_tuple{}=T} ->
                    %% Position unknown, so we have to discard all element
                    %% information.
                    T#t_tuple{elements=#{}};
                {#t_integer{elements={Min,_Max}},_} ->
                    #t_tuple{size=Min};
                {_,_} ->
                    #t_tuple{}
            end;
        {erlang,'++',[LHS,RHS]} ->
            LType = get_type(LHS, Ts),
            RType = get_type(RHS, Ts),
            case LType =:= cons orelse RType =:= cons of
                true ->
                    cons;
                false ->
                    %% `[] ++ RHS` yields RHS, even if RHS is not a list.
                    join(list, RType)
            end;
        {erlang,'--',[_,_]} ->
            list;
        {lists,F,Args} ->
            Types = get_types(Args, Ts),
            lists_function_type(F, Types);
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
type(get_tuple_element, [Tuple, Offset], Ts, _Ds) ->
    #t_tuple{size=Size,elements=Es} = get_type(Tuple, Ts),
    #b_literal{val=N} = Offset,
    true = Size > N, %Assertion.
    get_element_type(N + 1, Es);
type(is_nonempty_list, [_], _Ts, _Ds) ->
    t_boolean();
type(is_tagged_tuple, [_,#b_literal{},#b_literal{}], _Ts, _Ds) ->
    t_boolean();
type(put_map, _Args, _Ts, _Ds) ->
    map;
type(put_list, _Args, _Ts, _Ds) ->
    cons;
type(put_tuple, Args, Ts, _Ds) ->
    {Es, _} = foldl(fun(Arg, {Es0, Index}) ->
                        Type = get_type(Arg, Ts),
                        Es = set_element_type(Index, Type, Es0),
                        {Es, Index + 1}
                    end, {#{}, 1}, Args),
    #t_tuple{exact=true,size=length(Args),elements=Es};
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
type(succeeded, [#b_literal{}], _Ts, _Ds) ->
    t_atom(true);
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

lists_function_type(F, Types) ->
    case {F,Types} of
        %% Functions that return booleans.
        {all,[_,_]} ->
            t_boolean();
        {any,[_,_]} ->
            t_boolean();
        {keymember,[_,_,_]} ->
            t_boolean();
        {member,[_,_]} ->
            t_boolean();
        {prefix,[_,_]} ->
            t_boolean();
        {suffix,[_,_]} ->
            t_boolean();

        %% Functions that return lists.
        {dropwhile,[_,_]} ->
            list;
        {duplicate,[_,_]} ->
            list;
        {filter,[_,_]} ->
            list;
        {flatten,[_]} ->
            list;
        {map,[_Fun,List]} ->
            same_length_type(List);
        {MapFold,[_Fun,_Acc,List]} when MapFold =:= mapfoldl;
                                        MapFold =:= mapfoldr ->
            #t_tuple{size=2,exact=true,
                     elements=#{1=>same_length_type(List)}};
        {partition,[_,_]} ->
            t_two_tuple(list, list);
        {reverse,[List]} ->
            same_length_type(List);
        {sort,[List]} ->
            same_length_type(List);
        {splitwith,[_,_]} ->
            t_two_tuple(list, list);
        {takewhile,[_,_]} ->
            list;
        {unzip,[List]} ->
            ListType = same_length_type(List),
            t_two_tuple(ListType, ListType);
        {usort,[List]} ->
            same_length_type(List);
        {zip,[_,_]} ->
            list;
        {zipwith,[_,_,_]} ->
            list;
        {_,_} ->
            any
    end.

%% For a lists function that return a list of the same
%% length as the input list, return the type of the list.
same_length_type(cons) -> cons;
same_length_type(nil) -> nil;
same_length_type(_) -> list.

t_two_tuple(Type1, Type2) ->
    #t_tuple{size=2,exact=true,
             elements=#{1=>Type1,2=>Type2}}.

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

simplify_switch_atom(#t_atom{elements=Atoms}, #b_switch{list=List0}=Sw) ->
    case sort([A || {#b_literal{val=A},_} <- List0]) of
        Atoms ->
            %% All possible atoms are included in the list. The
            %% failure label will never be used.
            [{_,Fail}|List] = List0,
            Sw#b_switch{fail=Fail,list=List};
        _ ->
            Sw
    end.

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

simplify_is_record(I, #t_tuple{exact=Exact,
                               size=Size,
                               elements=Es},
                   RecSize, RecTag, Ts) ->
    TagType = maps:get(1, Es, any),
    TagMatch = case get_literal_from_type(TagType) of
                   #b_literal{}=RecTag -> yes;
                   #b_literal{} -> no;
                   none ->
                       %% Is it at all possible for the tag to match?
                       case meet(get_type(RecTag, Ts), TagType) of
                           none -> no;
                           _ -> maybe
                       end
               end,
    if
        Size =/= RecSize, Exact; Size > RecSize; TagMatch =:= no ->
            #b_literal{val=false};
        Size =:= RecSize, Exact, TagMatch =:= yes ->
            #b_literal{val=true};
        true ->
            I
    end;
simplify_is_record(I, any, _Size, _Tag, _Ts) ->
    I;
simplify_is_record(_I, _Type, _Size, _Tag, _Ts) ->
    #b_literal{val=false}.

simplify_switch_bool(#b_switch{arg=B,fail=Fail,list=List0}, Ts, Ds) ->
    FalseVal = #b_literal{val=false},
    TrueVal = #b_literal{val=true},
    List1 = List0 ++ [{FalseVal,Fail},{TrueVal,Fail}],
    {_,FalseLbl} = keyfind(FalseVal, 1, List1),
    {_,TrueLbl} = keyfind(TrueVal, 1, List1),
    Br = beam_ssa:normalize(#b_br{bool=B,succ=TrueLbl,fail=FalseLbl}),
    simplify_not(Br, Ts, Ds).

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
    end;
simplify_not(#b_br{bool=#b_literal{}}=Br, _Ts, _Ds) -> Br.

%%%
%%% Calculate the set of variables that are only used once in the
%%% terminator of the block that defines them. That will allow us to
%%% discard type information for variables that will never be
%%% referenced by the successor blocks, potentially improving
%%% compilation times.
%%%

used_once(Linear, Args) ->
    Map0 = used_once_1(reverse(Linear), #{}),
    Map = maps:without(Args, Map0),
    cerl_sets:from_list(maps:keys(Map)).

used_once_1([{L,#b_blk{is=Is,last=Last}}|Bs], Uses0) ->
    Uses1 = used_once_last_uses(beam_ssa:used(Last), L, Uses0),
    Uses = used_once_2(reverse(Is), L, Uses1),
    used_once_1(Bs, Uses);
used_once_1([], Uses) -> Uses.

used_once_2([#b_set{dst=Dst}=I|Is], L, Uses0) ->
    Uses = used_once_uses(beam_ssa:used(I), L, Uses0),
    case Uses of
        #{Dst:=[L]} ->
            used_once_2(Is, L, Uses);
        #{} ->
            %% Used more than once or used once in
            %% in another block.
            used_once_2(Is, L, maps:remove(Dst, Uses))
    end;
used_once_2([], _, Uses) -> Uses.

used_once_uses([V|Vs], L, Uses) ->
    case Uses of
        #{V:=more_than_once} ->
            used_once_uses(Vs, L, Uses);
        #{} ->
            %% Already used or first use is not in
            %% a terminator.
            used_once_uses(Vs, L, Uses#{V=>more_than_once})
    end;
used_once_uses([], _, Uses) -> Uses.

used_once_last_uses([V|Vs], L, Uses) ->
    case Uses of
        #{V:=[_]} ->
            %% Second time this variable is used.
            used_once_last_uses(Vs, L, Uses#{V:=more_than_once});
        #{V:=more_than_once} ->
            %% Used at least twice before.
            used_once_last_uses(Vs, L, Uses);
        #{} ->
            %% First time this variable is used.
            used_once_last_uses(Vs, L, Uses#{V=>[L]})
    end;
used_once_last_uses([], _, Uses) -> Uses.


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
            {Es, _} = foldl(fun(E, {Es0, Index}) ->
                                Type = get_type(#b_literal{val=E}, #{}),
                                Es = set_element_type(Index, Type, Es0),
                                {Es, Index + 1}
                            end, {#{}, 1}, tuple_to_list(Val)),
            #t_tuple{exact=true,size=tuple_size(Val),elements=Es};
        Val =:= [] ->
            nil;
        true ->
            any
    end.

%% infer_types(Var, Types, #d{}) -> {SuccTypes,FailTypes}
%%  Looking at the expression that defines the variable Var, infer
%%  the types for the variables in the arguments. Return the updated
%%  type database for the case that the expression evaluates to
%%  true, and and for the case that it evaluates to false.
%%
%%  Here is an example. The variable being asked about is
%%  the variable Bool, which is defined like this:
%%
%%     Bool = is_nonempty_list L
%%
%%  If 'is_nonempty_list L' evaluates to 'true', L must
%%  must be cons. The meet of the previously known type of L and 'cons'
%%  will be added to SuccTypes.
%%
%%  On the other hand, if 'is_nonempty_list L' evaluates to false, L
%%  is not cons and cons can be subtracted from the previously known
%%  type for L. For example, if L was known to be 'list', subtracting
%%  'cons' would give 'nil' as the only possible type. The result of the
%%  subtraction for L will be added to FailTypes.

infer_types_br(#b_var{}=V, Ts, #d{ds=Ds}) ->
    #{V:=#b_set{op=Op,args=Args}} = Ds,
    PosTypes0 = infer_type(Op, Args, Ds),
    NegTypes0 = infer_type_negative(Op, Args, Ds),

    %% We must be careful with types inferred from '=:='.
    %%
    %% If we have seen L =:= [a], we know that L is 'cons' if the
    %% comparison succeeds. However, if the comparison fails, L could
    %% still be 'cons'. Therefore, we must not subtract 'cons' from the
    %% previous type of L.
    %%
    %% However, it is safe to subtract a type inferred from '=:=' if
    %% it is single-valued, e.g. if it is [] or the atom 'true'.

    EqTypes = infer_eq_type(Op, Args, Ts, Ds),
    NegTypes1 = [P || {_,T}=P <- EqTypes, is_singleton_type(T)],

    PosTypes = EqTypes ++ PosTypes0,
    SuccTs = meet_types(PosTypes, Ts),

    NegTypes = NegTypes0 ++ NegTypes1,
    FailTs = subtract_types(NegTypes, Ts),

    {SuccTs,FailTs}.

infer_types_switch(V, Lit, Ts, #d{ds=Ds}) ->
    Types = infer_eq_type({bif,'=:='}, [V, Lit], Ts, Ds),
    meet_types(Types, Ts).

infer_eq_type({bif,'=:='}, [#b_var{}=Src,#b_literal{}=Lit], Ts, Ds) ->
    Def = maps:get(Src, Ds),
    Type = get_type(Lit, Ts),
    [{Src,Type} | infer_eq_lit(Def, Lit)];
infer_eq_type({bif,'=:='}, [#b_var{}=Arg0,#b_var{}=Arg1], Ts, _Ds) ->
    %% As an example, assume that L1 is known to be 'list', and L2 is
    %% known to be 'cons'. Then if 'L1 =:= L2' evaluates to 'true', it can
    %% be inferred that L1 is 'cons' (the meet of 'cons' and 'list').
    Type0 = get_type(Arg0, Ts),
    Type1 = get_type(Arg1, Ts),
    Type = meet(Type0, Type1),
    [{V,MeetType} ||
        {V,OrigType,MeetType} <-
            [{Arg0,Type0,Type},{Arg1,Type1,Type}],
        OrigType =/= MeetType];
infer_eq_type(_Op, _Args, _Ts, _Ds) ->
    [].

infer_eq_lit(#b_set{op={bif,tuple_size},args=[#b_var{}=Tuple]},
             #b_literal{val=Size}) when is_integer(Size) ->
    [{Tuple,#t_tuple{exact=true,size=Size}}];
infer_eq_lit(#b_set{op=get_tuple_element,
                    args=[#b_var{}=Tuple,#b_literal{val=N}]},
             #b_literal{}=Lit) ->
    Index = N + 1,
    Es = set_element_type(Index, get_type(Lit, #{}), #{}),
    [{Tuple,#t_tuple{size=Index,elements=Es}}];
infer_eq_lit(_, _) -> [].

infer_type_negative(Op, Args, Ds) ->
    case is_negative_inference_safe(Op, Args) of
        true ->
            infer_type(Op, Args, Ds);
        false ->
            []
    end.

%% Conservative list of instructions for which negative
%% inference is safe.
is_negative_inference_safe(is_nonempty_list, _Args) -> true;
is_negative_inference_safe(_, _) -> false.

infer_type({bif,element}, [#b_literal{val=Pos},#b_var{}=Tuple], _Ds) ->
    if
        is_integer(Pos), 1 =< Pos ->
            [{Tuple,#t_tuple{size=Pos}}];
        true ->
            []
    end;
infer_type({bif,element}, [#b_var{}=Position,#b_var{}=Tuple], _Ds) ->
    [{Position,t_integer()},{Tuple,#t_tuple{}}];
infer_type({bif,Bif}, [#b_var{}=Src]=Args, _Ds) ->
    case inferred_bif_type(Bif, Args) of
        any -> [];
        T -> [{Src,T}]
    end;
infer_type({bif,binary_part}, [#b_var{}=Src,_], _Ds) ->
    [{Src,{binary,8}}];
infer_type({bif,is_map_key}, [_,#b_var{}=Src], _Ds) ->
    [{Src,map}];
infer_type({bif,map_get}, [_,#b_var{}=Src], _Ds) ->
    [{Src,map}];
infer_type({bif,Bif}, [_,_]=Args, _Ds) ->
    case inferred_bif_type(Bif, Args) of
        any -> [];
        T -> [{A,T} || #b_var{}=A <- Args]
    end;
infer_type({bif,binary_part}, [#b_var{}=Src,Pos,Len], _Ds) ->
    [{Src,{binary,8}}|
     [{V,t_integer()} || #b_var{}=V <- [Pos,Len]]];
infer_type(bs_start_match, [#b_var{}=Bin], _Ds) ->
    [{Bin,{binary,1}}];
infer_type(is_nonempty_list, [#b_var{}=Src], _Ds) ->
    [{Src,cons}];
infer_type(is_tagged_tuple, [#b_var{}=Src,#b_literal{val=Size},
                             #b_literal{}=Tag], _Ds) ->
    Es = set_element_type(1, get_type(Tag, #{}), #{}),
    [{Src,#t_tuple{exact=true,size=Size,elements=Es}}];
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
inferred_bif_type('bnot', [_]) -> t_integer();
inferred_bif_type(byte_size, [_]) -> {binary,1};
inferred_bif_type(ceil, [_]) -> number;
inferred_bif_type(float, [_]) -> number;
inferred_bif_type(floor, [_]) -> number;
inferred_bif_type(hd, [_]) -> cons;
inferred_bif_type(length, [_]) -> list;
inferred_bif_type(map_size, [_]) -> map;
inferred_bif_type('not', [_]) -> t_boolean();
inferred_bif_type(round, [_]) -> number;
inferred_bif_type(trunc, [_]) -> number;
inferred_bif_type(tl, [_]) -> cons;
inferred_bif_type(tuple_size, [_]) -> #t_tuple{};
inferred_bif_type('and', [_,_]) -> t_boolean();
inferred_bif_type('or', [_,_]) -> t_boolean();
inferred_bif_type('xor', [_,_]) -> t_boolean();
inferred_bif_type('band', [_,_]) -> t_integer();
inferred_bif_type('bor', [_,_]) -> t_integer();
inferred_bif_type('bsl', [_,_]) -> t_integer();
inferred_bif_type('bsr', [_,_]) -> t_integer();
inferred_bif_type('bxor', [_,_]) -> t_integer();
inferred_bif_type('div', [_,_]) -> t_integer();
inferred_bif_type('rem', [_,_]) -> t_integer();
inferred_bif_type('+', [_,_]) -> number;
inferred_bif_type('-', [_,_]) -> number;
inferred_bif_type('*', [_,_]) -> number;
inferred_bif_type('/', [_,_]) -> number;
inferred_bif_type(_, _) -> any.

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

remove_element_info(#t_integer{elements={Min,Max}}, Es) ->
    foldl(fun(El, Acc) when Min =< El, El =< Max ->
                  maps:remove(El, Acc);
             (_El, Acc) -> Acc
          end, Es, maps:keys(Es)).

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

is_singleton_type(Type) ->
    get_literal_from_type(Type) =/= none.

get_element_type(Index, Es) ->
    case Es of
        #{ Index := T } -> T;
        #{} -> any
    end.

set_element_type(_Key, none, Es) ->
    Es;
set_element_type(Key, any, Es) ->
    maps:remove(Key, Es);
set_element_type(Key, Type, Es) ->
    Es#{ Key => Type }.

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
join(#t_tuple{size=Sz,exact=ExactA,elements=EsA},
     #t_tuple{size=Sz,exact=ExactB,elements=EsB}) ->
    Exact = ExactA and ExactB,
    Es = join_tuple_elements(Sz, EsA, EsB),
    #t_tuple{size=Sz,exact=Exact,elements=Es};
join(#t_tuple{size=SzA,elements=EsA}, #t_tuple{size=SzB,elements=EsB}) ->
    Sz = min(SzA, SzB),
    Es = join_tuple_elements(Sz, EsA, EsB),
    #t_tuple{size=Sz,elements=Es};
join(_T1, _T2) ->
    %%io:format("~p ~p\n", [_T1,_T2]),
    any.

join_tuple_elements(MinSize, EsA, EsB) ->
    Es0 = join_elements(EsA, EsB),
    maps:filter(fun(Index, _Type) -> Index =< MinSize end, Es0).

join_elements(Es1, Es2) ->
    Keys = if
               map_size(Es1) =< map_size(Es2) -> maps:keys(Es1);
               map_size(Es1) > map_size(Es2) -> maps:keys(Es2)
           end,
    join_elements_1(Keys, Es1, Es2, #{}).

join_elements_1([Key | Keys], Es1, Es2, Acc0) ->
    case {Es1, Es2} of
        {#{ Key := Type1 }, #{ Key := Type2 }} ->
            Acc = set_element_type(Key, join(Type1, Type2), Acc0),
            join_elements_1(Keys, Es1, Es2, Acc);
        {#{}, #{}} ->
            join_elements_1(Keys, Es1, Es2, Acc0)
    end;
join_elements_1([], _Es1, _Es2, Acc) ->
    Acc.

gcd(A, B) ->
    case A rem B of
        0 -> B;
        X -> gcd(B, X)
    end.

meet_types([{V,T0}|Vs], Ts) ->
    #{V:=T1} = Ts,
    case meet(T0, T1) of
        T1 -> meet_types(Vs, Ts);
        T -> meet_types(Vs, Ts#{V:=T})
    end;
meet_types([], Ts) -> Ts.

meet([T1,T2|Ts]) ->
    meet([meet(T1, T2)|Ts]);
meet([T]) -> T.

subtract_types([{V,T0}|Vs], Ts) ->
    #{V:=T1} = Ts,
    case subtract(T1, T0) of
        T1 -> subtract_types(Vs, Ts);
        T -> subtract_types(Vs, Ts#{V:=T})
    end;
subtract_types([], Ts) -> Ts.

%% subtract(Type1, Type2) -> Type.
%%  Subtract Type2 from Type1. Example:
%%
%%     subtract(list, cons) -> nil

subtract(#t_atom{elements=[_|_]=Set0}, #t_atom{elements=[_|_]=Set1}) ->
    case ordsets:subtract(Set0, Set1) of
        [] -> none;
        [_|_]=Set -> #t_atom{elements=Set}
    end;
subtract(number, float) -> #t_integer{};
subtract(number, #t_integer{elements=any}) -> float;
subtract(list, cons) -> nil;
subtract(list, nil) -> cons;
subtract(T, _) -> T.

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

meet_tuples(#t_tuple{size=Sz1,exact=true},
            #t_tuple{size=Sz2,exact=true}) when Sz1 =/= Sz2 ->
    none;
meet_tuples(#t_tuple{size=Sz1,exact=Ex1,elements=Es1},
            #t_tuple{size=Sz2,exact=Ex2,elements=Es2}) ->
    Size = max(Sz1, Sz2),
    Exact = Ex1 or Ex2,
    case meet_elements(Es1, Es2) of
        none ->
            none;
        Es ->
            #t_tuple{size=Size,exact=Exact,elements=Es}
    end.

meet_elements(Es1, Es2) ->
    Keys = maps:keys(Es1) ++ maps:keys(Es2),
    meet_elements_1(Keys, Es1, Es2, #{}).

meet_elements_1([Key | Keys], Es1, Es2, Acc) ->
    case {Es1, Es2} of
        {#{ Key := Type1 }, #{ Key := Type2 }} ->
            case meet(Type1, Type2) of
                none -> none;
                Type -> meet_elements_1(Keys, Es1, Es2, Acc#{ Key => Type })
            end;
        {#{ Key := Type1 }, _} ->
            meet_elements_1(Keys, Es1, Es2, Acc#{ Key => Type1 });
        {_, #{ Key := Type2 }} ->
            meet_elements_1(Keys, Es1, Es2, Acc#{ Key => Type2 })
    end;
meet_elements_1([], _Es1, _Es2, Acc) ->
    Acc.

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
verified_type(#t_tuple{size=Size,elements=Es}=T) ->
    %% All known elements must have a valid index and type. 'any' is prohibited
    %% since it's implicit and should never be present in the map.
    maps:fold(fun(Index, Element, _) when is_integer(Index),
                                          1 =< Index, Index =< Size,
                                          Element =/= any, Element =/= none ->
                      verified_type(Element)
              end, [], Es),
    T;
verified_type(float=T) -> T.
