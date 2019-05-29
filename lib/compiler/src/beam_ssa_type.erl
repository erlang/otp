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
-include("beam_types.hrl").

-import(lists, [all/2,any/2,droplast/1,foldl/3,last/1,member/2,
                keyfind/3,reverse/1,reverse/2,
                sort/1,split/2,zip/2]).

-define(UNICODE_MAX, (16#10FFFF)).

-record(d,
        {ds :: #{beam_ssa:b_var():=beam_ssa:b_set()},
         ls :: #{beam_ssa:label():=type_db()},
         once :: cerl_sets:set(beam_ssa:b_var()),
         func_id :: func_id(),
         func_db :: func_info_db(),
         sub = #{} :: #{beam_ssa:b_var():=beam_ssa:value()},
         ret_type = [] :: [type()]}).

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
    join_arg_types_1(Args, TMs, Ts#{ Arg => beam_types:join(maps:values(TM))});
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
    JoinedType = beam_types:join(maps:values(TypeMap)),
    ParamInfo = case JoinedType of
                    any -> ParamInfo0;
                    _ -> ParamInfo0#{ Arg => JoinedType }
                end,
    opt_finish_1(Args, TypeMaps, ParamInfo);
opt_finish_1([], [], ParamInfo) ->
    ParamInfo.

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
            RetType = beam_types:join([none|D0#d.ret_type]),
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
            case beam_types:get_singleton_value(Type) of
                {ok, Lit} ->
                    Sub = Sub0#{Dst=>#b_literal{val=Lit}},
                    opt_is([], Ts0, Ds0, Fdb, D, Sub, Acc);
                error ->
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

opt_call(#b_set{dst=Dst,args=[#b_var{}=Fun|Args]}=I, _D, Ts0, Ds0, Fdb) ->
    Type = #t_fun{arity=length(Args)},
    Ts = Ts0#{ Fun => Type, Dst => any },
    Ds = Ds0#{ Dst => I },
    {Ts, Ds, Fdb, I};
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
            _ -> beam_ssa:add_anno(result_type, Type, I0)
        end,
    Ts = Ts0#{ Dst => Type },
    Ds = Ds0#{ Dst => I },
    {Ts, Ds, I}.

update_arg_types([Arg | Args], [TypeMap0 | TypeMaps], CallId, Ts) ->
    %% Match contexts are treated as bitstrings when optimizing arguments, as
    %% we don't yet support removing the "bs_start_match3" instruction.
    NewType = case get_type(Arg, Ts) of
                  #t_bs_context{} -> #t_bitstring{unit=1};
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
    case beam_types:get_tuple_size(get_type(Tuple, Ts)) of
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
simplify(#b_set{op={bif,is_function},args=[Fun,#b_literal{val=Arity}]}=I, Ts)
  when is_integer(Arity), Arity >= 0 ->
    case get_type(Fun, Ts) of
        #t_fun{arity=any} ->
            I;
        #t_fun{arity=Arity} ->
            #b_literal{val=true};
        any ->
            I;
        _ ->
            #b_literal{val=false}
    end;
simplify(#b_set{op={bif,Op0},args=Args}=I, Ts) when Op0 =:= '=='; Op0 =:= '/=' ->
    Types = get_types(Args, Ts),
    EqEq0 = case {beam_types:meet(Types),beam_types:join(Types)} of
               {none,any} -> true;
               {#t_integer{},#t_integer{}} -> true;
               {float,float} -> true;
               {#t_bitstring{},_} -> true;
               {#t_atom{},_} -> true;
               {_,_} -> false
           end,
    EqEq = EqEq0 orelse any_non_numeric_argument(Args, Ts),
    case EqEq of
        true ->
            Op = case Op0 of
                     '==' -> '=:=';
                     '/=' -> '=/='
                 end,
            simplify(I#b_set{op={bif,Op}}, Ts);
        false ->
            eval_bif(I, Ts)
    end;
simplify(#b_set{op={bif,'=:='},args=[Same,Same]}, _Ts) ->
    #b_literal{val=true};
simplify(#b_set{op={bif,'=:='},args=[A1,_A2]=Args}=I, Ts) ->
    [T1,T2] = get_types(Args, Ts),
    case beam_types:meet(T1, T2) of
        none ->
            #b_literal{val=false};
        _ ->
            case {beam_types:is_boolean_type(T1),T2} of
                {true,#t_atom{elements=[true]}} ->
                    %% Bool =:= true  ==>  Bool
                    A1;
                {true,#t_atom{elements=[false]}} ->
                    %% Bool =:= false ==> not Bool
                    %%
                    %% This will be further optimized to eliminate the
                    %% 'not', swapping the success and failure
                    %% branches in the br instruction. If A1 comes
                    %% from a type test (such as is_atom/1) or a
                    %% comparison operator (such as >=) that can be
                    %% translated to test instruction, this
                    %% optimization will eliminate one instruction.
                    simplify(I#b_set{op={bif,'not'},args=[A1]}, Ts);
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
            ElemType = beam_types:get_element_type(N + 1, Es),
            case beam_types:get_singleton_value(ElemType) of
                {ok, Val} -> #b_literal{val=Val};
                error -> I
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

any_non_numeric_argument([#b_literal{val=Lit}|_], _Ts) ->
    is_non_numeric(Lit);
any_non_numeric_argument([#b_var{}=V|T], Ts) ->
    is_non_numeric_type(get_type(V, Ts)) orelse any_non_numeric_argument(T, Ts);
any_non_numeric_argument([], _Ts) -> false.

is_non_numeric([H|T]) ->
    is_non_numeric(H) andalso is_non_numeric(T);
is_non_numeric(Tuple) when is_tuple(Tuple) ->
    is_non_numeric_tuple(Tuple, tuple_size(Tuple));
is_non_numeric(Map) when is_map(Map) ->
    %% Note that 17.x and 18.x compare keys in different ways.
    %% Be very conservative -- require that both keys and values
    %% are non-numeric.
    is_non_numeric(maps:to_list(Map));
is_non_numeric(Num) when is_number(Num) ->
    false;
is_non_numeric(_) -> true.

is_non_numeric_tuple(Tuple, El) when El >= 1 ->
    is_non_numeric(element(El, Tuple)) andalso
	is_non_numeric_tuple(Tuple, El-1);
is_non_numeric_tuple(_Tuple, 0) -> true.

is_non_numeric_type(#t_atom{}) -> true;
is_non_numeric_type(#t_bitstring{}) -> true;
is_non_numeric_type(nil) -> true;
is_non_numeric_type(#t_tuple{size=Size,exact=true,elements=Types})
  when map_size(Types) =:= Size ->
    is_non_numeric_tuple_type(Size, Types);
is_non_numeric_type(_) -> false.

is_non_numeric_tuple_type(0, _Types) ->
    true;
is_non_numeric_tuple_type(Pos, Types) ->
    is_non_numeric_type(map_get(Pos, Types)) andalso
        is_non_numeric_tuple_type(Pos - 1, Types).

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
    beam_types:is_boolean_type(T1) andalso beam_types:is_boolean_type(T2).

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
            case beam_types:get_singleton_value(Type) of
                {ok, Val} -> #b_literal{val=Val};
                error -> Arg
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
            case beam_types:is_boolean_type(Type) of
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
    case beam_types:meet(get_type(Arg, Ts), Type) of
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
            RetType = beam_types:join([get_type(Arg, Ts) | D#d.ret_type]),
            D#d{ret_type=[RetType]}
    end.

subtract_sw_list(V, List, Ts) ->
    Ts#{ V := sub_sw_list_1(get_type(V, Ts), List, Ts) }.

sub_sw_list_1(Type, [{Val,_}|T], Ts) ->
    ValType = get_type(Val, Ts),
    sub_sw_list_1(beam_types:subtract(Type, ValType), T, Ts);
sub_sw_list_1(Type, [], _Ts) ->
    Type.

update_successor_bool(#b_var{}=Var, BoolValue, S, Ts, D) ->
    case beam_types:is_boolean_type(get_type(Var, Ts)) of
        true ->
            update_successor(S, Ts#{ Var := beam_types:make_atom(BoolValue) }, D);
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
    beam_types:join(Types);
type({bif,Bif}, Args, Ts, _Ds) ->
    {RetType, _, _} = beam_call_types:types(erlang, Bif, get_types(Args, Ts)),
    RetType;
type(bs_init, _Args, _Ts, _Ds) ->
    #t_bitstring{};
type(bs_extract, [Ctx], _Ts, Ds) ->
    #b_set{op=bs_match,args=Args} = map_get(Ctx, Ds),
    bs_match_type(Args);
type(bs_match, _Args, _Ts, _Ds) ->
    #t_bs_context{};
type(bs_get_tail, _Args, _Ts, _Ds) ->
    #t_bitstring{};
type(call, [#b_remote{mod=#b_literal{val=Mod},
                      name=#b_literal{val=Name}}|Args], Ts, _Ds) ->
    {RetType, _, _} = beam_call_types:types(Mod, Name, get_types(Args, Ts)),
    RetType;
type(get_tuple_element, [Tuple, Offset], Ts, _Ds) ->
    #t_tuple{size=Size,elements=Es} = get_type(Tuple, Ts),
    #b_literal{val=N} = Offset,
    true = Size > N, %Assertion.
    beam_types:get_element_type(N + 1, Es);
type(is_nonempty_list, [_], _Ts, _Ds) ->
    beam_types:make_boolean();
type(is_tagged_tuple, [_,#b_literal{},#b_literal{}], _Ts, _Ds) ->
    beam_types:make_boolean();
type(make_fun, [#b_local{arity=TotalArity}|Env], _Ts, _Ds) ->
    #t_fun{arity=TotalArity-length(Env)};
type(put_map, _Args, _Ts, _Ds) ->
    #t_map{};
type(put_list, _Args, _Ts, _Ds) ->
    cons;
type(put_tuple, Args, Ts, _Ds) ->
    {Es, _} = foldl(fun(Arg, {Es0, Index}) ->
                            Type = get_type(Arg, Ts),
                            Es = beam_types:set_element_type(Index, Type, Es0),
                            {Es, Index + 1}
                    end, {#{}, 1}, Args),
    #t_tuple{exact=true,size=length(Args),elements=Es};
type(succeeded, [#b_var{}=Src], Ts, Ds) ->
    case maps:get(Src, Ds) of
        #b_set{op={bif,Bif},args=BifArgs} ->
            Types = get_types(BifArgs, Ts),
            case {Bif,Types} of
                {BoolOp,[T1,T2]} when BoolOp =:= 'and'; BoolOp =:= 'or' ->
                    BothBool = beam_types:is_boolean_type(T1) andalso
                        beam_types:is_boolean_type(T2),
                    case BothBool of
                        true -> beam_types:make_atom(true);
                        false -> beam_types:make_boolean()
                    end;
                {byte_size,[#t_bitstring{}]} ->
                    beam_types:make_atom(true);
                {bit_size,[#t_bitstring{}]} ->
                    beam_types:make_atom(true);
                {map_size,[#t_map{}]} ->
                    beam_types:make_atom(true);
                {'not',[Type]} ->
                    case beam_types:is_boolean_type(Type) of
                        true -> beam_types:make_atom(true);
                        false -> beam_types:make_boolean()
                    end;
                {size,[#t_bitstring{}]} ->
                    beam_types:make_atom(true);
                {tuple_size,[#t_tuple{}]} ->
                    beam_types:make_atom(true);
                {_,_} ->
                    beam_types:make_boolean()
            end;
        #b_set{op=get_hd} ->
            beam_types:make_atom(true);
        #b_set{op=get_tl} ->
            beam_types:make_atom(true);
        #b_set{op=get_tuple_element} ->
            beam_types:make_atom(true);
        #b_set{op=wait} ->
            beam_types:make_atom(false);
        #b_set{} ->
            beam_types:make_boolean()
    end;
type(succeeded, [#b_literal{}], _Ts, _Ds) ->
    beam_types:make_atom(true);
type(_, _, _, _) -> any.

%% will_succeed(TestOperation, Type) -> yes|no|maybe.
%%  Test whether TestOperation applied to an argument of type Type
%%  will succeed.  Return yes, no, or maybe.
%%
%%  Type can be any type as described in beam_types.hrl, but it must *never* be
%%  any.

will_succeed(is_atom, Type) ->
    case Type of
        #t_atom{} -> yes;
        _ -> no
    end;
will_succeed(is_binary, Type) ->
    case Type of
        #t_bitstring{unit=U} when U rem 8 =:= 0 -> yes;
        #t_bitstring{} -> maybe;
        _ -> no
    end;
will_succeed(is_bitstring, Type) ->
    case Type of
        #t_bitstring{} -> yes;
        _ -> no
    end;
will_succeed(is_boolean, Type) ->
    case Type of
        #t_atom{elements=any} ->
            maybe;
        #t_atom{elements=Es} ->
            case beam_types:is_boolean_type(Type) of
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
will_succeed(is_function, Type) ->
    case Type of
        #t_fun{} -> yes;
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
        #t_map{} -> yes;
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

bs_match_type([#b_literal{val=Type}|Args]) ->
    bs_match_type(Type, Args).

bs_match_type(binary, Args) ->
    [_,_,_,#b_literal{val=U}] = Args,
    #t_bitstring{unit=U};
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
                    beam_types:make_integer(0, (1 bsl NumBits)-1);
                false ->
                    %% Signed integer. Don't bother.
                    #t_integer{}
            end;
        [_|_] ->
            #t_integer{}
    end;
bs_match_type(skip, _) ->
    any;
bs_match_type(string, _) ->
    any;
bs_match_type(utf8, _) ->
    beam_types:make_integer(0, ?UNICODE_MAX);
bs_match_type(utf16, _) ->
    beam_types:make_integer(0, ?UNICODE_MAX);
bs_match_type(utf32, _) ->
    beam_types:make_integer(0, ?UNICODE_MAX).

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
                   RecSize, #b_literal{val=TagVal}=RecTag, Ts) ->
    TagType = maps:get(1, Es, any),
    TagMatch = case beam_types:get_singleton_value(TagType) of
                   {ok, TagVal} -> yes;
                   {ok, _} -> no;
                   error ->
                       %% Is it at all possible for the tag to match?
                       case beam_types:meet(get_type(RecTag, Ts), TagType) of
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
            case beam_types:is_boolean_type(get_type(Bool, Ts)) of
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
            beam_types:make_atom(Val);
        is_float(Val) ->
            float;
        is_function(Val) ->
            {arity,Arity} = erlang:fun_info(Val, arity),
            #t_fun{arity=Arity};
        is_integer(Val) ->
            beam_types:make_integer(Val);
        is_list(Val), Val =/= [] ->
            cons;
        is_map(Val) ->
            #t_map{};
        Val =:= {} ->
            beam_types:make_tuple(0, true);
        is_tuple(Val) ->
            {Es, _} = foldl(fun(E, {Es0, Index}) ->
                                    Type = get_type(#b_literal{val=E}, #{}),
                                    Es = beam_types:set_element_type(Index,
                                                                     Type,
                                                                     Es0),
                                    {Es, Index + 1}
                            end, {#{}, 1}, tuple_to_list(Val)),
            beam_types:make_tuple(tuple_size(Val), true, Es);
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
    {PosTypes0, NegTypes0} = infer_type(Op, Args, Ts, Ds),

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
    NegTypes1 = [P || {_,T}=P <- EqTypes, beam_types:is_singleton_type(T)],

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
    Type = beam_types:meet(Type0, Type1),
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
    Es = beam_types:set_element_type(Index, get_type(Lit, #{}), #{}),
    [{Tuple,#t_tuple{size=Index,elements=Es}}];
infer_eq_lit(_, _) -> [].

infer_type(succeeded, [#b_var{}=Src], Ts, Ds) ->
    #b_set{op=Op,args=Args} = maps:get(Src, Ds),
    infer_type(Op, Args, Ts, Ds);
infer_type(bs_start_match, [#b_var{}=Bin], _Ts, _Ds) ->
    T = {Bin,#t_bitstring{}},
    {[T], [T]};
infer_type(is_nonempty_list, [#b_var{}=Src], _Ts, _Ds) ->
    T = {Src,cons},
    {[T], [T]};
infer_type(is_tagged_tuple, [#b_var{}=Src,#b_literal{val=Size},
                             #b_literal{}=Tag], _Ts, _Ds) ->
    Es = beam_types:set_element_type(1, get_type(Tag, #{}), #{}),
    T = {Src,#t_tuple{exact=true,size=Size,elements=Es}},
    {[T], [T]};

%% Type tests are handled separately from other BIFs as we're inferring types
%% based on their result rather than whether they succeeded, so we know that
%% subtraction is always safe.
infer_type({bif,is_atom}, [Arg], _Ts, _Ds) ->
    T = {Arg, #t_atom{}},
    {[T], [T]};
infer_type({bif,is_binary}, [Arg], _Ts, _Ds) ->
    T = {Arg, #t_bitstring{unit=8}},
    {[T], [T]};
infer_type({bif,is_bitstring}, [Arg], _Ts, _Ds) ->
    T = {Arg, #t_bitstring{}},
    {[T], [T]};
infer_type({bif,is_boolean}, [Arg], _Ts, _Ds) ->
    T = {Arg, beam_types:make_boolean()},
    {[T], [T]};
infer_type({bif,is_float}, [Arg], _Ts, _Ds) ->
    T = {Arg, float},
    {[T], [T]};
infer_type({bif,is_integer}, [Arg], _Ts, _Ds) ->
    T = {Arg, #t_integer{}},
    {[T], [T]};
infer_type({bif,is_list}, [Arg], _Ts, _Ds) ->
    T = {Arg, list},
    {[T], [T]};
infer_type({bif,is_map}, [Arg], _Ts, _Ds) ->
    T = {Arg, #t_map{}},
    {[T], [T]};
infer_type({bif,is_number}, [Arg], _Ts, _Ds) ->
    T = {Arg, number},
    {[T], [T]};
infer_type({bif,is_tuple}, [Arg], _Ts, _Ds) ->
    T = {Arg, #t_tuple{}},
    {[T], [T]};

infer_type({bif,Op}, Args, Ts, _Ds) ->
    ArgTypes = get_types(Args, Ts),

    {_, PosTypes0, CanSubtract} = beam_call_types:types(erlang, Op, ArgTypes),
    PosTypes = [T || {#b_var{},_}=T <- zip(Args, PosTypes0)],

    case CanSubtract of
        true -> {PosTypes, PosTypes};
        false -> {PosTypes, []}
    end;

infer_type(_Op, _Args, _Ts, _Ds) ->
    {[], []}.

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
            case beam_types:join(T0, T1) of
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

meet_types([{V,T0}|Vs], Ts) ->
    #{V:=T1} = Ts,
    case beam_types:meet(T0, T1) of
        T1 -> meet_types(Vs, Ts);
        T -> meet_types(Vs, Ts#{V:=T})
    end;
meet_types([], Ts) -> Ts.

subtract_types([{V,T0}|Vs], Ts) ->
    #{V:=T1} = Ts,
    case beam_types:subtract(T1, T0) of
        T1 -> subtract_types(Vs, Ts);
        T -> subtract_types(Vs, Ts#{V:=T})
    end;
subtract_types([], Ts) -> Ts.
