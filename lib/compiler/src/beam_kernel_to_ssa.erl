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
%% Purpose: Convert the Kernel Erlang format to the SSA format.

-module(beam_kernel_to_ssa).

%% The main interface.
-export([module/2]).

-import(lists, [append/1,duplicate/2,flatmap/2,foldl/3,
                keysort/2,mapfoldl/3,map/2,member/2,
                reverse/1,reverse/2,sort/1]).

-include("v3_kernel.hrl").
-include("beam_ssa.hrl").

-type label() :: beam_ssa:label().

%% Main codegen structure.
-record(cg, {lcount=1 :: label(),               %Label counter
             bfail=1 :: label(),
             catch_label=none :: 'none' | label(),
             vars=#{} :: map(),     %Defined variables.
             break=0 :: label(),    %Break label
             recv=0 :: label(),     %Receive label
             ultimate_failure=0 :: label() %Label for ultimate match failure.
            }).

%% Internal records.
-record(cg_break, {args :: [beam_ssa:value()],
                   phi :: label()
                  }).
-record(cg_phi, {vars :: [beam_ssa:b_var()]
                }).
-record(cg_unreachable, {}).

-spec module(#k_mdef{}, [compile:option()]) -> {'ok',#b_module{}}.

module(#k_mdef{name=Mod,exports=Es,attributes=Attr,body=Forms}, _Opts) ->
    Body = functions(Forms, Mod),
    Module = #b_module{name=Mod,exports=Es,attributes=Attr,body=Body},
    {ok,Module}.

functions(Forms, Mod) ->
    [function(F, Mod) || F <- Forms].

function(#k_fdef{anno=Anno0,func=Name,arity=Arity,
                 vars=As0,body=Kb}, Mod) ->
    try
        #k_match{} = Kb,                   %Assertion.

        %% Generate the SSA form immediate format.
        St0 = #cg{},
        {As,St1} = new_ssa_vars(As0, St0),
        {Asm,St} = cg_fun(Kb, St1),
        Anno1 = line_anno(Anno0),
        Anno = Anno1#{func_info=>{Mod,Name,Arity}},
        #b_function{anno=Anno,args=As,bs=Asm,cnt=St#cg.lcount}
    catch
        Class:Error:Stack ->
            io:fwrite("Function: ~w/~w\n", [Name,Arity]),
            erlang:raise(Class, Error, Stack)
    end.

%% cg_fun([Lkexpr], [HeadVar], State) -> {[Ainstr],State}

cg_fun(Ke, St0) ->
    {UltimateFail,FailIs,St1} = make_failure(badarg, St0),
    St2 = St1#cg{bfail=UltimateFail,ultimate_failure=UltimateFail},
    {B,St} = cg(Ke, St2),
    Asm = [{label,0}|B++FailIs],
    finalize(Asm, St).

make_failure(Reason, St0) ->
    {Lbl,St1} = new_label(St0),
    {Dst,St} = new_ssa_var('@ssa_ret', St1),
    Is = [{label,Lbl},
          #b_set{op=call,dst=Dst,
                 args=[#b_remote{mod=#b_literal{val=erlang},
                                 name=#b_literal{val=error},
                                 arity=1},
                       #b_literal{val=Reason}]},
          #b_ret{arg=Dst}],
    {Lbl,Is,St}.

%% cg(Lkexpr, State) -> {[Ainstr],State}.
%%  Generate code for a kexpr.

cg(#k_match{body=M,ret=Rs}, St) ->
    do_match_cg(M, Rs, St);
cg(#k_guard_match{body=M,ret=Rs}, St) ->
    do_match_cg(M, Rs, St);
cg(#k_seq{arg=Arg,body=Body}, St0) ->
    {ArgIs,St1} = cg(Arg, St0),
    {BodyIs,St} = cg(Body, St1),
    {ArgIs++BodyIs,St};
cg(#k_call{anno=Le,op=Func,args=As,ret=Rs}, St) ->
    call_cg(Func, As, Rs, Le, St);
cg(#k_enter{anno=Le,op=Func,args=As}, St) ->
    enter_cg(Func, As, Le, St);
cg(#k_bif{anno=Le}=Bif, St) ->
    bif_cg(Bif, Le, St);
cg(#k_try{arg=Ta,vars=Vs,body=Tb,evars=Evs,handler=Th,ret=Rs}, St) ->
    try_cg(Ta, Vs, Tb, Evs, Th, Rs, St);
cg(#k_try_enter{arg=Ta,vars=Vs,body=Tb,evars=Evs,handler=Th}, St) ->
    try_enter_cg(Ta, Vs, Tb, Evs, Th, St);
cg(#k_catch{body=Cb,ret=[R]}, St) ->
    do_catch_cg(Cb, R, St);
cg(#k_receive{anno=Le,timeout=Te,var=Rvar,body=Rm,action=Tes,ret=Rs}, St) ->
    recv_loop_cg(Te, Rvar, Rm, Tes, Rs, Le, St);
cg(#k_receive_next{}, #cg{recv=Recv}=St) ->
    Is = [#b_set{op=recv_next},make_uncond_branch(Recv)],
    {Is,St};
cg(#k_receive_accept{}, St) ->
    Remove = #b_set{op=remove_message},
    {[Remove],St};
cg(#k_put{anno=Le,arg=Con,ret=Var}, St) ->
    put_cg(Var, Con, Le, St);
cg(#k_return{args=[Ret0]}, St) ->
    Ret = ssa_arg(Ret0, St),
    {[#b_ret{arg=Ret}],St};
cg(#k_break{args=Bs}, #cg{break=Br}=St) ->
    Args = ssa_args(Bs, St),
    {[#cg_break{args=Args,phi=Br}],St};
cg(#k_guard_break{args=Bs}, St) ->
    cg(#k_break{args=Bs}, St).

%% match_cg(Matc, [Ret], State) -> {[Ainstr],State}.
%%  Generate code for a match.

do_match_cg(M, Rs, St0) ->
    {B,St1} = new_label(St0),
    {Mis,St2} = match_cg(M, St1#cg.bfail, St1#cg{break=B}),
    {BreakVars,St} = new_ssa_vars(Rs, St2),
    {Mis ++ [{label,B},#cg_phi{vars=BreakVars}],
     St#cg{bfail=St0#cg.bfail,break=St1#cg.break}}.

%% match_cg(Match, Fail, State) -> {[Ainstr],State}.
%%  Generate code for a match tree.

match_cg(#k_alt{first=F,then=S}, Fail, St0) ->
    {Tf,St1} = new_label(St0),
    {Fis,St2} = match_cg(F, Tf, St1),
    {Sis,St3} = match_cg(S, Fail, St2),
    {Fis ++ [{label,Tf}] ++ Sis,St3};
match_cg(#k_select{var=#k_var{}=V,types=Scs}, Fail, St) ->
    match_fmf(fun (S, F, Sta) ->
                      select_cg(S, V, F, Fail, Sta)
              end, Fail, St, Scs);
match_cg(#k_guard{clauses=Gcs}, Fail, St) ->
    match_fmf(fun (G, F, Sta) ->
                      guard_clause_cg(G, F, Sta)
              end, Fail, St, Gcs);
match_cg(Ke, _Fail, St0) ->
    cg(Ke, St0).

%% select_cg(Sclause, V, TypeFail, ValueFail, State) -> {Is,State}.
%%  Selecting type and value needs two failure labels, TypeFail is the
%%  label to jump to of the next type test when this type fails, and
%%  ValueFail is the label when this type is correct but the value is
%%  wrong.  These are different as in the second case there is no need
%%  to try the next type, it will always fail.

select_cg(#k_type_clause{type=k_binary,values=[S]}, Var, Tf, Vf, St) ->
    select_binary(S, Var, Tf, Vf, St);
select_cg(#k_type_clause{type=k_bin_seg,values=Vs}, Var, Tf, _Vf, St) ->
    select_bin_segs(Vs, Var, Tf, St);
select_cg(#k_type_clause{type=k_bin_int,values=Vs}, Var, Tf, _Vf, St) ->
    select_bin_segs(Vs, Var, Tf, St);
select_cg(#k_type_clause{type=k_bin_end,values=[S]}, Var, Tf, _Vf, St) ->
    select_bin_end(S, Var, Tf, St);
select_cg(#k_type_clause{type=k_map,values=Vs}, Var, Tf, Vf, St) ->
    select_map(Vs, Var, Tf, Vf, St);
select_cg(#k_type_clause{type=k_cons,values=[S]}, Var, Tf, Vf, St) ->
    select_cons(S, Var, Tf, Vf, St);
select_cg(#k_type_clause{type=k_nil,values=[S]}, Var, Tf, Vf, St) ->
    select_nil(S, Var, Tf, Vf, St);
select_cg(#k_type_clause{type=k_literal,values=Vs}, Var, Tf, Vf, St) ->
    select_literal(Vs, Var, Tf, Vf, St);
select_cg(#k_type_clause{type=Type,values=Scs}, Var, Tf, Vf, St0) ->
    {Vis,St1} =
        mapfoldl(fun (S, Sta) ->
                         {Val,Is,Stb} = select_val(S, Var, Vf, Sta),
                         {{Is,[Val]},Stb}
                 end, St0, Scs),
    OptVls = combine(lists:sort(combine(Vis))),
    {Vls,Sis,St2} = select_labels(OptVls, St1, [], []),
    Arg = ssa_arg(Var, St2),
    {Is,St} = select_val_cg(Type, Arg, Vls, Tf, Vf, Sis, St2),
    {Is,St}.

select_val_cg(k_tuple, Tuple, Vls, Tf, Vf, Sis, St0) ->
    {Is0,St1} = make_cond_branch({bif,is_tuple}, [Tuple], Tf, St0),
    {Arity,St2} = new_ssa_var('@ssa_arity', St1),
    GetArity = #b_set{op={bif,tuple_size},dst=Arity,args=[Tuple]},
    {Is,St} = select_val_cg(k_int, Arity, Vls, Vf, Vf, Sis, St2),
    {Is0++[GetArity]++Is,St};
select_val_cg(Type, R, Vls, Tf, Vf, Sis, St0) ->
    {TypeIs,St1} = if
                       Tf =:= Vf ->
                           %% The type and value failure labels are the same;
                           %% we don't need a type test.
                           {[],St0};
                       true ->
                           %% Different labels for type failure and
                           %% label failure; we need a type test.
                           Test = select_type_test(Type),
                           make_cond_branch(Test, [R], Tf, St0)
                   end,
    case Vls of
        [{Val,Succ}] ->
            {Is,St} = make_cond({bif,'=:='}, [R,Val], Vf, Succ, St1),
            {TypeIs++Is++Sis,St};
        [_|_] ->
            {TypeIs++[#b_switch{arg=R,fail=Vf,list=Vls}|Sis],St1}
    end.

select_type_test(k_int) -> {bif,is_integer};
select_type_test(k_atom) -> {bif,is_atom};
select_type_test(k_float) -> {bif,is_float}.

combine([{Is,Vs1},{Is,Vs2}|Vis]) -> combine([{Is,Vs1 ++ Vs2}|Vis]);
combine([V|Vis]) -> [V|combine(Vis)];
combine([]) -> [].

select_labels([{Is,Vs}|Vis], St0, Vls, Sis) ->
    {Lbl,St1} = new_label(St0),
    select_labels(Vis, St1, add_vls(Vs, Lbl, Vls), [[{label,Lbl}|Is]|Sis]);
select_labels([], St, Vls, Sis) ->
    {Vls,append(Sis),St}.

add_vls([V|Vs], Lbl, Acc) ->
    add_vls(Vs, Lbl, [{#b_literal{val=V},Lbl}|Acc]);
add_vls([], _, Acc) -> Acc.

select_literal(S, V, Tf, Vf, St) ->
    Src = ssa_arg(V, St),
    F = fun(ValClause, Fail, St0) ->
                {Val,ValIs,St1} = select_val(ValClause, V, Vf, St0),
                Args = [Src,#b_literal{val=Val}],
                {Is,St2} = make_cond_branch({bif,'=:='}, Args, Fail, St1),
                {Is++ValIs,St2}
        end,
    match_fmf(F, Tf, St, S).

select_cons(#k_val_clause{val=#k_cons{hd=Hd,tl=Tl},body=B},
            V, Tf, Vf, St0) ->
    Es = [Hd,Tl],
    {Eis,St1} = select_extract_cons(V, Es, St0),
    {Bis,St2} = match_cg(B, Vf, St1),
    Src = ssa_arg(V, St2),
    {Is,St} = make_cond_branch(is_nonempty_list, [Src], Tf, St2),
    {Is ++ Eis ++ Bis,St}.

select_nil(#k_val_clause{val=#k_nil{},body=B}, V, Tf, Vf, St0) ->
    {Bis,St1} = match_cg(B, Vf, St0),
    Src = ssa_arg(V, St1),
    {Is,St} = make_cond_branch({bif,'=:='}, [Src,#b_literal{val=[]}], Tf, St1),
    {Is ++ Bis,St}.

select_binary(#k_val_clause{val=#k_binary{segs=#k_var{name=Ctx0}},body=B},
              #k_var{}=Src, Tf, Vf, St0) ->
    {Ctx,St1} = new_ssa_var(Ctx0, St0),
    {Bis0,St2} = match_cg(B, Vf, St1),
    {TestIs,St} = make_cond_branch(succeeded, [Ctx], Tf, St2),
    Bis1 = [#b_set{op=bs_start_match,dst=Ctx,
                   args=[ssa_arg(Src, St)]}] ++ TestIs ++ Bis0,
    Bis = finish_bs_matching(Bis1),
    {Bis,St}.

finish_bs_matching([#b_set{op=bs_match,
                           args=[#b_literal{val=string},Ctx,#b_literal{val=BinList}]}=Set|Is])
  when is_list(BinList) ->
    I = Set#b_set{args=[#b_literal{val=string},Ctx,
                        #b_literal{val=list_to_bitstring(BinList)}]},
    finish_bs_matching([I|Is]);
finish_bs_matching([I|Is]) ->
    [I|finish_bs_matching(Is)];
finish_bs_matching([]) -> [].

make_cond(Cond, Args, Fail, Succ, St0) ->
    {Bool,St} = new_ssa_var('@ssa_bool', St0),
    Bif = #b_set{op=Cond,dst=Bool,args=Args},
    Br = #b_br{bool=Bool,succ=Succ,fail=Fail},
    {[Bif,Br],St}.

make_cond_branch(Cond, Args, Fail, St0) ->
    {Bool,St1} = new_ssa_var('@ssa_bool', St0),
    {Succ,St} = new_label(St1),
    Bif = #b_set{op=Cond,dst=Bool,args=Args},
    Br = #b_br{bool=Bool,succ=Succ,fail=Fail},
    {[Bif,Br,{label,Succ}],St}.

make_uncond_branch(Fail) ->
    #b_br{bool=#b_literal{val=true},succ=Fail,fail=Fail}.

%% Instructions for selection of binary segments.

select_bin_segs(Scs, Ivar, Tf, St) ->
    match_fmf(fun(S, Fail, Sta) ->
                      select_bin_seg(S, Ivar, Fail, Sta)
              end, Tf, St, Scs).

select_bin_seg(#k_val_clause{val=#k_bin_seg{size=Size,unit=U,type=T,
                                            seg=Seg,flags=Fs,next=Next},
                             body=B,anno=Anno},
               #k_var{}=Src, Fail, St0) ->
    LineAnno = line_anno(Anno),
    Ctx = get_context(Src, St0),
    {Mis,St1} = select_extract_bin(Next, Size, U, T, Fs, Fail,
                                   Ctx, LineAnno, St0),
    {Extracted,St2} = new_ssa_var(Seg#k_var.name, St1),
    {Bis,St} = match_cg(B, Fail, St2),
    BsGet = #b_set{op=bs_extract,dst=Extracted,args=[ssa_arg(Next, St)]},
    Is = Mis ++ [BsGet] ++ Bis,
    {Is,St};
select_bin_seg(#k_val_clause{val=#k_bin_int{size=Sz,unit=U,flags=Fs,
                                            val=Val,next=Next},
                             body=B},
               #k_var{}=Src, Fail, St0) ->
    Ctx = get_context(Src, St0),
    {Mis,St1} = select_extract_int(Next, Val, Sz, U, Fs, Fail,
                                     Ctx, St0),
    {Bis,St} = match_cg(B, Fail, St1),
    Is = case Mis ++ Bis of
             [#b_set{op=bs_match,args=[#b_literal{val=string},OtherCtx1,Bin1]},
              #b_set{op=succeeded,dst=Bool1},
              #b_br{bool=Bool1,succ=Succ,fail=Fail},
              {label,Succ},
              #b_set{op=bs_match,dst=Dst,args=[#b_literal{val=string},_OtherCtx2,Bin2]}|
              [#b_set{op=succeeded,dst=Bool2},
               #b_br{bool=Bool2,fail=Fail}|_]=Is0] ->
                 %% We used to do this optimization later, but it
                 %% turns out that in huge functions with many
                 %% string matching instructions, it's a huge win
                 %% to do the combination now. To avoid copying the
                 %% binary data again and again, we'll combine bitstrings
                 %% in a list and convert all of it to a bitstring later.
                 {#b_literal{val=B1},#b_literal{val=B2}} = {Bin1,Bin2},
                 Bin = #b_literal{val=[B1,B2]},
                 Set = #b_set{op=bs_match,dst=Dst,args=[#b_literal{val=string},OtherCtx1,Bin]},
                 [Set|Is0];
             Is0 ->
                 Is0
         end,
    {Is,St}.

get_context(#k_var{}=Var, St) ->
    ssa_arg(Var, St).

select_bin_end(#k_val_clause{val=#k_bin_end{},body=B}, Src, Tf, St0) ->
    Ctx = get_context(Src, St0),
    {Bis,St1} = match_cg(B, Tf, St0),
    {TestIs,St} = make_cond_branch(bs_test_tail, [Ctx,#b_literal{val=0}], Tf, St1),
    Is = TestIs++Bis,
    {Is,St}.

select_extract_bin(#k_var{name=Hd}, Size0, Unit, Type, Flags, Vf,
                   Ctx, Anno, St0) ->
    {Dst,St1} = new_ssa_var(Hd, St0),
    Size = ssa_arg(Size0, St0),
    build_bs_instr(Anno, Type, Vf, Ctx, Size, Unit, Flags, Dst, St1).

select_extract_int(#k_var{name=Tl}, 0, #k_int{val=0}, _U, _Fs, _Vf,
                   Ctx, St0) ->
    St = set_ssa_var(Tl, Ctx, St0),
    {[],St};
select_extract_int(#k_var{name=Tl}, Val, #k_int{val=Sz}, U, Fs, Vf,
                   Ctx, St0) ->
    {Dst,St1} = new_ssa_var(Tl, St0),
    Bits = U*Sz,
    Bin = case member(big, Fs) of
              true ->
                  <<Val:Bits>>;
              false ->
                  true = member(little, Fs),	%Assertion.
                  <<Val:Bits/little>>
          end,
    Bits = bit_size(Bin),			%Assertion.
    {TestIs,St} = make_cond_branch(succeeded, [Dst], Vf, St1),
    Set = #b_set{op=bs_match,dst=Dst,
                 args=[#b_literal{val=string},Ctx,#b_literal{val=Bin}]},
    {[Set|TestIs],St}.

build_bs_instr(Anno, Type, Fail, Ctx, Size, Unit0, Flags0, Dst, St0) ->
    Unit = #b_literal{val=Unit0},
    Flags = #b_literal{val=Flags0},
    NeedSize = bs_need_size(Type),
    TypeArg = #b_literal{val=Type},
    Get = case NeedSize of
              true ->
                  #b_set{anno=Anno,op=bs_match,dst=Dst,
                         args=[TypeArg,Ctx,Flags,Size,Unit]};
              false ->
                  #b_set{anno=Anno,op=bs_match,dst=Dst,
                         args=[TypeArg,Ctx,Flags]}
          end,
    {Is,St} = make_cond_branch(succeeded, [Dst], Fail, St0),
    {[Get|Is],St}.

select_val(#k_val_clause{val=#k_tuple{es=Es},body=B}, V, Vf, St0) ->
    #k{us=Used} = k_get_anno(B),
    {Eis,St1} = select_extract_tuple(V, Es, Used, St0),
    {Bis,St2} = match_cg(B, Vf, St1),
    {length(Es),Eis ++ Bis,St2};
select_val(#k_val_clause{val=Val0,body=B}, _V, Vf, St0) ->
    Val = case Val0 of
              #k_atom{val=Lit} -> Lit;
              #k_float{val=Lit} -> Lit;
              #k_int{val=Lit} -> Lit;
              #k_literal{val=Lit} -> Lit
          end,
    {Bis,St1} = match_cg(B, Vf, St0),
    {Val,Bis,St1}.

%% select_extract_tuple(Src, [V], State) -> {[E],State}.
%%  Extract tuple elements, but only if they are actually used.
%%
%%  Not extracting tuple elements that are not used is an
%%  optimization for compile time and memory use during compilation.
%%  It is probably worthwhile because it is common to extract only a
%%  few elements from a huge record.

select_extract_tuple(Src, Vs, Used, St0) ->
    Tuple = ssa_arg(Src, St0),
    F = fun (#k_var{name=V}, {Elem,S0}) ->
                case member(V, Used) of
                    true ->
                        Args = [Tuple,#b_literal{val=Elem}],
                        {Dst,S} = new_ssa_var(V, S0),
                        Get = #b_set{op=get_tuple_element,dst=Dst,args=Args},
                        {[Get],{Elem+1,S}};
                    false ->
                        {[],{Elem+1,S0}}
                end
        end,
    {Es,{_,St}} = flatmapfoldl(F, {0,St0}, Vs),
    {Es,St}.

select_map(Scs, V, Tf, Vf, St0) ->
    MapSrc = ssa_arg(V, St0),
    {Is,St1} =
        match_fmf(fun(#k_val_clause{val=#k_map{op=exact,es=Es},
                                    body=B}, Fail, St1) ->
                          select_map_val(V, Es, B, Fail, St1)
                  end, Vf, St0, Scs),
    {TestIs,St} = make_cond_branch({bif,is_map}, [MapSrc], Tf, St1),
    {TestIs++Is,St}.

select_map_val(V, Es, B, Fail, St0) ->
    {Eis,St1} = select_extract_map(Es, V, Fail, St0),
    {Bis,St2} = match_cg(B, Fail, St1),
    {Eis++Bis,St2}.

select_extract_map([P|Ps], Src, Fail, St0) ->
    MapSrc = ssa_arg(Src, St0),
    #k_map_pair{key=Key0,val=#k_var{name=Dst0}} = P,
    Key = ssa_arg(Key0, St0),
    {Dst,St1} = new_ssa_var(Dst0, St0),
    Set = #b_set{op=get_map_element,dst=Dst,args=[MapSrc,Key]},
    {TestIs,St2} = make_cond_branch(succeeded, [Dst], Fail, St1),
    {Is,St} = select_extract_map(Ps, Src, Fail, St2),
    {[Set|TestIs]++Is,St};
select_extract_map([], _, _, St) ->
    {[],St}.

select_extract_cons(Src0, [#k_var{name=Hd},#k_var{name=Tl}], St0) ->
    Src = ssa_arg(Src0, St0),
    {HdDst,St1} = new_ssa_var(Hd, St0),
    {TlDst,St2} = new_ssa_var(Tl, St1),
    GetHd = #b_set{op=get_hd,dst=HdDst,args=[Src]},
    GetTl = #b_set{op=get_tl,dst=TlDst,args=[Src]},
    {[GetHd,GetTl],St2}.

guard_clause_cg(#k_guard_clause{guard=G,body=B}, Fail, St0) ->
    {Gis,St1} = guard_cg(G, Fail, St0),
    {Bis,St} = match_cg(B, Fail, St1),
    {Gis ++ Bis,St}.

%% guard_cg(Guard, Fail, State) -> {[Ainstr],State}.
%%  A guard is a boolean expression of tests.  Tests return true or
%%  false.  A fault in a test causes the test to return false.  Tests
%%  never return the boolean, instead we generate jump code to go to
%%  the correct exit point.  Primops and tests all go to the next
%%  instruction on success or jump to a failure label.

guard_cg(#k_protected{arg=Ts,ret=Rs,inner=Inner}, Fail, St) ->
    protected_cg(Ts, Rs, Inner, Fail, St);
guard_cg(#k_test{op=Test0,args=As,inverted=Inverted}, Fail, St0) ->
    #k_remote{mod=#k_atom{val=erlang},name=#k_atom{val=Test}} = Test0,
    test_cg(Test, Inverted, As, Fail, St0);
guard_cg(#k_seq{arg=Arg,body=Body}, Fail, St0) ->
    {ArgIs,St1} = guard_cg(Arg, Fail, St0),
    {BodyIs,St} = guard_cg(Body, Fail, St1),
    {ArgIs++BodyIs,St};
guard_cg(G, _Fail, St) ->
    cg(G, St).

test_cg('=/=', Inverted, As, Fail, St) ->
    test_cg('=:=', not Inverted, As, Fail, St);
test_cg('/=', Inverted, As, Fail, St) ->
    test_cg('==', not Inverted, As, Fail, St);
test_cg(Test, Inverted, As0, Fail, St0) ->
    As = ssa_args(As0, St0),
    case {Test,ssa_args(As0, St0)} of
        {is_record,[Tuple,#b_literal{val=Atom}=Tag,#b_literal{val=Int}=Arity]}
          when is_atom(Atom), is_integer(Int) ->
            test_is_record_cg(Inverted, Fail, Tuple, Tag, Arity, St0);
        {_,As} ->
            {Bool,St1} = new_ssa_var('@ssa_bool', St0),
            {Succ,St} = new_label(St1),
            Bif = #b_set{op={bif,Test},dst=Bool,args=As},
            Br = case Inverted of
                     false -> #b_br{bool=Bool,succ=Succ,fail=Fail};
                     true -> #b_br{bool=Bool,succ=Fail,fail=Succ}
                 end,
            {[Bif,Br,{label,Succ}],St}
    end.

test_is_record_cg(false, Fail, Tuple, TagVal, ArityVal, St0) ->
    {Arity,St1} = new_ssa_var('@ssa_arity', St0),
    {Tag,St2} = new_ssa_var('@ssa_tag', St1),
    {Is0,St3} = make_cond_branch({bif,is_tuple}, [Tuple], Fail, St2),
    GetArity = #b_set{op={bif,tuple_size},dst=Arity,args=[Tuple]},
    {Is1,St4} = make_cond_branch({bif,'=:='}, [Arity,ArityVal], Fail, St3),
    GetTag = #b_set{op=get_tuple_element,dst=Tag,
                    args=[Tuple,#b_literal{val=0}]},
    {Is2,St} = make_cond_branch({bif,'=:='}, [Tag,TagVal], Fail, St4),
    Is = Is0 ++ [GetArity] ++ Is1 ++ [GetTag] ++ Is2,
    {Is,St};
test_is_record_cg(true, Fail, Tuple, TagVal, ArityVal, St0) ->
    {Succ,St1} = new_label(St0),
    {Arity,St2} = new_ssa_var('@ssa_arity', St1),
    {Tag,St3} = new_ssa_var('@ssa_tag', St2),
    {Is0,St4} = make_cond_branch({bif,is_tuple}, [Tuple], Succ, St3),
    GetArity = #b_set{op={bif,tuple_size},dst=Arity,args=[Tuple]},
    {Is1,St5} = make_cond_branch({bif,'=:='}, [Arity,ArityVal], Succ, St4),
    GetTag = #b_set{op=get_tuple_element,dst=Tag,
                    args=[Tuple,#b_literal{val=0}]},
    {Is2,St} = make_cond_branch({bif,'=:='}, [Tag,TagVal], Succ, St5),
    Is3 = [make_uncond_branch(Fail),{label,Succ}],
    Is = Is0 ++ [GetArity] ++ Is1 ++ [GetTag] ++ Is2 ++ Is3,
    {Is,St}.

%% protected_cg([Kexpr], [Ret], Fail, St) -> {[Ainstr],St}.
%%  Do a protected.  Protecteds without return values are just done
%%  for effect, the return value is not checked, success passes on to
%%  the next instruction and failure jumps to Fail.  If there are
%%  return values then these must be set to 'false' on failure,
%%  control always passes to the next instruction.

protected_cg(Ts, [], _, Fail, St0) ->
    %% Protect these calls, revert when done.
    {Tis,St1} = guard_cg(Ts, Fail, St0#cg{bfail=Fail}),
    {Tis,St1#cg{bfail=St0#cg.bfail}};
protected_cg(Ts, Rs, Inner0, _Fail, St0) ->
    {Pfail,St1} = new_label(St0),
    {Br,St2} = new_label(St1),
    Prot = duplicate(length(Rs), #b_literal{val=false}),
    {Tis,St3} = guard_cg(Ts, Pfail, St2#cg{break=Pfail,bfail=Pfail}),
    Inner = ssa_args(Inner0, St3),
    {BreakVars,St} = new_ssa_vars(Rs, St3),
    Is = Tis ++ [#cg_break{args=Inner,phi=Br},
                 {label,Pfail},#cg_break{args=Prot,phi=Br},
                 {label,Br},#cg_phi{vars=BreakVars}],
    {Is,St#cg{break=St0#cg.break,bfail=St0#cg.bfail}}.

%% match_fmf(Fun, LastFail, State, [Clause]) -> {Is,State}.
%%  This is a special flatmapfoldl for match code gen where we
%%  generate a "failure" label for each clause. The last clause uses
%%  an externally generated failure label, LastFail.  N.B. We do not
%%  know or care how the failure labels are used.

match_fmf(F, LastFail, St, [H]) ->
    F(H, LastFail, St);
match_fmf(F, LastFail, St0, [H|T]) ->
    {Fail,St1} = new_label(St0),
    {R,St2} = F(H, Fail, St1),
    {Rs,St3} = match_fmf(F, LastFail, St2, T),
    {R ++ [{label,Fail}] ++ Rs,St3}.

%% fail_label(State) -> {Where,FailureLabel}.
%%       Where = guard | no_catch | in_catch
%%  Return an indication of which part of a function code is
%%  being generated for and the appropriate failure label to
%%  use.
%%
%%  Where has the following meaning:
%%
%%     guard     - Inside a guard.
%%     no_catch  - In a function body, not in the scope of
%%                 a try/catch or catch.
%%     in_catch  - In the scope of a try/catch or catch.

fail_label(#cg{catch_label=Catch,bfail=Fail,ultimate_failure=Ult}) ->
    if
        Fail =/= Ult ->
            {guard,Fail};
        Catch =:= none ->
            {no_catch,Fail};
        is_integer(Catch) ->
            {in_catch,Catch}
    end.

%% bif_fail_label(State) -> FailureLabel.
%%  Return the appropriate failure label for a guard BIF call or
%%  primop that fails.

bif_fail_label(St) ->
    {_,Fail} = fail_label(St),
    Fail.

%% call_cg(Func, [Arg], [Ret], Le, State) ->
%%      {[Ainstr],State}.
%% enter_cg(Func, [Arg], Le, St) -> {[Ainstr],St}.
%%  Generate code for call and enter.

call_cg(Func, As, [], Le, St) ->
    call_cg(Func, As, [#k_var{name='@ssa_ignored'}], Le, St);
call_cg(Func0, As, [#k_var{name=R}|MoreRs]=Rs, Le, St0) ->
    case fail_label(St0) of
        {guard,Fail} ->
            %% Inside a guard. The only allowed function call is to
            %% erlang:error/1,2. We will generate a branch to the
            %% failure branch.
            #k_remote{mod=#k_atom{val=erlang},
                      name=#k_atom{val=error}} = Func0, %Assertion.
            [#k_var{name=DestVar}] = Rs,
            St = set_ssa_var(DestVar, #b_literal{val=unused}, St0),
            {[make_uncond_branch(Fail),#cg_unreachable{}],St};
        {Catch,Fail} ->
            %% Ordinary function call in a function body.
            Args = ssa_args(As, St0),
            {Ret,St1} = new_ssa_var(R, St0),
            Func = call_target(Func0, Args, St0),
            Call = #b_set{anno=line_anno(Le),op=call,dst=Ret,args=[Func|Args]},

            %% If this is a call to erlang:error(), MoreRs could be a
            %% nonempty list of variables that each need a value.
            St2 = foldl(fun(#k_var{name=Dummy}, S) ->
                                set_ssa_var(Dummy, #b_literal{val=unused}, S)
                        end, St1, MoreRs),
            case Catch of
                no_catch ->
                    {[Call],St2};
                in_catch ->
                    {TestIs,St} = make_cond_branch(succeeded, [Ret], Fail, St2),
                    {[Call|TestIs],St}
            end
    end.

enter_cg(Func0, As0, Le, St0) ->
    Anno = line_anno(Le),
    Func = call_target(Func0, As0, St0),
    As = ssa_args(As0, St0),
    {Ret,St} = new_ssa_var('@ssa_ret', St0),
    Call = #b_set{anno=Anno,op=call,dst=Ret,args=[Func|As]},
    {[Call,#b_ret{arg=Ret}],St}.

call_target(Func, As, St) ->
    Arity = length(As),
    case Func of
        #k_remote{mod=Mod0,name=Name0} ->
            Mod = ssa_arg(Mod0, St),
            Name = ssa_arg(Name0, St),
            #b_remote{mod=Mod,name=Name,arity=Arity};
        #k_local{name=Name} when is_atom(Name) ->
            #b_local{name=#b_literal{val=Name},arity=Arity};
        #k_var{}=Var ->
            ssa_arg(Var, St)
    end.

%% bif_cg(#k_bif{}, Le,State) -> {[Ainstr],State}.
%%  Generate code for a guard BIF or primop.

bif_cg(#k_bif{op=#k_internal{name=Name},args=As,ret=Rs}, Le, St) ->
    internal_cg(Name, As, Rs, Le, St);
bif_cg(#k_bif{op=#k_remote{mod=#k_atom{val=erlang},name=#k_atom{val=Name}},
              args=As,ret=Rs}, Le, St) ->
    bif_cg(Name, As, Rs, Le, St).

%% internal_cg(Bif, [Arg], [Ret], Le, State) ->
%%      {[Ainstr],State}.

internal_cg(make_fun, [Name0,Arity0|As], Rs, _Le, St0) ->
    #k_atom{val=Name} = Name0,
    #k_int{val=Arity} = Arity0,
    case Rs of
        [#k_var{name=Dst0}] ->
            {Dst,St} = new_ssa_var(Dst0, St0),
            Args = ssa_args(As, St),
            Local = #b_local{name=#b_literal{val=Name},arity=Arity},
            MakeFun = #b_set{op=make_fun,dst=Dst,args=[Local|Args]},
            {[MakeFun],St};
        [] ->
            {[],St0}
    end;
internal_cg(bs_init_writable=I, As, [#k_var{name=Dst0}], _Le, St0) ->
    %% This behaves like a function call.
    {Dst,St} = new_ssa_var(Dst0, St0),
    Args = ssa_args(As, St),
    Set = #b_set{op=I,dst=Dst,args=Args},
    {[Set],St};
internal_cg(build_stacktrace=I, As, [#k_var{name=Dst0}], _Le, St0) ->
    {Dst,St} = new_ssa_var(Dst0, St0),
    Args = ssa_args(As, St),
    Set = #b_set{op=I,dst=Dst,args=Args},
    {[Set],St};
internal_cg(raise, As, [#k_var{name=Dst0}], _Le, St0) ->
    Args = ssa_args(As, St0),
    {Dst,St} = new_ssa_var(Dst0, St0),
    Resume = #b_set{op=resume,dst=Dst,args=Args},
    case St of
        #cg{catch_label=none} ->
            {[Resume],St};
        #cg{catch_label=Catch} when is_integer(Catch) ->
            Is = [Resume,make_uncond_branch(Catch),#cg_unreachable{}],
            {Is,St}
    end;
internal_cg(raw_raise=I, As, [#k_var{name=Dst0}], _Le, St0) ->
    %% This behaves like a function call.
    {Dst,St} = new_ssa_var(Dst0, St0),
    Args = ssa_args(As, St),
    Set = #b_set{op=I,dst=Dst,args=Args},
    {[Set],St}.

bif_cg(Bif, As0, [#k_var{name=Dst0}], Le, St0) ->
    {Dst,St1} = new_ssa_var(Dst0, St0),
    case {Bif,ssa_args(As0, St0)} of
        {is_record,[Tuple,#b_literal{val=Atom}=Tag,
                    #b_literal{val=Int}=Arity]}
          when is_atom(Atom), is_integer(Int) ->
            bif_is_record_cg(Dst, Tuple, Tag, Arity, St1);
        {_,As} ->
            I = #b_set{anno=line_anno(Le),op={bif,Bif},dst=Dst,args=As},
            case erl_bifs:is_safe(erlang, Bif, length(As)) of
                false ->
                    Fail = bif_fail_label(St1),
                    {Is,St} = make_cond_branch(succeeded, [Dst], Fail, St1),
                    {[I|Is],St};
                true->
                    {[I],St1}
            end
        end.

bif_is_record_cg(Dst, Tuple, TagVal, ArityVal, St0) ->
    {Arity,St1} = new_ssa_var('@ssa_arity', St0),
    {Tag,St2} = new_ssa_var('@ssa_tag', St1),
    {Phi,St3} = new_label(St2),
    {False,St4} = new_label(St3),
    {Is0,St5} = make_cond_branch({bif,is_tuple}, [Tuple], False, St4),
    GetArity = #b_set{op={bif,tuple_size},dst=Arity,args=[Tuple]},
    {Is1,St6} = make_cond_branch({bif,'=:='}, [Arity,ArityVal], False, St5),
    GetTag = #b_set{op=get_tuple_element,dst=Tag,
                    args=[Tuple,#b_literal{val=0}]},
    {Is2,St} = make_cond_branch({bif,'=:='}, [Tag,TagVal], False, St6),
    Is3 = [#cg_break{args=[#b_literal{val=true}],phi=Phi},
           {label,False},
           #cg_break{args=[#b_literal{val=false}],phi=Phi},
           {label,Phi},
           #cg_phi{vars=[Dst]}],
    Is = Is0 ++ [GetArity] ++ Is1 ++ [GetTag] ++ Is2 ++ Is3,
    {Is,St}.

%% recv_loop_cg(TimeOut, ReceiveVar, ReceiveMatch, TimeOutExprs,
%%              [Ret], Le, St) -> {[Ainstr],St}.

recv_loop_cg(Te, Rvar, Rm, Tes, Rs, Le, St0) ->
    %% Get labels.
    {Rl,St1} = new_label(St0),
    {Tl,St2} = new_label(St1),
    {Bl,St3} = new_label(St2),
    St4 = St3#cg{break=Bl,recv=Rl},
    {Ris,St5} = cg_recv_mesg(Rvar, Rm, Tl, Le, St4),
    {Wis,St6} = cg_recv_wait(Te, Tes, St5),
    {BreakVars,St} = new_ssa_vars(Rs, St6),
    {Ris ++ [{label,Tl}] ++ Wis ++
         [{label,Bl},#cg_phi{vars=BreakVars}],
     St#cg{break=St0#cg.break,recv=St0#cg.recv}}.

%% cg_recv_mesg( ) -> {[Ainstr],St}.

cg_recv_mesg(#k_var{name=R}, Rm, Tl, Le, St0) ->
    {Dst,St1} = new_ssa_var(R, St0),
    {Mis,St2} = match_cg(Rm, none, St1),
    RecvLbl = St1#cg.recv,
    {TestIs,St} = make_cond_branch(succeeded, [Dst], Tl, St2),
    Is = [#b_br{anno=line_anno(Le),bool=#b_literal{val=true},
                succ=RecvLbl,fail=RecvLbl},
          {label,RecvLbl},
          #b_set{op=peek_message,dst=Dst}|TestIs],
    {Is++Mis,St}.

%% cg_recv_wait(Te, Tes, St) -> {[Ainstr],St}.

cg_recv_wait(#k_int{val=0}, Es, St0) ->
    {Tis,St} = cg(Es, St0),
    {[#b_set{op=timeout}|Tis],St};
cg_recv_wait(Te, Es, St0) ->
    {Tis,St1} = cg(Es, St0),
    Args = [ssa_arg(Te, St1)],
    {WaitDst,St2} = new_ssa_var('@ssa_wait', St1),
    {WaitIs,St} = make_cond_branch(succeeded, [WaitDst], St1#cg.recv, St2),
    %% Infinite timeout will be optimized later.
    Is = [#b_set{op=wait_timeout,dst=WaitDst,args=Args}] ++ WaitIs ++
        [#b_set{op=timeout}] ++ Tis,
    {Is,St}.

%% try_cg(TryBlock, [BodyVar], TryBody, [ExcpVar], TryHandler, [Ret], St) ->
%%         {[Ainstr],St}.

try_cg(Ta, Vs, Tb, Evs, Th, Rs, St0) ->
    {B,St1} = new_label(St0),			%Body label
    {H,St2} = new_label(St1),			%Handler label
    {E,St3} = new_label(St2),			%End label
    {Next,St4} = new_label(St3),
    {TryTag,St5} = new_ssa_var('@ssa_catch_tag', St4),
    {SsaVs,St6} = new_ssa_vars(Vs, St5),
    {SsaEvs,St7} = new_ssa_vars(Evs, St6),
    {Ais,St8} = cg(Ta, St7#cg{break=B,catch_label=H}),
    St9 = St8#cg{break=E,catch_label=St7#cg.catch_label},
    {Bis,St10} = cg(Tb, St9),
    {His,St11} = cg(Th, St10),
    {BreakVars,St12} = new_ssa_vars(Rs, St11),
    {CatchedAgg,St} = new_ssa_var('@ssa_agg', St12),
    ExtractVs = extract_vars(SsaEvs, CatchedAgg, 0),
    KillTryTag = #b_set{op=kill_try_tag,args=[TryTag]},
    Args = [#b_literal{val='try'},TryTag],
    Handler = [{label,H},
               #b_set{op=landingpad,dst=CatchedAgg,args=Args}] ++
        ExtractVs ++ [KillTryTag],
    {[#b_set{op=new_try_tag,dst=TryTag,args=[#b_literal{val='try'}]},
      #b_br{bool=TryTag,succ=Next,fail=H},
      {label,Next}] ++ Ais ++
         [{label,B},#cg_phi{vars=SsaVs},KillTryTag] ++ Bis ++
         Handler ++ His ++
         [{label,E},#cg_phi{vars=BreakVars}],
     St#cg{break=St0#cg.break}}.

try_enter_cg(Ta, Vs, Tb, Evs, Th, St0) ->
    {B,St1} = new_label(St0),			%Body label
    {H,St2} = new_label(St1),			%Handler label
    {Next,St3} = new_label(St2),
    {TryTag,St4} = new_ssa_var('@ssa_catch_tag', St3),
    {SsaVs,St5} = new_ssa_vars(Vs, St4),
    {SsaEvs,St6} = new_ssa_vars(Evs, St5),
    {Ais,St7} = cg(Ta, St6#cg{break=B,catch_label=H}),
    St8 = St7#cg{catch_label=St6#cg.catch_label},
    {Bis,St9} = cg(Tb, St8),
    {His,St10} = cg(Th, St9),
    {CatchedAgg,St} = new_ssa_var('@ssa_agg', St10),
    ExtractVs = extract_vars(SsaEvs, CatchedAgg, 0),
    KillTryTag = #b_set{op=kill_try_tag,args=[TryTag]},
    Args = [#b_literal{val='try'},TryTag],
    Handler = [{label,H},
               #b_set{op=landingpad,dst=CatchedAgg,args=Args}] ++
        ExtractVs ++ [KillTryTag],
    {[#b_set{op=new_try_tag,dst=TryTag,args=[#b_literal{val='try'}]},
      #b_br{bool=TryTag,succ=Next,fail=H},
      {label,Next}] ++  Ais ++
         [{label,B},#cg_phi{vars=SsaVs},KillTryTag] ++ Bis ++
         Handler ++ His,
     St#cg{break=St0#cg.break}}.

extract_vars([V|Vs], Agg, N) ->
    I = #b_set{op=extract,dst=V,args=[Agg,#b_literal{val=N}]},
    [I|extract_vars(Vs, Agg, N+1)];
extract_vars([], _, _) -> [].

%% do_catch_cg(CatchBlock, Ret, St) -> {[Ainstr],St}.

do_catch_cg(Block, #k_var{name=R}, St0) ->
    {B,St1} = new_label(St0),
    {Next,St2} = new_label(St1),
    {H,St3} = new_label(St2),
    {CatchReg,St4} = new_ssa_var('@ssa_catch_tag', St3),
    {Dst,St5} = new_ssa_var(R, St4),
    {Succ,St6} = new_label(St5),
    {Cis,St7} = cg(Block, St6#cg{break=Succ,catch_label=H}),
    {CatchedVal,St8} = new_ssa_var('@catched_val', St7),
    {SuccVal,St9} = new_ssa_var('@success_val', St8),
    {CatchedAgg,St10} = new_ssa_var('@ssa_agg', St9),
    {CatchEndVal,St} = new_ssa_var('@catch_end_val', St10),
    Args = [#b_literal{val='catch'},CatchReg],
    {[#b_set{op=new_try_tag,dst=CatchReg,args=[#b_literal{val='catch'}]},
      #b_br{bool=CatchReg,succ=Next,fail=H},
      {label,Next}] ++ Cis ++
         [{label,H},
          #b_set{op=landingpad,dst=CatchedAgg,args=Args},
          #b_set{op=extract,dst=CatchedVal,
                 args=[CatchedAgg,#b_literal{val=0}]},
          #cg_break{args=[CatchedVal],phi=B},
          {label,Succ},
          #cg_phi{vars=[SuccVal]},
          #cg_break{args=[SuccVal],phi=B},
          {label,B},#cg_phi{vars=[CatchEndVal]},
          #b_set{op=catch_end,dst=Dst,args=[CatchReg,CatchEndVal]}],
     St#cg{break=St1#cg.break,catch_label=St1#cg.catch_label}}.

%% put_cg([Var], Constr, Le, Vdb, Bef, St) -> {[Ainstr],St}.
%%  Generate code for constructing terms.

put_cg([#k_var{name=R}], #k_cons{hd=Hd,tl=Tl}, _Le, St0) ->
    Args = ssa_args([Hd,Tl], St0),
    {Dst,St} = new_ssa_var(R, St0),
    PutList = #b_set{op=put_list,dst=Dst,args=Args},
    {[PutList],St};
put_cg([#k_var{name=R}], #k_tuple{es=Es}, _Le, St0) ->
    {Ret,St} = new_ssa_var(R, St0),
    Args = ssa_args(Es, St),
    PutTuple = #b_set{op=put_tuple,dst=Ret,args=Args},
    {[PutTuple],St};
put_cg([#k_var{name=R}], #k_binary{segs=Segs}, Le, St0) ->
    Fail = bif_fail_label(St0),
    {Dst,St1} = new_ssa_var(R, St0),
    cg_binary(Dst, Segs, Fail, Le, St1);
put_cg([#k_var{name=R}], #k_map{op=Op,var=Map,
                                es=[#k_map_pair{key=#k_var{}=K,val=V}]},
       Le, St0) ->
    %% Map: single variable key.
    SrcMap = ssa_arg(Map, St0),
    LineAnno = line_anno(Le),
    List = [ssa_arg(K, St0),ssa_arg(V, St0)],
    {Dst,St1} = new_ssa_var(R, St0),
    {Is,St} = put_cg_map(LineAnno, Op, SrcMap, Dst, List, St1),
    {Is,St};
put_cg([#k_var{name=R}], #k_map{op=Op,var=Map,es=Es}, Le, St0) ->
    %% Map: one or more literal keys.
    [] = [Var || #k_map_pair{key=#k_var{}=Var} <- Es], %Assertion
    SrcMap = ssa_arg(Map, St0),
    LineAnno = line_anno(Le),
    List = flatmap(fun(#k_map_pair{key=K,val=V}) ->
                           [ssa_arg(K, St0),ssa_arg(V, St0)]
                   end, Es),
    {Dst,St1} = new_ssa_var(R, St0),
    {Is,St} = put_cg_map(LineAnno, Op, SrcMap, Dst, List, St1),
    {Is,St};
put_cg([#k_var{name=R}], Con0, _Le, St0) ->
    %% Create an alias for a variable or literal.
    Con = ssa_arg(Con0, St0),
    St = set_ssa_var(R, Con, St0),
    {[],St}.

put_cg_map(LineAnno, Op, SrcMap, Dst, List, St0) ->
    Fail = bif_fail_label(St0),
    Args = [#b_literal{val=Op},SrcMap|List],
    PutMap = #b_set{anno=LineAnno,op=put_map,dst=Dst,args=Args},
    if
        Op =:= assoc ->
            {[PutMap],St0};
        true ->
            {Is,St} = make_cond_branch(succeeded, [Dst], Fail, St0),
            {[PutMap|Is],St}
    end.

%%%
%%% Code generation for constructing binaries.
%%%

cg_binary(Dst, Segs0, Fail, Le, St0) ->
    {PutCode0,SzCalc0,St1} = cg_bin_put(Segs0, Fail, St0),
    LineAnno = line_anno(Le),
    Anno = Le#k.a,
    case PutCode0 of
        [#b_set{op=bs_put,dst=Bool,args=[_,_,Src,#b_literal{val=all}|_]},
         #b_br{bool=Bool},
         {label,_}|_] ->
            #k_bin_seg{unit=Unit0,next=Segs} = Segs0,
            Unit = #b_literal{val=Unit0},
            {PutCode,SzCalc1,St2} = cg_bin_put(Segs, Fail, St1),
            {_,SzVar,SzCode0,St3} = cg_size_calc(1, SzCalc1, Fail, St2),
            SzCode = cg_bin_anno(SzCode0, LineAnno),
            Args = case member(single_use, Anno) of
                       true ->
                           [#b_literal{val=private_append},Src,SzVar,Unit];
                       false ->
                           [#b_literal{val=append},Src,SzVar,Unit]
                   end,
            BsInit = #b_set{anno=LineAnno,op=bs_init,dst=Dst,args=Args},
            {TestIs,St} = make_cond_branch(succeeded, [Dst], Fail, St3),
            {SzCode ++ [BsInit] ++ TestIs ++ PutCode,St};
        [#b_set{op=bs_put}|_] ->
            {Unit,SzVar,SzCode0,St2} = cg_size_calc(8, SzCalc0, Fail, St1),
            SzCode = cg_bin_anno(SzCode0, LineAnno),
            Args = [#b_literal{val=new},SzVar,Unit],
            BsInit = #b_set{anno=LineAnno,op=bs_init,dst=Dst,args=Args},
            {TestIs,St} = make_cond_branch(succeeded, [Dst], Fail, St2),
            {SzCode ++ [BsInit] ++ TestIs ++ PutCode0,St}
    end.

cg_bin_anno([Set|Sets], Anno) ->
    [Set#b_set{anno=Anno}|Sets];
cg_bin_anno([], _) -> [].

%% cg_size_calc(PreferredUnit, SzCalc, Fail, St0) ->
%%         {ActualUnit,SizeVariable,SizeCode,St}.
%%  Generate size calculation code.

cg_size_calc(Unit, error, _Fail, St) ->
    {#b_literal{val=Unit},#b_literal{val=badarg},[],St};
cg_size_calc(8, [{1,_}|_]=SzCalc, Fail, St) ->
    cg_size_calc(1, SzCalc, Fail, St);
cg_size_calc(8, SzCalc, Fail, St0) ->
    {Var,Pre,St} = cg_size_calc_1(SzCalc, Fail, St0),
    {#b_literal{val=8},Var,Pre,St};
cg_size_calc(1, SzCalc0, Fail, St0) ->
    SzCalc = map(fun({8,#b_literal{val=Size}}) ->
                         {1,#b_literal{val=8*Size}};
                    ({8,{{bif,byte_size},Src}}) ->
                         {1,{{bif,bit_size},Src}};
                    ({8,{_,_}=UtfCalc}) ->
                         {1,{'*',#b_literal{val=8},UtfCalc}};
                    ({_,_}=Pair) ->
                         Pair
                 end, SzCalc0),
    {Var,Pre,St} = cg_size_calc_1(SzCalc, Fail, St0),
    {#b_literal{val=1},Var,Pre,St}.

cg_size_calc_1(SzCalc, Fail, St0) ->
    cg_size_calc_2(SzCalc, #b_literal{val=0}, Fail, St0).

cg_size_calc_2([{_,{'*',Unit,{_,_}=Bif}}|T], Sum0, Fail, St0) ->
    {Sum1,Pre0,St1} = cg_size_calc_2(T, Sum0, Fail, St0),
    {BifDst,Pre1,St2} = cg_size_bif(Bif, Fail, St1),
    {Sum,Pre2,St} = cg_size_add(Sum1, BifDst, Unit, Fail, St2),
    {Sum,Pre0++Pre1++Pre2,St};
cg_size_calc_2([{_,#b_literal{}=Sz}|T], Sum0, Fail, St0) ->
    {Sum1,Pre0,St1} = cg_size_calc_2(T, Sum0, Fail, St0),
    {Sum,Pre,St} = cg_size_add(Sum1, Sz, #b_literal{val=1}, Fail, St1),
    {Sum,Pre0++Pre,St};
cg_size_calc_2([{_,#b_var{}=Sz}|T], Sum0, Fail, St0) ->
    {Sum1,Pre0,St1} = cg_size_calc_2(T, Sum0, Fail, St0),
    {Sum,Pre,St} = cg_size_add(Sum1, Sz, #b_literal{val=1}, Fail, St1),
    {Sum,Pre0++Pre,St};
cg_size_calc_2([{_,{_,_}=Bif}|T], Sum0, Fail, St0) ->
    {Sum1,Pre0,St1} = cg_size_calc_2(T, Sum0, Fail, St0),
    {BifDst,Pre1,St2} = cg_size_bif(Bif, Fail, St1),
    {Sum,Pre2,St} = cg_size_add(Sum1, BifDst, #b_literal{val=1}, Fail, St2),
    {Sum,Pre0++Pre1++Pre2,St};
cg_size_calc_2([], Sum, _Fail, St) ->
    {Sum,[],St}.

cg_size_bif(#b_var{}=Var, _Fail, St) ->
    {Var,[],St};
cg_size_bif({Name,Src}, Fail, St0) ->
    {Dst,St1} = new_ssa_var('@ssa_bif', St0),
    Bif = #b_set{op=Name,dst=Dst,args=[Src]},
    {TestIs,St} = make_cond_branch(succeeded, [Dst], Fail, St1),
    {Dst,[Bif|TestIs],St}.

cg_size_add(#b_literal{val=0}, Val, #b_literal{val=1}, _Fail, St) ->
    {Val,[],St};
cg_size_add(A, B, Unit, Fail, St0) ->
    {Dst,St1} = new_ssa_var('@ssa_sum', St0),
    {TestIs,St} = make_cond_branch(succeeded, [Dst], Fail, St1),
    BsAdd = #b_set{op=bs_add,dst=Dst,args=[A,B,Unit]},
    {Dst,[BsAdd|TestIs],St}.

cg_bin_put(Seg, Fail, St) ->
    cg_bin_put_1(Seg, Fail, [], [], St).

cg_bin_put_1(#k_bin_seg{size=Size0,unit=U,type=T,flags=Fs,seg=Src0,next=Next},
           Fail, Acc, SzCalcAcc, St0) ->
    [Src,Size] = ssa_args([Src0,Size0], St0),
    NeedSize = bs_need_size(T),
    TypeArg = #b_literal{val=T},
    Flags = #b_literal{val=Fs},
    Unit = #b_literal{val=U},
    Args = case NeedSize of
               true -> [TypeArg,Flags,Src,Size,Unit];
               false -> [TypeArg,Flags,Src]
           end,
    {Is,St} = make_cond_branch(bs_put, Args, Fail, St0),
    SzCalc = bin_size_calc(T, Src, Size, U),
    cg_bin_put_1(Next, Fail, reverse(Is, Acc), [SzCalc|SzCalcAcc], St);
cg_bin_put_1(#k_bin_end{}, _, Acc, SzCalcAcc, St) ->
    SzCalc = fold_size_calc(SzCalcAcc, 0, []),
    {reverse(Acc),SzCalc,St}.

bs_need_size(utf8) -> false;
bs_need_size(utf16) -> false;
bs_need_size(utf32) -> false;
bs_need_size(_) -> true.

bin_size_calc(utf8, Src, _Size, _Unit) ->
    {8,{bs_utf8_size,Src}};
bin_size_calc(utf16, Src, _Size, _Unit) ->
    {8,{bs_utf16_size,Src}};
bin_size_calc(utf32, _Src, _Size, _Unit) ->
    {8,#b_literal{val=4}};
bin_size_calc(binary, Src, #b_literal{val=all}, Unit) ->
    case Unit rem 8 of
        0 -> {8,{{bif,byte_size},Src}};
        _ -> {1,{{bif,bit_size},Src}}
    end;
bin_size_calc(_Type, _Src, Size, Unit) ->
    {Unit,Size}.

fold_size_calc([{Unit,#b_literal{val=Size}}|T], Bits, Acc) ->
    if
        is_integer(Size) ->
            fold_size_calc(T, Bits + Unit*Size, Acc);
        true ->
            error
    end;
fold_size_calc([{U,#b_var{}}=H|T], Bits, Acc) when U =:= 1; U =:= 8 ->
    fold_size_calc(T, Bits, [H|Acc]);
fold_size_calc([{U,#b_var{}=Var}|T], Bits, Acc) ->
    fold_size_calc(T, Bits, [{1,{'*',#b_literal{val=U},Var}}|Acc]);
fold_size_calc([{_,_}=H|T], Bits, Acc) ->
    fold_size_calc(T, Bits, [H|Acc]);
fold_size_calc([], Bits, Acc) ->
    Bytes = Bits div 8,
    RemBits = Bits rem 8,
    Sizes = sort([{1,#b_literal{val=RemBits}},{8,#b_literal{val=Bytes}}|Acc]),
    [Pair || {_,Sz}=Pair <- Sizes, Sz =/= #b_literal{val=0}].

%%%
%%% Utilities for creating the SSA types.
%%%

ssa_args(As, St) ->
    [ssa_arg(A, St) || A <- As].

ssa_arg(#k_var{name=V}, #cg{vars=Vars}) -> maps:get(V, Vars);
ssa_arg(#k_literal{val=V}, _) -> #b_literal{val=V};
ssa_arg(#k_atom{val=V}, _) -> #b_literal{val=V};
ssa_arg(#k_float{val=V}, _) -> #b_literal{val=V};
ssa_arg(#k_int{val=V}, _) -> #b_literal{val=V};
ssa_arg(#k_nil{}, _) -> #b_literal{val=[]}.

new_ssa_vars(Vs, St) ->
    mapfoldl(fun(#k_var{name=V}, S) ->
                     new_ssa_var(V, S)
             end, St, Vs).

new_ssa_var(VarBase, #cg{lcount=Uniq,vars=Vars}=St0)
  when is_atom(VarBase); is_integer(VarBase) ->
    case Vars of
        #{VarBase:=_} ->
            Var = #b_var{name={VarBase,Uniq}},
            St = St0#cg{lcount=Uniq+1,vars=Vars#{VarBase=>Var}},
            {Var,St};
        #{} ->
            Var = #b_var{name=VarBase},
            St = St0#cg{vars=Vars#{VarBase=>Var}},
            {Var,St}
    end.

set_ssa_var(VarBase, Val, #cg{vars=Vars}=St)
  when is_atom(VarBase); is_integer(VarBase) ->
    St#cg{vars=Vars#{VarBase=>Val}}.

%% new_label(St) -> {L,St}.

new_label(#cg{lcount=Next}=St) ->
    {Next,St#cg{lcount=Next+1}}.

%% line_anno(Le) -> #{} | #{location:={File,Line}}.
%%  Create a location annotation, containing information about the
%%  current filename and line number.  The annotation should be
%%  included in any operation that could cause an exception.

line_anno(#k{a=Anno}) ->
    line_anno_1(Anno).

line_anno_1([Line,{file,Name}]) when is_integer(Line) ->
    line_anno_2(Name, Line);
line_anno_1([_|_]=A) ->
    {Name,Line} = find_loc(A, no_file, 0),
    line_anno_2(Name, Line);
line_anno_1([]) ->
    #{}.

line_anno_2(no_file, _) ->
    #{};
line_anno_2(_, 0) ->
    %% Missing line number or line number 0.
    #{};
line_anno_2(Name, Line) ->
    #{location=>{Name,Line}}.

find_loc([Line|T], File, _) when is_integer(Line) ->
    find_loc(T, File, Line);
find_loc([{file,File}|T], _, Line) ->
    find_loc(T, File, Line);
find_loc([_|T], File, Line) ->
    find_loc(T, File, Line);
find_loc([], File, Line) -> {File,Line}.

flatmapfoldl(F, Accu0, [Hd|Tail]) ->
    {R,Accu1} = F(Hd, Accu0),
    {Rs,Accu2} = flatmapfoldl(F, Accu1, Tail),
    {R++Rs,Accu2};
flatmapfoldl(_, Accu, []) -> {[],Accu}.

%%%
%%% Finalize the code.
%%%

finalize(Asm0, St0) ->
    Asm1 = fix_phis(Asm0),
    {Asm,St} = fix_sets(Asm1, [], St0),
    {build_map(Asm),St}.

fix_phis(Is) ->
    fix_phis_1(Is, none, #{}).

fix_phis_1([{label,L},#cg_phi{vars=[]}=Phi|Is0], _Lbl, Map0) ->
    case maps:is_key(L, Map0) of
        false ->
            %% No #cg_break{} references this label. Nothing else can
            %% reference it, so it can be safely be removed.
            {Is,Map} = drop_upto_label(Is0, Map0),
            fix_phis_1(Is, none, Map);
        true ->
            %% There is a break referencing this label; probably caused
            %% by a try/catch whose return value is ignored.
            [{label,L}|fix_phis_1([Phi|Is0], L, Map0)]
    end;
fix_phis_1([{label,L}=I|Is], _Lbl, Map) ->
    [I|fix_phis_1(Is, L, Map)];
fix_phis_1([#cg_unreachable{}|Is0], _Lbl, Map0) ->
    {Is,Map} = drop_upto_label(Is0, Map0),
    fix_phis_1(Is, none, Map);
fix_phis_1([#cg_break{args=Args,phi=Target}|Is], Lbl, Map) when is_integer(Lbl) ->
    Pairs1 = case Map of
                 #{Target:=Pairs0} -> Pairs0;
                 #{} -> []
             end,
    Pairs = [[{Arg,Lbl} || Arg <- Args]|Pairs1],
    I = make_uncond_branch(Target),
    [I|fix_phis_1(Is, none, Map#{Target=>Pairs})];
fix_phis_1([#cg_phi{vars=Vars}|Is0], Lbl, Map0) ->
    Pairs = maps:get(Lbl, Map0),
    Map1 = maps:remove(Lbl, Map0),
    case gen_phis(Vars, Pairs) of
        [#b_set{op=phi,args=[]}] ->
            {Is,Map} = drop_upto_label(Is0, Map1),
            Ret = #b_ret{arg=#b_literal{val=unreachable}},
            [Ret|fix_phis_1(Is, none, Map)];
        Phis ->
            Phis ++ fix_phis_1(Is0, Lbl, Map1)
    end;
fix_phis_1([I|Is], Lbl, Map) ->
    [I|fix_phis_1(Is, Lbl, Map)];
fix_phis_1([], _, Map) ->
    [] = maps:to_list(Map),                     %Assertion.
    [].

gen_phis([V|Vs], Preds0) ->
    {Pairs,Preds} = collect_preds(Preds0, [], []),
    [#b_set{op=phi,dst=V,args=Pairs}|gen_phis(Vs, Preds)];
gen_phis([], _) -> [].

collect_preds([[First|Rest]|T], ColAcc, RestAcc) ->
    collect_preds(T, [First|ColAcc], [Rest|RestAcc]);
collect_preds([], ColAcc, RestAcc) ->
    {keysort(2, ColAcc),RestAcc}.

fix_sets([#b_set{dst=none}=Set|Is], Acc, St0) ->
    {Dst,St} = new_ssa_var('@ssa_ignored', St0),
    I = Set#b_set{dst=Dst},
    fix_sets(Is, [I|Acc], St);
fix_sets([I|Is], Acc, St) ->
    fix_sets(Is, [I|Acc], St);
fix_sets([], Acc, St) ->
    {reverse(Acc),St}.

build_map(Is) ->
    Blocks = build_graph_1(Is, [], []),
    maps:from_list(Blocks).

build_graph_1([{label,L}|Is], Lbls, []) ->
    build_graph_1(Is, [L|Lbls], []);
build_graph_1([{label,L}|Is], Lbls, [_|_]=BlockAcc) ->
    make_blocks(Lbls, BlockAcc) ++ build_graph_1(Is, [L], []);
build_graph_1([I|Is], Lbls, BlockAcc) ->
    build_graph_1(Is, Lbls, [I|BlockAcc]);
build_graph_1([], Lbls, BlockAcc) ->
    make_blocks(Lbls, BlockAcc).

make_blocks(Lbls, [Last|Is0]) ->
    Is = reverse(Is0),
    Block = #b_blk{is=Is,last=Last},
    [{L,Block} || L <- Lbls].

drop_upto_label([{label,_}|_]=Is, Map) ->
    {Is,Map};
drop_upto_label([#cg_break{phi=Target}|Is], Map) ->
    Pairs = case Map of
                #{Target:=Pairs0} -> Pairs0;
                #{} -> []
            end,
    drop_upto_label(Is, Map#{Target=>Pairs});
drop_upto_label([_|Is], Map) ->
    drop_upto_label(Is, Map).

k_get_anno(Thing) -> element(2, Thing).
