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

-import(lists, [all/2,append/1,flatmap/2,foldl/3,
                keysort/2,mapfoldl/3,map/2,member/2,
                reverse/1,reverse/2,sort/1]).

-include("v3_kernel.hrl").
-include("beam_ssa.hrl").

-type label() :: beam_ssa:label().

%% Main codegen structure.
-record(cg, {lcount=1 :: label(),   %Label counter
             bfail=1 :: label(),
             catch_label=none :: 'none' | label(),
             vars=#{} :: map(),     %Defined variables.
             break=0 :: label(),    %Break label
             recv=0 :: label(),     %Receive label
             ultimate_failure=0 :: label(), %Label for ultimate match failure.
             labels=#{} :: #{atom() => label()}
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
    ?EXCEPTION_BLOCK = UltimateFail,            %Assertion.
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
cg(#k_put{anno=Le,arg=Con,ret=Var}, St) ->
    put_cg(Var, Con, Le, St);
cg(#k_return{args=[Ret0]}, St) ->
    Ret = ssa_arg(Ret0, St),
    {[#b_ret{arg=Ret}],St};
cg(#k_break{args=Bs}, #cg{break=Br}=St) ->
    Args = ssa_args(Bs, St),
    {[#cg_break{args=Args,phi=Br}],St};
cg(#k_letrec_goto{label=Label,first=First,then=Then,ret=Rs},
   #cg{break=OldBreak,labels=Labels0}=St0) ->
    {Tf,St1} = new_label(St0),
    {B,St2} = new_label(St1),
    Labels = Labels0#{Label=>Tf},
    {Fis,St3} = cg(First, St2#cg{labels=Labels,break=B}),
    {Sis,St4} = cg(Then, St3),
    St5 = St4#cg{labels=Labels0},
    {BreakVars,St} = new_ssa_vars(Rs, St5),
    Phi = #cg_phi{vars=BreakVars},
    {Fis ++ [{label,Tf}] ++ Sis ++ [{label,B},Phi],St#cg{break=OldBreak}};
cg(#k_goto{label=Label}, #cg{labels=Labels}=St) ->
    Branch = map_get(Label, Labels),
    {[make_uncond_branch(Branch)],St}.

%% match_cg(Matc, [Ret], State) -> {[Ainstr],State}.
%%  Generate code for a match.

do_match_cg(M, Rs, #cg{bfail=Bfail,break=OldBreak}=St0) ->
    {B,St1} = new_label(St0),
    {Mis,St2} = match_cg(M, Bfail, St1#cg{break=B}),
    St3 = St2#cg{break=OldBreak},
    {BreakVars,St} = new_ssa_vars(Rs, St3),
    {Mis ++ [{label,B},#cg_phi{vars=BreakVars}],St}.

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

select_val_cg(k_atom, {succeeded,Dst}, Vls, _Tf, _Vf, Sis, St0) ->
    [{#b_literal{val=false},Fail},{#b_literal{val=true},Succ}] = sort(Vls),
    case Dst of
        #b_var{} ->
            %% Generate a `succeeded` instruction and two-way branch
            %% following the `peek_message` and `wait_timeout`
            %% instructions.
            {Bool,St} = new_ssa_var('@ssa_bool', St0),
            Succeeded = #b_set{op=succeeded,dst=Bool,args=[Dst]},
            Br = #b_br{bool=Bool,succ=Succ,fail=Fail},
            {[Succeeded,Br|Sis],St};
        #b_literal{val=true}=Bool ->
            %% A 'wait_timeout 0' instruction was optimized away.
            Br = #b_br{bool=Bool,succ=Succ,fail=Succ},
            {[Br|Sis],St0}
    end;
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

select_nil(#k_val_clause{val=#k_literal{val=[]},body=B}, V, Tf, Vf, St0) ->
    {Bis,St1} = match_cg(B, Vf, St0),
    Src = ssa_arg(V, St1),
    {Is,St} = make_cond_branch({bif,'=:='}, [Src,#b_literal{val=[]}], Tf, St1),
    {Is ++ Bis,St}.

select_binary(#k_val_clause{val=#k_binary{segs=#k_var{name=Ctx0}},body=B},
              #k_var{}=Src, Tf, Vf, St0) ->
    {Ctx,St1} = new_ssa_var(Ctx0, St0),
    {Bis0,St2} = match_cg(B, Vf, St1),
    {TestIs,St} = make_succeeded(Ctx, {guard, Tf}, St2),
    Bis1 = [#b_set{op=bs_start_match,dst=Ctx,
                   args=[#b_literal{val=new},
                         ssa_arg(Src, St)]}] ++ TestIs ++ Bis0,
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

%%
%% The 'succeeded' instruction needs special treatment in catch blocks to
%% prevent the checked operation from being optimized away if a later pass
%% determines that it always fails.
%%

make_succeeded(Var, {in_catch, CatchLbl}, St0) ->
    {Bool, St1} = new_ssa_var('@ssa_bool', St0),
    {Succ, St2} = new_label(St1),
    {Fail, St} = new_label(St2),

    Check = [#b_set{op=succeeded,dst=Bool,args=[Var]},
             #b_br{bool=Bool,succ=Succ,fail=Fail}],

    %% Add a dummy block that references the checked variable, ensuring it
    %% stays alive and that it won't be merged with the landing pad.
    Trampoline = [{label,Fail},
                  #b_set{op=exception_trampoline,args=[Var]},
                  make_uncond_branch(CatchLbl)],

    {Check ++ Trampoline ++ [{label,Succ}], St};
make_succeeded(Var, {no_catch, Fail}, St) ->
    %% Ultimate failure raises an exception, so we must treat it as if it were
    %% in a catch to keep it from being optimized out.
    #cg{ultimate_failure=Fail} = St,            %Assertion
    make_succeeded(Var, {in_catch, Fail}, St);
make_succeeded(Var, {guard, Fail}, St) ->
    make_cond_branch(succeeded, [Var], Fail, St).

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
    Size = case {Size0,ssa_arg(Size0, St0)} of
               {#k_var{},#b_literal{val=all}} ->
                   %% The size `all` is used for the size of the final binary
                   %% segment in a pattern. Using `all` explicitly is not allowed,
                   %% so we convert it to an obvious invalid size.
                   #b_literal{val=bad_size};
               {_,Size1} ->
                   Size1
           end,
    build_bs_instr(Anno, Type, Vf, Ctx, Size, Unit, Flags, Dst, St1).

select_extract_int(#k_var{name=Tl}, 0, #k_literal{val=0}, _U, _Fs, _Vf,
                   Ctx, St0) ->
    St = set_ssa_var(Tl, Ctx, St0),
    {[],St};
select_extract_int(#k_var{name=Tl}, Val, #k_literal{val=Sz}, U, Fs, Vf,
                   Ctx, St0) when is_integer(Sz) ->
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
    {TestIs,St} = make_succeeded(Dst, {guard, Vf}, St1),
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
    {Is,St} = make_succeeded(Dst, {guard, Fail}, St0),
    {[Get|Is],St}.

select_val(#k_val_clause{val=#k_tuple{es=Es},body=B}, V, Vf, St0) ->
    {Eis,St1} = select_extract_tuple(V, Es, St0),
    {Bis,St2} = match_cg(B, Vf, St1),
    {length(Es),Eis ++ Bis,St2};
select_val(#k_val_clause{val=#k_literal{val=Val},body=B}, _V, Vf, St0) ->
    {Bis,St1} = match_cg(B, Vf, St0),
    {Val,Bis,St1}.

%% select_extract_tuple(Src, [V], State) -> {[E],State}.
%%  Extract tuple elements, but only if they are actually used.
%%
%%  Not extracting tuple elements that are not used is an
%%  optimization for compile time and memory use during compilation.
%%  It is probably worthwhile because it is common to extract only a
%%  few elements from a huge record.

select_extract_tuple(Src, Vs, St0) ->
    Tuple = ssa_arg(Src, St0),
    F = fun (#k_var{anno=Anno,name=V}, {Elem,S0}) ->
                case member(unused, Anno) of
                    true ->
                        {[],{Elem+1,S0}};
                    false ->
                        Args = [Tuple,#b_literal{val=Elem}],
                        {Dst,S} = new_ssa_var(V, S0),
                        Get = #b_set{op=get_tuple_element,
                                     dst=Dst,args=Args},
                        {[Get],{Elem+1,S}}
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
    {TestIs,St2} = make_succeeded(Dst, {guard, Fail}, St1),
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

guard_cg(#k_try{arg=Ts,vars=[],body=#k_break{args=[]},
                evars=[],handler=#k_break{args=[]}},
         Fail,
         #cg{bfail=OldBfail,break=OldBreak}=St0) ->
    %% Do a try/catch without return value for effect. The return
    %% value is not checked; success passes on to the next instruction
    %% and failure jumps to Fail.
    {Next,St1} = new_label(St0),
    {Tis,St2} = guard_cg(Ts, Fail, St1#cg{bfail=Fail,break=Next}),
    Is = Tis ++ [{label,Next},#cg_phi{vars=[]}],
    {Is,St2#cg{bfail=OldBfail,break=OldBreak}};
guard_cg(#k_test{op=Test0,args=As}, Fail, St0) ->
    #k_remote{mod=#k_literal{val=erlang},name=#k_literal{val=Test}} = Test0,
    test_cg(Test, false, As, Fail, St0);
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
            false = Inverted,                   %Assertion.
            test_is_record_cg(Fail, Tuple, Tag, Arity, St0);
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

test_is_record_cg(Fail, Tuple, TagVal, ArityVal, St0) ->
    {Arity,St1} = new_ssa_var('@ssa_arity', St0),
    {Tag,St2} = new_ssa_var('@ssa_tag', St1),
    {Is0,St3} = make_cond_branch({bif,is_tuple}, [Tuple], Fail, St2),
    GetArity = #b_set{op={bif,tuple_size},dst=Arity,args=[Tuple]},
    {Is1,St4} = make_cond_branch({bif,'=:='}, [Arity,ArityVal], Fail, St3),
    GetTag = #b_set{op=get_tuple_element,dst=Tag,
                    args=[Tuple,#b_literal{val=0}]},
    {Is2,St} = make_cond_branch({bif,'=:='}, [Tag,TagVal], Fail, St4),
    Is = Is0 ++ [GetArity] ++ Is1 ++ [GetTag] ++ Is2,
    {Is,St}.

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

%% fail_context(State) -> {Where,FailureLabel}.
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

fail_context(#cg{catch_label=Catch,bfail=Fail,ultimate_failure=Ult}) ->
    if
        Fail =/= Ult ->
            {guard,Fail};
        Catch =:= none ->
            {no_catch,Fail};
        is_integer(Catch) ->
            {in_catch,Catch}
    end.

%% call_cg(Func, [Arg], [Ret], Le, State) ->
%%      {[Ainstr],State}.
%% enter_cg(Func, [Arg], Le, St) -> {[Ainstr],St}.
%%  Generate code for call and enter.

call_cg(Func, As, [], Le, St) ->
    call_cg(Func, As, [#k_var{name='@ssa_ignored'}], Le, St);
call_cg(Func, As, [#k_var{name=R}|MoreRs]=Rs, Le, St0) ->
    case fail_context(St0) of
        {guard,Fail} ->
            %% Inside a guard. The only allowed function call is to
            %% erlang:error/1,2. We will generate a branch to the
            %% failure branch.
            #k_remote{mod=#k_literal{val=erlang},
                      name=#k_literal{val=error}} = Func, %Assertion.
            [#k_var{name=DestVar}] = Rs,
            St = set_ssa_var(DestVar, #b_literal{val=unused}, St0),
            {[make_uncond_branch(Fail),#cg_unreachable{}],St};
        FailCtx ->
            %% Ordinary function call in a function body.
            Args = ssa_args([Func|As], St0),
            {Ret,St1} = new_ssa_var(R, St0),
            Call = #b_set{anno=line_anno(Le),op=call,dst=Ret,args=Args},

            %% If this is a call to erlang:error(), MoreRs could be a
            %% nonempty list of variables that each need a value.
            St2 = foldl(fun(#k_var{name=Dummy}, S) ->
                                set_ssa_var(Dummy, #b_literal{val=unused}, S)
                        end, St1, MoreRs),

            {TestIs,St} = make_succeeded(Ret, FailCtx, St2),
            {[Call|TestIs],St}
    end.

enter_cg(Func, As0, Le, St0) ->
    %% Adding a trampoline here would give us greater freedom in rewriting
    %% calls, but doing so makes it difficult to tell tail calls apart from
    %% body calls during code generation.
    %%
    %% We therefore skip the trampoline, reasoning that we've already left the
    %% current function by the time an exception is thrown.
    As = ssa_args([Func|As0], St0),
    {Ret,St} = new_ssa_var('@ssa_ret', St0),
    Call = #b_set{anno=line_anno(Le),op=call,dst=Ret,args=As},
    {[Call,#b_ret{arg=Ret}],St}.

%% bif_cg(#k_bif{}, Le,State) -> {[Ainstr],State}.
%%  Generate code for a guard BIF or primop.

bif_cg(#k_bif{op=#k_internal{name=Name},args=As,ret=Rs}, _Le, St) ->
    internal_cg(Name, As, Rs, St);
bif_cg(#k_bif{op=#k_remote{mod=#k_literal{val=erlang},name=#k_literal{val=Name}},
              args=As,ret=Rs}, Le, St) ->
    bif_cg(Name, As, Rs, Le, St).

%% internal_cg(Bif, [Arg], [Ret], Le, State) ->
%%      {[Ainstr],State}.

internal_cg(raise, As, [#k_var{name=Dst0}], St0) ->
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
internal_cg(recv_peek_message, [], [#k_var{name=Succeeded0},
                                    #k_var{name=Dst0}], St0) ->
    {Dst,St1} = new_ssa_var(Dst0, St0),
    St = new_succeeded_value(Succeeded0, Dst, St1),
    Set = #b_set{op=peek_message,dst=Dst,args=[]},
    {[Set],St};
internal_cg(recv_wait_timeout, As, [#k_var{name=Succeeded0}], St0) ->
    case ssa_args(As, St0) of
        [#b_literal{val=0}] ->
            %% If beam_ssa_opt is run (which is default), the
            %% `wait_timeout` instruction will be removed if the
            %% operand is a literal 0.  However, if optimizations have
            %% been turned off, we must not not generate a
            %% `wait_timeout` instruction with a literal 0 timeout,
            %% because the BEAM instruction will not handle it
            %% correctly.
            St = new_succeeded_value(Succeeded0, #b_literal{val=true}, St0),
            {[],St};
        Args ->
            {Wait,St1} = new_ssa_var('@ssa_wait', St0),
            St = new_succeeded_value(Succeeded0, Wait, St1),
            Set = #b_set{op=wait_timeout,dst=Wait,args=Args},
            {[Set],St}
    end;
internal_cg(Op, As, [#k_var{name=Dst0}], St0) when is_atom(Op) ->
    %% This behaves like a function call.
    {Dst,St} = new_ssa_var(Dst0, St0),
    Args = ssa_args(As, St),
    Set = #b_set{op=Op,dst=Dst,args=Args},
    {[Set],St};
internal_cg(Op, As, [], St0) when is_atom(Op) ->
    %% This behaves like a function call.
    {Dst,St} = new_ssa_var('@ssa_ignored', St0),
    Args = ssa_args(As, St),
    Set = #b_set{op=Op,dst=Dst,args=Args},
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
                    FailCtx = fail_context(St1),
                    {Is,St} = make_succeeded(Dst, FailCtx, St1),
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
    case {Vs,Tb,Th,is_guard_cg_safe_list(Ais)} of
        {[#k_var{name=X}],#k_break{args=[#k_var{name=X}]},
         #k_break{args=[#k_literal{}]},true} ->
            %% There are no instructions that will clobber X registers
            %% and the exception is not matched. Therefore, a
            %% try/catch is not needed. This code is probably located
            %% in a guard.
            {ProtIs,St9} = guard_cg(Ta, H, St7#cg{break=B,bfail=H}),
            {His,St10} = cg(Th, St9),
            {RetVars,St} = new_ssa_vars(Rs, St10),
            Is = ProtIs ++ [{label,H}] ++ His ++
                [{label,B},#cg_phi{vars=RetVars}],
            {Is,St#cg{break=St0#cg.break,bfail=St7#cg.bfail}};
        {_,_,_,_} ->
            %% The general try/catch (not in a guard).
            St9 = St8#cg{break=E,catch_label=St7#cg.catch_label},
            {Bis,St10} = cg(Tb, St9),
            {His,St11} = cg(Th, St10),
            {BreakVars,St12} = new_ssa_vars(Rs, St11),
            {CatchedAgg,St13} = new_ssa_var('@ssa_agg', St12),
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
             St13#cg{break=St0#cg.break}}
    end.

is_guard_cg_safe_list(Is) ->
    all(fun is_guard_cg_safe/1, Is).

is_guard_cg_safe(#b_set{op=call,args=Args}) ->
    case Args of
        [#b_remote{mod=#b_literal{val=erlang},
                   name=#b_literal{val=error},
                   arity=1}|_] ->
            true;
        _ ->
            false
    end;
is_guard_cg_safe(#b_set{}=I) -> not beam_ssa:clobbers_xregs(I);
is_guard_cg_safe(#b_br{}) -> true;
is_guard_cg_safe(#b_switch{}) -> true;
is_guard_cg_safe(#cg_break{}) -> true;
is_guard_cg_safe(#cg_phi{}) -> true;
is_guard_cg_safe({label,_}) -> true;
is_guard_cg_safe(#cg_unreachable{}) -> false.

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
    FailCtx = fail_context(St0),
    {Dst,St1} = new_ssa_var(R, St0),
    cg_binary(Dst, Segs, FailCtx, Le, St1);
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
    Args = [#b_literal{val=Op},SrcMap|List],
    PutMap = #b_set{anno=LineAnno,op=put_map,dst=Dst,args=Args},
    if
        Op =:= assoc ->
            {[PutMap],St0};
        true ->
            FailCtx = fail_context(St0),
            {Is,St} = make_succeeded(Dst, FailCtx, St0),
            {[PutMap|Is],St}
    end.

%%%
%%% Code generation for constructing binaries.
%%%

cg_binary(Dst, Segs0, FailCtx, Le, St0) ->
    {PutCode0,SzCalc0,St1} = cg_bin_put(Segs0, FailCtx, St0),
    LineAnno = line_anno(Le),
    Anno = Le,
    case PutCode0 of
        [#b_set{op=bs_put,dst=Bool,args=[_,_,Src,#b_literal{val=all}|_]},
         #b_br{bool=Bool},
         {label,_}|_] ->
            #k_bin_seg{unit=Unit0,next=Segs} = Segs0,
            Unit = #b_literal{val=Unit0},
            {PutCode,SzCalc1,St2} = cg_bin_put(Segs, FailCtx, St1),
            {_,SzVar,SzCode0,St3} = cg_size_calc(1, SzCalc1, FailCtx, St2),
            SzCode = cg_bin_anno(SzCode0, LineAnno),
            Args = case member(single_use, Anno) of
                       true ->
                           [#b_literal{val=private_append},Src,SzVar,Unit];
                       false ->
                           [#b_literal{val=append},Src,SzVar,Unit]
                   end,
            BsInit = #b_set{anno=LineAnno,op=bs_init,dst=Dst,args=Args},
            {TestIs,St} = make_succeeded(Dst, FailCtx, St3),
            {SzCode ++ [BsInit] ++ TestIs ++ PutCode,St};
        [#b_set{op=bs_put}|_] ->
            {Unit,SzVar,SzCode0,St2} = cg_size_calc(8, SzCalc0, FailCtx, St1),
            SzCode = cg_bin_anno(SzCode0, LineAnno),
            Args = [#b_literal{val=new},SzVar,Unit],
            BsInit = #b_set{anno=LineAnno,op=bs_init,dst=Dst,args=Args},
            {TestIs,St} = make_succeeded(Dst, FailCtx, St2),
            {SzCode ++ [BsInit] ++ TestIs ++ PutCode0,St}
    end.

cg_bin_anno([Set|Sets], Anno) ->
    [Set#b_set{anno=Anno}|Sets];
cg_bin_anno([], _) -> [].

%% cg_size_calc(PreferredUnit, SzCalc, FailCtx, St0) ->
%%         {ActualUnit,SizeVariable,SizeCode,St}.
%%  Generate size calculation code.

cg_size_calc(Unit, error, _FailCtx, St) ->
    {#b_literal{val=Unit},#b_literal{val=badarg},[],St};
cg_size_calc(8, [{1,_}|_]=SzCalc, FailCtx, St) ->
    cg_size_calc(1, SzCalc, FailCtx, St);
cg_size_calc(8, SzCalc, FailCtx, St0) ->
    {Var,Pre,St} = cg_size_calc_1(SzCalc, FailCtx, St0),
    {#b_literal{val=8},Var,Pre,St};
cg_size_calc(1, SzCalc0, FailCtx, St0) ->
    SzCalc = map(fun({8,#b_literal{val=Size}}) ->
                         {1,#b_literal{val=8*Size}};
                    ({8,{{bif,byte_size},Src}}) ->
                         {1,{{bif,bit_size},Src}};
                    ({8,{_,_}=UtfCalc}) ->
                         {1,{'*',#b_literal{val=8},UtfCalc}};
                    ({_,_}=Pair) ->
                         Pair
                 end, SzCalc0),
    {Var,Pre,St} = cg_size_calc_1(SzCalc, FailCtx, St0),
    {#b_literal{val=1},Var,Pre,St}.

cg_size_calc_1(SzCalc, FailCtx, St0) ->
    cg_size_calc_2(SzCalc, #b_literal{val=0}, FailCtx, St0).

cg_size_calc_2([{_,{'*',Unit,{_,_}=Bif}}|T], Sum0, FailCtx, St0) ->
    {Sum1,Pre0,St1} = cg_size_calc_2(T, Sum0, FailCtx, St0),
    {BifDst,Pre1,St2} = cg_size_bif(Bif, FailCtx, St1),
    {Sum,Pre2,St} = cg_size_add(Sum1, BifDst, Unit, FailCtx, St2),
    {Sum,Pre0++Pre1++Pre2,St};
cg_size_calc_2([{_,#b_literal{}=Sz}|T], Sum0, FailCtx, St0) ->
    {Sum1,Pre0,St1} = cg_size_calc_2(T, Sum0, FailCtx, St0),
    {Sum,Pre,St} = cg_size_add(Sum1, Sz, #b_literal{val=1}, FailCtx, St1),
    {Sum,Pre0++Pre,St};
cg_size_calc_2([{_,#b_var{}=Sz}|T], Sum0, FailCtx, St0) ->
    {Sum1,Pre0,St1} = cg_size_calc_2(T, Sum0, FailCtx, St0),
    {Sum,Pre,St} = cg_size_add(Sum1, Sz, #b_literal{val=1}, FailCtx, St1),
    {Sum,Pre0++Pre,St};
cg_size_calc_2([{_,{_,_}=Bif}|T], Sum0, FailCtx, St0) ->
    {Sum1,Pre0,St1} = cg_size_calc_2(T, Sum0, FailCtx, St0),
    {BifDst,Pre1,St2} = cg_size_bif(Bif, FailCtx, St1),
    {Sum,Pre2,St} = cg_size_add(Sum1, BifDst, #b_literal{val=1}, FailCtx, St2),
    {Sum,Pre0++Pre1++Pre2,St};
cg_size_calc_2([], Sum, _FailCtx, St) ->
    {Sum,[],St}.

cg_size_bif(#b_var{}=Var, _FailCtx, St) ->
    {Var,[],St};
cg_size_bif({Name,Src}, FailCtx, St0) ->
    {Dst,St1} = new_ssa_var('@ssa_bif', St0),
    Bif = #b_set{op=Name,dst=Dst,args=[Src]},
    {TestIs,St} = make_succeeded(Dst, FailCtx, St1),
    {Dst,[Bif|TestIs],St}.

cg_size_add(#b_literal{val=0}, Val, #b_literal{val=1}, _FailCtx, St) ->
    {Val,[],St};
cg_size_add(A, B, Unit, FailCtx, St0) ->
    {Dst,St1} = new_ssa_var('@ssa_sum', St0),
    {TestIs,St} = make_succeeded(Dst, FailCtx, St1),
    BsAdd = #b_set{op=bs_add,dst=Dst,args=[A,B,Unit]},
    {Dst,[BsAdd|TestIs],St}.

cg_bin_put(Seg, FailCtx, St) ->
    cg_bin_put_1(Seg, FailCtx, [], [], St).

cg_bin_put_1(#k_bin_seg{size=Size0,unit=U,type=T,flags=Fs,seg=Src0,next=Next},
             FailCtx, Acc, SzCalcAcc, St0) ->
    [Src,Size] = ssa_args([Src0,Size0], St0),
    NeedSize = bs_need_size(T),
    TypeArg = #b_literal{val=T},
    Flags = #b_literal{val=Fs},
    Unit = #b_literal{val=U},
    Args = case NeedSize of
               true -> [TypeArg,Flags,Src,Size,Unit];
               false -> [TypeArg,Flags,Src]
           end,
    %% bs_put has its own 'succeeded' logic, and should always jump directly to
    %% the fail label regardless of whether it's in a catch or not.
    {_, FailLbl} = FailCtx,
    {Is,St} = make_cond_branch(bs_put, Args, FailLbl, St0),
    SzCalc = bin_size_calc(T, Src, Size, U),
    cg_bin_put_1(Next, FailCtx, reverse(Is, Acc), [SzCalc|SzCalcAcc], St);
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

ssa_arg(#k_var{name=V}, #cg{vars=Vars}) -> map_get(V, Vars);
ssa_arg(#k_literal{val=V}, _) -> #b_literal{val=V};
ssa_arg(#k_remote{mod=Mod0,name=Name0,arity=Arity}, St) ->
    Mod = ssa_arg(Mod0, St),
    Name = ssa_arg(Name0, St),
    #b_remote{mod=Mod,name=Name,arity=Arity};
ssa_arg(#k_local{name=Name,arity=Arity}, _) when is_atom(Name) ->
    #b_local{name=#b_literal{val=Name},arity=Arity}.

new_succeeded_value(VarBase, Var, #cg{vars=Vars0}=St) ->
    Vars = Vars0#{VarBase=>{succeeded,Var}},
    St#cg{vars=Vars}.

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

line_anno([Line,{file,Name}]) when is_integer(Line) ->
    line_anno_1(Name, Line);
line_anno([_|_]=A) ->
    {Name,Line} = find_loc(A, no_file, 0),
    line_anno_1(Name, Line);
line_anno([]) ->
    #{}.

line_anno_1(no_file, _) ->
    #{};
line_anno_1(_, 0) ->
    %% Missing line number or line number 0.
    #{};
line_anno_1(Name, Line) ->
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

fix_sets([#b_set{op=Op,dst=Dst}=Set,#b_ret{arg=Dst}=Ret|Is], Acc, St) ->
    NoValue = case Op of
                  remove_message -> true;
                  timeout -> true;
                  _ -> false
              end,
    case NoValue of
        true ->
            %% An instruction without value was used in effect
            %% context in `after` block. Example:
            %%
            %%   try
            %%       ...
            %%   after
            %%       receive _ -> ignored end
            %%   end,
            %%   ok.
            %%
            fix_sets(Is, [Ret#b_ret{arg=#b_literal{val=ok}},Set|Acc], St);
        false ->
            fix_sets(Is, [Ret,Set|Acc], St)
    end;
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
