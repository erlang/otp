%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%% Purpose : Code generator for Beam.

-module(v3_codegen).

%% The main interface.
-export([module/2]).

-import(lists, [member/2,keymember/3,keysort/2,keydelete/3,
		append/1,flatmap/2,filter/2,foldl/3,foldr/3,mapfoldl/3,
		sort/1,reverse/1,reverse/2,map/2]).
-import(ordsets, [add_element/2,intersection/2,union/2]).

-include("v3_kernel.hrl").

%% These are not defined in v3_kernel.hrl.
get_kanno(Kthing) -> element(2, Kthing).
set_kanno(Kthing, Anno) -> setelement(2, Kthing, Anno).

%% Main codegen structure.
-record(cg, {lcount=1,				%Label counter
	     bfail,				%Fail label for BIFs
	     break,				%Break label
	     recv,				%Receive label
	     is_top_block,			%Boolean: top block or not
	     functable=#{},	                %Map of local functions: {Name,Arity}=>Label
	     in_catch=false,			%Inside a catch or not.
	     need_frame,			%Need a stack frame.
	     ultimate_failure,			%Label for ultimate match failure.
             ctx                                %Match context.
	    }).

%% Stack/register state record.
-record(sr, {reg=[],				%Register table
	     stk=[],				%Stack table
	     res=[]}).				%Registers to reserve

%% Internal records.
-record(cg_need_heap, {anno=[] :: term(),
                       h=0 :: integer()}).
-record(cg_block, {anno=[] :: term(),
                   es=[] :: [term()]}).

-type vdb_entry() :: {atom(),non_neg_integer(),non_neg_integer()}.

-record(l, {i=0 :: non_neg_integer(),           %Op number
	    vdb=[] :: [vdb_entry()],            %Variable database
	    a=[] :: [term()]}).                 %Core annotation

-spec module(#k_mdef{}, [compile:option()]) -> {'ok',beam_asm:module_code()}.

module(#k_mdef{name=Mod,exports=Es,attributes=Attr,body=Forms}, _Opts) ->
    {Asm,St} = functions(Forms, {atom,Mod}),
    {ok,{Mod,Es,Attr,Asm,St#cg.lcount}}.

functions(Forms, AtomMod) ->
    mapfoldl(fun (F, St) -> function(F, AtomMod, St) end, #cg{lcount=1}, Forms).

function(#k_fdef{anno=#k{a=Anno},func=Name,arity=Arity,
                 vars=As,body=Kb}, AtomMod, St0) ->
    try
        #k_match{} = Kb,                   %Assertion.

        %% Try to suppress the stack frame unless it is
        %% really needed.
        Body0 = avoid_stack_frame(Kb),

        %% Annotate kernel records with variable usage.
        Vdb0 = init_vars(As),
        {Body,_,Vdb} = body(Body0, 1, Vdb0),

        %% Generate the BEAM assembly code.
        {Asm,EntryLabel,St} = cg_fun(Body, As, Vdb, AtomMod,
                                     {Name,Arity}, Anno, St0),
        Func = {function,Name,Arity,EntryLabel,Asm},
        {Func,St}
    catch
        Class:Error:Stack ->
            io:fwrite("Function: ~w/~w\n", [Name,Arity]),
            erlang:raise(Class, Error, Stack)
    end.


%% avoid_stack_frame(Kernel) -> Kernel'
%%  If possible, avoid setting up a stack frame. Functions
%%  that only do matching, calls to guard BIFs, and tail-recursive
%%  calls don't need a stack frame.

avoid_stack_frame(#k_match{body=Body}=M) ->
    try
        M#k_match{body=avoid_stack_frame_1(Body)}
    catch
        impossible ->
            M
    end.

avoid_stack_frame_1(#k_alt{first=First0,then=Then0}=Alt) ->
    First = avoid_stack_frame_1(First0),
    Then = avoid_stack_frame_1(Then0),
    Alt#k_alt{first=First,then=Then};
avoid_stack_frame_1(#k_bif{op=Op}=Bif) ->
    case Op of
        #k_internal{} ->
            %% Most internal BIFs clobber the X registers.
            throw(impossible);
        _ ->
            Bif
    end;
avoid_stack_frame_1(#k_break{anno=Anno,args=Args}) ->
    #k_guard_break{anno=Anno,args=Args};
avoid_stack_frame_1(#k_guard_break{}=Break) ->
    Break;
avoid_stack_frame_1(#k_enter{}=Enter) ->
    %% Tail-recursive calls don't need a stack frame.
    Enter;
avoid_stack_frame_1(#k_guard{clauses=Cs0}=Guard) ->
    Cs = avoid_stack_frame_list(Cs0),
    Guard#k_guard{clauses=Cs};
avoid_stack_frame_1(#k_guard_clause{guard=G0,body=B0}=C) ->
    G = avoid_stack_frame_1(G0),
    B = avoid_stack_frame_1(B0),
    C#k_guard_clause{guard=G,body=B};
avoid_stack_frame_1(#k_match{anno=A,vars=Vs,body=B0,ret=Ret}) ->
    %% Use #k_guard_match{} instead to avoid saving the X registers
    %% to the stack before matching.
    B = avoid_stack_frame_1(B0),
    #k_guard_match{anno=A,vars=Vs,body=B,ret=Ret};
avoid_stack_frame_1(#k_guard_match{body=B0}=M) ->
    B = avoid_stack_frame_1(B0),
    M#k_guard_match{body=B};
avoid_stack_frame_1(#k_protected{arg=Arg0}=Prot) ->
    Arg = avoid_stack_frame_1(Arg0),
    Prot#k_protected{arg=Arg};
avoid_stack_frame_1(#k_put{}=Put) ->
    Put;
avoid_stack_frame_1(#k_return{}=Ret) ->
    Ret;
avoid_stack_frame_1(#k_select{var=#k_var{anno=Vanno},types=Types0}=Select) ->
    case member(reuse_for_context, Vanno) of
        false ->
            Types = avoid_stack_frame_list(Types0),
            Select#k_select{types=Types};
        true ->
            %% Including binary patterns that overwrite the register containing
            %% the binary with the match context may not be safe. For example,
            %% bs_match_SUITE:bin_tail_e/1 with inlining will be rejected by
            %% beam_validator.
            %%
            %% Essentially the following code is produced:
            %%
            %% bs_match {x,0} => {x,0}
            %% ...
            %% bs_match {x,0} => {x,1}  %% ILLEGAL
            %%
            %% A bs_match instruction will only accept a match context as the
            %% source operand if the source and destination registers are the
            %% the same (as in the first bs_match instruction above).
            %% The second bs_match instruction is therefore illegal.
            %%
            %% This situation is avoided if there is a stack frame:
            %%
            %% move {x,0} => {y,0}
            %% bs_match {x,0} => {x,0}
            %% ...
            %% bs_match {y,0} => {x,1}  %% LEGAL
            %%
            throw(impossible)
    end;
avoid_stack_frame_1(#k_seq{arg=#k_call{anno=Anno,op=Op}=Call,
                           body=#k_break{args=BrArgs0}}=Seq) ->
    case Op of
        #k_remote{mod=#k_atom{val=Mod},
                  name=#k_atom{val=Name},
                  arity=Arity} ->
            case erl_bifs:is_exit_bif(Mod, Name, Arity) of
                false ->
                    %% Will clobber X registers. Must have a stack frame.
                    throw(impossible);
                true ->
                    %% The call to this BIF will never return. It is safe
                    %% to suppress the stack frame.
                    Bif = #k_bif{anno=Anno,
                                 op=#k_internal{name=guard_error,arity=1},
                                 args=[Call],ret=[]},
                    BrArgs = lists:duplicate(length(BrArgs0), #k_nil{}),
                    GB = #k_guard_break{anno=#k{us=[],ns=[],a=[]},args=BrArgs},
                    Seq#k_seq{arg=Bif,body=GB}
            end;
        _ ->
            %% Will clobber X registers. Must have a stack frame.
            throw(impossible)
    end;
avoid_stack_frame_1(#k_seq{arg=A0,body=B0}=Seq) ->
    A = avoid_stack_frame_1(A0),
    B = avoid_stack_frame_1(B0),
    Seq#k_seq{arg=A,body=B};
avoid_stack_frame_1(#k_test{}=Test) ->
    Test;
avoid_stack_frame_1(#k_type_clause{values=Values0}=TC) ->
    Values = avoid_stack_frame_list(Values0),
    TC#k_type_clause{values=Values};
avoid_stack_frame_1(#k_val_clause{body=B0}=VC) ->
    B = avoid_stack_frame_1(B0),
    VC#k_val_clause{body=B};
avoid_stack_frame_1(_Body) ->
    throw(impossible).

avoid_stack_frame_list([H|T]) ->
    [avoid_stack_frame_1(H)|avoid_stack_frame_list(T)];
avoid_stack_frame_list([]) -> [].


%% This pass creates beam format annotated with variable lifetime
%% information.  Each thing is given an index and for each variable we
%% store the first and last index for its occurrence.  The variable
%% database, VDB, attached to each thing is only relevant internally
%% for that thing.
%%
%% For nested things like matches the numbering continues locally and
%% the VDB for that thing refers to the variable usage within that
%% thing.  Variables which live through a such a thing are internally
%% given a very large last index.  Internally the indexes continue
%% after the index of that thing.  This creates no problems as the
%% internal variable info never escapes and externally we only see
%% variable which are alive both before or after.
%%
%% This means that variables never "escape" from a thing and the only
%% way to get values from a thing is to "return" them, with 'break' or
%% 'return'.  Externally these values become the return values of the
%% thing.  This is no real limitation as most nested things have
%% multiple threads so working out a common best variable usage is
%% difficult.

%% body(Kbody, I, Vdb) -> {[Expr],MaxI,Vdb}.
%%  Handle a body.

body(#k_seq{arg=Ke,body=Kb}, I, Vdb0) ->
    %%ok = io:fwrite("life ~w:~p~n", [?LINE,{Ke,I,Vdb0}]),
    A = get_kanno(Ke),
    Vdb1 = use_vars(union(A#k.us, A#k.ns), I, Vdb0),
    {Es,MaxI,Vdb2} = body(Kb, I+1, Vdb1),
    E = expr(Ke, I, Vdb2),
    {[E|Es],MaxI,Vdb2};
body(Ke, I, Vdb0) ->
    %%ok = io:fwrite("life ~w:~p~n", [?LINE,{Ke,I,Vdb0}]),
    A = get_kanno(Ke),
    Vdb1 = use_vars(union(A#k.us, A#k.ns), I, Vdb0),
    E = expr(Ke, I, Vdb1),
    {[E],I,Vdb1}.

%% expr(Kexpr, I, Vdb) -> Expr.

expr(#k_test{anno=A}=Test, I, _Vdb) ->
    Test#k_test{anno=#l{i=I,a=A#k.a}};
expr(#k_call{anno=A}=Call, I, _Vdb) ->
    Call#k_call{anno=#l{i=I,a=A#k.a}};
expr(#k_enter{anno=A}=Enter, I, _Vdb) ->
    Enter#k_enter{anno=#l{i=I,a=A#k.a}};
expr(#k_bif{anno=A}=Bif, I, _Vdb) ->
    Bif#k_bif{anno=#l{i=I,a=A#k.a}};
expr(#k_match{anno=A,body=Kb,ret=Rs}, I, Vdb) ->
    %% Work out imported variables which need to be locked.
    Mdb = vdb_sub(I, I+1, Vdb),
    M = match(Kb, A#k.us, I+1, Mdb),
    L = #l{i=I,vdb=use_vars(A#k.us, I+1, Mdb),a=A#k.a},
    #k_match{anno=L,body=M,ret=Rs};
expr(#k_guard_match{anno=A,body=Kb,ret=Rs}, I, Vdb) ->
    %% Work out imported variables which need to be locked.
    Mdb = vdb_sub(I, I+1, Vdb),
    M = match(Kb, A#k.us, I+1, Mdb),
    L = #l{i=I,vdb=use_vars(A#k.us, I+1, Mdb),a=A#k.a},
    #k_guard_match{anno=L,body=M,ret=Rs};
expr(#k_protected{}=Protected, I, Vdb) ->
    protected(Protected, I, Vdb);
expr(#k_try{anno=A,arg=Ka,vars=Vs,body=Kb,evars=Evs,handler=Kh}=Try, I, Vdb) ->
    %% Lock variables that are alive before the catch and used afterwards.
    %% Don't lock variables that are only used inside the try.
    Tdb0 = vdb_sub(I, I+1, Vdb),
    %% This is the tricky bit. Lock variables in Arg that are used in
    %% the body and handler. Add try tag 'variable'.
    Ab = get_kanno(Kb),
    Ah = get_kanno(Kh),
    Tdb1 = use_vars(union(Ab#k.us, Ah#k.us), I+3, Tdb0),
    Tdb2 = vdb_sub(I, I+2, Tdb1),
    Vnames = fun (Kvar) -> Kvar#k_var.name end,	%Get the variable names
    {Aes,_,Adb} = body(Ka, I+2, add_var({catch_tag,I+1}, I+1, locked, Tdb2)),
    {Bes,_,Bdb} = body(Kb, I+4, new_vars(sort(map(Vnames, Vs)), I+3, Tdb2)),
    {Hes,_,Hdb} = body(Kh, I+4, new_vars(sort(map(Vnames, Evs)), I+3, Tdb2)),
    L = #l{i=I,vdb=Tdb1,a=A#k.a},
    Try#k_try{anno=L,
              arg=#cg_block{es=Aes,anno=#l{i=I+1,vdb=Adb,a=[]}},
              vars=Vs,body=#cg_block{es=Bes,anno=#l{i=I+3,vdb=Bdb,a=[]}},
              evars=Evs,handler=#cg_block{es=Hes,anno=#l{i=I+3,vdb=Hdb,a=[]}}};
expr(#k_try_enter{anno=A,arg=Ka,vars=Vs,body=Kb,evars=Evs,handler=Kh}, I, Vdb) ->
    %% Lock variables that are alive before the catch and used afterwards.
    %% Don't lock variables that are only used inside the try.
    Tdb0 = vdb_sub(I, I+1, Vdb),
    %% This is the tricky bit. Lock variables in Arg that are used in
    %% the body and handler. Add try tag 'variable'.
    Ab = get_kanno(Kb),
    Ah = get_kanno(Kh),
    Tdb1 = use_vars(union(Ab#k.us, Ah#k.us), I+3, Tdb0),
    Tdb2 = vdb_sub(I, I+2, Tdb1),
    Vnames = fun (Kvar) -> Kvar#k_var.name end,	%Get the variable names
    {Aes,_,Adb} = body(Ka, I+2, add_var({catch_tag,I+1}, I+1, 1000000, Tdb2)),
    {Bes,_,Bdb} = body(Kb, I+4, new_vars(sort(map(Vnames, Vs)), I+3, Tdb2)),
    {Hes,_,Hdb} = body(Kh, I+4, new_vars(sort(map(Vnames, Evs)), I+3, Tdb2)),
    L = #l{i=I,vdb=Tdb1,a=A#k.a},
    #k_try_enter{anno=L,
                 arg=#cg_block{es=Aes,anno=#l{i=I+1,vdb=Adb,a=[]}},
                 vars=Vs,body=#cg_block{es=Bes,anno=#l{i=I+3,vdb=Bdb,a=[]}},
                 evars=Evs,handler=#cg_block{es=Hes,anno=#l{i=I+3,vdb=Hdb,a=[]}}};
expr(#k_catch{anno=A,body=Kb}=Catch, I, Vdb) ->
    %% Lock variables that are alive before the catch and used afterwards.
    %% Don't lock variables that are only used inside the catch.
    %% Add catch tag 'variable'.
    Cdb0 = vdb_sub(I, I+1, Vdb),
    {Es,_,Cdb1} = body(Kb, I+1, add_var({catch_tag,I}, I, locked, Cdb0)),
    L = #l{i=I,vdb=Cdb1,a=A#k.a},
    Catch#k_catch{anno=L,body=#cg_block{es=Es}};
expr(#k_receive{anno=A,var=V,body=Kb,action=Ka}=Recv, I, Vdb) ->
    %% Work out imported variables which need to be locked.
    Rdb = vdb_sub(I, I+1, Vdb),
    M = match(Kb, add_element(V#k_var.name, A#k.us), I+1,
              new_vars([V#k_var.name], I, Rdb)),
    {Tes,_,Adb} = body(Ka, I+1, Rdb),
    Le = #l{i=I,vdb=use_vars(A#k.us, I+1, Vdb),a=A#k.a},
    Recv#k_receive{anno=Le,body=M,
                   action=#cg_block{anno=#l{i=I+1,vdb=Adb,a=[]},es=Tes}};
expr(#k_receive_accept{anno=A}, I, _Vdb) ->
    #k_receive_accept{anno=#l{i=I,a=A#k.a}};
expr(#k_receive_next{anno=A}, I, _Vdb) ->
    #k_receive_next{anno=#l{i=I,a=A#k.a}};
expr(#k_put{anno=A}=Put, I, _Vdb) ->
    Put#k_put{anno=#l{i=I,a=A#k.a}};
expr(#k_break{anno=A}=Break, I, _Vdb) ->
    Break#k_break{anno=#l{i=I,a=A#k.a}};
expr(#k_guard_break{anno=A}=Break, I, _Vdb) ->
    Break#k_guard_break{anno=#l{i=I,a=A#k.a}};
expr(#k_return{anno=A}=Ret, I, _Vdb) ->
    Ret#k_return{anno=#l{i=I,a=A#k.a}}.

%% protected(Kprotected, I, Vdb) -> Protected.
%%  Only used in guards.

protected(#k_protected{anno=A,arg=Ts}=Prot, I, Vdb) ->
    %% Lock variables that are alive before try and used afterwards.
    %% Don't lock variables that are only used inside the protected
    %% expression.
    Pdb0 = vdb_sub(I, I+1, Vdb),
    {T,MaxI,Pdb1} = body(Ts, I+1, Pdb0),
    Pdb2 = use_vars(A#k.ns, MaxI+1, Pdb1),	%Save "return" values
    Prot#k_protected{arg=T,anno=#l{i=I,a=A#k.a,vdb=Pdb2}}.

%% match(Kexpr, [LockVar], I, Vdb) -> Expr.
%%  Convert match tree to old format.

match(#k_alt{anno=A,first=Kf,then=Kt}, Ls, I, Vdb0) ->
    Vdb1 = use_vars(union(A#k.us, Ls), I, Vdb0),
    F = match(Kf, Ls, I+1, Vdb1),
    T = match(Kt, Ls, I+1, Vdb1),
    #k_alt{anno=[],first=F,then=T};
match(#k_select{anno=A,types=Kts}=Select, Ls, I, Vdb0) ->
    Vdb1 = use_vars(union(A#k.us, Ls), I, Vdb0),
    Ts = [type_clause(Tc, Ls, I+1, Vdb1) || Tc <- Kts],
    Select#k_select{anno=[],types=Ts};
match(#k_guard{anno=A,clauses=Kcs}, Ls, I, Vdb0) ->
    Vdb1 = use_vars(union(A#k.us, Ls), I, Vdb0),
    Cs = [guard_clause(G, Ls, I+1, Vdb1) || G <- Kcs],
    #k_guard{anno=[],clauses=Cs};
match(Other, Ls, I, Vdb0) ->
    Vdb1 = use_vars(Ls, I, Vdb0),
    {B,_,Vdb2} = body(Other, I+1, Vdb1),
    Le = #l{i=I,vdb=Vdb2,a=[]},
    #cg_block{anno=Le,es=B}.

type_clause(#k_type_clause{anno=A,type=T,values=Kvs}, Ls, I, Vdb0) ->
    %%ok = io:format("life ~w: ~p~n", [?LINE,{T,Kvs}]),
    Vdb1 = use_vars(union(A#k.us, Ls), I+1, Vdb0),
    Vs = [val_clause(Vc, Ls, I+1, Vdb1) || Vc <- Kvs],
    #k_type_clause{anno=[],type=T,values=Vs}.

val_clause(#k_val_clause{anno=A,val=V,body=Kb}, Ls0, I, Vdb0) ->
    New = (get_kanno(V))#k.ns,
    Bus = (get_kanno(Kb))#k.us,
    %%ok = io:format("Ls0 = ~p, Used=~p\n  New=~p, Bus=~p\n", [Ls0,Used,New,Bus]),
    Ls1 = union(intersection(New, Bus), Ls0),	%Lock for safety
    Vdb1 = use_vars(union(A#k.us, Ls1), I+1, new_vars(New, I, Vdb0)),
    B = match(Kb, Ls1, I+1, Vdb1),
    Le = #l{i=I,vdb=use_vars(Bus, I+1, Vdb1),a=A#k.a},
    #k_val_clause{anno=Le,val=V,body=B}.

guard_clause(#k_guard_clause{anno=A,guard=Kg,body=Kb}, Ls, I, Vdb0) ->
    Vdb1 = use_vars(union(A#k.us, Ls), I+2, Vdb0),
    Gdb = vdb_sub(I+1, I+2, Vdb1),
    G = protected(Kg, I+1, Gdb),
    B = match(Kb, Ls, I+2, Vdb1),
    Le = #l{i=I,vdb=use_vars((get_kanno(Kg))#k.us, I+2, Vdb1),a=A#k.a},
    #k_guard_clause{anno=Le,guard=G,body=B}.


%% Here follows the code generator pass.
%%
%% The following assumptions have been made:
%%
%% 1. Matches, i.e. things with {match,M,Ret} wrappers, only return
%% values; no variables are exported. If the match would have returned
%% extra variables then these have been transformed to multiple return
%% values.
%%
%% 2. All BIF's called in guards are gc-safe so there is no need to
%% put thing on the stack in the guard.  While this would in principle
%% work it would be difficult to keep track of the stack depth when
%% trimming.
%%
%% The code generation uses variable lifetime information added by
%% the previous pass to save variables, allocate registers and
%% move registers to the stack when necessary.
%%
%% We try to use a consistent variable name scheme throughout.  The
%% StackReg record is always called Bef,Int<n>,Aft.

%% cg_fun([Lkexpr], [HeadVar], Vdb, State) -> {[Ainstr],State}

cg_fun(Les, Hvs, Vdb, AtomMod, NameArity, Anno, St0) ->
    {Fi,St1} = new_label(St0),			%FuncInfo label
    {Fl,St2} = local_func_label(NameArity, St1),

    %%
    %% The pattern matching compiler (in v3_kernel) no longer
    %% provides its own catch-all clause, because the 
    %% call to erlang:exit/1 caused problem when cases were
    %% used in guards. Therefore, there may be tests that
    %% cannot fail (providing that there is not a bug in a
    %% previous optimzation pass), but still need to provide
    %% a label (there are instructions, such as is_tuple/2,
    %% that do not allow {f,0}).
    %%
    %% We will generate an ultimate failure label and put it
    %% at the end of function, followed by an 'if_end' instruction.
    %% Note that and 'if_end' instruction does not need any
    %% live x registers, so it will always be safe to jump to
    %% it. (We never ever expect the jump to be taken, and in
    %% most functions there will never be any references to
    %% the label in the first place.)
    %%

    {UltimateMatchFail,St3} = new_label(St2),

    %% Create initial stack/register state, clear unused arguments.
    Bef = clear_dead(#sr{reg=foldl(fun (#k_var{name=V}, Reg) ->
					   put_reg(V, Reg)
				   end, [], Hvs),
			 stk=[]}, 0, Vdb),
    {B,_Aft,St} = cg_list(Les, Vdb, Bef,
			  St3#cg{bfail=0,
				 ultimate_failure=UltimateMatchFail,
				 is_top_block=true}),
    {Name,Arity} = NameArity,
    Asm = [{label,Fi},line(Anno),{func_info,AtomMod,{atom,Name},Arity},
	   {label,Fl}|B++[{label,UltimateMatchFail},if_end]],
    {Asm,Fl,St}.

%% cg(Lkexpr, Vdb, StackReg, State) -> {[Ainstr],StackReg,State}.
%%  Generate code for a kexpr.

cg(#cg_block{anno=Le,es=Es}, Vdb, Bef, St) ->
    block_cg(Es, Le, Vdb, Bef, St);
cg(#k_match{anno=Le,body=M,ret=Rs}, Vdb, Bef, St) ->
    match_cg(M, Rs, Le, Vdb, Bef, St);
cg(#k_guard_match{anno=Le,body=M,ret=Rs}, Vdb, Bef, St) ->
    guard_match_cg(M, Rs, Le, Vdb, Bef, St);
cg(#k_call{anno=Le,op=Func,args=As,ret=Rs}, Vdb, Bef, St) ->
    call_cg(Func, As, Rs, Le, Vdb, Bef, St);
cg(#k_enter{anno=Le,op=Func,args=As}, Vdb, Bef, St) ->
    enter_cg(Func, As, Le, Vdb, Bef, St);
cg(#k_bif{anno=Le}=Bif, Vdb, Bef, St) ->
    bif_cg(Bif, Le, Vdb, Bef, St);
cg(#k_receive{anno=Le,timeout=Te,var=Rvar,body=Rm,action=Tes,ret=Rs},
   Vdb, Bef, St) ->
    recv_loop_cg(Te, Rvar, Rm, Tes, Rs, Le, Vdb, Bef, St);
cg(#k_receive_next{anno=Le}, Vdb, Bef, St) ->
    recv_next_cg(Le, Vdb, Bef, St);
cg(#k_receive_accept{}, _Vdb, Bef, St) ->
    {[remove_message],Bef,St};
cg(#k_try{anno=Le,arg=Ta,vars=Vs,body=Tb,evars=Evs,handler=Th,ret=Rs},
   Vdb, Bef, St) ->
    try_cg(Ta, Vs, Tb, Evs, Th, Rs, Le, Vdb, Bef, St);
cg(#k_try_enter{anno=Le,arg=Ta,vars=Vs,body=Tb,evars=Evs,handler=Th},
   Vdb, Bef, St) ->
    try_enter_cg(Ta, Vs, Tb, Evs, Th, Le, Vdb, Bef, St);
cg(#k_catch{anno=Le,body=Cb,ret=[R]}, Vdb, Bef, St) ->
    catch_cg(Cb, R, Le, Vdb, Bef, St);
cg(#k_put{anno=Le,arg=Con,ret=Var},  Vdb, Bef, St) ->
    put_cg(Var, Con, Le, Vdb, Bef, St);
cg(#k_return{anno=Le,args=Rs}, Vdb, Bef, St) ->
    return_cg(Rs, Le, Vdb, Bef, St);
cg(#k_break{anno=Le,args=Bs}, Vdb, Bef, St) ->
    break_cg(Bs, Le, Vdb, Bef, St);
cg(#k_guard_break{anno=Le,args=Bs}, Vdb, Bef, St) ->
    guard_break_cg(Bs, Le, Vdb, Bef, St);
cg(#cg_need_heap{h=H}, _Vdb, Bef, St) ->
    {[{test_heap,H,max_reg(Bef#sr.reg)}],Bef,St}.

%% cg_list([Kexpr], FirstI, Vdb, StackReg, St) -> {[Ainstr],StackReg,St}.

cg_list(Kes, Vdb, Bef, St0) ->
    {Keis,{Aft,St1}} =
	flatmapfoldl(fun (Ke, {Inta,Sta}) ->
			     {Keis,Intb,Stb} = cg(Ke, Vdb, Inta, Sta),
			     {Keis,{Intb,Stb}}
		     end, {Bef,St0}, need_heap(Kes)),
    {Keis,Aft,St1}.

%% need_heap([Lkexpr], I, St) -> [Lkexpr].
%%  Insert need_heap instructions in Kexpr list.  Try to be smart and
%%  collect them together as much as possible.

need_heap(Kes0) ->
    {Kes,H} = need_heap_0(reverse(Kes0), 0, []),

    %% Prepend need_heap if necessary.
    need_heap_need(H) ++ Kes.

need_heap_0([Ke|Kes], H0, Acc) ->
    {Ns,H} = need_heap_1(Ke, H0),
    need_heap_0(Kes, H, [Ke|Ns]++Acc);
need_heap_0([], H, Acc) ->
    {Acc,H}.

need_heap_1(#k_put{arg=#k_binary{}}, H) ->
    {need_heap_need(H),0};
need_heap_1(#k_put{arg=#k_map{}}, H) ->
    {need_heap_need(H),0};
need_heap_1(#k_put{arg=Val}, H) ->
    %% Just pass through adding to needed heap.
    {[],H + case Val of
		#k_cons{} -> 2;
		#k_tuple{es=Es} -> 1 + length(Es);
		_Other -> 0
	    end};
need_heap_1(#k_bif{}=Bif, H) ->
    case is_gc_bif(Bif) of
        false ->
            {[],H};
        true ->
            {need_heap_need(H),0}
    end;
need_heap_1(_Ke, H) ->
    %% Call or call-like instruction such as set_tuple_element/3.
    {need_heap_need(H),0}.

need_heap_need(0) -> [];
need_heap_need(H) -> [#cg_need_heap{h=H}].

%% is_gc_bif(#k_bif{}) -> true|false.
%% is_gc_bif(Name, Arity) -> true|false.
%%  Determines whether the BIF Name/Arity might do a GC.

is_gc_bif(#k_bif{op=#k_remote{name=#k_atom{val=Name}},args=Args}) ->
    is_gc_bif(Name, length(Args));
is_gc_bif(#k_bif{op=#k_internal{}}) ->
    true.

is_gc_bif(hd, 1) -> false;
is_gc_bif(tl, 1) -> false;
is_gc_bif(self, 0) -> false;
is_gc_bif(node, 0) -> false;
is_gc_bif(node, 1) -> false;
is_gc_bif(element, 2) -> false;
is_gc_bif(get, 1) -> false;
is_gc_bif(tuple_size, 1) -> false;
is_gc_bif(Bif, Arity) ->
    not (erl_internal:bool_op(Bif, Arity) orelse
	 erl_internal:new_type_test(Bif, Arity) orelse
	 erl_internal:comp_op(Bif, Arity)).

%% match_cg(Matc, [Ret], Le, Vdb, StackReg, State) ->
%%	{[Ainstr],StackReg,State}.
%%  Generate code for a match.  First save all variables on the stack
%%  that are to survive after the match.  We leave saved variables in
%%  their registers as they might actually be in the right place.

match_cg(M, Rs, Le, Vdb, Bef, St0) ->
    I = Le#l.i,
    {Sis,Int0} = adjust_stack(Bef, I, I+1, Vdb),
    {B,St1} = new_label(St0),
    {Mis,Int1,St2} = match_cg(M, St1#cg.ultimate_failure,
			      Int0, St1#cg{break=B}),
    %% Put return values in registers.
    Reg = load_vars(Rs, Int1#sr.reg),
    {Sis ++ Mis ++ [{label,B}],
     clear_dead(Int1#sr{reg=Reg}, I, Vdb),
     St2#cg{break=St1#cg.break}}.

guard_match_cg(M, Rs, Le, Vdb, Bef, St0) ->
    I = Le#l.i,
    {B,St1} = new_label(St0),
    Fail = case St0 of
               #cg{bfail=0,ultimate_failure=Fail0} -> Fail0;
               #cg{bfail=Fail0} -> Fail0
           end,
    {Mis,Aft,St2} = match_cg(M, Fail, Bef, St1#cg{break=B}),
    %% Update the register descriptors for the return registers.
    Reg = guard_match_regs(Aft#sr.reg, Rs),
    {Mis ++ [{label,B}],
     clear_dead(Aft#sr{reg=Reg}, I, Vdb),
     St2#cg{break=St1#cg.break}}.

guard_match_regs([{I,gbreakvar}|Rs], [#k_var{name=V}|Vs]) ->
    [{I,V}|guard_match_regs(Rs, Vs)];
guard_match_regs([R|Rs], Vs) ->
    [R|guard_match_regs(Rs, Vs)];
guard_match_regs([], []) -> [].
    

%% match_cg(Match, Fail, StackReg, State) -> {[Ainstr],StackReg,State}.
%%  Generate code for a match tree.  N.B. there is no need pass Vdb
%%  down as each level which uses this takes its own internal Vdb not
%%  the outer one.

match_cg(#k_alt{first=F,then=S}, Fail, Bef, St0) ->
    {Tf,St1} = new_label(St0),
    {Fis,Faft,St2} = match_cg(F, Tf, Bef, St1),
    {Sis,Saft,St3} = match_cg(S, Fail, Bef, St2),
    Aft = sr_merge(Faft, Saft),
    {Fis ++ [{label,Tf}] ++ Sis,Aft,St3};
match_cg(#k_select{var=#k_var{anno=Vanno,name=Vname}=V,types=Scs0}, Fail, Bef, St) ->
    ReuseForContext = member(reuse_for_context, Vanno) andalso
	find_reg(Vname, Bef#sr.reg) =/= error,
    Scs = case ReuseForContext of
	      false -> Scs0;
	      true -> bsm_rename_ctx(Scs0, Vname)
	  end,
    match_fmf(fun (S, F, Sta) ->
		      select_cg(S, V, F, Fail, Bef, Sta) end,
	      Fail, St, Scs);
match_cg(#k_guard{clauses=Gcs}, Fail, Bef, St) ->
    match_fmf(fun (G, F, Sta) -> guard_clause_cg(G, F, Bef, Sta) end,
	      Fail, St, Gcs);
match_cg(#cg_block{anno=Le,es=Es}, _Fail, Bef, St) ->
    %% Must clear registers and stack of dead variables.
    Int = clear_dead(Bef, Le#l.i, Le#l.vdb),
    block_cg(Es, Le, Int, St).

%% bsm_rename_ctx([Clause], Var) -> [Clause]
%%  We know from an annotation that the register for a binary can
%%  be reused for the match context because the two are not truly
%%  alive at the same time (even though the life time information
%%  says so).
%%
%%  The easiest way to have those variables share the same register is
%%  to rename the variable with the shortest life-span (the match
%%  context) to the variable for the binary (which can have a very
%%  long life-time because it is locked during matching). We KNOW that
%%  the match state variable will only be alive during the matching.
%%
%%  We must also remove all information about the match context
%%  variable from all life-time information databases (Vdb).

bsm_rename_ctx([#k_type_clause{type=k_binary,values=Vcs}=TC|Cs], New) ->
    [#k_val_clause{val=#k_binary{segs=#k_var{name=Old}}=Bin,
                   body=Ke0}=VC0] = Vcs,
    Ke = bsm_rename_ctx(Ke0, Old, New, false),
    VC = VC0#k_val_clause{val=Bin#k_binary{segs=#k_var{name=New}},
                          body=Ke},
    [TC#k_type_clause{values=[VC]}|bsm_rename_ctx(Cs, New)];
bsm_rename_ctx([C|Cs], New) ->
    [C|bsm_rename_ctx(Cs, New)];
bsm_rename_ctx([], _) -> [].

%% bsm_rename_ctx(Ke, OldName, NewName, InProt) -> Ke'
%%  Rename and clear OldName from life-time information. We must
%%  recurse into any block contained in a protected, but it would
%%  only complicatate things to recurse into blocks not in a protected
%%  (the match context variable is not live inside them).

bsm_rename_ctx(#k_select{var=#k_var{name=V},types=Cs0}=Sel,
               Old, New, InProt) ->
    Cs = bsm_rename_ctx_list(Cs0, Old, New, InProt),
    Sel#k_select{var=#k_var{name=bsm_rename_var(V, Old, New)},types=Cs};
bsm_rename_ctx(#k_type_clause{values=Cs0}=TC, Old, New, InProt) ->
    Cs = bsm_rename_ctx_list(Cs0, Old, New, InProt),
    TC#k_type_clause{values=Cs};
bsm_rename_ctx(#k_val_clause{body=Ke0}=VC, Old, New, InProt) ->
    Ke = bsm_rename_ctx(Ke0, Old, New, InProt),
    VC#k_val_clause{body=Ke};
bsm_rename_ctx(#k_alt{first=F0,then=S0}=Alt, Old, New, InProt) ->
    F = bsm_rename_ctx(F0, Old, New, InProt),
    S = bsm_rename_ctx(S0, Old, New, InProt),
    Alt#k_alt{first=F,then=S};
bsm_rename_ctx(#k_guard{clauses=Gcs0}=Guard, Old, New, InProt) ->
    Gcs = bsm_rename_ctx_list(Gcs0, Old, New, InProt),
    Guard#k_guard{clauses=Gcs};
bsm_rename_ctx(#k_guard_clause{guard=G0,body=B0}=GC, Old, New, InProt) ->
    G = bsm_rename_ctx(G0, Old, New, InProt),
    B = bsm_rename_ctx(B0, Old, New, InProt),
    %% A guard clause may cause unsaved variables to be saved on the stack.
    %% Since the match state variable Old is an alias for New (uses the
    %% same register), it is neither in the stack nor register descriptor
    %% lists and we would crash when we didn't find it unless we remove
    %% it from the database.
    bsm_forget_var(GC#k_guard_clause{guard=G,body=B}, Old);
bsm_rename_ctx(#k_protected{arg=Ts0}=Prot, Old, New, _InProt) ->
    InProt = true,
    Ts = bsm_rename_ctx_list(Ts0, Old, New, InProt),
    bsm_forget_var(Prot#k_protected{arg=Ts}, Old);
bsm_rename_ctx(#k_guard_match{body=Ms0}=Match, Old, New, InProt) ->
    Ms = bsm_rename_ctx(Ms0, Old, New, InProt),
    Match#k_guard_match{body=Ms};
bsm_rename_ctx(#k_test{}=Test, _, _, _) -> Test;
bsm_rename_ctx(#k_bif{}=Bif, _, _, _) -> Bif;
bsm_rename_ctx(#k_put{}=Put, _, _, _) -> Put;
bsm_rename_ctx(#k_call{}=Call, _, _, _) -> Call;
bsm_rename_ctx(#cg_block{}=Block, Old, _, false) ->
    %% This block is not inside a protected. The match context variable cannot
    %% possibly be live inside the block.
    bsm_forget_var(Block, Old);
bsm_rename_ctx(#cg_block{es=Es0}=Block, Old, New, true) ->
    %% A block in a protected. We must recursively rename the variable
    %% inside the block.
    Es = bsm_rename_ctx_list(Es0, Old, New, true),
    bsm_forget_var(Block#cg_block{es=Es}, Old);
bsm_rename_ctx(#k_guard_break{}=Break, Old, _New, _InProt) ->
    bsm_forget_var(Break, Old).

bsm_rename_ctx_list([C|Cs], Old, New, InProt) ->
    [bsm_rename_ctx(C, Old, New, InProt)|
     bsm_rename_ctx_list(Cs, Old, New, InProt)];
bsm_rename_ctx_list([], _, _, _) -> [].

bsm_rename_var(Old, Old, New) -> New;
bsm_rename_var(V, _, _) -> V.

%% bsm_forget_var(#l{}, Variable) -> #l{}
%%  Remove a variable from the variable life-time database.

bsm_forget_var(Ke, V) ->
    #l{vdb=Vdb} = L0 = get_kanno(Ke),
    L = L0#l{vdb=keydelete(V, 1, Vdb)},
    set_kanno(Ke, L).

%% block_cg([Kexpr], Le, Vdb, StackReg, St) -> {[Ainstr],StackReg,St}.
%% block_cg([Kexpr], Le, StackReg, St) -> {[Ainstr],StackReg,St}.

block_cg(Es, Le, _Vdb, Bef, St) ->
    block_cg(Es, Le, Bef, St).

block_cg(Es, Le, Bef, #cg{is_top_block=false}=St) ->
    cg_block(Es, Le#l.vdb, Bef, St);
block_cg(Es, Le, Bef, #cg{is_top_block=true}=St0) ->
    %% No stack frame has been established yet. Do we need one?
    case need_stackframe(Es) of
        true ->
            %% We need a stack frame. Generate the code and add the
            %% code for creating and deallocating the stack frame.
            {Is0,Aft,St} = cg_block(Es, Le#l.vdb, Bef,
                                    St0#cg{is_top_block=false,need_frame=false}),
            Is = top_level_block(Is0, Aft, max_reg(Bef#sr.reg), St),
            {Is,Aft,St#cg{is_top_block=true}};
        false ->
            %% This sequence of instructions ending in a #k_match{} (a
            %% 'case' or 'if') in the Erlang code does not need a
            %% stack frame yet. Delay the creation (if a stack frame
            %% is needed at all, it will be created inside the
            %% #k_match{}).
            cg_list(Es, Le#l.vdb, Bef, St0)
    end.

%% need_stackframe([Kexpr]) -> true|false.
%%  Does this list of instructions need a stack frame?
%%
%%  A sequence of instructions that don't clobber the X registers
%%  followed by a single #k_match{} doesn't need a stack frame.

need_stackframe([H|T]) ->
    case H of
        #k_bif{op=#k_internal{}} -> true;
        #k_put{arg=#k_binary{}} -> true;
        #k_bif{} -> need_stackframe(T);
        #k_put{} -> need_stackframe(T);
        #k_guard_match{} -> need_stackframe(T);
        #k_match{} when T =:= [] -> false;
        _ -> true
    end;
need_stackframe([]) -> false.

cg_block([], _Vdb, Bef, St0) ->
    {[],Bef,St0};
cg_block(Kes0, Vdb, Bef, St0) ->
    {Kes2,Int1,St1} =
	case basic_block(Kes0) of
	    {Kes1,LastI,Args,Rest} ->
		cg_basic_block(Kes1, LastI, Args, Vdb, Bef, St0);
	    {Kes1,Rest} ->
		cg_list(Kes1, Vdb, Bef, St0)
	end,
    {Kes3,Int2,St2} = cg_block(Rest, Vdb, Int1, St1),
    {Kes2 ++ Kes3,Int2,St2}.

basic_block(Kes) -> basic_block(Kes, []).

basic_block([Ke|Kes], Acc) ->
    case collect_block(Ke) of
	include -> basic_block(Kes, [Ke|Acc]);
	{block_end,As} ->
	    case Acc of
		[] ->
		    %% If the basic block does not contain any #k_put{} instructions,
		    %% it serves no useful purpose to do basic block optimizations.
		    {[Ke],Kes};
		_ ->
                    #l{i=I} = get_kanno(Ke),
		    {reverse(Acc, [Ke]),I,As,Kes}
	    end;
	no_block -> {reverse(Acc, [Ke]),Kes}
    end.

collect_block(#k_put{arg=Arg}) ->
    %% #k_put{} instructions that may garbage collect are not allowed
    %% in basic blocks.
    case Arg of
        #k_binary{} -> no_block;
        #k_map{} -> no_block;
        _ -> include
    end;
collect_block(#k_call{op=Func,args=As}) ->
    {block_end,As++func_vars(Func)};
collect_block(#k_enter{op=Func,args=As}) ->
    {block_end,As++func_vars(Func)};
collect_block(#k_return{args=Rs}) ->
    {block_end,Rs};
collect_block(#k_break{args=Bs}) ->
    {block_end,Bs};
collect_block(_) -> no_block.

func_vars(#k_var{}=Var) ->
    [Var];
func_vars(#k_remote{mod=M,name=F})
  when is_record(M, k_var); is_record(F, k_var) ->
    [M,F];
func_vars(_) -> [].

%% cg_basic_block([Kexpr], FirstI, LastI, Arguments, Vdb, StackReg, State) ->
%%      {[Ainstr],StackReg,State}.
%%
%%  Do a specialized code generation for a basic block of #put{}
%%  instructions (that don't do any garbage collection) followed by a
%%  call, break, or return.
%%
%%  'Arguments' is a list of the variables that must be loaded into
%%  consecutive X registers before the last instruction in the block.
%%  The point of this specialized code generation is to try put the
%%  all of the variables in 'Arguments' into the correct X register
%%  to begin with, instead of putting them into the first available
%%  X register and having to move them to the correct X register
%%  later.
%%
%%  To achieve that, we attempt to reserve the X registers that the
%%  variables in 'Arguments' will need to be in when the block ends.
%%
%%  To make it more likely that reservations will be successful, we
%%  will try to save variables that need to be saved to the stack as
%%  early as possible (if an X register needed by a variable in
%%  Arguments is occupied by another variable, the value in the
%%  X register can be evicted if it is saved on the stack).
%%
%%  We will take care not to increase the size of stack frame compared
%%  to what the standard code generator would have done (that is, to
%%  save all X registers at the last possible moment). We will do that
%%  by extending the stack frame to the minimal size needed to save
%%  all that needs to be saved using extend_stack/4, and use
%%  save_carefully/4 during code generation to only save the variables
%%  that can be saved without growing the stack frame.

cg_basic_block(Kes, Lf, As, Vdb, Bef, St0) ->
    Int0 = reserve_arg_regs(As, Bef),
    Int = extend_stack(Int0, Lf, Lf+1, Vdb),
    {Keis,{Aft,St1}} =
	flatmapfoldl(fun(Ke, St) -> cg_basic_block(Ke, St, Lf, Vdb) end,
		     {Int,St0}, need_heap(Kes)),
    {Keis,Aft,St1}.

cg_basic_block(#cg_need_heap{}=Ke, {Bef,St0}, _Lf, Vdb) ->
    {Keis,Aft,St1} = cg(Ke, Vdb, Bef, St0),
    {Keis,{Aft,St1}};
cg_basic_block(Ke, {Bef,St0}, Lf, Vdb) ->
    #l{i=I} = get_kanno(Ke),

    %% Save all we can to increase the possibility that reserving
    %% registers will succeed.
    {Sis,Int0} = save_carefully(Bef, I, Lf+1, Vdb),
    Int1 = reserve(Int0),
    {Keis,Aft,St1} = cg(Ke, Vdb, Int1, St0),
    {Sis ++ Keis,{Aft,St1}}.

%% reserve_arg_regs([Argument], Bef) -> Aft.
%%  Try to reserve the X registers for all arguments. All registers
%%  that we wish to reserve will be saved in Bef#sr.res.

reserve_arg_regs(As, Bef) ->
    Res = reserve_arg_regs_1(As, 0),
    reserve(Bef#sr{res=Res}).

reserve_arg_regs_1([#k_var{name=V}|As], I) ->
    [{I,V}|reserve_arg_regs_1(As, I+1)];
reserve_arg_regs_1([A|As], I) ->
    [{I,A}|reserve_arg_regs_1(As, I+1)];
reserve_arg_regs_1([], _) -> [].

%% reserve(Bef) -> Aft.
%%  Try to reserve more registers. The registers we wish to reserve
%%  are found in Bef#sr.res.

reserve(#sr{reg=Regs,stk=Stk,res=Res}=Sr) ->
    Sr#sr{reg=reserve_1(Res, Regs, Stk)}.

reserve_1([{I,V}|Rs], [free|Regs], Stk) ->
    [{reserved,I,V}|reserve_1(Rs, Regs, Stk)];
reserve_1([{I,V}|Rs], [{I,V}|Regs], Stk) ->
    [{I,V}|reserve_1(Rs, Regs, Stk)];
reserve_1([{I,V}|Rs], [{I,Var}|Regs], Stk) ->
    case on_stack(Var, Stk) of
	true -> [{reserved,I,V}|reserve_1(Rs, Regs, Stk)];
	false -> [{I,Var}|reserve_1(Rs, Regs, Stk)]
    end;
reserve_1([{I,V}|Rs], [{reserved,I,_}|Regs], Stk) ->
    [{reserved,I,V}|reserve_1(Rs, Regs, Stk)];
reserve_1([{I,V}|Rs], [], Stk) ->
    [{reserved,I,V}|reserve_1(Rs, [], Stk)];
reserve_1([], Regs, _) -> Regs.

%% extend_stack(Bef, FirstBefore, LastFrom, Vdb) -> Aft.
%%  Extend the stack enough to fit all variables alive past LastFrom
%%  and not already on the stack.

extend_stack(#sr{stk=Stk0}=Bef, Fb, Lf, Vdb) ->
    Stk1 = clear_dead_stk(Stk0, Fb, Vdb),
    New = new_not_on_stack(Stk1, Fb, Lf, Vdb),
    Stk2 = foldl(fun ({V,_,_}, Stk) -> put_stack(V, Stk) end, Stk1, New),
    Stk = Stk0 ++ lists:duplicate(length(Stk2) - length(Stk0), free),
    Bef#sr{stk=Stk}.

%% save_carefully(Bef, FirstBefore, LastFrom, Vdb) -> {[SaveVar],Aft}.
%%  Save variables which are used past current point and which are not
%%  already on the stack, but only if the variables can be saved without
%%  growing the stack frame.

save_carefully(#sr{stk=Stk}=Bef, Fb, Lf, Vdb) ->
    New0 = new_not_on_stack(Stk, Fb, Lf, Vdb),
    New = keysort(2, New0),
    save_carefully_1(New, Bef, []).

save_carefully_1([{V,_,_}|Vs], #sr{reg=Regs,stk=Stk0}=Bef, Acc) ->
    case put_stack_carefully(V, Stk0) of
	error ->
            {reverse(Acc),Bef};
	Stk1 ->
	    SrcReg = fetch_reg(V, Regs),
	    Move = {move,SrcReg,fetch_stack(V, Stk1)},
	    {x,_} = SrcReg,			%Assertion - must be X register.
	    save_carefully_1(Vs, Bef#sr{stk=Stk1}, [Move|Acc])
    end;
save_carefully_1([], Bef, Acc) ->
    {reverse(Acc),Bef}.

%% top_level_block([Instruction], Bef, MaxRegs, St) -> [Instruction].
%%  For the top-level block, allocate a stack frame a necessary,
%%  adjust Y register numbering and instructions that return
%%  from the function.

top_level_block(Keis, #sr{stk=[]}, _MaxRegs, #cg{need_frame=false}) ->
    Keis;
top_level_block(Keis, Bef, MaxRegs, _St) ->
    %% This top block needs an allocate instruction before it, and a
    %% deallocate instruction before each return.
    FrameSz = length(Bef#sr.stk),
    MaxY = FrameSz-1,
    Keis1 = flatmap(fun ({call_only,Arity,Func}) ->
			    [{call_last,Arity,Func,FrameSz}];
			({call_ext_only,Arity,Func}) ->
			    [{call_ext_last,Arity,Func,FrameSz}];
			({apply_only,Arity}) ->
			    [{apply_last,Arity,FrameSz}];
			(return) ->
			    [{deallocate,FrameSz},return];
			(Tuple) when is_tuple(Tuple) ->
			    [turn_yregs(Tuple, MaxY)];
			(Other) ->
			    [Other]
		    end, Keis),
    [{allocate_zero,FrameSz,MaxRegs}|Keis1].

%% turn_yregs(Size, Tuple, MaxY) -> Tuple'
%%   Renumber y register so that {y,0} becomes {y,FrameSize-1},
%%   {y,FrameSize-1} becomes {y,0} and so on.  This is to make nested
%%   catches work.  The code generation algorithm gives a lower register
%%   number to the outer catch, which is wrong.

turn_yregs({call,_,_}=I, _MaxY) -> I;
turn_yregs({call_ext,_,_}=I, _MaxY) -> I;
turn_yregs({jump,_}=I, _MaxY) -> I;
turn_yregs({label,_}=I, _MaxY) -> I;
turn_yregs({line,_}=I, _MaxY) -> I;
turn_yregs({test_heap,_,_}=I, _MaxY) -> I;
turn_yregs({bif,Op,F,A,B}, MaxY) ->
    {bif,Op,F,turn_yreg(A, MaxY),turn_yreg(B, MaxY)};
turn_yregs({gc_bif,Op,F,Live,A,B}, MaxY) when is_integer(Live) ->
    {gc_bif,Op,F,Live,turn_yreg(A, MaxY),turn_yreg(B, MaxY)};
turn_yregs({get_tuple_element,S,N,D}, MaxY) ->
    {get_tuple_element,turn_yreg(S, MaxY),N,turn_yreg(D, MaxY)};
turn_yregs({put_tuple,Arity,D}, MaxY) ->
    {put_tuple,Arity,turn_yreg(D, MaxY)};
turn_yregs({select_val,R,F,L}, MaxY) ->
    {select_val,turn_yreg(R, MaxY),F,L};
turn_yregs({test,Op,F,L}, MaxY) ->
    {test,Op,F,turn_yreg(L, MaxY)};
turn_yregs({test,Op,F,Live,A,B}, MaxY) when is_integer(Live) ->
    {test,Op,F,Live,turn_yreg(A, MaxY),turn_yreg(B, MaxY)};
turn_yregs({Op,A}, MaxY) ->
    {Op,turn_yreg(A, MaxY)};
turn_yregs({Op,A,B}, MaxY) ->
    {Op,turn_yreg(A, MaxY),turn_yreg(B, MaxY)};
turn_yregs({Op,A,B,C}, MaxY) ->
    {Op,turn_yreg(A, MaxY),turn_yreg(B, MaxY),turn_yreg(C, MaxY)};
turn_yregs(Tuple, MaxY) ->
    turn_yregs(tuple_size(Tuple), Tuple, MaxY).

turn_yregs(1, Tp, _) ->
    Tp;
turn_yregs(N, Tp, MaxY) ->
    E = turn_yreg(element(N, Tp), MaxY),
    turn_yregs(N-1, setelement(N, Tp, E), MaxY).

turn_yreg({yy,YY}, MaxY) ->
    {y,MaxY-YY};
turn_yreg({list,Ls},MaxY) ->
    {list,turn_yreg(Ls, MaxY)};
turn_yreg([_|_]=Ts, MaxY) ->
    [turn_yreg(T, MaxY) || T <- Ts];
turn_yreg(Other, _MaxY) ->
    Other.

%% select_cg(Sclause, V, TypeFail, ValueFail, StackReg, State) ->
%%      {Is,StackReg,State}.
%%  Selecting type and value needs two failure labels, TypeFail is the
%%  label to jump to of the next type test when this type fails, and
%%  ValueFail is the label when this type is correct but the value is
%%  wrong.  These are different as in the second case there is no need
%%  to try the next type, it will always fail.

select_cg(#k_type_clause{type=Type,values=Vs}, Var, Tf, Vf, Bef, St) ->
    #k_var{name=V} = Var,
    select_cg(Type, Vs, V, Tf, Vf, Bef, St).

select_cg(k_cons, [S], V, Tf, Vf, Bef, St) ->
    select_cons(S, V, Tf, Vf, Bef, St);
select_cg(k_nil, [S], V, Tf, Vf, Bef, St) ->
    select_nil(S, V, Tf, Vf, Bef, St);
select_cg(k_binary, [S], V, Tf, Vf, Bef, St) ->
    select_binary(S, V, Tf, Vf, Bef, St);
select_cg(k_bin_seg, S, V, Tf, _Vf, Bef, St) ->
    select_bin_segs(S, V, Tf, Bef, St);
select_cg(k_bin_int, S, V, Tf, _Vf, Bef, St) ->
    select_bin_segs(S, V, Tf, Bef, St);
select_cg(k_bin_end, [S], V, Tf, _Vf, Bef, St) ->
    select_bin_end(S, V, Tf, Bef, St);
select_cg(k_map, S, V, Tf, Vf, Bef, St) ->
    select_map(S, V, Tf, Vf, Bef, St);
select_cg(k_literal, S, V, Tf, Vf, Bef, St) ->
    select_literal(S, V, Tf, Vf, Bef, St);
select_cg(Type, Scs, V, Tf, Vf, Bef, St0) ->
    {Vis,{Aft,St1}} =
	mapfoldl(fun (S, {Int,Sta}) ->
			 {Val,Is,Inta,Stb} = select_val(S, V, Vf, Bef, Sta),
			 {{Is,[Val]},{sr_merge(Int, Inta),Stb}}
		 end, {void,St0}, Scs),
    OptVls = combine(lists:sort(combine(Vis))),
    {Vls,Sis,St2} = select_labels(OptVls, St1, [], []),
    {select_val_cg(Type, fetch_var(V, Bef), Vls, Tf, Vf, Sis), Aft, St2}.

select_val_cg(k_tuple, R, [Arity,{f,Lbl}], Tf, Vf, [{label,Lbl}|Sis]) ->
    [{test,is_tuple,{f,Tf},[R]},{test,test_arity,{f,Vf},[R,Arity]}|Sis];
select_val_cg(k_tuple, R, Vls, Tf, Vf, Sis) ->
    [{test,is_tuple,{f,Tf},[R]},{select_tuple_arity,R,{f,Vf},{list,Vls}}|Sis];
select_val_cg(Type, R, [Val, {f,Lbl}], Fail, Fail, [{label,Lbl}|Sis]) ->
    [{test,is_eq_exact,{f,Fail},[R,{type(Type),Val}]}|Sis];
select_val_cg(Type, R, [Val, {f,Lbl}], Tf, Vf, [{label,Lbl}|Sis]) ->
    [{test,select_type_test(Type),{f,Tf},[R]},
     {test,is_eq_exact,{f,Vf},[R,{type(Type),Val}]}|Sis];
select_val_cg(Type, R, Vls0, Tf, Vf, Sis) ->
    Vls1 = [case Value of
                {f,_Lbl} -> Value;
                _ -> {type(Type),Value}
            end || Value <- Vls0],
    [{test,select_type_test(Type),{f,Tf},[R]}, {select_val,R,{f,Vf},{list,Vls1}}|Sis].

type(k_atom) -> atom;
type(k_float) -> float;
type(k_int) -> integer.

select_type_test(k_int) -> is_integer;
select_type_test(k_atom) -> is_atom;
select_type_test(k_float) -> is_float.

combine([{Is,Vs1}, {Is,Vs2}|Vis]) -> combine([{Is,Vs1 ++ Vs2}|Vis]);
combine([V|Vis]) -> [V|combine(Vis)];
combine([]) -> [].

select_labels([{Is,Vs}|Vis], St0, Vls, Sis) ->
    {Lbl,St1} = new_label(St0),
    select_labels(Vis, St1, add_vls(Vs, Lbl, Vls), [[{label,Lbl}|Is]|Sis]);
select_labels([], St, Vls, Sis) ->
    {Vls,append(Sis),St}.

add_vls([V|Vs], Lbl, Acc) ->
    add_vls(Vs, Lbl, [V, {f,Lbl}|Acc]);
add_vls([], _, Acc) -> Acc.

select_literal(S, V, Tf, Vf, Bef, St) ->
    Reg = fetch_var(V, Bef),
    F = fun(ValClause, Fail, St0) ->
                {Val,Is,Aft,St1} = select_val(ValClause, V, Vf, Bef, St0),
                Test = {test,is_eq_exact,{f,Fail},[Reg,{literal,Val}]},
                {[Test|Is],Aft,St1}
        end,
    match_fmf(F, Tf, St, S).

select_cons(#k_val_clause{val=#k_cons{hd=Hd,tl=Tl},body=B,anno=#l{i=I,vdb=Vdb}},
            V, Tf, Vf, Bef, St0) ->
    Es = [Hd,Tl],
    {Eis,Int,St1} = select_extract_cons(V, Es, I, Vdb, Bef, St0),
    {Bis,Aft,St2} = match_cg(B, Vf, Int, St1),
    {[{test,is_nonempty_list,{f,Tf},[fetch_var(V, Bef)]}] ++ Eis ++ Bis,Aft,St2}.

select_nil(#k_val_clause{val=#k_nil{},body=B}, V, Tf, Vf, Bef, St0) ->
    {Bis,Aft,St1} = match_cg(B, Vf, Bef, St0),
    {[{test,is_nil,{f,Tf},[fetch_var(V, Bef)]}] ++ Bis,Aft,St1}.

select_binary(#k_val_clause{val=#k_binary{segs=#k_var{name=V}},body=B,
                            anno=#l{i=I,vdb=Vdb}}, V, Tf, Vf, Bef, St0) ->
    #cg{ctx=OldCtx} = St0,
    Int0 = clear_dead(Bef#sr{reg=Bef#sr.reg}, I, Vdb),
    {Bis0,Aft,St1} = match_cg(B, Vf, Int0, St0#cg{ctx=V}),
    CtxReg = fetch_var(V, Int0),
    Live = max_reg(Bef#sr.reg),
    Bis1 = [{test,bs_start_match2,{f,Tf},Live,[CtxReg,{context,V}],CtxReg},
	    {bs_save2,CtxReg,{V,V}}|Bis0],
    Bis = finish_select_binary(Bis1),
    {Bis,Aft,St1#cg{ctx=OldCtx}};
select_binary(#k_val_clause{val=#k_binary{segs=#k_var{name=Ivar}},body=B,
                            anno=#l{i=I,vdb=Vdb}}, V, Tf, Vf, Bef, St0) ->
    #cg{ctx=OldCtx} = St0,
    Regs = put_reg(Ivar, Bef#sr.reg),
    Int0 = clear_dead(Bef#sr{reg=Regs}, I, Vdb),
    {Bis0,Aft,St1} = match_cg(B, Vf, Int0, St0#cg{ctx=Ivar}),
    CtxReg = fetch_var(Ivar, Int0),
    Live = max_reg(Bef#sr.reg),
    Bis1 = [{test,bs_start_match2,{f,Tf},Live,
             [fetch_var(V, Bef),{context,Ivar}],CtxReg},
	    {bs_save2,CtxReg,{Ivar,Ivar}}|Bis0],
    Bis = finish_select_binary(Bis1),
    {Bis,Aft,St1#cg{ctx=OldCtx}}.

finish_select_binary([{bs_save2,R,Point}=I,{bs_restore2,R,Point}|Is]) ->
    [I|finish_select_binary(Is)];
finish_select_binary([{bs_save2,R,Point}=I,{test,is_eq_exact,_,_}=Test,
		      {bs_restore2,R,Point}|Is]) ->
    [I,Test|finish_select_binary(Is)];
finish_select_binary([{test,bs_match_string,F,[Ctx,BinList]}|Is])
  when is_list(BinList) ->
    I = {test,bs_match_string,F,[Ctx,list_to_bitstring(BinList)]},
    [I|finish_select_binary(Is)];
finish_select_binary([I|Is]) ->
    [I|finish_select_binary(Is)];
finish_select_binary([]) -> [].

%% New instructions for selection of binary segments.

select_bin_segs(Scs, Ivar, Tf, Bef, St) ->
    match_fmf(fun(S, Fail, Sta) ->
		      select_bin_seg(S, Ivar, Fail, Bef, Sta) end,
	      Tf, St, Scs).

select_bin_seg(#k_val_clause{val=#k_bin_seg{size=Size,unit=U,type=T,
                                            seg=Seg,flags=Fs0,next=Next},
                             body=B,
                             anno=#l{i=I,vdb=Vdb,a=A}}, Ivar, Fail, Bef, St0) ->
    Ctx = St0#cg.ctx,
    Fs = [{anno,A}|Fs0],
    Es = case Next of
             [] -> [Seg];
             _ -> [Seg,Next]
         end,
    {Mis,Int,St1} = select_extract_bin(Es, Size, U, T, Fs, Fail,
				       I, Vdb, Bef, Ctx, B, St0),
    {Bis,Aft,St2} = match_cg(B, Fail, Int, St1),
    CtxReg = fetch_var(Ctx, Bef),
    Is = if
	     Mis =:= [] ->
		 %% No bs_restore2 instruction needed if no match instructions.
		 Bis;
	     true ->
		 [{bs_restore2,CtxReg,{Ctx,Ivar}}|Mis++Bis]
	 end,
    {Is,Aft,St2};
select_bin_seg(#k_val_clause{val=#k_bin_int{size=Sz,unit=U,flags=Fs,
                                            val=Val,next=Next},
                             body=B,
                             anno=#l{i=I,vdb=Vdb}}, Ivar, Fail, Bef, St0) ->
    Ctx = St0#cg.ctx,
    {Mis,Int,St1} = select_extract_int(Next, Val, Sz, U, Fs, Fail,
				       I, Vdb, Bef, Ctx, St0),
    {Bis,Aft,St2} = match_cg(B, Fail, Int, St1),
    CtxReg = fetch_var(Ctx, Bef),
    Is = case Mis ++ Bis of
	     [{test,bs_match_string,F,[OtherCtx,Bin1]},
	      {bs_save2,OtherCtx,_},
	      {bs_restore2,OtherCtx,_},
	      {test,bs_match_string,F,[OtherCtx,Bin2]}|Is0] ->
		 %% We used to do this optimization later, but it
		 %% turns out that in huge functions with many
		 %% bs_match_string instructions, it's a big win
		 %% to do the combination now. To avoid copying the
		 %% binary data again and again, we'll combine bitstrings
		 %% in a list and convert all of it to a bitstring later.
		 [{test,bs_match_string,F,[OtherCtx,[Bin1,Bin2]]}|Is0];
	     Is0 ->
		 Is0
	 end,
    {[{bs_restore2,CtxReg,{Ctx,Ivar}}|Is],Aft,St2}.

select_extract_int(#k_var{name=Tl}, Val, #k_int{val=Sz}, U, Fs, Vf,
		   I, Vdb, Bef, Ctx, St) ->
    Bits = U*Sz,
    Bin = case member(big, Fs) of
	      true ->
		  <<Val:Bits>>;
	      false ->
		  true = member(little, Fs),	%Assertion.
		  <<Val:Bits/little>>
	  end,
    Bits = bit_size(Bin),			%Assertion.
    CtxReg = fetch_var(Ctx, Bef),
    Is = if
	     Bits =:= 0 ->
		 [{bs_save2,CtxReg,{Ctx,Tl}}];
	     true ->
		 [{test,bs_match_string,{f,Vf},[CtxReg,Bin]},
		  {bs_save2,CtxReg,{Ctx,Tl}}]
	 end,
    {Is,clear_dead(Bef, I, Vdb),St}.

select_extract_bin([#k_var{name=Hd},#k_var{name=Tl}], Size0, Unit, Type, Flags, Vf,
		   I, Vdb, Bef, Ctx, _Body, St) ->
    SizeReg = get_bin_size_reg(Size0, Bef),
    {Es,Aft} =
	case vdb_find(Hd, Vdb) of
	    {_,_,Lhd} when Lhd =< I ->
		%% The extracted value will not be used.
		CtxReg = fetch_var(Ctx, Bef),
		Live = max_reg(Bef#sr.reg),
		Skip = build_skip_instr(Type, Vf, CtxReg, Live,
					SizeReg, Unit, Flags),
		{[Skip,{bs_save2,CtxReg,{Ctx,Tl}}],Bef};
	    {_,_,_} ->
		Reg = put_reg(Hd, Bef#sr.reg),
		Int1 = Bef#sr{reg=Reg},
		Rhd = fetch_reg(Hd, Reg),
		CtxReg = fetch_reg(Ctx, Reg),
		Live = max_reg(Bef#sr.reg),
		{[build_bs_instr(Type, Vf, CtxReg, Live, SizeReg,
				 Unit, Flags, Rhd),
		  {bs_save2,CtxReg,{Ctx,Tl}}],Int1}
	end,
    {Es,clear_dead(Aft, I, Vdb),St};
select_extract_bin([#k_var{name=Hd}], Size, Unit, binary, Flags, Vf,
		   I, Vdb, Bef, Ctx, Body, St) ->
    %% Match the last segment of a binary. We KNOW that the size
    %% must be 'all'.
    #k_atom{val=all} = Size,                    %Assertion.
    {Es,Aft} =
	case vdb_find(Hd, Vdb) of
	    {_,_,Lhd} when Lhd =< I ->
		%% The result will not be used. Furthermore, since we
		%% we are at the end of the binary, the position will
		%% not be used again; thus, it is safe to do a cheaper
		%% test of the unit.
		CtxReg = fetch_var(Ctx, Bef),
		{case Unit of
		     1 ->
			 [];
		     _ ->
			 [{test,bs_test_unit,{f,Vf},[CtxReg,Unit]}]
		 end,Bef};
	    {_,_,_} ->
		case is_context_unused(Body) of
		    false ->
			Reg = put_reg(Hd, Bef#sr.reg),
			Int1 = Bef#sr{reg=Reg},
			Rhd = fetch_reg(Hd, Reg),
			CtxReg = fetch_reg(Ctx, Reg),
			Name = bs_get_binary2,
			Live = max_reg(Bef#sr.reg),
			{[{test,Name,{f,Vf},Live,
			   [CtxReg,atomic(Size),Unit,{field_flags,Flags}],Rhd}],
			 Int1};
		    true ->
			%% Since the matching context will not be used again,
			%% we can reuse its register. Reusing the register
			%% opens some interesting optimizations in the
			%% run-time system.

			Reg0 = Bef#sr.reg,
			CtxReg = fetch_reg(Ctx, Reg0),
			Reg = replace_reg_contents(Ctx, Hd, Reg0),
			Int1 = Bef#sr{reg=Reg},
			Name = bs_get_binary2,
			Live = max_reg(Int1#sr.reg),
			{[{test,Name,{f,Vf},Live,
			   [CtxReg,atomic(Size),Unit,{field_flags,Flags}],CtxReg}],
			 Int1}
		end
	end,
    {Es,clear_dead(Aft, I, Vdb),St}.

%% is_context_unused(Ke) -> true | false
%%   Simple heurististic to determine whether the code that follows
%%   will use the current matching context again. (The liveness
%%   information is too conservative to be useful for this purpose.)
%%   'true' means that the code that follows will definitely not use
%%   the context again (because it is a block, not guard or matching
%%   code); 'false' that we are not sure (there could be more
%%   matching).

is_context_unused(#k_alt{then=Then}) ->
    %% #k_alt{} can be used for different purposes. If the Then part
    %% is a block, it means that matching has finished and is used for a guard
    %% to choose between the matched clauses.
    is_context_unused(Then);
is_context_unused(#cg_block{}) ->
    true;
is_context_unused(_) ->
    false.

select_bin_end(#k_val_clause{val=#k_bin_end{},body=B}, Ivar, Tf, Bef, St0) ->
    Ctx = St0#cg.ctx,
    {Bis,Aft,St2} = match_cg(B, Tf, Bef, St0),
    CtxReg = fetch_var(Ctx, Bef),
    {[{bs_restore2,CtxReg,{Ctx,Ivar}},
      {test,bs_test_tail2,{f,Tf},[CtxReg,0]}|Bis],Aft,St2}.

get_bin_size_reg(#k_var{name=V}, Bef) ->
    fetch_var(V, Bef);
get_bin_size_reg(Literal, _Bef) ->
    atomic(Literal).

build_bs_instr(Type, Vf, CtxReg, Live, SizeReg, Unit, Flags, Rhd) ->
    {Format,Name} = case Type of
			integer -> {plain,bs_get_integer2};
			float ->   {plain,bs_get_float2};
			binary ->  {plain,bs_get_binary2};
			utf8 ->    {utf,bs_get_utf8};
			utf16 ->   {utf,bs_get_utf16};
			utf32 ->   {utf,bs_get_utf32}
		   end,
    case Format of
	plain ->
	    {test,Name,{f,Vf},Live,
	     [CtxReg,SizeReg,Unit,{field_flags,Flags}],Rhd};
	utf ->
	    {test,Name,{f,Vf},Live,
	     [CtxReg,{field_flags,Flags}],Rhd}
    end.

build_skip_instr(Type, Vf, CtxReg, Live, SizeReg, Unit, Flags) ->
    {Format,Name} = case Type of
			utf8 -> {utf,bs_skip_utf8};
			utf16 -> {utf,bs_skip_utf16};
			utf32 -> {utf,bs_skip_utf32};
			_ -> {plain,bs_skip_bits2}
		    end,
    case Format of
	plain ->
	    {test,Name,{f,Vf},[CtxReg,SizeReg,Unit,{field_flags,Flags}]};
	utf ->
	    {test,Name,{f,Vf},[CtxReg,Live,{field_flags,Flags}]}
    end.

select_val(#k_val_clause{val=#k_tuple{es=Es},body=B,anno=#l{i=I,vdb=Vdb}},
           V, Vf, Bef, St0) ->
    {Eis,Int,St1} = select_extract_tuple(V, Es, I, Vdb, Bef, St0),
    {Bis,Aft,St2} = match_cg(B, Vf, Int, St1),
    {length(Es),Eis ++ Bis,Aft,St2};
select_val(#k_val_clause{val=Val0,body=B}, _V, Vf, Bef, St0) ->
    Val = case Val0 of
              #k_atom{val=Lit} -> Lit;
              #k_float{val=Lit} -> Lit;
              #k_int{val=Lit} -> Lit;
              #k_literal{val=Lit} -> Lit
          end,
    {Bis,Aft,St1} = match_cg(B, Vf, Bef, St0),
    {Val,Bis,Aft,St1}.

%% select_extract_tuple(Src, [V], I, Vdb, StackReg, State) ->
%%      {[E],StackReg,State}.
%%  Extract tuple elements, but only if they do not immediately die.

select_extract_tuple(Src, Vs, I, Vdb, Bef, St) ->
    F = fun (#k_var{name=V}, {Int0,Elem}) ->
		case vdb_find(V, Vdb) of
		    {V,_,L} when L =< I -> {[], {Int0,Elem+1}};
		    _Other ->
			Reg1 = put_reg(V, Int0#sr.reg),
			Int1 = Int0#sr{reg=Reg1},
			Rsrc = fetch_var(Src, Int1),
			{[{get_tuple_element,Rsrc,Elem,fetch_reg(V, Reg1)}],
			 {Int1,Elem+1}}
		end
	end,
    {Es,{Aft,_}} = flatmapfoldl(F, {Bef,0}, Vs),
    {Es,Aft,St}.

select_map(Scs, V, Tf, Vf, Bef, St0) ->
    Reg = fetch_var(V, Bef),
    {Is,Aft,St1} =
	match_fmf(fun(#k_val_clause{val=#k_map{op=exact,es=Es},
                                    body=B,anno=#l{i=I,vdb=Vdb}}, Fail, St1) ->
                          select_map_val(V, Es, B, Fail, I, Vdb, Bef, St1)
                  end, Vf, St0, Scs),
    {[{test,is_map,{f,Tf},[Reg]}|Is],Aft,St1}.

select_map_val(V, Es, B, Fail, I, Vdb, Bef, St0) ->
    {Eis,Int,St1} = select_extract_map(V, Es, Fail, I, Vdb, Bef, St0),
    {Bis,Aft,St2} = match_cg(B, Fail, Int, St1),
    {Eis++Bis,Aft,St2}.

select_extract_map(_, [], _, _, _, Bef, St) -> {[],Bef,St};
select_extract_map(Src, Vs, Fail, I, Vdb, Bef, St) ->
    %% First split the instruction flow
    %% We want one set of each
    %% 1) has_map_fields (no target registers)
    %% 2) get_map_elements (with target registers)
    %% Assume keys are term-sorted
    Rsrc = fetch_var(Src, Bef),

    {{HasKs,GetVs,HasVarKs,GetVarVs},Aft} =
        foldr(fun(#k_map_pair{key=#k_var{name=K},val=#k_var{name=V}},
                  {{HasKsi,GetVsi,HasVarVsi,GetVarVsi},Int0}) ->
                      case vdb_find(V, Vdb) of
                          {V,_,L} when L =< I ->
                              RK = fetch_var(K,Int0),
                              {{HasKsi,GetVsi,[RK|HasVarVsi],GetVarVsi},Int0};
                          _Other ->
                              Reg1 = put_reg(V, Int0#sr.reg),
                              Int1 = Int0#sr{reg=Reg1},
                              RK = fetch_var(K,Int0),
                              RV = fetch_reg(V,Reg1),
                              {{HasKsi,GetVsi,HasVarVsi,[[RK,RV]|GetVarVsi]},Int1}
                      end;
                 (#k_map_pair{key=Key,val=#k_var{name=V}},
                  {{HasKsi,GetVsi,HasVarVsi,GetVarVsi},Int0}) ->
                      case vdb_find(V, Vdb) of
                          {V,_,L} when L =< I ->
                              {{[atomic(Key)|HasKsi],GetVsi,HasVarVsi,GetVarVsi},Int0};
                          _Other ->
                              Reg1 = put_reg(V, Int0#sr.reg),
                              Int1 = Int0#sr{reg=Reg1},
                              {{HasKsi,[atomic(Key),fetch_reg(V, Reg1)|GetVsi],
                                HasVarVsi,GetVarVsi},Int1}
                      end
              end, {{[],[],[],[]},Bef}, Vs),

    Code = [{test,has_map_fields,{f,Fail},Rsrc,{list,HasKs}} || HasKs =/= []] ++
	   [{test,has_map_fields,{f,Fail},Rsrc,{list,[K]}}   || K <- HasVarKs] ++
	   [{get_map_elements,   {f,Fail},Rsrc,{list,GetVs}} || GetVs =/= []] ++
	   [{get_map_elements,   {f,Fail},Rsrc,{list,[K,V]}} || [K,V] <- GetVarVs],
    {Code, Aft, St}.


select_extract_cons(Src, [#k_var{name=Hd},#k_var{name=Tl}], I, Vdb, Bef, St) ->
    Rsrc = fetch_var(Src, Bef),
    Int = clear_dead(Bef, I, Vdb),
    {{_,_,Lhd},{_,_,Ltl}} = {vdb_find(Hd, Vdb),vdb_find(Tl, Vdb)},
    case {Lhd =< I, Ltl =< I} of
        {true,true} ->
            %% Both dead.
            {[],Bef,St};
        {true,false} ->
            %% Head dead.
            Reg0 = put_reg(Tl, Bef#sr.reg),
            Aft = Int#sr{reg=Reg0},
            Rtl = fetch_reg(Tl, Reg0),
            {[{get_tl,Rsrc,Rtl}],Aft,St};
        {false,true} ->
            %% Tail dead.
            Reg0 = put_reg(Hd, Bef#sr.reg),
            Aft = Int#sr{reg=Reg0},
            Rhd = fetch_reg(Hd, Reg0),
            {[{get_hd,Rsrc,Rhd}],Aft,St};
        {false,false} ->
            %% Both used.
            Reg0 = put_reg(Tl, put_reg(Hd, Bef#sr.reg)),
            Aft = Bef#sr{reg=Reg0},
            Rhd = fetch_reg(Hd, Reg0),
            Rtl = fetch_reg(Tl, Reg0),
            {[{get_hd,Rsrc,Rhd},{get_tl,Rsrc,Rtl}],Aft,St}
    end.

guard_clause_cg(#k_guard_clause{anno=#l{vdb=Vdb},guard=G,body=B}, Fail, Bef, St0) ->
    {Gis,Int,St1} = guard_cg(G, Fail, Vdb, Bef, St0),
    {Bis,Aft,St} = match_cg(B, Fail, Int, St1),
    {Gis ++ Bis,Aft,St}.

%% guard_cg(Guard, Fail, Vdb, StackReg, State) ->
%%      {[Ainstr],StackReg,State}.
%%  A guard is a boolean expression of tests.  Tests return true or
%%  false.  A fault in a test causes the test to return false.  Tests
%%  never return the boolean, instead we generate jump code to go to
%%  the correct exit point.  Primops and tests all go to the next
%%  instruction on success or jump to a failure label.

guard_cg(#k_protected{arg=Ts,ret=Rs,anno=#l{vdb=Pdb}}, Fail, _Vdb, Bef, St) ->
    protected_cg(Ts, Rs, Fail, Pdb, Bef, St);
guard_cg(#k_test{anno=#l{i=I},op=Test0,args=As,inverted=Inverted},
         Fail, Vdb, Bef, St0) ->
    #k_remote{mod=#k_atom{val=erlang},name=#k_atom{val=Test}} = Test0,
    case Inverted of
	false ->
	    test_cg(Test, As, Fail, I, Vdb, Bef, St0);
	true ->
	    {Psucc,St1} = new_label(St0),
	    {Is,Aft,St2} = test_cg(Test, As, Psucc, I, Vdb, Bef, St1),
	    {Is++[{jump,{f,Fail}},{label,Psucc}],Aft,St2}
    end;
guard_cg(G, _Fail, Vdb, Bef, St) ->
    %%ok = io:fwrite("cg ~w: ~p~n", [?LINE,{G,Fail,Vdb,Bef}]),
    {Gis,Aft,St1} = cg(G, Vdb, Bef, St),
    %%ok = io:fwrite("cg ~w: ~p~n", [?LINE,{Aft}]),
    {Gis,Aft,St1}.

%% guard_cg_list([Kexpr], Fail, I, Vdb, StackReg, St) ->
%%      {[Ainstr],StackReg,St}.

guard_cg_list(Kes, Fail, Vdb, Bef, St0) ->
    {Keis,{Aft,St1}} =
	flatmapfoldl(fun (Ke, {Inta,Sta}) ->
			     {Keis,Intb,Stb} =
				 guard_cg(Ke, Fail, Vdb, Inta, Sta),
			     {Keis,{Intb,Stb}}
		     end, {Bef,St0}, need_heap(Kes)),
    {Keis,Aft,St1}.

%% protected_cg([Kexpr], [Ret], Fail, I, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  Do a protected.  Protecteds without return values are just done
%%  for effect, the return value is not checked, success passes on to
%%  the next instruction and failure jumps to Fail.  If there are
%%  return values then these must be set to 'false' on failure,
%%  control always passes to the next instruction.

protected_cg(Ts, [], Fail, Vdb, Bef, St0) ->
    %% Protect these calls, revert when done.
    {Tis,Aft,St1} = guard_cg_list(Ts, Fail, Vdb, Bef, St0#cg{bfail=Fail}),
    {Tis,Aft,St1#cg{bfail=St0#cg.bfail}};
protected_cg(Ts, Rs, _Fail, Vdb, Bef, St0) ->
    {Pfail,St1} = new_label(St0),
    {Psucc,St2} = new_label(St1),
    {Tis,Aft,St3} = guard_cg_list(Ts, Pfail, Vdb, Bef,
				  St2#cg{bfail=Pfail}),
    %%ok = io:fwrite("cg ~w: ~p~n", [?LINE,{Rs,I,Vdb,Aft}]),
    %% Set return values to false.
    Mis = [{move,{atom,false},fetch_var(V,Aft)}||#k_var{name=V} <- Rs],
    {Tis ++ [{jump,{f,Psucc}},
	     {label,Pfail}] ++ Mis ++ [{label,Psucc}],
     Aft,St3#cg{bfail=St0#cg.bfail}}.    

%% test_cg(TestName, Args, Fail, I, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  Generate test instruction.  Use explicit fail label here.

test_cg(is_map, [A], Fail, I, Vdb, Bef, St) ->
    %% We must avoid creating code like this:
    %%
    %%   move x(0) y(0)
    %%   is_map Fail [x(0)]
    %%   make_fun => x(0)  %% Overwrite x(0)
    %%   put_map_assoc y(0) ...
    %%
    %% The code is safe, but beam_validator does not understand that.
    %% Extending beam_validator to handle such (rare) code as the
    %% above would make it slower for all programs. Instead, change
    %% the code generator to always prefer the Y register for is_map()
    %% and put_map_assoc() instructions, ensuring that they use the
    %% same register.
    Arg = cg_reg_arg_prefer_y(A, Bef),
    Aft = clear_dead(Bef, I, Vdb),
    {[{test,is_map,{f,Fail},[Arg]}],Aft,St};
test_cg(is_boolean, [#k_atom{val=Val}], Fail, I, Vdb, Bef, St) ->
    Aft = clear_dead(Bef, I, Vdb),
    Is = case is_boolean(Val) of
	     true -> [];
	     false -> [{jump,{f,Fail}}]
	 end,
    {Is,Aft,St};
test_cg(Test, As, Fail, I, Vdb, Bef, St) ->
    Args = cg_reg_args(As, Bef),
    Aft = clear_dead(Bef, I, Vdb),
    {[beam_utils:bif_to_test(Test, Args, {f,Fail})],Aft,St}.

%% match_fmf(Fun, LastFail, State, [Clause]) -> {Is,Aft,State}.
%%  This is a special flatmapfoldl for match code gen where we
%%  generate a "failure" label for each clause. The last clause uses
%%  an externally generated failure label, LastFail.  N.B. We do not
%%  know or care how the failure labels are used.

match_fmf(F, LastFail, St, [H]) ->
    F(H, LastFail, St);
match_fmf(F, LastFail, St0, [H|T]) ->
    {Fail,St1} = new_label(St0),
    {R,Aft1,St2} = F(H, Fail, St1),
    {Rs,Aft2,St3} = match_fmf(F, LastFail, St2, T),
    {R ++ [{label,Fail}] ++ Rs,sr_merge(Aft1, Aft2),St3}.

%% call_cg(Func, [Arg], [Ret], Le, Vdb, StackReg, State) ->
%%      {[Ainstr],StackReg,State}.
%% enter_cg(Func, [Arg], Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  Call and enter first put the arguments into registers and save any
%%  other registers, then clean up and compress the stack and set the
%%  frame size. Finally the actual call is made.  Call then needs the
%%  return values filled in.

call_cg(#k_var{}=Var, As, Rs, Le, Vdb, Bef, St0) ->
    {Sis,Int} = cg_setup_call(As++[Var], Bef, Le#l.i, Vdb),
    %% Put return values in registers.
    Reg = load_vars(Rs, clear_regs(Int#sr.reg)),
    %% Build complete code and final stack/register state.
    Arity = length(As),
    {Frees,Aft} = free_dead(clear_dead(Int#sr{reg=Reg}, Le#l.i, Vdb)),
    {Sis ++ Frees ++ [line(Le),{call_fun,Arity}],Aft,
     need_stack_frame(St0)};
call_cg(#k_remote{mod=Mod,name=Name}, As, Rs, Le, Vdb, Bef, St0)
  when is_record(Mod, k_var); is_record(Name, k_var) ->
    {Sis,Int} = cg_setup_call(As++[Mod,Name], Bef, Le#l.i, Vdb),
    %% Put return values in registers.
    Reg = load_vars(Rs, clear_regs(Int#sr.reg)),
    %% Build complete code and final stack/register state.
    Arity = length(As),
    St = need_stack_frame(St0),
    %%{Call,St1} = build_call(Func, Arity, St0),
    {Frees,Aft} = free_dead(clear_dead(Int#sr{reg=Reg}, Le#l.i, Vdb)),
    {Sis ++ Frees ++ [line(Le),{apply,Arity}],Aft,St};
call_cg(Func, As, Rs, Le, Vdb, Bef, St0) ->
    case St0 of
	#cg{bfail=Fail} when Fail =/= 0 ->
	    %% Inside a guard. The only allowed function call is to
	    %% erlang:error/1,2. We will generate the following code:
	    %%
	    %%     move {atom,ok} DestReg
	    %%     jump FailureLabel
	    #k_remote{mod=#k_atom{val=erlang},
                      name=#k_atom{val=error}} = Func, %Assertion.
	    [#k_var{name=DestVar}] = Rs,
	    Int0 = clear_dead(Bef, Le#l.i, Vdb),
	    Reg = put_reg(DestVar, Int0#sr.reg),
	    Int = Int0#sr{reg=Reg},
	    Dst = fetch_reg(DestVar, Reg),
	    {[{move,{atom,ok},Dst},{jump,{f,Fail}}],
	     clear_dead(Int, Le#l.i, Vdb),St0};
	#cg{} ->
	    %% Ordinary function call in a function body.
	    {Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
	    %% Put return values in registers.
	    Reg = load_vars(Rs, clear_regs(Int#sr.reg)),
	    %% Build complete code and final stack/register state.
	    Arity = length(As),
	    {Call,St1} = build_call(Func, Arity, St0),
	    {Frees,Aft} = free_dead(clear_dead(Int#sr{reg=Reg}, Le#l.i, Vdb)),
	    {Sis ++ Frees ++ [line(Le)|Call],Aft,St1}
    end.

build_call(#k_remote{mod=#k_atom{val=erlang},name=#k_atom{val='!'}}, 2, St0) ->
    {[send],need_stack_frame(St0)};
build_call(#k_remote{mod=#k_atom{val=Mod},name=#k_atom{val=Name}}, Arity, St0) ->
    {[{call_ext,Arity,{extfunc,Mod,Name,Arity}}],need_stack_frame(St0)};
build_call(#k_local{name=Name}, Arity, St0) when is_atom(Name) ->
    {Lbl,St1} = local_func_label(Name, Arity, need_stack_frame(St0)),
    {[{call,Arity,{f,Lbl}}],St1}.

free_dead(#sr{stk=Stk0}=Aft) ->
    {Instr,Stk} = free_dead(Stk0, 0, [], []),
    {Instr,Aft#sr{stk=Stk}}.

free_dead([dead|Stk], Y, Instr, StkAcc) ->
    %% Note: kill/1 is equivalent to init/1 (translated by beam_asm).
    %% We use kill/1 to help further optimisation passes.
    free_dead(Stk, Y+1, [{kill,{yy,Y}}|Instr], [free|StkAcc]);
free_dead([Any|Stk], Y, Instr, StkAcc) ->
    free_dead(Stk, Y+1, Instr, [Any|StkAcc]);
free_dead([], _, Instr, StkAcc) -> {Instr,reverse(StkAcc)}.

enter_cg(#k_var{} = Var, As, Le, Vdb, Bef, St0) ->
    {Sis,Int} = cg_setup_call(As++[Var], Bef, Le#l.i, Vdb),
    %% Build complete code and final stack/register state.
    Arity = length(As),
    {Sis ++ [line(Le),{call_fun,Arity},return],
     clear_dead(Int#sr{reg=clear_regs(Int#sr.reg)}, Le#l.i, Vdb),
     need_stack_frame(St0)};
enter_cg(#k_remote{mod=Mod,name=Name}, As, Le, Vdb, Bef, St0)
  when is_record(Mod, k_var); is_record(Name, k_var) ->
    {Sis,Int} = cg_setup_call(As++[Mod,Name], Bef, Le#l.i, Vdb),
    %% Build complete code and final stack/register state.
    Arity = length(As),
    St = need_stack_frame(St0),
    {Sis ++ [line(Le),{apply_only,Arity}],
     clear_dead(Int#sr{reg=clear_regs(Int#sr.reg)}, Le#l.i, Vdb),
     St};
enter_cg(Func, As, Le, Vdb, Bef, St0) ->
    {Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
    %% Build complete code and final stack/register state.
    Arity = length(As),
    {Call,St1} = build_enter(Func, Arity, St0),
    Line = enter_line(Func, Arity, Le),
    {Sis ++ Line ++ Call,
     clear_dead(Int#sr{reg=clear_regs(Int#sr.reg)}, Le#l.i, Vdb),
     St1}.

build_enter(#k_remote{mod=#k_atom{val=erlang},name=#k_atom{val='!'}}, 2, St0) ->
    {[send,return],need_stack_frame(St0)};
build_enter(#k_remote{mod=#k_atom{val=Mod},name=#k_atom{val=Name}}, Arity, St0) ->
    St1 = case trap_bif(Mod, Name, Arity) of
	      true -> need_stack_frame(St0);
	      false -> St0
	  end,
    {[{call_ext_only,Arity,{extfunc,Mod,Name,Arity}}],St1};
build_enter(#k_local{name=Name}, Arity, St0) when is_atom(Name) ->
    {Lbl,St1} = local_func_label(Name, Arity, St0),
    {[{call_only,Arity,{f,Lbl}}],St1}.

enter_line(#k_remote{mod=#k_atom{val=Mod},name=#k_atom{val=Name}}, Arity, Le) ->
    case erl_bifs:is_safe(Mod, Name, Arity) of
	false ->
	    %% Tail-recursive call, possibly to a BIF.
	    %% We'll need a line instruction in case the
	    %% BIF call fails.
	    [line(Le)];
	true ->
	    %% Call to a safe BIF. Since it cannot fail,
	    %% we don't need any line instruction here.
	    []
    end;
enter_line(_, _, _) ->
    %% Tail-recursive call to a local function. A line
    %% instruction will not be useful.
    [].

%% local_func_label(Name, Arity, State) -> {Label,State'}
%% local_func_label({Name,Arity}, State) -> {Label,State'}
%%  Get the function entry label for a local function.

local_func_label(Name, Arity, St) ->
    local_func_label({Name,Arity}, St).

local_func_label(Key, #cg{functable=Map}=St0) ->
    case Map of
        #{Key := Label} -> {Label,St0};
        _ ->
  	    {Label,St} = new_label(St0),
	    {Label,St#cg{functable=Map#{Key => Label}}}
    end.

%% need_stack_frame(State) -> State'
%%  Make a note in the state that this function will need a stack frame.

need_stack_frame(#cg{need_frame=true}=St) -> St;
need_stack_frame(St) -> St#cg{need_frame=true}.

%% trap_bif(Mod, Name, Arity) -> true|false
%%   Trap bifs that need a stack frame.

trap_bif(erlang, link, 1) -> true;
trap_bif(erlang, unlink, 1) -> true;
trap_bif(erlang, monitor_node, 2) -> true;
trap_bif(erlang, group_leader, 2) -> true;
trap_bif(erlang, exit, 2) -> true;
trap_bif(_, _, _) -> false.

%% bif_cg(#k_bif{}, Le, Vdb, StackReg, State) ->
%%      {[Ainstr],StackReg,State}.
%%  Generate code a BIF.

bif_cg(#k_bif{op=#k_internal{name=Name},args=As,ret=Rs}, Le, Vdb, Bef, St) ->
    internal_cg(Name, As, Rs, Le, Vdb, Bef, St);
bif_cg(#k_bif{op=#k_remote{mod=#k_atom{val=erlang},name=#k_atom{val=Name}},
              args=As,ret=Rs}, Le, Vdb, Bef, St) ->
    Ar = length(As),
    case is_gc_bif(Name, Ar) of
	false ->
            bif_cg(Name, As, Rs, Le, Vdb, Bef, St);
	true ->
            gc_bif_cg(Name, As, Rs, Le, Vdb, Bef, St)
    end.

%% internal_cg(Bif, [Arg], [Ret], Le, Vdb, StackReg, State) ->
%%      {[Ainstr],StackReg,State}.

internal_cg(bs_context_to_binary=Instr, [Src0], [], Le, Vdb, Bef, St0) ->
    [Src] = cg_reg_args([Src0], Bef),
    {[{Instr,Src}],clear_dead(Bef, Le#l.i, Vdb), St0};
internal_cg(dsetelement, [Index0,Tuple0,New0], _Rs, Le, Vdb, Bef, St0) ->
    [New,Tuple,{integer,Index1}] = cg_reg_args([New0,Tuple0,Index0], Bef),
    Index = Index1-1,
    {[{set_tuple_element,New,Tuple,Index}],
     clear_dead(Bef, Le#l.i, Vdb), St0};
internal_cg(make_fun, [Func0,Arity0|As], Rs, Le, Vdb, Bef, St0) ->
    %% This behaves more like a function call.
    #k_atom{val=Func} = Func0,
    #k_int{val=Arity} = Arity0,
    {Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
    Reg = load_vars(Rs, clear_regs(Int#sr.reg)),
    {FuncLbl,St1} = local_func_label(Func, Arity, St0),
    MakeFun = {make_fun2,{f,FuncLbl},0,0,length(As)},
    {Sis ++ [MakeFun],
     clear_dead(Int#sr{reg=Reg}, Le#l.i, Vdb),
     St1};
internal_cg(bs_init_writable=I, As, Rs, Le, Vdb, Bef, St) ->
    %% This behaves like a function call.
    {Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
    Reg = load_vars(Rs, clear_regs(Int#sr.reg)),
    {Sis++[I],clear_dead(Int#sr{reg=Reg}, Le#l.i, Vdb),St};
internal_cg(build_stacktrace=I, As, Rs, Le, Vdb, Bef, St) ->
    %% This behaves like a function call.
    {Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
    Reg = load_vars(Rs, clear_regs(Int#sr.reg)),
    {Sis++[I],clear_dead(Int#sr{reg=Reg}, Le#l.i, Vdb),St};
internal_cg(raise, As, Rs, Le, Vdb, Bef, St) ->
    %% raise can be treated like a guard BIF.
    bif_cg(raise, As, Rs, Le, Vdb, Bef, St);
internal_cg(guard_error, [ExitCall], _Rs, Le, Vdb, Bef, St) ->
    %% A call an exit BIF from inside a #k_guard_match{}.
    %% Generate a standard call, but leave the register descriptors
    %% alone, effectively pretending that there was no call.
    #k_call{op=#k_remote{mod=#k_atom{val=Mod},name=#k_atom{val=Name}},
            args=As} = ExitCall,
    Arity = length(As),
    {Ms,_} = cg_call_args(As, Bef, Le#l.i, Vdb),
    Call = {call_ext,Arity,{extfunc,Mod,Name,Arity}},
    Is = Ms++[line(Le),Call],
    {Is,Bef,St};
internal_cg(raw_raise=I, As, Rs, Le, Vdb, Bef, St) ->
    %% This behaves like a function call.
    {Sis,Int} = cg_setup_call(As, Bef, Le#l.i, Vdb),
    Reg = load_vars(Rs, clear_regs(Int#sr.reg)),
    {Sis++[I],clear_dead(Int#sr{reg=Reg}, Le#l.i, Vdb),St}.

%% bif_cg(Bif, [Arg], [Ret], Le, Vdb, StackReg, State) ->
%%      {[Ainstr],StackReg,State}.

bif_cg(Bif, As, [#k_var{name=V}], Le, Vdb, Bef, St0) ->
    Ars = cg_reg_args(As, Bef),

    %% If we are inside a catch and in a body (not in guard) and the
    %% BIF may fail, we must save everything that will be alive after
    %% the catch (because the code after the code assumes that all
    %% variables that are live are stored on the stack).
    %%
    %%   Currently, we are somewhat pessimistic in
    %% that we save any variable that will be live after this BIF call.

    MayFail = not erl_bifs:is_safe(erlang, Bif, length(As)),
    {Sis,Int0} =
	case MayFail of
	    true ->
		maybe_adjust_stack(Bef, Le#l.i, Le#l.i+1, Vdb, St0);
	    false ->
		{[],Bef}
	end,
    Int1 = clear_dead(Int0, Le#l.i, Vdb),
    Reg = put_reg(V, Int1#sr.reg),
    Int = Int1#sr{reg=Reg},
    Dst = fetch_reg(V, Reg),
    BifFail = {f,St0#cg.bfail},
    %% We need a line instructions for BIFs that may fail in a body.
    Line = case BifFail of
	       {f,0} when MayFail ->
		   [line(Le)];
	       _ ->
		   []
	   end,
    {Sis++Line++[{bif,Bif,BifFail,Ars,Dst}],
     clear_dead(Int, Le#l.i, Vdb), St0}.


%% gc_bif_cg(Bif, [Arg], [Ret], Le, Vdb, StackReg, State) ->
%%      {[Ainstr],StackReg,State}.

gc_bif_cg(Bif, As, [#k_var{name=V}], Le, Vdb, Bef, St0) ->
    Ars = cg_reg_args(As, Bef),

    %% If we are inside a catch and in a body (not in guard) and the
    %% BIF may fail, we must save everything that will be alive after
    %% the catch (because the code after the code assumes that all
    %% variables that are live are stored on the stack).
    %%
    %%   Currently, we are somewhat pessimistic in
    %% that we save any variable that will be live after this BIF call.

    {Sis,Int0} = maybe_adjust_stack(Bef, Le#l.i, Le#l.i+1, Vdb, St0),

    Int1 = clear_dead(Int0, Le#l.i, Vdb),
    Reg = put_reg(V, Int1#sr.reg),
    Int = Int1#sr{reg=Reg},
    Dst = fetch_reg(V, Reg),
    BifFail = {f,St0#cg.bfail},
    Line = case BifFail of
	       {f,0} -> [line(Le)];
	       {f,_} -> []
	   end,
    {Sis++Line++[{gc_bif,Bif,BifFail,max_reg(Bef#sr.reg),Ars,Dst}],
     clear_dead(Int, Le#l.i, Vdb), St0}.

%% recv_loop_cg(TimeOut, ReceiveVar, ReceiveMatch, TimeOutExprs,
%%              [Ret], Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.

recv_loop_cg(Te, Rvar, Rm, Tes, Rs, Le, Vdb, Bef, St0) ->
    {Sis,Int0} = adjust_stack(Bef, Le#l.i, Le#l.i, Vdb),
    Int1 = Int0#sr{reg=clear_regs(Int0#sr.reg)},
    %% Get labels.
    {Rl,St1} = new_label(St0),
    {Tl,St2} = new_label(St1),
    {Bl,St3} = new_label(St2),
    St4 = St3#cg{break=Bl,recv=Rl},		%Set correct receive labels
    {Ris,Raft,St5} = cg_recv_mesg(Rvar, Rm, Tl, Int1, St4),
    {Wis,Taft,St6} = cg_recv_wait(Te, Tes, Le#l.i, Int1, St5),
    Int2 = sr_merge(Raft, Taft),		%Merge stack/registers
    Reg = load_vars(Rs, Int2#sr.reg),
    {Sis ++ [line(Le)] ++ Ris ++ [{label,Tl}] ++ Wis ++ [{label,Bl}],
     clear_dead(Int2#sr{reg=Reg}, Le#l.i, Vdb),
     St6#cg{break=St0#cg.break,recv=St0#cg.recv}}.

%% cg_recv_mesg( ) -> {[Ainstr],Aft,St}.

cg_recv_mesg(#k_var{name=R}, Rm, Tl, Bef, St0) ->
    Int0 = Bef#sr{reg=put_reg(R, Bef#sr.reg)},
    Ret = fetch_reg(R, Int0#sr.reg),
    %% Int1 = clear_dead(Int0, I, Rm#l.vdb),
    Int1 = Int0,
    {Mis,Int2,St1} = match_cg(Rm, none, Int1, St0),
    {[{label,St1#cg.recv},{loop_rec,{f,Tl},Ret}|Mis],Int2,St1}.

%% cg_recv_wait(Te, Tes, I, Vdb, Int2, St3) -> {[Ainstr],Aft,St}.

cg_recv_wait(#k_atom{val=infinity}, #cg_block{anno=Le,es=Tes}, I, Bef, St0) ->
    %% We know that the 'after' body will never be executed.
    %% But to keep the stack and register information up to date,
    %% we will generate the code for the 'after' body, and then discard it.
    Int1 = clear_dead(Bef, I, Le#l.vdb),
    {_,Int2,St1} = cg_block(Tes, Le#l.vdb,
                            Int1#sr{reg=clear_regs(Int1#sr.reg)}, St0),
    {[{wait,{f,St1#cg.recv}}],Int2,St1};
cg_recv_wait(#k_int{val=0}, #cg_block{anno=Le,es=Tes}, _I, Bef, St0) ->
    {Tis,Int,St1} = cg_block(Tes, Le#l.vdb, Bef, St0),
    {[timeout|Tis],Int,St1};
cg_recv_wait(Te, #cg_block{anno=Le,es=Tes}, I, Bef, St0) ->
    Reg = cg_reg_arg(Te, Bef),
    %% Must have empty registers here!  Bug if anything in registers.
    Int0 = clear_dead(Bef, I, Le#l.vdb),
    {Tis,Int,St1} = cg_block(Tes, Le#l.vdb,
			     Int0#sr{reg=clear_regs(Int0#sr.reg)}, St0),
    {[{wait_timeout,{f,St1#cg.recv},Reg},timeout] ++ Tis,Int,St1}.

%% recv_next_cg(Le, Vdb, StackReg, St) -> {[Ainstr],StackReg,St}.
%%  Use adjust stack to clear stack, but only need it for Aft.

recv_next_cg(Le, Vdb, Bef, St) ->
    {Sis,Aft} = adjust_stack(Bef, Le#l.i, Le#l.i+1, Vdb),
    {[{loop_rec_end,{f,St#cg.recv}}] ++ Sis,Aft,St}.	%Joke

%% try_cg(TryBlock, [BodyVar], TryBody, [ExcpVar], TryHandler, [Ret],
%%        Le, Vdb, StackReg, St) -> {[Ainstr],StackReg,St}.

try_cg(Ta, Vs, Tb, Evs, Th, Rs, Le, Vdb, Bef, St0) ->
    {B,St1} = new_label(St0),			%Body label
    {H,St2} = new_label(St1),			%Handler label
    {E,St3} = new_label(St2),			%End label
    #l{i=TryTag} = get_kanno(Ta),
    Int1 = Bef#sr{stk=put_catch(TryTag, Bef#sr.stk)},
    TryReg = fetch_stack({catch_tag,TryTag}, Int1#sr.stk),
    {Ais,Int2,St4} = cg(Ta, Vdb, Int1, St3#cg{break=B,in_catch=true}),
    Int3 = Int2#sr{stk=drop_catch(TryTag, Int2#sr.stk)},
    St5 = St4#cg{break=E,in_catch=St3#cg.in_catch},
    {Bis,Baft,St6} = cg(Tb, Vdb, Int3#sr{reg=load_vars(Vs, Int3#sr.reg)}, St5),
    {His,Haft,St7} = cg(Th, Vdb, Int3#sr{reg=load_vars(Evs, Int3#sr.reg)}, St6),
    Int4 = sr_merge(Baft, Haft),		%Merge stack/registers
    Aft = Int4#sr{reg=load_vars(Rs, Int4#sr.reg)},
    {[{'try',TryReg,{f,H}}] ++ Ais ++ 
     [{label,B},{try_end,TryReg}] ++ Bis ++
     [{label,H},{try_case,TryReg}] ++ His ++
     [{label,E}],
     clear_dead(Aft, Le#l.i, Vdb),
     St7#cg{break=St0#cg.break}}.     

try_enter_cg(Ta, Vs, Tb, Evs, Th, Le, Vdb, Bef, St0) ->
    {B,St1} = new_label(St0),			%Body label
    {H,St2} = new_label(St1),			%Handler label
    #l{i=TryTag} = get_kanno(Ta),
    Int1 = Bef#sr{stk=put_catch(TryTag, Bef#sr.stk)},
    TryReg = fetch_stack({catch_tag,TryTag}, Int1#sr.stk),
    {Ais,Int2,St3} = cg(Ta, Vdb, Int1, St2#cg{break=B,in_catch=true}),
    Int3 = Int2#sr{stk=drop_catch(TryTag, Int2#sr.stk)},
    St4 = St3#cg{in_catch=St2#cg.in_catch},
    {Bis,Baft,St5} = cg(Tb, Vdb, Int3#sr{reg=load_vars(Vs, Int3#sr.reg)}, St4),
    {His,Haft,St6} = cg(Th, Vdb, Int3#sr{reg=load_vars(Evs, Int3#sr.reg)}, St5),
    Int4 = sr_merge(Baft, Haft),		%Merge stack/registers
    Aft = Int4,
    {[{'try',TryReg,{f,H}}] ++ Ais ++ 
     [{label,B},{try_end,TryReg}] ++ Bis ++
     [{label,H},{try_case,TryReg}] ++ His,
     clear_dead(Aft, Le#l.i, Vdb),
     St6#cg{break=St0#cg.break}}.     

%% catch_cg(CatchBlock, Ret, Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.

catch_cg(#cg_block{es=C}, #k_var{name=R}, Le, Vdb, Bef, St0) ->
    {B,St1} = new_label(St0),
    CatchTag = Le#l.i,
    Int1 = Bef#sr{stk=put_catch(CatchTag, Bef#sr.stk)},
    CatchReg = fetch_stack({catch_tag,CatchTag}, Int1#sr.stk),
    {Cis,Int2,St2} = cg_block(C, Le#l.vdb, Int1,
			      St1#cg{break=B,in_catch=true}),
    [] = Int2#sr.reg,				%Assertion.
    Aft = Int2#sr{reg=[{0,R}],stk=drop_catch(CatchTag, Int2#sr.stk)},
    {[{'catch',CatchReg,{f,B}}] ++ Cis ++
     [{label,B},{catch_end,CatchReg}],
     clear_dead(Aft, Le#l.i, Vdb),
     St2#cg{break=St1#cg.break,in_catch=St1#cg.in_catch}}.

%% put_cg([Var], Constr, Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  We have to be careful how a 'put' works. First the structure is
%%  built, then it is filled and finally things can be cleared. The
%%  annotation must reflect this and make sure that the return
%%  variable is allocated first.
%%
%%  put_list and put_map are atomic instructions, both of
%%  which can safely resuse one of the source registers as target.

put_cg([#k_var{name=R}], #k_cons{hd=Hd,tl=Tl}, Le, Vdb, Bef, St) ->
    [S1,S2] = cg_reg_args([Hd,Tl], Bef),
    Int0 = clear_dead(Bef, Le#l.i, Vdb),
    Int1 = Int0#sr{reg=put_reg(R, Int0#sr.reg)},
    Ret = fetch_reg(R, Int1#sr.reg),
    {[{put_list,S1,S2,Ret}], Int1, St};
put_cg([#k_var{name=R}], #k_binary{segs=Segs}, Le, Vdb, Bef,
       #cg{bfail=Bfail}=St) ->
    %% At run-time, binaries are constructed in three stages:
    %% 1) First the size of the binary is calculated.
    %% 2) Then the binary is allocated.
    %% 3) Then each field in the binary is constructed.
    %% For simplicity, we use the target register to also hold the
    %% size of the binary. Therefore the target register must *not*
    %% be one of the source registers.

    %% First allocate the target register.
    Int0 = Bef#sr{reg=put_reg(R, Bef#sr.reg)},
    Target = fetch_reg(R, Int0#sr.reg),

    %% Also allocate a scratch register for size calculations.
    Temp = find_scratch_reg(Int0#sr.reg),

    %% First generate the code that constructs each field.
    Fail = {f,Bfail},
    PutCode = cg_bin_put(Segs, Fail, Bef),
    {Sis,Int1} = maybe_adjust_stack(Int0, Le#l.i, Le#l.i+1, Vdb, St),
    MaxRegs = max_reg(Bef#sr.reg),
    Aft = clear_dead(Int1, Le#l.i, Vdb),

    %% Now generate the complete code for constructing the binary.
    Code = cg_binary(PutCode, Target, Temp, Fail, MaxRegs, Le#l.a),
    {Sis++Code,Aft,St};

%% Map: single variable key.
put_cg([#k_var{name=R}], #k_map{op=Op,var=Map,
                                es=[#k_map_pair{key=#k_var{}=K,val=V}]},
       Le, Vdb, Bef, St0) ->
    {Sis,Int0} = maybe_adjust_stack(Bef, Le#l.i, Le#l.i+1, Vdb, St0),

    SrcReg = cg_reg_arg_prefer_y(Map, Int0),
    Line = line(Le#l.a),

    List = [cg_reg_arg(K,Int0),cg_reg_arg(V,Int0)],

    Live = max_reg(Bef#sr.reg),

    %% The target register can reuse one of the source registers.
    Aft0 = clear_dead(Int0, Le#l.i, Vdb),
    Aft = Aft0#sr{reg=put_reg(R, Aft0#sr.reg)},
    Target = fetch_reg(R, Aft#sr.reg),

    {Is,St1} = put_cg_map(Line, Op, SrcReg, Target, Live, List, St0),
    {Sis++Is,Aft,St1};

%% Map: (possibly) multiple literal keys.
put_cg([#k_var{name=R}], #k_map{op=Op,var=Map,es=Es}, Le, Vdb, Bef, St0) ->

    %% assert key literals
    [] = [Var || #k_map_pair{key=#k_var{}=Var} <- Es],

    {Sis,Int0} = maybe_adjust_stack(Bef, Le#l.i, Le#l.i+1, Vdb, St0),
    SrcReg = cg_reg_arg_prefer_y(Map, Int0),
    Line = line(Le#l.a),

    %% fetch registers for values to be put into the map
    List = flatmap(fun(#k_map_pair{key=K,val=V}) ->
                           [atomic(K),cg_reg_arg(V, Int0)]
                   end, Es),

    Live = max_reg(Bef#sr.reg),

    %% The target register can reuse one of the source registers.
    Aft0 = clear_dead(Int0, Le#l.i, Vdb),
    Aft = Aft0#sr{reg=put_reg(R, Aft0#sr.reg)},
    Target = fetch_reg(R, Aft#sr.reg),

    {Is,St1} = put_cg_map(Line, Op, SrcReg, Target, Live, List, St0),
    {Sis++Is,Aft,St1};

%% Everything else.
put_cg([#k_var{name=R}], Con, Le, Vdb, Bef, St) ->
    %% Find a place for the return register first.
    Int = Bef#sr{reg=put_reg(R, Bef#sr.reg)},
    Ret = fetch_reg(R, Int#sr.reg),
    Ais = case Con of
	      #k_tuple{es=Es} ->
		  [{put_tuple,length(Es),Ret}] ++ cg_build_args(Es, Bef);
	      Other ->
		  [{move,cg_reg_arg(Other, Int),Ret}]
	  end,
    {Ais,clear_dead(Int, Le#l.i, Vdb),St}.


put_cg_map(Line, Op0, SrcReg, Target, Live, List, St0) ->
    Bfail = St0#cg.bfail,
    Fail = {f,St0#cg.bfail},
    Op = case Op0 of
	     assoc -> put_map_assoc;
	     exact -> put_map_exact
	 end,
    {OkLbl,St1} = new_label(St0),
    {BadLbl,St2} = new_label(St1),
    Is = if
	     Bfail =:= 0 orelse Op =:= put_map_assoc ->
		 [Line,{Op,{f,0},SrcReg,Target,Live,{list,List}}];
	     true ->
		 %% Ensure that Target is always set, even if
		 %% the map update operation fails. That is necessary
		 %% because Target may be included in a test_heap
		 %% instruction.
		 [Line,
		  {Op,{f,BadLbl},SrcReg,Target,Live,{list,List}},
		  {jump,{f,OkLbl}},
		  {label,BadLbl},
		  {move,{atom,ok},Target},
		  {jump,Fail},
		  {label,OkLbl}]
	 end,
    {Is,St2}.

%%%
%%% Code generation for constructing binaries.
%%%

cg_binary([{bs_put_binary,Fail,{atom,all},U,_Flags,Src}|PutCode],
	  Target, Temp, Fail, MaxRegs, Anno) ->
    Line = line(Anno),
    Live = cg_live(Target, MaxRegs),
    SzCode = cg_bitstr_size(PutCode, Target, Temp, Fail, Live),
    BinFlags = {field_flags,[]},
    Code = [Line|SzCode] ++
	[case member(single_use, Anno) of
	     true ->
		 {bs_private_append,Fail,Target,U,Src,BinFlags,Target};
	     false ->
		 {bs_append,Fail,Target,0,MaxRegs,U,Src,BinFlags,Target}
	 end] ++ PutCode,
    cg_bin_opt(Code);
cg_binary(PutCode, Target, Temp, Fail, MaxRegs, Anno) ->
    Line = line(Anno),
    Live = cg_live(Target, MaxRegs),
    {InitOp,SzCode} = cg_binary_size(PutCode, Target, Temp, Fail, Live),

    Code = [Line|SzCode] ++ [{InitOp,Fail,Target,0,MaxRegs,
			      {field_flags,[]},Target}|PutCode],
    cg_bin_opt(Code).

cg_live({x,X}, MaxRegs) when X =:= MaxRegs -> MaxRegs+1;
cg_live({x,X}, MaxRegs) when X < MaxRegs -> MaxRegs.

%% Generate code that calculate the size of the bitstr to be
%% built in BITS.

cg_bitstr_size(PutCode, Target, Temp, Fail, Live) ->
    {Bits,Es} = cg_bitstr_size_1(PutCode, 0, []),
    reverse(cg_gen_binsize(Es, Target, Temp, Fail, Live,
			   [{move,{integer,Bits},Target}])).

cg_bitstr_size_1([{bs_put_utf8,_,_,Src}|Next], Bits, Acc) ->
    cg_bitstr_size_1(Next, Bits, [{'*',{bs_utf8_size,Src},8}|Acc]);
cg_bitstr_size_1([{bs_put_utf16,_,_,Src}|Next], Bits, Acc) ->
    cg_bitstr_size_1(Next, Bits, [{'*',{bs_utf16_size,Src},8}|Acc]);
cg_bitstr_size_1([{bs_put_utf32,_,_,_}|Next], Bits, Acc) ->
    cg_bitstr_size_1(Next, Bits+32, Acc);
cg_bitstr_size_1([{_,_,S,U,_,Src}|Next], Bits, Acc) ->
    case S of
	{integer,N} -> cg_bitstr_size_1(Next, Bits+N*U, Acc);
	{atom,all} -> cg_bitstr_size_1(Next, Bits, [{bit_size,Src}|Acc]);
	_ when U =:= 1 -> cg_bitstr_size_1(Next, Bits, [S|Acc]);
	_ -> cg_bitstr_size_1(Next, Bits, [{'*',S,U}|Acc])
    end;
cg_bitstr_size_1([], Bits, Acc) -> {Bits,Acc}.

%% Generate code that calculate the size of the bitstr to be
%% built in BYTES or BITS (depending on what is easiest).

cg_binary_size(PutCode, Target, Temp, Fail, Live) ->
    {InitInstruction,Szs} = cg_binary_size_1(PutCode, 0, []),
    SizeExpr = reverse(cg_gen_binsize(Szs, Target, Temp, Fail, Live, [{move,{integer,0},Target}])),
    {InitInstruction,SizeExpr}.

cg_binary_size_1([{bs_put_utf8,_Fail,_Flags,Src}|T], Bits, Acc) ->
    cg_binary_size_1(T, Bits, [{8,{bs_utf8_size,Src}}|Acc]);
cg_binary_size_1([{bs_put_utf16,_Fail,_Flags,Src}|T], Bits, Acc) ->
    cg_binary_size_1(T, Bits, [{8,{bs_utf16_size,Src}}|Acc]);
cg_binary_size_1([{bs_put_utf32,_Fail,_Flags,_Src}|T], Bits, Acc) ->
    cg_binary_size_1(T, Bits+32, Acc);
cg_binary_size_1([{_Put,_Fail,S,U,_Flags,Src}|T], Bits, Acc) ->
    cg_binary_size_2(S, U, Src, T, Bits, Acc);
cg_binary_size_1([], Bits, Acc) ->
    Bytes = Bits div 8,
    RemBits = Bits rem 8,
    Sizes0 = sort([{1,{integer,RemBits}},{8,{integer,Bytes}}|Acc]),
    Sizes = filter(fun({_,{integer,0}}) -> false;
		      (_) -> true end, Sizes0),
    case Sizes of
	[{1,_}|_] ->
	    {bs_init_bits,cg_binary_bytes_to_bits(Sizes, [])};
	[{8,_}|_] ->
	    {bs_init2,[E || {8,E} <- Sizes]};
	[] ->
	    {bs_init_bits,[]}
    end.

cg_binary_size_2({integer,N}, U, _, Next, Bits, Acc) ->
    cg_binary_size_1(Next, Bits+N*U, Acc);
cg_binary_size_2({atom,all}, U, E, Next, Bits, Acc) ->
    if 
	U rem 8 =:= 0 ->
	    cg_binary_size_1(Next, Bits, [{8,{byte_size,E}}|Acc]);
	true ->
	    cg_binary_size_1(Next, Bits, [{1,{bit_size,E}}|Acc])
    end;
cg_binary_size_2(Reg, 1, _, Next, Bits, Acc) ->
    cg_binary_size_1(Next, Bits, [{1,Reg}|Acc]);
cg_binary_size_2(Reg, 8, _, Next, Bits, Acc) ->
    cg_binary_size_1(Next, Bits, [{8,Reg}|Acc]);
cg_binary_size_2(Reg, U, _, Next, Bits, Acc) ->
    cg_binary_size_1(Next, Bits, [{1,{'*',Reg,U}}|Acc]).

cg_binary_bytes_to_bits([{8,{integer,N}}|T], Acc) ->
    cg_binary_bytes_to_bits(T, [{integer,8*N}|Acc]);
cg_binary_bytes_to_bits([{8,{byte_size,Reg}}|T], Acc) ->
    cg_binary_bytes_to_bits(T, [{bit_size,Reg}|Acc]);
cg_binary_bytes_to_bits([{8,Reg}|T], Acc) ->
    cg_binary_bytes_to_bits(T, [{'*',Reg,8}|Acc]);
cg_binary_bytes_to_bits([{1,Sz}|T], Acc) ->
    cg_binary_bytes_to_bits(T, [Sz|Acc]);
cg_binary_bytes_to_bits([], Acc) ->
    cg_binary_bytes_to_bits_1(sort(Acc)).

cg_binary_bytes_to_bits_1([{integer,I},{integer,J}|T]) ->
    cg_binary_bytes_to_bits_1([{integer,I+J}|T]);
cg_binary_bytes_to_bits_1([H|T]) ->
    [H|cg_binary_bytes_to_bits_1(T)];
cg_binary_bytes_to_bits_1([]) -> [].

cg_gen_binsize([{'*',{bs_utf8_size,Src},B}|T], Target, Temp, Fail, Live, Acc) ->
    Size = {bs_utf8_size,Fail,Src,Temp},
    Add = {bs_add,Fail,[Target,Temp,B],Target},
    cg_gen_binsize(T, Target, Temp, Fail, Live,
		   [Add,Size|Acc]);
cg_gen_binsize([{'*',{bs_utf16_size,Src},B}|T], Target, Temp, Fail, Live, Acc) ->
    Size = {bs_utf16_size,Fail,Src,Temp},
    Add = {bs_add,Fail,[Target,Temp,B],Target},
    cg_gen_binsize(T, Target, Temp, Fail, Live,
		   [Add,Size|Acc]);
cg_gen_binsize([{'*',A,B}|T], Target, Temp, Fail, Live, Acc) ->
    cg_gen_binsize(T, Target, Temp, Fail, Live,
		   [{bs_add,Fail,[Target,A,B],Target}|Acc]);
cg_gen_binsize([{bit_size,B}|T], Target, Temp, Fail, Live, Acc) ->
    cg_gen_binsize([Temp|T], Target, Temp, Fail, Live,
		   [{gc_bif,bit_size,Fail,Live,[B],Temp}|Acc]);
cg_gen_binsize([{byte_size,B}|T], Target, Temp, Fail, Live, Acc) ->
    cg_gen_binsize([Temp|T], Target, Temp, Fail, Live,
		   [{gc_bif,byte_size,Fail,Live,[B],Temp}|Acc]);
cg_gen_binsize([{bs_utf8_size,B}|T], Target, Temp, Fail, Live, Acc) ->
    cg_gen_binsize([Temp|T], Target, Temp, Fail, Live,
		   [{bs_utf8_size,Fail,B,Temp}|Acc]);
cg_gen_binsize([{bs_utf16_size,B}|T], Target, Temp, Fail, Live, Acc) ->
    cg_gen_binsize([Temp|T], Target, Temp, Fail, Live,
		   [{bs_utf16_size,Fail,B,Temp}|Acc]);
cg_gen_binsize([E0|T], Target, Temp, Fail, Live, Acc) ->
    cg_gen_binsize(T, Target, Temp, Fail, Live,
		   [{bs_add,Fail,[Target,E0,1],Target}|Acc]);
cg_gen_binsize([], _, _, _, _, Acc) -> Acc.


%% cg_bin_opt(Code0) -> Code
%%  Optimize the size calculations for binary construction.

cg_bin_opt([{move,S1,{x,X}=D},{gc_bif,Op,Fail,Live0,As,Dst}|Is]) ->
    Live = if
               X + 1 =:= Live0 -> X;
               true -> Live0
           end,
    [{gc_bif,Op,Fail,Live,As,D}|cg_bin_opt([{move,S1,Dst}|Is])];
cg_bin_opt([{move,_,_}=I1,{Op,_,_,_}=I2|Is])
  when Op =:= bs_utf8_size orelse Op =:= bs_utf16_size ->
    [I2|cg_bin_opt([I1|Is])];
cg_bin_opt([{bs_add,_,[{integer,0},Src,1],Dst}|Is]) ->
    cg_bin_opt_1([{move,Src,Dst}|Is]);
cg_bin_opt([{bs_add,_,[Src,{integer,0},_],Dst}|Is]) ->
    cg_bin_opt_1([{move,Src,Dst}|Is]);
cg_bin_opt(Is) ->
    cg_bin_opt_1(Is).

cg_bin_opt_1([{move,Size,D},{bs_append,Fail,D,Extra,Regs,U,Bin,Flags,D}|Is]) ->
    [{bs_append,Fail,Size,Extra,Regs,U,Bin,Flags,D}|cg_bin_opt(Is)];
cg_bin_opt_1([{move,Size,D},{bs_private_append,Fail,D,U,Bin,Flags,D}|Is]) ->
    [{bs_private_append,Fail,Size,U,Bin,Flags,D}|cg_bin_opt(Is)];
cg_bin_opt_1([{move,Size,D},{Op,Fail,D,Extra,Regs,Flags,D}|Is])
  when Op =:= bs_init2; Op =:= bs_init_bits ->
    Bytes = case Size of
                {integer,Int} -> Int;
                _ -> Size
            end,
    [{Op,Fail,Bytes,Extra,Regs,Flags,D}|cg_bin_opt(Is)];
cg_bin_opt_1([{move,S1,D},{bs_add,Fail,[D,S2,U],Dst}|Is]) ->
    cg_bin_opt([{bs_add,Fail,[S1,S2,U],Dst}|Is]);
cg_bin_opt_1([{move,S1,D},{bs_add,Fail,[S2,D,U],Dst}|Is]) ->
    cg_bin_opt([{bs_add,Fail,[S2,S1,U],Dst}|Is]);
cg_bin_opt_1([I|Is]) ->
    [I|cg_bin_opt(Is)];
cg_bin_opt_1([]) ->
    [].

cg_bin_put(#k_bin_seg{size=S0,unit=U,type=T,flags=Fs,seg=E0,next=Next},
           Fail, Bef) ->
    S1 = cg_reg_arg(S0, Bef),
    E1 = cg_reg_arg(E0, Bef),
    {Format,Op} = case T of
		      integer -> {plain,bs_put_integer};
		      utf8 ->    {utf,bs_put_utf8};
		      utf16 ->   {utf,bs_put_utf16};
		      utf32 ->   {utf,bs_put_utf32};
		      binary  -> {plain,bs_put_binary};
		      float   -> {plain,bs_put_float}
		  end,
    case Format of
	plain ->
	    [{Op,Fail,S1,U,{field_flags,Fs},E1}|cg_bin_put(Next, Fail, Bef)];
	utf ->
	    [{Op,Fail,{field_flags,Fs},E1}|cg_bin_put(Next, Fail, Bef)]
    end;
cg_bin_put(#k_bin_end{}, _, _) -> [].

cg_build_args(As, Bef) ->
    [{put,cg_reg_arg(A, Bef)} || A <- As].

%% return_cg([Val], Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%% break_cg([Val], Le, Vdb, Bef, St) -> {[Ainstr],Aft,St}.
%%  These are very simple, just put return/break values in registers
%%  from 0, then return/break.  Use the call setup to clean up stack,
%%  but must clear registers to ensure sr_merge works correctly.

return_cg(Rs, Le, Vdb, Bef, St) ->
    {Ms,Int} = cg_setup_call(Rs, Bef, Le#l.i, Vdb),
    {Ms ++ [return],Int#sr{reg=clear_regs(Int#sr.reg)},St}.

break_cg(Bs, Le, Vdb, Bef, St) ->
    {Ms,Int} = cg_setup_call(Bs, Bef, Le#l.i, Vdb),
    {Ms ++ [{jump,{f,St#cg.break}}],
     Int#sr{reg=clear_regs(Int#sr.reg)},St}.

guard_break_cg(Bs, #l{i=I}, Vdb, #sr{reg=Reg0}=Bef, St) ->
    #sr{reg=Reg1} = Int = clear_dead(Bef, I, Vdb),
    Reg2 = trim_free(Reg1),
    NumLocked = length(Reg2),
    Moves0 = gen_moves(Bs, Bef, NumLocked, []),
    Moves = order_moves(Moves0, find_scratch_reg(Reg0)),
    {BreakVars,_} = mapfoldl(fun(_, RegNum) ->
				     {{RegNum,gbreakvar},RegNum+1}
			     end, length(Reg2), Bs),
    Reg = Reg2 ++ BreakVars,
    Aft = Int#sr{reg=Reg},
    {Moves ++ [{jump,{f,St#cg.break}}],Aft,St}.

%% cg_reg_arg(Arg0, Info) -> Arg
%% cg_reg_args([Arg0], Info) -> [Arg]
%%  Convert argument[s] into registers. Literal values are returned unchanged.

cg_reg_args(As, Bef) -> [cg_reg_arg(A, Bef) || A <- As].

cg_reg_arg(#k_var{name=V}, Bef) -> fetch_var(V, Bef);
cg_reg_arg(Literal, _) -> atomic(Literal).

cg_reg_arg_prefer_y(#k_var{name=V}, Bef) -> fetch_var_prefer_y(V, Bef);
cg_reg_arg_prefer_y(Literal, _) -> atomic(Literal).

%% cg_setup_call([Arg], Bef, Cur, Vdb) -> {[Instr],Aft}.
%%  Do the complete setup for a call/enter.

cg_setup_call(As, Bef, I, Vdb) ->
    {Ms,Int0} = cg_call_args(As, Bef, I, Vdb),
    %% Have set up arguments, can now clean up, compress and save to stack.
    Int1 = Int0#sr{stk=clear_dead_stk(Int0#sr.stk, I, Vdb),res=[]},
    {Sis,Int2} = adjust_stack(Int1, I, I+1, Vdb),
    {Ms ++ Sis,Int2}.

%% cg_call_args([Arg], SrState) -> {[Instr],SrState}.
%%  Setup the arguments to a call/enter/bif. Put the arguments into
%%  consecutive registers starting at {x,0} moving any data which
%%  needs to be saved. Return a modified SrState structure with the
%%  new register contents.  N.B. the resultant register info will
%%  contain non-variable values when there are non-variable values.
%%
%%  This routine is complicated by unsaved values in x registers.
%%  We'll move away any unsaved values that are in the registers
%%  to be overwritten by the arguments.

cg_call_args(As, Bef, I, Vdb) ->
    Regs0 = load_arg_regs(Bef#sr.reg, As),
    Unsaved = unsaved_registers(Regs0, Bef#sr.stk, I, I+1, Vdb),
    {UnsavedMoves,Regs} = move_unsaved(Unsaved, Bef#sr.reg, Regs0),
    Moves0 = gen_moves(As, Bef),
    Moves = order_moves(Moves0, find_scratch_reg(Regs)),
    {UnsavedMoves ++ Moves,Bef#sr{reg=Regs}}.

%% load_arg_regs([Reg], Arguments) -> [Reg]
%%  Update the register descriptor to include the arguments (from {x,0}
%%  and upwards). Values in argument register are overwritten.
%%  Values in x registers above the arguments are preserved.

load_arg_regs(Regs, As) -> load_arg_regs(Regs, As, 0).

load_arg_regs([_|Rs], [#k_var{name=V}|As], I) -> [{I,V}|load_arg_regs(Rs, As, I+1)];
load_arg_regs([_|Rs], [A|As], I) -> [{I,A}|load_arg_regs(Rs, As, I+1)];
load_arg_regs([], [#k_var{name=V}|As], I) -> [{I,V}|load_arg_regs([], As, I+1)];
load_arg_regs([], [A|As], I) -> [{I,A}|load_arg_regs([], As, I+1)];
load_arg_regs(Rs, [], _) -> Rs.

%% Returns the variables must be saved and are currently in the
%% x registers that are about to be overwritten by the arguments.

unsaved_registers(Regs, Stk, Fb, Lf, Vdb) ->
    [V || {V,F,L} <- Vdb,
	  F < Fb,
	  L >= Lf,
	  not on_stack(V, Stk),
	  not in_reg(V, Regs)].

in_reg(V, Regs) -> keymember(V, 2, Regs).

%% Move away unsaved variables from the registers that are to be
%% overwritten by the arguments.
move_unsaved(Vs, OrigRegs, NewRegs) ->
    move_unsaved(Vs, OrigRegs, NewRegs, []).
    
move_unsaved([V|Vs], OrigRegs, NewRegs0, Acc) ->
    NewRegs = put_reg(V, NewRegs0),
    Src = fetch_reg(V, OrigRegs),
    Dst = fetch_reg(V, NewRegs),
    move_unsaved(Vs, OrigRegs, NewRegs, [{move,Src,Dst}|Acc]);
move_unsaved([], _, Regs, Acc) -> {Acc,Regs}.
    
%% gen_moves(As, Sr)
%%  Generate the basic move instruction to move the arguments
%%  to their proper registers. The list will be sorted on
%%  destinations. (I.e. the move to {x,0} will be first --
%%  see the comment to order_moves/2.)

gen_moves(As, Sr) -> gen_moves(As, Sr, 0, []).

gen_moves([#k_var{name=V}|As], Sr, I, Acc) ->
    case fetch_var(V, Sr) of
	{x,I} -> gen_moves(As, Sr, I+1, Acc);
	Reg -> gen_moves(As, Sr, I+1, [{move,Reg,{x,I}}|Acc])
    end;
gen_moves([A0|As], Sr, I, Acc) ->
    A = atomic(A0),
    gen_moves(As, Sr, I+1, [{move,A,{x,I}}|Acc]);
gen_moves([], _, _, Acc) -> lists:keysort(3, Acc).

%% order_moves([Move], ScratchReg) -> [Move]
%%  Orders move instruction so that source registers are not
%%  destroyed before they are used. If there are cycles
%%  (such as {move,{x,0},{x,1}}, {move,{x,1},{x,1}}),
%%  the scratch register is used to break up the cycle.
%%    If possible, the first move of the input list is placed
%%  last in the result list (to make the move to {x,0} occur
%%  just before the call to allow the Beam loader to coalesce
%%  the instructions).

order_moves(Ms, Scr) -> order_moves(Ms, Scr, []).

order_moves([{move,_,_}=M|Ms0], ScrReg, Acc0) ->
    {Chain,Ms} = collect_chain(Ms0, [M], ScrReg),
    Acc = reverse(Chain, Acc0),
    order_moves(Ms, ScrReg, Acc);
order_moves([], _, Acc) -> Acc.

collect_chain(Ms, Path, ScrReg) ->
    collect_chain(Ms, Path, [], ScrReg).

collect_chain([{move,Src,Same}=M|Ms0], [{move,Same,_}|_]=Path, Others, ScrReg) ->
    case lists:keyfind(Src, 3, Path) of
	false ->
	    collect_chain(reverse(Others, Ms0), [M|Path], [], ScrReg);
	_ ->	% We have a cycle.
	    {break_up_cycle(M, Path, ScrReg),reverse(Others, Ms0)}
    end;
collect_chain([M|Ms], Path, Others, ScrReg) ->
    collect_chain(Ms, Path, [M|Others], ScrReg);
collect_chain([], Path, Others, _) ->
    {Path,Others}.

break_up_cycle({move,Src,_}=M, Path, ScrReg) ->
    [{move,ScrReg,Src},M|break_up_cycle1(Src, Path, ScrReg)].

break_up_cycle1(Dst, [{move,Src,Dst}|Path], ScrReg) ->
    [{move,Src,ScrReg}|Path];
break_up_cycle1(Dst, [M|Path], LastMove) ->
    [M|break_up_cycle1(Dst, Path, LastMove)].

%% clear_dead(Sr, Until, Vdb) -> Aft.
%%  Remove all variables in Sr which have died AT ALL so far.

clear_dead(#sr{stk=Stk}=Sr0, Until, Vdb) ->
    Sr = Sr0#sr{reg=clear_dead_reg(Sr0, Until, Vdb),
                stk=clear_dead_stk(Stk, Until, Vdb)},
    reserve(Sr).

clear_dead_reg(Sr, Until, Vdb) ->
    [case R of
         {_I,V} = IV ->
             case vdb_find(V, Vdb) of
                 {V,_,L} when L > Until -> IV;
                 _ -> free                      %Remove anything else
             end;
         {reserved,_I,_V}=Reserved -> Reserved;
         free -> free
     end || R <- Sr#sr.reg].

clear_dead_stk(Stk, Until, Vdb) ->
    [case S of
	    {V} = T ->
            case vdb_find(V, Vdb) of
                {V,_,L} when L > Until -> T;
                _ -> dead   %Remove anything else
            end;
        free -> free;
        dead -> dead
     end || S <- Stk].


%% sr_merge(Sr1, Sr2) -> Sr.
%%  Merge two stack/register states keeping the longest of both stack
%%  and register. Perform consistency check on both, elements must be
%%  the same.  Allow frame size 'void' to make easy creation of
%%  "empty" frame.

sr_merge(#sr{reg=R1,stk=S1,res=[]}, #sr{reg=R2,stk=S2,res=[]}) ->
    #sr{reg=longest(R1, R2),stk=longest(S1, S2),res=[]};
sr_merge(void, S2) -> S2#sr{res=[]}.

longest([H|T1], [H|T2]) -> [H|longest(T1, T2)];
longest([dead|T1], [free|T2]) -> [dead|longest(T1, T2)];
longest([free|T1], [dead|T2]) -> [dead|longest(T1, T2)];
longest([dead|_] = L, []) -> L;
longest([], [dead|_] = L) -> L;
longest([free|_] = L, []) -> L;
longest([], [free|_] = L) -> L;
longest([], []) -> [].

trim_free([R|Rs0]) ->
    case {trim_free(Rs0),R} of
	{[],free} -> [];
	{Rs,R} -> [R|Rs]
    end;
trim_free([]) -> [].

%% maybe_adjust_stack(Bef, FirstBefore, LastFrom, Vdb, St) -> {[Ainstr],Aft}.
%%  Adjust the stack, but only if the code is inside a catch and not
%%  inside a guard.  Use this funtion before instructions that may
%%  cause an exception.

maybe_adjust_stack(Bef, Fb, Lf, Vdb, St) ->
    case St of
	#cg{in_catch=true,bfail=0} ->
	    adjust_stack(Bef, Fb, Lf, Vdb);
	#cg{} ->
	    {[],Bef}
    end.

%% adjust_stack(Bef, FirstBefore, LastFrom, Vdb) -> {[Ainstr],Aft}.
%%  Do complete stack adjustment by compressing stack and adding
%%  variables to be saved.  Try to optimise ordering on stack by
%%  having reverse order to their lifetimes.
%%
%%  In Beam, there is a fixed stack frame and no need to do stack compression.

adjust_stack(Bef, Fb, Lf, Vdb) ->
    Stk0 = Bef#sr.stk,
    {Stk1,Saves} = save_stack(Stk0, Fb, Lf, Vdb),
    {saves(Saves, Bef#sr.reg, Stk1),
     Bef#sr{stk=Stk1}}.

%% save_stack(Stack, FirstBefore, LastFrom, Vdb) -> {[SaveVar],NewStack}.
%%  Save variables which are used past current point and which are not
%%  already on the stack.

save_stack(Stk0, Fb, Lf, Vdb) ->
    %% New variables that are in use but not on stack.
    New = new_not_on_stack(Stk0, Fb, Lf, Vdb),

    %% Add new variables that are not just dropped immediately.
    %% N.B. foldr works backwards from the end!!
    Saves = [V || {V,_,_} <- keysort(3, New)],
    Stk1 = foldr(fun (V, Stk) -> put_stack(V, Stk) end, Stk0, Saves),
    {Stk1,Saves}.

%% new_not_on_stack(Stack, FirstBefore, LastFrom, Vdb) ->
%%                 [{Variable,First,Last}]
%%  Return information about all variables that are used past current
%%  point and that are not already on the stack.

new_not_on_stack(Stk, Fb, Lf, Vdb) ->
    [VFL || {V,F,L} = VFL <- Vdb,
            F < Fb,
            L >= Lf,
            not on_stack(V, Stk)].

%% saves([SaveVar], Reg, Stk) -> [{move,Reg,Stk}].
%%  Generate move instructions to save variables onto stack.  The
%%  stack/reg info used is that after the new stack has been made.

saves(Ss, Reg, Stk) ->
    [{move,fetch_reg(V, Reg),fetch_stack(V, Stk)} || V <- Ss].

%% fetch_var(VarName, StkReg) -> r{R} | sp{Sp}.
%% find_var(VarName, StkReg) -> ok{r{R} | sp{Sp}} | error.
%%  Fetch/find a variable in either the registers or on the
%%  stack. Fetch KNOWS it's there.

fetch_var(V, Sr) ->
    case find_reg(V, Sr#sr.reg) of
	{ok,R} -> R;
	error -> fetch_stack(V, Sr#sr.stk)
    end.

fetch_var_prefer_y(V, #sr{reg=Reg,stk=Stk}) ->
    case find_stack(V, Stk) of
	{ok,R} -> R;
	error -> fetch_reg(V, Reg)
    end.

load_vars(Vs, Regs) ->
    foldl(fun (#k_var{name=V}, Rs) -> put_reg(V, Rs) end, Regs, Vs).

%% put_reg(Val, Regs) -> Regs.
%% find_reg(Val, Regs) -> {ok,r{R}} | error.
%% fetch_reg(Val, Regs) -> r{R}.
%%  Functions to interface the registers.

% put_regs(Vs, Rs) -> foldl(fun put_reg/2, Rs, Vs).

put_reg(V, Rs) -> put_reg_1(V, Rs, 0).

put_reg_1(V, [free|Rs], I) -> [{I,V}|Rs];
put_reg_1(V, [{reserved,I,V}|Rs], I) -> [{I,V}|Rs];
put_reg_1(V, [R|Rs], I) -> [R|put_reg_1(V, Rs, I+1)];
put_reg_1(V, [], I) -> [{I,V}].

fetch_reg(V, [{I,V}|_]) -> {x,I};
fetch_reg(V, [_|SRs]) -> fetch_reg(V, SRs).

find_reg(V, [{I,V}|_]) -> {ok,{x,I}};
find_reg(V, [_|SRs]) -> find_reg(V, SRs);
find_reg(_, []) -> error.

%% For the bit syntax, we need a scratch register if we are constructing
%% a binary that will not be used.

find_scratch_reg(Rs) -> find_scratch_reg(Rs, 0).
    
find_scratch_reg([free|_], I) -> {x,I};
find_scratch_reg([_|Rs], I) -> find_scratch_reg(Rs, I+1);
find_scratch_reg([], I) -> {x,I}.

replace_reg_contents(Old, New, [{I,Old}|Rs]) -> [{I,New}|Rs];
replace_reg_contents(Old, New, [R|Rs]) -> [R|replace_reg_contents(Old, New, Rs)].

%%clear_regs(Regs) -> map(fun (R) -> free end, Regs).
clear_regs(_) -> [].

max_reg(Regs) ->
    foldl(fun ({I,_}, _) -> I;
	      (_, Max) -> Max end,
	  -1, Regs) + 1.

%% put_stack(Val, [{Val}]) -> [{Val}].
%% fetch_stack(Var, Stk) -> sp{S}.
%% find_stack(Var, Stk) -> ok{sp{S}} | error.
%%  Functions to interface the stack.

put_stack(Val, []) -> [{Val}];
put_stack(Val, [dead|Stk]) -> [{Val}|Stk];
put_stack(Val, [free|Stk]) -> [{Val}|Stk];
put_stack(Val, [NotFree|Stk]) -> [NotFree|put_stack(Val, Stk)].

put_stack_carefully(Val, Stk0) ->
    try
	put_stack_carefully1(Val, Stk0)
    catch
	throw:error ->
	    error
    end.

put_stack_carefully1(_, []) -> throw(error);
put_stack_carefully1(Val, [dead|Stk]) -> [{Val}|Stk];
put_stack_carefully1(Val, [free|Stk]) -> [{Val}|Stk];
put_stack_carefully1(Val, [NotFree|Stk]) ->
    [NotFree|put_stack_carefully1(Val, Stk)].

fetch_stack(Var, Stk) -> fetch_stack(Var, Stk, 0).

fetch_stack(V, [{V}|_], I) -> {yy,I};
fetch_stack(V, [_|Stk], I) -> fetch_stack(V, Stk, I+1).

find_stack(Var, Stk) -> find_stack(Var, Stk, 0).

find_stack(V, [{V}|_], I) -> {ok,{yy,I}};
find_stack(V, [_|Stk], I) -> find_stack(V, Stk, I+1);
find_stack(_, [], _) -> error.

on_stack(V, Stk) -> keymember(V, 1, Stk).

%% put_catch(CatchTag, Stack) -> Stack'
%% drop_catch(CatchTag, Stack) -> Stack'
%%  Special interface for putting and removing catch tags, to ensure that
%%  catches nest properly. Also used for try tags.

put_catch(Tag, Stk0) -> put_catch(Tag, reverse(Stk0), []).

put_catch(Tag, [], Stk) ->
    put_stack({catch_tag,Tag}, Stk);
put_catch(Tag, [{{catch_tag,_}}|_]=RevStk, Stk) ->
    reverse(RevStk, put_stack({catch_tag,Tag}, Stk));
put_catch(Tag, [Other|Stk], Acc) ->
    put_catch(Tag, Stk, [Other|Acc]).

drop_catch(Tag, [{{catch_tag,Tag}}|Stk]) -> [free|Stk];
drop_catch(Tag, [Other|Stk]) -> [Other|drop_catch(Tag, Stk)].

%% atomic(Klit) -> Lit.
%% atomic_list([Klit]) -> [Lit].

atomic(#k_literal{val=V}) -> {literal,V};
atomic(#k_int{val=I}) -> {integer,I};
atomic(#k_float{val=F}) -> {float,F};
atomic(#k_atom{val=A}) -> {atom,A};
%%atomic(#k_char{val=C}) -> {char,C};
atomic(#k_nil{}) -> nil.

%% new_label(St) -> {L,St}.

new_label(#cg{lcount=Next}=St) ->
    {Next,St#cg{lcount=Next+1}}.

%% line(Le) -> {line,[] | {location,File,Line}}
%%  Create a line instruction, containing information about
%%  the current filename and line number. A line information
%%  instruction should be placed before any operation that could
%%  cause an exception.

line(#l{a=Anno}) ->
    line(Anno);
line([Line,{file,Name}]) when is_integer(Line) ->
    line_1(Name, Line);
line([_|_]=A) ->
    {Name,Line} = find_loc(A, no_file, 0),
    line_1(Name, Line);
line([]) ->
    {line,[]}.

line_1(no_file, _) ->
    {line,[]};
line_1(_, 0) ->
    %% Missing line number or line number 0.
    {line,[]};
line_1(Name, Line) ->
    {line,[{location,Name,Line}]}.

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

%% Keep track of life time for variables.
%%
%% init_vars([{var,VarName}]) -> Vdb.
%% new_vars([VarName], I, Vdb) -> Vdb.
%% use_vars([VarName], I, Vdb) -> Vdb.
%% add_var(VarName, F, L, Vdb) -> Vdb.
%%
%% The list of variable names for new_vars/3 and use_vars/3
%% must be sorted.

init_vars(Vs) ->
    vdb_new(Vs).

new_vars([], _, Vdb) -> Vdb;
new_vars([V], I, Vdb) -> vdb_store_new(V, {V,I,I}, Vdb);
new_vars(Vs, I, Vdb) -> vdb_update_vars(Vs, Vdb, I).

use_vars([], _, Vdb) ->
    Vdb;
use_vars([V], I, Vdb) ->
    case vdb_find(V, Vdb) of
        {V,F,L} when I > L -> vdb_update(V, {V,F,I}, Vdb);
        {V,_,_} -> Vdb;
        error -> vdb_store_new(V, {V,I,I}, Vdb)
    end;
use_vars(Vs, I, Vdb) -> vdb_update_vars(Vs, Vdb, I).

add_var(V, F, L, Vdb) ->
    vdb_store_new(V, {V,F,L}, Vdb).

%% vdb

vdb_new(Vs) ->
    ordsets:from_list([{V,0,0} || #k_var{name=V} <- Vs]).

-type var() :: atom().

-spec vdb_find(var(), [vdb_entry()]) -> 'error' | vdb_entry().

vdb_find(V, Vdb) ->
    case lists:keyfind(V, 1, Vdb) of
        false -> error;
        Vd -> Vd
    end.

vdb_update(V, Update, [{V,_,_}|Vdb]) ->
    [Update|Vdb];
vdb_update(V, Update, [Vd|Vdb]) ->
    [Vd|vdb_update(V, Update, Vdb)].

vdb_store_new(V, New, [{V1,_,_}=Vd|Vdb]) when V > V1 ->
    [Vd|vdb_store_new(V, New, Vdb)];
vdb_store_new(V, New, [{V1,_,_}|_]=Vdb) when V < V1 ->
    [New|Vdb];
vdb_store_new(_, New, []) -> [New].

vdb_update_vars([V|_]=Vs, [{V1,_,_}=Vd|Vdb], I) when V > V1 ->
    [Vd|vdb_update_vars(Vs, Vdb, I)];
vdb_update_vars([V|Vs], [{V1,_,_}|_]=Vdb, I) when V < V1 ->
    %% New variable.
    [{V,I,I}|vdb_update_vars(Vs, Vdb, I)];
vdb_update_vars([V|Vs], [{_,F,L}=Vd|Vdb], I) ->
    %% Existing variable.
    if
        I > L -> [{V,F,I}|vdb_update_vars(Vs, Vdb, I)];
        true ->  [Vd|vdb_update_vars(Vs, Vdb, I)]
    end;
vdb_update_vars([V|Vs], [], I) ->
    %% New variable.
    [{V,I,I}|vdb_update_vars(Vs, [], I)];
vdb_update_vars([], Vdb, _) -> Vdb.

%% vdb_sub(Min, Max, Vdb) -> Vdb.
%%  Extract variables which are used before and after Min.  Lock
%%  variables alive after Max.

vdb_sub(Min, Max, Vdb) ->
    [ if L >= Max -> {V,F,locked};
         true -> Vd
      end || {V,F,L}=Vd <- Vdb,
             F < Min,
             L >= Min ].
