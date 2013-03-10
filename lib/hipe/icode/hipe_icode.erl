%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HiPE Intermediate Code
%% ====================================================================
%%  Filename : 	hipe_icode.erl
%%  Module   :	hipe_icode
%%  Purpose  :  Provide primops for the Icode data structure.
%%  History  :	1997-? Erik Johansson (happi@it.uu.se): Created.
%%              2001-01-30 EJ (happi@it.uu.se):
%%                             Apply, primop, guardop removed
%%              2003-03-15 ES (happi@acm.org):
%%                             Started commenting in Edoc.
%%                             Moved pretty printer to separate file.
%%
%% $Id$
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%@doc
%% This module implements "Linear Icode" and Icode instructions. 
%%          
%% <p> Icode is a simple (in that it has few instructions) imperative
%% language, used as the first Intermediate Code in the HiPE compiler.
%% Icode is closely related to Erlang, and Icode instructions operate
%% on Erlang terms. </p>
%%
%% <h2><a href="#type-icode">Icode</a></h2>
%%
%% <p> Linear Icode for a function consists of:
%%     <ul>
%%       <li> the function's name (`{M,F,A}'), </li>
%%       <li> a list of parameters, </li>
%%       <li> a list of instructions, </li>
%%       <li> data, </li>
%%       <li> information about whether the function is a leaf function, </li>
%%       <li> information about whether the function is a closure, and </li>
%%       <li> the range for labels and variables in the code. </li>
%%     </ul>
%% </p>
%%
%% <h2><a href="#type-icode_instruction">Icode Instructions</a> (and
%% their components)</h2>
%%
%% Control flow:
%% <dl>
%%    <dt><code><a href="#type-if">'if'</a> 
%%          {Cond::<a href="#type-cond">cond()</a>, 
%%           Args::[<a href="#type-arg">arg()</a>],
%%           TrueLabel::<a href="#type-label_name">label_name()</a>, 
%%           FalseLabel::<a href="#type-label_name">label_name()</a>
%%          } :: 
%%           <a href="#type-icode_instruction">icode_instr()</a></code></dt>
%%    <dd>
%%        The if instruction compares the arguments (Args) with
%%        condition (Cond) and jumps to either TrueLabel or
%%        FalseLabel. (At the moment...) There are only binary
%%        conditions so the number of arguments should be two.
%%        <p>
%%        An if instructions ends a basic block and should be followed
%%        by a label (or be the last instruction of the code).
%%        </p></dd>
%%
%%    <dt><code><a href="#type-switch_val">switch_val</a> 
%%                    {Term::<a href="#type-arg">var()</a>, 
%%                     FailLabel::<a href="#type-label_name">label_name()</a>, 
%%                     Length::integer(), 
%%                     Cases::[{<a href="#type-symbol">symbol()</a>,<a
%%                     href="#type-label_name">label_name()</a>}]
%%		      }::
%%           <a href="#type-icode_instruction">icode_instr()</a></code></dt>
%%    <dd>
%%        The switch_val instruction compares the argument Term to the
%%        symbols in the lists Cases, control is transfered to the label
%%        that corresponds to the first symbol that matches.  If no
%%        symbol matches control is transfered to FailLabel.  (NOTE: The
%%        length argument is not currently in use.)
%%        <p>
%%        The switch_val instruction can be assumed to be implemented as
%%        efficiently as possible given the symbols in the case
%%        list. (Jump-table, bianry-serach, or nested ifs)
%%        </p><p>
%%        A switch_val instructions ends a basic block and should be
%%        followed by a label (or be the last instruction of the code).
%%        </p></dd>
%%
%%    <dt><code><a href="#type-switch_tuple_arity">switch_tuple_arity</a>
%%         {Term::<a href="#type-arg">var()</a>, 
%%          FailLabel::<a href="#type-label_name">label_name()</a>, 
%%          Length::integer(),  
%%          Cases::[{integer(),<a href="#type-label_name">label_name()</a>}]
%%         }::
%%           <a href="#type-icode_instruction">icode_instr()</a></code></dt>
%%    <dd>
%%        The switch_tuple_arity instruction compares the size of the
%%        tuple in the argument Term to the integers in the lists Cases,
%%        control is transfered to the label that corresponds to the
%%        first integer that matches.  If no integer matches control is
%%        transfered to FailLabel.  (NOTE: The length argument is not
%%        currently in use.)
%%        <p>
%%        The switch_tuple_arity instruction can be assumed to be
%%        implemented as efficently as possible given the symbols in the
%%        case list. (Jump-table, bianry-serach, or nested ifs)
%%        </p><p>
%%        A switch_tuple_arity instructions ends a basic block and
%%        should be followed by a label (or be the last instruction of
%%        the code).
%%        </p></dd>
%%
%%    <dt>`type {typ_expr, arg, true_label, false_label}}'</dt>
%%    <dt>`goto {label}'</dt>
%%    <dt>`label {name}'</dt>
%% </dl>
%%
%% Moves:
%% <dl>
%%    <dt>`move {dst, src}'</dt>
%%    <dt>`phi {dst, arglist}'</dt>
%% </dl>
%%
%% Function application:
%% <dl>
%%    <dt>`call {[dst], fun, [arg], type, continuation, fail,
%%               in_guard}'</dt>
%%    <dd>
%%        Where `type' is one of {`local', `remote', `primop'}
%%        and `in_guard' is either `true' or `false'.</dd>
%%    <dt>`enter {fun, [arg], type}'</dt>
%%    <dd>
%%        Where `type' is one of {`local', `remote', `primop'}
%%        and `in_guard' is either `true' or `false'.</dd>
%%    <dt>`return {[var]}'</dt>
%%    <dd>
%%        <strong>WARNING:</strong> Multiple return values are yet not
%%        fully implemented and tested.
%%    </dd>
%% </dl>
%%
%% Error handling:
%% <dl>
%%    <dt>`begin_try {label, successor}'</dt>
%%    <dt>`end_try'</dt>
%%    <dt>`begin_handler {dstlist}'</dt>
%%    <dt>`fail {Args, Class}'</dt>
%%    <dd>Where `Class' is one of 
%%      {`exit', `throw', `error', `rethrow'}. For `error/2', `[args]'
%%      is `[Reason,Trace]'. For `rethrow', `Args' is
%%      `[Exception,Reason]' - this only occurs in autogenerated code.
%%    </dd>
%% </dl>
%%
%% Comments:
%% <dl>
%%    <dt>`comment{Text::string()}'</dt>
%% </dl>
%%
%% <h4>Notes</h4>
%%
%%  <p> A constant can only show up on the RHS of a `move' instruction
%%      and in `if' and `switch_*'</p>
%%  <p>
%%      Classification of primops should be like this:
%%      <ul>
%%      <li> `erlang:exit/1, erlang:throw/1, erlang:error/1,
%%            erlang:error/2, erlang:fault/1',
%%           and `erlang:fault/2' should use the
%%           {@link fail(). fail-instruction} in Icode.</li>
%%      <li> Calls or tail-recursive calls to BIFs, operators, or internal
%%           functions should be implemented with `call' or `enter' 
%%           respectively, with the primop flag set.</li>
%%      <li> All other Erlang functions should be implemented with `call'
%%           or `enter' respectively, without the primop flag set.</li>
%%      </ul>
%%  </p>
%%
%% <h4>Primops</h4>
%%
%% <pre>
%%  Constructors:
%%    cons                       - [Car, Cdr]
%%    mktuple                    - [Element1, Element2, ..., ElementN]
%%    call_fun                   - [BoundArg1, ..., BoundArgN, Fun]
%%    enter_fun                  - [BoundArg1, ..., BoundArgN, Fun]
%%    #mkfun{}                   - [FreeVar1, FreeVar2, ..., FreeVarN]
%%
%%  Binaries:
%%    bs_init
%%    {bs_put_string, Bytes, Size}
%%    bs_final
%%
%%  Selectors:
%%    element                    - [Index, Tuple]
%%    unsafe_hd                  - [List]
%%    unsafe_tl                  - [List]
%%    #unsafe_element{}          - [Tuple]
%%    #unsafe_update_element{}   - [Tuple, Val]
%%    #closure_element{}         - [Fun]
%%
%%  Arithmetic:       [Arg1, Arg2]
%%    '+', '-', '*', '/', 'div', 'rem',
%%    'band', 'bor', 'bxor', 'bnot', 'bsl', 'bsr'
%%
%%  Receive:         
%%    check_get_msg - []
%%    next_msg      - []
%%    select_msg    - []
%%    set_timeout   - [Timeout]
%%    clear_timeout - []
%%    suspend_msg   - []
%%
%% </pre>
%%
%% <h4>Guardops: (primops that can be used in guards and can fail)</h4>
%%  <pre>
%%  Selectors:
%%    unsafe_hd         - [List]
%%    unsafe_tl         - [List]
%%    #element{}        - [Index, Tuple]
%%    #unsafe_element{} - [Tuple]
%%
%%  Arithmetic:       [Arg1, Arg2]
%%    '+', '-', '*', '/', 'div', 'rem',
%%   'band', 'bor', 'bxor', 'bnot', 'bsl', 'bsr',
%%    fix_add, fix_sub               %% Do these exist?
%%
%%  Concurrency:
%%    {erlang,self,0}          - []
%% </pre>
%%
%%
%% <h4>Relational Operations (Cond in if instruction)</h4>
%% <pre>
%%    gt, lt, geq, leq,
%%    eqeq, neq, exact_eqeq, exact_neq
%% </pre>
%%
%% <h4>Type tests</h4>
%% <pre>
%%    list
%%    nil
%%    cons
%%    tuple
%%    {tuple, N}
%%    atom
%%    {atom, Atom}
%%    number
%%    integer
%%    {integer, N}
%%    fixnum
%%    bignum
%%    float
%%    pid
%%    port
%%    {record, Atom, Size}
%%    reference
%%    binary
%%    function
%% </pre>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%=====================================================================

-module(hipe_icode).

-include("../main/hipe.hrl").
-include("hipe_icode.hrl").

%% @type icode(Fun, Params, IsClosure, IsLeaf, Code, Data, VarRange,LabelRange)
%%    Fun = mfa()
%%    Params = [var()]
%%    IsClosure = boolean()
%%    IsLeaf = boolean()
%%    Code = [icode_instr()]
%%    Data = data()
%%    VarRange = {integer(),integer()}
%%    LabelRange = {integer(),integer()}
%%
%% @type icode_instr(I) 
%%    I = if() | switch_val() | switch_tuple_arity() | type() | goto()
%%      | label() | move() | phi() | call() | enter() | return() 
%%      | begin_try() | end_try() | begin_handler() | fail() | comment()
%%
%% @type if(Cond, Args, TrueLabel, FalseLabel)
%%    Cond = cond()
%%    Args = [arg()]
%%    TrueLabel = label_name()
%%    FalseLabel = label_name()
%%
%% @type switch_val(Term, FailLabel, Length, Cases) 
%%    Term = var()
%%    FailLabel = label_name()
%%    Length = integer()
%%    Cases = [{symbol(),label_name()}]
%%
%% @type switch_tuple_arity(Arg, FailLabel, Length, Cases)
%%    Term = var()
%%    FailLabel = label_name()
%%    Length = integer()
%%    Cases = [{symbol(),label_name()}]
%%
%% @type type(TypeTest, Arg, True_label, False_label)
%%    TypeTest = type_test()
%%    Args = [arg()]
%%    TrueLabel = label_name()
%%    FalseLabel = label_name()
%%
%% @type goto(Label) Label = label_name()
%%
%% @type label(Name) Name = label_name()
%%
%% @type move(Dst, Src) Dst = var() Src = arg()
%%
%% @type phi(Dst, Id, Arglist) 
%%    Dst = var() | fvar()
%%    Id = var() | fvar()
%%    Arglist = [{Pred, Src}]
%%    Pred = label_name()
%%    Src = var() | fvar() 
%%
%% @type call(Dst, Fun, Arg, Type, Continuation, FailLabel, InGuard)
%%    Dst = [var()]
%%    Fun = mfa() | primop() | closure() 
%%    Arg = [var()]
%%    Type = call_type()
%%    Continuation = [] | label_name()
%%    FailLabel = [] | label_name()
%%    InGuard = boolean()
%%
%% @type enter(Fun, Arg, Type)
%%    Fun = mfa() | primop() | closure() 
%%    Arg = [var()] 
%%    Type = call_type()
%%
%% @type return (Vars) Vars = [var()]
%%
%% @type begin_try(FailLabel, Successor)
%%    FailLabel = label_name()
%%    Successor = label_name()
%%
%% @type end_try()
%%
%% @type begin_handler(Dst) 
%%    Dst = [var()]
%%
%% @type fail(Class, Args, Label)
%%    Class = exit_class()
%%    Args = [var()]
%%    Label = label_name()
%%
%% @type comment(Text) Text = string()

%% @type call_type()  = 'local' | 'remote' | 'primop'
%% @type exit_class() = 'exit' | 'throw' | 'error' | 'rethrow'
%% @type cond() = gt | lt | geq | leq | eqeq | neq | exact_eqeq | exact_neq
%% @type type_test() = 
%%      list
%%    | nil
%%    | cons
%%    | tuple
%%    | {tuple, integer()}
%%    | atom
%%    | {atom, atom()}
%%    | number
%%    | integer
%%    | {integer, integer()}
%%    | fixnum
%%    | bignum
%%    | float
%%    | pid
%%    | port
%%    | {record, atom(), integer()}
%%    | reference
%%    | binary
%%    | function
%%
%% @type mfa(Mod,Fun,Arity) = {atom(),atom(),arity()}

%% @type arg() = var() | const()
%% @type farg() = fvar() | float()
%% @type var(Name) Name = integer()
%% @type fvar(Name) Name = integer()
%% @type label_name(Name) Name = integer()
%% @type symbol(S) = atom() | number()
%% @type const(C)  C = immediate()
%% @type immediate(I) = I
%%    I = term()
%% @end


%% ____________________________________________________________________
%%
%% Exports
%%
-export([mk_icode/7, %% mk_icode(Fun, Params, IsClosure, IsLeaf, 
		     %%          Code, VarRange, LabelRange)
	 mk_icode/8, %% mk_icode(Fun, Params, IsClosure, IsLeaf, 
		     %%          Code, Data, VarRange, LabelRange)
	 icode_fun/1,
	 icode_params/1,
	 icode_params_update/2,
	 icode_is_closure/1,
	 icode_closure_arity/1,
	 icode_closure_arity_update/2,
	 icode_is_leaf/1,
	 icode_code/1,
	 icode_code_update/2,
	 icode_data/1,
	 %% icode_data_update/2,
	 icode_var_range/1,
	 icode_label_range/1,
	 icode_info/1,
	 icode_info_update/2]).

-export([mk_if/4,           %% mk_if(Op, Args, TrueLbl, FalseLbl)
	 %% mk_if/5,	    %% mk_if(Op, Args, TrueLbl, FalseLbl, Prob)
	 if_op/1,
	 if_op_update/2,
	 if_true_label/1,
	 if_false_label/1,
	 if_args/1,
	 if_pred/1,
	 %% is_if/1,
	 
	 mk_switch_val/4,
	 %% mk_switch_val/5,
	 switch_val_term/1,
	 switch_val_fail_label/1,	
	 %% switch_val_length/1,
	 switch_val_cases/1,
	 switch_val_cases_update/2,
	 %% is_switch_val/1,
	 
	 mk_switch_tuple_arity/4,
	 %% mk_switch_tuple_arityl/5,
	 switch_tuple_arity_term/1,
	 switch_tuple_arity_fail_label/1,
	 switch_tuple_arity_fail_label_update/2,
	 %% switch_tuple_arity_length/1,
	 switch_tuple_arity_cases/1,
	 switch_tuple_arity_cases_update/2,
	 %% is_switch_tuple_arity/1,

	 mk_type/4,        %% mk_type(Args, Type, TrueLbl, FalseLbl)
	 mk_type/5,	   %% mk_type(Args, Type, TrueLbl, FalseLbl, P)
	 type_args/1,
	 %% type_args_update/2,
	 type_test/1,
	 type_true_label/1,
	 type_false_label/1,
	 type_pred/1,
	 is_type/1,

	 mk_guardop/5,     %% mk_guardop(Dst, Fun, Args, Continuation, Fail)
	 mk_primop/3,      %% mk_primop(Dst, Fun, Args)
	 mk_primop/5,      %% mk_primop(Dst, Fun, Args, Cont, Fail)
	 mk_call/5,	   %% mk_call(Dst, Mod, Fun, Args, Type)
	 %% mk_call/7,	   %% mk_call(Dst, Mod, Fun, Args, Type,
	                   %%         Continuation, Fail)
	 mk_call/8,	   %% mk_call(Dst, Mod, Fun, Args, Type,
	                   %%         Continuation, Fail, Guard)
	 call_dstlist/1,
	 call_dstlist_update/2,
	 %% call_dst_type/1,
	 call_args/1,
	 call_args_update/2,
	 call_fun/1,
	 call_fun_update/2,
	 call_type/1,
	 call_continuation/1,
	 call_fail_label/1,
	 call_set_fail_label/2,
	 call_set_continuation/2,
	 is_call/1, 
	 call_in_guard/1,

	 mk_goto/1,              %% mk_goto(Lbl)
	 goto_label/1,
	 
	 mk_enter/4,             %% mk_enter(Mod, Fun, Args, Type)
	 mk_enter_primop/2,      %% mk_enter_primop(Op, Type)
	 enter_fun/1,
	 enter_fun_update/2,
	 enter_args/1,
	 enter_args_update/2,
	 enter_type/1,
	 is_enter/1,
	 

	 mk_return/1,            %% mk_return(Vars)
	 %% mk_fail/1,	         %% mk_fail(Args) class = exit
	 mk_fail/2,              %% mk_fail(Args, Class)
	 %% mk_fail/3,           %% mk_fail(Args, Class, Label)
	 mk_move/2,              %% mk_move(Dst, Src)
	 %% mk_moves/2,		 %% mk_moves(DstList, SrcList)
	 mk_begin_try/2,         %% mk_begin_try(Label, Successor)
	 mk_begin_handler/1,     %% mk_begin_handler(ReasonDst)
	 mk_end_try/0,           %% mk_end_try()
	 %% mk_elements/2,       %% mk_elements(Tuple, Vars)
	 mk_label/1,             %% mk_label(Name)
	 mk_new_label/0,         %% mk_new_label()
	 mk_comment/1,           %% mk_comment(Text)
	 mk_const/1,             %% mk_const(Const)
	 mk_var/1,               %% mk_var(Id)
	 annotate_variable/2,  %% annotate_var_or_reg(VarOrReg, Type)
	 unannotate_variable/1,%% unannotate_var_or_reg(VarOrReg)
	 mk_reg/1,               %% mk_reg(Id)
	 mk_fvar/1,              %% mk_fvar(Id)
	 mk_new_var/0,           %% mk_new_var()
	 mk_new_fvar/0,          %% mk_new_fvar()
	 mk_new_reg/0,           %% mk_new_reg()
	 mk_phi/1,               %% mk_phi(Id)
	 mk_phi/2                %% mk_phi(Id, ArgList)
	]).

%%
%% Identifiers
%%

-export([%% is_fail/1,
	 is_return/1,
	 is_move/1,
	 %% is_begin_try/1,
	 %% is_begin_handler/1,
	 %% is_end_try/1,
	 is_goto/1,
	 is_label/1,
	 is_comment/1,
	 is_const/1,
	 is_var/1,
	 is_fvar/1,
	 is_reg/1,
	 is_variable/1,
	 is_annotated_variable/1,
	 %% is_uncond/1,
         is_phi/1]).

%%
%% Selectors
%%

-export([phi_dst/1,
         phi_id/1,
         %% phi_args/1,
         phi_arg/2,
         phi_arglist/1,
	 phi_enter_pred/3,
	 phi_remove_pred/2,
	 phi_redirect_pred/3,
	 move_dst/1,
	 move_src/1,
	 move_src_update/2,
	 begin_try_label/1,
	 begin_try_successor/1,
	 begin_handler_dstlist/1,
	 label_name/1,
	 comment_text/1,
	 return_vars/1,
	 fail_args/1,
	 fail_class/1,
	 fail_label/1,
	 fail_set_label/2,
	 var_name/1,
	 variable_annotation/1,
	 fvar_name/1,
	 reg_name/1,		 
	 reg_is_gcsafe/1,
	 const_value/1
	]).

%%
%% Misc
%%

-export([args/1,
	 uses/1,
	 defines/1,
	 is_safe/1,
	 strip_comments/1,
	 subst/2,
	 subst_uses/2,
	 subst_defines/2,
	 redirect_jmp/3,
	 successors/1,
	 fails_to/1,
	 is_branch/1
	]).

-export([highest_var/1, highest_label/1]).

%%---------------------------------------------------------------------
%% 
%% Icode
%%
%%---------------------------------------------------------------------

-spec mk_icode(mfa(), [icode_var()], boolean(), boolean(), [icode_instr()],
	       {non_neg_integer(),non_neg_integer()}, 
	       {icode_lbl(),icode_lbl()}) -> #icode{}.
mk_icode(Fun, Params, IsClosure, IsLeaf, Code, VarRange, LabelRange) ->
  #icode{'fun'=Fun, params=Params, code=Code,
	 is_closure=IsClosure,
	 is_leaf=IsLeaf,
	 data=hipe_consttab:new(),
	 var_range=VarRange,
	 label_range=LabelRange}.

-spec mk_icode(mfa(), [icode_var()], boolean(), boolean(), [icode_instr()],
	       hipe_consttab(), {non_neg_integer(),non_neg_integer()}, 
	       {icode_lbl(),icode_lbl()}) -> #icode{}.
mk_icode(Fun, Params, IsClosure, IsLeaf, Code, Data, VarRange, LabelRange) ->
  #icode{'fun'=Fun, params=Params, code=Code,
	 data=Data, is_closure=IsClosure, is_leaf=IsLeaf,
	 var_range=VarRange, label_range=LabelRange}.

-spec icode_fun(#icode{}) -> mfa().
icode_fun(#icode{'fun' = MFA}) -> MFA.

-spec icode_params(#icode{}) -> [icode_var()].
icode_params(#icode{params = Params}) -> Params.

-spec icode_params_update(#icode{}, [icode_var()]) -> #icode{}.
icode_params_update(Icode, Params) -> 
  Icode#icode{params = Params}.

-spec icode_is_closure(#icode{}) -> boolean().
icode_is_closure(#icode{is_closure = Closure}) -> Closure.

-spec icode_is_leaf(#icode{}) -> boolean().
icode_is_leaf(#icode{is_leaf = Leaf}) -> Leaf.

-spec icode_code(#icode{}) -> icode_instrs().
icode_code(#icode{code = Code}) -> Code.

-spec icode_code_update(#icode{}, icode_instrs()) -> #icode{}.
icode_code_update(Icode, NewCode) -> 
  Vmax = highest_var(NewCode),
  Lmax = highest_label(NewCode),
  Icode#icode{code = NewCode, var_range = {0,Vmax}, label_range = {0,Lmax}}.

-spec icode_data(#icode{}) -> hipe_consttab().
icode_data(#icode{data=Data}) -> Data.

%% %% -spec icode_data_update(#icode{}, hipe_consttab()) -> #icode{}.
%% icode_data_update(Icode, NewData) -> Icode#icode{data=NewData}.

-spec icode_var_range(#icode{}) -> {non_neg_integer(), non_neg_integer()}.
icode_var_range(#icode{var_range = VarRange}) -> VarRange.

-spec icode_label_range(#icode{}) -> {non_neg_integer(), non_neg_integer()}.
icode_label_range(#icode{label_range = LabelRange}) -> LabelRange.

-spec icode_info(#icode{}) -> icode_info().
icode_info(#icode{info = Info}) -> Info.

-spec icode_info_update(#icode{}, icode_info()) -> #icode{}.
icode_info_update(Icode, Info) -> Icode#icode{info = Info}.

-spec icode_closure_arity(#icode{}) -> arity().
icode_closure_arity(#icode{closure_arity = Arity}) -> Arity.

-spec icode_closure_arity_update(#icode{}, arity()) -> #icode{}.
icode_closure_arity_update(Icode, Arity) -> Icode#icode{closure_arity = Arity}.
  

%%----------------------------------------------------------------------------
%% Instructions
%%----------------------------------------------------------------------------

%%----
%% if
%%----

-spec mk_if(icode_if_op(), [icode_term_arg()],
	    icode_lbl(), icode_lbl()) -> #icode_if{}.
mk_if(Op, Args, TrueLbl, FalseLbl) ->
  #icode_if{op=Op, args=Args, true_label=TrueLbl, false_label=FalseLbl, p=0.5}.
%% mk_if(Op, Args, TrueLbl, FalseLbl, P) ->
%%  #icode_if{op=Op, args=Args, true_label=TrueLbl, false_label=FalseLbl, p=P}.

-spec if_op(#icode_if{}) -> icode_if_op().
if_op(#icode_if{op=Op}) -> Op.

-spec if_op_update(#icode_if{}, icode_if_op()) -> #icode_if{}.
if_op_update(IF, NewOp) -> IF#icode_if{op=NewOp}.

-spec if_args(#icode_if{}) -> [icode_term_arg()].
if_args(#icode_if{args=Args}) -> Args.

-spec if_true_label(#icode_if{}) -> icode_lbl().
if_true_label(#icode_if{true_label=TrueLbl}) -> TrueLbl.

-spec if_true_label_update(#icode_if{}, icode_lbl()) -> #icode_if{}.
if_true_label_update(IF, TrueLbl) -> IF#icode_if{true_label=TrueLbl}.

-spec if_false_label(#icode_if{}) -> icode_lbl().
if_false_label(#icode_if{false_label=FalseLbl}) -> FalseLbl.

-spec if_false_label_update(#icode_if{}, icode_lbl()) -> #icode_if{}.
if_false_label_update(IF, FalseLbl) -> IF#icode_if{false_label=FalseLbl}.

-spec if_pred(#icode_if{}) -> float().
if_pred(#icode_if{p=P}) -> P.

%%------------
%% switch_val
%%------------

-spec mk_switch_val(icode_var(), icode_lbl(),
		    non_neg_integer(), [icode_switch_case()]) ->
	 #icode_switch_val{}.
mk_switch_val(Term = #icode_variable{kind='var'}, FailLbl, Length, Cases) ->
  #icode_switch_val{term=Term, fail_label=FailLbl, length=Length, cases=Cases}.

-spec switch_val_term(#icode_switch_val{}) -> icode_var().
switch_val_term(#icode_switch_val{term=Term}) -> Term.

-spec switch_val_fail_label(#icode_switch_val{}) -> icode_lbl().
switch_val_fail_label(#icode_switch_val{fail_label=FailLbl}) -> FailLbl.

-spec switch_val_fail_label_update(#icode_switch_val{}, icode_lbl()) ->
	  #icode_switch_val{}.
switch_val_fail_label_update(SV, FailLbl) ->
  SV#icode_switch_val{fail_label=FailLbl}.

%% switch_val_length(#icode_switch_val{length=Length}) -> Length.

-spec switch_val_cases(#icode_switch_val{}) -> [icode_switch_case()].
switch_val_cases(#icode_switch_val{cases=Cases}) -> Cases.

-spec switch_val_cases_update(#icode_switch_val{}, [icode_switch_case()]) ->
	  #icode_switch_val{}.
switch_val_cases_update(SV, NewCases) -> 
  SV#icode_switch_val{cases = NewCases}.

%%--------------------
%% switch_tuple_arity
%%--------------------

-spec mk_switch_tuple_arity(icode_var(), icode_lbl(),
			    non_neg_integer(), [icode_switch_case()]) ->
	 #icode_switch_tuple_arity{}.
mk_switch_tuple_arity(Term = #icode_variable{kind='var'}, FailLbl, Length, Cases) ->
  #icode_switch_tuple_arity{term=Term, fail_label=FailLbl,
			    length=Length, cases=Cases}.

-spec switch_tuple_arity_term(#icode_switch_tuple_arity{}) -> icode_var().
switch_tuple_arity_term(#icode_switch_tuple_arity{term=Term}) -> Term.

-spec switch_tuple_arity_fail_label(#icode_switch_tuple_arity{}) -> icode_lbl().
switch_tuple_arity_fail_label(#icode_switch_tuple_arity{fail_label=FailLbl}) ->
  FailLbl.

-spec switch_tuple_arity_fail_label_update(#icode_switch_tuple_arity{}, icode_lbl()) ->
	 #icode_switch_tuple_arity{}.
switch_tuple_arity_fail_label_update(S, FailLbl) ->
  S#icode_switch_tuple_arity{fail_label=FailLbl}.

%% switch_tuple_arity_length(#icode_switch_tuple_arity{length=Length}) -> Length.

-spec switch_tuple_arity_cases(#icode_switch_tuple_arity{}) -> [icode_switch_case()].
switch_tuple_arity_cases(#icode_switch_tuple_arity{cases=Cases}) -> Cases.

-spec switch_tuple_arity_cases_update(#icode_switch_tuple_arity{},
				      [icode_switch_case()]) ->
	 #icode_switch_tuple_arity{}.
switch_tuple_arity_cases_update(Cond, NewCases) -> 
  Cond#icode_switch_tuple_arity{cases = NewCases}.

%%------
%% type
%%------

-spec mk_type([icode_term_arg()], icode_type_test(), icode_lbl(), icode_lbl()) ->
	 #icode_type{}.
mk_type(Args, Test, TrueLbl, FalseLbl) -> 
  mk_type(Args, Test, TrueLbl, FalseLbl, 0.5).

-spec mk_type([icode_term_arg()], icode_type_test(),
	      icode_lbl(), icode_lbl(), float()) -> #icode_type{}.
mk_type(Args, Test, TrueLbl, FalseLbl, P) ->
  #icode_type{test=Test, args=Args,
	      true_label=TrueLbl, false_label=FalseLbl, p=P}.

-spec type_test(#icode_type{}) -> icode_type_test().
type_test(#icode_type{test=Test}) -> Test.

-spec type_args(#icode_type{}) -> [icode_term_arg()].
type_args(#icode_type{args=Args}) -> Args.

%% type_args_update(T, Args) -> T#icode_type{args=Args}.

-spec type_true_label(#icode_type{}) -> icode_lbl().
type_true_label(#icode_type{true_label=TrueLbl}) -> TrueLbl.

-spec type_false_label(#icode_type{}) -> icode_lbl().
type_false_label(#icode_type{false_label=FalseLbl}) -> FalseLbl.

-spec type_pred(#icode_type{}) -> float().
type_pred(#icode_type{p=P}) -> P.

-spec is_type(icode_instr()) -> boolean().
is_type(#icode_type{}) -> true;
is_type(_) -> false.

%%------
%% goto
%%------

-spec mk_goto(icode_lbl()) -> #icode_goto{}.
mk_goto(Lbl) -> #icode_goto{label=Lbl}.

-spec goto_label(#icode_goto{}) -> icode_lbl().
goto_label(#icode_goto{label=Lbl}) -> Lbl.

-spec is_goto(icode_instr()) -> boolean().
is_goto(#icode_goto{}) -> true;
is_goto(_) -> false.

%%--------
%% return
%%--------

-spec mk_return([icode_var()]) -> #icode_return{}.
mk_return(Vars) -> #icode_return{vars=Vars}.

-spec return_vars(#icode_return{}) -> [icode_var()].
return_vars(#icode_return{vars=Vars}) -> Vars.

-spec is_return(icode_instr()) -> boolean().
is_return(#icode_return{}) -> true;
is_return(_) -> false.
  
%%------
%% fail
%%------

%% mk_fail(Args) when is_list(Args) -> mk_fail(Args, error).

-spec mk_fail([icode_term_arg()], icode_exit_class()) -> #icode_fail{}.
mk_fail(Args, Class) when is_list(Args) ->
  case Class of
    error -> ok;
    exit -> ok;
    rethrow -> ok;
    throw -> ok
  end,
  #icode_fail{class=Class, args=Args}.

%% mk_fail(Args, Class, Label) when is_list(Args) ->
%%   #icode_fail{class=Class, args=Args, fail_label=Label}.

-spec fail_class(#icode_fail{}) -> icode_exit_class().
fail_class(#icode_fail{class=Class}) -> Class.

-spec fail_args(#icode_fail{}) -> [icode_term_arg()].
fail_args(#icode_fail{args=Args}) -> Args.

-spec fail_label(#icode_fail{}) -> [] | icode_lbl().
fail_label(#icode_fail{fail_label=Label}) -> Label.

-spec fail_set_label(#icode_fail{}, [] | icode_lbl()) -> #icode_fail{}.
fail_set_label(I=#icode_fail{}, Label) ->
  I#icode_fail{fail_label = Label}.

%%------
%% move
%%------

-spec mk_move(#icode_variable{}, #icode_variable{} | #icode_const{}) ->
	 #icode_move{}.
mk_move(Dst, Src) -> 
  case Src of
    #icode_variable{} -> ok;
    #icode_const{} -> ok
  end,
  #icode_move{dst=Dst, src=Src}.

-spec move_dst(#icode_move{}) -> #icode_variable{}.
move_dst(#icode_move{dst=Dst}) -> Dst.

-spec move_src(#icode_move{}) -> #icode_variable{} | #icode_const{}.
move_src(#icode_move{src=Src}) -> Src.

-spec move_src_update(#icode_move{}, #icode_variable{} | #icode_const{}) ->
	 #icode_move{}.
move_src_update(M, NewSrc) -> M#icode_move{src=NewSrc}.

-spec is_move(icode_instr()) -> boolean().
is_move(#icode_move{}) -> true;
is_move(_) -> false.

%%-----
%% phi
%%-----

%% The id field is not entirely redundant. It is used in mappings
%% in the SSA pass since the dst field can change.
-spec mk_phi(#icode_variable{}) -> #icode_phi{}.
mk_phi(Var) -> #icode_phi{dst=Var, id=Var, arglist=[]}.

-spec mk_phi(#icode_variable{}, [{icode_lbl(), #icode_variable{}}]) ->
	 #icode_phi{}.
mk_phi(Var, ArgList) -> #icode_phi{dst=Var, id=Var, arglist=ArgList}.

-spec phi_dst(#icode_phi{}) -> #icode_variable{}.
phi_dst(#icode_phi{dst=Dst}) -> Dst.

-spec phi_id(#icode_phi{}) -> #icode_variable{}.
phi_id(#icode_phi{id=Id}) -> Id.

-spec phi_arglist(#icode_phi{}) -> [{icode_lbl(), #icode_variable{}}].
phi_arglist(#icode_phi{arglist=ArgList}) -> ArgList.

-spec phi_args(#icode_phi{}) -> [#icode_variable{}].
phi_args(P) -> [Var || {_, Var} <- phi_arglist(P)].

-spec phi_arg(#icode_phi{}, icode_lbl()) -> #icode_variable{}.
phi_arg(P, Pred) ->
  case lists:keyfind(Pred, 1, phi_arglist(P)) of
    {_, Var} -> Var;
    false -> exit({'No such predecessor to phi', {Pred, P}})
  end.

-spec is_phi(icode_instr()) -> boolean().
is_phi(#icode_phi{}) -> true;
is_phi(_) -> false.

-spec phi_enter_pred(#icode_phi{}, icode_lbl(), #icode_variable{}) ->
	 #icode_phi{}.
phi_enter_pred(Phi, Pred, Var) ->
  NewArg = {Pred, Var},
  Phi#icode_phi{arglist=[NewArg|lists:keydelete(Pred, 1, phi_arglist(Phi))]}.

-spec phi_remove_pred(#icode_phi{}, icode_lbl()) -> #icode_move{} | #icode_phi{}.
phi_remove_pred(Phi, Pred) ->
  NewArgList = lists:keydelete(Pred, 1, phi_arglist(Phi)),
  case NewArgList of
    [Arg] -> %% the Phi should be turned into an appropriate move instruction
      {_Label, Var = #icode_variable{}} = Arg,
      mk_move(phi_dst(Phi), Var);
    [_|_] ->
      Phi#icode_phi{arglist=NewArgList}
  end.

phi_argvar_subst(P, Subst) ->
  NewArgList = [{Pred, subst1(Subst, Var)} || {Pred,Var} <- phi_arglist(P)],
  P#icode_phi{arglist=NewArgList}.

-spec phi_redirect_pred(#icode_phi{}, icode_lbl(), icode_lbl()) -> #icode_phi{}.
phi_redirect_pred(P, OldPred, NewPred) ->
  Subst = [{OldPred, NewPred}],
  NewArgList = [{subst1(Subst, Pred), Var} || {Pred,Var} <- phi_arglist(P)],
  P#icode_phi{arglist=NewArgList}.

%%
%% primop and guardop
%%
%% Whether a function is a "primop" - i.e., an internal thing - or not,
%% is really only shown by its name. An {M,F,A} always represents a
%% function in some Erlang module (although it might be a BIF, and
%% could possibly be inline expanded). It is convenient to let the
%% constructor functions check the name and set the type automatically,
%% especially for guardops - some guardops are primitives and some are
%% MFA:s, and this way we won't have to rewrite all calls to mk_guardop
%% to flag whether they are primops or not.

-spec mk_primop([#icode_variable{}], icode_funcall(),
	        [icode_argument()]) -> #icode_call{}.
mk_primop(DstList, Fun, ArgList) ->
  mk_primop(DstList, Fun, ArgList, [], []).

-spec mk_primop([#icode_variable{}], icode_funcall(),
	        [icode_argument()], [] | icode_lbl(), [] | icode_lbl()) ->
	 #icode_call{}.
mk_primop(DstList, Fun, ArgList, Continuation, Fail) ->
  Type = op_type(Fun),
  make_call(DstList, Fun, ArgList, Type, Continuation, Fail, false).

%% Note that a 'guardop' is just a call that occurred in a guard. In
%% this case, we should always have continuation labels True and False.

-spec mk_guardop([#icode_variable{}], icode_funcall(),
	         [icode_argument()], icode_lbl(), icode_lbl()) -> #icode_call{}.
mk_guardop(DstList, Fun, ArgList, True, False) ->
  Type = op_type(Fun),
  make_call(DstList, Fun, ArgList, Type, True, False, true).

op_type(Fun) ->
  case is_mfa(Fun) of
    true -> remote;
    false -> primop
  end.

is_mfa({M,F,A}) when is_atom(M), is_atom(F), 
		     is_integer(A), 0 =< A, A =< 255 -> true;
is_mfa(_) -> false.

%%------
%% call
%%------

-spec mk_call([#icode_variable{}], atom(), atom(),
	      [icode_argument()], 'local' | 'remote') -> #icode_call{}.
mk_call(DstList, M, F, ArgList, Type) ->
  mk_call(DstList, M, F, ArgList, Type, [], [], false).

%% mk_call(DstList, M, F, ArgList, Type, Continuation, Fail) ->
%%   mk_call(DstList, M, F, ArgList, Type, Continuation, Fail, false).

-spec mk_call([#icode_variable{}], atom(), atom(), [icode_argument()],
	      'local' | 'remote', [] | icode_lbl(), [] | icode_lbl(), boolean()) ->
	 #icode_call{}.
mk_call(DstList, M, F, ArgList, Type, Continuation, Fail, InGuard)
  when is_atom(M), is_atom(F) ->
  case Type of
    local -> ok;
    remote -> ok
  end,
  Fun = {M,F,length(ArgList)},
  make_call(DstList, Fun, ArgList, Type, Continuation, Fail, InGuard).

%% The common constructor for all calls (for internal use only)
%%
%% Note: If the "guard" flag is `true', it means that if the call fails,
%% we can simply jump to the Fail label (if it exists) without
%% generating any additional exception information - it isn't needed.
-spec make_call([#icode_variable{}], icode_funcall(), [icode_argument()],
	        icode_call_type(), [] | icode_lbl(), [] | icode_lbl(), boolean()) ->
	 #icode_call{}.
make_call(DstList, Fun, ArgList, Type, Continuation, Fail, InGuard) ->
  #icode_call{dstlist=DstList, 'fun'=Fun, args=ArgList, type=Type,
	      continuation=Continuation, fail_label=Fail, in_guard=InGuard}.

-spec call_dstlist(#icode_call{}) -> [#icode_variable{}].
call_dstlist(#icode_call{dstlist=DstList}) -> DstList.

-spec call_dstlist_update(#icode_call{}, [#icode_variable{}]) -> #icode_call{}.
call_dstlist_update(C, Dest) -> C#icode_call{dstlist=Dest}.

-spec call_type(#icode_call{}) -> icode_call_type().
call_type(#icode_call{type=Type}) -> Type.

%% -spec call_dst_type(#icode_call{}) -> erl_type().
%% call_dst_type(#icode_call{dst_type=DstType}) -> DstType.

-spec call_args(#icode_call{}) -> [icode_argument()].
call_args(#icode_call{args=Args}) -> Args.

-spec call_args_update(#icode_call{}, [icode_argument()]) -> #icode_call{}.
call_args_update(C, Args) -> C#icode_call{args=Args}.

-spec call_fun(#icode_call{}) -> icode_funcall().
call_fun(#icode_call{'fun'=Fun}) -> Fun.

%% Note that updating the name field requires recomputing the call type,
%% in case it changes from a remote/local call to a primop call.
-spec call_fun_update(#icode_call{}, icode_funcall()) -> #icode_call{}.
call_fun_update(C, Fun) ->
  Type = case is_mfa(Fun) of
	   true -> call_type(C);
	   false -> primop
	 end,
  C#icode_call{'fun'=Fun, type=Type}.

-spec call_continuation(#icode_call{}) -> [] | icode_lbl().
call_continuation(#icode_call{continuation=Continuation}) -> Continuation.

-spec call_fail_label(#icode_call{}) -> [] | icode_lbl().
call_fail_label(#icode_call{fail_label=Fail}) -> Fail.

-spec call_set_continuation(#icode_call{}, [] | icode_lbl()) -> #icode_call{}.
call_set_continuation(I, Continuation) ->
  I#icode_call{continuation = Continuation}.

-spec call_set_fail_label(#icode_call{}, [] | icode_lbl()) -> #icode_call{}.
call_set_fail_label(I=#icode_call{}, Fail) ->
  case Fail of
    [] ->
      I#icode_call{fail_label=Fail, in_guard=false};
    _  ->
      I#icode_call{fail_label=Fail}
  end.

-spec is_call(icode_instr()) -> boolean().
is_call(#icode_call{}) -> true;
is_call(_) -> false.

-spec call_in_guard(#icode_call{}) -> boolean().
call_in_guard(#icode_call{in_guard=InGuard}) -> InGuard.

%%-------
%% enter
%%-------

-spec mk_enter(atom(), atom(), [icode_term_arg()], 'local' | 'remote') ->
	 #icode_enter{}.
mk_enter(M, F, Args, Type) when is_atom(M), is_atom(F) ->
  case Type of
    local -> ok;
    remote -> ok
  end,
  #icode_enter{'fun'={M,F,length(Args)}, args=Args, type=Type}.

-spec enter_fun(#icode_enter{}) -> icode_funcall().
enter_fun(#icode_enter{'fun'=Fun}) -> Fun.

-spec enter_fun_update(#icode_enter{}, icode_funcall()) ->
	 #icode_enter{}.
enter_fun_update(E, Fun) ->
  Type = case is_mfa(Fun) of
	   true -> enter_type(E);
	   false -> primop
	 end,
  E#icode_enter{'fun'=Fun, type=Type}.

-spec enter_args(#icode_enter{}) -> [icode_term_arg()].
enter_args(#icode_enter{args=Args}) -> Args.

-spec enter_args_update(#icode_enter{}, [icode_term_arg()]) -> #icode_enter{}.
enter_args_update(E, Args) -> E#icode_enter{args=Args}.

-spec enter_type(#icode_enter{}) -> icode_call_type().
enter_type(#icode_enter{type=Type}) -> Type.

-spec is_enter(icode_instr()) -> boolean().
is_enter(#icode_enter{}) -> true;
is_enter(_) -> false.

-spec mk_enter_primop(icode_primop(), [icode_term_arg()]) ->
	 #icode_enter{type::'primop'}.
mk_enter_primop(Op, Args) ->
  #icode_enter{'fun'=Op, args=Args, type=primop}.

%%-----------
%% begin_try
%%-----------

%% The reason that begin_try is a branch instruction is just so that it
%% keeps the fail-to block linked into the CFG, until the exception
%% handling instructions are eliminated.

-spec mk_begin_try(icode_lbl(), icode_lbl()) -> #icode_begin_try{}.
mk_begin_try(Label, Successor) ->
  #icode_begin_try{label=Label, successor=Successor}.

-spec begin_try_label(#icode_begin_try{}) -> icode_lbl().
begin_try_label(#icode_begin_try{label=Label}) -> Label.

-spec begin_try_successor(#icode_begin_try{}) -> icode_lbl().
begin_try_successor(#icode_begin_try{successor=Successor}) -> Successor.

%%---------
%% end_try
%%---------

-spec mk_end_try() -> #icode_end_try{}.
mk_end_try() -> #icode_end_try{}.

%%---------------
%% begin_handler
%%---------------

-spec mk_begin_handler([icode_var()]) -> #icode_begin_handler{}.
mk_begin_handler(Dstlist) ->
  #icode_begin_handler{dstlist=Dstlist}.

-spec begin_handler_dstlist(#icode_begin_handler{}) -> [icode_var()].
begin_handler_dstlist(#icode_begin_handler{dstlist=Dstlist}) -> Dstlist.

%% -spec is_begin_handler(icode_instr()) -> boolean().
%% is_begin_handler(#icode_begin_handler{}) -> true;
%% is_begin_handler(_) -> false.

%%-------
%% label
%%-------

-spec mk_label(icode_lbl()) -> #icode_label{}.
mk_label(Name) when is_integer(Name), Name >= 0 -> #icode_label{name=Name}.

-spec label_name(#icode_label{}) -> icode_lbl().
label_name(#icode_label{name=Name}) -> Name.

-spec is_label(icode_instr()) -> boolean().
is_label(#icode_label{}) -> true;
is_label(_) -> false.

%%---------
%% comment
%%---------

-spec mk_comment(icode_comment_text()) -> #icode_comment{}.
%% @doc If `Txt' is a list of characters (possibly deep), it will be
%% printed as a string; otherwise, `Txt' will be printed as a term.
mk_comment(Txt) -> #icode_comment{text=Txt}.

-spec comment_text(#icode_comment{}) -> icode_comment_text().
comment_text(#icode_comment{text=Txt}) -> Txt.

-spec is_comment(icode_instr()) -> boolean().
is_comment(#icode_comment{}) -> true;
is_comment(_) -> false.


%%---------------------------------------------------------------------
%% Arguments (variables and constants)
%%---------------------------------------------------------------------

%%-------
%% const
%%-------

-spec mk_const(simple_const() | structured_const() | binary()) -> #icode_const{}.
mk_const(C) -> #icode_const{value=#flat{value=C}}.

-spec const_value(#icode_const{}) -> simple_const() | structured_const() | binary().
const_value(#icode_const{value=#flat{value=X}}) -> X.

-spec is_const(icode_argument()) -> boolean().
is_const(#icode_const{}) -> true;
is_const(_) -> false.

%%-----
%% var
%%-----

-spec mk_var(non_neg_integer()) -> #icode_variable{kind::'var'}.
mk_var(V) -> #icode_variable{name=V, kind=var}.

-spec var_name(#icode_variable{kind::'var'}) -> non_neg_integer().
var_name(#icode_variable{name=Name, kind=var}) -> Name.

-spec is_var(icode_argument()) -> boolean().
is_var(#icode_variable{kind=var}) -> true;
is_var(_) -> false.

-spec mk_reg(non_neg_integer()) -> #icode_variable{kind::'reg'}.
mk_reg(V) -> #icode_variable{name=V, kind=reg}.

-spec reg_name(#icode_variable{kind::'reg'}) -> non_neg_integer().
reg_name(#icode_variable{name=Name, kind=reg}) -> Name.

-spec reg_is_gcsafe(#icode_variable{kind::'reg'}) -> 'false'.
reg_is_gcsafe(#icode_variable{kind=reg}) -> false. % for now

-spec is_reg(icode_argument()) -> boolean().
is_reg(#icode_variable{kind=reg}) -> true;
is_reg(_) -> false.

-spec mk_fvar(non_neg_integer()) -> #icode_variable{kind::'fvar'}.
mk_fvar(V) -> #icode_variable{name=V, kind=fvar}.

-spec fvar_name(#icode_variable{kind::'fvar'}) -> non_neg_integer().
fvar_name(#icode_variable{name=Name, kind=fvar}) -> Name.

-spec is_fvar(icode_argument()) -> boolean().
is_fvar(#icode_variable{kind=fvar}) -> true;
is_fvar(_) -> false.

-spec is_variable(icode_argument()) -> boolean().
is_variable(#icode_variable{}) -> true;
is_variable(_) -> false.

-spec annotate_variable(#icode_variable{}, variable_annotation()) ->
	 #icode_variable{}.
annotate_variable(X, Anno) ->
  X#icode_variable{annotation = Anno}.

-spec is_annotated_variable(icode_argument()) -> boolean().
is_annotated_variable(#icode_variable{annotation=[]}) ->
  false;
is_annotated_variable(#icode_variable{}) ->
  true;
is_annotated_variable(_) ->
  false.

-spec unannotate_variable(#icode_variable{}) -> #icode_variable{}.
unannotate_variable(X) ->
  X#icode_variable{annotation=[]}.

-spec variable_annotation(#icode_variable{}) -> variable_annotation().
variable_annotation(#icode_variable{annotation=Anno}) ->
  Anno.

%%
%% Floating point Icode instructions.
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Liveness info 
%%

-spec uses(icode_instr()) -> [#icode_variable{}].
uses(Instr) ->
  remove_constants(args(Instr)).

-spec args(icode_instr()) -> [icode_argument()].
args(I) ->
  case I of
    #icode_if{} -> if_args(I);
    #icode_switch_val{} -> [switch_val_term(I)];
    #icode_switch_tuple_arity{} -> [switch_tuple_arity_term(I)];
    #icode_type{} -> type_args(I);
    #icode_move{} -> [move_src(I)];
    #icode_fail{} -> fail_args(I);
    #icode_call{} -> call_args(I);
    #icode_enter{} -> enter_args(I);
    #icode_return{} -> return_vars(I);
    #icode_phi{} -> phi_args(I);
    #icode_goto{} -> [];
    #icode_begin_try{} -> [];
    #icode_begin_handler{} -> [];
    #icode_end_try{} -> [];
    #icode_comment{} -> [];
    #icode_label{} -> []
  end.

-spec defines(icode_instr()) -> [#icode_variable{}].
defines(I) ->
  case I of
    #icode_move{} -> remove_constants([move_dst(I)]);
    #icode_call{} -> remove_constants(call_dstlist(I));
    #icode_begin_handler{} -> remove_constants(begin_handler_dstlist(I));
    #icode_phi{} -> remove_constants([phi_dst(I)]);
    #icode_if{} -> [];
    #icode_switch_val{} -> [];
    #icode_switch_tuple_arity{} -> [];
    #icode_type{} -> [];
    #icode_goto{} -> [];
    #icode_fail{} -> [];
    #icode_enter{} -> [];
    #icode_return{} -> [];
    #icode_begin_try{} -> [];
    #icode_end_try{} -> [];
    #icode_comment{} -> [];
    #icode_label{} -> []
  end.

-spec remove_constants([icode_argument()]) -> [#icode_variable{}].
remove_constants(L) ->
  [V || V <- L, (not is_const(V))].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Utilities
%%

%%
%% Substitution: replace occurrences of X by Y if {X,Y} is in the
%%   Subst_list.

-spec subst([{_,_}], I) -> I when is_subtype(I, icode_instr()).

subst(Subst, I) ->
  subst_defines(Subst, subst_uses(Subst, I)).

-spec subst_uses([{_,_}], I) -> I when is_subtype(I, icode_instr()).

subst_uses(Subst, I) ->
  case I of
    #icode_if{} -> I#icode_if{args = subst_list(Subst, if_args(I))};
    #icode_switch_val{} ->
      I#icode_switch_val{term = subst1(Subst, switch_val_term(I))};
    #icode_switch_tuple_arity{} ->
      I#icode_switch_tuple_arity{term = subst1(Subst, switch_tuple_arity_term(I))};
    #icode_type{} -> I#icode_type{args = subst_list(Subst, type_args(I))};
    #icode_move{} -> I#icode_move{src = subst1(Subst, move_src(I))};
    #icode_fail{} -> I#icode_fail{args = subst_list(Subst, fail_args(I))};
    #icode_call{} -> I#icode_call{args = subst_list(Subst, call_args(I))};
    #icode_enter{} -> I#icode_enter{args = subst_list(Subst, enter_args(I))};
    #icode_return{} -> I#icode_return{vars = subst_list(Subst, return_vars(I))};
    #icode_phi{} -> phi_argvar_subst(I, Subst);
    #icode_goto{} -> I;
    #icode_begin_try{} -> I;
    #icode_begin_handler{} -> I;
    #icode_end_try{} -> I;
    #icode_comment{} -> I;
    #icode_label{} -> I
  end.

-spec subst_defines([{_,_}], I) -> I when is_subtype(I, icode_instr()).

subst_defines(Subst, I) ->
  case I of
    #icode_move{} -> I#icode_move{dst = subst1(Subst, move_dst(I))};
    #icode_call{} -> 
      I#icode_call{dstlist = subst_list(Subst, call_dstlist(I))};
    #icode_begin_handler{} -> 
      I#icode_begin_handler{dstlist = subst_list(Subst,
						 begin_handler_dstlist(I))};
    #icode_phi{} -> I#icode_phi{dst = subst1(Subst, phi_dst(I))};
    #icode_if{} -> I;
    #icode_switch_val{} -> I;
    #icode_switch_tuple_arity{} -> I;
    #icode_type{} -> I;
    #icode_goto{} -> I;
    #icode_fail{} -> I;
    #icode_enter{} -> I;
    #icode_return{} -> I;
    #icode_begin_try{} -> I;
    #icode_end_try{} -> I;
    #icode_comment{} -> I;
    #icode_label{} -> I
  end.

subst_list(S, Is) ->
  [subst1(S, I) || I <- Is].

subst1([], I) -> I;
subst1([{I,Y}|_], I) -> Y;
subst1([_|Pairs], I) -> subst1(Pairs, I).

%%
%% @doc Returns the successors of an Icode instruction.
%%      In CFG form only branch instructions have successors,
%%	but in linear form other instructions like e.g. moves and
%%	others might be the last instruction of some basic block.
%%

-spec successors(icode_instr()) -> [icode_lbl()].

successors(I) ->
  case I of
    #icode_if{} ->
      [if_true_label(I), if_false_label(I)];
    #icode_goto{} ->
      [goto_label(I)];
    #icode_switch_val{} ->
      CaseLabels = [L || {_,L} <- switch_val_cases(I)],
      [switch_val_fail_label(I) | CaseLabels];
    #icode_switch_tuple_arity{} ->
      CaseLabels = [L || {_,L} <- switch_tuple_arity_cases(I)],
      [switch_tuple_arity_fail_label(I) | CaseLabels];
    #icode_type{} ->
      [type_true_label(I), type_false_label(I)];
    #icode_call{} ->
      case call_continuation(I) of [] -> []; L when is_integer(L) -> [L] end
	++
      case call_fail_label(I) of [] -> []; L when is_integer(L) -> [L] end;
    #icode_begin_try{} ->
      [begin_try_successor(I), begin_try_label(I)];
    #icode_fail{} ->
      case fail_label(I) of [] -> []; L when is_integer(L) -> [L] end;
    #icode_enter{} -> [];
    #icode_return{} -> [];
    %% the following are included here for handling linear code
    #icode_move{} -> [];
    #icode_begin_handler{} -> []
  end.

%%
%% @doc Returns the fail labels of an Icode instruction.
%%

-spec fails_to(icode_instr()) -> [icode_lbl()].

fails_to(I) ->
  case I of
    #icode_switch_val{} -> [switch_val_fail_label(I)];
    #icode_switch_tuple_arity{} -> [switch_tuple_arity_fail_label(I)];
    #icode_call{} ->
      case call_fail_label(I) of [] -> []; L when is_integer(L) -> [L] end;
    #icode_begin_try{} -> [begin_try_label(I)];  % just for safety
    #icode_fail{} ->
      case fail_label(I) of [] -> []; L when is_integer(L) -> [L] end;
    #icode_if{} -> [];     % XXX: Correct?
    #icode_enter{} -> [];  % XXX: Correct?
    #icode_goto{} -> [];
    #icode_type{} -> [];   % XXX: Correct?
    #icode_return{} -> []
  end.

%%
%% @doc Redirects jumps from label Old to label New.
%% If the instruction does not jump to Old, it remains unchanged.
%% The New label can be the special [] label used for calls with
%% fall-throughs.
%%

-spec redirect_jmp(icode_instr(), icode_lbl(), [] | icode_lbl()) -> icode_instr().

redirect_jmp(Jmp, ToOld, ToOld) ->
  Jmp;    % no need to do anything
redirect_jmp(Jmp, ToOld, ToNew) ->
  NewI =
    case Jmp of
      #icode_if{} ->
	NewJmp = case if_true_label(Jmp) of
		   ToOld -> if_true_label_update(Jmp, ToNew);
		   _ -> Jmp
		 end,
	case if_false_label(NewJmp) of
	  ToOld -> if_false_label_update(NewJmp, ToNew);
	  _ -> NewJmp
	end;
      #icode_goto{} ->
	case goto_label(Jmp) of
	  ToOld -> Jmp#icode_goto{label=ToNew};
	  _ -> Jmp
	end;
      #icode_switch_val{} ->
	NewJmp = case switch_val_fail_label(Jmp) of
		   ToOld -> switch_val_fail_label_update(Jmp, ToNew);
		   _ -> Jmp
		 end,
	Cases = [case Pair of 
		   {Val,ToOld} -> {Val,ToNew};
		   Unchanged -> Unchanged
		 end || Pair <- switch_val_cases(NewJmp)],
	NewJmp#icode_switch_val{cases = Cases};
      #icode_switch_tuple_arity{} ->
	NewJmp = case switch_tuple_arity_fail_label(Jmp) of
		   ToOld -> 
		     Jmp#icode_switch_tuple_arity{fail_label=ToNew};
		   _ -> Jmp
		 end,
	Cases = [case Pair of 
		   {Val,ToOld} -> {Val,ToNew};
		   Unchanged -> Unchanged
		 end || Pair <- switch_tuple_arity_cases(NewJmp)],
	NewJmp#icode_switch_tuple_arity{cases = Cases};
      #icode_type{} ->
	NewJmp = case type_true_label(Jmp) of
		   ToOld -> Jmp#icode_type{true_label=ToNew};
		   _ -> Jmp
		 end,
	case type_false_label(NewJmp) of
	  ToOld -> NewJmp#icode_type{false_label=ToNew};
	  _ -> NewJmp
	end;
      #icode_call{} -> 
	NewCont = case call_continuation(Jmp) of
		    ToOld -> ToNew;
		    OldCont -> OldCont
		  end,
	NewFail = case call_fail_label(Jmp) of
		    ToOld -> ToNew;
		    OldFail -> OldFail
		  end,
	Jmp#icode_call{continuation = NewCont, 
		 fail_label = NewFail};
      #icode_begin_try{} ->
	NewLabl = case begin_try_label(Jmp) of
		    ToOld ->  ToNew;
		    OldLab -> OldLab
		  end,
	NewSucc = case begin_try_successor(Jmp) of
		    ToOld ->  ToNew;
		    OldSucc -> OldSucc
		  end,
	Jmp#icode_begin_try{label=NewLabl, successor=NewSucc};
      #icode_fail{} ->
	case fail_label(Jmp) of
	  ToOld -> Jmp#icode_fail{fail_label=ToNew};
	  _ -> Jmp
	end
    end,
  %% Turn a branch into a goto if it has only one successor and it is
  %% safe to do so.
  case ordsets:from_list(successors(NewI)) of
    [Label] ->
      Goto = mk_goto(Label),
      case NewI of
	#icode_if{} -> Goto;
	#icode_switch_tuple_arity{} -> Goto;
	#icode_switch_val{} -> Goto;
	#icode_type{} -> Goto;
	_ -> NewI
      end;
    _ -> NewI
  end.

%%
%% Is this an unconditional jump (causes a basic block not to have a 
%% fallthrough successor).
%%

%% is_uncond(I) ->
%%   case I of
%%     #icode_goto{} -> true;
%%     #icode_fail{} -> true;
%%     #icode_enter{} -> true;
%%     #icode_return{} -> true;
%%     #icode_call{} -> 
%%       case call_fail_label(I) of
%% 	[] -> 
%% 	  case call_continuation(I) of
%% 	    [] -> false;
%% 	    _ -> true
%% 	  end;
%% 	_ -> true
%%       end;
%%     _ -> false
%%   end.

%% @spec is_branch(icode_instr()) -> boolean()
%%
%% @doc Succeeds if the Icode instruction is a branch. I.e. a
%%      (possibly conditional) discontinuation of linear control flow.
%% @end

-spec is_branch(icode_instr()) -> boolean().
is_branch(Instr) ->
  case Instr of
    #icode_if{} -> true;
    #icode_switch_val{} -> true;
    #icode_switch_tuple_arity{} -> true;
    #icode_type{} -> true;
    #icode_goto{} -> true;
    #icode_fail{} -> true;
    #icode_call{} -> 
      case call_fail_label(Instr) of
	[] -> call_continuation(Instr) =/= [];
	_ -> true
      end;
    #icode_enter{} -> true;
    #icode_return{} -> true;
    #icode_begin_try{} -> true;
    %% false cases below
    #icode_move{} -> false;
    #icode_begin_handler{} -> false;
    #icode_end_try{} -> false;
    #icode_comment{} -> false;
    #icode_label{} -> false;
    #icode_phi{} -> false
  end.

%%
%% @doc Makes a new variable.
%%

-spec mk_new_var() -> icode_var().
mk_new_var() ->
  mk_var(hipe_gensym:get_next_var(icode)).

%%
%% @doc Makes a new fp variable.
%%

-spec mk_new_fvar() -> icode_fvar().
mk_new_fvar() ->
  mk_fvar(hipe_gensym:get_next_var(icode)).

%%
%% @doc Makes a new register.
%%

-spec mk_new_reg() -> icode_reg().
mk_new_reg() ->
  mk_reg(hipe_gensym:get_next_var(icode)).

%%
%% @doc Makes a new label.
%%

-spec mk_new_label() -> #icode_label{}.
mk_new_label() ->
  mk_label(hipe_gensym:get_next_label(icode)).

%% %%
%% %% @doc Makes a bunch of move operations.
%% %%
%% 
%% -spec mk_moves([_], [_]) -> [#icode_move{}].
%% mk_moves([], []) ->
%%   [];
%% mk_moves([X|Xs], [Y|Ys]) ->
%%   [mk_move(X, Y) | mk_moves(Xs, Ys)].

%%
%% Makes a series of element operations.
%%

%% mk_elements(_, []) -> 
%%   [];
%% mk_elements(Tuple, [X|Xs]) ->
%%   [mk_primop([X], #unsafe_element{index=length(Xs)+1}, [Tuple]) | 
%%    mk_elements(Tuple, Xs)].

%%
%% @doc Removes comments from Icode.
%%

-spec strip_comments(#icode{}) -> #icode{}.
strip_comments(ICode) ->
  icode_code_update(ICode, no_comments(icode_code(ICode))).

%% The following spec is underspecified: the resulting list does not
%% contain any #comment{} instructions
-spec no_comments(icode_instrs()) -> icode_instrs().
no_comments([]) ->
  [];
no_comments([I|Xs]) ->
  case is_comment(I) of 
    true -> no_comments(Xs);
    false -> [I|no_comments(Xs)]
  end.

%%-----------------------------------------------------------------------

%% @doc True if an Icode instruction is safe (can be removed if the
%% result is not used). Note that pure control flow instructions
%% cannot be regarded as safe, as they are not defining anything.

-spec is_safe(icode_instr()) -> boolean().

is_safe(Instr) ->
  case Instr of
    %% Instructions that are safe, or might be safe to remove.
    #icode_move{} -> true;
    #icode_phi{} -> true;
    #icode_begin_handler{} -> true;
    #icode_call{} ->
      case call_fun(Instr) of
	{M,F,A} ->
	  erl_bifs:is_safe(M,F,A);
	Op ->
	  hipe_icode_primops:is_safe(Op)
      end;
    %% Control flow instructions.
    #icode_if{} -> false;
    #icode_switch_val{} -> false;
    #icode_switch_tuple_arity{} -> false;
    #icode_type{} -> false;
    #icode_goto{} -> false;
    #icode_label{} -> false;
    %% Returning instructions without defines.
    #icode_return{} -> false;
    #icode_fail{} -> false;
    #icode_enter{} -> false;
    %% Internal auxiliary instructions that should not be removed
    %% unless you really know what you are doing.
    #icode_comment{} -> false;
    #icode_begin_try{} -> false;
    #icode_end_try{} -> false
  end.

%%-----------------------------------------------------------------------

-spec highest_var(icode_instrs()) -> non_neg_integer().
highest_var(Instrs) ->
  highest_var(Instrs, 0).

-spec highest_var(icode_instrs(), non_neg_integer()) -> non_neg_integer().
highest_var([I|Is], Max) ->
  Defs = defines(I),
  Uses = uses(I),
  highest_var(Is, new_max(Defs++Uses, Max));
highest_var([], Max) ->
  Max.

-spec new_max([#icode_variable{}], non_neg_integer()) -> non_neg_integer().
new_max([V|Vs], Max) ->
  VName = 
    case is_var(V) of
      true ->
	var_name(V);
      false ->
	case is_fvar(V) of
	  true ->
	    fvar_name(V);
	  _ ->
	    reg_name(V)
	end
    end,
  new_max(Vs, erlang:max(VName, Max));
new_max([], Max) when is_integer(Max) ->
  Max.

%%-----------------------------------------------------------------------

-spec highest_label(icode_instrs()) -> icode_lbl().
highest_label(Instrs) ->
  highest_label(Instrs, 0).

-spec highest_label(icode_instrs(), icode_lbl()) -> icode_lbl().
highest_label([I|Is], Max) ->
  case is_label(I) of 
    true ->
      L = label_name(I),
      NewMax = erlang:max(L, Max),
      highest_label(Is, NewMax);
    false ->
      highest_label(Is, Max)
  end;
highest_label([], Max) when is_integer(Max) ->
  Max.

%%-----------------------------------------------------------------------
