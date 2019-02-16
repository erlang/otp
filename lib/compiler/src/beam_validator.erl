%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2018. All Rights Reserved.
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

-module(beam_validator).

-compile({no_auto_import,[min/2]}).

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).

%% Interface for compiler.
-export([module/2, format_error/1]).
-export([type_anno/1, type_anno/2, type_anno/4]).

-import(lists, [any/2,dropwhile/2,foldl/3,map/2,foreach/2,reverse/1]).

%% To be called by the compiler.

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs,Lc}=Code, _Opts)
  when is_atom(Mod), is_list(Exp), is_list(Attr), is_integer(Lc) ->
    case validate(Mod, Fs) of
	[] ->
	    {ok,Code};
	Es0 ->
	    Es = [{?MODULE,E} || E <- Es0],
	    {error,[{atom_to_list(Mod),Es}]}
    end.

%% Provides a stable interface for type annotations, used by certain passes to
%% indicate that we can safely assume that a register has a given type.
-spec type_anno(term()) -> term().
type_anno(atom) -> {atom,[]};
type_anno(bool) -> bool;
type_anno({binary,_}) -> term;
type_anno(cons) -> cons;
type_anno(float) -> {float,[]};
type_anno(integer) -> {integer,[]};
type_anno(list) -> list;
type_anno(map) -> map;
type_anno(match_context) -> match_context;
type_anno(number) -> number;
type_anno(nil) -> nil.

-spec type_anno(term(), term()) -> term().
type_anno(atom, Value) -> {atom, Value};
type_anno(float, Value) -> {float, Value};
type_anno(integer, Value) -> {integer, Value}.

-spec type_anno(term(), term(), term(), term()) -> term().
type_anno(tuple, Size, Exact, Elements) when is_integer(Size), Size >= 0,
                                             is_map(Elements) ->
    case Exact of
        true -> {tuple, Size, Elements};
        false -> {tuple, [Size], Elements}
    end.

-spec format_error(term()) -> iolist().

format_error({{_M,F,A},{I,Off,limit}}) ->
    io_lib:format(
      "function ~p/~p+~p:~n"
      "  An implementation limit was reached.~n"
      "  Try reducing the complexity of this function.~n~n"
      "  Instruction: ~p~n", [F,A,Off,I]);
format_error({{_M,F,A},{undef_labels,Lbls}}) ->
    io_lib:format(
      "function ~p/~p:~n"
      "  Internal consistency check failed - please report this bug.~n"
      "  The following label(s) were referenced but not defined:~n", [F,A]) ++
	"  " ++ [[integer_to_list(L)," "] || L <- Lbls] ++ "\n";
format_error({{_M,F,A},{I,Off,Desc}}) ->
    io_lib:format(
      "function ~p/~p+~p:~n"
      "  Internal consistency check failed - please report this bug.~n"
      "  Instruction: ~p~n"
      "  Error:       ~p:~n", [F,A,Off,I,Desc]);
format_error(Error) ->
    io_lib:format("~p~n", [Error]).

%%%
%%% Local functions follow.
%%% 

%%%
%%% The validator follows.
%%%
%%% The purpose of the validator is to find errors in the generated
%%% code that may cause the emulator to crash or behave strangely.
%%% We don't care about type errors in the user's code that will
%%% cause a proper exception at run-time.
%%%

%%% Things currently not checked. XXX
%%%
%%% - Heap allocation for binaries.
%%%

%% validate(Module, [Function]) -> [] | [Error]
%%  A list of functions with their code. The code is in the same
%%  format as used in the compiler and in .S files.

validate(Module, Fs) ->
    Ft = index_parameter_types(Fs, []),
    validate_0(Module, Fs, Ft).

validate_0(_Module, [], _) -> [];
validate_0(Module, [{function,Name,Ar,Entry,Code}|Fs], Ft) ->
    try validate_1(Code, Name, Ar, Entry, Ft) of
	_ -> validate_0(Module, Fs, Ft)
    catch
	throw:Error ->
	    %% Controlled error.
	    [Error|validate_0(Module, Fs, Ft)];
        Class:Error:Stack ->
	    %% Crash.
	    io:fwrite("Function: ~w/~w\n", [Name,Ar]),
	    erlang:raise(Class, Error, Stack)
    end.

-type index() :: non_neg_integer().
-type reg_tab() :: gb_trees:tree(index(), 'none' | {'value', _}).

-record(st,				%Emulation state
	{x=init_regs(0, term)        :: reg_tab(),%x register info.
	 y=init_regs(0, initialized) :: reg_tab(),%y register info.
	 f=init_fregs(),                %
	 numy=none,			%Number of y registers.
	 h=0,				%Available heap size.
	 hf=0,				%Available heap size for floats.
	 fls=undefined,			%Floating point state.
	 ct=[],				%List of hot catch/try labels
         setelem=false,                 %Previous instruction was setelement/3.
         puts_left=none,                %put/1 instructions left.
         defs=#{},                      %Defining expression for each register.
         aliases=#{}
	}).

-type label()        :: integer().
-type label_set()    :: gb_sets:set(label()).
-type branched_tab() :: gb_trees:tree(label(), #st{}).
-type ft_tab()       :: gb_trees:tree().

-record(vst,				%Validator state
	{current=none              :: #st{} | 'none',	%Current state
	 branched=gb_trees:empty() :: branched_tab(),	%States at jumps
	 labels=gb_sets:empty()    :: label_set(),	%All defined labels
	 ft=gb_trees:empty()       :: ft_tab()          %Some other functions
	 		% in the module (those that start with bs_start_match2).
	}).

%% Match context type.
-record(ms,
	{id=make_ref() :: reference(),		%Unique ID.
	 valid=0 :: non_neg_integer(),		%Valid slots
	 slots=0 :: non_neg_integer()		%Number of slots
	}).

index_parameter_types([{function,_,_,Entry,Code0}|Fs], Acc0) ->
    Code = dropwhile(fun({label,L}) when L =:= Entry -> false;
			(_) -> true
		     end, Code0),
    case Code of
	[{label,Entry}|Is] ->
	    Acc = index_parameter_types_1(Is, Entry, Acc0),
	    index_parameter_types(Fs, Acc);
	_ ->
	    %% Something serious is wrong. Ignore it for now.
	    %% It will be detected and diagnosed later.
	    index_parameter_types(Fs, Acc0)
    end;
index_parameter_types([], Acc) ->
    gb_trees:from_orddict(lists:sort(Acc)).

index_parameter_types_1([{'%', {type_info, Reg, Type0}} | Is], Entry, Acc) ->
    Type = case Type0 of
                match_context -> #ms{};
                _ -> Type0
            end,
    Key = {Entry, Reg},
    index_parameter_types_1(Is, Entry, [{Key, Type} | Acc]);
index_parameter_types_1(_, _, Acc) ->
    Acc.

validate_1(Is, Name, Arity, Entry, Ft) ->
    validate_2(labels(Is), Name, Arity, Entry, Ft).

validate_2({Ls1,[{func_info,{atom,Mod},{atom,Name},Arity}=_F|Is]},
	   Name, Arity, Entry, Ft) ->
    validate_3(labels(Is), Name, Arity, Entry, Mod, Ls1, Ft);
validate_2({Ls1,Is}, Name, Arity, _Entry, _Ft) ->
    error({{'_',Name,Arity},{first(Is),length(Ls1),illegal_instruction}}).

validate_3({Ls2,Is}, Name, Arity, Entry, Mod, Ls1, Ft) ->
    Offset = 1 + length(Ls1) + 1 + length(Ls2),
    EntryOK = lists:member(Entry, Ls2),
    if
	EntryOK ->
	    St = init_state(Arity),
	    Vst0 = #vst{current=St,
			branched=gb_trees_from_list([{L,St} || L <- Ls1]),
			labels=gb_sets:from_list(Ls1++Ls2),
			ft=Ft},
	    MFA = {Mod,Name,Arity},
	    Vst = valfun(Is, MFA, Offset, Vst0),
	    validate_fun_info_branches(Ls1, MFA, Vst);
	true ->
	    error({{Mod,Name,Arity},{first(Is),Offset,no_entry_label}})
    end.

validate_fun_info_branches([L|Ls], MFA, #vst{branched=Branches}=Vst0) ->
    Vst = Vst0#vst{current=gb_trees:get(L, Branches)},
    validate_fun_info_branches_1(0, MFA, Vst),
    validate_fun_info_branches(Ls, MFA, Vst);
validate_fun_info_branches([], _, _) -> ok.

validate_fun_info_branches_1(Arity, {_,_,Arity}, _) -> ok;
validate_fun_info_branches_1(X, {Mod,Name,Arity}=MFA, Vst) ->
    try
        case Vst of
            #vst{current=#st{numy=none}} ->
                ok;
            #vst{current=#st{numy=Size}} ->
                error({unexpected_stack_frame,Size})
        end,
	get_term_type({x,X}, Vst)
    catch Error ->
	    I = {func_info,{atom,Mod},{atom,Name},Arity},
	    Offset = 2,
	    error({MFA,{I,Offset,Error}})
    end,
    validate_fun_info_branches_1(X+1, MFA, Vst).

first([X|_]) -> X;
first([]) -> [].

labels(Is) ->
    labels_1(Is, []).

labels_1([{label,L}|Is], R) ->
    labels_1(Is, [L|R]);
labels_1([{line,_}|Is], R) ->
    labels_1(Is, R);
labels_1(Is, R) ->
    {reverse(R),Is}.

init_state(Arity) ->
    Xs = init_regs(Arity, term),
    Ys = init_regs(0, initialized),
    kill_heap_allocation(#st{x=Xs,y=Ys,numy=none,ct=[]}).

kill_heap_allocation(St) ->
    St#st{h=0,hf=0}.

init_regs(0, _) ->
    gb_trees:empty();
init_regs(N, Type) ->
    gb_trees_from_list([{R,Type} || R <- lists:seq(0, N-1)]).

valfun([], MFA, _Offset, #vst{branched=Targets0,labels=Labels0}=Vst) ->
    Targets = gb_trees:keys(Targets0),
    Labels = gb_sets:to_list(Labels0),
    case Targets -- Labels of
	[] -> Vst;
	Undef ->
	    Error = {undef_labels,Undef},
	    error({MFA,Error})
    end;
valfun([I|Is], MFA, Offset, Vst0) ->
    valfun(Is, MFA, Offset+1,
	   try
	       Vst = val_dsetel(I, Vst0),
	       valfun_1(I, Vst)
	   catch Error ->
		   error({MFA,{I,Offset,Error}})
	   end).

%% Instructions that are allowed in dead code or when failing,
%% that is while the state is undecided in some way.
valfun_1({label,Lbl}, #vst{current=St0,branched=B,labels=Lbls}=Vst) ->
    St = merge_states(Lbl, St0, B),
    Vst#vst{current=St,branched=gb_trees:enter(Lbl, St, B),
	    labels=gb_sets:add(Lbl, Lbls)};
valfun_1(_I, #vst{current=none}=Vst) ->
    %% Ignore instructions after erlang:error/1,2, which
    %% the original R10B compiler thought would return.
    Vst;
valfun_1({badmatch,Src}, Vst) ->
    assert_not_fragile(Src, Vst),
    verify_y_init(Vst),
    kill_state(Vst);
valfun_1({case_end,Src}, Vst) ->
    assert_not_fragile(Src, Vst),
    verify_y_init(Vst),
    kill_state(Vst);
valfun_1(if_end, Vst) ->
    verify_y_init(Vst),
    kill_state(Vst);
valfun_1({try_case_end,Src}, Vst) ->
    verify_y_init(Vst),
    assert_not_fragile(Src, Vst),
    kill_state(Vst);
%% Instructions that cannot cause exceptions
valfun_1({bs_get_tail,Ctx,Dst,Live}, Vst0) ->
    bsm_validate_context(Ctx, Vst0),
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    Vst = prune_x_regs(Live, Vst0),
    extract_term(binary, [Ctx], Dst, Vst, Vst0);
valfun_1(bs_init_writable=I, Vst) ->
    call(I, 1, Vst);
valfun_1(build_stacktrace=I, Vst) ->
    call(I, 1, Vst);
valfun_1({move,Src,Dst}, Vst) ->
    assign(Src, Dst, Vst);
valfun_1({fmove,Src,{fr,_}=Dst}, Vst) ->
    assert_type(float, Src, Vst),
    set_freg(Dst, Vst);
valfun_1({fmove,{fr,_}=Src,Dst}, Vst0) ->
    assert_freg_set(Src, Vst0),
    assert_fls(checked, Vst0),
    Vst = eat_heap_float(Vst0),
    create_term({float,[]}, Dst, Vst);
valfun_1({kill,{y,_}=Reg}, Vst) ->
    set_type_y(initialized, Reg, Vst);
valfun_1({init,{y,_}=Reg}, Vst) ->
    set_type_y(initialized, Reg, Vst);
valfun_1({test_heap,Heap,Live}, Vst) ->
    test_heap(Heap, Live, Vst);
valfun_1({bif,Op,{f,_},Src,Dst}=I, Vst) ->
    case is_bif_safe(Op, length(Src)) of
	false ->
	    %% Since the BIF can fail, make sure that any catch state
	    %% is updated.
	    valfun_2(I, Vst);
	true ->
	    %% It can't fail, so we finish handling it here (not updating
	    %% catch state).
	    validate_src(Src, Vst),
	    Type = bif_type(Op, Src, Vst),
	    set_type_reg_expr(Type, I, Dst, Vst)
    end;
%% Put instructions.
valfun_1({put_list,A,B,Dst}, Vst0) ->
    assert_not_fragile(A, Vst0),
    assert_not_fragile(B, Vst0),
    Vst = eat_heap(2, Vst0),
    create_term(cons, Dst, Vst);
valfun_1({put_tuple2,Dst,{list,Elements}}, Vst0) ->
    _ = [assert_not_fragile(El, Vst0) || El <- Elements],
    Size = length(Elements),
    Vst = eat_heap(Size+1, Vst0),
    {Es,_} = foldl(fun(Val, {Es0, Index}) ->
                       Type = get_term_type(Val, Vst0),
                       Es = set_element_type(Index, Type, Es0),
                       {Es, Index + 1}
               end, {#{}, 1}, Elements),
    Type = {tuple,Size,Es},
    create_term(Type, Dst, Vst);
valfun_1({put_tuple,Sz,Dst}, Vst0) when is_integer(Sz) ->
    Vst1 = eat_heap(1, Vst0),
    Vst = create_term(tuple_in_progress, Dst, Vst1),
    #vst{current=St0} = Vst,
    St = St0#st{puts_left={Sz,{Dst,Sz,#{}}}},
    Vst#vst{current=St};
valfun_1({put,Src}, Vst0) ->
    assert_not_fragile(Src, Vst0),
    Vst = eat_heap(1, Vst0),
    #vst{current=St0} = Vst,
    case St0 of
        #st{puts_left=none} ->
            error(not_building_a_tuple);
        #st{puts_left={1,{Dst,Sz,Es}}} ->
            St = St0#st{puts_left=none},
            create_term({tuple,Sz,Es}, Dst, Vst#vst{current=St});
        #st{puts_left={PutsLeft,{Dst,Sz,Es0}}} when is_integer(PutsLeft) ->
            Index = Sz - PutsLeft + 1,
            Es = Es0#{ Index => get_term_type(Src, Vst0) },
            St = St0#st{puts_left={PutsLeft-1,{Dst,Sz,Es}}},
            Vst#vst{current=St}
    end;
%% Instructions for optimization of selective receives.
valfun_1({recv_mark,{f,Fail}}, Vst) when is_integer(Fail) ->
    Vst;
valfun_1({recv_set,{f,Fail}}, Vst) when is_integer(Fail) ->
    Vst;
%% Misc.
valfun_1(remove_message, Vst) ->
    %% The message term is no longer fragile. It can be used
    %% without restrictions.
    remove_fragility(Vst);
valfun_1({'%', {type_info, Reg, match_context}}, Vst) ->
    update_type(fun meet/2, #ms{}, Reg, Vst);
valfun_1({'%', {type_info, Reg, Type}}, Vst) ->
    %% Explicit type information inserted by optimization passes to indicate
    %% that Reg has a certain type, so that we can accept cross-function type
    %% optimizations.
    update_type(fun meet/2, Type, Reg, Vst);
valfun_1({'%',_}, Vst) ->
    Vst;
valfun_1({line,_}, Vst) ->
    Vst;
%% Exception generating calls
valfun_1({call_ext,Live,Func}=I, Vst) ->
    case return_type(Func, Vst) of
	exception ->
	    verify_live(Live, Vst),
            %% The stack will be scanned, so Y registers
            %% must be initialized.
            verify_y_init(Vst),
	    kill_state(Vst);
	_ ->
	    valfun_2(I, Vst)
    end;
valfun_1(_I, #vst{current=#st{ct=undecided}}) ->
    error(unknown_catch_try_state);
%%
%% Allocate and deallocate, et.al
valfun_1({allocate,Stk,Live}, Vst) ->
    allocate(false, Stk, 0, Live, Vst);
valfun_1({allocate_heap,Stk,Heap,Live}, Vst) ->
    allocate(false, Stk, Heap, Live, Vst);
valfun_1({allocate_zero,Stk,Live}, Vst) ->
    allocate(true, Stk, 0, Live, Vst);
valfun_1({allocate_heap_zero,Stk,Heap,Live}, Vst) ->
    allocate(true, Stk, Heap, Live, Vst);
valfun_1({deallocate,StkSize}, #vst{current=#st{numy=StkSize}}=Vst) ->
    verify_no_ct(Vst),
    deallocate(Vst);
valfun_1({deallocate,_}, #vst{current=#st{numy=NumY}}) ->
    error({allocated,NumY});
valfun_1({trim,N,Remaining}, #vst{current=#st{y=Yregs0,numy=NumY}=St}=Vst) ->
    if
	N =< NumY, N+Remaining =:= NumY ->
	    Yregs1 = [{Y-N,Type} || {Y,Type} <- gb_trees:to_list(Yregs0), Y >= N],
	    Yregs = gb_trees_from_list(Yregs1),
	    Vst#vst{current=St#st{y=Yregs,numy=NumY-N,aliases=#{}}};
	true ->
	    error({trim,N,Remaining,allocated,NumY})
    end;
%% Catch & try.
valfun_1({'catch',Dst,{f,Fail}}, Vst) when Fail =/= none ->
    init_try_catch_branch(catchtag, Dst, Fail, Vst);
valfun_1({'try',Dst,{f,Fail}}, Vst)  when Fail =/= none ->
    init_try_catch_branch(trytag, Dst, Fail, Vst);
valfun_1({catch_end,Reg}, #vst{current=#st{ct=[Fail|Fails]}}=Vst0) ->
    case get_special_y_type(Reg, Vst0) of
	{catchtag,Fail} ->
	    Vst = #vst{current=St} = set_catch_end(Reg, Vst0),
            Xregs = gb_trees:enter(0, term, St#st.x),
	    Vst#vst{current=St#st{x=Xregs,ct=Fails,fls=undefined,aliases=#{}}};
	Type ->
	    error({bad_type,Type})
    end;
valfun_1({try_end,Reg}, #vst{current=#st{ct=[Fail|Fails]}=St0}=Vst) ->
    case get_special_y_type(Reg, Vst) of
	{trytag,Fail} ->
	    St = St0#st{ct=Fails,fls=undefined},
	    set_catch_end(Reg, Vst#vst{current=St});
	Type ->
	    error({bad_type,Type})
    end;
valfun_1({try_case,Reg}, #vst{current=#st{ct=[Fail|Fails]}}=Vst0) ->
    case get_special_y_type(Reg, Vst0) of
	{trytag,Fail} ->
	    Vst = #vst{current=St} = set_catch_end(Reg, Vst0),
	    Xs = gb_trees_from_list([{0,{atom,[]}},{1,term},{2,term}]),
	    Vst#vst{current=St#st{x=Xs,ct=Fails,fls=undefined,aliases=#{}}};
	Type ->
	    error({bad_type,Type})
    end;
valfun_1({get_list,Src,D1,D2}, Vst0) ->
    assert_not_literal(Src),
    assert_type(cons, Src, Vst0),
    Vst = extract_term(term, [Src], D1, Vst0),
    extract_term(term, [Src], D2, Vst);
valfun_1({get_hd,Src,Dst}, Vst) ->
    assert_not_literal(Src),
    assert_type(cons, Src, Vst),
    extract_term(term, [Src], Dst, Vst);
valfun_1({get_tl,Src,Dst}, Vst) ->
    assert_not_literal(Src),
    assert_type(cons, Src, Vst),
    extract_term(term, [Src], Dst, Vst);
valfun_1({get_tuple_element,Src,N,Dst}, Vst) ->
    assert_not_literal(Src),
    assert_type({tuple_element,N+1}, Src, Vst),
    Type = get_element_type(N+1, Src, Vst),
    extract_term(Type, [Src], Dst, Vst);
valfun_1({jump,{f,Lbl}}, Vst) ->
    kill_state(branch_state(Lbl, Vst));
valfun_1(I, Vst) ->
    valfun_2(I, Vst).

init_try_catch_branch(Tag, Dst, Fail, Vst0) ->
    Vst1 = set_type_y({Tag,[Fail]}, Dst, Vst0),
    #vst{current=#st{ct=Fails}=St0} = Vst1,
    CurrentSt = St0#st{ct=[[Fail]|Fails]},

    %% Set the initial state at the try/catch label.
    %% Assume that Y registers contain terms or try/catch
    %% tags.
    Yregs0 = map(fun({Y,uninitialized}) -> {Y,term};
                    ({Y,initialized}) -> {Y,term};
                    (E) -> E
                 end, gb_trees:to_list(CurrentSt#st.y)),
    Yregs = gb_trees:from_orddict(Yregs0),
    BranchSt = CurrentSt#st{y=Yregs},

    Vst = branch_state(Fail, Vst1#vst{current=BranchSt}),
    Vst#vst{current=CurrentSt}.

%% Update branched state if necessary and try next set of instructions.
valfun_2(I, #vst{current=#st{ct=[]}}=Vst) ->
    valfun_3(I, Vst);
valfun_2(I, #vst{current=#st{ct=[[Fail]|_]}}=Vst) when is_integer(Fail) ->
    %% Update branched state.
    valfun_3(I, branch_state(Fail, Vst));
valfun_2(_, _) ->
    error(ambiguous_catch_try_state).

%% Handle the remaining floating point instructions here.
%% Floating point.
valfun_3({fconv,Src,{fr,_}=Dst}, Vst) ->
    assert_term(Src, Vst),
    set_freg(Dst, Vst);
valfun_3({bif,fadd,_,[_,_]=Src,Dst}, Vst) ->
    float_op(Src, Dst, Vst);
valfun_3({bif,fdiv,_,[_,_]=Src,Dst}, Vst) ->
    float_op(Src, Dst, Vst);
valfun_3({bif,fmul,_,[_,_]=Src,Dst}, Vst) ->
    float_op(Src, Dst, Vst);
valfun_3({bif,fnegate,_,[_]=Src,Dst}, Vst) ->
    float_op(Src, Dst, Vst);
valfun_3({bif,fsub,_,[_,_]=Src,Dst}, Vst) ->
    float_op(Src, Dst, Vst);
valfun_3(fclearerror, Vst) ->
    case get_fls(Vst) of
	undefined -> ok;
	checked -> ok;
	Fls -> error({bad_floating_point_state,Fls})
    end,
    set_fls(cleared, Vst);
valfun_3({fcheckerror,_}, Vst) ->
    assert_fls(cleared, Vst),
    set_fls(checked, Vst);
valfun_3(I, Vst) ->
    %% The instruction is not a float instruction.
    case get_fls(Vst) of
	undefined ->
	    valfun_4(I, Vst);
	checked ->
	    valfun_4(I, Vst);
	Fls ->
	    error({unsafe_instruction,{float_error_state,Fls}})
    end.

%% Instructions that can cause exceptions.
valfun_4({apply,Live}, Vst) ->
    call(apply, Live+2, Vst);
valfun_4({apply_last,Live,_}, Vst) ->
    tail_call(apply, Live+2, Vst);
valfun_4({call_fun,Live}, Vst) ->
    validate_src([{x,Live}], Vst),
    call('fun', Live+1, Vst);
valfun_4({call,Live,Func}, Vst) ->
    call(Func, Live, Vst);
valfun_4({call_ext,Live,Func}, Vst) ->
    %% Exception BIFs has already been taken care of above.
    call(Func, Live, Vst);
valfun_4({call_only,Live,Func}, Vst) ->
    tail_call(Func, Live, Vst);
valfun_4({call_ext_only,Live,Func}, Vst) ->
    tail_call(Func, Live, Vst);
valfun_4({call_last,Live,Func,StkSize}, #vst{current=#st{numy=StkSize}}=Vst) ->
    tail_call(Func, Live, Vst);
valfun_4({call_last,_,_,_}, #vst{current=#st{numy=NumY}}) ->
    error({allocated,NumY});
valfun_4({call_ext_last,Live,Func,StkSize}, 
	 #vst{current=#st{numy=StkSize}}=Vst) ->
    tail_call(Func, Live, Vst);
valfun_4({call_ext_last,_,_,_}, #vst{current=#st{numy=NumY}}) ->
    error({allocated,NumY});
valfun_4({make_fun2,_,_,_,Live}, Vst) ->
    call(make_fun, Live, Vst);
%% Other BIFs
valfun_4({bif,tuple_size,{f,Fail},[Tuple],Dst}=I, Vst0) ->
    Vst1 = branch_state(Fail, Vst0),
    Vst = update_type(fun meet/2, {tuple,[0],#{}}, Tuple, Vst1),
    set_type_reg_expr({integer,[]}, I, Dst, Vst);
valfun_4({bif,element,{f,Fail},[Pos,Tuple],Dst}, Vst0) ->
    PosType = get_durable_term_type(Pos, Vst0),
    ElementType = case PosType of
                      {integer,I} -> get_element_type(I, Tuple, Vst0);
                      _ -> term
                  end,
    InferredType = {tuple,[get_tuple_size(PosType)],#{}},
    Vst1 = branch_state(Fail, Vst0),
    Vst = update_type(fun meet/2, InferredType, Tuple, Vst1),
    extract_term(ElementType, [Tuple], Dst, Vst);
valfun_4({bif,raise,{f,0},Src,_Dst}, Vst) ->
    validate_src(Src, Vst),
    kill_state(Vst);
valfun_4(raw_raise=I, Vst) ->
    call(I, 3, Vst);
valfun_4({bif,map_get,{f,Fail},[_Key,Map]=Ss,Dst}, Vst0) ->
    validate_src(Ss, Vst0),
    Vst1 = branch_state(Fail, Vst0),
    Vst = update_type(fun meet/2, map, Map, Vst1),
    extract_term(term, Ss, Dst, Vst);
valfun_4({bif,is_map_key,{f,Fail},[_Key,Map]=Ss,Dst}, Vst0) ->
    validate_src(Ss, Vst0),
    Vst1 = branch_state(Fail, Vst0),
    Vst = update_type(fun meet/2, map, Map, Vst1),
    extract_term(bool, Ss, Dst, Vst);
valfun_4({bif,Op,{f,Fail},[Cons]=Ss,Dst}, Vst0)
  when Op =:= hd; Op =:= tl ->
    validate_src(Ss, Vst0),
    Vst1 = branch_state(Fail, Vst0),
    Vst = update_type(fun meet/2, cons, Cons, Vst1),
    Type = bif_type(Op, Ss, Vst),
    extract_term(Type, Ss, Dst, Vst);
valfun_4({bif,Op,{f,Fail},Ss,Dst}, Vst0) ->
    validate_src(Ss, Vst0),
    Vst = branch_state(Fail, Vst0),
    Type = bif_type(Op, Ss, Vst),
    extract_term(Type, Ss, Dst, Vst);
valfun_4({gc_bif,Op,{f,Fail},Live,Ss,Dst}, #vst{current=St0}=Vst0) ->
    validate_src(Ss, Vst0),
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    St = kill_heap_allocation(St0),
    Vst1 = Vst0#vst{current=St},
    Vst2 = branch_state(Fail, Vst1),
    Vst3 = case Op of
              length -> update_type(fun meet/2, list, hd(Ss), Vst2);
              map_size -> update_type(fun meet/2, map, hd(Ss), Vst2);
              _ -> Vst2
          end,
    Type = bif_type(Op, Ss, Vst3),
    Vst = prune_x_regs(Live, Vst3),
    extract_term(Type, Ss, Dst, Vst, Vst0);
valfun_4(return, #vst{current=#st{numy=none}}=Vst) ->
    assert_not_fragile({x,0}, Vst),
    kill_state(Vst);
valfun_4(return, #vst{current=#st{numy=NumY}}) ->
    error({stack_frame,NumY});
valfun_4({loop_rec,{f,Fail},Dst}, Vst0) ->
    Vst = branch_state(Fail, Vst0),
    %% This term may not be part of the root set until
    %% remove_message/0 is executed. If control transfers
    %% to the loop_rec_end/1 instruction, no part of
    %% this term must be stored in a Y register.
    create_term({fragile,term}, Dst, Vst);
valfun_4({wait,_}, Vst) ->
    verify_y_init(Vst),
    kill_state(Vst);
valfun_4({wait_timeout,_,Src}, Vst) ->
    assert_term(Src, Vst),
    verify_y_init(Vst),
    prune_x_regs(0, Vst);
valfun_4({loop_rec_end,_}, Vst) ->
    verify_y_init(Vst),
    kill_state(Vst);
valfun_4(timeout, #vst{current=St}=Vst) ->
    Vst#vst{current=St#st{x=init_regs(0, term)}};
valfun_4(send, Vst) ->
    call(send, 2, Vst);
valfun_4({set_tuple_element,Src,Tuple,N}, Vst) ->
    I = N + 1,
    assert_not_fragile(Src, Vst),
    assert_type({tuple_element,I}, Tuple, Vst),
    {tuple, Sz, Es0} = get_term_type(Tuple, Vst),
    Es = set_element_type(I, get_term_type(Src, Vst), Es0),
    set_aliased_type({tuple, Sz, Es}, Tuple, Vst);
%% Match instructions.
valfun_4({select_val,Src,{f,Fail},{list,Choices}}, Vst0) ->
    assert_term(Src, Vst0),
    assert_choices(Choices),
    Vst = branch_state(Fail, Vst0),
    kill_state(select_val_branches(Src, Choices, Vst));
valfun_4({select_tuple_arity,Tuple,{f,Fail},{list,Choices}}, Vst) ->
    assert_type(tuple, Tuple, Vst),
    assert_arities(Choices),
    TupleType = get_durable_term_type(Tuple, Vst),
    kill_state(branch_arities(Choices, Tuple, TupleType,
                              branch_state(Fail, Vst)));

%% New bit syntax matching instructions.
valfun_4({test,bs_start_match3,{f,Fail},Live,[Src],Dst}, Vst) ->
    validate_bs_start_match(Fail, Live, bsm_match_state(), Src, Dst, Vst);
valfun_4({test,bs_start_match2,{f,Fail},Live,[Src,Slots],Dst}, Vst) ->
    validate_bs_start_match(Fail, Live, bsm_match_state(Slots), Src, Dst, Vst);
valfun_4({test,bs_match_string,{f,Fail},[Ctx,_,_]}, Vst) ->
    bsm_validate_context(Ctx, Vst),
    branch_state(Fail, Vst);
valfun_4({test,bs_skip_bits2,{f,Fail},[Ctx,Src,_,_]}, Vst) ->
    bsm_validate_context(Ctx, Vst),
    assert_term(Src, Vst),
    branch_state(Fail, Vst);
valfun_4({test,bs_test_tail2,{f,Fail},[Ctx,_]}, Vst) ->
    bsm_validate_context(Ctx, Vst),
    branch_state(Fail, Vst);
valfun_4({test,bs_test_unit,{f,Fail},[Ctx,_]}, Vst) ->
    bsm_validate_context(Ctx, Vst),
    branch_state(Fail, Vst);
valfun_4({test,bs_skip_utf8,{f,Fail},[Ctx,Live,_]}, Vst) ->
    validate_bs_skip_utf(Fail, Ctx, Live, Vst);
valfun_4({test,bs_skip_utf16,{f,Fail},[Ctx,Live,_]}, Vst) ->
    validate_bs_skip_utf(Fail, Ctx, Live, Vst);
valfun_4({test,bs_skip_utf32,{f,Fail},[Ctx,Live,_]}, Vst) ->
    validate_bs_skip_utf(Fail, Ctx, Live, Vst);
valfun_4({test,bs_get_integer2,{f,Fail},Live,[Ctx,_,_,_],Dst}, Vst) ->
    validate_bs_get(Fail, Ctx, Live, {integer, []}, Dst, Vst);
valfun_4({test,bs_get_float2,{f,Fail},Live,[Ctx,_,_,_],Dst}, Vst) ->
    validate_bs_get(Fail, Ctx, Live, {float, []}, Dst, Vst);
valfun_4({test,bs_get_binary2,{f,Fail},Live,[Ctx,_,_,_],Dst}, Vst) ->
    Type = propagate_fragility(term, [Ctx], Vst),
    validate_bs_get(Fail, Ctx, Live, Type, Dst, Vst);
valfun_4({test,bs_get_utf8,{f,Fail},Live,[Ctx,_],Dst}, Vst) ->
    validate_bs_get(Fail, Ctx, Live, {integer, []}, Dst, Vst);
valfun_4({test,bs_get_utf16,{f,Fail},Live,[Ctx,_],Dst}, Vst) ->
    validate_bs_get(Fail, Ctx, Live, {integer, []}, Dst, Vst);
valfun_4({test,bs_get_utf32,{f,Fail},Live,[Ctx,_],Dst}, Vst) ->
    validate_bs_get(Fail, Ctx, Live, {integer, []}, Dst, Vst);
valfun_4({bs_save2,Ctx,SavePoint}, Vst) ->
    bsm_save(Ctx, SavePoint, Vst);
valfun_4({bs_restore2,Ctx,SavePoint}, Vst) ->
    bsm_restore(Ctx, SavePoint, Vst);
valfun_4({bs_get_position, Ctx, Dst, Live}, Vst0) ->
    bsm_validate_context(Ctx, Vst0),
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    Vst = prune_x_regs(Live, Vst0),
    create_term(bs_position, Dst, Vst);
valfun_4({bs_set_position, Ctx, Pos}, Vst) ->
    bsm_validate_context(Ctx, Vst),
    assert_type(bs_position, Pos, Vst),
    Vst;

%% Other test instructions.
valfun_4({test,is_atom,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, {atom,[]}, Src, Vst);
valfun_4({test,is_boolean,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, bool, Src, Vst);
valfun_4({test,is_float,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, {float,[]}, Src, Vst);
valfun_4({test,is_tuple,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, {tuple,[0],#{}}, Src, Vst);
valfun_4({test,is_integer,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, {integer,[]}, Src, Vst);
valfun_4({test,is_nonempty_list,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, cons, Src, Vst);
valfun_4({test,is_list,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, list, Src, Vst);
valfun_4({test,is_nil,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, nil, Src, Vst);
valfun_4({test,is_map,{f,Lbl},[Src]}, Vst) ->
    case Src of
        {Tag,_} when Tag =:= x; Tag =:= y ->
            type_test(Lbl, map, Src, Vst);
        {literal,Map} when is_map(Map) ->
            Vst;
        _ ->
            assert_term(Src, Vst),
            kill_state(Vst)
    end;
valfun_4({test,test_arity,{f,Lbl},[Tuple,Sz]}, Vst) when is_integer(Sz) ->
    assert_type(tuple, Tuple, Vst),
    update_type(fun meet/2, {tuple,Sz,#{}}, Tuple, branch_state(Lbl, Vst));
valfun_4({test,is_tagged_tuple,{f,Lbl},[Src,Sz,Atom]}, Vst0) ->
    assert_term(Src, Vst0),
    Vst = branch_state(Lbl, Vst0),
    update_type(fun meet/2, {tuple,Sz,#{ 1 => Atom }}, Src, Vst);
valfun_4({test,has_map_fields,{f,Lbl},Src,{list,List}}, Vst) ->
    assert_type(map, Src, Vst),
    assert_unique_map_keys(List),
    branch_state(Lbl, Vst);
valfun_4({test,is_eq_exact,{f,Lbl},[Src,Val]=Ss}, Vst0) ->
    validate_src(Ss, Vst0),
    Infer = infer_types(Src, Vst0),
    Vst1 = Infer(Val, Vst0),
    Vst2 = update_ne_types(Src, Val, Vst1),
    Vst3 = branch_state(Lbl, Vst2),
    Vst = Vst3#vst{current=Vst1#vst.current},
    update_eq_types(Src, Val, Vst);
valfun_4({test,is_ne_exact,{f,Lbl},[Src,Val]=Ss}, Vst0) ->
    validate_src(Ss, Vst0),
    Vst1 = update_eq_types(Src, Val, Vst0),
    Vst2 = branch_state(Lbl, Vst1),
    Vst = Vst2#vst{current=Vst0#vst.current},
    update_ne_types(Src, Val, Vst);
valfun_4({test,_Op,{f,Lbl},Src}, Vst) ->
    validate_src(Src, Vst),
    branch_state(Lbl, Vst);
valfun_4({bs_add,{f,Fail},[A,B,_],Dst}, Vst) ->
    assert_not_fragile(A, Vst),
    assert_not_fragile(B, Vst),
    create_term({integer,[]}, Dst, branch_state(Fail, Vst));
valfun_4({bs_utf8_size,{f,Fail},A,Dst}, Vst) ->
    assert_term(A, Vst),
    create_term({integer,[]}, Dst, branch_state(Fail, Vst));
valfun_4({bs_utf16_size,{f,Fail},A,Dst}, Vst) ->
    assert_term(A, Vst),
    create_term({integer,[]}, Dst, branch_state(Fail, Vst));
valfun_4({bs_init2,{f,Fail},Sz,Heap,Live,_,Dst}, Vst0) ->
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    if
	is_integer(Sz) ->
	    ok;
	true ->
	    assert_not_fragile(Sz, Vst0)
    end,
    Vst1 = heap_alloc(Heap, Vst0),
    Vst2 = branch_state(Fail, Vst1),
    Vst = prune_x_regs(Live, Vst2),
    create_term(binary, Dst, Vst);
valfun_4({bs_init_bits,{f,Fail},Sz,Heap,Live,_,Dst}, Vst0) ->
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    if
	is_integer(Sz) ->
	    ok;
	true ->
	    assert_term(Sz, Vst0)
    end,
    Vst1 = heap_alloc(Heap, Vst0),
    Vst2 = branch_state(Fail, Vst1),
    Vst = prune_x_regs(Live, Vst2),
    create_term(binary, Dst, Vst);
valfun_4({bs_append,{f,Fail},Bits,Heap,Live,_Unit,Bin,_Flags,Dst}, Vst0) ->
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    assert_not_fragile(Bits, Vst0),
    assert_not_fragile(Bin, Vst0),
    Vst1 = heap_alloc(Heap, Vst0),
    Vst2 = branch_state(Fail, Vst1),
    Vst = prune_x_regs(Live, Vst2),
    create_term(binary, Dst, Vst);
valfun_4({bs_private_append,{f,Fail},Bits,_Unit,Bin,_Flags,Dst}, Vst0) ->
    assert_not_fragile(Bits, Vst0),
    assert_not_fragile(Bin, Vst0),
    Vst = branch_state(Fail, Vst0),
    create_term(binary, Dst, Vst);
valfun_4({bs_put_string,Sz,_}, Vst) when is_integer(Sz) ->
    Vst;
valfun_4({bs_put_binary,{f,Fail},Sz,_,_,Src}, Vst) ->
    assert_not_fragile(Sz, Vst),
    assert_not_fragile(Src, Vst),
    branch_state(Fail, Vst);
valfun_4({bs_put_float,{f,Fail},Sz,_,_,Src}, Vst) ->
    assert_not_fragile(Sz, Vst),
    assert_not_fragile(Src, Vst),
    branch_state(Fail, Vst);
valfun_4({bs_put_integer,{f,Fail},Sz,_,_,Src}, Vst) ->
    assert_not_fragile(Sz, Vst),
    assert_not_fragile(Src, Vst),
    branch_state(Fail, Vst);
valfun_4({bs_put_utf8,{f,Fail},_,Src}, Vst) ->
    assert_not_fragile(Src, Vst),
    branch_state(Fail, Vst);
valfun_4({bs_put_utf16,{f,Fail},_,Src}, Vst) ->
    assert_not_fragile(Src, Vst),
    branch_state(Fail, Vst);
valfun_4({bs_put_utf32,{f,Fail},_,Src}, Vst) ->
    assert_not_fragile(Src, Vst),
    branch_state(Fail, Vst);
%% Map instructions.
valfun_4({put_map_assoc,{f,Fail},Src,Dst,Live,{list,List}}, Vst) ->
    verify_put_map(Fail, Src, Dst, Live, List, Vst);
valfun_4({put_map_exact,{f,Fail},Src,Dst,Live,{list,List}}, Vst) ->
    verify_put_map(Fail, Src, Dst, Live, List, Vst);
valfun_4({get_map_elements,{f,Fail},Src,{list,List}}, Vst) ->
    verify_get_map(Fail, Src, List, Vst);
valfun_4(_, _) ->
    error(unknown_instruction).

verify_get_map(Fail, Src, List, Vst0) ->
    assert_not_literal(Src),                    %OTP 22.
    assert_type(map, Src, Vst0),
    Vst1 = foldl(fun(D, Vsti) ->
                         case is_reg_defined(D,Vsti) of
                             true -> create_term(term, D, Vsti);
                             false -> Vsti
                         end
                 end, Vst0, extract_map_vals(List)),
    Vst2 = branch_state(Fail, Vst1),
    Keys = extract_map_keys(List),
    assert_unique_map_keys(Keys),
    verify_get_map_pair(List, Src, Vst0, Vst2).

extract_map_vals([_Key,Val|T]) ->
    [Val|extract_map_vals(T)];
extract_map_vals([]) -> [].

extract_map_keys([Key,_Val|T]) ->
    [Key|extract_map_keys(T)];
extract_map_keys([]) -> [].

verify_get_map_pair([Src,Dst|Vs], Map, Vst0, Vsti0) ->
    assert_term(Src, Vst0),
    Vsti = extract_term(term, [Map], Dst, Vsti0),
    verify_get_map_pair(Vs, Map, Vst0, Vsti);
verify_get_map_pair([], _Map, _Vst0, Vst) -> Vst.

verify_put_map(Fail, Src, Dst, Live, List, Vst0) ->
    assert_type(map, Src, Vst0),
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    foreach(fun (Term) -> assert_not_fragile(Term, Vst0) end, List),
    Vst1 = heap_alloc(0, Vst0),
    Vst2 = branch_state(Fail, Vst1),
    Vst = prune_x_regs(Live, Vst2),
    Keys = extract_map_keys(List),
    assert_unique_map_keys(Keys),
    create_term(map, Dst, Vst).

%%
%% Common code for validating bs_start_match* instructions.
%%

validate_bs_start_match(Fail, Live, Type, Src, Dst, Vst0) ->
    verify_live(Live, Vst0),
    verify_y_init(Vst0),

    %% #ms{} can represent either a match context or a term, so we have to mark
    %% the source as a term if it fails, and retain the incoming type if it
    %% succeeds (match context or not).
    Vst1 = set_aliased_type(term, Src, Vst0),
    Vst2 = prune_x_regs(Live, Vst1),
    Vst3 = branch_state(Fail, Vst2),
    extract_term(Type, [Src], Dst, Vst3, Vst0).

%%
%% Common code for validating bs_get* instructions.
%%
validate_bs_get(Fail, Ctx, Live, Type, Dst, Vst0) ->
    bsm_validate_context(Ctx, Vst0),
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    Vst1 = prune_x_regs(Live, Vst0),
    Vst = branch_state(Fail, Vst1),
    create_term(Type, Dst, Vst).

%%
%% Common code for validating bs_skip_utf* instructions.
%%
validate_bs_skip_utf(Fail, Ctx, Live, Vst0) ->
    bsm_validate_context(Ctx, Vst0),
    verify_y_init(Vst0),
    verify_live(Live, Vst0),
    Vst = prune_x_regs(Live, Vst0),
    branch_state(Fail, Vst).

%%
%% Special state handling for setelement/3 and set_tuple_element/3 instructions.
%% A possibility for garbage collection must not occur between setelement/3 and
%% set_tuple_element/3.
%%
%% Note that #vst.current will be 'none' if the instruction is unreachable.
%%
val_dsetel({move,_,_}, Vst) ->
    Vst;
val_dsetel({call_ext,3,{extfunc,erlang,setelement,3}}, #vst{current=#st{}=St}=Vst) ->
    Vst#vst{current=St#st{setelem=true}};
val_dsetel({set_tuple_element,_,_,_}, #vst{current=#st{setelem=false}}) ->
    error(illegal_context_for_set_tuple_element);
val_dsetel({set_tuple_element,_,_,_}, #vst{current=#st{setelem=true}}=Vst) ->
    Vst;
val_dsetel({get_tuple_element,_,_,_}, Vst) ->
    Vst;
val_dsetel({line,_}, Vst) ->
    Vst;
val_dsetel(_, #vst{current=#st{setelem=true}=St}=Vst) ->
    Vst#vst{current=St#st{setelem=false}};
val_dsetel(_, Vst) -> Vst.

kill_state(Vst) ->
    Vst#vst{current=none}.

%% A "plain" call.
%%  The stackframe must be initialized.
%%  The instruction will return to the instruction following the call.
call(Name, Live, #vst{current=St}=Vst) ->
    verify_call_args(Name, Live, Vst),
    verify_y_init(Vst),
    case return_type(Name, Vst) of
	Type when Type =/= exception ->
	    %% Type is never 'exception' because it has been handled earlier.
	    Xs = gb_trees_from_list([{0,Type}]),
	    Vst#vst{current=St#st{x=Xs,f=init_fregs(),aliases=#{}}}
    end.

%% Tail call.
%%  The stackframe must have a known size and be initialized.
%%  Does not return to the instruction following the call.
tail_call(Name, Live, Vst0) ->
    verify_y_init(Vst0),
    Vst = deallocate(Vst0),
    verify_call_args(Name, Live, Vst),
    verify_no_ct(Vst),
    kill_state(Vst).

verify_call_args(_, 0, #vst{}) ->
    ok;
verify_call_args({f,Lbl}, Live, Vst) when is_integer(Live)->
    verify_local_call(Lbl, Live, Vst);
verify_call_args(_, Live, Vst) when is_integer(Live)->
    verify_call_args_1(Live, Vst);
verify_call_args(_, Live, _) ->
    error({bad_number_of_live_regs,Live}).

verify_call_args_1(0, _) -> ok;
verify_call_args_1(N, Vst) ->
    X = N - 1,
    assert_not_fragile({x,X}, Vst),
    verify_call_args_1(X, Vst).

verify_local_call(Lbl, Live, Vst) ->
    F = fun({R, Type}) ->
                verify_arg_type(Lbl, R, Type, Vst)
        end,
    TRegs = typed_call_regs(Live, Vst),
    verify_no_ms_aliases(TRegs),
    foreach(F, TRegs).

typed_call_regs(0, _Vst) ->
    [];
typed_call_regs(Live0, Vst) ->
    Live = Live0 - 1,
    R = {x,Live},
    [{R, get_move_term_type(R, Vst)} | typed_call_regs(Live, Vst)].

%% Verifies that the same match context isn't present twice.
verify_no_ms_aliases(Regs) ->
    CtxIds = [Id || {_, #ms{id=Id}} <- Regs],
    UniqueCtxIds = ordsets:from_list(CtxIds),
    if
        length(UniqueCtxIds) < length(CtxIds) ->
            error({multiple_match_contexts, Regs});
        length(UniqueCtxIds) =:= length(CtxIds) ->
            ok
    end.

%% Verifies that the given argument narrows to what the function expects.
verify_arg_type(Lbl, Reg, #ms{}, #vst{ft=Ft}) ->
    %% Match contexts require explicit support, and may not be passed to a
    %% function that accepts arbitrary terms.
    case gb_trees:lookup({Lbl, Reg}, Ft) of
        {value, #ms{}} -> ok;
        _ -> error(no_bs_start_match2)
    end;
verify_arg_type(Lbl, Reg, GivenType, #vst{ft=Ft}) ->
    case gb_trees:lookup({Lbl, Reg}, Ft) of
        {value, bool} when GivenType =:= {atom, true};
                           GivenType =:= {atom, false};
                           GivenType =:= {atom, []} ->
            %% We don't yet support upgrading true/false to bool, so we
            %% assume unknown atoms can be bools when validating calls.
            ok;
        {value, #ms{}} ->
            %% Functions that accept match contexts also accept all other
            %% terms. This will change once we support union types.
            ok;
        {value, RequiredType} ->
            case meet(GivenType, RequiredType) of
                none -> error({bad_arg_type, Reg, GivenType, RequiredType});
                _ -> ok
            end;
        none ->
            ok
    end.

allocate(Zero, Stk, Heap, Live, #vst{current=#st{numy=none}}=Vst0) ->
    verify_live(Live, Vst0),
    Vst = #vst{current=St} = prune_x_regs(Live, Vst0),
    Ys = init_regs(Stk, case Zero of 
			    true -> initialized;
			    false -> uninitialized
			end),
    heap_alloc(Heap, Vst#vst{current=St#st{y=Ys,numy=Stk}});
allocate(_, _, _, _, #vst{current=#st{numy=Numy}}) ->
    error({existing_stack_frame,{size,Numy}}).

deallocate(#vst{current=St}=Vst) ->
    Vst#vst{current=St#st{y=init_regs(0, initialized),numy=none}}.

test_heap(Heap, Live, Vst0) ->
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    Vst = prune_x_regs(Live, Vst0),
    heap_alloc(Heap, Vst).

heap_alloc(Heap, #vst{current=St0}=Vst) ->
    St1 = kill_heap_allocation(St0),
    St = heap_alloc_1(Heap, St1),
    Vst#vst{current=St}.

heap_alloc_1({alloc,Alloc}, St) ->
    heap_alloc_2(Alloc, St);
heap_alloc_1(HeapWords, St) when is_integer(HeapWords) ->
    St#st{h=HeapWords}.

heap_alloc_2([{words,HeapWords}|T], St0) ->
    St = St0#st{h=HeapWords},
    heap_alloc_2(T, St);
heap_alloc_2([{floats,Floats}|T], St0) ->
    St = St0#st{hf=Floats},
    heap_alloc_2(T, St);
heap_alloc_2([], St) -> St.

prune_x_regs(Live, #vst{current=St0}=Vst)
  when is_integer(Live) ->
    #st{x=Xs0,defs=Defs0,aliases=Aliases0} = St0,
    Xs1 = gb_trees:to_list(Xs0),
    Xs = [P || {R,_}=P <- Xs1, R < Live],
    Defs = maps:filter(fun({x,X}, _) -> X < Live;
                          ({y,_}, _) -> true
                       end, Defs0),
    Aliases = maps:filter(fun({x,X1}, {x,X2}) ->
                                  X1 < Live andalso X2 < Live;
                             ({x,X}, _) ->
                                  X < Live;
                             (_, {x,X}) ->
                                  X < Live;
                             (_, _) ->
                                  true
                          end, Aliases0),
    St = St0#st{x=gb_trees:from_orddict(Xs),defs=Defs,aliases=Aliases},
    Vst#vst{current=St}.

%% All choices in a select_val list must be integers, floats, or atoms.
%% All must be of the same type.
assert_choices([{Tag,_},{f,_}|T]) ->
    if
        Tag =:= atom; Tag =:= float; Tag =:= integer ->
            assert_choices_1(T, Tag);
        true ->
            error(bad_select_list)
    end;
assert_choices([]) -> ok.

assert_choices_1([{Tag,_},{f,_}|T], Tag) ->
    assert_choices_1(T, Tag);
assert_choices_1([_,{f,_}|_], _Tag) ->
    error(bad_select_list);
assert_choices_1([], _Tag) -> ok.

assert_arities([Arity,{f,_}|T]) when is_integer(Arity) ->
    assert_arities(T);
assert_arities([]) -> ok;
assert_arities(_) -> error(bad_tuple_arity_list).


%%%
%%% Floating point checking.
%%%
%%% Possible values for the fls field (=floating point error state).
%%%
%%% undefined 	- Undefined (initial state). No float operations allowed.
%%%
%%% cleared	- fclearerror/0 has been executed. Float operations
%%%		  are allowed (such as fadd).
%%%
%%% checked	- fcheckerror/1 has been executed. It is allowed to
%%%               move values out of floating point registers.
%%%
%%% The following instructions may be executed in any state:
%%%
%%%   fconv Src {fr,_}             
%%%   fmove Src {fr,_}		%% Move INTO floating point register.
%%%

float_op(Src, Dst, Vst0) ->
    foreach (fun(S) -> assert_freg_set(S, Vst0) end, Src),
    assert_fls(cleared, Vst0),
    Vst = set_fls(cleared, Vst0),
    set_freg(Dst, Vst).

assert_fls(Fls, Vst) ->
    case get_fls(Vst) of
	Fls -> ok;
	OtherFls -> error({bad_floating_point_state,OtherFls})
    end.

set_fls(Fls, #vst{current=#st{}=St}=Vst) when is_atom(Fls) ->
    Vst#vst{current=St#st{fls=Fls}}.

get_fls(#vst{current=#st{fls=Fls}}) when is_atom(Fls) -> Fls.

init_fregs() -> 0.

set_freg({fr,Fr}=Freg, #vst{current=#st{f=Fregs0}=St}=Vst)
  when is_integer(Fr), 0 =< Fr ->
    check_limit(Freg),
    Bit = 1 bsl Fr,
    if
	Fregs0 band Bit =:= 0 ->
	    Fregs = Fregs0 bor Bit,
	    Vst#vst{current=St#st{f=Fregs}};
	true -> Vst
    end;
set_freg(Fr, _) -> error({bad_target,Fr}).

assert_freg_set({fr,Fr}=Freg, #vst{current=#st{f=Fregs}})
  when is_integer(Fr), 0 =< Fr ->
    if
	(Fregs bsr Fr) band 1 =:= 0 ->
	    error({uninitialized_reg,Freg});
	true ->
	    ok
    end;
assert_freg_set(Fr, _) -> error({bad_source,Fr}).

%%% Maps

%% A single item list may be either a list or a register.
%%
%% A list with more than item must contain unique literals.
%%
%% An empty list is not allowed.

assert_unique_map_keys([]) ->
    %% There is no reason to use the get_map_elements and
    %% has_map_fields instructions with empty lists.
    error(empty_field_list);
assert_unique_map_keys([_]) ->
    ok;
assert_unique_map_keys([_,_|_]=Ls) ->
    Vs = [begin
              assert_literal(L),
              L
          end || L <- Ls],
    case length(Vs) =:= sets:size(sets:from_list(Vs)) of
	true -> ok;
	false -> error(keys_not_unique)
    end.

%%%
%%% New binary matching instructions.
%%%

bsm_match_state() ->
    #ms{}.
bsm_match_state(Slots) ->
    #ms{slots=Slots}.

bsm_validate_context(Reg, Vst) ->
    _ = bsm_get_context(Reg, Vst),
    ok.

bsm_get_context({x,X}=Reg, #vst{current=#st{x=Xs}}=_Vst) when is_integer(X) ->
    case gb_trees:lookup(X, Xs) of
	{value,#ms{}=Ctx} -> Ctx;
	{value,{fragile,#ms{}=Ctx}} -> Ctx;
	_ -> error({no_bsm_context,Reg})
    end;
bsm_get_context({y,Y}=Reg, #vst{current=#st{y=Ys}}=_Vst) when is_integer(Y) ->
    case gb_trees:lookup(Y, Ys) of
	{value,#ms{}=Ctx} -> Ctx;
	{value,{fragile,#ms{}=Ctx}} -> Ctx;
	_ -> error({no_bsm_context,Reg})
    end;
bsm_get_context(Reg, _) -> error({bad_source,Reg}).

bsm_save(Reg, {atom,start}, Vst) ->
    %% Save point refering to where the match started.
    %% It is always valid. But don't forget to validate the context register.
    bsm_validate_context(Reg, Vst),
    Vst;
bsm_save(Reg, SavePoint, Vst) ->
    case bsm_get_context(Reg, Vst) of
	#ms{valid=Bits,slots=Slots}=Ctxt0 when SavePoint < Slots ->
	    Ctx = Ctxt0#ms{valid=Bits bor (1 bsl SavePoint),slots=Slots},
	    set_type_reg(Ctx, Reg, Vst);
	_ -> error({illegal_save,SavePoint})
    end.

bsm_restore(Reg, {atom,start}, Vst) ->
    %% (Mostly) automatic save point refering to where the match started.
    %% It is always valid. But don't forget to validate the context register.
    bsm_validate_context(Reg, Vst),
    Vst;
bsm_restore(Reg, SavePoint, Vst) ->
    case bsm_get_context(Reg, Vst) of
	#ms{valid=Bits,slots=Slots} when SavePoint < Slots ->
	    case Bits band (1 bsl SavePoint) of
		0 -> error({illegal_restore,SavePoint,not_set});
		_ -> Vst
	    end;
	_ -> error({illegal_restore,SavePoint,range})
    end.

select_val_branches(Src, Choices, Vst) ->
    Infer = infer_types(Src, Vst),
    select_val_branches_1(Choices, Src, Infer, Vst).

select_val_branches_1([Val,{f,L}|T], Src, Infer, Vst0) ->
    Vst1 = set_aliased_type(Val, Src, Infer(Val, Vst0)),
    Vst = branch_state(L, Vst1),
    select_val_branches_1(T, Src, Infer, Vst);
select_val_branches_1([], _, _, Vst) -> Vst.

infer_types(Src, Vst) ->
    case get_def(Src, Vst) of
        {bif,is_map,{f,_},[Map],_} ->
            fun({atom,true}, S) -> update_type(fun meet/2, map, Map, S);
               (_, S) -> S
            end;
        {bif,tuple_size,{f,_},[Tuple],_} ->
            fun({integer,Arity}, S) ->
                    update_type(fun meet/2, {tuple,Arity,#{}}, Tuple, S);
               (_, S) -> S
            end;
        {bif,'=:=',{f,_},[ArityReg,{integer,_}=Val],_} when ArityReg =/= Src ->
            fun({atom,true}, S) ->
                    Infer = infer_types(ArityReg, S),
                    Infer(Val, S);
               (_, S) -> S
            end;
        _ ->
            fun(_, S) -> S end
    end.

%%%
%%% Keeping track of types.
%%%

%% Assigns Src to Dst and marks them as aliasing each other.
assign({y,_}=Src, {y,_}=Dst, Vst) ->
    %% The stack trimming optimization may generate a move from an initialized
    %% but unassigned Y register to another Y register.
    case get_term_type_1(Src, Vst) of
        initialized -> set_type_reg(initialized, Dst, Vst);
        _ -> assign_1(Src, Dst, Vst)
    end;
assign({Kind,_}=Reg, Dst, Vst) when Kind =:= x; Kind =:= y ->
    assign_1(Reg, Dst, Vst);
assign(Literal, Dst, Vst) ->
    create_term(get_term_type(Literal, Vst), Dst, Vst).

%% Creates a completely new term with the given type.
create_term(Type, Dst, Vst) ->
    set_type_reg(Type, Dst, Vst).

%% Extracts a term from Ss, propagating fragility.
extract_term(Type, Ss, Dst, Vst) ->
    extract_term(Type, Ss, Dst, Vst, Vst).

%% As extract_term/4, but uses the incoming Vst for fragility in case x-regs
%% have been pruned and the sources can no longer be found.
extract_term(Type0, Ss, Dst, Vst, OrigVst) ->
    Type = propagate_fragility(Type0, Ss, OrigVst),
    set_type_reg(Type, Dst, Vst).

%% Helper function for simple "is_type" tests.
type_test(Fail, Type, Reg, Vst0) ->
    assert_term(Reg, Vst0),
    Vst = branch_state(Fail, update_type(fun subtract/2, Type, Reg, Vst0)),
    update_type(fun meet/2, Type, Reg, Vst).

%% This is used when linear code finds out more and more information about a
%% type, so that the type gets more specialized.
update_type(Merge, Type0, Reg, Vst) ->
    %% If the old type can't be merged with the new one, the type information
    %% is inconsistent and we know that some instructions will never be
    %% executed at run-time. For example:
    %%
    %%   {test,is_list,Fail,[Reg]}.
    %%   {test,is_tuple,Fail,[Reg]}.
    %%   {test,test_arity,Fail,[Reg,5]}.
    %%
    %% Note that the test_arity instruction can never be reached, so we use the
    %% new type instead of 'none'.
    Type = case Merge(get_durable_term_type(Reg, Vst), Type0) of
               none -> Type0;
               T -> T
           end,
    set_aliased_type(propagate_fragility(Type, [Reg], Vst), Reg, Vst).

update_ne_types(LHS, RHS, Vst) ->
    T1 = get_durable_term_type(LHS, Vst),
    T2 = get_durable_term_type(RHS, Vst),
    Type = propagate_fragility(subtract(T1, T2), [LHS], Vst),
    set_aliased_type(Type, LHS, Vst).

update_eq_types(LHS, RHS, Vst0) ->
    T1 = get_durable_term_type(LHS, Vst0),
    T2 = get_durable_term_type(RHS, Vst0),
    Meet = meet(T1, T2),
    Vst = case T1 =/= Meet of
              true ->
                  LType = propagate_fragility(Meet, [LHS], Vst0),
                  set_aliased_type(LType, LHS, Vst0);
              false ->
                  Vst0
          end,
    case T2 =/= Meet of
        true ->
            RType = propagate_fragility(Meet, [RHS], Vst0),
            set_aliased_type(RType, RHS, Vst);
        false ->
            Vst
    end.

%% Helper functions for the above.

assign_1(Src, Dst, Vst0) ->
    Type = get_move_term_type(Src, Vst0),
    Vst = set_type_reg(Type, Dst, Vst0),

    #vst{current=St0} = Vst,
    #st{aliases=Aliases0} = St0,
    Aliases = Aliases0#{Src=>Dst,Dst=>Src},
    St = St0#st{aliases=Aliases},

    Vst#vst{current=St}.

set_aliased_type(Type, Reg, #vst{current=#st{aliases=Aliases}}=Vst0) ->
    Vst1 = set_type(Type, Reg, Vst0),
    case Aliases of
        #{Reg:=OtherReg} ->
            Vst = set_type_reg(Type, OtherReg, Vst1),
            #vst{current=St} = Vst,
            Vst#vst{current=St#st{aliases=Aliases}};
        #{} ->
            Vst1
    end.

kill_aliases(Reg, #st{aliases=Aliases0}=St) ->
    case Aliases0 of
        #{Reg:=OtherReg} ->
            Aliases = maps:without([Reg,OtherReg], Aliases0),
            St#st{aliases=Aliases};
        #{} ->
            St
    end.

set_type(Type, {x,_}=Reg, Vst) ->
    set_type_reg(Type, Reg, Reg, Vst);
set_type(Type, {y,_}=Reg, Vst) ->
    set_type_reg(Type, Reg, Reg, Vst);
set_type(_, _, #vst{}=Vst) -> Vst.

set_type_reg(Type, Src, Dst, Vst) ->
    case get_term_type_1(Src, Vst) of
        {fragile,_} ->
            set_type_reg(make_fragile(Type), Dst, Vst);
        _ ->
            set_type_reg(Type, Dst, Vst)
    end.
set_type_reg(Type, Reg, Vst) ->
    set_type_reg_expr(Type, none, Reg, Vst).

set_type_reg_expr(Type, Expr, {x,_}=Reg, Vst) ->
    set_type_x(Type, Expr, Reg, Vst);
set_type_reg_expr(Type, Expr, Reg, Vst) ->
    set_type_y(Type, Expr, Reg, Vst).

set_type_y(Type, Reg, Vst) ->
    set_type_y(Type, none, Reg, Vst).

set_type_x(Type, Expr, {x,X}=Reg, #vst{current=#st{x=Xs0,defs=Defs0}=St0}=Vst)
  when is_integer(X), 0 =< X ->
    check_limit(Reg),
    Xs = case gb_trees:lookup(X, Xs0) of
             none ->
                 gb_trees:insert(X, Type, Xs0);
             {value,{fragile,_}} ->
                 gb_trees:update(X, make_fragile(Type), Xs0);
             {value,_} ->
                 gb_trees:update(X, Type, Xs0)
         end,
    Defs = Defs0#{Reg=>Expr},
    St = kill_aliases(Reg, St0),
    Vst#vst{current=St#st{x=Xs,defs=Defs}};
set_type_x(Type, _Expr, Reg, #vst{}) ->
    error({invalid_store,Reg,Type}).

set_type_y(Type, Expr, {y,Y}=Reg, #vst{current=#st{y=Ys0,defs=Defs0}=St0}=Vst)
  when is_integer(Y), 0 =< Y ->
    check_limit(Reg),
    Ys = case gb_trees:lookup(Y, Ys0) of
	     none ->
		 error({invalid_store,Reg,Type});
	     {value,{catchtag,_}=Tag} ->
		 error(Tag);
	     {value,{trytag,_}=Tag} ->
		 error(Tag);
	     {value,_} ->
		 gb_trees:update(Y, Type, Ys0)
	 end,
    check_try_catch_tags(Type, Y, Ys0),
    Defs = Defs0#{Reg=>Expr},
    St = kill_aliases(Reg, St0),
    Vst#vst{current=St#st{y=Ys,defs=Defs}};
set_type_y(Type, _Expr, Reg, #vst{}) ->
    error({invalid_store,Reg,Type}).

make_fragile({fragile,_}=Type) -> Type;
make_fragile(Type) -> {fragile,Type}.

set_catch_end({y,Y}, #vst{current=#st{y=Ys0}=St}=Vst) ->
    Ys = gb_trees:update(Y, initialized, Ys0),
    Vst#vst{current=St#st{y=Ys}}.

check_try_catch_tags(Type, LastY, Ys) ->
    case is_try_catch_tag(Type) of
        false ->
            ok;
        true ->
            %% Every catch or try/catch must use a lower Y register
            %% number than any enclosing catch or try/catch. That will
            %% ensure that when the stack is scanned when an
            %% exception occurs, the innermost try/catch tag is found
            %% first.
            Bad = [{{y,Y},Tag} || {Y,Tag} <- gb_trees:to_list(Ys),
                                  Y < LastY, is_try_catch_tag(Tag)],
            case Bad of
                [] ->
                    ok;
                [_|_] ->
                    error({bad_try_catch_nesting,{y,LastY},Bad})
            end
    end.

is_try_catch_tag({catchtag,_}) -> true;
is_try_catch_tag({trytag,_}) -> true;
is_try_catch_tag(_) -> false.

is_reg_defined({x,_}=Reg, Vst) -> is_type_defined_x(Reg, Vst);
is_reg_defined({y,_}=Reg, Vst) -> is_type_defined_y(Reg, Vst);
is_reg_defined(V, #vst{}) -> error({not_a_register, V}).

is_type_defined_x({x,X}, #vst{current=#st{x=Xs}}) ->
    gb_trees:is_defined(X,Xs).

is_type_defined_y({y,Y}, #vst{current=#st{y=Ys}}) ->
    gb_trees:is_defined(Y,Ys).

assert_term(Src, Vst) ->
    get_term_type(Src, Vst),
    ok.

assert_not_fragile(Src, Vst) ->
    case get_term_type(Src, Vst) of
        {fragile, _} -> error({fragile_message_reference, Src});
        _ -> ok
    end.

assert_literal(nil) -> ok;
assert_literal({atom,A}) when is_atom(A) -> ok;
assert_literal({float,F}) when is_float(F) -> ok;
assert_literal({integer,I}) when is_integer(I) -> ok;
assert_literal({literal,_L}) -> ok;
assert_literal(T) -> error({literal_required,T}).

assert_not_literal({x,_}) -> ok;
assert_not_literal({y,_}) -> ok;
assert_not_literal(Literal) -> error({literal_not_allowed,Literal}).

%% The possible types.
%%
%% First non-term types:
%%
%% initialized		Only for Y registers. Means that the Y register
%%			has been initialized with some valid term so that
%%			it is safe to pass to the garbage collector.
%%			NOT safe to use in any other way (will not crash the
%%			emulator, but clearly points to a bug in the compiler).
%%
%% {catchtag,[Lbl]}	A special term used within a catch. Must only be used
%%			by the catch instructions; NOT safe to use in other
%%			instructions.
%%
%% {trytag,[Lbl]}	A special term used within a try block. Must only be
%%			used by the catch instructions; NOT safe to use in other
%%			instructions.
%%
%% exception		Can only be used as a type returned by return_type/2
%%			(which gives the type of the value returned by a BIF).
%%			Thus 'exception' is never stored as type descriptor
%%			for a register.
%%
%% #ms{}	        A match context for bit syntax matching. We do allow
%%			it to moved/to from stack, but otherwise it must only
%%			be accessed by bit syntax matching instructions.
%%
%%
%% Normal terms:
%%
%% term			Any valid Erlang (but not of the special types above).
%%
%% binary               Binary or bitstring.
%%
%% bool			The atom 'true' or the atom 'false'.
%%
%% cons         	Cons cell: [_|_]
%%
%% nil			Empty list: []
%%
%% list                 List: [] or [_|_]
%%
%% {tuple,[Sz],Es}      Tuple. An element has been accessed using
%%                      element/2 or setelement/3 so that it is known that
%%                      the type is a tuple of size at least Sz. Es is a map
%%                      containing known types by tuple index.
%%
%% {tuple,Sz,Es}	Tuple. A test_arity instruction has been seen
%%           		so that it is known that the size is exactly Sz.
%%
%% {atom,[]}		Atom.
%% {atom,Atom}
%%
%% {integer,[]}		Integer.
%% {integer,Integer}
%%
%% {float,[]}		Float.
%% {float,Float}
%%
%% number		Integer or Float of unknown value
%%
%% map			Map.
%%
%% none                 A conflict in types. There will be an exception at runtime.
%%
%% FRAGILITY
%% ---------
%%
%% The loop_rec/2 instruction may return a reference to a term that is
%% not part of the root set. That term or any part of it must not be
%% included in a garbage collection. Therefore, the term (or any part
%% of it) must not be stored in an Y register.
%%
%% Such terms are wrapped in a {fragile,Type} tuple, where Type is one
%% of the types described above.

%% meet(Type1, Type2) -> Type
%%  Return the meet of two types. The meet is a more specific type.
%%  It will be 'none' if the types are in conflict.

meet(Same, Same) ->
    Same;
meet({literal,_}=T1, T2) ->
    meet_literal(T1, T2);
meet(T1, {literal,_}=T2) ->
    meet_literal(T2, T1);
meet(term, Other) ->
    Other;
meet(Other, term) ->
    Other;
meet(T1, T2) ->
    case {erlang:min(T1, T2),erlang:max(T1, T2)} of
        {{atom,_}=A,{atom,[]}} -> A;
        {bool,{atom,B}=Atom} when is_boolean(B) -> Atom;
        {bool,{atom,[]}} -> bool;
        {cons,list} -> cons;
        {{float,_}=T,{float,[]}} -> T;
        {{integer,_}=T,{integer,[]}} -> T;
        {list,nil} -> nil;
        {number,{integer,_}=T} -> T;
        {number,{float,_}=T} -> T;
        {{tuple,Size1,Es1},{tuple,Size2,Es2}} ->
            Es = meet_elements(Es1, Es2),
            case {Size1,Size2,Es} of
                {_, _, none} ->
                    none;
                {[Sz1],[Sz2],_} ->
                    {tuple,[erlang:max(Sz1, Sz2)],Es};
                {Sz1,[Sz2],_} when Sz2 =< Sz1 ->
                    {tuple,Sz1,Es};
                {Sz,Sz,_} ->
                    {tuple,Sz,Es};
                {_,_,_} ->
                    none
            end;
        {_,_} -> none
    end.

%% Meets types of literals.
meet_literal({literal,_}=Lit, T) ->
    meet_literal(T, get_literal_type(Lit));
meet_literal(T1, T2) ->
    %% We're done extracting the types, try merging them again.
    meet(T1, T2).

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

%% subtract(Type1, Type2) -> Type
%%  Subtract Type2 from Type2. Example:
%%      subtract(list, nil) -> cons

subtract(list, nil) -> cons;
subtract(list, cons) -> nil;
subtract(number, {integer,[]}) -> {float,[]};
subtract(number, {float,[]}) -> {integer,[]};
subtract(bool, {atom,false}) -> {atom, true};
subtract(bool, {atom,true}) -> {atom, false};
subtract(Type, _) -> Type.

assert_type(WantedType, Term, Vst) ->
    Type = get_durable_term_type(Term, Vst),
    assert_type(WantedType, Type).

assert_type(Correct, Correct) -> ok;
assert_type(float, {float,_}) -> ok;
assert_type(tuple, {tuple,_,_}) -> ok;
assert_type(tuple, {literal,Tuple}) when is_tuple(Tuple) -> ok;
assert_type({tuple_element,I}, {tuple,[Sz],_})
  when 1 =< I, I =< Sz ->
    ok;
assert_type({tuple_element,I}, {tuple,Sz,_})
  when is_integer(Sz), 1 =< I, I =< Sz ->
    ok;
assert_type({tuple_element,I}, {literal,Lit}) when I =< tuple_size(Lit) ->
    ok;
assert_type(cons, {literal,[_|_]}) ->
    ok;
assert_type(Needed, Actual) ->
    error({bad_type,{needed,Needed},{actual,Actual}}).

get_element_type(Key, Src, Vst) ->
    get_element_type_1(Key, get_durable_term_type(Src, Vst)).

get_element_type_1(Index, {tuple,Sz,Es}) ->
    case Es of
        #{ Index := Type } -> Type;
        #{} when Index =< Sz -> term;
        #{} -> none
    end;
get_element_type_1(_Index, _Type) ->
    term.

set_element_type(_Key, none, Es) ->
    Es;
set_element_type(Key, term, Es) ->
    maps:remove(Key, Es);
set_element_type(Key, Type, Es) ->
    Es#{ Key => Type }.

get_tuple_size({integer,[]}) -> 0;
get_tuple_size({integer,Sz}) -> Sz;
get_tuple_size(_) -> 0.

validate_src(Ss, Vst) when is_list(Ss) ->
    foreach(fun(S) -> get_term_type(S, Vst) end, Ss).

%% get_durable_term_type(Src, ValidatorState) -> Type
%%  Get the type of the source Src. The returned type Type will be
%%  a standard Erlang type (no catch/try tags or match contexts).
%%  Fragility will be stripped.

get_durable_term_type(Src, Vst) ->
    case get_term_type(Src, Vst) of
        {fragile,Type} -> Type;
        Type -> Type
    end.

%% get_move_term_type(Src, ValidatorState) -> Type
%%  Get the type of the source Src. The returned type Type will be
%%  a standard Erlang type (no catch/try tags). Match contexts are OK.

get_move_term_type(Src, Vst) ->
    case get_term_type_1(Src, Vst) of
	initialized -> error({unassigned,Src});
	{catchtag,_} -> error({catchtag,Src});
	{trytag,_} -> error({trytag,Src});
        tuple_in_progress -> error({tuple_in_progress,Src});
	Type -> Type
    end.

%% get_term_type(Src, ValidatorState) -> Type
%%  Get the type of the source Src. The returned type Type will be
%%  a standard Erlang type (no catch/try tags or match contexts).

get_term_type(Src, Vst) ->
    case get_move_term_type(Src, Vst) of
	#ms{} -> error({match_context,Src});
	Type -> Type
    end.

%% get_special_y_type(Src, ValidatorState) -> Type
%%  Return the type for the Y register without doing any validity checks.

get_special_y_type({y,_}=Reg, Vst) -> get_term_type_1(Reg, Vst);
get_special_y_type(Src, _) -> error({source_not_y_reg,Src}).

get_term_type_1({x,X}=Reg, #vst{current=#st{x=Xs}}) when is_integer(X) ->
    case gb_trees:lookup(X, Xs) of
	{value,Type} -> Type;
	none -> error({uninitialized_reg,Reg})
    end;
get_term_type_1({y,Y}=Reg, #vst{current=#st{y=Ys}}) when is_integer(Y) ->
    case gb_trees:lookup(Y, Ys) of
 	none -> error({uninitialized_reg,Reg});
	{value,uninitialized} -> error({uninitialized_reg,Reg});
	{value,Type} -> Type
    end;
get_term_type_1(Src, _) ->
    get_literal_type(Src).

get_def(Src, #vst{current=#st{defs=Defs}}) ->
    case Defs of
        #{Src:=Def} -> Def;
        #{} -> none
    end.

get_literal_type(nil=T) -> T;
get_literal_type({atom,A}=T) when is_atom(A) -> T;
get_literal_type({float,F}=T) when is_float(F) -> T;
get_literal_type({integer,I}=T) when is_integer(I) -> T;
get_literal_type({literal,[_|_]}) -> cons;
get_literal_type({literal,Bitstring}) when is_bitstring(Bitstring) -> binary;
get_literal_type({literal,Map}) when is_map(Map) -> map;
get_literal_type({literal,Tuple}) when is_tuple(Tuple) -> value_to_type(Tuple);
get_literal_type({literal,_}) -> term;
get_literal_type(T) -> error({not_literal,T}).

value_to_type([]) -> nil;
value_to_type(A) when is_atom(A) -> {atom, A};
value_to_type(F) when is_float(F) -> {float, F};
value_to_type(I) when is_integer(I) -> {integer, I};
value_to_type(T) when is_tuple(T) ->
     {Es,_} = foldl(fun(Val, {Es0, Index}) ->
                            Type = value_to_type(Val),
                            Es = set_element_type(Index, Type, Es0),
                            {Es, Index + 1}
                    end, {#{}, 1}, tuple_to_list(T)),
     {tuple, tuple_size(T), Es};
value_to_type(L) -> {literal, L}.

branch_arities([Sz,{f,L}|T], Tuple, {tuple,[_],Es0}=Type0, Vst0) when is_integer(Sz) ->
    %% Filter out element types that are no longer valid.
    Es = maps:filter(fun(Index, _Type) -> Index =< Sz end, Es0),
    Vst1 = set_aliased_type({tuple,Sz,Es}, Tuple, Vst0),
    Vst = branch_state(L, Vst1),
    branch_arities(T, Tuple, Type0, Vst);
branch_arities([Sz,{f,L}|T], Tuple, {tuple,Sz,_Es}=Type, Vst0) when is_integer(Sz) ->
    %% The type is already correct. (This test is redundant.)
    Vst = branch_state(L, Vst0),
    branch_arities(T, Tuple, Type, Vst);
branch_arities([Sz0,{f,_}|T], Tuple, {tuple,Sz,_Es}=Type, Vst)
  when is_integer(Sz), Sz0 =/= Sz ->
    %% We already have an established different exact size for the tuple.
    %% This label can't possibly be reached.
    branch_arities(T, Tuple, Type, Vst);
branch_arities([], _, _, #vst{}=Vst) -> Vst.

branch_state(0, #vst{}=Vst) ->
    %% If the instruction fails, the stack may be scanned
    %% looking for a catch tag. Therefore the Y registers
    %% must be initialized at this point.
    verify_y_init(Vst),
    Vst;
branch_state(L, #vst{current=St,branched=B}=Vst) ->
    Vst#vst{
      branched=case gb_trees:is_defined(L, B) of
		   false ->
		       gb_trees:insert(L, St, B);
		   true ->
		       MergedSt = merge_states(L, St, B),
		       gb_trees:update(L, MergedSt, B)
	       end}.

%% merge_states/3 is used when there are more than one way to arrive
%% at this point, and the type states for the different paths has
%% to be merged. The type states are downgraded to the least common
%% subset for the subsequent code.

merge_states(L, St, Branched) when L =/= 0 ->
    case gb_trees:lookup(L, Branched) of
	none -> St;
	{value,OtherSt} when St =:= none -> OtherSt;
	{value,OtherSt} -> merge_states_1(St, OtherSt)
    end.

merge_states_1(#st{x=Xs0,y=Ys0,numy=NumY0,h=H0,ct=Ct0,aliases=Aliases0},
	       #st{x=Xs1,y=Ys1,numy=NumY1,h=H1,ct=Ct1,aliases=Aliases1}) ->
    NumY = merge_stk(NumY0, NumY1),
    Xs = merge_regs(Xs0, Xs1),
    Ys = merge_y_regs(Ys0, Ys1),
    Ct = merge_ct(Ct0, Ct1),
    Aliases = merge_aliases(Aliases0, Aliases1),
    #st{x=Xs,y=Ys,numy=NumY,h=min(H0, H1),ct=Ct,aliases=Aliases}.

merge_stk(S, S) -> S;
merge_stk(_, _) -> undecided.

merge_ct(S, S) -> S;
merge_ct(Ct0, Ct1) -> merge_ct_1(Ct0, Ct1).

merge_ct_1([C0|Ct0], [C1|Ct1]) ->
    [ordsets:from_list(C0++C1)|merge_ct_1(Ct0, Ct1)];
merge_ct_1([], []) -> [];
merge_ct_1(_, _) -> undecided.

merge_regs(Rs0, Rs1) ->
    Rs = merge_regs_1(gb_trees:to_list(Rs0), gb_trees:to_list(Rs1)),
    gb_trees_from_list(Rs).

merge_regs_1([Same|Rs1], [Same|Rs2]) ->
    [Same|merge_regs_1(Rs1, Rs2)];
merge_regs_1([{R1,_}|Rs1], [{R2,_}|_]=Rs2) when R1 < R2 ->
    merge_regs_1(Rs1, Rs2);
merge_regs_1([{R1,_}|_]=Rs1, [{R2,_}|Rs2]) when R1 > R2 ->
    merge_regs_1(Rs1, Rs2);
merge_regs_1([{R,Type1}|Rs1], [{R,Type2}|Rs2]) ->
    [{R,join(Type1, Type2)}|merge_regs_1(Rs1, Rs2)];
merge_regs_1([], []) -> [];
merge_regs_1([], [_|_]) -> [];
merge_regs_1([_|_], []) -> [].

merge_y_regs(Rs0, Rs1) ->
    case {gb_trees:size(Rs0),gb_trees:size(Rs1)} of
	{Sz0,Sz1} when Sz0 < Sz1 ->
	    merge_y_regs_1(Sz0-1, Rs1, Rs0);
	{_,Sz1} ->
	    merge_y_regs_1(Sz1-1, Rs0, Rs1)
    end.

merge_y_regs_1(Y, S, Regs0) when Y >= 0 ->
    Type0 = gb_trees:get(Y, Regs0),
    case gb_trees:get(Y, S) of
	Type0 ->
	    merge_y_regs_1(Y-1, S, Regs0);
	Type1 ->
	    Type = join(Type0, Type1),
	    Regs = gb_trees:update(Y, Type, Regs0),
	    merge_y_regs_1(Y-1, S, Regs)
    end;
merge_y_regs_1(_, _, Regs) -> Regs.

%% join(Type1, Type2) -> Type
%%  Return the most specific type possible.
%%  Note: Type1 must NOT be the same as Type2.
join({literal,_}=T1, T2) ->
    join_literal(T1, T2);
join(T1, {literal,_}=T2) ->
    join_literal(T2, T1);
join({fragile,Same}=Type, Same) ->
    Type;
join({fragile,T1}, T2) ->
    make_fragile(join(T1, T2));
join(Same, {fragile,Same}=Type) ->
    Type;
join(T1, {fragile,T2}) ->
    make_fragile(join(T1, T2));
join(uninitialized=I, _) -> I;
join(_, uninitialized=I) -> I;
join(initialized=I, _) -> I;
join(_, initialized=I) -> I;
join({catchtag,T0},{catchtag,T1}) ->
    {catchtag,ordsets:from_list(T0++T1)};
join({trytag,T0},{trytag,T1}) ->
    {trytag,ordsets:from_list(T0++T1)};
join({tuple,Size,EsA}, {tuple,Size,EsB}) ->
    Es = join_tuple_elements(tuple_sz(Size), EsA, EsB),
    {tuple, Size, Es};
join({tuple,A,EsA}, {tuple,B,EsB}) ->
    Size = [min(tuple_sz(A), tuple_sz(B))],
    Es = join_tuple_elements(Size, EsA, EsB),
    {tuple, Size, Es};
join({Type,A}, {Type,B})
  when Type =:= atom; Type =:= integer; Type =:= float ->
    if A =:= B -> {Type,A};
       true -> {Type,[]}
    end;
join({Type,_}, number) 
  when Type =:= integer; Type =:= float ->
    number;
join(number, {Type,_}) 
  when Type =:= integer; Type =:= float ->
    number;
join(bool, {atom,A}) ->
    join_bool(A);
join({atom,A}, bool) ->
    join_bool(A);
join({atom,_}, {atom,_}) ->
    {atom,[]};
join(#ms{id=Id1,valid=B1,slots=Slots1},
	    #ms{id=Id2,valid=B2,slots=Slots2}) ->
    Id = if
             Id1 =:= Id2 -> Id1;
             true -> make_ref()
         end,
    #ms{id=Id,valid=B1 band B2,slots=min(Slots1, Slots2)};
join(T1, T2) when T1 =/= T2 ->
    %% We've exhaused all other options, so the type must either be a list or
    %% a 'term'.
    join_list(T1, T2).

join_tuple_elements(Size, EsA, EsB) ->
    Es0 = join_elements(EsA, EsB),
    MinSize = tuple_sz(Size),
    maps:filter(fun(Index, _Type) -> Index =< MinSize end, Es0).

join_elements(Es1, Es2) ->
    Keys = if
               map_size(Es1) =< map_size(Es2) -> maps:keys(Es1);
               map_size(Es1) > map_size(Es2) -> maps:keys(Es2)
           end,
    join_elements_1(Keys, Es1, Es2, #{}).

join_elements_1([Key | Keys], Es1, Es2, Acc0) ->
    Type = case {Es1, Es2} of
               {#{ Key := Same }, #{ Key := Same }} -> Same;
               {#{ Key := Type1 }, #{ Key := Type2 }} -> join(Type1, Type2);
               {#{}, #{}} -> term
           end,
    Acc = set_element_type(Key, Type, Acc0),
    join_elements_1(Keys, Es1, Es2, Acc);
join_elements_1([], _Es1, _Es2, Acc) ->
    Acc.

%% Joins types of literals; note that the left argument must either be a
%% literal or exactly equal to the second argument.
join_literal(Same, Same) ->
    Same;
join_literal({literal,_}=Lit, T) ->
    join_literal(T, get_literal_type(Lit));
join_literal(T1, T2) ->
    %% We're done extracting the types, try merging them again.
    join(T1, T2).

join_list(nil, cons) -> list;
join_list(nil, list) -> list;
join_list(cons, list) -> list;
join_list(T, nil) -> join_list(nil, T);
join_list(T, cons) -> join_list(cons, T);
join_list(_, _) ->
    %% Not a list, so it must be a term.
    term.

join_bool([]) -> {atom,[]};
join_bool(true) -> bool;
join_bool(false) -> bool;
join_bool(_) -> {atom,[]}.

tuple_sz([Sz]) -> Sz;
tuple_sz(Sz) -> Sz.

merge_aliases(Al0, Al1) when map_size(Al0) =< map_size(Al1) ->
    maps:filter(fun(K, V) ->
                        case Al1 of
                            #{K:=V} -> true;
                            #{} -> false
                        end
                end, Al0);
merge_aliases(Al0, Al1) ->
    merge_aliases(Al1, Al0).

verify_y_init(#vst{current=#st{y=Ys}}) ->
    verify_y_init_1(gb_trees:to_list(Ys)).

verify_y_init_1([]) -> ok;
verify_y_init_1([{Y,uninitialized}|_]) ->
    error({uninitialized_reg,{y,Y}});
verify_y_init_1([{Y,{fragile,_}}|_]) ->
    %% Unsafe. This term may be outside any heap belonging
    %% to the process and would be corrupted by a GC.
    error({fragile_message_reference,{y,Y}});
verify_y_init_1([{_,_}|Ys]) ->
    verify_y_init_1(Ys).

verify_live(0, #vst{}) -> ok;
verify_live(N, #vst{current=#st{x=Xs}}) ->
    verify_live_1(N, Xs).

verify_live_1(0, _) -> ok;
verify_live_1(N, Xs) when is_integer(N) ->
    X = N-1,
    case gb_trees:is_defined(X, Xs) of
	false -> error({{x,X},not_live});
	true -> verify_live_1(X, Xs)
    end;
verify_live_1(N, _) -> error({bad_number_of_live_regs,N}).

verify_no_ct(#vst{current=#st{numy=none}}) -> ok;
verify_no_ct(#vst{current=#st{numy=undecided}}) ->
    error(unknown_size_of_stackframe);
verify_no_ct(#vst{current=#st{y=Ys}}) ->
    case [Y || Y <- gb_trees:to_list(Ys), verify_no_ct_1(Y)] of
	[] -> ok;
	CT -> error({unfinished_catch_try,CT})
    end.

verify_no_ct_1({_, {catchtag, _}}) -> true;
verify_no_ct_1({_, {trytag, _}}) -> true;
verify_no_ct_1({_, _}) -> false.

eat_heap(N, #vst{current=#st{h=Heap0}=St}=Vst) ->
    case Heap0-N of
	Neg when Neg < 0 ->
	    error({heap_overflow,{left,Heap0},{wanted,N}});
	Heap ->
	    Vst#vst{current=St#st{h=Heap}}
    end.

eat_heap_float(#vst{current=#st{hf=HeapFloats0}=St}=Vst) ->
    case HeapFloats0-1 of
	Neg when Neg < 0 ->
	    error({heap_overflow,{left,{HeapFloats0,floats}},{wanted,{1,floats}}});
	HeapFloats ->
	    Vst#vst{current=St#st{hf=HeapFloats}}
    end.

remove_fragility(#vst{current=#st{x=Xs0,y=Ys0}=St0}=Vst) ->
    F = fun(_, {fragile,Type}) -> Type;
           (_, Type) -> Type
        end,
    Xs = gb_trees:map(F, Xs0),
    Ys = gb_trees:map(F, Ys0),
    St = St0#st{x=Xs,y=Ys},
    Vst#vst{current=St}.

propagate_fragility(Type, Ss, Vst) ->
    F = fun(S) ->
                case get_term_type_1(S, Vst) of
                    {fragile,_} -> true;
                    _ -> false
                end
        end,
    case any(F, Ss) of
        true -> make_fragile(Type);
        false -> Type
    end.

bif_type('-', Src, Vst) ->
    arith_type(Src, Vst);
bif_type('+', Src, Vst) ->
    arith_type(Src, Vst);
bif_type('*', Src, Vst) ->
    arith_type(Src, Vst);
bif_type(abs, [Num], Vst) ->
    case get_durable_term_type(Num, Vst) of
	{float,_}=T -> T;
	{integer,_}=T -> T;
	_ -> number
    end;
bif_type(float, _, _) -> {float,[]};
bif_type('/', _, _) -> {float,[]};
%% Binary operations
bif_type('byte_size', _, _) -> {integer,[]};
bif_type('bit_size', _, _) -> {integer,[]};
%% Integer operations.
bif_type(ceil, [_], _) -> {integer,[]};
bif_type('div', [_,_], _) -> {integer,[]};
bif_type(floor, [_], _) -> {integer,[]};
bif_type('rem', [_,_], _) -> {integer,[]};
bif_type(length, [_], _) -> {integer,[]};
bif_type(size, [_], _) -> {integer,[]};
bif_type(trunc, [_], _) -> {integer,[]};
bif_type(round, [_], _) -> {integer,[]};
bif_type('band', [_,_], _) -> {integer,[]};
bif_type('bor', [_,_], _) -> {integer,[]};
bif_type('bxor', [_,_], _) -> {integer,[]};
bif_type('bnot', [_], _) -> {integer,[]};
bif_type('bsl', [_,_], _) -> {integer,[]};
bif_type('bsr', [_,_], _) -> {integer,[]};
%% Booleans.
bif_type('==', [_,_], _) -> bool;
bif_type('/=', [_,_], _) -> bool;
bif_type('=<', [_,_], _) -> bool;
bif_type('<', [_,_], _) -> bool;
bif_type('>=', [_,_], _) -> bool;
bif_type('>', [_,_], _) -> bool;
bif_type('=:=', [_,_], _) -> bool;
bif_type('=/=', [_,_], _) -> bool;
bif_type('not', [_], _) -> bool;
bif_type('and', [_,_], _) -> bool;
bif_type('or', [_,_], _) -> bool;
bif_type('xor', [_,_], _) -> bool;
bif_type(is_atom, [_], _) -> bool;
bif_type(is_boolean, [_], _) -> bool;
bif_type(is_binary, [_], _) -> bool;
bif_type(is_float, [_], _) -> bool;
bif_type(is_function, [_], _) -> bool;
bif_type(is_integer, [_], _) -> bool;
bif_type(is_list, [_], _) -> bool;
bif_type(is_map, [_], _) -> bool;
bif_type(is_number, [_], _) -> bool;
bif_type(is_pid, [_], _) -> bool;
bif_type(is_port, [_], _) -> bool;
bif_type(is_reference, [_], _) -> bool;
bif_type(is_tuple, [_], _) -> bool;
%% Misc.
bif_type(tuple_size, [_], _) -> {integer,[]};
bif_type(node, [], _) -> {atom,[]};
bif_type(node, [_], _) -> {atom,[]};
bif_type(hd, [_], _) -> term;
bif_type(tl, [_], _) -> term;
bif_type(get, [_], _) -> term;
bif_type(Bif, _, _) when is_atom(Bif) -> term.

is_bif_safe('/=', 2) -> true;
is_bif_safe('<', 2) -> true;
is_bif_safe('=/=', 2) -> true;
is_bif_safe('=:=', 2) -> true;
is_bif_safe('=<', 2) -> true;
is_bif_safe('==', 2) -> true;
is_bif_safe('>', 2) -> true;
is_bif_safe('>=', 2) -> true;
is_bif_safe(is_atom, 1) -> true;
is_bif_safe(is_boolean, 1) -> true;
is_bif_safe(is_binary, 1) -> true;
is_bif_safe(is_bitstring, 1) -> true;
is_bif_safe(is_float, 1) -> true;
is_bif_safe(is_function, 1) -> true;
is_bif_safe(is_integer, 1) -> true;
is_bif_safe(is_list, 1) -> true;
is_bif_safe(is_map, 1) -> true;
is_bif_safe(is_number, 1) -> true;
is_bif_safe(is_pid, 1) -> true;
is_bif_safe(is_port, 1) -> true;
is_bif_safe(is_reference, 1) -> true;
is_bif_safe(is_tuple, 1) -> true;
is_bif_safe(get, 1) -> true;
is_bif_safe(self, 0) -> true;
is_bif_safe(node, 0) -> true;
is_bif_safe(_, _) -> false.

arith_type([A], Vst) ->
    %% Unary '+' or '-'.
    case get_durable_term_type(A, Vst) of
	{integer,_} -> {integer,[]};
	{float,_} -> {float,[]};
        _ -> number
    end;
arith_type([A,B], Vst) ->
    TypeA = get_durable_term_type(A, Vst),
    TypeB = get_durable_term_type(B, Vst),
    case {TypeA, TypeB} of
	{{integer,_},{integer,_}} -> {integer,[]};
	{{float,_},_} -> {float,[]};
	{_,{float,_}} -> {float,[]};
	{_,_} -> number
    end;
arith_type(_, _) -> number.

return_type({extfunc,M,F,A}, Vst) -> return_type_1(M, F, A, Vst);
return_type(_, _) -> term.

return_type_1(erlang, setelement, 3, Vst) ->
    IndexType = get_term_type({x,0}, Vst),
    TupleType =
        case get_term_type({x,1}, Vst) of
            {literal,Tuple}=Lit when is_tuple(Tuple) -> get_literal_type(Lit);
            {tuple,_,_}=TT -> TT;
            _ -> {tuple,[0],#{}}
        end,
    case IndexType of
        {integer,I} when is_integer(I) ->
            case meet({tuple,[I],#{}}, TupleType) of
                {tuple, Sz, Es0} ->
                    ValueType = get_term_type({x,2}, Vst),
                    Es = set_element_type(I, ValueType, Es0),
                    {tuple, Sz, Es};
                none ->
                    TupleType
            end;
        _ ->
            %% The index could point anywhere, so we must discard all element
            %% information.
            setelement(3, TupleType, #{})
    end;
return_type_1(erlang, '++', 2, Vst) ->
    case get_term_type({x,0}, Vst) =:= cons orelse
        get_term_type({x,1}, Vst) =:= cons of
        true -> cons;
        false -> list
    end;
return_type_1(erlang, '--', 2, _Vst) ->
    list;
return_type_1(erlang, F, A, _) ->
    return_type_erl(F, A);
return_type_1(math, F, A, _) ->
    return_type_math(F, A);
return_type_1(M, F, A, _) when is_atom(M), is_atom(F), is_integer(A), A >= 0 ->
    term.

return_type_erl(exit, 1) -> exception;
return_type_erl(throw, 1) -> exception;
return_type_erl(error, 1) -> exception;
return_type_erl(error, 2) -> exception;
return_type_erl(F, A) when is_atom(F), is_integer(A), A >= 0 -> term.

return_type_math(cos, 1) -> {float,[]};
return_type_math(cosh, 1) -> {float,[]};
return_type_math(sin, 1) -> {float,[]};
return_type_math(sinh, 1) -> {float,[]};
return_type_math(tan, 1) -> {float,[]};
return_type_math(tanh, 1) -> {float,[]};
return_type_math(acos, 1) -> {float,[]};
return_type_math(acosh, 1) -> {float,[]};
return_type_math(asin, 1) -> {float,[]};
return_type_math(asinh, 1) -> {float,[]};
return_type_math(atan, 1) -> {float,[]};
return_type_math(atanh, 1) -> {float,[]};
return_type_math(erf, 1) -> {float,[]};
return_type_math(erfc, 1) -> {float,[]};
return_type_math(exp, 1) -> {float,[]};
return_type_math(log, 1) -> {float,[]};
return_type_math(log2, 1) -> {float,[]};
return_type_math(log10, 1) -> {float,[]};
return_type_math(sqrt, 1) -> {float,[]};
return_type_math(atan2, 2) -> {float,[]};
return_type_math(pow, 2) -> {float,[]};
return_type_math(ceil, 1) -> {float,[]};
return_type_math(floor, 1) -> {float,[]};
return_type_math(fmod, 2) -> {float,[]};
return_type_math(pi, 0) -> {float,[]};
return_type_math(F, A) when is_atom(F), is_integer(A), A >= 0 -> term.

check_limit({x,X}) when is_integer(X), X < 1023 ->
    %% Note: x(1023) is reserved for use by the BEAM loader.
    ok;
check_limit({y,Y}) when is_integer(Y), Y < 1024 ->
    ok;
check_limit({fr,Fr}) when is_integer(Fr), Fr < 1024 ->
    ok;
check_limit(_) ->
    error(limit).

min(A, B) when is_integer(A), is_integer(B), A < B -> A;
min(A, B) when is_integer(A), is_integer(B) -> B.

gb_trees_from_list(L) -> gb_trees:from_orddict(lists:sort(L)).

error(Error) -> throw(Error).
