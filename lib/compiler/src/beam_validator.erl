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

-include("beam_types.hrl").

-define(UNICODE_MAX, (16#10FFFF)).

-compile({no_auto_import,[min/2]}).

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).

%% Interface for compiler.
-export([module/2, format_error/1]).

-import(lists, [dropwhile/2,foldl/3,member/2,reverse/1,sort/1,zip/2]).

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
    Ft = build_function_table(Fs, []),
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

-record(t_abstract, {kind}).

%% The types are the same as in 'beam_types.hrl', with the addition of
%% #t_abstract{} that describes tuples under construction, match context
%% positions, and so on.
-type validator_type() :: #t_abstract{} | type().

-record(value_ref, {id :: index()}).
-record(value, {op :: term(), args :: [argument()], type :: validator_type()}).

-type argument() :: #value_ref{} | literal().

-type index() :: non_neg_integer().

-type literal() :: {atom, [] | atom()} |
                   {float, [] | float()} |
                   {integer, [] | integer()} |
                   {literal, term()} |
                   nil.

%% Register tags describe the state of the register rather than the value they
%% contain (if any).
%%
%% initialized          The register has been initialized with some valid term
%%                      so that it is safe to pass to the garbage collector.
%%                      NOT safe to use in any other way (will not crash the
%%                      emulator, but clearly points to a bug in the compiler).
%%
%% uninitialized        The register contains any old garbage and can not be
%%                      passed to the garbage collector.
%%
%% {catchtag,[Lbl]}     A special term used within a catch. Must only be used
%%                      by the catch instructions; NOT safe to use in other
%%                      instructions.
%%
%% {trytag,[Lbl]}       A special term used within a try block. Must only be
%%                      used by the catch instructions; NOT safe to use in other
%%                      instructions.
-type tag() :: initialized |
               uninitialized |
               {catchtag, ordsets:ordset(label())} |
               {trytag, ordsets:ordset(label())}.

-type x_regs() :: #{ {x, index()} => #value_ref{} }.
-type y_regs() :: #{ {y, index()} => tag() | #value_ref{} }.

%% Emulation state
-record(st,
        {%% All known values.
         vs=#{} :: #{ #value_ref{} => #value{} },
         %% Register states.
         xs=#{} :: x_regs(),
         ys=#{} :: y_regs(),
         f=init_fregs(),
         %% A set of all registers containing "fragile" terms. That is, terms
         %% that don't exist on our process heap and would be destroyed by a
         %% GC.
         fragile=cerl_sets:new() :: cerl_sets:set(),
         %% Number of Y registers.
         %%
         %% Note that this may be 0 if there's a frame without saved values,
         %% such as on a body-recursive call.
         numy=none :: none | undecided | index(),
         %% Available heap size.
         h=0,
         %%Available heap size for floats.
         hf=0,
         %% Floating point state.
         fls=undefined,
         %% List of hot catch/try tags
         ct=[],
         %% Previous instruction was setelement/3.
         setelem=false,
         %% put/1 instructions left.
         puts_left=none,
         %% recv_mark/recv_set state.
         %%
         %% 'initialized' means we've saved a message position, and 'committed'
         %% means the next loop_rec instruction will use it.
         recv_marker=none :: none | undecided | initialized | committed
        }).

-type label()        :: integer().
-type label_set()    :: gb_sets:set(label()).
-type branched_tab() :: gb_trees:tree(label(), #st{}).
-type ft_tab()       :: gb_trees:tree().

%% Validator state
-record(vst,
        {%% Current state
         current=none              :: #st{} | 'none',
         %% States at labels
         branched=gb_trees:empty() :: branched_tab(),
         %% All defined labels
         labels=gb_sets:empty()    :: label_set(),
         %% Information of other functions in the module
         ft=gb_trees:empty()       :: ft_tab(),
         %% Counter for #value_ref{} creation
         ref_ctr=0                 :: index()
        }).

build_function_table([{function,_,Arity,Entry,Code0}|Fs], Acc0) ->
    Code = dropwhile(fun({label,L}) when L =:= Entry -> false;
			(_) -> true
		     end, Code0),
    case Code of
	[{label,Entry}|Is] ->
	    Info = #{ arity => Arity,
                  parameter_info => find_parameter_info(Is, #{}) },
	    build_function_table(Fs, [{Entry, Info} | Acc0]);
	_ ->
	    %% Something is seriously wrong. Ignore it for now.
	    %% It will be detected and diagnosed later.
	    build_function_table(Fs, Acc0)
    end;
build_function_table([], Acc) ->
    gb_trees:from_orddict(sort(Acc)).

find_parameter_info([{'%', {var_info, Reg, Info}} | Is], Acc) ->
    find_parameter_info(Is, Acc#{ Reg => Info });
find_parameter_info([{'%', _} | Is], Acc) ->
    find_parameter_info(Is, Acc);
find_parameter_info(_, Acc) ->
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
    EntryOK = member(Entry, Ls2),
    if
	EntryOK ->
            Vst0 = init_vst(Arity, Ls1, Ls2, Ft),
	    MFA = {Mod,Name,Arity},
	    Vst = validate_instrs(Is, MFA, Offset, Vst0),
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
        assert_term({x,X}, Vst)
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

init_vst(Arity, Ls1, Ls2, Ft) ->
    Vst0 = init_function_args(Arity - 1, #vst{current=#st{}}),
    Branches = gb_trees_from_list([{L,Vst0#vst.current} || L <- Ls1]),
    Labels = gb_sets:from_list(Ls1++Ls2),
    Vst0#vst{branched=Branches,
             labels=Labels,
             ft=Ft}.

init_function_args(-1, Vst) ->
    Vst;
init_function_args(X, Vst) ->
    init_function_args(X - 1, create_term(any, argument, [], {x,X}, Vst)).

kill_heap_allocation(St) ->
    St#st{h=0,hf=0}.

validate_instrs([], MFA, _Offset, #vst{branched=Targets0,labels=Labels0}=Vst) ->
    Targets = gb_trees:keys(Targets0),
    Labels = gb_sets:to_list(Labels0),
    case Targets -- Labels of
	[] -> Vst;
	Undef ->
	    Error = {undef_labels,Undef},
	    error({MFA,Error})
    end;
validate_instrs([I|Is], MFA, Offset, Vst0) ->
    validate_instrs(Is, MFA, Offset+1,
	   try
	       Vst = validate_mutation(I, Vst0),
	       vi_safe(I, Vst)
	   catch Error ->
		   error({MFA,{I,Offset,Error}})
	   end).

%%%
%%% vi_safe/2 handles instructions that will never throw an exception, and can
%%% thus be used when the state is undecided in some way.
%%%
vi_safe({label,Lbl}, #vst{current=St0,
                          ref_ctr=Counter0,
                          branched=B,
                          labels=Lbls}=Vst) ->
    {St, Counter} = merge_states(Lbl, St0, B, Counter0),
    Vst#vst{current=St,
            ref_ctr=Counter,
            branched=gb_trees:enter(Lbl, St, B),
            labels=gb_sets:add(Lbl, Lbls)};
vi_safe(_I, #vst{current=none}=Vst) ->
    %% Ignore all unreachable code.
    Vst;
vi_safe({bs_get_tail,Ctx,Dst,Live}, Vst0) ->
    assert_type(#t_bs_context{}, Ctx, Vst0),
    verify_live(Live, Vst0),
    verify_y_init(Vst0),

    #t_bs_context{tail_unit=Unit} = get_raw_type(Ctx, Vst0),

    Vst = prune_x_regs(Live, Vst0),
    extract_term(#t_bitstring{size_unit=Unit}, bs_get_tail, [Ctx], Dst,
                 Vst, Vst0);
vi_safe(bs_init_writable=I, Vst) ->
    call(I, 1, Vst);
vi_safe(build_stacktrace=I, Vst) ->
    call(I, 1, Vst);
vi_safe({move,Src,Dst}, Vst) ->
    assign(Src, Dst, Vst);
vi_safe({swap,RegA,RegB}, Vst0) ->
    assert_movable(RegA, Vst0),
    assert_movable(RegB, Vst0),

    %% We don't expect fragile registers to be swapped.
    %% Therefore, we can conservatively make both registers
    %% fragile if one of the register is fragile instead of
    %% swapping the fragility of the registers.
    Sources = [RegA,RegB],
    Vst1 = propagate_fragility(RegA, Sources, Vst0),
    Vst2 = propagate_fragility(RegB, Sources, Vst1),

    %% Swap the value references.
    VrefA = get_reg_vref(RegA, Vst2),
    VrefB = get_reg_vref(RegB, Vst2),
    Vst = set_reg_vref(VrefB, RegA, Vst2),
    set_reg_vref(VrefA, RegB, Vst);
vi_safe({fmove,Src,{fr,_}=Dst}, Vst) ->
    assert_type(#t_float{}, Src, Vst),
    set_freg(Dst, Vst);
vi_safe({fmove,{fr,_}=Src,Dst}, Vst0) ->
    assert_freg_set(Src, Vst0),
    assert_fls(checked, Vst0),
    Vst = eat_heap_float(Vst0),
    create_term(#t_float{}, fmove, [], Dst, Vst);
vi_safe({kill,Reg}, Vst) ->
    create_tag(initialized, kill, [], Reg, Vst);
vi_safe({init,Reg}, Vst) ->
    create_tag(initialized, init, [], Reg, Vst);
vi_safe({test_heap,Heap,Live}, Vst) ->
    test_heap(Heap, Live, Vst);
vi_safe({bif,Op,{f,0},Ss,Dst}=I, Vst0) ->
    case will_bif_succeed(Op, Ss, Vst0) of
        yes ->
            %% This BIF cannot fail, handle it here without updating catch
            %% state.
            validate_bif(Op, cannot_fail, Ss, Dst, Vst0);
        no ->
            %% The stack will be scanned, so Y registers must be initialized.
            Vst = branch_exception(Vst0),
            verify_y_init(Vst),
            kill_state(Vst);
        maybe ->
            %% The BIF can fail, make sure that any catch state is updated.
            Vst = branch_exception(Vst0),
            vi_float(I, Vst)
    end;
vi_safe({gc_bif,Op,{f,0},Live,Ss,Dst}=I, Vst0) ->
    case will_bif_succeed(Op, Ss, Vst0) of
        yes ->
            validate_gc_bif(Op, cannot_fail, Ss, Dst, Live, Vst0);
        no ->
            Vst = branch_exception(Vst0),
            verify_y_init(Vst),
            kill_state(Vst);
        maybe ->
            Vst = branch_exception(Vst0),
            assert_float_checked(Vst),
            vi_float(I, Vst)
    end;
%% Put instructions.
vi_safe({put_list,A,B,Dst}, Vst0) ->
    Vst = eat_heap(2, Vst0),

    Head = get_term_type(A, Vst),
    Tail = get_term_type(B, Vst),

    create_term(beam_types:make_cons(Head, Tail), put_list, [A, B], Dst, Vst);
vi_safe({put_tuple2,Dst,{list,Elements}}, Vst0) ->
    _ = [assert_term(El, Vst0) || El <- Elements],
    Size = length(Elements),
    Vst = eat_heap(Size+1, Vst0),
    {Es,_} = foldl(fun(Val, {Es0, Index}) ->
                       Type = get_term_type(Val, Vst0),
                       Es = beam_types:set_tuple_element(Index, Type, Es0),
                       {Es, Index + 1}
                   end, {#{}, 1}, Elements),
    Type = #t_tuple{exact=true,size=Size,elements=Es},
    create_term(Type, put_tuple2, [], Dst, Vst);
vi_safe({put_tuple,Sz,Dst}, Vst0) when is_integer(Sz) ->
    Vst1 = eat_heap(1, Vst0),
    Vst = create_term(#t_abstract{kind=unfinished_tuple}, put_tuple, [],
                      Dst, Vst1),
    #vst{current=St0} = Vst,
    St = St0#st{puts_left={Sz,{Dst,Sz,#{}}}},
    Vst#vst{current=St};
vi_safe({put,Src}, Vst0) ->
    assert_term(Src, Vst0),
    Vst = eat_heap(1, Vst0),
    #vst{current=St0} = Vst,
    case St0 of
        #st{puts_left=none} ->
            error(not_building_a_tuple);
        #st{puts_left={1,{Dst,Sz,Es0}}} ->
            ElementType = get_term_type(Src, Vst0),
            Es = beam_types:set_tuple_element(Sz, ElementType, Es0),
            St = St0#st{puts_left=none},
            Type = #t_tuple{exact=true,size=Sz,elements=Es},
            create_term(Type, put_tuple, [], Dst, Vst#vst{current=St});
        #st{puts_left={PutsLeft,{Dst,Sz,Es0}}} when is_integer(PutsLeft) ->
            Index = Sz - PutsLeft + 1,
            ElementType = get_term_type(Src, Vst0),
            Es = beam_types:set_tuple_element(Index, ElementType, Es0),
            St = St0#st{puts_left={PutsLeft-1,{Dst,Sz,Es}}},
            Vst#vst{current=St}
    end;
%% This instruction never fails, though it may be invalid in some contexts; see
%% validate_mutation/2
vi_safe({set_tuple_element,Src,Tuple,N}, Vst) ->
    I = N + 1,
    assert_term(Src, Vst),
    assert_type(#t_tuple{size=I}, Tuple, Vst),
    %% Manually update the tuple type; we can't rely on the ordinary update
    %% helpers as we must support overwriting (rather than just widening or
    %% narrowing) known elements, and we can't use extract_term either since
    %% the source tuple may be aliased.
    #t_tuple{elements=Es0}=Type = normalize(get_term_type(Tuple, Vst)),
    Es = beam_types:set_tuple_element(I, get_term_type(Src, Vst), Es0),
    override_type(Type#t_tuple{elements=Es}, Tuple, Vst);
%% Instructions for optimization of selective receives.
vi_safe({recv_mark,{f,Fail}}, Vst) when is_integer(Fail) ->
    set_receive_marker(initialized, Vst);
vi_safe({recv_set,{f,Fail}}, Vst) when is_integer(Fail) ->
    set_receive_marker(committed, Vst);
%% Misc.
vi_safe(remove_message, Vst0) ->
    Vst = set_receive_marker(none, Vst0),

    %% The message term is no longer fragile. It can be used
    %% without restrictions.
    remove_fragility(Vst);
vi_safe({'%', {var_info, Reg, Info}}, Vst) ->
    validate_var_info(Info, Reg, Vst);
vi_safe({'%', {remove_fragility, Reg}}, Vst) ->
    %% This is a hack to make prim_eval:'receive'/2 work.
    %%
    %% Normally it's illegal to pass fragile terms as a function argument as we
    %% have no way of knowing what the callee will do with it, but we know that
    %% prim_eval:'receive'/2 won't leak the term, nor cause a GC since it's
    %% disabled while matching messages.
    remove_fragility(Reg, Vst);
vi_safe({'%',_}, Vst) ->
    Vst;
vi_safe({line,_}, Vst) ->
    Vst;

%%
%% Calls; these may be okay when the try/catch state or stack is undecided,
%% depending on whether they always succeed or always fail.
%%
vi_safe({apply,Live}, Vst) ->
    validate_body_call(apply, Live+2, Vst);
vi_safe({apply_last,Live,N}, Vst) ->
    validate_tail_call(N, apply, Live+2, Vst);
vi_safe({call,Live,Func}, Vst) ->
    validate_body_call(Func, Live, Vst);
vi_safe({call_ext,Live,Func}, Vst) ->
    validate_body_call(Func, Live, Vst);
vi_safe({call_only,Live,Func}, Vst) ->
    validate_tail_call(none, Func, Live, Vst);
vi_safe({call_ext_only,Live,Func}, Vst) ->
    validate_tail_call(none, Func, Live, Vst);
vi_safe({call_last,Live,Func,N}, Vst) ->
    validate_tail_call(N, Func, Live, Vst);
vi_safe({call_ext_last,Live,Func,N}, Vst) ->
    validate_tail_call(N, Func, Live, Vst);
vi_safe(_I, #vst{current=#st{ct=undecided}}) ->
    error(unknown_catch_try_state);
%%
%% Allocate and deallocate, et.al
%%
vi_safe({allocate,Stk,Live}, Vst) ->
    allocate(uninitialized, Stk, 0, Live, Vst);
vi_safe({allocate_heap,Stk,Heap,Live}, Vst) ->
    allocate(uninitialized, Stk, Heap, Live, Vst);
vi_safe({allocate_zero,Stk,Live}, Vst) ->
    allocate(initialized, Stk, 0, Live, Vst);
vi_safe({allocate_heap_zero,Stk,Heap,Live}, Vst) ->
    allocate(initialized, Stk, Heap, Live, Vst);
vi_safe({deallocate,StkSize}, #vst{current=#st{numy=StkSize}}=Vst) ->
    verify_no_ct(Vst),
    deallocate(Vst);
vi_safe({deallocate,_}, #vst{current=#st{numy=NumY}}) ->
    error({allocated,NumY});
vi_safe({trim,N,Remaining}, #vst{current=St0}=Vst) ->
    #st{numy=NumY} = St0,
    if
        N =< NumY, N+Remaining =:= NumY ->
            Vst#vst{current=trim_stack(N, 0, NumY, St0)};
        N > NumY; N+Remaining =/= NumY ->
            error({trim,N,Remaining,allocated,NumY})
    end;
%% Catch & try.
vi_safe({'catch',Dst,{f,Fail}}, Vst) when Fail =/= none ->
    init_try_catch_branch(catchtag, Dst, Fail, Vst);
vi_safe({'try',Dst,{f,Fail}}, Vst)  when Fail =/= none ->
    init_try_catch_branch(trytag, Dst, Fail, Vst);
vi_safe({catch_end,Reg}, #vst{current=#st{ct=[Tag|_]}}=Vst0) ->
    case get_tag_type(Reg, Vst0) of
        {catchtag,_Fail}=Tag ->
            %% Kill the catch tag and receive marker.
            %%
            %% The marker is only cleared when an exception is thrown, but it's
            %% a bit too complicated to separate those cases at the moment.
            Vst1 = kill_catch_tag(Reg, Vst0),
            Vst = set_receive_marker(none, Vst1),

            %% {x,0} contains the caught term, if any.
            create_term(any, catch_end, [], {x,0}, Vst);
        Type ->
            error({wrong_tag_type,Type})
    end;
vi_safe({try_end,Reg}, #vst{current=#st{ct=[Tag|_]}}=Vst) ->
    case get_tag_type(Reg, Vst) of
        {trytag,_Fail}=Tag ->
            %% Kill the catch tag. Note that x registers and the receive marker
            %% are unaffected.
            kill_catch_tag(Reg, Vst);
        Type ->
            error({wrong_tag_type,Type})
    end;
vi_safe({try_case,Reg}, #vst{current=#st{ct=[Tag|_]}}=Vst0) ->
    case get_tag_type(Reg, Vst0) of
        {trytag,_Fail}=Tag ->
            %% Kill the catch tag, all x registers, and the receive marker.
            Vst1 = kill_catch_tag(Reg, Vst0),
            Vst2 = prune_x_regs(0, Vst1),
            Vst3 = set_receive_marker(none, Vst2),

            %% Class:Error:Stacktrace
            Vst4 = create_term(#t_atom{}, try_case, [], {x,0}, Vst3),
            Vst = create_term(any, try_case, [], {x,1}, Vst4),
            create_term(any, try_case, [], {x,2}, Vst);
        Type ->
            error({wrong_tag_type,Type})
    end;
%% Simple getters that can't fail.
vi_safe({get_list,Src,D1,D2}, Vst0) ->
    assert_not_literal(Src),
    assert_type(#t_cons{}, Src, Vst0),

    SrcType = get_term_type(Src, Vst0),
    {HeadType, _, _} = beam_call_types:types(erlang, hd, [SrcType]),
    {TailType, _, _} = beam_call_types:types(erlang, tl, [SrcType]),

    Vst = extract_term(HeadType, get_hd, [Src], D1, Vst0),
    extract_term(TailType, get_tl, [Src], D2, Vst, Vst0);
vi_safe({get_hd,Src,Dst}, Vst) ->
    assert_not_literal(Src),
    assert_type(#t_cons{}, Src, Vst),

    SrcType = get_term_type(Src, Vst),
    {HeadType, _, _} = beam_call_types:types(erlang, hd, [SrcType]),

    extract_term(HeadType, get_hd, [Src], Dst, Vst);
vi_safe({get_tl,Src,Dst}, Vst) ->
    assert_not_literal(Src),
    assert_type(#t_cons{}, Src, Vst),

    SrcType = get_term_type(Src, Vst),
    {TailType, _, _} = beam_call_types:types(erlang, tl, [SrcType]),

    extract_term(TailType, get_tl, [Src], Dst, Vst);
vi_safe({get_tuple_element,Src,N,Dst}, Vst) ->
    Index = N+1,
    assert_not_literal(Src),
    assert_type(#t_tuple{size=Index}, Src, Vst),
    #t_tuple{elements=Es} = normalize(get_term_type(Src, Vst)),
    Type = beam_types:get_tuple_element(Index, Es),
    extract_term(Type, {bif,element}, [{integer,Index}, Src], Dst, Vst);
vi_safe({jump,{f,Lbl}}, Vst) ->
    branch(Lbl, Vst,
           fun(SuccVst) ->
                   %% The next instruction is never executed.
                   kill_state(SuccVst)
           end);

vi_safe(return, Vst) ->
    assert_durable_term({x,0}, Vst),
    verify_return(Vst);

%%
%% Matching and test instructions.
%%

vi_safe({select_val,Src,{f,Fail},{list,Choices}}, Vst) ->
    assert_term(Src, Vst),
    assert_choices(Choices),
    validate_select_val(Fail, Choices, Src, Vst);
vi_safe({select_tuple_arity,Tuple,{f,Fail},{list,Choices}}, Vst) ->
    assert_type(#t_tuple{}, Tuple, Vst),
    assert_arities(Choices),
    validate_select_tuple_arity(Fail, Choices, Tuple, Vst);
vi_safe({test,has_map_fields,{f,Lbl},Src,{list,List}}, Vst) ->
    verify_has_map_fields(Lbl, Src, List, Vst);
vi_safe({test,is_atom,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_atom{}, Src, Vst);
vi_safe({test,is_binary,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_bitstring{size_unit=8}, Src, Vst);
vi_safe({test,is_bitstr,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_bitstring{}, Src, Vst);
vi_safe({test,is_boolean,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, beam_types:make_boolean(), Src, Vst);
vi_safe({test,is_float,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_float{}, Src, Vst);
vi_safe({test,is_tuple,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_tuple{}, Src, Vst);
vi_safe({test,is_integer,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_integer{}, Src, Vst);
vi_safe({test,is_nonempty_list,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_cons{}, Src, Vst);
vi_safe({test,is_number,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, number, Src, Vst);
vi_safe({test,is_list,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_list{}, Src, Vst);
vi_safe({test,is_map,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_map{}, Src, Vst);
vi_safe({test,is_nil,{f,Lbl},[Src]}, Vst) ->
    %% is_nil is an exact check against the 'nil' value, and should not be
    %% treated as a simple type test.
    assert_term(Src, Vst),
    branch(Lbl, Vst,
           fun(FailVst) ->
                   update_ne_types(Src, nil, FailVst)
           end,
           fun(SuccVst) ->
                   update_eq_types(Src, nil, SuccVst)
           end);
vi_safe({test,test_arity,{f,Lbl},[Tuple,Sz]}, Vst) when is_integer(Sz) ->
    assert_type(#t_tuple{}, Tuple, Vst),
    Type =  #t_tuple{exact=true,size=Sz},
    type_test(Lbl, Type, Tuple, Vst);
vi_safe({test,is_tagged_tuple,{f,Lbl},[Src,Sz,Atom]}, Vst) ->
    assert_term(Src, Vst),
    Es = #{ 1 => get_literal_type(Atom) },
    Type = #t_tuple{exact=true,size=Sz,elements=Es},
    type_test(Lbl, Type, Src, Vst);
vi_safe({test,is_eq_exact,{f,Lbl},[Src,Val]=Ss}, Vst) ->
    validate_src(Ss, Vst),
    branch(Lbl, Vst,
           fun(FailVst) ->
                   update_ne_types(Src, Val, FailVst)
           end,
           fun(SuccVst) ->
                   update_eq_types(Src, Val, SuccVst)
           end);
vi_safe({test,is_ne_exact,{f,Lbl},[Src,Val]=Ss}, Vst) ->
    validate_src(Ss, Vst),
    branch(Lbl, Vst,
           fun(FailVst) ->
                   update_eq_types(Src, Val, FailVst)
           end,
           fun(SuccVst) ->
                   update_ne_types(Src, Val, SuccVst)
           end);
%%
%% New bit syntax matching instructions.
%%
vi_safe({bs_start_match4,Fail,Live,Src,Dst}, Vst) ->
    validate_bs_start_match(Fail, Live, 0, Src, Dst, Vst);
vi_safe({test,bs_start_match3,{f,_}=Fail,Live,[Src],Dst}, Vst) ->
    validate_bs_start_match(Fail, Live, 0, Src, Dst, Vst);
vi_safe({test,bs_start_match2,{f,_}=Fail,Live,[Src,Slots],Dst}, Vst) ->
    validate_bs_start_match(Fail, Live, Slots, Src, Dst, Vst);
%%
%% Bit syntax positioning
%%
vi_safe({bs_save2,Ctx,SavePoint}, Vst) ->
    bsm_save(Ctx, SavePoint, Vst);
vi_safe({bs_restore2,Ctx,SavePoint}, Vst) ->
    bsm_restore(Ctx, SavePoint, Vst);
vi_safe({bs_get_position, Ctx, Dst, Live}, Vst0) ->
    assert_type(#t_bs_context{}, Ctx, Vst0),
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    Vst = prune_x_regs(Live, Vst0),
    create_term(#t_abstract{kind=ms_position}, bs_get_position, [Ctx],
                Dst, Vst, Vst0);
vi_safe({bs_set_position, Ctx, Pos}, Vst) ->
    assert_type(#t_bs_context{}, Ctx, Vst),
    assert_type(#t_abstract{kind=ms_position}, Pos, Vst),
    Vst;
%%
%% Bit syntax matching
%%
vi_safe({test,bs_match_string,{f,Fail},[Ctx,Rem,{string,String}]}, Vst) ->
    true = is_bitstring(String),                %Assertion.
    Stride = bit_size(String) + Rem,
    validate_bs_skip(Fail, Ctx, Stride, Vst);
vi_safe({test,bs_skip_bits2,{f,Fail},[Ctx,Size,Unit,_Flags]}, Vst) ->
    assert_term(Size, Vst),

    Stride = case get_raw_type(Size, Vst) of
                 #t_integer{elements={Same,Same}} -> Same * Unit;
                 _ -> Unit
             end,

    validate_bs_skip(Fail, Ctx, Stride, Vst);
vi_safe({test,bs_test_tail2,{f,Fail},[Ctx,_Size]}, Vst) ->
    assert_type(#t_bs_context{}, Ctx, Vst),
    branch(Fail, Vst, fun(V) -> V end);
vi_safe({test,bs_test_unit,{f,Fail},[Ctx,Unit]}, Vst) ->
    assert_type(#t_bs_context{}, Ctx, Vst),
    type_test(Fail, #t_bs_context{tail_unit=Unit}, Ctx, Vst);
vi_safe({test,bs_skip_utf8,{f,Fail},[Ctx,Live,_]}, Vst) ->
    validate_bs_skip(Fail, Ctx, 8, Live, Vst);
vi_safe({test,bs_skip_utf16,{f,Fail},[Ctx,Live,_]}, Vst) ->
    validate_bs_skip(Fail, Ctx, 16, Live, Vst);
vi_safe({test,bs_skip_utf32,{f,Fail},[Ctx,Live,_]}, Vst) ->
    validate_bs_skip(Fail, Ctx, 32, Live, Vst);
vi_safe({test,bs_get_integer2=Op,{f,Fail},Live,
          [Ctx,{integer,Size},Unit,{field_flags,Flags}],Dst},Vst) ->
    NumBits = Size * Unit,
    Type = case member(unsigned, Flags) of
               true when NumBits =< 64 ->
                   beam_types:make_integer(0, (1 bsl NumBits)-1);
               _ ->
                   %% Signed integer or way too large, don't bother.
                   #t_integer{}
           end,
    validate_bs_get(Op, Fail, Ctx, Live, NumBits, Type, Dst, Vst);
vi_safe({test,bs_get_integer2=Op,{f,Fail},Live,
          [Ctx,_Size,Unit,_Flags],Dst},Vst) ->
    validate_bs_get(Op, Fail, Ctx, Live, Unit, #t_integer{}, Dst, Vst);
vi_safe({test,bs_get_float2=Op,{f,Fail},Live,[Ctx,_,_,_],Dst}, Vst) ->
    validate_bs_get(Op, Fail, Ctx, Live, 1, #t_float{}, Dst, Vst);
vi_safe({test,bs_get_binary2=Op,{f,Fail},Live,[Ctx,_,Unit,_],Dst}, Vst) ->
    Type = #t_bitstring{size_unit=Unit},
    validate_bs_get(Op, Fail, Ctx, Live, Unit, Type, Dst, Vst);
vi_safe({test,bs_get_utf8=Op,{f,Fail},Live,[Ctx,_],Dst}, Vst) ->
    Type = beam_types:make_integer(0, ?UNICODE_MAX),
    validate_bs_get(Op, Fail, Ctx, Live, 8, Type, Dst, Vst);
vi_safe({test,bs_get_utf16=Op,{f,Fail},Live,[Ctx,_],Dst}, Vst) ->
    Type = beam_types:make_integer(0, ?UNICODE_MAX),
    validate_bs_get(Op, Fail, Ctx, Live, 16, Type, Dst, Vst);
vi_safe({test,bs_get_utf32=Op,{f,Fail},Live,[Ctx,_],Dst}, Vst) ->
    Type = beam_types:make_integer(0, ?UNICODE_MAX),
    validate_bs_get(Op, Fail, Ctx, Live, 32, Type, Dst, Vst);

vi_safe({test,_Op,{f,Lbl},Src}, Vst) ->
    %% is_pid, is_reference, et cetera.
    validate_src(Src, Vst),
    branch(Lbl, Vst, fun(V) -> V end);
vi_safe({put_map_assoc=Op,{f,Fail},Src,Dst,Live,{list,List}}, Vst) ->
    verify_put_map(Op, Fail, Src, Dst, Live, List, Vst);
vi_safe({get_map_elements,{f,Fail},Src,{list,List}}, Vst) ->
    verify_get_map(Fail, Src, List, Vst);
vi_safe(I, Vst0) ->
    Vst = branch_exception(Vst0),
    vi_float(I, Vst).

validate_var_info([{fun_type, Type} | Info], Reg, Vst0) ->
    %% Explicit type information inserted after make_fun2 instructions to mark
    %% the return type of the created fun.
    Vst = update_type(fun meet/2, #t_fun{type=Type}, Reg, Vst0),
    validate_var_info(Info, Reg, Vst);
validate_var_info([{type, none} | _Info], _Reg, Vst) ->
    %% Unreachable code, typically after a call that never returns.
    kill_state(Vst);
validate_var_info([{type, Type} | Info], Reg, Vst0) ->
    %% Explicit type information inserted by optimization passes to indicate
    %% that Reg has a certain type, so that we can accept cross-function type
    %% optimizations.
    Vst = update_type(fun meet/2, Type, Reg, Vst0),
    validate_var_info(Info, Reg, Vst);
validate_var_info([_ | Info], Reg, Vst) ->
    validate_var_info(Info, Reg, Vst);
validate_var_info([], _Reg, Vst) ->
    Vst.

validate_tail_call(Deallocate, Func, Live, #vst{current=#st{numy=NumY}}=Vst0) ->
    assert_float_checked(Vst0),
    case will_call_succeed(Func, Vst0) of
        yes when Deallocate =:= NumY ->
            %% This call cannot fail, handle it without updating catch state.
            tail_call(Func, Live, Vst0);
        maybe when Deallocate =:= NumY ->
            %% The call can fail, make sure that any catch state is updated.
            Vst = branch_exception(Vst0),
            tail_call(Func, Live, Vst);
        no ->
            %% The stack will be scanned, so Y registers must be initialized.
            %%
            %% Note that the compiler is allowed to emit garbage values for
            %% "Deallocate" as we know that it will not be used in this case.
            Vst = branch_exception(Vst0),
            verify_live(Live, Vst),
            verify_y_init(Vst),
            kill_state(Vst);
        _ when Deallocate =/= NumY ->
            error({allocated, NumY})
    end.

validate_body_call(Func, Live,
                   #vst{current=#st{numy=NumY}}=Vst0) when is_integer(NumY)->
    assert_float_checked(Vst0),
    case will_call_succeed(Func, Vst0) of
        yes ->
            call(Func, Live, Vst0);
        maybe ->
            Vst = branch_exception(Vst0),
            call(Func, Live, Vst);
        no ->
            Vst = branch_exception(Vst0),
            verify_live(Live, Vst),
            verify_y_init(Vst),
            kill_state(Vst)
    end;
validate_body_call(_, _, #vst{current=#st{numy=NumY}}) ->
    error({allocated, NumY}).

assert_float_checked(Vst) ->
    case get_fls(Vst) of
        undefined -> ok;
        checked -> ok;
        Fls -> error({unsafe_instruction,{float_error_state,Fls}})
    end.

init_try_catch_branch(Kind, Dst, Fail, Vst0) ->
    Tag = {Kind, [Fail]},
    Vst = create_tag(Tag, 'try_catch', [], Dst, Vst0),

    branch(Fail, Vst,
           fun(CatchVst0) ->
                   %% We add the tag here because branch/4 rejects jumps to
                   %% labels referenced by try tags.
                   #vst{current=#st{ct=Tags,ys=Ys}=St0} = CatchVst0,
                   St = St0#st{ct=[Tag|Tags]},
                   CatchVst1 = CatchVst0#vst{current=St},

                   %% The receive marker is cleared on exceptions.
                   CatchVst = set_receive_marker(none, CatchVst1),

                   maps:fold(fun init_catch_handler_1/3, CatchVst, Ys)
           end,
           fun(SuccVst0) ->
                   #vst{current=#st{ct=Tags}=St0} = SuccVst0,
                   St = St0#st{ct=[Tag|Tags]},
                   SuccVst = SuccVst0#vst{current=St},

                   %% All potentially-throwing instructions after this one will
                   %% implicitly branch to the current try/catch handler; see
                   %% the base case of vi_safe/2
                   SuccVst
           end).

%% Set the initial state at the try/catch label. Assume that Y registers
%% contain terms or try/catch tags.
init_catch_handler_1(Reg, initialized, Vst) ->
    create_term(any, 'catch_handler', [], Reg, Vst);
init_catch_handler_1(Reg, uninitialized, Vst) ->
    create_term(any, 'catch_handler', [], Reg, Vst);
init_catch_handler_1(_, _, Vst) ->
    Vst.

branch_exception(#vst{current=#st{ct=[{_,[Fail]}|_]}}=Vst)
   when is_integer(Fail) ->
    %% We have an active try/catch tag and we can jump there from this
    %% instruction, so we need to update the branched state of the try/catch
    %% handler.
    fork_state(Fail, Vst);
branch_exception(#vst{current=#st{ct=[]}}=Vst) ->
    Vst;
branch_exception(_) ->
    error(ambiguous_catch_try_state).

%% Handle the remaining floating point instructions here.
%% Floating point.
vi_float({fconv,Src,{fr,_}=Dst}, Vst) ->
    assert_term(Src, Vst),

    %% An exception is raised on error, hence branching to 0.
    branch(0, Vst,
           fun(SuccVst0) ->
               SuccVst = update_type(fun meet/2, number, Src, SuccVst0),
               set_freg(Dst, SuccVst)
           end);
vi_float({bif,fadd,_,[_,_]=Ss,Dst}, Vst) ->
    float_op(Ss, Dst, Vst);
vi_float({bif,fdiv,_,[_,_]=Ss,Dst}, Vst) ->
    float_op(Ss, Dst, Vst);
vi_float({bif,fmul,_,[_,_]=Ss,Dst}, Vst) ->
    float_op(Ss, Dst, Vst);
vi_float({bif,fnegate,_,[_]=Ss,Dst}, Vst) ->
    float_op(Ss, Dst, Vst);
vi_float({bif,fsub,_,[_,_]=Ss,Dst}, Vst) ->
    float_op(Ss, Dst, Vst);
vi_float(fclearerror, Vst) ->
    case get_fls(Vst) of
        undefined -> ok;
        checked -> ok;
        Fls -> error({bad_floating_point_state,Fls})
    end,
    set_fls(cleared, Vst);
vi_float({fcheckerror,_}, Vst) ->
    assert_fls(cleared, Vst),
    set_fls(checked, Vst);
vi_float(I, Vst) ->
    assert_float_checked(Vst),
    vi_throwing(I, Vst).

%%%
%%% vi_throwing/2 handles instructions that can cause exceptions.
%%%
vi_throwing({badmatch,Src}, Vst) ->
    assert_durable_term(Src, Vst),
    verify_y_init(Vst),
    kill_state(Vst);
vi_throwing({case_end,Src}, Vst) ->
    assert_durable_term(Src, Vst),
    verify_y_init(Vst),
    kill_state(Vst);
vi_throwing(if_end, Vst) ->
    verify_y_init(Vst),
    kill_state(Vst);
vi_throwing({try_case_end,Src}, Vst) ->
    verify_y_init(Vst),
    assert_durable_term(Src, Vst),
    kill_state(Vst);
vi_throwing({call_fun,Live}, Vst) ->
    Fun = {x,Live},
    assert_term(Fun, Vst),

    %% An exception is raised on error, hence branching to 0.
    branch(0, Vst,
           fun(SuccVst0) ->
                   SuccVst = update_type(fun meet/2, #t_fun{arity=Live},
                                         Fun, SuccVst0),
                   validate_body_call('fun', Live+1, SuccVst)
           end);
vi_throwing({make_fun2,{f,Lbl},_,_,NumFree}, #vst{ft=Ft}=Vst0) ->
    #{ arity := Arity0 } = gb_trees:get(Lbl, Ft),
    Arity = Arity0 - NumFree,

    true = Arity >= 0,                          %Assertion.

    Vst = prune_x_regs(NumFree, Vst0),
    verify_call_args(make_fun, NumFree, Vst),
    verify_y_init(Vst),

    create_term(#t_fun{arity=Arity}, make_fun, [], {x,0}, Vst);
%% Other BIFs
vi_throwing({bif,raise,{f,0},Src,_Dst}, Vst) ->
    validate_src(Src, Vst),
    kill_state(Vst);
vi_throwing(raw_raise=I, Vst) ->
    call(I, 3, Vst);
vi_throwing({bif,Op,{f,Fail},Ss,Dst}, Vst) ->
    validate_bif(Op, Fail, Ss, Dst, Vst);
vi_throwing({gc_bif,Op,{f,Fail},Live,Ss,Dst}, Vst) ->
    validate_gc_bif(Op, Fail, Ss, Dst, Live, Vst);
vi_throwing({loop_rec,{f,Fail},Dst}, Vst) ->
    %% This term may not be part of the root set until remove_message/0 is
    %% executed. If control transfers to the loop_rec_end/1 instruction, no
    %% part of this term must be stored in a Y register.
    branch(Fail, Vst,
           fun(SuccVst0) ->
                   {Ref, SuccVst} = new_value(any, loop_rec, [], SuccVst0),
                   mark_fragile(Dst, set_reg_vref(Ref, Dst, SuccVst))
           end);
vi_throwing({wait,_}, Vst) ->
    verify_y_init(Vst),
    kill_state(Vst);
vi_throwing({wait_timeout,_,Src}, Vst) ->
    %% Note that the receive marker is not cleared since we may re-enter the
    %% loop while waiting. If we time out we'll be transferred to a timeout
    %% instruction that clears the marker.
    assert_term(Src, Vst),
    verify_y_init(Vst),
    prune_x_regs(0, Vst);
vi_throwing({loop_rec_end,_}, Vst) ->
    verify_y_init(Vst),
    kill_state(Vst);
vi_throwing(timeout, Vst0) ->
    Vst = set_receive_marker(none, Vst0),
    prune_x_regs(0, Vst);
vi_throwing(send, Vst) ->
    call(send, 2, Vst);
vi_throwing({bs_add,{f,Fail},[A,B,_],Dst}, Vst) ->
    assert_term(A, Vst),
    assert_term(B, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   create_term(#t_integer{}, bs_add, [A, B], Dst, SuccVst)
           end);
vi_throwing({bs_utf8_size,{f,Fail},A,Dst}, Vst) ->
    assert_term(A, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   create_term(#t_integer{}, bs_utf8_size, [A], Dst, SuccVst)
           end);
vi_throwing({bs_utf16_size,{f,Fail},A,Dst}, Vst) ->
    assert_term(A, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   create_term(#t_integer{}, bs_utf16_size, [A], Dst, SuccVst)
           end);
vi_throwing({bs_init2,{f,Fail},Sz,Heap,Live,_,Dst}, Vst0) ->
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    if
	is_integer(Sz) ->
	    ok;
	true ->
	    assert_term(Sz, Vst0)
    end,
    Vst = heap_alloc(Heap, Vst0),
    branch(Fail, Vst,
           fun(SuccVst0) ->
                   SuccVst = prune_x_regs(Live, SuccVst0),
                   create_term(#t_bitstring{size_unit=8}, bs_init2, [], Dst,
                               SuccVst, SuccVst0)
           end);
vi_throwing({bs_init_bits,{f,Fail},Sz,Heap,Live,_,Dst}, Vst0) ->
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    if
	is_integer(Sz) ->
	    ok;
	true ->
	    assert_term(Sz, Vst0)
    end,
    Vst = heap_alloc(Heap, Vst0),
    branch(Fail, Vst,
           fun(SuccVst0) ->
                   SuccVst = prune_x_regs(Live, SuccVst0),
                   create_term(#t_bitstring{}, bs_init_bits, [], Dst, SuccVst)
           end);
vi_throwing({bs_append,{f,Fail},Bits,Heap,Live,Unit,Bin,_Flags,Dst}, Vst0) ->
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    assert_term(Bits, Vst0),
    assert_term(Bin, Vst0),
    Vst = heap_alloc(Heap, Vst0),
    branch(Fail, Vst,
           fun(SuccVst0) ->
                   SuccVst = prune_x_regs(Live, SuccVst0),
                   create_term(#t_bitstring{size_unit=Unit}, bs_append,
                               [Bin], Dst, SuccVst, SuccVst0)
           end);
vi_throwing({bs_private_append,{f,Fail},Bits,Unit,Bin,_Flags,Dst}, Vst) ->
    assert_term(Bits, Vst),
    assert_term(Bin, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   create_term(#t_bitstring{size_unit=Unit}, bs_private_append,
                               [Bin], Dst, SuccVst)
           end);
vi_throwing({bs_put_string,Sz,_}, Vst) when is_integer(Sz) ->
    Vst;
vi_throwing({bs_put_binary,{f,Fail},Sz,_,_,Src}, Vst) ->
    assert_term(Sz, Vst),
    assert_term(Src, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   update_type(fun meet/2, #t_bitstring{}, Src, SuccVst)
           end);
vi_throwing({bs_put_float,{f,Fail},Sz,_,_,Src}, Vst) ->
    assert_term(Sz, Vst),
    assert_term(Src, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   update_type(fun meet/2, #t_float{}, Src, SuccVst)
           end);
vi_throwing({bs_put_integer,{f,Fail},Sz,_,_,Src}, Vst) ->
    assert_term(Sz, Vst),
    assert_term(Src, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   update_type(fun meet/2, #t_integer{}, Src, SuccVst)
           end);
vi_throwing({bs_put_utf8,{f,Fail},_,Src}, Vst) ->
    assert_term(Src, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   update_type(fun meet/2, #t_integer{}, Src, SuccVst)
           end);
vi_throwing({bs_put_utf16,{f,Fail},_,Src}, Vst) ->
    assert_term(Src, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   update_type(fun meet/2, #t_integer{}, Src, SuccVst)
           end);
vi_throwing({bs_put_utf32,{f,Fail},_,Src}, Vst) ->
    assert_term(Src, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   update_type(fun meet/2, #t_integer{}, Src, SuccVst)
           end);
%% Map instructions.
vi_throwing({put_map_exact=Op,{f,Fail},Src,Dst,Live,{list,List}}, Vst) ->
    verify_put_map(Op, Fail, Src, Dst, Live, List, Vst);
vi_throwing(_, _) ->
    error(unknown_instruction).

verify_has_map_fields(Lbl, Src, List, Vst) ->
    assert_type(#t_map{}, Src, Vst),
    assert_unique_map_keys(List),
    verify_map_fields(List, Src, Lbl, Vst).

verify_map_fields([Key | Keys], Map, Lbl, Vst) ->
    assert_term(Key, Vst),
    case bif_types(map_get, [Key, Map], Vst) of
        {none, _, _} -> kill_state(Vst);
        {_, _, _} -> verify_map_fields(Keys, Map, Lbl, Vst)
    end;
verify_map_fields([], _Map, Lbl, Vst) ->
    branch(Lbl, Vst, fun(V) -> V end).

verify_get_map(Fail, Src, List, Vst0) ->
    assert_not_literal(Src),                    %OTP 22.
    assert_type(#t_map{}, Src, Vst0),

    branch(Fail, Vst0,
           fun(FailVst) ->
                   clobber_map_vals(List, Src, FailVst)
           end,
           fun(SuccVst) ->
                   Keys = extract_map_keys(List),
                   assert_unique_map_keys(Keys),
                   extract_map_vals(List, Src, SuccVst, SuccVst)
           end).

%% get_map_elements may leave its destinations in an inconsistent state when
%% the fail label is taken. Consider the following:
%%
%%    {get_map_elements,{f,7},{x,1},{list,[{atom,a},{x,1},{atom,b},{x,2}]}}.
%%
%% If 'a' exists but not 'b', {x,1} is overwritten when we jump to {f,7}.
%%
%% We must be careful to preserve the uninitialized status for Y registers
%% that have been allocated but not yet defined.
clobber_map_vals([Key,Dst|T], Map, Vst0) ->
    case is_reg_initialized(Dst, Vst0) of
        true ->
            Vst = extract_term(any, {bif,map_get}, [Key, Map], Dst, Vst0),
            clobber_map_vals(T, Map, Vst);
        false ->
            clobber_map_vals(T, Map, Vst0)
    end;
clobber_map_vals([], _Map, Vst) ->
    Vst.

is_reg_initialized({x,_}=Reg, #vst{current=#st{xs=Xs}}) ->
    is_map_key(Reg, Xs);
is_reg_initialized({y,_}=Reg, #vst{current=#st{ys=Ys}}) ->
    case Ys of
        #{Reg:=Val} ->
            Val =/= uninitialized;
        #{} ->
            false
    end;
is_reg_initialized(V, #vst{}) -> error({not_a_register, V}).

extract_map_keys([Key,_Val|T]) ->
    [Key|extract_map_keys(T)];
extract_map_keys([]) -> [].

extract_map_vals([Key, Dst | Vs], Map, Vst0, Vsti0) ->
    assert_term(Key, Vst0),
    case bif_types(map_get, [Key, Map], Vst0) of
        {none, _, _} ->
            kill_state(Vsti0);
        {DstType, _, _} ->
            Vsti = extract_term(DstType, {bif,map_get}, [Key, Map], Dst, Vsti0),
            extract_map_vals(Vs, Map, Vst0, Vsti)
    end;
extract_map_vals([], _Map, _Vst0, Vst) ->
    Vst.

verify_put_map(Op, Fail, Src, Dst, Live, List, Vst0) ->
    assert_type(#t_map{}, Src, Vst0),
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    _ = [assert_term(Term, Vst0) || Term <- List],
    Vst = heap_alloc(0, Vst0),

    branch(Fail, Vst,
           fun(SuccVst0) ->
                   SuccVst = prune_x_regs(Live, SuccVst0),
                   Keys = extract_map_keys(List),
                   assert_unique_map_keys(Keys),

                   Type = put_map_type(Src, List, Vst),
                   create_term(Type, Op, [Src], Dst, SuccVst, SuccVst0)
           end).

put_map_type(Map0, List, Vst) ->
    Map = normalize(get_term_type(Map0, Vst)),
    pmt_1(List, Vst, Map).

pmt_1([Key0, Value0 | List], Vst, Acc0) ->
    Key = normalize(get_term_type(Key0, Vst)),
    Value = normalize(get_term_type(Value0, Vst)),
    {Acc, _, _} = beam_call_types:types(maps, put, [Key, Value, Acc0]),
    pmt_1(List, Vst, Acc);
pmt_1([], _Vst, Acc) ->
    Acc.

%%
%% Common code for validating returns, whether naked or as part of a tail call.
%%

verify_return(#vst{current=#st{numy=NumY}}) when NumY =/= none ->
    error({stack_frame,NumY});
verify_return(#vst{current=#st{recv_marker=Mark}}) when Mark =/= none ->
    %% If the receive marker has not been cleared upon function return it may
    %% taint a completely unrelated receive. Note that the marker does not need
    %% to be committed to cause problems. Consider the following:
    %%
    %%    foo() ->
    %%        %% recv_mark
    %%        A = make_ref(),
    %%        bar(),
    %%        %% recv_set
    %%        receive A -> ok end.
    %%
    %% If bar/1 were to return with an initialized marker, the recv_set could
    %% refer to a position *after* `A = make_ref()`, making the receive skip
    %% the message.
    error({return_with_receive_marker,Mark});
verify_return(Vst) ->
    verify_no_ct(Vst),
    kill_state(Vst).

%%
%% Common code for validating BIFs.
%%
%% OrigVst is the state we entered the instruction with, which is needed for
%% gc_bifs as X registers are pruned prior to calling this function, which may
%% have clobbered the sources.
%%

validate_bif(Op, Fail, Ss, Dst, Vst) ->
    validate_src(Ss, Vst),
    validate_bif_1(bif, Op, Fail, Ss, Dst, Vst, Vst).

validate_gc_bif(Op, Fail, Ss, Dst, Live, #vst{current=St0}=Vst0) ->
    validate_src(Ss, Vst0),
    verify_live(Live, Vst0),
    verify_y_init(Vst0),

    %% Heap allocations and X registers are killed regardless of whether we
    %% fail or not, as we may fail after GC.
    St = kill_heap_allocation(St0),
    Vst = prune_x_regs(Live, Vst0#vst{current=St}),
    validate_src(Ss, Vst),

    validate_bif_1(gc_bif, Op, Fail, Ss, Dst, Vst, Vst).

validate_bif_1(Kind, Op, cannot_fail, Ss, Dst, OrigVst, Vst0) ->
    %% This BIF explicitly cannot fail; it will not jump to a guard nor throw
    %% an exception. Validation will fail if it returns 'none' or has a type
    %% conflict on one of its arguments.

    {Type, ArgTypes, _CanSubtract} = bif_types(Op, Ss, Vst0),
    ZippedArgs = zip(Ss, ArgTypes),

    Vst = foldl(fun({A, T}, V) ->
                            update_type(fun meet/2, T, A, V)
                    end, Vst0, ZippedArgs),

    true = Type =/= none,                       %Assertion.

    extract_term(Type, {Kind, Op}, Ss, Dst, Vst, OrigVst);
validate_bif_1(Kind, Op, Fail, Ss, Dst, OrigVst, Vst) ->
    {Type, ArgTypes, CanSubtract} = bif_types(Op, Ss, Vst),
    ZippedArgs = zip(Ss, ArgTypes),

    FailFun = case CanSubtract of
                  true ->
                          fun(FailVst0) ->
                              foldl(fun({A, T}, V) ->
                                            update_type(fun subtract/2, T, A, V)
                                    end, FailVst0, ZippedArgs)
                          end;
                  false ->
                      fun(S) -> S end
               end,
    SuccFun = fun(SuccVst0) ->
                  SuccVst = foldl(fun({A, T}, V) ->
                                          update_type(fun meet/2, T, A, V)
                                  end, SuccVst0, ZippedArgs),
                  extract_term(Type, {Kind, Op}, Ss, Dst, SuccVst, OrigVst)
              end,

    branch(Fail, Vst, FailFun, SuccFun).

%%
%% Common code for validating bs_start_match* instructions.
%%

validate_bs_start_match({atom,resume}, Live, 0, Src, Dst, Vst0) ->
    assert_type(#t_bs_context{}, Src, Vst0),
    verify_live(Live, Vst0),
    verify_y_init(Vst0),

    Vst = assign(Src, Dst, Vst0),
    prune_x_regs(Live, Vst);
validate_bs_start_match({atom,no_fail}, Live, Slots, Src, Dst, Vst0) ->
    verify_live(Live, Vst0),
    verify_y_init(Vst0),

    Vst1 = update_type(fun meet/2, #t_bs_matchable{}, Src, Vst0),

    %% Retain the current unit, if known.
    SrcType = get_movable_term_type(Src, Vst1),
    TailUnit = beam_types:get_bs_matchable_unit(SrcType),

    CtxType = #t_bs_context{slots=Slots,tail_unit=TailUnit},

    Vst = prune_x_regs(Live, Vst1),
    extract_term(CtxType, bs_start_match, [Src], Dst, Vst, Vst0);
validate_bs_start_match({f,Fail}, Live, Slots, Src, Dst, Vst) ->
    branch(Fail, Vst,
           fun(FailVst) ->
                   update_type(fun subtract/2, #t_bs_matchable{}, Src, FailVst)
           end,
           fun(SuccVst) ->
                   validate_bs_start_match({atom,no_fail}, Live, Slots,
                                           Src, Dst, SuccVst)
           end).

%%
%% Common code for validating bs_get* instructions.
%%
validate_bs_get(Op, Fail, Ctx, Live, Stride, Type, Dst, Vst) ->
    assert_type(#t_bs_context{}, Ctx, Vst),
    verify_live(Live, Vst),
    verify_y_init(Vst),

    #t_bs_context{tail_unit=TailUnit} = get_raw_type(Ctx, Vst),
    CtxType = #t_bs_context{tail_unit=gcd(Stride, TailUnit)},

    branch(Fail, Vst,
           fun(SuccVst0) ->
                   SuccVst1 = update_type(fun meet/2, CtxType, Ctx, SuccVst0),
                   SuccVst = prune_x_regs(Live, SuccVst1),
                   extract_term(Type, Op, [Ctx], Dst, SuccVst, SuccVst0)
           end).

%%
%% Common code for validating bs_skip* instructions.
%%
validate_bs_skip(Fail, Ctx, Stride, Vst) ->
    validate_bs_skip(Fail, Ctx, Stride, no_live, Vst).

validate_bs_skip(Fail, Ctx, Stride, Live, Vst) ->
    assert_type(#t_bs_context{}, Ctx, Vst),

    #t_bs_context{tail_unit=TailUnit} = get_raw_type(Ctx, Vst),
    CtxType = #t_bs_context{tail_unit=gcd(Stride, TailUnit)},

    validate_bs_skip_1(Fail, Ctx, CtxType, Live, Vst).

validate_bs_skip_1(Fail, Ctx, CtxType, no_live, Vst) ->
    branch(Fail, Vst,
           fun(SuccVst0) ->
                   update_type(fun meet/2, CtxType, Ctx, SuccVst0)
           end);
validate_bs_skip_1(Fail, Ctx, CtxType, Live, Vst) ->
    verify_y_init(Vst),
    verify_live(Live, Vst),
    branch(Fail, Vst,
           fun(SuccVst0) ->
                   SuccVst = update_type(fun meet/2, CtxType, Ctx, SuccVst0),
                   prune_x_regs(Live, SuccVst)
           end).
%%
%% Common code for is_$type instructions.
%%
type_test(Fail, #t_bs_context{}=Type, Reg, Vst) ->
    assert_movable(Reg, Vst),
    type_test_1(Fail, Type, Reg, Vst);
type_test(Fail, Type, Reg, Vst) ->
    assert_term(Reg, Vst),
    type_test_1(Fail, Type, Reg, Vst).

type_test_1(Fail, Type, Reg, Vst) ->
    branch(Fail, Vst,
           fun(FailVst) ->
                   update_type(fun subtract/2, Type, Reg, FailVst)
           end,
           fun(SuccVst) ->
                   update_type(fun meet/2, Type, Reg, SuccVst)
           end).

%%
%% Special state handling for setelement/3 and set_tuple_element/3 instructions.
%% A possibility for garbage collection must not occur between setelement/3 and
%% set_tuple_element/3.
%%
%% Note that #vst.current will be 'none' if the instruction is unreachable.
%%

validate_mutation(I, Vst) ->
    vm_1(I, Vst).

vm_1({move,_,_}, Vst) ->
    Vst;
vm_1({swap,_,_}, Vst) ->
    Vst;
vm_1({call_ext,3,{extfunc,erlang,setelement,3}}, #vst{current=#st{}=St}=Vst) ->
    Vst#vst{current=St#st{setelem=true}};
vm_1({set_tuple_element,_,_,_}, #vst{current=#st{setelem=false}}) ->
    error(illegal_context_for_set_tuple_element);
vm_1({set_tuple_element,_,_,_}, #vst{current=#st{setelem=true}}=Vst) ->
    Vst;
vm_1({get_tuple_element,_,_,_}, Vst) ->
    Vst;
vm_1({line,_}, Vst) ->
    Vst;
vm_1(_, #vst{current=#st{setelem=true}=St}=Vst) ->
    Vst#vst{current=St#st{setelem=false}};
vm_1(_, Vst) -> Vst.

kill_state(Vst) ->
    Vst#vst{current=none}.

%% A "plain" call.
%%  The stackframe must be initialized.
%%  The instruction will return to the instruction following the call.
call(Name, Live, #vst{current=St0}=Vst0) ->
    verify_call_args(Name, Live, Vst0),
    verify_y_init(Vst0),
    case call_types(Name, Live, Vst0) of
        {none, _, _} ->
            kill_state(Vst0);
        {RetType, _, _} ->
            St = St0#st{f=init_fregs()},
            Vst = prune_x_regs(0, Vst0#vst{current=St}),
            create_term(RetType, call, [], {x,0}, Vst)
    end.

%% Tail call.
%%  The stackframe must have a known size and be initialized.
%%  Does not return to the instruction following the call.
tail_call(Name, Live, Vst0) ->
    verify_y_init(Vst0),
    Vst = deallocate(Vst0),
    verify_call_args(Name, Live, Vst),
    verify_return(Vst).

verify_call_args(_, 0, #vst{}) ->
    ok;
verify_call_args({f,Lbl}, Live, #vst{ft=Ft}=Vst) when is_integer(Live) ->
    case gb_trees:lookup(Lbl, Ft) of
        {value, FuncInfo} ->
            #{ arity := Live,
               parameter_info := ParamInfo } = FuncInfo,
            verify_local_args(Live - 1, ParamInfo, #{}, Vst);
        none ->
            error(local_call_to_unknown_function)
    end;
verify_call_args(_, Live, Vst) when is_integer(Live)->
    verify_remote_args_1(Live - 1, Vst);
verify_call_args(_, Live, _) ->
    error({bad_number_of_live_regs,Live}).

verify_remote_args_1(-1, _) ->
    ok;
verify_remote_args_1(X, Vst) ->
    assert_durable_term({x, X}, Vst),
    verify_remote_args_1(X - 1, Vst).

verify_local_args(-1, _ParamInfo, _CtxIds, _Vst) ->
    ok;
verify_local_args(X, ParamInfo, CtxRefs, Vst) ->
    Reg = {x, X},
    assert_not_fragile(Reg, Vst),
    case get_movable_term_type(Reg, Vst) of
        #t_bs_context{}=Type ->
            VRef = get_reg_vref(Reg, Vst),
            case CtxRefs of
                #{ VRef := Other } ->
                    error({multiple_match_contexts, [Reg, Other]});
                #{} ->
                    verify_arg_type(Reg, Type, ParamInfo),
                    verify_local_args(X - 1, ParamInfo,
                                      CtxRefs#{ VRef => Reg }, Vst)
            end;
        Type ->
            verify_arg_type(Reg, Type, ParamInfo),
            verify_local_args(X - 1, ParamInfo, CtxRefs, Vst)
    end.

verify_arg_type(Reg, GivenType, ParamTypes) ->
    case {ParamTypes, GivenType} of
        {#{ Reg := Info }, #t_bs_context{}} ->
            %% Match contexts require explicit support, and may not be passed
            %% to a function that accepts arbitrary terms.
            case member(accepts_match_context, Info) of
                true -> verify_arg_type_1(Reg, GivenType, Info);
                false -> error(no_bs_start_match2)
            end;
        {_, #t_bs_context{}} ->
            error(no_bs_start_match2);
        {#{ Reg := Info }, _} ->
            verify_arg_type_1(Reg, GivenType, Info);
        {#{}, _} ->
            ok
    end.

verify_arg_type_1(Reg, GivenType, Info) ->
    RequiredType = proplists:get_value(type, Info, any),
    case meet(GivenType, RequiredType) of
        GivenType -> ok;
        _ -> error({bad_arg_type, Reg, GivenType, RequiredType})
    end.

allocate(Tag, Stk, Heap, Live, #vst{current=#st{numy=none}=St}=Vst0) ->
    verify_live(Live, Vst0),
    Vst1 = Vst0#vst{current=St#st{numy=Stk}},
    Vst2 = prune_x_regs(Live, Vst1),
    Vst = init_stack(Tag, Stk - 1, Vst2),
    heap_alloc(Heap, Vst);
allocate(_, _, _, _, #vst{current=#st{numy=Numy}}) ->
    error({existing_stack_frame,{size,Numy}}).

deallocate(#vst{current=St}=Vst) ->
    Vst#vst{current=St#st{ys=#{},numy=none}}.

init_stack(_Tag, -1, Vst) ->
    Vst;
init_stack(Tag, Y, Vst) ->
    init_stack(Tag, Y - 1, create_tag(Tag, allocate, [], {y,Y}, Vst)).

trim_stack(From, To, Top, #st{ys=Ys0}=St) when From =:= Top ->
    Ys = maps:filter(fun({y,Y}, _) -> Y < To end, Ys0),
    St#st{numy=To,ys=Ys};
trim_stack(From, To, Top, St0) ->
    Src = {y, From},
    Dst = {y, To},

    #st{ys=Ys0} = St0,
    Ys = case Ys0 of
             #{ Src := Ref } -> Ys0#{ Dst => Ref };
             #{} -> error({invalid_shift,Src,Dst})
         end,
    St = St0#st{ys=Ys},

    trim_stack(From + 1, To + 1, Top, St).

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

prune_x_regs(Live, #vst{current=St0}=Vst) when is_integer(Live) ->
    #st{fragile=Fragile0,xs=Xs0} = St0,
    Fragile = cerl_sets:filter(fun({x,X}) ->
                                       X < Live;
                                  ({y,_}) ->
                                       true
                               end, Fragile0),
    Xs = maps:filter(fun({x,X}, _) ->
                             X < Live
                     end, Xs0),
    St = St0#st{fragile=Fragile,xs=Xs},
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

float_op(Ss, Dst, Vst0) ->
    _ = [assert_freg_set(S, Vst0) || S <- Ss],
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

set_freg({fr,Fr}=Freg, #vst{current=#st{f=Fregs0}=St}=Vst) ->
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
    case length(Vs) =:= cerl_sets:size(cerl_sets:from_list(Vs)) of
	true -> ok;
	false -> error(keys_not_unique)
    end.

%%%
%%% New binary matching instructions.
%%%

bsm_save(Reg, {atom,start}, Vst) ->
    %% Save point refering to where the match started.
    %% It is always valid. But don't forget to validate the context register.
    assert_type(#t_bs_context{}, Reg, Vst),
    Vst;
bsm_save(Reg, SavePoint, Vst) ->
    case get_movable_term_type(Reg, Vst) of
        #t_bs_context{valid=Bits,slots=Slots}=Ctxt0 when SavePoint < Slots ->
            Ctx = Ctxt0#t_bs_context{valid=Bits bor (1 bsl SavePoint),
                                     slots=Slots},
            override_type(Ctx, Reg, Vst);
        _ ->
            error({illegal_save, SavePoint})
    end.

bsm_restore(Reg, {atom,start}, Vst) ->
    %% (Mostly) automatic save point refering to where the match started.
    %% It is always valid. But don't forget to validate the context register.
    assert_type(#t_bs_context{}, Reg, Vst),
    Vst;
bsm_restore(Reg, SavePoint, Vst) ->
    case get_movable_term_type(Reg, Vst) of
        #t_bs_context{valid=Bits,slots=Slots} when SavePoint < Slots ->
            case Bits band (1 bsl SavePoint) of
                0 -> error({illegal_restore, SavePoint, not_set});
                _ -> Vst
            end;
        _ ->
            error({illegal_restore, SavePoint, range})
    end.

validate_select_val(_Fail, _Choices, _Src, #vst{current=none}=Vst) ->
    %% We've already branched on all of Src's possible values, so we know we
    %% can't reach the fail label or any of the remaining choices.
    Vst;
validate_select_val(Fail, [Val,{f,L}|T], Src, Vst0) ->
    Vst = branch(L, Vst0,
                 fun(BranchVst) ->
                         update_eq_types(Src, Val, BranchVst)
                 end,
                 fun(FailVst) ->
                         update_ne_types(Src, Val, FailVst)
                 end),
    validate_select_val(Fail, T, Src, Vst);
validate_select_val(Fail, [], _Src, Vst) ->
    branch(Fail, Vst,
           fun(SuccVst) ->
                   %% The next instruction is never executed.
                   kill_state(SuccVst)
           end).

validate_select_tuple_arity(_Fail, _Choices, _Src, #vst{current=none}=Vst) ->
    %% We've already branched on all of Src's possible values, so we know we
    %% can't reach the fail label or any of the remaining choices.
    Vst;
validate_select_tuple_arity(Fail, [Arity,{f,L}|T], Tuple, Vst0) ->
    Type = #t_tuple{exact=true,size=Arity},
    Vst = branch(L, Vst0,
                 fun(BranchVst) ->
                         update_type(fun meet/2, Type, Tuple, BranchVst)
                 end,
                 fun(FailVst) ->
                         update_type(fun subtract/2, Type, Tuple, FailVst)
                 end),
    validate_select_tuple_arity(Fail, T, Tuple, Vst);
validate_select_tuple_arity(Fail, [], _, #vst{}=Vst) ->
    branch(Fail, Vst,
           fun(SuccVst) ->
                   %% The next instruction is never executed.
                   kill_state(SuccVst)
           end).

%%
%% Infers types from comparisons, looking at the expressions that produced the
%% compared values and updates their types if we've learned something new from
%% the comparison.
%%

infer_types(CompareOp, {Kind,_}=LHS, RHS, Vst) when Kind =:= x; Kind =:= y ->
    infer_types(CompareOp, get_reg_vref(LHS, Vst), RHS, Vst);
infer_types(CompareOp, LHS, {Kind,_}=RHS, Vst) when Kind =:= x; Kind =:= y ->
    infer_types(CompareOp, LHS, get_reg_vref(RHS, Vst), Vst);
infer_types(CompareOp, LHS, RHS, #vst{current=#st{vs=Vs}}=Vst0) ->
    case Vs of
        #{ LHS := LEntry, RHS := REntry } ->
            Vst = infer_types_1(LEntry, RHS, CompareOp, Vst0),
            infer_types_1(REntry, LHS, CompareOp, Vst);
        #{ LHS := LEntry } ->
            infer_types_1(LEntry, RHS, CompareOp, Vst0);
        #{ RHS := REntry } ->
            infer_types_1(REntry, LHS, CompareOp, Vst0);
        #{} ->
            Vst0
    end.

infer_types_1(#value{op={bif,'=:='},args=[LHS,RHS]}, Val, Op, Vst) ->
    case Val of
        {atom, Bool} when Op =:= eq_exact, Bool; Op =:= ne_exact, not Bool ->
            update_eq_types(LHS, RHS, Vst);
        {atom, Bool} when Op =:= ne_exact, Bool; Op =:= eq_exact, not Bool ->
            update_ne_types(LHS, RHS, Vst);
        _ ->
            Vst
    end;
infer_types_1(#value{op={bif,'=/='},args=[LHS,RHS]}, Val, Op, Vst) ->
    case Val of
        {atom, Bool} when Op =:= ne_exact, Bool; Op =:= eq_exact, not Bool ->
            update_ne_types(LHS, RHS, Vst);
        {atom, Bool} when Op =:= eq_exact, Bool; Op =:= ne_exact, not Bool ->
            update_eq_types(LHS, RHS, Vst);
        _ ->
            Vst
    end;
infer_types_1(#value{op={bif,element},args=[{integer,Index},Tuple]},
              Val, Op, Vst) when Index >= 1 ->
    ElementType = get_term_type(Val, Vst),
    Es = beam_types:set_tuple_element(Index, ElementType, #{}),
    TupleType = #t_tuple{size=Index,elements=Es},
    case Op of
        eq_exact ->
            update_type(fun meet/2, TupleType, Tuple, Vst);
        ne_exact ->
            %% Subtraction is only safe when ElementType is single-valued and
            %% the index is below the tuple element limit.
            case beam_types:is_singleton_type(ElementType) of
                true when Es =/= #{} ->
                    update_type(fun subtract/2, TupleType, Tuple, Vst);
                _ ->
                    Vst
            end
    end;
infer_types_1(#value{op={bif,is_atom},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(#t_atom{}, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,is_boolean},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(beam_types:make_boolean(), Src, Val, Op, Vst);
infer_types_1(#value{op={bif,is_binary},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(#t_bitstring{size_unit=8}, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,is_bitstring},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(#t_bitstring{}, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,is_float},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(#t_float{}, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,is_integer},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(#t_integer{}, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,is_list},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(#t_list{}, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,is_map},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(#t_map{}, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,is_number},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(number, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,is_tuple},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(#t_tuple{}, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,tuple_size}, args=[Tuple]},
              {integer,Arity}, Op, Vst) ->
    Type = #t_tuple{exact=true,size=Arity},
    case Op of
        eq_exact -> update_type(fun meet/2, Type, Tuple, Vst);
        ne_exact -> update_type(fun subtract/2, Type, Tuple, Vst)
    end;
infer_types_1(_, _, _, Vst) ->
    Vst.

infer_type_test_bif(Type, Src, Val, Op, Vst) ->
    case Val of
        {atom, Bool} when Op =:= eq_exact, Bool; Op =:= ne_exact, not Bool ->
            update_type(fun meet/2, Type, Src, Vst);
        {atom, Bool} when Op =:= ne_exact, Bool; Op =:= eq_exact, not Bool ->
            update_type(fun subtract/2, Type, Src, Vst);
        _ ->
            Vst
    end.

%%%
%%% Keeping track of types.
%%%

%% Assigns Src to Dst and marks them as aliasing each other.
assign({y,_}=Src, {y,_}=Dst, Vst) ->
    %% The stack trimming optimization may generate a move from an initialized
    %% but unassigned Y register to another Y register.
    case get_raw_type(Src, Vst) of
        initialized -> create_tag(initialized, init, [], Dst, Vst);
        _ -> assign_1(Src, Dst, Vst)
    end;
assign({Kind,_}=Src, Dst, Vst) when Kind =:= x; Kind =:= y ->
    assign_1(Src, Dst, Vst);
assign(Literal, Dst, Vst) ->
    Type = get_literal_type(Literal),
    create_term(Type, move, [Literal], Dst, Vst).

%% Creates a special tag value that isn't a regular term, such as 'initialized'
%% or 'catchtag'
create_tag(Tag, _Op, _Ss, {y,_}=Dst, #vst{current=#st{ys=Ys0}=St0}=Vst) ->
    case maps:get(Dst, Ys0, uninitialized) of
        {catchtag,_}=Prev ->
            error(Prev);
        {trytag,_}=Prev ->
            error(Prev);
        _ ->
            check_try_catch_tags(Tag, Dst, Vst),
            Ys = Ys0#{ Dst => Tag },
            St = St0#st{ys=Ys},
            remove_fragility(Dst, Vst#vst{current=St})
    end;
create_tag(_Tag, _Op, _Ss, Dst, _Vst) ->
    error({invalid_tag_register, Dst}).

%% Wipes a special tag, leaving the register initialized but empty.
kill_tag({y,_}=Reg, #vst{current=#st{ys=Ys0}=St0}=Vst) ->
    _ = get_tag_type(Reg, Vst),                 %Assertion.
    Ys = Ys0#{ Reg => initialized },
    Vst#vst{current=St0#st{ys=Ys}}.

%% Creates a completely new term with the given type.
create_term(Type, Op, Ss0, Dst, Vst0) ->
    create_term(Type, Op, Ss0, Dst, Vst0, Vst0).

%% As create_term/4, but uses the incoming Vst for argument resolution in
%% case x-regs have been pruned and the sources can no longer be found.
create_term(Type, Op, Ss0, Dst, Vst0, OrigVst) ->
    {Ref, Vst1} = new_value(Type, Op, resolve_args(Ss0, OrigVst), Vst0),
    Vst = remove_fragility(Dst, Vst1),
    set_reg_vref(Ref, Dst, Vst).

%% Extracts a term from Ss, propagating fragility.
extract_term(Type, Op, Ss0, Dst, Vst0) ->
    extract_term(Type, Op, Ss0, Dst, Vst0, Vst0).

%% As extract_term/4, but uses the incoming Vst for argument resolution in
%% case x-regs have been pruned and the sources can no longer be found.
extract_term(Type, Op, Ss0, Dst, Vst0, OrigVst) ->
    {Ref, Vst1} = new_value(Type, Op, resolve_args(Ss0, OrigVst), Vst0),
    Vst = propagate_fragility(Dst, Ss0, Vst1),
    set_reg_vref(Ref, Dst, Vst).

%% Translates instruction arguments into the argument() type, decoupling them
%% from their registers, allowing us to infer their types after they've been
%% clobbered or moved.
resolve_args([{Kind,_}=Src | Args], Vst) when Kind =:= x; Kind =:= y ->
    [get_reg_vref(Src, Vst) | resolve_args(Args, Vst)];
resolve_args([Lit | Args], Vst) ->
    assert_literal(Lit),
    [Lit | resolve_args(Args, Vst)];
resolve_args([], _Vst) ->
    [].

%% Overrides the type of Reg. This is ugly but a necessity for certain
%% destructive operations.
override_type(Type, Reg, Vst) ->
    update_type(fun(_, T) -> T end, Type, Reg, Vst).

%% This is used when linear code finds out more and more information about a
%% type, so that the type gets more specialized.
update_type(Merge, With, #value_ref{}=Ref, Vst) ->
    %% If the old type can't be merged with the new one, the type information
    %% is inconsistent and we know that some instructions will never be
    %% executed at run-time. For example:
    %%
    %%   {test,is_list,Fail,[Reg]}.
    %%   {test,is_tuple,Fail,[Reg]}.
    %%   {test,test_arity,Fail,[Reg,5]}.
    %%
    %% Note that the test_arity instruction can never be reached, so we need to
    %% kill the state to avoid raising an error when we encounter it.
    %%
    %% Simply returning `kill_state(Vst)` is unsafe however as we might be in
    %% the middle of an instruction, and altering the rest of the validator
    %% (eg. prune_x_regs/2) to no-op on dead states is prone to error.
    %%
    %% We therefore throw a 'type_conflict' error instead, which causes
    %% validation to fail unless we're in a context where such errors can be
    %% handled, such as in a branch handler.
    Current = get_raw_type(Ref, Vst),
    case Merge(Current, With) of
        none -> throw({type_conflict, Current, With});
        Type -> set_type(Type, Ref, Vst)
    end;
update_type(Merge, With, {Kind,_}=Reg, Vst) when Kind =:= x; Kind =:= y ->
    update_type(Merge, With, get_reg_vref(Reg, Vst), Vst);
update_type(Merge, With, Literal, Vst) ->
    %% Literals always retain their type, but we still need to bail on type
    %% conflicts.
    Type = get_literal_type(Literal),
    case Merge(Type, With) of
        none -> throw({type_conflict, Type, With});
        _Type -> Vst
    end.

update_eq_types(LHS, RHS, Vst0) ->
    LType = get_term_type(LHS, Vst0),
    RType = get_term_type(RHS, Vst0),

    Vst1 = update_type(fun meet/2, RType, LHS, Vst0),
    Vst = update_type(fun meet/2, LType, RHS, Vst1),

    infer_types(eq_exact, LHS, RHS, Vst).

update_ne_types(LHS, RHS, Vst0) ->
    Vst1 = update_ne_types_1(LHS, RHS, Vst0),
    Vst = update_ne_types_1(RHS, LHS, Vst1),

    infer_types(ne_exact, LHS, RHS, Vst).

update_ne_types_1(LHS, RHS, Vst0) ->
    %% While updating types on equality is fairly straightforward, inequality
    %% is a bit trickier since all we know is that the *value* of LHS differs
    %% from RHS, so we can't blindly subtract their types.
    %%
    %% Consider `number =/= #t_integer{}`; all we know is that LHS isn't equal
    %% to some *specific integer* of unknown value, and if we were to subtract
    %% #t_integer{} we would erroneously infer that the new type is float.
    %%
    %% Therefore, we only subtract when we know that RHS has a specific value.
    RType = get_term_type(RHS, Vst0),
    case beam_types:is_singleton_type(RType) of
        true ->
            Vst = update_type(fun subtract/2, RType, LHS, Vst0),

            %% If LHS has a specific value after subtraction we can infer types
            %% as if we've made an exact match, which is much stronger than
            %% ne_exact.
            LType = get_term_type(LHS, Vst),
            case beam_types:get_singleton_value(LType) of
                {ok, Value} ->
                    infer_types(eq_exact, LHS, value_to_literal(Value), Vst);
                error ->
                    Vst
            end;
        false ->
            Vst0
    end.

%% Helper functions for the above.

assign_1(Src, Dst, Vst0) ->
    assert_movable(Src, Vst0),
    Vst = propagate_fragility(Dst, [Src], Vst0),
    set_reg_vref(get_reg_vref(Src, Vst), Dst, Vst).

set_reg_vref(Ref, {x,_}=Dst, Vst) ->
    check_limit(Dst),
    #vst{current=#st{xs=Xs0}=St0} = Vst,
    St = St0#st{xs=Xs0#{ Dst => Ref }},
    Vst#vst{current=St};
set_reg_vref(Ref, {y,_}=Dst, #vst{current=#st{ys=Ys0}=St0} = Vst) ->
    check_limit(Dst),
    case Ys0 of
        #{ Dst := {catchtag,_}=Tag } ->
            error(Tag);
        #{ Dst := {trytag,_}=Tag } ->
            error(Tag);
        #{ Dst := _ } ->
            St = St0#st{ys=Ys0#{ Dst => Ref }},
            Vst#vst{current=St};
        #{} ->
            %% Storing into a non-existent Y register means that we haven't set
            %% up a (sufficiently large) stack.
            error({invalid_store, Dst})
    end.

get_reg_vref({x,_}=Src, #vst{current=#st{xs=Xs}}) ->
    check_limit(Src),
    case Xs of
        #{ Src := #value_ref{}=Ref } ->
            Ref;
        #{} ->
            error({uninitialized_reg, Src})
    end;
get_reg_vref({y,_}=Src, #vst{current=#st{ys=Ys}}) ->
    check_limit(Src),
    case Ys of
        #{ Src := #value_ref{}=Ref } ->
            Ref;
        #{ Src := initialized } ->
            error({unassigned, Src});
        #{ Src := Tag } when Tag =/= uninitialized ->
            error(Tag);
        #{} ->
            error({uninitialized_reg, Src})
    end.

set_type(Type, #value_ref{}=Ref, #vst{current=#st{vs=Vs0}=St}=Vst) ->
    #{ Ref := #value{}=Entry } = Vs0,
    Vs = Vs0#{ Ref => Entry#value{type=Type} },
    Vst#vst{current=St#st{vs=Vs}}.

new_value(Type, Op, Ss, #vst{current=#st{vs=Vs0}=St,ref_ctr=Counter}=Vst) ->
    Ref = #value_ref{id=Counter},
    Vs = Vs0#{ Ref => #value{op=Op,args=Ss,type=Type} },

    {Ref, Vst#vst{current=St#st{vs=Vs},ref_ctr=Counter+1}}.

kill_catch_tag(Reg, #vst{current=#st{ct=[Tag|Tags]}=St}=Vst0) ->
    Vst = Vst0#vst{current=St#st{ct=Tags,fls=undefined}},
    Tag = get_tag_type(Reg, Vst),               %Assertion.
    kill_tag(Reg, Vst).

check_try_catch_tags(Type, {y,N}=Reg, Vst) ->
    %% Every catch or try/catch must use a lower Y register number than any
    %% enclosing catch or try/catch. That will ensure that when the stack is
    %% scanned when an exception occurs, the innermost try/catch tag is found
    %% first.
    case is_try_catch_tag(Type) of
        true ->
            case collect_try_catch_tags(N - 1, Vst, []) of
                [_|_]=Bad -> error({bad_try_catch_nesting, Reg, Bad});
                [] -> ok
            end;
        false ->
            ok
    end.

assert_term(Src, Vst) ->
    _ = get_term_type(Src, Vst),
    ok.

assert_movable(Src, Vst) ->
    _ = get_movable_term_type(Src, Vst),
    ok.

assert_literal(Src) ->
    case is_literal(Src) of
        true -> ok;
        false -> error({literal_required,Src})
    end.

assert_not_literal(Src) ->
    case is_literal(Src) of
        true -> error({literal_not_allowed,Src});
        false -> ok
    end.

is_literal(nil) -> true;
is_literal({atom,A}) when is_atom(A) -> true;
is_literal({float,F}) when is_float(F) -> true;
is_literal({integer,I}) when is_integer(I) -> true;
is_literal({literal,_L}) -> true;
is_literal(_) -> false.

value_to_literal([]) -> nil;
value_to_literal(A) when is_atom(A) -> {atom,A};
value_to_literal(F) when is_float(F) -> {float,F};
value_to_literal(I) when is_integer(I) -> {integer,I};
value_to_literal(Other) -> {literal,Other}.

%% These are just wrappers around their equivalents in beam_types, which
%% handle the validator-specific #t_abstract{} type.
%%
%% The funny-looking abstract types produced here are intended to provoke
%% errors on actual use; they do no harm just lying around.

normalize(#t_abstract{}=A) -> error({abstract_type, A});
normalize(T) -> beam_types:normalize(T).

join(Same, Same) -> Same;
join(#t_abstract{}=A, B) -> #t_abstract{kind={join, A, B}};
join(A, #t_abstract{}=B) -> #t_abstract{kind={join, A, B}};
join(A, B) -> beam_types:join(A, B).

meet(Same, Same) -> Same;
meet(#t_abstract{}=A, B) -> #t_abstract{kind={meet, A, B}};
meet(A, #t_abstract{}=B) -> #t_abstract{kind={meet, A, B}};
meet(A, B) -> beam_types:meet(A, B).

subtract(#t_abstract{}=A, B) -> #t_abstract{kind={subtract, A, B}};
subtract(A, #t_abstract{}=B) -> #t_abstract{kind={subtract, A, B}};
subtract(A, B) -> beam_types:subtract(A, B).

assert_type(RequiredType, Term, Vst) ->
    GivenType = get_movable_term_type(Term, Vst),
    case meet(RequiredType, GivenType) of
        GivenType ->
            ok;
        _RequiredType ->
            error({bad_type,{needed,RequiredType},{actual,GivenType}})
    end.

validate_src(Ss, Vst) when is_list(Ss) ->
    _ = [assert_term(S, Vst) || S <- Ss],
    ok.

%% get_term_type(Src, ValidatorState) -> Type
%%  Get the type of the source Src. The returned type Type will be
%%  a standard Erlang type (no catch/try tags or match contexts).

get_term_type(Src, Vst) ->
    case get_movable_term_type(Src, Vst) of
        #t_bs_context{} -> error({match_context,Src});
        #t_abstract{} -> error({abstract_term,Src});
        Type -> Type
    end.

%% get_movable_term_type(Src, ValidatorState) -> Type
%%  Get the type of the source Src. The returned type Type will be
%%  a standard Erlang type (no catch/try tags). Match contexts are OK.

get_movable_term_type(Src, Vst) ->
    case get_raw_type(Src, Vst) of
        #t_abstract{kind=unfinished_tuple=Kind} -> error({Kind,Src});
        initialized -> error({unassigned,Src});
        uninitialized -> error({uninitialized_reg,Src});
        {catchtag,_} -> error({catchtag,Src});
        {trytag,_} -> error({trytag,Src});
        Type -> Type
    end.

%% get_tag_type(Src, ValidatorState) -> Type
%%  Return the tag type of a Y register, erroring out if it contains a term.

get_tag_type({y,_}=Src, Vst) ->
    case get_raw_type(Src, Vst) of
        {catchtag, _}=Tag -> Tag;
        {trytag, _}=Tag -> Tag;
        uninitialized=Tag -> Tag;
        initialized=Tag -> Tag;
        Other -> error({invalid_tag,Src,Other})
    end;
get_tag_type(Src, _) ->
    error({invalid_tag_register,Src}).

%% get_raw_type(Src, ValidatorState) -> Type
%%  Return the type of a register without doing any validity checks or
%%  conversions.
get_raw_type({x,X}=Src, #vst{current=#st{xs=Xs}}=Vst) when is_integer(X) ->
    check_limit(Src),
    case Xs of
        #{ Src := #value_ref{}=Ref } -> get_raw_type(Ref, Vst);
        #{} -> uninitialized
    end;
get_raw_type({y,Y}=Src, #vst{current=#st{ys=Ys}}=Vst) when is_integer(Y) ->
    check_limit(Src),
    case Ys of
        #{ Src := #value_ref{}=Ref } -> get_raw_type(Ref, Vst);
        #{ Src := Tag } -> Tag;
        #{} -> uninitialized
    end;
get_raw_type(#value_ref{}=Ref, #vst{current=#st{vs=Vs}}) ->
    case Vs of
        #{ Ref := #value{type=Type} } -> Type;
        #{} -> none
    end;
get_raw_type(Src, #vst{current=#st{}}) ->
    get_literal_type(Src).

get_literal_type(nil) -> 
    beam_types:make_type_from_value([]);
get_literal_type({atom,A}) when is_atom(A) -> 
    beam_types:make_type_from_value(A);
get_literal_type({float,F}) when is_float(F) -> 
    beam_types:make_type_from_value(F);
get_literal_type({integer,I}) when is_integer(I) -> 
    beam_types:make_type_from_value(I);
get_literal_type({literal,L}) -> 
    beam_types:make_type_from_value(L);
get_literal_type(T) ->
    error({not_literal,T}).

%%%
%%% Branch tracking
%%%

%% Forks the execution flow, with the provided funs returning the new state of
%% their respective branch; the "fail" fun returns the state where the branch
%% is taken, and the "success" fun returns the state where it's not.
%%
%% If either path is known not to be taken at runtime (eg. due to a type
%% conflict), it will simply be discarded.
-spec branch(Lbl :: label(),
             Original :: #vst{},
             FailFun :: BranchFun,
             SuccFun :: BranchFun) -> #vst{} when
     BranchFun :: fun((#vst{}) -> #vst{}).
branch(Lbl, Vst0, FailFun, SuccFun) ->
    validate_branch(Lbl, Vst0),
    #vst{current=St0} = Vst0,

    try FailFun(Vst0) of
        Vst1 ->
            Vst2 = fork_state(Lbl, Vst1),
            Vst = Vst2#vst{current=St0},
            try SuccFun(Vst) of
                V -> V
            catch
                {type_conflict, _, _} ->
                    %% The instruction is guaranteed to fail; kill the state.
                    kill_state(Vst)
            end
    catch
        {type_conflict, _, _} ->
            %% This instruction is guaranteed not to fail, so we run the
            %% success branch *without* catching type conflicts to avoid hiding
            %% errors in the validator itself; one of the branches must
            %% succeed.
            SuccFun(Vst0)
    end.

validate_branch(Lbl, #vst{current=#st{ct=Tags}}) ->
    validate_branch_1(Lbl, Tags).

validate_branch_1(Lbl, [{trytag, FailLbls} | Tags]) ->
    %% 'try_case' assumes that an exception has been thrown, so a direct branch
    %% will crash the emulator.
    %%
    %% (Jumping to a 'catch_end' is fine however as it will simply nop in the
    %% absence of an exception.)
    case ordsets:is_element(Lbl, FailLbls) of
        true -> error({illegal_branch, try_handler, Lbl});
        false -> validate_branch_1(Lbl, Tags)
    end;
validate_branch_1(Lbl, [_ | Tags]) ->
    validate_branch_1(Lbl, Tags);
validate_branch_1(_Lbl, []) ->
    ok.

%% A shorthand version of branch/4 for when the state is only altered on
%% success.
branch(Fail, Vst, SuccFun) ->
    branch(Fail, Vst, fun(V) -> V end, SuccFun).

%% Directly branches off the state. This is an "internal" operation that should
%% be used sparingly.
fork_state(0, #vst{}=Vst) ->
    %% If the instruction fails, the stack may be scanned looking for a catch
    %% tag. Therefore the Y registers must be initialized at this point.
    verify_y_init(Vst),
    Vst;
fork_state(L, #vst{current=St,branched=B,ref_ctr=Counter0}=Vst) ->
    case gb_trees:is_defined(L, B) of
        true ->
            {MergedSt, Counter} = merge_states(L, St, B, Counter0),
            Branched = gb_trees:update(L, MergedSt, B),
            Vst#vst{branched=Branched,ref_ctr=Counter};
        false ->
            Vst#vst{branched=gb_trees:insert(L, St, B)}
    end.

%% merge_states/3 is used when there's more than one way to arrive at a
%% certain point, requiring the states to be merged down to the least
%% common subset for the subsequent code.

merge_states(L, St, Branched, Counter) when L =/= 0 ->
    case gb_trees:lookup(L, Branched) of
        {value, OtherSt} -> merge_states_1(St, OtherSt, Counter);
        none -> {St, Counter}
    end.

merge_states_1(St, none, Counter) ->
    {St, Counter};
merge_states_1(none, St, Counter) ->
    {St, Counter};
merge_states_1(StA, StB, Counter0) ->
    #st{xs=XsA,ys=YsA,vs=VsA,fragile=FragA,numy=NumYA,
        h=HA,ct=CtA,recv_marker=MarkerA} = StA,
    #st{xs=XsB,ys=YsB,vs=VsB,fragile=FragB,numy=NumYB,
        h=HB,ct=CtB,recv_marker=MarkerB} = StB,

    %% When merging registers we drop all registers that aren't defined in both
    %% states, and resolve conflicts by creating new values (similar to phi
    %% nodes in SSA).
    %%
    %% While doing this we build a "merge map" detailing which values need to
    %% be kept and which new values need to be created to resolve conflicts.
    %% This map is then used to create a new value database where the types of
    %% all values have been joined.
    {Xs, Merge0, Counter1} = merge_regs(XsA, XsB, #{}, Counter0),
    {Ys, Merge, Counter} = merge_regs(YsA, YsB, Merge0, Counter1),
    Vs = merge_values(Merge, VsA, VsB),

    Marker = merge_receive_marker(MarkerA, MarkerB),
    Fragile = merge_fragility(FragA, FragB),
    NumY = merge_stk(NumYA, NumYB),
    Ct = merge_ct(CtA, CtB),

    St = #st{xs=Xs,ys=Ys,vs=Vs,fragile=Fragile,numy=NumY,
             h=min(HA, HB),ct=Ct,recv_marker=Marker},
    {St, Counter}.

%% Merges the contents of two register maps, returning the updated "merge map"
%% and the new registers.
merge_regs(RsA, RsB, Merge, Counter) ->
    Keys = if
               map_size(RsA) =< map_size(RsB) -> maps:keys(RsA);
               map_size(RsA) > map_size(RsB) -> maps:keys(RsB)
           end,
    merge_regs_1(Keys, RsA, RsB, #{}, Merge, Counter).

merge_regs_1([Reg | Keys], RsA, RsB, Regs, Merge0, Counter0) ->
    case {RsA, RsB} of
        {#{ Reg := #value_ref{}=RefA }, #{ Reg := #value_ref{}=RefB }} ->
            {Ref, Merge, Counter} = merge_vrefs(RefA, RefB, Merge0, Counter0),
            merge_regs_1(Keys, RsA, RsB, Regs#{ Reg => Ref }, Merge, Counter);
        {#{ Reg := TagA }, #{ Reg := TagB }} ->
            %% Tags describe the state of the register rather than the value it
            %% contains, so if a register contains a tag in one state we have
            %% to merge it as a tag regardless of whether the other state says
            %% it's a value.
            {y, _} = Reg,                       %Assertion.
            merge_regs_1(Keys, RsA, RsB, Regs#{ Reg => merge_tags(TagA,TagB) },
                         Merge0, Counter0);
        {#{}, #{}} ->
            merge_regs_1(Keys, RsA, RsB, Regs, Merge0, Counter0)
    end;
merge_regs_1([], _, _, Regs, Merge, Counter) ->
    {Regs, Merge, Counter}.

merge_tags(Same, Same) ->
    Same;
merge_tags(uninitialized, _) ->
    uninitialized;
merge_tags(_, uninitialized) ->
    uninitialized;
merge_tags({trytag, LblsA}, {trytag, LblsB}) ->
    {trytag, ordsets:union(LblsA, LblsB)};
merge_tags({catchtag, LblsA}, {catchtag, LblsB}) ->
    {catchtag, ordsets:union(LblsA, LblsB)};
merge_tags(_A, _B) ->
    %% All other combinations leave the register initialized. Errors arising
    %% from this will be caught later on.
    initialized.

merge_vrefs(Ref, Ref, Merge, Counter) ->
    %% We have two (potentially) different versions of the same value, so we
    %% should join their types into the same value.
    {Ref, Merge#{ Ref => Ref }, Counter};
merge_vrefs(RefA, RefB, Merge, Counter) ->
    %% We have two different values, so we need to create a new value from
    %% their joined type if we haven't already done so.
    Key = {RefA, RefB},
    case Merge of
        #{ Key := Ref } ->
            {Ref, Merge, Counter};
        #{} ->
            Ref = #value_ref{id=Counter},
            {Ref, Merge#{ Key => Ref }, Counter + 1}
    end.

merge_values(Merge, VsA, VsB) ->
    maps:fold(fun(Spec, New, Acc) ->
                      mv_1(Spec, New, VsA, VsB, Acc)
              end, #{}, Merge).

mv_1(Same, Same, VsA, VsB, Acc0) ->
    %% We're merging different versions of the same value, so it's safe to
    %% reuse old entries if the type's unchanged.
    #value{type=TypeA,args=Args}=EntryA = map_get(Same, VsA),
    #value{type=TypeB,args=Args}=EntryB = map_get(Same, VsB),

    Entry = case join(TypeA, TypeB) of
                TypeA -> EntryA;
                TypeB -> EntryB;
                JoinedType -> EntryA#value{type=JoinedType}
            end,

    Acc = Acc0#{ Same => Entry },

    %% Type inference may depend on values that are no longer reachable from a
    %% register, so all arguments must be merged into the new state.
    mv_args(Args, VsA, VsB, Acc);
mv_1({RefA, RefB}, New, VsA, VsB, Acc) ->
    #value{type=TypeA} = map_get(RefA, VsA),
    #value{type=TypeB} = map_get(RefB, VsB),
    Acc#{ New => #value{op=join,args=[],type=join(TypeA, TypeB)} }.

mv_args([#value_ref{}=Arg | Args], VsA, VsB, Acc0) ->
    case Acc0 of
        #{ Arg := _ } ->
            mv_args(Args, VsA, VsB, Acc0);
        #{} ->
            Acc = mv_1(Arg, Arg, VsA, VsB, Acc0),
            mv_args(Args, VsA, VsB, Acc)
    end;
mv_args([_ | Args], VsA, VsB, Acc) ->
    mv_args(Args, VsA, VsB, Acc);
mv_args([], _VsA, _VsB, Acc) ->
    Acc.

merge_fragility(FragileA, FragileB) ->
    cerl_sets:union(FragileA, FragileB).

merge_receive_marker(Same, Same) ->
    Same;
merge_receive_marker(none, initialized) ->
    %% Committing a cleared receive marker is harmless, so it's okay to
    %% recv_set if we're clear on one path (e.g. leaving a catch block) and
    %% initialized on another (leaving the happy path).
    initialized;
merge_receive_marker(initialized, none) ->
    initialized;
merge_receive_marker(_, _) ->
    undecided.

merge_stk(S, S) -> S;
merge_stk(_, _) -> undecided.

merge_ct(S, S) -> S;
merge_ct(Ct0, Ct1) -> merge_ct_1(Ct0, Ct1).

merge_ct_1([], []) ->
    [];
merge_ct_1([{trytag, LblsA} | CtA], [{trytag, LblsB} | CtB]) ->
    [{trytag, ordsets:union(LblsA, LblsB)} | merge_ct_1(CtA, CtB)];
merge_ct_1([{catchtag, LblsA} | CtA], [{catchtag, LblsB} | CtB]) ->
    [{catchtag, ordsets:union(LblsA, LblsB)} | merge_ct_1(CtA, CtB)];
merge_ct_1(_, _) ->
    undecided.

verify_y_init(#vst{current=#st{numy=NumY,ys=Ys}}=Vst) when is_integer(NumY) ->
    HighestY = maps:fold(fun({y,Y}, _, Acc) -> max(Y, Acc) end, -1, Ys),
    true = NumY > HighestY,                     %Assertion.
    verify_y_init_1(NumY - 1, Vst),
    ok;
verify_y_init(#vst{current=#st{numy=undecided,ys=Ys}}=Vst) ->
    HighestY = maps:fold(fun({y,Y}, _, Acc) -> max(Y, Acc) end, -1, Ys),
    verify_y_init_1(HighestY, Vst);
verify_y_init(#vst{}) ->
    ok.

verify_y_init_1(-1, _Vst) ->
    ok;
verify_y_init_1(Y, Vst) ->
    Reg = {y, Y},
    assert_not_fragile(Reg, Vst),
    case get_raw_type(Reg, Vst) of
        uninitialized -> error({uninitialized_reg,Reg});
        _ -> verify_y_init_1(Y - 1, Vst)
    end.

verify_live(0, _Vst) ->
    ok;
verify_live(Live, Vst) when is_integer(Live), 0 < Live, Live =< 1023 ->
    verify_live_1(Live - 1, Vst);
verify_live(Live, _Vst) ->
    error({bad_number_of_live_regs,Live}).

verify_live_1(-1, _) ->
    ok;
verify_live_1(X, Vst) when is_integer(X) ->
    Reg = {x, X},
    case get_raw_type(Reg, Vst) of
        uninitialized -> error({Reg, not_live});
        _ -> verify_live_1(X - 1, Vst)
    end.

verify_no_ct(#vst{current=#st{numy=none}}) ->
    ok;
verify_no_ct(#vst{current=#st{numy=undecided}}) ->
    error(unknown_size_of_stackframe);
verify_no_ct(#vst{current=St}=Vst) ->
    case collect_try_catch_tags(St#st.numy - 1, Vst, []) of
        [_|_]=Bad -> error({unfinished_catch_try,Bad});
        [] -> ok
    end.

%% Collects all try/catch tags, walking down from the Nth stack position.
collect_try_catch_tags(-1, _Vst, Acc) ->
    Acc;
collect_try_catch_tags(Y, Vst, Acc0) ->
    Tag = get_raw_type({y, Y}, Vst),
    Acc = case is_try_catch_tag(Tag) of
              true -> [{{y, Y}, Tag} | Acc0];
              false -> Acc0
          end,
    collect_try_catch_tags(Y - 1, Vst, Acc).

is_try_catch_tag({catchtag,_}) -> true;
is_try_catch_tag({trytag,_}) -> true;
is_try_catch_tag(_) -> false.

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

%%%
%%% RECEIVE
%%%

%% When the compiler knows that a message it's matching in a receive can't
%% exist before a certain point (e.g. it matches a newly created ref), it can
%% emit a recv_mark/recv_set pair to tell the next loop_rec where to start
%% looking.
%%
%% Since this affects the next loop_rec instruction it's very important that we
%% properly exit the receive loop the mark is intended for, either through
%% timing out or matching a message. Should we return from the function or
%% enter a different receive loop, we risk skipping messages that should have
%% been matched.
set_receive_marker(New, #vst{current=#st{recv_marker=Current}=St0}=Vst) ->
    case {Current, New} of
        {none, initialized} ->
            %% ??? -> recv_mark
            ok;
        {initialized, committed} ->
            %% recv_mark -> recv_set
            ok;
        {none, committed} ->
            %% ??? -> recv_set
            %%
            %% The marker has likely been killed by a 'catch_end'. This could
            %% be an error but we'll ignore it for now.
            ok;
        {_, none} ->
            ok;
        {_, _} ->
            error({invalid_receive_marker_change, Current, New})
    end,
    St = St0#st{recv_marker=New},
    Vst#vst{current=St}.

%% The loop_rec/2 instruction may return a reference to a term that is not
%% part of the root set. That term or any part of it must not be included in a
%% garbage collection. Therefore, the term (or any part of it) must not be
%% passed to another function, placed in another term, or live in a Y register
%% over an instruction that may GC.
%%
%% Fragility is marked on a per-register (rather than per-value) basis.

%% Marks Reg as fragile.
mark_fragile(Reg, Vst) ->
    #vst{current=#st{fragile=Fragile0}=St0} = Vst,
    Fragile = cerl_sets:add_element(Reg, Fragile0),
    St = St0#st{fragile=Fragile},
    Vst#vst{current=St}.

propagate_fragility(Reg, Args, #vst{current=St0}=Vst) ->
    #st{fragile=Fragile0} = St0,

    Sources = cerl_sets:from_list(Args),
    Fragile = case cerl_sets:is_disjoint(Sources, Fragile0) of
                  true -> cerl_sets:del_element(Reg, Fragile0);
                  false -> cerl_sets:add_element(Reg, Fragile0)
              end,

    St = St0#st{fragile=Fragile},
    Vst#vst{current=St}.

%% Marks Reg as durable, must be used when assigning a newly created value to
%% a register.
remove_fragility(Reg, Vst) ->
    #vst{current=#st{fragile=Fragile0}=St0} = Vst,
    case cerl_sets:is_element(Reg, Fragile0) of
        true ->
            Fragile = cerl_sets:del_element(Reg, Fragile0),
            St = St0#st{fragile=Fragile},
            Vst#vst{current=St};
        false ->
            Vst
    end.

%% Marks all registers as durable.
remove_fragility(#vst{current=St0}=Vst) ->
    St = St0#st{fragile=cerl_sets:new()},
    Vst#vst{current=St}.

assert_durable_term(Src, Vst) ->
    assert_term(Src, Vst),
    assert_not_fragile(Src, Vst).

assert_not_fragile({Kind,_}=Src, Vst) when Kind =:= x; Kind =:= y ->
    check_limit(Src),
    #vst{current=#st{fragile=Fragile}} = Vst,
    case cerl_sets:is_element(Src, Fragile) of
        true -> error({fragile_message_reference, Src});
        false -> ok
    end;
assert_not_fragile(Lit, #vst{}) ->
    assert_literal(Lit),
    ok.

%%%
%%% Return/argument types of calls and BIFs
%%%

bif_types(Op, Ss, Vst) ->
    Args = [normalize(get_term_type(Arg, Vst)) || Arg <- Ss],
    beam_call_types:types(erlang, Op, Args).

call_types({extfunc,M,F,A}, A, Vst) ->
    Args = get_call_args(A, Vst),
    beam_call_types:types(M, F, Args);
call_types(_, A, Vst) ->
    {any, get_call_args(A, Vst), false}.

will_bif_succeed(fadd, [_,_], _Vst) ->
    maybe;
will_bif_succeed(fdiv, [_,_], _Vst) ->
    maybe;
will_bif_succeed(fmul, [_,_], _Vst) ->
    maybe;
will_bif_succeed(fnegate, [_], _Vst) ->
    maybe;
will_bif_succeed(fsub, [_,_], _Vst) ->
    maybe;
will_bif_succeed(Op, Ss, Vst) ->
    Args = [normalize(get_term_type(Arg, Vst)) || Arg <- Ss],
    beam_call_types:will_succeed(erlang, Op, Args).

will_call_succeed({extfunc,M,F,A}, Vst) ->
    beam_call_types:will_succeed(M, F, get_call_args(A, Vst));
will_call_succeed(_Call, _Vst) ->
    maybe.

get_call_args(Arity, Vst) ->
    get_call_args_1(0, Arity, Vst).

get_call_args_1(Arity, Arity, _) ->
    [];
get_call_args_1(N, Arity, Vst) when N < Arity ->
    ArgType = normalize(get_movable_term_type({x,N}, Vst)),
    [ArgType | get_call_args_1(N + 1, Arity, Vst)].

check_limit({x,X}=Src) when is_integer(X) ->
    if
        %% Note: x(1023) is reserved for use by the BEAM loader.
        0 =< X, X < 1023 -> ok;
        1023 =< X -> error(limit);
        X < 0 -> error({bad_register, Src})
    end;
check_limit({y,Y}=Src) when is_integer(Y) ->
    if
        0 =< Y, Y < 1024 -> ok;
        1024 =< Y -> error(limit);
        Y < 0 -> error({bad_register, Src})
    end;
check_limit({fr,Fr}=Src) when is_integer(Fr) ->
    if
        0 =< Fr, Fr < 1023 -> ok;
        1023 =< Fr -> error(limit);
        Fr < 0 -> error({bad_register, Src})
    end.

min(A, B) when is_integer(A), is_integer(B), A < B -> A;
min(A, B) when is_integer(A), is_integer(B) -> B.

gcd(A, B) ->
    case A rem B of
        0 -> B;
        X -> gcd(B, X)
    end.

gb_trees_from_list(L) -> gb_trees:from_orddict(sort(L)).

error(Error) -> throw(Error).
