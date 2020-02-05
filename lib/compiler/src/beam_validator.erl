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

%% Provides a stable interface for type annotations, used by certain passes to
%% indicate that we can safely assume that a register has a given type.
-spec type_anno(term()) -> term().
type_anno(atom) -> {atom,[]};
type_anno(bool) -> bool;
type_anno({binary,_}) -> binary;
type_anno(cons) -> cons;
type_anno(float) -> {float,[]};
type_anno(integer) -> {integer,[]};
type_anno(list) -> list;
type_anno(map) -> map;
type_anno(match_context) -> match_context;
type_anno(number) -> number;
type_anno(nil) -> nil.

-spec type_anno(term(), term()) -> term().
type_anno(atom, Value) when is_atom(Value) -> {atom, Value};
type_anno(float, Value) when is_float(Value)  -> {float, Value};
type_anno(integer, Value) when is_integer(Value) -> {integer, Value}.

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

-record(value_ref, {id :: index()}).
-record(value, {op :: term(), args :: [argument()], type :: type()}).

-type argument() :: #value_ref{} | literal().

-type index() :: non_neg_integer().

-type literal() :: {atom, [] | atom()} |
                   {float, [] | float()} |
                   {integer, [] | integer()} |
                   {literal, term()} |
                   nil.

-type tuple_sz() :: [non_neg_integer()] | %% Inexact
                    non_neg_integer(). %% Exact.

%% Match context type.
-record(ms,
        {id=make_ref() :: reference(),          %Unique ID.
         valid=0 :: non_neg_integer(),          %Valid slots
         slots=0 :: non_neg_integer()           %Number of slots
        }).

-type type() :: binary |
                cons |
                list |
                map |
                nil |
                #ms{} |
                ms_position |
                none |
                number |
                term |
                tuple_in_progress |
                {tuple, tuple_sz(), #{ literal() => type() }} |
                literal().

-type tag() :: initialized |
               uninitialized |
               {catchtag, [label()]} |
               {trytag, [label()]}.

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
         %Available heap size for floats.
         hf=0,
         %% Floating point state.
         fls=undefined,
         %% List of hot catch/try labels
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
         %% Argument information of other functions in the module
         ft=gb_trees:empty()       :: ft_tab(),
         %% Counter for #value_ref{} creation
         ref_ctr=0                 :: index()
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
    gb_trees:from_orddict(sort(Acc)).

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
    EntryOK = member(Entry, Ls2),
    if
	EntryOK ->
            Vst0 = init_vst(Arity, Ls1, Ls2, Ft),
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
    init_function_args(X - 1, create_term(term, argument, [], {x,X}, Vst)).

kill_heap_allocation(St) ->
    St#st{h=0,hf=0}.

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
valfun_1({label,Lbl}, #vst{current=St0,
                           ref_ctr=Counter0,
                           branched=B,
                           labels=Lbls}=Vst) ->
    {St, Counter} = merge_states(Lbl, St0, B, Counter0),
    Vst#vst{current=St,
            ref_ctr=Counter,
            branched=gb_trees:enter(Lbl, St, B),
            labels=gb_sets:add(Lbl, Lbls)};
valfun_1(_I, #vst{current=none}=Vst) ->
    %% Ignore instructions after erlang:error/1,2, which
    %% the original R10B compiler thought would return.
    Vst;
valfun_1({badmatch,Src}, Vst) ->
    assert_durable_term(Src, Vst),
    verify_y_init(Vst),
    kill_state(Vst);
valfun_1({case_end,Src}, Vst) ->
    assert_durable_term(Src, Vst),
    verify_y_init(Vst),
    kill_state(Vst);
valfun_1(if_end, Vst) ->
    verify_y_init(Vst),
    kill_state(Vst);
valfun_1({try_case_end,Src}, Vst) ->
    verify_y_init(Vst),
    assert_durable_term(Src, Vst),
    kill_state(Vst);
%% Instructions that cannot cause exceptions
valfun_1({bs_get_tail,Ctx,Dst,Live}, Vst0) ->
    bsm_validate_context(Ctx, Vst0),
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    Vst = prune_x_regs(Live, Vst0),
    extract_term(binary, bs_get_tail, [Ctx], Dst, Vst, Vst0);
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
    create_term({float,[]}, fmove, [], Dst, Vst);
valfun_1({kill,Reg}, Vst) ->
    create_tag(initialized, kill, [], Reg, Vst);
valfun_1({init,Reg}, Vst) ->
    create_tag(initialized, init, [], Reg, Vst);
valfun_1({test_heap,Heap,Live}, Vst) ->
    test_heap(Heap, Live, Vst);
valfun_1({bif,Op,{f,_},Ss,Dst}=I, Vst) ->
    case is_bif_safe(Op, length(Ss)) of
	false ->
	    %% Since the BIF can fail, make sure that any catch state
	    %% is updated.
	    valfun_2(I, Vst);
	true ->
	    %% It can't fail, so we finish handling it here (not updating
	    %% catch state).
	    validate_src(Ss, Vst),
	    Type = bif_return_type(Op, Ss, Vst),
	    extract_term(Type, {bif,Op}, Ss, Dst, Vst)
    end;
%% Put instructions.
valfun_1({put_list,A,B,Dst}, Vst0) ->
    assert_term(A, Vst0),
    assert_term(B, Vst0),
    Vst = eat_heap(2, Vst0),
    create_term(cons, put_list, [A, B], Dst, Vst);
valfun_1({put_tuple2,Dst,{list,Elements}}, Vst0) ->
    _ = [assert_term(El, Vst0) || El <- Elements],
    Size = length(Elements),
    Vst = eat_heap(Size+1, Vst0),
    {Es,_} = foldl(fun(Val, {Es0, Index}) ->
                       Type = get_term_type(Val, Vst0),
                       Es = set_element_type({integer,Index}, Type, Es0),
                       {Es, Index + 1}
                   end, {#{}, 1}, Elements),
    Type = {tuple,Size,Es},
    create_term(Type, put_tuple2, [], Dst, Vst);
valfun_1({put_tuple,Sz,Dst}, Vst0) when is_integer(Sz) ->
    Vst1 = eat_heap(1, Vst0),
    Vst = create_term(tuple_in_progress, put_tuple, [], Dst, Vst1),
    #vst{current=St0} = Vst,
    St = St0#st{puts_left={Sz,{Dst,Sz,#{}}}},
    Vst#vst{current=St};
valfun_1({put,Src}, Vst0) ->
    assert_term(Src, Vst0),
    Vst = eat_heap(1, Vst0),
    #vst{current=St0} = Vst,
    case St0 of
        #st{puts_left=none} ->
            error(not_building_a_tuple);
        #st{puts_left={1,{Dst,Sz,Es0}}} ->
            Es = Es0#{ {integer,Sz} => get_term_type(Src, Vst0) },
            St = St0#st{puts_left=none},
            create_term({tuple,Sz,Es}, put_tuple, [], Dst, Vst#vst{current=St});
        #st{puts_left={PutsLeft,{Dst,Sz,Es0}}} when is_integer(PutsLeft) ->
            Index = Sz - PutsLeft + 1,
            Es = Es0#{ {integer,Index} => get_term_type(Src, Vst0) },
            St = St0#st{puts_left={PutsLeft-1,{Dst,Sz,Es}}},
            Vst#vst{current=St}
    end;
%% Instructions for optimization of selective receives.
valfun_1({recv_mark,{f,Fail}}, Vst) when is_integer(Fail) ->
    set_receive_marker(initialized, Vst);
valfun_1({recv_set,{f,Fail}}, Vst) when is_integer(Fail) ->
    set_receive_marker(committed, Vst);
%% Misc.
valfun_1(remove_message, Vst0) ->
    Vst = set_receive_marker(none, Vst0),

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
valfun_1({'%', {remove_fragility, Reg}}, Vst) ->
    %% This is a hack to make prim_eval:'receive'/2 work.
    %%
    %% Normally it's illegal to pass fragile terms as a function argument as we
    %% have no way of knowing what the callee will do with it, but we know that
    %% prim_eval:'receive'/2 won't leak the term, nor cause a GC since it's
    %% disabled while matching messages.
    remove_fragility(Reg, Vst);
valfun_1({'%',_}, Vst) ->
    Vst;
valfun_1({line,_}, Vst) ->
    Vst;
%% Exception generating calls
valfun_1({call_ext,Live,Func}=I, Vst) ->
    case call_return_type(Func, Vst) of
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
    allocate(uninitialized, Stk, 0, Live, Vst);
valfun_1({allocate_heap,Stk,Heap,Live}, Vst) ->
    allocate(uninitialized, Stk, Heap, Live, Vst);
valfun_1({allocate_zero,Stk,Live}, Vst) ->
    allocate(initialized, Stk, 0, Live, Vst);
valfun_1({allocate_heap_zero,Stk,Heap,Live}, Vst) ->
    allocate(initialized, Stk, Heap, Live, Vst);
valfun_1({deallocate,StkSize}, #vst{current=#st{numy=StkSize}}=Vst) ->
    verify_no_ct(Vst),
    deallocate(Vst);
valfun_1({deallocate,_}, #vst{current=#st{numy=NumY}}) ->
    error({allocated,NumY});
valfun_1({trim,N,Remaining}, #vst{current=St0}=Vst) ->
    #st{numy=NumY} = St0,
    if
        N =< NumY, N+Remaining =:= NumY ->
            Vst#vst{current=trim_stack(N, 0, NumY, St0)};
        N > NumY; N+Remaining =/= NumY ->
            error({trim,N,Remaining,allocated,NumY})
    end;
%% Catch & try.
valfun_1({'catch',Dst,{f,Fail}}, Vst) when Fail =/= none ->
    init_try_catch_branch(catchtag, Dst, Fail, Vst);
valfun_1({'try',Dst,{f,Fail}}, Vst)  when Fail =/= none ->
    init_try_catch_branch(trytag, Dst, Fail, Vst);
valfun_1({catch_end,Reg}, #vst{current=#st{ct=[Fail|_]}}=Vst0) ->
    case get_tag_type(Reg, Vst0) of
        {catchtag,Fail} ->
            %% Kill the catch tag and receive marker.
            %%
            %% The marker is only cleared when an exception is thrown, but it's
            %% a bit too complicated to separate those cases at the moment.
            Vst1 = kill_catch_tag(Reg, Vst0),
            Vst = set_receive_marker(none, Vst1),

            %% {x,0} contains the caught term, if any.
            create_term(term, catch_end, [], {x,0}, Vst);
        Type ->
            error({wrong_tag_type,Type})
    end;
valfun_1({try_end,Reg}, #vst{current=#st{ct=[Fail|_]}}=Vst) ->
    case get_tag_type(Reg, Vst) of
        {trytag,Fail} ->
            %% Kill the catch tag. Note that x registers and the receive marker
            %% are unaffected.
            kill_catch_tag(Reg, Vst);
        Type ->
            error({wrong_tag_type,Type})
    end;
valfun_1({try_case,Reg}, #vst{current=#st{ct=[Fail|_]}}=Vst0) ->
    case get_tag_type(Reg, Vst0) of
        {trytag,Fail} ->
            %% Kill the catch tag, all x registers, and the receive marker.
            Vst1 = kill_catch_tag(Reg, Vst0),
            Vst2 = prune_x_regs(0, Vst1),
            Vst3 = set_receive_marker(none, Vst2),

            %% Class:Error:Stacktrace
            Vst4 = create_term({atom,[]}, try_case, [], {x,0}, Vst3),
            Vst = create_term(term, try_case, [], {x,1}, Vst4),
            create_term(term, try_case, [], {x,2}, Vst);
        Type ->
            error({wrong_tag_type,Type})
    end;
valfun_1({get_list,Src,D1,D2}, Vst0) ->
    assert_not_literal(Src),
    assert_type(cons, Src, Vst0),
    Vst = extract_term(term, get_hd, [Src], D1, Vst0),
    extract_term(term, get_tl, [Src], D2, Vst);
valfun_1({get_hd,Src,Dst}, Vst) ->
    assert_not_literal(Src),
    assert_type(cons, Src, Vst),
    extract_term(term, get_hd, [Src], Dst, Vst);
valfun_1({get_tl,Src,Dst}, Vst) ->
    assert_not_literal(Src),
    assert_type(cons, Src, Vst),
    extract_term(term, get_tl, [Src], Dst, Vst);
valfun_1({get_tuple_element,Src,N,Dst}, Vst) ->
    assert_not_literal(Src),
    assert_type({tuple_element,N+1}, Src, Vst),
    Index = {integer,N+1},
    Type = get_element_type(Index, Src, Vst),
    extract_term(Type, {bif,element}, [Index, Src], Dst, Vst);
valfun_1({jump,{f,Lbl}}, Vst) ->
    branch(Lbl, Vst,
           fun(SuccVst) ->
                   %% The next instruction is never executed.
                   kill_state(SuccVst)
           end);

valfun_1(return, Vst) ->
    assert_durable_term({x,0}, Vst),
    verify_return(Vst),
    kill_state(Vst);

valfun_1({set_tuple_element,Src,Tuple,N}, Vst) ->
    I = N + 1,
    assert_term(Src, Vst),
    assert_type({tuple_element,I}, Tuple, Vst),
    %% Manually update the tuple type; we can't rely on the ordinary update
    %% helpers as we must support overwriting (rather than just widening or
    %% narrowing) known elements, and we can't use extract_term either since
    %% the source tuple may be aliased.
    {tuple, Sz, Es0} = get_term_type(Tuple, Vst),
    Es = set_element_type({integer,I}, get_term_type(Src, Vst), Es0),
    override_type({tuple, Sz, Es}, Tuple, Vst);

%% Match instructions.
valfun_1({select_val,Src,{f,Fail},{list,Choices}}, Vst) ->
    assert_term(Src, Vst),
    assert_choices(Choices),
    validate_select_val(Fail, Choices, Src, Vst);
valfun_1({select_tuple_arity,Tuple,{f,Fail},{list,Choices}}, Vst) ->
    assert_type(tuple, Tuple, Vst),
    assert_arities(Choices),
    validate_select_tuple_arity(Fail, Choices, Tuple, Vst);

%% New bit syntax matching instructions.
valfun_1({test,bs_start_match3,{f,Fail},Live,[Src],Dst}, Vst) ->
    validate_bs_start_match(Fail, Live, bsm_match_state(), Src, Dst, Vst);
valfun_1({test,bs_start_match2,{f,Fail},Live,[Src,Slots],Dst}, Vst) ->
    validate_bs_start_match(Fail, Live, bsm_match_state(Slots), Src, Dst, Vst);
valfun_1({test,bs_match_string,{f,Fail},[Ctx,_,_]}, Vst) ->
    bsm_validate_context(Ctx, Vst),
    branch(Fail, Vst, fun(V) -> V end);
valfun_1({test,bs_skip_bits2,{f,Fail},[Ctx,Src,_,_]}, Vst) ->
    bsm_validate_context(Ctx, Vst),
    assert_term(Src, Vst),
    branch(Fail, Vst, fun(V) -> V end);
valfun_1({test,bs_test_tail2,{f,Fail},[Ctx,_]}, Vst) ->
    bsm_validate_context(Ctx, Vst),
    branch(Fail, Vst, fun(V) -> V end);
valfun_1({test,bs_test_unit,{f,Fail},[Ctx,_]}, Vst) ->
    bsm_validate_context(Ctx, Vst),
    branch(Fail, Vst, fun(V) -> V end);
valfun_1({test,bs_skip_utf8,{f,Fail},[Ctx,Live,_]}, Vst) ->
    validate_bs_skip_utf(Fail, Ctx, Live, Vst);
valfun_1({test,bs_skip_utf16,{f,Fail},[Ctx,Live,_]}, Vst) ->
    validate_bs_skip_utf(Fail, Ctx, Live, Vst);
valfun_1({test,bs_skip_utf32,{f,Fail},[Ctx,Live,_]}, Vst) ->
    validate_bs_skip_utf(Fail, Ctx, Live, Vst);
valfun_1({test,bs_get_integer2=Op,{f,Fail},Live,[Ctx,_,_,_],Dst}, Vst) ->
    validate_bs_get(Op, Fail, Ctx, Live, {integer, []}, Dst, Vst);
valfun_1({test,bs_get_float2=Op,{f,Fail},Live,[Ctx,_,_,_],Dst}, Vst) ->
    validate_bs_get(Op, Fail, Ctx, Live, {float, []}, Dst, Vst);
valfun_1({test,bs_get_binary2=Op,{f,Fail},Live,[Ctx,_,_,_],Dst}, Vst) ->
    validate_bs_get(Op, Fail, Ctx, Live, binary, Dst, Vst);
valfun_1({test,bs_get_utf8=Op,{f,Fail},Live,[Ctx,_],Dst}, Vst) ->
    validate_bs_get(Op, Fail, Ctx, Live, {integer, []}, Dst, Vst);
valfun_1({test,bs_get_utf16=Op,{f,Fail},Live,[Ctx,_],Dst}, Vst) ->
    validate_bs_get(Op, Fail, Ctx, Live, {integer, []}, Dst, Vst);
valfun_1({test,bs_get_utf32=Op,{f,Fail},Live,[Ctx,_],Dst}, Vst) ->
    validate_bs_get(Op, Fail, Ctx, Live, {integer, []}, Dst, Vst);
valfun_1({bs_save2,Ctx,SavePoint}, Vst) ->
    bsm_save(Ctx, SavePoint, Vst);
valfun_1({bs_restore2,Ctx,SavePoint}, Vst) ->
    bsm_restore(Ctx, SavePoint, Vst);
valfun_1({bs_get_position, Ctx, Dst, Live}, Vst0) ->
    bsm_validate_context(Ctx, Vst0),
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    Vst = prune_x_regs(Live, Vst0),
    create_term(ms_position, bs_get_position, [Ctx], Dst, Vst, Vst0);
valfun_1({bs_set_position, Ctx, Pos}, Vst) ->
    bsm_validate_context(Ctx, Vst),
    assert_type(ms_position, Pos, Vst),
    Vst;
valfun_1({test,has_map_fields,{f,Lbl},Src,{list,List}}, Vst) ->
    assert_type(map, Src, Vst),
    assert_unique_map_keys(List),
    branch(Lbl, Vst, fun(V) -> V end);
valfun_1({test,is_atom,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, {atom,[]}, Src, Vst);
valfun_1({test,is_binary,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, binary, Src, Vst);
valfun_1({test,is_bitstr,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, binary, Src, Vst);
valfun_1({test,is_boolean,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, bool, Src, Vst);
valfun_1({test,is_float,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, {float,[]}, Src, Vst);
valfun_1({test,is_tuple,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, {tuple,[0],#{}}, Src, Vst);
valfun_1({test,is_integer,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, {integer,[]}, Src, Vst);
valfun_1({test,is_nonempty_list,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, cons, Src, Vst);
valfun_1({test,is_number,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, number, Src, Vst);
valfun_1({test,is_list,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, list, Src, Vst);
valfun_1({test,is_map,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, map, Src, Vst);
valfun_1({test,is_nil,{f,Lbl},[Src]}, Vst) ->
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
valfun_1({test,test_arity,{f,Lbl},[Tuple,Sz]}, Vst) when is_integer(Sz) ->
    assert_type(tuple, Tuple, Vst),
    Type = {tuple, Sz, #{}},
    type_test(Lbl, Type, Tuple, Vst);
valfun_1({test,is_tagged_tuple,{f,Lbl},[Src,Sz,Atom]}, Vst) ->
    assert_term(Src, Vst),
    Type = {tuple, Sz, #{ {integer,1} => Atom }},
    type_test(Lbl, Type, Src, Vst);
valfun_1({test,is_eq_exact,{f,Lbl},[Src,Val]=Ss}, Vst) ->
    validate_src(Ss, Vst),
    branch(Lbl, Vst,
           fun(FailVst) ->
                   update_ne_types(Src, Val, FailVst)
           end,
           fun(SuccVst) ->
                   update_eq_types(Src, Val, SuccVst)
           end);
valfun_1({test,is_ne_exact,{f,Lbl},[Src,Val]=Ss}, Vst) ->
    validate_src(Ss, Vst),
    branch(Lbl, Vst,
           fun(FailVst) ->
                   update_eq_types(Src, Val, FailVst)
           end,
           fun(SuccVst) ->
                   update_ne_types(Src, Val, SuccVst)
           end);
valfun_1({test,_Op,{f,Lbl},Src}, Vst) ->
    %% is_pid, is_reference, et cetera.
    validate_src(Src, Vst),
    branch(Lbl, Vst, fun(V) -> V end);

%% Map instructions.
valfun_1({put_map_assoc=Op,{f,Fail},Src,Dst,Live,{list,List}}, Vst) ->
    verify_put_map(Op, Fail, Src, Dst, Live, List, Vst);
valfun_1({get_map_elements,{f,Fail},Src,{list,List}}, Vst) ->
    verify_get_map(Fail, Src, List, Vst);

valfun_1(I, Vst) ->
    valfun_2(I, Vst).

init_try_catch_branch(Tag, Dst, Fail, Vst0) ->
    Vst1 = create_tag({Tag,[Fail]}, 'try_catch', [], Dst, Vst0),
    #vst{current=#st{ct=Fails}=St0} = Vst1,
    St = St0#st{ct=[[Fail]|Fails]},
    Vst = Vst0#vst{current=St},

    branch(Fail, Vst,
           fun(CatchVst0) ->
                    #vst{current=#st{ys=Ys}} = CatchVst0,
                    CatchVst = maps:fold(fun init_catch_handler_1/3,
                                         CatchVst0, Ys),
                    %% The receive marker is cleared on exceptions.
                    set_receive_marker(none, CatchVst)
           end,
           fun(SuccVst) ->
                    %% All potentially-throwing instructions after this
                    %% one will implicitly branch to the fail label;
                    %% see valfun_2/2
                    SuccVst
           end).

%% Set the initial state at the try/catch label. Assume that Y registers
%% contain terms or try/catch tags.
init_catch_handler_1(Reg, initialized, Vst) ->
    create_term(term, 'catch_handler', [], Reg, Vst);
init_catch_handler_1(Reg, uninitialized, Vst) ->
    create_term(term, 'catch_handler', [], Reg, Vst);
init_catch_handler_1(_, _, Vst) ->
    Vst.

valfun_2(I, #vst{current=#st{ct=[[Fail]|_]}}=Vst) when is_integer(Fail) ->
    %% We have an active try/catch tag and we can jump there from this
    %% instruction, so we need to update the branched state of the try/catch
    %% handler.
    valfun_3(I, branch_state(Fail, Vst));
valfun_2(I, #vst{current=#st{ct=[]}}=Vst) ->
    valfun_3(I, Vst);
valfun_2(_, _) ->
    error(ambiguous_catch_try_state).

%% Handle the remaining floating point instructions here.
%% Floating point.
valfun_3({fconv,Src,{fr,_}=Dst}, Vst) ->
    assert_term(Src, Vst),

    %% An exception is raised on error, hence branching to 0.
    branch(0, Vst,
           fun(SuccVst0) ->
               SuccVst = update_type(fun meet/2, number, Src, SuccVst0),
               set_freg(Dst, SuccVst)
           end);
valfun_3({bif,fadd,_,[_,_]=Ss,Dst}, Vst) ->
    float_op(Ss, Dst, Vst);
valfun_3({bif,fdiv,_,[_,_]=Ss,Dst}, Vst) ->
    float_op(Ss, Dst, Vst);
valfun_3({bif,fmul,_,[_,_]=Ss,Dst}, Vst) ->
    float_op(Ss, Dst, Vst);
valfun_3({bif,fnegate,_,[_]=Ss,Dst}, Vst) ->
    float_op(Ss, Dst, Vst);
valfun_3({bif,fsub,_,[_,_]=Ss,Dst}, Vst) ->
    float_op(Ss, Dst, Vst);
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
valfun_4({put_map_exact=Op,{f,Fail},Src,Dst,Live,{list,List}}, Vst) ->
    verify_put_map(Op, Fail, Src, Dst, Live, List, Vst);
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
valfun_4({bif,element,{f,Fail},[Pos,Src],Dst}, Vst) ->
    branch(Fail, Vst,
           fun(SuccVst0) ->
                   PosType = get_term_type(Pos, SuccVst0),
                   TupleType = {tuple,[get_tuple_size(PosType)],#{}},

                   SuccVst1 = update_type(fun meet/2, TupleType,
                                         Src, SuccVst0),
                   SuccVst = update_type(fun meet/2, {integer,[]},
                                         Pos, SuccVst1),

                   ElementType = get_element_type(PosType, Src, SuccVst),
                   extract_term(ElementType, {bif,element}, [Pos,Src],
                                Dst, SuccVst)
           end);
valfun_4({bif,raise,{f,0},Src,_Dst}, Vst) ->
    validate_src(Src, Vst),
    kill_state(Vst);
valfun_4(raw_raise=I, Vst) ->
    call(I, 3, Vst);
valfun_4({bif,Op,{f,Fail},[Src]=Ss,Dst}, Vst) when Op =:= hd; Op =:= tl ->
    assert_term(Src, Vst),
    branch(Fail, Vst,
           fun(FailVst) ->
                   update_type(fun subtract/2, cons, Src, FailVst)
           end,
           fun(SuccVst0) ->
                   SuccVst = update_type(fun meet/2, cons, Src, SuccVst0),
                   extract_term(term, {bif,Op}, Ss, Dst, SuccVst)
           end);
valfun_4({bif,Op,{f,Fail},Ss,Dst}, Vst) ->
    validate_src(Ss, Vst),
    branch(Fail, Vst,
           fun(SuccVst0) ->
                   %% Infer argument types. Note that we can't subtract
                   %% types as the BIF could fail for reasons other than
                   %% bad argument types.
                   ArgTypes = bif_arg_types(Op, Ss),
                   SuccVst = foldl(fun({Arg, T}, V) ->
                                           update_type(fun meet/2, T, Arg, V)
                                   end, SuccVst0, zip(Ss, ArgTypes)),
                   Type = bif_return_type(Op, Ss, SuccVst),
                   extract_term(Type, {bif,Op}, Ss, Dst, SuccVst)
           end);
valfun_4({gc_bif,Op,{f,Fail},Live,Ss,Dst}, #vst{current=St0}=Vst0) ->
    validate_src(Ss, Vst0),
    verify_live(Live, Vst0),
    verify_y_init(Vst0),

    %% Heap allocations and X registers are killed regardless of whether we
    %% fail or not, as we may fail after GC.
    St = kill_heap_allocation(St0),
    Vst = prune_x_regs(Live, Vst0#vst{current=St}),

    branch(Fail, Vst,
           fun(SuccVst0) ->
                   ArgTypes = bif_arg_types(Op, Ss),
                   SuccVst = foldl(fun({Arg, T}, V) ->
                                           update_type(fun meet/2, T, Arg, V)
                                   end, SuccVst0, zip(Ss, ArgTypes)),

                   Type = bif_return_type(Op, Ss, SuccVst),

                   %% We're passing Vst0 as the original because the
                   %% registers were pruned before the branch.
                   extract_term(Type, {gc_bif,Op}, Ss, Dst, SuccVst, Vst0)
           end);
valfun_4({loop_rec,{f,Fail},Dst}, Vst) ->
    %% This term may not be part of the root set until remove_message/0 is
    %% executed. If control transfers to the loop_rec_end/1 instruction, no
    %% part of this term must be stored in a Y register.
    branch(Fail, Vst,
           fun(SuccVst0) ->
                   {Ref, SuccVst} = new_value(term, loop_rec, [], SuccVst0),
                   mark_fragile(Dst, set_reg_vref(Ref, Dst, SuccVst))
           end);
valfun_4({wait,_}, Vst) ->
    verify_y_init(Vst),
    kill_state(Vst);
valfun_4({wait_timeout,_,Src}, Vst) ->
    %% Note that the receive marker is not cleared since we may re-enter the
    %% loop while waiting. If we time out we'll be transferred to a timeout
    %% instruction that clears the marker.
    assert_term(Src, Vst),
    verify_y_init(Vst),
    prune_x_regs(0, Vst);
valfun_4({loop_rec_end,_}, Vst) ->
    verify_y_init(Vst),
    kill_state(Vst);
valfun_4(timeout, Vst0) ->
    Vst = set_receive_marker(none, Vst0),
    prune_x_regs(0, Vst);
valfun_4(send, Vst) ->
    call(send, 2, Vst);

%% Other test instructions.
valfun_4({bs_add,{f,Fail},[A,B,_],Dst}, Vst) ->
    assert_term(A, Vst),
    assert_term(B, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   create_term({integer,[]}, bs_add, [A, B], Dst, SuccVst)
           end);
valfun_4({bs_utf8_size,{f,Fail},A,Dst}, Vst) ->
    assert_term(A, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   create_term({integer,[]}, bs_utf8_size, [A], Dst, SuccVst)
           end);
valfun_4({bs_utf16_size,{f,Fail},A,Dst}, Vst) ->
    assert_term(A, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   create_term({integer,[]}, bs_utf16_size, [A], Dst, SuccVst)
           end);
valfun_4({bs_init2,{f,Fail},Sz,Heap,Live,_,Dst}, Vst0) ->
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
                   create_term(binary, bs_init2, [], Dst, SuccVst, SuccVst0)
           end);
valfun_4({bs_init_bits,{f,Fail},Sz,Heap,Live,_,Dst}, Vst0) ->
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
                   create_term(binary, bs_init_bits, [], Dst, SuccVst)
           end);
valfun_4({bs_append,{f,Fail},Bits,Heap,Live,_Unit,Bin,_Flags,Dst}, Vst0) ->
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    assert_term(Bits, Vst0),
    assert_term(Bin, Vst0),
    Vst = heap_alloc(Heap, Vst0),
    branch(Fail, Vst,
           fun(SuccVst0) ->
                   SuccVst = prune_x_regs(Live, SuccVst0),
                   create_term(binary, bs_append, [Bin], Dst, SuccVst, SuccVst0)
           end);
valfun_4({bs_private_append,{f,Fail},Bits,_Unit,Bin,_Flags,Dst}, Vst) ->
    assert_term(Bits, Vst),
    assert_term(Bin, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   create_term(binary, bs_private_append, [Bin], Dst, SuccVst)
           end);
valfun_4({bs_put_string,Sz,_}, Vst) when is_integer(Sz) ->
    Vst;
valfun_4({bs_put_binary,{f,Fail},Sz,_,_,Src}, Vst) ->
    assert_term(Sz, Vst),
    assert_term(Src, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   update_type(fun meet/2, binary, Src, SuccVst)
           end);
valfun_4({bs_put_float,{f,Fail},Sz,_,_,Src}, Vst) ->
    assert_term(Sz, Vst),
    assert_term(Src, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   update_type(fun meet/2, {float,[]}, Src, SuccVst)
           end);
valfun_4({bs_put_integer,{f,Fail},Sz,_,_,Src}, Vst) ->
    assert_term(Sz, Vst),
    assert_term(Src, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   update_type(fun meet/2, {integer,[]}, Src, SuccVst)
           end);
valfun_4({bs_put_utf8,{f,Fail},_,Src}, Vst) ->
    assert_term(Src, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   update_type(fun meet/2, {integer,[]}, Src, SuccVst)
           end);
valfun_4({bs_put_utf16,{f,Fail},_,Src}, Vst) ->
    assert_term(Src, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   update_type(fun meet/2, {integer,[]}, Src, SuccVst)
           end);
valfun_4({bs_put_utf32,{f,Fail},_,Src}, Vst) ->
    assert_term(Src, Vst),
    branch(Fail, Vst,
           fun(SuccVst) ->
                   update_type(fun meet/2, {integer,[]}, Src, SuccVst)
           end);
valfun_4(_, _) ->
    error(unknown_instruction).

verify_get_map(Fail, Src, List, Vst0) ->
    assert_not_literal(Src),                    %OTP 22.
    assert_type(map, Src, Vst0),

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
            Vst = extract_term(term, {bif,map_get}, [Key, Map], Dst, Vst0),
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

extract_map_vals([Key,Dst|Vs], Map, Vst0, Vsti0) ->
    assert_term(Key, Vst0),
    Vsti = extract_term(term, {bif,map_get}, [Key, Map], Dst, Vsti0),
    extract_map_vals(Vs, Map, Vst0, Vsti);
extract_map_vals([], _Map, _Vst0, Vst) ->
    Vst.

verify_put_map(Op, Fail, Src, Dst, Live, List, Vst0) ->
    assert_type(map, Src, Vst0),
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    _ = [assert_term(Term, Vst0) || Term <- List],
    Vst = heap_alloc(0, Vst0),

    branch(Fail, Vst,
           fun(SuccVst0) ->
                   SuccVst = prune_x_regs(Live, SuccVst0),
                   Keys = extract_map_keys(List),
                   assert_unique_map_keys(Keys),
                   create_term(map, Op, [Src], Dst, SuccVst, SuccVst0)
           end).


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
    ok.

%%
%% Common code for validating bs_start_match* instructions.
%%

validate_bs_start_match(Fail, Live, Type, Src, Dst, Vst) ->
    verify_live(Live, Vst),
    verify_y_init(Vst),

    %% #ms{} can represent either a match context or a term, so we have to mark
    %% the source as a term if it fails with a match context as an input. This
    %% hack is only needed until we get proper union types.
    branch(Fail, Vst,
           fun(FailVst) ->
                   case get_movable_term_type(Src, FailVst) of
                       #ms{} -> override_type(term, Src, FailVst);
                       _ -> FailVst
                   end
           end,
           fun(SuccVst0) ->
                   SuccVst1 = update_type(fun meet/2, binary,
                                          Src, SuccVst0),
                   SuccVst = prune_x_regs(Live, SuccVst1),
                   extract_term(Type, bs_start_match, [Src], Dst,
                                SuccVst, SuccVst0)
           end).

%%
%% Common code for validating bs_get* instructions.
%%
validate_bs_get(Op, Fail, Ctx, Live, Type, Dst, Vst) ->
    bsm_validate_context(Ctx, Vst),
    verify_live(Live, Vst),
    verify_y_init(Vst),

    branch(Fail, Vst,
           fun(SuccVst0) ->
                   SuccVst = prune_x_regs(Live, SuccVst0),
                   extract_term(Type, Op, [Ctx], Dst, SuccVst, SuccVst0)
           end).

%%
%% Common code for validating bs_skip_utf* instructions.
%%
validate_bs_skip_utf(Fail, Ctx, Live, Vst) ->
    bsm_validate_context(Ctx, Vst),
    verify_y_init(Vst),
    verify_live(Live, Vst),

    branch(Fail, Vst,
           fun(SuccVst) ->
                   prune_x_regs(Live, SuccVst)
           end).

%%
%% Common code for is_$type instructions.
%%
type_test(Fail, Type, Reg, Vst) ->
    assert_term(Reg, Vst),
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
call(Name, Live, #vst{current=St0}=Vst0) ->
    verify_call_args(Name, Live, Vst0),
    verify_y_init(Vst0),
    case call_return_type(Name, Vst0) of
        Type when Type =/= exception ->
            %% Type is never 'exception' because it has been handled earlier.
            St = St0#st{f=init_fregs()},
            Vst = prune_x_regs(0, Vst0#vst{current=St}),
            create_term(Type, call, [], {x,0}, Vst)
    end.

%% Tail call.
%%  The stackframe must have a known size and be initialized.
%%  Does not return to the instruction following the call.
tail_call(Name, Live, Vst0) ->
    verify_y_init(Vst0),
    Vst = deallocate(Vst0),
    verify_call_args(Name, Live, Vst),
    case call_return_type(Name, Vst0) of
        exception -> verify_no_ct(Vst);
        _ -> verify_return(Vst)
    end,
    kill_state(Vst).

verify_call_args(_, 0, #vst{}) ->
    ok;
verify_call_args({f,Lbl}, Live, Vst) when is_integer(Live)->
    verify_local_args(Live - 1, Lbl, #{}, Vst);
verify_call_args(_, Live, Vst) when is_integer(Live)->
    verify_remote_args_1(Live - 1, Vst);
verify_call_args(_, Live, _) ->
    error({bad_number_of_live_regs,Live}).

verify_remote_args_1(-1, _) ->
    ok;
verify_remote_args_1(X, Vst) ->
    assert_durable_term({x, X}, Vst),
    verify_remote_args_1(X - 1, Vst).

verify_local_args(-1, _Lbl, _CtxIds, _Vst) ->
    ok;
verify_local_args(X, Lbl, CtxIds, Vst) ->
    Reg = {x, X},
    assert_not_fragile(Reg, Vst),
    case get_movable_term_type(Reg, Vst) of
        #ms{id=Id}=Type ->
            case CtxIds of
                #{ Id := Other } ->
                    error({multiple_match_contexts, [Reg, Other]});
                #{} ->
                    verify_arg_type(Lbl, Reg, Type, Vst),
                    verify_local_args(X - 1, Lbl, CtxIds#{ Id => Reg }, Vst)
            end;
        Type ->
            verify_arg_type(Lbl, Reg, Type, Vst),
            verify_local_args(X - 1, Lbl, CtxIds, Vst)
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
        {value, #ms{}} ->
            %% Functions that accept match contexts also accept all other
            %% terms. This will change once we support union types.
            ok;
        {value, RequiredType} ->
            case vat_1(GivenType, RequiredType) of
                true -> ok;
                false -> error({bad_arg_type, Reg, GivenType, RequiredType})
            end;
        none ->
            ok
    end.

%% Checks whether the Given argument is compatible with the Required one. This
%% is essentially a relaxed version of 'meet(Given, Req) =:= Given', where we
%% accept that the Given value has the right type but not necessarily the exact
%% same value; if {atom,gurka} is required, we'll consider {atom,[]} valid.
%%
%% This will catch all problems that could crash the emulator, like passing a
%% 1-tuple when the callee expects a 3-tuple, but some value errors might slip
%% through.
vat_1(Same, Same) -> true;
vat_1({atom,A}, {atom,B}) -> A =:= B orelse is_list(A) orelse is_list(B);
vat_1({atom,A}, bool) -> is_boolean(A) orelse is_list(A);
vat_1(bool, {atom,B}) -> is_boolean(B) orelse is_list(B);
vat_1(cons, list) -> true;
vat_1({float,A}, {float,B}) -> A =:= B orelse is_list(A) orelse is_list(B);
vat_1({float,_}, number) -> true;
vat_1({integer,A}, {integer,B}) -> A =:= B orelse is_list(A) orelse is_list(B);
vat_1({integer,_}, number) -> true;
vat_1(_, {literal,_}) -> false;
vat_1({literal,_}=Lit, Required) -> vat_1(get_literal_type(Lit), Required);
vat_1(nil, list) -> true;
vat_1({tuple,SzA,EsA}, {tuple,SzB,EsB}) ->
    if
        is_list(SzB) ->
            tuple_sz(SzA) >= tuple_sz(SzB) andalso vat_elements(EsA, EsB);
        SzA =:= SzB ->
            vat_elements(EsA, EsB);
        SzA =/= SzB ->
            false
    end;
vat_1(_, _) -> false.

vat_elements(EsA, EsB) ->
    maps:fold(fun(Key, Req, Acc) ->
                      case EsA of
                          #{ Key := Given } -> Acc andalso vat_1(Given, Req);
                          #{} -> false
                      end
              end, true, EsB).

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

bsm_get_context({Kind,_}=Reg, Vst) when Kind =:= x; Kind =:= y->
    case get_movable_term_type(Reg, Vst) of
        #ms{}=Ctx -> Ctx;
        _ -> error({no_bsm_context,Reg})
    end;
bsm_get_context(Reg, _) ->
    error({bad_source,Reg}).

bsm_save(Reg, {atom,start}, Vst) ->
    %% Save point refering to where the match started.
    %% It is always valid. But don't forget to validate the context register.
    bsm_validate_context(Reg, Vst),
    Vst;
bsm_save(Reg, SavePoint, Vst) ->
    case bsm_get_context(Reg, Vst) of
	#ms{valid=Bits,slots=Slots}=Ctxt0 when SavePoint < Slots ->
	    Ctx = Ctxt0#ms{valid=Bits bor (1 bsl SavePoint),slots=Slots},
	    override_type(Ctx, Reg, Vst);
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
validate_select_val(Fail, [], _, Vst) ->
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
    Type = {tuple, Arity, #{}},
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

infer_types({Kind,_}=Reg, Vst) when Kind =:= x; Kind =:= y ->
    infer_types(get_reg_vref(Reg, Vst), Vst);
infer_types(#value_ref{}=Ref, #vst{current=#st{vs=Vs}}) ->
    case Vs of
        #{ Ref := Entry } -> infer_types_1(Entry);
        #{} -> fun(_, S) -> S end
    end;
infer_types(_, #vst{}) ->
    fun(_, S) -> S end.

infer_types_1(#value{op={bif,'=:='},args=[LHS,RHS]}) ->
    fun({atom,true}, S) ->
            %% Either side might contain something worth inferring, so we need
            %% to check them both.
            Infer_L = infer_types(RHS, S),
            Infer_R = infer_types(LHS, S),
            Infer_R(RHS, Infer_L(LHS, S));
       (_, S) -> S
    end;
infer_types_1(#value{op={bif,element},args=[{integer,Index}=Key,Tuple]}) ->
    fun(Val, S) ->
            Type = {tuple,[Index], #{ Key => get_term_type(Val, S) }},
            update_type(fun meet/2, Type, Tuple, S)
    end;
infer_types_1(#value{op={bif,is_atom},args=[Src]}) ->
    infer_type_test_bif({atom,[]}, Src);
infer_types_1(#value{op={bif,is_boolean},args=[Src]}) ->
    infer_type_test_bif(bool, Src);
infer_types_1(#value{op={bif,is_binary},args=[Src]}) ->
    infer_type_test_bif(binary, Src);
infer_types_1(#value{op={bif,is_bitstring},args=[Src]}) ->
    infer_type_test_bif(binary, Src);
infer_types_1(#value{op={bif,is_float},args=[Src]}) ->
    infer_type_test_bif(float, Src);
infer_types_1(#value{op={bif,is_integer},args=[Src]}) ->
    infer_type_test_bif({integer,{}}, Src);
infer_types_1(#value{op={bif,is_list},args=[Src]}) ->
    infer_type_test_bif(list, Src);
infer_types_1(#value{op={bif,is_map},args=[Src]}) ->
    infer_type_test_bif(map, Src);
infer_types_1(#value{op={bif,is_number},args=[Src]}) ->
    infer_type_test_bif(number, Src);
infer_types_1(#value{op={bif,is_tuple},args=[Src]}) ->
    infer_type_test_bif({tuple,[0],#{}}, Src);
infer_types_1(#value{op={bif,tuple_size}, args=[Tuple]}) ->
    fun({integer,Arity}, S) ->
            update_type(fun meet/2, {tuple,Arity,#{}}, Tuple, S);
       (_, S) -> S
    end;
infer_types_1(_) ->
    fun(_, S) -> S end.

infer_type_test_bif(Type, Src) ->
    fun({atom,true}, S) ->
            update_type(fun meet/2, Type, Src, S);
       (_, S) ->
            S
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
    assert_literal(Literal),
    %% Literals always retain their type, but we still need to bail on type
    %% conflicts.
    case Merge(Literal, With) of
        none -> throw({type_conflict, Literal, With});
        _Type -> Vst
    end.

update_ne_types(LHS, RHS, Vst) ->
    %% While updating types on equality is fairly straightforward, inequality
    %% is a bit trickier since all we know is that the *value* of LHS differs
    %% from RHS, so we can't blindly subtract their types.
    %%
    %% Consider `number =/= {integer,[]}`; all we know is that LHS isn't equal
    %% to some *specific integer* of unknown value, and if we were to subtract
    %% {integer,[]} we would erroneously infer that the new type is {float,[]}.
    %%
    %% Therefore, we only subtract when we know that RHS has a specific value.
    RType = get_term_type(RHS, Vst),
    case is_literal(RType) of
        true -> update_type(fun subtract/2, RType, LHS, Vst);
        false -> Vst
    end.

update_eq_types(LHS, RHS, Vst0) ->
    %% Either side might contain something worth inferring, so we need
    %% to check them both.
    Infer_L = infer_types(RHS, Vst0),
    Infer_R = infer_types(LHS, Vst0),
    Vst1 = Infer_R(RHS, Infer_L(LHS, Vst0)),

    T1 = get_term_type(LHS, Vst1),
    T2 = get_term_type(RHS, Vst1),

    Vst = update_type(fun meet/2, T2, LHS, Vst1),
    update_type(fun meet/2, T1, RHS, Vst).

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
    case Vs0 of
        #{ Ref := #value{}=Entry } ->
            Vs = Vs0#{ Ref => Entry#value{type=Type} },
            Vst#vst{current=St#st{vs=Vs}};
        #{} ->
            %% Dead references may happen during type inference and are not an
            %% error in and of themselves. If a problem were to arise from this
            %% it'll explode elsewhere.
            Vst
    end.

new_value(Type, Op, Ss, #vst{current=#st{vs=Vs0}=St,ref_ctr=Counter}=Vst) ->
    Ref = #value_ref{id=Counter},
    Vs = Vs0#{ Ref => #value{op=Op,args=Ss,type=Type} },

    {Ref, Vst#vst{current=St#st{vs=Vs},ref_ctr=Counter+1}}.

kill_catch_tag(Reg, #vst{current=#st{ct=[Fail|Fails]}=St}=Vst0) ->
    Vst = Vst0#vst{current=St#st{ct=Fails,fls=undefined}},
    {_, Fail} = get_tag_type(Reg, Vst),         %Assertion.
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
%% exception            Can only be used as a type returned by
%%                      call_return_type/2 (which gives the type of the value
%%                      returned by a call). Thus 'exception' is never stored
%%                      as type descriptor for a register.
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

%% join(Type1, Type2) -> Type
%%  Return the most specific type possible.
join(Same, Same) ->
    Same;
join(none, Other) ->
    Other;
join(Other, none) ->
    Other;
join({literal,_}=T1, T2) ->
    join_literal(T1, T2);
join(T1, {literal,_}=T2) ->
    join_literal(T2, T1);
join({tuple,Size,EsA}, {tuple,Size,EsB}) ->
    Es = join_tuple_elements(tuple_sz(Size), EsA, EsB),
    {tuple, Size, Es};
join({tuple,A,EsA}, {tuple,B,EsB}) ->
    Size = min(tuple_sz(A), tuple_sz(B)),
    Es = join_tuple_elements(Size, EsA, EsB),
    {tuple, [Size], Es};
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
join({integer,_}, {float,_}) ->
    number;
join({float,_}, {integer,_}) ->
    number;
join(bool, {atom,A}) ->
    join_bool(A);
join({atom,A}, bool) ->
    join_bool(A);
join({atom,A}, {atom,B}) when is_boolean(A), is_boolean(B) ->
    bool;
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

join_tuple_elements(Limit, EsA, EsB) ->
    Es0 = join_elements(EsA, EsB),
    maps:filter(fun({integer,Index}, _Type) -> Index =< Limit end, Es0).

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

%% meet(Type1, Type2) -> Type
%%  Return the meet of two types. The meet is a more specific type.
%%  It will be 'none' if the types are in conflict.

meet(Same, Same) ->
    Same;
meet(term, Other) ->
    Other;
meet(Other, term) ->
    Other;
meet(#ms{}, binary) ->
    #ms{};
meet(binary, #ms{}) ->
    #ms{};
meet({literal,_}, {literal,_}) ->
    none;
meet(T1, {literal,_}=T2) ->
    meet(T2, T1);
meet({literal,_}=T1, T2) ->
    case meet(get_literal_type(T1), T2) of
        none -> none;
        _ -> T1
    end;
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
                    Sz = erlang:max(Sz1, Sz2),
                    assert_tuple_elements(Sz, Es),
                    {tuple,[Sz],Es};
                {Sz1,[Sz2],_} when Sz2 =< Sz1 ->
                    assert_tuple_elements(Sz1, Es),
                    {tuple,Sz1,Es};
                {Sz,Sz,_} ->
                    assert_tuple_elements(Sz, Es),
                    {tuple,Sz,Es};
                {_,_,_} ->
                    none
            end;
        {_,_} -> none
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

%% No tuple elements may have an index above the known size.
assert_tuple_elements(Limit, Es) ->
    true = maps:fold(fun({integer,Index}, _T, true) ->
                             Index =< Limit
                     end, true, Es).            %Assertion.

%% subtract(Type1, Type2) -> Type
%%  Subtract Type2 from Type2. Example:
%%      subtract(list, nil) -> cons

subtract(Same, Same) -> none;
subtract(list, nil) -> cons;
subtract(list, cons) -> nil;
subtract(number, {integer,[]}) -> {float,[]};
subtract(number, {float,[]}) -> {integer,[]};
subtract(bool, {atom,false}) -> {atom, true};
subtract(bool, {atom,true}) -> {atom, false};
subtract(Type, _) -> Type.

assert_type(WantedType, Term, Vst) ->
    Type = get_term_type(Term, Vst),
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
    get_element_type_1(Key, get_term_type(Src, Vst)).

get_element_type_1({integer,_}=Key, {tuple,_Sz,Es}) ->
    case Es of
        #{ Key := Type } -> Type;
        #{} -> term
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
    _ = [assert_term(S, Vst) || S <- Ss],
    ok.

%% get_term_type(Src, ValidatorState) -> Type
%%  Get the type of the source Src. The returned type Type will be
%%  a standard Erlang type (no catch/try tags or match contexts).

get_term_type(Src, Vst) ->
    case get_movable_term_type(Src, Vst) of
        #ms{} -> error({match_context,Src});
        Type -> Type
    end.

%% get_movable_term_type(Src, ValidatorState) -> Type
%%  Get the type of the source Src. The returned type Type will be
%%  a standard Erlang type (no catch/try tags). Match contexts are OK.

get_movable_term_type(Src, Vst) ->
    case get_raw_type(Src, Vst) of
        initialized -> error({unassigned,Src});
        uninitialized -> error({uninitialized_reg,Src});
        {catchtag,_} -> error({catchtag,Src});
        {trytag,_} -> error({trytag,Src});
        tuple_in_progress -> error({tuple_in_progress,Src});
        {literal,_}=Lit -> get_literal_type(Lit);
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
get_raw_type(Src, #vst{}) ->
    get_literal_type(Src).

get_literal_type(nil=T) -> T;
get_literal_type({atom,A}=T) when is_atom(A) -> T;
get_literal_type({float,F}=T) when is_float(F) -> T;
get_literal_type({integer,I}=T) when is_integer(I) -> T;
get_literal_type({literal,[_|_]}) -> cons;
get_literal_type({literal,Bitstring}) when is_bitstring(Bitstring) -> binary;
get_literal_type({literal,Map}) when is_map(Map) -> map;
get_literal_type({literal,Tuple}) when is_tuple(Tuple) -> glt_1(Tuple);
get_literal_type({literal,_}) -> term;
get_literal_type(T) -> error({not_literal,T}).

glt_1([]) -> nil;
glt_1(A) when is_atom(A) -> {atom, A};
glt_1(F) when is_float(F) -> {float, F};
glt_1(I) when is_integer(I) -> {integer, I};
glt_1(T) when is_tuple(T) ->
    {Es,_} = foldl(fun(Val, {Es0, Index}) ->
                           Type = glt_1(Val),
                           Es = set_element_type({integer,Index}, Type, Es0),
                           {Es, Index + 1}
                   end, {#{}, 1}, tuple_to_list(T)),
    {tuple, tuple_size(T), Es};
glt_1(L) ->
    {literal, L}.

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
    #vst{current=St0} = Vst0,
    try FailFun(Vst0) of
        Vst1 ->
            Vst2 = branch_state(Lbl, Vst1),
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

%% A shorthand version of branch/4 for when the state is only altered on
%% success.
branch(Fail, Vst, SuccFun) ->
    branch(Fail, Vst, fun(V) -> V end, SuccFun).

%% Directly branches off the state. This is an "internal" operation that should
%% be used sparingly.
branch_state(0, #vst{}=Vst) ->
    %% If the instruction fails, the stack may be scanned looking for a catch
    %% tag. Therefore the Y registers must be initialized at this point.
    verify_y_init(Vst),
    Vst;
branch_state(L, #vst{current=St,branched=B,ref_ctr=Counter0}=Vst) ->
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
        none ->
            {St, Counter};
        {value,OtherSt} when St =:= none ->
            {OtherSt, Counter};
        {value,OtherSt} ->
             merge_states_1(St, OtherSt, Counter)
    end.

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
merge_tags({catchtag,T0}, {catchtag,T1}) ->
    {catchtag, ordsets:from_list(T0 ++ T1)};
merge_tags({trytag,T0}, {trytag,T1}) ->
    {trytag, ordsets:from_list(T0 ++ T1)};
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

merge_ct_1([C0|Ct0], [C1|Ct1]) ->
    [ordsets:from_list(C0++C1)|merge_ct_1(Ct0, Ct1)];
merge_ct_1([], []) -> [];
merge_ct_1(_, _) -> undecided.

tuple_sz([Sz]) -> Sz;
tuple_sz(Sz) -> Sz.

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
%%% Return/argument types of BIFs
%%%

bif_return_type('-', Src, Vst) ->
    arith_return_type(Src, Vst);
bif_return_type('+', Src, Vst) ->
    arith_return_type(Src, Vst);
bif_return_type('*', Src, Vst) ->
    arith_return_type(Src, Vst);
bif_return_type(abs, [Num], Vst) ->
    case get_term_type(Num, Vst) of
        {float,_}=T -> T;
        {integer,_}=T -> T;
        _ -> number
    end;
bif_return_type(float, _, _) -> {float,[]};
bif_return_type('/', _, _) -> {float,[]};
%% Binary operations
bif_return_type('binary_part', [_,_], _) -> binary;
bif_return_type('binary_part', [_,_,_], _) -> binary;
bif_return_type('bit_size', [_], _) -> {integer,[]};
bif_return_type('byte_size', [_], _) -> {integer,[]};
%% Integer operations.
bif_return_type(ceil, [_], _) -> {integer,[]};
bif_return_type('div', [_,_], _) -> {integer,[]};
bif_return_type(floor, [_], _) -> {integer,[]};
bif_return_type('rem', [_,_], _) -> {integer,[]};
bif_return_type(length, [_], _) -> {integer,[]};
bif_return_type(size, [_], _) -> {integer,[]};
bif_return_type(trunc, [_], _) -> {integer,[]};
bif_return_type(round, [_], _) -> {integer,[]};
bif_return_type('band', [_,_], _) -> {integer,[]};
bif_return_type('bor', [_,_], _) -> {integer,[]};
bif_return_type('bxor', [_,_], _) -> {integer,[]};
bif_return_type('bnot', [_], _) -> {integer,[]};
bif_return_type('bsl', [_,_], _) -> {integer,[]};
bif_return_type('bsr', [_,_], _) -> {integer,[]};
%% Booleans.
bif_return_type('==', [_,_], _) -> bool;
bif_return_type('/=', [_,_], _) -> bool;
bif_return_type('=<', [_,_], _) -> bool;
bif_return_type('<', [_,_], _) -> bool;
bif_return_type('>=', [_,_], _) -> bool;
bif_return_type('>', [_,_], _) -> bool;
bif_return_type('=:=', [_,_], _) -> bool;
bif_return_type('=/=', [_,_], _) -> bool;
bif_return_type('not', [_], _) -> bool;
bif_return_type('and', [_,_], _) -> bool;
bif_return_type('or', [_,_], _) -> bool;
bif_return_type('xor', [_,_], _) -> bool;
bif_return_type(is_atom, [_], _) -> bool;
bif_return_type(is_boolean, [_], _) -> bool;
bif_return_type(is_binary, [_], _) -> bool;
bif_return_type(is_float, [_], _) -> bool;
bif_return_type(is_function, [_], _) -> bool;
bif_return_type(is_function, [_,_], _) -> bool;
bif_return_type(is_integer, [_], _) -> bool;
bif_return_type(is_list, [_], _) -> bool;
bif_return_type(is_map, [_], _) -> bool;
bif_return_type(is_map_key, [_, _], _) -> bool;
bif_return_type(is_number, [_], _) -> bool;
bif_return_type(is_pid, [_], _) -> bool;
bif_return_type(is_port, [_], _) -> bool;
bif_return_type(is_reference, [_], _) -> bool;
bif_return_type(is_tuple, [_], _) -> bool;
%% Misc.
bif_return_type(tuple_size, [_], _) -> {integer,[]};
bif_return_type(map_size, [_], _) -> {integer,[]};
bif_return_type(node, [], _) -> {atom,[]};
bif_return_type(node, [_], _) -> {atom,[]};
bif_return_type(hd, [_], _) -> term;
bif_return_type(tl, [_], _) -> term;
bif_return_type(get, [_], _) -> term;
bif_return_type(Bif, _, _) when is_atom(Bif) -> term.

%% Generic
bif_arg_types(tuple_size, [_]) -> [{tuple,[0],#{}}];
bif_arg_types(map_size, [_]) -> [map];
bif_arg_types(is_map_key, [_,_]) -> [term, map];
bif_arg_types(map_get, [_,_]) -> [term, map];
bif_arg_types(length, [_]) -> [list];
bif_arg_types(hd, [_]) -> [cons];
bif_arg_types(tl, [_]) -> [cons];
%% Boolean
bif_arg_types('not', [_]) -> [bool];
bif_arg_types('and', [_,_]) -> [bool, bool];
bif_arg_types('or', [_,_]) -> [bool, bool];
bif_arg_types('xor', [_,_]) -> [bool, bool];
%% Binary
bif_arg_types('binary_part', [_,_]) ->
    PosLen = {tuple, 2, #{ {integer,1} => {integer,[]},
                           {integer,2} => {integer,[]} }},
    [binary, PosLen];
bif_arg_types('binary_part', [_,_,_]) ->
    [binary, {integer,[]}, {integer,[]}];
bif_arg_types('bit_size', [_]) -> [binary];
bif_arg_types('byte_size', [_]) -> [binary];
%% Numerical
bif_arg_types('-', [_]) -> [number];
bif_arg_types('-', [_,_]) -> [number,number];
bif_arg_types('+', [_]) -> [number];
bif_arg_types('+', [_,_]) -> [number,number];
bif_arg_types('*', [_,_]) -> [number, number];
bif_arg_types('/', [_,_]) -> [number, number];
bif_arg_types(abs, [_]) -> [number];
bif_arg_types(ceil, [_]) -> [number];
bif_arg_types(float, [_]) -> [number];
bif_arg_types(floor, [_]) -> [number];
bif_arg_types(trunc, [_]) -> [number];
bif_arg_types(round, [_]) -> [number];
%% Integer-specific
bif_arg_types('div', [_,_]) -> [{integer,[]}, {integer,[]}];
bif_arg_types('rem', [_,_]) -> [{integer,[]}, {integer,[]}];
bif_arg_types('band', [_,_]) -> [{integer,[]}, {integer,[]}];
bif_arg_types('bor', [_,_]) -> [{integer,[]}, {integer,[]}];
bif_arg_types('bxor', [_,_]) -> [{integer,[]}, {integer,[]}];
bif_arg_types('bnot', [_]) -> [{integer,[]}];
bif_arg_types('bsl', [_,_]) -> [{integer,[]}, {integer,[]}];
bif_arg_types('bsr', [_,_]) -> [{integer,[]}, {integer,[]}];
%% Unsafe type tests that may fail if an argument doesn't have the right type.
bif_arg_types(is_function, [_,_]) -> [term, {integer,[]}];
bif_arg_types(_, Args) -> [term || _Arg <- Args].

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

arith_return_type([A], Vst) ->
    %% Unary '+' or '-'.
    case get_term_type(A, Vst) of
	{integer,_} -> {integer,[]};
	{float,_} -> {float,[]};
        _ -> number
    end;
arith_return_type([A,B], Vst) ->
    TypeA = get_term_type(A, Vst),
    TypeB = get_term_type(B, Vst),
    case {TypeA, TypeB} of
	{{integer,_},{integer,_}} -> {integer,[]};
	{{float,_},_} -> {float,[]};
	{_,{float,_}} -> {float,[]};
	{_,_} -> number
    end;
arith_return_type(_, _) -> number.

%%%
%%% Return/argument types of calls
%%%

call_return_type({extfunc,M,F,A}, Vst) -> call_return_type_1(M, F, A, Vst);
call_return_type(_, _) -> term.

call_return_type_1(erlang, setelement, 3, Vst) ->
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
                    Es = set_element_type({integer,I}, ValueType, Es0),
                    {tuple, Sz, Es};
                none ->
                    TupleType
            end;
        _ ->
            %% The index could point anywhere, so we must discard all element
            %% information.
            setelement(3, TupleType, #{})
    end;
call_return_type_1(erlang, '++', 2, Vst) ->
    LType = get_term_type({x,0}, Vst),
    RType = get_term_type({x,1}, Vst),
    case LType =:= cons orelse RType =:= cons of
        true ->
            cons;
        false ->
            %% `[] ++ RHS` yields RHS, even if RHS is not a list
            join(list, RType)
    end;
call_return_type_1(erlang, '--', 2, _Vst) ->
    list;
call_return_type_1(erlang, F, A, _) ->
    erlang_mod_return_type(F, A);
call_return_type_1(lists, F, A, Vst) ->
    lists_mod_return_type(F, A, Vst);
call_return_type_1(math, F, A, _) ->
    math_mod_return_type(F, A);
call_return_type_1(M, F, A, _) when is_atom(M), is_atom(F), is_integer(A), A >= 0 ->
    term.

erlang_mod_return_type(exit, 1) -> exception;
erlang_mod_return_type(throw, 1) -> exception;
erlang_mod_return_type(error, 1) -> exception;
erlang_mod_return_type(error, 2) -> exception;
erlang_mod_return_type(F, A) when is_atom(F), is_integer(A), A >= 0 -> term.

math_mod_return_type(cos, 1) -> {float,[]};
math_mod_return_type(cosh, 1) -> {float,[]};
math_mod_return_type(sin, 1) -> {float,[]};
math_mod_return_type(sinh, 1) -> {float,[]};
math_mod_return_type(tan, 1) -> {float,[]};
math_mod_return_type(tanh, 1) -> {float,[]};
math_mod_return_type(acos, 1) -> {float,[]};
math_mod_return_type(acosh, 1) -> {float,[]};
math_mod_return_type(asin, 1) -> {float,[]};
math_mod_return_type(asinh, 1) -> {float,[]};
math_mod_return_type(atan, 1) -> {float,[]};
math_mod_return_type(atanh, 1) -> {float,[]};
math_mod_return_type(erf, 1) -> {float,[]};
math_mod_return_type(erfc, 1) -> {float,[]};
math_mod_return_type(exp, 1) -> {float,[]};
math_mod_return_type(log, 1) -> {float,[]};
math_mod_return_type(log2, 1) -> {float,[]};
math_mod_return_type(log10, 1) -> {float,[]};
math_mod_return_type(sqrt, 1) -> {float,[]};
math_mod_return_type(atan2, 2) -> {float,[]};
math_mod_return_type(pow, 2) -> {float,[]};
math_mod_return_type(ceil, 1) -> {float,[]};
math_mod_return_type(floor, 1) -> {float,[]};
math_mod_return_type(fmod, 2) -> {float,[]};
math_mod_return_type(pi, 0) -> {float,[]};
math_mod_return_type(F, A) when is_atom(F), is_integer(A), A >= 0 -> term.

lists_mod_return_type(all, 2, _Vst) ->
    bool;
lists_mod_return_type(any, 2, _Vst) ->
    bool;
lists_mod_return_type(keymember, 3, _Vst) ->
    bool;
lists_mod_return_type(member, 2, _Vst) ->
    bool;
lists_mod_return_type(prefix, 2, _Vst) ->
    bool;
lists_mod_return_type(suffix, 2, _Vst) ->
    bool;
lists_mod_return_type(dropwhile, 2, _Vst) ->
    list;
lists_mod_return_type(duplicate, 2, _Vst) ->
    list;
lists_mod_return_type(filter, 2, _Vst) ->
    list;
lists_mod_return_type(flatten, 1, _Vst) ->
    list;
lists_mod_return_type(map, 2, Vst) ->
    same_length_type({x,1}, Vst);
lists_mod_return_type(MF, 3, Vst) when MF =:= mapfoldl; MF =:= mapfoldr ->
    ListType = same_length_type({x,2}, Vst),
    {tuple,2,#{ {integer,1} => ListType} };
lists_mod_return_type(partition, 2, _Vst) ->
    two_tuple(list, list);
lists_mod_return_type(reverse, 1, Vst) ->
    same_length_type({x,0}, Vst);
lists_mod_return_type(seq, 2, _Vst) ->
    list;
lists_mod_return_type(sort, 1, Vst) ->
    same_length_type({x,0}, Vst);
lists_mod_return_type(sort, 2, Vst) ->
    same_length_type({x,1}, Vst);
lists_mod_return_type(splitwith, 2, _Vst) ->
    two_tuple(list, list);
lists_mod_return_type(takewhile, 2, _Vst) ->
    list;
lists_mod_return_type(unzip, 1, Vst) ->
    ListType = same_length_type({x,0}, Vst),
    two_tuple(ListType, ListType);
lists_mod_return_type(usort, 1, Vst) ->
    same_length_type({x,0}, Vst);
lists_mod_return_type(zip, 2, _Vst) ->
    list;
lists_mod_return_type(zipwith, 3, _Vst) ->
    list;
lists_mod_return_type(_, _, _) ->
    term.

two_tuple(Type1, Type2) ->
    {tuple,2,#{ {integer,1} => Type1,
                {integer,2} => Type2 }}.

same_length_type(Reg, Vst) ->
    case get_term_type(Reg, Vst) of
        {literal,[_|_]} -> cons;
        cons -> cons;
        nil -> nil;
        _ -> list
    end.

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

gb_trees_from_list(L) -> gb_trees:from_orddict(sort(L)).

error(Error) -> throw(Error).
