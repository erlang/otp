%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2004-2025. All Rights Reserved.
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
-moduledoc false.

-include("beam_asm.hrl").

-define(UNICODE_MAX, (16#10FFFF)).

%% When an instruction throws an exception it does so by branching (branch/4)
%% to this label, following the {f,0} convention used in BIFs.
%%
%% Instructions that have fail labels but cannot throw exceptions should guard
%% themselves with assert_no_exception/1, and instructions that are statically
%% guaranteed not to fail should simply skip branching.
-define(EXCEPTION_LABEL, 0).

-compile({no_auto_import,[min/2]}).

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).

%% Interface for compiler.
-export([validate/2, format_error/1]).

-import(lists, [dropwhile/2,foldl/3,member/2,reverse/2,zip/2]).

%% To be called by the compiler.

-spec validate(Code, Level) -> Result when
      Code :: beam_utils:module_code(),
      Level :: strong | weak,
      Result :: ok | {error, [{atom(), list()}]}.

validate({Mod,Exp,Attr,Fs,Lc}, Level) when is_atom(Mod),
                                           is_list(Exp),
                                           is_list(Attr),
                                           is_integer(Lc) ->
    Ft = build_function_table(Fs, #{}),
    case validate_0(Fs, Mod, Level, Ft) of
        [] ->
            ok;
        Es0 ->
            Es = [{1,?MODULE,E} || E <- Es0],
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
format_error({{_M,F,A},too_many_arguments}) ->
    %% The linter rejects user-provided functions that violate this, leaving
    %% only generated functions like funs or comprehensions. This is not
    %% super-helpful but it's better than nothing.
    io_lib:format("System limit reached: generated function ~p/~p has too "
                  "many arguments.", [F, A]);
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

%% validate(Module, Level, [Function], Ft) -> [] | [Error]
%%  A list of functions with their code. The code is in the same
%%  format as used in the compiler and in .S files.


validate_0([], _Module, _Level, _Ft) ->
    [];
validate_0([{function, Name, Arity, Entry, Code} | Fs], Module, Level, Ft) ->
    MFA = {Module, Name, Arity},
    try validate_1(Code, MFA, Entry, Level, Ft) of
        _ ->
            validate_0(Fs, Module, Level, Ft)
    catch
        throw:Error ->
            %% Controlled error.
            [Error | validate_0(Fs, Module, Level, Ft)];
        Class:Error:Stack ->
            %% Crash.
            io:fwrite("Function: ~w/~w\n", [Name,Arity]),
            erlang:raise(Class, Error, Stack)
    end.

-record(t_abstract, {kind}).

%% The types are the same as in 'beam_types.hrl', with the addition of:
%%
%% * `#t_abstract{}` which describes tuples under construction, match context
%%   positions, and so on.
%% * `(fun(values()) -> type())` that defers figuring out the type until it's
%%   actually used. Mainly used to coax more type information out of
%%   `get_tuple_element` where a test on one element (e.g. record tag) may
%%   affect the type of another.
-type validator_type() :: #t_abstract{} | fun((values()) -> type()) | type().

-record(value_ref, {id :: index()}).
-record(value, {op :: term(), args :: [argument()], type :: validator_type()}).

-type argument() :: #value_ref{} | beam_literal().
-type values() :: #{ #value_ref{} => #value{} }.

-type index() :: non_neg_integer().

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
         vs=#{} :: values(),
         %% Register states.
         xs=#{} :: x_regs(),
         ys=#{} :: y_regs(),
         f=init_fregs(),
         %% A set of all registers containing "fragile" terms. That is, terms
         %% that don't exist on our process heap and would be destroyed by a
         %% GC.
         fragile=sets:new() :: sets:set(),
         %% Number of Y registers.
         %%
         %% Note that this may be 0 if there's a frame without saved values,
         %% such as on a body-recursive call.
         numy=none :: none | {undecided,index()} | index(),
         %% Available heap size.
         h=0,
         %% Available heap size for funs (aka lambdas).
         hl=0,
         %%Available heap size for floats.
         hf=0,
         %% List of hot catch/try tags
         ct=[],
         %% Previous instruction was setelement/3.
         setelem=false,
         %% put/1 instructions left.
         puts_left=none,
         %% Current receive state:
         %%
         %%   * 'none'            - Not in a receive loop.
         %%   * 'marked_position' - We've used a marker prior to loop_rec.
         %%   * 'entered_loop'    - We're in a receive loop.
         %%   * 'undecided'
         recv_state=none :: none | undecided | marked_position | entered_loop,
         %% Holds the current saved position for each `#t_bs_context{}`, in the
         %% sense that the position is equal to that of their context. They are
         %% invalidated whenever their context advances.
         %%
         %% These are used to update the unit of saved positions after
         %% operations that test the incoming unit, such as bs_test_unit and
         %% bs_get_binary2 with {atom,all}.
         ms_positions=#{} :: #{ Ctx :: #value_ref{} => Pos :: #value_ref{} }
        }).

-type label()        :: integer().
-type state()        :: #st{} | 'none'.

%% Validator state
-record(vst,
        {%% Current state
         current=none              :: state(),
         %% Validation level
         level                     :: strong | weak,
         %% States at labels
         branched=#{}              :: #{ label() => state() },
         %% All defined labels
         labels=sets:new()    :: sets:set(),
         %% Information of other functions in the module
         ft=#{}                    :: #{ label() => map() },
         %% Counter for #value_ref{} creation
         ref_ctr=0                 :: index()
        }).

build_function_table([{function,Name,Arity,Entry,Code0}|Fs], Acc) ->
    Code = dropwhile(fun({label,L}) when L =:= Entry ->
                             false;
                        (_) ->
                             true
                     end, Code0),
    case Code of
        [{label,Entry} | Is] ->
            Info = #{ name => Name,
                      arity => Arity,
                      parameter_info => find_parameter_info(Is, #{}),
                      always_fails => always_fails(Is) },
            build_function_table(Fs, Acc#{ Entry => Info });
        _ ->
            %% Something is seriously wrong. Ignore it for now.
            %% It will be detected and diagnosed later.
            build_function_table(Fs, Acc)
    end;
build_function_table([], Acc) ->
    Acc.

find_parameter_info([{'%', {var_info, Reg, Info}} | Is], Acc) ->
    find_parameter_info(Is, Acc#{ Reg => Info });
find_parameter_info([{'%', _} | Is], Acc) ->
    find_parameter_info(Is, Acc);
find_parameter_info(_, Acc) ->
    Acc.

always_fails([{jump,_}|_]) -> true;
always_fails(_) -> false.

validate_1(Is, MFA0, Entry, Level, Ft) ->
    {Offset, MFA, Header, Body} = extract_header(Is, MFA0, Entry, 1, []),

    Vst0 = init_vst(MFA, Level, Ft),

    %% We validate the header after the body as the latter may jump to the
    %% former to raise 'function_clause' exceptions.
    Vst1 = validate_instrs(Body, MFA, Offset, Vst0),
    Vst = validate_instrs(Header, MFA, 1, Vst1),

    validate_branches(MFA, Vst).

extract_header([{func_info, {atom, Mod}, {atom,Name}, Arity}=I |
                [{label, Entry} | _]=Is],
               {_, Name, Arity}, Entry, Offset, Acc) ->
    MFA = {Mod, Name, Arity} ,
    case Arity =< ?MAX_FUNC_ARGS of
        true -> {Offset + 1, MFA, reverse(Acc, [I]), Is};
        false -> error({MFA, too_many_arguments})
    end;
extract_header([{label,_}=I | Is], MFA, Entry, Offset, Acc) ->
    extract_header(Is, MFA, Entry, Offset + 1, [I | Acc]);
extract_header([{line,_}=I | Is], MFA, Entry, Offset, Acc) ->
    extract_header(Is, MFA, Entry, Offset + 1, [I | Acc]);
extract_header(_Is, MFA, _Entry, _Offset, _Acc) ->
    error({MFA, invalid_function_header}).

init_vst({_, _, Arity}, Level, Ft) ->
    Vst = #vst{branched=#{},
               current=#st{},
               ft=Ft,
               labels=sets:new(),
               level=Level},
    init_function_args(Arity - 1, Vst).

init_function_args(-1, Vst) ->
    Vst;
init_function_args(X, Vst) ->
    init_function_args(X - 1, create_term(any, argument, [], {x,X}, Vst)).

kill_heap_allocation(#vst{current=St0}=Vst) ->
    St = St0#st{h=0,hl=0,hf=0},
    Vst#vst{current=St}.

validate_branches(MFA, Vst) ->
    #vst{ branched=Targets0, labels=Labels0 } = Vst,
    Targets = maps:keys(Targets0),
    Labels = sets:to_list(Labels0),
    case Targets -- Labels of
        [_|_]=Undef ->
            Error = {undef_labels, Undef},
            error({MFA, Error});
        [] ->
            Vst
    end.

validate_instrs([I|Is], MFA, Offset, Vst0) ->
    validate_instrs(Is, MFA, Offset+1,
                    try
                        Vst = validate_mutation(I, Vst0),
                        vi(I, Vst)
                    catch Error ->
                        error({MFA, {I, Offset, Error}})
                    end);
validate_instrs([], _MFA, _Offset, Vst) ->
    Vst.

vi({label,Lbl}, #vst{current=St0,
                          ref_ctr=Counter0,
                          branched=Branched0,
                          labels=Labels0}=Vst) ->
    {St, Counter} = merge_states(Lbl, St0, Branched0, Counter0),

    Branched = Branched0#{ Lbl => St },
    Labels = sets:add_element(Lbl, Labels0),

    Vst#vst{current=St,
            ref_ctr=Counter,
            branched=Branched,
            labels=Labels};
vi(_I, #vst{current=none}=Vst) ->
    %% Ignore all unreachable code.
    Vst;

%%
%% Misc annotations
%%

vi({'%', {var_info, Reg, Info}}, Vst) ->
    validate_var_info(Info, Reg, Vst);
vi({'%', {remove_fragility, Reg}}, Vst) ->
    %% This is a hack to make prim_eval:'receive'/2 work.
    %%
    %% Normally it's illegal to pass fragile terms as a function argument as we
    %% have no way of knowing what the callee will do with it, but we know that
    %% prim_eval:'receive'/2 won't leak the term, nor cause a GC since it's
    %% disabled while matching messages.
    remove_fragility(Reg, Vst);
vi({'%',_}, Vst) ->
    Vst;
vi({line,_}, Vst) ->
    Vst;
vi({executable_line,_,Index}, Vst) when is_integer(Index) ->
    Vst;
vi({debug_line,_,Index,Live,Info}, Vst) when is_integer(Index),
                                             is_integer(Live) ->
    validate_debug_line(Info, Live, Vst);
vi({debug_line,_,_,Index,Live,Info}, Vst) when is_integer(Index),
                                               is_integer(Live) ->
    validate_debug_line(Info, Live, Vst);
vi(nif_start, Vst) ->
    Vst;
%%
%% Moves
%%

vi({move,Src,Dst}, Vst) ->
    assign(Src, Dst, Vst);
vi({swap,RegA,RegB}, Vst0) ->
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
vi({fmove,Src,{fr,_}=Dst}, Vst) ->
    assert_type(#t_float{}, Src, Vst),
    set_freg(Dst, Vst);
vi({fmove,{fr,_}=Src,Dst}, Vst0) ->
    assert_freg_set(Src, Vst0),
    Vst = eat_heap_float(Vst0),
    create_term(#t_float{}, fmove, [], Dst, Vst);
vi({init_yregs,{list,Yregs}}, Vst0) ->
    case ordsets:from_list(Yregs) of
        [] -> error(empty_list);
        Yregs -> ok;
        _ -> error(not_ordset)
    end,
    foldl(fun(Y, Vst) ->
                  create_tag(initialized, init, [], Y, Vst)
          end, Vst0, Yregs);

%%
%% Matching and test instructions.
%%

vi({jump,{f,Lbl}}, Vst) ->
    assert_no_exception(Lbl),

    %% The next instruction is never executed.
    branch(Lbl, Vst, fun kill_state/1);
vi({select_val,Src0,{f,Fail},{list,Choices}}, Vst) ->
    Src = unpack_typed_arg(Src0, Vst),
    assert_term(Src, Vst),
    assert_choices(Choices),
    validate_select_val(Fail, Choices, Src, Vst);
vi({select_tuple_arity,Tuple0,{f,Fail},{list,Choices}}, Vst) ->
    Tuple = unpack_typed_arg(Tuple0, Vst),

    assert_type(#t_tuple{}, Tuple, Vst),
    assert_arities(Choices),
    validate_select_tuple_arity(Fail, Choices, Tuple, Vst);
vi({test,has_map_fields,{f,Lbl},Src,{list,List}}, Vst) ->
    verify_has_map_fields(Lbl, Src, List, Vst);
vi({test,is_atom,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_atom{}, Src, Vst);
vi({test,is_binary,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_bitstring{size_unit=8}, Src, Vst);
vi({test,is_bitstr,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_bitstring{}, Src, Vst);
vi({test,is_boolean,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, beam_types:make_boolean(), Src, Vst);
vi({test,is_float,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_float{}, Src, Vst);
vi({test,is_function,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_fun{}, Src, Vst);
vi({test,is_function2,{f,Lbl},[Src,{integer,Arity}]}, Vst)
  when Arity >= 0, Arity =< ?MAX_FUNC_ARGS ->
    type_test(Lbl, #t_fun{arity=Arity}, Src, Vst);
vi({test,is_function2,{f,Lbl},[Src0,_Arity]}, Vst) ->
    Src = unpack_typed_arg(Src0, Vst),
    assert_term(Src, Vst),
    branch(Lbl, Vst,
           fun(FailVst) ->
                   %% We cannot subtract the function type when the arity is
                   %% unknown: `Src` may still be a function if the arity is
                   %% outside the allowed range.
                   FailVst
           end,
           fun(SuccVst) ->
                   update_type(fun meet/2, #t_fun{}, Src, SuccVst)
           end);
vi({test,is_tuple,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_tuple{}, Src, Vst);
vi({test,is_integer,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_integer{}, Src, Vst);
vi({test,is_nonempty_list,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_cons{}, Src, Vst);
vi({test,is_number,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_number{}, Src, Vst);
vi({test,is_list,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_list{}, Src, Vst);
vi({test,is_map,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, #t_map{}, Src, Vst);
vi({test,is_nil,{f,Lbl},[Src0]}, Vst) ->
    %% is_nil is an exact check against the 'nil' value, and should not be
    %% treated as a simple type test.
    Src = unpack_typed_arg(Src0, Vst),
    assert_term(Src, Vst),
    branch(Lbl, Vst,
           fun(FailVst) ->
                   update_ne_types(Src, nil, FailVst)
           end,
           fun(SuccVst) ->
                   update_eq_types(Src, nil, SuccVst)
           end);
vi({test,is_pid,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, pid, Src, Vst);
vi({test,is_port,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, port, Src, Vst);
vi({test,is_reference,{f,Lbl},[Src]}, Vst) ->
    type_test(Lbl, reference, Src, Vst);
vi({test,test_arity,{f,Lbl},[Tuple,Sz]}, Vst) when is_integer(Sz) ->
    assert_type(#t_tuple{}, Tuple, Vst),
    Type =  #t_tuple{exact=true,size=Sz},
    type_test(Lbl, Type, Tuple, Vst);
vi({test,is_tagged_tuple,{f,Lbl},[Src0,Sz,Atom]}, Vst) ->
    Src = unpack_typed_arg(Src0, Vst),
    assert_term(Src, Vst),
    Es = #{ 1 => get_literal_type(Atom) },
    Type = #t_tuple{exact=true,size=Sz,elements=Es},
    type_test(Lbl, Type, Src, Vst);
vi({test,is_eq_exact,{f,Lbl},[Src0,Val0]}, Vst) ->
    assert_no_exception(Lbl),
    Src = unpack_typed_arg(Src0, Vst),
    assert_term(Src, Vst),
    Val = unpack_typed_arg(Val0, Vst),
    assert_term(Val, Vst),
    branch(Lbl, Vst,
           fun(FailVst) ->
                   update_ne_types(Src, Val, FailVst)
           end,
           fun(SuccVst) ->
                   update_eq_types(Src, Val, SuccVst)
           end);
vi({test,is_ne_exact,{f,Lbl},[Src0,Val0]}, Vst) ->
    assert_no_exception(Lbl),
    Src = unpack_typed_arg(Src0, Vst),
    assert_term(Src, Vst),
    Val = unpack_typed_arg(Val0, Vst),
    assert_term(Val, Vst),
    branch(Lbl, Vst,
           fun(FailVst) ->
                   update_eq_types(Src, Val, FailVst)
           end,
           fun(SuccVst) ->
                   update_ne_types(Src, Val, SuccVst)
           end);


%%
%% Simple getters that can't fail.
%%

vi({get_list,Src,D1,D2}, Vst0) ->
    assert_not_literal(Src),
    assert_type(#t_cons{}, Src, Vst0),

    SrcType = get_term_type(Src, Vst0),
    {HeadType, _, _} = beam_call_types:types(erlang, hd, [SrcType]),
    {TailType, _, _} = beam_call_types:types(erlang, tl, [SrcType]),

    Vst = extract_term(HeadType, get_hd, [Src], D1, Vst0),
    extract_term(TailType, get_tl, [Src], D2, Vst, Vst0);
vi({get_hd,Src,Dst}, Vst) ->
    assert_not_literal(Src),
    assert_type(#t_cons{}, Src, Vst),

    SrcType = get_term_type(Src, Vst),
    {HeadType, _, _} = beam_call_types:types(erlang, hd, [SrcType]),

    extract_term(HeadType, get_hd, [Src], Dst, Vst);
vi({get_tl,Src,Dst}, Vst) ->
    assert_not_literal(Src),
    assert_type(#t_cons{}, Src, Vst),

    SrcType = get_term_type(Src, Vst),
    {TailType, _, _} = beam_call_types:types(erlang, tl, [SrcType]),

    extract_term(TailType, get_tl, [Src], Dst, Vst);
vi({get_tuple_element,Src,N,Dst}, Vst) ->
    Index = N+1,
    assert_not_literal(Src),
    assert_type(#t_tuple{size=Index}, Src, Vst),

    VRef = get_reg_vref(Src, Vst),
    Type = fun(Vs) ->
                    #value{type=T} = map_get(VRef, Vs),
                    #t_tuple{elements=Es} = normalize(concrete_type(T, Vs)),
                    beam_types:get_tuple_element(Index, Es)
           end,

    extract_term(Type, {bif,element}, [{integer,Index}, Src], Dst, Vst);

%%
%% Allocate, deallocate, et.al.
%%

vi({test_heap,Heap,Live}, Vst) ->
    test_heap(Heap, Live, Vst);
vi({allocate,Stk,Live}, Vst) ->
    allocate(uninitialized, Stk, 0, Live, Vst);
vi({allocate_heap,Stk,Heap,Live}, Vst) ->
    allocate(uninitialized, Stk, Heap, Live, Vst);
vi({deallocate,StkSize}, #vst{current=#st{numy=StkSize}}=Vst) ->
    verify_no_ct(Vst),
    deallocate(Vst);
vi({deallocate,_}, #vst{current=#st{numy=NumY}}) ->
    error({allocated,NumY});
vi({trim,N,Remaining}, #vst{current=St0}=Vst) ->
    #st{numy=NumY} = St0,
    if
        N =< NumY, N+Remaining =:= NumY ->
            Vst#vst{current=trim_stack(N, 0, NumY, St0)};
        N > NumY; N+Remaining =/= NumY ->
            error({trim,N,Remaining,allocated,NumY})
    end;

%%
%% Term-building instructions
%%

vi({put_list,A,B,Dst}, Vst0) ->
    Vst = eat_heap(2, Vst0),

    Head = get_term_type(A, Vst),
    Tail = get_term_type(B, Vst),

    create_term(beam_types:make_cons(Head, Tail), put_list, [A, B], Dst, Vst);
vi({put_tuple2,Dst,{list,Elements}}, Vst0) ->
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
vi({set_tuple_element,Src,Tuple,N}, Vst) ->
    %% This instruction never fails, though it may be invalid in some contexts;
    %% see validate_mutation/2
    I = N + 1,
    assert_term(Src, Vst),
    assert_type(#t_tuple{size=I}, Tuple, Vst),
    %% Manually update the tuple type; we can't rely on the ordinary update
    %% helpers as we must support overwriting (rather than just widening or
    %% narrowing) known elements, and we can't use extract_term either since
    %% the source tuple may be aliased.
    TupleType0 = get_term_type(Tuple, Vst),
    ArgType = get_term_type(Src, Vst),
    TupleType = beam_types:update_tuple(TupleType0, [{I, ArgType}]),
    override_type(TupleType, Tuple, Vst);
vi({update_record,_Hint,Size,Src,Dst,{list,Ss}}, Vst) ->
    verify_update_record(Size, Src, Dst, Ss, Vst);

%%
%% Calls
%%

vi({apply,Live}, Vst) ->
    validate_body_call(apply, Live+2, Vst);
vi({apply_last,Live,N}, Vst) ->
    validate_tail_call(N, apply, Live+2, Vst);
vi({call,Live,Func}, Vst) ->
    validate_body_call(Func, Live, Vst);
vi({call_ext,Live,Func}, Vst) ->
    validate_body_call(Func, Live, Vst);
vi({call_only,Live,Func}, Vst) ->
    validate_tail_call(none, Func, Live, Vst);
vi({call_ext_only,Live,Func}, Vst) ->
    validate_tail_call(none, Func, Live, Vst);
vi({call_last,Live,Func,N}, Vst) ->
    validate_tail_call(N, Func, Live, Vst);
vi({call_ext_last,Live,Func,N}, Vst) ->
    validate_tail_call(N, Func, Live, Vst);
vi({call_fun2,{f,Lbl},Live,Fun0}, #vst{ft=Ft}=Vst) ->
    %% Fun call with known target. Verify that the target exists, agrees with
    %% the type annotation for `Fun`, and that it has environment variables.
    %%
    %% Direct fun calls without environment variables must be expressed as
    %% local fun calls.
    #{ name := Name, arity := TotalArity } = map_get(Lbl, Ft),
    #tr{t=#t_fun{target={Name,TotalArity}},r=Fun} = Fun0, %Assertion.
    true = Live < TotalArity,                   %Assertion.
    assert_term(Fun, Vst),
    validate_body_call('fun', Live, Vst);
vi({call_fun2,Tag,Live,Fun0}, Vst) ->
    Fun = unpack_typed_arg(Fun0, Vst),
    assert_term(Fun, Vst),

    case Tag of
        {atom,safe} -> ok;
        {atom,unsafe} -> ok;
        _ -> error({invalid_tag,Tag})
    end,

    branch(?EXCEPTION_LABEL, Vst,
           fun(SuccVst0) ->
                   SuccVst = update_type(fun meet/2, #t_fun{arity=Live},
                                         Fun, SuccVst0),
                   validate_body_call('fun', Live, SuccVst)
           end);
vi({call_fun,Live}, Vst) ->
    Fun = {x,Live},
    assert_term(Fun, Vst),

    branch(?EXCEPTION_LABEL, Vst,
           fun(SuccVst0) ->
                   SuccVst = update_type(fun meet/2, #t_fun{arity=Live},
                                         Fun, SuccVst0),
                   validate_body_call('fun', Live+1, SuccVst)
           end);
vi({make_fun3,{f,Lbl},_,_,Dst,{list,Env}}, #vst{ft=Ft}=Vst0) ->
    _ = [assert_term(E, Vst0) || E <- Env],
    NumFree = length(Env),

    #{ name := Name, arity := TotalArity } = map_get(Lbl, Ft),
    Arity = TotalArity - NumFree,
    true = Arity >= 0,                          %Assertion.

    Vst = eat_heap_fun(Vst0),

    Type = #t_fun{target={Name,TotalArity},arity=Arity},

    create_term(Type, make_fun, [], Dst, Vst);
vi(return, Vst) ->
    assert_durable_term({x,0}, Vst),
    verify_return(Vst);

%%
%% BIF calls
%%

vi({bif,Op,{f,Fail},Ss0,Dst0}, Vst0) ->
    Ss = [unpack_typed_arg(Arg, Vst0) || Arg <- Ss0],
    Dst = unpack_typed_arg(Dst0, Vst0),

    case is_float_arith_bif(Op, Ss) of
        true ->
            ?EXCEPTION_LABEL = Fail,            %Assertion.
            validate_float_arith_bif(Ss, Dst, Vst0);
        false ->
            validate_bif(bif, Op, Fail, Ss, Dst, Vst0, Vst0)
    end;
vi({gc_bif,Op,{f,Fail},Live,Ss0,Dst0}, Vst0) ->
    Ss = [unpack_typed_arg(Arg, Vst0) || Arg <- Ss0],
    Dst = unpack_typed_arg(Dst0, Vst0),

    verify_live(Live, Vst0),
    verify_y_init(Vst0),

    %% Heap allocations and X registers are killed regardless of whether we
    %% fail or not, as we may fail after GC.
    Vst1 = kill_heap_allocation(Vst0),
    Vst = prune_x_regs(Live, Vst1),

    validate_bif(gc_bif, Op, Fail, Ss, Dst, Vst0, Vst);

%%
%% Message instructions
%%

vi(send, Vst) ->
    validate_body_call(send, 2, Vst);
vi({loop_rec,{f,Fail},Dst}, Vst0) ->
    assert_no_exception(Fail),

    Vst = update_receive_state(entered_loop, Vst0),

    branch(Fail, Vst,
           fun(SuccVst0) ->
                   %% This term may not be part of the root set until
                   %% remove_message/0 is executed. If control transfers to the
                   %% loop_rec_end/1 instruction, no part of this term must be
                   %% stored in a Y register.
                   {Ref, SuccVst} = new_value(any, loop_rec, [], SuccVst0),
                   mark_fragile(Dst, set_reg_vref(Ref, Dst, SuccVst))
           end);
vi({loop_rec_end,Lbl}, Vst) ->
    assert_no_exception(Lbl),
    verify_y_init(Vst),
    kill_state(Vst);
vi({recv_marker_reserve=Op, Dst}, Vst) ->
    create_term(#t_abstract{kind=receive_marker}, Op, [], Dst, Vst);
vi({recv_marker_bind, Marker, Ref}, Vst) ->
    assert_type(#t_abstract{kind=receive_marker}, Marker, Vst),
    assert_durable_term(Ref, Vst),
    assert_not_literal(Ref),
    Vst;
vi({recv_marker_clear, Ref}, Vst) ->
    assert_durable_term(Ref, Vst),
    assert_not_literal(Ref),
    Vst;
vi({recv_marker_use, Ref}, Vst) ->
    assert_durable_term(Ref, Vst),
    assert_not_literal(Ref),
    update_receive_state(marked_position, Vst);
vi(remove_message, Vst0) ->
    Vst = update_receive_state(none, Vst0),

    %% The message term is no longer fragile. It can be used
    %% without restrictions.
    remove_fragility(Vst);
vi(timeout, Vst0) ->
    Vst = update_receive_state(none, Vst0),
    prune_x_regs(0, Vst);
vi({wait,{f,Lbl}}, Vst) ->
    assert_no_exception(Lbl),
    verify_y_init(Vst),
    branch(Lbl, Vst, fun kill_state/1);
vi({wait_timeout,{f,Lbl},Src}, Vst0) ->
    assert_no_exception(Lbl),

    assert_term(Src, Vst0),
    verify_y_init(Vst0),

    Vst = branch(Lbl, schedule_out(0, Vst0)),
    branch(?EXCEPTION_LABEL, Vst);

%%
%% Catch & try.
%%
vi({'catch',Dst,{f,Fail}}, Vst) when Fail =/= none ->
    init_try_catch_branch(catchtag, Dst, Fail, Vst);
vi({'try',Dst,{f,Fail}}, Vst) when Fail =/= none ->
    init_try_catch_branch(trytag, Dst, Fail, Vst);
vi({catch_end,Reg}, #vst{current=#st{ct=[Tag|_]}}=Vst0) ->
    case get_tag_type(Reg, Vst0) of
        {catchtag,_Fail}=Tag ->
            Vst = kill_catch_tag(Reg, Vst0),

            %% {x,0} contains the caught term, if any.
            create_term(any, catch_end, [], {x,0}, Vst);
        Type ->
            error({wrong_tag_type,Type})
    end;
vi({try_end,Reg}, #vst{current=#st{ct=[Tag|_]}}=Vst) ->
    case get_tag_type(Reg, Vst) of
        {trytag,_Fail}=Tag ->
            %% Kill the catch tag without affecting X registers.
            kill_catch_tag(Reg, Vst);
        Type ->
            error({wrong_tag_type,Type})
    end;
vi({try_case,Reg}, #vst{current=#st{ct=[Tag|_]}}=Vst0) ->
    case get_tag_type(Reg, Vst0) of
        {trytag,_Fail}=Tag ->
            %% Kill the catch tag and all other state (as if we've been
            %% scheduled out with no live registers). Only previously allocated
            %% Y registers are alive at this point.
            Vst1 = kill_catch_tag(Reg, Vst0),
            Vst2 = schedule_out(0, Vst1),

            %% Class:Error:Stacktrace
            ClassType = #t_atom{elements=[error,exit,throw]},
            Vst3 = create_term(ClassType, try_case, [], {x,0}, Vst2),
            Vst = create_term(any, try_case, [], {x,1}, Vst3),
            create_term(any, try_case, [], {x,2}, Vst);
        Type ->
            error({wrong_tag_type,Type})
    end;
vi(build_stacktrace, Vst0) ->
    verify_y_init(Vst0),
    verify_live(1, Vst0),

    Vst = prune_x_regs(1, Vst0),
    Reg = {x,0},

    assert_durable_term(Reg, Vst),
    create_term(#t_list{}, build_stacktrace, [], Reg, Vst);

%%
%% Map instructions.
%%

vi({get_map_elements,{f,Fail},Src0,{list,List}}, Vst) ->
    Src = unpack_typed_arg(Src0, Vst),
    verify_get_map(Fail, Src, List, Vst);
vi({put_map_assoc=Op,{f,Fail},Src,Dst,Live,{list,List}}, Vst) ->
    verify_put_map(Op, Fail, Src, Dst, Live, List, Vst);
vi({put_map_exact=Op,{f,Fail},Src,Dst,Live,{list,List}}, Vst) ->
    verify_put_map(Op, Fail, Src, Dst, Live, List, Vst);

%%
%% Bit syntax matching
%%

vi({bs_match,{f,Fail},Ctx0,{commands,List}}, Vst) ->
    Ctx = unpack_typed_arg(Ctx0, Vst),

    assert_no_exception(Fail),
    assert_type(#t_bs_context{}, Ctx, Vst),
    verify_y_init(Vst),

    branch(Fail, Vst,
           fun(FailVst) ->
                   validate_failed_bs_match(List, Ctx, FailVst)
           end,
           fun(SuccVst) ->
                   validate_bs_match(List, Ctx, 0, 1, SuccVst)
           end);
vi({bs_get_tail,Ctx,Dst,Live}, Vst0) ->
    assert_type(#t_bs_context{}, Ctx, Vst0),
    verify_live(Live, Vst0),
    verify_y_init(Vst0),

    #t_bs_context{tail_unit=Unit} = get_concrete_type(Ctx, Vst0),

    Vst = prune_x_regs(Live, Vst0),
    extract_term(#t_bitstring{size_unit=Unit}, bs_get_tail, [Ctx], Dst,
                 Vst, Vst0);
vi({bs_start_match4,Fail,Live,Src,Dst}, Vst) ->
    validate_bs_start_match(Fail, Live, Src, Dst, Vst);
vi({test,bs_start_match3,{f,_}=Fail,Live,[Src],Dst}, Vst) ->
    validate_bs_start_match(Fail, Live, Src, Dst, Vst);
vi({test,bs_match_string,{f,Fail},[Ctx,Stride,{string,String}]}, Vst) ->
    true = is_bitstring(String),                %Assertion.
    validate_bs_skip(Fail, Ctx, Stride, Vst);
vi({test,bs_skip_bits2,{f,Fail},[Ctx,Size0,Unit,_Flags]}, Vst) ->
    Size = unpack_typed_arg(Size0, Vst),
    assert_term(Size, Vst),

    Stride = case get_concrete_type(Size, Vst) of
                 #t_integer{elements={Same,Same}} -> Same * Unit;
                 _ -> Unit
             end,

    validate_bs_skip(Fail, Ctx, Stride, Vst);
vi({test,bs_test_tail2,{f,Fail},[Ctx,_Size]}, Vst) ->
    assert_no_exception(Fail),
    assert_type(#t_bs_context{}, Ctx, Vst),
    branch(Fail, Vst);
vi({test,bs_test_unit,{f,Fail},[Ctx,Unit]}, Vst) ->
    assert_type(#t_bs_context{}, Ctx, Vst),

    Type = #t_bs_context{tail_unit=Unit},

    branch(Fail, Vst,
           fun(FailVst) ->
                   update_type(fun subtract/2, Type, Ctx, FailVst)
           end,
           fun(SuccVst0) ->
                   SuccVst = update_bs_unit(Ctx, Unit, SuccVst0),
                   update_type(fun meet/2, Type, Ctx, SuccVst)
           end);
vi({test,bs_skip_utf8,{f,Fail},[Ctx,Live,_]}, Vst) ->
    validate_bs_skip(Fail, Ctx, 8, Live, Vst);
vi({test,bs_skip_utf16,{f,Fail},[Ctx,Live,_]}, Vst) ->
    validate_bs_skip(Fail, Ctx, 16, Live, Vst);
vi({test,bs_skip_utf32,{f,Fail},[Ctx,Live,_]}, Vst) ->
    validate_bs_skip(Fail, Ctx, 32, Live, Vst);
vi({test,bs_get_binary2=Op,{f,Fail},Live,[Ctx,{atom,all},Unit,_],Dst}, Vst) ->
    Type = #t_bitstring{size_unit=Unit},
    validate_bs_get_all(Op, Fail, Ctx, Live, Unit, Type, Dst, Vst);
vi({test,bs_get_binary2=Op,{f,Fail},Live,[Ctx,Size,Unit,_],Dst}, Vst) ->
    Type = #t_bitstring{size_unit=bsm_size_unit(Size, Unit)},
    Stride = bsm_stride(Size, Unit),
    validate_bs_get(Op, Fail, Ctx, Live, Stride, Type, Dst, Vst);
vi({test,bs_get_integer2=Op,{f,Fail},Live,
    [Ctx,{integer,Size},Unit,{field_flags,Flags}],Dst},Vst) ->
    Type = bs_integer_type({Size, Size}, Unit, Flags),
    Stride = Unit * Size,
    validate_bs_get(Op, Fail, Ctx, Live, Stride, Type, Dst, Vst);
vi({test,bs_get_integer2=Op,{f,Fail},Live,[Ctx,Sz0,Unit,{field_flags,Flags}],Dst},Vst) ->
    Sz = unpack_typed_arg(Sz0, Vst),
    Type = case meet(get_term_type(Sz, Vst), #t_integer{}) of
               #t_integer{elements=Bounds} ->
                   bs_integer_type(Bounds, Unit, Flags);
               none ->
                   none
           end,
    validate_bs_get(Op, Fail, Ctx, Live, Unit, Type, Dst, Vst);
vi({test,bs_get_float2=Op,{f,Fail},Live,[Ctx,Size,Unit,_],Dst},Vst) ->
    Stride = bsm_stride(Size, Unit),
    validate_bs_get(Op, Fail, Ctx, Live, Stride, #t_float{}, Dst, Vst);
vi({test,bs_get_utf8=Op,{f,Fail},Live,[Ctx,_],Dst}, Vst) ->
    Type = beam_types:make_integer(0, ?UNICODE_MAX),
    validate_bs_get(Op, Fail, Ctx, Live, 8, Type, Dst, Vst);
vi({test,bs_get_utf16=Op,{f,Fail},Live,[Ctx,_],Dst}, Vst) ->
    Type = beam_types:make_integer(0, ?UNICODE_MAX),
    validate_bs_get(Op, Fail, Ctx, Live, 16, Type, Dst, Vst);
vi({test,bs_get_utf32=Op,{f,Fail},Live,[Ctx,_],Dst}, Vst) ->
    Type = beam_types:make_integer(0, ?UNICODE_MAX),
    validate_bs_get(Op, Fail, Ctx, Live, 32, Type, Dst, Vst);
vi({test,is_lt,{f,Fail},Args0}, Vst) ->
    Args = [unpack_typed_arg(Arg, Vst) || Arg <- Args0],
    validate_src(Args, Vst),
    Types = [get_term_type(Arg, Vst) || Arg <- Args],
    branch(Fail, Vst,
           fun(FailVst) ->
                   infer_relop_types('>=', Args, Types, FailVst)
           end,
           fun(SuccVst) ->
                   infer_relop_types('<', Args, Types, SuccVst)
           end);
vi({test,is_ge,{f,Fail},Args0}, Vst) ->
    Args = [unpack_typed_arg(Arg, Vst) || Arg <- Args0],
    validate_src(Args, Vst),
    Types = [get_term_type(Arg, Vst) || Arg <- Args],
    branch(Fail, Vst,
           fun(FailVst) ->
                   infer_relop_types('<', Args, Types, FailVst)
           end,
           fun(SuccVst) ->
                   infer_relop_types('>=', Args, Types, SuccVst)
           end);
vi({test,_Op,{f,Lbl},Ss}, Vst) ->
    validate_src([unpack_typed_arg(Arg, Vst) || Arg <- Ss], Vst),
    branch(Lbl, Vst);

%%
%% Bit syntax positioning
%%

vi({bs_get_position, Ctx, Dst, Live}, Vst0) ->
    assert_type(#t_bs_context{}, Ctx, Vst0),

    verify_live(Live, Vst0),
    verify_y_init(Vst0),

    #t_bs_context{tail_unit=Unit} = get_concrete_type(Ctx, Vst0),

    Vst1 = prune_x_regs(Live, Vst0),
    Vst = create_term(#t_abstract{kind={ms_position, Unit}},
                       bs_get_position, [Ctx], Dst, Vst1, Vst0),

    mark_current_ms_position(Ctx, Dst, Vst);
vi({bs_set_position, Ctx, Pos}, Vst0) ->
    assert_type(#t_bs_context{}, Ctx, Vst0),
    assert_type(#t_abstract{kind={ms_position,1}}, Pos, Vst0),

    #t_abstract{kind={ms_position,Unit}} = get_concrete_type(Pos, Vst0),
    Vst = override_type(#t_bs_context{tail_unit=Unit}, Ctx, Vst0),

    mark_current_ms_position(Ctx, Pos, Vst);

%%
%% Floating-point instructions (excluding BIFs)
%%
vi({fconv,Src0,{fr,_}=Dst}, Vst) ->
    Src = unpack_typed_arg(Src0, Vst),

    assert_term(Src, Vst),
    branch(?EXCEPTION_LABEL, Vst,
           fun(SuccVst0) ->
                   SuccVst = update_type(fun meet/2, #t_number{}, Src, SuccVst0),
                   set_freg(Dst, SuccVst)
           end);

%%
%% Exception-raising instructions
%%

vi({func_info, {atom, _Mod}, {atom, _Name}, Arity}, Vst) ->
    #vst{current=#st{numy=NumY}} = Vst,
    if
         NumY =:= none ->
            verify_live(Arity, Vst),
            verify_call_args(func_info, Arity, Vst),

            branch(?EXCEPTION_LABEL, Vst, fun kill_state/1);
         NumY =/= none ->
            error({allocated, NumY})
    end;
vi({badmatch,Src}, Vst) ->
    assert_durable_term(Src, Vst),
    branch(?EXCEPTION_LABEL, Vst, fun kill_state/1);
vi({case_end,Src}, Vst) ->
    assert_durable_term(Src, Vst),
    branch(?EXCEPTION_LABEL, Vst, fun kill_state/1);
vi(if_end, Vst) ->
    branch(?EXCEPTION_LABEL, Vst, fun kill_state/1);
vi({try_case_end,Src}, Vst) ->
    assert_durable_term(Src, Vst),
    branch(?EXCEPTION_LABEL, Vst, fun kill_state/1);
vi({badrecord,Src}, Vst) ->
    assert_durable_term(Src, Vst),
    branch(?EXCEPTION_LABEL, Vst, fun kill_state/1);
vi(raw_raise=I, Vst0) ->
    validate_body_call(I, 3, Vst0);

%%
%% Binary construction
%%

vi(bs_init_writable=I, Vst) ->
    validate_body_call(I, 1, Vst);
vi({bs_create_bin,{f,Fail},Heap,Live,Unit,Dst,{list,List0}}, Vst0) ->
    verify_live(Live, Vst0),
    verify_y_init(Vst0),
    List = [unpack_typed_arg(Arg, Vst0) || Arg <- List0],
    verify_create_bin_list(List, Vst0),
    Vst = prune_x_regs(Live, Vst0),
    branch(Fail, Vst,
           fun(FailVst0) ->
                   heap_alloc(0, FailVst0)
           end,
           fun(SuccVst0) ->
                   SuccVst1 = update_create_bin_list(List, SuccVst0),
                   SuccVst = heap_alloc(Heap, SuccVst1),
                   create_term(#t_bitstring{size_unit=Unit}, bs_create_bin, [], Dst,
                               SuccVst)
           end);

vi(_, _) ->
    error(unknown_instruction).

infer_relop_types(Op, Args, Types, Vst) ->
    case infer_relop_types(Op, Types) of
        [] ->
            Vst;
        Infer ->
            Zipped = zip(Args, Infer),
            foldl(fun({V,T}, Acc) ->
                          update_type(fun meet/2, T, V, Acc)
                  end, Vst, Zipped)
    end.

infer_relop_types(Op, [#t_integer{elements=R1},
                       #t_integer{elements=R2}]) ->
    case beam_bounds:infer_relop_types(Op, R1, R2) of
        {NewR1,NewR2} ->
            NewType1 = #t_integer{elements=NewR1},
            NewType2 = #t_integer{elements=NewR2},
            [NewType1,NewType2];
        none ->
            [none, none];
        any ->
            []
    end;
infer_relop_types(Op0, [Type1,Type2]) ->
    Op = case Op0 of
             '<' -> '=<';
             '>' -> '>=';
             _ -> Op0
         end,
    case {infer_get_range(Type1),infer_get_range(Type2)} of
        {none,_}=R ->
            [infer_relop_any(Op, R, Type1),Type2];
        {_,none}=R ->
            [Type1,infer_relop_any(Op, R, Type2)];
        {R1,R2} ->
            case beam_bounds:infer_relop_types(Op, R1, R2) of
                {NewR1,NewR2} ->
                    NewType1 = meet(#t_number{elements=NewR1}, Type1),
                    NewType2 = meet(#t_number{elements=NewR2}, Type2),
                    [NewType1,NewType2];
                none ->
                    [none, none];
                any ->
                    []
            end
    end;
infer_relop_types(_, _) ->
    [].

infer_relop_any('=<', {none,any}, Type) ->
    N = #t_number{},
    meet(N, Type);
infer_relop_any('=<', {none,{_,Max}}, Type) ->
    N = infer_make_number({'-inf',Max}),
    meet(N, Type);
infer_relop_any('>=', {any,none}, Type) ->
    N = #t_number{},
    meet(N, Type);
infer_relop_any('>=', {{_,Max},none}, Type) ->
    N = infer_make_number({'-inf',Max}),
    meet(N, Type);
infer_relop_any('>=', {none,{Min,_}}, Type) when is_integer(Min) ->
    N = #t_number{elements={'-inf',Min}},
    meet(subtract(any, N), Type);
infer_relop_any('=<', {{Min,_},none}, Type) when is_integer(Min) ->
    N = #t_number{elements={'-inf',Min}},
    meet(subtract(any, N), Type);
infer_relop_any(_, _, Type) ->
    Type.

infer_make_number({'-inf','+inf'}) ->
    #t_number{};
infer_make_number({_,_}=R) ->
    #t_number{elements=R}.

infer_get_range(#t_integer{elements=R}) -> R;
infer_get_range(#t_number{elements=R}) -> R;
infer_get_range(_) -> none.

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

%% Tail call.
%%  The stackframe must have a known size and be initialized.
%%  Does not return to the instruction following the call.
validate_tail_call(Deallocate, Func, Live, #vst{current=#st{numy=NumY}}=Vst0) ->
    verify_y_init(Vst0),
    verify_live(Live, Vst0),
    verify_call_args(Func, Live, Vst0),

    case will_call_succeed(Func, Live, Vst0) of
        yes when Deallocate =:= NumY ->
            %% The call cannot fail; we don't need to handle exceptions
            Vst = deallocate(Vst0),
            verify_return(Vst);
        'maybe' when Deallocate =:= NumY ->
            %% The call may fail; make sure we update exception state
            Vst = deallocate(Vst0),
            branch(?EXCEPTION_LABEL, Vst, fun verify_return/1);
        no ->
            %% The compiler is allowed to emit garbage values for "Deallocate"
            %% as we know that it will not be used in this case.
            branch(?EXCEPTION_LABEL, Vst0, fun kill_state/1);
        _ when Deallocate =/= NumY ->
            error({allocated, NumY})
    end.

%% A "plain" call.
%%  The stackframe must be initialized.
%%  The instruction will return to the instruction following the call.
validate_body_call(Func, Live,
                   #vst{current=#st{numy=NumY}}=Vst) when is_integer(NumY)->
    verify_y_init(Vst),
    verify_live(Live, Vst),
    verify_call_args(Func, Live, Vst),

    SuccFun = fun(SuccVst0) ->
                      %% Note that we don't try to infer anything from the
                      %% argument types, as that may cause types to become
                      %% concrete "too early."
                      %%
                      %% See beam_types_SUITE:premature_concretization/1 for
                      %% details.
                      {RetType, _, _} = call_types(Func, Live, SuccVst0),
                      true = RetType =/= none,  %Assertion.

                      SuccVst = schedule_out(0, SuccVst0),

                      create_term(RetType, call, [], {x,0}, SuccVst)
              end,

    case will_call_succeed(Func, Live, Vst) of
        yes ->
            SuccFun(Vst);
        'maybe' ->
            branch(?EXCEPTION_LABEL, Vst, SuccFun);
        no ->
            branch(?EXCEPTION_LABEL, Vst, fun kill_state/1)
    end;
validate_body_call(_, _, #vst{current=#st{numy=NumY}}) ->
    error({allocated, NumY}).

init_try_catch_branch(Kind, Dst, Fail, Vst0) ->
    assert_no_exception(Fail),

    Tag = {Kind, [Fail]},
    Vst = create_tag(Tag, 'try_catch', [], Dst, Vst0),

    #vst{current=St0} = Vst,
    #st{ct=Tags}=St0,
    St = St0#st{ct=[Tag|Tags]},
 
    Vst#vst{current=St}.

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
    branch(Lbl, Vst).

verify_get_map(Fail, Src, List, Vst0) ->
    assert_no_exception(Fail),
    assert_not_literal(Src),                    %OTP 22.
    assert_type(#t_map{}, Src, Vst0),

    branch(Fail, Vst0,
           fun(FailVst) ->
                   clobber_map_vals(List, Src, FailVst)
           end,
           fun(SuccVst) ->
                   Keys = extract_map_keys(List, SuccVst),
                   assert_unique_map_keys(Keys),
                   extract_map_vals(List, Src, SuccVst)
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
clobber_map_vals([Key0, Dst | T], Map, Vst0) ->
    Key = unpack_typed_arg(Key0, Vst0),
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

extract_map_keys([Key,_Val | T], Vst) ->
    [unpack_typed_arg(Key, Vst) | extract_map_keys(T, Vst)];
extract_map_keys([], _Vst) ->
    [].


extract_map_vals(List, Src, SuccVst) ->
    Seen = sets:new(),
    extract_map_vals(List, Src, Seen, SuccVst, SuccVst).

extract_map_vals([Key0, Dst | Vs], Map, Seen0, Vst0, Vsti0) ->
    case sets:is_element(Dst, Seen0) of
        true ->
            %% The destinations must not overwrite each other.
            error(conflicting_destinations);
        false ->
            Key = unpack_typed_arg(Key0, Vsti0),
            assert_term(Key, Vst0),
            case bif_types(map_get, [Key, Map], Vst0) of
                {none, _, _} ->
                    kill_state(Vsti0);
                {DstType, _, _} ->
                    Vsti = extract_term(DstType, {bif,map_get},
                                        [Key, Map], Dst, Vsti0),
                    Seen = sets:add_element(Dst, Seen0),
                    extract_map_vals(Vs, Map, Seen, Vst0, Vsti)
            end
    end;
extract_map_vals([], _Map, _Seen, _Vst0, Vst) ->
    Vst.

verify_put_map(Op, Fail, Src, Dst, Live, List, Vst0) ->
    assert_type(#t_map{}, Src, Vst0),
    verify_live(Live, Vst0),
    verify_y_init(Vst0),

    _ = [assert_term(Term, Vst0) || Term <- List],
    Vst = heap_alloc(0, Vst0),

    SuccFun = fun(SuccVst0) ->
                      SuccVst = prune_x_regs(Live, SuccVst0),
                      Keys = extract_map_keys(List, SuccVst),
                      assert_unique_map_keys(Keys),

                      Type = put_map_type(Src, List, Vst),
                      create_term(Type, Op, [Src], Dst, SuccVst, SuccVst0)
              end,

    case Op of
        put_map_exact ->
            branch(Fail, Vst, SuccFun);
        put_map_assoc ->
            %% This instruction cannot fail.
            ?EXCEPTION_LABEL = Fail,            %Assertion.
            SuccFun(Vst)
    end.

put_map_type(Map0, List, Vst) ->
    Map = get_term_type(Map0, Vst),
    pmt_1(List, Vst, Map).

pmt_1([Key0, Value0 | List], Vst, Acc0) ->
    Key = get_term_type(Key0, Vst),
    Value = get_term_type(Value0, Vst),
    {Acc, _, _} = beam_call_types:types(maps, put, [Key, Value, Acc0]),
    pmt_1(List, Vst, Acc);
pmt_1([], _Vst, Acc) ->
    Acc.

verify_update_record(Size, Src0, Dst, List0, Vst0) ->
    Src = unpack_typed_arg(Src0, Vst0),
    List = [unpack_typed_arg(Arg, Vst0) || Arg <- List0],
    assert_type(#t_tuple{exact=true,size=Size}, Src, Vst0),
    verify_y_init(Vst0),

    Vst = eat_heap(Size + 1, Vst0),

    case update_tuple_type(List, Src, Vst) of
        none -> error(invalid_index);
        Type -> create_term(Type, update_record, [], Dst, Vst)
    end.

update_tuple_type([_|_]=Updates0, Src, Vst) ->
    Filter = #t_tuple{size=update_tuple_highest_index(Updates0, -1)},
    case meet(get_term_type(Src, Vst), Filter) of
        none ->
            none;
        TupleType ->
            Updates = update_tuple_type_1(Updates0, Vst),
            beam_types:update_tuple(TupleType, Updates)
    end.

update_tuple_type_1([Index, Value | Updates], Vst) ->
    [{Index, get_term_type(Value, Vst)} | update_tuple_type_1(Updates, Vst)];
update_tuple_type_1([], _Vst) ->
    [].

update_tuple_highest_index([Index, _Val | List], Acc) when is_integer(Index) ->
    update_tuple_highest_index(List, max(Index, Acc));
update_tuple_highest_index([], Acc) when Acc >= 1 ->
    Acc.

verify_create_bin_list([{atom,string},_Seg,Unit,Flags,Val,Size|Args], Vst) ->
    assert_bs_unit({atom,string}, Unit),
    assert_term(Flags, Vst),
    case Val of
        {string,Bs} when is_binary(Bs) -> ok;
        _ -> error({not_string,Val})
    end,
    assert_term(Flags, Vst),
    assert_term(Size, Vst),
    verify_create_bin_list(Args, Vst);
verify_create_bin_list([Type,_Seg,Unit,Flags,Val,Size|Args], Vst) ->
    assert_term(Type, Vst),
    assert_bs_unit(Type, Unit),
    assert_term(Flags, Vst),
    assert_term(Val, Vst),
    assert_term(Size, Vst),
    verify_create_bin_list(Args, Vst);
verify_create_bin_list([], _Vst) -> ok.

update_create_bin_list([{atom,string},_Seg,_Unit,_Flags,_Val,_Size|T], Vst) ->
    update_create_bin_list(T, Vst);
update_create_bin_list([{atom,Op},_Seg,_Unit,_Flags,Val,_Size|T], Vst0) ->
    Type = update_create_bin_type(Op),
    Vst = update_type(fun meet/2, Type, Val, Vst0),
    update_create_bin_list(T, Vst);
update_create_bin_list([], Vst) -> Vst.

update_create_bin_type(append) -> #t_bitstring{};
update_create_bin_type(private_append) -> #t_bitstring{};
update_create_bin_type(binary) -> #t_bitstring{};
update_create_bin_type(float) -> #t_number{};
update_create_bin_type(integer) -> #t_integer{};
update_create_bin_type(utf8) -> #t_integer{};
update_create_bin_type(utf16) -> #t_integer{};
update_create_bin_type(utf32) -> #t_integer{}.

assert_bs_unit({atom,Type}, 0) ->
    case Type of
        utf8 -> ok;
        utf16 -> ok;
        utf32 -> ok;
        _ -> error({zero_unit_invalid_for_type,Type})
    end;
assert_bs_unit({atom,_Type}, Unit) when is_integer(Unit), 0 < Unit, Unit =< 256 ->
    ok;
assert_bs_unit(_, Unit) ->
    error({invalid,Unit}).

%%
%% Common code for validating returns, whether naked or as part of a tail call.
%%

verify_return(#vst{current=#st{numy=NumY}}) when NumY =/= none ->
    error({stack_frame,NumY});
verify_return(#vst{current=#st{recv_state=State}}) when State =/= none ->
    %% Returning in the middle of a receive loop will ruin the next receive.
    error({return_in_receive,State});
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

validate_bif(Kind, Op, Fail, Ss, Dst, OrigVst, Vst) ->
    validate_bif_sources(Op, Ss, Vst),
    case will_bif_succeed(Op, Ss, Vst) of
        yes ->
            %% This BIF cannot fail (neither throw nor branch), make sure it's
            %% handled without updating exception state.
            validate_bif_1(Kind, Op, cannot_fail, Ss, Dst, OrigVst, Vst);
        no ->
            %% This BIF always fails; jump directly to the fail block or
            %% exception handler.
            branch(Fail, Vst, fun kill_state/1);
        'maybe' ->
            validate_bif_1(Kind, Op, Fail, Ss, Dst, OrigVst, Vst)
    end.

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

validate_bif_sources(Op, Ss, Vst)
  when Op =:= bit_size;
       Op =:= byte_size ->
    %% These BIFs accept both match contexts and bitstrings
    _ = [assert_movable(S, Vst) || S <- Ss],
    ok;
validate_bif_sources(_, Ss, Vst) ->
    validate_src(Ss, Vst).

%%
%% Common code for validating bs_start_match* instructions.
%%

validate_bs_start_match({atom,resume}, Live, Src, Dst, Vst0) ->
    assert_type(#t_bs_context{}, Src, Vst0),
    verify_live(Live, Vst0),
    verify_y_init(Vst0),

    Vst = prune_x_regs(Live, Src, Vst0),
    assign(Src, Dst, Vst);
validate_bs_start_match({atom,no_fail}, Live, Src, Dst, Vst0) ->
    verify_live(Live, Vst0),
    verify_y_init(Vst0),

    Vst1 = update_type(fun meet/2, #t_bs_matchable{}, Src, Vst0),

    %% Retain the current unit, if known.
    SrcType = get_movable_term_type(Src, Vst1),
    TailUnit = beam_types:get_bs_matchable_unit(SrcType),

    Vst = prune_x_regs(Live, Src, Vst1),
    extract_term(#t_bs_context{tail_unit=TailUnit}, bs_start_match,
                 [Src], Dst, Vst, Vst0);
validate_bs_start_match({f,Fail}, Live, Src, Dst, Vst) ->
    assert_no_exception(Fail),

    branch(Fail, Vst,
           fun(FailVst) ->
                   update_type(fun subtract/2, #t_bs_matchable{}, Src, FailVst)
           end,
           fun(SuccVst) ->
                   validate_bs_start_match({atom,no_fail}, Live,
                                           Src, Dst, SuccVst)
           end).

%%
%% Validate the bs_match instruction.
%%

validate_bs_match([I|Is], Ctx, Ensured, Unit0, Vst0) ->
    case I of
        {ensure_at_least,Size,Unit} ->
            Type = #t_bs_context{tail_unit=Unit},
            Vst1 = update_bs_unit(Ctx, Unit, Vst0),
            Vst = update_type(fun meet/2, Type, Ctx, Vst1),
            validate_bs_match(Is, Ctx, Size, Unit, Vst);
        {ensure_exactly, Size} ->
            validate_bs_match(Is, Ctx, Size, Unit0, Vst0);
        {'=:=',nil,Bits,Value} when Bits =< 64, is_integer(Value) ->
            validate_bs_match(Is,
                              Ctx,
                              consume_bits(I, Bits, Ensured),
                              Unit0,
                              Vst0);
        {Type0,Live,{literal,Flags},Size,Unit,Dst} when Type0 =:= binary;
                                                        Type0 =:= integer ->
            true = is_integer(Size), %Assertion.
            validate_ctx_live(Ctx, Live),
            verify_live(Live, Vst0),
            Vst1 = prune_x_regs(Live, Vst0),
            Type = case Type0 of
                       integer ->
                           bs_integer_type({Size, Size}, Unit, Flags);
                       binary ->
                           SizeUnit = bsm_size_unit({integer,Size}, Unit),
                           #t_bitstring{size_unit=SizeUnit}
                   end,
            Vst = extract_term(Type, bs_match, [Ctx], Dst, Vst1, Vst0),
            validate_bs_match(Is,
                              Ctx,
                              consume_bits(I, Size, Ensured),
                              Unit0,
                              Vst);
        {skip, Size} ->
            validate_bs_match(Is,
                              Ctx,
                              consume_bits(I, Size, Ensured),
                              Unit0,
                              Vst0);
        {get_tail,Live,_,Dst} ->
            validate_ctx_live(Ctx, Live),
            verify_live(Live, Vst0),
            Vst1 = prune_x_regs(Live, Vst0),
            #t_bs_context{tail_unit=Unit} = get_concrete_type(Ctx, Vst0),
            Type = #t_bitstring{size_unit=Unit},
            Vst = extract_term(Type, get_tail, [Ctx], Dst, Vst1, Vst0),
            %% In rare circumstances, there can be multiple `get_tail` sub
            %% commands.
            validate_bs_match(Is, Ctx, 0, Unit, Vst)
    end;
validate_bs_match([], _Ctx, _Ensured, _Unit, Vst) ->
    Vst.

consume_bits(I, Size, Ensured) when Size > Ensured ->
    error({insufficient_bits, I, Size, Ensured});
consume_bits(_I, Size, Ensured) ->
    Ensured - Size.

validate_ctx_live({x,X}=Ctx, Live) when X >= Live ->
    error({live_does_not_preserve_context,Live,Ctx});
validate_ctx_live(_, _) ->
    ok.

validate_failed_bs_match([{ensure_at_least,_Size,Unit}|_], Ctx, Vst) ->
    Type = #t_bs_context{tail_unit=Unit},
    update_type(fun subtract/2, Type, Ctx, Vst);
validate_failed_bs_match([_|Is], Ctx, Vst) ->
    validate_failed_bs_match(Is, Ctx, Vst);
validate_failed_bs_match([], _Ctx, Vst) ->
    Vst.

bs_integer_type(Bounds, Unit, Flags) ->
    case beam_bounds:bounds('*', Bounds, {Unit, Unit}) of
        {_, MaxBits} when is_integer(MaxBits), MaxBits >= 1, MaxBits =< 64 ->
            case member(signed, Flags) of
                true ->
                    Max = (1 bsl (MaxBits - 1)) - 1,
                    Min = -(Max + 1),
                    beam_types:make_integer(Min, Max);
                false ->
                    Max = (1 bsl MaxBits) - 1,
                    beam_types:make_integer(0, Max)
            end;
        {_, 0} ->
            beam_types:make_integer(0);
        _ ->
            case member(signed, Flags) of
                true -> #t_integer{};
                false -> #t_integer{elements={0,'+inf'}}
            end
    end.

%%
%% Common code for validating bs_get* instructions.
%%

validate_bs_get(_Op, Fail, Ctx0, Live, _Stride, none, _Dst, Vst) ->
    Ctx = unpack_typed_arg(Ctx0, Vst),
    validate_bs_get_1(
      Fail, Ctx, Live, Vst,
      fun(SuccVst) ->
              kill_state(SuccVst)
      end);
validate_bs_get(Op, Fail, Ctx0, Live, Stride, Type, Dst, Vst) ->
    Ctx = unpack_typed_arg(Ctx0, Vst),
    validate_bs_get_1(
      Fail, Ctx, Live, Vst,
      fun(SuccVst0) ->
              SuccVst1 = advance_bs_context(Ctx, Stride, SuccVst0),
              SuccVst = prune_x_regs(Live, SuccVst1),
              extract_term(Type, Op, [Ctx], Dst, SuccVst, SuccVst0)
      end).

validate_bs_get_all(Op, Fail, Ctx0, Live, Stride, Type, Dst, Vst) ->
    Ctx = unpack_typed_arg(Ctx0, Vst),
    validate_bs_get_1(
      Fail, Ctx, Live, Vst,
      fun(SuccVst0) ->
              %% This acts as an implicit unit test on the current match
              %% position, so we'll update the unit in case we rewind here
              %% later on.
              SuccVst1 = update_bs_unit(Ctx, Stride, SuccVst0),

              SuccVst2 = advance_bs_context(Ctx, Stride, SuccVst1),
              SuccVst = prune_x_regs(Live, SuccVst2),
              extract_term(Type, Op, [Ctx], Dst, SuccVst, SuccVst0)
      end).

validate_bs_get_1(Fail, Ctx, Live, Vst, SuccFun) ->
    assert_no_exception(Fail),

    assert_type(#t_bs_context{}, Ctx, Vst),
    verify_live(Live, Vst),
    verify_y_init(Vst),

    branch(Fail, Vst, SuccFun).

%%
%% Common code for validating bs_skip* instructions.
%%
validate_bs_skip(Fail, Ctx, Stride, Vst) ->
    validate_bs_skip(Fail, Ctx, Stride, no_live, Vst).

validate_bs_skip(Fail, Ctx, Stride, Live, Vst) ->
    assert_no_exception(Fail),

    assert_type(#t_bs_context{}, Ctx, Vst),

    validate_bs_skip_1(Fail, Ctx, Stride, Live, Vst).

validate_bs_skip_1(Fail, Ctx, Stride, no_live, Vst) ->
    branch(Fail, Vst,
           fun(SuccVst) ->
                   advance_bs_context(Ctx, Stride, SuccVst)
           end);
validate_bs_skip_1(Fail, Ctx, Stride, Live, Vst) ->
    verify_y_init(Vst),
    verify_live(Live, Vst),
    branch(Fail, Vst,
           fun(SuccVst0) ->
                   SuccVst = advance_bs_context(Ctx, Stride, SuccVst0),
                   prune_x_regs(Live, SuccVst)
           end).

advance_bs_context(_Ctx, 0, Vst) ->
    %% We _KNOW_ we're not moving anywhere. Retain our current position and
    %% type.
    Vst;
advance_bs_context(_Ctx, Stride, _Vst) when Stride < 0 ->
    %% We _KNOW_ we'll fail at runtime.
    throw({invalid_argument, {negative_stride, Stride}});
advance_bs_context(Ctx, Stride, Vst0) ->
    Vst = update_type(fun join/2, #t_bs_context{tail_unit=Stride}, Ctx, Vst0),

    %% The latest saved position (if any) is no longer current, make sure
    %% it isn't updated on the next match operation.
    invalidate_current_ms_position(Ctx, Vst).

%% Updates the unit of our latest saved position, if it's current.
update_bs_unit(Ctx, Unit, #vst{current=St}=Vst) ->
    CtxRef = get_reg_vref(Ctx, Vst),

    case St#st.ms_positions of
        #{ CtxRef := PosRef } ->
            PosType = #t_abstract{kind={ms_position, Unit}},
            update_type(fun meet/2, PosType, PosRef, Vst);
        #{} ->
            Vst
    end.

mark_current_ms_position(Ctx, Pos, #vst{current=St0}=Vst) ->
    CtxRef = get_reg_vref(Ctx, Vst),
    PosRef = get_reg_vref(Pos, Vst),

    #st{ms_positions=MsPos0} = St0,

    MsPos = MsPos0#{ CtxRef => PosRef },

    St = St0#st{ ms_positions=MsPos },
    Vst#vst{current=St}.

invalidate_current_ms_position(Ctx, #vst{current=St0}=Vst) ->
    CtxRef = get_reg_vref(Ctx, Vst),
    #st{ms_positions=MsPos0} = St0,

    case MsPos0 of
        #{ CtxRef := _ } ->
            MsPos = maps:remove(CtxRef, MsPos0),
            St = St0#st{ms_positions=MsPos},
            Vst#vst{current=St};
        #{} ->
            Vst
    end.

%%
%% Common code for is_$type instructions.
%%
type_test(Fail, Type, Reg0, Vst) ->
    Reg = unpack_typed_arg(Reg0, Vst),

    assert_term(Reg, Vst),
    assert_no_exception(Fail),
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

verify_call_args(_, 0, #vst{}) ->
    ok;
verify_call_args({f,Lbl}, Live, #vst{ft=Ft}=Vst) ->
    case Ft of
        #{ Lbl := FuncInfo } ->
            #{ arity := Live,
               parameter_info := ParamInfo } = FuncInfo,
            verify_local_args(Live - 1, ParamInfo, #{}, Vst);
        #{} ->
            error(local_call_to_unknown_function)
    end;
verify_call_args(_, Live, Vst) ->
    verify_remote_args_1(Live - 1, Vst).

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
                    verify_arg_type(Reg, Type, ParamInfo, Vst),
                    verify_local_args(X - 1, ParamInfo,
                                      CtxRefs#{ VRef => Reg }, Vst)
            end;
        Type ->
            verify_arg_type(Reg, Type, ParamInfo, Vst),
            verify_local_args(X - 1, ParamInfo, CtxRefs, Vst)
    end.

verify_arg_type(Reg, GivenType, ParamInfo, Vst) ->
    case {ParamInfo, GivenType} of
        {#{ Reg := Info }, #t_bs_context{}} ->
            %% Match contexts require explicit support, and may not be passed
            %% to a function that accepts arbitrary terms.
            case member(accepts_match_context, Info) of
                true -> verify_arg_type_1(Reg, GivenType, Info, Vst);
                false -> error(no_bs_start_match2)
            end;
        {_, #t_bs_context{}} ->
            error(no_bs_start_match2);
        {#{ Reg := Info }, _} ->
            verify_arg_type_1(Reg, GivenType, Info, Vst);
        {#{}, _} ->
            ok
    end.

verify_arg_type_1(Reg, GivenType, Info, Vst) ->
    RequiredType = proplists:get_value(type, Info, any),
    case meet(GivenType, RequiredType) of
        Type when Type =/= none, Vst#vst.level =:= weak ->
            %% Strictly speaking this should always match GivenType, but
            %% beam_jump:share/1 sometimes joins blocks in a manner that makes
            %% it very difficult to accurately reconstruct type information,
            %% for example:
            %%
            %%        bug({Tag, _, _} = Key) when Tag == a; Tag == b ->
            %%            foo(Key);
            %%        bug({Tag, _} = Key) when Tag == a; Tag == b ->
            %%            foo(Key).
            %%
            %%        foo(I) -> I.
            %%
            %% At the first call to foo/1, we know that we have either `{a,_}`
            %% or `{b,_}`, and at the second call `{a,_,_}` or `{b,_,_}`.
            %%
            %% When both calls to foo/1 are joined into the same block, all we
            %% know is that we have tuple with an arity of at least 2, whose
            %% first element is `a` or `b`.
            %%
            %% Fixing this properly is a big undertaking, so for now we've
            %% decided on a compromise that splits validation into two steps.
            %%
            %% We run a 'strong' pass directly after code generation in which
            %% arguments must be at least as narrow as expected, and a 'weak'
            %% pass after all optimizations have been applied in which we
            %% tolerate arguments that aren't in direct conflict.
            ok;
        GivenType ->
            true = GivenType =/= none,          %Assertion.
            ok;
        _ ->
            error({bad_arg_type, Reg, GivenType, RequiredType})
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
    Ys = #{Reg => Val || {y,Y}=Reg := Val <- Ys0, Y < To},
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
    {HeapWords, Floats, Funs} = heap_alloc_1(Heap),

    St = St0#st{h=HeapWords,hf=Floats,hl=Funs},

    Vst#vst{current=St}.

heap_alloc_1({alloc, Alloc}) ->
    heap_alloc_2(Alloc, 0, 0, 0);
heap_alloc_1(HeapWords) when is_integer(HeapWords) ->
    {HeapWords, 0, 0}.

heap_alloc_2([{words, HeapWords} | T], 0, Floats, Funs) ->
    heap_alloc_2(T, HeapWords, Floats, Funs);
heap_alloc_2([{floats, Floats} | T], HeapWords, 0, Funs) ->
    heap_alloc_2(T, HeapWords, Floats, Funs);
heap_alloc_2([{funs, Funs} | T], HeapWords, Floats, 0) ->
    heap_alloc_2(T, HeapWords, Floats, Funs);
heap_alloc_2([], HeapWords, Floats, Funs) ->
    {HeapWords, Floats, Funs}.

schedule_out(Live, Vst0) when is_integer(Live) ->
    Vst1 = prune_x_regs(Live, Vst0),
    Vst2 = kill_heap_allocation(Vst1),
    Vst = kill_fregs(Vst2),
    update_receive_state(none, Vst).

prune_x_regs(Live, Vst) when is_integer(Live) ->
    prune_x_regs(Live, [], Vst).

prune_x_regs(Live, Preserved, #vst{current=St0}=Vst) when is_integer(Live) ->
    #st{fragile=Fragile0,xs=Xs0} = St0,
    Fragile = sets:filter(fun({x,X}=Reg) ->
                                  X < Live orelse Reg =:= Preserved;
                             ({y,_}) ->
                                  true
                         end, Fragile0),
    Xs = #{Reg => Val || {x,X}=Reg := Val <- Xs0,
           (X < Live orelse X =:= Preserved)},
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

is_float_arith_bif(fadd, [_, _]) -> true;
is_float_arith_bif(fdiv, [_, _]) -> true;
is_float_arith_bif(fmul, [_, _]) -> true;
is_float_arith_bif(fnegate, [_]) -> true;
is_float_arith_bif(fsub, [_, _]) -> true;
is_float_arith_bif(_, _) -> false.

validate_float_arith_bif(Ss, Dst, Vst) ->
    _ = [assert_freg_set(S, Vst) || S <- Ss],
    set_freg(Dst, Vst).

init_fregs() -> 0.

kill_fregs(#vst{current=St0}=Vst) ->
    St = St0#st{f=init_fregs()},
    Vst#vst{current=St}.

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

bsm_stride({integer, Size}, Unit) ->
    Size * Unit;
bsm_stride(_Size, Unit) ->
    Unit.

bsm_size_unit({integer, Size}, Unit) ->
    max(1, Size) * max(1, Unit);
bsm_size_unit(_Size, Unit) ->
    max(1, Unit).

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
%% Validate debug information in `debug_line` instructions.
%%

validate_debug_line({entry,Args}, Live, Vst) ->
    do_validate_debug_line(none, Live, Vst),
    _ = [get_term_type(Reg, Vst) || {_Name,[Reg]} <:- Args],
    prune_x_regs(Live, Vst);
validate_debug_line({Stk,Vars}, Live, Vst0) ->
    do_validate_debug_line(Stk, Live, Vst0),
    Vst = prune_x_regs(Live, Vst0),
    _ = [validate_dbg_vars(Regs, Name, Vst) || {Name,Regs} <:- Vars],
    Vst.

do_validate_debug_line(ExpectedStk, Live, #vst{current=St}=Vst) ->
    case St of
        #st{numy=ExpectedStk} ->
            ok;
        #st{numy=ActualStk} ->
            error({beam_debug_info,frame_size,ExpectedStk,actual,ActualStk})
    end,
    verify_live(Live, Vst),
    verify_y_init(Vst).

validate_dbg_vars([R|Rs], Name, Vst) ->
    Type = get_term_type(R, Vst),
    validate_dbg_vars(Rs, Type, Name, Vst).

validate_dbg_vars([R|Rs], Type, Name, Vst) ->
    case get_term_type(R, Vst) of
        Type ->
            validate_dbg_vars(Rs, Type, Name, Vst);
        OtherType ->
            error({type_mismatch,Name,OtherType,Type})
    end;
validate_dbg_vars([], _Type, _Name, _Vst) ->
    ok.

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
            Vst = infer_types_1(LEntry, canonical_value(RHS, Vst0),
                                CompareOp, Vst0),
            infer_types_1(REntry, canonical_value(LHS, Vst),
                          CompareOp, Vst);
        #{ LHS := LEntry } ->
            infer_types_1(LEntry, canonical_value(RHS, Vst0), CompareOp, Vst0);
        #{ RHS := REntry } ->
            infer_types_1(REntry, canonical_value(LHS, Vst0), CompareOp, Vst0);
        #{} ->
            Vst0
    end.

infer_types_1(#value{op={bif,'and'},args=[LHS,RHS]}, Val, Op, Vst0) ->
    case Val of
        {atom, Bool} when Op =:= eq_exact, Bool; Op =:= ne_exact, not Bool ->
            Vst = update_eq_types(LHS, {atom,true}, Vst0),
            update_eq_types(RHS, {atom,true}, Vst);
        _ ->
            %% As either of the arguments could be 'false', we can't infer
            %% anything useful from that result.
            Vst0
    end;
infer_types_1(#value{op={bif,'or'},args=[LHS,RHS]}, Val, Op, Vst0) ->
    case Val of
        {atom, Bool} when Op =:= eq_exact, not Bool; Op =:= ne_exact, Bool ->
            Vst = update_eq_types(LHS, {atom,false}, Vst0),
            update_eq_types(RHS, {atom,false}, Vst);
        _ ->
            %% As either of the arguments could be 'true', we can't infer
            %% anything useful from that result.
            Vst0
    end;
infer_types_1(#value{op={bif,'not'},args=[Arg]}, Val, Op, Vst0) ->
    case Val of
        {atom, Bool} when Op =:= eq_exact, Bool; Op =:= ne_exact, not Bool ->
            update_eq_types(Arg, {atom,false}, Vst0);
        {atom, Bool} when Op =:= eq_exact, not Bool; Op =:= ne_exact, Bool ->
            update_eq_types(Arg, {atom,true}, Vst0);
        _ ->
            Vst0
    end;
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
        {atom, Bool} when Op =:= eq_exact, Bool; Op =:= ne_exact, not Bool ->
            update_ne_types(LHS, RHS, Vst);
        {atom, Bool} when Op =:= ne_exact, Bool; Op =:= eq_exact, not Bool ->
            update_eq_types(LHS, RHS, Vst);
        _ ->
            Vst
    end;
infer_types_1(#value{op={bif,RelOp},args=[_,_]=Args}, Val, Op, Vst)
  when RelOp =:= '<'; RelOp =:= '=<'; RelOp =:= '>='; RelOp =:= '>' ->
    case Val of
        {atom, Bool} when Op =:= eq_exact, Bool; Op =:= ne_exact, not Bool ->
            Types = [get_term_type(Arg, Vst) || Arg <- Args],
            infer_relop_types(RelOp, Args, Types, Vst);
        {atom, Bool} when Op =:= ne_exact, Bool; Op =:= eq_exact, not Bool ->
            Types = [get_term_type(Arg, Vst) || Arg <- Args],
            infer_relop_types(invert_relop(RelOp), Args, Types, Vst);
        _ ->
            Vst
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
infer_types_1(#value{op={bif,is_function},args=[Src,{integer,Arity}]},
              Val, Op, Vst)
  when Arity >= 0, Arity =< ?MAX_FUNC_ARGS ->
    infer_type_test_bif(#t_fun{arity=Arity}, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,is_function},args=[Src,_Arity]}, Val, Op, Vst) ->
    case Val of
        {atom, Bool} when Op =:= eq_exact, Bool; Op =:= ne_exact, not Bool ->
            update_type(fun meet/2, #t_fun{}, Src, Vst);
        _ ->
            %% We cannot subtract the function type when the arity is unknown:
            %% `Src` may still be a function if the arity is outside the
            %% allowed range.
            Vst
    end;
infer_types_1(#value{op={bif,is_function},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(#t_fun{}, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,is_integer},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(#t_integer{}, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,is_list},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(#t_list{}, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,is_map},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(#t_map{}, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,is_number},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(#t_number{}, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,is_pid},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(pid, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,is_port},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(port, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,is_reference},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(reference, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,is_tuple},args=[Src]}, Val, Op, Vst) ->
    infer_type_test_bif(#t_tuple{}, Src, Val, Op, Vst);
infer_types_1(#value{op={bif,tuple_size}, args=[Tuple]},
              {integer,Arity}, Op, Vst) ->
    Type = #t_tuple{exact=true,size=Arity},
    case Op of
        eq_exact -> update_type(fun meet/2, Type, Tuple, Vst);
        ne_exact -> update_type(fun subtract/2, Type, Tuple, Vst)
    end;
infer_types_1(#value{op={bif,element},args=[{integer,Index}, Tuple]},
              Val, eq_exact, Vst)
  when Index >= 1 ->
    ValType = get_term_type(Val, Vst),
    Es = beam_types:set_tuple_element(Index, ValType, #{}),
    TupleType = #t_tuple{size=Index,elements=Es},
    update_type(fun meet/2, TupleType, Tuple, Vst);
infer_types_1(#value{op={bif,element},args=[{integer,Index}, Tuple]},
              Val, ne_exact, Vst)
  when Index >= 1 ->
    %% Subtraction is only safe with singletons, see update_ne_types/3 for
    %% details.
    ValType = get_term_type(Val, Vst),
    case beam_types:is_singleton_type(ValType) of
        true ->
            case beam_types:set_tuple_element(Index, ValType, #{}) of
                #{ Index := _ }=Es ->
                    TupleType = #t_tuple{size=Index,elements=Es},
                    update_type(fun subtract/2, TupleType, Tuple, Vst);
                #{} ->
                    %% Index was above the element limit; subtraction is not
                    %% safe.
                    Vst
            end;
        false ->
            Vst
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

invert_relop('<') -> '>=';
invert_relop('=<') -> '>';
invert_relop('>=') -> '<';
invert_relop('>') -> '=<'.

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
update_type(Merge, With, #value_ref{}=Ref, Vst0) ->
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
    Current = get_concrete_type(Ref, Vst0),
    case Merge(Current, With) of
        none ->
            throw({type_conflict, Current, With});
        Current ->
            Vst0;
        Type ->
            Vst = update_container_type(Type, Ref, Vst0),
            set_type(Type, Ref, Vst)
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

%% Updates the container the given value was extracted from, if any.
update_container_type(Type, Ref, #vst{current=#st{vs=Vs}}=Vst) ->
    case Vs of
        #{ Ref := #value{op={bif,element},
                         args=[{integer,Index},Tuple]} } when Index >= 1 ->
            case {Index,Type} of
                {1,#t_atom{elements=[_,_|_]}} ->
                    %% The first element is one atom out of a set of
                    %% at least two atoms. We must take care to
                    %% construct an atom set.
                    update_type(fun meet_tuple_set/2, Type, Tuple, Vst);
                {_,_} ->
                    Es = beam_types:set_tuple_element(Index, Type, #{}),
                    TupleType = #t_tuple{size=Index,elements=Es},
                    update_type(fun meet/2, TupleType, Tuple, Vst)
            end;
        #{} ->
            Vst
    end.

meet_tuple_set(Type, #t_atom{elements=Atoms}) ->
    %% Try to create a tuple set out of the known atoms for the first element.
    #t_tuple{size=Size,exact=Exact} = normalize(meet(Type, #t_tuple{})),
    Tuples = [#t_tuple{size=Size,exact=Exact,
                       elements=#{1 => #t_atom{elements=[A]}}} ||
                 A <- Atoms],
    TupleSet = foldl(fun join/2, hd(Tuples), tl(Tuples)),
    meet(Type, TupleSet).

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
    %% Consider `#number{} =/= #t_integer{}`; all we know is that LHS isn't equal
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
            case canonical_value(LHS, Vst) of
                LHS -> Vst;
                Value -> infer_types(eq_exact, LHS, Value, Vst)
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
    end;
set_reg_vref(_Ref, Dst, _Vst) ->
    error({invalid_register, Dst}).

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
    end;
get_reg_vref(Src, _Vst) ->
    error({invalid_register, Src}).

set_type(Type, #value_ref{}=Ref, #vst{current=#st{vs=Vs0}=St}=Vst) ->
    #{ Ref := #value{}=Entry } = Vs0,
    Vs = Vs0#{ Ref => Entry#value{type=Type} },
    Vst#vst{current=St#st{vs=Vs}}.

new_value(none, _, _, _) ->
    error(creating_none_value);
new_value(Type, Op, Ss, #vst{current=#st{vs=Vs0}=St,ref_ctr=Counter}=Vst) ->
    Ref = #value_ref{id=Counter},
    Vs = Vs0#{ Ref => #value{op=Op,args=Ss,type=Type} },

    {Ref, Vst#vst{current=St#st{vs=Vs},ref_ctr=Counter+1}}.

kill_catch_tag(Reg, #vst{current=#st{ct=[Tag|Tags]}=St}=Vst0) ->
    Vst = Vst0#vst{current=St#st{ct=Tags}},
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

assert_no_exception(?EXCEPTION_LABEL) ->
    error(throws_exception);
assert_no_exception(_) ->
    ok.

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

canonical_value(Val, Vst) ->
    Type = get_term_type(Val, Vst),
    case beam_types:is_singleton_type(Type) of
        true ->
            case beam_types:get_singleton_value(Type) of
                {ok, Res} -> value_to_literal(Res);
                error -> Val
            end;
        false ->
            Val
    end.

%% These are just wrappers around their equivalents in beam_types, which
%% handle the validator-specific #t_abstract{} type.
%%
%% The funny-looking abstract types produced here are intended to provoke
%% errors on actual use; they do no harm just lying around.

normalize(#t_abstract{}=A) -> error({abstract_type, A});
normalize(T) -> beam_types:normalize(T).

join(Same, Same) ->
    Same;
join(#t_abstract{kind={ms_position, UnitA}},
     #t_abstract{kind={ms_position, UnitB}}) ->
    #t_abstract{kind={ms_position, gcd(UnitA, UnitB)}};
join(#t_abstract{}=A, B) ->
    #t_abstract{kind={join, A, B}};
join(A, #t_abstract{}=B) ->
    #t_abstract{kind={join, A, B}};
join(A, B) ->
    beam_types:join(A, B).

meet(Same, Same) ->
    Same;
meet(#t_abstract{kind={ms_position, UnitA}},
     #t_abstract{kind={ms_position, UnitB}}) ->
    Unit = UnitA * UnitB div gcd(UnitA, UnitB),
    #t_abstract{kind={ms_position, Unit}};
meet(#t_abstract{}=A, B) ->
    #t_abstract{kind={meet, A, B}};
meet(A, #t_abstract{}=B) ->
    #t_abstract{kind={meet, A, B}};
meet(A, B) ->
    beam_types:meet(A, B).

subtract(#t_abstract{}=A, B) ->
    #t_abstract{kind={subtract, A, B}};
subtract(A, #t_abstract{}=B) ->
    #t_abstract{kind={subtract, A, B}};
subtract(A, B) ->
    beam_types:subtract(A, B).

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

unpack_typed_arg(#tr{r=Reg,t=Type}, Vst) ->
    %% The validator is not yet clever enough to do proper range analysis like
    %% the main type pass, so our types will be a bit cruder here, but they
    %% should at the very least not be in direct conflict.
    Current = get_movable_term_type(Reg, Vst),
    case beam_types:meet(Current, Type) of
        none ->
            throw({bad_typed_register, Current, Type});
        _ ->
            ok
    end,
    Reg;
unpack_typed_arg(Arg, _Vst) ->
    Arg.

%% get_term_type(Src, ValidatorState) -> Type
%%  Gets the type of the source Src, resolving deferred types into
%%  a concrete type.
get_concrete_type(Src, #vst{current=#st{vs=Vs}}=Vst) ->
    concrete_type(get_raw_type(Src, Vst), Vs).

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
    case get_concrete_type(Src, Vst) of
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
%% conflict or argument errors), it will simply be discarded.
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
                %% The instruction is guaranteed to fail; kill the state.
                {type_conflict, _, _} ->
                    kill_state(Vst);
                {invalid_argument, _} ->
                    kill_state(Vst)
            end
    catch
        %% This instruction is guaranteed not to fail, so we run the success
        %% branch *without* catching further errors to avoid hiding bugs in the
        %% validator itself; one of the branches must succeed.
        {type_conflict, _, _} ->
            SuccFun(Vst0);
        {invalid_argument, _} ->
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
validate_branch_1(_Lbl, _Tags) ->
    ok.

%% A shorthand version of branch/4 for when the state is only altered on
%% success.
branch(Fail, Vst, SuccFun) ->
    branch(Fail, Vst, fun(V) -> V end, SuccFun).

%% Shorthand of branch/4 for when the state is neither altered on failure nor
%% success.
branch(Fail, Vst) ->
    branch(Fail, Vst, fun(V) -> V end).

%% Directly branches off the state. This is an "internal" operation that should
%% be used sparingly.
fork_state(?EXCEPTION_LABEL, Vst0) ->
    #vst{current=#st{ct=CatchTags,numy=NumY}} = Vst0,

    %% The stack will be scanned looking for a catch tag, so all Y registers
    %% must be initialized.
    verify_y_init(Vst0),

    case CatchTags of
        [{_, [Fail]} | _] when is_integer(Fail) ->
            true = Fail =/= ?EXCEPTION_LABEL,   %Assertion.
            true = NumY =/= none,               %Assertion.

            %% Clear the receive marker and fork to our exception handler.
            Vst = update_receive_state(none, Vst0),
            fork_state(Fail, Vst);
        [] ->
            %% No catch handler; the exception leaves the function.
            Vst0;
        _ ->
            error(ambiguous_catch_try_state)
    end;
fork_state(L, #vst{current=St0,branched=Branched0,ref_ctr=Counter0}=Vst) ->
    {St, Counter} = merge_states(L, St0, Branched0, Counter0),
    Branched = Branched0#{ L => St },
    Vst#vst{branched=Branched,ref_ctr=Counter}.

%% merge_states/3 is used when there's more than one way to arrive at a
%% certain point, requiring the states to be merged down to the least
%% common subset for the subsequent code.

merge_states(L, St, Branched, Counter) when L =/= 0 ->
    case Branched of
        #{ L := OtherSt } -> merge_states_1(St, OtherSt, Counter);
        #{} -> {St, Counter}
    end.

merge_states_1(St, none, Counter) ->
    {St, Counter};
merge_states_1(none, St, Counter) ->
    {St, Counter};
merge_states_1(StA, StB, Counter0) ->
    #st{xs=XsA,ys=YsA,vs=VsA,fragile=FragA,numy=NumYA,
        h=HA,ct=CtA,recv_state=RecvStA,
        ms_positions=MsPosA} = StA,
    #st{xs=XsB,ys=YsB,vs=VsB,fragile=FragB,numy=NumYB,
        h=HB,ct=CtB,recv_state=RecvStB,
        ms_positions=MsPosB} = StB,

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

    RecvSt = merge_receive_state(RecvStA, RecvStB),
    MsPos = merge_ms_positions(MsPosA, MsPosB, Vs),
    Fragile = merge_fragility(FragA, FragB),
    NumY = merge_stk(YsA, YsB, NumYA, NumYB),
    Ct = merge_ct(CtA, CtB),

    St = #st{xs=Xs,ys=Ys,vs=Vs,fragile=Fragile,numy=NumY,
             h=min(HA, HB),ct=Ct,recv_state=RecvSt,
             ms_positions=MsPos},

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
    Entry = case {VsA, VsB} of
                {#{ Same := Entry0 }, #{ Same := Entry0 } } ->
                    Entry0;
                {#{ Same := #value{type=TypeA}=Entry0 },
                 #{ Same := #value{type=TypeB} } } ->
                    ConcreteA = concrete_type(TypeA, VsA),
                    ConcreteB = concrete_type(TypeB, VsB),
                    Entry0#value{type=join(ConcreteA, ConcreteB)}
            end,

    Acc = Acc0#{ Same => Entry },

    %% Type inference may depend on values that are no longer reachable from a
    %% register, so all arguments must be merged into the new state.
    mv_args(Entry#value.args, VsA, VsB, Acc);
mv_1({RefA, RefB}, New, VsA, VsB, Acc) ->
    #value{type=TypeA} = map_get(RefA, VsA),
    #value{type=TypeB} = map_get(RefB, VsB),

    ConcreteA = concrete_type(TypeA, VsA),
    ConcreteB = concrete_type(TypeB, VsB),
    Acc#{ New => #value{op=join,args=[],type=join(ConcreteA, ConcreteB)} }.

concrete_type(T, Vs) when is_function(T) -> T(Vs);
concrete_type(T, _Vs) -> T.

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
    sets:union(FragileA, FragileB).

merge_ms_positions(MsPosA, MsPosB, Vs) ->
    Keys = if
               map_size(MsPosA) =< map_size(MsPosB) -> maps:keys(MsPosA);
               map_size(MsPosA) > map_size(MsPosB) -> maps:keys(MsPosB)
           end,
    merge_ms_positions_1(Keys, MsPosA, MsPosB, Vs, #{}).

merge_ms_positions_1([Key | Keys], MsPosA, MsPosB, Vs, Acc) ->
    case {MsPosA, MsPosB} of
        {#{ Key := Pos }, #{ Key := Pos }} when is_map_key(Pos, Vs) ->
            merge_ms_positions_1(Keys, MsPosA, MsPosB, Vs, Acc#{ Key => Pos });
        {#{}, #{}} ->
            merge_ms_positions_1(Keys, MsPosA, MsPosB, Vs, Acc)
    end;
merge_ms_positions_1([], _MsPosA, _MsPosB, _Vs, Acc) ->
    Acc.

merge_receive_state(Same, Same) -> Same;
merge_receive_state(_, _) -> undecided.

merge_stk(_, _, S, S) ->
    S;
merge_stk(YsA, YsB, StkA, StkB) ->
    merge_stk_undecided(YsA, YsB, StkA, StkB).

merge_stk_undecided(YsA, YsB, {undecided, StkA}, {undecided, StkB}) ->
    %% We're merging two branches with different stack sizes. This is only okay
    %% if we're about to throw an exception, in which case all Y registers must
    %% be initialized on both paths.
    ok = merge_stk_verify_init(StkA - 1, YsA),
    ok = merge_stk_verify_init(StkB - 1, YsB),

    {undecided, min(StkA, StkB)};
merge_stk_undecided(YsA, YsB, StkA, StkB) when is_integer(StkA) ->
    merge_stk_undecided(YsA, YsB, {undecided, StkA}, StkB);
merge_stk_undecided(YsA, YsB, StkA, StkB) when is_integer(StkB) ->
    merge_stk_undecided(YsA, YsB, StkA, {undecided, StkB});
merge_stk_undecided(YsA, YsB, none, StkB) ->
    merge_stk_undecided(YsA, YsB, {undecided, 0}, StkB);
merge_stk_undecided(YsA, YsB, StkA, none) ->
    merge_stk_undecided(YsA, YsB, StkA, {undecided, 0}).

merge_stk_verify_init(-1, _Ys) ->
    ok;
merge_stk_verify_init(Y, Ys) ->
    Reg = {y, Y},
    case Ys of
        #{ Reg := TagOrVRef } when TagOrVRef =/= uninitialized ->
            merge_stk_verify_init(Y - 1, Ys);
        #{} ->
            error({unsafe_stack, Reg, Ys})
    end.

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
verify_y_init(#vst{current=#st{numy={undecided,MinSlots},ys=Ys}}=Vst) ->
    HighestY = maps:fold(fun({y,Y}, _, Acc) -> max(Y, Acc) end, -1, Ys),
    true = MinSlots > HighestY,                 %Assertion.
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
verify_no_ct(#vst{current=#st{numy={undecided,_}}}) ->
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

eat_heap_fun(#vst{current=#st{hl=HeapFuns0}=St}=Vst) ->
    case HeapFuns0-1 of
	Neg when Neg < 0 ->
	    error({heap_overflow,{left,{HeapFuns0,funs}},{wanted,{1,funs}}});
	HeapFuns ->
	    Vst#vst{current=St#st{hl=HeapFuns}}
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
%% exist before a certain point (e.g. it matches a newly created ref), it will
%% use "receive markers" to tell the corresponding loop_rec where to start
%% looking.
%%
%% Since receive markers affect the next loop_rec instruction it's very
%% important that we properly exit the receive loop the mark is intended for,
%% either through timing out or matching a message. Should we return from the
%% function or enter a different receive loop, we risk skipping messages that
%% should have been matched.
update_receive_state(New0, #vst{current=St0}=Vst) ->
    #st{recv_state=Current} = St0,
    New = case {Current, New0} of
              {none, marked_position} ->
                  marked_position;
              {marked_position, entered_loop} ->
                  entered_loop;
              {none, entered_loop} ->
                  entered_loop;
              {_, none} ->
                  none;
              {_, _} ->
                  error({invalid_receive_state_change, Current, New0})
          end,
    St = St0#st{recv_state=New},
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
    Fragile = sets:add_element(Reg, Fragile0),
    St = St0#st{fragile=Fragile},
    Vst#vst{current=St}.

propagate_fragility(Reg, Args, #vst{current=St0}=Vst) ->
    #st{fragile=Fragile0} = St0,

    Sources = sets:from_list(Args),
    Fragile = case sets:is_disjoint(Sources, Fragile0) of
                  true -> sets:del_element(Reg, Fragile0);
                  false -> sets:add_element(Reg, Fragile0)
              end,

    St = St0#st{fragile=Fragile},
    Vst#vst{current=St}.

%% Marks Reg as durable, must be used when assigning a newly created value to
%% a register.
remove_fragility(Reg, Vst) ->
    #vst{current=#st{fragile=Fragile0}=St0} = Vst,
    case sets:is_element(Reg, Fragile0) of
        true ->
            Fragile = sets:del_element(Reg, Fragile0),
            St = St0#st{fragile=Fragile},
            Vst#vst{current=St};
        false ->
            Vst
    end.

%% Marks all registers as durable.
remove_fragility(#vst{current=St0}=Vst) ->
    St = St0#st{fragile=sets:new()},
    Vst#vst{current=St}.

assert_durable_term(Src, Vst) ->
    assert_term(Src, Vst),
    assert_not_fragile(Src, Vst).

assert_not_fragile({Kind,_}=Src, Vst) when Kind =:= x; Kind =:= y ->
    check_limit(Src),
    #vst{current=#st{fragile=Fragile}} = Vst,
    case sets:is_element(Src, Fragile) of
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
    %% Note: match contexts and the likes are rejected by validate_bif/7 when
    %% this is invalid. We don't need to duplicate the check here.
    Args = [get_concrete_type(Arg, Vst) || Arg <- Ss],
    case {Op,Ss} of
        {element,[_,{literal,Tuple}]} when tuple_size(Tuple) > 0 ->
            case beam_call_types:types(erlang, Op, Args) of
                {any,ArgTypes,Safe} ->
                    RetType = join_tuple_elements(Tuple),
                    {RetType,ArgTypes,Safe};
                Other ->
                    Other
            end;
        {_,_} ->
            Res0 = beam_call_types:types(erlang, Op, Args),
            {Ret0, ArgTypes, SubSafe} = Res0,

            %% Match the non-converging range analysis done in
            %% `beam_ssa_type:opt_ranges/1`. This is safe since the validator
            %% doesn't have to worry about convergence.
            case beam_call_types:arith_type({bif, Op}, Args) of
                any -> Res0;
                Ret0 -> Res0;
                Ret -> {meet(Ret, Ret0), ArgTypes, SubSafe}
            end
    end.

join_tuple_elements(Tuple) ->
    join_tuple_elements(tuple_size(Tuple), Tuple, none).

join_tuple_elements(0, _Tuple, Type) ->
    Type;
join_tuple_elements(I, Tuple, Type0) ->
    Type1 = beam_types:make_type_from_value(element(I, Tuple)),
    Type = beam_types:join(Type0, Type1),
    join_tuple_elements(I - 1, Tuple, Type).

call_types({extfunc,M,F,A}, A, Vst) ->
    Args = get_call_args(A, Vst),
    beam_call_types:types(M, F, Args);
call_types(bs_init_writable, A, Vst) ->
    T = beam_types:make_type_from_value(<<>>),
    {T, get_call_args(A, Vst), false};
call_types(_, A, Vst) ->
    {any, get_call_args(A, Vst), false}.

will_bif_succeed(raise, [_,_], _Vst) ->
    %% Compiler-generated raise, the user-facing variant that can return
    %% 'badarg' is erlang:raise/3.
    no;
will_bif_succeed(Op, Ss, Vst) ->
    case is_float_arith_bif(Op, Ss) of
        true ->
            'maybe';
        false ->
            %% Note: match contexts and the likes are rejected by
            %% validate_bif/7 when this is invalid. We don't need to duplicate
            %% the check here.
            Args = [get_concrete_type(Arg, Vst) || Arg <- Ss],
            beam_call_types:will_succeed(erlang, Op, Args)
    end.

will_call_succeed({extfunc,M,F,A}, _Live, Vst) ->
    beam_call_types:will_succeed(M, F, get_call_args(A, Vst));
will_call_succeed(bs_init_writable, _Live, _Vst) ->
    yes;
will_call_succeed(raw_raise, _Live, _Vst) ->
    no;
will_call_succeed({f,Lbl}, _Live, #vst{ft=Ft}) ->
    case Ft of
        #{Lbl := #{always_fails := true}} ->
            no;
        #{} ->
            'maybe'
    end;
will_call_succeed(apply, Live, Vst) ->
    Mod = get_term_type({x,Live-2}, Vst),
    Name = get_term_type({x,Live-1}, Vst),
    case {Mod,Name} of
        {#t_atom{},#t_atom{}} ->
            'maybe';
        {_,_} ->
            no
    end;
will_call_succeed(_Call, _Live, _Vst) ->
    'maybe'.

get_call_args(Arity, Vst) ->
    get_call_args_1(0, Arity, Vst).

get_call_args_1(Arity, Arity, _) ->
    [];
get_call_args_1(N, Arity, Vst) when N < Arity ->
    ArgType = get_movable_term_type({x,N}, Vst),
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

error(Error) -> throw(Error).
