%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2025. All Rights Reserved.
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
%% Purpose: Generate BEAM assembly code from the SSA format.

-module(beam_ssa_codegen).
-moduledoc false.

-export([module/2]).
-export([is_original_variable/1]).  %Called from beam_core_to_ssa.
-export([classify_heap_need/2]).    %Called from beam_ssa_pre_codegen.

-export_type([ssa_register/0]).

-include("beam_ssa.hrl").
-include("beam_asm.hrl").

-import(lists, [append/1,foldl/3,keymember/3,keysort/2,map/2,mapfoldl/3,
                member/2,reverse/1,reverse/2,sort/1,
                splitwith/2,takewhile/2]).

-record(cg, {lcount=1 :: beam_label(),          %Label counter
	     functable=#{} :: #{fa() => beam_label()},
             labels=#{} :: #{ssa_label() => 0|beam_label()},
             used_labels=gb_sets:empty() :: gb_sets:set(ssa_label()),
             regs=#{} :: #{beam_ssa:b_var() => ssa_register()},
             ultimate_fail=1 :: beam_label(),
             catches=gb_sets:empty() :: gb_sets:set(ssa_label()),
             fc_label=1 :: beam_label(),
             debug_info=false :: boolean()
            }).

-spec module(beam_ssa:b_module(), [compile:option()]) ->
          {'ok',beam_asm:module_code()}.

module(#b_module{name=Mod,exports=Es,attributes=Attrs,body=Fs}, Opts) ->
    DebugInfo = member(beam_debug_info, Opts),
    {Asm,St} = functions(Fs, {atom,Mod}, DebugInfo),
    {ok,{Mod,Es,Attrs,Asm,St#cg.lcount}}.

-record(need, {h=0 :: non_neg_integer(),   % heap words
               l=0 :: non_neg_integer(),   % lambdas (funs)
               f=0 :: non_neg_integer()}). % floats

-record(cg_blk, {anno=#{} :: anno(),
                 is=[] :: [instruction()],
                 last :: terminator()}).

-record(cg_set, {anno=#{} :: anno(),
                 dst :: b_var(),
                 op :: beam_ssa:op() | 'nop',
                 args :: [beam_ssa:argument() | xreg()]}).

-record(cg_alloc, {anno=#{} :: anno(),
                   stack=none :: 'none' | pos_integer(),
                   words=#need{} :: #need{},
                   live :: 'undefined' | pos_integer(),
                   def_yregs=[] :: [b_var()]
                  }).

-record(cg_br, {bool :: beam_ssa:value(),
                succ :: ssa_label(),
                fail :: ssa_label()
               }).
-record(cg_ret, {arg :: cg_value(),
                 dealloc=none :: 'none' | pos_integer()
                }).
-record(cg_switch, {anno=#{} :: anno(),
                    arg :: cg_value(),
                    fail :: ssa_label(),
                    list :: [sw_list_item()]
                   }).

-type fa() :: {beam_asm:function_name(),arity()}.
-type ssa_label() :: beam_ssa:label().
-type beam_label() :: beam_asm:label().

-type anno() :: beam_ssa:anno().

-type b_var() :: beam_ssa:b_var().
-type b_literal() :: beam_ssa:b_literal().

-type cg_value() :: beam_ssa:value() | xreg().

-type cg_set() :: #cg_set{}.
-type cg_alloc() :: #cg_alloc{}.

-type instruction() :: cg_set() | cg_alloc().

-type cg_br() :: #cg_br{}.
-type cg_ret() :: #cg_ret{}.
-type cg_switch() :: #cg_switch{}.
-type terminator() :: cg_br() | cg_ret() | cg_switch().

-type sw_list_item() :: {b_literal(),ssa_label()}.

-type ssa_register() :: xreg() | yreg() | freg() | zreg().

functions(Forms, AtomMod, DebugInfo) ->
    mapfoldl(fun (F, St) -> function(F, AtomMod, St) end,
             #cg{lcount=1,debug_info=DebugInfo}, Forms).

function(#b_function{anno=Anno,bs=Blocks,args=Args}, AtomMod, St0) ->
    #{func_info := {_,Name,Arity}} = Anno,
    NoBsMatch = not maps:get(bs_ensure_opt, Anno, false),
    try
        assert_exception_block(Blocks),            %Assertion.
        Regs = maps:get(registers, Anno),
        St1 = St0#cg{labels=#{},used_labels=gb_sets:empty(),
                     regs=Regs},
        {Fi,St2} = new_label(St1),              %FuncInfo label
        {Entry,St3} = local_func_label(Name, Arity, St2),
        {Ult,St4} = new_label(St3),             %Ultimate failure
        Labels = (St4#cg.labels)#{0=>Entry,?EXCEPTION_BLOCK=>0},
        St5 = St4#cg{labels=Labels,used_labels=gb_sets:singleton(Entry),
                     ultimate_fail=Ult},
        {Body,St} = cg_fun(Blocks, Args, NoBsMatch, St5#cg{fc_label=Fi}),
        Asm0 = [{label,Fi},line(Anno),
                {func_info,AtomMod,{atom,Name},Arity}] ++
               add_parameter_annos(Body, Anno) ++
               [{label,Ult},if_end],
        Asm = fix_debug_line(Asm0, Arity, St),
        Func = {function,Name,Arity,Entry,Asm},
        {Func,St}
    catch
        Class:Error:Stack ->
            io:fwrite("Function: ~w/~w\n", [Name,Arity]),
            erlang:raise(Class, Error, Stack)
    end.

fix_debug_line(Is0, Live, #cg{debug_info=true}) ->
    case Is0 of
        [{label,_}=FiLbl,
         {line,_}=Li,
         {func_info,_,_,_}=Fi,
         {label,_}=Entry,
         {debug_line,Location,Index,Live,{none,Args0}}|Is] ->
            %% Mark this debug_line instruction as being the
            %% very first instruction in the function.
            RegToVar = #{Reg => Var || {Var,[{x,_}=Reg|_]} <- Args0},
            Args = [begin
                        X = {x,I-1},
                        case RegToVar of
                            #{X := Var} -> {Var,[X]};
                            #{} -> {I,[X]}
                        end
                    end || I <- lists:seq(1, Live)],
            DbgLine = {debug_line,Location,Index,Live,{entry,Args}},
            [FiLbl,Li,Fi,Entry,DbgLine|Is];
        _ ->
            Is0
    end;
fix_debug_line(Is, _Arity, #cg{debug_info=false}) ->
    Is.

assert_exception_block(Blocks) ->
    %% Assertion: ?EXCEPTION_BLOCK must be a call erlang:error(badarg).
    case Blocks of
        #{?EXCEPTION_BLOCK:=Blk} ->
            #b_blk{is=[#b_set{op=call,dst=Ret,
                              args=[#b_remote{mod=#b_literal{val=erlang},
                                              name=#b_literal{val=error}},
                                    #b_literal{val=badarg}]}],
                   last=#b_ret{arg=Ret}} = Blk,
            ok;
        #{} ->
            %% ?EXCEPTION_BLOCK has been removed because it was never used.
            ok
    end.

add_parameter_annos([{label, _}=Entry | Body], Anno) ->
    ParamTypes = maps:get(parameter_info, Anno, #{}),

    Annos = [begin
                 Info = map_get(K, ParamTypes),
                 {'%', {var_info, V, Info}}
             end || K := V <- map_get(registers, Anno),
                    is_map_key(K, ParamTypes)],

    [Entry | sort(Annos)] ++ Body.

cg_fun(Blocks, Args, NoBsMatch, St0) ->
    Linear0 = linearize(Blocks),
    St1 = collect_catch_labels(Linear0, St0),
    Linear1 = need_heap(Linear0),
    Linear2 = prefer_xregs(Linear1, St1),
    Linear3 = liveness(Linear2, St1),
    Linear4 = defined(Linear3, St1),
    Linear5 = opt_allocate(Linear4, St1),
    Linear6 = fix_wait_timeout(Linear5),
    Linear = add_debug_info(Linear6, Args, St1),
    {Asm,St} = cg_linear(Linear, St1),
    case NoBsMatch of
        true -> {Asm,St};
        false -> {bs_translate(Asm),St}
    end.

%% collect_catch_labels(Linear, St) -> St.
%%  Collect all catch labels (labels for blocks that begin
%%  with 'landingpad' instructions) for later use.

collect_catch_labels(Linear, St) ->
    Labels = collect_catch_labels_1(Linear),
    St#cg{catches=gb_sets:from_list(Labels)}.

collect_catch_labels_1([{L,#cg_blk{is=[#cg_set{op=landingpad}|_]}}|Bs]) ->
    [L|collect_catch_labels_1(Bs)];
collect_catch_labels_1([_|Bs]) ->
    collect_catch_labels_1(Bs);
collect_catch_labels_1([]) -> [].

%% need_heap([{BlockLabel,Block]) -> [{BlockLabel,Block}].
%%  Insert need_heap instructions in the instruction list. Try to be smart and
%%  collect them together as much as possible.

need_heap(Bs0) ->
    Bs1 = need_heap_allocs(Bs0, #{}),
    {Bs,#need{h=0,l=0,f=0}} = need_heap_blks(reverse(Bs1), #need{}, []),
    Bs.

need_heap_allocs([{L,#cg_blk{is=Is0,last=Terminator}=Blk0}|Bs], Counts0) ->
    Next = next_block(Bs),
    Successors = successors(Terminator),
    Counts = foldl(fun(S, Cnts) ->
                           case Cnts of
                               #{S:=C} -> Cnts#{S:=C+1};
                               #{} when S =:= Next -> Cnts#{S=>1};
                               #{} -> Cnts#{S=>42}
                           end
                   end, Counts0, Successors),
    case Counts of
        #{L:=1} ->
            [{L,Blk0}|need_heap_allocs(Bs, Counts)];
        #{L:=_} ->
            %% This block has multiple predecessors. Force an allocation
            %% in this block so that the predecessors don't need to do
            %% an allocation on behalf of this block.
            Is = case need_heap_never(Is0) of
                     true -> Is0;
                     false -> [#cg_alloc{}|Is0]
                 end,
            Blk = Blk0#cg_blk{is=Is},
            [{L,Blk}|need_heap_allocs(Bs, Counts)];
        #{} ->
            [{L,Blk0}|need_heap_allocs(Bs, Counts)]
    end;
need_heap_allocs([], _) -> [].

need_heap_never([#cg_alloc{}|_]) ->
    true;
need_heap_never([#cg_set{op=recv_next}|_]) ->
    true;
need_heap_never([#cg_set{op=wait_timeout,
                         args=[#b_literal{val=infinity}]}|_]) ->
    true;
need_heap_never(_) ->
    false.

need_heap_blks([{L,#cg_blk{is=Is0}=Blk0}|Bs], H0, Acc) ->
    {Is1,H1} = need_heap_is(reverse(Is0), H0, []),
    {Ns,H} = need_heap_terminator(Bs, L, H1),
    Is = delay_alloc(Ns ++ Is1),
    Blk = Blk0#cg_blk{is=Is},
    need_heap_blks(Bs, H, [{L,Blk}|Acc]);
need_heap_blks([], H, Acc) ->
    {Acc,H}.

delay_alloc([#cg_alloc{}=AI,
             #cg_set{op=debug_line}=ELI|Is2]) ->
    [ELI|delay_alloc([AI|Is2])];
delay_alloc(Is) -> Is.

need_heap_is([#cg_alloc{words=Words}=Alloc0|Is], N, Acc) ->
    Alloc = Alloc0#cg_alloc{words=add_heap_words(N, Words)},
    need_heap_is(Is, #need{}, [Alloc|Acc]);
need_heap_is([#cg_set{anno=Anno,op=bs_create_bin}=I0|Is], N, Acc) ->
    Alloc = case need_heap_need(N) of
                [#cg_alloc{words=Need}] -> alloc(Need);
                [] -> 0
            end,
    I = I0#cg_set{anno=Anno#{alloc=>Alloc}},
    need_heap_is(Is, #need{}, [I|Acc]);
need_heap_is([#cg_set{op=Op,args=Args}=I|Is], N, Acc) ->
    case classify_heap_need(Op, Args) of
        {put,Words} ->
            %% Pass through adding to needed heap.
            need_heap_is(Is, add_heap_words(N, Words), [I|Acc]);
        {put_fun,NArgs} ->
            need_heap_is(Is, add_heap_fun(N, NArgs), [I|Acc]);
        put_float ->
            need_heap_is(Is, add_heap_float(N), [I|Acc]);
        neutral ->
            need_heap_is(Is, N, [I|Acc]);
        gc ->
            need_heap_is(Is, #need{}, [I]++need_heap_need(N)++Acc)
    end;
need_heap_is([], N, Acc) ->
    {Acc,N}.

need_heap_terminator([{_,#cg_blk{last=#cg_br{succ=L,fail=L}}}|_], L, N) ->
    %% Fallthrough.
    {[],N};
need_heap_terminator([{_,#cg_blk{is=Is,last=#cg_br{succ=L}}}|_], L, N) ->
    case need_heap_need(N) of
        [] ->
            {[],#need{}};
        [_|_]=Alloc ->
            %% If the preceding instruction is a bs_create_bin instruction,
            %% hoist the allocation and incorporate into the bs_create_bin
            %% instruction.
            case reverse(Is) of
                [#cg_set{op=succeeded},#cg_set{op=bs_create_bin}|_] ->
                    {[],N};
                _ ->
                    %% Not binary construction. Must emit an allocation
                    %% instruction in this block.
                    {Alloc,#need{}}
            end
    end;
need_heap_terminator([{_,#cg_blk{}}|_], _, N) ->
    {need_heap_need(N),#need{}};
need_heap_terminator([], _, H) ->
    {need_heap_need(H),#need{}}.

need_heap_need(#need{h=0,l=0,f=0}) -> [];
need_heap_need(#need{}=N) -> [#cg_alloc{words=N}].

add_heap_words(#need{h=H1,l=L1,f=F1}, #need{h=H2,l=L2,f=F2}) ->
    #need{h=H1+H2,l=L1+L2,f=F1+F2};
add_heap_words(#need{h=Heap}=N, Words) when is_integer(Words) ->
    N#need{h=Heap+Words}.

add_heap_fun(#need{h=Heap, l=Lambdas}=N, NArgs) ->
    N#need{h=Heap+NArgs, l=Lambdas+1}.

add_heap_float(#need{f=F}=N) ->
    N#need{f=F+1}.

%% classify_heap_need(Operation, Arguments) ->
%%        gc | neutral | {put,Words} | put_float.
%%  Classify the heap need for this instruction. The return
%%  values have the following meaning.
%%
%%  {put,Words} means that the instruction will use Words words to build
%%  something on the heap.
%%
%%  'put_float' means that the instruction will build one floating point
%%  number on the heap.
%%
%%  'gc' means that that the instruction can potentially do a GC or throw an
%%  exception. That means that an allocation instruction for any building
%%  must be placed after this instruction.
%%
%%  'neutral' means that the instruction does nothing to disturb the heap.

-spec classify_heap_need(beam_ssa:op(), [beam_ssa:value()]) ->
                                'gc' | 'neutral' |
                                {'put',non_neg_integer()} |
                                {'put_fun', non_neg_integer()} |
                                'put_float'.

classify_heap_need(put_list, _) ->
    {put,2};
classify_heap_need(put_tuple, Elements) ->
    {put,length(Elements)+1};
classify_heap_need(make_fun, Args) ->
    {put_fun,length(Args)-1};
classify_heap_need({bif,Name}, Args) ->
    case is_gc_bif(Name, Args) of
        false -> neutral;
        true -> gc
    end;
classify_heap_need({float,Op}, _Args) ->
    case Op of
        get -> put_float;
        _ -> neutral
    end;
classify_heap_need(update_record, [_Flag, #b_literal{val=Size} |_ ]) ->
    {put, Size + 1};
classify_heap_need(Name, _Args) ->
    classify_heap_need(Name).

%% classify_heap_need(Operation) -> gc | neutral.
%%  Return either 'gc' or 'neutral'.
%%
%%  'gc' means that that the instruction can potentially do a GC or throw an
%%  exception. That means that an allocation instruction for any building
%%  must be placed after this instruction.
%%
%%  'neutral' means that the instruction does nothing to disturb the heap.
%%
%%  Note: Only handle operations in this function that are not handled
%%  by classify_heap_need/2.

classify_heap_need(bs_ensure) -> gc;
classify_heap_need(bs_ensured_get) -> gc;
classify_heap_need(bs_ensured_skip) -> gc;
classify_heap_need(bs_ensured_match_string) -> gc;
classify_heap_need(bs_get) -> gc;
classify_heap_need(bs_get_tail) -> gc;
classify_heap_need(bs_init_writable) -> gc;
classify_heap_need(bs_match_string) -> gc;
classify_heap_need(bs_create_bin) -> gc;
classify_heap_need(bs_get_position) -> gc;
classify_heap_need(bs_set_position) -> neutral;
classify_heap_need(bs_skip) -> gc;
classify_heap_need(bs_start_match) -> gc;
classify_heap_need(bs_test_tail) -> neutral;
classify_heap_need(build_stacktrace) -> gc;
classify_heap_need(call) -> gc;
classify_heap_need(catch_end) -> gc;
classify_heap_need(copy) -> neutral;
classify_heap_need(debug_line) -> gc;
classify_heap_need(executable_line) -> neutral;
classify_heap_need(extract) -> gc;
classify_heap_need(get_hd) -> neutral;
classify_heap_need(get_map_element) -> neutral;
classify_heap_need(get_tl) -> neutral;
classify_heap_need(get_tuple_element) -> neutral;
classify_heap_need(has_map_field) -> neutral;
classify_heap_need(is_nonempty_list) -> neutral;
classify_heap_need(is_tagged_tuple) -> neutral;
classify_heap_need(kill_try_tag) -> gc;
classify_heap_need(landingpad) -> gc;
classify_heap_need(match_fail) -> gc;
classify_heap_need(nif_start) -> neutral;
classify_heap_need(nop) -> neutral;
classify_heap_need(new_try_tag) -> neutral;
classify_heap_need(peek_message) -> gc;
classify_heap_need(put_map) -> gc;
classify_heap_need(raw_raise) -> gc;
classify_heap_need(recv_marker_bind) -> neutral;
classify_heap_need(recv_marker_clear) -> neutral;
classify_heap_need(recv_marker_reserve) -> gc;
classify_heap_need(recv_next) -> gc;
classify_heap_need(remove_message) -> neutral;
classify_heap_need(resume) -> gc;
classify_heap_need(set_tuple_element) -> gc;
classify_heap_need(succeeded) -> neutral;
classify_heap_need(wait_timeout) -> gc.

%%%
%%% Because beam_ssa_pre_codegen has inserted 'copy' instructions to copy
%%% variables that must be saved on the stack, a value can for some time
%%% be in both an X register and a Y register.
%%%
%%% Here we will keep track of variables that have the same value and
%%% rewrite instructions to use the variable that refers to the X
%%% register instead of the Y register. That could improve performance,
%%% since the BEAM interpreter have more optimized instructions
%%% operating on X registers than on Y registers.
%%%
%%% In call instructions there is also the possibility that a 'move'
%%% instruction can be eliminated because a value is already in the
%%% correct X register.
%%%
%%% Because of the new 'swap' instruction introduced in OTP 23, it
%%% is always beneficial to prefer X register over Y registers. That
%%% was not the case in OTP 22, which lacks the 'swap' instruction.
%%%

prefer_xregs(Linear, St) ->
    prefer_xregs(Linear, St, #{0=>#{}}).

prefer_xregs([{L,#cg_blk{is=Is0,last=Last0}=Blk0}|Bs], St, Map0) ->
    Copies0 = maps:get(L, Map0),
    {Is,Copies} = prefer_xregs_is(Is0, St, Copies0, []),
    Last = prefer_xregs_terminator(Last0, Copies, St),
    Blk = Blk0#cg_blk{is=Is,last=Last},
    Successors = successors(Last),
    Map = prefer_xregs_successors(Successors, Copies, Map0),
    [{L,Blk}|prefer_xregs(Bs, St, Map)];
prefer_xregs([], _St, _Map) -> [].

prefer_xregs_successors([L|Ls], Copies0, Map0) ->
    case Map0 of
        #{L:=Copies1} ->
            Copies = merge_copies(Copies0, Copies1),
            Map = Map0#{L:=Copies},
            prefer_xregs_successors(Ls, Copies0, Map);
        #{} ->
            Map = Map0#{L=>Copies0},
            prefer_xregs_successors(Ls, Copies0, Map)
    end;
prefer_xregs_successors([], _, Map) -> Map.

prefer_xregs_is([#cg_alloc{}=I|Is], St, Copies0, Acc) ->
    Copies = case I of
                 #cg_alloc{stack=none,words=#need{h=0,l=0,f=0}} ->
                     Copies0;
                 #cg_alloc{} ->
                     #{}
             end,
    prefer_xregs_is(Is, St, Copies, [I|Acc]);
prefer_xregs_is([#cg_set{op=copy,dst=Dst,args=[Src]}=I|Is], St, Copies0, Acc) ->
    Copies1 = prefer_xregs_prune(I, Copies0, St),
    Copies = case beam_args([Src,Dst], St) of
                 [Same,Same] -> Copies1;
                 [_,_] -> Copies1#{Dst=>Src}
             end,
    prefer_xregs_is(Is, St, Copies, [I|Acc]);
prefer_xregs_is([#cg_set{op=call,dst=Dst}=I0|Is], St, Copies, Acc) ->
    I = prefer_xregs_call(I0, Copies, St),
    prefer_xregs_is(Is, St, #{Dst=>{x,0}}, [I|Acc]);
prefer_xregs_is([#cg_set{op=Op}=I|Is], St, Copies0, Acc)
  when Op =:= bs_ensured_get;
       Op =:= bs_ensured_skip;
       Op =:= bs_ensure;
       Op =:= bs_ensured_match_string;
       Op =:= bs_match_string ->
    Copies = prefer_xregs_prune(I, Copies0, St),
    prefer_xregs_is(Is, St, Copies, [I|Acc]);
prefer_xregs_is([#cg_set{args=Args0}=I0|Is], St, Copies0, Acc) ->
    Args = [do_prefer_xreg(A, Copies0, St) || A <- Args0],
    I = I0#cg_set{args=Args},
    Copies = prefer_xregs_prune(I, Copies0, St),
    prefer_xregs_is(Is, St, Copies, [I|Acc]);
prefer_xregs_is([], _St, Copies, Acc) ->
    {reverse(Acc),Copies}.

prefer_xregs_terminator(#cg_br{bool=Arg0}=I, Copies, St) ->
    Arg = do_prefer_xreg(Arg0, Copies, St),
    I#cg_br{bool=Arg};
prefer_xregs_terminator(#cg_ret{arg=Arg0}=I, Copies, St) ->
    Arg = do_prefer_xreg(Arg0, Copies, St),
    I#cg_ret{arg=Arg};
prefer_xregs_terminator(#cg_switch{arg=Arg0}=I, Copies, St) ->
    Arg = do_prefer_xreg(Arg0, Copies, St),
    I#cg_switch{arg=Arg}.

prefer_xregs_prune(#cg_set{anno=#{clobbers:=true}}, _, _) ->
    #{};
prefer_xregs_prune(#cg_set{dst=Dst}, Copies, St) ->
    DstReg = beam_arg(Dst, St),
    #{V => Alias || V := Alias <- Copies,
                    beam_arg(Alias, St) =/= DstReg}.

%% prefer_xregs_call(Instruction, Copies, St) -> Instruction.
%%  Given a 'call' instruction rewrite the arguments
%%  to use an X register instead of a Y register if a value is
%%  is available in both.

prefer_xregs_call(#cg_set{args=[F0|Args0]}=I, Copies, St) ->
    F = case F0 of
            #b_var{} ->
                do_prefer_xreg(F0, Copies, St);
            #b_remote{mod=Mod,name=Name} ->
                F0#b_remote{mod=do_prefer_xreg(Mod, Copies, St),
                            name=do_prefer_xreg(Name, Copies, St)};
            _ ->
                F0
        end,
    Args = [do_prefer_xreg(A, Copies, St) || A <- Args0],
    I#cg_set{args=[F|Args]}.

do_prefer_xreg(#b_var{}=A, Copies, St) ->
    case {beam_arg(A, St),Copies} of
        {{y,_},#{A:=Copy}} ->
            Copy;
        {_,_} ->
            A
    end;
do_prefer_xreg(A, _, _) -> A.

merge_copies(Copies0, Copies1) when map_size(Copies0) =< map_size(Copies1) ->
    #{K => V || K := V <- Copies0,
                case Copies1 of
                    #{K := V} -> true;
                    #{} -> false
                end};
merge_copies(Copies0, Copies1) ->
    merge_copies(Copies1, Copies0).


%%%
%%% Add annotations for the number of live registers.
%%%

liveness(Linear, #cg{regs=Regs}) ->
    liveness(reverse(Linear), #{}, Regs, []).

liveness([{L,#cg_blk{is=Is0,last=Last0}=Blk0}|Bs], LiveMap0, Regs, Acc) ->
    Successors = liveness_successors(Last0),
    Live0 = ordsets:union([liveness_get(S, LiveMap0) || S <- Successors]),
    Live1 = liveness_terminator(Last0, Live0),
    {Is,Live} = liveness_is(reverse(Is0), Regs, Live1, []),
    LiveMap = LiveMap0#{L=>Live},
    Blk = Blk0#cg_blk{is=Is},
    liveness(Bs, LiveMap, Regs, [{L,Blk}|Acc]);
liveness([], _LiveMap, _Regs, Acc) -> Acc.

liveness_get(S, LiveMap) ->
    case LiveMap of
        #{S:=Live} -> Live;
        #{} -> []
    end.

liveness_successors(Terminator) ->
    successors(Terminator) -- [?EXCEPTION_BLOCK].

liveness_is([#cg_alloc{}=I0|Is], Regs, Live, Acc) ->
    I = I0#cg_alloc{live=num_live(Live, Regs)},
    liveness_is(Is, Regs, Live, [I|Acc]);
liveness_is([#cg_set{dst=Dst,args=Args}=I0|Is], Regs, Live0, Acc) ->
    Live1 = liveness_clobber(I0, Live0, Regs),
    I1 = liveness_yregs_anno(I0, Live1, Regs),
    Live2 = liveness_args(Args, Live1),
    Live = ordsets:del_element(Dst, Live2),
    I = liveness_anno(I1, Live, Regs),
    liveness_is(Is, Regs, Live, [I|Acc]);
liveness_is([], _, Live, Acc) ->
    {Acc,Live}.

liveness_terminator(#cg_br{bool=Arg}, Live) ->
    liveness_terminator_1(Arg, Live);
liveness_terminator(#cg_switch{arg=Arg}, Live) ->
    liveness_terminator_1(Arg, Live);
liveness_terminator(#cg_ret{arg=Arg}, Live) ->
    liveness_terminator_1(Arg, Live).

liveness_terminator_1(#b_var{}=V, Live) ->
    ordsets:add_element(V, Live);
liveness_terminator_1(#b_literal{}, Live) ->
    Live;
liveness_terminator_1(Reg, Live) ->
    _ = verify_beam_register(Reg),
    ordsets:add_element(Reg, Live).

liveness_args([#b_var{}=V|As], Live) ->
    liveness_args(As, ordsets:add_element(V, Live));
liveness_args([#b_remote{mod=Mod,name=Name}|As], Live) ->
    liveness_args([Mod,Name|As], Live);
liveness_args([A|As], Live) ->
    case is_beam_register(A) of
        true ->
            liveness_args(As, ordsets:add_element(A, Live));
        false ->
            liveness_args(As, Live)
    end;
liveness_args([], Live) -> Live.

liveness_anno(#cg_set{op=Op}=I, Live, Regs) ->
    case need_live_anno(Op) of
        true ->
            NumLive = num_live(Live, Regs),
            Anno = (I#cg_set.anno)#{live=>NumLive},
            I#cg_set{anno=Anno};
        false ->
            I
    end.

liveness_yregs_anno(#cg_set{op=Op,dst=Dst}=I, Live0, Regs) ->
    case need_live_anno(Op) of
        true ->
            Live = ordsets:del_element(Dst, Live0),
            LiveYregs = [V || V <- Live, is_yreg(V, Regs)],
            Anno = (I#cg_set.anno)#{live_yregs=>LiveYregs},
            I#cg_set{anno=Anno};
        false ->
            I
    end.

liveness_clobber(#cg_set{anno=Anno}, Live, Regs) ->
    case Anno of
        #{clobbers:=true} ->
            [R || R <- Live, is_yreg(R, Regs)];
        _ ->
            Live
    end.

is_yreg(R, Regs) ->
    case Regs of
        #{R:={y,_}} -> true;
        #{} -> false
    end.

num_live(Live, Regs) ->
    Rs = ordsets:from_list([get_register(V, Regs) || V <- Live]),
    num_live_1(Rs, 0).

num_live_1([{x,X}|T], X) ->
    num_live_1(T, X+1);
num_live_1([{x,_}|_]=T, X) ->
    %% error({hole,{x,X},expected,Next});
    num_live_1(T, X+1);
num_live_1([{y,_}|_], X) ->
    X;
num_live_1([{z,_}|_], X) ->
    X;
num_live_1([{fr,_}|T], X) ->
    num_live_1(T, X);
num_live_1([], X) ->
    X.

get_live(#cg_set{anno=#{live:=Live}}) ->
    Live.

%% need_live_anno(Operation) -> true|false.
%%  Return 'true' if the instruction needs a 'live' annotation with
%%  the number live X registers, or 'false' otherwise.

need_live_anno(Op) ->
    case Op of
        {bif,_} -> true;
        bs_create_bin -> true;
        bs_ensured_get -> true;
        bs_get -> true;
        bs_get_position -> true;
        bs_get_tail -> true;
        bs_start_match -> true;
        bs_skip -> true;
        call -> true;
        debug_line -> true;
        put_map -> true;
        update_record -> true;
        _ -> false
    end.

%%%
%%% Add the following annotations for Y registers:
%%%
%%%   def_yregs   An ordset with variables that refer to live Y registers.
%%%               That is, Y registers that that have been killed
%%%               are not included. This annotation is added to all
%%%               instructions that require Y registers to be initialized.
%%%
%%%   kill_yregs  This annotation is added to call instructions. It is
%%%               an ordset containing variables referring to Y registers
%%%               that will no longer be used after the call instruction.
%%%

defined(Linear, #cg{regs=Regs}) ->
    def(Linear, #{}, Regs).

def([{L,#cg_blk{is=Is0,last=Last}=Blk0}|Bs], DefMap0, Regs) ->
    Def0 = def_get(L, DefMap0),
    {Is,Def,MaybeDef} = def_is(Is0, Regs, Def0, []),
    DefMap = def_successors(Last, Def, MaybeDef, DefMap0),
    Blk = Blk0#cg_blk{is=Is},
    [{L,Blk}|def(Bs, DefMap, Regs)];
def([], _, _) -> [].

def_get(L, DefMap) ->
    case DefMap of
        #{L:=Def} -> Def;
        #{} -> []
    end.

def_is([#cg_alloc{anno=Anno0}=I0|Is], Regs, Def, Acc) ->
    I = I0#cg_alloc{anno=Anno0#{def_yregs=>Def}},
    def_is(Is, Regs, Def, [I|Acc]);
def_is([#cg_set{op=succeeded,args=[Var]}=I], Regs, Def, Acc) ->
    %% Var will only be defined on the success branch of the `br`
    %% for this block.
    MaybeDef = def_add_yreg(Var, [], Regs),
    {reverse(Acc, [I]),Def,MaybeDef};
def_is([#cg_set{op=kill_try_tag,args=[#b_var{}=Tag]}=I|Is], Regs, Def0, Acc) ->
    Def = ordsets:del_element(Tag, Def0),
    def_is(Is, Regs, Def, [I|Acc]);
def_is([#cg_set{op=catch_end,args=[#b_var{}=Tag|_]}=I|Is], Regs, Def0, Acc) ->
    Def = ordsets:del_element(Tag, Def0),
    def_is(Is, Regs, Def, [I|Acc]);
def_is([#cg_set{anno=Anno0,op=call,dst=Dst}=I0|Is],
       Regs, Def0, Acc) ->
    #{live_yregs:=LiveYregVars} = Anno0,
    LiveRegs = gb_sets:from_list([maps:get(V, Regs) || V <- LiveYregVars]),
    Kill0 = ordsets:subtract(Def0, LiveYregVars),

    %% Kill0 is the set of variables that have just died. However, the registers
    %% used for killed variables may have been reused, so we must check that the
    %% registers to be killed are not used by other variables.
    Kill = [K || K <- Kill0, not gb_sets:is_element(maps:get(K, Regs), LiveRegs)],
    Anno = Anno0#{def_yregs=>Def0,kill_yregs=>Kill},
    I = I0#cg_set{anno=Anno},
    Def1 = ordsets:subtract(Def0, Kill),
    Def = def_add_yreg(Dst, Def1, Regs),
    def_is(Is, Regs, Def, [I|Acc]);
def_is([#cg_set{anno=Anno0,op={bif,Bif},dst=Dst,args=Args}=I0|Is],
       Regs, Def0, Acc) ->
    Arity = length(Args),
    I = case is_gc_bif(Bif, Args) orelse not erl_bifs:is_safe(erlang, Bif, Arity) of
            true ->
                I0#cg_set{anno=Anno0#{def_yregs=>Def0}};
            false ->
                I0
        end,
    Def = def_add_yreg(Dst, Def0, Regs),
    def_is(Is, Regs, Def, [I|Acc]);
def_is([#cg_set{anno=Anno0,dst=Dst}=I0|Is], Regs, Def0, Acc) ->
    I = case need_y_init(I0) of
            true ->
                I0#cg_set{anno=Anno0#{def_yregs=>Def0}};
            false ->
                I0
        end,
    Def = def_add_yreg(Dst, Def0, Regs),
    def_is(Is, Regs, Def, [I|Acc]);
def_is([], _, Def, Acc) ->
    {reverse(Acc),Def,[]}.

def_add_yreg(Dst, Def, Regs) ->
    case is_yreg(Dst, Regs) of
        true -> ordsets:add_element(Dst, Def);
        false -> Def
    end.

def_successors(#cg_br{bool=#b_var{},succ=Succ,fail=Fail}, Def, MaybeDef, DefMap0) ->
    DefMap = def_successors([Fail], ordsets:subtract(Def, MaybeDef), DefMap0),
    def_successors([Succ], Def, DefMap);
def_successors(Last, Def, [], DefMap) ->
    def_successors(successors(Last), Def, DefMap).

def_successors([S|Ss], Def0, DefMap) ->
    case DefMap of
        #{S:=Def1} ->
            Def = ordsets:intersection(Def0, Def1),
            def_successors(Ss, Def0, DefMap#{S:=Def});
        #{} ->
            def_successors(Ss, Def0, DefMap#{S=>Def0})
    end;
def_successors([], _, DefMap) -> DefMap.

%% need_y_init(#cg_set{}) -> true|false.
%%  Return true if this instructions needs initialized Y registers
%%  (because the instruction may do a GC or cause an exception
%%  so that the stack will be scanned), or false otherwise.

need_y_init(#cg_set{anno=#{clobbers:=Clobbers}}) -> Clobbers;
need_y_init(#cg_set{op=bs_get}) -> true;
need_y_init(#cg_set{op=bs_get_position}) -> true;
need_y_init(#cg_set{op=bs_get_tail}) -> true;
need_y_init(#cg_set{op=bs_create_bin}) -> true;
need_y_init(#cg_set{op=bs_skip,args=[#b_literal{val=Type}|_]}) ->
    case Type of
        utf8 -> true;
        utf16 -> true;
        utf32 -> true;
        _ -> false
    end;
need_y_init(#cg_set{op=bs_start_match}) -> true;
need_y_init(#cg_set{op=debug_line}) -> true;
need_y_init(#cg_set{op=put_map}) -> true;
need_y_init(#cg_set{op=update_record}) -> true;
need_y_init(#cg_set{}) -> false.

%% opt_allocate([{BlockLabel,Block}], #st{}) -> [BeamInstruction].
%%  Update the def_yregs field of each #cg_alloc{} that allocates
%%  a stack frame. #cg_alloc.def_yregs will list all Y registers
%%  that will be initialized by the subsequent code (thus, the
%%  listed Y registers don't require init/1 instructions).

opt_allocate(Linear, #cg{regs=Regs}) ->
    opt_allocate_1(Linear, Regs).

opt_allocate_1([{L,#cg_blk{is=[#cg_alloc{stack=Stk}=I0|Is]}=Blk0}|Bs]=Bs0, Regs)
  when is_integer(Stk) ->
    %% Collect the variables that are initialized by copy
    %% instruction in this block.
    case ordsets:from_list(opt_allocate_defs(Is, Regs)) of
        Yregs when length(Yregs) =:= Stk ->
            %% Those copy instructions are sufficient to fully
            %% initialize the stack frame.
            I = I0#cg_alloc{def_yregs=Yregs},
            [{L,Blk0#cg_blk{is=[I|Is]}}|opt_allocate_1(Bs, Regs)];
        Yregs0 ->
            %% Determine a conservative approximation of the Y
            %% registers that are guaranteed to be initialized by all
            %% successors of this block, and to it add the variables
            %% initialized by copy instructions in this block.
            Yregs1 = opt_alloc_def(Bs0, gb_sets:singleton(L), []),
            Yregs = ordsets:union(Yregs0, Yregs1),
            I = I0#cg_alloc{def_yregs=Yregs},
            [{L,Blk0#cg_blk{is=[I|Is]}}|opt_allocate_1(Bs, Regs)]
    end;
opt_allocate_1([B|Bs], Regs) ->
    [B|opt_allocate_1(Bs, Regs)];
opt_allocate_1([], _) -> [].

opt_allocate_defs([#cg_set{op=copy,dst=Dst}|Is], Regs) ->
    case is_yreg(Dst, Regs) of
        true -> [Dst|opt_allocate_defs(Is, Regs)];
        false -> []
    end;
opt_allocate_defs([#cg_set{anno=Anno,op={bif,Bif},args=Args,dst=Dst}|Is], Regs) ->
    case is_gc_bif(Bif, Args) of
        false ->
            ArgTypes = maps:get(arg_types, Anno, #{}),
            case is_yreg(Dst, Regs) andalso will_bif_succeed(Bif, Args, ArgTypes) of
                true -> [Dst|opt_allocate_defs(Is, Regs)];
                false -> []
            end;
        true ->
            []
    end;
opt_allocate_defs(_, _Regs) -> [].

will_bif_succeed(Bif, Args, ArgTypes) ->
    Types = will_bif_succeed_types(Args, ArgTypes, 0),
    case beam_call_types:will_succeed(erlang, Bif, Types) of
        yes -> true;
        _ -> false
    end.

will_bif_succeed_types([#b_literal{val=Val}|Args], ArgTypes, N) ->
    Type = beam_types:make_type_from_value(Val),
    [Type|will_bif_succeed_types(Args, ArgTypes, N + 1)];
will_bif_succeed_types([#b_var{}|Args], ArgTypes, N) ->
    Type = maps:get(N, ArgTypes, any),
    [Type|will_bif_succeed_types(Args, ArgTypes, N + 1)];
will_bif_succeed_types([], _, _) -> [].

opt_alloc_def([{L,#cg_blk{is=Is,last=Last}}|Bs], Ws0, Def0) ->
    case gb_sets:is_member(L, Ws0) of
        false ->
            opt_alloc_def(Bs, Ws0, Def0);
        true ->
            case opt_allocate_is(Is) of
                none ->
                    Succ = successors(Last),
                    Ws = gb_sets:union(Ws0, gb_sets:from_list(Succ)),
                    opt_alloc_def(Bs, Ws, Def0);
                Def1 when is_list(Def1) ->
                    Def = [Def1|Def0],
                    opt_alloc_def(Bs, Ws0, Def)
            end
    end;
opt_alloc_def([], _, Def) ->
    ordsets:intersection(Def).

opt_allocate_is([#cg_set{anno=Anno}|Is]) ->
    case Anno of
        #{def_yregs:=Yregs} ->
            Yregs;
        #{} ->
            opt_allocate_is(Is)
    end;
opt_allocate_is([#cg_alloc{anno=#{def_yregs:=Yregs},stack=none}|_]) ->
    Yregs;
opt_allocate_is([#cg_alloc{}|Is]) ->
    opt_allocate_is(Is);
opt_allocate_is([]) -> none.

%% fix_wait_timeout([Block]) -> [Block].
%%  In SSA code, the `wait_timeout` instruction is a three-way branch
%%  (because there will be an exception for a bad timeout value). In
%%  BEAM code, the potential raising of an exception for a bad timeout
%%  duration is not explicitly represented. Thus we will need to
%%  rewrite the following code:
%%
%%       WaitBool = wait_timeout TimeoutValue
%%       Succeeded = succeeded:body WaitBool
%%       br Succeeded, ^good_timeout_value, ^bad_timeout_value
%%
%%   good_timeout_value:
%%       br WaitBool, ^timeout_expired, ^new_message_received
%%
%%  To this code:
%%
%%       WaitBool = wait_timeout TimeoutValue
%%       br WaitBool, ^timeout_expired, ^new_message_received
%%
fix_wait_timeout([{L1,#cg_blk{is=Is0,last=#cg_br{bool=#b_var{},succ=L2}}=Blk1},
                  {L2,#cg_blk{is=[],last=#cg_br{}=Br}=Blk2}|Bs]) ->
    case fix_wait_timeout_is(Is0, []) of
        no ->
            [{L1,Blk1},{L2,Blk2}|fix_wait_timeout(Bs)];
        {yes,Is} ->
            [{L1,Blk1#cg_blk{is=Is,last=Br}}|fix_wait_timeout(Bs)]
    end;
fix_wait_timeout([B|Bs]) ->
    [B|fix_wait_timeout(Bs)];
fix_wait_timeout([]) -> [].

fix_wait_timeout_is([#cg_set{op=wait_timeout,dst=WaitBool}=WT,
                     #cg_set{op=succeeded,args=[WaitBool]}], Acc) ->
    {yes,reverse(Acc, [WT])};
fix_wait_timeout_is([I|Is], Acc) ->
    fix_wait_timeout_is(Is, [I|Acc]);
fix_wait_timeout_is([], _Acc) -> no.

%%%
%%% Gather debug information and add as annotations to `debug_line`
%%% instructions.
%%%
%%% This pass is run when collection of BEAM debug information has
%%% been requested.
%%%

add_debug_info(Linear0, Args, #cg{regs=Regs,debug_info=true}) ->
    Def0 = ordsets:from_list(Args),
    Linear = anno_defined_regs(Linear0, Def0, Regs),
    FrameSzMap = #{0 => none},
    VarMap = #{},
    add_debug_info_blk(Linear, Regs, FrameSzMap, VarMap);
add_debug_info(Linear, _Args, #cg{debug_info=false}) ->
    Linear.

add_debug_info_blk([{L,#cg_blk{is=Is0,last=Last}=Blk0}|Bs],
                   Regs, FrameSzMap0, VarMap0) ->
    FrameSize0 = map_get(L, FrameSzMap0),
    {Is,VarMap,FrameSize} =
        add_debug_info_is(Is0, Regs, FrameSize0, VarMap0, []),
    Successors = successors(Last),
    FrameSzMap = foldl(fun(Succ, Acc) ->
                               Acc#{Succ => FrameSize}
                       end, FrameSzMap0, Successors),
    Blk = Blk0#cg_blk{is=Is},
    [{L,Blk}|add_debug_info_blk(Bs, Regs, FrameSzMap, VarMap)];
add_debug_info_blk([], _Regs, _FrameSzMap, _VarMap) ->
    [].

add_debug_info_is([#cg_alloc{stack=FrameSize}=I|Is],
                  Regs, FrameSize0, VarMap, Acc) ->
    if
        is_integer(FrameSize) ->
            add_debug_info_is(Is, Regs, FrameSize, VarMap, [I|Acc]);
        true ->
            add_debug_info_is(Is, Regs, FrameSize0, VarMap, [I|Acc])
    end;
add_debug_info_is([#cg_set{anno=#{was_phi := true},op=copy}=I|Is],
                  Regs, FrameSize, VarMap, Acc) ->
    %% This copy operation originates from a phi node. The source and
    %% destination are not equivalent and must not be added to VarMap.
    add_debug_info_is(Is, Regs, FrameSize, VarMap, [I|Acc]);
add_debug_info_is([#cg_set{anno=Anno,op=copy,dst=#b_var{name=Dst},
                           args=[#b_var{name=Src}]}=I|Is],
                  Regs, FrameSize, VarMap0, Acc) ->
    VarMap = case Anno of
                 #{delayed_yreg_copy := true} ->
                     VarMap0#{Src => [Dst]};
                 #{} ->
                     VarMap0#{Dst => [Src]}
             end,
    add_debug_info_is(Is, Regs, FrameSize, VarMap, [I|Acc]);
add_debug_info_is([#cg_set{anno=Anno0,op=debug_line,args=[Index]}=I0|Is],
                  Regs, FrameSize, VarMap, Acc) ->
    #{def_regs := DefRegs,
      alias := Alias,
      literals := Literals0,
      live := NumLive0} = Anno0,
    AliasMap = maps:merge_with(fun(_, L1, L2) -> L1 ++ L2 end,
                               VarMap, Alias),
    Literals1 = [{get_original_names(#b_var{name=Var}, AliasMap),Val} ||
                    {Val,Var} <:- Literals0],
    Literals = [{hd(Vars),[{literal,Val}]} ||
                   {Vars,Val} <:- Literals1, Vars =/= []],
    RegVarMap = [{map_get(V, Regs),get_original_names(V, AliasMap)} ||
                    V <- DefRegs,
                    not is_beam_register(V)],
    S0 = sofs:family(RegVarMap, [{reg,[variable]}]),
    S1 = sofs:family_to_relation(S0),
    S2 = sofs:converse(S1),
    S3 = sofs:relation_to_family(S2),
    S = sort(Literals ++ sofs:to_external(S3)),
    Live = max(NumLive0, num_live(DefRegs, Regs)),
    Info = {FrameSize,S},
    I = I0#cg_set{args=[Index,#b_literal{val=Live},#b_literal{val=Info}]},
    add_debug_info_is(Is, Regs, FrameSize, VarMap, [I|Acc]);
add_debug_info_is([#cg_set{}=I|Is], Regs, FrameSize, VarMap, Acc) ->
    add_debug_info_is(Is, Regs, FrameSize, VarMap, [I|Acc]);
add_debug_info_is([], _Regs, FrameSize, VarMap, Info) ->
    {reverse(Info),VarMap,FrameSize}.

get_original_names(#b_var{name=Name}, AliasMap) ->
    get_original_names_1([Name], AliasMap, sets:new()).

get_original_names_1([Name|Names0], AliasMap, Seen0) ->
    case sets:is_element(Name, Seen0) of
        true ->
            get_original_names_1(Names0, AliasMap, Seen0);
        false ->
            Seen = sets:add_element(Name, Seen0),
            Names = case AliasMap of
                        #{Name := Vars0} ->
                            Vars = Vars0 ++ Names0,
                            get_original_names_1(Vars, AliasMap, Seen);
                        #{} ->
                            Names0
                    end,
            case is_original_variable(Name) of
                true ->
                    [Name|get_original_names_1(Names, AliasMap, Seen)];
                false ->
                    get_original_names_1(Names, AliasMap, Seen)
            end
    end;
get_original_names_1([], _, _) ->
    [].

-spec is_original_variable(Name) -> boolean() when
      Name :: non_neg_integer() | atom().

%% Test whether the variable name originates from the Erlang source
%% code, meaning that it was not invented by the compiler. It is
%% sufficient to check that the first character can legally start an
%% Erlang variable name, because all new variables inserted by the
%% compiler always start with an invalid character such as "@" or a
%% lower-case letter.
is_original_variable(Name) when is_atom(Name) ->
    <<C/utf8,_/binary>> = atom_to_binary(Name),
    if
        %% A variable name must start with "_" or an upper-case letter
        %% included in ISO Latin-1.
        C =:= $_ -> true;
        $A =< C, C =< $Z -> true;
        $À =< C, C =< $Þ, C =/= $× -> true;
        true -> false
    end;
is_original_variable(Name) when is_integer(Name) ->
    false.

%%%
%%% Annotate `debug_line` instructions with all variables that have
%%% been defined and are still available in a BEAM register.
%%%

anno_defined_regs(Linear, Def, Regs) ->
    def_regs(Linear, #{0 => Def}, Regs).

def_regs([{L,#cg_blk{is=Is0,last=Last}=Blk0}|Bs], DefMap0, Regs) ->
    Def0 = map_get(L, DefMap0),
    {Is,Def,MaybeDef} = def_regs_is(Is0, Regs, Def0, []),
    DefMap = def_successors(Last, Def, MaybeDef, DefMap0),
    Blk = Blk0#cg_blk{is=Is},
    [{L,Blk}|def_regs(Bs, DefMap, Regs)];
def_regs([], _, _) -> [].

def_regs_is([#cg_alloc{live=Live}=I|Is], Regs, Def0, Acc) when is_integer(Live) ->
    Def = trim_xregs(Def0, Live, Regs),
    def_regs_is(Is, Regs, Def, [I|Acc]);
def_regs_is([#cg_set{op=succeeded,args=[Var]}=I], _Regs, Def, Acc) ->
    %% Var will only be defined on the success branch of the `br`
    %% for this block.
    MaybeDef = [Var],
    {reverse(Acc, [I]),Def,MaybeDef};
def_regs_is([#cg_set{op=kill_try_tag,args=[#b_var{}=Tag]}=I|Is], Regs, Def0, Acc) ->
    Def = ordsets:del_element(Tag, Def0),
    def_regs_is(Is, Regs, Def, [I|Acc]);
def_regs_is([#cg_set{op=catch_end,dst=Dst,args=[#b_var{}=Tag|_]}=I|Is], Regs, Def0, Acc) ->
    Def1 = trim_xregs(Def0, 0, Regs),
    Def2 = kill_regs(Def1, [Dst,Tag], Regs),
    Def = ordsets:add_element(Dst, Def2),
    def_regs_is(Is, Regs, Def, [I|Acc]);
def_regs_is([#cg_set{anno=Anno0,op=debug_line}=I0|Is], Regs, Def, Acc) ->
    Anno = Anno0#{def_regs => Def},
    I = I0#cg_set{anno=Anno},
    def_regs_is(Is, Regs, Def, [I|Acc]);
def_regs_is([#cg_set{anno=Anno,dst=Dst,op={bif,Bif},args=Args}=I|Is], Regs, Def0, Acc) ->
    Def1 = case is_gc_bif(Bif, Args) of
               true ->
                   #{live := Live} = Anno,
                   trim_xregs(Def0, Live, Regs);
               false ->
                   Def0
           end,
    case Regs of
        #{Dst := {Tag,_}=R} when Tag =:= x; Tag =:= y ->
            Def2 = kill_reg(Def1, R, Regs),
            Def = ordsets:add_element(Dst, Def2),
            def_regs_is(Is, Regs, Def, [I|Acc]);
        #{} ->
            def_regs_is(Is, Regs, Def1, [I|Acc])
    end;
def_regs_is([#cg_set{anno=Anno,dst=Dst}=I|Is], Regs, Def0, Acc) ->
    Def1 = case Anno of
               #{live := Live} -> trim_xregs(Def0, Live, Regs);
               #{} -> Def0
           end,
    Def2 = case Anno of
               #{kill_yregs := KillYregs} ->
                   kill_regs(Def1, KillYregs, Regs);
               #{} ->
                   Def1
           end,
    case Anno of
        #{clobbers := true} ->
            Def3 = trim_xregs(Def2, 0, Regs),
            Def = case Regs of
                      #{Dst := {Tag,_}=R} when Tag =:= x; Tag =:= y ->
                          Def4 = kill_reg(Def3, R, Regs),
                          ordsets:add_element(Dst, Def4);
                      #{} ->
                          Def3
                  end,
            def_regs_is(Is, Regs, Def, [I|Acc]);
        #{} ->
            case Regs of
                #{Dst := {Tag,_}=R} when Tag =:= x; Tag =:= y ->
                    Def3 = kill_reg(Def2, R, Regs),
                    Def = ordsets:add_element(Dst, Def3),
                    def_regs_is(Is, Regs, Def, [I|Acc]);
                #{} ->
                    def_regs_is(Is, Regs, Def1, [I|Acc])
            end
    end;
def_regs_is([], _Regs, Def, Acc) ->
    {reverse(Acc),Def,[]}.

trim_xregs([V|Vs], Live, Regs) ->
    case Regs of
        #{V := {x,R}} when R < Live ->
            [V|trim_xregs(Vs, Live, Regs)];
        #{V := {y,_}}->
            [V|trim_xregs(Vs, Live, Regs)];
        #{} ->
            trim_xregs(Vs, Live, Regs)
    end;
trim_xregs([], _, _) -> [].

kill_reg([V|Vs], R, Regs) ->
    case Regs of
        #{V := R} -> Vs;
        #{} -> [V|kill_reg(Vs, R, Regs)]
    end;
kill_reg([], _, _) -> [].

kill_regs(Defs, KillRegs0, Regs) ->
    KillRegs = #{map_get(V, Regs) => [] || V <- KillRegs0},
    [D || D <- Defs, not is_map_key(map_get(D, Regs), KillRegs)].

%%%
%%% Here follows the main code generation functions.
%%%

%% cg_linear([{BlockLabel,Block}]) -> [BeamInstruction].
%%  Generate BEAM instructions.

cg_linear([{L, #cg_blk{is=[#cg_set{op=peek_message,
                                   args=[Marker]}=Peek | Is0]}=B0} | Bs],
          St0)->
    B = B0#cg_blk{is=[Peek#cg_set{args=[]} | Is0]},
    {Is, St} = cg_linear([{L, B} | Bs], St0),
    case Marker of
        #b_literal{val=Val} ->
            none = Val,                         %Assertion.
            {Is, St};
        _ ->
            %% We never jump directly into receive loops so we can be certain
            %% that recv_marker_use/1 is always executed despite preceding
            %% the loop label. This is verified by the validator.
            Reg = beam_arg(Marker, St0),
            {[{recv_marker_use, Reg} | Is], St}
    end;
cg_linear([{L,#cg_blk{is=Is0,last=Last}}|Bs], St0) ->
    Next = next_block(Bs),
    St1 = new_block_label(L, St0),
    {Is1,St2} = cg_block(Is0, Last, Next, St1),
    {Is2,St} = cg_linear(Bs, St2),
    {def_block_label(L, St)++Is1++Is2,St};
cg_linear([], St) -> {[],St}.

cg_block([#cg_set{op=recv_next}], #cg_br{succ=Lr0}, _Next, St0) ->
    {Lr,St} = use_block_label(Lr0, St0),
    {[{loop_rec_end,Lr}],St};
cg_block([#cg_set{op=wait_timeout,
                  args=[#b_literal{val=infinity}]}],
         Last, _Next, St0) ->
    %% 'infinity' will never time out, so we'll simplify this to a 'wait'
    %% instruction that always jumps back to peek_message (fail label).
    #cg_br{fail=Lr0} = Last,
    {Lr,St} = use_block_label(Lr0, St0),
    {[{wait,Lr}],St};
cg_block(Is0, Last, Next, St0) ->
    case Last of
        #cg_br{succ=Next,fail=Next} ->
            cg_block(Is0, none, St0);
        #cg_br{succ=Same,fail=Same} when Same =:= ?EXCEPTION_BLOCK ->
            %% An expression in this block *always* throws an exception, so we
            %% terminate it with an 'if_end' to make sure the validator knows
            %% that the following instructions won't actually be reached.
            {Is,St} = cg_block(Is0, none, St0),
            {Is++[if_end],St};
        #cg_br{succ=Same,fail=Same} ->
            {Fail,St1} = use_block_label(Same, St0),
            {Is,St} = cg_block(Is0, none, St1),
            {Is++[jump(Fail)],St};
        #cg_br{bool=Bool,succ=Next,fail=Fail0} ->
            {Fail,St1} = use_block_label(Fail0, St0),
            {Is,St} = cg_block(Is0, {Bool,Fail}, St1),
            {Is,St};
        #cg_br{bool=Bool,succ=Succ0,fail=Fail0} ->
            {[Succ,Fail],St1} = use_block_labels([Succ0,Fail0], St0),
            {Is,St} = cg_block(Is0, {Bool,Fail}, St1),
            {Is++[jump(Succ)],St};
        #cg_ret{arg=Src0,dealloc=N} ->
            Src = beam_arg(Src0, St0),
            cg_block(Is0, {return,Src,N}, St0);
        #cg_switch{} ->
            cg_switch(Is0, Last, St0)
    end.

cg_switch(Is0, Last, St0) ->
    #cg_switch{anno=Anno,arg=Src0,fail=Fail0,list=List0} = Last,
    Src1 = beam_arg(Src0, St0),
    {Fail1,St1} = use_block_label(Fail0, St0),
    Fail = ensure_label(Fail1, St1),
    {List1,St2} =
        flatmapfoldl(fun({V,L}, S0) ->
                             {Lbl,S} = use_block_label(L, S0),
                             {[beam_arg(V, S),Lbl],S}
                     end, St1, List0),
    {Is1,St} = cg_block(Is0, none, St2),
    case reverse(Is1) of
        [{bif,tuple_size,_,[Tuple],{z,_}=Src1}|More] ->
            List = map(fun({integer,Arity}) -> Arity;
                          ({f,_}=F) -> F
                       end, List1),
            Is = reverse(More, [{select_tuple_arity,Tuple,Fail,{list,List}}]),
            {Is,St};
        _ ->
            [Src] = typed_args([Src0], Anno, St),
            SelectVal = {select_val,Src,Fail,{list,List1}},
            {Is1 ++ [SelectVal],St}
    end.

jump({f,_}=Fail) ->
    {jump,Fail};
jump({catch_tag,Fail}) ->
    {jump,Fail}.

bif_fail({f,_}=Fail) -> Fail;
bif_fail({catch_tag,_}) -> {f,0}.

next_block([]) -> none;
next_block([{Next,_}|_]) -> Next.

%% Certain instructions (such as get_map_element or is_nonempty_list)
%% are only used in guards and **must** have a non-zero label;
%% otherwise, the loader will refuse to load the
%% module. ensure_label/2 replaces a zero label with the "ultimate
%% failure" label to make the module loadable.  The instruction that
%% have had the zero label replaced is **not** supposed to ever fail
%% and actually jump to the label.

ensure_label(Fail0, #cg{ultimate_fail=Lbl}) ->
    case bif_fail(Fail0) of
        {f,0} -> {f,Lbl};
        {f,_}=Fail -> Fail
    end.

cg_block([#cg_set{op=new_try_tag,dst=Tag,args=Args}], {Tag,Fail0}, St) ->
    {catch_tag,Fail} = Fail0,
    [Reg,{atom,Kind}] = beam_args([Tag|Args], St),
    {[{Kind,Reg,Fail}],St};
cg_block([#cg_set{anno=Anno,op={bif,Name},dst=Dst0,args=Args0}=I,
          #cg_set{op=succeeded,dst=Bool}], {Bool,Fail0}, St) ->
    Args = typed_args(Args0, Anno, St),
    Dst = beam_arg(Dst0, St),
    Line0 = call_line(body, {extfunc,erlang,Name,length(Args)}, Anno),
    Fail = bif_fail(Fail0),
    Line = case Fail of
               {f,0} -> Line0;
               {f,_} -> []
           end,
    case is_gc_bif(Name, Args) of
        true ->
            Live = get_live(I),
            Kill = kill_yregs(Anno, St),
            {Kill++Line++[{gc_bif,Name,Fail,Live,Args,Dst}],St};
        false ->
            {Line++[{bif,Name,Fail,Args,Dst}],St}
    end;
cg_block([#cg_set{op={bif,tuple_size},dst=Arity0,args=[Tuple0]},
          #cg_set{op={bif,'=:='},dst=Bool,args=[Arity0,#b_literal{val=Ar}]}=Eq],
         {Bool,Fail}=Context, St0) ->
    Tuple = beam_arg(Tuple0, St0),
    case beam_arg(Arity0, St0) of
        {z,_} ->
            %% The size will only be used once. Combine to a test_arity instruction.
            Test = {test,test_arity,ensure_label(Fail, St0),[Tuple,Ar]},
            {[Test],St0};
        Arity ->
            %% The size will be used more than once. Must do an explicit
            %% BIF call followed by the '==' test.
            TupleSize = {bif,tuple_size,{f,0},[Tuple],Arity},
            {Is,St} = cg_block([Eq], Context, St0),
            {[TupleSize|Is],St}
    end;
cg_block([#cg_set{anno=Anno,op={bif,Name},dst=Dst0,args=Args0}]=Is0,
         {Dst0,Fail}, St0) ->
    Args = typed_args(Args0, Anno, St0),
    case beam_arg(Dst0, St0) of
        {z,_} ->
            %% The result of the BIF call will only be used once. Convert to
            %% a test instruction.
            {Test,St1} = bif_to_test(Name, Args, ensure_label(Fail, St0), St0),
            {Test,St1};
        _ ->
            %% Must explicitly call the BIF since the result will be used
            %% more than once.
            {Is1,St1} = cg_block(Is0, none, St0),
            {Is2,St} = cg_block([], {Dst0,Fail}, St1),
            {Is1++Is2,St}
    end;
cg_block([#cg_set{anno=Anno,op={bif,Name},dst=Dst0,args=Args0}=I|T],
         Context, St0) ->
    Args = typed_args(Args0, Anno, St0),
    Dst = beam_arg(Dst0, St0),
    {Is0,St} = cg_block(T, Context, St0),
    case is_gc_bif(Name, Args) of
        true ->
            Line = call_line(body, {extfunc,erlang,Name,length(Args)}, Anno),
            Live = get_live(I),
            Kill = kill_yregs(Anno, St),
            Is = Kill++Line++[{gc_bif,Name,{f,0},Live,Args,Dst}|Is0],
            {Is,St};
        false ->
            Bif = case {Name,Args} of
                      {'not',[{tr,_,#t_atom{elements=[false,true]}}=Arg]} ->
                          {bif,'=:=',{f,0},[Arg,{atom,false}],Dst};
                      {_,_} ->
                          {bif,Name,{f,0},Args,Dst}
                  end,
            Is = [Bif|Is0],
            {Is,St}
    end;
cg_block([#cg_set{op=bs_create_bin,dst=Dst0,args=Args0,anno=Anno}=I,
          #cg_set{op=succeeded,dst=Bool}], {Bool,Fail0}, St) ->
    Args1 = typed_args(Args0, Anno, St),
    Fail = bif_fail(Fail0),
    Line = line(Anno),
    Alloc = map_get(alloc, Anno),
    Live = get_live(I),
    Dst = beam_arg(Dst0, St),
    Args = bs_args(Args1),
    Unit0 = maps:get(unit, Anno, 1),
    Unit = case Args of
               [{atom,append},_Seg,U|_] ->
                   max(U, Unit0);
               [{atom,private_append},_Seg,U|_] ->
                   max(U, Unit0);
               _ ->
                   Unit0
           end,
    TypeInfo = case Anno of
                   #{result_type := #t_bitstring{appendable=true}=Type} ->
                       [{'%',{var_info,Dst,[{type,Type}]}}];
                   _ ->
                       []
               end,
    Is = [Line,{bs_create_bin,Fail,Alloc,Live,Unit,Dst,{list,Args}}],
    {Is++TypeInfo,St};
cg_block([#cg_set{op=bs_start_match,
                  dst=Ctx0,
                  args=[#b_literal{val=new},Bin0]}=I,
          #cg_set{op=succeeded,dst=Bool}], {Bool,Fail}, St) ->
    [Dst,Bin1] = beam_args([Ctx0,Bin0], St),
    {Bin,Pre} = force_reg(Bin1, Dst),
    Live = get_live(I),
    Is = Pre ++ [{test,bs_start_match3,Fail,Live,[Bin],Dst}],
    {Is,St};
cg_block([#cg_set{op=bs_ensure,args=Ss0},
          #cg_set{op=succeeded,dst=Bool}], {Bool,Fail}, St) ->
    %% Temporary instruction that will be incorporated into a bs_match
    %% instruction by the bs_translate sub pass.
    [Ctx,{integer,Size},{integer,Unit}] = beam_args(Ss0, St),
    Is = [{test,bs_ensure,Fail,[Ctx,Size,Unit]}],
    {Is,St};
cg_block([#cg_set{op=bs_get}=Set,
          #cg_set{op=succeeded,dst=Bool}], {Bool,Fail}, St) ->
    {cg_bs_get(Fail, Set, St),St};
cg_block([#cg_set{op=Op,args=[CtxVar,#b_literal{val=String0}]},
          #cg_set{op=succeeded,dst=Bool}], {Bool,Fail}, St)
  when Op =:= bs_match_string; Op =:= bs_ensured_match_string ->
    CtxReg = beam_arg(CtxVar, St),

    Bits = bit_size(String0),
    String = case Bits rem 8 of
                 0 -> String0;
                 Rem -> <<String0/bitstring,0:(8-Rem)>>
             end,

    Is = [{test,Op,Fail,[CtxReg,Bits,{string,String}]}],
    {Is,St};
cg_block([#cg_set{dst=Dst0,op=landingpad,args=Args0}|T], Context, St0) ->
    [Dst,{atom,Kind},Tag] = beam_args([Dst0|Args0], St0),
    case Kind of
        'catch' ->
            cg_catch(Dst, T, Context, St0);
        'try' ->
            cg_try(Dst, Tag, T, Context, St0)
    end;
cg_block([#cg_set{op=kill_try_tag,args=Args0}|Is], Context, St0) ->
    [Reg] = beam_args(Args0, St0),
    {Is0,St} = cg_block(Is, Context, St0),
    {[{try_end,Reg}|Is0],St};
cg_block([#cg_set{op=catch_end,dst=Dst0,args=Args0}|Is], Context, St0) ->
    [Dst,Reg,{x,0}] = beam_args([Dst0|Args0], St0),
    {Is0,St} = cg_block(Is, Context, St0),
    {[{catch_end,Reg}|copy({x,0}, Dst)++Is0],St};
cg_block([#cg_set{op=call}=I,
          #cg_set{op=succeeded,dst=Bool}], {Bool,_Fail}, St) ->
    %% A call in try/catch block.
    cg_block([I], none, St);
cg_block([#cg_set{op=match_fail}=I,
          #cg_set{op=succeeded,dst=Bool}], {Bool,_Fail}, St) ->
    %% A match_fail instruction in a try/catch block.
    cg_block([I], none, St);
cg_block([#cg_set{op=get_map_element,dst=Dst0,args=Args0,anno=Anno},
          #cg_set{op=succeeded,dst=Bool}], {Bool,Fail0}, St) ->
    [Map,Key] = typed_args(Args0, Anno, St),
    Dst = beam_arg(Dst0, St),
    Fail = ensure_label(Fail0, St),
    {[{get_map_elements,Fail,Map,{list,[Key,Dst]}}],St};
cg_block([#cg_set{op={float,convert},dst=Dst0,args=Args0,anno=Anno},
          #cg_set{op=succeeded,dst=Bool}], {Bool,Fail}, St) ->
    {f,0} = bif_fail(Fail),                     %Assertion.
    [Src] = typed_args(Args0, Anno, St),
    Dst = beam_arg(Dst0, St),
    {[line(Anno),{fconv,Src,Dst}], St};
cg_block([#cg_set{op=bs_skip,args=Args0,anno=Anno}=I,
          #cg_set{op=succeeded,dst=Bool}], {Bool,Fail}, St) ->
    Args = typed_args(Args0, Anno, St),
    {cg_bs_skip(bif_fail(Fail), Args, I),St};
cg_block([#cg_set{op=Op,dst=Dst0,args=Args0}=I,
          #cg_set{op=succeeded,dst=Bool}], {Bool,Fail}, St) ->
    [Dst|Args] = beam_args([Dst0|Args0], St),
    {cg_test(Op, bif_fail(Fail), Args, Dst, I),St};
cg_block([#cg_set{op=bs_test_tail,dst=Bool,args=Args0}], {Bool,Fail}, St) ->
    [Ctx,{integer,Bits}] = beam_args(Args0, St),
    {[{test,bs_test_tail2,bif_fail(Fail),[Ctx,Bits]}],St};
cg_block([#cg_set{op=is_tagged_tuple,anno=Anno,dst=Bool,args=Args0}], {Bool,Fail}, St) ->
    case Anno of
        #{constraints := arity} ->
            [Src,{integer,Arity},_Tag] = beam_args(Args0, St),
            {[{test,test_arity,ensure_label(Fail, St),[Src,Arity]}],St};
        #{constraints := tuple_arity} ->
            [Src,{integer,Arity},_Tag] = beam_args(Args0, St),
            {[{test,is_tuple,ensure_label(Fail, St),[Src]},
              {test,test_arity,ensure_label(Fail, St),[Src,Arity]}],St};
        #{} ->
            [Src,{integer,Arity},Tag] = typed_args(Args0, Anno, St),
            {[{test,is_tagged_tuple,ensure_label(Fail, St),[Src,Arity,Tag]}],St}
    end;
cg_block([#cg_set{op=is_nonempty_list,dst=Bool0,args=Args0}=Set], {Bool0,Fail0}, St) ->
    Fail = ensure_label(Fail0, St),
    Args = beam_args(Args0, St),
    case beam_args([Bool0|Args0], St) of
        [{z,0}|Args] ->
            {[{test,is_nonempty_list,Fail,Args}],St};
        [Dst|Args] ->
            %% This instruction was a call to is_list/1, which was
            %% rewritten to an is_nonempty_list test by
            %% beam_ssa_type. BEAM has no is_nonempty_list instruction
            %% that will return a boolean, so we must revert it to an
            %% is_list/1 call.
            #cg_set{anno=#{was_bif_is_list := true}} = Set, %Assertion.
            {[{bif,is_list,Fail0,Args,Dst},
              {test,is_eq_exact,Fail,[Dst,{atom,true}]}],St}
    end;
cg_block([#cg_set{op=has_map_field,dst=Dst0,args=Args0}], {Dst0,Fail0}, St) ->
    Fail = ensure_label(Fail0, St),
    case beam_args([Dst0|Args0], St) of
        [{z,0},Src,Key] ->
            {[{test,has_map_fields,Fail,Src,{list,[Key]}}],St};
        [Dst,Src,Key] ->
            %% The result is used more than once. Must rewrite to bif:is_map_key
            %% to set the destination register.
            {[{bif,is_map_key,Fail0,[Key,Src],Dst},
              {test,is_eq_exact,Fail,[Dst,{atom,true}]}],St}
    end;
cg_block([#cg_set{op=call}=Call], {_Bool,_Fail}=Context, St0) ->
    {Is0,St1} = cg_call(Call, body, none, St0),
    {Is1,St} = cg_block([], Context, St1),
    {Is0++Is1,St};
cg_block([#cg_set{op=call,dst=Dst0}=Call], Context, St) ->
    Dst = beam_arg(Dst0, St),
    case Context of
        {return,Dst,_} ->
            cg_call(Call, tail, Context, St);
        _ ->
            cg_call(Call, body, Context, St)
    end;
cg_block([#cg_set{op=call}=Call|T], Context, St0) ->
    {Is0,St1} = cg_call(Call, body, none, St0),
    {Is1,St} = cg_block(T, Context, St1),
    {Is0++Is1,St};
cg_block([#cg_set{anno=Anno,op=make_fun,dst=Dst0,args=[Local|Args0]}|T],
         Context, St0) ->
    #b_local{name=#b_literal{val=Func},arity=Arity} = Local,
    [Dst|Args] = beam_args([Dst0|Args0], St0),
    {FuncLbl,St1} = local_func_label(Func, Arity, St0),
    Is0 = [{make_fun3,{f,FuncLbl},0,0,Dst,{list,Args}}],
    Is1 = case Anno of
              #{result_type := Type} ->
                  Info = {var_info, Dst, [{fun_type, Type}]},
                  Is0 ++ [{'%', Info}];
              #{} ->
                  Is0
          end,
    {Is2,St} = cg_block(T, Context, St1),
    {Is1++Is2,St};
cg_block([#cg_set{op=copy}|_]=T0, Context, St0) ->
    {Is0,T} = cg_copy(T0, St0),
    {Is1,St} = cg_block(T, Context, St0),
    Is = Is0 ++ Is1,
    case is_call(T) of
        {yes,Arity} ->
            {opt_call_moves(Is, Arity),St};
        no ->
            {Is,St}
    end;
cg_block([#cg_set{op=match_fail,args=Args0,anno=Anno}], none, St) ->
    Args = beam_args(Args0, St),
    Is = cg_match_fail(Args, line(Anno), none),
    {Is,St};
cg_block([#cg_set{op=match_fail,args=Args0,anno=Anno}|T], Context, St0) ->
    FcLabel = case Context of
                  {return,_,none} ->
                      %% There is no stack frame. If this is a function_clause
                      %% exception, it is safe to jump to the label of the
                      %% func_info instruction.
                      St0#cg.fc_label;
                  _ ->
                      %% This is most probably not a function_clause.
                      %% If this is a function_clause exception
                      %% (rare), it is not safe to jump to the
                      %% func_info label.
                      none
              end,
    Args = beam_args(Args0, St0),
    Is0 = cg_match_fail(Args, line(Anno), FcLabel),
    {Is1,St} = cg_block(T, Context, St0),
    {Is0++Is1,St};
cg_block([#cg_set{op=wait_timeout,dst=Bool,args=Args0}], {Bool,Fail}, St) ->
    Is = case beam_args(Args0, St) of
             [{integer,0}] ->
                 [timeout];
             [Timeout] ->
                 true = Timeout =/= {atom,infinity},
                 [{wait_timeout,Fail,Timeout},timeout]
         end,
    {Is,St};
cg_block([#cg_set{op=has_map_field,dst=Dst0,args=Args0,anno=Anno}|T], Context, St0) ->
    [Map,Key] = typed_args(Args0, Anno, St0),
    Dst = beam_arg(Dst0, St0),
    I = {bif,is_map_key,{f,0},[Key,Map],Dst},
    {Is0,St} = cg_block(T, Context, St0),
    Is = [I|Is0],
    {Is,St};
cg_block([#cg_set{op=update_record,dst=Dst0,args=Args0,anno=Anno}|T], Context, St0) ->
    Args = typed_args(Args0, Anno, St0),
    Dst = beam_arg(Dst0, St0),
    [Hint,{integer,Size},Src|Ss0] = Args,
    Ss = cg_update_record_list(Ss0),
    I = {update_record,Hint,Size,Src,Dst,{list,Ss}},
    {Is1,St} = cg_block(T, Context, St0),
    {[I|Is1],St};
cg_block([#cg_set{op=Op,dst=Dst0,args=Args0}=Set], none, St) ->
    [Dst|Args] = beam_args([Dst0|Args0], St),
    Is = cg_instr(Op, Args, Dst, Set),
    {Is,St};
cg_block([#cg_set{op=Op,dst=Dst0,args=Args0}=Set|T], Context, St0) ->
    [Dst|Args] = beam_args([Dst0|Args0], St0),
    Is0 = cg_instr(Op, Args, Dst, Set),
    {Is1,St} = cg_block(T, Context, St0),
    {Is0++Is1,St};
cg_block([#cg_alloc{}=Alloc|T], Context, St0) ->
    Is0 = cg_alloc(Alloc, St0),
    {Is1,St} = cg_block(T, Context, St0),
    {Is0++Is1,St};
cg_block([], {return,Arg,none}, St) ->
    Is = copy(Arg, {x,0}) ++ [return],
    {Is,St};
cg_block([], {return,Arg,N}, St) ->
    Is = copy(Arg, {x,0}) ++ [{deallocate,N},return],
    {Is,St};
cg_block([], none, St) ->
    {[],St};
cg_block([], {Bool0,Fail}, St) ->
    [Bool] = beam_args([Bool0], St),
    {[{test,is_eq_exact,Fail,[Bool,{atom,true}]}],St}.

bs_args([{atom,binary},{literal,[1|_]},{literal,Bs},{atom,all}|Args])
  when bit_size(Bs) =:= 0 ->
    bs_args(Args);
bs_args([{atom,binary},{literal,[1|_]}=UFs,{literal,Bs},{atom,all}|Args0])
  when is_bitstring(Bs) ->
    Bits = bit_size(Bs),
    Bytes = Bits div 8,
    case Bits rem 8 of
        0 ->
            [{atom,string},0,8,nil,{string,Bs},{integer,byte_size(Bs)}|bs_args(Args0)];
        Rem ->
            <<Binary:Bytes/bytes,Int:Rem>> = Bs,
            Args = [{atom,binary},UFs,{literal,Binary},{atom,all},
                    {atom,integer},{literal,[1]},{integer,Int},{integer,Rem}|Args0],
            bs_args(Args)
    end;
bs_args([Type,{literal,[Unit|Fs0]},Val,Size|Args]) ->
    Segment = proplists:get_value(segment, Fs0, 0),
    Fs1 = proplists:delete(segment, Fs0),
    Fs = case Fs1 of
             [] -> nil;
             [_|_] -> {literal,Fs1}
         end,
    [Type,Segment,Unit,Fs,Val,Size|bs_args(Args)];
bs_args([]) -> [].

cg_copy(T0, St) ->
    {Copies,T} = splitwith(fun(#cg_set{op=copy}) -> true;
                              (_) -> false
                           end, T0),
    Moves0 = cg_copy_1(Copies, St),
    Moves1 = [Move || {move,Src,Dst}=Move <- Moves0, Src =/= Dst],
    Moves = order_moves(Moves1),
    {Moves,T}.

cg_copy_1([#cg_set{dst=Dst0,args=Args}|T], St) ->
    [Dst,Src] = beam_args([Dst0|Args], St),
    Copies = cg_copy_1(T, St),
    case keymember(Dst, 3, Copies) of
        true ->
            %% Will be overwritten. Don't generate a move instruction.
            Copies;
        false ->
            [{move,Src,Dst}|Copies]
    end;
cg_copy_1([], _St) -> [].

-define(IS_LITERAL(Val), (Val =:= nil orelse
                          element(1, Val) =:= integer orelse
                          element(1, Val) =:= float orelse
                          element(1, Val) =:= atom orelse
                          element(1, Val) =:= literal)).

bif_to_test(min, Args, Fail, St) ->
    %% The min/2 and max/2 BIFs can only be rewritten to tests when
    %% both arguments are known to be booleans.
    bif_to_test('and', Args, Fail, St);
bif_to_test(max, Args, Fail, St) ->
    bif_to_test('or', Args, Fail, St);
bif_to_test('or', [V1,V2], {f,Lbl}=Fail, St0) when Lbl =/= 0 ->
    {SuccLabel,St} = new_label(St0),
    {[{test,is_eq_exact,{f,SuccLabel},[V1,{atom,false}]},
      {test,is_eq_exact,Fail,[V2,{atom,true}]},
      {label,SuccLabel}],St};
bif_to_test(Op, Args, Fail, St) ->
    {bif_to_test(Op, Args, Fail),St}.

bif_to_test('and', [V1,V2], Fail) ->
    [{test,is_eq_exact,Fail,[V1,{atom,true}]},
     {test,is_eq_exact,Fail,[V2,{atom,true}]}];
bif_to_test('not', [Var], Fail) ->
    [{test,is_eq_exact,Fail,[Var,{atom,false}]}];
bif_to_test(Name, Args, Fail) ->
    [bif_to_test_1(Name, Args, Fail)].

bif_to_test_1(is_atom,     [_]=Ops, Fail) ->
    {test,is_atom,Fail,Ops};
bif_to_test_1(is_boolean,  [_]=Ops, Fail) ->
    {test,is_boolean,Fail,Ops};
bif_to_test_1(is_binary,   [_]=Ops, Fail) ->
    {test,is_binary,Fail,Ops};
bif_to_test_1(is_bitstring,[_]=Ops, Fail) ->
    {test,is_bitstr,Fail,Ops};
bif_to_test_1(is_float,    [_]=Ops, Fail) ->
    {test,is_float,Fail,Ops};
bif_to_test_1(is_function, [_]=Ops, Fail) ->
    {test,is_function,Fail,Ops};
bif_to_test_1(is_function, [_,_]=Ops, Fail) ->
    {test,is_function2,Fail,Ops};
bif_to_test_1(is_integer,  [_]=Ops, Fail) ->
    {test,is_integer,Fail,Ops};
bif_to_test_1(is_list,     [_]=Ops, Fail) ->
    {test,is_list,Fail,Ops};
bif_to_test_1(is_map,      [_]=Ops, Fail) ->
    {test,is_map,Fail,Ops};
bif_to_test_1(is_number,   [_]=Ops, Fail) ->
    {test,is_number,Fail,Ops};
bif_to_test_1(is_pid,      [_]=Ops, Fail) ->
    {test,is_pid,Fail,Ops};
bif_to_test_1(is_port,     [_]=Ops, Fail) ->
    {test,is_port,Fail,Ops};
bif_to_test_1(is_reference, [_]=Ops, Fail) ->
    {test,is_reference,Fail,Ops};
bif_to_test_1(is_tuple,    [_]=Ops, Fail) ->
    {test,is_tuple,Fail,Ops};
bif_to_test_1('=<', [A,B], Fail) ->
    {test,is_ge,Fail,[B,A]};
bif_to_test_1('>', [A,B], Fail) ->
    {test,is_lt,Fail,[B,A]};
bif_to_test_1('<', [_,_]=Ops, Fail) ->
    {test,is_lt,Fail,Ops};
bif_to_test_1('>=', [_,_]=Ops, Fail) ->
    {test,is_ge,Fail,Ops};
bif_to_test_1('==', [C,A], Fail) when ?IS_LITERAL(C) ->
    {test,is_eq,Fail,[A,C]};
bif_to_test_1('==', [_,_]=Ops, Fail) ->
    {test,is_eq,Fail,Ops};
bif_to_test_1('/=', [C,A], Fail) when ?IS_LITERAL(C) ->
    {test,is_ne,Fail,[A,C]};
bif_to_test_1('/=', [_,_]=Ops, Fail) ->
    {test,is_ne,Fail,Ops};
bif_to_test_1('=:=', [C,A], Fail) when ?IS_LITERAL(C) ->
    {test,is_eq_exact,Fail,[A,C]};
bif_to_test_1('=:=', [_,_]=Ops, Fail) ->
    {test,is_eq_exact,Fail,Ops};
bif_to_test_1('=/=', [C,A], Fail) when ?IS_LITERAL(C) ->
    {test,is_ne_exact,Fail,[A,C]};
bif_to_test_1('=/=', [_,_]=Ops, Fail) ->
    {test,is_ne_exact,Fail,Ops}.

opt_call_moves(Is0, Arity) ->
    {Moves0,Is} = splitwith(fun({move,_,_}) -> true;
                               ({init_yregs,_}) -> true;
                               (_) -> false
                            end, Is0),
    Moves = opt_call_moves_1(Moves0, Arity),
    Moves ++ Is.

opt_call_moves_1([{move,Src,{x,_}=Tmp}=M1,
                  {init_yregs,{list,Yregs}}=Init|Is], Arity) ->
    %% There could be a {move,Tmp,{x,0}} instruction after the
    %% init_yreg/1 instruction (moved to there by opt_move_to_x0/1).
    case Is of
        [{move,{x,_}=Tmp,{x,0}}=M2] ->
            %% The two move/2 instructions (M1 and M2) can be combined
            %% to one. The question is, though, is it safe to place
            %% them after the kill/1 instructions?
            case member(Src, Yregs) of
                true ->
                    %% Src (a Y register) is killed by the
                    %% init_yregs/1 instruction. Thus M1 and M2 must
                    %% be placed before the kill/1 instructions
                    %% (essentially undoing what opt_move_to_x0/1 did,
                    %% which turned out to be a pessimization in this
                    %% case).
                    opt_call_moves_1([M1,M2,Init], Arity);
                false ->
                    %% Src is not killed by the init_yregs/1
                    %% instructions. Thus it is safe to place
                    %% M1 and M2 after the kill/1 instructions.
                    opt_call_moves_1([Init,M1,M2], Arity)
            end;
        _ ->
            [M1,Init|Is]
    end;
opt_call_moves_1([{move,Src,{x,_}=Tmp}=M1,{move,Tmp,Dst}=M2|Is], Arity) ->
    case is_killed(Tmp, Is, Arity) of
        true ->
            %% The X register Tmp is never used again. We can collapse
            %% the two move instruction into one.
            [{move,Src,Dst}|opt_call_moves_1(Is, Arity)];
        false ->
            [M1|opt_call_moves_1([M2|Is], Arity)]
    end;
opt_call_moves_1([M|Ms], Arity) ->
    [M|opt_call_moves_1(Ms, Arity)];
opt_call_moves_1([], _Arity) -> [].

is_killed(R, [{move,R,_}|_], _) ->
    false;
is_killed(R, [{move,_,R}|_], _) ->
    true;
is_killed(R, [{move,_,_}|Is], Arity) ->
    is_killed(R, Is, Arity);
is_killed({x,_}=R, [{init_yregs,_}|Is], Arity) ->
    is_killed(R, Is, Arity);
is_killed({x,X}, [], Arity) ->
    X >= Arity.

cg_alloc(#cg_alloc{stack=none,words=#need{h=0,l=0,f=0}}, _St) ->
    [];
cg_alloc(#cg_alloc{stack=none,words=Need,live=Live}, _St) ->
    [{test_heap,alloc(Need),Live}];
cg_alloc(#cg_alloc{stack=Stk,words=Need,live=Live,def_yregs=DefYregs},
         #cg{regs=Regs}) when is_integer(Stk) ->
    Alloc = alloc(Need),
    All = [{y,Y} || Y <- lists:seq(0, Stk-1)],
    Def = ordsets:from_list([maps:get(V, Regs) || V <- DefYregs]),
    NeedInit = ordsets:subtract(All, Def),
    I = case Alloc of
            0 -> {allocate,Stk,Live};
            _ -> {allocate_heap,Stk,Alloc,Live}
        end,
    [I|init_yregs(NeedInit)].

init_yregs([_|_]=Yregs) ->
    [{init_yregs,{list,Yregs}}];
init_yregs([]) -> [].

alloc(#need{h=Words,l=0,f=0}) ->
    Words;
alloc(#need{h=Words,l=Lambdas,f=Floats}) ->
    {alloc,[{words,Words},{floats,Floats},{funs,Lambdas}]}.

is_call([#cg_set{op=call,args=[#b_var{}|Args]}|_]) ->
    {yes,1+length(Args)};
is_call([#cg_set{op=call,args=[_|Args]}|_]) ->
    {yes,length(Args)};
is_call(_) ->
    no.

cg_call(#cg_set{anno=Anno,op=call,dst=Dst0,args=[#b_local{}=Func0|Args0]},
        Where, Context, St0) ->
    [Dst|Args] = beam_args([Dst0|Args0], St0),
    #b_local{name=Name0,arity=Arity} = Func0,
    {atom,Name} = beam_arg(Name0, St0),
    {FuncLbl,St} = local_func_label(Name, Arity, St0),
    Line = call_line(Where, local, Anno),
    Call = build_call(call, Arity, {f,FuncLbl}, Context, Dst),
    Is = setup_args(Args, Anno, Context, St) ++ Line ++ Call,
    case Anno of
        #{ result_type := Type } ->
            Info = {var_info, Dst, [{type,Type}]},
            {Is ++ [{'%', Info}], St};
        #{} ->
            {Is, St}
    end;
cg_call(#cg_set{anno=Anno0,op=call,dst=Dst0,args=[#b_remote{}=Func0|Args0]},
        Where, Context, St) ->
    [Dst|Args] = beam_args([Dst0|Args0], St),
    #b_remote{mod=Mod0,name=Name0,arity=Arity} = Func0,
    case {beam_arg(Mod0, St),beam_arg(Name0, St)} of
        {{atom,Mod},{atom,Name}} ->
            Func = {extfunc,Mod,Name,Arity},
            Line = call_line(Where, Func, Anno0),
            Call = build_call(call_ext, Arity, Func, Context, Dst),
            Anno = case erl_bifs:is_exit_bif(Mod, Name, Arity) of
                       true ->
                           %% There is no need to kill Y registers
                           %% before calling an exit BIF.
                           maps:remove(kill_yregs, Anno0);
                       false ->
                           Anno0
                   end,
            Is = setup_args(Args, Anno, Context, St) ++ Line ++ Call,
            {Is,St};
        {Mod,Name} ->
            Apply = build_apply(Arity, Context, Dst),
            Is = setup_args(Args++[Mod,Name], Anno0, Context, St) ++
                [line(Anno0)] ++ Apply,
            {Is,St}
    end;
cg_call(#cg_set{anno=Anno,op=call,dst=Dst0,args=[Func | Args0]},
        Where, Context, St0) ->
    Line = call_line(Where, Func, Anno),
    Args = beam_args(Args0 ++ [Func], St0),

    Arity = length(Args0),
    Dst = beam_arg(Dst0, St0),

    %% Note that we only inspect the (possible) type of the fun while building
    %% the call, we don't want the arguments to be typed.
    [TypedFunc] = typed_args([Func], Anno, St0),
    {Call, St} = build_fun_call(Arity, TypedFunc, Context, Dst, St0),

    Is = setup_args(Args, Anno, Context, St) ++ Line ++ Call,
    case Anno of
        #{ result_type := Type } ->
            Info = {var_info, Dst, [{type,Type}]},
            {Is ++ [{'%', Info}], St};
        #{} ->
            {Is, St}
    end.

cg_match_fail([{atom,function_clause}|Args], Line, Fc) ->
    case Fc of
        none ->
            %% There is a stack frame (probably because of inlining).
            %% Jumping to the func_info label is not allowed by
            %% beam_validator. Rewrite the instruction as a call to
            %% erlang:error/2.
            make_fc(Args, Line);
        _ ->
            setup_args(Args) ++ [{jump,{f,Fc}}]
    end;
cg_match_fail([{atom,Op}], Line, _Fc) ->
    [Line,Op];
cg_match_fail([{atom,Op},Val], Line, _Fc) ->
    [Line,{Op,Val}].

make_fc(Args, Line) ->
    %% Recreate the original call to erlang:error/2.
    Live = foldl(fun({x,X}, A) -> max(X+1, A);
                    (_, A) -> A
                 end, 0, Args),
    TmpReg = {x,Live},
    StkMoves = build_stk(reverse(Args), TmpReg, nil),
    [{test_heap,2*length(Args),Live}|StkMoves] ++
        [{move,{atom,function_clause},{x,0}},
         Line,
         {call_ext,2,{extfunc,erlang,error,2}}].

build_stk([V], _TmpReg, Tail) ->
    [{put_list,V,Tail,{x,1}}];
build_stk([V|Vs], TmpReg, Tail) ->
    I = {put_list,V,Tail,TmpReg},
    [I|build_stk(Vs, TmpReg, TmpReg)];
build_stk([], _TmpReg, nil) ->
    [{move,nil,{x,1}}].

build_call(call_ext, 2, {extfunc,erlang,'!',2}, none, Dst) ->
    [send|copy({x,0}, Dst)];
build_call(call_ext, 2, {extfunc,erlang,'!',2}, {return,Dst,N}, Dst)
  when is_integer(N) ->
    [send,{deallocate,N},return];
build_call(Prefix, Arity, Func, {return,Dst,none}, Dst) ->
    I = case Prefix of
            call -> call_only;
            call_ext -> call_ext_only
        end,
    [{I,Arity,Func}];
build_call(call_ext, Arity, {extfunc,Mod,Name,Arity}=Func, {return,_,none}, _Dst) ->
    true = erl_bifs:is_exit_bif(Mod, Name, Arity), %Assertion.
    [{call_ext_only,Arity,Func}];
build_call(Prefix, Arity, Func, {return,Dst,N}, Dst) when is_integer(N) ->
    I = case Prefix of
            call -> call_last;
            call_ext -> call_ext_last
        end,
    [{I,Arity,Func,N}];
build_call(I, Arity, Func, {return,Val,N}, _Dst) when is_integer(N) ->
    [{I,Arity,Func}|copy(Val, {x,0})++[{deallocate,N},return]];
build_call(I, Arity, Func, none, Dst) ->
    [{I,Arity,Func}|copy({x,0}, Dst)].

build_fun_call(Arity, #tr{}=Func0, none, Dst, St0) ->
    %% Func0 was the source register prior to copying arguments, and has been
    %% moved to {x, Arity}. Update it to match.
    Func = Func0#tr{r={x,Arity}},
    {Tag, St} = fun_call_tag(Arity, Func, St0),
    Is = [{call_fun2,Tag,Arity,Func}|copy({x,0}, Dst)],
    {Is, St};
build_fun_call(Arity, #tr{}=Func0, {return,Dst,N}, Dst, St0)
  when is_integer(N) ->
    Func = Func0#tr{r={x,Arity}},
    {Tag, St} = fun_call_tag(Arity, Func, St0),
    Is = [{call_fun2,Tag,Arity,Func},{deallocate,N},return],
    {Is, St};
build_fun_call(Arity, #tr{}=Func0, {return,Val,N}, _Dst, St0)
  when is_integer(N) ->
    Func = Func0#tr{r={x,Arity}},
    {Tag, St} = fun_call_tag(Arity, Func, St0),
    Is = [{call_fun2,Tag,Arity,Func},
          {move,Val,{x,0}},
          {deallocate,N},return],
    {Is, St};
build_fun_call(Arity, _Func, none, Dst, St) ->
    {[{call_fun,Arity}|copy({x,0}, Dst)], St};
build_fun_call(Arity, _Func, {return,Dst,N}, Dst, St) when is_integer(N) ->
    {[{call_fun,Arity},{deallocate,N},return], St};
build_fun_call(Arity, _Func, {return,Val,N}, _Dst, St) when is_integer(N) ->
    {[{call_fun,Arity},{move,Val,{x,0}},{deallocate,N},return], St}.

fun_call_tag(Arity, #tr{t=#t_fun{arity=Arity,target={Name,TotalArity}}}, St0) ->
    {FuncLbl, St} = local_func_label(Name, TotalArity, St0),
    {{f,FuncLbl}, St};
fun_call_tag(Arity, #tr{t=#t_fun{arity=Arity}}, St) ->
    {{atom,safe}, St};
fun_call_tag(_Arity, _Func, St) ->
    {{atom,unsafe}, St}.

build_apply(Arity, {return,Dst,N}, Dst) when is_integer(N) ->
    [{apply_last,Arity,N}];
build_apply(Arity, {return,Val,N}, _Dst) when is_integer(N) ->
    [{apply,Arity}|copy(Val, {x,0})++[{deallocate,N},return]];
build_apply(Arity, none, Dst) ->
    [{apply,Arity}|copy({x,0}, Dst)].

cg_instr(bs_start_match, [{atom,resume}, Src], Dst, Set) ->
    Live = get_live(Set),
    [{bs_start_match4,{atom,resume},Live,Src,Dst}];
cg_instr(bs_start_match, [{atom,new}, Src0], Dst, Set) ->
    {Src, Pre} = force_reg(Src0, Dst),
    Live = get_live(Set),
    Pre ++ [{bs_start_match4,{atom,no_fail},Live,Src,Dst}];
cg_instr(bs_ensured_get, [Kind,Ctx,{literal,Flags},{integer,Size},{integer,Unit}], Dst, Set) ->
    %% Temporary instruction that will be incorporated into a bs_match
    %% instruction by the bs_translate sub pass.
    Live = get_live(Set),
    [{bs_ensured_get,Live,Kind,Ctx,field_flags(Flags, Set),Size,Unit,Dst}];
cg_instr(bs_ensured_get, [{atom,binary},Ctx,{literal,_Flags},
                          {atom,all},{integer,Unit}], Dst, Set) ->
    %% Temporary instruction that will be incorporated into a bs_match
    %% instruction by the bs_translate sub pass.
    Live = get_live(Set),
    [{bs_ensured_get_tail,Live,Ctx,Unit,Dst}];
cg_instr(bs_get_tail, [Src], Dst, Set) ->
    Live = get_live(Set),
    [{bs_get_tail,Src,Dst,Live}];
cg_instr(bs_get_position, [Ctx], Dst, Set) ->
    Live = get_live(Set),
    [{bs_get_position,Ctx,Dst,Live}];
cg_instr(executable_line, [{integer,Index}], _Dst, #cg_set{anno=Anno}) ->
    {line,Location} = line(Anno),
    [{executable_line,Location,Index}];
cg_instr(debug_line, [{integer,Index},{integer,Live},{literal,Info}],
         _Dst, #cg_set{anno=Anno}) ->
    case line(Anno) of
        {line,[]} ->
            [];
        {line,Location} ->
            [{debug_line,Location,Index,Live,Info}]
    end;
cg_instr(put_map, [{atom,assoc},SrcMap|Ss], Dst, Set) ->
    Live = get_live(Set),
    [{put_map_assoc,{f,0},SrcMap,Dst,Live,{list,Ss}}];
cg_instr(put_map, [{atom,exact},SrcBadMap|_Ss], _Dst, #cg_set{anno=Anno}=Set) ->
    %% GH-7283: An exact `put_map` without a failure label was not
    %% handled. The absence of the failure label can only mean that
    %% the source is known not to be a valid map. (None of the current
    %% optimization passes can figure out that the key is always
    %% present in the map and that the operation therefore can never
    %% fail.)
    Live = get_live(Set),
    [{test_heap,3,Live},
     {put_tuple2,{x,0},{list,[{atom,badmap},SrcBadMap]}},
     line(Anno),
     {call_ext_last,1,{extfunc,erlang,error,1},1}];
cg_instr(is_nonempty_list, Ss, Dst, Set) ->
    #cg_set{anno=#{was_bif_is_list := true}} = Set, %Assertion.

    %% This instruction was a call to is_list/1, which was rewritten
    %% to an is_nonempty_list test by beam_ssa_type. BEAM has no
    %% is_nonempty_list instruction that will return a boolean, so
    %% we must revert it to an is_list/1 call.
    [{bif,is_list,{f,0},Ss,Dst}];
cg_instr(Op, Args, Dst, _Set) ->
    cg_instr(Op, Args, Dst).

cg_instr(bs_ensured_skip, [_Type,Ctx,_Flags,{integer,Sz},{integer,U}], {z,_})
  when is_integer(Sz) ->
    %% Temporary instruction that will be incorporated into a bs_match
    %% instruction by the bs_translate sub pass.
    [{bs_ensured_skip,Ctx,Sz*U}];
cg_instr(bs_ensured_skip, [_Type,_Ctx,_Flags,{atom,all},{integer,_U}], {z,_}) ->
    [];
cg_instr(bs_init_writable, Args, Dst) ->
    setup_args(Args) ++ [bs_init_writable|copy({x,0}, Dst)];
cg_instr(bs_set_position, [Ctx,Pos], _Dst) ->
    [{bs_set_position,Ctx,Pos}];
cg_instr(build_stacktrace, Args, Dst) ->
    setup_args(Args) ++ [build_stacktrace|copy({x,0}, Dst)];
cg_instr(set_tuple_element=Op, [New,Tuple,{integer,Index}], _Dst) ->
    [{Op,New,Tuple,Index}];
cg_instr({float,get}, [Src], Dst) ->
    [{fmove,Src,Dst}];
cg_instr({float,put}, [Src], Dst) ->
    [{fmove,Src,Dst}];
cg_instr(get_hd=Op, [Src], Dst) ->
    [{Op,Src,Dst}];
cg_instr(get_tl=Op, [Src], Dst) ->
    [{Op,Src,Dst}];
cg_instr(get_tuple_element=Op, [Src,{integer,N}], Dst) ->
    [{Op,Src,N,Dst}];
cg_instr(nif_start, [], _Dst) ->
    [nif_start];
cg_instr(put_list=Op, [Hd,Tl], Dst) ->
    [{Op,Hd,Tl,Dst}];
cg_instr(nop, [], _Dst) ->
    [];
cg_instr(put_tuple, Elements, Dst) ->
    [{put_tuple2,Dst,{list,Elements}}];
cg_instr(raw_raise, Args, Dst) ->
    setup_args(Args) ++ [raw_raise|copy({x,0}, Dst)];
cg_instr(recv_marker_bind, [Mark, Ref], _Dst) ->
    [{recv_marker_bind, Mark, Ref}];
cg_instr(recv_marker_clear, [Src], _Dst) ->
    [{recv_marker_clear, Src}];
cg_instr(recv_marker_reserve, [], Dst) ->
    [{recv_marker_reserve, Dst}];
cg_instr(remove_message, [], _Dst) ->
    [remove_message];
cg_instr(resume, [A,B], _Dst) ->
    [{bif,raise,{f,0},[A,B],{x,0}}].

cg_test({float,Op0}, Fail, Args, Dst, #cg_set{anno=Anno}) ->
    Op = case Op0 of
             '+' -> fadd;
             '-' when length(Args) =:= 2 -> fsub;
             '-' -> fnegate;
             '*' -> fmul;
             '/' -> fdiv
         end,
    [line(Anno),{bif,Op,Fail,Args,Dst}];
cg_test(peek_message, Fail, [], Dst, _I) ->
    [{loop_rec,Fail,{x,0}}|copy({x,0}, Dst)];
cg_test(put_map, Fail, [{atom,exact},SrcMap|Ss], Dst, #cg_set{anno=Anno}=Set) ->
    Live = get_live(Set),
    [line(Anno),{put_map_exact,Fail,SrcMap,Dst,Live,{list,Ss}}];
cg_test(set_tuple_element=Op, Fail, Args, Dst, Set) ->
    {f,0} = Fail,                               %Assertion.
    cg_instr(Op, Args, Dst, Set);
cg_test(raw_raise, _Fail, Args, Dst, _I) ->
    cg_instr(raw_raise, Args, Dst);
cg_test(resume, _Fail, [_,_]=Args, Dst, _I) ->
    cg_instr(resume, Args, Dst).

cg_update_record_list([{integer, Index}, Value]) ->
    [Index, Value];
cg_update_record_list([_,_|_]=Updates) ->
    cg_update_record_list_1(Updates, #{}).

cg_update_record_list_1([{integer, Index}, Value | Updates], Acc) ->
    cg_update_record_list_1(Updates, Acc#{ Index => Value });
cg_update_record_list_1([], Acc) ->
    append([[Index, Value] || Index := Value <- maps:iterator(Acc, ordered)]).

cg_bs_get(Fail, #cg_set{dst=Dst0,args=Args,anno=Anno}=Set, St) ->
    [{atom,Type}|Ss0] = typed_args(Args, Anno, St),
    Dst = beam_arg(Dst0, St),
    Op = case Type of
             integer -> bs_get_integer2;
             float   -> bs_get_float2;
             binary  -> bs_get_binary2;
             utf8    -> bs_get_utf8;
             utf16   -> bs_get_utf16;
             utf32   -> bs_get_utf32
         end,
    Ss = case Ss0 of
             [Ctx,{literal,Flags},Size,{integer,Unit}] ->
                 %% Plain integer/float/binary.
                 [Ctx,Size,Unit,field_flags(Flags, Set)];
             [Ctx,{literal,Flags}] ->
                 %% Utf8/16/32.
                 [Ctx,field_flags(Flags, Set)]
         end,
    Live = get_live(Set),
    [{test,Op,Fail,Live,Ss,Dst}].

cg_bs_skip(Fail, [{atom,Type}|Ss0], Set) ->
    Op = case Type of
             utf8 -> bs_skip_utf8;
             utf16 -> bs_skip_utf16;
             utf32 -> bs_skip_utf32;
             _ -> bs_skip_bits2
         end,
    Live = get_live(Set),
    Ss = case Ss0 of
             [Ctx,{literal,Flags},Size,{integer,Unit}] ->
                 %% Plain integer/float/binary.
                 [Ctx,Size,Unit,field_flags(Flags, Set)];
             [Ctx,{literal,Flags}] ->
                 %% Utf8/16/32.
                 [Ctx,Live,field_flags(Flags, Set)]
         end,
    case {Type,Ss} of
        {binary,[_,{atom,all},1,_]} ->
            [];
        {binary,[R,{atom,all},U,_]} ->
            [{test,bs_test_unit,Fail,[R,U]}];
        {_,_} ->
            [{test,Op,Fail,Ss}]
    end.

field_flags(Flags, #cg_set{anno=#{location:={File,Line}}}) ->
    {field_flags,[{anno,[Line,{file,File}]}|Flags]};
field_flags(Flags, _) ->
    {field_flags,Flags}.

cg_catch(Agg, T0, Context, St0) ->
    {Moves,T1} = cg_extract(T0, Agg, St0),
    {T,St} = cg_block(T1, Context, St0),
    {Moves++T,St}.

cg_try(Agg, Tag, T0, Context, St0) ->
    {Moves0,T1} = cg_extract(T0, Agg, St0),
    Moves = order_moves(Moves0),
    [#cg_set{op=kill_try_tag}|T2] = T1,
    {T,St} = cg_block(T2, Context, St0),
    {[{try_case,Tag}|Moves++T],St}.

cg_extract([#cg_set{op=extract,dst=Dst0,args=Args0}|Is0], Agg, St) ->
    [Dst,Agg,{integer,X}] = beam_args([Dst0|Args0], St),
    {Ds,Is} = cg_extract(Is0, Agg, St),
    case keymember(Dst, 3, Ds) of
        true ->
            %% This destination will be overwritten.
            {Ds,Is};
        false ->
            {copy({x,X}, Dst)++Ds,Is}
    end;
cg_extract(Is, _, _) ->
    {[],Is}.

-spec copy(Src, Dst) -> [{move,Src,Dst}] when
      Src :: beam_reg() | beam_literal(),
      Dst :: beam_reg().
copy(Src, Src) -> [];
copy(Src, Dst) -> [{move,Src,Dst}].

force_reg({literal,_}=Lit, Reg) ->
    {Reg,[{move,Lit,Reg}]};
force_reg({integer,_}=Lit, Reg) ->
    {Reg,[{move,Lit,Reg}]};
force_reg({atom,_}=Lit, Reg) ->
    {Reg,[{move,Lit,Reg}]};
force_reg({float,_}=Lit, Reg) ->
    {Reg,[{move,Lit,Reg}]};
force_reg(nil=Lit, Reg) ->
    {Reg,[{move,Lit,Reg}]};
force_reg({Kind,_}=R, _) when Kind =:= x; Kind =:= y ->
    {R,[]}.

%% successors(Terminator) -> [Successor].
%%  Return an ordset of all successors for the given terminator.

successors(#cg_br{succ=Succ,fail=Fail}) ->
    ordsets:from_list([Succ,Fail]);
successors(#cg_switch{fail=Fail,list=List}) ->
    ordsets:from_list([Fail|[Lbl || {_,Lbl} <:- List]]);
successors(#cg_ret{}) -> [].

%% linearize(Blocks) -> [{BlockLabel,#cg_blk{}}].
%%  Linearize the intermediate representation of the code. Also
%%  translate blocks from the SSA records to internal record types
%%  used only in this module.

linearize(Blocks) ->
    Linear = beam_ssa:linearize(Blocks),
    linearize_1(Linear, Blocks).

linearize_1([{?EXCEPTION_BLOCK,_}|Ls], Blocks) ->
    linearize_1(Ls, Blocks);
linearize_1([{L,Block0}|Ls], Blocks) ->
    Block = translate_block(L, Block0, Blocks),
    [{L,Block}|linearize_1(Ls, Blocks)];
linearize_1([], _Blocks) -> [].

%% translate_block(BlockLabel, #b_blk{}, Blocks) -> #cg_blk{}.
%%  Translate a block to the internal records used in this module.
%%  Also eliminate phi nodes, replacing them with 'copy' instructions
%%  in the predecessor blocks.

translate_block(L, #b_blk{anno=Anno,is=Is0,last=Last0}, Blocks) ->
    Last = translate_terminator(Last0),
    PhiCopies = translate_phis(L, Last, Blocks),
    Is1 = translate_is(Is0, PhiCopies),
    Is = case Anno of
             #{frame_size:=Size} ->
                 Alloc = #cg_alloc{stack=Size},
                 [Alloc|Is1];
             #{} -> Is1
         end,
    #cg_blk{anno=Anno,is=Is,last=Last}.

translate_is([#b_set{op=phi}|Is], Tail) ->
    translate_is(Is, Tail);
translate_is([#b_set{anno=Anno0,op=Op,dst=Dst,args=Args}=I|Is], Tail) ->
    Anno1 = case beam_ssa:clobbers_xregs(I) of
                true -> Anno0#{clobbers=>true};
                false -> Anno0
           end,
    Anno = prune_arg_types(Anno1, Args),
    [#cg_set{anno=Anno,op=Op,dst=Dst,args=Args}|translate_is(Is, Tail)];
translate_is([], Tail) -> Tail.

prune_arg_types(#{arg_types := ArgTypes0}=Anno, Args) ->
    ArgTypes = prune_arg_types_1(Args, 0, ArgTypes0),
    if
        ArgTypes =:= #{} ->
            maps:remove(arg_types, Anno);
        true ->
            Anno#{arg_types := ArgTypes}
    end;
prune_arg_types(Anno, _Args) -> Anno.

prune_arg_types_1([#b_var{}|As], N, ArgTypes) ->
    prune_arg_types_1(As, N + 1, ArgTypes);
prune_arg_types_1([_|As], N, ArgTypes) ->
    prune_arg_types_1(As, N + 1, maps:remove(N, ArgTypes));
prune_arg_types_1([], _N, ArgTypes) -> ArgTypes.

translate_terminator(#b_ret{anno=Anno,arg=Arg}) ->
    Dealloc = case Anno of
                  #{deallocate:=N} -> N;
                  #{} -> none
              end,
    #cg_ret{arg=Arg,dealloc=Dealloc};
translate_terminator(#b_br{bool=#b_literal{val=true},succ=Succ}) ->
    #cg_br{bool=#b_literal{val=true},succ=Succ,fail=Succ};
translate_terminator(#b_br{bool=Bool,succ=Succ,fail=Fail}) ->
    #cg_br{bool=Bool,succ=Succ,fail=Fail};
translate_terminator(#b_switch{anno=Anno,arg=Bool,fail=Fail,list=List}) ->
    #cg_switch{anno=Anno,arg=Bool,fail=Fail,list=List}.

translate_phis(L, #cg_br{succ=Target,fail=Target}, Blocks) ->
    #b_blk{is=Is} = maps:get(Target, Blocks),
    Phis = takewhile(fun(#b_set{op=phi}) -> true;
                        (#b_set{}) -> false
                     end, Is),
    case Phis of
        [] ->
            [];
        [#b_set{op=phi,dst=NopDst}|_]=Phis ->
            %% In rare cases (so far only seen in unoptimized code),
            %% copy instructions can be combined like this:
            %%
            %%     y0/yreg_0 = copy x0/xreg_0
            %%     x0/xreg_1 = copy y0/yreg_0
            %%
            %% This will result in a swap instruction instead of
            %% two move instructions. To avoid that, insert a
            %% dummy instruction before the copy instructions
            %% resulting from the phi node:
            %%
            %%     y0/yreg_0 = copy x0/xreg_0
            %%     _ = nop
            %%     x0/xreg_1 = copy y0/yreg_0
            %%
            Nop = #cg_set{op=nop,dst=NopDst,args=[]},
            [Nop|phi_copies(Phis, L)]
    end;
translate_phis(_, _, _) -> [].

phi_copies([#b_set{anno=Anno0,dst=Dst,args=PhiArgs}|Sets], L) ->
    CopyArgs = [V || {V,Target} <- PhiArgs, Target =:= L],
    Anno = Anno0#{was_phi => true},
    [#cg_set{anno=Anno,op=copy,dst=Dst,args=CopyArgs}|phi_copies(Sets, L)];
phi_copies([], _) -> [].

%% opt_move_to_x0([Instruction]) -> [Instruction].
%%  Simple peep-hole optimization to move a {move,Any,{x,0}} past
%%  any kill up to the next call instruction. (To give the loader
%%  an opportunity to combine the 'move' and the 'call' instructions.)

opt_move_to_x0(Moves) ->
    opt_move_to_x0(Moves, []).

opt_move_to_x0([{move,_,{x,0}}=I|Is0], Acc0) ->
    case move_past_kill(Is0, I, Acc0) of
       impossible -> opt_move_to_x0(Is0, [I|Acc0]);
       {Is,Acc} -> opt_move_to_x0(Is, Acc)
    end;
opt_move_to_x0([I|Is], Acc) ->
    opt_move_to_x0(Is, [I|Acc]);
opt_move_to_x0([], Acc) -> reverse(Acc).

move_past_kill([{init_yregs,{list,Yregs}}=I|Is], {move,Src,_}=Move, Acc) ->
    case member(Src, Yregs) of
        true -> impossible;
        false -> move_past_kill(Is, Move, [I|Acc])
    end;
move_past_kill(Is, Move, Acc) ->
    {Is,[Move|Acc]}.

%% setup_args(Args, Anno, Context) -> [Instruction].
%% setup_args(Args) -> [Instruction].
%%  Set up X registers for a call.

setup_args(Args, Anno, none, St) ->
    case {setup_args(Args),kill_yregs(Anno, St)} of
        {Moves,[]} ->
            Moves;
        {Moves,Kills} ->
            opt_move_to_x0(Moves ++ Kills)
    end;
setup_args(Args, _, _, _) ->
    setup_args(Args).

setup_args([]) ->
    [];
setup_args([_|_]=Args) ->
    Moves = gen_moves(Args, 0, []),
    order_moves(Moves).

%% kill_yregs(Anno, #cg{}) -> [{init_yregs,{list,[{y,Y}]}}].
%%  Kill Y registers that will not be used again.

kill_yregs(#{kill_yregs:=Kill}, #cg{regs=Regs}) ->
    case ordsets:from_list([map_get(V, Regs) || V <- Kill]) of
        [] -> [];
        [_|_]=List -> [{init_yregs,{list,List}}]
    end;
kill_yregs(#{}, #cg{}) -> [].

%% gen_moves(As, I, Acc)
%%  Generate the basic move instruction to move the arguments
%%  to their proper registers. The list will be sorted on
%%  destinations. (I.e. the move to {x,0} will be first --
%%  see the comment to order_moves/2.)

gen_moves([A|As], I, Acc) ->
    gen_moves(As, I+1, copy(A, {x,I}) ++ Acc);
gen_moves([], _, Acc) ->
    keysort(3, Acc).

%% order_moves([Move]) -> [Move]
%%  Orders move instruction so that source registers are not
%%  destroyed before they are used. If there are cycles
%%  (such as {move,{x,0},{x,1}}, {move,{x,1},{x,1}}),
%%  swap instructions will be used to break up the cycle.
%%
%%  If possible, the first move of the input list is placed
%%  last in the result list (to make the move to {x,0} occur
%%  just before the call to allow the Beam loader to coalesce
%%  the instructions).

order_moves(Ms) -> order_moves(Ms, []).

order_moves([{move,_,_}=M|Ms0], Acc0) ->
    {Chain,Ms} = collect_chain(Ms0, [M]),
    Acc = reverse(Chain, Acc0),
    order_moves(Ms, Acc);
order_moves([], Acc) -> Acc.

collect_chain(Ms, Path) ->
    collect_chain(Ms, Path, []).

collect_chain([{move,Src,Same}=M|Ms0], [{move,Same,_}|_]=Path, Others) ->
    case keymember(Src, 3, Path) of
        false ->
            collect_chain(reverse(Others, Ms0), [M|Path], []);
        true ->
            %% There is a cycle.
            {break_up_cycle(M, Path),reverse(Others, Ms0)}
    end;
collect_chain([M|Ms], Path, Others) ->
    collect_chain(Ms, Path, [M|Others]);
collect_chain([], Path, Others) ->
    {Path,Others}.

break_up_cycle({move,Src,_Dst}=M, Path) ->
    break_up_cycle_1(Src, [M|Path], []).

break_up_cycle_1(Dst, [{move,_Src,Dst}|Path], Acc) ->
    reverse(Acc, Path);
break_up_cycle_1(Dst, [{move,S,D}|Path], Acc) ->
    break_up_cycle_1(Dst, Path, [{swap,S,D}|Acc]).

%%%
%%% Collect and translate binary match instructions, producing a
%%% bs_match instruction.
%%%

bs_translate([{bs_get_tail,_,_,_}=I|Is]) ->
    %% A lone bs_get_tail. There is no advantage to incorporating it into
    %% a bs_match instruction.
    [I|bs_translate(Is)];
bs_translate([I|Is0]) ->
    case bs_translate_instr(I) of
        none ->
            [I|bs_translate(Is0)];
        {Ctx,Fail0,First} ->
            {Instrs0,Fail,Is} = bs_translate_collect(Is0, Ctx, Fail0, [First]),
            Instrs1 = bs_seq_match_fixup(Instrs0),
            Instrs = bs_eq_fixup(Instrs1),
            [{bs_match,Fail,Ctx,{commands,Instrs}}|bs_translate(Is)]
    end;
bs_translate([]) -> [].

bs_translate_collect([I|Is]=Is0, Ctx, Fail, Acc) ->
    case bs_translate_instr(I) of
        {Ctx,_,{ensure_at_least,_,_}} ->
            %% There should only be a single `ensure_at_least`
            %% instruction in each `bs_match` instruction.
            {bs_translate_fixup(Acc),Fail,Is0};
        {Ctx,Fail,Instr} ->
            bs_translate_collect(Is, Ctx, Fail, [Instr|Acc]);
        {Ctx,{f,0},Instr} ->
            bs_translate_collect(Is, Ctx, Fail, [Instr|Acc]);
        {_,_,_} ->
            {bs_translate_fixup(Acc),Fail,Is0};
        none ->
            {bs_translate_fixup(Acc),Fail,Is0}
    end.

bs_translate_fixup([{get_tail,_,_,_}=GT,{test_tail,Bits}|Is0]) ->
    Is = reverse(Is0),
    bs_translate_fixup_tail(Is, Bits) ++ [GT];
bs_translate_fixup([{test_tail,Bits}|Is0]) ->
    Is = reverse(Is0),
    bs_translate_fixup_tail(Is, Bits);
bs_translate_fixup(Is) ->
    reverse(Is).

%% Fix up matching of multiple binaries in parallel. Example:
%%    f(<<_:8>> = <<X:8>>) -> ...
bs_seq_match_fixup([{test_tail,Bits},{ensure_exactly,Bits}|Is]) ->
    [{ensure_exactly,Bits}|bs_seq_match_fixup(Is)];
bs_seq_match_fixup([{test_tail,Bits0},{ensure_at_least,Bits1,Unit}|Is])
  when Bits0 >= Bits1, Bits0 rem Unit =:= 0 ->
    %% The tail test is at least as strict as the ensure_at_least test.
    [{ensure_exactly,Bits0}|bs_seq_match_fixup(Is)];
bs_seq_match_fixup([{test_tail,Bits}|Is]) ->
    [{ensure_exactly,Bits}|bs_seq_match_fixup(Is)];
bs_seq_match_fixup([I|Is]) ->
    [I|bs_seq_match_fixup(Is)];
bs_seq_match_fixup([]) -> [].

bs_eq_fixup([{'=:=',nil,Bits,Value}|Is]) ->
    EqInstrs = bs_eq_fixup_split(Bits, <<Value:Bits>>),
    EqInstrs ++ bs_eq_fixup(Is);
bs_eq_fixup([I|Is]) ->
    [I|bs_eq_fixup(Is)];
bs_eq_fixup([]) -> [].

%% In the 32-bit runtime system, each integer to be matched must
%% fit in a SIGNED 32-bit word. Therefore, we will split the
%% instruction into multiple instructions each matching at most
%% 31 bits.
bs_eq_fixup_split(Bits, Value) when Bits =< 31 ->
    <<I:Bits>> = Value,
    [{'=:=',nil,Bits,I}];
bs_eq_fixup_split(Bits, Value0) ->
    <<I:31,Value/bits>> = Value0,
    [{'=:=',nil,31,I} | bs_eq_fixup_split(Bits - 31, Value)].

bs_translate_fixup_tail([{ensure_at_least,Bits0,_}|Is], Bits) ->
    [{ensure_exactly,Bits0+Bits}|Is];
bs_translate_fixup_tail([I|Is], Bits) ->
    [I|bs_translate_fixup_tail(Is, Bits)];
bs_translate_fixup_tail([], Bits) ->
    [{ensure_exactly,Bits}].

bs_translate_instr({test,bs_ensure,Fail,[Ctx,Size,Unit]}) ->
    {Ctx,Fail,{ensure_at_least,Size,Unit}};
bs_translate_instr({bs_ensured_get,Live,{atom,Type},Ctx,{field_flags,Flags0},
                    Size,Unit,Dst}) ->
    %% Only keep flags that have a meaning for binary matching and are
    %% distinct from the default value.
    Flags = [Flag || Flag <- Flags0,
                     case Flag of
                         little -> true;
                         native -> true;
                         big -> false;
                         signed -> true;
                         unsigned -> false;
                         {anno,_} -> false
                     end],
    {Ctx,{f,0},{Type,Live,{literal,Flags},Size,Unit,Dst}};
bs_translate_instr({test,bs_ensured_match_string,Fail,
                    [Ctx,Bits,{string,String}]}) ->
    <<Value:Bits,_/bitstring>> = String,
    Live = nil,
    {Ctx,Fail,{'=:=',Live,Bits,Value}};
bs_translate_instr({bs_ensured_skip,Ctx,Stride}) ->
    {Ctx,{f,0},{skip,Stride}};
bs_translate_instr({bs_ensured_get_tail,Live,Ctx,Unit,Dst}) ->
    {Ctx,{f,0},{get_tail,Live,Unit,Dst}};
bs_translate_instr({bs_get_tail,Ctx,Dst,Live}) ->
    {Ctx,{f,0},{get_tail,Live,1,Dst}};
bs_translate_instr({test,bs_test_tail2,Fail,[Ctx,Bits]}) ->
    {Ctx,Fail,{test_tail,Bits}};
bs_translate_instr(_) -> none.


%%%
%%% General utility functions.
%%%

verify_beam_register({x,_}=Reg) -> Reg.

is_beam_register({x,_}) -> true;
is_beam_register(_) -> false.

get_register(V, Regs) ->
    case is_beam_register(V) of
        true -> V;
        false -> maps:get(V, Regs)
    end.

typed_args(As, Anno, St) ->
    typed_args_1(As, Anno, St, 0).

typed_args_1([Arg | Args], Anno, St, Index) ->
   case Anno of
       #{ arg_types := #{ Index := Type } } ->
           Typed = #tr{r=beam_arg(Arg, St),t=Type},
           [Typed | typed_args_1(Args, Anno, St, Index + 1)];
       #{} ->
           [beam_arg(Arg, St) | typed_args_1(Args, Anno, St, Index + 1)]
   end;
typed_args_1([], _Anno, _St, _Index) ->
    [].

beam_args(As, St) ->
    [beam_arg(A, St) || A <- As].

beam_arg(#b_var{}=Name, #cg{regs=Regs}) ->
    maps:get(Name, Regs);
beam_arg(#b_literal{val=Val}, _) ->
    if
        is_atom(Val) -> {atom,Val};
        is_float(Val) -> {float,Val};
        is_integer(Val) -> {integer,Val};
        Val =:= [] -> nil;
        true -> {literal,Val}
    end;
beam_arg(Reg, _) ->
    verify_beam_register(Reg).

new_block_label(L, St0) ->
    {_Lbl,St} = label_for_block(L, St0),
    St.

def_block_label(L, #cg{labels=Labels,used_labels=Used}) ->
    Lbl = maps:get(L, Labels),
    case gb_sets:is_member(Lbl, Used) of
        false -> [];
        true -> [{label,Lbl}]
    end.

use_block_labels(Ls, St) ->
    mapfoldl(fun use_block_label/2, St, Ls).

use_block_label(L, #cg{used_labels=Used,catches=Catches}=St0) ->
    {Lbl,St} = label_for_block(L, St0),
    case gb_sets:is_member(L, Catches) of
        true ->
            {{catch_tag,{f,Lbl}},
             St#cg{used_labels=gb_sets:add(Lbl, Used)}};
        false ->
            {{f,Lbl},St#cg{used_labels=gb_sets:add(Lbl, Used)}}
    end.

label_for_block(L, #cg{labels=Labels0}=St0) ->
    case Labels0 of
        #{L:=Lbl} ->
            {Lbl,St0};
        #{} ->
            {Lbl,St} = new_label(St0),
            Labels = Labels0#{L=>Lbl},
            {Lbl,St#cg{labels=Labels}}
    end.

%% local_func_label(Name, Arity, State) -> {Label,State'}
%% local_func_label({Name,Arity}, State) -> {Label,State'}
%%  Get the function entry label for a local function.

local_func_label(Name, Arity, St) ->
    local_func_label({Name,Arity}, St).

local_func_label(Key, #cg{functable=Map}=St0) ->
    case Map of
       #{Key := Label} ->
            {Label,St0};
        _ ->
            {Label,St} = new_label(St0),
            {Label,St#cg{functable=Map#{Key => Label}}}
    end.

%% is_gc_bif(Name, Args) -> true|false.
%%  Determines whether the BIF Name/Arity might do a GC.

is_gc_bif(hd, [_]) -> false;
is_gc_bif(tl, [_]) -> false;
is_gc_bif(self, []) -> false;
is_gc_bif(max, [_,_]) -> false;
is_gc_bif(min, [_,_]) -> false;
is_gc_bif(node, []) -> false;
is_gc_bif(node, [_]) -> false;
is_gc_bif(element, [_,_]) -> false;
is_gc_bif(get, [_]) -> false;
is_gc_bif(is_map_key, [_,_]) -> false;
is_gc_bif(map_get, [_,_]) -> false;
is_gc_bif(tuple_size, [_]) -> false;
is_gc_bif(Bif, Args) ->
    Arity = length(Args),
    not (erl_internal:bool_op(Bif, Arity) orelse
	 erl_internal:new_type_test(Bif, Arity) orelse
	 erl_internal:comp_op(Bif, Arity)).

%% new_label(St) -> {L,St}.

new_label(#cg{lcount=Next}=St) ->
    {Next,St#cg{lcount=Next+1}}.

%% call_line(tail|body, Func, Anno) -> [] | [{line,...}].
%%  Produce a line instruction if it will be needed by the
%%  call to Func.

call_line(_Context, {extfunc,Mod,Name,Arity}, Anno) ->
    case erl_bifs:is_safe(Mod, Name, Arity) of
	false ->
	    %% The call could be to a BIF.
	    %% We'll need a line instruction in case the
	    %% BIF call fails.
	    [line(Anno)];
	true ->
	    %% Call to a safe BIF. Since it cannot fail,
	    %% we don't need any line instruction here.
	    []
    end;
call_line(body, _, Anno) ->
    [line(Anno)];
call_line(tail, local, _) ->
    %% Tail-recursive call to a local function. A line
    %% instruction will not be useful.
    [];
call_line(tail, _, Anno) ->
    %% Call to a fun.
    [line(Anno)].

%% line(Le) -> {line,[] | {location,File,Line}}
%%  Create a line instruction, containing information about
%%  the current filename and line number. A line information
%%  instruction should be placed before any operation that could
%%  cause an exception.

line(#{location:={File,Line}}) ->
    {line,[{location,File,Line}]};
line(#{}) ->
    {line,[]}.

flatmapfoldl(F, Accu0, [Hd|Tail]) ->
    {R,Accu1} = F(Hd, Accu0),
    {Rs,Accu2} = flatmapfoldl(F, Accu1, Tail),
    {R++Rs,Accu2};
flatmapfoldl(_, Accu, []) -> {[],Accu}.
