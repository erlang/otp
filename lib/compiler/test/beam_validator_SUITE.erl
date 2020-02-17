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
%%
-module(beam_validator_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 compiler_bug/1,stupid_but_valid/1,
	 xrange/1,yrange/1,stack/1,call_last/1,merge_undefined/1,
	 uninit/1,unsafe_catch/1,
	 dead_code/1,
	 overwrite_catchtag/1,overwrite_trytag/1,accessing_tags/1,bad_catch_try/1,
	 cons_guard/1,
	 freg_range/1,freg_uninit/1,freg_state/1,
	 bad_bin_match/1,bad_dsetel/1,
	 state_after_fault_in_catch/1,no_exception_in_catch/1,
	 undef_label/1,illegal_instruction/1,failing_gc_guard_bif/1,
	 map_field_lists/1,cover_bin_opt/1,
	 val_dsetel/1,bad_tuples/1,bad_try_catch_nesting/1,
         receive_stacked/1,aliased_types/1,type_conflict/1,
         infer_on_eq/1,infer_dead_value/1,
         receive_marker/1,safe_instructions/1,
         missing_return_type/1]).

-include_lib("common_test/include/ct.hrl").

init_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Config.

end_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,10}}].

all() -> 
    [{group,p}].

groups() -> 
    [{p,test_lib:parallel(),
      [compiler_bug,stupid_but_valid,xrange,
       yrange,stack,call_last,merge_undefined,uninit,
       unsafe_catch,dead_code,
       overwrite_catchtag,overwrite_trytag,accessing_tags,
       bad_catch_try,cons_guard,freg_range,freg_uninit,
       freg_state,bad_bin_match,bad_dsetel,
       state_after_fault_in_catch,no_exception_in_catch,
       undef_label,illegal_instruction,failing_gc_guard_bif,
       map_field_lists,cover_bin_opt,val_dsetel,
       bad_tuples,bad_try_catch_nesting,
       receive_stacked,aliased_types,type_conflict,
       infer_on_eq,infer_dead_value,receive_marker,
       safe_instructions,missing_return_type]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.

compiler_bug(Config) when is_list(Config) ->
    %% Check that the compiler returns an error if we try to
    %% assemble one of the bad '.S' files.
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "compiler_bug"),
    error = compile:file(File, [from_asm,report_errors,time]),

    %% Make sure that the error was reported by
    %% the beam_validator module.
    {error,
     [{"compiler_bug",
       [{beam_validator,_}]}],
     []} = compile:file(File, [from_asm,return_errors,time]),
    ok.

%% The following code is stupid but it should compile.
stupid_but_valid(Config) when is_list(Config) ->
    AnAtom = nisse,
    try setelement(5, setelement(6, AnAtom, value), another_value) of
	Term -> ct:fail({what_happened,Term})
    catch
	error:badarg -> ok
    end,
    ok.

xrange(Config) when is_list(Config) ->
    Errors = do_val(xrange, Config),
    [{{t,sum_1,2},
      {{bif,'+',{f,0},[{x,-1},{x,1}],{x,0}},4,
       {bad_register,{x,-1}}}},
     {{t,sum_2,2},
      {{bif,'+',{f,0},[{x,0},{x,1023}],{x,0}},4,limit}},
     {{t,sum_3,2},
      {{bif,'+',{f,0},[{x,0},{x,1}],{x,-1}},4,
       {bad_register,{x,-1}}}},
     {{t,sum_4,2},
      {{bif,'+',{f,0},[{x,0},{x,1}],{x,1023}},4,limit}}] = Errors,
    ok.

yrange(Config) when is_list(Config) ->
    Errors = do_val(yrange, Config),
    [{{t,sum_1,2},
      {{move,{x,1},{y,-1}},5,
       {bad_register,{y,-1}}}},
     {{t,sum_2,2},
      {{bif,'+',{f,0},[{x,0},{y,1024}],{x,0}},7,
       limit}},
     {{t,sum_3,2},
      {{move,{x,1},{y,1024}},5,limit}},
     {{t,sum_4,2},
      {{move,{x,1},{y,-1}},5,
       {bad_register,{y,-1}}}}] = Errors,
    ok.

stack(Config) when is_list(Config) ->
    Errors = do_val(stack, Config),
    [{{t,a,2},{return,9,{stack_frame,2}}},
     {{t,b,2},{{deallocate,2},4,{allocated,none}}},
     {{t,bad_1,0},{{allocate_zero,2,10},4,{{x,9},not_live}}},
     {{t,bad_2,0},{{move,{y,0},{x,0}},5,{unassigned,{y,0}}}},
     {{t,c,2},{{deallocate,2},10,{allocated,none}}},
     {{t,d,2},
      {{allocate,2,2},5,{existing_stack_frame,{size,2}}}},
     {{t,e,2},{{deallocate,5},6,{allocated,2}}}] = Errors,
    ok.

call_last(Config) when is_list(Config) ->
    Errors = do_val(call_last, Config),
    [{{t,a,1},{{call_last,1,{f,8},2},9,{allocated,1}}},
     {{t,b,1},
      {{call_ext_last,2,{extfunc,lists,seq,2},2},
       10,
       {allocated,1}}}] = Errors,
    ok.

merge_undefined(Config) when is_list(Config) ->
    Errors = do_val(merge_undefined, Config),
    [{{t,handle_call,2},
      {{call_ext,2,{extfunc,debug,filter,2}},
       22,
       {uninitialized_reg,{y,_}}}}] = Errors,
    ok.

uninit(Config) when is_list(Config) ->
    Errors = do_val(uninit, Config),
    [{{t,sum_1,2},
      {{move,{y,0},{x,0}},5,{uninitialized_reg,{y,0}}}},
     {{t,sum_2,2},
      {{call,1,{f,8}},5,{uninitialized_reg,{y,0}}}},
     {{t,sum_3,2},
      {{bif,'+',{f,0},[{x,0},{y,0}],{x,0}},
       6,
       {unassigned,{y,0}}}}] = Errors,
    ok.

unsafe_catch(Config) when is_list(Config) ->
    Errors = do_val(unsafe_catch, Config),
    [{{t,small,2},
      {{bs_put_integer,{f,0},{integer,16},1,
        {field_flags,[unsigned,big]},{y,0}},
       20,
       {unassigned,{y,0}}}}] = Errors,
    ok.

dead_code(Config) when is_list(Config) ->
    [] = do_val(dead_code, Config),
    ok.

overwrite_catchtag(Config) when is_list(Config) ->
    Errors = do_val(overwrite_catchtag, Config),
    [{{overwrite_catchtag,foo,1},
      {{move,{x,0},{y,0}},6,{catchtag,_}}}] = Errors,
    ok.

overwrite_trytag(Config) when is_list(Config) ->
    Errors = do_val(overwrite_trytag, Config),
    [{{overwrite_trytag,foo,1},
      {{kill,{y,2}},8,{trytag,_}}}] = Errors,
    ok.

accessing_tags(Config) when is_list(Config) ->
    Errors = do_val(accessing_tags, Config),
    [{{accessing_tags,bar,1},
      {{move,{y,0},{x,0}},6,{trytag,_}}},
     {{accessing_tags,foo,1},
      {{move,{y,0},{x,0}},6,{catchtag,_}}}] = Errors,
    ok.

bad_catch_try(Config) when is_list(Config) ->
    Errors = do_val(bad_catch_try, Config),
    [{{bad_catch_try,bad_1,1},
      {{'catch',{x,0},{f,3}},
       5,{invalid_tag_register,{x,0}}}},
     {{bad_catch_try,bad_2,1},
      {{catch_end,{x,9}},
       8,{invalid_tag_register,{x,9}}}},
     {{bad_catch_try,bad_3,1},
      {{catch_end,{y,1}},9,{invalid_tag,{y,1},{atom,kalle}}}},
     {{bad_catch_try,bad_4,1},
      {{'try',{x,0},{f,15}},5,{invalid_tag_register,{x,0}}}},
     {{bad_catch_try,bad_5,1},
      {{try_case,{y,1}},12,{invalid_tag,{y,1},term}}},
     {{bad_catch_try,bad_6,1},
      {{move,{integer,1},{y,1}},7,
       {invalid_store,{y,1}}}}] = Errors,
    ok.

cons_guard(Config) when is_list(Config) ->
    Errors = do_val(cons, Config),
    [{{cons,foo,1},
      {{get_list,{x,0},{x,1},{x,2}},
       5,
       {bad_type,{needed,cons},{actual,term}}}}] = Errors,
    ok.

freg_range(Config) when is_list(Config) ->
    Errors = do_val(freg_range, Config),
    [{{t,sum_1,2},
      {{bif,fadd,{f,0},[{fr,-1},{fr,1}],{fr,0}},
       5,
       {bad_source,{fr,-1}}}},
     {{t,sum_2,2},
      {{bif,fadd,{f,0},[{fr,0},{fr,1024}],{fr,0}},
       6,
       {uninitialized_reg,{fr,1024}}}},
     {{t,sum_3,2},
      {{bif,fadd,{f,0},[{fr,0},{fr,1}],{fr,-1}},
       7,
       {bad_register,{fr,-1}}}},
     {{t,sum_4,2},
      {{bif,fadd,{f,0},[{fr,0},{fr,1}],{fr,1024}},
       7,
       limit}}] = Errors,
    ok.

freg_uninit(Config) when is_list(Config) ->
    Errors = do_val(freg_uninit, Config),
    [{{t,sum_1,2},
      {{bif,fadd,{f,0},[{fr,0},{fr,1}],{fr,0}},
       6,
       {uninitialized_reg,{fr,1}}}},
     {{t,sum_2,2},
      {{bif,fadd,{f,0},[{fr,0},{fr,1}],{fr,0}},
       9,
       {uninitialized_reg,{fr,0}}}}] = Errors,
    ok.

freg_state(Config) when is_list(Config) ->
    Errors = do_val(freg_state, Config),
    [{{t,sum_1,2},
      {{bif,fmul,{f,0},[{fr,0},{fr,1}],{fr,0}},
       6,
       {bad_floating_point_state,undefined}}},
     {{t,sum_2,2},
      {{fmove,{fr,0},{x,0}},
       8,
       {bad_floating_point_state,cleared}}},
     {{t,sum_3,2},
      {{bif,'-',{f,0},[{x,1},{x,0}],{x,1}},
       8,
       {unsafe_instruction,{float_error_state,cleared}}}},
     {{t,sum_4,2},
      {{fcheckerror,{f,0}},
       4,
       {bad_floating_point_state,undefined}}},
     {{t,sum_5,2},
      {fclearerror,5,{bad_floating_point_state,cleared}}}] = Errors,
    ok.

bad_bin_match(Config) when is_list(Config) ->
    [{{t,t,1},{return,5,{match_context,{x,0}}}}] =
	do_val(bad_bin_match, Config),
    ok.

bad_dsetel(Config) when is_list(Config) ->
    Errors = do_val(bad_dsetel, Config),
    [{{t,t,1},
      {{set_tuple_element,{x,1},{x,0},1},
       17,
       illegal_context_for_set_tuple_element}}] = Errors,
    ok.

state_after_fault_in_catch(Config) when is_list(Config) ->
    Errors = do_val(state_after_fault_in_catch, Config),
    [{{state_after_fault_in_catch,badmatch,1},
      {{move,{x,1},{x,0}},9,{uninitialized_reg,{x,1}}}},
     {{state_after_fault_in_catch,case_end,1},
      {{move,{x,1},{x,0}},9,{uninitialized_reg,{x,1}}}},
     {{state_after_fault_in_catch,if_end,1},
      {{move,{x,1},{x,0}},9,{uninitialized_reg,{x,1}}}},
     {{t,foo,1},
      {{move,{x,1},{x,0}},10,{uninitialized_reg,{x,1}}}}] = Errors,
    ok.

no_exception_in_catch(Config) when is_list(Config) ->
    Errors = do_val(no_exception_in_catch, Config),
    [{{no_exception_in_catch,nested_of_1,4},
      {{move,{x,3},{x,0}},87,{uninitialized_reg,{x,3}}}}] = Errors,
    ok.

undef_label(Config) when is_list(Config) ->
    M = {undef_label,
	 [{t,1}],
	 [],
	 [{function,t,1,2,
	   [{label,1},
	    {func_info,{atom,undef_label},{atom,t},1},
	    {label,2},
	    {test,is_eq_exact,{f,42},[{x,0},{atom,x}]},
	    {move,{atom,ok},{x,0}},
	    return]},
	  {function,x,1,17,
	   [{label,3},
	    {func_info,{atom,undef_label},{atom,x},1},
	    {label,4},
	    return]}],
	 5},
    Errors = beam_val(M),
    [{{undef_label,t,1},{undef_labels,[42]}},
     {{undef_label,x,1},{return,4,no_entry_label}}] = Errors,
    ok.

illegal_instruction(Config) when is_list(Config) ->
    M = {illegal_instruction,
	 [{t,1},{x,1},{y,0}],
	 [],
	 [{function,t,1,2,
	   [{label,1},
	    {func_info,{atom,illegal_instruction},{atom,t},1},
	    {label,2},
	    {my_illegal_instruction,{x,0}},
	    return]},
	  {function,x,1,4,
	   [{label,3},
	    bad_func_info,
	    {label,4},
	    {my_illegal_instruction,{x,0}},
	    return]},
	  {function,y,0,17,[]}],
	 5},
    Errors = beam_val(M),
    [{{illegal_instruction,t,1},
      {{my_illegal_instruction,{x,0}},4,unknown_instruction}},
     {{'_',x,1},{bad_func_info,1,illegal_instruction}},
     {{'_',y,0},{[],0,illegal_instruction}}] = Errors,
    ok.

%% The beam_validator used to assume that a GC guard BIF could
%% do a garbage collection even if it failed. That assumption
%% is not correct, and will cause the beam_validator to reject
%% valid programs such as this test case.
%%
%% (Thanks to Kiran Khaladkar.)
%%
failing_gc_guard_bif(Config) when is_list(Config) ->
    ok = process_request(lists:seq(1, 36)),
    error = process_request([]),
    error = process_request(not_a_list),
    ok.

process_request(ConfId) ->
    case process_request_foo(ConfId) of
	false ->
	    if
		length(ConfId) == 36 ->
		    Response = ok;
		true ->
		    Response = error
	    end
    end,
    process_request_bar(self(), [Response]).

process_request_foo(_) ->
    false.

process_request_bar(Pid, [Response]) when is_pid(Pid) ->
    Response.

map_field_lists(Config) ->
    Errors = do_val(map_field_lists, Config),
    [{{map_field_lists,x,1},
      {{test,has_map_fields,{f,1},{x,0},
	{list,[{atom,a},{atom,a}]}},
       5,
       keys_not_unique}},
     {{map_field_lists,y,1},
      {{test,has_map_fields,{f,3},{x,0},{list,[]}},
       5,
       empty_field_list}}
    ] = Errors.

%% Coverage and smoke test of beam_validator.
cover_bin_opt(_Config) ->
    Ms = [beam_utils_SUITE,
	  bs_match_SUITE,
	  bs_bincomp_SUITE,
	  bs_bit_binaries_SUITE,
	  bs_utf_SUITE],
    test_lib:p_run(fun try_bin_opt/1, Ms),
    ok.

try_bin_opt(Mod) ->
    try
	do_bin_opt(Mod)
    catch
	Class:Error:Stk ->
	    io:format("~p: ~p ~p\n~p\n",
		      [Mod,Class,Error,Stk]),
	    error
    end.

do_bin_opt(Mod) ->
    Beam = code:which(Mod),
    {ok,{Mod,[{abstract_code,
	       {raw_abstract_v1,Abstr}}]}} =
	beam_lib:chunks(Beam, [abstract_code]),
    {ok,Mod,Asm} = compile:forms(Abstr, ['S']),
    do_bin_opt(Mod, Asm).

do_bin_opt(Mod, Asm) ->
    do_bin_opt(fun enable_bin_opt/1, Mod, Asm),
    do_bin_opt(fun remove_bs_start_match/1, Mod, Asm),
    do_bin_opt(fun remove_bs_save/1, Mod, Asm),
    do_bin_opt(fun destroy_ctxt/1, Mod, Asm),
    do_bin_opt(fun destroy_save_point/1, Mod, Asm),
    ok.

do_bin_opt(Transform, Mod, Asm0) ->
    Asm = Transform(Asm0),
    case compile:forms(Asm, [from_asm,no_postopt,return]) of
	{ok,Mod,Code,_Warnings} when is_binary(Code) ->
	    ok;
	{error,Errors0,_} ->
	    %% beam_validator must return errors, not simply crash,
	    %% when illegal code is found.
	    ModString = atom_to_list(Mod),
	    [{ModString,Errors}] = Errors0,
	    _ = [verify_bin_opt_error(E) || E <- Errors],
	    ok
    end.

verify_bin_opt_error({beam_validator,_}) ->
    ok.

enable_bin_opt(Module) ->
    transform_is(fun enable_bin_opt_body/1, Module).

enable_bin_opt_body([_,{'%',{no_bin_opt,_Reason,_Anno}}|Is]) ->
    enable_bin_opt_body(Is);
enable_bin_opt_body([I|Is]) ->
    [I|enable_bin_opt_body(Is)];
enable_bin_opt_body([]) ->
    [].

remove_bs_start_match(Module) ->
    transform_remove(fun({test,bs_start_match2,_,_,_,_}) -> true;
			(_) -> false
		     end, Module).

remove_bs_save(Module) ->
    transform_remove(fun({bs_save2,_,_}) -> true;
			(_) -> false
		     end, Module).

destroy_save_point(Module) ->
    transform_i(fun do_destroy_save_point/1, Module).

do_destroy_save_point({I,Ctx,_Point})
  when I =:= bs_save2; I =:= bs_restore2 ->
    {I,Ctx,42};
do_destroy_save_point(I) ->
    I.

destroy_ctxt(Module) ->
    transform_i(fun do_destroy_ctxt/1, Module).

do_destroy_ctxt({bs_save2=I,Ctx,Point}) ->
    {I,destroy_reg(Ctx),Point};
do_destroy_ctxt({bs_restore2=I,Ctx,Point}) ->
    {I,destroy_reg(Ctx),Point};
do_destroy_ctxt({bs_context_to_binary=I,Ctx}) ->
    {I,destroy_reg(Ctx)};
do_destroy_ctxt(I) ->
    I.

destroy_reg({Tag,N}) ->
    case rand:uniform() of
	R when R < 0.6 ->
	    {Tag,N+1};
	_ ->
	    {y,N+1}
    end.

bad_tuples(Config) ->
    Errors = do_val(bad_tuples, Config),
    [{{bad_tuples,heap_overflow,1},
      {{put,{x,0}},8,{heap_overflow,{left,0},{wanted,1}}}},
     {{bad_tuples,long,2},
      {{put,{atom,too_long}},8,not_building_a_tuple}},
     {{bad_tuples,self_referential,1},
      {{put,{x,1}},7,{tuple_in_progress,{x,1}}}},
     {{bad_tuples,short,1},
      {{move,{x,1},{x,0}},7,{tuple_in_progress,{x,1}}}}] = Errors,

    ok.

bad_try_catch_nesting(Config) ->
    Errors = do_val(bad_try_catch_nesting, Config),
    [{{bad_try_catch_nesting,main,2},
      {{'try',{y,2},{f,3}},
       7,
       {bad_try_catch_nesting,{y,2},[{{y,1},{trytag,[5]}}]}}}] = Errors,
    ok.

receive_stacked(Config) ->
    Mod = ?FUNCTION_NAME,
    Errors = do_val(Mod, Config),
    [{{receive_stacked,f1,0},
      {{loop_rec_end,{f,3}},
       17,
       {fragile_message_reference,{y,_}}}},
     {{receive_stacked,f2,0},
      {{test_heap,3,0},10,{fragile_message_reference,{y,_}}}},
     {{receive_stacked,f3,0},
      {{test_heap,3,0},10,{fragile_message_reference,{y,_}}}},
     {{receive_stacked,f4,0},
      {{test_heap,3,0},10,{fragile_message_reference,{y,_}}}},
     {{receive_stacked,f5,0},
      {{loop_rec_end,{f,23}},
       23,
       {fragile_message_reference,{y,_}}}},
     {{receive_stacked,f6,0},
      {{gc_bif,byte_size,{f,29},0,[{y,_}],{x,0}},
       12,
       {fragile_message_reference,{y,_}}}},
     {{receive_stacked,f7,0},
      {{loop_rec_end,{f,33}},
       20,
       {fragile_message_reference,{y,_}}}},
     {{receive_stacked,f8,0},
      {{loop_rec_end,{f,38}},
       20,
       {fragile_message_reference,{y,_}}}},
     {{receive_stacked,m1,0},
      {{loop_rec_end,{f,43}},
       19,
       {fragile_message_reference,{y,_}}}},
     {{receive_stacked,m2,0},
      {{loop_rec_end,{f,48}},
       33,
       {fragile_message_reference,{y,_}}}}] = Errors,

    %% Compile the original source code as a smoke test.
    Data = proplists:get_value(data_dir, Config),
    Base = atom_to_list(Mod),
    File = filename:join(Data, Base),
    {ok,Mod,_} = compile:file(File, [binary]),

    ok.

aliased_types(Config) ->
    Seq = lists:seq(1, 5),
    1 = aliased_types_1(Seq, Config),

    {1,1} = aliased_types_2(Seq),
    {42,none} = aliased_types_2([]),

    gurka = aliased_types_3([gurka]),
    gaffel = aliased_types_3([gaffel]),

    ok.

%% ERL-735: validator failed to track types on aliased registers, rejecting
%% legitimate optimizations.
%%
%%    move x0 y0
%%    bif hd L1 x0
%%    get_hd y0     %% The validator failed to see that y0 was a list
%%
aliased_types_1(Bug, Config) ->
    if
        Config =/= [gurka, gaffel] -> %% Pointless branch.
            _ = hd(Bug),
            lists:seq(1, 5),
            hd(Bug)
    end.

%% ERL-832: validator failed to realize that a Y register was a cons.
aliased_types_2(Bug) ->
    Res = case Bug of
              [] -> id(42);
              _ -> hd(Bug)
          end,
    {Res,case Bug of
             [] -> none;
             _ -> hd(Bug)
         end}.

%% ERL-832 part deux; validator failed to realize that an aliased register was
%% a cons.
aliased_types_3(Bug) ->
    List = [Y || Y <- Bug],
    case List of
        [] -> Bug;
        _ ->
            if
                hd(List) -> a:a();
                true -> ok
            end,
            hd(List)
    end.


%% ERL-867; validation proceeded after a type conflict, causing incorrect types
%% to be joined.

-record(r, { e1 = e1, e2 = e2 }).

type_conflict(Config) when is_list(Config) ->
    {e1, e2} = type_conflict_1(#r{}),
    ok.

type_conflict_1(C) ->
    Src = id(C#r.e2),
    TRes = try id(Src) of
               R -> R
           catch
               %% C:R can never match, yet it assumed that the type of 'C' was
               %% an atom from here on.
               C:R -> R
           end,
    {C#r.e1, TRes}.

%% ERL-886; validation failed to infer types on both sides of '=:='

infer_on_eq(Config) when is_list(Config) ->
    {ok, gurka} = infer_on_eq_1(id({gurka})),
    {ok, gaffel} = infer_on_eq_2(id({gaffel})),
    {ok, elefant} = infer_on_eq_3(id({elefant})),
    {ok, myra} = infer_on_eq_4(id({myra})),
    ok.

infer_on_eq_1(T) ->
    1 = erlang:tuple_size(T),
    {ok, erlang:element(1, T)}.

infer_on_eq_2(T) ->
    Size = erlang:tuple_size(T),
    Size = 1,
    {ok, erlang:element(1, T)}.

infer_on_eq_3(T) ->
    true = 1 =:= erlang:tuple_size(T),
    {ok, erlang:element(1, T)}.

infer_on_eq_4(T) ->
    true = erlang:tuple_size(T) =:= 1,
    {ok, erlang:element(1, T)}.

%% ERIERL-348; types were inferred for dead values, causing validation to fail.

-record(idv, {key}).

infer_dead_value(Config) when is_list(Config) ->
    a = idv_1({a, b, c, d, e, f, g}, {0, 0, 0, 0, 0, 0, 0}),
    b = idv_1({a, b, c, d, 0, 0, 0}, {a, b, c, d, 0, 0, 0}),
    c = idv_1({0, 0, 0, 0, 0, f, g}, {0, 0, 0, 0, 0, f, g}),
    error = idv_1(gurka, gaffel),

    ok = idv_2(id(#idv{})),

    ok.

idv_1({_A, _B, _C, _D, _E, _F, _G},
      {0, 0, 0, 0, 0, 0, 0}) ->
    a;
idv_1({A, B, C, D,_E, _F, _G}=_Tuple1,
      {A, B, C, D, 0, 0, 0}=_Tuple2) ->
    b;
idv_1({_A, _B, _C, _D, _E, F, G},
      {0, 0, 0, 0, 0, F, G}) ->
    c;
idv_1(_A, _B) ->
    error.

%% ERL-995: The first solution to ERIERL-348 was incomplete and caused
%% validation to fail when living values depended on delayed type inference on
%% "dead" values.

idv_2(State) ->
    Flag = (State#idv.key == undefined),
    case id(gurka) of
        {_} -> id([Flag]);
        _ -> ok
    end,
    if
        Flag -> idv_called_once(State);
        true -> ok
    end.

idv_called_once(_State) -> ok.

receive_marker(Config) when is_list(Config) ->
    Errors = do_val(receive_marker, Config),

    [{{receive_marker,t1,1},
      {return,_,
       {return_with_receive_marker,committed}}},
     {{receive_marker,t2,1},
      {{call_last,1,{f,2},1},_,
       {return_with_receive_marker,committed}}}] = Errors,

    ok.

%% ERL-1128: the validator erroneously thought that many non-throwing
%% instructions like is_eq_exact could throw.
safe_instructions(Config) when is_list(Config) ->
    Errors = do_val(safe_instructions, Config),

    [] = Errors,

    ok.

missing_return_type(Config) when is_list(Config) ->
    %% ERL-1161: the validator didn't know that is_map_key always returns a
    %% bool.
    Map = #{ hello => there },
    true = mrt_1(true),
    false = mrt_1(false),
    true = mrt_1(is_map_key(id(hello), Map)),
    false = mrt_1(is_map_key(id(there), Map)),

    ok.

mrt_1(Bool) ->
    true = is_boolean(Bool),
    Bool.

%%%-------------------------------------------------------------------------

transform_remove(Remove, Module) ->
    transform_is(fun(Is) -> [I || I <- Is, not Remove(I)] end, Module).

transform_i(Transform, Module) ->
    transform_is(fun(Is) -> [Transform(I) || I <- Is] end, Module).

transform_is(Transform, {Mod,Exp,Imp,Fs0,Lc}) ->
    Fs = [transform_is_1(Transform, F) || F <- Fs0],
    {Mod,Exp,Imp,Fs,Lc}.

transform_is_1(Transform, {function,N,A,E,Is0}) ->
    Is = Transform(Is0),
    {function,N,A,E,Is}.

do_val(Mod, Config) ->
    Data = proplists:get_value(data_dir, Config),
    Base = atom_to_list(Mod),
    File = filename:join(Data, Base),
    case compile:file(File, [from_asm,no_postopt,return_errors]) of
	{error,L,[]} ->
	    [{Base,Errors0}] = L,
	    Errors = [E || {beam_validator,E} <- Errors0],
	    _ = [io:put_chars(beam_validator:format_error(E)) ||
		    E <- Errors],
	    Errors;
	{ok,Mod} ->
	    []
    end.

beam_val(M) ->
    Name = atom_to_list(element(1, M)),
    {error,[{Name,Errors0}]} = beam_validator:module(M, []),
    Errors = [E || {beam_validator,E} <- Errors0],
    _ = [io:put_chars(beam_validator:format_error(E)) ||
	    E <- Errors],
    Errors.

%%%-------------------------------------------------------------------------

val_dsetel(_Config) ->
    self() ! 13,
    {'EXIT',{{try_clause,participating},_}} = (catch night(0)),
    ok.

night(Turned) ->
    receive
	13 ->
	    try participating of engine -> 16 after false end
    end,
    %% The setelement/3 call is unreachable.
    Turned(setelement(#{true => Turned},
		      participating(Turned, "suit", 40, []),
		      Turned < Turned)),
    ok.

participating(_, _, _, _) -> ok.

id(I) ->
    I.
