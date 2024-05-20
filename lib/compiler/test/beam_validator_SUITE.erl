%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2024. All Rights Reserved.
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
	 freg_range/1,freg_uninit/1,
	 bad_bin_match/1,bad_dsetel/1,
	 state_after_fault_in_catch/1,no_exception_in_catch/1,
	 undef_label/1,illegal_instruction/1,failing_gc_guard_bif/1,
	 map_field_lists/1,cover_bin_opt/1,
	 val_dsetel/1,bad_tuples/1,bad_try_catch_nesting/1,
         receive_stacked/1,aliased_types/1,type_conflict/1,
         infer_on_eq/1,infer_dead_value/1,infer_on_ne/1,
         branch_to_try_handler/1,call_without_stack/1,
         receive_marker/1,safe_instructions/1,
         missing_return_type/1,will_succeed/1,
         bs_saved_position_units/1,parent_container/1,
         container_performance/1,
         infer_relops/1,
         not_equal_inference/1,bad_bin_unit/1,singleton_inference/1,
         inert_update_type/1,range_inference/1,
         bif_inference/1]).

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
       bad_bin_match,bad_dsetel,
       state_after_fault_in_catch,no_exception_in_catch,
       undef_label,illegal_instruction,failing_gc_guard_bif,
       map_field_lists,cover_bin_opt,val_dsetel,
       bad_tuples,bad_try_catch_nesting,
       receive_stacked,aliased_types,type_conflict,
       infer_on_eq,infer_dead_value,infer_on_ne,
       branch_to_try_handler,call_without_stack,
       receive_marker,safe_instructions,
       missing_return_type,will_succeed,
       bs_saved_position_units,parent_container,
       container_performance,infer_relops,
       not_equal_inference,bad_bin_unit,singleton_inference,
       inert_update_type,range_inference,
       bif_inference]}].

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
       [{_Pos,beam_validator,_}]}],
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
     {{t,bad_1,0},{{allocate,2,10},4,{{x,9},not_live}}},
     {{t,bad_2,0},{{move,{y,0},{x,0}},6,{unassigned,{y,0}}}},
     {{t,c,2},{{deallocate,2},10,{allocated,none}}},
     {{t,d,2},
      {{allocate,2,2},5,{existing_stack_frame,{size,2}}}},
     {{t,e,2},{{deallocate,5},6,{allocated,2}}}] = Errors,
    ok.

call_last(Config) when is_list(Config) ->
    Errors = do_val(call_last, Config),
    [{{t,a,1},
      {{call_last,1,{f,8},2},9,{allocated,1}}},
     {{t,b,1},
      {{call_ext_last,2,{extfunc,lists,seq,2},2},10,{allocated,1}}},
     {{t,baz,2},
      {{call_ext_only,2,{extfunc,erlang,put,2}},5,{allocated,0}}},
     {{t,biz,2},
      {{call_only,2,{f,10}},5,{allocated,0}}}] = Errors,
    ok.

call_without_stack(Config) when is_list(Config) ->
    Errors = do_val(call_without_stack, Config),
    [{{t,local,2},
        {{call,2,{f,2}},4,{allocated,none}}},
     {{t,remote,2},
        {{call_ext,2,{extfunc,lists,seq,2}},4,{allocated,none}}}] = Errors,
    ok.

merge_undefined(Config) when is_list(Config) ->
    Errors = do_val(merge_undefined, Config),
    [{{t,undecided,2},
      {{label,11},
       19,
       {unsafe_stack,{y,1},
        #{{y,0} := uninitialized,
          {y,1} := uninitialized}}}},
     {{t,uninitialized,2},
      {{call_ext,2,{extfunc,io,format,2}},
       17,
       {uninitialized_reg,{y,1}}}}] = Errors,
    ok.

uninit(Config) when is_list(Config) ->
    Errors = do_val(uninit, Config),
    [{{t,sum_1,2},
      {{move,{y,0},{x,0}},5,{uninitialized_reg,{y,0}}}},
     {{t,sum_2,2},
      {{call,1,{f,8}},5,{uninitialized_reg,{y,0}}}},
     {{t,sum_3,2},
      {{bif,'+',{f,0},[{x,0},{y,0}],{x,0}},
       7,
       {unassigned,{y,0}}}}] = Errors,
    ok.

unsafe_catch(Config) when is_list(Config) ->
    Errors = do_val(unsafe_catch, Config),
    [{{t,small,2},
      {{bs_put_integer,{f,0},{integer,16},1,
        {field_flags,[unsigned,big]},{y,0}},
       21,
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
      {{init_yregs,{list,[{y,2}]}},9,{trytag,_}}}] = Errors,
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
      {{catch_end,{y,1}},9,{invalid_tag,{y,1},{t_atom,[kalle]}}}},
     {{bad_catch_try,bad_4,1},
      {{'try',{x,0},{f,15}},6,{invalid_tag_register,{x,0}}}},
     {{bad_catch_try,bad_5,1},
      {{try_case,{y,1}},13,{invalid_tag,{y,1},any}}},
     {{bad_catch_try,bad_6,1},
      {{move,{integer,1},{y,1}},8,
       {invalid_store,{y,1}}}}] = Errors,
    ok.

cons_guard(Config) when is_list(Config) ->
    Errors = do_val(cons, Config),
    [{{cons,foo,1},
      {{get_list,{x,0},{x,1},{x,2}},
       5,
       {bad_type,{needed,{t_cons,any,any}},{actual,any}}}}] = Errors,
    ok.

freg_range(Config) when is_list(Config) ->
    Errors = do_val(freg_range, Config),
    [{{t,sum_1,2},
      {{bif,fadd,{f,0},[{fr,-1},{fr,1}],{fr,0}},
       4,
       {bad_source,{fr,-1}}}},
     {{t,sum_2,2},
      {{bif,fadd,{f,0},[{fr,0},{fr,1024}],{fr,0}},
       5,
       {uninitialized_reg,{fr,1024}}}},
     {{t,sum_3,2},
      {{bif,fadd,{f,0},[{fr,0},{fr,1}],{fr,-1}},
       6,
       {bad_register,{fr,-1}}}},
     {{t,sum_4,2},
      {{bif,fadd,{f,0},[{fr,0},{fr,1}],{fr,1024}},
       6,
       limit}}] = Errors,
    ok.

freg_uninit(Config) when is_list(Config) ->
    Errors = do_val(freg_uninit, Config),
    [{{t,sum_1,2},
      {{bif,fadd,{f,0},[{fr,0},{fr,1}],{fr,0}},
       5,
       {uninitialized_reg,{fr,1}}}},
     {{t,sum_2,2},
      {{bif,fadd,{f,0},[{fr,0},{fr,1}],{fr,0}},
       8,
       {uninitialized_reg,{fr,0}}}}] = Errors,
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
      {{try_case_end,{x,0}},152,ambiguous_catch_try_state}}] = Errors,
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
     {{undef_label,x,1},no_entry_label}] = Errors,
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
     {{illegal_instruction,x,1},invalid_function_header},
     {{illegal_instruction,y,0},invalid_function_header}] = Errors,
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
      {{test,has_map_fields,{f,1},{x,0},{list,[{atom,a},{atom,a}]}},
       6,
       keys_not_unique}},
     {{map_field_lists,y,1},
      {{test,has_map_fields,{f,3},{x,0},{list,[]}},
       6,
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
      {{put_tuple2,{x,0},{list,[{atom,ok},{x,0}]}},6,
       {heap_overflow,{left,2},{wanted,3}}}}] = Errors,

    ok.

bad_try_catch_nesting(Config) ->
    Errors = do_val(bad_try_catch_nesting, Config),
    [{{bad_try_catch_nesting,main,2},
      {{'try',{y,2},{f,3}},
       9,
       {bad_try_catch_nesting,{y,2},[{{y,1},{trytag,[5]}}]}}}] = Errors,
    ok.

receive_stacked(Config) ->
    Mod = ?FUNCTION_NAME,
    Errors = do_val(Mod, Config),
    [{{receive_stacked,f1,0},
      {{loop_rec_end,{f,3}},
       19,
       {fragile_message_reference,{y,_}}}},
     {{receive_stacked,f2,0},
      {{test_heap,3,0},12,{fragile_message_reference,{y,_}}}},
     {{receive_stacked,f3,0},
      {{test_heap,3,0},12,{fragile_message_reference,{y,_}}}},
     {{receive_stacked,f4,0},
      {{test_heap,3,0},12,{fragile_message_reference,{y,_}}}},
     {{receive_stacked,f5,0},
      {{loop_rec_end,{f,23}},
       23,
       {fragile_message_reference,{y,_}}}},
     {{receive_stacked,f6,0},
      {{gc_bif,byte_size,{f,29},0,[{y,_}],{x,0}},
       14,
       {fragile_message_reference,{y,_}}}},
     {{receive_stacked,f7,0},
      {{loop_rec_end,{f,33}},
       22,
       {fragile_message_reference,{y,_}}}},
     {{receive_stacked,f8,0},
      {{loop_rec_end,{f,38}},
       22,
       {fragile_message_reference,{y,_}}}},
     {{receive_stacked,m1,0},
      {{loop_rec_end,{f,43}},
       21,
       {fragile_message_reference,{y,_}}}},
     {{receive_stacked,m2,0},
      {{loop_rec_end,{f,48}},
       32,
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

%% ERL-998; type inference for select_val (#b_switch{}) was more clever than
%% that for is_ne_exact (#b_br{}), sometimes failing validation when the type
%% optimization pass acted on the former and the validator got the latter.

-record(ion, {state}).

infer_on_ne(Config) when is_list(Config) ->
    #ion{state = closing} = ion_1(#ion{ state = id(open) }),
    #ion{state = closing} = ion_close(#ion{ state = open }),
    ok.

ion_1(State = #ion{state = open}) -> ion_2(State);
ion_1(State = #ion{state = closing}) -> ion_2(State).

ion_2(State = #ion{state = open}) -> ion_close(State);
ion_2(#ion{state = closing}) -> ok.

ion_close(State = #ion{}) -> State#ion{state = closing}.

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

%% Direct jumps to try/catch handlers crash the emulator and must fail
%% validation. This is provoked by OTP-15945.

branch_to_try_handler(Config) ->
    Errors = do_val(branch_to_try_handler, Config),
    [{{branch_to_try_handler,main,1},
      {{bif,tuple_size,{f,3},[{y,0}],{x,0}},
       13,
       {illegal_branch,try_handler,3}}}] = Errors,
    ok.

receive_marker(Config) when is_list(Config) ->
    Errors = do_val(receive_marker, Config),

    [{{receive_marker,t1,1},
      {return,_,
       {return_in_receive,entered_loop}}},
     {{receive_marker,t2,1},
      {{call_last,1,{f,2},1},_,
       {return_in_receive,entered_loop}}},
     {{receive_marker,t3,1},
      {return,_,
       {return_in_receive,entered_loop}}}] = Errors,

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

%% ERL-1340: the unit of previously saved match positions wasn't updated.
bs_saved_position_units(Config) when is_list(Config) ->
    M = {bs_saved_position_units,
         [{no_errors,1},{some_errors,1}],
         [],
         [{function,ctx_test_8,1,2,
              [{label,1},
               {func_info,{atom,bs_saved_position_units},{atom,ctx_test_8},1},
               {label,2},
               {'%',
                   {var_info,
                       {x,0},
                       [{type,{t_bs_context,8}},accepts_match_context]}},
               {move,nil,{x,0}},
               return]},
          {function,no_errors,1,4,
              [{label,3},
               {func_info,{atom,bs_saved_position_units},{atom,no_errors},1},
               {label,4},
               {'%',{var_info,{x,0},[accepts_match_context]}},
               {test,bs_start_match3,{f,3},1,[{x,0}],{x,1}},
               {bs_get_position,{x,1},{x,0},2},
               {test,bs_test_unit,{f,5},[{x,1},8]},
               {bs_set_position,{x,1},{x,0}},
               {test,bs_get_binary2,
                   {f,5},
                   2,
                   [{x,1},{atom,all},1,{field_flags,[unsigned,big]}],
                   {x,2}},
               {bs_set_position,{x,1},{x,0}},
               {bs_get_tail,{x,1},{x,0},3},
               {test,is_eq_exact,{f,5},[{x,2},{x,0}]},
               {move,{x,1},{x,0}},
               %% Context unit should be 8 here.
               {call_only,1,{f,2}},
               {label,5},
               {bs_get_tail,{x,1},{x,0},2},
               {jump,{f,3}}]},
          {function,some_errors,1,7,
              [{label,6},
               {func_info,{atom,bs_saved_position_units},{atom,some_errors},1},
               {label,7},
               {'%',{var_info,{x,0},[accepts_match_context]}},
               {test,bs_start_match3,{f,6},1,[{x,0}],{x,1}},
               {bs_get_position,{x,1},{x,0},2},
               {test,bs_get_binary2,
                   {f,8},
                   2,
                   [{x,1},{atom,all},4,{field_flags,[unsigned,big]}],
                   {x,2}},
               {bs_set_position,{x,1},{x,0}},
               {test,bs_test_unit,{f,9},[{x,1},3]},
               {bs_set_position,{x,1},{x,0}},
               {bs_get_tail,{x,1},{x,0},3},
               {test,is_eq_exact,{f,8},[{x,2},{x,0}]},
               {move,{x,1},{x,0}},
               %% Context unit should be 12 here, failing validation.
               {call_only,1,{f,2}},
               {label,8},
               {bs_get_tail,{x,1},{x,0},2},
               {jump,{f,6}},
               {label,9},
               %% Context unit should be 4 here.
               {move,nil,{x,0}},
               return]}],
         10},

    Errors = beam_val(M),

    [{{bs_saved_position_units,some_errors,1},
      {{call_only,1,{f,2}},
       14,
       {bad_arg_type,{x,0},
                     {t_bs_context,12},
                     {t_bs_context,8}}}}] = Errors,

    ok.

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
	    Errors = [E || {_Pos,beam_validator,E} <- Errors0],
	    _ = [io:put_chars(beam_validator:format_error(E)) ||
		    E <- Errors],
	    Errors;
	{ok,Mod} ->
	    []
    end.

beam_val(M) ->
    Name = atom_to_list(element(1, M)),
    {error,[{Name,Errors0}]} = beam_validator:validate(M, strong),
    Errors = [E || {_Pos,beam_validator,E} <- Errors0],
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

will_succeed(_Config) ->
    ok = will_succeed_1(body),

    self() ! whatever,
    error = will_succeed_2(),

    self() ! whatever,
    error = will_succeed_3(),

    ok.

%% map_get was known as returning 'none', but 'will_succeed' still returned
%% 'maybe' causing validation to continue, eventually exploding when the 'none'
%% value was used.
%%
%% +no_ssa_opt
will_succeed_1(body) when map_get(girl, #{friend => node()}); [], community ->
    case $q and $K of
        _V0 ->
            0.1825965401179273;
        0 ->
            state#{[] => 0.10577334580729858, $J => 0}
    end;
will_succeed_1(body) ->
    ok.

%% The apply of 42:name/0 was known to fail, but 'will_succeed' still
%% returned 'maybe', causing validation to continue and fail because
%% of the jump to the try_case instruction.
will_succeed_2() ->
    try
        receive
            _ ->
                42
        end:name()
    catch
        _:_ ->
            error
    end.

will_succeed_3() ->
    try
        receive
            _ ->
                42
        end:name(a, b)
    catch
        _:_ ->
            error
    end.

%% ERL-1426: When a value was extracted from a tuple, subsequent type tests did
%% not update the type of said tuple.

-record(pc, {a}).

parent_container(_Config) ->
    ok = pc_1(id(#pc{a=true})).

pc_1(#pc{a=A}=R) ->
    case A of
        true -> ok;
        false -> ok
    end,
    ok = pc_2(R).

pc_2(_R) ->
    ok.

%% GH-5915: The following function took an incredibly long time to validate.
container_performance(Config) ->
    case Config of
        ({b,_}) -> {k1};
        ({a,{b,_}}) -> {k2};
        ({a,{a,{b,_}}}) -> {k3};
        ({a,{a,{a,{b,_}}}}) -> {k4};
        ({a,{a,{a,{a,{b,_}}}}}) -> {k5};
        ({a,{a,{a,{a,{a,{b,_}}}}}}) -> {k6};
        ({a,{a,{a,{a,{a,{a,{b,_}}}}}}}) -> {k7};
        ({a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}) -> {k8};
        ({a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}) -> {k9};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}) -> {k10};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}) -> {k11};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}}) -> {k12};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}}}) -> {k13};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}}}}) -> {k14};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}}}}}) -> {k15};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}}}}}}) -> {k16};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}}}}}}}) -> {k17};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}}}}}}}}) -> {k18};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}}}}}}}}}) -> {k19};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}}}}}}}}}}) -> {k20};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}}}}}}}}}}}) -> {k21};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}}}}}}}}}}}}) -> {k22};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}}}}}}}}}}}}}) -> {k23};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}}}}}}}}}}}}}}) -> {k24};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}}}}}}}}}}}}}}}) -> {k25};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}}}}}}}}}}}}}}}}) -> {k26};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}}}}}}}}}}}}}}}}}) -> {k27};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}}}}}}}}}}}}}}}}}}) -> {k28};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{b,_}}}}}}}}}}}}}}}}}}}}}}}}}}}}}) -> {k29};
        ({a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,{a,_}}}}}}}}}}}}}}}}}}}}}}}}}}}}}) -> {k30};
        _ -> ok
    end.

%% Type inference was half-broken for relational operators, being implemented
%% for is_lt/is_ge instructions but not the {bif,RelOp} form.
infer_relops(_Config) ->
    [lt = infer_relops_1(N) || N <- lists:seq(0,3)],
    [ge = infer_relops_1(N) || N <- lists:seq(4,7)],
    ok.

infer_relops_1(N) ->
    true = N >= 0,
    Below4 = N < 4,
    id(N), %% Force Below4 to use the {bif,'<'} form instead of is_lt
    case Below4 of
        true -> infer_relops_true(Below4, N);
        false -> infer_relops_false(Below4, N)
    end.

infer_relops_true(_, _) -> lt.
infer_relops_false(_, _) -> ge.

%% OTP-18365: A brainfart in inference for '=/=' inverted the results.
not_equal_inference(_Config) ->
    {'EXIT', {function_clause, _}} = (catch not_equal_inference_1(id([0]))),
    ok.

not_equal_inference_1(X) when (X /= []) /= is_port(0 div 0) ->
    [X || _ <- []].

bad_bin_unit(_Config) ->
    {'EXIT', {function_clause,_}} = catch bad_bin_unit_1(<<1:1>>),
    [] = bad_bin_unit_2(),
    ok.

bad_bin_unit_1(<<X:((ok > {<<(true andalso ok)>>}) orelse 1)>>) ->
    try
        bad_bin_unit_1_a()
    after
        -(X + bad_bin_unit_1_b(not ok)),
        try
            ok
        catch
            _ ->
                ok;
            _ ->
                ok;
            _ ->
                ok;
            _ ->
                ok;
            _ ->
                ok;
            _ ->
                ok
        end
    end.

bad_bin_unit_1_a() -> ok.
bad_bin_unit_1_b(_) -> ok.

bad_bin_unit_2() ->
   [
       ok
       || <<X:(is_number(<<(<<(0 bxor 0)>>)>>) orelse 1)>> <= <<>>,
       #{X := _} <- ok
   ].

%% GH-6962: Type inference with singleton types in registers was weaker than
%% inference on their corresponding literals.
singleton_inference(Config) ->
    Mod = ?FUNCTION_NAME,

    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "singleton_inference.erl"),

    {ok, Mod} = compile:file(File, [no_copt, no_bool_opt, no_ssa_opt]),

    ok = Mod:test(),

    ok.

%% GH-6969: A type was made concrete even though that added no additional
%% information.
inert_update_type(_Config) ->
    hello(<<"string">>, id(42)).

hello(A, B) ->
    mike([{sys_period, {A, B}}, {some_atom, B}]).

mike([Head | _Rest]) -> joe(Head).

joe({Name, 42}) -> Name;
joe({sys_period, {A, _B}}) -> {41, 42, A}.

range_inference(_Config) ->
    ok = range_inference_1(id(<<$a>>)),
    ok = range_inference_1(id(<<0>>)),
    ok = range_inference_1(id(<<1114111/utf8>>)),

    ok.

range_inference_1(<<X/utf8>>) ->
    case 9223372036854775807 - abs(X) of
        Y when X < Y ->
            ok;
        9223372036854775807 ->
            ok;
        -2147483648 ->
            ok
    end.

bif_inference(_Config) ->
    ok = bif_inference_is_bitstring(id(<<>>), id(<<>>)),
    error = bif_inference_is_bitstring(id(a), id(a)),

    ok = bif_inference_is_function(id(fun id/1), id(fun id/1)),
    ok = bif_inference_is_function(true, true),
    error = bif_inference_is_function(id(fun id/1), a),
    error = bif_inference_is_function(a, a),

    ok.

bif_inference_is_bitstring(A, A) when A andalso ok; is_bitstring(A) ->
    ok;
bif_inference_is_bitstring(_, _) ->
    error.

bif_inference_is_function(A, A)  when A orelse ok; is_function(A) ->
    ok;
bif_inference_is_function(_, _) ->
    error.

id(I) ->
    I.
