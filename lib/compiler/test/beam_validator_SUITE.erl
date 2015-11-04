%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2013. All Rights Reserved.
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
	 map_field_lists/1]).
	 
-include_lib("test_server/include/test_server.hrl").

init_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Dog = test_server:timetrap(?t:minutes(10)),
    [{watchdog,Dog}|Config].

end_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
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
       map_field_lists]}].

init_per_suite(Config) ->
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
    Data = ?config(data_dir, Config),
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
    ?line try setelement(5, setelement(6, AnAtom, value), another_value) of
	      Term -> ?line ?t:fail({what_happened,Term})
	  catch
	      error:badarg -> ok
	  end,
    ok.

xrange(Config) when is_list(Config) ->
    Errors = do_val(xrange, Config),
    ?line
	[{{t,sum_1,2},
	  {{bif,'+',{f,0},[{x,-1},{x,1}],{x,0}},4,
	   {uninitialized_reg,{x,-1}}}},
	 {{t,sum_2,2},
	  {{bif,'+',{f,0},[{x,0},{x,1023}],{x,0}},4,
	   {uninitialized_reg,{x,1023}}}},
	 {{t,sum_3,2},
	  {{bif,'+',{f,0},[{x,0},{x,1}],{x,-1}},4,
	   {invalid_store,{x,-1},number}}},
	 {{t,sum_4,2},
	  {{bif,'+',{f,0},[{x,0},{x,1}],{x,1023}},4,limit}}] = Errors,
    ok.

yrange(Config) when is_list(Config) ->
    Errors = do_val(yrange, Config),
    ?line
	[{{t,sum_1,2},
	  {{move,{x,1},{y,-1}},5,
	   {invalid_store,{y,-1},term}}},
	 {{t,sum_2,2},
	  {{bif,'+',{f,0},[{x,0},{y,1024}],{x,0}},7,
	   {uninitialized_reg,{y,1024}}}},
	 {{t,sum_3,2},
	  {{move,{x,1},{y,1024}},5,limit}},
	 {{t,sum_4,2},
	  {{move,{x,1},{y,-1}},5,
	   {invalid_store,{y,-1},term}}}] = Errors,
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
       {uninitialized_reg,{y,0}}}}] = Errors,
    ok.

uninit(Config) when is_list(Config) ->
    Errors = do_val(uninit, Config),
    ?line
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
    ?line
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
    ?line
	[{{overwrite_catchtag,foo,1},
	  {{move,{x,0},{y,0}},6,{catchtag,_}}}] = Errors,
    ok.

overwrite_trytag(Config) when is_list(Config) ->
    Errors = do_val(overwrite_trytag, Config),
    ?line
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
       5,{invalid_store,{x,0},{catchtag,[3]}}}},
     {{bad_catch_try,bad_2,1},
      {{catch_end,{x,9}},
       8,{source_not_y_reg,{x,9}}}},
     {{bad_catch_try,bad_3,1},
      {{catch_end,{y,1}},9,{bad_type,{atom,kalle}}}},
     {{bad_catch_try,bad_4,1},
      {{'try',{x,0},{f,15}},5,{invalid_store,{x,0},{trytag,[15]}}}},
     {{bad_catch_try,bad_5,1},
      {{try_case,{y,1}},12,{bad_type,term}}},
     {{bad_catch_try,bad_6,1},
      {{move,{integer,1},{y,1}},7,
       {invalid_store,{y,1},{integer,1}}}}] = Errors,
    ok.

cons_guard(Config) when is_list(Config) ->
    Errors = do_val(cons, Config),
    ?line
	[{{cons,foo,1},
	  {{get_list,{x,0},{x,1},{x,2}},
	   5,
	   {bad_type,{needed,cons},{actual,term}}}}] = Errors,
    ok.

freg_range(Config) when is_list(Config) ->
    Errors = do_val(freg_range, Config),
    ?line 
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
	   {bad_target,{fr,-1}}}},
	 {{t,sum_4,2},
	  {{bif,fadd,{f,0},[{fr,0},{fr,1}],{fr,1024}},
	   7,
	   limit}}] = Errors,
    ok.

freg_uninit(Config) when is_list(Config) ->
    Errors = do_val(freg_uninit, Config),
    ?line 
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
    ?line
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
    ?line
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
      {{move,{x,3},{x,0}},88,{uninitialized_reg,{x,3}}}}] = Errors,
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
    ?line ok = process_request(lists:seq(1, 36)),
    ?line error = process_request([]),
    ?line error = process_request(not_a_list),
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

%%%-------------------------------------------------------------------------

do_val(Mod, Config) ->
    Data = ?config(data_dir, Config),
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
