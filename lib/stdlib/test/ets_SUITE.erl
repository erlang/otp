%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2013. All Rights Reserved.
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
-module(ets_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([default/1,setbag/1,badnew/1,verybadnew/1,named/1,keypos2/1,
	 privacy/1,privacy_owner/2]).
-export([empty/1,badinsert/1]).
-export([time_lookup/1,badlookup/1,lookup_order/1]).
-export([delete_elem/1,delete_tab/1,delete_large_tab/1,
	 delete_large_named_table/1,
	 evil_delete/1,baddelete/1,match_delete/1,table_leak/1]).
-export([match_delete3/1]).
-export([firstnext/1,firstnext_concurrent/1]).
-export([slot/1]).
-export([ match1/1, match2/1, match_object/1, match_object2/1]).
-export([ dups/1, misc1/1, safe_fixtable/1, info/1, tab2list/1]).
-export([ tab2file/1, tab2file2/1, tabfile_ext1/1,
	tabfile_ext2/1, tabfile_ext3/1, tabfile_ext4/1]).
-export([ heavy_lookup/1, heavy_lookup_element/1, heavy_concurrent/1]).
-export([ lookup_element_mult/1]).
-export([]).
-export([foldl_ordered/1, foldr_ordered/1, foldl/1, foldr/1, fold_empty/1]).
-export([t_delete_object/1, t_init_table/1, t_whitebox/1, 
	 t_delete_all_objects/1, t_insert_list/1, t_test_ms/1,
	 t_select_delete/1,t_ets_dets/1]).

-export([do_lookup/2, do_lookup_element/3]).

-export([ordered/1, ordered_match/1, interface_equality/1,
	 fixtable_next/1, fixtable_insert/1, rename/1, rename_unnamed/1, evil_rename/1,
	 update_element/1, update_counter/1, evil_update_counter/1, partly_bound/1, match_heavy/1]).
-export([member/1]).
-export([memory/1]).
-export([select_fail/1]).
-export([t_insert_new/1]).
-export([t_repair_continuation/1]).
-export([t_match_spec_run/1]).
-export([t_bucket_disappears/1]).
-export([otp_5340/1]).
-export([otp_6338/1]).
-export([otp_6842_select_1000/1]).
-export([otp_7665/1]).
-export([meta_wb/1]).
-export([grow_shrink/1, grow_pseudo_deleted/1, shrink_pseudo_deleted/1]).
-export([
	 meta_lookup_unnamed_read/1, meta_lookup_unnamed_write/1, 
	 meta_lookup_named_read/1, meta_lookup_named_write/1,
	 meta_newdel_unnamed/1, meta_newdel_named/1]).
-export([smp_insert/1, smp_fixed_delete/1, smp_unfix_fix/1, smp_select_delete/1,
         otp_8166/1, otp_8732/1]).
-export([exit_large_table_owner/1,
	 exit_many_large_table_owner/1,
	 exit_many_tables_owner/1,
	 exit_many_many_tables_owner/1]).
-export([write_concurrency/1, heir/1, give_away/1, setopts/1]).
-export([bad_table/1, types/1]).
-export([otp_9932/1]).
-export([otp_9423/1]).
-export([otp_10182/1]).
-export([memory_check_summary/1]).

-export([init_per_testcase/2, end_per_testcase/2]).
%% Convenience for manual testing
-export([random_test/0]).

% internal exports
-export([dont_make_worse_sub/0, make_better_sub1/0, make_better_sub2/0]).
-export([t_repair_continuation_do/1, t_bucket_disappears_do/1,
	 select_fail_do/1, whitebox_1/1, whitebox_2/1, t_delete_all_objects_do/1,
	 t_delete_object_do/1, t_init_table_do/1, t_insert_list_do/1,
	 update_element_opts/1, update_element_opts/4, update_element/4, update_element_do/4,
	 update_element_neg/1, update_element_neg_do/1, update_counter_do/1, update_counter_neg/1,
	 evil_update_counter_do/1, fixtable_next_do/1, heir_do/1, give_away_do/1, setopts_do/1,
	 rename_do/1, rename_unnamed_do/1, interface_equality_do/1, ordered_match_do/1,
	 ordered_do/1, privacy_do/1, empty_do/1, badinsert_do/1, time_lookup_do/1,
	 lookup_order_do/1, lookup_element_mult_do/1, delete_tab_do/1, delete_elem_do/1,
	 match_delete_do/1, match_delete3_do/1, firstnext_do/1, 
	 slot_do/1, match1_do/1, match2_do/1, match_object_do/1, match_object2_do/1,
	 misc1_do/1, safe_fixtable_do/1, info_do/1, dups_do/1, heavy_lookup_do/1,
	 heavy_lookup_element_do/1, member_do/1, otp_5340_do/1, otp_7665_do/1, meta_wb_do/1,
	 do_heavy_concurrent/1, tab2file2_do/2, exit_large_table_owner_do/2,
         types_do/1, sleeper/0, memory_do/1,
	 ms_tracee_dummy/1, ms_tracee_dummy/2, ms_tracee_dummy/3, ms_tracee_dummy/4
	]).

-export([t_select_reverse/1]).

-include_lib("test_server/include/test_server.hrl").

-define(m(A,B), ?line assert_eq(A,B)).

init_per_testcase(Case, Config) ->
    Seed = {S1,S2,S3} = random:seed0(), %now(),
    random:seed(S1,S2,S3),
    io:format("*** SEED: ~p ***\n", [Seed]),
    start_spawn_logger(),
    wait_for_test_procs(), %% Ensure previous case cleaned up
    Dog=test_server:timetrap(test_server:minutes(20)),
    [{watchdog, Dog}, {test_case, Case} | Config].

end_per_testcase(_Func, Config) ->
    Dog=?config(watchdog, Config),
    wait_for_test_procs(true),
    test_server:timetrap_cancel(Dog).
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group, new}, {group, insert}, {group, lookup},
     {group, delete}, firstnext, firstnext_concurrent, slot,
     {group, match}, t_match_spec_run,
     {group, lookup_element}, {group, misc}, {group, files},
     {group, heavy}, ordered, ordered_match,
     interface_equality, fixtable_next, fixtable_insert,
     rename, rename_unnamed, evil_rename, update_element,
     update_counter, evil_update_counter, partly_bound,
     match_heavy, {group, fold}, member, t_delete_object,
     t_init_table, t_whitebox, t_delete_all_objects,
     t_insert_list, t_test_ms, t_select_delete, t_ets_dets,
     memory, t_select_reverse, t_bucket_disappears,
     select_fail, t_insert_new, t_repair_continuation,
     otp_5340, otp_6338, otp_6842_select_1000, otp_7665,
     otp_8732, meta_wb, grow_shrink, grow_pseudo_deleted,
     shrink_pseudo_deleted, {group, meta_smp}, smp_insert,
     smp_fixed_delete, smp_unfix_fix, smp_select_delete,
     otp_8166, exit_large_table_owner,
     exit_many_large_table_owner, exit_many_tables_owner,
     exit_many_many_tables_owner, write_concurrency, heir,
     give_away, setopts, bad_table, types,
     otp_10182,
     otp_9932,
     otp_9423,
     
     memory_check_summary]. % MUST BE LAST

groups() -> 
    [{new, [],
      [default, setbag, badnew, verybadnew, named, keypos2,
       privacy]},
     {insert, [], [empty, badinsert]},
     {lookup, [], [time_lookup, badlookup, lookup_order]},
     {lookup_element, [], [lookup_element_mult]},
     {delete, [],
      [delete_elem, delete_tab, delete_large_tab,
       delete_large_named_table, evil_delete, table_leak,
       baddelete, match_delete, match_delete3]},
     {match, [],
      [match1, match2, match_object, match_object2]},
     {misc, [],
      [misc1, safe_fixtable, info, dups, tab2list]},
     {files, [],
      [tab2file, tab2file2, tabfile_ext1,
       tabfile_ext2, tabfile_ext3, tabfile_ext4]},
     {heavy, [],
      [heavy_lookup, heavy_lookup_element, heavy_concurrent]},
     {fold, [],
      [foldl_ordered, foldr_ordered, foldl, foldr,
       fold_empty]},
     {meta_smp, [],
      [meta_lookup_unnamed_read, meta_lookup_unnamed_write,
       meta_lookup_named_read, meta_lookup_named_write,
       meta_newdel_unnamed, meta_newdel_named]}].

init_per_suite(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    Config.

end_per_suite(_Config) ->
    stop_spawn_logger(),
    catch erts_debug:set_internal_state(available_internal_state, false),
    ok.

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.

%% Test that we did not have "too many" failed verify_etsmem()'s
%% in the test suite.
%% verify_etsmem() may give a low number of false positives
%% as concurrent activities, such as lingering processes
%% from earlier test suites, may do unrelated ets (de)allocations.
memory_check_summary(_Config) ->
    case whereis(ets_test_spawn_logger) of
	undefined ->
	    ?t:fail("No spawn logger exist");
	_ ->
	    ets_test_spawn_logger ! {self(), get_failed_memchecks},
	    receive {get_failed_memchecks, FailedMemchecks} -> ok end,
	    io:format("Failed memchecks: ~p\n",[FailedMemchecks]),
	    if FailedMemchecks > 3 ->
		    ct:fail("Too many failed (~p) memchecks", [FailedMemchecks]);
	       true ->
		    ok
	    end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t_bucket_disappears(suite) ->
    [];
t_bucket_disappears(doc) ->
    ["Test that a disappearing bucket during select of a non-fixed table works."];
t_bucket_disappears(Config) when is_list(Config) ->
    repeat_for_opts(t_bucket_disappears_do).

t_bucket_disappears_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line ets_new(abcd, [named_table, public, {keypos, 2} | Opts]),
    ?line ets:insert(abcd, {abcd,1,2}),
    ?line ets:insert(abcd, {abcd,2,2}),
    ?line ets:insert(abcd, {abcd,3,2}),
    ?line {_, Cont} = ets:select(abcd, [{{'_', '$1', '_'}, 
					 [{'<', '$1', {const, 10}}], 
					 ['$1']}], 1),
    ?line ets:delete(abcd, 2),
    ?line ets:select(Cont),
    ?line true = ets:delete(abcd),
    ?line verify_etsmem(EtsMem).
    

t_match_spec_run(suite) -> 
    [];
t_match_spec_run(doc) ->
    ["Check ets:match_spec_run/2."];
t_match_spec_run(Config) when is_list(Config) ->
    init_externals(),
    ?line EtsMem = etsmem(),

    t_match_spec_run_test([{1},{2},{3}],
			  [{{'$1'},[{'>','$1',1}],['$1']}],
			  [2,3]),

    ?line Huge = [{X} || X <- lists:seq(1,2500)],
    ?line L = lists:seq(2476,2500),
    t_match_spec_run_test(Huge, [{{'$1'},[{'>','$1',2475}],['$1']}], L),

    ?line L2 = [{X*16#FFFFFFF} || X <- L],
    t_match_spec_run_test(Huge,
			  [{{'$1'}, [{'>','$1',2475}], [{{{'*','$1',16#FFFFFFF}}}]}],
			  L2),

    t_match_spec_run_test(Huge, [{{'$1'}, [{'=:=',{'rem','$1',500},0}], ['$1']}],
			  [500,1000,1500,2000,2500]),

    %% More matching fun with several match clauses and guards,
    %% applied to a variety of terms.
    Fun = fun(Term) ->
		  CTerm = {const, Term},

		  N_List = [{Term, "0", "v-element"},
			    {"=hidden_node", "0", Term},
			    {"0", Term, Term},
			    {"something", Term, "something else"},
			    {"guard and res", Term, 872346},
			    {Term, {'and',Term,'again'}, 3.14},
			    {Term, {'and',Term,'again'}, "m&g"},
			    {Term, {'and',Term,'again'}, "m&g&r"},
			    {[{second,Term}, 'and', "tail"], Term, ['and',"tail"]}],

		  N_MS = [{{'$1','$2','$3'},
			   [{'=:=','$1',CTerm}, {'=:=','$2',{const,"0"}}],
			   [{{"Guard only for $1",'$3'}}]},

			  {{'$3','$1','$4'},
			   [{'=:=','$3',"=hidden_node"}, {'=:=','$1',{const,"0"}}],
			   [{{"Result only for $4",'$4'}}]},

			  {{'$2','$1','$1'},
			   [{'=:=','$2',{const,"0"}}],
			   [{{"Match only for $1",'$2'}}]},

			  {{'$2',Term,['$3'|'_']},
			   [{is_list,'$2'},{'=:=','$3',$s}],
			   [{{"Matching term",'$2'}}]},

			  {{'$1','$2',872346},
			   [{'=:=','$2',CTerm}, {is_list,'$1'}],
			   [{{"Guard and result",'$2'}}]},

			  {{'$1', {'and','$1','again'}, '$2'},
			   [{is_float,'$2'}],
			   [{{"Match and result",'$1'}}]},

			  {{'$1', {'and','$1','again'}, '$2'},
			   [{'=:=','$1',CTerm}, {'=:=', '$2', "m&g"}],
			   [{{"Match and guard",'$2'}}]},

			  {{'$1', {'and','$1','again'}, "m&g&r"},
			   [{'=:=','$1',CTerm}],
			   [{{"Match, guard and result",'$1'}}]},

			  {{'$1', '$2', '$3'},
			   [{'=:=','$1',[{{second,'$2'}} | '$3']}],
			   [{{"Building guard"}}]}
			 ],

		  N_Result = [{"Guard only for $1", "v-element"},
			      {"Result only for $4", Term},
			      {"Match only for $1", "0"},
			      {"Matching term","something"},
			      {"Guard and result",Term},
			      {"Match and result",Term},
			      {"Match and guard","m&g"},
			      {"Match, guard and result",Term},
			      {"Building guard"}],

		  F = fun(N_MS_Perm) ->
			      t_match_spec_run_test(N_List, N_MS_Perm, N_Result)
		      end,
		  repeat_for_permutations(F, N_MS)
	  end,
    test_terms(Fun, skip_refc_check),

    ?line verify_etsmem(EtsMem).

t_match_spec_run_test(List, MS, Result) ->

    %%io:format("ms = ~p\n",[MS]),

    ?m(Result, ets:match_spec_run(List, ets:match_spec_compile(MS))),

    %% Check that ets:select agree
    Tab = ets:new(xxx, [bag]),
    ets:insert(Tab, List),
    SRes = lists:sort(Result),
    ?m(SRes, lists:sort(ets:select(Tab, MS))),
    ets:delete(Tab),

    %% Check that tracing agree
    Self = self(),
    {Tracee, MonRef} = my_spawn_monitor(fun() -> ms_tracee(Self, List) end),
    receive {Tracee, ready} -> ok end,

    MST = lists:map(fun(Clause) -> ms_clause_ets_to_trace(Clause) end, MS),

    %%io:format("MS = ~p\nMST= ~p\n",[MS,MST]),

    erlang:trace_pattern({?MODULE,ms_tracee_dummy,'_'}, MST , [local]),
    erlang:trace(Tracee, true, [call]),
    Tracee ! start,
    TRes = ms_tracer_collect(Tracee, MonRef, []),
    %erlang:trace(Tracee, false, [call]),
    %Tracee ! stop,
    case TRes of
	SRes -> ok;
	_ ->
	    io:format("TRACE MATCH FAILED\n"),
	    io:format("Input = ~p\nMST = ~p\nExpected = ~p\nGot = ~p\n", [List, MST, SRes, TRes]),
	    ?t:fail("TRACE MATCH FAILED")
    end,
    ok.



ms_tracer_collect(Tracee, Ref, Acc) ->
    receive
	{trace, Tracee, call, _Args, [Msg]} ->
	    %io:format("trace Args=~p  Msg=~p\n", [_Args, Msg]),
	    ms_tracer_collect(Tracee, Ref, [Msg | Acc]);

	{'DOWN', Ref, process, Tracee, _} ->
	    %io:format("monitor DOWN for ~p\n", [Tracee]),
	    TDRef = erlang:trace_delivered(Tracee),
	    ms_tracer_collect(Tracee, TDRef, Acc);

	{trace_delivered, Tracee, Ref} ->
	    %%io:format("trace delivered for ~p\n", [Tracee]),
	    lists:sort(Acc);

	Other ->
	    io:format("Unexpected message = ~p\n", [Other]),
	    ?t:fail("Unexpected tracer msg")
    end.


ms_tracee(Parent, CallArgList) ->
    %io:format("ms_tracee ~p started with ArgList = ~p\n", [self(), CallArgList]),
    Parent ! {self(), ready},
    receive start -> ok end,
    lists:foreach(fun(Args) ->
			  erlang:apply(?MODULE, ms_tracee_dummy, tuple_to_list(Args))
		  end, CallArgList).
    %%receive stop -> ok end.



ms_tracee_dummy(_) -> ok.
ms_tracee_dummy(_,_) -> ok.
ms_tracee_dummy(_,_,_) -> ok.
ms_tracee_dummy(_,_,_,_) -> ok.

ms_clause_ets_to_trace({Head, Guard, Body}) ->
    {tuple_to_list(Head), Guard, [{message, Body}]}.

assert_eq(A,A) -> ok;
assert_eq(A,B) ->
    io:format("FAILED MATCH:\n~p\n =/=\n~p\n",[A,B]),
    ?t:fail("assert_eq failed").


t_repair_continuation(suite) -> 
    [];
t_repair_continuation(doc) ->
    ["Check ets:repair_continuation/2."];
t_repair_continuation(Config) when is_list(Config) ->
    repeat_for_opts(t_repair_continuation_do).


t_repair_continuation_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line MS = [{'_',[],[true]}],
    ?line MS2 = [{{{'$1','_'},'_'},[],['$1']}],
    (fun() ->
	     ?line T = ets_new(x,[ordered_set|Opts]),
	     ?line F = fun(0,_)->ok;(N,F) -> ets:insert(T,{N,N}), F(N-1,F) end,
	     ?line F(1000,F),
	     ?line {_,C} = ets:select(T,MS,5),
	     ?line C2 = erlang:setelement(5,C,<<>>),
	     ?line {'EXIT',{badarg,_}} = (catch ets:select(C2)),
	     ?line C3 = ets:repair_continuation(C2,MS),
	     ?line {[true,true,true,true,true],_} = ets:select(C3),
	     ?line {[true,true,true,true,true],_} = ets:select(C),
	     ?line true = ets:delete(T)
     end)(),
    (fun() ->
	     ?line T = ets_new(x,[ordered_set|Opts]),
	     ?line F = fun(0,_)->ok;(N,F) -> ets:insert(T,{N,N}), F(N-1,F) end,
	     ?line F(1000,F),
	     ?line {_,C} = ets:select(T,MS,1001),
	     ?line C = '$end_of_table',
	     ?line C3 = ets:repair_continuation(C,MS),
	     ?line '$end_of_table' = ets:select(C3),
	     ?line '$end_of_table' = ets:select(C),
	     ?line true = ets:delete(T)
     end)(),
    
    (fun() ->
	     ?line T = ets_new(x,[ordered_set|Opts]),
	     ?line F = fun(0,_)->ok;(N,F) -> 
			       ets:insert(T,{integer_to_list(N),N}), 
			       F(N-1,F) 
		       end,
	     ?line F(1000,F),
	     ?line {_,C} = ets:select(T,MS,5),
	     ?line C2 = erlang:setelement(5,C,<<>>),
	     ?line {'EXIT',{badarg,_}} = (catch ets:select(C2)),
	     ?line C3 = ets:repair_continuation(C2,MS),
	     ?line {[true,true,true,true,true],_} = ets:select(C3),
	     ?line {[true,true,true,true,true],_} = ets:select(C),
	     ?line true = ets:delete(T)
     end)(),
    (fun() ->
	     ?line T = ets_new(x,[ordered_set|Opts]),
	     ?line F = fun(0,_)->ok;(N,F) -> 
			       ets:insert(T,{{integer_to_list(N),N},N}), 
			       F(N-1,F) 
		       end,
	     ?line F(1000,F),
	     ?line {_,C} = ets:select(T,MS2,5),
	     ?line C2 = erlang:setelement(5,C,<<>>),
	     ?line {'EXIT',{badarg,_}} = (catch ets:select(C2)),
	     ?line C3 = ets:repair_continuation(C2,MS2),
	     ?line {[_,_,_,_,_],_} = ets:select(C3),
	     ?line {[_,_,_,_,_],_} = ets:select(C),
	     ?line true = ets:delete(T)
     end)(),
    
    (fun() ->
	     ?line T = ets_new(x,[set|Opts]),
	     ?line F = fun(0,_)->ok;(N,F) -> 
			       ets:insert(T,{N,N}), 
			       F(N-1,F) 
		       end,
	     ?line F(1000,F),
	     ?line {_,C} = ets:select(T,MS,5),
	     ?line C2 = erlang:setelement(4,C,<<>>),
	     ?line {'EXIT',{badarg,_}} = (catch ets:select(C2)),
	     ?line C3 = ets:repair_continuation(C2,MS),
	     ?line {[true,true,true,true,true],_} = ets:select(C3),
	     ?line {[true,true,true,true,true],_} = ets:select(C),
	     ?line true = ets:delete(T)
     end)(),
    (fun() ->
	     ?line T = ets_new(x,[set|Opts]),
	     ?line F = fun(0,_)->ok;(N,F) -> 
			       ets:insert(T,{integer_to_list(N),N}), 
			       F(N-1,F) 
		       end,
	     ?line F(1000,F),
	     ?line {_,C} = ets:select(T,MS,5),
	     ?line C2 = erlang:setelement(4,C,<<>>),
	     ?line {'EXIT',{badarg,_}} = (catch ets:select(C2)),
	     ?line C3 = ets:repair_continuation(C2,MS),
	     ?line {[true,true,true,true,true],_} = ets:select(C3),
	     ?line {[true,true,true,true,true],_} = ets:select(C),
	     ?line true = ets:delete(T)
     end)(),
    (fun() ->
	     ?line T = ets_new(x,[bag|Opts]),
	     ?line F = fun(0,_)->ok;(N,F) -> 
			       ets:insert(T,{integer_to_list(N),N}), 
			       F(N-1,F) 
		       end,
	     ?line F(1000,F),
	     ?line {_,C} = ets:select(T,MS,5),
	     ?line C2 = erlang:setelement(4,C,<<>>),
	     ?line {'EXIT',{badarg,_}} = (catch ets:select(C2)),
	     ?line C3 = ets:repair_continuation(C2,MS),
	     ?line {[true,true,true,true,true],_} = ets:select(C3),
	     ?line {[true,true,true,true,true],_} = ets:select(C),
	     ?line true = ets:delete(T)
     end)(),
    (fun() ->
	     ?line T = ets_new(x,[duplicate_bag|Opts]),
	     ?line F = fun(0,_)->ok;(N,F) -> 
			       ets:insert(T,{integer_to_list(N),N}), 
			       F(N-1,F) 
		       end,
	     ?line F(1000,F),
	     ?line {_,C} = ets:select(T,MS,5),
	     ?line C2 = erlang:setelement(4,C,<<>>),
	     ?line {'EXIT',{badarg,_}} = (catch ets:select(C2)),
	     ?line C3 = ets:repair_continuation(C2,MS),
	     ?line {[true,true,true,true,true],_} = ets:select(C3),
	     ?line {[true,true,true,true,true],_} = ets:select(C),
	     ?line true = ets:delete(T)
     end)(),
    ?line false = ets:is_compiled_ms(<<>>),
    ?line true = ets:is_compiled_ms(ets:match_spec_compile(MS)),
    ?line verify_etsmem(EtsMem).


default(doc) ->
    ["Check correct default vaules of a new ets table"];
default(suite) -> [];
default(Config) when is_list(Config) ->
    %% Default should be set,protected
    ?line EtsMem = etsmem(),
    ?line Def = ets_new(def,[]),
    ?line set = ets:info(Def,type),
    ?line protected = ets:info(Def,protection),
    Compressed = erlang:system_info(ets_always_compress),
    ?line Compressed = ets:info(Def,compressed),
    Self = self(),
    ?line Self = ets:info(Def,owner),
    ?line none = ets:info(Def, heir),
    ?line false = ets:info(Def,named_table),
    ?line ets:delete(Def),
    ?line verify_etsmem(EtsMem).

select_fail(doc) ->
    ["Test that select fails even if nothing can match"];
select_fail(suite) ->
    [];
select_fail(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    repeat_for_opts(select_fail_do, [all_types,write_concurrency]),
    ?line verify_etsmem(EtsMem).

select_fail_do(Opts) ->
    ?line T = ets_new(x,Opts),
    ?line ets:insert(T,{a,a}),
    ?line case (catch 
		    ets:select(T,[{{a,'_'},[],[{snuffla}]}])) of
	      {'EXIT',{badarg,_}} ->
		  ok;
	      Else0 ->
		  exit({type,ets:info(T,type),
			expected,'EXIT',got,Else0})
	  end,
    ?line case (catch 
		    ets:select(T,[{{b,'_'},[],[{snuffla}]}])) of
	      {'EXIT',{badarg,_}} ->
		  ok;
	      Else1 ->
		  exit({type,ets:info(T,type),
			expected,'EXIT',got,Else1})
	  end,
    ?line ets:delete(T).
 

-define(S(T),ets:info(T,memory)).
-define(TAB_STRUCT_SZ, erts_debug:get_internal_state('DbTable_words')).
%%-define(NORMAL_TAB_STRUCT_SZ, 26). %% SunOS5.8, 32-bit, non smp, private heap
%%
%% The hardcoded expected memory sizes (in words) are the ones we expect on:
%%   SunOS5.8, 32-bit, non smp, private heap
%%
memory(doc) -> ["Whitebox test of ets:info(X,memory)"];
memory(suite) -> [];
memory(Config) when is_list(Config) ->
    ?line ok = chk_normal_tab_struct_size(),
    repeat_for_opts(memory_do,[compressed]),
    ?line catch erts_debug:set_internal_state(available_internal_state, false).

memory_do(Opts) ->
    ?line L = [T1,T2,T3,T4] = fill_sets_int(1000,Opts),
    XR1 = case mem_mode(T1) of
	    {normal,_} ->     {13836,13046,13046,13052}; %{13862,13072,13072,13078};
	    {compressed,4} -> {11041,10251,10251,10252}; %{11067,10277,10277,10278};
	    {compressed,8} -> {10050,9260,9260,9260} %{10076,9286,9286,9286}
	  end,
    ?line XRes1 = adjust_xmem(L, XR1),
    ?line Res1 = {?S(T1),?S(T2),?S(T3),?S(T4)},
    ?line lists:foreach(fun(T) ->
 				Before = ets:info(T,size),
 				Key = 2, %894, %%ets:first(T),
 				Objs = ets:lookup(T,Key),
				?line ets:delete(T,Key),
				io:format("deleted key ~p from ~p changed size ~p to ~p: ~p\n",
					  [Key, ets:info(T,type), Before, ets:info(T,size), Objs])
		  end,
		  L),
    XR2 = case mem_mode(T1) of
	    {normal,_} ->     {13826,13037,13028,13034}; %{13852,13063,13054,13060};
	    {compressed,4} -> {11031,10242,10233,10234}; %{11057,10268,10259,10260};
	    {compressed,8} -> {10040,9251,9242,9242}     %10066,9277,9268,9268}
	  end,
    ?line XRes2 = adjust_xmem(L, XR2),
    ?line Res2 = {?S(T1),?S(T2),?S(T3),?S(T4)},
    ?line lists:foreach(fun(T) ->
 				Before = ets:info(T,size),
 				Key = 4, %802, %ets:first(T),
 				Objs = ets:lookup(T,Key),
 				?line ets:match_delete(T,{Key,'_'}),
				io:format("match_deleted key ~p from ~p changed size ~p to ~p: ~p\n",
					  [Key, ets:info(T,type), Before, ets:info(T,size), Objs])
			end,
			L),
    XR3 = case mem_mode(T1) of
		{normal,_} ->     {13816,13028,13010,13016}; %{13842,13054,13036,13042};
		{compressed,4} -> {11021,10233,10215,10216}; %{11047,10259,10241,10242};
		{compressed,8} -> {10030,9242,9224,9224} %{10056,9268,9250,9250}
	  end,
    ?line XRes3 = adjust_xmem(L, XR3),
    ?line Res3 = {?S(T1),?S(T2),?S(T3),?S(T4)},
    ?line lists:foreach(fun(T) ->
			  ?line ets:delete_all_objects(T)
		  end,
		  L),
    ?line XRes4 = adjust_xmem(L, {50,260,260,260}), %{76,286,286,286}),
    ?line Res4 = {?S(T1),?S(T2),?S(T3),?S(T4)},
    lists:foreach(fun(T) ->
			  ?line ets:delete(T)
		  end,
		  L),
    ?line L2 =  [T11,T12,T13,T14] = fill_sets_int(1000),
    ?line lists:foreach(fun(T) ->
			  ?line ets:select_delete(T,[{'_',[],[true]}])
		  end,
		  L2),
    ?line XRes5 = adjust_xmem(L2, {50,260,260,260}), %{76,286,286,286}),
    ?line Res5 = {?S(T11),?S(T12),?S(T13),?S(T14)},
    ?line io:format("XRes1 = ~p~n"
		    " Res1 = ~p~n~n"
		    "XRes2 = ~p~n"
		    " Res2 = ~p~n~n"
		    "XRes3 = ~p~n"
		    " Res3 = ~p~n~n"
		    "XRes4 = ~p~n"
		    " Res4 = ~p~n~n"
		    "XRes5 = ~p~n"
		    " Res5 = ~p~n~n",
		    [XRes1, Res1,
		     XRes2, Res2,
		     XRes3, Res3,
		     XRes4, Res4,
		     XRes5, Res5]),
    ?line XRes1 = Res1,
    ?line XRes2 = Res2,
    ?line XRes3 = Res3,
    ?line XRes4 = Res4,
    ?line XRes5 = Res5,
    ?line ok.

mem_mode(T) ->
    {case ets:info(T,compressed) of
	true -> compressed;
	false -> normal
     end,
     erlang:system_info(wordsize)}.

chk_normal_tab_struct_size() ->
    ?line System = {os:type(),
		    os:version(),
		    erlang:system_info(wordsize),
		    erlang:system_info(smp_support),
		    erlang:system_info(heap_type)},
    ?line ?t:format("System = ~p~n", [System]),
    %%?line ?t:format("?NORMAL_TAB_STRUCT_SZ=~p~n", [?NORMAL_TAB_STRUCT_SZ]),
    ?line ?t:format("?TAB_STRUCT_SZ=~p~n", [?TAB_STRUCT_SZ]),
    ok.
%   ?line case System of
%             {{unix, sunos}, {5, 8, 0}, 4, false, private} ->
%       	  ?line ?NORMAL_TAB_STRUCT_SZ = ?TAB_STRUCT_SZ,
%       	  ?line ok;
%             _ ->
%       	  ?line ok
%         end.

-define(DB_TREE_STACK_NEED,50). % The static stack for a tree, in halfword pointers are two internal words
                                % so the stack gets twice as big
-define(DB_HASH_SIZEOF_EXTSEG,260). % The segment size in words, in halfword this will be twice as large.

adjust_xmem([T1,T2,T3,T4], {A0,B0,C0,D0} = _Mem0) ->
    %% Adjust for 64-bit, smp, and os:
    %%   Table struct size may differ.

%   Mem1 = case ?TAB_STRUCT_SZ of
%              ?NORMAL_TAB_STRUCT_SZ ->
%       	   Mem0;
%              TabStructSz ->
%       	   TabDiff = TabStructSz - ?NORMAL_TAB_STRUCT_SZ,
%       	   {A0+TabDiff, B0+TabDiff, C0+TabDiff, D0+TabDiff}
%          end,

    TabDiff = ?TAB_STRUCT_SZ,
    Mem1 = {A0+TabDiff, B0+TabDiff, C0+TabDiff, D0+TabDiff},

    case {erlang:system_info({wordsize,internal}),erlang:system_info({wordsize,external})} of
	%% Halfword, corrections for regular pointers occupying two internal words.
	{4,8} ->
	    {A1,B1,C1,D1} = Mem1,
	    {A1+4*ets:info(T1, size)+?DB_TREE_STACK_NEED,
	     B1+3*ets:info(T2, size)+?DB_HASH_SIZEOF_EXTSEG,
	     C1+3*ets:info(T3, size)+?DB_HASH_SIZEOF_EXTSEG,
	     D1+3*ets:info(T4, size)+?DB_HASH_SIZEOF_EXTSEG};
	_ ->
	    Mem1
    end.

t_whitebox(doc) ->
    ["Diverse whitebox testes"];
t_whitebox(suite) ->
    [];
t_whitebox(Config) when is_list(Config) ->    
    ?line EtsMem = etsmem(),
    repeat_for_opts(whitebox_1),
    repeat_for_opts(whitebox_1),
    repeat_for_opts(whitebox_1),
    repeat_for_opts(whitebox_2),
    repeat_for_opts(whitebox_2),
    repeat_for_opts(whitebox_2),
    ?line verify_etsmem(EtsMem).

whitebox_1(Opts) ->
    ?line T=ets_new(x,[bag | Opts]),
    ?line ets:insert(T,[{du,glade},{ta,en}]),
    ?line ets:insert(T,[{hej,hopp2},{du,glade2},{ta,en2}]),
    ?line {_,C}=ets:match(T,{ta,'$1'},1),
    ?line ets:select(C),
    ?line ets:match(C),
    ?line ets:delete(T),
    ok.

whitebox_2(Opts) ->
    ?line T=ets_new(x,[ordered_set, {keypos,2} | Opts]),
    ?line T2=ets_new(x,[set, {keypos,2}| Opts]),
    ?line 0 = ets:select_delete(T,[{{hej},[],[true]}]),
    ?line 0 = ets:select_delete(T,[{{hej,hopp},[],[true]}]),
    ?line 0 = ets:select_delete(T2,[{{hej},[],[true]}]),
    ?line 0 = ets:select_delete(T2,[{{hej,hopp},[],[true]}]),
    ?line ets:delete(T),
    ?line ets:delete(T2),
    ok.
    
    
t_ets_dets(doc) ->
    ["Test ets:to/from_dets"];
t_ets_dets(suite) ->
    [];
t_ets_dets(Config) when is_list(Config) ->
    repeat_for_opts(fun(Opts) -> t_ets_dets(Config,Opts) end).

t_ets_dets(Config, Opts) ->
    ?line Fname = gen_dets_filename(Config,1),
    ?line (catch file:delete(Fname)),
    ?line {ok,DTab} = dets:open_file(testdets_1,
			       [{file, Fname}]),
    ?line ETab = ets_new(x,Opts),
    ?line filltabint(ETab,3000),
    ?line DTab = ets:to_dets(ETab,DTab),
    ?line ets:delete_all_objects(ETab),
    ?line 0 = ets:info(ETab,size),
    ?line true = ets:from_dets(ETab,DTab),
    ?line 3000 = ets:info(ETab,size),
    ?line ets:delete(ETab),
    ?line check_badarg(catch ets:to_dets(ETab,DTab),
		       ets, to_dets, [ETab,DTab]),
    ?line check_badarg(catch ets:from_dets(ETab,DTab),
		       ets, from_dets, [ETab,DTab]),
    ?line ETab2 = ets_new(x,Opts),
    ?line filltabint(ETab2,3000),
    ?line dets:close(DTab),
    ?line check_badarg(catch ets:to_dets(ETab2,DTab),
		       ets, to_dets, [ETab2,DTab]),
    ?line check_badarg(catch ets:from_dets(ETab2,DTab),
		       ets, from_dets, [ETab2,DTab]),
    ?line ets:delete(ETab2),
    ?line (catch file:delete(Fname)),
    ok.

check_badarg({'EXIT', {badarg, [{M,F,Args,_} | _]}}, M, F, Args) ->
    true;
check_badarg({'EXIT', {badarg, [{M,F,A,_} | _]}}, M, F, Args)  ->
    true = test_server:is_native(M) andalso length(Args) =:= A.

t_delete_all_objects(doc) ->
    ["Test ets:delete_all_objects/1"];
t_delete_all_objects(suite) ->
    [];
t_delete_all_objects(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    repeat_for_opts(t_delete_all_objects_do),
    ?line verify_etsmem(EtsMem).

get_kept_objects(T) ->
    case ets:info(T,stats) of
	false ->
	    0;
	{_,_,_,_,_,_,KO}  ->
	    KO
    end.

t_delete_all_objects_do(Opts) ->
    ?line T=ets_new(x,Opts),
    ?line filltabint(T,4000),
    ?line O=ets:first(T),
    ?line ets:next(T,O),
    ?line ets:safe_fixtable(T,true),
    ?line true = ets:delete_all_objects(T),
    ?line '$end_of_table' = ets:next(T,O),
    ?line 0 = ets:info(T,size),
    ?line 4000 = get_kept_objects(T),
    ?line ets:safe_fixtable(T,false),
    ?line 0 = ets:info(T,size),
    ?line 0 = get_kept_objects(T),
    ?line filltabint(T,4000),
    ?line 4000 = ets:info(T,size),
    ?line true = ets:delete_all_objects(T),
    ?line 0 = ets:info(T,size),
    ?line ets:delete(T).


t_delete_object(doc) ->
    ["Test ets:delete_object/2"];
t_delete_object(suite) ->
    [];
t_delete_object(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    repeat_for_opts(t_delete_object_do),
    ?line verify_etsmem(EtsMem).

t_delete_object_do(Opts) ->
    ?line T = ets_new(x,Opts),
    ?line filltabint(T,4000),
    ?line del_one_by_one_set(T,1,4001),
    ?line filltabint(T,4000),
    ?line del_one_by_one_set(T,4000,0),
    ?line filltabint(T,4000),
    ?line First = ets:first(T),
    ?line Next = ets:next(T,First),
    ?line ets:safe_fixtable(T,true),
    ?line ets:delete_object(T,{First, integer_to_list(First)}),
    ?line Next = ets:next(T,First),
    ?line 3999 = ets:info(T,size),
    ?line 1 = get_kept_objects(T),
    ?line ets:safe_fixtable(T,false),
    ?line 3999 = ets:info(T,size),
    ?line 0 = get_kept_objects(T),
    ?line ets:delete(T),
    ?line T1 = ets_new(x,[ordered_set | Opts]),
    ?line filltabint(T1,4000),
    ?line del_one_by_one_set(T1,1,4001),
    ?line filltabint(T1,4000),
    ?line del_one_by_one_set(T1,4000,0),
    ?line ets:delete(T1),
    ?line T2 = ets_new(x,[bag | Opts]),
    ?line filltabint2(T2,4000),
    ?line del_one_by_one_bag(T2,1,4001),
    ?line filltabint2(T2,4000),
    ?line del_one_by_one_bag(T2,4000,0),
    ?line ets:delete(T2),
    ?line T3 = ets_new(x,[duplicate_bag | Opts]),
    ?line filltabint3(T3,4000),
    ?line del_one_by_one_dbag_1(T3,1,4001),
    ?line filltabint3(T3,4000),
    ?line del_one_by_one_dbag_1(T3,4000,0),
    ?line filltabint(T3,4000),
    ?line filltabint3(T3,4000),
    ?line del_one_by_one_dbag_2(T3,1,4001),
    ?line filltabint(T3,4000),
    ?line filltabint3(T3,4000),
    ?line del_one_by_one_dbag_2(T3,4000,0),

    ?line filltabint2(T3,4000),
    ?line filltabint(T3,4000),
    ?line del_one_by_one_dbag_3(T3,4000,0),
    ?line ets:delete(T3),    
    ok.

make_init_fun(N) when N > 4000->
    fun(read) ->
	    end_of_input;
       (close) ->
	    exit(close_not_expected)
    end;
make_init_fun(N) ->
    fun(read) ->
	    case N rem 2 of
		0 ->
		    {[{N, integer_to_list(N)}, {N, integer_to_list(N)}],
		     make_init_fun(N + 1)};
		1 ->
		    {[], make_init_fun(N + 1)}
	    end;
       (close) ->
	    exit(close_not_expected)
    end.

t_init_table(doc) ->
    ["Test ets:init_table/2"];
t_init_table(suite) ->
    [];
t_init_table(Config) when is_list(Config)->
    ?line EtsMem = etsmem(),
    repeat_for_opts(t_init_table_do),
    ?line verify_etsmem(EtsMem).

t_init_table_do(Opts) ->
    ?line T = ets_new(x,[duplicate_bag | Opts]),
    ?line filltabint(T,4000),
    ?line ets:init_table(T, make_init_fun(1)),
    ?line del_one_by_one_dbag_1(T,4000,0),
    ?line ets:delete(T),
    ok.

do_fill_dbag_using_lists(T,0) ->
    T;
do_fill_dbag_using_lists(T,N) ->
    ets:insert(T,[{N,integer_to_list(N)},
		  {N + N rem 2,integer_to_list(N + N rem 2)}]),
    do_fill_dbag_using_lists(T,N - 1). 


t_insert_new(doc) ->
    ["Test the insert_new function"];
t_insert_new(suite) ->
    [];
t_insert_new(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    ?line L = fill_sets_int(1000) ++ fill_sets_int(1000,[{write_concurrency,true}]),
    lists:foreach(fun(Tab) ->
			  ?line false = ets:insert_new(Tab,{2,"2"}),
			  ?line true = ets:insert_new(Tab,{2002,"2002"}),
			  ?line false = ets:insert_new(Tab,{2002,"2002"}),
			  ?line true = ets:insert(Tab,{2002,"2002"}),
			  ?line false =  ets:insert_new(Tab,[{2002,"2002"}]),
			  ?line false =  ets:insert_new(Tab,[{2002,"2002"},
						       {2003,"2003"}]),
			  ?line false =  ets:insert_new(Tab,[{2001,"2001"},
						       {2002,"2002"},
						       {2003,"2003"}]),
			  ?line false =  ets:insert_new(Tab,[{2001,"2001"},
						       {2002,"2002"}]),
			  ?line true =  ets:insert_new(Tab,[{2001,"2001"},
						      {2003,"2003"}]),
			  ?line false = ets:insert_new(Tab,{2001,"2001"}),
			  ?line false = ets:insert_new(Tab,{2002,"2002"}),
			  ?line false = ets:insert_new(Tab,{2003,"2003"}),
			  ?line true = ets:insert_new(Tab,{2004,"2004"}),
			  ?line true = ets:insert_new(Tab,{2000,"2000"}),
			  ?line true = ets:insert_new(Tab,[{2005,"2005"},
							   {2006,"2006"},
							   {2007,"2007"}]),
			  ?line Num = 
			      case ets:info(Tab,type) of
				  bag ->
				      ?line true = 
					  ets:insert(Tab,{2004,"2004-2"}),
				      ?line false = 
					  ets:insert_new(Tab,{2004,"2004-3"}),
				      1009;
				  duplicate_bag ->
				      ?line true = 
					  ets:insert(Tab,{2004,"2004"}),
				      ?line false = 
					  ets:insert_new(Tab,{2004,"2004"}),
				      1010;
				  _ ->
				      1008
			      end,
			  ?line Num = ets:info(Tab,size),
			  ?line List = ets:tab2list(Tab),
			  ?line ets:delete_all_objects(Tab),
			  ?line true = ets:insert_new(Tab,List),
			  ?line false = ets:insert_new(Tab,List),
			  ?line ets:delete(Tab)
		  end,
		  L),
    ?line verify_etsmem(EtsMem).

t_insert_list(doc) ->
    ["Test ets:insert/2 with list of objects."];
t_insert_list(suite) ->
    [];
t_insert_list(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    repeat_for_opts(t_insert_list_do),
    ?line verify_etsmem(EtsMem).

t_insert_list_do(Opts) ->
    ?line T = ets_new(x,[duplicate_bag | Opts]),
    ?line do_fill_dbag_using_lists(T,4000),
    ?line del_one_by_one_dbag_2(T,4000,0),
    ?line ets:delete(T).


t_test_ms(doc) ->
    ["Test interface of ets:test_ms/2"];
t_test_ms(suite) ->
    [];
t_test_ms(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    ?line {ok,[a,b]} = ets:test_ms({a,b},
				   [{{'$1','$2'},[{'<','$1','$2'}],['$$']}]),
    ?line {ok,false} = ets:test_ms({a,b},
				   [{{'$1','$2'},[{'>','$1','$2'}],['$$']}]),
    Tpl = {a,gb_sets:new()},
    ?line {ok,Tpl} = ets:test_ms(Tpl, [{{'_','_'},  [], ['$_']}]), % OTP-10190
    ?line {error,[{error,String}]} = ets:test_ms({a,b},
						 [{{'$1','$2'},
						   [{'flurp','$1','$2'}],
						   ['$$']}]),
    ?line true = (if is_list(String) -> true; true -> false end),
    ?line verify_etsmem(EtsMem).

t_select_reverse(doc) ->
    ["Test the select reverse BIF's"];
t_select_reverse(suite) ->
    [];
t_select_reverse(Config) when is_list(Config) ->
    ?line Table = ets_new(xxx, [ordered_set]),
    ?line filltabint(Table,1000),
    ?line A = lists:reverse(ets:select(Table,[{{'$1', '_'},
					 [{'>',
					   {'rem',
					    '$1', 5},
					   2}],
					 ['$_']}])),
    ?line A = ets:select_reverse(Table,[{{'$1', '_'},
				   [{'>',
				     {'rem',
				      '$1', 5},
				     2}],
				   ['$_']}]),
    ?line A = reverse_chunked(Table,[{{'$1', '_'},
				   [{'>',
				     {'rem',
				      '$1', 5},
				     2}],
				   ['$_']}],3),
    % A set/bag/duplicate_bag should get the same result regardless
    % of select or select_reverse
    ?line Table2 = ets_new(xxx, [set]),
    ?line filltabint(Table2,1000),
    ?line Table3 = ets_new(xxx, [bag]),
    ?line filltabint(Table3,1000),
    ?line Table4 = ets_new(xxx, [duplicate_bag]),
    ?line filltabint(Table4,1000),
    ?line lists:map(fun(Tab) ->
		      B = ets:select(Tab,[{{'$1', '_'},
					   [{'>',
					     {'rem',
					      '$1', 5},
					     2}],
					   ['$_']}]),
		      B = ets:select_reverse(Tab,[{{'$1', '_'},
						   [{'>',
						     {'rem',
						      '$1', 5},
						     2}],
						   ['$_']}])
	      end,[Table2, Table3, Table4]),
    ok.



reverse_chunked(T,MS,N) ->
    do_reverse_chunked(ets:select_reverse(T,MS,N),[]).

do_reverse_chunked('$end_of_table',Acc) ->
    lists:reverse(Acc);
do_reverse_chunked({L,C},Acc) ->
    NewAcc = lists:reverse(L)++Acc,
    do_reverse_chunked(ets:select_reverse(C), NewAcc).


t_select_delete(doc) ->
    ["Test the ets:select_delete/2 and ets:select_count/2 BIF's"];
t_select_delete(suite) ->
    [];
t_select_delete(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    ?line Tables = fill_sets_int(10000) ++ fill_sets_int(10000,[{write_concurrency,true}]),
    lists:foreach
      (fun(Table) ->
	       ?line 4000 = ets:select_count(Table,[{{'$1', '_'},
						     [{'>', 
						       {'rem', 
							'$1', 5}, 
						       2}],
						     [true]}]),
	       ?line 4000 = ets:select_delete(Table,[{{'$1', '_'},
						      [{'>', 
							{'rem', 
							 '$1', 5}, 
							2}],
						      [true]}]),
	       ?line check(Table,
			   fun({N,_}) when (N rem 5) =< 2 ->
				   true;
			      (_) ->
				   false
			   end,
			   6000)

       end,
       Tables),
    lists:foreach
      (fun(Table) ->
	       ?line ets:select_delete(Table,[{'_',[],[true]}]),
	       ?line xfilltabint(Table,4000),
	       ?line successive_delete(Table,1,4001,bound),
	       ?line 0 = ets:info(Table,size),
	       ?line xfilltabint(Table,4000),
	       ?line successive_delete(Table,4000,0, bound),
	       ?line 0 = ets:info(Table,size),
	       ?line xfilltabint(Table,4000),
	       ?line successive_delete(Table,1,4001,unbound),
	       ?line 0 = ets:info(Table,size),
	       ?line xfilltabint(Table,4000),
	       ?line successive_delete(Table,4000,0, unbound),
	       ?line 0 = ets:info(Table,size)

       end,
       Tables),
    lists:foreach
      (fun(Table) ->
	       F = case ets:info(Table,type) of
		       X when X == bag; X == duplicate_bag ->
			   2;
		       _ -> 
			   1
		   end,
 	       ?line xfilltabstr(Table, 4000),
	       ?line 1000 = ets:select_count(Table,
					     [{{[$3 | '$1'], '_'},
					       [{'==',
						 {'length', '$1'},
						 3}],[true]}]) div F,
	       ?line 1000 = ets:select_delete(Table,
					      [{{[$3 | '$1'], '_'},
						[{'==',
						  {'length', '$1'},
						  3}],[true]}]) div F,
	       ?line check(Table, fun({[3,_,_,_],_}) -> false;
				     (_) -> true
				  end, 3000*F),
	       ?line 8 = ets:select_count(Table,
					  [{{"7",'_'},[],[false]},
					   {{['_'], '_'},
					    [],[true]}]) div F,
	       ?line 8 = ets:select_delete(Table,
					   [{{"7",'_'},[],[false]},
					    {{['_'], '_'},
					     [],[true]}]) div F,
	       ?line check(Table, fun({"7",_}) -> true;
				     ({[_],_}) -> false;
				     (_) -> true
				  end, 2992*F),
	       ?line xfilltabstr(Table, 4000),
	       %% This happens to be interesting for other select types too
	       ?line 200 = length(ets:select(Table,
					     [{{[$3,'_','_'],'_'},
					       [],[true]},
					      {{[$1,'_','_'],'_'},
					       [],[true]}])) div F,
	       ?line 200 = ets:select_count(Table,
					    [{{[$3,'_','_'],'_'},
					      [],[true]},
					     {{[$1,'_','_'],'_'},
					      [],[true]}]) div F,
	       ?line 200 = length(element(1,ets:select(Table,
						       [{{[$3,'_','_'],'_'},
							 [],[true]},
							{{[$1,'_','_'],'_'},
							 [],[true]}],
						       1000))) div F,
	       ?line 200 = length(
			     ets:select_reverse(Table,
						[{{[$3,'_','_'],'_'},
						  [],[true]},
						 {{[$1,'_','_'],'_'},
						  [],[true]}])) div F,
	       ?line 200 = length(
			     element(1,
				     ets:select_reverse
				     (Table,
				      [{{[$3,'_','_'],'_'},
					[],[true]},
				       {{[$1,'_','_'],'_'},
					[],[true]}],
				      1000))) div F,
	       ?line 200 = ets:select_delete(Table,
					     [{{[$3,'_','_'],'_'},
					       [],[true]},
					      {{[$1,'_','_'],'_'},
					       [],[true]}]) div F,
	       ?line 0 = ets:select_count(Table,
					  [{{[$3,'_','_'],'_'},
					    [],[true]},
					   {{[$1,'_','_'],'_'},
					    [],[true]}]) div F,
	       ?line check(Table, fun({[$3,_,_],_}) -> false;
				     ({[$1,_,_],_}) -> false;
				     (_) -> true
				  end, 3800*F)
       end,
       Tables),
    lists:foreach(fun(Tab) -> ets:delete(Tab) end,Tables),
    ?line verify_etsmem(EtsMem).

partly_bound(doc) ->
    ["Test that partly bound keys gives faster matches"];
partly_bound(suite) ->
    [];
partly_bound(Config) when is_list(Config) ->
    case os:type() of
	{win32,_} ->
	    {skip,"Inaccurate measurements on Windows"};
	_ ->
	    ?line EtsMem = etsmem(),
	    ?line dont_make_worse(),
	    ?line make_better(),
	    ?line verify_etsmem(EtsMem)
    end.

dont_make_worse() ->
    seventyfive_percent_success({?MODULE,dont_make_worse_sub,[]},0,0,10).

dont_make_worse_sub() ->
    ?line T = build_table([a,b],[a,b],15000),
    ?line T1 = time_match_object(T,{'_',a,a,1500}, [{{a,a,1500},a,a,1500}]),
    ?line T2 = time_match_object(T,{{a,a,'_'},a,a,1500}, 
				 [{{a,a,1500},a,a,1500}]),
    ?line ets:delete(T),
    ?line true = (T1 > T2),
    ok.
    
make_better() ->
    fifty_percent_success({?MODULE,make_better_sub2,[]},0,0,10),
    fifty_percent_success({?MODULE,make_better_sub1,[]},0,0,10).
make_better_sub1() ->
    ?line T = build_table2([a,b],[a,b],15000),
    ?line T1 = time_match_object(T,{'_',1500,a,a}, [{{1500,a,a},1500,a,a}]),
    ?line T2 = time_match_object(T,{{1500,a,'_'},1500,a,a}, 
				 [{{1500,a,a},1500,a,a}]),
    ?line ets:delete(T),
    ?line io:format("~p>~p~n",[(T1 / 100),T2]),
    ?line true = ((T1 / 100) > T2), % More marginal than needed.
    ok.

make_better_sub2() ->
    ?line T = build_table2([a,b],[a,b],15000),
    ?line T1 = time_match(T,{'$1',1500,a,a}),
    ?line T2 = time_match(T,{{1500,a,'$1'},1500,a,a}),
    ?line ets:delete(T),
    ?line io:format("~p>~p~n",[(T1 / 100),T2]),
    ?line true = ((T1 / 100) > T2), % More marginal than needed.
    ok.


match_heavy(doc) ->
    ["Heavy random matching, comparing set with ordered_set."];
match_heavy(suite) ->
    [];
match_heavy(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir,Config),
    DataDir = ?config(data_dir, Config),
    %% Easier to have in process dictionary when manually
    %% running the test function.
    put(where_to_read,DataDir),
    put(where_to_write,PrivDir),
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),    
    NewDog=test_server:timetrap(test_server:seconds(1000)),
    NewConfig = [{watchdog, NewDog} | lists:keydelete(watchdog,1,Config)],
    random_test(),
    drop_match(),
    NewConfig.

%%% Extra safety for the very low probability that this is not
%%% caught by the random test (Statistically impossible???) 
drop_match() ->
    ?line EtsMem = etsmem(),
    ?line T = build_table([a,b],[a],1500),
    ?line [{{a,a,1},a,a,1},{{b,a,1},b,a,1}] = 
	ets:match_object(T, {'_','_','_',1}),
    ?line true = ets:delete(T),
    ?line verify_etsmem(EtsMem).



ets_match(Tab,Expr) ->
    case random:uniform(2) of
	1 ->
	    ets:match(Tab,Expr);
	_ ->
	    match_chunked(Tab,Expr)
    end.

match_chunked(Tab,Expr) ->
    match_chunked_collect(ets:match(Tab,Expr,
				    random:uniform(1999) + 1)).
match_chunked_collect('$end_of_table') ->
    [];
match_chunked_collect({Results, Continuation}) ->
    Results ++ match_chunked_collect(ets:match(Continuation)).

ets_match_object(Tab,Expr) ->
    case random:uniform(2) of
	1 ->
	    ets:match_object(Tab,Expr);
	_ ->
	    match_object_chunked(Tab,Expr)
    end.

match_object_chunked(Tab,Expr) ->
    match_object_chunked_collect(ets:match_object(Tab,Expr,
						  random:uniform(1999) + 1)).
match_object_chunked_collect('$end_of_table') ->
    [];
match_object_chunked_collect({Results, Continuation}) ->
    Results ++ match_object_chunked_collect(ets:match_object(Continuation)).


    
random_test() ->
    ?line ReadDir = get(where_to_read),
    ?line WriteDir = get(where_to_write),
    ?line (catch file:make_dir(WriteDir)),
    ?line Seed = case file:consult(filename:join([ReadDir, 
					    "preset_random_seed.txt"])) of
	       {ok,[X]} ->
		   X;
	       _ ->
		   {A,B,C} = erlang:now(),
		   random:seed(A,B,C),
		   get(random_seed)
	   end,
    put(random_seed,Seed),
    ?line {ok, F} = file:open(filename:join([WriteDir, 
					     "last_random_seed.txt"]), 
			      [write]),
    io:format(F,"~p. ~n",[Seed]),
    file:close(F),
    io:format("Random seed ~p written to ~s, copy to ~s to rerun with "
	      "same seed.",[Seed, 
			    filename:join([WriteDir, "last_random_seed.txt"]),
			    filename:join([ReadDir, 
					    "preset_random_seed.txt"])]),
    do_random_test().

do_random_test() ->
    ?line EtsMem = etsmem(),
    ?line OrdSet = ets_new(xxx,[ordered_set]),
    ?line Set = ets_new(xxx,[]),
    ?line do_n_times(fun() ->
		       ?line Key = create_random_string(25),
		       ?line Value = create_random_tuple(25),
		       ?line ets:insert(OrdSet,{Key,Value}),
		       ?line ets:insert(Set,{Key,Value})
	       end, 5000),
    ?line io:format("~nData inserted~n"),
    ?line do_n_times(fun() ->
		       ?line I = random:uniform(25),
		       ?line Key = create_random_string(I) ++ '_',
		       ?line L1 = ets_match_object(OrdSet,{Key,'_'}),
		       ?line L2 = lists:sort(ets_match_object(Set,{Key,'_'})),
		       case L1 == L2 of
			   false ->
			       io:format("~p != ~p~n",
					 [L1,L2]),
			       ?line exit({not_eq, L1, L2});
			   true ->
			       ok
		       end
	       end,
	       2000),
    ?line io:format("~nData matched~n"),
    ?line ets:match_delete(OrdSet,'_'),
    ?line ets:match_delete(Set,'_'),
    ?line do_n_times(fun() ->
		       ?line Value = create_random_string(25),
		       ?line Key = create_random_tuple(25),
		       ?line ets:insert(OrdSet,{Key,Value}),
		       ?line ets:insert(Set,{Key,Value})
	       end, 2000),
    ?line io:format("~nData inserted~n"),
    (fun() ->
	     ?line Key = list_to_tuple(lists:duplicate(25,'_')),
	     ?line L1 = ets_match_object(OrdSet,{Key,'_'}),
	     ?line L2 = lists:sort(ets_match_object(Set,{Key,'_'})),
	     ?line 2000 = length(L1),
	     case L1 == L2 of
		 false ->
		     io:format("~p != ~p~n",
			       [L1,L2]),
		     ?line exit({not_eq, L1, L2});
		 true ->
		     ok
	     end
     end)(),
    (fun() ->
	     ?line Key = {'$1','$2','$3','$4',
			  '$5','$6','$7','$8',
			  '$9','$10','$11','$12',
			  '$13','$14','$15','$16',
			  '$17','$18','$19','$20',
			  '$21','$22','$23','$24',
			  '$25'},
	     ?line L1 = ets_match_object(OrdSet,{Key,'_'}),
	     ?line L2 = lists:sort(ets_match_object(Set,{Key,'_'})),
	     ?line 2000 = length(L1),
	     case L1 == L2 of
		 false ->
		     io:format("~p != ~p~n",
			       [L1,L2]),
		     ?line exit({not_eq, L1, L2});
		 true ->
		     ok
	     end
     end)(),
    (fun() ->
	     ?line Key = {'$1','$2','$3','$4',
			  '$5','$6','$7','$8',
			  '$9','$10','$11','$12',
			  '$13','$14','$15','$16',
			  '$17','$18','$19','$20',
			  '$21','$22','$23','$24',
			  '$25'},
	     ?line L1 = ets_match(OrdSet,{Key,'_'}),
	     ?line L2 = lists:sort(ets_match(Set,{Key,'_'})),
	     ?line 2000 = length(L1),
	     case L1 == L2 of
		 false ->
		     io:format("~p != ~p~n",
			       [L1,L2]),
		     ?line exit({not_eq, L1, L2});
		 true ->
		     ok
	     end
     end)(),
    ?line ets:match_delete(OrdSet,'_'),
    ?line ets:match_delete(Set,'_'),
    ?line do_n_times(fun() ->
		       ?line Value = create_random_string(25),
		       ?line Key = create_random_tuple(25),
		       ?line ets:insert(OrdSet,{Key,Value}),
		       ?line ets:insert(Set,{Key,Value})
	       end, 2000),
    ?line io:format("~nData inserted~n"),
    do_n_times(fun() ->
		       ?line Key = create_partly_bound_tuple(25),
		       ?line L1 = ets_match_object(OrdSet,{Key,'_'}),
		       ?line L2 = lists:sort(ets_match_object(Set,{Key,'_'})),
		       case L1 == L2 of
			   false ->
			       io:format("~p != ~p~n",
					 [L1,L2]),
			       ?line exit({not_eq, L1, L2});
			   true ->
			       ok
		       end
	       end,
	       2000),
    ?line do_n_times(fun() ->
		       ?line Key = create_partly_bound_tuple2(25),
		       ?line L1 = ets_match_object(OrdSet,{Key,'_'}),
		       ?line L2 = lists:sort(ets_match_object(Set,{Key,'_'})),
		       case L1 == L2 of
			   false ->
			       io:format("~p != ~p~n",
					 [L1,L2]),
			       ?line exit({not_eq, L1, L2});
			   true ->
			       ok
		       end
	       end,
	       2000),
    do_n_times(fun() ->
		       ?line Key = create_partly_bound_tuple2(25),
		       ?line L1 = ets_match(OrdSet,{Key,'_'}),
		       ?line L2 = lists:sort(ets_match(Set,{Key,'_'})),
		       case L1 == L2 of
			   false ->
			       io:format("~p != ~p~n",
					 [L1,L2]),
			       ?line exit({not_eq, L1, L2});
			   true ->
			       ok
		       end
	       end,
	       2000),
    io:format("~nData matched~n"),
    ets:match_delete(OrdSet,'_'),
    ets:match_delete(Set,'_'),
    do_n_times(fun() ->
		       do_n_times(fun() ->
					  ?line Value = 
					      create_random_string(25),
					  ?line Key = create_random_tuple(25),
					  ?line ets:insert(OrdSet,{Key,Value}),
					  ?line ets:insert(Set,{Key,Value})
				  end, 500),
		       io:format("~nData inserted~n"),
		       do_n_times(fun() ->
					  ?line Key = 
					      create_partly_bound_tuple(25),
					  ets:match_delete(OrdSet,{Key,'_'}),
					  ets:match_delete(Set,{Key,'_'}),
					  L1 = ets:info(OrdSet,size),
					  L2 = ets:info(Set,size),
					  [] = ets_match_object(OrdSet,
								{Key,'_'}),
					  case L1 == L2 of
					      false ->
						  io:format("~p != ~p "
							    "(deleted ~p)~n",
							    [L1,L2,Key]),
						  exit({not_eq, L1, L2,
							{deleted,Key}});
					      true ->
						  ok
					  end
				  end,
				  50),
		       io:format("~nData deleted~n")
	       end,
	       10),
    ets:delete(OrdSet),
    ets:delete(Set),
    ?line verify_etsmem(EtsMem).

update_element(doc) ->
    ["test various variants of update_element"];
update_element(suite) ->
    [];
update_element(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    repeat_for_opts(update_element_opts),
    ?line verify_etsmem(EtsMem).

update_element_opts(Opts) ->
    TupleCases = [{{key,val}, 1 ,2}, 
		  {{val,key}, 2, 1}, 
		  {{key,val}, 1 ,[2]}, 
		  {{key,val,val}, 1, [2,3]},
		  {{val,key,val,val}, 2, [3,4,1]},
		  {{val,val,key,val}, 3, [1,4,1,2]}, % update pos1 twice
		  {{val,val,val,key}, 4, [2,1,2,3]}],% update pos2 twice
    
    lists:foreach(fun({Tuple,KeyPos,UpdPos}) -> update_element_opts(Tuple,KeyPos,UpdPos,Opts) end, 
		  TupleCases),
    
    update_element_neg(Opts).
    


update_element_opts(Tuple,KeyPos,UpdPos,Opts) ->
    Set = ets_new(set,[{keypos,KeyPos} | Opts]),
    OrdSet = ets_new(ordered_set,[ordered_set,{keypos,KeyPos} | Opts]),
    update_element(Set,Tuple,KeyPos,UpdPos),
    update_element(OrdSet,Tuple,KeyPos,UpdPos),
    true = ets:delete(Set),
    true = ets:delete(OrdSet),
    ok.

update_element(T,Tuple,KeyPos,UpdPos) -> 
    KeyList = [17,"seventeen",<<"seventeen">>,{17},list_to_binary(lists:seq(1,100)),make_ref(), self()],
    lists:foreach(fun(Key) -> 
			  TupleWithKey = setelement(KeyPos,Tuple,Key),
			  update_element_do(T,TupleWithKey,Key,UpdPos)
		  end,
		  KeyList).
		    
update_element_do(Tab,Tuple,Key,UpdPos) ->

    % Strategy: Step around in Values array and call ets:update_element for the values.
    % Take Length number of steps of size 1, then of size 2, ..., Length-1.
    % This will try all combinations of {fromValue,toValue}
    %
    % IMPORTANT: size(Values) must be a prime number for this to work!!!

    %io:format("update_element_do for key=~p\n",[Key]),
    Big32 = 16#12345678,
    Big64 = 16#123456789abcdef0,
    Values = { 623, -27, 0, Big32, -Big32, Big64, -Big64, Big32*Big32,
	       -Big32*Big32, Big32*Big64, -Big32*Big64, Big64*Big64, -Big64*Big64,
	       "A", "Sverker", [], {12,-132}, {},
	       <<45,232,0,12,133>>, <<234,12,23>>, list_to_binary(lists:seq(1,100)),
	       (fun(X) -> X*Big32 end),
	       make_ref(), make_ref(), self(), ok, update_element, 28, 29 },
    Length = size(Values),
    
    PosValArgF = fun(ToIx, ResList, [Pos | PosTail], Rand, MeF) ->
			 NextIx = (ToIx+Rand) rem Length,
			 MeF(NextIx, [{Pos,element(ToIx+1,Values)} | ResList], PosTail, Rand, MeF);

		    (_ToIx, ResList, [], _Rand, _MeF) ->
			 ResList;
		    
		    (ToIx, [], Pos, _Rand, _MeF) ->
			 {Pos, element(ToIx+1,Values)}   % single {pos,value} arg
		 end,

    UpdateF = fun(ToIx,Rand) -> 
		      PosValArg = PosValArgF(ToIx,[],UpdPos,Rand,PosValArgF),
		      %%io:format("update_element(~p)~n",[PosValArg]),
		      ArgHash = erlang:phash2({Tab,Key,PosValArg}),
		      ?line true = ets:update_element(Tab, Key, PosValArg),
		      ?line ArgHash = erlang:phash2({Tab,Key,PosValArg}),
		      NewTuple = update_tuple(PosValArg,Tuple),
		      ?line [NewTuple] = ets:lookup(Tab,Key)
	      end,

    LoopF = fun(_FromIx, Incr, _Times, Checksum, _MeF) when Incr >= Length -> 
		    Checksum; % done

	       (FromIx, Incr, 0, Checksum, MeF) -> 
		    MeF(FromIx, Incr+1, Length, Checksum, MeF);

	       (FromIx, Incr, Times, Checksum, MeF) -> 
		    ToIx = (FromIx + Incr) rem Length,
		    UpdateF(ToIx,Checksum),
		    if 
			Incr =:= 0 -> UpdateF(ToIx,Checksum);  % extra update to same value
			true -> true
		    end,	    
		    MeF(ToIx, Incr, Times-1, Checksum+ToIx+1, MeF)
	    end,

    FirstTuple = Tuple,
    ?line true = ets:insert(Tab,FirstTuple),
    ?line [FirstTuple] = ets:lookup(Tab,Key),
    
    Checksum = LoopF(0, 1, Length, 0, LoopF),
    ?line Checksum = (Length-1)*Length*(Length+1) div 2,  % if Length is a prime
    ok.

update_tuple({Pos,Val}, Tpl) ->
    setelement(Pos, Tpl, Val);
update_tuple([{Pos,Val} | Tail], Tpl) ->
    update_tuple(Tail,setelement(Pos, Tpl, Val));
update_tuple([], Tpl) ->
    Tpl.



update_element_neg(Opts) ->
    Set = ets_new(set,Opts),
    OrdSet = ets_new(ordered_set,[ordered_set | Opts]),
    update_element_neg_do(Set),
    update_element_neg_do(OrdSet),
    ets:delete(Set),
    ?line {'EXIT',{badarg,_}} = (catch ets:update_element(Set,key,{2,1})),
    ets:delete(OrdSet),
    ?line {'EXIT',{badarg,_}} = (catch ets:update_element(OrdSet,key,{2,1})),

    ?line Bag = ets_new(bag,[bag | Opts]),
    ?line DBag = ets_new(duplicate_bag,[duplicate_bag | Opts]),
    ?line {'EXIT',{badarg,_}} = (catch ets:update_element(Bag,key,{2,1})),
    ?line {'EXIT',{badarg,_}} = (catch ets:update_element(DBag,key,{2,1})),
    true = ets:delete(Bag),
    true = ets:delete(DBag),
    ok.


update_element_neg_do(T) ->
    Object = {key, 0, "Hej"},
    ?line true = ets:insert(T,Object),

    UpdateF = fun(Arg3) -> 
		      ArgHash = erlang:phash2({T,key,Arg3}),
		      ?line {'EXIT',{badarg,_}} = (catch ets:update_element(T,key,Arg3)),
		      ?line ArgHash = erlang:phash2({T,key,Arg3}),
		      ?line [Object] = ets:lookup(T,key)
	      end,

    %% List of invalid {Pos,Value} tuples
    InvList = [false, {2}, {2,1,false}, {false,1}, {0,1}, {1,1}, {-1,1}, {4,1}],

    lists:foreach(UpdateF, InvList),
    lists:foreach(fun(InvTpl) -> UpdateF([{2,1},InvTpl]) end, InvList),
    lists:foreach(fun(InvTpl) -> UpdateF([InvTpl,{2,1}]) end, InvList),
    lists:foreach(fun(InvTpl) -> UpdateF([{2,1},{3,"Hello"},InvTpl]) end, InvList),
    lists:foreach(fun(InvTpl) -> UpdateF([{3,"Hello"},{2,1},InvTpl]) end, InvList),
    lists:foreach(fun(InvTpl) -> UpdateF([{2,1},InvTpl,{3,"Hello"}]) end, InvList),
    lists:foreach(fun(InvTpl) -> UpdateF([InvTpl,{3,"Hello"},{2,1}]) end, InvList),
    UpdateF([{2,1} | {3,1}]),
    lists:foreach(fun(InvTpl) -> UpdateF([{2,1} | InvTpl]) end, InvList),

    ?line true = ets:update_element(T,key,[]),
    ?line false = ets:update_element(T,false,[]),
    ?line false = ets:update_element(T,false,{2,1}),
    ?line ets:delete(T,key),
    ?line false = ets:update_element(T,key,{2,1}),
    ok.


update_counter(doc) ->
    ["test various variants of update_counter"];
update_counter(suite) ->
    [];
update_counter(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    repeat_for_opts(update_counter_do),
    ?line verify_etsmem(EtsMem).

update_counter_do(Opts) ->
    Set = ets_new(set,Opts),
    OrdSet = ets_new(ordered_set,[ordered_set | Opts]),
    update_counter_for(Set),
    update_counter_for(OrdSet),
    ets:delete(Set),
    ets:delete(OrdSet),
    update_counter_neg(Opts).

update_counter_for(T) ->
    ?line ets:insert(T,{a,1,1}),
    ?line 101 = ets:update_counter(T,a,100),
    ?line [{a,101,1}] = ets:lookup(T,a),
    ?line 101 = ets:update_counter(T,a,{3,100}),
    ?line [{a,101,101}] = ets:lookup(T,a),


    LooperF = fun(Obj, 0, _, _) ->
		      Obj;
		 
		 (Obj, Times, Arg3, Myself) ->
		      ?line {NewObj, Ret} = uc_mimic(Obj,Arg3),
		      ArgHash = erlang:phash2({T,a,Arg3}),
		      %%io:format("update_counter(~p, ~p, ~p) expecting ~p\n",[T,a,Arg3,Ret]),
		      ?line Ret = ets:update_counter(T,a,Arg3),
		      ?line ArgHash = erlang:phash2({T,a,Arg3}),
		      %%io:format("NewObj=~p~n ",[NewObj]),
		      ?line [NewObj] = ets:lookup(T,a),
		      Myself(NewObj,Times-1,Arg3,Myself)
	      end,		  

    LoopF = fun(Obj, Times, Arg3) ->
		    %%io:format("Loop start:\nObj = ~p\nArg3=~p\n",[Obj,Arg3]),
		    LooperF(Obj,Times,Arg3,LooperF)
	    end,

    SmallMax32 = (1 bsl 27) - 1,
    SmallMax64 = (1 bsl (27+32)) - 1,
    Big1Max32 = (1 bsl 32) - 1,
    Big1Max64 = (1 bsl 64) - 1,

    Steps = 100,
    Obj0 = {a,0,0,0,0},
    ?line ets:insert(T,Obj0),
    ?line Obj1 = LoopF(Obj0, Steps, {2,(SmallMax32 div Steps)*2}),
    ?line Obj2 = LoopF(Obj1, Steps, {3,(SmallMax64 div Steps)*2}),
    ?line Obj3 = LoopF(Obj2, Steps, {4,(Big1Max32 div Steps)*2}),
    ?line Obj4 = LoopF(Obj3, Steps, {5,(Big1Max64 div Steps)*2}),

    ?line Obj5 = LoopF(Obj4, Steps, {2,-(SmallMax32 div Steps)*4}),
    ?line Obj6 = LoopF(Obj5, Steps, {3,-(SmallMax64 div Steps)*4}),
    ?line Obj7 = LoopF(Obj6, Steps, {4,-(Big1Max32 div Steps)*4}),
    ?line Obj8 = LoopF(Obj7, Steps, {5,-(Big1Max64 div Steps)*4}),

    ?line Obj9 = LoopF(Obj8, Steps, {2,(SmallMax32 div Steps)*2}),
    ?line ObjA = LoopF(Obj9, Steps, {3,(SmallMax64 div Steps)*2}),
    ?line ObjB = LoopF(ObjA, Steps, {4,(Big1Max32 div Steps)*2}),
    ?line Obj0 = LoopF(ObjB, Steps, {5,(Big1Max64 div Steps)*2}),

    %% back at zero, same trip again with lists

    ?line Obj4 = LoopF(Obj0,Steps,[{2, (SmallMax32 div Steps)*2},
 				   {3, (SmallMax64 div Steps)*2},
 				   {4, (Big1Max32 div Steps)*2},
 				   {5, (Big1Max64 div Steps)*2}]),

    ?line Obj8 = LoopF(Obj4,Steps,[{4, -(Big1Max32 div Steps)*4},
 				   {2, -(SmallMax32 div Steps)*4},
 				   {5, -(Big1Max64 div Steps)*4},
 				   {3, -(SmallMax64 div Steps)*4}]),

    ?line Obj0 = LoopF(Obj8,Steps,[{5, (Big1Max64 div Steps)*2},
 				   {2, (SmallMax32 div Steps)*2},
 				   {4, (Big1Max32 div Steps)*2},				   
 				   {3, (SmallMax64 div Steps)*2}]),

    %% make them shift size at the same time
    ?line ObjC = LoopF(Obj0,Steps,[{5, (Big1Max64 div Steps)*2},
 				   {3, (Big1Max64 div Steps)*2 + 1},
 				   {2, -(Big1Max64 div Steps)*2},
 				   {4, -(Big1Max64 div Steps)*2 + 1}]),

    %% update twice in same list
    ?line ObjD = LoopF(ObjC,Steps,[{5, -(Big1Max64 div Steps) + 1},
 				   {3, -(Big1Max64 div Steps)*2 - 1},
 				   {5, -(Big1Max64 div Steps) - 1},
 				   {4, (Big1Max64 div Steps)*2 - 1}]),

    ?line Obj0 = LoopF(ObjD,Steps,[{2, (Big1Max64 div Steps) - 1},
 				   {4, Big1Max64*2},
 				   {2, (Big1Max64 div Steps) + 1},
 				   {4, -Big1Max64*2}]),

    %% warping with list
    ?line ObjE = LoopF(Obj0,1000,
 		       [{3,SmallMax32*4 div 5,SmallMax32*2,-SmallMax32*2},
 			{5,-SmallMax64*4 div 7,-SmallMax64*2,SmallMax64*2},
 			{4,-Big1Max32*4 div 11,-Big1Max32*2,Big1Max32*2},
 			{2,Big1Max64*4 div 13,Big1Max64*2,-Big1Max64*2}]),

    %% warping without list
    ?line ObjF = LoopF(ObjE,1000,{3,SmallMax32*4 div 5,SmallMax32*2,-SmallMax32*2}),
    ?line ObjG = LoopF(ObjF,1000,{5,-SmallMax64*4 div 7,-SmallMax64*2,SmallMax64*2}),
    ?line ObjH = LoopF(ObjG,1000,{4,-Big1Max32*4 div 11,-Big1Max32*2,Big1Max32*2}),
    ?line ObjI = LoopF(ObjH,1000,{2,Big1Max64*4 div 13,Big1Max64*2,-Big1Max64*2}),

    %% mixing it up
    ?line LoopF(ObjI,1000,
		[{3,SmallMax32*4 div 5,SmallMax32*2,-SmallMax32*2},
		 {5,-SmallMax64*4 div 3},
		 {3,-SmallMax32*4 div 11},
		 {5,0},
		 {4,1},
		 {5,-SmallMax64*4 div 7,-SmallMax64*2,SmallMax64*2},
		 {2,Big1Max64*4 div 13,Big1Max64*2,-Big1Max64*2}]),
    ok.

%% uc_mimic works kind of like the real ets:update_counter
%% Obj = Tuple in ets
%% Pits = {Pos,Incr} | {Pos,Incr,Thres,Warp}
%% Returns {Updated tuple in ets, Return value from update_counter}
uc_mimic(Obj, Pits) when is_tuple(Pits) ->
    ?line Pos = element(1,Pits),
    ?line NewObj = setelement(Pos, Obj, uc_adder(element(Pos,Obj),Pits)),
    ?line {NewObj, element(Pos,NewObj)};

uc_mimic(Obj, PitsList) when is_list(PitsList) ->
    ?line {NewObj,ValList} = uc_mimic(Obj,PitsList,[]),
    ?line {NewObj,lists:reverse(ValList)}.

uc_mimic(Obj, [], Acc) ->
    ?line {Obj,Acc};
uc_mimic(Obj, [Pits|Tail], Acc) ->
    ?line {NewObj,NewVal} = uc_mimic(Obj,Pits),
    ?line uc_mimic(NewObj,Tail,[NewVal|Acc]).		    

uc_adder(Init, {_Pos, Add}) ->
    Init + Add;
uc_adder(Init, {_Pos, Add, Thres, Warp}) ->		   
    case Init + Add of
	X when X > Thres, Add > 0 ->
	    Warp;
	Y when Y < Thres, Add < 0 ->
	    Warp;
	Z ->
	    Z
    end.
    
update_counter_neg(Opts) ->
    Set = ets_new(set,Opts),
    OrdSet = ets_new(ordered_set,[ordered_set | Opts]),
    update_counter_neg_for(Set),
    update_counter_neg_for(OrdSet),
    ets:delete(Set),
    ?line {'EXIT',{badarg,_}} = (catch ets:update_counter(Set,key,1)),
    ets:delete(OrdSet),
    ?line {'EXIT',{badarg,_}} = (catch ets:update_counter(OrdSet,key,1)),

    ?line Bag = ets_new(bag,[bag | Opts]),
    ?line DBag = ets_new(duplicate_bag,[duplicate_bag | Opts]),
    ?line {'EXIT',{badarg,_}} = (catch ets:update_counter(Bag,key,1)),
    ?line {'EXIT',{badarg,_}} = (catch ets:update_counter(DBag,key,1)),
    true = ets:delete(Bag),
    true = ets:delete(DBag),
    ok.

update_counter_neg_for(T) ->
    Object = {key,0,false,1},
    ?line true = ets:insert(T,Object),

    UpdateF = fun(Arg3) -> 
		      ArgHash = erlang:phash2({T,key,Arg3}),
		      ?line {'EXIT',{badarg,_}} = (catch ets:update_counter(T,key,Arg3)),
		      ?line ArgHash = erlang:phash2({T,key,Arg3}),
		      ?line [Object] = ets:lookup(T,key)
	      end,

    %% List of invalid arg3-tuples
    InvList = [false, {2}, {2,false}, {false,1},
	       {0,1}, {-1,1}, % BUG < R12B-2
	       {1,1}, {3,1}, {5,1}, {2,1,100}, {2,1,100,0,false}, {2,1,false,0}, {2,1,0,false}],

    lists:foreach(UpdateF, InvList),
    lists:foreach(fun(Inv) -> UpdateF([{2,1},Inv]) end, InvList),
    lists:foreach(fun(Inv) -> UpdateF([Inv,{2,1}]) end, InvList),
    lists:foreach(fun(Inv) -> UpdateF([{2,1},{4,-100},Inv]) end, InvList),
    lists:foreach(fun(Inv) -> UpdateF([{4,100,50,0},{2,1},Inv]) end, InvList),
    lists:foreach(fun(Inv) -> UpdateF([{2,1},Inv,{4,100,50,0}]) end, InvList),
    lists:foreach(fun(Inv) -> UpdateF([Inv,{4,100,50,0},{2,1}]) end, InvList),
    UpdateF([{2,1} | {4,1}]),
    lists:foreach(fun(Inv) -> UpdateF([{2,1} | Inv]) end, InvList),

    ?line {'EXIT',{badarg,_}} = (catch ets:update_counter(T,false,1)),
    ?line ets:delete(T,key),
    ?line {'EXIT',{badarg,_}} = (catch ets:update_counter(T,key,1)),
    ok.

		    
evil_update_counter(Config) when is_list(Config) ->
    %% The code server uses ets table. Pre-load modules that might not be
    %% already loaded.
    gb_sets:module_info(),
    math:module_info(),
    ordsets:module_info(),
    random:module_info(),

    repeat_for_opts(evil_update_counter_do).

evil_update_counter_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line process_flag(trap_exit, true),
    ?line Pids = [my_spawn_link(fun() -> evil_counter(I,Opts) end)  || I <- lists:seq(1, 40)],
    ?line wait_for_all(gb_sets:from_list(Pids)),
    ?line verify_etsmem(EtsMem),
    ok.

wait_for_all(Pids0) ->
    case gb_sets:is_empty(Pids0) of
	true ->
	    ok;
	false ->
	    receive
		{'EXIT',Pid,normal} ->
		    ?line Pids = gb_sets:delete(Pid, Pids0),
		    wait_for_all(Pids);
		Other ->
		    io:format("unexpected: ~p\n", [Other]),
		    ?line ?t:fail()
	    end
    end.

evil_counter(I,Opts) ->
    T = ets_new(a, Opts),
    Start0 = case I rem 3 of
		0 -> 16#12345678;
		1 -> 16#12345678FFFFFFFF;
		2 -> 16#7777777777FFFFFFFF863648726743
	    end,
    Start = Start0 + random:uniform(100000),
    ets:insert(T, {dracula,Start}),
    Iter = 40000,
    End = Start + Iter,
    End = evil_counter_1(Iter, T),
    ets:delete(T).

evil_counter_1(0, T) ->
    [{dracula,Count}] = ets:lookup(T, dracula),
    Count;
evil_counter_1(Iter, T) ->
    ets:update_counter(T, dracula, 1),
    evil_counter_1(Iter-1, T).

fixtable_next(doc) ->
    ["Check that a first-next sequence always works on a fixed table"];
fixtable_next(suite) ->
    [];
fixtable_next(Config) when is_list(Config) ->
    repeat_for_opts(fixtable_next_do, [write_concurrency,all_types]).

fixtable_next_do(Opts) ->
    ?line EtsMem = etsmem(),    
    ?line do_fixtable_next(ets_new(set,[public | Opts])),
    ?line verify_etsmem(EtsMem).
    
do_fixtable_next(Tab) ->
   ?line  F = fun(X,T,FF) -> case X of 
			   0 -> true; 
			   _ -> 
			       ets:insert(T, {X,
					      integer_to_list(X),
					      X rem 10}),
			       FF(X-1,T,FF) 
		       end 
	end,
    ?line F(100,Tab,F),
    ?line ets:safe_fixtable(Tab,true),
    ?line First = ets:first(Tab),
    ?line ets:delete(Tab, First),
    ?line ets:next(Tab, First),
    ?line ets:match_delete(Tab,{'_','_','_'}),
    ?line '$end_of_table' = ets:next(Tab, First),
    ?line true = ets:info(Tab, fixed),
    ?line ets:safe_fixtable(Tab, false),
    ?line false = ets:info(Tab, fixed),
    ?line ets:delete(Tab).

fixtable_insert(doc) ->    
    ["Check inserts of deleted keys in fixed bags"];
fixtable_insert(suite) ->
    [];
fixtable_insert(Config) when is_list(Config) ->
    Combos = [[Type,{write_concurrency,WC}] || Type<- [bag,duplicate_bag], 
					       WC <- [false,true]],
    lists:foreach(fun(Opts) -> fixtable_insert_do(Opts) end,
		  Combos),
    ok.
    
fixtable_insert_do(Opts) ->
    io:format("Opts = ~p\n",[Opts]),
    Ets = make_table(ets, Opts, [{a,1}, {a,2}, {b,1}, {b,2}]),
    ets:safe_fixtable(Ets,true),
    ets:match_delete(Ets,{b,1}),
    First = ets:first(Ets),
    ?line Next = case First of
		     a -> b;
		     b -> a
		 end,
    ?line Next = ets:next(Ets,First),
    ets:delete(Ets,Next),
    ?line '$end_of_table' = ets:next(Ets,First),
    ets:insert(Ets, {Next,1}),
    ?line false = ets:insert_new(Ets, {Next,1}),
    ?line Next = ets:next(Ets,First),
    ?line '$end_of_table' = ets:next(Ets,Next),
    ets:delete(Ets,Next),
    '$end_of_table' = ets:next(Ets,First),
    ets:insert(Ets, {Next,2}),
    ?line false = ets:insert_new(Ets, {Next,1}),
    Next = ets:next(Ets,First),
    '$end_of_table' = ets:next(Ets,Next),
    ets:delete(Ets,First),
    ?line Next = ets:first(Ets),
    ?line '$end_of_table' = ets:next(Ets,Next),
    ets:delete(Ets,Next),
    ?line '$end_of_table' = ets:next(Ets,First),
    ?line true = ets:insert_new(Ets,{Next,1}),
    ?line false = ets:insert_new(Ets,{Next,2}),
    ?line Next = ets:next(Ets,First),
    ets:delete_object(Ets,{Next,1}),
    ?line '$end_of_table' = ets:next(Ets,First),
    ?line true = ets:insert_new(Ets,{Next,2}),
    ?line false = ets:insert_new(Ets,{Next,1}),
    ?line Next = ets:next(Ets,First),
    ets:delete(Ets,First),
    ets:safe_fixtable(Ets,false),
    {'EXIT',{badarg,_}} = (catch ets:next(Ets,First)),
    ok.

write_concurrency(doc) -> ["The 'write_concurrency' option"];
write_concurrency(suite) -> [];
write_concurrency(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    Yes1 = ets_new(foo,[public,{write_concurrency,true}]),
    Yes2 = ets_new(foo,[protected,{write_concurrency,true}]),
    No1 = ets_new(foo,[private,{write_concurrency,true}]),

    Yes3 = ets_new(foo,[bag,public,{write_concurrency,true}]),
    Yes4 = ets_new(foo,[bag,protected,{write_concurrency,true}]),
    No2 = ets_new(foo,[bag,private,{write_concurrency,true}]),

    Yes5 = ets_new(foo,[duplicate_bag,public,{write_concurrency,true}]),
    Yes6 = ets_new(foo,[duplicate_bag,protected,{write_concurrency,true}]),
    No3 = ets_new(foo,[duplicate_bag,private,{write_concurrency,true}]),

    No4 = ets_new(foo,[ordered_set,public,{write_concurrency,true}]),
    No5 = ets_new(foo,[ordered_set,protected,{write_concurrency,true}]),
    No6 = ets_new(foo,[ordered_set,private,{write_concurrency,true}]),

    No7 = ets_new(foo,[public,{write_concurrency,false}]),
    No8 = ets_new(foo,[protected,{write_concurrency,false}]),

    ?line YesMem = ets:info(Yes1,memory),
    ?line NoHashMem = ets:info(No1,memory),
    ?line NoTreeMem = ets:info(No4,memory),
    io:format("YesMem=~p NoHashMem=~p NoTreeMem=~p\n",[YesMem,NoHashMem,NoTreeMem]),

    ?line YesMem = ets:info(Yes2,memory),
    ?line YesMem = ets:info(Yes3,memory),
    ?line YesMem = ets:info(Yes4,memory),
    ?line YesMem = ets:info(Yes5,memory),
    ?line YesMem = ets:info(Yes6,memory),
    ?line NoHashMem = ets:info(No2,memory),
    ?line NoHashMem = ets:info(No3,memory),
    ?line NoTreeMem = ets:info(No5,memory),
    ?line NoTreeMem = ets:info(No6,memory),
    ?line NoHashMem = ets:info(No7,memory),
    ?line NoHashMem = ets:info(No8,memory),
    
    case erlang:system_info(smp_support) of
	true ->
	    ?line true = YesMem > NoHashMem,
	    ?line true = YesMem > NoTreeMem;
	false ->
	    ?line true = YesMem =:= NoHashMem
    end,

    ?line {'EXIT',{badarg,_}} = (catch ets_new(foo,[public,{write_concurrency,foo}])),
    ?line {'EXIT',{badarg,_}} = (catch ets_new(foo,[public,{write_concurrency}])),
    ?line {'EXIT',{badarg,_}} = (catch ets_new(foo,[public,{write_concurrency,true,foo}])),
    ?line {'EXIT',{badarg,_}} = (catch ets_new(foo,[public,write_concurrency])),

    lists:foreach(fun(T) -> ets:delete(T) end,
		  [Yes1,Yes2,Yes3,Yes4,Yes5,Yes6,
		   No1,No2,No3,No4,No5,No6,No7,No8]),
    ?line verify_etsmem(EtsMem),
    ok.
    
    
heir(doc) ->   ["The 'heir' option"];
heir(suite) -> [];
heir(Config) when is_list(Config) ->
    repeat_for_opts(heir_do).

heir_do(Opts) ->
    ?line EtsMem = etsmem(),
    Master = self(),

    %% Different types of heir data and link/monitor relations
    TestFun = fun(Arg) -> {EtsMem,Arg} end,
    Combos = [{Data,Mode} || Data<-[foo_data, <<"binary">>, 
				    lists:seq(1,10), {17,TestFun,self()},
				    "The busy heir"],
			     Mode<-[none,link,monitor]],
    ?line lists:foreach(fun({Data,Mode})-> heir_1(Data,Mode,Opts) end,
			Combos),    			
			 
    %% No heir
    {Founder1,MrefF1} = my_spawn_monitor(fun()->heir_founder(Master,foo_data,Opts)end),
    Founder1 ! {go, none},
    ?line {"No heir",Founder1} = receive_any(),
    ?line {'DOWN', MrefF1, process, Founder1, normal} = receive_any(),
    ?line undefined = ets:info(foo),

    %% An already dead heir
    {Heir2,MrefH2} = my_spawn_monitor(fun()->die end),
    ?line {'DOWN', MrefH2, process, Heir2, normal} = receive_any(),
    {Founder2,MrefF2} = my_spawn_monitor(fun()->heir_founder(Master,foo_data,Opts)end),
    Founder2 ! {go, Heir2},
    ?line {"No heir",Founder2} = receive_any(),
    ?line {'DOWN', MrefF2, process, Founder2, normal} = receive_any(),
    ?line undefined = ets:info(foo),

    %% When heir dies before founder
    {Founder3,MrefF3} = my_spawn_monitor(fun()->heir_founder(Master,"The dying heir",Opts)end),
    {Heir3,MrefH3} = my_spawn_monitor(fun()->heir_heir(Founder3)end),
    Founder3 ! {go, Heir3},
    ?line {'DOWN', MrefH3, process, Heir3, normal} = receive_any(),
    Founder3 ! die_please,
    ?line {'DOWN', MrefF3, process, Founder3, normal} = receive_any(),
    ?line undefined = ets:info(foo),

    %% When heir dies and pid reused before founder dies
    repeat_while(fun() ->
			 NextPidIx = erts_debug:get_internal_state(next_pid),
			 {Founder4,MrefF4} = my_spawn_monitor(fun()->heir_founder(Master,"The dying heir",Opts)end),
			 {Heir4,MrefH4} = my_spawn_monitor(fun()->heir_heir(Founder4)end),
			 Founder4 ! {go, Heir4},
			 ?line {'DOWN', MrefH4, process, Heir4, normal} = receive_any(),
			 erts_debug:set_internal_state(next_pid, NextPidIx),
			 DoppelGanger = spawn_monitor_with_pid(Heir4, 
							       fun()-> ?line die_please = receive_any() end),
			 Founder4 ! die_please,
			 ?line {'DOWN', MrefF4, process, Founder4, normal} = receive_any(),
			 case DoppelGanger of
			     {Heir4,MrefH4_B} ->
				 Heir4 ! die_please,
				 ?line {'DOWN', MrefH4_B, process, Heir4, normal} = receive_any(),
				 ?line undefined = ets:info(foo),
				 false;
			     failed ->
				 io:format("Failed to spawn process with pid ~p\n", [Heir4]),
				 true % try again
			 end			  
		 end),
	    
    ?line verify_etsmem(EtsMem).

heir_founder(Master, HeirData, Opts) ->    
    ?line {go,Heir} = receive_any(),
    HeirTpl = case Heir of
		  none -> {heir,none};
		  _ -> {heir, Heir, HeirData}
	      end,
    ?line T = ets_new(foo,[named_table, private, HeirTpl | Opts]),
    ?line true = ets:insert(T,{key,1}),
    ?line [{key,1}] = ets:lookup(T,key),
    Self = self(),
    ?line Self = ets:info(T,owner),
    ?line case ets:info(T,heir) of
	      none ->
		  ?line true = (Heir =:= none) orelse (not is_process_alive(Heir)),
		  Master ! {"No heir",self()};
	      
	      Heir -> 
		  ?line true = is_process_alive(Heir),
		  Heir ! {table,T,HeirData},
		  die_please = receive_any()
	  end.


heir_heir(Founder) ->
    heir_heir(Founder, none).
heir_heir(Founder, Mode) ->
    ?line {table,T,HeirData} = receive_any(),
    ?line {'EXIT',{badarg,_}} = (catch ets:lookup(T,key)),
    ?line case HeirData of
	      "The dying heir" -> exit(normal);
	      _ -> ok
	  end,

    ?line Mref = case Mode of
		     link -> process_flag(trap_exit, true),
			     link(Founder);			      
		     monitor -> erlang:monitor(process,Founder);
		     none -> ok
		 end,
    ?line Founder ! die_please,
    ?line Msg = case HeirData of
		    "The busy heir" -> receive_any_spinning();
		    _ -> receive_any()
		end,
    ?line {'ETS-TRANSFER', T, Founder, HeirData} = Msg,
    ?line foo = T,
    ?line Self = self(),
    ?line Self = ets:info(T,owner),
    ?line Self = ets:info(T,heir),
    ?line [{key,1}] = ets:lookup(T,key),
    ?line true = ets:insert(T,{key,2}),
    ?line [{key,2}] = ets:lookup(T,key),
    ?line case Mode of % Verify that EXIT or DOWN comes after ETS-TRANSFER
	      link -> 
		  {'EXIT',Founder,normal} = receive_any(),
		  process_flag(trap_exit, false);
	      monitor -> 
		  {'DOWN', Mref, process, Founder, normal} = receive_any();
	      none -> ok
	  end.


heir_1(HeirData,Mode,Opts) ->
    io:format("test with heir_data = ~p\n", [HeirData]),
    Master = self(),
    ?line Founder = my_spawn_link(fun() -> heir_founder(Master,HeirData,Opts) end),
    io:format("founder spawned = ~p\n", [Founder]),
    ?line {Heir,Mref} = my_spawn_monitor(fun() -> heir_heir(Founder,Mode) end),
    io:format("heir spawned = ~p\n", [{Heir,Mref}]),
    ?line Founder ! {go, Heir},
    ?line {'DOWN', Mref, process, Heir, normal} = receive_any().

give_away(doc) -> ["ets:give_way/3"];    
give_away(suite) -> [];
give_away(Config) when is_list(Config) ->    
    repeat_for_opts(give_away_do).

give_away_do(Opts) ->
    ?line T = ets_new(foo,[named_table, private | Opts]),
    ?line true = ets:insert(T,{key,1}),
    ?line [{key,1}] = ets:lookup(T,key),
    Parent = self(),

    %% Give and then give back
    ?line {Receiver,Mref} = my_spawn_monitor(fun()-> give_away_receiver(T,Parent) end),
    ?line give_me = receive_any(),
    ?line true = ets:give_away(T,Receiver,here_you_are),
    ?line {'EXIT',{badarg,_}} = (catch ets:lookup(T,key)),
    ?line Receiver ! give_back,
    ?line {'ETS-TRANSFER',T,Receiver,"Tillbakakaka"} = receive_any(),
    ?line [{key,2}] = ets:lookup(T,key),
    ?line {'DOWN', Mref, process, Receiver, normal} = receive_any(),

    %% Give and then let receiver keep it
    ?line true = ets:insert(T,{key,1}),
    ?line {Receiver3,Mref3} = my_spawn_monitor(fun()-> give_away_receiver(T,Parent) end),
    ?line give_me = receive_any(),
    ?line true = ets:give_away(T,Receiver3,here_you_are),
    ?line {'EXIT',{badarg,_}} = (catch ets:lookup(T,key)),
    ?line Receiver3 ! die_please,
    ?line {'DOWN', Mref3, process, Receiver3, normal} = receive_any(),
    ?line undefined = ets:info(T),

    %% Give and then kill receiver to get back
    ?line T2 = ets_new(foo,[private | Opts]),
    ?line true = ets:insert(T2,{key,1}),
    ?line ets:setopts(T2,{heir,self(),"Som en gummiboll..."}),
    ?line {Receiver2,Mref2} = my_spawn_monitor(fun()-> give_away_receiver(T2,Parent) end),
    ?line give_me = receive_any(),
    ?line true = ets:give_away(T2,Receiver2,here_you_are),
    ?line {'EXIT',{badarg,_}} = (catch ets:lookup(T2,key)),
    ?line Receiver2 ! die_please,
    ?line {'ETS-TRANSFER',T2,Receiver2,"Som en gummiboll..."} = receive_any(),
    ?line [{key,2}] = ets:lookup(T2,key),
    ?line {'DOWN', Mref2, process, Receiver2, normal} = receive_any(),

    %% Some negative testing
    ?line {'EXIT',{badarg,_}} = (catch ets:give_away(T2,Receiver,"To a dead one")),
    ?line {'EXIT',{badarg,_}} = (catch ets:give_away(T2,self(),"To myself")),
    ?line {'EXIT',{badarg,_}} = (catch ets:give_away(T2,"not a pid","To wrong type")),

    ?line true = ets:delete(T2),
    ?line {ReceiverNeg,MrefNeg} = my_spawn_monitor(fun()-> give_away_receiver(T2,Parent) end),
    ?line give_me = receive_any(),
    ?line {'EXIT',{badarg,_}} = (catch ets:give_away(T2,ReceiverNeg,"A deleted table")),

    ?line T3 = ets_new(foo,[public | Opts]),
    my_spawn_link(fun()-> {'EXIT',{badarg,_}} = (catch ets:give_away(T3,ReceiverNeg,"From non owner")),
		       Parent ! done
	       end),
    ?line done = receive_any(),
    ?line ReceiverNeg ! no_soup_for_you,
    ?line {'DOWN', MrefNeg, process, ReceiverNeg, normal} = receive_any(),
    ok.

give_away_receiver(T, Giver) ->
    ?line {'EXIT',{badarg,_}} = (catch ets:lookup(T,key)),
    ?line Giver ! give_me,
    ?line case receive_any() of
	      {'ETS-TRANSFER',T,Giver,here_you_are} ->
		  ?line [{key,1}] = ets:lookup(T,key),
		  ?line true = ets:insert(T,{key,2}),
		  ?line case receive_any() of
			    give_back ->
				?line true = ets:give_away(T,Giver,"Tillbakakaka"),
				?line {'EXIT',{badarg,_}} = (catch ets:lookup(T,key));
			    die_please ->
				ok
			end;
	      no_soup_for_you ->
		  ok
	  end.


setopts(doc) -> ["ets:setopts/2"];
setopts(suite) -> [];
setopts(Config) when is_list(Config) ->
    repeat_for_opts(setopts_do,[write_concurrency,all_types]).

setopts_do(Opts) ->
    Self = self(),
    ?line T = ets_new(foo,[named_table, private | Opts]),
    ?line none = ets:info(T,heir),
    Heir = my_spawn_link(fun()->heir_heir(Self) end),
    ?line ets:setopts(T,{heir,Heir,"Data"}),
    ?line Heir = ets:info(T,heir),
    ?line ets:setopts(T,{heir,self(),"Data"}),
    ?line Self = ets:info(T,heir),
    ?line ets:setopts(T,[{heir,Heir,"Data"}]),
    ?line Heir = ets:info(T,heir),
    ?line ets:setopts(T,[{heir,none}]),
    ?line none = ets:info(T,heir),

    ?line {'EXIT',{badarg,_}} = (catch ets:setopts(T,[{heir,self(),"Data"},false])),
    ?line {'EXIT',{badarg,_}} = (catch ets:setopts(T,{heir,self()})),
    ?line {'EXIT',{badarg,_}} = (catch ets:setopts(T,{heir,false})),
    ?line {'EXIT',{badarg,_}} = (catch ets:setopts(T,heir)),
    ?line {'EXIT',{badarg,_}} = (catch ets:setopts(T,{heir,false,"Data"})),
    ?line {'EXIT',{badarg,_}} = (catch ets:setopts(T,{false,self(),"Data"})),

    ?line ets:setopts(T,{protection,protected}),
    ?line ets:setopts(T,{protection,public}),
    ?line ets:setopts(T,{protection,private}),
    ?line ets:setopts(T,[{protection,protected}]),
    ?line ets:setopts(T,[{protection,public}]),
    ?line ets:setopts(T,[{protection,private}]),

    ?line {'EXIT',{badarg,_}} = (catch ets:setopts(T,{protection})),
    ?line {'EXIT',{badarg,_}} = (catch ets:setopts(T,{protection,false})),
    ?line {'EXIT',{badarg,_}} = (catch ets:setopts(T,{protection,private,false})),
    ?line {'EXIT',{badarg,_}} = (catch ets:setopts(T,protection)),
    ?line ets:delete(T),
    unlink(Heir),
    exit(Heir, bang),
    ok.

bad_table(doc) -> ["All kinds of operations with bad table argument"];
bad_table(suite) -> [];
bad_table(Config) when is_list(Config) ->

    %% Open and close disk_log to stabilize etsmem.
    Name = make_ref(),
    ?line File = filename:join([?config(priv_dir, Config),"bad_table.dummy"]),
    ?line {ok, Name} = disk_log:open([{name, Name}, {file, File}]),
    ?line disk_log:close(Name),
    file:delete(File),

    ?line EtsMem = etsmem(),

    repeat_for_opts(fun(Opts) -> bad_table_do(Opts,File) end,
		    [write_concurrency, all_types]),
    ?line verify_etsmem(EtsMem),
    ok.

bad_table_do(Opts, DummyFile) ->
    Parent = self(),    
    {Pid,Mref} = my_spawn_opt(fun()-> ets_new(priv,[private,named_table | Opts]),
				      Priv = ets_new(priv,[private | Opts]),
				      ets_new(prot,[protected,named_table | Opts]),
				      Prot = ets_new(prot,[protected | Opts]),
				      Parent ! {self(),Priv,Prot},
				      die_please = receive_any()
			      end,
			      [link, monitor]),
    {Pid,Priv,Prot} = receive_any(),
    MatchSpec = {{key,'_'}, [], ['$$']},
    Fun = fun(X,_) -> X end,
    OpList = [{delete,[key],update},
	      {delete_all_objects,[],update},
	      {delete_object,[{key,data}],update},
	      {first,[],read},
	      {foldl,[Fun, 0], read, tabarg_last},
	      {foldr,[Fun, 0], read, tabarg_last},
	      %%{from_dets,[DetsTab], update},
	      {give_away,[Pid, data], update},
	      %%{info, [], read},
	      %%{info, [safe_fixed], read},
	      %%{init_table,[Name, InitFun],update},
	      {insert, [{key,data}], update},
	      {insert_new, [{key,data}], update},
	      {insert_new, [[{key,data},{other,data}]], update},
	      {last, [], read},
	      {lookup, [key], read},
	      {lookup_element, [key, 2], read},
	      {match, [{}], read},
	      {match, [{},17], read},
	      {match_delete, [{}], update},
	      {match_object, [{}], read},
	      {match_object, [{},17], read},
	      {member,[key], read},
	      {next, [key], read},
	      {prev, [key], read},
	      {rename, [new_name], update},
	      {safe_fixtable, [true], read},
	      {select,[MatchSpec], read},
	      {select,[MatchSpec,17], read},
	      {select_count,[MatchSpec], read},
	      {select_delete,[MatchSpec], update},
	      {setopts, [{heir,none}], update},
	      {slot, [0], read},
	      {tab2file, [DummyFile], read, {return,{error,badtab}}},
	      {tab2file, [DummyFile,[]], read, {return,{error,badtab}}},
	      {tab2list, [], read},
	      %%{table,[], read},
	      %%{to_dets, [DetsTab], read},
	      {update_counter,[key,1], update},
	      {update_element,[key,{2,new_data}], update}
	     ],
    Info = {Opts, Priv, Prot},
    lists:foreach(fun(Op) -> bad_table_op(Info, Op) end,
     		  OpList),
    Pid ! die_please,
    {'DOWN', Mref, process, Pid, normal} = receive_any(),
    ok.

bad_table_op({Opts,Priv,Prot}, Op) ->
    %%io:format("Doing Op=~p on ~p's\n",[Op,Type]),
    T1 = ets_new(noname,Opts),
    bad_table_call(noname,Op),
    ets:delete(T1),
    bad_table_call(T1,Op),
    T2 = ets_new(named,[named_table | Opts]),
    ets:delete(T2),
    bad_table_call(named,Op),
    bad_table_call(T2,Op),
    bad_table_call(priv,Op),
    bad_table_call(Priv,Op),
    case element(3,Op) of
	update ->
	    bad_table_call(prot,Op),
	    bad_table_call(Prot,Op);
	read -> ok
    end.

bad_table_call(T,{F,Args,_}) ->
    ?line {'EXIT',{badarg,_}} = (catch apply(ets, F, [T|Args]));
bad_table_call(T,{F,Args,_,tabarg_last}) ->
    ?line {'EXIT',{badarg,_}} = (catch apply(ets, F, Args++[T]));
bad_table_call(T,{F,Args,_,{return,Return}}) ->
    try
	?line Return = apply(ets, F, [T|Args])
    catch
	error:badarg -> ok
    end.


rename(doc) ->
    ["Check rename of ets tables"];
rename(suite) ->
    [];
rename(Config) when is_list(Config) ->
    repeat_for_opts(rename_do, [write_concurrency, all_types]).

rename_do(Opts) ->
    ?line EtsMem = etsmem(),
    ets_new(foobazz,[named_table, public | Opts]),
    ets:insert(foobazz,{foo,bazz}),
    ungermanbazz = ets:rename(foobazz,ungermanbazz),
    {'EXIT',{badarg, _}} = (catch ets:lookup(foobazz,foo)),
    [{foo,bazz}] = ets:lookup(ungermanbazz,foo),
    {'EXIT',{badarg,_}} =  (catch ets:rename(ungermanbazz,"no atom")),
    ets:delete(ungermanbazz),
    ?line verify_etsmem(EtsMem).

rename_unnamed(doc) ->
    ["Check rename of unnamed ets table"];
rename_unnamed(suite) ->
    [];
rename_unnamed(Config) when is_list(Config) ->
    repeat_for_opts(rename_unnamed_do,[write_concurrency,all_types]).

rename_unnamed_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line Tab = ets_new(bonkz,[public | Opts]),
    ?line {'EXIT',{badarg, _}} = (catch ets:insert(bonkz,{foo,bazz})),
    ?line bonkz = ets:info(Tab, name),
    ?line Tab = ets:rename(Tab, tjabonkz),
    ?line {'EXIT',{badarg, _}} = (catch ets:insert(tjabonkz,{foo,bazz})),
    ?line tjabonkz = ets:info(Tab, name),
    ?line ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

evil_rename(doc) ->
    "Rename a table with many fixations, and at the same time delete it.";
evil_rename(Config) when is_list(Config) ->
    ?line evil_rename_1(old_hash, new_hash, [public,named_table]),
    ?line EtsMem = etsmem(),
    ?line evil_rename_1(old_tree, new_tree, [public,ordered_set,named_table]),
    ?line verify_etsmem(EtsMem).

evil_rename_1(Old, New, Flags) ->
    ?line process_flag(trap_exit, true),
    ?line Old = ets_new(Old, Flags),
    ?line Fixer = fun() -> ets:safe_fixtable(Old, true) end,
    ?line crazy_fixtable(15000, Fixer),
    ?line erlang:yield(),
    ?line New = ets:rename(Old, New),
    ?line erlang:yield(),
    ets:delete(New),
    ok.

crazy_fixtable(N, Fixer) ->
    Dracula = ets_new(count_dracula, [public]),
    ets:insert(Dracula, {count,0}),
    SpawnFun = fun() ->
		       Fixer(),
		       case ets:update_counter(Dracula, count, 1) rem 15 of
			   0 -> evil_creater_destroyer();
			   _ -> erlang:hibernate(erlang, error, [dont_wake_me])
		       end
	       end,
    crazy_fixtable_1(N, SpawnFun),
    crazy_fixtable_wait(N, Dracula),
    Dracula.

crazy_fixtable_wait(N, Dracula) ->
    case ets:lookup(Dracula, count) of
	[{count,N}] ->
	    ets:delete(Dracula);
	Other ->
	    io:format("~p\n", [Other]),
	    receive after 10 -> ok end,
	    crazy_fixtable_wait(N, Dracula)
    end.

crazy_fixtable_1(0, _) ->
    ok;
crazy_fixtable_1(N, Fun) ->
    spawn_link(Fun),
    crazy_fixtable_1(N-1, Fun).

evil_creater_destroyer() ->
    T1 = evil_create_fixed_tab(),
    ets:delete(T1).

evil_create_fixed_tab() ->
    T = ets_new(arne, [public]),
    ets:safe_fixtable(T, true),
    T.

interface_equality(doc) ->
    ["Tests that the return values and errors are equal for set's and"
     " ordered_set's where applicable"];
interface_equality(suite) ->
    [];
interface_equality(Config) when is_list(Config) ->
    repeat_for_opts(interface_equality_do).

interface_equality_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line Set = ets_new(set,[set | Opts]),
    ?line OrderedSet = ets_new(ordered_set,[ordered_set | Opts]),
    ?line F = fun(X,T,FF) -> case X of 
			   0 -> true; 
			   _ -> 
			       ets:insert(T, {X,
					      integer_to_list(X),
					      X rem 10}),
			       FF(X-1,T,FF) 
		       end 
	end,
    ?line F(100,Set,F),
    ?line F(100,OrderedSet,F),
    ?line equal_results(ets, insert, Set, OrderedSet, [{a,"a"}]),
    ?line equal_results(ets, insert, Set, OrderedSet, [{1,1,"1"}]),
    ?line equal_results(ets, lookup, Set, OrderedSet, [10]),
    ?line equal_results(ets, lookup, Set, OrderedSet, [1000]),
    ?line equal_results(ets, delete, Set, OrderedSet, [10]),
    ?line equal_results(ets, delete, Set, OrderedSet, [nott]),
    ?line equal_results(ets, lookup, Set, OrderedSet, [1000]),
    ?line equal_results(ets, insert, Set, OrderedSet, [10]),
    ?line equal_results(ets, next, Set, OrderedSet, ['$end_of_table']),
    ?line equal_results(ets, prev, Set, OrderedSet, ['$end_of_table']),
    ?line equal_results(ets, match, Set, OrderedSet, [{'_','_','_'}]),
    ?line equal_results(ets, match, Set, OrderedSet, [{'_','_','_','_'}]),
    ?line equal_results(ets, match, Set, OrderedSet, [{$3,$2,2}]),
    ?line equal_results(ets, match, Set, OrderedSet, ['_']),
    ?line equal_results(ets, match, Set, OrderedSet, ['$1']),
    ?line equal_results(ets, match, Set, OrderedSet, [{'_','$50',3}]),
    ?line equal_results(ets, match, Set, OrderedSet, [['_','$50',3]]),
    ?line equal_results(ets, match_delete, Set, OrderedSet, [{'_','_',4}]),
    ?line equal_results(ets, match_delete, Set, OrderedSet, [{'_','_',4}]),
    ?line equal_results(ets, match_object, Set, OrderedSet, [{'_','_',4}]),
    ?line equal_results(ets, match_object, Set, OrderedSet, [{'_','_',5}]),
    ?line equal_results(ets, match_object, Set, OrderedSet, [{'_','_',4}]),
    ?line equal_results(ets, match_object, Set, OrderedSet, ['_']),
    ?line equal_results(ets, match_object, Set, OrderedSet, ['$5011']),
    ?line equal_results(ets, match_delete, Set, OrderedSet, ['$20']),
    ?line equal_results(ets, lookup_element, Set, OrderedSet, [13,2]),
    ?line equal_results(ets, lookup_element, Set, OrderedSet, [13,4]),
    ?line equal_results(ets, lookup_element, Set, OrderedSet, [14,2]),
    ?line equal_results(ets, delete, Set, OrderedSet, []),
    ?line verify_etsmem(EtsMem).

equal_results(M, F, FirstArg1, FirstArg2 ,ACommon) ->
    Res = maybe_sort((catch apply(M,F, [FirstArg1 | ACommon]))),
    Res = maybe_sort((catch apply(M,F,[FirstArg2 | ACommon]))).

maybe_sort(L) when is_list(L) ->
    lists:sort(L);
%maybe_sort({'EXIT',{Reason, [{Module, Function, _}|_]}}) ->
%    {'EXIT',{Reason, [{Module, Function, '_'}]}};
maybe_sort({'EXIT',{Reason, List}}) when is_list(List) ->
    {'EXIT',{Reason, lists:map(fun({Module, Function, _, _}) ->
				       {Module, Function, '_'}
			       end,
			       List)}};
maybe_sort(Any) ->
    Any.

ordered_match(doc) ->
    ["Test match, match_object and match_delete in ordered set's"];
ordered_match(suite) ->
    [];
ordered_match(Config) when is_list(Config)->
    repeat_for_opts(ordered_match_do).

ordered_match_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line F = fun(X,T,FF) -> case X of 
			 0 -> true; 
			 _ -> 
			     ets:insert(T, {X,
					    integer_to_list(X), 
					    X rem 10,
					    X rem 100,
					    X rem 1000}), 
			     FF(X-1,T,FF) 
		     end 
	end,
    ?line T1 = ets_new(xxx,[ordered_set| Opts]),
    ?line F(3000,T1,F),
    ?line [[3,3],[3,3],[3,3]] = ets:match(T1, {'_','_','$1','$2',3}),
    ?line F2 = fun(X,Rem,Res,FF) -> case X of 
				  0 -> []; 
				  _ -> 
				      case X rem Rem of
					  Res ->
					      FF(X-1,Rem,Res,FF) ++
						  [{X,
						    integer_to_list(X), 
						    X rem 10,
						    X rem 100,
						    X rem 1000}];
					  _ ->
					      FF(X-1,Rem,Res,FF)
				      end
			      end 
	 end,
    ?line OL1 = F2(3000,100,2,F2),
    ?line OL1 = ets:match_object(T1, {'_','_','_',2,'_'}),
    ?line true = ets:match_delete(T1,{'_','_','_',2,'_'}), 
    ?line [] = ets:match_object(T1, {'_','_','_',2,'_'}),
    ?line OL2 = F2(3000,100,3,F2),
    ?line OL2 = ets:match_object(T1, {'_','_','_',3,'_'}),
    ?line ets:delete(T1),
    ?line verify_etsmem(EtsMem).
    

ordered(doc) ->
    ["Test basic functionality in ordered_set's."];
ordered(suite) ->
    [];
ordered(Config) when is_list(Config) ->
    repeat_for_opts(ordered_do).

ordered_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line T = ets_new(oset, [ordered_set | Opts]),
    ?line InsList = [ 
		      25,26,27,28,
		      5,6,7,8,
		      21,22,23,24,
		      9,10,11,12,
		      1,2,3,4,
		      17,18,19,20,
		      13,14,15,16,
		      1 bsl 33
		     ],
    ?line lists:foreach(fun(X) ->
			  ets:insert(T,{X,integer_to_list(X)})
		  end,
		  InsList),
    ?line IL2 = lists:map(fun(X) -> {X,integer_to_list(X)} end, InsList),
    ?line L1 = pick_all_forward(T),
    ?line L2 = pick_all_backwards(T),
    ?line S1 = lists:sort(IL2),
    ?line S2 = lists:reverse(lists:sort(IL2)),
    ?line S1 = L1,
    ?line S2 = L2,
    ?line [{1,"1"}] = ets:slot(T,0),
    ?line [{28,"28"}] = ets:slot(T,27),
    ?line [{1 bsl 33,_}] = ets:slot(T,28),
    ?line 27 = ets:prev(T,28),
    ?line [{7,"7"}] = ets:slot(T,6),
    ?line '$end_of_table' = ets:next(T,1 bsl 33),
    ?line [{12,"12"}] = ets:slot(T,11),
    ?line '$end_of_table' = ets:slot(T,29),
    ?line [{1,"1"}] = ets:slot(T,0),
    ?line 28 = ets:prev(T,1 bsl 33),
    ?line 1 = ets:next(T,0),
    ?line pick_all_forward(T),
    ?line [{7,"7"}] = ets:slot(T,6),
    ?line L2 = pick_all_backwards(T),
    ?line [{7,"7"}] = ets:slot(T,6),
    ?line ets:delete(T),
    ?line verify_etsmem(EtsMem).

pick_all(_T,'$end_of_table',_How) ->
    [];
pick_all(T,Last,How) ->
    ?line This = case How of
	       next ->
		   ?line ets:next(T,Last);
	       prev ->
		   ?line ets:prev(T,Last)
	   end,
    ?line [LastObj] = ets:lookup(T,Last),
    ?line [LastObj | pick_all(T,This,How)].

pick_all_forward(T) ->
    ?line pick_all(T,ets:first(T),next).
pick_all_backwards(T) ->
    ?line pick_all(T,ets:last(T),prev).
    
    

setbag(doc) ->   ["Small test case for both set and bag type ets tables."];
setbag(suite) -> [];
setbag(Config) when is_list(Config) ->    
    ?line EtsMem = etsmem(),
    ?line Set = ets_new(set,[set]),
    ?line Bag = ets_new(bag,[bag]),
    ?line Key = {foo,bar},
    
    %% insert some value
    ?line ets:insert(Set,{Key,val1}),
    ?line ets:insert(Bag,{Key,val1}),
    
    %% insert new value for same key again
    ?line ets:insert(Set,{Key,val2}),
    ?line ets:insert(Bag,{Key,val2}),
    
    %% check
    ?line [{Key,val2}] = ets:lookup(Set,Key),
    ?line [{Key,val1},{Key,val2}] = ets:lookup(Bag,Key),

    true = ets:delete(Set),
    true = ets:delete(Bag),
    ?line verify_etsmem(EtsMem).

badnew(doc) ->
    ["Test case to check proper return values for illegal ets_new() calls."];
badnew(suite) -> [];
badnew(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    ?line {'EXIT',{badarg,_}} = (catch ets_new(12,[])),
    ?line {'EXIT',{badarg,_}} = (catch ets_new({a,b},[])),
    ?line {'EXIT',{badarg,_}} = (catch ets_new(name,[foo])),
    ?line {'EXIT',{badarg,_}} = (catch ets_new(name,{bag})),
    ?line {'EXIT',{badarg,_}} = (catch ets_new(name,bag)),
    ?line verify_etsmem(EtsMem).

verybadnew(doc) ->
    ["Test case to check that a not well formed list does not crash the "
     "emulator. OTP-2314 "];
verybadnew(suite) -> [];
verybadnew(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    ?line {'EXIT',{badarg,_}} = (catch ets_new(verybad,[set|protected])),
    ?line verify_etsmem(EtsMem).

named(doc) ->   ["Small check to see if named tables work."];
named(suite) -> [];
named(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    ?line Tab = make_table(foo,
			   [named_table],
			   [{key,val}]),
    ?line [{key,val}] = ets:lookup(foo,key),
    ?line true = ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

keypos2(doc) ->   ["Test case to check if specified keypos works."];
keypos2(suite) -> [];
keypos2(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    ?line Tab = make_table(foo,
			   [set,{keypos,2}],
			   [{val,key}, {val2,key}]),
    ?line [{val2,key}] = ets:lookup(Tab,key),
    ?line true = ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

privacy(doc) ->
    ["Privacy check. Check that a named(public/private/protected) table "
     "cannot be read by",
     "the wrong process(es)."];
privacy(suite) -> [];
privacy(Config) when is_list(Config) ->
    repeat_for_opts(privacy_do).

privacy_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line process_flag(trap_exit,true),
    ?line Owner = my_spawn_link(?MODULE,privacy_owner,[self(),Opts]),
    receive
	{'EXIT',Owner,Reason} ->
	    ?line exit({privacy_test,Reason});
	ok ->
	    ok
    end,

    privacy_check(pub,prot,priv),

    Owner ! {shift,1,{pub,prot,priv}},   
    receive {Pub1,Prot1,Priv1} -> ok end,   
    privacy_check(Pub1,Prot1,Priv1),

    Owner ! {shift,2,{Pub1,Prot1,Priv1}},   
    receive {Pub2,Prot2,Priv2} -> ok end,   
    privacy_check(Pub2,Prot2,Priv2),

    Owner ! {shift,0,{Pub2,Prot2,Priv2}},   
    receive {Pub2,Prot2,Priv2} -> ok end,   
    privacy_check(Pub2,Prot2,Priv2),

    Owner ! die,
    receive {'EXIT',Owner,_} -> ok end,
    ?line verify_etsmem(EtsMem).

privacy_check(Pub,Prot,Priv) ->
    %% check read rights
    ?line [] = ets:lookup(Pub, foo),
    ?line [] = ets:lookup(Prot,foo),
    ?line {'EXIT',{badarg,_}} = (catch ets:lookup(Priv,foo)),

    %% check write rights
    ?line true = ets:insert(Pub, {1,foo}),
    ?line {'EXIT',{badarg,_}} = (catch ets:insert(Prot,{2,foo})),
    ?line {'EXIT',{badarg,_}} = (catch ets:insert(Priv,{3,foo})),

    %% check that it really wasn't written, either
    ?line [] = ets:lookup(Prot,foo).

privacy_owner(Boss, Opts) ->
    ets_new(pub, [public,named_table | Opts]),
    ets_new(prot,[protected,named_table | Opts]),
    ets_new(priv,[private,named_table | Opts]),
    Boss ! ok,
    privacy_owner_loop(Boss).

privacy_owner_loop(Boss) ->
    receive
	{shift,N,Pub_Prot_Priv} ->
	    {Pub,Prot,Priv} = rotate_tuple(Pub_Prot_Priv, N),
	    
	    ets:setopts(Pub,{protection,public}),
	    ets:setopts(Prot,{protection,protected}),
	    ets:setopts(Priv,{protection,private}),
	    Boss ! {Pub,Prot,Priv},
	    privacy_owner_loop(Boss);

	die -> ok
    end.

rotate_tuple(Tuple, 0) ->
    Tuple;
rotate_tuple(Tuple, N) ->
    [H|T] = tuple_to_list(Tuple),
    rotate_tuple(list_to_tuple(T ++ [H]), N-1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


empty(doc) ->
    ["Check lookup in an empty table and lookup of a non-existing key"];
empty(suite) -> [];
empty(Config) when is_list(Config) ->
    repeat_for_opts(empty_do).

empty_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line Tab = ets_new(foo,Opts),
    ?line [] = ets:lookup(Tab,key),
    ?line true = ets:insert(Tab,{key2,val}),
    ?line [] = ets:lookup(Tab,key),
    ?line true = ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

badinsert(doc) ->
    ["Check proper return values for illegal insert operations."];
badinsert(suite) -> [];
badinsert(Config) when is_list(Config) ->
    repeat_for_opts(badinsert_do).

badinsert_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line {'EXIT',{badarg,_}} = (catch ets:insert(foo,{key,val})),
    
    ?line Tab = ets_new(foo,Opts),
    ?line {'EXIT',{badarg,_}} = (catch ets:insert(Tab,{})),

    ?line Tab3 = ets_new(foo,[{keypos,3}| Opts]),
    ?line {'EXIT',{badarg,_}} = (catch ets:insert(Tab3,{a,b})),

    ?line {'EXIT',{badarg,_}} = (catch ets:insert(Tab,[key,val2])),
    ?line true = ets:delete(Tab),
    ?line true = ets:delete(Tab3),
    ?line verify_etsmem(EtsMem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


time_lookup(doc) ->   ["Lookup timing."];
time_lookup(suite) -> [];
time_lookup(Config) when is_list(Config) ->
    %% just for timing, really
    ?line EtsMem = etsmem(),
    Values = repeat_for_opts(time_lookup_do),
    ?line verify_etsmem(EtsMem),
    ?line {comment,lists:flatten(io_lib:format(
				   "~p ets lookups/s",[Values]))}.

time_lookup_do(Opts) ->
    ?line Tab = ets_new(foo,Opts),
    ?line fill_tab(Tab,foo),
    ?line ets:insert(Tab,{{a,key},foo}),
    ?line {Time,_} = ?t:timecall(test_server,do_times,
				    [10000,ets,lookup,[Tab,{a,key}]]),
    ?line true = ets:delete(Tab),
    round(10000 / Time). % lookups/s

badlookup(doc) ->
    ["Check proper return values from bad lookups in existing/non existing "
     " ets tables"];
badlookup(suite) -> [];
badlookup(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    ?line {'EXIT',{badarg,_}} = (catch ets:lookup(foo,key)),
    ?line Tab = ets_new(foo,[]),
    ?line ets:delete(Tab),
    ?line {'EXIT',{badarg,_}} = (catch ets:lookup(Tab,key)),
    ?line verify_etsmem(EtsMem).

lookup_order(doc) ->   ["Test that lookup returns objects in order of insertion for bag and dbag."];
lookup_order(suite) -> [];
lookup_order(Config) when is_list(Config) ->  
    EtsMem = etsmem(),
    repeat_for_opts(lookup_order_do, [write_concurrency,[bag,duplicate_bag]]),
    ?line verify_etsmem(EtsMem),
    ok.

lookup_order_do(Opts) ->
    lookup_order_2(Opts, false),
    lookup_order_2(Opts, true).

lookup_order_2(Opts, Fixed) ->
    io:format("Opts=~p Fixed=~p\n",[Opts,Fixed]),

    A = 1, B = 2, C = 3,
    ABC = [A,B,C],
    Pair = [{A,B},{B,A},{A,C},{C,A},{B,C},{C,B}],
    Combos = [{D1,D2,D3} || D1<-ABC, D2<-Pair, D3<-Pair],
    lists:foreach(fun({D1,{D2a,D2b},{D3a,D3b}}) ->
			  T = ets_new(foo,Opts),
			  case Fixed of
			      true -> ets:safe_fixtable(T,true);
			      false -> ok
			  end,			  
			  S10 = {T,[],key},
			  S20 = check_insert(S10,A),
			  S30 = check_insert(S20,B),
			  S40 = check_insert(S30,C),
			  S50 = check_delete(S40,D1),
			  S55 = check_insert(S50,D1),
			  S60 = check_insert(S55,D1),
			  S70 = check_delete(S60,D2a),
			  S80 = check_delete(S70,D2b),
			  S90 = check_insert(S80,D2a),
			  SA0 = check_delete(S90,D3a),
			  SB0 = check_delete(SA0,D3b),			  
			  check_insert_new(SB0,D3b),

			  true = ets:delete(T)
		  end,
		  Combos).
			  

check_insert({T,List0,Key},Val) ->
    %%io:format("insert ~p into ~p\n",[Val,List0]),
    ets:insert(T,{Key,Val}),
    List1 = case (ets:info(T,type) =:= bag andalso
		  lists:member({Key,Val},List0)) of
		true -> List0;		
		false -> [{Key,Val} | List0]
	    end,
    check_check({T,List1,Key}).

check_insert_new({T,List0,Key},Val) ->
    %%io:format("insert_new ~p into ~p\n",[Val,List0]),
    Ret = ets:insert_new(T,{Key,Val}),
    ?line Ret = (List0 =:= []),
    List1 = case Ret of
		true -> [{Key,Val}];
		false -> List0
	    end,
    check_check({T,List1,Key}).


check_delete({T,List0,Key},Val) ->
    %%io:format("delete ~p from ~p\n",[Val,List0]),
    ets:delete_object(T,{Key,Val}),
    List1 = lists:filter(fun(Obj) -> Obj =/= {Key,Val} end,
			 List0),
    check_check({T,List1,Key}).

check_check(S={T,List,Key}) ->
    case lists:reverse(ets:lookup(T,Key)) of
	List -> ok;
        ETS -> io:format("check failed:\nETS: ~p\nCHK: ~p\n", [ETS,List]),
	       ?t:fail("Invalid return value from ets:lookup")
    end,
    ?line Items = ets:info(T,size),
    ?line Items = length(List),
    S.
    


fill_tab(Tab,Val) ->
    ?line ets:insert(Tab,{key,Val}),
    ?line ets:insert(Tab,{{a,144},Val}),
    ?line ets:insert(Tab,{{a,key2},Val}),
    ?line ets:insert(Tab,{14,Val}),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


lookup_element_mult(doc) ->   ["Multiple return elements (OTP-2386)"];
lookup_element_mult(suite) -> [];
lookup_element_mult(Config) when is_list(Config) ->
    repeat_for_opts(lookup_element_mult_do).

lookup_element_mult_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line T = ets_new(service, [bag, {keypos, 2} | Opts]),
    ?line D = lists:reverse(lem_data()),
    ?line lists:foreach(fun(X) -> ets:insert(T, X) end, D),
    ?line ok = lem_crash_3(T),
    ?line ets:insert(T, {0, "heap_key"}),
    ?line ets:lookup_element(T, "heap_key", 2),
    ?line true = ets:delete(T),
    ?line verify_etsmem(EtsMem).

lem_data() ->
    [
     {service,'eddie2@boromir',{150,236,14,103},httpd88,self()},
     {service,'eddie2@boromir',{150,236,14,103},httpd80,self()},
     {service,'eddie3@boromir',{150,236,14,107},httpd88,self()},
     {service,'eddie3@boromir',{150,236,14,107},httpd80,self()},
     {service,'eddie4@boromir',{150,236,14,108},httpd88,self()}
    ].

lem_crash(T) ->
    L = ets:lookup_element(T, 'eddie2@boromir', 3),
    {erlang:phash(L, 256), L}.

lem_crash_3(T) ->
    lem_crash(T),
    io:format("Survived once~n"),
    lem_crash(T),
    io:format("Survived twice~n"),
    lem_crash(T),
    io:format("Survived all!~n"),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


delete_elem(doc) ->
    ["Check delete of an element inserted in a `filled' table."];
delete_elem(suite) -> [];
delete_elem(Config) when is_list(Config) ->
    repeat_for_opts(delete_elem_do, [write_concurrency, all_types]).

delete_elem_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line Tab = ets_new(foo,Opts),
    ?line fill_tab(Tab,foo),
    ?line ets:insert(Tab,{{b,key},foo}),
    ?line ets:insert(Tab,{{c,key},foo}),
    ?line true = ets:delete(Tab,{b,key}),
    ?line [] = ets:lookup(Tab,{b,key}),
    ?line [{{c,key},foo}] = ets:lookup(Tab,{c,key}),
    ?line true = ets:delete(Tab),
    ?line verify_etsmem(EtsMem).
    
delete_tab(doc) ->
    ["Check that ets:delete() works and releases the name of the deleted "
     "table."];
delete_tab(suite) -> [];
delete_tab(Config) when is_list(Config) ->
    repeat_for_opts(delete_tab_do,[write_concurrency,all_types]).

delete_tab_do(Opts) ->
    Name = foo,
    ?line EtsMem = etsmem(),
    ?line Name = ets_new(Name, [named_table | Opts]),
    ?line true = ets:delete(foo),
    %% The name should be available again.
    ?line Name = ets_new(Name, [named_table | Opts]),
    ?line true = ets:delete(Name),
    ?line verify_etsmem(EtsMem).

delete_large_tab(doc) ->
    "Check that ets:delete/1 works and that other processes can run.";
delete_large_tab(Config) when is_list(Config) ->
    ?line Data = [{erlang:phash2(I, 16#ffffff),I} || I <- lists:seq(1, 200000)],
    ?line EtsMem = etsmem(),
    repeat_for_opts(fun(Opts) -> delete_large_tab_do(Opts,Data) end),
    ?line verify_etsmem(EtsMem).

delete_large_tab_do(Opts,Data) ->
    ?line delete_large_tab_1(foo_hash, Opts, Data, false),
    ?line delete_large_tab_1(foo_tree, [ordered_set | Opts], Data, false),
    ?line delete_large_tab_1(foo_hash, Opts, Data, true).


delete_large_tab_1(Name, Flags, Data, Fix) ->
    ?line Tab = ets_new(Name, Flags),
    ?line ets:insert(Tab, Data),

    case Fix of
	false -> ok;
	true ->
	    ?line true = ets:safe_fixtable(Tab, true),
	    ?line lists:foreach(fun({K,_}) -> ets:delete(Tab, K) end, Data)
    end,

    {priority, Prio} = process_info(self(), priority),	    
    ?line Deleter = self(),
    ?line [SchedTracer]
	= start_loopers(1,
			Prio,
			fun (SC) ->
				receive
				    {trace, Deleter, out, _} ->
					undefined = ets:info(Tab),
					SC+1;
				    {trace,
				     Deleter,
				     register,
				     delete_large_tab_done_marker}->
					Deleter ! {schedule_count, SC},
					exit(normal);
				    _ ->
					SC
				end
			end,
			0),
    SchedTracerMon = monitor(process, SchedTracer),
    ?line Loopers = start_loopers(erlang:system_info(schedulers),
				  Prio,
				  fun (_) -> erlang:yield() end,
				  ok),
    ?line erlang:yield(),
    ?line 1 = erlang:trace(self(),true,[running,procs,{tracer,SchedTracer}]),
    ?line true = ets:delete(Tab),
    %% The register stuff is just a trace marker
    ?line true = register(delete_large_tab_done_marker, self()),
    ?line true = unregister(delete_large_tab_done_marker),
    ?line undefined = ets:info(Tab),
    ?line ok = stop_loopers(Loopers),
    ?line receive
	      {schedule_count, N} ->
		  ?line io:format("~s: context switches: ~p", [Name,N]),
		  if
		      N >= 5 -> ?line ok;
		      true -> ?line ?t:fail()
		  end
	  end,
    receive {'DOWN',SchedTracerMon,process,SchedTracer,_} -> ok end,
    ok.

delete_large_named_table(doc) ->
    "Delete a large name table and try to create a new table with the same name in another process.";
delete_large_named_table(Config) when is_list(Config) ->    
    ?line Data = [{erlang:phash2(I, 16#ffffff),I} || I <- lists:seq(1, 200000)],
    ?line EtsMem = etsmem(),
    repeat_for_opts(fun(Opts) -> delete_large_named_table_do(Opts,Data) end),
    ?line verify_etsmem(EtsMem),
    ok.

delete_large_named_table_do(Opts,Data) ->
    ?line delete_large_named_table_1(foo_hash, [named_table | Opts], Data, false),
    ?line delete_large_named_table_1(foo_tree, [ordered_set,named_table | Opts], Data, false),
    ?line delete_large_named_table_1(foo_hash, [named_table | Opts], Data, true).

delete_large_named_table_1(Name, Flags, Data, Fix) ->
    ?line Tab = ets_new(Name, Flags),
    ?line ets:insert(Tab, Data),

    case Fix of
	false -> ok;
	true ->
	    ?line true = ets:safe_fixtable(Tab, true),
	    ?line lists:foreach(fun({K,_}) -> ets:delete(Tab, K) end, Data)
    end,
    Parent = self(),
    {Pid, MRef} = my_spawn_opt(fun() ->
				      receive
					  ets_new ->
					      ets_new(Name, [named_table])				      
				      end
			       end,
			       [link, monitor]),
    true = ets:delete(Tab),
    Pid ! ets_new,
    receive {'DOWN',MRef,process,Pid,_} -> ok end,
    ok.

evil_delete(doc) ->
    "Delete a large table, and kill the process during the delete.";
evil_delete(Config) when is_list(Config) ->
    ?line Data = [{I,I*I} || I <- lists:seq(1, 100000)],
    repeat_for_opts(fun(Opts) -> evil_delete_do(Opts,Data) end).

evil_delete_do(Opts,Data) ->
    ?line EtsMem = etsmem(),
    ?line evil_delete_owner(foo_hash, Opts, Data, false),
    ?line verify_etsmem(EtsMem),
    ?line evil_delete_owner(foo_hash, Opts, Data, true),
    ?line verify_etsmem(EtsMem),
    ?line evil_delete_owner(foo_tree, [ordered_set | Opts], Data, false),
    ?line verify_etsmem(EtsMem),
    ?line TabA = evil_delete_not_owner(foo_hash, Opts, Data, false),
    ?line verify_etsmem(EtsMem),
    ?line TabB = evil_delete_not_owner(foo_hash, Opts, Data, true),
    ?line verify_etsmem(EtsMem),
    ?line TabC = evil_delete_not_owner(foo_tree, [ordered_set | Opts], Data, false),
    ?line verify_etsmem(EtsMem),
    ?line lists:foreach(fun(T) -> undefined = ets:info(T) end,
  			[TabA,TabB,TabC]).

evil_delete_not_owner(Name, Flags, Data, Fix) ->
    io:format("Not owner: ~p, fix = ~p", [Name,Fix]),
    ?line Tab = ets_new(Name, [public|Flags]),
    ?line ets:insert(Tab, Data),
    case Fix of
	false -> ok;
	true ->
	    ?line true = ets:safe_fixtable(Tab, true),
	    ?line lists:foreach(fun({K,_}) -> ets:delete(Tab, K) end, Data)
    end,
    ?line Pid = my_spawn(fun() ->
				 P = my_spawn_link(
				       fun() ->
					       receive kill -> ok end,
					       erlang:yield(),
					       exit(kill_linked_processes_now)
				       end),
				 erlang:yield(),
				 P ! kill,
				 true = ets:delete(Tab)
			 end),
    ?line Ref = erlang:monitor(process, Pid),
    ?line receive {'DOWN',Ref,_,_,_} -> ok end,
    Tab.

evil_delete_owner(Name, Flags, Data, Fix) ->
    ?line Fun = fun() ->
			?line Tab = ets_new(Name, [public|Flags]),
			?line ets:insert(Tab, Data),
			case Fix of
			    false -> ok;
			    true ->
				?line true = ets:safe_fixtable(Tab, true),
				?line lists:foreach(fun({K,_}) ->
							    ets:delete(Tab, K)
						    end, Data)
			end,
			erlang:yield(),
			my_spawn_link(fun() ->
					      erlang:yield(),
					      exit(kill_linked_processes_now)
				      end),
			true = ets:delete(Tab)
		end,
    ?line Pid = my_spawn(Fun),
    ?line Ref = erlang:monitor(process, Pid),
    ?line receive {'DOWN',Ref,_,_,_} -> ok end.


exit_large_table_owner(doc) ->
    [];
exit_large_table_owner(suite) ->
    [];
exit_large_table_owner(Config) when is_list(Config) ->
    %%?line Data = [{erlang:phash2(I, 16#ffffff),I} || I <- lists:seq(1, 500000)],
    ?line FEData = fun(Do) -> repeat_while(fun(500000) -> {false,ok};
                                              (I) -> Do({erlang:phash2(I, 16#ffffff),I}),
                                                     {true, I+1}
                                           end, 1)
                   end,
    ?line EtsMem = etsmem(),
    repeat_for_opts({exit_large_table_owner_do,{FEData,Config}}),
    ?line verify_etsmem(EtsMem).

exit_large_table_owner_do(Opts,{FEData,Config}) ->
    ?line verify_rescheduling_exit(Config, FEData, [named_table | Opts], true, 1, 1),
    ?line verify_rescheduling_exit(Config, FEData, Opts, false, 1, 1).

exit_many_large_table_owner(doc) -> [];
exit_many_large_table_owner(suite) -> [];
exit_many_large_table_owner(Config) when is_list(Config) ->
    %%?line Data = [{erlang:phash2(I, 16#ffffff),I} || I <- lists:seq(1, 500000)],
    ?line FEData = fun(Do) -> repeat_while(fun(500000) -> {false,ok};
                                              (I) -> Do({erlang:phash2(I, 16#ffffff),I}),
                                                     {true, I+1}
                                           end, 1)
                   end,
    ?line EtsMem = etsmem(),
    repeat_for_opts(fun(Opts) -> exit_many_large_table_owner_do(Opts,FEData,Config) end),
    ?line verify_etsmem(EtsMem).

exit_many_large_table_owner_do(Opts,FEData,Config) ->
    ?line verify_rescheduling_exit(Config, FEData, Opts, true, 1, 4),
    ?line verify_rescheduling_exit(Config, FEData, [named_table | Opts], false, 1, 4).

exit_many_tables_owner(doc) -> [];
exit_many_tables_owner(suite) -> [];
exit_many_tables_owner(Config) when is_list(Config) ->
    NoData = fun(_Do) -> ok end,
    ?line EtsMem = etsmem(),
    ?line verify_rescheduling_exit(Config, NoData, [named_table], false, 1000, 1),
    ?line verify_rescheduling_exit(Config, NoData, [named_table,{write_concurrency,true}], false, 1000, 1),
    ?line verify_etsmem(EtsMem).

exit_many_many_tables_owner(doc) -> [];
exit_many_many_tables_owner(suite) -> [];
exit_many_many_tables_owner(Config) when is_list(Config) ->
    ?line Data = [{erlang:phash2(I, 16#ffffff),I} || I <- lists:seq(1, 50)],
    ?line FEData = fun(Do) -> lists:foreach(Do, Data) end,
    repeat_for_opts(fun(Opts) -> exit_many_many_tables_owner_do(Opts,FEData,Config) end).

exit_many_many_tables_owner_do(Opts,FEData,Config) ->
    ?line verify_rescheduling_exit(Config, FEData, [named_table | Opts], true, 200, 5),
    ?line verify_rescheduling_exit(Config, FEData, Opts, false, 200, 5),
    ?line wait_for_test_procs(),
    ?line EtsMem = etsmem(),
    ?line verify_rescheduling_exit(Config, FEData, Opts, true, 200, 5),
    ?line verify_rescheduling_exit(Config, FEData, [named_table | Opts], false, 200, 5),
    ?line verify_etsmem(EtsMem).
    

count_exit_sched(TP) ->
    receive
	{trace, TP, in_exiting, 0} ->
	    count_exit_sched_out(TP, 1);
	{trace, TP, out_exiting, 0} ->
	    count_exit_sched_in(TP, 1);
	{trace, TP, out_exited, 0} ->
	    0
    end.

count_exit_sched_in(TP, N) ->
    receive
	{trace, TP, in_exiting, 0} ->
	    count_exit_sched_out(TP, N);
	{trace, TP, _, _} = Msg ->
	    exit({unexpected_trace_msg, Msg})
    end.

count_exit_sched_out(TP, N) ->
    receive
	{trace, TP, out_exiting, 0} ->
	    count_exit_sched_in(TP, N+1);
	{trace, TP, out_exited, 0} ->
	    N;
	{trace, TP, _, _} = Msg ->
	    exit({unexpected_trace_msg, Msg})
    end.

vre_fix_tables(Tab) ->
    Parent = self(),
    Go = make_ref(),
    my_spawn_link(fun () ->
			  true = ets:safe_fixtable(Tab, true),
			  Parent ! Go,
			  receive infinity -> ok end
		  end),
    receive Go -> ok end,
    ok.

verify_rescheduling_exit(Config, ForEachData, Flags, Fix, NOTabs, NOProcs) ->
    ?line NoFix = 5,
    ?line TestCase = atom_to_list(?config(test_case, Config)),
    ?line Parent = self(),
    ?line KillMe = make_ref(),
    ?line PFun =
	fun () ->
		repeat(
		  fun () ->
			  {A, B, C} = now(),
			  ?line Name = list_to_atom(
					 TestCase
					 ++ "-" ++ integer_to_list(A)
					 ++ "-" ++ integer_to_list(B)
					 ++ "-" ++ integer_to_list(C)),
			  Tab = ets_new(Name, Flags),
                          ForEachData(fun(Data) -> ets:insert(Tab, Data) end),
			  case Fix of
			      false -> ok;
			      true ->
				  lists:foreach(fun (_) ->
							vre_fix_tables(Tab)
						end,
						lists:seq(1,NoFix)),
                                  KeyPos = ets:info(Tab,keypos),
                                  ForEachData(fun(Data) ->
                                                ets:delete(Tab, element(KeyPos,Data))
                                              end)
			  end
		  end,
		  NOTabs),
		Parent ! {KillMe, self()},
		receive after infinity -> ok end
	end,
    ?line TPs = lists:map(fun (_) ->
				  ?line TP = my_spawn_link(PFun),
				  ?line 1 = erlang:trace(TP, true, [exiting]),
				  TP
			  end,
			  lists:seq(1, NOProcs)),
    ?line lists:foreach(fun (TP) ->
				receive {KillMe, TP} -> ok end
			end,
			TPs),
    ?line LPs = start_loopers(erlang:system_info(schedulers),
			      normal,
			      fun (_) ->
				      erlang:yield()
			      end,
			      ok),
    ?line lists:foreach(fun (TP) ->
				?line unlink(TP),
				?line exit(TP, bang)
			end,
			TPs),
    ?line lists:foreach(fun (TP) ->
				?line XScheds = count_exit_sched(TP),
				?line ?t:format("~p XScheds=~p~n",
						[TP, XScheds]),
				?line true = XScheds >= 5
			end,
			TPs),
    ?line stop_loopers(LPs),
    ?line ok.

			    

table_leak(doc) ->
    "Make sure that slots for ets tables are cleared properly.";
table_leak(Config) when is_list(Config) ->
    repeat_for_opts(fun(Opts) -> table_leak_1(Opts,20000) end).

table_leak_1(_,0) -> ok;
table_leak_1(Opts,N) ->
    ?line T = ets_new(fooflarf, Opts),
    ?line true = ets:delete(T),
    table_leak_1(Opts,N-1).

baddelete(doc) ->
    ["Check proper return values for illegal delete operations."];
baddelete(suite) -> [];
baddelete(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    ?line {'EXIT',{badarg,_}} = (catch ets:delete(foo)),
    ?line Tab = ets_new(foo,[]),
    ?line true = ets:delete(Tab),
    ?line {'EXIT',{badarg,_}} = (catch ets:delete(Tab)),
    ?line verify_etsmem(EtsMem).

match_delete(doc) ->
    ["Check that match_delete works. Also tests tab2list function."];
match_delete(suite) -> [];    
match_delete(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    repeat_for_opts(match_delete_do,[write_concurrency,all_types]),
    ?line verify_etsmem(EtsMem).

match_delete_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line Tab = ets_new(kad,Opts),
    ?line fill_tab(Tab,foo),
    ?line ets:insert(Tab,{{c,key},bar}),
    ?line _ = ets:match_delete(Tab,{'_',foo}),
    ?line [{{c,key},bar}] = ets:tab2list(Tab),
    ?line _ = ets:match_delete(Tab,'_'),
    ?line [] = ets:tab2list(Tab),
    ?line true = ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

match_delete3(doc) ->
    ["OTP-3005: check match_delete with constant argument."];
match_delete3(suite) -> [];    
match_delete3(Config) when is_list(Config) ->
    repeat_for_opts(match_delete3_do).

match_delete3_do(Opts) ->
    ?line EtsMem = etsmem(),
    T = make_table(test,
		   [duplicate_bag | Opts],
		   [{aa,17},
		    {cA,1000},
		    {cA,17},
		    {cA,1000},
		    {aa,17}]),
    %% 'aa' and 'cA' have the same hash value in the current
    %% implementation. This causes the aa's to precede the cA's, to make
    %% the test more interesting.
    [{cA,1000},{cA,1000}] = ets:match_object(T, {'_', 1000}),
    ets:match_delete(T, {cA,1000}),
    [] = ets:match_object(T, {'_', 1000}),
    ets:delete(T),
    ?line verify_etsmem(EtsMem).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

firstnext(doc) -> ["Tests ets:first/1 & ets:next/2."];
firstnext(suite) -> [];
firstnext(Config) when is_list(Config) ->
    repeat_for_opts(firstnext_do).

firstnext_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line Tab = ets_new(foo,Opts),
    ?line [] = firstnext_collect(Tab,ets:first(Tab),[]),
    ?line fill_tab(Tab,foo),
    ?line Len = length(ets:tab2list(Tab)),
    ?line Len = length(firstnext_collect(Tab,ets:first(Tab),[])),
    ?line true = ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

firstnext_collect(_Tab,'$end_of_table',List) ->
    ?line List;
firstnext_collect(Tab,Key,List) ->
    ?line firstnext_collect(Tab,ets:next(Tab,Key),[Key|List]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

firstnext_concurrent(doc) -> "Tests ets:first/1 & ets:next/2.";
firstnext_concurrent(Config) when is_list(Config) ->
    register(master, self()),
    ets_init(?MODULE, 20),
    [dynamic_go() || _ <- lists:seq(1, 2)],
    receive
	after 5000 -> ok
    end.

ets_init(Tab, N) ->
    ets_new(Tab, [named_table,public,ordered_set]),
    cycle(Tab, lists:seq(1,N+1)).

cycle(_Tab, [H|T]) when H > length(T)-> ok;
cycle(Tab, L) ->
    ets:insert(Tab,list_to_tuple(L)),
    cycle(Tab, tl(L)++[hd(L)]).

dynamic_go() -> my_spawn_link(fun dynamic_init/0).

dynamic_init() -> [dyn_lookup(?MODULE) || _ <- lists:seq(1, 10)].

dyn_lookup(T) -> dyn_lookup(T, ets:first(T)).

dyn_lookup(_T, '$end_of_table') -> [];
dyn_lookup(T, K) ->
    NextKey=ets:next(T,K),	
    case ets:next(T,K) of
	NextKey ->
	    dyn_lookup(T, NextKey);
	NK ->
	    io:fwrite("hmmm... ~p =/= ~p~n", [NextKey,NK]),
	    exit(failed)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slot(suite) -> [];
slot(Config) when is_list(Config) ->
    repeat_for_opts(slot_do).

slot_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line Tab = ets_new(foo,Opts),
    ?line fill_tab(Tab,foo),
    ?line Elts = ets:info(Tab,size),
    ?line Elts = slot_loop(Tab,0,0),
    ?line true = ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

slot_loop(Tab,SlotNo,EltsSoFar) ->
    ?line case ets:slot(Tab,SlotNo) of
	      '$end_of_table' ->
		  ?line {'EXIT',{badarg,_}} =
		      (catch ets:slot(Tab,SlotNo+1)),
		  ?line EltsSoFar;
	      Elts ->
		  ?line slot_loop(Tab,SlotNo+1,EltsSoFar+length(Elts))
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


match1(suite) -> [];
match1(Config) when is_list(Config) ->
    repeat_for_opts(match1_do).

match1_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line Tab = ets_new(foo,Opts),
    ?line fill_tab(Tab,foo),
    ?line [] = ets:match(Tab,{}),
    ?line ets:insert(Tab,{{one,4},4}),
    ?line ets:insert(Tab,{{one,5},5}),
    ?line ets:insert(Tab,{{two,4},4}),
    ?line ets:insert(Tab,{{two,5},6}),
    ?line case ets:match(Tab,{{one,'_'},'$0'}) of
	      [[4],[5]] -> ok;
	      [[5],[4]] -> ok
	  end,
    ?line case ets:match(Tab,{{two,'$1'},'$0'}) of
	      [[4,4],[6,5]] -> ok;
	      [[6,5],[4,4]] -> ok
	  end,
    ?line case ets:match(Tab,{{two,'$9'},'$4'}) of
	      [[4,4],[6,5]] -> ok;
	      [[6,5],[4,4]] -> ok
	  end,
    ?line case ets:match(Tab,{{two,'$9'},'$22'}) of
	      [[4,4],[5,6]] -> ok;
	      [[5,6],[4,4]] -> ok
	  end,
    ?line [[4]] = ets:match(Tab,{{two,'$0'},'$0'}),
    ?line Len = length(ets:match(Tab,'$0')),
    ?line Len = length(ets:match(Tab,'_')),
    ?line if Len > 4 -> ok end,
    ?line true = ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

match2(doc) -> ["Tests match with specified keypos bag table."];
match2(suite) -> [];
match2(Config) when is_list(Config) ->
    repeat_for_opts(match2_do).

match2_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line Tab = make_table(foobar,
			   [bag, named_table, {keypos, 2} | Opts],
			   [{value1, key1},
			    {value2_1, key2},
			    {value2_2, key2},
			    {value3_1, key3},
			    {value3_2, key3},
			    {value2_1, key2_wannabe}]),
    ?line case length(ets:match(Tab, '$1')) of
	      6 -> ok;
	      _ -> ?t:fail("Length of matched list is wrong.")
	  end,
    ?line [[value3_1],[value3_2]] = ets:match(Tab, {'$1', key3}),
    ?line [[key1]] = ets:match(Tab, {value1, '$1'}),
    ?line [[key2_wannabe],[key2]] = ets:match(Tab, {value2_1, '$2'}),
    ?line [] = ets:match(Tab,{'$1',nosuchkey}),
    ?line [] = ets:match(Tab,{'$1',kgY2}), % same hash as key2
    ?line [] = ets:match(Tab,{nosuchvalue,'$1'}),
    ?line true = ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

match_object(doc) -> ["Some ets:match_object test."];
match_object(suite) -> [];    
match_object(Config) when is_list(Config) ->
    repeat_for_opts(match_object_do).

match_object_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line Tab = ets_new(foobar, Opts),
    ?line fill_tab(Tab, foo),
    ?line ets:insert(Tab, {{one, 4}, 4}),
    ?line ets:insert(Tab,{{one,5},5}),
    ?line ets:insert(Tab,{{two,4},4}),
    ?line ets:insert(Tab,{{two,5},6}),
    ?line case ets:match_object(Tab, {{one, '_'}, '$0'}) of
	[{{one,5},5},{{one,4},4}] -> ok;
	[{{one,4},4},{{one,5},5}] -> ok;
	_ -> ?t:fail("ets:match_object() returned something funny.")
    end,
    ?line case ets:match_object(Tab, {{two, '$1'}, '$0'}) of
	[{{two,5},6},{{two,4},4}] -> ok;
	[{{two,4},4},{{two,5},6}] -> ok;
	_ -> ?t:fail("ets:match_object() returned something funny.")
    end,
    ?line case ets:match_object(Tab, {{two, '$9'}, '$4'}) of
	[{{two,5},6},{{two,4},4}] -> ok;
	[{{two,4},4},{{two,5},6}] -> ok;
	_ -> ?t:fail("ets:match_object() returned something funny.")
    end,
    ?line case ets:match_object(Tab, {{two, '$9'}, '$22'}) of
	[{{two,5},6},{{two,4},4}] -> ok;
	[{{two,4},4},{{two,5},6}] -> ok;
	_ -> ?t:fail("ets:match_object() returned something funny.")
    end,
    % Check that unsucessful match returns an empty list.
    ?line [] = ets:match_object(Tab, {{three,'$0'}, '$92'}),
    % Check that '$0' equals '_'.
    Len = length(ets:match_object(Tab, '$0')),
    Len = length(ets:match_object(Tab, '_')),
    ?line if Len > 4 -> ok end,
    ?line true = ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

match_object2(suite) -> [];
match_object2(doc) -> ["Tests that db_match_object does not generate "
		       "a `badarg' when resuming a search with no "
		       "previous matches."];
match_object2(Config) when is_list(Config) ->
    repeat_for_opts(match_object2_do).

match_object2_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line Tab = ets_new(foo, [bag, {keypos, 2} | Opts]),
    ?line fill_tab2(Tab, 0, 13005),     % match_db_object does 1000
                                       % elements per pass, might
                                       % change in the future.
    ?line case catch ets:match_object(Tab, {hej, '$1'}) of
	      {'EXIT', _} ->
		  ets:delete(Tab),
		  ?t:fail("match_object EXIT:ed");
	      [] ->
		  io:format("Nothing matched.");
	      List ->
		  io:format("Matched:~p~n",[List])
	  end,
    ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


tab2list(doc) -> ["Tests tab2list (OTP-3319)"];
tab2list(suite) -> [];
tab2list(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    ?line Tab = make_table(foo,
			   [ordered_set],
			   [{a,b}, {c,b}, {b,b}, {a,c}]),
    ?line [{a,c},{b,b},{c,b}] = ets:tab2list(Tab),
    ?line true = ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

misc1(doc) -> ["Simple general small test. ",
	      "If this fails, ets is in really bad shape."];
misc1(suite) -> [];
misc1(Config) when is_list(Config) ->
    repeat_for_opts(misc1_do).

misc1_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line Tab = ets_new(foo,Opts),
    ?line true = lists:member(Tab,ets:all()),
    ?line ets:delete(Tab),
    ?line false = lists:member(Tab,ets:all()),
    ?line case catch ets:delete(Tab) of
	      {'EXIT',_Reason} ->
		       ?line verify_etsmem(EtsMem);
	      true ->
		  ?t:fail("Delete of nonexisting table returned `true'.")
	  end,
    ok.

safe_fixtable(doc) ->  ["Check the safe_fixtable function."];
safe_fixtable(suite) -> [];
safe_fixtable(Config) when is_list(Config) ->
    repeat_for_opts(safe_fixtable_do).

safe_fixtable_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line Tab = ets_new(foo, Opts),
    ?line fill_tab(Tab, foobar),
    ?line true = ets:safe_fixtable(Tab, true),
    ?line receive after 1 -> ok end,
    ?line true = ets:safe_fixtable(Tab, false),
    ?line false = ets:info(Tab,safe_fixed),
    ?line true = ets:safe_fixtable(Tab, true),
    Self = self(),
    ?line {{_,_,_},[{Self,1}]} = ets:info(Tab,safe_fixed),
    %% Test that an unjustified 'unfix' is a no-op.
    {Pid,MRef} = my_spawn_monitor(fun() -> true = ets:safe_fixtable(Tab,false) end),
    {'DOWN', MRef, process, Pid, normal} = receive M -> M end,
    ?line true = ets:info(Tab,fixed),
    ?line {{_,_,_},[{Self,1}]} = ets:info(Tab,safe_fixed),
    %% badarg's
    ?line {'EXIT', {badarg, _}} = (catch ets:safe_fixtable(Tab, foobar)),
    ?line true = ets:info(Tab,fixed),
    ?line true = ets:safe_fixtable(Tab, false),
    ?line false = ets:info(Tab,fixed),
    ?line {'EXIT', {badarg, _}} = (catch ets:safe_fixtable(Tab, foobar)),
    ?line false = ets:info(Tab,fixed),
    ?line ets:delete(Tab),
    ?line case catch ets:safe_fixtable(Tab, true) of
	      {'EXIT', _Reason} ->
		       ?line verify_etsmem(EtsMem);
	      _ ->
		  ?t:fail("Fixtable on nonexisting table returned `true'")
	  end,
    ok.

info(doc) -> ["Tests ets:info result for required tuples."];
info(suite) -> [];
info(Config) when is_list(Config) ->
    repeat_for_opts(info_do).

info_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line MeMyselfI=self(),
    ?line ThisNode=node(),
    ?line Tab = ets_new(foobar, [{keypos, 2} | Opts]),

    %% Note: ets:info/1 used to return a tuple, but from R11B onwards it
    %% returns a list.
    ?line Res = ets:info(Tab),
    ?line {value, {memory, _Mem}} = lists:keysearch(memory, 1, Res),
    ?line {value, {owner, MeMyselfI}} = lists:keysearch(owner, 1, Res),
    ?line {value, {name, foobar}} = lists:keysearch(name, 1, Res),
    ?line {value, {size, 0}} = lists:keysearch(size, 1, Res),
    ?line {value, {node, ThisNode}} = lists:keysearch(node, 1, Res),
    ?line {value, {named_table, false}} = lists:keysearch(named_table, 1, Res),
    ?line {value, {type, set}} = lists:keysearch(type, 1, Res),
    ?line {value, {keypos, 2}} = lists:keysearch(keypos, 1, Res),
    ?line {value, {protection, protected}} =
	lists:keysearch(protection, 1, Res),
    ?line true = ets:delete(Tab),
    ?line undefined = ets:info(non_existing_table_xxyy),
    ?line undefined = ets:info(non_existing_table_xxyy,type),
    ?line undefined = ets:info(non_existing_table_xxyy,node),
    ?line undefined = ets:info(non_existing_table_xxyy,named_table),
    ?line undefined = ets:info(non_existing_table_xxyy,safe_fixed),
    ?line verify_etsmem(EtsMem).

dups(doc) -> ["Test various duplicate_bags stuff"];
dups(suite) -> [];
dups(Config) when is_list(Config) ->
    repeat_for_opts(dups_do).

dups_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line T = make_table(funky,
			 [duplicate_bag | Opts],
			 [{1, 2}, {1, 2}]),
    ?line 2 = length(ets:tab2list(T)),
    ?line ets:delete(T, 1),
    ?line [] = ets:lookup(T, 1),

    ?line ets:insert(T, {1, 2, 2}),
    ?line ets:insert(T, {1, 2, 4}),
    ?line ets:insert(T, {1, 2, 2}),
    ?line ets:insert(T, {1, 2, 2}),
    ?line ets:insert(T, {1, 2, 4}),

    ?line 5 = length(ets:tab2list(T)),

    ?line 5 = length(ets:match(T, {'$1', 2, '$2'})),
    ?line 3 = length(ets:match(T, {'_', '$1', '$1'})),
    ?line ets:match_delete(T, {'_', '$1', '$1'}),
    ?line 0 = length(ets:match(T, {'_', '$1', '$1'})),
    ?line ets:delete(T),
    ?line verify_etsmem(EtsMem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tab2file(doc) -> ["Check the ets:tab2file function on an empty "
		  "ets table."];
tab2file(suite) -> [];
tab2file(Config) when is_list(Config) ->
    %% Write an empty ets table to a file, read back and check properties.
    ?line Tab = ets_new(ets_SUITE_foo_tab, [named_table, set, private,
					    {keypos, 2}]),
    ?line FName = filename:join([?config(priv_dir, Config),"tab2file_case"]),
    ?line ok = ets:tab2file(Tab, FName),
    ?line true = ets:delete(Tab),
    %
    ?line EtsMem = etsmem(),
    ?line {ok, Tab2} = ets:file2tab(FName),
    ?line private = ets:info(Tab2, protection),
    ?line true = ets:info(Tab2, named_table),
    ?line 2 = ets:info(Tab2, keypos),
    ?line set = ets:info(Tab2, type),
    ?line true = ets:delete(Tab2),
    ?line verify_etsmem(EtsMem).
    
tab2file2(doc) -> ["Check the ets:tab2file function on a ",
		   "filled set/bag type ets table."];
tab2file2(suite) -> [];
tab2file2(Config) when is_list(Config) ->
    repeat_for_opts({tab2file2_do,Config}, [[set,bag],compressed]).

tab2file2_do(Opts, Config) ->
    ?line EtsMem = etsmem(),
    ?line Tab = ets_new(ets_SUITE_foo_tab, [named_table, private,
					    {keypos, 2} | Opts]),
    ?line FName = filename:join([?config(priv_dir, Config),"tab2file2_case"]),
    ?line ok = fill_tab2(Tab, 0, 10000),   % Fill up the table (grucho mucho!)
    ?line Len = length(ets:tab2list(Tab)),
    ?line Mem = ets:info(Tab, memory),
    ?line Type = ets:info(Tab, type),
    %%io:format("org tab: ~p\n",[ets:info(Tab)]),
    ?line ok = ets:tab2file(Tab, FName),
    ?line true = ets:delete(Tab),

    ?line EtsMem4 = etsmem(),

    ?line {ok, Tab2} = ets:file2tab(FName),
    %%io:format("loaded tab: ~p\n",[ets:info(Tab2)]),
    ?line private = ets:info(Tab2, protection),
    ?line true = ets:info(Tab2, named_table),
    ?line 2 = ets:info(Tab2, keypos),
    ?line Type = ets:info(Tab2, type),
    ?line Len = length(ets:tab2list(Tab2)),
    ?line Mem = ets:info(Tab2, memory),
    ?line true = ets:delete(Tab2),
    io:format("Between = ~p\n", [EtsMem4]),
    ?line verify_etsmem(EtsMem).

-define(test_list, [8,5,4,1,58,125,255, 250, 245, 240, 235,
		    230, Num rem 255, 255, 125, 130, 135, 140, 145,
		    150, 134, 12, 54, Val rem 255, 12, 3, 6, 9, 126]).
-define(big_test_list, [Num rem 256|lists:seq(1, 66)]).
-define(test_integer, 2846287468+Num).
-define(test_float, 187263.18236-Val).
-define(test_atom, some_crazy_atom).
-define(test_tuple, {just, 'Some', 'Tuple', 1, [list, item], Val+Num}).

%% Insert different datatypes into a ets table.
fill_tab2(_Tab, _Val, 0) ->
    ok;
fill_tab2(Tab, Val, Num) ->
    ?line Item =
	case Num rem 10 of
	    0 -> "String";
	    1 -> ?line ?test_atom;
	    2 -> ?line ?test_tuple;
	    3 -> ?line ?test_integer;
	    4 -> ?line ?test_float;
	    5 -> ?line list_to_binary(?test_list); %Heap binary
	    6 -> ?line list_to_binary(?big_test_list); %Refc binary
	    7 -> ?line make_sub_binary(?test_list, Num); %Sub binary
	    8 -> ?line ?test_list;
	    9 -> ?line fun(X) -> {Tab,Val,X*Num} end
	end,
    ?line true=ets:insert(Tab, {Item, Val}),
    ?line fill_tab2(Tab, Val+1, Num-1),
    ok.

tabfile_ext1(suite) ->
    [];
tabfile_ext1(doc) ->
    ["Tests verification of tables with object count extended_info"];
tabfile_ext1(Config) when is_list(Config) ->
    repeat_for_opts(fun(Opts) -> tabfile_ext1_do(Opts, Config) end).

tabfile_ext1_do(Opts,Config) ->
    ?line FName = filename:join([?config(priv_dir, Config),"nisse.dat"]),
    ?line FName2 = filename:join([?config(priv_dir, Config),"countflip.dat"]),
    L = lists:seq(1,10),
    T = ets_new(x,Opts),
    Name = make_ref(),
    [ets:insert(T,{X,integer_to_list(X)}) || X <- L],
    ok = ets:tab2file(T,FName,[{extended_info,[object_count]}]),
    true = lists:sort(ets:tab2list(T)) =:= 
	lists:sort(ets:tab2list(element(2,ets:file2tab(FName)))),
    true = lists:sort(ets:tab2list(T)) =:= 
	lists:sort(ets:tab2list(
		     element(2,ets:file2tab(FName,[{verify,true}])))),
    {ok,Name} = disk_log:open([{name,Name},{file,FName}]),
    {_,[H0|T0]} = disk_log:chunk(Name,start),
    disk_log:close(Name),
    LH0=tuple_to_list(H0),
    {value,{size,N}}=lists:keysearch(size,1,LH0),
    NewLH0 = lists:keyreplace(size,1,LH0,{size,N-1}),
    NewH0 = list_to_tuple(NewLH0),
    NewT0=lists:keydelete(8,1,T0),
    file:delete(FName2),
    disk_log:open([{name,Name},{file,FName2},{mode,read_write}]),
    disk_log:log_terms(Name,[NewH0|NewT0]),
    disk_log:close(Name),
    9 = length(ets:tab2list(element(2,ets:file2tab(FName2)))),
    {error,invalid_object_count} = ets:file2tab(FName2,[{verify,true}]),
    {ok, _} = ets:tabfile_info(FName2),
    {ok, _} = ets:tabfile_info(FName),
    file:delete(FName),
    file:delete(FName2),
    ok.

tabfile_ext2(suite) ->
    [];
tabfile_ext2(doc) ->
    ["Tests verification of tables with md5sum extended_info"];
tabfile_ext2(Config) when is_list(Config) ->
    repeat_for_opts(fun(Opts) -> tabfile_ext2_do(Opts,Config) end).

tabfile_ext2_do(Opts,Config) ->
    ?line FName = filename:join([?config(priv_dir, Config),"olle.dat"]),
    ?line FName2 = filename:join([?config(priv_dir, Config),"bitflip.dat"]),
    L = lists:seq(1,10),
    T = ets_new(x,Opts),
    Name = make_ref(),
    [ets:insert(T,{X,integer_to_list(X)}) || X <- L],
    ok = ets:tab2file(T,FName,[{extended_info,[md5sum]}]),
    true = lists:sort(ets:tab2list(T)) =:= 
	lists:sort(ets:tab2list(element(2,ets:file2tab(FName)))),
    true = lists:sort(ets:tab2list(T)) =:= 
	lists:sort(ets:tab2list(
		     element(2,ets:file2tab(FName,[{verify,true}])))),
    {ok, Name} = disk_log:open([{name,Name},{file,FName}]),
    {_,[H1|T1]} = disk_log:chunk(Name,start),
    disk_log:close(Name),
    NewT1=lists:keyreplace(8,1,T1,{8,"9"}),
    file:delete(FName2),
    disk_log:open([{name,Name},{file,FName2},{mode,read_write}]),
    disk_log:log_terms(Name,[H1|NewT1]),
    disk_log:close(Name),
    {value,{8,"9"}} = lists:keysearch(8,1,
				      ets:tab2list(
					element(2,ets:file2tab(FName2)))),
    {error,checksum_error} = ets:file2tab(FName2,[{verify,true}]),
    {value,{extended_info,[md5sum]}} = 
	lists:keysearch(extended_info,1,element(2,ets:tabfile_info(FName2))),
    {value,{extended_info,[md5sum]}} = 
	lists:keysearch(extended_info,1,element(2,ets:tabfile_info(FName))),
    file:delete(FName),
    file:delete(FName2),
    ok.

tabfile_ext3(suite) ->
    [];
tabfile_ext3(doc) ->
    ["Tests verification of (named) tables without extended info"];
tabfile_ext3(Config) when is_list(Config) ->
    ?line FName = filename:join([?config(priv_dir, Config),"namn.dat"]),
    ?line FName2 = filename:join([?config(priv_dir, Config),"ncountflip.dat"]),
    L = lists:seq(1,10),
    Name = make_ref(),
    ?MODULE = ets_new(?MODULE,[named_table]),
    [ets:insert(?MODULE,{X,integer_to_list(X)}) || X <- L],
    ets:tab2file(?MODULE,FName),
    {error,cannot_create_table} = ets:file2tab(FName),
    true = ets:delete(?MODULE),
    {ok,?MODULE} = ets:file2tab(FName),
    true = ets:delete(?MODULE),
    disk_log:open([{name,Name},{file,FName}]),
    {_,[H2|T2]} = disk_log:chunk(Name,start),
    disk_log:close(Name),
    NewT2=lists:keydelete(8,1,T2),
    file:delete(FName2),
    disk_log:open([{name,Name},{file,FName2},{mode,read_write}]),
    disk_log:log_terms(Name,[H2|NewT2]),
    disk_log:close(Name),
    9 = length(ets:tab2list(element(2,ets:file2tab(FName2)))),
    true = ets:delete(?MODULE),
    {error,invalid_object_count} = ets:file2tab(FName2,[{verify,true}]),
    {'EXIT',_} = (catch ets:delete(?MODULE)),
    {ok,_} = ets:tabfile_info(FName2),
    {ok,_} = ets:tabfile_info(FName),
    file:delete(FName),
    file:delete(FName2),
    ok.

tabfile_ext4(suite) ->
    [];
tabfile_ext4(doc) ->
    ["Tests verification of large table with md5 sum"];
tabfile_ext4(Config) when is_list(Config) ->
    ?line FName = filename:join([?config(priv_dir, Config),"bauta.dat"]),
    LL = lists:seq(1,10000),
    TL = ets_new(x,[]),
    Name2 = make_ref(),
    [ets:insert(TL,{X,integer_to_list(X)}) || X <- LL],
    ok = ets:tab2file(TL,FName,[{extended_info,[md5sum]}]),
    {ok, Name2} = disk_log:open([{name, Name2}, {file, FName}, 
				 {mode, read_only}]),
    {C,[_|_]} = disk_log:chunk(Name2,start),
    {_,[_|_]} = disk_log:chunk(Name2,C),
    disk_log:close(Name2),
    true = lists:sort(ets:tab2list(TL)) =:= 
	lists:sort(ets:tab2list(element(2,ets:file2tab(FName)))),
    Res = [
	   begin
	       {ok,FD} = file:open(FName,[binary,read,write]),
	       {ok, Bin} = file:pread(FD,0,1000),
	       <<B1:N/binary,Ch:8,B2/binary>> = Bin,
	       Ch2 = (Ch + 1) rem 255,
	       Bin2 = <<B1/binary,Ch2:8,B2/binary>>,
	       ok = file:pwrite(FD,0,Bin2),
	       ok = file:close(FD),
	       X = case ets:file2tab(FName) of
		       {ok,TL2} ->
			   true = lists:sort(ets:tab2list(TL)) =/= 
			       lists:sort(ets:tab2list(TL2));
		       _ ->
			   totally_broken
		   end,
	       {error,Y} = ets:file2tab(FName,[{verify,true}]),
	       ets:tab2file(TL,FName,[{extended_info,[md5sum]}]),
	       {X,Y}
	   end || N <- lists:seq(400,500) ],
    io:format("~p~n",[Res]),
    file:delete(FName),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_sub_binary(List, Num) when is_list(List) ->
    N = Num rem 23,
    Bin = list_to_binary([lists:seq(0, N)|List]),
    {_,B} = split_binary(Bin, N+1),
    B.


%% Lookup stuff like crazy...
heavy_lookup(doc) -> ["Performs multiple lookups for every key ",
		      "in a large table."];
heavy_lookup(suite) -> [];
heavy_lookup(Config) when is_list(Config) ->
    repeat_for_opts(heavy_lookup_do).

heavy_lookup_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line Tab = ets_new(foobar_table, [set, protected, {keypos, 2} | Opts]),
    ?line ok = fill_tab2(Tab, 0, 7000),
    ?line ?t:do_times(50, ?MODULE, do_lookup, [Tab, 6999]),
    ?line true = ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

do_lookup(_Tab, 0) -> ok;
do_lookup(Tab, N) ->
    case ets:lookup(Tab, N) of
	[] -> ?t:format("Set #~p was reported as empty. Not valid.",
				 [N]),
	      exit('Invalid lookup');
	_ ->  do_lookup(Tab, N-1)
    end.

heavy_lookup_element(doc) -> ["Performs multiple lookups for ",
			      "every element in a large table."];
heavy_lookup_element(suite) -> [];
heavy_lookup_element(Config) when is_list(Config) ->
    repeat_for_opts(heavy_lookup_element_do).

heavy_lookup_element_do(Opts) ->
    EtsMem = etsmem(),
    Tab = ets_new(foobar_table, [set, protected, {keypos, 2} | Opts]),
    ok = fill_tab2(Tab, 0, 7000),
    % lookup ALL elements 50 times
    ?t:do_times(50, ?MODULE, do_lookup_element, [Tab, 6999, 1]),
    true = ets:delete(Tab),
    verify_etsmem(EtsMem).

do_lookup_element(_Tab, 0, _) -> ok;
do_lookup_element(Tab, N, M) ->
    ?line case catch ets:lookup_element(Tab, N, M) of
	      {'EXIT', {badarg, _}} ->
		  case M of
		      1 -> ?t:fail("Set #~p reported as empty. Not valid.",
				   [N]),
			   exit('Invalid lookup_element');
		      _ -> ?line do_lookup_element(Tab, N-1, 1)
		  end;
	      _ -> ?line do_lookup_element(Tab, N, M+1)
    end.


heavy_concurrent(Config) when is_list(Config) ->
    repeat_for_opts(do_heavy_concurrent).

do_heavy_concurrent(Opts) ->
    ?line Size = 10000,
    ?line EtsMem = etsmem(),
    ?line Tab = ets_new(blupp, [set, public, {keypos, 2} | Opts]),
    ?line ok = fill_tab2(Tab, 0, Size),
    ?line Procs = lists:map(
		    fun (N) ->
			    my_spawn_link(
			      fun () ->
				      do_heavy_concurrent_proc(Tab, Size, N)
			      end)
		    end,
		    lists:seq(1, 500)),
    ?line lists:foreach(fun (P) ->
				M = erlang:monitor(process, P),
				receive
				    {'DOWN', M, process, P, _} ->
					ok
				end
			end,
			Procs),
    ?line true = ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

do_heavy_concurrent_proc(_Tab, 0, _Offs) ->
    done;
do_heavy_concurrent_proc(Tab, N, Offs) when (N+Offs) rem 100 == 0 ->
    Data = {"here", are, "S O M E ", data, "toooooooooooooooooo", insert,
	    make_ref(), make_ref(), make_ref()},
    true=ets:insert(Tab, {{self(),Data}, N}),
    do_heavy_concurrent_proc(Tab, N-1, Offs);
do_heavy_concurrent_proc(Tab, N, Offs) ->
    _ = ets:lookup(Tab, N),
    do_heavy_concurrent_proc(Tab, N-1, Offs).


fold_empty(doc) ->
    [];
fold_empty(suite) -> [];
fold_empty(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    ?line Tab = make_table(a, [], []),
    ?line [] = ets:foldl(fun(_X) -> exit(hej) end, [], Tab),
    ?line [] = ets:foldr(fun(_X) -> exit(hej) end, [], Tab),
    ?line true = ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

foldl(doc) ->
    [];
foldl(suite) -> [];
foldl(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    ?line L = [{a,1}, {c,3}, {b,2}],
    ?line LS = lists:sort(L),
    ?line Tab = make_table(a, [bag], L),
    ?line LS = lists:sort(ets:foldl(fun(E,A) -> [E|A] end, [], Tab)),
    ?line true = ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

foldr(doc) ->
    [];
foldr(suite) -> [];
foldr(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    ?line L = [{a,1}, {c,3}, {b,2}],
    ?line LS = lists:sort(L),
    ?line Tab = make_table(a, [bag], L),
    ?line LS = lists:sort(ets:foldr(fun(E,A) -> [E|A] end, [], Tab)),
    ?line true = ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

foldl_ordered(doc) ->
    [];
foldl_ordered(suite) -> [];
foldl_ordered(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    ?line L = [{a,1}, {c,3}, {b,2}],
    ?line LS = lists:sort(L),
    ?line Tab = make_table(a, [ordered_set], L),
    ?line LS = lists:reverse(ets:foldl(fun(E,A) -> [E|A] end, [], Tab)),
    ?line true = ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

foldr_ordered(doc) ->
    [];
foldr_ordered(suite) -> [];
foldr_ordered(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    ?line L = [{a,1}, {c,3}, {b,2}],
    ?line LS = lists:sort(L),
    ?line Tab = make_table(a, [ordered_set], L),
    ?line LS = ets:foldr(fun(E,A) -> [E|A] end, [], Tab),
    ?line true = ets:delete(Tab),
    ?line verify_etsmem(EtsMem).

member(suite) ->
    [];
member(doc) ->
    ["Tests ets:member BIF"];
member(Config) when is_list(Config) ->
    repeat_for_opts(member_do, [write_concurrency, all_types]).

member_do(Opts) ->
    ?line EtsMem = etsmem(),
    ?line T = ets_new(xxx, Opts),
    ?line false = ets:member(T,hej),
    ?line E = fun(0,_F)->ok;
		 (N,F) -> 
		      ?line ets:insert(T,{N,N rem 10}), 
		      F(N-1,F) 
	      end,
    ?line E(10000,E),                                                          
    ?line false = ets:member(T,hej),
    ?line true = ets:member(T,1),
    ?line false = ets:member(T,20000),
    ?line ets:delete(T,5),
    ?line false = ets:member(T,5),
    ?line ets:safe_fixtable(T,true),
    ?line ets:delete(T,6),
    ?line false = ets:member(T,6),
    ?line ets:safe_fixtable(T,false),
    ?line false = ets:member(T,6),
    ?line ets:delete(T),
    ?line {'EXIT',{badarg,_}} = (catch ets:member(finnsinte, 23)),
    ?line {'EXIT',{badarg,_}} = (catch ets:member(T, 23)),
    ?line verify_etsmem(EtsMem).


build_table(L1,L2,Num) ->
    T = ets_new(xxx, [ordered_set]
	       ),
    lists:foreach(
      fun(X1) ->
	      lists:foreach(
		fun(X2) ->
			F = fun(FF,N) ->
				    ets:insert(T,{{X1,X2,N}, 
						  X1, X2, N}),
				    case N of 
					0 ->
					    ok;
					_ ->
					    FF(FF,N-1)
				    end
			    end,
			F(F,Num)
		end, L2)
      end, L1),
    T.

build_table2(L1,L2,Num) ->
    T = ets_new(xxx, [ordered_set]
	       ),
    lists:foreach(
      fun(X1) ->
	      lists:foreach(
		fun(X2) ->
			F = fun(FF,N) ->
				    ets:insert(T,{{N,X1,X2}, 
						  N, X1, X2}),
				    case N of 
					0 ->
					    ok;
					_ ->
					    FF(FF,N-1)
				    end
			    end,
			F(F,Num)
		end, L2)
      end, L1),
    T.

time_match_object(Tab,Match, Res) ->
    T1 = erlang:now(),
    Res = ets:match_object(Tab,Match),
    T2 = erlang:now(),
    nowdiff(T1,T2).

time_match(Tab,Match) ->
    T1 = erlang:now(),
    ets:match(Tab,Match),
    T2 = erlang:now(),
    nowdiff(T1,T2).

seventyfive_percent_success(_,S,Fa,0) ->
    true = (S > ((S + Fa) * 0.75));

seventyfive_percent_success({M,F,A},S,Fa,N) ->
    case (catch apply(M,F,A)) of
	{'EXIT', _} ->
	    seventyfive_percent_success({M,F,A},S,Fa+1,N-1);
	_ ->
	    seventyfive_percent_success({M,F,A},S+1,Fa,N-1)
    end.

fifty_percent_success(_,S,Fa,0) ->
    true = (S > ((S + Fa) * 0.5));

fifty_percent_success({M,F,A},S,Fa,N) ->
    case (catch apply(M,F,A)) of
	{'EXIT', _} ->
	    fifty_percent_success({M,F,A},S,Fa+1,N-1);
	_ ->
	    fifty_percent_success({M,F,A},S+1,Fa,N-1)
    end.


nowtonumber({Mega, Secs, Milli}) ->
    Milli + Secs * 1000000 + Mega * 1000000000000.
nowdiff(T1,T2) ->
    nowtonumber(T2) - nowtonumber(T1).

create_random_string(0) ->
    [];

create_random_string(OfLength) ->
    C = case random:uniform(2) of
	1 ->
	    (random:uniform($Z - $A + 1) - 1) + $A;
	_ ->
	    (random:uniform($z - $a + 1) - 1) + $a
	end,
    [C | create_random_string(OfLength - 1)].


create_random_tuple(OfLength) ->
    list_to_tuple(lists:map(fun(X) ->
				    list_to_atom([X])
			    end,create_random_string(OfLength))).

create_partly_bound_tuple(OfLength) ->
    case random:uniform(2) of
	1 ->
	   create_partly_bound_tuple1(OfLength); 
	_ ->
	   create_partly_bound_tuple3(OfLength)
    end.

create_partly_bound_tuple1(OfLength) ->
    T0 = create_random_tuple(OfLength),
    I = random:uniform(OfLength),
    setelement(I,T0,'$1').


set_n_random_elements(T0,0,_,_) ->
    T0;
set_n_random_elements(T0,N,OfLength,GenFun) ->
    I = random:uniform(OfLength),
    What = GenFun(I),
    case element(I,T0) of
	What ->
	    set_n_random_elements(T0,N,OfLength,GenFun);
	_Else ->
	    set_n_random_elements(setelement(I,T0,What),
				  N-1,OfLength,GenFun)
    end.

make_dollar_atom(I) ->
    list_to_atom([$$] ++ integer_to_list(I)).
create_partly_bound_tuple2(OfLength) ->
    T0 = create_random_tuple(OfLength),
    I = random:uniform(OfLength - 1),
    set_n_random_elements(T0,I,OfLength,fun make_dollar_atom/1).

create_partly_bound_tuple3(OfLength) ->
    T0 = create_random_tuple(OfLength),
    I = random:uniform(OfLength - 1),
    set_n_random_elements(T0,I,OfLength,fun(_) -> '_' end).

do_n_times(_,0) ->
    ok;
do_n_times(Fun,N) ->
    Fun(),
    case N rem 1000 of
	0 ->
	    io:format(".");
	_ ->
	    ok
    end,
    do_n_times(Fun,N-1).

make_table(Name, Options, Elements) ->
    T = ets_new(Name, Options),
    lists:foreach(fun(E) -> ets:insert(T, E) end, Elements),
    T.
filltabint(Tab,0) ->
    Tab;
filltabint(Tab,N) ->
    ets:insert(Tab,{N,integer_to_list(N)}),
    filltabint(Tab,N-1).
filltabint2(Tab,0) ->
    Tab;
filltabint2(Tab,N) ->
    ets:insert(Tab,{N + N rem 2,integer_to_list(N)}),
    filltabint2(Tab,N-1).
filltabint3(Tab,0) ->
    Tab;
filltabint3(Tab,N) ->
    ets:insert(Tab,{N + N rem 2,integer_to_list(N + N rem 2)}),
    filltabint3(Tab,N-1).
xfilltabint(Tab,N) ->
    case ets:info(Tab,type) of
	bag ->
	    filltabint2(Tab,N);
	duplicate_bag ->
	    ets:select_delete(Tab,[{'_',[],[true]}]),
	    filltabint3(Tab,N);
	_ ->
	    filltabint(Tab,N)
    end.
    

filltabstr(Tab,N) ->
    filltabstr(Tab,0,N).
filltabstr(Tab,N,N) ->
    Tab;
filltabstr(Tab,Floor,N) when N > Floor ->
    ets:insert(Tab,{integer_to_list(N),N}),
    filltabstr(Tab,Floor,N-1).

filltabstr2(Tab,0) ->
    Tab;
filltabstr2(Tab,N) ->
    ets:insert(Tab,{integer_to_list(N),N}),
    ets:insert(Tab,{integer_to_list(N),N+1}),
    filltabstr2(Tab,N-1).
filltabstr3(Tab,0) ->
    Tab;
filltabstr3(Tab,N) ->
    ets:insert(Tab,{integer_to_list(N),N}),
    ets:insert(Tab,{integer_to_list(N),N}),
    filltabstr3(Tab,N-1).
xfilltabstr(Tab,N) ->
    case ets:info(Tab,type) of
	bag ->
	    filltabstr2(Tab,N);
	duplicate_bag ->
	    ets:select_delete(Tab,[{'_',[],[true]}]),
	    filltabstr3(Tab,N);
	_ ->
	    filltabstr(Tab,N)
    end.

fill_sets_int(N) ->
    fill_sets_int(N,[]).
fill_sets_int(N,Opts) ->
    Tab1 = ets_new(xxx, [ordered_set|Opts]),
    filltabint(Tab1,N),
    Tab2 = ets_new(xxx, [set|Opts]),
    filltabint(Tab2,N),
    Tab3 = ets_new(xxx, [bag|Opts]),
    filltabint2(Tab3,N),
    Tab4 = ets_new(xxx, [duplicate_bag|Opts]),
    filltabint3(Tab4,N),
    [Tab1,Tab2,Tab3,Tab4].

check_fun(_Tab,_Fun,'$end_of_table') ->
    ok;
check_fun(Tab,Fun,Item) ->
    lists:foreach(fun(Obj) ->
			  true = Fun(Obj)
		  end,
		  ets:lookup(Tab,Item)),
    check_fun(Tab,Fun,ets:next(Tab,Item)).

check(Tab,Fun,N) ->
    N = ets:info(Tab, size),
    check_fun(Tab,Fun,ets:first(Tab)).



del_one_by_one_set(T,N,N) ->
    0 = ets:info(T,size),
    ok;
del_one_by_one_set(T,From,To) ->
    N = ets:info(T,size),
    ets:delete_object(T,{From, integer_to_list(From)}),
    N = (ets:info(T,size) + 1),
    Next = if
	       From < To ->
		   From + 1;
	       true ->
		   From - 1
	   end,
    del_one_by_one_set(T,Next,To).

del_one_by_one_bag(T,N,N) ->
    0 = ets:info(T,size),
    ok;
del_one_by_one_bag(T,From,To) ->
    N = ets:info(T,size),
    ets:delete_object(T,{From + From rem 2, integer_to_list(From)}),
    N = (ets:info(T,size) + 1),
    Next = if
	       From < To ->
		   From + 1;
	       true ->
		   From - 1
	   end,
    del_one_by_one_bag(T,Next,To).


del_one_by_one_dbag_1(T,N,N) ->
    0 = ets:info(T,size),
    ok;
del_one_by_one_dbag_1(T,From,To) ->
    N = ets:info(T,size),
    ets:delete_object(T,{From, integer_to_list(From)}),
    case From rem 2 of
	0 ->
	    N = (ets:info(T,size) + 2);
	1 ->
	    N = ets:info(T,size)
    end,
    Next = if
	       From < To ->
		   From + 1;
	       true ->
		   From - 1
	   end,
    del_one_by_one_dbag_1(T,Next,To).

del_one_by_one_dbag_2(T,N,N) ->
    0 = ets:info(T,size),
    ok;
del_one_by_one_dbag_2(T,From,To) ->
    N = ets:info(T,size),
    ets:delete_object(T,{From, integer_to_list(From)}),
    case From rem 2 of
	0 ->
	    N = (ets:info(T,size) + 3);
	1 ->
	    N = (ets:info(T,size) + 1)
    end,
    Next = if
	       From < To ->
		   From + 1;
	       true ->
		   From - 1
	   end,
    del_one_by_one_dbag_2(T,Next,To).

del_one_by_one_dbag_3(T,N,N) ->
    0 = ets:info(T,size),
    ok;
del_one_by_one_dbag_3(T,From,To) ->
    N = ets:info(T,size),
    Obj = {From + From rem 2, integer_to_list(From)},
    ets:delete_object(T,Obj),
    case From rem 2 of
	0 ->
	    N = (ets:info(T,size) + 2);
	1 ->
	    N = (ets:info(T,size) + 1),
	    Obj2 = {From, integer_to_list(From)},
	    ets:delete_object(T,Obj2),
	    N = (ets:info(T,size) + 2)	    
    end,
    Next = if
	       From < To ->
		   From + 1;
	       true ->
		   From - 1
	   end,
    del_one_by_one_dbag_3(T,Next,To).


successive_delete(Table,From,To,Type) ->
    successive_delete(Table,From,To,Type,ets:info(Table,type)).

successive_delete(_Table,N,N,_,_) ->
    ok;
successive_delete(Table,From,To,Type,TType) ->
    MS = case Type of
	     bound ->
		 [{{From,'_'},[],[true]}];
	     unbound ->
		 [{{'$1','_'},[],[{'==', '$1', From}]}]
	 end,
    case TType of
	X when X == bag; X == duplicate_bag ->
	    %erlang:display(From),
	    case From rem 2 of
		0 ->
		    2 = ets:select_delete(Table,MS);
		_ ->
		    0 = ets:select_delete(Table,MS)
	    end;
	_ ->
	    1 = ets:select_delete(Table,MS)
    end,
    Next = if
	       From < To ->
		   From + 1;
	       true ->
		   From - 1
	   end,
    successive_delete(Table, Next, To, Type,TType).

gen_dets_filename(Config,N) ->
    filename:join(?config(priv_dir,Config),
		  "testdets_" ++ integer_to_list(N) ++ ".dets").

otp_6842_select_1000(Config) when is_list(Config) -> 
    ?line Tab = ets_new(xxx,[ordered_set]),
    ?line [ets:insert(Tab,{X,X}) || X <- lists:seq(1,10000)],
    ?line AllTrue = lists:duplicate(10,true),
    ?line AllTrue = 
	[ length(
	    element(1,
		    ets:select(Tab,[{'_',[],['$_']}],X*1000))) =:= 
	  X*1000 || X <- lists:seq(1,10) ],
    ?line Sequences = [[1000,1000,1000,1000,1000,1000,1000,1000,1000,1000],
		 [2000,2000,2000,2000,2000],
		 [3000,3000,3000,1000],
		 [4000,4000,2000],
		 [5000,5000],
		 [6000,4000],
		 [7000,3000],
		 [8000,2000],
		 [9000,1000],
		 [10000]],
    ?line AllTrue = [ check_seq(Tab, ets:select(Tab,[{'_',[],['$_']}],hd(L)),L) || 
		  L <- Sequences ],
    ?line ets:delete(Tab),
    ok.

check_seq(_,'$end_of_table',[]) ->
    true;
check_seq(Tab,{L,C},[H|T]) when length(L) =:= H ->
    check_seq(Tab, ets:select(C),T);
check_seq(A,B,C) ->
    erlang:display({A,B,C}),
    false.

otp_6338(Config) when is_list(Config) ->
    L = binary_to_term(<<131,108,0,0,0,2,104,2,108,0,0,0,2,103,100,0,19,112,112,98,49,95,98,115,49,50,64,98,108,97,100,101,95,48,95,53,0,0,33,50,0,0,0,4,1,98,0,0,23,226,106,100,0,4,101,120,105,116,104,2,108,0,0,0,2,104,2,100,0,3,115,98,109,100,0,19,112,112,98,50,95,98,115,49,50,64,98,108,97,100,101,95,48,95,56,98,0,0,18,231,106,100,0,4,114,101,99,118,106>>),
    T = ets_new(xxx,[ordered_set]),
    lists:foreach(fun(X) -> ets:insert(T,X) end,L),
    [[4839,recv]] = ets:match(T,{[{sbm,ppb2_bs12@blade_0_8},'$1'],'$2'}),
    ets:delete(T).

%% Elements could come in the wrong order in a bag if a rehash occurred.
otp_5340(Config) when is_list(Config) ->
    repeat_for_opts(otp_5340_do).

otp_5340_do(Opts) ->
    N = 3000,
    T = ets_new(otp_5340, [bag,public | Opts]),
    Ids = [1,2,3,4,5],
    [w(T, N, Id) || Id <- Ids],
    verify(T, Ids),
    ets:delete(T).

w(_,0, _) -> ok;
w(T,N, Id) -> 
    ets:insert(T, {N, Id}),
    w(T,N-1,Id).
    
verify(T, Ids) ->
    List = my_tab_to_list(T),
    Errors = lists:filter(fun(Bucket) ->
				  verify2(Bucket, Ids)
			  end, List),
    case Errors of
	[] ->
	    ok;
	_ ->
	    io:format("Failed:\n~p\n", [Errors]),
	    ?t:fail()
    end.

verify2([{_N,Id}|RL], [Id|R]) ->
    verify2(RL,R);
verify2([],[]) -> false;
verify2(_Err, _) ->
    true.

otp_7665(doc) -> ["delete_object followed by delete on fixed bag failed to delete objects."];
otp_7665(suite) -> [];
otp_7665(Config) when is_list(Config) ->
    repeat_for_opts(otp_7665_do).

otp_7665_do(Opts) ->
    Tab = ets_new(otp_7665,[bag | Opts]),
    Min = 0,
    Max = 10,
    lists:foreach(fun(N)-> otp_7665_act(Tab,Min,Max,N) end,
		  lists:seq(Min,Max)),
    ?line true = ets:delete(Tab).
    
otp_7665_act(Tab,Min,Max,DelNr) ->
    List1 = [{key,N} || N <- lists:seq(Min,Max)],
    ?line true = ets:insert(Tab, List1),
    ?line true = ets:safe_fixtable(Tab, true),
    ?line true = ets:delete_object(Tab, {key,DelNr}),
    List2 = lists:delete({key,DelNr}, List1),

    %% Now verify that we find all remaining objects
    ?line List2 = ets:lookup(Tab,key),
    ?line EList2 = lists:map(fun({key,N})-> N end, 
			     List2),
    ?line EList2 = ets:lookup_element(Tab,key,2),    
    ?line true = ets:delete(Tab, key),
    ?line [] = ets:lookup(Tab, key),
    ?line true = ets:safe_fixtable(Tab, false),
    ok.

%% Whitebox testing of meta name table hashing.
meta_wb(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    repeat_for_opts(meta_wb_do),
    ?line verify_etsmem(EtsMem).


meta_wb_do(Opts) ->
    %% Do random new/delete/rename of colliding named tables
    Names0 = [pioneer | colliding_names(pioneer)],

    %% Remove any names that happen to exist as tables already
    Names = lists:filter(fun(Name) -> ets:info(Name) == undefined end,
                         Names0),
    Len = length(Names),
    OpFuns = {fun meta_wb_new/4, fun meta_wb_delete/4, fun meta_wb_rename/4},

    ?line true = (Len >= 3),

    io:format("Colliding names = ~p\n",[Names]),
    F = fun(0,_,_) -> ok;
	   (N,Tabs,Me) -> Name1 = lists:nth(random:uniform(Len),Names), 
			  Name2 = lists:nth(random:uniform(Len),Names), 
			  Op = element(random:uniform(3),OpFuns),
			  NTabs = Op(Name1, Name2, Tabs, Opts),
			  Me(N-1,NTabs,Me) 
	end,
    F(Len*100, [], F),

    % cleanup
    lists:foreach(fun(Name)->catch ets:delete(Name) end,
		  Names).
    
meta_wb_new(Name, _, Tabs, Opts) ->
    case (catch ets_new(Name,[named_table|Opts])) of
	Name ->
	    ?line false = lists:member(Name, Tabs),
	    [Name | Tabs];	
	{'EXIT',{badarg,_}} ->
	    ?line true = lists:member(Name, Tabs),
	    Tabs
    end.
meta_wb_delete(Name, _, Tabs, _) ->
    case (catch ets:delete(Name)) of
	true ->
	    ?line true = lists:member(Name, Tabs),
	    lists:delete(Name, Tabs);
	{'EXIT',{badarg,_}} ->
	    ?line false = lists:member(Name, Tabs),
	    Tabs
    end.
meta_wb_rename(Old, New, Tabs, _) ->
    case (catch ets:rename(Old,New)) of
	New ->
	    ?line true = lists:member(Old, Tabs)
		andalso not lists:member(New, Tabs),
	    [New | lists:delete(Old, Tabs)];
	{'EXIT',{badarg,_}} ->
	    ?line true = not lists:member(Old, Tabs) 
		orelse lists:member(New,Tabs),
	    Tabs
    end.
	    
    
colliding_names(Name) ->
    erts_debug:set_internal_state(colliding_names, {Name,5}).


%% OTP_6913: Grow and shrink.

grow_shrink(Config) when is_list(Config) ->
    ?line EtsMem = etsmem(),
    ?line grow_shrink_0(lists:seq(3071, 5000), EtsMem),
    ?line verify_etsmem(EtsMem).

grow_shrink_0([N|Ns], EtsMem) ->
    ?line grow_shrink_1(N, [set]),
    ?line grow_shrink_1(N, [ordered_set]),
    %% Verifying ets-memory here takes too long time, since
    %% lock-free allocators were introduced...
    %% ?line verify_etsmem(EtsMem),
    grow_shrink_0(Ns, EtsMem);
grow_shrink_0([], _) -> ok.

grow_shrink_1(N, Flags) ->
    ?line T = ets_new(a, Flags),
    ?line grow_shrink_2(N, N, T),
    ?line ets:delete(T).

grow_shrink_2(0, Orig, T) ->
    List = [{I,a} || I <- lists:seq(1, Orig)],
    List = lists:sort(ets:tab2list(T)),
    grow_shrink_3(Orig, T);
grow_shrink_2(N, Orig, T) ->
    true = ets:insert(T, {N,a}),
    grow_shrink_2(N-1, Orig, T).

grow_shrink_3(0, T) ->
    [] = ets:tab2list(T);
grow_shrink_3(N, T) ->
    true = ets:delete(T, N),
    grow_shrink_3(N-1, T).
    
grow_pseudo_deleted(doc) -> ["Grow a table that still contains pseudo-deleted objects"];
grow_pseudo_deleted(suite) -> [];
grow_pseudo_deleted(Config) when is_list(Config) ->
    only_if_smp(fun() -> grow_pseudo_deleted_do() end).

grow_pseudo_deleted_do() ->
    lists:foreach(fun(Type) -> grow_pseudo_deleted_do(Type) end,
		  [set,bag,duplicate_bag]).

grow_pseudo_deleted_do(Type) ->
    process_flag(scheduler,1),
    Self = self(),
    ?line T = ets_new(kalle,[Type,public,{write_concurrency,true}]),
    Mod = 7, Mult = 10000,
    filltabint(T,Mod*Mult),
    ?line true = ets:safe_fixtable(T,true),
    ?line Mult = ets:select_delete(T,
				   [{{'$1', '_'},
				     [{'=:=', {'rem', '$1', Mod}, 0}],
				     [true]}]),
    Left = Mult*(Mod-1),
    ?line Left = ets:info(T,size),
    ?line Mult = get_kept_objects(T),
    filltabstr(T,Mult),
    my_spawn_opt(fun()-> ?line true = ets:info(T,fixed),
			 Self ! start,
			 io:format("Starting to filltabstr... ~p\n",[now()]),
			 filltabstr(T,Mult,Mult+10000),
			 io:format("Done with filltabstr. ~p\n",[now()]),
			 Self ! done 
		 end, [link, {scheduler,2}]),
    ?line start = receive_any(),
    io:format("Unfixing table...~p nitems=~p\n",[now(),ets:info(T,size)]),
    ?line true = ets:safe_fixtable(T,false),
    io:format("Unfix table done. ~p nitems=~p\n",[now(),ets:info(T,size)]),
    ?line false = ets:info(T,fixed),
    ?line 0 = get_kept_objects(T),
    ?line done = receive_any(),
    %%verify_table_load(T), % may fail if concurrency is poor (genny)
    ets:delete(T),
    process_flag(scheduler,0).

shrink_pseudo_deleted(doc) -> ["Shrink a table that still contains pseudo-deleted objects"];
shrink_pseudo_deleted(suite) -> [];
shrink_pseudo_deleted(Config) when is_list(Config) ->
    only_if_smp(fun()->shrink_pseudo_deleted_do() end).

shrink_pseudo_deleted_do() ->
    lists:foreach(fun(Type) -> shrink_pseudo_deleted_do(Type) end,
		  [set,bag,duplicate_bag]).

shrink_pseudo_deleted_do(Type) ->
    process_flag(scheduler,1),
    Self = self(),
    ?line T = ets_new(kalle,[Type,public,{write_concurrency,true}]),
    Half = 10000,
    filltabint(T,Half*2),
    ?line true = ets:safe_fixtable(T,true),
    ?line Half = ets:select_delete(T,
				   [{{'$1', '_'},
				     [{'>', '$1', Half}],
				     [true]}]),    
    ?line Half = ets:info(T,size),
    ?line Half = get_kept_objects(T),
    my_spawn_opt(fun()-> ?line true = ets:info(T,fixed),
			 Self ! start,
			 io:format("Starting to delete... ~p\n",[now()]),
			 del_one_by_one_set(T,1,Half+1),
			 io:format("Done with delete. ~p\n",[now()]),
			 Self ! done 
		 end, [link, {scheduler,2}]),
    ?line start = receive_any(),
    io:format("Unfixing table...~p nitems=~p\n",[now(),ets:info(T,size)]),
    ?line true = ets:safe_fixtable(T,false),
    io:format("Unfix table done. ~p nitems=~p\n",[now(),ets:info(T,size)]),
    ?line false = ets:info(T,fixed),
    ?line 0 = get_kept_objects(T),
    ?line done = receive_any(),
    %%verify_table_load(T), % may fail if concurrency is poor (genny)
    ets:delete(T),
    process_flag(scheduler,0).

    

meta_lookup_unnamed_read(suite) -> [];
meta_lookup_unnamed_read(Config) when is_list(Config) ->
    InitF = fun(_) -> Tab = ets_new(unnamed,[]),
		     true = ets:insert(Tab,{key,data}),
		     Tab
	    end,
    ExecF = fun(Tab) -> [{key,data}] = ets:lookup(Tab,key),
			Tab		       
	    end,
    FiniF = fun(Tab) -> true = ets:delete(Tab)
	    end,
    run_workers(InitF,ExecF,FiniF,10000).

meta_lookup_unnamed_write(suite) -> [];
meta_lookup_unnamed_write(Config) when is_list(Config) ->
    InitF = fun(_) -> Tab = ets_new(unnamed,[]),
			  {Tab,0}
	    end,
    ExecF = fun({Tab,N}) -> true = ets:insert(Tab,{key,N}),
			    {Tab,N+1}
	    end,
    FiniF = fun({Tab,_}) -> true = ets:delete(Tab)
	    end,
    run_workers(InitF,ExecF,FiniF,10000).

meta_lookup_named_read(suite) -> [];
meta_lookup_named_read(Config) when is_list(Config) ->
    InitF = fun([ProcN|_]) -> Name = list_to_atom(integer_to_list(ProcN)),
			      Tab = ets_new(Name,[named_table]),
			      true = ets:insert(Tab,{key,data}),
			      Tab
	    end,
    ExecF = fun(Tab) -> [{key,data}] = ets:lookup(Tab,key),
			Tab		       
	    end,
    FiniF = fun(Tab) -> true = ets:delete(Tab)
	    end,
    run_workers(InitF,ExecF,FiniF,10000).

meta_lookup_named_write(suite) -> [];
meta_lookup_named_write(Config) when is_list(Config) ->
    InitF = fun([ProcN|_]) -> Name = list_to_atom(integer_to_list(ProcN)),
			  Tab = ets_new(Name,[named_table]),
			  {Tab,0}
	    end,
    ExecF = fun({Tab,N}) -> true = ets:insert(Tab,{key,N}),
			    {Tab,N+1}
	    end,
    FiniF = fun({Tab,_}) -> true = ets:delete(Tab)
	    end,
    run_workers(InitF,ExecF,FiniF,10000).

meta_newdel_unnamed(suite) -> [];
meta_newdel_unnamed(Config) when is_list(Config) ->
    InitF = fun(_) -> ok end,
    ExecF = fun(_) -> Tab = ets_new(unnamed,[]),
		      true = ets:delete(Tab)
	    end,
    FiniF = fun(_) -> ok end,
    run_workers(InitF,ExecF,FiniF,10000).

meta_newdel_named(suite) -> [];
meta_newdel_named(Config) when is_list(Config) ->
    InitF = fun([ProcN|_]) -> list_to_atom(integer_to_list(ProcN))
	    end,
    ExecF = fun(Name) -> Name = ets_new(Name,[named_table]),
			 true = ets:delete(Name),
			 Name
	    end,
    FiniF = fun(_) -> ok end,
    run_workers(InitF,ExecF,FiniF,10000).

smp_insert(doc) -> ["Concurrent insert's on same table"];
smp_insert(suite) -> [];
smp_insert(Config) when is_list(Config) ->
    ets_new(smp_insert,[named_table,public,{write_concurrency,true}]),
    InitF = fun(_) -> ok end,
    ExecF = fun(_) -> true = ets:insert(smp_insert,{random:uniform(10000)})
	    end,
    FiniF = fun(_) -> ok end,
    run_workers(InitF,ExecF,FiniF,100000),
    verify_table_load(smp_insert),
    ets:delete(smp_insert).

smp_fixed_delete(doc) -> ["Concurrent delete's on same fixated table"];
smp_fixed_delete(suite) -> [];
smp_fixed_delete(Config) when is_list(Config) ->
    only_if_smp(fun()->smp_fixed_delete_do() end).

smp_fixed_delete_do() ->
    T = ets_new(foo,[public,{write_concurrency,true}]),
    %%Mem = ets:info(T,memory),
    NumOfObjs = 100000,
    filltabint(T,NumOfObjs),
    ets:safe_fixtable(T,true),
    Buckets = num_of_buckets(T),
    InitF = fun([ProcN,NumOfProcs|_]) -> {ProcN,NumOfProcs} end,
    ExecF = fun({Key,_}) when Key > NumOfObjs -> 
		    [end_of_work];
	       ({Key,Increment}) -> 
		    true = ets:delete(T,Key),
		    {Key+Increment,Increment}
	    end,
    FiniF = fun(_) -> ok end,
    run_workers_do(InitF,ExecF,FiniF,NumOfObjs),
    ?line 0 = ets:info(T,size),
    ?line true = ets:info(T,fixed),
    ?line Buckets = num_of_buckets(T),
    ?line NumOfObjs = get_kept_objects(T),
    ets:safe_fixtable(T,false),
    %% Will fail as unfix does not shrink the table:
    %%?line Mem = ets:info(T,memory),
    %%verify_table_load(T),
    ets:delete(T).

num_of_buckets(T) ->
    ?line element(1,ets:info(T,stats)).

smp_unfix_fix(doc) -> ["Fixate hash table while other process is busy doing unfix"];
smp_unfix_fix(suite) -> [];
smp_unfix_fix(Config) when is_list(Config) ->
    only_if_smp(fun()-> smp_unfix_fix_do() end).

smp_unfix_fix_do() ->
    process_flag(scheduler,1),
    Parent = self(),
    T = ets_new(foo,[public,{write_concurrency,true}]),
    %%Mem = ets:info(T,memory),
    NumOfObjs = 100000,
    Deleted = 50000,    
    filltabint(T,NumOfObjs),
    ets:safe_fixtable(T,true),
    Buckets = num_of_buckets(T),
    ?line Deleted = ets:select_delete(T,[{{'$1', '_'},
					  [{'=<','$1', Deleted}],
					  [true]}]),
    ?line Buckets = num_of_buckets(T),
    Left = NumOfObjs - Deleted,
    ?line Left = ets:info(T,size),
    ?line true = ets:info(T,fixed),
    ?line Deleted = get_kept_objects(T),
    
    {Child, Mref} = 
      my_spawn_opt(fun()-> ?line true = ets:info(T,fixed),
			   Parent ! start,
			   io:format("Child waiting for table to be unfixed... now=~p mem=~p\n",
				     [now(),ets:info(T,memory)]),
			   repeat_while(fun()-> ets:info(T,fixed) end),
			   io:format("Table unfixed. Child Fixating! now=~p mem=~p\n",
				     [now(),ets:info(T,memory)]),    
			   ?line true = ets:safe_fixtable(T,true),
			   repeat_while(fun(Key) when Key =< NumOfObjs -> 
						ets:delete(T,Key), {true,Key+1};
					   (Key) -> {false,Key}
					end,
					Deleted),
			   ?line 0 = ets:info(T,size),
			   ?line true = get_kept_objects(T) >= Left,		      
			   ?line done = receive_any()
		   end, 
		   [link, monitor, {scheduler,2}]),
    
    ?line start = receive_any(),        
    ?line true = ets:info(T,fixed),
    io:format("Parent starting to unfix... ~p\n",[now()]),
    ets:safe_fixtable(T,false),
    io:format("Parent done with unfix. ~p\n",[now()]),
    Child ! done,
    {'DOWN', Mref, process, Child, normal} = receive_any(),
    ?line false = ets:info(T,fixed),
    ?line 0 = get_kept_objects(T),
    %%verify_table_load(T),
    ets:delete(T),
    process_flag(scheduler,0).

otp_8166(doc) -> ["Unsafe unfix was done by trapping select/match"];
otp_8166(suite) -> [];
otp_8166(Config) when is_list(Config) ->
    only_if_smp(3, fun()-> otp_8166_do(false),
			   otp_8166_do(true)
		   end).

otp_8166_do(WC) ->
    %% Bug scenario: One process segv while reading the table because another
    %% process is doing unfix without write-lock at the end of a trapping match_object.
    process_flag(scheduler,1),
    T = ets_new(foo,[public, {write_concurrency,WC}]),
    NumOfObjs = 3000,  %% Need more than 1000 live objects for match_object to trap one time
    Deleted = NumOfObjs div 2,
    filltabint(T,NumOfObjs),
    {ReaderPid, ReaderMref} = 
 	my_spawn_opt(fun()-> otp_8166_reader(T,NumOfObjs) end, 
		     [link, monitor, {scheduler,2}]),
    {ZombieCrPid, ZombieCrMref} = 
 	my_spawn_opt(fun()-> otp_8166_zombie_creator(T,Deleted) end, 
		     [link, monitor, {scheduler,3}]),

    repeat(fun() -> ZombieCrPid ! {loop, self()},
		    zombies_created = receive_any(),
		    otp_8166_trapper(T, 10, ZombieCrPid)
	   end,
	   100),

    ReaderPid ! quit,
    {'DOWN', ReaderMref, process, ReaderPid, normal} = receive_any(),
    ZombieCrPid ! quit,    
    {'DOWN', ZombieCrMref, process, ZombieCrPid, normal} = receive_any(),
    ?line false = ets:info(T,fixed),
    ?line 0 = get_kept_objects(T),
    %%verify_table_load(T),
    ets:delete(T),
    process_flag(scheduler,0).

%% Keep reading the table
otp_8166_reader(T, NumOfObjs) ->
    repeat_while(fun(0) -> 
			 receive quit -> {false,done}
			 after 0 -> {true,NumOfObjs}
			 end;
		    (Key) ->
			 ets:lookup(T,Key),
			 {true, Key-1}
		 end,
		 NumOfObjs).

%% Do a match_object that will trap and thereby fixate and then unfixate the table
otp_8166_trapper(T, Try, ZombieCrPid) ->
    [] = ets:match_object(T,{'_',"Pink Unicorn"}),
    case {ets:info(T,fixed),Try} of
	{true,1} -> 
	    io:format("failed to provoke unsafe unfix, give up...\n",[]),
	    ZombieCrPid ! unfix;
	{true,_} -> 
	    io:format("trapper too fast, trying again...\n",[]),
	    otp_8166_trapper(T, Try-1, ZombieCrPid);
	{false,_} -> done
    end.	   	    


%% Fixate table and create some pseudo-deleted objects (zombies)
%% Then wait for trapper to fixate before unfixing, as we want the trappers'
%% unfix to be the one that purges the zombies.
otp_8166_zombie_creator(T,Deleted) ->
    case receive_any() of
	quit -> done;

	{loop,Pid} ->
	    filltabint(T,Deleted),
	    ets:safe_fixtable(T,true),
	    ?line Deleted = ets:select_delete(T,[{{'$1', '_'},
						  [{'=<','$1', Deleted}],
						  [true]}]),
	    Pid ! zombies_created,
	    repeat_while(fun() -> case ets:info(T,safe_fixed) of
				      {_,[_P1,_P2]} ->
					  false;
				      _ -> 
					  receive unfix -> false
					  after 0 -> true
					  end
				  end
			 end),
	    ets:safe_fixtable(T,false),
	    otp_8166_zombie_creator(T,Deleted);

	unfix ->
	    io:format("ignore unfix in outer loop?\n",[]),
	    otp_8166_zombie_creator(T,Deleted)
    end.
    
	
			 

verify_table_load(T) ->
    ?line Stats = ets:info(T,stats),
    ?line {Buckets,AvgLen,StdDev,ExpSD,_MinLen,_MaxLen,_} = Stats,
    ?line ok = if
		   AvgLen > 7 ->
		       io:format("Table overloaded: Stats=~p\n~p\n",
				 [Stats, ets:info(T)]),
		       false;

		   Buckets>256, AvgLen < 6 ->
		       io:format("Table underloaded: Stats=~p\n~p\n",
				 [Stats, ets:info(T)]),
		       false;
		   
		   StdDev > ExpSD*2 ->
		       io:format("Too large standard deviation (poor hashing?),"
				 " stats=~p\n~p\n",[Stats, ets:info(T)]),
		       false;

		   true -> 
		       io:format("Stats = ~p\n",[Stats]),
		       ok
	       end.
    
		  
otp_8732(doc) -> ["ets:select on a tree with NIL key object"];
otp_8732(Config) when is_list(Config) ->
    Tab = ets_new(noname,[ordered_set]),
    filltabstr(Tab,999),
    ets:insert(Tab,{[],"nasty NIL object"}),
    ?line [] = ets:match(Tab,{'_',nomatch}), %% Will hang if bug not fixed
    ok.


smp_select_delete(suite) -> [];
smp_select_delete(doc) ->
    ["Run concurrent select_delete (and inserts) on same table."];
smp_select_delete(Config) when is_list(Config) ->
    T = ets_new(smp_select_delete,[named_table,public,{write_concurrency,true}]),
    Mod = 17,
    Zeros = erlang:make_tuple(Mod,0),
    InitF = fun(_) -> Zeros end,
    ExecF = fun(Diffs0) -> 
		    case random:uniform(20) of
			1 ->
			    Mod = 17,
			    Eq = random:uniform(Mod) - 1,
			    Deleted = ets:select_delete(T,
							[{{'_', '$1'},
							  [{'=:=', {'rem', '$1', Mod}, Eq}],
							  [true]}]),
			    Diffs1 = setelement(Eq+1, Diffs0,
						element(Eq+1,Diffs0) - Deleted),
			    Diffs1;
			_ ->
			    Key = random:uniform(10000),
			    Eq = Key rem Mod,
			    ?line case ets:insert_new(T,{Key,Key}) of
				      true ->
					  Diffs1 = setelement(Eq+1, Diffs0,
							      element(Eq+1,Diffs0)+1),
					  Diffs1;
				      false -> Diffs0
				  end
		    end	    
	    end,
    FiniF = fun(Result) -> Result end,
    Results = run_workers_do(InitF,ExecF,FiniF,20000),
    ?line TotCnts = lists:foldl(fun(Diffs, Sum) -> add_lists(Sum,tuple_to_list(Diffs)) end,
				lists:duplicate(Mod, 0), Results),
    io:format("TotCnts = ~p\n",[TotCnts]),
    ?line LeftInTab = lists:foldl(fun(N,Sum) -> Sum+N end,
				  0, TotCnts),
    io:format("LeftInTab = ~p\n",[LeftInTab]),
    ?line LeftInTab = ets:info(T,size),
    lists:foldl(fun(Cnt,Eq) -> 
			WasCnt = ets:select_count(T,
						  [{{'_', '$1'},
						    [{'=:=', {'rem', '$1', Mod}, Eq}],
						    [true]}]),
			io:format("~p: ~p =?= ~p\n",[Eq,Cnt,WasCnt]),
			?line Cnt = WasCnt,
			Eq+1
		end, 
		0, TotCnts),
    verify_table_load(T),
    ?line LeftInTab = ets:select_delete(T, [{{'$1','$1'}, [], [true]}]),
    ?line 0 = ets:info(T,size),
    ?line false = ets:info(T,fixed),
    ets:delete(T).

types(doc) -> ["Test different types"];
types(Config) when is_list(Config) ->
    init_externals(),
    repeat_for_opts(types_do,[[set,ordered_set],compressed]).

types_do(Opts) ->
    EtsMem = etsmem(),
    ?line T = ets_new(xxx,Opts),
    Fun = fun(Term) ->
            ets:insert(T,{Term}),
            ?line [{Term}] = ets:lookup(T,Term),
            ets:insert(T,{Term,xxx}),
            ?line [{Term,xxx}] = ets:lookup(T,Term),
            ets:insert(T,{Term,"xxx"}),
            ?line [{Term,"xxx"}] = ets:lookup(T,Term),
            ets:insert(T,{xxx,Term}),
            ?line [{xxx,Term}] = ets:lookup(T,xxx),
            ets:insert(T,{"xxx",Term}),
            ?line [{"xxx",Term}] = ets:lookup(T,"xxx"),
            ets:delete_all_objects(T),
            ?line 0 = ets:info(T,size)
          end,
    test_terms(Fun, strict),
    ets:delete(T),
    ?line verify_etsmem(EtsMem).


%% OTP-9932: Memory overwrite when inserting large integers in compressed bag.
%% Will crash with segv on 64-bit opt if not fixed.
otp_9932(Config) when is_list(Config) ->
    T = ets:new(xxx, [bag, compressed]),    
    Fun = fun(N) -> 
		  Key = {1316110174588445 bsl N,1316110174588583 bsl N},
		  S = {Key, Key},
		  true = ets:insert(T, S),
		  [S] = ets:lookup(T, Key),
		  true = ets:insert(T, S),
		  [S] = ets:lookup(T, Key)
	  end,
    lists:foreach(Fun, lists:seq(0, 16)),
    ets:delete(T).
    

otp_9423(doc) -> ["vm-deadlock caused by race between ets:delete and others on write_concurrency table"];
otp_9423(Config) when is_list(Config) ->
    InitF = fun(_) -> {0,0} end,
    ExecF = fun({S,F}) -> 
		    receive 
			stop -> 
			    io:format("~p got stop\n", [self()]),
			    [end_of_work | {"Succeded=",S,"Failed=",F}]
		    after 0 ->
			    %%io:format("~p (~p) doing lookup\n", [self(), {S,F}]),
			    try ets:lookup(otp_9423, key) of
				[] -> {S+1,F}
			    catch
				error:badarg -> {S,F+1}
			    end
		    end
	    end,
    FiniF = fun(R) -> R end,
    case run_workers(InitF, ExecF, FiniF, infinite, 1) of
	Pids when is_list(Pids) ->
	    %%[P ! start || P <- Pids],
	    repeat(fun() -> ets:new(otp_9423, [named_table, public, {write_concurrency,true}]),
			    ets:delete(otp_9423)
		   end, 10000),
	    [P ! stop || P <- Pids],
	    wait_pids(Pids),
	    ok;
	
	Skipped -> Skipped
    end.
	    

%% Corrupted binary in compressed table
otp_10182(Config) when is_list(Config) ->
    Bin = <<"aHR0cDovL2hvb3RzdWl0ZS5jb20vYy9wcm8tYWRyb2xsLWFi">>,
    Key = {test, Bin},
    Value = base64:decode(Bin),
    In = {Key,Value},
    Db = ets:new(undefined, [set, protected, {read_concurrency, true}, compressed]),
    ets:insert(Db, In),
    [Out] = ets:lookup(Db, Key),
    io:format("In :  ~p\nOut: ~p\n", [In,Out]),
    ets:delete(Db),
    In = Out.

    
    

%
% Utility functions:
%

add_lists(L1,L2) ->     
    add_lists(L1,L2,[]).
add_lists([],[],Acc) ->
    lists:reverse(Acc);
add_lists([E1|T1], [E2|T2], Acc) ->
    add_lists(T1, T2, [E1+E2 | Acc]).    

run_workers(InitF,ExecF,FiniF,Laps) ->
    run_workers(InitF,ExecF,FiniF,Laps, 0).
run_workers(InitF,ExecF,FiniF,Laps, Exclude) ->
    case erlang:system_info(smp_support) of
	true ->
	    run_workers_do(InitF,ExecF,FiniF,Laps, Exclude);
	false ->
	    {skipped,"No smp support"}
    end.

run_workers_do(InitF,ExecF,FiniF,Laps) ->
    run_workers_do(InitF,ExecF,FiniF,Laps, 0).
run_workers_do(InitF,ExecF,FiniF,Laps, Exclude) ->
    ?line NumOfProcs = case erlang:system_info(schedulers) of
			   N when (N > Exclude) -> N - Exclude
		       end,
    io:format("smp starting ~p workers\n",[NumOfProcs]),
    Seeds = [{ProcN,random:uniform(9999)} || ProcN <- lists:seq(1,NumOfProcs)],
    Parent = self(),
    Pids = [my_spawn_link(fun()-> worker(Seed,InitF,ExecF,FiniF,Laps,Parent,NumOfProcs) end)
	    || Seed <- Seeds],
    case Laps of
	infinite -> Pids;
	_ -> wait_pids(Pids)
    end.
	    
worker({ProcN,Seed}, InitF, ExecF, FiniF, Laps, Parent, NumOfProcs) ->
    io:format("smp worker ~p, seed=~p~n",[self(),Seed]),
    random:seed(Seed,Seed,Seed),
    State1 = InitF([ProcN, NumOfProcs]),
    State2 = worker_loop(Laps, ExecF, State1),
    Result = FiniF(State2),
    io:format("worker ~p done\n",[self()]),
    Parent ! {self(), Result}.

worker_loop(0, _, State) ->
    State;
worker_loop(_, _, [end_of_work|State]) ->
    State;
worker_loop(infinite, ExecF, State) ->
    worker_loop(infinite,ExecF,ExecF(State));
worker_loop(N, ExecF, State) ->
    worker_loop(N-1,ExecF,ExecF(State)).
    
wait_pids(Pids) -> 
    wait_pids(Pids,[]).
wait_pids([],Acc) -> 
    Acc;
wait_pids(Pids, Acc) ->
    receive
	{Pid,Result} ->
	    ?line true = lists:member(Pid,Pids),
	    Others = lists:delete(Pid,Pids),
	    io:format("wait_pid got ~p from ~p, still waiting for ~p\n",[Result,Pid,Others]),
	    wait_pids(Others,[Result | Acc])
    end.




my_tab_to_list(Ts) ->
    Key = ets:first(Ts),
    my_tab_to_list(Ts,ets:next(Ts,Key),[ets:lookup(Ts, Key)]).

my_tab_to_list(_Ts,'$end_of_table', Acc) -> lists:reverse(Acc);
my_tab_to_list(Ts,Key, Acc) ->
    my_tab_to_list(Ts,ets:next(Ts,Key),[ets:lookup(Ts, Key)| Acc]).


wait_for_memory_deallocations() ->
    try
	erts_debug:set_internal_state(wait, deallocations)
    catch
	error:undef ->
	    erts_debug:set_internal_state(available_internal_state, true),
	    wait_for_memory_deallocations()
    end.
	    

etsmem() ->
    wait_for_memory_deallocations(),

    AllTabs = lists:map(fun(T) -> {T,ets:info(T,name),ets:info(T,size),
				   ets:info(T,memory),ets:info(T,type)} 
			end, ets:all()),

    EtsAllocInfo = erlang:system_info({allocator,ets_alloc}),
    ErlangMemoryEts = try erlang:memory(ets) catch error:notsup -> notsup end,

    Mem =
    {ErlangMemoryEts,
     case EtsAllocInfo of
	 false -> undefined;
	 MemInfo ->
	     CS = lists:foldl(
		    fun ({instance, _, L}, Acc) ->
			    {value,{mbcs,MBCS}} = lists:keysearch(mbcs, 1, L),
			    {value,{sbcs,SBCS}} = lists:keysearch(sbcs, 1, L),
			    NewAcc = [MBCS, SBCS | Acc],
			    case lists:keysearch(mbcs_pool, 1, L) of
				{value,{mbcs_pool, MBCS_POOL}} ->
				    [MBCS_POOL|NewAcc];
				_ -> NewAcc
			    end
		    end,
		    [],
		    MemInfo),
	     lists:foldl(
	       fun(L, {Bl0,BlSz0}) ->
		       {value,BlTup} = lists:keysearch(blocks, 1, L),
		       blocks = element(1, BlTup),
		       Bl = element(2, BlTup),
		       {value,BlSzTup} = lists:keysearch(blocks_size, 1, L),	
		       blocks_size = element(1, BlSzTup),
		       BlSz = element(2, BlSzTup),
		       {Bl0+Bl,BlSz0+BlSz}
	       end, {0,0}, CS)
     end},
     {Mem,AllTabs}.

verify_etsmem({MemInfo,AllTabs}) ->
    wait_for_test_procs(),
    case etsmem() of
	{MemInfo,_} ->
	    io:format("Ets mem info: ~p", [MemInfo]),
	    case MemInfo of
		{ErlMem,EtsAlloc} when ErlMem == notsup; EtsAlloc == undefined ->
		    %% Use 'erl +Mea max' to do more complete memory leak testing.
		    {comment,"Incomplete or no mem leak testing"};
		_ ->
		    ok
	    end;
	{MemInfo2, AllTabs2} ->
	    io:format("Expected: ~p", [MemInfo]),
	    io:format("Actual:   ~p", [MemInfo2]),
	    io:format("Changed tables before: ~p\n",[AllTabs -- AllTabs2]),
	    io:format("Changed tables after: ~p\n", [AllTabs2 -- AllTabs]),
	    ets_test_spawn_logger ! failed_memcheck,
	    {comment, "Failed memory check"}
    end.


start_loopers(N, Prio, Fun, State) ->
    lists:map(fun (_) ->
		      my_spawn_opt(fun () -> looper(Fun, State) end,
				   [{priority, Prio}, link])
	      end,
	      lists:seq(1, N)).

stop_loopers(Loopers) ->
    lists:foreach(fun (P) ->
			  unlink(P),
			  exit(P, bang)
		  end,
		  Loopers),
    ok.

looper(Fun, State) ->
    looper(Fun, Fun(State)).

spawn_logger(Procs, FailedMemchecks) ->
    receive
	{new_test_proc, Proc} ->
	    spawn_logger([Proc|Procs], FailedMemchecks);
	{sync_test_procs, Kill, From} ->
	    lists:foreach(fun (Proc) when From == Proc ->
				  ok;
			      (Proc) ->
				  Mon = erlang:monitor(process, Proc),
				  receive
				      {'DOWN', Mon, _, _, _} ->
					  ok
				  after 0 ->
					  case Kill of
					      true -> exit(Proc, kill);
					      _ -> ok
					  end,
					  erlang:display({"Waiting for 'DOWN' from", Proc,
							  process_info(Proc), pid_status(Proc)}),
					  receive
					      {'DOWN', Mon, _, _, _} ->
						  ok
					  end
				  end
			  end, Procs),
	    From ! test_procs_synced,
	    spawn_logger([From], FailedMemchecks);

	failed_memcheck ->
	    spawn_logger(Procs, FailedMemchecks+1);

	{Pid, get_failed_memchecks} ->
	    Pid ! {get_failed_memchecks, FailedMemchecks},
	    spawn_logger(Procs, FailedMemchecks)
    end.

pid_status(Pid) ->
    try
	erts_debug:get_internal_state({process_status, Pid})
    catch
	error:undef ->
	    erts_debug:set_internal_state(available_internal_state, true),
	    pid_status(Pid)
    end. 

start_spawn_logger() ->
    case whereis(ets_test_spawn_logger) of
	Pid when is_pid(Pid) -> true;
	_ -> register(ets_test_spawn_logger,
		      spawn_opt(fun () -> spawn_logger([], 0) end,
				[{priority, max}]))
    end.

%% restart_spawn_logger() ->
%%     stop_spawn_logger(),
%%     start_spawn_logger().

stop_spawn_logger() ->
    Mon = erlang:monitor(process, ets_test_spawn_logger),
    (catch exit(whereis(ets_test_spawn_logger), kill)),
    receive {'DOWN', Mon, _, _, _} -> ok end.

wait_for_test_procs() ->
    wait_for_test_procs(false).

wait_for_test_procs(Kill) ->
    ets_test_spawn_logger ! {sync_test_procs, Kill, self()},
    receive test_procs_synced -> ok end.

log_test_proc(Proc) when is_pid(Proc) ->
    ets_test_spawn_logger ! {new_test_proc, Proc},
    Proc.

my_spawn(Fun) -> log_test_proc(spawn(Fun)).
%%my_spawn(M,F,A) -> log_test_proc(spawn(M,F,A)).
%%my_spawn(N,M,F,A) -> log_test_proc(spawn(N,M,F,A)).

my_spawn_link(Fun) -> log_test_proc(spawn_link(Fun)).
my_spawn_link(M,F,A) -> log_test_proc(spawn_link(M,F,A)).
%%my_spawn_link(N,M,F,A) -> log_test_proc(spawn_link(N,M,F,A)).

my_spawn_opt(Fun,Opts) ->
    case spawn_opt(Fun,Opts) of
	Pid when is_pid(Pid) -> log_test_proc(Pid);
	{Pid, _} = Res when is_pid(Pid) -> log_test_proc(Pid), Res
    end.

my_spawn_monitor(Fun) ->
    Res = spawn_monitor(Fun),
    {Pid, _} = Res,
    log_test_proc(Pid),
    Res.

repeat(_Fun, 0) ->
    ok;
repeat(Fun, N) ->
    Fun(),
    repeat(Fun, N-1).

repeat_while(Fun) ->
    case Fun() of
	true -> repeat_while(Fun);
	false -> false
    end.

repeat_while(Fun, Arg0) ->
    case Fun(Arg0) of
	{true,Arg1} -> repeat_while(Fun,Arg1);
	{false,Ret} -> Ret
    end.

%% Some (but not all) permutations of List
repeat_for_permutations(Fun, List) ->
    repeat_for_permutations(Fun, List, length(List)-1).
repeat_for_permutations(Fun, List, 0) ->
    Fun(List);
repeat_for_permutations(Fun, List, N) ->
    {A,B} = lists:split(N, List),
    L1 = B++A,
    L2 = lists:reverse(L1),
    L3 = B++lists:reverse(A),
    L4 = lists:reverse(B)++A,
    Fun(L1), Fun(L2), Fun(L3), Fun(L4),
    repeat_for_permutations(Fun, List, N-1).

receive_any() ->
    receive M ->
	    io:format("Process ~p got msg ~p\n", [self(),M]),
	    M
    end.

receive_any_spinning() ->
    receive_any_spinning(1000000).
receive_any_spinning(Loops) ->
    receive_any_spinning(Loops,Loops,1).    
receive_any_spinning(Loops,0,Tries) ->
    receive M ->
	    io:format("Spinning process ~p got msg ~p after ~p tries\n", [self(),M,Tries]),
	    M
    after 0 ->
	    receive_any_spinning(Loops, Loops, Tries+1)
    end;
receive_any_spinning(Loops, N, Tries) when N>0 ->
    receive_any_spinning(Loops, N-1, Tries).
    


spawn_monitor_with_pid(Pid, Fun) when is_pid(Pid) ->
    spawn_monitor_with_pid(Pid, Fun, 10).

spawn_monitor_with_pid(_, _, 0) ->
    failed;
spawn_monitor_with_pid(Pid, Fun, N) ->
    case my_spawn(fun()-> case self() of
			      Pid -> Fun();
			      _ -> die
			  end
		  end) of
	Pid ->
	    {Pid, erlang:monitor(process, Pid)};
	_Other ->
	    spawn_monitor_with_pid(Pid,Fun,N-1)
    end.


only_if_smp(Func) ->
    only_if_smp(2, Func).
only_if_smp(Schedulers, Func) ->
    case {erlang:system_info(smp_support),
	  erlang:system_info(schedulers_online)} of
	{false,_} -> {skip,"No smp support"};
	{true,N} when N < Schedulers -> {skip,"Too few schedulers online"};
	{true,_} -> Func()
    end.

%% Copy-paste from emulator/test/binary_SUITE.erl
-define(heap_binary_size, 64).
test_terms(Test_Func, Mode) ->
    garbage_collect(),
    ?line Pib0 = process_info(self(),binary),

    ?line Test_Func(atom),
    ?line Test_Func(''),
    ?line Test_Func('a'),
    ?line Test_Func('ab'),
    ?line Test_Func('abc'),
    ?line Test_Func('abcd'),
    ?line Test_Func('abcde'),
    ?line Test_Func('abcdef'),
    ?line Test_Func('abcdefg'),
    ?line Test_Func('abcdefgh'),

    ?line Test_Func(fun() -> ok end),
    X = id([a,{b,c},c]),
    Y = id({x,y,z}),
    Z = id(1 bsl 8*257),
    ?line Test_Func(fun() -> X end),
    ?line Test_Func(fun() -> {X,Y} end),
    ?line Test_Func([fun() -> {X,Y,Z} end,
              fun() -> {Z,X,Y} end,
              fun() -> {Y,Z,X} end]),

    ?line Test_Func({trace_ts,{even_bigger,{some_data,fun() -> ok end}},{1,2,3}}),
    ?line Test_Func({trace_ts,{even_bigger,{some_data,<<1,2,3,4,5,6,7,8,9,10>>}},
              {1,2,3}}),

    ?line Test_Func(1),
    ?line Test_Func(42),
    ?line Test_Func(-23),
    ?line Test_Func(256),
    ?line Test_Func(25555),
    ?line Test_Func(-3333),

    ?line Test_Func(1.0),

    ?line Test_Func(183749783987483978498378478393874),
    ?line Test_Func(-37894183749783987483978498378478393874),
    Very_Big = very_big_num(),
    ?line Test_Func(Very_Big),
    ?line Test_Func(-Very_Big+1),

    ?line Test_Func([]),
    ?line Test_Func("abcdef"),
    ?line Test_Func([a, b, 1, 2]),
    ?line Test_Func([a|b]),

    ?line Test_Func({}),
    ?line Test_Func({1}),
    ?line Test_Func({a, b}),
    ?line Test_Func({a, b, c}),
    ?line Test_Func(list_to_tuple(lists:seq(0, 255))),
    ?line Test_Func(list_to_tuple(lists:seq(0, 256))),

    ?line Test_Func(make_ref()),
    ?line Test_Func([make_ref(), make_ref()]),

    ?line Test_Func(make_port()),

    ?line Test_Func(make_pid()),
    ?line Test_Func(make_ext_pid()),
    ?line Test_Func(make_ext_port()),
    ?line Test_Func(make_ext_ref()),

    Bin0 = list_to_binary(lists:seq(0, 14)),
    ?line Test_Func(Bin0),
    Bin1 = list_to_binary(lists:seq(0, ?heap_binary_size)),
    ?line Test_Func(Bin1),
    Bin2 = list_to_binary(lists:seq(0, ?heap_binary_size+1)),
    ?line Test_Func(Bin2),
    Bin3 = list_to_binary(lists:seq(0, 255)),
    garbage_collect(),
    Pib = process_info(self(),binary),
    ?line Test_Func(Bin3),
    garbage_collect(),
    case Mode of
	strict -> ?line Pib = process_info(self(),binary);
	skip_refc_check -> ok
    end,

    ?line Test_Func(make_unaligned_sub_binary(Bin0)),
    ?line Test_Func(make_unaligned_sub_binary(Bin1)),
    ?line Test_Func(make_unaligned_sub_binary(Bin2)),
    ?line Test_Func(make_unaligned_sub_binary(Bin3)),

    ?line Test_Func(make_sub_binary(lists:seq(42, 43))),
    ?line Test_Func(make_sub_binary([42,43,44])),
    ?line Test_Func(make_sub_binary([42,43,44,45])),
    ?line Test_Func(make_sub_binary([42,43,44,45,46])),
    ?line Test_Func(make_sub_binary([42,43,44,45,46,47])),
    ?line Test_Func(make_sub_binary([42,43,44,45,46,47,48])),
    ?line Test_Func(make_sub_binary(lists:seq(42, 49))),
    ?line Test_Func(make_sub_binary(lists:seq(0, 14))),
    ?line Test_Func(make_sub_binary(lists:seq(0, ?heap_binary_size))),
    ?line Test_Func(make_sub_binary(lists:seq(0, ?heap_binary_size+1))),
    ?line Test_Func(make_sub_binary(lists:seq(0, 255))),

    ?line Test_Func(make_unaligned_sub_binary(lists:seq(42, 43))),
    ?line Test_Func(make_unaligned_sub_binary([42,43,44])),
    ?line Test_Func(make_unaligned_sub_binary([42,43,44,45])),
    ?line Test_Func(make_unaligned_sub_binary([42,43,44,45,46])),
    ?line Test_Func(make_unaligned_sub_binary([42,43,44,45,46,47])),
    ?line Test_Func(make_unaligned_sub_binary([42,43,44,45,46,47,48])),
    ?line Test_Func(make_unaligned_sub_binary(lists:seq(42, 49))),
    ?line Test_Func(make_unaligned_sub_binary(lists:seq(0, 14))),
    ?line Test_Func(make_unaligned_sub_binary(lists:seq(0, ?heap_binary_size))),
    ?line Test_Func(make_unaligned_sub_binary(lists:seq(0, ?heap_binary_size+1))),
    ?line Test_Func(make_unaligned_sub_binary(lists:seq(0, 255))),

    %% Bit level binaries.
    ?line Test_Func(<<1:1>>),
    ?line Test_Func(<<2:2>>),
    ?line Test_Func(<<42:10>>),
    ?line Test_Func(list_to_bitstring([<<5:6>>|lists:seq(0, 255)])),

    ?line Test_Func(F = fun(A) -> 42*A end),
    ?line Test_Func(lists:duplicate(32, F)),

    ?line Test_Func(FF = fun binary_SUITE:all/1),
    ?line Test_Func(lists:duplicate(32, FF)),

    garbage_collect(),
    case Mode of
	strict -> ?line Pib0 = process_info(self(),binary);
	skip_refc_check -> ok
    end,
    ok.


id(I) -> I.

very_big_num() ->
    very_big_num(33, 1).

very_big_num(Left, Result) when Left > 0 ->
    ?line very_big_num(Left-1, Result*256);
very_big_num(0, Result) ->
    ?line Result.

make_port() ->
    ?line open_port({spawn, "efile"}, [eof]).

make_pid() ->
    ?line spawn_link(?MODULE, sleeper, []).

sleeper() ->
    ?line receive after infinity -> ok end.

make_ext_pid() ->
    {Pid, _, _} = get(externals),
    Pid.

make_ext_port() ->
    {_, Port, _} = get(externals),
    Port.
make_ext_ref() ->
    {_, _, Ref} = get(externals),
    Ref.

init_externals() ->
    case get(externals) of
	undefined ->
	    OtherNode = {gurka@sallad, 1},
	    Res = {mk_pid(OtherNode, 7645, 8123),
		   mk_port(OtherNode, 187489773),
		   mk_ref(OtherNode, [262143, 1293964255, 3291964278])},
	    put(externals, Res);

	{_,_,_} -> ok
    end.

%%
%% Node container constructor functions
%%

-define(VERSION_MAGIC,       131).
-define(PORT_EXT,            102).
-define(PID_EXT,             103).
-define(NEW_REFERENCE_EXT,   114).

uint32_be(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 32 ->
    [(Uint bsr 24) band 16#ff,
     (Uint bsr 16) band 16#ff,
     (Uint bsr 8) band 16#ff,
     Uint band 16#ff];
uint32_be(Uint) ->
    exit({badarg, uint32_be, [Uint]}).

uint16_be(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 16 ->
    [(Uint bsr 8) band 16#ff,
     Uint band 16#ff];
uint16_be(Uint) ->
    exit({badarg, uint16_be, [Uint]}).

uint8(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 8 ->
    Uint band 16#ff;
uint8(Uint) ->
    exit({badarg, uint8, [Uint]}).

mk_pid({NodeName, Creation}, Number, Serial) when is_atom(NodeName) ->
    <<?VERSION_MAGIC, NodeNameExt/binary>> = term_to_binary(NodeName),
    mk_pid({NodeNameExt, Creation}, Number, Serial);
mk_pid({NodeNameExt, Creation}, Number, Serial) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					?PID_EXT,
					NodeNameExt,
					uint32_be(Number),
					uint32_be(Serial),
					uint8(Creation)])) of
	Pid when is_pid(Pid) ->
	    Pid;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_pid, [{NodeNameExt, Creation}, Number, Serial]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.

mk_port({NodeName, Creation}, Number) when is_atom(NodeName) ->
    <<?VERSION_MAGIC, NodeNameExt/binary>> = term_to_binary(NodeName),
    mk_port({NodeNameExt, Creation}, Number);
mk_port({NodeNameExt, Creation}, Number) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ?PORT_EXT,
					      NodeNameExt,
					      uint32_be(Number),
					      uint8(Creation)])) of
	Port when is_port(Port) ->
	    Port;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_port, [{NodeNameExt, Creation}, Number]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.

mk_ref({NodeName, Creation}, Numbers) when is_atom(NodeName),
					   is_integer(Creation),
					   is_list(Numbers) ->
    <<?VERSION_MAGIC, NodeNameExt/binary>> = term_to_binary(NodeName),
    mk_ref({NodeNameExt, Creation}, Numbers);
mk_ref({NodeNameExt, Creation}, Numbers) when is_binary(NodeNameExt),
					      is_integer(Creation),
					      is_list(Numbers) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ?NEW_REFERENCE_EXT,
					      uint16_be(length(Numbers)),
					      NodeNameExt,
					      uint8(Creation),
					      lists:map(fun (N) ->
								uint32_be(N)
							end,
							Numbers)])) of
	Ref when is_reference(Ref) ->
	    Ref;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_ref, [{NodeNameExt, Creation}, Numbers]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.


make_sub_binary(Bin) when is_binary(Bin) ->
    {_,B} = split_binary(list_to_binary([0,1,3,Bin]), 3),
    B;
make_sub_binary(List) ->
    make_sub_binary(list_to_binary(List)).

make_unaligned_sub_binary(Bin0) when is_binary(Bin0) ->
    Bin1 = <<0:3,Bin0/binary,31:5>>,
    Sz = size(Bin0),
    <<0:3,Bin:Sz/binary,31:5>> = id(Bin1),
    Bin;
make_unaligned_sub_binary(List) ->
    make_unaligned_sub_binary(list_to_binary(List)).

%% Repeat test function with different combination of table options
%%       
repeat_for_opts(F) ->
    repeat_for_opts(F, [write_concurrency, read_concurrency, compressed]).

repeat_for_opts(F, OptGenList) when is_atom(F) ->
    repeat_for_opts(fun(Opts) -> ?MODULE:F(Opts) end, OptGenList);
repeat_for_opts({F,Args}, OptGenList) when is_atom(F) ->
    repeat_for_opts(fun(Opts) -> ?MODULE:F(Opts,Args) end, OptGenList);
repeat_for_opts(F, OptGenList) ->
    repeat_for_opts(F, OptGenList, []).

repeat_for_opts(F, [], Acc) ->
    lists:foldl(fun(Opts, RV_Acc) ->
			OptList = lists:filter(fun(E) -> E =/= void end, Opts),
			io:format("Calling with options ~p\n",[OptList]),
			RV = F(OptList),
			case RV_Acc of
			    {comment,_} -> RV_Acc;
			    _ -> case RV of
				     {comment,_} -> RV;
				     _ -> [RV | RV_Acc]
				 end
			end
	          end, [], Acc);
repeat_for_opts(F, [OptList | Tail], []) when is_list(OptList) ->
    repeat_for_opts(F, Tail, [[Opt] || Opt <- OptList]);
repeat_for_opts(F, [OptList | Tail], AccList) when is_list(OptList) ->
    repeat_for_opts(F, Tail, [[Opt|Acc] || Opt <- OptList, Acc <- AccList]);
repeat_for_opts(F, [Atom | Tail], AccList) when is_atom(Atom) ->
    repeat_for_opts(F, [repeat_for_opts_atom2list(Atom) | Tail ], AccList).

repeat_for_opts_atom2list(all_types) -> [set,ordered_set,bag,duplicate_bag];
repeat_for_opts_atom2list(write_concurrency) -> [{write_concurrency,false},{write_concurrency,true}];
repeat_for_opts_atom2list(read_concurrency) -> [{read_concurrency,false},{read_concurrency,true}];
repeat_for_opts_atom2list(compressed) -> [compressed,void].
    
ets_new(Name, Opts) ->
    %%ets:new(Name, [compressed | Opts]).
    ets:new(Name, Opts).
