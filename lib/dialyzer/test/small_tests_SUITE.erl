%% ATTENTION!
%% This is an automatically generated file. Do not edit.
%% Use './remake' script to refresh it if needed.
%% All Dialyzer options should be defined in dialyzer_options
%% file.

-module(small_tests_SUITE).

-include("ct.hrl").
-include("dialyzer_test_constants.hrl").

-export([suite/0, init_per_suite/0, init_per_suite/1,
         end_per_suite/1, all/0]).
-export([small_tests_SUITE_consistency/1, app_call/1, appmon_place/1, 
         areq/1, atom_call/1, atom_guard/1, atom_widen/1, 
         bs_fail_constr/1, bs_utf8/1, cerl_hipeify/1, comm_layer/1, 
         compare1/1, confusing_warning/1, contract2/1, contract3/1, 
         contract5/1, disj_norm_form/1, eqeq/1, ets_select/1, 
         exhaust_case/1, failing_guard1/1, flatten/1, fun_app/1, 
         fun_ref_match/1, fun_ref_record/1, gencall/1, gs_make/1, 
         inf_loop2/1, invalid_specs/1, letrec1/1, list_match/1, lzip/1,
         make_tuple/1, minus_minus/1, mod_info/1, my_filter/1,
         my_sofs/1, no_match/1, no_unused_fun/1, no_unused_fun2/1,
         non_existing/1, not_guard_crash/1, or_bug/1, orelsebug/1,
         orelsebug2/1, overloaded1/1, port_info_test/1,
         process_info_test/1, pubsub/1, receive1/1, record_construct/1,
         record_pat/1, record_send_test/1, record_test/1,
         recursive_types1/1, recursive_types2/1, recursive_types3/1,
         recursive_types4/1, recursive_types5/1, recursive_types6/1,
         recursive_types7/1, refine_bug1/1, toth/1, trec/1, try1/1,
         tuple1/1, unsafe_beamcode_bug/1, unused_cases/1,
         unused_clauses/1, zero_tuple/1]).

suite() ->
  [{timetrap, {minutes, 1}}].

init_per_suite() ->
  [{timetrap, ?plt_timeout}].
init_per_suite(Config) ->
  OutDir = ?config(priv_dir, Config),
  case dialyzer_common:check_plt(OutDir) of
    fail -> {skip, "Plt creation/check failed."};
    ok -> [{dialyzer_options, []}|Config]
  end.

end_per_suite(_Config) ->
  ok.

all() ->
  [small_tests_SUITE_consistency,app_call,appmon_place,areq,atom_call,
   atom_guard,atom_widen,bs_fail_constr,bs_utf8,cerl_hipeify,comm_layer,
   compare1,confusing_warning,contract2,contract3,contract5,disj_norm_form,
   eqeq,ets_select,exhaust_case,failing_guard1,flatten,fun_app,fun_ref_match,
   fun_ref_record,gencall,gs_make,inf_loop2,invalid_specs,letrec1,list_match,
   lzip,make_tuple,minus_minus,mod_info,my_filter,my_sofs,no_match,
   no_unused_fun,no_unused_fun2,non_existing,not_guard_crash,or_bug,orelsebug,
   orelsebug2,overloaded1,port_info_test,process_info_test,pubsub,receive1,
   record_construct,record_pat,record_send_test,record_test,recursive_types1,
   recursive_types2,recursive_types3,recursive_types4,recursive_types5,
   recursive_types6,recursive_types7,refine_bug1,toth,trec,try1,tuple1,
   unsafe_beamcode_bug,unused_cases,unused_clauses,zero_tuple].

dialyze(Config, TestCase) ->
  Opts = ?config(dialyzer_options, Config),
  Dir = ?config(data_dir, Config),
  OutDir = ?config(priv_dir, Config),
  dialyzer_common:check(TestCase, Opts, Dir, OutDir).

small_tests_SUITE_consistency(Config) ->
  Dir = ?config(data_dir, Config),
  case dialyzer_common:new_tests(Dir, all()) of
    []  -> ok;
    New -> ct:fail({missing_tests,New})
  end.

app_call(Config) ->
  case dialyze(Config, app_call) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

appmon_place(Config) ->
  case dialyze(Config, appmon_place) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

areq(Config) ->
  case dialyze(Config, areq) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

atom_call(Config) ->
  case dialyze(Config, atom_call) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

atom_guard(Config) ->
  case dialyze(Config, atom_guard) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

atom_widen(Config) ->
  case dialyze(Config, atom_widen) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

bs_fail_constr(Config) ->
  case dialyze(Config, bs_fail_constr) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

bs_utf8(Config) ->
  case dialyze(Config, bs_utf8) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

cerl_hipeify(Config) ->
  case dialyze(Config, cerl_hipeify) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

comm_layer(Config) ->
  case dialyze(Config, comm_layer) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

compare1(Config) ->
  case dialyze(Config, compare1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

confusing_warning(Config) ->
  case dialyze(Config, confusing_warning) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

contract2(Config) ->
  case dialyze(Config, contract2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

contract3(Config) ->
  case dialyze(Config, contract3) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

contract5(Config) ->
  case dialyze(Config, contract5) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

disj_norm_form(Config) ->
  case dialyze(Config, disj_norm_form) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

eqeq(Config) ->
  case dialyze(Config, eqeq) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_select(Config) ->
  case dialyze(Config, ets_select) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

exhaust_case(Config) ->
  case dialyze(Config, exhaust_case) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

failing_guard1(Config) ->
  case dialyze(Config, failing_guard1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

flatten(Config) ->
  case dialyze(Config, flatten) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

fun_app(Config) ->
  case dialyze(Config, fun_app) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

fun_ref_match(Config) ->
  case dialyze(Config, fun_ref_match) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

fun_ref_record(Config) ->
  case dialyze(Config, fun_ref_record) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

gencall(Config) ->
  case dialyze(Config, gencall) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

gs_make(Config) ->
  case dialyze(Config, gs_make) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

inf_loop2(Config) ->
  case dialyze(Config, inf_loop2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

invalid_specs(Config) ->
  case dialyze(Config, invalid_specs) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

letrec1(Config) ->
  case dialyze(Config, letrec1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

list_match(Config) ->
  case dialyze(Config, list_match) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

lzip(Config) ->
  case dialyze(Config, lzip) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

make_tuple(Config) ->
  case dialyze(Config, make_tuple) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

minus_minus(Config) ->
  case dialyze(Config, minus_minus) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

mod_info(Config) ->
  case dialyze(Config, mod_info) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

my_filter(Config) ->
  case dialyze(Config, my_filter) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

my_sofs(Config) ->
  case dialyze(Config, my_sofs) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

no_match(Config) ->
  case dialyze(Config, no_match) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

no_unused_fun(Config) ->
  case dialyze(Config, no_unused_fun) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

no_unused_fun2(Config) ->
  case dialyze(Config, no_unused_fun2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

non_existing(Config) ->
  case dialyze(Config, non_existing) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

not_guard_crash(Config) ->
  case dialyze(Config, not_guard_crash) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

or_bug(Config) ->
  case dialyze(Config, or_bug) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

orelsebug(Config) ->
  case dialyze(Config, orelsebug) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

orelsebug2(Config) ->
  case dialyze(Config, orelsebug2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

overloaded1(Config) ->
  case dialyze(Config, overloaded1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

port_info_test(Config) ->
  case dialyze(Config, port_info_test) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

process_info_test(Config) ->
  case dialyze(Config, process_info_test) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

pubsub(Config) ->
  case dialyze(Config, pubsub) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

receive1(Config) ->
  case dialyze(Config, receive1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

record_construct(Config) ->
  case dialyze(Config, record_construct) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

record_pat(Config) ->
  case dialyze(Config, record_pat) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

record_send_test(Config) ->
  case dialyze(Config, record_send_test) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

record_test(Config) ->
  case dialyze(Config, record_test) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

recursive_types1(Config) ->
  case dialyze(Config, recursive_types1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

recursive_types2(Config) ->
  case dialyze(Config, recursive_types2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

recursive_types3(Config) ->
  case dialyze(Config, recursive_types3) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

recursive_types4(Config) ->
  case dialyze(Config, recursive_types4) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

recursive_types5(Config) ->
  case dialyze(Config, recursive_types5) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

recursive_types6(Config) ->
  case dialyze(Config, recursive_types6) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

recursive_types7(Config) ->
  case dialyze(Config, recursive_types7) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

refine_bug1(Config) ->
  case dialyze(Config, refine_bug1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

toth(Config) ->
  case dialyze(Config, toth) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

trec(Config) ->
  case dialyze(Config, trec) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

try1(Config) ->
  case dialyze(Config, try1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

tuple1(Config) ->
  case dialyze(Config, tuple1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

unsafe_beamcode_bug(Config) ->
  case dialyze(Config, unsafe_beamcode_bug) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

unused_cases(Config) ->
  case dialyze(Config, unused_cases) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

unused_clauses(Config) ->
  case dialyze(Config, unused_clauses) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

zero_tuple(Config) ->
  case dialyze(Config, zero_tuple) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

