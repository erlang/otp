%% ATTENTION!
%% This is an automatically generated file. Do not edit.
%% Use './remake' script to refresh it if needed.
%% All Dialyzer options should be defined in dialyzer_options
%% file.

-module(race_tests_SUITE).

-include("ct.hrl").
-include("dialyzer_test_constants.hrl").

-export([suite/0, init_per_suite/0, init_per_suite/1,
         end_per_suite/1, all/0]).
-export([race_tests_SUITE_consistency/1, ets_insert_args1/1, 
         ets_insert_args2/1, ets_insert_args3/1, ets_insert_args4/1, 
         ets_insert_args5/1, ets_insert_args6/1, ets_insert_args7/1, 
         ets_insert_args8/1, ets_insert_control_flow1/1, 
         ets_insert_control_flow2/1, ets_insert_control_flow3/1, 
         ets_insert_control_flow4/1, ets_insert_control_flow5/1, 
         ets_insert_diff_atoms_race1/1, ets_insert_diff_atoms_race2/1, 
         ets_insert_diff_atoms_race3/1, ets_insert_diff_atoms_race4/1, 
         ets_insert_diff_atoms_race5/1, ets_insert_diff_atoms_race6/1, 
         ets_insert_double1/1, ets_insert_double2/1, ets_insert_funs1/1, 
         ets_insert_funs2/1, ets_insert_new/1, ets_insert_param/1, 
         extract_translations/1, mnesia_diff_atoms_race1/1, 
         mnesia_diff_atoms_race2/1, mnesia_dirty_read_one_write_two/1, 
         mnesia_dirty_read_two_write_one/1, 
         mnesia_dirty_read_write_double1/1, 
         mnesia_dirty_read_write_double2/1, 
         mnesia_dirty_read_write_double3/1, 
         mnesia_dirty_read_write_double4/1, mnesia_dirty_read_write_one/1, 
         mnesia_dirty_read_write_two/1, whereis_control_flow1/1, 
         whereis_control_flow2/1, whereis_control_flow3/1, 
         whereis_control_flow4/1, whereis_control_flow5/1, 
         whereis_control_flow6/1, whereis_diff_atoms_no_race/1, 
         whereis_diff_atoms_race/1, whereis_diff_functions1/1, 
         whereis_diff_functions1_nested/1, 
         whereis_diff_functions1_pathsens/1, 
         whereis_diff_functions1_twice/1, whereis_diff_functions2/1, 
         whereis_diff_functions2_nested/1, 
         whereis_diff_functions2_pathsens/1, 
         whereis_diff_functions2_twice/1, whereis_diff_functions3/1, 
         whereis_diff_functions3_nested/1, 
         whereis_diff_functions3_pathsens/1, whereis_diff_functions4/1, 
         whereis_diff_functions5/1, whereis_diff_functions6/1, 
         whereis_diff_modules1/1, whereis_diff_modules1_pathsens/1, 
         whereis_diff_modules1_rec/1, whereis_diff_modules2/1, 
         whereis_diff_modules2_pathsens/1, whereis_diff_modules2_rec/1, 
         whereis_diff_modules3/1, whereis_diff_modules_nested/1, 
         whereis_diff_modules_twice/1, whereis_diff_vars_no_race/1, 
         whereis_diff_vars_race/1, whereis_intra_inter_module1/1, 
         whereis_intra_inter_module2/1, whereis_intra_inter_module3/1, 
         whereis_intra_inter_module4/1, whereis_intra_inter_module5/1, 
         whereis_intra_inter_module6/1, whereis_intra_inter_module7/1, 
         whereis_intra_inter_module8/1, whereis_param/1, 
         whereis_param_inter_module/1, whereis_rec_function1/1, 
         whereis_rec_function2/1, whereis_rec_function3/1, 
         whereis_rec_function4/1, whereis_rec_function5/1, 
         whereis_rec_function6/1, whereis_rec_function7/1, 
         whereis_rec_function8/1, whereis_try_catch/1, whereis_vars1/1, 
         whereis_vars10/1, whereis_vars11/1, whereis_vars12/1, 
         whereis_vars13/1, whereis_vars14/1, whereis_vars15/1, 
         whereis_vars16/1, whereis_vars17/1, whereis_vars18/1, 
         whereis_vars19/1, whereis_vars2/1, whereis_vars20/1, 
         whereis_vars21/1, whereis_vars22/1, whereis_vars3/1, 
         whereis_vars4/1, whereis_vars5/1, whereis_vars6/1, 
         whereis_vars7/1, whereis_vars8/1, whereis_vars9/1]).

suite() ->
  [{timetrap, {minutes, 1}}].

init_per_suite() ->
  [{timetrap, ?plt_timeout}].
init_per_suite(Config) ->
  OutDir = ?config(priv_dir, Config),
  case dialyzer_common:check_plt(OutDir) of
    fail -> {skip, "Plt creation/check failed."};
    ok -> [{dialyzer_options, [{warnings,[race_conditions]}]}|Config]
  end.

end_per_suite(_Config) ->
  ok.

all() ->
  [race_tests_SUITE_consistency,ets_insert_args1,ets_insert_args2,
   ets_insert_args3,ets_insert_args4,ets_insert_args5,ets_insert_args6,
   ets_insert_args7,ets_insert_args8,ets_insert_control_flow1,
   ets_insert_control_flow2,ets_insert_control_flow3,ets_insert_control_flow4,
   ets_insert_control_flow5,ets_insert_diff_atoms_race1,
   ets_insert_diff_atoms_race2,ets_insert_diff_atoms_race3,
   ets_insert_diff_atoms_race4,ets_insert_diff_atoms_race5,
   ets_insert_diff_atoms_race6,ets_insert_double1,ets_insert_double2,
   ets_insert_funs1,ets_insert_funs2,ets_insert_new,ets_insert_param,
   extract_translations,mnesia_diff_atoms_race1,mnesia_diff_atoms_race2,
   mnesia_dirty_read_one_write_two,mnesia_dirty_read_two_write_one,
   mnesia_dirty_read_write_double1,mnesia_dirty_read_write_double2,
   mnesia_dirty_read_write_double3,mnesia_dirty_read_write_double4,
   mnesia_dirty_read_write_one,mnesia_dirty_read_write_two,
   whereis_control_flow1,whereis_control_flow2,whereis_control_flow3,
   whereis_control_flow4,whereis_control_flow5,whereis_control_flow6,
   whereis_diff_atoms_no_race,whereis_diff_atoms_race,whereis_diff_functions1,
   whereis_diff_functions1_nested,whereis_diff_functions1_pathsens,
   whereis_diff_functions1_twice,whereis_diff_functions2,
   whereis_diff_functions2_nested,whereis_diff_functions2_pathsens,
   whereis_diff_functions2_twice,whereis_diff_functions3,
   whereis_diff_functions3_nested,whereis_diff_functions3_pathsens,
   whereis_diff_functions4,whereis_diff_functions5,whereis_diff_functions6,
   whereis_diff_modules1,whereis_diff_modules1_pathsens,
   whereis_diff_modules1_rec,whereis_diff_modules2,
   whereis_diff_modules2_pathsens,whereis_diff_modules2_rec,
   whereis_diff_modules3,whereis_diff_modules_nested,
   whereis_diff_modules_twice,whereis_diff_vars_no_race,
   whereis_diff_vars_race,whereis_intra_inter_module1,
   whereis_intra_inter_module2,whereis_intra_inter_module3,
   whereis_intra_inter_module4,whereis_intra_inter_module5,
   whereis_intra_inter_module6,whereis_intra_inter_module7,
   whereis_intra_inter_module8,whereis_param,whereis_param_inter_module,
   whereis_rec_function1,whereis_rec_function2,whereis_rec_function3,
   whereis_rec_function4,whereis_rec_function5,whereis_rec_function6,
   whereis_rec_function7,whereis_rec_function8,whereis_try_catch,
   whereis_vars1,whereis_vars10,whereis_vars11,whereis_vars12,whereis_vars13,
   whereis_vars14,whereis_vars15,whereis_vars16,whereis_vars17,whereis_vars18,
   whereis_vars19,whereis_vars2,whereis_vars20,whereis_vars21,whereis_vars22,
   whereis_vars3,whereis_vars4,whereis_vars5,whereis_vars6,whereis_vars7,
   whereis_vars8,whereis_vars9].

dialyze(Config, TestCase) ->
  Opts = ?config(dialyzer_options, Config),
  Dir = ?config(data_dir, Config),
  OutDir = ?config(priv_dir, Config),
  dialyzer_common:check(TestCase, Opts, Dir, OutDir).

race_tests_SUITE_consistency(Config) ->
  Dir = ?config(data_dir, Config),
  case dialyzer_common:new_tests(Dir, all()) of
    []  -> ok;
    New -> ct:fail({missing_tests,New})
  end.

ets_insert_args1(Config) ->
  case dialyze(Config, ets_insert_args1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_args2(Config) ->
  case dialyze(Config, ets_insert_args2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_args3(Config) ->
  case dialyze(Config, ets_insert_args3) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_args4(Config) ->
  case dialyze(Config, ets_insert_args4) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_args5(Config) ->
  case dialyze(Config, ets_insert_args5) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_args6(Config) ->
  case dialyze(Config, ets_insert_args6) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_args7(Config) ->
  case dialyze(Config, ets_insert_args7) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_args8(Config) ->
  case dialyze(Config, ets_insert_args8) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_control_flow1(Config) ->
  case dialyze(Config, ets_insert_control_flow1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_control_flow2(Config) ->
  case dialyze(Config, ets_insert_control_flow2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_control_flow3(Config) ->
  case dialyze(Config, ets_insert_control_flow3) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_control_flow4(Config) ->
  case dialyze(Config, ets_insert_control_flow4) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_control_flow5(Config) ->
  case dialyze(Config, ets_insert_control_flow5) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_diff_atoms_race1(Config) ->
  case dialyze(Config, ets_insert_diff_atoms_race1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_diff_atoms_race2(Config) ->
  case dialyze(Config, ets_insert_diff_atoms_race2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_diff_atoms_race3(Config) ->
  case dialyze(Config, ets_insert_diff_atoms_race3) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_diff_atoms_race4(Config) ->
  case dialyze(Config, ets_insert_diff_atoms_race4) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_diff_atoms_race5(Config) ->
  case dialyze(Config, ets_insert_diff_atoms_race5) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_diff_atoms_race6(Config) ->
  case dialyze(Config, ets_insert_diff_atoms_race6) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_double1(Config) ->
  case dialyze(Config, ets_insert_double1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_double2(Config) ->
  case dialyze(Config, ets_insert_double2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_funs1(Config) ->
  case dialyze(Config, ets_insert_funs1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_funs2(Config) ->
  case dialyze(Config, ets_insert_funs2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_new(Config) ->
  case dialyze(Config, ets_insert_new) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets_insert_param(Config) ->
  case dialyze(Config, ets_insert_param) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

extract_translations(Config) ->
  case dialyze(Config, extract_translations) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

mnesia_diff_atoms_race1(Config) ->
  case dialyze(Config, mnesia_diff_atoms_race1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

mnesia_diff_atoms_race2(Config) ->
  case dialyze(Config, mnesia_diff_atoms_race2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

mnesia_dirty_read_one_write_two(Config) ->
  case dialyze(Config, mnesia_dirty_read_one_write_two) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

mnesia_dirty_read_two_write_one(Config) ->
  case dialyze(Config, mnesia_dirty_read_two_write_one) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

mnesia_dirty_read_write_double1(Config) ->
  case dialyze(Config, mnesia_dirty_read_write_double1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

mnesia_dirty_read_write_double2(Config) ->
  case dialyze(Config, mnesia_dirty_read_write_double2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

mnesia_dirty_read_write_double3(Config) ->
  case dialyze(Config, mnesia_dirty_read_write_double3) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

mnesia_dirty_read_write_double4(Config) ->
  case dialyze(Config, mnesia_dirty_read_write_double4) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

mnesia_dirty_read_write_one(Config) ->
  case dialyze(Config, mnesia_dirty_read_write_one) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

mnesia_dirty_read_write_two(Config) ->
  case dialyze(Config, mnesia_dirty_read_write_two) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_control_flow1(Config) ->
  case dialyze(Config, whereis_control_flow1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_control_flow2(Config) ->
  case dialyze(Config, whereis_control_flow2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_control_flow3(Config) ->
  case dialyze(Config, whereis_control_flow3) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_control_flow4(Config) ->
  case dialyze(Config, whereis_control_flow4) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_control_flow5(Config) ->
  case dialyze(Config, whereis_control_flow5) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_control_flow6(Config) ->
  case dialyze(Config, whereis_control_flow6) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_atoms_no_race(Config) ->
  case dialyze(Config, whereis_diff_atoms_no_race) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_atoms_race(Config) ->
  case dialyze(Config, whereis_diff_atoms_race) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_functions1(Config) ->
  case dialyze(Config, whereis_diff_functions1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_functions1_nested(Config) ->
  case dialyze(Config, whereis_diff_functions1_nested) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_functions1_pathsens(Config) ->
  case dialyze(Config, whereis_diff_functions1_pathsens) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_functions1_twice(Config) ->
  case dialyze(Config, whereis_diff_functions1_twice) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_functions2(Config) ->
  case dialyze(Config, whereis_diff_functions2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_functions2_nested(Config) ->
  case dialyze(Config, whereis_diff_functions2_nested) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_functions2_pathsens(Config) ->
  case dialyze(Config, whereis_diff_functions2_pathsens) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_functions2_twice(Config) ->
  case dialyze(Config, whereis_diff_functions2_twice) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_functions3(Config) ->
  case dialyze(Config, whereis_diff_functions3) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_functions3_nested(Config) ->
  case dialyze(Config, whereis_diff_functions3_nested) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_functions3_pathsens(Config) ->
  case dialyze(Config, whereis_diff_functions3_pathsens) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_functions4(Config) ->
  case dialyze(Config, whereis_diff_functions4) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_functions5(Config) ->
  case dialyze(Config, whereis_diff_functions5) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_functions6(Config) ->
  case dialyze(Config, whereis_diff_functions6) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_modules1(Config) ->
  case dialyze(Config, whereis_diff_modules1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_modules1_pathsens(Config) ->
  case dialyze(Config, whereis_diff_modules1_pathsens) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_modules1_rec(Config) ->
  case dialyze(Config, whereis_diff_modules1_rec) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_modules2(Config) ->
  case dialyze(Config, whereis_diff_modules2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_modules2_pathsens(Config) ->
  case dialyze(Config, whereis_diff_modules2_pathsens) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_modules2_rec(Config) ->
  case dialyze(Config, whereis_diff_modules2_rec) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_modules3(Config) ->
  case dialyze(Config, whereis_diff_modules3) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_modules_nested(Config) ->
  case dialyze(Config, whereis_diff_modules_nested) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_modules_twice(Config) ->
  case dialyze(Config, whereis_diff_modules_twice) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_vars_no_race(Config) ->
  case dialyze(Config, whereis_diff_vars_no_race) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_diff_vars_race(Config) ->
  case dialyze(Config, whereis_diff_vars_race) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_intra_inter_module1(Config) ->
  case dialyze(Config, whereis_intra_inter_module1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_intra_inter_module2(Config) ->
  case dialyze(Config, whereis_intra_inter_module2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_intra_inter_module3(Config) ->
  case dialyze(Config, whereis_intra_inter_module3) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_intra_inter_module4(Config) ->
  case dialyze(Config, whereis_intra_inter_module4) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_intra_inter_module5(Config) ->
  case dialyze(Config, whereis_intra_inter_module5) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_intra_inter_module6(Config) ->
  case dialyze(Config, whereis_intra_inter_module6) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_intra_inter_module7(Config) ->
  case dialyze(Config, whereis_intra_inter_module7) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_intra_inter_module8(Config) ->
  case dialyze(Config, whereis_intra_inter_module8) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_param(Config) ->
  case dialyze(Config, whereis_param) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_param_inter_module(Config) ->
  case dialyze(Config, whereis_param_inter_module) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_rec_function1(Config) ->
  case dialyze(Config, whereis_rec_function1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_rec_function2(Config) ->
  case dialyze(Config, whereis_rec_function2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_rec_function3(Config) ->
  case dialyze(Config, whereis_rec_function3) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_rec_function4(Config) ->
  case dialyze(Config, whereis_rec_function4) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_rec_function5(Config) ->
  case dialyze(Config, whereis_rec_function5) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_rec_function6(Config) ->
  case dialyze(Config, whereis_rec_function6) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_rec_function7(Config) ->
  case dialyze(Config, whereis_rec_function7) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_rec_function8(Config) ->
  case dialyze(Config, whereis_rec_function8) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_try_catch(Config) ->
  case dialyze(Config, whereis_try_catch) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars1(Config) ->
  case dialyze(Config, whereis_vars1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars10(Config) ->
  case dialyze(Config, whereis_vars10) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars11(Config) ->
  case dialyze(Config, whereis_vars11) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars12(Config) ->
  case dialyze(Config, whereis_vars12) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars13(Config) ->
  case dialyze(Config, whereis_vars13) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars14(Config) ->
  case dialyze(Config, whereis_vars14) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars15(Config) ->
  case dialyze(Config, whereis_vars15) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars16(Config) ->
  case dialyze(Config, whereis_vars16) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars17(Config) ->
  case dialyze(Config, whereis_vars17) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars18(Config) ->
  case dialyze(Config, whereis_vars18) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars19(Config) ->
  case dialyze(Config, whereis_vars19) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars2(Config) ->
  case dialyze(Config, whereis_vars2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars20(Config) ->
  case dialyze(Config, whereis_vars20) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars21(Config) ->
  case dialyze(Config, whereis_vars21) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars22(Config) ->
  case dialyze(Config, whereis_vars22) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars3(Config) ->
  case dialyze(Config, whereis_vars3) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars4(Config) ->
  case dialyze(Config, whereis_vars4) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars5(Config) ->
  case dialyze(Config, whereis_vars5) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars6(Config) ->
  case dialyze(Config, whereis_vars6) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars7(Config) ->
  case dialyze(Config, whereis_vars7) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars8(Config) ->
  case dialyze(Config, whereis_vars8) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

whereis_vars9(Config) ->
  case dialyze(Config, whereis_vars9) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

