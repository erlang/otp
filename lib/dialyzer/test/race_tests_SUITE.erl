-module(race_tests_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, groups/0, init_per_group/2, end_per_group/2,
         init_per_testcase/2, fin_per_testcase/2]).

-export([ets_insert_args1/1, ets_insert_args2/1, ets_insert_args3/1, 
         ets_insert_args4/1, ets_insert_args5/1, ets_insert_args6/1, 
         ets_insert_args7/1, ets_insert_args8/1, 
         ets_insert_control_flow1/1, ets_insert_control_flow2/1, 
         ets_insert_control_flow3/1, ets_insert_control_flow4/1, 
         ets_insert_control_flow5/1, ets_insert_diff_atoms_race1/1, 
         ets_insert_diff_atoms_race2/1, ets_insert_diff_atoms_race3/1, 
         ets_insert_diff_atoms_race4/1, ets_insert_diff_atoms_race5/1, 
         ets_insert_diff_atoms_race6/1, ets_insert_double1/1, 
         ets_insert_double2/1, ets_insert_funs1/1, ets_insert_funs2/1, 
         ets_insert_new/1, ets_insert_param/1, extract_translations/1, 
         mnesia_diff_atoms_race1/1, mnesia_diff_atoms_race2/1, 
         mnesia_dirty_read_one_write_two/1, 
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

-define(default_timeout, ?t:minutes(1)).
-define(dialyzer_options, ?config(dialyzer_options, Config)).
-define(datadir, ?config(data_dir, Config)).
-define(privdir, ?config(priv_dir, Config)).

groups() -> [].

init_per_group(_GroupName, Config) -> Config.

end_per_group(_GroupName, Config) -> Config.

init_per_testcase(_Case, Config) ->
    ?line Dog = ?t:timetrap(?default_timeout),
    [{dialyzer_options, [{warnings,[race_conditions]}]}, {watchdog, Dog} | Config].

fin_per_testcase(_Case, _Config) ->
    Dog = ?config(watchdog, _Config),
    ?t:timetrap_cancel(Dog),
    ok.

all() ->
    [ets_insert_args1,ets_insert_args2,ets_insert_args3,ets_insert_args4,
     ets_insert_args5,ets_insert_args6,ets_insert_args7,ets_insert_args8,
     ets_insert_control_flow1,ets_insert_control_flow2,
     ets_insert_control_flow3,ets_insert_control_flow4,
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
     whereis_diff_atoms_no_race,whereis_diff_atoms_race,
     whereis_diff_functions1,whereis_diff_functions1_nested,
     whereis_diff_functions1_pathsens,whereis_diff_functions1_twice,
     whereis_diff_functions2,whereis_diff_functions2_nested,
     whereis_diff_functions2_pathsens,whereis_diff_functions2_twice,
     whereis_diff_functions3,whereis_diff_functions3_nested,
     whereis_diff_functions3_pathsens,whereis_diff_functions4,
     whereis_diff_functions5,whereis_diff_functions6,whereis_diff_modules1,
     whereis_diff_modules1_pathsens,whereis_diff_modules1_rec,
     whereis_diff_modules2,whereis_diff_modules2_pathsens,
     whereis_diff_modules2_rec,whereis_diff_modules3,
     whereis_diff_modules_nested,whereis_diff_modules_twice,
     whereis_diff_vars_no_race,whereis_diff_vars_race,
     whereis_intra_inter_module1,whereis_intra_inter_module2,
     whereis_intra_inter_module3,whereis_intra_inter_module4,
     whereis_intra_inter_module5,whereis_intra_inter_module6,
     whereis_intra_inter_module7,whereis_intra_inter_module8,whereis_param,
     whereis_param_inter_module,whereis_rec_function1,whereis_rec_function2,
     whereis_rec_function3,whereis_rec_function4,whereis_rec_function5,
     whereis_rec_function6,whereis_rec_function7,whereis_rec_function8,
     whereis_try_catch,whereis_vars1,whereis_vars10,whereis_vars11,
     whereis_vars12,whereis_vars13,whereis_vars14,whereis_vars15,
     whereis_vars16,whereis_vars17,whereis_vars18,whereis_vars19,
     whereis_vars2,whereis_vars20,whereis_vars21,whereis_vars22,whereis_vars3,
     whereis_vars4,whereis_vars5,whereis_vars6,whereis_vars7,whereis_vars8,
     whereis_vars9].

ets_insert_args1(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_args1, file}),
    ok.

ets_insert_args2(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_args2, file}),
    ok.

ets_insert_args3(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_args3, file}),
    ok.

ets_insert_args4(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_args4, file}),
    ok.

ets_insert_args5(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_args5, file}),
    ok.

ets_insert_args6(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_args6, file}),
    ok.

ets_insert_args7(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_args7, file}),
    ok.

ets_insert_args8(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_args8, file}),
    ok.

ets_insert_control_flow1(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_control_flow1, file}),
    ok.

ets_insert_control_flow2(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_control_flow2, file}),
    ok.

ets_insert_control_flow3(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_control_flow3, file}),
    ok.

ets_insert_control_flow4(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_control_flow4, file}),
    ok.

ets_insert_control_flow5(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_control_flow5, file}),
    ok.

ets_insert_diff_atoms_race1(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_diff_atoms_race1, file}),
    ok.

ets_insert_diff_atoms_race2(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_diff_atoms_race2, file}),
    ok.

ets_insert_diff_atoms_race3(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_diff_atoms_race3, file}),
    ok.

ets_insert_diff_atoms_race4(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_diff_atoms_race4, file}),
    ok.

ets_insert_diff_atoms_race5(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_diff_atoms_race5, file}),
    ok.

ets_insert_diff_atoms_race6(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_diff_atoms_race6, file}),
    ok.

ets_insert_double1(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_double1, file}),
    ok.

ets_insert_double2(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_double2, file}),
    ok.

ets_insert_funs1(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_funs1, file}),
    ok.

ets_insert_funs2(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_funs2, file}),
    ok.

ets_insert_new(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_new, file}),
    ok.

ets_insert_param(Config) when is_list(Config) ->
    ?line run(Config, {ets_insert_param, file}),
    ok.

extract_translations(Config) when is_list(Config) ->
    ?line run(Config, {extract_translations, file}),
    ok.

mnesia_diff_atoms_race1(Config) when is_list(Config) ->
    ?line run(Config, {mnesia_diff_atoms_race1, file}),
    ok.

mnesia_diff_atoms_race2(Config) when is_list(Config) ->
    ?line run(Config, {mnesia_diff_atoms_race2, file}),
    ok.

mnesia_dirty_read_one_write_two(Config) when is_list(Config) ->
    ?line run(Config, {mnesia_dirty_read_one_write_two, file}),
    ok.

mnesia_dirty_read_two_write_one(Config) when is_list(Config) ->
    ?line run(Config, {mnesia_dirty_read_two_write_one, file}),
    ok.

mnesia_dirty_read_write_double1(Config) when is_list(Config) ->
    ?line run(Config, {mnesia_dirty_read_write_double1, file}),
    ok.

mnesia_dirty_read_write_double2(Config) when is_list(Config) ->
    ?line run(Config, {mnesia_dirty_read_write_double2, file}),
    ok.

mnesia_dirty_read_write_double3(Config) when is_list(Config) ->
    ?line run(Config, {mnesia_dirty_read_write_double3, file}),
    ok.

mnesia_dirty_read_write_double4(Config) when is_list(Config) ->
    ?line run(Config, {mnesia_dirty_read_write_double4, file}),
    ok.

mnesia_dirty_read_write_one(Config) when is_list(Config) ->
    ?line run(Config, {mnesia_dirty_read_write_one, file}),
    ok.

mnesia_dirty_read_write_two(Config) when is_list(Config) ->
    ?line run(Config, {mnesia_dirty_read_write_two, file}),
    ok.

whereis_control_flow1(Config) when is_list(Config) ->
    ?line run(Config, {whereis_control_flow1, file}),
    ok.

whereis_control_flow2(Config) when is_list(Config) ->
    ?line run(Config, {whereis_control_flow2, file}),
    ok.

whereis_control_flow3(Config) when is_list(Config) ->
    ?line run(Config, {whereis_control_flow3, file}),
    ok.

whereis_control_flow4(Config) when is_list(Config) ->
    ?line run(Config, {whereis_control_flow4, file}),
    ok.

whereis_control_flow5(Config) when is_list(Config) ->
    ?line run(Config, {whereis_control_flow5, file}),
    ok.

whereis_control_flow6(Config) when is_list(Config) ->
    ?line run(Config, {whereis_control_flow6, file}),
    ok.

whereis_diff_atoms_no_race(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_atoms_no_race, file}),
    ok.

whereis_diff_atoms_race(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_atoms_race, file}),
    ok.

whereis_diff_functions1(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_functions1, file}),
    ok.

whereis_diff_functions1_nested(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_functions1_nested, file}),
    ok.

whereis_diff_functions1_pathsens(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_functions1_pathsens, file}),
    ok.

whereis_diff_functions1_twice(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_functions1_twice, file}),
    ok.

whereis_diff_functions2(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_functions2, file}),
    ok.

whereis_diff_functions2_nested(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_functions2_nested, file}),
    ok.

whereis_diff_functions2_pathsens(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_functions2_pathsens, file}),
    ok.

whereis_diff_functions2_twice(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_functions2_twice, file}),
    ok.

whereis_diff_functions3(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_functions3, file}),
    ok.

whereis_diff_functions3_nested(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_functions3_nested, file}),
    ok.

whereis_diff_functions3_pathsens(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_functions3_pathsens, file}),
    ok.

whereis_diff_functions4(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_functions4, file}),
    ok.

whereis_diff_functions5(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_functions5, file}),
    ok.

whereis_diff_functions6(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_functions6, file}),
    ok.

whereis_diff_modules1(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_modules1, dir}),
    ok.

whereis_diff_modules1_pathsens(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_modules1_pathsens, dir}),
    ok.

whereis_diff_modules1_rec(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_modules1_rec, dir}),
    ok.

whereis_diff_modules2(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_modules2, dir}),
    ok.

whereis_diff_modules2_pathsens(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_modules2_pathsens, dir}),
    ok.

whereis_diff_modules2_rec(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_modules2_rec, dir}),
    ok.

whereis_diff_modules3(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_modules3, dir}),
    ok.

whereis_diff_modules_nested(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_modules_nested, dir}),
    ok.

whereis_diff_modules_twice(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_modules_twice, dir}),
    ok.

whereis_diff_vars_no_race(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_vars_no_race, file}),
    ok.

whereis_diff_vars_race(Config) when is_list(Config) ->
    ?line run(Config, {whereis_diff_vars_race, file}),
    ok.

whereis_intra_inter_module1(Config) when is_list(Config) ->
    ?line run(Config, {whereis_intra_inter_module1, dir}),
    ok.

whereis_intra_inter_module2(Config) when is_list(Config) ->
    ?line run(Config, {whereis_intra_inter_module2, dir}),
    ok.

whereis_intra_inter_module3(Config) when is_list(Config) ->
    ?line run(Config, {whereis_intra_inter_module3, dir}),
    ok.

whereis_intra_inter_module4(Config) when is_list(Config) ->
    ?line run(Config, {whereis_intra_inter_module4, dir}),
    ok.

whereis_intra_inter_module5(Config) when is_list(Config) ->
    ?line run(Config, {whereis_intra_inter_module5, dir}),
    ok.

whereis_intra_inter_module6(Config) when is_list(Config) ->
    ?line run(Config, {whereis_intra_inter_module6, dir}),
    ok.

whereis_intra_inter_module7(Config) when is_list(Config) ->
    ?line run(Config, {whereis_intra_inter_module7, dir}),
    ok.

whereis_intra_inter_module8(Config) when is_list(Config) ->
    ?line run(Config, {whereis_intra_inter_module8, dir}),
    ok.

whereis_param(Config) when is_list(Config) ->
    ?line run(Config, {whereis_param, file}),
    ok.

whereis_param_inter_module(Config) when is_list(Config) ->
    ?line run(Config, {whereis_param_inter_module, dir}),
    ok.

whereis_rec_function1(Config) when is_list(Config) ->
    ?line run(Config, {whereis_rec_function1, file}),
    ok.

whereis_rec_function2(Config) when is_list(Config) ->
    ?line run(Config, {whereis_rec_function2, file}),
    ok.

whereis_rec_function3(Config) when is_list(Config) ->
    ?line run(Config, {whereis_rec_function3, file}),
    ok.

whereis_rec_function4(Config) when is_list(Config) ->
    ?line run(Config, {whereis_rec_function4, file}),
    ok.

whereis_rec_function5(Config) when is_list(Config) ->
    ?line run(Config, {whereis_rec_function5, file}),
    ok.

whereis_rec_function6(Config) when is_list(Config) ->
    ?line run(Config, {whereis_rec_function6, file}),
    ok.

whereis_rec_function7(Config) when is_list(Config) ->
    ?line run(Config, {whereis_rec_function7, file}),
    ok.

whereis_rec_function8(Config) when is_list(Config) ->
    ?line run(Config, {whereis_rec_function8, file}),
    ok.

whereis_try_catch(Config) when is_list(Config) ->
    ?line run(Config, {whereis_try_catch, file}),
    ok.

whereis_vars1(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars1, file}),
    ok.

whereis_vars10(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars10, file}),
    ok.

whereis_vars11(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars11, file}),
    ok.

whereis_vars12(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars12, file}),
    ok.

whereis_vars13(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars13, file}),
    ok.

whereis_vars14(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars14, file}),
    ok.

whereis_vars15(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars15, file}),
    ok.

whereis_vars16(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars16, file}),
    ok.

whereis_vars17(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars17, file}),
    ok.

whereis_vars18(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars18, file}),
    ok.

whereis_vars19(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars19, file}),
    ok.

whereis_vars2(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars2, file}),
    ok.

whereis_vars20(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars20, file}),
    ok.

whereis_vars21(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars21, file}),
    ok.

whereis_vars22(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars22, file}),
    ok.

whereis_vars3(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars3, file}),
    ok.

whereis_vars4(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars4, file}),
    ok.

whereis_vars5(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars5, file}),
    ok.

whereis_vars6(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars6, file}),
    ok.

whereis_vars7(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars7, file}),
    ok.

whereis_vars8(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars8, file}),
    ok.

whereis_vars9(Config) when is_list(Config) ->
    ?line run(Config, {whereis_vars9, file}),
    ok.

run(Config, TestCase) ->
    case run_test(Config, TestCase) of
        ok -> ok;
        {fail, Reason} ->
            ?t:format("~s",[Reason]),
            fail()
    end.

run_test(Config, {TestCase, Kind}) ->
    Dog = ?config(watchdog, Config),
    Options = ?dialyzer_options,
    Dir = ?datadir,
    OutDir = ?privdir,
    case dialyzer_test:dialyzer_test(Options, TestCase, Kind,
                                     Dir, OutDir, Dog) of
        same -> ok;
        {differ, DiffList} ->
            {fail,
               io_lib:format("\nTest ~p failed:\n~p\n",
                            [TestCase, DiffList])}
    end.

fail() ->
    io:format("failed\n"),
    ?t:fail().
