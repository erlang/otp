-module(small_tests_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, groups/0, init_per_group/2, end_per_group/2,
         init_per_testcase/2, fin_per_testcase/2]).

-export([app_call/1, appmon_place/1, areq/1, atom_call/1, atom_guard/1, 
         atom_widen/1, bs_fail_constr/1, bs_utf8/1, cerl_hipeify/1, 
         comm_layer/1, compare1/1, confusing_warning/1, contract2/1, 
         contract3/1, contract5/1, disj_norm_form/1, eqeq/1, 
         ets_select/1, exhaust_case/1, failing_guard1/1, flatten/1, 
         fun_app/1, fun_ref_match/1, fun_ref_record/1, gencall/1, 
         gs_make/1, inf_loop2/1, letrec1/1, list_match/1, lzip/1, 
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

-define(default_timeout, ?t:minutes(1)).
-define(dialyzer_options, ?config(dialyzer_options, Config)).
-define(datadir, ?config(data_dir, Config)).
-define(privdir, ?config(priv_dir, Config)).

groups() -> [].

init_per_group(_GroupName, Config) -> Config.

end_per_group(_GroupName, Config) -> Config.

init_per_testcase(_Case, Config) ->
    ?line Dog = ?t:timetrap(?default_timeout),
    [{dialyzer_options, []}, {watchdog, Dog} | Config].

fin_per_testcase(_Case, _Config) ->
    Dog = ?config(watchdog, _Config),
    ?t:timetrap_cancel(Dog),
    ok.

all() ->
    [app_call,appmon_place,areq,atom_call,atom_guard,atom_widen,
     bs_fail_constr,bs_utf8,cerl_hipeify,comm_layer,compare1,
     confusing_warning,contract2,contract3,contract5,disj_norm_form,eqeq,
     ets_select,exhaust_case,failing_guard1,flatten,fun_app,fun_ref_match,
     fun_ref_record,gencall,gs_make,inf_loop2,letrec1,list_match,lzip,
     make_tuple,minus_minus,mod_info,my_filter,my_sofs,no_match,no_unused_fun,
     no_unused_fun2,non_existing,not_guard_crash,or_bug,orelsebug,orelsebug2,
     overloaded1,port_info_test,process_info_test,pubsub,receive1,
     record_construct,record_pat,record_send_test,record_test,
     recursive_types1,recursive_types2,recursive_types3,recursive_types4,
     recursive_types5,recursive_types6,recursive_types7,refine_bug1,toth,trec,
     try1,tuple1,unsafe_beamcode_bug,unused_cases,unused_clauses,zero_tuple].

app_call(Config) when is_list(Config) ->
    ?line run(Config, {app_call, file}),
    ok.

appmon_place(Config) when is_list(Config) ->
    ?line run(Config, {appmon_place, file}),
    ok.

areq(Config) when is_list(Config) ->
    ?line run(Config, {areq, file}),
    ok.

atom_call(Config) when is_list(Config) ->
    ?line run(Config, {atom_call, file}),
    ok.

atom_guard(Config) when is_list(Config) ->
    ?line run(Config, {atom_guard, file}),
    ok.

atom_widen(Config) when is_list(Config) ->
    ?line run(Config, {atom_widen, file}),
    ok.

bs_fail_constr(Config) when is_list(Config) ->
    ?line run(Config, {bs_fail_constr, file}),
    ok.

bs_utf8(Config) when is_list(Config) ->
    ?line run(Config, {bs_utf8, file}),
    ok.

cerl_hipeify(Config) when is_list(Config) ->
    ?line run(Config, {cerl_hipeify, file}),
    ok.

comm_layer(Config) when is_list(Config) ->
    ?line run(Config, {comm_layer, dir}),
    ok.

compare1(Config) when is_list(Config) ->
    ?line run(Config, {compare1, file}),
    ok.

confusing_warning(Config) when is_list(Config) ->
    ?line run(Config, {confusing_warning, file}),
    ok.

contract2(Config) when is_list(Config) ->
    ?line run(Config, {contract2, file}),
    ok.

contract3(Config) when is_list(Config) ->
    ?line run(Config, {contract3, file}),
    ok.

contract5(Config) when is_list(Config) ->
    ?line run(Config, {contract5, file}),
    ok.

disj_norm_form(Config) when is_list(Config) ->
    ?line run(Config, {disj_norm_form, file}),
    ok.

eqeq(Config) when is_list(Config) ->
    ?line run(Config, {eqeq, file}),
    ok.

ets_select(Config) when is_list(Config) ->
    ?line run(Config, {ets_select, file}),
    ok.

exhaust_case(Config) when is_list(Config) ->
    ?line run(Config, {exhaust_case, file}),
    ok.

failing_guard1(Config) when is_list(Config) ->
    ?line run(Config, {failing_guard1, file}),
    ok.

flatten(Config) when is_list(Config) ->
    ?line run(Config, {flatten, file}),
    ok.

fun_app(Config) when is_list(Config) ->
    ?line run(Config, {fun_app, file}),
    ok.

fun_ref_match(Config) when is_list(Config) ->
    ?line run(Config, {fun_ref_match, file}),
    ok.

fun_ref_record(Config) when is_list(Config) ->
    ?line run(Config, {fun_ref_record, file}),
    ok.

gencall(Config) when is_list(Config) ->
    ?line run(Config, {gencall, file}),
    ok.

gs_make(Config) when is_list(Config) ->
    ?line run(Config, {gs_make, file}),
    ok.

inf_loop2(Config) when is_list(Config) ->
    ?line run(Config, {inf_loop2, file}),
    ok.

letrec1(Config) when is_list(Config) ->
    ?line run(Config, {letrec1, file}),
    ok.

list_match(Config) when is_list(Config) ->
    ?line run(Config, {list_match, file}),
    ok.

lzip(Config) when is_list(Config) ->
    ?line run(Config, {lzip, file}),
    ok.

make_tuple(Config) when is_list(Config) ->
    ?line run(Config, {make_tuple, file}),
    ok.

minus_minus(Config) when is_list(Config) ->
    ?line run(Config, {minus_minus, file}),
    ok.

mod_info(Config) when is_list(Config) ->
    ?line run(Config, {mod_info, file}),
    ok.

my_filter(Config) when is_list(Config) ->
    ?line run(Config, {my_filter, file}),
    ok.

my_sofs(Config) when is_list(Config) ->
    ?line run(Config, {my_sofs, file}),
    ok.

no_match(Config) when is_list(Config) ->
    ?line run(Config, {no_match, file}),
    ok.

no_unused_fun(Config) when is_list(Config) ->
    ?line run(Config, {no_unused_fun, file}),
    ok.

no_unused_fun2(Config) when is_list(Config) ->
    ?line run(Config, {no_unused_fun2, file}),
    ok.

non_existing(Config) when is_list(Config) ->
    ?line run(Config, {non_existing, file}),
    ok.

not_guard_crash(Config) when is_list(Config) ->
    ?line run(Config, {not_guard_crash, file}),
    ok.

or_bug(Config) when is_list(Config) ->
    ?line run(Config, {or_bug, file}),
    ok.

orelsebug(Config) when is_list(Config) ->
    ?line run(Config, {orelsebug, file}),
    ok.

orelsebug2(Config) when is_list(Config) ->
    ?line run(Config, {orelsebug2, file}),
    ok.

overloaded1(Config) when is_list(Config) ->
    ?line run(Config, {overloaded1, file}),
    ok.

port_info_test(Config) when is_list(Config) ->
    ?line run(Config, {port_info_test, file}),
    ok.

process_info_test(Config) when is_list(Config) ->
    ?line run(Config, {process_info_test, file}),
    ok.

pubsub(Config) when is_list(Config) ->
    ?line run(Config, {pubsub, dir}),
    ok.

receive1(Config) when is_list(Config) ->
    ?line run(Config, {receive1, file}),
    ok.

record_construct(Config) when is_list(Config) ->
    ?line run(Config, {record_construct, file}),
    ok.

record_pat(Config) when is_list(Config) ->
    ?line run(Config, {record_pat, file}),
    ok.

record_send_test(Config) when is_list(Config) ->
    ?line run(Config, {record_send_test, file}),
    ok.

record_test(Config) when is_list(Config) ->
    ?line run(Config, {record_test, file}),
    ok.

recursive_types1(Config) when is_list(Config) ->
    ?line run(Config, {recursive_types1, file}),
    ok.

recursive_types2(Config) when is_list(Config) ->
    ?line run(Config, {recursive_types2, file}),
    ok.

recursive_types3(Config) when is_list(Config) ->
    ?line run(Config, {recursive_types3, file}),
    ok.

recursive_types4(Config) when is_list(Config) ->
    ?line run(Config, {recursive_types4, file}),
    ok.

recursive_types5(Config) when is_list(Config) ->
    ?line run(Config, {recursive_types5, file}),
    ok.

recursive_types6(Config) when is_list(Config) ->
    ?line run(Config, {recursive_types6, file}),
    ok.

recursive_types7(Config) when is_list(Config) ->
    ?line run(Config, {recursive_types7, file}),
    ok.

refine_bug1(Config) when is_list(Config) ->
    ?line run(Config, {refine_bug1, file}),
    ok.

toth(Config) when is_list(Config) ->
    ?line run(Config, {toth, file}),
    ok.

trec(Config) when is_list(Config) ->
    ?line run(Config, {trec, file}),
    ok.

try1(Config) when is_list(Config) ->
    ?line run(Config, {try1, file}),
    ok.

tuple1(Config) when is_list(Config) ->
    ?line run(Config, {tuple1, file}),
    ok.

unsafe_beamcode_bug(Config) when is_list(Config) ->
    ?line run(Config, {unsafe_beamcode_bug, file}),
    ok.

unused_cases(Config) when is_list(Config) ->
    ?line run(Config, {unused_cases, file}),
    ok.

unused_clauses(Config) when is_list(Config) ->
    ?line run(Config, {unused_clauses, file}),
    ok.

zero_tuple(Config) when is_list(Config) ->
    ?line run(Config, {zero_tuple, file}),
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
