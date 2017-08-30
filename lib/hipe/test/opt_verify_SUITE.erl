-module(opt_verify_SUITE).

-compile([export_all]).

all() ->
    [call_elim].

groups() ->
    [].

init_per_suite(Config) ->
    case erlang:system_info(hipe_architecture) of
        undefined -> {skip, "HiPE not available or enabled"};
        _ -> Config
    end.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

call_elim_test_file(Config, FileName, Option) ->
    PrivDir = test_server:lookup_config(priv_dir, Config),
    TempOut = test_server:temp_name(filename:join(PrivDir, "call_elim_out")),
    {ok, TestCase} = compile:file(FileName),
    {ok, TestCase} = hipe:c(TestCase, [Option, {pp_range_icode, {file, TempOut}}]),
    {ok, Icode} = file:read_file(TempOut),
    ok = file:delete(TempOut),
    Icode.

substring_count(Icode, Substring) ->
    substring_count(Icode, Substring, 0).
substring_count(Icode, Substring, N) ->
    case string:str(Icode, Substring) of
        0 -> N;
        I -> substring_count(lists:nthtail(I, Icode), Substring, N+1)
    end.

call_elim() ->
    [{doc, "Test that the call elimination optimization pass is ok"}].
call_elim(Config) ->
    DataDir = test_server:lookup_config(data_dir, Config),
    F1 = filename:join(DataDir, "call_elim_test.erl"),
    Icode1 = call_elim_test_file(Config, F1, icode_call_elim),
    0 = substring_count(binary:bin_to_list(Icode1), "is_key"),
    Icode2 = call_elim_test_file(Config, F1, no_icode_call_elim),
    true = (0 /= substring_count(binary:bin_to_list(Icode2), "is_key")),
    F2 = filename:join(DataDir, "call_elim_test_branches_no_opt_poss.erl"),
    Icode3 = call_elim_test_file(Config, F2, icode_call_elim),
    3 = substring_count(binary:bin_to_list(Icode3), "is_key"),
    Icode4 = call_elim_test_file(Config, F2, no_icode_call_elim),
    3 = substring_count(binary:bin_to_list(Icode4), "is_key"),
    F3 = filename:join(DataDir, "call_elim_test_branches_opt_poss.erl"),
    Icode5 = call_elim_test_file(Config, F3, icode_call_elim),
    0 = substring_count(binary:bin_to_list(Icode5), "is_key"),
    Icode6 = call_elim_test_file(Config, F3, no_icode_call_elim),
    3 = substring_count(binary:bin_to_list(Icode6), "is_key"),
    ok.
