Counting Instructions
=====================

Here is an example that shows how to count how many times each
instruction is executed:

    $ (cd erts/emulator && make icount)
     MAKE	icount
    make[1]: Entering directory `/home/uabbgus/otp/erts/emulator'
      .
      .
      .
    make[1]: Leaving directory `/home/uabbgus/otp/erts/emulator'
    $ cat t.erl
    -module(t).
    -compile([export_all,nowarn_export_all]).

    count() ->
        erts_debug:ic(fun benchmark/0).

    benchmark() ->
        %% Run dialyzer.
        Root = code:root_dir(),
        Wc1 = filename:join(Root, "lib/{kernel,stdlib}/ebin/*.beam"),
        Wc2 = filename:join(Root, "erts/preloaded/ebin/*.beam"),
        Files = filelib:wildcard(Wc1) ++ filelib:wildcard(Wc2),
        Opts = [{analysis_type,plt_build},{files,Files},{get_warnings,true}],
        dialyzer:run(Opts).
    $ $ERL_TOP/bin/cerl -icount
    Erlang/OTP 22 [RELEASE CANDIDATE 1] [erts-10.2.4] [source-ac0d451] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe] [instruction-counting]

    Eshell V10.2.4  (abort with ^G)
    1> c(t).
    {ok,t}
    2> t:count().
               0 badarg_j
               0 badmatch_x
               0 bs_add_jsstx
               0 bs_context_to_binary_x
                .
                .
                .
       536461394 move_call_last_yfQ
       552405176 allocate_tt
       619920327 i_is_eq_exact_immed_frc
       636419163 is_nonempty_list_allocate_frtt
       641859278 i_get_tuple_element_xPx
       678196718 move_return_c
       786289914 is_tagged_tuple_frAa
       865826424 i_call_f
    Total: 20728870321
    []
    3>
