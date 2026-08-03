<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2019-2025. All Rights Reserved.
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
-->

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
