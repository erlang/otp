%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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
%%
%% Measures which crypto NIF calls block a normal scheduler.

-module(crypto_sched_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([hash/1,
         xof/1,
         not_implemented/1]).

-export([hash_specs/1, xof_specs/1]).

-record(ctx, {medium, %% Binary with size ?MEDIUM
              large, %% Binary with size ?LARGE
              max}). %% Binary with size ?MAX

-record(spec, {name, %% Name of the test
               setup, %% {M, F, A}, fun() -> {M, F, A} or list with any of previous or fun(S) -> {M, F, A}
               test}). %% Actual test {M, F, A} or fun(S) -> {M, F, A}

-define(MAX, 20000). %% Same as MAX_BYTES_TO_NIF
-define(MEDIUM, (1 * 1024 * 1024)). %% 1 MiB
-define(LARGE, (16 * 1024 * 1024)). %% 16 MiB
-define(CALLS, 3). %% Number of calls per testcase
-define(LONG_SCHEDULE, 20). %% 1ms is the limit of "well behaving nif", set higher to limit "borderline" failures

suite() ->
    [{timetrap, {minutes, 30}}].

all() ->
    [hash,
     xof,
     not_implemented].

init_per_suite(Config) ->
    ok = application:start(crypto),
    case crypto:info_fips() of
        enabled -> {skip, "Does not run in fips mode."};
        _ ->
            [{_, _, InfoLib}] = crypto:info_lib(),
            ct:pal("OTP: ~s, crypto: ~s, ~s~n"
                   "schedulers: ~w, dirty cpu: ~w, dirty io: ~w~n",
                   [erlang:system_info(otp_release), crypto:version(), InfoLib,
                    erlang:system_info(schedulers_online),
                    erlang:system_info(dirty_cpu_schedulers_online),
                    erlang:system_info(dirty_io_schedulers)]),
            ct:comment("~s", [InfoLib]),
            Ctx = #ctx{medium = fill(?MEDIUM),
                       large = fill(?LARGE),
                       max = fill(?MAX)},
            [{ctx, Ctx} | Config]
    end.

end_per_suite(_Config) ->
    application:stop(crypto).

init_per_testcase(TC, Config) when TC /= not_implemented ->
    Tracer = tracer(),
    Session = trace:session_create(?MODULE, Tracer, []),
    ok = trace:system(Session, long_schedule, ?LONG_SCHEDULE),
    [{tracer, Tracer}, {session, Session} | Config];
init_per_testcase(not_implemented, Config) ->
    Config.

end_per_testcase(TC, Config) when TC /= not_implemented ->
    Session = proplists:get_value(session, Config),
    trace:session_destroy(Session),
    ok;
end_per_testcase(not_implemented, _Config) ->
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

hash(Config) ->
    run(fun hash_specs/1, Config).
xof(Config) ->
    run(fun xof_specs/1, Config).

not_implemented(Config) ->
    %% Check that everything exported, but not tested causes a test failure
    Ignored = [
               %% Built-in stuff
               {version, 0},
               {module_info, 0},
               {module_info, 1},
               %% Misc - not supposed to be tested
               {start, 0},
               {stop, 0},
               {info, 0},
               {info_fips, 0},
               {enable_fips_mode, 1},
               {info_lib, 0},
               {format_error, 2},
               {supports, 0},
               {supports, 1},
               {hash_info, 1},
               {cipher_info, 1},
               {crypto_get_data, 1},
               {rand_seed, 0},
               {rand_seed, 1},
               {rand_seed_s, 0},
               {rand_seed_alg, 1},
               {rand_seed_alg, 2},
               {rand_seed_alg_s, 1},
               {rand_seed_alg_s, 2},
               {rand_uniform, 2},
               {rand_plugin_uniform, 1},
               {rand_plugin_uniform, 2},
               {rand_plugin_next, 1},
               {rand_plugin_aes_next, 1},
               {rand_plugin_aes_jump, 1},
               {rand_cache_plugin_next, 1},
               {rand_plugin_aes_jump_2pow20, 1},
               {ec_curves, 0},
               {ec_curve, 1},
               {privkey_to_pubkey, 2},
               {bytes_to_integer, 1},
               {packed_openssl_version, 4},
               %% Engine - not supposed to be tested
               {engine_get_all_methods, 0},
               {engine_load, 3},
               {engine_load, 4},
               {engine_unload, 1},
               {engine_unload, 2},
               {engine_by_id, 1},
               {engine_add, 1},
               {engine_remove, 1},
               {engine_register, 2},
               {engine_unregister, 2},
               {engine_get_id, 1},
               {engine_get_name, 1},
               {engine_list, 0},
               {engine_ctrl_cmd_string, 3},
               {engine_ctrl_cmd_string, 4},
               {ensure_engine_loaded, 2},
               {ensure_engine_loaded, 3},
               {ensure_engine_unloaded, 1},
               {ensure_engine_unloaded, 2},
               {engine_methods_convert_to_bitmask, 2},
               {get_test_engine, 0},
               %% Misc stuff - to be implemented
               {exor, 2},
               {mod_pow, 3},
               {hash_equals, 2},
               {strong_rand_bytes, 1},
               {strong_rand_range, 1},
               %% Hash - not supposed to be tested
               {hash_init, 1},
               %% Mac - to be implemented
               {mac, 3},
               {mac, 4},
               {macN, 4},
               {macN, 5},
               {mac_init, 2},
               {mac_init, 3},
               {mac_update, 2},
               {mac_final, 1},
               {mac_finalN, 2},
               %% Cipher - to be implemented
               {crypto_init, 3},
               {crypto_init, 4},
               {crypto_update, 2},
               {crypto_final, 1},
               {crypto_one_time, 4},
               {crypto_one_time, 5},
               %% AEAD - to be implemented
               {crypto_one_time_aead, 6},
               {crypto_one_time_aead, 7},
               {crypto_one_time_aead_init, 4},
               {crypto_one_time_aead, 4},
               %% Sign/verify - to be implemented
               {sign, 4},
               {sign, 5},
               {verify, 5},
               {verify, 6},
               %% Public/private encrypt/decrypt - to be implemented
               {public_encrypt, 4},
               {public_decrypt, 4},
               {private_encrypt, 4},
               {private_decrypt, 4},
               %% Generate key - to be implemented
               {generate_key, 2},
               {generate_key, 3},
               %% Compute key - to be implemented
               {compute_key, 4},
               %% KDF - to be implemented
               {pbkdf2_hmac, 5},
               %% PQC - to be implemented
               {encapsulate_key, 2},
               {decapsulate_key, 3}],
    Exports = crypto:module_info(exports),
    Ctx = proplists:get_value(ctx, Config),
    FAs = lists:flatten([lists:map(fun map_spec_to_fa/1, specs(TC, Ctx))
                         || TC <- all() -- [?FUNCTION_NAME]]),
    ToBeTested = Exports -- Ignored,
    case ToBeTested -- FAs of
        [] ->
            ok;
        Untested ->
            ct:fail("Exported but untested: ~p~n", [Untested])
    end,
    case Ignored -- Exports of
        [] ->
            ok;
        NotExist ->
            ct:fail("Ignored but doesn't exist: ~p~n", [NotExist])
    end.

%%%===================================================================
%%% Tracer
%%%===================================================================

tracer() ->
    spawn_link(fun() -> tracer_fun(maps:new()) end).

tracer_fun(State0) ->
    receive
        {monitor, Pid, long_schedule, Info} when is_pid(Pid) ->
            State = maps:update_with(Pid, fun(Infos) -> [Info | Infos] end, [Info], State0),
            tracer_fun(State);
        {monitor, _, _, _} ->
            tracer_fun(State0);
        {get_infos, From, Ref, Pid} ->
            Infos = maps:get(Pid, State0, []),
            From ! {Ref, Infos},
            tracer_fun(State0)
    end.

get_infos(Pid, Config) ->
    Tracer = proplists:get_value(tracer, Config),
    Ref = make_ref(),
    Tracer ! {get_infos, self(), Ref, Pid},
    receive {Ref, Infos} -> Infos end.

%%%===================================================================
%%% Runner
%%%===================================================================

run(SpecsFun, Config) ->
    Ctx = proplists:get_value(ctx, Config),
    case SpecsFun(Ctx) of
        [] ->
            {skip, "No supported algorithms"};
        Specs ->
            lists:foreach(fun(Spec) -> run_spec(Spec, Config) end, Specs)
    end.

run_spec(#spec{name = Name, setup = Setup, test = Test}, Config) ->
    State = run_setup(Setup, undefined),
    Parent = self(),
    Ref = make_ref(),
    {Pid, MRef} =
        spawn_opt(fun() ->
                          erlang:garbage_collect(),
                          Result = run_test(Test, State, ?CALLS),
                          Parent ! {Ref, Result, get_infos(self(), Config)}
                  end, [monitor, {min_heap_size, 100000}]),
    receive
        {Ref, Res, Infos} ->
            erlang:demonitor(MRef, [flush]),
            case [Res, Infos] of
                [ok, []] ->
                    ct:log("~ts: worked passed", [Name]);
                [ok, Infos] ->
                    ct:log("~ts: scheduler stall~n~ts", [Name, fmt_stalls(Infos)]),
                    ct:fail("~ts: scheduler stall, ~w event(s), max ~w ms",
                            [Name, length(Infos), max_stall(Infos)]);
                [Other, _] ->
                    ct:log("~ts: worker returned: ~0P", [Name, Other, 10])
            end;
        {'DOWN', MRef, process, Pid, Reason} ->
            ct:log("~ts: worker crashed: ~0P", [Name, Reason, 10])
    end.

run_setup(undefined, State) ->
    State;
run_setup({M, F, A}, _State) ->
    apply(M, F, A);
run_setup(Fun, _State) when is_function(Fun, 0) ->
    {M, F, A} = Fun(),
    apply(M, F, A);
run_setup([], State) ->
    State;
run_setup([{M, F, A} | Rest], _State0) ->
    State = apply(M, F, A),
    run_setup(Rest, State);
run_setup([Fun | Rest], _State0) when is_function(Fun, 0) ->
    {M, F, A} = Fun(),
    State = apply(M, F, A),
    run_setup(Rest, State);
run_setup([Fun | Rest], State0) when is_function(Fun, 1) ->
    {M, F, A} = Fun(State0),
    State = apply(M, F, A),
    run_setup(Rest, State).

run_test(_Test, _State, 0) ->
    ok;
run_test({M, F, A} = Test, State, RemainingCalls) ->
    erlang:yield(),
    apply(M, F, A),
    run_test(Test, State, RemainingCalls - 1);
run_test(Fun, State, RemainingCalls) when is_function(Fun, 1) ->
    erlang:yield(),
    {M, F, A} = Fun(State),
    apply(M, F, A),
    run_test(Fun, State, RemainingCalls - 1).

%%%===================================================================
%%% Specs
%%%===================================================================

hash_specs(#ctx{large = Large, max = Max}) ->
    lists:append([[
                   #spec{name = fmt("hash(~w, ~wB)", [H, ?MAX]),
                         test = {crypto, hash, [H, Max]}},
                   #spec{name = fmt("hash(~w, ~wB)", [H, ?LARGE]),
                         test = {crypto, hash, [H, Large]}},
                   #spec{name = fmt("hash_update(~w, ~wB)", [H, ?MAX]),
                         setup = {crypto, hash_init, [H]},
                         test = fun(S) -> {crypto, hash_update, [S, Max]} end},
                   #spec{name = fmt("hash_final(~w)", [H]),
                         setup = [{crypto, hash_init, [H]}, fun(S) -> {crypto, hash_update, [S, fill(127)]} end],
                         test = fun(S) -> {crypto, hash_final, [S]} end}]
                  || H <- hashes()]).

xof_specs(#ctx{max = Max}) ->
    lists:append([[
                   #spec{name = fmt("hash_final_xof(~w, ~wB out)", [H, Ln]),
                         setup = {crypto, hash_init, [H]},
                         test = fun(S) -> {crypto, hash_final_xof, [S, Ln * 8]} end},
                   #spec{name = fmt("hash_xof(~w, ~wB in, ~wB out)", [H, ?MAX, Ln]),
                         test = {crypto, hash_xof, [H, Max, Ln * 8]}}]
                  || H <- hashes(), Ln <- [?MAX, 1000000, ?LARGE]]).

%%%===================================================================
%%% Algorithm helpers
%%%===================================================================

hashes() ->
    lists:sort(crypto:supports(hashs)).

%%%===================================================================
%%% Helpers
%%%===================================================================

fill(N) when is_integer(N), N > 0 ->
    binary:copy(<<16#ff>>, N);
fill(_) ->
    <<>>.

fmt(Format, Data) ->
    io_lib:format(Format, Data).

specs(TC, Ctx) ->
    SpecFun = list_to_existing_atom(atom_to_list(TC) ++ "_specs"),
    ?MODULE:SpecFun(Ctx).

map_spec_to_fa(#spec{test = Test}) ->
    map_to_fa(Test).

map_to_fa(Fun) when is_function(Fun, 1) ->
    map_to_fa(Fun(ignore));
map_to_fa({_M, F, A}) ->
    {F, length(A)}.

fmt_stalls(Infos) ->
    [io_lib:format("  ~w ms~n", [proplists:get_value(timeout, I)])
     || I <- lists:reverse(Infos)].

max_stall(Infos) ->
    lists:max([proplists:get_value(timeout, I, 0) || I <- Infos]).
