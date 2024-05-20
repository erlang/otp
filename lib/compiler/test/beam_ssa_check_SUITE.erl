%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2024. All Rights Reserved.
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
-module(beam_ssa_check_SUITE).

%% Runs tests checking the structure of BEAM SSA code.

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/erl_compile.hrl").

-export([all/0, suite/0, groups/0,
         init_per_suite/1, end_per_suite/1,
	 init_per_group/2, end_per_group/2,

         alias_checks/1,
         annotation_checks/1,
         appendable_checks/1,
         bs_size_unit_checks/1,
         private_append_checks/1,
         ret_annotation_checks/1,
         sanity_checks/1,
         tuple_inplace_checks/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,post_ssa_opt_dynamic},{group,post_ssa_opt_static}].

groups() ->
    [{post_ssa_opt_static,test_lib:parallel(),
      [alias_checks,
       annotation_checks,
       appendable_checks,
       private_append_checks,
       ret_annotation_checks,
       sanity_checks,
       tuple_inplace_checks]},
     {post_ssa_opt_dynamic,test_lib:parallel(),
      [bs_size_unit_checks]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(post_ssa_opt_dynamic, Config) ->
    TargetDir = dynamic_workdir(Config),
    ct:log("Creating working directory for generated test cases: ~p~n",
           [TargetDir]),
    ok = file:make_dir(TargetDir),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(post_ssa_opt_dynamic, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    TargetDir = filename:join(PrivDir, "dynamic"),
    case proplists:get_bool(keep_generated, Config) of
        false ->
            ct:log("Removing working directory for generated test cases: ~p~n",
                   [TargetDir]),
            file:del_dir_r(TargetDir);
        true ->
            Config
    end;
end_per_group(_GroupName, Config) ->
    Config.

alias_checks(Config) when is_list(Config) ->
    run_post_ssa_opt(alias, Config),
    run_post_ssa_opt(alias_non_convergence, Config),
    run_post_ssa_opt(alias_chain, Config).

annotation_checks(Config) when is_list(Config) ->
    run_post_ssa_opt(annotations, Config).

appendable_checks(Config) when is_list(Config) ->
    run_post_ssa_opt(appendable, Config).

bs_size_unit_checks(Config) when is_list(Config) ->
    gen_and_run_post_ssa_opt(bs_size_unit_checks, Config).

private_append_checks(Config) when is_list(Config) ->
    run_post_ssa_opt(private_append, Config).

tuple_inplace_checks(Config) when is_list(Config) ->
    run_post_ssa_opt(tuple_inplace_checks, Config),
    run_post_ssa_opt(tuple_inplace_abort0, Config),
    run_post_ssa_opt(tuple_inplace_abort1, Config).

ret_annotation_checks(Config) when is_list(Config) ->
    run_post_ssa_opt(ret_annotation, Config).

sanity_checks(Config) when is_list(Config) ->
    run_post_ssa_opt(sanity_checks, Config).

dynamic_workdir(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    filename:join(PrivDir, "dynamic").

run_post_ssa_opt(Module, Config) ->
    File = atom_to_list(Module) ++ ".erl",

    DataDir = proplists:get_value(data_dir, Config),
    Source = filename:join(DataDir, File),
    run_checks(Source, post_ssa_opt, Config).

run_checks(SourceFile, Pass, _Config) ->
    Flags = [binary, deterministic, {check_ssa,Pass}, report_errors],
    case compile:file(SourceFile, Flags) of
        {ok,_,_} -> ok;
        error -> ct:fail({unexpected_error, "SSA check failed"})
    end.

gen_and_run_post_ssa_opt(Base, Config) ->
    BaseStr = atom_to_list(Base),
    GenFilenameBase = "gen_" ++ BaseStr,
    GenModule = list_to_atom(GenFilenameBase),
    GenFilename = filename:join(proplists:get_value(data_dir, Config),
                                GenFilenameBase ++ ".erl"),
    ct:log("Compiling generator ~s~n", [GenFilename]),
    {ok,GenModule,GenCode} = compile:file(GenFilename, [binary]),
    {module,GenModule} = code:load_binary(GenModule, GenFilename, GenCode),
    TargetFileName = filename:join(dynamic_workdir(Config), BaseStr ++ ".erl"),
    ct:log("Generating ~s~n", [TargetFileName]),
    ok = GenModule:generate(TargetFileName, Config),
    run_checks(TargetFileName, post_ssa_opt, Config).
