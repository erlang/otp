%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
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
-module(compile_SUITE).

%% Tests compile:file/1 and compile:file/2 with various options.

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/erl_compile.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 app_test/1,appup_test/1,bigE_roundtrip/1,
	 debug_info/4, custom_debug_info/1, custom_compile_info/1,
	 file_1/1, forms_2/1, module_mismatch/1, outdir/1,
	 binary/1, makedep/1, cond_and_ifdef/1, listings/1, listings_big/1,
	 other_output/1, encrypted_abstr/1,
	 strict_record/1, utf8_atoms/1, utf8_functions/1, extra_chunks/1,
	 cover/1, env/1, core_pp/1, tuple_calls/1,
	 core_roundtrip/1, asm/1, asm_labels/1,
	 sys_pre_attributes/1, dialyzer/1, no_core_prepare/1,
         beam_ssa_pp_smoke_test/1,
	 warnings/1, pre_load_check/1, env_compiler_options/1,
         bc_options/1, deterministic_include/1, deterministic_paths/1,
         deterministic_docs/1,
         compile_attribute/1, message_printing/1, other_options/1,
         transforms/1, erl_compile_api/1, types_pp/1, bs_init_writable/1,
         annotations_pp/1, option_order/1,
         sys_coverage/1
	]).

suite() -> [{ct_hooks,[ts_install_cth]}].

%% To cover the stripping of 'type' and 'spec' in beam_asm.
-type all_return_type() :: [atom()].
-spec all() -> all_return_type().

all() -> 
    [app_test, appup_test, bigE_roundtrip, file_1,
     forms_2, module_mismatch, outdir,
     binary, makedep, cond_and_ifdef, listings, listings_big,
     other_output, encrypted_abstr, tuple_calls,
     strict_record, utf8_atoms, utf8_functions, extra_chunks,
     cover, env, core_pp, core_roundtrip, asm, asm_labels, no_core_prepare,
     sys_pre_attributes, dialyzer, beam_ssa_pp_smoke_test,
     warnings, pre_load_check,
     env_compiler_options, custom_debug_info, bc_options,
     custom_compile_info, deterministic_include, deterministic_paths,
     deterministic_docs,
     compile_attribute, message_printing, other_options, transforms,
     erl_compile_api, types_pp, bs_init_writable, annotations_pp,
     option_order, sys_coverage].

groups() -> 
    [].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.



%% Test that the Application file has no `basic' errors.";
app_test(Config) when is_list(Config) ->
    test_server:app_test(compiler).

%% Test that the Application upgrade file has no `basic' errors.";
appup_test(Config) when is_list(Config) ->
    ok = test_server:appup_test(compiler).

%% Check that a file compiled to the abstract form and dumped with -E
%% can be compiled. We use a file constructed to produce errors if the
%% dumping fails to legalize compiler generated variable names.
bigE_roundtrip(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Source = filename:join(DataDir, "bigE.erl"),
    TargetDir = filename:join(PrivDir, "bigE"),
    Target = filename:join(TargetDir, "bigE.E"),
    TargetSource = filename:join(TargetDir, "bigE.erl"),
    ok = file:make_dir(TargetDir),
    io:format("Source: ~p~nTargetDir: ~p~nTarget: ~p\n",
              [Source, TargetDir, Target]),
    case compile:file(Source,
                      ['E', warnings_as_errors, {outdir, TargetDir}]) of
        {ok, _} -> ok;
        Other -> ct:fail({unexpected_result, Other})
    end,
    %% Rename the output to .erl so that the compiler accepts it and
    %% we won't get a warning due to the filename not matching the
    %% module name.
    ok = file:rename(Target, TargetSource),
    case compile:file(TargetSource,
                      [warnings_as_errors, {outdir, TargetDir}]) of
        {ok, _} -> ok;
        Other1 -> ct:fail({unexpected_result, Other1})
    end,
    file:delete(TargetSource),
    file:del_dir(TargetDir),
    ok.

%% Tests that we can compile and run a simple Erlang program,
%% using compile:file/1.

file_1(Config) when is_list(Config) ->
    process_flag(trap_exit, true),

    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),

    {Simple, Target} = get_files(Config, simple, "file_1"),
    {ok, Cwd} = file:get_cwd(),
    ok = file:set_cwd(filename:dirname(Target)),

    {ok,simple} = compile:file(Simple, [slim]), %Smoke test only.
    {ok,simple} = compile:file(Simple),	%Smoke test only.

    compile_and_verify(Simple, Target, []),
    compile_and_verify(Simple, Target, [debug_info]),
    compile_and_verify(Simple, Target, [no_postopt]),

    {ok,simple} = compile:file(Simple, [no_line_info]), %Coverage
    {ok,simple} = compile:file(Simple, [{eprof,beam_z}]), %Coverage
    {ok,simple} = compile:file(Simple, [{call_time,beam_z}]), %Coverage
    {ok,simple} = compile:file(Simple, [{call_memory,beam_z}]), %Coverage

    %% Cover option not in a list (undocumented feature).
    {ok,simple} = compile:file(Simple, no_postopt),

    %% Test compiling from an .abstr file.
    {ok,[]} = compile:file(Simple, [dabstr]),
    {ok,simple} = compile:file(filename:rootname(Target), [from_abstr]),
    ok = file:delete(filename:rootname(Target) ++ ".abstr"),

    %% Test option 'deterministic'.
    {ok,simple} = compile:file(Simple, [deterministic]),
    {module,simple} = c:l(simple),
    [{version,_}] = simple:module_info(compile),
    true = code:delete(simple),
    false = code:purge(simple),

    ok = file:set_cwd(Cwd),
    true = exists(Target),
    passed = run(Target, test, []),

    %% Test option 'deterministic' as a compiler attribute.
    Det = deterministic_module,
    {DetPath, DetTarget} = get_files(Config, Det, "det_target"),
    {ok,Det,DetCode} = compile:file(DetPath, [binary]),
    {module,Det} = code:load_binary(Det, "", DetCode),
    [{version,_}] = Det:module_info(compile),
    true = code:delete(Det),
    false = code:purge(Det),

    %% Cover error handling code.
    NonExisting = definitely__no__such__module,
    error = compile:file(NonExisting, [report]),
    error = compile:file(NonExisting, [from_abstr,report]),
    error = compile:file(NonExisting, [from_core,report]),
    error = compile:file(NonExisting, [from_asm,report]),

    error = compile:file(filename:join(DataDir, "bad_core"), [from_core,report]),
    error = compile:file(filename:join(DataDir, "bad_core_tokens"), [from_core,report]),

    %% Cover handling of obsolete options.
    ObsoleteOptions = [r18,r19,r20,r21,r22,r23,
                       no_bsm3,no_get_hd_tl,no_put_tuple2,no_utf8_atoms,
                       no_swap,no_init_yregs,no_shared_fun_wrappers,no_make_fun3],
    _ = [begin
             {error,[{_Simple,
                      [{none,compile,{obsolete_option,Opt}}]}],
              []} =
                 compile:file(Simple, [Opt,return]),
             error = compile:file(Simple, [Opt,report])
         end || Opt <- ObsoleteOptions],

    %% Create a directory with the same name as the temp file to cover
    %% handling of write errors.
    Simple = filename:join(DataDir, "simple"),
    SimpleTempFile = filename:join(PrivDir, "simple.bea#"),
    SimpleOutputFile = filename:join(PrivDir, "simple.beam"),
    ok = file:make_dir(SimpleTempFile),
    ok = file:make_dir(SimpleOutputFile),
    try
        error = compile:file(Simple, [{outdir,PrivDir}, report]),
        _ = file:del_dir(SimpleTempFile),
        error = compile:file(Simple, [{outdir,PrivDir}, report])
    after
        _ = file:del_dir(SimpleTempFile),
        _ = file:del_dir(SimpleOutputFile)
    end,

    %% Cleanup.
    ok = file:delete(Target),
    ok = file:del_dir(filename:dirname(Target)),
    ok = file:del_dir(filename:dirname(DetTarget)),

    %% There should not be any messages in the messages.
    receive
	Any ->
	    ct:fail({unexpected,Any})
    after 10 ->
	    ok
    end,

    ok.

forms_2(Config) when is_list(Config) ->
    {Simple, Target} = get_files(Config, simple, "file_1"),
    ok = file:del_dir(filename:dirname(Target)),

    Src = Simple,
    AbsSrc = filename:absname(Src),
    {ok,[],SimpleCode} = compile:file(Simple, [dabstr,binary]),

    {ok,simple,Bin1} = compile:forms(SimpleCode, [binary,{source,Src}]),
    {ok,simple,_} = compile:forms(SimpleCode,
                                  [binary,{error_location,line},{source,Src}]),

    %% Cover option not in a list (undocumented feature).
    {ok,simple,_} = compile:forms(SimpleCode, no_postopt),

    %% Load and test that the proper source is returned.
    AbsSrc = forms_load_code(simple, Src, Bin1),

    %% Work in a deleted directory.
    PrivDir = proplists:get_value(priv_dir, Config),
    WorkDir = filename:join(PrivDir, ?FUNCTION_NAME),
    ok = file:make_dir(WorkDir),
    ok = file:set_cwd(WorkDir),
    case os:type() of
	{unix,_} -> os:cmd("rm -rf " ++ WorkDir);
	_ -> ok
    end,
    {ok,simple,Bin2} = compile:forms(SimpleCode),
    undefined = forms_load_code(simple, "ignore", Bin2),

    {ok,simple,Bin3} = compile:forms(SimpleCode, [{source,Src},report]),
    case forms_load_code(simple, "ignore", Bin3) of
	Src ->					%Unix.
	    ok;
	AbsSrc ->				%Windows.
	    ok
    end,

    {ok,simple,Core} = compile:forms(SimpleCode, [to_core0,binary]),
    forms_compile_and_load(Core, [from_core]),

    {ok,simple,Asm} = compile:forms(SimpleCode, [to_asm,binary]),
    forms_compile_and_load(Asm, [from_asm]),

    %% The `from_abstr` option is redundant when compiling from forms,
    %% but it should work.
    forms_compile_and_load(SimpleCode, [from_abstr]),

    %% Cover the error handling code.
    error = compile:forms(bad_core, [from_core,report]),
    error = compile:forms(bad_asm, [from_asm,report]),

    ok.


forms_load_code(Mod, Src, Bin) ->
    {module,Mod} = code:load_binary(Mod, Src, Bin),
    Info = Mod:module_info(compile),
    SourceOption = proplists:get_value(source, Info),

    %% Ensure that the options are not polluted with 'source'.
    [] = proplists:get_value(options, Info),

    %% Cleanup.
    true = code:delete(simple),
    false = code:purge(simple),

    SourceOption.

forms_compile_and_load(Code, Opts) ->
    Mod = simple,
    {ok,Mod,Bin} = compile:forms(Code, Opts),
    {module,Mod} = code:load_binary(Mod, "ignore", Bin),
    _ = Mod:module_info(),
    true = code:delete(simple),
    false = code:purge(simple),
    ok.

module_mismatch(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    File = filename:join(DataDir, "wrong_module_name.erl"),
    {error,[{"wrong_module_name.beam",
	     [{none,compile,{module_name,arne,"wrong_module_name"}}]}],
	   []} = compile:file(File, [return]),
    error = compile:file(File, [report]),

    {ok,arne,[]} = compile:file(File,
				      [return,no_error_module_mismatch]),

    ok.

%% Tests that the {outdir, Dir} option works.

outdir(Config) when is_list(Config) ->
    {Simple, Target} = get_files(Config, simple, "outdir"),
    {ok, simple} = compile:file(Simple, [{outdir, filename:dirname(Target)}]),
    true = exists(Target),
    passed = run(Target, test, []),
    ok = file:delete(Target),
    ok = file:del_dir(filename:dirname(Target)),
    ok.

%% Tests that the binary option works.

binary(Config) when is_list(Config) ->
    {Simple, Target} = get_files(Config, simple, "binary"),
    {ok, simple, Binary} = compile:file(Simple, [binary]),
    code:load_binary(simple, Target, Binary),
    passed = simple:test(),
    true = code:delete(simple),
    false = code:purge(simple),
    ok = file:del_dir(filename:dirname(Target)),
    ok.

%% Tests that the dependencies-Makefile-related options work.

makedep(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),

    {Simple,Target} = get_files(Config, simple, "makedep"),
    SimpleRootname = filename:rootname(Simple),
    IncludeDir = filename:join(filename:dirname(Simple), "include"),
    IncludeOptions = [
		      {d,need_foo},
		      {d,foo_value,42},
		      {d,include_generated},
		      {i,IncludeDir}
		     ],

    %% Basic rule.
    BasicMf1Name = SimpleRootname ++ "-basic1.mk",
    {ok,BasicMf1} = file:read_file(BasicMf1Name),
    {ok,_,Mf1} = compile:file(Simple, [binary,makedep]),
    BasicMf1 = makedep_canonicalize_result(Mf1, DataDir),

    %% Basic rule with one existing header.
    BasicMf2Name = SimpleRootname ++ "-basic2.mk",
    {ok,BasicMf2} = file:read_file(BasicMf2Name),
    {ok,_,Mf2} = compile:file(Simple, [binary,makedep|IncludeOptions]),
    BasicMf2 = makedep_canonicalize_result(Mf2, DataDir),

    %% Rule with one existing header and one missing header.
    MissingMfName = SimpleRootname ++ "-missing.mk",
    {ok,MissingMf} = file:read_file(MissingMfName),
    {ok,_,Mf3} = compile:file(Simple,
      [binary,makedep,makedep_add_missing|IncludeOptions]),
    MissingMf = makedep_canonicalize_result(Mf3, DataDir),

    %% Rule with modified target.
    TargetMf1Name = SimpleRootname ++ "-target1.mk",
    {ok,TargetMf1} = file:read_file(TargetMf1Name),
    {ok,_,Mf4} = compile:file(Simple,
      [binary,makedep,{makedep_target,"$target"}|IncludeOptions]),
    TargetMf1 = makedep_modify_target(
      makedep_canonicalize_result(Mf4, DataDir), "$$target"),

    %% Rule with quoted modified target.
    TargetMf2Name = SimpleRootname ++ "-target2.mk",
    {ok,TargetMf2} = file:read_file(TargetMf2Name),
    {ok,_,Mf5} = compile:file(Simple,
      [binary,makedep,{makedep_target,"$target"},makedep_quote_target|
        IncludeOptions]),
    TargetMf2 = makedep_modify_target(
      makedep_canonicalize_result(Mf5, DataDir), "$$target"),

    %% Basic rule written to some file.
    {ok,_} = compile:file(Simple,
      [makedep,{makedep_output,Target}|IncludeOptions]),
    {ok,Mf6} = file:read_file(Target),
    BasicMf2 = makedep_canonicalize_result(Mf6, DataDir),

    %% Rule with creating phony target.
    PhonyMfName = SimpleRootname ++ "-phony.mk",
    {ok,PhonyMf} = file:read_file(PhonyMfName),
    {ok,_,Mf7} = compile:file(Simple,
      [binary,makedep,makedep_phony|IncludeOptions]),
    PhonyMf = makedep_canonicalize_result(Mf7, DataDir),

    %% Basic rule written to the default file.
    {ok,_} = compile:file(Simple, [makedep|IncludeOptions]),
    {ok,Mf8} = file:read_file(Target),
    BasicMf2 = makedep_canonicalize_result(Mf8, DataDir),

    %% Generate dependencies and compile normally at the same time.
    GeneratedHrl = filename:join(PrivDir, "generated.hrl"),
    GeneratedDoc = filename:join(proplists:get_value(data_dir, Config), "foo.md"),
    try
        ok = file:write_file(GeneratedHrl, ""),
        ok = file:write_file(GeneratedDoc, ""),
        {ok,simple} = compile:file(Simple, [report,makedep_side_effect,
                                            {makedep_output,Target},
                                            {i,PrivDir}|IncludeOptions]),
        {ok,Mf9} = file:read_file(Target),
        BasicMf3 = iolist_to_binary([string:trim(BasicMf2), " $(srcdir)/foo.md ", filename:join(PrivDir, "generated.hrl"), "\n"]),
        BasicMf3 = makedep_canonicalize_result(Mf9, DataDir),
        error = compile:file(Simple, [report,makedep_side_effect,
                                      {makedep_output,PrivDir}|IncludeOptions])
    after
        ok = file:delete(GeneratedHrl),
        ok = file:delete(GeneratedDoc)
    end,

    %% Cover generation of long lines that must be split.
    CompileModule = filename:join(code:lib_dir(compiler), "src/compile.erl"),
    {ok,_} = compile:file(CompileModule, [report,
                                          makedep,{makedep_output,standard_io},
                                          {i,filename:join(code:lib_dir(stdlib), "include")}]),

    %% Basic rule written to the standard output.
    {ok,_} = compile:file(Simple, [makedep,{makedep_output,standard_io}|IncludeOptions]),

    %% Test error handling.
    error = compile:file(Simple, [report,makedep,{makedep_output,DataDir}]),
    error = compile:file(Simple, [report,makedep,{makedep_output,a_bad_output_device}]),

    ok = file:delete(Target),
    ok = file:del_dir(filename:dirname(Target)),
    ok.

makedep_canonicalize_result(Mf, DataDir) ->
    Mf0 = binary_to_list(Mf),
    %% Replace the Datadir by "$(srcdir)".
    Mf1 = re:replace(Mf0, DataDir, "$(srcdir)/",
      [global,multiline,{return,list}]),
    %% Long lines are split, put back everything on one line.
    Mf2 = re:replace(Mf1, "\\\\\n  ", "", [global,multiline,{return,list}]),
    list_to_binary(Mf2).

makedep_modify_target(Mf, Target) ->
    Mf0 = binary_to_list(Mf),
    Mf1 = re:replace(Mf0, Target, "$target", [{return,list}]),
    list_to_binary(Mf1).

%% Tests that conditional compilation, defining values, including files work.

no_core_prepare(_Config) ->
    Mod = {c_module,[],
              {c_literal,[],sample_receive},
              [{c_var,[],{discard,0}}],
              [],
              [{{c_var,[],{discard,0}},
                {c_fun,[],[],
                    {c_case,[],
                        {c_values,[],[]},
                        [{c_clause,[],[],
                             {c_literal,[],true},
                             {c_receive,[],[],{c_literal,[],0},{c_literal,[],ok}}}]}}}]},

    {ok,sample_receive,_,_} = compile:forms(Mod, [from_core,binary,return]),
    {error,_,_} = compile:forms(Mod, [from_core,binary,return,no_core_prepare]),
    ok.

cond_and_ifdef(Config) when is_list(Config) ->
    {Simple, Target} = get_files(Config, simple, "cond_and_ifdef"),
    IncludeDir = filename:join(filename:dirname(Simple), "include"),
    Options = [{outdir, filename:dirname(Target)},
		     {d, need_foo}, {d, foo_value, 42},
		     {i, IncludeDir}, report],
    {ok, simple} = compile:file(Simple, Options),
    true = exists(Target),
    {hiker, 42} = run(Target, foo, []),
    ok = file:delete(Target),
    ok = file:del_dir(filename:dirname(Target)),
    ok.

listings(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    ok = do_file_listings(DataDir, PrivDir, [
	    "simple",
	    "small",
	    "small_maps"
	]),

    %% Cover handling of write errors.
    Simple = filename:join(DataDir, "simple"),
    SimpleTarget = filename:join(PrivDir, "simple.S"),
    ok = file:make_dir(SimpleTarget),
    try
        error = compile:file(Simple, ['S', {outdir,PrivDir}, report])
    after
        ok = file:del_dir(SimpleTarget)
    end,

    ok.

do_file_listings(_, _, []) -> ok;
do_file_listings(DataDir, PrivDir, [File|Files]) ->
    Simple = filename:join(DataDir, File),
    TargetDir = filename:join(PrivDir, listings),
    ok = file:make_dir(TargetDir),

    List = [{'S',".S"},
            {'E',".E"},
            {'P',".P"},
            {dpp, ".pp"},
            {dabstr, ".abstr"},
            {dexp, ".expand"},
            {dcore, ".core"},
            {doldinline, ".oldinline"},
            {dinline, ".inline"},
            {dcore, ".core"},
            {dcopt, ".copt"},
            {dcbsm, ".core_bsm"},
            {dssa, ".ssa"},
            {dbool, ".bool"},
            {dssashare, ".ssashare"},
            {dssaopt, ".ssaopt"},
            {dprecg, ".precodegen"},
            {dcg, ".codegen"},
            {dblk, ".block"},
            {djmp, ".jump"},
            {dclean, ".clean"},
            {dopt, ".optimize"},
            {diffable, ".S"}],
    p_listings(List, Simple, TargetDir),

    %% Test options that produce a listing file if 'binary' is not given.
    do_listing(Simple, TargetDir, to_pp, ".P"),
    do_listing(Simple, TargetDir, to_exp, ".E"),
    do_listing(Simple, TargetDir, to_core0, ".core"),
    ok = file:delete(filename:join(TargetDir, File ++ ".core")),
    do_listing(Simple, TargetDir, to_core, ".core"),
    do_listing(Simple, TargetDir, to_dis, ".dis"),

    %% Final clean up.
    lists:foreach(fun(F) -> ok = file:delete(F) end,
	filelib:wildcard(filename:join(TargetDir, "*"))),
    ok = file:del_dir(TargetDir),

    do_file_listings(DataDir,PrivDir,Files).

listings_big(Config) when is_list(Config) ->
    {Big,Target} = get_files(Config, big, listings_big),
    TargetDir = filename:dirname(Target),
    List = [{'S',".S"},
            {'E',".E"},
            {'P',".P"},
            {dssa, ".ssa"},
            {dssaopt, ".ssaopt"},
            {dprecg, ".precodegen"},
            {to_dis, ".dis"}],
    p_listings(List, Big, TargetDir).

p_listings(List, File, BaseDir) ->
    Run = fun({Option,Extension}) ->
                  Uniq = erlang:unique_integer([positive]),
                  Dir = filename:join(BaseDir, integer_to_list(Uniq)),
                  ok = file:make_dir(Dir),
                  try
                      do_listing(File, Dir, Option, Extension),
                      ok
                  catch
                      Class:Error:Stk ->
                          io:format("~p:~p\n~p\n", [Class,Error,Stk]),
                          error
                  after
                      _ = [ok = file:delete(F) ||
                              F <- filelib:wildcard(filename:join(Dir, "*"))],
                      ok = file:del_dir(Dir)
                  end
          end,
    test_lib:p_run(Run, List).

other_output(Config) when is_list(Config) ->
    {Simple,_Target} = get_files(Config, simple, "other_output"),

    io:put_chars("to_pp"),
    {ok,[],PP} = compile:file(Simple, [to_pp,binary,time]),
    [] = [E || E <- PP,
	       begin
		   case element(1, E) of
		       attribute -> false;
		       function -> false;
		       eof -> false
		   end
	       end],

    io:put_chars("to_exp (file)"),
    {ok,[],Expand} = compile:file(Simple, [to_exp,binary,time]),
    true = is_list(Expand),
    {attribute,_,module,simple} = lists:keyfind(module, 3, Expand),
    io:put_chars("to_exp (forms)"),
    {ok,[],Expand} = compile:forms(PP, [to_exp,binary,time]),

    io:put_chars("to_core (file)"),
    {ok,simple,Core} = compile:file(Simple, [to_core,binary,time]),
    c_module = element(1, Core),
    {ok,_} = core_lint:module(Core),
    io:put_chars("to_core (forms)"),
    {ok,simple,Core} = compile:forms(PP, [to_core,binary,time]),

    io:put_chars("to_asm (file)"),
    {ok,simple,Asm} = compile:file(Simple, [to_asm,binary,time]),
    {simple,_,_,_,_} = Asm,
    io:put_chars("to_asm (forms)"),
    {ok,simple,Asm} = compile:forms(PP, [to_asm,binary,time]),

    ok.

encrypted_abstr(Config) when is_list(Config) ->
    {Simple,Target} = get_files(Config, simple, "encrypted_abstr"),

    Res = case has_crypto() of
	      false ->
		  %% No crypto.
		  encrypted_abstr_no_crypto(Simple, Target),
		  {comment,"The crypto application is missing or broken"};
	      true ->
		  %% Simulate not having crypto by removing
		  %% the crypto application from the path.
		  OldPath = code:get_path(),
		  try
		      NewPath = OldPath -- [filename:dirname(code:which(crypto))],
		      (catch application:stop(crypto)),
		      code:delete(crypto),
		      code:purge(crypto),
		      code:set_path(NewPath),
		      encrypted_abstr_no_crypto(Simple, Target)
		      after
			  code:set_path(OldPath)
		      end,

		  %% Now run the tests that require crypto.
		  encrypted_abstr_1(Simple, Target),
		  ok = file:delete(Target),
		  _ = file:del_dir_r(filename:dirname(Target))
	  end,
    
    %% Cleanup.
    Res.

encrypted_abstr_1(Simple, Target) ->
    TargetDir = filename:dirname(Target),
    Key = "ablurf123BX#$;3",
    install_crypto_key(Key),
    {ok,simple} = compile:file(Simple,
				     [debug_info,{debug_info_key,Key},
				      {outdir,TargetDir}]),
    verify_abstract(Target, erl_abstract_code),

    {ok,simple} = compile:file(Simple,
				     [{debug_info_key,Key},
				      {outdir,TargetDir}]),
    verify_abstract(Target, erl_abstract_code),

    {ok,simple} = compile:file(Simple,
				     [debug_info,{debug_info_key,{des3_cbc,Key}},
				      {outdir,TargetDir}]),
    verify_abstract(Target, erl_abstract_code),

    {ok,simple} = compile:file(Simple,
				     [{debug_info,{?MODULE,ok}},
				      {debug_info_key,Key},
				      {outdir,TargetDir}]),
    verify_abstract(Target, ?MODULE),

    {ok,{simple,[{compile_info,CInfo}]}} =
	beam_lib:chunks(Target, [compile_info]),
    {_,Opts} = lists:keyfind(options, 1, CInfo),
    {_,'********'} = lists:keyfind(debug_info_key, 1, Opts),

    %% Try some illegal forms of crypto keys.
    error = compile:file(Simple,
			       [debug_info,{debug_info_key,{blurf,"ss"}},report]),
    error = compile:file(Simple,
			       [debug_info,{debug_info_key,{blurf,1,"ss"}},report]),
    error = compile:file(Simple,
			       [debug_info,{debug_info_key,42},report]),

    beam_lib:clear_crypto_key_fun(),

    %% Test to place the crypto file on disk. The test is dependent on the
    %% $HOME of the emulator, so we do this test in another node.
    TestHome = filename:join(TargetDir, "home"),
    filelib:ensure_dir(TestHome),
    HomeEnv = case os:type() of
                  {win32, _} ->
                      [Drive | Path] = filename:split(TestHome),
                      [{"APPDATA", filename:join(TestHome,"AppData")},
                       {"HOMEDRIVE", Drive}, {"HOMEPATH", filename:join(Path)}];
                  _ ->
                      [{"HOME", TestHome}]
              end,

    {ok, Peer, Node} = ?CT_PEER(#{ env => HomeEnv }),

    erpc:call(
      Node,
      fun() ->
              ok = file:set_cwd(TargetDir),

              error = compile:file(Simple, [encrypt_debug_info,report]),

              CWDKey = "better use another key here",
              CWDFile = ".erlang.crypt",
              XDGKey = "better use yet another key here",
              XDGFile = filename:join(
                          filename:basedir(user_config,"erlang"),
                          ".erlang.crypt"),
              HOMEKey = "better use the home key here",
              HOMEFile = filename:join(TestHome,".erlang.crypt"),

              write_crypt_file(CWDFile, CWDKey),
              write_crypt_file(XDGFile, XDGKey),
              write_crypt_file(HOMEFile, HOMEKey),

              %% First we test that .erlang.crypt in cwd works
              {ok,simple} = compile:file(Simple, [encrypt_debug_info,report]),
              verify_abstract("simple.beam", erl_abstract_code),
              ok = file:delete(CWDFile),
              beam_lib:clear_crypto_key_fun(),

              %% Then we test that .erlang.crypt in HOME does **not** work
              {error,beam_lib,{key_missing_or_invalid,"simple.beam",abstract_code}} =
                  beam_lib:chunks("simple.beam", [abstract_code]),

              %% Then we test that .erlang.crypt in HOME does work
              {ok,simple} = compile:file(Simple, [encrypt_debug_info,report]),
              verify_abstract("simple.beam", erl_abstract_code),
              ok = file:delete(HOMEFile),
              beam_lib:clear_crypto_key_fun(),

              %% Then we test that .erlang.crypt in XDG does **not** work
              {error,beam_lib,{key_missing_or_invalid,"simple.beam",abstract_code}} =
                  beam_lib:chunks("simple.beam", [abstract_code]),


              %% Then we test that .erlang.crypt in XDG does work
              {ok,simple} = compile:file(Simple, [encrypt_debug_info,report]),
              verify_abstract("simple.beam", erl_abstract_code),
              ok = file:delete(XDGFile),

              beam_lib:clear_crypto_key_fun(),
              {error,beam_lib,{key_missing_or_invalid,"simple.beam",abstract_code}} =
                  beam_lib:chunks("simple.beam", [abstract_code])

      end),

    peer:stop(Peer),

    %% Test key compatibility by reading a BEAM file produced before
    %% the update to the new crypto functions.
    install_crypto_key("an old key"),
    KeyCompat = filename:join(filename:dirname(Simple),
			      "key_compatibility"),
    {ok,{key_compatibility,[Chunk]}} = beam_lib:chunks(KeyCompat,
						       [abstract_code]),
    {abstract_code,{raw_abstract_v1,_}} = Chunk,

    ok.

write_crypt_file(File, Key) ->
    Contents = ["[{debug_info,des3_cbc,simple,\"",Key,"\"}].\n"],
    io:format("~s: ~s\n", [File, Contents]),
    ok = filelib:ensure_dir(File),
    ok = file:write_file(File, Contents).

encrypted_abstr_no_crypto(Simple, Target) ->
    io:format("simpe: ~p~n", [Simple]),
    TargetDir = filename:dirname(Target),
    Key = "ablurf123BX#$;3",
    error = compile:file(Simple,
			       [debug_info,{debug_info_key,Key},
				{outdir,TargetDir},report]),
    ok.
    
verify_abstract(Beam, Backend) ->
    {ok,{simple,[Abst, Dbgi]}} = beam_lib:chunks(Beam, [abstract_code, debug_info]),
    {abstract_code,{raw_abstract_v1,_}} = Abst,
    {debug_info,{debug_info_v1,Backend,_}} = Dbgi.

has_crypto() ->
    try
	application:start(crypto),
	application:stop(crypto),
	true
    catch
	error:_ -> false
    end.

install_crypto_key(Key) ->
    F = fun (init) -> ok;
	    ({debug_info,des3_cbc,_,_}) -> Key;
	    (clear) -> ok
	end,
    ok = beam_lib:crypto_key_fun(F).

%% Miscellaneous tests, mainly to get better coverage.
debug_info(erlang_v1, Module, ok, _Opts) ->
    {ok, [Module]};
debug_info(erlang_v1, _Module, error, _Opts) ->
    {error, unknown_format}.

custom_debug_info(Config) when is_list(Config) ->
    {Simple,_} = get_files(Config, simple, "file_1"),

    {ok,simple,OkBin} = compile:file(Simple, [binary, {debug_info,{?MODULE,ok}}]), %Coverage
    {ok,{simple,[{abstract_code,{raw_abstract_v1,[simple]}}]}} =
	beam_lib:chunks(OkBin, [abstract_code]),
    {ok,{simple,[{debug_info,{debug_info_v1,?MODULE,ok}}]}} =
	beam_lib:chunks(OkBin, [debug_info]),

    {ok,simple,ErrorBin} = compile:file(Simple, [binary, {debug_info,{?MODULE,error}}]), %Coverage
    {ok,{simple,[{abstract_code,no_abstract_code}]}} =
	beam_lib:chunks(ErrorBin, [abstract_code]),
    {ok,{simple,[{debug_info,{debug_info_v1,?MODULE,error}}]}} =
	beam_lib:chunks(ErrorBin, [debug_info]).

custom_compile_info(Config) when is_list(Config) ->
    Anno = erl_anno:new(1),
    Forms = [{attribute,Anno,module,custom_compile_info}],
    Opts = [binary,{compile_info,[{another,version}]}],

    {ok,custom_compile_info,Bin} = compile:forms(Forms, Opts),
    {ok,{custom_compile_info,[{compile_info,CompileInfo}]}} =
	beam_lib:chunks(Bin, [compile_info]),
    version = proplists:get_value(another, CompileInfo),
    CompileOpts = proplists:get_value(options, CompileInfo),
    undefined = proplists:get_value(compile_info, CompileOpts),

    {ok,custom_compile_info,DetBin} = compile:forms(Forms, [deterministic|Opts]),
    {ok,{custom_compile_info,[{compile_info,DetInfo}]}} =
	beam_lib:chunks(DetBin, [compile_info]),
    version = proplists:get_value(another, DetInfo).

cover(Config) when is_list(Config) ->
    io:format("~p\n", [compile:options()]),
    ok.

do_listing(Source, TargetDir, Type, Ext) ->
    io:format("Source: ~p TargetDir: ~p\n  Type: ~p Ext: ~p\n",
	      [Source, TargetDir, Type, Ext]),
    case compile:file(Source, [Type, time, {outdir, TargetDir}]) of
	{ok, _} -> ok;
	Other -> ct:fail({unexpected_result, Other})
    end,
    SourceBase = filename:rootname(filename:basename(Source)),

    Target = filename:join(TargetDir, SourceBase ++ Ext),
    true = exists(Target).

get_files(Config, Module, OutputName) ->
    code:delete(Module),
    code:purge(Module),
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Src = filename:join(DataDir, atom_to_list(Module)),
    TargetDir = filename:join(PrivDir, OutputName),
    ok = file:make_dir(TargetDir),
    File = atom_to_list(Module) ++ code:objfile_extension(),
    Target = filename:join(TargetDir, File),
    {Src, Target}.

run(Target, Func, Args) ->
    Module = list_to_atom(filename:rootname(filename:basename(Target))),
    {module, Module} = code:load_abs(filename:rootname(Target)),
    Result = (catch apply(Module, Func, Args)),
    true = code:delete(Module),
    false = code:purge(Module),
    Result.

exists(Name) ->
    case file:read_file_info(Name) of
	{ok, _}    -> true;
	{error, _} -> false
    end.


strict_record(Config) when is_list(Config) ->
    Priv = proplists:get_value(priv_dir, Config),
    ok = file:set_cwd(proplists:get_value(data_dir, Config)),
    Opts = [{outdir,Priv},report_errors],
    M = record_access,
 
    {ok,M} = c:c(M, [strict_record_tests|Opts]),
    Turtle = test_strict(),

    {ok,M} = c:c(M, [no_strict_record_tests|Opts]),
    Turtle = test_sloppy(),

    %% The option last given wins.
    {ok,M} = c:c(M, [strict_record_tests,no_strict_record_tests|Opts]),
    Turtle = test_sloppy(),
    {ok,M} = c:c(M, [no_strict_record_tests,strict_record_tests|Opts]),
    Turtle = test_strict(),

    %% Default (possibly influenced by ERL_COMPILER_OPTIONS).
    {ok,M} = c:c(M, [{outdir,Priv},report_errors]),
    try
        {1,2} = record_access:test(Turtle),
        {comment,"Default: no_strict_record_tests"}
    catch
        error:{badrecord,Turtle} ->
            {comment,"Default: strict_record_tests"}
    end.

test_strict() ->
    Turtle = record_access:turtle(),
    try
        record_access:test(Turtle)
    catch
        error:{badrecord,Turtle} ->
            ok
    end,
    Turtle.

test_sloppy() ->
    Turtle = record_access:turtle(),
    {1,2} = record_access:test(Turtle),
    Turtle.

utf8_atoms(Config) when is_list(Config) ->
    do_utf8_atom(binary_to_atom(<<"こんにちは"/utf8>>, utf8)),

    LongAtom = binary_to_atom(binary:copy(<<240,159,159,166>>, 255)),
    do_utf8_atom(LongAtom),

    ok.

do_utf8_atom(Atom) ->
    Mod = ?FUNCTION_NAME,
    Anno = erl_anno:new(1),
    Forms = [{attribute,Anno,module,Mod},
             {attribute,Anno,compile,[export_all]},
	     {function,Anno,atom,0,[{clause,Anno,[],[],[{atom,Anno,Atom}]}]}],

    {ok,Mod,Utf8AtomBin} = compile:forms(Forms, [binary,report]),
    {ok,{Mod,[{atoms,_}]}} = beam_lib:chunks(Utf8AtomBin, [atoms]),

    code:load_binary(Mod, "compile_SUITE", Utf8AtomBin),

    Atom = Mod:atom(),
    true = is_atom(Atom),

    true = code:delete(Mod),
    false = code:purge(Mod),

    ok.

utf8_functions(Config) when is_list(Config) ->
    Anno = erl_anno:new(1),
    Atom = binary_to_atom(<<"こんにちは"/utf8>>, utf8),
    Forms = [{attribute,Anno,compile,[export_all]},
	     {function,Anno,Atom,0,[{clause,Anno,[],[],[{atom,Anno,world}]}]}],

    Utf8FunctionForms = [{attribute,Anno,module,utf8_function}|Forms],
    {ok,utf8_function,Utf8FunctionBin} =
	compile:forms(Utf8FunctionForms, [binary]),
    {ok,{utf8_function,[{atoms,_}]}} =
	beam_lib:chunks(Utf8FunctionBin, [atoms]),
    code:load_binary(utf8_function, "compile_SUITE", Utf8FunctionBin),
    world = utf8_function:Atom(),

    NoUtf8FunctionForms = [{attribute,Anno,module,no_utf8_function}|Forms],
    error = compile:forms(NoUtf8FunctionForms, [binary, r19]).

extra_chunks(Config) when is_list(Config) ->
    Anno = erl_anno:new(1),
    Forms = [{attribute,Anno,module,extra_chunks}],

    {ok,extra_chunks,ExtraChunksBinary} =
	compile:forms(Forms, [binary, {extra_chunks, [{<<"ExCh">>, <<"Contents">>}]}]),
    {ok,{extra_chunks,[{"ExCh",<<"Contents">>}]}} =
	beam_lib:chunks(ExtraChunksBinary, ["ExCh"]).

tuple_calls(Config) when is_list(Config) ->
    Anno = erl_anno:new(1),
    Forms = [{attribute,Anno,export,[{size,1},{store,1}]},
	     {function,Anno,size,1,
	      [{clause,Anno,[{var,Anno,mod}],[],
	       [{call,Anno,{remote,Anno,{var,Anno,mod},{atom,Anno,size}},
                 []}]}]},
	     {function,Anno,store,1,
	      [{clause,Anno,[{var,Anno,mod}],[],
	       [{call,Anno,{remote,Anno,{var,Anno,mod},{atom,Anno,store}},
                 [{atom,Anno,key},{atom,Anno,value}]}]}]}],

    TupleCallsFalse = [{attribute,Anno,module,tuple_calls_false}|Forms],
    {ok,_,TupleCallsFalseBinary} = compile:forms(TupleCallsFalse, [binary]),
    code:load_binary(tuple_calls_false, "compile_SUITE.erl", TupleCallsFalseBinary),
    {'EXIT',{badarg,_}} = (catch tuple_calls_false:store(dict())),
    {'EXIT',{badarg,_}} = (catch tuple_calls_false:size(dict())),
    {'EXIT',{badarg,_}} = (catch tuple_calls_false:size(empty_tuple())),

    TupleCallsTrue = [{attribute,Anno,module,tuple_calls_true}|Forms],
    {ok,_,TupleCallsTrueBinary} = compile:forms(TupleCallsTrue, [binary,tuple_calls]),
    code:load_binary(tuple_calls_true, "compile_SUITE.erl", TupleCallsTrueBinary),
    Dict = tuple_calls_true:store(dict()),
    1 = tuple_calls_true:size(Dict),
    {'EXIT',{badarg,_}} = (catch tuple_calls_true:size(empty_tuple())),

    ok.

dict() ->
    dict:new().
empty_tuple() ->
    {}.

env(Config) when is_list(Config) ->
    {Simple,Target} = get_files(Config, simple, env),
    {ok,Cwd} = file:get_cwd(),
    ok = file:set_cwd(filename:dirname(Target)),

    true = os:putenv("ERL_COMPILER_OPTIONS", "binary"),
    try
	env_1(Simple, Target)
    after
	true = os:putenv("ERL_COMPILER_OPTIONS", "ignore_me"),
	file:set_cwd(Cwd),
	file:delete(Target),
	file:del_dir(filename:dirname(Target))
    end,
    ok.

env_1(Simple, Target) ->
    %% file
    {ok,simple,<<_/binary>>} = compile:file(Simple),
    {ok,simple} = compile:noenv_file(Simple, [debug_info]),
    {ok,simple} = compile:noenv_file(Simple, debug_info),

    true = exists(Target),
    {ok,{simple,[{abstract_code,Abstr0}]}} =
	beam_lib:chunks(Target, [abstract_code]),
    {raw_abstract_v1,Forms} = Abstr0,

    %% forms
    true = os:putenv("ERL_COMPILER_OPTIONS", "strong_validation"),
    {ok,simple} = compile:forms(Forms),
    {ok,simple,<<"FOR1",_/binary>>} = compile:noenv_forms(Forms, []),

    %% output_generated
    false = compile:output_generated([]),
    true = compile:noenv_output_generated([]),

    ok = file:delete(Target),

    %% Cover error handling.
    true = os:putenv("ERL_COMPILER_OPTIONS", "'unterminated_atom"),
    {ok,[]} = compile:forms(Forms, [basic_validation]),
    true = os:putenv("ERL_COMPILER_OPTIONS", ",,,"),
    {ok,[]} = compile:forms(Forms, [basic_validation]),
    {ok,simple,<<"FOR1",_/binary>>} = compile:noenv_forms(Forms, no_postopt),

    ok.

%% Test pretty-printing in Core Erlang format and then try to
%% compile the generated Core Erlang files.

core_pp(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Outdir = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME)),
    ok = file:make_dir(Outdir),

    TestBeams = get_unique_beam_files(),
    Abstr = [begin {ok,{Mod,[{abstract_code,
				    {raw_abstract_v1,Abstr}}]}} = 
			     beam_lib:chunks(Beam, [abstract_code]),
			 {Mod,Abstr} end || Beam <- TestBeams],
    test_lib:p_run(fun(F) -> do_core_pp(F, Outdir) end, Abstr).
    
do_core_pp({M,A}, Outdir) ->
    try
	do_core_pp_1(M, A, Outdir)
    catch
	throw:{error,Error} ->
	    io:format("*** compilation failure '~p' for module ~s\n",
		      [Error,M]),
	    error;
	Class:Error:Stk ->
	    io:format("~p: ~p ~p\n~p\n", [M,Class,Error,Stk]),
	    error
    end.

do_core_pp_1(M, A, Outdir) ->
    {ok,M,Core0} = compile:forms(A, [to_core]),
    CoreFile = filename:join(Outdir, atom_to_list(M)++".core"),
    CorePP = core_pp:format(Core0),
    ok = file:write_file(CoreFile, unicode:characters_to_binary(CorePP)),

    %% Parse the .core file and return the result as Core Erlang Terms.
    Core = case compile:file(CoreFile, [report_errors,from_core,no_copt,to_core,binary]) of
	       {ok,M,Core1} -> Core1;
	       Other -> throw({error,Other})
	   end,
    ok = file:delete(CoreFile),

    %% Compile as usual (including optimizations).
    compile_forms(M, Core, [clint,ssalint,from_core,binary]),

    %% Don't optimize to test that we are not dependent
    %% on the Core Erlang optimization passes.
    %% (Example of a previous bug: The core_parse pass
    %% would not turn map literals into #c_literal{}
    %% records; if sys_core_fold was run it would fix
    %% that; if sys_core_fold was not run v3_kernel would
    %% crash.)
    compile_forms(M, Core, [clint,ssalint,from_core,no_copt,binary]),

    ok.

compile_forms(Mod, Forms, Opts) ->
    case compile:forms(Forms, [report_errors|Opts]) of
	{ok,Mod,_} ->  ok;
	Other -> throw({error,Other})
    end.

%% Pretty-print core and read it back. Should be identical.

core_roundtrip(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Outdir = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME)),
    ok = file:make_dir(Outdir),

    TestBeams = get_unique_beam_files(),

    Test = fun(F) -> do_core_roundtrip(F, Outdir) end,
    test_lib:p_run(Test, TestBeams).

do_core_roundtrip(Beam, Outdir) ->
    try
	{ok,{Mod,[{abstract_code,{raw_abstract_v1,Abstr}}]}} =
	    beam_lib:chunks(Beam, [abstract_code]),
	do_core_roundtrip_1(Mod, Abstr, Outdir)
    catch
	throw:{error,Error} ->
	    io:format("*** compilation failure '~p' for file ~s\n",
		      [Error,Beam]),
	    error;
	Class:Error:Stk ->
	    io:format("~p: ~p ~p\n~p\n", [Beam,Class,Error,Stk]),
	    error
    end.

do_core_roundtrip_1(Mod, Abstr, Outdir) ->
    {ok,Mod,Core0} = compile:forms(Abstr, [to_core0]),
    do_core_roundtrip_2(Mod, Core0, Outdir),

    %% Primarily, test that annotations are accepted for all
    %% constructs. Secondarily, smoke test cerl_trees:label/1.
    {Core1,_} = cerl_trees:label(Core0),
    do_core_roundtrip_2(Mod, Core1, Outdir),

    %% Run the inliner to force generation of variables
    %% with numeric names.
    {ok,Mod,Core2} = compile:forms(Abstr, [inline,to_core]),
    do_core_roundtrip_2(Mod, Core2, Outdir).

do_core_roundtrip_2(M, Core0, Outdir) ->
    CoreFile = filename:join(Outdir, atom_to_list(M)++".core"),
    CorePP = core_pp:format_all(Core0),
    ok = file:write_file(CoreFile, unicode:characters_to_binary(CorePP)),

    %% Parse the .core file and return the result as Core Erlang Terms.
    Core2 = case compile:file(CoreFile, [report_errors,from_core,
					 no_copt,to_core,binary]) of
		{ok,M,Core1} -> Core1;
		Other -> throw({error,Other})
	    end,
    Core = undo_var_translation(Core2),
    ok = file:delete(CoreFile),

    case cmp_core(Core0, Core, M) of
	true -> ok;
	false -> error
    end.

undo_var_translation(Tree) ->
    F = fun(Node) ->
		case cerl:is_c_var(Node) of
		    true ->
			Name0 = cerl:var_name(Node),
			try atom_to_list(Name0) of
			    "_X"++Name ->
				cerl:update_c_var(Node, list_to_atom(Name));
			    "_"++Name ->
				cerl:update_c_var(Node, list_to_atom(Name));
			    _ ->
				Node
			catch
			    error:badarg ->
				Node

			end;
		    false ->
			Node
		end
	end,
    cerl_trees:map(F, Tree).

cmp_core(E, E, _Mod) ->
    true;
cmp_core(M1, M2, Mod) ->
    cmp_core_fs(cerl:module_defs(M1), cerl:module_defs(M2), Mod).

cmp_core_fs([F1|T1], [F2|T2], Mod) ->
    cmp_core_f(F1, F2, Mod) andalso cmp_core_fs(T1, T2, Mod);
cmp_core_fs([], [], _Mod) ->
    true;
cmp_core_fs(_, _, _Mod) ->
    false.

cmp_core_f(E, E, _Mod) ->
    true;
cmp_core_f({Name,F1}, {Name,F2}, Mod) ->
    case diff(F1, F2) of
	F1 ->
	    true;
	Diff ->
	    io:format("~p ~p:\n~p\n", [Mod,Name,Diff]),
	    false
    end.

diff(E, E) ->
    E;
diff([H1|T1], [H2|T2]) ->
    [diff(H1, H2)|diff(T1, T2)];
diff(T1, T2) when tuple_size(T1) =:= tuple_size(T2) ->
    case cerl:is_c_var(T1) andalso cerl:is_c_var(T2) of
        true ->
            diff_var(T1, T2);
        false ->
            case cerl:is_c_map(T1) andalso cerl:is_c_map(T2) of
                true ->
                    diff_map(T1, T2);
                false ->
                    diff_tuple(T1, T2)
            end
    end;
diff(E1, E2) ->
    {'DIFF',E1,E2}.

diff_var(V1, V2) ->
    case {cerl:var_name(V1),cerl:var_name(V2)} of
        {Same,Same} ->
            V1;
        {Name1,Name2} ->
            %% The inliner uses integers as variable names. Such integers
            %% are read back as atoms.
            case is_integer(Name1) andalso
                list_to_atom(integer_to_list(Name1)) =:= Name2 of
                true ->
                    V1;
                _ ->
                    cerl:update_c_var(V1, {'DIFF',Name1,Name2})
            end
    end.

%% Annotations for maps are not preserved exactly, but that is not
%% a real problem. Workaround by not comparing all annotations when
%% comparing maps.

diff_map(M, M) ->
    M;
diff_map(M1, M2) ->
    case cerl:get_ann(M1) =:= cerl:get_ann(M2) of
        false ->
            diff_tuple(M1, M2);
        true ->
            case remove_compiler_gen(M1) =:= remove_compiler_gen(M2) of
                true ->
                    M1;
                false ->
                    diff_tuple(M1, M2)
            end
    end.

diff_tuple(T1, T2) ->
    L = diff(tuple_to_list(T1), tuple_to_list(T2)),
    list_to_tuple(L).

remove_compiler_gen(M) ->
    Arg0 = cerl:map_arg(M),
    Arg = cerl:set_ann(Arg0, []),
    Es0 = cerl:map_es(M),
    Es = [remove_compiler_gen_1(Pair) || Pair <- Es0],
    cerl:update_c_map(M, Arg, Es).

remove_compiler_gen_1(Pair) ->
    Op0 = cerl:map_pair_op(Pair),
    Op = cerl:set_ann(Op0, []),
    K = map_var(cerl:map_pair_key(Pair)),
    V = map_var(cerl:map_pair_val(Pair)),
    cerl:update_c_map_pair(Pair, Op, K, V).

map_var(Var) ->
    case cerl:is_c_var(Var) of
        true ->
            case cerl:var_name(Var) of
                Name when is_atom(Name) ->
                    L = atom_to_list(Name),
                    try list_to_integer(L) of
                        Int ->
                            cerl:update_c_var(Var, Int)
                    catch
                        error:_ ->
                            Var
                    end;
                _ ->
                    Var
            end;
        false ->
            Var
    end.

%% Compile to Beam assembly language (.S) and then try to
%% run .S through the compiler again.

asm(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Outdir = filename:join(PrivDir, "asm"),
    ok = file:make_dir(Outdir),

    TestBeams = get_unique_beam_files(),
    Res = test_lib:p_run(fun(F) -> do_asm(F, Outdir) end, TestBeams),
    Res.

    
do_asm(Beam, Outdir) ->
    {ok,{M,[{abstract_code,{raw_abstract_v1,A}}]}} =
	beam_lib:chunks(Beam, [abstract_code]),
    try
        Opts = test_lib:opt_opts(M),
	{ok,M,Asm} = compile:forms(A, ['S'|Opts]),
	AsmFile = filename:join(Outdir, atom_to_list(M)++".S"),
	{ok,Fd} = file:open(AsmFile, [write,{encoding,utf8}]),
	beam_listing:module(Fd, Asm),
	ok = file:close(Fd),
        case compile:file(AsmFile, [from_asm,binary,report|Opts]) of
	    {ok,M,_} ->
		ok = file:delete(AsmFile);
	    Other ->
		io:format("*** failure '~p' for ~s\n",
			  [Other,AsmFile]),
		error
	end
    catch Class:Error:Stk ->
	    io:format("~p: ~p ~p\n~p\n", [M,Class,Error,Stk]),
	    error
    end.

%% Compile a crafted file which produces the three call instructions
%% which should have a comment with the called function in clear
%% text. We check that the expected functions and comments occur in
%% the listing.

asm_labels(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    InFile = filename:join(DataDir, "asm_labels.erl"),
    OutDir = filename:join(PrivDir, "asm_labels"),
    OutFile = filename:join(OutDir, "asm_labels.S"),
    ok = file:make_dir(OutDir),
    {ok,asm_labels} = compile:file(InFile, ['S',{outdir,OutDir}]),
    {ok,Listing} = file:read_file(OutFile),
    Os = [global,multiline,{capture,all_but_first,list}],
    {match,[_]} = re:run(Listing, "({call,.+,{f,.+}}\\. % foo/1)", Os),
    {match,[_]} = re:run(Listing, "({call_only,.+,{f,.+}}\\. % foo/1)", Os),
    {match,[_]} = re:run(Listing, "({call_last,.+,{f,.+},.+}\\. % bar/1)", Os),
    ok = file:del_dir_r(OutDir).

sys_pre_attributes(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    File = filename:join(DataDir, "attributes.erl"),
    Mod = attributes,
    CommonOpts = [binary,report,verbose,
		  {parse_transform,sys_pre_attributes}],
    PreOpts = [{attribute,delete,deleted}],
    PostOpts = [{attribute,insert,inserted,"value"}],
    PrePostOpts = [{attribute,replace,replaced,42},
		   {attribute,replace,replace_nonexisting,new}],
    {ok,Mod,Code} = compile:file(File, PrePostOpts ++ PreOpts ++
				     PostOpts ++ CommonOpts),
    code:load_binary(Mod, File, Code),
    Attr = Mod:module_info(attributes),
    io:format("~p", [Attr]),
    {inserted,"value"} = lists:keyfind(inserted, 1, Attr),
    {replaced,[42]} = lists:keyfind(replaced, 1, Attr),
    {replace_nonexisting,[new]} = lists:keyfind(replace_nonexisting, 1, Attr),
    false = lists:keymember(deleted, 1, Attr),

    %% Cover more code.
    {ok,Mod,_} = compile:file(File, PostOpts ++ CommonOpts),
    {ok,Mod,_} = compile:file(File, CommonOpts -- [verbose]),
    {ok,Mod,_} = compile:file(File, PreOpts ++ CommonOpts),
    {ok,Mod,_} = compile:file(File,
			      [{attribute,replace,replaced,42}|CommonOpts]),
    {ok,Mod,_} = compile:file(File, PrePostOpts ++ PreOpts ++
				  PostOpts ++ CommonOpts --
				  [report,verbose]),
    ok.

%% Test the dialyzer option to cover more code.
dialyzer(Config) ->
    Priv = proplists:get_value(priv_dir, Config),
    ok = file:set_cwd(proplists:get_value(data_dir, Config)),
    Opts = [{outdir,Priv},report_errors],
    M = dialyzer_test,
    {ok,M} = c:c(M, [dialyzer|Opts]),
    [{a,b,c}] = M:M(),

    %% Cover huge line numbers without the 'dialyzer' option.
    {ok,M} = c:c(M, Opts),
    [{a,b,c}] = M:M(),
    ok.

beam_ssa_pp_smoke_test(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Outdir = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME)),
    ok = file:make_dir(Outdir),
    TestBeams = get_unique_beam_files(),
    test_lib:p_run(fun(F) -> beam_ssa_pp(F, Outdir) end, TestBeams).

beam_ssa_pp(Beam, Outdir) ->
    try
	{ok,{Mod,[{abstract_code,{raw_abstract_v1,Abstr}}]}} =
	    beam_lib:chunks(Beam, [abstract_code]),
	beam_ssa_pp_1(Mod, Abstr, Outdir)
    catch
	throw:{error,Error} ->
	    io:format("*** compilation failure '~p' for file ~s\n",
		      [Error,Beam]),
	    error;
	Class:Error:Stk ->
	    io:format("~p: ~p ~p\n~p\n", [Beam,Class,Error,Stk]),
	    error
    end.

beam_ssa_pp_1(Mod, Abstr, Outdir) ->
    Opts = test_lib:opt_opts(Mod),
    {ok,Mod,SSA} = compile:forms(Abstr, [dssaopt|Opts]),
    ListFile = filename:join(Outdir, atom_to_list(Mod) ++ ".ssaopt"),
    {ok,Fd} = file:open(ListFile, [write,{encoding,utf8}]),
    beam_listing:module(Fd, SSA),
    ok = file:close(Fd).

%% Test that warnings contain filenames and line numbers.
warnings(_Config) ->
    Files = test_lib:get_unique_files(".erl"),
    test_lib:p_run(fun do_warnings/1, Files).

do_warnings(F) ->
    Options = [{feature,maybe_expr,enable},binary,bin_opt_info,recv_opt_info,return],
    {ok,_,_,Ws} = compile:file(F, Options),
    do_warnings_1(Ws, F).

do_warnings_1([{"no_file",Ws}|_], F) ->
    io:format("~s:\nMissing file for warnings: ~p\n",
	      [F,Ws]),
    error;
do_warnings_1([{Name,Ws}|T], F) ->
    case filename:extension(Name) of
	".erl" ->
	    do_warnings_2(Ws, T, F);
	_ ->
	    io:format("~s:\nNo .erl extension\n", [F]),
	    error
    end;
do_warnings_1([], _) -> ok.

do_warnings_2([{Pos,_,_}=W|T], Next, F) ->
    case Pos of
	Line when is_integer(Line) ->
	    do_warnings_2(T, Next, F);
	{Line,Col} when is_integer(Line), is_integer(Col) ->
	    do_warnings_2(T, Next, F);
	true ->
	    io:format("~s:\nMissing line number: ~p\n",
		      [F,W]),
	    error
    end;
do_warnings_2([], Next, F) ->
    do_warnings_1(Next, F).

message_printing(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),

    BadEncFile = filename:join(DataDir, "bad_enc.erl"),
    {error,BadEncErrors, []} = compile:file(BadEncFile, [return]),

    [":7:15: cannot parse file, giving up\n"
     "%    7| \t    {ok, \"xyz\n"
     "%     | \t             ^\n\n"
    ,
     ":7:15: cannot translate from UTF-8\n"
     "%    7| \t    {ok, \"xyz\n"
     "%     | \t             ^\n\n"
    ] = messages(BadEncErrors),

    UTF8File = filename:join(DataDir, "col_utf8.erl"),
    {ok,_,UTF8Errors} = compile:file(UTF8File, [return]),
    [":5:23: a term is constructed, but never used\n"
     "%    5|     B = <<\"xyzåäö\">>,\t<<\"12345\">>,\n"
     "%     |                      \t^\n\n"
    ] = messages(UTF8Errors),

    Latin1File = filename:join(DataDir, "col_lat1.erl"),
    {ok,_,Latin1Errors} = compile:file(Latin1File, [return]),
    [":6:23: a term is constructed, but never used\n"
     "%    6|     B = <<\"xyzåäö\">>,\t<<\"12345\">>,\n"
     "%     |                      \t^\n\n"
    ] = messages(Latin1Errors),

    LongFile = filename:join(PrivDir, "long.erl"),
    Long = ["-module(long).\n",
            "-export([foo/0]).\n",
            "unused() -> ok.\n",
            lists:duplicate(10000, $\n),
            "foo() -> bar().\n"],
    ok = file:write_file(LongFile, Long),
    {error,LongErrors,LongWarnings} = compile:file(LongFile, [return]),
    [":10004:10: function bar/0 undefined\n"
     "% 10004| foo() -> bar().\n"
     "%      |          ^\n\n"
    ] = messages(LongErrors),
    [":3:1: function unused/0 is unused\n"
     "%    3| unused() -> ok.\n"
     "%     | ^\n\n"
    ] = messages(LongWarnings),
    ok = file:delete(LongFile),

    {ok,OldCwd} = file:get_cwd(),
    try
        ok = file:set_cwd(DataDir),
        {ok,cover_messages,_} = compile:file(cover_messages, [report, binary])
    after
        file:set_cwd(OldCwd)
    end,

    ok.

messages(Errors) ->
    lists:flatmap(fun ({{File,_L},Descs}) -> format_descs(File, Descs);
                      ({File,Descs}) -> format_descs(File, Descs)
                  end,
                  Errors).

format_descs(File, Descs) ->
    [strip_prefix(File, lists:flatten(Text))
     || {_Where, Text} <- sys_messages:format_messages(File, "", Descs, [])].

strip_prefix(Prefix, String) ->
    case string:prefix(String, Prefix) of
        nomatch -> String;
        Rest -> Rest
    end.

%% Test that the compile:pre_load/0 function (used by 'erlc')
%% pre-loads the modules that are used by a typical compilation.

pre_load_check(Config) ->
    case test_server:is_cover() of
        true ->
            {skip,"Cover is running"};
        false ->
            try
                do_pre_load_check(Config)
            after
                dbg:stop()
            end
    end.

do_pre_load_check(Config) ->
    DataDir = ?config(data_dir, Config),
    Simple = filename:join(DataDir, "simple.erl"),
    Big = filename:join(DataDir, "big.erl"),
    {ok,_} = dbg:tracer(process, {fun pre_load_trace/2,[]}),
    dbg:p(self(), call),
    dbg:p(new, call),
    {ok,_} = dbg:tpl({?MODULE,get_trace_data,0}, []),
    {ok,_} = dbg:tp({code,ensure_modules_loaded,1}, []),

    %% Compile a simple module using the erl_compile interface
    %% to find out the modules that are pre-loaded by
    %% compile:pre_load/0.
    Opts = #options{specific=[binary]},
    {ok,simple,_} = compile:compile(Simple, "", Opts),
    [{code,ensure_modules_loaded,[PreLoaded0]}] = get_trace_data(),
    PreLoaded1 = ordsets:from_list(PreLoaded0),

    %% Since 'compile' is the function doing the pre-loaded,
    %% it is useless to include it in the list.
    case ordsets:is_element(compile, PreLoaded1) of
	true ->
	    io:put_chars("The 'compile' module should not be included "
			 "in the list of modules to be pre-loaded."),
	    ct:fail(compile);
	false ->
	    []
    end,
    PreLoaded = ordsets:add_element(compile, PreLoaded1),

    %% Now unload all pre-loaded modules and all modules in
    %% compiler application. Then compile a module to find
    %% which modules that get loaded.
    CompilerMods = compiler_modules(),
    Unload = ordsets:union(ordsets:from_list(CompilerMods), PreLoaded),
    _ = [begin
	     code:delete(M),
	     code:purge(M)
	 end || M <- Unload],

    {ok,_} = dbg:ctp({code,ensure_modules_loaded,1}),
    {ok,_} = dbg:tp({code,ensure_loaded,1}, []),
    {ok,big,_} = compile:file(Big, [binary]),
    WasLoaded0 = get_trace_data(),
    WasLoaded1 = [M || {code,ensure_loaded,[M]} <- WasLoaded0],
    WasLoaded = ordsets:from_list(WasLoaded1),

    %% Check for modules that should have been pre-loaded.
    case ordsets:subtract(WasLoaded, PreLoaded) of
	[] ->
	    ok;
	[_|_]=NotPreLoaded ->
	    io:format("The following modules were used "
		      "but not pre-loaded:\n~p\n",
		      [NotPreLoaded]),
	    ct:fail({not_preload,NotPreLoaded})
    end,

    %% Check for modules that should not be pre-loaded.
    case ordsets:subtract(PreLoaded, WasLoaded) of
	[] ->
	    ok;
	[_|_]=NotUsed ->
	    io:format("The following modules were pre-loaded"
		      " but not used:\n~p\n",
		      [NotUsed]),
	    ct:fail({not_used,NotUsed})
    end,

    ok.

get_trace_data() ->
    %% Apparantely, doing a receive at the beginning of
    %% a traced function can cause extra trace messages.
    %% To avoid that, don't do the receive in this function.
    do_get_trace_data().

do_get_trace_data() ->
    receive
	{trace_data,Data} -> Data
    end.

pre_load_trace({trace,Pid,call,{?MODULE,get_trace_data,[]}}, Acc) ->
    Pid ! {trace_data,Acc},
    [];
pre_load_trace({trace,_,call,MFA}, Acc) ->
    [MFA|Acc].

compiler_modules() ->
    Wc = filename:join([code:lib_dir(compiler),"ebin","*.beam"]),
    Ms = filelib:wildcard(Wc),
    FN = filename,
    [list_to_atom(FN:rootname(FN:basename(M), ".beam")) || M <- Ms].

%% Test that ERL_COMPILER_OPTIONS are correctly retrieved
%% by env_compiler_options/0

env_compiler_options(_Config) ->
    Cases = [
        {"bin_opt_info", [bin_opt_info]},
        {"recv_opt_info", [recv_opt_info]},
        {"'S'", ['S']},
        {"{source, \"test.erl\"}", [{source, "test.erl"}]},
        {"[{d,macro_one,1},{d,macro_two}]", [{d, macro_one, 1}, {d, macro_two}]},
        {"[warn_export_all, warn_export_vars]", [warn_export_all, warn_export_vars]}
    ],
    F = fun({Env, Expected}) ->
        true = os:putenv("ERL_COMPILER_OPTIONS", Env),
        Expected = compile:env_compiler_options()
    end,
    lists:foreach(F, Cases).

%% Test options for compatibility with previous major versions of OTP.

bc_options(Config) ->
    DataDir = proplists:get_value(data_dir, Config),

    DataDir = proplists:get_value(data_dir, Config),

    L = [{177, small_float, []},

         {177, small, [no_ssa_opt_record,
                       no_ssa_opt_float,
                       no_line_info,
                       no_type_opt,
                       no_bs_match]},

         {177, funs, [no_ssa_opt_record,
                      no_ssa_opt_float,
                      no_line_info,
                      no_stack_trimming,
                      no_type_opt]},

         {177, small_maps, [no_type_opt]},

         {177, big, [no_ssa_opt_record,
                     no_ssa_opt_float,
                     no_line_info,
                     no_type_opt]},

         {178, small, [r25]},
         {178, big, [r25]},
         {178, funs, []},
         {178, big, []},

         {182, small, [r26]},
         {182, small, []},

         {183, small, [line_coverage]},

         {184, small, [beam_debug_info]},
         {184, big, [beam_debug_info]}
        ],

    Test = fun({Expected,Mod,Options}) ->
                   case highest_opcode(DataDir, Mod, Options) of
                       Expected ->
                           ok;
                       Got ->
                           io:format("*** module ~p, options ~p => got ~p; expected ~p\n",
                                     [Mod,Options,Got,Expected]),
                           error
                   end
           end,
    test_lib:p_run(Test, L),
    ok.

highest_opcode(DataDir, Mod, Opt) ->
    Src = filename:join(DataDir, atom_to_list(Mod)++".erl"),
    {ok,Mod,Beam} = compile:file(Src, [binary,report_errors|Opt]),
    test_lib:highest_opcode(Beam).

deterministic_include(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Simple = filename:join(DataDir, "simple"),
 
    %% Files without +deterministic should differ if their include paths do,
    %% as their debug info will be different.
    {ok,_,NonDetA} = compile:file(Simple, [binary, {i,"gurka"}]),
    {ok,_,NonDetB} = compile:file(Simple, [binary, {i,"gaffel"}]),
    true = NonDetA =/= NonDetB,

    %% ... but files with +deterministic shouldn't.
    {ok,_,DetC} = compile:file(Simple, [binary, deterministic, {i,"gurka"}]),
    {ok,_,DetD} = compile:file(Simple, [binary, deterministic, {i,"gaffel"}]),
    true = DetC =:= DetD,

    ok.

deterministic_paths(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),

    %% Files without +deterministic should differ if they were compiled from a
    %% different directory.
    true = deterministic_paths_1(DataDir, "simple", []),

    %% ... but files with +deterministic shouldn't.
    false = deterministic_paths_1(DataDir, "simple", [deterministic]),

    ok.

deterministic_paths_1(DataDir, Name, Opts) ->
    Simple = filename:join(DataDir, "simple"),
    {ok, Cwd} = file:get_cwd(),
    try
        {ok,_,A} = compile:file(Simple, [binary | Opts]),
        ok = file:set_cwd(DataDir),
        {ok,_,B} = compile:file(Name, [binary | Opts]),
        A =/= B
    after
        file:set_cwd(Cwd)
    end.

%% The test case uses ssh.erl from ssh application.
deterministic_docs(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Filepath = filename:join(DataDir, "ssh"),
    true = deterministic_docs_1(Filepath, [binary, deterministic], 25),
    ok.

deterministic_docs_1(Filepath, Opts, Checks) ->
    {ok, _, Reference} = compile:file(Filepath, Opts),
    lists:all(
      fun(_) ->
              {ok, Peer, Node} = ?CT_PEER(#{}),
              {ok, _, Testing} =
                  erpc:call(
                    Node,
                    fun() ->
                            compile:file(Filepath, Opts)
                    end),
              peer:stop(Peer),
              Testing =:= Reference
      end, lists:seq(1, Checks)).

%% ERL-1058: -compile(debug_info) had no effect
compile_attribute(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),

    %% The test module has a -compile([debug_info]). attribute, which means
    %% debug information should always be included.
    debug_info_attribute(DataDir, "debug_info", [debug_info]),
    debug_info_attribute(DataDir, "debug_info", []),

    ok.

debug_info_attribute(DataDir, Name, Opts) ->
    File = filename:join(DataDir, Name),
    {ok,_,Bin} = compile:file(File, [binary | Opts]),
    {ok, {_, Attrs}} = beam_lib:chunks(Bin, [debug_info]),

    [{debug_info,{debug_info_v1,erl_abstract_code, {Forms, _}}}] = Attrs,
    [{attribute,{1,1},file,{_,1}},
     {attribute,{1,2},module,debug_info},
     {attribute,{2,2},compile,[debug_info]},
     {eof,_}] = forms_to_terms(Forms),

    ok.

forms_to_terms(Forms) ->
    [erl_parse:anno_to_term(Form) || Form <- Forms].

%% Test compiler options not tested in other test cases.
other_options(Config) ->
    DataDir = proplists:get_value(data_dir, Config),

    %% Smoke test of no_spawn_compiler_process, brief, and absolute_source
    %% options.
    Big = filename:join(DataDir, "big"),
    {ok,big,<<_/binary>>} =
        compile:file(Big, [binary,
                           no_spawn_compiler_process,
                           brief,
                           absolute_source,
                           report]),

    %% Test generating a compressed BEAM file. Also cover the redundant
    %% `beam` option and the `no_inline` option.
    Small = filename:join(DataDir, "small"),
    {ok,small} = compile:file(Small, [report, no_inline, compressed, beam]),

    ok.

%% Test core transforms and parse transforms.
transforms(Config) ->
    {ok, Cwd} = file:get_cwd(),
    try
        do_transforms(Config)
    after
        file:set_cwd(Cwd)
    end.

do_transforms(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    TargetDir = filename:join(PrivDir, ?FUNCTION_NAME),
    ok = file:make_dir(TargetDir),
    ok = file:set_cwd(TargetDir),

    %% Compile our parse transforms.
    LinePt = filename:join(DataDir, "line_pt"),
    {ok,line_pt} = compile:file(LinePt, [report, {outdir,TargetDir}]),
    ColumnPt = filename:join(DataDir, "column_pt"),
    {ok,column_pt} = compile:file(ColumnPt, [report, {outdir,TargetDir}]),
    GenericPt = filename:join(DataDir, "generic_pt"),
    {ok,generic_pt} = compile:file(GenericPt, [report, {outdir,TargetDir}]),

    %% Compile a file using line_pt and verify that column numbers
    %% have been stripped.
    Big = filename:join(DataDir, "big"),
    {[],[_|_]} = compile_partition_warnings(Big, [{parse_transform,line_pt}]),

    %% Compile a file using column_pt and verify that column numbers
    %% have NOT been stripped.
    {[_|_],[]} = compile_partition_warnings(Big, [{parse_transform,column_pt}]),

    %% Compile a file using column_pt and error_location=line and verify
    %% that column numbers have been stripped.
    {[],[_|_]} = compile_partition_warnings(Big, [{error_location,line},
                                                  {parse_transform,column_pt}]),

    %% Compile a file using column_pt, line_pt and verify
    %% that column numbers have been stripped.
    {[],[_|_]} = compile_partition_warnings(Big, [{parse_transform,column_pt},
                                                  {parse_transform,line_pt}]),

    %% Compile a file using column_pt that adds columns and error_location=line and
    %% verify that column numbers have been stripped.
    {[],[_|_]} = compile_partition_warnings(Big, [{error_location,line},
                                                  add_columns,
                                                  {parse_transform,column_pt}]),

    %% Compile a file using column_pt that adds columns and error_location=line and
    %% then call column_pt again to check that columns are stripped in between calls.
    %% and then verify that column numbers have been stripped from output.
    {[],[_|_]} = compile_partition_warnings(Big, [{error_location,line},
                                                  add_columns,
                                                  {parse_transform,column_pt},
                                                  {parse_transform,column_pt}]),

    %% Compile a file using column_pt that adds columns and en error and error_location=line and
    %% verify that column numbers have been stripped.
    {error,[{_What,[{Line,_,_}]}],[_|_]} =
        compile_partition_warnings(Big, [{error_location,line},
                                         add_columns,
                                         add_error,
                                         {parse_transform,column_pt}]),
    true = is_integer(Line),

    %% Cover transform code implementing the `time` option.
    {ok,big,_} = compile:file(Big, [binary, time, report,
                                    {core_transform,generic_pt},
                                    {parse_transform,generic_pt}]),

    %% Test exceptions from a core transform.
    Simple = filename:join(DataDir, simple),
    error = compile:file(Simple, [report, {core_transform,generic_pt}, {action, crash}]),
    {error,_,[]} = compile:file(Simple, [return, {core_transform,generic_pt}, {action, crash}]),

    error = compile:file(Simple, [report, {core_transform,generic_pt}, {action, throw}]),
    {error,_,[]} = compile:file(Simple, [return, {core_transform,generic_pt}, {action, throw}]),

    error = compile:file(Simple, [report, {core_transform,generic_pt}, {action, exit}]),
    {error,_,[]} = compile:file(Simple, [return, {core_transform,generic_pt}, {action, exit}]),

    %% Test exceptions from a parse transform.
    error = compile:file(Simple, [report, {parse_transform,generic_pt}, {action, crash}]),
    {error,_,[]} = compile:file(Simple, [return, {parse_transform,generic_pt}, {action, crash}]),

    error = compile:file(Simple, [report, {parse_transform,generic_pt}, {action, throw}]),
    {error,_,[]} = compile:file(Simple, [return, {parse_transform,generic_pt}, {action, throw}]),

    error = compile:file(Simple, [report, {parse_transform,generic_pt}, {action, exit}]),
    {error,_,[]} = compile:file(Simple, [return, {parse_transform,generic_pt}, {action, exit}]),

    %% Test generating errors and warnings in a parse_transform.
    {ok,simple} = compile:file(Simple, [report, {parse_transform,generic_pt}, {action, warning}]),
    {ok,simple,[_|_]} = compile:file(Simple, [return, {parse_transform,generic_pt}, {action, warning}]),
    error = compile:file(Simple, [report, {parse_transform,generic_pt}, {action, error}]),
    {error,[_|_],[_|_]} = compile:file(Simple, [return, {parse_transform,generic_pt}, {action, error}]),
    error = compile:file(Simple, [report, {parse_transform,generic_pt}, {action, undefined_error}]),
    {error,[_|_],[]} = compile:file(Simple, [return, {parse_transform,generic_pt}, {action, undefined_error}]),

    ok.

compile_partition_warnings(Source, Opts) ->
    case compile:file(Source, [binary, return | Opts]) of
        {ok,big,<<_/binary>>,Ws0} ->
            [{_SourcePath,Ws}] = Ws0,

            %% Return {[ColumnWarning], [LineWarning]}.
            lists:partition(fun({{L,C},_,_}) when is_integer(L), is_integer(C) -> true;
                               ({L,_,_}) when is_integer(L) -> false
                            end, Ws);
        Error ->
            Error
    end.

%% Cover the erl_compile API used by erlc.
erl_compile_api(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Simple = filename:join(DataDir, "simple.erl"),

    Opts = #options{outdir=PrivDir},
    BinOpts = Opts#options{specific=[binary]},

    ok = compile:compile(Simple, "ignored", Opts),
    ok = compile:compile(Simple, "ignored", Opts#options{cwd=PrivDir,includes=[PrivDir]}),
    {ok,simple,_} = compile:compile(Simple, "ignored", BinOpts),

    ok = compile:compile(Simple, "ignored", Opts#options{specific=[dabstr]}),
    ok = compile:compile(Simple, "ignored", Opts#options{specific=[to_core]}),
    ok = compile:compile(Simple, "ignored", Opts#options{specific=[to_asm]}),

    SimpleAbstr = filename:join(PrivDir, "simple.abstr"),
    SimpleCore = filename:join(PrivDir, "simple.core"),
    SimpleAsm = filename:join(PrivDir, "simple.S"),

    ok = compile:compile_abstr(SimpleAbstr, "ignored", Opts),
    ok = compile:compile_core(SimpleCore, "ignored", Opts),
    ok = compile:compile_asm(SimpleAsm, "ignored", Opts),

    {ok,simple,<<_/binary>>} = compile:compile_abstr(SimpleAbstr, "ignored", BinOpts),
    {ok,simple,<<_/binary>>} = compile:compile_core(SimpleCore, "ignored", BinOpts),
    {ok,simple,<<_/binary>>} = compile:compile_asm(SimpleAsm, "ignored", BinOpts),

    NeedsDefines = filename:join(DataDir, "needs_defines.erl"),
    ok = compile:compile(NeedsDefines, "ignored", Opts#options{defines=[compile_this,{'TEST_RESULT',whatever}]}),

    ok = file:delete(SimpleAbstr),
    ok = file:delete(SimpleCore),
    ok = file:delete(SimpleAsm),
    ok = file:delete(filename:join(PrivDir, "simple.beam")),
    ok = file:delete(filename:join(PrivDir, "needs_defines.beam")),

    ok.

%% Check that an ssa dump contains the pretty printed types we expect.
%% The module we compile and dump, types_pp, is crafted so it contains
%% calls to functions which have the result types we want to check the
%% pretty printer for. We check all types except for bs_context,
%% bs_matchable and the interval form of float as the first two never
%% seem to appear in result types and the latter doesn't appear in any
%% module compiled by diffable.
types_pp(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    TargetDir = filename:join(PrivDir, types_pp),
    File = filename:join(DataDir, "types_pp.erl"),
    Listing = filename:join(TargetDir, "types_pp.ssaopt"),
    ok = file:make_dir(TargetDir),

    {ok,_} = compile:file(File, [dssaopt, {outdir, TargetDir}]),
    {ok, Data} = file:read_file(Listing),
    Lines = string:split(binary_to_list(Data), "\n", all),
    ResultTypes = get_result_types(Lines),
    io:format("Calls: ~p~n", [ResultTypes]),

    TypesToCheck = [{make_atom, "'an_atom'"},
                    {make_number, "number()"},
                    {make_float, "3.14"},
                    {make_integer, "17"},
                    {make_integer_range, "0..3"},
                    {make_nil, "nil()"},
                    {make_list, "list(any())"},
                    {make_list_of_ints, "list(integer())"},
                    {make_maybe_improper_list,
                     "maybe_improper_list(any(), any())"},
                    {make_nonempty_list, "nonempty_list(any())"},
                    {make_nonempty_improper_list,
                     "nonempty_improper_list(any(), ''end'')"},
                    {make_empty_map, "#{}"},
                    {make_map, "map()"},
                    {make_map_known_types, "#{integer()=>float()}"},
                    {make_fun_unknown_arity_known_type,
                     "fun((...) -> number())"},
                    {make_fun_known_arity_known_type,
                     "fun((_, _) -> number())"},
                    {make_fun_unknown_arity_unknown_type,
                     "fun()"},
                    {make_fun_known_arity_unknown_type,
                     "fun((_, _))"},
                    {make_unconstrained_tuple, "{...}"},
                    {make_known_size_tuple,
                     "{any(), any(), any(), any(), any()}"},
                    {make_inexact_tuple, "{any(), any(), any(), ...}"},
                    {make_union,
                     "'foo' | nonempty_list(1..3) | number(3, 7) |"
                     " {'tag0', 1, 2} | {'tag1', 3, 4} | bitstring(8)"},
                    {make_bitstring, "bitstring(8)"},
                    {make_none, "none()"}],
    lists:foreach(fun({FunName, Expected}) ->
                          Actual = map_get(atom_to_list(FunName), ResultTypes),
                          case Actual of
                              Expected ->
                                  ok;
                              _ ->
                                  ct:fail("Expected type of ~p is ~s, found ~s",
                                          [FunName, Expected, Actual])
                          end
                  end, TypesToCheck),
    ok = file:del_dir_r(TargetDir),
    ok.

%% Parsing for result types. Remember the last seen "Result type"
%% annotation and apply it to calls when we see them to a call when we
%% see them.
get_result_types(Lines) ->
    get_result_types(Lines, none, #{}).

get_result_types(["  %% Result type:"++_,"  %%    "++TypeLine|Lines], _, Acc) ->
    get_result_types(Lines, TypeLine, Acc);
get_result_types([Line|Lines], TypeLine, Acc0) ->
    Split = string:split(Line, "="),
    Acc = case Split of
              [_, " call" ++ Rest] ->
                  case string:split(Rest, "`", all) of
                      [_,Callee,_] ->
                          Acc0#{ Callee => TypeLine };
                      _ ->
                          Acc0
                  end;
              _ ->
                  Acc0
          end,
    get_result_types(Lines, TypeLine, Acc);
get_result_types([], _, Acc) ->
    Acc.

%% Check that the beam_ssa_type pass knows about bs_init_writable.
bs_init_writable(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    InFile = filename:join(DataDir, "bs_init_writable.erl"),
    OutDir = filename:join(PrivDir, "bs_init_writable"),
    OutFile = filename:join(OutDir, "bs_init_writable.S"),
    ok = file:make_dir(OutDir),
    {ok,bs_init_writable} = compile:file(InFile, ['S',{outdir,OutDir}]),
    {ok,Listing} = file:read_file(OutFile),
    Os = [global,multiline,{capture,all_but_first,list}],
    %% The is_bitstr test should be optimized away.
    nomatch = re:run(Listing, "({test,is_bitstr,.+})", Os),
    %% The is_bitstr test should be optimized away.
    nomatch = re:run(Listing, "({test,is_binary,.+})", Os),
    ok = file:del_dir_r(OutDir).


%% Check that an SSA listing contains pretty printed annotations, this
%% blindly checks that the expected annotation occurs the expected
%% number of times. Checking that the annotations are correctly placed
%% and contains the correct information is done in
%% beam_ssa_check_SUITE.
annotations_pp(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    TargetDir = filename:join(PrivDir, types_pp),
    File = filename:join(DataDir, "annotations_pp.erl"),
    Listing = filename:join(TargetDir, "annotations_pp.ssaopt"),
    ok = file:make_dir(TargetDir),

    {ok,_} = compile:file(File, [dssaopt, {outdir, TargetDir}]),
    {ok, Data} = file:read_file(Listing),
    Lines = string:split(binary_to_list(Data), "\n", all),

    ResultTypes = get_annotations("  %% Result type:", Lines),
    10 = length(ResultTypes),

    Uniques = get_annotations("  %% Unique:", Lines),
    10 = length(Uniques),

    Aliased = get_annotations("  %% Aliased:", Lines),
    13 = length(Aliased),

    ok = file:del_dir_r(TargetDir),
    ok.

get_annotations(Key, [Key,"  %%    "++Anno|Lines]) ->
    [Anno|get_annotations(Key, Lines)];
get_annotations(Key, [_|Lines]) ->
    get_annotations(Key, Lines);
get_annotations(_, []) ->
    [].

option_order(Config) ->
    Ts = [{spec1,
           ~"""
            -compile(nowarn_missing_spec).
            foo() -> ok.
            """,
           [],                                  %Environment
           [warn_missing_spec],
           []},
          {spec2,
           ~"""
            foo() -> ok.
            """,
           [{"ERL_COMPILER_OPTIONS", "warn_missing_spec"}],
           [nowarn_missing_spec],
           []},
          {spec3,
           ~"""
            -compile(nowarn_missing_spec).
            foo() -> ok.
            """,
           [{"ERL_COMPILER_OPTIONS", "nowarn_missing_spec"}],
           [warn_missing_spec],
           []},
          {spec4,
           ~"""
            -compile(warn_missing_spec).
            foo() -> ok.
            """,
           [{"ERL_COMPILER_OPTIONS", "nowarn_missing_spec"}],
           [],
           {warnings,[{{2,1},erl_lint,{missing_spec,{foo,0}}}]}
          },
          {spec5,
           ~"""
            -compile([warn_missing_spec,nowarn_missing_spec]).
            foo() -> ok.
            """,
           [{"ERL_COMPILER_OPTIONS", "nowarn_missing_spec"}],
           [warn_missing_spec],
           []},
          {records1,
           ~"""
            -record(r, {x,y}).
            rec_test(#r{x=X,y=Y}) -> X + Y.
            """,
           [],
           [strict_record_tests],
           fun(M) ->
                   try M:rec_test({r,1,2,3}) of
                       3 ->
                           fail()
                   catch
                       error:function_clause ->
                           ok
                   end
           end},
          {records2,
           ~"""
            -record(r, {x,y}).
            rec_test(R) -> R#r.x + R#r.y.
            """,
           [],
           [no_strict_record_tests],
           fun(M) ->
                   3 = M:rec_test({r,1,2,3}),
                   ok
           end},
          {records3,
           ~"""
            -compile(no_strict_record_tests).
            -record(r, {x,y}).
            rec_test(R) -> R#r.x + R#r.y.
            """,
           [],
           [strict_record_tests],
           fun(M) ->
                   3 = M:rec_test({r,1,2,3}),
                   ok
           end},
          {records4,
           ~"""
            -record(r, {x,y}).
            rec_test(#r{x=X,y=Y}) -> X + Y.
            """,
           [{"ERL_COMPILER_OPTIONS", "strict_record_tests"}],
           [],
           fun(M) ->
                   try M:rec_test({r,1,2,3}) of
                       3 ->
                           fail()
                   catch
                       error:function_clause ->
                           ok
                   end
           end},
          {records5,
           ~"""
            -record(r, {x,y}).
            rec_test(R) -> R#r.x + R#r.y.
            """,
           [{"ERL_COMPILER_OPTIONS", "strict_record_tests"}],
           [no_strict_record_tests],
           fun(M) ->
                   3 = M:rec_test({r,1,2,3}),
                   ok
           end},
          {records6,
           ~"""
            -compile(no_strict_record_tests).
            -record(r, {x,y}).
            rec_test(R) -> R#r.x + R#r.y.
            """,
           [{"ERL_COMPILER_OPTIONS", "strict_record_tests"}],
           [],
           fun(M) ->
                   3 = M:rec_test({r,1,2,3}),
                   ok
           end},
          {records7,
           ~"""
            -record(r, {x,y}).
            rec_test(R) -> R#r.x + R#r.y.
            """,
           [{"ERL_COMPILER_OPTIONS", "no_strict_record_tests"}],
           [no_strict_record_tests, strict_record_tests],
           fun(M) ->
                   try M:rec_test({r,1,2,3}) of
                       3 ->
                           fail()
                   catch
                       error:{badrecord,{r,1,2,3}} ->
                           ok
                   end
           end}

         ],
    run(Config, Ts),
    ok.

sys_coverage(Config) ->
    DataDir = proplists:get_value(data_dir, Config),

    sys_coverage_1(DataDir),
    sys_coverage_2(DataDir),

    ok.

%% Make sure that the `line_coverage` option will not change line
%% numbers in exceptions.
sys_coverage_1(DataDir) ->
    Mod = exceptions,
    Source = filename:join(DataDir, "exceptions"),
    {ok,Mod,Code} = compile:file(Source, [line_coverage,binary,report]),
    {module,Mod} = code:load_binary(Mod, "", Code),

    Mod:Mod(DataDir),

    true = code:delete(Mod),
    false = code:purge(Mod),

    ok.

%% Make sure that the `line_coverage` option given in the `compile`
%% attribute in a module works.
sys_coverage_2(DataDir) ->
    Mod = embedded_line_coverage,
    Source = filename:join(DataDir, "embedded_line_coverage"),
    {ok,Mod,Asm} = compile:file(Source, ['S',binary,report]),

    {Mod,_,_,Fs,_} = Asm,
    [{function,add,2,_,Is}|_] = Fs,
    true = lists:keymember(executable_line, 1, Is),

    ok.

%%%
%%% Utilities.
%%%

compile_and_verify(Name, Target, Opts) ->
    Mod = list_to_atom(filename:basename(Name, ".erl")),
    {ok,Mod} = compile:file(Name, Opts),
    {ok,{Mod,[{compile_info,CInfo}]}} = 
	beam_lib:chunks(Target, [compile_info]),
    {options,BeamOpts} = lists:keyfind(options, 1, CInfo),
    Opts = BeamOpts.

get_unique_beam_files() ->
    test_lib:get_unique_files(".beam").

%% Compiles a test module and returns the list of errors and warnings.

run(Config, Tests) ->
    F = fun({N,P,Env,Ws,Run}, _BadL) when is_function(Run, 1) ->
                case catch run_test(Config, P, Env, Ws, Run) of
                    ok ->
                        ok;
                    Bad ->
                        io:format("~nTest ~p failed. Expected~n  ~p~n"
                                  "but got~n  ~p~n", [N, ok, Bad]),
                        fail()
                end;
           ({N,P,Env,Ws,Expected}, BadL)
              when is_list(Expected); is_tuple(Expected) ->
                io:format("### ~s\n", [N]),
                case catch run_test(Config, P, Env, Ws, none) of
                    Expected ->
                        BadL;
                    Bad ->
                        io:format("~nTest ~p failed. Expected~n  ~p~n"
                                  "but got~n  ~p~n", [N, Expected, Bad]),
			fail()
                end
        end,
    lists:foldl(F, [], Tests).

run_test(Conf, Test0, Env, Options, Run) ->
    run_test_putenv(Env),
    Module = "warnings" ++ test_lib:uniq(),
    Filename = Module ++ ".erl",
    DataDir = proplists:get_value(priv_dir, Conf),
    Test1 = ["-module(", Module, "). -file( \"", Filename, "\", 1). ", Test0],
    Test = iolist_to_binary(Test1),
    File = filename:join(DataDir, Filename),
    Opts = [binary,export_all,return|Options],
    ok = file:write_file(File, Test),

    %% Compile once just to print all warnings (and cover more code).
    _ = compile:file(File, [binary,export_all,report|Options]),

    %% Test result of compilation.
    {ok, Mod, Beam, Warnings} = compile:file(File, Opts),
    _ = file:delete(File),

    if
        is_function(Run, 1) ->
            {module,Mod} = code:load_binary(Mod, "", Beam),
            ok = Run(Mod),
            run_test_unsetenv(Env),
            true = code:delete(Mod),
            _ = code:purge(Mod),
            ok;
        Run =:= none ->
            run_test_unsetenv(Env),
            Res = get_warnings(Warnings),
            case Res of
                [] ->
                    [];
                {warnings, Ws} ->
                    print_warnings(Ws, Test),
                    Res
            end
    end.

run_test_putenv(Env) ->
    _ = [_ = os:putenv(Name, Value) || {Name,Value} <- Env],
    ok.

run_test_unsetenv(Env) ->
    _ = [_ = os:unsetenv(Name) || {Name,_Value} <- Env],
    ok.

get_warnings([]) ->
    [];
get_warnings(WsL) ->
    case WsL of
        [{_File,Ws}] -> {warnings, Ws};
        _ -> {warnings, WsL}
    end.

print_warnings(Warnings, Source) ->
    Lines = binary:split(Source, <<"\n">>, [global]),
    Cs = [print_warning(W, Lines) || W <- Warnings],
    io:put_chars(Cs),
    ok.

print_warning({{LineNum,Column},Mod,Data}, Lines) ->
    Line0 = lists:nth(LineNum, Lines),
    <<Line1:(Column-1)/binary,_/binary>> = Line0,
    Spaces = re:replace(Line1, <<"[^\t]">>, <<" ">>, [global]),
    CaretLine = [Spaces,"^"],
    [io_lib:format("~p:~p: ~ts\n",
                   [LineNum,Column,Mod:format_error(Data)]),
     Line0, "\n",
     CaretLine, "\n\n"];
print_warning(_, _) ->
    [].

fail() ->
    ct:fail(failed).
