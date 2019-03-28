%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
	 app_test/1,appup_test/1,
	 debug_info/4, custom_debug_info/1, custom_compile_info/1,
	 file_1/1, forms_2/1, module_mismatch/1, outdir/1,
	 binary/1, makedep/1, cond_and_ifdef/1, listings/1, listings_big/1,
	 other_output/1, kernel_listing/1, encrypted_abstr/1,
	 strict_record/1, utf8_atoms/1, utf8_functions/1, extra_chunks/1,
	 cover/1, env/1, core_pp/1, tuple_calls/1,
	 core_roundtrip/1, asm/1,
	 sys_pre_attributes/1, dialyzer/1,
	 warnings/1, pre_load_check/1, env_compiler_options/1,
         bc_options/1, deterministic_include/1, deterministic_paths/1
	]).

suite() -> [{ct_hooks,[ts_install_cth]}].

%% To cover the stripping of 'type' and 'spec' in beam_asm.
-type all_return_type() :: [atom()].
-spec all() -> all_return_type().

all() -> 
    [app_test, appup_test, file_1, forms_2, module_mismatch, outdir,
     binary, makedep, cond_and_ifdef, listings, listings_big,
     other_output, kernel_listing, encrypted_abstr, tuple_calls,
     strict_record, utf8_atoms, utf8_functions, extra_chunks,
     cover, env, core_pp, core_roundtrip, asm,
     sys_pre_attributes, dialyzer, warnings, pre_load_check,
     env_compiler_options, custom_debug_info, bc_options,
     custom_compile_info, deterministic_include, deterministic_paths].

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

%% Tests that we can compile and run a simple Erlang program,
%% using compile:file/1.

file_1(Config) when is_list(Config) ->
    process_flag(trap_exit, true),

    {Simple, Target} = get_files(Config, simple, "file_1"),
    {ok, Cwd} = file:get_cwd(),
    ok = file:set_cwd(filename:dirname(Target)),

    %% Native from BEAM without compilation info.
    {ok,simple} = compile:file(Simple, [slim]), %Smoke test only.
    {ok,simple} = compile:file(Target, [native,from_beam]), %Smoke test.

    %% Native from BEAM with compilation info.
    {ok,simple} = compile:file(Simple),	%Smoke test only.
    {ok,simple} = compile:file(Target, [native,from_beam]), %Smoke test.

    {ok,simple} = compile:file(Simple, [native,report]), %Smoke test.

    compile_and_verify(Simple, Target, []),
    compile_and_verify(Simple, Target, [native]),
    compile_and_verify(Simple, Target, [debug_info]),
    compile_and_verify(Simple, Target, [no_postopt]),
    {ok,simple} = compile:file(Simple, [no_line_info]), %Coverage

    {ok,simple} = compile:file(Simple, [{eprof,beam_z}]), %Coverage


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
    Src = "/foo/bar",
    AbsSrc = filename:absname(Src),
    Anno = erl_anno:new(1),
    SimpleCode = [{attribute,Anno,module,simple}],
    {ok,simple,Bin1} = compile:forms(SimpleCode, [binary,{source,Src}]),

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
    forms_compile_and_load(Core, [from_core,native]),

    {ok,simple,Asm} = compile:forms(SimpleCode, [to_asm,binary]),
    forms_compile_and_load(Asm, [from_asm]),
    forms_compile_and_load(Asm, [from_asm,native]),

    {ok,simple,Beam} = compile:forms(SimpleCode, []),
    forms_compile_and_load(Beam, [from_beam]),
    forms_compile_and_load(Beam, [from_beam,native]),

    %% Cover the error handling code.
    error = compile:forms(bad_core, [from_core,report]),
    error = compile:forms(bad_asm, [from_asm,report]),
    error = compile:forms(<<"bad_beam">>, [from_beam,report]),
    error = compile:forms(<<"bad_beam">>, [from_beam,native,report]),

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
    {Simple,Target} = get_files(Config, simple, "makedep"),
    DataDir = proplists:get_value(data_dir, Config),
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

    ok = file:delete(Target),
    ok = file:del_dir(filename:dirname(Target)),
    ok.

makedep_canonicalize_result(Mf, DataDir) ->
    Mf0 = binary_to_list(Mf),
    %% Replace the Datadir by "$(srcdir)".
    Mf1 = re:replace(Mf0, DataDir, "$(srcdir)/",
      [global,multiline,{return,list}]),
    %% Long lines are splitted, put back everything on one line.
    Mf2 = re:replace(Mf1, "\\\\\n  ", "", [global,multiline,{return,list}]),
    list_to_binary(Mf2).

makedep_modify_target(Mf, Target) ->
    Mf0 = binary_to_list(Mf),
    Mf1 = re:replace(Mf0, Target, "$target", [{return,list}]),
    list_to_binary(Mf1).

%% Tests that conditional compilation, defining values, including files work.

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
            {dkern, ".kernel"},
            {dssa, ".ssa"},
            {dssaopt, ".ssaopt"},
            {dprecg, ".precodegen"},
            {dcg, ".codegen"},
            {dblk, ".block"},
            {dexcept, ".except"},
            {djmp, ".jump"},
            {dclean, ".clean"},
            {dpeep, ".peep"},
            {dopt, ".optimize"},
            {diffable, ".S"}],
    p_listings(List, Simple, TargetDir),

    %% Test options that produce a listing file if 'binary' is not given.
    do_listing(Simple, TargetDir, to_pp, ".P"),
    do_listing(Simple, TargetDir, to_exp, ".E"),
    do_listing(Simple, TargetDir, to_core0, ".core"),
    Listings = filename:join(PrivDir, listings),
    ok = file:delete(filename:join(Listings, File ++ ".core")),
    do_listing(Simple, TargetDir, to_core, ".core"),
    do_listing(Simple, TargetDir, to_kernel, ".kernel"),
    do_listing(Simple, TargetDir, to_dis, ".dis"),

    %% Final clean up.
    lists:foreach(fun(F) -> ok = file:delete(F) end,
	filelib:wildcard(filename:join(Listings, "*"))),
    ok = file:del_dir(Listings),

    do_file_listings(DataDir,PrivDir,Files).

listings_big(Config) when is_list(Config) ->
    {Big,Target} = get_files(Config, big, listings_big),
    TargetDir = filename:dirname(Target),
    List = [{'S',".S"},
            {'E',".E"},
            {'P',".P"},
            {dkern, ".kernel"},
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

    io:put_chars("to_kernel (file)"),
    {ok,simple,Kernel} = compile:file(Simple, [to_kernel,binary,time]),
    k_mdef = element(1, Kernel),
    io:put_chars("to_kernel (forms)"),
    {ok,simple,Kernel} = compile:forms(PP, [to_kernel,binary,time]),

    io:put_chars("to_asm (file)"),
    {ok,simple,Asm} = compile:file(Simple, [to_asm,binary,time]),
    {simple,_,_,_,_} = Asm,
    io:put_chars("to_asm (forms)"),
    {ok,simple,Asm} = compile:forms(PP, [to_asm,binary,time]),

    ok.

%% Smoke test and cover of pretty-printing of Kernel code.
kernel_listing(_Config) ->
    TestBeams = get_unique_beam_files(),
    Abstr = [begin {ok,{Mod,[{abstract_code,
			      {raw_abstract_v1,Abstr}}]}} =
		       beam_lib:chunks(Beam, [abstract_code]),
		   {Mod,Abstr} end || Beam <- TestBeams],
    test_lib:p_run(fun(F) -> do_kernel_listing(F) end, Abstr).

do_kernel_listing({M,A}) ->
    try
	{ok,M,Kern} = compile:forms(A, [to_kernel]),
	IoList = v3_kernel_pp:format(Kern),
	case unicode:characters_to_binary(IoList) of
	    Bin when is_binary(Bin) ->
		ok
	end
    catch
	throw:{error,Error} ->
	    io:format("*** compilation failure '~p' for module ~s\n",
		      [Error,M]),
	    error;
	Class:Error:Stk ->
	    io:format("~p: ~p ~p\n~p\n", [M,Class,Error,Stk]),
	    error
    end.

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
		      (catch crypto:stop()),
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
		  ok = file:del_dir(filename:dirname(Target))
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

    %% Place the crypto key in .erlang.crypt.
    beam_lib:clear_crypto_key_fun(),
    {ok,OldCwd} = file:get_cwd(),
    ok = file:set_cwd(TargetDir),

    error = compile:file(Simple, [encrypt_debug_info,report]),

    NewKey = "better use another key here",
    write_crypt_file(["[{debug_info,des3_cbc,simple,\"",NewKey,"\"}].\n"]),
    {ok,simple} = compile:file(Simple, [encrypt_debug_info,report]),
    verify_abstract("simple.beam", erl_abstract_code),
    ok = file:delete(".erlang.crypt"),
    beam_lib:clear_crypto_key_fun(),
    {error,beam_lib,{key_missing_or_invalid,"simple.beam",abstract_code}} =
	beam_lib:chunks("simple.beam", [abstract_code]),
    ok = file:set_cwd(OldCwd),

    %% Test key compatibility by reading a BEAM file produced before
    %% the update to the new crypto functions.
    install_crypto_key("an old key"),
    KeyCompat = filename:join(filename:dirname(Simple),
			      "key_compatibility"),
    {ok,{key_compatibility,[Chunk]}} = beam_lib:chunks(KeyCompat,
						       [abstract_code]),
    {abstract_code,{raw_abstract_v1,_}} = Chunk,

    ok.


write_crypt_file(Contents0) ->
    Contents = list_to_binary([Contents0]),
    io:format("~s\n", [binary_to_list(Contents)]),
    ok = file:write_file(".erlang.crypt", Contents).

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
	crypto:start(),
	crypto:stop(),
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

%% Miscellanous tests, mainly to get better coverage.
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

    %% The option first given wins.
    {ok,M} = c:c(M, [no_strict_record_tests,strict_record_tests|Opts]),
    Turtle = test_sloppy(),
    {ok,M} = c:c(M, [strict_record_tests,no_strict_record_tests|Opts]),
    Turtle = test_strict(),

    %% Default (possibly influenced by ERL_COMPILER_OPTIONS).
    {ok,M} = c:c(M, [{outdir,Priv},report_errors]),
    try
	      {1,2} = record_access:test(Turtle),
	      {comment,"Default: no_strict_record_tests"}
	  catch
	      error:{badrecord,tortoise} ->
		  {comment,"Default: strict_record_tests"}
	  end.

test_strict() ->
    Turtle = record_access:turtle(),
    try
	      record_access:test(Turtle)
	  catch
	      error:{badrecord,tortoise} ->
		  ok
	  end,
    Turtle.

test_sloppy() ->
    Turtle = record_access:turtle(),
    {1,2} = record_access:test(Turtle),
    Turtle.

utf8_atoms(Config) when is_list(Config) ->
    Anno = erl_anno:new(1),
    Atom = binary_to_atom(<<"こんにちは"/utf8>>, utf8),
    Forms = [{attribute,Anno,compile,[export_all]},
	     {function,Anno,atom,0,[{clause,Anno,[],[],[{atom,Anno,Atom}]}]}],

    Utf8AtomForms = [{attribute,Anno,module,utf8_atom}|Forms],
    {ok,utf8_atom,Utf8AtomBin} =
	compile:forms(Utf8AtomForms, [binary]),
    {ok,{utf8_atom,[{atoms,_}]}} =
	beam_lib:chunks(Utf8AtomBin, [atoms]),
    code:load_binary(utf8_atom, "compile_SUITE", Utf8AtomBin),
    Atom = utf8_atom:atom(),

    NoUtf8AtomForms = [{attribute,Anno,module,no_utf8_atom}|Forms],
    error = compile:forms(NoUtf8AtomForms, [binary, r19]).

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
	      [{clause,Anno,[{var,[],mod}],[],
	       [{call,[],{remote,[],{var,[],mod},{atom,[],size}},[]}]}]},
	     {function,Anno,store,1,
	      [{clause,Anno,[{var,[],mod}],[],
	       [{call,[],{remote,[],{var,[],mod},{atom,[],store}},[{atom,[],key},{atom,[],value}]}]}]}],

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
    %% on the Core Erlang optmimization passes.
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
    test_lib:p_run(fun(F) -> do_core_roundtrip(F, Outdir) end, TestBeams).

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
	{ok,M,Asm} = compile:forms(A, ['S']),
	AsmFile = filename:join(Outdir, atom_to_list(M)++".S"),
	{ok,Fd} = file:open(AsmFile, [write,{encoding,utf8}]),
	beam_listing:module(Fd, Asm),
	ok = file:close(Fd),
	case compile:file(AsmFile, [from_asm,binary,report]) of
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


%% Test that warnings contain filenames and line numbers.
warnings(_Config) ->
    Files = get_unique_files(".erl"),
    test_lib:p_run(fun do_warnings/1, Files).

do_warnings(F) ->
    {ok,_,_,Ws} = compile:file(F, [binary,bin_opt_info,return]),
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

do_warnings_2([{Int,_,_}=W|T], Next, F) ->
    if
	is_integer(Int) ->
	    do_warnings_2(T, Next, F);
	true ->
	    io:format("~s:\nMissing line number: ~p\n",
		      [F,W]),
	    error
    end;
do_warnings_2([], Next, F) ->
    do_warnings_1(Next, F).


%% Test that the compile:pre_load/0 function (used by 'erlc')
%% pre-loads the modules that are used by a typical compilation.

pre_load_check(Config) ->
    case {test_server:is_cover(),code:module_info(native)} of
	{true,_} ->
	    {skip,"Cover is running"};
        {false,true} ->
            %% Tracing won't work.
            {skip,"'code' is native-compiled"};
	{false,false} ->
	    try
		do_pre_load_check(Config)
	    after
		dbg:stop_clear()
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
	    ?t:fail(compile);
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
	    ?t:fail({not_preload,NotPreLoaded})
    end,

    %% Check for modules that should not be pre-loaded.
    case ordsets:subtract(PreLoaded, WasLoaded) of
	[] ->
	    ok;
	[_|_]=NotUsed ->
	    io:format("The following modules were pre-loaded"
		      " but not used:\n~p\n",
		      [NotUsed]),
	    ?t:fail({not_used,NotUsed})
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

    L = [{101, small_float, [no_get_hd_tl,no_line_info]},
         {103, big, [no_put_tuple2,no_get_hd_tl,no_ssa_opt_record,
                     no_line_info,no_stack_trimming]},
         {125, small_float, [no_get_hd_tl,no_line_info,no_ssa_opt_float]},

         {132, small, [no_put_tuple2,no_get_hd_tl,no_ssa_opt_record,
                       no_ssa_opt_float,no_line_info,no_bsm3]},

         {153, small, [r20]},
         {153, small, [r21]},

         {136, big, [no_put_tuple2,no_get_hd_tl,
                     no_ssa_opt_record,no_line_info]},

         {153, big, [no_put_tuple2,no_get_hd_tl, no_ssa_opt_record]},
         {153, big, [r16]},
         {153, big, [r17]},
         {153, big, [r18]},
         {153, big, [r19]},
         {153, small_float, [r16]},
         {153, small_float, []},

         {158, small_maps, [r17]},
         {158, small_maps, [r18]},
         {158, small_maps, [r19]},
         {158, small_maps, [r20]},
         {158, small_maps, [r21]},

         {164, small_maps, []},
         {164, big, []}
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
    {ok,Mod,Beam} = compile:file(Src, [binary|Opt]),
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
    get_unique_files(".beam").

get_unique_files(Ext) ->
    Wc = filename:join(filename:dirname(code:which(?MODULE)), "*"++Ext),
    [F || F <- filelib:wildcard(Wc),
	  not is_cloned(F, Ext), not is_lfe_module(F, Ext)].

is_cloned(File, Ext) ->
    Mod = list_to_atom(filename:basename(File, Ext)),
    test_lib:is_cloned_mod(Mod).

is_lfe_module(File, Ext) ->
    case filename:basename(File, Ext) of
	"lfe_" ++ _ -> true;
	_ -> false
    end.
