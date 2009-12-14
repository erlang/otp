%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(compile_SUITE).

%% Tests compile:file/1 and compile:file/2 with various options.

-include("test_server.hrl").

-export([all/1,
	 app_test/1,
	 file_1/1, module_mismatch/1, big_file/1, outdir/1, 
	 binary/1, makedep/1, cond_and_ifdef/1, listings/1, listings_big/1,
	 other_output/1, package_forms/1, encrypted_abstr/1,
	 bad_record_use/1, bad_record_use1/1, bad_record_use2/1, strict_record/1,
	 missing_testheap/1, cover/1, env/1, core/1, asm/1]).

-export([init/3]).


%% To cover the stripping of 'type' and 'spec' in beam_asm.
-type all_return_type() :: [atom()].
-spec all('suite' | [_]) -> all_return_type().

all(suite) ->
    test_lib:recompile(?MODULE),
    [app_test,
     file_1, module_mismatch, big_file, outdir, binary, makedep,
     cond_and_ifdef, listings, listings_big,
     other_output, package_forms,
     encrypted_abstr,
     bad_record_use, strict_record,
     missing_testheap, cover, env, core, asm].


%% Test that the Application file has no `basic' errors.";
app_test(Config) when is_list(Config) ->
    ?line ?t:app_test(compiler).

%% Tests that we can compile and run a simple Erlang program,
%% using compile:file/1.

file_1(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(5)),
    ?line {Simple, Target} = files(Config, "file_1"),
    ?line {ok, Cwd} = file:get_cwd(),
    ?line ok = file:set_cwd(filename:dirname(Target)),
    ?line {ok,simple} = compile:file(Simple),	%Smoke test only.
    ?line {ok,simple} = compile:file(Simple, [slim]), %Smoke test only.
    ?line {ok,simple} = compile:file(Simple, [native,report]), %Smoke test.
    ?line {ok,simple} = compile:file(Target, [native,from_beam]), %Smoke test.
    ?line {ok,simple} = compile:file(Simple, [debug_info]),
    ?line ok = file:set_cwd(Cwd),
    ?line true = exists(Target),
    ?line passed = run(Target, test, []),

    %% Cleanup.
    ?line ok = file:delete(Target),
    ?line ok = file:del_dir(filename:dirname(Target)),
    ?line test_server:timetrap_cancel(Dog),
    ok.

module_mismatch(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line File = filename:join(DataDir, "wrong_module_name.erl"),
    ?line {error,[{"wrong_module_name.beam",
		   [{compile,{module_name,arne,"wrong_module_name"}}]}],
	   []} = compile:file(File, [return]),
    ?line error = compile:file(File, [report]),

    ?line {ok,arne,[]} = compile:file(File,
				      [return,no_error_module_mismatch]),

    ok.

big_file(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(5)),
    ?line DataDir = ?config(data_dir, Config),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Big = filename:join(DataDir, "big.erl"),
    ?line Target = filename:join(PrivDir, "big.beam"),
    ?line ok = file:set_cwd(PrivDir),
    ?line {ok,big} = compile:file(Big, []),
    ?line {ok,big} = compile:file(Big, [r9,debug_info]),
    ?line {ok,big} = compile:file(Big, [no_postopt]),
    ?line true = exists(Target),

    %% Cleanup.
    ?line ok = file:delete(Target),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%% Tests that the {outdir, Dir} option works.

outdir(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(60)),
    ?line {Simple, Target} = files(Config, "outdir"),
    ?line {ok, simple} = compile:file(Simple, [{outdir, filename:dirname(Target)}]),
    ?line true = exists(Target),
    ?line passed = run(Target, test, []),
    ?line ok = file:delete(Target),
    ?line ok = file:del_dir(filename:dirname(Target)),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%% Tests that the binary option works.

binary(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(60)),
    ?line {Simple, Target} = files(Config, "binary"),
    ?line {ok, simple, Binary} = compile:file(Simple, [binary]),
    ?line code:load_binary(simple, Target, Binary),
    ?line passed = simple:test(),
    ?line true = code:delete(simple),
    ?line false = code:purge(simple),
    ?line ok = file:del_dir(filename:dirname(Target)),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%% Tests that the dependencies-Makefile-related options work.

makedep(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(60)),
    ?line {Simple,Target} = files(Config, "makedep"),
    ?line DataDir = ?config(data_dir, Config),
    ?line SimpleRootname = filename:rootname(Simple),
    ?line IncludeDir = filename:join(filename:dirname(Simple), "include"),
    ?line IncludeOptions = [
      {d,need_foo},
      {d,foo_value,42},
      {d,include_generated},
      {i,IncludeDir}
    ],
    %% Basic rule.
    ?line BasicMf1Name = SimpleRootname ++ "-basic1.mk",
    ?line {ok,BasicMf1} = file:read_file(BasicMf1Name),
    ?line {ok,_,Mf1} = compile:file(Simple, [binary,makedep]),
    ?line BasicMf1 = makedep_canonicalize_result(Mf1, DataDir),
    %% Basic rule with one existing header.
    ?line BasicMf2Name = SimpleRootname ++ "-basic2.mk",
    ?line {ok,BasicMf2} = file:read_file(BasicMf2Name),
    ?line {ok,_,Mf2} = compile:file(Simple, [binary,makedep|IncludeOptions]),
    ?line BasicMf2 = makedep_canonicalize_result(Mf2, DataDir),
    %% Rule with one existing header and one missing header.
    ?line MissingMfName = SimpleRootname ++ "-missing.mk",
    ?line {ok,MissingMf} = file:read_file(MissingMfName),
    ?line {ok,_,Mf3} = compile:file(Simple,
      [binary,makedep,makedep_add_missing|IncludeOptions]),
    ?line MissingMf = makedep_canonicalize_result(Mf3, DataDir),
    %% Rule with modified target.
    ?line TargetMf1Name = SimpleRootname ++ "-target1.mk",
    ?line {ok,TargetMf1} = file:read_file(TargetMf1Name),
    ?line {ok,_,Mf4} = compile:file(Simple,
      [binary,makedep,{makedep_target,"$target"}|IncludeOptions]),
    ?line TargetMf1 = makedep_modify_target(
      makedep_canonicalize_result(Mf4, DataDir), "$$target"),
    %% Rule with quoted modified target.
    ?line TargetMf2Name = SimpleRootname ++ "-target2.mk",
    ?line {ok,TargetMf2} = file:read_file(TargetMf2Name),
    ?line {ok,_,Mf5} = compile:file(Simple,
      [binary,makedep,{makedep_target,"$target"},makedep_quote_target|
        IncludeOptions]),
    ?line TargetMf2 = makedep_modify_target(
      makedep_canonicalize_result(Mf5, DataDir), "$$target"),
    %% Basic rule written to some file.
    ?line {ok,_} = compile:file(Simple,
      [makedep,{makedep_output,Target}|IncludeOptions]),
    ?line {ok,Mf6} = file:read_file(Target),
    ?line BasicMf2 = makedep_canonicalize_result(Mf6, DataDir),

    ?line ok = file:delete(Target),
    ?line ok = file:del_dir(filename:dirname(Target)),
    ?line test_server:timetrap_cancel(Dog),
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
    ?line Dog = test_server:timetrap(test_server:seconds(60)),
    ?line {Simple, Target} = files(Config, "cond_and_ifdef"),
    ?line IncludeDir = filename:join(filename:dirname(Simple), "include"),
    ?line Options = [{outdir, filename:dirname(Target)},
		     {d, need_foo}, {d, foo_value, 42},
		     {i, IncludeDir}, report],
    ?line {ok, simple} = compile:file(Simple, Options),
    ?line true = exists(Target),
    ?line {hiker, 42} = run(Target, foo, []),
    ?line ok = file:delete(Target),
    ?line ok = file:del_dir(filename:dirname(Target)),
    ?line test_server:timetrap_cancel(Dog),
    ok.

listings(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(8)),
    ?line DataDir = ?config(data_dir, Config),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Simple = filename:join(DataDir, simple),
    ?line TargetDir = filename:join(PrivDir, listings),
    ?line ok = file:make_dir(TargetDir),

    %% Test all dedicated listing options.
    ?line do_listing(Simple, TargetDir, 'S'),
    ?line do_listing(Simple, TargetDir, 'E'),
    ?line do_listing(Simple, TargetDir, 'P'),
    ?line do_listing(Simple, TargetDir, dpp, ".pp"),
    ?line do_listing(Simple, TargetDir, dabstr, ".abstr"),
    ?line do_listing(Simple, TargetDir, dexp, ".expand"),
    ?line do_listing(Simple, TargetDir, dcore, ".core"),
    ?line do_listing(Simple, TargetDir, doldinline, ".oldinline"),
    ?line do_listing(Simple, TargetDir, dinline, ".inline"),
    ?line do_listing(Simple, TargetDir, dcore, ".core"),
    ?line do_listing(Simple, TargetDir, dcopt, ".copt"),
    ?line do_listing(Simple, TargetDir, dsetel, ".dsetel"),
    ?line do_listing(Simple, TargetDir, dkern, ".kernel"),
    ?line do_listing(Simple, TargetDir, dlife, ".life"),
    ?line do_listing(Simple, TargetDir, dcg, ".codegen"),
    ?line do_listing(Simple, TargetDir, dblk, ".block"),
    ?line do_listing(Simple, TargetDir, dbool, ".bool"),
    ?line do_listing(Simple, TargetDir, dtype, ".type"),
    ?line do_listing(Simple, TargetDir, ddead, ".dead"),
    ?line do_listing(Simple, TargetDir, djmp, ".jump"),
    ?line do_listing(Simple, TargetDir, dclean, ".clean"),
    ?line do_listing(Simple, TargetDir, dpeep, ".peep"),
    ?line do_listing(Simple, TargetDir, dopt, ".optimize"),

    %% First clean up.
    ?line Listings = filename:join(PrivDir, listings),
    ?line lists:foreach(fun(F) -> ok = file:delete(F) end,
			filelib:wildcard(filename:join(Listings, "*"))),

    %% Test options that produce a listing file if 'binary' is not given.
    ?line do_listing(Simple, TargetDir, to_pp, ".P"),
    ?line do_listing(Simple, TargetDir, to_exp, ".E"),
    ?line do_listing(Simple, TargetDir, to_core0, ".core"),
    ?line ok = file:delete(filename:join(Listings, "simple.core")),
    ?line do_listing(Simple, TargetDir, to_core, ".core"),
    ?line do_listing(Simple, TargetDir, to_kernel, ".kernel"),

    %% Final clean up.
    ?line lists:foreach(fun(F) -> ok = file:delete(F) end,
			filelib:wildcard(filename:join(Listings, "*"))),
    ?line ok = file:del_dir(Listings),
    ?line test_server:timetrap_cancel(Dog),
    ok.

listings_big(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(10)),
    ?line DataDir = ?config(data_dir, Config),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Big = filename:join(DataDir, big),
    ?line TargetDir = filename:join(PrivDir, listings_big),
    ?line ok = file:make_dir(TargetDir),
    ?line do_listing(Big, TargetDir, 'S'),
    ?line do_listing(Big, TargetDir, 'E'),
    ?line do_listing(Big, TargetDir, 'P'),
    ?line do_listing(Big, TargetDir, dkern, ".kernel"),

    ?line Target = filename:join(TargetDir, big),
    ?line {ok,big} = compile:file(Target, [asm,{outdir,TargetDir}]),

    %% Cleanup.
    ?line ok = file:delete(Target ++ ".beam"),
    ?line lists:foreach(fun(F) -> ok = file:delete(F) end,
			filelib:wildcard(filename:join(TargetDir, "*"))),
    ?line ok = file:del_dir(TargetDir),
    ?line test_server:timetrap_cancel(Dog),
    ok.

other_output(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(8)),
    ?line DataDir = ?config(data_dir, Config),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Simple = filename:join(DataDir, simple),
    ?line TargetDir = filename:join(PrivDir, other_output),
    ?line ok = file:make_dir(TargetDir),

    io:put_chars("to_pp"),
    ?line {ok,[],PP} = compile:file(Simple, [to_pp,binary,time]),
    ?line [] = [E || E <- PP,
		     begin
			 case element(1, E) of
			     attribute -> false;
			     function -> false;
			     eof -> false
			 end
		     end],

    io:put_chars("to_exp (file)"),
    ?line {ok,simple,Expand} = compile:file(Simple, [to_exp,binary,time]),
    ?line case Expand of
	      {simple,Exports,Forms} when is_list(Exports), is_list(Forms) -> ok
	  end,
    io:put_chars("to_exp (forms)"),
    ?line {ok,simple,Expand} = compile:forms(PP, [to_exp,binary,time]),

    io:put_chars("to_core (file)"),
    ?line {ok,simple,Core} = compile:file(Simple, [to_core,binary,time]),
    ?line c_module = element(1, Core),
    ?line {ok,_} = core_lint:module(Core),
    io:put_chars("to_core (forms)"),
    ?line {ok,simple,Core} = compile:forms(PP, [to_core,binary,time]),

    io:put_chars("to_kernel (file)"),
    ?line {ok,simple,Kernel} = compile:file(Simple, [to_kernel,binary,time]),
    ?line k_mdef = element(1, Kernel),
    io:put_chars("to_kernel (forms)"),
    ?line {ok,simple,Kernel} = compile:forms(PP, [to_kernel,binary,time]),

    io:put_chars("to_asm (file)"),
    ?line {ok,simple,Asm} = compile:file(Simple, [to_asm,binary,time]),
    ?line {simple,_,_,_,_} = Asm,
    io:put_chars("to_asm (forms)"),
    ?line {ok,simple,Asm} = compile:forms(PP, [to_asm,binary,time]),

    ?line test_server:timetrap_cancel(Dog),
    ok.

package_forms(Config) when is_list(Config) ->
    Fs = [{attribute,1,file,{"./p.erl",1}},
	  {attribute,1,module,[p,p]},
	  {attribute,3,compile,export_all},
	  {attribute,1,file,
	   {"/clearcase/otp/erts/lib/stdlib/include/qlc.hrl",1}},
	  {attribute,6,file,{"./p.erl",6}},
	  {function,7,q,0,
	   [{clause,7,[],[],
	     [{call,8,
	       {remote,8,{atom,8,qlc},{atom,8,q}},
	       [{tuple,-8,
		 [{atom,-8,qlc_lc},
		  {'fun',-8,
		   {clauses,
		    [{clause,-8,[],[],
		      [{tuple,-8,
			[{atom,-8,simple_v1},
			 {atom,-8,'X'},
			 {'fun',-8,{clauses,[{clause,-8,[],[],[{nil,8}]}]}},
			 {integer,-8,8}]}]}]}},
		  {atom,-8,undefined}]}]}]}]},
	  {eof,9}],
    {ok,'p.p',_} = compile:forms(Fs, ['S',report]),
    ok.

encrypted_abstr(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(10)),
    ?line {Simple,Target} = files(Config, "encrypted_abstr"),

    Res = case has_crypto() of
	      no ->
		  %% No crypto.
		  ?line encrypted_abstr_no_crypto(Simple, Target),
		  {comment,"The crypto application is missing or broken"};
	      yes ->
		  %% Simulate not having crypto by removing
		  %% the crypto application from the path.
		  ?line OldPath = code:get_path(),
		  try
		      ?line NewPath = OldPath -- [filename:dirname(code:which(crypto))],
		      ?line (catch crypto:stop()),
		      ?line code:delete(crypto),
		      ?line code:purge(crypto),
		      ?line code:set_path(NewPath),
		      ?line encrypted_abstr_no_crypto(Simple, Target)
		      after
			  code:set_path(OldPath)
		      end,

		  %% Now run the tests that require crypto.
		  ?line encrypted_abstr_1(Simple, Target),
		  ?line ok = file:delete(Target),
		  ?line ok = file:del_dir(filename:dirname(Target))
	  end,
    
    %% Cleanup.
    ?line test_server:timetrap_cancel(Dog),
    Res.

encrypted_abstr_1(Simple, Target) ->
    ?line TargetDir = filename:dirname(Target),
    ?line Key = "ablurf123BX#$;3",
    ?line install_crypto_key(Key),
    ?line {ok,simple} = compile:file(Simple,
				     [debug_info,{debug_info_key,Key},
				      {outdir,TargetDir}]),
    ?line verify_abstract(Target),

    ?line {ok,simple} = compile:file(Simple,
				     [{debug_info_key,Key},
				      {outdir,TargetDir}]),
    ?line verify_abstract(Target),

    ?line {ok,simple} = compile:file(Simple,
				     [debug_info,{debug_info_key,{des3_cbc,Key}},
				      {outdir,TargetDir}]),
    ?line verify_abstract(Target),

    ?line {ok,{simple,[{compile_info,CInfo}]}} = 
	beam_lib:chunks(Target, [compile_info]),
    ?line {value,{_,Opts}} = lists:keysearch(options, 1, CInfo),
    ?line {value,{_,'********'}} = lists:keysearch(debug_info_key, 1, Opts),

    %% Try some illegal forms of crypto keys.
    ?line error = compile:file(Simple,
			       [debug_info,{debug_info_key,{blurf,"ss"}},report]),
    ?line error = compile:file(Simple,
			       [debug_info,{debug_info_key,{blurf,1,"ss"}},report]),
    ?line error = compile:file(Simple,
			       [debug_info,{debug_info_key,42},report]),

    %% Place the crypto key in .erlang.crypt.
    ?line beam_lib:clear_crypto_key_fun(),
    ?line {ok,OldCwd} = file:get_cwd(),
    ?line ok = file:set_cwd(TargetDir),

    ?line error = compile:file(Simple, [encrypt_debug_info,report]),

    ?line NewKey = "better use another key here",
    ?line write_crypt_file(["[{debug_info,des3_cbc,simple,\"",NewKey,"\"}].\n"]),
    ?line {ok,simple} = compile:file(Simple, [encrypt_debug_info,report]),
    ?line verify_abstract("simple.beam"),
    ?line ok = file:delete(".erlang.crypt"),
    ?line beam_lib:clear_crypto_key_fun(),
    ?line {error,beam_lib,{key_missing_or_invalid,"simple.beam",abstract_code}} =
	beam_lib:chunks("simple.beam", [abstract_code]),
    ?line ok = file:set_cwd(OldCwd),
    ok.


write_crypt_file(Contents0) ->
    Contents = list_to_binary([Contents0]),
    io:format("~s\n", [binary_to_list(Contents)]),
    ok = file:write_file(".erlang.crypt", Contents).

encrypted_abstr_no_crypto(Simple, Target) ->
    ?line TargetDir = filename:dirname(Target),
    ?line Key = "ablurf123BX#$;3",
    ?line error = compile:file(Simple,
			       [debug_info,{debug_info_key,Key},
				{outdir,TargetDir},report]),
    ok.
    
verify_abstract(Target) ->
    {ok,{simple,[Chunk]}} = beam_lib:chunks(Target, [abstract_code]),
    {abstract_code,{raw_abstract_v1,_}} = Chunk.

has_crypto() ->
    try
	crypto:start(),
	crypto:info(),
	crypto:stop(),
	yes
    catch
	error:_ -> no
    end.

install_crypto_key(Key) ->
    F = fun (init) -> ok;
	    ({debug_info,des3_cbc,_,_}) -> Key;
	    (clear) -> ok
	end,
    ok = beam_lib:crypto_key_fun(F).

%% Miscellanous tests, mainly to get better coverage.
cover(Config) when is_list(Config) ->
    ?line io:format("~p\n", [compile:options()]),
    ok.

do_listing(Source, TargetDir, Type) ->
    do_listing(Source, TargetDir, Type, "." ++ atom_to_list(Type)).

do_listing(Source, TargetDir, Type, Ext) ->
    io:format("Source: ~p TargetDir: ~p\n  Type: ~p Ext: ~p\n",
	      [Source, TargetDir, Type, Ext]),
    case compile:file(Source, [Type, time, {outdir, TargetDir}]) of
	{ok, _} -> ok;
	Other -> test_server:fail({unexpected_result, Other})
    end,
    SourceBase = filename:rootname(filename:basename(Source)),

    Target = filename:join(TargetDir, SourceBase ++ Ext),
    true = exists(Target).

files(Config, Name) ->
    ?line code:delete(simple),
    ?line code:purge(simple),
    ?line DataDir = ?config(data_dir, Config),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Simple = filename:join(DataDir, "simple"),
    ?line TargetDir = filename:join(PrivDir, Name),
    ?line ok = file:make_dir(TargetDir),
    ?line Target = filename:join(TargetDir, "simple"++code:objfile_extension()),
    {Simple, Target}.


run(Target, Func, Args) ->
    ?line Module = list_to_atom(filename:rootname(filename:basename(Target))),
    ?line {module, Module} = code:load_abs(filename:rootname(Target)),
    ?line Result = (catch apply(Module, Func, Args)),
    ?line true = code:delete(Module),
    ?line false = code:purge(Module),
    Result.

exists(Name) ->
    case file:read_file_info(Name) of
	{ok, _}    -> true;
	{error, _} -> false
    end.

bad_record_use(suite) ->  [bad_record_use1, bad_record_use2].

%% Tests that the compiler does not accept
%% bad use of records.
bad_record_use1(Config) when is_list(Config) ->
    ?line {ok, Cwd} = file:get_cwd(),
    ?line file:set_cwd(?config(data_dir, Config)),
    ?line true=exists("bad_record_use.erl"),
    ?line Ret=c:c(bad_record_use),
    ?line file:set_cwd(Cwd),
    ?line error=Ret,
    ok.

%% Tests that the compiler does not accept
%% bad use of records.
bad_record_use2(Config) when is_list(Config) ->
    ?line {ok, Cwd} = file:get_cwd(),
    ?line file:set_cwd(?config(data_dir, Config)),
    ?line true=exists("bad_record_use2.erl"),
    ?line Ret=c:c(bad_record_use),
    ?line file:set_cwd(Cwd),
    ?line error=Ret,
    ok.

strict_record(Config) when is_list(Config) ->
    ?line Priv = ?config(priv_dir, Config),
    ?line file:set_cwd(?config(data_dir, Config)),
    ?line Opts = [{outdir,Priv},report_errors],
    M = record_access,
 
    ?line {ok,M} = c:c(M, [strict_record_tests|Opts]),
    ?line Turtle = test_strict(),

    ?line {ok,M} = c:c(M, [no_strict_record_tests|Opts]),
    ?line Turtle = test_sloppy(),

    %% The option first given wins.
    ?line {ok,M} = c:c(M, [no_strict_record_tests,strict_record_tests|Opts]),
    ?line Turtle = test_sloppy(),
    ?line {ok,M} = c:c(M, [strict_record_tests,no_strict_record_tests|Opts]),
    ?line Turtle = test_strict(),

    %% Default (possibly influenced by ERL_COMPILER_OPTIONS).
    ?line {ok,M} = c:c(M, [{outdir,Priv},report_errors]),
    ?line try
	      {1,2} = record_access:test(Turtle),
	      {comment,"Default: no_strict_record_tests"}
	  catch
	      error:{badrecord,tortoise} ->
		  {comment,"Default: strict_record_tests"}
	  end.

test_strict() ->
    Turtle = record_access:turtle(),
    ?line try
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

missing_testheap(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    Opts = [{outdir,PrivDir}],
    OldPath = code:get_path(),
    try
	code:add_patha(PrivDir),
	c:c(filename:join(DataDir, "missing_testheap1"), Opts),
	c:c(filename:join(DataDir, "missing_testheap2"), Opts),
	?line ok = test(fun() ->
				missing_testheap1:f({a,self()},{state,true,b})
			end, {a,b}),
	?line ok = test(fun() ->
				missing_testheap2:f({a,self()},16#80000000) end,
			bigger)
	after
	    code:set_path(OldPath),
	    file:delete(filename:join(PrivDir, "missing_testheap1.beam")),
	    file:delete(filename:join(PrivDir, "missing_testheap2.beam"))
	end,
    ok.
    
test(Fun, Result) ->
    test(500, Fun, Result, []).

test(0, _, _, _) ->
    ok;
test(Iter, Fun, Result, Filler) ->
    spawn(?MODULE, init, [self(), Fun, list_to_tuple(Filler)]),
    receive
	{result, Result} ->
	    test(Iter-1, Fun, Result, [0|Filler]);
	{result, Other} ->
	    io:format("Expected ~p; got ~p~n", [Result, Other]),
	    test_server:fail()
    end.

init(ReplyTo, Fun, _Filler) ->
    ReplyTo ! {result, Fun()}.

env(Config) when is_list(Config) ->
    ?line {Simple,Target} = files(Config, "file_1"),
    ?line {ok,Cwd} = file:get_cwd(),
    ?line ok = file:set_cwd(filename:dirname(Target)),

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
    ?line {ok,simple,<<_/binary>>} = compile:file(Simple),
    ?line {ok,simple} = compile:noenv_file(Simple, [debug_info]),
    ?line true = exists(Target),
    ?line {ok,{simple,[{abstract_code,Abstr0}]}} =
	beam_lib:chunks(Target, [abstract_code]),
    ?line {raw_abstract_v1,Forms} = Abstr0,

    %% forms
    ?line true = os:putenv("ERL_COMPILER_OPTIONS", "strong_validation"),
    ?line {ok,simple} = compile:forms(Forms),
    ?line {ok,simple,<<"FOR1",_/binary>>} = compile:noenv_forms(Forms, []),

    %% output_generated
    ?line false = compile:output_generated([]),
    ?line true = compile:noenv_output_generated([]),

    ?line ok = file:delete(Target),

    ok.

%% Test pretty-printing in Core Erlang format and then try to
%% compile the generated Core Erlang files.

core(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(5)),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Outdir = filename:join(PrivDir, "core"),
    ?line ok = file:make_dir(Outdir),

    ?line Wc = filename:join(filename:dirname(code:which(?MODULE)), "*.beam"),
    ?line TestBeams = filelib:wildcard(Wc),
    ?line Abstr = [begin {ok,{Mod,[{abstract_code,
				    {raw_abstract_v1,Abstr}}]}} = 
			     beam_lib:chunks(Beam, [abstract_code]),
			 {Mod,Abstr} end || Beam <- TestBeams],
    ?line Res = test_lib:p_run(fun(F) -> do_core(F, Outdir) end, Abstr),
    ?line test_server:timetrap_cancel(Dog),
    Res.

    
do_core({M,A}, Outdir) ->
    try
	{ok,M,Core} = compile:forms(A, [to_core,report]),
	CoreFile = filename:join(Outdir, atom_to_list(M)++".core"),
	CorePP = core_pp:format(Core),
	ok = file:write_file(CoreFile, CorePP),
	case compile:file(CoreFile, [clint,from_core,binary]) of
	    {ok,M,_} ->
		ok = file:delete(CoreFile);
	    Other ->
		io:format("*** core_lint failure '~p' for ~s\n",
			  [Other,CoreFile]),
		error
	end
    catch Class:Error ->
	    io:format("~p: ~p ~p\n~p\n",
		      [M,Class,Error,erlang:get_stacktrace()]),
	    error
    end.

%% Compile to Beam assembly language (.S) and the try to
%% run .S throught the compiler again.

asm(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(20)),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Outdir = filename:join(PrivDir, "asm"),
    ?line ok = file:make_dir(Outdir),

    ?line Wc = filename:join(filename:dirname(code:which(?MODULE)), "*.beam"),
    ?line TestBeams = filelib:wildcard(Wc),
    ?line Res = test_lib:p_run(fun(F) -> do_asm(F, Outdir) end, TestBeams),
    ?line test_server:timetrap_cancel(Dog),
    Res.

    
do_asm(Beam, Outdir) ->
    {ok,{M,[{abstract_code,{raw_abstract_v1,A}}]}} =
	beam_lib:chunks(Beam, [abstract_code]),
    try
	{ok,M,Asm} = compile:forms(A, ['S']),
	AsmFile = filename:join(Outdir, atom_to_list(M)++".S"),
	{ok,Fd} = file:open(AsmFile, [write]),
	beam_listing:module(Fd, Asm),
	ok = file:close(Fd),
	case compile:file(AsmFile, [from_asm,no_postopt,binary,report]) of
	    {ok,M,_} ->
		ok = file:delete(AsmFile);
	    Other ->
		io:format("*** failure '~p' for ~s\n",
			  [Other,AsmFile]),
		error
	end
    catch Class:Error ->
	    io:format("~p: ~p ~p\n~p\n",
		      [M,Class,Error,erlang:get_stacktrace()]),
	    error
    end.
