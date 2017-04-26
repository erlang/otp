%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%% Purpose : Compiles various modules with tough code

-module(compilation_SUITE).
-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 beam_compiler_4/1,
	 beam_compiler_6/1,
	 beam_compiler_7/1,
	 beam_compiler_8/1,
	 beam_compiler_9/1,
	 beam_compiler_10/1,
	 beam_compiler_11/1,
	 compiler_1/1,
	 const_list_256/1,
	 convopts/1,
	 live_var/1,
	 on_load/1,
	 on_load_inline/1,
	 opt_crash/1,
	 otp_2330/1,
	 otp_2380/1,
	 otp_4790/1,
	 otp_5151/1,
	 otp_5235/1,
	 otp_5404/1,
	 otp_5436/1,
	 otp_5481/1,
	 otp_5553/1,
	 otp_5632/1,
	 otp_5714/1,
	 otp_5872/1,
	 otp_6121/1,
	 otp_7202/1,
	 otp_8949_a/1,
	 redundant_case/1,
	 self_compile/1,
	 self_compile_old_inliner/1,
	 split_cases/1,
	 string_table/1,
	 vsn_1/1,
	 vsn_2/1,
	 vsn_3/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,10}}].

all() -> 
    test_lib:recompile(?MODULE),
    [self_compile_old_inliner,self_compile,
     {group,p}].

groups() -> 
    [{vsn,[parallel],[vsn_1,vsn_2,vsn_3]},
     {p,test_lib:parallel(),
      [compiler_1,
       beam_compiler_4,beam_compiler_6,beam_compiler_7,
       beam_compiler_8,beam_compiler_9,beam_compiler_10,
       beam_compiler_11,
       otp_2330,
       {group,vsn},otp_2380,otp_4790,
       const_list_256,live_var,convopts,
       redundant_case,
       otp_5151,otp_5235,
       opt_crash,otp_5404,otp_5436,otp_5481,
       otp_5553,otp_5632,otp_5714,otp_5872,otp_6121,
       otp_7202,on_load,on_load_inline,
       string_table,otp_8949_a,split_cases]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

-define(comp(N),
	N(Config) when is_list(Config) -> try_it(N, Config)).

?comp(compiler_1).

?comp(beam_compiler_4).
?comp(beam_compiler_6).
?comp(beam_compiler_8).
?comp(beam_compiler_9).
?comp(beam_compiler_10).
?comp(beam_compiler_11).

?comp(otp_2330).
?comp(otp_2380).
?comp(otp_4790).
?comp(otp_5235).

?comp(const_list_256).

?comp(otp_5151).

?comp(live_var).
?comp(opt_crash).

?comp(otp_5404).
?comp(otp_5436).
?comp(otp_5481).
?comp(otp_5553).
?comp(otp_5632).
?comp(otp_5714).
?comp(otp_5872).
?comp(otp_6121).
?comp(convopts).
?comp(otp_7202).
?comp(on_load).
?comp(on_load_inline).

%% Code snippet submitted from Ulf Wiger which fails in R3 Beam.
beam_compiler_7(Config) when is_list(Config) ->
    done = empty(2, false).

empty(N, Toggle) when N > 0 ->
    %% R3 Beam copies the second argument to the first before call.
    empty(N-1, not(Toggle));
empty(_, _) ->
    done.

redundant_case(Config) when is_list(Config) ->
    d = redundant_case_1(1),
    d = redundant_case_1(2),
    d = redundant_case_1(3),
    d = redundant_case_1(4),
    d = redundant_case_1(5),
    d = redundant_case_1({glurf,glarf}),
    ok.

%% This function always returns 'd'. Check that the compiler otptimizes
%% it properly.
redundant_case_1(1) -> d;
redundant_case_1(2) -> d;
redundant_case_1(3) -> d;
redundant_case_1(4) -> d;
redundant_case_1(_) -> d.

try_it(Module, Conf) ->
    Timetrap = {minutes,10},
    OtherOpts = [],			%Can be changed to [time] if needed
    Src = filename:join(proplists:get_value(data_dir, Conf),
			atom_to_list(Module)),
    Out = proplists:get_value(priv_dir,Conf),
    io:format("Compiling: ~s\n", [Src]),
    CompRc0 = compile:file(Src, [clint0,clint,{outdir,Out},report,
				 bin_opt_info|OtherOpts]),
    io:format("Result: ~p\n",[CompRc0]),
    {ok,_Mod} = CompRc0,

    load_and_call(Out, Module),

    ct:timetrap(Timetrap),
    io:format("Compiling (without optimization): ~s\n", [Src]),
    CompRc1 = compile:file(Src,
			   [no_copt,no_postopt,
			    {outdir,Out},report|OtherOpts]),

    io:format("Result: ~p\n",[CompRc1]),
    {ok,_Mod} = CompRc1,
    load_and_call(Out, Module),

    ct:timetrap(Timetrap),
    io:format("Compiling (with old inliner): ~s\n", [Src]),
    CompRc2 = compile:file(Src, [clint,
				 {outdir,Out},report,bin_opt_info,
				 {inline,1000}|OtherOpts]),
    io:format("Result: ~p\n",[CompRc2]),
    {ok,_Mod} = CompRc2,
    load_and_call(Out, Module),

    ct:timetrap(Timetrap),
    io:format("Compiling (from assembly): ~s\n", [Src]),
    {ok,_} = compile:file(Src, [to_asm,{outdir,Out},report|OtherOpts]),
    Asm = filename:join(Out, lists:concat([Module, ".S"])),
    CompRc3 = compile:file(Asm, [from_asm,{outdir,Out},report|OtherOpts]),
    io:format("Result: ~p\n",[CompRc3]),
    {ok,_} = CompRc3,
    load_and_call(Out, Module),

    ok.

load_and_call(Out, Module) ->
    io:format("Loading...\n",[]),
    {module,Module} = code:load_abs(filename:join(Out, Module)),

    io:format("Calling...\n",[]),
    %% Call M:M, and expect ok back, that's our interface
    CallRc = Module:Module(),
    io:format("Got value: ~p\n",[CallRc]),

    ok = CallRc,

    %% Smoke-test of beam disassembler.
    test_lib:smoke_disasm(Module),

    _ = code:delete(Module),
    _ = code:purge(Module),

    %% Restore state of trap_exit just in case. (Since the compiler
    %% uses a temporary process, we will get {'EXIT',Pid,normal} messages
    %% if trap_exit is true.)

    process_flag(trap_exit, false),
    ok.


%% Test generation of 'vsn' attribute.
vsn_1(Conf) when is_list(Conf) ->
    M = vsn_1,

    compile_load(M, proplists:get_value(data_dir, Conf), Conf),
    Vsn1 = get_vsn(M),
    timer:sleep(1000),

    compile_load(M, proplists:get_value(data_dir, Conf), Conf),
    Vsn2 = get_vsn(M),

    compile_load(M, filename:join(proplists:get_value(data_dir, Conf),
				  "other"),
		 Conf),
    Vsn3 = get_vsn(M),
    if
	Vsn1 == Vsn2, Vsn2 == Vsn3 ->
	    ok;
	true ->
	    ct:fail({vsn, Vsn1, Vsn2, Vsn3})
    end,
    ok.

%% Test overriding of generation of 'vsn' attribute.
vsn_2(Conf) when is_list(Conf) ->
    M = vsn_2,

    compile_load(M, proplists:get_value(data_dir, Conf), Conf),
    Vsn = get_vsn(M),
    case Vsn of
	[34] ->
	    ok;
	_ ->
	    ct:fail({vsn, Vsn})
    end,
    ok.

%% Test that different code yields different generated 'vsn'.
vsn_3(Conf) when is_list(Conf) ->
    M = vsn_3,

    compile_load(M, proplists:get_value(data_dir, Conf), Conf),
    Vsn1 = get_vsn(M),

    compile_load(M, filename:join(proplists:get_value(data_dir, Conf),
				  "other"),
		 Conf),
    Vsn2 = get_vsn(M),
    if
	Vsn1 /= Vsn2 ->
	    ok;
	true ->
	    ct:fail({vsn, Vsn1, Vsn2})
    end,
    ok.

get_vsn(M) ->
    {vsn,V} = lists:keyfind(vsn, 1, M:module_info(attributes)),
    V.

compile_load(Module, Dir, Conf) ->
    Src = filename:join(Dir, atom_to_list(Module)),
    Out = proplists:get_value(priv_dir,Conf),
    CompRc = compile:file(Src, [{outdir,Out}]),
    {ok, Module} = CompRc,
    code:purge(Module),
    {module, Module} =
	code:load_abs(filename:join(Out, atom_to_list(Module))),
    ok.

self_compile(Config) when is_list(Config) ->
    self_compile_1(Config, "new", [inline]).

self_compile_old_inliner(Config) when is_list(Config) ->
    %% The old inliner is useful for testing that sys_core_fold does not
    %% introduce name capture problems.
    self_compile_1(Config, "old", [verbose,{inline,500}]).

self_compile_1(Config, Prefix, Opts) ->
    ct:timetrap({minutes,40}),

    Priv = proplists:get_value(priv_dir,Config),
    Version = compiler_version(),

    %% Compile the compiler. (In this node to get better coverage.)
    CompA = make_compiler_dir(Priv, Prefix++"compiler_a"),
    VsnA = Version ++ ".0",
    compile_compiler(compiler_src(), CompA, VsnA, Opts),

    %% Compile the compiler again using the newly compiled compiler.
    %% (In another node because reloading the compiler would disturb cover.)
    CompilerB = Prefix++"compiler_b",
    CompB = make_compiler_dir(Priv, CompilerB),
    VsnB = VsnA ++ ".0",
    self_compile_node(CompA, CompB, VsnB, Opts),

    %% Compare compiler directories. The compiler directories should
    %% be equal (except for beam_asm that contains the compiler version).
    compare_compilers(CompA, CompB),

    ok.

self_compile_node(CompilerDir, OutDir, Version, Opts) ->
    ct:timetrap({minutes,15}),
    Pa = "-pa " ++ filename:dirname(code:which(?MODULE)) ++
	" -pa " ++ CompilerDir,
    Files = compiler_src(),

    %% We don't want the cover server started on the other node,
    %% because it will load the same cover-compiled code as on this
    %% node. Use a shielded node to prevent the cover server from
    %% being started.
    test_server:run_on_shielded_node(
      fun() ->
	      compile_compiler(Files, OutDir, Version, Opts)
      end, Pa),

    ok.

compile_compiler(Files, OutDir, Version, InlineOpts) ->
    io:format("~ts", [code:which(compile)]),
    io:format("Compiling ~s into ~ts", [Version,OutDir]),
    Opts = [report,
	    clint0,clint,
	    bin_opt_info,
	    {outdir,OutDir},
	    {d,'COMPILER_VSN',"\""++Version++"\""},
	    nowarn_shadow_vars,
	    {i,filename:join(code:lib_dir(stdlib), "include")}|InlineOpts],
    test_lib:p_run(fun(File) ->
			   case compile:file(File, Opts) of
			       {ok,_} -> ok;
			       _ -> error
			   end
		   end, Files).

compiler_src() ->
    filelib:wildcard(filename:join([code:lib_dir(compiler), "src", "*.erl"])).

make_compiler_dir(Priv, Dir0) ->
    Dir = filename:join(Priv, Dir0),
    ok = file:make_dir(Dir),
    Dir.

compiler_version() ->
    {version,Version} = lists:keyfind(version, 1,
				      compile:module_info(compile)),
    Version.

compare_compilers(ADir, BDir) ->
    {[],[],D} = beam_lib:cmp_dirs(ADir, BDir),

    %% beam_asm.beam contains compiler version and therefore it *must*
    %% compare unequal.
    ["beam_asm.beam"] = [filename:basename(A) || {A,_} <- D],
    ok.

%% Check the generation of the string table.

string_table(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    File = filename:join(DataDir, "string_table.erl"),
    {ok,string_table,Beam,[]} = compile:file(File, [return, binary]),
    {ok,{string_table,[StringTableChunk]}} = beam_lib:chunks(Beam, ["StrT"]),
    {"StrT", <<"stringtable">>} = StringTableChunk,
    ok.

otp_8949_a(Config) when is_list(Config) ->
    value = do_otp_8949_a(),
    ok.

-record(cs, {exs,keys = [],flags = 1}).
-record(exs, {children = []}).

do_otp_8949_a() ->
    case id([#cs{}]) of
        [#cs{}=Cs] ->
            SomeVar = id(value),
	    if
		Cs#cs.flags band 1 =/= 0 ->
		    id(SomeVar);
		(((Cs#cs.exs)#exs.children /= [])
                 and
		   (Cs#cs.flags band (1 bsl 0 bor (1 bsl 22)) == 0));
		Cs#cs.flags band (1 bsl 22) =/= 0 ->
		    ok
	    end
    end.
    
split_cases(_) ->
    dummy1 = do_split_cases(x),
    {'EXIT',{{badmatch,b},_}} = (catch do_split_cases(y)),
    ok.

do_split_cases(A) ->
    case A of
        x ->
	    Z = dummy1;
        _ ->
	    Z = dummy2,
	    a=b
    end,
    Z.


id(I) -> I.
