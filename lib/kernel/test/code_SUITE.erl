%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
-module(code_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("syntax_tools/include/merl.hrl").

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2]).
-export([set_path/1, get_path/1, add_path/1, add_paths/1, del_path/1,
	 replace_path/1, load_file/1, load_abs/1, ensure_loaded/1,
	 delete/1, purge/1, purge_many_exits/0, purge_many_exits/1,
         soft_purge/1, is_loaded/1, all_loaded/1,
	 load_binary/1, dir_req/1, object_code/1, set_path_file/1,
	 upgrade/1,
	 sticky_dir/1, pa_pz_option/1, add_del_path/1,
	 dir_disappeared/1, ext_mod_dep/1, clash/1,
	 where_is_file/1,
	 purge_stacktrace/1, mult_lib_roots/1, bad_erl_libs/1,
	 code_archive/1, code_archive2/1, on_load/1, on_load_binary/1,
	 on_load_embedded/1, on_load_errors/1, on_load_update/1,
         on_load_trace_on_load/1,
	 on_load_purge/1, on_load_self_call/1, on_load_pending/1,
	 on_load_deleted/1,
	 big_boot_embedded/1,
         module_status/1,
	 native_early_modules/1, get_mode/1,
	 normalized_paths/1, mult_embedded_flags/1]).

-export([init_per_testcase/2, end_per_testcase/2,
	 init_per_suite/1, end_per_suite/1]).

%% error_logger
-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).

-export([compile_load/4]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,30}}].

all() ->
    [set_path, get_path, add_path, add_paths, del_path,
     replace_path, load_file, load_abs, ensure_loaded,
     delete, purge, purge_many_exits, soft_purge, is_loaded, all_loaded,
     load_binary, dir_req, object_code, set_path_file,
     upgrade,
     sticky_dir, pa_pz_option, add_del_path, dir_disappeared,
     ext_mod_dep, clash, where_is_file,
     purge_stacktrace, mult_lib_roots,
     bad_erl_libs, code_archive, code_archive2, on_load,
     on_load_binary, on_load_embedded, on_load_errors,
     {group, sequence},
     on_load_purge, on_load_self_call, on_load_pending,
     on_load_deleted,
     module_status,
     big_boot_embedded, native_early_modules, get_mode, normalized_paths,
     mult_embedded_flags].

%% These need to run in order
groups() -> [{sequence, [sequence], [on_load_update,
                                     on_load_trace_on_load]}].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.

init_per_suite(Config) ->
    %% The compiler will no longer create a Beam file if
    %% the module name does not match the filename, so
    %% we must compile to a binary and write the Beam file
    %% ourselves.
    Dir = filename:dirname(code:which(?MODULE)),
    File = filename:join(Dir, "code_a_test"),
    {ok,code_b_test,Code} = compile:file(File, [binary]),
    ok = file:write_file(File++".beam", Code),
    Config.

end_per_suite(Config) ->
    Config.

-define(TESTMOD, test_dummy).
-define(TESTMODSTR, "test_dummy").
-define(TESTMODSRC, ?TESTMODSTR ".erl").
-define(TESTMODOBJ, ?TESTMODSTR ".beam").

init_per_testcase(big_boot_embedded, Config) ->
    case catch crypto:start() of
	ok ->
	    init_per_testcase(do_big_boot_embedded, Config);
	_Else ->
	    {skip, "Needs crypto!"}
    end;
init_per_testcase(on_load_embedded, Config0) ->
    LibRoot = code:lib_dir(),
    LinkName = filename:join(LibRoot, "on_load_app-1.0"),
    Config = [{link_name,LinkName}|Config0],
    init_per_testcase(Config);
init_per_testcase(_Func, Config) ->
    init_per_testcase(Config).

init_per_testcase(Config) ->
    P = code:get_path(),
    [{code_path, P}|Config].


end_per_testcase(module_status, Config) ->
    code:purge(?TESTMOD),
    code:delete(?TESTMOD),
    code:purge(?TESTMOD),
    file:delete(?TESTMODOBJ),
    file:delete(?TESTMODSRC),
    end_per_testcase(Config);
end_per_testcase(TC, Config) when TC == mult_lib_roots;
				  TC == big_boot_embedded ->
    {ok, HostName} = inet:gethostname(),
    NodeName = list_to_atom(atom_to_list(TC)++"@"++HostName),
    test_server:stop_node(NodeName),
    end_per_testcase(Config);
end_per_testcase(on_load_embedded, Config) ->
    LinkName = proplists:get_value(link_name, Config),
    _ = del_link(LinkName),
    end_per_testcase(Config);
end_per_testcase(upgrade, Config) ->
    %% Make sure tracing is turned off even if the test times out.
    erlang:trace_pattern({error_handler,undefined_function,3}, false, [global]),
    erlang:trace(self(), false, [call]),
    end_per_testcase(Config);
end_per_testcase(_Func, Config) ->
    end_per_testcase(Config).

end_per_testcase(Config) ->
    code:purge(code_b_test),
    P=proplists:get_value(code_path, Config),
    true=code:set_path(P),
    P=code:get_path(),
    ok.

set_path(Config) when is_list(Config) ->
    P = code:get_path(),
    NonExDir = filename:join(proplists:get_value(priv_dir, Config), test_server:temp_name("hej")),
    {'EXIT',_} = (catch code:set_path({a})),
    {error, bad_directory} = (catch code:set_path([{a}])),
    {error, bad_directory} = code:set_path(NonExDir),
    P = code:get_path(), % still the same path.
    true = code:set_path(P), % set the same path again.
    P = code:get_path(), % still the same path.
    LibDir = code:lib_dir(),
    true = code:set_path([LibDir | P]),
    [LibDir | P] = code:get_path(),
    true = code:set_path([LibDir]),
    [LibDir] = code:get_path(),
    ok.

get_path(Config) when is_list(Config) ->
    P = code:get_path(),
    %% test that all directories are strings (lists).
    [] = lists:filter(fun
	    (Dir) when is_list(Dir) -> false;
	    (_) -> true
	end, P),
    ok.

add_path(Config) when is_list(Config) ->
    P = code:get_path(),
    {'EXIT',_} = (catch code:add_path({})),
    {'EXIT',_} = (catch code:add_patha({})),
    {'EXIT',_} = (catch code:add_pathz({})),
    {error, bad_directory} = code:add_path("xyz"),
    {error, bad_directory} = code:add_patha("xyz"),
    {error, bad_directory} = code:add_pathz("xyz"),
    LibDir = code:lib_dir(),
    true = code:add_path(LibDir),
    LibDir = lists:last(code:get_path()),
    code:set_path(P),
    true = code:add_pathz(LibDir),
    LibDir = lists:last(code:get_path()),
    code:set_path(P),
    true = code:add_patha(LibDir),
    [LibDir|_] = code:get_path(),
    code:set_path(P),
    ok.

add_paths(Config) when is_list(Config) ->
    P = code:get_path(),
    ok = code:add_paths([{}]),
    ok = code:add_pathsa([{}]),
    ok = code:add_pathsz([{}]),
    ok = code:add_paths(["xyz"]),
    ok = code:add_pathsa(["xyz"]),
    ok = code:add_pathsz(["xyz"]),
    P = code:get_path(), % check that no directory is added.

    LibDir = code:lib_dir(),
    ok = code:add_paths([LibDir]),
    LibDir = lists:last(code:get_path()),
    code:set_path(P),
    ok = code:add_pathsz([LibDir]),
    LibDir = lists:last(code:get_path()),
    code:set_path(P),
    ok = code:add_pathsa([LibDir]),
    [LibDir|P] = code:get_path(),
    code:set_path(P),

    RootDir = code:root_dir(),
    Res = P ++ [LibDir, RootDir],
    ok = code:add_paths([LibDir, RootDir]),
    Res = code:get_path(),
    code:set_path(P),
    ok = code:add_pathsz([LibDir, RootDir]),
    Res = code:get_path(),
    code:set_path(P),
    ok = code:add_pathsa([LibDir, RootDir]),
    [RootDir, LibDir|P] = code:get_path(),
    code:set_path(P),

    ok = code:add_paths([LibDir, "xyz"]),
    Res1 = P ++ [LibDir],
    Res1 = code:get_path(),
    code:set_path(P),
    ok = code:add_pathsz([LibDir, "xyz"]),
    Res1 = code:get_path(),
    code:set_path(P),
    ok = code:add_pathsa([LibDir, "xyz"]),
    [LibDir|P] = code:get_path(),
    code:set_path(P),
    ok.

del_path(Config) when is_list(Config) ->
    P = code:get_path(),
    try
	del_path_1(P)
    after
	code:set_path(P)
    end.

del_path_1(P) ->
    io:format("Initial code:get_path()=~p~n",[P]),
    {'EXIT',_} = (catch code:del_path(3)),
    false = code:del_path(my_dummy_name),
    false = code:del_path("/kdlk/my_dummy_dir"),
    Dir = filename:join([code:lib_dir(kernel),"ebin"]),
    io:format("kernel dir: ~p~n",[Dir]),

    true = code:del_path(kernel),
    NewP = code:get_path(),
    io:format("Path after removing 'kernel':~p~n",[NewP]),
    ReferenceP = lists:delete(Dir,P),
    io:format("Reference path:~p~n",[ReferenceP]),
    NewP = ReferenceP, % check that dir is deleted
    code:set_path(P),

    %% An superfluous "/" should also work.
    true = code:del_path("kernel/"),
    NewP = ReferenceP,			   % check that dir is deleted
    code:set_path(P),

    true = code:del_path(Dir),
    NewP1 = code:get_path(),
    NewP1 = lists:delete(Dir,P), % check that dir is deleted
    ok.

replace_path(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    P = code:get_path(),
    {'EXIT',_} = (catch code:replace_path(3,"")),
    {error, bad_name} = code:replace_path(dummy_name,""),
    {error, bad_name} = code:replace_path(kernel,
						"/kdlk/my_dummy_dir"),
    {error, bad_directory} = code:replace_path(kernel,
						     "/kdlk/kernel-1.2"),
    P = code:get_path(), % Check that path is not changed.

    ok = file:set_cwd(PrivDir),

    %% Replace an existing application.

    file:make_dir("./kernel-2.11"),
    {ok, Cwd} = file:get_cwd(),
    NewDir = Cwd ++ "/kernel-2.11",
    true = code:replace_path(kernel, NewDir),
    NewDir = code:lib_dir(kernel),
    true = code:set_path(P),			%Reset path
    ok = file:del_dir("./kernel-2.11"),

    %% Add a completly new application.

    NewAppName = 'blurf_blarfer',
    NewAppDir = filename:join(Cwd, atom_to_list(NewAppName) ++ "-6.33.1"),
    ok = file:make_dir(NewAppDir),
    true = code:replace_path(NewAppName, NewAppDir),
    NewAppDir = code:lib_dir(NewAppName),
    NewAppDir = lists:last(code:get_path()),
    true = code:set_path(P),			%Reset path
    ok = file:del_dir(NewAppDir),

    ok.

%% OTP-3977.
dir_disappeared(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, "temp"),
    ok = file:make_dir(Dir),
    true = code:add_path(Dir),
    ok = file:del_dir(Dir),
    non_existing = code:which(bubbelskrammel),
    ok.

load_file(Config) when is_list(Config) ->
    {error, nofile} = code:load_file(duuuumy_mod),
    {error, badfile} = code:load_file(code_a_test),
    {'EXIT', _} = (catch code:load_file(123)),
    {module, code_b_test} = code:load_file(code_b_test),
    TestDir = test_dir(),
    code:stick_dir(TestDir),
    {error, sticky_directory} = code:load_file(code_b_test),
    code:unstick_dir(TestDir),
    ok.

test_dir() ->
    filename:dirname(code:which(?MODULE)).

load_abs(Config) when is_list(Config) ->
    TestDir = test_dir(),
    {error, nofile} = code:load_abs(TestDir ++ "/duuuumy_mod"),
    {error, badfile} = code:load_abs(TestDir ++ "/code_a_test"),
    {'EXIT', _} = (catch code:load_abs({})),
    {error, nofile} = code:load_abs("Non-latin-имя-файла"),
    {module, code_b_test} = code:load_abs(TestDir ++ "/code_b_test"),
    code:stick_dir(TestDir),
    {error, sticky_directory} = code:load_abs(TestDir ++ "/code_b_test"),
    code:unstick_dir(TestDir),
    ok.

ensure_loaded(Config) when is_list(Config) ->
    {module, lists} = code:ensure_loaded(lists),
    case init:get_argument(mode) of
	{ok, [["embedded"] | _]} ->
	    {error, embedded} = code:ensure_loaded(code_b_test),
	    {error, badarg} = code:ensure_loaded(34),
	    ok;
	_ ->
	    {error, nofile} = code:ensure_loaded(duuuumy_mod),
	    {error, badfile} = code:ensure_loaded(code_a_test),
	    {'EXIT', _} = (catch code:ensure_loaded(34)),
	    {module, code_b_test} = code:ensure_loaded(code_b_test),
	    {module, code_b_test} = code:ensure_loaded(code_b_test),
	    ok
    end.

delete(Config) when is_list(Config) ->
    OldFlag = process_flag(trap_exit, true),
    code:purge(code_b_test),
    Pid = code_b_test:do_spawn(),
    true = code:delete(code_b_test),
    {'EXIT',_} = (catch code:delete(122)),
    false = code_b_test:check_exit(Pid),
    false = code:delete(code_b_test),
    false = code_b_test:check_exit(Pid),
    exit(Pid,kill),
    true = code_b_test:check_exit(Pid),
    false = code:delete(code_b_test),
    code:purge(code_b_test),
    process_flag(trap_exit, OldFlag),
    ok.

purge(Config) when is_list(Config) ->
    OldFlag = process_flag(trap_exit, true),
    code:purge(code_b_test),
    {'EXIT',_} = (catch code:purge({})),
    false = code:purge(code_b_test),
    Pid = code_b_test:do_spawn(),
    true = code:delete(code_b_test),
    false = code_b_test:check_exit(Pid),
    true = code:purge(code_b_test),
    true = code_b_test:check_exit(Pid),
    process_flag(trap_exit, OldFlag),
    ok.

purge_many_exits() ->
    [{timetrap, {minutes, 2}}].

purge_many_exits(Config) when is_list(Config) ->
    OldFlag = process_flag(trap_exit, true),

    code:purge(code_b_test),
    {'EXIT',_} = (catch code:purge({})),

    CodePurgeF = fun(M, Exp) -> Exp = code:purge(M) end,
    purge_many_exits_do(CodePurgeF),

    %% Let's repeat test for erlang:purge_module as it does the same thing
    %% now in erts-8.0 (except for return value).
    ErlangPurgeF = fun(M, _Exp) -> erlang:purge_module(M) end,
    purge_many_exits_do(ErlangPurgeF),

    process_flag(trap_exit, OldFlag),
    ok.


purge_many_exits_do(PurgeF) ->
    false = code:purge(code_b_test),
    TPids = lists:map(fun (_) ->
			      {code_b_test:do_spawn(),
			       spawn_link(fun () ->
						  receive
						  after infinity -> ok
						  end
					  end)}
			 end,
			 lists:seq(1, 1000)),
    %% Give them time to start...
    receive after 1000 -> ok end,
    true = code:delete(code_b_test),
    lists:foreach(fun ({Pid1, Pid2}) ->
			  true = erlang:is_process_alive(Pid1),
			  false = code_b_test:check_exit(Pid1),
			  true = erlang:is_process_alive(Pid2)
		  end, TPids),
    PurgeF(code_b_test, true),
    lists:foreach(fun ({Pid1, Pid2}) ->
			  false = erlang:is_process_alive(Pid1),
			  true = code_b_test:check_exit(Pid1),
			  true = erlang:is_process_alive(Pid2),
			  exit(Pid2, kill)
		  end, TPids),
    lists:foreach(fun ({_Pid1, Pid2}) ->
			  receive {'EXIT', Pid2, _} -> ok end
		  end, TPids).


soft_purge(Config) when is_list(Config) ->
    OldFlag = process_flag(trap_exit, true),
    code:purge(code_b_test),
    {'EXIT',_} = (catch code:soft_purge(23)),
    true = code:soft_purge(code_b_test),
    Pid = code_b_test:do_spawn(),
    true = code:delete(code_b_test),
    false = code_b_test:check_exit(Pid),
    false = code:soft_purge(code_b_test),
    false = code_b_test:check_exit(Pid),
    exit(Pid,kill),
    true = code_b_test:check_exit(Pid),
    true = code:soft_purge(code_b_test),
    process_flag(trap_exit, OldFlag),
    ok.

is_loaded(Config) when is_list(Config) ->
    code:purge(code_b_test),
    code:delete(code_b_test),
    false = code:is_loaded(duuuuuumy_mod),
    {'EXIT',_} = (catch code:is_loaded(23)),
    {file, preloaded} = code:is_loaded(init),
    TestDir = test_dir(),
    {module, code_b_test} = code:load_abs(TestDir ++ "/code_b_test"),
    {file, _Loaded} = code:is_loaded(code_b_test),
    code:purge(code_b_test),
    code:delete(code_b_test),
    ok.

all_loaded(Config) when is_list(Config) ->
    case test_server:is_cover() of
	true -> {skip,"Cover is running"};
	false -> all_loaded_1()
    end.

all_loaded_1() ->
    Preloaded = [{M,preloaded} || M <- lists:sort(erlang:pre_loaded())],

    Loaded0 = lists:sort(code:all_loaded()),
    all_unique(Loaded0),
    Loaded1 = lists:keysort(2, Loaded0),
    Loaded2 = match_and_remove(Preloaded, Loaded1),

    ObjExt = code:objfile_extension(),
    [] = lists:filter(fun
	    ({Mod,AbsName}) when is_atom(Mod), is_list(AbsName) ->
		Mod =/= list_to_atom(filename:basename(AbsName, ObjExt));
	    (_) -> true
	end, Loaded2),
    ok.

match_and_remove([], List) -> List;
match_and_remove([X|T1], [X|T2]) -> match_and_remove(T1, T2).

all_unique([]) -> ok;
all_unique([_]) -> ok;
all_unique([{X,_}|[{Y,_}|_]=T]) when X < Y -> all_unique(T).

load_binary(Config) when is_list(Config) ->
    TestDir = test_dir(),
    File = TestDir ++ "/code_b_test" ++ code:objfile_extension(),
    {ok,Bin} = file:read_file(File),
    {'EXIT',_} = (catch code:load_binary(12, File, Bin)),
    {'EXIT',_} = (catch code:load_binary(code_b_test, 12, Bin)),
    {'EXIT',_} = (catch code:load_binary(code_b_test, File, 12)),
    {module, code_b_test} = code:load_binary(code_b_test, File, Bin),
    code:stick_dir(TestDir),
    {error, sticky_directory} = code:load_binary(code_b_test, File, Bin),
    code:unstick_dir(TestDir),
    code:purge(code_b_test),
    code:delete(code_b_test),
    ok.


upgrade(Config) ->
    DataDir = proplists:get_value(data_dir, Config),

    case erlang:system_info(hipe_architecture) of
        undefined ->
            upgrade_do(DataDir, beam, [beam]);

        _ ->
            T = [beam, hipe],
            [upgrade_do(DataDir, Client, T) || Client <- T],

            case hipe:erllvm_is_supported() of
                false -> ok;
                true  ->
                    T2 = [beam, hipe_llvm],
                    [upgrade_do(DataDir, Client, T2) || Client <- T2]
            end
    end,
    ok.

upgrade_do(DataDir, Client, T) ->
    compile_load(upgrade_client, DataDir, undefined, Client),
    [upgrade_client:run(DataDir, U1, U2, O1, O2)
     || U1<-T, U2<-T, O1<-T, O2<-T],
    ok.

compile_load(Mod, Dir, Ver, CodeType) ->
    %%erlang:display({"{{{{{{{{{{{{{{{{Loading",Mod,Ver,CodeType}),
    Version = case Ver of
	undefined ->
	    io:format("Compiling '~p' as ~p\n", [Mod, CodeType]),
	    [];
	_ ->
	    io:format("Compiling version ~p of '~p' as ~p\n",
		[Ver, Mod, CodeType]),
	    [{d,list_to_atom("VERSION_" ++ integer_to_list(Ver))}]
    end,
    Target = case CodeType of
	beam -> [];
	hipe -> [native];
	hipe_llvm -> [native,{hipe,[to_llvm]}]
    end,
    CompOpts = [binary, report] ++ Target ++ Version,

    Src = filename:join(Dir, atom_to_list(Mod) ++ ".erl"),
    T1 = erlang:now(),
    {ok,Mod,Code} = compile:file(Src, CompOpts),
    T2 = erlang:now(),
    ObjFile = filename:basename(Src,".erl") ++ ".beam",
    {module,Mod} = code:load_binary(Mod, ObjFile, Code),
    T3 = erlang:now(),
    io:format("Compile time ~p ms, Load time ~p ms\n",
	      [timer:now_diff(T2,T1) div 1000, timer:now_diff(T3,T2) div 1000]),
    %%erlang:display({"}}}}}}}}}}}}}}}Loaded",Mod,Ver,CodeType}),
    ok.

dir_req(Config) when is_list(Config) ->
    {ok,[[Root0]]} = init:get_argument(root),
    Root = filename:join([Root0]),	% Normalised form.
    Root = code:root_dir(),
    LibDir = Root ++ "/lib",
    LibDir = code:lib_dir(),
    code:compiler_dir(),
    {error, bad_name} = code:lib_dir(duuumy),
    KernLib = code:lib_dir(kernel),
    Priv = KernLib ++ "/priv",
    Priv = code:priv_dir(kernel),
    {error, bad_name} = code:priv_dir(duuumy),
    ok.

object_code(Config) when is_list(Config) ->
    TestDir = test_dir(),
    P = code:get_path(),
    P = code:get_path(),
    code:add_path(TestDir),
    {module, code_b_test} = code:load_abs(TestDir ++ "/code_b_test"),
    LoadedFile = filename:absname(TestDir ++ "/code_b_test" ++
				  code:objfile_extension()),
    case code:get_object_code(code_b_test) of
	      {code_b_test,Bin,LoadedFile} when is_binary(Bin) ->
		  ok
	  end,
    code:purge(code_b_test),
    code:delete(code_b_test),
    error = code:get_object_code(dddddddduuuuuuumy),
    {'EXIT',_} = (catch code:get_object_code(23)),
    code:set_path(P),
    P=code:get_path(),
    ok.

%% Test that set_path does not accept
%% files as pathnames (known previous bug)
set_path_file(Config) when is_list(Config) ->
    File=filename:join(proplists:get_value(priv_dir, Config), "testfil"),
    ok=file:write_file(File, list_to_binary("lite data")),
    {error, bad_directory}=code:set_path([File]).

%% Test that a module with the same name as a module in
%% a sticky directory cannot be loaded.
sticky_dir(Config) when is_list(Config) ->
    Pa = filename:dirname(code:which(?MODULE)),
    {ok,Node} = test_server:start_node(sticky_dir, slave, [{args,"-pa "++Pa}]),
    Mods = [code,lists,erlang,init],
    OutDir = filename:join(proplists:get_value(priv_dir, Config), sticky_dir),
    _ = file:make_dir(OutDir),
    Ret = rpc:call(Node, erlang, apply,
		   [fun sticky_compiler/2,[Mods,OutDir]]),
    case Ret of
	[] ->
	    ok;
	Other ->
	    io:format("~p\n", [Other]),
	    ct:fail(failed)
    end,
    test_server:stop_node(Node),
    ok.

sticky_compiler(Files, PrivDir) ->
    code:add_patha(PrivDir),
    Rets = [do_sticky_compile(F, PrivDir) || F <- Files],
    [R || R <- Rets, R =/= ok].

do_sticky_compile(Mod, Dir) ->
    case code:is_sticky(Mod) of
        true ->
            %% Make sure that the module is loaded. A module being sticky
            %% only prevents it from begin reloaded, not from being loaded
            %% from the wrong place to begin with.
            Mod = Mod:module_info(module),
            File = filename:append(Dir, atom_to_list(Mod)),
            Src = io_lib:format("-module(~s).\n"
                                "-export([test/1]).\n"
                                "test(me) -> fail.\n", [Mod]),
            ok = file:write_file(File++".erl", Src),
            case c:c(File, [{outdir,Dir}]) of
                {ok,Module} ->
                    Module:test(me);
                {error,sticky_directory} ->
                    ok
            end;
        false ->
            %% For some reason the module is not sticky
            %% could be that the .erlang file has
            %% unstuck it?
            {Mod, is_not_sticky}
    end.

%% Test that the -pa and -pz options work as expected.
pa_pz_option(Config) when is_list(Config) ->
    DDir = proplists:get_value(data_dir,Config),
    PaDir = filename:join(DDir,"pa"),
    PzDir = filename:join(DDir,"pz"),
    {ok, Node}=test_server:start_node(pa_pz1, slave,
	[{args,
		"-pa " ++ PaDir
		++ " -pz " ++ PzDir}]),
    Ret=rpc:call(Node, code, get_path, []),
    [PaDir|Paths] = Ret,
    [PzDir|_] = lists:reverse(Paths),
    test_server:stop_node(Node),
    {ok, Node2}=test_server:start_node(pa_pz2, slave,
	[{args,
		"-mode embedded " ++ "-pa "
		++ PaDir ++ " -pz " ++ PzDir}]),
    Ret2=rpc:call(Node2, code, get_path, []),
    [PaDir|Paths2] = Ret2,
    [PzDir|_] = lists:reverse(Paths2),
    test_server:stop_node(Node2).

%% add_path, del_path should not cause priv_dir(App) to fail.
add_del_path(Config) when is_list(Config) ->
    DDir = proplists:get_value(data_dir,Config),
    Dir1 = filename:join(DDir,"dummy_app-1.0/ebin"),
    Dir2 = filename:join(DDir,"dummy_app-2.0/ebin"),
    code:add_patha(Dir1),
    PrivDir1 = filename:join(DDir,"dummy_app-1.0/priv"),
    PrivDir1 = code:priv_dir(dummy_app),
    code:add_path(Dir2), % put last in path
    PrivDir1 = code:priv_dir(dummy_app),
    code:del_path(Dir2),
    PrivDir1 = code:priv_dir(dummy_app),
    ok.


clash(Config) when is_list(Config) ->
    DDir = proplists:get_value(data_dir,Config)++"clash/",
    P = code:get_path(),

    %% test non-clashing entries

    true = code:add_path(DDir++"foobar-0.1/ebin"),
    true = code:add_path(DDir++"zork-0.8/ebin"),
    ct:capture_start(),
    ok = code:clash(),
    ct:capture_stop(),
    [OKMsg|_] = ct:capture_get(),
    true = lists:prefix("** Found 0 name clashes", OKMsg),
    true = code:set_path(P),

    %% test clashing entries

    true = code:add_path(DDir++"foobar-0.1/ebin"),
    true = code:add_path(DDir++"foobar-0.1.ez/foobar-0.1/ebin"),
    ct:capture_start(),
    ok = code:clash(),
    ct:capture_stop(),
    [ClashMsg|_] = ct:capture_get(),
    {match, [" hides "]} = re:run(ClashMsg, "\\*\\* .*( hides ).*",
					[{capture,all_but_first,list}]),
    true = code:set_path(P),

    %% test "Bad path can't read"

    Priv = proplists:get_value(priv_dir, Config),
    TmpEzFile = Priv++"foobar-0.tmp.ez",
    {ok, _} = file:copy(DDir++"foobar-0.1.ez", TmpEzFile),
    true = code:add_path(TmpEzFile++"/foobar-0.1/ebin"),
    case os:type() of
        {win32,_} ->
	    %% The file wont be deleted on windows until it's closed, why we
	    %% need to rename instead.
	    ok = file:rename(TmpEzFile,TmpEzFile++".moved");
	 _ ->
    	    ok = file:delete(TmpEzFile)
    end,
    ct:capture_start(),
    ok = code:clash(),
    ct:capture_stop(),
    [BadPathMsg|_] = ct:capture_get(),
    true = lists:prefix("** Bad path can't read", BadPathMsg),
    true = code:set_path(P),
    file:delete(TmpEzFile++".moved"), %% Only effect on windows
    ok.

%% Every module that the code_server uses should be preloaded,
%% this test case verifies that.
ext_mod_dep(Config) when is_list(Config) ->
    xref:start(s),
    xref:set_default(s, [{verbose,false},{warnings,false},
			 {builtins,true},{recurse,true}]),
    xref:set_library_path(s, code:get_path()),
    xref:add_directory(s, filename:join(code:lib_dir(kernel),"ebin")),
    xref:add_directory(s, filename:join(code:lib_dir(stdlib),"ebin")),
    case catch ext_mod_dep2() of
	{'EXIT', Reason} ->
	    xref:stop(s),
	    exit(Reason);
	Else ->
    	    xref:stop(s),
	    case Else of
		ok -> ok;
		_ -> ct:fail(Else)
	    end
    end.

ext_mod_dep2() ->
    Exports0 = code_server:module_info(exports) --
	[{module_info,0},{module_info,1}],
    Exports = [{code_server,M,A} || {M,A} <- Exports0],
    case analyse(Exports, [], [], 0) of
	{_Visited,0} ->
	    ok;
	{_Visited,ErrCnt} ->
	    {not_verified,ErrCnt}
    end.

analyse([], [], Visited, ErrCnt) ->
    {Visited,ErrCnt};
analyse([], [This={M,F,A}|Path], Visited, ErrCnt0) ->
    %% The code_server has been granted to use the following modules,
    %% These modules should be loaded by code.erl before
    %% the code_server is started.
    OK = [erlang, os, prim_file, erl_prim_loader, init, ets,
	  code_server, lists, lists_sort, unicode, binary, filename,
	  gb_sets, gb_trees, hipe_unified_loader, hipe_bifs,
	  erts_code_purger,
	  prim_zip, zlib],
    ErrCnt1 =
	case lists:member(M, OK) or erlang:is_builtin(M,F,A) of
	    true ->
		0;
	    false ->
		check_funs(This, Path)
	end,
    {Visited, ErrCnt1+ErrCnt0};
analyse([MFA|R], Path, Visited0, ErrCnt0) ->
    case lists:member(MFA,Visited0) of
	false ->
	    {Visited,ErrCnt1} = analyse2(MFA, Path, Visited0),
	    analyse(R, Path, Visited, ErrCnt1+ErrCnt0);
	true ->
	    analyse(R, Path, Visited0, ErrCnt0)
    end.

analyse2(MFA = {'$M_EXPR',_, _}, Path, Visited0) ->
    analyse([], [MFA|Path], Visited0, 0);
analyse2(MFA={_,_,_}, Path, Visited0) ->
    {ok, FL} = xref:analyze(s,{call,MFA}),
    analyse(FL, [MFA|Path], my_usort([MFA|Visited0]), 0).

%%%% We need to check these manually...
%% fun's are ok as long as they are defined locally.
check_funs({'$M_EXPR','$F_EXPR',_},
	   [{unicode,characters_to_binary_int,3},
	    {unicode,characters_to_binary,3},
	    {filename,filename_string_to_binary,1}|_]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',_},
	   [{unicode,ml_map,3},
	    {unicode,characters_to_binary_int,3},
	    {unicode,characters_to_binary,3},
	    {filename,filename_string_to_binary,1}|_]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',_},
	   [{unicode,do_o_binary2,2},
	    {unicode,do_o_binary,2},
	    {unicode,o_trans,1},
	    {unicode,characters_to_binary_int,3},
	    {unicode,characters_to_binary,3},
	    {filename,filename_string_to_binary,1}|_]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',_},
	   [{code_server,do_mod_call,4},
	    {code_server,handle_call,3}|_]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',_},
	   [{lists,flatmap,2},
	    {lists,concat,1},
	    {code_server,load_abs,4},
	    {code_server,handle_call,3},
	    {code_server,loop,1}|_]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',_},
	   [{lists,foreach,2},
	    {code_server,stick_dir,3},
	    {code_server,handle_call,3},
	    {code_server,loop,1}|_]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',1},
	   [{lists,all,2},
	    {code_server,is_numstr,1},
	    {code_server,is_vsn,1},
	    {code_server,vsn_to_num,1},
	    {code_server,create_bundle,2},
	    {code_server,choose_bundles,1},
	    {code_server,make_path,2},
	    {code_server,get_user_lib_dirs_1,1},
	    {code_server,get_user_lib_dirs,0},
	    {code_server,init,3},
	    {code_server,start_link,1}]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',1},
	   [{lists,filter,2},
	    {code_server,try_archive_subdirs,3}|_]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',_},
	   [{erlang,apply,2},
	    {erlang,spawn_link,1},
	    {code_server,start_link,1}]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',_},
	   [{erlang,spawn_link,1},{code_server,start_link,1}]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',2},
	   [{hipe_unified_loader,write_words,3} | _]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',2},
	   [{hipe_unified_loader,patch_label_or_labels,4} | _]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',2},
	   [{hipe_unified_loader,sort_and_write,5} | _]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',2},
	   [{lists,foldl,3},
	    {hipe_unified_loader,sort_and_write,5} | _]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',1},
	   [{lists,foreach,2},
	    {hipe_unified_loader,patch_consts,4} | _]) -> 0;
check_funs({'$M_EXPR',warning_msg,2},
	   [{code_server,finish_on_load_report,2} | _]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',1},
	   [{code_server,run,2}|_]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',2},
	   [{code_server,handle_on_load,5}|_]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',2},
	   [{code_server,handle_pending_on_load,4}|_]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',2},
	   [{code_server,finish_on_load_2,3}|_]) -> 0;
%% This is cheating! /raimo
%%
%% check_funs(This = {M,_,_}, Path) ->
%%     case catch atom_to_list(M) of
%% 	[$h,$i,$p,$e | _] ->
%% 	    io:format("hipe_module_ignored(~p, ~p)~n", [This, Path]),
%% 	    0;
%% 	_ ->
%% 	    io:format("not_verified(~p, ~p)~n", [This, Path]),
%% 	    1
%%     end;
check_funs(This, Path) ->
    io:format("not_verified(~p, ~p)~n", [This, Path]),
    1.

my_usort(List) ->
    lists:reverse(uniq(lists:sort(List),[])).

uniq([],A) ->
    A;
uniq([H|T],[]) ->
    uniq(T,[H]);
uniq([H|T],[H|_]=A) ->
    uniq(T,A);
uniq([H|T],A) ->
    uniq(T,[H|A]).


where_is_file(Config) when is_list(Config) ->
    {T,KernelBeamFile} = timer:tc(code, where_is_file, ["kernel.beam"]),
    io:format("Load time: ~w ms~n", [T]),
    KernelEbinDir = filename:dirname(KernelBeamFile),
    AppFile = filename:join(KernelEbinDir, "kernel.app"),
    AppFile = code:where_is_file("kernel.app"),
    non_existing = code:where_is_file("kernel"), % no such file
    ok.

%% Test that stacktrace is deleted when purging a referred module.
purge_stacktrace(Config) when is_list(Config) ->
    code:purge(code_b_test),
    try code_b_test:call(fun(b) -> ok end, a)
    catch
	error:function_clause:Stacktrace ->
	    code:load_file(code_b_test),
	    case Stacktrace of
		      [{?MODULE,_,[a],_},
		       {code_b_test,call,2,_},
		       {?MODULE,purge_stacktrace,1,_}|_] ->
			  false = code:purge(code_b_test)
		  end
    end,
    try code_b_test:call(nofun, 2)
    catch
	error:function_clause:Stacktrace2 ->
	    code:load_file(code_b_test),
	    case Stacktrace2 of
		      [{code_b_test,call,[nofun,2],_},
		       {?MODULE,purge_stacktrace,1,_}|_] ->
			  false = code:purge(code_b_test)
		  end
    end,
    Args = [erlang,error,[badarg]],
    try code_b_test:call(erlang, error, [badarg,Args])
    catch
	error:badarg:Stacktrace3 ->
	    code:load_file(code_b_test),
	    case Stacktrace3 of
		      [{code_b_test,call,Args,_},
		       {?MODULE,purge_stacktrace,1,_}|_] ->
			  false = code:purge(code_b_test)
		  end
    end,
    ok.

mult_lib_roots(Config) when is_list(Config) ->
    DataDir = filename:join(proplists:get_value(data_dir, Config), "mult_lib_roots"),
    mult_lib_compile(DataDir, "my_dummy_app-b/ebin/lists"),
    mult_lib_compile(DataDir,
			   "my_dummy_app-c/ebin/code_SUITE_mult_root_module"),

    %% Set up ERL_LIBS and start a slave node.
    ErlLibs = filename:join(DataDir, "first_root") ++ mult_lib_sep() ++
	filename:join(DataDir, "second_root"),

    {ok,Node} =
	test_server:start_node(mult_lib_roots, slave,
		      [{args,"-env ERL_LIBS "++ErlLibs}]),

    Path0 = rpc:call(Node, code, get_path, []),
    ["."|Path1] = Path0,
    [Kernel|Path2] = Path1,
    [Stdlib|Path3] = Path2,
    mult_lib_verify_lib(Kernel, "kernel"),
    mult_lib_verify_lib(Stdlib, "stdlib"),
    [Lib1,Lib2,Lib3,Lib4,Lib5|Path] = Path3,


    ["first_root/my_dummy_app-a/ebin",
     "first_root/my_dummy_app-b/ebin",
     "first_root/my_dummy_app-c/ebin",
     "second_root/my_dummy_app-d/ebin",
     "second_root/my_dummy_app-e/ebin"] =
	[mult_lib_remove_prefix(E, DataDir) ||
	    E <- lists:sort([Lib1,Lib2,Lib3,Lib4,Lib5])],
    io:format("~p\n", [Path]),

    true = rpc:call(Node, code_SUITE_mult_root_module, works_fine, []),

    ok.

mult_lib_compile(Root, Last) ->
    Mod = list_to_atom(filename:basename(Last)),
    Name = filename:join([Root,"first_root",Last]),
    Dir = filename:dirname(Name),
    {ok,Mod} = compile:file(Name, [report,{outdir,Dir}]),
    ok.

mult_lib_sep() ->
    case os:type() of
	{win32,_} -> ";";
	_ -> ":"
    end.

mult_lib_verify_lib(Path, Expected) ->
    Dir = filename:basename(filename:dirname(Path)),
    true = lists:prefix(Expected, Dir).

mult_lib_remove_prefix([H|T1], [H|T2]) ->
    mult_lib_remove_prefix(T1, T2);
mult_lib_remove_prefix([$/|T], []) -> T.

bad_erl_libs(Config) when is_list(Config) ->
    %% Preserve ERL_LIBS if set.
    BadLibs0 = "/no/such/dir",
    BadLibs =
         case os:getenv("ERL_LIBS") of
             false -> BadLibs0;
             Libs -> BadLibs0 ++ ":" ++ Libs
         end,
    {ok,Node} =
	test_server:start_node(bad_erl_libs, slave, []),
    Code = rpc:call(Node,code,get_path,[]),
    test_server:stop_node(Node),

    {ok,Node2} =
	test_server:start_node(bad_erl_libs, slave,
			       [{args,"-env ERL_LIBS " ++ BadLibs}]),
    Code2 = rpc:call(Node,code,get_path,[]),
    test_server:stop_node(Node2),
    %% Test that code path is not affected by the faulty ERL_LIBS
    Code = Code2,

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Create an archive file containing an application and make use of it.

code_archive(Config) when is_list(Config) ->
    do_code_archive(Config, "code_archive_libs", false).

code_archive2(Config) when is_list(Config) ->
    do_code_archive(Config, "code_archive_libs2", true).

do_code_archive(Config, Root, StripVsn) when is_list(Config) ->
    %% Copy the orig files to priv_dir
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    App = code_archive_dict,
    VsnBase = atom_to_list(App) ++ "-1.0",
    Base =
	case StripVsn of
	    true  -> atom_to_list(App);
	    false -> VsnBase
	end,
    Ext = init:archive_extension(),
    RootDir = filename:join([PrivDir, Root]),
    ok = file:make_dir(RootDir),
    Archive = filename:join([RootDir, VsnBase ++ Ext]),
    {ok, _} = zip:create(Archive, [VsnBase],
			       [{compress, []}, {cwd, DataDir}]),
    {ok, _} = zip:extract(Archive, [{cwd, PrivDir}]),

    case StripVsn of
	true ->
	    ok = file:rename(filename:join([PrivDir, VsnBase]),
				   filename:join([PrivDir, Base]));
	false ->
	    ok
    end,

    io:format("DEBUG: ~p\n", [?LINE]),
    %% Compile the code
    ok = compile_app(PrivDir, Base),

    %% Create the archive
    ok = file:delete(Archive),
    {ok, _} = zip:create(Archive, [Base],
			       [{compress, []}, {cwd, PrivDir}]),

    %% Create a directory and a file outside of the archive.
    OtherFile = filename:join([RootDir,VsnBase,"other","other.txt"]),
    OtherContents = ?MODULE:module_info(md5),
    filelib:ensure_dir(OtherFile),
    ok = file:write_file(OtherFile, OtherContents),

    %% Set up ERL_LIBS and start a slave node.
    {ok, Node} =
	test_server:start_node(code_archive, slave,
		      [{args,"-env ERL_LIBS " ++ RootDir}]),
    CodePath = rpc:call(Node, code, get_path, []),
    AppEbin = filename:join([Archive, Base, "ebin"]),
    io:format("AppEbin: ~p\n", [AppEbin]),
    io:format("CodePath: ~p\n", [CodePath]),
    io:format("Archive: ~p\n", [erl_prim_loader:read_file_info(Archive)]),
    true = lists:member(AppEbin, CodePath),

    %% Start the app
    ok = rpc:call(Node, application, start, [App]),

    %% Get the lib dir for the app.
    AppLibDir = rpc:call(Node, code, lib_dir, [App]),
    io:format("AppLibDir: ~p\n", [AppLibDir]),
    AppLibDir = filename:join(RootDir, VsnBase),

    %% Access the app priv dir
    AppPrivDir = rpc:call(Node, code, priv_dir, [App]),
    AppPrivFile = filename:join([AppPrivDir, "code_archive.txt"]),
    io:format("AppPrivFile: ~p\n", [AppPrivFile]),
    {ok, _Bin, _} =
	rpc:call(Node, erl_prim_loader, get_file, [AppPrivFile]),

    %% Read back the other text file.
    OtherDirPath = rpc:call(Node, code, lib_dir, [App,other]),
    OtherFilePath = filename:join(OtherDirPath, "other.txt"),
    io:format("OtherFilePath: ~p\n", [OtherFilePath]),
    {ok, OtherContents, _} =
	rpc:call(Node, erl_prim_loader, get_file, [OtherFilePath]),

    %% Use the app
    Tab = code_archive_tab,
    Key = foo,
    Val = bar,
    {ok, _Pid} =  rpc:call(Node, App, new, [Tab]),
    error =  rpc:call(Node, App, find, [Tab, Key]),
    ok =  rpc:call(Node, App, store, [Tab, Key, Val]),
    {ok, Val} =  rpc:call(Node, App, find, [Tab, Key]),
    ok =  rpc:call(Node, App, erase, [Tab, Key]),
    error =  rpc:call(Node, App, find, [Tab, Key]),
    ok =  rpc:call(Node, App, erase, [Tab]),

    test_server:stop_node(Node),
    ok.

compile_app(TopDir, AppName) ->
    AppDir = filename:join([TopDir, AppName]),
    SrcDir = filename:join([AppDir, "src"]),
    OutDir = filename:join([AppDir, "ebin"]),
    {ok, Files} = file:list_dir(SrcDir),
    compile_files(Files, SrcDir, OutDir).

compile_files([File | Files], SrcDir, OutDir) ->
    case filename:extension(File) of
	".erl" ->
	    AbsFile = filename:join([SrcDir, File]),
	    case compile:file(AbsFile, [{outdir, OutDir}]) of
		{ok, _Mod} ->
		    compile_files(Files, SrcDir, OutDir);
		Error ->
		    {compilation_error, AbsFile, OutDir, Error}
	    end;
	_ ->
	    compile_files(Files, SrcDir, OutDir)
    end;
compile_files([], _, _) ->
    ok.

%% Test that a boot file with (almost) all of OTP can be used to start an
%% embeddedd system.
big_boot_embedded(Config) when is_list(Config) ->
    {BootArg,AppsInBoot} = create_big_boot(Config),
    {ok, Node} =
	test_server:start_node(big_boot_embedded, slave,
		      [{args,"-boot "++BootArg++" -mode embedded"}]),
    RemoteNodeApps =
	[ {X,Y} || {X,_,Y} <-
		       rpc:call(Node,application,loaded_applications,[]) ],
    true = lists:sort(AppsInBoot) =:=  lists:sort(RemoteNodeApps),
    ok.

on_load(Config) when is_list(Config) ->
    Master = on_load_test_case_process,

    Data = filename:join([proplists:get_value(data_dir, Config),"on_load"]),
    ok = file:set_cwd(Data),
    up_to_date = make:all([{d,'MASTER',Master}]),

    %% Register a name for this process.
    register(Master, self()),

    {_,Ref} = spawn_monitor(fun() ->
					  exit(on_load_a:data())
				  end),
    receive
	{on_load_a,start} -> ok
    end,
    receive
	{on_load_b,start} -> ok
    end,
    receive
	{on_load_c,PidC} -> ok
    end,

    Refs = on_load_massive_spawn(lists:seq(1, 50)),
    receive after 7 -> ok end,

    PidC ! go,

    KernelLibDir = code:lib_dir(kernel),
    receive
	{on_load_c,done} -> ok
    end,
    receive
	{on_load_b,done} -> ok
    end,
    receive
	{on_load_a,KernelLibDir} -> ok
    end,

    receive
	{'DOWN',Ref,process,_,Res} ->
	    [a,b,c] = Res
    end,

    on_load_wait_for_all(Refs),
    receive
	Any ->
	    ct:fail({unexpected,Any})
    after 10 ->
	    ok
    end.

on_load_massive_spawn([_|T]) ->
    {_,Ra} = spawn_monitor(fun() -> [a,b,c] = on_load_a:data() end),
    {_,Rb} = spawn_monitor(fun() -> [b,c] = on_load_b:data() end),
    {_,Rc} = spawn_monitor(fun() -> [c] = on_load_c:data() end),
    [Ra,Rb,Rc|on_load_massive_spawn(T)];
on_load_massive_spawn([]) -> [].

on_load_wait_for_all([Ref|T]) ->
    receive
	{'DOWN',Ref,process,_,normal} ->
	    on_load_wait_for_all(T)
    end;
on_load_wait_for_all([]) -> ok.

on_load_binary(_) ->
    Master = on_load_binary_test_case_process,
    register(Master, self()),

    %% Construct, compile and pretty-print.
    Mod = ?FUNCTION_NAME,
    File = atom_to_list(Mod) ++ ".erl",
    Tree = ?Q(["-module('@Mod@').\n",
	       "-export([ok/0]).\n",
	       "-on_load({init,0}).\n",
	       "init() ->\n",
	       "  '@Master@' ! {on_load_binary,self()},\n",
	       "  receive go -> ok end.\n",
	       "ok() -> true.\n"]),
    {ok,Mod,Bin} = merl:compile(Tree),
    merl:print(Tree),

    {Pid1,Ref1} = spawn_monitor(fun() ->
					code:load_binary(Mod, File, Bin),
					true = on_load_binary:ok()
				end),
    receive {Mod,OnLoadPid} -> ok end,
    {Pid2,Ref2} = spawn_monitor(fun() ->
					true = on_load_binary:ok()
				end),
    erlang:yield(),
    OnLoadPid ! go,
    receive {'DOWN',Ref1,process,Pid1,Exit1} -> ok end,
    receive {'DOWN',Ref2,process,Pid2,Exit2} -> ok end,
    normal = Exit1,
    normal = Exit2,
    true = code:delete(on_load_binary),
    false = code:purge(on_load_binary),
    ok.

on_load_embedded(Config) when is_list(Config) ->
    try
	on_load_embedded_1(Config)
    catch
	throw:{skip,_}=Skip ->
	    Skip
    end.

on_load_embedded_1(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    LinkName = proplists:get_value(link_name, Config),

    %% Link the on_load_app application into the lib directory.
    OnLoadApp = filename:join(DataDir, "on_load_app-1.0"),
    del_link(LinkName),
    io:format("LinkName :~p, OnLoadApp: ~p~n",[LinkName,OnLoadApp]),
    case file:make_symlink(OnLoadApp, LinkName) of
	{error,enotsup} ->
	    throw({skip,"Support for symlinks required"});
	{error,eperm} ->
	    %% On Windows, we may not have permissions to create symlinks.
	    throw({skip,"Support for symlinks required"});
	ok -> ok
    end,

    %% Compile the code.
    OnLoadAppEbin = filename:join(LinkName, "ebin"),
    {ok,_ } = compile:file(filename:join([OnLoadApp,"src",
						"on_load_embedded"]),
				 [{outdir,OnLoadAppEbin}]),

    %% Create and compile a boot file.
    true = code:add_pathz(OnLoadAppEbin),
    Options = case is_source_dir() of
		  true -> [local];
		  false -> []
	      end,
    BootScript = create_boot(Config, Options),
    true = code:del_path(OnLoadAppEbin),

    %% Start the node and check that the on_load function was run.
    {ok,Node} = start_node(on_load_embedded,
				 "-mode embedded -boot " ++ BootScript),
    ok = rpc:call(Node, on_load_embedded, status, []),

    %% Clean up.
    stop_node(Node).

del_link(LinkName) ->
   case file:delete(LinkName) of
       {error,eperm} ->
             file:del_dir(LinkName);
       Other ->
       	     Other
   end.

create_boot(Config, Options) ->
    {ok, OldDir} = file:get_cwd(),
    {LatestDir,LatestName} = create_script(Config),
    ok = file:set_cwd(LatestDir),
    ok = systools:make_script(LatestName, Options),
    ok = file:set_cwd(OldDir),
    filename:join(LatestDir, LatestName).

create_script(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Name = PrivDir ++ "on_load_test",
    Apps = application_controller:which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel, 1, Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib, 1, Apps),
    {ok,Fd} = file:open(Name ++ ".rel", [write]),
    io:format(Fd,
		    "{release, {\"Test release 3\", \"P2A\"}, \n"
		    " {erts, \"9.42\"}, \n"
		    " [{kernel, \"~s\"}, {stdlib, \"~s\"},"
		    " {on_load_app, \"1.0\"}]}.\n",
		    [KernelVer,StdlibVer]),
    file:close(Fd),
    {filename:dirname(Name),filename:basename(Name)}.

create_big_boot(Config) ->
    {ok, OldDir} = file:get_cwd(),
    {Options,Local} = case is_source_dir() of
				true -> {[no_module_tests,local],true};
				_ -> {[no_module_tests],false}
			    end,
    {LatestDir,LatestName,Apps} = create_big_script(Config,Local),
    ok = file:set_cwd(LatestDir),
    ok = systools:make_script(LatestName, Options),
    ok = file:set_cwd(OldDir),
    {filename:join(LatestDir, LatestName),Apps}.

%% The following apps cannot be loaded.
%% hipe .app references (or can reference) files that have no
%% corresponding beam file (if hipe is not enabled).
filter_app("hipe",_) -> false;

%% Dialyzer depends on hipe
filter_app("dialyzer",_) -> false;

%% Orber requires explicit configuration
filter_app("orber",_) -> false;

%% cos* depends on orber
filter_app("cos"++_,_) -> false;

%% ic has a mod instruction in the app file but no corresponding start
%% function
filter_app("ic",_) -> false;

%% Netconf has some dependency that I really do not understand (maybe
%% like orber)
filter_app("netconf",_) -> false;

%% Safe has the same kind of error in the .app file as ic
filter_app("safe",_) -> false;

%% Comte cannot be started in the "usual" way
filter_app("comte",_) -> false;

%% OS_mon does not find its port program when running cerl
filter_app("os_mon",true) -> false;

%% erts is not a "real" app either =/
filter_app("erts",_) -> false;

%% Other apps should be OK.
filter_app(_,_) -> true.

create_big_script(Config,Local) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Name = filename:join(PrivDir,"full_script_test"),
    InitialApplications=application:loaded_applications(),
    %% Applications left loaded by the application suite, unload them!
    UnloadFix=[app0,app1,app2,group_leader,app_start_error],
    [application:unload(Leftover) ||
	      Leftover <- UnloadFix,
	      lists:keymember(Leftover,1,InitialApplications) ],
    %% Now we should have only "real" applications...
    [application:load(list_to_atom(Y))
	|| {match,[Y]} <- [re:run(X,code:lib_dir()++"/"++"([^/-]*).*/ebin",
		[{capture,[1],list},unicode]) ||
	    X <- code:get_path()],filter_app(Y,Local)],
    Apps = [ {N,V} || {N,_,V} <- application:loaded_applications()],
    {ok,Fd} = file:open(Name ++ ".rel", [write]),
    io:format(Fd,
		    "{release, {\"Test release 3\", \"P2A\"}, \n"
		    " {erts, \"9.42\"}, \n"
		    " ~p}.\n",
		    [Apps]),
    file:close(Fd),
    NewlyLoaded =
	application:loaded_applications() -- InitialApplications,
    [ application:unload(N) || {N,_,_} <- NewlyLoaded],
    {filename:dirname(Name),filename:basename(Name),Apps}.

is_source_dir() ->
    filename:basename(code:lib_dir(kernel)) =:= "kernel" andalso
	filename:basename(code:lib_dir(stdlib)) =:= "stdlib".

on_load_errors(Config) when is_list(Config) ->
    Master = on_load_error_test_case_process,
    register(Master, self()),

    Data = filename:join([proplists:get_value(data_dir, Config),"on_load_errors"]),
    ok = file:set_cwd(Data),
    up_to_date = make:all([{d,'MASTER',Master}]),

    do_on_load_error(an_atom),

    error_logger:add_report_handler(?MODULE, self()),

    do_on_load_error({something,terrible,is,wrong}),
    receive
	Any1 ->
	    {_, "The on_load function"++_,
		   [on_load_error,
		    {something,terrible,is,wrong},_]} = Any1
    end,

    do_on_load_error(fail),		%Cause exception.
    receive
	Any2 ->
	    {_, "The on_load function"++_,
		   [on_load_error,{failed,[_|_]},_]} = Any2
    end,

    %% There should be no more messages.
    receive
	Unexpected ->
	    ct:fail({unexpected,Unexpected})
    after 10 ->
	    ok
    end,

    %% Make sure that the code loading functions return the correct
    %% error code.
    Simple = simple_on_load_error,
    SimpleList = atom_to_list(Simple),
    {error,on_load_failure} = code:load_file(Simple),
    {error,on_load_failure} = code:ensure_loaded(Simple),
    {ok,SimpleCode} = file:read_file("simple_on_load_error.beam"),
    {error,on_load_failure} = code:load_binary(Simple, "", SimpleCode),
    {error,on_load_failure} = code:load_abs(SimpleList),
    {error,on_load_failure} = code:load_abs(SimpleList, Simple),

    ok.

do_on_load_error(ReturnValue) ->
    {_,Ref} = spawn_monitor(fun() ->
					  exit(on_load_error:main())
				  end),
    receive {on_load_error,ErrorPid} -> ok end,
    ErrorPid ! ReturnValue,
    receive
	{'DOWN',Ref,process,_,Exit} ->
	    {undef,[{on_load_error,main,[],_}|_]} = Exit
    end.

on_load_update(Config) ->
    {Mod,Code1} = on_load_update_code(1),
    {module,Mod} = code:load_binary(Mod, "", Code1),
    42 = Mod:a(),
    100 = Mod:b(99),
    4 = erlang:trace_pattern({Mod,'_','_'}, true),

    {Mod,Code2} = on_load_update_code(2),
    {error,on_load_failure} = code:load_binary(Mod, "", Code2),
    42 = Mod:a(),
    78 = Mod:b(77),
    {'EXIT',{undef,_}} = (catch Mod:never()),
    4 = erlang:trace_pattern({Mod,'_','_'}, false),

    {Mod,Code3} = on_load_update_code(3),
    {module,Mod} = code:load_binary(Mod, "", Code3),
    100 = Mod:c(),
    {'EXIT',{undef,_}} = (catch Mod:a()),
    {'EXIT',{undef,_}} = (catch Mod:b(10)),
    {'EXIT',{undef,_}} = (catch Mod:never()),

    code:purge(Mod),
    code:delete(Mod),
    code:purge(Mod),
    ok.

on_load_update_code(Version) ->
    Mod = ?FUNCTION_NAME,
    Tree = on_load_update_code_1(Version, Mod),
    io:format("Version ~p", [Version]),
    {ok,Mod,Code} = merl:compile(Tree),
    merl:print(Tree),
    io:nl(),
    {Mod,Code}.

on_load_update_code_1(1, Mod) ->
    ?Q(["-module('@Mod@').\n",
	"-export([a/0,b/1]).\n"
	"-on_load(f/0).\n",
	"f() -> ok.\n",
	"a() -> 42.\n"
	"b(I) -> I+1.\n"]);
on_load_update_code_1(2, Mod) ->
    ?Q(["-module('@Mod@').\n",
	"-export([never/0]).\n"
	"-on_load(f/0).\n",
	"f() -> 42 = '@Mod@':a(), 1 = '@Mod@':b(0), fail.\n",
	"never() -> never.\n"]);
on_load_update_code_1(3, Mod) ->
    ?Q(["-module('@Mod@').\n",
	"-export([c/0]).\n"
	"-on_load(f/0).\n",
	"f() -> ok.\n",
	"c() -> 100.\n"]).

%% Test -on_load while trace feature 'on_load' is enabled (OTP-14612)
on_load_trace_on_load(Config) ->
    %% 'on_load' enables tracing for all newly loaded modules, so we make a dry
    %% run to ensure that ancillary modules like 'merl' won't be loaded during
    %% the actual test.
    on_load_update(Config),

    Papa = self(),
    Tracer = spawn_link(fun F() -> receive M -> Papa ! M end, F() end),
    {tracer,[]} = erlang:trace_info(self(),tracer),
    erlang:trace(self(), true, [call, {tracer, Tracer}]),
    erlang:trace_pattern(on_load, true, []),
    on_load_update(Config),
    erlang:trace_pattern(on_load, false, []),
    erlang:trace(self(), false, [call]),

    Ms = flush(),
    [{trace, Papa, call, {on_load_update_code, a, []}},
     {trace, Papa, call, {on_load_update_code, b, [99]}},
     {trace, Papa, call, {on_load_update_code, c, []}}] = Ms,

    exit(Tracer, normal),
    ok.

flush() ->
    receive M -> [M | flush()]
    after 100 -> []
    end.


on_load_purge(_Config) ->
    Mod = ?FUNCTION_NAME,
    register(Mod, self()),
    Tree = ?Q(["-module('@Mod@').\n",
	       "-on_load(f/0).\n",
	       "loop() -> loop().\n",
	       "f() ->\n",
	       "'@Mod@' ! {self(),spawn(fun loop/0)},\n",
	       "receive Ack -> Ack end.\n"]),
    merl:print(Tree),
    {ok,Mod,Code} = merl:compile(Tree),
    P = spawn(fun() ->
		      exit(code:load_binary(Mod, "", Code))
	      end),
    monitor(process, P),
    receive
	{Pid1,Pid2} ->
	    monitor(process, Pid2),
	    Pid1 ! ack_and_failure,
	    receive
		{'DOWN',_,process,P,Exit1} ->
		    {error,on_load_failure} = Exit1
	    end,
	    receive
		{'DOWN',_,process,Pid2,Exit2} ->
		    io:format("~p\n", [Exit2])
	    after 10000 ->
		    ct:fail(no_down_message)
	    end
    end.

on_load_self_call(_Config) ->
    Mod = ?FUNCTION_NAME,
    register(Mod, self()),
    Tree = ?Q(["-module('@Mod@').\n",
	       "-export([ext/0]).\n",
	       "-on_load(f/0).\n",
	       "f() ->\n",
	       "  '@Mod@' ! (catch '@Mod@':ext()),\n",
	       "  ok.\n",
	       "ext() -> good_work.\n"]),
        merl:print(Tree),
    {ok,Mod,Code} = merl:compile(Tree),

    {'EXIT',{undef,_}} = on_load_do_load(Mod, Code),
    good_work = on_load_do_load(Mod, Code),

    ok.

on_load_do_load(Mod, Code) ->
    spawn(fun() ->
		  {module,Mod} = code:load_binary(Mod, "", Code)
	  end),
    receive
	Any -> Any
    end.

on_load_pending(_Config) ->
    Mod = ?FUNCTION_NAME,
    Tree1 = ?Q(["-module('@Mod@').\n",
		"-on_load(f/0).\n",
		"f() ->\n",
		"  register('@Mod@', self()),\n",
		"  receive _ -> ok end.\n"]),
    merl:print(Tree1),
    {ok,Mod,Code1} = merl:compile(Tree1),

    Tree2 = ?Q(["-module('@Mod@').\n",
		"-export([t/0]).\n",
		"t() -> ok.\n"]),
    merl:print(Tree2),
    {ok,Mod,Code2} = merl:compile(Tree2),

    Self = self(),
    {_,Ref1} =
	spawn_monitor(fun() ->
			      Self ! started1,
			      {module,Mod} = code:load_binary(Mod, "", Code1)
		      end),
    receive started1 -> ok end,
    timer:sleep(10),
    {_,Ref2} =
	spawn_monitor(fun() ->
			      Self ! started2,
			      {module,Mod} = code:load_binary(Mod, "", Code2),
			      ok = Mod:t()
		      end),
    receive started2 -> ok end,
    receive
	Unexpected ->
	    ct:fail({unexpected,Unexpected})
    after 100 ->
	    ok
    end,
    Mod ! go,
    receive
	{'DOWN',Ref1,process,_,normal} -> ok
    end,
    receive
	{'DOWN',Ref2,process,_,normal} -> ok
    end,
    ok = Mod:t(),
    ok.

on_load_deleted(_Config) ->
    Mod = ?FUNCTION_NAME,

    R0 = fun() ->
		 Tree = ?Q(["-module('@Mod@').\n",
			    "-on_load(f/0).\n",
			    "f() -> ok.\n"]),
		 merl:print(Tree),
		 {ok,Mod,Code} = merl:compile(Tree),
		 {module,Mod} = code:load_binary(Mod, "", Code)
	 end,
    delete_before_reload(Mod, R0),
    delete_before_reload(Mod, R0),

    R1 = fun() ->
		 Tree = ?Q(["-module('@Mod@').\n",
			    "-on_load(f/0).\n",
			    "f() -> fail.\n"]),
		 merl:print(Tree),
		 {ok,Mod,Code} = merl:compile(Tree),
		 {error,on_load_failure} = code:load_binary(Mod, "", Code)
	 end,
    delete_before_reload(Mod, R1),
    delete_before_reload(Mod, R1),

    OtherMod = list_to_atom(lists:concat([Mod,"_42"])),
    OtherTree = ?Q(["-module('@OtherMod@').\n"]),
    merl:print(OtherTree),
    {ok,OtherMod,OtherCode} = merl:compile(OtherTree),

    R2 = fun() ->
		 RegName = 'on_load__registered_name',
		 Tree = ?Q(["-module('@Mod@').\n",
			    "-on_load(f/0).\n",
			    "f() ->\n",
			    "  register('@RegName@', self()),\n",
			    "  receive _ -> ok end.\n"]),
		 merl:print(Tree),
		 {ok,Mod,Code} = merl:compile(Tree),
		 spawn(fun() ->
			       {module,Mod} = code:load_binary(Mod, "", Code)
		       end),
		 receive after 1 -> ok end,
		 {module,OtherMod} = code:load_binary(OtherMod, "",
						      OtherCode),
		 RegName ! stop
	 end,
    delete_before_reload(Mod, R2),

    ok.

delete_before_reload(Mod, Reload) ->
    false = check_old_code(Mod),

    Tree1 = ?Q(["-module('@Mod@').\n",
		"-export([f/1]).\n",
		"f(Parent) ->\n",
		"  register('@Mod@', self()),\n",
		"  Parent ! started,\n",
		"  receive _ -> ok end.\n"]),
    merl:print(Tree1),
    {ok,Mod,Code1} = merl:compile(Tree1),

    Self = self(),
    spawn(fun() ->
		  {module,Mod} = code:load_binary(Mod, "", Code1),
		  Mod:f(Self)
	  end),
    receive started -> ok end,

    true = code:delete(Mod),
    true = check_old_code(Mod),

    Reload(),

    %% When loading the the module with the -on_load() function,
    %% the reference to the old code would be lost. Make sure that
    %% the old code is remembered and is still preventing the
    %% purge.
    false = code:soft_purge(Mod),

    %% Get rid of the old code.
    Mod ! stop,
    receive after 1 -> ok end,
    true = code:soft_purge(Mod),

    %% Unload the version of the module with the -on_load() function.
    true = code:delete(Mod),
    true = code:soft_purge(Mod),

    ok.


%% Test that the native code of early loaded modules is loaded.
native_early_modules(Config) when is_list(Config) ->
    case erlang:system_info(hipe_architecture) of
	undefined ->
	    {skip,"Native code support is not enabled"};
	Architecture ->
	    native_early_modules_1(Architecture)
    end.

native_early_modules_1(Architecture) ->
    {lists, ListsBinary, _ListsFilename} = code:get_object_code(lists),
    ChunkName = hipe_unified_loader:chunk_name(Architecture),
    NativeChunk = beam_lib:chunks(ListsBinary, [ChunkName]),
    IsHipeCompiled = case NativeChunk of
        {ok,{_,[{_,Bin}]}} when is_binary(Bin) -> true;
        {error, beam_lib, _} -> false
    end,
    case IsHipeCompiled of
        false ->
	    {skip,"OTP apparently not configured with --enable-native-libs"};
        true ->
	    true = lists:all(fun code:is_module_native/1,
		[ets,file,filename,gb_sets,gb_trees,
		    %%hipe_unified_loader, no_native as workaround
		    lists,os]),
            ok
    end.

%% Test that the mode of the code server is properly retrieved.
get_mode(Config) when is_list(Config) ->
    interactive = code:get_mode().

%% Make sure that the paths for all loaded modules have been normalized.
normalized_paths(_Config) ->
    do_normalized_paths(erlang:loaded()).

do_normalized_paths([M|Ms]) ->
    case code:which(M) of
	Special when is_atom(Special) ->
	    do_normalized_paths(Ms);
	File when is_list(File) ->
	    File = filename:join([File]),
	    do_normalized_paths(Ms)
    end;
do_normalized_paths([]) ->
    ok.

%% Make sure that the extra -mode flags are ignored
mult_embedded_flags(_Config) ->
    Modes = [{" -mode embedded", embedded},
	     {" -mode interactive", interactive},
	     {" -mode invalid", interactive}],

    [ begin
	  {ArgMode, ExpectedMode} = Mode,
	  {ok, Node} = start_node(mode_test, ArgMode),
	  ExpectedMode = rpc:call(Node, code, get_mode, []),
	  true = stop_node(Node)
      end || Mode <- Modes],

    [ begin
	  {ArgIgnoredMode, _} = IgnoredMode,
	  {ArgRelevantMode, ExpectedMode} = RelevantMode,
	  {ok, Node} = start_node(mode_test, ArgRelevantMode ++ ArgIgnoredMode),
	  ExpectedMode = rpc:call(Node, code, get_mode, []),
	  true = stop_node(Node)
      end || IgnoredMode <- Modes, RelevantMode <- Modes],
    ok.

%% Test that module_status/1 behaves as expected
module_status(_Config) ->
    case test_server:is_cover() of
        true ->
            module_status();
        false ->
            %% Make sure that we terminate the cover server.
            try
                module_status()
            after
                cover:stop()
            end
    end.

module_status() ->
    %% basics
    not_loaded = code:module_status(fubar),     % nonexisting
    {file, preloaded} = code:is_loaded(erlang),
    loaded = code:module_status(erlang),        % preloaded
    loaded = code:module_status(?MODULE),       % normal known loaded

    non_existing = code:which(?TESTMOD), % verify dummy name not in path
    code:purge(?TESTMOD), % ensure no previous version in memory
    code:delete(?TESTMOD),
    code:purge(?TESTMOD),

    %% generated code is detected as such
    {ok,?TESTMOD,Bin} = compile:forms(dummy_ast(), []),
    {module,?TESTMOD} = code:load_binary(?TESTMOD,"",Bin),  % no source file
    ok = ?TESTMOD:f(),
    "" = code:which(?TESTMOD), % verify empty string for source file
    loaded = code:module_status(?TESTMOD),

    %% deleting generated code
    true = code:delete(?TESTMOD),
    non_existing = code:which(?TESTMOD), % verify still not in path
    not_loaded = code:module_status(?TESTMOD),

    %% beam file exists but not loaded
    make_source_file(<<"0">>),
    compile_beam(0),
    true = (non_existing =/= code:which(?TESTMOD)), % verify in path
    not_loaded = code:module_status(?TESTMOD),

    %% loading code from disk makes it loaded
    load_code(),
    loaded = code:module_status(?TESTMOD), % loaded

    %% cover compiling a module
    {ok,?TESTMOD} = cover:compile(?TESTMOD),
    {file, cover_compiled} = code:is_loaded(?TESTMOD), % verify cover compiled
    modified = code:module_status(?TESTMOD), % loaded cover code but file exists
    remove_code(),
    removed = code:module_status(?TESTMOD), % removed
    compile_beam(0),
    modified = code:module_status(?TESTMOD), % recreated
    load_code(),
    loaded = code:module_status(?TESTMOD), % loading removes cover status
    code:purge(?TESTMOD),
    true = code:delete(?TESTMOD),
    not_loaded = code:module_status(?TESTMOD), % deleted

    %% recompilation ignores timestamps, only md5 matters
    load_code(),
    compile_beam(1100),
    loaded = code:module_status(?TESTMOD),

    %% modifying module detects different md5
    make_source_file(<<"1">>),
    compile_beam(0),
    modified = code:module_status(?TESTMOD),

    %% loading the modified code from disk makes it loaded
    load_code(),
    loaded = code:module_status(?TESTMOD),

    %% removing and recreating a module with same md5
    remove_code(),
    removed = code:module_status(?TESTMOD),
    compile_beam(0),
    loaded = code:module_status(?TESTMOD),

    case erlang:system_info(hipe_architecture) of
	undefined ->
	    %% no native support
	    ok;
	_ ->
	    %% native chunk is ignored if beam code is already loaded
	    load_code(),
	    loaded = code:module_status(?TESTMOD),
	    false = has_native(?TESTMOD),
	    compile_native(0),
	    BeamMD5 = erlang:get_module_info(?TESTMOD, md5),
	    {ok,{?TESTMOD,BeamMD5}} = beam_lib:md5(?TESTMODOBJ), % beam md5 unchanged
	    loaded = code:module_status(?TESTMOD),

	    %% native code reported as loaded, though different md5 from beam
	    load_code(),
	    true = has_native(?TESTMOD),
	    NativeMD5 = erlang:get_module_info(?TESTMOD, md5),
	    true = (BeamMD5 =/= NativeMD5),
	    loaded = code:module_status(?TESTMOD),

	    %% recompilation ignores timestamps, only md5 matters
	    compile_native(1100), % later timestamp
	    loaded = code:module_status(?TESTMOD),

	    %% modifying native module detects different md5
	    make_source_file(<<"2">>),
	    compile_native(0),
	    modified = code:module_status(?TESTMOD),

	    %% loading the modified native code from disk makes it loaded
	    load_code(),
	    true = has_native(?TESTMOD),
	    NativeMD5_2 = erlang:get_module_info(?TESTMOD, md5),
	    true = (NativeMD5 =/= NativeMD5_2), % verify native md5 changed
	    {ok,{?TESTMOD,BeamMD5_2}} = beam_lib:md5(?TESTMODOBJ),
	    true = (BeamMD5_2 =/= NativeMD5_2), % verify md5 differs from beam
	    loaded = code:module_status(?TESTMOD),

	    %% removing and recreating a native module with same md5
	    remove_code(),
	    removed = code:module_status(?TESTMOD),
	    compile_native(0),
	    loaded = code:module_status(?TESTMOD),

	    %% purging/deleting native module
	    code:purge(?TESTMOD),
	    true = code:delete(?TESTMOD),
	    not_loaded = code:module_status(?TESTMOD)
    end,
    ok.

compile_beam(Sleep) ->
    compile(Sleep, []).

compile_native(Sleep) ->
    compile(Sleep, [native]).

compile(Sleep, Opts) ->
    timer:sleep(Sleep),  % increment compilation timestamp
    {ok,?TESTMOD} = compile:file(?TESTMODSRC, Opts).

load_code() ->
    code:purge(?TESTMOD),
    {module,?TESTMOD} = code:load_file(?TESTMOD).

remove_code() ->
    ok = file:delete(?TESTMODOBJ).

has_native(Module) ->
    case erlang:get_module_info(Module, native_addresses) of
	[] -> false;
	[_|_] -> true
    end.

make_source_file(Body) ->
    ok = file:write_file(?TESTMODSRC, dummy_source(Body)).

dummy_source(Body) ->
    [<<"-module(" ?TESTMODSTR ").\n"
       "-export([f/0]).\n"
       "f() -> ">>, Body, <<".\n">>].

dummy_ast() ->
    dummy_ast(?TESTMODSTR).

dummy_ast(Mod) when is_atom(Mod) ->
    dummy_ast(atom_to_list(Mod));
dummy_ast(ModStr) ->
    [scan_form("-module(" ++ ModStr ++ ")."),
     scan_form("-export([f/0])."),
     scan_form("f() -> ok.")].

scan_form(String) ->
    {ok,Ts,_} = erl_scan:string(String),
    {ok,F} = erl_parse:parse_form(Ts),
    F.

%%-----------------------------------------------------------------
%% error_logger handler.
%% (Copied from stdlib/test/proc_lib_SUITE.erl.)
%%-----------------------------------------------------------------
init(Tester) ->
    {ok, Tester}.

handle_event({warning_msg, _GL, Msg}, Tester) ->
    Tester ! Msg,
    {ok, Tester};
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_, State) ->
    {ok, State}.

handle_call(_Query, State) -> {ok, {error, bad_query}, State}.

terminate(_Reason, State) ->
    State.

%%%
%%% Common utility functions.
%%%

start_node(Name, Param) ->
    test_server:start_node(Name, slave, [{args, Param}]).

stop_node(Node) ->
    test_server:stop_node(Node).
