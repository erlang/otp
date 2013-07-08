%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2013. All Rights Reserved.
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
-module(code_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2]).
-export([set_path/1, get_path/1, add_path/1, add_paths/1, del_path/1,
	 replace_path/1, load_file/1, load_abs/1, ensure_loaded/1,
	 delete/1, purge/1, soft_purge/1, is_loaded/1, all_loaded/1,
	 load_binary/1, dir_req/1, object_code/1, set_path_file/1,
	 upgrade/1,
	 sticky_dir/1, pa_pz_option/1, add_del_path/1,
	 dir_disappeared/1, ext_mod_dep/1, clash/1,
	 load_cached/1, start_node_with_cache/1, add_and_rehash/1,
	 where_is_file_cached/1, where_is_file_no_cache/1,
	 purge_stacktrace/1, mult_lib_roots/1, bad_erl_libs/1,
	 code_archive/1, code_archive2/1, on_load/1, on_load_binary/1,
	 on_load_embedded/1, on_load_errors/1, big_boot_embedded/1,
	 native_early_modules/1, get_mode/1]).

-export([init_per_testcase/2, end_per_testcase/2,
	 init_per_suite/1, end_per_suite/1,
	 sticky_compiler/1]).

%% error_logger
-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).

-export([compile_load/4]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [set_path, get_path, add_path, add_paths, del_path,
     replace_path, load_file, load_abs, ensure_loaded,
     delete, purge, soft_purge, is_loaded, all_loaded,
     load_binary, dir_req, object_code, set_path_file,
     upgrade,
     pa_pz_option, add_del_path, dir_disappeared,
     ext_mod_dep, clash, load_cached, start_node_with_cache,
     add_and_rehash, where_is_file_no_cache,
     where_is_file_cached, purge_stacktrace, mult_lib_roots,
     bad_erl_libs, code_archive, code_archive2, on_load,
     on_load_binary, on_load_embedded, on_load_errors,
     big_boot_embedded, native_early_modules, get_mode].

groups() ->
    [].

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

init_per_testcase(big_boot_embedded, Config) ->
    case catch crypto:start() of
	ok ->
	    init_per_testcase(do_big_boot_embedded, Config);
	_Else ->
	    {skip, "Needs crypto!"}
    end;
init_per_testcase(_Func, Config) ->
    Dog=?t:timetrap(?t:minutes(5)),
    P=code:get_path(),
    P=code:get_path(),
    [{watchdog, Dog}, {code_path, P}|Config].

end_per_testcase(TC, Config) when TC == mult_lib_roots;
				  TC == big_boot_embedded ->
    {ok, HostName} = inet:gethostname(),
    NodeName = list_to_atom(atom_to_list(TC)++"@"++HostName),
    ?t:stop_node(NodeName),
    end_per_testcase(Config);
end_per_testcase(_Func, Config) ->
    end_per_testcase(Config).

end_per_testcase(Config) ->
    code:purge(code_b_test),
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    P=?config(code_path, Config),
    true=code:set_path(P),
    P=code:get_path(),
    ok.

set_path(suite) -> [];
set_path(doc) -> [];
set_path(Config) when is_list(Config) ->
    P = code:get_path(),
    NonExDir = filename:join(?config(priv_dir, Config), ?t:temp_name("hej")),
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

get_path(suite) -> [];
get_path(doc) -> [];
get_path(Config) when is_list(Config) ->
    P = code:get_path(),
    % test that all directories are strings (lists).
    [] = lists:filter(fun
	    (Dir) when is_list(Dir) -> false;
	    (_) -> true
	end, P),
    ok.

add_path(suite) -> [];
add_path(doc) -> [];
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

add_paths(suite) -> [];
add_paths(doc) -> [];
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

del_path(suite) -> [];
del_path(doc) -> [];
del_path(Config) when is_list(Config) ->
    P = code:get_path(),
    test_server:format("Initial code:get_path()=~p~n",[P]),
    {'EXIT',_} = (catch code:del_path(3)),
    false = code:del_path(my_dummy_name),
    false = code:del_path("/kdlk/my_dummy_dir"),
    Dir = filename:join([code:lib_dir(kernel),"ebin"]),
    test_server:format("kernel dir: ~p~n",[Dir]),


    true = code:del_path(kernel),
    NewP = code:get_path(),
    test_server:format("Path after removing 'kernel':~p~n",[NewP]),
    ReferenceP = lists:delete(Dir,P),
    test_server:format("Reference path:~p~n",[ReferenceP]),
    NewP = ReferenceP, % check that dir is deleted

    code:set_path(P),
    true = code:del_path(Dir),
    NewP1 = code:get_path(),
    NewP1 = lists:delete(Dir,P), % check that dir is deleted
    code:set_path(P),
    ok.

replace_path(suite) -> [];
replace_path(doc) -> [];
replace_path(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
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

dir_disappeared(suite) -> [];
dir_disappeared(doc) -> ["OTP-3977"];
dir_disappeared(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Dir = filename:join(PrivDir, "temp"),
    ok = file:make_dir(Dir),
    true = code:add_path(Dir),
    ok = file:del_dir(Dir),
    non_existing = code:which(bubbelskrammel),
    ok.

load_file(suite) -> [];
load_file(doc) -> [];
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

load_abs(suite) -> [];
load_abs(doc) -> [];
load_abs(Config) when is_list(Config) ->
    TestDir = test_dir(),
    {error, nofile} = code:load_abs(TestDir ++ "/duuuumy_mod"),
    {error, badfile} = code:load_abs(TestDir ++ "/code_a_test"),
    {'EXIT', _} = (catch code:load_abs({})),
    {module, code_b_test} = code:load_abs(TestDir ++ "/code_b_test"),
    code:stick_dir(TestDir),
    {error, sticky_directory} = code:load_abs(TestDir ++ "/code_b_test"),
    code:unstick_dir(TestDir),
    ok.

ensure_loaded(suite) -> [];
ensure_loaded(doc) -> [];
ensure_loaded(Config) when is_list(Config) ->
    {module, lists} = code:ensure_loaded(lists),
    case init:get_argument(mode) of
	{ok, [["embedded"]]} ->
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

delete(suite) -> [];
delete(doc) -> [];
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

purge(suite) -> [];
purge(doc) -> [];
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

soft_purge(suite) -> [];
soft_purge(doc) -> [];
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

is_loaded(suite) -> [];
is_loaded(doc) -> [];
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

all_loaded(suite) -> [];
all_loaded(doc) -> [];
all_loaded(Config) when is_list(Config) ->
    case ?t:is_cover() of
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

load_binary(suite) -> [];
load_binary(doc) -> [];
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
    DataDir = ?config(data_dir, Config),

    %%T = [beam, hipe],
    T = [beam],

    [upgrade_do(DataDir, Client, U1, U2, O1, O2)
     || Client<-T, U1<-T, U2<-T, O1<-T, O2<-T],

    ok.

upgrade_do(DataDir, Client, U1, U2, O1, O2) ->
    compile_load(upgrade_client, DataDir, undefined, Client),
    upgrade_client:run(DataDir, U1, U2, O1, O2),
    ok.

compile_load(Mod, Dir, Ver, CodeType) ->
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
	hipe -> [native]
    end,
    CompOpts = [binary, report] ++ Target ++ Version,

    Src = filename:join(Dir, atom_to_list(Mod) ++ ".erl"),
    %io:format("compile:file(~p,~p)\n", [Src, CompOpts]),
    {ok,Mod,Code} = compile:file(Src, CompOpts),
    ObjFile = filename:basename(Src,".erl") ++ ".beam",
    {module,Mod} = code:load_binary(Mod, ObjFile, Code),
    %IsNative = code:is_module_native(Mod),
    ok.

dir_req(suite) -> [];
dir_req(doc) -> [];
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

object_code(suite) -> [];
object_code(doc) -> [];
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

set_path_file(suite) -> [];
set_path_file(doc) -> ["Test that set_path does not accept ",
		       "files as pathnames (known previous bug)"];
set_path_file(Config) when is_list(Config) ->
    File=filename:join(?config(priv_dir, Config), "testfil"),
    ok=file:write_file(File, list_to_binary("lite data")),
    {error, bad_directory}=code:set_path([File]).

sticky_dir(suite) -> [];
sticky_dir(doc) -> ["Test that a module with the same name as a module in ",
		    "a sticky directory cannot be loaded."];
sticky_dir(Config) when is_list(Config) ->
    MyDir=filename:dirname(code:which(?MODULE)),
    {ok, Node}=?t:start_node(sticky_dir, slave,[{args, "-pa \""++MyDir++"\""}]),
    File=filename:join([?config(data_dir, Config), "calendar"]),
    Ret=rpc:call(Node, ?MODULE, sticky_compiler, [File]),
    case Ret of
	fail ->
	    ?t:fail("c:c allowed a sticky module to be compiled and loaded.");
	ok ->
	    ok;
	Other ->
	    test_server:format("Other: ~p",[Other])
    end,
    ?t:stop_node(Node).

sticky_compiler(File) ->
    Compiled=File++code:objfile_extension(),
    Dir=filename:dirname(File),
    code:add_patha(Dir),
    file:delete(Compiled),
    case c:c(File, [{outdir, Dir}]) of
	{ok, Module} ->
	    case catch Module:test(apa) of
		{error, _} ->
		    fail;
		{'EXIT', _} ->
		    ok
	    end;
	Other ->
	    test_server:format("c:c(~p) returned: ~p",[File, Other]),
	    ok
    end.

pa_pz_option(suite) -> [];
pa_pz_option(doc) -> ["Test that the -pa and -pz options work as expected"];
pa_pz_option(Config) when is_list(Config) ->
    DDir = ?config(data_dir,Config),
    PaDir = filename:join(DDir,"pa"),
    PzDir = filename:join(DDir,"pz"),
    {ok, Node}=?t:start_node(pa_pz1, slave,
	[{args,
		"-pa " ++ PaDir
		++ " -pz " ++ PzDir}]),
    Ret=rpc:call(Node, code, get_path, []),
    [PaDir|Paths] = Ret,
    [PzDir|_] = lists:reverse(Paths),
    ?t:stop_node(Node),
    {ok, Node2}=?t:start_node(pa_pz2, slave,
	[{args,
		"-mode embedded " ++ "-pa "
		++ PaDir ++ " -pz " ++ PzDir}]),
    Ret2=rpc:call(Node2, code, get_path, []),
    [PaDir|Paths2] = Ret2,
    [PzDir|_] = lists:reverse(Paths2),
    ?t:stop_node(Node2).

add_del_path(suite) ->
    [];
add_del_path(doc) -> ["add_path, del_path should not cause priv_dir(App) to fail"];
add_del_path(Config) when is_list(Config) ->
    DDir = ?config(data_dir,Config),
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
    DDir = ?config(data_dir,Config)++"clash/",
    P = code:get_path(),
    [TestServerPath|_] = [Path || Path <- code:get_path(),
				  re:run(Path,"test_server/?$",[]) /= nomatch],

    %% test non-clashing entries

    %% remove TestServerPath to prevent clash with test-server path
    true = code:del_path(TestServerPath),
    true = code:add_path(DDir++"foobar-0.1/ebin"),
    true = code:add_path(DDir++"zork-0.8/ebin"),
    test_server:capture_start(),
    ok = code:clash(),
    test_server:capture_stop(),
    [OKMsg|_] = test_server:capture_get(),
    true = lists:prefix("** Found 0 name clashes", OKMsg),
    true = code:set_path(P),

    %% test clashing entries

    %% remove TestServerPath to prevent clash with test-server path
    true = code:del_path(TestServerPath),
    true = code:add_path(DDir++"foobar-0.1/ebin"),
    true = code:add_path(DDir++"foobar-0.1.ez/foobar-0.1/ebin"),
    test_server:capture_start(),
    ok = code:clash(),
    test_server:capture_stop(),
    [ClashMsg|_] = test_server:capture_get(),
    {match, [" hides "]} = re:run(ClashMsg, "\\*\\* .*( hides ).*",
					[{capture,all_but_first,list}]),
    true = code:set_path(P),

    %% test "Bad path can't read"

    %% remove TestServerPath to prevent clash with test-server path
    Priv = ?config(priv_dir, Config),
    true = code:del_path(TestServerPath),
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
    test_server:capture_start(),
    ok = code:clash(),
    test_server:capture_stop(),
    [BadPathMsg|_] = test_server:capture_get(),
    true = lists:prefix("** Bad path can't read", BadPathMsg),
    true = code:set_path(P),
    file:delete(TmpEzFile++".moved"), %% Only effect on windows
    ok.

ext_mod_dep(suite) ->
    [];
ext_mod_dep(doc) ->
    ["Every module that the code_server uses should be preloaded, "
     "this test case verifies that"];
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
		_ -> test_server:fail(Else)
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
% fun's are ok as long as they are defined locally.
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
	   [{code_server,load_native_code,4},
	    {code_server,load_native_code_1,2},
	    {code_server,load_native_code,2},
	    {code_server,try_load_module,4},
	    {code_server,do_load_binary,4},
	    {code_server,handle_call,3},
	    {code_server,loop,1}|_]) -> 0;
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
	    {code_server,try_archive_subdirs,3},
	    {code_server,all_archive_subdirs,1},
	    {code_server,archive_subdirs,1},
	    {code_server,insert_name,3},
	    {code_server,replace_name,2},
	    {code_server,update,2},
	    {code_server,maybe_update,2},
	    {code_server,do_add,4},
	    {code_server,add_path,4},
	    {code_server,handle_call,3},
	    {code_server,loop,1},
	    {code_server,system_continue,3}]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',_},
	   [{erlang,apply,2},
	    {erlang,spawn_link,1},
	    {code_server,start_link,1}]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',_},
	   [{erlang,spawn_link,1},{code_server,start_link,1}]) -> 0;
check_funs({'$M_EXPR',module_info,1},
	   [{hipe_unified_loader,patch_to_emu_step1,1} | _]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',2},
	   [{lists,foldl,3},
	    {hipe_unified_loader,sort_and_write,4} | _]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',1},
	   [{lists,foreach,2},
	    {hipe_unified_loader,patch_consts,3} | _]) -> 0;
check_funs({'$M_EXPR','$F_EXPR',1},
	   [{lists,foreach,2},
	    {hipe_unified_loader,mark_referred_from,1},
	    {hipe_unified_loader,get_refs_from,2}| _]) -> 0;
check_funs({'$M_EXPR',warning_msg,2},
	   [{code_server,finish_on_load_report,2} | _]) -> 0;
%% This is cheating! /raimo
%%
%% check_funs(This = {M,_,_}, Path) ->
%%     case catch atom_to_list(M) of
%% 	[$h,$i,$p,$e | _] ->
%% 	    test_server:format("hipe_module_ignored(~p, ~p)~n", [This, Path]),
%% 	    0;
%% 	_ ->
%% 	    test_server:format("not_verified(~p, ~p)~n", [This, Path]),
%% 	    1
%%     end;
check_funs(This, Path) ->
    test_server:format("not_verified(~p, ~p)~n", [This, Path]),
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


load_cached(suite) ->
    [];
load_cached(doc) ->
    [];
load_cached(Config) when is_list(Config) ->
    Priv = ?config(priv_dir, Config),
    WD = filename:dirname(code:which(?MODULE)),
    {ok,Node} =
	?t:start_node(code_cache_node, peer, [{args,
					       "-pa \"" ++ WD ++ "\""},
					      {erl, [this]}]),
    CCTabCreated = fun(Tab) ->
			   case ets:info(Tab, name) of
			       code_cache -> true;
			       _ -> false
			   end
		   end,
    Tabs = rpc:call(Node, ets, all, []),
    case rpc:call(Node, lists, any, [CCTabCreated,Tabs]) of
	true ->
	    ?t:stop_node(Node),
	    ?t:fail("Code cache should not be active!");
	false ->
	    ok
    end,
    rpc:call(Node, code, del_path, [Priv]),
    rpc:call(Node, code, add_pathz, [Priv]),

    FullModName = Priv ++ "/code_cache_test",
    {ok,Dev} = file:open(FullModName ++ ".erl", [write]),
    io:format(Dev, "-module(code_cache_test). -export([a/0]). a() -> ok.~n", []),
    ok = file:close(Dev),
    {ok,code_cache_test} = compile:file(FullModName, [{outdir,Priv}]),

    F = fun load_loop/2,
    N = 1000,
    {T0,T1} = rpc:call(Node, erlang, apply, [F, [N,code_cache_test]]),
    TNoCache = now_diff(T1, T0),
    rpc:call(Node, code, rehash, []),
    {T2,T3} = rpc:call(Node, erlang, apply, [F, [N,code_cache_test]]),
    TCache = now_diff(T3, T2),
    AvgNoCache = TNoCache/N,
    AvgCache = TCache/N,
    io:format("Avg. load time (no_cache/cache): ~w/~w~n", [AvgNoCache,AvgCache]),
    ?t:stop_node(Node),
    if AvgNoCache =< AvgCache ->
	    ?t:fail("Cache not working properly.");
       true ->
	    ok
    end.

load_loop(N, M) ->
    load_loop(N, M, now()).
load_loop(0, _M, T0) ->
    {T0,now()};
load_loop(N, M, T0) ->
    code:load_file(M),
    code:delete(M),
    code:purge(M),
    load_loop(N-1, M, T0).

now_diff({A2, B2, C2}, {A1, B1, C1}) ->
    ((A2-A1)*1000000 + B2-B1)*1000000 + C2-C1.

start_node_with_cache(suite) ->
    [];
start_node_with_cache(doc) ->
    [];
start_node_with_cache(Config) when is_list(Config) ->
    {ok,Node} =
	?t:start_node(code_cache_node, peer, [{args,
					       "-code_path_cache"},
					      {erl, [this]}]),
    Tabs = rpc:call(Node, ets, all, []),
    io:format("Tabs: ~w~n", [Tabs]),
    CCTabCreated = fun(Tab) ->
			   case rpc:call(Node, ets, info, [Tab,name]) of
			       code_cache -> true;
			       _ -> false
			   end
		   end,
    true = lists:any(CCTabCreated, Tabs),
    ?t:stop_node(Node),
    ok.

add_and_rehash(suite) ->
    [];
add_and_rehash(doc) ->
    [];
add_and_rehash(Config) when is_list(Config) ->
    Priv = ?config(priv_dir, Config),
    WD = filename:dirname(code:which(?MODULE)),
    {ok,Node} =
	?t:start_node(code_cache_node, peer, [{args,
					       "-pa \"" ++ WD ++ "\""},
					      {erl, [this]}]),
    CCTabCreated = fun(Tab) ->
			   case ets:info(Tab, name) of
			       code_cache -> true;
			       _ -> false
			   end
		   end,
    Tabs0 = rpc:call(Node, ets, all, []),
    case rpc:call(Node, lists, any, [CCTabCreated,Tabs0]) of
	true ->
	    ?t:stop_node(Node),
	    ?t:fail("Code cache should not be active!");
	false ->
	    ok
    end,
    ok = rpc:call(Node, code, rehash, []),	             % create cache
    Tabs1 = rpc:call(Node, ets, all, []),
    true = rpc:call(Node, lists, any, [CCTabCreated,Tabs1]), % cache table created
    ok = rpc:call(Node, code, rehash, []),
    OkDir = filename:join(Priv, ""),
    BadDir = filename:join(Priv, "guggemuffsussiputt"),
    CP = [OkDir | rpc:call(Node, code, get_path, [])],
    true = rpc:call(Node, code, set_path, [CP]),
    CP1 = [BadDir | CP],
    {error,_} = rpc:call(Node, code, set_path, [CP1]),
    true = rpc:call(Node, code, del_path, [OkDir]),
    true = rpc:call(Node, code, add_path, [OkDir]),
    true = rpc:call(Node, code, add_path, [OkDir]),
    {error,_} = rpc:call(Node, code, add_path, [BadDir]),
    ok = rpc:call(Node, code, rehash, []),

    ?t:stop_node(Node),
    ok.

where_is_file_no_cache(suite) ->
    [];
where_is_file_no_cache(doc) ->
    [];
where_is_file_no_cache(Config) when is_list(Config) ->
    {T,KernelBeamFile} = timer:tc(code, where_is_file, ["kernel.beam"]),
    io:format("Load time: ~w ms~n", [T]),
    KernelEbinDir = filename:dirname(KernelBeamFile),
    AppFile = filename:join(KernelEbinDir, "kernel.app"),
    AppFile = code:where_is_file("kernel.app"),
    non_existing = code:where_is_file("kernel"), % no such file
    ok.

where_is_file_cached(suite) ->
    [];
where_is_file_cached(doc) ->
    [];
where_is_file_cached(Config) when is_list(Config) ->
    {ok,Node} =
	?t:start_node(code_cache_node, peer, [{args,
					       "-code_path_cache"},
					      {erl, [this]}]),
    Tabs = rpc:call(Node, ets, all, []),
    io:format("Tabs: ~w~n", [Tabs]),
    CCTabCreated = fun(Tab) ->
			   case rpc:call(Node, ets, info, [Tab,name]) of
			       code_cache -> true;
			       _ -> false
			   end
		   end,
    true = lists:any(CCTabCreated, Tabs),
    KernelBeamFile = rpc:call(Node, code, where_is_file, ["kernel.beam"]),
    {T,KernelBeamFile} = rpc:call(Node, timer, tc, [code,where_is_file,["kernel.beam"]]),
    io:format("Load time: ~w ms~n", [T]),
    KernelEbinDir = rpc:call(Node, filename, dirname, [KernelBeamFile]),
    AppFile = rpc:call(Node, filename, join, [KernelEbinDir,"kernel.app"]),
    AppFile = rpc:call(Node, code, where_is_file, ["kernel.app"]),
    non_existing = rpc:call(Node, code, where_is_file, ["kernel"]), % no such file
    ?t:stop_node(Node),
    ok.


purge_stacktrace(suite) ->
    [];
purge_stacktrace(doc) ->
    ["Test that stacktrace is deleted when purging a referred module"];
purge_stacktrace(Config) when is_list(Config) ->
    code:purge(code_b_test),
    try code_b_test:call(fun(b) -> ok end, a)
    catch
	error:function_clause ->
	    code:load_file(code_b_test),
	    case erlang:get_stacktrace() of
		      [{?MODULE,_,[a],_},
		       {code_b_test,call,2,_},
		       {?MODULE,purge_stacktrace,1,_}|_] ->
			  false = code:purge(code_b_test),
			  [] = erlang:get_stacktrace()
		  end
    end,
    try code_b_test:call(nofun, 2)
    catch
	error:function_clause ->
	    code:load_file(code_b_test),
	    case erlang:get_stacktrace() of
		      [{code_b_test,call,[nofun,2],_},
		       {?MODULE,purge_stacktrace,1,_}|_] ->
			  false = code:purge(code_b_test),
			  [] = erlang:get_stacktrace()
		  end
    end,
    Args = [erlang,error,[badarg]],
    try code_b_test:call(erlang, error, [badarg,Args])
    catch
	error:badarg ->
	    code:load_file(code_b_test),
	    case erlang:get_stacktrace() of
		      [{code_b_test,call,Args,_},
		       {?MODULE,purge_stacktrace,1,_}|_] ->
			  false = code:purge(code_b_test),
			  [] = erlang:get_stacktrace()
		  end
    end,
    ok.

mult_lib_roots(Config) when is_list(Config) ->
    DataDir = filename:join(?config(data_dir, Config), "mult_lib_roots"),
    mult_lib_compile(DataDir, "my_dummy_app-b/ebin/lists"),
    mult_lib_compile(DataDir,
			   "my_dummy_app-c/ebin/code_SUITE_mult_root_module"),

    %% Set up ERL_LIBS and start a slave node.
    ErlLibs = filename:join(DataDir, "first_root") ++ mult_lib_sep() ++
	filename:join(DataDir, "second_root"),

    {ok,Node} =
	?t:start_node(mult_lib_roots, slave,
		      [{args,"-env ERL_LIBS "++ErlLibs}]),

    TSPath = filename:dirname(code:which(test_server)),
    Path0 = rpc:call(Node, code, get_path, []),
    [TSPath,"."|Path1] = Path0,
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
    {ok,Node} =
	?t:start_node(mult_lib_roots, slave,
		      [{args,"-env ERL_LIBS "}]),

    ?t:stop_node(Node),

    {ok,Node2} =
	?t:start_node(mult_lib_roots, slave,
		      [{args,"-env ERL_LIBS /no/such/dir"}]),

    ?t:stop_node(Node2),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Create an archive file containing an application and make use of it.

code_archive(Config) when is_list(Config) ->
    do_code_archive(Config, "code_archive_libs", false).

code_archive2(Config) when is_list(Config) ->
    do_code_archive(Config, "code_archive_libs2", true).

do_code_archive(Config, Root, StripVsn) when is_list(Config) ->
    %% Copy the orig files to priv_dir
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
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

    %% Set up ERL_LIBS and start a slave node.
    {ok, Node} =
	?t:start_node(code_archive, slave,
		      [{args,"-env ERL_LIBS " ++ RootDir}]),
    CodePath = rpc:call(Node, code, get_path, []),
    AppEbin = filename:join([Archive, Base, "ebin"]),
    io:format("AppEbin: ~p\n", [AppEbin]),
    io:format("CodePath: ~p\n", [CodePath]),
    io:format("Archive: ~p\n", [erl_prim_loader:read_file_info(Archive)]),
    true = lists:member(AppEbin, CodePath),

    %% Start the app
    ok = rpc:call(Node, application, start, [App]),

    %% Access the app priv dir
    AppPrivDir = rpc:call(Node, code, priv_dir, [App]),
    AppPrivFile = filename:join([AppPrivDir, "code_archive.txt"]),
    io:format("AppPrivFile: ~p\n", [AppPrivFile]),
    {ok, _Bin, _Path} =
	rpc:call(Node, erl_prim_loader, get_file, [AppPrivFile]),

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

    ?t:stop_node(Node),
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

big_boot_embedded(suite) ->
    [];
big_boot_embedded(doc) ->
    ["Test that a boot file with (almost) all of OTP can be used to start an"
     " embeddedd system."];
big_boot_embedded(Config) when is_list(Config) ->
    {BootArg,AppsInBoot} = create_big_boot(Config),
    {ok, Node} =
	?t:start_node(big_boot_embedded, slave,
		      [{args,"-boot "++BootArg++" -mode embedded"}]),
    RemoteNodeApps =
	[ {X,Y} || {X,_,Y} <-
		       rpc:call(Node,application,loaded_applications,[]) ],
    true = lists:sort(AppsInBoot) =:=  lists:sort(RemoteNodeApps),
    ok.

on_load(Config) when is_list(Config) ->
    Master = on_load_test_case_process,

    Data = filename:join([?config(data_dir, Config),"on_load"]),
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
	    ?t:fail({unexpected,Any})
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
    Mod = on_load_binary,
    File = atom_to_list(Mod) ++ ".erl",
    Forms = [{attribute,1,file,{File,1}},
	     {attribute,1,module,Mod},
	     {attribute,2,export,[{ok,0}]},
	     {attribute,3,on_load,{init,0}},
	     {function,5,init,0,
	      [{clause,5,[],[],
		[{op,6,'!',
		  {atom,6,Master},
		  {tuple,6,[{atom,6,Mod},{call,6,{atom,6,self},[]}]}},
		 {'receive',7,[{clause,8,[{atom,8,go}],[],[{atom,8,ok}]}]}]}]},
	     {function,11,ok,0,[{clause,11,[],[],[{atom,11,true}]}]}],
    {ok,Mod,Bin} = compile:forms(Forms, [report]),
    [io:put_chars(erl_pp:form(F)) || F <- Forms],

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
    DataDir = ?config(data_dir, Config),

    %% Link the on_load_app application into the lib directory.
    LibRoot = code:lib_dir(),
    LinkName = filename:join(LibRoot, "on_load_app-1.0"),
    OnLoadApp = filename:join(DataDir, "on_load_app-1.0"),
    del_link(LinkName),
    io:format("LinkName :~p, OnLoadApp: ~p~n",[LinkName,OnLoadApp]),
    case file:make_symlink(OnLoadApp, LinkName) of
	{error,enotsup} ->
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
    stop_node(Node),
    ok = del_link(LinkName).

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
    PrivDir = ?config(priv_dir, Config),
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

% The following apps cannot be loaded
% hipe .app references (or can reference) files that have no
% corresponding beam file (if hipe is not enabled)
filter_app("hipe",_) -> false;
% Dialyzer and typer depends on hipe
filter_app("dialyzer",_) -> false;
filter_app("typer",_) -> false;
% Orber requires explicit configuration
filter_app("orber",_) -> false;
% cos* depends on orber
filter_app("cos"++_,_) -> false;
% ic has a mod instruction in the app file but no corresponding start function
filter_app("ic",_) -> false;
% Netconf has some dependency that I really do not understand (maybe like orber)
filter_app("netconf",_) -> false;
% Safe has the same kind of error in the .app file as ic
filter_app("safe",_) -> false;
% Comte cannot be started in the "usual" way
filter_app("comte",_) -> false;
% OS_mon does not find it's port program when running cerl
filter_app("os_mon",true) -> false;
% erts is not a "real" app either =/
filter_app("erts",_) -> false;
% Other apps should be OK.
filter_app(_,_) -> true.
create_big_script(Config,Local) ->
    PrivDir = ?config(priv_dir, Config),
    Name = filename:join(PrivDir,"full_script_test"),
    InitialApplications=application:loaded_applications(),
    %% Applications left loaded by the application suite, unload them!
    UnloadFix=[app0,app1,app2,group_leader,app_start_error],
    [application:unload(Leftover) ||
	      Leftover <- UnloadFix,
	      lists:keymember(Leftover,1,InitialApplications) ],
    %% Now we should have only "real" applications...
    [application:load(list_to_atom(Y)) || {match,[Y]} <- [ re:run(X,code:lib_dir()++"/"++"([^/-]*).*/ebin",[{capture,[1],list}]) || X <- code:get_path()],filter_app(Y,Local)],
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

    Data = filename:join([?config(data_dir, Config),"on_load_errors"]),
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
	    ?t:fail({unexpected,Unexpected})
    after 10 ->
	    ok
    end,

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

native_early_modules(suite) -> [];
native_early_modules(doc) -> ["Test that the native code of early loaded modules is loaded"];
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

get_mode(suite) -> [];
get_mode(doc) -> ["Test that the mode of the code server is properly retrieved"];
get_mode(Config) when is_list(Config) ->
    interactive = code:get_mode().

%%-----------------------------------------------------------------
%% error_logger handler.
%% (Copied from stdlib/test/proc_lib_SUITE.erl.)
%%-----------------------------------------------------------------
init(Tester) ->
    {ok, Tester}.

handle_event({error, _GL, {emulator, _, _}}, Tester) ->
    {ok, Tester};
handle_event({error, _GL, Msg}, Tester) ->
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
    ?t:start_node(Name, slave, [{args, Param}]).

stop_node(Node) ->
    ?t:stop_node(Node).
