%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2017. All Rights Reserved.
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

-module(multi_load_SUITE).
-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 basic_atomic_load/1,basic_errors/1,sticky_dir/1,
	 on_load_failing/1,ensure_modules_loaded/1,
	 native_code/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("syntax_tools/include/merl.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [basic_atomic_load,basic_errors,sticky_dir,on_load_failing,
     ensure_modules_loaded,native_code].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

basic_atomic_load(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, multi_load_sticky_dir),
    _ = file:make_dir(Dir),

    OldPath = code:get_path(),
    try
	code:add_patha(Dir),
	do_basic(Dir)
    after
	code:set_path(OldPath)
    end,

    ok.

do_basic(Dir) ->
    MsVer1_0 = make_modules(5, versioned_module(1)),
    MsVer1 = [{M,filename:absname(F, Dir),Bin} || {M,F,Bin} <- MsVer1_0],
    _ = [ok = file:write_file(F, Bin) || {_,F,Bin} <- MsVer1],

    Ms = [M || {M,_,_} <- MsVer1],
    [] = [loaded || M <- Ms, is_loaded(M)],

    ok = code:atomic_load(Ms),
    _ = [1 = M:M() || M <- Ms],
    _ = [F = code:which(M) || {M,F,_} <- MsVer1],
    [] = [not_loaded || M <- Ms, not is_loaded(M)],

    MsVer2 = update_modules(Ms, versioned_module(2)),
    {ok,Prepared} = code:prepare_loading(MsVer2),
    ok = code:finish_loading(Prepared),
    _ = [2 = M:M() || M <- Ms],
    _ = [F = code:which(M) || {M,F,_} <- MsVer2],
    [] = [not_loaded || M <- Ms, not is_loaded(M)],

    MsVer3 = update_modules(Ms, versioned_module(2)),
    NotPurged = lists:sort([{M,not_purged} || M <- Ms]),
    NotPurged = atomic_load_error(MsVer3, true),

    ok.

versioned_module(Ver) ->
    fun(Mod) ->
	    ?Q(["-module('@Mod@').\n",
		"-export(['@Mod@'/0]).\n",
		"'@Mod@'() -> _@Ver@.\n"])
    end.

basic_errors(_Config) ->
    atomic_load_fc([42]),
    atomic_load_fc([{"mod","file","bin"}]),

    finish_loading_fc(atom),
    {ok,{PrepTag,_}} = code:prepare_loading([code]),
    finish_loading_fc({PrepTag,[x]}),
    finish_loading_fc({PrepTag,[{m,{<<>>,"",<<>>}}]}),
    Prep = prepared_with_wrong_magic_bin(),
    finish_loading_fc(Prep),

    [{x,badfile}] = atomic_load_error([{x,"x",<<"bad">>}], false),
    [{a,badfile},{some_nonexistent_file,nofile}] =
	atomic_load_error([some_nonexistent_file,{a,"a",<<>>}],
			  false),

    %% Modules mentioned more than once.
    Mods = make_modules(2, fun basic_module/1),
    Ms = [M || {M,_,_} <- Mods],
    DupMods = Mods ++ [mnesia] ++ Mods ++ [mnesia],
    DupErrors0 = lists:sort([mnesia|Ms]),
    DupErrors = [{M,duplicated} || M <- DupErrors0],
    DupErrors = atomic_load_error(DupMods, false),

    ok.

atomic_load_fc(L) ->
    {'EXIT',{function_clause,[{code,atomic_load,[L],_}|_]}} =
	(catch code:atomic_load(L)),
    {'EXIT',{function_clause,[{code,prepare_loading,[L],_}|_]}} =
	(catch code:prepare_loading(L)).

finish_loading_fc(Term) ->
    {'EXIT',{function_clause,[{code,finish_loading,[Term],_}|_]}} =
	(catch code:finish_loading(Term)).

prepared_with_wrong_magic_bin() ->
    {ok,Prep} = code:prepare_loading([?MODULE]),
    prep_magic(Prep).

prep_magic([H|T]) ->
    [prep_magic(H)|prep_magic(T)];
prep_magic(Tuple) when is_tuple(Tuple) ->
    L = prep_magic(tuple_to_list(Tuple)),
    list_to_tuple(L);
prep_magic(Ref) when is_reference(Ref) ->
    try erlang:has_prepared_code_on_load(Ref) of
	false ->
	    %% Create a different kind of magic ref.
	    ets:match_spec_compile([{'_',[true],['$_']}])
    catch
	_:_ ->
	    Ref
    end;
prep_magic(Other) ->
    Other.

sticky_dir(_Config) ->
    Mod0 = make_module(lists, fun basic_module/1),
    Mod1 = make_module(gen_server, fun basic_module/1),
    Ms = [Mod0,Mod1],
    SD = sticky_directory,
    StickyErrors = [{gen_server,SD},{lists,SD}],
    StickyErrors = atomic_load_error(Ms, true),

    ok.

on_load_failing(_Config) ->
    OnLoad = make_modules(1, fun on_load_module/1),
    [{OnLoadMod,_,_}] = OnLoad,
    Ms = make_modules(10, fun basic_module/1) ++ OnLoad,

    %% Fail because there is a module with on_load in the list.
    on_load_failure(OnLoadMod, Ms),
    on_load_failure(OnLoadMod, [lists:last(Ms)]),

    %% Fail because there already is a pending on_load.
    [{HangingOnLoad,_,_}|_] = Ms,
    spawn_hanging_on_load(HangingOnLoad),
    NoOnLoadMs = lists:droplast(Ms),
    {error,[{HangingOnLoad,pending_on_load}]} =
	code:atomic_load(NoOnLoadMs),
    hanging_on_load ! stop_hanging_and_unload,

    ok.

on_load_failure(OnLoadMod, Ms) ->
    [{OnLoadMod,on_load_not_allowed}] = atomic_load_error(Ms, false).

spawn_hanging_on_load(Mod) ->
    {Mod,Name,Bin} = make_module(Mod, "unknown",
				 fun(_) ->
					 hanging_on_load_module(Mod)
				 end),
    spawn_link(fun() ->
		       {error,on_load_failure} =
			   code:load_binary(Mod, Name, Bin)
	       end).

hanging_on_load_module(Mod) ->
    ?Q(["-module('@Mod@').\n",
	"-on_load(hang/0).\n",
	"hang() ->\n"
	"  register(hanging_on_load, self()),\n"
	"  receive _ -> unload end.\n"]).

ensure_modules_loaded(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, multi_load_ensure_modules_loaded),
    _ = file:make_dir(Dir),

    OldPath = code:get_path(),
    try
	code:add_patha(Dir),
	do_ensure_modules_loaded(Dir)
    after
	code:set_path(OldPath)
    end,

    ok.

do_ensure_modules_loaded(Dir) ->
    %% Create a dummy "lists" module and place it in our code path.
    {lists,ListsFile,ListsCode} = make_module(lists, fun basic_module/1),
    ok = file:write_file(filename:absname(ListsFile, Dir), ListsCode),
    {error,sticky_directory} = code:load_file(lists),

    %% Make a new module that we can load.
    Mod = make_module_file(Dir, fun basic_module/1),
    false = is_loaded(Mod),

    %% Make a new module with an on_load function.
    OLMod = make_module_file(Dir, fun on_load_module/1),
    false = is_loaded(OLMod),

    %% lists should not be loaded again; Mod and OLMod should be
    %% loaded.  ?MODULE should not be reloaded, but there is no easy
    %% way to test that.  Repeating modules is OK.
    ok = code:ensure_modules_loaded([?MODULE,lists,Mod,OLMod,
				     Mod,OLMod,Mod,lists]),
    last = lists:last([last]),
    true = is_loaded(Mod),
    ok = Mod:Mod(),
    true = is_loaded(OLMod),
    _ = OLMod:module_info(),

    %% Unload the modules that were loaded.
    [begin
	 code:purge(M),
	 code:delete(M),
	 code:purge(M),
	 false = is_loaded(M)
     end || M <- [Mod,OLMod]],

    %% If there are some errors, all other modules should be loaded
    %% anyway.
    [{BadMod,BadFile,_}] = make_modules(1, fun basic_module/1),
    ok = file:write_file(filename:absname(BadFile, Dir), <<"bad_code">>),
    BadOLMod = make_module_file(Dir, fun failing_on_load_module/1),
    BadEgg = bad__egg,
    NativeMod = a_native_module,
    NativeModFile = atom_to_list(NativeMod) ++ ".beam",
    {NativeMod,_,NativeCode} = make_module(NativeMod, NativeModFile,
					   fun basic_module/1, [native]),
    ok = file:write_file(filename:absname(NativeModFile, Dir), NativeCode),
    ModulesToLoad = [OLMod,?MODULE,Mod,BadOLMod,NativeMod,
		     BadEgg,BadMod,lists],
    {error,Error0} = code:ensure_modules_loaded(ModulesToLoad),
    Error = lists:sort([{BadEgg,nofile},
			{BadMod,badfile},
			{BadOLMod,on_load_failure}]),
    Error = lists:sort(Error0),
    true = is_loaded(Mod),
    true = is_loaded(OLMod),
    true = is_loaded(NativeMod),

    ModuleNative = case erlang:system_info(hipe_architecture) of
		       undefined -> false;
		       _ -> true
		   end,
    ModuleNative = NativeMod:module_info(native),

    ok.

failing_on_load_module(Mod) ->
    ?Q(["-module('@Mod@').\n",
	"-on_load(f/0).\n",
	"f() -> fail.\n"]).

native_code(_Config) ->
    case erlang:system_info(hipe_architecture) of
	undefined ->
	    {skip,"No native support"};
	_ ->
	    do_native_code()
    end.

do_native_code() ->
    CalledMod = native_called_module,
    CallingMod = native_calling_module,

    %% Create a module in native code that calls another module.
    CallingMod = make_and_load(CallingMod,
			       calling_module_fun(CalledMod),
			       [native]),

    %% Create a threaded-code module.
    _ = make_and_load(CalledMod, called_module_fun(42), []),
    42 = CallingMod:call(),

    %% Now replace it with a changed module in native code.
    code:purge(CalledMod),
    make_and_load(CalledMod, called_module_fun(43), [native]),
    true = test_server:is_native(CalledMod),
    43 = CallingMod:call(),

    %% Reload the called module and call it.
    code:purge(CalledMod),
    ModVer3 = make_module(CalledMod, "", called_module_fun(changed)),
    ok = code:atomic_load([ModVer3]),
    false = test_server:is_native(CalledMod),
    changed = CallingMod:call(),
    code:purge(CalledMod),

    ok.

make_and_load(Mod, Fun, Opts) ->
    {Mod,_,Code} = make_module(Mod, "", Fun, Opts),
    {module,Mod} = code:load_binary(Mod, "", Code),
    Mod.

calling_module_fun(Called) ->
    fun(Mod) ->
	    ?Q(["-module('@Mod@').\n",
		"-export([call/0]).\n",
		"call() -> _@Called@:f().\n"])
    end.

called_module_fun(Ret) ->
    fun(Mod) ->
	    ?Q(["-module('@Mod@').\n",
		"-export([f/0]).\n",
		"f() -> _@Ret@.\n"])
    end.

%%%
%%% Common utilities
%%%

atomic_load_error(Modules, ErrorInFinishLoading) ->
    {error,Errors0} = code:atomic_load(Modules),
    {Errors1,Bool} =
	case code:prepare_loading(Modules) of
	    {ok,Prepared} ->
		{error,Es0} = code:finish_loading(Prepared),
		{Es0,true};
	     {error,Es0} ->
		{Es0,false}
	end,
    Errors = lists:sort(Errors0),
    Errors = lists:sort(Errors1),
    case {ErrorInFinishLoading,Bool} of
	{B,B} ->
	    Errors;
	{false,true} ->
	    ct:fail("code:prepare_loading/1 should have failed");
	{true,false} ->
	    ct:fail("code:prepare_loading/1 should have succeeded")
    end.

is_loaded(Mod) ->
    case erlang:module_loaded(Mod) of
	false ->
	    false = code:is_loaded(Mod);
	true ->
	    {file,_} = code:is_loaded(Mod),
	    true
    end.

basic_module(Mod) ->
    ?Q(["-module('@Mod@').\n"
	"-export(['@Mod@'/0]).\n",
	"'@Mod@'() -> ok."]).

on_load_module(Mod) ->
    ?Q(["-module('@Mod@').\n",
	"-on_load(f/0).\n",
	"f() -> ok.\n"]).

make_module_file(Dir, Fun) ->
    [{Mod,File,Code}] = make_modules(1, Fun),
    ok = file:write_file(filename:absname(File, Dir), Code),
    Mod.

make_modules(0, _) ->
    [];
make_modules(N, Fun) ->
    U = erlang:unique_integer([positive]),
    ModName = "m__" ++ integer_to_list(N) ++ "_" ++ integer_to_list(U),
    Mod = list_to_atom(ModName),
    ModItem = make_module(Mod, Fun),
    [ModItem|make_modules(N-1, Fun)].

update_modules(Ms, Fun) ->
    [make_module(M, Fun) || M <- Ms].

make_module(Mod, Fun) ->
    Filename = atom_to_list(Mod) ++ ".beam",
    make_module(Mod, Filename, Fun).

make_module(Mod, Filename, Fun) ->
    make_module(Mod, Filename, Fun, []).

make_module(Mod, Filename, Fun, Opts) ->
    Tree = Fun(Mod),
    merl:print(Tree),
    {ok,Mod,Code} = merl:compile(Tree, Opts),
    {Mod,Filename,Code}.
