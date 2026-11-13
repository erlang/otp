%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1999-2025. All Rights Reserved.
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
         batch_load_basic/1,batch_load_errors/1,batch_load_blockers/1,
         batch_load_on_load_ok/1,batch_load_on_load_failures/1,
         batch_load_hung_on_load/1,batch_load_pending/1,
         batch_load_concurrent/1,batch_load_embedded/1,
         batch_load_system_limit/1,batch_load_system_limit_peer/1,
	 on_load_failing/1,ensure_modules_loaded/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("syntax_tools/include/merl.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [basic_atomic_load,basic_errors,sticky_dir,on_load_failing,
     ensure_modules_loaded,
     batch_load_basic,batch_load_errors,batch_load_blockers,
     batch_load_on_load_ok,batch_load_on_load_failures,
     batch_load_hung_on_load,batch_load_pending,
     batch_load_concurrent,batch_load_embedded,batch_load_system_limit].

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
    register(spawn_hanging_on_load, self()),
    Pid = spawn_link(fun() ->
                             {error,on_load_failure} =
                                 code:load_binary(Mod, Name, Bin)
                     end),
    receive hanging_on_load -> ok end,
    unregister(spawn_hanging_on_load),
    Pid.

hanging_on_load_module(Mod) ->
    ?Q(["-module('@Mod@').\n",
	"-on_load(hang/0).\n",
	"hang() ->\n"
	"  register(hanging_on_load, self()),\n"
        "  spawn_hanging_on_load ! hanging_on_load,\n"
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
    ModulesToLoad = [OLMod,?MODULE,Mod,BadOLMod,BadEgg,BadMod,lists],
    {error,Error0} = code:ensure_modules_loaded(ModulesToLoad),
    Error = lists:sort([{BadEgg,nofile},
			{BadMod,badfile},
			{BadOLMod,on_load_failure}]),
    Error = lists:sort(Error0),
    true = is_loaded(Mod),
    true = is_loaded(OLMod),

    ok.

failing_on_load_module(Mod) ->
    ?Q(["-module('@Mod@').\n",
	"-on_load(f/0).\n",
	"f() -> fail.\n"]).

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

%%%
%%% code:batch_load/1 tests
%%%

batch_load_basic(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dir = filename:join(PrivDir, batch_load_basic),
    _ = file:make_dir(Dir),
    OldPath = code:get_path(),
    try
        code:add_patha(Dir),
        do_batch_load_basic(Dir)
    after
        code:set_path(OldPath)
    end,
    ok.

do_batch_load_basic(Dir) ->
    ok = batch_load_ok([]),
    [{BinaryM,_,BinaryCode}] = make_modules(1, versioned_module(1)),
    BinaryFile = filename:join(Dir, "nonexistent.beam"),
    try
        false = filelib:is_file(BinaryFile),
        ok = batch_load_ok([{BinaryM,BinaryFile,BinaryCode}]),
        BinaryFile = code:which(BinaryM),
        [{BinaryM,nofile}] = batch_load_error([BinaryM]),
        1 = BinaryM:BinaryM()
    after
        cleanup_modules([BinaryM])
    end,
    MsVer1_0 = make_modules(4, versioned_module(1)),
    MsVer1 = [{M,filename:absname(F, Dir),Bin} || {M,F,Bin} <- MsVer1_0],
    _ = [ok = file:write_file(F, Bin) || {_,F,Bin} <- MsVer1],
    Ms = [M || {M,_,_} <- MsVer1],
    try
        [] = [loaded || M <- Ms, is_loaded(M)],
        %% Mixed input: half by name (found via the code path),
        %% half as explicit {Module, Filename, Binary} tuples.
        {ByName1,ByBin1} = lists:split(2, MsVer1),
        Specs1 = [M || {M,_,_} <- ByName1] ++
                 [{M,F,B} || {M,F,B} <- ByBin1],
        ok = batch_load_ok(Specs1),
        _ = [1 = M:M() || M <- Ms],
        _ = [F = code:which(M) || {M,F,_} <- MsVer1],
        [] = [not_loaded || M <- Ms, not is_loaded(M)],
        %% Binary provenance is kept for tuple inputs.
        [{PM,PF,PB} | _] = ByBin1,
        {PM,LoadedBin,PF} = code:get_object_code(PM),
        true = LoadedBin =:= PB,
        %% Explicit reload with a new version, again mixed.
        MsVer2_0 = update_modules(Ms, versioned_module(2)),
        MsVer2 = [{M,filename:absname(F, Dir),Bin} ||
                     {M,F,Bin} <- MsVer2_0],
        _ = [ok = file:write_file(F, Bin) || {_,F,Bin} <- MsVer2],
        {ByName2,ByBin2} = lists:split(2, MsVer2),
        Specs2 = [M || {M,_,_} <- ByName2] ++
                 [{M,F,B} || {M,F,B} <- ByBin2],
        ok = batch_load_ok(Specs2),
        _ = [2 = M:M() || M <- Ms],
        _ = [F = code:which(M) || {M,F,_} <- MsVer2],
        [] = [not_loaded || M <- Ms, not is_loaded(M)],
        ok
    after
        cleanup_modules(Ms)
    end.

batch_load_errors(_Config) ->
    %% Malformed inputs raise function_clause ...
    batch_load_fc(42),
    batch_load_fc(atom),
    batch_load_fc([42]),
    batch_load_fc([{"mod","file","bin"}]),
    batch_load_fc([{m,"f","not_a_binary"}]),
    batch_load_fc([{42,"f",<<>>}]),
    batch_load_fc([{m,"f",<<>>,extra}]),
    batch_load_fc([{m,42,<<>>}]),
    batch_load_fc([m|not_a_list]),
    %% ... before any loads: a valid peer in the same call stays unloaded.
    [{GoodM,GoodF,GoodBin}] = make_modules(1, fun basic_module/1),
    try
        batch_load_fc([{GoodM,GoodF,GoodBin},42]),
        false = is_loaded(GoodM),
        ok
    after
        cleanup_modules([GoodM])
    end,
    %% badfile and nofile peers do not stop unique peers.
    [{BadM,_,_},{MismatchM,_,_}] = make_modules(2, fun basic_module/1),
    NoFileMod = list_to_atom("no_such_batch_mod_" ++
                            integer_to_list(erlang:unique_integer([positive]))),
    GoodMods = make_modules(2, fun basic_module/1),
    GoodMs = [M || {M,_,_} <- GoodMods],
    GoodSpecs = [{M,F,B} || {M,F,B} <- GoodMods],
    [{_,_,MismatchedBin}|_] = GoodMods,
    try
        Specs = [{BadM,"badfile_dummy",<<"bad">>},
                 {MismatchM,"mismatch.beam",MismatchedBin},NoFileMod] ++ GoodSpecs,
        ExpectedErrors = lists:sort([{BadM,badfile},{MismatchM,badfile},
                                    {NoFileMod,nofile}]),
        ExpectedErrors = batch_load_error(Specs),
        false = is_loaded(BadM),
        false = is_loaded(MismatchM),
        false = is_loaded(NoFileMod),
        _ = [ok = M:M() || M <- GoodMs],
        ok
    after
        cleanup_modules([BadM,MismatchM,NoFileMod] ++ GoodMs)
    end,
    %% Duplicates by name across input forms: all inputs for that
    %% module are skipped, unique peers still load.
    [{D1,D1F,D1B},{D2,D2F,D2B}] = make_modules(2, fun basic_module/1),
    [{U,UF,UB}] = make_modules(1, fun basic_module/1),
    {E,_,EB1} = make_module(list_to_atom("dup_both_" ++
                                       integer_to_list(erlang:unique_integer([positive]))),
                           fun basic_module/1),
    {E,EF2,EB2} = make_module(E, "dup_second.beam", fun basic_module/1),
    try
        DupSpecs = [{D1,D1F,D1B},D1,{D2,D2F,D2B},D2,
                    {E,"dup_first.beam",EB1},{E,EF2,EB2},
                    {U,UF,UB}],
        DupErrors = lists:sort([{D1,duplicated},{D2,duplicated},{E,duplicated}]),
        DupErrors = batch_load_error(DupSpecs),
        false = is_loaded(D1),
        false = is_loaded(D2),
        false = is_loaded(E),
        true = is_loaded(U),
        ok = U:U(),
        ok
    after
        cleanup_modules([D1,D2,E,U])
    end,
    ok.

batch_load_blockers(_Config) ->
    %% Build modules with old code left behind: v1 then v2, no purge.
    NpV1 = make_modules(2, versioned_module(1)),
    NpMs = [M || {M,_,_} <- NpV1],
    try
        ok = batch_load_ok([{M,F,B} || {M,F,B} <- NpV1]),
        NpV2_0 = update_modules(NpMs, versioned_module(2)),
        NpV2 = [{M,F,B} || {M,F,B} <- NpV2_0],
        ok = batch_load_ok(NpV2),
        _ = [2 = M:M() || M <- NpMs],
        %% Next binaries fail with not_purged; sticky fakes fail with
        %% sticky_directory; good peers in the same call still load and
        %% blocked modules keep their current versions.
        NpV3_0 = update_modules(NpMs, versioned_module(2)),
        NpV3 = [{M,F,B} || {M,F,B} <- NpV3_0],
        GoodMods = make_modules(2, fun basic_module/1),
        GoodMs = [M || {M,_,_} <- GoodMods],
        {lists,ListsF,ListsB} = make_module(lists, fun basic_module/1),
        {gen_server,GsF,GsB} = make_module(gen_server, fun basic_module/1),
        try
            Specs = NpV3 ++ [{lists,ListsF,ListsB},{gen_server,GsF,GsB}] ++
                    [{M,F,B} || {M,F,B} <- GoodMods],
            Expected = lists:sort([{M,not_purged} || M <- NpMs] ++
                                  [{lists,sticky_directory},{gen_server,sticky_directory}]),
            Expected = batch_load_error(Specs),
            _ = [ok = M:M() || M <- GoodMs],
            _ = [2 = M:M() || M <- NpMs],
            last = lists:last([last]),
            ok
        after
            cleanup_modules(GoodMs)
        end,
        ok
    after
        cleanup_modules(NpMs)
    end,
    ok.

batch_load_on_load_ok(_Config) ->
    OrdMods = make_modules(3, fun basic_module/1),
    SigMods = make_modules(2, fun signalling_on_load_module/1),
    OrdMs = [M || {M,_,_} <- OrdMods],
    [Dependency|_] = OrdMs,
    DepMods = make_modules(1, fun(M) -> dependent_on_load_module(M, Dependency) end),
    SigMs = [M || {M,_,_} <- SigMods],
    AllMs = OrdMs ++ SigMs ++ [M || {M,_,_} <- DepMods],
    try
        [persistent_term:erase({M,batch_on_load_ran}) || M <- SigMs],
        %% Unlike atomic_load/1, on_load modules are accepted here.
        Specs = [{M,F,B} || {M,F,B} <- DepMods ++ OrdMods ++ SigMods],
        ok = batch_load_ok(Specs),
        _ = [ok = M:M() || M <- OrdMs],
        _ = [true = is_loaded(M) || M <- SigMs],
        _ = [true = persistent_term:get({M,batch_on_load_ran}, false) ||
                M <- SigMs],
        [persistent_term:erase({M,batch_on_load_ran}) || M <- SigMs],
        ok
    after
        [catch persistent_term:erase({M,batch_on_load_ran}) || M <- SigMs],
        cleanup_modules(AllMs)
    end,
    ok.

batch_load_on_load_failures(_Config) ->
    %% First-load hook failures leave the module unloaded, peers load.
    [{FailM,FailF,FailB}] = make_modules(1, fun signalling_failing_on_load_module/1),
    [{ThrowM,ThrowF,ThrowB}] = make_modules(1, fun throwing_on_load_module/1),
    [{SigM,SigF,SigB}] = make_modules(1, fun signalling_on_load_module/1),
    HookMs = [FailM,ThrowM,SigM],
    GoodMods = make_modules(2, fun basic_module/1),
    GoodMs = [M || {M,_,_} <- GoodMods],
    try
        Specs = [{FailM,FailF,FailB},{ThrowM,ThrowF,ThrowB},{SigM,SigF,SigB}] ++
                [{M,F,B} || {M,F,B} <- GoodMods],
        HookErrors = lists:sort([{FailM,on_load_failure},{ThrowM,on_load_failure}]),
        HookErrors = batch_load_error(Specs),
        false = is_loaded(FailM),
        false = is_loaded(ThrowM),
        true = is_loaded(SigM),
        _ = [true = persistent_term:get({M,batch_on_load_ran}, false) || M <- HookMs],
        _ = [ok = M:M() || M <- GoodMs],
        ok
    after
        [persistent_term:erase({M,batch_on_load_ran}) || M <- HookMs],
        cleanup_modules(HookMs ++ GoodMs)
    end,
    %% Failed replacement keeps the previous current version.
    [{M,F1,B1}] = make_modules(1, versioned_module(1)),
    try
        ok = batch_load_ok([{M,F1,B1}]),
        1 = M:M(),
        Which1 = code:which(M),
        {M,F2,B2} = make_module(M, "replacement.beam",
                               fun failing_on_load_module/1),
        [{GP,GF,GB}] = make_modules(1, fun basic_module/1),
        try
            [{M,on_load_failure}] =
                batch_load_error([{M,F2,B2},{GP,GF,GB}]),
            1 = M:M(),
            Which1 = code:which(M),
            true = is_loaded(GP),
            ok = GP:GP(),
            ok
        after
            cleanup_modules([GP])
        end,
        ok
    after
        cleanup_modules([M])
    end,
    ok.

batch_load_hung_on_load(_Config) ->
    true = register(batch_load_hung_coord, self()),
    try
        do_batch_load_hung()
    after
        _ = (catch unregister(batch_load_hung_coord)),
        drain_batch_messages()
    end,
    ok.

do_batch_load_hung() ->
    Specs = make_modules(2, fun hung_batch_on_load_module/1),
    Ms = [M || {M,_,_} <- Specs],
    Expected = batch_load_modules(Specs),
    Test = self(),
    {Loader,MRef} = spawn_monitor(fun() ->
        Test ! {hung_loader_result,code:batch_load(Specs)}
    end),
    try
        Pids = [receive
                    {hung_entered,Pid} -> Pid
                after 30000 ->
                    ct:fail(hung_hook_never_entered)
                end || _ <- Specs],
        2 = length(lists:usort(Pids)),
        _ = [true = lists:member(whereis(M), Pids) || M <- Ms],
        _ = [false = is_loaded(M) || M <- Ms],
        receive
            {hung_loader_result,_} -> ct:fail(loader_returned_before_on_load)
        after 0 ->
            ok
        end,
        _ = [Pid ! release || Pid <- Pids],
        receive
            {hung_loader_result,{ok,Loaded,NotLoaded}} ->
                Expected = lists:sort(Loaded),
                0 = map_size(NotLoaded)
        after 30000 ->
            ct:fail(loader_never_finished)
        end,
        _ = [true = is_loaded(M) || M <- Ms],
        ok
    after
        stop_batch_loader(Ms, Loader, MRef),
        cleanup_modules(Ms)
    end.

batch_load_pending(_Config) ->
    do_batch_load_pending_external(),
    do_batch_load_pending_recursive(),
    ok.

do_batch_load_pending_external() ->
    [{HungM,HungF,HungB}] = make_modules(1, fun hung_batch_on_load_module/1),
    true = register(batch_load_hung_coord, self()),
    Test = self(),
    Spec = {HungM,HungF,HungB},
    {Loader,MRef} = spawn_monitor(fun() ->
        Test ! {pending_loader_result,code:load_binary(HungM,HungF,HungB)}
    end),
    try
        HungPid = receive
                      {hung_entered,Pid} -> Pid
                  after 30000 ->
                      ct:fail(hung_hook_never_entered)
                  end,
        [{HungM,pending_on_load}] = batch_load_error([Spec]),
        NormalSpec = make_module(HungM, "no_on_load.beam", fun basic_module/1),
        lists:foreach(fun(PendingSpec) ->
            [{GP,GF,GB}] = make_modules(1, fun basic_module/1),
            try
                [{HungM,pending_on_load}] =
                    batch_load_error([PendingSpec,{GP,GF,GB}]),
                true = is_loaded(GP),
                ok = GP:GP()
            after
                cleanup_modules([GP])
            end
        end, [Spec,NormalSpec]),
        HungPid ! release,
        receive
            {pending_loader_result,{module,HungM}} -> ok
        after 30000 ->
            ct:fail(loader_never_finished)
        end
    after
        stop_batch_loader([HungM], Loader, MRef),
        unregister(batch_load_hung_coord),
        drain_batch_messages(),
        cleanup_modules([HungM])
    end.

do_batch_load_pending_recursive() ->
    true = register(batch_load_recursive_coord, self()),
    try
        [{RM,RF,RB}] = make_modules(1, fun recursive_batch_on_load_module/1),
        [{PM,PF,PB}] = make_modules(1, fun basic_module/1),
        persistent_term:put({RM,batch_recursive_spec}, [{RM,RF,RB},{PM,PF,PB}]),
        try
            %% Reject the pending self without skipping the unrelated peer.
            ok = batch_load_ok([{RM,RF,RB}]),
            Expected = #{RM => pending_on_load},
            receive
                {recursive_result,{ok,[PM],Expected}} -> ok
            after 30000 ->
                ct:fail(recursive_hook_never_reported)
            end,
            true = is_loaded(RM),
            true = is_loaded(PM),
            ok = PM:PM()
        after
            persistent_term:erase({RM,batch_recursive_spec}),
            cleanup_modules([RM,PM])
        end,
        ok
    after
        _ = (catch unregister(batch_load_recursive_coord)),
        drain_batch_messages()
    end.

batch_load_concurrent(_Config) ->
    Groups = [make_modules(2, fun basic_module/1) || _ <- [1,2,3,4]],
    AllMs = [M || G <- Groups, {M,_,_} <- G],
    try
        Test = self(),
        Workers = [spawn_monitor(fun() ->
            Test ! {concurrent_result,self(),batch_load_ok(Specs)}
        end) || Specs <- Groups],
        Pids = [Pid || {Pid,_} <- Workers],
        _ = [receive
                {concurrent_result,Pid,ok} -> ok
            after 30000 ->
                ct:fail(concurrent_worker_never_finished)
            end || Pid <- Pids],
        _ = [receive
                {'DOWN',Ref,process,Pid,normal} -> ok
            after 5000 ->
                ct:fail(concurrent_worker_never_exited)
            end || {Pid,Ref} <- Workers],
        _ = [ok = M:M() || M <- AllMs],
        [] = [not_loaded || M <- AllMs, not is_loaded(M)],
        ok
    after
        cleanup_modules(AllMs)
    end,
    ok.

batch_load_system_limit(_Config) ->
    [Spec] = make_modules(1, fun signalling_on_load_module/1),
    {ok,Peer,_} = ?CT_PEER(#{connection => standard_io,
                           args => ["+P","1024"],
                           env => [{"ERL_ZFLAGS","+S 1 +Q 65536"}]}),
    try
        ok = peer:call(Peer, ?MODULE, batch_load_system_limit_peer, [Spec], 30000)
    after
        peer:stop(Peer)
    end.

batch_load_system_limit_peer({M,_,_}=Spec) ->
    1 = erlang:system_info(schedulers_online),
    1024 = erlang:system_info(process_limit),
    ok = code:ensure_modules_loaded([lists,maps,logger,persistent_term]),
    ok = logger:set_primary_config(level, none),
    lists:foreach(fun(FreeSlots) -> batch_load_exhaustion(Spec, FreeSlots) end, [0,2]),
    {ok,[M],NotLoaded} = code:batch_load([Spec]),
    0 = map_size(NotLoaded),
    {file,_} = code:is_loaded(M),
    true = persistent_term:get({M,batch_on_load_ran}),
    ok.

batch_load_exhaustion({M,_,_}=Spec, FreeSlots) ->
    CS = whereis(code_server),
    Parent = self(),
    Ref = make_ref(),
    {Caller,Mon} = spawn_monitor(fun() ->
        receive go -> ok end,
        Result = try code:batch_load([Spec])
                 catch
                     Class:Reason:Stack -> {exception,Class,Reason,Stack}
                 end,
        Parent ! {Ref,Result}
    end),
    1 = erlang:trace_pattern({erlang,spawn_opt,2},
                            [{'_',[],[{exception_trace}]}], [local]),
    1 = erlang:trace_pattern({erts_code_purger,finish_after_on_load,2},
                            [{'_',[],[{return_trace}]}], [local]),
    1 = erlang:trace(CS, true, [call,{tracer,self()}]),
    1 = erlang:trace(Caller, true, [send,{tracer,self()}]),
    true = erlang:suspend_process(CS),
    Caller ! go,
    try
        %% Let preparation finish before withholding slots from callback creation.
        receive
            {trace,Caller,send,{code_call,Caller,{batch_finish_loading,[]}},_} -> ok
        after 5000 ->
            error(preparation_did_not_finish)
        end,
        1 = erlang:trace(Caller, false, [send]),
        Fillers = batch_load_fill_processes([]),
        try
            Limit = erlang:system_info(process_limit),
            Limit = erlang:system_info(process_count),
            lists:foreach(fun batch_load_kill_process/1,
                          lists:sublist(Fillers, FreeSlots)),
            FreeSlots = Limit - erlang:system_info(process_count),
            %% Two slots admit do_par's coordinator and worker, but not the callback.
            true = erlang:resume_process(CS),
            Result = receive
                         {Ref,R} -> R
                     after 5000 ->
                         error(batch_load_did_not_return)
                     end,
            case FreeSlots of
                0 ->
                    {exception,error,system_limit,[_|_]=Stack} = Result,
                    true = lists:any(fun({code,do_par,_,_}) -> true;
                                        (_) -> false
                                     end, Stack);
                2 ->
                    {ok,[],Failed} = Result,
                    #{M := system_limit} = Failed,
                    1 = map_size(Failed),
                    batch_load_spawn_failure_trace(CS, M)
            end,
            false = code:is_loaded(M),
            false = persistent_term:get({M,batch_on_load_ran}, false),
            CS = whereis(code_server),
            receive
                {'DOWN',Mon,process,Caller,normal} -> ok
            after 5000 ->
                error(batch_load_caller_did_not_exit)
            end
        after
            lists:foreach(fun batch_load_kill_process/1, Fillers)
        end
    after
        try erlang:resume_process(CS) catch error:badarg -> ok end,
        try erlang:trace(CS, false, [call]) catch error:badarg -> ok end,
        try erlang:trace(Caller, false, [send]) catch error:badarg -> ok end,
        erlang:trace_pattern({erlang,spawn_opt,2}, false, [local]),
        erlang:trace_pattern({erts_code_purger,finish_after_on_load,2}, false, [local]),
        erlang:exit_signal(Caller, kill),
        erlang:demonitor(Mon, [flush])
    end.

batch_load_spawn_failure_trace(CS, M) ->
    receive
        {trace,CS,exception_from,{erlang,spawn_opt,2},{error,system_limit}} -> ok
    after 5000 -> error(callback_spawn_did_not_fail) end,
    receive
        {trace,CS,call,{erts_code_purger,finish_after_on_load,[M,false]}} -> ok
    after 5000 -> error(on_load_cleanup_not_called) end,
    receive
        {trace,CS,return_from,{erts_code_purger,finish_after_on_load,2},ok} -> ok
    after 5000 -> error(on_load_cleanup_not_completed) end.

batch_load_fill_processes(Acc) ->
    try spawn_opt(fun() -> receive after infinity -> ok end end, [{priority,low}]) of
        Pid -> batch_load_fill_processes([Pid|Acc])
    catch
        error:system_limit -> Acc
    end.

batch_load_kill_process(Pid) ->
    Ref = monitor(process, Pid),
    erlang:exit_signal(Pid, kill),
    receive
        {'DOWN',Ref,process,Pid,_} -> ok
    after 5000 -> error(process_cleanup_timed_out) end.

batch_load_embedded(_Config) ->
    OrdMods = make_modules(2, fun basic_module/1),
    SigMods = make_modules(1, fun signalling_on_load_module/1),
    Specs = OrdMods ++ SigMods,
    {ok,Peer,Node} = ?CT_PEER(["-mode","embedded"]),
    try
        embedded = rpc:call(Node, code, get_mode, []),
        %% Explicit batch loads work in embedded mode, including
        %% modules with on_load functions.
        {ok,Loaded,NotLoaded} = rpc:call(Node, code, batch_load, [Specs]),
        Expected = batch_load_modules(Specs),
        Expected = lists:sort(Loaded),
        0 = map_size(NotLoaded),
        _ = [{file,_} = rpc:call(Node, code, is_loaded, [M]) ||
                {M,_,_} <- Specs],
        _ = [ok = rpc:call(Node, M, M, []) || {M,_,_} <- OrdMods],
        _ = [true = rpc:call(Node, persistent_term, get,
                            [{M,batch_on_load_ran},false]) || {M,_,_} <- SigMods],
        ok
    after
        peer:stop(Peer)
    end,
    ok.

%%%
%%% batch_load/1 helpers
%%%

batch_load_modules(Modules) ->
    lists:usort([case Item of
                     M when is_atom(M) -> M;
                     {M,_,_} -> M
                 end || Item <- Modules]).

batch_load_ok(Modules) ->
    {ok,Loaded,NotLoaded} = code:batch_load(Modules),
    0 = map_size(NotLoaded),
    Expected = batch_load_modules(Modules),
    Expected = lists:sort(Loaded),
    ok.

batch_load_error(Modules) ->
    {ok,Loaded,NotLoaded} = code:batch_load(Modules),
    true = is_map(NotLoaded),
    Expected = [M || M <- batch_load_modules(Modules), not is_map_key(M, NotLoaded)],
    Expected = lists:sort(Loaded),
    lists:sort(maps:to_list(NotLoaded)).

batch_load_fc(Term) ->
    {'EXIT',{function_clause,[{code,batch_load,[Term],_} | _]}} =
        (catch code:batch_load(Term)).

cleanup_modules(Ms) ->
    [begin
         code:purge(M),
         code:delete(M),
         code:purge(M),
         false = is_loaded(M)
     end || M <- Ms],
    ok.

stop_batch_loader(Mods, Loader, Ref) ->
    _ = [case whereis(Mod) of
             undefined -> ok;
             Hook -> erlang:exit_signal(Hook, kill)
         end || Mod <- Mods],
    receive
        {'DOWN',Ref,process,Loader,_} -> ok
    after 5000 ->
        erlang:exit_signal(Loader, kill),
        receive {'DOWN',Ref,process,Loader,_} -> ok end
    end,
    demonitor(Ref, [flush]).

drain_batch_messages() ->
    receive
        {hung_entered,_} -> drain_batch_messages();
        {hung_loader_result,_} -> drain_batch_messages();
        {pending_loader_result,_} -> drain_batch_messages();
        {recursive_result,_} -> drain_batch_messages();
        {concurrent_result,_,_} -> drain_batch_messages()
    after 0 ->
        ok
    end.

dependent_on_load_module(Mod, Dependency) ->
    ?Q(["-module('@Mod@').\n",
        "-on_load(f/0).\n",
        "f() -> '@Dependency@':'@Dependency@'().\n"]).

signalling_on_load_module(Mod) ->
    ?Q(["-module('@Mod@').\n",
        "-on_load(f/0).\n",
        "f() -> persistent_term:put({'@Mod@', batch_on_load_ran}, true), ok.\n"]).

signalling_failing_on_load_module(Mod) ->
    ?Q(["-module('@Mod@').\n",
        "-on_load(f/0).\n",
        "f() -> persistent_term:put({'@Mod@', batch_on_load_ran}, true), fail.\n"]).

throwing_on_load_module(Mod) ->
    ?Q(["-module('@Mod@').\n",
        "-on_load(f/0).\n",
        "f() -> persistent_term:put({'@Mod@', batch_on_load_ran}, true),\n",
        "       throw(batch_on_load_throw).\n"]).

hung_batch_on_load_module(Mod) ->
    ?Q(["-module('@Mod@').\n",
        "-on_load(hang/0).\n",
        "hang() ->\n",
        "    register('@Mod@', self()),\n",
        "    whereis(batch_load_hung_coord) ! {hung_entered, self()},\n",
        "    receive release -> ok end.\n"]).

recursive_batch_on_load_module(Mod) ->
    ?Q(["-module('@Mod@').\n",
        "-on_load(f/0).\n",
        "f() ->\n",
        "    Specs = persistent_term:get({'@Mod@', batch_recursive_spec}),\n",
        "    R = code:batch_load(Specs),\n",
        "    whereis(batch_load_recursive_coord) ! {recursive_result, R},\n",
        "    ok.\n"]).
