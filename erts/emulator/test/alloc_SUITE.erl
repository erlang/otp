%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2022. All Rights Reserved.
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

-module(alloc_SUITE).
-author('rickard.green@uab.ericsson.se').
-export([all/0, suite/0, init_per_testcase/2, end_per_testcase/2,
	 init_per_suite/1, end_per_suite/1]).
-export([basic/1,
	 coalesce/1,
	 threads/1,
	 realloc_copy/1,
	 bucket_index/1,
	 bucket_mask/1,
	 rbtree/1,
	 mseg_clear_cache/1,
	 erts_mmap/1,
	 cpool/1,
         set_dyn_param/1,
	 migration/1,
         cpool_opt/1]).

%% Internal export
-export([run_drv_case/2]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 4}}].

all() -> 
    [basic, coalesce, threads, realloc_copy, bucket_index,
     set_dyn_param,
     bucket_mask, rbtree, mseg_clear_cache, erts_mmap, cpool, migration,
     cpool_opt].

init_per_suite(Config) ->
    case test_server:memory_checker() of
	MC when MC =:= valgrind; MC =:= asan ->
	    %% No point testing own allocators under valgrind or asan.
	    {skip, "Memory checker " ++ atom_to_list(MC)};
	none ->
	    Config
    end.

end_per_suite(_Config) ->
    ok.

init_per_testcase(Case, Config) when is_list(Config) ->
    [{testcase, Case}|Config].

end_per_testcase(_Case, Config) when is_list(Config) ->
    erts_test_utils:ept_check_leaked_nodes(Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                        %%
%% Testcases                                                              %%
%%                                                                        %%

basic(Cfg) -> drv_case(Cfg).
coalesce(Cfg) -> drv_case(Cfg).
threads(Cfg) -> drv_case(Cfg).
realloc_copy(Cfg) -> drv_case(Cfg).
bucket_index(Cfg) -> drv_case(Cfg).
bucket_mask(Cfg) -> drv_case(Cfg).
rbtree(Cfg) -> drv_case(Cfg).
mseg_clear_cache(Cfg) -> drv_case(Cfg).
cpool(Cfg) -> drv_case(Cfg).

migration(Cfg) ->
    %% Enable test_alloc.
    %% Disable driver_alloc to avoid recursive alloc_util calls
    %% through enif_mutex_create() in my_creating_mbc().
    drv_case(Cfg, concurrent, ["+MZe", "true", "+MRe", "false"]),
    drv_case(Cfg, concurrent, ["+MZe", "true", "+MRe", "false", "+MZas", "ageffcbf"]),
    drv_case(Cfg, concurrent, ["+MZe", "true", "+MRe", "false", "+MZas", "chaosff"]).

cpool_opt(Config) when is_list(Config) ->
    {ok, PeerA, NodeA} = ?CT_PEER(["+Mue", "true", "+Mut", "true", "+Muacul", "de", "+Mucp", "@"]),
    {cp, '@'} = get_cp_opt(NodeA, binary_alloc),
    {cp, '@'} = get_cp_opt(NodeA, std_alloc),
    {cp, '@'} = get_cp_opt(NodeA, ets_alloc),
    {cp, '@'} = get_cp_opt(NodeA, fix_alloc),
    {cp, '@'} = get_cp_opt(NodeA, eheap_alloc),
    {cp, '@'} = get_cp_opt(NodeA, ll_alloc),
    {cp, '@'} = get_cp_opt(NodeA, driver_alloc),
    {cp, '@'} = get_cp_opt(NodeA, sl_alloc),
    peer:stop(PeerA),
    {ok, PeerB, NodeB} = ?CT_PEER(["+Mue", "true", "+Mut", "true", "+Muacul", "de", "+Mucp", ":"]),
    {cp, 'B'} = get_cp_opt(NodeB, binary_alloc),
    {cp, 'D'} = get_cp_opt(NodeB, std_alloc),
    {cp, 'E'} = get_cp_opt(NodeB, ets_alloc),
    {cp, 'F'} = get_cp_opt(NodeB, fix_alloc),
    {cp, 'H'} = get_cp_opt(NodeB, eheap_alloc),
    {cp, 'L'} = get_cp_opt(NodeB, ll_alloc),
    {cp, 'R'} = get_cp_opt(NodeB, driver_alloc),
    {cp, 'S'} = get_cp_opt(NodeB, sl_alloc),
    peer:stop(PeerB),
    {ok, PeerC, NodeC} = ?CT_PEER(["+Mue", "true", "+Mut", "true", "+Muacul", "de", "+Mucp", ":", "+MEcp", "H"]),
    {cp, 'B'} = get_cp_opt(NodeC, binary_alloc),
    {cp, 'D'} = get_cp_opt(NodeC, std_alloc),
    {cp, 'H'} = get_cp_opt(NodeC, ets_alloc),
    {cp, 'F'} = get_cp_opt(NodeC, fix_alloc),
    {cp, 'H'} = get_cp_opt(NodeC, eheap_alloc),
    {cp, 'L'} = get_cp_opt(NodeC, ll_alloc),
    {cp, 'R'} = get_cp_opt(NodeC, driver_alloc),
    {cp, 'S'} = get_cp_opt(NodeC, sl_alloc),
    peer:stop(PeerC).

get_cp_opt(Node, Alloc) ->
    AInfo = rpc:call(Node, erlang, system_info, [{allocator,Alloc}]),
    {instance, 1, IList} = lists:keyfind(1, 2, AInfo),
    {options, OList} = lists:keyfind(options, 1, IList),
    lists:keyfind(cp, 1, OList).
    

erts_mmap(Config) when is_list(Config) ->
    case {os:type(), mmsc_flags()} of
	{{unix,_}, false} ->
	    [erts_mmap_do(SCO, SCRPM, SCRFSD)
	     || SCO <-[true,false], SCRFSD <-[1234,0], SCRPM <- [true,false]];
	{{unix,_}, Flags} ->
	    {skipped, Flags};
	{{SkipOs,_},_} ->
	    {skipped,
		   lists:flatten(["Not run on "
				  | io_lib:format("~p",[SkipOs])])}
    end.

%% Check if there are ERL_FLAGS set that will mess up this test case
mmsc_flags() ->
    case mmsc_flags("ERL_FLAGS") of
	false -> mmsc_flags("ERL_ZFLAGS");
	Flags -> Flags
    end.
mmsc_flags(Env) ->
    case os:getenv(Env) of
	false -> false;
	V ->
            case string:find(V, "+MMsc") of
                nomatch -> false;
                SubStr -> Env ++ "=" ++ SubStr
            end
    end.

erts_mmap_do(SCO, SCRPM, SCRFSD) ->
    %% We use the number of schedulers + 1 * approx main carriers size
    %% to calculate how large the super carrier has to be
    %% and then use a minimum of 100 for systems with a low amount of
    %% schedulers
    Schldr = erlang:system_info(schedulers_online)+1,
    SCS = max(round((262144 * 6 + 3 * 1048576) * Schldr / 1024 / 1024),100),
    O1 = ["+MMscs" ++ integer_to_list(SCS),
	"+MMsco" ++ atom_to_list(SCO),
	"+MMscrpm" ++ atom_to_list(SCRPM)],
    Opts = case SCRFSD of
	       0 -> O1;
	       _ -> O1 ++ ["+MMscrfsd"++integer_to_list(SCRFSD)]
	   end,
    {ok, Peer, Node} = ?CT_PEER(Opts),
    F = fun() ->
                SI = erlang:system_info({allocator,erts_mmap}),
                {default_mmap,EM} = lists:keyfind(default_mmap, 1, SI),
                {supercarrier,SC} = lists:keyfind(supercarrier, 1, EM),
                {sizes,Sizes} = lists:keyfind(sizes, 1, SC),
                {free_segs,Segs} = lists:keyfind(free_segs,1,SC),
                {total,Total} = lists:keyfind(total,1,Sizes),
                io:format("Expecting total ~w, got ~w~n", [SCS*1024*1024,Total]),
                Total = SCS*1024*1024,

                {reserved,Reserved} = lists:keyfind(reserved,1,Segs),
                true = (Reserved >= SCRFSD),

                case {SCO,lists:keyfind(os,1,EM)} of
                    {true, false} -> ok;
                    {false, {os,_}} -> ok
                end,

                exit(ok)
        end,

    {Pid, MRef} = spawn_monitor(Node, F),
    Result = receive {'DOWN', MRef, process, Pid, Rslt} -> Rslt end,
    peer:stop(Peer),
    Result.


%% Test erlang:system_flag(erts_alloc, ...)
set_dyn_param(_Config) ->
    {_, _, _, AlcList} = erlang:system_info(allocator),

    {Enabled, Disabled, Others} =
        lists:foldl(fun({sys_alloc,_}, {Es, Ds, Os}) ->
                            {Es, [sys_alloc | Ds], Os};

                       ({AT, Opts}, {Es, Ds, Os}) when is_list(Opts) ->
                            case lists:keyfind(e, 1, Opts) of
                                {e, true} ->
                                    {[AT | Es], Ds, Os};
                                {e, false} ->
                                    {Es, [AT | Ds], Os};
                                false ->
                                    {Es, Ds, [AT | Os]}
                            end;

                       (_, Acc) -> Acc
                    end,
                    {[], [], []},
                    AlcList),

    Param = sbct,
    lists:foreach(fun(AT) -> set_dyn_param_enabled(AT, Param) end,
                  Enabled),

    lists:foreach(fun(AT) ->
                          Tpl = {AT, Param, 12345},
                          io:format("~p\n", [Tpl]),
                          notsup = erlang:system_flag(erts_alloc, Tpl)
                  end,
                  Disabled),

    lists:foreach(fun(AT) ->
                          Tpl = {AT, Param, 12345},
                          io:format("~p\n", [Tpl]),
                          {'EXIT',{badarg,_}} =
                              (catch erlang:system_flag(erts_alloc, Tpl))
                  end,
                  Others),
    ok.

set_dyn_param_enabled(AT, Param) ->
    OldVal = get_alc_param(AT, Param),

    Val1 = OldVal div 2,
    Tuple = {AT, Param, Val1},
    io:format("~p\n", [Tuple]),
    ok = erlang:system_flag(erts_alloc, Tuple),
    Val1 = get_alc_param(AT, Param),

    ok = erlang:system_flag(erts_alloc, {AT, Param, OldVal}),
    OldVal = get_alc_param(AT, Param),
    ok.

get_alc_param(AT, Param) ->
    lists:foldl(fun({instance,_,Istats}, Acc) ->
                        {options,Opts} = lists:keyfind(options, 1, Istats),
                        {Param,Val} = lists:keyfind(Param, 1, Opts),
                        {as,Strategy} = lists:keyfind(as, 1, Opts),

                        case {param_for_strat(Param, Strategy), Acc} of
                            {false, _} -> Acc;
                            {true, undefined} -> Val;
                            {true, _} ->
                                Val = Acc
                        end
                end,
                undefined,
                erlang:system_info({allocator, AT})).

param_for_strat(sbct, gf) -> false;
param_for_strat(_, _) -> true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                        %%
%% Internal functions                                                     %%
%%                                                                        %%

drv_case(Config) ->
    drv_case(Config, one_shot, "").

drv_case(Config, Mode, NodeOpts) when is_list(Config) ->
    case os:type() of
	{Family, _} when Family == unix; Family == win32 ->
            %% ?CT_PEER(#{exec => {"/usr/local/bin/erl", ["-emu_type", "debug"]}})
            TC = proplists:get_value(testcase, Config),
	    {ok, Peer, Node} = ?CT_PEER(#{name => ?CT_PEER_NAME(TC), args => NodeOpts}),
            Result = erpc:call(Node, ?MODULE, run_drv_case, [Config, Mode]),
	    peer:stop(Peer),
	    Result;
	SkipOs ->
	    {skipped,
		   lists:flatten(["Not run on "
				  | io_lib:format("~p",[SkipOs])])}
    end.

run_drv_case(Config, Mode) ->
    DataDir = proplists:get_value(data_dir,Config),
    CaseName = proplists:get_value(testcase,Config),
    File = filename:join(DataDir, CaseName),
    {ok,CaseName,Bin} = compile:file(File, [binary,return_errors]),
    {module,CaseName} = erlang:load_module(CaseName,Bin),
    print_stats(CaseName),
    ok = CaseName:init(File),

    SlaveState = slave_init(CaseName),
    case Mode of
	one_shot ->
	    Result = one_shot(CaseName);

	concurrent ->
	    Result = concurrent(CaseName)
    end,

    wait_for_memory_deallocations(),
    print_stats(CaseName),

    true = erlang:delete_module(CaseName),
    slave_end(SlaveState),
    Result.

slave_init(migration) ->
    A0 = case application:start(sasl) of
	     ok -> [sasl];
	     _ -> []
	 end,
    case application:start(os_mon) of
	ok -> [os_mon|A0];
	_ -> A0
    end;
slave_init(_) -> [].

slave_end(Apps) ->
    lists:foreach(fun (A) -> application:stop(A) end, Apps).

wait_for_memory_deallocations() ->
    try
        erts_debug:set_internal_state(wait, deallocations)
    catch
        error:undef ->
            erts_debug:set_internal_state(available_internal_state, true),
            wait_for_memory_deallocations()
    end.

print_stats(migration) ->
    IFun = fun({instance,_,Stats}, {Regular0, Pooled0}) ->
                   {mbcs,MBCS} = lists:keyfind(mbcs, 1, Stats),
                   {sbcs,SBCS} = lists:keyfind(sbcs, 1, Stats),

                   Regular = MBCS ++ SBCS ++ Regular0,
                   case lists:keyfind(mbcs_pool, 1, Stats) of
                        {mbcs_pool,Pool} -> {Regular, Pool ++ Pooled0};
                        false -> {Regular, Pooled0}
                   end;
              (_, Acc) ->
                  Acc
           end,

    Stats = erlang:system_info({allocator,test_alloc}),
    {Regular, Pooled} = lists:foldl(IFun, {[], []}, Stats),

    {RegBlocks, RegCarriers} = summarize_alloc_stats(Regular, {0, 0}),
    {PooledBlocks, PooledCarriers} = summarize_alloc_stats(Pooled, {0, 0}),

    io:format("Number of blocks  : ~p\n", [RegBlocks]),
    io:format("Number of carriers: ~p\n", [RegCarriers]),
    io:format("Number of pooled blocks  : ~p\n", [PooledBlocks]),
    io:format("Number of pooled carriers: ~p\n", [PooledCarriers]);
print_stats(_) ->
    ok.

summarize_alloc_stats([{blocks,L} | Rest], {Blocks0, Carriers}) ->
    Blocks = count_blocks([S || {_Type, S} <- L], Blocks0),
    summarize_alloc_stats(Rest, {Blocks, Carriers});
summarize_alloc_stats([{carriers, Count, _, _} | Rest], {Blocks, Carriers0}) ->
    summarize_alloc_stats(Rest, {Blocks, Carriers0 + Count});
summarize_alloc_stats([{carriers, Count} | Rest], {Blocks, Carriers0}) ->
    summarize_alloc_stats(Rest, {Blocks, Carriers0 + Count});
summarize_alloc_stats([_ | Rest], Acc) ->
    summarize_alloc_stats(Rest, Acc);
summarize_alloc_stats([], Acc) ->
    Acc.

count_blocks([{count, Count, _, _} | Rest], Acc) ->
    count_blocks(Rest, Acc + Count);
count_blocks([{count, Count} | Rest], Acc) ->
    count_blocks(Rest, Acc + Count);
count_blocks([_ | Rest], Acc) ->
    count_blocks(Rest, Acc);
count_blocks([], Acc) ->
    Acc.

one_shot(CaseName) ->
    State = CaseName:start({1, 0, erlang:system_info(build_type)}),
    Result0 = CaseName:run(State),
    false = (Result0 =:= continue),
    Result1 = handle_result(State, Result0),
    CaseName:stop(State),
    Result1.


many_shot(CaseName, I, Mem) ->
    State = CaseName:start({I, Mem, erlang:system_info(build_type)}),
    Result1 = repeat_while(fun() ->
				   Result0 = CaseName:run(State),
				   handle_result(State, Result0)
			   end,
			   10*1000, I),
    CaseName:stop(State),
    flush_log(),
    Result1.

concurrent(CaseName) ->
    NSched = erlang:system_info(schedulers_online),
    Mem = (free_memory() * 3) div 4,
    PRs = lists:map(fun(I) -> spawn_opt(fun() ->
						many_shot(CaseName, I,
							  Mem div NSched)
					end,
				       [monitor, {scheduler,I}])
		    end,
		    lists:seq(1, NSched)),
    lists:foreach(fun({Pid,Ref}) ->
			  receive {'DOWN', Ref, process, Pid, Reason} ->
				  Reason
			  end
		  end,
		  PRs),
    ok.

repeat_while(Fun, Timeout, I) ->
    TRef = erlang:start_timer(Timeout, self(), timeout),
    R = repeat_while_loop(Fun, TRef, I),
    erlang:cancel_timer(TRef, [{async,true},{info,false}]),
    R.

repeat_while_loop(Fun, TRef, I) ->
    receive
	{timeout, TRef, timeout} ->
	    io:format("~p: Timeout, enough is enough.",[I]),
	    succeeded
    after 0 ->
	    %%io:format("~p calls fun\n", [self()]),
	    case Fun() of
		continue -> repeat_while_loop(Fun, TRef, I);
		R -> R
	    end
    end.

flush_log() ->
    receive
	{print, Str} ->
	    io:format("~s", [Str]),
	    flush_log()
    after 0 ->
	    ok
    end.

handle_result(_State, Result0) ->
    flush_log(),
    case Result0 of
	{'EXIT', Error} ->
	    ct:fail(Error);
	{'EXIT', error, Error} ->
	    ct:fail(Error);
	{failed, Comment} ->
	    ct:fail(Comment);
	{skipped, Comment} ->
	    {skipped, Comment};
	{succeeded, ""} ->
	    succeeded;
	{succeeded, Comment} ->
	    {comment, Comment};
	continue ->
	    continue
    end.

free_memory() ->
    %% Free memory in MB.
    try
	SMD = memsup:get_system_memory_data(),
        TotFree = proplists:get_value(
                    available_memory, SMD,
                    proplists:get_value(free_memory, SMD) +
                        proplists:get_value(cached_memory, SMD, 0) +
                        proplists:get_value(buffered_memory, SMD, 0)
                   ),
	TotFree div (1024*1024)
    catch
	error : undef ->
	    ct:fail({"os_mon not built"})
    end.



