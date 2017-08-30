%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
-export([all/0, suite/0, init_per_testcase/2, end_per_testcase/2]).

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
	 migration/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 4}}].

all() -> 
    [basic, coalesce, threads, realloc_copy, bucket_index,
     bucket_mask, rbtree, mseg_clear_cache, erts_mmap, cpool, migration].

init_per_testcase(Case, Config) when is_list(Config) ->
    [{testcase, Case},{debug,false}|Config].

end_per_testcase(_Case, Config) when is_list(Config) ->
    ok.

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
    case erlang:system_info(smp_support) of
	true ->
	    drv_case(Cfg, concurrent, "+MZe true");
	false ->
	    {skipped, "No smp"}
    end.

erts_mmap(Config) when is_list(Config) ->
    case {os:type(), mmsc_flags()} of
	{{unix,_}, false} ->
	    [erts_mmap_do(Config, SCO, SCRPM, SCRFSD)
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
	V -> case string:str(V, "+MMsc") of
		 0 -> false;
		 P -> Env ++ "=" ++ string:substr(V, P)
	     end
    end.

erts_mmap_do(Config, SCO, SCRPM, SCRFSD) ->
    %% We use the number of schedulers + 1 * approx main carriers size
    %% to calculate how large the super carrier has to be
    %% and then use a minimum of 100 for systems with a low amount of
    %% schedulers
    Schldr = erlang:system_info(schedulers_online)+1,
    SCS = max(round((262144 * 6 + 3 * 1048576) * Schldr / 1024 / 1024),100),
    O1 = "+MMscs" ++ integer_to_list(SCS)
	++ " +MMsco" ++ atom_to_list(SCO)
	++ " +MMscrpm" ++ atom_to_list(SCRPM),
    Opts = case SCRFSD of
	       0 -> O1;
	       _ -> O1 ++ " +MMscrfsd"++integer_to_list(SCRFSD)
	   end,
    {ok, Node} = start_node(Config, Opts),
    Self = self(),
    Ref = make_ref(),
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

                Self ! {Ref, ok}
        end,

    spawn_link(Node, F),
    Result = receive {Ref, Rslt} -> Rslt end,
    stop_node(Node),
    Result.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                        %%
%% Internal functions                                                     %%
%%                                                                        %%

drv_case(Config) ->
    drv_case(Config, one_shot, "").

drv_case(Config, Mode, NodeOpts) when is_list(Config) ->
    case os:type() of
	{Family, _} when Family == unix; Family == win32 ->
	    {ok, Node} = start_node(Config, NodeOpts),
	    Self = self(),
	    Ref = make_ref(),
	    spawn_link(Node,
			     fun () ->
				     Res = run_drv_case(Config, Mode),
				     Self ! {Ref, Res}
			     end),
	    Result = receive {Ref, Rslt} -> Rslt end,
	    stop_node(Node),
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
    {Btot,Ctot} = lists:foldl(fun({instance,Inr,Istats}, {Bacc,Cacc}) ->
				      {mbcs,MBCS} = lists:keyfind(mbcs, 1, Istats),
				      Btup = lists:keyfind(blocks, 1, MBCS),
				      Ctup = lists:keyfind(carriers, 1, MBCS),
				      io:format("{instance,~p,~p,~p}\n", [Inr, Btup, Ctup]),
				      {tuple_add(Bacc,Btup),tuple_add(Cacc,Ctup)};
				 (_, Acc) -> Acc
			      end,
			      {{blocks,0,0,0},{carriers,0,0,0}},
			      erlang:system_info({allocator,test_alloc})),

    io:format("Number of blocks  : ~p\n", [Btot]),
    io:format("Number of carriers: ~p\n", [Ctot]);
print_stats(_) -> ok.

tuple_add(T1, T2) ->
    list_to_tuple(lists:zipwith(fun(E1,E2) when is_number(E1), is_number(E2) ->
					 E1 + E2;
				    (A,A) ->
					 A
				 end,
				 tuple_to_list(T1), tuple_to_list(T2))).


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
    NSched = erlang:system_info(schedulers),
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

start_node(Config, Opts) when is_list(Config), is_list(Opts) ->
    case proplists:get_value(debug,Config) of
	true -> {ok, node()};
	_ -> start_node_1(Config, Opts)
    end.

start_node_1(Config, Opts) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Name = list_to_atom(atom_to_list(?MODULE)
			++ "-"
			++ atom_to_list(proplists:get_value(testcase, Config))
			++ "-"
			++ integer_to_list(erlang:system_time(seconds))
			++ "-"
			++ integer_to_list(erlang:unique_integer([positive]))),
    test_server:start_node(Name, slave, [{args, Opts++" -pa "++Pa}]).

stop_node(Node) when Node =:= node() -> ok;
stop_node(Node) ->
    test_server:stop_node(Node).

free_memory() ->
    %% Free memory in MB.
    try
	SMD = memsup:get_system_memory_data(),
	{value, {free_memory, Free}} = lists:keysearch(free_memory, 1, SMD),
	TotFree = (Free +
		   case lists:keysearch(cached_memory, 1, SMD) of
		       {value, {cached_memory, Cached}} -> Cached;
		       false -> 0
		   end +
		   case lists:keysearch(buffered_memory, 1, SMD) of
		       {value, {buffered_memory, Buffed}} -> Buffed;
		       false -> 0
		   end),
	TotFree div (1024*1024)
    catch
	error : undef ->
	    ct:fail({"os_mon not built"})
    end.

