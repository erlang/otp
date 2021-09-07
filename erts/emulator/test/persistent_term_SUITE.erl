%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2020. All Rights Reserved.
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

-module(persistent_term_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,suite/0,init_per_suite/1,end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2,
	 basic/1,purging/1,sharing/1,get_trapping/1,
         destruction/1,
         get_all_race/1,
         info/1,info_trapping/1,killed_while_trapping/1,
         off_heap_values/1,keys/1,collisions/1,
         init_restart/1, put_erase_trapping/1,
         killed_while_trapping_put/1,
         killed_while_trapping_erase/1,
         error_info/1,
	 whole_message/1,
	 non_message_signal/1]).

%%
-export([test_init_restart_cmd/1]).

%% Test writing helper
-export([find_colliding_keys/0]).


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,10}}].

all() ->
    [basic,purging,sharing,get_trapping,info,info_trapping,
     destruction,
     get_all_race,
     killed_while_trapping,off_heap_values,keys,collisions,
     init_restart, put_erase_trapping, killed_while_trapping_put,
     killed_while_trapping_erase,
     error_info,
     whole_message,
     non_message_signal].

init_per_suite(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    %% Put a term in the dict so that we know that the testcases handle
    %% stray terms left by stdlib or other test suites.
    persistent_term:put(init_per_suite, {?MODULE}),
    Config.

end_per_suite(Config) ->
    persistent_term:erase(init_per_suite),
    erts_debug:set_internal_state(available_internal_state, false),
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok;
end_per_testcase(get_all_race, _Config) ->
    get_all_race_cleanup(),
    ok.

basic(_Config) ->
    Chk = chk(),
    N = 777,
    Seq = lists:seq(1, N),
    par(2, N, Seq, Chk),
    seq(3, Seq, Chk),
    seq(3, Seq, Chk),                                %Same values.
    _ = [begin
             Key = {?MODULE,{key,I}},
             true = persistent_term:erase(Key),
             false = persistent_term:erase(Key),
             {'EXIT',{badarg,_}} = (catch persistent_term:get(Key)),
             {not_present,Key} = persistent_term:get(Key, {not_present,Key})
         end || I <- Seq],
    [] = [P || {{?MODULE,_},_}=P <- pget(Chk)],
    chk(Chk).

par(C, N, Seq, Chk) ->
    _ = [spawn_link(fun() ->
                            ok = persistent_term:put({?MODULE,{key,I}},
                                                     {value,C*I})
                    end) || I <- Seq],
    Result = wait(N, Chk),
    _ = [begin
             Double = C*I,
             {{?MODULE,{key,I}},{value,Double}} = Res
         end || {I,Res} <- lists:zip(Seq, Result)],
    ok.

seq(C, Seq, Chk) ->
    _ = [ok = persistent_term:put({?MODULE,{key,I}}, {value,C*I}) ||
            I <- Seq],
    All = pget(Chk),
    All = [P || {{?MODULE,_},_}=P <- All],
    All = [{Key,persistent_term:get(Key)} || {Key,_} <- All],
    Result = lists:sort(All),
    _ = [begin
             Double = C*I,
             {{?MODULE,{key,I}},{value,Double}} = Res
         end || {I,Res} <- lists:zip(Seq, Result)],
    ok.

wait(N, Chk) ->
    All = [P || {{?MODULE,_},_}=P <- pget(Chk)],
    case length(All) of
        N ->
            All = [{Key,persistent_term:get(Key)} || {Key,_} <- All],
            lists:sort(All);
        _ ->
            receive after 10 -> ok end,
            wait(N, Chk)
    end.

%% Make sure that terms that have been erased are copied into all
%% processes that still hold a pointer to them.

purging(_Config) ->
    Chk = chk(),
    do_purging(fun(K) -> persistent_term:put(K, {?MODULE,new}) end,
               replaced),
    do_purging(fun persistent_term:erase/1, erased),
    chk(Chk).

do_purging(Eraser, Type) ->
    Parent = self(),
    Key = {?MODULE,?FUNCTION_NAME},
    ok = persistent_term:put(Key, {term,[<<"abc",0:777/unit:8>>]}),
    Ps0 = [spawn_monitor(fun() -> purging_tester(Parent, Key) end) ||
              _ <- lists:seq(1, 50)],
    Ps = maps:from_list(Ps0),
    purging_recv(gotten, Ps),
    Eraser(Key),
    _ = [P ! {Parent,Type} || P <- maps:keys(Ps)],
    purging_wait(Ps).

purging_recv(Tag, Ps) when map_size(Ps) > 0 ->
    receive
        {Pid,Tag} ->
            true = is_map_key(Pid, Ps),
            purging_recv(Tag, maps:remove(Pid, Ps))
    end;
purging_recv(_, _) -> ok.

purging_wait(Ps) when map_size(Ps) > 0 ->
    receive
        {'DOWN',Ref,process,Pid,Reason} ->
            normal = Reason,
            Ref = map_get(Pid, Ps),
            purging_wait(maps:remove(Pid, Ps))
    end;
purging_wait(_) -> ok.

purging_tester(Parent, Key) ->
    Term = persistent_term:get(Key),
    purging_check_term(Term),
    0 = erts_debug:size_shared(Term),
    Parent ! {self(),gotten},
    receive
        {Parent,erased} ->
            {'EXIT',{badarg,_}} = (catch persistent_term:get(Key)),
            purging_tester_1(Term, 1);
        {Parent,replaced} ->
            {?MODULE,new} = persistent_term:get(Key),
            purging_tester_1(Term, 1)
    end.

%% Wait for the term to be copied into this process.
purging_tester_1(Term, Timeout) ->
    purging_check_term(Term),
    receive after Timeout -> ok end,
    case erts_debug:size_shared(Term) of
        0 ->
            case Timeout of
                1000 ->
                    flush_later_ops(),
                    purging_tester_1(Term, 1);
                _ ->
                    purging_tester_1(Term, Timeout*10)
            end;
        Size ->
            %% The term has been copied into this process.
            purging_check_term(Term),
            Size = erts_debug:size(Term)
    end.

purging_check_term({term,[<<"abc",0:777/unit:8>>]}) ->
    ok.

%% Make sure terms are really deallocated when overwritten or erased.
destruction(Config) ->
    ok = erts_test_destructor:init(Config),

    NKeys = 100,
    Keys = lists:seq(0,NKeys-1),
    [begin
         V = erts_test_destructor:send(self(), K),
         persistent_term:put({?MODULE,K}, V)
     end
     || K <- Keys],

    %% Erase or overwrite all keys in "random" order.
    lists:foldl(fun(_, K) ->
                        case erlang:phash2(K) band 1 of
                            0 ->
                                %%io:format("erase key ~p\n", [K]),
                                persistent_term:erase({?MODULE,K});
                            1 ->
                                %%io:format("replace key ~p\n", [K]),
                                persistent_term:put({?MODULE,K}, value)
                        end,
                        (K + 13) rem NKeys
                end,
                17, Keys),

    destruction_1(Keys).

destruction_1(Keys) ->
    erlang:garbage_collect(),

    %% Receive all destruction messages
    MsgLst = destruction_recv(length(Keys), [], 2),
    ok = case lists:sort(MsgLst) of
             Keys ->
                 ok;
             _ ->
                 io:format("GOT ~p\n", [MsgLst]),
                 io:format("MISSING ~p\n", [Keys -- MsgLst]),
                 error
         end,

    %% Cleanup all remaining
    [persistent_term:erase({?MODULE,K}) || K <- Keys],
    ok.

destruction_recv(0, Acc, _) ->
    Acc;
destruction_recv(N, Acc, Flush) ->
    receive M ->
            destruction_recv(N-1, [M | Acc], Flush)
    after 1000 ->
            io:format("TIMEOUT. Missing ~p destruction messages.\n", [N]),
            case Flush of
                0 ->
                    Acc;
                _ ->
                    io:format("Try flush last literal area cleanup...\n"),
                    flush_later_ops(),
                    destruction_recv(N, Acc, Flush-1)
            end
    end.

%% Both persistent_term itself and erts_literal_are_collector use
%% erts_schedule_thr_prgr_later_cleanup_op() to schedule purge and deallocation
%% of literals. To avoid waiting forever on sleeping schedulers we flush
%% all later ops to make these cleanup jobs go through.
flush_later_ops() ->
    try
        erts_debug:set_internal_state(wait, thread_progress)
    catch
        error:system_limit ->
            ok % already ongoing; called by other process
    end,
    ok.


%% Test that sharing is preserved when storing terms.

sharing(_Config) ->
    Chk = chk(),
    Depth = 10,
    Size = 2*Depth,
    Shared = lists:foldl(fun(_, A) -> [A|A] end,
                         [], lists:seq(1, Depth)),
    Size = erts_debug:size(Shared),
    Key = {?MODULE,?FUNCTION_NAME},
    ok = persistent_term:put(Key, Shared),
    SharedStored = persistent_term:get(Key),
    Size = erts_debug:size(SharedStored),
    0 = erts_debug:size_shared(SharedStored),

    {Pid,Ref} = spawn_monitor(fun() ->
                                      Term = persistent_term:get(Key),
                                      Size = erts_debug:size(Term),
                                      0 = erts_debug:size_shared(Term),
                                      true = Term =:= SharedStored
                              end),
    receive
        {'DOWN',Ref,process,Pid,normal} ->
            true = persistent_term:erase(Key),
            Size = erts_debug:size(SharedStored),
            chk(Chk)
    end.

%% Test trapping of persistent_term:get/0.

get_trapping(_Config) ->
    Chk = chk(),

    %% Assume that the get/0 traps after 4000 iterations
    %% in a non-debug emulator.
    N = case test_server:timetrap_scale_factor() of
            1 -> 10000;
            _ -> 1000
        end,
    spawn_link(fun() -> get_trapping_create(N) end),
    All = do_get_trapping(N, [], Chk),
    N = get_trapping_check_result(lists:sort(All), 1),
    erlang:garbage_collect(),
    get_trapping_erase(N),
    chk(Chk).

do_get_trapping(N, Prev, Chk) ->
    case pget(Chk) of
        Prev when length(Prev) >= N ->
            All = [P || {{?MODULE,{get_trapping,_}},_}=P <- Prev],
            case length(All) of
                N -> All;
                _ -> do_get_trapping(N, Prev, Chk)
            end;
        New ->
            receive after 1 -> ok end,
            do_get_trapping(N, New, Chk)
    end.

get_trapping_create(0) ->
    ok;
get_trapping_create(N) ->
    ok = persistent_term:put({?MODULE,{get_trapping,N}}, N),
    get_trapping_create(N-1).

get_trapping_check_result([{{?MODULE,{get_trapping,N}},N}|T], N) ->
    get_trapping_check_result(T, N+1);
get_trapping_check_result([], N) -> N-1.

get_trapping_erase(0) ->
    ok;
get_trapping_erase(N) ->
    true = persistent_term:erase({?MODULE,{get_trapping,N}}),
    get_trapping_erase(N-1).

%% Test retrieving information about persistent terms.

info(_Config) ->
    Chk = chk(),

    %% White box test of info/0.
    N = 100,
    try
        Overhead = info_literal_area_overhead(),
        io:format("Overhead = ~p\n", [Overhead]),
        info_wb(N, Overhead, info_info())
    after
        _ = [_ = persistent_term:erase({?MODULE,I}) ||
                I <- lists:seq(1, N)]
    end,

    chk(Chk).

%% White box test of persistent_term:info/0. We take into account
%% that there might already exist persistent terms (created by the
%% OTP standard libraries), but we assume that they are not
%% changed during the execution of this test case.

info_wb(0, _, _) ->
    ok;
info_wb(N, Overhead, {BaseCount,BaseMemory}) ->
    Key = {?MODULE,N},
    Value = lists:seq(1, N),
    ok = persistent_term:put(Key, Value),

    %% Calculate the extra memory needed for this term.
    WordSize = erlang:system_info(wordsize),
    ExtraMemory = Overhead + 2 * N * WordSize,

    %% Call persistent_term:info/0.
    {Count,Memory} = info_info(),

    %% There should be one more persistent term.
    Count = BaseCount + 1,

    %% Verify that the amount of memory is correct.
    case BaseMemory + ExtraMemory of
        Memory ->
            %% Exactly right. The size of the hash table was not changed.
            ok;
        Expected ->
            %% The size of the hash table has been doubled to avoid filling
            %% the table to more than 50 percent. The previous number
            %% of entries must have been exactly half the size of the
            %% hash table. The expected number of extra words added by
            %% the resizing will be twice that number.
            ExtraWords = BaseCount * 2,
            true = ExtraWords * WordSize =:= (Memory - Expected)
    end,
    info_wb(N-1, Overhead, {Count,Memory}).

info_info() ->
    #{count:=Count,memory:=Memory} = persistent_term:info(),
    true = is_integer(Count) andalso Count >= 0,
    true = is_integer(Memory) andalso Memory >= 0,
    {Count,Memory}.

%% Calculate the number of extra bytes needed for storing each term in
%% the literal, assuming that the key is a tuple of size 2 with
%% immediate elements. The calculated number is the size of the
%% ErtsLiteralArea struct excluding the storage for the literal term
%% itself.

info_literal_area_overhead() ->
    Key1 = {?MODULE,1},
    Key2 = {?MODULE,2},
    #{memory:=Mem0} = persistent_term:info(),
    ok = persistent_term:put(Key1, literal),
    #{memory:=Mem1} = persistent_term:info(),
    ok = persistent_term:put(Key2, literal),
    #{memory:=Mem2} = persistent_term:info(),
    true = persistent_term:erase(Key1),
    true = persistent_term:erase(Key2),

    %% The size of the hash table may have doubled when inserting
    %% one of the keys. To avoiding counting the change in the hash
    %% table size, take the smaller size increase.
    min(Mem2-Mem1, Mem1-Mem0).

%% Test trapping of persistent_term:info/0.

info_trapping(_Config) ->
    Chk = chk(),

    %% Assume that the info/0 traps after 4000 iterations
    %% in a non-debug emulator.
    N = case test_server:timetrap_scale_factor() of
            1 -> 10000;
            _ -> 1000
        end,
    spawn_link(fun() -> info_trapping_create(N) end),
    All = do_info_trapping(N, 0, Chk),
    N = info_trapping_check_result(lists:sort(All), 1),
    erlang:garbage_collect(),
    info_trapping_erase(N),
    chk(Chk).

do_info_trapping(N, PrevMem, Chk) ->
    case info_info() of
        {M,Mem} when M >= N ->
            true = Mem >= PrevMem,
            All = [P || {{?MODULE,{info_trapping,_}},_}=P <- pget(Chk)],
            case length(All) of
                N -> All;
                _ -> do_info_trapping(N, PrevMem, Chk)
            end;
        {_,Mem} ->
            true = Mem >= PrevMem,
            receive after 1 -> ok end,
            do_info_trapping(N, Mem, Chk)
    end.

info_trapping_create(0) ->
    ok;
info_trapping_create(N) ->
    ok = persistent_term:put({?MODULE,{info_trapping,N}}, N),
    info_trapping_create(N-1).

info_trapping_check_result([{{?MODULE,{info_trapping,N}},N}|T], N) ->
    info_trapping_check_result(T, N+1);
info_trapping_check_result([], N) -> N-1.

info_trapping_erase(0) ->
    ok;
info_trapping_erase(N) ->
    true = persistent_term:erase({?MODULE,{info_trapping,N}}),
    info_trapping_erase(N-1).

%% Test that hash tables are deallocated if a process running
%% persistent_term:get/0 is killed.

killed_while_trapping(_Config) ->
    Chk = chk(),
    N = case test_server:timetrap_scale_factor() of
            1 -> 20000;
            _ -> 2000
        end,
    kwt_put(N),
    kwt_spawn(10),
    kwt_erase(N),
    chk(Chk).

kwt_put(0) ->
    ok;
kwt_put(N) ->
    ok = persistent_term:put({?MODULE,{kwt,N}}, N),
    kwt_put(N-1).

kwt_spawn(0) ->
    ok;
kwt_spawn(N) ->
    Pids = [spawn(fun kwt_getter/0) || _ <- lists:seq(1, 20)],
    erlang:yield(),
    _ = [exit(Pid, kill) || Pid <- Pids],
    kwt_spawn(N-1).

kwt_getter() ->
    _ = persistent_term:get(),
    kwt_getter().

kwt_erase(0) ->
    ok;
kwt_erase(N) ->
    true = persistent_term:erase({?MODULE,{kwt,N}}),
    kwt_erase(N-1).

%% Test storing off heap values (such as ref-counted binaries).

off_heap_values(_Config) ->
    Chk = chk(),
    Key = {?MODULE,?FUNCTION_NAME},
    Val = {a,list_to_binary(lists:seq(0, 255)),make_ref(),fun() -> ok end},
    ok = persistent_term:put(Key, Val),
    FetchedVal = persistent_term:get(Key),
    Val = FetchedVal,
    true = persistent_term:erase(Key),
    off_heap_values_wait(FetchedVal, Val),
    chk(Chk).

off_heap_values_wait(FetchedVal, Val) ->
    case erts_debug:size_shared(FetchedVal) of
        0 ->
            Val = FetchedVal,
            ok;
        _ ->
            erlang:yield(),
            off_heap_values_wait(FetchedVal, Val)
    end.

%% Test some more data types as keys. Use the module name as a key
%% to minimize the risk of collision with any key used
%% by the OTP libraries.

keys(_Config) ->
    Chk = chk(),
    do_key(?MODULE),
    do_key([?MODULE]),
    do_key(?MODULE_STRING),
    do_key(list_to_binary(?MODULE_STRING)),
    chk(Chk).

do_key(Key) ->
    Val = term_to_binary(Key),
    ok = persistent_term:put(Key, Val),
    StoredVal = persistent_term:get(Key),
    Val = StoredVal,
    true = persistent_term:erase(Key).

%% Create persistent terms with keys that are known to collide.
%% Delete them in random order, making sure that all others
%% terms can still be found.

collisions(_Config) ->
    Chk = chk(),

    %% Create persistent terms with random keys.
    Keys = lists:flatten(colliding_keys()),
    Kvs = [{K,rand:uniform(1000)} || K <- Keys],
    _ = [ok = persistent_term:put(K, V) || {K,V} <- Kvs],
    _ = [V = persistent_term:get(K) || {K,V} <- Kvs],

    %% Now delete the persistent terms in random order.
    collisions_delete(lists:keysort(2, Kvs), Chk),

    chk(Chk).

collisions_delete([{Key,Val}|Kvs], Chk) ->
    Val = persistent_term:get(Key),
    true = persistent_term:erase(Key),
    true = lists:sort(pget(Chk)) =:= lists:sort(Kvs),
    _ = [V = persistent_term:get(K) || {K,V} <- Kvs],
    collisions_delete(Kvs, Chk);
collisions_delete([], _) ->
    ok.

colliding_keys() ->
    %% Collisions found by find_colliding_keys() below
    L = [[77674392,148027],
	 [103370644,950908],
	 [106444046,870178],
	 [22217246,735880],
	 [18088843,694607],
	 [63426007,612179],
	 [117354942,906431],
	 [121434305,94282311,816072],
	 [118441466,93873772,783366],
	 [124338174,1414801,123089],
	 [20240282,17113486,923647],
	 [126495528,61463488,164994],
	 [125341723,5729072,445539],
	 [127450932,80442669,348245],
	 [123354692,85724182,14241288,180793],
	 [99159367,65959274,61680971,289939],
	 [107637580,104512101,62639807,181644],
	 [139547511,51654420,2062545,151944],
	 [88078274,73031465,53388204,428872],
	 [141314238,75761379,55699508,861797],
	 [88045216,59272943,21030492,180903]],

    %% Verify that the keys still collide (this will fail if the
    %% internal hash function has been changed).
    case erlang:system_info(wordsize) of
        8 ->
            verify_colliding_keys(L);
        4 ->
            %% Not guaranteed to collide on a 32-bit system.
            ok
    end,

    L.

verify_colliding_keys([[K|Ks]|Gs]) ->
    Hash = internal_hash(K),
    [Hash] = lists:usort([internal_hash(Key) || Key <- Ks]),
    verify_colliding_keys(Gs);
verify_colliding_keys([]) ->
    ok.

internal_hash(Term) ->
    erts_debug:get_internal_state({internal_hash,Term}).

%% Use this function to (re)generate the list in colliding_keys/0
find_colliding_keys() ->
    MaxCollSz = 4,
    OfEachSz = 7,
    erts_debug:set_internal_state(available_internal_state, true),
    MaxInserts = 1 bsl 20,
    T = ets:new(x, [set, private]),
    ok = fck_loop_1(T, 1, MaxInserts, MaxCollSz, OfEachSz),
    fck_collect(T, MaxCollSz, OfEachSz, []).

fck_collect(_T, 1, _OfEachSz, Acc) ->
    Acc;
fck_collect(T, CollSz, OfEachSz, Acc) ->
    {List, _} = ets:select(T,
			   [{{'$1','$2'}, [{'==',{length,'$2'},CollSz}], ['$2']}],
			   OfEachSz),
    fck_collect(T, CollSz-1, OfEachSz, List ++ Acc).


fck_loop_1(T, Key, 0, MaxCollSz, MaxSzLeft) ->
    fck_loop_2(T, Key, MaxCollSz, MaxSzLeft);
fck_loop_1(T, Key, Inserts, MaxCollSz, MaxSzLeft) ->
    Hash = internal_hash(Key),
    case ets:insert_new(T, {Hash, [Key]}) of
	true ->
	    fck_loop_1(T, Key+1, Inserts-1, MaxCollSz, MaxSzLeft);
	false ->
	    [{Hash, KeyList}] = ets:lookup(T, Hash),
	    true = ets:insert(T, {Hash, [Key | KeyList]}),
	    fck_loop_1(T, Key+1, Inserts, MaxCollSz, MaxSzLeft)
    end.

fck_loop_2(_T, _Key, _MaxCollSz, 0) ->
    ok;
fck_loop_2(T, Key, MaxCollSz, MaxSzLeft0) ->
    Hash = internal_hash(Key),
    case ets:lookup(T, Hash) of
	[] ->
	    fck_loop_2(T, Key+1, MaxCollSz, MaxSzLeft0);
	[{Hash, KeyList}] ->
	    true = ets:insert(T, {Hash, [Key | KeyList]}),
	    MaxSzLeft1 = case length(KeyList)+1 of
			     MaxCollSz ->
				 MaxSzLeft0 - 1;
			     _ ->
				 MaxSzLeft0
			 end,
	    fck_loop_2(T, Key+1, MaxCollSz, MaxSzLeft1)
    end.



%% Test that all persistent terms are erased by init:restart/0.

init_restart(_Config) ->
    File = "command_file",
    ok = file:write_file(File, term_to_binary(restart)),
    {ok,[[Erl]]} = init:get_argument(progname),
    ModPath = filename:dirname(code:which(?MODULE)),
    Cmd = Erl ++ " -pa " ++ ModPath ++ " -noshell "
        "-run " ++ ?MODULE_STRING ++ " test_init_restart_cmd " ++
        File,
    io:format("~s\n", [Cmd]),
    Expected = "12ok",
    case os:cmd(Cmd) of
        Expected ->
            ok;
        Actual ->
            io:format("Expected: ~s", [Expected]),
            io:format("Actual:   ~s\n", [Actual]),
            ct:fail(unexpected_output)
    end.

test_init_restart_cmd([File]) ->
    try
        do_test_init_restart_cmd(File)
    catch
        C:R ->
            io:format("\n~p ~p\n", [C,R]),
            halt()
    end,
    receive
        _ -> ok
    end.

do_test_init_restart_cmd(File) ->
    {ok,Bin} = file:read_file(File),
    Seq = lists:seq(1, 50),
    case binary_to_term(Bin) of
        restart ->
            _ = [persistent_term:put({?MODULE,I}, {value,I}) ||
                    I <- Seq],
            ok = file:write_file(File, term_to_binary(was_restarted)),
            io:put_chars("1"),
            init:restart(),
            receive
                _ -> ok
            end;
        was_restarted ->
            io:put_chars("2"),
            ok = file:delete(File),
            _ = [begin
                     Key = {?MODULE,I},
                     {'EXIT',{badarg,_}} = (catch persistent_term:get(Key))
                 end || I <- Seq],
            io:put_chars("ok"),
            init:stop()
    end.

%% Test that the literal is copied when removed also when
%% the whole message is a literal...

whole_message(Config) when is_list(Config) ->
    whole_message_test(on_heap),
    whole_message_test(off_heap),
    ok.
    
whole_message_test(MQD) ->
    io:format("Testing on ~p~n", [MQD]),
    Go = make_ref(),
    Done = make_ref(),
    TestRef = make_ref(),
    Tester = self(),
    persistent_term:put(test_ref, TestRef),
    Pid = spawn_opt(fun () ->
                             receive Go -> ok end,
                             receive TestRef -> ok end,
                             receive TestRef -> ok end,
                             receive TestRef -> ok end,
                             receive [TestRef] -> ok end,
                             receive [TestRef] -> ok end,
                             receive [TestRef] -> ok end,
                             Tester ! Done
                     end, [link, {message_queue_data, MQD}]),
    Pid ! persistent_term:get(test_ref),
    Pid ! persistent_term:get(test_ref),
    Pid ! persistent_term:get(test_ref),
    %% Throw in some messages with a reference from the heap
    %% while we're at it...
    Pid ! [persistent_term:get(test_ref)],
    Pid ! [persistent_term:get(test_ref)],
    Pid ! [persistent_term:get(test_ref)],
    persistent_term:erase(test_ref),
    receive after 1000 -> ok end,
    Pid ! Go,
    receive Done -> ok end,
    unlink(Pid),
    exit(Pid, kill),
    false = is_process_alive(Pid),
    ok.

%% Check that there is the same number of persistents terms before
%% and after each test case.

chk() ->
    {xtra_info(), persistent_term:get()}.

chk({Info1, _Initial} = Chk) ->
    #{count := Count, memory := Memory1, table := Table1} = Info1,
    case xtra_info() of
        Info1 ->
            ok;
        #{count := Count, memory := Memory2, table := Table2}=Info2
          when Memory2 > Memory1,
               Table2 > Table1 ->
            %% Check increased memory is only table growth hysteresis
            MemDiff = Memory2 - Memory1,
            TabDiff = (Table2 - Table1) * erlang:system_info(wordsize),
            {MemDiff,MemDiff} = {MemDiff, TabDiff},

            case (Count / Table2) of
                Load when Load >= 0.25 ->
                    ok;
                _ ->
                    chk_fail("Hash table too large", Info1, Info2)
            end;
        Info2 ->
            chk_fail("Memory diff", Info1, Info2)
    end,
    Key = {?MODULE,?FUNCTION_NAME},
    ok = persistent_term:put(Key, {term,Info1}),
    Term = persistent_term:get(Key),
    true = persistent_term:erase(Key),
    chk_not_stuck(Term, 1),
    [persistent_term:erase(K) || {K, _} <- pget(Chk)],
    ok.

xtra_info() ->
    maps:merge(persistent_term:info(),
               erts_debug:get_internal_state(persistent_term)).

chk_fail(Error, Info1, Info2) ->
    io:format("Info1 = ~p\n", [Info1]),
    io:format("Info2 = ~p\n", [Info2]),
    ct:fail(Error).

chk_not_stuck(Term, Timeout) ->
    %% Hash tables to be deleted are put onto a queue.
    %% Make sure that the queue isn't stuck by a table with
    %% a non-zero ref count.

    case erts_debug:size_shared(Term) of
        0 ->
            receive after Timeout -> ok end,
            case Timeout of
                1000 ->
                    flush_later_ops(),
                    chk_not_stuck(Term, 1);
                _ ->
                    chk_not_stuck(Term, Timeout*10)
            end;
        _ ->
            ok
    end.

pget({_, Initial}) ->
    persistent_term:get() -- Initial.


killed_while_trapping_put(_Config) ->
    repeat(
      fun() ->
              NrOfPutsInChild = 10000,
              do_puts(2500, my_value),
              Pid =
                  spawn(fun() ->
                                do_puts(NrOfPutsInChild, my_value2)
                        end),
              timer:sleep(1),
              erlang:exit(Pid, kill),
              do_erases(NrOfPutsInChild)
      end,
      10),
    ok.

killed_while_trapping_erase(_Config) ->
    repeat(
      fun() ->
              NrOfErases = 2500,
              do_puts(NrOfErases, my_value),
              Pid =
                  spawn(fun() ->
                                do_erases(NrOfErases)
                        end),
              timer:sleep(1),
              erlang:exit(Pid, kill),
              do_erases(NrOfErases)
      end,
      10),
    ok.

put_erase_trapping(_Config) ->
    NrOfItems = 5000,
    do_puts(NrOfItems, first),
    do_puts(NrOfItems, second),
    do_erases(NrOfItems),
    ok.

do_puts(0, _) -> ok;
do_puts(NrOfPuts, ValuePrefix) ->
    Key = {?MODULE, NrOfPuts},
    Value = {ValuePrefix, NrOfPuts},
    erts_debug:set_internal_state(reds_left, rand:uniform(250)),
    persistent_term:put(Key, Value),
    Value = persistent_term:get(Key),
    do_puts(NrOfPuts - 1, ValuePrefix).

do_erases(0) -> ok;
do_erases(NrOfErases) ->
    Key = {?MODULE,NrOfErases},
    erts_debug:set_internal_state(reds_left, rand:uniform(500)),
    persistent_term:erase(Key),
    not_found = persistent_term:get(Key, not_found),
    do_erases(NrOfErases - 1).

repeat(_Fun, 0) ->
    ok;
repeat(Fun, N) ->
    Fun(),
    repeat(Fun, N-1).

error_info(_Config) ->
    L = [{erase, [{?MODULE,my_key}], [no_fail]},
         {get, [{?MODULE,certainly_not_existing}]},
         {get, [{?MODULE,certainly_not_existing}, default], [no_fail]},
         {put, 2}                               %Can't fail.
        ],
    do_error_info(L).

do_error_info(L0) ->
    L1 = lists:foldl(fun({_,A}, Acc) when is_integer(A) -> Acc;
                        ({F,A}, Acc) -> [{F,A,[]}|Acc];
                        ({F,A,Opts}, Acc) -> [{F,A,Opts}|Acc]
                     end, [], L0),
    Tests = ordsets:from_list([{F,length(A)} || {F,A,_} <- L1] ++
                                  [{F,A} || {F,A} <- L0, is_integer(A)]),
    Bifs0 = [{F,A} || {F,A} <- persistent_term:module_info(exports),
                      A =/= 0,
                      F =/= module_info],
    Bifs = ordsets:from_list(Bifs0),
    NYI = [{F,lists:duplicate(A, '*'),nyi} || {F,A} <- Bifs -- Tests],
    L = lists:sort(NYI ++ L1),
    do_error_info(L, []).

do_error_info([{_,Args,nyi}=H|T], Errors) ->
    case lists:all(fun(A) -> A =:= '*' end, Args) of
        true ->
            do_error_info(T, [{nyi,H}|Errors]);
        false ->
            do_error_info(T, [{bad_nyi,H}|Errors])
    end;
do_error_info([{F,Args,Opts}|T], Errors) ->
    eval_bif_error(F, Args, Opts, T, Errors);
do_error_info([], Errors0) ->
    case lists:sort(Errors0) of
        [] ->
            ok;
        [_|_]=Errors ->
            io:format("\n~p\n", [Errors]),
            ct:fail({length(Errors),errors})
    end.

eval_bif_error(F, Args, Opts, T, Errors0) ->
    try apply(persistent_term, F, Args) of
        Result ->
            case lists:member(no_fail, Opts) of
                true ->
                    do_error_info(T, Errors0);
                false ->
                    do_error_info(T, [{should_fail,{F,Args},Result}|Errors0])
            end
    catch
        error:Reason:Stk ->
            SF = fun(Mod, _, _) -> Mod =:= test_server end,
            Str = erl_error:format_exception(error, Reason, Stk, #{stack_trim_fun => SF}),
            BinStr = iolist_to_binary(Str),
            ArgStr = lists:join(", ", [io_lib:format("~p", [A]) || A <- Args]),
            io:format("\nerlang:~p(~s)\n~ts", [F,ArgStr,BinStr]),

            case Stk of
                [{persistent_term,ActualF,ActualArgs,Info}|_] ->
                    RE = <<"[*][*][*] argument \\d+:">>,
                    Errors1 = case re:run(BinStr, RE, [{capture, none}]) of
                                  match ->
                                      Errors0;
                                  nomatch when Reason =:= system_limit ->
                                      Errors0;
                                  nomatch ->
                                      [{no_explanation,{F,Args},Info}|Errors0]
                              end,

                    Errors = case {ActualF,ActualArgs} of
                                 {F,Args} ->
                                     Errors1;
                                 _ ->
                                     [{renamed,{F,length(Args)},{ActualF,ActualArgs}}|Errors1]
                             end,

                    do_error_info(T, Errors);
                _ ->
                    Errors = [{renamed,{F,length(Args)},hd(Stk)}|Errors0],
                    do_error_info(T, Errors)
            end
    end.


%% OTP-17298
get_all_race(_Config) ->
    N = 20 * erlang:system_info(schedulers_online),
    persistent_term:put(get_all_race, N),
    SPs = [spawn_link(fun() -> gar_setter(Seq) end) || Seq <- lists:seq(1, N)],
    GPs = [spawn_link(fun gar_getter/0) || _ <- lists:seq(1, N)],
    receive after 2000 -> ok end,
    [begin unlink(Pid), exit(Pid,kill) end || Pid <- (SPs ++ GPs)],
    ok.

get_all_race_cleanup() ->
    N = persistent_term:get(get_all_race, 0),
    _ = persistent_term:erase(get_all_race),
    [_ = persistent_term:erase(Seq) || Seq <- lists:seq(1, N)],
    ok.

gar_getter() ->
    erts_debug:set_internal_state(reds_left, 1),
    _ = persistent_term:get(),
    gar_getter().

gar_setter(Key) ->
    erts_debug:set_internal_state(reds_left, 1),
    persistent_term:erase(Key),
    persistent_term:put(Key, {complex, term}),
    gar_setter(Key).

%% Test that literals in non-message signals are copied
%% when removed...
%%
%% Currently the only non-message signal that may carry
%% literals are alias-message signals. Exit signals have
%% been added to the test since they should be added
%% next...

non_message_signal(Config) when is_list(Config) ->
    non_message_signal_test(on_heap),
    non_message_signal_test(off_heap),
    ok.

non_message_signal_test(MQD) ->
    io:format("Testing on ~p~n", [MQD]),
    process_flag(scheduler, 1),
    process_flag(priority, max),
    MultiSched = erlang:system_info(schedulers) > 1,
    {TokOpts, RecvOpts}
        = case MultiSched of
              true ->
                  {[link, {scheduler, 2}, {priority, high}],
                   [link, {scheduler, 2}, {priority, low},
		    {message_queue_data, MQD}]};
              false ->
                  {[link], [link, {message_queue_data, MQD}]}
          end,
    RecvPrepared = make_ref(),
    TokPrepared = make_ref(),
    Go = make_ref(),
    Done = make_ref(),
    TestRef = make_ref(),
    Tester = self(),
    persistent_term:put(test_ref, TestRef),
    Pid = spawn_opt(fun () ->
			    process_flag(trap_exit, true),
                            Tester ! {RecvPrepared, alias()},
                            receive Go -> ok end,
			    recv_msg(TestRef, 50),
			    recv_msg([TestRef], 50),
                            recv_msg({'EXIT', Tester, TestRef}, 50),
                            recv_msg({'EXIT', Tester, [TestRef]}, 50),
                            Tester ! Done
                    end, RecvOpts),
    Alias = receive {RecvPrepared, A} -> A end,
    Tok = spawn_opt(fun () ->
                            Tester ! TokPrepared,
                            tok_loop()
                    end, TokOpts),
    receive TokPrepared -> ok end,
    lists:foreach(fun (_) -> Alias ! persistent_term:get(test_ref) end,
		  lists:seq(1, 50)),
    lists:foreach(fun (_) -> Alias ! [persistent_term:get(test_ref)] end,
		  lists:seq(1, 50)),
    lists:foreach(fun (_) -> exit(Pid, persistent_term:get(test_ref)) end,
		  lists:seq(1, 50)),
    lists:foreach(fun (_) -> exit(Pid, [persistent_term:get(test_ref)]) end,
		  lists:seq(1, 50)),
    persistent_term:erase(test_ref),
    receive after 1000 -> ok end,
    unlink(Tok),
    exit(Tok, kill),
    receive after 1000 -> ok end,
    Pid ! Go,
    receive Done -> ok end,
    unlink(Pid),
    exit(Pid, kill),
    false = is_process_alive(Pid),
    false = is_process_alive(Tok),
    ok.

recv_msg(_Msg, 0) ->
    ok;
recv_msg(Msg, N) ->
    receive
        Msg ->
            recv_msg(Msg, N-1)
    end.

tok_loop() ->
    tok_loop().
