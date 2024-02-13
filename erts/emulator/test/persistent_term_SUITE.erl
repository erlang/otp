%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2024. All Rights Reserved.
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
         shared_magic_ref/1,
	 non_message_signal/1,
         get_put_colliding_bucket/1]).

%%
-export([test_init_restart_cmd/1]).

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
     shared_magic_ref,
     non_message_signal,
     get_put_colliding_bucket].

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

end_per_testcase(get_all_race, _Config) ->
    get_all_race_cleanup(),
    ok;
end_per_testcase(_, _Config) ->
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
    Mask = 16#FFFFFFFF,

    %% Collisions found by find_colliding_keys(Mask) in `map_SUITE`.
    ByMethod = #{
        %% 64-bit internal hash of `0`
        15677855740172624429 =>
            [[-4294967296,-3502771103,1628104549],
             [-2750312253,-2208396507,-2147483648,1926198452,3660971145],
             [-2542330914,-1089175976,-1073741824,290495829],
             [-2155350068,0],
             [1073741824,2807978463,3625918826],
             [-1032333168,-705082324,1541401419,1594347321,2147483648,
              2266580263,2823045213],
             [-2465550512,3221225472],
             [2854075383,651030299,-1581781966,-3419595364,-4294967295],
             [3351133532,968011333,-2217176682,-4294967294],
             [598547769,-1379599129,-4294967293],
             [-649195724,-4294967292],
             [2943767758,-645518858,-875893937,-1294474094,-4294967291],
             [3255309205,-2208705073,-4294967290],
             [2162086262,-3745041100,-4294967288],
             [-36087602,-1146855151,-1687820340,-3221225471],
             [4177844763,3846951687,3485974116,3175597814,590007752,
              -3221225470],
             [3264460518,1553643847,1183174568,-3221225469],
             [-577423597,-3221225468,-3522984153],
             [3855876603,3019389034,-1323003840,-2576022240,-3221225467],
             [-471176452,-3221225466],
             [-1122194611,-3221225465,-4210494386],
             [3603262778,994932591,-1788155141,-1921175318,-3221225464],
             [3836440544,-1003007187,-2147483647],
             [-2051344765,-2147483646],
             [3650711544,-2147483645,-2799381711,-3556915274],
             [3489936963,1240642555,-2147483644,-3957745840],
             [1085161678,-2052366093,-2147483643,-3483479006],
             [1939744936,-2147483642,-3856508363],
             [-566163246,-2060332302,-2147483641,-4230104575],
             [1203359280,237551462,-1073741823],
             [1727228961,-813544803,-1073741822,-1309725727,-1666872574,
              -2203000992],
             [3698637395,3362609925,876970478,-714241238,-1073741821],
             [1765842640,-354951691,-566902540,-1073741820],
             [3963091352,2371749084,591553116,-1073741819],
             [-1073741817,-2715118400],
             [-1073741816,-3224015310],
             [2762405117,1,-2123671186],
             [2470477117,2,-331878960,-2322233731],
             [3815926349,2088957086,3],
             [1968999576,870968367,4,-1268233288,-3048698020],
             [979559827,5],
             [946684365,753214037,6,-2648059890],
             [3790852688,2964822264,2830450758,7,-3580232887],
             [1073741825,-3356417243,-3706053980],
             [1073741827,-2621798828],
             [1073741828,-2347690873],
             [2090309310,1073741830,-1375115411,-2016799213,-4267952630],
             [1073741831,672032559],
             [1073741832,-2577014530,-3065907606],
             [3796535022,2351766515,2147483649,-2136894649],
             [2280176922,2147483650],
             [4198987324,3244673818,2147483651,270823276,-2880202587],
             [3880317786,3256588678,2670024934,2147483652,-2327563310,
              -3284218582,-3844717086],
             [2178108296,2147483653,-3361345880],
             [2954325696,2147483654,-1059451308,-1331847237],
             [3189358149,2147483655,-1477948284,-1669797549,-3362853705,
              -3928750615],
             [2147483656,471953932,-355892383],
             [3221225473,-3995083753,-4092880912],
             [3221225474,-2207482759,-3373076062],
             [3221225475,2400978919,2246389041,1052806668,-781893221,
              -1811850779],
             [3221225476,-245369539,-1842612521],
             [3221225477,688232807],
             [3221225478,209327542,-2793530395],
             [3221225479,-2303080520,-4225327222],
             [4216539003,3221225480]],

        %% 32-bit internal hash of `0`
        416211501 =>
            [[-55973163,-134217697],[43918753,-134217684],
             [107875525,-134217667],[-30291033,-134217663],
             [-40285269,-111848095],[35020004,-111848056],
             [-44437601,-111848046],[103325476,-69901823,-111848030],
             [126809757,-111848012],[-92672406,-111848005],
             [-64199103,-111847990],[102238942,-111847982],
             [62106519,-89478468],[-89478462,-128994853],
             [-67899866,-89478412],[-45432484,-89478397],
             [120764819,-89478387],[9085208,-89478382],
             [10859155,-89478369],[45834467,-67108863],
             [-67108857,-124327693],[104597114,-67108847],
             [11918558,-67108783],[50986187,-67108760],
             [113683827,64978564,-67108752],
             [111972669,-67108751],[27085194,-44739227],
             [46760231,-44739221],[101248827,-44739220],
             [30692154,-44739176],[33768394,-44739117],
             [-12083942,-44739116],[-22369572,-112420685],
             [-22369568,-98812798],[-22369550,-78759395],
             [47792095,-22369543],[9899495,-22369540],
             [99744593,-22369511],[76325343,52],
             [122425143,68],[21651445,74],
             [129537216,119],[125,-110161190],
             [80229747,22369626],[22369629,-55742042],
             [128416574,22369631],[105267606,22369643],
             [22369693,-2286278],[126622985,22369698],
             [22369701,-13725583],[22369728,-22765683],
             [22369731,-54786216],[22369740,-65637968],
             [44739246,12048008],[44739259,-26636781],
             [126966693,44739272],[44739274,-130215175],
             [44739277,15051453],[44739292,17890441],
             [44739301,-72627814],[106949249,44739322],
             [44739323,-56882381],[67108879,-111259055],
             [67108888,37627968],[67108894,-53291767],
             [67108896,-127782577],[67108908,-1014167],
             [82796148,67108959],[67108962,-71355523],
             [67108984,-62077338,-77539719],[126106374,89478485],
             [89478488,85703113],[132215738,89478495],
             [89478515,-122049151],[89478518,-22611374],
             [94050181,89478530],[89478547,42736340],
             [89478553,86641584],[129419863,111848199],
             [111848217,-32493354],[112586988,111848229]]
    },

    HashKey = internal_hash(0),
    #{ HashKey := Keys } = ByMethod,

    verify_colliding_keys(Keys, Mask).

verify_colliding_keys([[K | Ks]=Group | Gs], Mask) ->
    Hash = internal_hash(K) band Mask,
    [Hash] = lists:usort([(internal_hash(Key) band Mask) || Key <- Ks]),
    [Group | verify_colliding_keys(Gs, Mask)];
verify_colliding_keys([], _Mask) ->
    [].

internal_hash(Term) ->
    erts_debug:get_internal_state({internal_hash,Term}).

%% OTP-17700 Bug skipped refc++ of shared magic reference
shared_magic_ref(_Config) ->
    Ref = atomics:new(10, []),
    persistent_term:put(shared_magic_ref, {Ref, Ref}),
    shared_magic_ref_cont().

shared_magic_ref_cont() ->
    erlang:garbage_collect(),
    {Ref, Ref} = persistent_term:get(shared_magic_ref),
    0 = atomics:get(Ref, 1),  %% would definitely fail on debug vm
    ok.


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

%% GH-5908: `persistent_term:get/1,2` could race with `persistent_term:put/2`
%% and return an unexpected value if `get/1,2` happened to _abort_ its lookup
%% in the same bucket that `put/2` was writing to.
get_put_colliding_bucket(_Config) ->
    [[Key, CollidesWith | _] | _] = colliding_keys(),
    false = persistent_term:erase(Key),

    {Pid, MRef} =
        spawn_monitor(fun() ->
                              Schedulers = erlang:system_info(schedulers),
                              [spawn_link(fun() -> gpcb_reader(Key) end)
                               || _ <- lists:seq(1, Schedulers - 1)],
                              gpcb_updater(CollidesWith)
                      end),

    %% The race usually gets detected within a second or two, so we'll consider
    %% the test passed if we can't reproduce it in 10 seconds.
    receive
        {'DOWN', MRef, process, Pid, Reason} ->
            ct:fail(Reason)
    after 10000 ->
            exit(Pid, kill),
            ok
    end.

gpcb_reader(Key) ->
    expected = persistent_term:get(Key, expected),
    gpcb_reader(Key).

gpcb_updater(CollidesWith) ->
    persistent_term:erase(CollidesWith),
    persistent_term:put(CollidesWith, unexpected),
    gpcb_updater(CollidesWith).
