%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
	 basic/1,purging/1,sharing/1,get_trapping/1,
         info/1,info_trapping/1,killed_while_trapping/1,
         off_heap_values/1,keys/1,collisions/1,
         init_restart/1, put_erase_trapping/1,
         killed_while_trapping_put/1,
         killed_while_trapping_erase/1]).

%%
-export([test_init_restart_cmd/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,10}}].

all() ->
    [basic,purging,sharing,get_trapping,info,info_trapping,
     killed_while_trapping,off_heap_values,keys,collisions,
     init_restart, put_erase_trapping, killed_while_trapping_put,
     killed_while_trapping_erase].

init_per_suite(Config) ->
    %% Put a term in the dict so that we know that the testcases handle
    %% stray terms left by stdlib or other test suites.
    persistent_term:put(init_per_suite, {?MODULE}),
    Config.

end_per_suite(Config) ->
    persistent_term:erase(init_per_suite),
    Config.

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
            purging_tester_1(Term);
        {Parent,replaced} ->
            {?MODULE,new} = persistent_term:get(Key),
            purging_tester_1(Term)
    end.

%% Wait for the term to be copied into this process.
purging_tester_1(Term) ->
    purging_check_term(Term),
    receive after 1 -> ok end,
    case erts_debug:size_shared(Term) of
        0 ->
            purging_tester_1(Term);
        Size ->
            %% The term has been copied into this process.
            purging_check_term(Term),
            Size = erts_debug:size(Term)
    end.

purging_check_term({term,[<<"abc",0:777/unit:8>>]}) ->
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
    %% Collisions found by Jesper L. Andersen for breaking maps.
    L = [[764492191,2361333849],
         [49527266765044,90940896816021,20062927283041,267080852079651],
         [249858369443708,206247021789428,20287304470696,25847120931175],
         [10645228898670,224705626119556,267405565521452,258214397180678],
         [264783762221048,166955943492306,98802957003141,102012488332476],
         [69425677456944,177142907243411,137138950917722,228865047699598],
         [116031213307147,29203342183358,37406949328742,255198080174323],
         [200358182338308,235207156008390,120922906095920,116215987197289],
         [58728890318426,68877471005069,176496507286088,221041411345780],
         [91094120814795,50665258299931,256093108116737,19777509566621],
         [74646746200247,98350487270564,154448261001199,39881047281135],
         [23408943649483,164410325820923,248161749770122,274558342231648],
         [169531547115055,213630535746863,235098262267796,200508473898303],
         [235098564415817,85039146398174,51721575960328,173069189684390],
         [176136386396069,155368359051606,147817099696487,265419485459634],
         [137542881551462,40028925519736,70525669519846,63445773516557],
         [173854695142814,114282444507812,149945832627054,99605565798831],
         [177686773562184,127158716984798,132495543008547],
         [227073396444896,139667311071766,158915951283562],
         [26212438434289,94902985796531,198145776057315],
         [266279278943923,58550737262493,74297973216378],
         [32373606512065,131854353044428,184642643042326],
         [34335377662439,85341895822066,273492717750246]],

    %% Verify that the keys still collide (this will fail if the
    %% internal hash function has been changed).
    erts_debug:set_internal_state(available_internal_state, true),
    try
        case erlang:system_info(wordsize) of
            8 ->
                verify_colliding_keys(L);
            4 ->
                %% Not guaranteed to collide on a 32-bit system.
                ok
        end
    after
        erts_debug:set_internal_state(available_internal_state, false)
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

%% Check that there is the same number of persistents terms before
%% and after each test case.

chk() ->
    {persistent_term:info(), persistent_term:get()}.

chk({Info, _Initial} = Chk) ->
    Info = persistent_term:info(),
    Key = {?MODULE,?FUNCTION_NAME},
    ok = persistent_term:put(Key, {term,Info}),
    Term = persistent_term:get(Key),
    true = persistent_term:erase(Key),
    chk_not_stuck(Term),
    [persistent_term:erase(K) || {K, _} <- pget(Chk)],
    ok.

chk_not_stuck(Term) ->
    %% Hash tables to be deleted are put onto a queue.
    %% Make sure that the queue isn't stuck by a table with
    %% a non-zero ref count.

    case erts_debug:size_shared(Term) of
        0 ->
            erlang:yield(),
            chk_not_stuck(Term);
        _ ->
            ok
    end.

pget({_, Initial}) ->
    persistent_term:get() -- Initial.


killed_while_trapping_put(_Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
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
    erts_debug:set_internal_state(available_internal_state, false).

killed_while_trapping_erase(_Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
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
    erts_debug:set_internal_state(available_internal_state, false).

put_erase_trapping(_Config) ->
    NrOfItems = 5000,
    erts_debug:set_internal_state(available_internal_state, true),
    do_puts(NrOfItems, first),
    do_puts(NrOfItems, second),
    do_erases(NrOfItems),
    erts_debug:set_internal_state(available_internal_state, false).

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
