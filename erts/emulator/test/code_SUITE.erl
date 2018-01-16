%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2018. All Rights Reserved.
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
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1, 
         versions/1,new_binary_types/1, call_purged_fun_code_gone/1,
	 call_purged_fun_code_reload/1, call_purged_fun_code_there/1,
         multi_proc_purge/1, t_check_old_code/1,
         external_fun/1,get_chunk/1,module_md5/1,
         constant_pools/1,constant_refc_binaries/1,
         fake_literals/1,
         false_dependency/1,coverage/1,fun_confusion/1,
         t_copy_literals/1, t_copy_literals_frags/1,
         erl_544/1]).

-define(line_trace, 1).
-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [versions, new_binary_types, call_purged_fun_code_gone,
     call_purged_fun_code_reload, call_purged_fun_code_there,
     multi_proc_purge, t_check_old_code, external_fun, get_chunk,
     module_md5,
     constant_pools, constant_refc_binaries, fake_literals,
     false_dependency,
     coverage, fun_confusion, t_copy_literals, t_copy_literals_frags,
     erl_544].

init_per_suite(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    Config.

end_per_suite(_Config) ->
    catch erts_debug:set_internal_state(available_internal_state, false),
    ok.

%% Make sure that only two versions of a module can be loaded.
versions(Config) when is_list(Config) ->
    V1 = compile_version(1, Config),
    V2 = compile_version(2, Config),
    V3 = compile_version(3, Config),

    {ok,P1,1} = load_version(V1, 1),
    {ok,P2,2} = load_version(V2, 2),
    {error,not_purged} = load_version(V2, 2),
    {error,not_purged} = load_version(V3, 3),

    1 = check_version(P1),
    2 = check_version(P2),
    2 = versions:version(),

    %% Kill processes, unload code.
    _ = monitor(process, P1),
    _ = monitor(process, P2),
    P1 ! P2 ! done,
    receive
        {'DOWN',_,process,P1,normal} -> ok
    end,
    receive
        {'DOWN',_,process,P2,normal} -> ok
    end,
    true = erlang:purge_module(versions),
    true = erlang:delete_module(versions),
    true = erlang:purge_module(versions),
    ok.

compile_version(Version, Config) ->
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "versions"),
    {ok,versions,Bin} = compile:file(File, [{d,'VERSION',Version},
                                            binary,report]),
    Bin.

load_version(Code, Ver) ->
    case erlang:load_module(versions, Code) of
        {module,versions} ->
            Pid = spawn_link(versions, loop, []),
            Ver = versions:version(),
            Ver = check_version(Pid),
            {ok,Pid,Ver};
        Error ->
            Error
    end.

check_version(Pid) ->
    Pid ! {self(),version},
    receive
        {Pid,version,Version} ->
            Version
    end.

new_binary_types(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "my_code_test"),
    {ok,my_code_test,Bin} = compile:file(File, [binary]),
    {module,my_code_test} = erlang:load_module(my_code_test,
                                               make_sub_binary(Bin)),
    true = erlang:delete_module(my_code_test),
    true = erlang:purge_module(my_code_test),

    {module,my_code_test} = erlang:load_module(my_code_test,
                                               make_unaligned_sub_binary(Bin)),
    true = erlang:delete_module(my_code_test),
    true = erlang:purge_module(my_code_test),

    %% Try heap binaries and bad binaries.
    {error,badfile} = erlang:load_module(my_code_test, <<1,2>>),
    {error,badfile} = erlang:load_module(my_code_test,
                                         make_sub_binary(<<1,2>>)),
    {error,badfile} = erlang:load_module(my_code_test,
                                         make_unaligned_sub_binary(<<1,2>>)),
    {'EXIT',{badarg,_}} = (catch erlang:load_module(my_code_test,
                                                    bit_sized_binary(Bin))),
    ok.

call_purged_fun_code_gone(Config) when is_list(Config) ->
    Priv = proplists:get_value(priv_dir, Config),
    Data = proplists:get_value(data_dir, Config),
    call_purged_fun_test(Priv, Data, code_gone),
    ok.

call_purged_fun_code_reload(Config) when is_list(Config) ->
    Priv = proplists:get_value(priv_dir, Config),
    Data = proplists:get_value(data_dir, Config),
    Path = code:get_path(),
    true = code:add_path(Priv),
    try
	call_purged_fun_test(Priv, Data, code_reload)
    after
	code:set_path(Path)
    end,
    ok.

call_purged_fun_code_there(Config) when is_list(Config) ->
    Priv = proplists:get_value(priv_dir, Config),
    Data = proplists:get_value(data_dir, Config),
    call_purged_fun_test(Priv, Data, code_there),
    ok.

call_purged_fun_test(Priv, Data, Type) ->
    OptsList = case erlang:system_info(hipe_architecture) of
                   undefined -> [[]];
                   _ -> [[], [native,{d,hipe}]]
               end,
    [call_purged_fun_test_do(Priv, Data, Type, CO, FO)
     || CO <- OptsList, FO <- OptsList].


call_purged_fun_test_do(Priv, Data, Type, CallerOpts, FunOpts) ->
    io:format("Compile caller as ~p and funs as ~p\n", [CallerOpts, FunOpts]),
    SrcFile = filename:join(Data, "call_purged_fun_tester.erl"),
    ObjFile = filename:join(Priv, "call_purged_fun_tester.beam"),
    {ok,Mod,Code} = compile:file(SrcFile, [binary, report | CallerOpts]),
    {module,Mod} = code:load_binary(Mod, ObjFile, Code),

    call_purged_fun_tester:do(Priv, Data, Type, FunOpts).


multi_proc_purge(Config) when is_list(Config) ->
    %%
    %% Make sure purge requests aren't lost when
    %% purger process is working.
    %%
    Priv = proplists:get_value(priv_dir, Config),
    Data = proplists:get_value(data_dir, Config),
    File1 = filename:join(Data, "my_code_test"),
    File2 = filename:join(Data, "my_code_test2"),
    
    {ok,my_code_test} = c:c(File1, [{outdir,Priv}]),
    {ok,my_code_test2} = c:c(File2, [{outdir,Priv}]),
    erlang:delete_module(my_code_test),
    erlang:delete_module(my_code_test2),

    Self = self(),

    Fun1 = fun () ->
		   erts_code_purger:purge(my_code_test),
		   Self ! {self(), done}
	   end,
    Fun2 = fun () ->
		   erts_code_purger:soft_purge(my_code_test2),
		   Self ! {self(), done}
	   end,
    Fun3 = fun () ->
		   erts_code_purger:purge('__nonexisting_module__'),
		   Self ! {self(), done}
	   end,
    Fun4 = fun () ->
		   erts_code_purger:soft_purge('__another_nonexisting_module__'),
		   Self ! {self(), done}
	   end,

    Pid1 = spawn_link(Fun1),
    Pid2 = spawn_link(Fun2),
    Pid3 = spawn_link(Fun3),
    Pid4 = spawn_link(Fun4),
    Pid5 = spawn_link(Fun1),
    Pid6 = spawn_link(Fun2),
    Pid7 = spawn_link(Fun3),
    receive after 50 -> ok end,
    Pid8 = spawn_link(Fun4),
    Pid9 = spawn_link(Fun1),
    Pid10 = spawn_link(Fun2),
    Pid11 = spawn_link(Fun3),
    Pid12 = spawn_link(Fun4),
    Pid13 = spawn_link(Fun1),
    receive after 50 -> ok end,
    Pid14 = spawn_link(Fun2),
    Pid15 = spawn_link(Fun3),
    Pid16 = spawn_link(Fun4),

    lists:foreach(fun (P) -> receive {P, done} -> ok end end,
		  [Pid1, Pid2, Pid3, Pid4, Pid5, Pid6, Pid7, Pid8,
		   Pid9, Pid10, Pid11, Pid12, Pid13, Pid14, Pid15, Pid16]),
    ok.

%% Test the erlang:check_old_code/1 BIF.
t_check_old_code(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "my_code_test"),

    catch erlang:purge_module(my_code_test),
    catch erlang:delete_module(my_code_test),
    catch erlang:purge_module(my_code_test),

    false = erlang:check_old_code(my_code_test),

    {ok,my_code_test,Code} = compile:file(File, [binary]),
    {module,my_code_test} = code:load_binary(my_code_test, File, Code),

    false = erlang:check_old_code(my_code_test),
    {module,my_code_test} = code:load_binary(my_code_test, File, Code),
    true = erlang:check_old_code(my_code_test),

    true = erlang:purge_module(my_code_test),
    true = erlang:delete_module(my_code_test),
    true = erlang:purge_module(my_code_test),

    {'EXIT',_} = (catch erlang:check_old_code([])),

    ok.

external_fun(Config) when is_list(Config) ->
    false = erlang:function_exported(another_code_test, x, 1),
    AnotherCodeTest = id(another_code_test),
    ExtFun = fun AnotherCodeTest:x/1,
    {'EXIT',{undef,_}} = (catch ExtFun(answer)),
    false = erlang:function_exported(another_code_test, x, 1),
    false = lists:member(another_code_test, erlang:loaded()),
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "another_code_test"),
    {ok,another_code_test,Code} = compile:file(File, [binary,report]),
    {module,another_code_test} = erlang:load_module(another_code_test, Code),
    42 = ExtFun(answer),
    ok.

get_chunk(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "my_code_test"),
    {ok,my_code_test,Code} = compile:file(File, [binary]),

    %% Should work.
    Chunk = get_chunk_ok("AtU8", Code),
    Chunk = get_chunk_ok("AtU8", make_sub_binary(Code)),
    Chunk = get_chunk_ok("AtU8", make_unaligned_sub_binary(Code)),

    %% Should fail.
    {'EXIT',{badarg,_}} = (catch code:get_chunk(bit_sized_binary(Code), "AtU8")),
    {'EXIT',{badarg,_}} = (catch code:get_chunk(Code, "bad chunk id")),

    %% Invalid beam code or missing chunk should return 'undefined'.
    undefined = code:get_chunk(<<"not a beam module">>, "AtU8"),
    undefined = code:get_chunk(Code, "XXXX"),

    ok.

get_chunk_ok(Chunk, Code) ->
    case code:get_chunk(Code, Chunk) of
        Bin when is_binary(Bin) -> Bin
    end.

module_md5(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "my_code_test"),
    {ok,my_code_test,Code} = compile:file(File, [binary]),

    %% Should work.
    Chunk = module_md5_ok(Code),
    Chunk = module_md5_ok(make_sub_binary(Code)),
    Chunk = module_md5_ok(make_unaligned_sub_binary(Code)),

    %% Should fail.
    {'EXIT',{badarg,_}} = (catch code:module_md5(bit_sized_binary(Code))),

    %% Invalid beam code should return 'undefined'.
    undefined = code:module_md5(<<"not a beam module">>),
    ok.

module_md5_ok(Code) ->
    case code:module_md5(Code) of
        Bin when is_binary(Bin), size(Bin) =:= 16 -> Bin
    end.


constant_pools(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "literals"),
    {ok,literals,Code} = compile:file(File, [report,binary]),
    {module,literals} = erlang:load_module(literals,
                                           make_sub_binary(Code)),

    %% Initialize.
    A = literals:a(),
    B = literals:b(),
    C = literals:huge_bignum(),
    process_flag(trap_exit, true),
    Self = self(),

    %% Have a process WITHOUT old heap that references the literals
    %% in the 'literals' module.
    NoOldHeap = spawn_link(fun() -> no_old_heap(Self) end),
    receive go -> ok end,
    true = erlang:delete_module(literals),
    false = erlang:check_process_code(NoOldHeap, literals),
    erlang:check_process_code(self(), literals),
    true = erlang:purge_module(literals),
    NoOldHeap ! done,
    receive
        {'EXIT',NoOldHeap,{A,B,C}} ->
            ok;
        Other ->
            ct:fail({unexpected,Other})
    end,
    {module,literals} = erlang:load_module(literals, Code),

    %% Have a process WITH an old heap that references the literals
    %% in the 'literals' module.
    OldHeap = spawn_link(fun() -> old_heap(Self) end),
    receive go -> ok end,
    true = erlang:delete_module(literals),
    false = erlang:check_process_code(OldHeap, literals),
    erlang:check_process_code(self(), literals),
    erlang:purge_module(literals),
    OldHeap ! done,
    receive
	{'EXIT',OldHeap,{A,B,C,[1,2,3|_]=Seq}} when length(Seq) =:= 16 ->
	    ok
    end,

    {module,literals} = erlang:load_module(literals, Code),
    %% Have a hibernated process that references the literals
    %% in the 'literals' module.
    {Hib, Mon} = spawn_monitor(fun() -> hibernated(Self) end),
    receive go -> ok end,
    [{heap_size,OldHeapSz},
     {total_heap_size,OldTotHeapSz}] = process_info(Hib, [heap_size,
							  total_heap_size]),
    OldHeapSz = OldTotHeapSz,
    io:format("OldHeapSz=~p OldTotHeapSz=~p~n", [OldHeapSz, OldTotHeapSz]),
    true = erlang:delete_module(literals),
    false = erlang:check_process_code(Hib, literals),
    erlang:check_process_code(self(), literals),
    erlang:purge_module(literals),
    receive after 1000 -> ok end,
    [{heap_size,HeapSz},
     {total_heap_size,TotHeapSz}] = process_info(Hib, [heap_size,
						       total_heap_size]),
    io:format("HeapSz=~p TotHeapSz=~p~n", [HeapSz, TotHeapSz]),
    Hib ! hej,
    receive
	{'DOWN', Mon, process, Hib, Reason} ->
	    {undef, [{no_module,
		      no_function,
		      [{A,B,C,[1,2,3|_]=Seq}], _}]} = Reason,
	    16 = length(Seq)
    end,
    HeapSz = TotHeapSz, %% Ensure restored to hibernated state...
    true = HeapSz > OldHeapSz,
    ok.

no_old_heap(Parent) ->
    A = literals:a(),
    B = literals:b(),
    Res = {A,B,literals:huge_bignum()},
    Parent ! go,
    receive
        done ->
            exit(Res)
    end.

old_heap(Parent) ->
    A = literals:a(),
    B = literals:b(),
    Res = {A,B,literals:huge_bignum(),lists:seq(1, 16)},
    create_old_heap(),
    Parent ! go,
    receive
        done ->
            exit(Res)
    end.

hibernated(Parent) ->
    A = literals:a(),
    B = literals:b(),
    Res = {A,B,literals:huge_bignum(),lists:seq(1, 16)},
    Parent ! go,
    erlang:hibernate(no_module, no_function, [Res]).

create_old_heap() ->
    case process_info(self(), [heap_size,total_heap_size]) of
        [{heap_size,Sz},{total_heap_size,Total}] when Sz < Total ->
            ok;
        _ ->
            create_old_heap()
    end.

constant_refc_binaries(Config) when is_list(Config) ->
    wait_for_memory_deallocations(),
    Bef = memory_binary(),
    io:format("Binary data (bytes) before test: ~p\n", [Bef]),

    %% Compile the the literals module.
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "literals"),
    {ok,literals,Code} = compile:file(File, [report,binary]),

    %% Load the code and make sure that the binary is a refc binary.
    {module,literals} = erlang:load_module(literals, Code),
    Bin = literals:binary(),
    Sz = byte_size(Bin),
    Check = erlang:md5(Bin),
    io:format("Size of literal refc binary: ~p\n", [Sz]),
    {refc_binary,Sz,_,_} = erts_debug:get_internal_state({binary_info,Bin}),
    true = erlang:delete_module(literals),
    false = erlang:check_process_code(self(), literals),
    true = erlang:purge_module(literals),

    %% Now try to provoke a memory leak.
    provoke_mem_leak(10, Code, Check),

    %% Calculate the change in allocated binary data.
    erlang:garbage_collect(),
    wait_for_memory_deallocations(),
    Aft = memory_binary(),
    io:format("Binary data (bytes) after test: ~p", [Aft]),
    Diff = Aft - Bef,
    if
        Diff < 0 ->
            io:format("~p less bytes", [abs(Diff)]);
        Diff > 0 ->
            io:format("~p more bytes", [Diff]);
        true ->
            ok
    end,

    %% Test for leaks. We must accept some natural variations in
    %% the size of allocated binaries.
    if
        Diff > 64*1024 ->
            ct:fail(binary_leak);
        true ->
            ok
    end.

memory_binary() ->
    try
        erlang:memory(binary)
    catch
        error:notsup ->
            0
    end.

provoke_mem_leak(0, _, _) -> ok;
provoke_mem_leak(N, Code, Check) ->
    {module,literals} = erlang:load_module(literals, Code),

    %% Create several processes with references to the literal binary.
    Self = self(),
    Pids = [spawn_link(fun() ->
                               create_binaries(Self, NumRefs, Check)
                       end) || NumRefs <- lists:seq(1, 10)],
    [receive {started,Pid} -> ok end || Pid <- Pids],

    %% Make the code old and remove references to the constant pool
    %% in all processes.
    true = erlang:delete_module(literals),
    Ms = [spawn_monitor(fun() ->
                                false = erlang:check_process_code(Pid, literals)
                        end) || Pid <- Pids],
    [receive
         {'DOWN',R,process,P,normal} ->
             ok
     end || {P,R} <- Ms],

    %% Purge the code.
    true = erlang:purge_module(literals),

    %% Tell the processes that the code has been purged.
    [begin
         monitor(process, Pid),
         Pid ! purged
     end || Pid <- Pids],

    %% Wait for all processes to terminate.
    [receive
         {'DOWN',_,process,Pid,normal} ->
             ok
     end || Pid <- Pids],

    %% We now expect that the binary has been deallocated.
    provoke_mem_leak(N-1, Code, Check).

create_binaries(Parent, NumRefs, Check) ->
    Bin = literals:binary(),
    Bins = lists:duplicate(NumRefs, Bin),
    {bits,Bits} = literals:bits(),
    Parent ! {started,self()},
    receive
        purged ->
            %% The code has been purged. Now make sure that
            %% the binaries haven't been corrupted.
            Check = erlang:md5(Bin),
            [Bin = B || B <- Bins],
            <<42:13,Bin/binary>> = Bits,

            %% Remove all references to the binaries
            %% Doing it explicitly like this ensures that
            %% the binaries are gone when the parent process
            %% receives the 'DOWN' message.
            erlang:garbage_collect()
    end.

wait_for_memory_deallocations() ->
    try
        erts_debug:set_internal_state(wait, deallocations)
    catch
        error:undef ->
            erts_debug:set_internal_state(available_internal_state, true),
            wait_for_memory_deallocations()
    end.

fake_literals(_Config) ->
    Mod = fake__literals__module,
    try
        do_fake_literals(Mod)
    after
        _ = code:purge(Mod),
        _ = code:delete(Mod),
        _ = code:purge(Mod),
        _ = code:delete(Mod)
    end,
    ok.

do_fake_literals(Mod) ->
    Tid = ets:new(test, []),
    ExtTerms = get_external_terms(),
    Term0 = {self(),make_ref(),Tid,fun() -> ok end,ExtTerms},
    Terms = [begin
                 make_literal_module(Mod, Term0),
                 Mod:term()
             end || _ <- lists:seq(1, 10)],
    verify_lit_terms(Terms, Term0),
    true = ets:delete(Tid),
    ok.

make_literal_module(Mod, Term) ->
    Exp = [{term,0}],
    Attr = [],
    Fs = [{function,term,0,2,
           [{label,1},
            {line,[]},
            {func_info,{atom,Mod},{atom,term},0},
            {label,2},
            {move,{literal,Term},{x,0}},
            return]}],
    Asm = {Mod,Exp,Attr,Fs,2},
    {ok,Mod,Beam} = compile:forms(Asm, [from_asm,binary,report]),
    code:load_binary(Mod, atom_to_list(Mod), Beam).

verify_lit_terms([H|T], Term) ->
    case H =:= Term of
        true ->
            verify_lit_terms(T, Term);
        false ->
            error({bad_term,H})
    end;
verify_lit_terms([], _) ->
    ok.

get_external_terms() ->
    {ok,Node} =	test_server:start_node(?FUNCTION_NAME, slave, []),
    Ref = rpc:call(Node, erlang, make_ref, []),
    Ports = rpc:call(Node, erlang, ports, []),
    Pid = rpc:call(Node, erlang, self, []),
    _ = test_server:stop_node(Node),
    {Ref,hd(Ports),Pid}.

%% OTP-7559: c_p->cp could contain garbage and create a false dependency
%% to a module in a process. (Thanks to Richard Carlsson.)
false_dependency(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "cpbugx"),
    {ok,cpbugx,Code} = compile:file(File, [binary,report]),

    do_false_dependency(fun cpbugx:before/0, Code),
    do_false_dependency(fun cpbugx:before2/0, Code),
    do_false_dependency(fun cpbugx:before3/0, Code),

    %%     %% Spawn process. Make sure it has called cpbugx:before/0 and returned.
    %%     Parent = self(),
    %%     Pid = spawn_link(fun() -> false_dependency_loop(Parent) end),
    %%     receive initialized -> ok end,

    %%     %% Reload the module. Make sure the process is still alive.
    %%     {module,cpbugx} = erlang:load_module(cpbugx, Bin),
    %%     io:put_chars(binary_to_list(element(2, process_info(Pid, backtrace)))),
    %%     true = is_process_alive(Pid),

    %%     %% There should not be any dependency to cpbugx.
    %%     false = erlang:check_process_code(Pid, cpbugx),




    %%     %% Kill the process.
    %%     unlink(Pid), exit(Pid, kill),
    ok.

do_false_dependency(Init, Code) ->
    {module,cpbugx} = erlang:load_module(cpbugx, Code),

    %% Spawn process. Make sure it has the appropriate init function
    %% and returned. CP should not contain garbage after the return.
    Parent = self(),
    Pid = spawn_link(fun() -> false_dependency_loop(Parent, Init, true) end),
    receive initialized -> ok end,

    %% Reload the module. Make sure the process is still alive.
    {module,cpbugx} = erlang:load_module(cpbugx, Code),
    io:put_chars(binary_to_list(element(2, process_info(Pid, backtrace)))),
    true = is_process_alive(Pid),

    %% There should not be any dependency to cpbugx.
    false = erlang:check_process_code(Pid, cpbugx),

    %% Kill the process and completely unload the code.
    unlink(Pid), exit(Pid, kill),
    true = erlang:purge_module(cpbugx),
    true = erlang:delete_module(cpbugx),
    code:is_module_native(cpbugx),  % test is_module_native on deleted code
    true = erlang:purge_module(cpbugx),
    code:is_module_native(cpbugx),  % test is_module_native on purged code
    ok.

false_dependency_loop(Parent, Init, SendInitAck) ->
    Init(),
    case SendInitAck of
        true -> Parent ! initialized;
        false -> void
                 %% Just send one init-ack. I guess the point of this test
                 %% wasn't to fill parents msg-queue (?). Seen to cause
                 %% out-of-mem (on halfword-vm for some reason) by
                 %% 91 million msg in queue. /sverker
    end,
    receive
        _ -> false_dependency_loop(Parent, Init, false)
    end.

coverage(Config) when is_list(Config) ->
    code:is_module_native(?MODULE),
    {'EXIT',{badarg,_}} = (catch erlang:purge_module({a,b,c})),
    {'EXIT',{badarg,_}} = (catch code:is_module_native({a,b,c})),
    {'EXIT',{badarg,_}} = (catch erlang:check_process_code(not_a_pid, ?MODULE)),
    {'EXIT',{badarg,_}} = (catch erlang:check_process_code(self(), [not_a_module])),
    {'EXIT',{badarg,_}} = (catch erlang:delete_module([a,b,c])),
    {'EXIT',{badarg,_}} = (catch erlang:module_loaded(42)),
    ok.

fun_confusion(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Src = filename:join(Data, "fun_confusion"),
    Mod = fun_confusion,

    %% Load first version of module.
    compile_load(Mod, Src, 1),
    F1 = Mod:f(),
    1 = F1(),

    %% Load second version of module.
    compile_load(Mod, Src, 2),
    F2 = Mod:f(),

    %% F1 should refer to the old code, not the newly loaded code.
    1 = F1(),
    2 = F2(),
    ok.

compile_load(Mod, Src, Ver) ->
    {ok,Mod,Code1} = compile:file(Src, [binary,{d,version,Ver}]),
    {module,Mod} = code:load_binary(Mod, "fun_confusion.beam", Code1),
    ok.


t_copy_literals(Config) when is_list(Config) ->
    %% Compile the the literals module.
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "literals"),
    {ok,literals,Code} = compile:file(File, [report,binary]),
    {module,literals} = erlang:load_module(literals, Code),

    N   = 30,
    Me  = self(),
    %% reload literals code every 567 ms
    Rel = spawn_link(fun() -> reloader(literals,Code,567) end),
    %% add new literal msgs to the loop every 789 ms
    Sat = spawn_link(fun() -> saturate(Me,789) end),
    %% run for 10s
    _   = spawn_link(fun() -> receive after 10000 -> Me ! done end end),
    ok  = chase_msg(N, Me),
    %% cleanup
    Rel ! done,
    Sat ! done,
    ok  = flush(),
    ok.

-define(mod, t_copy_literals_frags).
t_copy_literals_frags(Config) when is_list(Config) ->
    Bin = gen_lit(?mod,[{a,{1,2,3,4,5,6,7}},
                        {b,"hello world"},
                        {c, <<"hello world">>},
                        {d, {"hello world", {1.0, 2.0, <<"some">>, "string"}}},
                        {e, <<"off heap", 0, 1, 2, 3, 4, 5, 6, 7,
                                          8, 9,10,11,12,13,14,15,
                                          0, 1, 2, 3, 4, 5, 6, 7,
                                          8, 9,10,11,12,13,14,15,
                                          0, 1, 2, 3, 4, 5, 6, 7,
                                          8, 9,10,11,12,13,14,15,
                                          0, 1, 2, 3, 4, 5, 6, 7,
                                          8, 9,10,11,12,13,14,15>>}]),

    {module, ?mod} = erlang:load_module(?mod, Bin),
    N = 6000,
    Recv = spawn_opt(fun() -> receive
                                  read ->
                                      io:format("reading"),
                                      literal_receiver()
                              end
                     end, [link,{min_heap_size, 10000}]),
    Switcher = spawn_link(fun() -> literal_switcher() end),
    Pids = [spawn_opt(fun() -> receive
                                   {Pid, go, Recv, N} ->
                                       io:format("sender batch (~w) start ~w~n",[N,self()]),
                                       literal_sender(N,Recv),
                                       Pid ! {self(), ok}
                               end
                      end, [link,{min_heap_size,800}]) || _ <- lists:seq(1,100)],
    _ = [Pid ! {self(), go, Recv, N} || Pid <- Pids],
    %% don't read immediately
    timer:sleep(5),
    Recv ! read,
    Switcher ! {switch,?mod,Bin,[Recv|Pids],200},
    _ = [receive {Pid, ok} -> ok end || Pid <- Pids],
    Switcher ! {self(), done},
    receive {Switcher, ok} -> ok end,
    Recv ! {self(), done},
    receive {Recv, ok} -> ok end,
    ok.

literal_receiver() ->
    receive
        {Pid, done} ->
            io:format("reader_done~n"),
            Pid ! {self(), ok};
        {_Pid, msg, [A,B,C,D,E]} ->
            A = ?mod:a(),
            B = ?mod:b(),
            C = ?mod:c(),
            D = ?mod:d(),
            E = ?mod:e(),
            literal_receiver();
        {Pid, sender_confirm} ->
            io:format("sender confirm ~w~n", [Pid]),
            Pid ! {self(), ok},
            literal_receiver()
    end.

literal_sender(0, Recv) ->
    Recv ! {self(), sender_confirm},
    receive {Recv, ok} -> ok end;
literal_sender(N, Recv) ->
    Recv ! {self(), msg, [?mod:a(),
                          ?mod:b(),
                          ?mod:c(),
                          ?mod:d(),
                          ?mod:e()]},
    literal_sender(N - 1, Recv).

literal_switcher() ->
    receive
        {switch,Mod,Bin,Pids,Tmo} ->
            literal_switcher(Mod,Bin,Pids,Tmo)
    end.
literal_switcher(Mod,Bin,Pids,Tmo) ->
    receive
        {Pid,done} ->
            Pid ! {self(),ok}
    after Tmo ->
              io:format("load module ~w~n", [Mod]),
              {module, Mod} = erlang:load_module(Mod,Bin),
              ok = check_and_purge(Pids,Mod),
              io:format("purge complete ~w~n", [Mod]),
              literal_switcher(Mod,Bin,Pids,Tmo+Tmo)
    end.

check_and_purge([],Mod) ->
    erlang:purge_module(Mod),
    ok;
check_and_purge(Pids,Mod) ->
    io:format("purge ~w~n", [Mod]),
    Tag = make_ref(),
    _ = [begin
             erlang:check_process_code(Pid,Mod,[{async,{Tag,Pid}}])
         end || Pid <- Pids],
    Retry = check_and_purge_receive(Pids,Tag,[]),
    check_and_purge(Retry,Mod).

check_and_purge_receive([Pid|Pids],Tag,Retry) ->
    receive
        {check_process_code, {Tag, Pid}, false} ->
            check_and_purge_receive(Pids,Tag,Retry);
        {check_process_code, {Tag, Pid}, true} ->
            check_and_purge_receive(Pids,Tag,[Pid|Retry])
    end;
check_and_purge_receive([],_,Retry) ->
    Retry.


gen_lit(Module,Terms) ->
    FunStrings = [lists:flatten(io_lib:format("~w() -> ~w.~n", [F,Term]))||{F,Term}<-Terms],
    FunForms = function_forms(FunStrings),
    Forms    = [{attribute,erl_anno:new(1),module,Module},
                {attribute,erl_anno:new(2),export,[FA || {FA,_} <- FunForms]}] ++
               [Function || {_, Function} <- FunForms],
    {ok, Module, Bin} = compile:forms(Forms),
    Bin.

function_forms([]) -> [];
function_forms([S|Ss]) ->
    {ok, Ts,_} = erl_scan:string(S),
    {ok, Form} = erl_parse:parse_form(Ts),
    Fun   = element(3, Form),
    Arity = element(4, Form),
    [{{Fun,Arity}, Form}|function_forms(Ss)].

chase_msg(0, Pid) ->
    chase_loop(Pid);
chase_msg(N, Master) ->
    Pid = spawn_link(fun() -> chase_msg(N - 1,Master) end),
    chase_loop(Pid).

chase_loop(Pid) ->
    receive
        done ->
            Pid ! done,
            ok;
        {_From,Msg} ->
            Pid ! {self(), Msg},
            ok = traverse(Msg),
            chase_loop(Pid)
    end.

saturate(Pid,Time) ->
    Es = [msg1,msg2,msg3,msg4,msg5],
    Msg = [literals:E()||E <- Es],
    Pid ! {self(), Msg},
    receive
        done -> ok
    after Time ->
              saturate(Pid,Time)
    end.

traverse([]) -> ok;
traverse([H|T]) ->
    ok = traverse(H),
    traverse(T);
traverse(T) when is_tuple(T) -> ok;
traverse(B) when is_binary(B) -> ok;
traverse(I) when is_integer(I) -> ok;
traverse(#{ 1 := V1, b := V2 }) ->
    ok = traverse(V1),
    ok = traverse(V2),
    ok.


reloader(Mod,Code,Time) ->
    receive
        done -> ok
    after Time ->
              code:purge(Mod),
              {module,Mod} = erlang:load_module(Mod, Code),
              reloader(Mod,Code,Time)
    end.

erl_544(Config) when is_list(Config) ->
    case file:native_name_encoding() of
        utf8 ->
            {ok, CWD} = file:get_cwd(),
            try
                Mod = erl_544,
                FileName = atom_to_list(Mod) ++ ".erl",
                Priv = proplists:get_value(priv_dir, Config),
                Data = proplists:get_value(data_dir, Config),
                {ok, FileContent} = file:read_file(filename:join(Data,
                                                                 FileName)),
                Dir = filename:join(Priv, [16#2620,16#2620,16#2620]),
                File = filename:join(Dir, FileName),
                io:format("~ts~n", [File]),
                ok = file:make_dir(Dir),
                ok = file:set_cwd(Dir),
                ok = file:write_file(File, [FileContent]),
                {ok, Mod} = compile:file(File),
                Res1 = (catch Mod:err()),
                io:format("~p~n", [Res1]),
                {'EXIT', {err, [{Mod, err, 0, Info1}|_]}} = Res1,
                File = proplists:get_value(file, Info1),
                Me = self(),
                Go = make_ref(),
                Tester = spawn_link(fun () ->
                                            Mod:wait(Me, Go),
                                            Mod:err()
                                    end),
                receive Go -> ok end,
                Res2 = process_info(Tester, current_stacktrace),
                io:format("~p~n", [Res2]),
                {current_stacktrace, Stack} = Res2,
                [{Mod, wait, 2, Info2}|_] = Stack,
                File = proplists:get_value(file, Info2),
                StackFun = fun(_, _, _) -> false end,
                FormatFun = fun (Term, _) -> io_lib:format("~tp", [Term]) end,
                Formated =
                    lib:format_stacktrace(1, Stack, StackFun, FormatFun),
                true = is_list(Formated),
                ok
            after
                ok = file:set_cwd(CWD)
            end,
            ok;
        _Enc ->
            {skipped, "Only run when native file name encoding is utf8"}
    end.

%% Utilities.

make_sub_binary(Bin) when is_binary(Bin) ->
    {_,B1} = split_binary(list_to_binary([0,1,3,Bin,4,5,6,7]), 3),
    {B,_} = split_binary(B1, size(Bin)),
    B;
make_sub_binary(List) ->
    make_sub_binary(list_to_binary(List)).

make_unaligned_sub_binary(Bin0) ->
    Bin1 = <<0:3,Bin0/binary,31:5>>,
    Sz = size(Bin0),
    <<0:3,Bin:Sz/binary,31:5>> = id(Bin1),
    Bin.

%% Add 1 bit to the size of the binary.
bit_sized_binary(Bin0) ->
    Bin = <<Bin0/binary,1:1>>,
    BitSize = bit_size(Bin),
    BitSize = 8*size(Bin) + 1,
    Bin.

flush() ->
    receive _ -> flush() after 0 -> ok end.

id(I) -> I.

