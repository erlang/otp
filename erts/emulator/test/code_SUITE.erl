%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2012. All Rights Reserved.
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
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 versions/1,new_binary_types/1,
	 t_check_process_code/1,t_check_old_code/1,
	 t_check_process_code_ets/1,
	 external_fun/1,get_chunk/1,module_md5/1,make_stub/1,
	 make_stub_many_funs/1,constant_pools/1,constant_refc_binaries/1,
	 false_dependency/1,coverage/1,fun_confusion/1]).

-define(line_trace, 1).
-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [versions, new_binary_types, t_check_process_code,
     t_check_process_code_ets, t_check_old_code, external_fun, get_chunk,
     module_md5, make_stub, make_stub_many_funs,
     constant_pools, constant_refc_binaries, false_dependency,
     coverage, fun_confusion].

groups() -> 
    [].

init_per_suite(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    Config.

end_per_suite(_Config) ->
    catch erts_debug:set_internal_state(available_internal_state, false),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

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
    P1 ! P2 ! done,
    _ = monitor(process, P1),
    _ = monitor(process, P2),
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
    Data = ?config(data_dir, Config),
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
    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "my_code_test"),
    ?line {ok,my_code_test,Bin} = compile:file(File, [binary]),
    ?line {module,my_code_test} = erlang:load_module(my_code_test,
						     make_sub_binary(Bin)),
    ?line true = erlang:delete_module(my_code_test),
    ?line true = erlang:purge_module(my_code_test),

    ?line {module,my_code_test} = erlang:load_module(my_code_test,
						     make_unaligned_sub_binary(Bin)),
    ?line true = erlang:delete_module(my_code_test),
    ?line true = erlang:purge_module(my_code_test),

    %% Try heap binaries and bad binaries.
    ?line {error,badfile} = erlang:load_module(my_code_test, <<1,2>>),
    ?line {error,badfile} = erlang:load_module(my_code_test,
						make_sub_binary(<<1,2>>)),
    ?line {error,badfile} = erlang:load_module(my_code_test,
						make_unaligned_sub_binary(<<1,2>>)),
    ?line {'EXIT',{badarg,_}} = (catch erlang:load_module(my_code_test,
						      bit_sized_binary(Bin))),
    ok.

t_check_process_code(Config) when is_list(Config) ->
    ?line Priv = ?config(priv_dir, Config),
    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "my_code_test"),
    ?line Code = filename:join(Priv, "my_code_test"),

    ?line {ok,my_code_test} = c:c(File, [{outdir,Priv}]),

    ?line MyFun = fun(X, Y) -> X + Y end,	%Confuse things.
    ?line F = my_code_test:make_fun(42),
    ?line 2 = fun_refc(F),
    ?line MyFun2 = fun(X, Y) -> X * Y end,	%Confuse things.
    ?line 44 = F(2),

    %% Delete the module and call the fun again.
    ?line true = erlang:delete_module(my_code_test),
    ?line 2 = fun_refc(F),
    ?line 45 = F(3),
    ?line {'EXIT',{undef,_}} = (catch my_code_test:make_fun(33)),

    %% The fun should still be there, preventing purge.
    ?line true = erlang:check_process_code(self(), my_code_test),
    gc(),
    gc(),					%Place funs on the old heap.
    ?line true = erlang:check_process_code(self(), my_code_test),

    %% Using the funs here guarantees that they will not be prematurely garbed.
    ?line 48 = F(6),
    ?line 3 = MyFun(1, 2),
    ?line 12 = MyFun2(3, 4),

    %% Kill all funs.
    t_check_process_code1(Code, []).

%% The real fun was killed, but we have some fakes which look similar.

t_check_process_code1(Code, Fakes) ->
    ?line MyFun = fun(X, Y) -> X + Y + 1 end,	%Confuse things.
    ?line false = erlang:check_process_code(self(), my_code_test),
    ?line 4 = MyFun(1, 2),
    t_check_process_code2(Code, Fakes).

t_check_process_code2(Code, _) ->
    ?line false = erlang:check_process_code(self(), my_code_test),
    ?line true = erlang:purge_module(my_code_test),

    %% In the next test we will load the same module twice.
    ?line {module,my_code_test} = code:load_abs(Code),
    ?line F = my_code_test:make_fun(37),
    ?line 2 = fun_refc(F),
    ?line false = erlang:check_process_code(self(), my_code_test),
    ?line {module,my_code_test} = code:load_abs(Code),
    ?line 2 = fun_refc(F),

    %% Still false because the fun with the same identify is found
    %% in the current code.
    ?line false = erlang:check_process_code(self(), my_code_test),
    
    %% Some fake funs in the same module should not do any difference.
    ?line false = erlang:check_process_code(self(), my_code_test),

    38 = F(1),
    t_check_process_code3(Code, F, []).

t_check_process_code3(Code, F, Fakes) ->
    Pid = spawn_link(fun() -> body(F, Fakes) end),
    ?line true = erlang:purge_module(my_code_test),
    ?line false = erlang:check_process_code(self(), my_code_test),
    ?line false = erlang:check_process_code(Pid, my_code_test),

    ?line true = erlang:delete_module(my_code_test),
    ?line true = erlang:check_process_code(self(), my_code_test),
    ?line true = erlang:check_process_code(Pid, my_code_test),
    39 = F(2),
    t_check_process_code4(Code, Pid).

t_check_process_code4(_Code, Pid) ->
    Pid ! drop_funs,
    receive after 1 -> ok end,
    ?line false = erlang:check_process_code(Pid, my_code_test),
    ok.

body(F, Fakes) ->
    receive
	jog ->
	    40 = F(3),
	    erlang:garbage_collect(),
	    body(F, Fakes);
	drop_funs ->
	    dropped_body()
    end.

dropped_body() ->
    receive
	X -> exit(X)
    end.

gc() ->
    erlang:garbage_collect(),
    gc1().
gc1() -> ok.

t_check_process_code_ets(doc) ->
    "Test check_process_code/2 in combination with a fun obtained from an ets table.";
t_check_process_code_ets(Config) when is_list(Config) ->
    case test_server:is_native(?MODULE) of
	true ->
	    {skip,"Native code"};
	false ->
	    do_check_process_code_ets(Config)
    end.

do_check_process_code_ets(Config) ->
    ?line Priv = ?config(priv_dir, Config),
    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "my_code_test"),

    ?line erlang:purge_module(my_code_test),
    ?line erlang:delete_module(my_code_test),
    ?line {ok,my_code_test} = c:c(File, [{outdir,Priv}]),

    ?line T = ets:new(my_code_test, []),
    ?line ets:insert(T, {7,my_code_test:make_fun(107)}),
    ?line ets:insert(T, {8,my_code_test:make_fun(108)}),
    ?line erlang:delete_module(my_code_test),
    ?line false = erlang:check_process_code(self(), my_code_test),
    Body = fun() ->
		   [{7,F1}] = ets:lookup(T, 7),
		   [{8,F2}] = ets:lookup(T, 8),
		   IdleLoop = fun() -> receive _X -> ok end end,
		   RecLoop = fun(Again) ->
				     receive
					 call -> 110 = F1(3),
						 100 = F2(-8),
						 Again(Again);
					 {drop_funs,To} ->
					     To ! funs_dropped,
					     IdleLoop()
				     end
			     end,
		   true = erlang:check_process_code(self(), my_code_test),
		   RecLoop(RecLoop)
	   end,
    ?line Pid = spawn_link(Body),
    receive after 1 -> ok end,
    ?line true = erlang:check_process_code(Pid, my_code_test),
    Pid ! call,
    Pid ! {drop_funs,self()},

    receive
	funs_dropped -> ok;
	Other -> ?t:fail({unexpected,Other})
    after 10000 ->
	    ?line ?t:fail(no_funs_dropped_answer)
    end,

    ?line false = erlang:check_process_code(Pid, my_code_test),
    ok.

fun_refc(F) ->
    {refc,Count} = erlang:fun_info(F, refc),
    Count.


%% Test the erlang:check_old_code/1 BIF.
t_check_old_code(Config) when is_list(Config) ->
    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "my_code_test"),

    ?line erlang:purge_module(my_code_test),
    ?line erlang:delete_module(my_code_test),
    ?line catch erlang:purge_module(my_code_test),

    ?line false = erlang:check_old_code(my_code_test),

    ?line {ok,my_code_test,Code} = compile:file(File, [binary]),
    ?line {module,my_code_test} = code:load_binary(my_code_test, File, Code),
    
    ?line false = erlang:check_old_code(my_code_test),
    ?line {module,my_code_test} = code:load_binary(my_code_test, File, Code),
    ?line true = erlang:check_old_code(my_code_test),

    ?line true = erlang:purge_module(my_code_test),
    ?line true = erlang:delete_module(my_code_test),
    ?line true = erlang:purge_module(my_code_test),

    ?line {'EXIT',_} = (catch erlang:check_old_code([])),
    
    ok.

external_fun(Config) when is_list(Config) ->
    ?line false = erlang:function_exported(another_code_test, x, 1),
    AnotherCodeTest = id(another_code_test),
    ExtFun = fun AnotherCodeTest:x/1,
    ?line {'EXIT',{undef,_}} = (catch ExtFun(answer)),
    ?line false = erlang:function_exported(another_code_test, x, 1),
    ?line false = lists:member(another_code_test, erlang:loaded()),
    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "another_code_test"),
    ?line {ok,another_code_test,Code} = compile:file(File, [binary,report]),
    ?line {module,another_code_test} = erlang:load_module(another_code_test, Code),
    ?line 42 = ExtFun(answer),
    ok.

get_chunk(Config) when is_list(Config) ->
    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "my_code_test"),
    ?line {ok,my_code_test,Code} = compile:file(File, [binary]),

    %% Should work.
    ?line Chunk = get_chunk_ok("Atom", Code),
    ?line Chunk = get_chunk_ok("Atom", make_sub_binary(Code)),
    ?line Chunk = get_chunk_ok("Atom", make_unaligned_sub_binary(Code)),

    %% Should fail.
    ?line {'EXIT',{badarg,_}} = (catch code:get_chunk(bit_sized_binary(Code), "Atom")),
    ?line {'EXIT',{badarg,_}} = (catch code:get_chunk(Code, "bad chunk id")),

    %% Invalid beam code or missing chunk should return 'undefined'.
    ?line undefined = code:get_chunk(<<"not a beam module">>, "Atom"),
    ?line undefined = code:get_chunk(Code, "XXXX"),

    ok.

get_chunk_ok(Chunk, Code) ->
    case code:get_chunk(Code, Chunk) of
	Bin when is_binary(Bin) -> Bin
    end.

module_md5(Config) when is_list(Config) ->
    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "my_code_test"),
    ?line {ok,my_code_test,Code} = compile:file(File, [binary]),

    %% Should work.
    ?line Chunk = module_md5_ok(Code),
    ?line Chunk = module_md5_ok(make_sub_binary(Code)),
    ?line Chunk = module_md5_ok(make_unaligned_sub_binary(Code)),

    %% Should fail.
    ?line {'EXIT',{badarg,_}} = (catch code:module_md5(bit_sized_binary(Code))),

    %% Invalid beam code should return 'undefined'.
    ?line undefined = code:module_md5(<<"not a beam module">>),
    ok.
    
module_md5_ok(Code) ->
    case code:module_md5(Code) of
	Bin when is_binary(Bin), size(Bin) =:= 16 -> Bin
    end.


make_stub(Config) when is_list(Config) ->
    catch erlang:purge_module(my_code_test),
    MD5 = erlang:md5(<<>>),

    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "my_code_test"),
    ?line {ok,my_code_test,Code} = compile:file(File, [binary]),

    ?line my_code_test = code:make_stub_module(my_code_test, Code, {[],[],MD5}),
    ?line true = erlang:delete_module(my_code_test),
    ?line true = erlang:purge_module(my_code_test),

    ?line my_code_test = code:make_stub_module(my_code_test, 
 					       make_unaligned_sub_binary(Code),
 					       {[],[],MD5}),
    ?line true = erlang:delete_module(my_code_test),
    ?line true = erlang:purge_module(my_code_test),

    ?line my_code_test = code:make_stub_module(my_code_test, zlib:gzip(Code),
 					       {[],[],MD5}),
    ?line true = erlang:delete_module(my_code_test),
    ?line true = erlang:purge_module(my_code_test),

    %% Should fail.
    ?line {'EXIT',{badarg,_}} =
	(catch code:make_stub_module(my_code_test, <<"bad">>, {[],[],MD5})),
    ?line {'EXIT',{badarg,_}} =
	(catch code:make_stub_module(my_code_test,
				     bit_sized_binary(Code),
				     {[],[],MD5})),
    ?line {'EXIT',{badarg,_}} =
	(catch code:make_stub_module(my_code_test_with_wrong_name,
				     Code, {[],[],MD5})),
    ok.

make_stub_many_funs(Config) when is_list(Config) ->
    catch erlang:purge_module(many_funs),
    MD5 = erlang:md5(<<>>),

    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "many_funs"),
    ?line {ok,many_funs,Code} = compile:file(File, [binary]),

    ?line many_funs = code:make_stub_module(many_funs, Code, {[],[],MD5}),
    ?line true = erlang:delete_module(many_funs),
    ?line true = erlang:purge_module(many_funs),
    ?line many_funs = code:make_stub_module(many_funs, 
 					       make_unaligned_sub_binary(Code),
 					       {[],[],MD5}),
    ?line true = erlang:delete_module(many_funs),
    ?line true = erlang:purge_module(many_funs),

    %% Should fail.
    ?line {'EXIT',{badarg,_}} =
	(catch code:make_stub_module(many_funs, <<"bad">>, {[],[],MD5})),
    ?line {'EXIT',{badarg,_}} =
	(catch code:make_stub_module(many_funs,
				     bit_sized_binary(Code),
				     {[],[],MD5})),
    ok.

constant_pools(Config) when is_list(Config) ->
    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "literals"),
    ?line {ok,literals,Code} = compile:file(File, [report,binary]),
    ?line {module,literals} = erlang:load_module(literals,
						 make_sub_binary(Code)),

    %% Initialize.
    ?line A = literals:a(),
    ?line B = literals:b(),
    ?line C = literals:huge_bignum(),
    ?line process_flag(trap_exit, true),
    Self = self(),

    %% Have a process WITHOUT old heap that references the literals
    %% in the 'literals' module.
    ?line NoOldHeap = spawn_link(fun() -> no_old_heap(Self) end),
    receive go -> ok end,
    ?line true = erlang:delete_module(literals),
    ?line false = erlang:check_process_code(NoOldHeap, literals),
    ?line erlang:check_process_code(self(), literals),
    ?line true = erlang:purge_module(literals),
    ?line NoOldHeap ! done,
    ?line receive
	      {'EXIT',NoOldHeap,{A,B,C}} ->
		  ok;
	      Other ->
		  ?line ?t:fail({unexpected,Other})
	  end,
    ?line {module,literals} = erlang:load_module(literals, Code),

    %% Have a process WITH an old heap that references the literals
    %% in the 'literals' module.
    ?line OldHeap = spawn_link(fun() -> old_heap(Self) end),
    receive go -> ok end,
    ?line true = erlang:delete_module(literals),
    ?line false = erlang:check_process_code(OldHeap, literals),
    ?line erlang:check_process_code(self(), literals),
    ?line erlang:purge_module(literals),
    ?line OldHeap ! done,
    receive
	{'EXIT',OldHeap,{A,B,C,[1,2,3|_]=Seq}} when length(Seq) =:= 16 ->
	    ok
    end.

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
    Data = ?config(data_dir, Config),
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
	    ?t:fail(binary_leak);
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

%% OTP-7559: c_p->cp could contain garbage and create a false dependency
%% to a module in a process. (Thanks to Richard Carlsson.)
false_dependency(Config) when is_list(Config) ->
    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "cpbugx"),
    ?line {ok,cpbugx,Code} = compile:file(File, [binary,report]),

    do_false_dependency(fun cpbugx:before/0, Code),
    do_false_dependency(fun cpbugx:before2/0, Code),
    do_false_dependency(fun cpbugx:before3/0, Code),

%%     %% Spawn process. Make sure it has called cpbugx:before/0 and returned.
%%     Parent = self(),
%%     ?line Pid = spawn_link(fun() -> false_dependency_loop(Parent) end),
%%     ?line receive initialized -> ok end,

%%     %% Reload the module. Make sure the process is still alive.
%%     ?line {module,cpbugx} = erlang:load_module(cpbugx, Bin),
%%     ?line io:put_chars(binary_to_list(element(2, process_info(Pid, backtrace)))),
%%     ?line true = is_process_alive(Pid),

%%     %% There should not be any dependency to cpbugx.
%%     ?line false = erlang:check_process_code(Pid, cpbugx),
    



%%     %% Kill the process.
%%     ?line unlink(Pid), exit(Pid, kill),
    ok.

do_false_dependency(Init, Code) ->
    ?line {module,cpbugx} = erlang:load_module(cpbugx, Code),

    %% Spawn process. Make sure it has the appropriate init function
    %% and returned. CP should not contain garbage after the return.
    Parent = self(),
    ?line Pid = spawn_link(fun() -> false_dependency_loop(Parent, Init, true) end),
    ?line receive initialized -> ok end,

    %% Reload the module. Make sure the process is still alive.
    ?line {module,cpbugx} = erlang:load_module(cpbugx, Code),
    ?line io:put_chars(binary_to_list(element(2, process_info(Pid, backtrace)))),
    ?line true = is_process_alive(Pid),

    %% There should not be any dependency to cpbugx.
    ?line false = erlang:check_process_code(Pid, cpbugx),

    %% Kill the process and completely unload the code.
    ?line unlink(Pid), exit(Pid, kill),
    ?line true = erlang:purge_module(cpbugx),
    ?line true = erlang:delete_module(cpbugx),
    ?line code:is_module_native(cpbugx),  % test is_module_native on deleted code
    ?line true = erlang:purge_module(cpbugx),
    ?line code:is_module_native(cpbugx),  % test is_module_native on purged code
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
    ?line code:is_module_native(?MODULE),
    ?line {'EXIT',{badarg,_}} = (catch erlang:purge_module({a,b,c})),
    ?line {'EXIT',{badarg,_}} = (catch code:is_module_native({a,b,c})),
    ?line {'EXIT',{badarg,_}} = (catch erlang:check_process_code(not_a_pid, ?MODULE)),
    ?line {'EXIT',{badarg,_}} = (catch erlang:check_process_code(self(), [not_a_module])),
    ?line {'EXIT',{badarg,_}} = (catch erlang:delete_module([a,b,c])),
    ?line {'EXIT',{badarg,_}} = (catch erlang:module_loaded(42)),
    ok.

fun_confusion(Config) when is_list(Config) ->
    Data = ?config(data_dir, Config),
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

id(I) -> I.
