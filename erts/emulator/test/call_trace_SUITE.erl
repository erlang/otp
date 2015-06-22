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

%%% Purpose : Tests the new call_trace BIF.

-module(call_trace_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 hipe/1,process_specs/1,basic/1,flags/1,errors/1,pam/1,change_pam/1,
	 return_trace/1,exception_trace/1,on_load/1,deep_exception/1,
	 upgrade/1,
	 exception_nocatch/1,bit_syntax/1]).

%% Helper functions.

-export([bar/0,foo/0,foo/1,foo/2,expect/1,worker_foo/1,pam_foo/2,nasty/0,
	 id/1,deep/3,deep_1/3,deep_2/2,deep_3/2,deep_4/1,deep_5/1,
	 bs_sum_a/2,bs_sum_b/2]).

%% Debug
-export([abbr/1,abbr/2]).


-include_lib("test_server/include/test_server.hrl").

-define(P, 20).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    Common = [errors, on_load],
    NotHipe = [process_specs, basic, flags, pam, change_pam,
	       upgrade,
	       return_trace, exception_trace, deep_exception,
	       exception_nocatch, bit_syntax],
    Hipe = [hipe],
    case test_server:is_native(call_trace_SUITE) of
	true -> Hipe ++ Common;
	false -> NotHipe ++ Common
    end.

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


init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog = ?t:timetrap(?t:seconds(30)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),

    %% Reloading the module will clear all trace patterns, and
    %% in a debug-compiled emulator run assertions of the counters
    %% for the number of traced exported functions in this module.

    c:l(?MODULE).

hipe(Config) when is_list(Config) ->
    ?line 0 = erlang:trace_pattern({?MODULE,worker_foo,1}, true),
    ?line 0 = erlang:trace_pattern({?MODULE,worker_foo,1}, true, [local]),
    ?line AllFuncs = erlang:trace_pattern({'_','_','_'}, true),

    %% Make sure that a traced, exported function can still be found.
    ?line true = erlang:function_exported(error_handler, undefined_function, 3),
    ?line AllFuncs = erlang:trace_pattern({'_','_','_'}, false),
    ok.

process_specs(doc) ->
    "Tests 'all', 'new', and 'existing' for specifying processes.";
process_specs(suite) -> [];
process_specs(Config) when is_list(Config) ->
    ?line Tracer = start_tracer(),
    ?line {flags,[call]} = trace_info(self(), flags),
    ?line {tracer,Tracer} = trace_info(self(), tracer),
    ?line trace_func({?MODULE,worker_foo,1}, []),

    %% Test the 'new' flag.

    ?line {Work1A,Work1B} = start_and_trace(new, [1,2,3], A1B={3,2,1}),
    {flags,[]} = trace_info(Work1A, flags),
    {tracer,[]} = trace_info(Work1A, tracer),
    {tracer,Tracer} = trace_info(Work1B, tracer),
    {flags,[call]} = trace_info(Work1B, flags),
    ?line expect({trace,Work1B,call,{?MODULE,worker_foo,[A1B]}}),
    ?line unlink(Work1B),
    ?line Mref = erlang:monitor(process, Work1B),
    ?line exit(Work1B, kill),
    receive
	{'DOWN',Mref,_,_,_} -> ok
    end,
    ?line undefined = trace_info(Work1B, flags),
    ?line {flags,[]} = trace_info(new, flags),
    ?line {tracer,[]} = trace_info(new, tracer),

    %% Test the 'existing' flag.
    ?line {Work2A,_Work2B} = start_and_trace(existing, A2A=[5,6,7], [7,6,5]),
    ?line expect({trace,Work2A,call,{?MODULE,worker_foo,[A2A]}}),

    %% Test the 'all' flag.
    ?line {Work3A,Work3B} = start_and_trace(all, A3A=[12,13], A3B=[13,12]),
    ?line expect({trace,Work3A,call,{?MODULE,worker_foo,[A3A]}}),
    ?line expect({trace,Work3B,call,{?MODULE,worker_foo,[A3B]}}),
    
    ok.

start_and_trace(Flag, A1, A2) ->
    W1 = start_worker(),
    trace_pid(Flag, true, [call]),
    W2 = start_worker(),
    call_worker(W1, A1),
    call_worker(W2, A2),
    case Flag of
	new ->
	    {flags,[call]} = trace_info(new, flags),
	    {tracer,_} = trace_info(new, tracer);
	_Other ->
	    ok
    end,
    trace_pid(Flag, false, [call]),
    {W1,W2}.

start_worker() ->
    ?line spawn(fun worker_loop/0).

call_worker(Pid, Arg) ->
    Pid ! {self(),{call,Arg}},
    receive
	{result,Res} -> Res
    after 5000 ->
	    ?line ?t:fail(no_answer_from_worker)
    end.

worker_loop() ->
    receive
	{From,{call,Arg}} ->
	    From ! {result,?MODULE:worker_foo(Arg)},
	    worker_loop();
	Other ->
	    exit({unexpected_message,Other})
    end.

worker_foo(_Arg) ->
    ok.

%% Basic test of the call tracing (we trace one process).
basic(_Config) ->
    case test_server:is_native(lists) of
	true -> {skip,"lists is native"};
	false -> basic()
    end.

basic() ->
    ?line start_tracer(),
    ?line trace_info(self(), flags),
    ?line trace_info(self(), tracer),
    ?line 0 = trace_func({?MODULE,no_such_function,0}, []),
    ?line {traced,undefined} = 
	trace_info({?MODULE,no_such_function,0}, traced),
    ?line {match_spec, undefined} = 
	trace_info({?MODULE,no_such_function,0}, match_spec),

    %% Trace some functions...

    ?line trace_func({lists,'_','_'}, []),

    %% Make sure that tracing the same functions more than once
    %% does not cause any problems.
    ?line 3 = trace_func({?MODULE,foo,'_'}, true),
    ?line 3 = trace_func({?MODULE,foo,'_'}, true),
    ?line 1 = trace_func({?MODULE,bar,0}, true),
    ?line 1 = trace_func({?MODULE,bar,0}, true),
    ?line {traced,global} = trace_info({?MODULE,bar,0}, traced),
    ?line 1 = trace_func({erlang,list_to_integer,1}, true),
    ?line {traced,global} = trace_info({erlang,list_to_integer,1}, traced),

    %% ... and call them...

    ?line AList = [x,y,z],
    ?line true = lists:member(y, AList),
    ?line foo0 = ?MODULE:foo(),
    ?line 4 = ?MODULE:foo(3),
    ?line 11 = ?MODULE:foo(7, 4),
    ?line ok = ?MODULE:bar(),
    ?line 42 = list_to_integer(non_literal("42")),

    %% ... make sure the we got trace messages (but not for ?MODULE:expect/1).

    ?line Self = self(),
    ?line ?MODULE:expect({trace,Self,call,{lists,member,[y,AList]}}),
    ?line ?MODULE:expect({trace,Self,call,{?MODULE,foo,[]}}),
    ?line ?MODULE:expect({trace,Self,call,{?MODULE,foo,[3]}}),
    ?line ?MODULE:expect({trace,Self,call,{?MODULE,foo,[7,4]}}),
    ?line ?MODULE:expect({trace,Self,call,{?MODULE,bar,[]}}),
    ?line ?MODULE:expect({trace,Self,call,{erlang,list_to_integer,["42"]}}),

    %% Turn off trace for this module and call functions...

    ?line trace_func({?MODULE,'_','_'}, false),
    ?line {traced,false} = trace_info({?MODULE,bar,0}, traced),
    ?line foo0 = ?MODULE:foo(),
    ?line 4 = ?MODULE:foo(3),
    ?line 11 = ?MODULE:foo(7, 4),
    ?line ok = ?MODULE:bar(),
    ?line [1,2,3,4,5,6,7,8,9,10] = lists:seq(1, 10),
    ?line 777 = list_to_integer(non_literal("777")),

    %% ... turn on all trace messages...

    ?line trace_func({'_','_','_'}, false),
    ?line [b,a] = lists:reverse([a,b]),

    %% Read out the remaing trace messages.

    ?line ?MODULE:expect({trace,Self,call,{lists,seq,[1,10]}}),
    ?line ?MODULE:expect({trace,Self,call,{erlang,list_to_integer,["777"]}}),
    receive
	Any ->
	    ?line ?t:fail({unexpected_message,Any})
    after 1 ->
	    ok
    end,

    %% Turn on and then off tracing on all external functions.
    %% This might cause the emulator to crasch later if it doesn't
    %% restore all export entries properly.

    ?line AllFuncs = trace_func({'_','_','_'}, true),
    io:format("AllFuncs = ~p", [AllFuncs]),
    %% Make sure that a traced, exported function can still be found.
    ?line true = erlang:function_exported(error_handler, undefined_function, 3),
    ?line AllFuncs = trace_func({'_','_','_'}, false),
    ?line erlang:trace_delivered(all),
    receive
	{trace_delivered,_,_} -> ok
    end,
    c:flush(),					% Print the traces messages.
    c:flush(),					% Print the traces messages.

    ?line {traced,false} = trace_info({erlang,list_to_integer,1}, traced),

    ok.

non_literal(X) -> X.

bar() ->
    ok.

foo() -> foo0.
foo(X) -> X+1.
foo(X, Y) -> X+Y.


%% Note that the semantics that this test case verifies
%% are not explicitly specified in the docs (what I could find in R15B).
%% This test case was written to verify that we do not change
%% any behaviour with the introduction of "block-free" upgrade in R16.
%% In short: Do not refer to this test case as an authority of how it must work.
upgrade(doc) ->
    "Test tracing on module being upgraded";
upgrade(Config) when is_list(Config) ->
    V1 = compile_version(my_upgrade_test, 1, Config),
    V2 = compile_version(my_upgrade_test, 2, Config),
    start_tracer(),
    upgrade_do(V1, V2, false),
    upgrade_do(V1, V2, true).

upgrade_do(V1, V2, TraceLocalVersion) ->
    {module,my_upgrade_test} = erlang:load_module(my_upgrade_test, V1),


    %% Test that trace is cleared after load_module

    trace_func({my_upgrade_test,'_','_'}, [], [global]),
    case TraceLocalVersion of
	true -> trace_func({my_upgrade_test,local_version,0}, [], [local]);
	_ -> ok
    end,
    1 = my_upgrade_test:version(),
    1 = my_upgrade_test:do_local(),
    1 = my_upgrade_test:do_real_local(),
    put('F1_exp', my_upgrade_test:make_fun_exp()),
    put('F1_loc', my_upgrade_test:make_fun_local()),
    1 = (get('F1_exp'))(),
    1 = (get('F1_loc'))(),

    Self = self(),
    expect({trace,Self,call,{my_upgrade_test,version,[]}}),
    expect({trace,Self,call,{my_upgrade_test,do_local,[]}}),
    expect({trace,Self,call,{my_upgrade_test,do_real_local,[]}}),
    case TraceLocalVersion of
	true -> expect({trace,Self,call,{my_upgrade_test,local_version,[]}});
	_ -> ok
    end,
    expect({trace,Self,call,{my_upgrade_test,make_fun_exp,[]}}),
    expect({trace,Self,call,{my_upgrade_test,make_fun_local,[]}}),
    expect({trace,Self,call,{my_upgrade_test,version,[]}}), % F1_exp
    case TraceLocalVersion of
	true -> expect({trace,Self,call,{my_upgrade_test,local_version,[]}}); % F1_loc
	_ -> ok
    end,

    {module,my_upgrade_test} = erlang:load_module(my_upgrade_test, V2),
    2 = my_upgrade_test:version(),
    put('F2_exp', my_upgrade_test:make_fun_exp()),
    put('F2_loc', my_upgrade_test:make_fun_local()),
    2 = (get('F1_exp'))(),
    1 = (get('F1_loc'))(),
    2 = (get('F2_exp'))(),
    2 = (get('F2_loc'))(),
    expect(),

    put('F1_exp', undefined),
    put('F1_loc', undefined),
    erlang:garbage_collect(),
    erlang:purge_module(my_upgrade_test),

    % Test that trace is cleared after delete_module

    trace_func({my_upgrade_test,'_','_'}, [], [global]),
    case TraceLocalVersion of
	true -> trace_func({my_upgrade_test,local_version,0}, [], [local]);
	_ -> ok
    end,
    2 = my_upgrade_test:version(),
    2 = my_upgrade_test:do_local(),
    2 = my_upgrade_test:do_real_local(),
    2 = (get('F2_exp'))(),
    2 = (get('F2_loc'))(),

    expect({trace,Self,call,{my_upgrade_test,version,[]}}),
    expect({trace,Self,call,{my_upgrade_test,do_local,[]}}),
    expect({trace,Self,call,{my_upgrade_test,do_real_local,[]}}),
    case TraceLocalVersion of
	true -> expect({trace,Self,call,{my_upgrade_test,local_version,[]}});
	_ -> ok
    end,
    expect({trace,Self,call,{my_upgrade_test,version,[]}}), % F2_exp
    case TraceLocalVersion of
	true -> expect({trace,Self,call,{my_upgrade_test,local_version,[]}}); % F2_loc
	_ -> ok
    end,

    true = erlang:delete_module(my_upgrade_test),
    {'EXIT',{undef,_}} = (catch my_upgrade_test:version()),
    {'EXIT',{undef,_}} = (catch ((get('F2_exp'))())),
    2 = (get('F2_loc'))(),
    expect(),

    put('F2_exp', undefined),
    put('F2_loc', undefined),
    erlang:garbage_collect(),
    erlang:purge_module(my_upgrade_test),
    ok.

compile_version(Module, Version, Config) ->
    Data = ?config(data_dir, Config),
    File = filename:join(Data, atom_to_list(Module)),
    {ok,Module,Bin} = compile:file(File, [{d,'VERSION',Version},
					    binary,report]),
    Bin.



%% Test flags (arity, timestamp) for call_trace/3.
%% Also, test the '{tracer,Pid}' option.
flags(_Config) ->
    case test_server:is_native(filename) of
	true -> {skip,"filename is native"};
	false -> flags()
    end.

flags() ->
    ?line Tracer = start_tracer_loop(),
    ?line trace_pid(self(), true, [call,{tracer,Tracer}]),

    %% Trace some functions...

    ?line trace_func({filename,'_','_'}, true),

    %% ... and call them...

    ?line Self = self(),
    ?line filename:absname("nisse"),
    ?line ?MODULE:expect({trace,Self,call,{filename,absname,["nisse"]}}),
    ?line trace_pid(Self, true, [call,arity]),
    ?line filename:absname("kalle"),
    ?line filename:absname("kalle", "/root"),
    ?line ?MODULE:expect({trace,Self,call,{filename,absname,1}}),
    ?line ?MODULE:expect({trace,Self,call,{filename,absname,2}}),
    ?line trace_info(Self, flags),

    %% Timestamp + arity.

    flag_test(fun() ->
		      ?line trace_pid(Self, true, [timestamp]),
		      ?line "dum" = filename:basename("/abcd/dum"),
		      ?line Ts = expect({trace_ts,Self,call,{filename,basename,1},ts}),
		      ?line trace_info(Self, flags),
		      Ts
	      end),

    %% Timestamp.

    ?line AnArg = "/abcd/hejsan",
    flag_test(fun() ->
		      ?line trace_pid(Self, false, [arity]),
		      ?line "hejsan" = filename:basename(AnArg),
		      ?line Ts = expect({trace_ts,Self,call,
					 {filename,basename,[AnArg]},ts}),
		      ?line trace_info(Self, flags),
		      Ts
	      end),

    %% All flags turned off.

    ?line trace_pid(Self, false, [timestamp]),
    ?line AnotherArg = filename:join(AnArg, "hoppsan"),
    ?line "hoppsan" = filename:basename(AnotherArg),
    ?line expect({trace,Self,call,{filename,join,[AnArg,"hoppsan"]}}),
    ?line expect({trace,Self,call,{filename,basename,[AnotherArg]}}),
    ?line trace_info(Self, flags),
    
    ok.

flag_test(Test) ->
    Now = now(),
    Ts = Test(),
    case timer:now_diff(Ts, Now) of
	Time when Time < 5*1000000 ->
	    %% Reasonable short time.
	    ok;
	_Diff ->
	    %% Too large difference.
	    io:format("Now = ~p\n", [Now]),
	    io:format("Ts = ~p\n", [Ts]),
	    ?line ?t:fail()
    end,
    flag_test_cpu_timestamp(Test).

flag_test_cpu_timestamp(Test) ->
    try erlang:trace(all, true, [cpu_timestamp]) of
	_ ->
	    io:format("CPU timestamps"),
	    Ts = Test(),
	    erlang:trace(all, false, [cpu_timestamp]),
	    Origin = {0,0,0},
	    Hour = 3600*1000000,
	    case timer:now_diff(Ts, Origin) of
		Diff when Diff < 4*Hour ->
		    %% In the worst case, CPU timestamps count from when this
		    %% Erlang emulator was started. The above test is a conservative
		    %% test that all CPU timestamps should pass.
		    ok;
		_Time ->
		    io:format("Strange CPU timestamp: ~p", [Ts]),
		    ?line ?t:fail()
	    end,
	    io:format("Turned off CPU timestamps")
    catch
	error:badarg -> ok
    end.

errors(doc) -> "Test bad arguments for trace/3 and trace_pattern/3.";
errors(suite) -> [];
errors(Config) when is_list(Config) ->
    ?line expect_badarg_pid(aaa, true, []),
    ?line expect_badarg_pid({pid,dum}, false, []),
    ?line expect_badarg_func({'_','_',1}, []),
    ?line expect_badarg_func({'_',gosh,1}, []),
    ?line expect_badarg_func({xxx,'_',2}, []),
    ?line expect_badarg_func({xxx,yyy,b}, glurp),
    ok.

expect_badarg_pid(What, How, Flags) ->
    case catch erlang:trace(What, How, Flags) of
	{'EXIT',{badarg,Where}} ->
	    io:format("trace(~p, ~p, ~p) ->\n  {'EXIT',{badarg,~p}}",
		      [What,How,Flags,Where]),
	    ok;
	Other ->
	    io:format("trace(~p, ~p, ~p) -> ~p",
		      [What,How,Flags,Other]),
	    ?t:fail({unexpected,Other})
    end.

expect_badarg_func(MFA, Pattern) ->
    case catch erlang:trace_pattern(MFA, Pattern) of
	{'EXIT',{badarg,Where}} ->
	    io:format("trace_pattern(~p, ~p) ->\n  {'EXIT',{badarg,~p}}",
		      [MFA,Pattern,Where]),
	    ok;
	Other ->
	    io:format("trace_pattern(~p, ~p) -> ~p",
		      [MFA, Pattern, Other]),
	    ?t:fail({unexpected,Other})
    end.

pam(doc) -> "Basic test of PAM.";
pam(suite) -> [];
pam(Config) when is_list(Config) ->
    ?line start_tracer(),
    ?line Self = self(),

    %% Build the match program.
    ?line Prog1 = {[{a,tuple},'$1'],[],[]},
    ?line Prog2 = {[{a,bigger,tuple},'$1'],[],[{message,'$1'}]},
    ?line MatchProg = [Prog1,Prog2],
    ?line pam_trace(MatchProg),

    %% Do some calls.
    ?line ?MODULE:pam_foo(not_a_tuple, [a,b]),
    ?line ?MODULE:pam_foo({a,tuple}, [a,list]),
    ?line ?MODULE:pam_foo([this,one,will,'not',match], dummy_arg),
    ?line LongList = lists:seq(1,10),
    ?line ?MODULE:pam_foo({a,bigger,tuple}, LongList),

    %% Check that we get the correct trace messages.
    ?line expect({trace,Self,call,{?MODULE,pam_foo,[{a,tuple},[a,list]]}}),
    ?line expect({trace,Self,call,
		  {?MODULE,pam_foo,[{a,bigger,tuple},LongList]},
		  LongList}),

    ?line trace_func({?MODULE,pam_foo,'_'}, false),
    ok.

pam_trace(Prog) ->
    1 = trace_func({?MODULE,pam_foo,'_'}, Prog),
    {match_spec,Prog} = trace_info({?MODULE,pam_foo,2}, match_spec),
    ok.

pam_foo(A, B) ->
    {ok,A,B}.


%% Test changing PAM programs for a function.
change_pam(_Config) ->
    case test_server:is_native(lists) of
	true -> {skip,"lists is native"};
	false -> change_pam()
    end.

change_pam() ->
    ?line start_tracer(),
    ?line Self = self(),

    %% Install the first match program.
    %% Test using timestamp at the same time.

    ?line trace_pid(Self, true, [call,arity,timestamp]),
    ?line Prog1 = [{['$1','$2'],[],[{message,'$1'}]}],
    ?line change_pam_trace(Prog1),
    ?line [x,y] = lists:append(id([x]), id([y])),
    ?line {heap_size,_} = erlang:process_info(Self, heap_size),
    ?line expect({trace_ts,Self,call,{lists,append,2},[x],ts}),
    ?line expect({trace_ts,Self,call,{erlang,process_info,2},Self,ts}),

    %% Install a new PAM program.

    ?line Prog2 = [{['$1','$2'],[],[{message,'$2'}]}],
    ?line change_pam_trace(Prog2),
    ?line [xx,yy] = lists:append(id([xx]), id([yy])),
    ?line {current_function,_} = erlang:process_info(Self, current_function),
    ?line expect({trace_ts,Self,call,{lists,append,2},[yy],ts}),
    ?line expect({trace_ts,Self,call,{erlang,process_info,2},current_function,ts}),

    ?line 1 = trace_func({lists,append,2}, false),
    ?line 1 = trace_func({erlang,process_info,2}, false),
    ?line {match_spec,false} = trace_info({lists,append,2}, match_spec),
    ?line {match_spec,false} = trace_info({erlang,process_info,2}, match_spec),

    ok.

change_pam_trace(Prog) ->
    1 = trace_func({lists,append,2}, Prog),
    1 = trace_func({erlang,process_info,2}, Prog),
    {match_spec,Prog} = trace_info({lists,append,2}, match_spec),
    {match_spec,Prog} = trace_info({erlang,process_info,2}, match_spec),
    ok.

return_trace(_Config) ->
    case test_server:is_native(lists) of
	true -> {skip,"lists is native"};
	false -> return_trace()
    end.

return_trace() ->
    X = {save,me},
    ?line start_tracer(),
    ?line Self = self(),

    %% Test call and return trace and timestamp.

    ?line trace_pid(Self, true, [call,timestamp]),
    Stupid = {pointless,tuple},
    ?line Prog1 = [{['$1','$2'],[],[{return_trace},{message,{Stupid}}]}],
    ?line 1 = trace_func({lists,append,2}, Prog1),
    ?line 1 = trace_func({erlang,process_info,2}, Prog1),
    ?line {match_spec,Prog1} = trace_info({lists,append,2}, match_spec),
    ?line {match_spec,Prog1} = trace_info({erlang,process_info,2}, match_spec),

    ?line [x,y] = lists:append(id([x]), id([y])),
    Current = {current_function,{?MODULE,return_trace,0}},
    ?line Current = erlang:process_info(Self, current_function),
    ?line expect({trace_ts,Self,call,{lists,append,[[x],[y]]},Stupid,ts}),
    ?line expect({trace_ts,Self,return_from,{lists,append,2},[x,y],ts}),
    ?line expect({trace_ts,Self,call,{erlang,process_info,[Self,current_function]},
		  Stupid,ts}),
    ?line expect({trace_ts,Self,return_from,{erlang,process_info,2},Current,ts}),

    %% Try catch/exit.

    ?line 1 = trace_func({?MODULE,nasty,0}, [{[],[],[{return_trace},{message,false}]}]),
    ?line {'EXIT',good_bye} = (catch ?MODULE:nasty()),
    ?line 1 = trace_func({?MODULE,nasty,0}, false),

    %% Turn off trace.

    ?line 1 = trace_func({lists,append,2}, false),
    ?line 1 = trace_func({erlang,process_info,2}, false),
    ?line {match_spec,false} = trace_info({lists,append,2}, match_spec),
    ?line {match_spec,false} = trace_info({erlang,process_info,2}, match_spec),

    %% No timestamp, no trace message for call.

    ?line trace_pid(Self, false, [timestamp]),
    ?line Prog2 = [{['$1','$2'],[],[{return_trace},{message,false}]},
		   {['$1'],[],[{return_trace},{message,false}]}],
    ?line 1 = trace_func({lists,seq,2}, Prog2),
    ?line 1 = trace_func({erlang,atom_to_list,1}, Prog2),
    ?line {match_spec,Prog2} = trace_info({lists,seq,2}, match_spec),
    ?line {match_spec,Prog2} = trace_info({erlang,atom_to_list,1}, match_spec),

    ?line lists:seq(2, 7),
    ?line _ = atom_to_list(non_literal(nisse)),
    ?line expect({trace,Self,return_from,{lists,seq,2},[2,3,4,5,6,7]}),
    ?line expect({trace,Self,return_from,{erlang,atom_to_list,1},"nisse"}),

    %% Turn off trace.

    ?line 1 = trace_func({lists,seq,2}, false),
    ?line 1 = trace_func({erlang,atom_to_list,1}, false),
    ?line {match_spec,false} = trace_info({lists,seq,2}, match_spec),
    ?line {match_spec,false} = trace_info({erlang,atom_to_list,1}, match_spec),

    ?line {save,me} = X,
    
    ok.

nasty() ->
    exit(good_bye).

exception_trace(_Config) ->
    case test_server:is_native(lists) of
	true -> {skip,"lists is native"};
	false -> exception_trace()
    end.

exception_trace() ->
    X = {save,me},
    ?line start_tracer(),
    ?line Self = self(),

    %% Test call and return trace and timestamp.

    ?line trace_pid(Self, true, [call,timestamp]),
    Stupid = {pointless,tuple},
    ?line Prog1 = [{['$1','$2'],[],[{exception_trace},{message,{Stupid}}]}],
    ?line 1 = trace_func({lists,append,2}, Prog1),
    ?line 1 = trace_func({erlang,process_info,2}, Prog1),
    ?line {match_spec,Prog1} = trace_info({lists,append,2}, match_spec),
    ?line {match_spec,Prog1} = 
	trace_info({erlang,process_info,2}, match_spec),

    ?line [x,y] = lists:append(id([x]), id([y])),
    Current = {current_function,{?MODULE,exception_trace,0}},
    ?line Current = erlang:process_info(Self, current_function),
    ?line expect({trace_ts,Self,call,{lists,append,[[x],[y]]},Stupid,ts}),
    ?line expect({trace_ts,Self,return_from,{lists,append,2},[x,y],ts}),
    ?line expect({trace_ts,Self,call,{erlang,process_info,
				      [Self,current_function]},
		  Stupid,ts}),
    ?line expect({trace_ts,Self,return_from,
		  {erlang,process_info,2},Current,ts}),

    %% Try catch/exit.

    ?line 1 = trace_func({?MODULE,nasty,0}, 
			 [{[],[],[{exception_trace},{message,false}]}]),
    ?line {'EXIT',good_bye} = (catch ?MODULE:nasty()),
    ?line expect({trace_ts,Self,exception_from,
		  {?MODULE,nasty,0},{exit,good_bye},ts}),
    ?line 1 = trace_func({?MODULE,nasty,0}, false),

    %% Turn off trace.

    ?line 1 = trace_func({lists,append,2}, false),
    ?line 1 = trace_func({erlang,process_info,2}, false),
    ?line {match_spec,false} = trace_info({lists,append,2}, match_spec),
    ?line {match_spec,false} = 
	trace_info({erlang,process_info,2}, match_spec),

    %% No timestamp, no trace message for call.

    ?line trace_pid(Self, false, [timestamp]),
    ?line Prog2 = [{['$1','$2'],[],[{exception_trace},{message,false}]},
		   {['$1'],[],[{exception_trace},{message,false}]}],
    ?line 1 = trace_func({lists,seq,2}, Prog2),
    ?line 1 = trace_func({erlang,atom_to_list,1}, Prog2),
    ?line {match_spec,Prog2} = trace_info({lists,seq,2}, match_spec),
    ?line {match_spec,Prog2} = 
	trace_info({erlang,atom_to_list,1}, match_spec),

    ?line lists:seq(2, 7),
    ?line _ = atom_to_list(non_literal(nisse)),
    ?line expect({trace,Self,return_from,{lists,seq,2},[2,3,4,5,6,7]}),
    ?line expect({trace,Self,return_from,{erlang,atom_to_list,1},"nisse"}),

    %% Turn off trace.

    ?line 1 = trace_func({lists,seq,2}, false),
    ?line 1 = trace_func({erlang,atom_to_list,1}, false),
    ?line {match_spec,false} = trace_info({lists,seq,2}, match_spec),
    ?line {match_spec,false} = 
	trace_info({erlang,atom_to_list,1}, match_spec),

    ?line expect(),
    ?line {save,me} = X,
    ok.

on_load(doc) -> "Test the on_load argument for trace_pattern/3.";
on_load(suite) -> [];
on_load(Config) when is_list(Config) ->
    ?line 0 = erlang:trace_pattern(on_load, []),
    ?line {traced,global} = erlang:trace_info(on_load, traced),
    ?line {match_spec,[]} = erlang:trace_info(on_load, match_spec),

    ?line 0 = erlang:trace_pattern(on_load, true, [local]),
    ?line {traced,local} = erlang:trace_info(on_load, traced),
    ?line {match_spec,[]} = erlang:trace_info(on_load, match_spec),

    ?line 0 = erlang:trace_pattern(on_load, false, [local]),
    ?line {traced,false} = erlang:trace_info(on_load, traced),
    ?line {match_spec,false} = erlang:trace_info(on_load, match_spec),

    ?line Pam1 = [{[],[],[{message,false}]}],
    ?line 0 = erlang:trace_pattern(on_load, Pam1),
    ?line {traced,global} = erlang:trace_info(on_load, traced),
    ?line {match_spec,Pam1} = erlang:trace_info(on_load, match_spec),

    ?line 0 = erlang:trace_pattern(on_load, true, [local]),
    ?line 0 = erlang:trace_pattern(on_load, false, [local]),

    ok.



deep_exception(doc) -> "Test the new exception trace.";
deep_exception(suite) -> [];
deep_exception(Config) when is_list(Config) ->
    deep_exception().

deep_exception() ->
    ?line start_tracer(),
    ?line Self = self(),
    ?line N = 200000,
    ?line LongImproperList = seq(1, N-1, N),
    
    Prog = [{'_',[],[{exception_trace}]}],
%%    ?line 1 = trace_pid(Self, true, [call]),
    ?line 1 = trace_func({?MODULE,deep,'_'}, Prog),
    ?line 1 = trace_func({?MODULE,deep_1,'_'}, Prog),
    ?line 1 = trace_func({?MODULE,deep_2,'_'}, Prog),
    ?line 1 = trace_func({?MODULE,deep_3,'_'}, Prog),
    ?line 1 = trace_func({?MODULE,deep_4,'_'}, Prog),
    ?line 1 = trace_func({?MODULE,deep_5,'_'}, Prog),
    ?line 1 = trace_func({?MODULE,id,'_'}, Prog),
    ?line 1 = trace_func({erlang,'++','_'}, Prog),
    ?line 1 = trace_func({erlang,exit,1}, Prog),
    ?line 1 = trace_func({erlang,throw,1}, Prog),
    ?line 2 = trace_func({erlang,error,'_'}, Prog),
    ?line 1 = trace_func({lists,reverse,2}, Prog),

    ?line deep_exception(?LINE, exit, [paprika], 1, 
			 [{trace,Self,call,{erlang,exit,[paprika]}},
			  {trace,Self,exception_from,{erlang,exit,1},
			   {exit,paprika}}], 
			 exception_from, {exit,paprika}),
    ?line deep_exception(?LINE, throw, [3.14], 2, 
			 [{trace,Self,call,{erlang,throw,[3.14]}},
			  {trace,Self,exception_from,{erlang,throw,1},
			   {throw,3.14}}], 
			 exception_from, {throw,3.14}),
    ?line deep_exception(?LINE, error, [{paprika}], 3, 
			 [{trace,Self,call,{erlang,error,[{paprika}]}},
			  {trace,Self,exception_from,{erlang,error,1},
			   {error,{paprika}}}], 
			 exception_from, {error,{paprika}}),
    ?line deep_exception(?LINE, error, ["{paprika}",[]], 3, 
			 [{trace,Self,call,{erlang,error,["{paprika}",[]]}},
			  {trace,Self,exception_from,{erlang,error,2},
			   {error,"{paprika}"}}], 
			 exception_from, {error,"{paprika}"}),
    ?line deep_exception(?LINE, id, [broccoli], 4, [], 
			 return_from, broccoli),
    ?line deep_exception(
	    ?LINE, append, [1,2], 5,
	    [{trace,Self,call,{erlang,'++',[1,2]}},
	     {trace,Self,exception_from,{erlang,'++',2},{error,badarg}}],
	    exception_from, {error,badarg}),
    ?line deep_exception(?LINE, '=', [1,2], 6, [],
			 exception_from, {error,{badmatch,2}}),
    %%
    ?line io:format("== Subtest: ~w", [?LINE]),
    ?line try lists:reverse(LongImproperList, []) of
	      R1 -> test_server:fail({returned,abbr(R1)})
	  catch error:badarg -> ok
	  end,
    ?line expect(fun ({trace,S,call,{lists,reverse,[L1,L2]}})
		     when is_list(L1), is_list(L2), S == Self ->
			 next;
		     ({trace,S,exception_from,
		       {lists,reverse,2},{error,badarg}}) 
		     when S == Self ->
			 expected;
		     ('_') ->
			 {trace,Self,exception_from,
			  {lists,reverse,2},{error,badarg}};
		     (_) ->
			 {unexpected,
			  {trace,Self,exception_from,
			   {lists,reverse,2},{error,badarg}}}
		 end),
    ?line deep_exception(?LINE, deep_5, [1,2], 7, 
			 [{trace,Self,call,{erlang,error,[undef]}},
			  {trace,Self,exception_from,{erlang,error,1},
			   {error,undef}}],
			 exception_from, {error,undef}),
    ?line deep_exception(?LINE, deep_5, [undef], 8, 
			 [{trace,Self,call,{?MODULE,deep_5,[undef]}},
			  {trace,Self,exception_from,{?MODULE,deep_5,1},
			   {error,function_clause}}],
			 exception_from, {error,function_clause}),
    
    %% Apply
    %%
    ?line deep_exception(?LINE, apply, [erlang,error,[[mo|rot]]], 1, 
			 [{trace,Self,call,{erlang,error,[[mo|rot]]}},
			  {trace,Self,exception_from,{erlang,error,1},
			   {error,[mo|rot]}}],
			 exception_from, {error,[mo|rot]}),
    ?line deep_exception(?LINE, apply, [erlang,error,[[mo|"rot"],[]]], 1, 
			 [{trace,Self,call,{erlang,error,[[mo|"rot"],[]]}},
			  {trace,Self,exception_from,{erlang,error,2},
			   {error,[mo|"rot"]}}],
			 exception_from, {error,[mo|"rot"]}),
    ?line Morot = make_ref(),
    ?line deep_exception(?LINE, apply, [erlang,throw,[Morot]], 3, 
			 [{trace,Self,call,{erlang,throw,[Morot]}},
			  {trace,Self,exception_from,{erlang,throw,1},
			   {throw,Morot}}],
			 exception_from, {throw,Morot}),
    ?line deep_exception(?LINE, apply, [erlang,exit,[["morot"|Morot]]], 2, 
			 [{trace,Self,call,{erlang,exit,[["morot"|Morot]]}},
			  {trace,Self,exception_from,{erlang,exit,1},
			   {exit,["morot"|Morot]}}],
			 exception_from, {exit,["morot"|Morot]}),
    ?line deep_exception(
	    ?LINE, apply, [?MODULE,id,[spenat]], 4,
	    [{trace,Self,call,{?MODULE,id,[spenat]}},
	     {trace,Self,return_from,{?MODULE,id,1},spenat}],
	    return_from, spenat),
    ?line deep_exception(
	    ?LINE, apply, [erlang,'++',[1,2]], 5,
	    [{trace,Self,call,{erlang,'++',[1,2]}},
	     {trace,Self,exception_from,{erlang,'++',2},{error,badarg}}],
	    exception_from, {error,badarg}),
    ?line io:format("== Subtest: ~w", [?LINE]),
    ?line try apply(lists, reverse, [LongImproperList, []]) of
	      R2 -> test_server:fail({returned,abbr(R2)})
	  catch error:badarg -> ok
	  end,
    ?line expect(fun ({trace,S,call,{lists,reverse,[L1,L2]}})
		     when is_list(L1), is_list(L2), S == Self ->
			 next;
		     ({trace,S,exception_from,
		       {lists,reverse,2},{error,badarg}}) 
		     when S == Self ->
			 expected;
		     ('_') ->
			 {trace,Self,exception_from,
			  {lists,reverse,2},{error,badarg}};
		     (_) ->
			 {unexpected,
			  {trace,Self,exception_from,
			   {lists,reverse,2},{error,badarg}}}
		 end),
    ?line deep_exception(?LINE, apply, [?MODULE,deep_5,[1,2]], 7, 
			 [{trace,Self,call,{erlang,error,[undef]}},
			  {trace,Self,exception_from,{erlang,error,1},
			   {error,undef}}],
			 exception_from, {error,undef}),
    ?line deep_exception(?LINE, apply, [?MODULE,deep_5,[undef]], 8, 
			 [{trace,Self,call,{?MODULE,deep_5,[undef]}},
			  {trace,Self,exception_from,{?MODULE,deep_5,1},
			   {error,function_clause}}],
			 exception_from, {error,function_clause}),
    %% Apply of fun
    %%
    ?line deep_exception(?LINE, apply, 
			 [fun () -> 
				  erlang:error([{"palsternacka",3.14},17]) 
			  end, []], 1, 
			 [{trace,Self,call,
			   {erlang,error,[[{"palsternacka",3.14},17]]}},
			  {trace,Self,exception_from,{erlang,error,1},
			   {error,[{"palsternacka",3.14},17]}}],
			 exception_from, {error,[{"palsternacka",3.14},17]}),
    ?line deep_exception(?LINE, apply, 
			 [fun () -> 
				  erlang:error(["palsternacka",17], []) 
			  end, []], 1, 
			 [{trace,Self,call,
			   {erlang,error,[["palsternacka",17],[]]}},
			  {trace,Self,exception_from,{erlang,error,2},
			   {error,["palsternacka",17]}}],
			 exception_from, {error,["palsternacka",17]}),
    ?line deep_exception(?LINE, apply, 
			 [fun () -> erlang:throw(Self) end, []], 2, 
			 [{trace,Self,call,{erlang,throw,[Self]}},
			  {trace,Self,exception_from,{erlang,throw,1},
			   {throw,Self}}],
			 exception_from, {throw,Self}),
    ?line deep_exception(?LINE, apply, 
			 [fun () -> 
				  erlang:exit({1,2,3,4,[5,palsternacka]})
			  end, []], 3,
			 [{trace,Self,call,
			   {erlang,exit,[{1,2,3,4,[5,palsternacka]}]}},
			  {trace,Self,exception_from,{erlang,exit,1},
			   {exit,{1,2,3,4,[5,palsternacka]}}}],
			 exception_from, {exit,{1,2,3,4,[5,palsternacka]}}),
    ?line deep_exception(?LINE, apply, 
			 [fun () -> ?MODULE:id(bladsallad) end, []], 4,
			 [{trace,Self,call,{?MODULE,id,[bladsallad]}},
			  {trace,Self,return_from,{?MODULE,id,1},bladsallad}],
			 return_from, bladsallad),
    ?line deep_exception(?LINE, apply, 
			 [fun (A, B) -> A ++ B end, [1,2]], 5,
			 [{trace,Self,call,{erlang,'++',[1,2]}},
			  {trace,Self,exception_from,
			   {erlang,'++',2},{error,badarg}}],
			 exception_from, {error,badarg}),
    ?line deep_exception(?LINE, apply, [fun (A, B) -> A = B end, [1,2]], 6, 
			 [],
			 exception_from, {error,{badmatch,2}}),
    ?line io:format("== Subtest: ~w", [?LINE]),
    ?line try apply(fun() -> lists:reverse(LongImproperList, []) end, []) of
	      R3 -> test_server:fail({returned,abbr(R3)})
	  catch error:badarg -> ok
	  end,
    ?line expect(fun ({trace,S,call,{lists,reverse,[L1,L2]}})
		     when is_list(L1), is_list(L2), S == Self ->
			 next;
		     ({trace,S,exception_from,
		       {lists,reverse,2},{error,badarg}}) 
		     when S == Self ->
			 expected;
		     ('_') ->
			 {trace,Self,exception_from,
			  {lists,reverse,2},{error,badarg}};
		     (_) ->
			 {unexpected,
			  {trace,Self,exception_from,
			   {lists,reverse,2},{error,badarg}}}
		 end),
    ?line deep_exception(?LINE, apply, 
			 [fun () -> ?MODULE:deep_5(1,2) end, []], 7, 
			 [{trace,Self,call,{erlang,error,[undef]}},
			  {trace,Self,exception_from,{erlang,error,1},
			   {error,undef}}],
			 exception_from, {error,undef}),
    ?line deep_exception(?LINE, apply, 
			 [fun () -> ?MODULE:deep_5(undef) end, []], 8, 
			 [{trace,Self,call,{?MODULE,deep_5,[undef]}},
			  {trace,Self,exception_from,{?MODULE,deep_5,1},
			   {error,function_clause}}],
			 exception_from, {error,function_clause}),
    
    ?line trace_func({?MODULE,'_','_'}, false),
    ?line trace_func({erlang,'_','_'}, false),
    ?line trace_func({lists,'_','_'}, false),
    ?line expect(),
    ?line ok.


deep_exception(Line, B, Q, N, Extra, Tag, R) ->
    ?line Self = self(),
    ?line io:format("== Subtest: ~w", [Line]),
    ?line Result = ?MODULE:deep(N, B, Q),
    ?line Result = deep_expect(Self, B, Q, N, Extra, Tag, R).

deep_expect(Self, B, Q, N, Extra, Tag, R) ->
    ?line expect({trace,Self,call,{?MODULE,deep,[N,B,Q]}}),
    ?line Result = deep_expect_N(Self, B, Q, N, Extra, Tag, R),
    ?line expect({trace,Self,return_from,{?MODULE,deep,3},Result}),
    ?line Result.

deep_expect_N(Self, B, Q, N, Extra, Tag, R) ->
    deep_expect_N(Self, B, Q, N, Extra, Tag, R, N).

deep_expect_N(Self, B, Q, N, Extra, Tag, R, J) when J > 0 ->
    ?line expect({trace,Self,call,{?MODULE,deep_1,[J,B,Q]}}),
    ?line deep_expect_N(Self, B, Q, N, Extra, Tag, R, J-1);
deep_expect_N(Self, B, Q, N, Extra, Tag, R, 0) ->
    ?line expect({trace,Self,call,{?MODULE,deep_2,[B,Q]}}),
    ?line expect({trace,Self,call,{?MODULE,deep_3,[B,Q]}}),
    ?line expect({trace,Self,return_from,{?MODULE,deep_3,2},{B,Q}}),
    ?line expect({trace,Self,call,{?MODULE,deep_4,[{B,Q}]}}),
    ?line expect({trace,Self,call,{?MODULE,id,[{B,Q}]}}),
    ?line expect({trace,Self,return_from,{?MODULE,id,1},{B,Q}}),
    ?line deep_expect_Extra(Self, N, Extra, Tag, R),
    ?line expect({trace,Self,Tag,{?MODULE,deep_4,1},R}),
    ?line expect({trace,Self,Tag,{?MODULE,deep_2,2},R}),
    ?line deep_expect_N(Self, N, Tag, R).

deep_expect_Extra(Self, N, [E|Es], Tag, R) ->
    ?line expect(E),
    ?line deep_expect_Extra(Self, N, Es, Tag, R);
deep_expect_Extra(_Self, _N, [], _Tag, _R) ->
    ?line ok.

deep_expect_N(Self, N, Tag, R) when N > 0 ->
    ?line expect({trace,Self,Tag,{?MODULE,deep_1,3},R}),
    ?line deep_expect_N(Self, N-1, Tag, R);
deep_expect_N(_Self, 0, return_from, R) ->
    ?line {value,R};
deep_expect_N(_Self, 0, exception_from, R) ->
    ?line R.



exception_nocatch(doc) -> "Test the new exception trace.";
exception_nocatch(suite) -> [];
exception_nocatch(Config) when is_list(Config) ->
    exception_nocatch().

exception_nocatch() ->
    Deep4LocThrow = get_deep_4_loc({throw,[42]}),
    Deep4LocError = get_deep_4_loc({error,[42]}),
    Deep4LocBadmatch = get_deep_4_loc({'=',[a,b]}),

    Prog = [{'_',[],[{exception_trace}]}],
    ?line 1 = erlang:trace_pattern({?MODULE,deep_1,'_'}, Prog),
    ?line 1 = erlang:trace_pattern({?MODULE,deep_2,'_'}, Prog),
    ?line 1 = erlang:trace_pattern({?MODULE,deep_3,'_'}, Prog),
    ?line 1 = erlang:trace_pattern({?MODULE,deep_4,'_'}, Prog),
    ?line 1 = erlang:trace_pattern({?MODULE,deep_5,'_'}, Prog),
    ?line 1 = erlang:trace_pattern({?MODULE,id,'_'}, Prog),
    ?line 1 = erlang:trace_pattern({erlang,exit,1}, Prog),
    ?line 1 = erlang:trace_pattern({erlang,throw,1}, Prog),
    ?line 2 = erlang:trace_pattern({erlang,error,'_'}, Prog),
    ?line Q1 = {make_ref(),Prog},
    ?line T1 = 
	exception_nocatch(?LINE, exit, [Q1], 3, 
			  [{trace,t1,call,{erlang,exit,[Q1]}},
			   {trace,t1,exception_from,{erlang,exit,1},
			    {exit,Q1}}],
			  exception_from, {exit,Q1}),
    ?line expect({trace,T1,exit,Q1}),
    ?line Q2 = {cake,14.125},
    ?line T2 = 
	exception_nocatch(?LINE, throw, [Q2], 2, 
			  [{trace,t2,call,{erlang,throw,[Q2]}},
			   {trace,t2,exception_from,{erlang,throw,1},
			    {error,{nocatch,Q2}}}],
			  exception_from, {error,{nocatch,Q2}}),
    ?line expect({trace,T2,exit,{{nocatch,Q2},[{erlang,throw,[Q2],[]},
					       {?MODULE,deep_4,1,
						Deep4LocThrow}]}}),
    ?line Q3 = {dump,[dump,{dump}]},
    ?line T3 = 
	exception_nocatch(?LINE, error, [Q3], 4, 
			  [{trace,t3,call,{erlang,error,[Q3]}},
			   {trace,t3,exception_from,{erlang,error,1},
			    {error,Q3}}],
			  exception_from, {error,Q3}),
    ?line expect({trace,T3,exit,{Q3,[{erlang,error,[Q3],[]},
				     {?MODULE,deep_4,1,Deep4LocError}]}}),
    ?line T4 = 
	exception_nocatch(?LINE, '=', [17,4711], 5, [], 
			  exception_from, {error,{badmatch,4711}}),
    ?line expect({trace,T4,exit,{{badmatch,4711},
				 [{?MODULE,deep_4,1,Deep4LocBadmatch}]}}),
    %%
    ?line erlang:trace_pattern({?MODULE,'_','_'}, false),
    ?line erlang:trace_pattern({erlang,'_','_'}, false),
    ?line expect(),
    ?line ok.

get_deep_4_loc(Arg) ->
    try
	deep_4(Arg),
	?t:fail(should_not_return_to_here)
    catch
	_:_ ->
	    [{?MODULE,deep_4,1,Loc0}|_] = erlang:get_stacktrace(),
	    Loc0
    end.

exception_nocatch(Line, B, Q, N, Extra, Tag, R) ->
    ?line io:format("== Subtest: ~w", [Line]),
    ?line Go = make_ref(),
    ?line Tracee = 
	spawn(fun () ->
		      receive
			  Go ->
			      deep_1(N, B, Q)
		      end
	      end),
    ?line 1 = erlang:trace(Tracee, true, [call,return_to,procs]),
    ?line Tracee ! Go,
    ?line deep_expect_N(Tracee, B, Q, N-1, 
			[setelement(2, T, Tracee) || T <- Extra], Tag, R),
    ?line Tracee.

%% Make sure that code that uses the optimized bit syntax matching
%% can be traced without crashing the emulator. (Actually, it seems
%% that we can't trigger the bug using external call trace, but we
%% will keep the test case anyway.)

bit_syntax(Config) when is_list(Config) ->
    ?line start_tracer(),
    ?line 1 = trace_func({?MODULE,bs_sum_a,'_'}, []),
    ?line 1 = trace_func({?MODULE,bs_sum_b,'_'}, []),

    ?line 6 = call_bs_sum_a(<<1,2,3>>),
    ?line 10 = call_bs_sum_b(<<1,2,3,4>>),

    ?line trace_func({?MODULE,'_','_'}, false),
    ?line erlang:trace_delivered(all),
    receive
	{trace_delivered,_,_} -> ok
    end,
    
    Self = self(),
    ?line expect({trace,Self,call,{?MODULE,bs_sum_a,[<<2,3>>,1]}}),
    ?line expect({trace,Self,call,{?MODULE,bs_sum_b,[1,<<2,3,4>>]}}),

    ok.

call_bs_sum_a(<<H,T/binary>>) ->
    ?MODULE:bs_sum_a(T, H).

call_bs_sum_b(<<H,T/binary>>) ->
    ?MODULE:bs_sum_b(H, T).

bs_sum_a(<<H,T/binary>>, Acc) -> bs_sum_a(T, H+Acc);
bs_sum_a(<<>>, Acc) -> Acc.

bs_sum_b(Acc, <<H,T/binary>>) -> bs_sum_b(H+Acc, T);
bs_sum_b(Acc, <<>>) -> Acc.
    



%%% Help functions.

expect() ->
    case flush() of
	[] -> ok;
	Msgs ->
	    test_server:fail({unexpected,abbr(Msgs)})
    end.

expect({trace_ts,Pid,Type,MFA,Term,ts}=Message) ->
    receive
	M ->
	    case M of
		{trace_ts,Pid,Type,MFA,Term,Ts}=MessageTs ->
		    ok = io:format("Expected and got ~p", [abbr(MessageTs)]),
		    Ts;
		_ ->
		    io:format("Expected ~p; got ~p", [abbr(Message),abbr(M)]),
		    test_server:fail({unexpected,abbr([M|flush()])})
	    end
    after 5000 ->
	    io:format("Expected ~p; got nothing", [abbr(Message)]),
	    test_server:fail(no_trace_message)
    end;
expect({trace_ts,Pid,Type,MFA,ts}=Message) ->
    receive
	M ->
	    case M of
		{trace_ts,Pid,Type,MFA,Ts} ->
		    ok = io:format("Expected and got ~p", [abbr(M)]),
		    Ts;
		_ ->
		    io:format("Expected ~p; got ~p", [abbr(Message),abbr(M)]),
		    test_server:fail({unexpected,abbr([M|flush()])})
	    end
    after 5000 ->
	    io:format("Expected ~p; got nothing", [abbr(Message)]),
	    test_server:fail(no_trace_message)
    end;
expect(Validator) when is_function(Validator) ->
    receive
	M ->
	    case Validator(M) of
		expected ->
		    ok = io:format("Expected and got ~p", [abbr(M)]);
 		next ->
 		    ok = io:format("Expected and got ~p", [abbr(M)]),
 		    expect(Validator);
 		{unexpected,Message} ->
 		    io:format("Expected ~p; got ~p", [abbr(Message),abbr(M)]),
 		    test_server:fail({unexpected,abbr([M|flush()])})
	    end
    after 5000 ->
	    io:format("Expected ~p; got nothing", [abbr(Validator('_'))]),
	    test_server:fail(no_trace_message)
    end;
expect(Message) ->
    receive
	M ->
	    case M of
		Message ->
		    ok = io:format("Expected and got ~p", [abbr(Message)]);
		Other ->
		    io:format("Expected ~p; got ~p", 
			      [abbr(Message),abbr(Other)]),
		    test_server:fail({unexpected,abbr([Other|flush()])})
	    end
    after 5000 ->
	    io:format("Expected ~p; got nothing", [abbr(Message)]),
	    test_server:fail(no_trace_message)
    end.

trace_info(What, Key) ->
    get(tracer) ! {apply,self(),{erlang,trace_info,[What,Key]}},
    Res = receive
	      {apply_result,Result} -> Result
	  end,
    ok = io:format("erlang:trace_info(~p, ~p) -> ~p",
		   [What,Key,Res]),
    Res.
    
trace_func(MFA, MatchSpec) ->
    trace_func(MFA, MatchSpec, []).
trace_func(MFA, MatchSpec, Flags) ->
    get(tracer) ! {apply,self(),{erlang,trace_pattern,[MFA, MatchSpec, Flags]}},
    Res = receive
	      {apply_result,Result} -> Result
	  end,
    ok = io:format("trace_pattern(~p, ~p, ~p) -> ~p", [MFA,MatchSpec,Flags,Res]),
    Res.

trace_pid(Pid, On, Flags) ->
    get(tracer) ! {apply,self(),{erlang,trace,[Pid,On,Flags]}},
    Res = receive
	      {apply_result,Result} -> Result
	  end,
    ok = io:format("trace(~p, ~p, ~p) -> ~p", [Pid,On,Flags,Res]),
    Res.

start_tracer() ->
    Self = self(),
    put(tracer, spawn(fun() -> tracer(Self) end)),
    get(tracer).

start_tracer_loop() ->
    Self = self(),
    put(tracer, spawn(fun() -> tracer_loop(Self) end)),
    get(tracer).

tracer(RelayTo) ->
    erlang:trace(RelayTo, true, [call]),
    tracer_loop(RelayTo).

tracer_loop(RelayTo) ->
    receive
	{apply,From,{M,F,A}} ->
	    From ! {apply_result,apply(M, F, A)},
	    tracer_loop(RelayTo);
	Msg ->
	    RelayTo ! Msg,
	    tracer_loop(RelayTo)
    end.

id(I) -> I.

deep(N, Class, Reason) ->
    try ?MODULE:deep_1(N, Class, Reason) of
	Value -> {value,Value}
    catch C:R -> {C,R}
    end.

deep_1(1, Class, Reason) ->
    ?MODULE:deep_2(Class, Reason);
deep_1(N, Class, Reason) when is_integer(N), N > 1 ->
    ?MODULE:deep_1(N-1, Class, Reason).

deep_2(Class, Reason) ->
    ?MODULE:deep_4(?MODULE:deep_3(Class, Reason)).

deep_3(Class, Reason) ->
    {Class,Reason}.

deep_4(CR) ->
    case ?MODULE:id(CR) of
	{exit,[Reason]} ->
	    erlang:exit(Reason);
	{throw,[Reason]} ->
	    erlang:throw(Reason);
	{error,[Reason,Arglist]} ->
	    erlang:error(Reason, Arglist);
	{error,[Reason]} ->
	    erlang:error(Reason);
	{id,[Reason]} ->
	    Reason;
	{reverse,[A,B]} ->
	    lists:reverse(A, B);
	{append,[A,B]} ->
	    A ++ B;
	{apply,[Fun,Args]} ->
	    erlang:apply(Fun, Args);
	{apply,[M,F,Args]} ->
	    erlang:apply(M, F, Args);
	{deep_5,[A,B]} ->
	    ?MODULE:deep_5(A, B);
	{deep_5,[A]} ->
	    ?MODULE:deep_5(A);
	{'=',[A,B]} ->
	    A = B
    end.

deep_5(A) when is_integer(A) ->
    A.

flush() ->
    receive X ->
	    [X|flush()]
    after 1000 ->
	    []
    end.

%% Abbreviate large complex terms
abbr(Term) ->
    abbr(Term, 20).
%%
abbr(Tuple, N) when is_tuple(Tuple) ->
    abbr_tuple(Tuple, 1, N, []);
abbr(List, N) when is_list(List) ->
    abbr_list(List, N, []);
abbr(Term, _) -> Term.
%%
abbr_tuple(_, _, 0, R) ->
    list_to_tuple(reverse(R, ['...']));
abbr_tuple(Tuple, J, N, R) when J =< size(Tuple) ->
    M = N-1,
    abbr_tuple(Tuple, J+1, M, [abbr(element(J, Tuple), M)|R]);
abbr_tuple(_, _, _, R) ->
    list_to_tuple(reverse(R)).
%%
abbr_list(_, 0, R) ->
    case io_lib:printable_list(R) of
	true ->
	    reverse(R, "...");
	false ->
	    reverse(R, '...')
    end;
abbr_list([H|T], N, R) ->
    M = N-1,
    abbr_list(T, M, [abbr(H, M)|R]);
abbr_list(T, _, R) ->
    reverse(R, T).

%% Lean and mean list functions

%% Do not build garbage
seq(M, N, R) when M =< N ->
    seq(M, N-1, [N|R]);
seq(_, _, R) -> R.

%% lists:reverse can not be called since it is traced
reverse(L) ->
    reverse(L, []).
%%
reverse([], R) -> R;
reverse([H|T], R) ->
    reverse(T, [H|R]).
