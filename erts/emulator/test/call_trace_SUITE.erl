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

%%% Purpose : Tests the new call_trace BIF.

-module(call_trace_SUITE).

-export([all/0, suite/0,
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

-include_lib("common_test/include/ct.hrl").

-define(P, 20).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

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

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    %% Reloading the module will clear all trace patterns, and
    %% in a debug-compiled emulator run assertions of the counters
    %% for the number of traced exported functions in this module.

    c:l(?MODULE).

hipe(Config) when is_list(Config) ->
    0 = erlang:trace_pattern({?MODULE,worker_foo,1}, true),
    0 = erlang:trace_pattern({?MODULE,worker_foo,1}, true, [local]),
    AllFuncs = erlang:trace_pattern({'_','_','_'}, true),

    %% Make sure that a traced, exported function can still be found.
    true = erlang:function_exported(error_handler, undefined_function, 3),
    AllFuncs = erlang:trace_pattern({'_','_','_'}, false),
    ok.

%% Tests 'all', 'new', and 'existing' for specifying processes.
process_specs(Config) when is_list(Config) ->
    Tracer = start_tracer(),
    {flags,[call]} = trace_info(self(), flags),
    {tracer,Tracer} = trace_info(self(), tracer),
    trace_func({?MODULE,worker_foo,1}, []),

    %% Test the 'new' and 'new_processes' flags.

    New = fun(Flag) ->
                  {Work1A,Work1B} = start_and_trace(Flag, [1,2,3], A1B={3,2,1}),
                  {flags,[]} = trace_info(Work1A, flags),
                  {tracer,[]} = trace_info(Work1A, tracer),
                  {tracer,Tracer} = trace_info(Work1B, tracer),
                  {flags,[call]} = trace_info(Work1B, flags),
                  expect({trace,Work1B,call,{?MODULE,worker_foo,[A1B]}}),
                  unlink(Work1B),
                  Mref = erlang:monitor(process, Work1B),
                  exit(Work1B, kill),
                  receive
                      {'DOWN',Mref,_,_,_} -> ok
                  end,
                  undefined = trace_info(Work1B, flags),
                  {flags,[]} = trace_info(Flag, flags),
                  {tracer,[]} = trace_info(Flag, tracer)
          end,
    New(new),
    New(new_processes),

    %% Test the 'existing' and 'existing_processes' flags.
    Existing =
        fun(Flag) ->
                {Work2A,_Work2B} = start_and_trace(Flag, A2A=[5,6,7], [7,6,5]),
                expect({trace,Work2A,call,{?MODULE,worker_foo,[A2A]}})
        end,
    Existing(existing),
    Existing(existing_processes),

    %% Test the 'all' and 'processes' flags.
    All =
        fun(Flag) ->
                   {Work3A,Work3B} = start_and_trace(Flag, A3A=[12,13], A3B=[13,12]),
                   expect({trace,Work3A,call,{?MODULE,worker_foo,[A3A]}}),
                   expect({trace,Work3B,call,{?MODULE,worker_foo,[A3B]}})
        end,
    All(all),
    All(processes),

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
    spawn(fun worker_loop/0).

call_worker(Pid, Arg) ->
    Pid ! {self(),{call,Arg}},
    receive
        {result,Res} -> Res
    after 5000 ->
              ct:fail(no_answer_from_worker)
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
    start_tracer(),
    trace_info(self(), flags),
    trace_info(self(), tracer),
    0 = trace_func({?MODULE,no_such_function,0}, []),
    {traced,undefined} = 
    trace_info({?MODULE,no_such_function,0}, traced),
    {match_spec, undefined} = 
    trace_info({?MODULE,no_such_function,0}, match_spec),

    %% Trace some functions...

    trace_func({lists,'_','_'}, []),

    %% Make sure that tracing the same functions more than once
    %% does not cause any problems.
    3 = trace_func({?MODULE,foo,'_'}, true),
    3 = trace_func({?MODULE,foo,'_'}, true),
    1 = trace_func({?MODULE,bar,0}, true),
    1 = trace_func({?MODULE,bar,0}, true),
    {traced,global} = trace_info({?MODULE,bar,0}, traced),
    1 = trace_func({erlang,list_to_integer,1}, true),
    {traced,global} = trace_info({erlang,list_to_integer,1}, traced),

    %% ... and call them...

    AList = [x,y,z],
    true = lists:member(y, AList),
    foo0 = ?MODULE:foo(),
    4 = ?MODULE:foo(3),
    11 = ?MODULE:foo(7, 4),
    ok = ?MODULE:bar(),
    42 = list_to_integer(non_literal("42")),

    %% ... make sure the we got trace messages (but not for ?MODULE:expect/1).

    Self = self(),
    ?MODULE:expect({trace,Self,call,{lists,member,[y,AList]}}),
    ?MODULE:expect({trace,Self,call,{?MODULE,foo,[]}}),
    ?MODULE:expect({trace,Self,call,{?MODULE,foo,[3]}}),
    ?MODULE:expect({trace,Self,call,{?MODULE,foo,[7,4]}}),
    ?MODULE:expect({trace,Self,call,{?MODULE,bar,[]}}),
    ?MODULE:expect({trace,Self,call,{erlang,list_to_integer,["42"]}}),

    %% Turn off trace for this module and call functions...

    trace_func({?MODULE,'_','_'}, false),
    {traced,false} = trace_info({?MODULE,bar,0}, traced),
    foo0 = ?MODULE:foo(),
    4 = ?MODULE:foo(3),
    11 = ?MODULE:foo(7, 4),
    ok = ?MODULE:bar(),
    [1,2,3,4,5,6,7,8,9,10] = lists:seq(1, 10),
    777 = list_to_integer(non_literal("777")),

    %% ... turn on all trace messages...

    trace_func({'_','_','_'}, false),
    [b,a] = lists:reverse([a,b]),

    %% Read out the remaining trace messages.

    ?MODULE:expect({trace,Self,call,{lists,seq,[1,10]}}),
    ?MODULE:expect({trace,Self,call,{erlang,list_to_integer,["777"]}}),
    receive
        Any ->
            ct:fail({unexpected_message,Any})
    after 1 ->
              ok
    end,

    %% Turn on and then off tracing on all external functions.
    %% This might cause the emulator to crasch later if it doesn't
    %% restore all export entries properly.

    AllFuncs = trace_func({'_','_','_'}, true),
    io:format("AllFuncs = ~p", [AllFuncs]),
    %% Make sure that a traced, exported function can still be found.
    true = erlang:function_exported(error_handler, undefined_function, 3),
    AllFuncs = trace_func({'_','_','_'}, false),
    erlang:trace_delivered(all),
    receive
        {trace_delivered,_,_} -> ok
    end,
    c:flush(),					% Print the traces messages.
    c:flush(),					% Print the traces messages.

    {traced,false} = trace_info({erlang,list_to_integer,1}, traced),

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

%% Test tracing on module being upgraded
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
    Data = proplists:get_value(data_dir, Config),
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
    Tracer = start_tracer_loop(),
    trace_pid(self(), true, [call,{tracer,Tracer}]),

    %% Trace some functions...

    trace_func({filename,'_','_'}, true),

    %% ... and call them...

    Self = self(),
    filename:absname("nisse"),
    ?MODULE:expect({trace,Self,call,{filename,absname,["nisse"]}}),
    trace_pid(Self, true, [call,arity]),
    filename:absname("kalle"),
    filename:absname("kalle", "/root"),
    ?MODULE:expect({trace,Self,call,{filename,absname,1}}),
    ?MODULE:expect({trace,Self,call,{filename,absname,2}}),
    trace_info(Self, flags),

    %% Timestamp + arity.

    flag_test(fun() ->
                      trace_pid(Self, true, [timestamp]),
                      "dum" = filename:basename("/abcd/dum"),
                      Ts = expect({trace_ts,Self,call,{filename,basename,1},ts}),
                      trace_info(Self, flags),
                      Ts
              end),

    %% Timestamp.

    AnArg = "/abcd/hejsan",
    flag_test(fun() ->
                      trace_pid(Self, false, [arity]),
                      "hejsan" = filename:basename(AnArg),
                      Ts = expect({trace_ts,Self,call,
                                   {filename,basename,[AnArg]},ts}),
                      trace_info(Self, flags),
                      Ts
              end),

    %% All flags turned off.

    trace_pid(Self, false, [timestamp]),
    AnotherArg = filename:join(AnArg, "hoppsan"),
    "hoppsan" = filename:basename(AnotherArg),
    expect({trace,Self,call,{filename,join,[AnArg,"hoppsan"]}}),
    expect({trace,Self,call,{filename,basename,[AnotherArg]}}),
    trace_info(Self, flags),

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
            ct:fail("Now = ~p, Ts = ~p", [Now, Ts])
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
                    ct:fail("Strange CPU timestamp: ~p", [Ts])
            end,
            io:format("Turned off CPU timestamps")
    catch
        error:badarg -> ok
    end.

%% Test bad arguments for trace/3 and trace_pattern/3.
errors(Config) when is_list(Config) ->
    expect_badarg_pid(aaa, true, []),
    expect_badarg_pid({pid,dum}, false, []),
    expect_badarg_func({'_','_',1}, []),
    expect_badarg_func({'_',gosh,1}, []),
    expect_badarg_func({xxx,'_',2}, []),
    expect_badarg_func({xxx,yyy,b}, glurp),
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
            ct:fail({unexpected,Other})
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
            ct:fail({unexpected,Other})
    end.

%% Basic test of PAM.
pam(Config) when is_list(Config) ->
    start_tracer(),
    Self = self(),

    %% Build the match program.
    Prog1 = {[{a,tuple},'$1'],[],[]},
    Prog2 = {[{a,bigger,tuple},'$1'],[],[{message,'$1'}]},
    MatchProg = [Prog1,Prog2],
    pam_trace(MatchProg),

    %% Do some calls.
    ?MODULE:pam_foo(not_a_tuple, [a,b]),
    ?MODULE:pam_foo({a,tuple}, [a,list]),
    ?MODULE:pam_foo([this,one,will,'not',match], dummy_arg),
    LongList = lists:seq(1,10),
    ?MODULE:pam_foo({a,bigger,tuple}, LongList),

    %% Check that we get the correct trace messages.
    expect({trace,Self,call,{?MODULE,pam_foo,[{a,tuple},[a,list]]}}),
    expect({trace,Self,call,
            {?MODULE,pam_foo,[{a,bigger,tuple},LongList]},
            LongList}),

    trace_func({?MODULE,pam_foo,'_'}, false),
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
    start_tracer(),
    Self = self(),

    %% Install the first match program.
    %% Test using timestamp at the same time.

    trace_pid(Self, true, [call,arity,timestamp]),
    Prog1 = [{['$1','$2'],[],[{message,'$1'}]}],
    change_pam_trace(Prog1),
    [x,y] = lists:append(id([x]), id([y])),
    {heap_size,_} = erlang:process_info(Self, heap_size),
    expect({trace_ts,Self,call,{lists,append,2},[x],ts}),
    expect({trace_ts,Self,call,{erlang,process_info,2},Self,ts}),

    %% Install a new PAM program.

    Prog2 = [{['$1','$2'],[],[{message,'$2'}]}],
    change_pam_trace(Prog2),
    [xx,yy] = lists:append(id([xx]), id([yy])),
    {current_function,_} = erlang:process_info(Self, current_function),
    expect({trace_ts,Self,call,{lists,append,2},[yy],ts}),
    expect({trace_ts,Self,call,{erlang,process_info,2},current_function,ts}),

    1 = trace_func({lists,append,2}, false),
    1 = trace_func({erlang,process_info,2}, false),
    {match_spec,false} = trace_info({lists,append,2}, match_spec),
    {match_spec,false} = trace_info({erlang,process_info,2}, match_spec),

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
    start_tracer(),
    Self = self(),

    %% Test call and return trace and timestamp.

    trace_pid(Self, true, [call,timestamp]),
    Stupid = {pointless,tuple},
    Prog1 = [{['$1','$2'],[],[{return_trace},{message,{Stupid}}]}],
    1 = trace_func({lists,append,2}, Prog1),
    1 = trace_func({erlang,process_info,2}, Prog1),
    {match_spec,Prog1} = trace_info({lists,append,2}, match_spec),
    {match_spec,Prog1} = trace_info({erlang,process_info,2}, match_spec),

    [x,y] = lists:append(id([x]), id([y])),
    Current = {current_function,{?MODULE,return_trace,0}},
    Current = erlang:process_info(Self, current_function),
    expect({trace_ts,Self,call,{lists,append,[[x],[y]]},Stupid,ts}),
    expect({trace_ts,Self,return_from,{lists,append,2},[x,y],ts}),
    expect({trace_ts,Self,call,{erlang,process_info,[Self,current_function]},
            Stupid,ts}),
    expect({trace_ts,Self,return_from,{erlang,process_info,2},Current,ts}),

    %% Try catch/exit.

    1 = trace_func({?MODULE,nasty,0}, [{[],[],[{return_trace},{message,false}]}]),
    {'EXIT',good_bye} = (catch ?MODULE:nasty()),
    1 = trace_func({?MODULE,nasty,0}, false),

    %% Turn off trace.

    1 = trace_func({lists,append,2}, false),
    1 = trace_func({erlang,process_info,2}, false),
    {match_spec,false} = trace_info({lists,append,2}, match_spec),
    {match_spec,false} = trace_info({erlang,process_info,2}, match_spec),

    %% No timestamp, no trace message for call.

    trace_pid(Self, false, [timestamp]),
    Prog2 = [{['$1','$2'],[],[{return_trace},{message,false}]},
             {['$1'],[],[{return_trace},{message,false}]}],
    1 = trace_func({lists,seq,2}, Prog2),
    1 = trace_func({erlang,atom_to_list,1}, Prog2),
    {match_spec,Prog2} = trace_info({lists,seq,2}, match_spec),
    {match_spec,Prog2} = trace_info({erlang,atom_to_list,1}, match_spec),

    lists:seq(2, 7),
    _ = atom_to_list(non_literal(nisse)),
    expect({trace,Self,return_from,{lists,seq,2},[2,3,4,5,6,7]}),
    expect({trace,Self,return_from,{erlang,atom_to_list,1},"nisse"}),

    %% Turn off trace.

    1 = trace_func({lists,seq,2}, false),
    1 = trace_func({erlang,atom_to_list,1}, false),
    {match_spec,false} = trace_info({lists,seq,2}, match_spec),
    {match_spec,false} = trace_info({erlang,atom_to_list,1}, match_spec),

    {save,me} = X,

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
    start_tracer(),
    Self = self(),

    %% Test call and return trace and timestamp.

    trace_pid(Self, true, [call,timestamp]),
    Stupid = {pointless,tuple},
    Prog1 = [{['$1','$2'],[],[{exception_trace},{message,{Stupid}}]}],
    1 = trace_func({lists,append,2}, Prog1),
    1 = trace_func({erlang,process_info,2}, Prog1),
    {match_spec,Prog1} = trace_info({lists,append,2}, match_spec),
    {match_spec,Prog1} = 
    trace_info({erlang,process_info,2}, match_spec),

    [x,y] = lists:append(id([x]), id([y])),
    Current = {current_function,{?MODULE,exception_trace,0}},
    Current = erlang:process_info(Self, current_function),
    expect({trace_ts,Self,call,{lists,append,[[x],[y]]},Stupid,ts}),
    expect({trace_ts,Self,return_from,{lists,append,2},[x,y],ts}),
    expect({trace_ts,Self,call,{erlang,process_info,
                                [Self,current_function]},
            Stupid,ts}),
    expect({trace_ts,Self,return_from,
            {erlang,process_info,2},Current,ts}),

    %% Try catch/exit.

    1 = trace_func({?MODULE,nasty,0}, 
                   [{[],[],[{exception_trace},{message,false}]}]),
    {'EXIT',good_bye} = (catch ?MODULE:nasty()),
    expect({trace_ts,Self,exception_from,
            {?MODULE,nasty,0},{exit,good_bye},ts}),
    1 = trace_func({?MODULE,nasty,0}, false),

    %% Turn off trace.

    1 = trace_func({lists,append,2}, false),
    1 = trace_func({erlang,process_info,2}, false),
    {match_spec,false} = trace_info({lists,append,2}, match_spec),
    {match_spec,false} = 
    trace_info({erlang,process_info,2}, match_spec),

    %% No timestamp, no trace message for call.

    trace_pid(Self, false, [timestamp]),
    Prog2 = [{['$1','$2'],[],[{exception_trace},{message,false}]},
             {['$1'],[],[{exception_trace},{message,false}]}],
    1 = trace_func({lists,seq,2}, Prog2),
    1 = trace_func({erlang,atom_to_list,1}, Prog2),
    {match_spec,Prog2} = trace_info({lists,seq,2}, match_spec),
    {match_spec,Prog2} = 
    trace_info({erlang,atom_to_list,1}, match_spec),

    lists:seq(2, 7),
    _ = atom_to_list(non_literal(nisse)),
    expect({trace,Self,return_from,{lists,seq,2},[2,3,4,5,6,7]}),
    expect({trace,Self,return_from,{erlang,atom_to_list,1},"nisse"}),

    %% Turn off trace.

    1 = trace_func({lists,seq,2}, false),
    1 = trace_func({erlang,atom_to_list,1}, false),
    {match_spec,false} = trace_info({lists,seq,2}, match_spec),
    {match_spec,false} = 
    trace_info({erlang,atom_to_list,1}, match_spec),

    expect(),
    {save,me} = X,
    ok.

%% Test the on_load argument for trace_pattern/3.
on_load(Config) when is_list(Config) ->
    0 = erlang:trace_pattern(on_load, []),
    {traced,global} = erlang:trace_info(on_load, traced),
    {match_spec,[]} = erlang:trace_info(on_load, match_spec),

    0 = erlang:trace_pattern(on_load, true, [local]),
    {traced,local} = erlang:trace_info(on_load, traced),
    {match_spec,[]} = erlang:trace_info(on_load, match_spec),

    0 = erlang:trace_pattern(on_load, false, [local]),
    {traced,false} = erlang:trace_info(on_load, traced),
    {match_spec,false} = erlang:trace_info(on_load, match_spec),

    Pam1 = [{[],[],[{message,false}]}],
    0 = erlang:trace_pattern(on_load, Pam1),
    {traced,global} = erlang:trace_info(on_load, traced),
    {match_spec,Pam1} = erlang:trace_info(on_load, match_spec),

    0 = erlang:trace_pattern(on_load, true, [local]),
    0 = erlang:trace_pattern(on_load, false, [local]),

    ok.



%% Test the new exception trace.
deep_exception(Config) when is_list(Config) ->
    deep_exception().

deep_exception() ->
    start_tracer(),
    Self = self(),
    N = 200000,
    LongImproperList = seq(1, N-1, N),

    Prog = [{'_',[],[{exception_trace}]}],
    %%    1 = trace_pid(Self, true, [call]),
    1 = trace_func({?MODULE,deep,'_'}, Prog),
    1 = trace_func({?MODULE,deep_1,'_'}, Prog),
    1 = trace_func({?MODULE,deep_2,'_'}, Prog),
    1 = trace_func({?MODULE,deep_3,'_'}, Prog),
    1 = trace_func({?MODULE,deep_4,'_'}, Prog),
    1 = trace_func({?MODULE,deep_5,'_'}, Prog),
    1 = trace_func({?MODULE,id,'_'}, Prog),
    1 = trace_func({erlang,'++','_'}, Prog),
    1 = trace_func({erlang,exit,1}, Prog),
    1 = trace_func({erlang,throw,1}, Prog),
    2 = trace_func({erlang,error,'_'}, Prog),
    1 = trace_func({lists,reverse,2}, Prog),

    deep_exception(?LINE, exit, [paprika], 1, 
                   [{trace,Self,call,{erlang,exit,[paprika]}},
                    {trace,Self,exception_from,{erlang,exit,1},
                     {exit,paprika}}], 
                   exception_from, {exit,paprika}),
    deep_exception(?LINE, throw, [3.14], 2, 
                   [{trace,Self,call,{erlang,throw,[3.14]}},
                    {trace,Self,exception_from,{erlang,throw,1},
                     {throw,3.14}}], 
                   exception_from, {throw,3.14}),
    deep_exception(?LINE, error, [{paprika}], 3, 
                   [{trace,Self,call,{erlang,error,[{paprika}]}},
                    {trace,Self,exception_from,{erlang,error,1},
                     {error,{paprika}}}], 
                   exception_from, {error,{paprika}}),
    deep_exception(?LINE, error, ["{paprika}",[]], 3, 
                   [{trace,Self,call,{erlang,error,["{paprika}",[]]}},
                    {trace,Self,exception_from,{erlang,error,2},
                     {error,"{paprika}"}}], 
                   exception_from, {error,"{paprika}"}),
    deep_exception(?LINE, id, [broccoli], 4, [], 
                   return_from, broccoli),
    deep_exception(
      ?LINE, append, [1,2], 5,
      [{trace,Self,call,{erlang,'++',[1,2]}},
       {trace,Self,exception_from,{erlang,'++',2},{error,badarg}}],
      exception_from, {error,badarg}),
    deep_exception(?LINE, '=', [1,2], 6, [],
                   exception_from, {error,{badmatch,2}}),
    %%
    io:format("== Subtest: ~w", [?LINE]),
    try lists:reverse(LongImproperList, []) of
        R1 -> ct:fail({returned,abbr(R1)})
    catch error:badarg -> ok
    end,
    expect(fun ({trace,S,call,{lists,reverse,[L1,L2]}})
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
    deep_exception(?LINE, deep_5, [1,2], 7, 
                   [{trace,Self,call,{erlang,error,[undef]}},
                    {trace,Self,exception_from,{erlang,error,1},
                     {error,undef}}],
                   exception_from, {error,undef}),
    deep_exception(?LINE, deep_5, [undef], 8, 
                   [{trace,Self,call,{?MODULE,deep_5,[undef]}},
                    {trace,Self,exception_from,{?MODULE,deep_5,1},
                     {error,function_clause}}],
                   exception_from, {error,function_clause}),

    %% Apply
    %%
    deep_exception(?LINE, apply, [erlang,error,[[mo|rot]]], 1, 
                   [{trace,Self,call,{erlang,error,[[mo|rot]]}},
                    {trace,Self,exception_from,{erlang,error,1},
                     {error,[mo|rot]}}],
                   exception_from, {error,[mo|rot]}),
    deep_exception(?LINE, apply, [erlang,error,[[mo|"rot"],[]]], 1, 
                   [{trace,Self,call,{erlang,error,[[mo|"rot"],[]]}},
                    {trace,Self,exception_from,{erlang,error,2},
                     {error,[mo|"rot"]}}],
                   exception_from, {error,[mo|"rot"]}),
    Morot = make_ref(),
    deep_exception(?LINE, apply, [erlang,throw,[Morot]], 3, 
                   [{trace,Self,call,{erlang,throw,[Morot]}},
                    {trace,Self,exception_from,{erlang,throw,1},
                     {throw,Morot}}],
                   exception_from, {throw,Morot}),
    deep_exception(?LINE, apply, [erlang,exit,[["morot"|Morot]]], 2, 
                   [{trace,Self,call,{erlang,exit,[["morot"|Morot]]}},
                    {trace,Self,exception_from,{erlang,exit,1},
                     {exit,["morot"|Morot]}}],
                   exception_from, {exit,["morot"|Morot]}),
    deep_exception(
      ?LINE, apply, [?MODULE,id,[spenat]], 4,
      [{trace,Self,call,{?MODULE,id,[spenat]}},
       {trace,Self,return_from,{?MODULE,id,1},spenat}],
      return_from, spenat),
    deep_exception(
      ?LINE, apply, [erlang,'++',[1,2]], 5,
      [{trace,Self,call,{erlang,'++',[1,2]}},
       {trace,Self,exception_from,{erlang,'++',2},{error,badarg}}],
      exception_from, {error,badarg}),
    io:format("== Subtest: ~w", [?LINE]),
    try apply(lists, reverse, [LongImproperList, []]) of
        R2 -> ct:fail({returned,abbr(R2)})
    catch error:badarg -> ok
    end,
    expect(fun ({trace,S,call,{lists,reverse,[L1,L2]}})
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
    deep_exception(?LINE, apply, [?MODULE,deep_5,[1,2]], 7, 
                   [{trace,Self,call,{erlang,error,[undef]}},
                    {trace,Self,exception_from,{erlang,error,1},
                     {error,undef}}],
                   exception_from, {error,undef}),
    deep_exception(?LINE, apply, [?MODULE,deep_5,[undef]], 8, 
                   [{trace,Self,call,{?MODULE,deep_5,[undef]}},
                    {trace,Self,exception_from,{?MODULE,deep_5,1},
                     {error,function_clause}}],
                   exception_from, {error,function_clause}),
    %% Apply of fun
    %%
    deep_exception(?LINE, apply, 
                   [fun () -> 
                            erlang:error([{"palsternacka",3.14},17]) 
                    end, []], 1, 
                   [{trace,Self,call,
                     {erlang,error,[[{"palsternacka",3.14},17]]}},
                    {trace,Self,exception_from,{erlang,error,1},
                     {error,[{"palsternacka",3.14},17]}}],
                   exception_from, {error,[{"palsternacka",3.14},17]}),
    deep_exception(?LINE, apply, 
                   [fun () -> 
                            erlang:error(["palsternacka",17], []) 
                    end, []], 1, 
                   [{trace,Self,call,
                     {erlang,error,[["palsternacka",17],[]]}},
                    {trace,Self,exception_from,{erlang,error,2},
                     {error,["palsternacka",17]}}],
                   exception_from, {error,["palsternacka",17]}),
    deep_exception(?LINE, apply, 
                   [fun () -> erlang:throw(Self) end, []], 2, 
                   [{trace,Self,call,{erlang,throw,[Self]}},
                    {trace,Self,exception_from,{erlang,throw,1},
                     {throw,Self}}],
                   exception_from, {throw,Self}),
    deep_exception(?LINE, apply, 
                   [fun () -> 
                            erlang:exit({1,2,3,4,[5,palsternacka]})
                    end, []], 3,
                   [{trace,Self,call,
                     {erlang,exit,[{1,2,3,4,[5,palsternacka]}]}},
                    {trace,Self,exception_from,{erlang,exit,1},
                     {exit,{1,2,3,4,[5,palsternacka]}}}],
                   exception_from, {exit,{1,2,3,4,[5,palsternacka]}}),
    deep_exception(?LINE, apply, 
                   [fun () -> ?MODULE:id(bladsallad) end, []], 4,
                   [{trace,Self,call,{?MODULE,id,[bladsallad]}},
                    {trace,Self,return_from,{?MODULE,id,1},bladsallad}],
                   return_from, bladsallad),
    deep_exception(?LINE, apply, 
                   [fun (A, B) -> A ++ B end, [1,2]], 5,
                   [{trace,Self,call,{erlang,'++',[1,2]}},
                    {trace,Self,exception_from,
                     {erlang,'++',2},{error,badarg}}],
                   exception_from, {error,badarg}),
    deep_exception(?LINE, apply, [fun (A, B) -> A = B end, [1,2]], 6, 
                   [],
                   exception_from, {error,{badmatch,2}}),
    io:format("== Subtest: ~w", [?LINE]),
    try apply(fun() -> lists:reverse(LongImproperList, []) end, []) of
        R3 -> ct:fail({returned,abbr(R3)})
    catch error:badarg -> ok
    end,
    expect(fun ({trace,S,call,{lists,reverse,[L1,L2]}})
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
    deep_exception(?LINE, apply, 
                   [fun () -> ?MODULE:deep_5(1,2) end, []], 7, 
                   [{trace,Self,call,{erlang,error,[undef]}},
                    {trace,Self,exception_from,{erlang,error,1},
                     {error,undef}}],
                   exception_from, {error,undef}),
    deep_exception(?LINE, apply, 
                   [fun () -> ?MODULE:deep_5(undef) end, []], 8, 
                   [{trace,Self,call,{?MODULE,deep_5,[undef]}},
                    {trace,Self,exception_from,{?MODULE,deep_5,1},
                     {error,function_clause}}],
                   exception_from, {error,function_clause}),

    trace_func({?MODULE,'_','_'}, false),
    trace_func({erlang,'_','_'}, false),
    trace_func({lists,'_','_'}, false),
    expect(),
    ok.


deep_exception(Line, B, Q, N, Extra, Tag, R) ->
    Self = self(),
    io:format("== Subtest: ~w", [Line]),
    Result = ?MODULE:deep(N, B, Q),
    Result = deep_expect(Self, B, Q, N, Extra, Tag, R).

deep_expect(Self, B, Q, N, Extra, Tag, R) ->
    expect({trace,Self,call,{?MODULE,deep,[N,B,Q]}}),
    Result = deep_expect_N(Self, B, Q, N, Extra, Tag, R),
    expect({trace,Self,return_from,{?MODULE,deep,3},Result}),
    Result.

deep_expect_N(Self, B, Q, N, Extra, Tag, R) ->
    deep_expect_N(Self, B, Q, N, Extra, Tag, R, N).

deep_expect_N(Self, B, Q, N, Extra, Tag, R, J) when J > 0 ->
    expect({trace,Self,call,{?MODULE,deep_1,[J,B,Q]}}),
    deep_expect_N(Self, B, Q, N, Extra, Tag, R, J-1);
deep_expect_N(Self, B, Q, N, Extra, Tag, R, 0) ->
    expect({trace,Self,call,{?MODULE,deep_2,[B,Q]}}),
    expect({trace,Self,call,{?MODULE,deep_3,[B,Q]}}),
    expect({trace,Self,return_from,{?MODULE,deep_3,2},{B,Q}}),
    expect({trace,Self,call,{?MODULE,deep_4,[{B,Q}]}}),
    expect({trace,Self,call,{?MODULE,id,[{B,Q}]}}),
    expect({trace,Self,return_from,{?MODULE,id,1},{B,Q}}),
    deep_expect_Extra(Self, N, Extra, Tag, R),
    expect({trace,Self,Tag,{?MODULE,deep_4,1},R}),
    expect({trace,Self,Tag,{?MODULE,deep_2,2},R}),
    deep_expect_N(Self, N, Tag, R).

deep_expect_Extra(Self, N, [E|Es], Tag, R) ->
    expect(E),
    deep_expect_Extra(Self, N, Es, Tag, R);
deep_expect_Extra(_Self, _N, [], _Tag, _R) ->
    ok.

deep_expect_N(Self, N, Tag, R) when N > 0 ->
    expect({trace,Self,Tag,{?MODULE,deep_1,3},R}),
    deep_expect_N(Self, N-1, Tag, R);
deep_expect_N(_Self, 0, return_from, R) ->
    {value,R};
deep_expect_N(_Self, 0, exception_from, R) ->
    R.



%% Test the new exception trace.
exception_nocatch(Config) when is_list(Config) ->
    exception_nocatch().

exception_nocatch() ->
    Deep4LocThrow = get_deep_4_loc({throw,[42]}),
    Deep4LocError = get_deep_4_loc({error,[42]}),
    Deep4LocBadmatch = get_deep_4_loc({'=',[a,b]}),

    Prog = [{'_',[],[{exception_trace}]}],
    1 = erlang:trace_pattern({?MODULE,deep_1,'_'}, Prog),
    1 = erlang:trace_pattern({?MODULE,deep_2,'_'}, Prog),
    1 = erlang:trace_pattern({?MODULE,deep_3,'_'}, Prog),
    1 = erlang:trace_pattern({?MODULE,deep_4,'_'}, Prog),
    1 = erlang:trace_pattern({?MODULE,deep_5,'_'}, Prog),
    1 = erlang:trace_pattern({?MODULE,id,'_'}, Prog),
    1 = erlang:trace_pattern({erlang,exit,1}, Prog),
    1 = erlang:trace_pattern({erlang,throw,1}, Prog),
    2 = erlang:trace_pattern({erlang,error,'_'}, Prog),
    Q1 = {make_ref(),Prog},
    T1 = 
    exception_nocatch(?LINE, exit, [Q1], 3, 
                      [{trace,t1,call,{erlang,exit,[Q1]}},
                       {trace,t1,exception_from,{erlang,exit,1},
                        {exit,Q1}}],
                      exception_from, {exit,Q1}),
    expect({trace,T1,exit,Q1}),
    Q2 = {cake,14.125},
    T2 = 
    exception_nocatch(?LINE, throw, [Q2], 2, 
                      [{trace,t2,call,{erlang,throw,[Q2]}},
                       {trace,t2,exception_from,{erlang,throw,1},
                        {error,{nocatch,Q2}}}],
                      exception_from, {error,{nocatch,Q2}}),
    expect({trace,T2,exit,{{nocatch,Q2},[{?MODULE,deep_4,1,
                                          Deep4LocThrow}]}}),
    Q3 = {dump,[dump,{dump}]},
    T3 = 
    exception_nocatch(?LINE, error, [Q3], 4, 
                      [{trace,t3,call,{erlang,error,[Q3]}},
                       {trace,t3,exception_from,{erlang,error,1},
                        {error,Q3}}],
                      exception_from, {error,Q3}),
    expect({trace,T3,exit,{Q3,[{?MODULE,deep_4,1,Deep4LocError}]}}),
    T4 = 
    exception_nocatch(?LINE, '=', [17,4711], 5, [], 
                      exception_from, {error,{badmatch,4711}}),
    expect({trace,T4,exit,{{badmatch,4711},
                           [{?MODULE,deep_4,1,Deep4LocBadmatch}]}}),
    %%
    erlang:trace_pattern({?MODULE,'_','_'}, false),
    erlang:trace_pattern({erlang,'_','_'}, false),
    expect(),
    ok.

get_deep_4_loc(Arg) ->
    try
        deep_4(Arg),
        ct:fail(should_not_return_to_here)
    catch
        _:_:Stk ->
            [{?MODULE,deep_4,1,Loc0}|_] = Stk,
            Loc0
    end.

exception_nocatch(Line, B, Q, N, Extra, Tag, R) ->
    io:format("== Subtest: ~w", [Line]),
    Go = make_ref(),
    Tracee = 
    spawn(fun () ->
                  receive
                      Go ->
                          deep_1(N, B, Q)
                  end
          end),
    1 = erlang:trace(Tracee, true, [call,return_to,procs]),
    Tracee ! Go,
    deep_expect_N(Tracee, B, Q, N-1, 
                  [setelement(2, T, Tracee) || T <- Extra], Tag, R),
    Tracee.

%% Make sure that code that uses the optimized bit syntax matching
%% can be traced without crashing the emulator. (Actually, it seems
%% that we can't trigger the bug using external call trace, but we
%% will keep the test case anyway.)

bit_syntax(Config) when is_list(Config) ->
    start_tracer(),
    1 = trace_func({?MODULE,bs_sum_a,'_'}, []),
    1 = trace_func({?MODULE,bs_sum_b,'_'}, []),

    6 = call_bs_sum_a(<<1,2,3>>),
    10 = call_bs_sum_b(<<1,2,3,4>>),

    trace_func({?MODULE,'_','_'}, false),
    erlang:trace_delivered(all),
    receive
        {trace_delivered,_,_} -> ok
    end,

    Self = self(),
    expect({trace,Self,call,{?MODULE,bs_sum_a,[<<2,3>>,1]}}),
    expect({trace,Self,call,{?MODULE,bs_sum_b,[1,<<2,3,4>>]}}),

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
            ct:fail({unexpected,abbr(Msgs)})
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
                    ct:fail({unexpected,abbr([M|flush()])})
            end
    after 5000 ->
              io:format("Expected ~p; got nothing", [abbr(Message)]),
              ct:fail(no_trace_message)
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
                    ct:fail({unexpected,abbr([M|flush()])})
            end
    after 5000 ->
              io:format("Expected ~p; got nothing", [abbr(Message)]),
              ct:fail(no_trace_message)
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
                    ct:fail({unexpected,abbr([M|flush()])})
            end
    after 5000 ->
              io:format("Expected ~p; got nothing", [abbr(Validator('_'))]),
              ct:fail(no_trace_message)
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
                    ct:fail({unexpected,abbr([Other|flush()])})
            end
    after 5000 ->
              io:format("Expected ~p; got nothing", [abbr(Message)]),
              ct:fail(no_trace_message)
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

%% lists:reverse cannot be called since it is traced
reverse(L) ->
    reverse(L, []).
%%
reverse([], R) -> R;
reverse([H|T], R) ->
    reverse(T, [H|R]).
