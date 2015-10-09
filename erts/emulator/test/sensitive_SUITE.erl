%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2011. All Rights Reserved.
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

-module(sensitive_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 stickiness/1,send_trace/1,recv_trace/1,proc_trace/1,call_trace/1,
	 meta_trace/1,running_trace/1,gc_trace/1,seq_trace/1,
	 t_process_info/1,t_process_display/1,save_calls/1]).

-export([remote_process_display/0,an_exported_function/1]).

-import(lists, [keysearch/3,foreach/2,sort/1]).

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(5)),
    [{watchdog,Dog}|Config].

end_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [stickiness, send_trace, recv_trace, proc_trace,
     call_trace, meta_trace, running_trace, gc_trace,
     seq_trace, t_process_info, t_process_display,
     save_calls].

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

    
stickiness(Config) when is_list(Config) ->
    ?line {Tracer,Mref} = spawn_monitor(fun() ->
						receive after infinity -> ok end
					end),
    ?line false = process_flag(sensitive, true),
    put(foo, bar),

    Flags = sort([send,'receive',procs,call,running,garbage_collection,
		  set_on_spawn,set_on_first_spawn,set_on_link,set_on_first_link]),
    ?line foreach(fun(F) ->
			  1 = erlang:trace(self(), true, [F,{tracer,Tracer}])
		  end, Flags),
    ?line foreach(fun(F) ->
			  1 = erlang:trace(self(), false, [F,{tracer,Tracer}])
		  end, Flags),
    ?line 1 = erlang:trace(self(), true, [{tracer,Tracer}|Flags]),
    ?line 1 = erlang:trace(self(), false, [{tracer,Tracer}|Flags]),

    ?line {messages,[]} = process_info(Tracer, messages),
    exit(Tracer, kill),
    receive {'DOWN',Mref,_,_,_} -> ok end,
    
    case process_info(self(), dictionary) of
	{dictionary,[]} -> ok;
	{dictionary,_} -> ?line ?t:fail(sensitive_flag_cleared)
    end,

    NewTracer = spawn_link(fun() -> receive after infinity -> ok end end),
    ?line 1 = erlang:trace(self(), true, [{tracer,NewTracer}|Flags]),
    ?line Flags = sort(element(2, erlang:trace_info(self(), flags))),
    ?line {tracer,NewTracer} = erlang:trace_info(self(), tracer),

    %% Process still sensitive. Tracer should disappear when we clear
    %% all trace flags.
    ?line 1 = erlang:trace(self(), false, [{tracer,NewTracer}|Flags]),
    ?line {tracer,[]} = erlang:trace_info(self(), tracer),

    ?line unlink(NewTracer), exit(NewTracer, kill),
    ok.

send_trace(Config) when is_list(Config) ->
    ?line {Dead,Mref} = spawn_monitor(fun() -> ok end),
    receive {'DOWN',Mref,_,_,_} -> ok end,
    ?line Tracer = spawn_link(fun() -> receive after infinity -> ok end end),
    ?line Sink = spawn_link(fun() -> receive after infinity -> ok end end),
    Self = self(),

    ?line 1 = erlang:trace(self(), true, [send,{tracer,Tracer}]),
    ?line Dead ! before,
    ?line Sink ! before,
    ?line false = process_flag(sensitive, true),
    ?line Sink ! {blurf,lists:seq(1, 50)},
    ?line true = process_flag(sensitive, true),
    ?line Sink ! lists:seq(1, 100),
    ?line Dead ! forget_me,
    ?line true = process_flag(sensitive, false),
    ?line Sink ! after1,
    ?line false = process_flag(sensitive, false),
    ?line Sink ! after2,
    ?line Dead ! after2,
    ?line wait_trace(Self),

    ?line {messages,Messages} = process_info(Tracer, messages),
    ?line [{trace,Self,send_to_non_existing_process,before,Dead},
	   {trace,Self,send,before,Sink},
	   {trace,Self,send,after1,Sink},
	   {trace,Self,send,after2,Sink},
	   {trace,Self,send_to_non_existing_process,after2,Dead}] = Messages,

    ?line unlink(Tracer), exit(Tracer, kill),
    ?line unlink(Sink), exit(Sink, kill),
    ok.

recv_trace(Config) when is_list(Config) ->
    Parent = self(),
    ?line Tracer = spawn_link(fun() -> receive after infinity -> ok end end),
    ?line Sender = spawn_link(fun() -> recv_trace_sender(Parent) end),

    ?line 1 = erlang:trace(self(), true, ['receive',{tracer,Tracer}]),

    Sender ! 1,
    receive a -> wait_trace(Sender) end,

    ?line false = process_flag(sensitive, true),

    Sender ! 2,
    receive {b,[x,y,z]} -> wait_trace(Sender) end,
    
    ?line true = process_flag(sensitive, false),

    Sender ! 3,
    receive c -> wait_trace(Sender) end,
    
    ?line {messages,Messages} = process_info(Tracer, messages),
    [{trace,Parent,'receive',a},
     {trace,Parent,'receive',{trace_delivered,_,_}},
     {trace,Parent,'receive',c}|_] = Messages,

    ?line unlink(Tracer), exit(Tracer, kill),
    ?line unlink(Sender), exit(Sender, kill),
    ok.

recv_trace_sender(Pid) ->
    receive
	1 -> Pid ! a;
	2 -> Pid ! {b,[x,y,z]};
	3 -> Pid ! c
    end,
    recv_trace_sender(Pid).

proc_trace(Config) when is_list(Config) ->
    Self = self(),
    ?line Tracer = spawn_link(fun() -> receive after infinity -> ok end end),

    ?line 1 = erlang:trace(self(), true, [procs,{tracer,Tracer}]),
    ?line false = process_flag(sensitive, true),

    spawn(fun() -> ok end),
    ?line register(nisse, self()),
    ?line unregister(nisse),
    ?line link(Tracer),
    ?line unlink(Tracer),
    ?line Linker0 = spawn_link(fun() -> ok end),
    Mref0 = erlang:monitor(process, Linker0),

    ?line {_,Mref} = spawn_monitor(fun() -> link(Self), unlink(Self) end),

    receive {'DOWN',Mref0,_,_,_} -> ok end,

    receive {'DOWN',Mref,_,_,_} -> ok end,

    ?line true = process_flag(sensitive, false),

    Dead = spawn(fun() -> ok end),
    ?line register(arne, self()),
    ?line unregister(arne),
    ?line {Linker,Mref2} = spawn_monitor(fun() -> link(Self), unlink(Self) end),
    receive {'DOWN',Mref2,_,_,_} -> ok end,
    ?line Last = spawn_link(fun() -> ok end),
    receive after 10 -> ok end,
    ?line wait_trace(all),
    ?line {messages,Messages} = process_info(Tracer, messages),    
    [{trace,Self,spawn,Dead,{erlang,apply,_}},
     {trace,Self,register,arne},
     {trace,Self,unregister,arne},
     {trace,Self,spawn,Linker,_},
     {trace,Self,getting_linked,Linker},
     {trace,Self,getting_unlinked,Linker},
     {trace,Self,spawn,Last,_},
     {trace,Self,link,Last},
     {trace,Self,getting_unlinked,Last}] = Messages,

    ?line unlink(Tracer), exit(Tracer, kill),
    ok.

call_trace(Config) when is_list(Config) ->
    Self = self(),
    ?line Tracer = spawn_link(fun() -> receive after infinity -> ok end end),

    ?line 1 = erlang:trace(self(), true, [call,{tracer,Tracer}]),
    ?line 1 = erlang:trace_pattern({?MODULE,an_exported_function,1},
				   true, [global]),
    ?line 1 = erlang:trace_pattern({erlang,list_to_binary,1}, true, [global]),
    ?line 1 = erlang:trace_pattern({erlang,binary_to_list,1}, true, [local]),
    ?line Local = erlang:trace_pattern({?MODULE,'_','_'}, true, [local]),

    ?line false = process_flag(sensitive, true),
    ?line {ok,42} = a_local_function(42),
    ?line 7 = an_exported_function(6),
    ?line <<7,8,9,10>> = list_to_binary(id([7,8,9,10])),
    ?line [42,43] = binary_to_list(id(<<42,43>>)),
    ?line true = process_flag(sensitive, false),

    ?line {ok,{a,b}} = a_local_function({a,b}),
    ?line 1 = an_exported_function(0),
    ?line <<1,2,3>> = list_to_binary(id([1,2,3])),
    ?line [42,43,44] = binary_to_list(id(<<42,43,44>>)),

    ?line wait_trace(Self),

    ?line {messages,Messages} = process_info(Tracer, messages),    
    ?line [{trace,Self,call,{?MODULE,a_local_function,[{a,b}]}},
	   {trace,Self,call,{?MODULE,an_exported_function,[0]}},
	   {trace,Self,call,{?MODULE,id,[_]}},
	   {trace,Self,call,{erlang,list_to_binary,[[1,2,3]]}},
	   {trace,Self,call,{sensitive_SUITE,id,[<<42,43,44>>]}},
	   {trace,Self,call,{erlang,binary_to_list,[<<42,43,44>>]}},
	   {trace,Self,call,{?MODULE,wait_trace,[Self]}}] = Messages,

    ?line Local = erlang:trace_pattern({?MODULE,'_','_'}, false, [local]),
    ?line erlang:trace_pattern({erlang,'_','_'}, false, [local]),
    ?line erlang:trace_pattern({'_','_','_'}, false, [global]),

    ?line unlink(Tracer), exit(Tracer, kill),
    ok.

meta_trace(Config) when is_list(Config) ->
    Self = self(),
    ?line Tracer = spawn_link(fun() -> receive after infinity -> ok end end),

    ?line Local = erlang:trace_pattern({?MODULE,'_','_'}, true, [{meta,Tracer}]),
    ?line 1 = erlang:trace_pattern({erlang,list_to_binary,1}, true, [{meta,Tracer}]),

    ?line false = process_flag(sensitive, true),
    ?line {ok,blurf} = a_local_function(blurf),
    ?line 100 = an_exported_function(99),
    ?line <<8,9,10>> = list_to_binary(id([8,9,10])),
    ?line true = process_flag(sensitive, false),

    ?line {ok,{x,y}} = a_local_function({x,y}),
    ?line 1 = an_exported_function(0),
    ?line <<10>> = list_to_binary(id([10])),
    ?line wait_trace(Self),

    ?line Local = erlang:trace_pattern({?MODULE,'_','_'}, false, [meta]),
    ?line 1 = erlang:trace_pattern({erlang,list_to_binary,1}, false, [meta]),
    ?line a_local_function(0),

    ?line {messages,Messages} = process_info(Tracer, messages),    
    ?line [{trace_ts,Self,call,{?MODULE,a_local_function,[{x,y}]},{_,_,_}},
	   {trace_ts,Self,call,{?MODULE,an_exported_function,[0]},{_,_,_}},
	   {trace_ts,Self,call,{?MODULE,id,[_]},{_,_,_}},
	   {trace_ts,Self,call,{erlang,list_to_binary,[[10]]},{_,_,_}},
	   {trace_ts,Self,call,{?MODULE,wait_trace,[Self]},{_,_,_}}] = Messages,

    ?line unlink(Tracer), exit(Tracer, kill),
    ok.

a_local_function(A) ->
    {ok,A}.

an_exported_function(X) ->
    X+1.

running_trace(Config) when is_list(Config) ->
    Self = self(),
    ?line Tracer = spawn_link(fun() -> receive after infinity -> ok end end),

    ?line false = process_flag(sensitive, true),
    ?line 1 = erlang:trace(Self, true, [running,{tracer,Tracer}]),
    erlang:yield(), erlang:yield(), erlang:yield(), erlang:yield(),
    erlang:yield(), erlang:yield(), erlang:yield(), erlang:yield(),
    ?line true = process_flag(sensitive, false),
    erlang:yield(),
    ?line 1 = erlang:trace(Self, false, [running,{tracer,Tracer}]),

    ?line wait_trace(Self),
    ?line {messages,Messages} = process_info(Tracer, messages),    
    ?line [{trace,Self,out,{sensitive_SUITE,running_trace,1}},
	   {trace,Self,in,{sensitive_SUITE,running_trace,1}}] = Messages,

    ?line unlink(Tracer), exit(Tracer, kill),
    ok.

gc_trace(Config) when is_list(Config) ->
    Self = self(),
    ?line Tracer = spawn_link(fun() -> receive after infinity -> ok end end),

    ?line false = process_flag(sensitive, true),
    ?line 1 = erlang:trace(Self, true, [garbage_collection,{tracer,Tracer}]),
    erlang:garbage_collect(), erlang:garbage_collect(),
    erlang:garbage_collect(), erlang:garbage_collect(),
    erlang:garbage_collect(), erlang:garbage_collect(),
    erlang:garbage_collect(), erlang:garbage_collect(),
    ?line true = process_flag(sensitive, false),
    erlang:garbage_collect(),
    ?line 1 = erlang:trace(Self, false, [garbage_collection,{tracer,Tracer}]),

    ?line wait_trace(Self),
    ?line {messages,Messages} = process_info(Tracer, messages),    
    ?line [{trace,Self,gc_start,_},{trace,Self,gc_end,_}] = Messages,

    ?line unlink(Tracer), exit(Tracer, kill),
    ok.

seq_trace(Config) when is_list(Config) ->
    Self = self(),
    ?line Tracer = spawn_link(fun() -> receive after infinity -> ok end end),
    ?line seq_trace:set_system_tracer(Tracer),

    ?line false = process_flag(sensitive, true),

    ?line Echo = spawn_link(fun() ->
				    receive
					{Pid,Message} ->
					    Pid ! {reply,Message}
				    end
			    end),
    ?line Sender = spawn_link(fun() ->
				      seq_trace:set_token(label, 42),
				      seq_trace:set_token('receive', true),
				      seq_trace:set_token(send, true),
				      seq_trace:set_token(print, true),
				      seq_trace:print(42, "trace started"),
				      Self ! blurf
			    end),
    seq_trace:set_token(label, 17),
    seq_trace:set_token('receive', true),
    seq_trace:set_token(send, true),
    seq_trace:set_token(print, true),
    seq_trace:print(17, "trace started"),
    Echo ! {Self,hello},
    receive {reply,hello} -> ok end,
    receive blurf -> ok end,

    ?line wait_trace(all),

    ?line {messages,Messages} = process_info(Tracer, messages),
    ?line [{seq_trace,17,{'receive',{0,2},Self,Echo,{Self,hello}}},
	   {seq_trace,17,{send,{2,3},Echo,Self,{reply,hello}}}] =
	[M || {seq_trace,17,_}=M <- Messages],

    ?line [{seq_trace,42,{print,{0,1},Sender,[],"trace started"}},
	   {seq_trace,42,{send,{0,2},Sender,Self,blurf}}] =
	[M || {seq_trace,42,_}=M <- Messages],
    
    ?line unlink(Tracer), exit(Tracer, kill),
    ?line unlink(Echo), exit(Echo, kill),
    ?line unlink(Sender), exit(Sender, kill),
    ok.

t_process_info(Config) when is_list(Config) ->
    Parent = self(),
    ?line Pid = spawn_link(fun() ->
				   put(foo, bar),
				   false = process_flag(sensitive, true),
				   Parent ! go,
				   receive
				       revert ->
					   true = process_flag(sensitive, false),
					   Parent ! go_again,
					   receive never -> ok end
				   end end),
    receive go -> ok end,

    ?line put(foo, bar),
    ?line self() ! Pid ! {i,am,a,message},

    ?line false = process_flag(sensitive, true),
    ?line t_process_info_suppressed(self()),
    ?line t_process_info_suppressed(Pid),

    ?line true = process_flag(sensitive, false),
    Pid ! revert,
    receive go_again -> ok end,

    ?line t_process_info_normal(self()),
    ?line t_process_info_normal(Pid),
    ok.

t_process_info_suppressed(Pid) ->
    [] = my_process_info(Pid, dictionary),
    <<>> = my_process_info(Pid, backtrace),
    [] = my_process_info(Pid, messages).

t_process_info_normal(Pid) ->
    {value,{foo,bar}} = keysearch(foo, 1, my_process_info(Pid, dictionary)),
    case process_info(Pid, backtrace) of
	{backtrace,Bin} when size(Bin) > 20 -> ok
    end,
    [{i,am,a,message}] = my_process_info(Pid, messages).

my_process_info(Pid, Tag) ->
    {Tag,Value} = process_info(Pid, Tag),
    All = process_info(Pid),
    case keysearch(Tag, 1, All) of
	false -> Value;
	{value,{Tag,Value}} -> Value
    end.

t_process_display(Config) when is_list(Config) ->
    ?line Dir = filename:dirname(code:which(?MODULE)),
    ?line Cmd = atom_to_list(lib:progname()) ++ " -noinput -pa " ++ Dir ++
	" -run " ++ ?MODULE_STRING ++ " remote_process_display",
    ?line io:put_chars(Cmd),
    ?line P = open_port({spawn,Cmd}, [in,stderr_to_stdout,eof]),
    <<"done",_/binary>> = get_all(P),
    ok.

remote_process_display() ->
    false = process_flag(sensitive, true),
    erlang:process_display(self(), backtrace),
    erlang:display(done),
    receive after 10 -> ok end,
    init:stop().

get_all(P) ->
    get_all(P, []).

get_all(P, Acc) ->
    receive
	{P,{data,S}} ->
	    get_all(P, [Acc|S]);
	{P,eof} ->
	    iolist_to_binary(Acc)
    end.

save_calls(Config) when is_list(Config) ->
    process_flag(save_calls, 10),

    false = process_flag(sensitive, true),
    ?line {last_calls,LastCalls} = process_info(self(), last_calls),
    ?line [{erlang,process_flag,2}] = LastCalls,
    ?line [2,4,6] = lists:map(fun(E) -> 2*E end, [1,2,3]),
    ?line {last_calls,LastCalls} = process_info(self(), last_calls),
    ok.

wait_trace(Pid) ->
    Ref = erlang:trace_delivered(Pid),
    receive
	{trace_delivered,Pid,Ref} -> ok
    end.
    
id(I) -> I.
    
