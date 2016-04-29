%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
-module(dyntrace_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([smoke/1,process/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() ->
    case erlang:system_info(dynamic_trace) of
	none ->
	    {skip,"No dynamic trace in this run-time system"};
	dtrace ->
	    [{group,smoke}];
	systemtap ->
	    {skip,"SystemTap tests currently not supported"};
	lttng ->
	    {skip,"LTTng tests currently not supported"}
    end.

groups() ->
    [{smoke,[sequence],[smoke,{group,rest}]},
     {rest,[],
      [process]}].

init_per_suite(Config) ->
    N = "beam" ++
	case erlang:system_info(debug_compiled) of
	    false -> "";
	    true -> ".debug"
	end ++
	case erlang:system_info(smp_support) of
	    false -> "";
	    true -> ".smp"
	end,
    [{emu_name,N}|Config].

end_per_suite(_Config) ->
    ok.

smoke(Config) ->
    Emu = test_server:lookup_config(emu_name, Config),
    BinEmu = list_to_binary(Emu),
    case erlang:system_info(dynamic_trace) of
	dtrace ->
	    Probes = os:cmd("sudo /usr/sbin/dtrace -l -m" ++ Emu),
	    io:put_chars(Probes),
	    [_|Lines] = re:split(Probes, "\n", [trim]),
	    [{_,_} = binary:match(L, BinEmu) || L <- Lines],
	    ok
    end,

    %% Test that the framework for running dtrace/systemtap works
    %% by executing an empty script.
    {ok,[]} = dyntrace("", fun() -> ok end),
    ok.


process(_Config) ->
    Script = [{probe,"process-spawn"},
	      {action,[{printf,["spawn %s %s\n",{arg,0},{arg,1}]}]},
	      {probe,"process-scheduled"},
	      {action,[{printf,["in %s\n",{arg,0}]}]},
	      {probe,"process-unscheduled"},
	      {action,[{printf,["out %s\n",{arg,0}]}]},
	      {probe,"process-hibernate"},
	      {action,[{printf,["hibernate %s %s\n",{arg,0},{arg,1}]}]},
	      {probe,"process-exit"},
	      {action,[{printf,["exit %s %s\n",{arg,0},{arg,1}]}]}],
    F = fun() ->
		{Pid,Ref} = spawn_monitor(fun my_process/0),
		Pid ! hibernate,
		Pid ! quit,
		receive
		    {'DOWN',Ref,process,Pid,{terminated,Pid}} ->
			ok
		end,
		Pid
	end,
    {Pid,Output0} = dyntrace(Script, F),
    Output1 = [termify_line(L) || L <- Output0],
    PidStr = pid_to_list(Pid),
    Output = [L || L <- Output1, element(2, L) =:= PidStr],
    Reason = "{terminated,"++PidStr++"}",
    io:format("~p\n", [Output]),
    [{spawn,PidStr,"erlang:apply/2"},
     {in,PidStr},
     {hibernate,PidStr,"erlang:apply/2"},
     {out,PidStr},
     {in,PidStr},
     {exit,PidStr,Reason},
     {out,PidStr}] = Output,
    ok.

termify_line(L) ->
    [H|T] = re:split(L, " ", [{return,list}]),
    list_to_tuple([list_to_atom(H)|T]).

my_process() ->
    receive
	hibernate ->
	    erlang:hibernate(erlang, apply, [fun my_process/0,[]]);
	quit ->
	    exit({terminated,self()})
    end.

%%%
%%% Utility functions.
%%%

dyntrace(Script0, Action) ->
    Sudo = os:find_executable(sudo),
    {Termination,Pid} = termination_probe(),
    Script1 = Script0++Termination,
    Script = translate_script(Script1),
    io:format("~s\n", [Script]),
    SrcFile = "test-dyntrace.d",
    ok = file:write_file(SrcFile, Script),
    Args = ["/usr/sbin/dtrace", "-q","-s",SrcFile],
    Port = open_port({spawn_executable,Sudo},
		     [{args,Args},stream,in,stderr_to_stdout,eof]),
    receive
	{Port,{data,Sofar}} ->
	    Res = Action(),
	    Pid ! quit,
	    {Res,get_data(Port, Sofar)}
    end.

get_data(Port, Sofar) ->
    receive
	{Port,{data,Bytes}} ->
	    get_data(Port, [Sofar|Bytes]);
	{Port,eof} ->
	    port_close(Port),
	    [$\n|T] = lists:flatten(Sofar),
	    re:split(T, "\n", [{return,list},trim])
    end.

termination_probe() ->
    Pid = spawn(fun() ->
			receive
			    _ ->
				exit(done)
			end
		end),
    S = [{'BEGIN',[{printf,["\n"]}]},
	 {probe,"process-exit"},
	 {pred,{'==',{arg,0},Pid}},
	 {action,[{exit,[0]}]}],
    {S,Pid}.

translate_script(Script) ->
    [dtrace_op(Op) || Op <- Script].

dtrace_op({probe,Function}) ->
    OsPid = os:getpid(),
    ["erlang",OsPid,":::",Function,$\n];
dtrace_op({pred,Pred}) ->
    ["/",dtrace_op(Pred),"/\n"];
dtrace_op({action,List}) ->
    ["{ ",action_list(List)," }\n\n"];
dtrace_op({'BEGIN',List}) ->
    ["BEGIN { ",action_list(List)," }\n\n"];
dtrace_op({'==',Op1,Op2}) ->
    [dtrace_op(Op1)," == ",dtrace_op(Op2)];
dtrace_op({arg,N}) ->
    ["copyinstr(arg",integer_to_list(N),")"];
dtrace_op({Func,List}) when is_atom(Func), is_list(List) ->
    [atom_to_list(Func),"(",comma_sep_ops(List),")"];
dtrace_op(Pid) when is_pid(Pid) ->
    ["\"",pid_to_list(Pid),"\""];
dtrace_op(Str) when is_integer(hd(Str)) ->
    io_lib:format("~p", [Str]);
dtrace_op(Int) when is_integer(Int) ->
    integer_to_list(Int).

comma_sep_ops([A,B|T]) ->
    [dtrace_op(A),","|comma_sep_ops([B|T])];
comma_sep_ops([H]) ->
    dtrace_op(H);
comma_sep_ops([]) -> [].

action_list([H|T]) ->
    [dtrace_op(H),";"|action_list(T)];
action_list([]) -> [].
