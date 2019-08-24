%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
-module(sys_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,log/1,log_to_file/1,
	 stats/1,trace/1,suspend/1,install/1,special_process/1]).
-export([handle_call/3,terminate/2,init/1]).
-include_lib("common_test/include/ct.hrl").

-define(server,sys_SUITE_server).


%% Doesn't look into change_code at all


suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [log, log_to_file, stats, trace, suspend, install, special_process].

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

log(Config) when is_list(Config) ->
    {ok,_Server} = start(),
    ok = sys:log(?server,true),
    {ok,-44} = public_call(44),
    ok = sys:log(?server,false),
    ok = sys:log(?server,print),
    stop(),
    ok.

log_to_file(Config) when is_list(Config) ->
    TempName = test_server:temp_name(proplists:get_value(priv_dir,Config) ++ "sys."),
    {ok,_Server} = start(),
    ok = sys:log_to_file(?server,TempName),
    {ok,-44} = public_call(44),
    ok = sys:log_to_file(?server,false),
    {ok,Fd} = file:open(TempName,[read]),
    Msg1 = io:get_line(Fd,''),
    Msg2 = io:get_line(Fd,''),
    file:close(Fd),
    "*DBG* sys_SUITE_server got call {req,44} from " ++ _ = Msg1,
    "*DBG* sys_SUITE_server sent {ok,-44} to " ++ _ = Msg2,
    stop(),
    ok.

stats(Config) when is_list(Config) ->
    Self = self(),
    {ok,_Server} = start(),
    ok = sys:statistics(?server,true),
    {ok,-44} = public_call(44),
    {ok,Stats} = sys:statistics(?server,get),
    true = lists:member({messages_in,1}, Stats),
    true = lists:member({messages_out,1}, Stats),
    ok = sys:statistics(?server,false),
    {status,_Pid,{module,_Mod},[_PDict,running,Self,_,_]} =
	sys:get_status(?server),
    {ok,no_statistics} = sys:statistics(?server,get),
    stop(),
    ok.

trace(Config) when is_list(Config) ->
    {ok,_Server} = start(),
    ct:sleep(2000),
    ct:capture_start(),
    sys:trace(?server,true),
    {ok,-44} = public_call(44),
    %% ho, hum, allow for the io to reach us..
    ct:sleep(1000),
    ct:capture_stop(),
    [Msg1,Msg2] = ct:capture_get(),
    "*DBG* sys_SUITE_server got call {req,44} from " ++ _ = Msg1,
    "*DBG* sys_SUITE_server sent {ok,-44} to " ++ _ = Msg2,
    stop(),
    ok.

suspend(Config) when is_list(Config) ->
    {ok,_Server} = start(),
    sys:suspend(?server,1000),
    {'EXIT',_} = (catch public_call(48)),
    {status,_,_,[_,suspended,_,_,_]} = sys:get_status(?server),
    sys:suspend(?server,1000), %% doing it twice is no error
    {'EXIT',_} = (catch public_call(48)),
    sys:resume(?server),
    {status,_,_,[_,running,_,_,_]} = sys:get_status(?server),
    {ok,-48} = (catch public_call(48)),
    sys:resume(?server), %% doing it twice is no error
    {ok,-48} = (catch public_call(48)),
    stop(),
    ok.

install(Config) when is_list(Config) ->
    {ok,_Server} = start(),
    Master = self(),
    SpyFun =
	fun(func_state,Event,ProcState) ->
		case Event of
		    {in,{'$gen_call',_From,{req,Arg}}} ->
			io:format("Trigged\n"),
			Master ! {spy_got,{request,Arg},ProcState};
		    Other ->
			io:format("Trigged other=~p\n",[Other])
		end,
                func_state
	end,
    sys:install(?server,{SpyFun,func_state}),
    {ok,-1} = (catch public_call(1)),
    sys:no_debug(?server),
    {ok,-2} = (catch public_call(2)),
    sys:install(?server,{SpyFun,func_state}),
    sys:install(?server,{SpyFun,func_state}),
    {ok,-3} = (catch public_call(3)),
    {ok,-4} = (catch public_call(4)),
    sys:remove(?server,SpyFun),
    {ok,-5} = (catch public_call(5)),
    [{spy_got,{request,1},sys_SUITE_server},
     {spy_got,{request,3},sys_SUITE_server},
     {spy_got,{request,4},sys_SUITE_server}] = get_messages(),

    sys:install(?server,{id1, SpyFun, func_state}),
    sys:install(?server,{id1, SpyFun, func_state}), %% should not be installed
    sys:install(?server,{id2, SpyFun, func_state}),    
    {ok,-1} = (catch public_call(1)),
    %% We have two SpyFun installed:
    [{spy_got,{request,1},sys_SUITE_server},
     {spy_got,{request,1},sys_SUITE_server}] = get_messages(),
    sys:remove(?server, id1),
    {ok,-1} = (catch public_call(1)),
    %% We have one SpyFun installed:
    [{spy_got,{request,1},sys_SUITE_server}] = get_messages(),
    sys:no_debug(?server),
    {ok,-1} = (catch public_call(1)),
    [] = get_messages(),
    stop(),
    ok.

get_messages() ->
    receive
	Msg -> [Msg|get_messages()]
    after 1 -> []
    end.

special_process(Config) when is_list(Config) ->
    ok = spec_proc(sys_sp1),
    ok = spec_proc(sys_sp2).

spec_proc(Mod) ->
    {ok,_} = Mod:start_link(100),
    ok = sys:statistics(Mod,true),
    ok = sys:trace(Mod,true),
    1 = Ch = Mod:alloc(),
    Free = lists:seq(2,100),
    Replace = case sys:get_state(Mod) of
		  {[Ch],Free} ->
		      fun({A,F}) ->
			      Free = F,
			      {A,[2,3,4]}
		      end;
		  {state,[Ch],Free} ->
		      fun({state,A,F}) ->
			      Free = F,
			      {state,A,[2,3,4]}
		      end
	      end,
    case sys:replace_state(Mod, Replace) of
	{[Ch],[2,3,4]} -> ok;
	{state,[Ch],[2,3,4]} -> ok
    end,
    ok = Mod:free(Ch),
    case sys:get_state(Mod) of
	{[],[1,2,3,4]} -> ok;
	{state,[],[1,2,3,4]} -> ok
    end,
    {ok,[{start_time,_},
	 {current_time,_},
	 {reductions,_},
	 {messages_in,2},
	 {messages_out,1}]} = sys:statistics(Mod,get),
    ok = sys:statistics(Mod,false),
    [] = sys:replace_state(Mod, fun(_) -> [] end),
    process_flag(trap_exit,true),
    ok = case catch sys:get_state(Mod) of
	     [] ->
		 ok;
	     {'EXIT',{{callback_failed,
		       {Mod,system_get_state},{throw,fail}},_}} ->
		 ok
	 end,
    ok = sync_terminate(Mod),
    {ok,_} = Mod:start_link(4),
    ok = case catch sys:replace_state(Mod, fun(_) -> {} end) of
	     {} ->
		 ok;
	     {'EXIT',{{callback_failed,
		       {Mod,system_replace_state},{throw,fail}},_}} ->
		 ok
	 end,
    ok = sync_terminate(Mod),
    {ok,_} = Mod:start_link(4),
    StateFun = fun(_) -> error(fail) end,
    ok = case catch sys:replace_state(Mod, StateFun) of
	     {} ->
		 ok;
	     {'EXIT',{{callback_failed,
		       {Mod,system_replace_state},{error,fail}},_}} ->
		 ok;
	     {'EXIT',{{callback_failed,StateFun,{error,fail}},_}} ->
		 ok
	 end,
    ok = sync_terminate(Mod).

sync_terminate(Mod) ->
    P = whereis(Mod),
    MRef = erlang:monitor(process,P),
    ok = sys:terminate(Mod, normal),
    receive
        {'DOWN',MRef,_,_,normal} ->
            ok
    end,
    undefined = whereis(Mod),
    ok.

%%%%%%%%%%%%%%%%%%%%
%% Dummy server

public_call(Arg) ->
    gen_server:call(?server,{req,Arg},1000).

start() ->
    gen_server:start_link({local,?server},?MODULE,[],[]).

stop() ->
    gen_server:call(?server,stop,1000).

init([]) ->
    {ok,0}.

handle_call({req,Arg},_From,State) ->
    NewState = State+1,
    {reply,{ok,-Arg},NewState};
handle_call(stop,_From,State) ->
    {stop,normal,ok,State}.

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
