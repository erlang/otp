%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2014. All Rights Reserved.
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
-include_lib("test_server/include/test_server.hrl").

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

log(suite) -> [];
log(Config) when is_list(Config) ->
    {ok,_Server} = start(),
    ok = sys:log(?server,true),
    {ok,-44} = public_call(44),
    ok = sys:log(?server,false),
    ok = sys:log(?server,print),
    stop(),
    ok.

log_to_file(suite) -> [];
log_to_file(Config) when is_list(Config) ->
    TempName = test_server:temp_name(?config(priv_dir,Config) ++ "sys."),
    {ok,_Server} = start(),
    ok = sys:log_to_file(?server,TempName),
    {ok,-44} = public_call(44),
    ok = sys:log_to_file(?server,false),
    {ok,Fd} = file:open(TempName,[read]),
    Msg1 = io:get_line(Fd,''),
    Msg2 = io:get_line(Fd,''),
    file:close(Fd),
    lists:prefix("*DBG* sys_SUITE_server got call {req,44} from ",Msg1),
    lists:prefix("*DBG* sys_SUITE_server sent {ok,-44} to ",Msg2),
    stop(),
    ok.

stats(suite) -> [];
stats(Config) when is_list(Config) ->
    Self = self(),
    {ok,_Server} = start(),
    ok = sys:statistics(?server,true),
    {ok,-44} = public_call(44),
    {ok,Stats} = sys:statistics(?server,get),
    lists:member({messages_in,1},Stats),
    lists:member({messages_out,1},Stats),
    ok = sys:statistics(?server,false),
    {status,_Pid,{module,_Mod},[_PDict,running,Self,_,_]} =
	sys:get_status(?server),
    {ok,no_statistics} = sys:statistics(?server,get),
    stop(),
    ok.

trace(suite) -> [];
trace(Config) when is_list(Config) ->
    {ok,_Server} = start(),
    test_server:sleep(2000),
    test_server:capture_start(),
    sys:trace(?server,true),
    {ok,-44} = public_call(44),
    %% ho, hum, allow for the io to reach us..
    test_server:sleep(1000),
    test_server:capture_stop(),
    [Msg1,Msg2] = test_server:capture_get(),
    lists:prefix("*DBG* sys_SUITE_server got call {req,44} from ",Msg1),
    lists:prefix("*DBG* sys_SUITE_server sent {ok,-44} to ",Msg2),
    stop(),
    ok.

suspend(suite) -> [];
suspend(Config) when is_list(Config) ->
    ?line {ok,_Server} = start(),
    ?line sys:suspend(?server,1000),
    ?line {'EXIT',_} = (catch public_call(48)),
    ?line {status,_,_,[_,suspended,_,_,_]} = sys:get_status(?server),
    ?line sys:suspend(?server,1000), %% doing it twice is no error
    ?line {'EXIT',_} = (catch public_call(48)),
    ?line sys:resume(?server),
    ?line {status,_,_,[_,running,_,_,_]} = sys:get_status(?server),
    ?line {ok,-48} = (catch public_call(48)),
    ?line sys:resume(?server), %% doing it twice is no error
    ?line {ok,-48} = (catch public_call(48)),
    ?line stop(),
    ok.

install(suite) -> [];
install(Config) when is_list(Config) ->
    ?line {ok,_Server} = start(),
    ?line Master = self(),
    ?line SpyFun = 
	fun(func_state,Event,ProcState) ->
		case Event of
		    {in,{'$gen_call',_From,{req,Arg}}} ->
			io:format("Trigged\n"),
			Master ! {spy_got,{request,Arg},ProcState};
		    Other ->
			io:format("Trigged other=~p\n",[Other])
		end
	end,
    ?line sys:install(?server,{SpyFun,func_state}),
    ?line {ok,-1} = (catch public_call(1)),
    ?line sys:no_debug(?server),
    ?line {ok,-2} = (catch public_call(2)),
    ?line sys:install(?server,{SpyFun,func_state}),
    ?line sys:install(?server,{SpyFun,func_state}),
    ?line {ok,-3} = (catch public_call(3)),
    ?line sys:remove(?server,SpyFun),
    ?line {ok,-4} = (catch public_call(4)),
    ?line Msgs = test_server:messages_get(),
    ?line [{spy_got,{request,1},sys_SUITE_server},
	   {spy_got,{request,3},sys_SUITE_server}] = Msgs,
    ?line stop(),
    ok.

special_process(suite) -> [];
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
    ok = sys:terminate(Mod, normal),
    {ok,_} = Mod:start_link(4),
    ok = case catch sys:replace_state(Mod, fun(_) -> {} end) of
	     {} ->
		 ok;
	     {'EXIT',{{callback_failed,
		       {Mod,system_replace_state},{throw,fail}},_}} ->
		 ok
	 end,
    ok = sys:terminate(Mod, normal),
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
    ok = sys:terminate(Mod, normal).

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
