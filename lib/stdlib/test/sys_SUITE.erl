%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(sys_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,log/1,log_to_file/1,
	 stats/1,trace/1,suspend/1,install/1]).
-export([handle_call/3,terminate/2,init/1]).
-include_lib("test_server/include/test_server.hrl").

-define(server,sys_SUITE_server).


%% Doesn't look into change_code at all
%% Doesn't address writing your own process that understands
%% system messages at all.


suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [log, log_to_file, stats, trace, suspend, install].

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
    ?line {ok,_Server} = start(),
    ?line ok = sys:log(?server,true),
    ?line {ok,-44} = public_call(44),
    ?line ok = sys:log(?server,false),
    ?line ok = sys:log(?server,print),
    ?line stop(),
    ok.

log_to_file(suite) -> [];
log_to_file(Config) when is_list(Config) ->
    TempName = test_server:temp_name(?config(priv_dir,Config) ++ "sys."),
    ?line {ok,_Server} = start(),
    ?line ok = sys:log_to_file(?server,TempName),
    ?line {ok,-44} = public_call(44),
    ?line ok = sys:log_to_file(?server,false),
    ?line {ok,Fd} = file:open(TempName,[read]),
    ?line Msg1 = io:get_line(Fd,''),
    ?line Msg2 = io:get_line(Fd,''),
    ?line file:close(Fd),
    ?line lists:prefix("*DBG* sys_SUITE_server got call {req,44} from ",Msg1),
    ?line lists:prefix("*DBG* sys_SUITE_server sent {ok,-44} to ",Msg2),
    ?line stop(),
    ok.

stats(suite) -> [];
stats(Config) when is_list(Config) ->
    ?line Self = self(),
    ?line {ok,_Server} = start(),
    ?line ok = sys:statistics(?server,true),
    ?line {ok,-44} = public_call(44),
    ?line {ok,Stats} = sys:statistics(?server,get),
    ?line lists:member({messages_in,1},Stats),
    ?line lists:member({messages_out,1},Stats),
    ?line ok = sys:statistics(?server,false),
    ?line {status,_Pid,{module,_Mod},[_PDict,running,Self,_,_]} =
	sys:get_status(?server),
    ?line {ok,no_statistics} = sys:statistics(?server,get),
    ?line stop(),
    ok.

trace(suite) -> [];
trace(Config) when is_list(Config) ->
    ?line {ok,_Server} = start(),
    case os:type() of
	vxworks ->
	    ?line test_server:sleep(20000);
	_ ->
	    ?line test_server:sleep(2000)
    end,
    ?line test_server:capture_start(),
    ?line sys:trace(?server,true),
    ?line {ok,-44} = public_call(44),
    %% ho, hum, allow for the io to reach us..
    case os:type() of
	vxworks ->
	    ?line test_server:sleep(10000);
	_ ->
	    ?line test_server:sleep(1000)
    end,
    ?line test_server:capture_stop(),
    ?line [Msg1,Msg2] = test_server:capture_get(),
    ?line lists:prefix("*DBG* sys_SUITE_server got call {req,44} from ",Msg1),
    ?line lists:prefix("*DBG* sys_SUITE_server sent {ok,-44} to ",Msg2),
    ?line stop(),
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
