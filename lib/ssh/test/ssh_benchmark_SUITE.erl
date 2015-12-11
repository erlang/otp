%%%-------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015. All Rights Reserved.
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
-module(ssh_benchmark_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct_event.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("ssh/src/ssh.hrl").
-include_lib("ssh/src/ssh_connect.hrl").

suite() -> [{ct_hooks,[{ts_install_cth,[{nodenames,2}]}]}].
%%suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> [{group, opensshc_erld} 
%%	  {group, erlc_opensshd}
	 ].

groups() ->
    [{opensshc_erld, [{repeat, 3}], [openssh_client_shell]},
     {erlc_opensshd, [{repeat, 3}], [erl_shell]}
    ].


init_per_suite(Config) ->
    catch ssh:stop(),
    catch crypto:stop(),
    try 
	ok = crypto:start(),
	ok = ssh:start(),
	{ok,TracerPid} = erlang_trace(),
	[{tracer_pid,TracerPid} | Config]
    catch
	C:E ->
	    {skip, io_lib:format("Couldn't start ~p:~p",[C,E])}
    end.
    
end_per_suite(_Config) ->
    catch ssh:stop(),
    catch crypto:stop(),
    ok.



init_per_group(opensshc_erld, Config) ->
    case ssh_test_lib:ssh_type() of
	openSSH -> 
	    DataDir = ?config(data_dir, Config),
	    UserDir = ?config(priv_dir, Config),
	    ssh_test_lib:setup_dsa(DataDir, UserDir),
	    ssh_test_lib:setup_rsa(DataDir, UserDir),
	    ssh_test_lib:setup_ecdsa("256", DataDir, UserDir),
	    [{c_kexs, ssh_test_lib:sshc(kex)},
	     {c_ciphers, ssh_test_lib:sshc(cipher)}
	     | Config];
	_ -> 
	    {skip, "No OpenSsh client found"}
    end;
		       
init_per_group(erlc_opensshd, _) ->
    {skip, "Group erlc_opensshd not implemented"};

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.


init_per_testcase(_Func, Conf) ->
    Conf.

end_per_testcase(_Func, _Conf) ->
    ok.

%%%================================================================
openssh_client_shell(Config) ->
    SystemDir = ?config(data_dir, Config),
    UserDir = ?config(priv_dir, Config),
    KnownHosts = filename:join(UserDir, "known_hosts"),
    
    {ok, TracerPid} = erlang_trace(),
    {ServerPid, _Host, Port} =
	ssh_test_lib:daemon([{system_dir, SystemDir},
			     {public_key_alg, ssh_dsa},
			     {failfun, fun ssh_test_lib:failfun/2}]),
    ct:sleep(500),

    Data = lists:duplicate(100000, $a),
    Cmd = lists:concat(["ssh -p ",Port,
			" -o UserKnownHostsFile=", KnownHosts,
			" -o \"StrictHostKeyChecking no\"",
			" localhost '\"",Data,"\"'."]),
%%    ct:pal("Cmd ="++Cmd),

    Parent = self(),
    SlavePid = spawn(fun() ->
			     Parent ! {self(),os:cmd(Cmd)}
		     end),
    receive
	{SlavePid, ClientResponse} ->
%%	    ct:pal("ClientResponse = ~p",[ClientResponse]),
	    {ok, List} = get_trace_list(TracerPid),
	    Times = find_times(List),
	    Algs = proplists:get_value(algorithms, List, #alg{}),
	    ct:pal("List = ~p~n~nAlgorithms = ~p~n~nTimes = ~p",[List,Algs,Times]),
	    lists:foreach(
	      fun({Tag0,MicroSeconds,Unit}) ->
		      Tag = case Tag0 of
				{A,B} -> lists:concat([A," ",B]);
				_ when is_list(Tag0) -> lists:concat(Tag0);
				_ when is_atom(Tag0) -> Tag0
			    end,
		      DataName = 
			  ["Erl server ",Tag,sp(algo(Tag,Algs))," [",Unit,"]"],
		      EventData = [{value, MicroSeconds},
				   {suite, ?MODULE}, 
				   {name, lists:concat(DataName)}
				  ],
		      ct:pal("ct_event:notify ~p",[EventData]),
		      ct_event:notify(#event{name = benchmark_data,
					     data = EventData})
	      end, Times),
	    ssh:stop_daemon(ServerPid),
	    ok
    after 10000 ->
	    ssh:stop_daemon(ServerPid),
	    exit(SlavePid, kill),
	    {fail, timeout}
    end.


algo(kex,      #alg{kex=Alg}        ) -> Alg;
algo(_, _) -> "".
    
sp("") -> "";
sp(A) -> lists:concat([" ",A]).
    
%%%================================================================
find_times(L) ->
    [{accept_to_hello, find_time([tcp_accept,
				  {send,hello}],  L, [])/1000,
      millisec},
     {kex,             find_time([{send,hello},
				  {send,ssh_msg_newkeys}], L, []),
      microsec},
     {kex_to_auth,     find_time([{send,ssh_msg_newkeys},
				  {recv,ssh_msg_userauth_request}], L, []),
      microsec},
     {auth,            find_time([{recv,ssh_msg_userauth_request},
				  {send,ssh_msg_userauth_success}], L, []),
      microsec},
     {to_prompt,       find_time([tcp_accept,
				  {recv,{ssh_msg_channel_request,"env"}}], L, []),
      microsec}

     | alg_times([encrypt,decrypt], L)
    ].


find_time([Event|Events], [{Event,T}|TraceList], Ts) ->
    %% Important that the first one found is used!
    find_time(Events, TraceList, [T|Ts]);
find_time([], _, [T1,T0]) ->
    now2micro_sec(now_diff(T1,T0));
find_time(Events, [_|TraceList], Ts) ->
    find_time(Events, TraceList, Ts);
find_time(_, [], _Ts) ->
    throw({error,not_found}).



alg_times(Ops, L) ->
    OpAlgs = lists:usort([{Op,Alg} || Op <- Ops,
				      {{{Op,Alg},_,_},_} <- L]),
    [begin
	 {[Op,"(",Alg,")"],
	  sum_times(OpAlg, L, 0, 0),
	  "microsec/kbyte"
	 }
     end || {Op,Alg} = OpAlg <- OpAlgs].


sum_times(T, [{{T,start,Id={_,Nbytes}},TS0}|Events], SumBytes, SumMicroSec) ->
    TS1 = proplists:get_value({T,stop,Id}, Events),
    sum_times(T, Events, SumBytes+Nbytes, SumMicroSec+now2micro_sec(now_diff(TS1,TS0)));
sum_times(T, [_|Events], SumBytes, SumMicroSec) ->
    sum_times(T, Events, SumBytes, SumMicroSec);
sum_times(T, [], SumBytes, SumMicroSec) ->
    round(1024*SumMicroSec / SumBytes). % Microseconds per 1k bytes.

%%%----------------------------------------------------------------
%%%
%%% API for the traceing
%%% 
get_trace_list(TracerPid) ->
    TracerPid ! {get_trace_list,self()},
    receive
	{trace_list,L} -> {ok,lists:reverse(L)}
    after 5000 -> {error,no_reply}
    end.

erlang_trace() ->
    TracerPid = spawn(fun trace_loop/0),
    0 = erlang:trace(new, true, [call,timestamp,{tracer,TracerPid}]),
    [init_trace(MFA, TP)
     || {MFA,TP} <- [{{ssh_acceptor,handle_connection,5},  []},
		     {{ssh_connection_handler,hello,2},    []},
		     {{ssh_message,encode,1},              []},
		     {{ssh_message,decode,1},              [{['_'],         [], [{return_trace}]}]},
		     {{ssh_transport,select_algorithm,3},  [{['_','_','_'], [], [{return_trace}]}]},
		     {{ssh_transport,encrypt,2},           [{['_','_'],     [], [{return_trace}]}]},
		     {{ssh_transport,decrypt,2},           [{['_','_'],     [], [{return_trace}]}]}
		    ]],
    {ok, TracerPid}.


%%%----------------
init_trace(MFA = {Module,_,_}, TP) ->
    case code:is_loaded(Module) of
	false -> code:load_file(Module);
	_ -> ok
    end,
    erlang:trace_pattern(MFA, TP, [local]).

    
trace_loop() ->
    trace_loop([]).

trace_loop(L) ->
    receive
	{trace_ts, Pid, call, {M,F,Args}, TS} = Ev -> 
	    cond_pal(Ev),
	    trace_loop(save_event(call, Pid, {M,F,Args}, TS, L));
	{trace_ts, Pid, return_from, {M,F,Arity}, Ret, TS} = Ev -> 
	    cond_pal(Ev),
	    trace_loop(save_event(return_from, Pid, {M,F,Arity,Ret}, TS, L));
	{get_trace_list, From} ->
	    From ! {trace_list, L},
	    trace_loop(L)

	; Other -> io:format('~p got ~p~n',[self(),Other]), trace_loop(L)
    end.
	    
%%cond_pal(Ev) -> ct:pal("~p",[Ev]).
cond_pal(Ev) -> ok.
    

save_event(_Type, _Pid, MFA, TimeStamp, L) ->
    try	
	event_name(MFA)
    of
	{Tag, 'TS'} -> [{Tag,TimeStamp} | L];
	Val -> [Val | L]
    catch
	_:_ -> L
    end.

event_name({ssh_acceptor,handle_connection,_}) ->                      {tcp_accept, 'TS'};
event_name({ssh_connection_handler,hello,[socket_control|_]}) ->       {{send,hello}, 'TS'};
event_name({ssh_connection_handler,hello,[{version_exchange,_}|_]}) -> {{recv,hello}, 'TS'};
event_name({ssh_message,encode,[Msg]}) ->                              {{send,element(1,Msg)}, 'TS'};
event_name({ssh_message,decode,1,
	    #ssh_msg_channel_request{request_type=ReqType}}) ->  {{recv,{ssh_msg_channel_request,ReqType}}, 'TS'};
event_name({ssh_message,decode,1,Return}) ->                     {{recv,element(1,Return)}, 'TS'};
event_name({ssh_transport,select_algorithm,3,{ok,Algs}}) ->      {algorithms,Algs};
event_name({ssh_transport,encrypt,[S,Data]}) ->   {{{encrypt,S#ssh.encrypt},start, {S#ssh.send_sequence,size(Data)}}, 'TS'};
event_name({ssh_transport,encrypt,2,{S,Ret}}) ->  {{{encrypt,S#ssh.encrypt},stop,  {S#ssh.send_sequence,size(Ret) }}, 'TS'};
event_name({ssh_transport,decrypt,[S,Data]}) ->   {{{decrypt,S#ssh.decrypt},start, {S#ssh.recv_sequence,size(Data)}}, 'TS'};
event_name({ssh_transport,decrypt,2,{S,Ret}}) ->  {{{decrypt,S#ssh.decrypt},stop,  {S#ssh.recv_sequence,size(Ret) }}, 'TS'}.


now2sec({A,B,C}) -> A*1000000 + B + C/1000000.

now2micro_sec({A,B,C}) -> (A*1000000 + B)*1000000 + C.
		    
now_diff({A1,B1,C1}, {A0,B0,C0}) -> {A1-A0, B1-B0, C1-C0}.
    
