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
-include_lib("ssh/src/ssh_transport.hrl").
-include_lib("ssh/src/ssh_connect.hrl").
-include_lib("ssh/src/ssh_userauth.hrl").


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
	{SlavePid, _ClientResponse} ->
%%	    ct:pal("ClientResponse = ~p",[_ClientResponse]),
	    {ok, List} = get_trace_list(TracerPid),
	    Times = find_times(List),
	    Algs = find_algs(List),
	    ct:pal("Algorithms = ~p~n~nTimes = ~p",[Algs,Times]),
	    lists:foreach(
	      fun({Tag,Value,Unit}) ->
		      EventData =
			  case Tag of
			      {A,B} when A==encrypt ; A==decrypt -> 
				  [{value, Value},
				   {suite, ?MODULE}, 
				   {name, mk_name(["Cipher ",A," ",B," [",Unit,"]"])}
				  ];
			      kex ->
				  [{value, Value},
				   {suite, ?MODULE}, 
				   {name, mk_name(["Erl server kex ",Algs#alg.kex," [",Unit,"]"])}
				  ];
			      _ when is_atom(Tag) ->
				  [{value, Value},
				   {suite, ?MODULE}, 
				   {name, mk_name(["Erl server ",Tag," [",Unit,"]"])}
				  ]
			  end,
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


%%%================================================================
mk_name(Name) -> [char(C) || C <- lists:concat(Name)].

char($-) -> $_;
char(C) -> C.

%%%----------------------------------------------------------------
find_times(L) ->
    Xs =  [accept_to_hello, kex, kex_to_auth, auth, to_prompt],
    [find_time(X,L) || X <- Xs]	++
	crypto_algs_times_sizes([encrypt,decrypt], L).

-record(call, {
	  mfa,
	  pid,
	  t_call,
	  t_return,
	  args,
	  result
	 }).

%%%----------------
-define(send(M), fun(C=#call{mfa = {ssh_message,encode,1},
			     args = [M]}) ->
			 C#call.t_return
		 end).

-define(recv(M), fun(C=#call{mfa = {ssh_message,decode,1},
			     result = M}) ->
			    C#call.t_call
		 end).

find_time(accept_to_hello, L) ->
    [T0,T1] = find([fun(C=#call{mfa = {ssh_acceptor,handle_connection,5}}) ->
			    C#call.t_call
		    end,
		    fun(C=#call{mfa = {ssh_connection_handler,hello,_},
				args = [socket_control|_]}) ->
			    C#call.t_return
		    end
		   ], L, []),
    {accept_to_hello, now2micro_sec(now_diff(T1,T0)), microsec};
find_time(kex, L) ->
    [T0,T1] = find([fun(C=#call{mfa = {ssh_connection_handler,hello,_},
				args = [socket_control|_]}) ->
			    C#call.t_call
		    end,
		    ?send(#ssh_msg_newkeys{})
		    ], L, []),
    {kex, now2micro_sec(now_diff(T1,T0)), microsec};
find_time(kex_to_auth, L) ->
    [T0,T1] = find([?send(#ssh_msg_newkeys{}),
		    ?recv(#ssh_msg_userauth_request{})
		   ], L, []),
    {kex_to_auth, now2micro_sec(now_diff(T1,T0)), microsec};
find_time(auth, L) ->
    [T0,T1] = find([?recv(#ssh_msg_userauth_request{}),
		    ?send(#ssh_msg_userauth_success{})
		   ], L, []),
    {auth, now2micro_sec(now_diff(T1,T0)), microsec};
find_time(to_prompt, L) ->
    [T0,T1] = find([fun(C=#call{mfa = {ssh_acceptor,handle_connection,5}}) ->
			    C#call.t_call
		    end,
		    ?recv(#ssh_msg_channel_request{request_type="env"})
		   ], L, []),
    {to_prompt, now2micro_sec(now_diff(T1,T0)), microsec}.


find([F|Fs], [C|Cs], Acc) when is_function(F,1) ->
    try
	F(C)
    of
	T -> find(Fs, Cs, [T|Acc])
    catch
	_:_ -> find([F|Fs], Cs, Acc)
    end;
find([], _, Acc) ->
    lists:reverse(Acc).


find_algs(L) ->
    {value,#call{result={ok,Algs}}} =
	lists:keysearch({ssh_transport,select_algorithm,3}, #call.mfa, L),
    Algs.

%%%----------------
crypto_algs_times_sizes(EncDecs, L) ->
    Raw = [{_Algorithm = case EncDec of
			     encrypt -> {encrypt,S#ssh.encrypt};
			     decrypt -> {decrypt,S#ssh.decrypt}
			 end, 
	    size(Data),
	    now2micro_sec(now_diff(T1, T0))
	   }
	   || EncDec <- EncDecs,
	      #call{mfa = {ssh_transport,ED,2},
		    args = [S,Data],
		    t_call = T0,
		    t_return = T1} <- L,
	      ED == EncDec
	  ],
    [{Alg, round(1024*Time/Size), "microsec per kbyte"}  % Microseconds per 1k bytes.
     || {Alg,Size,Time} <- lists:foldl(fun increment/2, [], Raw)].

increment({Alg,Sz,T}, [{Alg,SumSz,SumT}|Acc]) ->
    [{Alg,SumSz+Sz,SumT+T} | Acc];
increment(Spec, [X|Acc]) ->
    [X | increment(Spec,Acc)]; % Not so many Alg, 2 or 3
increment({Alg,Sz,T},[]) ->
    [{Alg,Sz,T}].

%%%----------------------------------------------------------------
%%%
%%% API for the traceing
%%% 
get_trace_list(TracerPid) ->
    TracerPid ! {get_trace_list,self()},
    receive
	{trace_list,L} -> {ok, pair_events(lists:reverse(L))}
    after 5000 -> {error,no_reply}
    end.

erlang_trace() ->
    TracerPid = spawn(fun trace_loop/0),
    0 = erlang:trace(new, true, [call,timestamp,{tracer,TracerPid}]),
    [init_trace(MFA, tp(MFA))
     || MFA <- [{ssh_acceptor,handle_connection,5},
		{ssh_connection_handler,hello,2},
		{ssh_message,encode,1},
		{ssh_message,decode,1},
		{ssh_transport,select_algorithm,3},
		{ssh_transport,encrypt,2},
		{ssh_transport,decrypt,2}
	       ]],
    {ok, TracerPid}.

tp({_M,_F,Arity}) ->
    [{lists:duplicate(Arity,'_'), [], [{return_trace}]}].

%%%----------------------------------------------------------------
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
	{get_trace_list, From} ->
	    From ! {trace_list, L},
	    trace_loop(L);
	Ev ->
	    trace_loop([Ev|L])
    end.

pair_events(L) ->
    pair_events(L, []).

pair_events([{trace_ts,Pid,call,{M,F,Args},TS0} | L], Acc) ->
    Arity = length(Args),
    {ReturnValue,TS1} = find_return(Pid, {M,F,Arity}, L),
    pair_events(L, [#call{mfa = {M,F,Arity},
			  pid = Pid,
			  t_call = TS0,
			  t_return = TS1,
			  args = Args,
			  result = ReturnValue} | Acc]);
pair_events([_|L], Acc) ->
    pair_events(L, Acc);
pair_events([], Acc) ->
    lists:reverse(Acc).


find_return(Pid, MFA, 
	    [{trace_ts, Pid, return_from, MFA, ReturnValue, TS}|_]) ->
    {ReturnValue, TS};
find_return(Pid, MFA, [_|L]) ->
    find_return(Pid, MFA, L);
find_return(_, _, []) ->
    {undefined, undefined}.

%%%----------------------------------------------------------------
now2sec({A,B,C}) -> A*1000000 + B + C/1000000.

now2micro_sec({A,B,C}) -> (A*1000000 + B)*1000000 + C.
		    
now_diff({A1,B1,C1}, {A0,B0,C0}) -> {A1-A0, B1-B0, C1-C0}.

