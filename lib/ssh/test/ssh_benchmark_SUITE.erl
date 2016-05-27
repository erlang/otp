%%%-------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2016. All Rights Reserved.
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


suite() -> [{ct_hooks,[{ts_install_cth,[{nodenames,2}]}]},
	    {timetrap,{minutes,3}}
	   ].
%%suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> [{group, opensshc_erld} 
%%	  {group, erlc_opensshd}
	 ].

groups() ->
    [{opensshc_erld, [{repeat, 3}], [openssh_client_shell,
				     openssh_client_sftp]}
    ].


init_per_suite(Config) ->
    catch ssh:stop(),
    try 
	report_client_algorithms(),
	ok = ssh:start(),
	{ok,TracerPid} = erlang_trace(),
	[{tracer_pid,TracerPid} | init_sftp_dirs(Config)]
    catch
	C:E ->
	    {skip, io_lib:format("Couldn't start ~p:~p",[C,E])}
    end.
    
end_per_suite(_Config) ->
    catch ssh:stop(),
    ok.



init_per_group(opensshc_erld, Config) ->
    case ssh_test_lib:ssh_type() of
	openSSH -> 
	    DataDir = proplists:get_value(data_dir, Config),
	    UserDir = proplists:get_value(priv_dir, Config),
	    ssh_test_lib:setup_dsa(DataDir, UserDir),
	    ssh_test_lib:setup_rsa(DataDir, UserDir),
	    ssh_test_lib:setup_ecdsa("256", DataDir, UserDir),
	    Common = ssh_test_lib:intersect_bi_dir(
		       ssh_test_lib:intersection(ssh:default_algorithms(),
						 ssh_test_lib:default_algorithms(sshc))),
	    [{c_kexs, ssh_test_lib:sshc(kex)},
	     {c_ciphers, ssh_test_lib:sshc(cipher)},
	     {common_algs, Common}
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


init_sftp_dirs(Config) ->
    UserDir = proplists:get_value(priv_dir, Config),
    SrcDir = filename:join(UserDir, "sftp_src"),
    ok = file:make_dir(SrcDir),
    SrcFile = "big_data",
    DstDir = filename:join(UserDir, "sftp_dst"),
    ok = file:make_dir(DstDir),
    N = 100 * 1024*1024,
    ok = file:write_file(filename:join(SrcDir,SrcFile), crypto:strong_rand_bytes(N)),
    [{sftp_src_dir,SrcDir}, {sftp_dst_dir,DstDir}, {src_file,SrcFile}, {sftp_size,N}
     | Config].

%%%================================================================
openssh_client_shell(Config) ->
    lists:foreach(
      fun(PrefAlgs=[{kex,[Kex]}]) when Kex == 'diffie-hellman-group-exchange-sha256' ->
	      lists:foreach(
		fun(Grp) ->
			openssh_client_shell(Config, 
					     [{preferred_algorithms, PrefAlgs},
					      {dh_gex_groups, [Grp]}
					     ])
		end, moduli());
	 (PrefAlgs) -> 
	      openssh_client_shell(Config, 
				   [{preferred_algorithms, PrefAlgs}])
      end, variants(kex,Config) ++ variants(cipher,Config)
     ).
    

openssh_client_shell(Config, Options) ->
    SystemDir = proplists:get_value(data_dir, Config),
    UserDir = proplists:get_value(priv_dir, Config),
    KnownHosts = filename:join(UserDir, "known_hosts"),
    
    {ok, TracerPid} = erlang_trace(),
    {ServerPid, _Host, Port} =
	ssh_test_lib:daemon([{system_dir, SystemDir},
			     {public_key_alg, ssh_dsa},
			     {failfun, fun ssh_test_lib:failfun/2} |
			     Options]),
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
	    Times = find_times(List, [accept_to_hello, kex, kex_to_auth, auth, to_prompt]),
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
				  KexAlgStr = fmt_alg(Algs#alg.kex, List),
				  [{value, Value},
				   {suite, ?MODULE}, 
				   {name, mk_name(["Erl server kex ",KexAlgStr," [",Unit,"]"])}
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
    after 60*1000 ->
	    ssh:stop_daemon(ServerPid),
	    exit(SlavePid, kill),
	    {fail, timeout}
    end.


%%%================================================================
openssh_client_sftp(Config) ->
    lists:foreach(
      fun(PrefAlgs) -> 
	      openssh_client_sftp(Config, [{preferred_algorithms,PrefAlgs}])
      end, variants(cipher,Config)).


openssh_client_sftp(Config, Options) ->
    SystemDir = proplists:get_value(data_dir, Config),
    UserDir = proplists:get_value(priv_dir, Config),
    SftpSrcDir = proplists:get_value(sftp_src_dir, Config),
    SrcFile = proplists:get_value(src_file, Config),
    SrcSize = proplists:get_value(sftp_size, Config),
    KnownHosts = filename:join(UserDir, "known_hosts"),

    {ok, TracerPid} = erlang_trace(),
    {ServerPid, _Host, Port} =
	ssh_test_lib:daemon([{system_dir, SystemDir},
			     {public_key_alg, ssh_dsa},
			     {subsystems,[ssh_sftpd:subsystem_spec([%{cwd,  SftpSrcDir},
								    {root, SftpSrcDir}])]},
			     {failfun, fun ssh_test_lib:failfun/2} 
			     | Options]),
    ct:pal("ServerPid = ~p",[ServerPid]),
    ct:sleep(500),
    Cmd = lists:concat(["sftp",
			" -b -",
			" -P ",Port,
			" -o UserKnownHostsFile=", KnownHosts,
			" -o \"StrictHostKeyChecking no\"",
			" localhost:",SrcFile
		       ]),
%%    ct:pal("Cmd = ~p",[Cmd]),
    
    Parent = self(),
    SlavePid = spawn(fun() ->
			     Parent ! {self(),os:cmd(Cmd)}
		     end),
    receive
	{SlavePid, _ClientResponse} ->
	    ct:pal("ClientResponse = ~p~nServerPid = ~p",[_ClientResponse,ServerPid]),
	    {ok, List} = get_trace_list(TracerPid),
%%ct:pal("List=~p",[List]),
	    Times = find_times(List, [channel_open_close]),
	    Algs = find_algs(List),
	    ct:pal("Algorithms = ~p~n~nTimes = ~p",[Algs,Times]),
	    lists:foreach(
	      fun({{A,B},Value,Unit}) when A==encrypt ; A==decrypt -> 
		      Data = [{value, Value},
			      {suite, ?MODULE}, 
			      {name, mk_name(["Sftp Cipher ",A," ",B," [",Unit,"]"])}
			     ],
		      ct:pal("sftp ct_event:notify ~p",[Data]),
		      ct_event:notify(#event{name = benchmark_data,
					     data = Data});
		 ({channel_open_close,Value,Unit}) ->
		      Cipher = fmt_alg(Algs#alg.encrypt, List),
		      Data = [{value, round( (1024*Value) / SrcSize )},
			      {suite, ?MODULE}, 
			      {name, mk_name(["Sftp transfer ",Cipher," [",Unit," per kbyte]"])}
			     ],
		      ct:pal("sftp ct_event:notify ~p",[Data]),
		      ct_event:notify(#event{name = benchmark_data,
					     data = Data});
		 (_) ->
		      skip
	      end, Times),
	    ssh:stop_daemon(ServerPid),
	    ok
    after 2*60*1000 ->
	    ssh:stop_daemon(ServerPid),
	    exit(SlavePid, kill),
	    {fail, timeout}
    end.

%%%================================================================
variants(Tag, Config) ->
    TagType =
	case proplists:get_value(Tag, ssh:default_algorithms()) of
	    [{_,_}|_] -> one_way;
	    [A|_] when is_atom(A) -> two_way
	end,
    [ [{Tag,tag_value(TagType,Alg)}]
      || Alg <- proplists:get_value(Tag, proplists:get_value(common_algs,Config))
    ].

tag_value(two_way, Alg) -> [Alg];
tag_value(one_way, Alg) -> [{client2server,[Alg]},
			    {server2client,[Alg]}].
    
%%%----------------------------------------------------------------
fmt_alg(Alg, List) when is_atom(Alg) ->
    fmt_alg(atom_to_list(Alg), List);
fmt_alg(Alg = "diffie-hellman-group-exchange-sha" ++ _, List) ->
    try 
	integer_to_list(find_gex_size_string(List))
    of
	GexSize -> lists:concat([Alg," ",GexSize])
    catch
	_:_ -> Alg
    end;
fmt_alg(Alg, _List) ->
    Alg.

%%%----------------------------------------------------------------
mk_name(Name) -> [char(C) || C <- lists:concat(Name)].

char($-) -> $_;
char(C) -> C.

%%%----------------------------------------------------------------
find_times(L, Xs) ->
    [find_time(X,L) || X <- Xs]	++
	function_algs_times_sizes([{ssh_transport,encrypt,2},
				   {ssh_transport,decrypt,2},
				   {ssh_message,decode,1},
				   {ssh_message,encode,1}], L).

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
		    ?LINE,
		    fun(C=#call{mfa = {ssh_connection_handler,handle_event,4},
				args = [_, {version_exchange,_}, {hello,_}, _]}) ->
			    C#call.t_call
		    end,
		    ?LINE
		   ], L, []),
    {accept_to_hello, now2micro_sec(now_diff(T1,T0)), microsec};
find_time(kex, L) ->
    [T0,T1] = find([fun(C=#call{mfa = {ssh_connection_handler,handle_event,4},
				args = [_, {version_exchange,_}, {hello,_}, _]}) ->
			    C#call.t_call
		    end,
		    ?LINE,
		    ?send(#ssh_msg_newkeys{}),
		    ?LINE
		    ], L, []),
    {kex, now2micro_sec(now_diff(T1,T0)), microsec};
find_time(kex_to_auth, L) ->
    [T0,T1] = find([?send(#ssh_msg_newkeys{}),
		    ?LINE,
		    ?recv(#ssh_msg_userauth_request{}),
		    ?LINE
		   ], L, []),
    {kex_to_auth, now2micro_sec(now_diff(T1,T0)), microsec};
find_time(auth, L) ->
    [T0,T1] = find([?recv(#ssh_msg_userauth_request{}),
		    ?LINE,
		    ?send(#ssh_msg_userauth_success{}),
		    ?LINE
		   ], L, []),
    {auth, now2micro_sec(now_diff(T1,T0)), microsec};
find_time(to_prompt, L) ->
    [T0,T1] = find([fun(C=#call{mfa = {ssh_acceptor,handle_connection,5}}) ->
			    C#call.t_call
		    end,
		    ?LINE,
		    ?recv(#ssh_msg_channel_request{request_type="env"}),
		    ?LINE
		   ], L, []),
    {to_prompt, now2micro_sec(now_diff(T1,T0)), microsec};
find_time(channel_open_close, L) ->
    [T0,T1] = find([?recv(#ssh_msg_channel_request{request_type="subsystem"}),
		    ?LINE,
		    ?send(#ssh_msg_channel_close{}),
		    ?LINE
		   ], L, []),
    {channel_open_close, now2micro_sec(now_diff(T1,T0)), microsec}.
 


find([F,Id|Fs], [C|Cs], Acc) when is_function(F,1) ->
    try
	F(C)
    of
	T -> find(Fs, Cs, [T|Acc])
    catch
	_:_ -> find([F,Id|Fs], Cs, Acc)
    end;
find([], _, Acc) ->
    lists:reverse(Acc).


find_algs(L) ->
    {value, #call{result={ok,Algs}}} =
	lists:keysearch({ssh_transport,select_algorithm,3}, #call.mfa, L),
    Algs.

find_gex_size_string(L) ->
    %% server
    {value, #call{result={ok,{Size, _}}}} =
	lists:keysearch({public_key,dh_gex_group,4}, #call.mfa, L),
    Size.

%%%----------------
function_algs_times_sizes(EncDecs, L) ->
    Raw = [begin
	       {Tag,Size} = function_ats_result(EncDec, C),
	       {Tag, Size, now2micro_sec(now_diff(T1,T0))}
	   end
	   || EncDec <- EncDecs,
	      C = #call{mfa = ED,
			% args = Args,  %%[S,Data],
			t_call = T0,
			t_return = T1} <- L,
	      ED == EncDec
	  ],
    [{Alg, round(1024*Time/Size), "microsec per kbyte"}  % Microseconds per 1k bytes.
     || {Alg,Size,Time} <- lists:foldl(fun increment/2, [], Raw)].

function_ats_result({ssh_transport,encrypt,2}, #call{args=[S,Data]}) ->
    {{encrypt,S#ssh.encrypt}, size(Data)};
function_ats_result({ssh_transport,decrypt,2}, #call{args=[S,Data]}) ->
    {{decrypt,S#ssh.decrypt}, size(Data)};
function_ats_result({ssh_message,encode,1}, #call{result=Data}) ->
    {encode, size(Data)};
function_ats_result({ssh_message,decode,1}, #call{args=[Data]}) ->
    {decode, size(Data)}.
    

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
    MonRef = monitor(process, TracerPid),
    TracerPid ! {get_trace_list,self()},
    receive
	{trace_list,L} ->
	    demonitor(MonRef),
	    {ok, pair_events(lists:reverse(L))};
	{'DOWN', MonRef, process, TracerPid, Info} ->
	    {error, {tracer_down,Info}}
			      
    after 3*60*1000 -> 
	    demonitor(MonRef),
	    {error,no_reply}
    end.

erlang_trace() ->
    TracerPid = spawn(fun trace_loop/0),
    0 = erlang:trace(new, true, [call,timestamp,{tracer,TracerPid}]),
    [init_trace(MFA, tp(MFA))
     || MFA <- [{ssh_acceptor,handle_connection,5},
%%		{ssh_connection_handler,hello,2},
		{ssh_message,encode,1},
		{ssh_message,decode,1},
		{ssh_transport,select_algorithm,3},
		{ssh_transport,encrypt,2},
		{ssh_transport,decrypt,2},
		{ssh_message,encode,1},
		{ssh_message,decode,1},
		{public_key,dh_gex_group,4} % To find dh_gex group size
	       ]],
    init_trace({ssh_connection_handler,handle_event,4},
	       [{['_', {version_exchange,'_'}, {hello,'_'}, '_'],
		 [],
		 [return_trace]}]),
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
report_client_algorithms() ->
    try
	ssh_test_lib:extract_algos( ssh_test_lib:default_algorithms(sshc) )
    of
	ClientAlgs ->
	    ct:pal("The client supports:~n~p",[ClientAlgs])
    catch
	Cls:Err ->
	    ct:pal("Testing client about algorithms failed:~n~p  ~p",[Cls,Err])
    end.

%%%----------------------------------------------------------------


now2sec({A,B,C}) -> A*1000000 + B + C/1000000.

now2micro_sec({A,B,C}) -> (A*1000000 + B)*1000000 + C.
		    
now_diff({A1,B1,C1}, {A0,B0,C0}) -> {A1-A0, B1-B0, C1-C0}.

%%%================================================================
moduli() ->
    [{1023, 5, 16#CF973CD39DC7D62F2C45AAC5180491104C76E0FE5D80A10E6C06AE442F1F373167B0FCBC931F3C157B10A5557008FDE20D68051E6A4DB11CEE0B0749F76D7134B937A59DA998C42BC234A5C1A3CFCD70E624D253D7694076F7B1FD7B8D3427849C9377B3555796ACA58C69DFF542EEEC9859D3ADCE5CC88DF6F7817C9D182EB7},
     {2047, 5, 16#F7693FC11FDDEAA493D3BA36F1FFF9264AA9952209203192A88A697BE9D0E306E306A27430BD87AB9EE9DB4BC78C41950C2EB0E5E4C686E8B1BA6D6A2B1FE91EF40C5EA32C51018323E1D305FE637F35ACABDBFC40AD683F779570A76869EB90015A342B2D1F7C81602688081FCAAA8D623090258D9C5C729C8CDDC0C12CA2D561DD987DB79B6AD7A2A509EBC383BF223FD95BC5A2FCC26FB3F3A0DD3FDC1228E338D3290235A596F9465F7BF490974847E616229A9E60B8F4AA161C52F655843CCCAE8821B40C426B535DE087964778652BBD4EC601C0456AE7128B593FCC64402C891227AE6EE88CC839416FBF462B4852999C646BE0BED7D8CF2BE5E381EF},
     {4095, 2, 16#C8842271626E53546E0C712FA265713F2EE073C20A0723C96B6B182B1EAACC96233D4A199BD0E85F264078A513AD2454F284B8DF543D85019D1E70F2FF54BA43EFBC64AF465C170C3E376F5EC328F98E33E1ED8BED84FA097ABE584152B0E9827ED5CC2B1D4F5ECF2DC46F45C59816D02698EA26F319311E2B6973E83C37021CC8B416AEF653896A1764EE0CEE718A45E8B47CB960BD5907D0E843E8A8E7D4698363C3C3FB3ADC512368B72CAF16510C69052EA2AF51BE00BC8CA04DF1F00A00CC2CA4D74254A1E8738460FD244DDB446CB36554B0A24EEF3710E44DBCF39881E7D3F9AE223388084E7A49A3CB12612AE36416C0EB5628DF1477FEE4A5CF77CDC09AA0E2C989C0B7D1310AFA44B81DA79A65226C7EA510057991EABF9388DC5EA9F52FEA5D3B0872843F50878740794E523E9DC60E0EA1FC8746A7B2AA31FCA89AAA2FA907BED116C69D98F912DD5089BECF28577064225DE96FC214ED1794E7CCE8024F94036D915A123A464C951DA96A5ED7F286F205BEE71BDE2D133FD1891B31178FF25D31611A5B7839F0E68EAF0F8901A571E6917C580F31842A9F19C47E0638483B7947DDCD7864660AC2F8B2C430F1E7FC0F22FA51F96F0499332C5AD3FF9DC7F4332DD5BCCA820CC779B90C0F4C5F0CA52E96FAA187361753FBADC5C80D0492CD80A3EEA5D578772DA9FC1C0E10A0203098AF36D0ED2156BA7321EB},
     {6143, 5, 16#FD9E6B52785CD7BE64D396A599DA4B97CD0BB49183F932A97694D80CA553354DBC26E77B8A0EC002257AADDF6AD27819CE64A06416E4A80B6EA92F28EA8D5B96C774109EEE5816B4B18F84368D1B41864C11AA73D6881675D779B174F6B4E344303F3EFD11BD7DE468467242372FD00908F296F5A2B20E2684F9122D08A46D647B05E298F0BCDAB60468349CCA6DA1B9FEBBC69D256FB9A3F1980F68466364FCEF1C98C1405191A6737A3627BA7F7313A8A18FC0B8521BF3430B1C6805CB44BCEB39904DD30130D24B225B598ED83C5FD757B80189FD9D5C2F9596687C40BAB1C6ED6244944629849D074A4C33FB15DDB3F9760FC59C44BEBB0EC032177147F61789769DAAAE2123CE488F7ECF19BDA051925BA9ED11EAA72DF70C9ECC8F714B4C35728E6679E66A1B56CCAE0FBBD3F9EBF950D4D623ED78E77CC3AD604E91F304EA78CE876F036214BD6F1977BD04C9ADD707D7A3BCCE87AD5D5A11C95E7025B0EA9C649DCB37942A3970A4FB04C284E4DDB4DC90163353B98B1C254FFD28443353F17A87C02E0BDB9F05424CC44C86309F1D73706F039CDAAC3EDC1A64F38FB42707D351DB5360C2680ADC1CC8D1C4AD312ACC904382C26BE33DA0E61429A5940820356ED28586BEB629ED1521D12D25B4DA01926295F3DA504DC9F431B719AC63277BE675E6F6DD4F7499CA11A23744577D653941963E8DAB610F7F226DB52CE5C683F72AEED2B6CE35ED07C29410397A6F7F606477CCC0EDE18CD0D96A7863BC4606193A8799B5AC1EEE6AC5EE36AC3077EC8DAB30EE94434B45B78BC13D96F74D6C4056EAA528CD3C68D308344808819B12F2BFB95A5C1A7DEEE188BF139216DDB7D757D7A50D3C46CE18881D776D617DCFFAA62276045373AA4D9446D7570338F99C0CA8A08851B4F9D388B4C275D3F9B7BA25F235D4329F63F7457C2EB5C68CE2A96D19766F0ED8E19F66DF3C5E29A38795B2F92291BB6EAB6F70A7E89DC9691F28486E9CF87FF11D5DF2E6B030A30B5D476AD59A34EE7262712ED96CEF4A5CAC3F08B3563D44683F746DA094C9CDB34427AF8D8CC2AE1B23C3BEB637},
     {8191, 2, 16#DC61EF13E4F3FC10CC946EEABC33F83EFCB35E0F47E4EC25C1CCBB2C7B502B2EFB0691AA231C8476DD51BA73204E6EA10B1A970FE2CF14AF01E72E1AEA87519A91D00D1499189F94A6CDA9E29C05F11F17FE74A4919A710A2787E180744465DF81C62AA65662FDA46FA6175E8A31E5B29E66DED6701C8FC4217E91D733FE94380F046680967D4CEA7BAC8F3916CDF96AA2C474FAD9650F48403FD0B5B756D34667D36A07767FA33027AE55484D0F701C3CA16632F413A14E4B8645AFAF15B78978C19A7661EDC569BEC72394B1204B166A48FCD5F56BE29840C7794CA6D3440356F15858CDCA9B429C7EA92E17242893FDC8C9C63841A382C32F20CFAB121B4BCAFD7BF9EF07FBF7CDFFECA0CEF3A49C3E2B24FA836F3318435255655E1B281071F62D5E4CD63361299B7828F72936E3FEA9E8044562A6F6ADD5321187C3101E4669C6271598FE1A866C93FE2870A4CEB9254BA32A4719E439317EA42200A335B5CFFA7946A7D0F1BD1A69AA11288B73C71C80B77FE3707CB077DDDEA5CA36A449FAB230C9625A0B12F8275D3FF82F5DA380E7A3F11B6F155FE7E91AC960BD95D9B13F7423AB9B15CC3C4DC34EF296033F009468EA16A721AD659F56C18516025050749ABF05E6D3EBD9778142A530979291F46DAA399A86B7BCDF09CC3E6EEF101419762A306DB45AEFC96C64E83F28338D55905F6A387E0F515E580C3A9B35330E21C32198CDEE3AFB355967A098F635FCA7C49CB4E1E82464B2B390EF1F259E40B9A06235C0273F76284FE6BD534EF3AF7CB01A4A5252B8B94CADC2850B2E56D53F9A31D7C029DF967D0A30C05BC64E119BED6076818FABC8CDD93F3255693E14EFC1A740A5D63A5E847FFE87BAB1DDE0506E1762EA61EFA9F9756151ECCCADD91B98A961A901A2D8B01ABDDD29EC804E8C8D28214BBA26048F924CA66316696E51A49D02FF034D20E44914B1115339CAD3819E0CB1640F0084886FEDDE5E28C29DC48ED30A8C3D789734338F5A9DF42584326E536FD1CF30BC85B8DCBD6120D127C98FE4B3614074F13C2CA4854E6D794156C185C40EB3DA7619CE96ADAF0941BD5499848B034C2B11DFECC0BDFA81C594241F759EF53FC7CDE7F2DE4F23CF81A5A0B7D62E31DABB9198D40307F7824DD130B7D1B80E9B6D322FEEDB5ACE34944F0BFB7D016762A9B2E173BFDD69303766AFBAB45FAB75D05430B4A3515858C4B7F04E23414E4AD03842CB0A20D8FF4B59B7C852BA9A5BE982A8ADA5CB70C36CE2A4D2C31A7015C9F3275E43D192C1B2924424088907A057DA7F2D32A2149922AB2E33F2147D637A3508911CB3FEA5E1AAB4525BACF27B6DD7A3E0AFA978FC3A39DE8882FB22688C3CCC92B6E69ACB0BBF575AB3368E51A2F6A20C414C6F146727CC0045F29061E695D29F7C030CE6929EB3AD11A5CBD0CDEE37347869A3}].
