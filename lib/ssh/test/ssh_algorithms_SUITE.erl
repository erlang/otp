%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%%

-module(ssh_algorithms_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("ssh/src/ssh_transport.hrl").
-include("ssh_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(TIMEOUT, 35000).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,40}}].

all() -> 
    %% [{group,kex},{group,cipher}... etc
    [{group,C} || C <- tags()].


groups() ->
    ErlAlgos = extract_algos(ssh:default_algorithms()),
    SshcAlgos = extract_algos(ssh_test_lib:default_algorithms(sshc)),
    SshdAlgos = extract_algos(ssh_test_lib:default_algorithms(sshd)),
    
    DoubleAlgos = 
	[{Tag, double(Algs)} || {Tag,Algs} <- ErlAlgos,
				length(Algs) > 1,
				lists:member(Tag, two_way_tags())],
    TagGroupSet =
	[{Tag, [], group_members_for_tag(Tag,Algs,DoubleAlgos)}
	 || {Tag,Algs} <- ErlAlgos,
	    lists:member(Tag,tags())
	],

    AlgoTcSet =
	[{Alg, [parallel], specific_test_cases(Tag,Alg,SshcAlgos,SshdAlgos)}
	 || {Tag,Algs} <- ErlAlgos ++ DoubleAlgos,
	    Alg <- Algs],

    TagGroupSet ++ AlgoTcSet.

tags() -> [kex,cipher,mac,compression].
two_way_tags() -> [cipher,mac,compression].
    
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ?CHECK_CRYPTO(
       begin
	   ct:log("~n"
		  "Environment:~n============~n"
		  "os:getenv(\"HOME\") = ~p~n"
		  "init:get_argument(home) = ~p~n~n~n"
		  "OS ssh:~n=======~n~p~n~n~n"
		  "Erl ssh:~n========~n~p~n~n~n"
		  "crypto:info_lib():~n========~n~p~n~n~n"
		  "Installed ssh client:~n=====================~n~p~n~n~n"
		  "Installed ssh server:~n=====================~n~p~n~n~n"
		  "Misc values:~n============~n"
		  " -- Default dh group exchange parameters ({min,def,max}): ~p~n"
		  " -- dh_default_groups: ~p~n"
		  " -- Max num algorithms: ~p~n"
		 ,[os:getenv("HOME"),
		   init:get_argument(home),
		   os:cmd("ssh -V"),
		   ssh:default_algorithms(),
		   crypto:info_lib(),
		   ssh_test_lib:default_algorithms(sshc),
		   ssh_test_lib:default_algorithms(sshd),
		   {?DEFAULT_DH_GROUP_MIN,?DEFAULT_DH_GROUP_NBITS,?DEFAULT_DH_GROUP_MAX},
		   public_key:dh_gex_group_sizes(),
		   ?MAX_NUM_ALGORITHMS
		  ]),
	   ct:log("all() ->~n    ~p.~n~ngroups()->~n    ~p.~n",[all(),groups()]),
	   ssh:start(),
	   [{std_simple_sftp_size,25000} % Sftp transferred data size
	    | setup_pubkey(Config)]
       end
      ).


end_per_suite(_Config) ->
    ssh:stop().


init_per_group(Group, Config) ->
    case lists:member(Group, tags()) of
	true ->
	    %% A tag group
	    Tag = Group,
	    ct:comment("==== ~p ====",[Tag]),
	    Config;
	false ->
	    %% An algorithm group
	    Tag = proplists:get_value(name,
				      hd(proplists:get_value(tc_group_path, Config))),
	    Alg = Group,
	    PA =
		case split(Alg) of
		    [_] ->
			[Alg];
		    [A1,A2] ->
			[{client2server,[A1]},
			 {server2client,[A2]}]
		end,
	    ct:log("Init tests for tag=~p alg=~p",[Tag,PA]),
	    PrefAlgs = {preferred_algorithms,[{Tag,PA}]},
	    start_std_daemon([PrefAlgs],
			     [{pref_algs,PrefAlgs} | Config])
    end.

end_per_group(_Alg, Config) ->
    case proplists:get_value(srvr_pid,Config) of
	Pid when is_pid(Pid) ->
	    ssh:stop_daemon(Pid),
	    ct:log("stopped ~p",[proplists:get_value(srvr_addr,Config)]);
	_ ->
	    ok
    end.



init_per_testcase(sshc_simple_exec_os_cmd, Config) ->
    start_pubkey_daemon([proplists:get_value(pref_algs,Config)], Config);
init_per_testcase(_TC, Config) ->
    Config.


end_per_testcase(sshc_simple_exec_os_cmd, Config) ->
    case proplists:get_value(srvr_pid,Config) of
	Pid when is_pid(Pid) ->
	    ssh:stop_daemon(Pid),
	    ct:log("stopped ~p",[proplists:get_value(srvr_addr,Config)]);
	_ ->
	    ok
    end;
end_per_testcase(_TC, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
%% A simple sftp transfer
simple_sftp(Config) ->
    {Host,Port} = proplists:get_value(srvr_addr, Config),
    ssh_test_lib:std_simple_sftp(Host, Port, Config).

%%--------------------------------------------------------------------
%% A simple exec call
simple_exec(Config) ->
    {Host,Port} = proplists:get_value(srvr_addr, Config),
    ssh_test_lib:std_simple_exec(Host, Port, Config).

%%--------------------------------------------------------------------
%% Testing if no group matches
simple_exec_groups_no_match_too_small(Config) ->
    try simple_exec_group({400,500,600}, Config)
    of
	_ -> ct:fail("Exec though no group available")
    catch
	error:{badmatch,{error,"No possible diffie-hellman-group-exchange group found"}} ->
	    ok
    end.

simple_exec_groups_no_match_too_large(Config) ->
    try simple_exec_group({9200,9500,9700}, Config)
    of
	_ -> ct:fail("Exec though no group available")
    catch
	error:{badmatch,{error,"No possible diffie-hellman-group-exchange group found"}} ->
	    ok
    end.

%%--------------------------------------------------------------------
%% Testing all default groups

simple_exec_groups() -> [{timetrap,{minutes,5}}].

simple_exec_groups(Config) ->
    Sizes = interpolate( public_key:dh_gex_group_sizes() ),
    lists:foreach(
      fun(Sz) ->
	      ct:log("Try size ~p",[Sz]),
	      ct:comment(Sz),
	      case simple_exec_group(Sz, Config) of
		  expected -> ct:log("Size ~p ok",[Sz]);
		  _ -> ct:log("Size ~p not ok",[Sz])
	      end
      end, Sizes),
    ct:comment("~p",[lists:map(fun({_,I,_}) -> I;
				  (I) -> I
			       end,Sizes)]).


interpolate([I1,I2|Is]) ->
    OneThird = (I2-I1) div 3,
    [I1,
     {I1, I1 + OneThird, I2},
     {I1, I1 + 2*OneThird, I2} | interpolate([I2|Is])];
interpolate(Is) ->
    Is.

%%--------------------------------------------------------------------
%% Use the ssh client of the OS to connect

sshc_simple_exec_os_cmd(Config) ->
    PrivDir = ?config(priv_dir, Config),
    KnownHosts = filename:join(PrivDir, "known_hosts"),
    {Host,Port} = ?config(srvr_addr, Config),
    Parent = self(),
    Client = spawn(
	       fun() ->
		       Cmd = lists:concat(["ssh -p ",Port,
					   " -C"
					   " -o UserKnownHostsFile=",KnownHosts,
					   " -o StrictHostKeyChecking=no"
					   " ",Host," 1+1."]),
		       Result = os:cmd(Cmd),
		       ct:log("~p~n  = ~p",[Cmd, Result]),
		       Parent ! {result, self(), Result, "2"}
	       end),
    receive
	{result, Client, RawResult, Expect} ->
	    Lines = string:tokens(RawResult, "\r\n"),
	    case lists:any(fun(Line) -> Line==Expect end,
			   Lines) of
		true ->
		    ok;
		false ->
		    ct:log("Bad result: ~p~nExpected: ~p~nMangled result: ~p", [RawResult,Expect,Lines]),
		    {fail, "Bad result"}
	    end
    after ?TIMEOUT ->
	    ct:fail("Did not receive answer")
    end.

%%--------------------------------------------------------------------
%% Connect to the ssh server of the OS
sshd_simple_exec(_Config) ->
    ConnectionRef = ssh_test_lib:connect(22, [{silently_accept_hosts, true},
					      {user_interaction, false}]),
    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "echo testing", infinity),
    Data0 = {ssh_cm, ConnectionRef, {data, ChannelId0, 0, <<"testing\n">>}},
    case ssh_test_lib:receive_exec_result(Data0) of
	expected ->
	    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId0);
	{unexpected_msg,{ssh_cm, ConnectionRef, {exit_status, ChannelId0, 0}}
	 = ExitStatus0} ->
	    ct:log("0: Collected data ~p", [ExitStatus0]),
	    ssh_test_lib:receive_exec_result(Data0,
					     ConnectionRef, ChannelId0);
	Other0 ->
	    ct:fail(Other0)
    end,
    
    {ok, ChannelId1} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId1,
				  "echo testing1", infinity),
    Data1 = {ssh_cm, ConnectionRef, {data, ChannelId1, 0, <<"testing1\n">>}},
    case ssh_test_lib:receive_exec_result(Data1) of
	expected ->
	    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId1);
	{unexpected_msg,{ssh_cm, ConnectionRef, {exit_status, ChannelId1, 0}}
	 = ExitStatus1} ->
	    ct:log("0: Collected data ~p", [ExitStatus1]),
	    ssh_test_lib:receive_exec_result(Data1,
					     ConnectionRef, ChannelId1);
	Other1 ->
	    ct:fail(Other1)
    end,
    ssh:close(ConnectionRef).


%%%================================================================
%%%
%%% Lib functions
%%% 

%%%----------------------------------------------------------------
%%%
%%% For construction of the result of all/0 and groups/0
%%% 
group_members_for_tag(Tag, Algos, DoubleAlgos) ->
    [{group,Alg} || Alg <- Algos++proplists:get_value(Tag,DoubleAlgos,[])].

double(Algs) -> [concat(A1,A2) || A1 <- Algs,
				  A2 <- Algs,
				  A1 =/= A2].

concat(A1, A2) -> list_to_atom(lists:concat([A1," + ",A2])).

split(Alg) -> ssh_test_lib:to_atoms(string:tokens(atom_to_list(Alg), " + ")).

specific_test_cases(Tag, Alg, SshcAlgos, SshdAlgos) -> 
    [simple_exec, simple_sftp] ++
	case supports(Tag, Alg, SshcAlgos) of
	    true ->
		case ssh_test_lib:ssh_type() of
		    openSSH ->
			[sshc_simple_exec_os_cmd];
		    _ ->
			[]
		end;
	    false ->
		[]
	end ++
	case supports(Tag, Alg, SshdAlgos) of
	    true ->
		[sshd_simple_exec];
	    _ ->
		[]
	end ++
	case {Tag,Alg} of
	    {kex,_} when Alg == 'diffie-hellman-group-exchange-sha1' ;
			 Alg == 'diffie-hellman-group-exchange-sha256' ->
		[simple_exec_groups,
		 simple_exec_groups_no_match_too_large,
		 simple_exec_groups_no_match_too_small
		];
	    _ ->
		[]
	end.

supports(Tag, Alg, Algos) ->
    lists:all(fun(A) ->
		      lists:member(A, proplists:get_value(Tag, Algos,[]))
	      end,
	      split(Alg)).
		      

extract_algos(Spec) ->
    [{Tag,get_atoms(List)} || {Tag,List} <- Spec].

get_atoms(L) ->
    lists:usort(
      [ A || X <- L,
	     A <- case X of
		      {_,L1} when is_list(L1) -> L1;
		      Y when is_atom(Y) -> [Y]
		  end]).

%%%----------------------------------------------------------------
%%%
%%% Test case related
%%%
start_std_daemon(Opts, Config) ->
    ct:log("starting std_daemon",[]),
    {Pid, Host, Port} = ssh_test_lib:std_daemon(Config, Opts),
    ct:log("started ~p:~p  ~p",[Host,Port,Opts]),
    [{srvr_pid,Pid},{srvr_addr,{Host,Port}} | Config].

start_pubkey_daemon(Opts0, Config) ->
    Opts = [{auth_methods,"publickey"}|Opts0],
    {Pid, Host, Port} = ssh_test_lib:std_daemon1(Config, Opts),
    ct:log("started pubkey_daemon ~p:~p  ~p",[Host,Port,Opts]),
    [{srvr_pid,Pid},{srvr_addr,{Host,Port}} | Config].


setup_pubkey(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    UserDir = proplists:get_value(priv_dir, Config),
    ssh_test_lib:setup_dsa(DataDir, UserDir),
    ssh_test_lib:setup_rsa(DataDir, UserDir),
    ssh_test_lib:setup_ecdsa("256", DataDir, UserDir),
    Config.


simple_exec_group(I, Config) when is_integer(I) ->
    simple_exec_group({I,I,I}, Config);
simple_exec_group({Min,I,Max}, Config) ->
    {Host,Port} = proplists:get_value(srvr_addr, Config),
    ssh_test_lib:std_simple_exec(Host, Port, Config,
				 [{dh_gex_limits,{Min,I,Max}}]).

