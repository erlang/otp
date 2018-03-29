%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,round(1.5*?TIMEOUT/1000)}}].

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
 
    TypeSSH = ssh_test_lib:ssh_type(),

    AlgoTcSet =
	[{Alg, [parallel], specific_test_cases(Tag,Alg,SshcAlgos,SshdAlgos,TypeSSH)}
	 || {Tag,Algs} <- ErlAlgos ++ DoubleAlgos,
	    Alg <- Algs],

    TagGroupSet ++ AlgoTcSet.

tags() -> [kex,cipher,mac,compression,public_key].
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
		   ssh_test_lib:installed_ssh_version("TIMEOUT"),
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
            init_per_group(Tag, Alg, Config)
    end.


init_per_group(public_key=Tag, Alg, Config) ->
    ct:log("Init tests for public_key ~p",[Alg]),
    PrefAlgs = {preferred_algorithms,[{Tag,[Alg]}]},
    %% Daemon started later in init_per_testcase
    try
        setup_pubkey(Alg,
                 [{pref_algs,PrefAlgs},
                  {tag_alg,{Tag,Alg}}
                  | Config])
    catch
        _:_ -> {skip, io_lib:format("Unsupported: ~p",[Alg])}
    end;

init_per_group(Tag, Alg, Config) ->
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
                     [{pref_algs,PrefAlgs},
                      {tag_alg,{Tag,Alg}}
                      | Config]).


end_per_group(_Alg, Config) ->
    case proplists:get_value(srvr_pid,Config) of
	Pid when is_pid(Pid) ->
	    ssh:stop_daemon(Pid),
	    ct:log("stopped ~p",[proplists:get_value(srvr_addr,Config)]);
	_ ->
	    ok
    end.



init_per_testcase(TC, Config) ->
    init_per_testcase(TC, proplists:get_value(tag_alg,Config), Config).


init_per_testcase(TC, {public_key,Alg}, Config) ->
    ExtraOpts = case TC of
                    simple_connect ->
                        [{user_dir, proplists:get_value(priv_dir,Config)}];
                    _ ->
                        []
                end,
    Opts = pubkey_opts(Config) ++ ExtraOpts,
    case {ssh_file:user_key(Alg,Opts), ssh_file:host_key(Alg,Opts)} of
        {{ok,_}, {ok,_}} ->
            start_pubkey_daemon([proplists:get_value(pref_algs,Config)
                                | ExtraOpts],
                                [{extra_daemon,true}|Config]);
        {{ok,_}, {error,Err}} ->
            {skip, io_lib:format("No host key: ~p",[Err])};
        
        {{error,Err}, {ok,_}} ->
            {skip, io_lib:format("No user key: ~p",[Err])};
        
        _ ->
            {skip, "Neither host nor user key"}
    end;

init_per_testcase(sshc_simple_exec_os_cmd, _, Config) ->
     start_pubkey_daemon([proplists:get_value(pref_algs,Config)],
                         [{extra_daemon,true}|Config]);

init_per_testcase(_, _, Config) ->
    Config.


end_per_testcase(_TC, Config) ->
    case proplists:get_value(extra_daemon, Config, false) of
        true ->
            case proplists:get_value(srvr_pid,Config) of
                Pid when is_pid(Pid) ->
                    ssh:stop_daemon(Pid),
                    ct:log("stopped ~p",[proplists:get_value(srvr_addr,Config)]),
                    Config;
                _ ->
                    Config
            end;
        _ ->
            Config
    end.

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
%% A simple exec call
simple_connect(Config) ->
    {Host,Port} = proplists:get_value(srvr_addr, Config),
    Opts =
        case proplists:get_value(tag_alg, Config) of
            {public_key,Alg} -> [{pref_public_key_algs,[Alg]}];
            _ -> []
        end,
    ConnectionRef = ssh_test_lib:std_connect(Config, Host, Port, Opts),
    ct:log("~p:~p connected! ~p",[?MODULE,?LINE,ConnectionRef]),
    ssh:close(ConnectionRef).

%%--------------------------------------------------------------------
%% Testing if no group matches
simple_exec_groups_no_match_too_small(Config) ->
    try_exec_simple_group({400,500,600}, Config).

simple_exec_groups_no_match_too_large(Config) ->
    try_exec_simple_group({9200,9500,9700}, Config).


try_exec_simple_group(Group, Config) ->
    try simple_exec_group(Group, Config)
    of
	_ -> ct:fail("Exec though no group available")
    catch
        error:{badmatch,{error,"Key exchange failed"}} -> ok
    end.

%%--------------------------------------------------------------------
%% Testing all default groups

simple_exec_groups() ->
    [{timetrap,{seconds,120}}].
    
simple_exec_groups(Config) ->
    Sizes = interpolate( public_key:dh_gex_group_sizes() ),
    lists:foreach(
      fun(Sz) ->
	      ct:log("Try size ~p",[Sz]),
	      ct:comment(Sz),
	      simple_exec_group(Sz, Config),
	      ct:log("Size ~p ok",[Sz])
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
                       Result = ssh_test_lib:open_sshc(Host, Port,
                                                       [" -C"
                                                        " -o UserKnownHostsFile=",KnownHosts,
                                                        " -o StrictHostKeyChecking=no"
                                                        ],
                                                       " 1+1."),
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
		    {fail, "Bad result (see log in testcase)"}
	    end
    after ?TIMEOUT ->
	    ct:fail("Did not receive answer (timeout)")
    end.

%%--------------------------------------------------------------------
%% Connect to the ssh server of the OS
sshd_simple_exec(Config) ->
    ClientPubKeyOpts =
        case proplists:get_value(tag_alg,Config) of
            {public_key,Alg} -> [{pref_public_key_algs,[Alg]}];
            _ -> []
        end,
    ConnectionRef = ssh_test_lib:connect(22, [{silently_accept_hosts, true},
                                              proplists:get_value(pref_algs,Config),
					      {user_interaction, false}
                                              | ClientPubKeyOpts]),
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

specific_test_cases(Tag, Alg, SshcAlgos, SshdAlgos, TypeSSH) -> 
    case Tag of
        public_key -> [simple_connect];
        _ -> [simple_connect, simple_exec, simple_sftp]
    end 
    ++ case supports(Tag, Alg, SshcAlgos) of
           true when TypeSSH == openSSH ->
               [sshc_simple_exec_os_cmd];
           _ ->
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
    ct:log("starting pubkey_daemon",[]),
    Opts = pubkey_opts(Config) ++ Opts0,
    {Pid, Host, Port} = ssh_test_lib:daemon([{failfun, fun ssh_test_lib:failfun/2}
                                             | Opts]),
    ct:log("started ~p:~p  ~p",[Host,Port,Opts]),
    [{srvr_pid,Pid},{srvr_addr,{Host,Port}} | Config].


pubkey_opts(Config) ->
    SystemDir = filename:join(proplists:get_value(priv_dir,Config), "system"),
    [{auth_methods,"publickey"},
     {system_dir, SystemDir}].


setup_pubkey(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    UserDir = proplists:get_value(priv_dir, Config),
    Keys =
        [ssh_test_lib:setup_dsa(DataDir, UserDir),
         ssh_test_lib:setup_rsa(DataDir, UserDir),
         ssh_test_lib:setup_ecdsa("256", DataDir, UserDir)
        ],
    ssh_test_lib:write_auth_keys(Keys, UserDir), % 'authorized_keys' shall contain ALL pub keys
    Config.

setup_pubkey(Alg, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    UserDir = proplists:get_value(priv_dir, Config),
    ct:log("Setup keys for ~p",[Alg]),
    case Alg of
        'ssh-dss' -> ssh_test_lib:setup_dsa(DataDir, UserDir);
        'ssh-rsa' -> ssh_test_lib:setup_rsa(DataDir, UserDir);
        'rsa-sha2-256' -> ssh_test_lib:setup_rsa(DataDir, UserDir);
        'rsa-sha2-512' -> ssh_test_lib:setup_rsa(DataDir, UserDir);
        'ecdsa-sha2-nistp256' -> ssh_test_lib:setup_ecdsa("256", DataDir, UserDir);
        'ecdsa-sha2-nistp384' -> ssh_test_lib:setup_ecdsa("384", DataDir, UserDir);
        'ecdsa-sha2-nistp521' -> ssh_test_lib:setup_ecdsa("521", DataDir, UserDir)
    end,
    Config.


simple_exec_group(I, Config) when is_integer(I) ->
    simple_exec_group({I,I,I}, Config);
simple_exec_group({Min,I,Max}, Config) ->
    {Host,Port} = proplists:get_value(srvr_addr, Config),
    ssh_test_lib:std_simple_exec(Host, Port, Config,
				 [{dh_gex_limits,{Min,I,Max}}]).

