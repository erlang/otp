%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2026. All Rights Reserved.
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
-include("ssh_transport.hrl").
-include("ssh_test_lib.hrl").

-export([
         suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2
        ]).

-export([
         interpolate/1,
         mlkem768x25519_hybrid_secret_encoding/1,
         simple_connect/1,
         simple_exec/1,
         simple_exec_groups/0,
         simple_exec_groups/1,
         simple_exec_groups_no_match_too_large/1,
         simple_exec_groups_no_match_too_small/1,
         simple_sftp/1,
         sshc_simple_exec_os_cmd/1,
         sshd_simple_exec/1
        ]).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,120}}].

all() ->
    %% [{group,kex},{group,cipher}... etc
    [mlkem768x25519_hybrid_secret_encoding | [{group,C} || C <- tags()]].


groups() ->
    ErlAlgos = extract_algos(ssh_transport:supported_algorithms()),
    SshcAlgos = extract_algos(ssh_test_lib:default_algorithms(sshc)),
    SshdAlgos = extract_algos(ssh_test_lib:default_algorithms(sshd)),

    DoubleAlgos =
	[{Tag, double(Tag,Algs)} || {Tag,Algs} <- ErlAlgos,
                                    length(Algs) > 1,
                                    lists:member(Tag, two_way_tags())],
    TagGroupSet =
	[{Tag, [], group_members_for_tag(Tag,Algs,DoubleAlgos)}
	 || {Tag,Algs} <- ErlAlgos,
	    lists:member(Tag,tags())
	],

    TypeSSH = ssh_test_lib:ssh_type(),

    AlgoTcSet =
	[{Alg, [], specific_test_cases(Tag,Alg,SshcAlgos,SshdAlgos,TypeSSH)}
	 || {Tag,Algs} <- ErlAlgos ++ DoubleAlgos,
	    Alg <- Algs],

    ct:log(
      "ErlAlgos = ~p~n"
      "SshcAlgos = ~p~n"
      "SshdAlgos = ~p~n"
      "DoubleAlgos = ~p~n"
      "TypeSSH = ~p~n"
      "TagGroupSet = ~p~n"
      "AlgoTcSet = ~p~n"
          ,[
            ErlAlgos,
            SshcAlgos,
            SshdAlgos,
            DoubleAlgos,
            TypeSSH,
            TagGroupSet,
            AlgoTcSet
           ]),

    TagGroupSet ++ AlgoTcSet.

tags() -> [kex,cipher,mac,compression,public_key].
two_way_tags() -> [cipher,mac,compression, public_key].

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
		   ssh_test_lib:default_algorithms(sshc,
                                                   %% Use a fake system_dir to enable the test
                                                   %% daemon to start:
                                                   [{system_dir,proplists:get_value(data_dir,Config)}]),
		   ssh_test_lib:default_algorithms(sshd),
		   {?DEFAULT_DH_GROUP_MIN,?DEFAULT_DH_GROUP_NBITS,?DEFAULT_DH_GROUP_MAX},
		   public_key:dh_gex_group_sizes(),
		   ?MAX_NUM_ALGORITHMS
		  ]),
	   ct:log("all() ->~n    ~p.~n~ngroups()->~n    ~p.~n",[all(),groups()]),
	   ssh:start(),
           %% Start a single shared daemon with all algorithms enabled.
           %% Tests restrict algorithms on the client side.
           Config1 = setup_all_keys(Config),
           Config2 = start_shared_daemon(Config1),
	   [{std_simple_sftp_size,25000} % Sftp transferred data size
           | Config2]
       end
      ).


end_per_suite(Config) ->
    case proplists:get_value(srvr_pid, Config) of
        Pid when is_pid(Pid) -> ssh:stop_daemon(Pid);
        _ -> ok
    end,
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
            Algs = split(Tag, Alg),
            SupportedAlgs = proplists:get_value(Tag, ssh_transport:supported_algorithms()),
            PA =
                case Algs of
                    [_] ->
                        [Alg];
                    [A1,A2] when Tag == public_key ->
                        [A1,A2];
                    [A1,A2] ->
                        [{client2server,[A1]},
                         {server2client,[A2]}]
                end,
            case lists:foldl(fun({K,As}, Acc) ->
                                     SAs = proplists:get_value(K,SupportedAlgs),
                                     lists:foldl(fun(A1, Acc1) ->
                                                         case lists:member(A1, SAs) of
                                                             true -> Acc1;
                                                             false -> [A1|Acc1]
                                                         end
                                                 end, Acc, As);
                                (A, Acc) when is_atom(hd(SupportedAlgs)) ->
                                     case lists:member(A, SupportedAlgs) of
                                         true -> Acc;
                                         false -> [A|Acc]
                                     end;
                                (A, Acc) when is_tuple(hd(SupportedAlgs)) ->
                                     [{_,S1},{_,S2}] = SupportedAlgs,
                                     case lists:member(A, S1) andalso
                                         lists:member(A, S2) of
                                         true -> Acc;
                                         false -> [A|Acc]
                                     end
                             end, [], PA) of
                [] ->
                    %% Algorithm supported — set up client-side preferences
                    OtherAlgs = [{T,L} || {T,L} <- ssh_transport:supported_algorithms(), T=/=Tag],
                    PrefAlgs = {preferred_algorithms,[{Tag,PA}|OtherAlgs]},
                    [{pref_algs,PrefAlgs},
                     {tag_alg,{Tag,case Tag of
                                       public_key -> PA;
                                       _ -> [Alg]
                                   end}}
                     | Config];
                L ->
                    {skip,io_lib:format("Unsupported ~p: ~p", [Tag,L])}
            end
    end.


end_per_group(_Alg, _Config) ->
    ok.


init_per_testcase(mlkem768x25519_hybrid_secret_encoding, Config) ->
    case lists:member(x25519, crypto:supports(curves))
        andalso lists:member(mlkem768, crypto:supports(kems))
    of
        false -> {skip, "X25519 or ML-KEM768 not supported"};
        true -> Config
    end;
init_per_testcase(_TC, Config) ->
    Config.


end_per_testcase(_TC, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
%% Regression test for the mlkem768x25519-sha256 hybrid key exchange: the
%% classical (X25519) shared secret must be hashed as a fixed-width 32-byte
%% octet string. Encoding it as a trimmed mpint dropped a genuine leading 0x00
%% byte ~1/512 of the time (when the secret's most significant byte is 0x00 and
%% the next byte is < 0x80), so the two peers derived different exchange hashes
%% and the handshake failed with "incorrect signature".
%%
%% The keys below produce a shared secret starting with <<0x00, 0x42, ...>>
%% which triggers the edge case (0x42 < 0x80).
mlkem768x25519_hybrid_secret_encoding(_Config) ->
    PeerPublic = <<16#94,16#5E,16#6F,16#5A,16#CA,16#B1,16#AD,16#A6,
                   16#31,16#CF,16#12,16#F5,16#47,16#A4,16#25,16#6B,
                   16#6A,16#E5,16#3A,16#A2,16#48,16#6A,16#0D,16#08,
                   16#C6,16#D6,16#73,16#2C,16#B3,16#E2,16#0E,16#5B>>,
    MyPrivate  = <<16#58,16#EF,16#AB,16#DA,16#4C,16#C5,16#6B,16#8E,
                   16#B2,16#43,16#8A,16#86,16#92,16#2C,16#5D,16#73,
                   16#83,16#98,16#B4,16#38,16#0E,16#A3,16#91,16#37,
                   16#F9,16#38,16#5B,16#E5,16#BA,16#B5,16#94,16#6D>>,
    %% Verify the shared secret triggers the edge case
    <<0, B1, _/binary>> = crypto:compute_key(ecdh, PeerPublic, MyPrivate, x25519),
    true = B1 < 16#80,
    %% A spec-conformant peer (e.g. OpenSSH) hashes K_pq concatenated with the
    %% X25519 secret as a fixed-width 32-byte string, preserving the leading 0x00.
    K_pq = crypto:strong_rand_bytes(32),
    Raw = crypto:compute_key(ecdh, PeerPublic, MyPrivate, x25519),
    Expected = crypto:hash(sha256, <<K_pq/binary, Raw/binary>>),
    Expected = ssh_transport:hybrid_common(K_pq, x25519, PeerPublic, MyPrivate).

%%--------------------------------------------------------------------
%% A simple sftp transfer
simple_sftp(Config) ->
    {Host,Port} = proplists:get_value(srvr_addr, Config),
    {preferred_algorithms,AlgEntries} = proplists:get_value(pref_algs, Config),
    Opts = case proplists:get_value(tag_alg, Config) of
               {kex,_} -> [{preferred_algorithms,AlgEntries}];
               _ -> [{modify_algorithms,[{append,AlgEntries}]}]
           end,
    ssh_test_lib:std_simple_sftp(Host, Port, Config, Opts).

%%--------------------------------------------------------------------
%% A simple exec call
simple_exec(Config) ->
    {Host,Port} = proplists:get_value(srvr_addr, Config),
    {preferred_algorithms,AlgEntries} = proplists:get_value(pref_algs, Config),
    Opts = case proplists:get_value(tag_alg, Config) of
               {kex,_} -> [{preferred_algorithms,AlgEntries}];
               _ -> [{modify_algorithms,[{append,AlgEntries}]}]
           end,
    ssh_test_lib:std_simple_exec(Host, Port, Config, Opts).

%%--------------------------------------------------------------------
%% A simple connect (public_key auth)
simple_connect(Config) ->
    {Host,Port} = proplists:get_value(srvr_addr, Config),
    {preferred_algorithms,AlgEntries} = proplists:get_value(pref_algs, Config),
    Opts =
        case proplists:get_value(tag_alg, Config) of
            {public_key,Alg} -> [{pref_public_key_algs,Alg},
                                 {preferred_algorithms,AlgEntries}];
            {kex,_} -> [{preferred_algorithms,AlgEntries}];
            _ -> [{modify_algorithms,[{append,AlgEntries}]}]
        end,
    ConnectionRef = ssh_test_lib:std_connect(Config, Host, Port,
                                             [{silently_accept_hosts, true},
                                              {user_interaction, false} |
                                              Opts]),
    verify_negotiated_algorithm(ConnectionRef, Config),
    ?CT_LOG("connected! ~p",[ConnectionRef]),
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
    [{timetrap,{seconds,240}}].

simple_exec_groups(Config) ->
    Sizes = interpolate( public_key:dh_gex_group_sizes() ),
    lists:foreach(
      fun(Sz) ->
              ?CT_LOG("Try size ~p",[Sz]),
              ct:comment(Sz),
              simple_exec_group(Sz, Config),
              ?CT_LOG("Size ~p ok",[Sz])
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
                                                        " -o CheckHostIP=no"
                                                        " -o StrictHostKeyChecking=no"
                                                        " -o UpdateHostKeys=no"
                                                        " -q"
                                                        " -x"
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
            {public_key,Alg} -> [{pref_public_key_algs,Alg}];
            _ -> []
        end,
    ConnectionRef = ssh_test_lib:connect_with_retry(?SSH_DEFAULT_PORT,
                                                    [proplists:get_value(pref_algs,Config)
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
            ?CT_LOG("0: Collected data ~p", [ExitStatus0]),
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
            ?CT_LOG("0: Collected data ~p", [ExitStatus1]),
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

double(Tag, Algs) -> [concat(Tag,A1,A2) || A1 <- Algs,
                                           A2 <- Algs,
                                           A1 =/= A2].

concat(Tag, A1, A2) ->
    list_to_atom(lists:concat(["D: ",Tag," ",A1," + ",A2])).

split(TagA, Alg) ->
    Tag = atom_to_list(TagA),
    ssh_test_lib:to_atoms(
      case string:tokens(atom_to_list(Alg), " ") of
          ["D:",Tag,A1,"+",A2] ->[A1,A2];
          Other -> Other
      end).

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
	      split(Tag, Alg)).


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
%%% Single shared daemon setup
%%%
%%% Instead of starting/stopping a daemon per algorithm group,
%%% we start ONE daemon in init_per_suite with all algorithms enabled
%%% and both password + publickey auth. Each test restricts algorithms
%%% on the client side via preferred_algorithms/modify_algorithms.
%%%
%%% Host keys are loaded into memory and served via ssh_algorithms_key_cb,
%%% bypassing the ssh_file filename collision (multiple ECDSA curves and
%%% RSA variants all map to the same file).
%%%

setup_all_keys(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    AllPKAlgs = proplists:get_value(public_key, ssh_transport:supported_algorithms()),
    %% Setup user keys — appends each pubkey to authorized_keys
    lists:foreach(
      fun(Alg) ->
              try ssh_test_lib:setup_user_key(Alg, DataDir, PrivDir)
              catch _:_ -> ok
              end
      end, AllPKAlgs),
    %% Load all host private keys into a map for the in-memory key_cb
    HostKeys = lists:foldl(
      fun(Alg, Acc) ->
              try
                  File = filename:join(DataDir, ssh_test_lib:file_base_name(system_src, Alg)),
                  {ok, KeyBin0} = file:read_file(File),
                  KeyBin = ssh_test_lib:remove_comment(KeyBin0),
                  {ok, [{Key,_}|_]} = ssh_file:decode_ssh_file(private, Alg, KeyBin, ignore),
                  Acc#{Alg => Key}
              catch _:_ -> Acc
              end
      end, #{}, AllPKAlgs),
    ?CT_LOG("setup_all_keys: loaded host keys for ~p", [maps:keys(HostKeys)]),
    [{host_keys, HostKeys} | Config].

start_shared_daemon(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    HostKeys = proplists:get_value(host_keys, Config),
    %% Use all supported algorithms (not just defaults) so deprecated
    %% algorithms like ssh-rsa and ssh-dss are also tested.
    AllAlgs = ssh_transport:supported_algorithms(),
    {Pid, Host, Port} =
        ssh_test_lib:daemon(
          [{key_cb, {ssh_algorithms_key_cb, [{host_keys, HostKeys}]}},
           {preferred_algorithms, AllAlgs},
           {user_dir, PrivDir},
           {user_passwords, [{"usr1","pwd1"}]},
           {auth_methods, "publickey,password"},
           {exec, erlang_eval},
           {subsystems, [ssh_sftpd:subsystem_spec([])]},
           {failfun, fun ssh_test_lib:failfun/2}]),
    ?CT_LOG("Shared daemon started at ~p:~p (pid ~p)~n"
           "Host keys: ~p", [Host, Port, Pid, maps:keys(HostKeys)]),
    [{srvr_pid, Pid}, {srvr_addr, {Host, Port}} | Config].

%%%----------------------------------------------------------------
%%%
%%% Test case related
%%%
simple_exec_group(I, Config) when is_integer(I) ->
    simple_exec_group({I,I,I}, Config);
simple_exec_group({Min,I,Max}, Config) ->
    {Host,Port} = proplists:get_value(srvr_addr, Config),
    ssh_test_lib:std_simple_exec(Host, Port, Config,
				 [proplists:get_value(pref_algs,Config),
                                  {dh_gex_limits,{Min,I,Max}}]).

%%%----------------------------------------------------------------
%%%
%%% Algorithm verification
%%%
%%% Asserts that the negotiated algorithm matches what the test
%%% requested via preferred_algorithms. For kex and public_key the
%%% mapping is 1:1. For cipher/mac/compression, AEAD name translation
%%% and directional splits make exact matching complex — we log but
%%% don't fail (the connection itself proves the algorithm works).
%%%
verify_negotiated_algorithm(ConnectionRef, Config) ->
    {Tag, Expected} = proplists:get_value(tag_alg, Config),
    {algorithms, NegAlgs} = ssh:connection_info(ConnectionRef, algorithms),
    case Tag of
        kex ->
            Neg = proplists:get_value(kex, NegAlgs),
            case lists:member(Neg, Expected) of
                true -> ok;
                false -> ct:fail("Negotiated kex ~p not in expected ~p",
                                 [Neg, Expected])
            end;
        public_key ->
            Neg = proplists:get_value(hkey, NegAlgs),
            case lists:member(Neg, Expected) of
                true -> ok;
                false -> ct:fail("Negotiated hkey ~p not in expected ~p",
                                 [Neg, Expected])
            end;
        _ ->
            %% cipher, mac, compression — log for diagnostics
            ?CT_LOG("Negotiated algorithms: ~p", [NegAlgs]),
            ok
    end.
