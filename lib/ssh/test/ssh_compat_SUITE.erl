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

-module(ssh_compat_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("ssh/src/ssh_transport.hrl"). % #ssh_msg_kexinit{}
-include_lib("kernel/include/inet.hrl"). % #hostent{}
-include_lib("kernel/include/file.hrl"). % #file_info{}
-include("ssh_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(USER,"sshtester").
-define(PASSWD, "foobar").
-define(BAD_PASSWD, "NOT-"?PASSWD).
-define(DOCKER_PFX, "ssh_compat_suite-ssh").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [%%{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,40}}].

all() ->
%%    [check_docker_present] ++
    [{group,G} || G <- ssh_image_versions()].

groups() ->
    [{otp_client, [], [login_otp_is_client,
                       all_algorithms_sftp_exec_reneg_otp_is_client,
                       send_recv_big_with_renegotiate_otp_is_client
                      ]},
     {otp_server, [], [login_otp_is_server,
                       all_algorithms_sftp_exec_reneg_otp_is_server
                      ]} |
     [{G, [], [{group,otp_client}, {group,otp_server}]} || G <- ssh_image_versions()]
    ].


ssh_image_versions() ->
    try
        %% Find all useful containers in such a way that undefined command, too low
        %% priviliges, no containers and containers found give meaningful result:
        L0 = ["REPOSITORY"++_|_] = string:tokens(os:cmd("docker images"), "\r\n"),
        [["REPOSITORY","TAG"|_]|L1] = [string:tokens(E, " ") || E<-L0],
        [list_to_atom(V) || [?DOCKER_PFX,V|_] <- L1]
    of
        Vs ->
            lists:sort(Vs)
    catch
        error:{badmatch,_} ->
            []
    end.

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ?CHECK_CRYPTO(
       case os:find_executable("docker") of
           false ->
               {skip, "No docker"};
           _ ->
               ssh:start(),
               ct:log("Crypto info: ~p",[crypto:info_lib()]),
               Config
       end).

end_per_suite(Config) ->
    %% Remove all containers that are not running:
%%%    os:cmd("docker rm $(docker ps -aq -f status=exited)"),
    %% Remove dangling images:
%%%    os:cmd("docker rmi $(docker images -f dangling=true -q)"),
    catch ssh:stop(),
    Config.


init_per_group(otp_server, Config) ->
    case proplists:get_value(common_remote_client_algs, Config) of
        undefined ->
            SSHver = proplists:get_value(ssh_version, Config, ""),
            {skip,"No "++SSHver++ " client found in docker"};
        _ ->
            Config
    end;

init_per_group(otp_client, Config) ->
    Config;

init_per_group(G, Config0) ->
    case lists:member(G, ssh_image_versions()) of
	true ->
            %% This group is for one of the images
            Vssh = atom_to_list(G),
            Cmnt = io_lib:format("+++ ~s +++",[Vssh]),
            ct:comment("~s",[Cmnt]),
            try start_docker(G) of
                {ok,ID} ->
                    ct:log("==> ~p started",[G]),
                    %% Find the algorithms that both client and server supports:
                    {IP,Port} = ip_port([{id,ID}]),
                    ct:log("Try contact ~p:~p",[IP,Port]),
                    Config1 = [{id,ID},
                               {ssh_version,Vssh}
                               | Config0],
                    try common_algs(Config1, IP, Port) of
                        {ok, ServerHello, RemoteServerCommon, ClientHello, RemoteClientCommon} ->
                            case chk_hellos([ServerHello,ClientHello], Cmnt) of
                                Cmnt ->
                                    ok;
                                NewCmnt ->
                                    ct:comment("~s",[NewCmnt])
                            end,
                            AuthMethods =
                                %% This should be obtained by quering the peer, but that
                                %% is a bit hard. It is possible with ssh_protocol_SUITE
                                %% techniques, but it can wait.
                                case Vssh of
                                    "dropbear" ++ _ ->
                                        [password, publickey];
                                    _ ->
                                        [password, 'keyboard-interactive', publickey]
                                end,
                            [{common_remote_server_algs,RemoteServerCommon},
                             {common_remote_client_algs,RemoteClientCommon},
                             {common_authmethods,AuthMethods}
                             |Config1];
                        Other ->
                            ct:log("Error in init_per_group: ~p",[Other]),
                            stop_docker(ID),
                            {fail, "Can't contact docker sshd"}
                    catch
                        Class:Exc ->
                            ST = erlang:get_stacktrace(),
                            ct:log("common_algs: ~p:~p~n~p",[Class,Exc,ST]),
                            stop_docker(ID),
                            {fail, "Failed during setup"}
                    end
            catch
                cant_start_docker ->
                    {skip, "Can't start docker"};

                C:E ->
                    ST = erlang:get_stacktrace(),
                    ct:log("No ~p~n~p:~p~n~p",[G,C,E,ST]),
                    {skip, "Can't start docker"}
            end;

	false ->
	    Config0
    end.

end_per_group(G, Config) ->
    case lists:member(G, ssh_image_versions()) of
        true ->
            catch stop_docker(proplists:get_value(id,Config));
        false ->
            ok
    end.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
check_docker_present(_Config) ->
    ct:log("This testcase is just to show in Monitor that we have a test host with docker installed",[]),
    {fail, "Test is OK: just showing docker is available"}.

%%--------------------------------------------------------------------
login_otp_is_client(Config) ->
    {IP,Port} = ip_port(Config),
    PublicKeyAlgs = [A || {public_key,A} <- proplists:get_value(common_remote_server_algs, Config)],
    CommonAuths =
        [{AuthMethod,Alg} || AuthMethod <- proplists:get_value(common_authmethods, Config),
                             Alg <- case AuthMethod of
                                        publickey ->
                                            PublicKeyAlgs;
                                        _ ->
                                            [' ']
                                    end
        ],
                                        
    chk_all_algos(?FUNCTION_NAME, CommonAuths, Config,
                  fun(AuthMethod,Alg) ->
                          {Opts,Dir} =
                              case AuthMethod of
                                  publickey ->
                                      {[], setup_remote_auth_keys_and_local_priv(Alg, Config)};
                                  _ ->
                                      {[{password,?PASSWD}], new_dir(Config)}
                              end,
                          ssh:connect(IP, Port, [{auth_methods, atom_to_list(AuthMethod)},
                                                 {user,?USER},
                                                 {user_dir, Dir},
                                                 {silently_accept_hosts,true},
                                                 {user_interaction,false}
                                                 | Opts
                                                ])
                  end).
    

%%--------------------------------------------------------------------
login_otp_is_server(Config) ->
    PublicKeyAlgs = [A || {public_key,A} <- proplists:get_value(common_remote_client_algs, Config)],
    CommonAuths =
        [{AuthMethod,Alg} || AuthMethod <- proplists:get_value(common_authmethods, Config),
                             Alg <- case AuthMethod of
                                        publickey ->
                                            PublicKeyAlgs;
                                        _ ->
                                            [' ']
                                    end
        ],
    SysDir = setup_local_hostdir(hd(PublicKeyAlgs), Config),
    chk_all_algos(?FUNCTION_NAME, CommonAuths, Config,
                  fun(AuthMethod,Alg) ->
                          {Opts,UsrDir} =
                              case AuthMethod of
                                  publickey ->
                                      {[{user_passwords, [{?USER,?BAD_PASSWD}]}],
                                       setup_remote_priv_and_local_auth_keys(Alg, Config)
                                      };
                                  _ ->
                                      {[{user_passwords, [{?USER,?PASSWD}]}],
                                       new_dir(Config)
                                      }
                              end,
                          {Server, Host, HostPort} =
                              ssh_test_lib:daemon(0,
                                                  [{auth_methods, atom_to_list(AuthMethod)},
                                                   {system_dir, SysDir},
                                                   {user_dir, UsrDir},
                                                   {failfun, fun ssh_test_lib:failfun/2}
                                                   | Opts
                                                  ]),
                          R = exec_from_docker(Config, Host, HostPort,
                                               "'lists:concat([\"Answer=\",1+3]).\r\n'",
                                               [<<"Answer=4">>],
                                               ""),
                          ssh:stop_daemon(Server),
                          R
                  end).

%%--------------------------------------------------------------------
all_algorithms_sftp_exec_reneg_otp_is_client(Config) ->
    CommonAlgs = proplists:get_value(common_remote_server_algs, Config),
    {IP,Port} = ip_port(Config),
    chk_all_algos(?FUNCTION_NAME, CommonAlgs, Config,
                  fun(Tag, Alg) ->
                          ConnRes =
                              ssh:connect(IP, Port, 
                                          [{user,?USER},
                                           {password,?PASSWD},
                                           {auth_methods, "password"},
                                           {user_dir, new_dir(Config)},
                                           {preferred_algorithms, [{Tag,[Alg]}]},
                                           {silently_accept_hosts,true},
                                           {user_interaction,false}
                                          ])  ,
                          test_erl_client_reneg(ConnRes, % Seems that max 10 channels may be open in sshd
                                                [{exec,1},
                                                 {sftp,5},
                                                 {no_subsyst,1},
                                                 {setenv, 1},
                                                 {sftp_async,1}
                                                ])
                  end).

%%--------------------------------------------------------------------
all_algorithms_sftp_exec_reneg_otp_is_server(Config) ->
    CommonAlgs = proplists:get_value(common_remote_client_algs, Config),
    UserDir = setup_remote_priv_and_local_auth_keys('ssh-rsa', Config),
    chk_all_algos(?FUNCTION_NAME, CommonAlgs, Config,
                  fun(Tag,Alg) ->
                          HostKeyAlg = case Tag of
                                           public_key -> Alg;
                                           _ -> 'ssh-rsa'
                                       end,
                          SftpRootDir = new_dir(Config),
                          %% ct:log("Rootdir = ~p",[SftpRootDir]),
                          {Server, Host, HostPort} =
                              ssh_test_lib:daemon(0,
                                                  [{preferred_algorithms, [{Tag,[Alg]}]},
                                                   {system_dir, setup_local_hostdir(HostKeyAlg, Config)},
                                                   {user_dir, UserDir},
                                                   {user_passwords, [{?USER,?PASSWD}]},
                                                   {failfun, fun ssh_test_lib:failfun/2},
                                                   {subsystems,
                                                    [ssh_sftpd:subsystem_spec([{cwd,SftpRootDir},
                                                                               {root,SftpRootDir}]),
                                                     {"echo_10",{ssh_echo_server,[10,[{dbg,true}]]}}
                                                    ]}
                                                  ]),
                          R = do([fun() ->
                                          exec_from_docker(Config, Host, HostPort,
                                                           "hi_there.\r\n",
                                                           [<<"hi_there">>],
                                                           "")
                                  end,
                                  fun() ->
                                          sftp_tests_erl_server(Config, Host, HostPort, SftpRootDir, UserDir)
                                  end
                                 ]),
                          ssh:stop_daemon(Server),
                          R
                  end).

%%--------------------------------------------------------------------
send_recv_big_with_renegotiate_otp_is_client(Config) ->
    %% Connect to the remote openssh server:
    {IP,Port} = ip_port(Config),
    {ok,C} = ssh:connect(IP, Port, [{user,?USER},
                                    {password,?PASSWD},
                                    {user_dir, setup_remote_auth_keys_and_local_priv('ssh-rsa', Config)},
                                    {silently_accept_hosts,true},
                                    {user_interaction,false}
                                   ]),

    %% Open a channel and exec the Linux 'cat' command at the openssh side.
    %% This 'cat' will read stdin and write to stdout until an eof is read from stdin.
    {ok, Ch1} = ssh_connection:session_channel(C, infinity),
    success = ssh_connection:exec(C, Ch1, "cat", infinity),

    %% Build big binary
    HalfSizeBytes = 100*1000*1000,
    Data = << <<X:32>> || X <- lists:seq(1, HalfSizeBytes div 4)>>,

    %% Send the data. Must spawn a process to avoid deadlock. The client will block
    %% until all is sent through the send window. But the server will stop receiveing
    %% when the servers send-window towards the client is full.
    %% Since the client can't receive before the server has received all but 655k from the client
    %% ssh_connection:send/4 is blocking...
    spawn_link(
      fun() ->
              ct:comment("Sending ~p Mbytes with renegotiation in the middle",[2*byte_size(Data)/1000000]),
              %% ct:log("sending first ~p bytes",[byte_size(Data)]),
              ok = ssh_connection:send(C, Ch1, Data, 10000),
              %% ct:log("Init renegotiation test",[]),
              Kex1 = renegotiate_test(init, C),
              %% ct:log("sending next ~p bytes",[byte_size(Data)]),
              ok = ssh_connection:send(C, Ch1, Data, 10000),
              %% ct:log("Finnish renegotiation test",[]),
              renegotiate_test(Kex1, C),
              %% ct:log("sending eof",[]),
              ok = ssh_connection:send_eof(C, Ch1)
              %%, ct:log("READY, sent ~p bytes",[2*byte_size(Data)])
      end),

    {eof,ReceivedData} =
        loop_until(fun({eof,_}) -> true;
                      (_      ) -> false
                   end,
                   fun(Acc) ->
                           %%ct:log("Get more ~p",[ ExpectedSize-byte_size(Acc) ]),
                           receive
                               {ssh_cm, C, {eof,Ch}} when Ch==Ch1 ->
                                   %% ct:log("eof received",[]),
                                   {eof,Acc};

                               {ssh_cm, C, {data,Ch,0,B}} when Ch==Ch1,
                                                               is_binary(B) ->
                                   %% ct:log("(1) Received ~p bytes (total ~p), missing ~p bytes",
                                   %%        [byte_size(B),
                                   %%         byte_size(B)+byte_size(Acc),
                                   %%         2*byte_size(Data)-(byte_size(B)+byte_size(Acc))]),
                                   ssh_connection:adjust_window(C, Ch1, byte_size(B)),
                                   <<Acc/binary, B/binary>>
                           end
                   end,
                   <<>>),

    ExpectedData = <<Data/binary, Data/binary>>,
    case ReceivedData of
        ExpectedData ->
            %% ct:log("Correct data returned",[]),
            %% receive close messages
            loop_until(fun(Left) -> %% ct:log("Expect: ~p",[Left]),
                                    Left == []
                       end,
                       fun([Next|Rest]) ->
                               receive
                                  {ssh_cm,C,Next} -> Rest
                               end 
                       end,
                       [%% Already received: {eof, Ch1},
                        {exit_status,Ch1,0},
                        {closed,Ch1}]
                      ),
            ok;
        _ when is_binary(ReceivedData) ->
            ct:fail("~p bytes echoed but ~p expected", [byte_size(ReceivedData), 2*byte_size(Data)])
    end.

%%--------------------------------------------------------------------
%% Utilities ---------------------------------------------------------
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%%
%% A practical meta function
%% 
loop_until(CondFun, DoFun, Acc) ->
    case CondFun(Acc) of
        true ->
            Acc;
        false ->
            loop_until(CondFun, DoFun, DoFun(Acc))
    end.

%%--------------------------------------------------------------------
%%
%% Exec the Command in the docker.  Add the arguments ExtraSshArg in the
%% ssh command.
%%
%% If Expects is returned, then return 'ok', else return {fail,Msg}.
%% 
exec_from_docker(Config, HostIP, HostPort, Command, Expects, ExtraSshArg) when is_binary(hd(Expects)),
                                                                               is_list(Config) ->
    {DockerIP,DockerPort} = ip_port(Config),
    {ok,C} = ssh:connect(DockerIP, DockerPort,
                         [{user,?USER},
                          {password,?PASSWD},
                          {user_dir, new_dir(Config)},
                          {silently_accept_hosts,true},
                          {user_interaction,false}
                         ]),
    R = exec_from_docker(C, HostIP, HostPort, Command, Expects, ExtraSshArg, Config),
    ssh:close(C),
    R.

exec_from_docker(C, DestIP, DestPort, Command, Expects, ExtraSshArg, Config) when is_binary(hd(Expects)) ->
    ExecCommand =
        lists:concat(
          ["sshpass -p ",?PASSWD," "
           | case proplists:get_value(ssh_version,Config) of
                 "dropbear" ++ _ ->
                     ["dbclient -y -y -p ",DestPort," ",ExtraSshArg," ",iptoa(DestIP)," "];

                 _ -> %% OpenSSH or compatible
                     ["/buildroot/ssh/bin/ssh -o 'CheckHostIP=no' -o 'StrictHostKeyChecking=no' ",
                      ExtraSshArg," -p ",DestPort," ",iptoa(DestIP)," "]
             end]) ++ Command,

    case exec(C, ExecCommand) of
        {ok,{ExitStatus,Result}} = R when ExitStatus == 0 ->
            case binary:match(Result, Expects) of
                nomatch ->
                    ct:log("Result of~n    ~s~nis~n    ~p",[ExecCommand,R]),
                    {fail, "Bad answer"};
                _ ->
                    ok
            end;
        {ok,_} = R ->
            ct:log("Result of~n    ~s~nis~n    ~p",[ExecCommand,R]),
            {fail, "Exit status =/= 0"};
        R ->
            ct:log("Result of~n    ~s~nis~n    ~p",[ExecCommand,R]),
            {fail, "Couldn't login to host"}
    end.


exec(C, Cmd) ->
    %% ct:log("~s",[Cmd]),
    {ok,Ch} = ssh_connection:session_channel(C, 10000),
    success = ssh_connection:exec(C, Ch, Cmd, 10000),
    result_of_exec(C, Ch).


result_of_exec(C, Ch) ->
    result_of_exec(C, Ch, undefined, <<>>).

result_of_exec(C, Ch, ExitStatus, Acc) ->
    receive
        {ssh_cm,C,{closed,Ch}} ->
            %%ct:log("CHAN ~p got *closed*",[Ch]),
            {ok, {ExitStatus, Acc}};

        {ssh_cm,C,{exit_status,Ch,ExStat}} when ExitStatus == undefined ->
            %%ct:log("CHAN ~p got *exit status ~p*",[Ch,ExStat]),
            result_of_exec(C, Ch, ExStat, Acc);

        {ssh_cm,C,{data,Ch,_,Data}=_X} when ExitStatus == undefined ->
            %%ct:log("CHAN ~p got ~p",[Ch,_X]),
            result_of_exec(C, Ch, ExitStatus, <<Acc/binary, Data/binary>>);

        _Other ->
            %%ct:log("OTHER: ~p",[_Other]),
            result_of_exec(C, Ch, ExitStatus, Acc)

    after 5000 ->
            ct:log("NO MORE, received so far:~n~s",[Acc]),
            {error, timeout}
    end.


%%--------------------------------------------------------------------
%%
%% Loop through all {Tag,Alg} pairs in CommonAlgs, call DoTestFun(Tag,Alg) which
%% returns one of {ok,C}, ok, or Other.
%%
%% The chk_all_algos returns 'ok' or {fail,FaledAlgosList}
%% 

chk_all_algos(FunctionName, CommonAlgs, Config, DoTestFun) when is_function(DoTestFun,2) ->
    ct:comment("~p algorithms",[length(CommonAlgs)]),
    %% Check each algorithm
    Failed =
        lists:foldl(
          fun({Tag,Alg}, FailedAlgos) ->
                  %% ct:log("Try ~p",[Alg]),
                  case DoTestFun(Tag,Alg) of
                      {ok,C} ->
                          ssh:close(C),
                          FailedAlgos;
                      ok ->
                          FailedAlgos;
                      Other ->
                          ct:log("FAILED! ~p ~p: ~p",[Tag,Alg,Other]),
                          [{Alg,Other}|FailedAlgos]
                  end
          end, [], CommonAlgs),
    ct:pal("~s", [format_result_table_use_all_algos(FunctionName, Config, CommonAlgs, Failed)]),
    case Failed of
        [] ->
            ok;
        _ ->
            {fail, Failed}
    end.



%%%----------------------------------------------------------------
%%%
%%% Call all Funs as Fun() which returns 'ok', {ok,C} or Other.
%%% do/1 returns 'ok' or the first encountered value that is not
%%% successful.
%%%

do(Funs) ->
    do(Funs, 1).

do([Fun|Funs], N) ->
    case Fun() of
        ok ->
            %% ct:log("Fun ~p ok",[N]),
            do(Funs, N-1);
        {ok,C} ->
            %% ct:log("Fun ~p {ok,C}",[N]),
            ssh:close(C),
            do(Funs, N-1);
        Other ->
            ct:log("Fun ~p FAILED:~n~p",[N, Other]),
            Other
    end;

do([], _) ->
    %% ct:log("All Funs ok",[]),
    ok.

%%--------------------------------------------------------------------
%%
%% Functions to set up local and remote host's and user's keys and directories
%% 

setup_local_hostdir(KeyAlg, Config) ->
    setup_local_hostdir(KeyAlg, new_dir(Config), Config).
setup_local_hostdir(KeyAlg, HostDir, Config) ->
    {ok, {Priv,Publ}} = host_priv_pub_keys(Config, KeyAlg),
    %% Local private and public key
    DstFile = filename:join(HostDir, dst_filename(host,KeyAlg)),
    ok = file:write_file(DstFile,         Priv),
    ok = file:write_file(DstFile++".pub", Publ),
    HostDir.


setup_remote_auth_keys_and_local_priv(KeyAlg, Config) ->
    {IP,Port} = ip_port(Config),
    setup_remote_auth_keys_and_local_priv(KeyAlg, IP, Port, new_dir(Config), Config).

setup_remote_auth_keys_and_local_priv(KeyAlg, UserDir, Config) ->
    {IP,Port} = ip_port(Config),
    setup_remote_auth_keys_and_local_priv(KeyAlg, IP, Port, UserDir, Config).

setup_remote_auth_keys_and_local_priv(KeyAlg, IP, Port, Config) ->
    setup_remote_auth_keys_and_local_priv(KeyAlg, IP, Port, new_dir(Config), Config).

setup_remote_auth_keys_and_local_priv(KeyAlg, IP, Port, UserDir, Config) ->
    {ok, {Priv,Publ}} = user_priv_pub_keys(Config, KeyAlg),
    %% Local private and public keys
    DstFile = filename:join(UserDir, dst_filename(user,KeyAlg)),
    ok = file:write_file(DstFile,         Priv),
    ok = file:write_file(DstFile++".pub", Publ),
    %% Remote auth_methods with public key
    {ok,Ch,Cc} = ssh_sftp:start_channel(IP, Port, [{user,     ?USER  },
                                                   {password, ?PASSWD   },
                                                   {auth_methods, "password"},
                                                   {silently_accept_hosts,true},
                                                   {user_interaction,false}
                                                  ]),
    _ = ssh_sftp:make_dir(Ch, ".ssh"),
    ok = ssh_sftp:write_file(Ch, ".ssh/authorized_keys", Publ),
    ok = ssh_sftp:write_file_info(Ch, ".ssh/authorized_keys",  #file_info{mode=8#700}),
    ok = ssh_sftp:write_file_info(Ch, ".ssh",  #file_info{mode=8#700}),
    ok = ssh_sftp:stop_channel(Ch),
    ok = ssh:close(Cc),
    UserDir.


setup_remote_priv_and_local_auth_keys(KeyAlg, Config) ->
    {IP,Port} = ip_port(Config),
    setup_remote_priv_and_local_auth_keys(KeyAlg, IP, Port, new_dir(Config), Config).

setup_remote_priv_and_local_auth_keys(KeyAlg, UserDir, Config) ->
    {IP,Port} = ip_port(Config),
    setup_remote_priv_and_local_auth_keys(KeyAlg, IP, Port, UserDir, Config).

setup_remote_priv_and_local_auth_keys(KeyAlg, IP, Port, Config) ->
    setup_remote_priv_and_local_auth_keys(KeyAlg, IP, Port, new_dir(Config), Config).

setup_remote_priv_and_local_auth_keys(KeyAlg, IP, Port, UserDir, Config) ->
    {ok, {Priv,Publ}} = user_priv_pub_keys(Config, KeyAlg),
    %% Local auth_methods with public key
    AuthKeyFile = filename:join(UserDir, "authorized_keys"),
    ok = file:write_file(AuthKeyFile, Publ),
    %% Remote private and public key
    {ok,Ch,Cc} = ssh_sftp:start_channel(IP, Port, [{user,     ?USER  },
                                                   {password, ?PASSWD   },
                                                   {auth_methods, "password"},
                                                   {silently_accept_hosts,true},
                                                   {user_interaction,false}
                                                  ]),
    _ = ssh_sftp:make_dir(Ch, ".ssh"),
    DstFile = filename:join(".ssh", dst_filename(user,KeyAlg)),
    ok = ssh_sftp:write_file(Ch, DstFile, Priv),
    ok = ssh_sftp:write_file_info(Ch, DstFile,  #file_info{mode=8#700}),
    ok = ssh_sftp:write_file(Ch, DstFile++".pub", Publ),
    ok = ssh_sftp:write_file_info(Ch, ".ssh",  #file_info{mode=8#700}),
    ok = ssh_sftp:stop_channel(Ch),
    ok = ssh:close(Cc),
    UserDir.

user_priv_pub_keys(Config, KeyAlg) -> priv_pub_keys("users_keys", user, Config, KeyAlg).
host_priv_pub_keys(Config, KeyAlg) -> priv_pub_keys("host_keys",  host, Config, KeyAlg).

priv_pub_keys(KeySubDir, Type, Config, KeyAlg) ->
    KeyDir = filename:join(proplists:get_value(data_dir,Config), KeySubDir),
    {ok,Priv} = file:read_file(filename:join(KeyDir,src_filename(Type,KeyAlg))),
    {ok,Publ} = file:read_file(filename:join(KeyDir,src_filename(Type,KeyAlg)++".pub")),
    {ok, {Priv,Publ}}.


%%%---------------- The default filenames
src_filename(user, 'ssh-rsa'            ) -> "id_rsa";
src_filename(user, 'rsa-sha2-256'       ) -> "id_rsa";
src_filename(user, 'rsa-sha2-512'       ) -> "id_rsa";
src_filename(user, 'ssh-dss'            ) -> "id_dsa";
src_filename(user, 'ecdsa-sha2-nistp256') -> "id_ecdsa256";
src_filename(user, 'ecdsa-sha2-nistp384') -> "id_ecdsa384";
src_filename(user, 'ecdsa-sha2-nistp521') -> "id_ecdsa521";
src_filename(host, 'ssh-rsa'            ) -> "ssh_host_rsa_key";
src_filename(host, 'rsa-sha2-256'       ) -> "ssh_host_rsa_key";
src_filename(host, 'rsa-sha2-512'       ) -> "ssh_host_rsa_key";
src_filename(host, 'ssh-dss'            ) -> "ssh_host_dsa_key";
src_filename(host, 'ecdsa-sha2-nistp256') -> "ssh_host_ecdsa_key256";
src_filename(host, 'ecdsa-sha2-nistp384') -> "ssh_host_ecdsa_key384";
src_filename(host, 'ecdsa-sha2-nistp521') -> "ssh_host_ecdsa_key521".

dst_filename(user, 'ssh-rsa'            ) -> "id_rsa";
dst_filename(user, 'rsa-sha2-256'       ) -> "id_rsa";
dst_filename(user, 'rsa-sha2-512'       ) -> "id_rsa";
dst_filename(user, 'ssh-dss'            ) -> "id_dsa";
dst_filename(user, 'ecdsa-sha2-nistp256') -> "id_ecdsa";
dst_filename(user, 'ecdsa-sha2-nistp384') -> "id_ecdsa";
dst_filename(user, 'ecdsa-sha2-nistp521') -> "id_ecdsa";
dst_filename(host, 'ssh-rsa'            ) -> "ssh_host_rsa_key";
dst_filename(host, 'rsa-sha2-256'       ) -> "ssh_host_rsa_key";
dst_filename(host, 'rsa-sha2-512'       ) -> "ssh_host_rsa_key";
dst_filename(host, 'ssh-dss'            ) -> "ssh_host_dsa_key";
dst_filename(host, 'ecdsa-sha2-nistp256') -> "ssh_host_ecdsa_key";
dst_filename(host, 'ecdsa-sha2-nistp384') -> "ssh_host_ecdsa_key";
dst_filename(host, 'ecdsa-sha2-nistp521') -> "ssh_host_ecdsa_key".


%%--------------------------------------------------------------------
%%
%% Format the result table for chk_all_algos/4
%% 
format_result_table_use_all_algos(FunctionName, Config, CommonAlgs, Failed) ->
    %% Write a nice table with the result
    AlgHead = 'Algorithm',
    AlgWidth = lists:max([length(atom_to_list(A)) || {_,A} <- CommonAlgs]),
    {ResultTable,_} =
        lists:mapfoldl(
          fun({T,A}, Tprev) ->
                  Tag = case T of
                            Tprev -> "";
                            _ -> io_lib:format('~s~n',[T])
                        end,
                  {io_lib:format('~s     ~*s ~s~n',
                                 [Tag, -AlgWidth, A,
                                  case proplists:get_value(A,Failed) of
                                      undefined -> "(ok)";
                                      Err -> io_lib:format("<<<< FAIL <<<< ~p",[Err])
                                  end]),
                   T}
          end, undefined, CommonAlgs),

    Vssh = proplists:get_value(ssh_version,Config,""),
    io_lib:format("~nResults of ~p, Peer version: ~s~n~n"
                  "Tag  ~*s Result~n"
                  "=====~*..=s=======~n~s"
                 ,[FunctionName, Vssh,
                   -AlgWidth, AlgHead,
                   AlgWidth, "", ResultTable]).

%%--------------------------------------------------------------------
%%
%% Docker handling: start_docker/1 and stop_docker/1
%% 
start_docker(Ver) ->
    Cmnd = lists:concat(["docker run -itd --rm -p 1234 ",?DOCKER_PFX,":",Ver]),
    Id0 = os:cmd(Cmnd),
    ct:log("Ver = ~p, Cmnd ~p~n-> ~p",[Ver,Cmnd,Id0]),
    case is_docker_sha(Id0) of
        true ->
            Id = hd(string:tokens(Id0, "\n")),
            IP = ip(Id),
            Port = 1234,
            {ok, {Ver,{IP,Port},Id}};
        false ->
            throw(cant_start_docker)
    end.


stop_docker({_Ver,_,Id}) ->
    Cmnd = lists:concat(["docker kill ",Id]),
    os:cmd(Cmnd).

is_docker_sha(L) ->
    lists:all(fun(C) when $a =< C,C =< $z -> true;
                 (C) when $0 =< C,C =< $9 -> true;
                 ($\n) -> true;
                 (_) -> false
              end, L).

%%--------------------------------------------------------------------
%%
%% Misc docker info functions

ip_port(Config) ->
    {_Ver,{IP,Port},_} = proplists:get_value(id,Config),
    {IP,Port}.

port_mapped_to(Id) ->
    Cmnd = lists:concat(["docker ps --format \"{{.Ports}}\"  --filter id=",Id]),
    [_, PortStr | _] = string:tokens(os:cmd(Cmnd), ":->/"),
    list_to_integer(PortStr).

ip(Id) ->
    Cmnd = lists:concat(["docker inspect --format='{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' ",
			 Id]),
    IPstr0 = os:cmd(Cmnd),
    ct:log("Cmnd ~p~n-> ~p",[Cmnd,IPstr0]),
    IPstr = hd(string:tokens(IPstr0, "\n")),
    {ok,IP} = inet:parse_address(IPstr),
    IP.

%%--------------------------------------------------------------------
%%
%%  Normalize the host returned from ssh_test_lib

iptoa({0,0,0,0}) -> inet_parse:ntoa(host_ip());
iptoa(IP) -> inet_parse:ntoa(IP).

host_ip() ->
    {ok,Name} = inet:gethostname(),
    {ok,#hostent{h_addr_list = [IP|_]}} = inet_res:gethostbyname(Name),
    IP.

%%--------------------------------------------------------------------
%%
%% Create a new fresh directory or clear an existing one
%% 

new_dir(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    SubDirName = integer_to_list(erlang:system_time()),
    Dir = filename:join(PrivDir, SubDirName),
    case file:read_file_info(Dir) of
        {error,enoent} ->
            ok = file:make_dir(Dir),
            Dir;
        _ ->
            timer:sleep(25),
            new_dir(Config)
    end.

clear_dir(Dir) ->
    delete_all_contents(Dir),
    {ok,[]} = file:list_dir(Dir),
    Dir.

delete_all_contents(Dir) ->
    {ok,Fs} = file:list_dir(Dir),
    lists:map(fun(F0) ->
                      F = filename:join(Dir, F0),
                      case filelib:is_file(F) of
                          true ->
                              file:delete(F);
                          false ->
                              case filelib:is_dir(F) of
                                  true ->
                                      delete_all_contents(F),
                                      file:del_dir(F);
                                  false ->
                                      ct:log("Neither file nor dir: ~p",[F])
                              end
                      end
              end, Fs).

%%--------------------------------------------------------------------
%%
%% Find the intersection of algoritms for otp ssh and the docker ssh.
%% Returns {ok, ServerHello, Server, ClientHello, Client} where Server are the algorithms common
%% with the docker server and analogous for Client.
%%
%% Client may be undefined if no usable client is found.
%%
%% Both Server and Client are lists of {Tag,AlgName}.
%% 

common_algs(Config, IP, Port) ->
    case remote_server_algs(IP, Port) of
        {ok, {ServerHello, RemoteServerKexInit}} ->
            RemoteServerAlgs = kexint_msg2default_algorithms(RemoteServerKexInit),
            Server = find_common_algs(RemoteServerAlgs,
                                      use_algorithms(ServerHello)),
            ct:log("Remote server:~n~p~n~p",[ServerHello, RemoteServerAlgs]),
            case remote_client_algs(Config) of
                {ok,{ClientHello,RemoteClientKexInit}} ->
                    RemoteClientAlgs = kexint_msg2default_algorithms(RemoteClientKexInit),
                    Client = find_common_algs(RemoteClientAlgs,
                                              use_algorithms(ClientHello)),
                    ct:log("Remote client:~n~p~n~p",[ClientHello, RemoteClientAlgs]),
                    {ok, ServerHello, Server, ClientHello, Client};
                {error,_} =TO ->
                    ct:log("Remote client algs can't be found: ~p",[TO]),
                    {ok, ServerHello, Server, undefined, undefined};
                Other ->
                    Other
            end;
        Other ->
            Other
    end.


chk_hellos(Hs, Str) ->
    lists:foldl(
      fun(H, Acc) ->
              try binary:split(H, <<"-">>, [global])
              of
                  %% [<<"SSH">>,<<"2.0">>|_] ->
                  %%     Acc;
                  [<<"SSH">>,OldVer = <<"1.",_/binary>>|_] ->
                      io_lib:format("~s, Old SSH ver ~s",[Acc,OldVer]);
                  _ ->
                      Acc
              catch
                  _:_ ->
                      Acc
              end
      end, Str, Hs).
              

find_common_algs(Remote, Local) ->
    [{T,V} || {T,Vs} <- ssh_test_lib:extract_algos(
                          ssh_test_lib:intersection(Remote,
                                                    Local)),
              V <- Vs].


use_algorithms(RemoteHelloBin) ->
    MyAlgos = ssh:chk_algos_opts(
                [{modify_algorithms,
                  [{append,
                    [{kex,['diffie-hellman-group1-sha1']}
                    ]}
                  ]}
                ]),
    ssh_transport:adjust_algs_for_peer_version(binary_to_list(RemoteHelloBin)++"\r\n",
                                               MyAlgos).

kexint_msg2default_algorithms(#ssh_msg_kexinit{kex_algorithms = Kex,
                                               server_host_key_algorithms = PubKey,
                                               encryption_algorithms_client_to_server = CipherC2S,
                                               encryption_algorithms_server_to_client = CipherS2C,
                                               mac_algorithms_client_to_server = MacC2S,
                                               mac_algorithms_server_to_client = MacS2C,
                                               compression_algorithms_client_to_server = CompC2S,
                                               compression_algorithms_server_to_client = CompS2C
                                              }) ->
    [{kex,         ssh_test_lib:to_atoms(Kex)},
     {public_key,  ssh_test_lib:to_atoms(PubKey)},
     {cipher,      [{client2server,ssh_test_lib:to_atoms(CipherC2S)},
                    {server2client,ssh_test_lib:to_atoms(CipherS2C)}]},
     {mac,         [{client2server,ssh_test_lib:to_atoms(MacC2S)},
                    {server2client,ssh_test_lib:to_atoms(MacS2C)}]},
     {compression, [{client2server,ssh_test_lib:to_atoms(CompC2S)},
                    {server2client,ssh_test_lib:to_atoms(CompS2C)}]}].


%%--------------------------------------------------------------------
%%
%% Find the algorithms supported by the remote server
%%
%% Connect with tcp to the server, send a hello and read the returned
%% server hello and kexinit message.
%% 
remote_server_algs(IP, Port) ->
    case try_gen_tcp_connect(IP, Port, 5) of
        {ok,S} ->
            ok = gen_tcp:send(S, "SSH-2.0-CheckAlgs\r\n"),
            receive_hello(S);
        {error,Error} ->
            {error,Error}
    end.

try_gen_tcp_connect(IP, Port, N) when N>0 ->
    case gen_tcp:connect(IP, Port, [binary]) of
        {ok,S} ->
            {ok,S};
        {error,_Error} when N>1 ->
            receive after 1000 -> ok end,
            try_gen_tcp_connect(IP, Port, N-1);
        {error,Error} ->
            {error,Error}
    end;
try_gen_tcp_connect(_, _, _) ->
    {error, "No contact"}.


%%--------------------------------------------------------------------
%%
%% Find the algorithms supported by the remote client
%%
%% Set up a fake ssh server and make the remote client connect to it. Use
%% hello message and the kexinit message.
%% 
remote_client_algs(Config) ->
    Parent = self(),
    Ref = make_ref(),
    spawn(
      fun() ->
              {ok,Sl} = gen_tcp:listen(0, [binary]),
              {ok,{IP,Port}} = inet:sockname(Sl),
              Parent ! {addr,Ref,IP,Port},
              {ok,S} = gen_tcp:accept(Sl),
              ok = gen_tcp:send(S, "SSH-2.0-CheckAlgs\r\n"),
              Parent ! {Ref,receive_hello(S)}
      end),
    receive
        {addr,Ref,IP,Port} ->
            spawn(fun() ->
                          exec_from_docker(Config, IP, Port,
                                           "howdy.\r\n",
                                           [<<"howdy">>],
                                           "")
                  end),
            receive
                {Ref, Result} ->
                    Result
            after 5000 ->
                    {error, {timeout,2}}
            end
    after 5000 ->
            {error, {timeout,1}}
    end.


%%% Receive a few packets from the remote server or client and find what is supported:

receive_hello(S) ->
    try
        receive_hello(S, <<>>)
    of
        Result ->
            Result
    catch
        Class:Error ->
            ST = erlang:get_stacktrace(),
            {error, {Class,Error,ST}}
    end.
        

receive_hello(S, Ack) ->
    %% The Ack is to collect bytes until the full message is received
    receive
        {tcp, S, Bin0} when is_binary(Bin0) ->
            case binary:split(<<Ack/binary, Bin0/binary>>, [<<"\r\n">>,<<"\r">>,<<"\n">>]) of
                [Hello = <<"SSH-2.0-",_/binary>>, NextPacket] ->
                    %% ct:log("Got 2.0 hello (~p), ~p bytes to next msg",[Hello,size(NextPacket)]),
                    {ok, {Hello, receive_kexinit(S, NextPacket)}};

                [Hello = <<"SSH-1.99-",_/binary>>, NextPacket] ->
                    %% ct:log("Got 1.99 hello (~p), ~p bytes to next msg",[Hello,size(NextPacket)]),
                    {ok, {Hello, receive_kexinit(S, NextPacket)}};

                [Bin] when size(Bin) < 256 ->
                    %% ct:log("Got part of hello (~p chars):~n~s~n~s",[size(Bin),Bin,
                    %%                                                 [io_lib:format('~2.16.0b ',[C])
                    %%                                                  || C <- binary_to_list(Bin0)
                    %%                                                 ]
                    %%                                                ]),
                    receive_hello(S, Bin0);

                _ ->
                    ct:log("Bad hello string (line ~p, ~p chars):~n~s~n~s",[?LINE,size(Bin0),Bin0,
                                                                  [io_lib:format('~2.16.0b ',[C])
                                                                   || C <- binary_to_list(Bin0)
                                                                  ]
                                                                 ]),
                    ct:fail("Bad hello string received")
            end;
        Other ->
            ct:log("Bad hello string (line ~p):~n~p",[?LINE,Other]),
            ct:fail("Bad hello string received")

    after 10000 ->
            ct:log("Timeout waiting for hello!~n~s",[Ack]),
            throw(timeout)
    end.


receive_kexinit(_S, <<PacketLen:32, PaddingLen:8, PayloadAndPadding/binary>>)
  when PacketLen < 5000, % heuristic max len to stop huge attempts if packet decodeing get out of sync
       size(PayloadAndPadding) >= (PacketLen-1) % Need more bytes?
       ->
    ct:log("Has all ~p packet bytes",[PacketLen]),
    PayloadLen = PacketLen - PaddingLen - 1,
    <<Payload:PayloadLen/binary, _Padding:PaddingLen/binary>> = PayloadAndPadding,
    ssh_message:decode(Payload);

receive_kexinit(S, Ack) ->
    ct:log("Has ~p bytes, need more",[size(Ack)]),
    receive
        {tcp, S, Bin0} when is_binary(Bin0) ->
            receive_kexinit(S, <<Ack/binary, Bin0/binary>>);
        Other ->
            ct:log("Bad hello string (line ~p):~n~p",[?LINE,Other]),
            ct:fail("Bad hello string received")

    after 10000 ->
            ct:log("Timeout waiting for kexinit!~n~s",[Ack]),
            throw(timeout)
    end.

%%%----------------------------------------------------------------
%%% Test of sftp from the OpenSSH client side
%%%

sftp_tests_erl_server(Config, ServerIP, ServerPort, ServerRootDir, UserDir) ->
    try
        Cmnds = prepare_local_directory(ServerRootDir),
        call_sftp_in_docker(Config, ServerIP, ServerPort, Cmnds, UserDir),
        check_local_directory(ServerRootDir)
    catch
        Class:Error ->
            ST = erlang:get_stacktrace(),
            {error, {Class,Error,ST}}
    end.


prepare_local_directory(ServerRootDir) ->
    file:write_file(filename:join(ServerRootDir,"tst1"),
                    <<"Some test text">>
                   ),
    ["get tst1",
     "put tst1 tst2",
     "put tst1 tst3",
     "rename tst1 ex_tst1",
     "rm tst3",
     "mkdir mydir",
     "cd mydir",
     "put tst1 file_1",
     "put tst1 unreadable_file",
     "chmod 222 unreadable_file",
     "exit"].

check_local_directory(ServerRootDir) ->
    case lists:sort(ok(file:list_dir(ServerRootDir)) -- [".",".."]) of
        ["ex_tst1","mydir","tst2"] ->
            {ok,Expect} = file:read_file(filename:join(ServerRootDir,"ex_tst1")),
            case file:read_file(filename:join(ServerRootDir,"tst2")) of
                {ok,Expect} ->
                    case lists:sort(ok(file:list_dir(filename:join(ServerRootDir,"mydir"))) -- [".",".."]) of
                        ["file_1","unreadable_file"] ->
                            case file:read_file(filename:join([ServerRootDir,"mydir","file_1"])) of
                                {ok,Expect} ->
                                    case file:read_file(filename:join([ServerRootDir,"mydir","unreadable_file"])) of
                                        {error,_} ->
                                            ok;
                                        {ok,_} ->
                                            {error, {could_read_unreadable,"mydir/unreadable_file"}}
                                    end;
                                {ok,Other} ->
                                    ct:log("file_1:~n~s~nExpected:~n~s",[Other,Expect]),
                                    {error, {bad_contents_in_file,"mydir/file_1"}}
                            end;
                        Other ->
                            ct:log("Directory ~s~n~p",[filename:join(ServerRootDir,"mydir"),Other]),
                            {error,{bad_dir_contents,"mydir"}}
                    end;
                {ok,Other} ->
                    ct:log("tst2:~n~s~nExpected:~n~s",[Other,Expect]),
                    {error, {bad_contents_in_file,"tst2"}}
            end;
        ["tst1"] ->
            {error,{missing_file,"tst2"}};
        Other ->
            ct:log("Directory ~s~n~p",[ServerRootDir,Other]),
            {error,{bad_dir_contents,"/"}}
    end.

call_sftp_in_docker(Config, ServerIP, ServerPort, Cmnds, UserDir) ->
    {DockerIP,DockerPort} = ip_port(Config),
    {ok,C} = ssh:connect(DockerIP, DockerPort,
                         [{user,?USER},
                          {password,?PASSWD},
                          {user_dir, UserDir},
                          {silently_accept_hosts,true},
                          {user_interaction,false}
                         ]),

    %% Make commands for "expect" in the docker:
    PreExpectCmnds = ["spawn /buildroot/ssh/bin/sftp -oPort="++integer_to_list(ServerPort)++
                          " -oCheckHostIP=no -oStrictHostKeyChecking=no " ++
                          iptoa(ServerIP)++"\n"
                     ],
    PostExpectCmnds= [],
    ExpectCmnds = 
        PreExpectCmnds ++
        ["expect \"sftp>\" {send \""++Cmnd++"\n\"}\n" || Cmnd <- Cmnds] ++
        PostExpectCmnds,
    
    %% Make an commands file in the docker
    {ok,Ch} = ssh_sftp:start_channel(C, [{timeout,10000}]),
    ok = ssh_sftp:write_file(Ch, "commands", erlang:iolist_to_binary(ExpectCmnds)),
    ok = ssh_sftp:stop_channel(Ch),

    %% Call expect in the docker
    {ok, Ch1} = ssh_connection:session_channel(C, infinity),
    Kex1 = renegotiate_test(init, C),
    success = ssh_connection:exec(C, Ch1, "expect commands", infinity),

    renegotiate_test(Kex1, C),
    recv_log_msgs(C, Ch1),

    %% Done.
    ssh:close(C).

recv_log_msgs(C, Ch) ->
    receive
        {ssh_cm,C,{closed,Ch}} ->
            %% ct:log("Channel closed ~p",[{closed,1}]),
            ok;
        {ssh_cm,C,{data,Ch,1,Msg}} ->
            ct:log("*** ERROR from docker:~n~s",[Msg]),
            recv_log_msgs(C, Ch);
        {ssh_cm,C,_Msg} ->
            %% ct:log("Got ~p",[_Msg]),
            recv_log_msgs(C, Ch)
    end.

%%%----------------------------------------------------------------
%%%----------------------------------------------------------------
%%%
%%% Tests from the Erlang client side
%%%
%%%----------------------------------------------------------------
%%%----------------------------------------------------------------
test_erl_client_reneg({ok,C}, Spec) ->
    %% Start the test processes on the connection C:
    Parent = self(),
    Pids = [spawn(
              fun() ->
                      Parent ! {self(), TestType, Id, one_test_erl_client(TestType,Id,C)}
              end
             ) 
            || {TestType,N} <- Spec,
               Id <- lists:seq(1,N)],

    Kex1 = renegotiate_test(init, C),

    %% Collect the results:
    case lists:filter(
           fun(R) -> R=/=ok end,
           [receive
                {Pid,_TestType,_Id,ok} ->
                    %% ct:log("Test ~p:~p passed!", [_TestType,_Id]),
                    ok;
                {Pid,TestType,Id,OtherResult} ->
                    ct:log("~p:~p ~p ~p~n~p",[?MODULE,?LINE,TestType,Id,OtherResult]),
                    {error,TestType,Id}
            end || Pid <- Pids])
    of
        [] ->
            renegotiate_test(Kex1, C),
            {ok,C};
        Other ->
            renegotiate_test(Kex1, C),
            Other
    end;

test_erl_client_reneg(Error, _) ->
    Error.


one_test_erl_client(exec, Id, C) ->
    {ok, Ch} = ssh_connection:session_channel(C, infinity),
    success = ssh_connection:exec(C, Ch, "echo Hi there", 5000),
    case loop_until(fun({eof,_}) -> true;
                       (_      ) -> false
                    end,
                    fun(Acc) ->
                            receive
                                {ssh_cm, C, {eof,Ch}} ->
                                    {eof,Acc};
                                {ssh_cm, C, {data,Ch,0,B}} when is_binary(B) ->
                                    <<Acc/binary, B/binary>>
                            end
                    end,
                    <<>>) of
        {eof,<<"Hi there\n">>} ->
            ok;
        Other ->
            ct:pal("exec Got other ~p", [Other]),
            {error, {exec,Id,bad_msg,Other,undefined}}
    end;

one_test_erl_client(no_subsyst, Id, C) ->
    {ok, Ch} = ssh_connection:session_channel(C, infinity),
    case ssh_connection:subsystem(C, Ch, "foo", infinity) of
        failure ->
            ok;
        Other ->
            ct:pal("no_subsyst Got other ~p", [Other]),
            {error, {no_subsyst,Id,bad_ret,Other,undefined}}
    end;

one_test_erl_client(setenv, Id, C) ->
    {ok, Ch} = ssh_connection:session_channel(C, infinity),
    Var = "ENV_TEST",
    Value = lists:concat(["env_test_",Id,"_",erlang:system_time()]),
    Env = case ssh_connection:setenv(C, Ch, Var, Value, infinity) of
	      success -> binary_to_list(Value++"\n");
	      failure -> <<"\n">>
	  end,
    success = ssh_connection:exec(C, Ch, "echo $"++Var, 5000),
    case loop_until(fun({eof,_}) -> true;
                       (_      ) -> false
                    end,
                    fun(Acc) ->
                            receive
                                {ssh_cm, C, {eof,Ch}} ->
                                    {eof,Acc};
                                {ssh_cm, C, {data,Ch,0,B}} when is_binary(B) ->
                                    <<Acc/binary, B/binary>>
                            end
                    end,
                    <<>>) of
        {eof,Env} ->
            ok;
        Other ->
            ct:pal("setenv Got other ~p", [Other]),
            {error, {setenv,Id,bad_msg,Other,undefined}}
    end;

one_test_erl_client(SFTP, Id, C) when SFTP==sftp ; SFTP==sftp_async ->
    try
        {ok,Ch} = ssh_sftp:start_channel(C, [{timeout,10000}]),
        %% A new fresh name of a new file tree:
        RootDir = lists:concat(["r_",Id,"_",erlang:system_time()]),
        %% Check that it does not exist:
        false = lists:member(RootDir, ok(ssh_sftp:list_dir(Ch, "."))),
        %% Create it:
        ok = ssh_sftp:make_dir(Ch, RootDir),
        {ok, #file_info{type=directory, access=read_write}} = ssh_sftp:read_file_info(Ch, RootDir),
        R = do_sftp_tests_erl_client(SFTP, C, Ch, Id, RootDir),
        catch ssh_sftp:stop_channel(Ch),
        R
    catch
        Class:Error ->
            ST = erlang:get_stacktrace(),
            {error, {SFTP,Id,Class,Error,ST}}
    end.



do_sftp_tests_erl_client(sftp_async, _C, Ch, _Id, RootDir) ->
    FileName1 = "boring_name",
    F1 = filename:join(RootDir, FileName1),
    %% Open a new handle and start writing:
    {ok,Handle1} = ssh_sftp:open(Ch, F1, [write,binary]),
    {async,Aref1} = ssh_sftp:awrite(Ch, Handle1, <<0:250000/unsigned-unit:8>>),
    wait_for_async_result(Aref1);

do_sftp_tests_erl_client(sftp, _C, Ch, _Id, RootDir) ->
    FileName0 = "f0",
    F0 = filename:join(RootDir, FileName0),

    %% Create and write a file:
    ok = ssh_sftp:write_file(Ch,
                             F0 = filename:join(RootDir, FileName0),
                             Data0 = mkbin(1234,240)),
    {ok,Data0} = ssh_sftp:read_file(Ch, F0),
    {ok, #file_info{type=regular, access=read_write, size=1234}} = ssh_sftp:read_file_info(Ch, F0),

    %% Re-write:
    {ok,Handle0} = ssh_sftp:open(Ch, F0, [write,read,binary]),
    ok = ssh_sftp:pwrite(Ch, Handle0, 16, Data0_1=mkbin(10,255)),

    <<B1:16/binary, _:10/binary, B2:(1234-26)/binary>> = Data0,
    FileContents = <<B1:16/binary, Data0_1:10/binary, B2:(1234-26)/binary>>,

    <<_:1/binary, Part:25/binary, _/binary>> = FileContents,
    {ok, Part} = ssh_sftp:pread(Ch, Handle0, 1, 25),

    %% Check:
    {ok, FileContents} = ssh_sftp:pread(Ch, Handle0, 0, 1234),
    ok = ssh_sftp:close(Ch, Handle0),

    %% Check in another way:
    {ok, FileContents} = ssh_sftp:read_file(Ch, F0),

    %% Remove write access rights and check that it can't be written:
    ok = ssh_sftp:write_file_info(Ch, F0, #file_info{mode=8#400}), %read}),
    {ok, #file_info{type=regular, access=read}} = ssh_sftp:read_file_info(Ch, F0),
    {error,permission_denied} = ssh_sftp:write_file(Ch, F0, mkbin(10,14)),

    %% Test deletion of file and dir:
    [FileName0] = ok(ssh_sftp:list_dir(Ch, RootDir)) -- [".", ".."],
    ok = ssh_sftp:delete(Ch, F0),
    [] = ok(ssh_sftp:list_dir(Ch, RootDir)) -- [".", ".."],
    ok = ssh_sftp:del_dir(Ch, RootDir),
    false = lists:member(RootDir, ok(ssh_sftp:list_dir(Ch, "."))),
    ok.


wait_for_async_result(Aref) ->
    receive
        {async_reply, Aref, Result} ->
            Result
    after
        60000 ->
            timeout
    end.


mkbin(Size, Byte) ->
    list_to_binary(lists:duplicate(Size,Byte)).

ok({ok,X}) -> X.
    
%%%----------------------------------------------------------------
renegotiate_test(init, ConnectionRef) ->
    Kex1 = ssh_test_lib:get_kex_init(ConnectionRef),
    ssh_connection_handler:renegotiate(ConnectionRef),
    %%ct:log("Renegotiate test initiated!",[]),
    Kex1;

renegotiate_test(Kex1, ConnectionRef) ->
    case ssh_test_lib:get_kex_init(ConnectionRef) of
        Kex1 ->
            ct:log("Renegotiate test failed, Kex1 == Kex2!",[]),
            error(renegotiate_failed);
        _ ->
            %% ct:log("Renegotiate test passed!",[]),
            ok
    end.
