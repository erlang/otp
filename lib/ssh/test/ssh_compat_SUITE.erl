%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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
-define(PWD, "foobar").
-define(DOCKER_PFX, "ssh_compat_suite-ssh").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [%%{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,40}}].

all() ->
    [{group,G} || G <- vers()].

groups() ->
    [{G, [], tests()} || G <- vers()].

tests() ->
    [login_with_password_otp_is_client,
     login_with_password_otp_is_server,
     login_with_keyboard_interactive_otp_is_client,
     login_with_keyboard_interactive_otp_is_server,
     login_with_all_public_keys_otp_is_client,
     login_with_all_public_keys_otp_is_server,
     all_algorithms_otp_is_client,
     all_algorithms_otp_is_server
    ].



vers() ->
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
    Config.



init_per_group(G, Config) ->
    case lists:member(G, vers()) of
	true ->
            try start_docker(G) of
                {ok,ID} ->
                    ct:log("==> ~p",[G]),
                    [Vssh|VsslRest] = string:tokens(atom_to_list(G), "-"),
                    Vssl = lists:flatten(lists:join($-,VsslRest)),
                    ct:comment("+++ ~s + ~s +++",[Vssh,Vssl]),
                    %% Find the algorithms that both client and server supports:
                    {IP,Port} = ip_port([{id,ID}]),
                    try common_algs([{id,ID}|Config], IP, Port) of
                        {ok, RemoteServerCommon, RemoteClientCommon} ->
                            [{ssh_version,Vssh},{ssl_version,Vssl},
                             {id,ID},
                             {common_server_algs,RemoteServerCommon},
                             {common_client_algs,RemoteClientCommon}
                             |Config];
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
	    Config
    end.

end_per_group(_, Config) ->
    catch stop_docker(proplists:get_value(id,Config)),
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
login_with_password_otp_is_client(Config) ->
    {IP,Port} = ip_port(Config),
    {ok,C} = ssh:connect(IP, Port, [{auth_methods,"password"},
                                    {user,?USER},
                                    {password,?PWD},
				    {user_dir, new_dir(Config)},
				    {silently_accept_hosts,true},
                                    {user_interaction,false}
				   ]),
    ssh:close(C).

%%--------------------------------------------------------------------
login_with_password_otp_is_server(Config) ->
    {Server, Host, HostPort} =
        ssh_test_lib:daemon(0,
                            [{auth_methods,"password"},
                             {system_dir, setup_local_hostdir('ssh-rsa',Config)},
                             {user_dir, new_dir(Config)},
                             {user_passwords, [{?USER,?PWD}]},
                             {failfun, fun ssh_test_lib:failfun/2}
                            ]),
    R = exec_from_docker(Config, Host, HostPort,
                         "'lists:concat([\"Answer=\",1+2]).\r\n'",
                         [<<"Answer=3">>],
                         ""),
    ssh:stop_daemon(Server),
    R.

%%--------------------------------------------------------------------
login_with_keyboard_interactive_otp_is_client(Config) ->
    {DockerIP,DockerPort} = ip_port(Config),
    {ok,C} = ssh:connect(DockerIP, DockerPort,
                         [{auth_methods,"keyboard-interactive"},
                          {user,?USER},
                          {password,?PWD},
                          {user_dir, new_dir(Config)},
                          {silently_accept_hosts,true},
                          {user_interaction,false}
                         ]),
    ssh:close(C).

%%--------------------------------------------------------------------
login_with_keyboard_interactive_otp_is_server(Config) ->
    {Server, Host, HostPort} =
        ssh_test_lib:daemon(0,
                            [{auth_methods,"keyboard-interactive"},
                             {system_dir, setup_local_hostdir('ssh-rsa',Config)},
                             {user_dir, new_dir(Config)},
                             {user_passwords, [{?USER,?PWD}]},
                             {failfun, fun ssh_test_lib:failfun/2}
                            ]),
    R = exec_from_docker(Config, Host, HostPort,
                         "'lists:concat([\"Answer=\",1+3]).\r\n'",
                         [<<"Answer=4">>],
                         ""),
    ssh:stop_daemon(Server),
    R.

%%--------------------------------------------------------------------
login_with_all_public_keys_otp_is_client(Config) ->
    CommonAlgs = [{public_key_from_host,A}
                  || {public_key,A} <- proplists:get_value(common_server_algs, Config)],
    {DockerIP,DockerPort} = ip_port(Config),
    chk_all_algos(CommonAlgs, Config,
                  fun(_Tag,Alg) ->
                          ssh:connect(DockerIP, DockerPort,
                                      [{auth_methods, "publickey"},
                                       {user, ?USER},
                                       {user_dir, setup_remote_auth_keys_and_local_priv(Alg, Config)},
                                       {silently_accept_hosts,true},
                                       {user_interaction,false}
                                      ])
                  end).

%%--------------------------------------------------------------------
login_with_all_public_keys_otp_is_server(Config) ->
    CommonAlgs = [{public_key_to_host,A}
                  || {public_key,A} <- proplists:get_value(common_client_algs, Config)],
    UserDir = new_dir(Config),
    {Server, Host, HostPort} =
        ssh_test_lib:daemon(0,
                            [{auth_methods, "publickey"},
                             {system_dir,  setup_local_hostdir('ssh-rsa',Config)},
                             {user_dir, UserDir},
                             {user_passwords, [{?USER,?PWD}]},
                             {failfun, fun ssh_test_lib:failfun/2}
                            ]),

    R = chk_all_algos(CommonAlgs, Config,
                      fun(_Tag,Alg) ->
                              setup_remote_priv_and_local_auth_keys(Alg, clear_dir(UserDir), Config),
                              exec_from_docker(Config, Host, HostPort,
                                               "'lists:concat([\"Answer=\",1+4]).\r\n'",
                                               [<<"Answer=5">>],
                                               "")
                      end),
    ssh:stop_daemon(Server),
    R.

%%--------------------------------------------------------------------
all_algorithms_otp_is_client(Config) ->
    CommonAlgs = proplists:get_value(common_server_algs, Config),
    {IP,Port} = ip_port(Config),
    chk_all_algos(CommonAlgs, Config,
                  fun(Tag, Alg) ->
                          ssh:connect(IP, Port, [{user,?USER},
                                                 {password,?PWD},
                                                 {auth_methods, "password"},
                                                 {user_dir, new_dir(Config)},
                                                 {preferred_algorithms, [{Tag,[Alg]}]},
                                                 {silently_accept_hosts,true},
                                                 {user_interaction,false}
                                                ])
                  end).

%%--------------------------------------------------------------------
all_algorithms_otp_is_server(Config) ->
    CommonAlgs = proplists:get_value(common_client_algs, Config),
    UserDir = setup_remote_priv_and_local_auth_keys('ssh-rsa', Config),
    chk_all_algos(CommonAlgs, Config,
                  fun(Tag,Alg) ->
                          HostKeyAlg = case Tag of
                                           public_key -> Alg;
                                           _ -> 'ssh-rsa'
                                       end,
                          {Server, Host, HostPort} =
                              ssh_test_lib:daemon(0,
                                                  [{preferred_algorithms, [{Tag,[Alg]}]},
                                                   {system_dir, setup_local_hostdir(HostKeyAlg, Config)},
                                                   {user_dir, UserDir},
                                                   {user_passwords, [{?USER,?PWD}]},
                                                   {failfun, fun ssh_test_lib:failfun/2}
                                                  ]),
                          R = exec_from_docker(Config, Host, HostPort,
                                               "hi_there.\r\n",
                                               [<<"hi_there">>],
                                               ""),
                          ssh:stop_daemon(Server),
                          R
                  end).

%%--------------------------------------------------------------------
%% Utilities ---------------------------------------------------------
%%--------------------------------------------------------------------
exec_from_docker(WhatEver, {0,0,0,0}, HostPort, Command, Expects, ExtraSshArg) ->
    exec_from_docker(WhatEver, host_ip(), HostPort, Command, Expects, ExtraSshArg);

exec_from_docker(Config, HostIP, HostPort, Command, Expects, ExtraSshArg) when is_binary(hd(Expects)),
                                                                               is_list(Config) ->
    {DockerIP,DockerPort} = ip_port(Config),
    {ok,C} = ssh:connect(DockerIP, DockerPort,
                         [{user,?USER},
                          {password,?PWD},
                          {user_dir, new_dir(Config)},
                          {silently_accept_hosts,true},
                          {user_interaction,false}
                         ]),
    R = exec_from_docker(C, HostIP, HostPort, Command, Expects, ExtraSshArg),
    ssh:close(C),
    R;

exec_from_docker(C, HostIP, HostPort, Command, Expects, ExtraSshArg) when is_binary(hd(Expects)) ->
    SSH_from_docker =
        lists:concat(["sshpass -p ",?PWD," ",
                      "/buildroot/ssh/bin/ssh -p ",HostPort," -o 'CheckHostIP=no' -o 'StrictHostKeyChecking=no' ",
                      ExtraSshArg," ",
                      inet_parse:ntoa(HostIP)," "
                     ]),
    ExecCommand = SSH_from_docker ++ Command,
    R = exec(C, ExecCommand),
    case R of
        {ok,{ExitStatus,Result}} when ExitStatus == 0 ->
            case binary:match(Result, Expects) of
                nomatch ->
                    ct:log("Result of~n    ~s~nis~n    ~p",[ExecCommand,R]),
                    {fail, "Bad answer"};
                _ ->
                    ok
            end;
        {ok,_} ->
            ct:log("Result of~n    ~s~nis~n    ~p",[ExecCommand,R]),
            {fail, "Exit status =/= 0"};
        _ ->
            ct:log("Result of~n    ~s~nis~n    ~p",[ExecCommand,R]),
            {fail, "Couldn't login to host"}
    end.




exec(C, Cmd) ->
    ct:log("~s",[Cmd]),
    {ok,Ch} = ssh_connection:session_channel(C, 10000),
    success = ssh_connection:exec(C, Ch, Cmd, 10000),
    exec_result(C, Ch).


exec_result(C, Ch) ->
    exec_result(C, Ch, undefined, <<>>).

exec_result(C, Ch, ExitStatus, Acc) ->
    receive
        {ssh_cm,C,{closed,Ch}} ->
            %%ct:log("CHAN ~p got *closed*",[Ch]),
            {ok, {ExitStatus, Acc}};

        {ssh_cm,C,{exit_status,Ch,ExStat}} when ExitStatus == undefined ->
            %%ct:log("CHAN ~p got *exit status ~p*",[Ch,ExStat]),
            exec_result(C, Ch, ExStat, Acc);

        {ssh_cm,C,{data,Ch,_,Data}=_X} when ExitStatus == undefined ->
            %%ct:log("CHAN ~p got ~p",[Ch,_X]),
            exec_result(C, Ch, ExitStatus, <<Acc/binary, Data/binary>>);

        _Other ->
            %%ct:log("OTHER: ~p",[_Other]),
            exec_result(C, Ch, ExitStatus, Acc)

    after 5000 ->
            %%ct:log("NO MORE, received so far:~n~s",[Acc]),
            {error, timeout}
    end.


chk_all_algos(CommonAlgs, Config, DoTestFun) when is_function(DoTestFun,2) ->
    ct:comment("~p algorithms",[length(CommonAlgs)]),
    %% Check each algorithm
    Failed =
        lists:foldl(
          fun({Tag,Alg}, FailedAlgos) ->
                  ct:log("Try ~p",[Alg]),
                  case DoTestFun(Tag,Alg) of
                      {ok,C} ->
                          ssh:close(C),
                          FailedAlgos;
                      ok ->
                          FailedAlgos;
                      Other ->
                          ct:log("FAILED! ~p ~p: ~p",[Tag,Alg,Other]),
                          [Alg|FailedAlgos]
                  end
          end, [], CommonAlgs),
    ct:pal("~s", [format_result_table_use_all_algos(Config, CommonAlgs, Failed)]),
    case Failed of
        [] ->
            ok;
        _ ->
            {fail, Failed}
    end.

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
                                                   {password, ?PWD   },
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
                                                   {password, ?PWD   },
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


format_result_table_use_all_algos(Config, CommonAlgs, Failed) ->
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
                                  case lists:member(A,Failed) of
                                      true -> "<<<< FAIL <<<<";
                                      false-> "(ok)"
                                  end]),
                   T}
          end, undefined, CommonAlgs),

    Vssh = proplists:get_value(ssh_version,Config,""),
    Vssl = proplists:get_value(ssl_version,Config,""),
    io_lib:format("~nResults, Peer versions: ~s and ~s~n"
                  "Tag  ~*s Result~n"
                  "=====~*..=s=======~n~s"
                 ,[Vssh,Vssl,
                   -AlgWidth,AlgHead,
                   AlgWidth, "", ResultTable]).


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

common_algs(Config, IP, Port) ->
    case remote_server_algs(IP, Port) of
        {ok, {RemoteHelloBin, RemoteServerKexInit}} ->
            case remote_client_algs(Config) of
                {ok,{_Hello,RemoteClientKexInit}} ->
                    RemoteServerAlgs = kexint_msg2default_algorithms(RemoteServerKexInit),
                    Server = find_common_algs(RemoteServerAlgs,
                                              use_algorithms(RemoteHelloBin)),
                    RemoteClientAlgs = kexint_msg2default_algorithms(RemoteClientKexInit),
                    Client = find_common_algs(RemoteClientAlgs,
                                              use_algorithms(RemoteHelloBin)),
                    ct:log("Docker server algorithms:~n ~p~n~nDocker client algorithms:~n ~p",
                           [RemoteServerAlgs,RemoteClientAlgs]),
                    {ok, Server, Client};
                Other ->
                    Other
            end;
        Other ->
            Other
    end.


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



remote_server_algs(IP, Port) ->
    case try_gen_tcp_connect(IP, Port, 5) of
        {ok,S} ->
            ok = gen_tcp:send(S, "SSH-2.0-CheckAlgs\r\n"),
            receive_hello(S, <<>>);
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
              Parent ! {Ref,receive_hello(S, <<>>)}
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
            after 15000 ->
                    {error, timeout2}
            end
    after 15000 ->
            {error, timeout1}
    end.



receive_hello(S, Ack) ->
    %% The Ack is to collect bytes until the full message is received
    receive
        {tcp, S, Bin0} when is_binary(Bin0) ->
            case binary:split(<<Ack/binary, Bin0/binary>>, [<<"\r\n">>,<<"\r">>,<<"\n">>]) of
                [Hello = <<"SSH-2.0-",_/binary>>, NextPacket] ->
                    ct:log("Got 2.0 hello (~p), ~p bytes to next msg",[Hello,size(NextPacket)]),
                    {ok, {Hello, receive_kexinit(S, NextPacket)}};

                [Hello = <<"SSH-1.99-",_/binary>>, NextPacket] ->
                    ct:comment("Old SSH ~s",["1.99"]),
                    ct:log("Got 1.99 hello (~p), ~p bytes to next msg",[Hello,size(NextPacket)]),
                    {ok, {Hello, receive_kexinit(S, NextPacket)}};

                [Bin] when size(Bin) < 256 ->
                    ct:log("Got part of hello (~p chars):~n~s~n~s",[size(Bin),Bin,
                                                                    [io_lib:format('~2.16.0b ',[C])
                                                                     || C <- binary_to_list(Bin0)
                                                                    ]
                                                                   ]),
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



host_ip() ->
    {ok,Name} = inet:gethostname(),
    {ok,#hostent{h_addr_list = [IP|_]}} = inet_res:gethostbyname(Name),
    IP.


