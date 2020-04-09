%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2020. All Rights Reserved.
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
-module(ssh_pubkey_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("ssh_test_lib.hrl").

%%%----------------------------------------------------------------
%%% Common Test interface functions -------------------------------
%%%----------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,40}}].

all() -> 
    [{group, old_format},
     {group, new_format},
     {group, option_space}
    ].


-define(tests_old, [connect_rsa_to_rsa,
                    connect_rsa_to_dsa,
                    connect_rsa_to_ecdsa,
                    connect_dsa_to_rsa,
                    connect_dsa_to_dsa,
                    connect_dsa_to_ecdsa,
                    connect_ecdsa_to_rsa,
                    connect_ecdsa_to_dsa,
                    connect_ecdsa_to_ecdsa
                   ]).

-define(tests_new, [
                    connect_dsa_to_ed25519,
                    connect_dsa_to_ed448,
                    connect_ecdsa_to_ed25519,
                    connect_ecdsa_to_ed448,
                    connect_ed25519_to_dsa,
                    connect_ed25519_to_ecdsa,
                    connect_ed25519_to_ed448,
                    connect_ed25519_to_ed25519,
                    connect_ed25519_to_rsa,
                    connect_ed448_to_dsa,
                    connect_ed448_to_ecdsa,
                    connect_ed448_to_ed25519,
                    connect_ed448_to_ed448,
                    connect_ed448_to_rsa,
                    connect_rsa_to_ed25519,
                    connect_rsa_to_ed448
                    | ?tests_old % but taken from the new format directory
                   ]).

groups() ->
    [{new_format,  [], ?tests_new},
     {old_format,  [], ?tests_old++[{group,passphrase}]},
     {passphrase,  [], ?tests_old},
     {option_space,[], [{group,new_format}]}
    ].

%%%----------------------------------------------------------------
init_per_suite(Config) ->
    ?CHECK_CRYPTO(
       begin
	   ssh:start(),
	   [{client_opts,[]},
            {daemon_opts,[]}
            | Config]
       end).

end_per_suite(_onfig) ->
    ssh:stop().

%%%----------------------------------------------------------------
init_per_group(new_format, Config) ->
    Dir = filename:join(proplists:get_value(data_dir,Config), "new_format"),
    [{fmt,new_format},
     {key_src_dir,Dir} | Config];

init_per_group(old_format, Config) ->
    Dir = filename:join(proplists:get_value(data_dir,Config), "old_format"),
    [{fmt,old_format},
     {key_src_dir,Dir} | Config];

init_per_group(option_space, Config) ->
    extend_optsL([client_opts,daemon_opts],
                 [{key_cb, {ssh_file, [{optimize, space}]}}],
                 Config);

init_per_group(passphrase, Config0) ->
    case supported(hashs, md5) of
        true ->
            Dir = filename:join(proplists:get_value(data_dir,Config0), "old_format_passphrase"),
            PassPhrases = [{K,"somepwd"} || K <- [dsa_pass_phrase,
                                                  rsa_pass_phrase,
                                                  ecdsa_pass_phrase]],
            Config1 = extend_optsL(client_opts, PassPhrases, Config0),
            replace_opt(key_src_dir, Dir, Config1);
        false ->
            {skip, "Unsupported hash"}
    end;

init_per_group(_, Config) ->
    Config.


extend_opts(OptName, Value, Config) ->
    Opts = proplists:get_value(OptName, Config),
    replace_opt(OptName, [Value|Opts], Config).

extend_optsL(OptNames, Values, Config) when is_list(OptNames) ->
    lists:foldl(fun(N, Cnf) ->
                        extend_optsL(N, Values, Cnf)
                end, Config, OptNames);
extend_optsL(OptName, Values, Config) when is_atom(OptName) ->
    Opts = proplists:get_value(OptName, Config),
    replace_opt(OptName, Values ++ Opts, Config).

replace_opt(OptName, Value, Config) ->      
    lists:keyreplace(OptName, 1, Config, {OptName,Value}).



end_per_group(_, Config) ->
    Config.

%%%----------------------------------------------------------------
init_per_testcase(connect_rsa_to_rsa, Config0) ->
    setup_user_system_dir(rsa, rsa, Config0);
init_per_testcase(connect_rsa_to_dsa, Config0) ->
    setup_user_system_dir(rsa, dsa, Config0);
init_per_testcase(connect_rsa_to_ecdsa, Config0) ->
    setup_user_system_dir(rsa, ecdsa, Config0);
init_per_testcase(connect_rsa_to_ed25519, Config0) ->
    setup_user_system_dir(rsa, ed25519, Config0);
init_per_testcase(connect_rsa_to_ed448, Config0) ->
    setup_user_system_dir(rsa, ed448, Config0);
init_per_testcase(connect_dsa_to_rsa, Config0) ->
    setup_user_system_dir(dsa, rsa, Config0);
init_per_testcase(connect_dsa_to_dsa, Config0) ->
    setup_user_system_dir(dsa, dsa, Config0);
init_per_testcase(connect_dsa_to_ecdsa, Config0) ->
    setup_user_system_dir(dsa, ecdsa, Config0);
init_per_testcase(connect_dsa_to_ed25519, Config0) ->
    setup_user_system_dir(dsa, ed25519, Config0);
init_per_testcase(connect_dsa_to_ed448, Config0) ->
    setup_user_system_dir(dsa, ed448, Config0);
init_per_testcase(connect_ecdsa_to_rsa, Config0) ->
    setup_user_system_dir(ecdsa, rsa, Config0);
init_per_testcase(connect_ecdsa_to_dsa, Config0) ->
    setup_user_system_dir(ecdsa, dsa, Config0);
init_per_testcase(connect_ecdsa_to_ecdsa, Config0) ->
    setup_user_system_dir(ecdsa, ecdsa, Config0);
init_per_testcase(connect_ecdsa_to_ed25519, Config0) ->
    setup_user_system_dir(ecdsa, ed25519, Config0);
init_per_testcase(connect_ecdsa_to_ed448, Config0) ->
    setup_user_system_dir(ecdsa, ed448, Config0);
init_per_testcase(connect_ed25519_to_rsa, Config0) ->
    setup_user_system_dir(ed25519, rsa, Config0);
init_per_testcase(connect_ed25519_to_dsa, Config0) ->
    setup_user_system_dir(ed25519, dsa, Config0);
init_per_testcase(connect_ed25519_to_ecdsa, Config0) ->
    setup_user_system_dir(ed25519, ecdsa, Config0);
init_per_testcase(connect_ed25519_to_ed25519, Config0) ->
    setup_user_system_dir(ed25519, ed25519, Config0);
init_per_testcase(connect_ed25519_to_ed448, Config0) ->
    setup_user_system_dir(ed25519, ed448, Config0);
init_per_testcase(connect_ed448_to_rsa, Config0) ->
    setup_user_system_dir(ed448, rsa, Config0);
init_per_testcase(connect_ed448_to_dsa, Config0) ->
    setup_user_system_dir(ed448, dsa, Config0);
init_per_testcase(connect_ed448_to_ecdsa, Config0) ->
    setup_user_system_dir(ed448, ecdsa, Config0);
init_per_testcase(connect_ed448_to_ed25519, Config0) ->
    setup_user_system_dir(ed448, ed25519, Config0);
init_per_testcase(connect_ed448_to_ed448, Config0) ->
    setup_user_system_dir(ed448, ed448, Config0);
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

%%%----------------------------------------------------------------
%%% Test Cases ----------------------------------------------------
%%%----------------------------------------------------------------
connect_rsa_to_rsa(Config) ->
    try_connect(Config).

connect_rsa_to_dsa(Config) ->
    try_connect(Config).

connect_rsa_to_ecdsa(Config) ->
    try_connect(Config). 

connect_rsa_to_ed25519(Config) ->
    try_connect(Config).

connect_rsa_to_ed448(Config) ->
    try_connect(Config).

connect_dsa_to_rsa(Config) ->
    try_connect(Config).

connect_dsa_to_dsa(Config) ->
    try_connect(Config).

connect_dsa_to_ecdsa(Config) ->
    try_connect(Config). 

connect_dsa_to_ed25519(Config) ->
    try_connect(Config).

connect_dsa_to_ed448(Config) ->
    try_connect(Config).

connect_ecdsa_to_rsa(Config) ->
    try_connect(Config). 

connect_ecdsa_to_dsa(Config) ->
    try_connect(Config). 

connect_ecdsa_to_ecdsa(Config) ->
    try_connect(Config).

connect_ecdsa_to_ed25519(Config) ->
    try_connect(Config).

connect_ecdsa_to_ed448(Config) ->
    try_connect(Config).

connect_ed25519_to_rsa(Config) ->
    try_connect(Config).

connect_ed25519_to_dsa(Config) ->
    try_connect(Config).

connect_ed25519_to_ecdsa(Config) ->
    try_connect(Config).

connect_ed25519_to_ed25519(Config) ->
    try_connect(Config).

connect_ed25519_to_ed448(Config) ->
    try_connect(Config).

connect_ed448_to_rsa(Config) ->
    try_connect(Config).

connect_ed448_to_dsa(Config) ->
    try_connect(Config).

connect_ed448_to_ecdsa(Config) ->
    try_connect(Config).

connect_ed448_to_ed25519(Config) ->
    try_connect(Config).

connect_ed448_to_ed448(Config) ->
    try_connect(Config).


%%%----------------------------------------------------------------
try_connect({skip,Reson}) ->
    {skip,Reson};
try_connect(Config) ->
    SystemDir = proplists:get_value(system_dir, Config),
    UserDir = proplists:get_value(user_dir, Config),
    ClientOpts = proplists:get_value(client_opts, Config, []),
    DaemonOpts = proplists:get_value(daemon_opts, Config, []),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir}
                                             | DaemonOpts]),

    C = ssh_test_lib:connect(Host, Port, [{user_dir, UserDir},
                                          {silently_accept_hosts, true},
                                          {user_interaction, false}
                                          | ClientOpts]),
    ssh:close(C),
    ssh:stop_daemon(Pid).

%%%----------------------------------------------------------------
%%% Local ---------------------------------------------------------
%%%----------------------------------------------------------------
setup_user_system_dir(ClientAlg, ServerAlg, Config) ->
    case supported(public_keys, ClientAlg) andalso supported(public_keys, ServerAlg) of
        true ->
            PrivDir = proplists:get_value(priv_dir, Config),
            KeySrcDir = proplists:get_value(key_src_dir, Config),
            Fmt = proplists:get_value(fmt, Config),

            System = lists:concat(["system_", ClientAlg, "_", ServerAlg, "_", Fmt]),
            SystemDir = filename:join(PrivDir, System),
            file:make_dir(SystemDir),

            User   = lists:concat(["user_", ClientAlg, "_", ServerAlg, "_", Fmt]),
            UserDir   = filename:join(PrivDir, User),
            file:make_dir(UserDir),

            HostSrcFile = filename:join(KeySrcDir, file(host,ServerAlg)),
            HostDstFile = filename:join(SystemDir, file(host,ServerAlg)),

            UserSrcFile = filename:join(KeySrcDir, file(user,ClientAlg)),
            UserDstFile = filename:join(UserDir, file(user,ClientAlg)),

            UserPubSrcFile = filename:join(KeySrcDir, file(user,ClientAlg)++".pub"),
            AuthorizedKeys = filename:join(UserDir, "authorized_keys"),

            try
                {ok,_} = file:copy(UserSrcFile, UserDstFile),
                {ok,_} = file:copy(UserPubSrcFile, AuthorizedKeys),
                {ok,_} = file:copy(HostSrcFile, HostDstFile)
            of
                _ ->
                    ModAlgs = [{modify_algorithms,
                                [{append,[{public_key,
                                           lists:usort([alg(ClientAlg),
                                                        alg(ServerAlg)])}]}]}
                              ],
                    [{system_dir,SystemDir},
                     {user_dir,UserDir}
                     | extend_optsL([daemon_opts,client_opts], ModAlgs, Config)]
            catch
                error:{badmatch,{error,enoent}}:S ->
                    ct:log("~p:~p Stack:~n~p", [?MODULE,?LINE,S]),
                    {skip, no_key_file_found}
            end;

        false ->
            {skip, unsupported_algorithm}
    end.

%%%----------------------------------------------------------------
file(host, dsa)     -> "ssh_host_dsa_key";
file(host, ecdsa)   -> "ssh_host_ecdsa_key";
file(host, ed25519) -> "ssh_host_ed25519_key";
file(host, ed448)   -> "ssh_host_ed448_key";
file(host, rsa)     -> "ssh_host_rsa_key";
file(user, dsa)     -> "id_dsa";
file(user, ecdsa)   -> "id_ecdsa";
file(user, ed25519) -> "id_ed25519";
file(user, ed448)   -> "id_ed448";
file(user, rsa)     -> "id_rsa".

alg(dsa)     -> 'ssh-dss';
alg(ecdsa)   -> 'ecdsa-sha2-nistp256';
alg(ed25519) -> 'ssh-ed25519';
alg(ed448)   -> 'ssh-ed448';
alg(rsa)     -> 'ssh-rsa'.


supported(public_keys, rsa) ->     supported(public_key, 'ssh-rsa') orelse
                                       supported(public_key, 'rsa-sha2-256') orelse
                                       supported(public_key, 'rsa-sha2-521');
supported(public_keys, dsa) ->     supported(public_key, 'ssh-dss');
supported(public_keys, ecdsa) ->   supported(public_key, 'ecdsa-sha2-nistp256') orelse
                                       supported(public_key, 'ecdsa-sha2-nistp384') orelse
                                       supported(public_key, 'ecdsa-sha2-nistp521');
supported(public_keys, ed448) ->   supported(public_key, 'ssh-ed448');
supported(public_keys, ed25519) -> supported(public_key, 'ssh-ed25519');
supported(Type, Alg) ->
    case proplists:get_value(Type,ssh_transport:supported_algorithms()) of
        undefined ->
            lists:member(Alg, crypto:supports(Type));
        L ->
            lists:member(Alg, L)
    end.
