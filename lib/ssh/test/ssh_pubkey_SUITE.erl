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
         check_dsa_disabled/1,
         check_rsa_sha1_disabled/1,
         connect_dsa_to_dsa/1,
         connect_dsa_to_ecdsa/1,
         connect_dsa_to_ed25519/1,
         connect_dsa_to_ed448/1,
         connect_dsa_to_rsa_sha2/1,
         connect_ecdsa_to_dsa/1,
         connect_ecdsa_to_ecdsa/1,
         connect_ecdsa_to_ed25519/1,
         connect_ecdsa_to_ed448/1,
         connect_ecdsa_to_rsa_sha2/1,
         connect_ed25519_to_dsa/1,
         connect_ed25519_to_ecdsa/1,
         connect_ed25519_to_ed25519/1,
         connect_ed25519_to_ed448/1,
         connect_ed25519_to_rsa_sha2/1,
         connect_ed448_to_dsa/1,
         connect_ed448_to_ecdsa/1,
         connect_ed448_to_ed25519/1,
         connect_ed448_to_ed448/1,
         connect_ed448_to_rsa_sha2/1,
         connect_rsa_sha1_to_dsa/1,
         connect_rsa_sha2_to_dsa/1,
         connect_rsa_sha2_to_ecdsa/1,
         connect_rsa_sha2_to_ed25519/1,
         connect_rsa_sha2_to_ed448/1,
         connect_rsa_sha2_to_rsa_sha2/1,

         ssh_rsa_public_key/1,
         ssh_dsa_public_key/1,
         ssh_ecdsa_public_key/1,
         ssh_rfc4716_rsa_comment/1,
         ssh_rfc4716_dsa_comment/1,
         ssh_rfc4716_rsa_subject/1,
         ssh_list_public_key/1,
         ssh_known_hosts/1,
         ssh1_known_hosts/1,
         ssh_auth_keys/1,
         ssh1_auth_keys/1,
         ssh_openssh_key_with_comment/1,
         ssh_openssh_key_long_header/1,

         ssh_hostkey_fingerprint_md5_implicit/1,
         ssh_hostkey_fingerprint_md5/1,
         ssh_hostkey_fingerprint_sha/1,
         ssh_hostkey_fingerprint_sha256/1,
         ssh_hostkey_fingerprint_sha384/1,
         ssh_hostkey_fingerprint_sha512/1,
         ssh_hostkey_fingerprint_list/1,

         chk_known_hosts/1,
         ssh_hostkey_pkcs8/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").
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
     {group, option_space},
     {group, ssh_hostkey_fingerprint},
     {group, ssh_public_key_decode_encode},
     {group, pkcs8},
     chk_known_hosts
    ].


-define(tests_old, [connect_rsa_sha2_to_rsa_sha2,
                    connect_rsa_sha1_to_dsa,
                    connect_rsa_sha2_to_dsa,
                    connect_rsa_sha2_to_ecdsa,
                    connect_dsa_to_rsa_sha2,
                    connect_dsa_to_dsa,
                    connect_dsa_to_ecdsa,
                    connect_ecdsa_to_rsa_sha2,
                    connect_ecdsa_to_dsa,
                    connect_ecdsa_to_ecdsa,
                    connect_dsa_to_ed25519,
                    connect_ecdsa_to_ed25519,
                    connect_rsa_sha2_to_ed25519,
                    connect_dsa_to_ed448,
                    connect_ecdsa_to_ed448,
                    connect_rsa_sha2_to_ed448
                   ]).

-define(tests_new, [connect_ed25519_to_dsa,
                    connect_ed25519_to_ecdsa,
                    connect_ed25519_to_ed448,
                    connect_ed25519_to_ed25519,
                    connect_ed25519_to_rsa_sha2,
                    connect_ed448_to_dsa,
                    connect_ed448_to_ecdsa,
                    connect_ed448_to_ed25519,
                    connect_ed448_to_ed448,
                    connect_ed448_to_rsa_sha2
                    | ?tests_old % but taken from the new format directory
                   ]).

groups() ->
    [{new_format,  [], ?tests_new},
     {old_format,  [], [check_dsa_disabled, check_rsa_sha1_disabled | ?tests_old++[{group,passphrase}] ]},
     {passphrase,  [], ?tests_old},
     {option_space,[], [{group,new_format}]},
     {pkcs8, [], [ssh_hostkey_pkcs8]},

     {ssh_hostkey_fingerprint, [],
      [ssh_hostkey_fingerprint_md5_implicit,
       ssh_hostkey_fingerprint_md5,
       ssh_hostkey_fingerprint_sha,
       ssh_hostkey_fingerprint_sha256,
       ssh_hostkey_fingerprint_sha384,
       ssh_hostkey_fingerprint_sha512,
       ssh_hostkey_fingerprint_list]},

     {ssh_public_key_decode_encode, [],
      [ssh_rsa_public_key, ssh_dsa_public_key, ssh_ecdsa_public_key,
       ssh_rfc4716_rsa_comment, ssh_rfc4716_dsa_comment,
       ssh_rfc4716_rsa_subject,
       ssh_list_public_key,
       ssh_known_hosts, %% ssh1_known_hosts,
       ssh_auth_keys, %% ssh1_auth_keys,
       ssh_openssh_key_with_comment,
       ssh_openssh_key_long_header]}
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

init_per_group(pkcs8, Config) ->
    Dir = filename:join(proplists:get_value(data_dir,Config), "pkcs8"),
    [{fmt,pkcs8},
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

init_per_group(ssh_public_key_decode_encode, Config) ->
    [{pk_data_dir,
      filename:join([proplists:get_value(data_dir, Config),
                     "public_key"])
     } | Config];

init_per_group(_, Config) ->
    Config.


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
init_per_testcase(ssh_hostkey_pkcs8, Config0) ->
    setup_user_system_dir(rsa_sha2, rsa_sha2, Config0);
init_per_testcase(connect_rsa_sha2_to_rsa_sha2, Config0) ->
    setup_user_system_dir(rsa_sha2, rsa_sha2, Config0);
init_per_testcase(connect_rsa_sha1_to_dsa, Config0) ->
    setup_user_system_dir(rsa_sha1, dsa, Config0);
init_per_testcase(connect_rsa_sha2_to_dsa, Config0) ->
    setup_user_system_dir(rsa_sha2, dsa, Config0);
init_per_testcase(connect_rsa_sha2_to_ecdsa, Config0) ->
    setup_user_system_dir(rsa_sha2, ecdsa, Config0);
init_per_testcase(connect_rsa_sha2_to_ed25519, Config0) ->
    setup_user_system_dir(rsa_sha2, ed25519, Config0);
init_per_testcase(connect_rsa_sha2_to_ed448, Config0) ->
    setup_user_system_dir(rsa_sha2, ed448, Config0);
init_per_testcase(connect_dsa_to_rsa_sha2, Config0) ->
    setup_user_system_dir(dsa, rsa_sha2, Config0);
init_per_testcase(connect_dsa_to_dsa, Config0) ->
    setup_user_system_dir(dsa, dsa, Config0);
init_per_testcase(connect_dsa_to_ecdsa, Config0) ->
    setup_user_system_dir(dsa, ecdsa, Config0);
init_per_testcase(connect_dsa_to_ed25519, Config0) ->
    setup_user_system_dir(dsa, ed25519, Config0);
init_per_testcase(connect_dsa_to_ed448, Config0) ->
    setup_user_system_dir(dsa, ed448, Config0);
init_per_testcase(connect_ecdsa_to_rsa_sha2, Config0) ->
    setup_user_system_dir(ecdsa, rsa_sha2, Config0);
init_per_testcase(connect_ecdsa_to_dsa, Config0) ->
    setup_user_system_dir(ecdsa, dsa, Config0);
init_per_testcase(connect_ecdsa_to_ecdsa, Config0) ->
    setup_user_system_dir(ecdsa, ecdsa, Config0);
init_per_testcase(connect_ecdsa_to_ed25519, Config0) ->
    setup_user_system_dir(ecdsa, ed25519, Config0);
init_per_testcase(connect_ecdsa_to_ed448, Config0) ->
    setup_user_system_dir(ecdsa, ed448, Config0);
init_per_testcase(connect_ed25519_to_rsa_sha2, Config0) ->
    setup_user_system_dir(ed25519, rsa_sha2, Config0);
init_per_testcase(connect_ed25519_to_dsa, Config0) ->
    setup_user_system_dir(ed25519, dsa, Config0);
init_per_testcase(connect_ed25519_to_ecdsa, Config0) ->
    setup_user_system_dir(ed25519, ecdsa, Config0);
init_per_testcase(connect_ed25519_to_ed25519, Config0) ->
    setup_user_system_dir(ed25519, ed25519, Config0);
init_per_testcase(connect_ed25519_to_ed448, Config0) ->
    setup_user_system_dir(ed25519, ed448, Config0);
init_per_testcase(connect_ed448_to_rsa_sha2, Config0) ->
    setup_user_system_dir(ed448, rsa_sha2, Config0);
init_per_testcase(connect_ed448_to_dsa, Config0) ->
    setup_user_system_dir(ed448, dsa, Config0);
init_per_testcase(connect_ed448_to_ecdsa, Config0) ->
    setup_user_system_dir(ed448, ecdsa, Config0);
init_per_testcase(connect_ed448_to_ed25519, Config0) ->
    setup_user_system_dir(ed448, ed25519, Config0);
init_per_testcase(connect_ed448_to_ed448, Config0) ->
    setup_user_system_dir(ed448, ed448, Config0);

init_per_testcase(check_dsa_disabled, Config0) ->
    setup_default_user_system_dir(dsa, Config0);
init_per_testcase(check_rsa_sha1_disabled, Config0) ->
    setup_default_user_system_dir(rsa_sha1, Config0);

init_per_testcase(ssh_hostkey_fingerprint_md5_implicit, Config) ->
    init_fingerprint_testcase([md5], Config);

init_per_testcase(ssh_hostkey_fingerprint_md5, Config) ->
    init_fingerprint_testcase([md5], Config);

init_per_testcase(ssh_hostkey_fingerprint_sha, Config) ->
    init_fingerprint_testcase([sha], Config);

init_per_testcase(ssh_hostkey_fingerprint_sha256, Config) ->
    init_fingerprint_testcase([sha256], Config);

init_per_testcase(ssh_hostkey_fingerprint_sha384, Config) ->
    init_fingerprint_testcase([sha384], Config);

init_per_testcase(ssh_hostkey_fingerprint_sha512, Config) ->
    init_fingerprint_testcase([sha512], Config);

init_per_testcase(ssh_hostkey_fingerprint_list  , Config) ->
    init_fingerprint_testcase([sha,md5], Config);

init_per_testcase(_, Config) ->
    Config.


end_per_testcase(_, Config) ->
    Config.

%%%----
init_fingerprint_testcase(Algs, Config0) ->
    Hashs = proplists:get_value(hashs, crypto:supports(), []),
    case Algs -- Hashs of
        [] ->
            Config = lists:keydelete(watchdog, 1, Config0),
            Dog = ct:timetrap(?TIMEOUT),
            [{watchdog, Dog} | Config];
        UnsupportedAlgs ->
            {skip,{UnsupportedAlgs,not_supported}}
    end.

%%%----------------------------------------------------------------
%%% Test Cases ----------------------------------------------------
%%%----------------------------------------------------------------
connect_rsa_sha2_to_rsa_sha2(Config) ->
    try_connect(Config).

connect_rsa_sha1_to_dsa(Config) ->
    try_connect(Config).

connect_rsa_sha2_to_dsa(Config) ->
    try_connect(Config).

connect_rsa_sha2_to_ecdsa(Config) ->
    try_connect(Config). 

connect_rsa_sha2_to_ed25519(Config) ->
    try_connect(Config).

connect_rsa_sha2_to_ed448(Config) ->
    try_connect(Config).

connect_dsa_to_rsa_sha2(Config) ->
    try_connect(Config).

connect_dsa_to_dsa(Config) ->
    try_connect(Config).

connect_dsa_to_ecdsa(Config) ->
    try_connect(Config). 

connect_dsa_to_ed25519(Config) ->
    try_connect(Config).

connect_dsa_to_ed448(Config) ->
    try_connect(Config).

connect_ecdsa_to_rsa_sha2(Config) ->
    try_connect(Config). 

connect_ecdsa_to_dsa(Config) ->
    try_connect(Config). 

connect_ecdsa_to_ecdsa(Config) ->
    try_connect(Config).

connect_ecdsa_to_ed25519(Config) ->
    try_connect(Config).

connect_ecdsa_to_ed448(Config) ->
    try_connect(Config).

connect_ed25519_to_rsa_sha2(Config) ->
    try_connect(Config).

connect_ed25519_to_dsa(Config) ->
    try_connect(Config).

connect_ed25519_to_ecdsa(Config) ->
    try_connect(Config).

connect_ed25519_to_ed25519(Config) ->
    try_connect(Config).

connect_ed25519_to_ed448(Config) ->
    try_connect(Config).

connect_ed448_to_rsa_sha2(Config) ->
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
check_dsa_disabled(Config) ->
    try_connect_disabled(Config).
            
check_rsa_sha1_disabled(Config) ->
    try_connect_disabled(Config).


%%%----------------------------------------------------------------

%% Check of different host keys left to later
ssh_hostkey_pkcs8(Config) ->
    try_connect(Config).

%%%----------------------------------------------------------------

%% Check of different host keys left to later
ssh_hostkey_fingerprint_md5_implicit(_Config) ->
    Expected = "4b:0b:63:de:0f:a7:3a:ab:2c:cc:2d:d1:21:37:1d:3a",
    Expected = ssh:hostkey_fingerprint(ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Check of different host keys left to later
ssh_hostkey_fingerprint_md5(_Config) ->
    Expected = "MD5:4b:0b:63:de:0f:a7:3a:ab:2c:cc:2d:d1:21:37:1d:3a",
    Expected = ssh:hostkey_fingerprint(md5, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead. The Expected is generated with:
%%       $ openssh-7.3p1/ssh-keygen -E sha1 -lf <file>
%%       2048 SHA1:Soammnaqg06jrm2jivMSnzQGlmk none@example.org (RSA)
ssh_hostkey_fingerprint_sha(_Config) ->
    Expected = "SHA1:Soammnaqg06jrm2jivMSnzQGlmk",
    Expected = ssh:hostkey_fingerprint(sha, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead.
ssh_hostkey_fingerprint_sha256(_Config) ->
    Expected = "SHA256:T7F1BahkJWR7iJO8+rpzWOPbp7LZP4MlNrDExdNYOvY",
    Expected = ssh:hostkey_fingerprint(sha256, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead.
ssh_hostkey_fingerprint_sha384(_Config) ->
    Expected = "SHA384:QhkLoGNI4KXdPvC//HxxSCP3uTQVADqxdajbgm+Gkx9zqz8N94HyP1JmH8C4/aEl",
    Expected = ssh:hostkey_fingerprint(sha384, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead.
ssh_hostkey_fingerprint_sha512(_Config) ->
    Expected = "SHA512:ezUismvm3ADQQb6Nm0c1DwQ6ydInlJNfsnSQejFkXNmABg1Aenk9oi45CXeBOoTnlfTsGG8nFDm0smP10PBEeA",
    Expected = ssh:hostkey_fingerprint(sha512, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead.
ssh_hostkey_fingerprint_list(_Config) ->
    Expected = ["SHA1:Soammnaqg06jrm2jivMSnzQGlmk",
                "MD5:4b:0b:63:de:0f:a7:3a:ab:2c:cc:2d:d1:21:37:1d:3a"],
    Expected = ssh:hostkey_fingerprint([sha,md5], ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
ssh_rsa_public_key(Config) when is_list(Config) ->
    Datadir = proplists:get_value(pk_data_dir, Config),
    {ok, RSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_rsa_pub")),
    [{PubKey, Attributes1}] = ssh_file:decode(RSARawSsh2, public_key),
    [{PubKey, Attributes1}] = ssh_file:decode(RSARawSsh2, rfc4716_key),

    {ok, RSARawOpenSsh} = file:read_file(filename:join(Datadir, "openssh_rsa_pub")),
    [{PubKey, Attributes2}] = ssh_file:decode(RSARawOpenSsh, public_key),
    [{PubKey, Attributes2}] = ssh_file:decode(RSARawOpenSsh, openssh_key),

    %% Can not check EncodedSSh == RSARawSsh2 and EncodedOpenSsh
    %% = RSARawOpenSsh as line breakpoints may differ

    EncodedSSh = ssh_file:encode([{PubKey, Attributes1}], rfc4716_key),
    EncodedOpenSsh = ssh_file:encode([{PubKey, Attributes2}], openssh_key),

    [{PubKey, Attributes1}] =
	ssh_file:decode(EncodedSSh, public_key),
    [{PubKey, Attributes2}] =
	ssh_file:decode(EncodedOpenSsh, public_key).

%%--------------------------------------------------------------------
ssh_dsa_public_key(Config) when is_list(Config) ->
    Datadir = proplists:get_value(pk_data_dir, Config),

    {ok, DSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_dsa_pub")),
    [{PubKey, Attributes1}] = ssh_file:decode(DSARawSsh2, public_key),
    [{PubKey, Attributes1}] = ssh_file:decode(DSARawSsh2, rfc4716_key),

    {ok, DSARawOpenSsh} = file:read_file(filename:join(Datadir, "openssh_dsa_pub")),
    [{PubKey, Attributes2}] = ssh_file:decode(DSARawOpenSsh, public_key),
    [{PubKey, Attributes2}] = ssh_file:decode(DSARawOpenSsh, openssh_key),

    %% Can not check EncodedSSh == DSARawSsh2 and EncodedOpenSsh
    %% = DSARawOpenSsh as line breakpoints may differ

    EncodedSSh = ssh_file:encode([{PubKey, Attributes1}], rfc4716_key),
    EncodedOpenSsh = ssh_file:encode([{PubKey, Attributes2}], openssh_key),

    [{PubKey, Attributes1}] =
	ssh_file:decode(EncodedSSh, public_key),
    [{PubKey, Attributes2}] =
	ssh_file:decode(EncodedOpenSsh, public_key).

%%--------------------------------------------------------------------
ssh_ecdsa_public_key(Config) when is_list(Config) ->
    Datadir = proplists:get_value(pk_data_dir, Config),

    {ok, ECDSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_ecdsa_pub")),
    [{PubKey, Attributes1}] = ssh_file:decode(ECDSARawSsh2, public_key),
    [{PubKey, Attributes1}] = ssh_file:decode(ECDSARawSsh2, rfc4716_key),

    {ok, ECDSARawOpenSsh} = file:read_file(filename:join(Datadir, "openssh_ecdsa_pub")),
    [{PubKey, Attributes2}] = ssh_file:decode(ECDSARawOpenSsh, public_key),
    [{PubKey, Attributes2}] =ssh_file:decode(ECDSARawOpenSsh, openssh_key),

    %% Can not check EncodedSSh == ECDSARawSsh2 and EncodedOpenSsh
    %% = ECDSARawOpenSsh as line breakpoints may differ

    EncodedSSh = ssh_file:encode([{PubKey, Attributes1}], rfc4716_key),
    EncodedOpenSsh = ssh_file:encode([{PubKey, Attributes2}], openssh_key),

    [{PubKey, Attributes1}] =
	ssh_file:decode(EncodedSSh, public_key),
    [{PubKey, Attributes2}] =
	ssh_file:decode(EncodedOpenSsh, public_key).

%%--------------------------------------------------------------------
ssh_list_public_key(Config) when is_list(Config) ->
    DataDir = proplists:get_value(pk_data_dir, Config),
    {Data_ssh2, Expect_ssh2} =
        collect_binaries_expected(DataDir, rfc4716_key,
                                  ["ssh2_rsa_pub", "ssh2_rsa_comment_pub",
                                   "ssh2_dsa_pub", "ssh2_dsa_comment_pub",
                                   "ssh2_ecdsa_pub", 
                                   "ssh2_subject_pub"]),
    {Data_openssh, Expect_openssh} =
        collect_binaries_expected(DataDir, openssh_key,
                                  ["openssh_rsa_pub", "openssh_dsa_pub", "openssh_ecdsa_pub"]),

    true =
        (chk_decode(Data_openssh,   Expect_openssh, openssh_key) and
         chk_decode(Data_ssh2,      Expect_ssh2,    rfc4716_key) and
         chk_decode(Data_openssh,   Expect_openssh, public_key)         and
         chk_decode(Data_ssh2,      Expect_ssh2,    public_key)         and
         chk_encode(Expect_openssh, openssh_key) and
         chk_encode(Expect_ssh2,    rfc4716_key)
        ).

chk_encode(Data, Type) ->
    case ssh_file:decode(ssh_file:encode(Data,Type), Type) of
        Data->
            ct:log("re-encode ~p ok", [Type]),
            true;
        Result ->
            ct:log("re-encode ~p FAILED~n"
                   "Got~n ~p~nExpect~n ~p~n",
                   [Type, Result, Data]),
            false
    end.


chk_decode(Data, Expect, Type) ->
    case ssh_file:decode(Data, Type) of
        Expect ->
            ct:log("decode ~p ok", [Type]),
            true;
        BadResult ->
            ct:log("decode ~p FAILED~n"
                   "Result~n ~p~nExpect~n ~p~n"
                   "~p",
                   [Type, BadResult, Expect,
                    if
                        is_list(BadResult) ->
                            lists:foldr(fun({Key,Attrs}, Acc) ->
                                                case Key of
                                                    #'RSAPublicKey'{} when is_list(Attrs) -> Acc;
                                                    {_, #'Dss-Parms'{}} when is_list(Attrs) -> Acc;
                                                    {#'ECPoint'{}, {namedCurve,_}} when is_list(Attrs) -> Acc;
                                                    _  when is_list(Attrs) -> [{bad_key,{Key,Attrs}}|Acc];
                                                    _ -> [{bad_attrs,{Key,Attrs}}|Acc]
                                                end;
                                           (Other,Acc) ->
                                                [{other,Other}|Acc]
                                        end, [], BadResult);
                        true ->
                            '???'
                    end]),
            false
    end.


collect_binaries_expected(Dir, Type, Files) ->
    Bins0 = [B || F <- Files,
                  {ok,B} <- [ file:read_file(filename:join(Dir,F)) ]
            ],
    {list_to_binary( lists:join("\n", Bins0)),
     lists:flatten([ssh_file:decode(B,Type) || B <- Bins0])}.

%%--------------------------------------------------------------------
ssh_rfc4716_rsa_comment(Config) when is_list(Config) ->
    Datadir = proplists:get_value(pk_data_dir, Config),

    {ok, RSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_rsa_comment_pub")),
    [{#'RSAPublicKey'{} = PubKey, Attributes}] =
        ssh_file:decode(RSARawSsh2, public_key),

    Headers = proplists:get_value(headers, Attributes),

    Value = proplists:get_value("Comment", Headers, undefined),
    true = Value =/= undefined,
    RSARawSsh2 = ssh_file:encode([{PubKey, Attributes}], rfc4716_key).

%%--------------------------------------------------------------------
ssh_rfc4716_dsa_comment(Config) when is_list(Config) ->
    Datadir = proplists:get_value(pk_data_dir, Config),

    {ok, DSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_dsa_comment_pub")),
    [{{_, #'Dss-Parms'{}} = PubKey, Attributes}] =
        ssh_file:decode(DSARawSsh2, public_key),

    Headers = proplists:get_value(headers, Attributes),

    Value = proplists:get_value("Comment", Headers, undefined),
    true = Value =/= undefined,

    %% Can not check Encoded == DSARawSsh2 as line continuation breakpoints may differ
    Encoded  = ssh_file:encode([{PubKey, Attributes}], rfc4716_key),
    [{PubKey, Attributes}] =
        ssh_file:decode(Encoded, public_key).

%%--------------------------------------------------------------------
ssh_rfc4716_rsa_subject(Config) when is_list(Config) ->
    Datadir = proplists:get_value(pk_data_dir, Config),

    {ok, RSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_subject_pub")),
    [{#'RSAPublicKey'{} = PubKey, Attributes}] =
        ssh_file:decode(RSARawSsh2, public_key),

    Headers = proplists:get_value(headers, Attributes),

    Value = proplists:get_value("Subject", Headers, undefined),
    true = Value =/= undefined,

    %% Can not check Encoded == RSARawSsh2 as line continuation breakpoints may differ
    Encoded  = ssh_file:encode([{PubKey, Attributes}], rfc4716_key),
    [{PubKey, Attributes}] =
        ssh_file:decode(Encoded, public_key).

%%--------------------------------------------------------------------
ssh_known_hosts(Config) when is_list(Config) ->
    Datadir = proplists:get_value(pk_data_dir, Config),

    {ok, SshKnownHosts} = file:read_file(filename:join(Datadir, "known_hosts")),
    [{#'RSAPublicKey'{}, Attributes1}, {#'RSAPublicKey'{}, Attributes2},
     {#'RSAPublicKey'{}, Attributes3}, {#'RSAPublicKey'{}, Attributes4}] = Decoded =
        ssh_file:decode(SshKnownHosts, known_hosts),

    Comment1 = undefined,
    Comment2 = "foo@bar.com",
    Comment3 = "Comment with whitespaces",
    Comment4 = "foo@bar.com Comment with whitespaces",
    	
    Comment1 = proplists:get_value(comment, Attributes1, undefined),
    Comment2 = proplists:get_value(comment, Attributes2),
    Comment3 = proplists:get_value(comment, Attributes3),
    Comment4 = proplists:get_value(comment, Attributes4),	

    Value1 = proplists:get_value(hostnames, Attributes1, undefined),
    Value2 = proplists:get_value(hostnames, Attributes2, undefined),
    true = (Value1 =/= undefined) and (Value2 =/= undefined),

    Encoded = ssh_file:encode(Decoded, known_hosts),
    Decoded = ssh_file:decode(Encoded, known_hosts).

%%--------------------------------------------------------------------
ssh1_known_hosts(Config) when is_list(Config) ->
    Datadir = proplists:get_value(pk_data_dir, Config),

    {ok, SshKnownHosts} = file:read_file(filename:join(Datadir, "ssh1_known_hosts")),
    [{#'RSAPublicKey'{}, Attributes1}, {#'RSAPublicKey'{}, Attributes2},{#'RSAPublicKey'{}, Attributes3}] 
	= Decoded = ssh_file:decode(SshKnownHosts, known_hosts),

    Value1 = proplists:get_value(hostnames, Attributes1, undefined),
    Value2 = proplists:get_value(hostnames, Attributes2, undefined),
    true = (Value1 =/= undefined) and (Value2 =/= undefined),

    Comment ="dhopson@VMUbuntu-DSH comment with whitespaces",
    Comment = proplists:get_value(comment, Attributes3),

    Encoded = ssh_file:encode(Decoded, known_hosts),
    Decoded = ssh_file:decode(Encoded, known_hosts).

%%--------------------------------------------------------------------
ssh_auth_keys(Config) when is_list(Config) ->
    Datadir = proplists:get_value(pk_data_dir, Config),

    {ok, SshAuthKeys} = file:read_file(filename:join(Datadir, "auth_keys")),
    [{#'RSAPublicKey'{}, Attributes1}, {{_, #'Dss-Parms'{}}, Attributes2},
     {#'RSAPublicKey'{}, Attributes3}, {{_, #'Dss-Parms'{}}, Attributes4}
    ] = Decoded =
        ssh_file:decode(SshAuthKeys, auth_keys),

    Value1 = proplists:get_value(options, Attributes1, undefined),
    true = Value1 =/= undefined,

    Comment1 = Comment2 = "dhopson@VMUbuntu-DSH",
    Comment3 = Comment4 ="dhopson@VMUbuntu-DSH comment with whitespaces",
    
    Comment1 = proplists:get_value(comment, Attributes1),
    Comment2 = proplists:get_value(comment, Attributes2),
    Comment3 = proplists:get_value(comment, Attributes3),
    Comment4 = proplists:get_value(comment, Attributes4),

    Encoded = ssh_file:encode(Decoded, auth_keys),
    Decoded = ssh_file:decode(Encoded, auth_keys).

%%--------------------------------------------------------------------
ssh1_auth_keys(Config) when is_list(Config) ->
    Datadir = proplists:get_value(pk_data_dir, Config),

    {ok, SshAuthKeys} = file:read_file(filename:join(Datadir, "ssh1_auth_keys")),
    [{#'RSAPublicKey'{}, Attributes1},
     {#'RSAPublicKey'{}, Attributes2}, {#'RSAPublicKey'{}, Attributes3},
     {#'RSAPublicKey'{}, Attributes4}, {#'RSAPublicKey'{}, Attributes5}] = Decoded =
        ssh_file:decode(SshAuthKeys, auth_keys),

    Value1 = proplists:get_value(bits, Attributes2, undefined),
    Value2 = proplists:get_value(bits, Attributes3, undefined),
    true = (Value1 =/= undefined) and (Value2 =/= undefined),

    Comment2 = Comment3 = "dhopson@VMUbuntu-DSH",
    Comment4 = Comment5 ="dhopson@VMUbuntu-DSH comment with whitespaces",
    
    undefined = proplists:get_value(comment, Attributes1, undefined),
    Comment2 = proplists:get_value(comment, Attributes2),
    Comment3 = proplists:get_value(comment, Attributes3),
    Comment4 = proplists:get_value(comment, Attributes4),
    Comment5 = proplists:get_value(comment, Attributes5),

    Encoded = ssh_file:encode(Decoded, auth_keys),
    Decoded = ssh_file:decode(Encoded, auth_keys).

%%--------------------------------------------------------------------
ssh_openssh_key_with_comment(Config) when is_list(Config) ->
    Datadir = proplists:get_value(pk_data_dir, Config),

    {ok, DSARawOpenSsh} = file:read_file(filename:join(Datadir, "openssh_dsa_with_comment_pub")),
    [{{_, #'Dss-Parms'{}}, _}] = ssh_file:decode(DSARawOpenSsh, openssh_key).

%%--------------------------------------------------------------------
ssh_openssh_key_long_header(Config) when is_list(Config) ->
    Datadir = proplists:get_value(pk_data_dir, Config),

    {ok,RSARawOpenSsh} = file:read_file(filename:join(Datadir, "ssh_rsa_long_header_pub")),
    [{#'RSAPublicKey'{}, _}] = Decoded = ssh_file:decode(RSARawOpenSsh, public_key),

    Encoded = ssh_file:encode(Decoded, rfc4716_key),
    Decoded = ssh_file:decode(Encoded, rfc4716_key).

%%%----------------------------------------------------------------
%%% Test case helpers
%%%----------------------------------------------------------------
%% Should use stored keys instead
ssh_hostkey(rsa) ->
    [{PKdecoded,_}] =
	ssh_file:decode(
	  <<"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDYXcYmsyJBstl4EfFYzfQJmSiUE162zvSGSoMYybShYOI6rnnyvvihfw8Aml+2gZ716F2tqG48FQ/yPZEGWNPMrCejPpJctaPWhpNdNMJ8KFXSEgr5bY2mEpa19DHmuDeXKzeJJ+X7s3fVdYc4FMk5731KIW6Huf019ZnTxbx0VKG6b1KAJBg3vpNsDxEMwQ4LFMB0JHVklOTzbxmpaeULuIxvl65A+eGeFVeo2Q+YI9UnwY1vSgmc9Azwy8Ie9Z0HpQBN5I7Uc5xnknT8V6xDhgNfXEfzsgsRdDfZLECt1WO/1gP9wkosvAGZWt5oG8pbNQWiQdFq536ck8WQD9WD none@example.org">>,
	  public_key),
    PKdecoded.

%%%----------------------------------------------------------------
chk_known_hosts(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),

    DataDir = filename:join(proplists:get_value(data_dir,Config), "new_format"),
    SysDir = filename:join(PrivDir, "chk_known_hosts_sys_dir"),
    ssh_test_lib:setup_all_host_keys(DataDir, SysDir),

    UsrDir = filename:join(PrivDir, "chk_known_hosts_usr_dir"),
    file:make_dir(UsrDir),
    KnownHostsFile = filename:join(UsrDir, "known_hosts"),

    DaemonOpts = [{system_dir, SysDir},
                  {user_dir, UsrDir},
                  {password, "bar"}],

    UserOpts = [{user_dir, UsrDir},
                {user, "foo"},
                {password, "bar"},
                {silently_accept_hosts, true},
                {user_interaction, false}
               ],

    {_Pid1, Host1, Port1} = ssh_test_lib:daemon(DaemonOpts),
    {_Pid2, Host2, Port2} = ssh_test_lib:daemon(DaemonOpts),

    _C1 = ssh_test_lib:connect(Host1, Port1, UserOpts),
    {ok,KnownHosts1} = file:read_file(KnownHostsFile),
    Sz1 = byte_size(KnownHosts1),
    ct:log("~p bytes KnownHosts1 = ~p", [Sz1, KnownHosts1]),

    _C2 = ssh_test_lib:connect(Host2, Port2, UserOpts),
    {ok,KnownHosts2} = file:read_file(KnownHostsFile),
    Sz2 = byte_size(KnownHosts2),
    ct:log("~p bytes KnownHosts2 = ~p", [Sz2, KnownHosts2]),

    %% Check that 2nd is appended after the 1st:
    <<KnownHosts1:Sz1/binary, _/binary>> = KnownHosts2,

    %% Check that there are exactly two NLs:
    2 = lists:foldl(fun($\n, Sum) -> Sum + 1;
                       (_,   Sum) -> Sum
                    end, 0, binary_to_list(KnownHosts2)),

    %% Check that at least one NL terminates both two lines:
    <<_:(Sz1-1)/binary, $\n, _:(Sz2-Sz1-1)/binary, $\n>> = KnownHosts2.


%%%----------------------------------------------------------------
try_connect({skip,Reason}) ->
    {skip,Reason};
try_connect(Config) ->
    SystemDir = proplists:get_value(system_dir, Config),
    UserDir = proplists:get_value(user_dir, Config),
    ClientOpts = proplists:get_value(client_opts, Config, []),
    DaemonOpts = proplists:get_value(daemon_opts, Config, []),

    ssh_dbg:start(fun ct:log/2), ssh_dbg:on([alg]),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir}
                                             | DaemonOpts]),

    C = ssh_test_lib:connect(Host, Port, [{user_dir, UserDir},
                                          {silently_accept_hosts, true},
                                          {user_interaction, false}
                                          | ClientOpts]),
    ssh:close(C),
    ssh_dbg:stop(),
    ssh:stop_daemon(Pid).


try_connect_disabled(Config) ->
    try try_connect(Config)
    of _ -> {fail, "non-default algorithm accepted"}
    catch error:{badmatch,{error,"Service not available"}} -> ok
    end.

%%%----------------------------------------------------------------
%%% Local ---------------------------------------------------------
%%%----------------------------------------------------------------
setup_user_system_dir(ClientAlg, ServerAlg, Config) ->
    case supported(public_key, ClientAlg) andalso supported(public_key, ServerAlg) of
        true ->
            try
                setup_dirs(ClientAlg, ServerAlg, Config)
            of
                {ok, {SystemDir,UserDir}} ->
                    ModAlgs = [{preferred_algorithms, 
                                [{public_key, lists:usort([alg(ClientAlg), alg(ServerAlg)])}]
                               }],
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


setup_default_user_system_dir(ClientAlg, Config) ->
    ServerAlg = ecdsa,
    case default(public_key, ClientAlg) of
        false ->
            case supported(public_key, ClientAlg) of
                true ->
                    case supported(public_key, ServerAlg) of
                        true ->
                            try
                                setup_dirs(ClientAlg, ServerAlg, Config)
                            of
                                {ok, {SystemDir,UserDir}} ->
                                    ModAlgs = [{modify_algorithms,
                                                [{append,[{public_key,[alg(ServerAlg)]}]},
                                                 {rm, [{public_key,[alg(ClientAlg)|inv_algs(ClientAlg)]}]}
                                                ]}],
                                    [{system_dir,SystemDir},
                                     {user_dir,UserDir}
                                     | extend_optsL([daemon_opts,client_opts], ModAlgs, Config)]
                            catch
                                error:{badmatch,{error,enoent}}:S ->
                                    ct:log("~p:~p Stack:~n~p", [?MODULE,?LINE,S]),
                                    {skip, no_key_file_found}
                            end;
                        false ->
                            {skip, unsupported_server_algorithm}
                    end;
                false ->
                    {skip, unsupported_client_algorithm}
            end;
        true ->
            {fail, disabled_algorithm_present}
    end.
            
            
setup_dirs(ClientAlg, ServerAlg, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    KeySrcDir = proplists:get_value(key_src_dir, Config),
    Fmt = proplists:get_value(fmt, Config),

    System = lists:concat(["system_", ClientAlg, "_", ServerAlg, "_", Fmt]),
    SystemDir = filename:join(PrivDir, System),
    file:make_dir(SystemDir),

    User   = lists:concat(["user_", ClientAlg, "_", ServerAlg, "_", Fmt]),
    UserDir   = filename:join(PrivDir, User),
    file:make_dir(UserDir),

    HostSrcFile = filename:join(KeySrcDir, file(src,host,ServerAlg)),
    HostDstFile = filename:join(SystemDir, file(dst,host,ServerAlg)),

    UserSrcFile = filename:join(KeySrcDir, file(src,user,ClientAlg)),
    UserDstFile = filename:join(UserDir, file(dst,user,ClientAlg)),

    UserPubSrcFile = filename:join(KeySrcDir, file(src,user,ClientAlg)++".pub"),
    AuthorizedKeys = filename:join(UserDir, "authorized_keys"),

    ct:log("UserSrcFile = ~p~nUserDstFile = ~p", [UserSrcFile, UserDstFile]),
    {ok,_} = file:copy(UserSrcFile, UserDstFile),
    ct:log("UserPubSrcFile = ~p~nAuthorizedKeys = ~p", [UserPubSrcFile, AuthorizedKeys]),
    {ok,_} = file:copy(UserPubSrcFile, AuthorizedKeys),
    ct:log("HostSrcFile = ~p~nHostDstFile = ~p", [HostSrcFile, HostDstFile]),
    {ok,_} = file:copy(HostSrcFile, HostDstFile),
    
    ct:log("SystemDir = ~p~nUserDir = ~p", [SystemDir,UserDir]),
    {ok, {SystemDir,UserDir}}.

%%%----------------------------------------------------------------
file(  _, host, dsa)     -> "ssh_host_dsa_key";
file(  _, host, ecdsa)   -> "ssh_host_ecdsa_key";
file(  _, host, ed25519) -> "ssh_host_ed25519_key";
file(  _, host, ed448)   -> "ssh_host_ed448_key";
file(  _, host, rsa_sha2)-> "ssh_host_rsa_key";
file(src, host, rsa_sha1)-> "ssh_host_rsa_key";
file(dst, host, rsa_sha1)-> "ssh_host_rsa_key";
file(  _, user, dsa)     -> "id_dsa";
file(  _, user, ecdsa)   -> "id_ecdsa";
file(  _, user, ed25519) -> "id_ed25519";
file(  _, user, ed448)   -> "id_ed448";
file(  _, user, rsa_sha2)-> "id_rsa";
file(src, user, rsa_sha1)-> "id_rsa";
file(dst, user, rsa_sha1)-> "id_rsa".

alg(dsa)     -> 'ssh-dss';
alg(ecdsa)   -> 'ecdsa-sha2-nistp256';
alg(ed25519) -> 'ssh-ed25519';
alg(ed448)   -> 'ssh-ed448';
alg(rsa_sha2)-> 'rsa-sha2-256';
alg(rsa_sha1)-> 'ssh-rsa'.

inv_algs(rsa_sha1) -> algs(rsa_sha2);
inv_algs(_) -> [].

algs(dsa)     -> ['ssh-dss'];
algs(ecdsa)   -> ['ecdsa-sha2-nistp256', 'ecdsa-sha2-nistp384', 'ecdsa-sha2-521'];
algs(ed25519) -> ['ssh-ed25519'];
algs(ed448)   -> ['ssh-ed448'];
algs(rsa_sha2)-> ['rsa-sha2-256', 'rsa-sha2-384', 'rsa-sha2-512'];
algs(rsa_sha1)-> ['ssh-rsa'];
algs(A) -> [A].



default(Type, Alg) -> listed(algs(Alg), ssh_transport:default_algorithms(Type)).

supported(Type, Alg) -> listed(algs(Alg),
                               try
                                   ssh_transport:supported_algorithms(Type)
                               catch
                                   error:function_clause -> crypto:supports(Type)
                               end).

listed(As, L) -> lists:any(fun(A) -> lists:member(A,L) end,
                           As).
                                   
    
