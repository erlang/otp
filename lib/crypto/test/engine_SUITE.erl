%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2017. All Rights Reserved.
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

-module(engine_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds, 10}}
    ].

all() ->
    [
     get_all_possible_methods,
     engine_load_all_methods,
     engine_load_some_methods,
     bad_arguments,
     unknown_engine,
     pre_command_fail_bad_value,
     pre_command_fail_bad_key,
     failed_engine_init,
     ctrl_cmd_string,
     ctrl_cmd_string_optional,
     {group, engine_stored_key}
    ].

groups() ->
    [{engine_stored_key, [],
      [sign_verify_rsa,
       sign_verify_dsa,
       sign_verify_ecdsa,
       sign_verify_rsa_pwd,
       sign_verify_rsa_pwd_bad_pwd,
       priv_encrypt_pub_decrypt_rsa,
       priv_encrypt_pub_decrypt_rsa_pwd,
       pub_encrypt_priv_decrypt_rsa,
       pub_encrypt_priv_decrypt_rsa_pwd,
       get_pub_from_priv_key_rsa,
       get_pub_from_priv_key_rsa_pwd,
       get_pub_from_priv_key_rsa_pwd_no_pwd,
       get_pub_from_priv_key_rsa_pwd_bad_pwd,
       get_pub_from_priv_key_dsa,
       get_pub_from_priv_key_ecdsa
      ]}].


init_per_suite(Config) ->
    try crypto:start() of
	ok ->
            case crypto:info_lib() of
                [{_,_, <<"OpenSSL 1.0.1s-freebsd  1 Mar 2016">>}] ->
                    {skip, "Problem with engine on OpenSSL 1.0.1s-freebsd"};
                _ ->
                    Config
            end;
        {error,{already_started,crypto}} ->
            Config
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
init_per_group(engine_stored_key, Config) ->
    case load_storage_engine(Config) of
        {ok, E} ->
            KeyDir = key_dir(Config),
            [{storage_engine,E}, {storage_dir,KeyDir} | Config];
        {error, notexist} ->
            {skip, "OTP Test engine not found"};
        {error, notsup} ->
            {skip, "Engine not supported on this OpenSSL version"};
        {error, bad_engine_id} ->
            {skip, "Dynamic Engine not supported"};
        Other ->
            ct:log("Engine load failed: ~p",[Other]),
            {fail, "Engine load failed"}
    end;
init_per_group(_Group, Config0) ->
    Config0.

end_per_group(engine_stored_key, Config) ->
    case proplists:get_value(storage_engine, Config) of
        undefined ->
            ok;
        E ->
            ok = crypto:engine_unload(E)
    end;
end_per_group(_, _) ->
    ok.

%%--------------------------------------------------------------------
init_per_testcase(_Case, Config) ->
    Config.
end_per_testcase(_Case, _Config) ->
    ok.

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
get_all_possible_methods() ->
    [{doc, "Just fetch all possible engine methods supported."}].

get_all_possible_methods(Config) when is_list(Config) ->
    try
        List = crypto:engine_get_all_methods(),
        ct:log("crypto:engine_get_all_methods() -> ~p\n", [List]),
        ok
    catch
        error:notsup ->
            {skip, "Engine not supported on this OpenSSL version"}
    end.

engine_load_all_methods()->
    [{doc, "Use a dummy md5 engine that does not implement md5"
      "but rather returns a static binary to test that crypto:engine_load "
      "functions works."}].

engine_load_all_methods(Config) when is_list(Config) ->
    case crypto:get_test_engine() of
        {error, notexist} ->
            {skip, "OTP Test engine not found"};
        {ok, Engine} ->
            try 
                Md5Hash1 =  <<106,30,3,246,166,222,229,158,244,217,241,179,50,232,107,109>>,
                Md5Hash1 = crypto:hash(md5, "Don't panic"),
                Md5Hash2 =  <<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>,
                case crypto:engine_load(<<"dynamic">>,
                                        [{<<"SO_PATH">>, Engine},
                                         {<<"ID">>, <<"MD5">>},
                                         <<"LOAD">>],
                                        []) of
                    {ok, E} ->
                        case crypto:hash(md5, "Don't panic") of
                            Md5Hash1 ->
                                ct:fail(fail_to_load_still_original_engine);
                            Md5Hash2 ->
                                ok;
                            _ ->
                                ct:fail(fail_to_load_engine)
                        end,
                        ok = crypto:engine_unload(E),
                        case crypto:hash(md5, "Don't panic") of
                            Md5Hash2 ->
                                ct:fail(fail_to_unload_still_test_engine);
                            Md5Hash1 ->
                                ok;
                            _ ->
                                ct:fail(fail_to_unload_engine)
                        end;
                    {error, bad_engine_id} ->
                        {skip, "Dynamic Engine not supported"}
                end
           catch
               error:notsup ->
                  {skip, "Engine not supported on this OpenSSL version"}
           end
    end.

engine_load_some_methods()->
    [{doc, "Use a dummy md5 engine that does not implement md5"
      "but rather returns a static binary to test that crypto:engine_load "
      "functions works."}].

engine_load_some_methods(Config) when is_list(Config) ->
    case crypto:get_test_engine() of
        {error, notexist} ->
            {skip, "OTP Test engine not found"};
        {ok, Engine} ->
            try 
                Md5Hash1 =  <<106,30,3,246,166,222,229,158,244,217,241,179,50,232,107,109>>,
                Md5Hash1 = crypto:hash(md5, "Don't panic"),
                Md5Hash2 =  <<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>,
                EngineMethods = crypto:engine_get_all_methods() --
                    [engine_method_dh,engine_method_rand,
                     engine_method_ciphers, engine_method_store,
                     engine_method_pkey_meths, engine_method_pkey_asn1_meths],
                case crypto:engine_load(<<"dynamic">>,
                                        [{<<"SO_PATH">>, Engine},
                                         {<<"ID">>, <<"MD5">>},
                                         <<"LOAD">>],
                                        [],
                                        EngineMethods) of
                    {ok, E} ->        
                        case crypto:hash(md5, "Don't panic") of
                            Md5Hash1 ->
                                ct:fail(fail_to_load_engine_still_original);
                            Md5Hash2 ->
                                ok;
                            _ ->
                                ct:fail(fail_to_load_engine)
                        end,
                        ok = crypto:engine_unload(E),
                        case crypto:hash(md5, "Don't panic") of
                            Md5Hash2 ->
                                ct:fail(fail_to_unload_still_test_engine);
                            Md5Hash1 ->
                                ok;
                            _ ->
                                ct:fail(fail_to_unload_engine)
                        end;
                    {error, bad_engine_id} ->
                    {skip, "Dynamic Engine not supported"}
                end
           catch
               error:notsup ->
                  {skip, "Engine not supported on this OpenSSL version"}
           end
    end.

%%-------------------------------------------------------------------------
%% Error cases
bad_arguments()->
    [{doc, "Test different arguments in bad format."}].

bad_arguments(Config) when is_list(Config) ->
    case crypto:get_test_engine() of
        {error, notexist} ->
            {skip, "OTP Test engine not found"};
        {ok, Engine} ->
            try 
                try
                    crypto:engine_load(fail_engine, [], [])
                catch
                    error:badarg ->
                       ok
                end,
                try
                    crypto:engine_load(<<"dynamic">>,
                                       [{<<"SO_PATH">>, Engine},
                                        1,
                                        {<<"ID">>, <<"MD5">>},
                                        <<"LOAD">>],
                                       [])
                catch
                    error:badarg ->
                       ok
                end,
                try
                    crypto:engine_load(<<"dynamic">>,
                                       [{<<"SO_PATH">>, Engine},
                                        {'ID', <<"MD5">>},
                                        <<"LOAD">>],
                                       [])
                catch
                    error:badarg ->
                       ok
                end
          catch
              error:notsup ->
                 {skip, "Engine not supported on this OpenSSL version"}
          end
    end.

unknown_engine() ->
    [{doc, "Try to load a non existent engine."}].

unknown_engine(Config) when is_list(Config) ->
    try
        {error, bad_engine_id} = crypto:engine_load(<<"fail_engine">>, [], []),
        ok
    catch
        error:notsup ->
           {skip, "Engine not supported on this OpenSSL version"}
    end.

pre_command_fail_bad_value() ->
    [{doc, "Test pre command due to bad value"}].

pre_command_fail_bad_value(Config) when is_list(Config) ->
    DataDir = unicode:characters_to_binary(code:priv_dir(crypto)),
    try
        case crypto:engine_load(<<"dynamic">>,
                                [{<<"SO_PATH">>,
                                  <<DataDir/binary, <<"/libfail_engine.so">>/binary >>},
                                 {<<"ID">>, <<"MD5">>},
                                 <<"LOAD">>],
                                []) of
            {error, ctrl_cmd_failed} ->
                ok;
            {error, bad_engine_id} ->
                {skip, "Dynamic Engine not supported"}
        end
    catch
        error:notsup ->
           {skip, "Engine not supported on this OpenSSL version"}
    end.

pre_command_fail_bad_key() ->
    [{doc, "Test pre command due to bad key"}].

pre_command_fail_bad_key(Config) when is_list(Config) ->
    try
        case crypto:get_test_engine() of
            {error, notexist} ->
                {skip, "OTP Test engine not found"};
            {ok, Engine} ->
                case crypto:engine_load(<<"dynamic">>,
                                        [{<<"SO_WRONG_PATH">>, Engine},
                                         {<<"ID">>, <<"MD5">>},
                                         <<"LOAD">>],
                                        []) of
                    {error, ctrl_cmd_failed} ->
                        ok;
                    {error, bad_engine_id} ->
                        {skip, "Dynamic Engine not supported"}
                end
        end
   catch 
       error:notsup ->
          {skip, "Engine not supported on this OpenSSL version"}
   end.

failed_engine_init()->
    [{doc, "Test failing engine init due to missed pre command"}].

failed_engine_init(Config) when is_list(Config) ->
    try
        case crypto:get_test_engine() of
            {error, notexist} ->
                {skip, "OTP Test engine not found"};
            {ok, Engine} ->
                case crypto:engine_load(<<"dynamic">>,
                                        [{<<"SO_PATH">>, Engine},
                                         {<<"ID">>, <<"MD5">>}],
                                        []) of
                    {error, add_engine_failed} ->
                        ok;
                    {error, bad_engine_id} ->
                        {skip, "Dynamic Engine not supported"}
                end
        end
   catch 
       error:notsup ->
          {skip, "Engine not supported on this OpenSSL version"}
   end.


ctrl_cmd_string()->
    [{doc, "Test that a not known optional ctrl comand do not fail"}].
ctrl_cmd_string(Config) when is_list(Config) ->
    try
        case crypto:get_test_engine() of
            {error, notexist} ->
                {skip, "OTP Test engine not found"};
            {ok, Engine} ->
                case crypto:engine_load(<<"dynamic">>,
                                        [{<<"SO_PATH">>, Engine},
                                         {<<"ID">>, <<"MD5">>},
                                         <<"LOAD">>],
                                        []) of
                    {ok, E} ->        
                        case crypto:engine_ctrl_cmd_string(E, <<"TEST">>, <<"17">>) of
                            ok ->
                                ct:fail(fail_ctrl_cmd_should_fail);
                            {error,ctrl_cmd_failed} -> 
                                ok
                        end,
                        ok = crypto:engine_unload(E);              
                    {error, bad_engine_id} ->
                        {skip, "Dynamic Engine not supported"}
                end
        end
   catch 
       error:notsup ->
          {skip, "Engine not supported on this OpenSSL version"}
   end.        

ctrl_cmd_string_optional()->
    [{doc, "Test that a not known optional ctrl comand do not fail"}].
ctrl_cmd_string_optional(Config) when is_list(Config) ->
    try
        case crypto:get_test_engine() of
            {error, notexist} ->
                {skip, "OTP Test engine not found"};
            {ok, Engine} ->
                case crypto:engine_load(<<"dynamic">>,
                                        [{<<"SO_PATH">>, Engine},
                                         {<<"ID">>, <<"MD5">>},
                                         <<"LOAD">>],
                                        []) of
                    {ok, E} ->        
                        case crypto:engine_ctrl_cmd_string(E, <<"TEST">>, <<"17">>, true) of
                            ok ->
                                ok;
                            _ -> 
                                ct:fail(fail_ctrl_cmd_string)
                        end,
                        ok = crypto:engine_unload(E);              
                    {error, bad_engine_id} ->
                        {skip, "Dynamic Engine not supported"}
                end
        end
   catch 
       error:notsup ->
          {skip, "Engine not supported on this OpenSSL version"}
   end.        

%%%----------------------------------------------------------------
%%% Pub/priv key storage tests.  Thoose are for testing the crypto.erl
%%% support for using priv/pub keys stored in an engine.

sign_verify_rsa(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key.pem")},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_public_key.pem")},
    sign_verify(rsa, sha, Priv, Pub).

sign_verify_dsa(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "dsa_private_key.pem")},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "dsa_public_key.pem")},
    sign_verify(dss, sha, Priv, Pub).

sign_verify_ecdsa(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "ecdsa_private_key.pem")},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "ecdsa_public_key.pem")},
    sign_verify(ecdsa, sha, Priv, Pub).

sign_verify_rsa_pwd(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key_pwd.pem"),
             password => "password"},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_public_key_pwd.pem")},
    sign_verify(rsa, sha, Priv, Pub).

sign_verify_rsa_pwd_bad_pwd(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key_pwd.pem"),
             password => "Bad password"},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_public_key_pwd.pem")},
    try sign_verify(rsa, sha, Priv, Pub) of
        _ -> {fail, "PWD prot pubkey sign succeded with no pwd!"}
    catch
        error:badarg -> ok
    end.  

priv_encrypt_pub_decrypt_rsa(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key.pem")},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_public_key.pem")},
    priv_enc_pub_dec(rsa, Priv, Pub,  rsa_pkcs1_padding).

priv_encrypt_pub_decrypt_rsa_pwd(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key_pwd.pem"),
             password => "password"},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_public_key_pwd.pem")},
    priv_enc_pub_dec(rsa, Priv, Pub,  rsa_pkcs1_padding).

pub_encrypt_priv_decrypt_rsa(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key.pem")},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_public_key.pem")},
    pub_enc_priv_dec(rsa, Pub, Priv,  rsa_pkcs1_padding).

pub_encrypt_priv_decrypt_rsa_pwd(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key_pwd.pem"),
             password => "password"},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_public_key_pwd.pem")},
    pub_enc_priv_dec(rsa, Pub, Priv,  rsa_pkcs1_padding).

get_pub_from_priv_key_rsa(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key.pem")},
    case crypto:privkey_to_pubkey(rsa, Priv) of
        {error, not_found} ->
            {fail, "Key not found"};
        {error, notsup} ->
            {skip, "RSA not supported"};
        {error, Error} ->
            {fail, {wrong_error,Error}};
        Pub ->
            ct:log("rsa Pub = ~p",[Pub]),
            sign_verify(rsa, sha, Priv, Pub)
    end.

get_pub_from_priv_key_rsa_pwd(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key_pwd.pem"),
             password => "password"},
    case crypto:privkey_to_pubkey(rsa, Priv) of
        {error, not_found} ->
            {fail, "Key not found"};
        {error, notsup} ->
            {skip, "RSA not supported"};
        {error, Error} ->
            {fail, {wrong_error,Error}};
        Pub ->
            ct:log("rsa Pub = ~p",[Pub]),
            sign_verify(rsa, sha, Priv, Pub)
    end.

get_pub_from_priv_key_rsa_pwd_no_pwd(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key_pwd.pem")},
    case crypto:privkey_to_pubkey(rsa, Priv) of
        {error, not_found} ->
            ok;
        {error, notsup} ->
            {skip, "RSA not supported"};
        {error, Error} ->
            {fail, {wrong_error,Error}};
        Pub -> 
            ct:log("rsa Pub = ~p",[Pub]),
            {fail, "PWD prot pubkey fetch succeded although no pwd!"}
    end.

get_pub_from_priv_key_rsa_pwd_bad_pwd(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key_pwd.pem"),
             password => "Bad password"},
    case crypto:privkey_to_pubkey(rsa, Priv) of
        {error, not_found} ->
            ok;
        {error, notsup} ->
            {skip, "RSA not supported"};
        {error, Error} ->
            {fail, {wrong_error,Error}};
        Pub -> 
            ct:log("rsa Pub = ~p",[Pub]),
            {fail, "PWD prot pubkey fetch succeded with bad pwd!"}
    end.

get_pub_from_priv_key_dsa(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "dsa_private_key.pem")},
    case crypto:privkey_to_pubkey(dss, Priv) of
        {error, not_found} ->
            {fail, "Key not found"};
        {error, notsup} ->
            {skip, "DSA not supported"};
        {error, Error} ->
            {fail, {wrong_error,Error}};
        Pub ->
            ct:log("dsa Pub = ~p",[Pub]),
            sign_verify(dss, sha, Priv, Pub)
    end.

get_pub_from_priv_key_ecdsa(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "ecdsa_private_key.pem")},
    case crypto:privkey_to_pubkey(ecdsa, Priv) of
        {error, not_found} ->
            {fail, "Key not found"};
        {error, notsup} ->
            {skip, "ECDSA not supported"};
        {error, Error} ->
            {fail, {wrong_error,Error}};
        Pub ->
            ct:log("ecdsa Pub = ~p",[Pub]),
            sign_verify(ecdsa, sha, Priv, Pub)
    end.
    
%%%================================================================
%%% Help for engine_stored_pub_priv_keys* test cases
%%%
load_storage_engine(_Config) ->
    case crypto:get_test_engine() of
        {ok, Engine} ->
            try crypto:engine_load(<<"dynamic">>,
                                   [{<<"SO_PATH">>, Engine},
                                    <<"LOAD">>],
                                   [])
            catch
                error:notsup ->
                    {error, notsup}
            end;

        {error, Error} ->
            {error, Error}
    end.


key_dir(Config) ->
    DataDir = unicode:characters_to_binary(proplists:get_value(data_dir, Config)),
    filename:join(DataDir, "pkcs8").


engine_ref(Config) ->
    proplists:get_value(storage_engine, Config).

key_id(Config, File) ->
    filename:join(proplists:get_value(storage_dir,Config), File).

pubkey_alg_supported(Alg) ->
    lists:member(Alg,
                 proplists:get_value(public_keys, crypto:supports())).


pub_enc_priv_dec(Alg, KeyEnc, KeyDec, Padding) ->
    case pubkey_alg_supported(Alg) of
        true ->
            PlainText = <<"Hej på dig">>,
            CryptoText = crypto:public_encrypt(Alg, PlainText, KeyEnc, Padding),
            case crypto:private_decrypt(Alg, CryptoText, KeyDec, Padding) of
                PlainText -> ok;
                _ -> {fail, "Encrypt-decrypt error"}
            end;
        false ->
            {skip, lists:concat([Alg," is not supported by cryptolib"])}
    end.

priv_enc_pub_dec(Alg, KeyEnc, KeyDec, Padding) ->
    case pubkey_alg_supported(Alg) of
        true ->
            PlainText = <<"Hej på dig">>,
            CryptoText = crypto:private_encrypt(Alg, PlainText, KeyEnc, Padding),
            case crypto:public_decrypt(Alg, CryptoText, KeyDec, Padding) of
                PlainText -> ok;
                _ -> {fail, "Encrypt-decrypt error"}
            end;
        false ->
            {skip, lists:concat([Alg," is not supported by cryptolib"])}
    end.

sign_verify(Alg, Sha, KeySign, KeyVerify) ->
    case pubkey_alg_supported(Alg) of
        true ->
            PlainText = <<"Hej på dig">>,
            Signature = crypto:sign(Alg, Sha, PlainText, KeySign),
            case crypto:verify(Alg, Sha, PlainText, Signature, KeyVerify) of
                true -> ok;
                _ -> {fail, "Sign-verify error"}
            end;
        false ->
            {skip, lists:concat([Alg," is not supported by cryptolib"])}
    end.
