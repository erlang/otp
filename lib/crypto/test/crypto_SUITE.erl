%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
-module(crypto_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).
%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [app,
     appup,
     {group, fips},
     {group, non_fips},
     mod_pow,
     exor,
     rand_uniform,
     rand_plugin,
     rand_plugin_s
    ].

groups() ->
    [{non_fips, [], [{group, md4},
                     {group, md5},
                     {group, ripemd160},
                     {group, sha},
                     {group, sha224},
                     {group, sha256},
                     {group, sha384},
                     {group, sha512},
                     {group, rsa},
                     {group, dss},
                     {group, ecdsa},
                     {group, dh},
                     {group, ecdh},
                     {group, srp},
                     {group, des_cbc},
                     {group, des_cfb},
                     {group, des3_cbc},
                     {group, des3_cbf},
		     {group, des3_cfb},
                     {group, des_ede3},
                     {group, blowfish_cbc},
                     {group, blowfish_ecb},
                     {group, blowfish_cfb64},
                     {group, blowfish_ofb64},
                     {group, aes_cbc128},
                     {group, aes_cfb8},
                     {group, aes_cfb128},
                     {group, aes_cbc256},
                     {group, aes_ige256},
                     {group, rc2_cbc},
                     {group, rc4},
                     {group, aes_ctr},
		     {group, aes_gcm},
		     {group, chacha20_poly1305},
		     {group, aes_cbc}]},
     {fips, [], [{group, no_md4},
                 {group, no_md5},
                 {group, no_ripemd160},
                 {group, sha},
                 {group, sha224},
                 {group, sha256},
                 {group, sha384},
                 {group, sha512},
                 {group, rsa},
                 {group, dss},
                 {group, ecdsa},
                 {group, dh},
                 {group, ecdh},
                 {group, no_srp},
                 {group, no_des_cbc},
                 {group, no_des_cfb},
                 {group, des3_cbc},
                 {group, des3_cbf},
		 {group, des3_cfb},
                 {group, des_ede3},
                 {group, no_blowfish_cbc},
                 {group, no_blowfish_ecb},
                 {group, no_blowfish_cfb64},
                 {group, no_blowfish_ofb64},
                 {group, aes_cbc128},
                 {group, aes_cfb8},
                 {group, aes_cfb128},
                 {group, aes_cbc256},
                 {group, no_aes_ige256},
                 {group, no_rc2_cbc},
                 {group, no_rc4},
                 {group, aes_ctr},
		 {group, aes_gcm},
		 {group, no_chacha20_poly1305},
		 {group, aes_cbc}]},
     {md4, [], [hash]},
     {md5, [], [hash, hmac]},
     {ripemd160, [], [hash]},
     {sha, [], [hash, hmac]},
     {sha224, [], [hash, hmac]},
     {sha256, [], [hash, hmac]},
     {sha384, [], [hash, hmac]},
     {sha512, [], [hash, hmac]},
     {rsa, [], [sign_verify,
                public_encrypt,
                generate
               ]},
     {dss, [], [sign_verify]},
     {ecdsa, [], [sign_verify]},
     {dh, [], [generate_compute]},
     {ecdh, [], [compute, generate]},
     {srp, [], [generate_compute]},
     {des_cbc, [], [block]},
     {des_cfb, [], [block]},
     {des3_cbc,[], [block]},
     {des_ede3,[], [block]},
     {des3_cbf,[], [block]},
     {des3_cfb,[], [block]},
     {rc2_cbc,[], [block]},
     {aes_cbc128,[], [block, cmac]},
     {aes_cfb8,[], [block]},
     {aes_cfb128,[], [block]},
     {aes_cbc256,[], [block, cmac]},
     {aes_ecb,[], [block]},
     {aes_ige256,[], [block]},
     {blowfish_cbc, [], [block]},
     {blowfish_ecb, [], [block]},
     {blowfish_cfb64, [], [block]},
     {blowfish_ofb64,[], [block]},
     {rc4, [], [stream]},
     {aes_ctr, [], [stream]},
     {aes_gcm, [], [aead]},
     {chacha20_poly1305, [], [aead]},
     {aes_cbc, [], [block]},
     {no_md4, [], [no_support, no_hash]},
     {no_md5, [], [no_support, no_hash, no_hmac]},
     {no_ripemd160, [], [no_support, no_hash]},
     {no_srp, [], [no_support, no_generate_compute]},
     {no_des_cbc, [], [no_support, no_block]},
     {no_des_cfb, [], [no_support, no_block]},
     {no_blowfish_cbc, [], [no_support, no_block]},
     {no_blowfish_ecb, [], [no_support, no_block]},
     {no_blowfish_cfb64, [], [no_support, no_block]},
     {no_blowfish_ofb64, [], [no_support, no_block]},
     {no_aes_ige256, [], [no_support, no_block]},
     {no_chacha20_poly1305, [], [no_support, no_aead]},
     {no_rc2_cbc, [], [no_support, no_block]},
     {no_rc4, [], [no_support, no_stream]}
    ].

%%-------------------------------------------------------------------
init_per_suite(Config) ->
    try crypto:start() of
	ok ->
	    try crypto:strong_rand_bytes(1) of
		_ ->
		    Config
	    catch error:low_entropy ->
                    %% We are testing on an OS with low entropy in its random
                    %% seed. So we have to seed it with a binary to get started.

		    %% This is NOT how you want to do seeding, it is just here
		    %% to make the tests pass. Check your OS manual for how you
		    %% really want to seed.
		    {H,M,L} = erlang:now(),
		    Bin = <<H:24,M:20,L:20>>,
		    crypto:rand_seed(<< <<Bin/binary>> || _ <- lists:seq(1,16) >>),
		    Config
	    end
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    application:stop(crypto).

%%-------------------------------------------------------------------
init_per_group(fips, Config) ->
    FIPSConfig = [{fips, true} | Config],
    case crypto:info_fips() of
        enabled ->
            FIPSConfig;
        not_enabled ->
            case crypto:enable_fips_mode(true) of
		true ->
		    enabled = crypto:info_fips(),
		    FIPSConfig;
		false ->
		    {skip, "Failed to enable FIPS mode"}
	    end;
        not_supported ->
            {skip, "FIPS mode not supported"}
    end;
init_per_group(non_fips, Config) ->
    NonFIPSConfig = [{fips, false} | Config],
    case crypto:info_fips() of
        enabled ->
            true = crypto:enable_fips_mode(false),
            not_enabled = crypto:info_fips(),
            NonFIPSConfig;
        _NotEnabled ->
            NonFIPSConfig
    end;
init_per_group(GroupName, Config) ->
    case atom_to_list(GroupName) of
        "no_" ++ TypeStr ->
            %% Negated test case: check the algorithm is not supported
            %% (e.g. due to FIPS mode limitations)
            TypeAtom = list_to_atom(TypeStr),
            [{type, TypeAtom} | group_config(TypeAtom, Config)];
        _Other ->
            %% Regular test case: skip if the algorithm is not supported
            case is_supported(GroupName) of
                true ->
                    [{type, GroupName} | group_config(GroupName, Config)];
                false ->
                    {skip, "Group not supported"}
            end
    end.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(info, Config) ->
    Config;
init_per_testcase(cmac, Config) ->
    case crypto:info_lib() of
        [{<<"OpenSSL">>,LibVer,_}] when is_integer(LibVer), LibVer > 16#10001000 ->
            Config;
        _Else ->
            % The CMAC functionality was introduced in OpenSSL 1.0.1
            {skip, "OpenSSL is too old"}
    end;
init_per_testcase(generate, Config) ->
    case proplists:get_value(type, Config) of
	rsa ->
	    % RSA key generation is a lengthy process, and is only available
	    % if dirty CPU scheduler support was enabled for this runtime.
	    case try erlang:system_info(dirty_cpu_schedulers) of
		     N -> N > 0
		 catch
		     error:badarg -> false
		 end of
		true -> Config;
		false -> {skip, "RSA key generation requires dirty scheduler support."}
	    end;
	_ -> Config
    end;
init_per_testcase(_Name,Config) ->
    Config.

end_per_testcase(info, Config) ->
    Config;
end_per_testcase(_Name,Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
app() ->
    [{doc, "Test that the crypto app file is ok"}].
app(Config) when is_list(Config) ->
    ok = ?t:app_test(crypto).
%%--------------------------------------------------------------------
appup() ->
    [{doc, "Test that the crypto appup file is ok"}].
appup(Config) when is_list(Config) ->
    ok = ?t:appup_test(crypto).
%%--------------------------------------------------------------------
no_support() ->
    [{doc, "Test an algorithm is not reported in the supported list"}].
no_support(Config) when is_list(Config) ->
    Type  = ?config(type, Config),
    false = is_supported(Type).
%%--------------------------------------------------------------------
hash() ->
    [{doc, "Test all different hash functions"}].
hash(Config) when is_list(Config) ->
    {Type, MsgsLE, Digests} = proplists:get_value(hash, Config),
    Msgs = lazy_eval(MsgsLE),
    [LongMsg | _] = lists:reverse(Msgs),
    Inc = iolistify(LongMsg),
    [IncrDigest | _] = lists:reverse(Digests),
    hash(Type, Msgs, Digests),
    hash(Type, lists:map(fun iolistify/1, Msgs), Digests),
    hash_increment(Type, Inc, IncrDigest).
%%--------------------------------------------------------------------
no_hash() ->
    [{doc, "Test all disabled hash functions"}].
no_hash(Config) when is_list(Config) ->
    Type = ?config(type, Config),
    notsup(fun crypto:hash/2, [Type, <<"Hi There">>]),
    notsup(fun crypto:hash_init/1, [Type]).
%%--------------------------------------------------------------------
hmac() ->
     [{doc, "Test all different hmac functions"}].
hmac(Config) when is_list(Config) ->
    {Type, Keys, DataLE, Expected} = proplists:get_value(hmac, Config),
    Data = lazy_eval(DataLE),
    hmac(Type, Keys, Data, Expected),
    hmac(Type, lists:map(fun iolistify/1, Keys), lists:map(fun iolistify/1, Data), Expected),
    hmac_increment(Type).
%%--------------------------------------------------------------------
no_hmac() ->
     [{doc, "Test all disabled hmac functions"}].
no_hmac(Config) when is_list(Config) ->
    Type = ?config(type, Config),
    notsup(fun crypto:hmac/3, [Type, <<"Key">>, <<"Hi There">>]),
    notsup(fun crypto:hmac_init/2, [Type, <<"Key">>]).
%%--------------------------------------------------------------------
cmac() ->
     [{doc, "Test all different cmac functions"}].
cmac(Config) when is_list(Config) ->
    Pairs = proplists:get_value(cmac, Config),
    lists:foreach(fun cmac_check/1, Pairs),
    lists:foreach(fun cmac_check/1, cmac_iolistify(Pairs)).
%%--------------------------------------------------------------------
block() ->
     [{doc, "Test block ciphers"}].
block(Config) when is_list(Config) ->
    Fips = proplists:get_bool(fips, Config),
    Type = ?config(type, Config),
    %% See comment about EVP_CIPHER_CTX_set_key_length in
    %% block_crypt_nif in crypto.c.
    case {Fips, Type} of
	{true, aes_cfb8} ->
	    throw({skip, "Cannot test aes_cfb8 in FIPS mode because of key length issue"});
	{true, aes_cfb128} ->
	    throw({skip, "Cannot test aes_cfb128 in FIPS mode because of key length issue"});
	_ ->
	    ok
    end,

    Blocks = proplists:get_value(block, Config),
    lists:foreach(fun block_cipher/1, Blocks),
    lists:foreach(fun block_cipher/1, block_iolistify(Blocks)),
    lists:foreach(fun block_cipher_increment/1, block_iolistify(Blocks)).

%%--------------------------------------------------------------------
no_block() ->
     [{doc, "Test disabled block ciphers"}].
no_block(Config) when is_list(Config) ->
    Blocks = proplists:get_value(block, Config),
    Args = case Blocks of
	       [{_Type, _Key, _PlainText} = A | _] ->
		   tuple_to_list(A);
	       [{_Type, _Key, _IV, _PlainText} = A | _] ->
		   tuple_to_list(A);
	       [{Type, Key, IV, PlainText, _CipherText} | _] ->
		   [Type, Key, IV, PlainText]
	   end,
    N = length(Args),
    notsup(fun crypto:block_encrypt/N, Args),
    notsup(fun crypto:block_decrypt/N, Args).
%%--------------------------------------------------------------------
no_aead() ->
     [{doc, "Test disabled aead ciphers"}].
no_aead(Config) when is_list(Config) ->
    [{Type, Key, PlainText, Nonce, AAD, CipherText, CipherTag} | _] =
	proplists:get_value(aead, Config),
    EncryptArgs = [Type, Key, Nonce, {AAD, PlainText}],
    DecryptArgs = [Type, Key, Nonce, {AAD, CipherText, CipherTag}],
    notsup(fun crypto:block_encrypt/4, EncryptArgs),
    notsup(fun crypto:block_decrypt/4, DecryptArgs).
%%--------------------------------------------------------------------
stream() ->
      [{doc, "Test stream ciphers"}].
stream(Config) when is_list(Config) ->
    Streams = lazy_eval(proplists:get_value(stream, Config)),

    lists:foreach(fun stream_cipher/1, Streams),
    lists:foreach(fun stream_cipher/1, stream_iolistify(Streams)),
    lists:foreach(fun stream_cipher_incment/1, stream_iolistify(Streams)).
%%--------------------------------------------------------------------
no_stream() ->
      [{doc, "Test disabled stream ciphers"}].
no_stream(Config) when is_list(Config) ->
    Type = ?config(type, Config),
    notsup(fun crypto:stream_init/2, [Type, <<"Key">>]).

%%--------------------------------------------------------------------
aead() ->
      [{doc, "Test AEAD ciphers"}].
aead(Config) when is_list(Config) ->
    AEADs = lazy_eval(proplists:get_value(aead, Config)),

    FilteredAEADs =
	case proplists:get_bool(fips, Config) of
	    false ->
		AEADs;
	    true ->
		%% In FIPS mode, the IV length must be at least 12 bytes.
		lists:filter(
		  fun(Tuple) ->
			  IVLen = byte_size(element(4, Tuple)),
			  IVLen >= 12
		  end, AEADs)
	end,

    lists:foreach(fun aead_cipher/1, FilteredAEADs).

%%-------------------------------------------------------------------- 
sign_verify() ->
     [{doc, "Sign/verify digital signatures"}].
sign_verify(Config) when is_list(Config) ->
    SignVerify = proplists:get_value(sign_verify, Config),
    lists:foreach(fun do_sign_verify/1, SignVerify).

%%-------------------------------------------------------------------- 
public_encrypt() ->
     [{doc, "Test public_encrypt/decrypt and private_encrypt/decrypt functions. "}].
public_encrypt(Config) when is_list(Config) ->
    Params = proplists:get_value(pub_priv_encrypt, Config),
    lists:foreach(fun do_public_encrypt/1, Params),
    lists:foreach(fun do_private_encrypt/1, Params).

%%--------------------------------------------------------------------
generate_compute() ->
     [{doc, " Test crypto:genarate_key and crypto:compute_key"}].
generate_compute(Config) when is_list(Config) ->
    GenCom = proplists:get_value(generate_compute, Config),
    lists:foreach(fun do_generate_compute/1, GenCom).
%%--------------------------------------------------------------------
no_generate_compute() ->
     [{doc, "Test crypto:genarate_key and crypto:compute_key "
       "for disabled algorithms"}].
no_generate_compute(Config) when is_list(Config) ->
    %% This test is specific to the SRP protocol
    srp = ?config(type, Config),
    {srp,
     UserPrivate, UserGenParams, UserComParams,
     HostPublic, HostPrivate, HostGenParams, HostComParams,
     _SessionKey} = srp3(),
    UserPublic = HostPublic,                    % use a fake public key
    notsup(fun crypto:generate_key/3, [srp, UserGenParams, UserPrivate]),
    notsup(fun crypto:generate_key/3, [srp, HostGenParams, HostPrivate]),
    notsup(fun crypto:compute_key/4,
           [srp, HostPublic, {UserPublic, UserPrivate}, UserComParams]),
    notsup(fun crypto:compute_key/4,
           [srp, UserPublic, {HostPublic, HostPrivate}, HostComParams]).
%%--------------------------------------------------------------------
compute() ->
     [{doc, " Test crypto:compute_key"}].
compute(Config) when is_list(Config) ->
    Gen = proplists:get_value(compute, Config),
    lists:foreach(fun do_compute/1, Gen).
%%--------------------------------------------------------------------
generate() ->
     [{doc, " Test crypto:generate_key"}].
generate(Config) when is_list(Config) ->
    Gen = proplists:get_value(generate, Config),
    lists:foreach(fun do_generate/1, Gen).
%%--------------------------------------------------------------------
mod_pow() ->
    [{doc, "mod_pow testing (A ^ M % P with bignums)"}].
mod_pow(Config) when is_list(Config) ->
    mod_pow_aux_test(2, 5, 10, 8).
%%--------------------------------------------------------------------
exor() ->
    [{doc, "Test the exor function"}].
exor(Config) when is_list(Config) ->
    do_exor(<<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>),
    do_exor(term_to_binary(lists:seq(1, 1000000))).
%%--------------------------------------------------------------------
rand_uniform() ->
    [{doc, "rand_uniform and random_bytes testing"}].
rand_uniform(Config) when is_list(Config) ->
    rand_uniform_aux_test(10),
    10 = byte_size(crypto:strong_rand_bytes(10)).

%%--------------------------------------------------------------------
rand_plugin() ->
    [{doc, "crypto rand plugin testing (implicit state / process dictionary)"}].
rand_plugin(Config) when is_list(Config) ->
    rand_plugin_aux(implicit_state).

rand_plugin_s() ->
    [{doc, "crypto rand plugin testing (explicit state)"}].
rand_plugin_s(Config) when is_list(Config) ->
    rand_plugin_aux(explicit_state).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
hash(_, [], []) ->
    ok;
hash(Type, [Msg | RestMsg], [Digest| RestDigest]) ->
    case crypto:hash(Type, Msg) of
	Digest ->
	    hash(Type, RestMsg, RestDigest);
	Other ->
	    ct:fail({{crypto, hash, [Type, Msg]}, {expected, Digest}, {got, Other}})
    end.

hash_increment(Type, Increments, Digest) ->
    State = crypto:hash_init(Type),
    case hash_increment(State, Increments) of
	Digest ->
	    ok;
	Other ->
	    ct:fail({{crypto, "hash_init/update/final", [Type, Increments]}, {expected, Digest}, {got, Other}})  
    end.

hash_increment(State, []) ->
    crypto:hash_final(State);
hash_increment(State0, [Increment | Rest]) ->
    State = crypto:hash_update(State0, Increment),
    hash_increment(State, Rest).

hmac(_, [],[],[]) ->
    ok;
hmac(sha = Type, [Key | Keys], [ <<"Test With Truncation">> = Data| Rest], [Expected | Expects]) ->
    case crypto:hmac(Type, Key, Data, 20) of
	Expected ->
	    ok;
	Other ->
	    ct:fail({{crypto, hmac, [Type, Key, Data]}, {expected, Expected}, {got, Other}})
    end,  
    hmac(Type, Keys, Rest, Expects);

hmac(Type, [Key | Keys], [ <<"Test With Truncation">> = Data| Rest], [Expected | Expects]) ->
    case crypto:hmac(Type, Key, Data, 16) of
	Expected ->
	    ok;
	Other ->
	    ct:fail({{crypto, hmac, [Type, Key, Data]}, {expected, Expected}, {got, Other}})
    end,  
    hmac(Type, Keys, Rest, Expects);

hmac(Type, [Key | Keys], [Data| Rest], [Expected | Expects]) ->
    case crypto:hmac(Type, Key, Data) of
	Expected ->
	    ok;
	Other ->
	    ct:fail({{crypto, hmac, [Type, Key, Data]}, {expected, Expected}, {got, Other}})
    end,  
    hmac(Type, Keys, Rest, Expects).

hmac_increment(Type) ->
    Key = hmac_key(Type),
    Increments = hmac_inc(Type),
    Expected = crypto:hmac(Type, Key, lists:flatten(Increments)),
    State = crypto:hmac_init(Type, Key),
    case hmac_increment(State, Increments) of
	Expected ->
	    ok;
	Other ->
	    ct:fail({{crypto, "hmac_init/update/final", [Type, Increments]}, {expected, Expected}, {got, Other}})  
    end.

hmac_increment(State, []) ->
    crypto:hmac_final(State);
hmac_increment(State0, [Increment | Rest]) ->
    State = crypto:hmac_update(State0, Increment),
    hmac_increment(State, Rest).

cmac_check({Type, Key, Text, CMac}) ->
    ExpCMac = iolist_to_binary(CMac),
    case crypto:cmac(Type, Key, Text) of
        ExpCMac ->
            ok;
        Other ->
            ct:fail({{crypto, cmac, [Type, Key, Text]}, {expected, ExpCMac}, {got, Other}})
    end;
cmac_check({Type, Key, Text, Size, CMac}) ->
    ExpCMac = iolist_to_binary(CMac),
    case crypto:cmac(Type, Key, Text, Size) of
        ExpCMac ->
            ok;
        Other ->
            ct:fail({{crypto, cmac, [Type, Key, Text, Size]}, {expected, ExpCMac}, {got, Other}})
    end.

block_cipher({Type, Key,  PlainText}) ->
    Plain = iolist_to_binary(PlainText),
    CipherText = crypto:block_encrypt(Type, Key, PlainText),
    case crypto:block_decrypt(Type, Key, CipherText) of
	Plain ->
	    ok;
	Other ->
	    ct:fail({{crypto, block_decrypt, [Type, Key, CipherText]}, {expected, Plain}, {got, Other}})
    end;

block_cipher({Type, Key,  IV, PlainText}) ->
    Plain = iolist_to_binary(PlainText),
    CipherText = crypto:block_encrypt(Type, Key, IV, PlainText),
    case crypto:block_decrypt(Type, Key, IV, CipherText) of
	Plain ->
	    ok;
	Other ->
	    ct:fail({{crypto, block_decrypt, [Type, Key, IV, CipherText]}, {expected, Plain}, {got, Other}})
    end;

block_cipher({Type, Key, IV, PlainText, CipherText}) ->
    Plain = iolist_to_binary(PlainText),
    case crypto:block_encrypt(Type, Key, IV, Plain) of
	CipherText ->
	    ok;
	Other0 ->
	    ct:fail({{crypto, block_encrypt, [Type, Key, IV, Plain]}, {expected, CipherText}, {got, Other0}})
    end,
    case crypto:block_decrypt(Type, Key, IV, CipherText) of
	Plain ->
	    ok;
	Other1 ->
	    ct:fail({{crypto, block_decrypt, [Type, Key, IV, CipherText]}, {expected, Plain}, {got, Other1}})
    end.

block_cipher_increment({Type, Key, IV, PlainTexts})
  when Type == des_cbc; Type == aes_cbc; Type == des3_cbc ->
    block_cipher_increment(Type, Key, IV, IV, PlainTexts, iolist_to_binary(PlainTexts), []);
block_cipher_increment({Type, Key, IV, PlainTexts, _CipherText}) when Type == aes_cbc ->
    Plain = iolist_to_binary(PlainTexts),
    Blocks = [iolistify(Block) || << Block:128/bitstring >> <= Plain],
    block_cipher_increment(Type, Key, IV, IV, Blocks, Plain, []);
block_cipher_increment({_Type, _, _, _}) ->
    ok;
block_cipher_increment({_,_,_}) ->
    ok.
block_cipher_increment(Type, Key, IV0, _IV, [], Plain, Acc) ->
    CipherText = iolist_to_binary(lists:reverse(Acc)),
    case crypto:block_decrypt(Type, Key, IV0, CipherText) of
	Plain ->
	    ok;
	Other ->
	    ct:fail({{crypto, block_decrypt, [Type, Key, IV0, CipherText]}, {expected, Plain}, {got, Other}})
    end;
block_cipher_increment(Type, Key, IV0, IV, [PlainText | PlainTexts], Plain, Acc) ->
    CipherText = crypto:block_encrypt(Type, Key, IV, PlainText),
    NextIV = crypto:next_iv(Type, CipherText),
    block_cipher_increment(Type, Key, IV0, NextIV, PlainTexts, Plain, [CipherText | Acc]).

stream_cipher({Type, Key, PlainText}) ->
    Plain = iolist_to_binary(PlainText),
    State = crypto:stream_init(Type, Key),
    {_, CipherText} = crypto:stream_encrypt(State, PlainText),
    case crypto:stream_decrypt(State, CipherText) of
	{_, Plain} ->
	    ok;
	Other ->
	    ct:fail({{crypto, stream_decrypt, [State, CipherText]}, {expected, PlainText}, {got, Other}})
    end;
stream_cipher({Type, Key, IV, PlainText}) ->
    Plain = iolist_to_binary(PlainText),
    State = crypto:stream_init(Type, Key, IV),
    {_, CipherText} = crypto:stream_encrypt(State, PlainText),
    case crypto:stream_decrypt(State, CipherText) of
	{_, Plain} ->
	    ok;
	Other ->
	    ct:fail({{crypto, stream_decrypt, [State, CipherText]}, {expected, PlainText}, {got, Other}})
    end.

stream_cipher_incment({Type, Key, PlainTexts}) ->
    State = crypto:stream_init(Type, Key),
    stream_cipher_incment(State, State, PlainTexts, [], iolist_to_binary(PlainTexts));
stream_cipher_incment({Type, Key, IV, PlainTexts}) ->
    State = crypto:stream_init(Type, Key, IV),
    stream_cipher_incment(State, State, PlainTexts, [], iolist_to_binary(PlainTexts)).

stream_cipher_incment(_State, OrigState, [], Acc, Plain) ->
    CipherText = iolist_to_binary(lists:reverse(Acc)),
    case crypto:stream_decrypt(OrigState, CipherText) of
	{_, Plain} ->
	    ok;
	Other ->
	    ct:fail({{crypto, stream_decrypt, [OrigState, CipherText]}, {expected, Plain}, {got, Other}})
    end;
stream_cipher_incment(State0, OrigState, [PlainText | PlainTexts], Acc, Plain) ->
    {State, CipherText} = crypto:stream_encrypt(State0, PlainText),
    stream_cipher_incment(State, OrigState, PlainTexts, [CipherText | Acc], Plain).

aead_cipher({Type, Key, PlainText, IV, AAD, CipherText, CipherTag}) ->
    Plain = iolist_to_binary(PlainText),
    case crypto:block_encrypt(Type, Key, IV, {AAD, Plain}) of
	{CipherText, CipherTag} ->
	    ok;
	Other0 ->
	    ct:fail({{crypto, block_encrypt, [Plain, PlainText]}, {expected, {CipherText, CipherTag}}, {got, Other0}})
    end,
    case crypto:block_decrypt(Type, Key, IV, {AAD, CipherText, CipherTag}) of
	Plain ->
	    ok;
	Other1 ->
	    ct:fail({{crypto, block_decrypt, [CipherText]}, {expected, Plain}, {got, Other1}})
    end;
aead_cipher({Type, Key, PlainText, IV, AAD, CipherText, CipherTag, TagLen}) ->
    <<TruncatedCipherTag:TagLen/binary, _/binary>> = CipherTag,
    Plain = iolist_to_binary(PlainText),
    case crypto:block_encrypt(Type, Key, IV, {AAD, Plain, TagLen}) of
	{CipherText, TruncatedCipherTag} ->
	    ok;
	Other0 ->
	    ct:fail({{crypto, block_encrypt, [Plain, PlainText]}, {expected, {CipherText, TruncatedCipherTag}}, {got, Other0}})
    end,
    case crypto:block_decrypt(Type, Key, IV, {AAD, CipherText, TruncatedCipherTag}) of
	Plain ->
	    ok;
	Other1 ->
	    ct:fail({{crypto, block_decrypt, [CipherText]}, {expected, Plain}, {got, Other1}})
    end.

do_sign_verify({Type, Hash, Public, Private, Msg}) ->
    Signature = crypto:sign(Type, Hash, Msg, Private),
    case crypto:verify(Type, Hash, Msg, Signature, Public) of
	true ->
	    negative_verify(Type, Hash, Msg, <<10,20>>, Public);
	false ->
	    ct:fail({{crypto, verify, [Type, Hash, Msg, Signature, Public]}})
    end. 

negative_verify(Type, Hash, Msg, Signature, Public) ->
    case crypto:verify(Type, Hash, Msg, Signature, Public) of
	true ->
	    ct:fail({{crypto, verify, [Type, Hash, Msg, Signature, Public]}, should_fail});
	false ->
	    ok
    end.

do_public_encrypt({Type, Public, Private, Msg, Padding}) ->
    PublicEcn = (catch crypto:public_encrypt(Type, Msg, Public, Padding)),
    case crypto:private_decrypt(Type, PublicEcn, Private, Padding) of
	Msg ->
	    ok;
	Other ->
	    ct:fail({{crypto, private_decrypt, [Type, PublicEcn, Private, Padding]}, {expected, Msg}, {got, Other}})
    end. 

do_private_encrypt({_Type, _Public, _Private, _Msg, rsa_pkcs1_oaep_padding}) ->
    ok; %% Not supported by openssl
do_private_encrypt({Type, Public, Private, Msg, Padding}) ->
    PrivEcn = (catch crypto:private_encrypt(Type, Msg, Private, Padding)),
    case crypto:public_decrypt(rsa, PrivEcn, Public, Padding) of
	Msg ->
	    ok;
	Other ->
	    ct:fail({{crypto, public_decrypt, [Type, PrivEcn, Public, Padding]}, {expected, Msg}, {got, Other}})
    end.
     
do_generate_compute({srp = Type, UserPrivate, UserGenParams, UserComParams,
		     HostPublic, HostPrivate, HostGenParams, HostComParam, SessionKey}) ->
    {UserPublic, UserPrivate} = crypto:generate_key(Type, UserGenParams, UserPrivate),
    {HostPublic, HostPrivate} = crypto:generate_key(Type, HostGenParams, HostPrivate),
    SessionKey = crypto:compute_key(Type, HostPublic, {UserPublic, UserPrivate},
     				    UserComParams),
    SessionKey = crypto:compute_key(Type, UserPublic, {HostPublic, HostPrivate},
				    HostComParam);
do_generate_compute({dh, P, G}) ->
    {UserPub, UserPriv} = crypto:generate_key(dh, [P, G]),
    {HostPub, HostPriv} = crypto:generate_key(dh, [P, G]),
    SharedSecret = crypto:compute_key(dh, HostPub, UserPriv, [P, G]),
    SharedSecret = crypto:compute_key(dh, UserPub, HostPriv, [P, G]).
    
do_compute({ecdh = Type, Pub, Priv, Curve, SharedSecret}) ->
    Secret = crypto:compute_key(Type, Pub, Priv, Curve),
     case Secret of
	 SharedSecret ->
	     ok;
	 Other ->
	     ct:fail({{crypto, compute_key, [Type, Pub, Priv, Curve]}, {expected, SharedSecret}, {got, Other}})
     end.

do_generate({ecdh = Type, Curve, Priv, Pub}) ->
    case crypto:generate_key(Type, Curve, Priv) of
	{Pub, _} ->
	    ok;
	{Other, _} ->
	    ct:fail({{crypto, generate_key, [Type, Priv, Curve]}, {expected, Pub}, {got, Other}})
    end;
do_generate({rsa = Type, Mod, Exp}) ->
    {Pub,Priv} = crypto:generate_key(Type, {Mod,Exp}),
    do_sign_verify({rsa, sha256, Pub, Priv, rsa_plain()}).

notsup(Fun, Args) ->
    Result =
        try
            {error, {return, apply(Fun, Args)}}
        catch
            error:notsup ->
                ok;
            Class:Error ->
                {error, {Class, Error}}
        end,
    case Result of
        ok ->
            ok;
        {error, Value} ->
            {module, Module} = erlang:fun_info(Fun, module),
            {name,   Name}   = erlang:fun_info(Fun, name),
            ct:fail({{Module, Name, Args}, {expected, {error, notsup}}, {got, Value}})
    end.

hexstr2point(X, Y) ->
    <<4:8, (hexstr2bin(X))/binary, (hexstr2bin(Y))/binary>>.

hexstr2bin(S) ->
    list_to_binary(hexstr2list(S)).

hexstr2list([X,Y|T]) ->
    [mkint(X)*16 + mkint(Y) | hexstr2list(T)];
hexstr2list([]) ->
    [].
mkint(C) when $0 =< C, C =< $9 ->
    C - $0;
mkint(C) when $A =< C, C =< $F ->
    C - $A + 10;
mkint(C) when $a =< C, C =< $f ->
    C - $a + 10.

is_supported(Group) ->
    lists:member(Group, lists:append([Algo ||  {_, Algo}  <- crypto:supports()])). 

cmac_iolistify(Blocks) ->
    lists:map(fun do_cmac_iolistify/1, Blocks).
block_iolistify(Blocks) ->
    lists:map(fun do_block_iolistify/1, Blocks).
stream_iolistify(Streams) ->
    lists:map(fun do_stream_iolistify/1, Streams).

do_cmac_iolistify({Type, Key, Text, CMac}) ->
    {Type, iolistify(Key), iolistify(Text), CMac};
do_cmac_iolistify({Type, Key, Text, Size, CMac}) ->
    {Type, iolistify(Key), iolistify(Text), Size, CMac}.

do_stream_iolistify({Type, Key, PlainText}) ->
    {Type, iolistify(Key), iolistify(PlainText)};
do_stream_iolistify({Type, Key, IV, PlainText}) ->
    {Type, iolistify(Key), IV, iolistify(PlainText)}.

do_block_iolistify({des_cbc = Type, Key, IV, PlainText}) ->
    {Type, Key, IV, des_iolistify(PlainText)};
do_block_iolistify({des3_cbc = Type, Key, IV, PlainText}) ->
    {Type, Key, IV, des_iolistify(PlainText)};
do_block_iolistify({des3_cbf = Type, Key, IV, PlainText}) ->
    {Type, Key, IV, des_iolistify(PlainText)};
do_block_iolistify({des3_cfb = Type, Key, IV, PlainText}) ->
    {Type, Key, IV, des_iolistify(PlainText)};
do_block_iolistify({des_ede3 = Type, Key, IV, PlainText}) ->
    {Type, Key, IV, des_iolistify(PlainText)};
do_block_iolistify({Type, Key, PlainText}) ->
    {Type, iolistify(Key), iolistify(PlainText)};
do_block_iolistify({Type, Key, IV, PlainText}) ->
    {Type, iolistify(Key), IV, iolistify(PlainText)};
do_block_iolistify({Type, Key, IV, PlainText, CipherText}) ->
    {Type, iolistify(Key), IV, iolistify(PlainText), CipherText}.

iolistify(<<"Test With Truncation">>)->
    %% Do not iolistify as it spoils this special case
    <<"Test With Truncation">>;
iolistify(Msg) when is_binary(Msg) ->
    Length = erlang:byte_size(Msg),
    Split = Length div 2,
    List0 = binary_to_list(Msg),
   case lists:split(Split, List0) of
       {[Element | List1], List2} ->
	   [[Element], List1, List2];
       {List1, List2}->
	   [List1, List2]
   end;
iolistify(Msg) ->
    iolistify(list_to_binary(Msg)).

des_iolistify(Msg) ->    
    des_iolist(erlang:byte_size(Msg) div 8, Msg, []).

des_iolist(1, Msg, Acc) ->
    lists:reverse([Msg | Acc]);
des_iolist(Split, Msg, Acc) ->
    <<Part:8/binary, Rest/binary>> = Msg,
    des_iolist(Split-1, Rest, [Part | Acc]).

%%--------------------------------------------------------------------
mod_pow_aux_test(_, _, _, 0) ->
    ok;
mod_pow_aux_test(B, E, M, N) ->
    Result = crypto:bytes_to_integer(crypto:mod_pow(B, E, M)),
    Result = ipow(B, E, M),
    mod_pow_aux_test(B, E*E+1, M*M+1, N-1).

%% mod_exp in erlang (copied from jungerl's ssh_math.erl)
ipow(A, B, M) when M > 0, B >= 0 ->
    if A == 1 -> 
 	    1;
       true -> 
 	    ipow(A, B, M, 1)
    end.

ipow(A, 1, M, Prod) ->
    (A*Prod) rem M;
ipow(_A, 0, _M, Prod) ->
    Prod;
ipow(A, B, M, Prod)  ->
    B1 = B bsr 1,
    A1 = (A*A) rem M,
    if B - B1 == B1 ->
	    ipow(A1, B1, M, Prod);
       true ->
	    ipow(A1, B1, M, (A*Prod) rem M)
    end.

do_exor(B) ->
    Z1 = zero_bin(B),
    Z1 = crypto:exor(B, B),
    B1 = crypto:strong_rand_bytes(100),
    B2 = crypto:strong_rand_bytes(100),
    Z2 = zero_bin(B1),
    Z2 = crypto:exor(B1, B1),
    Z2 = crypto:exor(B2, B2),
    R = xor_bytes(B1, B2),
    R = crypto:exor(B1, B2).

zero_bin(N) when is_integer(N) ->
    N8 = N * 8,
    <<0:N8/integer>>;
zero_bin(B) when is_binary(B) ->
    zero_bin(size(B)).
xor_bytes(Bin1, Bin2) when is_binary(Bin1), is_binary(Bin2) ->
    L1 = binary_to_list(Bin1),
    L2 = binary_to_list(Bin2),
    list_to_binary(xor_bytes(L1, L2));
xor_bytes(L1, L2) ->
    xor_bytes(L1, L2, []).

xor_bytes([], [], Acc) ->
    lists:reverse(Acc);
xor_bytes([N1 | Tl1], [N2 | Tl2], Acc) ->
    xor_bytes(Tl1, Tl2, [N1 bxor N2 | Acc]).
rand_uniform_aux_test(0) ->
    ok;
rand_uniform_aux_test(N) ->
    L = N*1000,
    H = N*100000+1,
    crypto_rand_uniform(L, H),
    crypto_rand_uniform(-L, L),
    crypto_rand_uniform(-H, -L),
    crypto_rand_uniform(-H, L),
    rand_uniform_aux_test(N-1).

crypto_rand_uniform(L,H) ->
    R1 = crypto:rand_uniform(L, H),
    case (R1 >= L) and (R1 < H) of
	true  ->
	    ok;
	false ->
	    ct:fail({"Not in interval", R1, L, H})
    end.

foldallmap(_Fun, AccN, []) ->
    {true, AccN};
foldallmap(Fun, AccN, [H|T]) ->
    case Fun(H, AccN) of
        {true, AccM} -> foldallmap(Fun, AccM, T);
        {{false, Result}, AccM} -> {Result, AccM}
    end.

allmap(_Fun, []) ->
    true;
allmap(Fun, [H|T]) ->
    case Fun(H) of
        true -> allmap(Fun, T);
        {false, Result} -> Result
    end.

rand_plugin_aux(StateType) ->
    {Seeder, SeedExporter, FloatGenerator, IntegerGenerator} = rand_plugin_functions(StateType),
    State0 = Seeder(),
    {crypto, no_seed} = SeedExporter(State0),
    {FloatTestResult, State1} = rand_plugin_aux_floats(State0, FloatGenerator),
    case FloatTestResult of
        true ->
            {IntegerTestResult, _State2} = rand_plugin_aux_integers(State1, IntegerGenerator),
            IntegerTestResult;
        {false, _} ->
            FloatTestResult
    end.

% returns {Seeder, SeedExporter, FloatGenerator, IntegerGenerator} with consistent signatures
rand_plugin_functions(implicit_state) ->
    {fun () -> crypto:rand_seed(), implicit_state end,
     fun (implicit_state) -> rand:export_seed() end,
     fun (implicit_state) -> {rand:uniform(), implicit_state} end,
     fun (N, implicit_state) -> {rand:uniform(N), implicit_state} end};
rand_plugin_functions(explicit_state) ->
    {fun crypto:rand_seed_s/0,
     fun rand:export_seed_s/1,
     fun rand:uniform_s/1,
     fun rand:uniform_s/2}.

rand_plugin_aux_floats(State0, FloatGenerator) ->
    {FloatSamples, State1} =
        lists:mapfoldl(
          fun (_, StateAcc) ->
                  FloatGenerator(StateAcc)
          end,
          State0,
          lists:seq(1, 10000)),

    {allmap(
       fun (V) ->
               (V >= 0.0 andalso V < 1.0)
               orelse {false, ct:fail({"Float sample not in interval", V, 0.0, 1.0})}
       end,
       FloatSamples),
     State1}.

rand_plugin_aux_integers(State0, IntegerGenerator) ->
    MaxIntegerCeiling = 1 bsl 32,
    {IntegerCeilings, State1} =
        lists:mapfoldl(
          fun (_, StateAcc) ->
                  IntegerGenerator(MaxIntegerCeiling, StateAcc)
          end,
          State0,
          lists:seq(1, 100)),

    foldallmap(
      fun (Ceiling, StateAcc) ->
              case Ceiling >= 1 andalso Ceiling =< MaxIntegerCeiling of
                  false ->
                      {{false, ct:fail({"Integer ceiling not in interval",
                                        Ceiling, 1, MaxIntegerCeiling})},
                       StateAcc};
                  true ->
                      foldallmap(
                        fun (_, SubStateAcc) ->
                                {Sample, NewSubStateAcc} = IntegerGenerator(Ceiling, SubStateAcc),
                                case Sample >= 1 andalso Sample =< Ceiling of
                                    false ->
                                        {{false, ct:fail({"Integer sample not in interval",
                                                          Sample, 1, Ceiling})},
                                         NewSubStateAcc};
                                    true ->
                                        {true, NewSubStateAcc}
                                end
                        end,
                        StateAcc,
                        lists:seq(1, 100))
              end
      end,
      State1,
      IntegerCeilings).

%%--------------------------------------------------------------------
%% Test data ------------------------------------------------
%%--------------------------------------------------------------------
group_config(md4 = Type, Config) ->
    Msgs = rfc_1321_msgs(),
    Digests = rfc_1321_md4_digests(),
    [{hash, {Type, Msgs, Digests}} | Config];
group_config(md5 = Type, Config) ->
    Msgs = rfc_1321_msgs(),
    Digests = rfc_1321_md5_digests(),
    Keys = rfc_2202_md5_keys() ++ [long_hmac_key(md5)],
    Data = rfc_2202_msgs() ++ [long_msg()],
    Hmac = rfc_2202_hmac_md5()  ++ [long_hmac(md5)],
    [{hash, {Type, Msgs, Digests}}, {hmac, {Type, Keys, Data, Hmac}} | Config];
group_config(ripemd160 = Type, Config) ->
    Msgs = ripemd160_msgs(),
    Digests = ripemd160_digests(),
   [{hash, {Type, Msgs, Digests}} | Config];
group_config(sha = Type, Config) ->
    Msgs = [rfc_4634_test1(), rfc_4634_test2_1(),long_msg()],
    Digests = rfc_4634_sha_digests() ++ [long_sha_digest()],
    Keys = rfc_2202_sha_keys() ++ [long_hmac_key(sha)],
    Data = rfc_2202_msgs() ++ [long_msg()],
    Hmac = rfc_2202_hmac_sha()  ++ [long_hmac(sha)],
    [{hash, {Type, Msgs, Digests}}, {hmac, {Type, Keys, Data, Hmac}} | Config];
group_config(sha224 = Type, Config) ->
    Msgs = [rfc_4634_test1(), rfc_4634_test2_1()], 
    Digests = rfc_4634_sha224_digests(),
    Keys = rfc_4231_keys(),
    Data = rfc_4231_msgs(),
    Hmac = rfc4231_hmac_sha224(),
   [{hash, {Type, Msgs, Digests}}, {hmac, {Type, Keys, Data, Hmac}}  | Config];
group_config(sha256 = Type, Config) ->
    Msgs =   [rfc_4634_test1(), rfc_4634_test2_1(), long_msg()],
    Digests = rfc_4634_sha256_digests()  ++ [long_sha256_digest()],
    Keys = rfc_4231_keys() ++ [long_hmac_key(sha256)],
    Data = rfc_4231_msgs()  ++ [long_msg()],
    Hmac = rfc4231_hmac_sha256()  ++ [long_hmac(sha256)],
    [{hash, {Type, Msgs, Digests}}, {hmac, {Type, Keys, Data, Hmac}}  | Config];
group_config(sha384 = Type, Config) ->
    Msgs =  [rfc_4634_test1(), rfc_4634_test2(), long_msg()],
    Digests = rfc_4634_sha384_digests()  ++ [long_sha384_digest()],
    Keys = rfc_4231_keys() ++ [long_hmac_key(sha384)],
    Data = rfc_4231_msgs()  ++ [long_msg()],
    Hmac = rfc4231_hmac_sha384()  ++ [long_hmac(sha384)],
    [{hash, {Type, Msgs, Digests}}, {hmac, {Type, Keys, Data, Hmac}}  | Config];
group_config(sha512 = Type, Config) ->
    Msgs =  [rfc_4634_test1(), rfc_4634_test2(), long_msg()],
    Digests = rfc_4634_sha512_digests() ++ [long_sha512_digest()],
    Keys = rfc_4231_keys() ++ [long_hmac_key(sha512)],
    Data = rfc_4231_msgs() ++ [long_msg()],
    Hmac = rfc4231_hmac_sha512() ++ [long_hmac(sha512)],
    [{hash, {Type, Msgs, Digests}}, {hmac, {Type, Keys, Data, Hmac}}  | Config];
group_config(rsa = Type, Config) ->
    Msg = rsa_plain(),
    Public = rsa_public(),
    Private = rsa_private(),
    PublicS = rsa_public_stronger(),
    PrivateS = rsa_private_stronger(),
    SignVerify =
        case ?config(fips, Config) of
            true ->
                %% Use only the strong keys in FIPS mode
                sign_verify_tests(Type, Msg,
                                  PublicS, PrivateS,
                                  PublicS, PrivateS);
            false ->
                sign_verify_tests(Type, Msg,
                                  Public,  Private,
                                  PublicS, PrivateS)
        end,
    MsgPubEnc = <<"7896345786348 Asldi">>,
    PubPrivEnc = [{rsa, PublicS, PrivateS, MsgPubEnc, rsa_pkcs1_padding},
                  rsa_oaep(),
                  no_padding()
                 ],
    Generate = [{rsa, 2048, 17}, {rsa, 3072, 65537}],
    [{sign_verify, SignVerify}, {pub_priv_encrypt, PubPrivEnc}, {generate, Generate} | Config];
group_config(dss = Type, Config) ->
    Msg = dss_plain(),
    Public = dss_params() ++ [dss_public()], 
    Private = dss_params() ++ [dss_private()], 
    SignVerify = [{Type, sha, Public, Private, Msg}],
    [{sign_verify, SignVerify} | Config];

group_config(ecdsa = Type, Config) ->
    {Private, Public} = ec_key_named(),
    Msg = ec_msg(),
    SignVerify = [{Type, sha, Public, Private, Msg}],
    [{sign_verify, SignVerify} | Config];
group_config(srp, Config) ->
    GenerateCompute = [srp3(), srp6(), srp6a(), srp6a_smaller_prime()],
    [{generate_compute, GenerateCompute} | Config];
group_config(ecdh, Config) ->
    Compute = ecdh(),
    Generate = ecc(),
    [{compute, Compute}, {generate, Generate} | Config];
group_config(dh, Config) ->
    GenerateCompute = [dh()],
    [{generate_compute, GenerateCompute} | Config];
group_config(des_cbc, Config) ->
    Block = des_cbc(),
    [{block, Block} | Config];
group_config(des_cfb, Config) ->
    Block = des_cfb(),
    [{block, Block} | Config];
group_config(des3_cbc, Config) ->
    Block = des3_cbc(),
    [{block, Block} | Config];
group_config(des3_cbf, Config) ->
    Block = des3_cbf(),
    [{block, Block} | Config];
group_config(des3_cfb, Config) ->
    Block = des3_cfb(),
    [{block, Block} | Config];
group_config(des_ede3, Config) ->
    Block = des_ede3(),
    [{block, Block} | Config];
group_config(rc2_cbc, Config) ->
    Block = rc2_cbc(),
    [{block, Block} | Config];
group_config(aes_cbc128 = Type, Config) ->
    Block = aes_cbc128(),
    Pairs = cmac_nist(Type),
    [{block, Block}, {cmac, Pairs} | Config];
group_config(aes_cbc256 = Type, Config) ->
    Block = aes_cbc256(),
    Pairs = cmac_nist(Type),
    [{block, Block}, {cmac, Pairs} | Config];
group_config(aes_ecb, Config) ->
    Block = aes_ecb(),
    [{block, Block} | Config];    
group_config(aes_ige256, Config) ->
    Block = aes_ige256(),
    [{block, Block} | Config];
group_config(aes_cfb8, Config) ->
    Block = aes_cfb8(),
    [{block, Block} | Config];
group_config(aes_cfb128, Config) ->
    Block = aes_cfb128(),
    [{block, Block} | Config];
group_config(blowfish_cbc, Config) ->
    Block = blowfish_cbc(),
    [{block, Block} | Config];
group_config(blowfish_ecb, Config) ->
    Block = blowfish_ecb(),
    [{block, Block} | Config];
group_config(blowfish_cfb64, Config) ->
    Block = blowfish_cfb64(),
    [{block, Block} | Config];
group_config(blowfish_ofb64, Config) ->
    Block = blowfish_ofb64(),
    [{block, Block} | Config];
group_config(rc4, Config) ->
    Stream = rc4(),
    [{stream, Stream} | Config];
group_config(aes_ctr, Config) ->
    Stream = aes_ctr(),
    [{stream, Stream} | Config];
group_config(aes_gcm, Config) ->
    AEAD = aes_gcm(),
    [{aead, AEAD} | Config];
group_config(chacha20_poly1305, Config) ->
    AEAD = chacha20_poly1305(),
    [{aead, AEAD} | Config];
group_config(aes_cbc, Config) ->
    Block = aes_cbc(),
    [{block, Block} | Config];
group_config(_, Config) ->
    Config.

sign_verify_tests(Type, Msg, Public, Private, PublicS, PrivateS) ->
    sign_verify_tests(Type, [md5, sha, sha224, sha256], Msg, Public, Private) ++
	sign_verify_tests(Type, [sha384, sha512], Msg, PublicS, PrivateS).

sign_verify_tests(Type, Hashs, Msg, Public, Private) ->
    lists:foldl(fun(Hash, Acc) -> 
			case is_supported(Hash) of
			    true ->
				[{Type, Hash,  Public, Private, Msg}|Acc];
			    false ->
			      Acc
			end
		end, [], Hashs).

rfc_1321_msgs() ->
    [<<"">>, 
     <<"a">>,
     <<"abc">>, 
     <<"message digest">>,
     <<"abcdefghijklmnopqrstuvwxyz">>,
     <<"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789">>,
     <<"12345678901234567890123456789012345678901234567890123456789012345678901234567890">>
    ].

rfc_1321_md4_digests() ->
    [hexstr2bin("31d6cfe0d16ae931b73c59d7e0c089c0"),
     hexstr2bin("bde52cb31de33e46245e05fbdbd6fb24"),
     hexstr2bin("a448017aaf21d8525fc10ae87aa6729d"),
     hexstr2bin("d9130a8164549fe818874806e1c7014b"),
     hexstr2bin("d79e1c308aa5bbcdeea8ed63df412da9"),
     hexstr2bin("043f8582f241db351ce627e153e7f0e4"),
     hexstr2bin("e33b4ddc9c38f2199c3e7b164fcc0536")].

rfc_1321_md5_digests() ->
    [hexstr2bin("d41d8cd98f00b204e9800998ecf8427e"),
     hexstr2bin("0cc175b9c0f1b6a831c399e269772661"),
     hexstr2bin("900150983cd24fb0d6963f7d28e17f72"),
     hexstr2bin("f96b697d7cb7938d525a2f31aaf161d0"),
     hexstr2bin("c3fcd3d76192e4007dfb496cca67e13b"),
     hexstr2bin("d174ab98d277d9f5a5611c2c9f419d9f"),
     hexstr2bin("57edf4a22be3c955ac49da2e2107b67a")].

rfc_4634_test1() ->
    <<"abc">>.
rfc_4634_test2_1() ->
    <<"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq">>.
rfc_4634_test2_2a() ->
    <<"abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn">>.
rfc_4634_test2_2b() ->
    <<"hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu">>.
rfc_4634_test2() ->
    A2 =rfc_4634_test2_2a(),
    B2 = rfc_4634_test2_2b(),
    <<A2/binary, B2/binary>>.
 
rfc_4634_sha_digests()->
     [hexstr2bin("A9993E364706816ABA3E25717850C26C9CD0D89D"),
      hexstr2bin("84983E441C3BD26EBAAE4AA1F95129E5E54670F1")].
rfc_4634_sha224_digests() ->
     [hexstr2bin("23097D223405D8228642A477BDA255B32AADBCE4BDA0B3F7E36C9DA7"),
      hexstr2bin("75388B16512776CC5DBA5DA1FD890150B0C6455CB4F58B1952522525")].
rfc_4634_sha256_digests() ->
    [
     hexstr2bin("BA7816BF8F01CFEA4141"
		"40DE5DAE2223B00361A396177A9CB410FF61F20015AD"),
     hexstr2bin("248D6A61D20638B8"
		"E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1")
    ].
rfc_4634_sha384_digests() ->
    [hexstr2bin("CB00753F45A35E8BB5A03D699AC65007272C32AB0EDED1631A8B605A43FF5BED8086072BA1E7CC2358BAECA134C825A7"),
     hexstr2bin("09330C33F71147E83D192FC782CD1B4753111B173B3B05D22FA08086E3B0F712FCC7C71A557E2DB966C3E9FA91746039")
    ].
rfc_4634_sha512_digests() -> 
    [hexstr2bin("DDAF35A193617ABACC417349AE20413112E6FA4E89A97EA2"
		"0A9EEEE64B55D39A2192992A274FC1A836BA3C23A3FEEBBD"
		"454D4423643CE80E2A9AC94FA54CA49F"),
     hexstr2bin("8E959B75DAE313DA8CF4F72814FC143F8F7779C6EB9F7FA17299AEADB6889018501D289E4900F7E4331B99DEC4B5433AC7D329EEB6DD26545E96E55B874BE909")].

long_msg() ->
    fun() -> lists:duplicate(1000000, $a) end.

%% Building huge terms (like long_msg/0) in init_per_group seems to cause
%% test_server crash with 'no_answer_from_tc_supervisor' sometimes on some
%% machines. Therefore lazy evaluation when test case has started.
lazy_eval(F) when is_function(F) -> F();
lazy_eval(Lst)  when is_list(Lst) -> lists:map(fun lazy_eval/1, Lst);
lazy_eval(Tpl) when is_tuple(Tpl) -> list_to_tuple(lists:map(fun lazy_eval/1, tuple_to_list(Tpl)));
lazy_eval(Term) -> Term.

long_sha_digest() ->
    hexstr2bin("34aa973c" "d4c4daa4" "f61eeb2b" "dbad2731" "6534016f").

long_sha256_digest() ->
    hexstr2bin("cdc76e5c" "9914fb92" "81a1c7e2" "84d73e67" "f1809a48" "a497200e" "046d39cc" "c7112cd0").

long_sha384_digest() ->
    hexstr2bin("9d0e1809716474cb" "086e834e310a4a1c" "ed149e9c00f24852" "7972cec5704c2a5b"
	       "07b8b3dc38ecc4eb" "ae97ddd87f3d8985").

long_sha512_digest() ->
    hexstr2bin("e718483d0ce76964" "4e2e42c7bc15b463" "8e1f98b13b204428" "5632a803afa973eb"
	       "de0ff244877ea60a" "4cb0432ce577c31b" "eb009c5c2c49aa2e" "4eadb217ad8cc09b").

ripemd160_msgs() ->
    [<<"">>,
     <<"a">>,
     <<"abc">>,
     <<"message digest">>,
     <<"abcdefghijklmnopqrstuvwxyz">>,
     <<"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq">>,
     <<"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789">>
    ].

ripemd160_digests() ->
    [hexstr2bin("9c1185a5c5e9fc54612808977ee8f548b2258d31"),
     hexstr2bin("0bdc9d2d256b3ee9daae347be6f4dc835a467ffe"),
     hexstr2bin("8eb208f7e05d987a9b044a8e98c6b087f15a0bfc"),
     hexstr2bin("5d0689ef49d2fae572b881b123a85ffa21595f36"),
     hexstr2bin("f71c27109c692c1b56bbdceb5b9d2865b3708dbc"),
     hexstr2bin("12a053384a9c0c88e405a06c27dcf49ada62eb2b"),
     hexstr2bin("b0e20b6e3116640286ed3a87a5713079b21f5189")
    ].

ripemd160_incr_msgs() ->
     [<<"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefg">>,<<"hijklmnopqrstuvwxyz0123456789">>].
ripemd160_incr_digest() ->
    hexstr2bin("b0e20b6e3116640286ed3a87a5713079b21f5189").

rfc_2202_md5_keys() ->
    [binary:copy(<<16#0b>>, 16),
     <<"Jefe">>,
     binary:copy(<<16#aa>>, 16),
     list_to_binary(lists:seq(1, 16#19)),
     binary:copy(<<16#0c>>, 16),
     binary:copy(<<16#aa>>, 80),
     binary:copy(<<16#aa>>, 80)].

rfc_2202_sha_keys() ->
    [binary:copy(<<16#0b>>, 20),
     <<"Jefe">>,
     binary:copy(<<16#aa>>, 20),
     list_to_binary(lists:seq(1, 16#19)),
     binary:copy(<<16#0c>>, 20),
     binary:copy(<<16#aa>>, 80),
     binary:copy(<<16#aa>>, 80)].

rfc_2202_msgs()->
    [<<"Hi There">>,
     <<"what do ya want for nothing?">>,
     binary:copy(<<16#dd>>, 50),
     binary:copy(<<16#cd>>, 50),
     <<"Test With Truncation">>,
     <<"Test Using Larger Than Block-Size Key - Hash Key First">>,
     <<"Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data">>
    ].

hmac_key(md5) ->
    [<<"A fine speach">>, <<"by a fine man!">>];
hmac_key(_) ->
    hexstr2bin("00010203101112132021222330313233"
	       "04050607141516172425262734353637"
	       "08090a0b18191a1b28292a2b38393a3b"
	       "0c0d0e0f1c1d1e1f2c2d2e2f3c3d3e3f").
hmac_inc(_) ->
    [<<"Sampl">>, <<"e #1">>].

%% https://www.cosic.esat.kuleuven.be/nessie/testvectors/
long_hmac_key(Type) when Type == sha384;
			 Type == sha512 ->
    hexstr2bin("00112233445566778899AABBCCDDEEFF"
	       "0123456789ABCDEF0011223344556677"
	       "8899AABBCCDDEEFF0123456789ABCDEF"
	       "00112233445566778899AABBCCDDEEFF");
long_hmac_key(_) ->
    hexstr2bin("0123456789ABCDEF0123456789ABCDEF"
	       "0123456789ABCDEF0123456789ABCDEF"
	       "0123456789ABCDEF0123456789ABCDEF"
	       "0123456789ABCDEF0123456789ABCDEF").
long_hmac(md5) ->
    hexstr2bin("82FDDA30202CB6ACC6F24D4F8A50EB7A");
long_hmac(sha) ->
    hexstr2bin("61D1D0B6459860755FDA892938C23DD401E54A7E");
long_hmac(sha256) ->
    hexstr2bin("50008B8DC7ED3926936347FDC1A01E9D"
	       "5220C6CC4B038B482C0F28A4CD88CA37");
long_hmac(sha384) ->
    hexstr2bin("C1EB08DAFA015833D3FC6B29A387558B"
	       "3F6FA1524AA1A8EB64798D5A76A39D6E"
	       "A1465525342E060EE996277B4FFCDDC9");
long_hmac(sha512) ->
    hexstr2bin("D116BF471AAE1264854F1906025E846A"
	       "61618A965FCA30B695220EA2D6E547E3"
	       "F3B5A4B54E6778928C26D5D3D810498E"
	       "8DF86CB3CC1E9F66A00419B13B6B0C9A").

rfc_2202_hmac_md5() ->
    [
     hexstr2bin("9294727a3638bb1c13f48ef8158bfc9d"),
     hexstr2bin("750c783e6ab0b503eaa86e310a5db738"),
     hexstr2bin("56be34521d144c88dbb8c733f0e8b3f6"),
     hexstr2bin("697eaf0aca3a3aea3a75164746ffaa79"),
     hexstr2bin("56461ef2342edc00f9bab995690efd4c"),
     hexstr2bin("6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd"),
     hexstr2bin("6f630fad67cda0ee1fb1f562db3aa53e")
    ].

rfc_2202_hmac_sha() ->
    [
     hexstr2bin("b617318655057264e28bc0b6fb378c8ef146be00"),
     hexstr2bin("effcdf6ae5eb2fa2d27416d5f184df9c259a7c79"),
     hexstr2bin("125d7342b9ac11cd91a39af48aa17b4f63f175d3"),
     hexstr2bin("4c9007f4026250c6bc8414f9bf50c86c2d7235da"),
     hexstr2bin("4c1a03424b55e07fe7f27be1d58bb9324a9a5a04"),
     hexstr2bin("aa4ae5e15272d00e95705637ce8a3b55ed402112"),
     hexstr2bin("e8e99d0f45237d786d6bbaa7965c7808bbff1a91")
    ].


rfc_4231_keys() ->
    [binary:copy(<<16#0b>>, 20),
     <<"Jefe">>,
     binary:copy(<<16#aa>>, 20),
     list_to_binary(lists:seq(1, 16#19)),
     binary:copy(<<16#0c>>, 20),
     binary:copy(<<16#aa>>, 131),
     binary:copy(<<16#aa>>, 131)
    ].
    
rfc_4231_msgs() ->
    [<<"Hi There">>,
     <<"what do ya want for nothing?">>,
     binary:copy(<<16#dd>>, 50),
     binary:copy(<<16#cd>>, 50),
     <<"Test With Truncation">>,
     <<"Test Using Larger Than Block-Size Key - Hash Key First">>,
     <<"This is a test using a larger than block-size key and a larger t",
       "han block-size data. The key needs to be hashed before being use",
       "d by the HMAC algorithm.">>
    ].
    
rfc4231_hmac_sha224() ->
    [hexstr2bin("896fb1128abbdf196832107cd49df33f"
		       "47b4b1169912ba4f53684b22"),
     hexstr2bin("a30e01098bc6dbbf45690f3a7e9e6d0f"
		       "8bbea2a39e6148008fd05e44"),
     hexstr2bin("7fb3cb3588c6c1f6ffa9694d7d6ad264"
		       "9365b0c1f65d69d1ec8333ea"),
     hexstr2bin("6c11506874013cac6a2abc1bb382627c"
		       "ec6a90d86efc012de7afec5a"),
     hexstr2bin("0e2aea68a90c8d37c988bcdb9fca6fa8"),
     hexstr2bin("95e9a0db962095adaebe9b2d6f0dbce2"
		       "d499f112f2d2b7273fa6870e"),
     hexstr2bin("3a854166ac5d9f023f54d517d0b39dbd"
		"946770db9c2b95c9f6f565d1")].
rfc4231_hmac_sha256() ->
    [hexstr2bin("b0344c61d8db38535ca8afceaf0bf12b"
		"881dc200c9833da726e9376c2e32cff7"),
     hexstr2bin("5bdcc146bf60754e6a042426089575c7"
		"5a003f089d2739839dec58b964ec3843"),
     hexstr2bin("773ea91e36800e46854db8ebd09181a7"
		"2959098b3ef8c122d9635514ced565fe"),
    hexstr2bin("82558a389a443c0ea4cc819899f2083a"
	       "85f0faa3e578f8077a2e3ff46729665b"),
     hexstr2bin("a3b6167473100ee06e0c796c2955552b"),
     hexstr2bin("60e431591ee0b67f0d8a26aacbf5b77f"
		"8e0bc6213728c5140546040f0ee37f54"),
     hexstr2bin("9b09ffa71b942fcb27635fbcd5b0e944"
		"bfdc63644f0713938a7f51535c3a35e2")].

rfc4231_hmac_sha384() ->
    [hexstr2bin("afd03944d84895626b0825f4ab46907f"
		"15f9dadbe4101ec682aa034c7cebc59c"
		"faea9ea9076ede7f4af152e8b2fa9cb6"),
     hexstr2bin("af45d2e376484031617f78d2b58a6b1b"
		"9c7ef464f5a01b47e42ec3736322445e"
	       "8e2240ca5e69e2c78b3239ecfab21649"),
     hexstr2bin("88062608d3e6ad8a0aa2ace014c8a86f"
	       "0aa635d947ac9febe83ef4e55966144b"
		"2a5ab39dc13814b94e3ab6e101a34f27"),
     hexstr2bin("3e8a69b7783c25851933ab6290af6ca7"
		"7a9981480850009cc5577c6e1f573b4e"
		"6801dd23c4a7d679ccf8a386c674cffb"),
     hexstr2bin("3abf34c3503b2a23a46efc619baef897"),
     hexstr2bin("4ece084485813e9088d2c63a041bc5b4"
		"4f9ef1012a2b588f3cd11f05033ac4c6"
		"0c2ef6ab4030fe8296248df163f44952"),
     hexstr2bin("6617178e941f020d351e2f254e8fd32c"
		"602420feb0b8fb9adccebb82461e99c5"
		"a678cc31e799176d3860e6110c46523e")].
rfc4231_hmac_sha512() ->
    [hexstr2bin("87aa7cdea5ef619d4ff0b4241a1d6cb0"
		"2379f4e2ce4ec2787ad0b30545e17cde"
		"daa833b7d6b8a702038b274eaea3f4e4"
		"be9d914eeb61f1702e696c203a126854"),
     hexstr2bin("164b7a7bfcf819e2e395fbe73b56e0a3"
		"87bd64222e831fd610270cd7ea250554"
		"9758bf75c05a994a6d034f65f8f0e6fd"
		"caeab1a34d4a6b4b636e070a38bce737"),
     hexstr2bin("fa73b0089d56a284efb0f0756c890be9"
		"b1b5dbdd8ee81a3655f83e33b2279d39"
		"bf3e848279a722c806b485a47e67c807"
		"b946a337bee8942674278859e13292fb"),
     hexstr2bin("b0ba465637458c6990e5a8c5f61d4af7"
		"e576d97ff94b872de76f8050361ee3db"
		"a91ca5c11aa25eb4d679275cc5788063"
		"a5f19741120c4f2de2adebeb10a298dd"),
     hexstr2bin("415fad6271580a531d4179bc891d87a6"),
     hexstr2bin("80b24263c7c1a3ebb71493c1dd7be8b4"
		"9b46d1f41b4aeec1121b013783f8f352"
		"6b56d037e05f2598bd0fd2215d6a1e52"
		"95e64f73f63f0aec8b915a985d786598"),
     hexstr2bin("e37b6a775dc87dbaa4dfa9f96e5e3ffd"
		"debd71f8867289865df5a32d20cdc944"
		"b6022cac3c4982b10d5eeb55c3e4de15"
		"134676fb6de0446065c97440fa8c6a58")].
des_cbc() ->
    [{des_cbc, 
     hexstr2bin("0123456789abcdef"), 
     hexstr2bin("1234567890abcdef"),
     <<"Now is the time for all ">> }].
      
des_cfb() ->
    [{des_cfb, 
     hexstr2bin("0123456789abcdef"),
     hexstr2bin("1234567890abcdef"),
     <<"Now is the">>}].

des3_cbc() ->
    [{des3_cbc,
     [hexstr2bin("0123456789abcdef"), 
      hexstr2bin("fedcba9876543210"),
      hexstr2bin("0f2d4b6987a5c3e1")],
     hexstr2bin("1234567890abcdef"),
     <<"Now is the time for all ">>
     }].

des_ede3() ->
    [{des_ede3,
     [hexstr2bin("8000000000000000"),
      hexstr2bin("4000000000000000"),
      hexstr2bin("2000000000000000")],
      hexstr2bin("7AD16FFB79C45926"),
      hexstr2bin("0000000000000000")
     }].

des3_cbf() ->
    [{des3_cbf,
     [hexstr2bin("0123456789abcdef"),
      hexstr2bin("fedcba9876543210"),
      hexstr2bin("0f2d4b6987a5c3e1")],
     hexstr2bin("1234567890abcdef"),
     <<"Now is the time for all ">>
     }].

des3_cfb() ->
    [{des3_cfb,
     [hexstr2bin("0123456789abcdef"),
      hexstr2bin("fedcba9876543210"),
      hexstr2bin("0f2d4b6987a5c3e1")],
     hexstr2bin("1234567890abcdef"),
     <<"Now is the time for all ">>
     }].

rc2_cbc() ->
    [{rc2_cbc,
     <<146,210,160,124,215,227,153,239,227,17,222,140,3,93,27,191>>,
      <<72,91,135,182,25,42,35,210>>,
     <<36,245,206,158,168,230,58,69,148,137,32,192,250,41,237,181,181,251, 192,2,175,135,177,171,57,30,111,117,159,149,15,28,88,158,28,81,28,115, 85,219,241,82,117,222,91,85,73,117,164,25,182,52,191,64,123,57,26,19, 211,27,253,31,194,219,231,104,247,240,172,130,119,21,225,154,101,247, 32,216,42,216,133,169,78,22,97,27,227,26,196,224,172,168,17,9,148,55, 203,91,252,40,61,226,236,221,215,160,78,63,13,181,68,57,196,241,185, 207, 116,129,152,237,60,139,247,153,27,146,161,246,222,98,185,222,152, 187,135, 236,86,34,7,110,91,230,173,34,160,242,202,222,121,127,181,140, 101,203,195, 190,88,250,86,147,127,87,72,126,171,16,71,47,110,248,88, 14,29,143,161,152, 129,236,148,22,152,186,208,119,70,8,174,193,203,100, 193,203,200,117,102,242, 134,142,96,125,135,200,217,190,76,117,50,70, 209,186,101,241,200,91,40,193,54, 90,195,38,47,59,197,38,234,86,223,16, 51,253,204,129,20,171,66,21,241,26,135,216, 196,114,110,91,15,53,40, 164,201,136,113,95,247,51,181,208,241,68,168,98,151,36, 155,72,24,57, 42,191,14,125,204,10,167,214,233,138,115,125,234,121,134,227,26,247, 77,200,117,110,117,111,168,156,206,67,159,149,189,173,150,193,91,199, 216,153,22, 189,137,185,89,160,13,131,132,58,109,28,110,246,252,251,14, 232,91,38,52,29,101,188,69,123,50,0,130,178,93,73,239,118,7,77,35,59, 253,10,159,45,86,142,37,78,232,48>>
     }].

%% AES CBC test vectors from http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf
aes_cbc() ->
    [
     %% F.2.1 CBC-AES128.Encrypt, F.2.2 CBC-AES128.Decrypt
     {aes_cbc,
      hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"),                    %% Key
      hexstr2bin("000102030405060708090a0b0c0d0e0f"),                    %% IV
      hexstr2bin("6bc1bee22e409f96e93d7e117393172a"                      %% PlainText
		 "ae2d8a571e03ac9c9eb76fac45af8e51"
		 "30c81c46a35ce411e5fbc1191a0a52ef"
		 "f69f2445df4f9b17ad2b417be66c3710"),
      hexstr2bin("7649abac8119b246cee98e9b12e9197d"                      %% CipherText
		 "5086cb9b507219ee95db113a917678b2"
		 "73bed6b8e3c1743b7116e69e22229516"
		 "3ff1caa1681fac09120eca307586e1a7")},
     %% F.2.3 CBC-AES192.Encrypt, F.2.4 CBC-AES192.Decrypt
     {aes_cbc,
      hexstr2bin("8e73b0f7da0e6452c810f32b809079e5"                      %% Key
		 "62f8ead2522c6b7b"),
      hexstr2bin("000102030405060708090a0b0c0d0e0f"),                    %% IV
      hexstr2bin("6bc1bee22e409f96e93d7e117393172a"                      %% PlainText
		 "ae2d8a571e03ac9c9eb76fac45af8e51"
		 "30c81c46a35ce411e5fbc1191a0a52ef"
		 "f69f2445df4f9b17ad2b417be66c3710"),
      hexstr2bin("4f021db243bc633d7178183a9fa071e8"                      %% CipherText
		 "b4d9ada9ad7dedf4e5e738763f69145a"
		 "571b242012fb7ae07fa9baac3df102e0"
		 "08b0e27988598881d920a9e64f5615cd")},
     %% F.2.5 CBC-AES256.Encrypt, F.2.6 CBC-AES256.Decrypt
     {aes_cbc,
      hexstr2bin("603deb1015ca71be2b73aef0857d7781"                      %% Key
		 "1f352c073b6108d72d9810a30914dff4"),
      hexstr2bin("000102030405060708090a0b0c0d0e0f"),                    %% IV
      hexstr2bin("6bc1bee22e409f96e93d7e117393172a"                      %% PlainText
		 "ae2d8a571e03ac9c9eb76fac45af8e51"
		 "30c81c46a35ce411e5fbc1191a0a52ef"
		 "f69f2445df4f9b17ad2b417be66c3710"),
      hexstr2bin("f58c4c04d6e5f1ba779eabfb5f7bfbd6"                      %% CipherText
		 "9cfc4e967edb808d679f777bc6702c7d"
		 "39f23369a9d9bacfa530e26304231461"
		 "b2eb05e2c39be9fcda6c19078c6a9d1b")}
    ].

aes_cbc128() ->
    [{aes_cbc128,
      hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
      hexstr2bin("000102030405060708090a0b0c0d0e0f"),
      hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
     {aes_cbc128,
      hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
      hexstr2bin("7649ABAC8119B246CEE98E9B12E9197D"),
      hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
     {aes_cbc128,
      hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
      hexstr2bin("5086CB9B507219EE95DB113A917678B2"),
      hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef")},
     {aes_cbc128,
      hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
      hexstr2bin("73BED6B8E3C1743B7116E69E22229516"),
      hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")}
    ].

aes_cbc256() -> 
    [{aes_cbc256,
      hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"), 
      hexstr2bin("000102030405060708090A0B0C0D0E0F"),
      hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
      {aes_cbc256,
       hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"), 
       hexstr2bin("F58C4C04D6E5F1BA779EABFB5F7BFBD6"),
       hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
      {aes_cbc256,
       hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"), 
       hexstr2bin("9CFC4E967EDB808D679F777BC6702C7D"),
       hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef")},
      {aes_cbc256,
       hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"), 
       hexstr2bin("39F23369A9D9BACFA530E26304231461"),
       hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")}
     ].

aes_ecb() -> 
    [
     {aes_ecb,
      <<"YELLOW SUBMARINE">>, 
      <<"YELLOW SUBMARINE">>},
     {aes_ecb,
      <<"0000000000000000">>, 
      <<"0000000000000000">>},
     {aes_ecb,
      <<"FFFFFFFFFFFFFFFF">>, 
      <<"FFFFFFFFFFFFFFFF">>},
     {aes_ecb,
      <<"3000000000000000">>, 
      <<"1000000000000001">>},
     {aes_ecb,
      <<"1111111111111111">>, 
      <<"1111111111111111">>},
     {aes_ecb,
      <<"0123456789ABCDEF">>, 
      <<"1111111111111111">>},
     {aes_ecb,
      <<"0000000000000000">>, 
      <<"0000000000000000">>},
     {aes_ecb,
      <<"FEDCBA9876543210">>, 
      <<"0123456789ABCDEF">>},
     {aes_ecb,
      <<"7CA110454A1A6E57">>, 
      <<"01A1D6D039776742">>},
     {aes_ecb,
      <<"0131D9619DC1376E">>, 
      <<"5CD54CA83DEF57DA">>},
     {aes_ecb,
      <<"07A1133E4A0B2686">>, 
      <<"0248D43806F67172">>},
     {aes_ecb,
      <<"3849674C2602319E">>, 
      <<"51454B582DDF440A">>},
     {aes_ecb,
      <<"04B915BA43FEB5B6">>, 
      <<"42FD443059577FA2">>},
     {aes_ecb,
      <<"0113B970FD34F2CE">>, 
      <<"059B5E0851CF143A">>},
     {aes_ecb,
      <<"0170F175468FB5E6">>, 
      <<"0756D8E0774761D2">>},
     {aes_ecb,
      <<"43297FAD38E373FE">>, 
      <<"762514B829BF486A">>},
     {aes_ecb,
      <<"07A7137045DA2A16">>, 
      <<"3BDD119049372802">>},
     {aes_ecb,
      <<"04689104C2FD3B2F">>, 
      <<"26955F6835AF609A">>},
     {aes_ecb,
      <<"37D06BB516CB7546">>, 
      <<"164D5E404F275232">>},
     {aes_ecb,
      <<"1F08260D1AC2465E">>, 
      <<"6B056E18759F5CCA">>},
     {aes_ecb,
      <<"584023641ABA6176">>, 
      <<"004BD6EF09176062">>},
     {aes_ecb,
      <<"025816164629B007">>, 
      <<"480D39006EE762F2">>},
     {aes_ecb,
      <<"49793EBC79B3258F">>, 
      <<"437540C8698F3CFA">>},
     {aes_ecb,
      <<"018310DC409B26D6">>, 
      <<"1D9D5C5018F728C2">>},
     {aes_ecb,
      <<"1C587F1C13924FEF">>, 
      <<"305532286D6F295A">>},
     {aes_ecb,
      <<"0101010101010101">>, 
      <<"0123456789ABCDEF">>},
     {aes_ecb,
      <<"1F1F1F1F0E0E0E0E">>, 
      <<"0123456789ABCDEF">>},
     {aes_ecb,
      <<"E0FEE0FEF1FEF1FE">>, 
      <<"0123456789ABCDEF">>},
     {aes_ecb,
      <<"0000000000000000">>, 
      <<"FFFFFFFFFFFFFFFF">>},
     {aes_ecb,
      <<"FFFFFFFFFFFFFFFF">>, 
      <<"0000000000000000">>},
     {aes_ecb,
      <<"0123456789ABCDEF">>, 
      <<"0000000000000000">>},
     {aes_ecb,
      <<"FEDCBA9876543210">>, 
      <<"FFFFFFFFFFFFFFFF">>},
     %% AES ECB test vectors from http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf
     %% F.1.1 ECB-AES128.Encrypt, F.1.2 ECB-AES128.Decrypt
     {aes_ecb,
      hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"),
      hexstr2bin("6bc1bee22e409f96e93d7e117393172a"
		 "ae2d8a571e03ac9c9eb76fac45af8e51"
		 "30c81c46a35ce411e5fbc1191a0a52ef"
		 "f69f2445df4f9b17ad2b417be66c3710")},
     %% F.1.3 ECB-AES192.Encrypt, F.1.4 ECB-AES192.Decrypt
     {aes_ecb,
      hexstr2bin("8e73b0f7da0e6452c810f32b809079e5"
		 "62f8ead2522c6b7b"),
      hexstr2bin("6bc1bee22e409f96e93d7e117393172a"
		 "ae2d8a571e03ac9c9eb76fac45af8e51"
		 "30c81c46a35ce411e5fbc1191a0a52ef"
		 "f69f2445df4f9b17ad2b417be66c3710")},
     %% F.1.5 ECB-AES256.Encrypt, F.1.6 ECB-AES256.Decrypt
     {aes_ecb,
      hexstr2bin("603deb1015ca71be2b73aef0857d7781"
		 "1f352c073b6108d72d9810a30914dff4"),
      hexstr2bin("6bc1bee22e409f96e93d7e117393172a"
		 "ae2d8a571e03ac9c9eb76fac45af8e51"
		 "30c81c46a35ce411e5fbc1191a0a52ef"
		 "f69f2445df4f9b17ad2b417be66c3710")}
    ].

aes_ige256() ->
    [{aes_ige256,
      hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
      hexstr2bin("000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F"),
      hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
      {aes_ige256,
       hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
       hexstr2bin("4D0F9E735749215C05CB20DA00F7814B77D33F8A668BEBBAC1739AB20302D4FE"),
       hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
      {aes_ige256,
       hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
       hexstr2bin("2A5569424DAE1ACEABDEEA108DB4606AE21A9227CAB5F55BF52535CFA2B34717"),
       hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef")},
      {aes_ige256,
       hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
       hexstr2bin("15D5A583D2D668E518E683D9BDF1B6D0E0C3B1E5D5C1D51E964822E1ADE88DFA"),
       hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")}
     ].

aes_cfb8() -> 
    [{aes_cfb8,
      hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
      hexstr2bin("000102030405060708090a0b0c0d0e0f"),
      hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
      {aes_cfb8,
       hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
       hexstr2bin("3B3FD92EB72DAD20333449F8E83CFB4A"),
       hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
      {aes_cfb8,
       hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
       hexstr2bin("C8A64537A0B3A93FCDE3CDAD9F1CE58B"),
       hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef")},
      {aes_cfb8,
       hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
       hexstr2bin("26751F67A3CBB140B1808CF187A4F4DF"),
       hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")},
      {aes_cfb8,
       hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"),
       hexstr2bin("000102030405060708090a0b0c0d0e0f"),
       hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
      {aes_cfb8,
       hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"),
       hexstr2bin("cdc80d6fddf18cab34c25909c99a4174"),
       hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
      {aes_cfb8,
       hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"),
       hexstr2bin("67ce7f7f81173621961a2b70171d3d7a"),
       hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef")},
      {aes_cfb8,
       hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"),
       hexstr2bin("2e1e8a1dd59b88b1c8e60fed1efac4c9"),
       hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")},
      {aes_cfb8,
       hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
       hexstr2bin("000102030405060708090a0b0c0d0e0f"),
       hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
      {aes_cfb8,
       hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
       hexstr2bin("dc7e84bfda79164b7ecd8486985d3860"),
       hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
      {aes_cfb8,
       hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
       hexstr2bin("39ffed143b28b1c832113c6331e5407b"),
       hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef")},
      {aes_cfb8,
       hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
       hexstr2bin("df10132415e54b92a13ed0a8267ae2f9"),
       hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")}
     ].

aes_cfb128() -> 
    [{aes_cfb128,
      hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
      hexstr2bin("000102030405060708090a0b0c0d0e0f"),
      hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
      {aes_cfb128,
       hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
       hexstr2bin("3B3FD92EB72DAD20333449F8E83CFB4A"),
       hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
      {aes_cfb128,
       hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
       hexstr2bin("C8A64537A0B3A93FCDE3CDAD9F1CE58B"),
       hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef")},
      {aes_cfb128,
       hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
       hexstr2bin("26751F67A3CBB140B1808CF187A4F4DF"),
       hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")},
      {aes_cfb128,
       hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"),
       hexstr2bin("000102030405060708090a0b0c0d0e0f"),
       hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
      {aes_cfb128,
       hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"),
       hexstr2bin("cdc80d6fddf18cab34c25909c99a4174"),
       hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
      {aes_cfb128,
       hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"),
       hexstr2bin("67ce7f7f81173621961a2b70171d3d7a"),
       hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef")},
      {aes_cfb128,
       hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"),
       hexstr2bin("2e1e8a1dd59b88b1c8e60fed1efac4c9"),
       hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")},
      {aes_cfb128,
       hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
       hexstr2bin("000102030405060708090a0b0c0d0e0f"),
       hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
      {aes_cfb128,
       hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
       hexstr2bin("dc7e84bfda79164b7ecd8486985d3860"),
       hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
      {aes_cfb128,
       hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
       hexstr2bin("39ffed143b28b1c832113c6331e5407b"),
       hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef")},
      {aes_cfb128,
       hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
       hexstr2bin("df10132415e54b92a13ed0a8267ae2f9"),
       hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")}
     ].

blowfish_cbc() ->
    [{blowfish_cbc,
      hexstr2bin("0123456789ABCDEFF0E1D2C3B4A59687"), 
      hexstr2bin("FEDCBA9876543210"),
      hexstr2bin("37363534333231204E6F77206973207468652074696D6520666F722000000000")
     }].

blowfish_ecb() ->
    [
     {blowfish_ecb,
      hexstr2bin("0000000000000000"), 
      hexstr2bin("0000000000000000")},
     {blowfish_ecb,
      hexstr2bin("FFFFFFFFFFFFFFFF"), 
      hexstr2bin("FFFFFFFFFFFFFFFF")},
     {blowfish_ecb,
      hexstr2bin("3000000000000000"), 
      hexstr2bin("1000000000000001")},
     {blowfish_ecb,
      hexstr2bin("1111111111111111"), 
      hexstr2bin("1111111111111111")},
     {blowfish_ecb,
      hexstr2bin("0123456789ABCDEF"), 
      hexstr2bin("1111111111111111")},
     {blowfish_ecb,
      hexstr2bin("0000000000000000"), 
      hexstr2bin("0000000000000000")},
     {blowfish_ecb,
      hexstr2bin("FEDCBA9876543210"), 
      hexstr2bin("0123456789ABCDEF")},
     {blowfish_ecb,
      hexstr2bin("7CA110454A1A6E57"), 
      hexstr2bin("01A1D6D039776742")},
     {blowfish_ecb,
      hexstr2bin("0131D9619DC1376E"), 
      hexstr2bin("5CD54CA83DEF57DA")},
     {blowfish_ecb,
      hexstr2bin("07A1133E4A0B2686"), 
      hexstr2bin("0248D43806F67172")},
     {blowfish_ecb,
      hexstr2bin("3849674C2602319E"), 
      hexstr2bin("51454B582DDF440A")},
     {blowfish_ecb,
      hexstr2bin("04B915BA43FEB5B6"), 
      hexstr2bin("42FD443059577FA2")},
     {blowfish_ecb,
      hexstr2bin("0113B970FD34F2CE"), 
      hexstr2bin("059B5E0851CF143A")},
     {blowfish_ecb,
      hexstr2bin("0170F175468FB5E6"), 
      hexstr2bin("0756D8E0774761D2")},
     {blowfish_ecb,
      hexstr2bin("43297FAD38E373FE"), 
      hexstr2bin("762514B829BF486A")},
     {blowfish_ecb,
      hexstr2bin("07A7137045DA2A16"), 
      hexstr2bin("3BDD119049372802")},
     {blowfish_ecb,
      hexstr2bin("04689104C2FD3B2F"), 
      hexstr2bin("26955F6835AF609A")},
     {blowfish_ecb,
      hexstr2bin("37D06BB516CB7546"), 
      hexstr2bin("164D5E404F275232")},
     {blowfish_ecb,
      hexstr2bin("1F08260D1AC2465E"), 
      hexstr2bin("6B056E18759F5CCA")},
     {blowfish_ecb,
      hexstr2bin("584023641ABA6176"), 
      hexstr2bin("004BD6EF09176062")},
     {blowfish_ecb,
      hexstr2bin("025816164629B007"), 
      hexstr2bin("480D39006EE762F2")},
     {blowfish_ecb,
      hexstr2bin("49793EBC79B3258F"), 
      hexstr2bin("437540C8698F3CFA")},
     {blowfish_ecb,
      hexstr2bin("018310DC409B26D6"), 
      hexstr2bin("1D9D5C5018F728C2")},
     {blowfish_ecb,
      hexstr2bin("1C587F1C13924FEF"), 
      hexstr2bin("305532286D6F295A")},
     {blowfish_ecb,
      hexstr2bin("0101010101010101"), 
      hexstr2bin("0123456789ABCDEF")},
     {blowfish_ecb,
      hexstr2bin("1F1F1F1F0E0E0E0E"), 
      hexstr2bin("0123456789ABCDEF")},
     {blowfish_ecb,
      hexstr2bin("E0FEE0FEF1FEF1FE"), 
      hexstr2bin("0123456789ABCDEF")},
     {blowfish_ecb,
      hexstr2bin("0000000000000000"), 
      hexstr2bin("FFFFFFFFFFFFFFFF")},
     {blowfish_ecb,
      hexstr2bin("FFFFFFFFFFFFFFFF"), 
      hexstr2bin("0000000000000000")},
     {blowfish_ecb,
      hexstr2bin("0123456789ABCDEF"), 
      hexstr2bin("0000000000000000")},
     {blowfish_ecb,
      hexstr2bin("FEDCBA9876543210"), 
      hexstr2bin("FFFFFFFFFFFFFFFF")}
    ].

blowfish_cfb64() ->
    [{blowfish_cfb64,
      hexstr2bin("0123456789ABCDEFF0E1D2C3B4A59687"), 
      hexstr2bin("FEDCBA9876543210"),
      hexstr2bin("37363534333231204E6F77206973207468652074696D6520666F722000")
     }].
blowfish_ofb64() ->
    [{blowfish_ofb64,
      hexstr2bin("0123456789ABCDEFF0E1D2C3B4A59687"), 
      hexstr2bin("FEDCBA9876543210"),
      hexstr2bin("37363534333231204E6F77206973207468652074696D6520666F722000")
     }].

rc4() ->
    [{rc4, <<"apaapa">>, <<"Yo baby yo">>},
     {rc4, <<"apaapa">>, list_to_binary(lists:seq(0, 255))},
     {rc4, <<"apaapa">>, long_msg()}
    ].

aes_ctr() ->
    [  %% F.5.3  CTR-AES192.Encrypt
       {aes_ctr, hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"), 
	hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
       {aes_ctr, hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff00"), 
	hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
       {aes_ctr, hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff01"), 
	hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef") },
       {aes_ctr, hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff02"), 
	hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")},
       
       %% F.5.3  CTR-AES192.Encrypt
       {aes_ctr, hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"), 
	hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
       {aes_ctr, hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff00"), 
	hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
       {aes_ctr, hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff01"), 
	hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef")},
       {aes_ctr, hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff02"), 
	hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")},
       
       %% F.5.5  CTR-AES256.Encrypt
       {aes_ctr, hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"), 
	hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
       {aes_ctr, hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff00"), 
	hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
       {aes_ctr, hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff01"), 
	hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef")},
       {aes_ctr, hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff02"), 
	hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")},

       {aes_ctr,  hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"),
	long_msg()}
    ].


%% AES GCM test vectors from http://csrc.nist.gov/groups/ST/toolkit/BCM/documents/proposedmodes/gcm/gcm-spec.pdf
aes_gcm() ->
    [
     %% Test Case 1
     {aes_gcm, hexstr2bin("00000000000000000000000000000000"),           %% Key
      hexstr2bin(""),                                                    %% PlainText
      hexstr2bin("000000000000000000000000"),                            %% IV
      hexstr2bin(""),                                                    %% AAD
      hexstr2bin(""),                                                    %% CipherText
      hexstr2bin("58e2fccefa7e3061367f1d57a4e7455a")},                   %% CipherTag

     %% Test Case 2
     {aes_gcm, hexstr2bin("00000000000000000000000000000000"),           %% Key
      hexstr2bin("00000000000000000000000000000000"),                    %% PlainText
      hexstr2bin("000000000000000000000000"),                            %% IV
      hexstr2bin(""),                                                    %% AAD
      hexstr2bin("0388dace60b6a392f328c2b971b2fe78"),                    %% CipherText
      hexstr2bin("ab6e47d42cec13bdf53a67b21257bddf")},                   %% CipherTag

     %% Test Case 3
     {aes_gcm, hexstr2bin("feffe9928665731c6d6a8f9467308308"),           %% Key
      hexstr2bin("d9313225f88406e5a55909c5aff5269a"                      %% PlainText
		 "86a7a9531534f7da2e4c303d8a318a72"
		 "1c3c0c95956809532fcf0e2449a6b525"
		 "b16aedf5aa0de657ba637b391aafd255"),
      hexstr2bin("cafebabefacedbaddecaf888"),                            %% IV
      hexstr2bin(""),                                                    %% AAD
      hexstr2bin("42831ec2217774244b7221b784d0d49c"                      %% CipherText
		 "e3aa212f2c02a4e035c17e2329aca12e"
		 "21d514b25466931c7d8f6a5aac84aa05"
		 "1ba30b396a0aac973d58e091473f5985"),
      hexstr2bin("4d5c2af327cd64a62cf35abd2ba6fab4")},                   %% CipherTag

     %% Test Case 4
     {aes_gcm, hexstr2bin("feffe9928665731c6d6a8f9467308308"),           %% Key
      hexstr2bin("d9313225f88406e5a55909c5aff5269a"                      %% PlainText
		 "86a7a9531534f7da2e4c303d8a318a72"
		 "1c3c0c95956809532fcf0e2449a6b525"
		 "b16aedf5aa0de657ba637b39"),
      hexstr2bin("cafebabefacedbaddecaf888"),                            %% IV
      hexstr2bin("feedfacedeadbeeffeedfacedeadbeef"                      %% AAD
		 "abaddad2"),
      hexstr2bin("42831ec2217774244b7221b784d0d49c"                      %% CipherText
		 "e3aa212f2c02a4e035c17e2329aca12e"
		 "21d514b25466931c7d8f6a5aac84aa05"
		 "1ba30b396a0aac973d58e091"),
      hexstr2bin("5bc94fbc3221a5db94fae95ae7121a47")},                   %% CipherTag

     %% Test Case 5
     {aes_gcm, hexstr2bin("feffe9928665731c6d6a8f9467308308"),           %% Key
      hexstr2bin("d9313225f88406e5a55909c5aff5269a"                      %% PlainText
		 "86a7a9531534f7da2e4c303d8a318a72"
		 "1c3c0c95956809532fcf0e2449a6b525"
		 "b16aedf5aa0de657ba637b39"),
      hexstr2bin("cafebabefacedbad"),                                    %% IV
      hexstr2bin("feedfacedeadbeeffeedfacedeadbeef"                      %% AAD
		 "abaddad2"),
      hexstr2bin("61353b4c2806934a777ff51fa22a4755"                      %% CipherText
		 "699b2a714fcdc6f83766e5f97b6c7423"
		 "73806900e49f24b22b097544d4896b42"
		 "4989b5e1ebac0f07c23f4598"),
      hexstr2bin("3612d2e79e3b0785561be14aaca2fccb")},                   %% CipherTag

     %% Test Case 6"
     {aes_gcm, hexstr2bin("feffe9928665731c6d6a8f9467308308"),           %% Key
      hexstr2bin("d9313225f88406e5a55909c5aff5269a"                      %% PlainText
		 "86a7a9531534f7da2e4c303d8a318a72"
		 "1c3c0c95956809532fcf0e2449a6b525"
		 "b16aedf5aa0de657ba637b39"),
      hexstr2bin("9313225df88406e555909c5aff5269aa"                      %% IV
		 "6a7a9538534f7da1e4c303d2a318a728"
		 "c3c0c95156809539fcf0e2429a6b5254"
		 "16aedbf5a0de6a57a637b39b"),
      hexstr2bin("feedfacedeadbeeffeedfacedeadbeef"                      %% AAD
		 "abaddad2"),
      hexstr2bin("8ce24998625615b603a033aca13fb894"                      %% CipherText
		 "be9112a5c3a211a8ba262a3cca7e2ca7"
		 "01e4a9a4fba43c90ccdcb281d48c7c6f"
		 "d62875d2aca417034c34aee5"),
      hexstr2bin("619cc5aefffe0bfa462af43c1699d050")},                   %% CipherTag

     %% Test Case 7
     {aes_gcm, hexstr2bin("00000000000000000000000000000000"             %% Key
			  "0000000000000000"),
      hexstr2bin(""),                                                    %% PlainText
      hexstr2bin("000000000000000000000000"),                            %% IV
      hexstr2bin(""),                                                    %% AAD
      hexstr2bin(""),                                                    %% CipherText
      hexstr2bin("cd33b28ac773f74ba00ed1f312572435")},                   %% CipherTag

     %% Test Case 8
     {aes_gcm, hexstr2bin("00000000000000000000000000000000"             %% Key
			  "0000000000000000"),
      hexstr2bin("00000000000000000000000000000000"),                    %% PlainText
      hexstr2bin("000000000000000000000000"),                            %% IV
      hexstr2bin(""),                                                    %% AAD
      hexstr2bin("98e7247c07f0fe411c267e4384b0f600"),                    %% CipherText
      hexstr2bin("2ff58d80033927ab8ef4d4587514f0fb")},                   %% CipherTag

     %% Test Case 9
     {aes_gcm, hexstr2bin("feffe9928665731c6d6a8f9467308308"             %% Key
			  "feffe9928665731c"),
      hexstr2bin("d9313225f88406e5a55909c5aff5269a"                      %% PlainText
		 "86a7a9531534f7da2e4c303d8a318a72"
		 "1c3c0c95956809532fcf0e2449a6b525"
		 "b16aedf5aa0de657ba637b391aafd255"),
      hexstr2bin("cafebabefacedbaddecaf888"),                            %% IV
      hexstr2bin(""),                                                    %% ADD
      hexstr2bin("3980ca0b3c00e841eb06fac4872a2757"                      %% CipherText
		 "859e1ceaa6efd984628593b40ca1e19c"
		 "7d773d00c144c525ac619d18c84a3f47"
		 "18e2448b2fe324d9ccda2710acade256"),
      hexstr2bin("9924a7c8587336bfb118024db8674a14")},                   %% CipherTag

     %% Test Case 10
     {aes_gcm, hexstr2bin("feffe9928665731c6d6a8f9467308308"             %% Key
			  "feffe9928665731c"),
      hexstr2bin("d9313225f88406e5a55909c5aff5269a"                      %% PlainText
		 "86a7a9531534f7da2e4c303d8a318a72"
		 "1c3c0c95956809532fcf0e2449a6b525"
		 "b16aedf5aa0de657ba637b39"),
      hexstr2bin("cafebabefacedbaddecaf888"),                            %% IV
      hexstr2bin("feedfacedeadbeeffeedfacedeadbeef"                      %% AAD
		 "abaddad2"),
      hexstr2bin("3980ca0b3c00e841eb06fac4872a2757"                      %% CipherText
		 "859e1ceaa6efd984628593b40ca1e19c"
		 "7d773d00c144c525ac619d18c84a3f47"
		 "18e2448b2fe324d9ccda2710"),
      hexstr2bin("2519498e80f1478f37ba55bd6d27618c")},                   %% CipherTag

     %% Test Case 11
     {aes_gcm, hexstr2bin("feffe9928665731c6d6a8f9467308308"             %% Key
			  "feffe9928665731c"),
      hexstr2bin("d9313225f88406e5a55909c5aff5269a"                      %% PlainText
		 "86a7a9531534f7da2e4c303d8a318a72"
		 "1c3c0c95956809532fcf0e2449a6b525"
		 "b16aedf5aa0de657ba637b39"),
      hexstr2bin("cafebabefacedbad"),                                    %% IV
      hexstr2bin("feedfacedeadbeeffeedfacedeadbeef"                      %% AAD
		 "abaddad2"),
      hexstr2bin("0f10f599ae14a154ed24b36e25324db8"                      %% CipherText
		 "c566632ef2bbb34f8347280fc4507057"
		 "fddc29df9a471f75c66541d4d4dad1c9"
		 "e93a19a58e8b473fa0f062f7"),
      hexstr2bin("65dcc57fcf623a24094fcca40d3533f8")},                   %% CipherTag

     %% Test Case 12
     {aes_gcm, hexstr2bin("feffe9928665731c6d6a8f9467308308"             %% Key
		 "feffe9928665731c"),
      hexstr2bin("d9313225f88406e5a55909c5aff5269a"                      %% PlainText
		 "86a7a9531534f7da2e4c303d8a318a72"
		 "1c3c0c95956809532fcf0e2449a6b525"
		 "b16aedf5aa0de657ba637b39"),
      hexstr2bin("9313225df88406e555909c5aff5269aa"                      %% IV
		 "6a7a9538534f7da1e4c303d2a318a728"
		 "c3c0c95156809539fcf0e2429a6b5254"
		 "16aedbf5a0de6a57a637b39b"),
      hexstr2bin("feedfacedeadbeeffeedfacedeadbeef"                      %% AAD
		 "abaddad2"),
      hexstr2bin("d27e88681ce3243c4830165a8fdcf9ff"                      %% CipherText
		 "1de9a1d8e6b447ef6ef7b79828666e45"
		 "81e79012af34ddd9e2f037589b292db3"
		 "e67c036745fa22e7e9b7373b"),
      hexstr2bin("dcf566ff291c25bbb8568fc3d376a6d9")},                   %% CipherTag

     %% Test Case 13
     {aes_gcm, hexstr2bin("00000000000000000000000000000000"             %% Key
			  "00000000000000000000000000000000"),
      hexstr2bin(""),                                                    %% PlainText
      hexstr2bin("000000000000000000000000"),                            %% IV
      hexstr2bin(""),                                                    %% AAD
      hexstr2bin(""),                                                    %% CipherText
      hexstr2bin("530f8afbc74536b9a963b4f1c4cb738b")},                   %% CipherTag

     %% Test Case 14
     {aes_gcm, hexstr2bin("00000000000000000000000000000000"             %% Key
			  "00000000000000000000000000000000"),
      hexstr2bin("00000000000000000000000000000000"),                    %% PlainText
      hexstr2bin("000000000000000000000000"),                            %% IV
      hexstr2bin(""),                                                    %% AAD
      hexstr2bin("cea7403d4d606b6e074ec5d3baf39d18"),                    %% CipherText
      hexstr2bin("d0d1c8a799996bf0265b98b5d48ab919")},                   %% CipherTag

     %% Test Case 15
     {aes_gcm, hexstr2bin("feffe9928665731c6d6a8f9467308308"             %% Key
			  "feffe9928665731c6d6a8f9467308308"),
      hexstr2bin("d9313225f88406e5a55909c5aff5269a"                      %% PlainText
		 "86a7a9531534f7da2e4c303d8a318a72"
		 "1c3c0c95956809532fcf0e2449a6b525"
		 "b16aedf5aa0de657ba637b391aafd255"),
      hexstr2bin("cafebabefacedbaddecaf888"),                            %% IV
      hexstr2bin(""),                                                    %% AAD
      hexstr2bin("522dc1f099567d07f47f37a32a84427d"                      %% CipherText
		 "643a8cdcbfe5c0c97598a2bd2555d1aa"
		 "8cb08e48590dbb3da7b08b1056828838"
		 "c5f61e6393ba7a0abcc9f662898015ad"),
      hexstr2bin("b094dac5d93471bdec1a502270e3cc6c")},                   %% CipherTag

     %% Test Case 16
     {aes_gcm, hexstr2bin("feffe9928665731c6d6a8f9467308308"             %% Key
			  "feffe9928665731c6d6a8f9467308308"),
      hexstr2bin("d9313225f88406e5a55909c5aff5269a"                      %% PlainText
		 "86a7a9531534f7da2e4c303d8a318a72"
		 "1c3c0c95956809532fcf0e2449a6b525"
		 "b16aedf5aa0de657ba637b39"),
      hexstr2bin("cafebabefacedbaddecaf888"),                            %% IV
      hexstr2bin("feedfacedeadbeeffeedfacedeadbeef"                      %% AAD
		 "abaddad2"),
      hexstr2bin("522dc1f099567d07f47f37a32a84427d"                      %% CipherText
		 "643a8cdcbfe5c0c97598a2bd2555d1aa"
		 "8cb08e48590dbb3da7b08b1056828838"
		 "c5f61e6393ba7a0abcc9f662"),
      hexstr2bin("76fc6ece0f4e1768cddf8853bb2d551b")},                   %% CipherTag

     %% Test Case 17
     {aes_gcm, hexstr2bin("feffe9928665731c6d6a8f9467308308"             %% Key
			  "feffe9928665731c6d6a8f9467308308"),
      hexstr2bin("d9313225f88406e5a55909c5aff5269a"                      %% PlainText
		 "86a7a9531534f7da2e4c303d8a318a72"
		 "1c3c0c95956809532fcf0e2449a6b525"
		 "b16aedf5aa0de657ba637b39"),
      hexstr2bin("cafebabefacedbad"),                                    %% IV
      hexstr2bin("feedfacedeadbeeffeedfacedeadbeef"                      %% AAD
		 "abaddad2"),
      hexstr2bin("c3762df1ca787d32ae47c13bf19844cb"                      %% CipherText
		 "af1ae14d0b976afac52ff7d79bba9de0"
		 "feb582d33934a4f0954cc2363bc73f78"
		 "62ac430e64abe499f47c9b1f"),
      hexstr2bin("3a337dbf46a792c45e454913fe2ea8f2")},                   %% CipherTag

     %% Test Case 18
     {aes_gcm, hexstr2bin("feffe9928665731c6d6a8f9467308308"             %% Key
			  "feffe9928665731c6d6a8f9467308308"),
      hexstr2bin("d9313225f88406e5a55909c5aff5269a"                      %% PlainText
		 "86a7a9531534f7da2e4c303d8a318a72"
		 "1c3c0c95956809532fcf0e2449a6b525"
		 "b16aedf5aa0de657ba637b39"),
      hexstr2bin("9313225df88406e555909c5aff5269aa"                      %% IV
		 "6a7a9538534f7da1e4c303d2a318a728"
		 "c3c0c95156809539fcf0e2429a6b5254"
		 "16aedbf5a0de6a57a637b39b"),
      hexstr2bin("feedfacedeadbeeffeedfacedeadbeef"                      %% AAD
		 "abaddad2"),
      hexstr2bin("5a8def2f0c9e53f1f75d7853659e2a20"                      %% CipherText
		 "eeb2b22aafde6419a058ab4f6f746bf4"
		 "0fc0c3b780f244452da3ebf1c5d82cde"
		 "a2418997200ef82e44ae7e3f"),
      hexstr2bin("a44a8266ee1c8eb0c8b5d4cf5ae9f19a")},                   %% CipherTag

     %% Test Case 0 for TagLength = 1
     {aes_gcm, hexstr2bin("00000000000000000000000000000000"),           %% Key
      hexstr2bin(""),                                                    %% PlainText
      hexstr2bin("000000000000000000000000"),                            %% IV
      hexstr2bin(""),                                                    %% AAD
      hexstr2bin(""),                                                    %% CipherText
      hexstr2bin("58"),                                                  %% CipherTag
      1},                                                                %% TagLength

     %% Test Case 18 for TagLength = 1
     {aes_gcm, hexstr2bin("feffe9928665731c6d6a8f9467308308"             %% Key
			  "feffe9928665731c6d6a8f9467308308"),
      hexstr2bin("d9313225f88406e5a55909c5aff5269a"                      %% PlainText
		 "86a7a9531534f7da2e4c303d8a318a72"
		 "1c3c0c95956809532fcf0e2449a6b525"
		 "b16aedf5aa0de657ba637b39"),
      hexstr2bin("9313225df88406e555909c5aff5269aa"                      %% IV
		 "6a7a9538534f7da1e4c303d2a318a728"
		 "c3c0c95156809539fcf0e2429a6b5254"
		 "16aedbf5a0de6a57a637b39b"),
      hexstr2bin("feedfacedeadbeeffeedfacedeadbeef"                      %% AAD
		 "abaddad2"),
      hexstr2bin("5a8def2f0c9e53f1f75d7853659e2a20"                      %% CipherText
		 "eeb2b22aafde6419a058ab4f6f746bf4"
		 "0fc0c3b780f244452da3ebf1c5d82cde"
		 "a2418997200ef82e44ae7e3f"),
      hexstr2bin("a4"),                                                  %% CipherTag
      1}                                                                 %% TagLength
    ].

%% https://tools.ietf.org/html/rfc7539#appendix-A.5
chacha20_poly1305() ->
    [
     {chacha20_poly1305,
      hexstr2bin("1c9240a5eb55d38af333888604f6b5f0"                      %% Key
			"473917c1402b80099dca5cbc207075c0"),
      hexstr2bin("496e7465726e65742d44726166747320"                      %% PlainText
      "61726520647261667420646f63756d65"
      "6e74732076616c696420666f72206120"
      "6d6178696d756d206f6620736978206d"
      "6f6e74687320616e64206d6179206265"
      "20757064617465642c207265706c6163"
      "65642c206f72206f62736f6c65746564"
      "206279206f7468657220646f63756d65"
      "6e747320617420616e792074696d652e"
      "20497420697320696e617070726f7072"
      "6961746520746f2075736520496e7465"
      "726e65742d4472616674732061732072"
      "65666572656e6365206d617465726961"
      "6c206f7220746f206369746520746865"
      "6d206f74686572207468616e20617320"
      "2fe2809c776f726b20696e2070726f67"
      "726573732e2fe2809d"),
      hexstr2bin("000000000102030405060708"),                            %% Nonce
      hexstr2bin("f33388860000000000004e91"),                            %% AAD
      hexstr2bin("64a0861575861af460f062c79be643bd"                      %% CipherText
      "5e805cfd345cf389f108670ac76c8cb2"
      "4c6cfc18755d43eea09ee94e382d26b0"
      "bdb7b73c321b0100d4f03b7f355894cf"
      "332f830e710b97ce98c8a84abd0b9481"
      "14ad176e008d33bd60f982b1ff37c855"
      "9797a06ef4f0ef61c186324e2b350638"
      "3606907b6a7c02b0f9f6157b53c867e4"
      "b9166c767b804d46a59b5216cde7a4e9"
      "9040c5a40433225ee282a1b0a06c523e"
      "af4534d7f83fa1155b0047718cbc546a"
      "0d072b04b3564eea1b422273f548271a"
      "0bb2316053fa76991955ebd63159434e"
      "cebb4e466dae5a1073a6727627097a10"
      "49e617d91d361094fa68f0ff77987130"
      "305beaba2eda04df997b714d6c6f2c29"
      "a6ad5cb4022b02709b"),
      hexstr2bin("eead9d67890cbb22392336fea1851f38")}                    %% CipherTag
    ].

rsa_plain() ->
    <<"7896345786348756234 Hejsan Svejsan, erlang crypto debugger"
      "09812312908312378623487263487623412039812 huagasd">>.
rsa_public() ->
    [65537, 7919488123861148172698919999061127847747888703039837999377650217570191053151807772962118671509138346758471459464133273114654252861270845708312601272799123].
rsa_private() ->
    rsa_public() ++ [7531712708607620783801185371644749935066152052780368689827275932079815492940396744378735701395659435842364793962992309884847527234216715366607660219930945].

rsa_public_stronger() ->
    [65537, 24629450921918866883077380602720734920775458960049554761386137065662137652635369332143446151320538248280934442179850504891395344346514465469955766163141133564033962851182759993807898821114734943339732032639891483186089941567854227407119560631150779000222837755424893038740314247760600374970909894211201220612920040986106639419467243909950276018045907029941478599124238353052062083560294570722081552510960894164859765695309596889747541376908786225647625736062865138957717982693312699025417086612046330464651009693307624955796202070510577399561730651967517158452930742355327167632521808183383868100102455048819375344881].

rsa_private_stronger() ->
    rsa_public_stronger() ++ [13565232776562604620467234237694854016819673873109064019820773052201665024482754648718278717031083946624786145611240731564761987114634269887293030432042088547345315212418830656522115993209293567218379960177754901461542373481136856927955012596579314262051109321754382091434920473734937991286600905464814063189230779981494358415076362038786197620360127262110530926733754185204773610295221669711309000953136320804528874719105049753061737780710448207922456570922652651354760939379096788728229638142403068102990416717272880560951246813789730402978652924934794503277969128609831043469924881848849409122972426787999886557185].

dss_plain() ->
    rsa_plain().
dss_public() ->
    25854665488880835237281628794585130313500176551981812527054397586638455298000483144002221850980183404910190346416063318160497344811383498859129095184158800144312512447497510551471331451396405348497845813002058423110442376886564659959543650802132345311573634832461635601376738282831340827591903548964194832978.
dss_private() ->
    441502407453038284293378221372000880210588566361.  
dss_params() ->
    [109799869232806890760655301608454668257695818999841877165019612946154359052535682480084145133201304812979481136659521529774182959764860329095546511521488413513097576425638476458000255392402120367876345280670101492199681798674053929238558140260669578407351853803102625390950534052428162468100618240968893110797,
     1349199015905534965792122312016505075413456283393,
     18320614775012672475365915366944922415598782131828709277168615511695849821411624805195787607930033958243224786899641459701930253094446221381818858674389863050420226114787005820357372837321561754462061849169568607689530279303056075793886577588606958623645901271866346406773590024901668622321064384483571751669].

ec_key_named() ->
    Curve = hd(crypto:ec_curves()),
    {D2_pub, D2_priv} = crypto:generate_key(ecdh, Curve),
    {[D2_priv, Curve], [D2_pub, Curve]}.

ec_msg() ->
    <<99,234,6,64,190,237,201,99,80,248,58,40,70,45,149,218,5,246,242,63>>.

srp3() ->
    Username = <<"alice">>,
    Password = <<"password123">>,
    Salt = hexstr2bin("2857827A19266A1F2BC6"),
    Prime = hexstr2bin("EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C"
		       "9C256576D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE4"
		       "8E495C1D6089DAD15DC7D7B46154D6B6CE8EF4AD69B15D4982559B29"
		       "7BCF1885C529F566660E57EC68EDBC3C05726CC02FD4CBF4976EAA9A"
		       "FD5138FE8376435B9FC61D2FC0EB06E3"),
    Generator = <<2>>,
    Version = '3',
    Scrambler = hexstr2bin("02E2476A"),

    %% X = hexstr2bin("96E54AB0CD4C5123EDCFA4A1502918AAD3C9E2A8"),
    Verifier = hexstr2bin("96EB5F13621D911AA1CA405DE9C64217D4108EEEECAFFE500034FE0E"
			  "C031E42C8714667C161BCE0E7996F7DDE1B63824C130D2D7286C08C0"
			  "49758420735961347112AE102A3F23B3F687F8FEE0DF2BFAF933C608"
			  "D6FE5B5EEE3116FE54016E065BF8E8C9FDBBC08719231AC215149140"
			  "519E8FDD9AA4F410C28A58AF42974D2D"),
    ClientPrivate = hexstr2bin("6411DE75538BED8170677D577D0608F39112BC95B503C447EB6AC945"
			  "49C75C7B"),
    ServerPrivate = hexstr2bin("85E44A6F694DBE676145DB245A045CD37C99F05C562C7840A31F270D"
			  "9AADCF8B"),
    ClientPublic = hexstr2bin("B22B1FFA2244B8CB94F3A9080F419CAEAB0DBA93EA1965B5E84587EE"
			 "55C79E7A118865DC59B9D0353362C2A8261E7C1B0D221A0E233C2AD1"
			 "640DACBB8664CBC9733EAC392DA7800142860380C3FC573C3C064329"
			 "CF54063FD114C7210E9CB3A611EA8002B1844B698F930D95D143899B"
			 "948A090E0C25938E5F84067D1883DC63"),
    ServerPublic = hexstr2bin("93A8C4D8B7F7395ADCFD4ABA37B015124513D3F37B3E85EB23064BE5"
			 "F53C0AE32FFB9D8C0AA0DCFFA74D632DD67DEBB5C35AAE9812286CC8"
			 "C43CC176ECBC6D3F447594D9554E995B2509127BF88FADDDA4982D03"
			 "8EC3001320712D3B1269308CE70F319B2295FA57674F03A2D993CFB1"
			 "F84C35B7D0C012FA73CD4C8F7D5A71C7"),

    SessionKey = hexstr2bin("C29A986C4D521BBC66428ED11D994CD7431574A6184B83CDCC345092"
			    "791E75748A1D38CAC4BD14760F0D2694B711236419240FF2F172454C"
			    "46ABF4FF39498DAFDD2C82924F7D7BD76CDFCE688C77D93F18A65409"
			    "9176A9192615DC0277AE7C12F1F6A7F6563FCA11675D809AF578BDE5"
			    "2B51E05D440B63099A017A0B45044801"),
    UserPassHash = crypto:hash(sha, [Salt, crypto:hash(sha, [Username, <<$:>>, Password])]),
    Verifier = crypto:mod_pow(Generator, UserPassHash, Prime), 
    ClientPublic = crypto:mod_pow(Generator, ClientPrivate, Prime), 
    srp(ClientPrivate, Generator, Prime, Version, Verifier, ServerPublic, ServerPrivate, UserPassHash, Scrambler, SessionKey).

srp6() ->
    Username = <<"alice">>,
    Password = <<"password123">>,
    Salt = hexstr2bin("2857827A19266A1F2BC6"),
    Prime = hexstr2bin("EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C"
		       "9C256576D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE4"
		       "8E495C1D6089DAD15DC7D7B46154D6B6CE8EF4AD69B15D4982559B29"
		       "7BCF1885C529F566660E57EC68EDBC3C05726CC02FD4CBF4976EAA9A"
		       "FD5138FE8376435B9FC61D2FC0EB06E3"),
    Generator = <<2>>,
    Version = '6',
    Scrambler = hexstr2bin("0A2534C0BF52A0DA9001EEC62CF2A546AB0908A7"),
    Verifier = hexstr2bin("96EB5F13621D911AA1CA405DE9C64217D4108EEEECAFFE500034FE0E"
			  "C031E42C8714667C161BCE0E7996F7DDE1B63824C130D2D7286C08C0"
			  "49758420735961347112AE102A3F23B3F687F8FEE0DF2BFAF933C608"
			  "D6FE5B5EEE3116FE54016E065BF8E8C9FDBBC08719231AC215149140"
			  "519E8FDD9AA4F410C28A58AF42974D2D"),
    ClientPrivate = hexstr2bin("6411DE75538BED8170677D577D0608F39112BC95B503C447EB6AC945"
			  "49C75C7B"),
    ServerPrivate = hexstr2bin("85E44A6F694DBE676145DB245A045CD37C99F05C562C7840A31F270D"
			  "9AADCF8B"),
    ClientPublic = hexstr2bin("B22B1FFA2244B8CB94F3A9080F419CAEAB0DBA93EA1965B5E84587EE"
			 "55C79E7A118865DC59B9D0353362C2A8261E7C1B0D221A0E233C2AD1"
			 "640DACBB8664CBC9733EAC392DA7800142860380C3FC573C3C064329"
			 "CF54063FD114C7210E9CB3A611EA8002B1844B698F930D95D143899B"
			 "948A090E0C25938E5F84067D1883DC63"),
    ServerPublic = hexstr2bin("D2D07845CE7ECDB9845DD36B10ACD3598CC29049DE9F467F84CE16B6"
			 "D97A6DC567AF8B0F9FEDF74962400AD5C357951E64E67B641246F264"
			 "C8DE6D9A72E554D6C8D3194548780A0C438A0FCC509CA88A14AA1DEB"
			 "C0F09E4B37A965D1545DB4AD361346F3189B0EA569C06D326C4E4797"
			 "9E381C748293B7C0591BE0BE419E053E"),

    SessionKey = hexstr2bin("19D22C19612874EBF1F2581F8EFCFDC44C6FDA3B87B0A73823D7E962"
				 "554295D4E48D3A336523ADBDDD0EC8FB0F02687109E97E01C17C93CC"
				 "7216F9CD8A4AC39F0429857D8D1023066614BDFCBCB89F59A0FEB81C"
				 "72E992AAD89095A84B6A5FADA152369AB1E350A03693BEF044DF3EDF"
				 "0C34741F4696C30E9F675D09F58ACBEB"),
    UserPassHash = crypto:hash(sha, [Salt, crypto:hash(sha, [Username, <<$:>>, Password])]),
    Verifier = crypto:mod_pow(Generator, UserPassHash, Prime), 
    ClientPublic = crypto:mod_pow(Generator, ClientPrivate, Prime),
    srp(ClientPrivate, Generator, Prime, Version, Verifier, ServerPublic, ServerPrivate, UserPassHash, Scrambler, SessionKey).


srp6a_smaller_prime() ->
    Username = <<"alice">>,
    Password = <<"password123">>,
    Salt = <<"mystrongsalt">>,
    Prime = hexstr2bin("894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7"),
    Generator = <<7>>,
    Version = '6a',
    Scrambler = hexstr2bin("18DE4A002AD05EF464B19AE2B6929F9B1319C7AA"),
    Verifier = hexstr2bin("867401D5DE10964768184EAF246B322760C847604075FA66A4423907"
			  "8428BCA5"),
    ClientPrivate = hexstr2bin("C49F832EE8D67ECF9E7F2785EB0622D8B3FE2344C00F96E1AEF4103C"
			  "A44D51F9"),
    ServerPrivate = hexstr2bin("6C78CCEAAEC15E69068A87795B2A20ED7B45CFC5A254EBE2F17F144A"
			  "4D99DB18"),
    ClientPublic = hexstr2bin("2452A57166BBBF690DB77539BAF9C57CD1ED99D5AA15ED925AD9B5C3"
			  "64BBEDFF"),
    ServerPublic = hexstr2bin("2C0464DE84B91E4963A3546CAC0EFE55F31F49208C3F0AD7EE55F444"
			  "8F38BA7F"),

    SessionKey = hexstr2bin("65581B2302580BD26F522A5A421CF969B9CCBCE4051196B034A2A9D22065D848"),
    UserPassHash = crypto:hash(sha, [Salt, crypto:hash(sha, [Username, <<$:>>, Password])]),
    Verifier = crypto:mod_pow(Generator, UserPassHash, Prime), 
    ClientPublic = crypto:mod_pow(Generator, ClientPrivate, Prime),
    srp(ClientPrivate, Generator, Prime, Version, Verifier, ServerPublic, ServerPrivate, UserPassHash, Scrambler, SessionKey).

srp6a() ->
    Username = <<"alice">>,
    Password = <<"password123">>,
    Salt = hexstr2bin("BEB25379D1A8581EB5A727673A2441EE"),
    Prime = hexstr2bin("EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C"
		       "9C256576D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE4"
		       "8E495C1D6089DAD15DC7D7B46154D6B6CE8EF4AD69B15D4982559B29"
		       "7BCF1885C529F566660E57EC68EDBC3C05726CC02FD4CBF4976EAA9A"
		       "FD5138FE8376435B9FC61D2FC0EB06E3"),
    Generator = <<2>>,
    Version = '6a',
    Scrambler = hexstr2bin("CE38B9593487DA98554ED47D70A7AE5F462EF019"),
    Verifier = hexstr2bin("7E273DE8696FFC4F4E337D05B4B375BEB0DDE1569E8FA00A9886D812"
			  "9BADA1F1822223CA1A605B530E379BA4729FDC59F105B4787E5186F5"
			  "C671085A1447B52A48CF1970B4FB6F8400BBF4CEBFBB168152E08AB5"
			  "EA53D15C1AFF87B2B9DA6E04E058AD51CC72BFC9033B564E26480D78"
			  "E955A5E29E7AB245DB2BE315E2099AFB"),
    ClientPrivate = hexstr2bin("60975527035CF2AD1989806F0407210BC81EDC04E2762A56AFD529DD"
			  "DA2D4393"),
    ServerPrivate = hexstr2bin("E487CB59D31AC550471E81F00F6928E01DDA08E974A004F49E61F5D1"
			  "05284D20"),
    ClientPublic = hexstr2bin("61D5E490F6F1B79547B0704C436F523DD0E560F0C64115BB72557EC4"
			      "4352E8903211C04692272D8B2D1A5358A2CF1B6E0BFCF99F921530EC"
			      "8E39356179EAE45E42BA92AEACED825171E1E8B9AF6D9C03E1327F44"
			      "BE087EF06530E69F66615261EEF54073CA11CF5858F0EDFDFE15EFEA"
			      "B349EF5D76988A3672FAC47B0769447B"),
    ServerPublic = hexstr2bin("BD0C61512C692C0CB6D041FA01BB152D4916A1E77AF46AE105393011"
			      "BAF38964DC46A0670DD125B95A981652236F99D9B681CBF87837EC99"
			      "6C6DA04453728610D0C6DDB58B318885D7D82C7F8DEB75CE7BD4FBAA"
			      "37089E6F9C6059F388838E7A00030B331EB76840910440B1B27AAEAE"
			      "EB4012B7D7665238A8E3FB004B117B58"),
    
    SessionKey = hexstr2bin("B0DC82BABCF30674AE450C0287745E7990A3381F63B387AAF271A10D"
			    "233861E359B48220F7C4693C9AE12B0A6F67809F0876E2D013800D6C"
			    "41BB59B6D5979B5C00A172B4A2A5903A0BDCAF8A709585EB2AFAFA8F"
			    "3499B200210DCC1F10EB33943CD67FC88A2F39A4BE5BEC4EC0A3212D"
			    "C346D7E474B29EDE8A469FFECA686E5A"),
    UserPassHash = crypto:hash(sha, [Salt, crypto:hash(sha, [Username, <<$:>>, Password])]),
    Verifier = crypto:mod_pow(Generator, UserPassHash, Prime), 
    ClientPublic = crypto:mod_pow(Generator, ClientPrivate, Prime), 
    srp(ClientPrivate, Generator, Prime, Version, Verifier, ServerPublic, ServerPrivate, UserPassHash, Scrambler, SessionKey).

srp(ClientPrivate, Generator, Prime, Version, Verifier, ServerPublic, ServerPrivate, UserPassHash, Scrambler, SessionKey)->
    {srp, ClientPrivate, 
     {user, [Generator, Prime, Version]}, {user, [UserPassHash, Prime, Generator, Version, Scrambler]}, 
     ServerPublic, ServerPrivate, {host, [Verifier, Generator, Prime, Version]},
     {host, [Verifier, Prime, Version, Scrambler]},
     SessionKey}.
ecdh() ->
    %% http://csrc.nist.gov/groups/STM/cavp/
    Curves = crypto:ec_curves(),
    TestCases =
        [{ecdh, hexstr2point("42ea6dd9969dd2a61fea1aac7f8e98edcc896c6e55857cc0", "dfbe5d7c61fac88b11811bde328e8a0d12bf01a9d204b523"),
          hexstr2bin("f17d3fea367b74d340851ca4270dcb24c271f445bed9d527"),
          secp192r1,
          hexstr2bin("803d8ab2e5b6e6fca715737c3a82f7ce3c783124f6d51cd0")},
         {ecdh, hexstr2point("deb5712fa027ac8d2f22c455ccb73a91e17b6512b5e030e7", "7e2690a02cc9b28708431a29fb54b87b1f0c14e011ac2125"),
          hexstr2bin("56e853349d96fe4c442448dacb7cf92bb7a95dcf574a9bd5"),
          secp192r1,
          hexstr2bin("c208847568b98835d7312cef1f97f7aa298283152313c29d")},
         {ecdh, hexstr2point("af33cd0629bc7e996320a3f40368f74de8704fa37b8fab69abaae280", "882092ccbba7930f419a8a4f9bb16978bbc3838729992559a6f2e2d7"),
          hexstr2bin("8346a60fc6f293ca5a0d2af68ba71d1dd389e5e40837942df3e43cbd"),
          secp224r1,
          hexstr2bin("7d96f9a3bd3c05cf5cc37feb8b9d5209d5c2597464dec3e9983743e8")},
         {ecdh, hexstr2point("13bfcd4f8e9442393cab8fb46b9f0566c226b22b37076976f0617a46", "eeb2427529b288c63c2f8963c1e473df2fca6caa90d52e2f8db56dd4"),
          hexstr2bin("043cb216f4b72cdf7629d63720a54aee0c99eb32d74477dac0c2f73d"),
          secp224r1,
          hexstr2bin("ee93ce06b89ff72009e858c68eb708e7bc79ee0300f73bed69bbca09")},
         {ecdh, hexstr2point("700c48f77f56584c5cc632ca65640db91b6bacce3a4df6b42ce7cc838833d287", "db71e509e3fd9b060ddb20ba5c51dcc5948d46fbf640dfe0441782cab85fa4ac"),
          hexstr2bin("7d7dc5f71eb29ddaf80d6214632eeae03d9058af1fb6d22ed80badb62bc1a534"),
          secp256r1,
          hexstr2bin("46fc62106420ff012e54a434fbdd2d25ccc5852060561e68040dd7778997bd7b")},
         {ecdh, hexstr2point("809f04289c64348c01515eb03d5ce7ac1a8cb9498f5caa50197e58d43a86a7ae", "b29d84e811197f25eba8f5194092cb6ff440e26d4421011372461f579271cda3"),
          hexstr2bin("38f65d6dce47676044d58ce5139582d568f64bb16098d179dbab07741dd5caf5"),
          secp256r1,
          hexstr2bin("057d636096cb80b67a8c038c890e887d1adfa4195e9b3ce241c8a778c59cda67")},
         {ecdh, hexstr2point("a7c76b970c3b5fe8b05d2838ae04ab47697b9eaf52e764592efda27fe7513272734466b400091adbf2d68c58e0c50066", "ac68f19f2e1cb879aed43a9969b91a0839c4c38a49749b661efedf243451915ed0905a32b060992b468c64766fc8437a"),
          hexstr2bin("3cc3122a68f0d95027ad38c067916ba0eb8c38894d22e1b15618b6818a661774ad463b205da88cf699ab4d43c9cf98a1"),
          secp384r1,
          hexstr2bin("5f9d29dc5e31a163060356213669c8ce132e22f57c9a04f40ba7fcead493b457e5621e766c40a2e3d4d6a04b25e533f1")},
         {ecdh, hexstr2point("30f43fcf2b6b00de53f624f1543090681839717d53c7c955d1d69efaf0349b7363acb447240101cbb3af6641ce4b88e0", "25e46c0c54f0162a77efcc27b6ea792002ae2ba82714299c860857a68153ab62e525ec0530d81b5aa15897981e858757"),
          hexstr2bin("92860c21bde06165f8e900c687f8ef0a05d14f290b3f07d8b3a8cc6404366e5d5119cd6d03fb12dc58e89f13df9cd783"),
          secp384r1,
          hexstr2bin("a23742a2c267d7425fda94b93f93bbcc24791ac51cd8fd501a238d40812f4cbfc59aac9520d758cf789c76300c69d2ff")},
         {ecdh, hexstr2point("00685a48e86c79f0f0875f7bc18d25eb5fc8c0b07e5da4f4370f3a9490340854334b1e1b87fa395464c60626124a4e70d0f785601d37c09870ebf176666877a2046d", "01ba52c56fc8776d9e8f5db4f0cc27636d0b741bbe05400697942e80b739884a83bde99e0f6716939e632bc8986fa18dccd443a348b6c3e522497955a4f3c302f676"),
          hexstr2bin("017eecc07ab4b329068fba65e56a1f8890aa935e57134ae0ffcce802735151f4eac6564f6ee9974c5e6887a1fefee5743ae2241bfeb95d5ce31ddcb6f9edb4d6fc47"),
          secp521r1,
          hexstr2bin("005fc70477c3e63bc3954bd0df3ea0d1f41ee21746ed95fc5e1fdf90930d5e136672d72cc770742d1711c3c3a4c334a0ad9759436a4d3c5bf6e74b9578fac148c831")},
         {ecdh, hexstr2point("01df277c152108349bc34d539ee0cf06b24f5d3500677b4445453ccc21409453aafb8a72a0be9ebe54d12270aa51b3ab7f316aa5e74a951c5e53f74cd95fc29aee7a", "013d52f33a9f3c14384d1587fa8abe7aed74bc33749ad9c570b471776422c7d4505d9b0a96b3bfac041e4c6a6990ae7f700e5b4a6640229112deafa0cd8bb0d089b0"),
          hexstr2bin("00816f19c1fb10ef94d4a1d81c156ec3d1de08b66761f03f06ee4bb9dcebbbfe1eaa1ed49a6a990838d8ed318c14d74cc872f95d05d07ad50f621ceb620cd905cfb8"),
          secp521r1,
          hexstr2bin("000b3920ac830ade812c8f96805da2236e002acbbf13596a9ab254d44d0e91b6255ebf1229f366fb5a05c5884ef46032c26d42189273ca4efa4c3db6bd12a6853759")},

         %% RFC-6954, Appendix A
         {ecdh, hexstr2point("A9C21A569759DA95E0387041184261440327AFE33141CA04B82DC92E",
                             "98A0F75FBBF61D8E58AE5511B2BCDBE8E549B31E37069A2825F590C1"),
          hexstr2bin("6060552303899E2140715816C45B57D9B42204FB6A5BF5BEAC10DB00"),
          brainpoolP224r1,
          hexstr2bin("1A4BFE705445120C8E3E026699054104510D119757B74D5FE2462C66")},
         {ecdh, hexstr2point("034A56C550FF88056144E6DD56070F54B0135976B5BF77827313F36B",
                             "75165AD99347DC86CAAB1CBB579E198EAF88DC35F927B358AA683681"),
          hexstr2bin("39F155483CEE191FBECFE9C81D8AB1A03CDA6790E7184ACE44BCA161"),
          brainpoolP224r1,
          hexstr2bin("1A4BFE705445120C8E3E026699054104510D119757B74D5FE2462C66")},
         {ecdh, hexstr2point("44106E913F92BC02A1705D9953A8414DB95E1AAA49E81D9E85F929A8E3100BE5",
                             "8AB4846F11CACCB73CE49CBDD120F5A900A69FD32C272223F789EF10EB089BDC"),
          hexstr2bin("55E40BC41E37E3E2AD25C3C6654511FFA8474A91A0032087593852D3E7D76BD3"),
          brainpoolP256r1,
          hexstr2bin("89AFC39D41D3B327814B80940B042590F96556EC91E6AE7939BCE31F3A18BF2B")},
         {ecdh, hexstr2point("8D2D688C6CF93E1160AD04CC4429117DC2C41825E1E9FCA0ADDD34E6F1B39F7B",
                             "990C57520812BE512641E47034832106BC7D3E8DD0E4C7F1136D7006547CEC6A"),
          hexstr2bin("81DB1EE100150FF2EA338D708271BE38300CB54241D79950F77B063039804F1D"),
          brainpoolP256r1,
          hexstr2bin("89AFC39D41D3B327814B80940B042590F96556EC91E6AE7939BCE31F3A18BF2B")},
         {ecdh, hexstr2point("68B665DD91C195800650CDD363C625F4E742E8134667B767B1B476793588F885AB698C852D4A6E77A252D6380FCAF068",
                             "55BC91A39C9EC01DEE36017B7D673A931236D2F1F5C83942D049E3FA20607493E0D038FF2FD30C2AB67D15C85F7FAA59"),
          hexstr2bin("032640BC6003C59260F7250C3DB58CE647F98E1260ACCE4ACDA3DD869F74E01F8BA5E0324309DB6A9831497ABAC96670"),
          brainpoolP384r1,
          hexstr2bin("0BD9D3A7EA0B3D519D09D8E48D0785FB744A6B355E6304BC51C229FBBCE239BBADF6403715C35D4FB2A5444F575D4F42")},
         {ecdh, hexstr2point("4D44326F269A597A5B58BBA565DA5556ED7FD9A8A9EB76C25F46DB69D19DC8CE6AD18E404B15738B2086DF37E71D1EB4",
                             "62D692136DE56CBE93BF5FA3188EF58BC8A3A0EC6C1E151A21038A42E9185329B5B275903D192F8D4E1F32FE9CC78C48"),
          hexstr2bin("1E20F5E048A5886F1F157C74E91BDE2B98C8B52D58E5003D57053FC4B0BD65D6F15EB5D1EE1610DF870795143627D042"),
          brainpoolP384r1,
          hexstr2bin("0BD9D3A7EA0B3D519D09D8E48D0785FB744A6B355E6304BC51C229FBBCE239BBADF6403715C35D4FB2A5444F575D4F42")},
         {ecdh, hexstr2point("0A420517E406AAC0ACDCE90FCD71487718D3B953EFD7FBEC5F7F27E28C6149999397E91E029E06457DB2D3E640668B392C2A7E737A7F0BF04436D11640FD09FD",
                             "72E6882E8DB28AAD36237CD25D580DB23783961C8DC52DFA2EC138AD472A0FCEF3887CF62B623B2A87DE5C588301EA3E5FC269B373B60724F5E82A6AD147FDE7"),
          hexstr2bin("230E18E1BCC88A362FA54E4EA3902009292F7F8033624FD471B5D8ACE49D12CFABBC19963DAB8E2F1EBA00BFFB29E4D72D13F2224562F405CB80503666B25429"),
          brainpoolP512r1,
          hexstr2bin("A7927098655F1F9976FA50A9D566865DC530331846381C87256BAF3226244B76D36403C024D7BBF0AA0803EAFF405D3D24F11A9B5C0BEF679FE1454B21C4CD1F")},
         {ecdh, hexstr2point("9D45F66DE5D67E2E6DB6E93A59CE0BB48106097FF78A081DE781CDB31FCE8CCBAAEA8DD4320C4119F1E9CD437A2EAB3731FA9668AB268D871DEDA55A5473199F",
                             "2FDC313095BCDD5FB3A91636F07A959C8E86B5636A1E930E8396049CB481961D365CC11453A06C719835475B12CB52FC3C383BCE35E27EF194512B71876285FA"),
          hexstr2bin("16302FF0DBBB5A8D733DAB7141C1B45ACBC8715939677F6A56850A38BD87BD59B09E80279609FF333EB9D4C061231FB26F92EEB04982A5F1D1764CAD57665422"),
          brainpoolP512r1,
          hexstr2bin("A7927098655F1F9976FA50A9D566865DC530331846381C87256BAF3226244B76D36403C024D7BBF0AA0803EAFF405D3D24F11A9B5C0BEF679FE1454B21C4CD1F")}],
    lists:filter(fun ({_Type, _Pub, _Priv, Curve, _SharedSecret}) ->
                         lists:member(Curve, Curves)
                 end,
                 TestCases).

dh() ->
    {dh, 90970053988169282502023478715631717259407236400413906591937635666709823903223997309250405131675572047545403771567755831138144089197560332757755059848492919215391041119286178688014693040542889497092308638580104031455627238700168892909539193174537248629499995652186913900511641708112112482297874449292467498403, 2}.

rsa_oaep() ->
    %% ftp://ftp.rsa.com/pub/rsalabs/tmp/pkcs1v15crypt-vectors.txt
    Public = [hexstr2bin("010001"),
	      hexstr2bin("a8b3b284af8eb50b387034a860f146c4919f318763cd6c5598c8ae4811a1e0abc4c7e0b082d693a5e7fced675cf4668512772c0cbc64a742c6c630f533c8cc72f62ae833c40bf25842e984bb78bdbf97c0107d55bdb662f5c4e0fab9845cb5148ef7392dd3aaff93ae1e6b667bb3d4247616d4f5ba10d4cfd226de88d39f16fb")],
    Private = Public ++ [hexstr2bin("53339cfdb79fc8466a655c7316aca85c55fd8f6dd898fdaf119517ef4f52e8fd8e258df93fee180fa0e4ab29693cd83b152a553d4ac4d1812b8b9fa5af0e7f55fe7304df41570926f3311f15c4d65a732c483116ee3d3d2d0af3549ad9bf7cbfb78ad884f84d5beb04724dc7369b31def37d0cf539e9cfcdd3de653729ead5d1"),
			 hexstr2bin("d32737e7267ffe1341b2d5c0d150a81b586fb3132bed2f8d5262864a9cb9f30af38be448598d413a172efb802c21acf1c11c520c2f26a471dcad212eac7ca39d"),
			 hexstr2bin("cc8853d1d54da630fac004f471f281c7b8982d8224a490edbeb33d3e3d5cc93c4765703d1dd791642f1f116a0dd852be2419b2af72bfe9a030e860b0288b5d77"),
			 hexstr2bin("0e12bf1718e9cef5599ba1c3882fe8046a90874eefce8f2ccc20e4f2741fb0a33a3848aec9c9305fbecbd2d76819967d4671acc6431e4037968db37878e695c1"),
			 hexstr2bin("95297b0f95a2fa67d00707d609dfd4fc05c89dafc2ef6d6ea55bec771ea333734d9251e79082ecda866efef13c459e1a631386b7e354c899f5f112ca85d71583"),
			 hexstr2bin("4f456c502493bdc0ed2ab756a3a6ed4d67352a697d4216e93212b127a63d5411ce6fa98d5dbefd73263e3728142743818166ed7dd63687dd2a8ca1d2f4fbd8e1")],
    %%Msg = hexstr2bin("6628194e12073db03ba94cda9ef9532397d50dba79b987004afefe34"),
    Msg =  hexstr2bin("750c4047f547e8e41411856523298ac9bae245efaf1397fbe56f9dd5"),
    {rsa, Public, Private, Msg, rsa_pkcs1_oaep_padding}.

ecc() ->
%%               http://point-at-infinity.org/ecc/nisttv
%%
%% Test vectors for the NIST elliptic curves P192, P224, P256, P384, P521,
%% B163, B233, B283, B409, B571, K163, K233, K283, K409 and K571. For more
%% information about the curves see
%%       http://csrc.nist.gov/encryption/dss/ecdsa/NISTReCur.pdf
%%
    Curves = crypto:ec_curves(),
    TestCases =
        [{ecdh,secp192r1,1,
          hexstr2point("188DA80EB03090F67CBF20EB43A18800F4FF0AFD82FF1012",
                       "07192B95FFC8DA78631011ED6B24CDD573F977A11E794811")},
         {ecdh,secp192r1,2,
          hexstr2point("DAFEBF5828783F2AD35534631588A3F629A70FB16982A888",
                       "DD6BDA0D993DA0FA46B27BBC141B868F59331AFA5C7E93AB")},
         {ecdh,secp192r1,3,
          hexstr2point("76E32A2557599E6EDCD283201FB2B9AADFD0D359CBB263DA",
                       "782C37E372BA4520AA62E0FED121D49EF3B543660CFD05FD")},
         {ecdh,secp192r1,4,
          hexstr2point("35433907297CC378B0015703374729D7A4FE46647084E4BA",
                       "A2649984F2135C301EA3ACB0776CD4F125389B311DB3BE32")}],
    lists:filter(fun ({_Type, Curve, _Priv, _Pub}) ->
                         lists:member(Curve, Curves)
                 end,
                 TestCases).

%% Test data from Appendix D of NIST Special Publication 800-38B
%% http://csrc.nist.gov/publications/nistpubs/800-38B/Updated_CMAC_Examples.pdf
%% The same AES128 test data are also in the RFC 4493
%% https://tools.ietf.org/html/rfc4493
cmac_nist(aes_cbc128 = Type) ->
    Key = hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"),
    [{Type, Key, <<"">>,
                 hexstr2bin("bb1d6929e95937287fa37d129b756746")},
    {Type, Key, hexstr2bin("6bc1bee22e409f96e93d7e117393172a"),
                hexstr2bin("070a16b46b4d4144f79bdd9dd04a287c")},
    {Type, Key, hexstr2bin("6bc1bee22e409f96e93d7e117393172a"
                           "ae2d8a571e03ac9c9eb76fac45af8e51"
                           "30c81c46a35ce411"),
                hexstr2bin("dfa66747de9ae63030ca32611497c827")},
    {Type, Key, hexstr2bin("6bc1bee22e409f96e93d7e117393172a"
                           "ae2d8a571e03ac9c9eb76fac45af8e51"
                           "30c81c46a35ce411e5fbc1191a0a52ef"
                           "f69f2445df4f9b17ad2b417be66c3710"),
                hexstr2bin("51f0bebf7e3b9d92fc49741779363cfe")},
    % truncation
    {Type, Key, <<"">>, 4,
                 hexstr2bin("bb1d6929")}];

cmac_nist(aes_cbc256 = Type) ->
    Key = hexstr2bin("603deb1015ca71be2b73aef0857d7781"
                     "1f352c073b6108d72d9810a30914dff4"),
    [{Type, Key, <<"">>,
                 hexstr2bin("028962f61b7bf89efc6b551f4667d983")},
    {Type, Key, hexstr2bin("6bc1bee22e409f96e93d7e117393172a"),
                hexstr2bin("28a7023f452e8f82bd4bf28d8c37c35c")},
    {Type, Key, hexstr2bin("6bc1bee22e409f96e93d7e117393172a"
                           "ae2d8a571e03ac9c9eb76fac45af8e51"
                           "30c81c46a35ce411"),
                hexstr2bin("aaf3d8f1de5640c232f5b169b9c911e6")},
    {Type, Key, hexstr2bin("6bc1bee22e409f96e93d7e117393172a"
                           "ae2d8a571e03ac9c9eb76fac45af8e51"
                           "30c81c46a35ce411e5fbc1191a0a52ef"
                           "f69f2445df4f9b17ad2b417be66c3710"),
                hexstr2bin("e1992190549f6ed5696a2c056c315410")},
    % truncation
    {Type, Key, <<"">>, 4,
                 hexstr2bin("028962f6")}].


no_padding() ->
    Public = [_, Mod] = rsa_public_stronger(),
    Private = rsa_private_stronger(),
    MsgLen = erlang:byte_size(int_to_bin(Mod)),
    Msg = list_to_binary(lists:duplicate(MsgLen, $X)),
    {rsa, Public, Private, Msg, rsa_no_padding}.

int_to_bin(X) when X < 0 -> int_to_bin_neg(X, []);
int_to_bin(X) -> int_to_bin_pos(X, []).

int_to_bin_pos(0,Ds=[_|_]) ->
    list_to_binary(Ds);
int_to_bin_pos(X,Ds) ->
    int_to_bin_pos(X bsr 8, [(X band 255)|Ds]).

int_to_bin_neg(-1, Ds=[MSB|_]) when MSB >= 16#80 ->
    list_to_binary(Ds);
int_to_bin_neg(X,Ds) ->
    int_to_bin_neg(X bsr 8, [(X band 255)|Ds]).
