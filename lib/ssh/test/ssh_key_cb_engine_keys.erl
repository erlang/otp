%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2017. All Rights Reserved.
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
%%----------------------------------------------------------------------

%% Note: This module is used by ssh_basic_SUITE

-module(ssh_key_cb_engine_keys).
-behaviour(ssh_server_key_api).
-compile(export_all).

host_key(SshAlg, Options) ->
    KBopts = proplists:get_value(key_cb_private, Options, []),
    Engine = proplists:get_value(engine, KBopts),
    case proplists:get_value(SshAlg, KBopts) of
        undefined ->
            {error, {unknown_alg,SshAlg}};
        KeyId ->
            case crypto_alg(SshAlg) of
                undefined ->
                    {error, {unsupported_alg,SshAlg}};
                CryptoAlg ->
                    PrivKey = #{engine => Engine,
                                key_id => KeyId,
                                algorithm => CryptoAlg},
                    %% Is there a key with this reference ?
                    case crypto:privkey_to_pubkey(CryptoAlg, PrivKey) of
                        [_|_] -> 
                            {ok, PrivKey};
                        _ ->
                            {error, {no_hostkey,SshAlg}}
                    end
            end
    end.

is_auth_key(_PublicUserKey, _User, _Options) ->
    false.



crypto_alg('ssh-rsa') -> rsa;
crypto_alg('ssh-dss') -> dss;
crypto_alg(_) -> undefined.
    
