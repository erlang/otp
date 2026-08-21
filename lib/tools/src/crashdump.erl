%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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

-module(crashdump).
-moduledoc """
A Tool for Handling Erlang Crash Dumps

This module contains functions for decrypting encrypted Erlang crash dumps,
which can be enabled with the `--enable-encrypted-crash-dumps` build flag.
""".
-moduledoc(#{ since => "OTP 29.0"}).
-include_lib("public_key/include/public_key.hrl").

-export([decrypt/2, decrypt/3, pem_decrypt/2, pem_decrypt/3]).

-doc """
This is a convenience wrapper around `crashdump:decrypt/2` that handles key
extraction from a given PEM file.
""".
-doc (#{ since => "OTP 29.0" }).
-spec pem_decrypt(EncryptedFile, KeyFile) ->
          {ok, erlang:iovec()} | {error, Reason} when
      EncryptedFile :: file:name(),
      KeyFile :: file:name(),
      Reason :: dynamic().
pem_decrypt(EncryptedFile, KeyFile) ->
    pem_decrypt_1(fun(PrivateKey) ->
                          decrypt(EncryptedFile, PrivateKey)
                  end, KeyFile).

-doc """
This is a convenience wrapper around `crashdump:decrypt/3` that handles key
extraction from a given PEM file.
""".
-doc (#{ since => "OTP 29.0" }).
-spec pem_decrypt(EncryptedFile, KeyFile, DecryptedFile) ->
          ok | {error, Reason} when
      EncryptedFile :: file:name(),
      KeyFile :: file:name(),
      DecryptedFile :: file:name(),
      Reason :: dynamic().
pem_decrypt(EncryptedFile, KeyFile, DecryptedFile) ->
    pem_decrypt_1(fun(PrivateKey) ->
                          decrypt(EncryptedFile, PrivateKey, DecryptedFile)
                  end, KeyFile).

pem_decrypt_1(F, KeyFile) ->
    case file:read_file(KeyFile) of
        {ok, KeyData} ->
            Entries0 = public_key:pem_decode(KeyData),
            Entries =
                [public_key:pem_entry_decode(PrivKey0) ||
                    {'PrivateKeyInfo', _, not_encrypted}=PrivKey0 <- Entries0],
            case [Entry || Entry <- Entries, is_private_key(Entry)] of
                [PrivateKey] ->
                    F(PrivateKey);
                [_, _ | _] ->
                    {error, ambiguous_private_key};
                [] ->
                    {error, no_private_key}
            end;
        {error, _}=Error ->
            Error
    end.

is_private_key(#'RSAPrivateKey'{}) -> true;
is_private_key(#'ML-KEMPrivateKey'{}) -> true;
is_private_key(_) -> false.

-doc """
Decrypts the Erlang crash dump at `EncryptedFile` using `PrivateKey`.
""".
-doc (#{ since => "OTP 29.0" }).
-spec decrypt(EncryptedFile,
              PrivateKey) -> {ok, erlang:iovec()} | {error, Reason} when
      EncryptedFile :: file:name(),
      PrivateKey :: public_key:private_key(),
      Reason :: dynamic().
decrypt(EncryptedFile, PrivateKey) ->
    decrypt_1(
      fun(Fd, Ref) ->
              Data = decrypt_data(fun ({data, Data}, Next) ->
                                          [Data | Next()];
                                      (truncated, _Next) ->
                                          []
                                  end, Fd, Ref),
              {ok, Data}
      end, EncryptedFile, PrivateKey).

-doc """
Decrypts the Erlang crash dump at `EncryptedFile` using `PrivateKey`, writing
the result to `DecryptedFile`.
""".
-doc (#{ since => "OTP 29.0" }).
-spec decrypt(EncryptedFile,
              PrivateKey,
              DecryptedFile) -> ok | {error, Reason} when
      EncryptedFile :: file:name(),
      PrivateKey :: public_key:private_key(),
      DecryptedFile :: file:name(),
      Reason :: dynamic().
decrypt(EncryptedFile, PrivateKey, DecryptedFile) ->
    decrypt_1(
      fun(InFd, Ref) ->
              case file:open(DecryptedFile, [write]) of
                  {ok, OutFd} ->
                      try
                          decrypt_data(fun ({data, Data}, Next) ->
                                               ok = file:write(OutFd, Data),
                                               Next();
                                           (truncated, _Next) ->
                                               ok
                                       end, InFd, Ref)
                      after
                          ok = file:close(OutFd)
                      end;
                  {error,_}=Error ->
                      Error
              end
      end, EncryptedFile, PrivateKey).

decrypt_1(F, EncryptedFile, PrivateKey) ->
    case file:open(EncryptedFile, [read, binary]) of
        {ok, Fd} ->
            try
                case decrypt_key(Fd, PrivateKey) of
                    {ok, AESKey, IV} ->
                        Ref = crypto:crypto_init(aes_256_cbc,
                                                 AESKey,
                                                 IV,
                                                 [{encrypt, false},
                                                  {padding, pkcs_padding}]),
                        F(Fd, Ref);
                    {error, _} = Error ->
                        Error
                end
            after
                file:close(Fd)
            end;
        {error, _} = Error ->
            Error
    end.

decrypt_key(Fd, PrivateKey) ->
    maybe
        {ok, <<"ENC0",
               IV:16/bytes,
               Len:16/little>>} ?= file:read(Fd, 4 + 16 + 2),
        {ok, <<Secret:Len/bytes>>} ?= file:read(Fd, Len),
        try decrypt_key_1(Secret, PrivateKey) of
            <<AESKey:32/bytes, _/bits>> ->
                {ok, AESKey, IV};
            <<_/bits>> ->
                {error, corrupt_header}
        catch
            error:_ ->
                {error, decapsulation_failed}
        end
    else
        {ok, <<_/bits>>} ->
            {error, corrupt_header};
        eof ->
            {error, corrupt_header};
        {error, _}=Error ->
            Error
    end.

decrypt_key_1(Secret, #'RSAPrivateKey'{}=PrivateKey) ->
    public_key:decrypt_private(Secret,
                               PrivateKey,
                               [{rsa_padding, rsa_no_padding}]);
decrypt_key_1(Secret, #'ML-KEMPrivateKey'{algorithm=Type,
                                          expandedkey=ExpandedKey}) ->
    crypto:decapsulate_key(Type, ExpandedKey, Secret).

%% Corresponds to the same macro in crypto.erl for performance reasons, but is
%% otherwise a completely arbitrary number.
-define(MAX_BYTES_TO_NIF, 20000).

decrypt_data(F, Fd, Ref) ->
    decrypt_data_1(fun Next() ->
                           decrypt_data_1(Next, F, Fd, Ref)
                   end, F, Fd, Ref).

decrypt_data_1(Next, F, Fd, Ref) ->
    case file:read(Fd, ?MAX_BYTES_TO_NIF) of
        {ok, <<_, _/bytes>>=Data} ->
            F({data, crypto:crypto_update(Ref, Data)}, Next);
        eof ->
            try crypto:crypto_final(Ref) of
                Data ->
                    F({data, Data}, Next)
            catch
                error:_ ->
                    %% Brutally truncated files often fail this final call.
                    %% Since we don't want to choke on such files (consider
                    %% 'enospc'), and emitting a warning is superflous as a
                    %% single look at the resulting dump makes it obvious what
                    %% happened, we'll just let `F` hide it whichever way it
                    %% wants.
                    F(truncated, Next)
            end
    end.
