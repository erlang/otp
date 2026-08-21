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

-module(crashdump_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0, suite/0]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([decrypt/1]).

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    catch lcnt:stop(),
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,4}}].

all() ->
    [decrypt].

%%----------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------

decrypt(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Priv = proplists:get_value(priv_dir, Config),

    %% This also covers crashdump:decrypt/2,3, as the PEM variants are just a
    %% thin wrapper around them.
    %%
    %% As an aside, the test dumps are forcefully truncated on purpose, without
    %% using the graceful termination provided by ERL_CRASH_DUMP_BYTES, as
    %% we've seen a lot of that in the wild and decryption ought to tolerate
    %% that.
    %%
    %% Should this occur, the silent truncation in the middle of the damaged
    %% file will make it obvious what happened.
    ok = crashdump:pem_decrypt(filename:join(Data, "rsa_encrypted_dump"),
                               filename:join(Data, "rsa_key.pem"),
                               filename:join(Priv, "rsa_out")),
    {ok, RSA} = file:read_file(filename:join(Data, "rsa_decrypted_dump")),
    {ok, RSA} = file:read_file(filename:join(Priv, "rsa_out")),

    {ok, RSA_IOVec} =
        crashdump:pem_decrypt(filename:join(Data, "rsa_encrypted_dump"),
                              filename:join(Data, "rsa_key.pem")),
    RSA = iolist_to_binary(RSA_IOVec),

    case lists:member(mlkem768, crypto:supports(kems)) of
        true ->
            ok = crashdump:pem_decrypt(filename:join(Data,
                                                     "ml-kem_encrypted_dump"),
                                       filename:join(Data, "ml-kem_key.pem"),
                                       filename:join(Priv, "ml-kem_out")),
            {ok, MLKEM} =
                file:read_file(filename:join(Data, "ml-kem_decrypted_dump")),
            {ok, MLKEM} = file:read_file(filename:join(Priv, "ml-kem_out")),

            {ok, MLKEM_IOVec} =
                crashdump:pem_decrypt(
                  filename:join(Data, "ml-kem_encrypted_dump"),
                  filename:join(Data, "ml-kem_key.pem")),
            MLKEM = iolist_to_binary(MLKEM_IOVec);
        false ->
            ok
    end,

    ok.
