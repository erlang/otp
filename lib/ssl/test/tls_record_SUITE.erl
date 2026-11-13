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

%%
%% Purpose: Unit tests for the pre-TLS-1.3 (and DTLS) record layer in
%% ssl_record. TLS-1.3 record tests live in tls_1_3_record_SUITE.
%%

-module(tls_record_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Callback functions
-export([all/0,
         init_per_suite/1,
         end_per_suite/1]).

%% Testcases
-export([is_correct_mac_different_length/0,
         is_correct_mac_different_length/1]).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [is_correct_mac_different_length].

init_per_suite(Config) ->
    try
        application:stop(crypto),
        ok = application:start(crypto),
        Config
    catch
        _:_ ->
            {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    application:stop(crypto).

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

is_correct_mac_different_length() ->
    [{doc,"The pre-TLS-1.3 / DTLS record MAC check "
      "(ssl_record:is_correct_mac/2) must treat a length-mismatched MAC as a "
      "plain mismatch (false), which the record layer maps to a bad_record_mac "
      "alert, and must not crash the connection process. crypto:hash_equals/2 "
      "raises error:badarg on different-length input."}].
is_correct_mac_different_length(_Config) ->
    %% Confirm the assumption this fix relies on: hash_equals/2 raises
    %% error:badarg (not exit, not a tuple reason) on different-length input.
    try crypto:hash_equals(<<1,2>>, <<1,2,3>>) of
        Unexpected ->
            ct:fail("hash_equals/2 returned ~p for different-length input, "
                    "expected error:badarg", [Unexpected])
    catch
        error:badarg ->
            ok
    end,

    %% Different length -> mismatch (false), not a crash.
    false = ssl_record:is_correct_mac(<<0:128>>, <<0:120>>),
    false = ssl_record:is_correct_mac(<<0:120>>, <<0:128>>),
    %% Equal length still compares normally.
    true  = ssl_record:is_correct_mac(<<0:128>>, <<0:128>>),
    false = ssl_record:is_correct_mac(<<0:128>>, <<1:128>>),
    ok.
