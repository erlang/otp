%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026-2026. All Rights Reserved.
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

-module(pubkey_eqc_SUITE).

-behaviour(ct_suite).

%% Common test
-export([all/0,
         init_per_suite/1,
         init_per_testcase/2,
         end_per_suite/1,
         end_per_testcase/2
        ]).

%% Test cases
-export([pkix_implicit/1,
         pkix_explicit/1,
         pkix_ocsp/1
         ]).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [
     pkix_implicit,
     pkix_explicit,
     pkix_ocsp
    ].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ct:timetrap({seconds, 20}),
    try application:ensure_all_started(public_key) of
	{ok, _} ->
            ct_property_test:init_per_suite(Config)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config0) ->
    Config0.

end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

pkix_implicit(Config) when is_list(Config) ->
    %% manual test: eqc:quickcheck(pkix:implicit_encode_decode()).
    %% proper:quickcheck(pkix:implicit_encode_decode()).
    true =  ct_property_test:quickcheck(pkix:implicit_encode_decode(),
                                        Config).
pkix_explicit(Config) when is_list(Config) ->
    %% manual test: eqc:quickcheck(pkix:implicit_encode_decode()).
    %% proper:quickcheck(pkix:explicit_encode_decode()).
    true =  ct_property_test:quickcheck(pkix:explicit_encode_decode(),
                                        Config).
pkix_ocsp(Config) when is_list(Config) ->
    %% manual test: eqc:quickcheck(pkix:implicit_encode_decode()).
    %% proper:quickcheck(pkix:explicit_encode_decode()).
    true =  ct_property_test:quickcheck(pkix:ocsp_encode_decode(),
                                        Config).
