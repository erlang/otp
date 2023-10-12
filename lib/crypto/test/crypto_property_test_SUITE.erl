%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2020. All Rights Reserved.
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

-module(crypto_property_test_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         encrypt_decrypt_one_time/1,
         init_update/1,
         init_update_multi/1
        ]).

-include_lib("common_test/include/ct.hrl").

all() -> [encrypt_decrypt_one_time,
          init_update,
          init_update_multi
         ].

%%% First prepare Config and compile the property tests for the found tool:
init_per_suite(Config) ->
    case
        try crypto:start() of
            ok -> true;
            {error, already_started} -> true;
            {error,{already_started,crypto}} -> true;
            _ -> false
        catch
            _:_ -> false
        end
    of
        true ->
            ct_property_test:init_per_suite(Config);
        false ->
	    {skip, "Crypto did not start"}
    end.


end_per_suite(Config) ->
    Config.

%%%================================================================
%%% Test suites
%%%
encrypt_decrypt_one_time(Config) ->
    ct_property_test:quickcheck(
      crypto_ng_api:prop__crypto_one_time(),
      Config
     ).

init_update(Config) ->
    ct_property_test:quickcheck(
      crypto_ng_api:prop__crypto_init_update_final(),
      Config
     ).

init_update_multi(Config) ->
    ct_property_test:quickcheck(
      crypto_ng_api_stateful:prop__crypto_init_multi(Config),
      Config
     ).

