%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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
-module(uri_string_property_test_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [recompose].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

%%%========================================================================
%%% Test suites
%%%========================================================================
recompose(Config) ->
    ct_property_test:quickcheck(
      uri_string_recompose:prop_recompose(),
      Config).
