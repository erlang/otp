%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

-module(ct_no_init_suite_config_SUITE).

-compile(export_all).

-include("ct.hrl").

%%% This suite is used to verify that pre/post_init_per_suite
%%% callbacks are called with correct SuiteName even if no
%%% init_per_suite function exist in the suite, and that the
%%% non-exported config function fails with 'undef'.

end_per_suite(Config) ->
    Config.

all() ->
    [test_case].

test_case(Config) ->
    ok.
