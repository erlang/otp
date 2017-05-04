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

-module(skip_req_SUITE).

-compile(export_all).

-include("ct.hrl").

suite() ->
    [{require,whatever}].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    ok.

init_per_group(_,Config) ->
    Config.

end_per_group(_,_) ->
    ok.

init_per_testcase(_,Config) ->
    Config.

end_per_testcase(_,_) ->
    ok.

all() ->
    [test_case].

%% Test cases starts here.
test_case(Config) ->
    ok.
