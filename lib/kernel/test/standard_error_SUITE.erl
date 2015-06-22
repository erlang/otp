%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014. All Rights Reserved.
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

-module(standard_error_SUITE).

-export([all/0,suite/0]).
-export([badarg/1,getopts/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() -> 
    [badarg,getopts].

badarg(Config) when is_list(Config) ->
    {'EXIT',{badarg,_}} = (catch io:put_chars(standard_error, [oops])),
    true = erlang:is_process_alive(whereis(standard_error)),
    ok.

getopts(Config) when is_list(Config) ->
    [{encoding,latin1}] = io:getopts(standard_error),
    ok.
