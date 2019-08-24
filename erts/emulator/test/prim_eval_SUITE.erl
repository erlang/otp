%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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

-module(prim_eval_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_testcase/2, end_per_testcase/2,
	 init_per_group/2, end_per_group/2]).

-export(['ERL-365'/1]).

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

all() ->
    ['ERL-365'].

'ERL-365'(Config) when is_list(Config) ->
    %% def_arg_reg[0] is used for storage of timeout instruction
    %% when a 'receive after' is executed. When a process was
    %% scheduled out inside prim_eval:'receive'/0 due to a function
    %% call, def_arg_reg[0] was overwritten due to storage of live
    %% registers.
    P = spawn_link(fun () ->
                           prim_eval:'receive'(fun (_M) -> 
                                                       erlang:bump_reductions((1 bsl 27)-1),
                                                       id(true),
                                                       nomatch
                                               end,
                                               200)
                   end),
    receive after 100 -> ok end,
    P ! {wont, match},
    receive after 200 -> ok end,
    ok.



id(X) ->
    X.
