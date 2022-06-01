%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2022. All Rights Reserved.
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

-module(etop_SUITE).

%% Test functions
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([text/1,text/2,text_tracing_off/1,text_tracing_off/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

-include_lib("common_test/include/ct.hrl").

-define(default_timeout, test_server:minutes(1)).

init_per_testcase(_Case, Config) ->
    Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
end_per_testcase(Case, Config) ->
    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [text, text_tracing_off].

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


%% Start etop with text presentation
text(_) ->
    {ok,Node} = test_server:start_node(node2,peer,[]),

    %% Must spawn this process, else the test case will never end.
    spawn_link(etop,start,[[{node,Node},{output,text},{interval,3}]]),
    timer:sleep(4000),
    etop:config(interval,2),
    timer:sleep(3000),
    etop:config(lines,5),
    timer:sleep(3000),
    etop:config(accumulate,true),
    timer:sleep(3000),
    etop:config(sort,reductions),
    timer:sleep(3000),
    etop:config(sort,memory),
    timer:sleep(3000),
    etop:config(sort,msg_q),
    timer:sleep(3000),
    ok.
text(cleanup,_Config) ->
    etop:stop(),
    {ok,Host} = inet:gethostname(),
    Node = list_to_atom("node2@"++Host),
    test_server:stop_node(Node).

text_tracing_off(suite) ->
    [];
text_tracing_off(doc) ->
    ["Start etop with text presentation, and tracing turned off"];
text_tracing_off(Config) when is_list(Config) ->
    {ok,Node} = test_server:start_node(node2,peer,[]),

    %% Must spawn this process, else the test case will never end.
    spawn_link(etop,start,[[{node,Node},
				  {output,text},
				  {interval,3},
				  {tracing,off}]]),
    timer:sleep(4000),
    etop:config(interval,2),
    timer:sleep(3000),
    etop:config(lines,5),
    timer:sleep(3000),
    etop:config(accumulate,true),
    timer:sleep(3000),
    etop:config(sort,memory),
    timer:sleep(3000),
    etop:config(sort,msg_q),
    timer:sleep(3000),
    etop:config(sort,runtime), % this should not crash, but has no effect
    timer:sleep(3000),
    ok.
text_tracing_off(cleanup,_Config) ->
    etop:stop(),
    {ok,Host} = inet:gethostname(),
    Node = list_to_atom("node2@"++Host),
    test_server:stop_node(Node).

