%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(etop_SUITE).

%% Test functions
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,text/1,text_tracing_off/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-include_lib("test_server/include/test_server.hrl").

-define(default_timeout, ?t:minutes(1)).

init_per_testcase(_Case, Config) ->
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
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


text(suite) ->
    [];
text(doc) ->
    ["Start etop with text presentation"];
text(Config) when is_list(Config) ->
    ?line {ok,Node} = ?t:start_node(node2,peer,[]),

    %% Must spawn this process, else the test case will never end.
    ?line spawn_link(etop,start,[[{node,Node},{output,text},{interval,3}]]),
    ?line timer:sleep(4000),
    ?line etop:config(interval,2),
    ?line timer:sleep(3000),
    ?line etop:config(lines,5),
    ?line timer:sleep(3000),
    ?line etop:config(accumulate,true),
    ?line timer:sleep(3000),
    ?line etop:config(sort,reductions),
    ?line timer:sleep(3000),
    ?line etop:config(sort,memory),
    ?line timer:sleep(3000),
    ?line etop:config(sort,msg_q),
    ?line timer:sleep(3000),
    ?line etop:stop(),
    ?line ?t:stop_node(Node),
    ok.

text_tracing_off(suite) ->
    [];
text_tracing_off(doc) ->
    ["Start etop with text presentation, and tracing turned off"];
text_tracing_off(Config) when is_list(Config) ->
    ?line {ok,Node} = ?t:start_node(node2,peer,[]),

    %% Must spawn this process, else the test case will never end.
    ?line spawn_link(etop,start,[[{node,Node},
				  {output,text},
				  {interval,3},
				  {tracing,off}]]),
    ?line timer:sleep(4000),
    ?line etop:config(interval,2),
    ?line timer:sleep(3000),
    ?line etop:config(lines,5),
    ?line timer:sleep(3000),
    ?line etop:config(accumulate,true),
    ?line timer:sleep(3000),
    ?line etop:config(sort,memory),
    ?line timer:sleep(3000),
    ?line etop:config(sort,msg_q),
    ?line timer:sleep(3000),
    ?line etop:config(sort,runtime), % this should not crash, but has no effect
    ?line timer:sleep(3000),
    ?line etop:stop(),
    ?line ?t:stop_node(Node),
    ok.

