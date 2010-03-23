%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2010. All Rights Reserved.
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
-export([all/1,text/1,text_tracing_off/1]).
-export([init_per_testcase/2, fin_per_testcase/2]).

-include("test_server.hrl").

-define(default_timeout, ?t:minutes(1)).

init_per_testcase(_Case, Config) ->
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
fin_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

all(suite) -> [text,text_tracing_off].

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

