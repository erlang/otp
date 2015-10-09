%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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

-module(receive_SUITE).

%% Tests receive after.

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 call_with_huge_message_queue/1,receive_in_between/1]).

-export([init_per_testcase/2,end_per_testcase/2]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [call_with_huge_message_queue, receive_in_between].

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


init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog=?t:timetrap(?t:minutes(3)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

call_with_huge_message_queue(Config) when is_list(Config) ->
    Pid = spawn_link(fun echo_loop/0),

    {Time,ok} = tc(fun() -> calls(10, Pid) end),

    [self() ! {msg,N} || N <- lists:seq(1, 500000)],
    erlang:garbage_collect(),
    {NewTime1,ok} = tc(fun() -> calls(10, Pid) end),
    {NewTime2,ok} = tc(fun() -> calls(10, Pid) end),

    io:format("Time for empty message queue: ~p", [Time]),
    io:format("Time1 for huge message queue: ~p", [NewTime1]),
    io:format("Time2 for huge message queue: ~p", [NewTime2]),

    case hd(lists:sort([(NewTime1+1) / (Time+1), (NewTime2+1) / (Time+1)])) of
	Q when Q < 10 ->
	    ok;
	Q ->
	    io:format("Best Q = ~p", [Q]),
	    ?t:fail()
    end,
    ok.

calls(0, _) -> ok;
calls(N, Pid) ->
    {ok,{ultimate_answer,42}} = call(Pid, {ultimate_answer,42}),
    calls(N-1, Pid).

call(Pid, Msg) ->
    Mref = erlang:monitor(process, Pid),
    Pid ! {Mref,{self(),Msg}},
    receive
	{Mref, Reply} ->
	    erlang:demonitor(Mref, [flush]),
	    {ok, Reply};
	{'DOWN', Mref, _, _, Reason} ->
	    exit(Reason)
    end.

receive_in_between(Config) when is_list(Config) ->
    Pid = spawn_link(fun echo_loop/0),
    [{ok,{a,b}} = call2(Pid, {a,b}) || _ <- lists:seq(1, 100000)],
    ok.

call2(Pid, Msg) ->
    self() ! dummy,
    Mref = erlang:monitor(process, Pid),
    Pid ! {Mref,{self(),Msg}},
    receive_one(),
    receive
	{Mref,Reply} ->
	    erlang:demonitor(Mref, [flush]),
	    {ok,Reply};
	{'DOWN',Mref,_,_,Reason} ->
	    exit(Reason)
    end.

receive_one() ->
    receive
	dummy -> ok
    end.

%%%
%%% Common helpers.
%%%

echo_loop() ->
    receive
	{Ref,{Pid,Msg}} ->
	    Pid ! {Ref,Msg},
	    echo_loop()
    end.

tc(Fun) ->
    timer:tc(erlang, apply, [Fun,[]]).
