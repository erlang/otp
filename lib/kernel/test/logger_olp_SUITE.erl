%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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
-module(logger_olp_SUITE).

-compile(export_all).

-include_lib("kernel/src/logger_olp.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(Case, Config) ->
    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,
    ok.

groups() ->
    [].

all() -> 
    [idle_timer].

%%%-----------------------------------------------------------------
%%% Test cases
idle_timer(_Config) ->
    {ok,_Pid,Olp} = logger_olp:start_link(?MODULE,?MODULE,self(),#{}),
    [logger_olp:load(Olp,{msg,N}) || N<-lists:seq(1,3)],
    timer:sleep(?IDLE_DETECT_TIME*2),
    [{load,{msg,1}},
     {load,{msg,2}},
     {load,{msg,3}},
     {notify,idle}] = test_server:messages_get(),
    logger_olp:cast(Olp,hello),
    timer:sleep(?IDLE_DETECT_TIME*2),
    [{cast,hello}] = test_server:messages_get(),
    ok.
idle_timer(cleanup,_Config) ->
    unlink(whereis(?MODULE)),
    logger_olp:stop(?MODULE),
    ok.

%%%-----------------------------------------------------------------
%%% Olp callbacks
init(P) ->
    {ok,P}.

handle_load(M,P) ->
    P ! {load,M},
    P.

handle_cast(M,P) ->
    P ! {cast,M},
    {noreply,P}.

notify(N,P) ->
    P ! {notify,N},
    P.
