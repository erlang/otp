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


-module(ct_util_cth).


-include_lib("common_test/src/ct_util.hrl").
-include_lib("common_test/include/ct_event.hrl").

%% Send a cth_error event if a callback is called with unexpected arguments

%% CT Hooks
-compile(export_all).

id(Opts) ->
    erlang:system_time(second).

init(Id, Opts) ->
    {ok,ok}.

pre_init_per_suite(Suite,Config,State) ->
    maybe_sleep(?FUNCTION_NAME,Suite),
    {Config, State}.

post_init_per_suite(Suite,Config,Return,State) ->
    maybe_sleep(?FUNCTION_NAME,Suite),
    {Return, State}.

pre_end_per_suite(Suite,Config,State) ->
    maybe_sleep(?FUNCTION_NAME,Suite),
    {Config, State}.

post_end_per_suite(Suite,Config,Return,State) ->
    maybe_sleep(?FUNCTION_NAME,Suite),
    {Return, State}.

pre_init_per_group(Suite, Group, Config, State) ->
    maybe_sleep(?FUNCTION_NAME,Suite,Group),
    {Config,State}.

post_init_per_group(Suite, Group, Config,Return,State) ->
    maybe_sleep(?FUNCTION_NAME,Suite,Group),
    {Return,State}.

pre_end_per_group(Suite, Group, Config, State) ->
    maybe_sleep(?FUNCTION_NAME,Suite,Group),
    {Config,State}.

post_end_per_group(Suite, Group, Config,Return,State) ->
    maybe_sleep(?FUNCTION_NAME,Suite,Group),
    {Return,State}.

pre_init_per_testcase(Suite, TC, Config, State) ->
    maybe_sleep(?FUNCTION_NAME,Suite,TC),
    {Config,State}.

post_init_per_testcase(Suite, TC, Config,Return,State) ->
    maybe_sleep(?FUNCTION_NAME,Suite,TC),
    {Return,State}.

pre_end_per_testcase(Suite, TC, Config, State) ->
    maybe_sleep(?FUNCTION_NAME,Suite,TC),
    {Config,State}.

post_end_per_testcase(Suite, TC, Config,Return,State) ->
    maybe_sleep(?FUNCTION_NAME,Suite,TC),
    {Return,State}.

%%%-----------------------------------------------------------------
maybe_sleep(FuncName,Suite) ->
    maybe_sleep(FuncName,Suite,undefined).
maybe_sleep(FuncName,Suite,GroupOrTC) ->
    case ct:get_config(FuncName) of
        {Suite,GroupOrTC,Fail} ->
            fail(Fail);
        {Suite,Fail} when GroupOrTC=:=undefined ->
            fail(Fail);
        _ ->
            ok
    end.

fail({timeout,T}) ->
    timer:sleep(T);
fail(kill) ->
    spawn_link(fun() -> ct:fail(hahahahahah) end),
    timer:sleep(10000).

