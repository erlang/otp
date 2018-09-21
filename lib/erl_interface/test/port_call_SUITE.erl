%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2018. All Rights Reserved.
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

%%
-module(port_call_SUITE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Checks if the dynamic driver and linker loader works.
%%%
%%% These tests can only be run installed (outside clearcase).
%%%
%%% XXX In this suite is missing test cases for reference counts
%%% and that drivers are unloaded when their processes die.
%%% (For me to add :-)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-export([all/0, suite/0,
         init_per_testcase/2,
         basic/1]).

% Private exports
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {seconds, 10}}].

all() -> 
    [basic].

init_per_testcase(Case, Config) ->
    runner:init_per_testcase(?MODULE, Case, Config).

basic(Config) when is_list(Config) ->
    case os:type() of
        {unix, linux} ->
            do_basic(Config);
        {unix, sunos} ->
            do_basic(Config);
        {win32,_} ->
            do_basic(Config);
        _ ->
            {skipped, "Dynamic linking and erl_interface not fully examined"
                      " on this platform..."}
    end.

do_basic(Config) ->
    Path = proplists:get_value(data_dir, Config),

    erl_ddll:start(),

    %% Load the echo driver and verify that it was loaded.
    {ok,L1,L2}=load_port_call_driver(Path),

    %% Verify that the driver works.

    Port = open_port({spawn, port_call_drv}, [eof]),
    {hej, "hopp",4711,123445567436543653} =
        erlang:port_call(Port,{hej, "hopp",4711,123445567436543653}),
    {hej, "hopp",4711,123445567436543653} =
        erlang:port_call(Port,0,{hej, "hopp",4711,123445567436543653}),
    {[], a, [], b, c} =
        erlang:port_call(Port,1,{hej, "hopp",4711,123445567436543653}),
    {return, {[], a, [], b, c}} =
        erlang:port_call(Port,2,{[], a, [], b, c}),
    List = lists:duplicate(200,5),
    {return, List} = erlang:port_call(Port,2,List),
    {'EXIT',{badarg,_}} = (catch erlang:port_call(Port,4711,[])),
    {'EXIT',{badarg,_}} = (catch erlang:port_call(sune,2,[])),
    register(gunnar,Port),
    {return, List} = erlang:port_call(gunnar,2,List),
    {return, a} = erlang:port_call(gunnar,2,a),
    erlang:port_close(Port),
    %% Unload the driver and verify that it was unloaded.
    ok=unload_port_call_driver(L1,L2),

    {error, {already_started, _}} = erl_ddll:start(),
    ok = erl_ddll:stop(),
    ok.

load_port_call_driver(Path) ->
    {ok, L1} = erl_ddll:loaded_drivers(),
    ok = erl_ddll:load_driver(Path, port_call_drv),
    {ok, L2} = erl_ddll:loaded_drivers(),
    ["port_call_drv"] = ordsets:to_list(ordsets:subtract(ordsets:from_list(L2),
                                                         ordsets:from_list(L1))),
    {ok,L1,L2}.

unload_port_call_driver(L1,L2) ->
    {ok, L2} = erl_ddll:loaded_drivers(),
    ok = erl_ddll:unload_driver(port_call_drv),
    {ok, L3} = erl_ddll:loaded_drivers(),
    [] = ordsets:to_list(ordsets:subtract(ordsets:from_list(L3),
                                          ordsets:from_list(L1))),
    ok.
