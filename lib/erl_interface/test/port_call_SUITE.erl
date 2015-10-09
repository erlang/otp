%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2011. All Rights Reserved.
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


-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, init_per_group/2,end_per_group/2, basic/1]).
% Private exports
-include_lib("test_server/include/test_server.hrl").


suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
[basic].

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


basic(suite) -> [];
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
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),

    ?line erl_ddll:start(),

    %% Load the echo driver and verify that it was loaded.
    {ok,L1,L2}=load_port_call_driver(Path),

    %% Verify that the driver works.

    ?line Port = open_port({spawn, port_call_drv}, [eof]),
    ?line {hej, "hopp",4711,123445567436543653} = 
	erlang:port_call(Port,{hej, "hopp",4711,123445567436543653}),
    ?line {hej, "hopp",4711,123445567436543653} = 
	erlang:port_call(Port,0,{hej, "hopp",4711,123445567436543653}),
    ?line {[], a, [], b, c} = 
	erlang:port_call(Port,1,{hej, "hopp",4711,123445567436543653}),
    ?line {return, {[], a, [], b, c}} = 
	erlang:port_call(Port,2,{[], a, [], b, c}),
    ?line List = lists:duplicate(200,5),
    ?line {return, List} = erlang:port_call(Port,2,List),
    ?line {'EXIT',{badarg,_}} = (catch erlang:port_call(Port,4711,[])),
    ?line {'EXIT',{badarg,_}} = (catch erlang:port_call(sune,2,[])),
    ?line register(gunnar,Port),
    ?line {return, List} = erlang:port_call(gunnar,2,List),
    ?line {return, a} = erlang:port_call(gunnar,2,a),
    ?line erlang:port_close(Port),
    %% Unload the driver and verify that it was unloaded.
    ok=unload_port_call_driver(L1,L2),

    ?line {error, {already_started, _}} = erl_ddll:start(),
    ?line ok = erl_ddll:stop(),

    ?line test_server:timetrap_cancel(Dog),
    ok.

load_port_call_driver(Path) ->
    ?line {ok, L1} = erl_ddll:loaded_drivers(),
    ?line ok = erl_ddll:load_driver(Path, port_call_drv),
    ?line {ok, L2} = erl_ddll:loaded_drivers(),
    ?line ["port_call_drv"] = ordsets:to_list(ordsets:subtract(ordsets:from_list(L2), 
							       ordsets:from_list(L1))),
    {ok,L1,L2}.

unload_port_call_driver(L1,L2) ->
    ?line {ok, L2} = erl_ddll:loaded_drivers(),
    ?line ok = erl_ddll:unload_driver(port_call_drv),
    ?line {ok, L3} = erl_ddll:loaded_drivers(),
    ?line [] = ordsets:to_list(ordsets:subtract(ordsets:from_list(L3),
						ordsets:from_list(L1))),
    ok.

