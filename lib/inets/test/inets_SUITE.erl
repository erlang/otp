%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
-module(inets_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("inets_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(NUM_DEFAULT_SERVICES, 1).

suite() -> 
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,5}}
    ].

all() -> 
    [{group, app_test}, 
     {group, services_test}, httpd_reload].

groups() -> 
    [{services_test, [],
      [start_inets, 
       start_httpc, 
       start_httpd
      ]},
     {app_test, [], [app, appup]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(Case, Config) -> Config
% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initiation before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(httpd_reload, Config) ->
    inets:stop(),
    ct:timetrap({seconds, 40}),
    Config;
init_per_testcase(_Case, Config) ->
    inets:stop(),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(Case, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(_, Config) ->
    Config.

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
app() ->
    [{doc, "Test that the inets app file is ok"}].
app(Config) when is_list(Config) ->
    ok = ?t:app_test(inets).
%%--------------------------------------------------------------------
appup() ->
    [{doc, "Test that the inets appup file is ok"}].
appup(Config) when is_list(Config) ->
    ok = ?t:appup_test(inets).

start_inets() ->
    [{doc, "Test inets API functions"}].
start_inets(Config) when is_list(Config) ->
    [_|_] = inets:service_names(),

    {error,inets_not_started} = inets:services(),
    {error,inets_not_started} = inets:services_info(),

    ok = inets:start(),
    
    %% httpc default profile always started
    [_|_] = inets:services(), 
    [_|_] =  inets:services_info(),

    {error,{already_started,inets}} = inets:start(),
    
    ok = inets:stop(),
    {error,{not_started,inets}} = inets:stop(),

    ok = inets:start(transient),
    ok = inets:stop(),
   
    ok = inets:start(permanent),
    ok = inets:stop().

%%-------------------------------------------------------------------------

start_httpc() ->
    [{doc, "Start/stop of httpc service"}].
start_httpc(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    PrivDir = proplists:get_value(priv_dir, Config),

    ok = inets:start(),
    {ok, Pid0} = inets:start(httpc, [{profile, foo}]),

    Pids0 =  [ServicePid || {_, ServicePid} <- inets:services()],  
    true = lists:member(Pid0, Pids0),
    [_|_] = inets:services_info(),	

    inets:stop(httpc, Pid0),

    ct:sleep(100),

    Pids1 =  [ServicePid || {_, ServicePid} <- inets:services()], 
    false = lists:member(Pid0, Pids1),        

    {ok, Pid1} = inets:start(httpc, [{profile, bar}], stand_alone),

    Pids2 =  [ServicePid || {_, ServicePid} <- inets:services()], 
    false = lists:member(Pid1, Pids2),   

    ok = inets:stop(stand_alone, Pid1),
    receive 
	{'EXIT', Pid1, shutdown} ->
	    ok
    after 100 ->
	    ct:fail(stand_alone_not_shutdown)
    end,

    ok = inets:stop(),

    application:load(inets),
    application:set_env(inets, services, [{httpc,[{profile, foo}, 
						  {data_dir, PrivDir}]}]),
    ok = inets:start(),

    (?NUM_DEFAULT_SERVICES + 1) = length(inets:services()),

    application:unset_env(inets, services),
    ok = inets:stop(),
    ok = inets:start(),

    {ok, Pid3} = inets:start(httpc, [{profile, foo}]),
    ok = inets:stop(httpc, foo),
    Pids3 =  [ServicePid || {_, ServicePid} <- inets:services()], 
    false = lists:member(Pid3, Pids3),      
    ok = inets:stop().

%%-------------------------------------------------------------------------

start_httpd() ->
    [{doc, "Start/stop of httpd service"}].
start_httpd(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    PrivDir = proplists:get_value(priv_dir, Config),
    HttpdConf = [{server_name, "httpd_test"}, {server_root, PrivDir},
		 {document_root, PrivDir}, {bind_address, any}],
    
    ok = inets:start(),
    {ok, Pid0} = inets:start(httpd, [{port, 0}, {ipfamily, inet} | HttpdConf]),
    Pids0 =  [ServicePid || {_, ServicePid} <- inets:services()],  
    true = lists:member(Pid0, Pids0),
    [_|_] = inets:services_info(),	
    inets:stop(httpd, Pid0),
    Pids1 =  [ServicePid || {_, ServicePid} <- inets:services()],
    false = lists:member(Pid0, Pids1),
    {ok, Pid0b} =
	inets:start(httpd, [{port, 0}, {ipfamily, inet6fb4} | HttpdConf]),
    Pids0b =  [ServicePid || {_, ServicePid} <- inets:services()],
    true = lists:member(Pid0b, Pids0b),
    [_|_] = inets:services_info(),
    inets:stop(httpd, Pid0b),
    Pids1 =  [ServicePid || {_, ServicePid} <- inets:services()], 
    false = lists:member(Pid0b, Pids1),
    {ok, Pid1} = 
	inets:start(httpd, [{port, 0}, {ipfamily, inet} | HttpdConf], 
		    stand_alone),
    Pids2 =  [ServicePid || {_, ServicePid} <- inets:services()], 
    false = lists:member(Pid1, Pids2),   
    ok = inets:stop(stand_alone, Pid1),
    receive 
	{'EXIT', Pid1, shutdown} ->
	    ok
    after 100 ->
	    ct:fail(stand_alone_not_shutdown)
    end,
    ok = inets:stop(),
    File0 = filename:join(PrivDir, "httpd.conf"),
    {ok, Fd0} =  file:open(File0, [write]),
    Str = io_lib:format("~p.~n", [[{port, 0}, {ipfamily, inet} | HttpdConf]]),
    ok = file:write(Fd0, Str),
    file:close(Fd0),

    application:load(inets),
    application:set_env(inets, 
			services, [{httpd, [{proplist_file, File0}]}]),
    ok = inets:start(),
    (?NUM_DEFAULT_SERVICES + 1) = length(inets:services()),
    application:unset_env(inets, services),
    ok = inets:stop().

%%-------------------------------------------------------------------------

httpd_reload() ->
    [{doc, "Reload httpd configuration without restarting service"}].
httpd_reload(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    PrivDir = proplists:get_value(priv_dir, Config),
    DataDir =  proplists:get_value(data_dir, Config),
    HttpdConf = [{server_name,   "httpd_test"}, 
		 {server_root,   PrivDir},
		 {document_root, PrivDir}, 
		 {bind_address,  "localhost"}],

    ok = inets:start(),
    ct:sleep(5000),

    {ok, Pid0} = inets:start(httpd, [{port, 0}, 
				     {ipfamily, inet} | HttpdConf]),
    ct:sleep(5000),

    [{port, Port0}] = httpd:info(Pid0, [port]),         
    ct:sleep(5000),

    [{document_root, PrivDir}] =  httpd:info(Pid0, [document_root]),
    ct:sleep(5000),

    ok = httpd:reload_config([{port, Port0}, {ipfamily, inet},
			      {server_name, "httpd_test"}, 
			      {server_root, PrivDir},
			      {document_root, DataDir}, 
			      {bind_address, "localhost"}], non_disturbing),
    ct:sleep(5000),    
    [{document_root, DataDir}] =  httpd:info(Pid0, [document_root]),
    ct:sleep(5000),    

    ok = httpd:reload_config([{port, Port0}, {ipfamily, inet},
			      {server_name, "httpd_test"}, 
			      {server_root, PrivDir},
			      {document_root, PrivDir}, 
			      {bind_address, "localhost"}], disturbing),

    [{document_root, PrivDir}] =  httpd:info(Pid0, [document_root]),
    ok = inets:stop(httpd, Pid0),
    ok = inets:stop().
