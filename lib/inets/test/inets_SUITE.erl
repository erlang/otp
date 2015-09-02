%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2015. All Rights Reserved.
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
-include("test_server_line.hrl").
-include("inets_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(NUM_DEFAULT_SERVICES, 1).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group, app_test}, {group, appup_test},
     {group, services_test}, httpd_reload].

groups() -> 
    [{services_test, [],
      [start_inets, start_httpc, start_httpd, start_ftpc,
       start_tftpd]},
     {app_test, [], [{inets_app_test, all}]},
     {appup_test, [], [{inets_appup_test, all}]}].

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



%%-------------------------------------------------------------------------

start_inets(doc) ->
    ["Test inets API functions"];
start_inets(suite) ->
    [];
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

start_httpc(doc) ->
    ["Start/stop of httpc service"];
start_httpc(suite) ->
    [];
start_httpc(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    tsp("start_httpc -> entry with"
	"~n   Config: ~p", [Config]),

    PrivDir = ?config(priv_dir, Config),

    tsp("start_httpc -> start (empty) inets"),
    ok = inets:start(),

    tsp("start_httpc -> start httpc (as inets service) with profile foo"),
    {ok, Pid0} = inets:start(httpc, [{profile, foo}]),

    tsp("start_httpc -> check running services"),
    Pids0 =  [ServicePid || {_, ServicePid} <- inets:services()],  
    true = lists:member(Pid0, Pids0),
    [_|_] = inets:services_info(),	

    tsp("start_httpc -> stop httpc"),
    inets:stop(httpc, Pid0),

    tsp("start_httpc -> sleep some"),
    test_server:sleep(100),

    tsp("start_httpc -> check running services"),
    Pids1 =  [ServicePid || {_, ServicePid} <- inets:services()], 
    false = lists:member(Pid0, Pids1),        

    tsp("start_httpc -> start httpc (stand-alone) with profile bar"),
    {ok, Pid1} = inets:start(httpc, [{profile, bar}], stand_alone),

    tsp("start_httpc -> check running services"),
    Pids2 =  [ServicePid || {_, ServicePid} <- inets:services()], 
    false = lists:member(Pid1, Pids2),   

    tsp("start_httpc -> stop httpc"),
    ok = inets:stop(stand_alone, Pid1),
    receive 
	{'EXIT', Pid1, shutdown} ->
	    ok
    after 100 ->
	    tsf(stand_alone_not_shutdown)
    end,

    tsp("start_httpc -> stop inets"),
    ok = inets:stop(),

    tsp("start_httpc -> unload inets"),
    application:load(inets),

    tsp("start_httpc -> set inets environment (httpc profile foo)"),
    application:set_env(inets, services, [{httpc,[{profile, foo}, 
						  {data_dir, PrivDir}]}]),

    tsp("start_httpc -> start inets"),
    ok = inets:start(),

    tsp("start_httpc -> check running services"),
    (?NUM_DEFAULT_SERVICES + 1) = length(inets:services()),

    tsp("start_httpc -> unset inets env"),
    application:unset_env(inets, services),

    tsp("start_httpc -> stop inets"),
    ok = inets:stop(),

    tsp("start_httpc -> start (empty) inets"),
    ok = inets:start(),

    tsp("start_httpc -> start inets httpc service with profile foo"),
    {ok, Pid3} = inets:start(httpc, [{profile, foo}]),

    tsp("start_httpc -> stop inets service httpc with profile foo"),
    ok = inets:stop(httpc, foo),

    tsp("start_httpc -> check running services"),
    Pids3 =  [ServicePid || {_, ServicePid} <- inets:services()], 
    false = lists:member(Pid3, Pids3),      

    tsp("start_httpc -> stop inets"),
    ok = inets:stop(),

    tsp("start_httpc -> done"),    
    ok.


%%-------------------------------------------------------------------------

start_httpd(doc) ->
    ["Start/stop of httpd service"];
start_httpd(suite) ->
    [];
start_httpd(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    i("start_httpd -> entry with"
      "~n   Config: ~p", [Config]),
    PrivDir = ?config(priv_dir, Config),
    HttpdConf = [{server_name, "httpd_test"}, {server_root, PrivDir},
		 {document_root, PrivDir}, {bind_address, "localhost"}],
    
    i("start_httpd -> start inets"),
    ok = inets:start(),

    i("start_httpd -> start httpd service"),
    {ok, Pid0} = inets:start(httpd, [{port, 0}, {ipfamily, inet} | HttpdConf]),
    Pids0 =  [ServicePid || {_, ServicePid} <- inets:services()],  
    true = lists:member(Pid0, Pids0),
    [_|_] = inets:services_info(),	

    i("start_httpd -> stop httpd service"),
    inets:stop(httpd, Pid0),
    test_server:sleep(500),
    Pids1 =  [ServicePid || {_, ServicePid} <- inets:services()], 
    false = lists:member(Pid0, Pids1),        
    i("start_httpd -> start (stand-alone) httpd service"),
    {ok, Pid1} = 
	inets:start(httpd, [{port, 0}, {ipfamily, inet} | HttpdConf], 
		    stand_alone),
    Pids2 =  [ServicePid || {_, ServicePid} <- inets:services()], 
    false = lists:member(Pid1, Pids2),   
    i("start_httpd -> stop (stand-alone) httpd service"),
    ok = inets:stop(stand_alone, Pid1),
    receive 
	{'EXIT', Pid1, shutdown} ->
	    ok
    after 100 ->
	    test_server:fail(stand_alone_not_shutdown)
    end,
    i("start_httpd -> stop inets"),
    ok = inets:stop(),
    File0 = filename:join(PrivDir, "httpd.conf"),
    {ok, Fd0} =  file:open(File0, [write]),
    Str = io_lib:format("~p.~n", [[{port, 0}, {ipfamily, inet} | HttpdConf]]),
    ok = file:write(Fd0, Str),
    file:close(Fd0),

    i("start_httpd -> [application] load inets"),
    application:load(inets),
    i("start_httpd -> [application] set httpd services env with proplist-file"),
    application:set_env(inets, 
			services, [{httpd, [{proplist_file, File0}]}]),
    i("start_httpd -> start inets"),
    ok = inets:start(),
    (?NUM_DEFAULT_SERVICES + 1) = length(inets:services()),
    i("start_httpd -> [application] unset services env"),
    application:unset_env(inets, services),
    i("start_httpd -> stop inets"),
    ok = inets:stop(),
    
    File1 = filename:join(PrivDir, "httpd_apache.conf"),
    
    {ok, Fd1} =  file:open(File1, [write]),
    file:write(Fd1, "ServerName   httpd_test\r\n"),
    file:write(Fd1, "ServerRoot   " ++ PrivDir ++ "\r\n"),
    file:write(Fd1, "DocumentRoot " ++ PrivDir ++" \r\n"),    
    file:write(Fd1, "BindAddress  *|inet\r\n"),
    file:write(Fd1, "Port 0\r\n"),
    file:close(Fd1),

    i("start_httpd -> [application] load inets"),
    application:load(inets),
    i("start_httpd -> [application] set httpd services env with file"),
    application:set_env(inets, 
			services, [{httpd, [{file, File1}]}]),
    i("start_httpd -> start inets"),
    ok = inets:start(),
    (?NUM_DEFAULT_SERVICES + 1) = length(inets:services()),
    i("start_httpd -> [application] unset services env"),
    application:unset_env(inets, services),
    i("start_httpd -> stop inets"),
    ok = inets:stop(),
    
    %% OLD format
    i("start_httpd -> [application] load inets"),
    application:load(inets),
    i("start_httpd -> [application] set httpd services OLD env"),
    application:set_env(inets, 
			services, [{httpd, File1}]),
    i("start_httpd -> start inets"),
    ok = inets:start(),
    (?NUM_DEFAULT_SERVICES + 1) = length(inets:services()),
    i("start_httpd -> [application] unset services enc"),
    application:unset_env(inets, services),
    i("start_httpd -> stop inets"),
    ok = inets:stop(),

    i("start_httpd -> start inets"),
    ok = inets:start(),
    i("start_httpd -> try (and fail) start httpd service - server_name"),
    {error, {missing_property, server_name}} = 
	inets:start(httpd, [{port, 0},
			    {server_root, PrivDir},
			    {document_root, PrivDir}, 
			    {bind_address, "localhost"}]),
    i("start_httpd -> try (and fail) start httpd service - missing document_root"),
    {error, {missing_property, document_root}} = 
	inets:start(httpd, [{port, 0},
			    {server_name, "httpd_test"}, 
			    {server_root, PrivDir},
			    {bind_address, "localhost"}]),
    i("start_httpd -> try (and fail) start httpd service - missing server_root"),
    {error, {missing_property, server_root}} = 
	inets:start(httpd, [{port, 0},
			    {server_name, "httpd_test"}, 
			    {document_root, PrivDir},
			    {bind_address, "localhost"}]),
    i("start_httpd -> try (and fail) start httpd service - missing port"),
    {error, {missing_property, port}} = 
	inets:start(httpd, HttpdConf),
    i("start_httpd -> stop inets"),
    ok = inets:stop(),
    i("start_httpd -> done"),
    ok.
    

%%-------------------------------------------------------------------------

start_ftpc(doc) ->
    ["Start/stop of ftpc service"];
start_ftpc(suite) ->
    [];
start_ftpc(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ok = inets:start(),
    try
	begin
	    {_Tag, FtpdHost} = ftp_suite_lib:dirty_select_ftpd_host(Config),
	    case inets:start(ftpc, [{host, FtpdHost}]) of
		{ok, Pid0} ->
		    Pids0 = [ServicePid || {_, ServicePid} <- 
					       inets:services()],  
		    true = lists:member(Pid0, Pids0),
		    [_|_] = inets:services_info(),	
		    inets:stop(ftpc, Pid0),
		    test_server:sleep(100),
		    Pids1 =  [ServicePid || {_, ServicePid} <- 
						inets:services()], 
		    false = lists:member(Pid0, Pids1),        
		    {ok, Pid1} = 
			inets:start(ftpc, [{host, FtpdHost}], stand_alone),
		    Pids2 =  [ServicePid || {_, ServicePid} <- 
						inets:services()], 
		    false = lists:member(Pid1, Pids2),   
		    ok = inets:stop(stand_alone, Pid1),
		    receive 
			{'EXIT', Pid1, shutdown} ->
			    ok
		    after 100 ->
			    tsf(stand_alone_not_shutdown)
		    end,
		    ok = inets:stop(),
		    ok;
		_ ->
		    {skip, "Unable to reach selected FTP server " ++ FtpdHost}
	    end
	end
    catch
	throw:{error, not_found} ->
	    {skip, "No available FTP servers"}
    end.
	    


%%-------------------------------------------------------------------------

start_tftpd(doc) ->
    ["Start/stop of tfpd service"];
start_tftpd(suite) ->
    [];
start_tftpd(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ok = inets:start(),
    {ok, Pid0} = inets:start(tftpd, [{host, "localhost"}, {port, 0}]),
    Pids0 =  [ServicePid || {_, ServicePid} <- inets:services()],  
    true = lists:member(Pid0, Pids0),
    [_|_] = inets:services_info(),	
    inets:stop(tftpd, Pid0),
    test_server:sleep(100),
    Pids1 =  [ServicePid || {_, ServicePid} <- inets:services()], 
    false = lists:member(Pid0, Pids1),        
    {ok, Pid1} = 
	inets:start(tftpd, [{host, "localhost"}, {port, 0}], stand_alone),
    Pids2 =  [ServicePid || {_, ServicePid} <- inets:services()], 
    false = lists:member(Pid1, Pids2),   
    ok = inets:stop(stand_alone, Pid1),
    receive 
	{'EXIT', Pid1, shutdown} ->
	    ok
    after 100 ->
	    test_server:fail(stand_alone_not_shutdown)
    end,
    ok = inets:stop(),
    application:load(inets),
    application:set_env(inets, services, [{tftpd,[{host, "localhost"}, 
						  {port, 0}]}]),
    ok = inets:start(),
    (?NUM_DEFAULT_SERVICES + 1) = length(inets:services()),
    application:unset_env(inets, services),
    ok = inets:stop().


%%-------------------------------------------------------------------------

httpd_reload(doc) ->
    ["Reload httpd configuration without restarting service"];
httpd_reload(suite) ->
    [];
httpd_reload(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    i("httpd_reload -> starting"),
    PrivDir = ?config(priv_dir, Config),
    DataDir =  ?config(data_dir, Config),
    HttpdConf = [{server_name,   "httpd_test"}, 
		 {server_root,   PrivDir},
		 {document_root, PrivDir}, 
		 {bind_address,  "localhost"}],

    i("httpd_reload -> start inets"),

    ok = inets:start(),
    test_server:sleep(5000),
    i("httpd_reload -> inets started - start httpd service"),

    {ok, Pid0} = inets:start(httpd, [{port, 0}, {ipfamily, inet} | HttpdConf]),
    test_server:sleep(5000),
    i("httpd_reload -> httpd service started (~p) - get port", [Pid0]),

    [{port, Port0}] = httpd:info(Pid0, [port]),         
    test_server:sleep(5000),
    i("httpd_reload -> Port: ~p - get document root", [Port0]),

    [{document_root, PrivDir}] =  httpd:info(Pid0, [document_root]),
    test_server:sleep(5000),
    i("httpd_reload -> document root: ~p - reload config", [PrivDir]),

    ok = httpd:reload_config([{port, Port0}, {ipfamily, inet},
			      {server_name, "httpd_test"}, 
			      {server_root, PrivDir},
			      {document_root, DataDir}, 
			      {bind_address, "localhost"}], non_disturbing),
    test_server:sleep(5000),    
    io:format("~w:~w:httpd_reload - reloaded - get document root~n", [?MODULE, ?LINE]),

    [{document_root, DataDir}] =  httpd:info(Pid0, [document_root]),
    test_server:sleep(5000),    
    i("httpd_reload -> document root: ~p - reload config", [DataDir]),

    ok = httpd:reload_config([{port, Port0}, {ipfamily, inet},
			      {server_name, "httpd_test"}, 
			      {server_root, PrivDir},
			      {document_root, PrivDir}, 
			      {bind_address, "localhost"}], disturbing),

    [{document_root, PrivDir}] =  httpd:info(Pid0, [document_root]),
    ok = inets:stop(httpd, Pid0),
    ok = inets:stop(),

    File = filename:join(PrivDir, "httpd_apache.conf"),
      
    {ok, Fd0} =  file:open(File, [write]),
    file:write(Fd0, "ServerName   httpd_test\r\n"),
    file:write(Fd0, "ServerRoot   " ++ PrivDir ++ "\r\n"),
    file:write(Fd0, "DocumentRoot " ++ PrivDir ++" \r\n"),    
    file:write(Fd0, "BindAddress  *\r\n"),
    file:write(Fd0, "Port 0\r\n"),
    file:close(Fd0),

    application:load(inets),
    application:set_env(inets, 
			services, [{httpd, [{file, File}]}]),
    
    ok = inets:start(),
    [Pid1] = [HttpdPid || {httpd, HttpdPid} <- inets:services()],
    [{server_name, "httpd_test"}] =  httpd:info(Pid1, [server_name]),
    [{port, Port1}] = httpd:info(Pid1, [port]),         
    {ok, Fd1} =  file:open(File, [write]),
    file:write(Fd1, "ServerName   httpd_test2\r\n"),
    file:write(Fd1, "ServerRoot   " ++ PrivDir ++ "\r\n"),
    file:write(Fd1, "DocumentRoot " ++ PrivDir ++" \r\n"),    
    file:write(Fd1, "BindAddress  *\r\n"),
    file:write(Fd1, "Port " ++ integer_to_list(Port1) ++ "\r\n"),
    file:close(Fd1),

    ok = httpd:reload_config(File, non_disturbing),
    [{server_name, "httpd_test2"}] =  httpd:info(Pid1, [server_name]),

    {ok, Fd2} =  file:open(File, [write]),
    file:write(Fd2, "ServerName   httpd_test\r\n"),
    file:write(Fd2, "ServerRoot   " ++ PrivDir ++ "\r\n"),
    file:write(Fd2, "DocumentRoot " ++ PrivDir ++" \r\n"),    
    file:write(Fd2, "BindAddress  *\r\n"),
    file:write(Fd2, "Port " ++ integer_to_list(Port1) ++ "\r\n"),
    file:close(Fd2),
    ok = httpd:reload_config(File, disturbing),
    [{server_name, "httpd_test"}] = httpd:info(Pid1, [server_name]),
    
    ok = inets:stop(httpd, Pid1),
    application:unset_env(inets, services),
    ok = inets:stop(),
    i("httpd_reload -> starting"),
    ok.
    

tsf(Reason) ->
    test_server:fail(Reason).

tsp(F) ->
    tsp(F, []).
tsp(F, A) ->
    Timestamp = inets_lib:formated_timestamp(),
    test_server:format("** ~s ** ~p ~p:" ++ F ++ "~n", [Timestamp, self(), ?MODULE | A]).

i(F) ->
    i(F, []).

i(F, A) ->
    Timestamp = inets_lib:formated_timestamp(),
    io:format("*** ~s ~w:" ++ F ++ "~n", [Timestamp, ?MODULE | A]).
