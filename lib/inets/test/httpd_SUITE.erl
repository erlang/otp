%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2011. All Rights Reserved.
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
%%

-module(httpd_SUITE).

-include("test_server.hrl").
-include("test_server_line.hrl").
-include("inets_test_lib.hrl").

-include_lib("kernel/include/file.hrl").

%% Test server specific exports
-export([all/1]).
-export([init_per_testcase/2, end_per_testcase/2,
	 init_per_suite/1, end_per_suite/1]).

%% Test cases must be exported.
-export([ip/1, ssl/1, http_1_1_ip/1, http_1_0_ip/1, http_0_9_ip/1,
	 ipv6/1, tickets/1]).

%% Core Server tests
-export([ip_mod_alias/1, ip_mod_actions/1, ip_mod_security/1, ip_mod_auth/1,
	 ip_mod_auth_api/1, ip_mod_auth_mnesia_api/1, 
	 ip_mod_htaccess/1, ip_mod_cgi/1, ip_mod_esi/1,
	 ip_mod_get/1, ip_mod_head/1, ip_mod_all/1, ip_load_light/1,
	 ip_load_medium/1, ip_load_heavy/1, ip_dos_hostname/1, 
	 ip_time_test/1, ip_block_disturbing_idle/1, 
	 ip_block_non_disturbing_idle/1, ip_block_503/1, 
	 ip_block_disturbing_active/1, ip_block_non_disturbing_active/1, 
	 ip_block_disturbing_active_timeout_not_released/1, 
	 ip_block_disturbing_active_timeout_released/1, 
	 ip_block_non_disturbing_active_timeout_not_released/1, 
	 ip_block_non_disturbing_active_timeout_released/1, 
	 ip_block_disturbing_blocker_dies/1, 
	 ip_block_non_disturbing_blocker_dies/1, 
	 ip_restart_no_block/1, ip_restart_disturbing_block/1, 
	 ip_restart_non_disturbing_block/1
	]).

-export([ssl_mod_alias/1, ssl_mod_actions/1, ssl_mod_security/1, 
	 ssl_mod_auth/1, ssl_mod_auth_api/1,  
	 ssl_mod_auth_mnesia_api/1, ssl_mod_htaccess/1, 
	 ssl_mod_cgi/1, ssl_mod_esi/1, ssl_mod_get/1, ssl_mod_head/1, 
	 ssl_mod_all/1, ssl_load_light/1, ssl_load_medium/1, 
	 ssl_load_heavy/1, ssl_dos_hostname/1, ssl_time_test/1,
	 ssl_restart_no_block/1, ssl_restart_disturbing_block/1,
	 ssl_restart_non_disturbing_block/1, ssl_block_disturbing_idle/1, 
	 ssl_block_non_disturbing_idle/1, ssl_block_503/1, 
	 ssl_block_disturbing_active/1, ssl_block_non_disturbing_active/1, 
	 ssl_block_disturbing_active_timeout_not_released/1, 
	 ssl_block_disturbing_active_timeout_released/1, 
	 ssl_block_non_disturbing_active_timeout_not_released/1, 
	 ssl_block_non_disturbing_active_timeout_released/1, 
	 ssl_block_disturbing_blocker_dies/1, 
	 ssl_block_non_disturbing_blocker_dies/1]).

%%% HTTP 1.1 tests
-export([ip_host/1, ip_chunked/1, ip_expect/1, ip_range/1,
	 ip_if_test/1, ip_http_trace/1, ip_http1_1_head/1, 
	 ip_mod_cgi_chunked_encoding_test/1]).

%%% HTTP 1.0 tests
-export([ip_head_1_0/1, ip_get_1_0/1, ip_post_1_0/1]).

%%% HTTP 0.9 tests
-export([ip_get_0_9/1]).

%%% Ticket tests
-export([ticket_5775/1,ticket_5865/1,ticket_5913/1,ticket_6003/1,
	 ticket_7304/1]).

%%% Misc
-export([ipv6_hostname/1, ipv6_address/1]).

%% Help functions 
-export([cleanup_mnesia/0, setup_mnesia/0, setup_mnesia/1]).

-define(IP_PORT, 8898).
-define(SSL_PORT, 8899).
-define(MAX_HEADER_SIZE, 256).
-define(IPV6_LOCAL_HOST, "0:0:0:0:0:0:0:1").

%% Minutes before failed auths timeout.
-define(FAIL_EXPIRE_TIME,1). 

%% Seconds before successful auths timeout.
-define(AUTH_TIMEOUT,5).

-record(httpd_user, {user_name, password, user_data}).
-record(httpd_group,{group_name, userlist}).


%%--------------------------------------------------------------------
%% all(Arg) -> [Doc] | [Case] | {skip, Comment}
%% Arg - doc | suite
%% Doc - string()
%% Case - atom() 
%%	Name of a test case function. 
%% Comment - string()
%% Description: Returns documentation/test cases in this test suite
%%		or a skip tuple if the platform is not supported.  
%%--------------------------------------------------------------------
all(doc) ->
    ["Test the http server in the intes application."];
all(suite) ->
    [
     ip, 
     ssl, 
     http_1_1_ip, 
     http_1_0_ip, 
     http_0_9_ip, 
     %% ipv6, 
     tickets
    ].
 
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
    io:format(user, "init_per_suite -> entry with"
	      "~n   Config: ~p"
	      "~n", [Config]),

    PrivDir = ?config(priv_dir, Config),
    SuiteTopDir = filename:join(PrivDir, ?MODULE),
    case file:make_dir(SuiteTopDir) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        Error ->
            throw({error, {failed_creating_suite_top_dir, Error}})
    end,

    [{suite_top_dir, SuiteTopDir},
     {node,          node()},
     {host,          inets_test_lib:hostname()},
     {address,       getaddr()} | Config].


%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------

end_per_suite(_Config) ->
    %% SuiteTopDir = ?config(suite_top_dir, Config), 
    %% inets_test_lib:del_dirs(SuiteTopDir),
    ok.


%%--------------------------------------------------------------------
%% Function: init_per_testcase(Case, Config) -> Config
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initiation before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(Case, Config) ->
    NewConfig = init_per_testcase2(Case, Config), 
    init_per_testcase3(Case, NewConfig).


init_per_testcase2(Case, Config) ->

    io:format(user, "~w:init_per_testcase2(~w) -> entry with"
	      "~n   Config: ~p"
	      "~n", [?MODULE, Case, Config]),

    IpNormal = integer_to_list(?IP_PORT)    ++ ".conf",
    IpHtacess = integer_to_list(?IP_PORT)   ++ "htacess.conf",
    SslNormal = integer_to_list(?SSL_PORT)  ++ ".conf",
    SslHtacess = integer_to_list(?SSL_PORT) ++ "htacess.conf",

    DataDir     = ?config(data_dir, Config),
    SuiteTopDir = ?config(suite_top_dir, Config),

    io:format(user, "~w:init_per_testcase2(~w) -> "
	      "~n   SuiteDir: ~p"
	      "~n   DataDir: ~p"
	      "~n", [?MODULE, Case, SuiteTopDir, DataDir]),
    
    TcTopDir  = filename:join(SuiteTopDir, Case),
    ?line ok  = file:make_dir(TcTopDir),

    io:format(user, "~w:init_per_testcase2(~w) -> "
	      "~n   TcTopDir: ~p"
	      "~n", [?MODULE, Case, TcTopDir]),

    DataSrc    = filename:join([DataDir, "server_root"]),
    ServerRoot = filename:join([TcTopDir, "server_root"]),
    
    io:format(user, "~w:init_per_testcase2(~w) -> "
	      "~n   DataSrc: ~p"
	      "~n   ServerRoot: ~p"
	      "~n", [?MODULE, Case, DataSrc, ServerRoot]),

    ok = file:make_dir(ServerRoot),
    ok = file:make_dir(filename:join([TcTopDir, "logs"])),

    NewConfig = [{tc_top_dir, TcTopDir}, {server_root, ServerRoot} | Config],

    io:format(user, "~w:init_per_testcase2(~w) -> "
	      "copy DataSrc to ServerRoot~n", 
	      [?MODULE, Case]),

    inets_test_lib:copy_dirs(DataSrc, ServerRoot),

    io:format(user, "~w:init_per_testcase2(~w) -> fix cgi~n", 
	      [?MODULE, Case]),
    EnvCGI =  filename:join([ServerRoot, "cgi-bin", "printenv.sh"]),
    {ok, FileInfo} = file:read_file_info(EnvCGI),
    ok = file:write_file_info(EnvCGI, 
			      FileInfo#file_info{mode = 8#00755}),
    
    EchoCGI = case test_server:os_type() of
	      {win32, _} ->
		  "cgi_echo.exe";
	      _ ->
		  "cgi_echo"
	      end,
    CGIDir = filename:join([ServerRoot, "cgi-bin"]),
    inets_test_lib:copy_file(EchoCGI, DataDir,  CGIDir),
    NewEchoCGI = filename:join([CGIDir, EchoCGI]),
    {ok, FileInfo1} = file:read_file_info(NewEchoCGI),
    ok = file:write_file_info(NewEchoCGI, 
			      FileInfo1#file_info{mode = 8#00755}),
    
    %% To be used by IP test cases
    io:format(user, "~w:init_per_testcase2(~w) -> ip testcase setups~n", 
	      [?MODULE, Case]),
    create_config([{port, ?IP_PORT}, {sock_type, ip_comm} | NewConfig], 
		  normal_acess, IpNormal), 
    create_config([{port, ?IP_PORT}, {sock_type, ip_comm} | NewConfig], 
    		  mod_htaccess, IpHtacess), 

    %% To be used by SSL test cases
    io:format(user, "~w:init_per_testcase2(~w) -> ssl testcase setups~n", 
	      [?MODULE, Case]),
    create_config([{port, ?SSL_PORT}, {sock_type, ssl} | NewConfig], 
		  normal_acess, SslNormal),
    create_config([{port, ?SSL_PORT}, {sock_type, ssl} | NewConfig],
    		  mod_htaccess, SslHtacess),  
  
    %% To be used by IPv6 test cases. Case-clause is so that
    %% you can do ts:run(inets, httpd_SUITE, <test case>)
    %% for all cases except the ipv6 cases as they depend
    %% on  'test_host_ipv6_only' that will only be present
    %% when you run the whole test suite due  to shortcomings
    %% of the test server.
    %% case (catch ?config(test_host_ipv6_only, Config)) of
    %% 	{_,IPv6Host,IPv6Adress,_,_} ->
    %% 	    create_ipv6_config([{port, ?IP_PORT}, 
    %% 				{sock_type, ip_comm} | NewConfig],
    %% 			       "ipv6_hostname.conf", IPv6Host),
    %% 	    create_ipv6_config([{port, ?IP_PORT}, 
    %% 				{sock_type, ip_comm} | NewConfig],
    %% 			       "ipv6_address.conf", IPv6Adress);
    %% 	_ ->
    %% 	    ok
    %%     end,
    
    io:format(user, "~w:init_per_testcase2(~w) -> done~n", 
	      [?MODULE, Case]),

    NewConfig.


init_per_testcase3(Case, Config) ->
    io:format(user, "~w:init_per_testcase3(~w) -> entry with"
	      "~n   Config: ~p", [?MODULE, Case, Config]),

    %% Clean up (we do not want this clean up in end_per_testcase
    %% if init_per_testcase crases for some testcase it will
    %% have contaminated the environment and there will be no clean up.)
    %% This init can take a few different paths so that one crashes
    %% does not mean that all invocations will.

    application:unset_env(inets, services),
    application:stop(inets),
    application:stop(ssl),
    cleanup_mnesia(),
    
    %% TraceLevel = max, 
    TraceLevel = 70, 
    TraceDest  = io, 
    inets:enable_trace(TraceLevel, TraceDest),

    %% Start initialization
    io:format(user, "~w:init_per_testcase3(~w) -> start init", 
	      [?MODULE, Case]),

    Dog = test_server:timetrap(inets_test_lib:minutes(10)),
    NewConfig = lists:keydelete(watchdog, 1, Config),
    TcTopDir = ?config(tc_top_dir, Config),
    CaseRest = 
	case atom_to_list(Case) of
	    "ip_mod_htaccess" ->
		inets_test_lib:start_http_server(
		  filename:join(TcTopDir,
				integer_to_list(?IP_PORT) ++
				"htacess.conf")),
		"mod_htaccess";
	    "ip_" ++ Rest ->
		inets_test_lib:start_http_server(
		  filename:join(TcTopDir,
				integer_to_list(?IP_PORT) ++ ".conf")),
		Rest;
	    "ticket_5913" ->
		HttpdOptions =
		    [{file,
		      filename:join(TcTopDir,
				    integer_to_list(?IP_PORT) ++ ".conf")},
		     {accept_timeout,30000},
		     {debug,[{exported_functions,
			      [httpd_manager,httpd_request_handler]}]}],
		inets_test_lib:start_http_server(HttpdOptions);
	    "ticket_"++Rest ->
		%% OTP-5913 use the new syntax of inets.config 
		inets_test_lib:start_http_server([{file,
		  filename:join(TcTopDir,
				integer_to_list(?IP_PORT) ++ ".conf")}]),
		Rest;
	    "ssl_mod_htaccess" ->
		case inets_test_lib:start_http_server_ssl(
		       filename:join(TcTopDir,
				     integer_to_list(?SSL_PORT) ++ 
				     "htacess.conf")) of
		    ok ->
			"mod_htaccess";
		    Other ->
			error_logger:info_report("Other: ~p~n", [Other]),
			{skip, "SSL does not seem to be supported"}
		end;
	    "ssl_" ++ Rest ->
		case inets_test_lib:start_http_server_ssl(
		       filename:join(TcTopDir,
				     integer_to_list(?SSL_PORT) ++ 
				     ".conf")) of
		    ok ->
			Rest;
		    Other ->
			error_logger:info_report("Other: ~p~n", [Other]),
			{skip, "SSL does not seem to be supported"}
		end;
	    "ipv6_" ++ _  = TestCaseStr ->
		{ok, Hostname} = inet:gethostname(),
		
		case lists:member(list_to_atom(Hostname), 
				  ?config(ipv6_hosts, Config)) of
		    true ->
			inets_test_lib:start_http_server(
			  filename:join(TcTopDir,
					TestCaseStr ++ ".conf"));
		    
		    false ->
			{skip, "Host does not support IPv6"}
		end
	end,

    case CaseRest of
	{skip, _} = Skip ->
	    Skip;
	"mod_auth_" ++ _ ->
	    start_mnesia(?config(node, Config)),
	    [{watchdog, Dog} | NewConfig];
	"mod_htaccess" ->
	    ServerRoot = ?config(server_root, Config), 
	    Path = filename:join([ServerRoot, "htdocs"]),
	    catch remove_htacess(Path),
	    create_htacess_data(Path, ?config(address, Config)),
	    [{watchdog, Dog} | NewConfig];
	"range" ->
	    ServerRoot = ?config(server_root, Config), 
	    Path = filename:join([ServerRoot, "htdocs"]),
	    create_range_data(Path),
	    [{watchdog, Dog} | NewConfig];
	_ ->
	    [{watchdog, Dog} | NewConfig]
    end.


%%--------------------------------------------------------------------
%% Function: end_per_testcase(Case, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    end_per_testcase2(Case, lists:keydelete(watchdog, 1, Config)),
    ok.

end_per_testcase2(Case, Config) ->
    io:format(user, "~w:end_per_testcase2(~w) -> entry with"
	      "~n   Config: ~p~n", 
	      [?MODULE, Case, Config]),
    application:unset_env(inets, services),
    application:stop(inets),
    application:stop(ssl),     
    cleanup_mnesia(),
    io:format(user, "~w:end_per_testcase2(~w) -> done~n", 
	      [?MODULE, Case]),
    ok.


%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
ip(doc) ->
    ["HTTP tests using TCP/IP"];
ip(suite) ->
    [
     ip_mod_alias, 
     ip_mod_actions, 
     ip_mod_security, 
     ip_mod_auth,
     ip_mod_auth_api, 
     ip_mod_auth_mnesia_api,
     ip_mod_htaccess, 
     ip_mod_cgi, 
     ip_mod_esi, 
     ip_mod_get,
     ip_mod_head, 
     ip_mod_all, 
     ip_load_light, 
     ip_load_medium, 
     ip_load_heavy,
     ip_dos_hostname, 
     ip_time_test, 
     ip_block_disturbing_idle, 
     ip_block_non_disturbing_idle, 
     ip_block_503, 
     ip_block_disturbing_active, 
     ip_block_non_disturbing_active, 
     ip_block_disturbing_active_timeout_not_released, 
     ip_block_disturbing_active_timeout_released, 
     ip_block_non_disturbing_active_timeout_not_released, 
     ip_block_non_disturbing_active_timeout_released, 
     ip_block_disturbing_blocker_dies, 
     ip_block_non_disturbing_blocker_dies, 
     ip_restart_no_block, 
     ip_restart_disturbing_block, 
     ip_restart_non_disturbing_block
    ].

%%-------------------------------------------------------------------------
ssl(doc) ->
    ["HTTP test using SSL"];
ssl(suite) ->
    [
     ssl_mod_alias, 
     ssl_mod_actions, 
     ssl_mod_security, 
     ssl_mod_auth,
     ssl_mod_auth_api, 
     ssl_mod_auth_mnesia_api,
     ssl_mod_htaccess, 
     ssl_mod_cgi, 
     ssl_mod_esi,
     ssl_mod_get, 
     ssl_mod_head, 
     ssl_mod_all, 
     ssl_load_light, 
     ssl_load_medium,
     ssl_load_heavy, 
     ssl_dos_hostname, 
     ssl_time_test,
     ssl_restart_no_block, 
     ssl_restart_disturbing_block,
     ssl_restart_non_disturbing_block, 
     ssl_block_disturbing_idle, 
     ssl_block_non_disturbing_idle, 
     ssl_block_503,
     ssl_block_disturbing_active, 
     ssl_block_non_disturbing_active, 
     ssl_block_disturbing_active_timeout_not_released, 
     ssl_block_disturbing_active_timeout_released, 
     ssl_block_non_disturbing_active_timeout_not_released, 
     ssl_block_non_disturbing_active_timeout_released, 
     ssl_block_disturbing_blocker_dies,
     ssl_block_non_disturbing_blocker_dies
    ].

%%-------------------------------------------------------------------------
http_1_1_ip(doc) ->
    ["HTTP/1.1"];
http_1_1_ip(suite) ->
    [
     %% ip_host, 
     %% ip_chunked, 
     %% ip_expect, 
     %% ip_range, 
     ip_if_test%% , 
     %% ip_http_trace,
     %% ip_http1_1_head, 
     %% ip_mod_cgi_chunked_encoding_test
    ].

%%-------------------------------------------------------------------------
http_1_0_ip(doc) ->
    ["HTTP/1.0"];
http_1_0_ip(suite) ->
    [
     ip_head_1_0, 
     ip_get_1_0, 
     ip_post_1_0
    ].

%%-------------------------------------------------------------------------
http_0_9_ip(doc) ->
    ["HTTP/0.9"];
http_0_9_ip(suite) ->
    [
     ip_get_0_9
    ].

%%-------------------------------------------------------------------------
ipv6(doc) ->
    ["Tests ipv6 functionality."];
ipv6(suite) ->
    [
     ipv6_hostname, 
     ipv6_address
    ].

%%-------------------------------------------------------------------------
tickets(doc) ->
    ["Test cases for reported bugs."];
tickets(suite) ->
    [
     ticket_5775, 
     ticket_5865, 
     ticket_5913, 
     ticket_6003, 
     ticket_7304
    ].

%%-------------------------------------------------------------------------
ip_mod_alias(doc) -> 
    ["Module test: mod_alias"];
ip_mod_alias(suite) -> 
    [];
ip_mod_alias(Config) when is_list(Config) ->
    httpd_mod:alias(ip_comm, ?IP_PORT, 
		    ?config(host, Config), ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_mod_actions(doc) -> 
    ["Module test: mod_actions"];
ip_mod_actions(suite) -> 
    [];
ip_mod_actions(Config) when is_list(Config) ->
    httpd_mod:actions(ip_comm, ?IP_PORT, 
		      ?config(host, Config), ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_mod_security(doc) -> 
    ["Module test: mod_security"];
ip_mod_security(suite) -> 
    [];
ip_mod_security(Config) when is_list(Config) ->
    ServerRoot = ?config(server_root, Config), 
    httpd_mod:security(ServerRoot, ip_comm, ?IP_PORT, 
		       ?config(host, Config), ?config(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ip_mod_auth(doc) -> 
    ["Module test: mod_auth"];
ip_mod_auth(suite) -> 
    [];
ip_mod_auth(Config) when is_list(Config) ->
    httpd_mod:auth(ip_comm, ?IP_PORT, 
		   ?config(host, Config), ?config(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ip_mod_auth_api(doc) -> 
    ["Module test: mod_auth_api"];
ip_mod_auth_api(suite) -> 
    [];
ip_mod_auth_api(Config) when is_list(Config) ->
    ServerRoot = ?config(server_root, Config), 
    Host =  ?config(host, Config),
    Node = ?config(node, Config),
    httpd_mod:auth_api(ServerRoot, "", ip_comm, ?IP_PORT, Host, Node),
    httpd_mod:auth_api(ServerRoot, "dets_", ip_comm, ?IP_PORT, Host, Node),
    httpd_mod:auth_api(ServerRoot, "mnesia_", ip_comm, ?IP_PORT, Host, Node),
    ok. 
%%-------------------------------------------------------------------------
ip_mod_auth_mnesia_api(doc) -> 
    ["Module test: mod_auth_mnesia_api"];
ip_mod_auth_mnesia_api(suite) -> 
    [];
ip_mod_auth_mnesia_api(Config) when is_list(Config) ->
    httpd_mod:auth_mnesia_api(ip_comm, ?IP_PORT, 
		   ?config(host, Config), ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_mod_htaccess(doc) -> 
    ["Module test: mod_htaccess"];
ip_mod_htaccess(suite) -> 
    [];
ip_mod_htaccess(Config) when is_list(Config) ->
    httpd_mod:htaccess(ip_comm, ?IP_PORT, 
		       ?config(host, Config), ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_mod_cgi(doc) ->
    ["Module test: mod_cgi"];
ip_mod_cgi(suite) ->
    [];
ip_mod_cgi(Config) when is_list(Config) ->
    case test_server:os_type() of
	vxworks ->
	    {skip, cgi_not_supported_on_vxwoks};
	_ ->
	    httpd_mod:cgi(ip_comm, ?IP_PORT, 
			  ?config(host, Config), ?config(node, Config)),
	    ok
    end.
%%-------------------------------------------------------------------------
ip_mod_esi(doc) ->
    ["Module test: mod_esi"];
ip_mod_esi(suite) ->
    [];
ip_mod_esi(Config) when is_list(Config) ->
    httpd_mod:esi(ip_comm, ?IP_PORT, 
		  ?config(host, Config), ?config(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ip_mod_get(doc) ->
    ["Module test: mod_get"];
ip_mod_get(suite) ->
    [];
ip_mod_get(Config) when is_list(Config) ->
    httpd_mod:get(ip_comm, ?IP_PORT, 
		  ?config(host, Config), ?config(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ip_mod_head(doc) ->
    ["Module test: mod_head"];
ip_mod_head(suite) ->
    [];
ip_mod_head(Config) when is_list(Config) ->
    httpd_mod:head(ip_comm, ?IP_PORT, 
		   ?config(host, Config), ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_mod_all(doc) ->
    ["All modules test"];
ip_mod_all(suite) ->
    [];
ip_mod_all(Config) when is_list(Config) ->
    httpd_mod:all(ip_comm, ?IP_PORT, 
		  ?config(host, Config), ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_load_light(doc) ->
    ["Test light load"];
ip_load_light(suite) ->
    [];
ip_load_light(Config) when is_list(Config) ->
    httpd_load:load_test(ip_comm, ?IP_PORT, ?config(host, Config), 
			 ?config(node, Config),
			 get_nof_clients(ip_comm, light)),
    ok.
%%-------------------------------------------------------------------------
ip_load_medium(doc) ->
    ["Test  medium load"];
ip_load_medium(suite) ->
    [];
ip_load_medium(Config) when is_list(Config) ->
      httpd_load:load_test(ip_comm, ?IP_PORT, ?config(host, Config),
			   ?config(node, Config),
			   get_nof_clients(ip_comm, medium)),
    ok.
%%-------------------------------------------------------------------------
ip_load_heavy(doc) ->
    ["Test heavy load"];
ip_load_heavy(suite) ->
    [];
ip_load_heavy(Config) when is_list(Config) ->
     httpd_load:load_test(ip_comm, ?IP_PORT, ?config(host, Config),
			  ?config(node, Config),
			  get_nof_clients(ip_comm, heavy)),
    ok.
%%-------------------------------------------------------------------------
ip_dos_hostname(doc) ->
    ["Denial Of Service (DOS) attack test case"];
ip_dos_hostname(suite) ->
    [];
ip_dos_hostname(Config) when is_list(Config) ->
    dos_hostname(ip_comm, ?IP_PORT, ?config(host, Config), 
		 ?config(node, Config), ?MAX_HEADER_SIZE),
    ok.
%%-------------------------------------------------------------------------
ip_time_test(doc) ->
    [""];
ip_time_test(suite) ->
    [];
ip_time_test(Config) when is_list(Config) ->
    %% <CONDITIONAL-SKIP>
    Skippable = [win32],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(Config, Condition),
    %% </CONDITIONAL-SKIP>
    
    httpd_time_test:t(ip_comm, ?config(host, Config), ?IP_PORT),
    ok.

%%-------------------------------------------------------------------------
ip_block_503(doc) ->
    ["Check that you will receive status code 503 when the server"
     " is blocked and 200 when its not blocked."];
ip_block_503(suite) ->
    [];
ip_block_503(Config) when is_list(Config) ->
    httpd_block:block_503(ip_comm, ?IP_PORT, ?config(host, Config), 
				 ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_block_disturbing_idle(doc) ->
    ["Check that you can block/unblock an idle server. The strategy " 
     "distribing does not really make a difference in this case."];
ip_block_disturbing_idle(suite) ->
    [];
ip_block_disturbing_idle(Config) when is_list(Config) ->
    httpd_block:block_disturbing_idle(ip_comm, ?IP_PORT, 
				      ?config(host, Config), 
				      ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_block_non_disturbing_idle(doc) ->
    ["Check that you can block/unblock an idle server. The strategy " 
     "non distribing does not really make a difference in this case."];
ip_block_non_disturbing_idle(suite) ->
    [];
ip_block_non_disturbing_idle(Config) when is_list(Config) ->
    httpd_block:block_non_disturbing_idle(ip_comm, ?IP_PORT, 
					  ?config(host, Config), 
					  ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_block_disturbing_active(doc) ->
    ["Check that you can block/unblock an active server. The strategy " 
     "distribing means ongoing requests should be terminated."];
ip_block_disturbing_active(suite) ->
    [];
ip_block_disturbing_active(Config) when is_list(Config) ->
    httpd_block:block_disturbing_active(ip_comm, ?IP_PORT, 
					?config(host, Config), 
					?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_block_non_disturbing_active(doc) ->
    ["Check that you can block/unblock an idle server. The strategy " 
     "non distribing means the ongoing requests should be compleated."];
ip_block_non_disturbing_active(suite) ->
    [];
ip_block_non_disturbing_active(Config) when is_list(Config) ->
    httpd_block:block_non_disturbing_idle(ip_comm, ?IP_PORT, 
					  ?config(host, Config), 
					  ?config(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ip_block_disturbing_active_timeout_not_released(doc) ->
    ["Check that you can block an active server. The strategy " 
     "distribing means ongoing requests should be compleated"
     "if the timeout does not occur."];
ip_block_disturbing_active_timeout_not_released(suite) ->
    [];
ip_block_disturbing_active_timeout_not_released(Config) 
  when is_list(Config) ->
    httpd_block:block_disturbing_active_timeout_not_released(ip_comm, 
							     ?IP_PORT, 
							     ?config(host,
								     Config), 
							     ?config(node, 
								     Config)),
    ok.
%%-------------------------------------------------------------------------
ip_block_disturbing_active_timeout_released(doc) ->
    ["Check that you can block an active server. The strategy " 
     "distribing means ongoing requests should be terminated when"
     "the timeout occurs."];
ip_block_disturbing_active_timeout_released(suite) ->
    [];
ip_block_disturbing_active_timeout_released(Config) 
  when is_list(Config) ->
    httpd_block:block_disturbing_active_timeout_released(ip_comm, 
							 ?IP_PORT, 
							 ?config(host,
								 Config), 
							 ?config(node, 
								 Config)),
    ok.

%%-------------------------------------------------------------------------
ip_block_non_disturbing_active_timeout_not_released(doc) ->
    ["Check that you can block an active server. The strategy " 
     "non non distribing means ongoing requests should be completed."];
ip_block_non_disturbing_active_timeout_not_released(suite) ->
    [];
ip_block_non_disturbing_active_timeout_not_released(Config)
  when is_list(Config) ->
    httpd_block:
	block_non_disturbing_active_timeout_not_released(ip_comm,
							 ?IP_PORT, 
							 ?config(host, 
								 Config), 
							 ?config(node, 
								 Config)),
    ok.
%%-------------------------------------------------------------------------
ip_block_non_disturbing_active_timeout_released(doc) ->
    ["Check that you can block an active server. The strategy " 
     "non non distribing means ongoing requests should be completed. "
     "When the timeout occurs the block operation sohould be canceled." ];
ip_block_non_disturbing_active_timeout_released(suite) ->
    [];
ip_block_non_disturbing_active_timeout_released(Config)
  when is_list(Config) ->
    httpd_block:
	block_non_disturbing_active_timeout_released(ip_comm,
						     ?IP_PORT, 
						     ?config(host, 
							     Config), 
						     ?config(node, 
							     Config)),
    ok.
%%-------------------------------------------------------------------------
ip_block_disturbing_blocker_dies(doc) ->
    [];
ip_block_disturbing_blocker_dies(suite) ->
    [];
ip_block_disturbing_blocker_dies(Config) when is_list(Config) ->
    httpd_block:disturbing_blocker_dies(ip_comm, ?IP_PORT, 
					?config(host, Config), 
					?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_block_non_disturbing_blocker_dies(doc) ->
    [];
ip_block_non_disturbing_blocker_dies(suite) ->
    [];
ip_block_non_disturbing_blocker_dies(Config) when is_list(Config) ->
    httpd_block:non_disturbing_blocker_dies(ip_comm, ?IP_PORT, 
					    ?config(host, Config), 
					    ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_restart_no_block(doc) ->
    [""];
ip_restart_no_block(suite) ->
    [];
ip_restart_no_block(Config) when is_list(Config) ->
    httpd_block:restart_no_block(ip_comm, ?IP_PORT, ?config(host, Config), 
				 ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_restart_disturbing_block(doc) ->
    [""];
ip_restart_disturbing_block(suite) ->
    [];
ip_restart_disturbing_block(Config) when is_list(Config) ->
    %% <CONDITIONAL-SKIP>
    Condition = 
	fun() -> 
		case os:type() of
		    {unix, linux} ->
			HW = string:strip(os:cmd("uname -m"), right, $\n),
			case HW of
			    "ppc" ->
				case inet:gethostname() of
				    {ok, "peach"} ->
					true;
				    _ ->
					false
				end;
			    _ ->
				false
			end;
		    _ ->
			false
		end
	end,
    ?NON_PC_TC_MAYBE_SKIP(Config, Condition),
    %% </CONDITIONAL-SKIP>

    httpd_block:restart_disturbing_block(ip_comm, ?IP_PORT, 
					 ?config(host, Config),
					 ?config(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ip_restart_non_disturbing_block(doc) ->
    [""];
ip_restart_non_disturbing_block(suite) ->
    [];
ip_restart_non_disturbing_block(Config) when is_list(Config) ->
    %% <CONDITIONAL-SKIP>
    Condition = 
	fun() -> 
		case os:type() of
		    {unix, linux} ->
			HW = string:strip(os:cmd("uname -m"), right, $\n),
			case HW of
			    "ppc" ->
				case inet:gethostname() of
				    {ok, "peach"} ->
					true;
				    _ ->
					false
				end;
			    _ ->
				false
			end;
		    _ ->
			false
		end
	end,
    ?NON_PC_TC_MAYBE_SKIP(Config, Condition),
    %% </CONDITIONAL-SKIP>

    httpd_block:restart_non_disturbing_block(ip_comm, ?IP_PORT,
					    ?config(host, Config), 
					    ?config(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ssl_mod_alias(doc) -> 
    ["Module test: mod_alias"];
ssl_mod_alias(suite) -> 
    [];
ssl_mod_alias(Config) when is_list(Config) ->
    httpd_mod:alias(ssl, ?SSL_PORT, 
		    ?config(host, Config), ?config(node, Config)),
    ok. 
%%-------------------------------------------------------------------------
ssl_mod_actions(doc) -> 
    ["Module test: mod_actions"];
ssl_mod_actions(suite) -> 
    [];
ssl_mod_actions(Config) when is_list(Config) ->
    httpd_mod:actions(ssl, ?SSL_PORT, 
		      ?config(host, Config), ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ssl_mod_security(doc) -> 
    ["Module test: mod_security"];
ssl_mod_security(suite) -> 
    [];
ssl_mod_security(Config) when is_list(Config) ->
    ServerRoot = ?config(server_root, Config), 
    httpd_mod:security(ServerRoot, ssl, ?SSL_PORT, 
		       ?config(host, Config), ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ssl_mod_auth(doc) -> 
    ["Module test: mod_auth"];
ssl_mod_auth(suite) -> 
    [];
ssl_mod_auth(Config) when is_list(Config) ->
    httpd_mod:auth(ssl, ?SSL_PORT, 
		   ?config(host, Config), ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ssl_mod_auth_api(doc) -> 
    ["Module test: mod_auth"];
ssl_mod_auth_api(suite) -> 
    [];
ssl_mod_auth_api(Config) when is_list(Config) ->
    ServerRoot = ?config(server_root, Config), 
    Host =  ?config(host, Config),
    Node = ?config(node, Config),
    httpd_mod:auth_api(ServerRoot, "", ssl, ?SSL_PORT, Host, Node),
    httpd_mod:auth_api(ServerRoot, "dets_", ssl, ?SSL_PORT, Host, Node),
    httpd_mod:auth_api(ServerRoot, "mnesia_", ssl, ?SSL_PORT, Host, Node),
    ok. 

%%-------------------------------------------------------------------------
ssl_mod_auth_mnesia_api(doc) -> 
    ["Module test: mod_auth_mnesia_api"];
ssl_mod_auth_mnesia_api(suite) -> 
    [];
ssl_mod_auth_mnesia_api(Config) when is_list(Config) ->
    httpd_mod:auth_mnesia_api(ssl, ?SSL_PORT, 
		   ?config(host, Config), ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ssl_mod_htaccess(doc) -> 
    ["Module test: mod_htaccess"];
ssl_mod_htaccess(suite) -> 
    [];
ssl_mod_htaccess(Config) when is_list(Config) ->
    httpd_mod:htaccess(ssl, ?SSL_PORT, 
		       ?config(host, Config), ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ssl_mod_cgi(doc) ->
    ["Module test: mod_cgi"];
ssl_mod_cgi(suite) ->
    [];
ssl_mod_cgi(Config) when is_list(Config) ->
    case test_server:os_type() of
	vxworks ->
	    {skip, cgi_not_supported_on_vxwoks};
	_ ->
	    httpd_mod:cgi(ssl, ?SSL_PORT, 
			  ?config(host, Config), ?config(node, Config)),
	    ok
    end.
%%-------------------------------------------------------------------------
ssl_mod_esi(doc) ->
    ["Module test: mod_esi"];
ssl_mod_esi(suite) ->
    [];
ssl_mod_esi(Config) when is_list(Config) ->
    httpd_mod:esi(ssl, ?SSL_PORT, 
		  ?config(host, Config), ?config(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ssl_mod_get(doc) ->
    ["Module test: mod_get"];
ssl_mod_get(suite) ->
    [];
ssl_mod_get(Config) when is_list(Config) ->
    httpd_mod:get(ssl, ?SSL_PORT, 
		  ?config(host, Config), ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ssl_mod_head(doc) ->
    ["Module test: mod_head"];
ssl_mod_head(suite) ->
    [];
ssl_mod_head(Config) when is_list(Config) ->
    httpd_mod:head(ssl, ?SSL_PORT, 
		   ?config(host, Config), ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ssl_mod_all(doc) ->
    ["All modules test"];
ssl_mod_all(suite) ->
    [];
ssl_mod_all(Config) when is_list(Config) ->
    httpd_mod:all(ssl, ?SSL_PORT, 
		  ?config(host, Config), ?config(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ssl_load_light(doc) ->
    ["Test light load"];
ssl_load_light(suite) ->
    [];
ssl_load_light(Config) when is_list(Config) ->
    httpd_load:load_test(ssl, ?SSL_PORT, ?config(host, Config), 
			 ?config(node, Config),
			 get_nof_clients(ssl, light)),
    ok.

%%-------------------------------------------------------------------------
ssl_load_medium(doc) ->
    ["Test medium load"];
ssl_load_medium(suite) ->
    [];
ssl_load_medium(Config) when is_list(Config) ->
    %% <CONDITIONAL-SKIP>
    Skippable = [win32],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(Config, Condition),
    %% </CONDITIONAL-SKIP>

    httpd_load:load_test(ssl, ?SSL_PORT, ?config(host, Config), 
			 ?config(node, Config),
			 get_nof_clients(ssl, medium)),
    ok.

%%-------------------------------------------------------------------------
ssl_load_heavy(doc) ->
    ["Test heavy load"];
ssl_load_heavy(suite) ->
    [];
ssl_load_heavy(Config) when is_list(Config) ->
    %% <CONDITIONAL-SKIP>
    Skippable = [win32],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(Config, Condition),
    %% </CONDITIONAL-SKIP>

    httpd_load:load_test(ssl, ?SSL_PORT, ?config(host, Config), 
			 ?config(node, Config),
			 get_nof_clients(ssl, heavy)),
    ok.

%%-------------------------------------------------------------------------
ssl_dos_hostname(doc) ->
    ["Denial Of Service (DOS) attack test case"];
ssl_dos_hostname(suite) ->
    [];
ssl_dos_hostname(Config) when is_list(Config) ->
    dos_hostname(ssl, ?SSL_PORT, ?config(host, Config), 
		 ?config(node, Config), ?MAX_HEADER_SIZE),
    ok.
%%-------------------------------------------------------------------------
ssl_time_test(doc) ->
    [""];
ssl_time_test(suite) ->
    [];
ssl_time_test(Config) when is_list(Config) ->
    %% <CONDITIONAL-SKIP>
    Condition = fun() -> true end,
    ?NON_PC_TC_MAYBE_SKIP(Config, Condition),
    %% </CONDITIONAL-SKIP>
    
    httpd_time_test:t(ssl, ?config(host, Config), ?SSL_PORT),
    ok.

%%-------------------------------------------------------------------------
ssl_block_503(doc) ->
    ["Check that you will receive status code 503 when the server"
     " is blocked and 200 when its not blocked."];
ssl_block_503(suite) ->
    [];
ssl_block_503(Config) when is_list(Config) ->
    httpd_block:block_503(ssl, ?SSL_PORT, ?config(host, Config), 
			  ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ssl_block_disturbing_idle(doc) ->
    ["Check that you can block/unblock an idle server. The strategy " 
     "distribing does not really make a difference in this case."];
ssl_block_disturbing_idle(suite) ->
    [];
ssl_block_disturbing_idle(Config) when is_list(Config) ->
    httpd_block:block_disturbing_idle(ssl, ?SSL_PORT, 
				      ?config(host, Config), 
				      ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ssl_block_non_disturbing_idle(doc) ->
    ["Check that you can block/unblock an idle server. The strategy " 
     "non distribing does not really make a difference in this case."];
ssl_block_non_disturbing_idle(suite) ->
    [];
ssl_block_non_disturbing_idle(Config) when is_list(Config) ->
    httpd_block:block_non_disturbing_idle(ssl, ?SSL_PORT, 
					  ?config(host, Config), 
					  ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ssl_block_disturbing_active(doc) ->
    ["Check that you can block/unblock an active server. The strategy " 
     "distribing means ongoing requests should be terminated."];
ssl_block_disturbing_active(suite) ->
    [];
ssl_block_disturbing_active(Config) when is_list(Config) ->
    httpd_block:block_disturbing_active(ssl, ?SSL_PORT, 
					?config(host, Config), 
					?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ssl_block_non_disturbing_active(doc) ->
    ["Check that you can block/unblock an idle server. The strategy " 
     "non distribing means the ongoing requests should be compleated."];
ssl_block_non_disturbing_active(suite) ->
    [];
ssl_block_non_disturbing_active(Config) when is_list(Config) ->
    httpd_block:block_non_disturbing_idle(ssl, ?SSL_PORT, 
					  ?config(host, Config), 
					  ?config(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ssl_block_disturbing_active_timeout_not_released(doc) ->
    ["Check that you can block an active server. The strategy " 
     "distribing means ongoing requests should be compleated"
     "if the timeout does not occur."];
ssl_block_disturbing_active_timeout_not_released(suite) ->
    [];
ssl_block_disturbing_active_timeout_not_released(Config) 
  when is_list(Config) ->
    httpd_block:
	block_disturbing_active_timeout_not_released(ssl, 
						     ?SSL_PORT, 
						     ?config(host,
							     Config), 
						     ?config(node, 
							     Config)),
    ok.
%%-------------------------------------------------------------------------
ssl_block_disturbing_active_timeout_released(doc) ->
    ["Check that you can block an active server. The strategy " 
     "distribing means ongoing requests should be terminated when"
     "the timeout occurs."];
ssl_block_disturbing_active_timeout_released(suite) ->
    [];
ssl_block_disturbing_active_timeout_released(Config) 
  when is_list(Config) ->
    httpd_block:block_disturbing_active_timeout_released(ssl, 
							 ?SSL_PORT, 
							 ?config(host,
								 Config), 
							 ?config(node, 
								 Config)),
    ok.

%%-------------------------------------------------------------------------
ssl_block_non_disturbing_active_timeout_not_released(doc) ->
    ["Check that you can block an active server. The strategy " 
     "non non distribing means ongoing requests should be completed."];
ssl_block_non_disturbing_active_timeout_not_released(suite) ->
    [];
ssl_block_non_disturbing_active_timeout_not_released(Config)
  when is_list(Config) ->
    httpd_block:
	block_non_disturbing_active_timeout_not_released(ssl,
							 ?SSL_PORT, 
							 ?config(host, 
								 Config), 
							 ?config(node, 
								 Config)),
    ok.
%%-------------------------------------------------------------------------
ssl_block_non_disturbing_active_timeout_released(doc) ->
    ["Check that you can block an active server. The strategy " 
     "non non distribing means ongoing requests should be completed. "
     "When the timeout occurs the block operation sohould be canceled." ];
ssl_block_non_disturbing_active_timeout_released(suite) ->
    [];
ssl_block_non_disturbing_active_timeout_released(Config)
  when is_list(Config) ->
    httpd_block:
	block_non_disturbing_active_timeout_released(ssl,
						     ?SSL_PORT, 
						     ?config(host, 
							     Config), 
						     ?config(node, 
							     Config)),
    ok.

%%-------------------------------------------------------------------------
ssl_block_disturbing_blocker_dies(doc) ->
    [];
ssl_block_disturbing_blocker_dies(suite) ->
    [];
ssl_block_disturbing_blocker_dies(Config) when is_list(Config) ->
    httpd_block:disturbing_blocker_dies(ssl, ?SSL_PORT, 
					?config(host, Config), 
					?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ssl_block_non_disturbing_blocker_dies(doc) ->
    [];
ssl_block_non_disturbing_blocker_dies(suite) ->
    [];
ssl_block_non_disturbing_blocker_dies(Config) when is_list(Config) ->
    httpd_block:non_disturbing_blocker_dies(ssl, ?SSL_PORT, 
					    ?config(host, Config), 
					    ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ssl_restart_no_block(doc) ->
    [""];
ssl_restart_no_block(suite) ->
    [];
ssl_restart_no_block(Config) when is_list(Config) ->
    httpd_block:restart_no_block(ssl, ?SSL_PORT, ?config(host, Config), 
				 ?config(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ssl_restart_disturbing_block(doc) ->
    [""];
ssl_restart_disturbing_block(suite) ->
    [];
ssl_restart_disturbing_block(Config) when is_list(Config) ->
    %% <CONDITIONAL-SKIP>
    Condition = 
	fun() -> 
		case os:type() of
		    {unix, linux} ->
			HW = string:strip(os:cmd("uname -m"), right, $\n),
			case HW of
			    "ppc" ->
				case inet:gethostname() of
				    {ok, "peach"} ->
					true;
				    _ ->
					false
				end;
			    _ ->
				false
			end;
		    _ ->
			false
		end
	end,
    ?NON_PC_TC_MAYBE_SKIP(Config, Condition),
    %% </CONDITIONAL-SKIP>

    httpd_block:restart_disturbing_block(ssl, ?SSL_PORT, 
					 ?config(host, Config), 
					 ?config(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ssl_restart_non_disturbing_block(doc) ->
    [""];
ssl_restart_non_disturbing_block(suite) ->
    [];
ssl_restart_non_disturbing_block(Config) when is_list(Config) ->
    %% <CONDITIONAL-SKIP>
    Condition = 
	fun() -> 
		case os:type() of
		    {unix, linux} ->
			HW = string:strip(os:cmd("uname -m"), right, $\n),
			case HW of
			    "ppc" ->
				case inet:gethostname() of
				    {ok, "peach"} ->
					true;
				    _ ->
					false
				end;
			    _ ->
				false
			end;
		    _ ->
			false
		end
	end,
    ?NON_PC_TC_MAYBE_SKIP(Config, Condition),
    %% </CONDITIONAL-SKIP>

    httpd_block:restart_non_disturbing_block(ssl, ?SSL_PORT, 
					    ?config(host, Config), 
					    ?config(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ip_host(doc) ->   
    ["Control that the server accepts/rejects requests with/ without host"];
ip_host(suite)->
    [];
ip_host(Config) when is_list(Config) ->
    httpd_1_1:host(ip_comm, ?IP_PORT, ?config(host, Config),
		   ?config(node, Config)),
    ok.
%%------------------------------------------------------------------------- 
ip_chunked(doc) ->   
    ["Control that the server accepts chunked requests"];
ip_chunked(suite) ->
    [];
ip_chunked(Config) when is_list(Config) ->
    httpd_1_1:chunked(ip_comm, ?IP_PORT, ?config(host, Config),
		      ?config(node, Config)),
    ok.
%%------------------------------------------------------------------------- 
ip_expect(doc) ->   
    ["Control that the server handles request with the expect header "
     "field appropiate"];
ip_expect(suite)->
    [];
ip_expect(Config) when is_list(Config) ->
    httpd_1_1:expect(ip_comm, ?IP_PORT, ?config(host, Config),
		     ?config(node, Config)),
    ok.
%%------------------------------------------------------------------------- 
ip_range(doc) ->   
    ["Control that the server can handle range requests to plain files"];
ip_range(suite)->
    [];
ip_range(Config) when is_list(Config) ->
    httpd_1_1:range(ip_comm, ?IP_PORT, ?config(host, Config),
		    ?config(node, Config)),
    ok.
%%------------------------------------------------------------------------- 
ip_if_test(doc) ->   
    ["Test that the if - request header fields is handled correclty"];
ip_if_test(suite) ->
    [];
ip_if_test(Config) when is_list(Config) ->
    ServerRoot = ?config(server_root, Config), 
    DocRoot = filename:join([ServerRoot, "htdocs"]),
    httpd_1_1:if_test(ip_comm, ?IP_PORT, ?config(host, Config),
		      ?config(node, Config), DocRoot),
    ok.
%%------------------------------------------------------------------------- 
ip_http_trace(doc) ->   
    ["Test the trace module "];
ip_http_trace(suite) -> 
    [];
ip_http_trace(Config) when is_list(Config) ->
    httpd_1_1:http_trace(ip_comm, ?IP_PORT, ?config(host, Config),
			 ?config(node, Config)),
    ok.
%%------------------------------------------------------------------------- 
ip_http1_1_head(doc) ->  
    ["Test the trace module "];
ip_http1_1_head(suite)->
    [];
ip_http1_1_head(Config) when is_list(Config) ->
    httpd_1_1:head(ip_comm, ?IP_PORT, ?config(host, Config),
			   ?config(node, Config)),
    ok.

%%------------------------------------------------------------------------- 
ip_get_0_9(doc) ->  
    ["Test simple HTTP/0.9 GET"];
ip_get_0_9(suite)->
    [];
ip_get_0_9(Config) when is_list(Config) ->
    Host =  ?config(host, Config),
    Node =  ?config(node, Config),
    ok = httpd_test_lib:verify_request(ip_comm, Host, ?IP_PORT, Node, 
				       "GET / \r\n\r\n", 
				       [{statuscode, 200},
					{version, "HTTP/0.9"} ]),
    %% Without space after uri
    ok = httpd_test_lib:verify_request(ip_comm, Host, ?IP_PORT, Node, 
				       "GET /\r\n\r\n", 
				       [{statuscode, 200},
					{version, "HTTP/0.9"} ]),
    ok = httpd_test_lib:verify_request(ip_comm, Host, ?IP_PORT, Node, 
				       "GET / HTTP/0.9\r\n\r\n", 
				       [{statuscode, 200},
					{version, "HTTP/0.9"}]),
    
    ok.
%%------------------------------------------------------------------------- 
ip_head_1_0(doc) ->  
    ["Test HTTP/1.0 HEAD"];
ip_head_1_0(suite)->
    [];
ip_head_1_0(Config) when is_list(Config) ->
    Host =  ?config(host, Config),
    Node =  ?config(node, Config),
    ok = httpd_test_lib:verify_request(ip_comm, Host, ?IP_PORT, Node, 
			 "HEAD / HTTP/1.0\r\n\r\n", [{statuscode, 200},
						    {version, "HTTP/1.0"}]),
    
    ok.
%%------------------------------------------------------------------------- 
ip_get_1_0(doc) ->  
    ["Test HTTP/1.0 GET"];
ip_get_1_0(suite)->
    [];
ip_get_1_0(Config) when is_list(Config) ->
    Host =  ?config(host, Config),
    Node =  ?config(node, Config),
    ok = httpd_test_lib:verify_request(ip_comm, Host, ?IP_PORT, Node, 
			 "GET / HTTP/1.0\r\n\r\n", [{statuscode, 200},
						    {version, "HTTP/1.0"}]),
    
    ok.
%%------------------------------------------------------------------------- 
ip_post_1_0(doc) ->  
    ["Test HTTP/1.0 POST"];
ip_post_1_0(suite)->
    [];
ip_post_1_0(Config) when is_list(Config) ->
    Host =  ?config(host, Config),
    Node =  ?config(node, Config),
    %% Test the post message formatin 1.0! Real post are testes elsewhere
    ok = httpd_test_lib:verify_request(ip_comm, Host, ?IP_PORT, Node, 
			 "POST / HTTP/1.0\r\n\r\n "  
			 "Content-Length:6 \r\n\r\nfoobar", 
			 [{statuscode, 500}, {version, "HTTP/1.0"}]),
    
    ok.
%%------------------------------------------------------------------------- 
ip_mod_cgi_chunked_encoding_test(doc) ->  
    ["Test the trace module "];
ip_mod_cgi_chunked_encoding_test(suite)->
    [];
ip_mod_cgi_chunked_encoding_test(Config) when is_list(Config) ->
    Host = ?config(host, Config),
    Script =
	case test_server:os_type() of
	    {win32, _} ->
		"/cgi-bin/printenv.bat";
	    _ ->
		"/cgi-bin/printenv.sh"
	end,
    Requests = 
	["GET " ++ Script ++ " HTTP/1.1\r\nHost:"++ Host ++"\r\n\r\n",
	 "GET /cgi-bin/erl/httpd_example/newformat  HTTP/1.1\r\nHost:"
	 ++ Host ++"\r\n\r\n"],
    httpd_1_1:mod_cgi_chunked_encoding_test(ip_comm, ?IP_PORT,
					    Host,
					    ?config(node, Config),
					    Requests),
    ok.

%------------------------------------------------------------------------- 
ipv6_hostname(doc) ->  
    ["Test standard ipv6 address"];
ipv6_hostname(suite)->
    [];
ipv6_hostname(Config) when is_list(Config) -> 
    Host = ?config(host, Config),
    httpd_test_lib:verify_request(ip_comm, Host, ?IP_PORT, node(), 
				  "GET / HTTP/1.1\r\n\r\n",
				  [{statuscode, 200},
				   {version, "HTTP/1.1"}]),
    ok.

%%------------------------------------------------------------------------- 
ipv6_address(doc) ->  
    ["Test standard ipv6 address"];
ipv6_address(suite)->
    [];
ipv6_address(Config) when is_list(Config) ->   
    httpd_test_lib:verify_request(ip_comm, ?IPV6_LOCAL_HOST, ?IP_PORT, 
				  node(), "GET / HTTP/1.1\r\n\r\n",
				  [{statuscode, 200},
				   {version, "HTTP/1.1"}]),
    ok.

%%--------------------------------------------------------------------
ticket_5775(doc) ->
    ["Tests that content-length is correct"];
ticket_5775(suite) ->
    [];
ticket_5775(Config) ->
    ok=httpd_test_lib:verify_request(ip_comm, ?config(host, Config),
				     ?IP_PORT, ?config(node, Config),
				       "GET /cgi-bin/erl/httpd_example:get_bin "
				       "HTTP/1.0\r\n\r\n", 
				       [{statuscode, 200},
				       {version, "HTTP/1.0"}]),
    ok.
ticket_5865(doc) ->
    ["Tests that a header without last-modified is handled"];
ticket_5865(suite) ->
    [];
ticket_5865(Config) ->
    Host = ?config(host,Config),
    ServerRoot = ?config(server_root, Config), 
    DocRoot = filename:join([ServerRoot, "htdocs"]),
    File = filename:join([DocRoot,"last_modified.html"]),

    Bad_mtime = case test_server:os_type() of
		    {win32, _} ->
			{{1600,12,31},{23,59,59}};
		    {unix, _} ->
			{{1969,12,31},{23,59,59}}
		end,
    
    {ok,FI}=file:read_file_info(File),
    
    case file:write_file_info(File,FI#file_info{mtime=Bad_mtime}) of
	ok ->
	    ok = httpd_test_lib:verify_request(ip_comm, Host,
					       ?IP_PORT, ?config(node, Config),
					       "GET /last_modified.html"
					       " HTTP/1.1\r\nHost:"
					       ++Host++"\r\n\r\n", 
					       [{statuscode, 200},
						{no_last_modified,
						 "last-modified"}]),
	    ok;
	{error, Reason} ->
	    Fault = 
		io_lib:format("Attempt to change the file info to set the"
			      " preconditions of the test case failed ~p~n",
			      [Reason]),
	    {skip, Fault}
    end.

ticket_5913(doc) ->
    ["Tests that a header without last-modified is handled"];
ticket_5913(suite) -> [];
ticket_5913(Config) ->
    ok=httpd_test_lib:verify_request(ip_comm, ?config(host, Config),
				     ?IP_PORT, ?config(node, Config),
				       "GET /cgi-bin/erl/httpd_example:get_bin "
				       "HTTP/1.0\r\n\r\n", 
				       [{statuscode, 200},
				       {version, "HTTP/1.0"}]),
    ok.

ticket_6003(doc) ->
    ["Tests that a URI with a bad hexadecimal code is handled"];
ticket_6003(suite) -> [];
ticket_6003(Config) ->
    ok=httpd_test_lib:verify_request(ip_comm, ?config(host, Config),
				     ?IP_PORT, ?config(node, Config),
				     "GET http://www.erlang.org/%skalle "
				     "HTTP/1.0\r\n\r\n",
				     [{statuscode, 400},
				      {version, "HTTP/1.0"}]),
    ok.

ticket_7304(doc) ->
     ["Tests missing CR in delimiter"];
ticket_7304(suite) -> 
    [];
ticket_7304(Config) ->
    ok = httpd_test_lib:verify_request(ip_comm, ?config(host, Config),
				       ?IP_PORT, ?config(node, Config),
				       "GET / HTTP/1.0\r\n\n",
				       [{statuscode, 200},
					{version, "HTTP/1.0"}]),
    ok.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
dos_hostname(Type, Port, Host, Node, Max) ->
    H1 = {"", 200},
    H2 = {"dummy-host.ericsson.se", 200},
    TooLongHeader = lists:append(lists:duplicate(Max + 1, "a")),
    H3 = {TooLongHeader, 403},
    Hosts = [H1,H2,H3],
    dos_hostname_poll(Type, Host, Port, Node, Hosts).

%% make_ipv6(T) when is_tuple(T) andalso (size(T) =:= 8) ->
%%     make_ipv6(tuple_to_list(T));

%% make_ipv6([_, _, _, _, _, _, _, _] = IPV6) ->
%%     lists:flatten(io_lib:format("~s:~s:~s:~s:~s:~s:~s:~s", IPV6)).


%%--------------------------------------------------------------------
%% Other help functions
create_config(Config, Access, FileName) ->
    ServerRoot = ?config(server_root, Config),
    TcTopDir = ?config(tc_top_dir, Config),
    Port =  ?config(port, Config),
    Type = ?config(sock_type, Config),
    Host =  ?config(host, Config),
    Mods         = io_lib:format("~p", [httpd_mod]),
    Funcs        = io_lib:format("~p", [ssl_password_cb]),
    MaxHdrSz     = io_lib:format("~p", [256]),
    MaxHdrAct    = io_lib:format("~p", [close]),
    SSL =
	case Type of
	    ssl ->
		[cline(["SSLCertificateFile ", 
			filename:join(ServerRoot, "ssl/ssl_server.pem")]),
		 cline(["SSLCertificateKeyFile ",
			filename:join(ServerRoot, "ssl/ssl_server.pem")]),
		 cline(["SSLCACertificateFile ",
			filename:join(ServerRoot, "ssl/ssl_server.pem")]),
		 cline(["SSLPasswordCallbackModule ", Mods]),
		 cline(["SSLPasswordCallbackFunction ", Funcs]),
		 cline(["SSLVerifyClient 0"]),
		 cline(["SSLVerifyDepth 1"])];
	    _ ->
		[]
	end,
    Mod_order = case Access of
		    mod_htaccess ->
			"Modules mod_alias mod_htaccess mod_auth "
			    "mod_security "
			    "mod_responsecontrol mod_trace mod_esi "
			    "mod_actions mod_cgi mod_include mod_dir "
			    "mod_range mod_get "
			    "mod_head mod_log mod_disk_log";
		    _ ->
			"Modules mod_alias mod_auth mod_security "
			    "mod_responsecontrol mod_trace mod_esi "
			    "mod_actions mod_cgi mod_include mod_dir "
			    "mod_range mod_get "
			    "mod_head mod_log mod_disk_log"
		    end,
	    
%% The test suite currently does not handle an explicit BindAddress.
%% They assume any has been used, that is Addr is always set to undefined!

%%     {ok, Hostname} = inet:gethostname(), 
%%     {ok, Addr} = inet:getaddr(Hostname, inet6),
%%     AddrStr = make_ipv6(Addr), 
%%     BindAddress = lists:flatten(io_lib:format("~s|inet6", [AddrStr])),

    %% BindAddress = "*|inet", 
    BindAddress = "*", 

    HttpConfig = [
		  cline(["Port ", integer_to_list(Port)]),
		  cline(["ServerName ", Host]),
		  cline(["SocketType ", atom_to_list(Type)]),
		  cline([Mod_order]),
		  %% cline(["LogFormat ", "erlang"]),
		  cline(["ServerAdmin mattias@erix.ericsson.se"]),
		  cline(["BindAddress ", BindAddress]),
		  cline(["ServerRoot ", ServerRoot]),
		  cline(["ErrorLog ", TcTopDir, 
		     "/logs/error_log_", integer_to_list(Port)]),
		  cline(["TransferLog ", TcTopDir, 
			 "/logs/access_log_", integer_to_list(Port)]),
		  cline(["SecurityLog ", TcTopDir, 
			 "/logs/security_log_", integer_to_list(Port)]),
		  cline(["ErrorDiskLog ", TcTopDir, 
			 "/logs/error_disk_log_", integer_to_list(Port)]),
		  cline(["ErrorDiskLogSize ", "190000 ", "11"]),
		  cline(["TransferDiskLog ", TcTopDir, 
			 "/logs/access_disk_log_", integer_to_list(Port)]),
		  cline(["TransferDiskLogSize ", "200000 ", "10"]),
		  cline(["SecurityDiskLog ", TcTopDir, 
			 "/logs/security_disk_log_", integer_to_list(Port)]),
		  cline(["SecurityDiskLogSize ", "210000 ", "9"]),
		  cline(["MaxClients 10"]),
		  cline(["MaxHeaderSize ", MaxHdrSz]),
		  cline(["MaxHeaderAction ", MaxHdrAct]),
		  cline(["DocumentRoot ", 
			 filename:join(ServerRoot, "htdocs")]),
		  cline(["DirectoryIndex ", "index.html ", "welcome.html"]),
		  cline(["DefaultType ", "text/plain"]),
		  SSL,
		  mod_alias_config(ServerRoot),
		  
		  config_directory(filename:join([ServerRoot,"htdocs",
						  "open"]),
				   "Open Area", 
				   filename:join(ServerRoot, "auth/passwd"),
				   filename:join(ServerRoot, "auth/group"),
				   plain,
				   "user one Aladdin",
				   filename:join(ServerRoot, "security_data")),
		  config_directory(filename:join([ServerRoot,"htdocs", 
						  "secret"]),
				   "Secret Area", 
				   filename:join(ServerRoot, "auth/passwd"),
				   filename:join(ServerRoot, "auth/group"),
				   plain,
				   "group group1 group2",
				   filename:join(ServerRoot, "security_data")),
		  config_directory(filename:join([ServerRoot,"htdocs", 
						  "secret", 
						  "top_secret"]),
				   "Top Secret Area", 
				   filename:join(ServerRoot, "auth/passwd"),
				   filename:join(ServerRoot, "auth/group"),
				   plain,
				   "group group3",
				   filename:join(ServerRoot, "security_data")),
		  
		  config_directory(filename:join([ServerRoot,"htdocs", 
						  "dets_open"]),
				   "Dets Open Area", 
				   filename:join(ServerRoot, "passwd"),
				   filename:join(ServerRoot, "group"),
				   dets,
				   "user one Aladdin",
				   filename:join(ServerRoot, "security_data")),
		  config_directory(filename:join([ServerRoot,"htdocs", 
						  "dets_secret"]),
			       "Dets Secret Area", 
				   filename:join(ServerRoot, "passwd"),
				   filename:join(ServerRoot, "group"),
				   dets,
				   "group group1 group2",
				   filename:join(ServerRoot, "security_data")),
		  config_directory(filename:join([ServerRoot,"htdocs", 
						  "dets_secret", 
						  "top_secret"]),
				   "Dets Top Secret Area", 
				   filename:join(ServerRoot, "passwd"),
				   filename:join(ServerRoot, "group"),
				   dets,
				   "group group3",
				   filename:join(ServerRoot, "security_data")),
		  
		  config_directory(filename:join([ServerRoot,"htdocs", 
						  "mnesia_open"]),
				   "Mnesia Open Area", 
				   false,
				   false,
				   mnesia,
				   "user one Aladdin",
			       filename:join(ServerRoot, "security_data")),
		  config_directory(filename:join([ServerRoot,"htdocs", 
						  "mnesia_secret"]),
				   "Mnesia Secret Area", 
				   false,
				   false,
				   mnesia,
				   "group group1 group2",
				   filename:join(ServerRoot, "security_data")),
		  config_directory(filename:join(
				     [ServerRoot, "htdocs", "mnesia_secret",
				      "top_secret"]),
				   "Mnesia Top Secret Area", 
				   false,
				   false,
				   mnesia,
				   "group group3",
				   filename:join(ServerRoot, "security_data"))
		 ],
    ConfigFile = filename:join([TcTopDir, FileName]),
    {ok, Fd} = file:open(ConfigFile, [write]),
    ok = file:write(Fd, lists:flatten(HttpConfig)),
    ok = file:close(Fd).

config_directory(Dir, AuthName, AuthUserFile, AuthGroupFile, AuthDBType, 
		 Require, SF) ->
    file:delete(SF),
    [
     cline(["<Directory ", Dir, ">"]),
     cline(["SecurityDataFile ", SF]),
     cline(["SecurityMaxRetries 3"]),
     cline(["SecurityFailExpireTime ", integer_to_list(?FAIL_EXPIRE_TIME)]),
     cline(["SecurityBlockTime 1"]),
     cline(["SecurityAuthTimeout ", integer_to_list(?AUTH_TIMEOUT)]),
     cline(["SecurityCallbackModule ", "httpd_mod"]),
     cline_if_set("AuthUserFile", AuthUserFile),
     cline_if_set("AuthGroupFile", AuthGroupFile),
     cline_if_set("AuthName", AuthName),
     cline_if_set("AuthDBType", AuthDBType),
     cline(["require ", Require]),
     cline(["</Directory>\r\n"])
    ].

mod_alias_config(Root) ->
    [
     cline(["Alias /icons/ ", filename:join(Root,"icons"), "/"]),
     cline(["Alias /pics/ ", filename:join(Root, "icons"), "/"]),
     cline(["ScriptAlias /cgi-bin/ ", filename:join(Root, "cgi-bin"), "/"]),
     cline(["ScriptAlias /htbin/ ", filename:join(Root, "cgi-bin"), "/"]),
     cline(["ErlScriptAlias /cgi-bin/erl httpd_example io"]),
     cline(["EvalScriptAlias /eval httpd_example io"])
    ].

cline(List) ->
    lists:flatten([List, "\r\n"]).

cline_if_set(_, false) ->
    [];
cline_if_set(Name, Var) when is_list(Var) ->
    cline([Name, " ", Var]);
cline_if_set(Name, Var) when is_atom(Var) ->
    cline([Name, " ", atom_to_list(Var)]).

getaddr() ->
    {ok,HostName} = inet:gethostname(),
    {ok,{A1,A2,A3,A4}} = inet:getaddr(HostName,inet),
    lists:flatten(io_lib:format("~p.~p.~p.~p",[A1,A2,A3,A4])).

start_mnesia(Node) ->
    case rpc:call(Node, ?MODULE, cleanup_mnesia, []) of
	ok ->
	    ok;
	Other ->
	    test_server:fail({failed_to_cleanup_mnesia, Other})
    end,
       case rpc:call(Node, ?MODULE, setup_mnesia, []) of
	{atomic, ok} ->
	    ok;
	Other2 ->
	   test_server:fail({failed_to_setup_mnesia, Other2})
    end,
    ok.

setup_mnesia() ->
  setup_mnesia([node()]).

setup_mnesia(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    ok = mnesia:start(),
    {atomic, ok} = mnesia:create_table(httpd_user,
				       [{attributes, 
					 record_info(fields, httpd_user)}, 
					{disc_copies,Nodes}, {type, set}]),
    {atomic, ok} = mnesia:create_table(httpd_group,
				       [{attributes, 
					 record_info(fields,
						     httpd_group)}, 
					{disc_copies,Nodes}, {type,bag}]).

cleanup_mnesia() ->
    mnesia:start(),
    mnesia:delete_table(httpd_user),
    mnesia:delete_table(httpd_group),
    stopped = mnesia:stop(),
    mnesia:delete_schema([node()]),
    ok.

create_htacess_data(Path, IpAddress)->
    create_htacess_dirs(Path),
    
    create_html_file(filename:join([Path,"ht/open/dummy.html"])),
    create_html_file(filename:join([Path,"ht/blocknet/dummy.html"])),
    create_html_file(filename:join([Path,"ht/secret/dummy.html"])),
    create_html_file(filename:join([Path,"ht/secret/top_secret/dummy.html"])),
    
    create_htacess_file(filename:join([Path,"ht/open/.htaccess"]),
			 Path, "user one Aladdin"),
    create_htacess_file(filename:join([Path,"ht/secret/.htaccess"]),
			 Path, "group group1 group2"),
    create_htacess_file(filename:join([Path,
				       "ht/secret/top_secret/.htaccess"]),
			Path, "user four"),
    create_htacess_file(filename:join([Path,"ht/blocknet/.htaccess"]),
			Path, nouser, IpAddress),
   
    create_user_group_file(filename:join([Path,"ht","users.file"]),
			   "one:OnePassword\ntwo:TwoPassword\nthree:"
			   "ThreePassword\nfour:FourPassword\nAladdin:"
			   "AladdinPassword"),
    create_user_group_file(filename:join([Path,"ht","groups.file"]),
			   "group1: two one\ngroup2: two three").

create_html_file(PathAndFileName)->
    file:write_file(PathAndFileName,list_to_binary(
	 "<html><head><title>test</title></head>
         <body>testar</body></html>")).

create_htacess_file(PathAndFileName, BaseDir, RequireData)->
    file:write_file(PathAndFileName,
		    list_to_binary(
		      "AuthUserFile "++ BaseDir ++
		      "/ht/users.file\nAuthGroupFile "++ BaseDir
		      ++ "/ht/groups.file\nAuthName Test\nAuthType"
		      " Basic\n<Limit>\nrequire " ++ RequireData ++
		      "\n</Limit>")).

create_htacess_file(PathAndFileName, BaseDir, nouser, IpAddress)->
    file:write_file(PathAndFileName,list_to_binary(
				      "AuthUserFile "++ BaseDir ++
				      "/ht/users.file\nAuthGroupFile " ++ 
				      BaseDir ++ "/ht/groups.file\nAuthName"
				      " Test\nAuthType"
				      " Basic\n<Limit GET>\n\tallow from " ++ 
				      format_ip(IpAddress,
						string:rchr(IpAddress,$.)) ++ 
				      "\n</Limit>")).

create_user_group_file(PathAndFileName, Data)->
    file:write_file(PathAndFileName, list_to_binary(Data)).

create_htacess_dirs(Path)->
    ok = file:make_dir(filename:join([Path,"ht"])),
    ok = file:make_dir(filename:join([Path,"ht/open"])),
    ok = file:make_dir(filename:join([Path,"ht/blocknet"])),
    ok = file:make_dir(filename:join([Path,"ht/secret"])),
    ok = file:make_dir(filename:join([Path,"ht/secret/top_secret"])).

remove_htacess_dirs(Path)->
    file:del_dir(filename:join([Path,"ht/secret/top_secret"])),
    file:del_dir(filename:join([Path,"ht/secret"])),
    file:del_dir(filename:join([Path,"ht/blocknet"])),
    file:del_dir(filename:join([Path,"ht/open"])),
    file:del_dir(filename:join([Path,"ht"])).

format_ip(IpAddress,Pos)when Pos > 0->
    case lists:nth(Pos,IpAddress) of
	$.->
	    case lists:nth(Pos-2,IpAddress) of
		$.->
		   format_ip(IpAddress,Pos-3);
		_->
		    lists:sublist(IpAddress,Pos-2) ++ "."
	    end;
	_ ->
	    format_ip(IpAddress,Pos-1)
    end;

format_ip(IpAddress, _Pos)->
    "1" ++ IpAddress.

remove_htacess(Path)->
    file:delete(filename:join([Path,"ht/open/dummy.html"])),
    file:delete(filename:join([Path,"ht/secret/dummy.html"])),
    file:delete(filename:join([Path,"ht/secret/top_secret/dummy.html"])),
    file:delete(filename:join([Path,"ht/blocknet/dummy.html"])),
    file:delete(filename:join([Path,"ht/blocknet/.htaccess"])),
    file:delete(filename:join([Path,"ht/open/.htaccess"])),
    file:delete(filename:join([Path,"ht/secret/.htaccess"])),
    file:delete(filename:join([Path,"ht/secret/top_secret/.htaccess"])),
    file:delete(filename:join([Path,"ht","users.file"])),
    file:delete(filename:join([Path,"ht","groups.file"])),
    remove_htacess_dirs(Path).


dos_hostname_poll(Type, Host, Port, Node, Hosts) ->
    [dos_hostname_poll1(Type, Host, Port, Node, Host1, Code)
     || {Host1,Code} <- Hosts].

dos_hostname_poll1(Type, Host, Port, Node, Host1, Code) ->
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       dos_hostname_request(Host1),
				       [{statuscode, Code},
					{version, "HTTP/1.0"}]).
        
dos_hostname_request(Host) ->
    "GET / HTTP/1.0\r\n" ++ Host ++ "\r\n\r\n".

get_nof_clients(Mode, Load) ->
    get_nof_clients(test_server:os_type(), Mode, Load).

get_nof_clients(vxworks, _, light)        -> 1;
get_nof_clients(vxworks, ip_comm, medium) -> 3;
get_nof_clients(vxworks, ssl, medium)     -> 3;
get_nof_clients(vxworks, ip_comm, heavy)  -> 5;
get_nof_clients(vxworks, ssl, heavy)      -> 5;
get_nof_clients(_, ip_comm, light)        -> 5;
get_nof_clients(_, ssl, light)            -> 2;
get_nof_clients(_, ip_comm, medium)       -> 10;
get_nof_clients(_, ssl, medium)           -> 4;
get_nof_clients(_, ip_comm, heavy)        -> 20;
get_nof_clients(_, ssl, heavy)            -> 6.

%% Make a file 100 bytes long containing 012...9*10
create_range_data(Path)->
    PathAndFileName=filename:join([Path,"range.txt"]),
    file:write_file(PathAndFileName,list_to_binary(["12345678901234567890",
						   "12345678901234567890",
						   "12345678901234567890",
						   "12345678901234567890",
						   "12345678901234567890"])).

%% create_ipv6_config(Config, FileName, Ipv6Address) ->
%%     ServerRoot = ?config(server_root, Config),
%%     TcTopDir = ?config(tc_top_dir, Config),
%%     Port =  ?config(port, Config),
%%     SockType = ?config(sock_type, Config),
%%
%%     MaxHdrSz     = io_lib:format("~p", [256]),
%%     MaxHdrAct    = io_lib:format("~p", [close]),
%%   
%%     Mod_order = "Modules mod_alias mod_auth mod_esi mod_actions mod_cgi" 
%% 	" mod_include mod_dir mod_get mod_head" 
%% 	" mod_log mod_disk_log mod_trace",
%%	    
%%     HttpConfig = [cline(["BindAddress ", "[" ++ Ipv6Address ++"]|inet6"]),
%% 		  cline(["Port ", integer_to_list(Port)]),
%% 		  cline(["ServerName ", "httpc_test"]),
%% 		  cline(["SocketType ", atom_to_list(SockType)]),
%% 		  cline([Mod_order]),
%% 		  cline(["ServerRoot ", ServerRoot]),
%% 		  cline(["DocumentRoot ",  
%% 			 filename:join(ServerRoot, "htdocs")]),
%% 		  cline(["MaxHeaderSize ",MaxHdrSz]),
%% 		  cline(["MaxHeaderAction ",MaxHdrAct]),
%% 		  cline(["DirectoryIndex ", "index.html "]),
%% 		  cline(["DefaultType ", "text/plain"])],
%%     ConfigFile = filename:join([TcTopDir,FileName]),
%%     {ok, Fd} = file:open(ConfigFile, [write]),
%%     ok = file:write(Fd, lists:flatten(HttpConfig)),
%%     ok = file:close(Fd).
