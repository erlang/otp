%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-module(old_httpd_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("inets_test_lib.hrl").

-include_lib("kernel/include/file.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2,
	 init_per_suite/1, end_per_suite/1]).

%% Core Server tests
-export([
	 ip_mod_alias/1, 
	 ip_mod_actions/1, 
	 ip_mod_security/1, 
	 ip_mod_auth/1,
	 ip_mod_auth_api/1, 
	 ip_mod_auth_mnesia_api/1, 
	 ip_mod_htaccess/1, 
	 ip_mod_cgi/1, 
	 ip_mod_esi/1,
	 ip_mod_get/1, 
	 ip_mod_head/1, 
	 ip_mod_all/1, 
	 ip_load_light/1,
	 ip_load_medium/1, 
	 ip_load_heavy/1, 
	 ip_dos_hostname/1, 
	 ip_time_test/1, 
	 ip_block_disturbing_idle/1, 
	 ip_block_non_disturbing_idle/1, 
	 ip_block_503/1, 
	 ip_block_disturbing_active/1, 
	 ip_block_non_disturbing_active/1, 
	 ip_block_disturbing_active_timeout_not_released/1, 
	 ip_block_disturbing_active_timeout_released/1, 
	 ip_block_non_disturbing_active_timeout_not_released/1, 
	 ip_block_non_disturbing_active_timeout_released/1, 
	 ip_block_disturbing_blocker_dies/1, 
	 ip_block_non_disturbing_blocker_dies/1, 
	 ip_restart_no_block/1, 
	 ip_restart_disturbing_block/1, 
	 ip_restart_non_disturbing_block/1
	]).

-export([
	 essl_mod_alias/1, 
	 essl_mod_actions/1, 
	 essl_mod_security/1, 
	 essl_mod_auth/1, 
	 essl_mod_auth_api/1,  
	 essl_mod_auth_mnesia_api/1, 
	 essl_mod_htaccess/1, 
	 essl_mod_cgi/1,
	 essl_mod_esi/1, 
	 essl_mod_get/1, 
	 essl_mod_head/1, 
	 essl_mod_all/1, 
	 essl_load_light/1, 
	 essl_load_medium/1, 
	 essl_load_heavy/1, 
	 essl_dos_hostname/1, 
	 essl_time_test/1,
	 essl_restart_no_block/1, 
	 essl_restart_disturbing_block/1,
	 essl_restart_non_disturbing_block/1, 
	 essl_block_disturbing_idle/1, 
	 essl_block_non_disturbing_idle/1, 
	 essl_block_503/1, 
	 essl_block_disturbing_active/1, 
	 essl_block_non_disturbing_active/1, 
	 essl_block_disturbing_active_timeout_not_released/1, 
	 essl_block_disturbing_active_timeout_released/1, 
	 essl_block_non_disturbing_active_timeout_not_released/1, 
	 essl_block_non_disturbing_active_timeout_released/1, 
	 essl_block_disturbing_blocker_dies/1, 
	 essl_block_non_disturbing_blocker_dies/1
	]).

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

%%% IPv6 tests
-export([ipv6_hostname_ipcomm/0, ipv6_hostname_ipcomm/1, 
	 ipv6_address_ipcomm/0,  ipv6_address_ipcomm/1, 
	 ipv6_hostname_essl/0,   ipv6_hostname_essl/1,   
	 ipv6_address_essl/0,    ipv6_address_essl/1]).

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

-record(httpd_user,  {user_name, password, user_data}).
-record(httpd_group, {group_name, userlist}).


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
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [
     {group, ip}, 
     {group, ssl}, 
     %%{group, http_1_1_ip},
     %%{group, http_1_0_ip}, 
     %%{group, http_0_9_ip},
     %%{group, ipv6}, 
     {group, tickets}
    ].

groups() -> 
    [
     {ip, [],
      [
       %%ip_mod_alias, 
       ip_mod_actions, 
       %%ip_mod_security,
       %% ip_mod_auth, 
       %% ip_mod_auth_api, 
       ip_mod_auth_mnesia_api,
       %%ip_mod_htaccess, 
       %%ip_mod_cgi, 
       %%ip_mod_esi, 
       %%ip_mod_get,
       %%ip_mod_head, 
       %%ip_mod_all, 
       %% ip_load_light, 
       %% ip_load_medium,
       %% ip_load_heavy, 
       %%ip_dos_hostname, 
       ip_time_test,
       %% Only used through load_config
       %% but we still need these tests
       %% should be cleaned up and moved to new test suite 
       %%ip_restart_no_block, 
       %%ip_restart_disturbing_block,
       %%ip_restart_non_disturbing_block,
       %% Tested in inets_SUITE
       %%ip_block_disturbing_idle, 
       %%ip_block_non_disturbing_idle,
       ip_block_503
       %% Tested in new httpd_SUITE
       %%ip_block_disturbing_active,
       %%ip_block_non_disturbing_active,
       %%ip_block_disturbing_blocker_dies,
       %%ip_block_non_disturbing_blocker_dies
       %% No longer relevant
       %%ip_block_disturbing_active_timeout_not_released,
       %%ip_block_disturbing_active_timeout_released,
       %%ip_block_non_disturbing_active_timeout_not_released,
       %%ip_block_non_disturbing_active_timeout_released,
      ]},
     {ssl, [], [{group, essl}]},
     {essl, [],
      [
       %%essl_mod_alias, 
       essl_mod_actions, 
       %% essl_mod_security,
       %% essl_mod_auth, 
       %% essl_mod_auth_api,
       essl_mod_auth_mnesia_api, 
       %%essl_mod_htaccess,
       %%essl_mod_cgi, 
       %%essl_mod_esi, 
       %%essl_mod_get, 
       %%essl_mod_head,
       %% essl_mod_all, 
       %% essl_load_light, 
       %% essl_load_medium,
       %% essl_load_heavy, 
       %%essl_dos_hostname, 
       essl_time_test
       %% Replaced by load_config
       %% essl_restart_no_block, 
       %% essl_restart_disturbing_block,
       %% essl_restart_non_disturbing_block,
       %% essl_block_disturbing_idle,
       %% essl_block_non_disturbing_idle, essl_block_503,
       %% essl_block_disturbing_active,
       %% essl_block_non_disturbing_active,
       %% essl_block_disturbing_active_timeout_not_released,
       %% essl_block_disturbing_active_timeout_released,
       %% essl_block_non_disturbing_active_timeout_not_released,
       %% essl_block_non_disturbing_active_timeout_released,
       %% essl_block_disturbing_blocker_dies,
       %%  essl_block_non_disturbing_blocker_dies
      ]},
     %% {http_1_1_ip, [],
     %%  [
     %%   %%ip_host, ip_chunked, ip_expect, 
     %%   %%ip_range, 
     %%   %%ip_if_test
     %%   %%ip_http_trace, ip_http1_1_head,
     %%   %%ip_mod_cgi_chunked_encoding_test
     %%  ]},
     %%{http_1_0_ip, [],
      %%[ip_head_1_0, ip_get_1_0, ip_post_1_0]},
     %%{http_0_9_ip, [], [ip_get_0_9]},
     %% {ipv6, [], [ipv6_hostname_ipcomm, ipv6_address_ipcomm, 
     %% 		 ipv6_hostname_essl,   ipv6_address_essl]},
     {tickets, [],
      [%%ticket_5775, ticket_5865, 
       ticket_5913%%, ticket_6003,
       %%ticket_7304
      ]}].

init_per_group(ipv6 = _GroupName, Config) ->
    case inets_test_lib:has_ipv6_support() of
	{ok, _} ->
	    Config;
	_ ->
	    {skip, "Host does not support IPv6"}
    end;
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
    io:format(user, "init_per_suite -> entry with"
	      "~n   Config: ~p"
	      "~n", [Config]),

    PrivDir = proplists:get_value(priv_dir, Config),
    SuiteTopDir = filename:join(PrivDir, ?MODULE),
    case file:make_dir(SuiteTopDir) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        Error ->
            throw({error, {failed_creating_suite_top_dir, Error}})
    end,

    [{has_ipv6_support, inets_test_lib:has_ipv6_support()}, 
     {suite_top_dir,    SuiteTopDir},
     {node,             node()},
     {host,             inets_test_lib:hostname()},
     {address,          getaddr()} | Config].


%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------

end_per_suite(_Config) ->
    %% SuiteTopDir = proplists:get_value(suite_top_dir, Config), 
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

    %% tsp("init_per_testcase2 -> entry with"
    %% 	"~n   Config: ~p", [Config]),
    
    IpNormal    = integer_to_list(?IP_PORT)    ++ ".conf",
    IpHtaccess  = integer_to_list(?IP_PORT)   ++ "htaccess.conf",
    SslNormal   = integer_to_list(?SSL_PORT)  ++ ".conf",
    SslHtaccess = integer_to_list(?SSL_PORT) ++ "htaccess.conf",

    DataDir     = proplists:get_value(data_dir, Config),
    SuiteTopDir = proplists:get_value(suite_top_dir, Config),

    %% tsp("init_per_testcase2 -> "
    %% 	"~n   SuiteDir: ~p"
    %% 	"~n   DataDir:  ~p", [SuiteTopDir, DataDir]),
    
    TcTopDir = filename:join(SuiteTopDir, Case),
    ?line ok = file:make_dir(TcTopDir),

    %% tsp("init_per_testcase2 -> "
    %% 	"~n   TcTopDir: ~p", [TcTopDir]),

    DataSrc    = filename:join([DataDir, "server_root"]),
    ServerRoot = filename:join([TcTopDir, "server_root"]),
    
    %% tsp("init_per_testcase2 -> "
    %% 	"~n   DataSrc:    ~p"
    %% 	"~n   ServerRoot: ~p", [DataSrc, ServerRoot]),

    ok = file:make_dir(ServerRoot),
    ok = file:make_dir(filename:join([TcTopDir, "logs"])),

    NewConfig = [{tc_top_dir, TcTopDir}, {server_root, ServerRoot} | Config],

    %% tsp("init_per_testcase2 -> copy DataSrc to ServerRoot"),

    inets_test_lib:copy_dirs(DataSrc, ServerRoot),

    %% tsp("init_per_testcase2 -> fix cgi"),
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
    %% tsp("init_per_testcase2 -> ip testcase setups"),
    create_config([{port, ?IP_PORT}, {sock_type, ip_comm} | NewConfig], 
		  normal_access, IpNormal), 
    create_config([{port, ?IP_PORT}, {sock_type, ip_comm} | NewConfig], 
    		  mod_htaccess, IpHtaccess), 

    %% To be used by SSL test cases
    %% tsp("init_per_testcase2 -> ssl testcase setups"),
    SocketType = 
	case atom_to_list(Case) of
	    [X, $s, $s, $l | _] ->
		case X of
		    $p -> ssl;
		    $e -> essl
		end;
	    _ ->
		ssl
	end,

    create_config([{port, ?SSL_PORT}, {sock_type, SocketType} | NewConfig], 
		  normal_access, SslNormal),
    create_config([{port, ?SSL_PORT}, {sock_type, SocketType} | NewConfig],
    		  mod_htaccess, SslHtaccess),  
  
    %% To be used by IPv6 test cases. Case-clause is so that
    %% you can do ts:run(inets, httpd_SUITE, <test case>)
    %% for all cases except the ipv6 cases as they depend
    %% on  'test_host_ipv6_only' that will only be present
    %% when you run the whole test suite due  to shortcomings
    %% of the test server.

    tsp("init_per_testcase2 -> maybe generate IPv6 config file(s)"),
    NewConfig2 = 
	case atom_to_list(Case) of
	    "ipv6_" ++ _ ->
		case (catch inets_test_lib:has_ipv6_support(NewConfig)) of
		    {ok, IPv6Address0} ->
			{ok, Hostname} = inet:gethostname(), 
			IPv6Address = http_transport:ipv6_name(IPv6Address0), 
			create_ipv6_config([{port, ?IP_PORT}, 
					    {sock_type, ip_comm},
					    {ipv6_host, IPv6Address} | 
					    NewConfig],
					   "ipv6_hostname_ipcomm.conf", 
					   Hostname),
			create_ipv6_config([{port, ?IP_PORT}, 
					    {sock_type, ip_comm},
					    {ipv6_host, IPv6Address} | 
					    NewConfig],
					   "ipv6_address_ipcomm.conf",  
					   IPv6Address),
			create_ipv6_config([{port, ?SSL_PORT}, 
					    {sock_type, essl},
					    {ipv6_host, IPv6Address} | 
					    NewConfig],
					   "ipv6_hostname_essl.conf", 
					   Hostname),
			create_ipv6_config([{port, ?SSL_PORT}, 
					    {sock_type, essl},
					    {ipv6_host, IPv6Address} | 
					    NewConfig],
					   "ipv6_address_essl.conf",  
					   IPv6Address),
			[{ipv6_host, IPv6Address} | NewConfig];
		    _ ->
			NewConfig
		end;

	    _ ->
		NewConfig
	end,

    %% tsp("init_per_testcase2 -> done when"
    %% 	"~n   NewConfig2: ~p", [NewConfig2]),

    NewConfig2.


init_per_testcase3(Case, Config) ->
    tsp("init_per_testcase3(~w) -> entry with"
	"~n   Config: ~p", [Case, Config]),

    
%%     %% Create a new fresh node to be used by the server in this test-case
    
%%     NodeName = list_to_atom(atom_to_list(Case) ++ "_httpd"), 
%%     Node     = inets_test_lib:start_node(NodeName),
    
    %% Clean up (we do not want this clean up in end_per_testcase
    %% if init_per_testcase crashes for some testcase it will
    %% have contaminated the environment and there will be no clean up.)
    %% This init can take a few different paths so that one crashes
    %% does not mean that all invocations will.

    application:unset_env(inets, services),
    application:stop(inets),
    application:stop(ssl),
    cleanup_mnesia(),
 
    %% Start initialization
    tsp("init_per_testcase3(~w) -> start init", [Case]),
 
    Dog = test_server:timetrap(inets_test_lib:minutes(10)),
    NewConfig = lists:keydelete(watchdog, 1, Config),
    TcTopDir = proplists:get_value(tc_top_dir, Config),
      
    CaseRest = 
	case atom_to_list(Case) of
	    "ip_mod_htaccess" ->
		inets_test_lib:start_http_server(
		  filename:join(TcTopDir,
				integer_to_list(?IP_PORT) ++
				"htaccess.conf")),
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

	    [X, $s, $s, $l, $_, $m, $o, $d, $_, $h, $t, $a, $c, $c, $e, $s, $s] ->
		?ENSURE_STARTED([crypto, public_key, ssl]),		
		SslTag = 
		    case X of
			$p -> ssl;  % Plain
			$e -> essl  % Erlang based ssl
		    end,
		case inets_test_lib:start_http_server_ssl(
		       filename:join(TcTopDir,
				     integer_to_list(?SSL_PORT) ++ 
				     "htaccess.conf"), SslTag) of
		    ok ->
			"mod_htaccess";
		    Other ->
			error_logger:info_msg("Other: ~p~n", [Other]),
			{skip, "SSL does not seem to be supported"}
		end;
	    [X, $s, $s, $l, $_ | Rest] ->
		?ENSURE_STARTED([crypto, public_key, ssl]),		
		SslTag = 
		    case X of
			$p -> ssl;
			$e -> essl
		    end,
		case inets_test_lib:start_http_server_ssl(
		       filename:join(TcTopDir,
				     integer_to_list(?SSL_PORT) ++ 
				     ".conf"), SslTag) of
		    ok ->
			Rest;
		    Other ->
			error_logger:info_msg("Other: ~p~n", [Other]),
			{skip, "SSL does not seem to be supported"}
		end;
	    "ipv6_" ++ _  = TestCaseStr ->
		case inets_test_lib:has_ipv6_support() of
		    {ok, _} ->
			inets_test_lib:start_http_server(
			  filename:join(TcTopDir,
					TestCaseStr ++ ".conf"));
		    
		    _ ->
			{skip, "Host does not support IPv6"}
		end
	end,

    InitRes = 
	case CaseRest of
	    {skip, _} = Skip ->
		Skip;
	    "mod_auth_" ++ _ ->
		start_mnesia(proplists:get_value(node, Config)),
		[{watchdog, Dog} | NewConfig];
	    "mod_htaccess" ->
		ServerRoot = proplists:get_value(server_root, Config), 
		Path = filename:join([ServerRoot, "htdocs"]),
		catch remove_htaccess(Path),
		create_htaccess_data(Path, proplists:get_value(address, Config)),
		[{watchdog, Dog} | NewConfig];
	    "range" ->
		ServerRoot = proplists:get_value(server_root, Config), 
		Path = filename:join([ServerRoot, "htdocs"]),
		create_range_data(Path),
		[{watchdog, Dog} | NewConfig];
	    _ ->
		[{watchdog, Dog} | NewConfig]
	end,
    
    tsp("init_per_testcase3(~w) -> done when"
	"~n   InitRes: ~p", [Case, InitRes]),

    InitRes.


%%--------------------------------------------------------------------
%% Function: end_per_testcase(Case, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(Case, Config) ->
    Dog = proplists:get_value(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    end_per_testcase2(Case, lists:keydelete(watchdog, 1, Config)),
    ok.

end_per_testcase2(Case, Config) ->
    tsp("end_per_testcase2(~w) -> entry with"
	"~n   Config: ~p", [Case, Config]),
    application:unset_env(inets, services),
    application:stop(inets),
    application:stop(ssl),     
    application:stop(crypto), % used by the new ssl (essl test cases)  
    cleanup_mnesia(),
    tsp("end_per_testcase2(~w) -> done", [Case]),
    ok.


%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------

%%-------------------------------------------------------------------------
ip_mod_alias(doc) -> 
    ["Module test: mod_alias"];
ip_mod_alias(suite) -> 
    [];
ip_mod_alias(Config) when is_list(Config) ->
    httpd_mod:alias(ip_comm, ?IP_PORT, 
		    proplists:get_value(host, Config), proplists:get_value(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ip_mod_actions(doc) -> 
    ["Module test: mod_actions"];
ip_mod_actions(suite) -> 
    [];
ip_mod_actions(Config) when is_list(Config) ->
    httpd_mod:actions(ip_comm, ?IP_PORT, 
		      proplists:get_value(host, Config), proplists:get_value(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ip_mod_security(doc) -> 
    ["Module test: mod_security"];
ip_mod_security(suite) -> 
    [];
ip_mod_security(Config) when is_list(Config) ->
    ServerRoot = proplists:get_value(server_root, Config), 
    httpd_mod:security(ServerRoot, ip_comm, ?IP_PORT, 
		       proplists:get_value(host, Config), proplists:get_value(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ip_mod_auth(doc) -> 
    ["Module test: mod_auth"];
ip_mod_auth(suite) -> 
    [];
ip_mod_auth(Config) when is_list(Config) ->
    httpd_mod:auth(ip_comm, ?IP_PORT, 
		   proplists:get_value(host, Config), proplists:get_value(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ip_mod_auth_api(doc) -> 
    ["Module test: mod_auth_api"];
ip_mod_auth_api(suite) -> 
    [];
ip_mod_auth_api(Config) when is_list(Config) ->
    ServerRoot = proplists:get_value(server_root, Config), 
    Host =  proplists:get_value(host, Config),
    Node = proplists:get_value(node, Config),
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
		   proplists:get_value(host, Config), proplists:get_value(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_mod_htaccess(doc) -> 
    ["Module test: mod_htaccess"];
ip_mod_htaccess(suite) -> 
    [];
ip_mod_htaccess(Config) when is_list(Config) ->
    httpd_mod:htaccess(ip_comm, ?IP_PORT, 
		       proplists:get_value(host, Config), proplists:get_value(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_mod_cgi(doc) ->
    ["Module test: mod_cgi"];
ip_mod_cgi(suite) ->
    [];
ip_mod_cgi(Config) when is_list(Config) ->
    httpd_mod:cgi(ip_comm, ?IP_PORT, 
	proplists:get_value(host, Config), proplists:get_value(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_mod_esi(doc) ->
    ["Module test: mod_esi"];
ip_mod_esi(suite) ->
    [];
ip_mod_esi(Config) when is_list(Config) ->
    httpd_mod:esi(ip_comm, ?IP_PORT, 
		  proplists:get_value(host, Config), proplists:get_value(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ip_mod_get(doc) ->
    ["Module test: mod_get"];
ip_mod_get(suite) ->
    [];
ip_mod_get(Config) when is_list(Config) ->
    httpd_mod:get(ip_comm, ?IP_PORT, 
		  proplists:get_value(host, Config), proplists:get_value(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ip_mod_head(doc) ->
    ["Module test: mod_head"];
ip_mod_head(suite) ->
    [];
ip_mod_head(Config) when is_list(Config) ->
    httpd_mod:head(ip_comm, ?IP_PORT, 
		   proplists:get_value(host, Config), proplists:get_value(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_mod_all(doc) ->
    ["All modules test"];
ip_mod_all(suite) ->
    [];
ip_mod_all(Config) when is_list(Config) ->
    httpd_mod:all(ip_comm, ?IP_PORT, 
		  proplists:get_value(host, Config), proplists:get_value(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_load_light(doc) ->
    ["Test light load"];
ip_load_light(suite) ->
    [];
ip_load_light(Config) when is_list(Config) ->
    httpd_load:load_test(ip_comm, ?IP_PORT, proplists:get_value(host, Config), 
			 proplists:get_value(node, Config),
			 get_nof_clients(ip_comm, light)),
    ok.
%%-------------------------------------------------------------------------
ip_load_medium(doc) ->
    ["Test  medium load"];
ip_load_medium(suite) ->
    [];
ip_load_medium(Config) when is_list(Config) ->
      httpd_load:load_test(ip_comm, ?IP_PORT, proplists:get_value(host, Config),
			   proplists:get_value(node, Config),
			   get_nof_clients(ip_comm, medium)),
    ok.
%%-------------------------------------------------------------------------
ip_load_heavy(doc) ->
    ["Test heavy load"];
ip_load_heavy(suite) ->
    [];
ip_load_heavy(Config) when is_list(Config) ->
     httpd_load:load_test(ip_comm, ?IP_PORT, proplists:get_value(host, Config),
			  proplists:get_value(node, Config),
			  get_nof_clients(ip_comm, heavy)),
    ok.


%%-------------------------------------------------------------------------
ip_dos_hostname(doc) ->
    ["Denial Of Service (DOS) attack test case"];
ip_dos_hostname(suite) ->
    [];
ip_dos_hostname(Config) when is_list(Config) ->
    dos_hostname(ip_comm, ?IP_PORT, proplists:get_value(host, Config), 
		 proplists:get_value(node, Config), ?MAX_HEADER_SIZE),
    ok.


%%-------------------------------------------------------------------------
ip_time_test(doc) ->
    [""];
ip_time_test(suite) ->
    [];
ip_time_test(Config) when is_list(Config) ->
    httpd_time_test:t(ip_comm, proplists:get_value(host, Config), ?IP_PORT),
    ok.

%%-------------------------------------------------------------------------
ip_block_503(doc) ->
    ["Check that you will receive status code 503 when the server"
     " is blocked and 200 when its not blocked."];
ip_block_503(suite) ->
    [];
ip_block_503(Config) when is_list(Config) ->
    httpd_block:block_503(ip_comm, ?IP_PORT, proplists:get_value(host, Config), 
				 proplists:get_value(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_block_disturbing_idle(doc) ->
    ["Check that you can block/unblock an idle server. The strategy " 
     "distribing does not really make a difference in this case."];
ip_block_disturbing_idle(suite) ->
    [];
ip_block_disturbing_idle(Config) when is_list(Config) ->
    httpd_block:block_disturbing_idle(ip_comm, ?IP_PORT, 
				      proplists:get_value(host, Config), 
				      proplists:get_value(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_block_non_disturbing_idle(doc) ->
    ["Check that you can block/unblock an idle server. The strategy " 
     "non distribing does not really make a difference in this case."];
ip_block_non_disturbing_idle(suite) ->
    [];
ip_block_non_disturbing_idle(Config) when is_list(Config) ->
    httpd_block:block_non_disturbing_idle(ip_comm, ?IP_PORT, 
					  proplists:get_value(host, Config), 
					  proplists:get_value(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_block_disturbing_active(doc) ->
    ["Check that you can block/unblock an active server. The strategy " 
     "distribing means ongoing requests should be terminated."];
ip_block_disturbing_active(suite) ->
    [];
ip_block_disturbing_active(Config) when is_list(Config) ->
    httpd_block:block_disturbing_active(ip_comm, ?IP_PORT, 
					proplists:get_value(host, Config), 
					proplists:get_value(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_block_non_disturbing_active(doc) ->
    ["Check that you can block/unblock an idle server. The strategy " 
     "non distribing means the ongoing requests should be compleated."];
ip_block_non_disturbing_active(suite) ->
    [];
ip_block_non_disturbing_active(Config) when is_list(Config) ->
    httpd_block:block_non_disturbing_idle(ip_comm, ?IP_PORT, 
					  proplists:get_value(host, Config), 
					  proplists:get_value(node, Config)),
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
							     proplists:get_value(host,
								     Config), 
							     proplists:get_value(node, 
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
							 proplists:get_value(host,
								 Config), 
							 proplists:get_value(node, 
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
							 proplists:get_value(host, 
								 Config), 
							 proplists:get_value(node, 
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
						     proplists:get_value(host, 
							     Config), 
						     proplists:get_value(node, 
							     Config)),
    ok.
%%-------------------------------------------------------------------------
ip_block_disturbing_blocker_dies(doc) ->
    [];
ip_block_disturbing_blocker_dies(suite) ->
    [];
ip_block_disturbing_blocker_dies(Config) when is_list(Config) ->
    httpd_block:disturbing_blocker_dies(ip_comm, ?IP_PORT, 
					proplists:get_value(host, Config), 
					proplists:get_value(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_block_non_disturbing_blocker_dies(doc) ->
    [];
ip_block_non_disturbing_blocker_dies(suite) ->
    [];
ip_block_non_disturbing_blocker_dies(Config) when is_list(Config) ->
    httpd_block:non_disturbing_blocker_dies(ip_comm, ?IP_PORT, 
					    proplists:get_value(host, Config), 
					    proplists:get_value(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_restart_no_block(doc) ->
    [""];
ip_restart_no_block(suite) ->
    [];
ip_restart_no_block(Config) when is_list(Config) ->
    httpd_block:restart_no_block(ip_comm, ?IP_PORT, proplists:get_value(host, Config), 
				 proplists:get_value(node, Config)),
    ok.
%%-------------------------------------------------------------------------
ip_restart_disturbing_block(doc) ->
    [""];
ip_restart_disturbing_block(suite) ->
    [];
ip_restart_disturbing_block(Config) when is_list(Config) ->
    httpd_block:restart_disturbing_block(ip_comm, ?IP_PORT, 
					 proplists:get_value(host, Config),
					 proplists:get_value(node, Config)),
    ok.

%%-------------------------------------------------------------------------
ip_restart_non_disturbing_block(doc) ->
    [""];
ip_restart_non_disturbing_block(suite) ->
    [];
ip_restart_non_disturbing_block(Config) when is_list(Config) ->
    httpd_block:restart_non_disturbing_block(ip_comm, ?IP_PORT,
					    proplists:get_value(host, Config), 
					    proplists:get_value(node, Config)),
    ok.

%%-------------------------------------------------------------------------

essl_mod_alias(doc) -> 
    ["Module test: mod_alias - using new of configure new SSL"];
essl_mod_alias(suite) -> 
    [];
essl_mod_alias(Config) when is_list(Config) ->
    ssl_mod_alias(essl, Config).


ssl_mod_alias(Tag, Config) ->
    httpd_mod:alias(Tag, ?SSL_PORT, 
		    proplists:get_value(host, Config), proplists:get_value(node, Config)),
    ok. 


%%-------------------------------------------------------------------------

essl_mod_actions(doc) -> 
    ["Module test: mod_actions - using new of configure new SSL"];
essl_mod_actions(suite) -> 
    [];
essl_mod_actions(Config) when is_list(Config) ->
    ssl_mod_actions(essl, Config).


ssl_mod_actions(Tag, Config) ->
    httpd_mod:actions(Tag, 
		      ?SSL_PORT, 
		      proplists:get_value(host, Config), 
		      proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------

essl_mod_security(doc) -> 
    ["Module test: mod_security - using new of configure new SSL"];
essl_mod_security(suite) -> 
    [];
essl_mod_security(Config) when is_list(Config) ->
    ssl_mod_security(essl, Config).

ssl_mod_security(Tag, Config) ->
    ServerRoot = proplists:get_value(server_root, Config), 
    httpd_mod:security(ServerRoot, 
		       Tag, 
		       ?SSL_PORT, 
		       proplists:get_value(host, Config), 
		       proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------

essl_mod_auth(doc) -> 
    ["Module test: mod_auth - using new of configure new SSL"];
essl_mod_auth(suite) -> 
    [];
essl_mod_auth(Config) when is_list(Config) ->
    ssl_mod_auth(essl, Config).

ssl_mod_auth(Tag, Config) ->
    httpd_mod:auth(Tag, 
		   ?SSL_PORT, 
		   proplists:get_value(host, Config), 
		   proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------


essl_mod_auth_api(doc) -> 
    ["Module test: mod_auth - using new of configure new SSL"];
essl_mod_auth_api(suite) -> 
    [];
essl_mod_auth_api(Config) when is_list(Config) ->
    ssl_mod_auth_api(essl, Config).

ssl_mod_auth_api(Tag, Config) ->
    ServerRoot = proplists:get_value(server_root, Config), 
    Host       =  proplists:get_value(host, Config),
    Node       = proplists:get_value(node, Config),
    httpd_mod:auth_api(ServerRoot, "",        Tag, ?SSL_PORT, Host, Node),
    httpd_mod:auth_api(ServerRoot, "dets_",   Tag, ?SSL_PORT, Host, Node),
    httpd_mod:auth_api(ServerRoot, "mnesia_", Tag, ?SSL_PORT, Host, Node),
    ok. 


%%-------------------------------------------------------------------------


essl_mod_auth_mnesia_api(doc) -> 
    ["Module test: mod_auth_mnesia_api - using new of configure new SSL"];
essl_mod_auth_mnesia_api(suite) -> 
    [];
essl_mod_auth_mnesia_api(Config) when is_list(Config) ->
    ssl_mod_auth_mnesia_api(essl, Config).

ssl_mod_auth_mnesia_api(Tag, Config) ->
    httpd_mod:auth_mnesia_api(Tag, 
			      ?SSL_PORT, 
			      proplists:get_value(host, Config), 
			      proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------

essl_mod_htaccess(doc) -> 
    ["Module test: mod_htaccess - using new of configure new SSL"];
essl_mod_htaccess(suite) -> 
    [];
essl_mod_htaccess(Config) when is_list(Config) ->
    ssl_mod_htaccess(essl, Config).

ssl_mod_htaccess(Tag, Config) ->
    httpd_mod:htaccess(Tag, 
		       ?SSL_PORT, 
		       proplists:get_value(host, Config), 
		       proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------

essl_mod_cgi(doc) ->
    ["Module test: mod_cgi - using new of configure new SSL"];
essl_mod_cgi(suite) ->
    [];
essl_mod_cgi(Config) when is_list(Config) ->
    ssl_mod_cgi(essl, Config).

ssl_mod_cgi(Tag, Config) ->
    httpd_mod:cgi(Tag, 
	?SSL_PORT, 
	proplists:get_value(host, Config), 
	proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------

essl_mod_esi(doc) ->
    ["Module test: mod_esi - using new of configure new SSL"];
essl_mod_esi(suite) ->
    [];
essl_mod_esi(Config) when is_list(Config) ->
    ssl_mod_esi(essl, Config).

ssl_mod_esi(Tag, Config) ->
    httpd_mod:esi(Tag, 
		  ?SSL_PORT, 
		  proplists:get_value(host, Config), 
		  proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------

essl_mod_get(doc) ->
    ["Module test: mod_get - using new of configure new SSL"];
essl_mod_get(suite) ->
    [];
essl_mod_get(Config) when is_list(Config) ->
    ssl_mod_get(essl, Config).

ssl_mod_get(Tag, Config) ->
    httpd_mod:get(Tag, 
		  ?SSL_PORT, 
		  proplists:get_value(host, Config), 
		  proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------

essl_mod_head(doc) ->
    ["Module test: mod_head - using new of configure new SSL"];
essl_mod_head(suite) ->
    [];
essl_mod_head(Config) when is_list(Config) ->
    ssl_mod_head(essl, Config).

ssl_mod_head(Tag, Config) ->
    httpd_mod:head(Tag, 
		   ?SSL_PORT, 
		   proplists:get_value(host, Config), 
		   proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------

essl_mod_all(doc) ->
    ["All modules test - using new of configure new SSL"];
essl_mod_all(suite) ->
    [];
essl_mod_all(Config) when is_list(Config) ->
    ssl_mod_all(essl, Config).

ssl_mod_all(Tag, Config) ->
    httpd_mod:all(Tag, 
		  ?SSL_PORT, 
		  proplists:get_value(host, Config), 
		  proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------

essl_load_light(doc) ->
    ["Test light load - using new of configure new SSL"];
essl_load_light(suite) ->
    [];
essl_load_light(Config) when is_list(Config) ->
    ssl_load_light(essl, Config).

ssl_load_light(Tag, Config) ->
    httpd_load:load_test(Tag, 
			 ?SSL_PORT, 
			 proplists:get_value(host, Config), 
			 proplists:get_value(node, Config),
			 get_nof_clients(ssl, light)),
    ok.


%%-------------------------------------------------------------------------

essl_load_medium(doc) ->
    ["Test medium load - using new of configure new SSL"];
essl_load_medium(suite) ->
    [];
essl_load_medium(Config) when is_list(Config) ->
    ssl_load_medium(essl, Config).

ssl_load_medium(Tag, Config) ->
    httpd_load:load_test(Tag, 
			 ?SSL_PORT, 
			 proplists:get_value(host, Config), 
			 proplists:get_value(node, Config),
			 get_nof_clients(ssl, medium)),
    ok.


%%-------------------------------------------------------------------------

essl_load_heavy(doc) ->
    ["Test heavy load - using new of configure new SSL"];
essl_load_heavy(suite) ->
    [];
essl_load_heavy(Config) when is_list(Config) ->
    ssl_load_heavy(essl, Config).

ssl_load_heavy(Tag, Config) ->
    httpd_load:load_test(Tag, 
			 ?SSL_PORT, 
			 proplists:get_value(host, Config), 
			 proplists:get_value(node, Config),
			 get_nof_clients(ssl, heavy)),
    ok.


%%-------------------------------------------------------------------------


essl_dos_hostname(doc) ->
    ["Denial Of Service (DOS) attack test case - using new of configure new SSL"];
essl_dos_hostname(suite) ->
    [];
essl_dos_hostname(Config) when is_list(Config) ->
    ssl_dos_hostname(essl, Config).

ssl_dos_hostname(Tag, Config) ->
    dos_hostname(Tag, 
		 ?SSL_PORT, 
		 proplists:get_value(host, Config), 
		 proplists:get_value(node, Config), 
		 ?MAX_HEADER_SIZE),
    ok.


%%-------------------------------------------------------------------------


essl_time_test(doc) ->
    ["using new of configure new SSL"];
essl_time_test(suite) ->
    [];
essl_time_test(Config) when is_list(Config) ->
    ssl_time_test(essl, Config).

ssl_time_test(Tag, Config) when is_list(Config) ->
    httpd_time_test:t(Tag, 
		      proplists:get_value(host, Config), 
		      ?SSL_PORT),
    ok.


%%-------------------------------------------------------------------------


essl_block_503(doc) ->
    ["Check that you will receive status code 503 when the server"
     " is blocked and 200 when its not blocked - using new of configure new SSL."];
essl_block_503(suite) ->
    [];
essl_block_503(Config) when is_list(Config) ->
    ssl_block_503(essl, Config).

ssl_block_503(Tag, Config) ->
    httpd_block:block_503(Tag, 
			  ?SSL_PORT, 
			  proplists:get_value(host, Config), 
			  proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------

essl_block_disturbing_idle(doc) ->
    ["Check that you can block/unblock an idle server. The strategy " 
     "distribing does not really make a difference in this case." 
     "Using new of configure new SSL"];
essl_block_disturbing_idle(suite) ->
    [];
essl_block_disturbing_idle(Config) when is_list(Config) ->
    ssl_block_disturbing_idle(essl, Config).

ssl_block_disturbing_idle(Tag, Config) ->
    httpd_block:block_disturbing_idle(Tag, 
				      ?SSL_PORT, 
				      proplists:get_value(host, Config), 
				      proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------

essl_block_non_disturbing_idle(doc) ->
    ["Check that you can block/unblock an idle server. The strategy " 
     "non distribing does not really make a difference in this case." 
     "Using new of configure new SSL"];
essl_block_non_disturbing_idle(suite) ->
    [];
essl_block_non_disturbing_idle(Config) when is_list(Config) ->
    ssl_block_non_disturbing_idle(essl, Config).

ssl_block_non_disturbing_idle(Tag, Config) ->
    httpd_block:block_non_disturbing_idle(Tag, 
					  ?SSL_PORT, 
					  proplists:get_value(host, Config), 
					  proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------

essl_block_disturbing_active(doc) ->
    ["Check that you can block/unblock an active server. The strategy " 
     "distribing means ongoing requests should be terminated." 
     "Using new of configure new SSL"];
essl_block_disturbing_active(suite) ->
    [];
essl_block_disturbing_active(Config) when is_list(Config) ->
    ssl_block_disturbing_active(essl, Config).

ssl_block_disturbing_active(Tag, Config) ->
    httpd_block:block_disturbing_active(Tag, 
					?SSL_PORT, 
					proplists:get_value(host, Config), 
					proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------

essl_block_non_disturbing_active(doc) ->
    ["Check that you can block/unblock an idle server. The strategy " 
     "non distribing means the ongoing requests should be compleated." 
     "Using new of configure new SSL"];
essl_block_non_disturbing_active(suite) ->
    [];
essl_block_non_disturbing_active(Config) when is_list(Config) ->
    ssl_block_non_disturbing_active(essl, Config).

ssl_block_non_disturbing_active(Tag, Config) ->
    httpd_block:block_non_disturbing_idle(Tag, 
					  ?SSL_PORT, 
					  proplists:get_value(host, Config), 
					  proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------

essl_block_disturbing_active_timeout_not_released(doc) ->
    ["Check that you can block an active server. The strategy " 
     "distribing means ongoing requests should be compleated"
     "if the timeout does not occur." 
    "Using new of configure new SSL"];
essl_block_disturbing_active_timeout_not_released(suite) ->
    [];
essl_block_disturbing_active_timeout_not_released(Config) 
  when is_list(Config) ->
    ssl_block_disturbing_active_timeout_not_released(essl, Config).

ssl_block_disturbing_active_timeout_not_released(Tag, Config) ->
    Port = ?SSL_PORT, 
    Host = proplists:get_value(host, Config), 
    Node = proplists:get_value(node, Config), 
    httpd_block:block_disturbing_active_timeout_not_released(Tag, 
							     Port, Host, Node),
    ok.


%%-------------------------------------------------------------------------

essl_block_disturbing_active_timeout_released(doc) ->
    ["Check that you can block an active server. The strategy " 
     "distribing means ongoing requests should be terminated when"
     "the timeout occurs." 
    "Using new of configure new SSL"];
essl_block_disturbing_active_timeout_released(suite) ->
    [];
essl_block_disturbing_active_timeout_released(Config) 
  when is_list(Config) ->
    ssl_block_disturbing_active_timeout_released(essl, Config).

ssl_block_disturbing_active_timeout_released(Tag, Config) ->
    Port = ?SSL_PORT, 
    Host = proplists:get_value(host, Config), 
    Node = proplists:get_value(node, Config),     
    httpd_block:block_disturbing_active_timeout_released(Tag, 
							 Port, 
							 Host, 
							 Node), 
    ok.


%%-------------------------------------------------------------------------

essl_block_non_disturbing_active_timeout_not_released(doc) ->
    ["Check that you can block an active server. The strategy " 
     "non non distribing means ongoing requests should be completed." 
    "Using new of configure new SSL"];
essl_block_non_disturbing_active_timeout_not_released(suite) ->
    [];
essl_block_non_disturbing_active_timeout_not_released(Config)
  when is_list(Config) ->
    ssl_block_non_disturbing_active_timeout_not_released(essl, Config).

ssl_block_non_disturbing_active_timeout_not_released(Tag, Config) ->
    Port = ?SSL_PORT, 
    Host = proplists:get_value(host, Config), 
    Node = proplists:get_value(node, Config), 
    httpd_block:block_non_disturbing_active_timeout_not_released(Tag,
								 Port, 
								 Host, 
								 Node),
    ok.


%%-------------------------------------------------------------------------


essl_block_non_disturbing_active_timeout_released(doc) ->
    ["Check that you can block an active server. The strategy " 
     "non distribing means ongoing requests should be completed. "
     "When the timeout occurs the block operation sohould be canceled." 
     "Using new of configure new SSL"];
essl_block_non_disturbing_active_timeout_released(suite) ->
    [];
essl_block_non_disturbing_active_timeout_released(Config)
  when is_list(Config) ->
    ssl_block_non_disturbing_active_timeout_released(essl, Config).

ssl_block_non_disturbing_active_timeout_released(Tag, Config)
  when is_list(Config) ->
    Port = ?SSL_PORT, 
    Host = proplists:get_value(host, Config), 
    Node = proplists:get_value(node, Config), 
    httpd_block:block_non_disturbing_active_timeout_released(Tag, 
							     Port, 
							     Host, 
							     Node), 

    ok.


%%-------------------------------------------------------------------------


essl_block_disturbing_blocker_dies(doc) ->
    ["using new of configure new SSL"];
essl_block_disturbing_blocker_dies(suite) ->
    [];
essl_block_disturbing_blocker_dies(Config) when is_list(Config) ->
    ssl_block_disturbing_blocker_dies(essl, Config).

ssl_block_disturbing_blocker_dies(Tag, Config) ->
    httpd_block:disturbing_blocker_dies(Tag, 
					?SSL_PORT, 
					proplists:get_value(host, Config), 
					proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------

essl_block_non_disturbing_blocker_dies(doc) ->
    ["using new of configure new SSL"];
essl_block_non_disturbing_blocker_dies(suite) ->
    [];
essl_block_non_disturbing_blocker_dies(Config) when is_list(Config) ->
    ssl_block_non_disturbing_blocker_dies(essl, Config).

ssl_block_non_disturbing_blocker_dies(Tag, Config) ->
    httpd_block:non_disturbing_blocker_dies(Tag, 
					    ?SSL_PORT, 
					    proplists:get_value(host, Config), 
					    proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------


essl_restart_no_block(doc) ->
    ["using new of configure new SSL"];
essl_restart_no_block(suite) ->
    [];
essl_restart_no_block(Config) when is_list(Config) ->
    ssl_restart_no_block(essl, Config).

ssl_restart_no_block(Tag, Config) ->
    httpd_block:restart_no_block(Tag, 
				 ?SSL_PORT, 
				 proplists:get_value(host, Config), 
				 proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------


essl_restart_disturbing_block(doc) ->
    ["using new of configure new SSL"];
essl_restart_disturbing_block(suite) ->
    [];
essl_restart_disturbing_block(Config) when is_list(Config) ->
    ssl_restart_disturbing_block(essl, Config).

ssl_restart_disturbing_block(Tag, Config) ->
    httpd_block:restart_disturbing_block(Tag, ?SSL_PORT, 
					 proplists:get_value(host, Config), 
					 proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------


essl_restart_non_disturbing_block(doc) ->
    ["using new of configure new SSL"];
essl_restart_non_disturbing_block(suite) ->
    [];
essl_restart_non_disturbing_block(Config) when is_list(Config) ->
    ssl_restart_non_disturbing_block(essl, Config).

ssl_restart_non_disturbing_block(Tag, Config) ->
    httpd_block:restart_non_disturbing_block(Tag, 
					     ?SSL_PORT, 
					     proplists:get_value(host, Config), 
					     proplists:get_value(node, Config)),
    ok.


%%-------------------------------------------------------------------------
ip_host(doc) ->   
    ["Control that the server accepts/rejects requests with/ without host"];
ip_host(suite)->
    [];
ip_host(Config) when is_list(Config) ->
    httpd_1_1:host(ip_comm, ?IP_PORT, proplists:get_value(host, Config),
		   proplists:get_value(node, Config)),
    ok.
%%------------------------------------------------------------------------- 
ip_chunked(doc) ->   
    ["Control that the server accepts chunked requests"];
ip_chunked(suite) ->
    [];
ip_chunked(Config) when is_list(Config) ->
    httpd_1_1:chunked(ip_comm, ?IP_PORT, proplists:get_value(host, Config),
		      proplists:get_value(node, Config)),
    ok.
%%------------------------------------------------------------------------- 
ip_expect(doc) ->   
    ["Control that the server handles request with the expect header "
     "field appropiate"];
ip_expect(suite)->
    [];
ip_expect(Config) when is_list(Config) ->
    httpd_1_1:expect(ip_comm, ?IP_PORT, proplists:get_value(host, Config),
		     proplists:get_value(node, Config)),
    ok.
%%------------------------------------------------------------------------- 
ip_range(doc) ->   
    ["Control that the server can handle range requests to plain files"];
ip_range(suite)->
    [];
ip_range(Config) when is_list(Config) ->
    httpd_1_1:range(ip_comm, ?IP_PORT, proplists:get_value(host, Config),
		    proplists:get_value(node, Config)),
    ok.
%%------------------------------------------------------------------------- 
ip_if_test(doc) ->   
    ["Test that the if - request header fields is handled correclty"];
ip_if_test(suite) ->
    [];
ip_if_test(Config) when is_list(Config) ->
    ServerRoot = proplists:get_value(server_root, Config), 
    DocRoot = filename:join([ServerRoot, "htdocs"]),
    httpd_1_1:if_test(ip_comm, ?IP_PORT, proplists:get_value(host, Config),
		      proplists:get_value(node, Config), DocRoot),
    ok.
%%------------------------------------------------------------------------- 
ip_http_trace(doc) ->   
    ["Test the trace module "];
ip_http_trace(suite) -> 
    [];
ip_http_trace(Config) when is_list(Config) ->
    httpd_1_1:http_trace(ip_comm, ?IP_PORT, proplists:get_value(host, Config),
			 proplists:get_value(node, Config)),
    ok.
%%------------------------------------------------------------------------- 
ip_http1_1_head(doc) ->  
    ["Test the trace module "];
ip_http1_1_head(suite)->
    [];
ip_http1_1_head(Config) when is_list(Config) ->
    httpd_1_1:head(ip_comm, ?IP_PORT, proplists:get_value(host, Config),
			   proplists:get_value(node, Config)),
    ok.

%%------------------------------------------------------------------------- 
ip_get_0_9(doc) ->  
    ["Test simple HTTP/0.9 GET"];
ip_get_0_9(suite)->
    [];
ip_get_0_9(Config) when is_list(Config) ->
    Host =  proplists:get_value(host, Config),
    Node =  proplists:get_value(node, Config),
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
    Host =  proplists:get_value(host, Config),
    Node =  proplists:get_value(node, Config),
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
    Host =  proplists:get_value(host, Config),
    Node =  proplists:get_value(node, Config),
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
    Host =  proplists:get_value(host, Config),
    Node =  proplists:get_value(node, Config),
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
    Host = proplists:get_value(host, Config),
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
					    proplists:get_value(node, Config),
					    Requests),
    ok.

%------------------------------------------------------------------------- 

ipv6_hostname_ipcomm() ->
    [{require, ipv6_hosts}].
ipv6_hostname_ipcomm(X) -> 
    SocketType = ip_comm,
    Port       = ?IP_PORT, 
    ipv6_hostname(SocketType, Port, X).

ipv6_hostname_essl() ->
    [{require, ipv6_hosts}].
ipv6_hostname_essl(X) -> 
    SocketType = essl, 
    Port       = ?SSL_PORT, 
    ipv6_hostname(SocketType, Port, X).

ipv6_hostname(_SocketType, _Port, doc) ->  
    ["Test standard ipv6 address"];
ipv6_hostname(_SocketType, _Port, suite)->
    [];
ipv6_hostname(SocketType, Port, Config) when is_list(Config) -> 
    tsp("ipv6_hostname -> entry with"
	"~n   SocketType: ~p"
	"~n   Port:       ~p"
	"~n   Config:     ~p", [SocketType, Port, Config]),
    Host = proplists:get_value(host, Config),
    URI  = "GET HTTP://" ++ 
	Host ++ ":" ++ integer_to_list(Port) ++ "/ HTTP/1.1\r\n\r\n", 
    tsp("ipv6_hostname -> Host: ~p", [Host]),
    httpd_test_lib:verify_request(SocketType, Host, Port, [inet6], 
				  node(), 
				  URI, 
				  [{statuscode, 200}, {version, "HTTP/1.1"}]),
    ok.

%%------------------------------------------------------------------------- 

ipv6_address_ipcomm() ->
    [{require, ipv6_hosts}].
ipv6_address_ipcomm(X) ->
    SocketType = ip_comm,
    Port       = ?IP_PORT, 
    ipv6_address(SocketType, Port, X).

ipv6_address_essl() ->
    [{require, ipv6_hosts}].
ipv6_address_essl(X) ->
    SocketType = essl,
    Port       = ?SSL_PORT, 
    ipv6_address(SocketType, Port, X).

ipv6_address(_SocketType, _Port, doc) ->
    ["Test standard ipv6 address"];
ipv6_address(_SocketType, _Port, suite)->
    [];
ipv6_address(SocketType, Port, Config) when is_list(Config) ->   
    tsp("ipv6_address -> entry with"
	"~n   SocketType: ~p"
	"~n   Port:       ~p"
	"~n   Config:     ~p", [SocketType, Port, Config]),
    Host = proplists:get_value(host, Config),
    tsp("ipv6_address -> Host: ~p", [Host]),
    URI = "GET HTTP://" ++ 
	Host ++ ":" ++ integer_to_list(Port) ++ "/ HTTP/1.1\r\n\r\n", 
    httpd_test_lib:verify_request(SocketType, Host, Port, [inet6], 
				  node(), 
				  URI, 
				  [{statuscode, 200}, {version, "HTTP/1.1"}]),
    ok.


%%--------------------------------------------------------------------
ticket_5775(doc) ->
    ["Tests that content-length is correct"];
ticket_5775(suite) ->
    [];
ticket_5775(Config) ->
    ok=httpd_test_lib:verify_request(ip_comm, proplists:get_value(host, Config),
				     ?IP_PORT, proplists:get_value(node, Config),
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
    ct:skip(as_of_r15_behaviour_of_calendar_has_changed),
    Host = proplists:get_value(host,Config),
    ServerRoot = proplists:get_value(server_root, Config), 
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
					       ?IP_PORT, proplists:get_value(node, Config),
					       "GET /last_modified.html"
					       " HTTP/1.1\r\nHost:"
					       ++Host++"\r\n\r\n", 
					       [{statuscode, 200},
						{no_header,
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
    ok = httpd_test_lib:verify_request(ip_comm, proplists:get_value(host, Config),
				       ?IP_PORT, proplists:get_value(node, Config),
				       "GET /cgi-bin/erl/httpd_example:get_bin "
				       "HTTP/1.0\r\n\r\n", 
				       [{statuscode, 200},
					{version, "HTTP/1.0"}]),
    ok.

ticket_6003(doc) ->
    ["Tests that a URI with a bad hexadecimal code is handled"];
ticket_6003(suite) -> [];
ticket_6003(Config) ->
    ok = httpd_test_lib:verify_request(ip_comm, proplists:get_value(host, Config),
				       ?IP_PORT, proplists:get_value(node, Config),
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
    ok = httpd_test_lib:verify_request(ip_comm, proplists:get_value(host, Config),
				       ?IP_PORT, proplists:get_value(node, Config),
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
    ServerRoot = proplists:get_value(server_root, Config),
    TcTopDir   = proplists:get_value(tc_top_dir,  Config),
    Port       = proplists:get_value(port,        Config),
    Type       = proplists:get_value(sock_type,   Config),
    Host       = proplists:get_value(host,        Config),
    Mods       = io_lib:format("~p", [httpd_mod]),
    Funcs      = io_lib:format("~p", [ssl_password_cb]),
    MaxHdrSz   = io_lib:format("~p", [256]),
    MaxHdrAct  = io_lib:format("~p", [close]),

    io:format(user, 
	      "create_config -> "
	      "~n   ServerRoot: ~p"
	      "~n   TcTopDir:   ~p"
	      "~n   Type:       ~p"
	      "~n   Port:       ~p"
	      "~n   Host:       ~p"
	      "~n", [ServerRoot, TcTopDir, Type, Port, Host]),

    SSL =
	if
	    (Type =:= ssl)  orelse 
	    (Type =:= essl) ->
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
	    true ->
		[]
	end,
    ModOrder = 
	case Access of
	    mod_htaccess ->
		"Modules mod_alias mod_htaccess mod_auth "
		    "mod_security "
		    "mod_responsecontrol mod_trace mod_esi "
		    "mod_actions mod_cgi mod_dir "
		    "mod_range mod_get "
		    "mod_head mod_log mod_disk_log";
	    _ ->
		"Modules mod_alias mod_auth mod_security "
		    "mod_responsecontrol mod_trace mod_esi "
		    "mod_actions mod_cgi mod_dir "
			   "mod_range mod_get "
		    "mod_head mod_log mod_disk_log"
	end,
    
    %% The test suite currently does not handle an explicit BindAddress.
    %% They assume any has been used, that is Addr is always set to undefined!

    %%     {ok, Hostname} = inet:gethostname(), 
    %%     {ok, Addr} = inet:getaddr(Hostname, inet6),
    %%     AddrStr = make_ipv6(Addr), 
    %%     BindAddress = lists:flatten(io_lib:format("~s|inet6", [AddrStr])),

    BindAddress = "*|inet", 
    %% BindAddress = "*", 

    HttpConfig = [
		  cline(["Port ", integer_to_list(Port)]),
		  cline(["ServerName ", Host]),
		  cline(["SocketType ", atom_to_list(Type)]),
		  cline([ModOrder]),
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
	    tsf({failed_to_cleanup_mnesia, Other})
    end,
    case rpc:call(Node, ?MODULE, setup_mnesia, []) of
	{atomic, ok} ->
	    ok;
	Other2 ->
	    tsf({failed_to_setup_mnesia, Other2})
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

create_htaccess_data(Path, IpAddress)->
    create_htaccess_dirs(Path),
    
    create_html_file(filename:join([Path,"ht/open/dummy.html"])),
    create_html_file(filename:join([Path,"ht/blocknet/dummy.html"])),
    create_html_file(filename:join([Path,"ht/secret/dummy.html"])),
    create_html_file(filename:join([Path,"ht/secret/top_secret/dummy.html"])),
    
    create_htaccess_file(filename:join([Path,"ht/open/.htaccess"]),
			 Path, "user one Aladdin"),
    create_htaccess_file(filename:join([Path,"ht/secret/.htaccess"]),
			 Path, "group group1 group2"),
    create_htaccess_file(filename:join([Path,
				       "ht/secret/top_secret/.htaccess"]),
			Path, "user four"),
    create_htaccess_file(filename:join([Path,"ht/blocknet/.htaccess"]),
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

create_htaccess_file(PathAndFileName, BaseDir, RequireData)->
    file:write_file(PathAndFileName,
		    list_to_binary(
		      "AuthUserFile "++ BaseDir ++
		      "/ht/users.file\nAuthGroupFile "++ BaseDir
		      ++ "/ht/groups.file\nAuthName Test\nAuthType"
		      " Basic\n<Limit>\nrequire " ++ RequireData ++
		      "\n</Limit>")).

create_htaccess_file(PathAndFileName, BaseDir, nouser, IpAddress)->
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

create_htaccess_dirs(Path)->
    ok = file:make_dir(filename:join([Path,"ht"])),
    ok = file:make_dir(filename:join([Path,"ht/open"])),
    ok = file:make_dir(filename:join([Path,"ht/blocknet"])),
    ok = file:make_dir(filename:join([Path,"ht/secret"])),
    ok = file:make_dir(filename:join([Path,"ht/secret/top_secret"])).

remove_htaccess_dirs(Path)->
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

remove_htaccess(Path)->
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
    remove_htaccess_dirs(Path).


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

get_nof_clients(_,       ip_comm, light)  -> 5;
get_nof_clients(_,       ssl,     light)  -> 2;
get_nof_clients(_,       ip_comm, medium) -> 10;
get_nof_clients(_,       ssl,     medium) -> 4;
get_nof_clients(_,       ip_comm, heavy)  -> 20;
get_nof_clients(_,       ssl,     heavy)  -> 6.

%% Make a file 100 bytes long containing 012...9*10
create_range_data(Path) ->
    PathAndFileName=filename:join([Path,"range.txt"]),
    file:write_file(PathAndFileName,list_to_binary(["12345678901234567890",
						   "12345678901234567890",
						   "12345678901234567890",
						   "12345678901234567890",
						   "12345678901234567890"])).

create_ipv6_config(Config, FileName, Ipv6Address) ->
    ServerRoot = proplists:get_value(server_root, Config),
    TcTopDir   = proplists:get_value(tc_top_dir,  Config),
    Port       = proplists:get_value(port,        Config),
    SockType   = proplists:get_value(sock_type,   Config),
    Mods       = io_lib:format("~p",  [httpd_mod]),
    Funcs      = io_lib:format("~p",  [ssl_password_cb]),
    Host       = proplists:get_value(ipv6_host,   Config),

    MaxHdrSz     = io_lib:format("~p", [256]),
    MaxHdrAct    = io_lib:format("~p", [close]),
  
    Mod_order = "Modules mod_alias mod_auth mod_esi mod_actions mod_cgi" 
	" mod_dir mod_get mod_head" 
	" mod_log mod_disk_log mod_trace",
	    
    SSL =
	if
	    (SockType =:= ssl)  orelse 
	    (SockType =:= essl) ->
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
	    true ->
		[]
	end,

    BindAddress = "[" ++ Ipv6Address ++"]|inet6", 

    HttpConfig = 
	[cline(["BindAddress ", BindAddress]),
	 cline(["Port ", integer_to_list(Port)]),
	 cline(["ServerName ", Host]),
	 cline(["SocketType ", atom_to_list(SockType)]),
	 cline([Mod_order]),
	 cline(["ServerRoot ", ServerRoot]),
	 cline(["DocumentRoot ", filename:join(ServerRoot, "htdocs")]),
	 cline(["MaxHeaderSize ",MaxHdrSz]),
	 cline(["MaxHeaderAction ",MaxHdrAct]),
	 cline(["DirectoryIndex ", "index.html "]),
	 cline(["DefaultType ", "text/plain"]), 
	 SSL],
    ConfigFile = filename:join([TcTopDir,FileName]),
    {ok, Fd} = file:open(ConfigFile, [write]),
    ok = file:write(Fd, lists:flatten(HttpConfig)),
    ok = file:close(Fd).


tsp(F) ->
    inets_test_lib:tsp("[~w]" ++ F, [?MODULE]).
tsp(F, A) ->
    inets_test_lib:tsp("[~w]" ++ F, [?MODULE|A]).

tsf(Reason) ->
    inets_test_lib:tsf(Reason).

