%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2013-2013. All Rights Reserved.
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

%% 
%% ct:run("../inets_test", httpd_SUITE).
%%

-module(httpd_SUITE).

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").
-include("inets_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-record(httpd_user,  {user_name, password, user_data}).
-record(httpd_group, {group_name, userlist}).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    [
     {group, http}
     %{group, https}
    ].

groups() ->
    [
     {http, [], all_groups()},
     %{https, [], all_groups()},
     {http_1_1, [], [host, chunked, expect, cgi, max_clients
		    ] ++ http_head() ++ http_get()},
     {http_1_0, [], [host, cgi] ++ http_head() ++ http_get()},
     {http_0_9, [], http_head() ++ http_get()}
    ].

all_groups ()->
    [{group, http_1_1},
     {group, http_1_0},
     {group, http_0_9}
    ].

http_head() ->
    [head].
http_get() ->
    [alias, get, 
     basic_auth, 
     esi, ssi].

init_per_suite(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    inets_test_lib:stop_apps([inets]),
    inets_test_lib:start_apps([inets]),
    ServerRoot = filename:join(PrivDir, "server_root"),
    inets_test_lib:del_dirs(ServerRoot),
    DocRoot = filename:join(ServerRoot, "htdocs"),
    setup_server_dirs(ServerRoot, DocRoot, DataDir),
    [{server_root, ServerRoot}, 
     {doc_root, DocRoot},
     {node,             node()},
     {host,             inets_test_lib:hostname()} | Config].

end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
init_per_group(https = Group, Config0) ->
    case start_apps(Group) of
	ok ->
	    init_httpd(Group, [{type, ssl} | Config0]);
	_ ->
	    {skip, "Could not start https apps"}
    end;

init_per_group(http = Group, Config0) ->
    init_httpd(Group, [{type, ip_comm} | Config0]);
init_per_group(http_1_1, Config) ->
    [{http_version, "HTTP/1.1"} | Config];
init_per_group(http_1_0, Config) ->
    [{http_version, "HTTP/1.0"} | Config];
init_per_group(http_0_9, Config) ->
    [{http_version, "HTTP/0.9"} | Config];
init_per_group(_, Config) ->
    Config.
end_per_group(http, _Config) ->
    inets:stop();
end_per_group(https, _Config) ->
    ssl:stop(),
    inets:stop();
end_per_group(_, _) ->
    ok.

init_httpd(Group, Config0) ->
    Config1 = proplists:delete(port, Config0),
    Config = proplists:delete(server_pid, Config1),
    {Pid, Port} = server_start(Group, server_config(Group, Config)),
    [{server_pid, Pid}, {port, Port} | Config].
%%--------------------------------------------------------------------
init_per_testcase(host = Case, Config) ->
    Prop = ?config(tc_group_properties, Config),
    Name = proplists:get_value(name, Prop),
    Cb = case Name of
	     http_1_0 ->
		 httpd_1_0;
	     http_1_1 ->
		 httpd_1_1
	 end,
    common_init_per_test_case(Case, [{version_cb, Cb} | proplists:delete(version_cb, Config)]);

%% init_per_testcase(basic_auth = Case, Config) ->
%%     start_mnesia(?config(node, Config)),
%%     common_init_per_test_case(Case, Config);
    
init_per_testcase(max_clients, Config) ->
    Pid = ?config(server_pid, Config),    
    Prop = httpd:info(Pid),
    Port = proplists:get_value(port, Prop),
    TempProp = [{port, Port} | proplists:delete(port, server_config(http, Config))],
    NewProp = [{max_clients, 1} | TempProp],
    httpd:reload_config(NewProp, non_disturbing),
    Config;

init_per_testcase(_Case, Config) ->
    common_init_per_test_case(_Case, Config).

%%% Should be run by all test cases except max_clients, to make
%%% sure failiure of max_clients does not affect other test cases
common_init_per_test_case(_Case, Config) ->    
    Pid = ?config(server_pid, Config),    
    Prop = httpd:info(Pid),
    case proplists:get_value(max_clients, Prop, 150) of
    	150 ->
	    Config;
    	_ ->
	    end_per_testcase(max_clients, Config)
    end.

end_per_testcase(max_clients, Config) ->
    Pid = ?config(server_pid, Config),    
    Prop = httpd:info(Pid),
    Port = proplists:get_value(port, Prop),
    TempProp = [{port, Port} | proplists:delete(port, server_config(http, Config))],
    NewProp = proplists:delete(max_clients, TempProp),
    httpd:reload_config(NewProp, non_disturbing),
    Config;

%% end_per_testcase(basic_auth, Config) ->
%%     cleanup_mnesia();
end_per_testcase(_Case, _Config) ->
    ok.

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------

head() ->
    [{doc, "HTTP HEAD request for static page"}].

head(Config) when is_list(Config) -> 
    Version = ?config(http_version, Config),
    Host = ?config(host, Config),
    ok = httpd_test_lib:verify_request(?config(type, Config), Host, 
				       ?config(port, Config),  ?config(node, Config),
				       http_request("HEAD /index.html ", Version, Host),
				       [{statuscode, head_status(Version)},
					{version, Version}]).

get() ->
    [{doc, "HTTP GET request for static page"}].

get(Config) when is_list(Config) -> 
    Version = ?config(http_version, Config),
    Host = ?config(host, Config),
    ok = httpd_test_lib:verify_request(?config(type, Config), Host, 
				       ?config(port, Config),  ?config(node, Config),
				       http_request("GET /index.html ", Version, Host),
				       [{statuscode, 200},
					{header, "Content-Type", "text/html"},
					{header, "Date"},
					{header, "Server"},
					{version, Version}]).

basic_auth() ->
    [{doc, "Test Basic authentication with WWW-Authenticate header"}].

basic_auth(Config) ->
    Version = ?config(http_version, Config),
    Host = ?config(host, Config),
    basic_auth_requiered(Config),
    %% Authentication OK! ["one:OnePassword" user first in user list]
    ok = auth_status(auth_request("/open/dummy.html", "one", "onePassword", Version, Host), Config, 
		     [{statuscode, 200}]),
    %% Authentication OK and a directory listing is supplied!
    %% ["Aladdin:open sesame" user second in user list]
    ok = auth_status(auth_request("/open/", "Aladdin", "AladdinPassword", Version, Host), Config, 
		     [{statuscode, 200}]),
     %% User correct but wrong password! ["one:one" user first in user list]
    ok = auth_status(auth_request("/open/dummy.html", "one", "one", Version, Host), Config, 
		     [{statuscode, 401},
		      {header, "WWW-Authenticate"}]),
    %% Make sure Authenticate header is received even the second time
    %% we try a incorrect password! Otherwise a browser client will hang!
    ok = auth_status(auth_request("/open/dummy.html", "one", "one", Version, Host), Config, 
		     [{statuscode, 401},
		      {header, "WWW-Authenticate"}]),
    %% Neither user or password correct! ["dummy:dummy"]
    ok = auth_status(auth_request("/open/dummy.html", "dummy", "dummy", Version, Host), Config, 
		     [{statuscode, 401}]),
    %% Nested secret/top_secret OK! ["Aladdin:open sesame"]
    ok = http_status(auth_request("/secret/top_secret/", "Aladdin", "AladdinPassword", Version, Host), 
		     Config, [{statuscode, 200}]),
    %% Authentication still required!
    basic_auth_requiered(Config).
   
ssi() ->
    [{doc, "HTTP GET server side include test"}].
ssi(Config) when is_list(Config) -> 
    Version = ?config(http_version, Config),
    Host = ?config(host, Config),
    ok = httpd_test_lib:verify_request(?config(type, Config), Host, ?config(port, Config),  
				       ?config(node, Config),
				       http_request("GET /fsize.shtml ", Version, Host),
				       [{statuscode, 200},
					{header, "Content-Type", "text/html"},
					{header, "Date"},
					{header, "Server"},
				       	{version, Version}]).
host() ->
    [{doc, "Test host header"}].

host(Config) when is_list(Config) -> 
    Cb = ?config(version_cb, Config),
    Cb:host(?config(type, Config), ?config(port, Config), 
	    ?config(host, Config), ?config(node, Config)).

chunked() ->
    [{doc, "Check that the server accepts chunked requests."}].

chunked(Config) when is_list(Config) ->
    httpd_1_1:chunked(?config(type, Config), ?config(port, Config), 
		      ?config(host, Config), ?config(node, Config)).

expect() ->   
    ["Check that the server handles request with the expect header "
     "field appropiate"].
expect(Config) when is_list(Config) ->
    httpd_1_1:expect(?config(type, Config), ?config(port, Config), 
		     ?config(host, Config), ?config(node, Config)).

max_clients() ->
    [{doc, "Test max clients limit"}].

max_clients(Config) when is_list(Config) -> 
    Version = ?config(http_version, Config),
    Host = ?config(host, Config),
    Pid = ?config(server_pid, Config),
    ct:pal("Configurartion: ~p~n", [httpd:info(Pid)]),
    spawn(fun() -> httpd_test_lib:verify_request(?config(type, Config), Host, 
						 ?config(port, Config),  ?config(node, Config),
						 http_request("GET /eval?httpd_example:delay(1000) ", 
							      Version, Host),
						 [{statuscode, 200},
						  {version, Version}])
	  end),
    ok = httpd_test_lib:verify_request(?config(type, Config), Host, 
				       ?config(port, Config),  ?config(node, Config),
				       http_request("GET /index.html ", Version, Host),
				       [{statuscode, 503},
					{version, Version}]),
    receive 
    after 1000 ->
	    ok = httpd_test_lib:verify_request(?config(type, Config), Host, 
				       ?config(port, Config),  ?config(node, Config),
				       http_request("GET /index.html ", Version, Host),
					       [{statuscode, 200},
						{version, Version}])
    end.
	    
esi() ->
    [{doc, "Test mod_esi"}].

esi(Config) when is_list(Config) -> 
    ok = http_status("GET /eval?httpd_example:print(\"Hi!\") ",
		     Config, [{statuscode, 200}]),
    ok = http_status("GET /eval?not_allowed:print(\"Hi!\") ",
		     Config, [{statuscode, 403}]),
    ok = http_status("GET /eval?httpd_example:undef(\"Hi!\") ",
		      Config, [{statuscode, 500}]),
    ok = http_status("GET /cgi-bin/erl/httpd_example ", 
		     Config, [{statuscode, 400}]),
    ok = http_status("GET /cgi-bin/erl/httpd_example:get ",
		     Config, [{statuscode, 200}]),
    ok = http_status("GET /cgi-bin/erl/httpd_example:"
		     "get?input=4711 ", Config,
		     [{statuscode, 200}]),
    ok = http_status("GET /cgi-bin/erl/httpd_example:post ",
		     Config, [{statuscode, 200}]),
    ok = http_status("GET /cgi-bin/erl/not_allowed:post ",
		     Config, [{statuscode, 403}]),
    ok = http_status("GET /cgi-bin/erl/httpd_example:undef ",
		     Config, [{statuscode, 404}]),
    ok = http_status("GET /cgi-bin/erl/httpd_example/yahoo ",
		     Config, [{statuscode, 302}]),
    %% Check "ErlScriptNoCache" directive (default: false)
    ok = http_status("GET /cgi-bin/erl/httpd_example:get ",
		     Config, [{statuscode, 200},
		      {no_header, "cache-control"}]).

cgi() ->
    [{doc, "Test mod_cgi"}].

cgi(Config) when is_list(Config) -> 
    {Script, Script2, Script3} =
	case test_server:os_type() of
	    {win32, _} ->
		{"printenv.bat", "printenv.sh", "cgi_echo.exe"};
	    _ ->
		{"printenv.sh", "printenv.bat", "cgi_echo"}
	end,

     %%The length (> 100) is intentional
     ok = http_status("POST /cgi-bin/" ++ Script3 ++ " ", 
     		     {"Content-Length:100 \r\n",
     		      "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
     		      "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
     		      "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
     		      "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
     		      "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
     		      "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
     		      "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
     		      "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
     		      "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
     		      "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
     		      "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
     		      "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
     		      "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
     		      "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
     		      "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
     		      "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
     		      "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"}, 
		      Config,
     		     [{statuscode, 200},
     		      {header, "content-type", "text/plain"}]),
    
    ok = http_status("GET /cgi-bin/"++ Script ++ " ", Config, [{statuscode, 200}]),

    ok = http_status("GET /cgi-bin/not_there ", Config, 
		     [{statuscode, 404}, {statuscode, 500}]),
    
    ok = http_status("GET /cgi-bin/"++ Script ++ "?Nisse:kkk?sss/lll ", 
     		     Config,
     		     [{statuscode, 200}]),
    
    ok = http_status("POST /cgi-bin/"++ Script  ++ " ", Config,
		     [{statuscode, 200}]),
    
    ok = http_status("GET /htbin/"++ Script ++ " ",  Config,
		     [{statuscode, 200}]),
    
    ok = http_status("GET /htbin/not_there ", Config,
		     [{statuscode, 404},{statuscode, 500}]),
    
    ok = http_status("GET /htbin/"++ Script ++ "?Nisse:kkk?sss/lll ", Config, 
     		     [{statuscode, 200}]),
    
    ok = http_status("POST /htbin/"++ Script ++ " ",   Config,
		     [{statuscode, 200}]),
    
    ok = http_status("POST /htbin/"++ Script ++ " ",  Config,
		     [{statuscode, 200}]),
    
    %% Execute an existing, but bad CGI script..
    ok = http_status("POST /htbin/"++ Script2 ++ " ",  Config, 
		     [{statuscode, 404}]),
    
    ok = http_status("POST /cgi-bin/"++ Script2 ++ " ", Config,
		     [{statuscode, 404}]),
    
    %% Check "ScriptNoCache" directive (default: false)
    ok = http_status("GET /cgi-bin/" ++ Script ++ " ", Config,
		     [{statuscode, 200},
		      {no_header, "cache-control"}]).

alias() ->
    [{doc, "Test mod_alias"}].

alias(Config) when is_list(Config) -> 
    ok = http_status("GET /pics/icon.sheet.gif ", Config,
		     [{statuscode, 200},
		      {header, "Content-Type","image/gif"},
		      {header, "Server"},
		      {header, "Date"}]),
    
    ok = http_status("GET / ", Config,
		     [{statuscode, 200},
		      {header, "Content-Type","text/html"},
		      {header, "Server"},
		      {header, "Date"}]),
    
    ok = http_status("GET /misc/ ", Config,
		     [{statuscode, 200},
		      {header, "Content-Type","text/html"},
		      {header, "Server"},
		      {header, "Date"}]),

    %% Check redirection if trailing slash is missing.
    ok = http_status("GET /misc ", Config,
		     [{statuscode, 301},
		      {header, "Location"},
		      {header, "Content-Type","text/html"}]).


%% auth_api() ->
%%     [{doc, "Test mod_auth API"}].

%% auth_api(Config) when is_list(Config) -> 
%%     Version = ?config(http_version, Config),
%%     Host = ?config(host, Config),
%%     ok = http_status("GET / ", Config,
%% 		     [{statuscode, 200}]),
%%     ok = auth_status(auth_request("/", "one", "WrongPassword", Version, Host), Config,
%% 		     [{statuscode, 200}]),

%%     %% Make sure Authenticate header is received even the second time
%%     %% we try a incorrect password! Otherwise a browser client will hang!
%%     ok = auth_status(auth_request("/" ++ AuthStoreType ++ "open/",
%% 				  "dummy", "WrongPassword", Host), Config, 
%% 		     [{statuscode, 401},
%% 		      {header, "WWW-Authenticate"}]),
%%     ok = auth_status(auth_request("/" ++ AuthStoreType ++ "open/", "dummy", "WrongPassword", 
%% 				  Host), Config, [{statuscode, 401},	
%% 						  {header, "WWW-Authenticate"}]),
    
%%     %% Change the password to DummyPassword then try to add a user 
%%     %% Get an error and set it to NoPassword
%%     ok = update_password(Node, ServerRoot, Host, Port, AuthStoreType ++ 
%% 			 "open", "NoPassword", "DummyPassword"),
%%     {error,bad_password} = 
%% 	add_user(Node, ServerRoot, Port, AuthStoreType ++ "open", "one", 
%% 		 "onePassword", []),
%%     ok = update_password(Node, ServerRoot, Host, Port, AuthStoreType ++"open",
%% 			 "DummyPassword", "NoPassword"),
  
%%     %% Test /*open, require user one Aladdin
%%     remove_users(Node, ServerRoot, Host, Port, AuthStoreType ++ "open"),

%%     auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "open/",
%% 		 "one", "onePassword", [{statuscode, 401}]),
    
%%     auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "open/",
%% 		 "two", "twoPassword", [{statuscode, 401}]),
 
%%     auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "open/", 
%% 		 "Aladdin", "onePassword", [{statuscode, 401}]),

%%     add_user(Node, ServerRoot, Port, AuthStoreType ++ "open", "one", 
%% 	     "onePassword", []),
%%     add_user(Node, ServerRoot, Port, AuthStoreType ++ "open", "two", 
%% 	     "twoPassword", []),
%%     add_user(Node, ServerRoot, Port, AuthStoreType ++ "open", "Aladdin", 
%% 	     "AladdinPassword", []),
   
%%     {ok, [_|_]} = list_users(Node, ServerRoot, Host, Port, 
%% 			  AuthStoreType++"open"),
%%     auth_request(Type, Host, Port, Node, "/" ++ AuthStoreType ++ "open/",
%% 		 "one", "WrongPassword", [{statuscode, 401}]),
%%     auth_request(Type, Host, Port, Node, "/" ++ AuthStoreType ++ "open/", 
%% 		 "one", "onePassword", [{statuscode, 200}]),
%%     auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "open/", 
%% 		 "two", "twoPassword", [{statuscode, 401}]),
%%     auth_request(Type, Host, Port, Node, "/" ++ AuthStoreType ++ "open/", 
%% 		 "Aladdin", "WrongPassword", [{statuscode, 401}]),
%%     auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "open/", 
%% 		 "Aladdin", "AladdinPassword", [{statuscode, 200}]),
   
%%     remove_users(Node, ServerRoot, Host, Port, AuthStoreType++"open"),
%%     {ok, []} = list_users(Node, ServerRoot, Host, Port, 
%% 			  AuthStoreType++"open"),

%%     %% Phase 2
%%     remove_users(Node, ServerRoot, Host, Port, AuthStoreType++"secret"),
%%     {ok, []} = list_users(Node, ServerRoot, Host, Port, AuthStoreType ++
%% 			  "secret"),
%%     auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "secret/",
%% 		 "one", "onePassword", [{statuscode, 401}]),
%%     auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "secret/", 
%% 		 "two", "twoPassword", [{statuscode, 401}]),
%%     auth_request(Type, Host, Port,  Node, "/" ++ AuthStoreType ++ "secret/", 
%% 		 "three", "threePassword", [{statuscode, 401}]),
%%     add_user(Node, ServerRoot, Port, AuthStoreType ++ "secret", "one",
%% 	     "onePassword", 
%% 	     []),
%%     add_user(Node, ServerRoot, Port, AuthStoreType ++ "secret", 
%% 	     "two", "twoPassword", []),
%%     add_user(Node, ServerRoot, Port, AuthStoreType++"secret", "Aladdin", 
%% 	     "AladdinPassword",[]),
%%     add_group_member(Node, ServerRoot, Port, AuthStoreType ++ "secret", 
%% 		     "one", "group1"),
%%     add_group_member(Node, ServerRoot, Port, AuthStoreType ++ "secret", 
%% 		     "two", "group1"),
%%     add_group_member(Node, ServerRoot, Port, AuthStoreType ++ 
%% 		     "secret", "Aladdin", "group2"),
%%     auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "secret/",
%% 		 "one", "onePassword", [{statuscode, 200}]),
%%     auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "secret/", 
%% 		 "two", "twoPassword", [{statuscode, 200}]),
%%     auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "secret/",
%% 		 "Aladdin", "AladdinPassword", [{statuscode, 200}]),
%%     auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "secret/",
%% 		 "three", "threePassword", [{statuscode, 401}]),
%%     remove_users(Node, ServerRoot, Host, Port, AuthStoreType ++ "secret"),
%%     {ok, []} = list_users(Node, ServerRoot, Host, Port, 
%% 			  AuthStoreType ++ "secret"),
%%     remove_groups(Node, ServerRoot, Host, Port, AuthStoreType ++ "secret"),
%%     Directory = filename:join([ServerRoot, "htdocs", AuthStoreType ++ 
%% 			       "secret"]),
%%     {ok, []} = list_groups(Node, ServerRoot, Host, Port, Directory),

%%     %% Phase 3
%%     remove_users(Node, ServerRoot, Host, Port, AuthStoreType ++ 
%% 		 "secret/top_secret"),
%%     remove_groups(Node, ServerRoot, Host, Port, AuthStoreType ++ 
%% 		  "secret/top_secret"),
%%     auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ 
%% 		 "secret/top_secret/",
%% 		 "three", "threePassword", [{statuscode, 401}]),
%%     auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ 
%% 		 "secret/top_secret/", "two", "twoPassword", 
%% 		 [{statuscode, 401}]),
%%     add_user(Node, ServerRoot, Port, AuthStoreType ++ 
%% 	     "secret/top_secret","three",
%% 	     "threePassword",[]),
%%     add_user(Node, ServerRoot, Port, AuthStoreType ++ "secret/top_secret",
%% 	     "two","twoPassword", []),
%%     add_group_member(Node, ServerRoot, Port, AuthStoreType ++ 
%% 		     "secret/top_secret",
%% 		     "three", "group3"),
%%     auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ 
%% 		 "secret/top_secret/", "three", "threePassword", 
%% 		 [{statuscode, 200}]),
%%     auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ 
%% 		 "secret/top_secret/", "two", "twoPassword", 
%% 		 [{statuscode, 401}]),
%%     add_group_member(Node, ServerRoot, Port, AuthStoreType ++ 
%% 		     "secret/top_secret",
%% 		     "two", "group3"),
%%     auth_request(Type,Host,Port,Node,"/" ++ AuthStoreType ++ 
%% 		 "secret/top_secret/",
%% 		 "two", "twoPassword", [{statuscode, 200}]),
%%     remove_users(Node, ServerRoot, Host, Port, AuthStoreType ++ 
%% 		 "secret/top_secret"),
%%     {ok, []} = list_users(Node, ServerRoot, Host, Port, 
%% 			  AuthStoreType ++ "secret/top_secret"),
%%     remove_groups(Node, ServerRoot, Host, Port, AuthStoreType ++ 
%% 		  "secret/top_secret"),
%%     Directory2 = filename:join([ServerRoot, "htdocs", 
%% 				AuthStoreType ++ "secret/top_secret"]),
%%     {ok, []} = list_groups(Node, ServerRoot, Host, Port, Directory2),
%%     auth_request(Type, Host, Port, Node, "/" ++ AuthStoreType ++ 
%% 		 "secret/top_secret/", "two", "twoPassword", 
%% 		 [{statuscode, 401}]),
%%     auth_request(Type, Host, Port, Node, "/" ++ AuthStoreType ++ 
%% 		 "secret/top_secret/","three", "threePassword",
%% 		 [{statuscode, 401}]).


%%--------------------------------------------------------------------
%% Internal functions -----------------------------------
%%--------------------------------------------------------------------
setup_server_dirs(ServerRoot, DocRoot, DataDir) ->   
    CgiDir =  filename:join(ServerRoot, "cgi-bin"),
    AuthDir =  filename:join(ServerRoot, "auth"),
    PicsDir =  filename:join(ServerRoot, "icons"),

    ok = file:make_dir(ServerRoot),
    ok = file:make_dir(DocRoot),
    ok = file:make_dir(CgiDir),
    ok = file:make_dir(AuthDir),
    ok = file:make_dir(PicsDir),

    DocSrc = filename:join(DataDir, "server_root/htdocs"),    
    AuthSrc = filename:join(DataDir, "server_root/auth"),    
    CgiSrc =  filename:join(DataDir, "server_root/cgi-bin"),    
    PicsSrc =  filename:join(DataDir, "server_root/icons"),    
    
    inets_test_lib:copy_dirs(DocSrc, DocRoot),
    inets_test_lib:copy_dirs(AuthSrc, AuthDir),
    inets_test_lib:copy_dirs(CgiSrc, CgiDir),
    inets_test_lib:copy_dirs(PicsSrc, PicsDir),
        
    Cgi = case test_server:os_type() of
	      {win32, _} ->
		  "cgi_echo.exe";
	      _ ->
		  "cgi_echo"
	  end,
    
    inets_test_lib:copy_file(Cgi, DataDir, CgiDir),
    AbsCgi = filename:join([CgiDir, Cgi]),
    {ok, FileInfo} = file:read_file_info(AbsCgi),
    ok = file:write_file_info(AbsCgi, FileInfo#file_info{mode = 8#00755}),
    
    EnvCGI =  filename:join([ServerRoot, "cgi-bin", "printenv.sh"]),
    {ok, FileInfo1} = file:read_file_info(EnvCGI),
    ok = file:write_file_info(EnvCGI, 
			      FileInfo1#file_info{mode = 8#00755}).
    
start_apps(https) ->
    inets_test_lib:start_apps([crypto, public_key, ssl]);
start_apps(_) ->
    ok.

server_start(_, HttpdConfig) ->
    {ok, Pid} = inets:start(httpd, HttpdConfig),
    Serv = inets:services_info(),
    {value, {_, _, Info}} = lists:keysearch(Pid, 2, Serv),
    {Pid, proplists:get_value(port, Info)}.

server_config(http, Config) ->
    ServerRoot = ?config(server_root, Config),
    [{port, 0},
     {server_name,"httpd_test"},
     {server_root, ServerRoot},
     {document_root, ?config(doc_root, Config)},
     {bind_address, any},
     {ipfamily, inet},
     {max_header_size, 256},
     {max_header_action, close},
     {mime_types, [{"html","text/html"},{"htm","text/html"}, {"shtml","text/html"},
		   {"gif", "image/gif"}]},
     {alias, {"/icons/", filename:join(ServerRoot,"icons") ++ "/"}},
     {alias, {"/pics/",  filename:join(ServerRoot,"icons") ++ "/"}},
     {script_alias, {"/cgi-bin/", filename:join(ServerRoot, "cgi-bin") ++ "/"}},
     {script_alias, {"/htbin/", filename:join(ServerRoot, "cgi-bin") ++ "/"}},
     {erl_script_alias, {"/cgi-bin/erl", [httpd_example, io]}},
     {eval_script_alias, {"/eval", [httpd_example, io]}}
    ] ++  auth_conf(ServerRoot);
server_config(_, _) ->
    [].

http_request(Request, "HTTP/1.1" = Version, Host, {Headers, Body}) ->
    Request ++ Version ++ "\r\nhost:" ++ Host ++ "\r\n" ++ Headers ++ "\r\n" ++ Body;
http_request(Request, Version, _, {Headers, Body}) ->
    Request ++ Version ++ "\r\n" ++ Headers  ++ "\r\n" ++ Body.

http_request(Request, "HTTP/1.1" = Version, Host) ->
    Request ++ Version ++ "\r\nhost:" ++ Host  ++ "\r\n\r\n";
http_request(Request, Version, _) ->
    Request ++ Version ++ "\r\n\r\n".

auth_request(Path, User, Passwd, "HTTP/1.1" = Version, Host) ->
    "GET " ++ Path ++ " " ++ Version ++  "\r\nhost:" ++ Host  ++
	"\r\nAuthorization: Basic " ++  
	base64:encode_to_string(User++":"++Passwd) ++
	"\r\n\r\n";
auth_request(Path, User, Passwd, Version, _Host) ->
    "GET " ++ Path ++ " " ++ Version ++  
	"\r\nAuthorization: Basic " ++  
	base64:encode_to_string(User++":"++Passwd) ++
	"\r\n\r\n".

head_status("HTTP/0.9") ->
    501; %% Not implemented in HTTP/0.9
head_status(_) ->
    200.

auth_conf(Root) ->
    [{directory, {filename:join(Root, "htdocs/open"), 
		  [{auth_type, plain},
		   {auth_name, "Open Area"},
		   {auth_user_file, filename:join(Root, "auth/passwd")},
		   {auth_group_file, filename:join(Root, "auth/group")},
		   {require_user, ["one", "Aladdin"]}]}},
    {directory, {filename:join(Root, "htdocs/secret"), 
		  [{auth_type, plain},
		   {auth_name, "Secret Area"},
		   {auth_user_file, filename:join(Root, "auth/passwd")},
		   {auth_group_file, filename:join(Root, "auth/group")},
		   {require_group, ["group1", "group2"]}]}},
     {directory, {filename:join(Root, "htdocs/secret/top_secret"), 
		  [{auth_type, plain},
		   {auth_name, "Top Secret Area"},
		   {auth_user_file, filename:join(Root, "auth/passwd")},
		   {auth_group_file, filename:join(Root, "auth/group")},
		   {require_group, ["group3"]}]}},
     {directory, {filename:join(Root, "htdocs/open"), 
		  [{auth_type, mnesia},
		   {auth_name, "Open Area"},
		   {auth_user_file, filename:join(Root, "auth/passwd")},
		   {auth_group_file, filename:join(Root, "auth/group")},
		   {require_user, ["one", "Aladdin"]}]}},
     {directory, {filename:join(Root, "htdocs/secret"), 
		  [{auth_type, mnesia},
		   {auth_name, "Secret Area"},
		   {auth_user_file, filename:join(Root, "auth/passwd")},
		   {auth_group_file, filename:join(Root, "auth/group")},
		   {require_group, ["group1", "group2"]}]}}
    ]. 


http_status(Request, Config, Expected) ->
    Version = ?config(http_version, Config),
    Host = ?config(host, Config),    
    httpd_test_lib:verify_request(?config(type, Config), Host, 
				  ?config(port, Config),  ?config(node, Config),
				  http_request(Request, Version, Host),
				  Expected ++ [{version, Version}]).

http_status(Request, HeadersAndBody, Config, Expected) ->
    Version = ?config(http_version, Config),
    Host = ?config(host, Config),    
    httpd_test_lib:verify_request(?config(type, Config), Host, 
				  ?config(port, Config),  ?config(node, Config),
				  http_request(Request, Version, Host, HeadersAndBody),
				  Expected ++ [{version, Version}]).

auth_status(AuthRequest, Config, Expected) ->
    Version = ?config(http_version, Config),
    Host = ?config(host, Config),    
    httpd_test_lib:verify_request(?config(type, Config), Host, 
				  ?config(port, Config),  ?config(node, Config),
				  AuthRequest,
				  Expected ++ [{version, Version}]).

basic_auth_requiered(Config) -> 
    ok = http_status("GET /open/ ", Config,  [{statuscode, 401},
					      {header, "WWW-Authenticate"}]),
    ok = http_status("GET /secret/ ", Config,  [{statuscode, 401},
						{header, "WWW-Authenticate"}]),
    ok = http_status("GET /secret/top_secret/ ", Config,  [{statuscode, 401},
						      {header, "WWW-Authenticate"}]).  

start_mnesia(Node) ->
    case rpc:call(Node, ?MODULE, cleanup_mnesia, []) of
	ok ->
	    ok;
	Other ->
	    ct:fail({failed_to_cleanup_mnesia, Other})
    end,
    case rpc:call(Node, ?MODULE, setup_mnesia, []) of
	{atomic, ok} ->
	    ok;
	Other2 ->
	    ct:fail({failed_to_setup_mnesia, Other2})
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
