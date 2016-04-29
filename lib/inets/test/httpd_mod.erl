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

-module(httpd_mod).

-include_lib("common_test/include/ct.hrl").

%% General testcases bodies called from httpd_SUITE
-export([alias/4, actions/4, security/5, auth/4, auth_api/6,
	 auth_mnesia_api/4, htaccess/4, 
	 cgi/4, esi/4, get/4, head/4, all/4]).

%% Help functions 
-export([event/4, ssl_password_cb/0]).

%% Seconds before successful auths timeout.
-define(AUTH_TIMEOUT,5).


%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
alias(Type, Port, Host, Node) ->
    %% This is very crude, but...
    Opts = [], 
    ok = httpd_test_lib:verify_request(Type, Host, Port, Opts, Node, 
 				       "GET /pics/icon.sheet.gif "
 				       "HTTP/1.0\r\n\r\n",
 				       [{statuscode, 200},
 					{header, "Content-Type","image/gif"},
 					{header, "Server"},
 					{header, "Date"},
 				        {version, "HTTP/1.0"}]),
    
    ok = httpd_test_lib:verify_request(Type, Host, Port, Opts, Node, 
 				       "GET / HTTP/1.0\r\n\r\n",
 				       [{statuscode, 200},
 					{header, "Content-Type","text/html"},
 					{header, "Server"},
 					{header, "Date"},
 				        {version, "HTTP/1.0"}]),

    ok = httpd_test_lib:verify_request(Type, Host, Port, Opts, Node, 
 				       "GET /misc/ HTTP/1.0\r\n\r\n",
 				       [{statuscode, 200},
 					{header, "Content-Type","text/html"},
 					{header, "Server"},
 					{header, "Date"},
 				        {version, "HTTP/1.0"}]),

    %% Check redirection if trailing slash is missing.
    ok = httpd_test_lib:verify_request(Type, Host, Port, Opts, Node, 
 				       "GET /misc HTTP/1.0\r\n\r\n",
 				       [{statuscode, 301},
 					{header, "Location"},
 					{header, "Content-Type","text/html"},
				        {version, "HTTP/1.0"}]).

%%-------------------------------------------------------------------------
actions(Type, Port, Host, Node) ->
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "HEAD / HTTP/1.0\r\n\r\n", 
				       [{statuscode, 200},
				        {version, "HTTP/1.0"}]).


%%-------------------------------------------------------------------------
security(ServerRoot, Type, Port, Host, Node) ->
   
    global:register_name(mod_security_test, self()),   % Receive events
   
    ct:sleep(5000),

    OpenDir = filename:join([ServerRoot, "htdocs", "open"]),

    %% Test blocking / unblocking of users.

    %% /open, require user one Aladdin
 
    remove_users(Node, ServerRoot, Host, Port, "open"),

    auth_request(Type, Host, Port, Node, "/open/", "one", "onePassword", 
		 [{statuscode, 401}]),

    receive_security_event({event, auth_fail, Port, OpenDir,
			    [{user, "one"}, {password, "onePassword"}]},
			   Node, Port),
    
    auth_request(Type,Host,Port,Node,"/open/", "two", "twoPassword",
		 [{statuscode, 401}]),
    
    receive_security_event({event, auth_fail, Port, OpenDir,
			    [{user, "two"}, {password, "twoPassword"}]},
			   Node, Port),
    auth_request(Type, Host, Port, Node,"/open/", "Aladdin", 
		 "AladdinPassword", [{statuscode, 401}]),

    receive_security_event({event, auth_fail, Port, OpenDir,
			    [{user, "Aladdin"},
			     {password, "AladdinPassword"}]},
			   Node, Port),
    add_user(Node, ServerRoot, Port, "open", "one", "onePassword", []),

    add_user(Node, ServerRoot, Port, "open", "two", "twoPassword", []),

    auth_request(Type, Host, Port, Node,"/open/", "one", "WrongPassword", 
		 [{statuscode, 401}]),
    receive_security_event({event, auth_fail, Port, OpenDir,
			    [{user, "one"}, {password, "WrongPassword"}]},
			   Node, Port),
    auth_request(Type, Host, Port, Node,"/open/", "one", "WrongPassword", 
		 [{statuscode, 401}]),

    receive_security_event({event, auth_fail, Port, OpenDir,
			    [{user, "one"}, {password, "WrongPassword"}]},
			   Node, Port),
        receive_security_event({event, user_block, Port, OpenDir,
			    [{user, "one"}]}, Node, Port),
    
    global:unregister_name(mod_security_test),   % No more events.

    auth_request(Type, Host, Port, Node,"/open/", "one", "WrongPassword", 
		 [{statuscode, 401}]),
    auth_request(Type, Host, Port, Node,"/open/", "one", "onePassword",
		 [{statuscode, 403}]),

    %% User "one" should be blocked now..
    case list_blocked_users(Node, Port) of
	[{"one",_, Port, OpenDir,_}] ->
	    ok;
	Blocked ->
	    exit({unexpected_blocked, Blocked})
    end,

    [{"one",_, Port, OpenDir,_}] = list_blocked_users(Node, Port, OpenDir),

    true = unblock_user(Node, "one", Port, OpenDir),
    %% User "one" should not be blocked any more.

    [] = list_blocked_users(Node, Port),

    auth_request(Type, Host, Port, Node,"/open/", "one", "onePassword", 
		 [{statuscode, 200}]),



    %% Test list_auth_users & auth_timeout
    ["one"] = list_auth_users(Node, Port),

    auth_request(Type, Host, Port, Node,"/open/", "two", "onePassword", 
		 [{statuscode, 401}]),
    ["one"] = list_auth_users(Node, Port),

    ["one"] = list_auth_users(Node, Port, OpenDir),

    auth_request(Type, Host, Port, Node,"/open/", "two", "twoPassword", 
		 [{statuscode, 401}]),

    ["one"] = list_auth_users(Node, Port),

    ["one"] = list_auth_users(Node, Port, OpenDir),

    %% Wait for successful auth to timeout.
    ct:sleep(?AUTH_TIMEOUT*1001),  

    [] = list_auth_users(Node, Port),


    [] = list_auth_users(Node, Port, OpenDir),

    %% "two" is blocked.

    true = unblock_user(Node, "two", Port, OpenDir),
    %% Test explicit blocking. Block user 'two'.

    [] = list_blocked_users(Node,Port,OpenDir),

    true = block_user(Node, "two", Port, OpenDir, 10),
    auth_request(Type, Host, Port, Node,"/open/", "two", "twoPassword", 
		 [{statuscode, 401}]).

%%-------------------------------------------------------------------------
auth(Type, Port, Host, Node) ->
 
    %% Authentication required!
    ok = httpd_test_lib:verify_request(Type,Host,Port,Node, 
				       "GET /open/ HTTP/1.0\r\n\r\n",
				       [{statuscode, 401},
					{version, "HTTP/1.0"},
					{header, "WWW-Authenticate"}]),
    ok = httpd_test_lib:verify_request(Type,Host,Port,Node, 
				       "GET /secret/ HTTP/1.0\r\n\r\n",
				       [{statuscode, 401},
					{version, "HTTP/1.0"}, 
					{header, "WWW-Authenticate"}]),
    ok = httpd_test_lib:verify_request(Type,Host,Port,Node, 
				       "GET /secret/top_secret/"
				       " HTTP/1.0\r\n\r\n",
				       [{statuscode, 401},
					{version, "HTTP/1.0"}, 
					{header, "WWW-Authenticate"}]),
 
    %% Authentication OK! ["one:OnePassword" user first in user list]
    auth_request(Type, Host, Port, Node, "/open/dummy.html", "one", 
		 "onePassword", [{statuscode, 200}]),
    %% Authentication OK and a directory listing is supplied!
    %% ["Aladdin:open sesame" user second in user list]
    auth_request(Type, Host, Port, Node, "/open/","Aladdin", 
		 "AladdinPassword", [{statuscode, 200}]),
    
    %% User correct but wrong password! ["one:one" user first in user list]
    auth_request(Type, Host, Port, Node, "/open/", "one", "one", 
		 [{statuscode, 401},{header, "WWW-Authenticate"}]),
    %% Make sure Authenticate header is received even the second time
    %% we try a incorrect password! Otherwise a browser client will hang!
    auth_request(Type, Host, Port, Node, "/open/", "one", "one", 
		 [{statuscode, 401},{header, "WWW-Authenticate"}]),

    %% Neither user or password correct! ["dummy:dummy"]
    auth_request(Type, Host, Port, Node, "/open/", "dummy", "dummy", 
		 [{statuscode, 401}]),
    
    %% Authentication OK! ["two:TwoPassword" user in first group]
    auth_request(Type, Host, Port, Node, "/secret/dummy.html", "two", 
		 "twoPassword", [{statuscode, 200}]),
    %% Authentication OK and a directory listing is supplied!
    %% ["three:ThreePassword" user in second group]
    auth_request(Type, Host, Port, Node,"/secret/", "three", 
		 "threePassword", [{statuscode, 200}]),
 
    %% User correct but wrong password! ["two:two" user in first group]
    auth_request(Type, Host, Port, Node, "/secret/", "two", "two", 
		 [{statuscode, 401}]),
    %% Neither user or password correct! ["dummy:dummy"]
    auth_request(Type, Host, Port, Node,"/secret/", "dummy", "dummy", 
		 [{statuscode, 401}]),

    %% Nested secret/top_secret OK! ["Aladdin:open sesame"]
    auth_request(Type, Host, Port, Node, "/secret/top_secret/", "Aladdin", 
		 "AladdinPassword", [{statuscode, 200}]),
    %% Authentication still required!
    ok =  httpd_test_lib:verify_request(Type, Host, Port, Node, "GET /open/ "
					"HTTP/1.0\r\n\r\n",
					[{statuscode, 401},
					 {version, "HTTP/1.0"}, 
					 {header, "WWW-Authenticate"}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, "GET /secret/ "
				       "HTTP/1.0\r\n\r\n",
				       [{statuscode, 401},
					{version, "HTTP/1.0"}, 
					{header, "WWW-Authenticate"}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET /secret/top_secret/ "
				       "HTTP/1.0\r\n\r\n",
				       [{statuscode, 401},
					{version, "HTTP/1.0"}, 
					{header, "WWW-Authenticate"}]).


%%-------------------------------------------------------------------------
%% What to test here:
%%
%% /open                      - plain,  require user one Aladdin
%% /secret                    - plain,  require group group1 group2
%% /secret/top_secret         - plain,  require group group3
%% /dets_open                 - dets,   require user one Aladdin
%% /dets_secret               - dets,   require group group1 group2
%% /dets_secret/top_secret    - dets,   require group group3
%% /mnesia_open/              - mnesia, require user one Aladdin
%% /mnesia_secret/            - mnesia, require group group1 group2
%% /mnesia_secret/top_secret/ - mnesia, require group group3
auth_api(ServerRoot, AuthStoreType, Type, Port, Host, Node) ->
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET / HTTP/1.0\r\n\r\n",
				       [{statuscode, 200},
				        {version, "HTTP/1.0"}]),
    auth_request(Type, Host, Port, Node, "/", "one", "WrongPassword", 
		 [{statuscode, 200}]),

    %% Make sure Authenticate header is received even the second time
    %% we try a incorrect password! Otherwise a browser client will hang!
    auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "open/",
		 "dummy", "WrongPassword", [{statuscode, 401},
					    {header, "WWW-Authenticate"}]),
    auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "open/",
		 "dummy", "WrongPassword", [{statuscode, 401},	
					    {header, "WWW-Authenticate"}]),
    
    %% Change the password to DummyPassword then try to add a user 
    %% Get an error and set it to NoPassword
    ok = update_password(Node, ServerRoot, Host, Port, AuthStoreType ++ 
			 "open", "NoPassword", "DummyPassword"),
    {error,bad_password} = 
	add_user(Node, ServerRoot, Port, AuthStoreType ++ "open", "one", 
		 "onePassword", []),
    ok = update_password(Node, ServerRoot, Host, Port, AuthStoreType ++"open",
			 "DummyPassword", "NoPassword"),
  
    %% Test /*open, require user one Aladdin
    remove_users(Node, ServerRoot, Host, Port, AuthStoreType ++ "open"),

    auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "open/",
		 "one", "onePassword", [{statuscode, 401}]),
    
    auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "open/",
		 "two", "twoPassword", [{statuscode, 401}]),
 
    auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "open/", 
		 "Aladdin", "onePassword", [{statuscode, 401}]),

    add_user(Node, ServerRoot, Port, AuthStoreType ++ "open", "one", 
	     "onePassword", []),
    add_user(Node, ServerRoot, Port, AuthStoreType ++ "open", "two", 
	     "twoPassword", []),
    add_user(Node, ServerRoot, Port, AuthStoreType ++ "open", "Aladdin", 
	     "AladdinPassword", []),
   
    {ok, [_|_]} = list_users(Node, ServerRoot, Host, Port, 
			  AuthStoreType++"open"),
    auth_request(Type, Host, Port, Node, "/" ++ AuthStoreType ++ "open/",
		 "one", "WrongPassword", [{statuscode, 401}]),
    auth_request(Type, Host, Port, Node, "/" ++ AuthStoreType ++ "open/", 
		 "one", "onePassword", [{statuscode, 200}]),
    auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "open/", 
		 "two", "twoPassword", [{statuscode, 401}]),
    auth_request(Type, Host, Port, Node, "/" ++ AuthStoreType ++ "open/", 
		 "Aladdin", "WrongPassword", [{statuscode, 401}]),
    auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "open/", 
		 "Aladdin", "AladdinPassword", [{statuscode, 200}]),
   
    remove_users(Node, ServerRoot, Host, Port, AuthStoreType++"open"),
    {ok, []} = list_users(Node, ServerRoot, Host, Port, 
			  AuthStoreType++"open"),

    %% Phase 2
    remove_users(Node, ServerRoot, Host, Port, AuthStoreType++"secret"),
    {ok, []} = list_users(Node, ServerRoot, Host, Port, AuthStoreType ++
			  "secret"),
    auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "secret/",
		 "one", "onePassword", [{statuscode, 401}]),
    auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "secret/", 
		 "two", "twoPassword", [{statuscode, 401}]),
    auth_request(Type, Host, Port,  Node, "/" ++ AuthStoreType ++ "secret/", 
		 "three", "threePassword", [{statuscode, 401}]),
    add_user(Node, ServerRoot, Port, AuthStoreType ++ "secret", "one",
	     "onePassword", 
	     []),
    add_user(Node, ServerRoot, Port, AuthStoreType ++ "secret", 
	     "two", "twoPassword", []),
    add_user(Node, ServerRoot, Port, AuthStoreType++"secret", "Aladdin", 
	     "AladdinPassword",[]),
    add_group_member(Node, ServerRoot, Port, AuthStoreType ++ "secret", 
		     "one", "group1"),
    add_group_member(Node, ServerRoot, Port, AuthStoreType ++ "secret", 
		     "two", "group1"),
    add_group_member(Node, ServerRoot, Port, AuthStoreType ++ 
		     "secret", "Aladdin", "group2"),
    auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "secret/",
		 "one", "onePassword", [{statuscode, 200}]),
    auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "secret/", 
		 "two", "twoPassword", [{statuscode, 200}]),
    auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "secret/",
		 "Aladdin", "AladdinPassword", [{statuscode, 200}]),
    auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ "secret/",
		 "three", "threePassword", [{statuscode, 401}]),
    remove_users(Node, ServerRoot, Host, Port, AuthStoreType ++ "secret"),
    {ok, []} = list_users(Node, ServerRoot, Host, Port, 
			  AuthStoreType ++ "secret"),
    remove_groups(Node, ServerRoot, Host, Port, AuthStoreType ++ "secret"),
    Directory = filename:join([ServerRoot, "htdocs", AuthStoreType ++ 
			       "secret"]),
    {ok, []} = list_groups(Node, ServerRoot, Host, Port, Directory),

    %% Phase 3
    remove_users(Node, ServerRoot, Host, Port, AuthStoreType ++ 
		 "secret/top_secret"),
    remove_groups(Node, ServerRoot, Host, Port, AuthStoreType ++ 
		  "secret/top_secret"),
    auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ 
		 "secret/top_secret/",
		 "three", "threePassword", [{statuscode, 401}]),
    auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ 
		 "secret/top_secret/", "two", "twoPassword", 
		 [{statuscode, 401}]),
    add_user(Node, ServerRoot, Port, AuthStoreType ++ 
	     "secret/top_secret","three",
	     "threePassword",[]),
    add_user(Node, ServerRoot, Port, AuthStoreType ++ "secret/top_secret",
	     "two","twoPassword", []),
    add_group_member(Node, ServerRoot, Port, AuthStoreType ++ 
		     "secret/top_secret",
		     "three", "group3"),
    auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ 
		 "secret/top_secret/", "three", "threePassword", 
		 [{statuscode, 200}]),
    auth_request(Type, Host, Port, Node,"/" ++ AuthStoreType ++ 
		 "secret/top_secret/", "two", "twoPassword", 
		 [{statuscode, 401}]),
    add_group_member(Node, ServerRoot, Port, AuthStoreType ++ 
		     "secret/top_secret",
		     "two", "group3"),
    auth_request(Type,Host,Port,Node,"/" ++ AuthStoreType ++ 
		 "secret/top_secret/",
		 "two", "twoPassword", [{statuscode, 200}]),
    remove_users(Node, ServerRoot, Host, Port, AuthStoreType ++ 
		 "secret/top_secret"),
    {ok, []} = list_users(Node, ServerRoot, Host, Port, 
			  AuthStoreType ++ "secret/top_secret"),
    remove_groups(Node, ServerRoot, Host, Port, AuthStoreType ++ 
		  "secret/top_secret"),
    Directory2 = filename:join([ServerRoot, "htdocs", 
				AuthStoreType ++ "secret/top_secret"]),
    {ok, []} = list_groups(Node, ServerRoot, Host, Port, Directory2),
    auth_request(Type, Host, Port, Node, "/" ++ AuthStoreType ++ 
		 "secret/top_secret/", "two", "twoPassword", 
		 [{statuscode, 401}]),
    auth_request(Type, Host, Port, Node, "/" ++ AuthStoreType ++ 
		 "secret/top_secret/","three", "threePassword",
		 [{statuscode, 401}]).

%%--------------------------------------------------------------------------
auth_mnesia_api(_Type, Port, _Host, _Node) ->
    %% Create three groups:
    %% group1 : one Aladdin
    %% group2 : two
    %% group3 : three
    mod_auth_mnesia:store_user("one", "onePassword", Port, 
			       "/mnesia_open", ""),
    mod_auth_mnesia:store_user("Aladdin", "AladdinPassword", Port, 
			       "/mnesia_open", ""),
    mod_auth_mnesia:store_user("two", "twoPassword", Port, 
			       "/mnesia_open", ""),
    mod_auth_mnesia:store_user("three", "threePassword", Port, 
				     "/mnesia_open", ""),
    Users = mod_auth_mnesia:list_users(Port, "/mnesia_open"),

    ok = check_lists_members(Users,["Aladdin","one","two","three"]),
    
    true = mod_auth_mnesia:store_group_member("group1", "one", Port,
					      "/mnesia_open", ""),
    true = mod_auth_mnesia:store_group_member("group1","Aladdin", Port,
					      "/mnesia_open", ""),
    true = mod_auth_mnesia:store_group_member("group2","two", Port,
					      "/mnesia_open", ""),
    true = mod_auth_mnesia:store_group_member("group3","three", Port, 
					      "/mnesia_open", ""),
    %% Check that all three created groups exist.
    Groups = mod_auth_mnesia:list_groups(Port, "/mnesia_open"),
    ok = check_lists_members(Groups, ["group1","group2","group3"]),

    %% Check that the members of all groups are correct.
    Group1 = mod_auth_mnesia:list_group_members("group1", Port, 
						"/mnesia_open"),
    ok = check_lists_members(Group1,["one","Aladdin"]),
    {ok,["two"]}  = mod_auth_mnesia:list_group_members("group2", Port, 
						"/mnesia_open"),
   
    {ok,["three"]} = mod_auth_mnesia:list_group_members("group3", Port, 
						       "/mnesia_open"),
    
    %% Delete user 'one' from group one and check that he was removed
    %% correctly.
    true = mod_auth_mnesia:remove_group_member("group1", "one", Port, 
					       "/mnesia_open", ""),
    {ok,["Aladdin"]}  = mod_auth_mnesia:list_group_members("group1", Port, 
							   "/mnesia_open"),
    
    %% Remove group1 and check that the group was removed correctly.
    true = mod_auth_mnesia:remove_group("group1", Port, "/mnesia_open", ""),
    Groups_1 = mod_auth_mnesia:list_groups(Port, "/mnesia_open"),
    ok = check_lists_members(Groups_1,["group2","group3"]),
    
    %% Check that the other users still exist in their groups.
    Users_1 = mod_auth_mnesia:list_users(Port, "/mnesia_open"),
    ok = check_lists_members(Users_1,["Aladdin","one","two","three"]),
    {ok,["two"]} = mod_auth_mnesia:list_group_members("group2", Port, 
						  "/mnesia_open"),
    {ok,["three"]} = mod_auth_mnesia:list_group_members("group3", Port,
						  "/mnesia_open"),
    
    %% Remove the remaining groups/users and check that all
    %% users/groups are removed.
    true = mod_auth_mnesia:remove_group("group2", Port, "/mnesia_open", ""),
    true = mod_auth_mnesia:remove_group("group3", Port, "/mnesia_open", ""),
    {ok, []} = mod_auth_mnesia:list_groups(Port, "/mnesia_open"),
    true = mod_auth_mnesia:remove_user("one", Port, "/mnesia_open", ""),
    true = mod_auth_mnesia:remove_user("Aladdin", Port, "/mnesia_open", ""),
    true = mod_auth_mnesia:remove_user("two", Port, "/mnesia_open", ""),
    true = mod_auth_mnesia:remove_user("three", Port, "/mnesia_open", ""),
    {ok, []} = mod_auth_mnesia:list_users(Port, "/mnesia_open"),
    ok.
%%--------------------------------------------------------------------------
htaccess(Type, Port, Host, Node) ->
    %% Control that authentication required!
    %% Control that the pages that shall be 
    %% authenticated really need authenticatin
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /ht/open/ HTTP/1.0\r\n\r\n",
				       [{statuscode, 401},
					{version, "HTTP/1.0"}, 
					{header, "WWW-Authenticate"}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /ht/secret/ HTTP/1.0\r\n\r\n",
				       [{statuscode, 401},
					{version, "HTTP/1.0"}, 
					{header, "WWW-Authenticate"}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /ht/secret/top_secret/ "
				       "HTTP/1.0\r\n\r\n",
				       [{statuscode, 401},
					{version, "HTTP/1.0"}, 
					{header, "WWW-Authenticate"}]),

    %% Make sure Authenticate header is received even the second time
    %% we try a incorrect password! Otherwise a browser client will hang!
    auth_request(Type, Host, Port, Node,"/ht/open/",
		 "dummy", "WrongPassword", [{statuscode, 401},
					    {header, "WWW-Authenticate"}]),
    auth_request(Type, Host, Port, Node,"/ht/open/",
		 "dummy", "WrongPassword", [{statuscode, 401},		
					    {header, "WWW-Authenticate"}]),
    
    %% Control that not just the first user in the list is valid
    %% Control the first user
    %% Authennticating ["one:OnePassword" user first in user list]
    auth_request(Type, Host, Port, Node, "/ht/open/dummy.html", "one", 
		 "OnePassword", [{statuscode, 200}]),
    
    %% Control the second user
    %% Authentication OK and a directory listing is supplied! 
    %% ["Aladdin:open sesame" user second in user list]
    auth_request(Type, Host, Port, Node, "/ht/open/","Aladdin", 
		 "AladdinPassword", [{statuscode, 200}]),
    
    %% Contro that bad passwords and userids get a good denial
    %% User correct but wrong password! ["one:one" user first in user list]
    auth_request(Type, Host, Port, Node, "/ht/open/", "one", "one", 
		 [{statuscode, 401}]),
    %% Neither user or password correct! ["dummy:dummy"]
    auth_request(Type, Host, Port, Node, "/ht/open/", "dummy", "dummy", 
		 [{statuscode, 401}]),
    
    %% Control that authetication still works, even if its a member in a group
    %% Authentication OK! ["two:TwoPassword" user in first group]
    auth_request(Type, Host, Port, Node, "/ht/secret/dummy.html", "two", 
		 "TwoPassword", [{statuscode, 200}]),
    
    %% Authentication OK and a directory listing is supplied! 
    %% ["three:ThreePassword" user in second group]
    auth_request(Type, Host, Port, Node,"/ht/secret/", "three",
		 "ThreePassword", [{statuscode, 200}]),
    
    %% Deny users with bad passwords even if the user is a group member
    %% User correct but wrong password! ["two:two" user in first group]
    auth_request(Type, Host, Port, Node, "/ht/secret/", "two", "two", 
		 [{statuscode, 401}]),
    %% Neither user or password correct! ["dummy:dummy"]
    auth_request(Type, Host, Port, Node,"/ht/secret/", "dummy", "dummy", 
		 [{statuscode, 401}]),

    %% control that we deny the users that are in subnet above the allowed
    auth_request(Type, Host, Port, Node,"/ht/blocknet/dummy.html", "four",
		 "FourPassword", [{statuscode, 403}]),
    %% Control that we only applies the rules to the right methods
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "HEAD /ht/blocknet/dummy.html"
				       " HTTP/1.0\r\n\r\n",
				       [{statuscode, 200},
					{version, "HTTP/1.0"}]),
    
    %% Control that the rerquire directive can be overrideen
    auth_request(Type, Host, Port, Node, 
		 "/ht/secret/top_secret/", "Aladdin", "AladdinPassword", 
		 [{statuscode, 401}]),
    
    %% Authentication still required!
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, "GET /ht/open/ "
				       "HTTP/1.0\r\n\r\n",
				       [{statuscode, 401},
					{version, "HTTP/1.0"}, 
					{header, "WWW-Authenticate"}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET /ht/secret/ HTTP/1.0\r\n\r\n",
				       [{statuscode, 401},
					{version, "HTTP/1.0"},    
					{header, "WWW-Authenticate"}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET /ht/secret/top_secret/ "
				       "HTTP/1.0\r\n\r\n",
				       [{statuscode, 401},
					{version, "HTTP/1.0"}, 
					{header, "WWW-Authenticate"}]).
%%--------------------------------------------------------------------
cgi(Type, Port, Host, Node) ->
    {Script, Script2, Script3} =
	case test_server:os_type() of
	    {win32, _} ->
		{"printenv.bat", "printenv.sh", "cgi_echo.exe"};
	    _ ->
		{"printenv.sh", "printenv.bat", "cgi_echo"}
	end,

    %% The length (> 100) is intentional
    ok = httpd_test_lib:
	verify_request(Type, Host, Port, Node, 
		       "POST /cgi-bin/" ++ Script3 ++
		       " HTTP/1.0\r\n"
		       "Content-Length:100 \r\n\r\n "
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
		       "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
		       " \r\n\r\n",
		       [{statuscode, 200},
			{version, "HTTP/1.0"},
			{header, "content-type", "text/plain"}]),
    
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET /cgi-bin/"++ Script ++
				       " HTTP/1.0\r\n\r\n", 
				       [{statuscode, 200},
				       {version, "HTTP/1.0"}]),

    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET /cgi-bin/not_there "
				       "HTTP/1.0\r\n\r\n", 
				       [{statuscode, 404},{statuscode, 500},
				       {version, "HTTP/1.0"}]),

    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET /cgi-bin/"++ Script ++
				       "?Nisse:kkk?sss/lll HTTP/1.0\r\n\r\n", 
				       [{statuscode, 200}, 
					{version, "HTTP/1.0"}]),

    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "POST /cgi-bin/"++ Script ++
				       " HTTP/1.0\r\n\r\n", 
				       [{statuscode, 200},
					{version, "HTTP/1.0"}]),

    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET /htbin/"++ Script ++
				       " HTTP/1.0\r\n\r\n",
				       [{statuscode, 200},
				       {version, "HTTP/1.0"}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET /htbin/not_there "
				       "HTTP/1.0\r\n\r\n", 
				       [{statuscode, 404},{statuscode, 500},
				        {version, "HTTP/1.0"}]),

    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET /htbin/"++ Script ++
				       "?Nisse:kkk?sss/lll HTTP/1.0\r\n\r\n", 
				       [{statuscode, 200},
					{version, "HTTP/1.0"}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "POST /htbin/"++ Script ++
				       " HTTP/1.0\r\n\r\n",
				       [{statuscode, 200},
				       {version, "HTTP/1.0"}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "POST /htbin/"++ Script ++
				       " HTTP/1.0\r\n\r\n",
				       [{statuscode, 200},
				       {version, "HTTP/1.0"}]),
    
    %% Execute an existing, but bad CGI script..
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "POST /htbin/"++ Script2 ++
				       " HTTP/1.0\r\n\r\n",
				       [{statuscode, 404},
					{version, "HTTP/1.0"}]),
    
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "POST /cgi-bin/"++ Script2 ++
				       " HTTP/1.0\r\n\r\n",
				       [{statuscode, 404},
				       {version, "HTTP/1.0"}]),

    %% Check "ScriptNoCache" directive (default: false)
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /cgi-bin/" ++ Script ++
				       " HTTP/1.0\r\n\r\n",
				       [{statuscode, 200},
					{no_header, "cache-control"},
					{version, "HTTP/1.0"}]).

%%--------------------------------------------------------------------
esi(Type, Port, Host, Node) ->
    %% Check "ErlScriptAlias" and "EvalScriptAlias" directives
     ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
 				       "GET /eval?httpd_example:print(\"Hi!\")"
 				       " HTTP/1.0\r\n\r\n", 
 				       [{statuscode, 200},
 				       {version, "HTTP/1.0"}]),
     ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
 				       "GET /eval?not_allowed:print(\"Hi!\")"
 				       " HTTP/1.0\r\n\r\n",
 				       [{statuscode, 403},
 				       {version, "HTTP/1.0"}]),
     ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
 				       "GET /eval?httpd_example:undef(\"Hi!\")"
 				       " HTTP/1.0\r\n\r\n",
 				       [{statuscode, 500},
 					{version, "HTTP/1.0"}]),
     ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
 				       "GET /cgi-bin/erl/httpd_example "
  				       "HTTP/1.0\r\n\r\n",
  				       [{statuscode, 400},
 					{version, "HTTP/1.0"}]),
     ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
 				       "GET /cgi-bin/erl/httpd_example:get "
 				       "HTTP/1.0\r\n\r\n", 
 				       [{statuscode, 200},
 				       {version, "HTTP/1.0"}]),
     ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
 				       "GET /cgi-bin/erl/httpd_example:"
 				       "get?input=4711"
 				       " HTTP/1.0\r\n\r\n", 
 				       [{statuscode, 200},
 				       {version, "HTTP/1.0"}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /cgi-bin/erl/httpd_example:"
				       "post HTTP/1.0\r\n\r\n",
				       [{statuscode, 200},
					{version, "HTTP/1.0"}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /cgi-bin/erl/not_allowed:post "
				       "HTTP/1.0\r\n\r\n",  
				       [{statuscode, 403},
				       {version, "HTTP/1.0"}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /cgi-bin/erl/httpd_example:undef "
				       "HTTP/1.0\r\n\r\n",
				       [{statuscode, 404},
				       {version, "HTTP/1.0"}]),
    ok =  httpd_test_lib:verify_request(Type, Host, Port, Node,
					"GET /cgi-bin/erl/httpd_example/yahoo"
					" HTTP/1.0\r\n\r\n",
					[{statuscode, 302},
					{version, "HTTP/1.0"}]),
    %% Check "ErlScriptNoCache" directive (default: false)
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /cgi-bin/erl/httpd_example:get"
				       " HTTP/1.0\r\n\r\n",
				       [{statuscode, 200},
					{no_header, "cache-control"},
					{version, "HTTP/1.0"}]),
    ok.


%%--------------------------------------------------------------------
get(Type, Port, Host, Node) ->
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /index.html HTTP/1.0\r\n\r\n",
				       [{statuscode, 200},
					{header, "Content-Type", "text/html"},
					{header, "Date"},
					{header, "Server"},
					{version, "HTTP/1.0"}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /fsize.shtml HTTP/1.1\r\nHost:" 
				       ++ Host ++ "\r\n\r\n", 
				       [{statuscode, 200},
					{header, "Content-Type", "text/html"},
					{header, "Date"},
					{header, "Server"}]),
    ok =  httpd_test_lib:verify_request(Type, Host, Port, Node,
					"GET /fsize.shtml HTTP/1.0\r\n\r\n", 
					[{statuscode, 200},
					 {header, "Content-Type"},
					 {header, "Server"},
					 {header, "Date"},
					{version, "HTTP/1.0"}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /secret/dummy.html "
				       "HTTP/1.0\r\n\r\n", 
				       [{statuscode, 401},
					{header, "WWW-Authenticate"},
					{version, "HTTP/1.0"}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /index.html HTTP/1.0\r\n\r\n", 
				       [{statuscode, 200},
					{header, "Server"},
					{header, "Date"},
					{header, "Content-Type", 
					 "text/html"},
				       {version, "HTTP/1.0"}]),
    ok.

%%--------------------------------------------------------------------
head(Type, Port, Host, Node) ->
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "HEAD /index.html HTTP/1.0\r\n\r\n",
				       [{statuscode, 200},
					{version, "HTTP/1.0"}]),
    ok.
%%--------------------------------------------------------------------
all(Type, Port, Host, Node) ->
    actions(Type, Port, Host, Node),
    alias(Type, Port, Host, Node),
    auth(Type, Port, Host, Node),
    cgi(Type, Port, Host, Node),
    esi(Type, Port, Host, Node),
    get(Type, Port, Host, Node),
    head(Type, Port, Host, Node),
    ok.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
auth_request(Type, Host, Port, Node, URI, User, Passwd, Expect) ->
    Req = ["GET ", URI, " HTTP/1.0\r\n", 
	   "Authorization: Basic ", 
	   base64:encode_to_string(User++":"++Passwd),
	   "\r\n\r\n"],
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       lists:flatten(Req), 
				       [{version, "HTTP/1.0"} | Expect]).

remove_users(Node, ServerRoot, Host, Port, Dir) ->
    %% List users, delete them, and make sure they are gone.
    case list_users(Node, ServerRoot, Host, Port, Dir) of
	{ok, Users} ->
	    lists:foreach(fun(User) -> 
				  delete_user(Node, ServerRoot, Host, 
					      Port, Dir, User)
			  end,
			  Users),
		  {ok, []} = list_users(Node, ServerRoot, Host, Port, Dir);
	_ ->
	    ok
    end.

add_user(Node, Root, Port, Dir, User, Password, UserData) ->
    Addr = undefined, 
    Directory = filename:join([Root, "htdocs", Dir]),
    rpc:call(Node, mod_auth, add_user, 
	     [User, Password, UserData, Addr, Port, Directory]).

delete_user(Node, Root, _Host, Port, Dir, User) ->
    Addr = undefined, 
    Directory = filename:join([Root, "htdocs", Dir]),
    rpc:call(Node, mod_auth, delete_user, [User, Addr, Port, Directory]).

list_users(Node, Root, _Host, Port, Dir) ->
    Addr = undefined, 
    Directory = filename:join([Root, "htdocs", Dir]),
    rpc:call(Node, mod_auth, list_users, [Addr, Port, Directory]).


receive_security_event(Event, Node, Port) ->
    receive 
	Event ->
	    ok;
	{'EXIT', _, _} ->
	    receive_security_event(Event, Node, Port)
    after 5000 ->
	    %% Flush the message queue, to see if we got something...
	    Msgs = inets_test_lib:flush(),
	    ct:fail({expected_event_not_received, Msgs})
				     
    end.

%% receive_security_event(Event, Node, Port) ->
%%     io:format(user, "~w:receive_security_event -> entry with"
%% 	      "~n   Event: ~p"
%% 	      "~n   Node:  ~p"
%% 	      "~n   Port:  ~p"
%% 	      "~n", [?MODULE, Event, Node, Port]),
%%     receive 
%% 	Event ->
%% 	    ok;
%% 	{'EXIT', _, _} ->
%% 	    receive_security_event(Event, Node, Port);	
%% 	Other ->
%% 	    ct:fail({unexpected_event, 
%% 			      {expected, Event}, {received, Other}})
%%     after 5000 ->
%% 	    ct:fail(no_event_recived)
				     
%%     end.

list_blocked_users(Node,Port) ->
    Addr = undefined, % Assumed to be on the same host
    rpc:call(Node, mod_security, list_blocked_users, [Addr,Port]).

list_blocked_users(Node,Port,Dir) ->
    Addr = undefined, % Assumed to be on the same host
    rpc:call(Node, mod_security, list_blocked_users, [Addr,Port,Dir]).

block_user(Node,User,Port,Dir,Sec) ->
    Addr = undefined, % Assumed to be on the same host
    rpc:call(Node, mod_security, block_user, [User, Addr, Port, Dir, Sec]).

unblock_user(Node,User,Port,Dir) ->
    Addr = undefined, % Assumed to be on the same host
    rpc:call(Node, mod_security, unblock_user, [User, Addr, Port, Dir]).

list_auth_users(Node,Port) ->
    Addr = undefined, % Assumed to be on the same host
    rpc:call(Node, mod_security, list_auth_users, [Addr,Port]).

list_auth_users(Node,Port,Dir) ->
    Addr = undefined, % Assumed to be on the same host
    rpc:call(Node, mod_security, list_auth_users, [Addr,Port,Dir]).

update_password(Node, ServerRoot, _Address, Port, Dir, Old, New)->
    Directory = filename:join([ServerRoot, "htdocs", Dir]),
    rpc:call(Node, mod_auth, update_password, 
	     [undefined, Port, Directory, Old, New, New]).

remove_groups(Node, ServerRoot, Host, Port, Dir) ->
    Directory = filename:join([ServerRoot, "htdocs", Dir]),
    {ok, Groups} = list_groups(Node, ServerRoot, Host, Port, Directory),
    lists:foreach(fun(Group) ->
				delete_group(Node, Group, Port, Directory)
			end,
			Groups),
    {ok, []} = list_groups(Node, ServerRoot, Host, Port, Directory),
    ok.

delete_group(Node, Group, Port, Dir) ->
    Addr = undefined, 
    rpc:call(Node, mod_auth, delete_group, [Group, Addr, Port, Dir]).

list_groups(Node, _, _, Port, Dir) ->
    Addr = undefined, 
    rpc:call(Node, mod_auth, list_groups, [Addr, Port, Dir]).

add_group_member(Node, ServerRoot, Port, Dir, User, Group) ->
    Addr = undefined, 
    rpc:call(Node, mod_auth, add_group_member, [Group, User, Addr, Port, 
						filename:join(
						  [ServerRoot,
						   "htdocs",Dir])]).
event(What, Port, Dir, Data) ->
    Msg = {event, What, Port, Dir, Data},
    case global:whereis_name(mod_security_test) of
	undefined ->
	    ok;
	_Pid ->
	    global:send(mod_security_test, Msg)
    end.

ssl_password_cb() ->
    "dummy-ssl-password".

check_lists_members({ok,L},L) -> 
    ok;
check_lists_members({ok,L1},L2) ->
    check_lists_members1(lists:sort(L1),lists:sort(L2));
check_lists_members(Error,_L) ->
    Error.

check_lists_members1(L,L) ->
    ok;
check_lists_members1(L1,L2) ->
    {error,{lists_not_equal,L1,L2}}.



