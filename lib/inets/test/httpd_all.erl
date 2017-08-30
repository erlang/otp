alias(Version, Type, Port, Host, Node) ->
    Opts = [], 
    ok = httpd_test_lib:verify_request(Type, Host, Port, Opts, Node, 
 				       "GET /pics/icon.sheet.gif "
 				       ++ Version ++ "\r\n\r\n",
 				       [{statuscode, 200},
 					{header, "Content-Type","image/gif"},
 					{header, "Server"},
 					{header, "Date"},
 				        {version, Version}]),
    
    ok = httpd_test_lib:verify_request(Type, Host, Port, Opts, Node, 
 				       "GET / " ++  Version ++ "\r\n\r\n",
 				       [{statuscode, 200},
 					{header, "Content-Type","text/html"},
 					{header, "Server"},
 					{header, "Date"},
 				        {version, Version}]),

    ok = httpd_test_lib:verify_request(Type, Host, Port, Opts, Node, 
 				       "GET /misc/ " ++ Version ++ "\r\n\r\n",
 				       [{statuscode, 200},
 					{header, "Content-Type","text/html"},
 					{header, "Server"},
 					{header, "Date"},
 				        {version, Version}]),

    %% Check redirection if trailing slash is missing.
    ok = httpd_test_lib:verify_request(Type, Host, Port, Opts, Node, 
 				       "GET /misc "++ Version ++ "\r\n\r\n",
 				       [{statuscode, 301},
 					{header, "Location"},
 					{header, "Content-Type","text/html"},
				        {version, Version}]).


head(Version, Type, Port, Host, Node) ->
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "HEAD /index.html " ++ Version ++ "\r\n\r\n",
				       [{statuscode, 200},
					{version, Version}]).


get(Version, Type, Port, Host, Node) ->
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /index.html " ++ Version ++ "\r\n\r\n",
				       [{statuscode, 200},
					{header, "Content-Type", "text/html"},
					{header, "Date"},
					{header, "Server"},
					{version, Version}]),

    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /fsize.shtml " ++ Version ++ "\r\nHost:" 
				       ++ Host ++ "\r\n\r\n", 
				       [{statuscode, 200},
					{header, "Content-Type", "text/html"},
					{header, "Date"},
					{header, "Server"}]),
    
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /secret/dummy.html "
				       ++ Version  ++ "\r\n\r\n", 
				       [{statuscode, 401},
					{header, "WWW-Authenticate"},
					{version, Version}]).

esi(Version, Type, Port, Host, Node) ->
    %% Check "ErlScriptAlias" and "EvalScriptAlias" directives
     ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
					"GET /eval?httpd_example:print(\"Hi!\") "
 				        ++ Version ++ "\r\n\r\n", 
					[{statuscode, 200},
					 {version, Version}]),
     ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
 				       "GET /eval?not_allowed:print(\"Hi!\") "
					++ Version ++ "\r\n\r\n",
					[{statuscode, 403},
					 {version, Version}]),
     ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
 				       "GET /eval?httpd_example:undef(\"Hi!\") "
 				       ++ Version ++ "\r\n\r\n",
 				       [{statuscode, 500},
 					{version, Version}]),
     ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
					"GET /cgi-bin/erl/httpd_example "
					++ Version ++ "\r\n\r\n",
  				       [{statuscode, 400},
 					{version, Version}]),
     ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
					"GET /cgi-bin/erl/httpd_example:get "
					++ Version  ++ "\r\n\r\n", 
					[{statuscode, 200},
					 {version, Version}]),
     ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
 				       "GET /cgi-bin/erl/httpd_example:"
 				       "get?input=4711"
 				       " HTTP/1.0\r\n\r\n", 
 				       [{statuscode, 200},
 				       {version, "HTTP/1.0"}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /cgi-bin/erl/httpd_example:"
				       "post " ++ Version ++ "\r\n\r\n",
				       [{statuscode, 200},
					{version, Version}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /cgi-bin/erl/not_allowed:post "
				       ++ Version ++ "\r\n\r\n",  
				       [{statuscode, 403},
					{version, Version}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /cgi-bin/erl/httpd_example:undef "
				       ++ Version ++ "\r\n\r\n",
				       [{statuscode, 404},
					{version, Version}]),
    ok =  httpd_test_lib:verify_request(Type, Host, Port, Node,
					"GET /cgi-bin/erl/httpd_example/yahoo "
					++ Version ++ "\r\n\r\n",
					[{statuscode, 302},
					{version, Version}]),
    %% Check "ErlScriptNoCache" directive (default: false)
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node,
				       "GET /cgi-bin/erl/httpd_example:get "
				       ++ Version ++ "\r\n\r\n",
				       [{statuscode, 200},
					{no_header, "cache-control"},
					{version, "HTTP/1.0"}]).

cgi(Version, Type, Port, Host, Node) ->
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
			   Version ++ " \r\n"
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
			{version, Version},
			{header, "content-type", "text/plain"}]),
    
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET /cgi-bin/"++ Script ++
					   " " ++ Version ++ "\r\n\r\n", 
				       [{statuscode, 200},
					{version, Version}]),
    
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET /cgi-bin/not_there " ++
					   Version ++ "\r\n\r\n", 
				       [{statuscode, 404},{statuscode, 500},
					{version, Version}]),

    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET /cgi-bin/"++ Script ++
					   "?Nisse:kkk?sss/lll " ++ Version ++ "\r\n\r\n", 
				       [{statuscode, 200}, 
					{version, Version}]),
    
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

