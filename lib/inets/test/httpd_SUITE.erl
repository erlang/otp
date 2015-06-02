%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2013-2015. All Rights Reserved.
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
-include_lib("public_key/include/public_key.hrl").
-include("inets_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-record(httpd_user,  {user_name, password, user_data}).
-record(httpd_group, {group_name, userlist}).
-define(MAX_HEADER_SIZE, 256).
%% Minutes before failed auths timeout.
-define(FAIL_EXPIRE_TIME,1). 
%% Seconds before successful auths timeout.
-define(AUTH_TIMEOUT,5).
-define(URL_START, "http://").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    [
     {group, http_basic},
     {group, https_basic},
     {group, http_limit},
     {group, https_limit},
     {group, http_custom},
     {group, https_custom},
     {group, http_basic_auth},
     {group, https_basic_auth},
     {group, http_auth_api},
     {group, https_auth_api},
     {group, http_auth_api_dets},
     {group, https_auth_api_dets},
     {group, http_auth_api_mnesia},
     {group, https_auth_api_mnesia},
     {group, http_htaccess}, 
     {group, https_htaccess},
     {group, http_security}, 
     {group, https_security},
     {group, http_reload},
     {group, https_reload},
     {group, http_mime_types} 
    ].

groups() ->
    [
     {http_basic, [], basic_groups()},
     {https_basic, [], basic_groups()},
     {http_limit, [], [{group, limit}]},
     {https_limit, [], [{group, limit}]},
     {http_custom, [], [{group,  custom}]},
     {https_custom, [], [{group,  custom}]},
     {http_basic_auth, [], [{group, basic_auth}]},
     {https_basic_auth, [], [{group, basic_auth}]},
     {http_auth_api, [], [{group, auth_api}]},
     {https_auth_api, [], [{group, auth_api}]},
     {http_auth_api_dets, [], [{group, auth_api_dets}]},
     {https_auth_api_dets, [], [{group, auth_api_dets}]},
     {http_auth_api_mnesia, [], [{group, auth_api_mnesia}]}, 
     {https_auth_api_mnesia, [], [{group, auth_api_mnesia}]},
     {http_htaccess, [], [{group, htaccess}]},
     {https_htaccess, [], [{group, htaccess}]},
     {http_security, [], [{group, security}]},
     {https_security, [], [{group, security}]},
     {http_reload, [], [{group, reload}]},
     {https_reload, [], [{group, reload}]},
     {http_mime_types, [], [alias_1_1, alias_1_0, alias_0_9]},
     {limit, [],  [max_clients_1_1, max_clients_1_0, max_clients_0_9]},  
     {custom, [],  [customize]},  
    inets_test_lib:start_apps([inets]).

server_start(_, HttpdConfig) ->
    {ok, Pid} = inets:start(httpd, HttpdConfig),
    Serv = inets:services_info(),
    {value, {_, _, Info}} = lists:keysearch(Pid, 2, Serv),
    {Pid, proplists:get_value(port, Info)}.

init_ssl(Group, Config) ->
    PrivDir = ?config(priv_dir, Config),
    CaKey = {_Trusted,_} = 
	erl_make_certs:make_cert([{key, dsa},
				  {subject, 
				   [{name, "Public Key"},
				    {?'id-at-name', 
				     {printableString, "public_key"}},
				    {?'id-at-pseudonym', 
				     {printableString, "pubkey"}},
				    {city, "Stockholm"},
				    {country, "SE"},
				    {org, "erlang"},
				    {org_unit, "testing dep"}
				   ]}
				 ]),
    ok = erl_make_certs:write_pem(PrivDir, "public_key_cacert", CaKey),
    
    CertK1 = {_Cert1, _} = erl_make_certs:make_cert([{issuer, CaKey}]),
    CertK2 = {_Cert2,_} = erl_make_certs:make_cert([{issuer, CertK1}, 
						   {digest, md5}, 
						   {extensions, false}]),
    ok = erl_make_certs:write_pem(PrivDir, "public_key_cert", CertK2),

    case start_apps(Group) of
	ok ->
	    init_httpd(Group, [{type, ssl} | Config]);
	_ ->
	    {skip, "Could not start https apps"}
    end.

server_config(http_basic, Config) ->
    basic_conf() ++ server_config(http, Config);
server_config(https_basic, Config) ->
    basic_conf() ++ server_config(https, Config);
server_config(http_reload, Config) ->
    [{keep_alive_timeout, 2}]  ++ server_config(http, Config);
server_config(https_reload, Config) ->
    [{keep_alive_timeout, 2}]  ++ server_config(https, Config);
server_config(http_limit, Config) ->
    [{max_clients, 1},
     %% Make sure option checking code is run
     {max_content_length, 100000002}]  ++ server_config(http, Config);
server_config(http_custom, Config) ->
    [{custom, ?MODULE}]  ++ server_config(http, Config);
server_config(https_custom, Config) ->
    [{custom, ?MODULE}]  ++ server_config(https, Config);
server_config(https_limit, Config) ->
    [{max_clients, 1}]  ++ server_config(https, Config);
server_config(http_basic_auth, Config) ->
    ServerRoot = ?config(server_root, Config),
    auth_conf(ServerRoot)  ++  server_config(http, Config);
server_config(https_basic_auth, Config) ->
    ServerRoot = ?config(server_root, Config),
    auth_conf(ServerRoot)  ++  server_config(https, Config);
server_config(http_auth_api, Config) ->
    ServerRoot = ?config(server_root, Config),
    auth_api_conf(ServerRoot, plain)  ++  server_config(http, Config);
server_config(https_auth_api, Config) ->
    ServerRoot = ?config(server_root, Config),
    auth_api_conf(ServerRoot, plain)  ++  server_config(https, Config);
server_config(http_auth_api_dets, Config) ->
    ServerRoot = ?config(server_root, Config),
    auth_api_conf(ServerRoot, dets)  ++  server_config(http, Config);
server_config(https_auth_api_dets, Config) ->
    ServerRoot = ?config(server_root, Config),
    auth_api_conf(ServerRoot, dets)  ++  server_config(https, Config);
server_config(http_auth_api_mnesia, Config) ->
    ServerRoot = ?config(server_root, Config),
    auth_api_conf(ServerRoot, mnesia)  ++  server_config(http, Config);
server_config(https_auth_api_mnesia, Config) ->
    ServerRoot = ?config(server_root, Config),
    auth_api_conf(ServerRoot, mnesia)  ++  server_config(https, Config);
server_config(http_htaccess, Config) ->
    auth_access_conf() ++ server_config(http, Config);
server_config(https_htaccess, Config) ->
    auth_access_conf() ++ server_config(https, Config);
server_config(http_security, Config) ->
    ServerRoot = ?config(server_root, Config),
    tl(auth_conf(ServerRoot)) ++ security_conf(ServerRoot) ++ server_config(http, Config);
server_config(https_security, Config) ->
    ServerRoot = ?config(server_root, Config),
    tl(auth_conf(ServerRoot)) ++ security_conf(ServerRoot) ++ server_config(https, Config);
server_config(http_mime_types, Config0) ->
    Config1 = basic_conf() ++  server_config(http, Config0),
    ServerRoot = ?config(server_root, Config0),
    MimeTypesFile = filename:join([ServerRoot,"config", "mime.types"]),
    [{mime_types, MimeTypesFile} | proplists:delete(mime_types, Config1)];

server_config(http, Config) ->
    ServerRoot = ?config(server_root, Config),
    [{port, 0},
     {server_name,"httpd_test"},
     {server_root, ServerRoot},
     {document_root, ?config(doc_root, Config)},
     {bind_address, any},
     {ipfamily, ?config(ipfamily, Config)},
     {max_header_size, 256},
     {max_header_action, close},
     {directory_index, ["index.html", "welcome.html"]},
     {mime_types, [{"html","text/html"},{"htm","text/html"}, {"shtml","text/html"},
		   {"gif", "image/gif"}]},
     {alias, {"/icons/", filename:join(ServerRoot,"icons") ++ "/"}},
     {alias, {"/pics/",  filename:join(ServerRoot,"icons") ++ "/"}},
     {script_alias, {"/cgi-bin/", filename:join(ServerRoot, "cgi-bin") ++ "/"}},
     {script_alias, {"/htbin/", filename:join(ServerRoot, "cgi-bin") ++ "/"}},
     {erl_script_alias, {"/cgi-bin/erl", [httpd_example, io]}},
     {eval_script_alias, {"/eval", [httpd_example, io]}}
    ];

server_config(https, Config) ->
    PrivDir = ?config(priv_dir, Config),
    [{socket_type, {essl,
		  [{cacertfile, 
		    filename:join(PrivDir, "public_key_cacert.pem")},
		   {certfile, 
		    filename:join(PrivDir, "public_key_cert.pem")},
		   {keyfile,
		    filename:join(PrivDir, "public_key_cert_key.pem")}
		  ]}}] ++ server_config(http, Config).

init_httpd(Group, Config0) ->
    Config1 = proplists:delete(port, Config0),
    Config = proplists:delete(server_pid, Config1),
    {Pid, Port} = server_start(Group, server_config(Group, Config)),
    [{server_pid, Pid}, {port, Port} | Config].

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

http_request_missing_CR(Request, "HTTP/1.1" = Version, Host) ->
    Request ++ Version ++ "\r\nhost:" ++ Host  ++ "\r\n\r\n\n";
http_request_missing_CR(Request, Version, _) ->
    Request ++ Version ++ "\r\n\n".

head_status("HTTP/0.9") ->
    501; %% Not implemented in HTTP/0.9
head_status(_) ->
    200.

basic_conf() ->
    [{modules, [mod_alias, mod_range, mod_responsecontrol,
		mod_trace, mod_esi, mod_cgi, mod_dir, mod_get, mod_head]}].

auth_access_conf() ->
    [{modules, [mod_alias, mod_htaccess, mod_dir, mod_get, mod_head]},
     {access_files, [".htaccess"]}].

auth_conf(Root) ->
    [{modules, [mod_alias, mod_auth, mod_dir, mod_get, mod_head]},
     {directory, {filename:join(Root, "htdocs/open"), 
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
		   {require_group, ["group3"]}]}}].     

auth_api_conf(Root, plain) ->
    [{modules, [mod_alias, mod_auth, mod_dir, mod_get, mod_head]},
     {directory, {filename:join(Root, "htdocs/open"), 
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
		   {require_group, ["group3"]}]}}];

auth_api_conf(Root, dets) ->
    [
     {modules, [mod_alias, mod_auth, mod_dir, mod_get, mod_head]},
     {directory, {filename:join(Root, "htdocs/dets_open"), 
		  [{auth_type, dets},
		   {auth_name, "Dets Open Area"},
		   {auth_user_file, filename:join(Root, "passwd")},
		   {auth_group_file, filename:join(Root, "group")},
		   {require_user, ["one", "Aladdin"]}]}},
     {directory, {filename:join(Root, "htdocs/dets_secret"), 
		  [{auth_type, dets},
		   {auth_name, "Dests Secret Area"},
		   {auth_user_file, filename:join(Root, "passwd")},
		   {auth_group_file, filename:join(Root, "group")},
		  {require_group, ["group1", "group2"]}]}},
     {directory, {filename:join(Root, "htdocs/dets_secret/top_secret"), 
		  [{auth_type, dets},
		   {auth_name, "Dets Top Secret Area"},
		   {auth_user_file, filename:join(Root, "passwd")},
		   {auth_group_file, filename:join(Root, "group")},
		   {require_group, ["group3"]}]}} 
    ];

auth_api_conf(Root, mnesia) ->
    [{modules, [mod_alias, mod_auth, mod_dir, mod_get, mod_head]},
     {directory, {filename:join(Root, "htdocs/mnesia_open"), 
		  [{auth_type, mnesia},
		   {auth_name, "Mnesia Open Area"},
		   {require_user, ["one", "Aladdin"]}]}},
     {directory, {filename:join(Root, "htdocs/mnesia_secret"), 
		  [{auth_type, mnesia},
		   {auth_name, "Mnesia Secret Area"},
		   {require_group, ["group1", "group2"]}]}},
     {directory, {filename:join(Root, "htdocs/mnesia_secret/top_secret"), 
		  [{auth_type, mnesia},
		   {auth_name, "Mnesia Top Secret Area"},
		   {require_group, ["group3"]}]}}].

security_conf(Root) ->
    SecFile = filename:join(Root, "security_data"),
    Open = filename:join(Root, "htdocs/open"),
    Secret = filename:join(Root, "htdocs/secret"),
    TopSecret = filename:join(Root, "htdocs/secret/top_secret"), 
	
    [{modules, [mod_alias, mod_auth, mod_security, mod_dir, mod_get, mod_head]},
     {security_directory, {Open, 
			   [{auth_name, "Open Area"},
			    {auth_user_file, filename:join(Root, "auth/passwd")},
			    {auth_group_file, filename:join(Root, "auth/group")},
			    {require_user, ["one", "Aladdin"]} | 
			    mod_security_conf(SecFile, Open)]}},
     {security_directory, {Secret, 
			   [{auth_name, "Secret Area"},
			    {auth_user_file, filename:join(Root, "auth/passwd")},
			    {auth_group_file, filename:join(Root, "auth/group")},
			    {require_group, ["group1", "group2"]} |
			    mod_security_conf(SecFile, Secret)]}},
     {security_directory, {TopSecret,
			   [{auth_name, "Top Secret Area"},
			    {auth_user_file, filename:join(Root, "auth/passwd")},
			    {auth_group_file, filename:join(Root, "auth/group")},
			    {require_group, ["group3"]} |
			    mod_security_conf(SecFile, TopSecret)]}}].     

mod_security_conf(SecFile, Dir) ->
    [{data_file, SecFile},
     {max_retries, 3},
     {fail_expire_time, ?FAIL_EXPIRE_TIME},
     {block_time, 1},
     {auth_timeout, ?AUTH_TIMEOUT},
     {callback_module, ?MODULE},
     {path, Dir} %% This is should not be needed, but is atm, awful design! 
    ].
    

http_status(Request, Config, Expected) ->
    Version = ?config(http_version, Config),
    Host = ?config(host, Config),    
    Type = ?config(type, Config),
    httpd_test_lib:verify_request(?config(type, Config), Host, 
				  ?config(port, Config),  
				  transport_opts(Type, Config),
				  ?config(node, Config),
				  http_request(Request, Version, Host),
				  Expected ++ [{version, Version}]).

http_status(Request, HeadersAndBody, Config, Expected) ->
    Version = ?config(http_version, Config),
    Host = ?config(host, Config),
    Type = ?config(type, Config),
    httpd_test_lib:verify_request(?config(type, Config), Host, 
				  ?config(port, Config),  
				  transport_opts(Type, Config),
				  ?config(node, Config),
				  http_request(Request, Version, Host, HeadersAndBody),
				  Expected ++ [{version, Version}]).

auth_status(AuthRequest, Config, Expected) ->
    Version = ?config(http_version, Config),
    Host = ?config(host, Config),    
    Type = ?config(type, Config),
    httpd_test_lib:verify_request(?config(type, Config), Host, 
				  ?config(port, Config),  
				  transport_opts(Type, Config),
				  ?config(node, Config),
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

transport_opts(ssl, Config) ->
    PrivDir = ?config(priv_dir, Config),
    [?config(ipfamily, Config),
     {cacertfile, filename:join(PrivDir, "public_key_cacert.pem")}];
transport_opts(_, Config) ->
    [?config(ipfamily, Config)].


%%% mod_range
create_range_data(Path) ->
    PathAndFileName=filename:join([Path,"range.txt"]),
    case file:read_file(PathAndFileName) of
	{error, enoent} ->
	    file:write_file(PathAndFileName,list_to_binary(["12345678901234567890",
							    "12345678901234567890",
							    "12345678901234567890",
							    "12345678901234567890",
							    "12345678901234567890"]));
	_ ->
	    ok
    end.

%%% mod_htaccess
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

dos_hostname(Type, Port, Host, Node, Version, Max) ->    
    TooLongHeader = lists:append(lists:duplicate(Max + 1, "a")),
    
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
 				       dos_hostname_request("", Version),
 				       [{statuscode, 200},
 					{version, Version}]),
    
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
 				       dos_hostname_request("dummy-host.ericsson.se", Version),
 				       [{statuscode, 200},
 					{version, Version}]),
    
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
 				       dos_hostname_request(TooLongHeader, Version),
 				       [{statuscode, request_entity_too_large_code(Version)},
 					{version, Version}]).
dos_hostname_request(Host, Version) ->
    dos_http_request("GET / ", Version, Host).

dos_http_request(Request,  "HTTP/1.1" = Version, Host) ->
    http_request(Request, Version, Host);
dos_http_request(Request, Version, Host) ->
    Request ++ Version ++ "\r\nhost:" ++ Host  ++ "\r\n\r\n".

request_entity_too_large_code("HTTP/1.0") ->
    403; %% 413 not defined in HTTP/1.0
request_entity_too_large_code(_) ->
    413.

length_required_code("HTTP/1.0") ->
    403; %% 411 not defined in HTTP/1.0
length_required_code(_) ->
    411.

garbage_content_length(Type, Port, Host, Node, Version) ->    
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
     				       garbage_content_length_request("GET / ", Version, Host, "aaaa"),	
     				       [{statuscode, length_required_code(Version)},
      					{version, Version}]),
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       garbage_content_length_request("GET / ", Version, Host, 
								      lists:duplicate($a, 100)),	
 				       [{statuscode, request_entity_too_large_code(Version)},
 					{version, Version}]).
 
garbage_content_length_request(Request, Version, Host, Garbage) ->	
    http_request(Request, Version, Host,
		 {"content-length:" ++ Garbage, "Body with garbage content length indicator"}).


update_password(Node, ServerRoot, _Address, Port, AuthPrefix, Dir, Old, New)->
    Directory = filename:join([ServerRoot, "htdocs", AuthPrefix ++ Dir]),
    rpc:call(Node, mod_auth, update_password, 
	     [undefined, Port, Directory, Old, New, New]).

add_user(Node, Root, Port, AuthPrefix, Dir, User, Password, UserData) ->
    Addr = undefined, 
    Directory = filename:join([Root, "htdocs", AuthPrefix ++ Dir]),
    rpc:call(Node, mod_auth, add_user, 
	     [User, Password, UserData, Addr, Port, Directory]).


delete_user(Node, Root, _Host, Port, AuthPrefix, Dir, User) ->
    Addr = undefined, 
    Directory = filename:join([Root, "htdocs", AuthPrefix ++ Dir]),
    rpc:call(Node, mod_auth, delete_user, [User, Addr, Port, Directory]).
remove_users(Node, ServerRoot, Host, Port, AuthPrefix, Dir) ->
    %% List users, delete them, and make sure they are gone.
    case list_users(Node, ServerRoot, Host, Port, AuthPrefix, Dir) of
	{ok, Users} ->
	    lists:foreach(fun(User) -> 
				  delete_user(Node, ServerRoot, Host, 
					      Port, AuthPrefix, Dir, User)
			  end,
			  Users),
		  {ok, []} = list_users(Node, ServerRoot, Host, Port, AuthPrefix, Dir);
	_ ->
	    ok
    end.

list_users(Node, Root, _Host, Port, AuthPrefix, Dir) ->
    Addr = undefined, 
    Directory = filename:join([Root, "htdocs", AuthPrefix ++ Dir]),
    rpc:call(Node, mod_auth, list_users, [Addr, Port, Directory]).

remove_groups(Node, ServerRoot, Host, Port,  AuthPrefix, Dir) ->
    {ok, Groups} = list_groups(Node, ServerRoot, Host, Port, AuthPrefix, Dir),
    lists:foreach(fun(Group) ->
			  delete_group(Node, Group, Port, ServerRoot, AuthPrefix, Dir)
		  end,
		  Groups),
    {ok, []} = list_groups(Node, ServerRoot, Host, Port, AuthPrefix, Dir).

delete_group(Node, Group, Port, Root, AuthPrefix, Dir) ->
    Addr = undefined, 
    Directory = filename:join([Root, "htdocs", AuthPrefix ++ Dir]),
    rpc:call(Node, mod_auth, delete_group, [Group, Addr, Port, Directory]).

list_groups(Node, Root, _, Port, AuthPrefix, Dir) ->
    Addr = undefined, 
    Directory = filename:join([Root, "htdocs", AuthPrefix ++ Dir]),
    rpc:call(Node, mod_auth, list_groups, [Addr, Port, Directory]).

add_group_member(Node, Root, Port, AuthPrefix, Dir, User, Group) ->
    Addr = undefined, 
    Directory = filename:join([Root, "htdocs", AuthPrefix ++ Dir]),
    rpc:call(Node, mod_auth, add_group_member, [Group, User, Addr, Port, 
					  Directory]).
getaddr() ->
    {ok,HostName} = inet:gethostname(),
    {ok,{A1,A2,A3,A4}} = inet:getaddr(HostName,inet),
    lists:flatten(io_lib:format("~p.~p.~p.~p",[A1,A2,A3,A4])).

receive_security_event(Event, Node, Port) ->
    receive 
	Event ->
	    ok;
	{'EXIT', _, _} ->
	    receive_security_event(Event, Node, Port)
    after 5000 ->
	    %% Flush the message queue, to see if we got something...
	    inets_test_lib:flush()
    end.

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

event(What, Port, Dir, Data) ->
    Msg = {event, What, Port, Dir, Data},
    case global:whereis_name(mod_security_test) of
	undefined ->
	    ok;
	_Pid ->
	    global:send(mod_security_test, Msg)
    end.

type(ip_comm) ->
    tcp;
type(_) ->
    ssl.

typestr(ip_comm) ->
    "tcp";
typestr(_) ->
    "ssl".
