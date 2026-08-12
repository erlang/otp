%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2023. All Rights Reserved.
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
-module(httpd_basic_SUITE).

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").
-include("inets_test_lib.hrl").


%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(URL_START, "http://localhost:").

suite() -> [{ct_hooks,[ts_install_cth]},
	    {timetrap, {seconds, 30}}].

all() -> 
    [{group, httpd_basic}].

groups() -> 
    [{httpd_basic, [parallel], [uri_too_long_414,
                                header_too_long_413,
                                entity_too_long,
                                http_0_9_not_supported,
                                erl_script_nocache_opt,
                                script_nocache,
                                escaped_url_in_error_body,
                                script_timeout,
                                {group, slowdose},
                                keep_alive_timeout,
                                invalid_rfc1123_date]},
     {slowdose, [parallel], [slowdose_min_bytes, slowdose_slow_header, slowdose_slow_body, slowdose_trickle_body]}].

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
    inets_test_lib:stop_apps([inets]),
    inets_test_lib:start_apps([inets]),
    PrivDir = proplists:get_value(priv_dir, Config),
    DataDir = proplists:get_value(data_dir, Config), 
    
    Dummy = 
	"<HTML>
<HEAD>
<TITLE>/index.html</TITLE>
</HEAD>
<BODY>
DUMMY
</BODY>
</HTML>",
    
    DummyFile = filename:join([PrivDir,"dummy.html"]),
    CgiDir =  filename:join(PrivDir, "cgi-bin"),
    ok = file:make_dir(CgiDir),
    {CgiPrintEnv, CgiSleep} = case os:type() of
				  {win32, _} ->
				      {"printenv.bat", "cgi_sleep.exe"};
				  _ ->
				      {"printenv.sh", "cgi_sleep"}
			      end,
    lists:foreach(
      fun(Cgi) ->
	      inets_test_lib:copy_file(Cgi, DataDir, CgiDir),
	      AbsCgi = filename:join([CgiDir, Cgi]),
	      {ok, FileInfo} = file:read_file_info(AbsCgi),
	      ok = file:write_file_info(AbsCgi, FileInfo#file_info{mode = 8#00755})
      end, [CgiPrintEnv, CgiSleep]),
    {ok, Fd}  = file:open(DummyFile, [write]),
    ok        = file:write(Fd, Dummy),
    ok        = file:close(Fd), 
    HttpdConf = [{port,          0}, 
		 {ipfamily,      inet}, 
		 {server_name,   "httpd_test"}, 
		 {server_root,   PrivDir},
		 {document_root, PrivDir}, 
		 {bind_address,  "localhost"}],

    [{httpd_conf, HttpdConf}, {cgi_dir, CgiDir},
     {cgi_printenv, CgiPrintEnv}, {cgi_sleep, CgiSleep} |  Config].

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    inets:stop(),
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
    Config.


%%--------------------------------------------------------------------
%% Function: end_per_testcase(Case, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(_Case, Config) ->
    Config.


%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
uri_too_long_414() ->
    [{doc, "Test that too long uri's get 414 HTTP code"}].
uri_too_long_414(Config) when is_list(Config) ->
    HttpdConf =   proplists:get_value(httpd_conf, Config),    
    {ok, Pid} = inets:start(httpd, [{max_uri_size, 10} 
				    | HttpdConf]),
    Info = httpd:info(Pid),
    Port = proplists:get_value(port, Info),
    Address = proplists:get_value(bind_address, Info),
    ok = httpd_test_lib:verify_request(ip_comm, Address, Port, node(), 
 				       "GET /morethantenchars "
 				       "HTTP/1.1\r\n\r\n",
 				       [{statuscode, 414},                            
       			        {version, "HTTP/1.1"}]),
    inets:stop(httpd, Pid).

%%-------------------------------------------------------------------------
header_too_long_413() ->
    [{doc,"Test that too long headers's get 413 HTTP code"}].
header_too_long_413(Config) when is_list(Config) ->
    HttpdConf = proplists:get_value(httpd_conf, Config), 
    {ok, Pid} = inets:start(httpd, [{max_header_size, 10}
				    | HttpdConf]),
    Info = httpd:info(Pid),
    Port = proplists:get_value(port, Info),
    Address = proplists:get_value(bind_address, Info),
    ok = httpd_test_lib:verify_request(ip_comm, Address, Port, node(), 
 				       "GET index.html "
 				       "HTTP/1.1\r\n"
				       "Connection:close \r\n\r\n ",
 				       [{statuscode, 413},
 				        {version, "HTTP/1.1"}]),
    inets:stop(httpd, Pid).

%%-------------------------------------------------------------------------

http_0_9_not_supported() ->
    [{doc, "Test that HTTP 0.9 is not supported"}].
http_0_9_not_supported(Config) when is_list(Config) ->
    HttpdConf =   proplists:get_value(httpd_conf, Config),
    {ok, Pid} = inets:start(httpd, HttpdConf),
    Info = httpd:info(Pid),
    Port = proplists:get_value(port, Info),
    Address = proplists:get_value(bind_address, Info),

    ok = httpd_test_lib:verify_request(ip_comm, Address, Port, node(),
     				       "GET /\r\n\r\n",
     				       [{statuscode, 505},
     				        {version, "HTTP/1.1"}]),
    inets:stop(httpd, Pid).

%%-------------------------------------------------------------------------

entity_too_long() ->
    [{doc, "Test that too long versions and method strings are rejected"}].
entity_too_long(Config) when is_list(Config) ->
    HttpdConf =   proplists:get_value(httpd_conf, Config),    
    {ok, Pid} = inets:start(httpd, HttpdConf),
    Info = httpd:info(Pid),
    Port = proplists:get_value(port, Info),
    Address = proplists:get_value(bind_address, Info),
    
    %% Not so long but wrong
    ok = httpd_test_lib:verify_request(ip_comm, Address, Port, node(), 
     				       "GET / " ++
					   lists:duplicate(5, $A) ++ "\r\n\r\n",
     				       [{statuscode, 400},
     				        {version, "HTTP/1.1"}]),

    %% Too long
    ok = httpd_test_lib:verify_request(ip_comm, Address, Port, node(), 
 				       "GET / " ++
					   lists:duplicate(100, $A) ++ "\r\n\r\n",
 				       [{statuscode, 413},
 				        {version, "HTTP/1.1"}]),
    %% Not so long but wrong
    ok = httpd_test_lib:verify_request(ip_comm, Address, Port, node(), 
				       lists:duplicate(5, $A) ++ " / "
				       "HTTP/1.1\r\n\r\n",
 				       [{statuscode, 501},
					%% Server will send lowest version
					%% as it will not get to the 
					%% client version
					%% before aborting
 				        {version, "HTTP/1.1"}]),
    %% Too long
    ok = httpd_test_lib:verify_request(ip_comm, Address, Port, node(), 
				       lists:duplicate(100, $A) ++ " / "
				       "HTTP/1.1\r\n\r\n",
 				       [{statuscode, 413},
					%% Server will send lowest version
					%% as it will not get to the 
					%% client version
					%% before aborting
 				        {version, "HTTP/1.1"}]),   
    inets:stop(httpd, Pid).
    
%%-------------------------------------------------------------------------

script_nocache() ->
    [{doc,"Test nocache option for mod_cgi and mod_esi"}].
script_nocache(Config) when is_list(Config) ->
    Normal = {no_header, "cache-control"},
    NoCache = {header, "cache-control", "no-cache"},
    verify_script_nocache(Config, false, false, Normal, Normal),
    verify_script_nocache(Config, true, false, NoCache, Normal),
    verify_script_nocache(Config, false, true, Normal, NoCache),
    verify_script_nocache(Config, true, true, NoCache, NoCache).

%%-------------------------------------------------------------------------
erl_script_nocache_opt(doc) ->
    ["Test that too long headers's get 413 HTTP code"];
erl_script_nocache_opt(suite) ->
    [];
erl_script_nocache_opt(Config) when is_list(Config) ->
    HttpdConf   = proplists:get_value(httpd_conf, Config),
    {ok, Pid}   = inets:start(httpd, [{port, 0}, {erl_script_nocache, true} | HttpdConf]),
    Info        = httpd:info(Pid),
    Port        = proplists:get_value(port,         Info),
    _Address    = proplists:get_value(bind_address, Info),
    URL1        = ?URL_START ++ integer_to_list(Port),
    case httpc:request(get, {URL1 ++ "/dummy.html", []},
		       [{url_encode,  false}, 
			{version,     "HTTP/1.0"}],
		       [{full_result, false}]) of
	{ok, {200, _}} ->
	    ok
    end,
    inets:stop(httpd, Pid).

%%-------------------------------------------------------------------------


%%-------------------------------------------------------------------------

escaped_url_in_error_body() ->
    [{doc, "Test Url-encoding see OTP-8940"}].
escaped_url_in_error_body(Config) when is_list(Config) -> 
    HttpdConf   = proplists:get_value(httpd_conf, Config),
    {ok, Pid}   = inets:start(httpd, [{port, 0} | HttpdConf]),
    Info        = httpd:info(Pid),
    Port        = proplists:get_value(port,         Info),
    URL1        = ?URL_START ++ integer_to_list(Port),
   
    %% Sanity check
    {ok, {200, _}} = httpc:request(get, {URL1 ++ "/dummy.html", []},
     				   [{url_encode,  false}, 
     				    {version,     "HTTP/1.0"}],
     				   [{full_result, false}]),
    {ok, {200, _}} = httpc:request(get, {URL1 ++ "/dummy.html", []},
     				   [{url_encode,  true}, 
     				    {version,     "HTTP/1.0"}],
     				   [{full_result, false}]),
    
    %% Ask for a non-existing page(1)
    Path            = "/<b>this_is_bold<b>",
    URL2 = uri_string:recompose(#{scheme => "http",
                                  host => "localhost",
                                  port => Port,
                                  path => Path}),
    
    #{path := EncodedPath} = uri_string:parse(URL2),
    HTMLEncodedPath =  http_util:html_encode(EncodedPath),
    {ok, {404, Body3}} = httpc:request(get, {URL2, []},
				       [{url_encode,  true}, 
					{version,     "HTTP/1.0"}],
				       [{full_result, false}]),

    HTMLEncodedPath = find_URL_path(string:tokens(Body3, " ")),

    {ok, {404, Body4}} = httpc:request(get, {URL2, []}, 
				       [{url_encode,  false}, 
					{version,     "HTTP/1.0"}],
				       [{full_result, false}]),
    
    HTMLEncodedPath = find_URL_path(string:tokens(Body4, " ")),
    inets:stop(httpd, Pid).

%%-------------------------------------------------------------------------

keep_alive_timeout(doc) ->
    ["Test the keep_alive_timeout option"];
keep_alive_timeout(suite) ->
    [];
keep_alive_timeout(Config) when is_list(Config) ->
    HttpdConf   = proplists:get_value(httpd_conf, Config),
    {ok, Pid}   = inets:start(httpd, [{port, 0}, {keep_alive, true}, {keep_alive_timeout, 2} | HttpdConf]),
    Info        = httpd:info(Pid),
    Port        = proplists:get_value(port,         Info),
    _Address    = proplists:get_value(bind_address, Info),
    {ok, S} = gen_tcp:connect("localhost", Port, []),
    receive
    after 3000 ->
	    {error, closed} = gen_tcp:send(S, "hey")
    end,
    inets:stop(httpd, Pid).

%%-------------------------------------------------------------------------

script_timeout(doc) ->
    ["Test the httpd script_timeout option"];
script_timeout(suite) ->
    [];
script_timeout(Config) when is_list(Config) ->
    verify_script_timeout(Config, 20, 200),
    verify_script_timeout(Config, 5, 403),
    ok.

verify_script_timeout(Config, ScriptTimeout, StatusCode) ->
    HttpdConf = proplists:get_value(httpd_conf, Config),
    CgiScript = proplists:get_value(cgi_sleep, Config),
    CgiDir = proplists:get_value(cgi_dir, Config),
    {ok, Pid} = inets:start(httpd, [{port, 0},
				    {script_alias,
				     {"/cgi-bin/", CgiDir ++ "/"}},
				    {script_timeout, ScriptTimeout}
				    | HttpdConf]),
    Info = httpd:info(Pid),
    Port = proplists:get_value(port, Info),
    Address = proplists:get_value(bind_address, Info),
    ok = httpd_test_lib:verify_request(ip_comm, Address, Port, node(),
				       "GET /cgi-bin/" ++ CgiScript ++
					   " HTTP/1.0\r\n\r\n",
				       [{statuscode, StatusCode},
					{version, "HTTP/1.0"}]),
    inets:stop(httpd, Pid).

%%-------------------------------------------------------------------------

slowdose_min_bytes() ->
    [{doc, "Testing minimum bytes per second option"}].
slowdose_min_bytes(Config) when is_list(Config) ->
    HttpdConf =   proplists:get_value(httpd_conf, Config),
    {ok, Pid} = inets:start(httpd, [{port, 0}, {minimum_bytes_per_second, 200}|HttpdConf]),
    Info = httpd:info(Pid),
    Port = proplists:get_value(port, Info),
    {ok, Socket} = gen_tcp:connect("localhost", Port, []),
    receive
    after 6000 ->
	    {error, closed} = gen_tcp:send(Socket, "Hey")
    end.

slowdose_slow_header() ->
    [{doc, "Testing timeout for slow headers"}].
slowdose_slow_header(Config) when is_list(Config) ->
    HttpdConf = proplists:get_value(httpd_conf, Config),
    {ok, Pid} = inets:start(httpd, [{port, 0}, {keep_alive_timeout, 2} | HttpdConf]),
    Info = httpd:info(Pid),
    Port = proplists:get_value(port, Info),
    {ok, Socket} = inets_test_lib:connect_bin(ip_comm, "localhost", Port, []),
    ok = inets_test_lib:send(ip_comm, Socket, "GET /dummy.html HTTP/1.0\r\n"),
    receive
        {tcp, Socket, Data} ->
            case binary:match(Data, <<"408">>) of
                {_, _} ->
                    ok;
                nomatch ->
                    ct:fail("Server did not reply with 408 timeout")
            end;
        Else ->
            ct:fail({"Unexpected data: ", Else})
    after 3000 ->
            ct:fail("Server did not close connection after slow header")
    end.

slowdose_slow_body() ->
    [{doc, "Testing timeout for slow body"}].
slowdose_slow_body(Config) when is_list(Config) ->
    HttpdConf =   proplists:get_value(httpd_conf, Config),
    {ok, Pid} = inets:start(httpd, [{port, 0}, {max_body_read_timeout, 1} | HttpdConf]),
    Info = httpd:info(Pid),
    Port = proplists:get_value(port, Info),
    {ok, Socket} = inets_test_lib:connect_bin(ip_comm, "localhost", Port, []),
    ok = inets_test_lib:send(ip_comm, Socket, "POST /dummy.html HTTP/1.1\r\n" ++
                             "Host: localhost\r\n" ++
                             "Content-Length: 10\r\n\r\n" ++
                             "12345"),
    receive
        {tcp, Socket, Data} ->
            case binary:match(Data, <<"408">>) of
                {_, _} ->
                    ok;
                nomatch ->
                    ct:fail({"Server did not reply with 408 timeout", Data})
            end;
        Else ->
            ct:fail({"Unexpected data: ", Else})
    after 3000 ->
            ct:fail("Server did not close connection after slow header")
    end.

slowdose_trickle_body() ->
    [{doc, "Testing timeout for trickling body"}].
slowdose_trickle_body(Config) when is_list(Config) ->

    %% First we test that max_body_read_timeout does not trigger this
    slowdose_trickle_body(Config, {max_body_read_timeout, 1}, <<"501">>),

    %% Then we test that minimum_bytes_per_second does trigger it
    slowdose_trickle_body(Config, {minimum_bytes_per_second, 50}, <<"408">>).

slowdose_trickle_body(Config, HttpdConfig, Status) ->
    HttpdConf =   proplists:get_value(httpd_conf, Config),
    {ok, Pid} = inets:start(httpd, [{port, 0}, HttpdConfig | HttpdConf]),
    Info = httpd:info(Pid),
    Port = proplists:get_value(port, Info),
    {ok, Socket} = inets_test_lib:connect_bin(ip_comm, "localhost", Port, []),
    ok = inets_test_lib:send(ip_comm, Socket, "POST /dummy.html HTTP/1.1\r\n" ++
                             "Host: localhost\r\n" ++
                             "Content-Length: 10\r\n\r\n"),
    [timer:sleep(500), inets_test_lib:send(ip_comm, Socket, integer_to_list(N)) || N <- lists:seq(1, 10)],
    receive
        {tcp, Socket, Data} ->
            case binary:match(Data, Status) of
                {_, _} ->
                    %% Flush the 501 contents if that is what we got, to avoid it interfering with other tests
                    inets_test_lib:flush();
                nomatch ->
                    ct:fail({"Server did not reply with 501 Not Implemented", Data})
            end;
        Else1 ->
            ct:fail({"Unexpected data: ", Else1})
    after 3000 ->
            ct:fail("Server did not close connection after slow header")
    end.

%%-------------------------------------------------------------------------

invalid_rfc1123_date() ->
    [{doc, "Test that a non-DST date is handled correctly"}].
invalid_rfc1123_date(Config) when is_list(Config) ->
    Rfc1123FormattedDate = "Sun, 26 Mar 2017 01:00:00 GMT",
    NonDSTDateTime = {{2017, 03, 26},{1, 0, 0}},
    Rfc1123FormattedDate =:= httpd_util:rfc1123_date(NonDSTDateTime).


%%-------------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------------

verify_script_nocache(Config, CgiNoCache, EsiNoCache, CgiOption, EsiOption) ->
    HttpdConf = proplists:get_value(httpd_conf, Config),
    CgiScript = proplists:get_value(cgi_printenv, Config),
    CgiDir = proplists:get_value(cgi_dir, Config),
    {ok, Pid} = inets:start(httpd, [{port, 0},
				    {script_alias,
				     {"/cgi-bin/", CgiDir ++ "/"}},
				    {script_nocache, CgiNoCache},
				    {erl_script_alias,
				     {"/cgi-bin/erl", [httpd_example,io]}},
				    {erl_script_nocache, EsiNoCache}
				    | HttpdConf]),
    Info = httpd:info(Pid),
    Port = proplists:get_value(port, Info),
    Address = proplists:get_value(bind_address, Info),
    ok = httpd_test_lib:verify_request(ip_comm, Address, Port, node(),
				       "GET /cgi-bin/" ++ CgiScript ++
					   " HTTP/1.0\r\n\r\n",
				       [{statuscode, 200},
					CgiOption,
					{version, "HTTP/1.0"}]),
    ok = httpd_test_lib:verify_request(ip_comm, Address, Port, node(),
				       "GET /cgi-bin/erl/httpd_example:get "
				       "HTTP/1.0\r\n\r\n",
				       [{statuscode, 200},
					EsiOption,
					{version, "HTTP/1.0"}]),
    inets:stop(httpd, Pid).

find_URL_path([]) ->
    "";
find_URL_path(["URL", URL | _]) ->
    URL;
find_URL_path([_ | Rest]) ->
    find_URL_path(Rest).

skip(Reason) ->
    {skip, Reason}.

