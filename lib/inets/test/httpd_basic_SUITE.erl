%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2007-2026. All Rights Reserved.
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
-include_lib("inets/include/httpd.hrl").
-include("inets_test_lib.hrl").


%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(URL_START, "http://localhost:").

%% The ESI env keys for the standard request headers, i.e. the
%% {http_<lowercase name>, string()} contract documented in mod_esi. The
%% list mirrors the standard fields of #http_request_h{} in
%% http_internal.hrl with '-' replaced by '_', and deliberately does NOT
%% include http_other, which is the env key carrying the headers that get
%% no atom of their own.
%%
%% Written out literally on purpose. Deriving it from
%% record_info(fields, http_request_h) would only restate whatever
%% httpd_request_handler:get_esi_headers/0 computes, and this list is the
%% complete set of atoms a request header name is allowed to reach, so a
%% change to it has to be noticed rather than silently followed.
-define(ESI_STD_HEADER_ATOMS,
        [http_cache_control, http_connection, http_date, http_pragma,
         http_trailer, http_transfer_encoding, http_upgrade, http_via,
         http_warning,
         http_accept, http_accept_charset, http_accept_encoding,
         http_accept_language, http_authorization, http_expect, http_from,
         http_host, http_if_match, http_if_modified_since,
         http_if_none_match, http_if_range, http_if_unmodified_since,
         http_max_forwards, http_proxy_authorization, http_range,
         http_referer, http_te, http_user_agent,
         http_allow, http_content_encoding, http_content_language,
         http_content_length, http_content_location, http_content_md5,
         http_content_range, http_content_type, http_expires,
         http_last_modified]).

suite() -> [{ct_hooks,[ts_install_cth]},
	    {timetrap, {seconds, 30}}].

all() -> 
    [{group, httpd_basic}, max_clients_default].

groups() -> 
    [{httpd_basic, [parallel], [uri_too_long_414,
                                header_too_long_413,
                                entity_too_long,
                                http_0_9_not_supported,
                                erl_script_nocache_opt,
                                script_nocache,
                                escaped_url_in_error_body,
                                script_timeout,
                                chunked_body_size_unbounded,
                                {group, slowdose},
                                response_header_ctl_sanitation,
                                keep_alive_timeout,
                                invalid_rfc1123_date,
                                ip_comm_options_fixed_port,
                                esi_header_atom_growth]},
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
    {ok,_}    = file:copy(DummyFile, filename:join([PrivDir,"index.html"])),
    HttpdConf = [{port,          0}, 
		 {ipfamily,      inet}, 
		 {server_name,   "httpd_test"}, 
		 {server_root,   PrivDir},
		 {document_root, PrivDir}, 
                 {modules, [mod_alias, mod_auth, mod_esi, mod_actions,
                            mod_cgi, mod_dir, mod_get, mod_head,
                            mod_log, mod_disk_log]},
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

%%-------------------------------------------------------------------------
response_header_ctl_sanitation() ->
    [{doc, "Test that CR, LF and NUL in a response header from a customize "
      "callback or an ESI script cannot inject headers or split the "
      "response."}].
response_header_ctl_sanitation(Config) when is_list(Config) ->
    HttpdConf = proplists:get_value(httpd_conf, Config),
    {ok, Pid} = inets:start(httpd, [{port, 0},
                                    {customize, ?MODULE},
                                    {erl_script_alias,
                                     {"/cgi-bin/erl", [?MODULE]}}
                                    | HttpdConf]),
    Info = httpd:info(Pid),
    Port = proplists:get_value(port, Info),
    Address = proplists:get_value(bind_address, Info),

    try
        %% customize callback returning a CRLF-carrying value
        Response1 = raw_request(Address, Port,
                                "GET /dummy.html HTTP/1.1\r\n"
                                "Host: localhost\r\n"
                                "Connection: close\r\n\r\n"),
        no_injected_header(Response1),

        %% ESI script emitting NUL in a header value
        Response2 = raw_request(Address, Port,
                                "GET /cgi-bin/erl/" ++ atom_to_list(?MODULE) ++
                                    ":esi_ctl_header HTTP/1.1\r\n"
                                "Host: localhost\r\n"
                                "Connection: close\r\n\r\n"),
        {Head2, _} = split_response(Response2),
        case binary:match(Head2, <<0>>) of
            nomatch -> ok;
            _ -> ct:fail({nul_byte_in_response_headers, Head2})
        end,
        no_injected_header(Response2)
    after
        inets:stop(httpd, Pid)
    end.

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
    {ok, Pid} = inets:start(httpd, [{port, 0}, {request_timeout, 2} | HttpdConf]),
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
    {ok, Pid} = inets:start(httpd, [{port, 0}, {request_timeout, 1} | HttpdConf]),
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

    %% First we test that request_timeout does not trigger this
    slowdose_trickle_body(Config, {request_timeout, 1}, <<"501">>),

    % Then we test that minimum_bytes_per_second does trigger it
    slowdose_trickle_body(Config, {minimum_bytes_per_second, 50}, <<"408">>).

slowdose_trickle_body(Config, HttpdConfig, Status) ->
    HttpdConf =   proplists:get_value(httpd_conf, Config),
    {ok, Pid} = inets:start(httpd, [{port, 0}, HttpdConfig | HttpdConf]),
    Info = httpd:info(Pid),
    Port = proplists:get_value(port, Info),
    {ok, Socket} = inets_test_lib:connect_bin(ip_comm, "localhost", Port, []),
    ok = inets_test_lib:send(ip_comm, Socket, "POST /dummy.html HTTP/1.1\r\n" ++
                             "Host: localhost\r\n" ++
                             "Content-Length: 11\r\n\r\n"),
    [inets_test_lib:send(ip_comm, Socket, integer_to_list(N)) || N <- lists:seq(1, 10), is_atom(timer:sleep(500))],
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
    end,
    inets_test_lib:close(ip_comm, Socket),
    ok = inets:stop(httpd, Pid).

%%-------------------------------------------------------------------------

%%-------------------------------------------------------------------------

esi_header_atom_growth() ->
    [{doc, "ESI request header names must not create atoms"}].
esi_header_atom_growth(Config) when is_list(Config) ->
    ct:comment("A request header name must never mint an atom"),
    HttpdConf = proplists:get_value(httpd_conf, Config),
    {ok, Pid} = inets:start(httpd, [{port, 0},
                                    {erl_script_alias,
                                     {"/esi", [httpd_example, io]}}
                                    | HttpdConf]),
    Info = httpd:info(Pid),
    Port = proplists:get_value(port, Info),
    Address = proplists:get_value(bind_address, Info),

    %% Header names that already exist as atoms, so they pass the raw name
    %% guard in httpd_script_env, but that are not standard HTTP headers.
    %% Before the fix each one permanently created "http_" ++ Name, and
    %% that atom then let a header of that name create yet another one.
    Seeds = ["node", "port", "data", "name", "path", "id", "error"],
    Rounds = [Seeds,
              ["http_" ++ N || N <- Seeds],
              ["http_http_" ++ N || N <- Seeds]],

    lists:foreach(
      fun(Names) ->
              lists:foreach(
                fun(Name) ->
                        Prefixed = "http_" ++ Name,
                        %% Some of these atoms may already exist for
                        %% reasons of their own in a given node; only the
                        %% ones that do not are evidence either way.
                        Existed = atom_exists(Prefixed),
                        ok = httpd_test_lib:verify_request(
                               ip_comm, Address, Port, node(),
                               "GET /esi/httpd_example:get?x=1 HTTP/1.0\r\n" ++
                                   Name ++ ": v\r\n\r\n",
                               [{statuscode, 200}, {version, "HTTP/1.0"}]),
                        %% The header name must not have created an atom.
                        case Existed of
                            true ->
                                ok;
                            false ->
                                false = atom_exists(Prefixed)
                        end
                end, Names)
      end, Rounds),

    %% The property above only holds because the lookup in
    %% httpd_script_env picks from a closed set of atoms. Check that the
    %% set is the documented one, and that the headers it names really do
    %% end up as atom keys rather than under http_other.
    ok = verify_esi_header_atoms(),
    ok = verify_esi_header_categorization(),

    inets:stop(httpd, Pid).

%% The closed set of atoms a request header name may reach must be
%% exactly the standard request headers -- no more (every extra entry is
%% a permanent atom reachable from request data) and no fewer (a missing
%% entry silently demotes a standard header to http_other).
verify_esi_header_atoms() ->
    Actual = httpd_request_handler:get_esi_headers(),

    %% A duplicate would mean two record fields map to the same ESI key.
    [] = Actual -- lists:usort(Actual),

    Expected = lists:sort(?ESI_STD_HEADER_ATOMS),
    case lists:sort(Actual) of
        Expected ->
            ok;
        Sorted ->
            ct:fail({esi_header_atoms_mismatch,
                     [{unexpected, Sorted -- Expected},
                      {missing, Expected -- Sorted}]})
    end,

    %% Implied by the comparison above, kept explicit because it is the
    %% one collision that silently loses data: http_other is the env key
    %% carrying the non-standard headers, so it must not also be
    %% reachable as a header name.
    false = lists:member(http_other, Actual),
    ok.

%% A standard header name becomes its own {http_<name>, Value} env
%% element; anything else keeps its raw name under http_other and gets no
%% atom key at all.
verify_esi_header_categorization() ->
    Standard = ?ESI_STD_HEADER_ATOMS,
    StdHeaders = [{esi_key_to_header_name(Key), atom_to_list(Key)}
                  || Key <- Standard],
    %% "node", "other" and "error" exist as atoms, so they pass the
    %% list_to_existing_atom/1 guard in httpd_script_env and reach the
    %% lookup; the last one does not exist as an atom at all. None of
    %% them is a standard header.
    NonStdHeaders =
        [{"node", "node-value"},
         {"other", "other-value"},
         {"error", "error-value"},
         {"x-unknown-" ++ integer_to_list(erlang:unique_integer([positive])),
          "x-value"}],
    Env = esi_env(StdHeaders ++ NonStdHeaders),
    Other = proplists:get_value(http_other, Env),
    true = is_list(Other),

    lists:foreach(
      fun(Key) ->
              Value = atom_to_list(Key),
              case proplists:lookup(Key, Env) of
                  {Key, Value} ->
                      ok;
                  none ->
                      ct:fail({standard_header_not_categorized, Key, Other});
                  {Key, Wrong} ->
                      ct:fail({wrong_value, Key, Wrong})
              end,
              %% ... and it is not duplicated under http_other.
              none = proplists:lookup(esi_key_to_header_name(Key), Other)
      end, Standard),

    lists:foreach(
      fun({Name, Value} = Header) ->
              case proplists:lookup(Name, Other) of
                  Header ->
                      ok;
                  Unexpected ->
                      ct:fail({non_standard_header_not_in_other, Name,
                               Unexpected})
              end,
              %% No atom key carries the value. Checked by value rather
              %% than by key absence because for "other" the key does
              %% exist -- it is the catch-all element itself.
              Prefixed = "http_" ++ Name,
              case atom_exists(Prefixed) of
                  false ->
                      ok;
                  true ->
                      false = lists:member({list_to_existing_atom(Prefixed),
                                            Value}, Env)
              end
      end, NonStdHeaders),
    ok.

esi_key_to_header_name(Key) ->
    "http_" ++ Name = atom_to_list(Key),
    re:replace(Name, "_", "-", [{return, list}, global]).

%% httpd_script_env:create_env/3 is called directly instead of going
%% through a request: content-length and transfer-encoding cannot be put
%% on the wire as plain header values without changing how the request
%% itself is framed, so a socket based check would leave exactly the
%% entity headers untested.
esi_env(ParsedHeader) ->
    ConfigDb = ets:new(httpd_test_config_db, [set, protected]),
    true = ets:insert(ConfigDb, [{server_name, "httpd_test"}, {port, 0}]),
    {ok, Socket} = gen_tcp:listen(0, [{ip, {127,0,0,1}}]),
    ModData = #mod{init_data = #init_data{peername = {0, "127.0.0.1"},
                                          sockname = {0, "127.0.0.1"},
                                          resolve = "localhost"},
                   socket_type = ip_comm,
                   socket = Socket,
                   config_db = ConfigDb,
                   method = "GET",
                   request_uri = "/esi/httpd_example:get",
                   http_version = "HTTP/1.1",
                   parsed_header = ParsedHeader},
    try
        httpd_script_env:create_env(esi, ModData, [])
    after
        gen_tcp:close(Socket),
        ets:delete(ConfigDb)
    end.

atom_exists(Name) ->
    try list_to_existing_atom(Name) of
        _ ->
            true
    catch
        error:badarg ->
            false
    end.

%%-------------------------------------------------------------------------

invalid_rfc1123_date() ->
    [{doc, "Test that a non-DST date is handled correctly"}].
invalid_rfc1123_date(Config) when is_list(Config) ->
    Rfc1123FormattedDate = "Sun, 26 Mar 2017 01:00:00 GMT",
    NonDSTDateTime = {{2017, 03, 26},{1, 0, 0}},
    Rfc1123FormattedDate =:= httpd_util:rfc1123_date(NonDSTDateTime).

%%-------------------------------------------------------------------------

ip_comm_options_fixed_port() ->
    [{doc, "GH-11312: httpd starts with {ip_comm, SockOpts} on a fixed "
           "(non-zero) port"}].
ip_comm_options_fixed_port(Config) when is_list(Config) ->
    HttpdConf = proplists:get_value(httpd_conf, Config),
    %% Reserve a free port up front, then close it, so that we can
    %% configure httpd with a fixed (non-zero) port. A fixed port
    %% together with {ip_comm, SockOpts} exercises http_transport:listen/4
    %% (as opposed to port 0, which exercises listen/5).
    {ok, LSock} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(LSock),
    ok = gen_tcp:close(LSock),
    Conf = [{port, Port},
            {socket_type, {ip_comm, [{nodelay, true}]}}
            | proplists:delete(port, HttpdConf)],
    {ok, Pid} = inets:start(httpd, Conf),
    Info = httpd:info(Pid),
    Port = proplists:get_value(port, Info),
    Address = proplists:get_value(bind_address, Info),
    ok = httpd_test_lib:verify_request(ip_comm, Address, Port, node(),
				       "GET / HTTP/1.1\r\n"
				       "Host: localhost\r\n\r\n",
				       [{statuscode, 200},
					{version, "HTTP/1.1"}]),
    inets:stop(httpd, Pid).

%%-------------------------------------------------------------------------

chunked_body_size_unbounded() ->
    [{doc, "check that never-completing and malformed chunks don't bypass max_body_size"}].
chunked_body_size_unbounded(Config) when is_list(Config) ->
    MaxBody   = 1024,
    HttpdConf = proplists:get_value(httpd_conf, Config),
    {ok, Pid} = inets:start(httpd, [{port, 0}, {max_body_size, MaxBody} | HttpdConf]),
    Port      = proplists:get_value(port, httpd:info(Pid)),
    try
        %% Control: completed chunk > max_body_size -> hits ?CR?LF -> body_too_big.
        chunk_probe(Port, [io_lib:format("~.16b\r\n", [4 * MaxBody]),
                                      payload(4 * MaxBody), "\r\n0\r\n\r\n"]),
        %% (a) huge declared size, stream 64x the limit, never terminate.
        chunk_probe(Port, ["FFFFFFFF\r\n", payload(64 * MaxBody)]),
        %% (b) correct size, non-CRLF where the terminator belongs, keep streaming.
        chunk_probe(Port, ["5\r\nAAAAA", "X", payload(64 * MaxBody)]),
        %% (c) many small chunks that together exceed the limit, never terminate.
        chunk_probe(Port, lists:flatten([iolist_to_binary([io_lib:format("~.16b\r\n", [MaxBody div 10]),
                                        payload(MaxBody div 10), "\r\n"]) || _ <- lists:seq(1, 11)]))

    after
        inets:stop(httpd, Pid)
    end.

%% Open a chunked POST, send Body
chunk_probe(Port, Body) ->
    {ok, S} = inets_test_lib:connect_bin(ip_comm, "localhost", Port, []),
    Req = ["POST / HTTP/1.1\r\n",
           "Host: localhost\r\n",
           "Transfer-Encoding: chunked\r\n\r\n"],
    ok = inets_test_lib:send(ip_comm, S, iolist_to_binary(Req)),
    [inets_test_lib:send(ip_comm, S, iolist_to_binary(B)) || B <- Body, is_atom(timer:sleep(50))],
    Verdict =
        receive
            {tcp, S, Data}  ->
                case binary:match(Data, <<"413">>) of
                    {_, _} -> bounded; %% limit hit, server replied
                    nomatch -> ct:fail({"Server replied but did not indicate body too large", Data})
                end
        after 5000 ->
            ct:fail("Server did not reply within timeout, likely still buffering the body (bug)")
        end,
    inets_test_lib:close(ip_comm, S),
    Verdict.

payload(N) -> binary:copy(<<$A>>, N).

max_clients_default(Config) when is_list(Config) ->
    HttpdConf = proplists:get_value(httpd_conf, Config),
    {ok, Pid} = inets:start(httpd, [{port, 0} | HttpdConf]),
    Port      = proplists:get_value(port, httpd:info(Pid)),
    HttpdClient = [{host, "localhost"}, {type, ip_comm}, {port, Port} | Config],
    try
        httpd_SUITE:do_max_clients([{http_version, "HTTP/1.1"} | HttpdClient], 150),
        httpd_SUITE:do_max_clients([{http_version, "HTTP/1.0"} | HttpdClient], 150)
    after
        inets:stop(httpd, Pid)
    end.

%%-------------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------------

%% Callbacks used by response_header_ctl_sanitation/1

%% httpd_custom_api callback
response_header({"x-injection-test", _}) ->
    {true, {"x-injection-test", "legit\r\nX-Injected: evil"}};
response_header(Header) ->
    {true, Header}.

request_header(Header) ->
    {true, Header}.

response_default_headers() ->
    [{"x-injection-test", "placeholder"}].

%% ESI callback
esi_ctl_header(SessionID, _Env, _Input) ->
    mod_esi:deliver(SessionID,
                    "content-type: text/plain\r\n"
                    "x-esi-test: legit\0hidden\r\n"
                    "\r\n"),
    mod_esi:deliver(SessionID, "body").

raw_request(Address, Port, RequestStr) ->
    {ok, Socket} = gen_tcp:connect(Address, Port,
                                   [binary, {packet, 0}, {active, false}]),
    ok = gen_tcp:send(Socket, RequestStr),
    Response = recv_response(Socket, <<>>),
    gen_tcp:close(Socket),
    Response.

recv_response(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Data} ->
            recv_response(Socket, <<Acc/binary, Data/binary>>);
        {error, closed} ->
            Acc;
        {error, timeout} ->
            Acc
    end.

split_response(Response) ->
    case binary:split(Response, <<"\r\n\r\n">>) of
        [Head, Body] -> {Head, Body};
        [Head] -> {Head, <<>>}
    end.

%% No header line of its own, and no second response
no_injected_header(Response) ->
    {Head, _Body} = split_response(Response),
    Lines = binary:split(Head, <<"\r\n">>, [global]),
    case [L || L <- Lines, binary:match(L, <<"X-Injected">>) =:= {0, 10}] of
        [] -> ok;
        Injected -> ct:fail({injected_header_line, Injected})
    end,
    case binary:matches(Response, <<"HTTP/1.">>) of
        [_] -> ok;
        Many -> ct:fail({response_split, length(Many), Response})
    end.

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

