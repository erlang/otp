%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2011. All Rights Reserved.
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
-module(httpd_basic_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(URL_START, "http://localhost:").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [
     uri_too_long_414, 
     header_too_long_413, 
     escaped_url_in_error_body
    ].

groups() -> 
    [].

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
    tsp("init_per_suite -> entry with"
	"~n   Config: ~p", [Config]),
    ok = inets:start(),
    PrivDir = ?config(priv_dir, Config),
    HttpdConf = [{port, 0}, {ipfamily, inet}, 
		 {server_name, "httpd_test"}, {server_root, PrivDir},
		 {document_root, PrivDir}, {bind_address, "localhost"}],
    [{httpd_conf, HttpdConf} |  Config].

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    tsp("end_per_suite -> entry with"
	"~n   Config: ~p", [_Config]),
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
init_per_testcase(Case, Config) ->
    tsp("init_per_testcase(~w) -> entry with"
	"~n   Config: ~p", [Case, Config]),
    Config.


%%--------------------------------------------------------------------
%% Function: end_per_testcase(Case, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(Case, Config) ->
    tsp("end_per_testcase(~w) -> entry with"
	"~n   Config: ~p", [Case, Config]),
    Config.


%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
uri_too_long_414(doc) ->
    ["Test that too long uri's get 414 HTTP code"];
uri_too_long_414(suite) ->
    [];
uri_too_long_414(Config) when is_list(Config) ->
    HttpdConf =   ?config(httpd_conf, Config),    
    {ok, Pid} = inets:start(httpd, [{port, 0}, {max_uri_size, 10} 
				    | HttpdConf]),
    Info = httpd:info(Pid),
    Port = proplists:get_value(port, Info),
    Address = proplists:get_value(bind_address, Info),
    ok = httpd_test_lib:verify_request(ip_comm, Address, Port, node(), 
 				       "GET /morethantenchars "
 				       "HTTP/1.1\r\n\r\n",
 				       [{statuscode, 414},
					%% Server will send lowest version
					%% as it will not get to the 
					%% client version
					%% before aborting
 				        {version, "HTTP/0.9"}]),    
    inets:stop(httpd, Pid).
    
header_too_long_413(doc) ->
    ["Test that too long headers's get 413 HTTP code"];
header_too_long_413(suite) ->
    [];
header_too_long_413(Config) when is_list(Config) ->
    HttpdConf = ?config(httpd_conf, Config), 
    {ok, Pid} = inets:start(httpd, [{port, 0}, {max_header_size, 10}
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
   
escaped_url_in_error_body(doc) ->
    ["Test Url-encoding see OTP-8940"];
escaped_url_in_error_body(suite) ->
    [];
escaped_url_in_error_body(Config) when is_list(Config) ->
    tsp("escaped_url_in_error_body -> entry with"
	"~n   Config: ~p", [Config]),
    HttpdConf = ?config(httpd_conf, Config),
    {ok, Pid} = inets:start(httpd, [{port, 0} | HttpdConf]),
    Info = httpd:info(Pid),
    Port = proplists:get_value(port, Info),
    _Address = proplists:get_value(bind_address, Info),
    Path = "/<b>this_is_bold</b>",
    URL = ?URL_START ++ integer_to_list(Port) ++ Path,
    EscapedPath = http_uri:encode(Path),
    {ok, {404, Body1}} = httpc:request(get, {URL, []},
				       [{url_encode, true}, 
					{version,    "HTTP/1.0"}],
				      [{full_result, false}]),
    EscapedPath = find_URL_path(string:tokens(Body1, " ")),
    {ok, {404, Body2}} = httpc:request(get, {URL, []},
				       [{url_encode,  false}, 
					{version,     "HTTP/1.0"}], 
				       [{full_result, false}]),
    HTMLEncodedPath = http_util:html_encode(Path),
    HTMLEncodedPath = find_URL_path(string:tokens(Body2, " ")),
    inets:stop(httpd, Pid),
    tsp("escaped_url_in_error_body -> done"),
    ok.

find_URL_path([]) ->
    "";
find_URL_path(["URL", URL | _]) ->
    URL;
find_URL_path([_ | Rest]) ->
    find_URL_path(Rest).


tsp(F) ->
    tsp(F, []).
tsp(F, A) ->
    test_server:format("~p ~p:" ++ F ++ "~n", [self(), ?MODULE | A]).

