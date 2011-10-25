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

-include("test_server.hrl").
-include("test_server_line.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(URL_START, "http://localhost:").

all(doc) ->
    ["Basic test of httpd."];

all(suite) ->
    [
     uri_too_long_414,
     header_too_long_413,
     escaped_url_in_error_body
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
    ok = inets:start(),
    PrivDir = ?config(priv_dir, Config),

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
    {ok, Fd}  = file:open(DummyFile, [write]),
    ok        = file:write(Fd, Dummy),
    ok        = file:close(Fd), 
    HttpdConf = [{port,          0}, 
		 {ipfamily,      inet}, 
		 {server_name,   "httpd_test"}, 
		 {server_root,   PrivDir},
		 {document_root, PrivDir}, 
		 {bind_address,  "localhost"}],

    [{httpd_conf, HttpdConf} |  Config].

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
end_per_testcase(_, Config) ->
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
    

%%-------------------------------------------------------------------------
%%-------------------------------------------------------------------------

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
   

%%-------------------------------------------------------------------------
%%-------------------------------------------------------------------------

escaped_url_in_error_body(doc) ->
    ["Test Url-encoding see OTP-8940"];
escaped_url_in_error_body(suite) ->
    [];
escaped_url_in_error_body(Config) when is_list(Config) ->
    tsp("escaped_url_in_error_body -> entry"),
    HttpdConf   = ?config(httpd_conf, Config),
    {ok, Pid}   = inets:start(httpd, [{port, 0} | HttpdConf]),
    Info        = httpd:info(Pid),
    Port        = proplists:get_value(port,         Info),
    _Address    = proplists:get_value(bind_address, Info),

    %% Request 1
    tsp("escaped_url_in_error_body -> request 1"),
    URL1        = ?URL_START ++ integer_to_list(Port),
    %% Make sure the server is ok, by making a request for a valid page
    case httpc:request(get, {URL1 ++ "/dummy.html", []},
		       [{url_encode,  false}, 
			{version,     "HTTP/1.0"}],
		       [{full_result, false}]) of
	{ok, {200, _}} ->
	    %% Don't care about the the body, just that we get a ok response
	    ok;
	{ok, UnexpectedOK1} ->
	    tsf({unexpected_ok_1, UnexpectedOK1})
    end,

    %% Request 2
    tsp("escaped_url_in_error_body -> request 2"),
    %% Make sure the server is ok, by making a request for a valid page
    case httpc:request(get, {URL1 ++ "/dummy.html", []},
		       [{url_encode,  true}, 
			{version,     "HTTP/1.0"}],
		       [{full_result, false}]) of
	{ok, {200, _}} ->
	    %% Don't care about the the body, just that we get a ok response
	    ok;
	{ok, UnexpectedOK2} ->
	    tsf({unexpected_ok_2, UnexpectedOK2})
    end,

    %% Request 3
    tsp("escaped_url_in_error_body -> request 3"),
    %% Ask for a non-existing page(1)
    Path            = "/<b>this_is_bold<b>",
    HTMLEncodedPath = http_util:html_encode(Path),
    URL2            = URL1 ++ Path,
    case httpc:request(get, {URL2, []},
		       [{url_encode,  true}, 
			{version,     "HTTP/1.0"}],
		       [{full_result, false}]) of
	{ok, {404, Body3}} ->
	    case find_URL_path(string:tokens(Body3, " ")) of
		HTMLEncodedPath ->
		    ok;
		BadPath3 ->
		    tsf({unexpected_path_3, HTMLEncodedPath, BadPath3})
	    end;
	{ok, UnexpectedOK3} ->
	    tsf({unexpected_ok_1, UnexpectedOK3})
    end,

    %% Request 4
    tsp("escaped_url_in_error_body -> request 4"),
    %% Ask for a non-existing page(2)
    case httpc:request(get, {URL2, []}, 
		       [{url_encode,  false}, 
			{version,     "HTTP/1.0"}],
		       [{full_result, false}]) of
	{ok, {404, Body4}} ->
	    case find_URL_path(string:tokens(Body4, " ")) of
		HTMLEncodedPath ->
		    ok;
		BadPath4 ->
		    tsf({unexpected_path_2, HTMLEncodedPath, BadPath4})
	    end;
	{ok, UnexpectedOK4} ->
	    tsf({unexpected_ok_4, UnexpectedOK4})
    end, 
    tsp("escaped_url_in_error_body -> stop inets"),
    inets:stop(httpd, Pid),
    tsp("escaped_url_in_error_body -> done"),    
    ok.

find_URL_path([]) ->
    "";
find_URL_path(["URL", URL | _]) ->
    URL;
find_URL_path([_ | Rest]) ->
    find_URL_path(Rest).

tsf(Reason) ->
    test_server:fail(Reason).


tsp(F) ->
    tsp(F, []).
tsp(F, A) ->
    Timestamp = formated_timestamp(), 
    test_server:format("** ~s ** ~p ~p:" ++ F ++ "~n", 
		       [Timestamp, self(), ?MODULE | A]).

formated_timestamp() ->
    format_timestamp( os:timestamp() ).

format_timestamp({_N1, _N2, N3} = Now) ->
    {Date, Time}   = calendar:now_to_datetime(Now),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate =
        io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
                      [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),
    lists:flatten(FormatDate).


skip(Reason) ->
    {skip, Reason}.

