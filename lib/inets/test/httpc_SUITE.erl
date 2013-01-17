%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2012. All Rights Reserved.
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
%% ts:run(inets, httpc_SUITE, [batch]).
%% 

-module(httpc_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("test_server_line.hrl").

-include_lib("kernel/include/file.hrl").
-include("inets_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

%% Test server specific exports
-define(IP_PORT, 8998).
-define(SSL_PORT, 8999).
-define(NOT_IN_USE_PORT, 8997).
-define(LOCAL_HOST, {127,0,0,1}).
-define(IPV6_LOCAL_HOST, "0:0:0:0:0:0:0:1").
-define(URL_START, "http://localhost:").
-define(SSL_URL_START, "https://localhost:").
-define(CR, $\r).
-define(LF, $\n).
-define(HTTP_MAX_HEADER_SIZE, 10240).


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
     http_options, 
     http_head, 
     http_get, 
     http_post,
     http_post_streaming, 
     http_dummy_pipe, 
     http_inets_pipe, 
     http_trace,
     http_async, 
     http_save_to_file, 
     http_save_to_file_async,
     http_headers, 
     http_headers_dummy, 
     http_bad_response,
     http_redirect, 
     http_redirect_loop,
     http_internal_server_error, 
     http_userinfo, http_cookie,
     http_server_does_not_exist, 
     http_invalid_http,
     http_emulate_lower_versions, 
     http_relaxed,
     page_does_not_exist, 
     parse_url, 
     options,
     headers_as_is, 
     selecting_session, 
     {group, ssl}, 
     {group, stream}, 
     {group, ipv6}, 
     {group, tickets},
     initial_server_connect
    ].

groups() -> 
    [
     {ssl,     [], [ssl_head, 
		    essl_head, 
		    ssl_get, 
		    essl_get, 
		    ssl_trace, 
		    essl_trace]}, 
     {stream,  [], [http_stream,
		    http_stream_once]},
     {tickets, [], [hexed_query_otp_6191, 
		    empty_body_otp_6243,
		    empty_response_header_otp_6830,
		    transfer_encoding_otp_6807, 
		    no_content_204_otp_6982, 
		    missing_CR_otp_7304,
		    {group, otp_7883}, 
		    {group, otp_8154}, 
		    {group, otp_8106},
		    otp_8056, 
		    otp_8352, 
		    otp_8371, 
		    otp_8739]},
     {otp_7883, [], [otp_7883_1, 
		     otp_7883_2]},
     {otp_8154, [], [otp_8154_1]},
     {otp_8106, [], [otp_8106_pid, 
		     otp_8106_fun, 
		     otp_8106_mfa]},
     {ipv6, [], [ipv6_ipcomm, ipv6_essl]}
    ].


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

    ?PRINT_SYSTEM_INFO([]),

    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    ServerRoot = filename:join(PrivDir, "server_root"),
    DocRoot = filename:join(ServerRoot, "htdocs"),
    IpConfFile = integer_to_list(?IP_PORT) ++ ".conf",
    SslConfFile = integer_to_list(?SSL_PORT) ++ ".conf",
    
    setup_server_dirs(ServerRoot, DocRoot, DataDir),
    create_config(IpConfFile, ip_comm, ?IP_PORT, PrivDir, ServerRoot, 
		  DocRoot, DataDir),
    create_config(SslConfFile, ssl, ?SSL_PORT, PrivDir, ServerRoot, 
		  DocRoot, DataDir),

    Cgi = case test_server:os_type() of
	      {win32, _} ->
		  filename:join([ServerRoot, "cgi-bin", "cgi_echo.exe"]);
	      _ ->
		  filename:join([ServerRoot, "cgi-bin", "cgi_echo"])
	  end,
    
    {ok, FileInfo} = file:read_file_info(Cgi),
    ok = file:write_file_info(Cgi, FileInfo#file_info{mode = 8#00755}),

    [{has_ipv6_support, inets_test_lib:has_ipv6_support()}, 
     {server_root,      ServerRoot}, 
     {doc_root,         DocRoot},
     {local_port,       ?IP_PORT}, 
     {local_ssl_port,   ?SSL_PORT} | Config].


%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(Config) ->
    PrivDir = ?config(priv_dir, Config), 	
    inets_test_lib:del_dirs(PrivDir),
    application:stop(inets),
    application:stop(ssl),
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

init_per_testcase(otp_8154_1 = Case, Config) ->
    init_per_testcase(Case, 5, Config);

init_per_testcase(initial_server_connect = Case, Config) ->
    %% Try to check if crypto actually exist or not, 
    %% this test case does not work unless it does
    try 
	begin
	    ?ENSURE_STARTED([crypto, public_key, ssl]),
	    inets:start(),
	    Config
	end
    catch
	throw:{error, {failed_starting, App, ActualError}} ->
	    tsp("init_per_testcase(~w) -> failed starting ~w: "
		"~n   ~p", [Case, App, ActualError]),
	    SkipString = 
		"Could not start " ++ atom_to_list(App),
	    skip(SkipString);
	  _:X ->
	    SkipString = 
		lists:flatten(
		  io_lib:format("Failed starting apps: ~p", [X])), 
	    skip(SkipString)
    end;

init_per_testcase(Case, Config) ->
    init_per_testcase(Case, 2, Config).

init_per_testcase(Case, Timeout, Config) ->
    io:format(user, 
	      "~n~n*** INIT ~w:~w[~w] ***"
	      "~n~n", [?MODULE, Case, Timeout]),

    PrivDir = ?config(priv_dir, Config),
    application:stop(inets),
    Dog         = test_server:timetrap(inets_test_lib:minutes(Timeout)),
    TmpConfig   = lists:keydelete(watchdog, 1, Config),
    IpConfFile  = integer_to_list(?IP_PORT) ++ ".conf",
    SslConfFile = integer_to_list(?SSL_PORT) ++ ".conf",

    %% inets:enable_trace(max, io, httpd),
    %% inets:enable_trace(max, io, httpc),
    %% inets:enable_trace(max, io, all),

    NewConfig = 
	case atom_to_list(Case) of
	    [$s, $s, $l | _] ->
		?ENSURE_STARTED([crypto, public_key, ssl]), 
		init_per_testcase_ssl(ssl, PrivDir, SslConfFile, 
				      [{watchdog, Dog} | TmpConfig]);

	    [$e, $s, $s, $l | _] ->
		?ENSURE_STARTED([crypto, public_key, ssl]), 
		init_per_testcase_ssl(essl, PrivDir, SslConfFile, 
				      [{watchdog, Dog} | TmpConfig]);

	    "ipv6_" ++ _Rest ->
		%% Ensure needed apps (crypto, public_key and ssl) are started
		try ?ENSURE_STARTED([crypto, public_key, ssl]) of
		    ok ->
			Profile = ipv6, 
			%% A stand-alone profile is represented by a pid()
			{ok, ProfilePid} = 
			    inets:start(httpc, 
					[{profile,  Profile}, 
					 {data_dir, PrivDir}], stand_alone),
			ok = httpc:set_options([{ipfamily, inet6}], 
					       ProfilePid), 
			tsp("httpc profile pid: ~p", [ProfilePid]),
			[{watchdog, Dog}, {profile, ProfilePid}| TmpConfig]
		catch 
		    throw:{error, {failed_starting, App, ActualError}} ->
			tsp("init_per_testcase(~w) -> failed starting ~w: "
			    "~n   ~p", [Case, App, ActualError]),
			SkipString = 
			    "Could not start " ++ atom_to_list(App),
			skip(SkipString);
		      _:X ->
			SkipString = 
			    lists:flatten(
			      io_lib:format("Failed starting apps: ~p", [X])), 
			skip(SkipString)
		end;

	    _ -> 
		%% Try inet6fb4 on windows...
		%% No need? Since it is set above?

		%% tsp("init_per_testcase -> allways try IPv6 on windows"),
		%% ?RUN_ON_WINDOWS(
		%%    fun() -> 
		%% 	   tsp("init_per_testcase:set_options_fun -> "
		%% 	       "set-option ipfamily to inet6fb4"),
		%% 	   Res = httpc:set_options([{ipfamily, inet6fb4}]),
 		%% 	   tsp("init_per_testcase:set_options_fun -> "
		%% 	       "~n   Res: ~p", [Res]),
		%% 	   Res
		%%    end),

		TmpConfig2 = lists:keydelete(local_server, 1, TmpConfig),
		%% Will start inets 
		tsp("init_per_testcase -> try start server"),
		Server = start_http_server(PrivDir, IpConfFile),
		[{watchdog, Dog}, {local_server, Server} | TmpConfig2]
	end,
    
    %% <IPv6>
    %% Set default ipfamily to the same as the main server has by default
    %% This makes the client try w/ ipv6 before falling back to ipv4,
    %% as that is what the server is configured to do.
    %% Note that this is required for the tests to run on *BSD w/ ipv6 enabled
    %% as well as on Windows. The Linux behaviour of allowing ipv4 connects
    %% to ipv6 sockets is not required or even encouraged.

    tsp("init_per_testcase -> Options before ipfamily set: ~n~p", 
	[httpc:get_options(all)]),
    ok = httpc:set_options([{ipfamily, inet6fb4}]),
    tsp("init_per_testcase -> Options after ipfamily set: ~n~p", 
	[httpc:get_options(all)]),

    %% Note that the IPv6 test case(s) *must* use inet6, 
    %% so this value will be overwritten (see "ipv6_" below).
    %% </IPv6>

    %% inets:enable_trace(max, io, all),
    %% snmp:set_trace([gen_tcp]),
    tsp("init_per_testcase(~w) -> done when"
	"~n   NewConfig:  ~p"
	"~n~n", [Case, NewConfig]),
    NewConfig.


init_per_testcase_ssl(Tag, PrivDir, SslConfFile, Config) ->
    tsp("init_per_testcase_ssl(~w) -> stop ssl", [Tag]),
    application:stop(ssl),
    Config2 = lists:keydelete(local_ssl_server, 1, Config),
    %% Will start inets 
    tsp("init_per_testcase_ssl(~w) -> try start http server (including inets)",
	[Tag]),
    Server = inets_test_lib:start_http_server(
	       filename:join(PrivDir, SslConfFile), Tag),
    tsp("init_per_testcase(~w) -> Server: ~p", [Tag, Server]),
    [{local_ssl_server, Server} | Config2].

start_http_server(ConfDir, ConfFile) ->
    inets_test_lib:start_http_server( filename:join(ConfDir, ConfFile) ).


%%--------------------------------------------------------------------
%% Function: end_per_testcase(Case, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(http_save_to_file = Case, Config) ->
    io:format(user, "~n~n*** END ~w:~w ***~n~n", 
	      [?MODULE, Case]),
    PrivDir  = ?config(priv_dir, Config), 	
    FullPath = filename:join(PrivDir, "dummy.html"),
    file:delete(FullPath),
    finish(Config);
	
end_per_testcase(Case, Config) ->
    io:format(user, "~n~n*** END ~w:~w ***~n~n", 
	      [?MODULE, Case]),
    case atom_to_list(Case) of
	"ipv6_" ++ _Rest ->
	    tsp("end_per_testcase(~w) -> stop ssl", [Case]),
	    application:stop(ssl),
	    tsp("end_per_testcase(~w) -> stop public_key", [Case]),
	    application:stop(public_key),
	    tsp("end_per_testcase(~w) -> stop crypto", [Case]),
	    application:stop(crypto),
	    ProfilePid = ?config(profile, Config), 
	    tsp("end_per_testcase(~w) -> stop httpc profile (~p)", 
		[Case, ProfilePid]),
	    unlink(ProfilePid),
	    inets:stop(stand_alone, ProfilePid),
	    tsp("end_per_testcase(~w) -> httpc profile (~p) stopped", 
		[Case, ProfilePid]),
	    ok;
	_ ->
	    ok
    end,
    finish(Config).

finish(Config) ->
    Dog = ?config(watchdog, Config),
    case Dog of 
	undefined ->
	    ok;
	_ ->
	    tsp("finish -> stop watchdog (~p)", [Dog]),
	    test_server:timetrap_cancel(Dog)
    end.

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------



%%-------------------------------------------------------------------------

http_options(doc) ->
    ["Test http options request against local server."];
http_options(suite) ->
    [];
http_options(Config) when is_list(Config) ->
    skip("Not supported by httpd").

http_head(doc) ->
    ["Test http head request against local server."];
http_head(suite) ->
    [];
http_head(Config) when is_list(Config) ->
    tsp("http_head -> entry with"
	"~n   Config: ~p", [Config]),
    Method   = head, 
    Port     = ?config(local_port, Config),
    URL      = ?URL_START ++ integer_to_list(Port) ++ "/dummy.html",
    Request  = {URL, []}, 
    HttpOpts = [], 
    Opts     = [], 
    VerifyResult = 
	fun({ok, {{_,200,_}, [_ | _], []}}) ->
		ok;
	   ({ok, UnexpectedReply}) ->
		tsp("http_head:verify_fun -> Unexpected Reply: "
		    "~n   ~p", [UnexpectedReply]),
		tsf({unexpected_reply, UnexpectedReply});
	   ({error, Reason} = Error) ->
		tsp("http_head:verify_fun -> Error reply: "
		    "~n   Reason: ~p", [Reason]),
		tsf({bad_reply, Error})
	end,
    simple_request_and_verify(Config, 
			      Method, Request, HttpOpts, Opts, VerifyResult).


%%-------------------------------------------------------------------------

http_get(doc) ->
    ["Test http get request against local server"];
http_get(suite) ->
    [];
http_get(Config) when is_list(Config) ->
    tsp("http_get -> entry with"
	"~n   Config: ~p", [Config]),
    case ?config(local_server, Config) of 
	ok ->
	    tsp("local-server running"),
	    Method       = get, 
	    Port         = ?config(local_port, Config),
	    URL          = ?URL_START ++ integer_to_list(Port) ++ "/dummy.html",
	    Request      = {URL, []}, 
	    Timeout      = timer:seconds(1), 
	    ConnTimeout  = Timeout + timer:seconds(1), 
	    HttpOptions1 = [{timeout,         Timeout}, 
			    {connect_timeout, ConnTimeout}], 
	    Options1     = [], 
	    Body = 
		case httpc:request(Method, Request, HttpOptions1, Options1) of
		    {ok, {{_,200,_}, [_ | _], ReplyBody = [_ | _]}} ->
			ReplyBody;
		    {ok, UnexpectedReply1} ->
			tsf({unexpected_reply, UnexpectedReply1});
		    {error, _} = Error1 ->
			tsf({bad_reply, Error1})
		end,

	    %% eqvivivalent to httpc:request(get, {URL, []}, [], []),
	    inets_test_lib:check_body(Body),

	    HttpOptions2 = [], 
	    Options2     = [{body_format, binary}], 
	    case httpc:request(Method, Request, HttpOptions2, Options2) of
		{ok, {{_,200,_}, [_ | _], Bin}} when is_binary(Bin) ->
		    ok;
		{ok, {{_,200,_}, [_ | _], BadBin}} ->
		    tsf({body_format_not_binary, BadBin});
		{ok,  UnexpectedReply2} ->
		    tsf({unexpected_reply, UnexpectedReply2});
		{error, _} = Error2 ->
		    tsf({bad_reply, Error2})
	    end;
	_ ->
	    skip("Failed to start local http-server")
    end.  

%%-------------------------------------------------------------------------

http_post(doc) ->
    ["Test http post request against local server. We do in this case "
     "only care about the client side of the the post. The server "
     "script will not actually use the post data."];
http_post(suite) ->
    [];
http_post(Config) when is_list(Config) ->
  case ?config(local_server, Config) of 
      ok -> 
	  Port = ?config(local_port, Config),
	  
	  URL = case test_server:os_type() of
		    {win32, _} ->
			?URL_START ++ integer_to_list(Port) ++ 
			    "/cgi-bin/cgi_echo.exe";
		    _ ->
			?URL_START ++ integer_to_list(Port) ++ 
			    "/cgi-bin/cgi_echo"	       
		
		end,
	  %% Cgi-script expects the body length to be 100 
	  Body = lists:duplicate(100, "1"),
	  
	  {ok, {{_,200,_}, [_ | _], [_ | _]}} =
	      httpc:request(post, {URL, [{"expect","100-continue"}],
				  "text/plain", Body}, [], []),
      
	  {ok, {{_,504,_}, [_ | _], []}} =
	      httpc:request(post, {URL, [{"expect","100-continue"}],
				  "text/plain", "foobar"}, [], []);
      _ ->
	  skip("Failed to start local http-server")
  end.  

%%-------------------------------------------------------------------------
http_post_streaming(doc) ->
    ["Test streaming http post request against local server. "
     "We only care about the client side of the the post. "
     "The server script will not actually use the post data."];
http_post_streaming(suite) ->
    [];
http_post_streaming(Config) when is_list(Config) ->
    case ?config(local_server, Config) of
        ok ->
            Port = ?config(local_port, Config),
            URL = case test_server:os_type() of
                {win32, _} ->
                    ?URL_START ++ integer_to_list(Port) ++
                        "/cgi-bin/cgi_echo.exe";
                 _ ->
                    ?URL_START ++ integer_to_list(Port) ++
                        "/cgi-bin/cgi_echo"
            end,
            %% Cgi-script expects the body length to be 100
            BodyFun = fun(0) ->
			      io:format("~w:http_post_streaming_fun -> "
					"zero~n", [?MODULE]),
			      eof;
			 (LenLeft) ->
			      io:format("~w:http_post_streaming_fun -> "
					"LenLeft: ~p~n", [?MODULE, LenLeft]),
			      {ok, lists:duplicate(10, "1"), LenLeft - 10}
		      end,
	    
            {ok, {{_,200,_}, [_ | _], [_ | _]}} =
		httpc:request(post, {URL,
				     [{"expect", "100-continue"}, 
				      {"content-length", "100"}],
				     "text/plain", {BodyFun, 100}}, [], []),
	    
            {ok, {{_,504,_}, [_ | _], []}} =
		httpc:request(post, {URL,
				     [{"expect", "100-continue"}, 
				      {"content-length", "10"}],
				     "text/plain", {BodyFun, 10}}, [], []);
	
      _ ->
          skip("Failed to start local http-server")
    end.


%%-------------------------------------------------------------------------
http_emulate_lower_versions(doc) ->
    ["Perform request as 0.9 and 1.0 clients."];
http_emulate_lower_versions(suite) ->
    [];
http_emulate_lower_versions(Config) when is_list(Config) ->
    case ?config(local_server, Config) of 
	ok ->
	    Port = ?config(local_port, Config),
	    URL = ?URL_START ++ integer_to_list(Port) ++ "/dummy.html",
	    {ok, Body0} =
  		httpc:request(get, {URL, []}, [{version, "HTTP/0.9"}], []),
	    inets_test_lib:check_body(Body0),
 	    {ok, {{"HTTP/1.0", 200, _}, [_ | _], Body1 = [_ | _]}} =
		httpc:request(get, {URL, []}, [{version, "HTTP/1.0"}], []),
	    inets_test_lib:check_body(Body1),
	    {ok, {{"HTTP/1.1", 200, _}, [_ | _], Body2 = [_ | _]}} =
		httpc:request(get, {URL, []}, [{version, "HTTP/1.1"}], []),
	    inets_test_lib:check_body(Body2);
        _->
	    skip("Failed to start local http-server")
    end.


%%-------------------------------------------------------------------------

http_relaxed(doc) ->
    ["Test relaxed mode"];
http_relaxed(suite) ->
    [];
http_relaxed(Config) when is_list(Config) ->
    ok = httpc:set_options([{ipv6, disabled}]), % also test the old option 
    %% ok = httpc:set_options([{ipfamily, inet}]),
    {DummyServerPid, Port} = dummy_server(ipv4),
    
    URL = ?URL_START ++ integer_to_list(Port) ++ 
	"/missing_reason_phrase.html",
        
    {error, Reason} =
	httpc:request(get, {URL, []}, [{relaxed, false}], []),

    test_server:format("Not relaxed: ~p~n", [Reason]),
    
    {ok, {{_, 200, _}, [_ | _], [_ | _]}} =
	httpc:request(get, {URL, []}, [{relaxed, true}], []),

    DummyServerPid ! stop,
    ok = httpc:set_options([{ipv6, enabled}]),   
    %% ok = httpc:set_options([{ipfamily, inet6fb4}]), 
    ok.


%%-------------------------------------------------------------------------
http_dummy_pipe(doc) ->
    ["Test pipelining code."];
http_dummy_pipe(suite) ->
    [];
http_dummy_pipe(Config) when is_list(Config) ->
    ok = httpc:set_options([{ipfamily, inet}]),
    {DummyServerPid, Port} = dummy_server(ipv4),
    
    URL = ?URL_START ++ integer_to_list(Port) ++ "/foobar.html",

    test_pipeline(URL),

    DummyServerPid ! stop,
    ok = httpc:set_options([{ipfamily, inet6fb4}]), 
    ok.

http_inets_pipe(doc) ->
    ["Test pipelining code."];
http_inets_pipe(suite) ->
    [];
http_inets_pipe(Config) when is_list(Config) ->
    
    case ?config(local_server, Config) of 
	ok ->
	    Port = ?config(local_port, Config),
	    URL = ?URL_START ++ integer_to_list(Port) ++ "/dummy.html",
	    test_pipeline(URL); 
	_ ->
	    skip("Failed to start local http-server")
    end.


test_pipeline(URL) ->
    p("test_pipeline -> entry with"
      "~n   URL: ~p", [URL]),
    
    httpc:set_options([{pipeline_timeout, 50000}]),
    
    p("test_pipeline -> issue (async) request 1"
      "~n   when profile info: ~p", [httpc:info()]),
    {ok, RequestIdA1} =
	httpc:request(get, {URL, []}, [], [{sync, false}]),
    tsp("RequestIdA1: ~p", [RequestIdA1]),
    p("test_pipeline -> RequestIdA1: ~p"
      "~n   when profile info: ~p", [RequestIdA1, httpc:info()]),
    
    %% Make sure pipeline is initiated
    p("test_pipeline -> sleep some", []),
    test_server:sleep(4000),
    
    p("test_pipeline -> issue (async) request A2, A3 and A4"
      "~n   when profile info: ~p", [httpc:info()]),
    {ok, RequestIdA2} =
	httpc:request(get, {URL, []}, [], [{sync, false}]),
    {ok, RequestIdA3} =
	httpc:request(get, {URL, []}, [], [{sync, false}]),
    {ok, RequestIdA4} =
	httpc:request(get, {URL, []}, [], [{sync, false}]),
    tsp("RequestIdAs => A2: ~p, A3: ~p and A4: ~p", 
	[RequestIdA2, RequestIdA3, RequestIdA4]),
    p("test_pipeline -> RequestIds => A2: ~p, A3: ~p and A4: ~p"
      "~n   when profile info: ~p", 
      [RequestIdA2, RequestIdA3, RequestIdA4, httpc:info()]),
    
    p("test_pipeline -> issue (sync) request 3"),
    {ok, {{_,200,_}, [_ | _], [_ | _]}} =
	httpc:request(get, {URL, []}, [], []),
    
    p("test_pipeline -> expect reply for (async) request A1, A2, A3 and A4"
      "~n   when profile info: ~p", [httpc:info()]),
    pipeline_await_async_reply([{RequestIdA1, a1, 200}, 
				{RequestIdA2, a2, 200}, 
				{RequestIdA3, a3, 200}, 
				{RequestIdA4, a4, 200}], ?MINS(1)),

    p("test_pipeline -> sleep some"
      "~n   when profile info: ~p", [httpc:info()]),
    test_server:sleep(4000),
    
    p("test_pipeline -> issue (async) request B1, B2, B3 and B4"
      "~n   when profile info: ~p", [httpc:info()]),
    {ok, RequestIdB1} =
	httpc:request(get, {URL, []}, [], [{sync, false}]),
    {ok, RequestIdB2} =
	httpc:request(get, {URL, []}, [], [{sync, false}]),
    {ok, RequestIdB3} =
	httpc:request(get, {URL, []}, [], [{sync, false}]),
    {ok, RequestIdB4} =
	httpc:request(get, {URL, []}, [], [{sync, false}]),
    tsp("RequestIdBs => B1: ~p, B2: ~p, B3: ~p and B4: ~p", 
	[RequestIdB1, RequestIdB2, RequestIdB3, RequestIdB4]),
    p("test_pipeline -> RequestIdBs => B1: ~p, B2: ~p, B3: ~p and B4: ~p"
      "~n   when profile info: ~p", 
      [RequestIdB1, RequestIdB2, RequestIdB3, RequestIdB4, httpc:info()]),
    
    p("test_pipeline -> cancel (async) request B2"
      "~n   when profile info: ~p", [httpc:info()]),
    ok = httpc:cancel_request(RequestIdB2),
    
    p("test_pipeline -> "
      "expect *no* reply for cancelled (async) request B2 (for 3 secs)"
      "~n   when profile info: ~p", [httpc:info()]),
    receive
	{http, {RequestIdB2, _}} ->
	    tsf(http_cancel_request_failed)
    after 3000 ->
	    ok
    end,
    
    p("test_pipeline -> expect reply for (async) request B1, B3 and B4"
      "~n   when profile info: ~p", [httpc:info()]),
    Bodies = pipeline_await_async_reply([{RequestIdB1, b1, 200}, 
					 {RequestIdB3, b3, 200}, 
					 {RequestIdB4, b4, 200}], ?MINS(1)),
    [{b1, Body}|_] = Bodies, 

    p("test_pipeline -> check reply for (async) request B1"
      "~n   when profile info: ~p", [httpc:info()]),
    inets_test_lib:check_body(binary_to_list(Body)),
    
    p("test_pipeline -> ensure no unexpected incomming"
      "~n   when profile info: ~p", [httpc:info()]),
    receive
	{http, Any} ->
	    tsf({unexpected_message, Any})
    after 500 ->
	    ok
    end,
    
    p("test_pipeline -> done"
      "~n   when profile info: ~p", [httpc:info()]),
    ok.

pipeline_await_async_reply(ReqIds, Timeout) ->
    pipeline_await_async_reply(ReqIds, Timeout, []).

pipeline_await_async_reply([], _, Acc) ->
    lists:keysort(1, Acc);
pipeline_await_async_reply(ReqIds, Timeout, Acc) when Timeout > 0 ->
    T1 = inets_test_lib:timestamp(), 
    p("pipeline_await_async_reply -> await replies"
      "~n   ReqIds:  ~p"
      "~n   Timeout: ~p", [ReqIds, Timeout]),
    receive
	{http, {RequestId, {{_, Status, _}, _, Body}}} ->
	    p("pipeline_await_async_reply -> received reply for"
	      "~n   RequestId: ~p"
	      "~n   Status:    ~p", [RequestId, Status]),
	    case lists:keysearch(RequestId, 1, ReqIds) of
		{value, {RequestId, N, Status}} ->
		    p("pipeline_await_async_reply -> "
		      "found expected request ~w", [N]),
		    ReqIds2 = lists:keydelete(RequestId, 1, ReqIds),
		    NewTimeout = Timeout - (inets_test_lib:timestamp()-T1), 
		    pipeline_await_async_reply(ReqIds2, NewTimeout, 
					      [{N, Body} | Acc]);
		{value, {RequestId, N, WrongStatus}} ->
		    p("pipeline_await_async_reply -> "
		      "found request ~w with wrong status", [N]),
		    tsf({reply_with_unexpected_status, 
			 {RequestId, N, WrongStatus}});
		false ->
		    tsf({unexpected_reply, {RequestId, Status}})
	    end;
	{http, Msg} ->
	    tsf({unexpected_reply, Msg})
    after Timeout ->
	    receive 
		Any ->
		    tsp("pipeline_await_async_reply -> "
			"received unknown data after timeout: "
			"~n   ~p", [Any]),
		    tsf({timeout, {unknown, Any}})
	    end
    end;
pipeline_await_async_reply(ReqIds, _, Acc) ->
    tsp("pipeline_await_async_reply -> "
	"timeout: "
	"~n   ~p"
	"~nwhen"
	"~n   ~p", [ReqIds, Acc]),
    tsf({timeout, ReqIds, Acc}).
    

    
%%-------------------------------------------------------------------------
http_trace(doc) ->
    ["Perform a TRACE request."];
http_trace(suite) ->
    [];
http_trace(Config) when is_list(Config) ->
    case ?config(local_server, Config) of 
	ok ->
	    Port = ?config(local_port, Config),
	    URL = ?URL_START ++ integer_to_list(Port) ++ "/dummy.html",
	    case httpc:request(trace, {URL, []}, [], []) of
		{ok, {{_,200,_}, [_ | _], "TRACE /dummy.html" ++ _}} ->
		    ok;
		{ok, {{_,200,_}, [_ | _], WrongBody}} ->
		    tsf({wrong_body, WrongBody});
		{ok, WrongReply} ->
		    tsf({wrong_reply, WrongReply});
		Error ->
		    tsf({failed, Error})
	    end;
	_ ->
	    skip("Failed to start local http-server")
    end.  
%%-------------------------------------------------------------------------
http_async(doc) ->
    ["Test an asynchrony http request."];
http_async(suite) ->
    [];
http_async(Config) when is_list(Config) ->
    case ?config(local_server, Config) of 
	ok ->
	    Port = ?config(local_port, Config),
	    URL = ?URL_START ++ integer_to_list(Port) ++ "/dummy.html",
	    {ok, RequestId} = 
		httpc:request(get, {URL, []}, [], [{sync, false}]),
	    
	    Body = 
		receive 
		    {http, {RequestId, {{_, 200, _}, _, BinBody}}} ->
			BinBody;
		    {http, Msg} ->
			tsf(Msg)
		end,
	    
	    inets_test_lib:check_body(binary_to_list(Body)),
	    
	    {ok, NewRequestId} = 
		httpc:request(get, {URL, []}, [], [{sync, false}]),
	    ok = httpc:cancel_request(NewRequestId),
	    receive 
		{http, {NewRequestId, _NewResult}} ->
		    tsf(http_cancel_request_failed)
	    after 3000 ->
		    ok
	    end;
	_ ->
	    skip("Failed to start local http-server")
    end.  

%%-------------------------------------------------------------------------
http_save_to_file(doc) ->
    ["Test to save the http body to a file"];
http_save_to_file(suite) ->
    [];
http_save_to_file(Config) when is_list(Config) ->
    case ?config(local_server, Config) of 
	ok ->
	    PrivDir = ?config(priv_dir, Config),
	    FilePath = filename:join(PrivDir, "dummy.html"),
	    Port = ?config(local_port, Config),
	    URL = ?URL_START ++ integer_to_list(Port) ++ "/dummy.html",
	    {ok, saved_to_file} 
		= httpc:request(get, {URL, []}, [], [{stream, FilePath}]),
	    {ok, Bin} = file:read_file(FilePath), 
	    {ok, {{_,200,_}, [_ | _], Body}} = httpc:request(URL),
	    Bin == Body;
	_ ->
	    skip("Failed to start local http-server")
    end.  


%%-------------------------------------------------------------------------
http_save_to_file_async(doc) ->
    ["Test to save the http body to a file"];
http_save_to_file_async(suite) ->
    [];
http_save_to_file_async(Config) when is_list(Config) ->
    case ?config(local_server, Config) of 
	ok ->
	    PrivDir = ?config(priv_dir, Config),
	    FilePath = filename:join(PrivDir, "dummy.html"),
	    Port = ?config(local_port, Config),
	    URL = ?URL_START ++ integer_to_list(Port) ++ "/dummy.html",
	    {ok, RequestId} = httpc:request(get, {URL, []}, [], 
					   [{stream, FilePath}, 
					    {sync, false}]),
	    receive
		{http, {RequestId, saved_to_file}} ->
		    ok;
		{http, Msg} ->
		    tsf(Msg)
	    end,

	    {ok, Bin} = file:read_file(FilePath), 
	    {ok, {{_,200,_}, [_ | _], Body}} = httpc:request(URL),
	    Bin == Body;
	_ ->
	    skip("Failed to start local http-server")
    end.  
%%-------------------------------------------------------------------------
http_headers(doc) ->
    ["Use as many request headers as possible not used in proxy_headers"];
http_headers(suite) ->
    [];
http_headers(Config) when is_list(Config) ->
    
    case ?config(local_server, Config) of 
	ok ->
	    Port = ?config(local_port, Config),
	    URL = ?URL_START ++ integer_to_list(Port) ++ "/dummy.html",
	    DocRoot = ?config(doc_root, Config),
	    {ok, FileInfo} = 
		file:read_file_info(filename:join([DocRoot,"dummy.html"])),
	    CreatedSec = 
		calendar:datetime_to_gregorian_seconds(
		  FileInfo#file_info.mtime),
	    
	    Mod = httpd_util:rfc1123_date(
		    calendar:gregorian_seconds_to_datetime(
		      CreatedSec-1)),
	    
	    Date = httpd_util:rfc1123_date({date(), time()}),
	    
	    {ok, {{_,200,_}, [_ | _], [_ | _]}} =
		httpc:request(get, {URL, [{"If-Modified-Since",
					  Mod}, 
					 {"From","webmaster@erlang.se"},
					 {"Date", Date}
					]}, [], []),
	    
	    Mod1 =  httpd_util:rfc1123_date(
		      calendar:gregorian_seconds_to_datetime(
			CreatedSec+1)),
	    
	    {ok, {{_,200,_}, [_ | _], [_ | _]}} =
		httpc:request(get, {URL, [{"If-UnModified-Since",
					  Mod1}
					]}, [], []),
	    
	    Tag = httpd_util:create_etag(FileInfo),
	    
	    
	    {ok, {{_,200,_}, [_ | _], [_ | _]}} =
		httpc:request(get, {URL, [{"If-Match",
					  Tag}
					]}, [], []),

	    {ok, {{_,200,_}, [_ | _], _}} =
		     httpc:request(get, {URL, [{"If-None-Match",
					       "NotEtag,NeihterEtag"},
					      {"Connection", "Close"}
					     ]}, [], []),
	    ok;
		     _ ->
	    skip("Failed to start local http-server")
    end.

%%-------------------------------------------------------------------------
http_headers_dummy(doc) ->
    ["Test the code for handling headers we do not want/can send "
     "to a real server. Note it is not logical to send"
     "all of these headers together, we only want to test that" 
     "the code for handling headers will not crash."];
http_headers_dummy(suite) ->
    [];
http_headers_dummy(Config) when is_list(Config) -> 
    ok = httpc:set_options([{ipfamily, inet}]),
    {DummyServerPid, Port} = dummy_server(ipv4),
    
    URL = ?URL_START ++ integer_to_list(Port) ++ "/dummy_headers.html",
    
    Foo = http_chunk:encode("foobar") ++ 
	binary_to_list(http_chunk:encode_last()),
    FooBar =  Foo ++ "\r\n\r\nOther:inets_test\r\n\r\n",

    UserPasswd = base64:encode_to_string("Alladin:Sesame"),
    Auth = "Basic " ++ UserPasswd,

    %% The dummy server will ignore the headers, we only want to test
    %% that the client header-handling code. This would not
    %% be a vaild http-request!
    {ok, {{_,200,_}, [_ | _], [_|_]}} = 
	httpc:request(post, 
		     {URL, 
		      [{"Via",
			"1.0 fred, 1.1 nowhere.com (Apache/1.1)"}, 
		       {"Warning","1#pseudonym foobar"},
		       {"Vary","*"},
		       {"Upgrade","HTTP/2.0"},
		       {"Pragma", "1#no-cache"},
		       {"Cache-Control", "no-cache"},
		       {"Connection", "close"},
		       {"Date", "Sat, 29 Oct 1994 19:43:31 GMT"},
		       {"Accept", " text/plain; q=0.5, text/html"},
		       {"Accept-Language", "en"},
		       {"Accept-Encoding","chunked"},
		       {"Accept-Charset", "ISO8859-1"},
		       {"Authorization", Auth},
		       {"Expect", "1#100-continue"},
		       {"User-Agent","inets"},
		       {"Transfer-Encoding","chunked"},
		       {"Range", " bytes=0-499"},
		       {"If-Range", "Sat, 29 Oct 1994 19:43:31 GMT"},
		       {"If-Match", "*"},
		       {"Content-Type", "text/plain"},
		       {"Content-Encoding", "chunked"},
		       {"Content-Length", "6"},
		       {"Content-Language", "en"},
		       {"Content-Location", "http://www.foobar.se"},
		       {"Content-MD5", 
			"104528739076276072743283077410617235478"},
		       {"Content-Range", "bytes 0-499/1234"},
		       {"Allow", "GET"},
		       {"Proxy-Authorization", Auth},
		       {"Expires", "Sat, 29 Oct 1994 19:43:31 GMT"},
		       {"Upgrade", "HTTP/2.0"},
		       {"Last-Modified", "Sat, 29 Oct 1994 19:43:31 GMT"},
		       {"Trailer","1#User-Agent"}
		      ], "text/plain", FooBar}, 
		     [], []),
    DummyServerPid ! stop,
    ok = httpc:set_options([{ipfamily, inet6fb4}]), 
    ok.
    

%%-------------------------------------------------------------------------
http_bad_response(doc) ->
    ["Test what happens when the server does not follow the protocol"];
http_bad_response(suite) ->
    [];
http_bad_response(Config) when is_list(Config) ->
    ok = httpc:set_options([{ipfamily, inet}]),
    {DummyServerPid, Port} = dummy_server(ipv4),
    
    URL = ?URL_START ++ integer_to_list(Port) ++ "/missing_crlf.html",
    
    URL1 = ?URL_START ++ integer_to_list(Port) ++ "/wrong_statusline.html",
    
    {error, timeout} = httpc:request(get, {URL, []}, [{timeout, 400}], []),
      
    {error, Reason} = httpc:request(URL1),
    
    test_server:format("Wrong Statusline: ~p~n", [Reason]),

    DummyServerPid ! stop,
    ok = httpc:set_options([{ipfamily, inet6fb4}]), 
    ok.


%%-------------------------------------------------------------------------
ssl_head(doc) ->
    ["Same as http_head/1 but over ssl sockets."];
ssl_head(suite) ->
    [];
ssl_head(Config) when is_list(Config) ->   
    ssl_head(ssl, Config).

essl_head(doc) ->
    ["Same as http_head/1 but over ssl sockets."];
essl_head(suite) ->
    [];
essl_head(Config) when is_list(Config) ->   
    ssl_head(essl, Config).

ssl_head(SslTag, Config) ->
    tsp("ssl_head -> entry with"
	"~n   SslTag: ~p"
	"~n   Config: ~p", [SslTag, Config]), 
    case ?config(local_ssl_server, Config) of 
 	ok ->
	    DataDir    = ?config(data_dir, Config),
	    Port       = ?config(local_ssl_port, Config),
	    URL        = ?SSL_URL_START ++ integer_to_list(Port) ++ "/dummy.html",
	    CertFile   = filename:join(DataDir, "ssl_client_cert.pem"),
	    SSLOptions = [{certfile, CertFile}, {keyfile, CertFile}],
	    SSLConfig     = 
		case SslTag of
		    ssl ->
			SSLOptions;
		    essl ->
			{essl, SSLOptions}
		end,
	    tsp("ssl_head -> make request using: "
		"~n   URL:        ~p"
		"~n   SslTag:     ~p"
		"~n   SSLOptions: ~p", [URL, SslTag, SSLOptions]),
	    {ok, {{_,200, _}, [_ | _], []}} =
		httpc:request(head, {URL, []}, [{ssl, SSLConfig}], []);
 	{ok, _} ->
 	    skip("local http-server not started");
 	_ ->
 	    skip("SSL not started")
    end.  

    
%%-------------------------------------------------------------------------
ssl_get(doc) ->
    ["Same as http_get/1 but over ssl sockets."];
ssl_get(suite) ->
    [];
ssl_get(Config) when is_list(Config) ->
    ssl_get(ssl, Config).

essl_get(doc) ->
    ["Same as http_get/1 but over ssl sockets."];
essl_get(suite) ->
    [];
essl_get(Config) when is_list(Config) ->
    ssl_get(essl, Config).

ssl_get(SslTag, Config) when is_list(Config) ->
    case ?config(local_ssl_server, Config) of 
	ok ->
	    DataDir    = ?config(data_dir, Config),
 	    Port       = ?config(local_ssl_port, Config),
	    URL        = ?SSL_URL_START ++ integer_to_list(Port) ++ "/dummy.html",
	    CertFile   = filename:join(DataDir, "ssl_client_cert.pem"),
	    SSLOptions = [{certfile, CertFile}, {keyfile, CertFile}],
	    SSLConfig  = 
		case SslTag of
		    ssl ->
			SSLOptions;
		    essl ->
			{essl, SSLOptions}
		end,
	    tsp("ssl_get -> make request using: "
		"~n   URL:        ~p"
		"~n   SslTag:     ~p"
		"~n   SSLOptions: ~p", [URL, SslTag, SSLOptions]),
	    case httpc:request(get, {URL, []}, [{ssl, SSLConfig}], []) of
		{ok, {{_,200, _}, [_ | _], Body = [_ | _]}} ->
		    inets_test_lib:check_body(Body),
		    ok;
		{ok, {StatusLine, Headers, _Body}} ->
		    tsp("ssl_get -> unexpected result: "
			"~n   StatusLine: ~p"
			"~n   Headers:    ~p", [StatusLine, Headers]),
		    tsf({unexpected_response, StatusLine, Headers});
		{error, Reason} ->
		    tsp("ssl_get -> request failed: "
			"~n   Reason: ~p", [Reason]),
		    tsf({request_failed, Reason})
	    end;
	 {ok, _} ->
	    skip("local http-server not started"); 
	 _ ->
	    skip("SSL not started")
     end.


%%-------------------------------------------------------------------------
ssl_trace(doc) ->
    ["Same as http_trace/1 but over ssl sockets."];
ssl_trace(suite) ->
    [];
ssl_trace(Config) when is_list(Config) ->
    ssl_trace(ssl, Config).

essl_trace(doc) ->
    ["Same as http_trace/1 but over ssl sockets."];
essl_trace(suite) ->
    [];
essl_trace(Config) when is_list(Config) ->
    ssl_trace(essl, Config).

ssl_trace(SslTag, Config) when is_list(Config) ->
    case ?config(local_ssl_server, Config) of 
	ok ->
	    DataDir    = ?config(data_dir, Config),
 	    Port       = ?config(local_ssl_port, Config),
	    URL        = ?SSL_URL_START ++ integer_to_list(Port) ++ "/dummy.html",
	    CertFile   = filename:join(DataDir, "ssl_client_cert.pem"),
	    SSLOptions = [{certfile, CertFile}, {keyfile, CertFile}],
	    SSLConfig  = 
		case SslTag of
		    ssl ->
			SSLOptions;
		    essl ->
			{essl, SSLOptions}
		end,
	    tsp("ssl_trace -> make request using: "
		"~n   URL:        ~p"
		"~n   SslTag:     ~p"
		"~n   SSLOptions: ~p", [URL, SslTag, SSLOptions]),
	    case httpc:request(trace, {URL, []}, [{ssl, SSLConfig}], []) of
		{ok, {{_,200, _}, [_ | _], "TRACE /dummy.html" ++ _}} ->
		    ok;
		{ok, {{_,200,_}, [_ | _], WrongBody}} ->
		    tsf({wrong_body,  WrongBody});
		{ok, WrongReply} ->
		    tsf({wrong_reply, WrongReply});
		Error ->
		    tsf({failed, Error})
	    end;
	{ok, _} ->
	    skip("local http-server not started"); 
	_ ->
	    skip("SSL not started")
    end.


%%-------------------------------------------------------------------------
http_redirect(doc) ->
    ["Test redirect with dummy server as httpd does not implement"
     " server redirect"];
http_redirect(suite) ->
    [];
http_redirect(Config) when is_list(Config) ->
    tsp("http_redirect -> entry with"
	"~n   Config: ~p", [Config]),
    case ?config(local_server, Config) of 
	ok ->
	    %% tsp("http_redirect -> set ipfamily option to inet"),
	    %% ok = httpc:set_options([{ipfamily, inet}]),

	    tsp("http_redirect -> start dummy server inet"),
	    {DummyServerPid, Port} = dummy_server(ipv4),
	    tsp("http_redirect -> server port = ~p", [Port]),
    
	    URL300 = ?URL_START ++ integer_to_list(Port) ++ "/300.html",
    
	    tsp("http_redirect -> issue request 1: "
		"~n   ~p", [URL300]),
	    {ok, {{_,200,_}, [_ | _], [_|_]}} 
 		= httpc:request(get, {URL300, []}, [], []),
	    
	    tsp("http_redirect -> issue request 2: "
		"~n   ~p", [URL300]),
	    {ok, {{_,300,_}, [_ | _], _}} = 
		httpc:request(get, {URL300, []}, [{autoredirect, false}], []),

	    URL301 = ?URL_START ++ integer_to_list(Port) ++ "/301.html",

	    tsp("http_redirect -> issue request 3: "
		"~n   ~p", [URL301]),
	    {ok, {{_,200,_}, [_ | _], [_|_]}} 
 		= httpc:request(get, {URL301, []}, [], []),
	    
	    tsp("http_redirect -> issue request 4: "
		"~n   ~p", [URL301]),
	    {ok, {{_,200,_}, [_ | _], []}} 
 		= httpc:request(head, {URL301, []}, [], []),
	    
	    tsp("http_redirect -> issue request 5: "
		"~n   ~p", [URL301]),
	    {ok, {{_,301,_}, [_ | _], [_|_]}} 
 		= httpc:request(post, {URL301, [],"text/plain", "foobar"},
			       [], []),

	    URL302 = ?URL_START ++ integer_to_list(Port) ++ "/302.html",
	 
	    tsp("http_redirect -> issue request 6: "
		"~n   ~p", [URL302]),
	    {ok, {{_,200,_}, [_ | _], [_|_]}} 
 		= httpc:request(get, {URL302, []}, [], []),	 
	    case httpc:request(get, {URL302, []}, [], []) of
		{ok, Reply7} ->
		    case Reply7 of
			{{_,200,_}, [_ | _], [_|_]} ->
			    tsp("http_redirect -> "
				"expected reply for request 7"), 
			    ok;
			{StatusLine, Headers, Body} ->
			    tsp("http_redirect -> "
				"unexpected reply for request 7: "
				"~n   StatusLine: ~p"
				"~n   Headers:    ~p"
				"~n   Body:       ~p", 
				[StatusLine, Headers, Body]),
			    tsf({unexpected_reply, Reply7})
		    end;
		Error7 ->
		    tsp("http_redirect -> "
			"unexpected result for request 7: "
			"~n   Error7:       ~p", 
			[Error7]),
		    tsf({unexpected_result, Error7})
	    end,
	    
	    tsp("http_redirect -> issue request 7: "
		"~n   ~p", [URL302]),
	    {ok, {{_,200,_}, [_ | _], []}} 
 		= httpc:request(head, {URL302, []}, [], []),	 
	    
	    tsp("http_redirect -> issue request 8: "
		"~n   ~p", [URL302]),
	    {ok, {{_,302,_}, [_ | _], [_|_]}} 
 		= httpc:request(post, {URL302, [],"text/plain", "foobar"},
			       [], []),
   
	    URL303 = ?URL_START ++ integer_to_list(Port) ++ "/303.html",

	    tsp("http_redirect -> issue request 9: "
		"~n   ~p", [URL303]),
	    {ok, {{_,200,_}, [_ | _], [_|_]}}
                = httpc:request(get, {URL303, []}, [], []),

	    tsp("http_redirect -> issue request 10: "
		"~n   ~p", [URL303]),
	    {ok, {{_,200,_}, [_ | _], []}}
                = httpc:request(head, {URL303, []}, [], []),

	    tsp("http_redirect -> issue request 11: "
		"~n   ~p", [URL303]),
	    {ok, {{_,200,_}, [_ | _], [_|_]}}
                = httpc:request(post, {URL303, [],"text/plain", "foobar"},
			       [], []),

	    URL307 = ?URL_START ++ integer_to_list(Port) ++ "/307.html",

	    tsp("http_redirect -> issue request 12: "
		"~n   ~p", [URL307]),
	    {ok, {{_,200,_}, [_ | _], [_|_]}} 
 		= httpc:request(get, {URL307, []}, [], []),
	
	    tsp("http_redirect -> issue request 13: "
		"~n   ~p", [URL307]),
	    {ok, {{_,200,_}, [_ | _], []}} 
 		= httpc:request(head, {URL307, []}, [], []),
	    
	    tsp("http_redirect -> issue request 14: "
		"~n   ~p", [URL307]),
	    {ok, {{_,307,_}, [_ | _], [_|_]}} 
 		= httpc:request(post, {URL307, [],"text/plain", "foobar"},
			       [], []),

	    tsp("http_redirect -> stop dummy server"),
	    DummyServerPid ! stop,
	    tsp("http_redirect -> reset ipfamily option (to inet6fb4)"),
	    ok = httpc:set_options([{ipfamily, inet6fb4}]), 
	    tsp("http_redirect -> done"),
	    ok;

	_ ->
	    skip("Failed to start local http-server")
    end.



%%-------------------------------------------------------------------------
http_redirect_loop(doc) ->
    ["Test redirect loop detection"];
http_redirect_loop(suite) ->
    [];
http_redirect_loop(Config) when is_list(Config) ->
    ok = httpc:set_options([{ipfamily, inet}]),
    {DummyServerPid, Port} = dummy_server(ipv4),
    
    URL = ?URL_START ++ integer_to_list(Port) ++ "/redirectloop.html",
    
    {ok, {{_,300,_}, [_ | _], _}} 
 	= httpc:request(get, {URL, []}, [], []),
    DummyServerPid ! stop,
    ok = httpc:set_options([{ipfamily, inet6fb4}]), 
    ok.

%%-------------------------------------------------------------------------
http_internal_server_error(doc) ->
    ["Test 50X codes"];
http_internal_server_error(suite) ->
    [];
http_internal_server_error(Config) when is_list(Config) ->
    ok = httpc:set_options([{ipfamily, inet}]),
    {DummyServerPid, Port} = dummy_server(ipv4),
    
    URL500 = ?URL_START ++ integer_to_list(Port) ++ "/500.html",
    
    {ok, {{_,500,_}, [_ | _], _}} 
 	= httpc:request(get, {URL500, []}, [], []),


    URL503 = ?URL_START ++ integer_to_list(Port) ++ "/503.html",

    %% Used to be able to make the service available after retry.
    ets:new(unavailable, [named_table, public, set]),
    ets:insert(unavailable, {503, unavailable}),
    
    {ok, {{_,200, _}, [_ | _], [_|_]}} =
	httpc:request(get, {URL503, []}, [], []),
    
    ets:insert(unavailable, {503, long_unavailable}),

    {ok, {{_,503, _}, [_ | _], [_|_]}} =
	httpc:request(get, {URL503, []}, [], []),

    ets:delete(unavailable),
    DummyServerPid ! stop,
    ok = httpc:set_options([{ipfamily, inet6fb4}]), 
    ok.


%%-------------------------------------------------------------------------
http_userinfo(doc) ->
    ["Test user info e.i. http://user:passwd@host:port/"];
http_userinfo(suite) ->
    [];
http_userinfo(Config) when is_list(Config) ->
    ok = httpc:set_options([{ipfamily, inet}]),

    {DummyServerPid, Port} = dummy_server(ipv4),
    
    URLAuth = "http://alladin:sesame@localhost:" 
	++ integer_to_list(Port) ++ "/userinfo.html",
    
    {ok, {{_,200,_}, [_ | _], _}} 
 	= httpc:request(get, {URLAuth, []}, [], []),

    URLUnAuth = "http://alladin:foobar@localhost:" 
	++ integer_to_list(Port) ++ "/userinfo.html",
    
    {ok, {{_,401, _}, [_ | _], _}} =
	httpc:request(get, {URLUnAuth, []}, [], []),
    
    DummyServerPid ! stop,
    ok = httpc:set_options([{ipfamily, inet6fb4}]), 
    ok.


%%-------------------------------------------------------------------------
http_cookie(doc) ->
    ["Test cookies."];
http_cookie(suite) ->
    [];
http_cookie(Config) when is_list(Config) ->
    ok = httpc:set_options([{cookies, enabled}, {ipfamily, inet}]),
    {DummyServerPid, Port} = dummy_server(ipv4),
    
    URLStart = ?URL_START  
	++ integer_to_list(Port),
    
    URLCookie = URLStart ++ "/cookie.html",
   
    {ok, {{_,200,_}, [_ | _], [_|_]}} 
 	= httpc:request(get, {URLCookie, []}, [], []),

    ets:new(cookie, [named_table, public, set]),
    ets:insert(cookie, {cookies, true}),

    {ok, {{_,200,_}, [_ | _], [_|_]}} 
 	= httpc:request(get, {URLStart ++ "/", []}, [], []),
    
    ets:delete(cookie),

    ok = httpc:set_options([{cookies, disabled}]), 
    DummyServerPid ! stop,
    ok = httpc:set_options([{ipfamily, inet6fb4}]), 
    ok.

%%-------------------------------------------------------------------------
http_server_does_not_exist(doc) ->
    ["Test that we get an error message back when the server "
     "does note exist."];
http_server_does_not_exist(suite) ->
    [];
http_server_does_not_exist(Config) when is_list(Config) ->
    {error, _} = 
	httpc:request(get, {"http://localhost:" ++ 
			   integer_to_list(?NOT_IN_USE_PORT) 
			   ++ "/", []},[], []),
    ok.


%%-------------------------------------------------------------------------
page_does_not_exist(doc) ->
    ["Test that we get a 404 when the page is not found."];
page_does_not_exist(suite) ->
    [];
page_does_not_exist(Config) when is_list(Config) ->
    Port = ?config(local_port, Config),
    URL = ?URL_START ++ integer_to_list(Port) ++ "/doesnotexist.html",
    {ok, {{_,404,_}, [_ | _], [_ | _]}} 
	= httpc:request(get, {URL, []}, [], []),
    ok.


%%-------------------------------------------------------------------------

http_stream(doc) ->
    ["Test the option stream for asynchrony requests"];
http_stream(suite) ->
    [];
http_stream(Config) when is_list(Config) ->
    Port = ?config(local_port, Config),
    URL = ?URL_START ++ integer_to_list(Port) ++ "/dummy.html",
    {ok, {{_,200,_}, [_ | _], Body}} = 
	httpc:request(get, {URL, []}, [], []),
    
    {ok, RequestId} =
	httpc:request(get, {URL, []}, [], [{sync, false}, 
					  {stream, self}]),
    
    receive 
	{http, {RequestId, stream_start, _Headers}} ->
	    ok;
	{http, Msg} ->
	    tsf(Msg)
    end,

    StreamedBody = receive_streamed_body(RequestId, <<>>),
    
    Body == binary_to_list(StreamedBody).


%%-------------------------------------------------------------------------

http_stream_once(doc) ->
    ["Test the option stream for asynchrony requests"];
http_stream_once(suite) ->
    [];
http_stream_once(Config) when is_list(Config) ->
    p("http_stream_once -> entry with"
      "~n   Config: ~p", [Config]),

    p("http_stream_once -> set ipfamily to inet", []),
    ok = httpc:set_options([{ipfamily, inet}]),
    p("http_stream_once -> start dummy server", []),
    {DummyServerPid, Port} = dummy_server(ipv4),    

    PortStr =  integer_to_list(Port),
    p("http_stream_once -> once", []),
    once(?URL_START ++ PortStr ++ "/once.html"),
    p("http_stream_once -> once_chunked", []),
    once(?URL_START ++ PortStr ++ "/once_chunked.html"),
    p("http_stream_once -> dummy", []),
    once(?URL_START ++ PortStr ++ "/dummy.html"),

    p("http_stream_once -> stop dummy server", []),
    DummyServerPid ! stop,
    p("http_stream_once -> set ipfamily to inet6fb4", []),
    ok = httpc:set_options([{ipfamily, inet6fb4}]), 
    p("http_stream_once -> done", []),
    ok.

once(URL) ->
    p("once -> issue sync request for ~p", [URL]),
    {ok, {{_,200,_}, [_ | _], Body}} = 
	httpc:request(get, {URL, []}, [], []),
    
    p("once -> issue async (self stream) request for ~p", [URL]),
    {ok, RequestId} =
	httpc:request(get, {URL, []}, [], [{sync, false}, 
					  {stream, {self, once}}]),
    
    p("once -> await stream_start reply for (async) request ~p", [RequestId]),
    NewPid = 
	receive 
	    {http, {RequestId, stream_start, _Headers, Pid}} ->
		p("once -> received stream_start reply for (async) request ~p: ~p", 
		  [RequestId, Pid]),
		Pid;
	    {http, Msg} ->
		tsf(Msg)
	end,

    tsp("once -> request handler: ~p", [NewPid]),

    p("once -> await stream reply for (async) request ~p", [RequestId]),
    BodyPart = 
	receive 
	    {http, {RequestId, stream, BinBodyPart}} ->
		p("once -> received stream reply for (async) request ~p: "
		  "~n~p", [RequestId, binary_to_list(BinBodyPart)]),
		BinBodyPart
	end,

    tsp("once -> first body part '~p' received", [binary_to_list(BodyPart)]),

    StreamedBody = receive_streamed_body(RequestId, BinBodyPart, NewPid),
    
    Body = binary_to_list(StreamedBody),

    p("once -> done when Bode: ~p", [Body]),
    ok.


%%-------------------------------------------------------------------------
parse_url(doc) ->
    ["Test that an url is parsed correctly"];
parse_url(suite) ->
    [];
parse_url(Config) when is_list(Config) ->
    %% ipv6
    {ok, {http,[],"2010:836B:4179::836B:4179",80,"/foobar.html",[]}} = 
	http_uri:parse("http://[2010:836B:4179::836B:4179]/foobar.html"),
    {ok, {http,[],"[2010:836B:4179::836B:4179]",80,"/foobar.html",[]}} = 
	http_uri:parse("http://[2010:836B:4179::836B:4179]/foobar.html", 
		       [{ipv6_host_with_brackets, true}]),
    {ok, {http,[],"2010:836B:4179::836B:4179",80,"/foobar.html",[]}} = 
	http_uri:parse("http://[2010:836B:4179::836B:4179]/foobar.html", 
		       [{ipv6_host_with_brackets, false}]),
    {ok, {http,[],"2010:836B:4179::836B:4179",80,"/foobar.html",[]}} = 
	http_uri:parse("http://[2010:836B:4179::836B:4179]/foobar.html", 
		       [{foo, false}]),
    {error,
     {malformed_url, _, "http://2010:836B:4179::836B:4179/foobar.html"}} =
	http_uri:parse("http://2010:836B:4179::836B:4179/foobar.html"), 

    %% ipv4
    {ok, {http,[],"127.0.0.1",80,"/foobar.html",[]}} =
	http_uri:parse("http://127.0.0.1/foobar.html"),
    
    %% host
    {ok, {http,[],"localhost",8888,"/foobar.html",[]}} = 
	http_uri:parse("http://localhost:8888/foobar.html"),
    
    %% Userinfo
    {ok, {http,"nisse:foobar","localhost",8888,"/foobar.html",[]}} =
	http_uri:parse("http://nisse:foobar@localhost:8888/foobar.html"),
    
    %% Scheme error
    {error, no_scheme} = http_uri:parse("localhost/foobar.html"),
    {error, {malformed_url, _, _}} =
	http_uri:parse("localhost:8888/foobar.html"),
    
    %% Query
    {ok, {http,[],"localhost",8888,"/foobar.html","?foo=bar&foobar=42"}} =
	http_uri:parse("http://localhost:8888/foobar.html?foo=bar&foobar=42"),
    
    %%  Esc chars
    {ok, {http,[],"www.somedomain.com",80,"/%2Eabc",[]}} =
	http_uri:parse("http://www.somedomain.com/%2Eabc"),
    {ok, {http,[],"www.somedomain.com",80,"/%252Eabc",[]}} = 
	http_uri:parse("http://www.somedomain.com/%252Eabc"),
    {ok, {http,[],"www.somedomain.com",80,"/%25abc",[]}} =
	http_uri:parse("http://www.somedomain.com/%25abc"),
    {ok, {http,[],"www.somedomain.com",80,"/%25abc", "?foo=bar"}} =
	http_uri:parse("http://www.somedomain.com/%25abc?foo=bar"),

    
    ok.    


%%-------------------------------------------------------------------------

ipv6_ipcomm() ->
    [{require, ipv6_hosts}].
ipv6_ipcomm(doc) ->
    ["Test ip_comm ipv6."];
ipv6_ipcomm(suite) ->
    [];
ipv6_ipcomm(Config) when is_list(Config) ->
    HTTPOptions = [],
    SocketType  = ip_comm, 
    Scheme      = "http", 
    Extra       = [], 
    ipv6(SocketType, Scheme, HTTPOptions, Extra, Config).


%%-------------------------------------------------------------------------

ipv6_essl() ->
    [{require, ipv6_hosts}].
ipv6_essl(doc) ->
    ["Test essl ipv6."];
ipv6_essl(suite) ->
    [];
ipv6_essl(Config) when is_list(Config) ->
    DataDir    = ?config(data_dir, Config),
    CertFile   = filename:join(DataDir, "ssl_client_cert.pem"),
    SSLOptions = [{certfile, CertFile}, {keyfile, CertFile}],
    SSLConfig  = {essl, SSLOptions},
    tsp("ssl_ipv6 -> make request using: "
	"~n   SSLOptions: ~p", [SSLOptions]),
    HTTPOptions = [{ssl, SSLConfig}], 
    SocketType  = essl, 
    Scheme      = "https", 
    Extra       = SSLOptions, 
    ipv6(SocketType, Scheme, HTTPOptions, Extra, Config).


%%-------------------------------------------------------------------------

ipv6(SocketType, Scheme, HTTPOptions, Extra, Config) ->
    %% Check if we are a IPv6 host
    tsp("ipv6 -> verify ipv6 support"),
    case inets_test_lib:has_ipv6_support(Config) of
	{ok, Addr} ->
	    tsp("ipv6 -> ipv6 supported: ~p", [Addr]),
	    {DummyServerPid, Port} = dummy_server(SocketType, ipv6, Extra),
	    Profile = ?config(profile, Config),
	    URL = 
		Scheme ++ 
		"://[" ++ http_transport:ipv6_name(Addr) ++ "]:" ++ 
		integer_to_list(Port) ++ "/foobar.html",
	    tsp("ipv6 -> issue request with: "
		"~n   URL:         ~p"
		"~n   HTTPOptions: ~p", [URL, HTTPOptions]),
	    case httpc:request(get, {URL, []}, HTTPOptions, [], Profile) of
		{ok, {{_,200,_}, [_ | _], [_|_]}} ->
		    tsp("ipv6 -> expected result"),
		    DummyServerPid ! stop,
		    ok;
		{ok, Unexpected} ->
		    tsp("ipv6 -> unexpected result: "
			"~n   ~p", [Unexpected]),
		    DummyServerPid ! stop,
		    tsf({unexpected_result, Unexpected});
		{error, Reason} ->
		    tsp("ipv6 -> error: "
			"~n   Reason: ~p", [Reason]),
		    DummyServerPid ! stop,
		    tsf(Reason)
	    end,
	    ok;
	_ ->
	    tsp("ipv6 -> ipv6 not supported"),
	    skip("Host does not support IPv6")
    end.


%%-------------------------------------------------------------------------

headers_as_is(doc) ->
    ["Test the option headers_as_is"];
headers_as_is(suite) ->
    [];
headers_as_is(Config) when is_list(Config) ->
    Port = ?config(local_port, Config),
    URL = ?URL_START ++ integer_to_list(Port) ++ "/dummy.html",
    {ok, {{_,200,_}, [_|_], [_|_]}} =
	httpc:request(get, {URL, [{"Host", "localhost"},{"Te", ""}]}, 
		     [], [{headers_as_is, true}]),
     
    {ok, {{_,400,_}, [_|_], [_|_]}} = 
	httpc:request(get, {URL, [{"Te", ""}]},[], [{headers_as_is, true}]),
    ok.


%%-------------------------------------------------------------------------

selecting_session(doc) ->
    ["Test selection of sessions - OTP-9847"];
selecting_session(suite) ->
    [];
selecting_session(Config) when is_list(Config) ->
    tsp("selecting_session -> entry with"
	"~n   Config: ~p", [Config]),

    tsp("selecting_session -> set ipfamily to inet"),
    ok = httpc:set_options([{ipfamily, inet}]),

    tsp("selecting_session -> start server"),
    {ServerPid, Port} = otp_9847_server(),    

    PortStr = integer_to_list(Port),
    URL     = ?URL_START ++ PortStr ++ "/index.html",
     
    tsp("selecting_session -> issue the first batch (three) requests"),
    lists:foreach(fun(P) -> 
			  tsp("selecting_session:fun1 -> "
			      "send stop request to ~p", [P]),
			  P ! stop 
		  end, 
		  reqs(URL, ServerPid, 3, 3, false)),
    tsp("selecting_session -> sleep some (1) to make sure nothing lingers"),
    ?SLEEP(5000),
    tsp("selecting_session -> "
	"instruct the server to reply to the first request"),
    ServerPid ! {answer, true}, 
    receive
	{answer, true} ->
	    tsp("selecting_session -> "
		"received ack from server to reply to the first request"),
	    ok
    end,
    tsp("selecting_session -> issue the second batch (four) requests"),
    lists:foreach(fun(P) -> 
			  tsp("selecting_session:fun2 -> "
			      "send stop request to ~p", [P]),
			  P ! stop 
		  end, 
		  reqs(URL, ServerPid, 4, 1, true)),
    tsp("selecting_session -> sleep some (2) to make sure nothing lingers"),
    ?SLEEP(5000),

    tsp("selecting_session -> stop server"),
    ServerPid ! stop,
    tsp("selecting_session -> set ipfamily (back) to inet6fb4"),
    ok = httpc:set_options([{ipfamily, inet6fb4}]), 
    tsp("selecting_session -> done"),
    ok.

reqs(URL, ServerPid, NumReqs, NumHandlers, InitialSync) ->
    tsp("reqs -> entry with"
	"~n   URL:         ~p"
	"~n   ServerPid:   ~w"
	"~n   NumReqs:     ~w"
	"~n   NumHandlers: ~w"
	"~n   InitialSync: ~w", 
	[URL, ServerPid, NumReqs, NumHandlers, InitialSync]),
    Handlers = reqs2(URL, NumReqs, [], InitialSync),
    tsp("reqs -> "
	"~n   Handlers: ~w", [Handlers]),
    case length(Handlers) of
	NumHandlers ->
	    tsp("reqs -> "
		"~n   NumHandlers: ~w", [NumHandlers]),
	    ServerPid ! num_handlers, 
	    receive
		{num_handlers, NumHandlers} ->
		    tsp("reqs -> received num_handlers with"
			"~n   NumHandlers: ~w", [NumHandlers]),
		    Handlers;
		{num_handlers, WrongNumHandlers} ->
		    tsp("reqs -> received num_handlers with"
			"~n   WrongNumHandlers: ~w", [WrongNumHandlers]),
		    exit({wrong_num_handlers1, WrongNumHandlers, NumHandlers})
	    end;
	WrongNumHandlers ->
	    tsp("reqs -> "
		"~n   WrongNumHandlers: ~w", [WrongNumHandlers]),
	    exit({wrong_num_handlers2, WrongNumHandlers, NumHandlers})
    end.
	    

reqs2(_URL, 0, Acc, _Sync) ->
    lists:reverse(Acc);
reqs2(URL, Num, Acc, Sync) ->
    tsp("reqs2 -> entry with"
	"~n   Num:  ~w"
	"~n   Sync: ~w", [Num, Sync]),
    case httpc:request(get, {URL, []}, [], [{sync, Sync}]) of
	{ok, _Reply} ->
	    tsp("reqs2 -> successful request: ~p", [_Reply]),
	    receive
		{handler, Handler, _Manager} ->
		    %% This is when a new handler is created
		    tsp("reqs2 -> received handler: ~p", [Handler]),
		    case lists:member(Handler, Acc) of
			true ->
			    tsp("reqs2 -> duplicate handler"),
			    exit({duplicate_handler, Handler, Num, Acc});
			false ->
			    tsp("reqs2 -> wait for data ack"),
			    receive
				{data_received, Handler} ->
				    tsp("reqs2 -> "
					"received data ack from ~p", [Handler]),
				    case Sync of
					true ->
					    reqs2(URL, Num-1, [Handler|Acc], 
						  false);
					false ->
					    reqs2(URL, Num-1, [Handler|Acc], 
						  Sync)
				    end
			    end
		    end;

		{data_received, Handler} ->
		    tsp("reqs2 -> "
			"received data ack from ~p", [Handler]),
		    reqs2(URL, Num-1, Acc, false)

	    end;

	{error, Reason} ->
	    tsp("reqs2 -> request ~w failed: ~p", [Num, Reason]),
	    exit({request_failed, Reason, Num, Acc})
    end.

otp_9847_server() ->
    TC  = self(), 
    Pid = spawn_link(fun() -> otp_9847_server_init(TC) end),
    receive
	{port, Port} ->
	    {Pid, Port}
    end.

otp_9847_server_init(TC) ->
    tsp("otp_9847_server_init -> entry with"
	"~n   TC: ~p", [TC]),
    {ok, ListenSocket} = 
	gen_tcp:listen(0, [binary, inet, {packet, 0},
			   {reuseaddr,true},
			   {active, false}]),
    tsp("otp_9847_server_init -> listen socket created: "
	"~n   ListenSocket: ~p", [ListenSocket]),
    {ok, Port} = inet:port(ListenSocket),
    tsp("otp_9847_server_init -> Port: ~p", [Port]),
    TC ! {port, Port},
    otp_9847_server_main(TC, ListenSocket, false, []).

otp_9847_server_main(TC, ListenSocket, Answer, Handlers) ->
    tsp("otp_9847_server_main -> entry with"
	"~n   TC:           ~p"
	"~n   ListenSocket: ~p"
	"~n   Answer:       ~p"
	"~n   Handlers:     ~p", [TC, ListenSocket, Answer, Handlers]),
    case gen_tcp:accept(ListenSocket, 1000) of
	{ok, Sock} ->
	    tsp("otp_9847_server_main -> accepted"
		"~n   Sock: ~p", [Sock]),
	    {Handler, Mon, Port} = otp_9847_handler(TC, Sock, Answer), 
	    tsp("otp_9847_server_main -> handler ~p created for ~w", 
		[Handler, Port]),
	    gen_tcp:controlling_process(Sock, Handler),
	    tsp("otp_9847_server_main -> control transfer"),
	    Handler ! owner, 
	    tsp("otp_9847_server_main -> "
		"handler ~p informed of owner transfer", [Handler]),
	    TC ! {handler, Handler, self()}, 
	    tsp("otp_9847_server_main -> "
		"TC ~p informed of handler ~p", [TC, Handler]),
	    otp_9847_server_main(TC, ListenSocket, Answer, 
				 [{Handler, Mon, Sock, Port}|Handlers]);

	{error, timeout} ->
	    tsp("otp_9847_server_main -> timeout"),
	    receive 
		{answer, true} ->
		    tsp("otp_9847_server_main -> received answer request"),
		    TC ! {answer, true}, 
		    otp_9847_server_main(TC, ListenSocket, true, Handlers);

		{'DOWN', _Mon, process, Pid, _Reason} ->
		    %% Could be one of the handlers
		    tsp("otp_9847_server_main -> received DOWN for ~p", [Pid]),
		    otp_9847_server_main(TC, ListenSocket, Answer, 
					 lists:keydelete(Pid, 1, Handlers));

		num_handlers ->
		    tsp("otp_9847_server_main -> "
			"received request for number of handlers (~w)", 
			[length(Handlers)]),
		    TC ! {num_handlers, length(Handlers)}, 
		    otp_9847_server_main(TC, ListenSocket, Answer, Handlers);

		stop ->
		    tsp("otp_9847_server_main -> received stop request"),
		    %% Stop all handlers (just in case)
		    Pids = [Handler || {Handler, _, _} <- Handlers], 
		    lists:foreach(fun(Pid) -> Pid ! stop end, Pids),
		    exit(normal);

		Any ->
		    tsp("otp_9847_server_main -> received"
			"~n   Any: ~p", [Any]),
		    exit({crap, Any})

	    after 0 ->
		    tsp("otp_9847_server_main -> nothing in queue"),
		    otp_9847_server_main(TC, ListenSocket, Answer, Handlers)
	    end;

	Error ->
	    exit(Error)
    end.


otp_9847_handler(TC, Sock, Answer) ->
    tsp("otp_9847_handler -> entry with"
	"~n   TC:     ~p" 
	"~n   Sock:   ~p" 
	"~n   Answer: ~p", [TC, Sock, Answer]),
    Self = self(), 
    {Pid, Mon} = 
	spawn_opt(fun() -> 
			  otp_9847_handler_init(TC, Self, Sock, Answer) 
		  end, 
		  [monitor]),
    receive
	{port, Port} ->
	    tsp("otp_9847_handler -> received port message (from ~p)"
		"~n   Port: ~p", [Pid, Port]),
	    {Pid, Mon, Port}
    end.
    

otp_9847_handler_init(TC, Server, Sock, Answer) ->
    tsp("otp_9847_handler_init -> entry with"
	"~n   TC:     ~p" 
	"~n   Server: ~p" 
	"~n   Sock:   ~p" 
	"~n   Answer: ~p", [TC, Server, Sock, Answer]),
    {ok, Port} = inet:port(Sock),
    Server ! {port, Port},
    receive 
	owner ->
	    tsp("otp_9847_handler_init -> "
		"received owner message - activate socket"),
	    inet:setopts(Sock, [{active, true}]),
	    otp_9847_handler_main(TC, Server, Sock, Answer, [?HTTP_MAX_HEADER_SIZE])
    end.
    
otp_9847_handler_main(TC, Server, Sock, Answer, ParseArgs) ->
    tsp("otp_9847_handler_main -> entry with"
	"~n   TC:        ~p" 
	"~n   Server:    ~p" 
	"~n   Sock:      ~p" 
	"~n   Answer:    ~p"
	"~n   ParseArgs: ~p", [TC, Server, Sock, Answer, ParseArgs]),
    receive
	stop ->
	    tsp("otp_9847_handler_main -> received stop request"),
	    exit(normal);

	{tcp, Sock, _Data} when Answer =:= false ->
	    tsp("otp_9847_handler_main -> received tcp data - no answer"),
	    TC ! {data_received, self()}, 
	    inet:setopts(Sock, [{active, true}]),
	    %% Ignore all data
	    otp_9847_handler_main(TC, Server, Sock, Answer, ParseArgs);

	{tcp, Sock, Data} when Answer =:= true ->
	    tsp("otp_9847_handler_main -> received tcp data - answer"),
	    TC ! {data_received, self()}, 
	    inet:setopts(Sock, [{active, true}]),
	    NewParseArgs = otp_9847_handler_request(Sock, [Data|ParseArgs]), 
	    otp_9847_handler_main(TC, Server, Sock, Answer, NewParseArgs);

	{tcp_closed, Sock} ->
	    tsp("otp_9847_handler_main -> received tcp socket closed"),
	    exit(normal);

	{tcp_error, Sock, Reason} ->
	    tsp("otp_9847_handler_main -> socket error: ~p", [Reason]),
	    (catch gen_tcp:close(Sock)),
	    exit(normal)

    %% after 30000 ->
    %% 	    gen_tcp:close(Sock),
    %% 	    exit(normal)
    end.
    
otp_9847_handler_request(Sock, Args) ->
    Msg = 
	case httpd_request:parse(Args) of
	    {ok, {_, "/index.html" = _RelUrl, _, _, _}} ->
		B = 
		    "<HTML><BODY>" ++ 
		    "...some body part..." ++ 
		    "</BODY></HTML>", 
		Len = integer_to_list(length(B)), 
		"HTTP/1.1 200 ok\r\n" ++
		    "Content-Length:" ++ Len ++ "\r\n\r\n" ++ B
	end,
    gen_tcp:send(Sock, Msg),
    [?HTTP_MAX_HEADER_SIZE].
	    


%%-------------------------------------------------------------------------

options(doc) ->
    ["Test the option parameters."];
options(suite) ->
    [];
options(Config) when is_list(Config) ->
    case ?config(local_server, Config) of 
	ok ->
	    Port = ?config(local_port, Config),
	    URL = ?URL_START ++ integer_to_list(Port) ++ "/dummy.html",
	    {ok, {{_,200,_}, [_ | _], Bin}} 
		= httpc:request(get, {URL, []}, [{foo, bar}], 
			       %% Ignore unknown options
			       [{body_format, binary}, {foo, bar}]),

	    true = is_binary(Bin),
	    {ok, {200, [_|_]}} 
		= httpc:request(get, {URL, []}, [{timeout, infinity}],
			       [{full_result, false}]);
	_ ->
	    skip("Failed to start local http-server")
    end.  


%%-------------------------------------------------------------------------

http_invalid_http(doc) ->
    ["Test parse error"];
http_invalid_http(suite) ->
    [];
http_invalid_http(Config) when is_list(Config) ->
    ok = httpc:set_options([{ipfamily, inet}]),
    {DummyServerPid, Port} = dummy_server(ipv4),
    
    URL = ?URL_START ++ integer_to_list(Port) ++ "/invalid_http.html",
    
    {error, {could_not_parse_as_http, _} = Reason} =
	httpc:request(get, {URL, []}, [], []),
    
    test_server:format("Parse error: ~p ~n", [Reason]),
    DummyServerPid ! stop,
    ok = httpc:set_options([{ipfamily, inet6fb4}]), 
    ok.


%%-------------------------------------------------------------------------

-define(GOOGLE, "www.google.com").

hexed_query_otp_6191(doc) ->
    [];
hexed_query_otp_6191(suite) ->
    [];
hexed_query_otp_6191(Config) when is_list(Config) ->
    Google = ?GOOGLE, 
    GoogleSearch = "http://" ++ Google ++ "/search",
    Search1 = "?hl=en&q=a%D1%85%D1%83%D0%B9&btnG=Google+Search", 
    URI1    = GoogleSearch ++ Search1,
    Search2 = "?hl=en&q=%25%25", 
    URI2    = GoogleSearch ++ Search2,
    Search3 = "?hl=en&q=%foo",
    URI3    = GoogleSearch ++ Search3, 

    Verify1 = 
	fun({http, [], ?GOOGLE, 80, "/search", _}) -> ok;
	   (_) -> error
	end,
    Verify2 = Verify1, 
    Verify3 = Verify1, 
    verify_uri(URI1, Verify1), 
    verify_uri(URI2, Verify2), 
    verify_uri(URI3, Verify3), 
    ok.

verify_uri(URI, Verify) ->
    case http_uri:parse(URI) of
	{ok, ParsedURI} ->
	    case Verify(ParsedURI) of
		ok ->
		    ok;
		error ->
		    Reason = {unexpected_parse_result, URI, ParsedURI}, 
		    ERROR  = {error, Reason}, 
		    throw(ERROR)
	    end;
	{error, _} = ERROR ->
	    throw(ERROR)
    end.


%%-------------------------------------------------------------------------

empty_body_otp_6243(doc) ->
    ["An empty body was not returned directly. There was a delay for several"
     "seconds."];
empty_body_otp_6243(suite) ->
    [];
empty_body_otp_6243(Config) when is_list(Config) ->
    Port = ?config(local_port, Config),
    URL = ?URL_START ++ integer_to_list(Port) ++ "/empty.html",
    {ok, {{_,200,_}, [_ | _], []}} =
	httpc:request(get, {URL, []}, [{timeout, 500}], []).


%%-------------------------------------------------------------------------

transfer_encoding_otp_6807(doc) ->
    ["Transfer encoding is case insensitive"];
transfer_encoding_otp_6807(suite) ->
    [];
transfer_encoding_otp_6807(Config) when is_list(Config) ->
    ok = httpc:set_options([{ipfamily, inet}]),
    {DummyServerPid, Port} = dummy_server(ipv4),
    
    URL = ?URL_START ++ integer_to_list(Port) ++ 
	"/capital_transfer_encoding.html",
    {ok, {{_,200,_}, [_|_], [_ | _]}} = httpc:request(URL),
    DummyServerPid ! stop,
    ok = httpc:set_options([{ipfamily, inet6fb4}]), 
    ok.


%%-------------------------------------------------------------------------

empty_response_header_otp_6830(doc) ->
    ["Test the case that the HTTP server does not send any headers"];
empty_response_header_otp_6830(suite) ->
    [];
empty_response_header_otp_6830(Config) when is_list(Config) ->
    ok = httpc:set_options([{ipfamily, inet}]),
    {DummyServerPid, Port} = dummy_server(ipv4),
    
    URL = ?URL_START ++ integer_to_list(Port) ++ "/no_headers.html",
    {ok, {{_,200,_}, [], [_ | _]}} = httpc:request(URL),
    DummyServerPid ! stop,
    ok = httpc:set_options([{ipfamily, inet6fb4}]), 
    ok.


%%-------------------------------------------------------------------------

no_content_204_otp_6982(doc) ->
    ["Test the case that the HTTP 204 no content header"];
no_content_204_otp_6982(suite) ->
    [];
no_content_204_otp_6982(Config) when is_list(Config) ->
    ok = httpc:set_options([{ipfamily, inet}]),
    {DummyServerPid, Port} = dummy_server(ipv4),
    
    URL = ?URL_START ++ integer_to_list(Port) ++ "/no_content.html",
    {ok, {{_,204,_}, [], []}} = httpc:request(URL),
    DummyServerPid ! stop,
    ok = httpc:set_options([{ipfamily, inet6fb4}]), 
    ok.


%%-------------------------------------------------------------------------

missing_CR_otp_7304(doc) ->
    ["Test the case that the HTTP server uses only LF instead of CRLF" 
     "as delimitor"];
missing_CR_otp_7304(suite) ->
    [];
missing_CR_otp_7304(Config) when is_list(Config) ->
    ok = httpc:set_options([{ipfamily, inet}]),
    {DummyServerPid, Port} = dummy_server(ipv4),
    
    URL = ?URL_START ++ integer_to_list(Port) ++ "/missing_CR.html",
    {ok, {{_,200,_}, _, [_ | _]}} = httpc:request(URL),
    DummyServerPid ! stop,
    ok = httpc:set_options([{ipfamily, inet6fb4}]), 
    ok.


%%-------------------------------------------------------------------------


otp_7883_1(doc) ->
    ["OTP-7883-sync"];
otp_7883_1(suite) ->
    [];
otp_7883_1(Config) when is_list(Config) ->
    ok = httpc:set_options([{ipfamily, inet}]),

    {DummyServerPid, Port} = dummy_server(ipv4),
    
    URL = ?URL_START ++ integer_to_list(Port) ++ "/just_close.html",
    {error, socket_closed_remotely} = httpc:request(URL),
    DummyServerPid ! stop,

    ok = httpc:set_options([{ipfamily, inet6fb4}]), 
    ok.

otp_7883_2(doc) ->
    ["OTP-7883-async"];
otp_7883_2(suite) ->
    [];
otp_7883_2(Config) when is_list(Config) ->
    ok = httpc:set_options([{ipfamily, inet}]),

    {DummyServerPid, Port} = dummy_server(ipv4),
    
    URL = ?URL_START ++ integer_to_list(Port) ++ "/just_close.html",
    Method      = get,
    Request     = {URL, []}, 
    HttpOptions = [], 
    Options     = [{sync, false}], 
    Profile     = httpc:default_profile(), 
    {ok, RequestId} = 
	httpc:request(Method, Request, HttpOptions, Options, Profile),
    ok = 
	receive
	    {http, {RequestId, {error, socket_closed_remotely}}} ->
		ok
    end,
    DummyServerPid ! stop,

    ok = httpc:set_options([{ipfamily, inet6fb4}]), 
    ok.


%%-------------------------------------------------------------------------


otp_8154_1(doc) ->
    ["OTP-8154"];
otp_8154_1(suite) ->
    [];
otp_8154_1(Config) when is_list(Config) ->
    start_inets(),
    ReqSeqNumServer = start_sequence_number_server(),
    RespSeqNumServer = start_sequence_number_server(),
    {ok, Server, Port} = start_slow_server(RespSeqNumServer),
    Clients = run_clients(105, Port, ReqSeqNumServer),
    %% ok = wait_for_clients(Clients),
    ok = wait4clients(Clients, timer:minutes(3)),
    Server ! shutdown,
    RespSeqNumServer ! shutdown,
    ReqSeqNumServer ! shutdown,
    ok.

start_inets() ->
    inets:start(),
    ok.


%% -----------------------------------------------------
%% A sequence number handler
%% The purpose is to be able to pair requests with responses.

start_sequence_number_server() ->
    proc_lib:spawn(fun() -> loop_sequence_number(1) end).

loop_sequence_number(N) ->
    receive
	shutdown ->
	    ok;
	{From, get_next} ->
	    From ! {next_is, N},
	    loop_sequence_number(N + 1)
    end.

get_next_sequence_number(SeqNumServer) ->
    SeqNumServer ! {self(), get_next},
    receive {next_is, N} -> N end.

%% -----------------------------------------------------
%% Client part
%% Sends requests randomly parallel

run_clients(NumClients, ServerPort, SeqNumServer) ->
    io:format("start clients when"
	      "~n   NumClients:   ~w"
	      "~n   ServerPort:   ~w"
	      "~n   SeqNumServer: ~w"
	      "~n", [NumClients, ServerPort, SeqNumServer]),
    set_random_seed(),
    lists:map(
      fun(Id) ->
	      io:format("starting client ~w~n", [Id]),
	      Req = f("req~3..0w", [get_next_sequence_number(SeqNumServer)]),
	      Url = f(?URL_START ++ "~w/~s", [ServerPort, Req]),
	      Pid = proc_lib:spawn(
		      fun() ->
			      io:format("[~w] client started - "
					"issue request~n", [Id]),
			      case httpc:request(Url) of
				  {ok, {{_,200,_}, _, Resp}} ->
				      io:format("[~w] 200 response: "
						"~p~n", [Id, Resp]),
				      case lists:prefix(Req++"->", Resp) of
					  true -> exit(normal);
					  false -> exit({bad_resp,Req,Resp})
				      end;
				  {ok, {{_,EC,Reason},_,Resp}}  ->
				      io:format("[~w] ~w response: "
						"~s~n~s~n", 
						[Id, EC, Reason, Resp]),
				      exit({bad_resp,Req,Resp});
				  Crap ->
				      io:format("[~w] bad response: ~p", 
						[Id, Crap]),
				      exit({bad_resp, Req, Crap})
			      end
		      end),
	      MRef = erlang:monitor(process, Pid),
	      timer:sleep(10 + random:uniform(1334)),
	      {Id, Pid, MRef}

      end,
      lists:seq(1, NumClients)).

%% wait_for_clients(Clients) ->
%%     lists:foreach(
%%       fun({Id, Pid, MRef}) ->
%% 	      io:format("waiting for client ~w termination~n", [Id]),
%% 	      receive
%% 		  {'DOWN', MRef, process, Pid, normal} ->
%% 		      io:format("waiting for clients: "
%% 				"normal exit from ~w (~p)~n", 
%% 				[Id, Pid]),
%% 		      ok;
%% 		  {'DOWN', MRef, process, Pid, Reason} ->
%% 		      io:format("waiting for clients: "
%% 				"unexpected exit from ~w (~p):"
%% 				"~n   Reason: ~p"
%% 				"~n", [Id, Pid, Reason]),
%% 		      erlang:error(Reason)
%% 	      end
%%       end,
%%       Clients).


wait4clients([], _Timeout) ->
    ok;
wait4clients(Clients, Timeout) when Timeout > 0 ->
    io:format("wait4clients -> entry with"
	      "~n   length(Clients): ~w"
	      "~n   Timeout:         ~w"
	      "~n", [length(Clients), Timeout]),
    T = t(),
    receive
	{'DOWN', _MRef, process, Pid, normal} ->
	    case lists:keysearch(Pid, 2, Clients) of
		{value, {Id, _, _}} ->
		    io:format("receive normal exit message "
			      "from client ~p (~p)", [Id, Pid]),
		    NewClients = 
			lists:keydelete(Id, 1, Clients),
		    wait4clients(NewClients, 
				 Timeout - (t() - T));
		false ->
		    io:format("receive normal exit message "
			      "from unknown process: ~p", [Pid]),
		    wait4clients(Clients, Timeout - (t() - T))
	    end;

	{'DOWN', _MRef, process, Pid, Reason} ->
	    case lists:keysearch(Pid, 2, Clients) of
		{value, {Id, _, _}} ->
		    io:format("receive bad exit message "
			      "from client ~p (~p):"
			      "~n   ~p", [Id, Pid, Reason]),
		    erlang:error({bad_client_termination, Id, Reason});
		false ->
		    io:format("receive normal exit message "
			      "from unknown process: ~p", [Pid]),
		    wait4clients(Clients, Timeout - (t() - T))
	    end

    after Timeout ->
	    erlang:error({client_timeout, Clients})  
    end;
wait4clients(Clients, _) ->
    erlang:error({client_timeout, Clients}).
		    
		    
%% Time in milli seconds
t() ->
    {A,B,C} = erlang:now(),
    A*1000000000+B*1000+(C div 1000).


%% -----------------------------------------------------
%% Webserver part:
%% Implements a web server that sends responses one character
%% at a time, with random delays between the characters.

start_slow_server(SeqNumServer) ->
    io:format("start slow server when"
	      "~n   SeqNumServer: ~w"
	      "~n", [SeqNumServer]),
    proc_lib:start(
      erlang, apply, [fun() -> init_slow_server(SeqNumServer) end, []]).

init_slow_server(SeqNumServer) ->
    io:format("[webserver ~w] init slow server"
	      "~n", [SeqNumServer]),
    {ok, LSock} = gen_tcp:listen(0, [binary, {packet,0}, {active,true},
				     {backlog, 100}]),
    io:format("[webserver ~w] LSock: ~p"
	      "~n", [SeqNumServer, LSock]),
    {ok, {_IP, Port}} = inet:sockname(LSock),
    io:format("[webserver ~w] Port: ~w"
	      "~n", [SeqNumServer, Port]),
    proc_lib:init_ack({ok, self(), Port}),
    loop_slow_server(LSock, SeqNumServer).

loop_slow_server(LSock, SeqNumServer) ->
    io:format("[webserver ~w] entry with"
	      "~n   LSock: ~p"
	      "~n", [SeqNumServer, LSock]),
    Master = self(),
    Acceptor = proc_lib:spawn(
		 fun() -> client_handler(Master, LSock, SeqNumServer) end),
    io:format("[webserver ~w] acceptor started"
	      "~n   Acceptor: ~p"
	      "~n", [SeqNumServer, Acceptor]),
    receive
	{accepted, Acceptor} ->
	    io:format("[webserver ~w] accepted"
		      "~n", [SeqNumServer]),
	    loop_slow_server(LSock, SeqNumServer);
	shutdown ->
	    gen_tcp:close(LSock),
	    exit(Acceptor, kill)
    end.


%% Handle one client connection
client_handler(Master, LSock, SeqNumServer) ->
    io:format("[acceptor ~w] await accept"
	      "~n", [SeqNumServer]),
    {ok, CSock} = gen_tcp:accept(LSock),
    io:format("[acceptor ~w] accepted"
	      "~n   CSock: ~p"
	      "~n", [SeqNumServer, CSock]),
    Master ! {accepted, self()},
    set_random_seed(),
    loop_client(1, CSock, SeqNumServer).

loop_client(N, CSock, SeqNumServer) ->
    %% Await request, don't bother parsing it too much,
    %% assuming the entire request arrives in one packet.
    io:format("[acceptor ~w] await request"
	      "~n   N: ~p"
	      "~n", [SeqNumServer, N]),
    receive
	{tcp, CSock, Req} ->
	    ReqNum = parse_req_num(Req),
	    RespSeqNum = get_next_sequence_number(SeqNumServer),
	    Response = f("~s->resp~3..0w/~2..0w", [ReqNum, RespSeqNum, N]),
	    Txt = f("Slow server (~p) got ~p, answering with ~p",
		    [self(), Req, Response]),
	    io:format("~s...~n", [Txt]),
	    slowly_send_response(CSock, Response),
	    case parse_connection_type(Req) of
		keep_alive ->
		    io:format("~s...done~n", [Txt]),
		    loop_client(N+1, CSock, SeqNumServer);
		close ->
		    io:format("~s...done (closing)~n", [Txt]),
		    gen_tcp:close(CSock)
	    end
    end.

slowly_send_response(CSock, Answer) ->
    Response = f("HTTP/1.1 200 OK\r\nContent-Length: ~w\r\n\r\n~s",
		 [length(Answer), Answer]),
    lists:foreach(
      fun(Char) ->
	      timer:sleep(random:uniform(500)),
	      gen_tcp:send(CSock, <<Char>>)
      end,
      Response).

parse_req_num(Request) ->
    Opts = [caseless,{capture,all_but_first,list}],
    {match, [ReqNum]} = re:run(Request, "GET /(.*) HTTP", Opts),
    ReqNum.

parse_connection_type(Request) ->
    Opts = [caseless,{capture,all_but_first,list}],
    {match,[CType]} = re:run(Request, "connection: *(keep-alive|close)", Opts),
    case string:to_lower(CType) of
	"close" -> close;
	"keep-alive" -> keep_alive
    end.


set_random_seed() ->
    {_, _, Micros} = now(),
    A = erlang:phash2([make_ref(), self(), Micros]),
    random:seed(A, A, A).

f(F, A) -> lists:flatten(io_lib:format(F,A)).




%%-------------------------------------------------------------------------



otp_8106_pid(doc) ->
    ["OTP-8106 - deliver reply info using \"other\" pid"];
otp_8106_pid(suite) ->
    [];
otp_8106_pid(Config) when is_list(Config) ->
    case ?config(local_server, Config) of 
	ok ->
	    ReceiverPid = create_receiver(pid),
	    Receiver    = ReceiverPid, 
	    
	    otp8106(ReceiverPid, Receiver, Config), 

	    stop_receiver(ReceiverPid), 
	    
	    ok;
	_ ->
	    skip("Failed to start local http-server")
    end.  


otp_8106_fun(doc) ->
    ["OTP-8106 - deliver reply info using fun"];
otp_8106_fun(suite) ->
    [];
otp_8106_fun(Config) when is_list(Config) ->
    case ?config(local_server, Config) of 
	ok ->
	    ReceiverPid = create_receiver(function),
	    Receiver = otp_8106_deliver_fun(ReceiverPid), 
	    
	    otp8106(ReceiverPid, Receiver, Config), 

	    stop_receiver(ReceiverPid), 
	    
	    ok;
	_ ->
	    skip("Failed to start local http-server")
    end.


otp_8106_mfa(doc) ->
    ["OTP-8106 - deliver reply info using mfa callback"];
otp_8106_mfa(suite) ->
    [];
otp_8106_mfa(Config) when is_list(Config) ->
    case ?config(local_server, Config) of 
	ok ->
	    ReceiverPid = create_receiver(mfa),
	    Receiver    = {?MODULE, otp_8106_deliver, [mfa, ReceiverPid]}, 
	    
	    otp8106(ReceiverPid, Receiver, Config), 

	    stop_receiver(ReceiverPid), 
	    
	    ok;
	_ ->
	    skip("Failed to start local http-server")
    end.  


 otp8106(ReceiverPid, Receiver, Config) ->
     Port        = ?config(local_port, Config),
     URL         = ?URL_START ++ integer_to_list(Port) ++ "/dummy.html",
     Request     = {URL, []}, 
     HTTPOptions = [], 
     Options     = [{sync, false}, {receiver, Receiver}], 

     {ok, RequestId} = 
	 httpc:request(get, Request, HTTPOptions, Options),

     Body = 
	 receive 
	     {reply, ReceiverPid, {RequestId, {{_, 200, _}, _, B}}} ->
		 B;
	     {reply, ReceiverPid, Msg} ->
		 tsf(Msg);
	     {bad_reply, ReceiverPid, Msg} ->
		 tsf(Msg)
	 end,

     inets_test_lib:check_body(binary_to_list(Body)),
     ok.


create_receiver(Type) ->
    Parent = self(), 
    Receiver = fun() -> receiver(Type, Parent) end,
    spawn_link(Receiver).

stop_receiver(Pid) ->
    Pid ! {stop, self()}.

receiver(Type, Parent) ->
    receive
	{stop, Parent} ->
	    exit(normal);

	{http, ReplyInfo} when (Type =:= pid) ->
	    Parent ! {reply, self(), ReplyInfo},
	    receiver(Type, Parent);

	{Type, ReplyInfo} ->
	    Parent ! {reply, self(), ReplyInfo},
	    receiver(Type, Parent);
	
	Crap ->
	    Parent ! {reply, self(), {bad_reply, Crap}},
	    receiver(Type, Parent)
    end.


otp_8106_deliver_fun(ReceiverPid) ->
    fun(ReplyInfo) -> otp_8106_deliver(ReplyInfo, function, ReceiverPid) end.
	     
otp_8106_deliver(ReplyInfo, Type, ReceiverPid) -> 
    ReceiverPid ! {Type, ReplyInfo},
    ok.



%%-------------------------------------------------------------------------

otp_8056(doc) ->
    "OTP-8056";
otp_8056(suite) ->
    [];
otp_8056(Config) when is_list(Config) ->
    Method      = get,
    Port        = ?config(local_port, Config),
    URL         = ?URL_START ++ integer_to_list(Port) ++ "/dummy.html",
    Request     = {URL, []}, 
    HTTPOptions = [], 
    Options1    = [{sync, true}, {stream, {self, once}}], 
    Options2    = [{sync, true}, {stream, self}], 
    {error, streaming_error} = httpc:request(Method, Request, 
					     HTTPOptions, Options1), 
    tsp("request 1 failed as expected"),
    {error, streaming_error} = httpc:request(Method, Request, 
					     HTTPOptions, Options2), 
    tsp("request 2 failed as expected"),
    ok.


%%-------------------------------------------------------------------------

otp_8352(doc) ->
    "OTP-8352";
otp_8352(suite) ->
    [];
otp_8352(Config) when is_list(Config) ->
    tsp("otp_8352 -> entry with"
	"~n   Config: ~p", [Config]),
    case ?config(local_server, Config) of 
	ok ->
	    tsp("local-server running"),

	    tsp("initial profile info(1): ~p", [httpc:info()]),
	    
	    MaxSessions      = 5,
	    MaxKeepAlive     = 10, 
	    KeepAliveTimeout = timer:minutes(2), 
	    ConnOptions = [{max_sessions,          MaxSessions}, 
			   {max_keep_alive_length, MaxKeepAlive}, 
			   {keep_alive_timeout,    KeepAliveTimeout}], 
	    httpc:set_options(ConnOptions), 

	    Method       = get, 
	    Port         = ?config(local_port, Config),
	    URL          = ?URL_START ++ integer_to_list(Port) ++ "/dummy.html",
	    Request      = {URL, []}, 
	    Timeout      = timer:seconds(1), 
	    ConnTimeout  = Timeout + timer:seconds(1), 
	    HttpOptions1 = [{timeout, Timeout}, {connect_timeout, ConnTimeout}], 
	    Options1     = [{socket_opts, [{tos,    87}, 
					   {recbuf, 16#FFFF}, 
					   {sndbuf, 16#FFFF}]}], 
	    case httpc:request(Method, Request, HttpOptions1, Options1) of
		{ok, {{_,200,_}, [_ | _], ReplyBody1 = [_ | _]}} ->
		    %% equivaliant to httpc:request(get, {URL, []}, [], []),
		    inets_test_lib:check_body(ReplyBody1);
		{ok, UnexpectedReply1} ->
		    tsf({unexpected_reply, UnexpectedReply1});
		{error, _} = Error1 ->
		    tsf({bad_reply, Error1})
	    end,

	    tsp("profile info (2): ~p", [httpc:info()]),

	    HttpOptions2 = [], 
	    Options2     = [{socket_opts, [{tos,    84}, 
					   {recbuf, 32#1FFFF}, 
					   {sndbuf, 32#1FFFF}]}], 
	    case httpc:request(Method, Request, HttpOptions2, Options2) of
		{ok, {{_,200,_}, [_ | _], ReplyBody2 = [_ | _]}} ->
		    %% equivaliant to httpc:request(get, {URL, []}, [], []),
		    inets_test_lib:check_body(ReplyBody2);
		{ok,  UnexpectedReply2} ->
		    tsf({unexpected_reply, UnexpectedReply2});
		{error, _} = Error2 ->
		    tsf({bad_reply, Error2})
	    end,
	    tsp("profile info (3): ~p", [httpc:info()]),
	    ok;

	_ ->
	    skip("Failed to start local http-server")
    end.  


%%-------------------------------------------------------------------------

otp_8371(doc) ->
    ["OTP-8371"];
otp_8371(suite) ->
    [];
otp_8371(Config) when is_list(Config) ->
    ok = httpc:set_options([{ipv6, disabled}]), % also test the old option 
    {DummyServerPid, Port} = dummy_server(ipv4),
    
    URL = ?URL_START ++ integer_to_list(Port) ++ 
	"/ensure_host_header_with_port.html",
        
    case httpc:request(get, {URL, []}, [], []) of
	{ok, Result} ->
	    case Result of
		{{_, 200, _}, _Headers, Body} ->
		    tsp("expected response with"
			"~n   Body: ~p", [Body]),
		    ok;
		{StatusLine, Headers, Body} ->
		    tsp("expected response with"
			"~n   StatusLine: ~p"
			"~n   Headers:    ~p"
			"~n   Body:       ~p", [StatusLine, Headers, Body]),
		    tsf({unexpected_result, 
			 [{status_line, StatusLine}, 
			  {headers,     Headers}, 
			  {body,        Body}]});
		_ ->
		    tsf({unexpected_result, Result})
	    end;
	Error ->
	    tsf({request_failed, Error})
    end,

    DummyServerPid ! stop,
    ok = httpc:set_options([{ipv6, enabled}]),   
    ok.


%%-------------------------------------------------------------------------

otp_8739(doc) ->
    ["OTP-8739"];
otp_8739(suite) ->
    [];
otp_8739(Config) when is_list(Config) ->
    {_DummyServerPid, Port} = otp_8739_dummy_server(),
    URL = ?URL_START ++ integer_to_list(Port) ++ "/dummy.html",
    Method      = get,
    Request     = {URL, []}, 
    HttpOptions = [{connect_timeout, 500}, {timeout, 1}], 
    Options     = [{sync, true}], 
    case httpc:request(Method, Request, HttpOptions, Options) of
	{error, timeout} ->
	    %% And now we check the size of the handler db
	    Info = httpc:info(),
	    tsp("Info: ~p", [Info]),
	    {value, {handlers, Handlers}} = 
		lists:keysearch(handlers, 1, Info),
	    case Handlers of
		[] ->
		    ok;
		_ ->
		    tsf({unexpected_handlers, Handlers})
	    end;
	Unexpected ->
	    tsf({unexpected, Unexpected})
    end.


otp_8739_dummy_server() ->
    Parent = self(), 
    Pid = spawn_link(fun() -> otp_8739_dummy_server_init(Parent) end),
    receive
	{port, Port} ->
	    {Pid, Port}
    end.

otp_8739_dummy_server_init(Parent) ->
    {ok, ListenSocket} = 
	gen_tcp:listen(0, [binary, inet, {packet, 0},
			   {reuseaddr,true},
			   {active, false}]),
    {ok, Port} = inet:port(ListenSocket),
    Parent ! {port, Port},
    otp_8739_dummy_server_main(Parent, ListenSocket).

otp_8739_dummy_server_main(_Parent, ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
	{ok, Sock} ->
	    %% Ignore the request, and simply wait for the socket to close
	    receive
		{tcp_closed, Sock} ->
		    (catch gen_tcp:close(ListenSocket)),
		    exit(normal);
		{tcp_error, Sock, Reason} ->
		    tsp("socket error: ~p", [Reason]),
		    (catch gen_tcp:close(ListenSocket)),
		    exit(normal)
	    after 10000 ->
		    %% Just in case
		    (catch gen_tcp:close(Sock)),
		    (catch gen_tcp:close(ListenSocket)),
		    exit(timeout)
	    end;
	Error ->
	    exit(Error)
    end.


%%-------------------------------------------------------------------------

initial_server_connect(doc) ->
    ["If this test cases times out the init of httpc_handler process is"
     "blocking the manager/client process (implementation dependent which) but nither"
     "should be blocked."];
initial_server_connect(suite) ->
    [];
initial_server_connect(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    ok = httpc:set_options([{ipfamily, inet}]),

    CertFile   = filename:join(DataDir, "ssl_server_cert.pem"),
    SSLOptions = [{certfile, CertFile}, {keyfile, CertFile}],

    {DummyServerPid, Port} = dummy_ssl_server_hang(self(), ipv4, SSLOptions),

    URL = ?SSL_URL_START ++ integer_to_list(Port) ++ "/index.html",

    httpc:request(get, {URL, []}, [{ssl,{essl,[]}}], [{sync, false}]),

    [{session_cookies,[]}] = httpc:which_cookies(),

    DummyServerPid ! stop,
    ok = httpc:set_options([{ipfamily, inet6fb4}]).
    
%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
setup_server_dirs(ServerRoot, DocRoot, DataDir) ->   
    ConfDir = filename:join(ServerRoot, "conf"),
    CgiDir =  filename:join(ServerRoot, "cgi-bin"),
    ok = file:make_dir(ServerRoot),
    ok = file:make_dir(DocRoot),
    ok = file:make_dir(ConfDir),
    ok = file:make_dir(CgiDir),

    {ok, Files} = file:list_dir(DataDir),
    
    lists:foreach(fun(File) -> case lists:suffix(".html", File) of
				   true ->
				       inets_test_lib:copy_file(File, 
								DataDir, 
								DocRoot);
				   false ->
				       ok
			       end
		  end, Files),
    
    Cgi = case test_server:os_type() of
	      {win32, _} ->
		  "cgi_echo.exe";
	      _ ->
		  "cgi_echo"
	  end,
    
    inets_test_lib:copy_file(Cgi, DataDir, CgiDir),
    inets_test_lib:copy_file("mime.types", DataDir, ConfDir).

create_config(FileName, ComType, Port, PrivDir, ServerRoot, DocRoot, 
	      SSLDir) ->
    MaxHdrSz     = io_lib:format("~p", [256]),
    MaxHdrAct    = io_lib:format("~p", [close]),
    SSL =
	case ComType of
	    ssl ->
		[cline(["SSLCertificateFile ", 
			filename:join(SSLDir, "ssl_server_cert.pem")]),
		 cline(["SSLCertificateKeyFile ",
			filename:join(SSLDir, "ssl_server_cert.pem")]),
		 cline(["SSLVerifyClient 0"])];
	    _ ->
		[]
	end,

    Mod_order = "Modules mod_alias mod_auth mod_esi mod_actions mod_cgi" 
	" mod_include mod_dir mod_get mod_head" 
	" mod_log mod_disk_log mod_trace",
	    
    %% BindAddress = "*|inet", % Force the use of IPv4
    BindAddress = "*", % This corresponds to using IpFamily inet6fb4

    HttpConfig = [
		  cline(["BindAddress ", BindAddress]),
		  cline(["Port ", integer_to_list(Port)]),
		  cline(["ServerName ", "httpc_test"]),
		  cline(["SocketType ", atom_to_list(ComType)]),
		  cline([Mod_order]),
		  cline(["ServerRoot ", ServerRoot]),
		  cline(["DocumentRoot ", DocRoot]),
		  cline(["MaxHeaderSize ",MaxHdrSz]),
		  cline(["MaxHeaderAction ",MaxHdrAct]),
		  cline(["DirectoryIndex ", "index.html "]),
		  cline(["DefaultType ", "text/plain"]),
		  cline(["ScriptAlias /cgi-bin/ ", 
			 filename:join(ServerRoot, "cgi-bin"), "/"]),
		  SSL],
    ConfigFile = filename:join([PrivDir,FileName]),
    {ok, Fd} = file:open(ConfigFile, [write]),
    ok = file:write(Fd, lists:flatten(HttpConfig)),
    ok = file:close(Fd).

cline(List) ->
    lists:flatten([List, "\r\n"]).

receive_streamed_body(RequestId, Body) ->
    receive 
	{http, {RequestId, stream, BinBodyPart}} ->
	    receive_streamed_body(RequestId, 
				  <<Body/binary, BinBodyPart/binary>>);
	{http, {RequestId, stream_end, _Headers}} ->
	    Body;
	{http, Msg} ->	    
	    tsf(Msg)
    end.

receive_streamed_body(RequestId, Body, Pid) ->
    httpc:stream_next(Pid),
    test_server:format("~p:receive_streamed_body -> requested next stream ~n", [?MODULE]),
    receive 
	{http, {RequestId, stream, BinBodyPart}} ->
	    receive_streamed_body(RequestId, 
				  <<Body/binary, BinBodyPart/binary>>, 
				  Pid);
	{http, {RequestId, stream_end, _Headers}} ->
	    Body;
	{http, Msg} ->	    
	    tsf(Msg)
    end.

%% Perform a synchronous stop
dummy_server_stop(Pid) ->
    Pid ! {stop, self()},
    receive 
	{stopped, Pid} ->
	    ok
    end.

dummy_server(IpV) ->
    dummy_server(self(), ip_comm, IpV, []).

dummy_server(SocketType, IpV, Extra) ->
    dummy_server(self(), SocketType, IpV, Extra).

dummy_server(Caller, SocketType, IpV, Extra) ->
    Args = [Caller, SocketType, IpV, Extra], 
    Pid = spawn(httpc_SUITE, dummy_server_init, Args),
    receive
	{port, Port} ->
	    {Pid, Port}
    end.

dummy_server_init(Caller, ip_comm, IpV, _) ->
    BaseOpts = [binary, {packet, 0}, {reuseaddr,true}, {active, false}], 
    {ok, ListenSocket} = 
	case IpV of 
	    ipv4 ->
		tsp("ip_comm ipv4 listen", []),
		gen_tcp:listen(0, [inet | BaseOpts]);
	    ipv6 ->
		tsp("ip_comm ipv6 listen", []),
		gen_tcp:listen(0, [inet6 | BaseOpts])
	end,
    {ok, Port} = inet:port(ListenSocket),
    tsp("dummy_server_init(ip_comm) -> Port: ~p", [Port]),
    Caller ! {port, Port},
    dummy_ipcomm_server_loop({httpd_request, parse, [?HTTP_MAX_HEADER_SIZE]},
			     [], ListenSocket);
dummy_server_init(Caller, essl, IpV, SSLOptions) ->
    BaseOpts = [{ssl_imp, new}, 
		{backlog, 128}, binary, {reuseaddr,true}, {active, false} |
	        SSLOptions], 
    dummy_ssl_server_init(Caller, BaseOpts, IpV).

dummy_ssl_server_init(Caller, BaseOpts, IpV) ->
    {ok, ListenSocket} = 
	case IpV of 
	    ipv4 ->
		tsp("dummy_ssl_server_init -> ssl ipv4 listen", []),
		ssl:listen(0, [inet | BaseOpts]);
	    ipv6 ->
		tsp("dummy_ssl_server_init -> ssl ipv6 listen", []),
		ssl:listen(0, [inet6 | BaseOpts])
	end,
    tsp("dummy_ssl_server_init -> ListenSocket: ~p", [ListenSocket]),    
    {ok, {_, Port}} = ssl:sockname(ListenSocket),
    tsp("dummy_ssl_server_init -> Port: ~p", [Port]),
    Caller ! {port, Port},
    dummy_ssl_server_loop({httpd_request, parse, [?HTTP_MAX_HEADER_SIZE]},
			  [], ListenSocket).

dummy_ipcomm_server_loop(MFA, Handlers, ListenSocket) ->
    receive
	stop ->
	    tsp("dummy_ipcomm_server_loop -> stop handlers", []),
	    lists:foreach(fun(Handler) -> Handler ! stop end, Handlers);
	{stop, From} ->
	    tsp("dummy_ipcomm_server_loop -> "
		"stop command from ~p for handlers (~p)", [From, Handlers]),
	    Stopper = fun(Handler) -> Handler ! stop end, 
	    lists:foreach(Stopper, Handlers),
	    From ! {stopped, self()}
    after 0 ->
	    tsp("dummy_ipcomm_server_loop -> await accept", []),
	    {ok, Socket} = gen_tcp:accept(ListenSocket),
	    tsp("dummy_ipcomm_server_loop -> accepted: ~p", [Socket]),
	    HandlerPid  = dummy_request_handler(MFA, Socket),
	    tsp("dummy_icomm_server_loop -> handler created: ~p", [HandlerPid]),
	    gen_tcp:controlling_process(Socket, HandlerPid),
	    tsp("dummy_ipcomm_server_loop -> "
		"control transfered to handler", []),
	    HandlerPid ! ipcomm_controller,
	    tsp("dummy_ipcomm_server_loop -> "
		"handler informed about control transfer", []),
	    dummy_ipcomm_server_loop(MFA, [HandlerPid | Handlers],
			      ListenSocket)
    end.

dummy_ssl_server_loop(MFA, Handlers, ListenSocket) ->
    receive
	stop ->
	    tsp("dummy_ssl_server_loop -> stop handlers", []),
	    lists:foreach(fun(Handler) -> Handler ! stop end, Handlers);
	{stop, From} ->
	    tsp("dummy_ssl_server_loop -> "
		"stop command from ~p for handlers (~p)", [From, Handlers]),
	    Stopper = fun(Handler) -> Handler ! stop end, 
	    lists:foreach(Stopper, Handlers),
	    From ! {stopped, self()}
    after 0 ->
	    tsp("dummy_ssl_server_loop -> await accept", []),
	    {ok, Socket} = ssl:transport_accept(ListenSocket),
	    tsp("dummy_ssl_server_loop -> accepted: ~p", [Socket]),
	    HandlerPid  = dummy_request_handler(MFA, Socket),
	    tsp("dummy_ssl_server_loop -> handler created: ~p", [HandlerPid]),
	    ssl:controlling_process(Socket, HandlerPid),
	    tsp("dummy_ssl_server_loop -> control transfered to handler", []),
	    HandlerPid ! ssl_controller,
	    tsp("dummy_ssl_server_loop -> "
		"handler informed about control transfer", []),
	    dummy_ssl_server_loop(MFA, [HandlerPid | Handlers],
				  ListenSocket)
    end.

dummy_request_handler(MFA, Socket) ->
    tsp("spawn request handler", []),
    spawn(httpc_SUITE, dummy_request_handler_init, [MFA, Socket]).

dummy_request_handler_init(MFA, Socket) ->
    SockType = 
	receive 
	    ipcomm_controller ->
		tsp("dummy_request_handler_init -> "
		    "received ip_comm controller - activate", []),
		inet:setopts(Socket, [{active, true}]),
		ip_comm;
	    ssl_controller ->
		tsp("dummy_request_handler_init -> "
		    "received ssl controller - activate", []),
		ssl:setopts(Socket, [{active, true}]),
		ssl
	end,
    dummy_request_handler_loop(MFA, SockType, Socket).
    
dummy_request_handler_loop({Module, Function, Args}, SockType, Socket) ->
    tsp("dummy_request_handler_loop -> entry with"
	"~n   Module:   ~p"
	"~n   Function: ~p"
	"~n   Args:     ~p", [Module, Function, Args]),
    receive 
	{Proto, _, Data} when (Proto =:= tcp) orelse (Proto =:= ssl) ->
	    tsp("dummy_request_handler_loop -> [~w] Data ~p", [Proto, Data]),
	    case handle_request(Module, Function, [Data | Args], Socket, Proto) of
		stop when Proto =:= tcp ->
		    gen_tcp:close(Socket);
		stop when Proto =:= ssl ->
		    ssl:close(Socket);
		NewMFA ->
		    dummy_request_handler_loop(NewMFA, SockType, Socket)
	    end;
	stop when SockType =:= ip_comm ->
	    gen_tcp:close(Socket);
	stop when SockType =:= ssl ->
	    ssl:close(Socket)
    end.


mk_close(tcp) -> fun(Sock) -> gen_tcp:close(Sock) end;
mk_close(ssl) -> fun(Sock) -> ssl:close(Sock)     end.

mk_send(tcp) -> fun(Sock, Data) -> gen_tcp:send(Sock, Data) end;
mk_send(ssl) -> fun(Sock, Data) -> ssl:send(Sock, Data)     end.

handle_request(Module, Function, Args, Socket, Proto) ->
    Close = mk_close(Proto),
    Send  = mk_send(Proto),
    handle_request(Module, Function, Args, Socket, Close, Send).

handle_request(Module, Function, Args, Socket, Close, Send) ->
    tsp("handle_request -> entry with"
	"~n   Module:   ~p"
	"~n   Function: ~p"
	"~n   Args:     ~p", [Module, Function, Args]),
    case Module:Function(Args) of
	{ok, Result} ->
	    tsp("handle_request -> ok"
		"~n   Result: ~p", [Result]),
	    case (catch handle_http_msg(Result, Socket, Close, Send)) of
		stop ->
		    stop;
		<<>> ->
		    tsp("handle_request -> empty data"),
		    {httpd_request, parse, [[<<>>, ?HTTP_MAX_HEADER_SIZE]]};
		Data ->	
		    handle_request(httpd_request, parse, 
				   [Data |[?HTTP_MAX_HEADER_SIZE]], Socket, 
				   Close, Send)
	    end;
	NewMFA ->
	    tsp("handle_request -> "
		"~n   NewMFA: ~p", [NewMFA]),
	    NewMFA
    end.

handle_http_msg({_, RelUri, _, {_, Headers}, Body}, Socket, Close, Send) ->
    tsp("handle_http_msg -> entry with: "
	"~n   RelUri:  ~p"
	"~n   Headers: ~p"
	"~n   Body:    ~p", [RelUri, Headers, Body]),
    NextRequest = 
	case RelUri of
	    "/dummy_headers.html" ->
		<<>>;
	    "/no_headers.html" ->
		stop;
	    "/just_close.html" ->
		stop;
	    _ ->
		ContentLength = content_length(Headers),    
		case size(Body) - ContentLength of
		    0 ->
			<<>>;
		    _ ->
			<<_BodyThisReq:ContentLength/binary, 
			  Next/binary>> = Body,
			Next
		end
	end,
   
    tsp("handle_http_msg -> NextRequest: ~p", [NextRequest]),
    case (catch ets:lookup(cookie, cookies)) of 
	[{cookies, true}]->
	    tsp("handle_http_msg -> check cookies ~p", []),
	    check_cookie(Headers);
	_ ->
	    ok
    end,
    
    DefaultResponse = "HTTP/1.1 200 ok\r\n" ++
	"Content-Length:32\r\n\r\n"
	"<HTML><BODY>foobar</BODY></HTML>",

    Msg = 
	case RelUri of
	    "/just_close.html" ->
		close; 
	    "/no_content.html" ->
		"HTTP/1.0 204 No Content\r\n\r\n";
	    "/no_headers.html" ->
		"HTTP/1.0 200 OK\r\n\r\nTEST";
	    "/ensure_host_header_with_port.html" ->
		%% tsp("handle_http_msg -> validate host with port"),
		case ensure_host_header_with_port(Headers) of
		    true ->
			B = 
			    "<HTML><BODY>" ++ 
			    "host with port" ++ 
			    "</BODY></HTML>", 
			Len = integer_to_list(length(B)), 
			"HTTP/1.1 200 ok\r\n" ++
			    "Content-Length:" ++ Len ++ "\r\n\r\n" ++ B;
		    false ->
			B = 
			    "<HTML><BODY>" ++ 
			    "Internal Server Error - host without port" ++
			    "</BODY></HTML>", 
			Len = integer_to_list(length(B)), 
			"HTTP/1.1 500 Internal Server Error\r\n" ++
			    "Content-Length:" ++ Len ++ "\r\n\r\n" ++ B
		end;
	    "/300.html" ->
		NewUri = ?URL_START ++
		    integer_to_list(?IP_PORT) ++ "/dummy.html",
		"HTTP/1.1 300 Multiple Choices\r\n" ++
		    "Location:" ++ NewUri ++  "\r\n" ++
		    "Content-Length:0\r\n\r\n";
	    "/301.html" ->
		NewUri = ?URL_START ++
		    integer_to_list(?IP_PORT) ++ "/dummy.html",
		"HTTP/1.1 301 Moved Permanently\r\n" ++
		    "Location:" ++ NewUri ++  "\r\n" ++
		    "Content-Length:80\r\n\r\n" ++
		    "<HTML><BODY><a href=" ++ NewUri ++
		    ">New place</a></BODY></HTML>";
	    "/302.html" ->
		NewUri = ?URL_START ++
		    integer_to_list(?IP_PORT) ++ "/dummy.html",
		"HTTP/1.1 302 Found \r\n" ++
		    "Location:" ++ NewUri ++  "\r\n" ++
		    "Content-Length:80\r\n\r\n" ++
		    "<HTML><BODY><a href=" ++ NewUri ++
		    ">New place</a></BODY></HTML>";
	    "/303.html" ->
		NewUri = ?URL_START ++
		    integer_to_list(?IP_PORT) ++ "/dummy.html",
		"HTTP/1.1 303 See Other \r\n" ++
		    "Location:" ++ NewUri ++  "\r\n" ++
		    "Content-Length:80\r\n\r\n" ++
		    "<HTML><BODY><a href=" ++ NewUri ++
		    ">New place</a></BODY></HTML>";
	    "/307.html" ->
		NewUri = ?URL_START ++
		    integer_to_list(?IP_PORT) ++ "/dummy.html",
		"HTTP/1.1 307 Temporary Rediect \r\n" ++
		    "Location:" ++ NewUri ++  "\r\n" ++
		    "Content-Length:80\r\n\r\n" ++
		    "<HTML><BODY><a href=" ++ NewUri ++
		    ">New place</a></BODY></HTML>";
	    "/500.html" ->
		"HTTP/1.1 500 Internal Server Error\r\n" ++
		    "Content-Length:47\r\n\r\n" ++
		    "<HTML><BODY>Internal Server Error</BODY></HTML>";
	    "/503.html" ->
		case ets:lookup(unavailable, 503) of
		    [{503, unavailable}] -> 
			ets:insert(unavailable, {503, available}),
			"HTTP/1.1 503 Service Unavailable\r\n" ++
			    "Retry-After:5\r\n" ++
			    "Content-Length:47\r\n\r\n" ++
			    "<HTML><BODY>Internal Server Error</BODY></HTML>";
		    [{503, available}]   ->
			DefaultResponse;
		    [{503, long_unavailable}]  ->
			"HTTP/1.1 503 Service Unavailable\r\n" ++
			    "Retry-After:120\r\n" ++
			    "Content-Length:47\r\n\r\n" ++
			    "<HTML><BODY>Internal Server Error</BODY></HTML>"
		end;
	    "/redirectloop.html" -> %% Create a potential endless loop!
		{ok, Port} = inet:port(Socket),
		NewUri = ?URL_START ++
		    integer_to_list(Port) ++ "/redirectloop.html",
		"HTTP/1.1 300 Multiple Choices\r\n" ++
		    "Location:" ++ NewUri ++  "\r\n" ++
		    "Content-Length:0\r\n\r\n";
	    "/userinfo.html" ->
		Challange = "HTTP/1.1 401 Unauthorized \r\n" ++
		    "WWW-Authenticate:Basic" ++"\r\n" ++
		    "Content-Length:0\r\n\r\n",
		case auth_header(Headers) of
		    {ok, Value} ->
			handle_auth(Value, Challange, DefaultResponse);
		    _ ->
			Challange
		end;
	    "/dummy_headers.html" ->
		%% The client will only care about the Transfer-Encoding
		%% header the rest of these headers are left to the
		%% user to evaluate. This is not a valid response 
		%% it only tests that the header handling code works.
		Head = "HTTP/1.1 200 ok\r\n" ++
		    "Content-Length:32\r\n" ++
		    "Pragma:1#no-cache\r\n"  ++
		    "Via:1.0 fred, 1.1 nowhere.com (Apache/1.1)\r\n"  ++
		    "Warning:1#pseudonym foobar\r\n"  ++
		    "Vary:*\r\n"  ++
		    "Trailer:Other:inets_test\r\n"  ++
		    "Upgrade:HTTP/2.0\r\n"  ++
		    "Age:4711\r\n" ++ 
		    "Transfer-Encoding:chunked\r\n" ++
		    "Content-Encoding:foo\r\n" ++
		    "Content-Language:en\r\n"  ++
		    "Content-Location:http://www.foobar.se\r\n"  ++
		    "Content-MD5:104528739076276072743283077410617235478\r\n" 
		    ++
		    "Content-Range:Sat, 29 Oct 1994 19:43:31 GMT\r\n"  ++
		    "Expires:Sat, 29 Oct 1994 19:43:31 GMT\r\n"  ++
		    "Proxy-Authenticate:#1Basic"  ++
		    "\r\n\r\n",
		Send(Socket, Head),
		Send(Socket, http_chunk:encode("<HTML><BODY>fo")),
		Send(Socket, http_chunk:encode("obar</BODY></HTML>")),
		http_chunk:encode_last();
	    "/capital_transfer_encoding.html" ->
		Head =  "HTTP/1.1 200 ok\r\n" ++
		    "Transfer-Encoding:Chunked\r\n\r\n",
		Send(Socket, Head),
		Send(Socket, http_chunk:encode("<HTML><BODY>fo")),
		Send(Socket, http_chunk:encode("obar</BODY></HTML>")),
		http_chunk:encode_last();
	    "/cookie.html" ->
		"HTTP/1.1 200 ok\r\n" ++
		    "set-cookie:" ++ "test_cookie=true; path=/;" ++
		    "max-age=60000\r\n" ++
		    "Content-Length:32\r\n\r\n"++
		    "<HTML><BODY>foobar</BODY></HTML>";
	    "/missing_crlf.html" ->
		"HTTP/1.1 200 ok" ++
		    "Content-Length:32\r\n" ++
		    "<HTML><BODY>foobar</BODY></HTML>";
	    "/wrong_statusline.html" ->
		"ok 200 HTTP/1.1\r\n\r\n" ++
		    "Content-Length:32\r\n\r\n" ++
		    "<HTML><BODY>foobar</BODY></HTML>";
	    "/once_chunked.html" ->
		Head =  "HTTP/1.1 200 ok\r\n" ++
		    "Transfer-Encoding:Chunked\r\n\r\n",
		Send(Socket, Head),
		Send(Socket, http_chunk:encode("<HTML><BODY>fo")),
		Send(Socket, 
			     http_chunk:encode("obar</BODY></HTML>")),
		http_chunk:encode_last();
	    "/once.html" ->
		Head =  "HTTP/1.1 200 ok\r\n" ++
		    "Content-Length:32\r\n\r\n", 
		Send(Socket, Head), 
		Send(Socket, "<HTML><BODY>fo"),
		test_server:sleep(1000),
		Send(Socket, "ob"),
		test_server:sleep(1000),
		Send(Socket, "ar</BODY></HTML>");
	    "/invalid_http.html" ->
		"HTTP/1.1 301\r\nDate:Sun, 09 Dec 2007 13:04:18 GMT\r\n" ++ 
		    "Transfer-Encoding:chunked\r\n\r\n";
	    "/missing_reason_phrase.html" ->
		"HTTP/1.1 200\r\n" ++
		    "Content-Length: 32\r\n\r\n"
		    "<HTML><BODY>foobar</BODY></HTML>";
	    "/missing_CR.html" ->
		"HTTP/1.1 200 ok\n" ++
		    "Content-Length:32\r\n\n"
		    "<HTML><BODY>foobar</BODY></HTML>";
	    _ ->
		DefaultResponse
	end,
    
    tsp("handle_http_msg -> Msg: ~p", [Msg]),
    case Msg of
	ok ->
	    %% Previously, this resulted in an {error, einval}. Now what?
	    ok;
	close ->
	    %% Nothing to send, just close
	    Close(Socket);
	_ when is_list(Msg) orelse is_binary(Msg) ->
	    Send(Socket, Msg)
    end,
    tsp("handle_http_msg -> done"),
    NextRequest.

ensure_host_header_with_port([]) ->
    false;
ensure_host_header_with_port(["host: " ++ Host| _]) ->
    case string:tokens(Host, [$:]) of
	[ActualHost, Port] ->
	    tsp("ensure_host_header_with_port -> "
		"~n   ActualHost: ~p"
		"~n   Port:       ~p", [ActualHost, Port]),
	    true;
	_ ->
	    false
    end;
ensure_host_header_with_port([_|T]) ->
    ensure_host_header_with_port(T).

auth_header([]) ->
    auth_header_not_found;
auth_header(["authorization:" ++ Value | _]) ->
    {ok, string:strip(Value)};
auth_header([_ | Tail]) ->
    auth_header(Tail).

handle_auth("Basic " ++ UserInfo, Challange, DefaultResponse) ->
    case string:tokens(base64:decode_to_string(UserInfo), ":") of
	["alladin", "sesame"] = Auth ->
	    test_server:format("Auth: ~p~n", [Auth]),
	    DefaultResponse;
	Other ->
	    test_server:format("UnAuth: ~p~n", [Other]),
	    Challange
    end.

check_cookie([]) ->
    tsf(no_cookie_header);
check_cookie(["cookie:" ++ _Value | _]) ->
    ok;
check_cookie([_Head | Tail]) ->
   check_cookie(Tail).

content_length([]) ->
    0;
content_length(["content-length:" ++ Value | _]) ->
    list_to_integer(string:strip(Value));
content_length([_Head | Tail]) ->
   content_length(Tail).

%% -------------------------------------------------------------------------

simple_request_and_verify(Config, 
			  Method, Request, HttpOpts, Opts, VerifyResult) 
  when (is_list(Config) andalso 
	is_atom(Method) andalso 
	is_list(HttpOpts) andalso 
	is_list(Opts) andalso 
	is_function(VerifyResult, 1)) ->
    tsp("request_and_verify -> entry with"
	"~n   Method:   ~p"
	"~n   Request:  ~p"
	"~n   HttpOpts: ~p"
	"~n   Opts:     ~p", [Method, Request, HttpOpts, Opts]),
    case ?config(local_server, Config) of
	ok ->
	    tsp("request_and_verify -> local-server running"),
	    Result = (catch httpc:request(Method, Request, HttpOpts, Opts)),
	    VerifyResult(Result);
	_ ->
	    tsp("request_and_verify -> local-server *not* running - skip"),
	    hard_skip("Local http-server not running")
    end.




not_implemented_yet() ->
    exit(not_implemented_yet).

p(F) ->
    p(F, []).

p(F, A) ->
    io:format("~p ~w:" ++ F ++ "~n", [self(), ?MODULE | A]).

tsp(F) ->
    inets_test_lib:tsp("[~w]" ++ F, [?MODULE]).
tsp(F, A) ->
    inets_test_lib:tsp("[~w]" ++ F, [?MODULE|A]).

tsf(Reason) ->
    test_server:fail(Reason).


dummy_ssl_server_hang(Caller, IpV, SslOpt) ->
    Pid = spawn(httpc_SUITE, dummy_ssl_server_hang_init, [Caller, IpV, SslOpt]),
    receive
	{port, Port} ->
	    {Pid, Port}
    end.

dummy_ssl_server_hang_init(Caller, IpV, SslOpt) ->
    {ok, ListenSocket} =
	case IpV of
	    ipv4 ->
		ssl:listen(0, [binary, inet, {packet, 0},
			       {reuseaddr,true},
			       {active, false}] ++ SslOpt);
	    ipv6 ->
		ssl:listen(0, [binary, inet6, {packet, 0},
			       {reuseaddr,true},
			       {active, false}] ++ SslOpt)
	end,
    {ok, {_,Port}} = ssl:sockname(ListenSocket),
    tsp("dummy_ssl_server_hang_init -> Port: ~p", [Port]),
    Caller ! {port, Port},
    {ok, AcceptSocket} = ssl:transport_accept(ListenSocket),
    dummy_ssl_server_hang_loop(AcceptSocket).

dummy_ssl_server_hang_loop(_) ->
    %% Do not do ssl:ssl_accept as we
    %% want to time out the underlying gen_tcp:connect
    receive
	stop ->
	    ok
    end.

hard_skip(Reason) ->
    throw(skip(Reason)).

skip(Reason) ->
    {skip, Reason}.
