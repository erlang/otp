%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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

%% 
%% ts:run(inets, httpc_proxy_SUITE, [batch]).
%% ct:run("../inets_test", httpc_proxy_SUITE).
%%

-module(httpc_proxy_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("kernel/include/file.hrl").
-include("inets_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(LOCAL_PROXY_SCRIPT, "server_proxy.sh").
-define(p(F, A), % Debug printout
	begin
	    io:format(
	      "~w ~w: " ++ begin F end,
	      [self(),?MODULE] ++ begin A end)
	end).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,local_proxy},
     {group,local_proxy_https}].

groups() -> 
    [{local_proxy,[],
      [http_emulate_lower_versions
       |local_proxy_cases()]},
     {local_proxy_https,[],
      local_proxy_cases() ++ local_proxy_https_cases()}].

%% internal functions

local_proxy_cases() ->
    [http_head,
     http_get,
     http_options,
     http_trace,
     http_post,
     http_put,
     http_delete,
     http_delete_body,
     http_headers,
     http_proxy_auth,
     http_doesnotexist,
     http_stream,
     http_not_modified_otp_6821].

local_proxy_https_cases() ->
    [https_connect_error].

%%--------------------------------------------------------------------

init_per_suite(Config0) ->
    case init_apps(suite_apps(), Config0) of
	Config when is_list(Config) ->
	    make_cert_files(dsa, "server-", Config),
	    Config;
	Other ->
	    Other
    end.

end_per_suite(_Config) ->
    [app_stop(App) || App <- r(suite_apps())],
    ok.

%% internal functions

suite_apps() ->
    [asn1,crypto,public_key].
    
%%--------------------------------------------------------------------

init_per_group(local_proxy, Config) ->
    init_local_proxy([{protocol,http}|Config]);
init_per_group(local_proxy_https, Config) ->
    init_local_proxy([{protocol,https}|Config]).

end_per_group(Group, Config)
  when
      Group =:= local_proxy;
      Group =:= local_proxy_https ->
    rcmd_local_proxy(["stop"], Config),
    Config;
end_per_group(_, Config) ->
    Config.

%%--------------------------------------------------------------------

init_per_testcase(Case, Config0) ->
    ct:timetrap({seconds,30}),
    Apps = apps(Case, Config0),
    case init_apps(Apps, Config0) of
	Config when is_list(Config) ->
	    case app_start(inets, Config) of
		ok ->
		    Config;
		Error ->
		    [app_stop(N) || N <- [inets|r(Apps)]],
		    ct:fail({could_not_init_inets,Error})
	    end;
	E3 ->
	    E3
    end.

end_per_testcase(_Case, Config) ->
    app_stop(inets),
    Config.

%% internal functions

apps(_Case, Config) ->
    case proplists:get_value(protocol, Config) of
	https ->
	    [ssl];
	_ ->
	    []
    end.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

http_head(doc) ->
    ["Test http/https HEAD request."];
http_head(Config) when is_list(Config) ->
    Method = head,
    URL = url("/index.html", Config),
    Request = {URL,[]},
    HttpOpts = [],
    Opts = [],
    {ok,{{_,200,_},[_|_],[]}} =
	httpc:request(Method, Request, HttpOpts, Opts),
    ok.

%%--------------------------------------------------------------------

http_get(doc) ->
    ["Test http/https GET request."];
http_get(Config) when is_list(Config) ->
    Method = get,
    URL = url("/index.html", Config),
    Request = {URL,[]},
    Timeout = timer:seconds(1),
    ConnTimeout = Timeout + timer:seconds(1),

    HttpOpts1 = [{timeout,Timeout},{connect_timeout,ConnTimeout}],
    Opts1 = [],
    {ok,{{_,200,_},[_|_],[_|_]=B1}} =
	httpc:request(Method, Request, HttpOpts1, Opts1),
    inets_test_lib:check_body(B1),

    HttpOpts2 = [],
    Opts2 = [{body_format,binary}],
    {ok,{{_,200,_},[_|_],B2}} =
	httpc:request(Method, Request, HttpOpts2, Opts2),
    inets_test_lib:check_body(binary_to_list(B2)).

%%--------------------------------------------------------------------

http_options(doc) ->
    ["Perform an OPTIONS request."];
http_options(Config) when is_list(Config) ->
    Method = options,
    URL = url("/index.html", Config),
    Request = {URL,[]},
    HttpOpts = [],
    Opts = [],
    {ok,{{_,200,_},Headers,_}} =
	httpc:request(Method, Request, HttpOpts, Opts),
    {value,_} = lists:keysearch("allow", 1, Headers),
    ok.

%%--------------------------------------------------------------------

http_trace(doc) ->
    ["Perform a TRACE request."];
http_trace(Config) when is_list(Config) ->
    Method = trace,
    URL = url("/index.html", Config),
    Request = {URL,[]},
    HttpOpts = [],
    Opts = [],
    {ok,{{_,200,_},[_|_],"TRACE "++_}} =
	httpc:request(Method, Request, HttpOpts, Opts),
    ok.

%%--------------------------------------------------------------------

http_post(doc) ->
    ["Perform a POST request that goes through a proxy. When the "
     "request goes to an ordinary file it seems the POST data "
     "is ignored."];
http_post(Config) when is_list(Config) ->
    Method = post,
    URL = url("/index.html", Config),
    Request = {URL,[],"text/plain","foobar"},
    HttpOpts = [],
    Opts = [],
    {ok,{{_,200,_},[_|_],[_|_]}} =
	httpc:request(Method, Request, HttpOpts, Opts),
    ok.

%%--------------------------------------------------------------------

http_put(doc) ->
    ["Perform a PUT request. The server will not allow it "
     "but we only test sending the request."];
http_put(Config) when is_list(Config) ->
    Method = put,
    URL = url("/put.html", Config),
    Content =
	"<html><body> <h1>foo</h1> <p>bar</p> </body></html>",
    Request = {URL,[],"html",Content},
    HttpOpts = [],
    Opts = [],
    {ok,{{_,405,_},[_|_],[_|_]}} =
	httpc:request(Method, Request, HttpOpts, Opts),
    ok.

%%--------------------------------------------------------------------

http_delete(doc) ->
    ["Perform a DELETE request that goes through a proxy. Note the server "
     "will reject the request with a 405 Method Not Allowed,"
     "but this is just a test of sending the request."];
http_delete(Config) when is_list(Config) ->
    Method = delete,
    URL = url("/delete.html", Config),
    Request = {URL,[]},
    HttpOpts = [],
    Opts = [],
    {ok,{{_,405,_},[_|_],[_|_]}} =
	httpc:request(Method, Request, HttpOpts, Opts),
    ok.

%%--------------------------------------------------------------------

http_delete_body(doc) ->
    ["Perform a DELETE request with a content body. The server will not allow it "
     "but we only test sending the request."];
http_delete_body(Config) when is_list(Config) ->
    Method = delete,
    URL = url("/delete.html", Config),
    Content = "foo=bar",
    Request = {URL,[],"application/x-www-form-urlencoded",Content},
    HttpOpts = [],
    Opts = [],
    {ok,{{_,405,_},[_|_],[_|_]}} =
    httpc:request(Method, Request, HttpOpts, Opts),
    ok.

%%--------------------------------------------------------------------

http_headers(doc) ->
    ["Use as many request headers as possible"];
http_headers(Config) when is_list(Config) ->
    Method = get,
    URL = url("/index.html", Config),
    Headers =
	[{"Accept",
	  "text/*, text/html, text/html;level=1, */*"},
	 {"Accept-Charset",
	  "iso-8859-5, unicode-1-1;q=0.8"},
	 {"Accept-Encoding", "*"},
	 {"Accept-Language",
	  "sv, en-gb;q=0.8, en;q=0.7"},
	 {"User-Agent", "inets"},
	 {"Max-Forwards","5"},
	 {"Referer",
	  "http://otp.ericsson.se:8000/product/internal"}],
    Request = {URL,Headers},
    HttpOpts = [],
    Opts = [],
    {ok,{{_,200,_},[_|_],[_|_]}} =
	httpc:request(Method, Request, HttpOpts, Opts),
    ok.

%%--------------------------------------------------------------------

http_proxy_auth(doc) ->
    ["Test the code for sending of proxy authorization."];
http_proxy_auth(Config) when is_list(Config) ->
    %% Our proxy seems to ignore the header, however our proxy
    %% does not requirer an auth header, but we want to know
    %% atleast the code for sending the header does not crash!
    Method = get,
    URL = url("/index.html", Config),
    Request = {URL,[]},
    HttpOpts = [{proxy_auth,{"foo","bar"}}],
    Opts = [],
    {ok,{{_,200,_},[_|_],[_|_]}} =
	httpc:request(Method, Request, HttpOpts, Opts),
    ok.

%%--------------------------------------------------------------------

http_doesnotexist(doc) ->
    ["Test that we get a 404 when the page is not found."];
http_doesnotexist(Config) when is_list(Config) ->
    Method = get,
    URL = url("/doesnotexist.html", Config),
    Request = {URL,[]},
    HttpOpts = [{proxy_auth,{"foo","bar"}}],
    Opts = [],
    {ok,{{_,404,_},[_|_],[_|_]}} =
	httpc:request(Method, Request, HttpOpts, Opts),
    ok.

%%--------------------------------------------------------------------

http_stream(doc) ->
    ["Test the option stream for asynchronous requests"];
http_stream(Config) when is_list(Config) ->
    Method = get,
    URL = url("/index.html", Config),
    Request = {URL,[]},
    HttpOpts = [],

    Opts1 = [{body_format,binary}],
    {ok,{{_,200,_},[_|_],Body}} =
	httpc:request(Method, Request, HttpOpts, Opts1),

    Opts2 = [{sync,false},{stream,self}],
    {ok,RequestId} =
	httpc:request(Method, Request, HttpOpts, Opts2),
    receive
	{http,{RequestId,stream_start,[_|_]}} ->
	    ok
    end,
    case http_stream(RequestId, <<>>) of
	Body -> ok
    end.
    %% StreamedBody = http_stream(RequestId, <<>>),
    %% Body =:= StreamedBody,
    %% ok.

http_stream(RequestId, Body) ->
    receive
	{http,{RequestId,stream,Bin}} ->
	    http_stream(RequestId, <<Body/binary,Bin/binary>>);
	{http,{RequestId,stream_end,_Headers}} ->
	    Body
    end.

%%--------------------------------------------------------------------

http_emulate_lower_versions(doc) ->
    ["Perform requests as 0.9 and 1.0 clients."];
http_emulate_lower_versions(Config) when is_list(Config) ->
    Method = get,
    URL = url("/index.html", Config),
    Request = {URL,[]},
    Opts = [],

    HttpOpts1 = [{version,"HTTP/0.9"}],
    {ok,[_|_]=B1} =
	httpc:request(Method, Request, HttpOpts1, Opts),
    inets_test_lib:check_body(B1),

    HttpOpts2 = [{version,"HTTP/1.0"}],
    {ok,{{_,200,_},[_|_],[_|_]=B2}} =
	httpc:request(Method, Request, HttpOpts2, Opts),
    inets_test_lib:check_body(B2),

    HttpOpts3 = [{version,"HTTP/1.1"}],
    {ok,{{_,200,_},[_|_],[_|_]=B3}} =
	httpc:request(Method, Request, HttpOpts3, Opts),
    inets_test_lib:check_body(B3),

    ok.

%%--------------------------------------------------------------------
http_not_modified_otp_6821(doc) ->
    ["If unmodified no body should be returned"];
http_not_modified_otp_6821(Config) when is_list(Config) ->
    Method = get,
    URL = url("/index.html", Config),
    Opts = [],

    Request1 = {URL,[]},
    HttpOpts1 = [],
    {ok,{{_,200,_},ReplyHeaders,[_|_]}} =
	 httpc:request(Method, Request1, HttpOpts1, Opts),
     ETag = header_value("etag", ReplyHeaders),
     LastModified = header_value("last-modified", ReplyHeaders),

     Request2 =
	 {URL,
	  [{"If-None-Match",ETag},
	   {"If-Modified-Since",LastModified}]},
     HttpOpts2 = [{timeout,15000}], % Limit wait for bug result
     {ok,{{_,304,_},_,[]}} = % Page Unchanged
	 httpc:request(Method, Request2, HttpOpts2, Opts),

     ok.

header_value(Name, [{HeaderName,HeaderValue}|Headers]) ->
    case string:to_lower(HeaderName) of
	Name ->
	    HeaderValue;
	_ ->
	    header_value(Name, Headers)
    end.

%%--------------------------------------------------------------------
https_connect_error(doc) ->
    ["Error from CONNECT tunnel should be returned"];
https_connect_error(Config) when is_list(Config) ->
    {HttpServer,HttpPort} = proplists:get_value(http, Config),
    Method = get,
    %% using HTTPS scheme with HTTP port to trigger connection error
    URL = "https://" ++ HttpServer ++ ":" ++
        integer_to_list(HttpPort) ++ "/index.html",
    Opts = [],
    HttpOpts = [],
    Request = {URL,[]},
    {error,{failed_connect,[_,{tls,_,_}]}} =
	httpc:request(Method, Request, HttpOpts, Opts).

%%--------------------------------------------------------------------
%% Internal Functions ------------------------------------------------
%%--------------------------------------------------------------------

init_apps([], Config) ->
    Config;
init_apps([App|Apps], Config) ->
    case app_start(App, Config) of
	ok ->
	    init_apps(Apps, Config);
	Error ->
	    Msg =
		lists:flatten(
		  io_lib:format(
		    "Could not start ~p due to ~p.~n",
		    [App, Error])),
	    {skip,Msg}
    end.

app_start(App, Config) ->
    try
	case App of
	    crypto ->
		crypto:stop(),
		ok = crypto:start();
	    inets ->
		application:stop(App),
		ok = application:start(App),
		case proplists:get_value(proxy, Config) of
		    undefined -> ok;
		    {_,ProxySpec} ->
			ok = httpc:set_options([{proxy,ProxySpec}])
		end;
	    _ ->
		application:stop(App),
		ok = application:start(App)
	end
    catch
	Class:Reason ->
	    {exception,Class,Reason}
    end.

app_stop(App) ->
    application:stop(App).

make_cert_files(Alg, Prefix, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    CaInfo = {CaCert,_} = erl_make_certs:make_cert([{key,Alg}]),
    {Cert,CertKey} = erl_make_certs:make_cert([{key,Alg},{issuer,CaInfo}]),
    CaCertFile = filename:join(PrivDir, Prefix++"cacerts.pem"),
    CertFile = filename:join(PrivDir, Prefix++"cert.pem"),
    KeyFile = filename:join(PrivDir, Prefix++"key.pem"),
    der_to_pem(CaCertFile, [{'Certificate', CaCert, not_encrypted}]),
    der_to_pem(CertFile, [{'Certificate', Cert, not_encrypted}]),
    der_to_pem(KeyFile, [CertKey]),
    ok.

der_to_pem(File, Entries) ->
    PemBin = public_key:pem_encode(Entries),
    file:write_file(File, PemBin).



url(AbsPath, Config) ->
    Protocol = proplists:get_value(protocol, Config),
    {ServerName,ServerPort} = proplists:get_value(Protocol, Config),
    atom_to_list(Protocol) ++ "://" ++
	ServerName ++ ":" ++ integer_to_list(ServerPort) ++
	AbsPath.

%%--------------------------------------------------------------------

init_local_proxy(Config) ->
    case os:type() of
	{unix,_} ->
	    case rcmd_local_proxy(["start"], Config) of
		{0,[":STARTED:"++String]} ->
		    init_local_proxy_string(String, Config);
		{_,[":SKIP:"++_|_]}=Reason ->
		    {skip,Reason};
		Error ->
		    rcmd_local_proxy(["stop"], Config),
		    ct:fail({local_proxy_start_failed,Error})
	    end;
	_ ->
	    {skip,"Platform can not run local proxy start script"}
    end.

init_local_proxy_string(String, Config) ->
    {Proxy,Server} = split($|, String),
    {ProxyName,ProxyPort} = split($:, Proxy),
    {ServerName,ServerPorts} = split($:, Server),
    {ServerHttpPort,ServerHttpsPort} = split($:, ServerPorts),
    [{proxy,{local,{{ProxyName,list_to_integer(ProxyPort)},[]}}},
     {http,{ServerName,list_to_integer(ServerHttpPort)}},
     {https,{ServerName,list_to_integer(ServerHttpsPort)}}
     |Config].

rcmd_local_proxy(Args, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Script = filename:join(DataDir, ?LOCAL_PROXY_SCRIPT),
    rcmd(Script, Args, [{cd,PrivDir}]).

rcmd(Cmd, Args, Opts) ->
    Port =
	erlang:open_port(
	  {spawn_executable,Cmd},
	  [{args,Args},{line,80},exit_status,eof,hide|Opts]),
    rcmd_loop(Port, [], [], undefined, false).

rcmd_loop(Port, Lines, Buf, Exit, EOF) ->
    receive
	{Port,{data,{Flag,Line}}} ->
	    case Flag of
		noeol ->
		    rcmd_loop(Port, Lines, r(Line, Buf), Exit, EOF);
		eol ->
		    rcmd_loop(Port, [r(Buf, Line)|Lines], [], Exit, EOF)
	    end;
	{Port,{exit_status,Status}} when Exit =:= undefined ->
	    case EOF of
		true ->
		    rcmd_close(Port, Lines, Buf, Status);
		false ->
		    rcmd_loop(Port, Lines, Buf, Status, EOF)
	    end;
	{Port,eof} when EOF =:= false ->
	    case Exit of
		undefined ->
		    rcmd_loop(Port, Lines, Buf, Exit, true);
		Status ->
		    rcmd_close(Port, Lines, Buf, Status)
	    end;
	{Port,_}=Unexpected ->
	    ct:fail({unexpected_from_port,Unexpected})
    end.

rcmd_close(Port, Lines, Buf, Status) ->
    catch port_close(Port),
    case Buf of
	[] ->
	    {Status,Lines};
	_ ->
	    {Status,[r(Buf)|Lines]}
    end.

%%--------------------------------------------------------------------

%% Split on first match of X in Ys, do not include X in neither part
split(X, Ys) ->
    split(X, Ys, []).
%%
split(X, [X|Ys], Rs) ->
    {r(Rs),Ys};
split(X, [Y|Ys], Rs) ->
    split(X, Ys, [Y|Rs]).

r(L) -> lists:reverse(L).
r(L, R) -> lists:reverse(L, R).
