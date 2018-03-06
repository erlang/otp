%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2015-2018. All Rights Reserved.
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
-module(http_test_lib).

-include_lib("common_test/include/ct.hrl").
-include("inets_test_lib.hrl").
-include("http_internal.hrl").
-include("httpc_internal.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

dummy_server(SocketType, Inet, Extra) ->
    dummy_server(self(), SocketType, Inet, Extra).

dummy_server(Caller, SocketType, Inet, Extra) ->
    Args = [Caller, SocketType, Inet, Extra],
    Pid = spawn(?MODULE, dummy_server_init, Args),
    receive
	{port, Port} ->
	    {Pid, Port}
    end.

dummy_server_init(Caller, ip_comm, Inet, Extra) ->
    ContentCb = proplists:get_value(content_cb, Extra),
    BaseOpts = [binary, {packet, 0}, {reuseaddr,true}, {active, false}, {nodelay, true}], 
    Conf = proplists:get_value(conf, Extra),
    {ok, ListenSocket} = gen_tcp:listen(0, [Inet | BaseOpts]),
    {ok, Port} = inet:port(ListenSocket),
    Caller ! {port, Port},
    dummy_ipcomm_server_loop({httpd_request, parse, [[{max_uri,    ?HTTP_MAX_URI_SIZE},
						      {max_header, ?HTTP_MAX_HEADER_SIZE},
						      {max_version,?HTTP_MAX_VERSION_STRING}, 
						      {max_method, ?HTTP_MAX_METHOD_STRING},
						      {max_content_length, ?HTTP_MAX_CONTENT_LENGTH},
						      {customize, httpd_custom}
						     ]]},
			     [], ContentCb, Conf, ListenSocket);

dummy_server_init(Caller, unix_socket, Inet, Extra) ->
    ContentCb = proplists:get_value(content_cb, Extra),
    UnixSocket = proplists:get_value(unix_socket, Extra),
    SocketAddr = {local, UnixSocket},
    BaseOpts = [binary, {packet, 0}, {reuseaddr,true}, {active, false}, {nodelay, true},
               {ifaddr, SocketAddr}],
    Conf = proplists:get_value(conf, Extra),
    {ok, ListenSocket} = gen_tcp:listen(0, [Inet | BaseOpts]),
    {ok, Port} = inet:port(ListenSocket),
    Caller ! {port, Port},
    dummy_ipcomm_server_loop({httpd_request, parse, [[{max_uri,    ?HTTP_MAX_URI_SIZE},
						      {max_header, ?HTTP_MAX_HEADER_SIZE},
						      {max_version,?HTTP_MAX_VERSION_STRING},
						      {max_method, ?HTTP_MAX_METHOD_STRING},
						      {max_content_length, ?HTTP_MAX_CONTENT_LENGTH},
						      {customize, httpd_custom}
						     ]]},
			     [], ContentCb, Conf, ListenSocket);

dummy_server_init(Caller, ssl, Inet, Extra) ->
    ContentCb = proplists:get_value(content_cb, Extra),
    SSLOptions = proplists:get_value(ssl, Extra),
    Conf = proplists:get_value(conf, Extra),
    BaseOpts = [binary, {reuseaddr,true}, {active, false}, {nodelay, true} |
	        SSLOptions], 
    dummy_ssl_server_init(Caller, BaseOpts, Inet, ContentCb, Conf).

dummy_ssl_server_init(Caller, BaseOpts, Inet, ContentCb, Conf) ->
    {ok, ListenSocket} = ssl:listen(0, [Inet | BaseOpts]),
    {ok, {_, Port}} = ssl:sockname(ListenSocket),
    Caller ! {port, Port},
    dummy_ssl_server_loop({httpd_request, parse, [[{max_uri,    ?HTTP_MAX_URI_SIZE},
						   {max_method, ?HTTP_MAX_METHOD_STRING},
						   {max_version,?HTTP_MAX_VERSION_STRING}, 
						   {max_method, ?HTTP_MAX_METHOD_STRING},
						   {max_content_length, ?HTTP_MAX_CONTENT_LENGTH},
						   {customize, httpd_custom}
						  ]]},
			  [], ContentCb, Conf, ListenSocket).

dummy_ipcomm_server_loop(MFA, Handlers, ContentCb, Conf, ListenSocket) ->
    receive
	stop ->
	    lists:foreach(fun(Handler) -> Handler ! stop end, Handlers);
	{stop, From} ->
	    Stopper = fun(Handler) -> Handler ! stop end, 
	    lists:foreach(Stopper, Handlers),
	    From ! {stopped, self()}
    after 0 ->
	    {ok, Socket} = gen_tcp:accept(ListenSocket),
	    HandlerPid  = dummy_request_handler(MFA, Socket, ContentCb, Conf),
	    gen_tcp:controlling_process(Socket, HandlerPid),
	    HandlerPid ! ipcomm_controller,
	    dummy_ipcomm_server_loop(MFA, [HandlerPid | Handlers],
				     ContentCb, Conf, ListenSocket)
    end.

dummy_ssl_server_loop(MFA, Handlers, ContentCb, Conf, ListenSocket) ->
    receive
	stop ->
	    lists:foreach(fun(Handler) -> Handler ! stop end, Handlers);
	{stop, From} ->
	    Stopper = fun(Handler) -> Handler ! stop end, 
	    lists:foreach(Stopper, Handlers),
	    From ! {stopped, self()}
    after 0 ->
	    {ok, Socket} = ssl:transport_accept(ListenSocket),
	    HandlerPid  = dummy_request_handler(MFA, Socket, ContentCb, Conf),
	    ssl:controlling_process(Socket, HandlerPid),
	    HandlerPid ! ssl_controller,
	    dummy_ssl_server_loop(MFA, [HandlerPid | Handlers],
				  ContentCb, Conf, ListenSocket)
    end.

dummy_request_handler(MFA, Socket, ContentCb, Conf) ->
    spawn(?MODULE, dummy_request_handler_init, [MFA, Socket, ContentCb, Conf]).

dummy_request_handler_init(MFA, Socket, ContentCb, Conf) ->
    SockType = 
	receive 
	    ipcomm_controller ->
		inet:setopts(Socket, [{active, true}]),
		ip_comm;
	    ssl_controller ->
		ok = ssl:ssl_accept(Socket, infinity),
		ssl:setopts(Socket, [{active, true}]),
		ssl
	end,
    dummy_request_handler_loop(MFA, SockType, Socket, ContentCb, Conf).
    
dummy_request_handler_loop({Module, Function, Args}, SockType, Socket, ContentCb, Conf) ->
    receive 
	{Proto, _, Data} when (Proto =:= tcp) orelse (Proto =:= ssl) ->
	    case handle_request(Module, Function, [Data | Args], Socket, ContentCb, Conf) of
		stop when Proto =:= tcp ->
		    gen_tcp:close(Socket);
		stop when Proto =:= ssl ->
		    ssl:close(Socket);
		NewMFA ->
		    dummy_request_handler_loop(NewMFA, SockType, Socket, ContentCb, Conf)
	    end;
	stop when SockType =:= ip_comm ->
	    gen_tcp:close(Socket);
	stop when SockType =:= ssl ->
	    ssl:close(Socket)
    end.

handle_request(Module, Function, Args, Socket, ContentCb, Conf) ->
    case Module:Function(Args) of
	{ok, Result} ->
	    case ContentCb:handle_http_msg(Result, Socket, Conf) of
		stop ->
		    stop;
		<<>> ->
		    {httpd_request, parse, [[{max_uri,?HTTP_MAX_URI_SIZE},
					     {max_header, ?HTTP_MAX_HEADER_SIZE},
					     {max_version,?HTTP_MAX_VERSION_STRING}, 
					     {max_method, ?HTTP_MAX_METHOD_STRING},
					     {max_content_length, ?HTTP_MAX_CONTENT_LENGTH},
					     {customize, httpd_custom}
					    ]]};
		Data ->	
		    handle_request(httpd_request, parse, 
				   [Data, [{max_uri,    ?HTTP_MAX_URI_SIZE},
					   {max_header, ?HTTP_MAX_HEADER_SIZE},
					   {max_version,?HTTP_MAX_VERSION_STRING}, 
					   {max_method, ?HTTP_MAX_METHOD_STRING},
					   {max_content_length, ?HTTP_MAX_CONTENT_LENGTH},
					   {customize, httpd_custom}
					  ]], Socket, ContentCb, Conf)
	    end;
	NewMFA ->
	    NewMFA
    end.

%% Perform a synchronous stop
dummy_server_stop(Pid) ->
    Pid ! {stop, self()},
    receive 
	{stopped, Pid} ->
	    ok
    end.
