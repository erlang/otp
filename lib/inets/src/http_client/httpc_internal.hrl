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

-ifndef(httpc_internal_hrl).
-define(httpc_internal_hrl, true).

-include_lib("inets/src/inets_app/inets_internal.hrl").

-define(SERVICE, httpc).
-define(hcri(Label, Data), ?report_important(Label, ?SERVICE, Data)).
-define(hcrv(Label, Data), ?report_verbose(Label,   ?SERVICE, Data)).
-define(hcrd(Label, Data), ?report_debug(Label,     ?SERVICE, Data)).
-define(hcrt(Label, Data), ?report_trace(Label,     ?SERVICE, Data)).

-define(HTTP_REQUEST_TIMEOUT,    infinity).
-define(HTTP_REQUEST_CTIMEOUT,   ?HTTP_REQUEST_TIMEOUT).
-define(HTTP_PIPELINE_TIMEOUT,   0).
-define(HTTP_PIPELINE_LENGTH,    2).
-define(HTTP_MAX_TCP_SESSIONS,   2).
-define(HTTP_MAX_REDIRECTS,      4).
-define(HTTP_KEEP_ALIVE_TIMEOUT, 120000).
-define(HTTP_KEEP_ALIVE_LENGTH,  5).
-define(TLS_UPGRADE_TOKEN, "TLS/1.0").

%%% HTTP Client per request settings
-record(http_options,
	{
	  %% string() - "HTTP/1.1" | "HTTP/1.0" | "HTTP/0.9"
	  version, 

	  %% integer() | infinity - ms before a request times out
	  timeout = ?HTTP_REQUEST_TIMEOUT,  

	  %% bool() - true if auto redirect on 30x response
	  autoredirect = true, 

	  %% ssl socket options
	  ssl = [], 

	  %% {User, Password} = {string(), string()} 
	  proxy_auth, 

	  %% bool() - true if not strictly std compliant
	  relaxed = false, 

	  %% integer() - ms before a connect times out
	  connect_timeout = ?HTTP_REQUEST_CTIMEOUT,

	  %% bool() - Use %-encoding rfc 2396
	  url_encode

	 }
       ).

%%% HTTP Client per profile setting. 
-record(options, 
	{
	 proxy = {undefined, []}, % {{ProxyHost, ProxyPort}, [NoProxy]},
	 https_proxy = {undefined, []}, % {{ProxyHost, ProxyPort}, [NoProxy]}
	 %% 0 means persistent connections are used without pipelining
	 pipeline_timeout      = ?HTTP_PIPELINE_TIMEOUT, 
	 max_pipeline_length   = ?HTTP_PIPELINE_LENGTH,
	 max_keep_alive_length = ?HTTP_KEEP_ALIVE_LENGTH,
	 keep_alive_timeout    = ?HTTP_KEEP_ALIVE_TIMEOUT, % Used when pipeline_timeout = 0
	 max_sessions          = ?HTTP_MAX_TCP_SESSIONS,
	 cookies               = disabled, % enabled | disabled | verify
	 verbose               = false,
	 ipfamily              = inet,    % inet | inet6 | inet6fb4
	 ip                    = default, % specify local interface
	 port                  = default, % specify local port
	 socket_opts           = []       % other socket options
	}
       ).

%%% All data associated to a specific HTTP request
-record(request,
	{
	  id,            % ref() - Request Id
	  from,          % pid() - Caller
	  redircount = 0,% Number of redirects made for this request
	  scheme,        % http | https 
	  address,       % ({Host,Port}) Destination Host and Port
	  path,          % string() - Path of parsed URL
	  pquery,        % string() - Rest of parsed URL
	  method,        % atom() - HTTP request Method
	  headers,       % #http_request_h{}
	  content,       % {ContentType, Body} - Current HTTP request
	  settings,      % #http_options{} - User defined settings
	  abs_uri,       % string() ex: "http://www.erlang.org"
	  userinfo,      % string() - optinal "<userinfo>@<host>:<port>"
	  stream,	 % boolean() - stream async reply?
	  headers_as_is, % boolean() - workaround for servers that does
			 % not honor the http standard, can also be used 
			 % for testing purposes.
	  started,       % integer() > 0 - When we started processing the 
			 % request
	  timer,         % undefined | ref()
	  socket_opts,   % undefined | [socket_option()]
	  ipv6_host_with_brackets % boolean()
	}
       ).               


-record(session,
	{
	  %% {{Host, Port}, HandlerPid}
	  id, 

	  %% true | false
	  client_close, 

	  %% http (HTTP/TCP) | https (HTTP/SSL/TCP)
	  scheme, 

	  %% Open socket, used by connection
	  socket, 
	  
	  %% socket-type, used by connection
	  socket_type,

	  %% Current length of pipeline or keep-alive queue  
	  queue_length = 1, 

	  %% pipeline | keep_alive (wait for response before sending new request) 
	  type, 

	  %% true | false
	  %% This will be true, when a response has been received for 
	  %% the first request. See type above.
	  available = false
	 }).


-record(http_cookie,
	{
	  domain,
	  domain_default = false,
	  name,
	  value,
	  comment,
	  max_age = session,
	  path, 
	  path_default = false,
	  secure = false,
	  version = "0" 
	 }).


%% -record(parsed_uri, 
%% 	{
%% 	  scheme, % http | https
%% 	  uinfo,  % string()
%% 	  host,   % string()
%% 	  port,   % integer()
%% 	  path,   % string()
%% 	  q       % query: string()
%% 	 }).


-endif. % -ifdef(httpc_internal_hrl).
