%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Mobile Arts AB
%% Portions created by Mobile Arts are Copyright 2002, Mobile Arts AB
%% All Rights Reserved.''
%%
%%

-define(HTTP_REQUEST_TIMEOUT,   5000).
-define(PIPELINE_LENGTH,3).
-define(OPEN_SESSIONS,400).


%%% FIXME! These definitions should probably be possible to defined via
%%% user settings
-define(MAX_REDIRECTS, 4).


%%% Note that if not persitent the connection can be closed immediately on a
%%% response, because new requests are not sent to this connection process.
%%%	  address,     % ({Host,Port}) Destination Host and Port
-record(session,{
	  id,          % (int) Session Id identifies session in http_manager
	  clientclose, % (bool) true if client requested "close" connection
	  scheme,      % (atom) http (HTTP/TCP) or https (TCP/SSL/TCP)
	  socket,      % (socket) Open socket, used by connection
	  pipeline=[], % (list) Sent requests, not yet taken care of by the
	               %        associated http_responder.
	  quelength=1, % (int) Current length of pipeline (1 when created)
	  max_quelength% (int) Max pipeline length
	 }).

%%%      [{Pid,RequestQue,QueLength},...] list where
%%%   - RequestQue (implemented with a list) contains sent requests that
%%%      has not yet received a response (pipelined) AND is not currently
%%%      handled (awaiting data) by the session process.
%%%   - QueLength is the length of this que, but

%%% Response headers
-record(res_headers,{
%%% --- Standard "General" headers
% 	  cache_control,
	  connection,
% 	  date,
% 	  pragma,
% 	  trailer,
	  transfer_encoding,
% 	  upgrade,
% 	  via,
% 	  warning,
%%% --- Standard "Request" headers
% 	  accept_ranges,
% 	  age,
% 	  etag,
	  location,
% 	  proxy_authenticate,
	  retry_after,
% 	  server,
% 	  vary,
% 	  www_authenticate,
%%% --- Standard "Entity" headers
% 	  allow,
% 	  content_encoding,
% 	  content_language,
	  content_length="0",
% 	  content_location,
% 	  content_md5,
% 	  content_range,
	  content_type,
% 	  expires,
% 	  last_modified,
	  other=[]        % (list) Key/Value list with other headers
	 }).

%%% All data associated to a specific HTTP request
-record(request,{
	  id,          % (int) Request Id
	  ref,         % Caller specific
	  from,        % (pid) Caller
	  redircount=0,% (int) Number of redirects made for this request
	  scheme,      % (http|https) (HTTP/TCP) or (TCP/SSL/TCP) connection
	  address,     % ({Host,Port}) Destination Host and Port
	  pathquery,   % (string) Rest of parsed URL
	  method,      % (atom) HTTP request Method
	  headers,     % (list) Key/Value list with Headers
	  content,     % ({ContentType,Body}) Current HTTP request
	  settings     % (#client_settings{}) User defined settings
	 }).

-record(response,{
	  scheme,      % (atom) http (HTTP/TCP) or https (TCP/SSL/TCP)
	  socket,      % (socket) Open socket, used by connection
	  status,
	  http_version,
	  headers=#res_headers{},
	  body = <<>>
	 }).




%%% HTTP Client settings
-record(client_settings,{
	  timeout=?HTTP_REQUEST_TIMEOUT,
	                     % (int) Milliseconds before a request times out
	  useproxy=false,    % (bool) True if the proxy should be used
	  proxy=undefined,   % (tuple) Parsed Proxy URL
	  noproxylist=[],    % (list) List with hosts not requiring proxy
	  autoredirect=true, % (bool) True if automatic redirection on 30X
			     %        responses.
	  max_sessions=?OPEN_SESSIONS,% (int) Max open sessions for any Adr,Port
	  max_quelength=?PIPELINE_LENGTH, % (int) Max pipeline length
%	  ssl=[{certfile,"/jb/server_root/ssl/ssl_client.pem"},
%	       {keyfile,"/jb/server_root/ssl/ssl_client.pem"},
%	       {verify,0}]
	  ssl=false    % (list) SSL settings. A non-empty list enables SSL/TLS
                       %  support in the HTTP client
	 }).
