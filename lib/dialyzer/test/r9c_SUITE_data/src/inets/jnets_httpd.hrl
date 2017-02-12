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

-include_lib("kernel/include/file.hrl").

-define(SOCKET_CHUNK_SIZE,8192).
-define(SOCKET_MAX_POLL,25).
-define(FILE_CHUNK_SIZE,64*1024).
-define(NICE(Reason),lists:flatten(atom_to_list(?MODULE)++": "++Reason)).
-define(DEFAULT_CONTEXT,
	[{errmsg,"[an error occurred while processing this directive]"},
	 {timefmt,"%A, %d-%b-%y %T %Z"},
	 {sizefmt,"abbrev"}]).


-ifdef(inets_debug).
-define(DEBUG(Format, Args), io:format("D(~p:~p:~p) : "++Format++"~n",
				       [self(),?MODULE,?LINE]++Args)).
-else.
-define(DEBUG(F,A),[]).
-endif.

-define(MAXBODYSIZE,16#ffffffff).

-define(HTTP_VERSION_10,0).
-define(HTTP_VERSION_11,1).

-define(CR,13).
-define(LF,10).


-record(init_data,{peername,resolve}).


-record(mod,{
	  init_data,           %
	  data= [],            % list() Used to propagate data between modules
	  socket_type=ip_comm, % socket_type() IP or SSL socket
	  socket,              % socket() Actual socket
	  config_db,           % ets() {key,val} db with config entries
	  method,              % atom() HTTP method, e.g. 'GET'
%	  request_uri,         % string() Request URI
	  path,                % string() Absolute path. May include query etc
	  http_version,        % int() HTTP minor version number, e.g. 0 or 1
%	  request_line,        % string() Request Line
	  headers,             % #req_headers{} Parsed request headers
	  entity_body= <<>>,   % binary() Body of request
	  connection,          % boolean() true if persistent connection
	  status_code,         % int() Status code
	  logging              % int() 0=No logging
	                       %       1=Only mod_log present
	                       %       2=Only mod_disk_log present
	                       %       3=Both mod_log and mod_disk_log present
	 }).

% -record(ssl,{
% 	  certfile,      %
% 	  keyfile,       %
% 	  verify= 0,     %
% 	  ciphers,       %
% 	  password,      %
% 	  depth = 1,     %
% 	  cacertfile,    %

% 	  cachetimeout   % Found in yaws....
% 	 }).


-record(http_request,{
	  method,  % atom() if known else string() HTTP methd
	  path,    % {abs_path,string()} URL path
	  version  % {int(),int()}  {Major,Minor} HTTP version
	 }).

-record(http_response,{
	  version, % {int(),int()}  {Major,Minor} HTTP version
	  status,  % int() Status code
	  phrase   % string() HTTP Reason phrase
	 }).


%%% Request headers
-record(req_headers,{
%%% --- Standard "General" headers
% 	  cache_control,
	  connection="keep-alive",
% 	  date,
% 	  pragma,
% 	  trailer,
	  transfer_encoding,
% 	  upgrade,
% 	  via,
% 	  warning,
%%% --- Standard "Request" headers
% 	  accept,
% 	  accept_charset,
% 	  accept_encoding,
% 	  accept_language,
	  authorization,
	  expect, %% FIXME! Update inet_drv.c!!
% 	  from,
	  host,
	  if_match,
	  if_modified_since,
	  if_none_match,
	  if_range,
	  if_unmodified_since,
% 	  max_forwards,
% 	  proxy_authorization,
	  range,
% 	  referer,
% 	  te, %% FIXME! Update inet_drv.c!!
	  user_agent,
%%% --- Standard "Entity" headers
% 	  content_encoding,
% 	  content_language,
	  content_length="0",
% 	  content_location,
% 	  content_md5,
% 	  content_range,
	  content_type,
% 	  last_modified,
	  other=[]        % (list) Key/Value list with other headers
	 }).
