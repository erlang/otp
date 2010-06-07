%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2010. All Rights Reserved.
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

-ifndef(http_internal_hrl).
-define(http_internal_hrl, true).

-include_lib("inets/src/inets_app/inets_internal.hrl").

-define(HTTP_MAX_BODY_SIZE,   nolimit).
-define(HTTP_MAX_HEADER_SIZE, 10240).
-define(HTTP_MAX_URI_SIZE,    nolimit).

-ifndef(HTTP_DEFAULT_SSL_KIND).
-define(HTTP_DEFAULT_SSL_KIND, ossl).
%% -define(HTTP_DEFAULT_SSL_KIND, essl).
-endif. % -ifdef(HTTP_DEFAULT_SSL_KIND).


%%% Response headers
-record(http_response_h,{
%%% --- Standard "General" headers
 	  'cache-control',
 	  connection,
 	  date,
 	  pragma,
 	  trailer,
 	  'transfer-encoding',
 	  upgrade,
 	  via,
 	  warning,
%%% --- Standard "Response" headers
 	  'accept-ranges',
 	  age,
 	  etag,
 	  location,
 	  'proxy-authenticate',
 	  'retry-after',
 	  server,
 	  vary,
 	  'www-authenticate',
%%% --- Standard "Entity" headers
 	  allow,
 	  'content-encoding',
 	  'content-language',
 	  'content-length' = "-1",
 	  'content-location',
	  'content-md5',
 	  'content-range',
 	  'content-type',
 	  expires,
 	  'last-modified',
	  other=[]        % list() - Key/Value list with other headers
	 }).


%%% Request headers
-record(http_request_h,{
%%% --- Standard "General" headers
 	  'cache-control',
 	  connection = "keep-alive",
 	  date,
 	  pragma,
 	  trailer,
 	  'transfer-encoding',
 	  upgrade,
 	  via,
 	  warning,
%%% --- Standard "Request" headers
 	  accept,
 	  'accept-charset',
 	  'accept-encoding',
 	  'accept-language',
 	  authorization,
 	  expect, 
 	  from,
 	  host,
 	  'if-match',
 	  'if-modified-since',
 	  'if-none-match',
 	  'if-range',
 	  'if-unmodified-since',
 	  'max-forwards',
	  'proxy-authorization', 
 	  range,
 	  referer,
 	  te, 
 	  'user-agent',
%%% --- Standard "Entity" headers
	  allow,
 	  'content-encoding',
 	  'content-language',
 	  'content-length' = "0",
	  'content-location',
 	  'content-md5',
 	  'content-range',
 	  'content-type',
	  expires,
 	  'last-modified',
	  other=[]        % list() - Key/Value list with other headers
	 }).

-endif. % -ifdef(http_internal_hrl).
