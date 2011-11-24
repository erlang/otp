%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
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

-ifndef(httpd_hrl).
-define(httpd_hrl, true).

-include_lib("kernel/include/file.hrl").

-record(init_data,{peername,resolve}).

-record(mod,{init_data,
	     data=[],
	     socket_type=ip_comm,
	     socket,
	     config_db,
	     method,
	     absolute_uri=[],
	     request_uri,
	     http_version,
	     request_line,
	     parsed_header=[],
	     entity_body,
	     connection}).
-endif. % -ifdef(httpd_hrl).
