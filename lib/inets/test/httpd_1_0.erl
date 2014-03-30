%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2013-2014. All Rights Reserved.
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

-module(httpd_1_0).

-export([host/4, trace/4]).

%%-------------------------------------------------------------------------
%% Test cases 
%%-------------------------------------------------------------------------
host(Type, Port, Host, Node) ->
    %% No host needed for HTTP/1.0
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "GET / HTTP/1.0\r\n\r\n", 
				       [{statuscode, 200},
					{version, "HTTP/1.0"}]).
trace(Type, Port, Host, Node)->
    ok = httpd_test_lib:verify_request(Type, Host, Port, Node, 
				       "TRACE / HTTP/1.0\r\n\r\n",
				       [{statuscode, 501}, 
					{version, "HTTP/1.0"}]).
