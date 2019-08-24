%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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
