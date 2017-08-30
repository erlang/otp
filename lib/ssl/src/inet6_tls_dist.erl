%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015. All Rights Reserved.
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
-module(inet6_tls_dist).

-export([childspecs/0, listen/1, accept/1, accept_connection/5,
	 setup/5, close/1, select/1]).

childspecs() ->
    inet_tls_dist:childspecs().

select(Node) ->
    inet_tls_dist:gen_select(inet6_tcp, Node).

listen(Name) ->
    inet_tls_dist:gen_listen(inet6_tcp, Name).

accept(Listen) ->
    inet_tls_dist:gen_accept(inet6_tcp, Listen).

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    inet_tls_dist:gen_accept_connection(inet6_tcp, AcceptPid, Socket, MyNode, Allowed, SetupTime).

setup(Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    inet_tls_dist:gen_setup(inet6_tcp, Node, Type, MyNode, LongOrShortNames,SetupTime).

close(Socket) ->
    inet_tls_dist:close(Socket).
