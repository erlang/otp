%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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

-module(snmp_test_config).

-export([ip_address/0, ip_address2/0]).

ip_address() ->
    {ok, Hostname} = inet:gethostname(),
    {ok, Address}  = inet:getaddr(Hostname, inet),
    io:format("~w", [tuple_to_list(Address)]).

ip_address2() ->
    {ok, Hostname} = inet:gethostname(),
    {ok, {A1, A2, A3, A4}}  = inet:getaddr(Hostname, inet),
    io:format("~w.~w.~w.~w", [A1, A2, A3, A4]).
