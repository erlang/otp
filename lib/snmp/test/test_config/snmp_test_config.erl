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
