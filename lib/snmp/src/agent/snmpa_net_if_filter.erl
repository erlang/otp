%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
-module(snmpa_net_if_filter).

%% Behaviour
-export([accept_recv/2, accept_send/2, accept_recv_pdu/3, accept_send_pdu/2]).

-include("snmp_debug.hrl").

accept_recv(Domain, _Address) when is_atom(Domain) ->
    ?d("accept_recv -> entry with~n"
       "   Domain:  ~p~n"
       "   Address: ~p", [Domain, _Address]),
    true;
accept_recv(_Addr, Port) when is_integer(Port) ->
    ?d("accept_recv -> entry with~n"
       "   Addr: ~p~n"
       "   Port: ~p", [_Addr, Port]),
    true.

accept_send(Domain, _Address) when is_atom(Domain) ->
    ?d("accept_send -> entry with~n"
       "   Domain:  ~p~n"
       "   Address: ~p", [Domain, _Address]),
    true;
accept_send(_Addr, Port) when is_integer(Port) ->
    ?d("accept_send -> entry with~n"
       "   Addr: ~p~n"
       "   Port: ~p", [_Addr, Port]),
    true.

accept_recv_pdu(Domain, _Address, _PduType) when is_atom(Domain) ->
    ?d("accept_recv -> entry with~n"
       "   Domain:  ~p~n"
       "   Address: ~p~n"
       "   PduType: ~p", [Domain, _Address, _PduType]),
    true;
accept_recv_pdu(_Addr, Port, _PduType) when is_integer(Port) ->
    ?d("accept_recv_pdu -> entry with~n"
       "   Addr: ~p~n"
       "   Port: ~p~n"
       "   PduType: ~p", [_Addr, Port, _PduType]),
    true.

accept_send_pdu(_Targets, _PduType) ->
    ?d("accept_send_pdu -> entry with~n"
       "   Targets: ~p~n"
       "   PduType: ~p", [_Targets, _PduType]),
    true.

