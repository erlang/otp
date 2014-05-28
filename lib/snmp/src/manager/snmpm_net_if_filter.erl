%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2014. All Rights Reserved.
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
-module(snmpm_net_if_filter).

-export([accept_recv/2, 
	 accept_send/2,
	 accept_recv_pdu/3,
	 accept_send_pdu/3]).

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
    ?d("accept_recv_pdu -> entry with~n"
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

accept_send_pdu(Domain, _Address, _PduType) when is_atom(Domain) ->
    ?d("accept_send_pdu -> entry with~n"
       "   Domain:  ~p~n"
       "   Address: ~p~n"
       "   PduType: ~p", [Domain, _Address, _PduType]),
    true;
accept_send_pdu(_Addr, Port, _PduType) when is_integer(Port) ->
    ?d("accept_send_pdu -> entry with~n"
       "   Addr:    ~p~n"
       "   Port:    ~p~n"
       "   PduType: ~p", [_Addr, Port, _PduType]),
    true.

