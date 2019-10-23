%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2019-2019. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Handles TLS session ticket introduced in TLS-1.3
%%----------------------------------------------------------------------

-module(tls_session_ticket).

-include("tls_handshake_1_3.hrl").

-export([get_ticket_data/2, 
         store_session_ticket/4,
         update_ticket_pos/2]).

update_ticket_pos(Key, Pos) ->
    ets:update_element(tls13_session_ticket_db, Key, {2, Pos}).


store_session_ticket(NewSessionTicket, HKDF, SNI, PSK) ->
    _TicketDb =
        case ets:whereis(tls13_session_ticket_db) of
            undefined ->
                ets:new(tls13_session_ticket_db, [public, named_table, ordered_set]);
            Tid ->
                Tid
        end,
    Id = make_ticket_id(NewSessionTicket),
    Timestamp = erlang:system_time(seconds),
    ets:insert(tls13_session_ticket_db, {Id, undefined, HKDF, SNI, PSK, Timestamp, NewSessionTicket}).


make_ticket_id(NewSessionTicket) ->
    {_, B} = tls_handshake_1_3:encode_handshake(NewSessionTicket),
    crypto:hash(sha256, B).


get_ticket_data(undefined, _) ->
    undefined;
get_ticket_data(_, undefined) ->
    undefined;
get_ticket_data(_, UseTicket) ->
    fetch_data(UseTicket, []).


fetch_data([], []) ->
    undefined; %% No tickets found
fetch_data([], Acc) ->
    Acc;
fetch_data([TicketId|T], Acc) ->
    case ets:lookup(tls13_session_ticket_db, TicketId) of
        [{Key, Pos, HKDF, _SNI, PSK, Timestamp, NewSessionTicket}] ->
            #new_session_ticket{
               ticket_lifetime = _LifeTime,
               ticket_age_add = AgeAdd,
               ticket_nonce = Nonce,
               ticket = Ticket,
               extensions = _Extensions
              } = NewSessionTicket,

            TicketAge =  erlang:system_time(seconds) - Timestamp,
            ObfuscatedTicketAge = obfuscate_ticket_age(TicketAge, AgeAdd),
            Identity = #psk_identity{
                            identity = Ticket,
                            obfuscated_ticket_age = ObfuscatedTicketAge},
            fetch_data(T, [{Key, Pos, Identity, PSK, Nonce, HKDF}|Acc]);
        [] ->
            fetch_data(T, Acc)
    end.


%% The "obfuscated_ticket_age"
%% field of each PskIdentity contains an obfuscated version of the
%% ticket age formed by taking the age in milliseconds and adding the
%% "ticket_age_add" value that was included with the ticket
%% (see Section 4.6.1), modulo 2^32.
obfuscate_ticket_age(TicketAge, AgeAdd) ->
    (TicketAge + AgeAdd) rem round(math:pow(2,32)).
