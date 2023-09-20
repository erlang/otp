%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2022-2023. All Rights Reserved.
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

-module(tls_client_ticket_store_SUITE).
-behaviour(ct_suite).

-include("ssl_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("ssl/src/ssl_internal.hrl").
-include_lib("ssl/src/tls_handshake_1_3.hrl").

%% Callback functions
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

%% Testcases
-export([ticket_obfuscated_age/0,
         ticket_obfuscated_age/1,
         ticket_expired/0,
         ticket_expired/1]).

-define(TICKET_STORE_SIZE, 2).
-define(LIFETIME, 2). % tickets expire after 2 second

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() ->
    [ticket_obfuscated_age, ticket_expired].

init_per_testcase(_TestCase, Config)  ->
    {ok, Pid} = tls_client_ticket_store:start_link(
                  ?TICKET_STORE_SIZE, ?LIFETIME),
    [{server_pid, Pid} | Config].

end_per_testcase(_TestCase, Config) ->
    Pid = ?config(server_pid, Config),
    exit(Pid, normal),
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
ticket_obfuscated_age() ->
    [{doc, "Verify the ticket store computes the obfuscated ticket age correctly"}].
ticket_obfuscated_age(_Config) ->
    TicketAgeAdd = 2900512354,
    Ticket =
        #new_session_ticket{
           ticket_lifetime = ?LIFETIME,
           ticket_age_add = TicketAgeAdd,
           ticket_nonce = <<0,0,0,0,0,0,0,1>>,
           ticket = <<1, 1, 1, 1, 1, 1, 1, 1>>,
           extensions = #{early_data => {early_data_indication_nst, 16384}}
        },
    CipherSuite = {Cipher, HashAlgo} = {aes_256_gcm, sha384},
    SNI = "some-test-sni",
    PSK = <<10, 10, 10, 10>>,

    ok = tls_client_ticket_store:store_ticket(Ticket, CipherSuite, SNI, PSK),

    timer:sleep(100),

    {Key, _} = tls_client_ticket_store:find_ticket(self(), [Cipher], [HashAlgo], SNI, undefined),
    [#ticket_data{identity = Identity}] = tls_client_ticket_store:get_tickets(self(), [Key]),
    #psk_identity{obfuscated_ticket_age = ObfAge} = Identity,
    Age = ObfAge - TicketAgeAdd,
    ?CT_LOG("Ticket age: ~p (obfuscated age: ~p, ticket age add: ~p)~n",
           [Age, ObfAge, TicketAgeAdd]),
    true = Age < 1000 andalso Age >= 100.

ticket_expired() ->
    [{doc, "Verify the ticket store does not return an expired ticket"}].
ticket_expired(_Config) ->
    TicketAgeAdd = 1234563451,
    Ticket =
        #new_session_ticket{
           ticket_lifetime = ?LIFETIME,
           ticket_age_add = TicketAgeAdd,
           ticket_nonce = <<0,0,0,0,0,0,0,2>>,
           ticket = <<2, 2, 2, 2, 2, 2, 2, 2>>,
           extensions = #{early_data => {early_data_indication_nst, 16384}}
        },
    CipherSuite = {Cipher, HashAlgo} = {aes_256_gcm, sha384},
    SNI = "some-test-sni",
    PSK = <<20, 20, 20, 20>>,

    ok = tls_client_ticket_store:store_ticket(Ticket, CipherSuite, SNI, PSK),

    timer:sleep(?LIFETIME * 1000 + 500),

    {undefined, undefined} = tls_client_ticket_store:find_ticket(
                               self(), [Cipher], [HashAlgo], SNI, undefined).
