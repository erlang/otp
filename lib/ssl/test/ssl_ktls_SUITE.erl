%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2026 Zeyu Zhang <zzy___1@sina.com>
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

%% TLS 1.3 connections handed over to kernel TLS with {ktls, true}
-module(ssl_ktls_SUITE).

-behaviour(ct_suite).

-include_lib("common_test/include/ct.hrl").
-include_lib("ssl/src/ssl_api.hrl").

%% Callback functions
-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Testcases
-export([handover/0, handover/1,
         large_data/0, large_data/1,
         active_once/0, active_once/1,
         active_n/0, active_n/1,
         packet/0, packet/1,
         options/0, options/1,
         read_ahead/0, read_ahead/1,
         connection_information/0, connection_information/1,
         controlling_process/0, controlling_process/1,
         update_keys_write/0, update_keys_write/1,
         update_keys_read_write/0, update_keys_read_write/1,
         peer_key_update_active/0, peer_key_update_active/1,
         repeated_key_updates/0, repeated_key_updates/1,
         key_updates_under_load/0, key_updates_under_load/1,
         concurrent_key_updates/0, concurrent_key_updates/1,
         key_update_full_send_buffer/0, key_update_full_send_buffer/1,
         key_update_two_readers/0, key_update_two_readers/1,
         recv_other_process/0, recv_other_process/1,
         session_tickets/0, session_tickets/1,
         close_notify/0, close_notify/1,
         peer_close_active/0, peer_close_active/1,
         aes_128_gcm/0, aes_128_gcm/1,
         aes_128_ccm/0, aes_128_ccm/1,
         max_fragment_length/0, max_fragment_length/1,
         unexpected_record/0, unexpected_record/1,
         unsupported_calls/0, unsupported_calls/1,
         split_handshake_message/0, split_handshake_message/1,
         key_update_with_trailing_data/0, key_update_with_trailing_data/1,
         failed_key_update/0, failed_key_update/1,
         key_update_reply_owed/0, key_update_reply_owed/1,
         close_notify_after_queued_output/0, close_notify_after_queued_output/1,
         controlling_process_pending_control/0,
         controlling_process_pending_control/1,
         packet_http/0, packet_http/1,
         packet_types_at_handover/0, packet_types_at_handover/1,
         early_data/0, early_data/1,
         data_with_finished/0, data_with_finished/1,
         key_update_overtakes_queued_output/0,
         key_update_overtakes_queued_output/1,
         recv_timeout_across_control/0, recv_timeout_across_control/1,
         key_install_waits_for_queued_output/0, key_install_waits_for_queued_output/1,
         chacha20_poly1305/0, chacha20_poly1305/1,
         tls_1_2/0, tls_1_2/1]).

-define(TIMEOUT, 10000).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [{group, inet}, {group, socket}].

groups() ->
    [{inet, [], test_cases()},
     {socket, [], test_cases()}].

test_cases() ->
    [handover, large_data, active_once, active_n, packet, options, read_ahead,
     connection_information, controlling_process, update_keys_write,
     update_keys_read_write, peer_key_update_active, repeated_key_updates,
     key_updates_under_load, concurrent_key_updates,
     key_update_full_send_buffer, key_update_two_readers, recv_other_process,
     session_tickets, close_notify, peer_close_active, aes_128_gcm,
     aes_128_ccm, chacha20_poly1305, tls_1_2, max_fragment_length,
     unexpected_record, unsupported_calls, split_handshake_message,
     key_update_with_trailing_data, key_install_waits_for_queued_output,
     failed_key_update, recv_timeout_across_control,
     controlling_process_pending_control, packet_http,
     packet_types_at_handover, early_data, data_with_finished,
     key_update_overtakes_queued_output, key_update_reply_owed,
     close_notify_after_queued_output].

%% Test cases that need a kernel that can replace a kTLS key
key_update_cases() ->
    [update_keys_write, update_keys_read_write, peer_key_update_active,
     repeated_key_updates, key_updates_under_load, concurrent_key_updates,
     key_update_full_send_buffer, key_update_two_readers, recv_other_process,
     failed_key_update, recv_timeout_across_control,
     controlling_process_pending_control, key_update_overtakes_queued_output,
     key_update_reply_owed].

init_per_suite(Config) ->
    case ssl_test_lib:ktls_os() of
        {ok, OS} ->
            case application:ensure_started(crypto) of
                ok ->
                    case ktls_support(OS) of
                        {ok, Rekey} ->
                            ssl_test_lib:clean_start(),
                            [{ktls_rekey, Rekey} | ssl_test_lib:make_rsa_cert(Config)];
                        {error, Reason} ->
                            {skip, {ktls_notsup, Reason}}
                    end;
                _ ->
                    {skip, "Crypto did not start"}
            end;
        {error, Reason} ->
            {skip, Reason}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

init_per_group(inet, Config) ->
    [{backend, []} | Config];
init_per_group(socket, Config) ->
    [{backend, [{inet_backend, socket}]} | Config].

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(TestCase, Config) ->
    ct:timetrap({seconds, 30}),
    case lists:member(TestCase, key_update_cases()) andalso
        not proplists:get_value(ktls_rekey, Config) of
        true ->
            {skip, "No kernel support for kTLS key updates"};
        false ->
            Config
    end.

end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

handover() ->
    [{doc, "The connection process hands a plain socket over and exits"}].
handover(Config) ->
    {Client, Server} = pair(Config),
    ok = handed_over(Client, Config),
    ok = handed_over(Server, Config),
    ok = send_recv(Client, Server, <<"ping">>),
    ok = send_recv(Server, Client, <<"pong">>),
    close(Client, Server).

large_data() ->
    [{doc, "Data spanning many records"}].
large_data(Config) ->
    {Client, Server} = pair(Config),
    Data = crypto:strong_rand_bytes(3 * 1024 * 1024),
    _ = spawn_link(fun() -> ok = ssl:send(Client, Data) end),
    {ok, Data} = ssl:recv(Server, byte_size(Data), ?TIMEOUT),
    close(Client, Server).

active_once() ->
    [{doc, "{active, once} delivers ssl messages"}].
active_once(Config) ->
    {Client, Server} = pair(Config),
    ok = ssl:setopts(Server, [{active, once}]),
    ok = ssl:send(Client, <<"first">>),
    receive {ssl, Server, <<"first">>} -> ok
    after ?TIMEOUT -> ct:fail(no_data)
    end,
    ok = ssl:send(Client, <<"second">>),
    receive {ssl, Server, _} = Unexpected -> ct:fail({still_active, Unexpected})
    after 200 -> ok
    end,
    ok = ssl:setopts(Server, [{active, once}]),
    receive {ssl, Server, <<"second">>} -> ok
    after ?TIMEOUT -> ct:fail(no_data)
    end,
    close(Client, Server).

active_n() ->
    [{doc, "{active, N} ends with ssl_passive"}].
active_n(Config) ->
    {Client, Server} = pair(Config),
    ok = ssl:setopts(Server, [{active, 2}]),
    ok = ssl:send(Client, <<"a">>),
    receive {ssl, Server, <<"a">>} -> ok
    after ?TIMEOUT -> ct:fail(no_data)
    end,
    ok = ssl:send(Client, <<"b">>),
    receive {ssl, Server, <<"b">>} -> ok
    after ?TIMEOUT -> ct:fail(no_data)
    end,
    receive {ssl_passive, Server} -> ok
    after ?TIMEOUT -> ct:fail(no_passive)
    end,
    close(Client, Server).

packet() ->
    [{doc, "The packet option applies after the handover"}].
packet(Config) ->
    {Client, Server} = pair(Config, [{packet, 4}], [{packet, 4}]),
    ok = ssl:send(Client, <<"framed">>),
    {ok, <<"framed">>} = ssl:recv(Server, 0, ?TIMEOUT),
    ok = ssl:send(Server, <<"reply">>),
    {ok, <<"reply">>} = ssl:recv(Client, 0, ?TIMEOUT),
    close(Client, Server).

options() ->
    [{doc, "getopts/2 and setopts/2 on a handed over socket"}].
options(Config) ->
    {Client, Server} = pair(Config),
    {ok, [{packet, 0}]} = ssl:getopts(Client, [packet]),
    {ok, [{mode, binary}]} = ssl:getopts(Client, [mode]),
    ok = ssl:setopts(Client, [{packet, 2}]),
    ok = ssl:setopts(Server, [{packet, 2}]),
    {ok, [{packet, 2}]} = ssl:getopts(Client, [packet]),
    ok = ssl:send(Client, <<"packet 2">>),
    {ok, <<"packet 2">>} = ssl:recv(Server, 0, ?TIMEOUT),
    close(Client, Server).

read_ahead() ->
    [{doc, "The read_ahead option is kept through the handover"}].
read_ahead(Config) ->
    {Client, Server} = pair(Config, [{read_ahead, false}], []),
    {ok, [{read_ahead, false}]} = ssl:getopts(Client, [read_ahead]),
    {ok, [{read_ahead, true}]} = ssl:getopts(Server, [read_ahead]),
    ok = send_recv(Server, Client, <<"no read ahead">>),
    close(Client, Server).

connection_information() ->
    [{doc, "Connection information is kept for after the handover"}].
connection_information(Config) ->
    {Client, Server} = pair(Config),
    {ok, Info} = ssl:connection_information(Client),
    'tlsv1.3' = proplists:get_value(protocol, Info),
    {ok, [{protocol, 'tlsv1.3'}]} = ssl:connection_information(Client, [protocol]),
    {ok, _} = ssl:peercert(Client),
    {error, no_peercert} = ssl:peercert(Server),
    {error, protocol_not_negotiated} = ssl:negotiated_protocol(Client),
    {ok, {_, Port}} = ssl:sockname(Server),
    {ok, {_, Port}} = ssl:peername(Client),
    {ok, [{recv_oct, _}]} = ssl:getstat(Client, [recv_oct]),
    close(Client, Server).

controlling_process() ->
    [{doc, "Active messages go to a new controlling process"}].
controlling_process(Config) ->
    {Client, Server} = pair(Config),
    Self = self(),
    Owner = spawn_link(fun() ->
                               receive go -> ok end,
                               ok = ssl:setopts(Client, [{active, once}]),
                               receive {ssl, Client, Data} -> Self ! {self(), Data}
                               after ?TIMEOUT -> Self ! {self(), timeout}
                               end,
                               receive stop -> ok end
                       end),
    ok = ssl:controlling_process(Client, Owner),
    Owner ! go,
    ok = ssl:send(Server, <<"new owner">>),
    receive {Owner, Result} -> <<"new owner">> = Result
    after ?TIMEOUT -> ct:fail(timeout)
    end,
    close(Client, Server),
    Owner ! stop,
    ok.

controlling_process_pending_control() ->
    [{doc, "A control record pending when the owner changes is handled by "
      "the new owner"}].
controlling_process_pending_control(Config) ->
    {Client, Server} = pair(Config),
    ok = ssl:setopts(Client, [{active, true}]),
    ok = ssl:update_keys(Server, write),
    ct:sleep(200), % a port pauses at the KeyUpdate
    Self = self(),
    Owner = spawn_link(fun() ->
                               receive go -> ok end,
                               _ = active_recv(Client, <<"new owner">>, false),
                               ok = ssl:controlling_process(Client, Self),
                               Self ! {self(), done}
                       end),
    ok = ssl:controlling_process(Client, Owner),
    receive {ssl_control, Client, _} -> ct:fail(left_with_old_owner)
    after 0 -> ok
    end,
    Owner ! go,
    ok = ssl:send(Server, <<"new owner">>),
    receive {Owner, done} -> ok
    after 2 * ?TIMEOUT -> ct:fail(timeout)
    end,
    close(Client, Server).

packet_http() ->
    [{doc, "Active messages of an HTTP packet type carry the ssl socket"}].
packet_http(Config) ->
    {Client, Server} = pair(Config),
    ok = ssl:setopts(Client, [{packet, http_bin}, {active, true}]),
    ok = ssl:send(Server, <<"GET / HTTP/1.1\r\nHost: x\r\n\r\n">>),
    {http_request, 'GET', {abs_path, <<"/">>}, {1, 1}} = active_msg(Client),
    {http_header, _, 'Host', _, <<"x">>} = active_msg(Client),
    http_eoh = active_msg(Client),
    ok = ssl:setopts(Client, [{packet, raw}, {active, false}]),
    ok = send_recv(Server, Client, <<"raw again">>),
    close(Client, Server).

packet_types_at_handover() ->
    [{doc, "Packet types other than raw and 1, 2, 4 apply at the handover"}].
packet_types_at_handover(Config) ->
    {Client, Server} = pair(Config, [{packet, line}], [{packet, http_bin}]),
    ok = ssl:send(Client, <<"GET / HTTP/1.1\r\nHost: x\r\n\r\n">>),
    {ok, {http_request, 'GET', {abs_path, <<"/">>}, {1, 1}}} =
        ssl:recv(Server, 0, ?TIMEOUT),
    {ok, {http_header, _, 'Host', _, <<"x">>}} = ssl:recv(Server, 0, ?TIMEOUT),
    {ok, http_eoh} = ssl:recv(Server, 0, ?TIMEOUT),
    ok = ssl:send(Server, <<"one\ntwo\n">>),
    {ok, <<"one\n">>} = ssl:recv(Client, 0, ?TIMEOUT),
    {ok, <<"two\n">>} = ssl:recv(Client, 0, ?TIMEOUT),
    close(Client, Server).

early_data() ->
    [{doc, "0-RTT data a kTLS server accepted comes before later data, with "
      "the server passive or active during the handshake"}].
early_data(Config) ->
    Backend = proplists:get_value(backend, Config),
    Cert = proplists:delete(reuseaddr,
                            ssl_test_lib:ssl_options(server_rsa_opts, Config)),
    Common = [{versions, ['tlsv1.3']}, {active, false}, binary],
    {ok, Listen} = ssl:listen(0, Backend ++ Common ++ Cert ++
                                  [{ktls, true}, {session_tickets, stateless},
                                   {early_data, enabled}]),
    {ok, {_, Port}} = ssl:sockname(Listen),
    ClientOpts = [{verify, verify_none}, {session_tickets, manual} | Common],
    {Client0, Server0, _} = early_connect(Listen, Port, ClientOpts, false),
    ok = send_recv(Server0, Client0, <<"tickets">>),
    [Ticket1, Ticket2 | _] = tickets([]),
    close(Client0, Server0),
    Early = fun(Ticket) ->
                    [{use_ticket, [Ticket]}, {early_data, <<"early">>} | ClientOpts]
            end,
    {Client1, Server1, accepted} =
        early_connect(Listen, Port, Early(Ticket1), false),
    ok = ssl:send(Client1, <<"|later">>),
    {ok, <<"early|later">>} = ssl:recv(Server1, 11, ?TIMEOUT),
    close(Client1, Server1),
    {Client2, Server2, accepted} =
        early_connect(Listen, Port, Early(Ticket2), true),
    ok = ssl:send(Client2, <<"|later">>),
    <<"early|later">> = active_bytes(Server2, 11, <<>>),
    close(Client2, Server2),
    ssl:close(Listen).

data_with_finished() ->
    [{doc, "Data that arrives together with the client's Finished is not lost"}].
data_with_finished(Config) ->
    Backend = proplists:get_value(backend, Config),
    Cert = proplists:delete(reuseaddr,
                            ssl_test_lib:ssl_options(server_rsa_opts, Config)),
    Common = [{versions, ['tlsv1.3']}, {active, false}, binary],
    {ok, Listen} = ssl:listen(0, Backend ++ Common ++ Cert ++ [{ktls, true}]),
    {ok, {_, Port}} = ssl:sockname(Listen),
    {ok, ProxyListen} = gen_tcp:listen(0, [binary, {active, false}]),
    {ok, ProxyPort} = inet:port(ProxyListen),
    _ = spawn_link(fun() -> proxy(ProxyListen, Port) end),
    Client = spawn_link(fun() ->
                                {ok, C} = ssl:connect("localhost", ProxyPort,
                                                      [{verify, verify_none} | Common],
                                                      ?TIMEOUT),
                                ok = ssl:send(C, <<"first">>),
                                receive stop -> ssl:close(C) end
                        end),
    {ok, Socket} = ssl:transport_accept(Listen, ?TIMEOUT),
    {ok, Server} = ssl:handshake(Socket, ?TIMEOUT),
    {ok, <<"first">>} = ssl:recv(Server, 5, ?TIMEOUT),
    Client ! stop,
    ok = ssl:close(Server),
    ok = ssl:close(Listen),
    ok = gen_tcp:close(ProxyListen).

update_keys_write() ->
    [{doc, "update_keys/2 with write"}].
update_keys_write(Config) ->
    {Client, Server} = pair(Config),
    ok = send_recv(Client, Server, <<"before">>),
    ok = ssl:update_keys(Client, write),
    ok = send_recv(Client, Server, <<"after">>),
    ok = send_recv(Server, Client, <<"reply">>),
    close(Client, Server).

update_keys_read_write() ->
    [{doc, "update_keys/2 with read_write: the peer's recv answers with its "
      "own KeyUpdate"}].
update_keys_read_write(Config) ->
    {Client, Server} = pair(Config),
    ok = ssl:update_keys(Client, read_write),
    ok = send_recv(Client, Server, <<"after">>),
    ok = send_recv(Server, Client, <<"reply">>),
    ok = send_recv(Client, Server, <<"again">>),
    close(Client, Server).

peer_key_update_active() ->
    [{doc, "An active owner gets ssl_control at a peer KeyUpdate"}].
peer_key_update_active(Config) ->
    {Client, Server} = pair(Config),
    ok = ssl:setopts(Server, [{active, true}]),
    ok = ssl:update_keys(Client, read_write),
    ok = ssl:send(Client, <<"after update">>),
    true = active_recv(Server, <<"after update">>, false),
    ok = ssl:setopts(Server, [{active, false}]),
    ok = send_recv(Server, Client, <<"reply">>),
    close(Client, Server).

repeated_key_updates() ->
    [{doc, "Key updates in both directions, many times"}].
repeated_key_updates(Config) ->
    {Client, Server} = pair(Config),
    [begin
         ok = ssl:update_keys(Client, write),
         ok = send_recv(Client, Server, <<"c", N:32>>),
         ok = ssl:update_keys(Server, read_write),
         ok = send_recv(Server, Client, <<"s", N:32>>)
     end || N <- lists:seq(1, 20)],
    ok = send_recv(Client, Server, <<"done">>),
    close(Client, Server).

key_updates_under_load() ->
    [{doc, "Key updates while another process sends"}].
key_updates_under_load(Config) ->
    {Client, Server} = pair(Config),
    Chunk = crypto:strong_rand_bytes(64 * 1024),
    N = 32,
    _ = spawn_link(fun() -> [ok = ssl:send(Client, Chunk) || _ <- lists:seq(1, N)] end),
    _ = spawn_link(fun() ->
                           [begin
                                timer:sleep(5),
                                ok = ssl:update_keys(Client, write)
                            end || _ <- lists:seq(1, 10)]
                   end),
    {ok, Data} = ssl:recv(Server, N * byte_size(Chunk), ?TIMEOUT),
    true = Data =:= binary:copy(Chunk, N),
    close(Client, Server).

concurrent_key_updates() ->
    [{doc, "update_keys/2 from several processes at once"}].
concurrent_key_updates(Config) ->
    {Client, Server} = pair(Config),
    Self = self(),
    Pids = [spawn_link(fun() -> Self ! {self(), ssl:update_keys(Client, write)} end)
            || _ <- lists:seq(1, 8)],
    [receive {Pid, Result} -> ok = Result
     after ?TIMEOUT -> ct:fail(timeout)
     end || Pid <- Pids],
    ok = send_recv(Client, Server, <<"after concurrent updates">>),
    close(Client, Server).

key_update_full_send_buffer() ->
    [{doc, "update_keys/2 while the send buffer is full and output is queued"}].
key_update_full_send_buffer(Config) ->
    {Client, Server} = pair(Config, [{sndbuf, 4096}], [{recbuf, 4096}]),
    Chunk = crypto:strong_rand_bytes(64 * 1024),
    N = 32,
    Self = self(),
    %% The server does not read yet, so the client's send buffer fills and
    %% the rest of the data waits in its send queue
    Sender = spawn_link(fun() ->
                                [ok = ssl:send(Client, Chunk) || _ <- lists:seq(1, N)],
                                Self ! {self(), sent}
                        end),
    timer:sleep(200),
    Reader = spawn_link(fun() ->
                                timer:sleep(500),
                                Self ! {self(), ssl:recv(Server, N * byte_size(Chunk),
                                                         ?TIMEOUT)}
                        end),
    %% The KeyUpdate waits for the queued data and for room to be sent
    ok = ssl:update_keys(Client, write),
    receive {Reader, Result} ->
            {ok, Data} = Result,
            true = Data =:= binary:copy(Chunk, N)
    after 2 * ?TIMEOUT -> ct:fail(no_data)
    end,
    receive {Sender, sent} -> ok
    after ?TIMEOUT -> ct:fail(not_sent)
    end,
    %% Derived from the state the first update stored
    ok = ssl:update_keys(Client, write),
    ok = send_recv(Client, Server, <<"after both updates">>),
    close(Client, Server).

key_update_two_readers() ->
    [{doc, "Two processes handle the same peer KeyUpdate: its key is "
      "installed once"}].
key_update_two_readers(Config) ->
    {Client, Server} = pair(Config),
    ok = send_recv(Client, Server, <<"before">>),
    ok = ssl:update_keys(Client, write),
    %% Until a process installs its key, every process handling control
    %% records gets the KeyUpdate, left unread
    {ok, <<2, _/binary>> = KeyUpdate} = control(Server, 100),
    {ok, KeyUpdate} = control(Server, 0),
    Self = self(),
    Pids = [spawn_link(fun() -> Self ! {self(), ssl:handle_control(Server, false)} end)
            || _ <- [1, 2]],
    [receive {Pid, Result} -> ok = Result
     after ?TIMEOUT -> ct:fail(timeout)
     end || Pid <- Pids],
    ok = send_recv(Client, Server, <<"after">>),
    ok = send_recv(Server, Client, <<"reply">>),
    close(Client, Server).

recv_other_process() ->
    [{doc, "A process other than the owner receives across a KeyUpdate"}].
recv_other_process(Config) ->
    {Client, Server} = pair(Config),
    Self = self(),
    ok = ssl:update_keys(Client, read_write),
    ok = ssl:send(Client, <<"other">>),
    Pid = spawn_link(fun() -> Self ! {self(), ssl:recv(Server, 5, ?TIMEOUT)} end),
    receive {Pid, Result} -> {ok, <<"other">>} = Result
    after 2 * ?TIMEOUT -> ct:fail(timeout)
    end,
    ok = send_recv(Server, Client, <<"reply">>),
    close(Client, Server).

session_tickets() ->
    [{doc, "NewSessionTicket messages after the handover are skipped"}].
session_tickets(Config) ->
    {Client, Server} = pair(Config, [], [{session_tickets, stateless}]),
    ok = send_recv(Server, Client, <<"after tickets">>),
    ok = send_recv(Client, Server, <<"reply">>),
    close(Client, Server).

close_notify() ->
    [{doc, "close/1 sends close_notify after the data"}].
close_notify(Config) ->
    {Client, Server} = pair(Config),
    ok = ssl:send(Client, <<"last">>),
    ok = ssl:close(Client),
    {ok, <<"last">>} = ssl:recv(Server, 4, ?TIMEOUT),
    {error, closed} = ssl:recv(Server, 0, ?TIMEOUT),
    _ = ssl:close(Server),
    ok.

peer_close_active() ->
    [{doc, "An active owner learns that the peer closed"}].
peer_close_active(Config) ->
    {Client, Server} = pair(Config),
    ok = ssl:setopts(Server, [{active, true}]),
    ok = ssl:close(Client),
    receive
        {ssl_closed, Server} ->
            ok;
        {ssl_control, Server, Active} ->
            {error, closed} = ssl:handle_control(Server, Active)
    after ?TIMEOUT ->
            ct:fail(no_close)
    end,
    _ = ssl:close(Server),
    ok.

aes_128_gcm() ->
    [{doc, "TLS_AES_128_GCM_SHA256"}].
aes_128_gcm(Config) ->
    Ciphers = [{ciphers, [#{key_exchange => any, cipher => aes_128_gcm,
                            mac => aead, prf => sha256}]}],
    {Client, Server} = pair(Config, Ciphers, Ciphers),
    {ok, [{selected_cipher_suite, #{cipher := aes_128_gcm}}]} =
        ssl:connection_information(Client, [selected_cipher_suite]),
    ok = send_recv(Client, Server, <<"aes_128_gcm">>),
    ok = send_recv(Server, Client, <<"reply">>),
    close(Client, Server).

aes_128_ccm() ->
    [{doc, "TLS_AES_128_CCM_SHA256, when the kernel has it"}].
aes_128_ccm(Config) ->
    case lists:member(aes_128_ccm, crypto:supports(ciphers)) of
        true ->
            Ciphers = [{ciphers, [#{key_exchange => any, cipher => aes_128_ccm,
                                    mac => aead, prf => sha256}]}],
            case connect(Config, Ciphers, Ciphers) of
                {ok, Client, Server} ->
                    ok = send_recv(Client, Server, <<"aes_128_ccm">>),
                    ok = send_recv(Server, Client, <<"reply">>),
                    close(Client, Server);
                {error, {ktls, enoent}} ->
                    {skip, "No kernel support for AES-128-CCM"}
            end;
        false ->
            {skip, "No crypto support for AES-128-CCM"}
    end.

chacha20_poly1305() ->
    [{doc, "TLS_CHACHA20_POLY1305_SHA256, when the kernel has it"}].
chacha20_poly1305(Config) ->
    case lists:member(chacha20_poly1305, crypto:supports(ciphers)) of
        true ->
            Ciphers = [{ciphers, [#{key_exchange => any, cipher => chacha20_poly1305,
                                    mac => aead, prf => sha256}]}],
            case connect(Config, Ciphers, Ciphers) of
                {ok, Client, Server} ->
                    ok = send_recv(Client, Server, <<"chacha20_poly1305">>),
                    ok = send_recv(Server, Client, <<"reply">>),
                    close(Client, Server);
                {error, {ktls, enoent}} ->
                    {skip, "No kernel support for ChaCha20-Poly1305"}
            end;
        false ->
            {skip, "No crypto support for ChaCha20-Poly1305"}
    end.

tls_1_2() ->
    [{doc, "A TLS 1.2 connection cannot be handed over"}].
tls_1_2(Config) ->
    Version = [{versions, ['tlsv1.2']}],
    {error, {ktls, {ktls_notsup, _}}} = connect(Config, Version, Version),
    ok.

max_fragment_length() ->
    [{doc, "The kernel sends full size records, so a negotiated "
      "max_fragment_length refuses the handover"}].
max_fragment_length(Config) ->
    {error, {ktls, {ktls_notsup, max_fragment_length}}} =
        connect(Config, [{max_fragment_length, 512}], []),
    ok.

unexpected_record() ->
    [{doc, "A control record the peer should not have sent gets a fatal alert"}].
unexpected_record(Config) ->
    {Client, Server} = pair(Config),
    %% A handshake message of an unknown type
    ok = send_record(Client, 22, <<99, 0, 0, 0>>),
    {error, {tls_alert, {unexpected_message, Own}}} = ssl:recv(Server, 0, ?TIMEOUT),
    {error, {tls_alert, {unexpected_message, Peer}}} = ssl:recv(Client, 0, ?TIMEOUT),
    %% The texts name the roles, as for any ssl connection
    true = string:find(Own, "generated SERVER ALERT") =/= nomatch,
    true = string:find(Peer, "received SERVER ALERT") =/= nomatch,
    close(Client, Server).

unsupported_calls() ->
    [{doc, "Calls that need the connection process return an error"}].
unsupported_calls(Config) ->
    {Client, Server} = pair(Config),
    {error, {ktls_notsup, downgrade}} = ssl:close(Client, {self(), ?TIMEOUT}),
    {error, notsup} = ssl:renegotiate(Client),
    {error, {ktls_notsup, export_key_materials}} =
        ssl:export_key_materials(Client, [<<"label">>], [no_context], [16]),
    {error, {ktls_notsup, export_key_materials}} =
        ssl:export_key_materials(Client, [<<"label">>], [no_context], [16], false),
    {error, {ktls_notsup, prf}} =
        ssl:prf(Client, master_secret, <<"label">>, [client_random, server_random], 16),
    ok = send_recv(Client, Server, <<"still connected">>),
    close(Client, Server).

split_handshake_message() ->
    [{doc, "A handshake message split across records is not supported: its "
      "first record gets an unexpected_message alert"}].
split_handshake_message(Config) ->
    {Client, Server} = pair(Config),
    Body = <<60:32, 1:32, 0, 2:16, 9, 9, 0:16>>, % lifetime, age_add, nonce, ticket, extensions
    {Part1, _} = split_binary(<<4, (byte_size(Body)):24, Body/binary>>, 7),
    ok = send_record(Server, 22, Part1),
    {error, {tls_alert, {unexpected_message, _}}} = ssl:recv(Client, 0, ?TIMEOUT),
    close(Client, Server).

key_update_with_trailing_data() ->
    [{doc, "A KeyUpdate must end its record (RFC 8446, 5.1)"}].
key_update_with_trailing_data(Config) ->
    {Client, Server} = pair(Config),
    Body = <<60:32, 1:32, 0, 2:16, 9, 9, 0:16>>, % lifetime, age_add, nonce, ticket, extensions
    Ticket = <<4, (byte_size(Body)):24, Body/binary>>,
    ok = send_record(Client, 22, <<24, 1:24, 0, Ticket/binary>>),
    {error, {tls_alert, {unexpected_message, _}}} = ssl:recv(Server, 0, ?TIMEOUT),
    {error, {tls_alert, {unexpected_message, _}}} = ssl:recv(Client, 0, ?TIMEOUT),
    close(Client, Server).

failed_key_update() ->
    [{doc, "A failed key update closes the socket, and later calls return "
      "{error, closed}"}].
failed_key_update(Config) ->
    {Client, Server} = pair(Config),
    %% The peer's kernel resets the connection at our next record
    ok = ssl:close(Server),
    {error, _} = until_error(fun() -> ssl:update_keys(Client, write) end, 50),
    {error, closed} = ssl:update_keys(Client, write),
    {error, closed} = ssl:handle_control(Client, false),
    {error, closed} = ssl:send(Client, <<"x">>),
    {error, closed} = ssl:recv(Client, 0, ?TIMEOUT),
    {error, closed} = ssl:connection_information(Client),
    ok = ssl:close(Client).

recv_timeout_across_control() ->
    [{doc, "A recv that handles a control record keeps its timeout"}].
recv_timeout_across_control(Config) ->
    {Client, Server} = pair(Config),
    ok = ssl:update_keys(Client, write), % only a control record for the server
    T0 = erlang:monotonic_time(millisecond),
    {error, timeout} = ssl:recv(Server, 0, 500),
    Elapsed = erlang:monotonic_time(millisecond) - T0,
    true = Elapsed >= 450 andalso Elapsed < 1500,
    ok = send_recv(Client, Server, <<"after the timeout">>),
    close(Client, Server).

key_update_overtakes_queued_output() ->
    [{doc, "A KeyUpdate does not wait for output a port still has queued"}].
key_update_overtakes_queued_output(Config) ->
    case proplists:get_value(backend, Config) of
        [] ->
            {Client, Server} = pair(Config, [{sndbuf, 65536}], [{recbuf, 65536}]),
            #sslsocket{socket_handle = Port} = Client,
            Data = binary:copy(<<"queued">>, 700000),
            Self = self(),
            Reader = spawn_link(fun() ->
                                        Self ! {self(), slow_recv(Server, byte_size(Data), <<>>)}
                                end),
            %% Most of Data waits in the port's queue while the server reads
            ok = ssl:send(Client, Data),
            {queue_size, Before} = erlang:port_info(Port, queue_size),
            true = Before > 0,
            ok = ssl:update_keys(Client, write),
            {queue_size, After} = erlang:port_info(Port, queue_size),
            After > 0 orelse ct:fail(key_update_waited_for_queued_output),
            receive {Reader, Received} -> Received = Data
            after 6 * ?TIMEOUT -> ct:fail(reader_timeout)
            end,
            ok = send_recv(Client, Server, <<"after">>),
            close(Client, Server);
        _ ->
            {skip, "The socket backend queues no output"}
    end.

key_update_reply_owed() ->
    [{doc, "A KeyUpdate the peer requests while our send buffer is full is "
      "owed: the reader does not wait, and it goes out before our data"}].
key_update_reply_owed(Config) ->
    {Client, Server} = pair(Config, [{recbuf, 65536}], [{sndbuf, 65536}]),
    Data = binary:copy(<<"owed">>, 1000000),
    Self = self(),
    %% The client does not read yet: the server's send buffer fills up
    Sender = spawn_link(fun() -> Self ! {self(), ssl:send(Server, Data)} end),
    ct:sleep(500),
    ok = ssl:update_keys(Client, read_write),
    T0 = erlang:monotonic_time(millisecond),
    {error, timeout} = ssl:recv(Server, 0, 1000),
    Elapsed = erlang:monotonic_time(millisecond) - T0,
    true = Elapsed < 2000,
    Data = recv_bytes(Client, byte_size(Data), <<>>),
    receive {Sender, Sent} -> ok = Sent
    after ?TIMEOUT -> ct:fail(not_sent)
    end,
    ok = send_recv(Server, Client, <<"after the owed update">>),
    ok = send_recv(Client, Server, <<"reply">>),
    close(Client, Server).

close_notify_after_queued_output() ->
    [{doc, "close_notify waits for the output a port still holds"}].
close_notify_after_queued_output(Config) ->
    case proplists:get_value(backend, Config) of
        [] ->
            {Client, Server} = pair(Config, [{recbuf, 65536}], [{sndbuf, 65536}]),
            Data = binary:copy(<<"queued">>, 200000),
            ok = ssl:send(Server, Data),
            Self = self(),
            Closer = spawn_link(fun() -> Self ! {self(), ssl:close(Server)} end),
            Data = recv_bytes(Client, byte_size(Data), <<>>),
            {error, closed} = ssl:recv(Client, 0, ?TIMEOUT),
            receive {Closer, Closed} -> ok = Closed
            after ?TIMEOUT -> ct:fail(not_closed)
            end,
            _ = ssl:close(Client),
            ok;
        _ ->
            {skip, "The socket backend queues no output"}
    end.

key_install_waits_for_queued_output() ->
    [{doc, "Installing a send key waits for the output a port has queued"}].
key_install_waits_for_queued_output(Config) ->
    case proplists:get_value(backend, Config) of
        [] ->
            {ok, OS} = ssl_test_lib:ktls_os(),
            {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {recbuf, 4096}]),
            {ok, Port} = inet:port(Listen),
            {ok, Client} = gen_tcp:connect({127,0,0,1}, Port,
                                           [binary, {active, false}, {sndbuf, 4096}]),
            {ok, Server} = gen_tcp:accept(Listen, ?TIMEOUT),
            Data = binary:copy(<<"x">>, 1024 * 1024),
            Self = self(),
            %% The server does not read yet, so most of Data waits in the
            %% client port's queue
            _ = spawn_link(fun() -> Self ! {sent, gen_tcp:send(Client, Data)} end),
            timer:sleep(200),
            _ = spawn_link(fun() ->
                                   timer:sleep(500),
                                   Self ! {read, gen_tcp:recv(Server, byte_size(Data),
                                                              ?TIMEOUT)}
                           end),
            ok = ssl_test_lib:ktls_set_ulp(Client, OS),
            ok = ssl_test_lib:ktls_set_cipher(Client, OS, tx, 1),
            receive {read, Read} -> {ok, Data} = Read
            after ?TIMEOUT -> ct:fail(no_data)
            end,
            receive {sent, Sent} -> ok = Sent
            after ?TIMEOUT -> ct:fail(not_sent)
            end,
            _ = gen_tcp:close(Client),
            _ = gen_tcp:close(Server),
            _ = gen_tcp:close(Listen),
            ok;
        _ ->
            {skip, "The socket backend queues no output"}
    end.

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

pair(Config) ->
    pair(Config, [], []).

pair(Config, ClientOpts, ServerOpts) ->
    case connect(Config, ClientOpts, ServerOpts) of
        {ok, Client, Server} ->
            {Client, Server};
        {error, Reason} ->
            ct:fail({connect, Reason})
    end.

connect(Config, ClientOpts, ServerOpts) ->
    %% inet_backend must come first
    Backend = proplists:get_value(backend, Config),
    Common = [{ktls, true}, {versions, ['tlsv1.3']}, {active, false}, binary],
    %% {reuseaddr, true} is not supported with the socket backend
    Cert = proplists:delete(reuseaddr,
                            ssl_test_lib:ssl_options(server_rsa_opts, Config)),
    {ok, Listen} = ssl:listen(0, Backend ++ Common ++ Cert ++ ServerOpts),
    {ok, {_, Port}} = ssl:sockname(Listen),
    Self = self(),
    Acceptor = spawn_link(fun() -> accept(Listen, Self) end),
    Client = ssl:connect("localhost", Port,
                         Backend ++ Common ++ [{verify, verify_none} | ClientOpts],
                         ?TIMEOUT),
    Server = receive {Acceptor, Result} -> Result
             after ?TIMEOUT -> {error, accept_timeout}
             end,
    ok = ssl:close(Listen),
    case {Client, Server} of
        {{ok, C}, {ok, S}} ->
            {ok, C, S};
        {{error, _} = Error, _} ->
            close_result(Server),
            Error;
        {_, {error, _} = Error} ->
            close_result(Client),
            Error
    end.

accept(Listen, Parent) ->
    Result =
        maybe
            {ok, Socket0} ?= ssl:transport_accept(Listen, ?TIMEOUT),
            {ok, Socket} ?= ssl:handshake(Socket0, ?TIMEOUT),
            ok ?= ssl:controlling_process(Socket, Parent),
            {ok, Socket}
        end,
    Parent ! {self(), Result}.

close_result({ok, Socket}) ->
    _ = ssl:close(Socket),
    ok;
close_result(_) ->
    ok.

close(Client, Server) ->
    _ = ssl:close(Client),
    _ = ssl:close(Server),
    ok.

handed_over(#sslsocket{socket_handle = Handle, connection_handler = undefined},
            Config) ->
    case {proplists:get_value(backend, Config), Handle} of
        {[], Port} when is_port(Port) ->
            ok;
        {[{inet_backend, socket}], {'$inet', gen_tcp_socket, {_, {'$socket', _}}}} ->
            ok
    end.

until_error(Fun, N) ->
    case Fun() of
        ok when N > 0 -> timer:sleep(20), until_error(Fun, N - 1);
        Result -> Result
    end.

recv_bytes(_, 0, Acc) ->
    Acc;
recv_bytes(Socket, N, Acc) ->
    {ok, Data} = ssl:recv(Socket, 0, ?TIMEOUT),
    recv_bytes(Socket, N - byte_size(Data), <<Acc/binary, Data/binary>>).

%% The control record at the head of the stream (a kTLS CONTROL
%% request), retried while there is none
control(#sslsocket{socket_handle = Handle} = Socket, N) ->
    case tls_request(Handle, <<4>>) of
        {ok, <<0>>} when N > 0 ->
            timer:sleep(10),
            control(Socket, N - 1);
        Result ->
            Result
    end.

%% Send one record of Type through the kernel, bypassing ssl
send_record(#sslsocket{socket_handle = Handle}, Type, Data) ->
    {ok, _} = tls_request(Handle, <<8, Type, Data/binary>>),
    ok.

tls_request(Port, Request) when is_port(Port) ->
    prim_inet:tls(Port, Request);
tls_request({'$inet', gen_tcp_socket, {_, {'$socket', Ref}}}, Request) ->
    prim_socket:tls(Ref, Request).

send_recv(From, To, Data) ->
    ok = ssl:send(From, Data),
    {ok, Data} = ssl:recv(To, byte_size(Data), ?TIMEOUT),
    ok.

%% Returns whether an ssl_control message came before Data
active_recv(Socket, Data, Control) ->
    receive
        {ssl, Socket, Data} ->
            Control;
        {ssl_control, Socket, Active} ->
            ok = ssl:handle_control(Socket, Active),
            active_recv(Socket, Data, true)
    after ?TIMEOUT ->
            ct:fail({no_data, Data})
    end.

%% The server accepts in this process, active or not during the handshake
early_connect(Listen, Port, ClientOpts, Active) ->
    ok = ssl:setopts(Listen, [{active, Active}]),
    Self = self(),
    Pid = spawn_link(fun() ->
                             {ok, Client} = ssl:connect("localhost", Port, ClientOpts,
                                                        ?TIMEOUT),
                             Accepted = receive {ssl, Client, {early_data, A}} -> A
                                        after 1000 -> none
                                        end,
                             ok = ssl:controlling_process(Client, Self),
                             forward_tickets(Self),
                             Self ! {self(), Client, Accepted}
                     end),
    {ok, Socket} = ssl:transport_accept(Listen, ?TIMEOUT),
    {ok, Server} = ssl:handshake(Socket, ?TIMEOUT),
    receive {Pid, Client, Accepted} -> {Client, Server, Accepted}
    after ?TIMEOUT -> ct:fail(client_timeout)
    end.

%% Tickets the client received before the owner changed
forward_tickets(To) ->
    receive {ssl, session_ticket, _} = Ticket -> To ! Ticket, forward_tickets(To)
    after 0 -> ok
    end.

tickets(Acc) ->
    receive {ssl, session_ticket, Ticket} -> tickets([Ticket | Acc])
    after 500 -> lists:reverse(Acc)
    end.

active_bytes(_, N, Acc) when byte_size(Acc) >= N ->
    Acc;
active_bytes(Socket, N, Acc) ->
    receive {ssl, Socket, Data} -> active_bytes(Socket, N, <<Acc/binary, Data/binary>>)
    after ?TIMEOUT -> ct:fail({received, Acc})
    end.

%% Forwards to Port; what the client sends is held until 50 ms after the
%% first byte, so its Finished and first data arrive in one segment
proxy(Listen, Port) ->
    {ok, C} = gen_tcp:accept(Listen, ?TIMEOUT),
    {ok, S} = gen_tcp:connect("localhost", Port, [binary, {active, true}]),
    ok = inet:setopts(C, [{active, true}]),
    proxy(C, S, [], false).

proxy(C, S, Held, Timer) ->
    receive
        {tcp, C, Data} when Timer ->
            proxy(C, S, [Held, Data], Timer);
        {tcp, C, Data} ->
            erlang:send_after(50, self(), flush),
            proxy(C, S, [Held, Data], true);
        flush ->
            _ = gen_tcp:send(S, Held),
            proxy(C, S, [], false);
        {tcp, S, Data} ->
            _ = gen_tcp:send(C, Data),
            proxy(C, S, Held, Timer);
        {tcp_closed, _} ->
            _ = gen_tcp:send(S, Held),
            _ = gen_tcp:close(C),
            gen_tcp:close(S)
    end.

%% 16 KB about every millisecond
slow_recv(_, 0, Acc) ->
    Acc;
slow_recv(Socket, N, Acc) ->
    timer:sleep(1),
    {ok, Data} = ssl:recv(Socket, min(N, 16384), ?TIMEOUT),
    slow_recv(Socket, N - byte_size(Data), <<Acc/binary, Data/binary>>).

active_msg(Socket) ->
    receive {ssl, Socket, Msg} -> Msg
    after ?TIMEOUT -> ct:fail(no_message)
    end.

%% Kernel TLS works, and a second key (a key update) can be installed
ktls_support(OS) ->
    {ok, Listen} = gen_tcp:listen(0, [{active, false}]),
    {ok, Port} = inet:port(Listen),
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [{active, false}]),
    try
        maybe
            ok ?= ssl_test_lib:ktls_set_ulp(Socket, OS),
            ok ?= ssl_test_lib:ktls_set_cipher(Socket, OS, tx, 1),
            {ok, ssl_test_lib:ktls_set_cipher(Socket, OS, tx, 2) =:= ok}
        end
    after
        _ = gen_tcp:close(Socket),
        _ = gen_tcp:close(Listen)
    end.
