%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2022. All Rights Reserved.
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
-module(ssl_packet_SUITE).

-behaviour(ct_suite).

-include_lib("common_test/include/ct.hrl").

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
-export([packet_raw_passive_many_small/0,
         packet_raw_passive_many_small/1,
         packet_0_passive_many_small/0,
         packet_0_passive_many_small/1,
         packet_1_passive_many_small/0,
         packet_1_passive_many_small/1,
         packet_2_passive_many_small/0,
         packet_2_passive_many_small/1,
         packet_4_passive_many_small/0,
         packet_4_passive_many_small/1,
         packet_raw_passive_some_big/0,
         packet_raw_passive_some_big/1,
         packet_0_passive_some_big/0,
         packet_0_passive_some_big/1,
         packet_1_passive_some_big/0,
         packet_1_passive_some_big/1,
         packet_2_passive_some_big/0,
         packet_2_passive_some_big/1,
         packet_4_passive_some_big/0,
         packet_4_passive_some_big/1,
         packet_wait_passive/0,
         packet_wait_passive/1,
         packet_size_passive/0,
         packet_size_passive/1,
         packet_size_passive_setgetopts/0,
         packet_size_passive_setgetopts/1,
         header_decode_one_byte_passive/0,
         header_decode_one_byte_passive/1,
         header_decode_two_bytes_passive/0,
         header_decode_two_bytes_passive/1,
         header_decode_two_bytes_two_sent_passive/0,
         header_decode_two_bytes_two_sent_passive/1,
         header_decode_two_bytes_one_sent_passive/0,
         header_decode_two_bytes_one_sent_passive/1,
         packet_httph_passive/0,
         packet_httph_passive/1,
         packet_httph_bin_passive/0,
         packet_httph_bin_passive/1,
         packet_http_error_passive/0,
         packet_http_error_passive/1,
         packet_baddata_passive/0,
         packet_baddata_passive/1,
         packet_raw_active_once_many_small/0,
         packet_raw_active_once_many_small/1,
         packet_0_active_once_many_small/0,
         packet_0_active_once_many_small/1,
         packet_1_active_once_many_small/0,
         packet_1_active_once_many_small/1,
         packet_2_active_once_many_small/0,
         packet_2_active_once_many_small/1,
         packet_4_active_once_many_small/0,
         packet_4_active_once_many_small/1,
         packet_raw_active_once_some_big/0,
         packet_raw_active_once_some_big/1,
         packet_0_active_once_some_big/0,
         packet_0_active_once_some_big/1,
         packet_1_active_once_some_big/0,
         packet_1_active_once_some_big/1,
         packet_2_active_once_some_big/0,
         packet_2_active_once_some_big/1,
         packet_4_active_once_some_big/0,
         packet_4_active_once_some_big/1,
         packet_httph_active_once/0,
         packet_httph_active_once/1,
         packet_httph_bin_active_once/0,
         packet_httph_bin_active_once/1,
         packet_raw_active_many_small/0,
         packet_raw_active_many_small/1,
         packet_0_active_many_small/0,
         packet_0_active_many_small/1,
         packet_1_active_many_small/0,
         packet_1_active_many_small/1,
         packet_2_active_many_small/0,
         packet_2_active_many_small/1,
         packet_4_active_many_small/0,
         packet_4_active_many_small/1,
         packet_raw_active_some_big/0,
         packet_raw_active_some_big/1,
         packet_0_active_some_big/0,
         packet_0_active_some_big/1,
         packet_1_active_some_big/0,
         packet_1_active_some_big/1,
         packet_2_active_some_big/0,
         packet_2_active_some_big/1,
         packet_4_active_some_big/0,
         packet_4_active_some_big/1,
         packet_wait_active/0,
         packet_wait_active/1,
         packet_size_active/0,
         packet_size_active/1,
         packet_size_active_setoptsgetopts/0,
         packet_size_active_setoptsgetopts/1,
         packet_switch/0,
         packet_switch/1,
         header_decode_one_byte_active/0,
         header_decode_one_byte_active/1,
         header_decode_two_bytes_active/0,
         header_decode_two_bytes_active/1,
         header_decode_two_bytes_two_sent_active/0,
         header_decode_two_bytes_two_sent_active/1,
         header_decode_two_bytes_one_sent_active/0,
         header_decode_two_bytes_one_sent_active/1,
         packet_httph_active/0,
         packet_httph_active/1,
         packet_httph_bin_active/0,
         packet_httph_bin_active/1,
         packet_baddata_active/0,
         packet_baddata_active/1,
         packet_cdr_decode/0,
         packet_cdr_decode/1,
         packet_cdr_decode_list/0,
	 packet_cdr_decode_list/1,
         packet_http_decode/0,
         packet_http_decode/1,
         packet_http_decode_list/0,
         packet_http_decode_list/1,
         packet_http_bin_decode_multi/0,
         packet_http_bin_decode_multi/1,
         packet_line_decode/0,
         packet_line_decode/1,
         packet_line_decode_list/0,
         packet_line_decode_list/1,
         packet_asn1_decode/0,
         packet_asn1_decode/1,
         packet_asn1_decode_list/0,
	 packet_asn1_decode_list/1,
         packet_sunrm_decode/0,
         packet_sunrm_decode/1,
         packet_sunrm_decode_list/0,
         packet_sunrm_decode_list/1,
         packet_send_to_large/0,
         packet_send_to_large/1,
         packet_tpkt_decode/0,
         packet_tpkt_decode/1,
         packet_tpkt_decode_list/0,
         packet_tpkt_decode_list/1,
         reject_packet_opt/0,
         reject_packet_opt/1
        ]).

%% Apply export
-export([send_raw/3,
         passive_raw/3,
         passive_recv_packet/3,
         passive_recv_packet_size/3,
         send/3,
         send_incomplete/3,
         send_incomplete_after_client/3,
         active_once_raw/4,
         active_once_packet/3,
         active_raw/3,
         active_once_raw/3,
         active_packet/3,
         active_packet_size/3,
         assert_packet_opt/2,
         server_packet_decode/2,
         client_packet_decode/2,
         server_header_decode_active/3,
         client_header_decode_active/3,
         server_header_decode_passive/3,
         client_header_decode_passive/3,
         server_line_packet_decode/2,
         server_line_packet_decode/4,
         client_line_packet_decode/2,
         client_line_packet_decode/5,
         server_http_decode/2,
         client_http_decode/2,
         server_http_bin_decode/3,
         client_http_bin_decode/3,
         client_http_decode_list/2,
         server_http_decode_error/2,
         server_send_trailer/2,
         client_http_decode_trailer_active/1,
         client_http_decode_trailer_bin_active/1,
         client_http_decode_trailer_active_once/1,
         client_http_decode_trailer_bin_active_once/1,
         client_http_decode_trailer_passive/1,
         client_http_decode_trailer_bin_passive/1,
         add_tpkt_header/1,
         client_reject_packet_opt/2,
         send_switch_packet/3,
         recv_switch_packet/3
         ]).

-define(BYTE(X),     X:8/unsigned-big-integer).
-define(UINT16(X),   X:16/unsigned-big-integer).
-define(UINT24(X),   X:24/unsigned-big-integer).
-define(UINT32(X),   X:32/unsigned-big-integer).
-define(UINT64(X),   X:64/unsigned-big-integer).
-define(STRING(X),   ?UINT32((byte_size(X))), (X)/binary).

-define(byte(X),   << ?BYTE(X) >> ).
-define(uint16(X), << ?UINT16(X) >> ).
-define(uint24(X), << ?UINT24(X) >> ).
-define(uint32(X), << ?UINT32(X) >> ).
-define(uint64(X), << ?UINT64(X) >> ).

-define(MANY, 1000).
-define(SOME, 50).
-define(BASE_TIMEOUT_SECONDS, 20).
-define(SOME_SCALE, 2).
-define(MANY_SCALE, 3).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() -> 
    [
     {group, 'tlsv1.3'},
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}
    ].

groups() ->
    [{'tlsv1.3', [], socket_packet_tests() ++ protocol_packet_tests()},
     {'tlsv1.2', [], socket_packet_tests() ++ protocol_packet_tests()},
     {'tlsv1.1', [], socket_packet_tests() ++ protocol_packet_tests()},
     {'tlsv1', [], socket_packet_tests() ++ protocol_packet_tests()},
     %% We will not support any packet types if the transport is
     %% not reliable. We might support it for DTLS over SCTP in the future 
     {'dtlsv1.2', [], [reject_packet_opt]},
     {'dtlsv1', [],  [reject_packet_opt]}
    ].

socket_packet_tests() ->
    socket_active_packet_tests() ++ socket_active_once_packet_tests() ++ 
        socket_passive_packet_tests() ++ [packet_send_to_large, packet_tpkt_decode, packet_tpkt_decode_list].

protocol_packet_tests() ->
    protocol_active_packet_tests() ++ protocol_active_once_packet_tests() ++ protocol_passive_packet_tests() ++
	[packet_cdr_decode, packet_cdr_decode_list,
	 packet_http_decode, packet_http_decode_list,
	 packet_http_bin_decode_multi,
	 packet_line_decode, packet_line_decode_list,
	 packet_asn1_decode, packet_asn1_decode_list,
	 packet_sunrm_decode, packet_sunrm_decode_list].

socket_passive_packet_tests() ->
    [packet_raw_passive_many_small,
     packet_0_passive_many_small,
     packet_1_passive_many_small,
     packet_2_passive_many_small,
     packet_4_passive_many_small,
     packet_raw_passive_some_big,
     packet_0_passive_some_big,
     packet_1_passive_some_big,
     packet_2_passive_some_big,
     packet_4_passive_some_big,
     packet_wait_passive,
     packet_size_passive,
     packet_size_passive_setgetopts,
     %% inet header option should be deprecated!
     header_decode_one_byte_passive,
     header_decode_two_bytes_passive,
     header_decode_two_bytes_two_sent_passive,
     header_decode_two_bytes_one_sent_passive
    ].

protocol_passive_packet_tests() ->
    [packet_httph_passive,
     packet_httph_bin_passive,
     packet_http_error_passive,
     packet_baddata_passive
    ].

socket_active_once_packet_tests() ->
    [packet_raw_active_once_many_small,
     packet_0_active_once_many_small,
     packet_1_active_once_many_small,
     packet_2_active_once_many_small,
     packet_4_active_once_many_small,
     packet_raw_active_once_some_big,
     packet_0_active_once_some_big,
     packet_1_active_once_some_big,
     packet_2_active_once_some_big,
     packet_4_active_once_some_big
    ].

protocol_active_once_packet_tests() ->
    [
     packet_httph_active_once,
     packet_httph_bin_active_once
    ].

socket_active_packet_tests() ->
    [packet_raw_active_many_small,
     packet_0_active_many_small,
     packet_1_active_many_small,
     packet_2_active_many_small,
     packet_4_active_many_small,
     packet_raw_active_some_big,
     packet_0_active_some_big,
     packet_1_active_some_big,
     packet_2_active_some_big,
     packet_4_active_some_big,
     packet_wait_active,
     packet_size_active,
     packet_size_active_setoptsgetopts,
     packet_switch,
     %% inet header option should be deprecated!
     header_decode_one_byte_active,
     header_decode_two_bytes_active,
     header_decode_two_bytes_two_sent_active,
     header_decode_two_bytes_one_sent_active
    ].


protocol_active_packet_tests() ->
    [packet_httph_active,
     packet_httph_bin_active,
     packet_baddata_active
    ].

init_per_suite(Config) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    ssl_test_lib:clean_start(),
            ssl_test_lib:make_rsa_cert(Config)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group(GroupName, Config). 

end_per_group(GroupName, Config) ->
  ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(_TestCase, Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS}),
    Config.


end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

packet_raw_passive_many_small() ->
    [{doc,"Test packet option {packet, raw} in passive mode."}].

packet_raw_passive_many_small(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?MANY_SCALE}),
    Data = "Packet option is {packet, raw}",
    packet(Config, Data, send, passive_raw, ?MANY, raw, false).

%%--------------------------------------------------------------------

packet_raw_passive_some_big() ->
    [{doc,"Test packet option {packet, raw} in passive mode."}].

packet_raw_passive_some_big(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?SOME_SCALE}),
    Data = lists:append(lists:duplicate(100, "1234567890")),
    packet(Config, Data, send, passive_raw, ?SOME, raw, false).
%%--------------------------------------------------------------------
packet_0_passive_many_small() ->
    [{doc,"Test packet option {packet, 0} in passive mode."}].

packet_0_passive_many_small(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?MANY_SCALE}),
    Data = "Packet option is {packet, 0}, equivalent to packet raw.",
    packet(Config, Data, send, passive_raw, ?MANY, 0, false).

%%--------------------------------------------------------------------
packet_0_passive_some_big() ->
    [{doc,"Test packet option {packet, 0} in passive mode."}].

packet_0_passive_some_big(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?SOME_SCALE}),
    Data = lists:append(lists:duplicate(100, "1234567890")),
    packet(Config, Data, send, passive_raw, ?SOME, 0, false).

%%--------------------------------------------------------------------
packet_1_passive_many_small() ->
    [{doc,"Test packet option {packet, 1} in passive mode."}].

packet_1_passive_many_small(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?MANY_SCALE}),
    Data = "Packet option is {packet, 1}",
    packet(Config, Data, send, passive_recv_packet, ?MANY, 1, false).

%%--------------------------------------------------------------------
packet_1_passive_some_big() ->
    [{doc,"Test packet option {packet, 1} in passive mode."}].

packet_1_passive_some_big(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?SOME_SCALE}),
    Data = lists:append(lists:duplicate(255, "1")),
    packet(Config, Data, send, passive_recv_packet, ?SOME, 1, false).

%%--------------------------------------------------------------------
packet_2_passive_many_small() ->
    [{doc,"Test packet option {packet, 2} in passive mode"}].

packet_2_passive_many_small(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?MANY_SCALE}),
    Data = "Packet option is {packet, 2}",
    packet(Config, Data, send, passive_recv_packet, ?MANY, 2, false).

%%--------------------------------------------------------------------
packet_2_passive_some_big() ->
    [{doc,"Test packet option {packet, 2} in passive mode"}].

packet_2_passive_some_big(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?SOME_SCALE}),
    Data = lists:append(lists:duplicate(100, "1234567890")),
    packet(Config, Data, send, passive_recv_packet, ?SOME, 2, false).

%%--------------------------------------------------------------------
packet_4_passive_many_small() ->
    [{doc,"Test packet option {packet, 4} in passive mode"}].

packet_4_passive_many_small(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?MANY_SCALE}),
    Data = "Packet option is {packet, 4}",
    packet(Config, Data, send, passive_recv_packet, ?MANY, 4, false).

%%--------------------------------------------------------------------
packet_4_passive_some_big() ->
    [{doc,"Test packet option {packet, 4} in passive mode"}].

packet_4_passive_some_big(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?SOME_SCALE}),
    Data = lists:append(lists:duplicate(100, "1234567890")),
    packet(Config, Data, send, passive_recv_packet, ?SOME, 4, false).

%%--------------------------------------------------------------------
packet_raw_active_once_many_small() ->
    [{doc,"Test packet option {packet, raw} in active once mode."}].

packet_raw_active_once_many_small(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?MANY_SCALE}),
    Data = "Packet option is {packet, raw}",
    packet(Config, Data, send_raw, active_once_raw, ?MANY, raw, once).

%%--------------------------------------------------------------------
packet_raw_active_once_some_big() ->
    [{doc,"Test packet option {packet, raw} in active once mode."}].

packet_raw_active_once_some_big(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?SOME_SCALE}),
    Data = lists:append(lists:duplicate(100, "1234567890")),
    packet(Config, Data, send_raw, active_once_raw, ?SOME, raw, once).

%%--------------------------------------------------------------------
packet_0_active_once_many_small() ->
    [{doc,"Test packet option {packet, 0} in active once mode."}].

packet_0_active_once_many_small(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?MANY_SCALE}),
    Data = "Packet option is {packet, 0}",
    packet(Config, Data, send_raw, active_once_raw, ?MANY, 0, once).

%%--------------------------------------------------------------------
packet_0_active_once_some_big() ->
    [{doc,"Test packet option {packet, 0} in active once mode."}].

packet_0_active_once_some_big(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?SOME_SCALE}),
    Data = lists:append(lists:duplicate(100, "1234567890")),
    packet(Config, Data, send_raw, active_once_raw, ?SOME, 0, once).

%%--------------------------------------------------------------------
packet_1_active_once_many_small() ->
    [{doc,"Test packet option {packet, 1} in active once mode."}].

packet_1_active_once_many_small(Config) when is_list(Config) ->
    Data = "Packet option is {packet, 1}",
    packet(Config, Data, send, active_once_packet, ?MANY, 1, once).

%%--------------------------------------------------------------------
packet_1_active_once_some_big() ->
    [{doc,"Test packet option {packet, 1} in active once mode."}].

packet_1_active_once_some_big(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?SOME_SCALE}),
    Data = lists:append(lists:duplicate(255, "1")),
    packet(Config, Data, send, active_once_packet, ?SOME, 1, once).


%%--------------------------------------------------------------------
packet_2_active_once_many_small() ->
    [{doc,"Test packet option {packet, 2} in active once mode"}].

packet_2_active_once_many_small(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?MANY_SCALE}),
    Data = "Packet option is {packet, 2}",
    packet(Config, Data, send, active_once_packet, ?MANY, 2, once).

%%--------------------------------------------------------------------
packet_2_active_once_some_big() ->
    [{doc,"Test packet option {packet, 2} in active once mode"}].

packet_2_active_once_some_big(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?SOME_SCALE}),
    Data = lists:append(lists:duplicate(100, "1234567890")),
    packet(Config, Data, send, active_once_raw, ?SOME, 2, once).

%%--------------------------------------------------------------------
packet_4_active_once_many_small() ->
    [{doc,"Test packet option {packet, 4} in active once mode"}].

packet_4_active_once_many_small(Config) when is_list(Config) ->
    Data = "Packet option is {packet, 4}",
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?MANY_SCALE}),
    packet(Config, Data, send, active_once_packet, ?MANY, 4, once).

%%--------------------------------------------------------------------
packet_4_active_once_some_big() ->
    [{doc,"Test packet option {packet, 4} in active once mode"}].

packet_4_active_once_some_big(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?SOME_SCALE}),
    Data = lists:append(lists:duplicate(100, "1234567890")),
    packet(Config, Data, send, active_once_packet, ?SOME, 4, once).

%%--------------------------------------------------------------------
packet_raw_active_many_small() ->
    [{doc,"Test packet option {packet, raw} in active mode."}].

packet_raw_active_many_small(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?MANY_SCALE}),
    Data = "Packet option is {packet, raw}",
    packet(Config, Data, send_raw, active_raw, ?MANY, raw, true).

%%--------------------------------------------------------------------
packet_raw_active_some_big() ->
    [{doc,"Test packet option {packet, raw} in active mode."}].

packet_raw_active_some_big(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?SOME_SCALE}),
    Data = lists:append(lists:duplicate(100, "1234567890")),
    packet(Config, Data, send_raw, active_raw, ?SOME, raw, true).

%%--------------------------------------------------------------------
packet_0_active_many_small() ->
    [{doc,"Test packet option {packet, 0} in active mode."}].

packet_0_active_many_small(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?MANY_SCALE}),
    Data = "Packet option is {packet, 0}",
    packet(Config, Data, send_raw, active_raw, ?MANY, 0, true).

%%--------------------------------------------------------------------
packet_0_active_some_big() ->
    [{doc,"Test packet option {packet, 0} in active mode."}].

packet_0_active_some_big(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?SOME_SCALE}),
    Data = lists:append(lists:duplicate(100, "1234567890")),
    packet(Config, Data, send, active_raw, ?SOME, 0, true).

%%--------------------------------------------------------------------
packet_1_active_many_small() ->
    [{doc,"Test packet option {packet, 1} in active mode."}].

packet_1_active_many_small(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?MANY_SCALE}),
    Data = "Packet option is {packet, 1}",
    packet(Config, Data, send, active_packet, ?MANY, 1, true).

%%--------------------------------------------------------------------
packet_1_active_some_big() ->
    [{doc,"Test packet option {packet, 1} in active mode."}].

packet_1_active_some_big(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?SOME_SCALE}),
    Data = lists:append(lists:duplicate(255, "1")),
    packet(Config, Data, send, active_packet, ?SOME, 1, true).

%%--------------------------------------------------------------------
packet_2_active_many_small() ->
    [{doc,"Test packet option {packet, 2} in active mode"}].

packet_2_active_many_small(Config) when is_list(Config) ->
      ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?MANY_SCALE}),
    Data = "Packet option is {packet, 2}",
    packet(Config, Data, send, active_packet, ?MANY, 2, true).

%%--------------------------------------------------------------------
packet_2_active_some_big() ->
    [{doc,"Test packet option {packet, 2} in active mode"}].

packet_2_active_some_big(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?SOME_SCALE}),
    Data = lists:append(lists:duplicate(100, "1234567890")),
    packet(Config, Data, send, active_packet, ?SOME, 2, true).

%%--------------------------------------------------------------------
packet_4_active_many_small() ->
    [{doc,"Test packet option {packet, 4} in active mode"}].

packet_4_active_many_small(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?MANY_SCALE}),
    Data = "Packet option is {packet, 4}",
    packet(Config, Data, send, active_packet, ?MANY, 4, true).

%%--------------------------------------------------------------------
packet_4_active_some_big() ->
    [{doc,"Test packet option {packet, 4} in active mode"}].

packet_4_active_some_big(Config) when is_list(Config) ->
    ct:timetrap({seconds, ?BASE_TIMEOUT_SECONDS * ?SOME_SCALE}),
    Data = lists:append(lists:duplicate(100, "1234567890")),
    packet(Config, Data, send, active_packet, ?SOME, 4, true).

%%--------------------------------------------------------------------
packet_send_to_large() ->
    [{doc,"Test setting the packet option {packet, 2} on the send side"}].

packet_send_to_large(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = lists:append(lists:duplicate(30, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, send, [Data, 1]}},
					{options, [{packet, 1}| ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, active_packet, [Data, 1]}},
					{options, [{active, true} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, {error, {badarg, 
					       {packet_to_large, 300, 255}}}),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_wait_active() ->
    [{doc,"Test waiting when complete packages have not arrived"}].

packet_wait_active(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = list_to_binary(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, send_incomplete,
					       [Data, ?SOME]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, 
					 {?MODULE, active_packet,
					  [binary_to_list(Data), ?SOME]}},
					{options, [{active, true},
						   {packet, 4} |
						   ClientOpts]}]),

    ssl_test_lib:check_result(Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
packet_wait_passive() ->
    [{doc,"Test waiting when complete packages have not arrived"}].

packet_wait_passive(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = list_to_binary(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, send_incomplete,
					       [Data, ?SOME]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, passive_recv_packet,
					       [binary_to_list(Data), ?SOME]}},
					{options, [{active, false},
						   {packet, 4} |
						   ClientOpts]}]),

    ssl_test_lib:check_result(Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
packet_baddata_active() ->
    [{doc,"Test that if a bad packet arrives error msg is sent and socket is closed"}].

packet_baddata_active(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = list_to_binary(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, send_incomplete ,[Data, 1]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, active_packet, [Data, 1]}},
					{options, [{active, true},
						   {packet, cdr} |
						   ClientOpts]}]),
    receive
	{Client, {ssl_error, _, {invalid_packet, _}}} ->
            ok;
	Unexpected ->
	    ct:fail({unexpected, Unexpected})
    end,    


    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
packet_baddata_passive() ->
    [{doc,"Test that if a bad packet arrives error msg is sent and socket is closed"}].

packet_baddata_passive(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = list_to_binary(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, send_incomplete ,[Data, 1]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, passive_recv_packet,
					       [Data, 1]}},
					{options, [{active, false},
						   {packet, cdr} |
						   ClientOpts]}]),

    receive
	{Client, {error, {invalid_packet, _}}} ->
            ok;
	Unexpected ->
	    ct:fail({unexpected, Unexpected})
    end,    

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------

packet_size_active() ->
    [{doc,"Test that if a packet of size larger than
    packet_size arrives error msg is sent and socket is closed"}].

packet_size_active(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = list_to_binary(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, send_incomplete ,[Data, 1]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, active_packet, [Data, 1]}},
					{options, [{active, true},
						   {packet, 4}, {packet_size, 10} |
						   ClientOpts]}]),
    receive
	{Client, {ssl_error, _, {invalid_packet, _}}}->
            ok;
	Unexpected ->
	    ct:fail({unexpected, Unexpected})
    end, 

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_size_active_setoptsgetopts() ->
    [{doc, "Test using packet_size with setopts and getopts. Note that we need to synchronization",
      " with server as we want to test what happens to data sent after options changed at client side."
      " just changing this kind of option without some kind of synchronization in active mode does not make sense." }].

packet_size_active_setoptsgetopts(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = list_to_binary(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
                                        %% Send data after client changed packet opts.
					{mfa, {?MODULE, send_incomplete_after_client ,[Data, 1]}},
					{options, [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, active_packet_size,
					       [Data, 30]}},
					{options, [{active, true},
                                                   {packet, 4} |
						   ClientOpts]}]),
    receive
	{Client, {ssl_error, _, {invalid_packet, _}}} ->
            ok;
	Unexpected ->
	    ct:fail({unexpected, Unexpected})
    end,

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

packet_size_passive() ->
    [{doc, "Test that if a packet of size larger
    than packet_size arrives error msg is sent and socket is closed"}].

packet_size_passive(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = list_to_binary(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, send_incomplete ,[Data, 1]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, passive_recv_packet,
					       [Data, 1]}},
					{options, [{active, false},
						   {packet, 4}, {packet_size, 30} |
						   ClientOpts]}]),
    receive
	{Client, {error, {invalid_packet, _}}} ->
            ok;
	Unexpected ->
	    ct:fail({unexpected, Unexpected})
    end,

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
packet_size_passive_setgetopts() ->
    [{doc, "Test using packet_size with setopts and getopts"}].

packet_size_passive_setgetopts(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = list_to_binary(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, send_incomplete ,[Data, 1]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, passive_recv_packet_size,
					       [Data, 30]}},
					{options, [{active, false},
						   {packet, 4} |
						   ClientOpts]}]),
    receive
	{Client, {error, {invalid_packet, _}}} ->
            ok;
	Unexpected ->
	    ct:fail({unexpected, Unexpected})
    end,

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_switch() ->
    [{doc,"Test packet option {packet, 2} followed by {packet, 4}"}].

packet_switch(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, send_switch_packet ,["Hello World", 4]}},
					{options, [{nodelay, true},{packet, 2} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, recv_switch_packet, ["Hello World", 4]}},
					{options, [{nodelay, true}, {packet, 2} |
						   ClientOpts]}]),

    ssl_test_lib:check_result(Client, ok, Server, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
packet_cdr_decode() ->
    [{doc,"Test setting the packet option {packet, cdr}, {mode, binary}"}].
packet_cdr_decode(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    %% A valid cdr packet
    Data = <<71,73,79,80,1,2,2,1,0,0,0,41,0,0,0,0,0,0,0,0,0,0,0,1,78,
	     69,79,0,0,0,0,2,0,10,0,0,0,0,0,0,0,0,0,18,0,0,0,0,0,0,0,4,49>>,

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_packet_decode,
					       [Data]}},
					{options, [{active, true}, binary, 
						   {packet, cdr}|ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_packet_decode, 
					       [Data]}},
					{options, [{active, true}, {packet, cdr},
						   binary | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_cdr_decode_list() ->
    [{doc,"Test setting the packet option {packet, cdr} {mode, list}"}].
packet_cdr_decode_list(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    %% A valid cdr packet
    Data = [71,73,79,80,1,2,2,1,0,0,0,41,0,0,0,0,0,0,0,0,0,0,0,1,78,
	     69,79,0,0,0,0,2,0,10,0,0,0,0,0,0,0,0,0,18,0,0,0,0,0,0,0,4,49],

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_packet_decode,
					       [Data]}},
					{options, [{active, true}, list,
						   {packet, cdr}|ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_packet_decode, 
					       [Data]}},
					{options, [{active, true}, {packet, cdr},
						   list | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_http_decode() ->
    [{doc, "Test setting the packet option {packet, http} {mode, binary} "
     "(Body will be binary http strings are lists)"}].

packet_http_decode(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Request = "GET / HTTP/1.1\r\n"
              "host: www.example.com\r\n"
	      "user-agent: HttpTester\r\n"
	      "\r\n",
    Response = "HTTP/1.1 200 OK\r\n"
	       "\r\n"
	       "Hello!",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_http_decode, 
					       [Response]}},
					{options, [{active, true},binary,
						   {packet, http} | ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_http_decode, 
					       [Request]}},
					{options, [{active, true}, binary,
						   {packet, http} |
						   ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


server_http_decode(Socket, HttpResponse) ->
    assert_packet_opt(Socket, http),
    receive
	{ssl, Socket, {http_request, 'GET', _, {1,1}}}  -> ok;
	Other1 -> exit({?LINE, Other1})
    end,
    assert_packet_opt(Socket, http),
    receive
	{ssl, Socket, {http_header, _, 'Host', _, "www.example.com"}}  -> ok;
	Other2 -> exit({?LINE, Other2})
    end,
    assert_packet_opt(Socket, http),
    receive
	{ssl, Socket, {http_header, _, 'User-Agent', _, "HttpTester"}}  -> ok;
	Other3 -> exit({?LINE, Other3})
    end,
    assert_packet_opt(Socket, http),
    receive
	{ssl, Socket, http_eoh}  -> ok;
	Other4 -> exit({?LINE, Other4})
    end,
    assert_packet_opt(Socket, http),
    spawn(fun() -> ssl:send(Socket, HttpResponse) end),
    ok.

client_http_decode(Socket, HttpRequest) ->
    spawn(fun() -> ssl:send(Socket, HttpRequest) end),
    receive
	{ssl, Socket, {http_response, {1,1}, 200, "OK"}}  -> ok;
	Other1 -> exit({?LINE, Other1})
    end,
    receive
	{ssl, Socket, http_eoh}  -> ok;
	Other2 -> exit({?LINE, Other2})
    end,
    ok = ssl:setopts(Socket, [{packet, 0}]),
    receive
	{ssl, Socket, <<"Hello!">>}  -> ok;
	Other3 -> exit({?LINE, Other3})
    end,
    ok.

%%--------------------------------------------------------------------
packet_http_decode_list() ->
    [{doc, "Test setting the packet option {packet, http}, {mode, list}"
      "(Body will be list too)"}].
packet_http_decode_list(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Request = "GET / HTTP/1.1\r\n"
              "host: www.example.com\r\n"
	      "user-agent: HttpTester\r\n"
	      "\r\n",
    Response = "HTTP/1.1 200 OK\r\n"
	       "\r\n"
	       "Hello!",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_http_decode, 
					       [Response]}},
					{options, [{active, true}, binary, 
						   {packet, http} |
						   ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_http_decode_list, 
					       [Request]}},
					{options, [{active, true}, list, 
						   {packet, http} |
						   ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


client_http_decode_list(Socket, HttpRequest) ->
    spawn(fun() -> ssl:send(Socket, HttpRequest) end),
    receive
	{ssl, Socket, {http_response, {1,1}, 200, "OK"}}  -> ok;
	Other1 -> exit({?LINE, Other1})
    end,
    receive
	{ssl, Socket, http_eoh}  -> ok;
	Other2 -> exit({?LINE, Other2})
    end,
    ok = ssl:setopts(Socket, [{packet, 0}]),
    receive
	{ssl, Socket, "Hello!"}  -> ok;
	Other3 -> exit({?LINE, Other3})
    end,
    ok.

%%--------------------------------------------------------------------
packet_http_bin_decode_multi() ->
    [{doc,"Test setting the packet option {packet, http_bin} with multiple requests"}].
packet_http_bin_decode_multi(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Request = <<"GET / HTTP/1.1\r\n"
                "host: www.example.com\r\n"
	        "user-agent: HttpTester\r\n"
	        "\r\n">>,
    Response = <<"HTTP/1.1 200 OK\r\n"
	         "\r\n"
	         "Hello!">>,
    NumMsgs = 3,

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_http_bin_decode, 
					       [Response, NumMsgs]}},
					{options, [{active, true}, binary, 
						   {packet, http_bin} |
						   ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_http_bin_decode, 
					       [Request, NumMsgs]}},
					{options, [{active, true}, binary, 
						   {packet, http_bin} |
						   ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


server_http_bin_decode(Socket, HttpResponse, Count) when Count > 0 ->
    assert_packet_opt(Socket, http_bin),
    receive
	{ssl, Socket, {http_request, 'GET', _, {1,1}}}  -> ok;
	Other1 -> exit({?LINE, Other1})
    end,
    assert_packet_opt(Socket, http_bin),
    receive
	{ssl, Socket, {http_header, _, 'Host', _, <<"www.example.com">>}}  -> ok;
	Other2 -> exit({?LINE, Other2})
    end,
    assert_packet_opt(Socket, http_bin),
    receive
	{ssl, Socket, {http_header, _, 'User-Agent', _, <<"HttpTester">>}}  -> ok;
	Other3 -> exit({?LINE, Other3})
    end,
    assert_packet_opt(Socket, http_bin),
    receive
	{ssl, Socket, http_eoh}  -> ok;
	Other4 -> exit({?LINE, Other4})
    end,
    assert_packet_opt(Socket, http_bin),
    spawn(fun() -> ssl:send(Socket, HttpResponse) end),
    server_http_bin_decode(Socket, HttpResponse, Count - 1);
server_http_bin_decode(_, _, _) ->
    ok.

client_http_bin_decode(Socket, HttpRequest, Count) when Count > 0 ->
    spawn(fun() -> ssl:send(Socket, HttpRequest) end),
    receive
	{ssl, Socket, {http_response, {1,1}, 200, <<"OK">>}}  -> ok;
	Other1 -> exit({?LINE, Other1})
    end,
    receive
	{ssl, Socket, http_eoh}  -> ok;
	Other2 -> exit({?LINE, Other2})
    end,
    ok = ssl:setopts(Socket, [{packet, 0}]),
    receive
	{ssl, Socket, <<"Hello!">>}  -> ok;
	Other3 -> exit({?LINE, Other3})
    end,
    ok = ssl:setopts(Socket, [{packet, http_bin}]),
    client_http_bin_decode(Socket, HttpRequest, Count - 1);
client_http_bin_decode(_, _, _) ->
    ok.

%%--------------------------------------------------------------------
packet_http_error_passive() ->
    [{doc,"Test setting the packet option {packet, http}, {active, false}"
      " with a incorrect http header."}].

packet_http_error_passive(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Request = "GET / HTTP/1.1\r\n"
              "host: www.example.com\r\n"
	      "user-agent HttpTester\r\n"
	      "\r\n",
    Response = "HTTP/1.1 200 OK\r\n"
	       "\r\n"
	       "Hello!",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_http_decode_error,
					       [Response]}},
					{options, [{active, false}, binary,
						   {packet, http} |
						   ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_http_decode_list,
					       [Request]}},
					{options, [{active, true}, list,
						   {packet, http} |
						   ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


server_http_decode_error(Socket, HttpResponse) ->
    assert_packet_opt(Socket, http),

    {ok, {http_request, 'GET', _, {1,1}}} = ssl:recv(Socket, 0),

    assert_packet_opt(Socket, http),

    {ok, {http_header, _, 'Host', _, "www.example.com"}} = ssl:recv(Socket, 0),
    assert_packet_opt(Socket, http),

    {ok, {http_error, _}} = ssl:recv(Socket, 0),

    assert_packet_opt(Socket, http),

    {ok, http_eoh} = ssl:recv(Socket, 0),

    assert_packet_opt(Socket, http),
    spawn(fun() -> ssl:send(Socket, HttpResponse) end),
    ok.
%%--------------------------------------------------------------------
packet_httph_active() ->
    [{doc,"Test setting the packet option {packet, httph}"}].

packet_httph_active(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Trailer = "Content-Encoding: gzip\r\n"
	"\r\n",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_send_trailer,
					       [Trailer]}},
					{options, [{active, true}, binary |
						   ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_http_decode_trailer_active,
					       []}},
					{options, [{active, true},
						   {packet, httph},
						   list |
						   ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


server_send_trailer(Socket, Trailer)->
    ssl:send(Socket, Trailer),
    ok.

client_http_decode_trailer_active(Socket) ->
    receive
	{ssl, Socket,
	 {http_header,36,'Content-Encoding',"Content-Encoding","gzip"}} ->
	    ok;
	Other1 ->
	    exit({?LINE, Other1})
    end,
    receive
	{ssl, Socket, http_eoh}  ->
	    ok;
	Other2 ->
	    exit({?LINE, Other2})
    end,
    ok.

%%--------------------------------------------------------------------
packet_httph_bin_active() ->
    [{doc,"Test setting the packet option {packet, httph_bin}"}].
packet_httph_bin_active(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Trailer = "Content-Encoding: gzip\r\n"
	"\r\n",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_send_trailer,
					       [Trailer]}},
					{options, [{active, true}, binary |
						   ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_http_decode_trailer_bin_active,
					       []}},
					{options, [{active, true},
						   {packet, httph_bin},
						   list |
						   ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

client_http_decode_trailer_bin_active(Socket) ->
    receive
	{ssl, Socket,
	 {http_header,36,'Content-Encoding',<<"Content-Encoding">>, <<"gzip">>}} ->
	    ok;
	Other1 ->
	    exit({?LINE, Other1})
    end,
    receive
	{ssl, Socket, http_eoh}  ->
	    ok;
	Other2 ->
	    exit({?LINE, Other2})
    end,
    ok.
%%--------------------------------------------------------------------
packet_httph_active_once() ->
    [{doc,"Test setting the packet option {packet, httph}"}].

packet_httph_active_once(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Trailer = "Content-Encoding: gzip\r\n"
	"\r\n",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_send_trailer,
					       [Trailer]}},
					{options, [{active, true}, binary |
						   ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_http_decode_trailer_active_once,
					       []}},
					{options, [{active, false},
						   {packet, httph},
						   list |
						   ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


client_http_decode_trailer_active_once(Socket) ->
    ssl:setopts(Socket, [{active, once}]),
    receive
	{ssl, Socket,
	 {http_header,36,'Content-Encoding',"Content-Encoding","gzip"}} ->
	    ok;
	Other1 ->
	    exit({?LINE, Other1})
    end,
    ssl:setopts(Socket, [{active, once}]),
    receive
	{ssl, Socket, http_eoh}  ->
	    ok;
	Other2 ->
	    exit({?LINE, Other2})
    end,
    ok.
%%--------------------------------------------------------------------
packet_httph_bin_active_once() ->
    [{doc,"Test setting the packet option {packet, httph_bin}"}].

packet_httph_bin_active_once(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Trailer = "Content-Encoding: gzip\r\n"
	"\r\n",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_send_trailer,
					       [Trailer]}},
					{options, [{active, true}, binary |
						   ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_http_decode_trailer_bin_active_once,
					       []}},
					{options, [{active, false},
						   {packet, httph_bin},
						   list |
						   ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

client_http_decode_trailer_bin_active_once(Socket) ->
    ssl:setopts(Socket, [{active, once}]),
    receive
	{ssl, Socket,
	 {http_header,36,'Content-Encoding',<<"Content-Encoding">>, <<"gzip">>}} ->
	    ok;
	Other1 ->
	    exit({?LINE, Other1})
    end,
    ssl:setopts(Socket, [{active, once}]),
    receive
	{ssl, Socket, http_eoh}  ->
	    ok;
	Other2 ->
	    exit({?LINE, Other2})
    end,
    ok.

%%--------------------------------------------------------------------

packet_httph_passive() ->
    [{doc,"Test setting the packet option {packet, httph}"}].

packet_httph_passive(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Trailer = "Content-Encoding: gzip\r\n"
	"\r\n",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_send_trailer,
					       [Trailer]}},
					{options, [{active, true}, binary |
						   ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_http_decode_trailer_passive,
					       []}},
					{options, [{active, false},
						   {packet, httph},
						   list |
						   ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

client_http_decode_trailer_passive(Socket) ->
    {ok,{http_header,36,'Content-Encoding',"Content-Encoding","gzip"}} = ssl:recv(Socket, 0),
    {ok, http_eoh} = ssl:recv(Socket, 0),
    ok.

%%--------------------------------------------------------------------
packet_httph_bin_passive() ->
    [{doc,"Test setting the packet option {packet, httph_bin}"}].

packet_httph_bin_passive(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Trailer = "Content-Encoding: gzip\r\n"
	"\r\n",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_send_trailer,
					       [Trailer]}},
					{options, [{active, true}, binary |
						   ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_http_decode_trailer_bin_passive,
					       []}},
					{options, [{active, false},
						   {packet, httph_bin},
						   list |
						   ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

client_http_decode_trailer_bin_passive(Socket) ->
    {ok,{http_header,36,'Content-Encoding',<<"Content-Encoding">>,<<"gzip">>}} = ssl:recv(Socket, 0),
    {ok, http_eoh} = ssl:recv(Socket, 0),
    ok.

%%--------------------------------------------------------------------
packet_line_decode() ->
    [{doc,"Test setting the packet option {packet, line}, {mode, binary}"}].

packet_line_decode(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = list_to_binary(lists:flatten(io_lib:format("Line ends here.~n"
						      "Now it is a new line.~n", 
						      []))),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_line_packet_decode,
					       [Data]}},
					{options, [{active, true}, binary, 
						   {packet, line}|ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_line_packet_decode, 
					       [Data]}},
					{options, [{active, true}, 
						   {packet, line}, 
						   binary | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

packet_line_decode_list() ->
    [{doc,"Test setting the packet option {packet, line}, {mode, list}"}].

packet_line_decode_list(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = lists:flatten(io_lib:format("Line ends here.~n"
				       "Now it is a new line.~n", [])),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, 
					       server_line_packet_decode,
					       [Data]}},
					{options, [{active, true}, list, 
						   {packet, line}|ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, 
					       client_line_packet_decode, 
					       [Data]}},
					{options, [{active, true}, 
						   {packet, line}, 
						   list | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------

packet_asn1_decode() ->
    [{doc,"Test setting the packet option {packet, asn1}"}].

packet_asn1_decode(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    File = proplists:get_value(certfile, ServerOpts),

    %% A valid asn1 BER packet (DER is stricter BER)
    [{'Certificate', Data, _}] = ssl_test_lib:pem_to_der(File),
    
    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_packet_decode,
					       [Data]}},
					{options, [{active, true}, binary, 
						   {packet, asn1}|ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_packet_decode, 
					       [Data]}},
					{options, [{active, true}, {packet, asn1},
						   binary | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_asn1_decode_list() ->
    [{doc,"Test setting the packet option {packet, asn1}"}].

packet_asn1_decode_list(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    File = proplists:get_value(certfile, ServerOpts),

    %% A valid asn1 BER packet (DER is stricter BER)
    [{'Certificate', BinData, _}] = ssl_test_lib:pem_to_der(File),
    
    Data = binary_to_list(BinData),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_packet_decode,
					       [Data]}},
					{options, [{active, true}, list, 
						   {packet, asn1}|ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_packet_decode, 
					       [Data]}},
					{options, [{active, true}, {packet, asn1},
						   list | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_tpkt_decode() ->
    [{doc,"Test setting the packet option {packet, tpkt}"}].

packet_tpkt_decode(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = list_to_binary(add_tpkt_header("TPKT data")),
    
    
    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_packet_decode,
					       [Data]}},
					{options, [{active, true}, binary, 
						   {packet, tpkt}|ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_packet_decode, 
					       [Data]}},
					{options, [{active, true}, {packet, tpkt},
						   binary | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
packet_tpkt_decode_list() ->
    [{doc,"Test setting the packet option {packet, tpkt}"}].

packet_tpkt_decode_list(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = binary_to_list(list_to_binary(add_tpkt_header("TPKT data"))),
    
    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_packet_decode,
					       [Data]}},
					{options, [{active, true}, list, 
						   {packet, tpkt}|ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_packet_decode, 
					       [Data]}},
					{options, [{active, true}, {packet, tpkt},
						   list | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

%% packet_fcgi_decode() ->
%%     [{doc,"Test setting the packet option {packet, fcgi}"}].

%% packet_fcgi_decode(Config) when is_list(Config) ->
%%     ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
%%     ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
%%     {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
%%     Data = ...
    
%%     Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
%% 					{from, self()},
%% 					{mfa, {?MODULE, server_packet_decode,
%% 					       [Data0, Data1]}},
%% 					{options, [{active, true}, binary, 
%% 						   {packet, fcgi}|ServerOpts]}]),

%%     Port = ssl_test_lib:inet_port(Server),
%%     Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
%% 					{host, Hostname},
%% 					{from, self()},
%% 					{mfa, {?MODULE, client_packet_decode, 
%% 					       [Data0, Data1]}},
%% 					{options, [{active, true}, {packet, fcgi},
%% 						   binary | ClientOpts]}]),

%%     ssl_test_lib:check_result(Server, ok, Client, ok),

%%     ssl_test_lib:close(Server),
%%     ssl_test_lib:close(Client).


%%--------------------------------------------------------------------

packet_sunrm_decode() ->
    [{doc,"Test setting the packet option {packet, sunrm}"}].
packet_sunrm_decode(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = <<11:32, "Hello world">>,
    
    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_packet_decode,
					       [Data]}},
					{options, [{active, true}, binary, 
						   {packet, sunrm}|ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_packet_decode, 
					       [Data]}},
					{options, [{active, true}, {packet, sunrm},
						   binary | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_sunrm_decode_list() ->
    [{doc,"Test setting the packet option {packet, sunrm}"}].

packet_sunrm_decode_list(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = binary_to_list(list_to_binary([<<11:32>>, "Hello world"])),
    
    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_packet_decode,
					       [Data]}},
					{options, [{active, true}, list, 
						   {packet, sunrm}|ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_packet_decode, 
					       [Data]}},
					{options, [{active, true}, {packet, sunrm},
						   list | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------

header_decode_one_byte_active() ->
    [{doc,"Test setting the packet option {header, 1}"}].

header_decode_one_byte_active(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = <<11:8, "Hello world">>,
    
    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_header_decode_active,
					       [Data, [11 | <<"Hello world">>]]}},
					{options, [{active, true}, binary, 
						   {header,1}|ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_header_decode_active,
					       [Data, [11 | <<"Hello world">> ]]}},
					{options, [{active, true}, binary, {header, 1}
						    | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

header_decode_two_bytes_active() ->
    [{doc,"Test setting the packet option {header, 2}"}].

header_decode_two_bytes_active(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = <<11:8, "Hello world">>,
    
    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_header_decode_active,
					       [Data, [11, $H | <<"ello world">> ]]}},
					{options, [{active, true}, binary, 
						   {header,2}|ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_header_decode_active,
					       [Data, [11, $H | <<"ello world">> ]]}},
					{options, [{active, true}, {header, 2},
						   binary | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------

header_decode_two_bytes_two_sent_active() ->
    [{doc,"Test setting the packet option {header, 2} and sending two byte"}].

header_decode_two_bytes_two_sent_active(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = <<"He">>,
    
    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_header_decode_active,
					       [Data, [$H, $e | <<>>]]}},
					{options, [{active, true}, binary, 
						   {header,2}|ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_header_decode_active,
					       [Data, [$H, $e | <<>>]]}},
					{options, [{active, true}, {header, 2},
						   binary | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------

header_decode_two_bytes_one_sent_active() ->
    [{doc,"Test setting the packet option {header, 2} and sending one byte"}].

header_decode_two_bytes_one_sent_active(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = <<"H">>,
    
    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_header_decode_active,
					       [Data, "H"]}},
					{options, [{active, true}, binary, 
						   {header,2}|ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_header_decode_active,
					       [Data, "H"]}},
					{options, [{active, true}, {header, 2},
						   binary | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

header_decode_one_byte_passive() ->
    [{doc,"Test setting the packet option {header, 1}"}].

header_decode_one_byte_passive(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = <<11:8, "Hello world">>,

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_header_decode_passive,
					       [Data, [11 | <<"Hello world">>]]}},
					{options, [{active, false}, binary,
						   {header,1}|ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_header_decode_passive,
					       [Data, [11 | <<"Hello world">> ]]}},
					{options, [{active, false}, binary, {header, 1}
						   | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

header_decode_two_bytes_passive() ->
    [{doc,"Test setting the packet option {header, 2}"}].

header_decode_two_bytes_passive(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = <<11:8, "Hello world">>,

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_header_decode_passive,
					       [Data, [11, $H | <<"ello world">> ]]}},
					{options, [{active, false}, binary,
						   {header,2}|ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_header_decode_passive,
					       [Data, [11, $H | <<"ello world">> ]]}},
					{options, [{active, false}, {header, 2},
						   binary | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------

header_decode_two_bytes_two_sent_passive() ->
    [{doc,"Test setting the packet option {header, 2} and sending two byte"}].

header_decode_two_bytes_two_sent_passive(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = <<"He">>,

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_header_decode_passive,
					       [Data, [$H, $e | <<>>]]}},
					{options, [{active, false}, binary,
						   {header,2}|ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_header_decode_passive,
					       [Data, [$H, $e | <<>>]]}},
					{options, [{active, false}, {header, 2},
						   binary | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------

header_decode_two_bytes_one_sent_passive() ->
    [{doc,"Test setting the packet option {header, 2} and sending one byte"}].

header_decode_two_bytes_one_sent_passive(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = <<"H">>,

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, server_header_decode_passive,
					       [Data, "H"]}},
					{options, [{active, false}, binary,
						   {header,2}|ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, client_header_decode_passive,
					       [Data, "H"]}},
					{options, [{active, false}, {header, 2},
						   binary | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
reject_packet_opt() ->
    [{doc,"Test packet option is rejected for DTLS over udp"}].

reject_packet_opt(Config) when is_list(Config) ->

    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
       
    {error,{options,{not_supported,{packet,4}}}} = 
        ssl:listen(9999, [{packet, 4} | ServerOpts]),
    {error,{options,{not_supported,{packet_size,1}}}} =  
        ssl:listen(9999, [{packet_size, 1} | ServerOpts]),
    {error,{options,{not_supported,{header,1}}}} =
        ssl:listen(9999, [{header, 1} | ServerOpts]),
    
    client_reject_packet_opt(Config, {packet,4}),
    client_reject_packet_opt(Config, {packet_size, 1}),
    client_reject_packet_opt(Config, {header, 1}).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

packet(Config, Data, Send, Recv, Quantity, Packet, Active) when Packet == 0;
								Packet == raw ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, Send ,[Data, Quantity]}},
					{options, [{nodelay, true},{packet, Packet} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, Recv, [Data, Quantity]}},
					{options, [{active, Active}, {nodelay, true},
						   {packet, Packet} |
						   ClientOpts]}]),

    ssl_test_lib:check_result(Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client);

packet(Config, Data, Send, Recv, Quantity, Packet, Active) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, Send ,[Data, Quantity]}},
					{options, [{packet, Packet}, {nodelay, true}| ServerOpts]
                                         ++ ssl_test_lib:bigger_buffers()}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, Recv, [Data, Quantity]}},
					{options, [{active, Active},
                                                   {nodelay, true},
						   {packet, Packet} |
						   ClientOpts] ++ ssl_test_lib:bigger_buffers()}]),

    ssl_test_lib:check_result(Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

send_raw(Socket,_, 0) ->
    ssl:send(Socket, <<>>),
    no_result_msg;
send_raw(Socket, Data, N) ->
    ssl:send(Socket, Data),
    send_raw(Socket, Data, N-1).

passive_raw(Socket, _, 0) ->
    {error, timeout} = ssl:recv(Socket, 0, 500),
    ok;
passive_raw(Socket, Data, N) ->
    Length = length(Data),
    {ok, Data} = ssl:recv(Socket, Length),
    passive_raw(Socket, Data, N-1).

passive_recv_packet_size(Socket, Data, Size) ->
    ok = ssl:setopts(Socket, [{packet_size, Size}]),
    {ok, [{packet_size, Size}]} = ssl:getopts(Socket, [packet_size]),
    passive_recv_packet(Socket, Data, 1).

passive_recv_packet(Socket, _, 0) ->
    case ssl:recv(Socket, 0) of
	{ok, []} ->
	    {error, timeout} = ssl:recv(Socket, 0, 500),
	    ok;
	Other ->
	    {other, Other, ssl:connection_information(Socket, [session_id, cipher_suite]), 0}
    end;
passive_recv_packet(Socket, Data, N) ->
    case ssl:recv(Socket, 0, 10000) of
	{ok, Data} -> 
	    passive_recv_packet(Socket, Data, N-1);
        {error, _} = Other ->
            Other
    end.

send(Socket,_, 0) ->
    ssl:send(Socket, <<>>),
    no_result_msg;
send(Socket, Data, N) ->
    case ssl:send(Socket, [Data]) of
	ok ->
	    send(Socket, Data, N-1);
	Other ->
	    Other
    end.

send_incomplete(Socket, Data, N) ->
    send_incomplete(Socket, Data, N, <<>>).
send_incomplete(Socket, _Data, 0, Prev) ->
    ssl:send(Socket, Prev),
    ssl:send(Socket, [?uint32(0)]),
    no_result_msg;
send_incomplete(Socket, Data, N, Prev) ->
    Length = byte_size(Data),
    <<Part1:42/binary, Rest/binary>> = Data,
    ssl:send(Socket, [Prev, ?uint32(Length), Part1]),
    send_incomplete(Socket, Data, N-1, Rest).

send_incomplete_after_client(Socket, Data, N) ->
    _ = ssl:recv(Socket, 0),
    send_incomplete(Socket, Data, N).

active_once_raw(Socket, Data, N) ->
    active_once_raw(Socket, Data, N, []).

active_once_raw(_, _, 0, _) ->
    ok;
active_once_raw(Socket, Data, N, Acc0) ->
    case lists:prefix(Data, Acc0) of
	true ->
	    DLen = length(Data),   
	    Start = DLen + 1,
	    Len = length(Acc0) - DLen,    
	    Acc = string:substr(Acc0, Start, Len),   
	    active_once_raw(Socket, Data, N-1, Acc);
	false ->	    
	    receive 
		{ssl, Socket, Info}  ->
		    ssl:setopts(Socket, [{active, once}]),
		    active_once_raw(Socket, Data, N, Acc0 ++ Info)
	    end
    end.

active_once_packet(Socket,_, 0) ->
    receive
	{ssl, Socket, []} ->
	    ok;
	{ssl, Socket, Other} ->
	    {other, Other, ssl:connection_information(Socket,  [session_id, cipher_suite]), 0}
    end;
active_once_packet(Socket, Data, N) ->
    receive 	
	{ssl, Socket, Data} ->
            ssl:setopts(Socket, [{active, once}]),
            active_once_packet(Socket, Data, N-1)
    end.

active_raw(Socket, Data, N) ->
    active_raw(Socket, (length(Data) * N)).
active_raw(_Socket, 0) ->
    ok;
active_raw(Socket, N) ->
    receive 
	{ssl, Socket, Bytes} ->
            active_raw(Socket, N-length(Bytes))
    end.

active_packet(Socket, _, 0) ->
    receive
	{ssl, Socket, []} ->
	    ok;
	Other ->
	    {other, Other, ssl:connection_information(Socket,  [session_id, cipher_suite]), 0}
    end;
active_packet(Socket, Data, N) ->
    receive 
	{ssl, Socket, Data} ->
	    active_packet(Socket, Data, N-1);
	Other ->
	    Other
    end.

active_packet_size(Socket, Data, Size) ->
    ok = ssl:setopts(Socket, [{packet_size, Size}]),
    {ok, [{packet_size, Size}]} = ssl:getopts(Socket, [packet_size]),
    ssl:send(Socket, "start"),
    active_packet(Socket, Data, 1).


assert_packet_opt(Socket, Type) ->
    {ok, [{packet, Type}]} = ssl:getopts(Socket, [packet]).

server_packet_decode(Socket, Packet) ->
    receive
	{ssl, Socket, Packet}  -> ok;
	Other1 -> exit({?LINE, Other1})
    end,
    spawn(fun() -> ssl:send(Socket, Packet) end),
    receive
	{ssl, Socket, Packet}  -> ok;
	Other2 -> exit({?LINE, Other2})
    end,
    spawn(fun() -> ssl:send(Socket, Packet) end),
    ok.

client_packet_decode(Socket, Packet) when is_binary(Packet)->
    <<P1:10/binary, P2/binary>> = Packet,
    client_packet_decode(Socket, P1, P2, Packet);
client_packet_decode(Socket, [Head | Tail] = Packet) ->
    client_packet_decode(Socket, [Head], Tail, Packet).

client_packet_decode(Socket, P1, P2, Packet) ->
    spawn(fun() -> ssl:send(Socket, P1), ssl:send(Socket, P2)  end),
    receive
	{ssl, Socket, Packet}  -> ok;
	Other1 -> exit({?LINE, Other1})
    end,
    spawn(fun() -> ssl:send(Socket, Packet) end),
    receive
	{ssl, Socket, Packet}  -> ok;
	Other2 -> exit({?LINE, Other2})
    end.

server_header_decode_active(Socket, Packet, Result) ->
    receive
	{ssl, Socket, Result}  ->
	    ok;
	{ssl, Socket, Other1} ->
	    check_header_result(Result, Other1)
    end,
    spawn(fun() -> ssl:send(Socket, Packet) end),
    ok.

client_header_decode_active(Socket, Packet, Result) ->
    spawn(fun() -> ssl:send(Socket, Packet) end),
    receive
	{ssl, Socket, Result}  ->
	    ok;
	{ssl, Socket, Other1} ->
	    check_header_result(Result, Other1)
    end.

server_header_decode_passive(Socket, Packet, Result) ->
    case ssl:recv(Socket, 0) of
	{ok, Result} ->
	    ok;
	{ok, Other} ->
	    check_header_result(Result, Other)
    end,
    spawn(fun() -> ssl:send(Socket, Packet) end),
    ok.

client_header_decode_passive(Socket, Packet, Result) ->
    spawn(fun() -> ssl:send(Socket, Packet) end),
    case ssl:recv(Socket, 0) of
	{ok, Result} ->
	    ok;
	{ok, Other} ->
	    check_header_result(Result, Other)
    end.

%% The inet header option is a broken option as it does not buffer until it gets enough data.
%% This check only checks that it has the same behavior as inet, but it is a quite useless
%% option and the bitsynax makes it obsolete!
check_header_result([Byte1 | _], [Byte1]) ->
    ok;
check_header_result([Byte1 | _], [Byte1| <<>>]) ->
    ok;
check_header_result([Byte1, Byte2 | _], [Byte1, Byte2]) ->
    ok;
check_header_result([Byte1, Byte2 | _], [Byte1, Byte2 | <<>>]) ->
    ok;
check_header_result(Expected,Got) ->
    exit({?LINE, {Expected, Got}}).
    
server_line_packet_decode(Socket, Packet) when is_binary(Packet) ->
    [L1, L2] = string:tokens(binary_to_list(Packet), "\n"),
    server_line_packet_decode(Socket, list_to_binary(L1 ++ "\n"), list_to_binary(L2 ++ "\n"), Packet);
server_line_packet_decode(Socket, Packet) ->
    [L1, L2] = string:tokens(Packet, "\n"),
    server_line_packet_decode(Socket, L1 ++ "\n", L2 ++ "\n", Packet).

server_line_packet_decode(Socket, L1, L2, Packet) ->
    receive
  	{ssl, Socket,  L1} -> ok;
  	Other1 -> exit({?LINE, Other1})
    end,
    receive
  	{ssl, Socket,  L2} -> ok;
  	Other2 -> exit({?LINE, Other2})
    end,
    spawn(fun() -> ssl:send(Socket, Packet) end),
    ok.

client_line_packet_decode(Socket, Packet) when is_binary(Packet)->
    <<P1:10/binary, P2/binary>> = Packet,
    [L1, L2] = string:tokens(binary_to_list(Packet), "\n"),
    client_line_packet_decode(Socket, P1, P2, list_to_binary(L1 ++ "\n"), list_to_binary(L2 ++ "\n"));
client_line_packet_decode(Socket, [Head | Tail] = Packet) ->
    [L1, L2] = string:tokens(Packet, "\n"),
    client_line_packet_decode(Socket, [Head], Tail, L1 ++ "\n", L2 ++ "\n").

client_line_packet_decode(Socket, P1, P2, L1, L2) ->
    spawn(fun() -> ssl:send(Socket, P1),  ssl:send(Socket, P2) end),
    receive
  	{ssl, Socket, L1} -> ok;
  	Other1 -> exit({?LINE, Other1})
    end,
    receive
  	{ssl, Socket,  L2} -> ok;
  	Other2 -> exit({?LINE, Other2})
    end.

add_tpkt_header(Data) when is_binary(Data) ->
    L = byte_size(Data) + 4,
    [3, 0, ((L) bsr 8) band 16#ff, (L) band 16#ff ,Data];
add_tpkt_header(IOList) when is_list(IOList) ->
    Binary = list_to_binary(IOList),
    L = byte_size(Binary) + 4,
    [3, 0, ((L) bsr 8) band 16#ff, (L) band 16#ff , Binary].


client_reject_packet_opt(Config, PacketOpt) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, no_result_msg ,[]}},
                                        {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client_error([{node, ServerNode}, {port, Port},
                                              {host, Hostname},
                                              {from, self()},
                                              {mfa, {ssl_test_lib, no_result_msg, []}},
                                              {options, [PacketOpt |
                                                         ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, {error, {options, {not_supported, PacketOpt}}}).


send_switch_packet(SslSocket, Data, NextPacket) ->
    spawn(fun() -> ssl:send(SslSocket, Data) end),
    receive
        {ssl, SslSocket, "Hello World"} ->
            ssl:setopts(SslSocket, [{packet, NextPacket}]),
            spawn(fun() -> ssl:send(SslSocket, Data) end),
            receive 
                {ssl, SslSocket, "Hello World"} ->
                    ok
            end
    end.
recv_switch_packet(SslSocket, Data, NextPacket) ->
    receive
        {ssl, SslSocket, "Hello World"} ->
            spawn(fun() -> ssl:send(SslSocket, Data) end),
            ssl:setopts(SslSocket, [{packet, NextPacket}]),
            receive 
                {ssl, SslSocket, "Hello World"} ->
                    spawn(fun() -> ssl:send(SslSocket, Data) end),
                    ok                        
            end
    end.
