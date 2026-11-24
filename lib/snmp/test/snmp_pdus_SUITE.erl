%% 
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2003-2026. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose:
%%----------------------------------------------------------------------
-module(snmp_pdus_SUITE).


%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include("snmp_test_lib.hrl").
-define(SNMP_USE_V3, true).
-include_lib("snmp/include/snmp_types.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------

-export([
         suite/0, all/0, groups/0,
         init_per_suite/1,    end_per_suite/1,
         init_per_group/2,    end_per_group/2, 
         init_per_testcase/2, end_per_testcase/2,

	 otp7575/1,
	 otp8563/1, 
	 otp9022/1, 
	 otp10132/1,

         dec_value_int_size_limits/1,
         dec_pdu_int_size_limits/1,
         dec_v3_hdr_int_size_limits/1,
         dec_v3_hdr_value_checks/1,
         dec_usm_sec_params_int_size_limits/1,
         dec_oid_size_limits/1,
         dec_oid_bounded_time/1
	]).


%%======================================================================
%% Common Test interface functions
%%======================================================================

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    [{group, tickets},
     {group, ber_limits}].

groups() -> 
    [{tickets,    [], tickets_cases()},
     {ber_limits, [], ber_limits_cases()}].

tickets_cases() ->
    [
     otp7575,
     otp8563,
     otp9022,
     otp10132
    ].

ber_limits_cases() ->
    [
     dec_value_int_size_limits,
     dec_pdu_int_size_limits,
     dec_v3_hdr_int_size_limits,
     dec_v3_hdr_value_checks,
     dec_usm_sec_params_int_size_limits,
     dec_oid_size_limits,
     dec_oid_bounded_time
    ].


%%
%% -----
%%

init_per_suite(Config0) when is_list(Config0) ->
    ?IPRINT("init_per_suite -> entry with"
            "~n   Config: ~p", [Config0]),

    case ?LIB:init_per_suite(Config0) of
        {skip, _} = SKIP ->
            SKIP;

        Config1 when is_list(Config1) ->
            %% We need a monitor on this node also
            snmp_test_sys_monitor:start(),

            ?IPRINT("init_per_suite -> end when"
                    "~n      Config1: ~p", [Config1]),
            
            Config1
    end.

end_per_suite(Config0) when is_list(Config0) ->
    ?IPRINT("end_per_suite -> entry with"
            "~n   Config: ~p", [Config0]),

    snmp_test_sys_monitor:stop(),
    Config1 = ?LIB:end_per_suite(Config0),

    ?IPRINT("end_per_suite -> end"),

    Config1.



%%
%% -----
%%

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%%
%% -----
%%

init_per_testcase(_Case, Config) when is_list(Config) ->
    ?IPRINT("init_per_testcase -> entry with"
            "~n   Config: ~p", [Config]),

    snmp_test_global_sys_monitor:reset_events(),

    ?IPRINT("init_per_testcase -> end"),

    Config.

end_per_testcase(_Case, Config) when is_list(Config) ->

    ?IPRINT("end_per_testcase -> entry with"
            "~n   Config: ~p",
            [Config]),

    ?IPRINT("system events during test: ~p",
            [snmp_test_global_sys_monitor:events()]),

    Config.



%%======================================================================
%% Test functions
%%======================================================================

otp7575(suite) -> [];
otp7575(doc) -> ["OTP-7575 - Message version"];
otp7575(Config) when is_list(Config) ->
    ?IPRINT("attempt to decode message with valid version"),
    MsgWithOkVersion = <<48,39,2,1,0,4,6,112,117,98,108,105,99,160,26,2,2,1,49,2,1,0,2,1,0,48,14,48,12,6,8,43,6,1,2,1,1,5,0,5,0>>,
    case (catch dec_message(MsgWithOkVersion)) of
	Msg when is_record(Msg, message) ->
	    ok;
	Unexpected1 ->
	    exit({unexpected_decode_result, 1, Unexpected1})
    end,    

    ?IPRINT("attempt to decode message with bad version"),
    MsgWithBadVersion = <<48,48,2,10,1,1,1,1,1,1,1,1,1,1,4,6,112,117,98,108,105,99,160,26,2,2,1,49,2,1,0,2,1,0,48,14,48,12,6,8,43,6,1,2,1,1,5,0,5,0>>,
    case (catch dec_message(MsgWithBadVersion)) of
	{'EXIT', {bad_version, BadVersion}} when is_integer(BadVersion) ->
	    ok;
	Unexpected2 ->
	    exit({unexpected_decode_result, 2, Unexpected2})
    end,

    ?IPRINT("attempt to decode message with very bad version"),
    MsgWithVeryBadVersion = <<48,49,2,11,1,1,1,1,1,1,1,1,1,1,1,4,6,112,117,98,108,105,99,160,26,2,2,1,49,2,1,0,2,1,0,48,14,48,12,6,8,43,6,1,2,1,1,5,0,5,0>>,
    case (catch dec_message(MsgWithVeryBadVersion)) of
	{'EXIT', {bad_version, {VersionSize, MaxVersionSize}}} when (VersionSize > MaxVersionSize) ->
	    ok;
	Unexpected3 ->
	    exit({unexpected_decode_result, 3, Unexpected3})
    end,
    ?IPRINT("done"),
    ok.


otp8563(suite) -> [];
otp8563(doc) -> ["OTP-8563 - Counter64"];
otp8563(Config) when is_list(Config) ->
    Val1 = 16#7fffffffffffffff,
    ?IPRINT("try encode and decode value 1: ~w (0x~.16b)", [Val1, Val1]),
    Enc1 = snmp_pdus:enc_value('Counter64', Val1), 
    ?IPRINT("  => ~w", [Enc1]),
    {{'Counter64', Val1}, []} = snmp_pdus:dec_value(Enc1), 

    Val2 = Val1 + 1,
    ?IPRINT("try encode and decode value 2: ~w (0x~.16b)", [Val2, Val2]),
    Enc2 = snmp_pdus:enc_value('Counter64', Val2), 
    ?IPRINT("  => ~w", [Enc2]),
    {{'Counter64', Val2}, []} = snmp_pdus:dec_value(Enc2), 

    Val3 = Val2 + 1,
    ?IPRINT("try encode and decode value 3: ~w (0x~.16b)", [Val3, Val3]),
    Enc3 = snmp_pdus:enc_value('Counter64', Val3), 
    ?IPRINT("  => ~w", [Enc3]),
    {{'Counter64', Val3}, []} = snmp_pdus:dec_value(Enc3), 

    Val4 = 16#fffffffffffffffe,
    ?IPRINT("try encode and decode value 4: ~w (0x~.16b)", [Val4, Val4]),
    Enc4 = snmp_pdus:enc_value('Counter64', Val4), 
    ?IPRINT("  => ~w", [Enc4]),
    {{'Counter64', Val4}, []} = snmp_pdus:dec_value(Enc4), 

    Val5 = Val4 + 1,
    ?IPRINT("try encode and decode value 5: ~w (0x~.16b)", [Val5, Val5]),
    Enc5 = snmp_pdus:enc_value('Counter64', Val5), 
    ?IPRINT("  => ~w", [Enc5]),
    {{'Counter64', Val5}, []} = snmp_pdus:dec_value(Enc5), 

    Val6 = 16#ffffffffffffffff + 1, 
    ?IPRINT("try and fail to encode value 6: ~w (0x~.16b)", [Val6, Val6]),
    case (catch snmp_pdus:enc_value('Counter64', Val6)) of
	{'EXIT', {error, {bad_counter64, Val6}}} ->
	    ok;
	Unexpected6 ->
	    ?IPRINT("  => ~w", [Unexpected6]),
	    exit({unexpected_encode_result, Unexpected6, Val6})
    end,

    Val7 = -1, 
    ?IPRINT("try and fail to encode value 7: ~w", [Val7]),
    case (catch snmp_pdus:enc_value('Counter64', Val7)) of
	{'EXIT', {error, {bad_counter64, Val7}}} ->
	    ok;
	Unexpected7 ->
	    ?IPRINT("  => ~w", [Unexpected7]),
	    exit({unexpected_encode_result, Unexpected7, Val7})
    end,

    ok.


otp9022(suite) -> [];
otp9022(doc) -> ["OTP-9022 - Counter32"];
otp9022(Config) when is_list(Config) ->
    Val0 = 2908389204, 
    ?IPRINT("try encode and decode value 0: ~w (0x~.16b)", [Val0, Val0]),
    Enc0 = snmp_pdus:enc_value('Counter32', Val0), 
    ?IPRINT("  => ~w", [Enc0]),
    {{'Counter32', Val0}, []} = snmp_pdus:dec_value(Enc0), 

    Val1 = 0, 
    ?IPRINT("try encode and decode value 1: ~w (0x~.16b)", [Val1, Val1]),
    Enc1 = snmp_pdus:enc_value('Counter32', Val1), 
    ?IPRINT("  => ~w", [Enc1]),
    {{'Counter32', Val1}, []} = snmp_pdus:dec_value(Enc1), 

    Val2 = Val1 + 1,
    ?IPRINT("try encode and decode value 2: ~w (0x~.16b)", [Val2, Val2]),
    Enc2 = snmp_pdus:enc_value('Counter32', Val2), 
    ?IPRINT("  => ~w", [Enc2]),
    {{'Counter32', Val2}, []} = snmp_pdus:dec_value(Enc2), 

    Val3 = 16#7ffffffe,
    ?IPRINT("try encode and decode value 3: ~w (0x~.16b)", [Val3, Val3]),
    Enc3 = snmp_pdus:enc_value('Counter32', Val3), 
    ?IPRINT("  => ~w", [Enc3]),
    {{'Counter32', Val3}, []} = snmp_pdus:dec_value(Enc3), 

    Val4 = Val3 + 1,
    ?IPRINT("try encode and decode value 4: ~w (0x~.16b)", [Val4, Val4]),
    Enc4 = snmp_pdus:enc_value('Counter32', Val4), 
    ?IPRINT("  => ~w", [Enc4]),
    {{'Counter32', Val4}, []} = snmp_pdus:dec_value(Enc4), 

    Val5 = Val4 + 1,
    ?IPRINT("try encode and decode value 5: ~w (0x~.16b)", [Val5, Val5]),
    Enc5 = snmp_pdus:enc_value('Counter32', Val5), 
    ?IPRINT("  => ~w", [Enc5]),
    {{'Counter32', Val5}, []} = snmp_pdus:dec_value(Enc5), 

    Val6 = 16#fffffffe,
    ?IPRINT("try encode and decode value 6: ~w (0x~.16b)", [Val6, Val6]),
    Enc6 = snmp_pdus:enc_value('Counter32', Val6), 
    ?IPRINT("  => ~w", [Enc6]),
    {{'Counter32', Val6}, []} = snmp_pdus:dec_value(Enc6), 

    Val7 = Val6 + 1,
    ?IPRINT("try encode and decode value 7: ~w (0x~.16b)", [Val7, Val7]),
    Enc7 = snmp_pdus:enc_value('Counter32', Val7), 
    ?IPRINT("  => ~w", [Enc7]),
    {{'Counter32', Val7}, []} = snmp_pdus:dec_value(Enc7), 

    Val8 = 16#ffffffff + 1, 
    ?IPRINT("try and fail to encode value 8: ~w (0x~.16b)", [Val8, Val8]),
    case (catch snmp_pdus:enc_value('Counter32', Val8)) of
	{'EXIT', {error, {bad_counter32, Val8}}} ->
	    ok;
	Unexpected8 ->
	    ?IPRINT("  => ~w", [Unexpected8]),
	    exit({unexpected_encode_result, Unexpected8, Val8})
    end,

    Val9 = -1, 
    ?IPRINT("try and fail to encode value 9: ~w", [Val9]),
    case (catch snmp_pdus:enc_value('Counter32', Val9)) of
	{'EXIT', {error, {bad_counter32, Val9}}} ->
	    ok;
	Unexpected9 ->
	    ?IPRINT("  => ~w", [Unexpected9]),
	    exit({unexpected_encode_result, Unexpected9, Val9})
    end,

    ok.


otp10132(suite) -> [];
otp10132(doc) -> ["OTP-10132 - TimeTicks"];
otp10132(Config) when is_list(Config) ->
    Val0 = 2159001034, 
    ?IPRINT("try encode and decode value 0: ~w (0x~.16b)", [Val0, Val0]),
    Enc0 = snmp_pdus:enc_value('TimeTicks', Val0), 
    ?IPRINT("  => ~w", [Enc0]),
    {{'TimeTicks', Val0}, []} = snmp_pdus:dec_value(Enc0), 

    Val1 = 0, 
    ?IPRINT("try encode and decode value 1: ~w (0x~.16b)", [Val1, Val1]),
    Enc1 = snmp_pdus:enc_value('TimeTicks', Val1), 
    ?IPRINT("  => ~w", [Enc1]),
    {{'TimeTicks', Val1}, []} = snmp_pdus:dec_value(Enc1), 

    Val2 = Val1 + 1, 
    ?IPRINT("try encode and decode value 2: ~w (0x~.16b)", [Val2, Val2]),
    Enc2 = snmp_pdus:enc_value('TimeTicks', Val2), 
    ?IPRINT("  => ~w", [Enc2]),
    {{'TimeTicks', Val2}, []} = snmp_pdus:dec_value(Enc2), 

    Val3 = 16#7ffffffe,
    ?IPRINT("try encode and decode value 3: ~w (0x~.16b)", [Val3, Val3]),
    Enc3 = snmp_pdus:enc_value('TimeTicks', Val3), 
    ?IPRINT("  => ~w", [Enc3]),
    {{'TimeTicks', Val3}, []} = snmp_pdus:dec_value(Enc3), 

    Val4 = Val3 + 1,
    ?IPRINT("try encode and decode value 4: ~w (0x~.16b)", [Val4, Val4]),
    Enc4 = snmp_pdus:enc_value('TimeTicks', Val4), 
    ?IPRINT("  => ~w", [Enc4]),
    {{'TimeTicks', Val4}, []} = snmp_pdus:dec_value(Enc4), 

    Val5 = Val4 + 1,
    ?IPRINT("try encode and decode value 5: ~w (0x~.16b)", [Val5, Val5]),
    Enc5 = snmp_pdus:enc_value('TimeTicks', Val5), 
    ?IPRINT("  => ~w", [Enc5]),
    {{'TimeTicks', Val5}, []} = snmp_pdus:dec_value(Enc5), 

    Val6 = 16#fffffffe,
    ?IPRINT("try encode and decode value 6: ~w (0x~.16b)", [Val6, Val6]),
    Enc6 = snmp_pdus:enc_value('TimeTicks', Val6), 
    ?IPRINT("  => ~w", [Enc6]),
    {{'TimeTicks', Val6}, []} = snmp_pdus:dec_value(Enc6), 

    Val7 = Val6 + 1,
    ?IPRINT("try encode and decode value 7: ~w (0x~.16b)", [Val7, Val7]),
    Enc7 = snmp_pdus:enc_value('TimeTicks', Val7), 
    ?IPRINT("  => ~w", [Enc7]),
    {{'TimeTicks', Val7}, []} = snmp_pdus:dec_value(Enc7), 

    Val8 = Val7 + 1, 
    ?IPRINT("try and fail to encode value 8: ~w (0x~.16b)", [Val8, Val8]),
    case (catch snmp_pdus:enc_value('TimeTicks', Val8)) of
	{'EXIT', {error, {bad_timeticks, Val8}}} ->
	    ok;
	Unexpected8 ->
	    ?IPRINT("  => ~w", [Unexpected8]),
	    exit({unexpected_encode_result, Unexpected8, Val8})
    end,

    Val9 = -1, 
    ?IPRINT("try and fail to encode value 9: ~w", [Val9]),
    case (catch snmp_pdus:enc_value('TimeTicks', Val9)) of
	{'EXIT', {error, {bad_timeticks, Val9}}} ->
	    ok;
	Unexpected9 ->
	    ?IPRINT("  => ~w", [Unexpected9]),
	    exit({unexpected_encode_result, Unexpected9, Val9})
    end,

    ?IPRINT("done"),
    ok.


%%======================================================================
%% BER integer content octet limits
%%======================================================================

%% snmp_pdus caps the number of BER content octets accepted for every
%% integer-valued field (?INT32_MAX_OCTETS and friends). The cases below
%% pin, for each field, the largest accepted encoding and the rejection of
%% one octet more.
%%
%% They also pin that the rejection is an *exit*. It used to be a throw,
%% which slipped through the (catch ...) wrappers in snmpa_mpd -- e.g.
%% around dec_scoped_pdu/1 in v3_proc/6 -- and produced a case_clause
%% instead of a discarded packet. dec_class/1 reports the exception class,
%% so a regression back to throw fails the case instead of passing
%% silently.

dec_value_int_size_limits(suite) -> [];
dec_value_int_size_limits(doc) ->
    ["Content octet limits for the integer types decoded by dec_value/1"];
dec_value_int_size_limits(Config) when is_list(Config) ->
    %% {Type, BER tag, max content octets}. The unsigned types allow one
    %% octet more than their width, for the leading zero that keeps the
    %% value positive.
    Types = [{'INTEGER',    2,  4},
             {'Counter32',  65, 5},
             {'Unsigned32', 66, 5},
             {'TimeTicks',  67, 5},
             {'Counter64',  70, 9}],
    lists:foreach(
      fun({Type, Tag, Max}) ->
              ?IPRINT("~w: ~w content octets is accepted", [Type, Max]),
              case snmp_pdus:dec_value(padded_value(Tag, Max)) of
                  {{Type, 1}, []} ->
                      ok;
                  UnexpectedOk ->
                      exit({unexpected_decode_result, Type, Max, UnexpectedOk})
              end,

              ?IPRINT("~w: ~w content octets is rejected", [Type, Max + 1]),
              case dec_class(fun() ->
                                     snmp_pdus:dec_value(
                                       padded_value(Tag, Max + 1))
                             end) of
                  {exit, {error, {bad_integer, {Bad, Max}}}} when Bad =:= Max + 1 ->
                      ok;
                  Unexpected ->
                      exit({unexpected_decode_result, Type, Max + 1, Unexpected})
              end
      end, Types),
    ?IPRINT("done"),
    ok.


dec_pdu_int_size_limits(suite) -> [];
dec_pdu_int_size_limits(doc) ->
    ["Content octet limits for the integer fields of PDU, BulkPDU and TrapPDU"];
dec_pdu_int_size_limits(Config) when is_list(Config) ->
    %% RFC 3416: request-id is INTEGER (-2147483648..2147483647) and
    %% error-index, non-repeaters and max-repetitions are
    %% INTEGER (0..max-bindings) with max-bindings = 2147483647 -- so four
    %% content octets each.
    ?IPRINT("get-request: a well formed PDU decodes"),
    #pdu{type         = 'get-request',
         request_id   = 1,
         error_status = noError,
         error_index  = 0,
         varbinds     = []} =
        snmp_pdus:dec_pdu(pdu(160, [int_tlv(1, 4),
                                    int_tlv(0, 1),
                                    int_tlv(0, 1)])),

    ?IPRINT("get-request: request-id with 5 content octets is rejected"),
    expect_bad_integer(request_id, {5, 4},
                       fun() ->
                               snmp_pdus:dec_pdu(pdu(160, [int_tlv(1, 5),
                                                           int_tlv(0, 1),
                                                           int_tlv(0, 1)]))
                       end),

    %% error-status is an enumerated INTEGER (noError(0)..inconsistentName(18)),
    %% so one content octet carries every legal value. The exact cap is not
    %% pinned here -- only that a grossly oversized encoding is rejected --
    %% because unrecognised values are deliberately passed through by
    %% dec_pdu/1 and the cap may be tightened without changing behaviour.
    ?IPRINT("get-request: error-status with 8 content octets is rejected"),
    case dec_class(fun() ->
                           snmp_pdus:dec_pdu(pdu(160, [int_tlv(1, 1),
                                                       int_tlv(0, 8),
                                                       int_tlv(0, 1)]))
                   end) of
        {exit, {error, {bad_integer, {8, _}}}} ->
            ok;
        UnexpectedErrStatus ->
            exit({unexpected_decode_result, error_status, UnexpectedErrStatus})
    end,

    ?IPRINT("get-request: error-index with 5 content octets is rejected"),
    expect_bad_integer(error_index, {5, 4},
                       fun() ->
                               snmp_pdus:dec_pdu(pdu(160, [int_tlv(1, 1),
                                                           int_tlv(0, 1),
                                                           int_tlv(0, 5)]))
                       end),

    ?IPRINT("get-bulk-request: a well formed PDU decodes"),
    #pdu{type         = 'get-bulk-request',
         request_id   = 1,
         error_status = 0,
         error_index  = 10,
         varbinds     = []} =
        snmp_pdus:dec_pdu(pdu(165, [int_tlv(1, 4),
                                    int_tlv(0, 1),
                                    int_tlv(10, 1)])),

    ?IPRINT("get-bulk-request: non-repeaters with 5 content octets is rejected"),
    expect_bad_integer(non_repeaters, {5, 4},
                       fun() ->
                               snmp_pdus:dec_pdu(pdu(165, [int_tlv(1, 1),
                                                           int_tlv(0, 5),
                                                           int_tlv(0, 1)]))
                       end),

    ?IPRINT("get-bulk-request: max-repetitions with 5 content octets is rejected"),
    expect_bad_integer(max_repetitions, {5, 4},
                       fun() ->
                               snmp_pdus:dec_pdu(pdu(165, [int_tlv(1, 1),
                                                           int_tlv(0, 1),
                                                           int_tlv(0, 5)]))
                       end),

    ?IPRINT("trap: a well formed TrapPDU decodes"),
    #trappdu{generic_trap  = 6,
             specific_trap = 1,
             time_stamp    = 0,
             varbinds      = []} =
        snmp_pdus:dec_pdu(trap_pdu(int_tlv(6, 1), int_tlv(1, 4))),

    ?IPRINT("trap: generic-trap with 5 content octets is rejected"),
    expect_bad_integer(generic_trap, {5, 4},
                       fun() ->
                               snmp_pdus:dec_pdu(
                                 trap_pdu(int_tlv(6, 5), int_tlv(1, 1)))
                       end),

    ?IPRINT("trap: specific-trap with 5 content octets is rejected"),
    expect_bad_integer(specific_trap, {5, 4},
                       fun() ->
                               snmp_pdus:dec_pdu(
                                 trap_pdu(int_tlv(6, 1), int_tlv(1, 5)))
                       end),

    ?IPRINT("done"),
    ok.


dec_v3_hdr_int_size_limits(suite) -> [];
dec_v3_hdr_int_size_limits(doc) ->
    ["Content octet limits for the v3 header data integer fields"];
dec_v3_hdr_int_size_limits(Config) when is_list(Config) ->
    %% RFC 3412: msgID and msgMaxSize are INTEGER (0..2147483647) and
    %% msgSecurityModel is INTEGER (1..2147483647) -- four content octets.
    ?IPRINT("the largest legal msgID is accepted"),
    #message{version = 'version-3',
             vsn_hdr = #v3_hdr{msgID            = 2147483647,
                               msgMaxSize       = 484,
                               msgSecurityModel = 3}} =
        snmp_pdus:dec_message_only(v3_msg([raw_int_tlv([127, 255, 255, 255]),
                                           v3_msg_max_size_tlv(),
                                           v3_msg_flags_tlv(),
                                           v3_sec_model_tlv()])),

    ?IPRINT("msgID with 5 content octets is rejected"),
    expect_exit(msg_id, {bad_msg_id_bytesize, {5, 4}},
                fun() ->
                        snmp_pdus:dec_message_only(
                          v3_msg([int_tlv(1, 5),
                                  v3_msg_max_size_tlv(),
                                  v3_msg_flags_tlv(),
                                  v3_sec_model_tlv()]))
                end),

    ?IPRINT("msgMaxSize with 5 content octets is rejected"),
    expect_exit(msg_max_size, {bad_msg_max_size_bytesize, {5, 4}},
                fun() ->
                        snmp_pdus:dec_message_only(
                          v3_msg([int_tlv(1, 1),
                                  int_tlv(1, 5),
                                  v3_msg_flags_tlv(),
                                  v3_sec_model_tlv()]))
                end),

    ?IPRINT("msgSecurityModel with 5 content octets is rejected"),
    expect_exit(msg_sec_model, {bad_sec_model_bytesize, {5, 4}},
                fun() ->
                        snmp_pdus:dec_message_only(
                          v3_msg([int_tlv(1, 1),
                                  v3_msg_max_size_tlv(),
                                  v3_msg_flags_tlv(),
                                  int_tlv(1, 5)]))
                end),

    ?IPRINT("done"),
    ok.


dec_v3_hdr_value_checks(suite) -> [];
dec_v3_hdr_value_checks(doc) ->
    ["Range checks on the v3 header data integer fields are applied in "
     "addition to the content octet limits"];
dec_v3_hdr_value_checks(Config) when is_list(Config) ->
    %% A content octet limit is not a range check: four octets still encode
    %% 16#80000000, and one octet still encodes a msgMaxSize below the
    %% RFC 3412 floor of 484. chk_msg_id/1, chk_msg_max_size/1 and
    %% chk_msg_sec_model/1 must keep rejecting those.
    ?IPRINT("a negative msgID is rejected"),
    expect_exit(msg_id, {bad_msg_id, -2147483648},
                fun() ->
                        snmp_pdus:dec_message_only(
                          v3_msg([raw_int_tlv([128, 0, 0, 0]),
                                  v3_msg_max_size_tlv(),
                                  v3_msg_flags_tlv(),
                                  v3_sec_model_tlv()]))
                end),

    ?IPRINT("a msgMaxSize below 484 is rejected"),
    expect_exit(msg_max_size, {bad_msg_max_size, 100},
                fun() ->
                        snmp_pdus:dec_message_only(
                          v3_msg([int_tlv(1, 1),
                                  int_tlv(100, 1),
                                  v3_msg_flags_tlv(),
                                  v3_sec_model_tlv()]))
                end),

    ?IPRINT("a negative msgSecurityModel is rejected"),
    expect_exit(msg_sec_model, {bad_msg_sec_model, -1},
                fun() ->
                        snmp_pdus:dec_message_only(
                          v3_msg([int_tlv(1, 1),
                                  v3_msg_max_size_tlv(),
                                  v3_msg_flags_tlv(),
                                  raw_int_tlv([255])]))
                end),

    ?IPRINT("done"),
    ok.


dec_usm_sec_params_int_size_limits(suite) -> [];
dec_usm_sec_params_int_size_limits(doc) ->
    ["Content octet limits for msgAuthoritativeEngineBoots and "
     "msgAuthoritativeEngineTime"];
dec_usm_sec_params_int_size_limits(Config) when is_list(Config) ->
    %% RFC 3414: both fields are INTEGER (0..2147483647) -- four content
    %% octets. They are parsed before the message is authenticated, so an
    %% unbounded encoding here is reachable pre-auth.
    ?IPRINT("well formed security parameters decode"),
    #usmSecurityParameters{msgAuthoritativeEngineID    = "",
                           msgAuthoritativeEngineBoots = 1,
                           msgAuthoritativeEngineTime  = 2} =
        snmp_pdus:dec_usm_security_parameters(usm_params(int_tlv(1, 4),
                                                        int_tlv(2, 4))),

    ?IPRINT("msgAuthoritativeEngineBoots with 5 content octets is rejected"),
    expect_exit(engine_boots, {bad_engine_boots_bytesize, {5, 4}},
                fun() ->
                        snmp_pdus:dec_usm_security_parameters(
                          usm_params(int_tlv(1, 5), int_tlv(2, 1)))
                end),

    ?IPRINT("msgAuthoritativeEngineTime with 5 content octets is rejected"),
    expect_exit(engine_time, {bad_engine_time_bytesize, {5, 4}},
                fun() ->
                        snmp_pdus:dec_usm_security_parameters(
                          usm_params(int_tlv(1, 1), int_tlv(2, 5)))
                end),

    ?IPRINT("done"),
    ok.


%%======================================================================
%% OBJECT IDENTIFIER limits
%%======================================================================

%% RFC 2578 #3.5: "there are at most 128 sub-identifiers in a value, and
%% each sub-identifier has a maximum value of 2^32-1". 2^32-1 needs
%% ceil(32/7) = 5 base-128 octets, so a sub-identifier spanning more than
%% five content octets cannot be legal.
%%
%% Without those bounds dec_oid_element/3 accumulates Num*128 + Dig over
%% as many continuation octets as the sender supplies. The accumulator
%% becomes a bignum and the decode is O(n^2). The path is reachable
%% pre-authentication: varbind OIDs are decoded by snmpa_mpd:v3_proc/6 via
%% dec_scoped_pdu/1 even for an unauthenticated USM stage-1 discovery
%% message, and for v1/v2c on any inbound packet.

dec_oid_size_limits(suite) -> [];
dec_oid_size_limits(doc) ->
    ["Sub-identifier octet count and sub-identifier count limits for "
     "OBJECT IDENTIFIER"];
dec_oid_size_limits(Config) when is_list(Config) ->
    ?IPRINT("a plain OID decodes"),
    {{'OBJECT IDENTIFIER', [1, 3, 6, 1, 2, 1, 1, 5, 0]}, []} =
        snmp_pdus:dec_value(oid_tlv([43, 6, 1, 2, 1, 1, 5, 0])),

    ?IPRINT("the largest legal sub-identifier (2^32-1) decodes"),
    {{'OBJECT IDENTIFIER', [1, 3, 4294967295]}, []} =
        snmp_pdus:dec_value(
          oid_tlv([43, 16#8F, 16#FF, 16#FF, 16#FF, 16#7F])),

    %% Five content octets carry 35 bits, so the octet cap alone would let
    %% values between 2^32 and 2^35-1 through. 16#90 8*80 80 80 00 is
    %% 16 * 128^4 = 4294967296, one above the limit.
    ?IPRINT("a 5 octet sub-identifier above 2^32-1 is rejected"),
    expect_exit(oid_subidentifier_value,
                {error, {bad_oid_subidentifier_value,
                         {4294967296, 4294967295}}},
                fun() ->
                        snmp_pdus:dec_value(
                          oid_tlv([43, 16#90, 16#80, 16#80, 16#80, 0]))
                end),

    %% The guard stops at the point of detection rather than walking to the
    %% end of the sub-identifier, so the reported count is the limit plus
    %% one regardless of how many octets actually follow.
    ?IPRINT("a sub-identifier of 6 content octets is rejected"),
    expect_exit(oid_subidentifier, {error, {bad_oid_subidentifier, {6, 5}}},
                fun() ->
                        snmp_pdus:dec_value(
                          oid_tlv([43, 16#81, 16#80, 16#80, 16#80, 16#80, 0]))
                end),

    ?IPRINT("128 sub-identifiers decode"),
    {{'OBJECT IDENTIFIER', Oid128}, []} =
        snmp_pdus:dec_value(oid_tlv([43 | lists:duplicate(126, 1)])),
    128 = length(Oid128),

    ?IPRINT("129 sub-identifiers are rejected"),
    expect_exit(oid_length, {error, {bad_oid_length, {129, 128}}},
                fun() ->
                        snmp_pdus:dec_value(
                          oid_tlv([43 | lists:duplicate(127, 1)]))
                end),

    ?IPRINT("done"),
    ok.


dec_oid_bounded_time(suite) -> [];
dec_oid_bounded_time(doc) ->
    ["OID decoding cost does not grow with the length of a single "
     "sub-identifier"];
dec_oid_bounded_time(Config) when is_list(Config) ->
    %% The sub-identifier cap makes the rejection cost independent of the
    %% input size -- the decoder gives up after six content octets -- so a
    %% generous wall clock budget is safe even on the slow test variants
    %% (debug, asan, valgrind, cover). Without the cap this vector is
    %% O(n^2) on a bignum accumulator: ~0.2 s for 64 kB of octets and tens
    %% of seconds for the 500 kB below.
    Budget = 2000000,                           % microseconds
    TLV    = oid_tlv([43 | big_subid(500000)]),
    {Micros, Result} =
        timer:tc(fun() ->
                         dec_class(fun() -> snmp_pdus:dec_value(TLV) end)
                 end),
    ?IPRINT("500000 octet sub-identifier handled in ~w us", [Micros]),
    case Result of
        {exit, {error, {bad_oid_subidentifier, {6, 5}}}} ->
            ok;
        Unexpected ->
            exit({unexpected_decode_result, oid_subidentifier, Unexpected})
    end,
    case Micros < Budget of
        true ->
            ok;
        false ->
            exit({decode_too_slow, Micros, Budget})
    end,
    ?IPRINT("done"),
    ok.


%%======================================================================
%% Internal functions
%%======================================================================

dec_message(B) when is_binary(B) ->
    L = binary_to_list(B),
    snmp_pdus:dec_message(L).


%% -----
%% Exception helpers
%%
%% dec_class/1 reports the exception class alongside the reason, so that a
%% throw cannot pass for an exit.

dec_class(Fun) ->
    try Fun() of
        Result ->
            {ok, Result}
    catch
        Class:Reason ->
            {Class, Reason}
    end.

expect_exit(Field, Reason, Fun) ->
    case dec_class(Fun) of
        {exit, Reason} ->
            ok;
        Unexpected ->
            exit({unexpected_decode_result, Field, Reason, Unexpected})
    end.

expect_bad_integer(Field, SizeAndLimit, Fun) ->
    expect_exit(Field, {error, {bad_integer, SizeAndLimit}}, Fun).


%% -----
%% BER encoding helpers
%%
%% All lengths stay below 128, so a single octet length is always enough.

%% INTEGER TLV carrying Value in exactly Size content octets, zero padded.
%% Padding is what the content octet limits exist to bound, so these
%% encodings are intentionally non-minimal.
int_tlv(Value, Size) when is_integer(Value), Value >= 0, Value < 128,
                         is_integer(Size), Size >= 1 ->
    raw_int_tlv(lists:duplicate(Size - 1, 0) ++ [Value]).

raw_int_tlv(Octets) ->
    [2, length(Octets) | Octets].

%% TLV for one of the integer valued application types: Size content
%% octets encoding the value 1.
padded_value(Tag, Size) when is_integer(Size), Size >= 1 ->
    [Tag, Size | lists:duplicate(Size - 1, 0) ++ [1]].

%% Minimal v3 message for dec_message_only/1: version, a header data
%% sequence built from the supplied TLVs, and empty msgSecurityParameters.
v3_msg(HdrDataTLVs) ->
    HdrData = lists:append(HdrDataTLVs),
    HdrSeq  = [48, length(HdrData) | HdrData],
    Content = raw_int_tlv([3]) ++ HdrSeq ++ [4, 0],
    [48, length(Content) | Content].

v3_msg_max_size_tlv() ->
    raw_int_tlv([1, 228]).                      % 484, the RFC 3412 minimum

v3_msg_flags_tlv() ->
    [4, 1, 4].                                  % reportable, noAuthNoPriv

v3_sec_model_tlv() ->
    raw_int_tlv([3]).                           % USM

%% USM security parameters with an empty engine ID, user name, and
%% authentication and privacy parameters.
usm_params(BootsTLV, TimeTLV) ->
    Content = [4, 0] ++ BootsTLV ++ TimeTLV ++ [4, 0] ++ [4, 0] ++ [4, 0],
    [48, length(Content) | Content].

%% PDU with the three supplied integer TLVs and an empty varbind list.
pdu(Tag, IntTLVs) ->
    Content = lists:append(IntTLVs) ++ [48, 0],
    [Tag, length(Content) | Content].

%% TrapPDU with enterprise 1.3, agent-addr 127.0.0.1, time-stamp 0 and an
%% empty varbind list.
trap_pdu(GenericTLV, SpecificTLV) ->
    Content = [6, 1, 43] ++ [64, 4, 127, 0, 0, 1] ++
              GenericTLV ++ SpecificTLV ++ [67, 1, 0] ++ [48, 0],
    [164, length(Content) | Content].

%% OBJECT IDENTIFIER TLV. Unlike the helpers above this one needs long form
%% lengths, so it encodes them explicitly.
oid_tlv(Content) ->
    [6 | ber_len(length(Content))] ++ Content.

ber_len(N) when N < 128 ->
    [N];
ber_len(N) when N < 256 ->
    [16#81, N];
ber_len(N) when N < 65536 ->
    [16#82, N bsr 8, N band 255];
ber_len(N) when N < 16777216 ->
    [16#83, (N bsr 16) band 255, (N bsr 8) band 255, N band 255].

%% One sub-identifier spread over Size content octets, with every
%% continuation octet carrying a payload so that the accumulator grows.
big_subid(Size) when is_integer(Size), Size >= 2 ->
    lists:duplicate(Size - 1, 16#FF) ++ [0].
