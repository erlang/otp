%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2020. All Rights Reserved.
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
	 otp10132/1
	]).


%%======================================================================
%% Common Test interface functions
%%======================================================================

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    [{group, tickets}].

groups() -> 
    [{tickets, [], tickets_cases()}].

tickets_cases() ->
    [
     otp7575,
     otp8563,
     otp9022,
     otp10132
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
%% Internal functions
%%======================================================================

dec_message(B) when is_binary(B) ->
    L = binary_to_list(B),
    snmp_pdus:dec_message(L).
