%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2010. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose:
%%----------------------------------------------------------------------
-module(snmp_pdus_test).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("test_server.hrl").
-include("snmp_test_lib.hrl").
-include_lib("snmp/include/snmp_types.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
	 all/1, 
	 tickets/1,
	 otp7575/1,
	 otp8563/1, 
         init_per_testcase/2, fin_per_testcase/2
	]).


%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([
        ]).


%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------

%%======================================================================
%% External functions
%%======================================================================

init_per_testcase(_Case, Config) when is_list(Config) ->
    Config.

fin_per_testcase(_Case, Config) when is_list(Config) ->
    Config.


%%======================================================================
%% Test case definitions
%%======================================================================
all(suite) ->
    [
     tickets
    ].

tickets(suite) ->
    [
     otp7575,
     otp8563
    ].



%%======================================================================
%% Test functions
%%======================================================================

otp7575(suite) -> [];
otp7575(doc) -> ["OTP-7575"];
otp7575(Config) when is_list(Config) ->
    io:format("attempt to decode message with valid version~n", []),
    MsgWithOkVersion = <<48,39,2,1,0,4,6,112,117,98,108,105,99,160,26,2,2,1,49,2,1,0,2,1,0,48,14,48,12,6,8,43,6,1,2,1,1,5,0,5,0>>,
    case (catch dec_message(MsgWithOkVersion)) of
	Msg when is_record(Msg, message) ->
	    ok;
	Unexpected1 ->
	    exit({unexpected_decode_result, 1, Unexpected1})
    end,    

    io:format("attempt to decode message with bad version~n", []),
    MsgWithBadVersion = <<48,48,2,10,1,1,1,1,1,1,1,1,1,1,4,6,112,117,98,108,105,99,160,26,2,2,1,49,2,1,0,2,1,0,48,14,48,12,6,8,43,6,1,2,1,1,5,0,5,0>>,
    case (catch dec_message(MsgWithBadVersion)) of
	{'EXIT', {bad_version, BadVersion}} when is_integer(BadVersion) ->
	    ok;
	Unexpected2 ->
	    exit({unexpected_decode_result, 2, Unexpected2})
    end,

    io:format("attempt to decode message with very bad version~n", []),
    MsgWithVeryBadVersion = <<48,49,2,11,1,1,1,1,1,1,1,1,1,1,1,4,6,112,117,98,108,105,99,160,26,2,2,1,49,2,1,0,2,1,0,48,14,48,12,6,8,43,6,1,2,1,1,5,0,5,0>>,
    case (catch dec_message(MsgWithVeryBadVersion)) of
	{'EXIT', {bad_version, {VersionSize, MaxVersionSize}}} when (VersionSize > MaxVersionSize) ->
	    ok;
	Unexpected3 ->
	    exit({unexpected_decode_result, 3, Unexpected3})
    end,
    io:format("done~n", []),
    ok.


otp8563(suite) -> [];
otp8563(doc) -> ["OTP-8563"];
otp8563(Config) when is_list(Config) ->
    Val1 = 16#7fffffffffffffff,
    io:format("try encode and decode ~w~n", [Val1]),
    Enc1 = snmp_pdus:enc_value('Counter64', Val1), 
    {{'Counter64', Val1}, []} = snmp_pdus:dec_value(Enc1), 

    Val2 = Val1 + 1,
    io:format("try encode and decode ~w~n", [Val2]),
    Enc2 = snmp_pdus:enc_value('Counter64', Val2), 
    {{'Counter64', Val2}, []} = snmp_pdus:dec_value(Enc2), 

    Val3 = Val2 + 1,
    io:format("try encode and decode ~w~n", [Val3]),
    Enc3 = snmp_pdus:enc_value('Counter64', Val3), 
    {{'Counter64', Val3}, []} = snmp_pdus:dec_value(Enc3), 

    Val4 = 16#fffffffffffffffe,
    io:format("try encode and decode ~w~n", [Val4]),
    Enc4 = snmp_pdus:enc_value('Counter64', Val4), 
    {{'Counter64', Val4}, []} = snmp_pdus:dec_value(Enc4), 

    Val5 = Val4 + 1,
    io:format("try encode and decode ~w~n", [Val5]),
    Enc5 = snmp_pdus:enc_value('Counter64', Val5), 
    {{'Counter64', Val5}, []} = snmp_pdus:dec_value(Enc5), 

    Val6 = 16#ffffffffffffffff + 1, 
    io:format("try and fail to encode ~w~n", [Val6]),
    case (catch snmp_pdus:enc_value('Counter64', Val6)) of
	{'EXIT', {error, {bad_counter64, Val6}}} ->
	    ok;
	Unexpected6 ->
	    exit({unexpected_encode_result, Unexpected6, Val6})
    end,

    Val7 = -1, 
    io:format("try and fail to encode ~w~n", [Val7]),
    case (catch snmp_pdus:enc_value('Counter64', Val7)) of
	{'EXIT', {error, {bad_counter64, Val7}}} ->
	    ok;
	Unexpected7 ->
	    exit({unexpected_encode_result, Unexpected7, Val7})
    end,
    
    ok.


%%======================================================================
%% Internal functions
%%======================================================================

dec_message(B) when is_binary(B) ->
    L = binary_to_list(B),
    snmp_pdus:dec_message(L).
