%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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
     otp7575
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


%%======================================================================
%% Internal functions
%%======================================================================

dec_message(B) when is_binary(B) ->
    L = binary_to_list(B),
    snmp_pdus:dec_message(L).
