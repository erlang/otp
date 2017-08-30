%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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
%% Purpose: Test encoding/decoding (codec) module of Megaco/H.248
%%----------------------------------------------------------------------

-module(megaco_codec_prev3c_test).

%% ----

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_prev3c.hrl").
-include("megaco_test_lib.hrl").

%% ----

-export([msgs/0]).
-export([rfc3525_msgs_display/0, rfc3525_msgs_test/0]).

-export([t/0, t/1]).

-export([all/0,groups/0,init_per_group/2,end_per_group/2, 
	 
	 pretty_test_msgs/1, 
	 
	 compact_test_msgs/1, 
	 
	 flex_pretty_init/1, 
	 flex_pretty_finish/1, 
	 flex_pretty_test_msgs/1,

	 
	 flex_compact_init/1, 
	 flex_compact_finish/1, 
	 flex_compact_test_msgs/1,

	 flex_compact_dm_timers1/1, 
	 flex_compact_dm_timers2/1, 
	 flex_compact_dm_timers3/1, 
	 flex_compact_dm_timers4/1, 
	 flex_compact_dm_timers5/1, 
	 flex_compact_dm_timers6/1, 
	 flex_compact_dm_timers7/1, 
	 flex_compact_dm_timers8/1, 
	 
	 bin_test_msgs/1,
	 
	 ber_test_msgs/1, 
	 
	 per_test_msgs/1,
	
	 erl_dist_m_test_msgs/1,

	 tickets/0, 
	 
	 compact_otp4011_msg1/1, 
	 compact_otp4011_msg2/1,
	 compact_otp4011_msg3/1,
	 compact_otp4013_msg1/1, 
	 compact_otp4085_msg1/1, 
	 compact_otp4085_msg2/1, 
	 compact_otp4280_msg1/1, 
	 compact_otp4299_msg1/1, 
	 compact_otp4359_msg1/1, 
	 compact_otp4920_msg0/1, 
	 compact_otp4920_msg1/1, 
	 compact_otp4920_msg2/1, 
	 compact_otp4920_msg3/1, 
	 compact_otp4920_msg4/1, 
	 compact_otp4920_msg5/1, 
	 compact_otp4920_msg6/1, 
	 compact_otp4920_msg7/1, 
	 compact_otp4920_msg8/1, 
	 compact_otp4920_msg9/1, 
	 compact_otp4920_msg10/1, 
	 compact_otp4920_msg11/1, 
	 compact_otp4920_msg12/1, 
	 compact_otp4920_msg20/1, 
	 compact_otp4920_msg21/1, 
	 compact_otp4920_msg22/1, 
	 compact_otp4920_msg23/1, 
	 compact_otp4920_msg24/1, 
	 compact_otp4920_msg25/1, 
	 compact_otp5186_msg01/1, 
	 compact_otp5186_msg02/1, 
	 compact_otp5186_msg03/1, 
	 compact_otp5186_msg04/1, 
	 compact_otp5186_msg05/1, 
	 compact_otp5186_msg06/1, 
	 compact_otp5793_msg01/1,
	 compact_otp5836_msg01/1,
         compact_otp5993_msg01/1,
         compact_otp5993_msg02/1,
         compact_otp5993_msg03/1,
         compact_otp6017_msg01/1,
         compact_otp6017_msg02/1,
         compact_otp6017_msg03/1,
	 
	 flex_compact_otp4299_msg1/1, 
         flex_compact_otp7431_msg01/1,
         flex_compact_otp7431_msg02/1,
         flex_compact_otp7431_msg03/1,
         flex_compact_otp7431_msg04/1,
         flex_compact_otp7431_msg05/1,
         flex_compact_otp7431_msg06/1,
         flex_compact_otp7431_msg07/1,
	 
	 pretty_otp4632_msg1/1, 
	 pretty_otp4632_msg2/1, 
	 pretty_otp4632_msg3/1, 
	 pretty_otp4632_msg4/1, 
	 pretty_otp4710_msg1/1, 
	 pretty_otp4710_msg2/1, 
	 pretty_otp4945_msg1/1, 
	 pretty_otp4945_msg2/1, 
	 pretty_otp4945_msg3/1, 
	 pretty_otp4945_msg4/1, 
	 pretty_otp4945_msg5/1, 
	 pretty_otp4945_msg6/1, 
	 pretty_otp4949_msg1/1, 
	 pretty_otp4949_msg2/1, 
	 pretty_otp4949_msg3/1, 
	 pretty_otp5042_msg1/1, 
	 pretty_otp5068_msg1/1, 
	 pretty_otp5085_msg1/1, 
	 pretty_otp5085_msg2/1, 
	 pretty_otp5085_msg3/1, 
	 pretty_otp5085_msg4/1, 
	 pretty_otp5085_msg5/1, 
	 pretty_otp5085_msg6/1, 
	 pretty_otp5085_msg7/1, 
	 pretty_otp5085_msg8/1, 
         pretty_otp5600_msg1/1,
         pretty_otp5600_msg2/1,
         pretty_otp5601_msg1/1,
         pretty_otp5793_msg01/1,
	 pretty_otp5803_msg01/1,
	 pretty_otp5803_msg02/1,
	 pretty_otp5805_msg01/1,
	 pretty_otp5836_msg01/1, 
	 pretty_otp5882_msg01/1, 
	 pretty_otp6490_msg01/1, 
	 pretty_otp6490_msg02/1, 
	 pretty_otp6490_msg03/1, 
	 pretty_otp6490_msg04/1, 
	 pretty_otp6490_msg05/1, 
	 pretty_otp6490_msg06/1, 
         pretty_otp7671_msg01/1,
         pretty_otp7671_msg02/1,
         pretty_otp7671_msg03/1,
         pretty_otp7671_msg04/1,
         pretty_otp7671_msg05/1,
         pretty_otp8114_msg01/1,
	 
	 flex_pretty_otp5042_msg1/1, 
	 flex_pretty_otp5085_msg1/1, 
	 flex_pretty_otp5085_msg2/1, 
	 flex_pretty_otp5085_msg3/1, 
	 flex_pretty_otp5085_msg4/1, 
	 flex_pretty_otp5085_msg5/1, 
	 flex_pretty_otp5085_msg6/1, 
	 flex_pretty_otp5085_msg7/1, 
	 flex_pretty_otp5085_msg8/1, 
         flex_pretty_otp5600_msg1/1,
         flex_pretty_otp5600_msg2/1,
         flex_pretty_otp5601_msg1/1,
         flex_pretty_otp5793_msg01/1,
	 flex_pretty_otp5803_msg01/1,
	 flex_pretty_otp5803_msg02/1,
	 flex_pretty_otp5805_msg01/1,
	 flex_pretty_otp5836_msg01/1,
         flex_pretty_otp7431_msg01/1,
         flex_pretty_otp7431_msg02/1,
         flex_pretty_otp7431_msg03/1,
         flex_pretty_otp7431_msg04/1,
         flex_pretty_otp7431_msg05/1,
         flex_pretty_otp7431_msg06/1,
         flex_pretty_otp7431_msg07/1,

	 init_per_testcase/2, end_per_testcase/2]).  

-export([display_text_messages/0, generate_text_messages/0]).


%% ----

-define(V3,    prev3c).
-define(EC_V3, {version3,?V3}).
-define(EC,    [?EC_V3]).

-define(VERSION,      3).
-define(VERSION_STR,  "3").
-define(MSG_LIB,      megaco_test_msg_prev3c_lib).
-define(DEFAULT_PORT, 55555).
-define(MG1_MID_NO_PORT, {ip4Address,
                          #'IP4Address'{address = [124, 124, 124, 222]}}).
-define(MG1_MID, {ip4Address, #'IP4Address'{address = [124, 124, 124, 222],
                                            portNumber = ?DEFAULT_PORT}}).
-define(MG2_MID, {ip4Address, #'IP4Address'{address = [125, 125, 125, 111],
                                            portNumber = ?DEFAULT_PORT}}).
-define(MGC_MID, {ip4Address, #'IP4Address'{address = [123, 123, 123, 4],
                                            portNumber = ?DEFAULT_PORT}}).

-define(A4444, ["11111111", "00000000", "00000000"]).
-define(A4445, ["11111111", "00000000", "11111111"]).
-define(A5555, ["11111111", "11111111", "00000000"]).
-define(A5556, ["11111111", "11111111", "11111111"]).


%% ----

display_text_messages() ->
    Msgs = 
	msgs1a(text) ++ 
	msgs5(text) ++ 
	msgs6(text) ++ 
	msgs7(text),
    megaco_codec_test_lib:display_text_messages(?VERSION, ?EC, Msgs).


generate_text_messages() ->
    Msgs = 
	msgs1a(text) ++ 
	msgs5(text) ++ 
	msgs6(text) ++ 
	msgs7(text),
    megaco_codec_test_lib:generate_text_messages(?V3, ?VERSION, ?EC, Msgs).


%% ----

tickets() ->
    %% io:format("~w:tickets -> entry~n", [?MODULE]),
    megaco_test_lib:tickets(?MODULE).

%% ----

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).

init_per_testcase(Case, Config) ->
    %% CaseString = io_lib:format("~p", [Case]),
    C = 
	case lists:suffix("time_test", atom_to_list(Case)) of
	    true ->
		[{tc_timeout, timer:minutes(10)}|Config];
	    false ->
		put(verbosity,trc),
		Config
	end,
    megaco_test_lib:init_per_testcase(Case, C).

end_per_testcase(Case, Config) ->
    erase(verbosity),
    megaco_test_lib:end_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

all() -> 
    [
     {group, text}, 
     {group, binary}, 
     {group, erl_dist},
     {group, tickets}
    ].

groups() -> 
    [{text, [],
      [{group, pretty}, 
       {group, flex_pretty},
       {group, compact}, 
       {group, flex_compact}]},
     {binary, [],
      [{group, bin}, 
       {group, ber}, 
       {group, per}]},
     {erl_dist, [], [{group, erl_dist_m}]},
     {pretty, [], [pretty_test_msgs]},
     {compact, [], [compact_test_msgs]},
     {flex_pretty, [], flex_pretty_cases()},
     {flex_compact, [], flex_compact_cases()},
     {bin, [], [bin_test_msgs]}, 
     {ber, [], [ber_test_msgs]},
     {per, [], [per_test_msgs]},
     {erl_dist_m, [], [erl_dist_m_test_msgs]},
     {tickets, [],
      [{group, compact_tickets},
       {group, flex_compact_tickets}, 
       {group, pretty_tickets},
       {group, flex_pretty_tickets}]},
     {compact_tickets, [],
      [compact_otp4011_msg1, compact_otp4011_msg2,
       compact_otp4011_msg3, compact_otp4013_msg1,
       compact_otp4085_msg1, compact_otp4085_msg2,
       compact_otp4280_msg1, compact_otp4299_msg1,
       compact_otp4359_msg1, compact_otp4920_msg0,
       compact_otp4920_msg1, compact_otp4920_msg2,
       compact_otp4920_msg3, compact_otp4920_msg4,
       compact_otp4920_msg5, compact_otp4920_msg6,
       compact_otp4920_msg7, compact_otp4920_msg8,
       compact_otp4920_msg9, compact_otp4920_msg10,
       compact_otp4920_msg11, compact_otp4920_msg12,
       compact_otp4920_msg20, compact_otp4920_msg21,
       compact_otp4920_msg22, compact_otp4920_msg23,
       compact_otp4920_msg24, compact_otp4920_msg25,
       compact_otp5186_msg01, compact_otp5186_msg02,
       compact_otp5186_msg03, compact_otp5186_msg04,
       compact_otp5186_msg05, compact_otp5186_msg06,
       compact_otp5793_msg01, compact_otp5836_msg01,
       compact_otp5993_msg01, compact_otp5993_msg02,
       compact_otp5993_msg03, compact_otp6017_msg01,
       compact_otp6017_msg02, compact_otp6017_msg03]},
     {flex_compact_tickets, [], flex_compact_tickets_cases()},
     {pretty_tickets, [],
      [pretty_otp4632_msg1, pretty_otp4632_msg2,
       pretty_otp4632_msg3, pretty_otp4632_msg4,
       pretty_otp4710_msg1, pretty_otp4710_msg2,
       pretty_otp4945_msg1, pretty_otp4945_msg2,
       pretty_otp4945_msg3, pretty_otp4945_msg4,
       pretty_otp4945_msg5, pretty_otp4945_msg6,
       pretty_otp4949_msg1, pretty_otp4949_msg2,
       pretty_otp4949_msg3, pretty_otp5042_msg1,
       pretty_otp5068_msg1, pretty_otp5085_msg1,
       pretty_otp5085_msg2, pretty_otp5085_msg3,
       pretty_otp5085_msg4, pretty_otp5085_msg5,
       pretty_otp5085_msg6, pretty_otp5085_msg7,
       pretty_otp5085_msg8, pretty_otp5600_msg1,
       pretty_otp5600_msg2, pretty_otp5601_msg1,
       pretty_otp5793_msg01, pretty_otp5803_msg01,
       pretty_otp5803_msg02, pretty_otp5805_msg01,
       pretty_otp5836_msg01, pretty_otp5882_msg01,
       pretty_otp6490_msg01, pretty_otp6490_msg02,
       pretty_otp6490_msg03, pretty_otp6490_msg04,
       pretty_otp6490_msg05, pretty_otp6490_msg06,
       pretty_otp7671_msg01, pretty_otp7671_msg02,
       pretty_otp7671_msg03, pretty_otp7671_msg04,
       pretty_otp7671_msg05, pretty_otp8114_msg01]},
     {flex_pretty_tickets, [], flex_pretty_tickets_cases()}].

init_per_group(flex_pretty_tickets, Config) -> 
    flex_pretty_init(Config);
init_per_group(flex_compact_tickets, Config) -> 
    flex_compact_init(Config);
init_per_group(flex_compact, Config) -> 
    flex_compact_init(Config);
init_per_group(flex_pretty, Config) -> 
    flex_pretty_init(Config);
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(flex_pretty_tickets, Config) -> 
    flex_pretty_finish(Config);
end_per_group(flex_compact_tickets, Config) -> 
    flex_compact_finish(Config);
end_per_group(flex_compact, Config) -> 
    flex_compact_finish(Config);
end_per_group(flex_pretty, Config) -> 
    flex_pretty_finish(Config);
end_per_group(_GroupName, Config) ->
    Config.

flex_pretty_cases() -> 
    [
     flex_pretty_test_msgs
    ].

flex_compact_cases() -> 
    [
     flex_compact_test_msgs, 
     flex_compact_dm_timers1,
     flex_compact_dm_timers2, 
     flex_compact_dm_timers3,
     flex_compact_dm_timers4, 
     flex_compact_dm_timers5,
     flex_compact_dm_timers6, 
     flex_compact_dm_timers7,
     flex_compact_dm_timers8
    ].

flex_compact_tickets_cases() -> 
    [
     flex_compact_otp4299_msg1, 
     flex_compact_otp7431_msg01,
     flex_compact_otp7431_msg02, 
     flex_compact_otp7431_msg03,
     flex_compact_otp7431_msg04, 
     flex_compact_otp7431_msg05,
     flex_compact_otp7431_msg06, 
     flex_compact_otp7431_msg07
    ].

flex_pretty_tickets_cases() -> 
    [
     flex_pretty_otp5042_msg1, 
     flex_pretty_otp5085_msg1,
     flex_pretty_otp5085_msg2, 
     flex_pretty_otp5085_msg3,
     flex_pretty_otp5085_msg4, 
     flex_pretty_otp5085_msg5,
     flex_pretty_otp5085_msg6, 
     flex_pretty_otp5085_msg7,
     flex_pretty_otp5085_msg8, 
     flex_pretty_otp5600_msg1,
     flex_pretty_otp5600_msg2, 
     flex_pretty_otp5601_msg1,
     flex_pretty_otp5793_msg01, 
     flex_pretty_otp5803_msg01,
     flex_pretty_otp5803_msg02, 
     flex_pretty_otp5805_msg01,
     flex_pretty_otp5836_msg01, 
     flex_pretty_otp7431_msg01,
     flex_pretty_otp7431_msg02, 
     flex_pretty_otp7431_msg03,
     flex_pretty_otp7431_msg04, 
     flex_pretty_otp7431_msg05,
     flex_pretty_otp7431_msg06, 
     flex_pretty_otp7431_msg07
    ].

		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pretty_test_msgs(suite) ->
    [];
pretty_test_msgs(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Msgs = 
   	msgs1a(text) ++ 
   	msgs1b(text) ++ 
   	msgs3525(text) ++ 
   	msgs5(text) ++ 
   	msgs6(text) ++ 
	msgs7(text),
    %% Msgs = msgs1a(text),
    %% Msgs = msgs1b(text),
    %% Msgs = msgs35525(text),
    %% Msgs = msgs5(text),
    %% Msgs = msgs6(text),
    %% Msgs = msgs7(text), 
    DynamicDecode = false,
    test_msgs(megaco_pretty_text_encoder, DynamicDecode, ?EC, Msgs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flex_pretty_init(Config) ->
    flex_init(Config).

flex_pretty_finish(Config) ->
    flex_finish(Config).
    
flex_pretty_test_msgs(suite) ->
    [];
flex_pretty_test_msgs(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Msgs = 
	msgs1a(text) ++ 
	msgs1b(text) ++ 
	msgs3525(text) ++ 
	msgs5(text) ++ 
	msgs6(text) ++ 
	msgs7(text),
    Conf = flex_scanner_conf(Config),
    DynamicDecode = false,
    test_msgs(megaco_pretty_text_encoder, DynamicDecode, [?EC_V3,Conf], Msgs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compact_test_msgs(suite) ->
    [];
compact_test_msgs(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Msgs = 
	msgs1a(text) ++ 
	msgs1b(text) ++ 
	msgs3525(text) ++ 
	msgs5(text) ++ 
 	msgs6(text) ++ 
	msgs7(text),
    %% Msgs = msgs7(text),
    DynamicDecode = false,
    test_msgs(megaco_compact_text_encoder, DynamicDecode, ?EC, Msgs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flex_compact_init(Config) ->
    flex_init(Config).

flex_compact_finish(Config) ->
    flex_finish(Config).

flex_compact_test_msgs(suite) ->
    [];
flex_compact_test_msgs(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Msgs = 
	msgs1a(text) ++ 
	msgs1b(text) ++ 
	msgs3525(text) ++ 
	msgs5(text) ++ 
	msgs6(text) ++ 
	msgs7(text),
    Conf = flex_scanner_conf(Config), 
    DynamicDecode = true,
    test_msgs(megaco_compact_text_encoder, DynamicDecode, [?EC_V3,Conf], Msgs).


flex_compact_dm_timers1(suite) ->
    [];
flex_compact_dm_timers1(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    M = build_dm_timers_message("1", "2", "3"),
    B = list_to_binary(M),
    Conf = flex_scanner_conf(Config), 
    case decode_message(megaco_compact_text_encoder, false, 
			[?EC_V3,Conf], B) of
	{ok, M1} when is_record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers1 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers({1,2,3}, M1);
	Else ->
	    exit({decode_failed, M, Else})
    end.


flex_compact_dm_timers2(suite) ->
    [];
flex_compact_dm_timers2(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    M = build_dm_timers_message("02", "03", "04"),
    B = list_to_binary(M),
    Conf = flex_scanner_conf(Config), 
    case decode_message(megaco_compact_text_encoder, false, 
			[?EC_V3,Conf], B) of
	{ok, M1} when is_record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers2 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers({2,3,4}, M1);
	Else ->
	    exit({decode_failed, M, Else})
    end.


flex_compact_dm_timers3(suite) ->
    [];
flex_compact_dm_timers3(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    M = build_dm_timers_message("1", "02", "31"),
    B = list_to_binary(M),
    Conf = flex_scanner_conf(Config), 
    case decode_message(megaco_compact_text_encoder, false, 
			[?EC_V3,Conf], B) of
	{ok, M1} when is_record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers3 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers({1,2,31}, M1);
	Else ->
	    exit({decode_failed, M, Else})
    end.


flex_compact_dm_timers4(suite) ->
    [];
flex_compact_dm_timers4(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    M = build_dm_timers_message("10", "21", "99"),
    B = list_to_binary(M),
    Conf = flex_scanner_conf(Config), 
    case decode_message(megaco_compact_text_encoder, false, 
			[?EC_V3,Conf], B) of
	{ok, M1} when is_record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers4 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers({10,21,99}, M1);
	Else ->
	    exit({decode_failed, M, Else})
    end.


flex_compact_dm_timers5(suite) ->
    [];
flex_compact_dm_timers5(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    M = build_dm_timers_message("99", "23", "11"),
    B = list_to_binary(M),
    Conf = flex_scanner_conf(Config), 
    case decode_message(megaco_compact_text_encoder, false, 
			[?EC_V3,Conf], B) of
	{ok, M1} when is_record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers5 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers({99,23,11}, M1);
	Else ->
	    exit({decode_failed, M, Else})
    end.


flex_compact_dm_timers6(suite) ->
    [];
flex_compact_dm_timers6(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    M = build_dm_timers_message("77", "09", "1"),
    B = list_to_binary(M),
    Conf = flex_scanner_conf(Config), 
    case decode_message(megaco_compact_text_encoder, false, 
			[?EC_V3,Conf], B) of
	{ok, M1} when is_record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers6 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers({77,9,1}, M1);
	Else ->
	    exit({decode_failed, M, Else})
    end.


flex_compact_dm_timers7(suite) ->
    [];
flex_compact_dm_timers7(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    M = build_dm_timers_message("77", "09", "1", "99"),
    B = list_to_binary(M),
    Conf = flex_scanner_conf(Config), 
    case decode_message(megaco_compact_text_encoder, false, 
			[?EC_V3,Conf], B) of
	{ok, M1} when is_record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers7 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers({77,9,1,99}, M1);
	Else ->
	    exit({decode_failed, M, Else})
    end.


flex_compact_dm_timers8(suite) ->
    [];
flex_compact_dm_timers8(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    M = build_dm_timers_message("01", "09", "01", "02"),
    B = list_to_binary(M),
    Conf = flex_scanner_conf(Config), 
    case decode_message(megaco_compact_text_encoder, false, 
			[?EC_V3,Conf], B) of
	{ok, M1} when is_record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers8 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers({1,9,1,2}, M1);
	Else ->
	    exit({decode_failed, M, Else})
    end.


build_dm_timers_message(T, S, L) ->
    TMRs = lists:flatten(io_lib:format("T:~s,S:~s,L:~s", [T, S, L])),
    build_dm_timers_message(TMRs).

build_dm_timers_message(T, S, L, Z) ->
    TMRs = lists:flatten(io_lib:format("T:~s,S:~s,L:~s,Z:~s", [T, S, L,Z])),
    build_dm_timers_message(TMRs).

build_dm_timers_message(TMRs) ->
    M = io_lib:format("!/" ?VERSION_STR " [123.123.123.4]:55555\nT=10001{C=-{MF=11111111/00000000/00000000{E=2223{al/on,dd/ce{DM=dialplan00}},SG{cg/rt},DM=dialplan00{~s,(0s| 00s|[1-7]xlxx|8lxxxxxxx|#xxxxxxx|*xx|9l1xxxxxxxxxx|9l011x.s)}}}}", [TMRs]),
    lists:flatten(M).


verify_dm_timers(TMRs, #'MegacoMessage'{mess = Mess}) ->
    #'Message'{messageBody = Body} = Mess,
    case get_dm_timers(Body) of
	TMRs ->
	    ok;
	{error, Reason} ->
	    exit({invalid_timer, {TMRs, Reason}});
	TMRs1 ->
	    exit({invalid_timer_values, {TMRs, TMRs1}})
    end.

get_dm_timers({transactions, T}) when is_list(T) ->
    get_dm_timers1(T);
get_dm_timers(Other) ->
    {error, {invalid_transactions, Other}}.

get_dm_timers1([{transactionRequest,T}|Ts]) 
  when is_record(T,'TransactionRequest') ->
    case get_dm_timers2(T) of
	{ok, Timers} ->
	    Timers;
	_ ->
	    get_dm_timers1(Ts)
    end;
get_dm_timers1([_|Ts]) ->
    get_dm_timers1(Ts);
get_dm_timers1([]) ->
    {error, {no_timers, 'TransactionRequest'}}.


get_dm_timers2(#'TransactionRequest'{actions = Actions}) when is_list(Actions) ->
    get_dm_timers3(Actions).


get_dm_timers3([#'ActionRequest'{commandRequests = Cmds}|Ars]) when is_list(Cmds) ->
    case get_dm_timers4(Cmds) of
	{ok, Timers} ->
	    {ok, Timers};
	_ ->
	    get_dm_timers3(Ars)
    end;
get_dm_timers3([_|Ars]) ->
    get_dm_timers3(Ars);
get_dm_timers3([]) ->
    {error, {no_timers, 'ActionRequest'}}.

get_dm_timers4([#'CommandRequest'{command = Cmd}|Cmds]) ->
    case get_dm_timers5(Cmd) of
	{ok, Timers} ->
	    {ok, Timers};
	_ ->
	    get_dm_timers4(Cmds)
    end;
get_dm_timers4([_|Cmds]) ->
    get_dm_timers4(Cmds);
get_dm_timers4([]) ->
    {error, {no_timers, 'CommandRequest'}}.


get_dm_timers5({modReq, #'AmmRequest'{descriptors = Descriptors}}) ->
    get_dm_timers6(Descriptors);
get_dm_timers5(R) ->
    {error, {no_modReq, R}}.


get_dm_timers6([{digitMapDescriptor, #'DigitMapDescriptor'{digitMapValue = Val}}|_]) ->
    case Val of
	#'DigitMapValue'{startTimer    = T,
			 shortTimer    = S,
			 longTimer     = L,
			 durationTimer = asn1_NOVALUE} ->
	    {ok, {T, S, L}};
	#'DigitMapValue'{startTimer    = T,
			 shortTimer    = S,
			 longTimer     = L,
			 durationTimer = Z} ->
	    {ok, {T, S, L, Z}};
	_ ->
	    {error, no_value_in_dm}
    end;
get_dm_timers6([_|Descs]) ->
    get_dm_timers6(Descs);
get_dm_timers6([]) ->
    {error, {no_timers, descriptors}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bin_test_msgs(suite) ->
    [];
bin_test_msgs(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Msgs = 
	msgs1a(binary) ++ 
	msgs5(binary) ++ 
	msgs6(binary) ++ 
	msgs7(binary),
    %% Msgs = msgs6(binary), 
    DynamicDecode = false,
    test_msgs(megaco_binary_encoder, DynamicDecode, ?EC, Msgs).

   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ber_test_msgs(suite) ->
    [];
ber_test_msgs(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Msgs = 
	msgs1a(binary) ++ 
	msgs5(binary) ++ 
	msgs6(binary) ++ 
	msgs7(binary),
    %% Msgs = msgs7(binary),
    DynamicDecode = false,
    test_msgs(megaco_ber_encoder, DynamicDecode, ?EC, Msgs).

   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

per_test_msgs(suite) ->
    [];
per_test_msgs(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Msgs = 
	msgs1a(binary) ++ 
	msgs5(binary) ++ 
	msgs6(binary) ++ 
	msgs7(binary),
    DynamicDecode = false,
    test_msgs(megaco_per_encoder, DynamicDecode, ?EC, Msgs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

erl_dist_m_test_msgs(suite) ->
    [];
erl_dist_m_test_msgs(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Msgs = 
	msgs1a(erlang) ++ 
	msgs1b(erlang) ++ 
	msgs3525(erlang) ++ 
	msgs5(erlang) ++ 
	msgs6(erlang) ++ 
	msgs7(erlang),
    DynamicDecode = false,
    Conf = [megaco_compressed], 
    test_msgs(megaco_erl_dist_encoder, DynamicDecode, Conf, Msgs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
%% Ticket test cases:


%% --------------------------------------------------------------
%% Observe that this decode SHALL fail
compact_otp4011_msg1(suite) ->
    [];
compact_otp4011_msg1(Config) when is_list(Config) ->
    %%     put(severity,trc),
    %%     put(dbg,true),
    d("compact_otp4011_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M = "!/" ?VERSION_STR " ML T=233350{C=${A=stedevice/01{M{O{MO=SR,RV=OFF,RG=OFF,tdmc/ec=OFF,MO=SR}}}}}",
    ok = compact_otp4011(M),
    %%     erase(severity),
    %%     erase(dbg),
    ok.


%% --------------------------------------------------------------
%% Observe that this decode SHALL fail
compact_otp4011_msg2(suite) ->
    [];
compact_otp4011_msg2(Config) when is_list(Config) ->
    %%     put(severity,trc),
    %%     put(dbg,true),
    d("compact_otp4011_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M = "!/" ?VERSION_STR " ML T=233350{C=${A=stedevice/01{M{O{MO=SO,RV=OFF,RG=OFF,tdmc/ec=OFF,MO=SR}}}}}",
    ok = compact_otp4011(M),
    %%     erase(severity),
    %%     erase(dbg),
    ok.

%% --------------------------------------------------------------
%% Observe that this decode SHALL fail
%% 

compact_otp4011_msg3(suite) ->
    [];
compact_otp4011_msg3(Config) when is_list(Config) ->
    %%     put(severity,trc),
    %%     put(dbg,true),
    d("compact_otp4011_msg3 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M = "!/" ?VERSION_STR " ML T=233350{C=${A=stedevice/01{M{O{MO=SR,RV=OFF,RG=OFF,tdmc/ec=OFF,MO=SO}}}}}",
    ok = compact_otp4011(M),
    %%     erase(severity),
    %%     erase(dbg),
    ok.

compact_otp4011(Msg) ->
    compact_otp4011(Msg, ?EC).

compact_otp4011(Msg, Conf) ->
    Codec  = megaco_compact_text_encoder,
    Decode = fun(B) -> decode_message(Codec, false, Conf, B) end,
    Check  = fun(Reason) when is_list(Reason) ->
		     compact_otp4011_chk1(Reason);
		(Crap) ->
		     {error, {unexpected_decode_result, Crap}}
	     end,
    megaco_codec_test_lib:expect_decode(Msg, Decode, Check).
		     
compact_otp4011_chk1(R1) ->
    case lists:keysearch(reason, 1, R1) of
	{value, {reason, R2}} ->
	    compact_otp4011_chk2(R2);
	false ->
	    {error, {unexpected_result, R1}}
    end.
	    
compact_otp4011_chk2({0, ParserMod, {ParserFunc, [A, B]}}) 
  when (ParserMod  =:= megaco_text_parser_prev3c) andalso
       (ParserFunc =:= do_merge_control_streamParms) andalso
       is_list(A) andalso
       is_record(B, 'LocalControlDescriptor') ->
    SM = B#'LocalControlDescriptor'.streamMode, 
    case lists:keysearch(mode, 1, A) of
	{value, {mode, _Mode}} when SM /= asn1_NOVALUE ->
	    ok;
	{value, {mode, _Mode}} ->
	    {error, {unexpected_streamMode_reason, {A, B}}};
	false ->
	    {error, {unexpected_mode_reason, {A, B}}}
    end;
compact_otp4011_chk2(Bad) ->
    {error, {unexpected_reason, Bad}}.


%% --------------------------------------------------------------
%% Note that this decode SHALL fail, because of the misspelled
%% MEGCAO instead of the correct MEGACO.
compact_otp4013_msg1(suite) ->
    [];
compact_otp4013_msg1(Config) when is_list(Config) ->
    %%     put(severity,trc),
    %%     put(dbg,true),
    d("compact_otp4013_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M = "MEGCAO/3 MG1 T=12345678{C=-{SC=root{SV{MT=RS,RE=901}}}}",
    ok = compact_otp4013(M),
    %%     erase(severity),
    %%     erase(dbg),
    ok.

compact_otp4013(Msg) ->
    compact_otp4013(Msg, ?EC).

compact_otp4013(Msg, Conf) ->    
    Codec  = megaco_compact_text_encoder,
    Decode = fun(B) -> decode_message(Codec, false, Conf, B) end,
    Check  = fun(Reason) when is_list(Reason) ->
		     compact_otp4013_chk1(Reason);
		(Crap) ->
		     {error, {unexpected_decode_result, Crap}}
	     end,
    megaco_codec_test_lib:expect_decode(Msg, Decode, Check).

compact_otp4013_chk1(Reason) ->
    case lists:keysearch(reason, 1, Reason) of
	{value, {reason, no_version_found, _}} ->
	    case lists:keysearch(token, 1, Reason) of
		{value, {token, [{'SafeChars',_,"megcao/3"}|_]}} ->
		    ok;
		{value, {token, Tokens}} ->
		    {error, {unexpected_tokens, Tokens}};
		false ->
		    {error, {tokens_not_found, Reason}}
	    end;
	{value, {reason, BadReason, _}} ->
	    {error, {unexpected_reason, BadReason}};
	false ->
	    {error, {reason_not_found, Reason}}
    end.


%% --------------------------------------------------------------
%% 
%% 
compact_otp4085_msg1(suite) ->
    [];
compact_otp4085_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4085_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M = compact_otp4085_erroneous_msg(),
    ok = compact_otp4085_1(M),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp4085_1(Msg) ->
    compact_otp4085_1(Msg, ?EC).

compact_otp4085_1(Msg, Conf) ->
    Codec  = megaco_compact_text_encoder,
    Decode = fun(B) -> decode_message(Codec, false, Conf, B) end,
    Check  = fun(Reason) when is_list(Reason) ->
		     compact_otp4085_1_chk1(Reason);
		(Crap) ->
		     {error, {unexpected_decode_result, Crap}}
	     end,
    megaco_codec_test_lib:expect_decode(Msg, Decode, Check).

compact_otp4085_1_chk1(Reason) ->
    case lists:keysearch(reason, 1, Reason) of
	{value, {reason, {Line, Module, Crap}}} when is_integer(Line) and 
						     is_atom(Module) ->
	    Crap2 = 
		case (catch lists:flatten(Crap)) of
		    L when is_list(L) ->
			L;
		    _ ->
			Crap
		end,
	    t("compact_otp4085_1_chk1 -> Expected: "
	      "~n   Line:   ~p"
	      "~n   Module: ~p"
	      "~n   Crap2:  ~p", [Line, Module, Crap2]),
	    ok;
	{value, BadReason} -> 
	    e("compact_otp4085_1_chk1 -> error: "
	      "~n   BadReason:   ~p", [BadReason]),
	    {error, {unexpected_reason, Reason}};
	false ->
	    {error, {reason_not_found, Reason}}
    end.


%% --------------------------------------------------------------
%% This test case is just to show that the message used in
%% compact_otp4085_msg1 is actually ok when you add '}' at the end.
compact_otp4085_msg2(suite) ->
    [];
compact_otp4085_msg2(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4085_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M = compact_otp4085_erroneous_msg() ++ "}",
    ok = compact_otp4085_2(M),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp4085_2(Msg) ->
    compact_otp4085_2(Msg, ?EC).

compact_otp4085_2(Msg, Conf) ->
    Codec  = megaco_compact_text_encoder,
    Decode = fun(B) -> decode_message(Codec, false, Conf, B) end,
    Check  = fun(M) when is_record(M, 'MegacoMessage') ->
		     ok;
		(Crap) ->
		     {error, {unexpected_decode_result, Crap}}
	     end,
    megaco_codec_test_lib:expect_decode_only(Msg, Decode, Check).


%% This message lack the ending parentesis (}).
compact_otp4085_erroneous_msg() ->
    M = "!/" 
	?VERSION_STR 
	" ML T=11223342{C=${A=${M{O{MO=SR,RV=OFF,RG=OFF},L{v=0,"
	"c=ATM NSAP $ ,"
	"a=eecid:$ ,"
	"m=audio - AAL1/ATMF -,"
	"}}},A=stee1181/01{M{O{MO=SR,RV=OFF,RG=OFF,tdmc/ec=off}}}}",
    M.

%% --------------------------------------------------------------
%% 
%% 
compact_otp4280_msg1(suite) ->
    [];
compact_otp4280_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4280_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M = compact_otp4280_msg(),
    ok = compact_otp4280(M),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp4280(Msg) ->
    compact_otp4280(Msg, ?EC).

compact_otp4280(Msg, Conf) ->
    Codec  = megaco_compact_text_encoder,
    Decode = fun(B) -> decode_message(Codec, false, Conf, B) end,
    Check  = fun(M) when is_record(M, 'MegacoMessage') ->
		     ok;
		(Crap) ->
		     {error, {unexpected_decode_result, Crap}}
	     end,
    megaco_codec_test_lib:expect_decode_only(Msg, Decode, Check).
    
compact_otp4280_msg() ->
    M = "!/"
	?VERSION_STR
	" mgw1 P=71853646{C=-{AV=root{M{TS{root/maxnumberofcontexts=49500,"
	"root/maxterminationspercontext=2,root/normalmgexecutiontime=200,"
	"root/normalmgcexecutiontime=150,"
	"root/provisionalresponsetimervalue=2000,BF=OFF,SI=IV}}}}}",
    M.


%% --------------------------------------------------------------
%% This ticket is about comments in a message
%% 
compact_otp4299_msg1(suite) ->
    [];
compact_otp4299_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4299_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg = compact_otp4299_msg(),
    ok = compact_otp4299(Msg),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp4299(Msg) ->
    compact_otp4299(Msg, ?EC).

compact_otp4299(Msg, Conf) ->
    Codec  = megaco_compact_text_encoder,
    Decode = fun(B) -> decode_message(Codec, false, Conf, B) end,
    Check  = fun(M) when is_record(M, 'MegacoMessage') ->
		     ok;
		(Crap) ->
		     {error, {unexpected_decode_result, Crap}}
	     end,
    megaco_codec_test_lib:expect_decode_only(Msg, Decode, Check).

compact_otp4299_msg() ->
    M = ";KALLE\n"
	"!/"
	?VERSION_STR
	" mg58_1 P=005197711{; YET ANOTHER COMMENT\n"
	"C=035146207{A=mg58_1_1_4_1_23/19; BEFORE COMMA\n"
	",; AFTER COMMA\n"
	"A=eph58_1/0xA4023371{M{L{\n"
	"v=0\n"
	"c=ATM NSAP 39.0102.0304.0506.0708.090a.0b58.0100.0000.0000.00\n"
	"m=audio - AAL1/ATMF -\n"
	"a=eecid:A4023371\n"
	"}}; HOBBE\n}; KALLE \"HOBBE \n}}"
	";KALLE\n\n",
    M.


%% --------------------------------------------------------------
%% 

compact_otp4359_msg1(suite) ->
    [];
compact_otp4359_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4359_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg = compact_otp4359_msg(), 
    ok = compact_otp4359(Msg), 
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp4359_msg() ->
    M = "!/" ?VERSION_STR " ml2 T={C=${A=${M{O {MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

compact_otp4359(Msg) ->
    compact_otp4359(Msg, ?EC).

compact_otp4359(Msg, Conf) ->
    Codec  = megaco_compact_text_encoder,
    Decode = fun(B) -> decode_message(Codec, false, Conf, B) end,
    Check  = fun(M) when is_record(M, 'MegacoMessage') ->
		     compact_otp4359_chk(M);
		(Crap) ->
		     {error, {unexpected_decode_result, Crap}}
	     end,
    megaco_codec_test_lib:expect_decode_only(Msg, Decode, Check).
    
compact_otp4359_chk(#'MegacoMessage'{mess = Mess}) ->
    case Mess#'Message'.messageBody of
	{transactions, Trans} ->
	    case Trans of
		[{transactionRequest, TR}] ->
		    case TR of
			#'TransactionRequest'{transactionId = asn1_NOVALUE} ->
			    ok;
			_ ->
			    {error, {unexpected_trans_req, TR}}
		    end;
		_ ->
		    {error, {unexpected_trans, Trans}}
	    end;
	Body ->
	    {error, {unexpected_messageBody, Body}}
    end.


%% --------------------------------------------------------------
%% 
compact_otp4920_msg0(suite) ->
    [];
compact_otp4920_msg0(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg0 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_decode_encode_ok( compact_otp4920_msg0() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp4920_msg1(suite) ->
    [];
compact_otp4920_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_decode_encode_ok( compact_otp4920_msg1() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp4920_msg2(suite) ->
    [];
compact_otp4920_msg2(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_decode_encode_ok( compact_otp4920_msg2() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp4920_msg3(suite) ->
    [];
compact_otp4920_msg3(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg3 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_decode_encode_ok( compact_otp4920_msg3() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp4920_msg4(suite) ->
    [];
compact_otp4920_msg4(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg4 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_decode_encode_ok( compact_otp4920_msg4() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp4920_msg5(suite) ->
    [];
compact_otp4920_msg5(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg5 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_decode_encode_ok( compact_otp4920_msg5() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp4920_msg6(suite) ->
    [];
compact_otp4920_msg6(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg6 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_decode_encode_ok( compact_otp4920_msg6() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp4920_msg7(suite) ->
    [];
compact_otp4920_msg7(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg7 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_decode_encode_ok( compact_otp4920_msg7() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp4920_msg8(suite) ->
    [];
compact_otp4920_msg8(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg8 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_decode_encode_ok( compact_otp4920_msg8() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp4920_msg9(suite) ->
    [];
compact_otp4920_msg9(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg9 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_decode_encode_ok( compact_otp4920_msg9() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp4920_msg10(suite) ->
    [];
compact_otp4920_msg10(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg10 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_decode_encode_ok( compact_otp4920_msg10() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp4920_msg11(suite) ->
    [];
compact_otp4920_msg11(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg11 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_decode_encode_ok( compact_otp4920_msg11() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp4920_msg12(suite) ->
    [];
compact_otp4920_msg12(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg12 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_decode_encode_ok( compact_otp4920_msg12() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

%% Duplicate padding
compact_otp4920_msg20(suite) ->
    [];
compact_otp4920_msg20(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg20 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = compact_otp4920(compact_otp4920_msg20(), bad_mid_duplicate_padding),
    %% erase(severity),
    %% erase(dbg),
    ok.

%% Length
compact_otp4920_msg21(suite) ->
    [];
compact_otp4920_msg21(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg21 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = compact_otp4920(compact_otp4920_msg21(), bad_mid_ip6addr_length),
    %% erase(severity),
    %% erase(dbg),
    ok.

%% Length
compact_otp4920_msg22(suite) ->
    [];
compact_otp4920_msg22(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg22 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = compact_otp4920(compact_otp4920_msg22(), bad_mid_ip6addr_length),
    %% erase(severity),
    %% erase(dbg),
    ok.

%% Length
compact_otp4920_msg23(suite) ->
    [];
compact_otp4920_msg23(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg23 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = compact_otp4920(compact_otp4920_msg23(), bad_mid_ip6addr_length),
    %% erase(severity),
    %% erase(dbg),
    ok.

%% Length
compact_otp4920_msg24(suite) ->
    [];
compact_otp4920_msg24(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg24 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = compact_otp4920(compact_otp4920_msg24(), bad_mid_ip6addr_length),
    %% erase(severity),
    %% erase(dbg),
    ok.

%% Length
compact_otp4920_msg25(suite) ->
    [];
compact_otp4920_msg25(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4920_msg25 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = compact_otp4920(compact_otp4920_msg25(), bad_mid_ip6addr_length),
    %% erase(severity),
    %% erase(dbg),
    ok.

    
compact_otp4920(Msg, ExpectedReason) ->
    compact_otp4920(Msg, ?EC, ExpectedReason).

compact_otp4920(Msg, Conf, ExpectedReason) ->
    Codec  = megaco_compact_text_encoder,
    Decode = fun(B) -> decode_message(Codec, false, Conf, B) end,
    Check  = fun(Reason) when is_list(Reason) ->
		     compact_otp4920_chk(Reason, ExpectedReason);
		(Crap) ->
		     {error, {unexpected_decode_result, Crap}}
	     end,
    megaco_codec_test_lib:expect_decode(Msg, Decode, Check).

compact_otp4920_chk(Reason, ExpectedReason) ->
    case lists:keysearch(reason, 1, Reason) of
	{value, {reason, {__Line, _Mod, ActualReason}}} ->
	    case element(1, ActualReason) of
		ExpectedReason ->
		    ok;
		_ ->
		    {error, {unexpected_decode_reason, 
			     {ActualReason, ExpectedReason}}}
	    end;
	{value, {reason, {_Mod, ActualReason}}} ->
	    case element(1, ActualReason) of
		ExpectedReason ->
		    ok;
		_ ->
		    {error, {unexpected_decode_reason, 
			     {ActualReason, ExpectedReason}}}
	    end;
	{value, UnknownReason} ->
	    {error, {unexpected_decode_reason, UnknownReason}};
	false ->
	    {error, {reason_not_found, Reason}}
    end.

compact_otp4920_msg0() ->
    M = "!/" ?VERSION_STR " [192.168.30.1]\nT=100{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

compact_otp4920_msg1() ->
    M = "!/" ?VERSION_STR " [2031:0000:130F:0000:0000:09C0:876A:130B]\nT=101{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

compact_otp4920_msg2() ->
    M = "!/" ?VERSION_STR " [2031:0:130F:0:0:9C0:876A:130B]\nT=102{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

compact_otp4920_msg3() ->
    M = "!/" ?VERSION_STR " [2031:0:130F::9C0:876A:130B]\nT=103{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

compact_otp4920_msg4() ->
    M = "!/" ?VERSION_STR " [::1]\nT=104{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

compact_otp4920_msg5() ->
    M = "!/" ?VERSION_STR " [::]\nT=105{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

compact_otp4920_msg6() ->
    M = "!/" ?VERSION_STR " [1::]\nT=106{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

compact_otp4920_msg7() ->
    M = "!/" ?VERSION_STR " [FEDC:1::]\nT=107{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

compact_otp4920_msg8() ->
    M = "!/" ?VERSION_STR " [2031:0:130F:0:0:9C0:135.106.19.11]\nT=108{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

compact_otp4920_msg9() ->
    M = "!/" ?VERSION_STR " [2031:0:130F::9C0:135.106.19.11]\nT=109{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

compact_otp4920_msg10() ->
    M = "!/" ?VERSION_STR " [::FFFF:192.168.30.1]\nT=110{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

compact_otp4920_msg11() ->
    M = "!/" ?VERSION_STR " [::192.168.30.1]\nT=111{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

compact_otp4920_msg12() ->
    M = "!/" ?VERSION_STR " [::C0A8:1E01]\nT=112{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

%% Illegal: only one :: allowed
compact_otp4920_msg20() ->
    M = "!/" ?VERSION_STR " [2031::130F::9C0]\nT=120{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

%% Illegal: length
compact_otp4920_msg21() ->
    M = "!/" ?VERSION_STR " [2031:FFEE:0000:130F:0000:0000:09C0:876A:130B]\nT=121{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

%% Illegal: length
compact_otp4920_msg22() ->
    M = "!/" ?VERSION_STR " [2031:FFEE:0:130F:0:0:9C0:135.106.19.11]\nT=122{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

%% Illegal: length
compact_otp4920_msg23() ->
    M = "!/" ?VERSION_STR " [2031:FFEE:0000:130F:2132:4354::09C0:876A:130B]\nT=123{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

%% Illegal: length
compact_otp4920_msg24() ->
    M = "!/" ?VERSION_STR " [::2031:FFEE:0000:130F:2132:4354:09C0:876A:130B]\nT=124{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.

%% Illegal: length
compact_otp4920_msg25() ->
    M = "!/" ?VERSION_STR " [2031:FFEE:0000:130F:2132:4354:09C0:876A:130B::]\nT=125{C=${A=${M{O{MO=SR,RG=OFF,RV=OFF}}}}}",
    M.


%% --------------------------------------------------------------
%% 

compact_otp5186_msg01(suite) ->
    [];
compact_otp5186_msg01(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp5186_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_decode_error( compact_otp5186_msg01() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp5186_msg02(suite) ->
    [];
compact_otp5186_msg02(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp5186_msg02 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_decode_encode_ok( compact_otp5186_msg02() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp5186_msg03(suite) ->
    [];
compact_otp5186_msg03(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp5186_msg03 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    %% ok = compact_otp5186_msg_2(compact_otp5186_msg03(), ok, ok),
    ok = ticket_compact_encode_decode_ok( compact_otp5186_msg03() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp5186_msg04(suite) ->
    [];
compact_otp5186_msg04(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp5186_msg04 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    %% ok = compact_otp5186_msg_2(compact_otp5186_msg04(), ok, ok),
    ok = ticket_compact_encode_decode_ok( compact_otp5186_msg04() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp5186_msg05(suite) ->
    [];
compact_otp5186_msg05(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp5186_msg05 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    %% ok = compact_otp5186_msg_2(compact_otp5186_msg05(), ok, ok),
    ok = ticket_compact_encode_decode_ok( compact_otp5186_msg05() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp5186_msg06(suite) ->
    [];
compact_otp5186_msg06(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp5186_msg06 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    %% ok = compact_otp5186_msg_2(compact_otp5186_msg06(), ok, ok),
    ok = ticket_compact_encode_decode_ok( compact_otp5186_msg06() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

%% --
						   
compact_otp5186_msg01() ->
    "!/" ?VERSION_STR " <mg5>\nP=67111298{C=2699{AV=mg5_ipeph/0x0f0001{}}}".

compact_otp5186_msg02() ->
    "!/" ?VERSION_STR " <mg5>\nP=67111298{C=2699{AV=mg5_ipeph/0x0f0001}}".

compact_otp5186_msg03() ->
    {'MegacoMessage',
     asn1_NOVALUE,
     {'Message',
      ?VERSION,
      {domainName,{'DomainName',"mg5",asn1_NOVALUE}},
      {transactions,
       [{transactionReply,
	 {'TransactionReply',67111298,asn1_NOVALUE,
	  {actionReplies,[
			  {'ActionReply',2699,asn1_NOVALUE,asn1_NOVALUE,
			   [
			    {auditValueReply,
			     {auditResult,
			      {'AuditResult',
			       {megaco_term_id,false,["mg5_ipeph","0x0f0001"]},
			       [
			       ]
			      }
			     }
			    }
			   ]
			  }
			 ]
	  },asn1_NOVALUE,asn1_NOVALUE
	 }
	}
       ]
      }
     }
    }.

compact_otp5186_msg04() ->
    {'MegacoMessage',asn1_NOVALUE,
     {'Message',?VERSION,{domainName,{'DomainName',"mg5",asn1_NOVALUE}},
      {transactions,
       [{transactionReply,
	 {'TransactionReply',67111298,asn1_NOVALUE,
	  {actionReplies,[
			  {'ActionReply',2699,asn1_NOVALUE,asn1_NOVALUE,
			   [
			    {auditValueReply,
			     {auditResult,
			      {'AuditResult',
			       {megaco_term_id,false,["mg5_ipeph","0x0f0001"]},
			       [
				{emptyDescriptors,
				 {'AuditDescriptor',asn1_NOVALUE,asn1_NOVALUE}
				}
			       ]
			      }
			     }
			    }
			   ]
			  }
			 ]
	  },asn1_NOVALUE,asn1_NOVALUE
	 }
	}
       ]
      }
     }
    }.

compact_otp5186_msg05() ->
    {'MegacoMessage',
     asn1_NOVALUE,
     {'Message',
      ?VERSION,
      {domainName,{'DomainName',"mg5",asn1_NOVALUE}},
      {transactions,
       [{transactionReply,
	 {'TransactionReply',67111298,asn1_NOVALUE,
	  {actionReplies,[
			  {'ActionReply',2699,asn1_NOVALUE,asn1_NOVALUE,
			   [
			    {addReply,
			     {'AmmsReply',
			      [
			       {megaco_term_id,false,["mg5_ipeph","0x0f0001"]}
			      ],
			      [
			      ]
			     }
			    }
			   ]
			  }
			 ]
	  },asn1_NOVALUE,asn1_NOVALUE
	 }
	}
       ]
      }
     }
    }.

compact_otp5186_msg06() ->
    {'MegacoMessage',asn1_NOVALUE,
     {'Message',?VERSION,{domainName,{'DomainName',"mg5",asn1_NOVALUE}},
      {transactions,
       [{transactionReply,
	 {'TransactionReply',67111298,asn1_NOVALUE,
	  {actionReplies,[
			  {'ActionReply',2699,asn1_NOVALUE,asn1_NOVALUE,
			   [
			    {addReply,
			     {'AmmsReply',
			      [
			       {megaco_term_id,false,["mg5_ipeph","0x0f0001"]}
			      ],
			      [
			       {emptyDescriptors,
				{'AuditDescriptor',asn1_NOVALUE,asn1_NOVALUE}
			       }
			      ]
			     }
			    }
			   ]
			  }
			 ]
	  },asn1_NOVALUE,asn1_NOVALUE
	 }
	}
       ]
      }
     }
    }.


%% --------------------------------------------------------------

compact_otp5793_msg01(suite) ->
    [];
compact_otp5793_msg01(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp5793_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_encode_decode_ok(pretty_otp5793_msg1()),
    %% erase(severity),
    %% erase(dbg),
    ok.    


%% --------------------------------------------------------------

compact_otp5836_msg01(suite) ->
    [];
compact_otp5836_msg01(Config) when is_list(Config) ->
    %%     put(severity,trc),
    %%     put(dbg,true),
    d("compact_otp5836_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_encode_decode_ok(compact_otp5836_msg1()),
    %% erase(severity),
    %% erase(dbg),
    ok.


compact_otp5836_msg1() ->
    {'MegacoMessage',
     asn1_NOVALUE,
     {'Message',
      3,
      {deviceName,"bs_sbg_4/34"},
      {transactions, 
       [{transactionReply, 
	 {'TransactionReply',
	  12,
	  asn1_NOVALUE,
	  {actionReplies,
	   [{'ActionReply',
	     4294967295,
	     asn1_NOVALUE,
	     asn1_NOVALUE,
	     [{auditValueReply,
	       {error, {'ErrorDescriptor', 431, asn1_NOVALUE}}}
	     ]
	    }
	   ]
	  },asn1_NOVALUE,asn1_NOVALUE
	 }
	}
       ]
      }
     }
    }.


%% --------------------------------------------------------------

compact_otp5993_msg01(suite) ->
    [];
compact_otp5993_msg01(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp5993_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_encode_decode_ok( compact_otp5993_msg01() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp5993_msg01() ->
    MT = h221,
    T  = #megaco_term_id{id = ?A4444},
    TL = [T],
    MD = #'MuxDescriptor'{muxType  = MT,
                          termList = TL},
    compact_otp5993_msg(MD).


compact_otp5993_msg02(suite) ->
    [];
compact_otp5993_msg02(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp5993_msg02 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_encode_decode_ok( compact_otp5993_msg02() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp5993_msg02() ->
    MT = h223,
    T1 = #megaco_term_id{id = ?A4445},
    T2 = #megaco_term_id{id = ?A5556},
    TL = [T1, T2],
    MD = #'MuxDescriptor'{muxType  = MT,
                          termList = TL},
    compact_otp5993_msg(MD).


compact_otp5993_msg03(suite) ->
    [];
compact_otp5993_msg03(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp5993_msg03 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_compact_encode_decode_ok( compact_otp5993_msg03() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

compact_otp5993_msg03() ->
    T1       = #megaco_term_id{id = ?A4445},
    T2       = #megaco_term_id{id = ?A5556},
    TIDs     = [T1, T2],
    AudRep   = {contextAuditResult, TIDs},
    CmdRep   = {auditValueReply, AudRep},
    ActRep   = #'ActionReply'{contextId    = 5993,
                              commandReply = [CmdRep]},
    TransRes = {actionReplies, [ActRep]},
    TransRep = #'TransactionReply'{transactionId     = 3995,
                                   transactionResult = TransRes},
    Trans    = {transactionReply, TransRep},
    Body     = {transactions, [Trans]},
    Msg      = #'Message'{version     = ?VERSION,
                          mId         = ?MG1_MID,
                          messageBody = Body},
    #'MegacoMessage'{mess = Msg}.


compact_otp5993_msg(MD) when is_record(MD, 'MuxDescriptor') ->
    AmmDesc  = {muxDescriptor, MD},
    AmmReq   = #'AmmRequest'{terminationID = [hd(MD#'MuxDescriptor'.termList)],
                             descriptors   = [AmmDesc]},
    Cmd      = {addReq, AmmReq},
    CmdReq   = #'CommandRequest'{command = Cmd},
    ActReq   = #'ActionRequest'{contextId       = 5993,
                                commandRequests = [CmdReq]},
    TransReq = #'TransactionRequest'{transactionId = 3995,
                                     actions       = [ActReq]},
    Trans    = {transactionRequest, TransReq},
    Body     = {transactions, [Trans]},
    Msg      = #'Message'{version = ?VERSION,
                          mId     = ?MG1_MID,
                          messageBody = Body},
    #'MegacoMessage'{mess = Msg}.


%% --------------------------------------------------------------

compact_otp6017_msg01(suite) ->
    [];
compact_otp6017_msg01(Config) when is_list(Config) ->
    d("compact_otp6017_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = compact_otp6017(0),
    ok.

compact_otp6017_msg02(suite) ->
    [];
compact_otp6017_msg02(Config) when is_list(Config) ->
    d("compact_otp6017_msg02 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = compact_otp6017(16#FFFFFFFE),
    ok.

compact_otp6017_msg03(suite) ->
    [];
compact_otp6017_msg03(Config) when is_list(Config) ->
    d("compact_otp6017_msg03 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = compact_otp6017(16#FFFFFFFF),
    ok.

compact_otp6017(BadCID) ->
    Conf = ?EC,
    M   = compact_otp6017_msg(BadCID),
    Bin = list_to_binary(M),
    case decode_message(megaco_compact_text_encoder, false, Conf, Bin) of
        {ok, Msg} ->
            exit({unexpected_decode_success, {Msg, M}});
        {error, Reason} when is_list(Reason) -> % Expected result
            case lists:keysearch(reason, 1, Reason) of
                {value, {reason, {_Line, _Mod, {bad_ContextID, BadCID}}}} ->
                    io:format(" ~w", [BadCID]),
                    ok;
                {value, {reason, ActualReason}} ->
                    exit({unexpected_reason, ActualReason});
                false ->
                    exit({reason_not_found, Reason})
            end;
        Crap ->
            exit({unexpected_decode_result, Crap})
    end.

compact_otp6017_msg(CID) when is_integer(CID) ->
    "MEGACO/" ?VERSION_STR " MG1 T=12345678{C=" ++
        integer_to_list(CID) ++
        "{SC=root{SV{MT=RS,RE=901}}}}".


%% ==============================================================
%%
%% F l e x   C o m p a c t   T e s t c a s e s
%%

flex_compact_otp4299_msg1(suite) ->
    [];
flex_compact_otp4299_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_comppact_otp4299_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg  = compact_otp4299_msg(),
    Conf = flex_scanner_conf(Config),
    ok = compact_otp4299(Msg, [?EC_V3,Conf]),
    %% erase(severity),
    %% erase(dbg),
    ok.


flex_compact_otp7431_msg01(suite) ->
    [];
flex_compact_otp7431_msg01(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_comppact_otp7431_msg01 -> entry", []),
    Conf = flex_scanner_conf(Config),
    flex_compact_otp7431(ok, flex_compact_otp7431_msg1(), [Conf]).

flex_compact_otp7431_msg02(suite) ->
    [];
flex_compact_otp7431_msg02(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_comppact_otp7431_msg02 -> entry", []),
    Conf = flex_scanner_conf(Config),
    flex_compact_otp7431(error, flex_compact_otp7431_msg2(), [Conf]).

flex_compact_otp7431_msg03(suite) ->
    [];
flex_compact_otp7431_msg03(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_comppact_otp7431_msg03 -> entry", []),
    Conf = flex_scanner_conf(Config),
    flex_compact_otp7431(error, flex_compact_otp7431_msg3(), [Conf]).

flex_compact_otp7431_msg04(suite) ->
    [];
flex_compact_otp7431_msg04(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_comppact_otp7431_msg04 -> entry", []),
    Conf = flex_scanner_conf(Config),
    flex_compact_otp7431(error, flex_compact_otp7431_msg4(), [Conf]).

flex_compact_otp7431_msg05(suite) ->
    [];
flex_compact_otp7431_msg05(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_comppact_otp7431_msg05 -> entry", []),
    Conf = flex_scanner_conf(Config),
    flex_compact_otp7431(error, flex_compact_otp7431_msg5(), [Conf]).

flex_compact_otp7431_msg06(suite) ->
    [];
flex_compact_otp7431_msg06(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_comppact_otp7431_msg06 -> entry", []),
    Conf = flex_scanner_conf(Config),
    flex_compact_otp7431(error, flex_compact_otp7431_msg6(), [Conf]).

flex_compact_otp7431_msg07(suite) ->
    [];
flex_compact_otp7431_msg07(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_comppact_otp7431_msg07 -> entry", []),
    Conf = flex_scanner_conf(Config),
    flex_compact_otp7431(error, flex_compact_otp7431_msg7(), [Conf]).


flex_compact_otp7431(Expected, Msg, Conf) ->
    otp7431(Expected, megaco_compact_text_encoder, Msg, Conf).

flex_compact_otp7431_msg1() ->
    "!/1 [124.124.124.222]:55555
P=10003{C=2000{A=a4444,A=a4445{M{ST=1{L{
v=0
o=- 2890844526 2890842807 IN IP4 124.124.124.222
s=-
t= 0 0
c=IN IP4 124.124.124.222
m=audio 2222 RTP/AVP 4
a=ptime:30
a=recvonly
}}}}}}".

flex_compact_otp7431_msg2() ->
    "!/1 [124.124.124.222]:55555
P=10003{C=2000{A=a4444,A=a4445{M{ST=1{L{
v=0
o=- 2890844526 2890842807 IN IP4 124.124.124.222
s=-
t= 0 0
c=IN IP4 124.124.124.222
m=audio 2222 RTP/AVP 4
a=ptime:30
a=     }
}}}}}".


flex_compact_otp7431_msg3() ->
    "!/1 [124.124.124.222]:55555
P=10003{C=2000{A=a4444,A=a4445{M{ST=1{L{
v=0
o=- 2890844526 2890842807 IN IP4 124.124.124.222
s=-
t= 0 0
c=IN IP4 124.124.124.222
m=audio 2222 RTP/AVP 4
a=ptime:30
a     }
}}}}}".


flex_compact_otp7431_msg4() ->
    "!/1 [124.124.124.222]:55555
P=10003{C=2000{A=a4444,A=a4445{M{ST=1{L{
v=0
o=- 2890844526 2890842807 IN IP4 124.124.124.222
s=-
t= 0 0
c=IN IP4 124.124.124.222
m=audio 2222 RTP/AVP 4
a=ptime:30
a}
}}}}}".


flex_compact_otp7431_msg5() ->
    "!/1 [124.124.124.222]:55555
P=10003{C=2000{A=a4444,A=a4445{M{ST=1{L{
v=       }
}}}}}".


flex_compact_otp7431_msg6() ->
    "!/1 [124.124.124.222]:55555
P=10003{C=2000{A=a4444,A=a4445{M{ST=1{L{
v       }
}}}}}".

flex_compact_otp7431_msg7() ->
    "!/1 [124.124.124.222]:55555
P=10003{C=2000{A=a4444,A=a4445{M{ST=1{L{
v}
}}}}}".


%% ==============================================================
%%
%% P r e t t y   T e s t c a s e s
%%

pretty_otp4632_msg1(suite) ->
    [];
pretty_otp4632_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp4632_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp4632_msg1() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp4632_msg1() ->
    msg4(?MG1_MID_NO_PORT, "901 mg col boot").

pretty_otp4632_msg2(suite) ->
    [];
pretty_otp4632_msg2(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp4632_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp4632_msg2() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp4632_msg2() ->
    msg4(?MG1_MID_NO_PORT, "901").


pretty_otp4632_msg3(suite) ->
    [];
pretty_otp4632_msg3(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp4632_msg3 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_decode_encode_ok( pretty_otp4632_msg3() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp4632_msg3() ->
    M = "MEGACO/" ?VERSION_STR " [124.124.124.222]\nTransaction = 9998 {\n\tContext = - {\n\t\tServiceChange = root {\n\t\t\tServices {\n\t\t\t\tMethod = Restart,\n\t\t\t\tServiceChangeAddress = 55555,\n\t\t\t\tProfile = resgw/1,\n\t\t\t\tReason = \"901\"\n\t\t\t}\n\t\t}\n\t}\n}",
    M.


pretty_otp4632_msg4(suite) ->
    [];
pretty_otp4632_msg4(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp4632_msg4 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Check = fun(B2, B1) -> pretty_otp4632_msg4_chk(B1, B2) end, 
    ok = ticket_pretty_decode_encode_only(pretty_otp4632_msg4(), Check), 
    %% erase(severity),
    %% erase(dbg),
    ok.


pretty_otp4632_msg4() ->
    M = "MEGACO/" ?VERSION_STR " [124.124.124.222]\nTransaction = 9998 {\n\tContext = - {\n\t\tServiceChange = root {\n\t\t\tServices {\n\t\t\t\tMethod = Restart,\n\t\t\t\tServiceChangeAddress = 55555,\n\t\t\t\tProfile = resgw/1,\n\t\t\t\tReason = 901\n\t\t\t}\n\t\t}\n\t}\n}",
    M.

pretty_otp4632_msg4_chk(B1, B2) when is_binary(B1) and is_binary(B2) ->
    S1 = binary_to_list(B1),
    S2 = binary_to_list(B2),
    %%     io:format("~n"
    %% 		 "S1: ~s~n"
    %% 		 "S2: ~s~n", [S1, S2]),
    pretty_otp4632_msg4_chk(S1, S2);
 
pretty_otp4632_msg4_chk([], []) ->
    messages_not_eq; 
pretty_otp4632_msg4_chk([], Rest2) ->
    {messages_not_eq2, Rest2}; 
pretty_otp4632_msg4_chk(Rest1, []) ->
    {messages_not_eq1, Rest1}; 
pretty_otp4632_msg4_chk([$R,$e,$a,$s,$o,$n,$ ,$=,$ ,$",$9,$0,$1,$"|_Rest1],
			[$R,$e,$a,$s,$o,$n,$ ,$=,$ ,$9,$0,$1|_Rest2]) ->
    ok;
pretty_otp4632_msg4_chk([_H1|Rest1], [_H2|Rest2]) ->
    pretty_otp4632_msg4_chk(Rest1, Rest2).


%% --------------------------------------------------------------
%% 
pretty_otp4710_msg1(suite) ->
    [];
pretty_otp4710_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp4710_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp4710_msg1() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp4710_msg1() ->
    msg40().


pretty_otp4710_msg2(suite) ->
    [];
pretty_otp4710_msg2(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp4710_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Check = fun(B1, B2) -> pretty_otp4710_msg2_chk(B1, B2) end, 
    ok = ticket_pretty_decode_encode_only(pretty_otp4710_msg2(), Check), 
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp4710_msg2() ->
    "Authentication = 0xEFCDAB89:0x12345678:0x1234567889ABCDEF76543210\nMEGACO/" ?VERSION_STR " [124.124.124.222]\nTransaction = 9998 {\n\tContext = - {\n\t\tServiceChange = root {\n\t\t\tServices {\n\t\t\t\tMethod = Restart,\n\t\t\t\tServiceChangeAddress = 55555,\n\t\t\t\tProfile = resgw/1,\n\t\t\t\tReason = \"901 mg col boot\"\n\t\t\t}\n\t\t}\n\t}\n}".

pretty_otp4710_msg2_chk(B1, B2) when is_binary(B1) and is_binary(B2) ->
    S1 = binary_to_list(B1), 
    S2 = binary_to_list(B2), 
    pretty_otp4710_msg2_chk(S1, S2);

pretty_otp4710_msg2_chk(Msg, Msg) ->
    ok;

pretty_otp4710_msg2_chk(
  [$A,$u,$t,$h,$e,$n,$t,$i,$c,$a,$t,$i,$o,$n,$=,$ |Msg0],
  [$A,$u,$t,$h,$e,$n,$t,$i,$c,$a,$t,$i,$o,$n,$=,$ |Msg1]) ->
    {AH0, Rest0} = pretty_otp4710_msg2_chk_ah(Msg0, []),
    {AH1, Rest1} = pretty_otp4710_msg2_chk_ah(Msg1, []),
    case AH0 == AH1 of
	true ->
	    exit({message_not_equal, Rest0, Rest1});
	false ->
	    exit({auth_header_not_equal, AH0, AH1})
    end.

pretty_otp4710_msg2_chk_ah([], _Acc) ->
    exit(no_auth_header_found);
pretty_otp4710_msg2_chk_ah([$M,$E,$G,$A,$C,$O,$/,_|Rest], Acc) ->
    {lists:reverse(Acc), Rest};
pretty_otp4710_msg2_chk_ah([C|R], Acc) ->
    pretty_otp4710_msg2_chk_ah(R, [C|Acc]).


%% --------------------------------------------------------------
%% 
pretty_otp4945_msg1(suite) ->
    [];
pretty_otp4945_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp4945_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Check = fun(R) -> pretty_otp4945_msg1_chk(R) end,
    ok = ticket_pretty_decode_error(pretty_otp4945_msg1(), Check),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp4945_msg1() ->
"MEGACO/" ?VERSION_STR " [124.124.124.222] Transaction = 9998 {
   Context = - {
      ServiceChange = ROOT {
         Services {
            Method = Restart,
            ServiceChangeAddress = 55555, 
            Profile = ResGW/1
         }
      }
   } 
}".
    
pretty_otp4945_msg1_chk(R) when is_list(R) ->
    ExpMissing = [serviceChangeReason], 
    Check = fun(Reason) ->
		    pretty_otp4945_chk(Reason, ExpMissing)
	    end,
    ticket_check_decode_only_error_reason(R, Check).


pretty_otp4945_msg2(suite) ->
    [];
pretty_otp4945_msg2(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp4945_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Check = fun(R) -> pretty_otp4945_msg2_chk(R) end,
    ok = ticket_pretty_decode_error(pretty_otp4945_msg2(), Check),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp4945_msg2() ->
"MEGACO/" ?VERSION_STR " [124.124.124.222] Transaction = 9998 {
   Context = - {
      ServiceChange = ROOT {
         Services {
            Reason = 901,
            ServiceChangeAddress = 55555, 
            Profile = ResGW/1
         }
      }
   } 
}".

pretty_otp4945_msg2_chk(R) when is_list(R) ->
    ExpMissing = [serviceChangeMethod], 
    Check = fun(Reason) ->
		    pretty_otp4945_chk(Reason, ExpMissing)
	    end,
    ticket_check_decode_only_error_reason(R, Check).
    

pretty_otp4945_msg3(suite) ->
    [];
pretty_otp4945_msg3(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp4945_msg3 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Check = fun(R) -> pretty_otp4945_msg3_chk(R) end,
    ok = ticket_pretty_decode_error(pretty_otp4945_msg3(), Check),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp4945_msg3() ->
"MEGACO/" ?VERSION_STR " [124.124.124.222] Transaction = 9998 {
   Context = - {
      ServiceChange = ROOT {
         Services {
            ServiceChangeAddress = 55555, 
            Profile = ResGW/1
         }
      }
   } 
}".

pretty_otp4945_msg3_chk(R) when is_list(R) ->
    ExpMissing = [serviceChangeReason, serviceChangeMethod], 
    Check = fun(Reason) ->
		    pretty_otp4945_chk(Reason, ExpMissing)
	    end,
    ticket_check_decode_only_error_reason(R, Check).

    
pretty_otp4945_msg4(suite) ->
    [];
pretty_otp4945_msg4(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp4945_msg4 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_decode_only( pretty_otp4945_msg4() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp4945_msg4() ->
"MEGACO/" ?VERSION_STR " [124.124.124.222] Transaction = 9998 {
   Context = - {
      ServiceChange = ROOT {
         Services {
            Method = Restart,
            Reason = 901,
            ServiceChangeAddress = 55555, 
            Profile = ResGW/1
         }
      }
   } 
}".
    

pretty_otp4945_msg5(suite) ->
    [];
pretty_otp4945_msg5(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp4945_msg5 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Check = fun(R) -> pretty_otp4945_msg5_chk(R) end,
    ok = ticket_pretty_decode_error(pretty_otp4945_msg5(), Check),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp4945_msg5() ->
"MEGACO/" ?VERSION_STR " [124.124.124.222] Transaction = 9998 {
   Context = - {
      ServiceChange = ROOT {
         Services {
            Method = Restart,
            Reason = 901,
            Profile = ResGW/1,
            ServiceChangeAddress = 55555, 
            Profile = ResGW/2
         }
      }
   } 
}".
    
pretty_otp4945_msg5_chk(R) when is_list(R) ->
    Check = fun({at_most_once_serviceChangeParm, {profile, _, _}}) ->
		    ok;
	       (Reason) ->
		    {error, {unexpected_reason, Reason}}
	    end,
    ticket_check_decode_only_error_reason(R, Check).


pretty_otp4945_msg6(suite) ->
    [];
pretty_otp4945_msg6(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp4945_msg6 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Check = fun(R) -> pretty_otp4945_msg6_chk(R) end,
    ok = ticket_pretty_decode_error(pretty_otp4945_msg6(), Check),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp4945_msg6() ->
"MEGACO/" ?VERSION_STR " [124.124.124.222] Transaction = 9998 {
   Context = - {
      ServiceChange = ROOT {
         Services {
            Method = Restart,
               Reason = 901,
               ServiceChangeAddress = 55555, 
               MgcIdToTry = kalle, 
               Profile = ResGW/1
            }
         }
   } 
}".

pretty_otp4945_msg6_chk(R) when is_list(R) ->
    Check = fun({not_both_address_mgcid_serviceChangeParm, _, _}) ->
		    ok;
	       (Reason) ->
		    {error, {unexpected_reason, Reason}}
	    end,
    ticket_check_decode_only_error_reason(R, Check).
   

pretty_otp4945_chk({missing_required_serviceChangeParm, Missing}, 
		    ExpMissing) when is_list(Missing) ->
    case ExpMissing -- Missing of
	[] ->
	    ok;
	Diff ->
	    {error, {unexpected_missing_serviceChangeParm, Diff}}
    end;
pretty_otp4945_chk(Reason, _ExpMissing) ->
    {error, {unexpected_reason, Reason}}.


%% --------------------------------------------------------------
%% 
pretty_otp4949_msg1(suite) ->
    [];
pretty_otp4949_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp4949_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_decode_only( pretty_otp4949_msg1() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp4949_msg1() ->
"MEGACO/" ?VERSION_STR " [124.124.124.222] Reply = 9998 {
   Context = - {
      ServiceChange = ROOT {
         Services {
            ServiceChangeAddress = 55555, 
            Profile = ResGW/1
         }
      }
   } 
}".
    

pretty_otp4949_msg2(suite) ->
    [];
pretty_otp4949_msg2(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp4949_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Check = fun(R) -> pretty_otp4949_msg2_chk(R) end,
    ok = ticket_pretty_decode_error( pretty_otp4949_msg2(), Check),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp4949_msg2() ->
"MEGACO/" ?VERSION_STR " [124.124.124.222] Reply = 9998 {
   Context = - {
      ServiceChange = ROOT {
         Services {
            Profile = ResGW/1,
            ServiceChangeAddress = 55555, 
            Profile = ResGW/2
         }
      }
   } 
}".
    
pretty_otp4949_msg2_chk(R) when is_list(R) ->
    Check = fun({at_most_once_servChgReplyParm, {profile, _, _}}) ->
		    ok;
	       (Reason) ->
		    {error, {unexpected_reason, Reason}}
	    end,
    ticket_check_decode_only_error_reason(R, Check).


pretty_otp4949_msg3(suite) ->
    [];
pretty_otp4949_msg3(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp4949_msg3 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Check = fun(R) -> pretty_otp4949_msg3_chk(R) end,
    ok = ticket_pretty_decode_error( pretty_otp4949_msg3(), Check ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp4949_msg3() ->
"MEGACO/" ?VERSION_STR " [124.124.124.222] Reply = 9998 {
   Context = - {
      ServiceChange = ROOT {
         Services {
            ServiceChangeAddress = 55555, 
            MgcIdToTry = kalle, 
            Profile = ResGW/1
         }
      }
   } 
}".

pretty_otp4949_msg3_chk(R) when is_list(R) ->
    Check = fun({not_both_address_mgcid_servChgReplyParm, _, _}) ->
		    ok;
	       (Reason) ->
		    {error, {unexpected_reason, Reason}}
	    end,
    ticket_check_decode_only_error_reason(R, Check).
    

%% --------------------------------------------------------------
%% 
pretty_otp5042_msg1(suite) ->
    [];
pretty_otp5042_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5042_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_decode_only( pretty_otp5042_msg1() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp5042_msg1() ->
"MEGACO/" ?VERSION_STR " <CATAPULT>:2944
Transaction = 102 { 
Context =  5 { Notify =  MUX/1 { ObservedEvents = 1 { 
h245bh/h245msgin { Stream =  1
, h245enc =
0270020600088175000653401004100403E802E00180018001780680000034301160000700088175010101007A0100020001800001320000C0000219D005027F0070500100040100021080000319D005027F00504001008000041C001250000700088175010000400280010003000880000518AA027F400006850130008011020100000001030002000300040005000006
 }  } 
 }  }  }".
  

%% --------------------------------------------------------------
%% 
pretty_otp5068_msg1(suite) ->
    [];
pretty_otp5068_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5068_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_only( pretty_otp5068_msg1() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp5068_msg1() ->
{'MegacoMessage',
 asn1_NOVALUE,
 {'Message',
  2,
  {deviceName,[109,103,51,51]},
  {transactions,
   [{transactionReply,
     {'TransactionReply',
      190,
      asn1_NOVALUE,
      {actionReplies,
       [{'ActionReply',  %% Comments: This is repeated many times.
	 0,
	 asn1_NOVALUE,
	 asn1_NOVALUE,
	 [{auditValueReply,
	   {auditResult,
	    {'AuditResult',
	     {megaco_term_id,false,
	      [[99,101,100,101,118,49,47,52,47,49,47,49],[51,49]]},
	     [{mediaDescriptor,
	       {'MediaDescriptor',
		{'TerminationStateDescriptor',
		 [],
		 asn1_NOVALUE,
		 inSvc},
		asn1_NOVALUE
	       }
	      }
	     ]
	    }
	   }
	  }
	 ]
	}
       ]
      },asn1_NOVALUE,asn1_NOVALUE
     }
    }
   ]
  }
 }
}.
  


%% --------------------------------------------------------------
%% 
pretty_otp5085_msg1(suite) ->
    [];
pretty_otp5085_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5085_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5085_msg1() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp5085_msg1() ->
    {'MegacoMessage',
     asn1_NOVALUE,
     {'Message',
      ?VERSION,
      {deviceName,"mg36"},
      {transactions,
       [{transactionReply,
	 {'TransactionReply',
	  230,
	  asn1_NOVALUE,
	  {actionReplies,
	   [{'ActionReply', 
	     400,
	     {'ErrorDescriptor',504,asn1_NOVALUE},
	     asn1_NOVALUE,
	     []
	    }
	   ]
	  },asn1_NOVALUE,asn1_NOVALUE
	 }
	}
       ]
      }
     }
    }.


pretty_otp5085_msg2(suite) ->
    [];
pretty_otp5085_msg2(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5085_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5085_msg2() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp5085_msg2() ->
    {'MegacoMessage',
     asn1_NOVALUE,
     {'Message',
      ?VERSION,
      {deviceName,"mg36"},
      {transactions,
       [{transactionReply,
	 {'TransactionReply',
	  230,
	  asn1_NOVALUE,
	  {actionReplies,
	   [{'ActionReply', 
	     400,
	     asn1_NOVALUE,
	     asn1_NOVALUE,
	     []
	    }
	   ]
	  },asn1_NOVALUE,asn1_NOVALUE
	 }
	}
       ]
      }
     }
    }.


pretty_otp5085_msg3(suite) ->
    [];
pretty_otp5085_msg3(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5085_msg3 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5085_msg3() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp5085_msg3() ->
    {'MegacoMessage',
     asn1_NOVALUE,
     {'Message',
      ?VERSION,
      {deviceName,"mg36"},
      {transactions,
       [{transactionReply,
	 {'TransactionReply',
	  230,
	  asn1_NOVALUE,
	  {actionReplies,
	   [{'ActionReply', 
	     400,
	     asn1_NOVALUE,
	     #'ContextRequest'{priority = 3},
	     []
	    }
	   ]
	  },asn1_NOVALUE,asn1_NOVALUE
	 }
	}
       ]
      }
     }
    }.


pretty_otp5085_msg4(suite) ->
    [];
pretty_otp5085_msg4(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5085_msg4 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5085_msg4() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp5085_msg4() ->
    {'MegacoMessage',
     asn1_NOVALUE,
     {'Message',
      ?VERSION,
      {deviceName,"mg36"},
      {transactions,
       [{transactionReply,
	 {'TransactionReply',
	  230,
	  asn1_NOVALUE,
	  {actionReplies,
	   [{'ActionReply', 
	     400,
	     asn1_NOVALUE,
	     asn1_NOVALUE,
	     [{addReply,    cre_AmmsReply([#megaco_term_id{id = ?A4444}])}, 
	      {notifyReply, cre_NotifyRep([#megaco_term_id{id = ?A5555}])}]
	    }
	   ]
	  },asn1_NOVALUE,asn1_NOVALUE
	 }
	}
       ]
      }
     }
    }.


pretty_otp5085_msg5(suite) ->
    [];
pretty_otp5085_msg5(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5085_msg5 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5085_msg5() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp5085_msg5() ->
    {'MegacoMessage',
     asn1_NOVALUE,
     {'Message',
      ?VERSION,
      {deviceName,"mg36"},
      {transactions,
       [{transactionReply,
	 {'TransactionReply',
	  230,
	  asn1_NOVALUE,
	  {actionReplies,
	   [{'ActionReply', 
	     400,
	     asn1_NOVALUE,
	     #'ContextRequest'{priority = 5},
	     [{addReply,    cre_AmmsReply([#megaco_term_id{id = ?A4444}])}, 
	      {notifyReply, cre_NotifyRep([#megaco_term_id{id = ?A5555}])}]
	    }
	   ]
	  },asn1_NOVALUE,asn1_NOVALUE
	 }
	}
       ]
      }
     }
    }.


pretty_otp5085_msg6(suite) ->
    [];
pretty_otp5085_msg6(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5085_msg6 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5085_msg6() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp5085_msg6() ->
    {'MegacoMessage',
     asn1_NOVALUE,
     {'Message',
      ?VERSION,
      {deviceName,"msg36"},
      {transactions,
       [{transactionReply,
	 {'TransactionReply',
	  230,
	  asn1_NOVALUE,
	  {actionReplies,
	   [{'ActionReply', 
	     400,
	     {'ErrorDescriptor',504,asn1_NOVALUE},
	     #'ContextRequest'{priority = 6},
	     [{addReply,    cre_AmmsReply([#megaco_term_id{id = ?A4444}])}, 
	      {notifyReply, cre_NotifyRep([#megaco_term_id{id = ?A5555}])}]
	    }
	   ]
	  },asn1_NOVALUE,asn1_NOVALUE
	 }
	}
       ]
      }
     }
    }.


pretty_otp5085_msg7(suite) ->
    [];
pretty_otp5085_msg7(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5085_msg7 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5085_msg7() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp5085_msg7() ->
    {'MegacoMessage',
     asn1_NOVALUE,
     {'Message',
      ?VERSION,
      {deviceName,"msg36"},
      {transactions,
       [{transactionReply,
	 {'TransactionReply',
	  230,
	  asn1_NOVALUE,
	  {actionReplies,
	   [{'ActionReply', 
	     400,
	     {'ErrorDescriptor',504,asn1_NOVALUE},
	     #'ContextRequest'{priority = 7},
	     [{notifyReply, cre_NotifyRep([#megaco_term_id{id = ?A5555}])}]
	    }
	   ]
	  },asn1_NOVALUE,asn1_NOVALUE
	 }
	}
       ]
      }
     }
    }.


pretty_otp5085_msg8(suite) ->
    [];
pretty_otp5085_msg8(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5085_msg8 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5085_msg8() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp5085_msg8() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1 = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2 = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    {'MegacoMessage',
     asn1_NOVALUE,
     {'Message',
      ?VERSION,
      {deviceName,"msg36"},
      {transactions,
       [{transactionReply,
	 {'TransactionReply',
	  230,
	  asn1_NOVALUE,
	  {actionReplies,
	   [{'ActionReply', 
	     400,
	     {'ErrorDescriptor',504,asn1_NOVALUE},
	     #'ContextRequest'{priority = 8, 
			       emergency = true,
			       topologyReq = 
			       [#'TopologyRequest'{terminationFrom = From1,
						   terminationTo   = To1,
						   topologyDirection = bothway},
				#'TopologyRequest'{terminationFrom = From2,
						   terminationTo   = To2,
						   topologyDirection = oneway}
			       ],
			       iepscallind = true,
			       contextProp = [cre_PropParm("tdmc/gain", "2")]},
	     [{notifyReply, cre_NotifyRep([#megaco_term_id{id = ?A5555}])}]
	    }
	   ]
	  },asn1_NOVALUE,asn1_NOVALUE
	 }
	}
       ]
      }
     }
    }.


%% --------------------------------------------------------------
%% 
pretty_otp5600_msg1(suite) ->
    [];
pretty_otp5600_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5600_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5600_msg1() ),
    %% erase(severity),
    %% erase(dbg),
    ok.
 
pretty_otp5600_msg1() ->
    SRE = #'SecondRequestedEvent'{ pkgdName = "al/on",
                                   evParList = [] },
     
    SED = #'SecondEventsDescriptor'{ requestID = 2,
                                     eventList = [ SRE ] },
     
    SIG = { signal, #'Signal'{ signalName = "cg/dt",
                               sigParList = [] } },
     
    RA = #'RequestedActions'{ secondEvent = SED,
                              signalsDescriptor = [ SIG ] },

    RE = #'RequestedEvent'{ pkgdName = "al/of",
                            eventAction = RA,
                            evParList = [] },

    EV = #'EventsDescriptor'{ requestID = 1, eventList = [ RE ] },

    TermID = {megaco_term_id, true, [[$*]] },

    AMMR = #'AmmRequest'{ terminationID = [ TermID ],
                          descriptors = [ { eventsDescriptor, EV } ] },

    CR = #'CommandRequest'{command = {modReq, AMMR}},

    AR = #'ActionRequest'{contextId = ?megaco_null_context_id,
                          commandRequests = [CR]},
    ARs = [AR],
    TR = #'TransactionRequest'{transactionId = 5600, actions = ARs},
    TRs = [{transactionRequest, TR}],
    Mess = #'Message'{version = ?VERSION,
                      mId = ?MGC_MID,
                      messageBody = {transactions, TRs}},
    #'MegacoMessage'{mess = Mess}.


pretty_otp5600_msg2(suite) ->
    [];
pretty_otp5600_msg2(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5600_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5600_msg2() ),
    %% erase(severity),
    %% erase(dbg),
    ok.
 
pretty_otp5600_msg2() ->
    SIG = { signal, #'Signal'{ signalName = "cg/dt",
                               sigParList = [] } },

    SRA = #'SecondRequestedActions'{ signalsDescriptor = [ SIG ] },
    
    SRE = #'SecondRequestedEvent'{ pkgdName    = "al/on",
                                   eventAction = SRA,
                                   evParList   = [] },
    
    SED = #'SecondEventsDescriptor'{ requestID = 2,
				     eventList = [ SRE ] },
    
    RA = #'RequestedActions'{ secondEvent = SED },
    
    RE = #'RequestedEvent'{ pkgdName = "al/of",
			    eventAction = RA,
			    evParList = [] },
    
    EV = #'EventsDescriptor'{ requestID = 1, eventList = [ RE ] },
    
    TermID = {megaco_term_id, true, [[$*]] },
    
    AMMR = #'AmmRequest'{ terminationID = [ TermID ],
			  descriptors = [ { eventsDescriptor, EV } ] },
    
    CR = #'CommandRequest'{command = {modReq, AMMR}},
    
    AR = #'ActionRequest'{contextId = ?megaco_null_context_id,
                          commandRequests = [CR]},
    ARs = [AR],
    TR = #'TransactionRequest'{transactionId = 5600, actions = ARs},
    TRs = [{transactionRequest, TR}],
    Mess = #'Message'{version = ?VERSION,
                      mId = ?MGC_MID,
                      messageBody = {transactions, TRs}},
    #'MegacoMessage'{mess = Mess}.


%% --------------------------------------------------------------
%% 
pretty_otp5601_msg1(suite) ->
    [];
pretty_otp5601_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5601_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5601_msg1() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp5601_msg1() ->
    SRE1 = #'SecondRequestedEvent'{ pkgdName = "al/on",
				    evParList = [] },

    SRA = #'SecondRequestedActions'{ eventDM = { digitMapName, "dialllan0" }},

    SRE2 = #'SecondRequestedEvent'{ pkgdName = "dd/ce",
				    eventAction = SRA,
				    evParList = [] },

    SED = #'SecondEventsDescriptor'{ requestID = 2,
				     eventList = [ SRE1, SRE2 ] },

    RA = #'RequestedActions'{ secondEvent = SED },

    RE = #'RequestedEvent'{ pkgdName = "al/of",
			    eventAction = RA,
			    evParList = [] },

    EV = #'EventsDescriptor'{ requestID = 1, eventList = [ RE ] },

    TermID = {megaco_term_id, true, [[$*]] },

    AMMR = #'AmmRequest'{ terminationID = [ TermID ],
			  descriptors = [ { eventsDescriptor, EV } ] },

    CR = #'CommandRequest'{command = {modReq, AMMR}},

    AR = #'ActionRequest'{contextId = ?megaco_null_context_id,
			  commandRequests = [CR]},
    ARs = [AR],
    TR = #'TransactionRequest'{transactionId = 5600, actions = ARs},
    TRs = [{transactionRequest, TR}],
    Mess = #'Message'{version = ?VERSION,
		      mId = ?MGC_MID,
		      messageBody = {transactions, TRs}},
    #'MegacoMessage'{mess = Mess}.


%% --------------------------------------------------------------
%% 
pretty_otp5793_msg01(suite) ->
    [];
pretty_otp5793_msg01(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5793_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5793_msg1() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp5793_msg1() ->
    {'MegacoMessage',asn1_NOVALUE,
     {'Message',3,
      {deviceName,"bs_sbg_4/99"},
      {transactions,
       [{transactionReply,
	 {'TransactionReply',
	  370,
	  asn1_NOVALUE,
	  {actionReplies,
	   [{'ActionReply',
	     3,
	     asn1_NOVALUE,
	     asn1_NOVALUE,
	     [{auditValueReply,
	       {contextAuditResult,
		[{megaco_term_id,
		  false,
		  ["ip",
		   "104",
		   "1",
		   "18"]}]}},
	      {auditValueReply,
	       {contextAuditResult,
		[{megaco_term_id,
		  false,
		  ["ip",
		   "104",
		   "2",
		   "19"]
		 }
		]
	       }
	      }
	     ]
	    }
	   ]
	  },asn1_NOVALUE,asn1_NOVALUE
	 }
	}
       ]
      }
     }
    }.



%% --------------------------------------------------------------
%% 
pretty_otp5803_msg01(suite) ->
    [];
pretty_otp5803_msg01(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5803_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_decode_encode_ok( pretty_otp5803_msg1() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp5803_msg1() ->
"MEGACO/" ?VERSION_STR " [134.138.234.29]Transaction=384{
  Context=27{
    Modify=ip/104/1/76{
      Media{
        Stream=1{
          Local{},
          Remote{}
        },
        Stream=2{
          Local{},
          Remote{}
        }
      },
      Audit{
        Media{
          Stream=1{
            Statistics{*/*}
          },
          Stream=2{
            Statistics{*/*}
          }
        }
      }
    },
    Modify=ip/104/2/77{
      Media{
        Stream=1{
          Local{},
          Remote{}
        },
        Stream=2{
          Local{},
          Remote{}
        }
      }
    }
  }
}".


pretty_otp5803_msg02(suite) ->
    [];
pretty_otp5803_msg02(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5803_msg02 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_decode_encode_ok( pretty_otp5803_msg2() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp5803_msg2() ->
"MEGACO/" ?VERSION_STR " [134.138.234.29]Transaction=384{
  Context=27{
    Modify=ip/104/1/76{
      Media{
        Stream=1{
           Local{},
           Remote{}
        },
        Stream=2{
          Local{},
          Remote{}
        }
      },
      Audit{
        Media{
          Stream=1{
            Statistics{*/*}
          }
        }
      }
    },
    Modify=ip/104/2/77{
      Media{
        Stream=1{
          Local{},
          Remote{}
        },
        Stream=2{
          Local{},
          Remote{}
        }
      }
    }
  }
}".


%% --------------------------------------------------------------
%% 
pretty_otp5805_msg01(suite) ->
    [];
pretty_otp5805_msg01(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5805_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_decode_error( pretty_otp5805_msg1() ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp5805_msg1() ->
"MEGACO/4 [134.138.234.29]
Transaction=1{
  Context=*{
    AuditValue=ip/0/*{
      Audit{}
    }
  }
}".


%% --------------------------------------------------------------
%% 
pretty_otp5836_msg01(suite) ->
    [];
pretty_otp5836_msg01(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5836_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( compact_otp5836_msg1() ),
    %% erase(severity),
    %% erase(dbg),
    ok.


%% --------------------------------------------------------------
%% 
pretty_otp5882_msg01(suite) ->
    [];
pretty_otp5882_msg01(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("pretty_otp5882_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Check = fun(R) -> pretty_otp5882_msg01_chk(R) end,
    ok = ticket_pretty_encode_error( pretty_otp5882_msg01(), Check ),
    %% erase(severity),
    %% erase(dbg),
    ok.
    
pretty_otp5882_msg01_chk({message_encode_failed, {error, {Reason, _}}, _}) ->
    case Reason of
	{invalid_LocalControlDescriptor, empty} ->
	    ok;
	_ ->
	    {error, {unexpected_error_actual_reason, Reason}}
    end;
pretty_otp5882_msg01_chk(Reason) ->
    {error, {unexpected_reason, Reason}}.

    
pretty_otp5882_msg01() ->
    LCD = #'LocalControlDescriptor'{}, % Create illegal LCD
    Parms      = cre_StreamParms(LCD),
    StreamDesc = cre_StreamDesc(1, Parms),
    MediaDesc  = cre_MediaDesc(StreamDesc),
    AmmReq     = cre_AmmReq([#megaco_term_id{id = ?A4445}],
			    [{mediaDescriptor, MediaDesc}]),
    CmdReq     = cre_CmdReq({modReq, AmmReq}),
    CID        = cre_CtxID(7301),
    ActReq     = cre_ActReq(CID, [CmdReq]),
    Actions    = [ActReq],
    TransId    = cre_TransId(7302),
    TransReq   = cre_TransReq(TransId, Actions),
    Trans      = cre_Trans(TransReq),
    Mid        = ?MG1_MID,
    Mess       = cre_Msg(Mid, [Trans]),
    cre_MegacoMessage(Mess).
    


%% --------------------------------------------------------------
%% 
pretty_otp6490_msg01(suite) ->
    [];
pretty_otp6490_msg01(Config) when is_list(Config) ->
    %% put(severity, trc),
    %% put(dbg,      true),
    d("pretty_otp6490_msg01 -> entry", []),
    %% ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp6490_msg01(), [] ),
    %% erase(dbg),
    erase(severity),
    ok.
    
pretty_otp6490_msg02(suite) ->
    [];
pretty_otp6490_msg02(Config) when is_list(Config) ->
    %% put(severity, trc),
    %% put(dbg,      true),
    d("pretty_otp6490_msg02 -> entry", []),
    %% ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp6490_msg02(), [] ),
    %% erase(severity),
    %% erase(dbg),
    ok.
    
pretty_otp6490_msg03(suite) ->
    [];
pretty_otp6490_msg03(Config) when is_list(Config) ->
    %% put(severity, trc),
    %% put(dbg,      true),
    d("pretty_otp6490_msg03 -> entry", []),
    %% ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp6490_msg03(), [] ),
    %% erase(severity),
    %% erase(dbg),
    ok.
    
pretty_otp6490_msg04(suite) ->
    [];
pretty_otp6490_msg04(Config) when is_list(Config) ->
    %% put(severity, trc),
    %% put(dbg,      true),
    d("pretty_otp6490_msg04 -> entry", []),
    %% ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp6490_msg04(), [] ),
    %% erase(severity),
    %% erase(dbg),
    ok.
    
pretty_otp6490_msg05(suite) ->
    [];
pretty_otp6490_msg05(Config) when is_list(Config) ->
    %% put(severity, trc),
    %% put(dbg,      true),
    d("pretty_otp6490_msg05 -> entry", []),
    %% ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp6490_msg05(), [] ),
    %% erase(severity),
    %% erase(dbg),
    ok.
    
pretty_otp6490_msg06(suite) ->
    [];
pretty_otp6490_msg06(Config) when is_list(Config) ->
    %% put(severity, trc),
    %% put(dbg,      true),
    d("pretty_otp6490_msg06 -> entry", []),
    %% ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp6490_msg06(), [] ),
    %% erase(severity),
    %% erase(dbg),
    ok.
    
pretty_otp6490_msg(EBD) ->
    AmmDesc    = ?MSG_LIB:cre_AmmDescriptor(EBD),
    AmmReq     = cre_AmmReq([#megaco_term_id{id = ?A4445}], [AmmDesc]),
    CmdReq     = cre_CmdReq({modReq, AmmReq}),
    CID        = cre_CtxID(64901),
    ActReq     = cre_ActReq(CID, [CmdReq]),
    Actions    = [ActReq],
    TransId    = cre_TransId(64902),
    TransReq   = cre_TransReq(TransId, Actions),
    Trans      = cre_Trans(TransReq),
    Mid        = ?MG1_MID,
    Mess       = cre_Msg(Mid, [Trans]),
    cre_MegacoMessage(Mess).

pretty_otp6490_msg01() ->
    EvSpecs = [], % This will result in an error
    EBD     = EvSpecs, % This is because the lib checks that the size is valid
    pretty_otp6490_msg(EBD).
    
pretty_otp6490_msg02() ->
    EvPar    = ?MSG_LIB:cre_EventParameter("sune", ["mangs"]),
    PkgdName = ?MSG_LIB:cre_PkgdName("foo", "a"),
    EvName   = ?MSG_LIB:cre_EventName(PkgdName),
    EvSpec   = ?MSG_LIB:cre_EventSpec(EvName, [EvPar]),
    EvSpecs  = [EvSpec], 
    EBD      = ?MSG_LIB:cre_EventBufferDescriptor(EvSpecs),
    pretty_otp6490_msg(EBD).
    
pretty_otp6490_msg03() ->
    EvPar1   = ?MSG_LIB:cre_EventParameter("sune",   ["mangs"]),
    EvPar2   = ?MSG_LIB:cre_EventParameter("kalle",  ["anka"]),
    EvPar3   = ?MSG_LIB:cre_EventParameter("flippa", ["ur"]),
    PkgdName = ?MSG_LIB:cre_PkgdName("foo", "a"),
    EvName   = ?MSG_LIB:cre_EventName(PkgdName),
    EvSpec   = ?MSG_LIB:cre_EventSpec(EvName, [EvPar1,EvPar2,EvPar3]),
    EvSpecs  = [EvSpec], 
    EBD      = ?MSG_LIB:cre_EventBufferDescriptor(EvSpecs),
    pretty_otp6490_msg(EBD).
    
pretty_otp6490_msg04() ->
    EvPar1    = ?MSG_LIB:cre_EventParameter("sune",   ["mangs"]),
    EvPar2    = ?MSG_LIB:cre_EventParameter("kalle",  ["anka"]),
    EvPar3    = ?MSG_LIB:cre_EventParameter("flippa", ["ur"]),
    PkgdName1 = ?MSG_LIB:cre_PkgdName("foo", "a"),
    EvName1   = ?MSG_LIB:cre_EventName(PkgdName1),
    EvSpec1   = ?MSG_LIB:cre_EventSpec(EvName1, [EvPar1,EvPar2,EvPar3]),
    EvPar4    = ?MSG_LIB:cre_EventParameter("hej",    ["hopp"]),
    PkgdName2 = ?MSG_LIB:cre_PkgdName("bar", "b"),
    EvName2   = ?MSG_LIB:cre_EventName(PkgdName2),
    EvSpec2   = ?MSG_LIB:cre_EventSpec(EvName2, [EvPar4]),
    EvSpecs   = [EvSpec1,EvSpec2], 
    EBD       = ?MSG_LIB:cre_EventBufferDescriptor(EvSpecs),
    pretty_otp6490_msg(EBD).
    
pretty_otp6490_msg05() ->
    EvPar    = ?MSG_LIB:cre_EventParameter("sune", ["mangs"]),
    PkgdName = ?MSG_LIB:cre_PkgdName("foo", root),
    EvName   = ?MSG_LIB:cre_EventName(PkgdName),
    EvSpec   = ?MSG_LIB:cre_EventSpec(EvName, [EvPar]),
    EvSpecs  = [EvSpec], 
    EBD      = ?MSG_LIB:cre_EventBufferDescriptor(EvSpecs),
    pretty_otp6490_msg(EBD).
    
pretty_otp6490_msg06() ->
    EvPar    = ?MSG_LIB:cre_EventParameter("sune", ["mangs"]),
    PkgdName = ?MSG_LIB:cre_PkgdName(root, root),
    EvName   = ?MSG_LIB:cre_EventName(PkgdName),
    EvSpec   = ?MSG_LIB:cre_EventSpec(EvName, [EvPar]),
    EvSpecs  = [EvSpec], 
    EBD      = ?MSG_LIB:cre_EventBufferDescriptor(EvSpecs),
    pretty_otp6490_msg(EBD).
    

%% --------------------------------------------------------------
%%

pretty_otp7671_msg01(suite) ->
    [];
pretty_otp7671_msg01(Config) when is_list(Config) ->
%%     put(severity, trc),
%%     put(dbg,      true),
    d("pretty_otp7671_msg01 -> entry", []),
    %% ?ACQUIRE_NODES(1, Config),
    ok = pretty_otp7671( pretty_otp7671_msg01(), [] ),
%%     erase(dbg),
%%     erase(severity),
    ok.

pretty_otp7671_msg02(suite) ->
    [];
pretty_otp7671_msg02(Config) when is_list(Config) ->
%%     put(severity, trc),
%%     put(dbg,      true),
    d("pretty_otp7671_msg02 -> entry", []),
    %% ?ACQUIRE_NODES(1, Config),
    ok = pretty_otp7671( pretty_otp7671_msg02(), [] ),
%%     erase(dbg),
%%     erase(severity),
   ok.

pretty_otp7671_msg03(suite) ->
    [];
pretty_otp7671_msg03(Config) when is_list(Config) ->
%%     put(severity, trc),
%%     put(dbg,      true),
    d("pretty_otp7671_msg03 -> entry", []),
    %% ?ACQUIRE_NODES(1, Config),
    ok = pretty_otp7671( pretty_otp7671_msg03(), [] ),
%%     erase(dbg),
%%     erase(severity),
    ok.

pretty_otp7671_msg04(suite) ->
    [];
pretty_otp7671_msg04(Config) when is_list(Config) ->
%%     put(severity, trc),
%%     put(dbg,      true),
    d("pretty_otp7671_msg04 -> entry", []),
    %% ?ACQUIRE_NODES(1, Config),
    ok = pretty_otp7671( pretty_otp7671_msg04(), [] , error, ignore),
%%     erase(dbg),
%%     erase(severity),
    ok.

pretty_otp7671_msg05(suite) ->
    [];
pretty_otp7671_msg05(Config) when is_list(Config) ->
%%     put(severity, trc),
%%     put(dbg,      true),
    d("pretty_otp7671_msg05 -> entry", []),
    Check = fun(M1, M2) -> cmp_otp7671_msg05(M1, M2) end,
    ok = pretty_otp7671( pretty_otp7671_msg05(), [] , ok, ok, Check),
%%     erase(dbg),
%%     erase(severity),
    ok.


pretty_otp7671(Msg, Conf) ->
    pretty_otp7671(Msg, Conf, ok).

pretty_otp7671(Msg, Conf, ExpectedEncode) ->
    pretty_otp7671(Msg, Conf, ExpectedEncode, ok).

pretty_otp7671(Msg, Conf, ExpectedEncode, ExpectedDecode) ->
    otp7671(Msg, megaco_pretty_text_encoder, Conf,
            ExpectedEncode, ExpectedDecode).

pretty_otp7671(Msg, Conf, ExpectedEncode, ExpectedDecode, Check) ->
    otp7671(Msg, megaco_pretty_text_encoder, Conf,
            ExpectedEncode, ExpectedDecode, Check).

otp7671(Msg, Codec, Conf, ExpectedEncode, ExpectedDecode) ->
    Check = fun(M1, M2) -> 
		    exit({unexpected_decode_result, M1, M2})
	    end,
    otp7671(Msg, Codec, Conf, ExpectedEncode, ExpectedDecode, Check).

otp7671(Msg, Codec, Conf, ExpectedEncode, ExpectedDecode, Check) ->
    case (catch encode_message(Codec, Conf, Msg)) of
        {error, _Reason} when ExpectedEncode =:= error ->
            ok;
        {error, Reason} when ExpectedEncode =:= ok ->
            exit({unexpected_encode_failure, Reason});
        {ok, Bin} when ExpectedEncode =:= error ->
            exit({unexpected_encode_success, Msg, binary_to_list(Bin)});
        {ok, Bin} when ExpectedEncode =:= ok ->
            case decode_message(Codec, false, Conf, Bin) of
                {ok, Msg} when ExpectedDecode =:= ok ->
		    ok;
                {ok, Msg2} when ExpectedDecode =:= ok ->
		    Check(Msg, Msg2);
                {ok, Msg} when ExpectedDecode =:= error ->
                    exit({unexpected_decode_success, Msg});
                {ok, Msg2} when ExpectedDecode =:= error ->
                    exit({unexpected_decode_success, Msg, Msg2});
                {error, _Reason} when ExpectedDecode =:= error ->
                    ok;
                {error, Reason} when ExpectedDecode == ok ->
                    exit({unexpected_decode_failure, Msg, Reason})
            end
    end.


pretty_otp7671_msg(DigitMapDesc) ->
    AmmReq = cre_AmmReq([#megaco_term_id{id = ?A4444}],
			[{digitMapDescriptor, DigitMapDesc}]),
    CmdReq = cre_CmdReq({modReq, AmmReq}),
    msg_request(?MGC_MID, 10001, ?megaco_null_context_id, [CmdReq]).

pretty_otp7671_msg01() ->
    Name         = "dialplan01",
    DigitMapDesc = cre_DigitMapDesc(Name),
    pretty_otp7671_msg(DigitMapDesc).

pretty_otp7671_msg02() ->
    Name         = "dialplan02",
    Body         = "(0s| 00s|[1-7]xlxx|8lxxxxxxx|#xxxxxxx|*xx|9l1xxxxxxxxxx|9l011x.s)",
    Value        = cre_DigitMapValue(Body),
    DigitMapDesc = cre_DigitMapDesc(Name, Value),
    pretty_otp7671_msg(DigitMapDesc).

pretty_otp7671_msg03() ->
    Body         = "(0s| 00s|[1-7]xlxx|8lxxxxxxx|#xxxxxxx|*xx|9l1xxxxxxxxxx|9l011x.s)",
    Value        = cre_DigitMapValue(Body),
    DigitMapDesc = cre_DigitMapDesc(Value),
    pretty_otp7671_msg(DigitMapDesc).

pretty_otp7671_msg04() ->
    DigitMapDesc = cre_DigitMapDesc(),
    pretty_otp7671_msg(DigitMapDesc).

pretty_otp7671_msg05() ->
    {'MegacoMessage',asn1_NOVALUE,
     {'Message',?VERSION,
      {domainName,{'DomainName',"tgc",asn1_NOVALUE}},
      {transactions,
       [{transactionRequest,
	 {'TransactionRequest',12582952,
	  [{'ActionRequest',0,asn1_NOVALUE,asn1_NOVALUE,
	    [{'CommandRequest',
	      {modReq,
	       {'AmmRequest',
		[{megaco_term_id,false,["root"]}],
		[{digitMapDescriptor,
		  {'DigitMapDescriptor',"dialplan1",
		   {'DigitMapValue',asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,[],
		    asn1_NOVALUE}}}]}},
	      asn1_NOVALUE,asn1_NOVALUE}]}]}}]}}}.

cmp_otp7671_msg05(#'MegacoMessage'{authHeader = asn1_NOVALUE, 
				   mess       = M1}, 
		  #'MegacoMessage'{authHeader = asn1_NOVALUE, 
				   mess       = M2}) ->
    #'Message'{messageBody = Body1} = M1,
    #'Message'{messageBody = Body2} = M2,
    {transactions, Trans1} = Body1,
    {transactions, Trans2} = Body2,
    [{transactionRequest, TR1}] = Trans1,
    [{transactionRequest, TR2}] = Trans2,
    #'TransactionRequest'{actions = Acts1} = TR1,
    #'TransactionRequest'{actions = Acts2} = TR2,
    [#'ActionRequest'{commandRequests = CR1}] = Acts1,
    [#'ActionRequest'{commandRequests = CR2}] = Acts2,
    [#'CommandRequest'{command = Cmd1}] = CR1,
    [#'CommandRequest'{command = Cmd2}] = CR2,
    {modReq, #'AmmRequest'{descriptors = Descs1}} = Cmd1,
    {modReq, #'AmmRequest'{descriptors = Descs2}} = Cmd2,
    [{digitMapDescriptor, 
      #'DigitMapDescriptor'{digitMapName = Name, 
			    digitMapValue = Value1}}] = Descs1,
    [{digitMapDescriptor, 
      #'DigitMapDescriptor'{digitMapName = Name, 
			    digitMapValue = Value2}}] = Descs2,
    #'DigitMapValue'{startTimer    = asn1_NOVALUE,
		     shortTimer    = asn1_NOVALUE,
		     longTimer     = asn1_NOVALUE,
		     digitMapBody  = [],
		     durationTimer = asn1_NOVALUE} = Value1,
    asn1_NOVALUE = Value2,
    ok.
    

%% --------------------------------------------------------------
%%


pretty_otp8114_msg01(suite) ->
    [];
pretty_otp8114_msg01(Config) when is_list(Config) ->
    put(severity, trc),
    put(dbg,      true),
    d("pretty_otp8114_msg01 -> entry", []),
    ok = otp8114( pretty_otp8114_msg01(), megaco_pretty_text_encoder, ?EC),
    erase(dbg),
    erase(severity),
    ok.

pretty_otp8114_msg01() ->
    "MEGACO/" ?VERSION_STR  " [10.10.10.10]:1234\nTransaction = 1 {\n\tContext =\n1 {\n\t\tModify = ip/1/1/1 {\n\t\t\tMedia {\n\t\t\t\tStream = 1\n{\n\t\t\t\t\t\tLocalControl {\n\t\t\t\t\t\tMode =\nSendReceive\n\t\t\t\t\t}\n\t\t\t\t}\n\t\t\t},\n\t\t\tEvents = 1\n{\n\t\t\t\tadid/ipstop\n{\n\t\t\t\t\tdt=30,\n\t\t\t\t\tdir=\"BOTH\"\n\t\t\t\t},\n\t\t\t\tg/cause\n\n\t\t\t}\n\t\t}\n\t}\n}".


otp8114(InitialMessage, Codec, Conf) ->
    Decode = fun(M) -> Codec:decode_message(Conf, M) end,
    Encode = fun(B) -> Codec:encode_message(Conf, B) end,
    InitialData = InitialMessage, 
    Instructions = 
	[
	 %% List to binary
	 megaco_codec_test_lib:expect_instruction(
	   "Convert (initial) message to a binary", 
	   fun(Msg) when is_list(Msg) ->
		   %% io:format("~s~n", [Msg]),
		   {ok, list_to_binary(Msg)};
	      (Bad) ->
		   {error, {invalid_data, Bad}}
	   end,
	   fun({ok, Bin}, _Msg) when is_binary(Bin) ->
		   {ok, Bin};
	      (Bad, _Msg) ->
		   {error, {failed_to_binary, Bad}}
	   end),
	 
	 %% Initial decode
	 megaco_codec_test_lib:expect_instruction(
	   "Decode (initial) message",
	   fun(Bin) when is_binary(Bin) ->
		   (catch Decode(Bin));
	      (Bad) ->
		   {error, {invalid_data, Bad}}
	   end,
	   fun({ok, Msg}, _Bin) when is_record(Msg, 'MegacoMessage') ->
		   %% io:format("~p~n", [Msg]),
		   {ok, Msg};
	      (Bad, _) ->
		   {error, {initial_decode_failed, Bad}}
	   end),

	 %% Encode
	 megaco_codec_test_lib:expect_instruction(
	   "Encode message",
	   fun(Msg) when is_record(Msg, 'MegacoMessage') ->
		   (catch Encode(Msg));
	      (Bad) ->
		   {error, {invalid_data, Bad}}
	   end,
	   fun({ok, Bin}, _Msg) when is_binary(Bin) ->
		   %% io:format("~s~n", [binary_to_list(Bin)]),
		   {ok, Bin};
	      (Bad, _) ->
		   {error, {encode_failed, Bad}}
	   end),
	 
	 %% Decode
	 megaco_codec_test_lib:expect_instruction(
	   "(final) Decode message",
	   fun(Bin) when is_binary(Bin) ->
		   (catch Decode(Bin));
	      (Bad) ->
		   {error, {invalid_data, Bad}}
	   end,
	   fun({ok, Msg}, _Bin) when is_record(Msg, 'MegacoMessage') ->
		   %% io:format("~p~n", [Msg]),
		   {ok, Msg};
	      (Bad, _) ->
		   {error, {decode_failed, Bad}}
	   end)
	],
    megaco_codec_test_lib:expect_exec(Instructions, InitialData).


%% ==============================================================
%%
%% F l e x   P r e t t y   T e s t c a s e s
%%

flex_pretty_otp5042_msg1(suite) ->
    [];
flex_pretty_otp5042_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_pretty_otp5042_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    ok = ticket_pretty_decode_only( pretty_otp5042_msg1() ),
    %% erase(severity),
    %% erase(dbg),
    ok.


%% --------------------------------------------------------------
%% 
flex_pretty_otp5085_msg1(suite) ->
    [];
flex_pretty_otp5085_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_pretty_otp5085_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5085_msg1(), [Conf] ),
    %% erase(severity),
    %% erase(dbg),
    ok.

flex_pretty_otp5085_msg2(suite) ->
    [];
flex_pretty_otp5085_msg2(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_pretty_otp5085_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5085_msg1(), [Conf] ),
    %% erase(severity),
    %% erase(dbg),
    ok.

flex_pretty_otp5085_msg3(suite) ->
    [];
flex_pretty_otp5085_msg3(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_pretty_otp5085_msg3 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5085_msg1(), [Conf] ),
    %% erase(severity),
    %% erase(dbg),
    ok.

flex_pretty_otp5085_msg4(suite) ->
    [];
flex_pretty_otp5085_msg4(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_pretty_otp5085_msg4 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5085_msg1(), [Conf] ),
    %% erase(severity),
    %% erase(dbg),
    ok.

flex_pretty_otp5085_msg5(suite) ->
    [];
flex_pretty_otp5085_msg5(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_pretty_otp5085_msg5 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5085_msg1(), [Conf] ),
    %% erase(severity),
    %% erase(dbg),
    ok.

flex_pretty_otp5085_msg6(suite) ->
    [];
flex_pretty_otp5085_msg6(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_pretty_otp5085_msg6 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5085_msg1(), [Conf] ),
    %% erase(severity),
    %% erase(dbg),
    ok.

flex_pretty_otp5085_msg7(suite) ->
    [];
flex_pretty_otp5085_msg7(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_pretty_otp5085_msg7 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5085_msg1(), [Conf] ),
    %% erase(severity),
    %% erase(dbg),
    ok.

flex_pretty_otp5085_msg8(suite) ->
    [];
flex_pretty_otp5085_msg8(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_pretty_otp5085_msg8 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5085_msg1(), [Conf] ),
    %% erase(severity),
    %% erase(dbg),
    ok.


%% --------------------------------------------------------------
%% 
flex_pretty_otp5600_msg1(suite) ->
    [];
flex_pretty_otp5600_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_pretty_otp5600_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5600_msg1(), [Conf] ),
    %% erase(severity),
    %% erase(dbg),
    ok.

flex_pretty_otp5600_msg2(suite) ->
    [];
flex_pretty_otp5600_msg2(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_pretty_otp5600_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5600_msg2(), [Conf] ),
    %% erase(severity),
    %% erase(dbg),
    ok.


%% --------------------------------------------------------------
%% 
flex_pretty_otp5601_msg1(suite) ->
    [];
flex_pretty_otp5601_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_pretty_otp5601_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5601_msg1(), [Conf] ),
    %% erase(severity),
    %% erase(dbg),
    ok.


%% --------------------------------------------------------------
%% 
flex_pretty_otp5793_msg01(suite) ->
    [];
flex_pretty_otp5793_msg01(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_pretty_otp5793_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    ok = ticket_pretty_encode_decode_ok( pretty_otp5793_msg1(), [Conf] ),
    %% erase(severity),
    %% erase(dbg),
    ok.


%% --------------------------------------------------------------
%% 
flex_pretty_otp5803_msg01(suite) ->
    [];
flex_pretty_otp5803_msg01(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_pretty_otp5803_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    ok = ticket_pretty_decode_encode_ok( pretty_otp5803_msg1(), [Conf] ),
    %% erase(severity),
    %% erase(dbg),
    ok.

flex_pretty_otp5803_msg02(suite) ->
    [];
flex_pretty_otp5803_msg02(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_pretty_otp5803_msg02 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    ok = ticket_pretty_decode_encode_ok( pretty_otp5803_msg2(), [Conf] ),
    %% erase(severity),
    %% erase(dbg),
    ok.


%% --------------------------------------------------------------
%% 
flex_pretty_otp5805_msg01(suite) ->
    [];
flex_pretty_otp5805_msg01(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_pretty_otp5805_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    ok = ticket_pretty_decode_error( pretty_otp5805_msg1(), [Conf] ),
    %% erase(severity),
    %% erase(dbg),
    ok.


%% --------------------------------------------------------------
%% 
flex_pretty_otp5836_msg01(suite) ->
    [];
flex_pretty_otp5836_msg01(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_pretty_otp5836_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    ok = ticket_pretty_encode_decode_ok( compact_otp5836_msg1(), [Conf] ),
    %% erase(severity),
    %% erase(dbg),
    ok.


flex_pretty_otp7431_msg01(suite) ->
    [];
flex_pretty_otp7431_msg01(Config) when is_list(Config) ->
    d("flex_pretty_otp7431_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    flex_pretty_otp7431(ok, flex_pretty_otp7431_msg1(), [Conf]).

flex_pretty_otp7431_msg02(suite) ->
    [];
flex_pretty_otp7431_msg02(Config) when is_list(Config) ->
    %%     put(severity,trc),
    %%     put(dbg,true),
    d("flex_pretty_otp7431_msg02 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    flex_pretty_otp7431(error, flex_pretty_otp7431_msg2(), [?EC_V3,Conf]).

flex_pretty_otp7431_msg03(suite) ->
    [];
flex_pretty_otp7431_msg03(Config) when is_list(Config) ->
    %%     put(severity,trc),
    %%     put(dbg,true),
    d("flex_pretty_otp7431_msg03 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    flex_pretty_otp7431(error, flex_pretty_otp7431_msg3(), [?EC_V3,Conf]).

flex_pretty_otp7431_msg04(suite) ->
    [];
flex_pretty_otp7431_msg04(Config) when is_list(Config) ->
    d("flex_pretty_otp7431_msg04 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    flex_pretty_otp7431(error, flex_pretty_otp7431_msg4(), [?EC_V3,Conf]).

flex_pretty_otp7431_msg05(suite) ->
    [];
flex_pretty_otp7431_msg05(Config) when is_list(Config) ->
    d("flex_pretty_otp7431_msg05 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    flex_pretty_otp7431(error, flex_pretty_otp7431_msg5(), [?EC_V3,Conf]).

flex_pretty_otp7431_msg06(suite) ->
    [];
flex_pretty_otp7431_msg06(Config) when is_list(Config) ->
    d("flex_pretty_otp7431_msg06 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    flex_pretty_otp7431(error, flex_pretty_otp7431_msg6(), [?EC_V3,Conf]).

flex_pretty_otp7431_msg07(suite) ->
    [];
flex_pretty_otp7431_msg07(Config) when is_list(Config) ->
    d("flex_pretty_otp7431_msg07 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    flex_pretty_otp7431(error, flex_pretty_otp7431_msg7(), [?EC_V3,Conf]).

flex_pretty_otp7431(Expected, Msg, Conf) ->
    otp7431(Expected, megaco_pretty_text_encoder, Msg, Conf).

otp7431(Expected, Codec, Msg0, Conf) ->
    Bin0 = list_to_binary(Msg0),
    case decode_message(Codec, false, Conf, Bin0) of
	{ok, _Msg1} when Expected =:= ok ->
 	    io:format(" decoded", []);
	{error, {bad_property_parm, Reason}} when (Expected =:= error) andalso 
						  is_list(Reason) ->
	    io:format("expected result: ~s", [Reason]),
	    ok;
	Else ->
	    io:format("unexpected result", []),
	    exit({unexpected_decode_result, Else})
    end.


flex_pretty_otp7431_msg1() ->
    "MEGACO/" ?VERSION_STR " [124.124.124.222]:55555 Reply = 10003 {
   Context = 2000 {
	       Add = A4444,
	       Add = A4445 {
		       Media {
			 Stream = 1 {
				    Local { 
				      v=0 
				      o=- 2890844526 2890842807 IN IP4 124.124.124.222 
				      s=- 
				      t= 0 0 
				      c=IN IP4 124.124.124.222 
				      m=audio 2222 RTP/AVP 4 
				      a=ptime:30 
				      a=recvonly 
				     } ; RTP profile for G.723.1 is 4
				   }
			}
		      }
	      } 
       }".

flex_pretty_otp7431_msg2() ->
    "MEGACO/" ?VERSION_STR " [124.124.124.222]:55555 Reply = 10003 {
   Context = 2000 {
	       Add = A4444,
	       Add = A4445 {
		       Media {
			 Stream = 1 {
				    Local { 
				      v=0 
				      o=- 2890844526 2890842807 IN IP4 124.124.124.222 
				      s=- 
				      t= 0 0 
				      c=IN IP4 124.124.124.222 
				      m=audio 2222 RTP/AVP 4 
				      a=ptime:30 
				      a=             }
				   }
			}
		      }
	      } 
       }".

flex_pretty_otp7431_msg3() ->
    "MEGACO/" ?VERSION_STR " [124.124.124.222]:55555 Reply = 10003 {
   Context = 2000 {
	       Add = A4444,
	       Add = A4445 {
		       Media {
			 Stream = 1 {
				    Local { 
				      v=0 
				      o=- 2890844526 2890842807 IN IP4 124.124.124.222 
				      s=- 
				      t= 0 0 
				      c=IN IP4 124.124.124.222 
				      m=audio 2222 RTP/AVP 4 
				      a=ptime:30 
				      a             }
				   }
			}
		      }
	      } 
       }".

flex_pretty_otp7431_msg4() ->
    "MEGACO/" ?VERSION_STR " [124.124.124.222]:55555 Reply = 10003 {
   Context = 2000 {
	       Add = A4444,
	       Add = A4445 {
		       Media {
			 Stream = 1 {
				    Local { 
				      v=0 
				      o=- 2890844526 2890842807 IN IP4 124.124.124.222 
				      s=- 
				      t= 0 0 
				      c=IN IP4 124.124.124.222 
				      m=audio 2222 RTP/AVP 4 
				      a=ptime:30 
				      a}
				   }
			}
		      }
	      } 
       }".

flex_pretty_otp7431_msg5() ->
    "MEGACO/" ?VERSION_STR " [124.124.124.222]:55555 Reply = 10003 {
   Context = 2000 {
	       Add = A4444,
	       Add = A4445 {
		       Media {
			 Stream = 1 {
				    Local { 
				      v=            }
				   }
			}
		      }
	      } 
       }".

flex_pretty_otp7431_msg6() ->
    "MEGACO/" ?VERSION_STR " [124.124.124.222]:55555 Reply = 10003 {
   Context = 2000 {
	       Add = A4444,
	       Add = A4445 {
		       Media {
			 Stream = 1 {
				    Local { 
				      v            }
				   }
			}
		      }
	      } 
       }".

flex_pretty_otp7431_msg7() ->
    "MEGACO/" ?VERSION_STR " [124.124.124.222]:55555 Reply = 10003 {
   Context = 2000 {
	       Add = A4444,
	       Add = A4445 {
		       Media {
			 Stream = 1 {
				    Local { 
				      v}
				   }
			}
		      }
	      } 
       }".



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

msgs() ->
    [M || {_, M, _, _} <- msgs(text)].

msgs(Encoding) ->
    msgs1a(Encoding) ++ 
	msgs1b(Encoding) ++ 
	msgs3525(Encoding) ++ 
	msgs5(Encoding) ++ 
	msgs6(Encoding) ++ 
	msgs7(Encoding).

msgs1a(_) ->
    Plain = 
	fun(Codec, DD, Ver, EC, M) ->
		megaco_codec_test_lib:plain_encode_decode(Codec, DD, Ver, 
							  EC, M) 
	end,
    [
     {msg01a, msg1a(),  Plain, [{dbg,false}]}, 
     {msg01b, msg1b(),  Plain, [{dbg,false}]},
     {msg02,  msg2(),   Plain, [{dbg,false}]},  
     {msg03,  msg3(),   Plain, [{dbg,false}]},  
     {msg04,  msg4(),   Plain, [{dbg,false}]},  
     {msg05,  msg5(),   Plain, [{dbg,false}]}, 
     {msg06a, msg6a(),  Plain, [{dbg,false}]}, 
     {msg06b, msg6b(),  Plain, [{dbg,false}]}, 
     {msg07,  msg7(),   Plain, [{dbg,false}]}, 
     {msg08a, msg8a(),  Plain, [{dbg,false}]}, 
     {msg08b, msg8b(),  Plain, [{dbg,false}]}, 
     {msg09,  msg9(),   Plain, [{dbg,false}]}, 
     {msg10,  msg10(),  Plain, [{dbg,false}]}, 
     {msg11,  msg11(),  Plain, [{dbg,false}]}, 
     {msg12,  msg12(),  Plain, [{dbg,false}]}, 
     {msg13,  msg13(),  Plain, [{dbg,false}]}, 
     {msg14,  msg14(),  Plain, [{dbg,false}]}, 
     {msg15,  msg15(),  Plain, [{dbg,false}]}, 
     {msg16,  msg16(),  Plain, [{dbg,false}]}, 
     {msg17,  msg17(),  Plain, [{dbg,false}]}, 
     {msg18,  msg18(),  Plain, [{dbg,false}]}, 
     {msg19,  msg19(),  Plain, [{dbg,false}]}, 
     {msg20,  msg20(),  Plain, [{dbg,false}]}, 
     {msg21,  msg21(),  Plain, [{dbg,false}]},
     {msg22a, msg22a(), Plain, [{dbg,false}]}, 
     {msg22b, msg22b(), Plain, [{dbg,false}]}, 
     {msg22c, msg22c(), Plain, [{dbg,false}]}, 
     {msg22d, msg22d(), Plain, [{dbg,false}]}, 
     {msg22e, msg22e(), Plain, [{dbg,false}]}, 
     {msg22f, msg22f(), Plain, [{dbg,false}]}, 
     {msg23a, msg23a(), Plain, [{dbg,false}]}, 
     {msg23b, msg23b(), Plain, [{dbg,false}]}, 
     {msg23c, msg23c(), Plain, [{dbg,false}]}, 
     {msg23d, msg23d(), Plain, [{dbg,false}]}, 
     {msg24,  msg24(),  Plain, [{dbg,false}]}, 
     {msg25,  msg25(),  Plain, [{dbg,false}]}, 
     {msg30a, msg30a(), Plain, [{dbg,false}]}, 
     {msg30b, msg30b(), Plain, [{dbg,false}]}, 
     {msg30c, msg30c(), Plain, [{dbg,false}]}, 
     {msg30d, msg30d(), Plain, [{dbg,false}]}
    ].


msgs1b(_) ->
    TransFirst = 
	fun(Codec, DD, Ver, EC, M) ->
		megaco_codec_test_lib:trans_first_encode_decode(Codec, DD, 
								Ver, EC, M) 
	end,
    ActionsFirst = 
	fun(Codec, DD, Ver, EC, M) ->
		megaco_codec_test_lib:actions_first_encode_decode(Codec, DD, 
								  Ver, EC, M) 
	end,
    ActionFirst = 
	fun(Codec, DD, Ver, EC, M) ->
		megaco_codec_test_lib:action_first_encode_decode(Codec, DD, 
								 Ver, EC, M) 
	end,
    [
     {msg01a_tf,  msg1a(),  TransFirst,   [{dbg,false}]}, 
     {msg02_tf,   msg2(),   TransFirst,   [{dbg,false}]}, 
     {msg10_tf,   msg10(),  TransFirst,   [{dbg,false}]}, 
     {msg11_tf,   msg11(),  TransFirst,   [{dbg,false}]}, 
     {msg23d_tf,  msg23d(), TransFirst,   [{dbg,false}]}, 
     {msg30b_tf,  msg30b(), TransFirst,   [{dbg,false}]}, 
     {msg30c_tf,  msg30c(), TransFirst,   [{dbg,false}]}, 
     {msg01a_asf, msg1a(),  ActionsFirst, [{dbg,false}]}, 
     {msg02_asf,  msg2(),   ActionsFirst, [{dbg,false}]}, 
     {msg10_asf,  msg10(),  ActionsFirst, [{dbg,false}]}, 
     {msg23d_asf, msg23d(), ActionsFirst, [{dbg,false}]}, 
     {msg01a_af,  msg1a(),  ActionFirst,  [{dbg,false}]},
     {msg02_af,   msg2(),   ActionFirst,  [{dbg,false}]},
     {msg10_af,   msg10(),  ActionFirst,  [{dbg,false}]},
     {msg23d_af,  msg23d(), ActionFirst,  [{dbg,false}]}
    ].


msgs3525(_) ->
    Plain = 
	fun(Codec, DD, Ver, EC, M) ->
		megaco_codec_test_lib:plain_encode_decode(Codec, DD, Ver, 
							  EC, M) 
	end,
    [{msgs3_name(Name), rfc3525_decode(M), Plain, [{dbg, false}]} ||
	{Name, M} <- rfc3525_msgs()].

msgs3_name(N) ->
    list_to_atom("rfc3525_" ++ atom_to_list(N)).

rfc3525_decode(M) when is_list(M) ->
    rfc3525_decode(list_to_binary(M));
rfc3525_decode(M) when is_binary(M) ->
    case (catch decode_message(megaco_pretty_text_encoder, false, ?EC, M)) of
	{ok, Msg} ->
	    Msg;
	Error ->
	    {error, {rfc3525_decode_error, Error}}
    end.


msgs5(_) ->
    Plain = 
	fun(Codec, DD, Ver, EC, M) ->
		megaco_codec_test_lib:plain_encode_decode(Codec, DD, Ver, 
							  EC, M) 
	end,
    [
     {msg51a, msg51a(), Plain, [{dbg, false}]},
     {msg51b, msg51b(), Plain, [{dbg, false}]},
     {msg51c, msg51c(), Plain, [{dbg, false}]},
     {msg51d, msg51d(), Plain, [{dbg, false}]},
     {msg51e, msg51e(), Plain, [{dbg, false}]},
     {msg51f, msg51f(), Plain, [{dbg, false}]},
     {msg51g, msg51g(), Plain, [{dbg, false}]},
     {msg51h, msg51h(), Plain, [{dbg, false}]},
     {msg51i, msg51i(), Plain, [{dbg, false}]},
     {msg52,  msg52(),  Plain, [{dbg, false}]},
     {msg53,  msg53(),  Plain, [{dbg, false}]},
     {msg54a, msg54a(), Plain, [{dbg, false}]},
     {msg54b, msg54b(), Plain, [{dbg, false}]},
     {msg54c, msg54c(), Plain, [{dbg, false}]},
     {msg55,  msg55(),  Plain, [{dbg, false}]},
     {msg56,  msg56(),  Plain, [{dbg, false}]},
     {msg57,  msg57(),  Plain, [{dbg, false}]},
     {msg58a, msg58a(), Plain, [{dbg, false}]},
     {msg58b, msg58b(), Plain, [{dbg, false}]}
    ].
    
   
msgs6(Encoding) ->
    Plain = 
	fun(Codec, DD, Ver, EC, M) ->
		megaco_codec_test_lib:plain_encode_decode(Codec, DD, Ver, 
							  EC, M)
	end,    

    PlainEDFail = 
	fun(Codec, DD, Ver, EC, M) ->
		Res = 
		    megaco_codec_test_lib:plain_encode_decode(Codec, DD, Ver, 
							      EC, M),
		case Res of
		    {error, {message_encode_failed, Reason, _M}} ->
			case Reason of
			    {error, {{deprecated, _}, _}} ->
				ok;
			    _ ->
				Res
			end;
		    _ ->
			Res
		end
	end,
    
    PlainDE = 
	fun(Codec, _DD, Ver, EC, B) ->
		Res = 
		    megaco_codec_test_lib:decode_message(Codec, false, Ver, 
							 EC, B),
		case Res of
		    {ok, M} ->
			#'MegacoMessage'{mess = Mess} = M,
			#'Message'{messageBody = {transactions, TRs}} = Mess,
			[{transactionRequest, TR}] = TRs,
			#'TransactionRequest'{actions = Actions} = TR,
			[Action] = Actions,
			#'ActionRequest'{commandRequests = CmdReqs} = Action,
			[CmdReq] = CmdReqs,
			#'CommandRequest'{command = Cmd} = CmdReq,
			{addReq,AmmReq} = Cmd,
			#'AmmRequest'{descriptors = []} = AmmReq,
			ok;
		    _ ->
			Res
		end
	end,

    Msgs = 
	[
	 {msg61a, msg61a(), Plain,       [{dbg,false}],[text,binary,erlang]},
	 {msg61b, msg61b(), Plain,       [{dbg,false}],[text,binary,erlang]},
	 {msg61c, msg61c(), Plain,       [{dbg,false}],[text,binary,erlang]},
	 {msg62a, msg62a(), PlainEDFail, [{dbg,false}],[text,binary,erlang]},
	 {msg62b, msg62b(), PlainDE,     [{dbg,false}],[text]}
	],
    [{N,M,F,C}||{N,M,F,C,E} <- Msgs,lists:member(Encoding,E)].
   

msgs7(Encoding) ->
    Plain = 
	fun(Codec, DD, Ver, EC, M) ->
		megaco_codec_test_lib:plain_encode_decode(Codec, DD, Ver, 
							  EC, M)
	end,    

    Msgs = 
	[
  	 {msg71a,   msg71a(),   Plain, [{dbg,false}],[text,binary,erlang]},
  	 {msg71b01, msg71b01(), Plain, [{dbg,false}],[text,binary,erlang]},
  	 {msg71b02, msg71b02(), Plain, [{dbg,false}],[text,binary,erlang]},
  	 {msg71b03, msg71b03(), Plain, [{dbg,false}],[text,binary,erlang]},
  	 {msg71b04, msg71b04(), Plain, [{dbg,false}],[text,binary,erlang]},
  	 {msg71b05, msg71b05(), Plain, [{dbg,false}],[text,binary,erlang]},
  	 {msg71b06, msg71b06(), Plain, [{dbg,false}],[text,binary,erlang]},
  	 {msg71b07, msg71b07(), Plain, [{dbg,false}],[text,binary,erlang]},
  	 {msg71b08, msg71b08(), Plain, [{dbg,false}],[text,binary,erlang]},
  	 {msg71b09, msg71b09(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71b10, msg71b10(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71b11, msg71b11(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71b12, msg71b12(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71b13, msg71b13(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71b14, msg71b14(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71b15, msg71b15(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71b16, msg71b16(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71b17, msg71b17(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71b18, msg71b18(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71b19, msg71b19(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71b20, msg71b20(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71b21, msg71b21(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71b22, msg71b22(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71c01, msg71c01(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71c02, msg71c02(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71c03, msg71c03(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71c04, msg71c04(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71c05, msg71c05(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71c06, msg71c06(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71c07, msg71c07(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71c08, msg71c08(), Plain, [{dbg,false}],[text,binary,erlang]},
   	 {msg71c09, msg71c09(), Plain, [{dbg,false}],[text,binary,erlang]},
   	 {msg71c10, msg71c10(), Plain, [{dbg,false}],[text,binary,erlang]},
   	 {msg71c11, msg71c11(), Plain, [{dbg,false}],[text,binary,erlang]},
   	 {msg71c12, msg71c12(), Plain, [{dbg,false}],[text,binary,erlang]},
   	 {msg71c13, msg71c13(), Plain, [{dbg,false}],[text,binary,erlang]},
   	 {msg71c14, msg71c14(), Plain, [{dbg,false}],[text,binary,erlang]},
   	 {msg71c15, msg71c15(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71d01, msg71d01(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71d02, msg71d02(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71d03, msg71d03(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg71d04, msg71d04(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg72a01, msg72a01(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg72a02, msg72a02(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg72a03, msg72a03(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg72b01, msg72b01(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg72b02, msg72b02(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg72b03, msg72b03(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg72b04, msg72b04(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg72c01, msg72c01(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg72c02, msg72c02(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg72c03, msg72c03(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg72c04, msg72c04(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg73a,   msg73a(),   Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg73b01, msg73b01(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg73b02, msg73b02(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg73c01, msg73c01(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg73c02, msg73c02(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg74a01, msg74a01(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg74a02, msg74a02(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg74a03, msg74a03(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg74a04, msg74a04(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg74a05, msg74a05(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg74a06, msg74a06(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg75a01, msg75a01(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg75a02, msg75a02(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg76a01, msg76a01(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg76a02, msg76a02(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg76b01, msg76b01(), Plain, [{dbg,false}],[text,binary,erlang]},
  	 %% {msg76b02, msg76b02(), Plain, [{dbg,true}],[text,binary,erlang]},
     	 {msg77a01, msg77a01(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg78a01, msg78a01(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg78a02, msg78a02(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg78a03, msg78a03(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg78a04, msg78a04(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg78a05, msg78a05(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg78a06, msg78a06(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg78a07, msg78a07(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg78a08, msg78a08(), Plain, [{dbg,false}],[text,binary,erlang]},
     	 {msg78a09, msg78a09(), Plain, [{dbg,false}],[text,binary,erlang]},
  	 {msg79a01, msg79a01(), Plain, [{dbg,false}],[text,binary,erlang]}
	],
    [{N,M,F,C}||{N,M,F,C,E} <- Msgs,lists:member(Encoding,E)].
    
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

msg_actions([], Actions) ->
    lists:reverse(Actions);
msg_actions([{CtxId, CmdReqs}|ActionInfo], Actions) ->
    Action = ?MSG_LIB:cre_ActionRequest(CtxId,CmdReqs),
    msg_actions(ActionInfo, [Action|Actions]).

megaco_trans_req([], Transactions) ->
    {transactions, lists:reverse(Transactions)};
megaco_trans_req([{TransId, ActionInfo}|TransInfo], Transactions) ->
    Actions = msg_actions(ActionInfo, []),
    TR      = ?MSG_LIB:cre_TransactionRequest(TransId, Actions),
    Trans   = ?MSG_LIB:cre_Transaction(TR), 
    megaco_trans_req(TransInfo, [Trans|Transactions]).

megaco_message(Version, Mid, Body) ->
    Mess = ?MSG_LIB:cre_Message(Version, Mid, Body),
    cre_MegacoMessage(Mess).

msg_request(Mid, TransInfo) ->
    TransReq = megaco_trans_req(TransInfo, []),
    megaco_message(?VERSION, Mid, TransReq).

msg_request(Mid, TransId, ContextId, CmdReq) ->
    Action  = ?MSG_LIB:cre_ActionRequest(ContextId, CmdReq),
    Actions = [Action],
    TR      = ?MSG_LIB:cre_TransactionRequest(TransId, Actions),
    Trans   = ?MSG_LIB:cre_Transaction(TR), 
    Mess    = ?MSG_LIB:cre_Message(?VERSION, Mid, [Trans]),
    cre_MegacoMessage(Mess).

msg_request(Auth, Mid, TransId, ContextId, CmdReq) ->
    Action  = ?MSG_LIB:cre_ActionRequest(ContextId, CmdReq),
    Actions = [Action],
    TR      = ?MSG_LIB:cre_TransactionRequest(TransId, Actions),
    Trans   = ?MSG_LIB:cre_Transaction(TR), 
    Mess    = ?MSG_LIB:cre_Message(?VERSION, Mid, [Trans]),
    cre_MegacoMessage(Auth, Mess).

msg_reply(Mid, TransId, Actions) ->
    TR    = cre_TransRep(TransId, Actions),
    Trans = ?MSG_LIB:cre_Transaction(TR), 
    Mess  = ?MSG_LIB:cre_Message(?VERSION, Mid, [Trans]),
    cre_MegacoMessage(Mess).

msg_reply(Mid, TransId, ContextId, CmdReply) ->
    Action  = cre_ActRep(ContextId, CmdReply),
    Actions = [Action], 
    msg_reply(Mid, TransId, Actions).

msg_ack(Mid, [Range|_] = Ranges) when is_tuple(Range) ->
    msg_ack(Mid, [Ranges]);

msg_ack(Mid, Ranges) ->
    %% TRAs = make_tras(Ranges, []),
    TRAs = make_tras(Ranges),
    Req  = {transactions, TRAs},
    cre_MegacoMessage(?VERSION, Mid, Req).

make_tras(TRARanges) ->
    F = fun(R) -> {transactionResponseAck, make_tra(R)} end,
    lists:map(F, TRARanges).

make_tra(Ranges) ->
    F = fun({F,L}) -> cre_TransAck(F,L) end,
    lists:map(F, Ranges).


%% -------------------------------------------------------------------------


msg1(Mid, Tid) ->
    Gain  = cre_PropParm("tdmc/gain", "2"),
    Ec    = cre_PropParm("tdmc/ec", "g165"), 
    LCD   = cre_LocalControlDesc(sendRecv,[Gain, Ec]),
    V     = cre_PropParm("v", "0"),
    %% C    = cre_PropParm("c", "IN IP4 $ "),
    C     = cre_PropParm("c", [$I,$N,$ ,$I,$P,$4,$ ,$$,$ ]),
    M     = cre_PropParm("m", "audio $ RTP/AVP 0"),
    A     = cre_PropParm("a", "fmtp:PCMU VAD=X-NNVAD"),
    LD    = cre_LocalRemoteDesc([[V, C, M, A]]),
    Parms = cre_StreamParms(LCD,LD),
    StreamDesc = cre_StreamDesc(1,Parms),
    MediaDesc  = cre_MediaDesc(StreamDesc),
    ReqEvent   = cre_ReqEv("al/of"),
    EventsDesc = cre_EvsDesc(2222,[ReqEvent]),
    AmmReq     = cre_AmmReq([#megaco_term_id{id = Tid}],
			    [{mediaDescriptor, MediaDesc},
			     {eventsDescriptor, EventsDesc}]),
    CmdReq     = cre_CmdReq({modReq, AmmReq}),
    Msg = msg_request(Mid, 9999, ?megaco_null_context_id, [CmdReq]),
    Msg.

msg1a() ->
    msg1a(?MGC_MID).
msg1a(Mid) ->
    msg1(Mid, ?A4444).

msg1b() ->
    msg1b(?MGC_MID).
msg1b(Mid) ->
    msg1(Mid, ?A4445).


%% --------------------------


msg2() ->
    msg2(?MGC_MID).
msg2(Mid) ->
    msg2(Mid, ?A4444).
msg2(Mid, Tid) ->
    Gain  = cre_PropParm("tdmc/gain", "2"),
    Ec    = cre_PropParm("tdmc/ec", "g165"), 
    LCD   = cre_LocalControlDesc(sendRecv,[Gain, Ec]),
    V     = cre_PropParm("v", "0"),
    %% C    = cre_PropParm("c", "IN IP4 $ "),
    C     = cre_PropParm("c", [$I,$N,$ ,$I,$P,$4,$ ,$$,$ ]),
    M     = cre_PropParm("m", "audio $ RTP/AVP 0"),
    A     = cre_PropParm("a", "fmtp:PCMU VAD=X-NNVAD"),
    LD    = cre_LocalRemoteDesc([[V, C, M, A]]),
    Parms = cre_StreamParms(LCD,LD),
    StreamDesc = cre_StreamDesc(1,Parms),
    MediaDesc  = cre_MediaDesc(StreamDesc),
    EventParm  = cre_EvParm("strict",["exact"]),
    ReqEvent   = cre_ReqEv("al/of", [EventParm]),
    EventsDesc = cre_EvsDesc(2222,[ReqEvent]),
    AmmReq     = cre_AmmReq([#megaco_term_id{id = Tid}],
			    [{mediaDescriptor, MediaDesc},
			     {eventsDescriptor, EventsDesc}]),
    CmdReq     = cre_CmdReq({modReq, AmmReq}),
    msg_request(Mid, 9999, ?megaco_null_context_id, [CmdReq]).


%% --------------------------

msg3() ->
    msg3(?MG1_MID).
msg3(Mid) ->
    TimeStamp = cre_TimeNot("19990729", "22000000"),
    Event     = cre_ObsEv("al/of",TimeStamp),
    Desc      = cre_ObsEvsDesc(2222,[Event]),
    NotifyReq = cre_NotifyReq([#megaco_term_id{id = ?A4444}],Desc),
    CmdReq    = cre_CmdReq({notifyReq, NotifyReq}),
    msg_request(Mid, 10000, ?megaco_null_context_id, [CmdReq]).


%% --------------------------

msg4() ->
    msg4(?MG1_MID_NO_PORT, "901 mg col boot").
msg4(Mid, Reason) when is_list(Reason) ->
    Address = {portNumber, ?DEFAULT_PORT},
    Profile = cre_SvcChProf("resgw",1),
    Parm    = cre_SvcChParm(restart,Address,[Reason],Profile),
    Req     = cre_SvcChReq([?megaco_root_termination_id],Parm),
    CmdReq  = cre_CmdReq({serviceChangeReq, Req}),
    msg_request(Mid, 9998, ?megaco_null_context_id, [CmdReq]).


%% --------------------------

msg5() ->
    msg5(?MGC_MID).
msg5(Mid) ->
    Address = {portNumber, ?DEFAULT_PORT},
    Profile = cre_SvcChProf("resgw",1),
    Parm    = cre_SvcChResParm(Address,Profile),
    Reply   = cre_SvcChRep([?megaco_root_termination_id],
			   {serviceChangeResParms,Parm}),
    msg_reply(Mid, 9998, ?megaco_null_context_id,
	      [{serviceChangeReply, Reply}]).


%% --------------------------

msg6(Mid, Tid) ->
    Reply = cre_AmmsReply([#megaco_term_id{id = Tid}]),
    msg_reply(Mid, 9999, ?megaco_null_context_id, [{modReply, Reply}]).

msg6a() ->
    msg6a(?MG1_MID).
msg6a(Mid) ->
    msg6(Mid, ?A4444).

msg6b() ->
    msg6b(?MG2_MID).
msg6b(Mid) ->
    msg6(Mid, ?A5555).


%% --------------------------

msg7() ->
    msg7(?MGC_MID).
msg7(Mid) ->
    Reply = cre_NotifyRep([#megaco_term_id{id = ?A4444}]),
    msg_reply(Mid, 10000, ?megaco_null_context_id, [{notifyReply, Reply}]).


%% --------------------------

msg8(Mid, DigitMapValue) ->
    Strict = cre_EvParm("strict",["state"]),
    On     = cre_ReqEv("al/on", [Strict]),
    Name   = "dialplan00",
    EDM    = cre_EvDM(Name),
    Action = cre_ReqActs(EDM),
    Ce     = cre_ReqEv("dd/ce", Action),
    EventsDesc = cre_EvsDesc(2223, [On, Ce]),
    Signal     = cre_Sig("cg/rt"),
    DigMapDesc = cre_DigitMapDesc(Name, DigitMapValue),
    AmmReq     = cre_AmmReq([#megaco_term_id{id = ?A4444}],
                           [{eventsDescriptor, EventsDesc},
			    {signalsDescriptor, [{signal, Signal}]},
			    {digitMapDescriptor, DigMapDesc}]),
    CmdReq     = cre_CmdReq({modReq, AmmReq}),
    msg_request(Mid, 10001, ?megaco_null_context_id, [CmdReq]).

msg8a() ->
    msg8a(?MGC_MID).
msg8a(Mid) ->
    Body = "(0s| 00s|[1-7]xlxx|8lxxxxxxx|#xxxxxxx|*xx|9l1xxxxxxxxxx|9l011x.s)",
    Value = cre_DigitMapValue(Body),
    msg8(Mid, Value).

msg8b() ->
    msg8b(?MGC_MID).
msg8b(Mid) ->
    Body = "(0s| 00s|[1-7]xlxx|8lxxxxxxx|#xxxxxxx|*xx|9l1xxxxxxxxxx|9l011x.s)",
    Value = cre_DigitMapValue(Body, 1, 23, 99),
    msg8(Mid, Value).


%% --------------------------

msg9() ->
    msg9(?MG1_MID).
msg9(Mid) ->
    TimeStamp = cre_TimeNot("19990729","22010001"),
    Parm      = cre_EvParm("ds",["916135551212"]),
    Event     = cre_ObsEv("dd/ce",TimeStamp,[Parm]),
    Desc      = cre_ObsEvsDesc(2223,[Event]),
    NotifyReq = cre_NotifyReq([#megaco_term_id{id = ?A4444}], Desc),
    CmdReq    = cre_CmdReq({notifyReq, NotifyReq}),
    msg_request(Mid, 10002, ?megaco_null_context_id, [CmdReq]).


%% --------------------------

msg10() ->
    msg10(?MGC_MID).
msg10(Mid) ->
    AmmReq = cre_AmmReq([#megaco_term_id{id = ?A4444}],[]),
    CmdReq = cre_CmdReq({addReq, AmmReq}),
    Jit = cre_PropParm("nt/jit", "40"),
    LCD = cre_LocalControlDesc(recvOnly,[Jit]),
    V   = cre_PropParm("v", "0"),
    C   = cre_PropParm("c", "IN IP4 $ "),
    M   = cre_PropParm("m", "audio $ RTP/AVP 4"),
    A   = cre_PropParm("a", "ptime:30"),
    V2  = cre_PropParm("v", "0"),
    C2  = cre_PropParm("c", "IN IP4 $ "),
    M2  = cre_PropParm("m", "audio $ RTP/AVP 0"),
    LD  = cre_LocalRemoteDesc([[V, C, M, A], [V2, C2, M2]]),
    Parms      = cre_StreamParms(LCD, LD),
    StreamDesc = cre_StreamDesc(1,Parms),
    MediaDesc  = cre_MediaDesc(StreamDesc),
    ChooseTid  = #megaco_term_id{contains_wildcards = true,
				 id = [[?megaco_choose]]},
    AmmReq2    = cre_AmmReq([ChooseTid],[{mediaDescriptor, MediaDesc}]),
    CmdReq2    = cre_CmdReq({addReq, AmmReq2}),
    msg_request(Mid, 10003, ?megaco_choose_context_id, [CmdReq, CmdReq2]).


msg11() ->
    msg11(?MG1_MID).
msg11(Mid) ->
    V  = cre_PropParm("v", "0"),
    C  = cre_PropParm("c", "IN IP4 124.124.124.222"),
    M  = cre_PropParm("m", "audio 2222 RTP/AVP 4"),
    A  = cre_PropParm("a", "ptime:30"),
    A2 = cre_PropParm("a", "recvonly"),
    LD = cre_LocalRemoteDesc([[V, C, M, A, A2]]),
    Parms      = cre_StreamParmsL(LD),
    StreamDesc = cre_StreamDesc(1, Parms),
    MediaDesc  = cre_MediaDesc(StreamDesc),
    Reply  = cre_AmmsReply([#megaco_term_id{id = ?A4444}]),
    Reply2 = cre_AmmsReply([#megaco_term_id{id = ?A4445}],
			   [{mediaDescriptor, MediaDesc}]),
    msg_reply(Mid, 10003, 2000, [{addReply, Reply}, {addReply, Reply2}]).


%% --------------------------

msg12() ->
    msg12(?MGC_MID).
msg12(Mid) ->
    LCD        = cre_LocalControlDesc(sendRecv),
    Parms      = cre_StreamParms(LCD),
    StreamDesc = cre_StreamDesc(1,Parms),
    MediaDesc  = cre_MediaDesc(StreamDesc),
    Signal     = cre_Sig("al/ri"),
    Descs      = [{mediaDescriptor, MediaDesc},
		  {signalsDescriptor, [{signal, Signal}]}],
    AmmReq     = cre_AmmReq([#megaco_term_id{id = ?A5555}], Descs),
    CmdReq     = cre_CmdReq({addReq, AmmReq}),
    Jit        = cre_PropParm("nt/jit", "40"),
    LCD2       = cre_LocalControlDesc(sendRecv, [Jit]),
    V      = cre_PropParm("v", "0"),
    C      = cre_PropParm("c", "IN IP4 $ "),
    M      = cre_PropParm("m", "audio $ RTP/AVP 4"),
    A      = cre_PropParm("a", "ptime:30"),
    LD2    = cre_LocalRemoteDesc([[V, C, M, A]]),
    V2     = cre_PropParm("v", "0"),
    C2     = cre_PropParm("c", "IN IP4 124.124.124.222"),
    M2     = cre_PropParm("m", "audio 2222 RTP/AVP 4"),
    RD2    = cre_LocalRemoteDesc([[V2, C2, M2]]),
    Parms2 = cre_StreamParms(LCD2,LD2,RD2),
    StreamDesc2 = cre_StreamDesc(1,Parms2),
    MediaDesc2  = cre_MediaDesc(StreamDesc2),
    ChooseTid   = #megaco_term_id{contains_wildcards = true,
				  id = [[?megaco_choose]]},
    AmmReq2     = cre_AmmReq([ChooseTid],[{mediaDescriptor, MediaDesc2}]),
    CmdReq2     = cre_CmdReq({addReq, AmmReq2}),
    msg_request(Mid, 50003, ?megaco_choose_context_id, [CmdReq, CmdReq2]).


%% --------------------------

msg13() ->
    msg13(?MG2_MID).
msg13(Mid) ->
    V     = cre_PropParm("v", "0"),
    C     = cre_PropParm("c", "IN IP4 125.125.125.111"),
    M     = cre_PropParm("m", "audio 1111 RTP/AVP 4"),
    LD    = cre_LocalRemoteDesc([[V, C, M]]),
    Parms = cre_StreamParmsL(LD),
    StreamDesc = cre_StreamDesc(1,Parms),
    MediaDesc  = cre_MediaDesc(StreamDesc),
    Reply      = cre_AmmsReply([#megaco_term_id{id = ?A5556}],
			       [{mediaDescriptor, MediaDesc}]),
    msg_reply(Mid, 50003, 5000, [{addReply, Reply}]).


%% --------------------------

msg14() ->
    msg14(?MGC_MID).
msg14(Mid) ->
    Signal  = cre_Sig("cg/rt"), 
    AmmReq1 = cre_AmmReq([#megaco_term_id{id = ?A4444}],
			[{signalsDescriptor, [{signal, Signal}]}]),
    CmdReq1 = cre_CmdReq({modReq, AmmReq1}),

    Gain    = cre_PropParm("tdmc/gain", "2"),
    Ec      = cre_PropParm("tdmc/ec", "g165"),
    LCD     = cre_LocalControlDesc(sendRecv, [Gain, Ec]),
    Parms2  = cre_StreamParms(LCD),
    StreamDesc2 = cre_StreamDesc(1,Parms2),
    MediaDesc2  = cre_MediaDesc(StreamDesc2),
    AmmReq2     = cre_AmmReq([#megaco_term_id{id = ?A4445}],
			     [{mediaDescriptor, MediaDesc2}]),
    CmdReq2     = cre_CmdReq({modReq, AmmReq2}),

    V      = cre_PropParm("v", "0"),
    C      = cre_PropParm("c", "IN IP4 125.125.125.111"),
    M      = cre_PropParm("m", "audio 1111 RTP/AVP 4"),
    RD     = cre_LocalRemoteDesc([[V, C, M]]),
    Parms3 = cre_StreamParmsR(RD),
    StreamDesc3 = cre_StreamDesc(2,Parms3),
    MediaDesc3  = cre_MediaDesc(StreamDesc3),
    AmmReq3     = cre_AmmReq([#megaco_term_id{id = ?A4445}],
			     [{mediaDescriptor, MediaDesc3}]),
    CmdReq3     = cre_CmdReq({modReq, AmmReq3}),
    msg_request(Mid, 10005, 2000, [CmdReq1, CmdReq2, CmdReq3]).


%% --------------------------

msg15() ->
    msg15(?MG1_MID).
msg15(Mid) ->
    Reply  = cre_AmmsReply([#megaco_term_id{id = ?A4444}]),
    Reply2 = cre_AmmsReply([#megaco_term_id{id = ?A4445}]),
    msg_reply(Mid, 10005, 2000, [{modReply, Reply}, {modReply, Reply2}]).


%% --------------------------

msg16() ->
    msg16(?MG2_MID).
msg16(Mid) ->
    TimeStamp = cre_TimeNot("19990729","22020002"),
    Event     = cre_ObsEv("al/of",TimeStamp),
    Desc      = cre_ObsEvsDesc(1234,[Event]),
    NotifyReq = cre_NotifyReq([#megaco_term_id{id = ?A5555}],Desc),
    CmdReq    = cre_CmdReq({notifyReq, NotifyReq}),
    msg_request(Mid, 50005, 5000, [CmdReq]).


%% --------------------------

msg17() ->
    msg17(?MGC_MID).
msg17(Mid) ->
    Reply = cre_NotifyRep([#megaco_term_id{id = ?A5555}]),
    msg_reply(Mid, 50005, ?megaco_null_context_id, [{notifyReply, Reply}]).


%% --------------------------

msg18() ->
    msg18(?MGC_MID).
msg18(Mid) ->
    On         = cre_ReqEv("al/on"),
    EventsDesc = cre_EvsDesc(1235,[On]),
    AmmReq     = cre_AmmReq([#megaco_term_id{id = ?A5555}],
			    [{eventsDescriptor, EventsDesc},
			     {signalsDescriptor, []}]),
    CmdReq     = cre_CmdReq({modReq, AmmReq}),
    msg_request(Mid, 50006, 5000, [CmdReq]).


%% --------------------------

msg19() ->
    msg19(?MG2_MID).
msg19(Mid) ->
    Reply = cre_AmmsReply([#megaco_term_id{id = ?A4445}]),
    msg_reply(Mid, 50006, 5000, [{modReply, Reply}]).


%% --------------------------

msg20() ->
    msg20(?MGC_MID).
msg20(Mid) ->
    LCD        = cre_LocalControlDesc(sendRecv),
    Parms      = cre_StreamParms(LCD),
    StreamDesc = cre_StreamDesc(1,Parms),
    MediaDesc  = cre_MediaDesc(StreamDesc),
    AmmReq     = cre_AmmReq([#megaco_term_id{id = ?A4445}],
			    [{mediaDescriptor, MediaDesc}]),
    CmdReq     = cre_CmdReq({modReq, AmmReq}),
    AmmReq2    = cre_AmmReq([#megaco_term_id{id = ?A4444}],
                            [{signalsDescriptor, []}]),
    CmdReq2    = cre_CmdReq({modReq, AmmReq2}),
    msg_request(Mid, 10006, 2000, [CmdReq, CmdReq2]).


%% --------------------------

msg21() ->
    msg21(?MGC_MID).
msg21(Mid) ->
    Tokens    = [mediaToken, eventsToken, signalsToken,
		 digitMapToken, statsToken, packagesToken],
    AuditDesc = cre_AuditDesc(Tokens),
    Req       = cre_AuditReq(#megaco_term_id{id = ?A5556},AuditDesc),
    CmdReq    = cre_CmdReq({auditValueRequest, Req}),
    msg_request(Mid, 50007, ?megaco_null_context_id, [CmdReq]).


%% --------------------------

msg22a() ->
    msg22(1).

msg22b() ->
    msg22(10).

msg22c() ->
    msg22(25).

msg22d() ->
    msg22(50).

msg22e() ->
    msg22(75).

msg22f() ->
    msg22(100).

msg22(N) ->
    msg22(?MG2_MID, N).
msg22(Mid, N) ->
    Jit = cre_PropParm("nt/jit", "40"),
    LCD = cre_LocalControlDesc(sendRecv,[Jit]),
    LDV = cre_PropParm("v", "0"),
    LDC = cre_PropParm("c", "IN IP4 125.125.125.111"),
    LDM = cre_PropParm("m", "audio 1111 RTP/AVP  4"),
    LDA = cre_PropParm("a", "ptime:30"),
    LD  = cre_LocalRemoteDesc([[LDV, LDC, LDM, LDA]]),
    RDV = cre_PropParm("v", "0"),
    RDC = cre_PropParm("c", "IN IP4 124.124.124.222"),
    RDM = cre_PropParm("m", "audio 2222 RTP/AVP  4"),
    RDA = cre_PropParm("a", "ptime:30"),
    RD  = cre_LocalRemoteDesc([[RDV, RDC, RDM, RDA]]),
    StreamParms   = cre_StreamParms(LCD,LD,RD),
    StreamDesc    = cre_StreamDesc(1,StreamParms),
    Media         = cre_MediaDesc(StreamDesc),
    PackagesItem  = cre_PkgsItem("nt",1),
    PackagesItem2 = cre_PkgsItem("rtp",1),
    Stat       = cre_StatsParm("rtp/ps","1200"),
    Stat2      = cre_StatsParm("nt/os","62300"),
    Stat3      = cre_StatsParm("rtp/pr","700"),
    Stat4      = cre_StatsParm("nt/or","45100"),
    Stat5      = cre_StatsParm("rtp/pl","0.2"),
    Stat6      = cre_StatsParm("rtp/jit","20"),
    Stat7      = cre_StatsParm("rtp/delay","40"),
    Statistics = [Stat, Stat2, Stat3, Stat4, Stat5, Stat6, Stat7],
    Audits     = [{mediaDescriptor, Media},
		  {packagesDescriptor, [PackagesItem, PackagesItem2]},
		  {statisticsDescriptor, Statistics}],
    Reply      = {auditResult, 
		  cre_AuditRes(#megaco_term_id{id = ?A5556},Audits)},
    msg_reply(Mid, 50007, ?megaco_null_context_id, 
	      lists:duplicate(N,{auditValueReply, Reply})).
%%     msg_reply(Mid, 50007, ?megaco_null_context_id, 
%% 	      lists.duplicate([{auditValueReply, Reply}]).


%% --------------------------

msg23a() ->
    msg23a(?MG2_MID).
msg23a(Mid) ->
    TimeStamp = cre_TimeNot("19990729","24020002"),
    Event     = cre_ObsEv("al/on",TimeStamp),
    Desc      = cre_ObsEvsDesc(1235,[Event]),
    NotifyReq = cre_NotifyReq([#megaco_term_id{id = ?A5555}],Desc),
    CmdReq    = cre_CmdReq({notifyReq, NotifyReq}),
    msg_request(Mid, 50008, 5000, [CmdReq]).


msg23b() ->
    msg23b(?MG2_MID).
msg23b(Mid) ->
    TimeStamp  = cre_TimeNot("19990729","24020002"),
    Event      = cre_ObsEv("al/on",TimeStamp),
    Desc       = cre_ObsEvsDesc(1235,[Event]),
    NotifyReq1 = cre_NotifyReq([#megaco_term_id{id = ?A5555}],Desc),
    CmdReq1    = cre_CmdReq({notifyReq, NotifyReq1}),
    NotifyReq2 = cre_NotifyReq([#megaco_term_id{id = ?A5556}],Desc),
    CmdReq2    = cre_CmdReq({notifyReq, NotifyReq2}),
    ActionInfo = [{5000, [CmdReq1]}, {5001, [CmdReq2]}],
    TransInfo  = [{50008, ActionInfo}],
    msg_request(Mid, TransInfo).


msg23c() ->
    msg23c(?MG2_MID).
msg23c(Mid) ->
    TimeStamp  = cre_TimeNot("19990729","24020002"),
    Event      = cre_ObsEv("al/on",TimeStamp),
    Desc       = cre_ObsEvsDesc(1235,[Event]),
    NotifyReq1 = cre_NotifyReq([#megaco_term_id{id = ?A5555}],Desc),
    CmdReq1    = cre_CmdReq({notifyReq, NotifyReq1}),
    NotifyReq2 = cre_NotifyReq([#megaco_term_id{id = ?A5556}],Desc),
    CmdReq2    = cre_CmdReq({notifyReq, NotifyReq2}),
    ActionInfo1 = [{5000, [CmdReq1]}],
    ActionInfo2 = [{5001, [CmdReq2]}],
    TransInfo   = [{50008, ActionInfo1}, {50009, ActionInfo2}],
    msg_request(Mid, TransInfo).


msg23d() ->
    msg23d(?MG2_MID).
msg23d(Mid) ->
    TimeStamp  = cre_TimeNot("19990729","24020002"),
    Event      = cre_ObsEv("al/on",TimeStamp),
    Desc       = cre_ObsEvsDesc(1235,[Event]),
    NotifyReq1 = cre_NotifyReq([#megaco_term_id{id = ?A5555}],Desc),
    CmdReq1    = cre_CmdReq({notifyReq, NotifyReq1}),
    NotifyReq2 = cre_NotifyReq([#megaco_term_id{id = ?A5556}],Desc),
    CmdReq2    = cre_CmdReq({notifyReq, NotifyReq2}),
    NotifyReq3 = cre_NotifyReq([#megaco_term_id{id = ?A4444}],Desc),
    CmdReq3    = cre_CmdReq({notifyReq, NotifyReq3}),
    NotifyReq4 = cre_NotifyReq([#megaco_term_id{id = ?A4445}],Desc),
    CmdReq4    = cre_CmdReq({notifyReq, NotifyReq4}),
    ActionInfo1 = [{5000, [CmdReq1]}, {5001, [CmdReq2]}],
    ActionInfo2 = [{5003, [CmdReq3]}, {5004, [CmdReq4]}],
    TransInfo   = [{50008, ActionInfo1}, {50009, ActionInfo2}],
    msg_request(Mid, TransInfo).


%% --------------------------

msg24() ->
    msg24(?MGC_MID).
msg24(Mid) ->
    AuditDesc = cre_AuditDesc([statsToken]),
    SubReq    = cre_SubReq([#megaco_term_id{id = ?A5555}], AuditDesc),
    SubReq2   = cre_SubReq([#megaco_term_id{id = ?A5556}], AuditDesc),
    CmdReq    = cre_CmdReq({subtractReq, SubReq}),
    CmdReq2   = cre_CmdReq({subtractReq, SubReq2}),
    msg_request(Mid, 50009, 5000, [CmdReq, CmdReq2]).


%% --------------------------

msg25() ->
    msg25(?MG2_MID).
msg25(Mid) ->
    Stat11 = cre_StatsParm("nt/os","45123"),
    Stat12 = cre_StatsParm("nt/dur", "40"),
    Stats1 = [Stat11, Stat12],
    Reply1 = cre_AmmsReply([#megaco_term_id{id = ?A5555}],
			   [{statisticsDescriptor, Stats1}]),
    Stat21 = cre_StatsParm("rtp/ps","1245"),
    Stat22 = cre_StatsParm("nt/os", "62345"),
    Stat23 = cre_StatsParm("rtp/pr", "780"),
    Stat24 = cre_StatsParm("nt/or", "45123"),
    Stat25 = cre_StatsParm("rtp/pl", "10"),
    Stat26 = cre_StatsParm("rtp/jit", "27"),
    Stat27 = cre_StatsParm("rtp/delay","48"),
    Stats2 = [Stat21, Stat22, Stat23, Stat24, Stat25, Stat26, Stat27],
    Reply2 = cre_AmmsReply([#megaco_term_id{id = ?A5556}],
                          [{statisticsDescriptor, Stats2}]),
    msg_reply(Mid, 50009, 5000, 
	      [{subtractReply, Reply1}, {subtractReply, Reply2}]).


msg30a() ->
    msg_ack(?MG2_MID, [{9,9}]).

msg30b() ->
    msg_ack(?MG2_MID, [{9,13}]).

msg30c() ->
    msg_ack(?MG2_MID, 
	    [{9,13},   {15,15},  {33,40},  {50,60},  {70,80},  {85,90},
	     {101,105},{109,119},{121,130},{140,160},{170,175},{180,189},
	     {201,205},{209,219},{221,230},{240,260},{270,275},{280,289},
	     {301,305},{309,319},{321,330},{340,360},{370,375},{380,389},
	     {401,405},{409,419},{421,430},{440,460},{470,475},{480,489},
	     {501,505},{509,519},{521,530},{540,560},{570,575},{580,589}
	    ]).

%% Don't think this will be used by the megaco stack, but since it
%% seem's to be a valid construction...
msg30d() ->
    msg_ack(?MG2_MID, 
	    [[{9,13},   {15,15},  {33,40},  {50,60},  {70,80},  {85,90}],
	     [{101,105},{109,119},{121,130},{140,160},{170,175},{180,189}],
	     [{201,205},{209,219},{221,230},{240,260},{270,275},{280,289}],
	     [{301,305},{309,319},{321,330},{340,360},{370,375},{380,389}],
	     [{401,405},{409,419},{421,430},{440,460},{470,475},{480,489}],
	     [{501,505},{509,519},{521,530},{540,560},{570,575},{580,589}]
	    ]).


    
msg40() ->
    msg40(?MG1_MID_NO_PORT, "901 mg col boot").
msg40(Mid, Reason) when is_list(Reason) ->
    Address = {portNumber, ?DEFAULT_PORT},
    Profile = cre_SvcChProf("resgw",1),
    Parm    = cre_SvcChParm(restart,Address,[Reason],Profile),
    Req     = cre_SvcChReq([?megaco_root_termination_id],Parm),
    CmdReq  = cre_CmdReq({serviceChangeReq, Req}),
    Auth    = cre_AuthHeader(),
    msg_request(Auth, Mid, 9998, ?megaco_null_context_id, [CmdReq]).


msg50(Mid, APT) ->
    AD     = cre_AuditDesc(asn1_NOVALUE, APT),
    Req    = cre_AuditReq(#megaco_term_id{id = ?A5556},AD),
    CmdReq = cre_CmdReq({auditValueRequest, Req}),
    msg_request(Mid, 50007, ?megaco_null_context_id, [CmdReq]).
    
%% IndAudMediaDescriptor:
msg51(Mid, IATSDorStream) ->
    IAMD   = cre_IndAudMediaDesc(IATSDorStream),
    IAP    = cre_IndAudParam(IAMD),
    APT    = [IAP],
    msg50(Mid, APT).
    
msg51a() ->
    msg51a(?MG2_MID).
msg51a(Mid) ->
    PP    = cre_IndAudPropertyParm("tdmc/gain"),
    PPs   = [PP],
    IATSD = cre_IndAudTermStateDesc(PPs),
    msg51(Mid, IATSD).

msg51b() ->
    msg51b(?MG2_MID).
msg51b(Mid) ->
    PP    = cre_IndAudPropertyParm("nt/jit"),
    PPs   = [PP],
    IATSD = cre_IndAudTermStateDesc(PPs),
    msg51(Mid, IATSD).

msg51c() ->
    msg51c(?MG2_MID).
msg51c(Mid) ->
    IATSD = cre_IndAudTermStateDesc([], asn1_NOVALUE, 'NULL'),
    msg51(Mid, IATSD).

msg51d() ->
    msg51d(?MG2_MID).
msg51d(Mid) ->
    IATSD = cre_IndAudTermStateDesc([], 'NULL', asn1_NOVALUE),
    msg51(Mid, IATSD).

msg51e() ->
    msg51e(?MG2_MID).
msg51e(Mid) ->
    IALCD = cre_IndAudLocalControlDesc('NULL', asn1_NOVALUE, 
				       asn1_NOVALUE, asn1_NOVALUE),
    IASP = cre_IndAudStreamParms(IALCD),
    msg51(Mid, IASP).

msg51f() ->
    msg51f(?MG2_MID).
msg51f(Mid) ->
    IALCD = cre_IndAudLocalControlDesc(asn1_NOVALUE, 'NULL', 
				       asn1_NOVALUE, asn1_NOVALUE),
    IASP = cre_IndAudStreamParms(IALCD),
    msg51(Mid, IASP).

msg51g() ->
    msg51g(?MG2_MID).
msg51g(Mid) ->
    IALCD = cre_IndAudLocalControlDesc(asn1_NOVALUE, asn1_NOVALUE, 
				       'NULL', asn1_NOVALUE),
    IASP  = cre_IndAudStreamParms(IALCD),
    msg51(Mid, IASP).

msg51h() ->
    msg51h(?MG2_MID).
msg51h(Mid) ->
    Name  = "nt/jit",
    IAPP  = cre_IndAudPropertyParm(Name),
    IALCD = cre_IndAudLocalControlDesc(asn1_NOVALUE, asn1_NOVALUE, 
				       asn1_NOVALUE, [IAPP]),
    IASP  = cre_IndAudStreamParms(IALCD),
    SID   = 123,
    IASD  = cre_IndAudStreamDesc(SID, IASP),
    msg51(Mid, [IASD]).


msg51i() ->
    msg51i(?MG2_MID).
msg51i(Mid) ->
    Name  = "nt/jit",
    Name2 = "tdmc/ec",
    IAPP  = cre_IndAudPropertyParm(Name),
    IAPP2 = cre_IndAudPropertyParm(Name2),
    IALCD = cre_IndAudLocalControlDesc('NULL', 'NULL', 'NULL', 
				       [IAPP, IAPP2]),
    IASP  = cre_IndAudStreamParms(IALCD),
    SID   = 123,
    IASD  = cre_IndAudStreamDesc(SID, IASP),
    msg51(Mid, [IASD]).


%% IndAudEventsDescriptor:
msg52() ->
    msg52(?MG2_MID).
msg52(Mid) ->
    RequestID = 1235,
    PkgdName  = "tonedet/std",
    IAED = cre_IndAudEvsDesc(RequestID, PkgdName),
    IAP  = cre_IndAudParam(IAED),
    APT  = [IAP],
    msg50(Mid, APT).

%% IndAudEventBufferDescriptor:
msg53() ->
    msg53(?MG2_MID).
msg53(Mid) ->
    EN    = "tonedet/std",
    SID   = 1,
    IAEBD = cre_IndAudEvBufDesc(EN, SID),
    IAP   = cre_IndAudParam(IAEBD),
    APT   = [IAP],
    msg50(Mid, APT).

%% IndAudSignalsDescriptor:
msg54(Mid, Sig) ->
    IASD = cre_IndAudSigsDesc(Sig),
    IAP  = cre_IndAudParam(IASD),
    APT  = [IAP],
    msg50(Mid, APT).
    
msg54a() ->
    msg54a(?MG2_MID).
msg54a(Mid) ->
    SN  = "tonegen/pt",
    Sig = cre_IndAudSig(SN),
    msg54(Mid, Sig).

msg54b() ->
    msg54b(?MG2_MID).
msg54b(Mid) ->
    SN  = "dg/d0",
    Sig = cre_IndAudSig(SN),
    msg54(Mid, Sig).

msg54c() ->
    msg54c(?MG2_MID).
msg54c(Mid) ->
    SN  = "ct/ct",
    Sig = cre_IndAudSig(SN),
    ID  = 4321,
    SSL = cre_IndAudSeqSigList(ID, Sig),
    msg54(Mid, SSL).

%% IndAudDigitMapDescriptor:
msg55() ->
    msg55(?MG2_MID).
msg55(Mid) ->
    DMN   = "dialplan00",
    IADMD = cre_IndAudDigitMapDesc(DMN),
    IAP   = cre_IndAudParam(IADMD),
    APT   = [IAP],
    msg50(Mid, APT).

%% IndAudStatisticsDescriptor:
msg56() ->
    msg56(?MG2_MID).
msg56(Mid) ->
    SN   = "nt/dur",
    IASD = cre_IndAudStatsDesc(SN),
    IAP  = cre_IndAudParam(IASD),
    APT  = [IAP],
    msg50(Mid, APT).    

%% IndAudPackagesDescriptor:
msg57() ->
    msg57(?MG2_MID).
msg57(Mid) ->
    PN   = "al",
    PV   = 1,
    IAPD = cre_IndAudPkgsDesc(PN, PV),
    IAP  = cre_IndAudParam(IAPD),
    APT  = [IAP],
    msg50(Mid, APT).    
    
%% Sum it up:
msg58_iaMediaDesc_iap(IATSD) ->
    IAMD  = cre_IndAudMediaDesc(IATSD),
    cre_IndAudParam(IAMD).
    
msg58_iaMediaDesc_iap_a() ->
    PP    = cre_IndAudPropertyParm("tdmc/gain"),
    PPs   = [PP],
    IATSD = cre_IndAudTermStateDesc(PPs),
    msg58_iaMediaDesc_iap(IATSD).
    
msg58_iaMediaDesc_iap_b() ->
    IATSD = cre_IndAudTermStateDesc([], 'NULL', asn1_NOVALUE),
    msg58_iaMediaDesc_iap(IATSD).
    
msg58_iaEvsDesc_iap() ->
    RequestID = 1235,
    PkgdName  = "tonedet/std",
    IAED = cre_IndAudEvsDesc(RequestID, PkgdName),
    cre_IndAudParam(IAED).

msg58_iaEvBufDesc_iap() ->    
    EN    = "tonedet/std",
    SID   = 1,
    IAEBD = cre_IndAudEvBufDesc(EN, SID),
    cre_IndAudParam(IAEBD).

msg58_iaSigsDesc_iap(S) ->
    IASD = cre_IndAudSigsDesc(S),
    cre_IndAudParam(IASD).

msg58_iaSigsDesc_iap_a() ->
    SN  = "tonegen/pt",
    Sig = cre_IndAudSig(SN),
    msg58_iaSigsDesc_iap(Sig).
   
msg58_iaSigsDesc_iap_b() ->
    SN  = "ct/ct",
    Sig = cre_IndAudSig(SN),
    ID  = 4321,
    SSL = cre_IndAudSeqSigList(ID, Sig),
    msg58_iaSigsDesc_iap(SSL).

msg58_iaDigMapDesc_iap() ->
    DMN   = "dialplan00",
    IADMD = cre_IndAudDigitMapDesc(DMN),
    cre_IndAudParam(IADMD).
    
msg58_iaStatsDesc_iap() ->
    SN   = "nt/dur",
    IASD = cre_IndAudStatsDesc(SN),
    cre_IndAudParam(IASD).

msg58_iaPacksDesc_iap() ->
    PN   = "al",
    PV   = 1,
    IAPD = cre_IndAudPkgsDesc(PN, PV),
    cre_IndAudParam(IAPD).

msg58a() ->
    msg58a(?MG2_MID).
msg58a(Mid) ->
    IAMD  = msg58_iaMediaDesc_iap_a(),
    IAED  = msg58_iaEvsDesc_iap(),
    IAEBD = msg58_iaEvBufDesc_iap(),
    IASiD = msg58_iaSigsDesc_iap_a(),
    IADMD = msg58_iaDigMapDesc_iap(),
    IAStD = msg58_iaStatsDesc_iap(),
    IAPD  = msg58_iaPacksDesc_iap(),
    APT   = [IAMD, IAED, IAEBD, IASiD, IADMD, IAStD, IAPD],
    msg50(Mid, APT).

msg58b() ->
    msg58b(?MG2_MID).
msg58b(Mid) ->
    IAMD  = msg58_iaMediaDesc_iap_b(),
    IAED  = msg58_iaEvsDesc_iap(),
    IAEBD = msg58_iaEvBufDesc_iap(),
    IASiD = msg58_iaSigsDesc_iap_b(),
    IADMD = msg58_iaDigMapDesc_iap(),
    IAStD = msg58_iaStatsDesc_iap(),
    IAPD  = msg58_iaPacksDesc_iap(),
    APT   = [IAMD, IAED, IAEBD, IASiD, IADMD, IAStD, IAPD],
    msg50(Mid, APT).


%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
%% Tests some of the changes in the v2 corr 1 (EmergencyOff and ModemDesc)

%% Emergency On/Off (optional) tests
msg61(EM) ->
    TS     = cre_TimeNot("19990729", "22000000"),
    Event  = cre_ObsEv("al/of",TS),
    Desc   = cre_ObsEvsDesc(2222,[Event]),
    NotReq = cre_NotifyReq([#megaco_term_id{id = ?A4444}],Desc),
    Cmd    = ?MSG_LIB:cre_Command(notifyReq, NotReq),
    CmdReq = cre_CmdReq(Cmd),
    CtxReq = cre_CtxReq(15, EM),
    ActReq = ?MSG_LIB:cre_ActionRequest(1, CtxReq, [CmdReq]),
    Acts   = [ActReq],
    TR     = ?MSG_LIB:cre_TransactionRequest(9898, Acts),
    Trans  = ?MSG_LIB:cre_Transaction(TR), 
    Mess   = ?MSG_LIB:cre_Message(?VERSION, ?MG1_MID, [Trans]),
    cre_MegacoMessage(Mess).

msg61a() ->
    msg61(false).

msg61b() ->
    msg61(true).

msg61c() ->
    msg61(asn1_NOVALUE).


msg62a() ->
    MT      = ?MSG_LIB:cre_ModemType(v18),
    PP      = cre_PropParm("c", "IN IP4 $ "),
    MD      = ?MSG_LIB:cre_ModemDescriptor([MT], [PP]),
    AmmDesc = ?MSG_LIB:cre_AmmDescriptor(MD),
    TermIDs = [#megaco_term_id{id = ?A4444}],
    AmmReq  = ?MSG_LIB:cre_AmmRequest(TermIDs, [AmmDesc]),
    Cmd     = ?MSG_LIB:cre_Command(addReq, AmmReq),
    CmdReq  = ?MSG_LIB:cre_CommandRequest(Cmd),
    ActReq  = ?MSG_LIB:cre_ActionRequest(2, [CmdReq]),
    Acts    = [ActReq],
    TR      = ?MSG_LIB:cre_TransactionRequest(9898, Acts),
    Trans   = ?MSG_LIB:cre_Transaction(TR), 
    Mess    = ?MSG_LIB:cre_Message(?VERSION, ?MG1_MID, [Trans]),
    cre_MegacoMessage(Mess).

msg62b() ->
    MP = 
"MEGACO/" ?VERSION_STR " [124.124.124.222]:55555
Transaction = 9898 {
        Context = 2 {
                Add = 11111111/00000000/00000000 {
                        Modem[V18] {
                                tdmc/gain=2
                        }
                }
        }
}",
%     MC = 
% "!/" ?VERSION_STR " [124.124.124.222]:55555\nT=9898{C=2{A=11111111/00000000/00000000{MD[V18]{tdmc/gain=2}}}}",
    list_to_binary(MP).

%% ActionRequest with various combinations of ContextRequest and
%% ContextAttrAuditRequest
msg71(CR, CAAR) ->
    TS1    = cre_TimeNot("19990729", "22000000"),
    TS2    = cre_TimeNot("19990729", "22000111"),
    Event1 = cre_ObsEv("al/of",TS1),
    Event2 = cre_ObsEv("al/on",TS2),
    Desc1  = cre_ObsEvsDesc(2222,[Event1]),
    Desc2  = cre_ObsEvsDesc(2222,[Event2]),
    NR1    = cre_NotifyReq([#megaco_term_id{id = ?A4444}],Desc1),
    NR2    = cre_NotifyReq([#megaco_term_id{id = ?A4444}],Desc2),
    Cmd1   = ?MSG_LIB:cre_Command(notifyReq, NR1),
    Cmd2   = ?MSG_LIB:cre_Command(notifyReq, NR2),
    CR1    = cre_CmdReq(Cmd1),
    CR2    = cre_CmdReq(Cmd2),
    ActReq = ?MSG_LIB:cre_ActionRequest(1, CR, CAAR, [CR1, CR2]),
    Acts   = [ActReq],
    TR     = ?MSG_LIB:cre_TransactionRequest(9898, Acts),
    Trans  = ?MSG_LIB:cre_Transaction(TR), 
    Mess   = ?MSG_LIB:cre_Message(?VERSION, ?MG1_MID, [Trans]),
    cre_MegacoMessage(Mess).
    
msg71a() ->
    CR   = cre_CtxReq(),
    CAAR = cre_CtxAttrAuditReq(),
    msg71(CR, CAAR).

msg71b(CR) ->    
    CAAR = asn1_NOVALUE,
    msg71(CR, CAAR).

msg71b01() ->
    CR = cre_CtxReq(15),
    msg71b(CR).
    
msg71b02() ->
    CR = cre_CtxReq(true),
    msg71b(CR).
    
msg71b03() ->
    CR = cre_CtxReq(false),
    msg71b(CR).
    
msg71b04() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = bothway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = isolate,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    CR    = cre_CtxReq(Top),
    msg71b(CR).
    
msg71b05() ->
    CR = cre_CtxReq(15, true),
    msg71b(CR).
    
msg71b06() ->
    CR = cre_CtxReq(15, false),
    msg71b(CR).
    
msg71b07() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = bothway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = oneway,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    CR    = cre_CtxReq(15, Top),
    msg71b(CR).
    
msg71b08() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = bothway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = bothway,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    CR    = cre_CtxReq(15, true, Top),
    msg71b(CR).
    
msg71b09() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = isolate,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = oneway,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    CR    = cre_CtxReq(15, false, Top),
    msg71b(CR).
    
msg71b10() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = isolate,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = onewayboth,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    CR    = cre_CtxReq(15, false, Top),
    msg71b(CR).
    
msg71b11() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = isolate,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = onewayexternal,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    CR    = cre_CtxReq(15, false, Top),
    msg71b(CR).
    
msg71b12() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = oneway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = bothway,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    CR    = cre_CtxReq(15, true, Top, true),
    msg71b(CR).
    
msg71b13() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = bothway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = bothway,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    CR    = cre_CtxReq(15, true, Top, false),
    msg71b(CR).
    
msg71b14() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = oneway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = oneway,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    PP    = cre_PropParm("tdmc/gain", "2"),
    Props = [PP],
    CR    = cre_CtxReq(15, true, Top, Props),
    msg71b(CR).
    
msg71b15() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = isolate,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = isolate,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    PP    = cre_PropParm("tdmc/gain", ["2"], relation, greaterThan),
    Props = [PP],
    CR    = cre_CtxReq(15, true, Top, Props),
    msg71b(CR).
    
msg71b16() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = isolate,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = bothway,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    PP    = cre_PropParm("tdmc/gain", ["2","10"], range, true), 
    Props = [PP],
    CR    = cre_CtxReq(15, true, Top, Props),
    msg71b(CR).
    
msg71b17() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = oneway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = bothway,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    PP    = cre_PropParm("nt/jit", ["40","50","50"], sublist, true),
    Props = [PP],
    CR    = cre_CtxReq(15, true, Top, Props),
    msg71b(CR).
    
msg71b18() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = oneway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = isolate,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    PP    = cre_PropParm("tdmc/gain", ["2","4","8"], sublist, false), 
    Props = [PP],
    CR    = cre_CtxReq(15, true, Top, true, Props),
    msg71b(CR).
    
msg71b19() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = oneway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = bothway,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    CR    = cre_CtxReq(15, true, Top, false),
    msg71b(CR).
    
msg71b20() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = oneway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = isolate,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    PP    = cre_PropParm("tdmc/gain", ["2","4","8"], sublist, false), 
    Props = [PP],
    CR    = cre_CtxReq(15, true, Top, false, Props),
    msg71b(CR).
    
msg71b21() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = oneway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = isolate,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    CID1  = cre_CtxID(10191),
    CID2  = cre_CtxID(10192),
    CIDs  = [CID1, CID2],
    CR    = cre_CtxReq(15, true, Top, false, CIDs),
    msg71b(CR).
    
msg71b22() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = oneway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = isolate,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    PP    = cre_PropParm("tdmc/gain", ["2","4","8"], sublist, false), 
    Props = [PP],
    CID1  = cre_CtxID(10191),
    CID2  = cre_CtxID(10192),
    CIDs  = [CID1, CID2],
    CR    = cre_CtxReq(15, true, Top, false, Props, CIDs),
    msg71b(CR).
    
msg71c(CAAR) ->
    CR = asn1_NOVALUE,
    msg71(CR, CAAR).
    
msg71c01() ->
    CAAR = cre_CtxAttrAuditReq('NULL', 'NULL', 'NULL'),
    msg71c(CAAR).

msg71c02() ->
    CAAR = cre_CtxAttrAuditReq('NULL', 'NULL', asn1_NOVALUE),
    msg71c(CAAR).

msg71c03() ->
    CAAR = cre_CtxAttrAuditReq('NULL', asn1_NOVALUE, 'NULL'),
    msg71c(CAAR).

msg71c04() ->
    CAAR = cre_CtxAttrAuditReq(asn1_NOVALUE, 'NULL', 'NULL'),
    msg71c(CAAR).

msg71c05() ->
    CAAR = cre_CtxAttrAuditReq(asn1_NOVALUE, asn1_NOVALUE, 'NULL'),
    msg71c(CAAR).

msg71c06() ->
    CAAR = cre_CtxAttrAuditReq(asn1_NOVALUE, 'NULL', asn1_NOVALUE),
    msg71c(CAAR).

msg71c07() ->
    CAAR = cre_CtxAttrAuditReq('NULL', asn1_NOVALUE, asn1_NOVALUE),
    msg71c(CAAR).

msg71c08() ->
    CAAR = cre_CtxAttrAuditReq('NULL', asn1_NOVALUE, 'NULL', 'NULL'),
    msg71c(CAAR).

msg71c09() ->
    Top   = 'NULL',
    Em    = 'NULL',
    Prio  = 'NULL',
    Ieps  = 'NULL',
    IAPP1 = cre_IndAudPropertyParm("tdmc/gain"),
    IAPP2 = cre_IndAudPropertyParm("nt/jit"),
    CPA   = [IAPP1, IAPP2],
    CAAR  = cre_CtxAttrAuditReq(Top, Em, Prio, Ieps, CPA),
    msg71c(CAAR).

msg71c10() ->
    Top   = 'NULL',
    Em    = 'NULL',
    Prio  = 'NULL',
    Ieps  = 'NULL',
    IAPP1 = cre_IndAudPropertyParm("tdmc/gain"),
    IAPP2 = cre_IndAudPropertyParm("nt/jit"),
    CPA   = [IAPP1, IAPP2],
    SPrio = 10,
    CAAR  = cre_CtxAttrAuditReq(Top, Em, Prio, Ieps, CPA, SPrio),
    msg71c(CAAR).

msg71c11() ->
    Top   = 'NULL',
    Em    = 'NULL',
    Prio  = 'NULL',
    Ieps  = 'NULL',
    IAPP1 = cre_IndAudPropertyParm("tdmc/gain"),
    IAPP2 = cre_IndAudPropertyParm("nt/jit"),
    CPA   = [IAPP1, IAPP2],
    SPrio = 10,
    SEm   = true,
    CAAR  = cre_CtxAttrAuditReq(Top, Em, Prio, Ieps, CPA, 
				SPrio, SEm, asn1_NOVALUE, asn1_NOVALUE),
    msg71c(CAAR).

msg71c12() ->
    Top   = 'NULL',
    Em    = 'NULL',
    Prio  = 'NULL',
    Ieps  = 'NULL',
    IAPP1 = cre_IndAudPropertyParm("tdmc/gain"),
    IAPP2 = cre_IndAudPropertyParm("nt/jit"),
    CPA   = [IAPP1, IAPP2],
    SPrio = 10,
    SEm   = true,
    SIeps = false,
    CAAR  = cre_CtxAttrAuditReq(Top, Em, Prio, Ieps, CPA, 
					SPrio, SEm, SIeps),
    msg71c(CAAR).

msg71c13() ->
    Top   = 'NULL',
    Em    = 'NULL',
    Prio  = 'NULL',
    Ieps  = 'NULL',
    IAPP1 = cre_IndAudPropertyParm("tdmc/gain"),
    IAPP2 = cre_IndAudPropertyParm("nt/jit"),
    CPA   = [IAPP1, IAPP2],
    SPrio = 10,
    SEm   = false,
    SIeps = true,
    SLog  = cre_SelectLogic(andAUDITSelect),
    CAAR  = cre_CtxAttrAuditReq(Top, Em, Prio, Ieps, CPA, 
				SPrio, SEm, SIeps, SLog),
    msg71c(CAAR).

msg71c14() ->
    Top   = 'NULL',
    Em    = 'NULL',
    Prio  = 'NULL',
    Ieps  = 'NULL',
    IAPP1 = cre_IndAudPropertyParm("tdmc/gain"),
    IAPP2 = cre_IndAudPropertyParm("nt/jit"),
    CPA   = [IAPP1, IAPP2],
    SPrio = 10,
    SEm   = true,
    SIeps = true,
    SLog  = cre_SelectLogic(orAUDITSelect),
    CAAR  = cre_CtxAttrAuditReq(Top, Em, Prio, Ieps, CPA, 
					SPrio, SEm, SIeps, SLog),
    msg71c(CAAR).

msg71c15() ->
    Top   = 'NULL',
    Em    = 'NULL',
    Prio  = 'NULL',
    Ieps  = 'NULL',
    IAPP1 = cre_IndAudPropertyParm("tdmc/gain"),
    IAPP2 = cre_IndAudPropertyParm("nt/jit"),
    CPA   = [IAPP1, IAPP2],
    SPrio = 10,
    SLog  = cre_SelectLogic(orAUDITSelect),
    CAAR  = cre_CtxAttrAuditReq(Top, Em, Prio, Ieps, CPA, 
				SPrio, SLog),
    msg71c(CAAR).

msg71d01() ->
    CR   = cre_CtxReq(15, true),
    CAAR = cre_CtxAttrAuditReq('NULL', 'NULL', 'NULL'),
    msg71(CR, CAAR).

msg71d02() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = bothway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = oneway,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    PP    = cre_PropParm("tdmc/gain", ["2"], relation, unequalTo),
    Props = [PP],
    CR    = cre_CtxReq(15, true, Top, true, Props),

    CAAR_Top = 'NULL',
    Em   = 'NULL',
    Prio = 'NULL',
    Ieps = 'NULL',
    IAPP1 = cre_IndAudPropertyParm("tdmc/gain"),
    IAPP2 = cre_IndAudPropertyParm("nt/jit"),
    CPA   = [IAPP1, IAPP2],
    CAAR = cre_CtxAttrAuditReq(CAAR_Top, Em, Prio, Ieps, CPA),
   
    msg71(CR, CAAR).

msg71d03() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = bothway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = oneway,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    PP    = cre_PropParm("tdmc/gain", ["2"], relation, unequalTo),
    Props = [PP],
    CR    = cre_CtxReq(15, true, Top, false, Props),

    CAAR_Top = 'NULL',
    Em   = 'NULL',
    Prio = 'NULL',
    Ieps = 'NULL',
    IAPP1 = cre_IndAudPropertyParm("tdmc/gain"),
    IAPP2 = cre_IndAudPropertyParm("nt/jit"),
    CPA   = [IAPP1, IAPP2],
    CAAR = cre_CtxAttrAuditReq(CAAR_Top, Em, Prio, Ieps, CPA),
   
    msg71(CR, CAAR).

msg71d04() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = bothway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = oneway,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    PP    = cre_PropParm("tdmc/gain", ["2"], relation, unequalTo),
    Props = [PP],
    CID1  = cre_CtxID(10191),
    CID2  = cre_CtxID(10192),
    CIDs  = [CID1, CID2],
    CR    = cre_CtxReq(15, true, Top, false, Props, CIDs),

    CAAR_Top = 'NULL', 
    Em   = 'NULL',
    Prio = 'NULL',
    Ieps = 'NULL',
    IAPP1 = cre_IndAudPropertyParm("tdmc/gain"),
    IAPP2 = cre_IndAudPropertyParm("nt/jit"),
    CPA   = [IAPP1, IAPP2],
    SPrio = 10,
    SEm   = true,
    SIeps = true,
    SLog  = cre_SelectLogic(orAUDITSelect),
    CAAR  = cre_CtxAttrAuditReq(CAAR_Top, Em, Prio, Ieps, CPA, 
				SPrio, SEm, SIeps, SLog),
    msg71(CR, CAAR).

msg72(ED, CR) ->
    V  = cre_PropParm("v", "0"),
    C  = cre_PropParm("c", "IN IP4 124.124.124.222"),
    M  = cre_PropParm("m", "audio 2222 RTP/AVP 4"),
    A  = cre_PropParm("a", "a=ptime:30"),
    A2 = cre_PropParm("a", "recvonly"),
    LD = cre_LocalRemoteDesc([[V, C, M, A, A2]]),
    Parms      = cre_StreamParmsL(LD),
    StreamDesc = cre_StreamDesc(1, Parms),
    MediaDesc  = cre_MediaDesc(StreamDesc),
    Reply  = cre_AmmsReply([#megaco_term_id{id = ?A4444}]),
    Reply2 = cre_AmmsReply([#megaco_term_id{id = ?A4445}],
			   [{mediaDescriptor, MediaDesc}]),
    CmdRep = [{addReply, Reply}, {addReply, Reply2}],
    Action = cre_ActRep(2000, ED, CR, CmdRep),
    msg_reply(?MGC_MID, 10003, [Action]).

msg72a(CR) ->
    ED = asn1_NOVALUE,
    msg72(ED, CR).

msg72a01() ->
    CR = cre_CtxReq(false),
    msg72a(CR).

msg72a02() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = bothway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = isolate,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    CR    = cre_CtxReq(Top),
    msg72a(CR).

msg72a03() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = bothway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = oneway,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    CR    = cre_CtxReq(15, Top),
    msg72a(CR).

msg72b(CR) ->
    EC = ?MSG_LIB:cre_ErrorCode(?megaco_not_ready),
    ED = ?MSG_LIB:cre_ErrorDescriptor(EC),
    msg72(ED, CR).

msg72b01() ->
    CR = cre_CtxReq(15, false),
    msg72b(CR).

msg72b02() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = isolate,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = oneway,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    CR    = cre_CtxReq(15, false, Top),
    msg72b(CR).

msg72b03() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = oneway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = bothway,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    CR    = cre_CtxReq(15, true, Top, true),
    msg72b(CR).

msg72b04() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = oneway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = bothway,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    CR    = cre_CtxReq(15, true, Top, false),
    msg72b(CR).

msg72c(CR) ->
    EC = ?MSG_LIB:cre_ErrorCode(?megaco_not_ready),
    ET = ?MSG_LIB:cre_ErrorText("Just another error string"),
    ED = ?MSG_LIB:cre_ErrorDescriptor(EC, ET),
    msg72(ED, CR).

msg72c01() ->
    CR = cre_CtxReq(15),
    msg72c(CR).

msg72c02() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = oneway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = oneway,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    PP    = cre_PropParm("tdmc/gain", "2"),
    Props = [PP],
    CR    = cre_CtxReq(15, true, Top, Props),
    msg72c(CR).

msg72c03() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = oneway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = isolate,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    PP    = cre_PropParm("tdmc/gain", ["2","4","8"], sublist, false), 
    Props = [PP],
    CR    = cre_CtxReq(15, true, Top, true, Props),
    msg72c(CR).

msg72c04() ->
    From1 = #megaco_term_id{id = ["11111111", "00000000", "00000000"]},
    To1   = #megaco_term_id{id = ["11111111", "00000000", "00001111"]},
    Dir1  = oneway,
    Top1  = cre_TopologyRequest(From1, To1, Dir1),
    From2 = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    To2   = #megaco_term_id{id = ["11111111", "00001111", "00001111"]},
    Dir2  = isolate,
    Top2  = cre_TopologyRequest(From2, To2, Dir2),
    Top   = [Top1, Top2],
    PP    = cre_PropParm("tdmc/gain", ["2","4","8"], sublist, false), 
    Props = [PP],
    CR    = cre_CtxReq(15, true, Top, false, Props),
    msg72c(CR).


msg73() ->
    Stat1    = cre_StatsParm("rtp/ps"),
    Stat2    = cre_StatsParm("nt/os","62300"),
    Stat3    = cre_StatsParm("rtp/pr","700"),
    Stat4    = cre_StatsParm("nt/or","45100"),
    Stat5    = cre_StatsParm("rtp/pl","0.2"),
    Stat6    = cre_StatsParm("rtp/jit","20"),
    Stat7    = cre_StatsParm("rtp/delay","40"),
    Stats    = [Stat1, Stat2, Stat3, Stat4, Stat5, Stat6, Stat7],
    cre_StatsDesc(Stats).

%% StatisticsDescriptor in AmmDescriptor
msg73a() ->
    StatDesc = msg73(),
    AmmDesc  = cre_AmmDesc(StatDesc),
    TermIDs  = [#megaco_term_id{id = ["11111111", "00001111", "00000000"]}],
    AmmReq   = cre_AmmReq(TermIDs, [AmmDesc]),
    Cmd      = cre_Cmd(addReq, AmmReq),
    CmdReq   = cre_CmdReq(Cmd),
    CID      = cre_CtxID(7301),
    ActReq   = cre_ActReq(CID, [CmdReq]),
    Actions  = [ActReq],
    TransId  = cre_TransId(7302),
    TransReq = cre_TransReq(TransId, Actions),
    Trans    = cre_Trans(TransReq),
    Mid      = ?MG1_MID,
    Mess     = cre_Msg(Mid, [Trans]),
    cre_MegacoMessage(Mess).
    

%% StatisticsDescriptor in IndAudStreamParms
msg73b1() ->
    IASD = cre_IndAudStatsDesc("nt/dur"),
    cre_IndAudStreamParms(IASD).

msg73b2(IAMD) ->
    IAP      = cre_IndAudParam(IAMD),
    AD       = cre_AuditDesc([IAP]),
    TermID   = #megaco_term_id{id = ["11111111", "00001111", "00000000"]},
    AudReq   = cre_AuditReq(TermID, AD),
    Cmd      = cre_Cmd(auditValueRequest, AudReq),
    CmdReq   = cre_CmdReq(Cmd),
    CID      = cre_CtxID(7311),
    ActReq   = cre_ActReq(CID, [CmdReq]),
    Actions  = [ActReq],
    TransId  = cre_TransId(7312),
    TransReq = cre_TransReq(TransId, Actions),
    Trans    = cre_Trans(TransReq),
    Mid      = ?MG1_MID,
    Mess     = cre_Msg(Mid, [Trans]),
    cre_MegacoMessage(Mess).
    
msg73b01() ->
    IASP = msg73b1(),
    IAMD = cre_IndAudMediaDesc(IASP),
    msg73b2(IAMD).
    
msg73b02() ->
    IASP = msg73b1(),
    SID  = cre_StreamID(303),
    IASD = cre_IndAudStreamDesc(SID, IASP),
    IAMD = cre_IndAudMediaDesc([IASD]),
    msg73b2(IAMD).
    
%% StatisticsDescriptor in StreamParms
msg73c1() ->
    StatDesc = msg73(),
    SP       = cre_StreamParms(StatDesc),
    SID      = cre_StreamID(505),
    cre_StreamDesc(SID, SP).

msg73c2(MD) ->
    ARP      = cre_AuditRetParam(MD),
    TA       = cre_TermAudit([ARP]),
    TermIDs  = [#megaco_term_id{id = ["11111111", "00001111", "00000000"]}],
    AmmsRep  = cre_AmmsReply(TermIDs, TA),
    CmdRep   = cre_CmdRep(moveReply, AmmsRep),
    CID      = cre_CtxID(606),
    ActRep   = cre_ActRep(CID, [CmdRep]),
    TransId  = cre_TransId(8899),
    TransRep = cre_TransRep(TransId, [ActRep]),
    Trans    = cre_Trans(TransRep),
    Mid      = ?MG1_MID,
    Mess     = cre_Msg(Mid, [Trans]),
    cre_MegacoMessage(Mess).

msg73c01() ->
    SD = msg73c1(),
    MD = cre_MediaDesc(SD),
    msg73c2(MD).

msg73c02() ->
    SD = msg73c1(),
    MD = cre_MediaDesc([SD]),
    msg73c2(MD).


%% New Signal (direction and requestID); msg74
msg74a1(D) ->
    Dir = cre_SigDir(D),
    cre_Sig("cg/rt", Dir, asn1_NOVALUE).

msg74a2(D, RID) ->
    Dir = cre_SigDir(D),
    cre_Sig("cg/rt", Dir, RID).

msg74a3(D, RID) ->
    Name = "al/ri",
    SID  = cre_StreamID(7401),
    ST   = cre_SigType(brief),
    Dur  = 7499,
    NC   = cre_NotifCompl([onTimeOut,otherReason]),
    KA   = cre_BOOLEAN(true),
    SPL  = [],
    Dir  = cre_SigDir(D),
    cre_Sig(Name, SID, ST, Dur, NC, KA, SPL, Dir, RID).

msg74a4(Sig) ->
    SR      = cre_SigReq(Sig),
    SD      = cre_SigsDesc([SR]),
    AD      = cre_AmmDesc(SD),
    TermIDs = [#megaco_term_id{id = ?A4444}], 
    AR      = cre_AmmReq(TermIDs, [AD]),
    Cmd     = cre_Cmd(modReq, AR),
    cre_CmdReq(Cmd).

msg74a01() ->
    Sig      = msg74a1(internal),
    CR       = msg74a4(Sig),
    CID      = cre_CtxID(7411),
    ActReq   = cre_ActReq(CID, [CR]),
    Actions  = [ActReq],
    TransId  = cre_TransId(7421),
    TransReq = cre_TransReq(TransId, Actions),
    Trans    = cre_Trans(TransReq),
    Mid      = ?MG1_MID,
    Mess     = cre_Msg(Mid, [Trans]),
    cre_MegacoMessage(Mess).
    
msg74a02() ->
    Sig      = msg74a1(both),
    CR       = msg74a4(Sig),
    CID      = cre_CtxID(7412),
    ActReq   = cre_ActReq(CID, [CR]),
    Actions  = [ActReq],
    TransId  = cre_TransId(7422),
    TransReq = cre_TransReq(TransId, Actions),
    Trans    = cre_Trans(TransReq),
    Mid      = ?MG1_MID,
    Mess     = cre_Msg(Mid, [Trans]),
    cre_MegacoMessage(Mess).
    
msg74a03() ->
    RID      = cre_ReqID(7433),
    Sig      = msg74a2(external, RID),
    CR       = msg74a4(Sig),
    CID      = cre_CtxID(7413),
    ActReq   = cre_ActReq(CID, [CR]),
    Actions  = [ActReq],
    TransId  = cre_TransId(7423),
    TransReq = cre_TransReq(TransId, Actions),
    Trans    = cre_Trans(TransReq),
    Mid      = ?MG1_MID,
    Mess     = cre_Msg(Mid, [Trans]),
    cre_MegacoMessage(Mess).
    
msg74a04() ->
    RID      = cre_ReqID(7434),
    Sig      = msg74a2(both, RID),
    CR       = msg74a4(Sig),
    CID      = cre_CtxID(7414),
    ActReq   = cre_ActReq(CID, [CR]),
    Actions  = [ActReq],
    TransId  = cre_TransId(7424),
    TransReq = cre_TransReq(TransId, Actions),
    Trans    = cre_Trans(TransReq),
    Mid      = ?MG1_MID,
    Mess     = cre_Msg(Mid, [Trans]),
    cre_MegacoMessage(Mess).
    
msg74a05() ->
    RID      = cre_ReqID(7435),
    Sig      = msg74a3(both, RID),
    CR       = msg74a4(Sig),
    CID      = cre_CtxID(7415),
    ActReq   = cre_ActReq(CID, [CR]),
    Actions  = [ActReq],
    TransId  = cre_TransId(7425),
    TransReq = cre_TransReq(TransId, Actions),
    Trans    = cre_Trans(TransReq),
    Mid      = ?MG1_MID,
    Mess     = cre_Msg(Mid, [Trans]),
    cre_MegacoMessage(Mess).
    
msg74a06() ->
    RID      = cre_ReqID(7436),
    Sig      = msg74a3(internal, RID),
    CR       = msg74a4(Sig),
    CID      = cre_CtxID(7416),
    ActReq   = cre_ActReq(CID, [CR]),
    Actions  = [ActReq],
    TransId  = cre_TransId(7426),
    TransReq = cre_TransReq(TransId, Actions),
    Trans    = cre_Trans(TransReq),
    Mid      = ?MG1_MID,
    Mess     = cre_Msg(Mid, [Trans]),
    cre_MegacoMessage(Mess).
    
    

%% New ServiceChangeParm (serviceChangeIncompleteFlag); msg75
msg75a(IncFlag) ->
    Method   = cre_SvcChMethod(restart),
    Address  = cre_SvcChAddr(portNumber, ?DEFAULT_PORT),
    Reason   = "901 mg col boot",
    Profile  = cre_SvcChProf("resgw",1),
    Parm     = cre_SvcChParm(Method, Address, [Reason], Profile, IncFlag),
    TermIDs  = [?megaco_root_termination_id], 
    Req      = cre_SvcChReq(TermIDs, Parm),
    Cmd      = cre_Cmd(serviceChangeReq, Req),
    CR       = cre_CmdReq(Cmd),
    CID      = cre_CtxID(7501),
    ActReq   = cre_ActReq(CID, [CR]),
    Actions  = [ActReq],
    TransId  = cre_TransId(7502),
    TransReq = cre_TransReq(TransId, Actions),
    Trans    = cre_Trans(TransReq),
    Mid      = ?MG1_MID,
    Mess     = cre_Msg(Mid, [Trans]),
    cre_MegacoMessage(Mess).

msg75a01() ->
    msg75a(asn1_NOVALUE).

msg75a02() ->
    msg75a('NULL').


msg76(IAMD) ->
    IAP   = cre_IndAudParam(IAMD),
    AD    = cre_AuditDesc(asn1_NOVALUE, [IAP]),
    AR    = cre_AuditReq(#megaco_term_id{id = ?A5556}, AD),
    CR    = cre_CmdReq({auditValueRequest, AR}),
    msg_request(?MG2_MID, 50076, ?megaco_null_context_id, [CR]).

%% IndAudLocalControlDescriptor and IndAudPropertyParm
msg76a(IALCD) when is_record(IALCD, 'IndAudLocalControlDescriptor') ->
    IASP  = cre_IndAudStreamParms(IALCD),
    SID   = 123,
    IASD  = cre_IndAudStreamDesc(SID, IASP),
    IAMD  = cre_IndAudMediaDesc([IASD]),
    msg76(IAMD).

msg76a01() ->
    Name1 = "nt/jit",
    IAPP1 = cre_IndAudPropertyParm(Name1),
    Name2 = "tdmc/ec",
    IAPP2 = cre_IndAudPropertyParm(Name2),
    SMS   = cre_StreamMode(recvOnly),
    IALCD = cre_IndAudLocalControlDesc(asn1_NOVALUE, 'NULL', 'NULL', 
				       [IAPP1, IAPP2], SMS),
    msg76a(IALCD).
    
msg76a02() ->
    Name1 = "tdmc/gain",
    PP1   = cre_PropParm("tdmc/gain", "2"), 
    IAPP1 = cre_IndAudPropertyParm(Name1, PP1),
    Name2 = "tdmc/gain",
    PP2   = cre_PropParm("tdmc/gain", "3"), 
    IAPP2 = cre_IndAudPropertyParm(Name2, PP2),
    SMS   = cre_StreamMode(recvOnly),
    IALCD = cre_IndAudLocalControlDesc(asn1_NOVALUE, 'NULL', 'NULL', 
				       [IAPP1, IAPP2], SMS),
    msg76a(IALCD).
    
%% IndAudTerminationStateDescription + ServiceState
msg76b(IATSD) ->
    IAMD = cre_IndAudMediaDesc(IATSD),
    msg76(IAMD).

msg76b01() ->
    SSS   = cre_ServiceState(outOfSvc),
    IATSD = cre_IndAudTermStateDesc([], asn1_NOVALUE, asn1_NOVALUE, SSS),
    msg76b(IATSD).

%% msg76b02() ->
%%     PP    = cre_PropParm("tdmc/gain", "2"),
%%     IAPP  = cre_IndAudPropertyParm("nt/jit", PP),
%%     IAPPs = [IAPP],
%%     SSS   = cre_ServiceState(outOfSvc),
%%     IATSD = cre_IndAudTermStateDesc(IAPPs, 'NULL', asn1_NOVALUE, SSS),
%%     msg76b(IATSD).

msg77a01() ->
    SN  = "tonegen/pt",
    Sig = cre_IndAudSig(SN, 7701),
    msg54(?MG2_MID, Sig).
		   

msg78a(Events) ->   
    EventsDesc = cre_EvsDesc(2223, Events),
    Signal     = cre_Sig("cg/rt"),
    Name       = "dialplan00",
    Body = "(0s| 00s|[1-7]xlxx|8lxxxxxxx|#xxxxxxx|*xx|9l1xxxxxxxxxx|9l011x.s)",
    DigitMapValue = cre_DigitMapValue(Body),
    DigMapDesc = cre_DigitMapDesc(Name, DigitMapValue),
    AmmReq     = cre_AmmReq([#megaco_term_id{id = ?A4444}],
                           [{eventsDescriptor, EventsDesc},
			    {signalsDescriptor, [{signal, Signal}]},
			    {digitMapDescriptor, DigMapDesc}]),
    CmdReq     = cre_CmdReq({modReq, AmmReq}),
    msg_request(?MG2_MID, 10001, ?megaco_null_context_id, [CmdReq]).
    
msg78a01() ->
    Name1  = "al/on",
    Strict = cre_EvParm("strict", ["state"]),
    EPL1   = [Strict],
    RE1    = cre_ReqEv(Name1, EPL1),
    Name2  = "al/on",
    SID2   = 7801,
    KA2    = true,
    EDM2   = cre_EvDM("dialplan00"),
    NB2    = cre_NotifBehav(notifyImmediate, 'NULL'),
    RED2   = asn1_NOVALUE, 
    RA2    = cre_ReqActs(KA2, EDM2, asn1_NOVALUE, asn1_NOVALUE, NB2, RED2),
    EPL2   = EPL1, 
    RE2    = cre_ReqEv(Name2, SID2, RA2, EPL2),
    msg78a([RE1, RE2]).

msg78a02() ->
    Name1  = "al/on",
    Strict = cre_EvParm("strict",["state"]),
    EPL1   = [Strict],
    RE1    = cre_ReqEv(Name1, EPL1),
    Name2  = "al/on",
    SID2   = 7802,
    KA2    = true,
    EDM2   = cre_EvDM("dialplan00"),
    NB2    = cre_NotifBehav(neverNotify, 'NULL'),
    RED2   = asn1_NOVALUE, 
    RA2    = cre_ReqActs(KA2, EDM2, asn1_NOVALUE, asn1_NOVALUE, NB2, RED2),
    EPL2   = EPL1, 
    RE2    = cre_ReqEv(Name2, SID2, RA2, EPL2),
    msg78a([RE1, RE2]).

msg78a03() ->
    Name1  = "al/on",
    Strict = cre_EvParm("strict",["state"]),
    EPL1   = [Strict],
    RE1    = cre_ReqEv(Name1, EPL1),
    Name2  = "al/on",
    SID2   = 7803,
    KA2    = true,
    EDM2   = cre_EvDM("dialplan00"),
    RED2   = cre_RegEmbedDesc(),
    NB2    = cre_NotifBehav(notifyRegulated, RED2),
    REvD2  = asn1_NOVALUE, 
    RA2    = cre_ReqActs(KA2, EDM2, asn1_NOVALUE, asn1_NOVALUE, NB2, REvD2),
    EPL2   = EPL1, 
    RE2    = cre_ReqEv(Name2, SID2, RA2, EPL2),
    msg78a([RE1, RE2]).

msg78a04() ->
    Name1  = "al/on",
    Strict = cre_EvParm("strict",["state"]),
    EPL1   = [Strict],
    RE1    = cre_ReqEv(Name1, EPL1),
    Name2  = "al/on",
    KA2    = true,
    EDM2   = cre_EvDM("dialplan00"),
    SRE2   = cre_SecReqEv("al/on"),
    SED2   = cre_SecEvsDesc(7814, [SRE2]),
    RED2   = cre_RegEmbedDesc(SED2),
    NB2    = cre_NotifBehav(notifyRegulated, RED2),
    REvD2  = asn1_NOVALUE, 
    RA2    = cre_ReqActs(KA2, EDM2, asn1_NOVALUE, asn1_NOVALUE, NB2, REvD2),
    EPL2   = EPL1, 
    RE2    = cre_ReqEv(Name2, 7824, RA2, EPL2),
    msg78a([RE1, RE2]).

msg78a05() ->
    Name1  = "al/on",
    Strict = cre_EvParm("strict",["state"]),
    EPL1   = [Strict],
    RE1    = cre_ReqEv(Name1, EPL1),
    Name2  = "al/on",
    SID2   = 7805,
    KA2    = true,
    EDM2   = cre_EvDM("dialplan00"),
    SD2    = cre_SigsDesc(),
    RED2   = cre_RegEmbedDesc(SD2),
    NB2    = cre_NotifBehav(notifyRegulated, RED2),
    REvD2  = asn1_NOVALUE, 
    RA2    = cre_ReqActs(KA2, EDM2, asn1_NOVALUE, asn1_NOVALUE, NB2, REvD2),
    EPL2   = EPL1, 
    RE2    = cre_ReqEv(Name2, SID2, RA2, EPL2),
    msg78a([RE1, RE2]).

msg78a06() ->
    Name1  = "al/on",
    Strict = cre_EvParm("strict",["state"]),
    EPL1   = [Strict],
    RE1    = cre_ReqEv(Name1, EPL1),
    Name2  = "al/on",
    KA2    = true,
    EDM2   = cre_EvDM("dialplan00"),
    SRE2   = cre_SecReqEv("al/on"),
    SED2   = cre_SecEvsDesc(7816, [SRE2]),
    Sig2   = cre_Sig("cg/rt", external, asn1_NOVALUE), 
    SR2    = cre_SigReq(Sig2), 
    SRs2   = [SR2],
    SD2    = cre_SigsDesc(SRs2),
    RED2   = cre_RegEmbedDesc(SED2, SD2),
    NB2    = cre_NotifBehav(notifyRegulated, RED2),
    REvD2  = asn1_NOVALUE, 
    RA2    = cre_ReqActs(KA2, EDM2, asn1_NOVALUE, asn1_NOVALUE, NB2, REvD2),
    EPL2   = EPL1, 
    RE2    = cre_ReqEv(Name2, 7826, RA2, EPL2),
    msg78a([RE1, RE2]).

msg78a07() ->
    Name1  = "al/on",
    Strict = cre_EvParm("strict",["state"]),
    EPL1   = [Strict],
    RE1    = cre_ReqEv(Name1, EPL1),
    Name2  = "al/on",
    KA2    = true,
    EDM2   = cre_EvDM("dialplan00"),
    SRE2   = cre_SecReqEv("al/on"),
    SED2   = cre_SecEvsDesc(7817, [SRE2]),
    Sig2   = cre_Sig("cg/rt", external, asn1_NOVALUE), 
    SR2    = cre_SigReq(Sig2), 
    SRs2   = [SR2],
    SD2    = cre_SigsDesc(SRs2),
    RED2   = cre_RegEmbedDesc(SED2, SD2),
    NB2    = cre_NotifBehav(notifyRegulated, RED2),
    REvD2  = 'NULL', 
    RA2    = cre_ReqActs(KA2, EDM2, asn1_NOVALUE, asn1_NOVALUE, NB2, REvD2),
    EPL2   = EPL1, 
    RE2    = cre_ReqEv(Name2, 7827, RA2, EPL2),
    msg78a([RE1, RE2]).

msg78a08() ->
    Name1  = "al/on",
    Strict = cre_EvParm("strict",["state"]),
    EPL1   = [Strict],
    RE1    = cre_ReqEv(Name1, EPL1),

    Name2  = "al/on",
    KA2    = true,
    EDM2   = cre_EvDM("dialplan00"),

    Sig21  = cre_Sig("al/ri", both, asn1_NOVALUE), 
    SR21   = cre_SigReq(Sig21), 
    SRs21  = [SR21],
    SD21   = cre_SigsDesc(SRs21),
    RED21  = cre_RegEmbedDesc(SD21),
    NB21   = cre_NotifBehav(notifyRegulated, RED21),
    SRA2   = cre_SecReqActs(KA2, EDM2, asn1_NOVALUE, NB21, 'NULL'),
    SRE2   = cre_SecReqEv("al/of", 7816, SRA2),
    SED2   = cre_SecEvsDesc(7826, [SRE2]),
    
    Sig22  = cre_Sig("cg/rt", external, asn1_NOVALUE), 
    SR22   = cre_SigReq(Sig22), 
    SRs22  = [SR22],
    SD22   = cre_SigsDesc(SRs22),
    RED22  = cre_RegEmbedDesc(SED2, SD22),
    NB22   = cre_NotifBehav(notifyRegulated, RED22),
    REvD2  = 'NULL', 

    RA2    = cre_ReqActs(KA2, EDM2, asn1_NOVALUE, asn1_NOVALUE, NB22, REvD2),
    EPL2   = EPL1, 
    RE2    = cre_ReqEv(Name2, 7836, RA2, EPL2),
    msg78a([RE1, RE2]).

msg78a09() ->
    Name1  = "al/on",
    Strict = cre_EvParm("strict",["state"]),
    EPL1   = [Strict],
    RE1    = cre_ReqEv(Name1, EPL1),

    Name2  = "al/on",
    KA2    = true,
    EDM2   = cre_EvDM("dialplan00"),

    SID21  = cre_StreamID(7819),
    ST21   = cre_SigType(timeOut),
    Dur21  = 7898,
    NC21   = cre_NotifCompl([onTimeOut, onInterruptByEvent, otherReason]),
    KA21   = cre_BOOLEAN(false),
    SPL21  = [],
    Dir21  = cre_SigDir(both),
    RID21  = cre_ReqID(7829),

    Sig21  = cre_Sig("cg/rt", SID21, ST21, Dur21, NC21, KA21, SPL21, Dir21, 
		     RID21, 7839),
    SR21   = cre_SigReq(Sig21), 
    SRs21  = [SR21],
    SD21   = cre_SigsDesc(SRs21),
    RED21  = cre_RegEmbedDesc(SD21),
    NB21   = cre_NotifBehav(notifyRegulated, RED21),

    SRA2   = cre_SecReqActs(KA2, EDM2, asn1_NOVALUE, NB21, 'NULL'),
    SRE2   = cre_SecReqEv("al/of", 7849, SRA2),
    SED2   = cre_SecEvsDesc(7859, [SRE2]),
    
    SID22  = cre_StreamID(7869),
    ST22   = cre_SigType(brief),
    Dur22  = 17809,
    NC22   = cre_NotifCompl([onTimeOut, otherReason]),
    KA22   = cre_BOOLEAN(true),
    SPL22  = [],
    Dir22  = cre_SigDir(external),
    RID22  = cre_ReqID(7879),

    Sig22  = cre_Sig("cg/rt", SID22, ST22, Dur22, NC22, KA22, SPL22, Dir22, 
		     RID22, 7889),
    SR22   = cre_SigReq(Sig22), 
    SRs22  = [SR22],
    SD22   = cre_SigsDesc(SRs22),
    RED22  = cre_RegEmbedDesc(SED2, SD22),

    NB22   = cre_NotifBehav(notifyRegulated, RED22),
    REvD2  = 'NULL', 

    RA2    = cre_ReqActs(KA2, EDM2, asn1_NOVALUE, asn1_NOVALUE, NB22, REvD2),
    EPL2   = EPL1, 
    RE2    = cre_ReqEv(Name2, 7899, RA2, EPL2),
    msg78a([RE1, RE2]).


msg79a01() ->
    TID1 = #megaco_term_id{id = ?A4444},
    TID2 = #megaco_term_id{id = ?A4445},
    TID3 = #megaco_term_id{id = ?A5555}, 
    TIDs = cre_TermIDList([TID1, TID2, TID3]),
    ErC  = cre_ErrCode(?megaco_not_ready),
    ErD  = cre_ErrDesc(ErC),
    ARP1 = cre_AuditRetParam(ErD),
    RE   = cre_ReqEv("al/of"),
    EvD  = cre_EvsDesc(7911,[RE]),
    ARP2 = cre_AuditRetParam(EvD),
    Tks  = [mediaToken, digitMapToken, statsToken, packagesToken],
    AD   = cre_AuditDesc(Tks),
    ARP3 = cre_AuditRetParam(AD),
    TAR  = cre_TermAudit([ARP1, ARP2, ARP3]),
    TLAR = cre_TermListAuditRes(TIDs, TAR),
    AudR = cre_AuditRep(TLAR),
    CR   = cre_CmdRep(auditValueReply, AudR),
    ActR = cre_ActRep(7921, [CR]),
    Acts = [ActR],
    msg_reply(?MG2_MID, 7931, Acts).


%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
%% Pretty RFC 3525 messages:

%% Added Reason
rfc3525_msg1() ->
"MEGACO/" ?VERSION_STR " [124.124.124.222] Transaction = 9998 {
   Context = - {
      ServiceChange = ROOT {
         Services {
            Method = Restart,
            Reason = 901,
            ServiceChangeAddress = 55555, 
            Profile = ResGW/1
         }
      }
   } 
}".

rfc3525_msg2() ->
"MEGACO/" ?VERSION_STR " [123.123.123.4]:55555 Reply = 9998 {
   Context = - {
      ServiceChange = ROOT {
         Services {
            ServiceChangeAddress = 55555, 
            Profile = ResGW/1
         } 
      } 
   } 
}".


%% Removed "," after LocalControl ending "}"
rfc3525_msg3() ->
"MEGACO/" ?VERSION_STR " [123.123.123.4]:55555 Transaction = 9999 {
   Context = - {
      Modify = A4444 {
         Media { 
            Stream = 1 {
               LocalControl {
                  Mode = SendReceive,
                  tdmc/gain=2,  ; in dB,
                  tdmc/ec=on
               }
            }
         },
         Events = 2222 {
            al/of {strict=state}
         }
      }
   }
}".

%% Removed the outermost "{}" pair (before the Reply token)
rfc3525_msg4() ->
"MEGACO/" ?VERSION_STR " [124.124.124.222]:55555 Reply = 9999 {
   Context = - {
      Modify = A4444
   }
}".

rfc3525_msg6() ->
"MEGACO/" ?VERSION_STR " [124.124.124.222]:55555 Transaction = 10000 {
   Context = - {
      Notify = A4444 {
         ObservedEvents =2222 {
            19990729T22000000:al/of{init=false}
         }
      }
   } 
}".
 

rfc3525_msg7() ->
"MEGACO/" ?VERSION_STR " [123.123.123.4]:55555 Reply = 10000 {
   Context = - {
      Notify = A4444
   } 
}".

rfc3525_msg8() ->
"MEGACO/" ?VERSION_STR " [123.123.123.4]:55555 Transaction = 10001 {
   Context = - {
      Modify = A4444 {
         Events = 2223 {
            al/on {strict=state}, 
            dd/ce {DigitMap=Dialplan0}
	 },
         Signals {cg/dt},
         DigitMap = Dialplan0 { 
            (0| 00|[1-7]xxx|8xxxxxxx|fxxxxxxx|exx|91xxxxxxxxxx|9011x.)
         }
      }
   }
}".

rfc3525_msg9() ->
"MEGACO/" ?VERSION_STR " [124.124.124.222]:55555 Reply = 10001 {
   Context = - {
      Modify = A4444
   }
}".

rfc3525_msg10() ->
"MEGACO/" ?VERSION_STR " [124.124.124.222]:55555 Transaction = 10002 {
   Context = - {
      Notify = A4444 {
         ObservedEvents =2223 {
            19990729T22010001:dd/ce {
               ds=\"916135551212\",
               Meth=UM
            }
         }
      }
   } 
}".


rfc3525_msg11() ->
"MEGACO/" ?VERSION_STR " [123.123.123.4]:55555 Reply = 10002 {
   Context = - {
      Notify = A4444
   }
}".

%% Added ?
rfc3525_msg12() ->
"MEGACO/" ?VERSION_STR " [123.123.123.4]:55555 Transaction = 10003 {
   Context = $ {
      Add = A4444,
      Add = $ {
         Media {
            Stream = 1 {
               LocalControl {
                  Mode = ReceiveOnly,
                  nt/jit=40 ; in ms
               },
               Local { 
                  v=0 c=IN IP4 $ m=audio $ RTP/AVP 4 a=ptime:30 v=0 c=IN IP4 $ m=audio $ RTP/AVP 0
               }
            }
         }
      }
   } 
}".

%% Added ?
rfc3525_msg13() ->
"MEGACO/" ?VERSION_STR " [124.124.124.222]:55555 Reply = 10003 {
   Context = 2000 {
      Add = A4444,
      Add = A4445 {
         Media {
            Stream = 1 {
               Local { 
v=0 
o=- 2890844526 2890842807 IN IP4 124.124.124.222 
s=- 
t= 0 0 
c=IN IP4 124.124.124.222 
m=audio 2222 RTP/AVP 4 
a=ptime:30 
a=recvonly 
               } ; RTP profile for G.723.1 is 4
            }
         }
      }
   } 
}".

%% 
%% Added ?
rfc3525_msg14() ->
"MEGACO/" ?VERSION_STR " [123.123.123.4]:55555 Transaction = 50003 {
   Context = $ {
      Add = A5555 { 
         Media {
            Stream = 1 {
               LocalControl {
                  Mode = SendReceive
               }
            }
         },
         Events = 1234 {
            al/of {strict=state}
         },
         Signals {al/ri}
      },
      Add = $ {
         Media {
            Stream = 1 {
               LocalControl {
                  Mode = SendReceive,
                  nt/jit=40 ; in ms
               },
               Local { 
                  v=0 c=IN IP4 $ m=audio $ RTP/AVP 4 a=ptime:30
               },
               Remote { 
                  v=0 c=IN IP4 124.124.124.222 m=audio 2222 RTP/AVP 4 a=ptime:30
               } ; RTP profile for G.723.1 is 4
            }
         }
      }
   } 
}".

%% Added ?
rfc3525_msg15() ->
"MEGACO/" ?VERSION_STR " [125.125.125.111]:55555 Reply = 50003 {
   Context = 5000 {
      Add = A5555,
      Add = A5556 {
         Media {
            Stream = 1 {
               Local { 
                  v=0 o=- 7736844526 7736842807 IN IP4 125.125.125.111 s=- t= 0 0 c=IN IP4 125.125.125.111 m=audio 1111 RTP/AVP 4 
               } ; RTP profile for G723.1 is 4
            } 
         }
      }
   }
}".

%% Added ?
rfc3525_msg16a() ->
"MEGACO/" ?VERSION_STR " [123.123.123.4]:55555 Transaction = 10005 {
   Context = 2000 {
      Modify = A4444 {
         Signals {cg/rt}
      },
      Modify = A4445 {
         Media {
            Stream = 1 {
               Remote { 
                  v=0 o=- 7736844526 7736842807 IN IP4 125.125.125.111 s=- t= 0 0 c=IN IP4 125.125.125.111 m=audio 1111 RTP/AVP 4
	       } ; RTP profile for G723.1 is 4
            } 
         }
      }
   }
}".

rfc3525_msg16b() ->
"MEGACO/" ?VERSION_STR " [124.124.124.222]:55555 Reply = 10005 {
   Context = 2000 {
      Modify = A4444, 
      Modify = A4445
   }
}".

rfc3525_msg17a() ->
"MEGACO/" ?VERSION_STR " [125.125.125.111]:55555 Transaction = 50005 {
   Context = 5000 {
      Notify = A5555 {
         ObservedEvents = 1234 {
            19990729T22020002:al/of{init=false}
         }
      }
   }
}".

rfc3525_msg17b() ->
"MEGACO/" ?VERSION_STR " [123.123.123.4]:55555 Reply = 50005 {
   Context = - {
      Notify = A5555
   }
}".

%% Removed "{ }" after Signals
rfc3525_msg17c() ->
"MEGACO/" ?VERSION_STR " [123.123.123.4]:55555 Transaction = 50006 {
   Context = 5000 {
      Modify = A5555 {
         Events = 1235 {
            al/on{strict=state}
         },
         Signals ; to turn off ringing
      }
   }
}".

rfc3525_msg17d() ->
"MEGACO/" ?VERSION_STR " [125.125.125.111]:55555 Reply = 50006 {
   Context = 5000 {
      Modify = A4445
   }
}".

%% Removed "{ }" after Signals
rfc3525_msg18a() ->
"MEGACO/" ?VERSION_STR " [123.123.123.4]:55555 Transaction = 10006 {
   Context = 2000 {
      Modify = A4445 {
         Media {
            Stream = 1 {
               LocalControl {
                  Mode = SendReceive
               }
            }
         }
      },
      Modify = A4444 {
         Signals 
      }
   }
}".

rfc3525_msg18b() ->
"MEGACO/" ?VERSION_STR " [124.124.124.222]:55555 Reply = 10006 {
   Context = 2000 {
      Modify = A4445, 
      Modify = A4444
   }
}".

rfc3525_msg19() ->
"MEGACO/" ?VERSION_STR " [123.123.123.4]:55555 Transaction = 50007 {
   Context = - {
      AuditValue = A5556 {
         Audit {
            Media, DigitMap, Events, Signals, Packages, Statistics
         }
      }
   }
}".

%% Added ?
rfc3525_msg20() ->
"MEGACO/" ?VERSION_STR " [125.125.125.111]:55555 Reply = 50007 {
   Context = - { 
      AuditValue = A5556 {
         Media {
            TerminationState { 
               ServiceStates = InService,
               Buffer = OFF 
            },
            Stream = 1 {
               LocalControl { 
                  Mode = SendReceive,
                  nt/jit=40 
               },
               Local { 
                  v=0 o=- 7736844526 7736842807 IN IP4 125.125.125.111 s=- t= 0 0 c=IN IP4 125.125.125.111 m=audio 1111 RTP/AVP  4 a=ptime:30
               },
               Remote { 
                  v=0 o=- 2890844526 2890842807 IN IP4 124.124.124.222 s=- t= 0 0 c=IN IP4 124.124.124.222 m=audio 2222 RTP/AVP  4 a=ptime:30
               }
            }
         },
         Events,
         Signals,
         DigitMap,
         Packages {nt-1, rtp-1},
         Statistics { 
            rtp/ps=1200,  ; packets sent
            nt/os=62300, ; octets sent
            rtp/pr=700, ; packets received
            nt/or=45100, ; octets received
            rtp/pl=0.2,  ; % packet loss
            rtp/jit=20,
            rtp/delay=40 ; avg latency
         } 
      }
   }
}".

rfc3525_msg21a() ->
"MEGACO/" ?VERSION_STR " [125.125.125.111]:55555 Transaction = 50008 {
   Context = 5000 {
      Notify = A5555 {
         ObservedEvents =1235 {
            19990729T24020002:al/on {init=false}
         }
      }
   }
}".

rfc3525_msg21b() ->
"MEGACO/" ?VERSION_STR " [123.123.123.4]:55555 Reply = 50008 {
   Context = - {
      Notify = A5555
   }
}".

rfc3525_msg22a() ->
"MEGACO/" ?VERSION_STR " [123.123.123.4]:55555 Transaction = 50009 {
   Context = 5000 {
      Subtract = A5555 {
         Audit {
            Statistics
         }
      },
      Subtract = A5556 {
         Audit {
            Statistics
         }
      }
   }
}".

%% Added ?
rfc3525_msg22b() ->
"MEGACO/" ?VERSION_STR " [125.125.125.111]:55555 Reply = 50009 {
   Context = 5000 {
      Subtract = A5555 {
         Statistics {
            nt/os=45123, ; Octets Sent
            nt/dur=40 ; in seconds
         }
      },
      Subtract = A5556 {
         Statistics {
            rtp/ps=1245, ; packets sent
            nt/os=62345, ; octets sent
            rtp/pr=780, ; packets received
            nt/or=45123, ; octets received
            rtp/pl=10, ;  % packets lost
            rtp/jit=27,
            rtp/delay=48 ; average latency
         }
      }
   }
}".

rfc3525_msgs() ->
    [
     {msg1,   rfc3525_msg1()},
     {msg2,   rfc3525_msg2()},
     {msg3,   rfc3525_msg3()},
     {msg4,   rfc3525_msg4()},
     {msg6,   rfc3525_msg6()},
     {msg7,   rfc3525_msg7()},
     {msg8,   rfc3525_msg8()},
     {msg9,   rfc3525_msg9()},
     {msg10,  rfc3525_msg10()},
     {msg11,  rfc3525_msg11()},
     {msg12,  rfc3525_msg12()},
     {msg13,  rfc3525_msg13()},
     {msg14,  rfc3525_msg14()},
     {msg15,  rfc3525_msg15()},
     {msg16a, rfc3525_msg16a()},
     {msg16b, rfc3525_msg16b()},
     {msg17a, rfc3525_msg17a()},
     {msg17b, rfc3525_msg17b()},
     {msg17c, rfc3525_msg17c()},
     {msg17d, rfc3525_msg17d()},
     {msg18a, rfc3525_msg18a()},
     {msg18b, rfc3525_msg18b()},
     {msg19,  rfc3525_msg19()},
     {msg20,  rfc3525_msg20()},
     {msg21a, rfc3525_msg21a()},
     {msg21b, rfc3525_msg21b()},
     {msg22a, rfc3525_msg22a()},
     {msg22b, rfc3525_msg22b()}
    ].

rfc3525_msgs_display() ->
    Msgs = rfc3525_msgs(),
    Fun = fun({Name, Msg}) ->
		  io:format("~w: ~n~s~n~n", [Name, Msg])
	  end,
    lists:foreach(Fun, Msgs).

rfc3525_msgs_test() ->
    put(dbg,true),
    Res = rfc3525_msgs_test(megaco_pretty_text_encoder, [], 2),
    erase(dbg),
    io:format("~w~n", [Res]).

rfc3525_msgs_test(Codec, Config, Ver) ->
    io:format("-----------------------------------------"
	      "~ntesting with"
	      "~n   Codec:   ~w"
	      "~n   Config:  ~w"
	      "~n   Version: ~w"
	      "~n", [Codec, Config, Ver]),
    Msgs = rfc3525_msgs(),
    Test = fun({N,M1}) ->
		   %% io:format("testing ~w: ", [N]),
		   io:format("~n*** testing ~w *** ~n~s~n", [N,M1]),
		   Bin1 = erlang:list_to_binary(M1),
		   case (catch Codec:decode_message(Config, Ver, Bin1)) of
		       {ok, M2} ->
			   %% io:format("d", []),
			   io:format("decoded:~n~p~n", [M2]),
			   case (catch Codec:encode_message(Config, Ver, M2)) of
			       {ok, Bin2} when is_binary(Bin2) ->
				   %% io:format("e~n", []),
				   io:format("encode: ~n~s~n", [erlang:binary_to_list(Bin2)]),
				   {N,ok};
			       {ok, M3} ->
				   %% io:format("e~n", []),
				   io:format("encode: ~n~s~n", [M3]),
				   {N,ok};
			       E ->
				   io:format("~n~p~n", [E]),
				   {N,encode_error}
			   end;
		       E ->
			   io:format("~n~p~n", [E]),
			   {N,decode_error}
		   end
	  end,
    [Test(M) || M <- Msgs].

%% --------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% skip(Reason) ->
%%     megaco_codec_test_lib:skip(Reason).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ticket_compact_encode_decode_ok(Msg) ->
    ticket_compact_encode_decode_ok(Msg, []).

ticket_compact_encode_decode_ok(Msg, Conf) ->
    Codec  = megaco_compact_text_encoder, 
    ticket_encode_decode_ok(Msg, Codec, Conf).

ticket_pretty_encode_decode_ok(Msg) ->
    ticket_pretty_encode_decode_ok(Msg, []).

ticket_pretty_encode_decode_ok(Msg, Conf) ->
    Codec  = megaco_pretty_text_encoder, 
    ticket_encode_decode_ok(Msg, Codec, Conf).

ticket_encode_decode_ok(Msg, Codec, Conf0) ->
    Conf   = [?EC_V3|Conf0],
    Encode = fun(M) -> encode_message(Codec, Conf, M) end,
    Decode = fun(B) -> decode_message(Codec, false, Conf, B) end,
    Check  = fun(M1, M2) -> chk_MegacoMessage(M1, M2) end,
    megaco_codec_test_lib:expect_encode_decode(Msg, Encode, Decode, Check).

%% -- 

%% ticket_compact_encode_error(Msg) ->
%%     ticket_compact_encode_error(Msg, []).

%% ticket_compact_encode_error(Msg, Conf) ->
%%     Codec  = megaco_compact_text_encoder, 
%%     ticket_encode_error(Msg, Codec, Conf).

%% ticket_pretty_encode_error(Msg) ->
%%     ticket_pretty_encode_error(Msg, []).

ticket_pretty_encode_error(Msg, Conf) when is_list(Conf) ->
    Codec = megaco_pretty_text_encoder, 
    ticket_encode_error(Msg, Codec, Conf);
ticket_pretty_encode_error(Msg, Check) when is_function(Check) ->
    ticket_pretty_encode_error(Msg, [], Check).

ticket_pretty_encode_error(Msg, Conf, Check) when is_function(Check) ->
    Codec = megaco_pretty_text_encoder, 
    ticket_encode_error(Msg, Codec, Conf, Check).

ticket_encode_error(Msg, Codec, Conf) when is_list(Conf) ->
    Check = fun(_) -> ok end, % Only called when encode failes
    ticket_encode_error(Msg, Codec, Conf, Check).

ticket_encode_error(Msg, Codec, Conf0, Check) ->
    Conf   = [?EC_V3|Conf0],
    Encode = fun(M) -> encode_message(Codec, Conf, M) end,
    megaco_codec_test_lib:expect_encode(Msg, Encode, Check).

%% --

ticket_compact_decode_encode_ok(Msg) ->
    ticket_compact_decode_encode_ok(Msg, []).

ticket_compact_decode_encode_ok(Msg, Conf) ->
    Codec  = megaco_compact_text_encoder, 
    ticket_decode_encode_ok(Msg, Codec, Conf).

ticket_pretty_decode_encode_ok(Msg) ->
    ticket_pretty_decode_encode_ok(Msg, []).

ticket_pretty_decode_encode_ok(Msg, Conf) ->
    Codec  = megaco_pretty_text_encoder, 
    ticket_decode_encode_ok(Msg, Codec, Conf).

ticket_decode_encode_ok(Msg, Codec, Conf0) ->
    Conf   = [?EC_V3|Conf0],
    Decode = fun(B) -> decode_message(Codec, false, Conf, B) end,
    Encode = fun(M) -> encode_message(Codec, Conf, M) end,
    Check  = fun(M1, M2) -> chk_MegacoMessage(M1, M2) end,
    megaco_codec_test_lib:expect_decode_encode(Msg, Decode, Encode, Check).

%% -- 

ticket_pretty_decode_encode_only(Msg, Check) ->
    ticket_pretty_decode_encode_only(Msg, Check, []).

ticket_pretty_decode_encode_only(Msg, Check, Conf) ->
    Codec  = megaco_pretty_text_encoder, 
    ticket_decode_encode_only(Msg, Codec, Check, Conf).

ticket_decode_encode_only(Msg, Codec, Check, Conf0) ->
    Conf   = [?EC_V3|Conf0],
    Decode = fun(B) -> decode_message(Codec, false, Conf, B) end,
    Encode = fun(M) -> encode_message(Codec, Conf, M) end,
    megaco_codec_test_lib:expect_decode_encode_only(Msg, Decode, Encode, 
						    Check).

%% -- 

ticket_pretty_encode_decode_only(Msg) ->
    ticket_pretty_encode_decode_only(Msg, []).

ticket_pretty_encode_decode_only(Msg, Conf) when is_list(Conf) ->
    Codec = megaco_pretty_text_encoder, 
    ticket_encode_decode_only(Msg, Codec, Conf);
ticket_pretty_encode_decode_only(Msg, Check) when is_function(Check) ->
    ticket_pretty_encode_decode_only(Msg, Check, []).

ticket_pretty_encode_decode_only(Msg, Check, Conf) ->
    Codec = megaco_pretty_text_encoder, 
    ticket_encode_decode_only(Msg, Codec, Check, Conf).

ticket_encode_decode_only(Msg, Codec, Conf) ->
    Check = fun(_) -> ok end, 
    ticket_encode_decode_only(Msg, Codec, Check, Conf).

ticket_encode_decode_only(Msg, Codec, Check, Conf0) ->
    Conf   = [?EC_V3|Conf0],
    Encode = fun(M) -> encode_message(Codec, Conf, M) end,
    Decode = fun(B) -> decode_message(Codec, false, Conf, B) end,
    megaco_codec_test_lib:expect_encode_decode_only(Msg, Encode, Decode, 
						    Check).

%% -- 

ticket_pretty_decode_only(Msg) ->
    ticket_pretty_decode_only(Msg, []).

ticket_pretty_decode_only(Msg, Conf) ->
    Codec = megaco_pretty_text_encoder, 
    ticket_decode_only(Msg, Codec, Conf).

ticket_decode_only(Msg, Codec, Conf) ->
    Check = fun(_) -> ok end, 
    ticket_decode_only(Msg, Codec, Check, Conf).

ticket_decode_only(Msg, Codec, Check, Conf0) ->
    Conf   = [?EC_V3|Conf0],
    Decode = fun(B) -> decode_message(Codec, false, Conf, B) end,
    megaco_codec_test_lib:expect_decode_only(Msg, Decode, Check).

ticket_check_decode_only_error_reason(R, Check) 
  when is_list(R) and is_function(Check) ->
    case lists:keysearch(reason, 1, R) of
	{value, {reason, Reason}} ->
	    Check(Reason);
	false ->
	    {error, {reason_not_found, R}}
    end.


%% -- 

ticket_compact_decode_error(Msg) ->
    ticket_compact_decode_error(Msg, []).

ticket_compact_decode_error(Msg, Conf) ->
    Codec = megaco_compact_text_encoder, 
    ticket_decode_error(Msg, Codec, Conf).

ticket_pretty_decode_error(Msg) ->
    ticket_pretty_decode_error(Msg, []).

ticket_pretty_decode_error(Msg, Conf) when is_list(Conf) ->
    Codec = megaco_pretty_text_encoder, 
    ticket_decode_error(Msg, Codec, Conf);
ticket_pretty_decode_error(Msg, Check) when is_function(Check) ->
    ticket_pretty_decode_error(Msg, [], Check).

ticket_pretty_decode_error(Msg, Conf, Check) ->
    Codec = megaco_pretty_text_encoder, 
    ticket_decode_error(Msg, Codec, Conf, Check).

ticket_decode_error(Msg, Codec, Conf) ->
    Check = fun(X) -> 
		    d("decode error reason: ~n~p~n", [X]),
		    ok 
	    end, % Only called when decode failes
    ticket_decode_error(Msg, Codec, Conf, Check).

ticket_decode_error(Msg, Codec, Conf0, Check) ->
    Conf   = [?EC_V3|Conf0],
    Decode = fun(B) -> decode_message(Codec, false, Conf, B) end,
    megaco_codec_test_lib:expect_decode(Msg, Decode, Check).

%% -- 

%% ticket_expect_exec(Instructions, Msg) ->
%%     megaco_codec_test_lib:expect_exec(Instructions, Msg).

%% ticket_expect_instruction(Desc, Cmd, Verify) ->
%%     megaco_codec_test_lib:expect_instruction(Desc, Cmd, Verify).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% pretty_decode_message(DynamicDecode, Conf, Bin) ->
%%     decode_message(megaco_pretty_text_encoder, DynamicDecode, Conf, Bin).

%% compact_decode_message(DynamicDecode, Conf, Bin) ->
%%     decode_message(megaco_compact_text_encoder, DynamicDecode, Conf, Bin).

decode_message(Codec, DynamicDecode, Conf, Bin) ->
    megaco_codec_test_lib:decode_message(Codec, DynamicDecode, ?VERSION, 
					 Conf, Bin).

%% pretty_encode_message(Conf, Msg) ->
%%     encode_message(megaco_pretty_text_encoder, Conf, Msg).

%% compact_encode_message(Conf, Msg) ->
%%     encode_message(megaco_compact_text_encoder, Conf, Msg).

encode_message(Codec, Conf, Msg) ->
    megaco_codec_test_lib:encode_message(Codec, ?VERSION, Conf, Msg).

test_msgs(Codec, DynamicDecode, Conf, Msgs) ->
    megaco_codec_test_lib:test_msgs(Codec, DynamicDecode, ?VERSION, Conf, 
				    fun chk_MegacoMessage/2, Msgs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chk_MegacoMessage(M1, M2) ->
    ?MSG_LIB:chk_MegacoMessage(M1, M2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cre_MegacoMessage(Mess) ->
    ?MSG_LIB:cre_MegacoMessage(Mess).

cre_MegacoMessage(Auth, Mess) ->
    ?MSG_LIB:cre_MegacoMessage(Auth, Mess).

cre_MegacoMessage(V, Mid, Body) ->
    Mess = ?MSG_LIB:cre_Message(V, Mid, Body),
    cre_MegacoMessage(Mess).

cre_AuthHeader() ->
    SecParmIdx = [239, 205, 171, 137], 
    SeqNum     = [18, 52, 86, 120], 
    AD         = [18, 52, 86, 120, 137, 171, 205, 239, 118, 84, 50, 16],
    cre_AuthHeader(SecParmIdx, SeqNum, AD).

cre_AuthHeader(Idx, Num, D) ->
    ?MSG_LIB:cre_AuthenticationHeader(Idx, Num, D).

cre_Msg(Mid, Body) ->
    cre_Msg(?VERSION, Mid, Body).

cre_Msg(V, Mid, Body) ->
    ?MSG_LIB:cre_Message(V, Mid, Body).

cre_TransId(TransId) ->
    ?MSG_LIB:cre_TransactionId(TransId).

cre_Trans(Trans) ->
    ?MSG_LIB:cre_Transaction(Trans).

cre_TransReq(TransId, Actions) ->
    ?MSG_LIB:cre_TransactionRequest(TransId, Actions).

cre_TransRep(TransId, Actions) ->
    ?MSG_LIB:cre_TransactionReply(TransId, Actions).

cre_TransAck(First, Last) ->
    ?MSG_LIB:cre_TransactionAck(First, Last).
    
cre_ActReq(CtxId, CmdReqs) ->
    ?MSG_LIB:cre_ActionRequest(CtxId, CmdReqs).

cre_ActRep(CtxId, CmdReps) ->
    ?MSG_LIB:cre_ActionReply(CtxId, CmdReps).

cre_ActRep(CtxId, ED, CR, CmdReps) ->
    ?MSG_LIB:cre_ActionReply(CtxId, ED, CR, CmdReps).

cre_CtxReq() ->
    ?MSG_LIB:cre_ContextRequest().

cre_CtxReq(A) ->
    ?MSG_LIB:cre_ContextRequest(A).

cre_CtxReq(A, B) ->
    ?MSG_LIB:cre_ContextRequest(A, B).

cre_CtxReq(A, B, C) ->
    ?MSG_LIB:cre_ContextRequest(A, B, C).

cre_CtxReq(A, B, C, D) ->
    ?MSG_LIB:cre_ContextRequest(A, B, C, D).

cre_CtxReq(A, B, C, D, E) ->
    ?MSG_LIB:cre_ContextRequest(A, B, C, D, E).

cre_CtxReq(Prio, Em, Top, Ieps, CtxProp, CtxList) ->
    ?MSG_LIB:cre_ContextRequest(Prio, Em, Top, Ieps, CtxProp, CtxList).

cre_CtxAttrAuditReq() ->
    ?MSG_LIB:cre_ContextAttrAuditRequest().

% cre_CtxAttrAuditReq(A) ->
%     ?MSG_LIB:cre_ContextAttrAuditRequest(A).

% cre_CtxAttrAuditReq(A, B) ->
%     ?MSG_LIB:cre_ContextAttrAuditRequest(A, B).

cre_CtxAttrAuditReq(A, B, C) ->
    ?MSG_LIB:cre_ContextAttrAuditRequest(A, B, C).

cre_CtxAttrAuditReq(A, B, C, D) ->
    ?MSG_LIB:cre_ContextAttrAuditRequest(A, B, C, D).

cre_CtxAttrAuditReq(A, B, C, D, E) ->
    ?MSG_LIB:cre_ContextAttrAuditRequest(A, B, C, D, E).

cre_CtxAttrAuditReq(Top, Em, Prio, Ieps, Ctx, SPrio) ->
    ?MSG_LIB:cre_ContextAttrAuditRequest(Top, Em, Prio, Ieps, Ctx, SPrio).

cre_CtxAttrAuditReq(A, B, C, D, E, F, G) ->
    ?MSG_LIB:cre_ContextAttrAuditRequest(A, B, C, D, E, F, G).

cre_CtxAttrAuditReq(Top, Em, Prio, Ieps, Ctx, SPrio, SEm, SIeps) ->
    ?MSG_LIB:cre_ContextAttrAuditRequest(Top, Em, Prio, Ieps, Ctx,
					 SPrio, SEm, SIeps).

cre_CtxAttrAuditReq(Top, Em, Prio, Ieps, Ctx, SPrio, SEm, SIeps, SLog) ->
    ?MSG_LIB:cre_ContextAttrAuditRequest(Top, Em, Prio, Ieps, Ctx,
					 SPrio, SEm, SIeps, SLog).

cre_TopologyRequest(From, To, Dir) ->
    ?MSG_LIB:cre_TopologyRequest(From, To, Dir).

%% Ind Aud related:

cre_IndAudParam(IAP) ->
    ?MSG_LIB:cre_IndAuditParameter(IAP).

cre_IndAudMediaDesc(D) ->
    ?MSG_LIB:cre_IndAudMediaDescriptor(D).

cre_IndAudStreamDesc(SID, SP) ->
    ?MSG_LIB:cre_IndAudStreamDescriptor(SID, SP).

cre_IndAudStreamParms(LCD) ->
    ?MSG_LIB:cre_IndAudStreamParms(LCD).

cre_IndAudLocalControlDesc(A, B, C, D) ->
    ?MSG_LIB:cre_IndAudLocalControlDescriptor(A, B, C, D).

cre_IndAudLocalControlDesc(SM, RV, RG, PP, SMS) ->
    ?MSG_LIB:cre_IndAudLocalControlDescriptor(SM, RV, RG, PP, SMS).

cre_IndAudPropertyParm(Name) ->
    ?MSG_LIB:cre_IndAudPropertyParm(Name).

cre_IndAudPropertyParm(Name, PP) ->
    ?MSG_LIB:cre_IndAudPropertyParm(Name, PP).

cre_IndAudTermStateDesc(PP) ->
    ?MSG_LIB:cre_IndAudTerminationStateDescriptor(PP).

cre_IndAudTermStateDesc(PP, EBC, SS) ->
    ?MSG_LIB:cre_IndAudTerminationStateDescriptor(PP, EBC, SS).

cre_IndAudTermStateDesc(PP, EBC, SS, SSS) ->
    ?MSG_LIB:cre_IndAudTerminationStateDescriptor(PP, EBC, SS, SSS).

cre_IndAudEvsDesc(RID, PN) 
  when is_integer(RID) ->
    ?MSG_LIB:cre_IndAudEventsDescriptor(RID, PN).

cre_IndAudEvBufDesc(EN, SID) ->
    ?MSG_LIB:cre_IndAudEventBufferDescriptor(EN, SID).

cre_IndAudSigsDesc(D) ->
    ?MSG_LIB:cre_IndAudSignalsDescriptor(D).

cre_IndAudSig(SN) ->
    ?MSG_LIB:cre_IndAudSignal(SN).

cre_IndAudSig(SN, SigRID) ->
    ?MSG_LIB:cre_IndAudSignal(SN, SigRID).

cre_IndAudSeqSigList(ID, SL) ->
    ?MSG_LIB:cre_IndAudSeqSigList(ID, SL).

cre_IndAudDigitMapDesc(DMN) ->
    ?MSG_LIB:cre_IndAudDigitMapDescriptor(DMN).

cre_IndAudStatsDesc(SN) ->
    ?MSG_LIB:cre_IndAudStatisticsDescriptor(SN).

cre_IndAudPkgsDesc(PN, PV) ->
    ?MSG_LIB:cre_IndAudPackagesDescriptor(PN, PV).

%% Parameter related
cre_PropParm(Name, Val) ->
    ?MSG_LIB:cre_PropertyParm(Name, [Val]).

cre_PropParm(Name, Vals, Tag, EI) ->
    ?MSG_LIB:cre_PropertyParm(Name, Vals, Tag, EI).


%% Statistics related
cre_StatsDesc(SPs) ->
    ?MSG_LIB:cre_StatisticsDescriptor(SPs).

cre_StatsParm(Name) ->
    ?MSG_LIB:cre_StatisticsParameter(Name).

cre_StatsParm(Name, Val) ->
    ?MSG_LIB:cre_StatisticsParameter(Name, [Val]).


% Event related 
cre_EvParm(Name, Val) ->
    ?MSG_LIB:cre_EventParameter(Name, Val).

cre_ObsEv(Name, Not) ->
    ?MSG_LIB:cre_ObservedEvent(Name, Not).
cre_ObsEv(Name, Not, Par) ->
    ?MSG_LIB:cre_ObservedEvent(Name, Par, Not).

cre_ReqEv(Name) ->
    ?MSG_LIB:cre_RequestedEvent(Name).
cre_ReqEv(Name, EPL) ->
    ?MSG_LIB:cre_RequestedEvent(Name, EPL).
cre_ReqEv(Name, SID, RA, EPL) ->
    ?MSG_LIB:cre_RequestedEvent(Name, SID, RA, EPL).


cre_ObsEvsDesc(Id, EvList) ->
    ?MSG_LIB:cre_ObservedEventsDescriptor(Id, EvList).

cre_EvsDesc(Id, EvList) ->
    ?MSG_LIB:cre_EventsDescriptor(Id, EvList).


%% Service change related
cre_SvcChParm(M, A, R, P) ->
    ?MSG_LIB:cre_ServiceChangeParm(M, A, P, R).

cre_SvcChParm(M, A, R, P, IF) ->
    ?MSG_LIB:cre_ServiceChangeParm(M, A, asn1_NOVALUE, P, R, asn1_NOVALUE, 
			  asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE, IF).

cre_SvcChResParm(A, P) ->
    ?MSG_LIB:cre_ServiceChangeResParm(A, P).

cre_SvcChReq(Tids, P) ->
    ?MSG_LIB:cre_ServiceChangeRequest(Tids, P).

cre_SvcChProf(Name, Ver) ->
    ?MSG_LIB:cre_ServiceChangeProfile(Name, Ver).

cre_SvcChAddr(Tag, Val) ->
    ?MSG_LIB:cre_ServiceChangeAddress(Tag, Val).

cre_SvcChMethod(M) ->
    ?MSG_LIB:cre_ServiceChangeMethod(M).

cre_SvcChRep(Tids, Res) ->
    ?MSG_LIB:cre_ServiceChangeReply(Tids, Res).


%% Stream related
cre_StreamID(Id) ->
    ?MSG_LIB:cre_StreamID(Id).

cre_StreamParms(Lcd) ->
    ?MSG_LIB:cre_StreamParms(Lcd).
cre_StreamParms(Lcd, Ld) ->
    ?MSG_LIB:cre_StreamParms(Lcd, Ld).
cre_StreamParms(Lcd, Ld, Rd) ->
    ?MSG_LIB:cre_StreamParms(Lcd, Ld, Rd).
cre_StreamParmsL(Ld) ->
    ?MSG_LIB:cre_StreamParms(asn1_NOVALUE, Ld, asn1_NOVALUE).
cre_StreamParmsR(Rd) ->
    ?MSG_LIB:cre_StreamParms(asn1_NOVALUE, asn1_NOVALUE, Rd).

cre_StreamDesc(Id, P) ->
    ?MSG_LIB:cre_StreamDescriptor(Id, P).


%% "Local" related
cre_LocalControlDesc(Mode) ->
    ?MSG_LIB:cre_LocalControlDescriptor(Mode).
cre_LocalControlDesc(Mode, Parms) ->
    ?MSG_LIB:cre_LocalControlDescriptor(Mode, Parms).

cre_LocalRemoteDesc(Grps) ->
    ?MSG_LIB:cre_LocalRemoteDescriptor(Grps).


%% DigitMap related
cre_DigitMapDesc() ->
    ?MSG_LIB:cre_DigitMapDescriptor().
cre_DigitMapDesc(NameOrVal) ->
    ?MSG_LIB:cre_DigitMapDescriptor(NameOrVal).
cre_DigitMapDesc(Name, Val) ->
    ?MSG_LIB:cre_DigitMapDescriptor(Name, Val).

cre_DigitMapValue(Body) ->
    ?MSG_LIB:cre_DigitMapValue(Body).

cre_DigitMapValue(Body, Start, Short, Long) ->
    ?MSG_LIB:cre_DigitMapValue(Start, Short, Long, Body).

%% Media related
cre_MediaDesc(SD) when is_record(SD, 'StreamDescriptor') ->
    cre_MediaDesc([SD]);
cre_MediaDesc(SDs) ->
    ?MSG_LIB:cre_MediaDescriptor(SDs).


%% Notify related
cre_NotifyReq(Tids, EvsDesc) ->
    ?MSG_LIB:cre_NotifyRequest(Tids, EvsDesc).

cre_NotifyRep(Tids) ->
    ?MSG_LIB:cre_NotifyReply(Tids).


%% Subtract related
cre_SubReq(Tids, Desc) ->
    ?MSG_LIB:cre_SubtractRequest(Tids, Desc).


%% Audit related
cre_AuditDesc(Tokens) ->
    ?MSG_LIB:cre_AuditDescriptor(Tokens).

cre_AuditDesc(Tokens, PropertTokens) ->
    ?MSG_LIB:cre_AuditDescriptor(Tokens, PropertTokens).

cre_AuditReq(Tid, Desc) ->
    ?MSG_LIB:cre_AuditRequest(Tid, Desc).

cre_AuditRep(AR) ->
    ?MSG_LIB:cre_AuditReply(AR).

cre_AuditRes(Tid, Res) ->
    ?MSG_LIB:cre_AuditResult(Tid, Res).

cre_TermAudit(ARP) ->
    ?MSG_LIB:cre_TerminationAudit(ARP).

cre_AuditRetParam(D) ->
    ?MSG_LIB:cre_AuditReturnParameter(D).

cre_TermListAuditRes(TIDs, TA) ->
    ?MSG_LIB:cre_TermListAuditResult(TIDs, TA).

%% AMM/AMMS related
cre_AmmDesc(D) ->
    ?MSG_LIB:cre_AmmDescriptor(D).

cre_AmmReq(Tids, Descs) ->
    ?MSG_LIB:cre_AmmRequest(Tids, Descs).

cre_AmmsReply(Tids) ->
    ?MSG_LIB:cre_AmmsReply(Tids).
cre_AmmsReply(Tids, Descs) ->
    ?MSG_LIB:cre_AmmsReply(Tids, Descs).


%% Command related
cre_Cmd(Tag, Req) ->
    ?MSG_LIB:cre_Command(Tag, Req).

cre_CmdReq(Cmd) ->
    ?MSG_LIB:cre_CommandRequest(Cmd).

cre_CmdRep(Tag, Rep) ->
    ?MSG_LIB:cre_CommandReply(Tag, Rep).


%% Actions related
cre_ReqActs(A) ->
    ?MSG_LIB:cre_RequestedActions(A).

cre_ReqActs(KA, EDM, SE, SD, NB, RED) ->
    ?MSG_LIB:cre_RequestedActions(KA, EDM, SE, SD, NB, RED).

%% cre_SecReqActs() ->
%%     ?MSG_LIB:cre_SecondRequestedActions().

%% cre_SecReqActs(A) ->
%%     ?MSG_LIB:cre_SecondRequestedActions(A).

cre_SecReqActs(KA, EDM, SD, NB, RED) ->
    ?MSG_LIB:cre_SecondRequestedActions(KA, EDM, SD, NB, RED).

cre_EvDM(Name) when is_list(Name) ->
    ?MSG_LIB:cre_EventDM(Name).

cre_RegEmbedDesc() ->
    ?MSG_LIB:cre_RegulatedEmbeddedDescriptor().

cre_RegEmbedDesc(D) ->
    ?MSG_LIB:cre_RegulatedEmbeddedDescriptor(D).

cre_RegEmbedDesc(SED, SD) ->
    ?MSG_LIB:cre_RegulatedEmbeddedDescriptor(SED, SD).

%% cre_SecEvsDesc(REDs) ->
%%     ?MSG_LIB:cre_SecondEventsDescriptor(REDs).

cre_SecEvsDesc(RID, REDs) ->
    ?MSG_LIB:cre_SecondEventsDescriptor(RID, REDs).

cre_SecReqEv(N) ->
    cre_SecReqEv(N, []).

cre_SecReqEv(N, EPL) ->
    ?MSG_LIB:cre_SecondRequestedEvent(N, EPL).

cre_SecReqEv(N, SID, EA) when is_list(N) andalso 
			      is_integer(SID) andalso 
			      is_record(EA, 'SecondRequestedActions') ->
    cre_SecReqEv(N, SID, EA, []);
cre_SecReqEv(A, B, C) ->
    ?MSG_LIB:cre_SecondRequestedEvent(A, B, C).

cre_SecReqEv(N, SID, EA, EPL) ->
    ?MSG_LIB:cre_SecondRequestedEvent(N, SID, EA, EPL).

%% Signal related
cre_SigsDesc() ->
    cre_SigsDesc([]).

cre_SigsDesc(SRs) ->
    ?MSG_LIB:cre_SignalsDescriptor(SRs).

cre_SigDir(D) ->
    ?MSG_LIB:cre_SignalDirection(D).

cre_Sig(Name) ->
    cre_Sig(Name, []).

cre_Sig(Name, SPL) ->
    ?MSG_LIB:cre_Signal(Name, SPL).

cre_Sig(Name, Dir, RID) ->
    cre_Sig(Name, [], Dir, RID).

cre_Sig(Name, SPL, Dir, RID) ->
    cre_Sig(Name, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE, 
	    asn1_NOVALUE, SPL, Dir, RID).

cre_Sig(Name, SID, ST, Dur, NC, KA, SPL, Dir, RID) ->
    ?MSG_LIB:cre_Signal(Name, SID, ST, Dur, NC, KA, SPL, Dir, RID).

cre_Sig(Name, SID, ST, Dur, NC, KA, SPL, Dir, RID, ISD) ->
    ?MSG_LIB:cre_Signal(Name, SID, ST, Dur, NC, KA, SPL, Dir, RID, ISD).

cre_SigReq(S) ->
    ?MSG_LIB:cre_SignalRequest(S).

cre_NotifBehav(Tag, Val) ->
    ?MSG_LIB:cre_NotifyBehaviour(Tag, Val).

cre_NotifCompl(NC) ->
    ?MSG_LIB:cre_NotifyCompletion(NC).

cre_SigType(ST) -> 
   ?MSG_LIB:cre_SignalType(ST).


%% Others
cre_ErrCode(EC) ->
    ?MSG_LIB:cre_ErrorCode(EC).

cre_ErrDesc(EC) ->
    ?MSG_LIB:cre_ErrorDescriptor(EC).

cre_TermIDList(TIDs) ->
    ?MSG_LIB:cre_TerminationIDList(TIDs).

cre_ServiceState(SS) ->
    ?MSG_LIB:cre_ServiceState(SS).

cre_StreamMode(SS) ->
    ?MSG_LIB:cre_StreamMode(SS).

cre_SelectLogic(Tag) ->
    ?MSG_LIB:cre_SelectLogic(Tag).

cre_CtxID(CID) ->
    ?MSG_LIB:cre_ContextID(CID).

cre_ReqID(RID) ->
    ?MSG_LIB:cre_RequestID(RID).

cre_TimeNot(D,T) ->
    ?MSG_LIB:cre_TimeNotation(D, T).

cre_PkgsItem(Name, Ver) ->
    ?MSG_LIB:cre_PackagesItem(Name, Ver).

cre_BOOLEAN(B) ->
    ?MSG_LIB:cre_BOOLEAN(B).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flex_init(Config) ->
    megaco_codec_flex_lib:init(Config).

flex_finish(Config) ->
    megaco_codec_flex_lib:finish(Config).

flex_scanner_conf(Config) ->
    megaco_codec_flex_lib:scanner_conf(Config).

%% start_flex_scanner() ->
%%     megaco_codec_flex_lib:start().

%% stop_flex_scanner(Pid) ->
%%     megaco_codec_flex_lib:stop(Pid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t(F,A) ->
    p(printable(get(severity),trc),trc,F,A).

d(F,A) ->
    p(printable(get(severity),dbg),dbg,F,A).

%% l(F,A) ->
%%     p(printable(get(severity),log),log,F,A).

e(F,A) ->
    p(printable(get(severity),err),err,F,A).


printable(trc,_) ->
    true;
printable(dbg,trc) ->
    false;
printable(dbg,_) ->
    true;
printable(log,log) ->
    true;
printable(log,err) ->
    true;
printable(err,err) ->
    true;
printable(_,_) ->
    false.


p(true,L,F,A) ->
    io:format("~s:" ++ F ++ "~n", [image_of(L)|A]);
p(_,_,_,_) ->
    ok.

image_of(trc) ->
    "TRC";
image_of(dbg) ->
    "DBG";
image_of(log) ->
    "LOG";
image_of(err) ->
    "ERR";
image_of(L) ->
    io_lib:format("~p",[L]).

