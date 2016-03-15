%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
%% Purpose: Test encoding/decoding (codec) module of Megaco/H.248 v1
%%----------------------------------------------------------------------

-module(megaco_codec_v1_test).

%% ----

-compile({no_auto_import,[error/1]}).

%% ----

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include("megaco_test_lib.hrl").

%% ----

%% -export([msg/0]).
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
	 compact_otp4299_msg2/1, 
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
	 compact_otp5993_msg01/1, 
	 compact_otp5993_msg02/1, 
	 compact_otp5993_msg03/1, 
	 compact_otp6017_msg01/1, 
	 compact_otp6017_msg02/1, 
	 compact_otp6017_msg03/1, 
	 
         flex_compact_otp7431_msg01a/1,
         flex_compact_otp7431_msg01b/1,
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
         pretty_otp5600_msg1/1,
         pretty_otp5600_msg2/1,
         pretty_otp5601_msg1/1,
         pretty_otp5793_msg01/1,
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
	 
	 flex_pretty_otp5042_msg1/1, 
	 flex_pretty_otp5085_msg1/1, 
	 flex_pretty_otp5085_msg2/1, 
	 flex_pretty_otp5085_msg3/1, 
	 flex_pretty_otp5085_msg4/1, 
	 flex_pretty_otp5085_msg5/1, 
	 flex_pretty_otp5085_msg6/1, 
	 flex_pretty_otp5085_msg7/1, 
         flex_pretty_otp5600_msg1/1,
         flex_pretty_otp5600_msg2/1,
         flex_pretty_otp5601_msg1/1,
         flex_pretty_otp5793_msg01/1,
         flex_pretty_otp7431_msg01/1,
         flex_pretty_otp7431_msg02/1,
         flex_pretty_otp7431_msg03/1,
         flex_pretty_otp7431_msg04/1,
         flex_pretty_otp7431_msg05/1,
         flex_pretty_otp7431_msg06/1,
         flex_pretty_otp7431_msg07/1,

	 init_per_testcase/2, end_per_testcase/2]).  

-export([display_text_messages/0, generate_text_messages/0]).


-export([msg15b/0, msg22f/0]).

-export([
	 %% Decode
	 profile_decode_compact_text_message/1,
	 profile_decode_compact_text_messages/0,
	 prof1/0, prof2/0, 
	 profile_decode_compact_flex_text_message/1,
	 profile_decode_compact_flex_text_messages/0,
	 profile_decode_pretty_text_message/1,
	 profile_decode_pretty_text_messages/0, 
	 profile_decode_pretty_flex_text_message/1,
	 profile_decode_pretty_flex_text_messages/0,

	 %% Encode
	 profile_encode_compact_text_messages/0,
	 profile_encode_pretty_text_messages/0
	]).


%% ----

-define(V1,           v1).
-define(EC,           []).
-define(VERSION,      1).
-define(VERSION_STR,  "1").
-define(DEFAULT_PORT, 55555).
-define(MSG_LIB,      megaco_test_msg_v1_lib).
-define(MG1_MID_NO_PORT, {ip4Address,
                          #'IP4Address'{address = [124, 124, 124, 222]}}).
-define(MG1_MID, {ip4Address, #'IP4Address'{address = [124, 124, 124, 222],
                                            portNumber = ?DEFAULT_PORT}}).
-define(MG2_MID, {ip4Address, #'IP4Address'{address = [125, 125, 125, 111],
                                            portNumber = ?DEFAULT_PORT}}).
-define(MG3_MID, {ip6Address, #'IP6Address'{address = [0,0,0,0,
						       0,0,0,0,
						       0,0,0,0,
						       125, 125, 125, 111],
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
	msgs1() ++ 
	msgs3(),
    megaco_codec_test_lib:display_text_messages(?VERSION, Msgs).


generate_text_messages() ->
    Msgs = 
	msgs1(),
    megaco_codec_test_lib:generate_text_messages(?V1, ?VERSION, ?EC, Msgs).


%% ----

prof1() ->
    megaco_codec_v1_test:profile_decode_compact_text_message(msg10).

prof2() ->
    megaco_codec_v1_test:profile_decode_compact_flex_text_message(msg10).

%% (catch megaco_codec_v1_test:profile_decode_compact_text_message(msg01a)).
%% (catch megaco_codec_v1_test:profile_decode_compact_text_message(msg01b)).
%% (catch megaco_codec_v1_test:profile_decode_compact_text_message(msg02)).
%% (catch megaco_codec_v1_test:profile_decode_compact_text_message(msg10)).
%% (catch megaco_codec_v1_test:profile_decode_compact_text_message(msg11)).
%% (catch megaco_codec_v1_test:profile_decode_compact_text_message(msg12)).
%% (catch megaco_codec_v1_test:profile_decode_compact_text_message(msg13)).
profile_decode_compact_text_message(MsgTag) when is_atom(MsgTag) ->
    Config = [],
    Slogan = list_to_atom("decode_compact_v1_" ++ atom_to_list(MsgTag)),
    profile_decode_compact_text_message(Slogan, Config, MsgTag).

profile_decode_compact_flex_text_message(MsgTag) when is_atom(MsgTag) ->
    Conf   = flex_init([]),
    Config = flex_scanner_conf(Conf),
    Slogan = list_to_atom("decode_compact_flex_v1_" ++  atom_to_list(MsgTag)),
    Res    = profile_decode_compact_text_message(Slogan, [Config], MsgTag),
    flex_finish(Conf),
    Res.

profile_decode_compact_text_message(Slogan, Config, MsgTag) ->
    Codec = megaco_compact_text_encoder,
    profile_decode_text_message(Slogan, Codec, Config, MsgTag).
    
%% (catch megaco_codec_v1_test:profile_decode_pretty_text_message(msg01a)).
%% (catch megaco_codec_v1_test:profile_decode_pretty_text_message(msg01b)).
%% (catch megaco_codec_v1_test:profile_decode_pretty_text_message(msg02)).
profile_decode_pretty_text_message(MsgTag) when is_atom(MsgTag) ->
    Config = [],
    Slogan = list_to_atom("decode_pretty_v1_" ++ atom_to_list(MsgTag)),
    profile_decode_pretty_text_message(Slogan, Config, MsgTag).

profile_decode_pretty_flex_text_message(MsgTag) when is_atom(MsgTag) ->
    Conf   = flex_init([]),
    Config = flex_scanner_conf(Conf),
    Slogan = list_to_atom("decode_pretty_flex_v1_" ++  atom_to_list(MsgTag)),
    Res    = profile_decode_pretty_text_message(Slogan, [Config], MsgTag),
    flex_finish(Conf),
    Res.

profile_decode_pretty_text_message(Slogan, Config, MsgTag) ->
    Codec = megaco_pretty_text_encoder,
    profile_decode_text_message(Slogan, Codec, Config, MsgTag).

profile_decode_text_message(Slogan, Codec, Config, MsgTag) ->
    Msgs = msgs1(), 
    case lists:keysearch(MsgTag, 1, Msgs) of
	{value, Msg} ->
	    [Res] = profile_decode_text_messages(Slogan, Codec, Config, [Msg]),
	    Res;
	false ->
	    {error, {no_such_message, MsgTag}}
    end.


%% (catch megaco_codec_v1_test:profile_decode_compact_text_messages()).
profile_decode_compact_text_messages() ->
    Config = [],
    Slogan = decode_compact_v1, 
    profile_decode_compact_text_messages(Slogan, Config).

%% (catch megaco_codec_v1_test:profile_decode_compact_flex_text_messages()).
profile_decode_compact_flex_text_messages() ->
    Conf   = flex_init([]),
    Config = flex_scanner_conf(Conf),
    Slogan = decode_compact_flex_v1, 
    Res = profile_decode_compact_text_messages(Slogan, [Config]),
    flex_finish(Conf),
    Res.

profile_decode_compact_text_messages(Slogan, Config) ->
    Codec = megaco_compact_text_encoder,
    profile_decode_text_messages(Slogan, Codec, Config).

%% (catch megaco_codec_v1_test:profile_decode_pretty_text_messages()).
profile_decode_pretty_text_messages() ->
    Config = [],
    Slogan = decode_pretty_v1, 
    profile_decode_pretty_text_messages(Slogan, Config).

%% (catch megaco_codec_v1_test:profile_decode_pretty_flex_text_messages()).
profile_decode_pretty_flex_text_messages() ->
    Conf   = flex_init([]),
    Config = flex_scanner_conf(Conf),
    Slogan = decode_pretty_flex_v1, 
    Res    = profile_decode_pretty_text_messages(Slogan, [Config]),
    flex_finish(Conf),
    Res.


profile_decode_pretty_text_messages(Slogan, Config) ->
    Codec = megaco_pretty_text_encoder,
    profile_decode_text_messages(Slogan, Codec, Config).

profile_decode_text_messages(Slogan, Codec, Config) ->
    Msgs = msgs1(),
    profile_decode_text_messages(Slogan, Codec, Config, Msgs).

profile_decode_text_messages(Slogan, Codec, Config, Msgs0) ->
    Msgs      = [Msg || {_, Msg, _, _} <- Msgs0],
    EncodeRes = encode_text_messages(Codec, Config, Msgs, []),
    Bins      = [Bin || {ok, Bin} <- EncodeRes],
    Fun = fun() ->
		decode_text_messages(Codec, Config, Bins, [])
	  end,
    %% Make a dry run, just to make sure all modules are loaded:
    io:format("make a dry run...~n", []),
    (catch Fun()),
    io:format("make the run...~n", []),
    megaco_profile:profile(Slogan, Fun).

%% (catch megaco_codec_v1_test:profile_encode_compact_text_messages()).
profile_encode_compact_text_messages() ->
    Codec  = megaco_compact_text_encoder,
    Config = [],
    Slogan = encode_compact_v1, 
    profile_encode_text_messages(Slogan, Codec, Config).

%% (catch megaco_codec_v1_test:profile_encode_pretty_text_messages()).
profile_encode_pretty_text_messages() ->
    Codec  = megaco_pretty_text_encoder,
    Config = [],
    Slogan = encode_pretty_v1, 
    profile_encode_text_messages(Slogan, Codec, Config).

profile_encode_text_messages(Slogan, Codec, Config) ->
    Msgs = msgs1(),
    profile_encode_text_messages(Slogan, Codec, Config, Msgs).

profile_encode_text_messages(Slogan, Codec, Config, Msgs0) ->
    Msgs = [Msg || {_, Msg, _, _} <- Msgs0],
    Fun = fun() ->
		encode_text_messages(Codec, Config, Msgs, [])
	  end,
    %% Make a dry run, just to make sure all modules are loaded:
    io:format("make a dry run...~n", []),
    (catch Fun()),
    io:format("make the run...~n", []),
    megaco_profile:profile(Slogan, Fun).

encode_text_messages(_Codec, _Config, [], Acc) ->
    Acc;
encode_text_messages(Codec, Config, [Msg|Msgs], Acc) ->
    Res = Codec:encode_message(Config, ?VERSION, Msg),
    encode_text_messages(Codec, Config, Msgs, [Res | Acc]).

decode_text_messages(_Codec, _Config, [], Acc) ->
    Acc;
decode_text_messages(Codec, Config, [Msg|Msgs], Acc) ->
    Res = Codec:decode_message(Config, dynamic, Msg),
    decode_text_messages(Codec, Config, Msgs, [Res | Acc]).


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
    [{text,            [], [{group, pretty}, 
			    {group, flex_pretty},
			    {group, compact}, 
			    {group, flex_compact}]},
     {binary,          [], [{group, bin}, 
			    {group, ber}, 
			    {group, per}]},
     {erl_dist,        [], [{group, erl_dist_m}]},
     {pretty,          [], [pretty_test_msgs]},
     {compact,         [], [compact_test_msgs]},
     {flex_pretty,     [], flex_pretty_cases()},
     {flex_compact,    [], flex_compact_cases()},
     {bin,             [], [bin_test_msgs]}, 
     {ber,             [], [ber_test_msgs]},
     {per,             [], [per_test_msgs]},
     {erl_dist_m,      [], [erl_dist_m_test_msgs]},
     {tickets,         [], [{group, compact_tickets}, 
			    {group, pretty_tickets},
			    {group, flex_compact_tickets},
			    {group, flex_pretty_tickets}]},
     {compact_tickets, [], [compact_otp4011_msg1, 
			    compact_otp4011_msg2,
			    compact_otp4011_msg3, 
			    compact_otp4013_msg1,
			    compact_otp4085_msg1, 
			    compact_otp4085_msg2,
			    compact_otp4280_msg1, 
			    compact_otp4299_msg1,
			    compact_otp4299_msg2, 
			    compact_otp4359_msg1,
			    compact_otp4920_msg0, 
			    compact_otp4920_msg1,
			    compact_otp4920_msg2, 
			    compact_otp4920_msg3,
			    compact_otp4920_msg4, 
			    compact_otp4920_msg5,
			    compact_otp4920_msg6, 
			    compact_otp4920_msg7,
			    compact_otp4920_msg8, 
			    compact_otp4920_msg9,
			    compact_otp4920_msg10, 
			    compact_otp4920_msg11,
			    compact_otp4920_msg12, 
			    compact_otp4920_msg20,
			    compact_otp4920_msg21, 
			    compact_otp4920_msg22,
			    compact_otp4920_msg23, 
			    compact_otp4920_msg24,
			    compact_otp4920_msg25, 
			    compact_otp5186_msg01,
			    compact_otp5186_msg02, 
			    compact_otp5186_msg03,
			    compact_otp5186_msg04, 
			    compact_otp5186_msg05,
			    compact_otp5186_msg06, 
			    compact_otp5793_msg01,
			    compact_otp5993_msg01, 
			    compact_otp5993_msg02,
			    compact_otp5993_msg03, 
			    compact_otp6017_msg01,
			    compact_otp6017_msg02, 
			    compact_otp6017_msg03]},
     {flex_compact_tickets, [], flex_compact_tickets_cases()},
     {pretty_tickets,       [], [pretty_otp4632_msg1, 
				 pretty_otp4632_msg2,
				 pretty_otp4632_msg3, 
				 pretty_otp4632_msg4,
				 pretty_otp4710_msg1, 
				 pretty_otp4710_msg2,
				 pretty_otp4945_msg1, 
				 pretty_otp4945_msg2,
				 pretty_otp4945_msg3, 
				 pretty_otp4945_msg4,
				 pretty_otp4945_msg5, 
				 pretty_otp4945_msg6,
				 pretty_otp4949_msg1, 
				 pretty_otp4949_msg2,
				 pretty_otp4949_msg3, 
				 pretty_otp5042_msg1,
				 pretty_otp5068_msg1, 
				 pretty_otp5085_msg1,
				 pretty_otp5085_msg2, 
				 pretty_otp5085_msg3,
				 pretty_otp5085_msg4, 
				 pretty_otp5085_msg5,
				 pretty_otp5085_msg6, 
				 pretty_otp5085_msg7,
				 pretty_otp5600_msg1, 
				 pretty_otp5600_msg2,
				 pretty_otp5601_msg1, 
				 pretty_otp5793_msg01,
				 pretty_otp5882_msg01, 
				 pretty_otp6490_msg01,
				 pretty_otp6490_msg02, 
				 pretty_otp6490_msg03,
				 pretty_otp6490_msg04, 
				 pretty_otp6490_msg05,
				 pretty_otp6490_msg06, 
				 pretty_otp7671_msg01,
				 pretty_otp7671_msg02, 
				 pretty_otp7671_msg03,
				 pretty_otp7671_msg04, 
				 pretty_otp7671_msg05]},
     {flex_pretty_tickets,  [], flex_pretty_tickets_cases()}].

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
     flex_compact_dm_timers6
    ].

flex_compact_tickets_cases() -> 
    [
     flex_compact_otp7431_msg01a,
     flex_compact_otp7431_msg01b, 
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
     flex_pretty_otp5600_msg1, 
     flex_pretty_otp5600_msg2,
     flex_pretty_otp5601_msg1, 
     flex_pretty_otp5793_msg01,
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
    Msgs = msgs1() ++ msgs2() ++ msgs3(),
    DynamicDecode = false,
    test_msgs(megaco_pretty_text_encoder, DynamicDecode, [], Msgs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flex_pretty_init(Config) ->
    flex_init(Config).
    
flex_pretty_finish(Config) ->
    flex_finish(Config).
    

flex_pretty_test_msgs(suite) ->
    [];
flex_pretty_test_msgs(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    %% Msgs = msgs1(), 
    Msgs = msgs1() ++ msgs2() ++ msgs3(),
    Conf = flex_scanner_conf(Config),
    DynamicDecode = false,
    test_msgs(megaco_pretty_text_encoder, DynamicDecode, [Conf], Msgs).

flex_pretty_otp5042_msg1(suite) ->
    [];
flex_pretty_otp5042_msg1(Config) when is_list(Config) ->
    d("flex_pretty_otp5042_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg0 = pretty_otp5042_msg1(),
    Bin0 = list_to_binary(Msg0),
    Conf = flex_scanner_conf(Config),
    case decode_message(megaco_pretty_text_encoder, false, [Conf], Bin0) of
	{error, [{reason, Reason}|_]} ->
	    case Reason of
		{_, _Mod, {not_an_integer, PropertyParm}} ->
		    exit({not_an_integer, PropertyParm});
		_ ->
		    io:format("flex_pretty_otp5042_msg1 -> "
			      "~n   Reason: ~w"
			      "~n", [Reason]),
		    exit({unexpected_decode_result, Reason})
	    end;
	{ok, M} ->
	    t("flex_pretty_otp5042_msg1 -> successfull decode:"
	      "~n~p", [M]),
	    ok
    end.


flex_pretty_otp5085_msg1(suite) ->
    [];
flex_pretty_otp5085_msg1(Config) when is_list(Config) ->
    d("flex_pretty_otp5085_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    pretty_otp5085(ok, pretty_otp5085_msg1(), [Conf]).

flex_pretty_otp5085_msg2(suite) ->
    [];
flex_pretty_otp5085_msg2(Config) when is_list(Config) ->
    d("flex_pretty_otp5085_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    pretty_otp5085(error, pretty_otp5085_msg2(), [Conf]).

flex_pretty_otp5085_msg3(suite) ->
    [];
flex_pretty_otp5085_msg3(Config) when is_list(Config) ->
    d("flex_pretty_otp5085_msg3 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    pretty_otp5085(ok, pretty_otp5085_msg3(), [Conf]).

flex_pretty_otp5085_msg4(suite) ->
    [];
flex_pretty_otp5085_msg4(Config) when is_list(Config) ->
    d("flex_pretty_otp5085_msg4 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    pretty_otp5085(ok, pretty_otp5085_msg4(), [Conf]).

flex_pretty_otp5085_msg5(suite) ->
    [];
flex_pretty_otp5085_msg5(Config) when is_list(Config) ->
    d("flex_pretty_otp5085_msg5 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    pretty_otp5085(ok, pretty_otp5085_msg5(), [Conf]).

flex_pretty_otp5085_msg6(suite) ->
    [];
flex_pretty_otp5085_msg6(Config) when is_list(Config) ->
    d("flex_pretty_otp5085_msg6 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    pretty_otp5085(ok, pretty_otp5085_msg6(), [Conf]).

flex_pretty_otp5085_msg7(suite) ->
    [];
flex_pretty_otp5085_msg7(Config) when is_list(Config) ->
    d("flex_pretty_otp5085_msg7 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    pretty_otp5085(ok, pretty_otp5085_msg7(), [Conf]).

flex_pretty_otp5600_msg1(suite) ->
    [];
flex_pretty_otp5600_msg1(Config) when is_list(Config) ->
    d("flex_pretty_otp5600_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    pretty_otp5600(ok, pretty_otp5600_msg1(), [Conf]).

flex_pretty_otp5600_msg2(suite) ->
    [];
flex_pretty_otp5600_msg2(Config) when is_list(Config) ->
    d("flex_pretty_otp5600_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    pretty_otp5600(ok, pretty_otp5600_msg2(), [Conf]).
 
flex_pretty_otp5601_msg1(suite) ->
    [];
flex_pretty_otp5601_msg1(Config) when is_list(Config) ->
    d("flex_pretty_otp5601_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    pretty_otp5601(ok, pretty_otp5601_msg1(), [Conf]).

flex_pretty_otp5793_msg01(suite) ->
    [];
flex_pretty_otp5793_msg01(Config) when is_list(Config) ->
    d("flex_pretty_otp5793_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    pretty_otp5793(ok, pretty_otp5793_msg1(), [Conf]).

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
    flex_pretty_otp7431(error, flex_pretty_otp7431_msg2(), [Conf]).

flex_pretty_otp7431_msg03(suite) ->
    [];
flex_pretty_otp7431_msg03(Config) when is_list(Config) ->
%%     put(severity,trc),
%%     put(dbg,true),
    d("flex_pretty_otp7431_msg03 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    flex_pretty_otp7431(error, flex_pretty_otp7431_msg3(), [Conf]).

flex_pretty_otp7431_msg04(suite) ->
    [];
flex_pretty_otp7431_msg04(Config) when is_list(Config) ->
    d("flex_pretty_otp7431_msg04 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    flex_pretty_otp7431(error, flex_pretty_otp7431_msg4(), [Conf]).

flex_pretty_otp7431_msg05(suite) ->
    [];
flex_pretty_otp7431_msg05(Config) when is_list(Config) ->
    d("flex_pretty_otp7431_msg05 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    flex_pretty_otp7431(error, flex_pretty_otp7431_msg5(), [Conf]).

flex_pretty_otp7431_msg06(suite) ->
    [];
flex_pretty_otp7431_msg06(Config) when is_list(Config) ->
    d("flex_pretty_otp7431_msg06 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    flex_pretty_otp7431(error, flex_pretty_otp7431_msg6(), [Conf]).

flex_pretty_otp7431_msg07(suite) ->
    [];
flex_pretty_otp7431_msg07(Config) when is_list(Config) ->
    d("flex_pretty_otp7431_msg07 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    flex_pretty_otp7431(error, flex_pretty_otp7431_msg7(), [Conf]).

flex_pretty_otp7431(Expected, Msg, Conf) ->
    otp7431(Expected, megaco_pretty_text_encoder, Msg, Conf).

otp7431(Expected, Codec, Msg0, Conf) ->
    Bin0 = list_to_binary(Msg0),
    case decode_message(Codec, false, Conf, Bin0) of
	{ok, _Msg1} when Expected =:= ok ->
 	    io:format(" decoded", []),
	    ok;
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

compact_test_msgs(suite) ->
    [];
compact_test_msgs(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Msgs = msgs1() ++ msgs2() ++ msgs3(),
    DynamicDecode = false,
    test_msgs(megaco_compact_text_encoder, DynamicDecode, [], Msgs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flex_compact_init(Config) ->
    flex_init(Config).
    

flex_compact_finish(Config) ->
    flex_finish(Config).
    

flex_compact_test_msgs(suite) ->
    [];
flex_compact_test_msgs(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Msgs = msgs1() ++ msgs2() ++ msgs3(),
    %% Msgs = msgs1(), 
    Conf = flex_scanner_conf(Config), 
    DynamicDecode = true,
    test_msgs(megaco_compact_text_encoder, DynamicDecode, [Conf], Msgs).


flex_compact_dm_timers1(suite) ->
    [];
flex_compact_dm_timers1(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    M = build_dm_timers_message("1", "2", "3"),
    B = list_to_binary(M),
    Conf = flex_scanner_conf(Config), 
    case decode_message(megaco_compact_text_encoder, false, [Conf], B) of
	{ok, M1} when is_record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers1 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers(1,2,3, M1);
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
    case decode_message(megaco_compact_text_encoder, false, [Conf], B) of
	{ok, M1} when is_record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers2 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers(2,3,4, M1);
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
    case decode_message(megaco_compact_text_encoder, false, [Conf], B) of
	{ok, M1} when is_record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers3 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers(1,2,31, M1);
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
    case decode_message(megaco_compact_text_encoder, false, [Conf], B) of
	{ok, M1} when is_record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers4 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers(10,21,99, M1);
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
    case decode_message(megaco_compact_text_encoder, false, [Conf], B) of
	{ok, M1} when is_record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers5 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers(99,23,11, M1);
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
    case decode_message(megaco_compact_text_encoder, false, [Conf], B) of
	{ok, M1} when is_record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers6 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers(77,9,1, M1);
	Else ->
	    exit({decode_failed, M, Else})
    end.


build_dm_timers_message(T, S, L) ->
    M = io_lib:format("!/" ?VERSION_STR " [123.123.123.4]:55555\nT=10001{C=-{MF=11111111/00000000/00000000{E=2223{al/on,dd/ce{DM=dialplan00}},SG{cg/rt},DM=dialplan00{T:~s,S:~s,L:~s,(0s| 00s|[1-7]xlxx|8lxxxxxxx|#xxxxxxx|*xx|9l1xxxxxxxxxx|9l011x.s)}}}}", [T, S, L]),
    lists:flatten(M).


verify_dm_timers(T,S,L, #'MegacoMessage'{mess = Mess}) ->
    #'Message'{messageBody = Body} = Mess,
    case get_dm_timers(Body) of
	{T, S, L} ->
	    ok;
	{T1, S1, L1} ->
	    exit({invalid_timer_values, {{T, S, L}, {T1, S1, L1}}});
	{error, Reason} ->
	    exit({invalid_timer, {T, S, L, Reason}})
    end.

get_dm_timers({transactions, T}) when is_list(T) ->
    get_dm_timers1(T);
get_dm_timers(Other) ->
    {error, {invalid_transactions, Other}}.

get_dm_timers1([{transactionRequest,T}|Ts]) when is_record(T,'TransactionRequest') ->
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
	#'DigitMapValue'{startTimer   = T,
			 shortTimer   = S,
			 longTimer    = L} ->
	    {ok, {T, S, L}};
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
    Msgs = msgs1(),
    DynamicDecode = false,
    test_msgs(megaco_binary_encoder, DynamicDecode, [], Msgs).

   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ber_test_msgs(suite) ->
    [];
ber_test_msgs(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Msgs = msgs1(),
    DynamicDecode = false,
    test_msgs(megaco_ber_encoder, DynamicDecode, [], Msgs).

   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

per_test_msgs(suite) ->
    [];
per_test_msgs(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Msgs = msgs1(),
    DynamicDecode = false,
    test_msgs(megaco_per_encoder, DynamicDecode, [], Msgs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

erl_dist_m_test_msgs(suite) ->
    [];
erl_dist_m_test_msgs(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Msgs = msgs1() ++ msgs2() ++ msgs3(),
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
%     put(severity,trc),
%     put(dbg,true),
    d("compact_otp4011_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M = "!/" ?VERSION_STR
	" ML T=233350{C=${A=stedevice/01{M{O{MO=SR,RV=OFF,RG=OFF,tdmc/ec=OFF,MO=SR}}}}}",
    ok = compact_otp4011(M).


%% --------------------------------------------------------------
%% Observe that this decode SHALL fail
compact_otp4011_msg2(suite) ->
    [];
compact_otp4011_msg2(Config) when is_list(Config) ->
    d("compact_otp4011_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M = "!/" ?VERSION_STR
	" ML T=233350{C=${A=stedevice/01{M{O{MO=SO,RV=OFF,RG=OFF,tdmc/ec=OFF,MO=SR}}}}}",
%     put(severity,trc),
%     put(dbg,true),
    ok = compact_otp4011(M).


%% --------------------------------------------------------------
%% Observe that this decode SHALL fail
compact_otp4011_msg3(suite) ->
    [];
compact_otp4011_msg3(Config) when is_list(Config) ->
    d("compact_otp4011_msg3 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M = "!/" ?VERSION_STR
	" ML T=233350{C=${A=stedevice/01{M{O{MO=SR,RV=OFF,RG=OFF,tdmc/ec=OFF,MO=SO}}}}}",
    %%     put(severity,trc),
    %%     put(dbg,true),
    ok = compact_otp4011(M).


compact_otp4011(M) ->
    d("compact_otp4011 -> entry with"
      "~n   M: '~s'", [M]),
    Bin = list_to_binary(M),
    case decode_message(megaco_compact_text_encoder, false, [], Bin) of
	{ok, _} ->
	    exit({decoded_erroneous_message,M});
	{error, Error} when is_list(Error) -> % Expected result
	    d("compact_otp4011 -> expected error result (so far)", []),
	    case lists:keysearch(reason,1,Error) of
		{value, {reason,Reason}} ->
		    d("compact_otp4011 -> expected error: "
		      "~n   Reason: ~p", [Reason]),
		    case Reason of
			{0, megaco_text_parser_v1, 
			 {do_merge_control_streamParms, [A,B]}} 
			when is_list(A) andalso is_record(B, 'LocalControlDescriptor') ->
			    case lists:keysearch(mode,1,A) of
				{value, {mode, _Mode}} 
				when B#'LocalControlDescriptor'.streamMode =/= asn1_NOVALUE ->
				    d("compact_otp4011 -> expected error",[]),
				    ok;
				Other ->
				    exit({unexpected_mode_reason, {A,B,Other}})
			    end;
			Other ->
			    exit({unexpected_reason, Other})
		    end;

		false ->
		    d("compact_otp4011 -> OUPS, wrong kind of error", []),
		    exit({unexpected_result, Error})
	    end;
	Else ->
	    d("compact_otp4011 -> unexpected decode result: ~p", [Else]),
	    exit({unexpected_decode_result, Else})
    end.
    
%% --------------------------------------------------------------
%% Note that this decode SHALL fail, because of the misspelled
%% MEGCAO instead of the correct MEGACO.
compact_otp4013_msg1(suite) ->
    [];
compact_otp4013_msg1(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("compact_otp4013_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M = "MEGCAO/1 MG1 T=12345678{C=-{SC=root{SV{MT=RS,RE=901}}}}",
    Bin = list_to_binary(M),
    case decode_message(megaco_compact_text_encoder, false, [], Bin) of
	{ok, _} ->
	    exit({decoded_erroneous_message,M});
	{error, Reason} when is_list(Reason) ->
	    {value, {reason, no_version_found, _}} = 
		lists:keysearch(reason, 1, Reason),
	    {value, {token, [{'SafeChars',_,"megcao/1"}|_]}} = 
		lists:keysearch(token, 1, Reason),
	    ok;
	Else ->
	    exit({unexpected_decode_result,Else})
    end.
	    


%% --------------------------------------------------------------
%% 
%% 
compact_otp4085_msg1(suite) ->
    [];
compact_otp4085_msg1(Config) when is_list(Config) ->
    d("compact_otp4085_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M = compact_otp4085_erroneous_msg(),
    Bin = list_to_binary(M),
    case decode_message(megaco_compact_text_encoder, false, [], Bin) of
	{ok, M} ->
	    exit({decoded_erroneous_message,M});
	{error, Error} when is_list(Error) -> % Expected result
	    t("compact_otp4085_msg1 -> decode failed", []),
	    case lists:keysearch(reason, 1, Error) of
		{value, {reason,{999999, Module, Crap}}} ->
		    t("compact_otp4085_msg1 -> THE ACTUAL ERROR: "
		      "~n   LINE NUMBER: 999999"
		      "~n   Module: ~p"
		      "~n   Crap:   ~p", [Module, Crap]),
		    %% ok;
		    exit({decode_failed_999999, Module, Crap});
		{value, {reason,{Line, Module, Crap}}} ->
		    t("compact_otp4085_msg1 -> Expected: "
		      "~n   Line:   ~p"
		      "~n   Module: ~p"
		      "~n   Crap:   ~p", [Line, Module, Crap]),
		    ok;
		false ->
		    exit({unexpected_result, Error})
	    end;
	Else ->
	    exit({unexpected_decode_result, Else})
    end.


%% --------------------------------------------------------------
%% This test case is just to show that the message used in
%% compact_otp4085_msg1 is actually ok when you add '}' at the end.
compact_otp4085_msg2(suite) ->
    [];
compact_otp4085_msg2(Config) when is_list(Config) ->
    d("compact_otp4085_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M1 = compact_otp4085_erroneous_msg() ++ "}",
    Bin = list_to_binary(M1),
    case decode_message(megaco_compact_text_encoder, false, [], Bin) of
	{ok, M2} ->
	    l("compact_otp4085_msg1 -> successfull decode"
	      "~n   M2: ~p", [M2]),
	    ok;
	Else ->
	    e("compact_otp4085_msg1 -> decode error"
	      "~n   Else: ~p", [Else]),
	    exit({unexpected_decode_result,Else})
    end.


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
    d("compact_otp4280_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Bin = list_to_binary(compact_otp4280_msg()),
    case decode_message(megaco_compact_text_encoder, false, [], Bin) of
	{ok, _Msg} ->
	    ok;
	{error, Error} when is_list(Error) -> 
	    t("compact_otp4280_msg1 -> decode failed", []),
	    case lists:keysearch(reason, 1, Error) of
		{value, {reason,{Line, Module, Reason} = R}} ->
		    t("compact_otp4280_msg1 -> "
		      "~n   Line:   ~w"
		      "~n   Module: ~w"
		      "~n   Reason: ~w", [Line, Module, Reason]),
		    exit({decode_failed, R});
		false ->
		    exit({unexpected_result, Error})
	    end;
	Else ->
	    exit({unexpected_decode_result, Else})
    end.

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
compact_otp4299_msg1(suite) ->
    [];
compact_otp4299_msg1(Config) when is_list(Config) ->
    d("compact_otp4299_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Bin = list_to_binary(compact_otp4299_msg()),
    case decode_message(megaco_compact_text_encoder, false, [], Bin) of
	{ok, _Msg} ->
	    ok;

	{error, Reason} ->
	    exit({decode_error, Reason});

	Else ->
	    exit({unexpected_decode_result, Else})
    end.


%% Same message, but this time decoded using the flex scanner
compact_otp4299_msg2(suite) ->
    [];
compact_otp4299_msg2(Config) when is_list(Config) ->
    d("compact_otp4299_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),

    {Pid, Conf} = compact_otp4299_msg2_init(),
    
    Bin = list_to_binary(compact_otp4299_msg()),
    Res = decode_message(megaco_compact_text_encoder, false, [Conf], Bin),
    compact_otp4299_msg2_finish(Pid),

    case Res of
	{ok, _Msg} ->
	    ok;

	{error, Reason} ->
	    exit({decode_error, Reason});

	Else ->
	    exit({unexpected_decode_result, Else})
    end.


compact_otp4299_msg2_init() ->
    Flag = process_flag(trap_exit, true),
    Res = (catch start_flex_scanner()),
    process_flag(trap_exit, Flag),
    case Res of
	{error, Reason} ->
	    skip(Reason);
	{ok, FlexConfig} ->
	    FlexConfig
    end.

compact_otp4299_msg2_finish(Pid) ->
    stop_flex_scanner(Pid).
    
    
compact_otp4299_msg() ->
    M = ";KALLE\n"
	"!/" ?VERSION_STR " mg58_1 P=005197711{; YET ANOTHER COMMENT\n"
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
%% 
compact_otp4359_msg1(suite) ->
    [];
compact_otp4359_msg1(Config) when is_list(Config) ->
    d("compact_otp4359_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Bin = list_to_binary(compact_otp4359_msg()),
    case decode_message(megaco_compact_text_encoder, false, [], Bin) of
	{ok, #'MegacoMessage'{mess = Mess}} ->
	    {transactions, Trans} = Mess#'Message'.messageBody,
	    case Trans of
		[{transactionRequest,#'TransactionRequest'{transactionId = asn1_NOVALUE}}] ->
		    ok;
		_ ->
		    exit({unexpected_transactions, Trans})
	    end;
	Else ->
	    t("compact_otp4359_msg1 -> "
	      "~n   Else: ~w", [Else]),
	    exit({unexpected_decode_result, Else})
    end.

compact_otp4359_msg() ->
    M = "!/" ?VERSION_STR " ml2 T={C=${A=${M{O {MO=SR,RG=OFF,RV=OFF}}}}}",
    M.


%% --------------------------------------------------------------
%% 
%% 
compact_otp4920_msg0(suite) ->
    [];
compact_otp4920_msg0(Config) when is_list(Config) ->
    d("compact_otp4920_msg0 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
%    put(dbg,true),
    compact_otp4920_msg_1(compact_otp4920_msg0(), true).

compact_otp4920_msg1(suite) ->
    [];
compact_otp4920_msg1(Config) when is_list(Config) ->
    d("compact_otp4920_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
%     put(dbg,true),
    compact_otp4920_msg_1(compact_otp4920_msg1(), false).

compact_otp4920_msg2(suite) ->
    [];
compact_otp4920_msg2(Config) when is_list(Config) ->
    d("compact_otp4920_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp4920_msg_1(compact_otp4920_msg2(), false).

compact_otp4920_msg3(suite) ->
    [];
compact_otp4920_msg3(Config) when is_list(Config) ->
    d("compact_otp4920_msg3 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp4920_msg_1(compact_otp4920_msg3(), true).

compact_otp4920_msg4(suite) ->
    [];
compact_otp4920_msg4(Config) when is_list(Config) ->
    d("compact_otp4920_msg4 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp4920_msg_1(compact_otp4920_msg4(), true).

compact_otp4920_msg5(suite) ->
    [];
compact_otp4920_msg5(Config) when is_list(Config) ->
    d("compact_otp4920_msg5 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp4920_msg_1(compact_otp4920_msg5(), true).

compact_otp4920_msg6(suite) ->
    [];
compact_otp4920_msg6(Config) when is_list(Config) ->
    d("compact_otp4920_msg6 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp4920_msg_1(compact_otp4920_msg6(), true).

compact_otp4920_msg7(suite) ->
    [];
compact_otp4920_msg7(Config) when is_list(Config) ->
    d("compact_otp4920_msg7 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
%    put(dbg,true),
    compact_otp4920_msg_1(compact_otp4920_msg7(), true).

compact_otp4920_msg8(suite) ->
    [];
compact_otp4920_msg8(Config) when is_list(Config) ->
    d("compact_otp4920_msg8 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
%    put(dbg,true),
    compact_otp4920_msg_1(compact_otp4920_msg8(), false).

compact_otp4920_msg9(suite) ->
    [];
compact_otp4920_msg9(Config) when is_list(Config) ->
    d("compact_otp4920_msg9 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp4920_msg_1(compact_otp4920_msg9(), false).

compact_otp4920_msg10(suite) ->
    [];
compact_otp4920_msg10(Config) when is_list(Config) ->
    d("compact_otp4920_msg10 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp4920_msg_1(compact_otp4920_msg10(), false).

compact_otp4920_msg11(suite) ->
    [];
compact_otp4920_msg11(Config) when is_list(Config) ->
    d("compact_otp4920_msg11 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp4920_msg_1(compact_otp4920_msg11(), false).

compact_otp4920_msg12(suite) ->
    [];
compact_otp4920_msg12(Config) when is_list(Config) ->
    d("compact_otp4920_msg12 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp4920_msg_1(compact_otp4920_msg12(), true).

%% Duplicate padding
compact_otp4920_msg20(suite) ->
    [];
compact_otp4920_msg20(Config) when is_list(Config) ->
    d("compact_otp4920_msg20 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp4920_msg_2(compact_otp4920_msg20(), bad_mid_duplicate_padding).

%% Length
compact_otp4920_msg21(suite) ->
    [];
compact_otp4920_msg21(Config) when is_list(Config) ->
    d("compact_otp4920_msg21 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp4920_msg_2(compact_otp4920_msg21(), bad_mid_ip6addr_length).

%% Length
compact_otp4920_msg22(suite) ->
    [];
compact_otp4920_msg22(Config) when is_list(Config) ->
    d("compact_otp4920_msg22 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp4920_msg_2(compact_otp4920_msg22(), bad_mid_ip6addr_length).

%% Length
compact_otp4920_msg23(suite) ->
    [];
compact_otp4920_msg23(Config) when is_list(Config) ->
    d("compact_otp4920_msg23 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp4920_msg_2(compact_otp4920_msg23(), bad_mid_ip6addr_length).

%% Length
compact_otp4920_msg24(suite) ->
    [];
compact_otp4920_msg24(Config) when is_list(Config) ->
    d("compact_otp4920_msg24 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp4920_msg_2(compact_otp4920_msg24(), bad_mid_ip6addr_length).

%% Length
compact_otp4920_msg25(suite) ->
    [];
compact_otp4920_msg25(Config) when is_list(Config) ->
    d("compact_otp4920_msg25 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp4920_msg_2(compact_otp4920_msg25(), bad_mid_ip6addr_length).

compact_otp4920_msg_1(M1, CheckEqual) ->
    Bin1 = list_to_binary(M1),
    case decode_message(megaco_compact_text_encoder, false, [], Bin1) of
	{ok, Msg} ->
 	    io:format(" decoded", []),
	    case encode_message(megaco_compact_text_encoder, [], Msg) of
		{ok, Bin1} ->
		    io:format(", encoded - equal:", []),
		    ok;
		{ok, Bin2} when CheckEqual =:= true ->
		    M2 = binary_to_list(Bin2),
		    io:format(", encoded - not equal:", []),
		    exit({messages_not_equal, M1, M2});
		{ok, _Bin2} ->
		    io:format(", encoded:", []),
		    ok;
		Else ->
		    io:format(", encode failed:", []),
		    exit({unexpected_encode_result, Else})
	    end;
	Else ->
	    io:format("decode failed:", []),
	    exit({unexpected_decode_result, Else})
    end.

compact_otp4920_msg_2(M1, ExpectedReason) ->
    Bin = list_to_binary(M1),
    case decode_message(megaco_compact_text_encoder, false, [], Bin) of
	{ok, Msg} ->
	    io:format("unexpected successfull decode", []),
	    exit({unexpected_encode_ok, Msg});
	{error, [{reason, {__Line, _Mod, Reason}}|_]} ->
	    case element(1, Reason) of
		ExpectedReason ->
		    ok;
		_ ->
		    exit({unexpected_decode_error_reason, 
			  ExpectedReason, Reason})
	    end;
	{error, [{reason, {_Mod, Reason}}|_]} ->
	    case element(1, Reason) of
		ExpectedReason ->
		    ok;
		_ ->
		    exit({unexpected_decode_error_reason, 
			  ExpectedReason, Reason})
	    end;
	Else ->
	    io:format("unexpected decode result", []),
	    exit({unexpected_decode_result, Else})
	    
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


compact_otp5186_msg01(suite) ->
    [];
compact_otp5186_msg01(Config) when is_list(Config) ->
    d("compact_otp5186_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp5186_msg_1(compact_otp5186_msg01(), error, ignore).

compact_otp5186_msg02(suite) ->
    [];
compact_otp5186_msg02(Config) when is_list(Config) ->
    d("compact_otp5186_msg02 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp5186_msg_1(compact_otp5186_msg02(), ok, ok).

compact_otp5186_msg03(suite) ->
    [];
compact_otp5186_msg03(Config) when is_list(Config) ->
    d("compact_otp5186_msg03 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp5186_msg_2(compact_otp5186_msg03(), ok, ok).

compact_otp5186_msg04(suite) ->
    [];
compact_otp5186_msg04(Config) when is_list(Config) ->
    d("compact_otp5186_msg04 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp5186_msg_2(compact_otp5186_msg04(), ok, ok).

compact_otp5186_msg05(suite) ->
    [];
compact_otp5186_msg05(Config) when is_list(Config) ->
    d("compact_otp5186_msg05 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp5186_msg_2(compact_otp5186_msg05(), ok, ok).

compact_otp5186_msg06(suite) ->
    [];
compact_otp5186_msg06(Config) when is_list(Config) ->
    d("compact_otp5186_msg06 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp5186_msg_2(compact_otp5186_msg06(), ok, ok).

compact_otp5186_msg_1(M1, DecodeExpect, EncodeExpect) ->
    Bin1 = list_to_binary(M1),
    case decode_message(megaco_compact_text_encoder, false, [], Bin1) of
	{ok, Msg} when DecodeExpect =:= ok ->
 	    io:format(" decoded", []),
	    case encode_message(megaco_compact_text_encoder, [], Msg) of
		{ok, Bin1} when EncodeExpect =:= ok ->
		    io:format(", encoded - equal:", []),
		    ok;
		{ok, Bin2} when EncodeExpect =:= ok ->
		    M2 = binary_to_list(Bin2),
		    io:format(", encoded - not equal:", []),
		    exit({messages_not_equal, Msg, M1, M2});
		{ok, Bin3} when EncodeExpect =:= error ->
		    M3 = binary_to_list(Bin3),
		    io:format(", unexpected encode:", []),
		    exit({unexpected_encode_success, Msg, M1, M3});
		_Else when EncodeExpect =:= error ->
		    io:format(", encode failed ", []),
		    ok
	    end;
	{ok, Msg} when DecodeExpect =:= error ->
 	    io:format(" decoded", []),
	    exit({unexpected_decode_success, Msg});
	_Else when DecodeExpect =:= error ->
	    io:format(" decode failed ", []),
	    ok;
	Else when DecodeExpect =:= ok ->
	    io:format(" decode failed ", []),
	    exit({unexpected_decode_result, Else})
    end.

compact_otp5186_msg_2(Msg1, EncodeExpect, DecodeExpect) ->
    case encode_message(megaco_compact_text_encoder, [], Msg1) of
	{ok, Bin} when EncodeExpect =:= ok ->
 	    io:format(" encoded", []),
	    case decode_message(megaco_compact_text_encoder, false, [], Bin) of
		{ok, Msg1} when DecodeExpect =:= ok ->
		    io:format(", decoded - equal:", []),
		    ok;
		{ok, Msg2} when DecodeExpect =:= ok ->
		    M = binary_to_list(Bin),
		    case (catch compact_otp5186_check_megamsg(Msg1, Msg2)) of
			ok ->
			    io:format(", decoded - not equal - ok:", []),
			    ok;
			{'EXIT', Reason} ->
			    io:format(", decoded - not equal:", []),
			    exit({messages_not_equal, M, Reason, Msg1, Msg2})
		    end;
		{ok, Msg3} when DecodeExpect =:= error ->
		    M = binary_to_list(Bin),
		    io:format(", decoded:", []),
		    exit({unexpected_decode_success, M, Msg1, Msg3});
		Else when DecodeExpect =:= ok ->
		    M = binary_to_list(Bin),
		    io:format(", decode failed ", []),
		    exit({unexpected_decode_success, Msg1, M, Else});
		_Else when DecodeExpect =:= error ->
		    io:format(", decode failed ", []),
		    ok
	    end;
	{ok, Bin} when EncodeExpect =:= error ->
	    M = binary_to_list(Bin),
	    io:format(" encoded", []),
	    exit({unexpected_encode_success, Msg1, M});
	_Else when EncodeExpect =:= error ->
	    io:format(" encode failed ", []),
	    ok;
	Else when EncodeExpect =:= ok ->
	    io:format(" encode failed ", []),
	    exit({unexpected_encode_result, Else})
    end.


%% --
						   
compact_otp5186_msg01() ->
    "!/1 <mg5>\nP=67111298{C=2699{AV=mg5_ipeph/0x0f0001{}}}".

compact_otp5186_msg02() ->
    "!/1 <mg5>\nP=67111298{C=2699{AV=mg5_ipeph/0x0f0001}}".

compact_otp5186_msg03() ->
    {'MegacoMessage',
     asn1_NOVALUE,
     {'Message',
      1,
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
	  }
	 }
	}
       ]
      }
     }
    }.

compact_otp5186_msg04() ->
    {'MegacoMessage',asn1_NOVALUE,
     {'Message',1,{domainName,{'DomainName',"mg5",asn1_NOVALUE}},
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
				 {'AuditDescriptor',asn1_NOVALUE}
				}
			       ]
			      }
			     }
			    }
			   ]
			  }
			 ]
	  }
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
      1,
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
	  }
	 }
	}
       ]
      }
     }
    }.

compact_otp5186_msg06() ->
    {'MegacoMessage',asn1_NOVALUE,
     {'Message',1,{domainName,{'DomainName',"mg5",asn1_NOVALUE}},
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
				{'AuditDescriptor',asn1_NOVALUE}
			       }
			      ]
			     }
			    }
			   ]
			  }
			 ]
	  }
	 }
	}
       ]
      }
     }
    }.

%% --

compact_otp5186_check_megamsg(M1, M1) ->
    ok;
compact_otp5186_check_megamsg(#'MegacoMessage'{authHeader = AH,
					       mess = M1}, 
			      #'MegacoMessage'{authHeader = AH,
					       mess = M2}) ->
    compact_otp5186_check_mess(M1, M2);
compact_otp5186_check_megamsg(#'MegacoMessage'{authHeader = AH1},
			      #'MegacoMessage'{authHeader = AH2}) ->
    exit({not_equal, authHeader, AH1, AH2}).

compact_otp5186_check_mess(M, M) ->
    ok;
compact_otp5186_check_mess(#'Message'{version     = V, 
				      mId         = MId,
				      messageBody = B1},
			   #'Message'{version     = V, 
				      mId         = MId,
				      messageBody = B2}) ->
    compact_otp5186_check_body(B1, B2);
compact_otp5186_check_mess(#'Message'{version     = V, 
				      mId         = MId1},
			   #'Message'{version     = V, 
				      mId         = MId2}) ->
    exit({not_equal, mId, MId1, MId2});
compact_otp5186_check_mess(#'Message'{version     = V1, 
				      mId         = MId},
			   #'Message'{version     = V2, 
				      mId         = MId}) ->
    exit({not_equal, version, V1, V2}).

compact_otp5186_check_body(B, B) ->
    ok;
compact_otp5186_check_body({transactions, T1}, {transactions, T2}) ->
    compact_otp5186_check_trans(T1, T2);
compact_otp5186_check_body({messageError, E1}, {messageError, E2}) ->
    compact_otp5186_check_merr(E1, E2);
compact_otp5186_check_body(B1, B2) ->
    exit({not_equal, messageBody, B1, B2}).

compact_otp5186_check_trans([], []) ->
    ok;
compact_otp5186_check_trans([], T2) ->
    exit({not_equal, transactions, [], T2});
compact_otp5186_check_trans(T1, []) ->
    exit({not_equal, transactions, T1, []});
compact_otp5186_check_trans([Tran1|Trans1], [Tran2|Trans2]) ->
    compact_otp5186_check_trans(Trans1, Trans2),
    compact_otp5186_check_transaction(Tran1, Tran2).

compact_otp5186_check_merr(ME, ME) ->
    ok;
compact_otp5186_check_merr(#'ErrorDescriptor'{errorCode = EC,
					      errorText = ET1},
			   #'ErrorDescriptor'{errorCode = EC,
					      errorText = ET2}) ->
    exit({not_equal, errorText, ET1, ET2});
compact_otp5186_check_merr(#'ErrorDescriptor'{errorCode = EC1,
					      errorText = ET},
			   #'ErrorDescriptor'{errorCode = EC2,
					      errorText = ET}) ->
    exit({not_equal, errorCode, EC1, EC2}).

compact_otp5186_check_transaction(T, T) ->
    ok;
compact_otp5186_check_transaction({transactionReply, TR1}, 
				  {transactionReply, TR2}) ->
    compact_otp5186_check_transRep(TR1, TR2);
compact_otp5186_check_transaction(T1, T2) ->
    exit({unexpected_transactions, T1, T2}).
    
compact_otp5186_check_transRep(T, T) ->
    ok;
compact_otp5186_check_transRep(#'TransactionReply'{transactionId     = TId,
						   immAckRequired    = IAR,
						   transactionResult = TR1},
			       #'TransactionReply'{transactionId     = TId,
						   immAckRequired    = IAR,
						   transactionResult = TR2}) ->
    compact_otp5186_check_transRes(TR1, TR2);
compact_otp5186_check_transRep(T1, T2) ->
    exit({unexpected_transaction_reply, T1, T2}).

compact_otp5186_check_transRes(TR, TR) ->
    ok;
compact_otp5186_check_transRes({actionReplies, AR1}, 
			       {actionReplies, AR2}) ->
    compact_otp5186_check_actReps(AR1, AR2);
compact_otp5186_check_transRes(TR1, TR2) ->
    exit({unexpected_transaction_result, TR1, TR2}).

compact_otp5186_check_actReps([], []) ->
    ok;
compact_otp5186_check_actReps(AR1, []) ->
    exit({not_equal, actionReplies, AR1, []});
compact_otp5186_check_actReps([], AR2) ->
    exit({not_equal, actionReplies, [], AR2});
compact_otp5186_check_actReps([AR1|ARs1], [AR2|ARs2]) ->
    compact_otp5186_check_actRep(AR1, AR2),
    compact_otp5186_check_actReps(ARs1, ARs2).

compact_otp5186_check_actRep(AR, AR) ->
    ok;
compact_otp5186_check_actRep(#'ActionReply'{contextId       = ID,
					    errorDescriptor = ED,
					    contextReply    = CtxRep,
					    commandReply    = CmdRep1},
			     #'ActionReply'{contextId       = ID,
					    errorDescriptor = ED,
					    contextReply    = CtxRep,
					    commandReply    = CmdRep2}) ->
    compact_otp5186_check_cmdReps(CmdRep1, CmdRep2);
compact_otp5186_check_actRep(AR1, AR2) ->
    exit({unexpected_actionReply, AR1, AR2}).

compact_otp5186_check_cmdReps([], []) ->
    ok;
compact_otp5186_check_cmdReps(CR1, []) ->
    exit({not_equal, commandReplies, CR1, []});
compact_otp5186_check_cmdReps([], CR2) ->
    exit({not_equal, commandReplies, [], CR2});
compact_otp5186_check_cmdReps([CR1|CRs1], [CR2|CRs2]) ->
    compact_otp5186_check_cmdRep(CR1, CR2),
    compact_otp5186_check_cmdReps(CRs1, CRs2).

compact_otp5186_check_cmdRep(CR, CR) ->
    ok;
compact_otp5186_check_cmdRep({auditValueReply, AVR1}, 
			     {auditValueReply, AVR2}) ->
    compact_otp5186_check_auditReply(AVR1, AVR2);
compact_otp5186_check_cmdRep({addReply, AVR1}, 
			     {addReply, AVR2}) ->
    compact_otp5186_check_ammsReply(AVR1, AVR2);
compact_otp5186_check_cmdRep(CR1, CR2) ->
    exit({unexpected_commandReply, CR1, CR2}).

compact_otp5186_check_auditReply(AR, AR) ->
    ok;
compact_otp5186_check_auditReply({auditResult, AR1}, 
				 {auditResult, AR2}) ->
    compact_otp5186_check_auditRes(AR1, AR2);
compact_otp5186_check_auditReply(AR1, AR2) ->
    exit({unexpected_auditReply, AR1, AR2}).

compact_otp5186_check_ammsReply(AR, AR) ->
    ok;
compact_otp5186_check_ammsReply(#'AmmsReply'{terminationID = ID,
					     terminationAudit = TA1},
				#'AmmsReply'{terminationID = ID,
					     terminationAudit = TA2}) ->
    %% This is just to simplify the test
    F = fun(asn1_NOVALUE) -> [];
	   (E) -> E
	end,
    compact_otp5186_check_termAudit(F(TA1), F(TA2));
compact_otp5186_check_ammsReply(AR1, AR2) ->
    exit({unexpected_ammsReply, AR1, AR2}).

compact_otp5186_check_auditRes(AR, AR) ->
    ok;
compact_otp5186_check_auditRes(#'AuditResult'{terminationID = ID,
					      terminationAuditResult = TAR1},
			       #'AuditResult'{terminationID = ID,
					      terminationAuditResult = TAR2}) ->
    compact_otp5186_check_termAuditRes(TAR1, TAR2);
compact_otp5186_check_auditRes(AR1, AR2) ->
    exit({unexpected_auditResult, AR1, AR2}).

compact_otp5186_check_termAuditRes([], []) ->
    ok;
%% An empty empty descriptor is removed
compact_otp5186_check_termAuditRes([{emptyDescriptors,
				     #'AuditDescriptor'{auditToken = asn1_NOVALUE}}|TAR1], []) ->
    compact_otp5186_check_termAuditRes(TAR1, []);
compact_otp5186_check_termAuditRes(TAR1, []) ->
    exit({not_equal, termAuditRes, TAR1, []});
%% An empty empty descriptor is removed
compact_otp5186_check_termAuditRes([], [{emptyDescriptors,
					 #'AuditDescriptor'{auditToken = asn1_NOVALUE}}|TAR2]) ->
    compact_otp5186_check_termAuditRes([], TAR2);
compact_otp5186_check_termAuditRes([], TAR2) ->
    exit({not_equal, termAuditRes, [], TAR2});
compact_otp5186_check_termAuditRes([ARP1|TAR1], [ARP2|TAR2]) ->
    compact_otp5186_check_auditRetParm(ARP1, ARP2),
    compact_otp5186_check_termAuditRes(TAR1, TAR2).

compact_otp5186_check_termAudit([], []) ->
    ok;
%% An empty empty descriptor is removed
compact_otp5186_check_termAudit([{emptyDescriptors,
				  #'AuditDescriptor'{auditToken = asn1_NOVALUE}}|TAR1], []) ->
    compact_otp5186_check_termAudit(TAR1, []);
compact_otp5186_check_termAudit(TAR1, []) ->
    exit({not_equal, termAudit, TAR1, []});
%% An empty empty descriptor is removed
compact_otp5186_check_termAudit([], 
				[{emptyDescriptors,
				  #'AuditDescriptor'{auditToken = asn1_NOVALUE}}|TAR2]) ->
    compact_otp5186_check_termAudit([], TAR2);
compact_otp5186_check_termAudit([], TAR2) ->
    exit({not_equal, termAudit, [], TAR2});
compact_otp5186_check_termAudit([ARP1|TAR1], [ARP2|TAR2]) ->
    compact_otp5186_check_auditRetParm(ARP1, ARP2),
    compact_otp5186_check_termAudit(TAR1, TAR2).

compact_otp5186_check_auditRetParm(ARP, ARP) ->
    ok;
compact_otp5186_check_auditRetParm({emptyDescriptors, AD1}, 
				   {emptyDescriptors, AD2}) ->
    compact_otp5186_check_auditDesc(AD1, AD2);
compact_otp5186_check_auditRetParm(ARP1, ARP2) ->
    exit({unexpected_auditRetParm, ARP1, ARP2}).

compact_otp5186_check_auditDesc(AD, AD) ->
    ok;
compact_otp5186_check_auditDesc(#'AuditDescriptor'{auditToken = L1},
				#'AuditDescriptor'{auditToken = L2}) ->
    compact_otp5186_check_auditDesc_auditItems(L1, L2);
compact_otp5186_check_auditDesc(AD1, AD2) ->
    exit({unexpected_auditDesc, AD1, AD2}).

compact_otp5186_check_auditDesc_auditItems([], []) ->
    ok;
compact_otp5186_check_auditDesc_auditItems(AI1, []) ->
    exit({not_equal, auditItems, AI1, []});
compact_otp5186_check_auditDesc_auditItems([], AI2) ->
    exit({not_equal, auditItems, [], AI2});
compact_otp5186_check_auditDesc_auditItems([AI1|AIs1], [AI2|AIs2]) ->
    compact_otp5186_check_auditDesc_auditItem(AI1, AI2),
    compact_otp5186_check_auditDesc_auditItems(AIs1, AIs2).

compact_otp5186_check_auditDesc_auditItem(AI, AI) ->
    ok;
compact_otp5186_check_auditDesc_auditItem(AI1, AI2) ->
    exit({not_equal, auditItem, AI1, AI2}).

compact_otp5793_msg01(suite) ->
    [];
compact_otp5793_msg01(Config) when is_list(Config) ->
    d("compact_otp5793_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp5793(ok, pretty_otp5793_msg1()).

compact_otp5793(Expected, Msg) ->
    expect_codec(Expected, megaco_compact_text_encoder, Msg, []).


%% --------------------------------------------------------------

compact_otp5993_msg01(suite) ->
    [];
compact_otp5993_msg01(Config) when is_list(Config) ->
    d("compact_otp5993_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp5993_msg_1(compact_otp5993_msg01(), ok, ok).

compact_otp5993_msg02(suite) ->
    [];
compact_otp5993_msg02(Config) when is_list(Config) ->
    d("compact_otp5993_msg02 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp5993_msg_1(compact_otp5993_msg02(), ok, ok).

compact_otp5993_msg03(suite) ->
    [];
compact_otp5993_msg03(Config) when is_list(Config) ->
    d("compact_otp5993_msg03 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    compact_otp5993_msg_1(compact_otp5993_msg03(), ok, ok).

compact_otp5993_msg_1(Msg1, EncodeExpect, DecodeExpect) ->
    case encode_message(megaco_compact_text_encoder, [], Msg1) of
	{ok, Bin} when EncodeExpect =:= ok ->
 	    io:format(" encoded", []),
 	    %% io:format(" encoded:~n~s~n", [binary_to_list(Bin)]),
	    case decode_message(megaco_compact_text_encoder, false, [], Bin) of
		{ok, Msg1} when DecodeExpect =:= ok ->
		    io:format(", decoded - equal:", []),
		    ok;
		{ok, Msg3} when DecodeExpect =:= error ->
		    M = binary_to_list(Bin),
		    io:format(", decoded:", []),
		    exit({unexpected_decode_success, M, Msg1, Msg3});
		Else when DecodeExpect =:= ok ->
		    M = binary_to_list(Bin),
		    io:format(", decode failed ", []),
		    exit({unexpected_decode_failure, Msg1, M, Else});
		_Else when DecodeExpect =:= error ->
		    io:format(", decode failed ", []),
		    ok
	    end;
	{ok, Bin} when EncodeExpect =:= error ->
	    M = binary_to_list(Bin),
	    io:format(" encoded", []),
	    exit({unexpected_encode_success, Msg1, M});
	_Else when EncodeExpect =:= error ->
	    io:format(" encode failed ", []),
	    ok;
	Else when EncodeExpect =:= ok ->
	    io:format(" encode failed ", []),
	    exit({unexpected_encode_result, Else})
    end.

compact_otp5993_msg01() ->
    MT = h221,
    T  = #megaco_term_id{id = ?A4444},
    TL = [T], 
    MD = #'MuxDescriptor'{muxType  = MT,
			  termList = TL},
    compact_otp5993_msg(MD).

compact_otp5993_msg02() ->
    MT = h223,
    T1 = #megaco_term_id{id = ?A4445},
    T2 = #megaco_term_id{id = ?A5556},
    TL = [T1, T2], 
    MD = #'MuxDescriptor'{muxType  = MT,
			  termList = TL},
    compact_otp5993_msg(MD).

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
    M   = compact_otp6017_msg(BadCID),
    Bin = list_to_binary(M), 
    case decode_message(megaco_compact_text_encoder, false, [], Bin) of
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

flex_compact_otp7431_msg01a(suite) ->
    [];
flex_compact_otp7431_msg01a(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_comppact_otp7431_msg01a -> entry", []),
    Conf = flex_scanner_conf(Config),
    flex_compact_otp7431(ok, flex_compact_otp7431_msg1a(), [Conf]).

flex_compact_otp7431_msg01b(suite) ->
    [];
flex_compact_otp7431_msg01b(Config) when is_list(Config) ->
    %% put(severity,trc),
    %% put(dbg,true),
    d("flex_comppact_otp7431_msg01b -> entry", []),
    Conf = flex_scanner_conf(Config),
    flex_compact_otp7431(ok, flex_compact_otp7431_msg1b(), [Conf]).

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

flex_compact_otp7431_msg1a() ->
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

flex_compact_otp7431_msg1b() ->
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
    d("pretty_otp4632_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg0 = pretty_otp4632_msg1(),
    case encode_message(megaco_pretty_text_encoder, [], Msg0) of
	{ok, BinMsg} when is_binary(BinMsg) ->
	    {ok, Msg1} = decode_message(megaco_pretty_text_encoder, false, 
					[], BinMsg),
	    ok = chk_MegacoMessage(Msg0,Msg1);
	Else ->
	    t("pretty_otp4632_msg1 -> "
	      "~n   Else: ~w", [Else]),
	    exit({unexpected_decode_result, Else})
    end.

pretty_otp4632_msg1() ->
    msg4(?MG1_MID_NO_PORT, "901 mg col boot").

pretty_otp4632_msg2(suite) ->
    [];
pretty_otp4632_msg2(Config) when is_list(Config) ->
    d("pretty_otp4632_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg0 = pretty_otp4632_msg2(),
    case encode_message(megaco_pretty_text_encoder, [], Msg0) of
	{ok, BinMsg} when is_binary(BinMsg) ->
	    {ok, Msg1} = decode_message(megaco_pretty_text_encoder, false, 
					[], BinMsg),
	    ok = chk_MegacoMessage(Msg0,Msg1);
	Else ->
	    t("pretty_otp4632_msg2 -> "
	      "~n   Else: ~w", [Else]),
	    exit({unexpected_decode_result, Else})
    end.

pretty_otp4632_msg2() ->
    msg4(?MG1_MID_NO_PORT, "901").


pretty_otp4632_msg3(suite) ->
    [];
pretty_otp4632_msg3(Config) when is_list(Config) ->
    d("pretty_otp4632_msg3 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg0 = pretty_otp4632_msg3(),
    Bin0 = list_to_binary(Msg0),
    case decode_message(megaco_pretty_text_encoder, 
			false, [], Bin0) of
	{ok, Msg} when is_record(Msg, 'MegacoMessage') ->
	    {ok, Bin1} = encode_message(megaco_pretty_text_encoder, [], Msg),
	    Msg1 = binary_to_list(Bin1),
	    %% io:format("Msg1:~n~s~n", [Msg1]),
	    Msg0 = Msg1,
	    ok;
	Else ->
	    t("pretty_otp4632_msg3 -> "
	      "~n   Else: ~w", [Else]),
	    exit({unexpected_decode_result, Else})
    end.

pretty_otp4632_msg3() ->
    M = "MEGACO/" ?VERSION_STR " [124.124.124.222]\nTransaction = 9998 {\n\tContext = - {\n\t\tServiceChange = root {\n\t\t\tServices {\n\t\t\t\tMethod = Restart,\n\t\t\t\tServiceChangeAddress = 55555,\n\t\t\t\tProfile = resgw/1,\n\t\t\t\tReason = \"901\"\n\t\t\t}\n\t\t}\n\t}\n}",
    M.


pretty_otp4632_msg4(suite) ->
    [];
pretty_otp4632_msg4(Config) when is_list(Config) ->
    d("pretty_otp4632_msg4 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg0 = pretty_otp4632_msg4(),
    Bin0 = list_to_binary(Msg0),
    case decode_message(megaco_pretty_text_encoder, false, [], Bin0) of
	{ok, Msg} when is_record(Msg, 'MegacoMessage') ->
	    {ok, Bin1} = encode_message(megaco_pretty_text_encoder, [], Msg),
	    Msg1 = binary_to_list(Bin1),
	    %% io:format("Msg1:~n~s~n", [Msg1]),
	    pretty_otp4632_msg4_chk(Msg0,Msg1);
	Else ->
	    t("pretty_otp4632_msg4 -> "
	      "~n   Else: ~w", [Else]),
	    exit({unexpected_decode_result, Else})
    end.


pretty_otp4632_msg4() ->
    M = "MEGACO/" ?VERSION_STR " [124.124.124.222]\nTransaction = 9998 {\n\tContext = - {\n\t\tServiceChange = root {\n\t\t\tServices {\n\t\t\t\tMethod = Restart,\n\t\t\t\tServiceChangeAddress = 55555,\n\t\t\t\tProfile = resgw/1,\n\t\t\t\tReason = 901\n\t\t\t}\n\t\t}\n\t}\n}",
    M.


pretty_otp4632_msg4_chk([], []) ->
    exit(messages_not_eq); 
pretty_otp4632_msg4_chk([], Rest1) ->
    exit({messages_not_eq1, Rest1}); 
pretty_otp4632_msg4_chk(Rest0, []) ->
    exit({messages_not_eq0, Rest0}); 
pretty_otp4632_msg4_chk([$R,$e,$a,$s,$o,$n,$ ,$=,$ ,$9,$0,$1|_Rest0],
			[$R,$e,$a,$s,$o,$n,$ ,$=,$ ,$",$9,$0,$1,$"|_Rest1]) ->
    ok;
pretty_otp4632_msg4_chk([_|Rest0], [_|Rest1]) ->
    pretty_otp4632_msg4_chk(Rest0,Rest1).


pretty_otp4710_msg1(suite) ->
    [];
pretty_otp4710_msg1(Config) when is_list(Config) ->
    d("pretty_otp4710_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg0 = pretty_otp4710_msg1(),
    case encode_message(megaco_pretty_text_encoder, [], Msg0) of
	{ok, Bin} when is_binary(Bin) ->
	    {ok, Msg1} = decode_message(megaco_pretty_text_encoder, false, 
					[], Bin),
	    ok = chk_MegacoMessage(Msg0,Msg1);
	Else ->
	    t("pretty_otp4710_msg1 -> "
	      "~n   Else: ~w", [Else]),
	    exit({unexpected_decode_result, Else})
    end.

pretty_otp4710_msg1() ->
    msg40().


pretty_otp4710_msg2(suite) ->
    [];
pretty_otp4710_msg2(Config) when is_list(Config) ->
    d("pretty_otp4710_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg0 = pretty_otp4710_msg2(),
    Bin0 = list_to_binary(Msg0),
    case decode_message(megaco_pretty_text_encoder, false, [], Bin0) of
	{ok, Msg} when is_record(Msg, 'MegacoMessage') ->
	    {ok, Bin1} = encode_message(megaco_pretty_text_encoder, [], Msg),
	    Msg1 = binary_to_list(Bin1),
	    %% io:format("Msg1:~n~s~n", [Msg1]),
	    pretty_otp4710_msg2_chk(Msg0,Msg1);
	Else ->
	    t("pretty_otp4710_msg2 -> "
	      "~n   Else: ~w", [Else]),
	    exit({unexpected_decode_result, Else})
    end.

pretty_otp4710_msg2() ->
    "Authentication = 0xEFCDAB89:0x12345678:0x1234567889ABCDEF76543210\nMEGACO/" ?VERSION_STR " [124.124.124.222]\nTransaction = 9998 {\n\tContext = - {\n\t\tServiceChange = root {\n\t\t\tServices {\n\t\t\t\tMethod = Restart,\n\t\t\t\tServiceChangeAddress = 55555,\n\t\t\t\tProfile = resgw/1,\n\t\t\t\tReason = \"901 mg col boot\"\n\t\t\t}\n\t\t}\n\t}\n}".

pretty_otp4710_msg2_chk(Msg,Msg) ->
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


pretty_otp4945_msg1(suite) ->
    [];
pretty_otp4945_msg1(Config) when is_list(Config) ->
    d("pretty_otp4945_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg0 = pretty_otp4945_msg1(),
    Bin0 = list_to_binary(Msg0),
    case decode_message(megaco_pretty_text_encoder, false, [], Bin0) of
	{error, [{reason, Reason}|_]} ->
	    case Reason of
		{missing_required_serviceChangeParm, [serviceChangeReason]} ->
		    ok;
		Else ->
		    t("pretty_otp4945_msg1 -> "
		      "~n   Else: ~w", [Else]),
		    exit({unexpected_decode_result, Else})
	    end;
	Else ->
	    io:format("pretty_otp4945_msg1 -> "
		      "~n   Else: ~w"
		      "~n", [Else]),
	    exit({unexpected_decode_result, Else})
    end.

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
    

pretty_otp4945_msg2(suite) ->
    [];
pretty_otp4945_msg2(Config) when is_list(Config) ->
    d("pretty_otp4945_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg0 = pretty_otp4945_msg2(),
    Bin0 = list_to_binary(Msg0),
    case decode_message(megaco_pretty_text_encoder, false, [], Bin0) of
	{error, [{reason, Reason}|_]} ->
	    case Reason of
		{missing_required_serviceChangeParm, [serviceChangeMethod]} ->
		    ok;
		Else ->
		    t("pretty_otp4945_msg2 -> "
		      "~n   Else: ~w", [Else]),
		    exit({unexpected_decode_result, Else})
	    end;
	Else ->
	    t("pretty_otp4945_msg2 -> "
	      "~n   Else: ~w", [Else]),
	    exit({unexpected_decode_result, Else})
    end.

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

    
pretty_otp4945_msg3(suite) ->
    [];
pretty_otp4945_msg3(Config) when is_list(Config) ->
    d("pretty_otp4945_msg3 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg0 = pretty_otp4945_msg3(),
    Bin0 = list_to_binary(Msg0),
    case decode_message(megaco_pretty_text_encoder, false, [], Bin0) of
	{error, [{reason, Reason}|_]} ->
	    case Reason of
		{missing_required_serviceChangeParm, [serviceChangeReason, serviceChangeMethod]} ->
		    ok;
		{missing_required_serviceChangeParm, [serviceChangeMethod, serviceChangeReason]} ->
		    ok;
		Else ->
		    t("pretty_otp4945_msg3 -> "
		      "~n   Else: ~w", [Else]),
		    exit({unexpected_decode_result, Else})
	    end;
	Else ->
	    t("pretty_otp4945_msg3 -> "
	      "~n   Else: ~w", [Else]),
	    exit({unexpected_decode_result, Else})
    end.

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

    
pretty_otp4945_msg4(suite) ->
    [];
pretty_otp4945_msg4(Config) when is_list(Config) ->
    d("pretty_otp4945_msg4 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg0 = pretty_otp4945_msg4(),
    Bin0 = list_to_binary(Msg0),
    case decode_message(megaco_pretty_text_encoder, false, [], Bin0) of
	{ok, _} ->
	    ok;
	Else ->
	    t("pretty_otp4945_msg4 -> "
	      "~n   Else: ~w", [Else]),
	    exit({unexpected_decode_result, Else})
    end.

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
    d("pretty_otp4945_msg5 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg0 = pretty_otp4945_msg5(),
    Bin0 = list_to_binary(Msg0),
    case decode_message(megaco_pretty_text_encoder, false, [], Bin0) of
	{error, [{reason, Reason}|_]} ->
	    case Reason of
		{at_most_once_serviceChangeParm, {profile, _Val1, _Val2}} ->
		    ok;
		Else ->
		    io:format("pretty_otp4945_msg6 -> "
			      "~n   Else: ~w"
			      "~n", [Else]),
		    exit({unexpected_decode_result, Else})
	    end;
	Else ->
	    t("pretty_otp4945_msg5 -> "
	      "~n   Else: ~w", [Else]),
	    exit({unexpected_decode_result, Else})
    end.

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
    

pretty_otp4945_msg6(suite) ->
    [];
pretty_otp4945_msg6(Config) when is_list(Config) ->
    d("pretty_otp4945_msg6 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg0 = pretty_otp4945_msg6(),
    Bin0 = list_to_binary(Msg0),
    case decode_message(megaco_pretty_text_encoder, false, [], Bin0) of
	{error, [{reason, Reason}|_]} ->
	    case Reason of
		{not_both_address_mgcid_serviceChangeParm, _Val1, _Val2} ->
		    ok;
		Else ->
		    io:format("pretty_otp4945_msg6 -> "
			      "~n   Else: ~w"
			      "~n", [Else]),
		    exit({unexpected_decode_result, Else})
	    end;
	Else ->
	    t("pretty_otp4945_msg6 -> "
	      "~n   Else: ~w", [Else]),
	    exit({unexpected_decode_result, Else})
    end.

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
    

pretty_otp4949_msg1(suite) ->
    [];
pretty_otp4949_msg1(Config) when is_list(Config) ->
    d("pretty_otp4949_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg0 = pretty_otp4949_msg1(),
    Bin0 = list_to_binary(Msg0),
    case decode_message(megaco_pretty_text_encoder, false, [], Bin0) of
	{ok, _} ->
	    ok;
	Else ->
	    t("pretty_otp4949_msg1 -> "
	      "~n   Else: ~w", [Else]),
	    exit({unexpected_decode_result, Else})
    end.

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
    d("pretty_otp4949_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg0 = pretty_otp4949_msg2(),
    Bin0 = list_to_binary(Msg0),
    case decode_message(megaco_pretty_text_encoder, false, [], Bin0) of
	{error, [{reason, Reason}|_]} ->
	    case Reason of
		{at_most_once_servChgReplyParm, {profile, _Val1, _Val2}} ->
		    ok;
		Else ->
		    io:format("pretty_otp4949_msg2 -> "
			      "~n   Else: ~w"
			      "~n", [Else]),
		    exit({unexpected_decode_result, Else})
	    end;
	Else ->
	    t("pretty_otp4949_msg2 -> "
	      "~n   Else: ~w", [Else]),
	    exit({unexpected_decode_result, Else})
    end.

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
    

pretty_otp4949_msg3(suite) ->
    [];
pretty_otp4949_msg3(Config) when is_list(Config) ->
    d("pretty_otp4949_msg3 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg0 = pretty_otp4949_msg3(),
    Bin0 = list_to_binary(Msg0),
    case decode_message(megaco_pretty_text_encoder, false, [], Bin0) of
	{error, [{reason, Reason}|_]} ->
	    case Reason of
		{not_both_address_mgcid_servChgReplyParm, _Val1, _Val2} ->
		    ok;
		Else ->
		    io:format("pretty_otp4949_msg3 -> "
			      "~n   Else: ~w"
			      "~n", [Else]),
		    exit({unexpected_decode_result, Else})
	    end;
	Else ->
	    t("pretty_otp4949_msg3 -> "
	      "~n   Else: ~w", [Else]),
	    exit({unexpected_decode_result, Else})
    end.

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
    

pretty_otp5042_msg1(suite) ->
    [];
pretty_otp5042_msg1(Config) when is_list(Config) ->
    d("pretty_otp5042_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg0 = pretty_otp5042_msg1(),
    Bin0 = list_to_binary(Msg0),
    case decode_message(megaco_pretty_text_encoder, false, [], Bin0) of
	{error, [{reason, Reason}|_]} ->
	    case Reason of
		{_, _Mod, {not_an_integer, PropertyParm}} ->
		    exit({not_an_integer, PropertyParm});
		_ ->
		    io:format("pretty_otp5042_msg1 -> "
			      "~n   Reason: ~w"
			      "~n", [Reason]),
		    exit({unexpected_decode_result, Reason})
	    end;
	{ok, M} ->
	    t("pretty_otp5042_msg1 -> successfull decode:"
	      "~n~p", [M]),
	    ok
    end.

pretty_otp5042_msg1() ->
"MEGACO/" ?VERSION_STR " <CATAPULT>:2944
Transaction = 102 { 
Context =  5 { Notify =  MUX/1 { ObservedEvents = 1 { 
h245bh/h245msgin { Stream =  1
, h245enc =
0270020600088175000653401004100403E802E00180018001780680000034301160000700088175010101007A0100020001800001320000C0000219D005027F0070500100040100021080000319D005027F00504001008000041C001250000700088175010000400280010003000880000518AA027F400006850130008011020100000001030002000300040005000006
 }  } 
 }  }  }".
  

pretty_otp5068_msg1(suite) ->
    [];
pretty_otp5068_msg1(Config) when is_list(Config) ->
    d("pretty_otp5068_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    Msg = pretty_otp5068_msg1(),
    case encode_message(megaco_pretty_text_encoder, [], Msg) of
	{error, Reason} ->
% 	    io:format("pretty_otp5068_msg1 -> "
% 		      "~n   Reason: ~w"
% 		      "~n", [Reason]),
	    exit({unexpected_encode_result, Reason});
	{ok, Bin} ->
% 	    io:format("pretty_otp5068_msg1 -> successfull encode:"
% 		      "~n~s~n", [binary_to_list(Bin)]),
	    case decode_message(megaco_pretty_text_encoder, false, [], Bin) of
		{ok, _} ->
% 		    io:format("pretty_otp5068_msg1 -> ok~n", []),
		    ok;
		Else ->
		    %% io:format("~npretty_otp5068_msg1 -> ~n~p~n", [Else]),
		    exit({unexpected_decode_result, Else})
	    end
    end.

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
		asn1_NOVALUE}}]}}}]}]}}}]}}}.
  

pretty_otp5085_msg1(suite) ->
    [];
pretty_otp5085_msg1(Config) when is_list(Config) ->
    d("pretty_otp5085_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    pretty_otp5085(ok, pretty_otp5085_msg1()).

pretty_otp5085_msg2(suite) ->
    [];
pretty_otp5085_msg2(Config) when is_list(Config) ->
    d("pretty_otp5085_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    pretty_otp5085(error, pretty_otp5085_msg2()).

pretty_otp5085_msg3(suite) ->
    [];
pretty_otp5085_msg3(Config) when is_list(Config) ->
    d("pretty_otp5085_msg3 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    pretty_otp5085(ok, pretty_otp5085_msg3()).

pretty_otp5085_msg4(suite) ->
    [];
pretty_otp5085_msg4(Config) when is_list(Config) ->
    d("pretty_otp5085_msg4 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    pretty_otp5085(ok, pretty_otp5085_msg4()).

pretty_otp5085_msg5(suite) ->
    [];
pretty_otp5085_msg5(Config) when is_list(Config) ->
    d("pretty_otp5085_msg5 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    pretty_otp5085(ok, pretty_otp5085_msg5()).

pretty_otp5085_msg6(suite) ->
    [];
pretty_otp5085_msg6(Config) when is_list(Config) ->
    d("pretty_otp5085_msg6 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    pretty_otp5085(ok, pretty_otp5085_msg6()).

pretty_otp5085_msg7(suite) ->
    [];
pretty_otp5085_msg7(Config) when is_list(Config) ->
    d("pretty_otp5085_msg7 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    pretty_otp5085(ok, pretty_otp5085_msg7()).

pretty_otp5085(Expected, Msg) ->
    pretty_otp5085(Expected, Msg, []).

pretty_otp5085(Expected, Msg, Conf) ->
    t("pretty_otp5085 -> entry with"
      "~n   Expected: ~p"
      "~n   Msg:      ~p", [Expected, Msg]),
    case (catch encode_message(megaco_pretty_text_encoder, Conf, Msg)) of
	{error, Reason} when Expected =:= error ->
 	    d("pretty_otp5085 -> encode failed as expected"
	      "~n   Reason: ~w", [Reason]),
	    ok;
	{error, Reason} ->
 	    e("pretty_otp5085 -> encode failed unexpectedly: "
	      "~n   Reason: ~w", [Reason]),
	    exit({unexpected_encode_result, Reason});
	{ok, Bin} when Expected =:= error ->
 	    e("pretty_otp5085 -> encode succeded unexpectedly: "
	      "~n   ~w", [binary_to_list(Bin)]),
	    exit({unexpected_encode_result, binary_to_list(Bin)});
	{ok, Bin} ->
	    d("pretty_otp5085 -> successfull encode as expected:"
	      "~n~s", [binary_to_list(Bin)]),
	    case decode_message(megaco_pretty_text_encoder, false, Conf, Bin) of
		{ok, Msg} ->
 		    d("pretty_otp5085 -> successfull decode~n", []),
		    ok;
		{ok, Msg2} ->
 		    e("pretty_otp5085 -> successfull decode"
		      " - but not equal", []),
		    exit({unexpected_decode_result, Msg, Msg2});
		Else ->
 		    e("pretty_otp5085 -> decode failed:~n~p", [Else]),
		    exit({unexpected_decode_result, Else})
	    end
    end.

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
	  }
	 }
	}
       ]
      }
     }
    }.

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
	  }
	 }
	}
       ]
      }
     }
    }.

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
	  }
	 }
	}
       ]
      }
     }
    }.

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
	     [{addReply,    cre_ammsReply([#megaco_term_id{id = ?A4444}])}, 
	      {notifyReply, cre_notifyReply([#megaco_term_id{id = ?A5555}])}]
	    }
	   ]
	  }
	 }
	}
       ]
      }
     }
    }.

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
	     [{addReply,    cre_ammsReply([#megaco_term_id{id = ?A4444}])}, 
	      {notifyReply, cre_notifyReply([#megaco_term_id{id = ?A5555}])}]
	    }
	   ]
	  }
	 }
	}
       ]
      }
     }
    }.

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
	     [{addReply,    cre_ammsReply([#megaco_term_id{id = ?A4444}])}, 
	      {notifyReply, cre_notifyReply([#megaco_term_id{id = ?A5555}])}]
	    }
	   ]
	  }
	 }
	}
       ]
      }
     }
    }.

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
	     [{notifyReply, cre_notifyReply([#megaco_term_id{id = ?A5555}])}]
	    }
	   ]
	  }
	 }
	}
       ]
      }
     }
    }.


pretty_otp5600_msg1(suite) ->
    [];
pretty_otp5600_msg1(Config) when is_list(Config) ->
    d("pretty_otp5600_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    %%     put(severity,trc),
    %%     put(dbg,true),
    pretty_otp5600(ok, pretty_otp5600_msg1()).

pretty_otp5600_msg2(suite) ->
    [];
pretty_otp5600_msg2(Config) when is_list(Config) ->
    d("pretty_otp5600_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
%%     put(severity,trc),
%%     put(dbg,true),
    pretty_otp5600(ok, pretty_otp5600_msg2()).
 
pretty_otp5600(Expected, Msg) ->
    pretty_otp5600(Expected, Msg, []).

pretty_otp5600(Expected, Msg, Conf) ->
    t("pretty_otp5600 -> entry with"
      "~n   Expected: ~p"
      "~n   Msg:      ~p", [Expected, Msg]),
    case (catch encode_message(megaco_pretty_text_encoder, Conf, Msg)) of
        {error, Reason} when Expected =:= error ->
            d("pretty_otp5600 -> encode failed as expected"
              "~n   Reason: ~w", [Reason]),
            ok;
        {error, Reason} ->
            e("pretty_otp5600 -> encode failed unexpectedly: "
              "~n   Reason: ~w", [Reason]),
            exit({unexpected_encode_result, Reason});
        {ok, Bin} when Expected =:= error ->
            e("pretty_otp5600 -> encode succeded unexpectedly: "
              "~n   ~w", [binary_to_list(Bin)]),
            exit({unexpected_encode_result, binary_to_list(Bin)});
        {ok, Bin} ->
            d("pretty_otp5600 -> successfull encode as expected:"
              "~n~s", [binary_to_list(Bin)]),
            case decode_message(megaco_pretty_text_encoder, false, Conf, Bin) of
                {ok, Msg} ->
                    d("pretty_otp5600 -> successfull decode~n", []),
                    ok;
                {ok, Msg2} ->
                    e("pretty_otp5600 -> successfull decode"
                      " - but not equal", []),
                    exit({unexpected_decode_result, Msg, Msg2});
                Else ->
                    e("pretty_otp5600 -> decode failed:~n~p", [Else]),
                    exit({unexpected_decode_result, Else})
            end
    end.

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


pretty_otp5601_msg1(suite) ->
    [];
pretty_otp5601_msg1(Config) when is_list(Config) ->
    d("pretty_otp5601_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    %% put(severity,trc),
    %% put(dbg,true),
    pretty_otp5601(ok, pretty_otp5601_msg1()).

pretty_otp5601(Expected, Msg) ->
    pretty_otp5601(Expected, Msg, []).

pretty_otp5601(Expected, Msg, Conf) ->
    t("pretty_otp5601 -> entry with"
      "~n   Expected: ~p"
      "~n   Msg:      ~p", [Expected, Msg]),
    case (catch encode_message(megaco_pretty_text_encoder, Conf, Msg)) of
	{error, Reason} when Expected =:= error ->
 	    d("pretty_otp5601 -> encode failed as expected"
	      "~n   Reason: ~w", [Reason]),
	    ok;
	{error, Reason} ->
 	    e("pretty_otp5601 -> encode failed unexpectedly: "
	      "~n   Reason: ~w", [Reason]),
	    exit({unexpected_encode_result, Reason});
	{ok, Bin} when Expected =:= error ->
 	    e("pretty_otp5601 -> encode succeded unexpectedly: "
	      "~n   ~w", [binary_to_list(Bin)]),
	    exit({unexpected_encode_result, binary_to_list(Bin)});
	{ok, Bin} ->
	    d("pretty_otp5601 -> successfull encode as expected:"
	      "~n~s", [binary_to_list(Bin)]),
	    case decode_message(megaco_pretty_text_encoder, false, Conf, Bin) of
		{ok, Msg} ->
 		    d("pretty_otp5601 -> successfull decode~n", []),
		    ok;
		{ok, Msg2} ->
 		    e("pretty_otp5601 -> successfull decode"
		      " - but not equal", []),
		    exit({unexpected_decode_result, Msg, Msg2});
		Else ->
 		    e("pretty_otp5601 -> decode failed:~n~p", [Else]),
		    exit({unexpected_decode_result, Else})
	    end
    end.

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

pretty_otp5793_msg01(suite) ->
    [];
pretty_otp5793_msg01(Config) when is_list(Config) ->
    d("pretty_otp5793_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
%     put(severity,trc),
%     put(dbg,true),
    pretty_otp5793(ok, pretty_otp5793_msg1()).

pretty_otp5793(Expected, Msg) ->
    expect_codec(Expected, megaco_pretty_text_encoder, Msg, []).

pretty_otp5793(Expected, Msg, Conf) ->
    expect_codec(Expected, megaco_pretty_text_encoder, Msg, Conf).


pretty_otp5793_msg1() ->
    {'MegacoMessage',asn1_NOVALUE,
     {'Message',2,
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
		   "19"]}]}}]}]}}}]}}}.


pretty_otp5882_msg01(suite) ->
    [];
pretty_otp5882_msg01(Config) when is_list(Config) ->
    d("pretty_otp5882_msg01 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    %% put(severity,trc),
    %% put(dbg,true),
    pretty_otp5882().

pretty_otp5882() ->	
    otp5882(megaco_pretty_text_encoder, []).

otp5882(Codec, Conf) ->		 
    Msg  = pretty_otp5882_msg01(),
    case (catch encode_message(Codec, Conf, Msg)) of
	{error, {message_encode_failed, {error, {ActualReason, _}}, _}} ->
	    case ActualReason of
		{invalid_LocalControlDescriptor, empty} ->
		    ok;
		_ ->
		    exit({unexpected_error_actual_reason, ActualReason})
	    end;
	{error, Reason} ->
	    exit({unexpected_error_reason, Reason});
	{ok, Bin} ->
	    exit({unexpected_encode_sucess, binary_to_list(Bin)})
    end.
    
pretty_otp5882_msg01() ->
    LCD = #'LocalControlDescriptor'{}, % Create illegal LCD
    Parms      = cre_streamParms(LCD),
    StreamDesc = cre_streamDesc(1, Parms),
    MediaDesc  = cre_mediaDesc(StreamDesc),
    AmmReq     = cre_ammReq([#megaco_term_id{id = ?A4445}],
			    [{mediaDescriptor, MediaDesc}]),
    CmdReq     = cre_commandReq({modReq, AmmReq}),
    ActReq     = #'ActionRequest'{contextId       = 5882,
				  commandRequests = [CmdReq]}, 
    Actions    = [ActReq],
    TransReq   = #'TransactionRequest'{transactionId = 5882,
				       actions       = Actions}, 
    Trans      = {transactionRequest, TransReq}, 
    Body       = {transactions, [Trans]},
    Mid        = ?MG1_MID,
    megaco_message(?VERSION, Mid, Body).
    

%% --------------------------------------------------------------
%%
pretty_otp6490_msg01(suite) ->
    [];
pretty_otp6490_msg01(Config) when is_list(Config) ->
    %% put(severity, trc),
    %% put(dbg,      true),
    d("pretty_otp6490_msg01 -> entry", []),
    %% ?ACQUIRE_NODES(1, Config),
    ok = pretty_otp6490( pretty_otp6490_msg01(), [] ),
    %% erase(dbg),
    %% erase(severity),
    ok.

pretty_otp6490_msg02(suite) ->
    [];
pretty_otp6490_msg02(Config) when is_list(Config) ->
    %% put(severity, trc),
    %% put(dbg,      true),
    d("pretty_otp6490_msg02 -> entry", []),
    %% ?ACQUIRE_NODES(1, Config),
    ok = pretty_otp6490( pretty_otp6490_msg02(), [] ),
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
    ok = pretty_otp6490( pretty_otp6490_msg03(), [] ),
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
    ok = pretty_otp6490( pretty_otp6490_msg04(), [] ),
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
    ok = pretty_otp6490( pretty_otp6490_msg05(), [] ),
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
    ok = pretty_otp6490( pretty_otp6490_msg06(), [] ),
    %% erase(severity),
    %% erase(dbg),
    ok.

pretty_otp6490(Msg, Conf) ->
    pretty_otp6490(Msg, Conf, ok).

pretty_otp6490(Msg, Conf, ExpectedEncode) ->
    pretty_otp6490(Msg, Conf, ExpectedEncode, ok).

pretty_otp6490(Msg, Conf, ExpectedEncode, ExpectedDecode) ->
    otp6490(Msg, megaco_pretty_text_encoder, Conf,
            ExpectedEncode, ExpectedDecode).

otp6490(Msg, Codec, Conf, ExpectedEncode, ExpectedDecode) ->
    case (catch encode_message(Codec, Conf, Msg)) of
        {error, _Reason} when ExpectedEncode =:= error ->
            ok;
        {error, Reason} when ExpectedEncode =:= ok ->
            exit({unexpected_encode_failure, Reason});
        {ok, Bin} when ExpectedEncode =:= error ->
            exit({unexpected_encode_success, Msg, binary_to_list(Bin)});
        {ok, Bin} when ExpectedEncode =:= ok ->
            case decode_message(Codec, false, Conf, Bin) of
                {ok, Msg} when ExpectedDecode == ok ->
                    ok;
                {ok, Msg} when ExpectedDecode =:= error ->
                    exit({unexpected_decode_success, Msg});
                {ok, Msg2} when ExpectedDecode =:= ok ->
                    exit({unexpected_decode_result, Msg, Msg2});
                {ok, Msg2} when ExpectedDecode =:= error ->
                    exit({unexpected_decode_success, Msg, Msg2});
                {error, _Reason} when ExpectedDecode =:= error ->
                    ok;
                {error, Reason} when ExpectedDecode =:= ok ->
                    exit({unexpected_decode_failure, Msg, Reason})
            end
    end.


pretty_otp6490_msg(EBD) ->
    AmmDesc    = ?MSG_LIB:cre_AmmDescriptor(EBD),
    AmmReq     = cre_ammReq([#megaco_term_id{id = ?A4445}], [AmmDesc]),
    CmdReq     = cre_commandReq({modReq, AmmReq}),
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
		    io:format("otp7671 -> decoded:identical~n", []),
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
    AmmReq = cre_ammReq([#megaco_term_id{id = ?A4444}],
			[{digitMapDescriptor, DigitMapDesc}]),
    CmdReq = cre_commandReq({modReq, AmmReq}),
    msg_request(?MGC_MID, 10001, ?megaco_null_context_id, [CmdReq]).

pretty_otp7671_msg01() ->
    Name         = "dialplan01",
    DigitMapDesc = cre_digitMapDesc(Name),
    pretty_otp7671_msg(DigitMapDesc).

pretty_otp7671_msg02() ->
    Name         = "dialplan02",
    Body         = "(0s| 00s|[1-7]xlxx|8lxxxxxxx|#xxxxxxx|*xx|9l1xxxxxxxxxx|9l011x.s)",
    Value        = cre_digitMapValue(Body),
    DigitMapDesc = cre_digitMapDesc(Name, Value),
    pretty_otp7671_msg(DigitMapDesc).

pretty_otp7671_msg03() ->
    Body         = "(0s| 00s|[1-7]xlxx|8lxxxxxxx|#xxxxxxx|*xx|9l1xxxxxxxxxx|9l011x.s)",
    Value        = cre_digitMapValue(Body),
    DigitMapDesc = cre_digitMapDesc(Value),
    pretty_otp7671_msg(DigitMapDesc).

pretty_otp7671_msg04() ->
    DigitMapDesc = cre_digitMapDesc(asn1_NOVALUE, asn1_NOVALUE),
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
		   {'DigitMapValue',asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,[]}}}]}},
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
		     digitMapBody  = []} = Value1,
    asn1_NOVALUE = Value2,
    ok.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expect_codec(Expect, Codec, Msg, Conf) ->
    t("expect_codec -> entry with"
      "~n   Expect: ~p"
      "~n   Msg:    ~p", [Expect, Msg]),
    case (catch encode_message(Codec, Conf, Msg)) of
	{error, _Reason} when Expect =:= error ->
 	    d("expect_codec -> encode failed as expected"
	      "~n   _Reason: ~w", [_Reason]),
	    ok;
	{error, Reason} ->
 	    e("expect_codec -> encode failed unexpectedly: "
	      "~n   Reason: ~w", [Reason]),
	    exit({unexpected_encode_result, Reason});
	{ok, Bin} when Expect =:= error ->
 	    e("expect_codec -> encode succeded unexpectedly: "
	      "~n   ~w", [binary_to_list(Bin)]),
	    exit({unexpected_encode_result, binary_to_list(Bin)});
	{ok, Bin} ->
	    d("expect_codec -> successfull encode as expected:"
	      "~n~s", [binary_to_list(Bin)]),
	    case (catch decode_message(Codec, false, Conf, Bin)) of
		{ok, Msg} ->
 		    d("expect_codec -> successfull decode~n", []),
		    ok;
		{ok, Msg2} ->
 		    e("expect_codec -> successfull decode"
		      " - but not equal", []),
		    chk_MegacoMessage(Msg, Msg2);
		%% exit({unexpected_decode_result, Msg, Msg2});
		Else ->
 		    e("expect_codec -> decode failed:~n~p", [Else]),
		    exit({unexpected_decode_result, Else})
	    end;
	Else ->
	    e("expect_codec -> encode failed:~n~p", [Else]),
	    exit({unexpected_encode_result, Else})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

msgs() ->
    Msgs = msgs1() ++ msgs2() ++ msgs3(),
    [M || {_, M, _, _} <- Msgs].

msgs1() ->
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
    
msgs2() ->
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


msgs3() ->
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
    case (catch decode_message(megaco_pretty_text_encoder, false, [], M)) of
	{ok, Msg} ->
	    Msg;
	Error ->
	    {error, {rfc3525_decode_error, Error}}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

msg_actions([], Actions) ->
    lists:reverse(Actions);
msg_actions([{CtxId, CmdReqs}|ActionInfo], Actions) ->
    Action = #'ActionRequest'{contextId       = CtxId,
			      commandRequests = CmdReqs},
    msg_actions(ActionInfo, [Action|Actions]).

megaco_trans_req([], Transactions) ->
    {transactions, lists:reverse(Transactions)};
megaco_trans_req([{TransId, ActionInfo}|TransInfo], Transactions) ->
    Actions  = msg_actions(ActionInfo, []),
    TransReq = {transactionRequest,
		#'TransactionRequest'{transactionId = TransId,
				      actions       = Actions}},
    megaco_trans_req(TransInfo, [TransReq|Transactions]).

megaco_message(Version, Mid, Body) ->
    #'MegacoMessage'{mess = #'Message'{version     = Version,
                                       mId         = Mid,
                                       messageBody = Body}}.

megaco_message(Auth, Version, Mid, Body) ->
    #'MegacoMessage'{authHeader = Auth,
		     mess = #'Message'{version     = Version,
                                       mId         = Mid,
                                       messageBody = Body}}.

msg_request(Mid, TransInfo) ->
    TransReq = megaco_trans_req(TransInfo, []),
    megaco_message(1, Mid, TransReq).

msg_request(Mid, TransId, ContextId, CmdReq) when is_list(CmdReq) ->
    Actions = [#'ActionRequest'{contextId = ContextId,
                                commandRequests = CmdReq}],
    Req = {transactions,
           [{transactionRequest,
             #'TransactionRequest'{transactionId = TransId,
                                   actions = Actions}}]},
    megaco_message(?VERSION, Mid, Req).

msg_request(Auth, Mid, TransId, ContextId, CmdReq) when is_list(CmdReq) ->
    Actions = [#'ActionRequest'{contextId = ContextId,
                                commandRequests = CmdReq}],
    Req = {transactions,
           [{transactionRequest,
             #'TransactionRequest'{transactionId = TransId,
                                   actions = Actions}}]},
    megaco_message(Auth, ?VERSION, Mid, Req).

msg_reply(Mid, TransId, ContextId, CmdReply) when is_list(CmdReply) ->
    ReplyData = [{ContextId, CmdReply}], 
    msg_replies(Mid, TransId, ReplyData).

msg_replies(Mid, TransId, ReplyData) when is_list(ReplyData) ->
    Actions = [cre_actionReply(ContextId, CmdReply) || 
		  {ContextId, CmdReply} <- ReplyData],
    Req = {transactions,
           [{transactionReply, cre_transactionReply(TransId, Actions)}]},
    cre_megacoMessage(?VERSION, Mid, Req).


msg_ack(Mid, [Range|_] = Ranges) when is_tuple(Range) ->
    msg_ack(Mid, [Ranges]);

msg_ack(Mid, Ranges) ->
    %% TRAs = make_tras(Ranges, []),
    TRAs = make_tras(Ranges),
    Req  = {transactions, TRAs},
    cre_megacoMessage(?VERSION, Mid, Req).

make_tras(TRARanges) ->
    F = fun(R) -> {transactionResponseAck, make_tra(R)} end,
    lists:map(F, TRARanges).

make_tra(Ranges) ->
    F = fun({F,L}) -> cre_transactionAck(F,L) end,
    lists:map(F, Ranges).


%% -------------------------------------------------------------------------


msg1(Mid, Tid) ->
    Gain  = cre_propertyParm("tdmc/gain", "2"),
    Ec    = cre_propertyParm("tdmc/ec", "g165"), 
    LCD   = cre_localControlDesc(sendRecv,[Gain, Ec]),
    V     = cre_propertyParm("v", "0"),
    %% C    = cre_propertyParm("c", "IN IP4 $ "),
    C     = cre_propertyParm("c", [$I,$N,$ ,$I,$P,$4,$ ,$$,$ ]),
    M     = cre_propertyParm("m", "audio $ RTP/AVP 0"),
    A     = cre_propertyParm("a", "fmtp:PCMU VAD=X-NNVAD"),
    LD    = cre_localRemoteDesc([[V, C, M, A]]),
    Parms = cre_streamParms(LCD,LD),
    StreamDesc = cre_streamDesc(1,Parms),
    MediaDesc  = cre_mediaDesc(StreamDesc),
    ReqEvent   = cre_requestedEvent("al/of"),
    EventsDesc = cre_eventsDesc(2222,[ReqEvent]),
    AmmReq     = cre_ammReq([#megaco_term_id{id = Tid}],
			    [{mediaDescriptor, MediaDesc},
			     {eventsDescriptor, EventsDesc}]),
    CmdReq     = cre_commandReq({modReq, AmmReq}),
    msg_request(Mid, 9999, ?megaco_null_context_id, [CmdReq]).


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
    Gain  = cre_propertyParm("tdmc/gain", "2"),
    Ec    = cre_propertyParm("tdmc/ec", "g165"), 
    LCD   = cre_localControlDesc(sendRecv,[Gain, Ec]),
    V     = cre_propertyParm("v", "0"),
    %% C    = cre_propertyParm("c", "IN IP4 $ "),
    C     = cre_propertyParm("c", [$I,$N,$ ,$I,$P,$4,$ ,$$,$ ]),
    M     = cre_propertyParm("m", "audio $ RTP/AVP 0"),
    A     = cre_propertyParm("a", "fmtp:PCMU VAD=X-NNVAD"),
    LD    = cre_localRemoteDesc([[V, C, M, A]]),
    Parms = cre_streamParms(LCD,LD),
    StreamDesc = cre_streamDesc(1,Parms),
    MediaDesc  = cre_mediaDesc(StreamDesc),
    EventParm  = cre_eventParm("strict",["exact"]),
    ReqEvent   = cre_requestedEvent("al/of", [EventParm]),
    EventsDesc = cre_eventsDesc(2222,[ReqEvent]),
    AmmReq     = cre_ammReq([#megaco_term_id{id = Tid}],
			    [{mediaDescriptor, MediaDesc},
			     {eventsDescriptor, EventsDesc}]),
    CmdReq     = cre_commandReq({modReq, AmmReq}),
    msg_request(Mid, 9999, ?megaco_null_context_id, [CmdReq]).


%% --------------------------

msg3() ->
    msg3(?MG1_MID).
msg3(Mid) ->
    TimeStamp = cre_timeNotation("19990729", "22000000"),
    Event     = cre_observedEvent("al/of",TimeStamp),
    Desc      = cre_observedEventsDesc(2222,[Event]),
    NotifyReq = cre_notifyReq([#megaco_term_id{id = ?A4444}],Desc),
    CmdReq    = cre_commandReq({notifyReq, NotifyReq}),
    msg_request(Mid, 10000, ?megaco_null_context_id, [CmdReq]).


%% --------------------------

msg4() ->
    msg4(?MG1_MID_NO_PORT, "901 mg col boot").
msg4(Mid, Reason) when is_list(Reason) ->
    Address = {portNumber, ?DEFAULT_PORT},
    Profile = cre_serviceChangeProf("resgw",1),
    Parm    = cre_serviceChangeParm(restart,Address,[Reason],Profile),
    Req     = cre_serviceChangeReq([?megaco_root_termination_id],Parm),
    CmdReq  = cre_commandReq({serviceChangeReq, Req}),
    msg_request(Mid, 9998, ?megaco_null_context_id, [CmdReq]).


%% --------------------------

msg5() ->
    msg5(?MGC_MID).
msg5(Mid) ->
    Address = {portNumber, ?DEFAULT_PORT},
    Profile = cre_serviceChangeProf("resgw",1),
    Parm    = cre_serviceChangeResParm(Address,Profile),
    Reply   = cre_serviceChangeReply([?megaco_root_termination_id],
				     {serviceChangeResParms,Parm}),
    msg_reply(Mid, 9998, ?megaco_null_context_id,
	      [{serviceChangeReply, Reply}]).


%% --------------------------

msg6(Mid, Tid) ->
    Reply = cre_ammsReply([#megaco_term_id{id = Tid}]),
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
    Reply = cre_notifyReply([#megaco_term_id{id = ?A4444}]),
    msg_reply(Mid, 10000, ?megaco_null_context_id, [{notifyReply, Reply}]).


%% --------------------------

msg8(Mid, DigitMapValue) ->
    Strict = cre_eventParm("strict",["state"]),
    On     = cre_requestedEvent("al/on", [Strict]),
    Name   = "dialplan00",
    Action = cre_requestedActions(Name),
    Ce     = cre_requestedEvent("dd/ce", Action),
    EventsDesc = cre_eventsDesc(2223,[On, Ce]),
    Signal     = cre_signal("cg/rt"),
    DigMapDesc = cre_digitMapDesc(Name, DigitMapValue),
    AmmReq     = cre_ammReq([#megaco_term_id{id = ?A4444}],
                           [{eventsDescriptor, EventsDesc},
			    {signalsDescriptor, [{signal, Signal}]},
			    {digitMapDescriptor, DigMapDesc}]),
    CmdReq     = cre_commandReq({modReq, AmmReq}),
    msg_request(Mid, 10001, ?megaco_null_context_id, [CmdReq]).

msg8a() ->
    msg8a(?MGC_MID).
msg8a(Mid) ->
    Body = "(0s| 00s|[1-7]xlxx|8lxxxxxxx|#xxxxxxx|*xx|9l1xxxxxxxxxx|9l011x.s)",
    Value = cre_digitMapValue(Body),
    msg8(Mid, Value).

msg8b() ->
    msg8b(?MGC_MID).
msg8b(Mid) ->
    Body = "(0s| 00s|[1-7]xlxx|8lxxxxxxx|#xxxxxxx|*xx|9l1xxxxxxxxxx|9l011x.s)",
    Value = cre_digitMapValue(Body, 1, 23, 99),
    msg8(Mid, Value).


%% --------------------------

msg9() ->
    msg9(?MG1_MID).
msg9(Mid) ->
    TimeStamp = cre_timeNotation("19990729","22010001"),
    Parm      = cre_eventParm("ds",["916135551212"]),
    Event     = cre_observedEvent("dd/ce",TimeStamp,[Parm]),
    Desc      = cre_observedEventsDesc(2223,[Event]),
    NotifyReq = cre_notifyReq([#megaco_term_id{id = ?A4444}], Desc),
    CmdReq    = cre_commandReq({notifyReq, NotifyReq}),
    msg_request(Mid, 10002, ?megaco_null_context_id, [CmdReq]).


%% --------------------------

msg10() ->
    msg10(?MGC_MID).
msg10(Mid) ->
    AmmReq = cre_ammReq([#megaco_term_id{id = ?A4444}],[]),
    CmdReq = cre_commandReq({addReq, AmmReq}),
    Jit = cre_propertyParm("nt/jit", "40"),
    LCD = cre_localControlDesc(recvOnly,[Jit]),
    V   = cre_propertyParm("v", "0"),
    C   = cre_propertyParm("c", "IN IP4 $ "),
    M   = cre_propertyParm("m", "audio $ RTP/AVP 4"),
    A   = cre_propertyParm("a", "ptime:30"),
    V2  = cre_propertyParm("v", "0"),
    C2  = cre_propertyParm("c", "IN IP4 $ "),
    M2  = cre_propertyParm("m", "audio $ RTP/AVP 0"),
    LD  = cre_localRemoteDesc([[V, C, M, A], [V2, C2, M2]]),
    Parms      = cre_streamParms(LCD, LD),
    StreamDesc = cre_streamDesc(1,Parms),
    MediaDesc  = cre_mediaDesc(StreamDesc),
    ChooseTid  = #megaco_term_id{contains_wildcards = true,
				 id = [[?megaco_choose]]},
    AmmReq2    = cre_ammReq([ChooseTid],[{mediaDescriptor, MediaDesc}]),
    CmdReq2    = cre_commandReq({addReq, AmmReq2}),
    msg_request(Mid, 10003, ?megaco_choose_context_id, [CmdReq, CmdReq2]).


msg11() ->
    msg11(?MG1_MID).
msg11(Mid) ->
    V  = cre_propertyParm("v", "0"),
    C  = cre_propertyParm("c", "IN IP4 124.124.124.222"),
    M  = cre_propertyParm("m", "audio 2222 RTP/AVP 4"),
    A  = cre_propertyParm("a", "ptime:30"),
    A2 = cre_propertyParm("a", "recvonly"),
    LD = cre_localRemoteDesc([[V, C, M, A, A2]]),
    Parms      = cre_streamParmsL(LD),
    StreamDesc = cre_streamDesc(1, Parms),
    MediaDesc  = cre_mediaDesc(StreamDesc),
    Reply  = cre_ammsReply([#megaco_term_id{id = ?A4444}]),
    Reply2 = cre_ammsReply([#megaco_term_id{id = ?A4445}],
			   [{mediaDescriptor, MediaDesc}]),
    msg_reply(Mid, 10003, 2000, [{addReply, Reply}, {addReply, Reply2}]).


%% --------------------------

msg12() ->
    msg12(?MGC_MID).
msg12(Mid) ->
    LCD        = cre_localControlDesc(sendRecv),
    Parms      = cre_streamParms(LCD),
    StreamDesc = cre_streamDesc(1,Parms),
    MediaDesc  = cre_mediaDesc(StreamDesc),
    Signal     = cre_signal("al/ri"),
    Descs      = [{mediaDescriptor, MediaDesc},
		  {signalsDescriptor, [{signal, Signal}]}],
    AmmReq     = cre_ammReq([#megaco_term_id{id = ?A5555}], Descs),
    CmdReq     = cre_commandReq({addReq, AmmReq}),
    Jit        = cre_propertyParm("nt/jit", "40"),
    LCD2       = cre_localControlDesc(sendRecv, [Jit]),
    V      = cre_propertyParm("v", "0"),
    C      = cre_propertyParm("c", "IN IP4 $ "),
    M      = cre_propertyParm("m", "audio $ RTP/AVP 4"),
    A      = cre_propertyParm("a", "ptime:30"),
    LD2    = cre_localRemoteDesc([[V, C, M, A]]),
    V2     = cre_propertyParm("v", "0"),
    C2     = cre_propertyParm("c", "IN IP4 124.124.124.222"),
    M2     = cre_propertyParm("m", "audio 2222 RTP/AVP 4"),
    RD2    = cre_localRemoteDesc([[V2, C2, M2]]),
    Parms2 = cre_streamParms(LCD2,LD2,RD2),
    StreamDesc2 = cre_streamDesc(1,Parms2),
    MediaDesc2  = cre_mediaDesc(StreamDesc2),
    ChooseTid   = #megaco_term_id{contains_wildcards = true,
				  id = [[?megaco_choose]]},
    AmmReq2     = cre_ammReq([ChooseTid],[{mediaDescriptor, MediaDesc2}]),
    CmdReq2     = cre_commandReq({addReq, AmmReq2}),
    msg_request(Mid, 50003, ?megaco_choose_context_id, [CmdReq, CmdReq2]).


%% --------------------------

msg13() ->
    msg13(?MG2_MID).
msg13(Mid) ->
    V     = cre_propertyParm("v", "0"),
    C     = cre_propertyParm("c", "IN IP4 125.125.125.111"),
    M     = cre_propertyParm("m", "audio 1111 RTP/AVP 4"),
    LD    = cre_localRemoteDesc([[V, C, M]]),
    Parms = cre_streamParmsL(LD),
    StreamDesc = cre_streamDesc(1,Parms),
    MediaDesc  = cre_mediaDesc(StreamDesc),
    Reply      = cre_ammsReply([#megaco_term_id{id = ?A5556}],
			       [{mediaDescriptor, MediaDesc}]),
    msg_reply(Mid, 50003, 5000, [{addReply, Reply}]).


%% --------------------------

msg14() ->
    msg14(?MGC_MID).
msg14(Mid) ->
    Signal  = cre_signal("cg/rt"), 
    AmmReq1 = cre_ammReq([#megaco_term_id{id = ?A4444}],
			 [{signalsDescriptor, [{signal, Signal}]}]),
    CmdReq1 = cre_commandReq({modReq, AmmReq1}),

    Gain    = cre_propertyParm("tdmc/gain", "2"),
    Ec      = cre_propertyParm("tdmc/ec", "g165"),
    LCD     = cre_localControlDesc(sendRecv, [Gain, Ec]),
    Parms2  = cre_streamParms(LCD),
    StreamDesc2 = cre_streamDesc(1,Parms2),
    MediaDesc2  = cre_mediaDesc(StreamDesc2),
    AmmReq2     = cre_ammReq([#megaco_term_id{id = ?A4445}],
			     [{mediaDescriptor, MediaDesc2}]),
    CmdReq2     = cre_commandReq({modReq, AmmReq2}),

    V      = cre_propertyParm("v", "0"),
    C      = cre_propertyParm("c", "IN IP4 125.125.125.111"),
    M      = cre_propertyParm("m", "audio 1111 RTP/AVP 4"),
    RD     = cre_localRemoteDesc([[V, C, M]]),
    Parms3 = cre_streamParmsR(RD),
    StreamDesc3 = cre_streamDesc(1,Parms3),
    MediaDesc3  = cre_mediaDesc(StreamDesc3),
    AmmReq3     = cre_ammReq([#megaco_term_id{id = ?A4445}],
			     [{mediaDescriptor, MediaDesc3}]),
    CmdReq3     = cre_commandReq({modReq, AmmReq3}),
    msg_request(Mid, 10005, 2000, [CmdReq1, CmdReq2, CmdReq3]).


%% --------------------------

msg15() ->
    msg15(?MG1_MID).
msg15(Mid) ->
    Reply  = cre_ammsReply([#megaco_term_id{id = ?A4444}]),
    Reply2 = cre_ammsReply([#megaco_term_id{id = ?A4445}]),
    msg_reply(Mid, 10005, 2000, [{modReply, Reply}, {modReply, Reply2}]).


msg15b() ->
    msg15b(?MG1_MID).

msg15b(Mid) ->
    %% We reuse the amms reply stuff
    Reply1 = cre_ammsReply([#megaco_term_id{id = ?A4444}]),
    Reply2 = cre_ammsReply([#megaco_term_id{id = ?A4445}]),
    ActionReplyData = 
	[{modReply, Reply1}, {modReply, Reply2}],
    ReplyData = 
	[{2001, ActionReplyData},
	 {2002, ActionReplyData},
	 {2003, ActionReplyData},
	 {2004, ActionReplyData},
	 {2005, ActionReplyData},
	 {2006, ActionReplyData},
	 {2007, ActionReplyData},
	 {2008, ActionReplyData},
	 {2009, ActionReplyData},
	 {2010, ActionReplyData},
	 {2011, ActionReplyData},
	 {2012, ActionReplyData},
	 {2013, ActionReplyData},
	 {2014, ActionReplyData},
	 {2015, ActionReplyData},
	 {2016, ActionReplyData},
	 {2017, ActionReplyData},
	 {2018, ActionReplyData},
	 {2019, ActionReplyData},
	 {2020, ActionReplyData},
	 {2021, ActionReplyData},
	 {2022, ActionReplyData},
	 {2023, ActionReplyData},
	 {2024, ActionReplyData},
	 {2025, ActionReplyData},
	 {2026, ActionReplyData},
	 {2027, ActionReplyData},
	 {2028, ActionReplyData},
	 {2029, ActionReplyData},
	 {2030, ActionReplyData},
	 {2031, ActionReplyData}],
     msg_replies(Mid, 10005, ReplyData).
	 

%% --------------------------

msg16() ->
    msg16(?MG2_MID).
msg16(Mid) ->
    TimeStamp = cre_timeNotation("19990729","22020002"),
    Event     = cre_observedEvent("al/of",TimeStamp),
    Desc      = cre_observedEventsDesc(1234,[Event]),
    NotifyReq = cre_notifyReq([#megaco_term_id{id = ?A5555}],Desc),
    CmdReq    = cre_commandReq({notifyReq, NotifyReq}),
    msg_request(Mid, 50005, 5000, [CmdReq]).


%% --------------------------

msg17() ->
    msg17(?MGC_MID).
msg17(Mid) ->
    Reply = cre_notifyReply([#megaco_term_id{id = ?A5555}]),
    msg_reply(Mid, 50005, ?megaco_null_context_id, [{notifyReply, Reply}]).


%% --------------------------

msg18() ->
    msg18(?MGC_MID).
msg18(Mid) ->
    On         = cre_requestedEvent("al/on"),
    EventsDesc = cre_eventsDesc(1235,[On]),
    AmmReq     = cre_ammReq([#megaco_term_id{id = ?A5555}],
			    [{eventsDescriptor, EventsDesc},
			     {signalsDescriptor, []}]),
    CmdReq     = cre_commandReq({modReq, AmmReq}),
    msg_request(Mid, 50006, 5000, [CmdReq]).


%% --------------------------

msg19() ->
    msg19(?MG2_MID).
msg19(Mid) ->
    Reply = cre_ammsReply([#megaco_term_id{id = ?A4445}]),
    msg_reply(Mid, 50006, 5000, [{modReply, Reply}]).


%% --------------------------

msg20() ->
    msg20(?MGC_MID).
msg20(Mid) ->
    LCD        = cre_localControlDesc(sendRecv),
    Parms      = cre_streamParms(LCD),
    StreamDesc = cre_streamDesc(1,Parms),
    MediaDesc  = cre_mediaDesc(StreamDesc),
    AmmReq     = cre_ammReq([#megaco_term_id{id = ?A4445}],
			    [{mediaDescriptor, MediaDesc}]),
    CmdReq     = cre_commandReq({modReq, AmmReq}),
    AmmReq2    = cre_ammReq([#megaco_term_id{id = ?A4444}],
                            [{signalsDescriptor, []}]),
    CmdReq2    = cre_commandReq({modReq, AmmReq2}),
    msg_request(Mid, 10006, 2000, [CmdReq, CmdReq2]).


%% --------------------------

msg21() ->
    msg21(?MGC_MID).
msg21(Mid) ->
    Tokens    = [mediaToken, eventsToken, signalsToken,
		 digitMapToken, statsToken, packagesToken],
    AuditDesc = cre_auditDesc(Tokens),
    Req       = cre_auditReq(#megaco_term_id{id = ?A5556},AuditDesc),
    CmdReq    = cre_commandReq({auditValueRequest, Req}),
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
    Jit = cre_propertyParm("nt/jit", "40"),
    LCD = cre_localControlDesc(sendRecv,[Jit]),
    LDV = cre_propertyParm("v", "0"),
    LDC = cre_propertyParm("c", "IN IP4 125.125.125.111"),
    LDM = cre_propertyParm("m", "audio 1111 RTP/AVP  4"),
    LDA = cre_propertyParm("a", "ptime:30"),
    LD  = cre_localRemoteDesc([[LDV, LDC, LDM, LDA]]),
    RDV = cre_propertyParm("v", "0"),
    RDC = cre_propertyParm("c", "IN IP4 124.124.124.222"),
    RDM = cre_propertyParm("m", "audio 2222 RTP/AVP  4"),
    RDA = cre_propertyParm("a", "ptime:30"),
    RD  = cre_localRemoteDesc([[RDV, RDC, RDM, RDA]]),
    StreamParms   = cre_streamParms(LCD,LD,RD),
    StreamDesc    = cre_streamDesc(1,StreamParms),
    Media         = cre_mediaDesc(StreamDesc),
    PackagesItem  = cre_packagesItem("nt",1),
    PackagesItem2 = cre_packagesItem("rtp",1),
    Stat       = cre_statisticsParm("rtp/ps","1200"),
    Stat2      = cre_statisticsParm("nt/os","62300"),
    Stat3      = cre_statisticsParm("rtp/pr","700"),
    Stat4      = cre_statisticsParm("nt/or","45100"),
    Stat5      = cre_statisticsParm("rtp/pl","0.2"),
    Stat6      = cre_statisticsParm("rtp/jit","20"),
    Stat7      = cre_statisticsParm("rtp/delay","40"),
    Statistics = [Stat, Stat2, Stat3, Stat4, Stat5, Stat6, Stat7],
    Audits     = [{mediaDescriptor, Media},
		  {packagesDescriptor, [PackagesItem, PackagesItem2]},
		  {statisticsDescriptor, Statistics}],
    Reply      = {auditResult, 
		  cre_auditRes(#megaco_term_id{id = ?A5556},Audits)},
    msg_reply(Mid, 50007, ?megaco_null_context_id, 
	      lists:duplicate(N,{auditValueReply, Reply})).
%%     msg_reply(Mid, 50007, ?megaco_null_context_id, 
%% 	      lists.duplicate([{auditValueReply, Reply}]).


%% --------------------------

msg23a() ->
    msg23a(?MG2_MID).
msg23a(Mid) ->
    TimeStamp = cre_timeNotation("19990729","24020002"),
    Event     = cre_observedEvent("al/on",TimeStamp),
    Desc      = cre_observedEventsDesc(1235,[Event]),
    NotifyReq = cre_notifyReq([#megaco_term_id{id = ?A5555}],Desc),
    CmdReq    = cre_commandReq({notifyReq, NotifyReq}),
    msg_request(Mid, 50008, 5000, [CmdReq]).


msg23b() ->
    msg23b(?MG2_MID).
msg23b(Mid) ->
    TimeStamp  = cre_timeNotation("19990729","24020002"),
    Event      = cre_observedEvent("al/on",TimeStamp),
    Desc       = cre_observedEventsDesc(1235,[Event]),
    NotifyReq1 = cre_notifyReq([#megaco_term_id{id = ?A5555}],Desc),
    CmdReq1    = cre_commandReq({notifyReq, NotifyReq1}),
    NotifyReq2 = cre_notifyReq([#megaco_term_id{id = ?A5556}],Desc),
    CmdReq2    = cre_commandReq({notifyReq, NotifyReq2}),
    ActionInfo = [{5000, [CmdReq1]}, {5001, [CmdReq2]}],
    TransInfo  = [{50008, ActionInfo}],
    msg_request(Mid, TransInfo).


msg23c() ->
    msg23c(?MG2_MID).
msg23c(Mid) ->
    TimeStamp  = cre_timeNotation("19990729","24020002"),
    Event      = cre_observedEvent("al/on",TimeStamp),
    Desc       = cre_observedEventsDesc(1235,[Event]),
    NotifyReq1 = cre_notifyReq([#megaco_term_id{id = ?A5555}],Desc),
    CmdReq1    = cre_commandReq({notifyReq, NotifyReq1}),
    NotifyReq2 = cre_notifyReq([#megaco_term_id{id = ?A5556}],Desc),
    CmdReq2    = cre_commandReq({notifyReq, NotifyReq2}),
    ActionInfo1 = [{5000, [CmdReq1]}],
    ActionInfo2 = [{5001, [CmdReq2]}],
    TransInfo   = [{50008, ActionInfo1}, {50009, ActionInfo2}],
    msg_request(Mid, TransInfo).


msg23d() ->
    msg23d(?MG2_MID).
msg23d(Mid) ->
    TimeStamp  = cre_timeNotation("19990729","24020002"),
    Event      = cre_observedEvent("al/on",TimeStamp),
    Desc       = cre_observedEventsDesc(1235,[Event]),
    NotifyReq1 = cre_notifyReq([#megaco_term_id{id = ?A5555}],Desc),
    CmdReq1    = cre_commandReq({notifyReq, NotifyReq1}),
    NotifyReq2 = cre_notifyReq([#megaco_term_id{id = ?A5556}],Desc),
    CmdReq2    = cre_commandReq({notifyReq, NotifyReq2}),
    NotifyReq3 = cre_notifyReq([#megaco_term_id{id = ?A4444}],Desc),
    CmdReq3    = cre_commandReq({notifyReq, NotifyReq3}),
    NotifyReq4 = cre_notifyReq([#megaco_term_id{id = ?A4445}],Desc),
    CmdReq4    = cre_commandReq({notifyReq, NotifyReq4}),
    ActionInfo1 = [{5000, [CmdReq1]}, {5001, [CmdReq2]}],
    ActionInfo2 = [{5003, [CmdReq3]}, {5004, [CmdReq4]}],
    TransInfo   = [{50008, ActionInfo1}, {50009, ActionInfo2}],
    msg_request(Mid, TransInfo).


%% --------------------------

msg24() ->
    msg24(?MGC_MID).
msg24(Mid) ->
    AuditDesc = cre_auditDesc([statsToken]),
    SubReq    = cre_subtractReq([#megaco_term_id{id = ?A5555}], AuditDesc),
    SubReq2   = cre_subtractReq([#megaco_term_id{id = ?A5556}], AuditDesc),
    CmdReq    = cre_commandReq({subtractReq, SubReq}),
    CmdReq2   = cre_commandReq({subtractReq, SubReq2}),
    msg_request(Mid, 50009, 5000, [CmdReq, CmdReq2]).


%% --------------------------

msg25() ->
    msg25(?MG2_MID).
msg25(Mid) ->
    Stat11 = cre_statisticsParm("nt/os","45123"),
    Stat12 = cre_statisticsParm("nt/dur", "40"),
    Stats1 = [Stat11, Stat12],
    Reply1 = cre_ammsReply([#megaco_term_id{id = ?A5555}],
			   [{statisticsDescriptor, Stats1}]),
    Stat21 = cre_statisticsParm("rtp/ps","1245"),
    Stat22 = cre_statisticsParm("nt/os", "62345"),
    Stat23 = cre_statisticsParm("rtp/pr", "780"),
    Stat24 = cre_statisticsParm("nt/or", "45123"),
    Stat25 = cre_statisticsParm("rtp/pl", "10"),
    Stat26 = cre_statisticsParm("rtp/jit", "27"),
    Stat27 = cre_statisticsParm("rtp/delay","48"),
    Stats2 = [Stat21, Stat22, Stat23, Stat24, Stat25, Stat26, Stat27],
    Reply2 = cre_ammsReply([#megaco_term_id{id = ?A5556}],
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
    Profile = cre_serviceChangeProf("resgw",1),
    Parm    = cre_serviceChangeParm(restart,Address,[Reason],Profile),
    Req     = cre_serviceChangeReq([?megaco_root_termination_id],Parm),
    CmdReq  = cre_commandReq({serviceChangeReq, Req}),
    Auth    = cre_authHeader(),
    msg_request(Auth, Mid, 9998, ?megaco_null_context_id, [CmdReq]).


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
    Res = rfc3525_msgs_test(megaco_pretty_text_encoder, [], 1),
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

skip(Reason) ->
    megaco_codec_test_lib:skip(Reason).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_message(Codec, DynamicDecode, Conf, Bin) ->
    megaco_codec_test_lib:decode_message(Codec, DynamicDecode, ?VERSION, 
					 Conf, Bin).
encode_message(Codec, Conf, Msg) ->
    megaco_codec_test_lib:encode_message(Codec, ?VERSION, Conf, Msg).

test_msgs(Codec, DynamicDecode, Conf, Msgs) ->
    megaco_codec_test_lib:test_msgs(Codec, DynamicDecode, ?VERSION, Conf, 
				    fun chk_MegacoMessage/2, Msgs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chk_MegacoMessage(M,M) when is_record(M,'MegacoMessage') ->
    ok;
chk_MegacoMessage(#'MegacoMessage'{authHeader = Auth1,
				   mess       = Mess1},
		  #'MegacoMessage'{authHeader = Auth2,
				   mess       = Mess2}) ->
    chk_opt_AuthenticationHeader(Auth1,Auth2),
    chk_Message(Mess1,Mess2),
    ok;
chk_MegacoMessage(M1, M2) ->
    wrong_type({'MegacoMessage', M1, M2}).
    
chk_opt_AuthenticationHeader(A,A) ->
    ok;
chk_opt_AuthenticationHeader(A1,A2) ->
    not_equal({auth,A1,A2}).

chk_Message(M,M) when is_record(M,'Message') ->
    ok;
chk_Message(#'Message'{version     = Version1,
		       mId         = MID1,
		       messageBody = Body1},
	    #'Message'{version     = Version2,
		       mId         = MID2,
		       messageBody = Body2}) ->
    chk_version(Version1,Version2),
    chk_MId(MID1,MID2),
    chk_messageBody(Body1,Body2),
    ok;
chk_Message(M1,M2) ->
    wrong_type({'Message',M1,M2}).


chk_version(V,V) when is_integer(V) ->
    ok;
chk_version(V1,V2) when is_integer(V1) andalso is_integer(V2) ->
    not_equal({version,V1,V2});
chk_version(V1,V2) ->
    wrong_type({integer,V1,V2}).


chk_MId(M,M) ->
    {equal,mid};
chk_MId({Tag,M1},{Tag,M2}) ->
    Res = chk_MId(Tag,M1,M2),
    equal(Res);
chk_MId(M1,M2) ->
    not_equal({mid,M1,M2}).

chk_MId(ip4Address,M1,M2) -> chk_IP4Address(M1, M2);
chk_MId(ip6Address,M1,M2) -> chk_IP6Address(M1, M2);
chk_MId(domainName,M1,M2) -> chk_DomainName(M1, M2);
chk_MId(deviceName,M1,M2) -> chk_PathName(M1, M2);
chk_MId(mtpAddress,M1,M2) -> chk_mtpAddress(M1, M2);
chk_MId(Tag,M1,M2) ->
    wrong_type({invalid_tag,Tag,M1,M2}).
			       
    
chk_IP4Address(M, M) ->
    ok;
chk_IP4Address(M1, M2) ->
    not_equal({ip4Address,M1,M2}).

chk_IP6Address(M, M) ->
    ok;
chk_IP6Address(M1, M2) ->
    not_equal({ip6Address,M1,M2}).

chk_DomainName(D, D) when is_record(D,'DomainName') ->
    ok;
chk_DomainName(#'DomainName'{name       = Name1,
			     portNumber = Port1},
	       #'DomainName'{name       = Name2,
			     portNumber = Port2}) ->
    chk_DomainName_name(Name1,Name2),
    chk_DomainName_opt_portNumber(Port1,Port2),
    equal('DomainName');
chk_DomainName(D1,D2) ->
    wrong_type({'DomainName',D1,D2}).

chk_DomainName_name(N,N) when is_list(N) ->
    ok;
chk_DomainName_name(N1,N2) when is_list(N1) andalso is_list(N2) ->
    not_equal({'DomainName',name,N1,N2});
chk_DomainName_name(N1,N2) ->
    wrong_type({'DomainName',name,N1,N2}).

chk_DomainName_opt_portNumber(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_DomainName_opt_portNumber(P,P) 
  when is_integer(P) andalso (P >= 0) ->
    ok;
chk_DomainName_opt_portNumber(P1,P2) 
  when (is_integer(P1) andalso (P1 >= 0) andalso 
	is_integer(P2) andalso (P2 >= 0)) ->
    not_equal({'DomainName',portNumber,P1,P2});
chk_DomainName_opt_portNumber(P1,P2) ->
    wrong_type({'DomainName',portNumber,P1,P2}).


chk_PathName(P, P) ->
    ok;
chk_PathName(P1, P2) ->
    not_equal({pathname,P1,P2}).

chk_mtpAddress(M, M) ->
    ok;
chk_mtpAddress(M1, M2) ->
    not_equal({mtpAddress, M1, M2}).
    

chk_messageBody({messageError, B},
		{messageError, B}) when is_record(B,'ErrorDescriptor') ->
    ok;
chk_messageBody({messageError,B1},{messageError,B2}) ->
    chk_ErrorDescriptor(B1,B2),
    equal({messageBody, messageError});
chk_messageBody({transactions,T},{transactions,T}) when is_list(T) ->
    ok;
chk_messageBody({transactions,T1},{transactions,T2}) ->
    chk_transactions(T1,T2),
    ok;
chk_messageBody(B1,B2) ->
    wrong_type({messageBody,B1,B2}).
     

chk_transactions(T,T) when is_list(T) ->
    ok;
chk_transactions(T1,T2) 
  when is_list(T1) andalso is_list(T2) andalso (length(T1) =:= length(T2)) ->
    chk_transactions1(T1,T2);
chk_transactions(T1,T2) 
  when is_list(T1) andalso is_list(T2) ->
    not_equal({transactions, T1, T2});
chk_transactions(T1, T2) ->
    wrong_type({transactions,T1,T2}).

chk_transactions1([],[]) ->
    equal(transactions);
chk_transactions1([T|Ts1],[T|Ts2]) ->
    chk_transactions1(Ts1,Ts2);
chk_transactions1([T1|_Ts1],[T2|_Ts2]) ->
    chk_transaction(T1,T2),
    ok.

chk_transaction(T,T) ->
    ok;
chk_transaction({transactionRequest,T1},{transactionRequest,T2}) ->
    chk_transactionRequest(T1,T2),
    ok;
chk_transaction({transactionPending,T1},{transactionPending,T2}) ->
    chk_transactionPending(T1,T2),
    equal({transactionPending,T1,T2});
chk_transaction({transactionReply,T1},{transactionReply,T2}) ->
    chk_transactionReply(T1,T2),
    equal({transactionReply,T1,T2});
chk_transaction({transactionResponseAck,T1},{transactionResponseAck,T2}) ->
    chk_transactionAck(T1,T2),
    equal({transactionResponseAck,T1,T2});
chk_transaction({Tag1,_T1},{Tag2,_T2}) ->
    wrong_type({transaction_tag,Tag1,Tag2}).


chk_transactionRequest(T,T) when is_record(T,'TransactionRequest') ->
    ok;
chk_transactionRequest(T1,T2) when is_record(T1,'TransactionRequest') andalso 
				   is_record(T2,'TransactionRequest') ->
    chk_transactionId(T1#'TransactionRequest'.transactionId,
		      T2#'TransactionRequest'.transactionId),
    chk_actionRequests(T1#'TransactionRequest'.actions,
		       T2#'TransactionRequest'.actions),
    ok;
chk_transactionRequest(T1,T2) ->
    wrong_type({transactionRequest,T1,T2}).
    

chk_transactionPending(T,T) when is_record(T,'TransactionPending') ->
    ok;
chk_transactionPending(#'TransactionPending'{transactionId = Id1},
		       #'TransactionPending'{transactionId = Id2}) ->
    chk_transactionId(Id1,Id2),
    equal(transactionPending);
chk_transactionPending(T1,T2) ->
    wrong_type({transactionPending,T1,T2}).

chk_transactionReply(T,T) when is_record(T,'TransactionReply') ->
    ok;
chk_transactionReply(#'TransactionReply'{transactionId     = Id1,
					 immAckRequired    = ImmAck1,
					 transactionResult = TransRes1},
		     #'TransactionReply'{transactionId     = Id2,
					 immAckRequired    = ImmAck2,
					 transactionResult = TransRes2}) ->
    chk_transactionId(Id1,Id2),
    ImmAck1 = ImmAck2,
    chk_transactionReply_transactionResult(TransRes1,TransRes2),
    equal(transactionReply);
chk_transactionReply(T1,T2) ->
    wrong_type({transactionReply,T1,T2}).

chk_transactionReply_transactionResult(R,R) ->
    ok;
chk_transactionReply_transactionResult(R1,R2) ->
    not_equal({transactionReply_transactionResult,R1,R2}).

chk_transactionAck(T,T) when is_record(T,'TransactionAck') ->
    ok;
chk_transactionAck(#'TransactionAck'{firstAck = F1,
				     lastAck  = L1},
		   #'TransactionAck'{firstAck = F2,
				     lastAck  = L2}) ->
    chk_transactionId(F1,F2),
    chk_opt_transactionId(L1,L2),
    equal('TransactionAck');
chk_transactionAck(T1,T2) ->
    wrong_type({transactionAck,T1,T2}).


chk_actionRequests(A,A) when is_list(A) andalso (length(A) =:= 0) ->
    ok;
chk_actionRequests(A,A) when is_list(A) ->
    case hd(A) of
	A when is_record(A,'ActionRequest') ->
	    ok;
	Else ->
	    wrong_type({'ActionRequest',Else})
    end;
chk_actionRequests(A1,A2) 
  when is_list(A1) andalso is_list(A2) andalso (length(A1) =:= length(A2)) ->
    chk_actionRequests1(A1,A2);
chk_actionRequests(A1,A2) ->
    wrong_type({actionRequests,A1,A2}).

chk_actionRequests1([],[]) ->
    equal(actionRequests);
chk_actionRequests1([A|As1],[A|As2]) when is_record(A,'ActionRequest') ->
    chk_actionRequests1(As1,As2);
chk_actionRequests1([A1|_As1],[A2|_As2]) ->
    chk_actionRequest(A1,A2),
    ok.

chk_actionRequest(A,A) when is_record(A,'ActionRequest') ->
    ok;
chk_actionRequest(#'ActionRequest'{contextId           = Id1,
				   contextRequest      = Req1,
				   contextAttrAuditReq = AuditReq1,
				   commandRequests     = CmdReqs1},
		  #'ActionRequest'{contextId           = Id2,
				   contextRequest      = Req2,
				   contextAttrAuditReq = AuditReq2,
				   commandRequests     = CmdReqs2}) ->
    t("chk_actionRequest -> entry with"
      "~n   CmdReqs1: ~p"
      "~n   CmdReqs2: ~p",[CmdReqs1,CmdReqs2]),
    chk_contextId(Id1,Id2),
    chk_opt_contextRequest(Req1,Req2),
    chk_opt_contextAttrAuditReq(AuditReq1,AuditReq2),
    chk_commandRequests(CmdReqs1,CmdReqs2),
    ok.
    
chk_contextId(Id,Id) when is_integer(Id) ->
    ok;
chk_contextId(Id1,Id2) when is_integer(Id1) andalso is_integer(Id2) ->
    not_equal({contextId,Id1,Id2});
chk_contextId(Id1,Id2) ->
    wrong_type({contextId,Id1,Id2}).

chk_opt_contextRequest(asn1_NOVALUE, asn1_NOVALUE) ->
    ok;
chk_opt_contextRequest(R,R) when is_record(R,'ContextRequest') ->
    ok;
chk_opt_contextRequest(#'ContextRequest'{priority    = Prio1,
					 emergency   = Em1,
					 topologyReq = TopReq1} = C1,
		       #'ContextRequest'{priority    = Prio2,
					 emergency   = Em2,
					 topologyReq = TopReq2} = C2) ->
    chk_contextRequest_priority(Prio1,Prio2),
    chk_contextRequest_emergency(Em1,Em2),
    chk_topologyRequest(TopReq1,TopReq2),
    equal({'ContextRequest',C1,C2}).

chk_contextRequest_priority(asn1_NOVALUE,asn1_NOVALUE) ->
    ok;
chk_contextRequest_priority(P,P) when is_integer(P) ->
    ok;
chk_contextRequest_priority(P1,P2) when is_integer(P1) andalso is_integer(P2) ->
    not_equal({contextRequest_priority,P1,P2});
chk_contextRequest_priority(P1,P2) ->
    wrong_type({contextRequest_priority,P1,P2}).

chk_contextRequest_emergency(asn1_NOVALUE,asn1_NOVALUE) ->
    ok;
chk_contextRequest_emergency(true,true) ->
    ok;
chk_contextRequest_emergency(false,false) ->
    ok;
chk_contextRequest_emergency(E1,E2) ->
    not_equal({contextRequest_emergency,E1,E2}).

chk_topologyRequest(asn1_NOVALUE,asn1_NOVALUE) ->
    ok;
chk_topologyRequest(T,T) when is_record(T,'TopologyRequest') ->
    ok;
chk_topologyRequest(#'TopologyRequest'{terminationFrom   = F1,
				       terminationTo     = T1,
				       topologyDirection = D1} = T1,
		    #'TopologyRequest'{terminationFrom   = F2,
				       terminationTo     = T2,
				       topologyDirection = D2} = T2) ->
    chk_terminationId(F1,F2),
    chk_terminationId(T1,T2),
    chk_topologyRequest_topologyDirection(D1,D2),
    equal({'TopologyRequest',D1,D2}).

chk_topologyRequest_topologyDirection(bothway,bothway) ->
    ok;
chk_topologyRequest_topologyDirection(isolate,isolate) ->
    ok;
chk_topologyRequest_topologyDirection(oneway,oneway) ->
    ok;
chk_topologyRequest_topologyDirection(D1,D2) ->
    not_equal({topologyRequest_topologyDirection, D1, D2}).

chk_opt_contextAttrAuditReq(asn1_NOVALUE,asn1_NOVALUE) ->
    ok;
chk_opt_contextAttrAuditReq(R,R) when is_record(R,'ContextAttrAuditRequest') ->
    ok;
chk_opt_contextAttrAuditReq(#'ContextAttrAuditRequest'{topology  = T1,
						       emergency = E1,
						       priority  = P1} = R1,
			    #'ContextAttrAuditRequest'{topology  = T2,
						       emergency = E2,
						       priority  = P2} = R2)  ->
    T1 = T2,
    E1 = E2,
    P1 = P2,
    equal({'ContextAttrAuditRequest',R1,R2}).

chk_commandRequests(C1,C2) 
  when is_list(C1) andalso is_list(C2) andalso (length(C1) =:= length(C2)) ->
    t("chk_commandRequests -> entry with"
      "~n   C1: ~p"
      "~n   C2: ~p", [C1, C2]),
    chk_commandRequests1(C1,C2);
chk_commandRequests(C1,C2) ->
    t("chk_commandRequests -> entry",[]),
    wrong_type({commandRequests,C1,C2}).

chk_commandRequests1([],[]) ->
    ok;
chk_commandRequests1([C1|Cs1],[C2|Cs2]) ->
    chk_commandRequest(C1,C2),
    chk_commandRequests1(Cs1,Cs2).

chk_commandRequest(C,C) when is_record(C,'CommandRequest') ->
    ok;
chk_commandRequest(#'CommandRequest'{command        = Cmd1,
				     optional       = O1,
				     wildcardReturn = W1},
		   #'CommandRequest'{command        = Cmd2,
				     optional       = O2,
				     wildcardReturn = W2}) ->
    t("chk_commandRequest -> entry with"
      "~n   C1: ~p"
      "~n   C2: ~p", [Cmd1, Cmd2]),
    chk_commandRequest_command(Cmd1,Cmd2),
    O1 = O2,
    W1 = W2,
    ok;
chk_commandRequest(C1,C2) ->
    wrong_type({commandRequest,C1,C2}).

chk_commandRequest_command({addReq,C1},{addReq,C2}) ->
    chk_AmmRequest(C1,C2);
chk_commandRequest_command({moveReq,C1},{moveReq,C2}) ->
    chk_AmmRequest(C1,C2);
chk_commandRequest_command({modReq,C1},{modReq,C2}) ->
    chk_AmmRequest(C1,C2);
chk_commandRequest_command({subtractReq,C1},{subtractReq,C2}) ->
    chk_SubtractRequest(C1,C2);
chk_commandRequest_command({auditCapRequest,C1},{auditCapRequest,C2}) ->
    chk_AuditRequest(C1,C2);
chk_commandRequest_command({auditValueRequest,C1},{auditValueRequest,C2}) ->
    chk_AuditRequest(C1,C2);
chk_commandRequest_command({notifyReq,C1},{notifyReq,C2}) ->
    chk_NotifyRequest(C1,C2);
chk_commandRequest_command({serviceChangeReq,C1},{serviceChangeReq,C2}) ->
    chk_ServiceChangeRequest(C1,C2);
chk_commandRequest_command(C1,C2) ->
    wrong_type({commandRequest_command,C1,C2}).
    

chk_AmmRequest(R,R) when is_record(R,'AmmRequest') ->
    ok;
chk_AmmRequest(#'AmmRequest'{terminationID = Tids1,
			     descriptors   = D1},
	       #'AmmRequest'{terminationID = Tids2,
			     descriptors   = D2}) ->
    chk_terminationIds(Tids1,Tids2),
    chk_AmmRequest_descriptors(D1,D2),
    ok;
chk_AmmRequest(R1,R2) ->
    wrong_type({'AmmRequest',R1,R2}).

chk_AmmRequest_descriptors([],[]) ->
    ok;
chk_AmmRequest_descriptors(D1,D2) 
  when is_list(D1) andalso is_list(D2) andalso (length(D1) =:= length(D2)) ->
    chk_AmmRequest_descriptors1(D1,D2);
chk_AmmRequest_descriptors(D1,D2) ->
    wrong_type({ammRequest_descriptors,D1,D2}).

chk_AmmRequest_descriptors1([],[]) ->
    ok;
chk_AmmRequest_descriptors1([D1|Ds1],[D2|Ds2]) ->
    chk_AmmRequest_descriptor(D1,D2),
    chk_AmmRequest_descriptors1(Ds1,Ds2).
    
chk_AmmRequest_descriptor({mediaDescriptor,D1},{mediaDescriptor,D2}) -> 
    chk_MediaDescriptor(D1,D2);
chk_AmmRequest_descriptor({modemDescriptor,D1},{modemDescriptor,D2}) -> 
    chk_ModemDescriptor(D1,D2);
chk_AmmRequest_descriptor({muxDescriptor,D1},{muxDescriptor,D2}) -> 
    chk_MuxDescriptor(D1,D2);
chk_AmmRequest_descriptor({eventsDescriptor,D1},{eventsDescriptor,D2}) -> 
    chk_EventsDescriptor(D1,D2);
chk_AmmRequest_descriptor({eventBufferDescriptor,D1},{eventBufferDescriptor,D2}) -> 
    chk_EventBufferDescriptor(D1,D2);
chk_AmmRequest_descriptor({signalsDescriptor,D1},{signalsDescriptor,D2}) -> 
    chk_SignalsDescriptor(D1,D2);
chk_AmmRequest_descriptor({digitMapDescriptor,D1},{digitMapDescriptor,D2}) -> 
    chk_DigitMapDescriptor(D1,D2);
chk_AmmRequest_descriptor({auditDescriptor,D1},{auditDescriptor,D2}) -> 
    chk_AuditDescriptor(D1,D2);
chk_AmmRequest_descriptor({Tag1,_D1},{Tag2,_D2}) -> 
    wrong_type({ammRequest_descriptor_tag,Tag1,Tag2}).
    
    
chk_SubtractRequest(R,R) when is_record(R,'SubtractRequest') ->
    ok;
chk_SubtractRequest(#'SubtractRequest'{terminationID   = Tids1,
				       auditDescriptor = D1} = R1,
		    #'SubtractRequest'{terminationID   = Tids2,
				       auditDescriptor = D2} = R2) ->
    chk_terminationIds(Tids1, Tids2),
    chk_opt_AuditDescriptor(D1, D2),
    equal({'SubtractRequest',R1,R2});
chk_SubtractRequest(R1,R2) ->
    wrong_type({'SubtractRequest',R1,R2}).


chk_AuditRequest(R,R) when is_record(R,'AuditRequest') ->
    ok;
chk_AuditRequest(#'AuditRequest'{terminationID   = Tid1,
				 auditDescriptor = D1} = R1,
		 #'AuditRequest'{terminationID   = Tid2,
				 auditDescriptor = D2} = R2) ->
    chk_terminationId(Tid1,Tid2),
    chk_AuditDescriptor(D1,D2),
    equal({'AuditRequest',R1,R2});
chk_AuditRequest(R1,R2) ->
    wrong_type({'AuditRequest',R1,R2}).


chk_NotifyRequest(R,R) when is_record(R,'NotifyRequest') ->
    ok;
chk_NotifyRequest(#'NotifyRequest'{terminationID            = Tids1,
				   observedEventsDescriptor = ObsDesc1,
				   errorDescriptor          = ErrDesc1} = R1,
		  #'NotifyRequest'{terminationID            = Tids2,
				   observedEventsDescriptor = ObsDesc2,
				   errorDescriptor          = ErrDesc2} = R2) ->
    chk_terminationIds(Tids1,Tids2),
    chk_ObservedEventsDescriptor(ObsDesc1,ObsDesc2),
    chk_opt_ErrorDescriptor(ErrDesc1,ErrDesc2),
    equal({'NotifyRequest',R1,R2});
chk_NotifyRequest(R1,R2) ->
    wrong_type({'NotifyRequest',R1,R2}).


chk_ServiceChangeRequest(R,R) when is_record(R,'ServiceChangeRequest') ->
    ok;
chk_ServiceChangeRequest(#'ServiceChangeRequest'{terminationID      = Tids1,
						 serviceChangeParms = P1} = R1,
			 #'ServiceChangeRequest'{terminationID      = Tids2,
						 serviceChangeParms = P2} = R2) ->
    chk_terminationIds(Tids1,Tids2),
    chk_ServiceChangeParm(P1,P2),
    equal({'ServiceChangeRequest',R1,R2});
chk_ServiceChangeRequest(R1,R2) ->
    wrong_type({'ServiceChangeRequest',R1,R2}).


chk_MediaDescriptor(D, D) when is_record(D,'MediaDescriptor') ->
    ok;
chk_MediaDescriptor(#'MediaDescriptor'{termStateDescr = Tsd1,
				       streams        = S1} = D1,
		    #'MediaDescriptor'{termStateDescr = Tsd2,
				       streams        = S2} = D2) ->
%%     io:format("chk_MediaDescriptor -> entry with"
%% 	      "~n   Tsd1: ~p"
%% 	      "~n   Tsd2: ~p"
%% 	      "~n   S1:   ~p"
%% 	      "~n   S2:   ~p"
%% 	      "~n", [Tsd1, Tsd2, S1, S2]),
    chk_MediaDescriptor_tsd(Tsd1, Tsd2),
    chk_MediaDescriptor_streams(S1, S2),
    equal({'MediaDescriptor',D1,D2});
chk_MediaDescriptor(D1,D2) ->
    wrong_type({'MediaDescriptor',D1,D2}).

chk_MediaDescriptor_tsd(D, D) ->
    ok;
chk_MediaDescriptor_tsd(D1, D2) ->
    not_equal({termStateDescr, D1, D2}).

chk_MediaDescriptor_streams({oneStream, S}, {oneStream, S}) ->
    ok;
chk_MediaDescriptor_streams({oneStream, S1}, {oneStream, S2}) ->
    not_equal({oneStream, S1, S2});
chk_MediaDescriptor_streams({multiStream, MS}, {multiStream, MS}) ->
    ok;
chk_MediaDescriptor_streams({multiStream, MS1}, {multiStream, MS2}) ->
    chk_StreamDescriptors(MS1, MS2);
chk_MediaDescriptor_streams(S1, S2) ->
    not_equal({streams, S1, S2}).
    
chk_StreamDescriptors([], []) ->
    ok;
chk_StreamDescriptors([SD1|MS1], [SD2|MS2]) ->
    chk_StreamDescriptor(SD1, SD2),
    chk_StreamDescriptors(MS1, MS2).

chk_StreamDescriptor(SD, SD) when is_record(SD, 'StreamDescriptor') ->
    ok;
chk_StreamDescriptor(#'StreamDescriptor'{streamID    = SID1,
					 streamParms = SP1} = SD1, 
		     #'StreamDescriptor'{streamID    = SID2,
					 streamParms = SP2} = SD2) ->
    SID1 = SID2,
    chk_StreamParms(SP1, SP2),
    equal({'StreamDescriptor',SD1, SD2});
chk_StreamDescriptor(SD1, SD2) ->
    wrong_type({'StreamDescriptor',SD1,SD2}).


chk_StreamParms(SP, SP) when is_record(SP, 'StreamParms') ->
    ok;
chk_StreamParms(#'StreamParms'{localControlDescriptor = LCD1,
			       localDescriptor        = LD1,
			       remoteDescriptor       = RD1} = SP1,
		#'StreamParms'{localControlDescriptor = LCD2,
			       localDescriptor        = LD2,
			       remoteDescriptor       = RD2} = SP2) ->
    LCD1 = LCD2,
    LD1  = LD2,
    RD1  = RD2,
    equal({'StreamParms', SP1, SP2});
chk_StreamParms(SP1, SP2) ->
    wrong_type({'StreamDescriptor', SP1, SP2}).

chk_ModemDescriptor(D,D) when is_record(D,'ModemDescriptor') ->
    ok;
chk_ModemDescriptor(#'ModemDescriptor'{mtl = T1,
				       mpl = P1} = D1,
		    #'ModemDescriptor'{mtl = T2,
				       mpl = P2} = D2) ->
    T1 = T2,
    P1 = P2,
    equal({'ModemDescriptor',D1,D2});
chk_ModemDescriptor(D1,D2) ->
    wrong_type({'ModemDescriptor',D1,D2}).

chk_MuxDescriptor(D,D) when is_record(D,'MuxDescriptor') ->
    ok;
chk_MuxDescriptor(#'MuxDescriptor'{muxType  = T1,
				   termList = I1} = D1,
		  #'MuxDescriptor'{muxType  = T2,
				   termList = I2} = D2) ->
    T1 = T2,
    I1 = I2,
    equal({'MuxDescriptor',D1,D2});
chk_MuxDescriptor(D1,D2) ->
    wrong_type({'MuxDescriptor',D1,D2}).

chk_EventsDescriptor(D,D) when is_record(D,'EventsDescriptor') ->
    ok;
chk_EventsDescriptor(#'EventsDescriptor'{requestID = I1,
					 eventList = E1} = D1,
		     #'EventsDescriptor'{requestID = I2,
					 eventList = E2} = D2) ->
    I1 = I2,
    E1 = E2,
    equal({'EventsDescriptor',D1,D2});
chk_EventsDescriptor(D1,D2) ->
    wrong_type({'EventsDescriptor',D1,D2}).

chk_EventBufferDescriptor(D1,D2) 
  when is_list(D1) andalso is_list(D2) andalso (length(D1) =:= length(D2)) ->
    chk_EventBufferDescriptor1(D1,D2);
chk_EventBufferDescriptor(D1,D2) ->
    wrong_type({eventBufferDescriptor,D1,D2}).

chk_EventBufferDescriptor1([],[]) ->
    ok;
chk_EventBufferDescriptor1([ES1|D1],[ES2|D2]) ->
    chk_EventSpec(ES1,ES2),
    chk_EventBufferDescriptor1(D1,D2).

chk_EventSpec(ES,ES) when is_record(ES,'EventSpec') ->
    ok;
chk_EventSpec(#'EventSpec'{eventName    = N1,
			   streamID     = I1,
			   eventParList = P1} = ES1,
	      #'EventSpec'{eventName    = N2,
			   streamID     = I2,
			   eventParList = P2} = ES2) ->
    N1 = N2,
    chk_opt_StreamId(I1,I2),
    chk_EventParameters(P1,P2),
    equal({'EventSpec',ES1,ES2});
chk_EventSpec(ES1,ES2) ->
    wrong_type({'EventSpec',ES1,ES2}).


chk_opt_ErrorDescriptor(asn1_NOVALUE,asn1_NOVALUE) ->
    ok;
chk_opt_ErrorDescriptor(E1,E2) ->
    chk_ErrorDescriptor(E1,E2).

chk_ErrorDescriptor(E,E) when is_record(E,'ErrorDescriptor') ->
    ok;
chk_ErrorDescriptor(#'ErrorDescriptor'{errorCode = Code1,
				       errorText = Text1} = E1,
		    #'ErrorDescriptor'{errorCode = Code2,
				       errorText = Text2} = E2) ->
    chk_ErrorCode(Code1,Code2),
    chk_opt_ErrorText(Text1,Text2),
    equal({'ErrorDescriptor',E1,E2});
chk_ErrorDescriptor(E1,E2) ->
    wrong_type({'ErrorDescriptor',E1,E2}).

chk_ErrorCode(C,C) when is_integer(C) ->
    ok;
chk_ErrorCode(C1,C2) when is_integer(C1) andalso is_integer(C2) ->
    not_equal({errorCode,C1,C2});
chk_ErrorCode(C1,C2) ->
    throw({wrong_type,{errorCode,C1,C2}}).

chk_opt_ErrorText(asn1_NOVALUE,asn1_NOVALUE) ->
    ok;
chk_opt_ErrorText(T,T) when is_list(T) ->
    ok;
chk_opt_ErrorText(T1,T2) when is_list(T1) andalso is_list(T2) ->
    not_equal({errorText,T1,T2});
chk_opt_ErrorText(T1,T2) ->
    wrong_type({errorText,T1,T2}).


chk_SignalsDescriptor(D1,D2) 
  when is_list(D1) andalso is_list(D2) andalso (length(D1) =:= length(D2)) ->
    chk_SignalsDescriptor1(D1,D2);
chk_SignalsDescriptor(D1,D2) ->
    wrong_type({signalsDescriptor,D1,D2}).

chk_SignalsDescriptor1([],[]) ->
    ok;
chk_SignalsDescriptor1([S1|D1],[S2|D2]) ->
    chk_SignalRequest(S1,S2),
    chk_SignalsDescriptor1(D1,D2).

chk_SignalRequest({signal,S1},{signal,S2}) ->
    chk_Signal(S1,S2);
chk_SignalRequest({seqSigList,S1},{seqSigList,S2}) ->
    chk_SeqSignalList(S1,S2);
chk_SignalRequest(R1,R2) ->
    wrong_type({signalRequest,R1,R2}).

chk_SeqSignalList(S,S) when is_record(S,'SeqSigList') ->
    ok;
chk_SeqSignalList(#'SeqSigList'{id         = Id1,
				signalList = SigList1} = S1,
		  #'SeqSigList'{id         = Id2,
				signalList = SigList2} = S2) ->
    Id1 = Id2,
    chk_Signals(SigList1,SigList2),
    equal({'SeqSigList',S1,S2});
chk_SeqSignalList(S1,S2) ->
    wrong_type({'SeqSigList',S1,S2}).


chk_Signals([],[]) ->
    ok;
chk_Signals([Sig1|Sigs1],[Sig2|Sigs2]) ->
    chk_Signal(Sig1,Sig2),
    chk_Signals(Sigs1,Sigs2).


chk_Signal(S,S) when is_record(S,'Signal') ->
    ok;
chk_Signal(#'Signal'{signalName       = N1,
		     streamID         = I1,
		     sigType          = T1,
		     duration         = D1,
		     notifyCompletion = C1,
		     keepActive       = K1,
		     sigParList       = P1} = S1,
	   #'Signal'{signalName       = N2,
		     streamID         = I2,
		     sigType          = T2,
		     duration         = D2,
		     notifyCompletion = C2,
		     keepActive       = K2,
		     sigParList       = P2} = S2) ->
    N1 = N2,
    chk_opt_StreamId(I1,I2),
    chk_opt_SignalType(T1,T2),
    chk_opt_duration(D1,D2),
    chk_opt_NotifyCompletion(C1,C2),
    chk_opt_keepAlive(K1,K2),
    chk_sigParameters(P1,P2),
    equal({'Signal',S1,S2});
chk_Signal(S1,S2) ->
    wrong_type({'Signal',S1,S2}).

chk_DigitMapDescriptor(D,D) when is_record(D,'DigitMapDescriptor') ->
    ok;
chk_DigitMapDescriptor(#'DigitMapDescriptor'{digitMapName  = N1,
					     digitMapValue = V1},
		       #'DigitMapDescriptor'{digitMapName  = N2,
					     digitMapValue = V2}) ->
    chk_opt_digitMapName(N1,N2),
    chk_opt_digitMapValue(V1,V2),
    ok;
chk_DigitMapDescriptor(D1,D2) ->
    wrong_type({'DigitMapDescriptor',D1,D2}).

chk_opt_digitMapName(asn1_NOVALUE,asn1_NOVALUE) ->
    ok;
chk_opt_digitMapName(N1,N2) ->
    chk_digitMapName(N1,N2).

chk_digitMapName(N,N) ->
    ok;
chk_digitMapName(N1,N2) ->
    not_equal({digitMapName,N1,N2}).

chk_opt_digitMapValue(asn1_NOVALUE,asn1_NOVALUE) ->
    ok;
chk_opt_digitMapValue(V1,V2) ->
    chk_digitMapValue(V1,V2).

chk_digitMapValue(V,V) when is_record(V,'DigitMapValue') ->
    ok;
chk_digitMapValue(#'DigitMapValue'{digitMapBody = Body1,
				   startTimer   = Start1,
				   shortTimer   = Short1,
				   longTimer    = Long1},
		  #'DigitMapValue'{digitMapBody = Body2,
				   startTimer   = Start2,
				   shortTimer   = Short2,
				   longTimer    = Long2}) ->
    chk_digitMapValue_digitMapBody(Body1,Body2), % Could contain trailing '\n', ...
    chk_opt_timer(Start1,Start2),
    chk_opt_timer(Short1,Short2),
    chk_opt_timer(Long1,Long2),
    ok;
chk_digitMapValue(V1,V2) ->
    wrong_type({digitMapValue,V1,V2}).

chk_digitMapValue_digitMapBody(B,B) when is_list(B) ->
    ok;
chk_digitMapValue_digitMapBody(B1, B2) 
  when is_list(B1) andalso is_list(B2) andalso (length(B1) > length(B2))  ->
    case string:str(B2, B1) of
	0 ->
	    ok;
	_ ->
	    not_equal({digitMapValue_digitMapBody,B1,B2})
    end;
chk_digitMapValue_digitMapBody(B1, B2) 
  when is_list(B1) andalso is_list(B2) andalso (length(B1) < length(B2))  ->
    case string:str(B1, B2) of
	0 ->
	    ok;
	_ ->
	    not_equal({digitMapValue_digitMapBody,B1,B2})
    end;
chk_digitMapValue_digitMapBody(B1,B2) when is_list(B1) andalso is_list(B2) ->
    not_equal({digitMapValue_digitMapBody,B1,B2});
chk_digitMapValue_digitMapBody(B1,B2) ->
    wrong_type({digitMapValue_digitMapBody,B1,B2}).


chk_opt_AuditDescriptor(asn1_NOVALUE,asn1_NOVALUE) ->
    ok;
chk_opt_AuditDescriptor(D1,D2) ->
    chk_AuditDescriptor(D1,D2).

chk_AuditDescriptor(D,D) when is_record(D,'AuditDescriptor') ->
    ok;
chk_AuditDescriptor(#'AuditDescriptor'{auditToken = T1} = D1,
		    #'AuditDescriptor'{auditToken = T2} = D2) ->
    chk_opt_auditToken(T1,T2),
    equal({'AuditDescriptor',D1,D2});
chk_AuditDescriptor(D1,D2) ->
    wrong_type({'AuditDescriptor',D1,D2}).

chk_opt_auditToken(asn1_NOVALUE,asn1_NOVALUE) ->
    ok;
chk_opt_auditToken(T1,T2) ->
    chk_auditToken(T1,T2).

chk_auditToken(T1,T2) 
  when is_list(T1) andalso is_list(T2) andalso (length(T1) =:= length(T2)) ->
    chk_auditToken1(T1,T2);
chk_auditToken(T1,T2) ->
    wrong_type({auditToken,T1,T2}).

chk_auditToken1([],[]) ->
    ok;
chk_auditToken1([H1|T1],[H2|T2]) ->
    chk_auditToken2(H1,H2),
    chk_auditToken1(T1,T2).

chk_auditToken2(muxToken,muxToken) ->
    ok;
chk_auditToken2(modemToken,modemToken) ->
    ok;
chk_auditToken2(mediaToken,mediaToken) ->
    ok;
chk_auditToken2(eventsToken,eventsToken) ->
    ok;
chk_auditToken2(signalsToken,signalsToken) ->
    ok;
chk_auditToken2(digitMapToken,digitMapToken) ->
    ok;
chk_auditToken2(statsToken,statsToken) ->
    ok;
chk_auditToken2(observedEventsToken,observedEventsToken) ->
    ok;
chk_auditToken2(packagesToken,packagesToken) ->
    ok;
chk_auditToken2(eventBufferToken,eventBufferToken) ->
    ok;
chk_auditToken2(T1,T2) when is_atom(T1) andalso is_atom(T2) ->
    not_equal({auditToken,T1,T2});
chk_auditToken2(T1,T2) ->
    wrong_type({auditToken,T1,T2}).

chk_ObservedEventsDescriptor(D,D) 
  when is_record(D,'ObservedEventsDescriptor') ->
    ok;
chk_ObservedEventsDescriptor(
  #'ObservedEventsDescriptor'{requestId        = Id1,
			      observedEventLst = E1} = D1,
  #'ObservedEventsDescriptor'{requestId        = Id2,
			      observedEventLst = E2} = D2) ->
    Id1 = Id2,
    chk_ObservedEvents(E1,E2),
    equal({'ObservedEventsDescriptor',D1,D2});
chk_ObservedEventsDescriptor(D1,D2) ->
    wrong_type({'ObservedEventsDescriptor',D1,D2}).
    

chk_ObservedEvents(E1,E2) 
  when is_list(E1) andalso is_list(E2) andalso (length(E1) =:= length(E2)) ->
    chk_ObservedEvents1(E1,E2);
chk_ObservedEvents(E1,E2) ->
    wrong_type({observedEvents,E1,E2}).


chk_ObservedEvents1([],[]) ->
    ok;
chk_ObservedEvents1([Ev1|Evs1],[Ev2|Evs2]) ->
    chk_ObservedEvent(Ev1,Ev2),
    chk_ObservedEvents1(Evs1,Evs2).

chk_ObservedEvent(#'ObservedEvent'{eventName    = N1,
				   streamID     = I1,
				   eventParList = P1,
				   timeNotation = T1} = E1,
		  #'ObservedEvent'{eventName    = N2,
				   streamID     = I2,
				   eventParList = P2,
				   timeNotation = T2} = E2) ->
    N1 = N2,
    chk_opt_StreamId(I1,I2),
    chk_EventParameters(P1,P2),
    chk_opt_TimeNotation(T1,T2),
    equal({'ObservedEvent',E1,E2});
chk_ObservedEvent(E1,E2) ->
    wrong_type({'ObservedEvent',E1,E2}).
    

chk_opt_TimeNotation(asn1_NOVALUE,asn1_NOVALUE) ->
    ok;
chk_opt_TimeNotation(T1,T2) ->
    chk_TimeNotation(T1,T2).

chk_TimeNotation(T,T) when is_record(T,'TimeNotation') ->
    ok;
chk_TimeNotation(#'TimeNotation'{date = Date1,
				 time = Time1} = T1,
		 #'TimeNotation'{date = Date2,
				 time = Time2} = T2) ->
    Date1 = Date2,
    Time1 = Time2,
    equal({'TimeNotation',T1,T2});
chk_TimeNotation(T1,T2) ->
    wrong_type({'TimeNotation',T1,T2}).
    
    
chk_opt_timer(asn1_NOVALUE,asn1_NOVALUE) ->
    ok;
chk_opt_timer(T1,T2) ->
    chk_timer(T1,T2).

chk_timer(T,T) when is_integer(T) ->
    {equal,timer};
chk_timer(T1,T2) when is_integer(T1) andalso is_integer(T2) ->
    throw({not_equal,{timer,T1,T2}});
chk_timer(T1,T2) ->
    throw({wrong_type,{timer,T1,T2}}).
    

chk_opt_SignalType(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,signalType};
chk_opt_SignalType(T1,T2) ->
    chk_SignalType(T1,T2).

chk_SignalType(brief,brief) ->
    {equal,signalType};
chk_SignalType(onOffonOff,onOffonOff) ->
    {equal,signalType};
chk_SignalType(timeOut,timeOut) ->
    {equal,signalType};
chk_SignalType(T1,T2) ->
    throw({wrong_type,{signalType,T1,T2}}).


chk_opt_duration(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,duration};
chk_opt_duration(D1,D2) ->
    chk_duration(D1,D2).

chk_duration(D,D) when is_integer(D) ->
    {equal,duration};
chk_duration(D1,D2) when is_integer(D1) andalso is_integer(D2) ->
    throw({not_equal,{duration,D1,D2}});
chk_duration(D1,D2) ->
    throw({wrong_type,{duration,D1,D2}}).


chk_opt_NotifyCompletion(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,notifyCompletion};
chk_opt_NotifyCompletion(N1,N2) ->
    chk_NotifyCompletion(N1,N2).

chk_NotifyCompletion([],[]) ->
    {equal,notifyCompletion};
chk_NotifyCompletion([Item1|Items1],[Item2|Items2]) ->
    chk_NotifyCompletion1(Item1,Item2),
    chk_NotifyCompletion(Items1,Items2);
chk_NotifyCompletion(C1,C2) ->
    throw({wrong_type,{notifyCompletion,C1,C2}}).

chk_NotifyCompletion1(onTimeOut,onTimeOut) ->
    {equal,notifyCompletion_part};
chk_NotifyCompletion1(onInterruptByEvent,onInterruptByEvent) ->
    {equal,notifyCompletion_part};
chk_NotifyCompletion1(onInterruptByNewSignalDescr,onInterruptByNewSignalDescr) ->
    {equal,notifyCompletion_part};
chk_NotifyCompletion1(otherReason,otherReason) ->
    {equal,notifyCompletion_part};
chk_NotifyCompletion1(C1,C2) ->
    throw({wrong_type,{notifyCompletion_part,C1,C2}}).


chk_opt_keepAlive(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,keepAlive};
chk_opt_keepAlive(K1,K2) ->
    chk_keepAlive(K1,K2).

chk_keepAlive(true,true) ->
    ok;
chk_keepAlive(false,false) ->
    ok;
chk_keepAlive(K1,K2) ->
    wrong_type({keepAlive,K1,K2}).


chk_ServiceChangeParm(P,P) when  is_record(P,'ServiceChangeParm') ->
    ok;
chk_ServiceChangeParm(#'ServiceChangeParm'{serviceChangeMethod  = M1, 
					   serviceChangeAddress = A1, 
					   serviceChangeVersion = V1, 
					   serviceChangeProfile = P1, 
					   serviceChangeReason  = R1, 
					   serviceChangeDelay   = D1, 
					   serviceChangeMgcId   = Mid1, 
					   timeStamp            = T1} = P1,
		      #'ServiceChangeParm'{serviceChangeMethod  = M2, 
					   serviceChangeAddress = A2, 
					   serviceChangeVersion = V2, 
					   serviceChangeProfile = P2, 
					   serviceChangeReason  = R2, 
					   serviceChangeDelay   = D2, 
					   serviceChangeMgcId   = Mid2, 
					   timeStamp            = T2} = P2) ->
    M1 = M2,
    A1 = A2,
    V1 = V2,
    P1 = P2,
    R1 = R2,
    D1 = D2,
    Mid1 = Mid2,
    T1 = T2,
    equal({'ServiceChangeParm',P1,P2});
chk_ServiceChangeParm(P1,P2) ->
    wrong_type({'ServiceChangeParm',P1,P2}).


chk_sigParameters(S1,S2) 
  when is_list(S1) andalso is_list(S2) andalso (length(S1) =:= length(S2)) ->
    chk_sigParameters1(S1,S2);
chk_sigParameters(S1,S2) ->
    wrong_type({sigParameters,S1,S2}).

chk_sigParameters1([],[]) ->
    ok;
chk_sigParameters1([H1|T1],[H2|T2]) ->
    chk_sigParameter(H1,H2),
    chk_sigParameters1(T1,T2);
chk_sigParameters1(P1,P2) ->
    wrong_type({sigParameters,P1,P2}).
    
chk_sigParameter(#'SigParameter'{sigParameterName = N1,
				 value            = V1,
				 extraInfo        = E1},
		 #'SigParameter'{sigParameterName = N2,
				 value            = V2,
				 extraInfo        = E2}) ->
    N1 = N2,
    chk_Value(V1,V2),
    chk_opt_extraInfo(E1,E2),
    ok;
chk_sigParameter(P1,P2) ->
    wrong_type({'SigParameter',P1,P2}).

    
chk_opt_StreamId(asn1_NOVALUE,asn1_NOVALUE) ->
    ok;
chk_opt_StreamId(I1,I2) ->
    chk_StreamId(I1,I2).

chk_StreamId(I,I) when is_integer(I) ->
    ok;
chk_StreamId(I1,I2) when is_integer(I1) andalso is_integer(I2) ->
    not_equal({streamId,I1,I2});
chk_StreamId(I1,I2) ->
    wrong_type({streamId,I1,I2}).
    

chk_EventParameters(EP1,EP2) 
  when is_list(EP1) andalso is_list(EP2) andalso (length(EP1) =:= length(EP2)) ->
    chk_EventParameters1(EP1,EP2);
chk_EventParameters(EP1,EP2) ->
    wrong_type({eventParameters,EP1,EP2}).

chk_EventParameters1([],[]) ->
    ok;
chk_EventParameters1([EP1|EPS1],[EP2|EPS2]) ->
    chk_EventParameter(EP1,EP2),
    chk_EventParameters1(EPS1,EPS2).
    
chk_EventParameter(EP,EP) when is_record(EP,'EventParameter') ->
    ok;
chk_EventParameter(#'EventParameter'{eventParameterName = N1,
				     value              = V1,
				     extraInfo          = E1} = EP1,
		   #'EventParameter'{eventParameterName = N2,
				     value              = V2,
				     extraInfo          = E2} = EP2) ->
    N1 = N2,
    chk_Value(V1,V2),
    chk_opt_extraInfo(E1,E2),
    equal({'EventParameter',EP1,EP2});
chk_EventParameter(EP1,EP2) ->
    wrong_type({'EventParameter',EP1,EP2}).


chk_Value(V,V) when is_list(V) ->
    chk_Value(V);
chk_Value(V1,V2) 
  when is_list(V1) andalso is_list(V2) andalso (length(V1) =:= length(V2)) ->
    chk_Value1(V1,V2);
chk_Value(V1,V2) ->
    wrong_type({value,V1,V2}).

chk_Value([]) ->
    ok;
chk_Value([H|T]) when is_list(H) ->
    chk_Value(T);
chk_Value([H|_T]) ->
    wrong_type({value_part,H}).

chk_Value1([],[]) ->
    ok;
chk_Value1([H|T1],[H|T2]) when is_list(H) ->
    chk_Value1(T1,T2);
chk_Value1([H|_T1],[H|_T2]) ->
    wrong_type({value_part,H});
chk_Value1([H1|_T1],[H2|_T2]) when is_list(H1) andalso is_list(H2) ->
    not_equal({value_part,H1,H2});
chk_Value1(V1,V2) ->
    wrong_type({value,V1,V2}).


chk_opt_extraInfo(asn1_NOVALUE,asn1_NOVALUE) ->
    ok;
chk_opt_extraInfo(E1,E2) ->
    chk_extraInfo(E1,E2).

chk_extraInfo({relation,greaterThan},{relation,greaterThan}) ->
    ok;
chk_extraInfo({relation,smallerThan},{relation,smallerThan}) ->
    ok;
chk_extraInfo({relation,unequalTo},{relation,unequalTo}) ->
    ok;
chk_extraInfo({range,true},{range,true}) ->
    ok;
chk_extraInfo({range,false},{range,false}) ->
    ok;
chk_extraInfo({sublist,true},{sublist,true}) ->
    ok;
chk_extraInfo({sublist,false},{sublist,false}) ->
    ok;
chk_extraInfo(E1,E2) ->
    wrong_type({extraInfo,E1,E2}).


chk_opt_transactionId(asn1_NOVALUE,asn1_NOVALUE) ->
    ok;
chk_opt_transactionId(Id1,Id2) ->
    chk_transactionId(Id1,Id2).

chk_transactionId(Id,Id) when is_integer(Id) ->
    ok;
chk_transactionId(Id1,Id2) when is_integer(Id1) andalso is_integer(Id2) ->
    not_equal({transactionId,Id1,Id2});
chk_transactionId(Id1,Id2) ->
    wrong_type({transactionId,Id1,Id2}).
    

chk_terminationIds(Tids1,Tids2) 
  when is_list(Tids1) andalso is_list(Tids2) andalso (length(Tids1) =:= length(Tids2)) ->
    chk_terminationIds1(Tids1,Tids2);
chk_terminationIds(Tids1,Tids2) ->
    wrong_type({terminationIds,Tids1,Tids2}).

chk_terminationIds1([],[]) ->
    ok;
chk_terminationIds1([Tid1|Tids1],[Tid2|Tids2]) ->
    chk_terminationId(Tid1,Tid2),
    chk_terminationIds1(Tids1,Tids2).

chk_terminationId(Id,Id) when is_record(Id,'TerminationID') ->
    ok;
chk_terminationId(Id,Id) when is_record(Id,megaco_term_id) ->
    ok;
chk_terminationId(#'TerminationID'{wildcard = W1,
				   id       = I1} = Tid1,
		  #'TerminationID'{wildcard = W2,
				   id       = I2} = Tid2) ->
    chk_terminationId_wildcard(W1,W2),
    chk_terminationId_id(I1,I2),
    equal({'TerminationID',Tid1,Tid2});
chk_terminationId(#megaco_term_id{contains_wildcards = W1,
				  id                 = I1} = Tid1,
		  #megaco_term_id{contains_wildcards = W2,
				  id                 = I2} = Tid2) ->
    chk_terminationId_wildcard(W1,W2),
    chk_terminationId_id(I1,I2),
    equal({megaco_term_id,Tid1,Tid2});
chk_terminationId(Tid1,Tid2) ->
    wrong_type({terminationId,Tid1,Tid2}).

chk_terminationId_wildcard(W,W) ->
    ok;
chk_terminationId_wildcard(W1,W2) ->
    not_equal({terminationId_wildcard,W1,W2}).

chk_terminationId_id(I,I) ->
    ok;
chk_terminationId_id(I1,I2) ->
    not_equal({terminationId_id,I1,I2}).


equal(What) ->
    error({equal, What}).

not_equal(What) ->
    error({not_equal, What}).

wrong_type(What) ->
    error({wrong_type, What}).

error(Reason) ->
    throw({error, Reason}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cre_MegacoMessage(Mess) ->
    ?MSG_LIB:cre_MegacoMessage(Mess).

cre_megacoMessage(V, Mid, Body) ->
    #'MegacoMessage'{mess = #'Message'{version     = V,
                                       mId         = Mid,
                                       messageBody = Body}}.

cre_Msg(Mid, Body) ->
    cre_Msg(?VERSION, Mid, Body).

cre_Msg(V, Mid, Body) ->
    ?MSG_LIB:cre_Message(V, Mid, Body).

cre_authHeader() ->
    SecParmIdx = [239, 205, 171, 137], 
    SeqNum     = [18, 52, 86, 120], 
    AD         = [18, 52, 86, 120, 137, 171, 205, 239, 118, 84, 50, 16],
    cre_authHeader(SecParmIdx, SeqNum, AD).

cre_authHeader(Idx, Num, D) ->
    #'AuthenticationHeader'{secParmIndex = Idx, 
			    seqNum       = Num, 
			    ad           = D}.

cre_TransId(TransId) ->
    ?MSG_LIB:cre_TransactionId(TransId).

cre_Trans(Trans) ->
    ?MSG_LIB:cre_Transaction(Trans).

cre_TransReq(TransId, Actions) ->
    ?MSG_LIB:cre_TransactionRequest(TransId, Actions).

cre_transactionReply(TransId, Actions) ->
    #'TransactionReply'{transactionId     = TransId,
			transactionResult = {actionReplies, Actions}}.

cre_transactionAck(Serial, Serial) ->
    #'TransactionAck'{firstAck = Serial};
cre_transactionAck(First, Last) ->
    #'TransactionAck'{firstAck = First, lastAck = Last}.
    
cre_ActReq(CtxId, CmdReqs) ->
    ?MSG_LIB:cre_ActionRequest(CtxId, CmdReqs).

cre_actionReply(CtxId, CmdReply) ->
    #'ActionReply'{contextId    = CtxId,
		   commandReply = CmdReply}.

cre_CtxID(Id) ->
    ?MSG_LIB:cre_ContextID(Id).

%% Parameter related
cre_propertyParm(Name, Val) ->
    #'PropertyParm'{name  = Name, value = [Val]}.


%% Statistics related
cre_statisticsParm(Name, Val) ->
    #'StatisticsParameter'{statName  = Name, statValue = [Val]}.


% Event related 
cre_eventParm(Name, Val) ->
    #'EventParameter'{eventParameterName = Name, value = Val}.

cre_observedEvent(Name, Not) ->
    #'ObservedEvent'{eventName = Name, timeNotation = Not}.
cre_observedEvent(Name, Not, Par) ->
    #'ObservedEvent'{eventName = Name, timeNotation = Not, eventParList = Par}.

cre_requestedEvent(Name) ->
    #'RequestedEvent'{pkgdName = Name}.
cre_requestedEvent(Name, ParList) when is_list(ParList) ->
    #'RequestedEvent'{pkgdName = Name, evParList = ParList};
cre_requestedEvent(Name, Action) when is_tuple(Action) ->
    #'RequestedEvent'{pkgdName = Name, eventAction = Action}.


cre_observedEventsDesc(Id, EvList) ->
    #'ObservedEventsDescriptor'{requestId = Id, observedEventLst = EvList}.

cre_eventsDesc(Id, EvList) ->
    #'EventsDescriptor'{requestID = Id, eventList = EvList}.


%% Service change related
cre_serviceChangeParm(M,A,R,P) ->
    #'ServiceChangeParm'{serviceChangeMethod  = M, serviceChangeAddress = A,
			 serviceChangeReason  = R, serviceChangeProfile = P}.

cre_serviceChangeResParm(A,P) ->
    #'ServiceChangeResParm'{serviceChangeAddress = A, 
			    serviceChangeProfile = P}.

cre_serviceChangeReq(Tid, P) ->
    #'ServiceChangeRequest'{terminationID = Tid, serviceChangeParms = P}.

cre_serviceChangeProf(Name, Ver) when is_list(Name) andalso is_integer(Ver) ->
    #'ServiceChangeProfile'{profileName = Name, version = Ver}.

cre_serviceChangeReply(Tid, Res) ->
    #'ServiceChangeReply'{terminationID = Tid, serviceChangeResult = Res}.


%% Stream related
cre_streamParms(Lcd) ->
    #'StreamParms'{localControlDescriptor = Lcd}.
cre_streamParms(Lcd, Ld) ->
    #'StreamParms'{localControlDescriptor = Lcd, localDescriptor = Ld}.
cre_streamParms(Lcd, Ld, Rd) ->
    #'StreamParms'{localControlDescriptor = Lcd, 
		   localDescriptor        = Ld,
		   remoteDescriptor       = Rd}.
cre_streamParmsL(Ld) ->
    #'StreamParms'{localDescriptor = Ld}.
cre_streamParmsR(Rd) ->
    #'StreamParms'{remoteDescriptor = Rd}.

cre_streamDesc(Id, P) ->
    #'StreamDescriptor'{streamID = Id, streamParms = P}.


%% "Local" related
cre_localControlDesc(Mode) ->
    #'LocalControlDescriptor'{streamMode = Mode}.
cre_localControlDesc(Mode, Parms) ->
    #'LocalControlDescriptor'{streamMode = Mode, propertyParms = Parms }.

cre_localRemoteDesc(Grps) ->
    #'LocalRemoteDescriptor'{propGrps = Grps}.


%% DigitMap related
cre_digitMapDesc(Value) when is_record(Value, 'DigitMapValue') ->
    #'DigitMapDescriptor'{digitMapValue = Value};
cre_digitMapDesc(Name) ->
    #'DigitMapDescriptor'{digitMapName = Name}.

cre_digitMapDesc(Name, Val) ->
    #'DigitMapDescriptor'{digitMapName = Name, digitMapValue = Val}.

cre_digitMapValue(Body) ->
    #'DigitMapValue'{digitMapBody = Body}.

cre_digitMapValue(Body, Start, Short, Long) ->
    #'DigitMapValue'{startTimer   = Start,
		     shortTimer   = Short,
		     longTimer    = Long,
		     digitMapBody = Body}.

%% Media related
cre_mediaDesc(StreamDesc) ->
    #'MediaDescriptor'{streams = {multiStream, [StreamDesc]}}.


%% Notify related
cre_notifyReq(Tid, EvsDesc) ->
    #'NotifyRequest'{terminationID = Tid, observedEventsDescriptor = EvsDesc}.

cre_notifyReply(Tid) ->
    #'NotifyReply'{terminationID = Tid}.


%% Subtract related
cre_subtractReq(Tid, Desc) ->
    #'SubtractRequest'{terminationID = Tid, auditDescriptor = Desc}.


%% Audit related
cre_auditDesc(Tokens) ->
    #'AuditDescriptor'{auditToken = Tokens}.

cre_auditReq(Tid, Desc) ->
    #'AuditRequest'{terminationID   = Tid, auditDescriptor = Desc}.

cre_auditRes(Tid, Res) ->
    #'AuditResult'{terminationID = Tid, terminationAuditResult = Res}.


%% AMM/AMMS related
cre_ammReq(Tid, Descs) ->
    #'AmmRequest'{terminationID = Tid, descriptors = Descs}.

cre_ammsReply(Tid) ->
    #'AmmsReply'{terminationID = Tid}.
cre_ammsReply(Tid, Descs) ->
    #'AmmsReply'{terminationID = Tid, terminationAudit = Descs}.


%% Command related
cre_commandReq(Cmd) ->
    #'CommandRequest'{command = Cmd}.


%% Actions related
cre_requestedActions(DmName) ->
    #'RequestedActions'{eventDM = {digitMapName, DmName}}.


%% Signal related
cre_signal(Name) ->
    #'Signal'{signalName = Name}.


%% Others
cre_timeNotation(D,T) ->
    #'TimeNotation'{date = D, time = T}.

cre_packagesItem(_Name, _Ver) ->
    #'PackagesItem'{packageName = "nt", packageVersion = 1}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flex_init(Config) ->
    megaco_codec_flex_lib:init(Config).

flex_finish(Config) ->
    megaco_codec_flex_lib:finish(Config).

flex_scanner_conf(Config) ->
    megaco_codec_flex_lib:scanner_conf(Config).

start_flex_scanner() ->
    megaco_codec_flex_lib:start().

stop_flex_scanner(Pid) ->
    megaco_codec_flex_lib:stop(Pid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t(F,A) ->
    p(printable(get(severity),trc),trc,F,A).

d(F,A) ->
    p(printable(get(severity),dbg),dbg,F,A).

l(F,A) ->
    p(printable(get(severity),log),log,F,A).

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
    io:format("~s: " ++ F ++ "~n", [image_of(L)|A]);
p(_,_,_,_) ->
    ok.

image_of(trc) ->
    "T";
image_of(dbg) ->
    "D";
image_of(log) ->
    "L";
image_of(err) ->
    "E";
image_of(L) ->
    io_lib:format("~p",[L]).


