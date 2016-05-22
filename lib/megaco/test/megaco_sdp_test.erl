%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%% Purpose: Test application config
%%----------------------------------------------------------------------

-module(megaco_sdp_test).

-export([all/0,groups/0,init_per_group/2,end_per_group/2,
	 decode_encode/1,

	 otp8123/1, 

	 init_per_testcase/2, end_per_testcase/2, 

	 t/0, t/1]).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include_lib("megaco/include/megaco_sdp.hrl").

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).

%% Test server callbacks
init_per_testcase(Case, Config) ->
    megaco_test_lib:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->
    megaco_test_lib:end_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


all() -> 
    [decode_encode, {group, tickets}].

groups() -> 
    [{tickets, [], [otp8123]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_encode(suite) ->
    [];
decode_encode(Config) when is_list(Config) ->
    io:format("decode_encode -> entry with"
	      "~n   Config: ~p"
	      "~n", [Config]),

    %%-------------------------------------
    %% Test data
    %%-------------------------------------

    %% -- (PP) (ok) --
    io:format("setup for protocol version 01 (ok)~n", []),
    PP_V01_V    = 0, 
    PP_V01      = cre_PropertyParm_v(PP_V01_V),
    SDP_V01     = cre_sdp_v(PP_V01_V), 
    PP_V01_Exp  = {ok, SDP_V01}, 
    SDP_V01_Exp = {ok, PP_V01}, 


    %% -- (PP) (ok) --
    io:format("setup for protocol version 02 (ok)~n", []),
    PP_V02_V    = 100, 
    PP_V02      = cre_PropertyParm_v(PP_V02_V),
    SDP_V02     = cre_sdp_v(PP_V02_V), 
    PP_V02_Exp  = {ok, SDP_V02}, 
    SDP_V02_Exp = {ok, PP_V02}, 


    %% -- (PP) (error) --
    io:format("setup for protocol version 03 (error)~n", []),
    PP_V03_V    = "sune", 
    PP_V03      = cre_PropertyParm_v(PP_V03_V),
    SDP_V03     = cre_sdp_v(PP_V03_V), 
    PP_V03_Exp  = {error, {invalid_protocol_version, PP_V03_V}}, 
    SDP_V03_Exp = PP_V03_Exp, 


    %% -- (PP) (ok) --
    io:format("setup for connection info 01 (ok)~n", []),
    PP_C01_CA    = "123.123.123.120",
    PP_C01       = cre_PropertyParm_c(ip4, PP_C01_CA),
    SDP_C01      = cre_sdp_c(ip4, PP_C01_CA),
    PP_C01_Exp   = {ok, SDP_C01}, 
    SDP_C01_Exp  = {ok, PP_C01}, 


    %% -- (PP) (ok) --
    io:format("setup for connection info 02 (ok)~n", []),
    PP_C02_TTL   = 121, 
    PP_C02_Base  = "123.123.123.121", 
    PP_C02       = cre_PropertyParm_c(ip4, PP_C02_Base, PP_C02_TTL), 
    SDP_C02_CA   = #megaco_sdp_c_conn_addr{base = PP_C02_Base,
					   ttl  = PP_C02_TTL}, 
    SDP_C02      = cre_sdp_c(ip4, SDP_C02_CA), 
    PP_C02_Exp   = {ok, SDP_C02}, 
    SDP_C02_Exp  = {ok, PP_C02}, 


    %% -- (PP) (ok) --
    io:format("setup for connection info 03 (ok)~n", []),
    PP_C03_CA    = "123.123.123.122", 
    PP_C03       = cre_PropertyParm_c(ip4, PP_C03_CA ++ "/"), 
    SDP_C03      = cre_sdp_c(ip4, PP_C03_CA), 
    PP_C03_Exp   = {ok, SDP_C03}, 
    SDP_C03_Exp  = {ok, cre_PropertyParm_c(ip4, PP_C03_CA)}, 


    %% -- (PP) (error) --
    io:format("setup for connection info 04 (error)~n", []),
    PP_C04_Base = "123.123.123.123", 
    PP_C04_TTL  = "sune", 
    PP_C04_CA   = PP_C04_Base ++ "/" ++ PP_C04_TTL, 
    PP_C04      = cre_PropertyParm_c(ip4, PP_C04_CA), 
    PP_C04_Exp  = {error, {invalid_connection_data_conn_addr_ttl, "sune"}}, 


    %% -- (PP) (ok) --
    io:format("setup for connection info 05 (ok)~n", []),
    PP_C05_TTL   = 124, 
    PP_C05_NOF   = 224, 
    PP_C05_Base  = "123.123.123.124", 
    PP_C05       = cre_PropertyParm_c(ip4, PP_C05_Base, PP_C05_TTL, 
				      PP_C05_NOF), 
    SDP_C05_CA   = #megaco_sdp_c_conn_addr{base   = PP_C05_Base,
					   ttl    = PP_C05_TTL,
					   num_of = PP_C05_NOF},
    SDP_C05      = cre_sdp_c(ip4, SDP_C05_CA), 
    PP_C05_Exp   = {ok, SDP_C05},
    SDP_C05_Exp  = {ok, PP_C05}, 


    %% -- (PP) (ok) --
    io:format("setup for connection info 06 (ok)~n", []),
    PP_C06_TTL   = 125, 
    PP_C06_Base  = "123.123.123.125", 
    PP_C06_CA    = PP_C06_Base ++ "/" ++ integer_to_list(PP_C06_TTL) ++ "/",
    PP_C06       = cre_PropertyParm_c(ip4, PP_C06_CA),
    SDP_C06_CA   = #megaco_sdp_c_conn_addr{base = PP_C06_Base,
					   ttl  = PP_C06_TTL},
    SDP_C06      = cre_sdp_c(ip4, SDP_C06_CA), 
    PP_C06_Exp   = {ok, SDP_C06},
    SDP_C06_Exp  = {ok, cre_PropertyParm_c(ip4, PP_C06_Base ++ "/" ++ integer_to_list(PP_C06_TTL))}, 


    %% -- (PP) (error) --
    io:format("setup for connection info 07 (ok)~n", []),
    PP_C07_NOF  = "sune", 
    PP_C07_TTL  = 125, 
    PP_C07_Base = "123.123.123.126", 
    PP_C07_CA   = PP_C07_Base ++ "/" ++ 
	integer_to_list(PP_C07_TTL) ++ "/" ++ PP_C07_NOF,
    PP_C07      = cre_PropertyParm_c(ip4, PP_C07_CA), 
    SDP_C07_CA  = #megaco_sdp_c_conn_addr{base   = PP_C07_Base,
					  ttl    = PP_C07_TTL,
					  num_of = PP_C07_NOF}, 
    SDP_C07     = cre_sdp_c(ip4, SDP_C07_CA), 
    PP_C07_Exp  = {error, {invalid_connection_data_conn_addr_num_of, PP_C07_NOF}}, 
    SDP_C07_Exp = {error, {invalid_connection_data_conn_addr_num_of, PP_C07_NOF}}, 


    %% -- (PP) (ok) --
    io:format("setup for connection info 08 (ok)~n", []),
    PP_C08_CA   = "FF1E:03AD::7F2E:172A:1E24",
    PP_C08      = cre_PropertyParm_c(ip6, PP_C08_CA),
    SDP_C08     = cre_sdp_c(ip6, PP_C08_CA), 
    PP_C08_Exp  = {ok, SDP_C08},
    SDP_C08_Exp = {ok, PP_C08}, 


    %% -- (PP) (ok) --
    io:format("setup for media announcement 01 (ok)~n", []),
    PP_M01_Media     = audio,
    PP_M01_Port      = 2000,
    PP_M01_Transport = "RTP/AVP",
    PP_M01_FMT_LST   = ["0"],
    PP_M01           = cre_PropertyParm_m(PP_M01_Media, 
					  PP_M01_Port, 
					  PP_M01_Transport,
					  PP_M01_FMT_LST),
    SDP_M01     = cre_sdp_m(PP_M01_Media, PP_M01_Port, 
			    PP_M01_Transport, PP_M01_FMT_LST), 
    PP_M01_Exp  = {ok, SDP_M01},
    SDP_M01_Exp = {ok, PP_M01},


    %% -- (PP) (ok) --
    io:format("setup for media announcement 02 (ok)~n", []),
    PP_M02_Media     = audio,
    PP_M02_Port      = 2000,
    PP_M02_NOP       = 2,
    PP_M02_Transport = "RTP/AVP",
    PP_M02_FMT_LST   = ["0"],
    PP_M02           = cre_PropertyParm_m(PP_M02_Media, PP_M02_Port, 
					  PP_M02_NOP, PP_M02_Transport, 
					  PP_M02_FMT_LST),
    SDP_M02          = cre_sdp_m(PP_M02_Media, PP_M02_Port, PP_M02_NOP,
				 PP_M02_Transport, PP_M02_FMT_LST),
    PP_M02_Exp       = {ok, SDP_M02}, 
    SDP_M02_Exp      = {ok, PP_M02}, 

    %% -- (PP) (ok) --
    io:format("setup for origin 01 (ok)~n", []),
    PP_O01_Name = "kalle",
    PP_O01_SID  = 1414,
    PP_O01_V    = 2,
    PP_O01_AT   = ip4,
    PP_O01_A    = "126.12.64.4", 
    PP_O01      = cre_PropertyParm_o(PP_O01_Name, PP_O01_SID, PP_O01_V,
				     PP_O01_AT, PP_O01_A),
    SDP_O01     = cre_sdp_o(PP_O01_Name, PP_O01_SID, PP_O01_V, 
			    PP_O01_AT, PP_O01_A),
    PP_O01_Exp  = {ok, SDP_O01},
    SDP_O01_Exp = {ok, PP_O01},


    %% -- (PP) (ok) --
    io:format("setup for origin 02 (ok)~n", []),
    PP_O02_Name = "bobbe",
    PP_O02_SID  = 1515,
    PP_O02_V    = 3,
    PP_O02_NT   = in,
    PP_O02_AT   = ip6,
    PP_O02_A    = "2201:056D::112E:144A:1E24", 
    PP_O02      = cre_PropertyParm_o(PP_O02_Name, PP_O02_SID, PP_O02_V,
				    PP_O02_NT, PP_O02_AT, PP_O02_A),
    SDP_O02     = cre_sdp_o(PP_O02_Name, PP_O02_SID, PP_O02_V, PP_O02_NT,
			    PP_O02_AT, PP_O02_A),
    PP_O02_Exp  = {ok, SDP_O02}, 
    SDP_O02_Exp = {ok, PP_O02}, 


    %% -- (PP) (ok) --
    PP_A01_PL   = 2, 
    PP_A01_EN   = "G726-32", 
    PP_A01_CR   = 8000, 
    PP_A01      = cre_PropertyParm_rtpmap(PP_A01_PL, PP_A01_EN, PP_A01_CR),
    SDP_A01     = cre_sdp_a_rtpmap(PP_A01_PL, PP_A01_EN, PP_A01_CR),
    PP_A01_Exp  = {ok, SDP_A01},
    SDP_A01_Exp = {ok, PP_A01}, 


    %% -- (PP) (ok) --
    PP_A02_PL   = 2, 
    PP_A02_EN   = "xxx", 
    PP_A02_CR   = 42, 
    PP_A02_EP   = ["1","2","3"],
    PP_A02      = cre_PropertyParm_rtpmap(PP_A02_PL, PP_A02_EN, 
					  PP_A02_CR, PP_A02_EP),
    SDP_A02     = cre_sdp_a_rtpmap(PP_A02_PL, PP_A02_EN, PP_A02_CR, PP_A02_EP),
    PP_A02_Exp  = {ok, SDP_A02},
    SDP_A02_Exp = {ok, PP_A02}, 


    %% -- (PP) (ok) --
    PP_A03_PT   = 12, 
    PP_A03      = cre_PropertyParm_ptime(PP_A03_PT), 
    SDP_A03     = cre_sdp_a_ptime(PP_A03_PT),
    PP_A03_Exp  = {ok, SDP_A03}, 
    SDP_A03_Exp = {ok, PP_A03},


    %% -- (PP) (error) --
    PP_A04_PT   = "sune", 
    PP_A04      = cre_PropertyParm_ptime(PP_A04_PT), 
    SDP_A04     = cre_sdp_a_ptime(PP_A04_PT), 
    PP_A04_Exp  = {error, {invalid_ptime_packet_time, PP_A04_PT}},
    SDP_A04_Exp = {error, {invalid_ptime_packet_time, PP_A04_PT}}, 


    %% -- (PP) (ok) --
    PP_A05_QA   = 10, 
    PP_A05      = cre_PropertyParm_quality(PP_A05_QA), 
    SDP_A05     = cre_sdp_a_quality(PP_A05_QA), 
    PP_A05_Exp  = {ok, SDP_A05}, 
    SDP_A05_Exp = {ok, PP_A05}, 


    %% -- (PP) (error) --
    PP_A06_QA   = "sune", 
    PP_A06      = cre_PropertyParm_quality(PP_A06_QA), 
    SDP_A06     = cre_sdp_a_quality(PP_A06_QA), 
    PP_A06_Exp  = {error, {invalid_quality_quality, PP_A06_QA}}, 
    SDP_A06_Exp = {error, {invalid_quality_quality, PP_A06_QA}}, 


    %% -- (PP) (ok) --
    PP_A07_A    = "recvonly", 
    PP_A07      = cre_PropertyParm_a(PP_A07_A), 
    SDP_A07     = cre_sdp_a(PP_A07_A), 
    PP_A07_Exp  = {ok, SDP_A07}, 
    SDP_A07_Exp = {ok, PP_A07}, 


    %% -- (PP) (ok) --
    PP_A08_V    = portrait, 
    PP_A08      = cre_PropertyParm_orient(PP_A08_V), 
    SDP_A08     = cre_sdp_a_orient(PP_A08_V),
    PP_A08_Exp  = {ok, SDP_A08}, 
    SDP_A08_Exp = {ok, PP_A08}, 


    %% -- (PP) (ok) --
    PP_A09_V    = landscape, 
    PP_A09      = cre_PropertyParm_orient(PP_A09_V), 
    SDP_A09     = cre_sdp_a_orient(PP_A09_V),
    PP_A09_Exp  = {ok, SDP_A09}, 
    SDP_A09_Exp = {ok, PP_A09}, 


    %% -- (PP) (ok) --
    PP_A10_V    = seascape, 
    PP_A10      = cre_PropertyParm_orient(PP_A10_V), 
    SDP_A10     = cre_sdp_a_orient(PP_A10_V),
    PP_A10_Exp  = {ok, SDP_A10}, 
    SDP_A10_Exp = {ok, PP_A10}, 


    %% -- (PP) (error) --
    PP_A11_V    = gurka, 
    SDP_A11_V   = atom_to_list(PP_A11_V),
    PP_A11      = cre_PropertyParm_orient(PP_A11_V), 
    SDP_A11     = cre_sdp_a_orient(PP_A11_V),
    PP_A11_Exp  = {error, {invalid_orient_orientation, SDP_A11_V}}, 
    SDP_A11_Exp = {error, {invalid_orient_orientation, PP_A11_V}}, 


    %% -- (PP) (ok) --
    PP_A12_V    = "gurka", 
    PP_A12      = cre_PropertyParm_cat(PP_A12_V), 
    SDP_A12     = cre_sdp_a_cat(PP_A12_V),
    PP_A12_Exp  = {ok, SDP_A12}, 
    SDP_A12_Exp = {ok, PP_A12}, 


    %% -- (PP) (ok) --
    PP_A13_V    = "gurka", 
    PP_A13      = cre_PropertyParm_keywds(PP_A13_V), 
    SDP_A13     = cre_sdp_a_keywds(PP_A13_V),
    PP_A13_Exp  = {ok, SDP_A13}, 
    SDP_A13_Exp = {ok, PP_A13}, 


    %% -- (PP) (ok) --
    PP_A14_V    = "gurka 1.0", 
    PP_A14      = cre_PropertyParm_tool(PP_A14_V), 
    SDP_A14     = cre_sdp_a_tool(PP_A14_V),
    PP_A14_Exp  = {ok, SDP_A14}, 
    SDP_A14_Exp = {ok, PP_A14}, 


    %% -- (PP) (ok) --
    PP_A15_V    = 15, 
    PP_A15      = cre_PropertyParm_maxptime(PP_A15_V), 
    SDP_A15     = cre_sdp_a_maxptime(PP_A15_V),
    PP_A15_Exp  = {ok, SDP_A15}, 
    SDP_A15_Exp = {ok, PP_A15},


    %% -- (PP) (error) --
    PP_A16_V    = "gurka", 
    PP_A16      = cre_PropertyParm_maxptime(PP_A16_V), 
    SDP_A16     = cre_sdp_a_maxptime(PP_A16_V),
    PP_A16_Exp  = {error, {invalid_maxptime_maximum_packet_time, PP_A16_V}},
    SDP_A16_Exp = {error, {invalid_maxptime_maximum_packet_time, PP_A16_V}},


    %% -- (PP) (ok) --
    PP_A17_V    = "H332", 
    PP_A17      = cre_PropertyParm_type(PP_A17_V ), 
    SDP_A17     = cre_sdp_a_type(PP_A17_V),
    PP_A17_Exp  = {ok, SDP_A17}, 
    SDP_A17_Exp = {ok, PP_A17},


    %% -- (PP) (ok) --
    PP_A18_V    = "ISO-8859-1", 
    PP_A18      = cre_PropertyParm_charset(PP_A18_V), 
    SDP_A18     = cre_sdp_a_charset(PP_A18_V),
    PP_A18_Exp  = {ok, SDP_A18}, 
    SDP_A18_Exp = {ok, PP_A18},


    %% -- (PP) (ok) --
    PP_A19_PT   = "gurka", 
    PP_A19      = cre_PropertyParm_sdplang(PP_A19_PT), 
    SDP_A19     = cre_sdp_a_sdplang(PP_A19_PT),
    PP_A19_Exp  = {ok, SDP_A19}, 
    SDP_A19_Exp = {ok, PP_A19},


    %% -- (PP) (ok) --
    PP_A20_PT   = "gurka", 
    PP_A20      = cre_PropertyParm_lang(PP_A20_PT), 
    SDP_A20     = cre_sdp_a_lang(PP_A20_PT),
    PP_A20_Exp  = {ok, SDP_A20}, 
    SDP_A20_Exp = {ok, PP_A20},


    %% -- (PP) (ok) --
    PP_A21_PT   = "21.0", 
    PP_A21      = cre_PropertyParm_framerate(PP_A21_PT), 
    SDP_A21     = cre_sdp_a_framerate(PP_A21_PT),
    PP_A21_Exp  = {ok, SDP_A21}, 
    SDP_A21_Exp = {ok, PP_A21},


%%     %% -- (PP) (ok) --
%%     PP_A22_PT   = "ISO-8859-1", 
%%     PP_A22      = cre_PropertyParm_(PP_A22_T), 
%%     SDP_A22     = cre_sdp_a_(PP_A22_PT),
%%     PP_A22_Exp  = {ok, SDP_A22}, 
%%     SDP_A22_Exp = {ok, PP_A22},


    %% -- (PP) (ok) --
    PP_A23_FORMAT = "125",
    PP_A23_PARAM  = "profile-level-id=222; max-br=1212; max-mbps=20200", 
    PP_A23        = cre_PropertyParm_a_fmtp(PP_A23_FORMAT, PP_A23_PARAM),
    SDP_A23       = cre_sdp_a_fmtp(PP_A23_FORMAT, PP_A23_PARAM), 
    PP_A23_Exp    = {ok, SDP_A23}, 
    SDP_A23_Exp   = {ok, PP_A23}, 


    %% -- (PP) (ok) --
    PP_B01_MOD  = "2",
    PP_B01_BW   = 523,
    PP_B01      = cre_PropertyParm_b(PP_B01_MOD, PP_B01_BW),
    SDP_B01     = cre_sdp_b(PP_B01_MOD, PP_B01_BW), 
    PP_B01_Exp  = {ok, SDP_B01}, 
    SDP_B01_Exp = {ok, PP_B01}, 


    %% -- (PP) (error) --
    PP_B02_B   = "sune", 
    PP_B02     = cre_PropertyParm("b", PP_B02_B), 
    PP_B02_Exp = {error, {invalid_PropertyParm,
			  {bandwidth_info, PP_B02_B, [PP_B02_B]}}}, 

    
    %% -- (PP) (ok) --
    PP_B03_MOD  = "X",
    PP_B03_BW   = 525,
    PP_B03      = cre_PropertyParm_b(PP_B03_MOD, PP_B03_BW),
    SDP_B03     = cre_sdp_b(PP_B03_MOD, PP_B03_BW), 
    PP_B03_Exp  = {ok, SDP_B03}, 
    SDP_B03_Exp = {ok, PP_B03}, 


    %% -- (PP) (error) --
    PP_B04_BWT  = "X",
    PP_B04_BW   = "sune", 
    PP_B04      = cre_PropertyParm_b(PP_B04_BWT, PP_B04_BW),
    SDP_B04     = cre_sdp_b(PP_B04_BWT, PP_B04_BW), 
    PP_B04_Exp  = {error, {invalid_bandwidth_bandwidth, PP_B04_BW}}, 
    SDP_B04_Exp = {error, {invalid_bandwidth_bandwidth, PP_B04_BW}}, 


    %% -- (PP) (ok) --
    PP_T01_START = 1200,
    PP_T01_STOP  = 1300,
    PP_T01       = cre_PropertyParm_t(PP_T01_START, PP_T01_STOP),
    SDP_T01      = cre_sdp_t(PP_T01_START, PP_T01_STOP), 
    PP_T01_Exp   = {ok, SDP_T01}, 
    SDP_T01_Exp  = {ok, PP_T01}, 


    %% -- (PP) (ok) --
    PP_R01_RPT  = "10",
    PP_R01_DUR  = "100",
    PP_R01_LOO  = ["2", "4", "6"],
    PP_R01      = cre_PropertyParm_r(PP_R01_RPT, PP_R01_DUR, PP_R01_LOO),
    SDP_R01     = cre_sdp_r(PP_R01_RPT, PP_R01_DUR, PP_R01_LOO), 
    PP_R01_Exp  = {ok, SDP_R01}, 
    SDP_R01_Exp = {ok, PP_R01}, 


    %% -- (PP) (ok) --
    PP_Z01_LOA_V1 = #megaco_sdp_z_adjustement{time   = "12121212", 
					      offset = "-1h"},
    PP_Z01_LOA_V2 = #megaco_sdp_z_adjustement{time   = "34343434", 
					      offset = "0"},
    PP_Z01_LOA  = [PP_Z01_LOA_V1, PP_Z01_LOA_V2], 
    PP_Z01      = cre_PropertyParm_z(PP_Z01_LOA), 
    SDP_Z01     = cre_sdp_z(PP_Z01_LOA), 
    PP_Z01_Exp  = {ok, SDP_Z01}, 
    SDP_Z01_Exp = {ok, PP_Z01}, 


    %% -- (PP) (error) --
    PP_Z02      = cre_PropertyParm("z", []), 
    SDP_Z02     = cre_sdp_z([]), 
    PP_Z02_Exp  = {error, {invalid_tzones_list_of_adjustments, []}}, 
    SDP_Z02_Exp = {error, {invalid_tzones_list_of_adjustments, []}}, 


    %% -- (PP) (ok) --
    PP_K01_M    = prompt,
    PP_K01_EK   = undefined,
    PP_K01      = cre_PropertyParm_k(PP_K01_M, PP_K01_EK),
    SDP_K01     = cre_sdp_k(PP_K01_M), 
    PP_K01_Exp  = {ok, SDP_K01}, 
    SDP_K01_Exp = {ok, PP_K01}, 


    %% -- (PP) (ok) --
    PP_K02_M    = clear,
    PP_K02_EK   = "whatever",
    PP_K02      = cre_PropertyParm_k(PP_K02_M, PP_K02_EK),
    SDP_K02     = cre_sdp_k(PP_K02_M, PP_K02_EK), 
    PP_K02_Exp  = {ok, SDP_K02}, 
    SDP_K02_Exp = {ok, PP_K02}, 


    %% -- (PP) (ok) --
    PP_K03_M    = "method", 
    PP_K03_EK   = "key", 
    PP_K03      = cre_PropertyParm_k(PP_K03_M, PP_K03_EK),
    SDP_K03     = cre_sdp_k(PP_K03_M, PP_K03_EK), 
    PP_K03_Exp  = {ok, SDP_K03}, 
    SDP_K03_Exp = {ok, PP_K03}, 


    %% -- (PP) (ok) --
    PP_S01_SN   = "new session", 
    PP_S01      = cre_PropertyParm_s(PP_S01_SN), 
    SDP_S01     = cre_sdp_s(PP_S01_SN), 
    PP_S01_Exp  = {ok, SDP_S01}, 
    SDP_S01_Exp = {ok, PP_S01}, 


    %% -- (PP) (ok) --
    PP_I01_SD   = "Session and Media Information", 
    PP_I01      = cre_PropertyParm_i(PP_I01_SD), 
    SDP_I01     = cre_sdp_i(PP_I01_SD), 
    PP_I01_Exp  = {ok, SDP_I01}, 
    SDP_I01_Exp = {ok, PP_I01}, 


    %% -- (PP) (ok) --
    PP_U01_URI  = "http://www.erlang.org/", 
    PP_U01      = cre_PropertyParm_u(PP_U01_URI), 
    SDP_U01     = cre_sdp_u(PP_U01_URI), 
    PP_U01_Exp  = {ok, SDP_U01}, 
    SDP_U01_Exp = {ok, PP_U01}, 


    %% -- (PP) (ok) --
    PP_E01_EMAIL = "kalle@company.se", 
    PP_E01       = cre_PropertyParm_e(PP_E01_EMAIL), 
    SDP_E01      = cre_sdp_e(PP_E01_EMAIL), 
    PP_E01_Exp   = {ok, SDP_E01}, 
    SDP_E01_Exp  = {ok, PP_E01}, 


    %% -- (PP) (ok) --
    PP_P01_PHONE = "+1 713 555 1234", 
    PP_P01       = cre_PropertyParm_p(PP_P01_PHONE), 
    SDP_P01      = cre_sdp_p(PP_P01_PHONE), 
    PP_P01_Exp   = {ok, SDP_P01}, 
    SDP_P01_Exp  = {ok, PP_P01}, 


    %% -- (PP) (error) --
    PP_N01     = cre_PropertyParm("not_recognized", "whatever"), 
    PP_N01_Exp = {error, undefined_PropertyParm}, 


    %% -- (PG) (ok) --
    PG01    = [{PP_V01, ok}, 
	       {PP_C01, ok}, 
	       {PP_M01, ok}], 


    %% -- (PG) (ok) --
    PG02    = [{PP_V02, ok}, 
	       {PP_C05, ok}, 
	       {PP_A02, ok}], 


    %% -- (PG) (error) --
    PG03    = [{PP_V03, error}, 
	       {PP_C08, ok}, 
	       {PP_M02, ok}], 


    %% -- (PG) (error) --
    PG04   = [{PP_V02, ok}, 
	      {PP_C04, error},
	      {PP_C07, error}],


    %% -- (PGs) (ok) --
    PGS01 = [PG01, PG02], 


    %% -- (PGs) (error) --
    PGS02 = [PG01, PG04], 


    Instructions = 
	[
	 pp_dec_instruction("version 01 - dec [ok]", PP_V01,  PP_V01_Exp), 
	 pp_enc_instruction("version 01 - enc [ok]", SDP_V01, SDP_V01_Exp),
	 pp_dec_instruction("version 02 - dec [ok]", PP_V02,  PP_V02_Exp),
	 pp_enc_instruction("version 02 - enc [ok]", SDP_V02, SDP_V02_Exp),
	 pp_dec_instruction("version 03 - dec [error]", PP_V03,  PP_V03_Exp),
	 pp_enc_instruction("version 03 - enc [error]", SDP_V03, SDP_V03_Exp),
	 pp_dec_instruction("connection info 01 - dec [ok]", PP_C01,  PP_C01_Exp),    
	 pp_enc_instruction("connection info 01 - enc [ok]", SDP_C01, SDP_C01_Exp),    
	 pp_dec_instruction("connection info 02 - dec [ok]", PP_C02,  PP_C02_Exp),    
	 pp_enc_instruction("connection info 02 - enc [ok]", SDP_C02, SDP_C02_Exp),    
	 pp_dec_instruction("connection info 03 - dec [ok]", PP_C03,  PP_C03_Exp),    
	 pp_enc_instruction("connection info 03 - enc [ok]", SDP_C03, SDP_C03_Exp),    
	 pp_dec_instruction("connection info 04 - dec [error]", PP_C04,  PP_C04_Exp),    

	 pp_dec_instruction("connection info 05 - dec [ok]", PP_C05,  PP_C05_Exp),    
	 pp_enc_instruction("connection info 05 - enc [ok]", SDP_C05, SDP_C05_Exp),    
	 pp_dec_instruction("connection info 06 - dec [ok]", PP_C06,  PP_C06_Exp),    
	 pp_enc_instruction("connection info 06 - enc [ok]", SDP_C06, SDP_C06_Exp),    
	 pp_dec_instruction("connection info 07 - dec [error]", PP_C07,  PP_C07_Exp),    
	 pp_enc_instruction("connection info 07 - enc [error]", SDP_C07, SDP_C07_Exp),    
	 pp_dec_instruction("connection info 08 - dec [ok]", PP_C08,  PP_C08_Exp),    
	 pp_enc_instruction("connection info 08 - enc [ok]", SDP_C08, SDP_C08_Exp),    
	 pp_dec_instruction("media announcement 01 - dec [ok]", PP_M01,  PP_M01_Exp),    
	 pp_enc_instruction("media announcement 01 - enc [ok]", SDP_M01, SDP_M01_Exp),    
	 pp_dec_instruction("media announcement 02 - dec [ok]", PP_M02,  PP_M02_Exp),    
	 pp_enc_instruction("media announcement 02 - enc [ok]", SDP_M02, SDP_M02_Exp),    
	 pp_dec_instruction("origin 01 - dec [ok]", PP_O01,  PP_O01_Exp),    
	 pp_enc_instruction("origin 01 - enc [ok]", SDP_O01, SDP_O01_Exp),    
	 pp_dec_instruction("origin 02 - dec [ok]", PP_O02,  PP_O02_Exp),    
	 pp_enc_instruction("origin 02 - enc [ok]", SDP_O02, SDP_O02_Exp),    
	 pp_dec_instruction("attributes 01 - dec [ok]", PP_A01,  PP_A01_Exp),    
	 pp_enc_instruction("attributes 01 - enc [ok]", SDP_A01, SDP_A01_Exp),    
	 pp_dec_instruction("attributes 02 - dec [ok]", PP_A02,  PP_A02_Exp),    
	 pp_enc_instruction("attributes 02 - enc [ok]", SDP_A02, SDP_A02_Exp),    
	 pp_dec_instruction("attributes 03 - dec [ok]", PP_A03,  PP_A03_Exp),    
	 pp_enc_instruction("attributes 03 - enc [ok]", SDP_A03, SDP_A03_Exp),    
	 pp_dec_instruction("attributes 04 - dec [error]", PP_A04,  PP_A04_Exp),    
	 pp_enc_instruction("attributes 04 - enc [error]", SDP_A04, SDP_A04_Exp),    
	 pp_dec_instruction("attributes 05 - dec [ok]", PP_A05,  PP_A05_Exp),    
	 pp_enc_instruction("attributes 05 - enc [ok]", SDP_A05, SDP_A05_Exp),    
	 pp_dec_instruction("attributes 06 - dec [error]", PP_A06,  PP_A06_Exp),    
	 pp_enc_instruction("attributes 06 - dec [error]", SDP_A06, SDP_A06_Exp),    
	 pp_dec_instruction("attributes 07 - dec [ok]", PP_A07,  PP_A07_Exp),    
	 pp_enc_instruction("attributes 07 - enc [ok]", SDP_A07, SDP_A07_Exp),    
	 pp_dec_instruction("attributes 08 - dec [ok]", PP_A08,  PP_A08_Exp),    
	 pp_enc_instruction("attributes 08 - enc [ok]", SDP_A08, SDP_A08_Exp),    

	 pp_dec_instruction("attributes 09 - dec [ok]", PP_A09,  PP_A09_Exp),    
	 pp_enc_instruction("attributes 09 - enc [ok]", SDP_A09, SDP_A09_Exp),    
	 pp_dec_instruction("attributes 10 - dec [ok]", PP_A10,  PP_A10_Exp),    
	 pp_enc_instruction("attributes 10 - enc [ok]", SDP_A10, SDP_A10_Exp),    
	 pp_dec_instruction("attributes 11 - dec [error]", PP_A11,  PP_A11_Exp),    
	 pp_enc_instruction("attributes 11 - enc [error]", SDP_A11, SDP_A11_Exp),    

	 pp_dec_instruction("attributes 12 - dec [ok]", PP_A12,  PP_A12_Exp),    
	 pp_enc_instruction("attributes 12 - enc [ok]", SDP_A12, SDP_A12_Exp),    
	 pp_dec_instruction("attributes 13 - dec [ok]", PP_A13,  PP_A13_Exp),    
	 pp_enc_instruction("attributes 13 - enc [ok]", SDP_A13, SDP_A13_Exp),    
	 pp_dec_instruction("attributes 14 - dec [ok]", PP_A14,  PP_A14_Exp),    
	 pp_enc_instruction("attributes 14 - enc [ok]", SDP_A14, SDP_A14_Exp),    
	 pp_dec_instruction("attributes 15 - dec [ok]", PP_A15,  PP_A15_Exp),    
	 pp_enc_instruction("attributes 15 - enc [ok]", SDP_A15, SDP_A15_Exp),    
	 pp_dec_instruction("attributes 16 - dec [error]", PP_A16,  PP_A16_Exp),    
	 pp_enc_instruction("attributes 16 - enc [error]", SDP_A16, SDP_A16_Exp),    
	 pp_dec_instruction("attributes 17 - dec [ok]", PP_A17,  PP_A17_Exp),    
	 pp_enc_instruction("attributes 17 - enc [ok]", SDP_A17, SDP_A17_Exp),    
	 pp_dec_instruction("attributes 18 - dec [ok]", PP_A18,  PP_A18_Exp),    
	 pp_enc_instruction("attributes 18 - enc [ok]", SDP_A18, SDP_A18_Exp),    
	 pp_dec_instruction("attributes 19 - dec [ok]", PP_A19,  PP_A19_Exp),    
	 pp_enc_instruction("attributes 19 - enc [ok]", SDP_A19, SDP_A19_Exp),    
	 pp_dec_instruction("attributes 20 - dec [ok]", PP_A20,  PP_A20_Exp),    
	 pp_enc_instruction("attributes 20 - enc [ok]", SDP_A20, SDP_A20_Exp),    
	 pp_dec_instruction("attributes 21 - dec [ok]", PP_A21,  PP_A21_Exp),    
	 pp_enc_instruction("attributes 21 - enc [ok]", SDP_A21, SDP_A21_Exp),    
	 pp_dec_instruction("attributes 23 - dec [ok]", PP_A23,  PP_A23_Exp),    
	 pp_enc_instruction("attributes 24 - enc [ok]", SDP_A23, SDP_A23_Exp),    

	 pp_dec_instruction("bandwidth 01 - dec [ok]", PP_B01,  PP_B01_Exp),    
	 pp_enc_instruction("bandwidth 01 - enc [ok]", SDP_B01, SDP_B01_Exp),    
	 pp_dec_instruction("bandwidth 02 - dec [ok]", PP_B02,  PP_B02_Exp),    

	 pp_dec_instruction("bandwidth 03 - dec [ok]", PP_B03,  PP_B03_Exp),    
	 pp_enc_instruction("bandwidth 03 - enc [ok]", SDP_B03, SDP_B03_Exp),    
	 pp_dec_instruction("bandwidth 04 - dec [error]", PP_B04,  PP_B04_Exp),    
	 pp_enc_instruction("bandwidth 04 - enc [error]", SDP_B04, SDP_B04_Exp),    
	 pp_dec_instruction("times 01 - dec [ok]", PP_T01,  PP_T01_Exp),    
	 pp_enc_instruction("times 01 - enc [ok]", SDP_T01, SDP_T01_Exp),    
	 pp_dec_instruction("repeat times 01 - dec [ok]", PP_R01,  PP_R01_Exp),    
	 pp_enc_instruction("repeat times 01 - enc [ok]", SDP_R01, SDP_R01_Exp),    
	 pp_dec_instruction("time zones 01 - dec [ok]", PP_Z01,  PP_Z01_Exp),    
	 pp_enc_instruction("time zones 01 - enc [ok]", SDP_Z01, SDP_Z01_Exp),    
	 pp_dec_instruction("time zones 02 - dec [error]", PP_Z02,  PP_Z02_Exp),    
	 pp_enc_instruction("time zones 02 - enc [error]", SDP_Z02, SDP_Z02_Exp),    
	 pp_dec_instruction("encryption keys 01 - dec [ok]", PP_K01,  PP_K01_Exp),    
	 pp_enc_instruction("encryption keys 01 - enc [ok]", SDP_K01, SDP_K01_Exp),    
	 pp_dec_instruction("encryption keys 01 - dec [ok]", PP_K02,  PP_K02_Exp),    
	 pp_enc_instruction("encryption keys 01 - enc [ok]", SDP_K02, SDP_K02_Exp),    
	 pp_dec_instruction("encryption keys 01 - dec [ok]", PP_K03,  PP_K03_Exp),    
	 pp_enc_instruction("encryption keys 01 - enc [ok]", SDP_K03, SDP_K03_Exp),    
	 pp_dec_instruction("session name 01 - dec [ok]", PP_S01,  PP_S01_Exp),    
	 pp_enc_instruction("session name 01 - enc [ok]", SDP_S01, SDP_S01_Exp),    
	 pp_dec_instruction("session and media information 01 - dec [ok]", PP_I01,  PP_I01_Exp),    
	 pp_enc_instruction("session and media information 01 - enc [ok]", SDP_I01, SDP_I01_Exp),    
	 pp_dec_instruction("uri 01 - dec [ok]", PP_U01,  PP_U01_Exp),    
	 pp_enc_instruction("uri 01 - enc [ok]", SDP_U01, SDP_U01_Exp),    
	 pp_dec_instruction("email 01 - dec [ok]", PP_E01,  PP_E01_Exp),    
	 pp_enc_instruction("email 01 - enc [ok]", SDP_E01, SDP_E01_Exp),    
	 pp_dec_instruction("phone 01 - dec [ok]", PP_P01,  PP_P01_Exp),    
	 pp_enc_instruction("phone 01 - enc [ok]", SDP_P01, SDP_P01_Exp),    
	 pp_dec_instruction("undefined 01 - dec [error]", PP_N01,  PP_N01_Exp),    

	 pg_dec_instruction("property group 01 - ok", PG01),
	 pg_dec_instruction("property group 02 - ok", PG02),
	 pg_dec_instruction("property group 03 - error", PG03),
	 pg_dec_instruction("property group 04 - error", PG04),
	 
	 pgs_dec_instruction("property groups 01 - ok", PGS01),
	 pgs_dec_instruction("property groups 02 - error", PGS02)

	],
    exec(Instructions).


verify_decode_pg([], []) ->
    ok;
verify_decode_pg([{PP, error}|PPs], [{PP, _Err}|SDPs]) ->
    verify_decode_pg(PPs, SDPs); 
verify_decode_pg([{PP, ok}|_], [{PP, _Err}|_]) ->
    error;
verify_decode_pg([{_PP, _ExpStatus}|PG], [_SDP|SDP_PG]) ->
    verify_decode_pg(PG, SDP_PG).

verify_decode_pgs(PGS, SDP_PGS) ->
    verify_decode_pg(lists:flatten(PGS), lists:flatten(SDP_PGS)).


%% ===============================================================

otp8123(suite) ->
    [];
otp8123(Config) when is_list(Config) ->
    io:format("otp8123 -> entry with"
	      "~n   Config: ~p"
	      "~n", [Config]),

    Instructions = 
	[
	 pg_dec_instruction("property group 01 - dec [ok]", otp8123_pg1()),
	 pg_dec_instruction("property group 02 - dec [ok]", otp8123_pg2()),
	 pg_dec_instruction("property group 03 - dec [ok]", otp8123_pg3()),
	 pg_dec_instruction("property group 04 - dec [ok]", otp8123_pg4())
	],
    exec(Instructions),
    ok.

otp8123_pg1() ->
    PP1 = #'PropertyParm'{name = "m",
			  value = ["audio 49154 RTP/AVP 8"]},
    PP2 = #'PropertyParm'{name = "a",
			  value = ["maxptime: 30"]},
    PP3 = #'PropertyParm'{name = "a",
			  value = ["ptime:2"]},
    PP4 = #'PropertyParm'{name = "a",
			  value = ["tpmap:8 PCMA/8000/1"]},
    PG = [PP1, PP2, PP3, PP4],
    [{PP, ok} || PP <- PG].

otp8123_pg2() ->
    PP1 = #'PropertyParm'{name = "m",
			  value = ["audio 49154 RTP/AVP 8"]},
    PP2 = #'PropertyParm'{name = "a",
			  value = ["maxptime: 30 "]},
    PP3 = #'PropertyParm'{name = "a",
			  value = ["ptime:2"]},
    PP4 = #'PropertyParm'{name = "a",
			  value = ["tpmap:8 PCMA/8000/1"]},
    PG = [PP1, PP2, PP3, PP4],
    [{PP, ok} || PP <- PG].

otp8123_pg3() ->
    PP1 = #'PropertyParm'{name = "m",
			  value = ["audio 49154 RTP/AVP 8"]},
    PP2 = #'PropertyParm'{name = "a",
			  value = ["maxptime:30"]},
    PP3 = #'PropertyParm'{name = "a",
			  value = ["ptime: 2"]},
    PP4 = #'PropertyParm'{name = "a",
			  value = ["tpmap:8 PCMA/8000/1"]},
    PG = [PP1, PP2, PP3, PP4],
    [{PP, ok} || PP <- PG].

otp8123_pg4() ->
    PP1 = #'PropertyParm'{name = "m",
			  value = ["audio 49154 RTP/AVP 8"]},
    PP2 = #'PropertyParm'{name = "a",
			  value = ["maxptime:30"]},
    PP3 = #'PropertyParm'{name = "a",
			  value = ["ptime: 2 "]},
    PP4 = #'PropertyParm'{name = "a",
			  value = ["tpmap:8 PCMA/8000/1"]},
    PG = [PP1, PP2, PP3, PP4],
    [{PP, ok} || PP <- PG].



%% ===============================================================
%% 
%% Instruction engine
%% 

instr_verify_pp(Expected) ->
    fun(Res) ->
	    case Res of
		Expected -> ok;
		_        -> {error, Expected}
	    end
    end.

instr_verify_dec_pg(Data) ->
    fun({ok, SDP}) -> 
	    verify_decode_pg(Data, SDP);
       (_Bad)      -> 
	    error
    end.

instr_verify_dec_pgs(Data) ->
    fun({ok, SDP}) -> 
	    verify_decode_pgs(Data, SDP);
       (_Bad)      -> 
	    error
    end.


pp_dec_instruction(Desc, Data, Exp) ->
    dec_instruction(Desc, Data, instr_verify_pp(Exp)).

pp_enc_instruction(Desc, Data, Exp) ->
    enc_instruction(Desc, Data, instr_verify_pp(Exp)).

pg_dec_instruction(Desc, Data0) -> 
    Data = [D || {D, _} <- Data0], 
    dec_instruction(Desc, Data, instr_verify_dec_pg(Data0)).

pgs_dec_instruction(Desc, Data0) ->
    Data = [[D || {D, _} <- PG] || PG <- Data0], 
    dec_instruction(Desc, Data, instr_verify_dec_pgs(Data0)).

dec_instruction(Desc, Data, Verify) ->
    instruction(Desc, fun(D) -> megaco:decode_sdp(D) end, Data, Verify).

enc_instruction(Desc, Data, Verify) ->
    instruction(Desc, fun(D) -> megaco:encode_sdp(D) end, Data, Verify).

instruction(Desc, Cmd, Data, Verify) ->
    {Desc, Cmd, Data, Verify}.

exec(Instructions) ->
    exec(Instructions, []).

exec([], []) ->
    ok;
exec([], Acc) ->
    {error, lists:reverse(Acc)};
exec([Instr|Instructions], Acc) ->
    case exec_instruction(Instr) of
	ok ->
	    exec(Instructions, Acc);
	Error ->
	    exec(Instructions, [Error|Acc])
    end.
    

exec_instruction({Desc, Cmd, Data, Verify}) ->
    io:format("~n"
	      "*** Test ~s ***"
	      "~n", [Desc]),
    Res = (catch Cmd(Data)), 
    case (catch Verify(Res)) of
	ok ->
	    ok;
	error ->
	    {error, {instruction_failed, {Desc, Data, Res}}};
	{error, Expected} ->
	    {error, {instruction_failed, {Desc, Data, Res, Expected}}};
	Else ->
	    {error, {verification_error, {Desc, Data, Res, Else}}}
    end.


%% ===============================================================
%% 
%% Utility functions to generate PropertyParm records
%% 

cre_PropertyParm_p(Num) when is_list(Num) ->
    cre_PropertyParm("p", Num).

cre_sdp_p(PN) ->
    #megaco_sdp_p{phone_number = PN}.

cre_PropertyParm_e(Email) when is_list(Email) ->
    cre_PropertyParm("e", Email).

cre_sdp_e(E) ->
    #megaco_sdp_e{email = E}.

cre_PropertyParm_u(URI) when is_list(URI) ->
    cre_PropertyParm("u", URI).

cre_sdp_u(U) ->
    #megaco_sdp_u{uri = U}.

cre_PropertyParm_i(SessionDescr) when is_list(SessionDescr) ->
    cre_PropertyParm("i", SessionDescr).

cre_sdp_i(SD) ->
    #megaco_sdp_i{session_descriptor = SD}.

cre_PropertyParm_s(Name) when is_list(Name) ->
    cre_PropertyParm("s", Name).

cre_sdp_s(N) ->
    #megaco_sdp_s{name = N}.

cre_PropertyParm_k(prompt, _) ->
    cre_PropertyParm("k", "prompt");
cre_PropertyParm_k(clear, EncryptionKey) when is_list(EncryptionKey) ->
    cre_PropertyParm("k", "clear:" ++ EncryptionKey);
cre_PropertyParm_k(base64, EncryptionKey) when is_list(EncryptionKey) ->
    cre_PropertyParm("k", "base64:" ++ EncryptionKey);
cre_PropertyParm_k(uri, EncryptionKey) when is_list(EncryptionKey) ->
    cre_PropertyParm("k", "uri:" ++ EncryptionKey);
cre_PropertyParm_k(Method, EncryptionKey) 
  when is_list(Method) and is_list(EncryptionKey) ->
    cre_PropertyParm("k", Method ++ ":" ++ EncryptionKey).

cre_sdp_k(M) ->
    #megaco_sdp_k{method = M}.
cre_sdp_k(M, EK) ->
    #megaco_sdp_k{method = M, encryption_key = EK}.

cre_PropertyParm_z([H | _] = LOA) when is_record(H, megaco_sdp_z_adjustement) ->
    F = fun(#megaco_sdp_z_adjustement{time = T, offset = O}, Str) -> 
		Str ++ " " ++ T ++ " " ++ O
	end, 
    cre_PropertyParm("z", lists:foldl(F, [], LOA)).

cre_sdp_z(LOA) ->
    #megaco_sdp_z{list_of_adjustments = LOA}.

cre_PropertyParm_r(Repeat, Duration, ListOfOffsets) 
  when is_list(Repeat) and is_list(Duration) and is_list(ListOfOffsets) ->
    F = fun(Elem, Str) -> Str ++ " " ++ Elem end, 
    Val = Repeat ++ " " ++ Duration ++  lists:foldl(F, [], ListOfOffsets),
    cre_PropertyParm("r", Val).

cre_sdp_r(Repeat, Duration, ListOfOffsets) ->
    #megaco_sdp_r{repeat_interval = Repeat, 
		  active_duration = Duration, 
		  list_of_offsets = ListOfOffsets}.

cre_PropertyParm_t(Start, Stop) 
  when is_list(Start) and is_list(Stop) ->
    cre_PropertyParm("t", Start ++ " " ++ Stop);
cre_PropertyParm_t(Start, Stop) ->
    cre_PropertyParm_t(i2s(Start), i2s(Stop)).

cre_sdp_t(Start, Stop) ->
    #megaco_sdp_t{start = Start, stop = Stop}.

cre_PropertyParm_b(BwType, Bandwidth) 
  when is_list(BwType) and is_integer(Bandwidth) ->
    cre_PropertyParm_b(BwType, i2s(Bandwidth));
cre_PropertyParm_b(BwType, Bandwidth) 
  when is_list(BwType) and is_list(Bandwidth) ->
    cre_PropertyParm("b", BwType ++ ":" ++ Bandwidth).

cre_sdp_b(BWT, BW) ->
    #megaco_sdp_b{bwtype = BWT, bandwidth = BW}.


cre_PropertyParm_cat(Cat) when is_list(Cat) ->
    cre_PropertyParm_a("cat", Cat).

cre_sdp_a_cat(C) ->
    #megaco_sdp_a_cat{category = C}.


cre_PropertyParm_keywds(KeyWds) when is_list(KeyWds) ->
    cre_PropertyParm_a("keywds", KeyWds).

cre_sdp_a_keywds(KW) ->
    #megaco_sdp_a_keywds{keywords = KW}.


cre_PropertyParm_tool(NameAndVersion) when is_list(NameAndVersion) ->
    cre_PropertyParm_a("tool", NameAndVersion).

cre_sdp_a_tool(NAV) ->
    #megaco_sdp_a_tool{name_and_version = NAV}.


cre_PropertyParm_ptime(PacketTime) when is_integer(PacketTime) ->
    cre_PropertyParm_ptime(i2s(PacketTime));
cre_PropertyParm_ptime(PacketTime) when is_list(PacketTime) ->
    cre_PropertyParm_a("ptime", PacketTime).

cre_sdp_a_ptime(PT) ->
    #megaco_sdp_a_ptime{packet_time = PT}.


cre_PropertyParm_maxptime(MaxPacketTime) when is_integer(MaxPacketTime) ->
    cre_PropertyParm_maxptime(i2s(MaxPacketTime));
cre_PropertyParm_maxptime(MaxPacketTime) when is_list(MaxPacketTime) ->
    cre_PropertyParm_a("maxptime", MaxPacketTime).

cre_sdp_a_maxptime(PT) ->
    #megaco_sdp_a_maxptime{maximum_packet_time = PT}.


cre_PropertyParm_rtpmap(Payload, EncName, ClockRate) ->
    cre_PropertyParm_rtpmap(Payload, EncName, ClockRate, []).

cre_PropertyParm_rtpmap(Payload, EncName, ClockRate, EncPar)
  when is_integer(Payload) and
       is_list(EncName) and
       is_integer(ClockRate) and
       is_list(EncPar) ->
    F = fun(Elem, Str) -> Str ++ "/" ++ Elem end, 
    Val = 
	integer_to_list(Payload) ++ " " ++
	EncName ++ "/" ++ integer_to_list(ClockRate) ++ 
	lists:foldl(F, [], EncPar),
    cre_PropertyParm_a("rtpmap", Val).

cre_sdp_a_rtpmap(Payload, EncName, ClockRate) ->
    #megaco_sdp_a_rtpmap{payload_type  = Payload, 
			 encoding_name = EncName,
			 clock_rate    = ClockRate}.
cre_sdp_a_rtpmap(Payload, EncName, ClockRate, EncParms) ->
    #megaco_sdp_a_rtpmap{payload_type   = Payload, 
			 encoding_name  = EncName,
			 clock_rate     = ClockRate,
			 encoding_parms = EncParms}.

cre_PropertyParm_orient(Orientation) when is_atom(Orientation) ->
    cre_PropertyParm_orient(atom_to_list(Orientation));
cre_PropertyParm_orient(Orientation) when is_list(Orientation) ->
    cre_PropertyParm_a("orient", Orientation).

cre_sdp_a_orient(O) ->
    #megaco_sdp_a_orient{orientation = O}.

cre_PropertyParm_type(CT) when is_list(CT) ->
    cre_PropertyParm_a("type", CT).

cre_sdp_a_type(CT) ->
    #megaco_sdp_a_type{conf_type = CT}.

cre_PropertyParm_charset(CS) when is_list(CS) ->
    cre_PropertyParm_a("charset", CS).

cre_sdp_a_charset(CS) ->
    #megaco_sdp_a_charset{char_set = CS}.

cre_PropertyParm_sdplang(L) when is_list(L) ->
    cre_PropertyParm_a("sdplang", L).

cre_sdp_a_sdplang(L) ->
    #megaco_sdp_a_sdplang{tag = L}.

cre_PropertyParm_lang(L) when is_list(L) ->
    cre_PropertyParm_a("lang", L).

cre_sdp_a_lang(L) ->
    #megaco_sdp_a_lang{tag = L}.

cre_PropertyParm_framerate(FR) when is_list(FR) ->
    cre_PropertyParm_a("framerate", FR).

cre_sdp_a_framerate(FR) ->
    #megaco_sdp_a_framerate{frame_rate = FR}.

cre_PropertyParm_quality(Quality) when is_integer(Quality) ->
    cre_PropertyParm_quality(i2s(Quality));
cre_PropertyParm_quality(Quality) when is_list(Quality) ->
    cre_PropertyParm_a("quality", Quality).

cre_sdp_a_quality(Qa) ->
    #megaco_sdp_a_quality{quality = Qa}.

cre_PropertyParm_a(Attr, AttrValue) 
  when is_list(Attr) and is_list(AttrValue) ->
    cre_PropertyParm("a", Attr ++ ":" ++ AttrValue).

cre_PropertyParm_a(Attr) when is_list(Attr) ->
    cre_PropertyParm("a", Attr).

cre_PropertyParm_a_fmtp(Format, Param) 
  when is_list(Format) and is_list(Param) ->
    cre_PropertyParm_a("fmtp", Format ++ " " ++ Param).

cre_sdp_a_fmtp(Fmt, Parm) ->
    #megaco_sdp_a_fmtp{format = Fmt, param = Parm}.

cre_sdp_a(Attr) ->
    #megaco_sdp_a{attribute = Attr}.

%% cre_sdp_a(Attr, Val) ->
%%     #megaco_sdp_a{attribute = Attr,
%% 		  value     = Val}.

cre_PropertyParm_o(User, SID, Version, AddrType, Addr) ->
    cre_PropertyParm_o(User, SID, Version, in, AddrType, Addr).

cre_PropertyParm_o(User, SID, Version, NetType, AddrType, Addr) 
  when is_list(User) and
       is_integer(SID) and
       is_integer(Version) and
       is_list(NetType) or (NetType == in) and
       is_list(AddrType) or ((AddrType == ip4) or (AddrType == ip6)) and
       is_list(Addr) ->
    NT = case NetType of
	     in -> "IN";
	     _  -> NetType
	 end,
    AT = case AddrType of
	     ip4 -> "IP4";
	     ip6 -> "IP6";
	     _   -> AddrType
	 end,
    Val = 
	User         ++ " " ++
	i2s(SID)     ++ " " ++
	i2s(Version) ++ " " ++
	NT           ++ " " ++
	AT           ++ " " ++
	Addr, 
    cre_PropertyParm("o", Val).

cre_sdp_o(Name, SID, V, AddrType, Addr) ->
    cre_sdp_o(Name, SID, V, in, AddrType, Addr).
cre_sdp_o(Name, SID, V, NetType, AddrType, Addr) ->
    #megaco_sdp_o{user_name    = Name,
		  session_id   = SID,
		  version      = V,
		  network_type = NetType,
		  address_type = AddrType, 
		  address      = Addr}.

cre_PropertyParm_m(Media, Port, Transport, FmtList) 
  when is_atom(Media) ->
    cre_PropertyParm_m(atom_to_list(Media), 
		       Port, Transport, FmtList);
cre_PropertyParm_m(Media, Port0, Transport, FmtList) 
  when is_list(Media) and is_list(Transport) and is_list(FmtList) ->
    Port = i2s(Port0),
    Val = 
	Media ++ " " ++ Port ++ " " ++ Transport ++ " " ++ val(FmtList),
    cre_PropertyParm("m", Val).

cre_PropertyParm_m(Media, Port, NumPorts, Transport, FmtList) 
  when is_atom(Media) ->
    cre_PropertyParm_m(atom_to_list(Media), 
		       Port, NumPorts, Transport, FmtList);
cre_PropertyParm_m(Media, Port0, NumPorts0, Transport, FmtList) 
  when is_list(Media) and is_list(Transport) and is_list(FmtList) ->
    Port     = i2s(Port0),
    NumPorts = i2s(NumPorts0),
    Val = 
	Media ++ " " ++ Port ++ "/" ++ NumPorts ++ " " ++ Transport ++ 
	" " ++ val(FmtList),
    cre_PropertyParm("m", Val).

cre_sdp_m(Media, Port, Transport, FmtList) ->
    #megaco_sdp_m{media     = Media,
		  port      = Port,
		  transport = Transport,
		  fmt_list  = FmtList}.

cre_sdp_m(Media, Port, NumPorts, Transport, FmtList) ->
    #megaco_sdp_m{media     = Media,
		  port      = Port,
		  num_ports = NumPorts, 
		  transport = Transport,
		  fmt_list  = FmtList}.


cre_PropertyParm_c(ip4, ConnAddr) ->
    cre_PropertyParm_c("IP4", ConnAddr);
cre_PropertyParm_c(ip6, ConnAddr) ->
    cre_PropertyParm_c("IP6", ConnAddr);
cre_PropertyParm_c(AddrType, ConnAddr) 
  when is_list(AddrType) and is_list(ConnAddr) ->
    Val = "IN " ++ AddrType ++ " " ++ ConnAddr, 
    cre_PropertyParm("c", Val).

cre_PropertyParm_c(ip4, Base, TTL) ->
    cre_PropertyParm_c("IP4", Base, i2s(TTL));
cre_PropertyParm_c(AddrType, Base, TTL) 
  when is_list(AddrType) and 
       is_list(Base)     and 
       is_list(TTL) ->
    Val = "IN " ++ AddrType ++ " " ++ Base ++ "/" ++ TTL, 
    cre_PropertyParm("c", Val).

cre_PropertyParm_c(ip4, Base, TTL, NumOf) ->
    cre_PropertyParm_c("IP4", Base, i2s(TTL), i2s(NumOf));
cre_PropertyParm_c(AddrType, Base, TTL, NumOf) 
  when is_list(AddrType) and 
       is_list(Base)     and 
       is_list(TTL)      and 
       is_list(NumOf) ->
    Val = 
	"IN " ++ AddrType ++ " " ++ Base ++ "/" ++ TTL ++ "/" ++ NumOf,
    cre_PropertyParm("c", Val).


cre_sdp_c(AddrType, ConnAddr) ->
    cre_sdp_c(in, AddrType, ConnAddr).

cre_sdp_c(NetType, AddrType, ConnAddr) ->
    #megaco_sdp_c{network_type    = NetType,
		  address_type    = AddrType, 
		  connection_addr = ConnAddr}.


cre_PropertyParm_v(Version) when is_integer(Version) ->
    cre_PropertyParm_v(integer_to_list(Version));
cre_PropertyParm_v(Version) when is_list(Version) ->
    cre_PropertyParm("v", Version).

cre_sdp_v(Version) ->
    #megaco_sdp_v{version = Version}.
	

cre_PropertyParm(Name, Val) when is_list(Name) and is_list(Val) ->
    #'PropertyParm'{name = Name, value = [Val]}.


val(Vals) ->
    val(Vals, " ").
val([Head|Tail], Sep) ->
    lists:foldl(fun(E, S) -> S ++ Sep ++ E end, Head, Tail).

i2s(I) when is_integer(I) ->
    integer_to_list(I);
i2s(S) when is_list(S) ->
    S.


%% error(Reason) ->
%%     throw({error, Reason}).
