%%----------------------------------------------------------------------
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File    : iop_ior_12_SUITE.erl
%% Description : Test suite for the IOR functions
%%
%%----------------------------------------------------------------------
-module(iop_ior_12_SUITE).


-include_lib("common_test/include/ct.hrl").
-include_lib("orber/src/orber_iiop.hrl").

-define(default_timeout, test_server:minutes(3)).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).
-compile(export_all).

%%-----------------------------------------------------------------
%% Func: all/1
%% Args: 
%% Returns: 
%%-----------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [encoding, create_and_get_ops].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%-----------------------------------------------------------------
%% Init and cleanup functions.
%%-----------------------------------------------------------------

init_per_testcase(_Case, Config) ->
    Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    Dog = proplists:get_value(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%-----------------------------------------------------------------
%% Test Case: IOR encoding test
%% Description: Just testing the string_encoding function because the
%%              other encodings is called from them.
%%-----------------------------------------------------------------
encoding(_) ->
    V = #'IIOP_Version'{major=1,minor=2},
    M0 = 'Module_Interface',
    T0 = "IDL:Module/Interface:1.0",
    H0 = "my.hostname.org",
    P0 = 4040,
    N0 = 'name',
    Components = case orber:iiop_ssl_port() of
		     -1 ->
			 [];
		     SSLPort ->
			 [#'IOP_TaggedComponent'{tag=?TAG_SSL_SEC_TRANS, 
						 component_data=[0 |
								 cdrlib:enc_unsigned_short(2, 
											   cdrlib:enc_unsigned_short(2,
								 cdrlib:enc_unsigned_short(SSLPort, [])))]}]
		 end,
    O0 = corba_fake_mk_objkey(M0, registered, N0),
    PB0 = #'IIOP_ProfileBody_1_1'{iiop_version=V, host=H0, port=P0, object_key=O0,
				  components=Components},
    TP0 = #'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB0},
    S0 = #'IOP_IOR'{type_id=T0, profiles=[TP0]},
    N1 = list_to_pid("<0.100.0>"),
    O1 = corba_fake_mk_objkey(M0, key, N1),
    PB1 = #'IIOP_ProfileBody_1_1'{iiop_version=V, host=H0, port=P0, object_key=O1,
				  components=[]},
    TP1 = #'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB1},
    S1 = #'IOP_IOR'{type_id=T0, profiles=[TP1]},
    O2 = "This is an external objectkey",
    PB2 = #'IIOP_ProfileBody_1_1'{iiop_version=V, host=H0, port=P0, object_key=O2,
				  components=[]},
    TP2 = #'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB2},
    S2 = #'IOP_IOR'{type_id=T0, profiles=[TP2]},
    C0 = iop_ior:string_code(S0),
    {S0, <<>>, _} = iop_ior:string_decode(C0),
    C1 = iop_ior:string_code(S1),
    {S1, <<>>, _} = iop_ior:string_decode(C1),
    C2 = iop_ior:string_code(S2),
    {S2, <<>>, _} = iop_ior:string_decode(C2),
    ok.


%%-----------------------------------------------------------------
%% Test Case: IOR creation test
%% Description: 
%%-----------------------------------------------------------------
create_and_get_ops(_) ->
    V = #'IIOP_Version'{major=1,minor=2},
    CSC = #'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
				 component_data=?DEFAULT_CODESETS},
    M0 = 'Module_Interface',
    T0 = "IDL:Module/Interface:1.0",
    H0 = "my.hostname.org",
    P0 = 4040,
    N0 = 'name',
    O0 = corba_fake_mk_objkey(M0, registered, N0),
    PB0 = #'IIOP_ProfileBody_1_1'
      {iiop_version=V, host=H0, port=P0, object_key=O0,
       components=[CSC]},
    TP0 = #'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB0},
    S0 = #'IOP_IOR'{type_id=T0, profiles=[TP0]},
    S0 = iop_ior:create({1, 2}, T0, [H0], P0, -1, O0, [CSC], 0, 0),
    N1 = list_to_pid("<0.100.0>"),
    O1 = corba_fake_mk_objkey(M0, key, N1),
    {_,_,K1,_,_,_} = O1,
    PB1 = #'IIOP_ProfileBody_1_1'
      {iiop_version=V, host=H0, port=P0, object_key=O1,
       components=[CSC]},
    TP1 = #'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB1},
    S1 = #'IOP_IOR'{type_id=T0, profiles=[TP1]},
    S1 = iop_ior:create({1, 2}, T0, [H0], P0, -1, O1, [CSC], 0, 0),
    O2 = "This is an external objectkey",
    PB2 = #'IIOP_ProfileBody_1_1'{iiop_version=V, host=H0, port=P0, object_key=O2,
				  components=[]},
    TP2 = #'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB2},
    S2 = #'IOP_IOR'{type_id=T0, profiles=[TP2]},
    {'internal_registered', N0, _, _, M0} = iop_ior:get_key(S0),
    {'internal', K1, _, _, M0} = iop_ior:get_key(S1),
    {'external', {H0, P0, O2,_,_,
		  #host_data{protocol = normal,
			     ssl_data = undefined,
			     version  = {1,2},
			     csiv2_mech = undefined,
			     csiv2_statefull = false,
			     charset = 65537,
			     wcharset = 65801,
			     ft_heartbeat = false,
			     ft_primary = false,
			     ft_group = undefined,
			     csiv2_addresses = []}}} 
	= iop_ior:get_key(S2),
    T0 = iop_ior:get_typeID(S0),
    O0 = iop_ior:get_objkey(S0),
    O1 = iop_ior:get_objkey(S1),
    O2 = iop_ior:get_objkey(S2),
    ok.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
corba_fake_mk_objkey(Id, 'key', Pid) when is_pid(Pid) ->
    Key = make_objkey(),
    {Id, 'key', Key, term_to_binary(undefined), 0, 0};
corba_fake_mk_objkey(Id, 'key', RegName) when is_atom(RegName) ->
    Key = term_to_binary(RegName),
    {Id, 'key', Key, term_to_binary(undefined), 0, 0};
corba_fake_mk_objkey(Id, 'registered', RegName) when is_atom(RegName) ->
    {Id, 'registered', RegName, term_to_binary(undefined), 0, 0}.

make_objkey() ->
    term_to_binary({{erlang:system_time(), 
		     erlang:unique_integer()}, 
		    node()}).
