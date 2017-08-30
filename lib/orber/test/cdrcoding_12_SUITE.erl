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
%%
%% Description:
%% Test suite for the CDR encode/decode functions
%%
%%-----------------------------------------------------------------

-module(cdrcoding_12_SUITE).

-include("idl_output/Module.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").

-define(default_timeout, test_server:minutes(5)).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-compile(export_all).

%%-----------------------------------------------------------------
%% Func: all/1
%% Args: 
%% Returns: 
%%-----------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    cases().

groups() -> 
    [{types, [],
      [do_register, null_type, void_type, principal_type,
       objref_type, struct_type, union_type, string_type,
       array_type, any_type, typecode_type, alias_type,
       exception_type, do_unregister]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


cases() -> 
    [{group, types}, reply, cancel_request,
     close_connection, message_error].
%% request, locate_request, locate_reply].

%%-----------------------------------------------------------------
%% Init and cleanup functions.
%%-----------------------------------------------------------------

init_per_testcase(_Case, Config) ->
    Path = code:which(?MODULE),
    code:add_pathz(filename:join(filename:dirname(Path), "idl_output")),
    Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    Path = code:which(?MODULE),
    code:del_path(filename:join(filename:dirname(Path), "idl_output")),
    Dog = proplists:get_value(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

init_per_suite(Config) when is_list(Config) ->
    orber:jump_start(0),
    if
	is_list(Config) ->
	    Config;
	true ->
	    exit("Config not a list")
    end.

end_per_suite(Config) when is_list(Config) ->
    orber:jump_stop(),
    Config.

%%-----------------------------------------------------------------
%% Test Case: type encoding tests
%% Description: Just testing the complex types, the others are 
%%              tested in the cdrlib SUITE.
%%-----------------------------------------------------------------

do_register(Config) when is_list(Config) ->
    'oe_orber_test':'oe_register'(),
    ok.

do_unregister(Config) when is_list(Config) ->
    'oe_orber_test':'oe_unregister'(),
    ok.
%%-----------------------------------------------------------------
%% Encode/decode test of type: null 
%%-----------------------------------------------------------------
null_type(Config) when is_list(Config) ->
    B = cdr_encode:enc_type(#giop_env{version = {1, 2}}, 'tk_null', 'null'),
    {'null', <<>>, _} = cdr_decode:dec_type('tk_null', {1, 2}, B, 0, big),
    ok.

%%-----------------------------------------------------------------
%% Encode/decode test of type: void 
%%-----------------------------------------------------------------
void_type(Config) when is_list(Config) ->
    B = cdr_encode:enc_type(#giop_env{version = {1, 2}}, 'tk_void', 'ok'),
    {'ok', <<>>, _} = cdr_decode:dec_type('tk_void', {1, 2}, B, 0, big),
    ok.

%%-----------------------------------------------------------------
%% Encode/decode test of type: principal 
%%-----------------------------------------------------------------
principal_type(Config) when is_list(Config) ->
    B0 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, 'tk_Principal', "principal"),
    {"principal", <<>>, _} = cdr_decode:dec_type('tk_Principal', {1, 2}, B0, 0, big),
    B1 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, 'tk_Principal', ""),
    {"", <<>>, _} = cdr_decode:dec_type('tk_Principal', {1, 2}, B1, 0, big),
    B2 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, 'tk_Principal', "principal"),
    {"principal", <<>>, _} =
	cdr_decode:dec_type('tk_Principal', {1, 2}, B2, 0, big),
    ok.

%%-----------------------------------------------------------------
%% Encode/decode test of type: object reference 
%%-----------------------------------------------------------------
version() -> #'IIOP_Version'{major=1,minor=2}.

objref(0) -> 
    PB = #'IIOP_ProfileBody_1_1'{iiop_version=version(),
				 host="my.hostname.org",
				 port=4040,
				 object_key="ExternalKey: which is an arbitary octet sequence",
				 components=[]},
    TP = #'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB},
    #'IOP_IOR'{type_id="IDL:Module/Interface:1.0", profiles=[TP]};
objref(1) -> 
    K = corba_fake_mk_objkey("IDL:Module/Interface:1.0", key,
			     list_to_pid("<0.100.0>")),
    PB = #'IIOP_ProfileBody_1_1'{iiop_version=version(),
				 host="my.hostname.org",
				 port=4040,
				 object_key=K, components=[]},
    TP = #'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB},
    #'IOP_IOR'{type_id="IDL:Module/Interface:1.0", profiles=[TP]};
objref(2) -> 
    K = corba_fake_mk_objkey("IDL:Module/Interface:1.0", registered,
			     list_to_atom("orber_nameservice")),
    PB = #'IIOP_ProfileBody_1_1'{iiop_version=version(),
				 host="my.hostname.org",
				 port=4040,
				 object_key=K, components=[]},
    TP = #'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB},
    #'IOP_IOR'{type_id="IDL:Module/Interface:1.0", profiles=[TP]}.

objref_type(Config) when is_list(Config) ->
    T = {'tk_objref', "IDL:Module/Interface:1.0", "Interface"},
    Objref0 = objref(0),
    B0 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T, Objref0),
    {Objref0, <<>>, _} = cdr_decode:dec_type(T, {1, 2}, B0, 0, big),
    Objref1 = objref(1),
    B1 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T, Objref1),
    {Objref1, <<>>, _} = cdr_decode:dec_type(T, {1, 2}, B1, 0, big),
    Objref2 = objref(2),
    B2 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T, Objref2),
    {Objref2, <<>>, _} = cdr_decode:dec_type(T, {1, 2}, B2, 0, big),
    ok.



%%-----------------------------------------------------------------
%% Encode/decode test of type: struct 
%%-----------------------------------------------------------------
struct_type(Config) when is_list(Config) ->
    T0 = {'tk_struct',"IDL:Module/Struct0:1.0", "Module_Struct0",
	  [{"long", 'tk_long'}, {"short", 'tk_short'}, {"character", 'tk_char'}]},
    S0 = #'Module_Struct0'{l=-4711, s=17, c=$a},
    B0 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T0, S0),
    {S0, <<>>, _} = cdr_decode:dec_type(T0, {1, 2}, B0, 0, big),
    
    T1 = {'tk_struct', "IDL:Module/Struct1:1.0", "Module_Struct1",
	  [{"string", {'tk_string', 0}}, {"ushort", 'tk_ushort'}, {"ulong", 'tk_ulong'}]},
    S1 = #'Module_Struct1'{s="Hi !!!!", us=17, ul=4711},
    B1 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T1, S1),
    {S1, <<>>, _} = cdr_decode:dec_type(T1, {1, 2}, B1, 0, big),
    
    T2 = {'tk_struct', "IDL:Module/Struct2:1.0", "Module_Struct2",
	  [{"long_sequence", {'tk_sequence', 'tk_long', 0}},
	   {"enum", {'tk_enum', "IDL:Module/Enum:1.0", "Module_Enum", ["horse", "pig", "cow"]}},
	   {"octet", 'tk_octet'}]},
    S2 = #'Module_Struct2'{long_sequence=[4711, 350000, 0, -3030, -600000], e=cow, o=$X},
    B2 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T2, S2),
    {S2, <<>>, _} = cdr_decode:dec_type(T2, {1, 2}, B2, 0, big),
    ok.

%%-----------------------------------------------------------------
%% Encode/decode test of type: union 
%%-----------------------------------------------------------------
union_type(Config) when is_list(Config) ->
    T0 = {'tk_union', "IDL:Module/Union:1.0", "Union", 'tk_short', 2,
	  [{0, "First", 'tk_short'},
	   {1, "Second", {'tk_string', 0}},
	   {2, "Third", 'tk_char'}]},
    S0 = #'Module_Union'{label=1, value="Foo Bar !"},
    B0 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T0, S0),
    {S0, <<>>, _} = cdr_decode:dec_type(T0, {1, 2}, B0, 0, big),
    S1 = #'Module_Union'{label=0, value=-17},
    B1 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T0, S1),
    {S1, <<>>, _} = cdr_decode:dec_type(T0, {1, 2}, B1, 0, big),
    S2 = #'Module_Union'{label=2, value=$X},
    B2 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T0, S2),
    {S2, <<>>, _} = cdr_decode:dec_type(T0, {1, 2}, B2, 0, big),
    T1 = {'tk_union', "IDL:Module/Union1:1.0", "Union1",
	  {'tk_enum', "IDL:Module/Enum:1.0",
	   "Module_Enum", ["horse", "pig", "cow"]}, "pig",
	  [{"horse", "First", 'tk_ushort'},
	   {"pig", "Second", {'tk_sequence', {'tk_string', 0}, 0}},
	   {"cow", "Third", {'tk_enum', "IDL:Module/Enum1:1.0",
			     "Module_Enum1", ["orange", "banana", "apple"]}}]},
    S3 = #'Module_Union1'{label=pig, value=["Foo", "Bar", "!"]},
    B3 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T1, S3),
    {S3, <<>>, _} = cdr_decode:dec_type(T1, {1, 2}, B3, 0, big),
    S4 = #'Module_Union1'{label=cow, value=apple},
    B4 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T1, S4),
    {S4, <<>>, _} = cdr_decode:dec_type(T1, {1, 2}, B4, 0, big),
    S5 = #'Module_Union1'{label=horse, value=17},
    B5 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T1, S5),
    {S5, <<>>, _} = cdr_decode:dec_type(T1, {1, 2}, B5, 0, big),
    T2 = {'tk_union', "IDL:Module/Union2:1.0", "Union2",
	  {'tk_enum', "IDL:Module/Enum:1.0",
	   "Module_Enum", ["horse", "pig", "cow"]}, "pig",
	  [{"horse", "First", {'tk_array', 'tk_long', 3}},
	   {"pig", "Second",
	    {'tk_union', "IDL:Module/Union:1.0", "Union", 'tk_short', 2,
	     [{0, "First", 'tk_short'},
	      {1, "Second", {'tk_string', 0}},
	      {2, "Third", 'tk_char'}]}},
	   {"cow", "Third", {'tk_union', "IDL:Module/Union1:1.0", "Union1",
			     {'tk_enum', "IDL:Module/Enum:1.0",
			      "Module_Enum", ["horse", "pig", "cow"]}, "pig",
			     [{"horse", "First", 'tk_ushort'},
			      {"pig", "Second", {'tk_sequence', 
						 {'tk_string', 0}, 0}},
			      {"cow", "Third", {'tk_enum', 
						"IDL:Module/Enum1:1.0",
						"Module_Enum1", 
						["orange", "banana", 
						 "apple"]}}]}}]},
    S6 = #'Module_Union2'{label=pig, value=#'Module_Union'{label=0, value=-17}},
    B6 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T2, S6),
    {S6, <<>>, _} = cdr_decode:dec_type(T2, {1, 2}, B6, 0, big),
    S7 = #'Module_Union2'{label=cow, value=#'Module_Union1'{label=pig,
						      value=["Foo", "Bar", "!"]}},
    B7 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T2, S7),
    {S7, <<>>, _} = cdr_decode:dec_type(T2, {1, 2}, B7, 0, big),
    S8 = #'Module_Union2'{label=horse, value={-17, 1234567890, -987654321}},
    B8 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T2, S8),
    {S8, <<>>, _} = cdr_decode:dec_type(T2, {1, 2}, B8, 0, big),
    ok.

%%-----------------------------------------------------------------
%% Encode/decode test of type: string 
%%-----------------------------------------------------------------
string_type(Config) when is_list(Config) ->
    S0 = "Foo Bar ???",
    B0 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, {'tk_string', 0}, S0),
    {S0, <<>>, _} = cdr_decode:dec_type({'tk_string', 0}, {1, 2}, B0, 0, big),
    S1 = "Yes, Foo Bar !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! more than 5000 characters",
    B1 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, {'tk_string', 0}, S1),
    {S1, <<>>, _} = cdr_decode:dec_type({'tk_string', 0}, {1, 2}, B1, 0, big),
    S2 = "",
    B2 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, {'tk_string', 0}, S2),
    {S2, <<>>, _} = cdr_decode:dec_type({'tk_string', 0}, {1, 2}, B2, 0, big),
    S3 = "\0",
    B3 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, {'tk_string', 0}, S3),
    {S3, <<>>, _} = cdr_decode:dec_type({'tk_string', 0}, {1, 2}, B3, 0, big),
    S4 = "~n",
    B4 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, {'tk_string', 0}, S4),
    {S4, <<>>, _} = cdr_decode:dec_type({'tk_string', 0}, {1, 2}, B4, 0, big),
    ok.

%%-----------------------------------------------------------------
%% Encode/decode test of type: array 
%%-----------------------------------------------------------------
array_type(Config) when is_list(Config) ->
    T0 = {'tk_array', 'tk_long', 5},
    S0 = {-100, 0, 30000, -900100900, 123456789},
    B0 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T0, S0),
    {S0, <<>>, _} = cdr_decode:dec_type(T0, {1, 2}, B0, 0, big),
    T1 = {'tk_array', {'tk_enum', "IDL:Module/Enum:1.0", "Module_Enum", ["horse", "pig", "cow"]}, 2},
    S1 = {pig, cow},
    B1 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T1, S1),
    {S1, <<>>, _} = cdr_decode:dec_type(T1, {1, 2}, B1, 0, big),
    T2 = {'tk_array', {'tk_union', "IDL:Module/Union:1.0", "Union",
		       {'tk_enum', "IDL:Module/Enum:1.0", "Module_Enum", ["horse", "pig", "cow"]}, "pig",
		       [{"horse", "First", 'tk_ushort'},
			{"pig", "Second", {'tk_sequence', {'tk_string', 0}, 0}},
			{"cow", "Third", {'tk_enum', "IDL:Module/Enum1:1.0",
					"Module_Enum1", ["orange", "banana", "apple"]}}]}, 2},
    S2 = {#'Module_Union'{label=cow, value=banana}, #'Module_Union'{label=pig, value=["This", "is", "a", "test", ""]}},
    B2 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T2, S2),
    {S2, <<>>, _} = cdr_decode:dec_type(T2, {1, 2}, B2, 0, big),
    T3 = {'tk_array', {'tk_objref', "IDL:Module/Interface:1.0", "Interface"}, 3},
    S3 = {objref(0), objref(1), objref(2)},
    B3 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T3, S3),
    {S3, <<>>, _} = cdr_decode:dec_type(T3, {1, 2}, B3, 0, big),    
    ok.
%%-----------------------------------------------------------------
%% Encode/decode test of type: TypeCode 
%%-----------------------------------------------------------------
any_type(Config) when is_list(Config) ->
    T = 'tk_any',
    TC = {'tk_struct', "IDL:Module/Struct2:1.0", "Module_Struct2",
	  [{"long_sequence", {'tk_sequence', 'tk_long', 0}},
	   {"enum", {'tk_enum', "IDL:Module/Enum:1.0", "Module_Enum",
		     ["horse", "pig", "cow"]}},
	   {"octet", 'tk_octet'}]},
    S = #'Module_Struct2'{long_sequence=[4711, 350000, 0, -3030, -600000], 
			  e=cow, o=$X},
    Any = #any{typecode=TC,value=S},
    B = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T,Any),
    {Any, <<>>, _} = cdr_decode:dec_type(T, {1, 2}, B, 0, big),
    TC1 = {'tk_array', {'tk_union', "IDL:Module/Union:1.0", "Union",
		      {'tk_enum', "IDL:Module/Enum:1.0", "Module_Enum",
		       ["horse", "pig", "cow"]}, 1,
		      [{"horse", "First", 'tk_ushort'},
		       {"pig", "Second", {'tk_sequence', {'tk_string', 0}, 0}},
		       {"cow", "Third", {'tk_enum', "IDL:Module/Enum1:1.0",
					 "Module_Enum1", ["orange", "banana",
							  "apple"]}}]},2},
    S1 = {#'Module_Union'{label=cow, value=banana}, #'Module_Union'{label=pig, value=["This", "is", "a", "test", ""]}},
    Any1 = #any{typecode=TC1,value=S1}, 
    B1 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T,Any1),
    {Any1, <<>>, _} = cdr_decode:dec_type(T, {1, 2}, B1, 0, big),
    ok.


%%-----------------------------------------------------------------
%% Encode/decode test of type: TypeCode 
%%-----------------------------------------------------------------
typecode_type(Config) when is_list(Config) ->
    T = 'tk_TypeCode',
    TC = {'tk_array', {'tk_union', "IDL:Module/Union:1.0", "Union",
		      {'tk_enum', "IDL:Module/Enum:1.0", "Module_Enum",
		       ["horse", "pig", "cow"]}, 1,
		      [{"horse", "First", 'tk_ushort'},
		       {"pig", "Second", {'tk_sequence', {'tk_string', 0}, 0}},
	       {"cow", "Third", {'tk_enum', "IDL:Module/Enum1:1.0",
					 "Module_Enum1", ["orange", "banana",
							  "apple"]}}]}, 10},
    B = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T,TC),
    {TC, <<>>, _} = cdr_decode:dec_type(T, {1, 2}, B, 0, big),
    TC1 = {'tk_union', "IDL:Module/Union2:1.0", "Union2",
	  {'tk_enum', "IDL:Module/Enum:1.0",
	   "Module_Enum", ["horse", "pig", "cow"]}, 2,
	  [{"horse", "First", 'tk_long'},
	   {"pig", "Second",
	    {'tk_union', "IDL:Module/Union:1.0", "Union", 'tk_short', 2,
	     [{0, "First", 'tk_short'},
	      {1, "Second", {'tk_string', 0}},
	      {2, "Third", 'tk_char'}]}},
	   {"cow", "Third", {'tk_union', "IDL:Module/Union1:1.0", "Union1",
			     {'tk_enum', "IDL:Module/Enum:1.0",
			      "Module_Enum", ["horse", "pig", "cow"]}, 2,
			     [{"horse", "First", 'tk_ushort'},
			      {"pig", "Second", {'tk_sequence', 
						 {'tk_string', 0}, 0}},
			      {"cow", "Third", {'tk_enum', 
						"IDL:Module/Enum1:1.0",
						"Module_Enum1", 
						["orange", "banana", 
						 "apple"]}}]}}]},
    B1 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T, TC1),
    {TC1, <<>>, _} = cdr_decode:dec_type(T, {1, 2}, B1, 0, big),
    ok.

%%-----------------------------------------------------------------
%% Encode/decode test of type: TypeCode 
%%-----------------------------------------------------------------
alias_type(Config) when is_list(Config) ->
    T = {'tk_alias', "IDL:Module/Alias:1.0", "Alias",
	 {'tk_struct', "IDL:Module/Struct2:1.0", "Module_Struct2",
	  [{"long_sequence", {'tk_sequence', 'tk_long', 0}},
	   {"enum", {'tk_enum', "IDL:Module/Enum:1.0", "Module_Enum",
		     ["horse", "pig", "cow"]}},
	   {"octet", 'tk_octet'}]}},
    S = #'Module_Struct2'{long_sequence=[4711, 350000, 0, -3030, -600000], 
			  e=cow, o=$X},
    B = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T,S),
    {S, <<>>, _} = cdr_decode:dec_type(T, {1, 2}, B, 0, big),
    T1 = {'tk_alias', "IDL:Module/Alias1:1.0", "Alias1",
	 {'tk_sequence', {'tk_union', "IDL:Module/Union:1.0", "Union",
		      {'tk_enum', "IDL:Module/Enum:1.0", "Module_Enum",
		       ["horse", "pig", "cow"]}, 2,
		      [{"horse", "First", 'tk_ushort'},
		       {"pig", "Second", {'tk_sequence', {'tk_string', 0}, 0}},
		       {"cow", "Third", {'tk_enum', "IDL:Module/Enum1:1.0",
					 "Module_Enum1", ["orange", "banana",
							  "apple"]}}]},0}},
    S1 = [#'Module_Union'{label=cow, value=banana}, #'Module_Union'{label=pig, value=["This", "is", "a", "test", ""]}],
    B1 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T1, S1),
    {S1, <<>>, _} = cdr_decode:dec_type(T1, {1, 2}, B1, 0, big),
    ok.

%%-----------------------------------------------------------------
%% Encode/decode test of type: exception 
%%-----------------------------------------------------------------
exception_type(Config) when is_list(Config) ->
    system_exceptions(),
    user_exceptions(),
    ok.

system_exceptions() ->
    E = #'UNKNOWN'{completion_status=?COMPLETED_YES},
    {system_exception, T, E} = orber_exceptions:get_def(E),
    B = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T,E),
    {E, _} = cdr_decode:dec_system_exception({1, 2}, B, 0, big),
    E1 = #'INV_OBJREF'{completion_status=?COMPLETED_NO},
    {system_exception, T1, E1} = orber_exceptions:get_def(E1),
    B1 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T1,E1),
    {E1, _} = cdr_decode:dec_system_exception({1, 2}, B1, 0, big),
    E2 = #'BAD_OPERATION'{completion_status=?COMPLETED_NO},
    {system_exception, T2, E2} = orber_exceptions:get_def(E2),
    B2 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T2,E2),
    {E2, _} = cdr_decode:dec_system_exception({1, 2}, B2, 0, big),
    E3 = #'INTF_REPOS'{completion_status=?COMPLETED_MAYBE},
    {system_exception, T3, E3} = orber_exceptions:get_def(E3),
    B3 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T3,E3),
    {E3, _} = cdr_decode:dec_system_exception({1, 2}, B3, 0, big),
    ok.

user_exceptions() ->
    E = #'Module_Except1'{rest_of_name=["I","am","testing","exceptions"], why="Error"},
    {user_exception, T, E} = orber_exceptions:get_def(E),
    B = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T, E),
    {E, _} = cdr_decode:dec_user_exception({1, 2}, B, 0, big),
    E1 = #'Module_Except2'{e=banana,
			   s=#'Module_Struct2'{long_sequence=[12,-4040,
							1234567898],
					 e=horse,
					 o=$a}},
    {user_exception, T1, E1} = orber_exceptions:get_def(E1),
    B1 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T1, E1),
    {E1, _} = cdr_decode:dec_user_exception({1, 2}, B1, 0, big),
    E2 = #'Module_Except3'{u=#'Module_Union1'{label=pig,value=["high","and","low"]},s=1313, o=objref(0)},
    {user_exception, T2, E2} = orber_exceptions:get_def(E2),
    B2 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T2, E2),
    {E2, _} = cdr_decode:dec_user_exception({1, 2}, B2, 0, big),
    E3 = #'Module_Except4'{},
    {user_exception, T3, E3} = orber_exceptions:get_def(E3),
    B3 = cdr_encode:enc_type(#giop_env{version = {1, 2}}, T3, E3),
    {E3, _} = cdr_decode:dec_user_exception({1, 2}, B3, 0, big),
    ok.

%%-----------------------------------------------------------------
%% Test Case: request encoding test
%% Description: Precondition the stack must be started so the
%%              objectkey is valid.
%%-----------------------------------------------------------------
%request(_) ->
%    exit(not_implemented).

%%-----------------------------------------------------------------
%% Test Case: reply encoding test
%% Description: 
%%-----------------------------------------------------------------
reply(Config) when is_list(Config) ->
    R = #reply_header{service_context=[], request_id=1,
		      reply_status='no_exception'},
    B = cdr_encode:enc_reply(#giop_env{version = {1, 2}, request_id = 1, 
					     reply_status = 'no_exception',
					     tc = {'tk_long', [], [{'tk_sequence',
								    {'tk_string', 0}, 0}]},
					     result = 1200, 
					     parameters = [["foo","Bar"]], 
					     ctx = []}),
   
    {R, 1200, [["foo","Bar"]]}  =
	cdr_decode:dec_message({'tk_long', [], [{'tk_sequence', {'tk_string', 0},0}]},
			       B),
    
    ok.

%%-----------------------------------------------------------------
%% Test Case: cancel_request encoding test
%% Description: 
%%-----------------------------------------------------------------
cancel_request(Config) when is_list(Config) ->
    R = #cancel_request_header{request_id=1},
    B = cdr_encode:enc_cancel_request(#giop_env{version = {1, 2}, 
						      request_id = 1}),
    R = cdr_decode:dec_message([], B),
    ok.

%%-----------------------------------------------------------------
%% Test Case: locate_request encoding test
%% Description: 
%%-----------------------------------------------------------------
locate_request(Config) when is_list(Config) ->
    io:format("Function not imlpemented yet"),
    exit(not_implemented).

%%-----------------------------------------------------------------
%% Test Case: locate_reply encoding test
%% Description: 
%%-----------------------------------------------------------------
locate_reply(Config) when is_list(Config) ->
    io:format("Function not imlpemented yet"),
    exit(not_implemented).

%%-----------------------------------------------------------------
%% Test Case: close_connection encoding test
%% Description: 
%%-----------------------------------------------------------------
close_connection(Config) when is_list(Config) ->
    B = cdr_encode:enc_close_connection(#giop_env{version = {1, 2}}),
    'close_connection' = cdr_decode:dec_message([], B),
    ok.

%%-----------------------------------------------------------------
%% Test Case: message_error encoding test
%% Description: 
%%-----------------------------------------------------------------
message_error(Config) when is_list(Config) ->
    B = cdr_encode:enc_message_error(#giop_env{version = {1, 2}}),
    'message_error' = cdr_decode:dec_message([], B),
    ok.



%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
corba_fake_mk_objkey(Id, 'key', Pid) when is_pid(Pid) ->
    Key = make_objkey(),
    {list_to_binary(Id), 'key', Key, term_to_binary(undefined),
     term_to_binary(undefined), term_to_binary(undefined)};
corba_fake_mk_objkey(Id, 'key', RegName) when is_atom(RegName) ->
    Key = term_to_binary(RegName),
    {list_to_binary(Id), 'key', Key, term_to_binary(undefined),
     term_to_binary(undefined), term_to_binary(undefined)};
corba_fake_mk_objkey(Id, 'registered', RegName) when is_atom(RegName) ->
    {list_to_binary(Id), 'registered', RegName, term_to_binary(undefined),
     term_to_binary(undefined), term_to_binary(undefined)}.

make_objkey() ->
    term_to_binary({{erlang:system_time(), 
		     erlang:unique_integer()}, 
		    node()}).
