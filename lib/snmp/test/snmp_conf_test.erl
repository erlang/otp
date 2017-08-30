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
%% Purpose:
%%----------------------------------------------------------------------
-module(snmp_conf_test).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include("snmp_test_lib.hrl").

-include_lib("snmp/include/STANDARD-MIB.hrl").
-include_lib("snmp/include/OTP-SNMPEA-MIB.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
	all/0,groups/0,init_per_group/2,end_per_group/2, 
         init_per_testcase/2, end_per_testcase/2,

	 check_mandatory/1,
	 check_integer1/1,
	 check_integer2/1,
	 check_string1/1,
	 check_string2/1,
	 check_atom/1,
	 check_ip/1,
	 check_taddress/1,
	 check_packet_size/1,
	 check_oid/1,
	 check_sec_model1/1,
	 check_sec_model2/1,
	 check_sec_level/1,
	 check_timer/1,

	 read/1,
	 read_files/1
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

end_per_testcase(_Case, Config) when is_list(Config) ->
    Config.

%%======================================================================
%% Test case definitions
%%======================================================================
all() -> 
[check_mandatory, check_integer1, check_integer2,
 check_string1, check_string2, check_atom, check_ip,
 check_taddress, check_packet_size, check_oid,
 check_sec_model1, check_sec_model2, check_sec_level,
 check_timer, read, read_files].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.



%%======================================================================
%% Test functions
%%======================================================================

check_mandatory(suite) -> [];
check_mandatory(Config) when is_list(Config) ->
    ?P(check_mandatory),
    %% d("check_mandatory -> entry"),
    A1 = [{a, hej}, {b, hopp}, {c, 10}, {d, 10101}, {f, 10.88}],
    B1 = [{a, {value, hejsan}}, 
	  {b, mandatory}, 
	  {d, {value, 20202}}, 
	  {e, {value, "kalle"}}],
    ?line {ok, _L1} = verify_mandatory(A1, B1),
    ?DBG("check_mandatory -> L1: ~p", [L1]),
    A2 = [{a, hej}, {c, 10}, {d, 10101}, {f, 10.88}],
    B2 = [{a, {value, hejsan}}, 
	  {b, mandatory}, 
	  {d, {value, 20202}}, 
	  {e, {value, "kalle"}}],
    ?line ok = verify_not_mandatory(A2, B2),
    ok.

verify_mandatory(A, B) ->
    case (catch snmp_conf:check_mandatory(A, B)) of
	{'EXIT', Reason} ->
	    ?FAIL({mandatory_fail, A, B, Reason});
	{ok, A} ->
	    ?FAIL({mandatory_not_updated, A, B});
	{ok, L} when A /= L ->
	    verify_mandatory2(B, L)
    end.

verify_mandatory2([], L) ->
    {ok, L};
verify_mandatory2([{Key, _}|T], L) ->
    case lists:keysearch(Key, 1, L) of
	false ->
	    ?FAIL({missing_key, Key, L});
	{value, _} ->
	    verify_mandatory2(T, L)
    end.

verify_not_mandatory(A, B) ->
    case (catch snmp_conf:check_mandatory(A, B)) of
	{error, _Reason} ->
	    ok;
	Else ->
	    ?FAIL({mandatory_not_fail, Else})
    end.


%%======================================================================

check_integer1(suite) -> [];
check_integer1(Config) when is_list(Config) ->
    ?P(check_integer1),
    ?line ok = verify_int(0),
    ?line ok = verify_int(16#FF),
    ?line ok = verify_int(16#FFFF),
    ?line ok = verify_int(16#FFFFFFFF),
    ?line ok = verify_int(-1),
    ?line ok = verify_int(-333),

    ?line ok = verify_not_int("kalle & hobbe"),
    ?line ok = verify_not_int(kalle_och_hobbe),
    ?line ok = verify_not_int(1.5),

    ok.

verify_int(Val) ->
    case (catch snmp_conf:check_integer(Val)) of
	{error, Reason} ->
	    ?FAIL({verify_int, Val, Reason});
	ok ->
	    ok
    end.

verify_not_int(Val) ->
    case (catch snmp_conf:check_integer(Val)) of
	ok ->
	    ?FAIL({verify_int, Val});
	{error, _Reason} ->
	    ok
    end.

%%======================================================================

check_integer2(suite) -> [];
check_integer2(Config) when is_list(Config) ->
    ?P(check_integer2),

    ?line ok = verify_int(0,      any),
    ?line ok = verify_int(-22222, any),
    ?line ok = verify_int(33333,  any),
    ?line ok = verify_int(1,      pos),
    ?line ok = verify_int(9999,   pos),
    ?line ok = verify_int(-1,     neg),
    ?line ok = verify_int(-9999,  neg),
    ?line ok = verify_int(1,      {gt, 0}),
    ?line ok = verify_int(88888,  {gt, -255}),
    ?line ok = verify_int(88888,  {gte, -255}),
    ?line ok = verify_int(88888,  {gte, 88888}),
    ?line ok = verify_int(88888,  {lt,  88889}),
    ?line ok = verify_int(88888,  {lte, 88888}),
    ?line ok = verify_int(88888,  {eq,  88888}),
    ?line ok = verify_int(88888,  {range, 88887,88889}),

    ?line ok = verify_not_int("kalle & hobbe", any),
    ?line ok = verify_not_int(kalle_och_hobbe, any),
    ?line ok = verify_not_int(1.5,             any),

    ?line ok = verify_not_int(0,      pos),
    ?line ok = verify_not_int(-22222, pos),
    ?line ok = verify_not_int(33333,  neg),
    ?line ok = verify_not_int(0,      {gt,  0}),
    ?line ok = verify_not_int(33333,  {gt,  99999}),
    ?line ok = verify_not_int(33333,  {gt,  33333}),
    ?line ok = verify_not_int(33333,  {gte, 33334}),
    ?line ok = verify_not_int(33333,  {lt,  33333}),
    ?line ok = verify_not_int(33333,  {lte, 33332}),
    ?line ok = verify_not_int(33333,  {eq,  33332}),
    ?line ok = verify_not_int(33333,  {eq,  -33333}),
    ?line ok = verify_not_int(33333,  {range, 33334, 33338}),
    ?line ok = verify_not_int(33339,  {range, 33334, 33338}),
    ?line ok = verify_not_int(33333,  {gt,  kalle}),
    ?line ok = verify_not_int(33333,  {gt,  1.55}),
    ?line ok = verify_not_int(33333,  {gte, "hejsan"}),
    ?line ok = verify_not_int(33333,  {lt,  hobbe}),
    ?line ok = verify_not_int(33333,  {lte, 1.7666}),
    ?line ok = verify_not_int(33333,  {eq,  33333.0}),
    ?line ok = verify_not_int(33333,  {eq,  -33333.0}),
    ?line ok = verify_not_int(33333,  {range, kalle, 33338}),
    ?line ok = verify_not_int(33339,  {range, 33334, kalle}),
    ?line ok = verify_not_int(33339,  {kalle, 33334, kalle}),

    ok.

verify_int(Val, Cond) ->
    case (catch snmp_conf:check_integer(Val, Cond)) of
	{error, Reason} ->
	    ?FAIL({verify_int, Val, Cond, Reason});
	ok ->
	    ok
    end.

verify_not_int(Val, Cond) ->
    case (catch snmp_conf:check_integer(Val, Cond)) of
	ok ->
	    ?FAIL({verify_int, Val, Cond});
	{error, _Reason} ->
	    ok
    end.

%%======================================================================

check_string1(suite) -> [];
check_string1(Config) when is_list(Config) ->
    ?P(check_string1),
    ?line ok = verify_string("kalle & hobbe"),
    ?line ok = verify_not_string(kalle_hobbe),
    ?line ok = verify_not_string(1000),
    ?line ok = verify_not_string(1.0),
    ok.

verify_string(Val) ->
    case (catch snmp_conf:check_string(Val)) of
	{error, Reason} ->
	    ?FAIL({verify_string, Val, Reason});
	ok ->
	    ok
    end.

verify_not_string(Val) ->
    case (catch snmp_conf:check_string(Val)) of
	ok ->
	    ?FAIL({verify_string, Val});
	{error, _Reason} ->
	    ok
    end.


%%======================================================================

check_string2(suite) -> [];
check_string2(Config) when is_list(Config) ->
    ?P(check_string2),
    Str = "kalle & hobbe",
    ?line ok = verify_string(Str, any),
    ?line ok = verify_string(Str, {gt,  length(Str) - 1}),
    ?line ok = verify_string(Str, {gte, length(Str)}),
    ?line ok = verify_string(Str, {lt,  length(Str) + 1}),
    ?line ok = verify_string(Str, {lte, length(Str)}),
    ?line ok = verify_string(Str, length(Str)),

    ?line ok = verify_not_string(kalle_hobbe, any),
    ?line ok = verify_not_string(1000, any),
    ?line ok = verify_not_string(1.0, any),
    ?line ok = verify_not_string(Str, {gt,  length(Str)}),
    ?line ok = verify_not_string(Str, {gte, length(Str) + 1}),
    ?line ok = verify_not_string(Str, {lt,  length(Str)}),
    ?line ok = verify_not_string(Str, {lte, length(Str) - 1}),
    ?line ok = verify_not_string(Str, length(Str) + 1),
    ok.

verify_string(Val, Limit) ->
    case (catch snmp_conf:check_string(Val, Limit)) of
	{error, Reason} ->
	    ?FAIL({verify_string, Val, Limit, Reason});
	ok ->
	    ok
    end.
    
verify_not_string(Val, Limit) ->
    case (catch snmp_conf:check_string(Val, Limit)) of
	ok ->
	    ?FAIL({verify_string, Val, Limit});
	{error, _Reason} ->
	    ok
    end.


%%======================================================================

check_atom(suite) -> [];
check_atom(Config) when is_list(Config) ->
    ?P(check_atom),
    Atoms = [{kalle, "kalle"}, {hobbe, "hobbe"}, {dummy, "dummy"}],
    ?line ok = verify_atom(kalle, Atoms),
    ?line ok = verify_not_atom(anka, Atoms),
    ?line ok = verify_not_atom("kalle", Atoms),
    ?line ok = verify_not_atom(1000, Atoms),
    ok.

verify_atom(Val, Atoms) ->
    case (catch snmp_conf:check_atom(Val, Atoms)) of
	{error, Reason} ->
	    ?FAIL({verify_atom, Val, Atoms, Reason});
	{ok, _} ->
	    ok
    end.

verify_not_atom(Val, Atoms) ->
    case (catch snmp_conf:check_atom(Val, Atoms)) of
	ok ->
	    ?FAIL({verify_atom, Val, Atoms});
	{error, _Reason} ->
	    ok
    end.


%%======================================================================

check_ip(suite) -> [];
check_ip(Config) when is_list(Config) ->
    ?P(check_ip),
    ?line ok = verify_ip([1,2,3,4]),
    ?line ok = verify_not_ip([1,2,3]),
    ?line ok = verify_not_ip([1,2,3,4,5]),
    ?line ok = verify_not_ip(kalle),
    ?line ok = verify_not_ip(1000),
    ?line ok = verify_not_ip([1,2,3.0,4]),
    ?line ok = verify_not_ip([1,two,3,4]),
    ok.

verify_ip(Val) ->
    case (catch snmp_conf:check_ip(Val)) of
	{error, Reason} ->
	    ?FAIL({verify_ip, Val, Reason});
	{ok, _} ->
	    ok;
	ok ->
	    ok
    end.

verify_not_ip(Val) ->
    case (catch snmp_conf:check_ip(Val)) of
	ok ->
	    ?FAIL({verify_ip, Val});
	{error, _Reason} ->
	    ok
    end.


%%======================================================================

check_taddress(suite) -> [];
check_taddress(Config) when is_list(Config) ->
    ?P(check_taddress),
    ?line ok = verify_taddress([1,2,3,4,5,6]),
    ?line ok = verify_not_taddress([1,2,3,4,5]),
    ?line ok = verify_not_taddress([1,2,3,4,5,6,7]),
    ?line ok = verify_not_taddress(kalle),
    ?line ok = verify_not_taddress(1000),
    ?line ok = verify_not_taddress([1,2,3.0,4,5,6]),
    ?line ok = verify_not_taddress([1,two,3,4,5,6]),
    ok.

verify_taddress(Val) ->
    case (catch snmp_conf:check_taddress(snmpUDPDomain, Val)) of
	{error, Reason} ->
	    ?FAIL({verify_taddress, Val, Reason});
	ok ->
	    ok
    end.

verify_not_taddress(Val) ->
    case (catch snmp_conf:check_taddress(snmpUDPDomain, Val)) of
	ok ->
	    ?FAIL({verify_taddress, Val});
	{error, _Reason} ->
	    ok
    end.


%%======================================================================

check_packet_size(suite) -> [];
check_packet_size(Config) when is_list(Config) ->
    ?P(check_packet_size),
    Min = 484,
    Max = 2147483647,
    ?line ok = verify_packet_size(Min),
    ?line ok = verify_packet_size(2*Min),
    ?line ok = verify_packet_size(Max),
    ?line ok = verify_not_packet_size(Min-1),
    ?line ok = verify_not_packet_size(Max+1),
    ?line ok = verify_not_packet_size(kalle),
    ?line ok = verify_not_packet_size("kalle"),
    ?line ok = verify_not_packet_size(1.0),
    ?line ok = verify_not_packet_size(1.0*Max),
    ok.

verify_packet_size(Val) ->
    case (catch snmp_conf:check_packet_size(Val)) of
	{error, Reason} ->
	    ?FAIL({verify_packet_size, Val, Reason});
	ok ->
	    ok
    end.

verify_not_packet_size(Val) ->
    case (catch snmp_conf:check_packet_size(Val)) of
	ok ->
	    ?FAIL({verify_packet_size, Val});
	{error, _Reason} ->
	    ok
    end.


%%======================================================================

check_oid(suite) -> [];
check_oid(Config) when is_list(Config) ->
    ?P(check_oid),
    [_,_|Rest] = ?otpSnmpeaModule,
    ErrOid = [6,16|Rest],
    ?line ok = verify_oid(?system),
    ?line ok = verify_oid(?sysDescr_instance),
    ?line ok = verify_oid(?otpSnmpeaModule),
    ?line ok = verify_not_oid(kalle),
    ?line ok = verify_not_oid("kalle"),
    ?line ok = verify_not_oid(1000),
    ?line ok = verify_not_oid(1.0),
    ?line ok = verify_not_oid(ErrOid),
    ok.

verify_oid(Val) ->
    case (catch snmp_conf:check_oid(Val)) of
	{error, Reason} ->
	    ?FAIL({verify_oid, Val, Reason});
	ok ->
	    ok
    end.

verify_not_oid(Val) ->
    case (catch snmp_conf:check_oid(Val)) of
	ok ->
	    ?FAIL({verify_oid, Val});
	{error, _Reason} ->
	    ok
    end.


%%======================================================================

check_sec_model1(suite) -> [];
check_sec_model1(Config) when is_list(Config) ->
    ?P(check_sec_model1),
    Exclude1 = [],
    Exclude2 = [v1],
    Exclude3 = [v1,usm],
    ?line ok = verify_sec_model(any, Exclude1),
    ?line ok = verify_sec_model(v1,  Exclude1),
    ?line ok = verify_sec_model(v2c, Exclude1),
    ?line ok = verify_sec_model(usm, Exclude1),
    ?line ok = verify_sec_model(any, Exclude2),
    ?line ok = verify_sec_model(v2c, Exclude2),
    ?line ok = verify_not_sec_model(v1, Exclude2),
    ?line ok = verify_not_sec_model(v1, Exclude3),
    ?line ok = verify_not_sec_model(usm, Exclude3),
    ok.

verify_sec_model(Val, Exclude) ->
    case (catch snmp_conf:check_sec_model(Val, Exclude)) of
	{error, Reason} ->
	    ?FAIL({verify_sec_model, Val, Reason});
	{ok, _} ->
	    ok
    end.

verify_not_sec_model(Val, Exclude) ->
    case (catch snmp_conf:check_sec_model(Val, Exclude)) of
	{ok, Res} ->
	    ?FAIL({verify_sec_model, Val, Res});
	{error, _Reason} ->
	    ok
    end.


%%======================================================================

check_sec_model2(suite) -> [];
check_sec_model2(Config) when is_list(Config) ->
    ?P(check_sec_model2),
    ?line ok = verify_sec_model(v1,  v1,  []),
    ?line ok = verify_sec_model(v1,  v1,  [v2c]),
    ?line ok = verify_sec_model(v2c, v2c, []),
    ?line ok = verify_sec_model(v2c, v2c, [v1]),
    ?line ok = verify_sec_model(v3,  usm, []),
    ?line ok = verify_sec_model(v3,  usm, [v2c]),
    ?line ok = verify_not_sec_model(v1,    v2c, []),
    ?line ok = verify_not_sec_model(v1,    v3,  [v2c]),
    ?line ok = verify_not_sec_model(v1,    v1,  [v1]),
    ?line ok = verify_not_sec_model(v2c,   v1,  []),
    ?line ok = verify_not_sec_model(v2c,   v3,  [v3]),
    ?line ok = verify_not_sec_model(v2c,   v2c, [v2c]),
    ?line ok = verify_not_sec_model(v3,    v1,  []),
    ?line ok = verify_not_sec_model(v3,    v2c, [v1]),
    ?line ok = verify_not_sec_model(v3,    v3,  [v2c]),
    ?line ok = verify_not_sec_model(kalle, v3,  []),
    ?line ok = verify_not_sec_model(1000,  v3,  []),
    ?line ok = verify_not_sec_model(1.0,   v3,  []),
    ok.


verify_sec_model(M1, M2, Exclude) ->
    case (catch snmp_conf:check_sec_model(M1, M2, Exclude)) of
	{error, Reason} ->
	    ?FAIL({verify_sec_model, M1, M2, Reason});
	{ok, _} ->
	    ok
    end.

verify_not_sec_model(M1, M2, Exclude) ->
    case (catch snmp_conf:check_sec_model(M1, M2, Exclude)) of
	{ok, Res} ->
	    ?FAIL({verify_sec_model, M1, M2, Res});
	{error, _Reason} ->
	    ok
    end.


%%======================================================================

check_sec_level(suite) -> [];
check_sec_level(Config) when is_list(Config) ->
    ?P(check_sec_level),
    ?line ok = verify_sec_level(noAuthNoPriv),
    ?line ok = verify_sec_level(authNoPriv),
    ?line ok = verify_sec_level(authPriv),
    ?line ok = verify_not_sec_level(kalle),
    ?line ok = verify_not_sec_level("noAuthNoPriv"),
    ?line ok = verify_not_sec_level(1000),
    ?line ok = verify_not_sec_level(1.0),
    ok.


verify_sec_level(Val) ->
    case (catch snmp_conf:check_sec_level(Val)) of
	{error, Reason} ->
	    ?FAIL({verify_sec_level, Val, Reason});
	{ok, _} ->
	    ok;
	Error ->
	    ?FAIL({verify_sec_level, Val, Error})
    end.

verify_not_sec_level(Val) ->
    case (catch snmp_conf:check_sec_level(Val)) of
	{ok, Res} ->
	    ?FAIL({verify_sec_level, Val, Res});
	{error, _Reason} ->
	    ok;
	{'EXIT', _Reason} ->
	    ok
    end.


%%======================================================================

check_timer(suite) -> [];
check_timer(Config) when is_list(Config) ->
    ?P(check_timer),
    ?line ok = verify_timer(infinity),
    ?line ok = verify_timer(1),
    ?line ok = verify_timer(10),
    ?line ok = verify_timer(2147483647),
    ?line ok = verify_timer(2*2147483647),
    ?line ok = verify_timer({1,1,0,0}),
    ?line ok = verify_timer({10,10,10,10}),
    ?line ok = verify_timer({2147483647,2147483647,2147483647,2147483647}),
    ?line ok = verify_not_timer(ytinifni),
    ?line ok = verify_not_timer("ytinifni"),
    ?line ok = verify_not_timer(0),
    ?line ok = verify_not_timer(-10),
    ?line ok = verify_not_timer({0,1,0,0}),
    ?line ok = verify_not_timer({1,0,0,0}),
    ?line ok = verify_not_timer({1,1,-1,0}),
    ?line ok = verify_not_timer({1,1,0,-1}),
    ?line ok = verify_not_timer({1.0,1,0,0}),
    ?line ok = verify_not_timer({1,1.0,0,0}),
    ?line ok = verify_not_timer({1,1,1.0,0}),
    ?line ok = verify_not_timer({1,1,0,1.0}),
    ?line ok = verify_not_timer({"1",1,0,0}),
    ?line ok = verify_not_timer({1,"1",0,0}),
    ?line ok = verify_not_timer({1,1,"0",0}),
    ?line ok = verify_not_timer({1,1,0,"0"}),
    ok.

verify_timer(Val) ->
    case (catch snmp_conf:check_timer(Val)) of
	{error, Reason} ->
	    ?FAIL({verify_timer, Val, Reason});
	{ok, _} ->
	    ok
    end.

verify_not_timer(Val) ->
    case (catch snmp_conf:check_timer(Val)) of
	{ok, Res} ->
	    ?FAIL({verify_timer, Val, Res});
	{error, _Reason} ->
	    ok
    end.


%%======================================================================

read(suite) -> [];
read(Config) when is_list(Config) ->
    ?P(read),
    ?SKIP(not_implemented_yet).


%%======================================================================

read_files(suite) -> [];
read_files(Config) when is_list(Config) ->
    ?P(read_files),
    ?SKIP(not_implemented_yet).


%%======================================================================
%% Internal functions
%%======================================================================

% d(F) ->
%     d(F, []).

% d(F, A) ->
%     io:format("~w:" ++ F ++ "~n", [?MODULE|A]).
