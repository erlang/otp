%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2022. All Rights Reserved.
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
-module(snmp_conf_SUITE).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").
-include("snmp_test_lib.hrl").

-include_lib("snmp/include/STANDARD-MIB.hrl").
-include_lib("snmp/include/OTP-SNMPEA-MIB.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
         suite/0, all/0, groups/0,
         init_per_suite/1,    end_per_suite/1,
         init_per_group/2,    end_per_group/2, 
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
	 read_files/1,

         fd_leak_check/1
	]).


%%======================================================================
%% Common Test interface functions
%%======================================================================

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    [
     check_mandatory,
     check_integer1, check_integer2,
     check_string1, check_string2,
     check_atom,
     check_ip,
     check_taddress,
     check_packet_size,
     check_oid,
     check_sec_model1,
     check_sec_model2,
     check_sec_level,
     check_timer, 
     read, read_files,
     fd_leak_check
    ].

groups() -> 
    [].



%%
%% -----
%%

init_per_suite(Config0) when is_list(Config0) ->
    ?IPRINT("init_per_suite -> entry with"
            "~n      Config0: ~p", [Config0]),

    case ?LIB:init_per_suite(Config0) of
        {skip, _} = SKIP ->
            SKIP;

        Config1 when is_list(Config1) ->
            %% We need a monitor on this node also
            snmp_test_sys_monitor:start(),

            PrivDir    = ?config(priv_dir, Config1),
            PrivSubdir = filename:join(PrivDir, "snmp_conf_test"),
            ok = filelib:ensure_dir(filename:join(PrivSubdir, "dummy")),
            Config2 = [{priv_subdir, PrivSubdir} | Config1],

            ?IPRINT("init_per_suite -> end when"
                    "~n      Config: ~p", [Config2]),
            
            Config2
    end.

end_per_suite(Config0) when is_list(Config0) ->
    ?IPRINT("end_per_suite -> entry with"
            "~n      Config0: ~p", [Config0]),

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

init_per_testcase(fd_leak_check = _Case, Config) when is_list(Config) ->
    ?IPRINT("init_per_testcase -> entry with"
            "~n   Config: ~p", [Config]),

    %% There are other ways to test this:
    %% lsof (linux and maybe FreeBSD):    lsof -p <pid>
    %%    os:cmd("lsof -p " ++ os:getpid() ++ " | grep -v COMMAND | wc -l").
    %% fstat (FreeBSD, OpenBSD and maybe NetBSD): fstat -p <pid>
    %%    os:cmd("fstat -p " ++ os:getpid() ++ " | grep -v USER | wc -l").
    %% But this (list:dir) is good enough...
    case os:type() of
        {unix, linux} ->
            ?IPRINT("init_per_testcase -> linux: check proc fs"),
            case file:read_file_info("/proc/" ++ os:getpid() ++ "/fd") of
                {ok, #file_info{type   = directory,
                                access = Access}}
                  when (Access =:= read) orelse
                       (Access =:= read_write) ->
                    ?IPRINT("init_per_testcase -> linux: usingh proc fs"),
                    [{num_open_fd, fun num_open_fd_using_list_dir/0} |
                     Config];
                _ ->
                    {skip, "Not proc fd"}
            end;
        {unix, solaris} ->
            %% Something strange happens when we use pfiles from within erlang,
            %% so skip the test for now

            %% For some reason even though 'which' exists (at least in
            %% a tcsh shell), it hangs when called via os:cmd/1.
            %% And type produces results that is not so easy to
            %% "analyze". So, we 'try it' and know that it starts 
            %% by writing the pid and program on the first line...

            %% ?IPRINT("init_per_testcase -> solaris: check pfiles"),
            %% PID = os:getpid(),
            %% case string:find(os:cmd("pfiles -n " ++ PID), PID) of
            %%     nomatch ->
            %%         {skip, "pfiles not found"};
            %%     _ ->
            %%         ?IPRINT("init_per_testcase -> solaris: pfiles found"),
            %%         [{num_open_fd, fun num_open_fd_using_pfiles/0} |
            %%          Config]
            %% end;
            {skip, "pfiles not found"};

        {unix, BSD} when (BSD =:= freebsd) orelse
                         (BSD =:= openbsd) orelse
                         (BSD =:= netbsd) ->
            ?IPRINT("init_per_testcase -> ~w: check fstat", [BSD]),
            case os:cmd("which fstat") of
                [] ->
                    {skip, "fstat not found"};
                _ ->
                    ?IPRINT("init_per_testcase -> ~w: fstat found", [BSD]),
                    [{num_open_fd, fun num_open_fd_using_fstat/0} |
                     Config]
            end;
        {win32, _} ->
            ?IPRINT("init_per_testcase -> windows: no check"),
            {skip, "Not implemented"};
        _ ->
            ?IPRINT("init_per_testcase -> no check"),
            {skip, "Not implemented"}
    end;
init_per_testcase(_Case, Config) when is_list(Config) ->
    Config.

end_per_testcase(_Case, Config) when is_list(Config) ->
    Config.



%%======================================================================
%% Test functions
%%======================================================================

check_mandatory(suite) -> [];
check_mandatory(Config) when is_list(Config) ->
    ?P(check_mandatory),
    %% ?IPRINT("check_mandatory -> entry"),
    A1 = [{a, hej}, {b, hopp}, {c, 10}, {d, 10101}, {f, 10.88}],
    B1 = [{a, {value, hejsan}}, 
	  {b, mandatory}, 
	  {d, {value, 20202}}, 
	  {e, {value, "kalle"}}],
    {ok, _L1} = verify_mandatory(A1, B1),
    ?DBG("check_mandatory -> L1: ~p", [L1]),
    A2 = [{a, hej}, {c, 10}, {d, 10101}, {f, 10.88}],
    B2 = [{a, {value, hejsan}}, 
	  {b, mandatory}, 
	  {d, {value, 20202}}, 
	  {e, {value, "kalle"}}],
    ok = verify_not_mandatory(A2, B2),
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
    ok = verify_int(0),
    ok = verify_int(16#FF),
    ok = verify_int(16#FFFF),
    ok = verify_int(16#FFFFFFFF),
    ok = verify_int(-1),
    ok = verify_int(-333),

    ok = verify_not_int("kalle & hobbe"),
    ok = verify_not_int(kalle_och_hobbe),
    ok = verify_not_int(1.5),

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

    ok = verify_int(0,      any),
    ok = verify_int(-22222, any),
    ok = verify_int(33333,  any),
    ok = verify_int(1,      pos),
    ok = verify_int(9999,   pos),
    ok = verify_int(-1,     neg),
    ok = verify_int(-9999,  neg),
    ok = verify_int(1,      {gt, 0}),
    ok = verify_int(88888,  {gt, -255}),
    ok = verify_int(88888,  {gte, -255}),
    ok = verify_int(88888,  {gte, 88888}),
    ok = verify_int(88888,  {lt,  88889}),
    ok = verify_int(88888,  {lte, 88888}),
    ok = verify_int(88888,  {eq,  88888}),
    ok = verify_int(88888,  {range, 88887,88889}),

    ok = verify_not_int("kalle & hobbe", any),
    ok = verify_not_int(kalle_och_hobbe, any),
    ok = verify_not_int(1.5,             any),

    ok = verify_not_int(0,      pos),
    ok = verify_not_int(-22222, pos),
    ok = verify_not_int(33333,  neg),
    ok = verify_not_int(0,      {gt,  0}),
    ok = verify_not_int(33333,  {gt,  99999}),
    ok = verify_not_int(33333,  {gt,  33333}),
    ok = verify_not_int(33333,  {gte, 33334}),
    ok = verify_not_int(33333,  {lt,  33333}),
    ok = verify_not_int(33333,  {lte, 33332}),
    ok = verify_not_int(33333,  {eq,  33332}),
    ok = verify_not_int(33333,  {eq,  -33333}),
    ok = verify_not_int(33333,  {range, 33334, 33338}),
    ok = verify_not_int(33339,  {range, 33334, 33338}),
    ok = verify_not_int(33333,  {gt,  kalle}),
    ok = verify_not_int(33333,  {gt,  1.55}),
    ok = verify_not_int(33333,  {gte, "hejsan"}),
    ok = verify_not_int(33333,  {lt,  hobbe}),
    ok = verify_not_int(33333,  {lte, 1.7666}),
    ok = verify_not_int(33333,  {eq,  33333.0}),
    ok = verify_not_int(33333,  {eq,  -33333.0}),
    ok = verify_not_int(33333,  {range, kalle, 33338}),
    ok = verify_not_int(33339,  {range, 33334, kalle}),
    ok = verify_not_int(33339,  {kalle, 33334, kalle}),

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
    ok = verify_string("kalle & hobbe"),
    ok = verify_not_string(kalle_hobbe),
    ok = verify_not_string(1000),
    ok = verify_not_string(1.0),
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
    ok = verify_string(Str, any),
    ok = verify_string(Str, {gt,  length(Str) - 1}),
    ok = verify_string(Str, {gte, length(Str)}),
    ok = verify_string(Str, {lt,  length(Str) + 1}),
    ok = verify_string(Str, {lte, length(Str)}),
    ok = verify_string(Str, length(Str)),

    ok = verify_not_string(kalle_hobbe, any),
    ok = verify_not_string(1000, any),
    ok = verify_not_string(1.0, any),
    ok = verify_not_string(Str, {gt,  length(Str)}),
    ok = verify_not_string(Str, {gte, length(Str) + 1}),
    ok = verify_not_string(Str, {lt,  length(Str)}),
    ok = verify_not_string(Str, {lte, length(Str) - 1}),
    ok = verify_not_string(Str, length(Str) + 1),
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
    ok = verify_atom(kalle, Atoms),
    ok = verify_not_atom(anka, Atoms),
    ok = verify_not_atom("kalle", Atoms),
    ok = verify_not_atom(1000, Atoms),
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
    ok = verify_ip([1,2,3,4]),
    ok = verify_not_ip([1,2,3]),
    ok = verify_not_ip([1,2,3,4,5]),
    ok = verify_not_ip(kalle),
    ok = verify_not_ip(1000),
    ok = verify_not_ip([1,2,3.0,4]),
    ok = verify_not_ip([1,two,3,4]),
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
    ok = verify_taddress([1,2,3,4,5,6]),
    ok = verify_not_taddress([1,2,3,4,5]),
    ok = verify_not_taddress([1,2,3,4,5,6,7]),
    ok = verify_not_taddress(kalle),
    ok = verify_not_taddress(1000),
    ok = verify_not_taddress([1,2,3.0,4,5,6]),
    ok = verify_not_taddress([1,two,3,4,5,6]),
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
    ok = verify_packet_size(Min),
    ok = verify_packet_size(2*Min),
    ok = verify_packet_size(Max),
    ok = verify_not_packet_size(Min-1),
    ok = verify_not_packet_size(Max+1),
    ok = verify_not_packet_size(kalle),
    ok = verify_not_packet_size("kalle"),
    ok = verify_not_packet_size(1.0),
    ok = verify_not_packet_size(1.0*Max),
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
    ok = verify_oid(?system),
    ok = verify_oid(?sysDescr_instance),
    ok = verify_oid(?otpSnmpeaModule),
    ok = verify_not_oid(kalle),
    ok = verify_not_oid("kalle"),
    ok = verify_not_oid(1000),
    ok = verify_not_oid(1.0),
    ok = verify_not_oid(ErrOid),
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
    ok = verify_sec_model(any, Exclude1),
    ok = verify_sec_model(v1,  Exclude1),
    ok = verify_sec_model(v2c, Exclude1),
    ok = verify_sec_model(usm, Exclude1),
    ok = verify_sec_model(any, Exclude2),
    ok = verify_sec_model(v2c, Exclude2),
    ok = verify_not_sec_model(v1, Exclude2),
    ok = verify_not_sec_model(v1, Exclude3),
    ok = verify_not_sec_model(usm, Exclude3),
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
    ok = verify_sec_model(v1,  v1,  []),
    ok = verify_sec_model(v1,  v1,  [v2c]),
    ok = verify_sec_model(v2c, v2c, []),
    ok = verify_sec_model(v2c, v2c, [v1]),
    ok = verify_sec_model(v3,  usm, []),
    ok = verify_sec_model(v3,  usm, [v2c]),
    ok = verify_not_sec_model(v1,    v2c, []),
    ok = verify_not_sec_model(v1,    v3,  [v2c]),
    ok = verify_not_sec_model(v1,    v1,  [v1]),
    ok = verify_not_sec_model(v2c,   v1,  []),
    ok = verify_not_sec_model(v2c,   v3,  [v3]),
    ok = verify_not_sec_model(v2c,   v2c, [v2c]),
    ok = verify_not_sec_model(v3,    v1,  []),
    ok = verify_not_sec_model(v3,    v2c, [v1]),
    ok = verify_not_sec_model(v3,    v3,  [v2c]),
    ok = verify_not_sec_model(kalle, v3,  []),
    ok = verify_not_sec_model(1000,  v3,  []),
    ok = verify_not_sec_model(1.0,   v3,  []),
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
    ok = verify_sec_level(noAuthNoPriv),
    ok = verify_sec_level(authNoPriv),
    ok = verify_sec_level(authPriv),
    ok = verify_not_sec_level(kalle),
    ok = verify_not_sec_level("noAuthNoPriv"),
    ok = verify_not_sec_level(1000),
    ok = verify_not_sec_level(1.0),
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
    ok = verify_timer(infinity),
    ok = verify_timer(1),
    ok = verify_timer(10),
    ok = verify_timer(2147483647),
    ok = verify_timer(2*2147483647),
    ok = verify_timer({1,1,0,0}),
    ok = verify_timer({10,10,10,10}),
    ok = verify_timer({2147483647,2147483647,2147483647,2147483647}),
    ok = verify_not_timer(ytinifni),
    ok = verify_not_timer("ytinifni"),
    ok = verify_not_timer(0),
    ok = verify_not_timer(-10),
    ok = verify_not_timer({0,1,0,0}),
    ok = verify_not_timer({1,0,0,0}),
    ok = verify_not_timer({1,1,-1,0}),
    ok = verify_not_timer({1,1,0,-1}),
    ok = verify_not_timer({1.0,1,0,0}),
    ok = verify_not_timer({1,1.0,0,0}),
    ok = verify_not_timer({1,1,1.0,0}),
    ok = verify_not_timer({1,1,0,1.0}),
    ok = verify_not_timer({"1",1,0,0}),
    ok = verify_not_timer({1,"1",0,0}),
    ok = verify_not_timer({1,1,"0",0}),
    ok = verify_not_timer({1,1,0,"0"}),
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

%% Since we need something to read, we also write.
%% And we can just as well check then (after write) too...
fd_leak_check(suite) -> [];
fd_leak_check(Config) when is_list(Config) ->
    ?TC_TRY(fd_leak_check,
            fun() -> ok end,
            fun(_) -> do_fd_leak_check(Config) end,
            fun(_) -> ok end).

do_fd_leak_check(Config) ->
    Dir       = ?config(priv_subdir, Config),
    NumOpenFD = ?config(num_open_fd, Config),

    %% Create "some" config
    ?IPRINT("do_fd_leak_check -> create some config"),
    Entries = [
               snmpa_conf:agent_entry(intAgentIpAddress, {0,0,0,0}),
               snmpa_conf:agent_entry(intAgentUDPPort, 161),
               snmpa_conf:agent_entry(snmpEngineMaxMessageSize, 484),
               snmpa_conf:agent_entry(snmpEngineID, "fooBarEI")
              ],

    ?IPRINT("do_fd_leak_check -> get number of FD (before test)"),
    NumFD01 = NumOpenFD(),
    ?IPRINT("do_fd_leak_check -> before config write: ~w", [NumFD01]),

    ?IPRINT("do_fd_leak_check -> write config to file"),
    ok = snmpa_conf:write_agent_config(Dir, Entries),

    NumFD02 = NumOpenFD(),
    ?IPRINT("do_fd_leak_check -> (after write) before config read: ~w", [NumFD02]),

    {ok, _} = snmpa_conf:read_agent_config(Dir),


    NumFD03 = NumOpenFD(),
    ?IPRINT("do_fd_leak_check -> after config read: ~w", [NumFD03]),
    if
        (NumFD01 =:= NumFD02) andalso (NumFD02 =:= NumFD03) ->
            ?IPRINT("do_fd_leak_check -> fd leak check ok"),
            ok;
        true ->
            ?EPRINT("do_fd_leak_check -> fd leak check failed: "
                    "~n      Before write: ~w"
                    "~n      Before read:  ~w"
                    "~n      After read:   ~w", [NumFD01, NumFD02, NumFD03]),
            ?FAIL({num_open_fd, NumFD01, NumFD02, NumFD03})
    end.



%% There are other ways to test this:
%% lsof (linux and maybe FreeBSD):    lsof -p <pid>
%%    os:cmd("lsof -p " ++ os:getpid() ++ " | grep -v COMMAND | wc -l").
%% fstat (FreeBSD and maybe OpenBSD): fstat -p <pid>
%%    os:cmd("fstat -p " ++ os:getpid() ++ " | grep -v USER | wc -l").
%% But this is good enough...
num_open_fd_using_list_dir() ->
    case file:list_dir("/proc/" ++ os:getpid() ++ "/fd") of
        {ok, FDs} ->
            length(FDs);
        {error, Reason} ->
            ?EPRINT("Failed listing proc fs (for fd): "
                    "~n      Reason: ~p", [Reason]),
            ?SKIP({failed_listing_fd, Reason})
    end.


%% num_open_fd_using_pfiles() ->
%%     NumString = os:cmd("pfiles -n " ++ os:getpid() ++
%%                            " | grep -v " ++ os:getpid() ++
%%                            " | grep -v \"Current rlimit\" | wc -l"),
%%     try list_to_integer(string:trim(NumString))
%%     catch
%%         C:E ->
%%             ?EPRINT("Failed pfiles: "
%%                     "~n      Error Class: ~p"
%%                     "~n      Error:       ~p", [C, E]),
%%             ?SKIP({failed_pfiles, C, E})
%%     end.


num_open_fd_using_fstat() ->
    NumString = os:cmd("fstat -p " ++ os:getpid() ++
                           " | grep -v MOUNT | wc -l"),
    try list_to_integer(string:trim(NumString))
    catch
        C:E ->
            ?EPRINT("Failed fstat: "
                    "~n      Error Class: ~p"
                    "~n      Error:       ~p", [C, E]),
            ?SKIP({failed_fstat, C, E})
    end.


%%======================================================================
%% Internal functions
%%======================================================================

