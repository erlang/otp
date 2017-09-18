%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2017. All Rights Reserved.
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
%% Purpose: Test the snmp mib compiler
%% 
%% Run test: ts:run(snmp, snmp_compiler_test, [batch]).
%% 
%%----------------------------------------------------------------------
-module(snmp_compiler_test).

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
	 all/0, 
	 groups/0, init_per_group/2, end_per_group/2, 
         init_per_testcase/2, end_per_testcase/2,
         init_per_suite/1, end_per_suite/1,

	 description/1,
	 oid_conflicts/1,
	 imports/1,
	 module_identity/1,
	 agent_capabilities/1,
	 module_compliance/1, 
	 warnings_as_errors/1,
	 augments_extra_info/1,

	 otp_6150/1,
	 otp_8574/1, 
	 otp_8595/1, 
	 otp_10799/1, 
	 otp_10808/1,
	 otp_14145/1,
         otp_13014/1
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

init_per_suite(Config0) when is_list(Config0) ->

    ?DBG("init_per_suite -> entry with"
	 "~n   Config0: ~p", [Config0]),

    Config1   = snmp_test_lib:init_suite_top_dir(?MODULE, Config0), 
    Config2   = snmp_test_lib:fix_data_dir(Config1),

    %% Mib-dirs
    %% data_dir is trashed by the test-server / common-test
    %% so there is no point in fixing it...
    MibDir    = snmp_test_lib:lookup(data_dir, Config2),
    StdMibDir = filename:join([code:priv_dir(snmp), "mibs"]),

    [{mib_dir, MibDir}, {std_mib_dir, StdMibDir} | Config2].

end_per_suite(Config) when is_list(Config) ->

    ?DBG("end_per_suite -> entry with"
	 "~n   Config: ~p", [Config]),

    Config.


init_per_testcase(Case, Config) when is_list(Config) ->

    ?DBG("init_per_testcase -> entry with"
	 "~n   Config: ~p", [Config]),

    CaseTopDir = snmp_test_lib:init_testcase_top_dir(Case, Config), 

    [{case_top_dir, CaseTopDir} | Config].

end_per_testcase(_Case, Config) when is_list(Config) ->
    Config.


%%======================================================================
%% Test case definitions
%%======================================================================

all() -> 
    [
     description, 
     oid_conflicts, 
     imports, 
     module_identity, 
     agent_capabilities, 
     module_compliance, 
     warnings_as_errors,
     augments_extra_info, 
     {group, tickets}
    ].

groups() -> 
    [{tickets, [],
      [otp_6150, otp_8574, otp_8595, otp_10799, otp_10808, otp_14145,
       otp_13014]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.




%%======================================================================
%% Test functions
%%======================================================================

description(suite) -> [];
description(Config) when is_list(Config) ->
    put(tname,desc),
    p("starting with Config: ~p~n", [Config]),

    Dir = ?config(case_top_dir, Config),
    Filename   = join(Dir,"test"),
    MibSrcName = Filename ++ ".mib",
    MibBinName = Filename ++ ".bin",
    Desctext   = "This is a test description",
    Oid = [1,3,6,1,2,1,15,1],
    write_mib(MibSrcName,Desctext),
    ?line {ok,_} = snmpc:compile(MibSrcName, [{outdir,      Dir},
					      {group_check, false},
					      {warnings,    false},
					      {description, false}]),
    MIB1 = read_mib(MibBinName),
    %% io:format("description -> MIB1: ~n~p~n", [MIB1]),
    check_mib(MIB1#mib.mes, Oid,  undefined),
    ?line {ok,_} = snmpc:compile(MibSrcName, [{outdir,      Dir},
					      {group_check, false},
					      {warnings,    false},
					      {description, true}]),
    MIB2 = read_mib(MibBinName),
    %% io:format("description -> MIB2: ~n~p~n", [MIB2]),
    check_mib(MIB2#mib.mes, Oid, Desctext),

    %% Cleanup
    file:delete(MibSrcName),
    file:delete(MibBinName),
    ok.


%%======================================================================

oid_conflicts(suite) -> [];
oid_conflicts(Config) when is_list(Config) ->
    put(tname,oid_conflicts),
    p("starting with Config: ~p~n", [Config]),

    Dir = ?config(case_top_dir, Config),
    Mib = join(Dir,"TESTv2.mib"),
    ?line ok = write_oid_conflict_mib(Mib),
    ?line {error,compilation_failed} = 
	snmpc:compile(Mib,[{outdir, Dir},{verbosity,trace}]),
    ok.


%%======================================================================

imports(suite) ->
    [];
imports(Config) when is_list(Config) ->
    ?SKIP(not_yet_implemented).


%%======================================================================

module_identity(suite) ->
    [];
module_identity(Config) when is_list(Config) ->
    ?SKIP(not_yet_implemented).


%%======================================================================

agent_capabilities(suite) ->
    [];
agent_capabilities(Config) when is_list(Config) ->
    put(tname,agent_capabilities),
    p("starting with Config: ~p~n", [Config]),

    SnmpPrivDir    = code:priv_dir(snmp),
    SnmpMibsDir    = join(SnmpPrivDir, "mibs"), 
    OtpMibsPrivDir = code:priv_dir(otp_mibs),
    OtpMibsMibsDir = join(OtpMibsPrivDir, "mibs"), 
    Dir   = ?config(mib_dir, Config),
    AcMib = join(Dir,"AC-TEST-MIB.mib"),
    ?line {ok, MibFile1} = snmpc:compile(AcMib, [options,
						 version,
						 {i,         [SnmpMibsDir, OtpMibsMibsDir]}, 
						 {outdir,    Dir}, 
						 {verbosity, trace}]),
    ?line {ok, Mib1} = snmp_misc:read_mib(MibFile1), 
    ?line {ok, MibFile2} = snmpc:compile(AcMib, [options,
						 version,
						 agent_capabilities,
						 {i,         [SnmpMibsDir, OtpMibsMibsDir]}, 
						 {outdir,    Dir}, 
						 {verbosity, trace}]),
    ?line {ok, Mib2} = snmp_misc:read_mib(MibFile2), 
    MEDiff = Mib2#mib.mes -- Mib1#mib.mes,
    %% This is a rather pathetic test, but it is somthing...
    io:format("agent_capabilities -> "
	      "~n   MEDiff: ~p"
	      "~n   Mib1:   ~p"
	      "~n   Mib2:   ~p"
	      "~n", [MEDiff, Mib1, Mib2]),
    case length(MEDiff) of
	2 ->
	    ok;
	_BadLen ->
	    exit({unexpected_mes, MEDiff})
    end,
    ok.


%%======================================================================

module_compliance(suite) ->
    [];
module_compliance(Config) when is_list(Config) ->
    put(tname,module_compliance),
    p("starting with Config: ~p~n", [Config]),

    SnmpPrivDir    = code:priv_dir(snmp),
    SnmpMibsDir    = join(SnmpPrivDir, "mibs"), 
    OtpMibsPrivDir = code:priv_dir(otp_mibs),
    OtpMibsMibsDir = join(OtpMibsPrivDir, "mibs"), 
    Dir   = ?config(mib_dir, Config),
    AcMib = join(Dir,"MC-TEST-MIB.mib"),
    ?line {ok, MibFile1} = snmpc:compile(AcMib, [options,
						 version,
						 {i,           [SnmpMibsDir, OtpMibsMibsDir]}, 
						 {outdir,      Dir}, 
						 {verbosity,   trace}]),
    ?line {ok, Mib1} = snmp_misc:read_mib(MibFile1), 
    ?line {ok, MibFile2} = snmpc:compile(AcMib, [options,
						 version,
						 module_compliance,
						 {i,           [SnmpMibsDir, OtpMibsMibsDir]}, 
						 {outdir,      Dir}, 
						 {verbosity,   trace}]),
    ?line {ok, Mib2} = snmp_misc:read_mib(MibFile2), 
    MEDiff = Mib2#mib.mes -- Mib1#mib.mes,
    %% This is a rather pathetic test, but it is somthing...
    io:format("agent_capabilities -> "
	      "~n   MEDiff: ~p"
	      "~n   Mib1:   ~p"
	      "~n   Mib2:   ~p"
	      "~n", [MEDiff, Mib1, Mib2]),
    case length(MEDiff) of
	1 ->
	    ok;
	_BadLen ->
	    exit({unexpected_mes, MEDiff})
    end,
    ok.


%%======================================================================

warnings_as_errors(suite) ->
    ["OTP-9437"];
warnings_as_errors(Config) when is_list(Config) ->
    put(tname,warnings_as_errors),
    p("starting with Config: ~p~n", [Config]),
    Dir     = ?config(case_top_dir, Config),
    MibDir  = ?config(mib_dir,  Config),
    MibFile = join(MibDir, "OTP8574-MIB.mib"),
    OutFile = join(Dir, "OTP8574-MIB.bin"),
    Opts =  [{group_check, false},
	     {outdir,      Dir},
	     {verbosity,   trace},
	     relaxed_row_name_assign_check],
    {error, compilation_failed} =
	snmpc:compile(MibFile, [warnings_as_errors|Opts]),
    false = filelib:is_regular(OutFile),
    {ok, _} = snmpc:compile(MibFile, Opts),
    true = filelib:is_regular(OutFile),
    ok = file:delete(OutFile),
    ok.


%%======================================================================

otp_6150(suite) ->
    [];
otp_6150(Config) when is_list(Config) ->
    put(tname, otp6150),
    p("starting with Config: ~p~n", [Config]),

    Dir     = ?config(case_top_dir, Config),
    MibDir  = ?config(mib_dir,  Config),
    MibFile = join(MibDir, "ERICSSON-TOP-MIB.mib"),
    ?line {ok, Mib} = 
	snmpc:compile(MibFile, [{outdir, Dir}, {verbosity, trace}]),
    io:format("otp_6150 -> Mib: ~n~p~n", [Mib]),
    ok.


%%======================================================================

otp_8574(suite) ->
    [];
otp_8574(Config) when is_list(Config) ->
    put(tname, otp8574),
    p("starting with Config: ~p~n", [Config]),

    Dir     = ?config(case_top_dir, Config),
    MibDir  = ?config(mib_dir,  Config),
    MibFile = join(MibDir, "OTP8574-MIB.mib"),
    
    p("ensure compile fail without relaxed assign check"),
    case snmpc:compile(MibFile, [{group_check, false}, {outdir, Dir}]) of
	{error, compilation_failed} ->
	    p("with relaxed assign check MIB compiles with warning"),
	    case snmpc:compile(MibFile, [{group_check, false}, 
					 {outdir, Dir}, 
					 relaxed_row_name_assign_check]) of
		{ok, _Mib} ->
		    ok;
		{error, Reason} ->
		    p("unexpected compile failure: "
		      "~n   Reason: ~p", [Reason]),
		    exit({unexpected_compile_failure, Reason})
	    end;

	{ok, _} ->
	    p("unexpected compile success"),
	    exit(unexpected_compile_success)
    end.


%%======================================================================

otp_8595(suite) ->
    [];
otp_8595(Config) when is_list(Config) ->
    put(tname, otp8595),
    p("starting with Config: ~p~n", [Config]),

    Dir     = ?config(case_top_dir, Config),
    MibDir  = ?config(mib_dir,  Config),
    MibFile = join(MibDir, "OTP8595-MIB.mib"),
    ?line {ok, Mib} = 
	snmpc:compile(MibFile, [{outdir,      Dir}, 
				{verbosity,   trace}, 
				{group_check, false}]),
    p("Mib: ~n~p~n", [Mib]),
    ok.


%%======================================================================

otp_10799(suite) ->
    [];
otp_10799(Config) when is_list(Config) ->
    put(tname, otp10799),
    p("starting with Config: ~p~n", [Config]),

    Dir     = ?config(case_top_dir, Config),
    MibDir  = ?config(mib_dir,      Config),
    MibFile = join(MibDir, "OTP10799-MIB.mib"),
    ?line {ok, Mib} = 
	snmpc:compile(MibFile, [{outdir, Dir}, {verbosity, trace}]),
    p("Mib: ~n~p~n", [Mib]),
    ok.


%%======================================================================

otp_10808(suite) ->
    [];
otp_10808(Config) when is_list(Config) ->
    put(tname, otp10808),
    p("starting with Config: ~p~n", [Config]),

    Dir     = ?config(case_top_dir, Config),
    MibDir  = ?config(mib_dir,      Config),
    MibFile = join(MibDir, "OTP10808-MIB.mib"),
    ?line {ok, Mib} = 
	snmpc:compile(MibFile, [{outdir,      Dir}, 
				{verbosity,   trace}, 
				{group_check, false}]),
    p("Mib: ~n~p~n", [Mib]),
    ok.


%%======================================================================

otp_14145(suite) ->
    [];
otp_14145(Config) when is_list(Config) ->
    put(tname, otp14145),
    p("starting with Config: ~p~n", [Config]),

    Dir     = ?config(case_top_dir, Config),
    MibDir  = ?config(mib_dir,      Config),
    MibName = "OTP14145-MIB",
    MibFile = join(MibDir, MibName++".mib"),
    ?line {ok, MibBin} =
	snmpc:compile(MibFile, [{outdir, Dir},
				{verbosity, trace},
				{group_check, false},
				module_compliance]),
    p("Mib: ~n~p~n", [MibBin]),
    MIB = read_mib(MibBin),
    Oid = [1,3,6,1,2,1,67,4],
    check_mib(MIB#mib.mes, Oid, undefined),
    ok.


%%======================================================================

otp_13014(suite) ->
    [];
otp_13014(Config) when is_list(Config) ->
    put(tname, otp13014),
    p("starting with Config: ~p~n", [Config]),

    Dir     = ?config(case_top_dir, Config),
    MibDir  = ?config(mib_dir,      Config),
    MibName = "Test-LLDP-MIB",
    MibFile = join(MibDir, MibName++".mib"),
    ?line {ok, MibBin} =
	snmpc:compile(MibFile, [{outdir, Dir},
				{verbosity, log},
				{group_check, false},
				module_compliance]),
    p("Mib: ~n~p~n", [MibBin]),
    #mib{mes = MEs} = read_mib(MibBin),
    Oid = [1,0,8802,1,1,2,1,1,7],
    #me{
       entrytype = table,
       aliasname = lldpConfigManAddrTable,
       assocList = [{table_info,TableInfo}]} =
        lists:keyfind(Oid, #me.oid, MEs),
    #table_info{
       nbr_of_cols = 1,
       first_accessible = 1,
       not_accessible = [],
       index_types = {augments,{lldpLocManAddrEntry,undefined}}} =
        TableInfo,
    ok.


%%======================================================================

augments_extra_info(suite) ->
    [];
augments_extra_info(Config) when is_list(Config) ->
    put(tname, augments_extra_info),
    p("starting with Config: ~p~n", [Config]),

    Dir       = ?config(case_top_dir, Config),
    MibDir    = ?config(mib_dir,      Config),
    Test2File = join(MibDir, "Test2.mib"),
    Test3File = join(MibDir, "Test3.mib"),
    ?line {ok, Test2BinFile} = 
	snmpc:compile(Test2File, [{outdir,      Dir}, 
				  {verbosity,   silence}, 
				  {group_check, false}]),
    io:format("Test2BinFile: ~n~p~n", [Test2BinFile]),
    ?line {ok, Test3BinFile} = 
	snmpc:compile(Test3File, [{i,           [MibDir]}, 
				  {outdir,      Dir}, 
				  {verbosity,   silence}, 
				  {group_check, true}]),
    io:format("Test3BinFile: ~n~p~n", [Test3BinFile]),
    {ok, Test3Mib} = snmp_misc:read_mib(Test3BinFile), 
    io:format("Test3Mib: ~n~p~n", [Test3Mib]),
    %% There is only one table in this mib
    #mib{table_infos = [{TableName, TI}]} = Test3Mib, 
    io:format("TableName: ~p"
	      "~n   Table Info: ~p"
	      "~n", [TableName, TI]), 
    #table_info{nbr_of_cols      = 4, 
		defvals          = DefVals, 
		not_accessible   = [2,4], 
		index_types      = {augments, {tEntry, undefined}},
		first_accessible = 1} = TI,
    io:format("Table info:   ~p"
	      "~n   DefVals: ~p"
	      "~n", [TableName, DefVals]), 
    ok.


%%======================================================================
%% Internal functions
%%======================================================================

write_oid_conflict_mib(Filename) ->
    MibText = "TESTv2 DEFINITIONS ::= BEGIN

IMPORTS
	MODULE-IDENTITY, OBJECT-TYPE, NOTIFICATION-TYPE,
    Integer32, snmpModules ,experimental
        FROM SNMPv2-SMI
	MODULE-COMPLIANCE, OBJECT-GROUP, NOTIFICATION-GROUP
        FROM SNMPv2-CONF
	DisplayString 
        FROM SNMPv2-TC
	RowStatus
        FROM STANDARD-MIB;


exampleModule MODULE-IDENTITY
LAST-UPDATED \"0005290000Z\"
        ORGANIZATION \"Erlang\"
        CONTACT-INFO \" test mib
                        Ericsson Utvecklings AB
Open System
Box 1505
SE-125 25 ÄLVSJÖ\"

DESCRIPTION 
\" Objects for management \"
        REVISION   \"0005290000Z\"
        DESCRIPTION 
\"The initial version\"
        ::= { snmpModules 1 }

example1 OBJECT IDENTIFIER ::= { experimental 7}
-- example2 OBJECT IDENTIFIER ::= { experimental 7}


myName OBJECT-TYPE
SYNTAX      DisplayString
MAX-ACCESS  read-write
STATUS  current
DESCRIPTION
\"My own name\"
              ::= { example1 1 }

myNotification NOTIFICATION-TYPE
STATUS      current 
DESCRIPTION 
\"test trap.\" 
              ::= { example1 1 }

friendsTable OBJECT-TYPE
SYNTAX  SEQUENCE OF FriendsEntry
MAX-ACCESS  not-accessible
STATUS   current
DESCRIPTION
\"A list of friends.\"
              ::= { example1 4 }

friendsEntry OBJECT-TYPE
SYNTAX  FriendsEntry
MAX-ACCESS  not-accessible
STATUS  current
DESCRIPTION
\"\"
              INDEX   { fIndex }
::= { friendsTable 1 }

FriendsEntry ::= SEQUENCE {
		   fIndex   INTEGER,
		   fName    DisplayString,
		   fAddress DisplayString,
		   fStatus  RowStatus
		  }

fIndex OBJECT-TYPE
SYNTAX      INTEGER
MAX-ACCESS  read-only
STATUS      current
DESCRIPTION
\"number of friend\"
              ::= { friendsEntry 1 }

fName OBJECT-TYPE
SYNTAX      DisplayString (SIZE (0..255))
MAX-ACCESS  read-write
STATUS      current
DESCRIPTION
\"Name of  a friend\"
              ::= { friendsEntry 2 }

fAddress OBJECT-TYPE
SYNTAX      DisplayString (SIZE (0..255))
MAX-ACCESS  read-write
STATUS      current
DESCRIPTION
\"Address of a friend\"
              ::= { friendsEntry 3 }

fStatus OBJECT-TYPE
SYNTAX      RowStatus
MAX-ACCESS  read-write
STATUS      current
DESCRIPTION
\"The status of this conceptual row.\"
              ::= { friendsEntry 4 }

-- myName2 OBJECT IDENTIFIER ::= { example1 1 }

friendGroup OBJECT-GROUP
OBJECTS { myName, fIndex, fName,fAddress, fStatus } 
STATUS current
DESCRIPTION \" A object group\"
        ::= { example1 2 }

myNotificationGroup NOTIFICATION-GROUP 
NOTIFICATIONS { myNotification } 
STATUS     current 
DESCRIPTION 
\"Test notification group\" 
      ::= { example1 3 }
END",

    file:write_file(Filename, MibText).


write_mib(Filename,Desc) ->
    Binary = "Test DEFINITIONS ::= BEGIN

IMPORTS
	MODULE-IDENTITY, OBJECT-TYPE, 
    snmpModules, mib-2
        FROM SNMPv2-SMI ;

snmpMIB MODULE-IDENTITY
LAST-UPDATED \"9511090000Z\"
    ORGANIZATION \"\" 
    CONTACT-INFO \"\"
    DESCRIPTION
::= { snmpModules 1 }


test   OBJECT IDENTIFIER ::= { mib-2 15 }

bits1 OBJECT-TYPE
SYNTAX      BITS { b0(0), b1(1), b2(2) }
MAX-ACCESS  read-write
STATUS      current
DESCRIPTION	\"" ++ Desc ++ "\"
    ::= { test 1 }

END",
    Message = file:write_file(Filename, Binary),
case Message of
    ok -> ok;
    {error, Reason} ->
	exit({failed_writing_mib, Reason})
end.


read_mib(Filename) ->
    case file:read_file(Filename) of
        {ok,Bin} -> 
	    binary_to_term(Bin);     
        {error,Reason} ->
            exit({failed_reading_mib,Filename,Reason})
    end.

check_mib([],_,_) ->
    not_found;
check_mib([#me{oid = Oid, description = Description}| _T], Oid, Testdata) ->
    check_desc(Description, Testdata);
check_mib([_H|T], Oid, Testdata ) ->
    check_mib(T, Oid, Testdata ).

check_desc(Desc, Desc) ->
    ok;
check_desc(Desc1, Desc2) ->
    exit({'description not equal', Desc1, Desc2}).


%% join(Comp) ->
%%     filename:join(Comp).

join(A,B) ->
    filename:join(A,B).


%% ------

%% p(F) ->
%%     p(F, []).

p(F) ->
    p(F, []).

p(F, A) ->
    p(get(tname), F, A).

p(TName, F, A) ->
    io:format("*** [~w][~s] ***"
              "~n" ++ F ++ "~n", [TName, formated_timestamp()|A]).

formated_timestamp() ->
    snmp_test_lib:formated_timestamp().
