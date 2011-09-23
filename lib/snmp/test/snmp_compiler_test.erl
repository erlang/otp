%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2011. All Rights Reserved.
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
%% Purpose: Test the snmp mib compiler
%% 
%% Run test: ts:run(snmp, snmp_compiler_test, [batch]).
%% 
%%----------------------------------------------------------------------
-module(snmp_compiler_test).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("test_server/include/test_server.hrl").
-include("snmp_test_lib.hrl").
-include_lib("snmp/include/snmp_types.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
	 all/0, 
	 groups/0, init_per_group/2, end_per_group/2, 
         init_per_testcase/2, end_per_testcase/2,

	 description/1,
	 oid_conflicts/1,
	 imports/1,
	 module_identity/1,
	 agent_capabilities/1,
	 module_compliance/1, 
	 warnings_as_errors/1,

	 otp_6150/1,
	 otp_8574/1, 
	 otp_8595/1

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
    Dir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    [_|RL] = lists:reverse(filename:split(DataDir)),
    MibDir = join(lists:reverse(["snmp_test_data"|RL])),
    CompDir = join(Dir, "comp_dir/"),
    ?line ok = file:make_dir(CompDir),
    [{comp_dir, CompDir}, {mib_dir, MibDir} | Config].

end_per_testcase(_Case, Config) when is_list(Config) ->
    CompDir = ?config(comp_dir, Config),
    ?line ok = ?DEL_DIR(CompDir),
    lists:keydelete(comp_dir, 1, Config).


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
     {group, tickets}
    ].

groups() -> 
    [{tickets, [], [otp_6150, otp_8574, otp_8595]}].

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

    Dir = ?config(comp_dir, Config),
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

    Dir = ?config(comp_dir, Config),
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
    Dir     = ?config(comp_dir, Config),
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
    put(tname,otp_6150),
    p("starting with Config: ~p~n", [Config]),

    Dir     = ?config(comp_dir, Config),
    MibDir  = ?config(mib_dir,  Config),
    MibFile = join(MibDir, "ERICSSON-TOP-MIB.mib"),
    ?line {ok, Mib} = snmpc:compile(MibFile, [{outdir, Dir}, {verbosity, trace}]),
    io:format("otp_6150 -> Mib: ~n~p~n", [Mib]),
    ok.


%%======================================================================

otp_8574(suite) ->
    [];
otp_8574(Config) when is_list(Config) ->
    put(tname,otp_8574),
    p("starting with Config: ~p~n", [Config]),

    Dir     = ?config(comp_dir, Config),
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
    put(tname,otp_8595),
    p("starting with Config: ~p~n", [Config]),

    Dir     = ?config(comp_dir, Config),
    MibDir  = ?config(mib_dir,  Config),
    MibFile = join(MibDir, "OTP8595-MIB.mib"),
    ?line {ok, Mib} = 
	snmpc:compile(MibFile, [{outdir,      Dir}, 
				{verbosity,   trace}, 
				{group_check, false}]),
    io:format("otp_8595 -> Mib: ~n~p~n", [Mib]),
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
    Message = file:write_file(Filename ,Binary),
case Message of
    ok -> ok;
    {error, Reason} ->
	exit({failed_writing_mib,Reason})
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


join(Comp) ->
    filename:join(Comp).

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
              "~n" ++ F ++ "~n", [TName,format_timestamp(now())|A]).

format_timestamp({_N1, _N2, N3}   = Now) ->
    {Date, Time}   = calendar:now_to_datetime(Now),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate =
        io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
                      [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),
    lists:flatten(FormatDate).

