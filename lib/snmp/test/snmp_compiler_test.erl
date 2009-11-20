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
%% Purpose: Test the snmp mib compiler
%% 
%% Run test: ts:run(snmp, snmp_compiler_test, [batch]).
%% 
%%----------------------------------------------------------------------
-module(snmp_compiler_test).

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
         init_per_testcase/2, fin_per_testcase/2,

	 description/1,
	 oid_conflicts/1,
	 imports/1,
	 module_identity/1,

	 tickets/1,
	 otp_6150/1

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
    [{comp_dir, CompDir},{mib_dir, MibDir}|Config].

fin_per_testcase(_Case, Config) when is_list(Config) ->
    CompDir = ?config(comp_dir, Config),
    ?line ok = ?DEL_DIR(CompDir),
    lists:keydelete(comp_dir, 1, Config).


%%======================================================================
%% Test case definitions
%%======================================================================

all(suite) ->
    [
     description,
     oid_conflicts,
     imports,
     module_identity,
     tickets
    ].

tickets(suite) ->
    [
     otp_6150
    ].


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


imports(suite) ->
    [];
imports(Config) when is_list(Config) ->
    ?SKIP(not_yet_implemented).


module_identity(suite) ->
    [];
module_identity(Config) when is_list(Config) ->
    ?SKIP(not_yet_implemented).


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

