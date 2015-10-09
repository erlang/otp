%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
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
%%% Purpose : Test suite for the ASN.1 application

-module(xmerl_xsd_SUITE).

-compile(export_all).
%%-export([Function/Arity, ...]).

-include_lib("test_server/include/test_server.hrl").
%%-include("xmerl.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-import(xmerl_xsd_type,[check_simpleType/3]).

all() -> 
    [{group, type_tests}, {group, facets},
     {group, misc_block_tests}, {group, validation_tests},
     {group, ticket_tests}].

groups() -> 
    [{type_tests, [],
      [{group, primitive_datatypes},
       {group, derived_datatypes}]},
     {validation_tests, [],
      [{group, xmlSchemaPrimerExamples},
       {group, miscXMLexamples}]},
     {primitive_datatypes, [],
      [string, boolean, decimal, float, double, duration,
       dateTime, time, date, gYearMonth, gYear, gMonthDay,
       gDay, gMonth, hexBinary, base64Binary, anyURI, 'QName',
       'NOTATION']},
     {derived_datatypes, [],
      [normalizedString, token, language, 'NMTOKEN',
       'NMTOKENS', 'Name', 'NCName', 'ID', 'IDREF', 'IDREFS',
       'ENTITY', 'ENTITIES', integer, nonPositiveInteger,
       negativeInteger, long, int, short, byte,
       nonNegativeInteger, unsignedLong, unsignedInt,
       unsignedShort, unsignedByte, positiveInteger]},
     {xmlSchemaPrimerExamples, [],
      [po, po1, po2, ipo, ipo_redefine, '4Q99']},
     {miscXMLexamples, [],
      [small, complexType1, model_group_all,
       substitutionGroup, attributeGroup, test_key1, sis1,
       sis2, state2file_file2state, union]},
     {ticket_tests, [],
      [ticket_6910, ticket_7165, ticket_7190, ticket_7288,
       ticket_7736, ticket_8599, ticket_9410]},
     {facets, [],
      [length, minLength, maxLength, pattern, enumeration,
       whiteSpace, maxInclusive, maxExclusive, minExclusive,
       minInclusive, totalDigits, fractionDigits]},
     {misc_block_tests, [],
      [compare_dateTime, compare_duration]}].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.

init_per_testcase(_TestCase,Config) ->
    {ok, _} = 
	file:read_file_info(filename:join([?config(priv_dir,Config)])),
    code:add_patha(?config(priv_dir,Config)),
    Dog=test_server:timetrap({minutes,10}),
    [{watchdog, Dog}|Config].

end_per_testcase(_Func,Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.


string(suite) -> []; 
string(_Config) ->
    %% #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
    Str = [16#9,16#A,16#D,16#20,16#D7FF,16#E000,16#FFFD,16#10000,
		 16#10FFFF],
    ?line {ok,_} = check_simpleType(string,Str,dummy).
    
boolean(suite) -> [];
boolean(_Config) ->
    ?line {ok,_} = check_simpleType(boolean,"1",dummy),
    ?line {ok,_} = check_simpleType(boolean,"0",dummy),
    ?line {ok,_} = check_simpleType(boolean,"true",dummy),
    ?line {ok,_} = check_simpleType(boolean,"false",dummy),
    ?line {error,_Reason} = check_simpleType(boolean,"gurka",dummy).

decimal(suite) -> [];
decimal(_Config) ->
    ?line {ok,_} = check_simpleType(decimal,"-1.23",dummy),
    ?line {ok,_} = check_simpleType(decimal,"12678967.543233",dummy),
    ?line {ok,_} = check_simpleType(decimal,"+100000.00",dummy),
    ?line {ok,_} = check_simpleType(decimal,"210",dummy).

float(suite) -> [];
float(_Config) ->
    %% -1E4, 1267.43233E12, 12.78e-2, 12 , -0, 0 , INF, -INF, NaN
    ?line {ok,_} = check_simpleType(float,"-1E4",dummy),
    ?line {ok,_} = check_simpleType(float,"1267.43233E12",dummy),
    ?line {ok,_} = check_simpleType(float,"12.78e-2",dummy),
    ?line {ok,_} = check_simpleType(float,"12",dummy),
    ?line {ok,_} = check_simpleType(float,"-0",dummy),
    ?line {ok,_} = check_simpleType(float,"0",dummy),
    ?line {ok,_} = check_simpleType(float,"INF",dummy),
    ?line {ok,_} = check_simpleType(float,"-INF",dummy),
    ?line {ok,_} = check_simpleType(float,"NaN",dummy).


double(suite) -> []; 
double(_Config) ->
    %% -1E4, 1267.43233E12, 12.78e-2, 12 , -0, 0 , INF, -INF, NaN
    ?line {ok,_} = check_simpleType(double,"-1E4",dummy),
    ?line {ok,_} = check_simpleType(double,"1267.43233E12",dummy),
    ?line {ok,_} = check_simpleType(double,"12.78e-2",dummy),
    ?line {ok,_} = check_simpleType(double,"12",dummy),
    ?line {ok,_} = check_simpleType(double,"-0",dummy),
    ?line {ok,_} = check_simpleType(double,"0",dummy),
    ?line {ok,_} = check_simpleType(double,"INF",dummy),
    ?line {ok,_} = check_simpleType(double,"-INF",dummy),
    ?line {ok,_} = check_simpleType(double,"NaN",dummy).


duration(suite) -> []; 
duration(_Config) ->
    %% allowed: P1Y2M3DT10H30M -P120D P1347Y P1347M P1Y2MT2H 
    %% P0Y1347M P0Y1347M0D -P1347M 
    %% not allowed: P-1347M P1Y2MT
    ?line {ok,_} = check_simpleType(duration,"P1Y2M3DT10H30M",dummy),
    ?line {ok,_} = check_simpleType(duration,"-P120D",dummy),
    ?line {ok,_} = check_simpleType(duration,"P1347Y",dummy),
    ?line {ok,_} = check_simpleType(duration,"P1347M",dummy),
    ?line {ok,_} = check_simpleType(duration,"P1Y2MT2H",dummy),
    ?line {ok,_} = check_simpleType(duration,"P0Y1347M",dummy),
    ?line {ok,_} = check_simpleType(duration,"P0Y1347M0D",dummy),
    ?line {ok,_} = check_simpleType(duration,"-P1347M",dummy),

    ?line {error,_} = check_simpleType(duration,"P-1347M",dummy),
    ?line {error,_} = check_simpleType(duration,"P1Y2MT",dummy).

%% '-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
dateTime(suite) -> []; 
dateTime(_Config) ->
    %% 2002-10-10T12:00:00-05:00
    DT1 = "2002-10-10T12:00:00-05:00",
    ?line {ok,_} = check_simpleType(dateTime,DT1,dummy),
    DT2 = "2002-10-10T17:00:00Z",
    ?line {ok,_} = check_simpleType(dateTime,DT2,dummy),
    %% plus sign prohibited
    DT3 = "+2002-10-10T17:00:00Z",
    ?line {error,_Reason3} = check_simpleType(dateTime,DT3,dummy),
    %% leading zeros when year are more than four digits prohibited
    DT4 = "002002-10-10T17:00:00Z",
    ?line {error,_Reason4} = check_simpleType(dateTime,DT4,dummy),
    DT5 = "1953-12-31T12:10:10.10+12:00",
    ?line {ok,_} = check_simpleType(dateTime,DT5,dummy).

time(suite) -> []; 
time(_Config) ->
    %% hh:mm:ss.sss with optional following time zone indicator.
    T1 = "13:20:00-05:00",
    ?line {ok,_} = check_simpleType(time,T1,dummy),
    %% canonical repr. of midnight
    T2 = "00:00:00",
    ?line {ok,_} = check_simpleType(time,T2,dummy),
    T3 = "12:34:56",
    ?line {ok,_} = check_simpleType(time,T3,dummy),
    T4 = "12:34:56.552",
    ?line {ok,_} = check_simpleType(time,T4,dummy),
    T5 = "12:34:56.552Z",
    ?line {ok,_} = check_simpleType(time,T5,dummy).    

date(suite) -> []; 
date(_Config) ->
    %% '-'? yyyy '-' mm '-' dd zzzzzz?
    %%  is 
    D1 = "2002-10-10+13:00",
    ?line {ok,_} = check_simpleType(date,D1,dummy),
    D2 = "2002-10-09-11:00",
    ?line {ok,_} = check_simpleType(date,D2,dummy),

    D12 = "+2002-13-09-11:00",
    ?line {error,_Reason12} = check_simpleType(date,D12,dummy),
    D13 = "2002-13-09-11:00",
    ?line {error,_Reason13} = check_simpleType(date,D13,dummy),
    D14 = "2002-12-39-11:00",
    ?line {error,_Reason14} = check_simpleType(date,D14,dummy).

gYearMonth(suite) -> []; 
gYearMonth(_Config) -> 
    %% '-'? yyyy '-' mm zzzzzz?
    GYM1 = "1955-10",
    ?line {ok,_} = check_simpleType(gYearMonth,GYM1,dummy),
    GYM2 = "-1955-10",
    ?line {ok,_} = check_simpleType(gYearMonth,GYM2,dummy),
    GYM3 = "1955-10Z",
    ?line {ok,_} = check_simpleType(gYearMonth,GYM3,dummy),
    GYM4 = "0055-10+10:00",
    ?line {ok,_} = check_simpleType(gYearMonth,GYM4,dummy),
    GYM5 = "0955-10Z",
    ?line {ok,_} = check_simpleType(gYearMonth,GYM5,dummy),
    GYM6 = "-11955-01",
    ?line {ok,_} = check_simpleType(gYearMonth,GYM6,dummy),

    ?line {error,_} = check_simpleType(gYearMonth,"+2000-10",dummy),
    ?line {error,_} = check_simpleType(gYearMonth,"2000-00",dummy),
    ?line {error,_} = check_simpleType(gYearMonth,"2000-10+10:70",dummy). 

gYear(suite) -> []; 
gYear(_Config) ->
    %% '-'? yyyy zzzzzz?
    ?line {ok,_} = check_simpleType(gYear,"2000",dummy),
    ?line {ok,_} = check_simpleType(gYear,"2000-11:30",dummy),
    ?line {ok,_} = check_simpleType(gYear,"-2000",dummy),
    ?line {error,_} = check_simpleType(gYear,"0000",dummy).

gMonthDay(suite) -> []; 
gMonthDay(_Config) ->
    %% mm '-' dd zzzzzz?
    ?line {ok,_} = check_simpleType(gMonthDay,"--05-03",dummy),
    ?line {ok,_} = check_simpleType(gMonthDay,"--05-03Z",dummy),
    ?line {error,_} = check_simpleType(gMonthDay,"05-00",dummy),
    ?line {error,_} = check_simpleType(gMonthDay,"00-03",dummy),
    ?line {error,_} = check_simpleType(gMonthDay,"-05-03",dummy).

gDay(suite) -> [];
gDay(_Config) ->
    %% dd zzzzzz?
    ?line {ok,_} = check_simpleType(gDay,"---05",dummy),
    ?line {ok,_} = check_simpleType(gDay,"---30+03:00",dummy),
    ?line {error,_} = check_simpleType(gDay,"-30+03:00",dummy),
    ?line {error,_} = check_simpleType(gDay,"---00+03:00",dummy),
    ?line {error,_} = check_simpleType(gDay,"---40+03:00",dummy),
    ?line {error,_} = check_simpleType(gDay,"05",dummy).

gMonth(suite) -> []; 
gMonth(_Config) ->
    %% mm zzzzzz?
    ?line {ok,_} = check_simpleType(gMonth,"--05",dummy),
    ?line {ok,_} = check_simpleType(gMonth,"--10+03:00",dummy),
    ?line {error,_} = check_simpleType(gMonth,"-10+03:00",dummy),
    ?line {error,_} = check_simpleType(gMonth,"00+03:00",dummy),
    ?line {error,_} = check_simpleType(gMonth,"14",dummy),
    ?line {error,_} = check_simpleType(gMonth,"05",dummy).


hexBinary(suite) -> [];
hexBinary(_Config) ->
    %% an even number of hexadecimal digits ([0-9a-fA-F]).
    ?line {ok,_} = check_simpleType(hexBinary,"05",dummy),
    ?line {ok,_} = check_simpleType(hexBinary,"aF",dummy),
    ?line {ok,_} = check_simpleType(hexBinary,
					     "0123456789abcdefABCDEF",dummy),
    ?line {error,_} = check_simpleType(hexBinary,
					     "0123456789absdefABCDEF",dummy),
    ?line {error,_} = check_simpleType(hexBinary,"aF5",dummy),
    ?line {error,_} = check_simpleType(hexBinary,"aFG",dummy).

base64Binary(suite) -> []; 
base64Binary(_Config) ->
    %% a-z, A-Z, 0-9, the plus sign (+), the forward slash (/) and the
    %% equal sign (=), together with the characters defined in [XML
    %% 1.0 (Second Edition)] as white space.(16#9, 16#A, 16#D, 16#20)
    ?line {ok,_} = check_simpleType(base64Binary,"05+/AA==",dummy),
    ?line {ok,_} = check_simpleType(base64Binary,"05+/AA= =",dummy),
    ?line {ok,_} = check_simpleType(base64Binary,"05+/A A= =",dummy),
    ?line {ok,_} = check_simpleType(base64Binary,"05+/ AA= =",dummy),
    ?line {error,_} = check_simpleType(base64Binary,"05+/AA== ",dummy),
    B64B1 = "AbCd GhZz 09w=",
    ?line {ok,_} = check_simpleType(base64Binary,B64B1,dummy),
    B64B2 = "AbCd GhZ9 0z8 =",
    ?line {ok,_} = check_simpleType(base64Binary,B64B2,dummy),
    ?line {ok,_} = check_simpleType(base64Binary,"0z8 =",dummy),
    ErrB641 = "AbCd GZ9 0z8 =",
    ?line {error,_} = check_simpleType(base64Binary,ErrB641,dummy).

anyURI(suite) -> [];
anyURI(_Config) ->
    URI1 = "ftp://ftp.is.co.za/rfc/rfc1808.txt",
    URI2 = "gopher://spinaltap.micro.umn.edu/00/Weather/California/Los%20Angeles",
    URI3 = "http://www.math.uio.no/faq/compression-faq/part1.html",
    URI4 = "mailto:mduerst@ifi.unizh.ch",
    URI5 = "news:comp.infosystems.www.servers.unix",
    URI6 = "telnet://melvyl.ucop.edu/",
    ?line ok=ok_loop(anyURI,[URI1,URI2,URI3,URI4,URI5,URI6]).


'QName'(suite) -> [];
'QName'(_Config) ->
    %%  QName 	   ::= 	(Prefix ':')? LocalPart
    %% 	Prefix 	   ::= 	NCName
    %% 	LocalPart  ::= 	NCName
    ?line {ok,_} = check_simpleType('QName',"abc:def",dummy),
    ?line {ok,_} = check_simpleType('QName',"abc",dummy),
    ?line {ok,_} = check_simpleType('QName',"abc:def:ijk",dummy).

'NOTATION'(suite) -> [];
'NOTATION'(_Config) ->
    ?line {ok,_} = check_simpleType('NOTATION',"abc:def",dummy),
    ?line {ok,_} = check_simpleType('NOTATION',"abc",dummy),
    ?line {ok,_} = check_simpleType('NOTATION',"abc:def:ijk",dummy).

normalizedString(suite) -> [];
normalizedString(_Config) ->
    %% not contain the carriage return (#xD), line feed (#xA) nor tab
    %% (#x9) characters.
    NStr1 = "this string is ok with extra space     between characters",
    NotNStr1 = "this string is not normalized \t",
    NotNStr2 = "neither is this \n string",
    NotNStr3 = "or this \r string",
    ?line {ok,_}  = check_simpleType(normalizedString,NStr1,dummy),
    ?line ok=error_loop(normalizedString,[NotNStr1,NotNStr2,NotNStr3]).

token(suite) -> [];
token(_Config) ->
    %% not contain the carriage return (#xD), line feed (#xA) nor tab
    %% (#x9) characters, that have no leading or trailing spaces
    %% (#x20) and that have no internal sequences of two or more
    %% spaces.
    T1 = "this string is tokenized with only single space between characters",
    NotT1 = "this string is not ok with  extra space     between  characters",
    NotT2 = " neither leading space",
    NotT3 = "nor trailing space ",
    NotT4 = "tabs not \t allowed",
    NotT5 = "newlines not allowed\n",
    NotT6 = "or \r (carriage return)",
    ?line {ok,_}  = check_simpleType(token,T1,dummy),
    ?line ok=error_loop(token,[NotT1,NotT2,NotT3,NotT4,NotT5,NotT6]).

language(suite) -> [];
language(_Config) ->
    %% strings that conform to the pattern
    %% [a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*
    L = "Abra-cadabra-123",
    NotL1 = "Abra123-cadabra!",
    NotL2 = "Abra-",
    NotL3 = "Abracadabra",
    NotL4 = "Abra-cadabrrra",
    ?line {ok,_}  = check_simpleType(language,L,dummy),
    ?line ok=error_loop(language,[NotL1,NotL2,NotL3,NotL4]).

'NMTOKEN'(suite) -> [];
'NMTOKEN'(_Config) ->
    N = "name:withoutspace",
    NotN1 = "name with space",
    NotN2 = "namewith#strang/chars)",
    ?line {ok,_}  = check_simpleType('NMTOKEN',N,dummy),
    ?line {error,_} = check_simpleType('NMTOKEN',NotN1,dummy),
    ?line {error,_} = check_simpleType('NMTOKEN',NotN2,dummy).

'NMTOKENS'(suite) -> [];
'NMTOKENS'(_Config) ->
    N1 = "name1 name:2 name:three",
    NotN1 = "name na%me",
    ?line {ok,_}  = check_simpleType('NMTOKENS',N1,dummy),
    ?line {error,_} = check_simpleType('NMTOKENS',NotN1,dummy).

'Name'(suite) -> [];
'Name'(_Config) ->
    ?line {ok,_}  = check_simpleType('Name',"_valid_Name",dummy).

'NCName'(suite) -> [];
'NCName'(_Config) ->
    ?line {ok,_}  = check_simpleType('NCName',"_valid_Name",dummy).

'ID'(suite) -> [];
'ID'(_Config) ->
    ?line {ok,_}  = check_simpleType('ID',"_valid_Name",dummy).

'IDREF'(suite) -> []; 
'IDREF'(_Config) ->
    ?line {ok,_}  = check_simpleType('IDREF',"_valid_Name",dummy).

'IDREFS'(suite) -> [];
'IDREFS'(_Config) ->
    ?line {ok,_}  = check_simpleType('IDREFS',"_valid_Name Name2",dummy).

'ENTITY'(suite) -> [];
'ENTITY'(_Config) ->
    ?line {ok,_}  = check_simpleType('ENTITY',"_valid_Name",dummy).

'ENTITIES'(suite) -> [];
'ENTITIES'(_Config) ->
    ?line {ok,_}  = check_simpleType('ENTITIES',"name name3",dummy).

integer(suite) -> [];
integer(_Config) ->
    IntList = ["-1", "0", "12678967543233", "+100000"],
    ?line ok = ok_loop(integer,IntList),
    ?line {error,_} = check_simpleType(integer,"1.3",dummy).

nonPositiveInteger(suite) -> [];
nonPositiveInteger(_Config) ->
    NPIList = ["0", "-12678967543233", "-100000"],
    ?line ok = ok_loop(nonPositiveInteger,NPIList),
    ?line {error,_} = check_simpleType(nonPositiveInteger,"1",dummy).

negativeInteger(suite) -> [];
negativeInteger(_Config) ->
    NIList = ["-1", "-12678967543233", "-100000"],
    ?line ok = ok_loop(negativeInteger,NIList),
    ?line {error,_} = check_simpleType(negativeInteger,"1",dummy),
    ?line {error,_} = check_simpleType(negativeInteger,"0",dummy).

long(suite) -> [];
long(_Config) ->
    L = ["9223372036854775807","-9223372036854775808","-1", "0",
	 "12678967543233", "+100000"],
    ?line ok = ok_loop(long,L),
    Err = ["9223372036854775808","-9223372036854775809"],
    ?line ok = error_loop(long,Err).

int(suite) -> [];
int(_Config) ->
    L = ["2147483647", "-2147483648", "-1", "0", "126789675", "+100000"],
    ?line ok = ok_loop(int,L),
    Err = ["2147483648", "-2147483649"],
    ?line ok = error_loop(int,Err).

short(suite) -> [];
short(_Config) ->
    L = ["32767", "-32768", "-1", "0", "12678", "+10000"],
    ?line ok = ok_loop(short,L),
    Err = ["32768", "-32769"],
    ?line ok = error_loop(short,Err).

byte(suite) -> [];
byte(_Config) ->
    L = ["-1", "0", "126", "+100", "127", "-128"],
    ?line ok = ok_loop(byte,L),
    Err = ["128", "-129"],
    ?line ok = error_loop(byte,Err).

nonNegativeInteger(suite) -> [];
nonNegativeInteger(_Config) ->
    L = ["1", "0", "12678967543233", "+100000"],
    ?line ok = ok_loop(nonNegativeInteger,L),
    ?line {error,_} = check_simpleType(nonNegativeInteger,"-1",dummy).

unsignedLong(suite) -> [];
unsignedLong(_Config) ->
    L = ["0", "12678967543233", "100000", "18446744073709551615"],
    ?line ok = ok_loop(unsignedLong,L),
    Err = ["-1","18446744073709551616"],
    ?line ok = error_loop(unsignedLong,Err).

unsignedInt(suite) -> [];
unsignedInt(_Config) ->
    L = ["4294967295", "0", "1267896754", "100000"],
    ?line ok = ok_loop(unsignedInt,L),
    Err = ["-1","4294967296"],
    ?line ok = error_loop(unsignedInt,Err).

unsignedShort(suite) -> [];
unsignedShort(_Config) ->
    L = ["65535", "0", "12678", "10000"],
    ?line ok = ok_loop(unsignedShort,L),
    Err = ["-1","65536"],
    ?line ok = error_loop(unsignedShort,Err).

unsignedByte(suite) -> [];
unsignedByte(_Config) ->
    L = ["255", "0", "126", "100"],
    ?line ok = ok_loop(unsignedByte,L),
    Err = ["-1","256"],
    ?line ok = error_loop(unsignedByte,Err). 

positiveInteger(suite) -> [];
positiveInteger(_Config) ->
    L = ["1", "12678967543233", "+100000"],
    ?line ok = ok_loop(positiveInteger,L),
    Err = ["-1","0"],
    ?line ok = error_loop(positiveInteger,Err). 
    


ok_loop(_Type,[]) ->
    ok;
ok_loop(Type,[H|T]) ->
    ?line {ok,_} = check_simpleType(Type,H,dummy),
    ok_loop(Type,T).

error_loop(_T,[]) ->
    ok;
error_loop(Type,[H|T]) ->
    ?line {error,_} = check_simpleType(Type,H,dummy),
    error_loop(Type,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Testing facets
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



length(suite) -> [];
length(_Config) ->
    ?line {ok,"string"} = 
	(xmerl_xsd_type:facet_fun(string,{length,"6"}))("string"),
    ?line {error,{length,12,should_be,6}} =
	(xmerl_xsd_type:facet_fun(string,{length,"6"}))("stringstring"),
    ok.
    
minLength(suite) -> [];
minLength(_Config) ->
    ?line {ok,"string"} = 
	(xmerl_xsd_type:facet_fun(string,{minLength,"6"}))("string"),
    ?line {error,{minLength,3,should_at_least_be,6}} =
	(xmerl_xsd_type:facet_fun(string,{minLength,"6"}))("str"),
    ok.

maxLength(suite) -> [];
maxLength(_Config) ->
    ?line {ok,"string"} =
	(xmerl_xsd_type:facet_fun(string,{maxLength,"6"}))("string"),
    ?line {error,{maxLength,12,should_not_be_more_than,6}} =
	(xmerl_xsd_type:facet_fun(string,{maxLength,"6"}))("stringstring"),
    ok.

pattern(suite) -> [];
pattern(_Config) ->
    RE1 = "[a-z]{5}",
    ?line {ok,"calle"} = 
	(xmerl_xsd_type:facet_fun(string,{pattern,RE1}))
	  ("calle"),
    ?line {error,{pattern_mismatch,"cal",RE1}} =
	(xmerl_xsd_type:facet_fun(string,{pattern,RE1}))
	  ("cal"),
    RE2 = "[A-Z]{2}\\d\\s\\d[A-Z]{2}",
    ?line {ok,"AY2 3BC"} =
	(xmerl_xsd_type:facet_fun(string,{pattern,RE2}))
	  ("AY2 3BC"),
    ?line {error,{pattern_mismatch,"AY23BC",RE2}} =
	(xmerl_xsd_type:facet_fun(string,{pattern,RE2}))
	  ("AY23BC").

enumeration(suite) -> [];
enumeration(_Config) ->
    ?line {ok,"tomat"} = 
	(xmerl_xsd_type:facet_fun(string,{enumeration,["gurka","tomat","sallad"]}))("tomat"),
    ?line {error,{enumeration,"morot",should_be_one_of,["gurka","tomat","sallad"]}} =
	(xmerl_xsd_type:facet_fun(string,{enumeration,["gurka","tomat","sallad"]}))("morot"),
    ok.

whiteSpace(suite) -> [];
whiteSpace(_Config) ->
    ?line {ok,"gur ka"} = (xmerl_xsd_type:facet_fun(string,{whiteSpace,"collapse"}))("  gur\tka "),
    ?line {ok," gur ka "} = (xmerl_xsd_type:facet_fun(string,{whiteSpace,"replace"}))(" gur\nka\t"),
    ?line {ok," gurk\na\t"} = (xmerl_xsd_type:facet_fun(string,{whiteSpace,"preserve"}))(" gurk\na\t"),
    ok.

maxInclusive(suite) -> [];
maxInclusive(_Config) ->
    ?line {error,{maxInclusive,"3",should_be_less_than_or_equal_with,"2"}} =
	(xmerl_xsd_type:facet_fun(integer,{maxInclusive,"2"}))("3"),

    ?line {error,{maxInclusive,"3",should_be_less_than_or_equal_with,"2"}} =
	(xmerl_xsd_type:facet_fun(decimal,{maxInclusive,"2"}))("3"),
    ?line {error,{maxInclusive,_,should_be_less_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(decimal,{maxInclusive,"2.234"}))("2.235"),
    ?line {error,{maxInclusive,_,should_be_less_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(decimal,{maxInclusive,"-2.222"}))("-2.221"),

    ?line {error,{maxInclusive,_,should_be_less_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(double,{maxInclusive,"2.333"}))("INF"),
    ?line {error,{maxInclusive,_,should_be_less_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(double,{maxInclusive,"1E3"}))("1001"),

    ?line {error,{maxInclusive,_,should_be_less_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(float,{maxInclusive,"-0.1"}))("-0"),
    ?line {error,{maxInclusive,_,should_be_less_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(float,{maxInclusive,"0"}))("0.01"),

    ?line {ok,"3"} = (xmerl_xsd_type:facet_fun(integer,{maxInclusive,"3"}))("3"),

    ?line {ok,"+100000.00"} = 
	(xmerl_xsd_type:facet_fun(decimal,{maxInclusive,"1E6"}))("+100000.00"),
    ?line {ok,"12678967.543222"} = 
	(xmerl_xsd_type:facet_fun(decimal,{maxInclusive,"12678967.543233"}))("12678967.543222"),

    ?line {ok,"3.2E-11"} = 
	(xmerl_xsd_type:facet_fun(double,{maxInclusive,"2E-10"}))("3.2E-11"),
    ?line {ok,"10E20"} = 
	(xmerl_xsd_type:facet_fun(double,{maxInclusive,"INF"}))("10E20"),
    ?line {ok,"0.127"} = 
	(xmerl_xsd_type:facet_fun(double,{maxInclusive,"12.78e-2"}))("0.127"),

    ?line {ok,"1267.43233E12"} = (xmerl_xsd_type:facet_fun(float,{maxInclusive,"1267.43233E12"}))("1267.43233E12"),
    ?line {ok,"34E-26"} = (xmerl_xsd_type:facet_fun(float,{maxInclusive,"33E-25"}))("34E-26"),

    ?line {ok,"2007-10-26T12:00:00+03:00"} =
	(xmerl_xsd_type:facet_fun(dateTime,{maxInclusive,"2007-10-26T12:00:00+03:00"}))("2007-10-26T12:00:00+03:00"),
    ?line {ok,"2007-10-26T11:00:00+03:00"} =
	(xmerl_xsd_type:facet_fun(dateTime,{maxInclusive,"2007-10-26T12:00:00+03:00"}))("2007-10-26T11:00:00+03:00"),
    ?line {error,{maxInclusive,_,should_be_less_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(dateTime,{maxInclusive,"2007-10-26T12:00:00+03:00"}))("2007-10-26T12:00:00"),

    ?line {ok,"P1Y2M3DT10H30M"} =
	(xmerl_xsd_type:facet_fun(duration,{maxInclusive,"P1Y2M4D"}))("P1Y2M3DT10H30M"),
    ?line {error,{maxInclusive,_,should_be_less_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(duration,{maxInclusive,"P1Y2M3DT10H"}))("P1Y2M3DT10H30M"),
    ok.
	
maxExclusive(suite) -> [];
maxExclusive(_Config) ->
    ?line {error,{maxExclusive,"2",not_less_than,"2"}} =
	(xmerl_xsd_type:facet_fun(integer,{maxExclusive,"2"}))("2"),
    ?line {error,{maxExclusive,"-19999",not_less_than,"-20000"}} =
	(xmerl_xsd_type:facet_fun(integer,{maxExclusive,"-20000"}))("-19999"),

    ?line {error,{maxExclusive,"3.0000",not_less_than,"2.9999"}} =
	(xmerl_xsd_type:facet_fun(decimal,{maxExclusive,"2.9999"}))("3.0000"),
    ?line {error,{maxExclusive,_,not_less_than,_}} =
	(xmerl_xsd_type:facet_fun(decimal,{maxExclusive,"2.234"}))("2.234"),
    ?line {error,{maxExclusive,_,not_less_than,_}} =
	(xmerl_xsd_type:facet_fun(decimal,{maxExclusive,"-2.22222222"}))("-2.22222222"),

    ?line {error,{maxExclusive,_,not_less_than,_}} =
	(xmerl_xsd_type:facet_fun(double,{maxExclusive,"2.333E23"}))("INF"),
    ?line {error,{maxExclusive,_,not_less_than,_}} =
	(xmerl_xsd_type:facet_fun(double,{maxExclusive,"1E3"}))("1000"),
    ?line {error,{maxExclusive,_,not_less_than,_}} =
	(xmerl_xsd_type:facet_fun(double,{maxExclusive,"1E-13"}))("0.999E-12"),

    ?line {error,{maxExclusive,_,not_less_than,_}} =
	(xmerl_xsd_type:facet_fun(float,{maxExclusive,"-0.1"}))("-0.01E1"),
    ?line {error,{maxExclusive,_,not_less_than,_}} =
	(xmerl_xsd_type:facet_fun(float,{maxExclusive,"-1E-1"}))("-0"),

    ?line {ok,"-4"} = (xmerl_xsd_type:facet_fun(integer,{maxExclusive,"3"}))("-4"),

    ?line {ok,"+100000.00"} = 
	(xmerl_xsd_type:facet_fun(decimal,{maxExclusive,"1E6"}))("+100000.00"),
    %% must support 18 digits
    ?line {ok,"12678967.5432323456"} = 
	(xmerl_xsd_type:facet_fun(decimal,{maxExclusive,"12678967.5432323457"}))("12678967.5432323456"),

    ?line {ok,"3.2E-11"} = 
	(xmerl_xsd_type:facet_fun(double,{maxExclusive,"2E-10"}))("3.2E-11"),
    ?line {ok,"10E20"} = 
	(xmerl_xsd_type:facet_fun(double,{maxExclusive,"INF"}))("10E20"),
    ?line {ok,"0.127"} = 
	(xmerl_xsd_type:facet_fun(double,{maxExclusive,"12.78e-2"}))("0.127"),

    ?line {ok,"1267.43233E11"} = (xmerl_xsd_type:facet_fun(float,{maxExclusive,"1267.43233E12"}))("1267.43233E11"),
    ?line {ok,"34E-26"} = (xmerl_xsd_type:facet_fun(float,{maxExclusive,"33E-25"}))("34E-26"), 
    
    ?line {ok,"P1Y2M3DT10H30M"}  = (xmerl_xsd_type:facet_fun(duration,{maxExclusive,"P1Y2M4D"}))("P1Y2M3DT10H30M"),

    ?line {ok,"2006-09-06T19:17:45Z"}  = (xmerl_xsd_type:facet_fun(dateTime,{maxExclusive,"2006-09-06T19:17:46Z"}))("2006-09-06T19:17:45Z"),
    ok.

minExclusive(suite) -> [];
minExclusive(_Config) ->
    ?line {error,{minExclusive,"2",not_greater_than,"2"}} =
	(xmerl_xsd_type:facet_fun(integer,{minExclusive,"2"}))("2"),
    ?line {error,{minExclusive,"-20001",not_greater_than,"-20000"}} =
	(xmerl_xsd_type:facet_fun(integer,{minExclusive,"-20000"}))("-20001"),
    
    ?line {error,{minExclusive,"2.9999",not_greater_than,"2.9999"}} =
	(xmerl_xsd_type:facet_fun(decimal,{minExclusive,"2.9999"}))("2.9999"),
    ?line {error,{minExclusive,_,not_greater_than,_}} =
	(xmerl_xsd_type:facet_fun(decimal,{minExclusive,"-123456789.123456788"}))("-123456789.123456789"),
    ?line {error,{minExclusive,_,not_greater_than,_}} =
	(xmerl_xsd_type:facet_fun(decimal,{minExclusive,"-2.222222000"}))("-2.22222222"),

    ?line {error,{minExclusive,_,not_greater_than,_}} =
	(xmerl_xsd_type:facet_fun(double,{minExclusive,"INF"}))("2.333E23"),
    ?line {error,{minExclusive,_,not_greater_than,_}} =
	(xmerl_xsd_type:facet_fun(double,{minExclusive,"1E3"}))("1000"),
    ?line {error,{minExclusive,_,not_greater_than,_}} =
	(xmerl_xsd_type:facet_fun(double,{minExclusive,"1E-13"}))("0.999E-14"),

    ?line {error,{minExclusive,_,not_greater_than,_}} =
	(xmerl_xsd_type:facet_fun(float,{minExclusive,"-0.1"}))("-0.01E1"),
    ?line {error,{minExclusive,_,not_greater_than,_}} =
	(xmerl_xsd_type:facet_fun(float,{minExclusive,"-0"}))("-1E-1"),

    ?line {ok,"4"} = (xmerl_xsd_type:facet_fun(integer,{minExclusive,"-3"}))("4"),

    ?line {ok,"+1000001.00"} = 
	(xmerl_xsd_type:facet_fun(decimal,{minExclusive,"1E6"}))("+1000001.00"),
    %% must support 18 digits
    ?line {ok,"12678967.5432323456"} = 
	(xmerl_xsd_type:facet_fun(decimal,{minExclusive,"12678967.54323234555"}))("12678967.5432323456"),

    ?line {ok,"3.2E-11"} = 
	(xmerl_xsd_type:facet_fun(double,{minExclusive,"2E-12"}))("3.2E-11"),
    ?line {ok,"10E20"} = 
	(xmerl_xsd_type:facet_fun(double,{minExclusive,"-INF"}))("10E20"),
    ?line {ok,"0.1279"} = 
	(xmerl_xsd_type:facet_fun(double,{minExclusive,"12.78e-2"}))("0.1279"),

    ?line {ok,"126743.233E11"} = (xmerl_xsd_type:facet_fun(float,{minExclusive,"1267.43233E12"}))("126743.233E11"),
    ?line {ok,"34E-26"} = (xmerl_xsd_type:facet_fun(float,{minExclusive,"33E-27"}))("34E-26"),

    ?line {ok,"P1Y2M3DT10H30M"}  = (xmerl_xsd_type:facet_fun(duration,{minExclusive,"P1Y2M3D"}))("P1Y2M3DT10H30M"),

    ?line {ok,"2006-09-06T19:17:45Z"}  = (xmerl_xsd_type:facet_fun(dateTime,{minExclusive,"2006-09-06T19:17:44Z"}))("2006-09-06T19:17:45Z"),
    ok.

minInclusive(suite) -> [];
minInclusive(_Config) ->
    ?line {error,{minInclusive,"1",not_greater_than_or_equal_with,"2"}} =
	(xmerl_xsd_type:facet_fun(integer,{minInclusive,"2"}))("1"),
    ?line {error,{minInclusive,"-20001",not_greater_than_or_equal_with,
	    "-20000"}} =
	(xmerl_xsd_type:facet_fun(integer,{minInclusive,"-20000"}))("-20001"),
    
    ?line {error,{minInclusive,"2.9999",not_greater_than_or_equal_with,
	    "2.99999"}} =
	(xmerl_xsd_type:facet_fun(decimal,{minInclusive,"2.99999"}))("2.9999"),
    ?line {error,{minInclusive,_,not_greater_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(decimal,{minInclusive,"-123456789.123456788"}))("-123456789.123456789"),
    ?line {error,{minInclusive,_,not_greater_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(decimal,{minInclusive,"-2.222222000"}))("-2.22222222"),

    ?line {error,{minInclusive,_,not_greater_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(double,{minInclusive,"2.333E23"}))("-INF"),
    ?line {error,{minInclusive,_,not_greater_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(double,{minInclusive,"1E3"}))("100"),
    ?line {error,{minInclusive,_,not_greater_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(double,{minInclusive,"1E-13"}))("0.999E-14"),

    ?line {error,{minInclusive,_,not_greater_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(float,{minInclusive,"-0.1"}))("-0.1E1"),
    ?line {error,{minInclusive,_,not_greater_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(float,{minInclusive,"-0"}))("-1E-1"),
    ?line {error,_}=(xmerl_xsd_type:facet_fun(float,{minInclusive,"10E-10"}))("10E-11"),

    ?line {ok,"4"} = (xmerl_xsd_type:facet_fun(integer,{minInclusive,"-3"}))("4"),

    ?line {ok,"+1000000.00"} = 
	(xmerl_xsd_type:facet_fun(decimal,{minInclusive,"1E6"}))("+1000000.00"),
    %% must support 18 digits
    ?line {ok,"12678967.5432323456"} = 
	(xmerl_xsd_type:facet_fun(decimal,{minInclusive,"12678967.54323234555"}))("12678967.5432323456"),

    ?line {ok,"3.2E-11"} = 
	(xmerl_xsd_type:facet_fun(double,{minInclusive,"2E-12"}))("3.2E-11"),
    ?line {ok,"10E20"} = 
	(xmerl_xsd_type:facet_fun(double,{minInclusive,"-INF"}))("10E20"),
    ?line {ok,"0.1279"} = 
	(xmerl_xsd_type:facet_fun(double,{minInclusive,"12.78e-2"}))("0.1279"),

    ?line {ok,"126743.233E11"} = (xmerl_xsd_type:facet_fun(float,{minInclusive,"1267.43233E12"}))("126743.233E11"),
    ?line {ok,"34E-26"} = (xmerl_xsd_type:facet_fun(float,{minInclusive,"33E-27"}))("34E-26"),
    ?line {ok,"34E-26"} = (xmerl_xsd_type:facet_fun(float,{minInclusive,"340E-27"}))("34E-26"),

    ?line {ok,"P1Y2M3DT10H30M"}  = (xmerl_xsd_type:facet_fun(duration,{minInclusive,"P1Y2M3D"}))("P1Y2M3DT10H30M"),

    ?line {ok,"2006-09-06T19:17:45Z"}  = (xmerl_xsd_type:facet_fun(dateTime,{minInclusive,"2006-09-06T19:17:45Z"}))("2006-09-06T19:17:45Z"),
    ok.

totalDigits(suite) -> [];
totalDigits(_Config) ->
    ?line {error,{totalDigits,4,to_many_digits}} =
	(xmerl_xsd_type:facet_fun(integer,{totalDigits,"3"}))("3456"),
    ?line {error,{totalDigits,4,to_many_digits}} =
	(xmerl_xsd_type:facet_fun(decimal,{totalDigits,"3"}))("00345.600"),
    
    ?line {ok,"555"} =
	(xmerl_xsd_type:facet_fun(integer,{totalDigits,"3"}))("555"),
    ?line {ok,"555"} =
	(xmerl_xsd_type:facet_fun(integer,{totalDigits,"7"}))("555"),
    ?line {ok,"555.555"} =
	(xmerl_xsd_type:facet_fun(decimal,{totalDigits,"7"}))("555.555"),
    ?line {ok,"555.555000000"} =
	(xmerl_xsd_type:facet_fun(decimal,{totalDigits,"7"}))("555.555000000"),
    ok.
    
fractionDigits(suite) -> [];
fractionDigits(_Config) ->
    ?line {error,{fractionDigits,3,to_many_digits_in,"555.555000000"}} =
	(xmerl_xsd_type:facet_fun(decimal,{fractionDigits,"2"}))("555.555000000"),
    ?line {error,{fractionDigits,6,to_many_digits_in,"555.555001"}} =
	(xmerl_xsd_type:facet_fun(decimal,{fractionDigits,"5"}))("555.555001"),

    ?line {ok,"555.55500"}  =
	(xmerl_xsd_type:facet_fun(decimal,{fractionDigits,"5"}))("555.55500"),
    ?line {ok,"555"}  =
	(xmerl_xsd_type:facet_fun(decimal,{fractionDigits,"5"}))("555"),
    ?line {ok,"555.000"}  =
	(xmerl_xsd_type:facet_fun(decimal,{fractionDigits,"0"}))("555.000"),

    ?line {ok,"555"}  =
	(xmerl_xsd_type:facet_fun(integer,{fractionDigits,"0"}))("555"),
    ok.

%% some block testing of dateTime and duration comparisons
compare_dateTime(suite) -> [];
compare_dateTime(_Config) ->
    %% comparison results according to table in section 3.2.7.4 of XML
    %% Schema part 2
    ?line lt = xmerl_xsd_type:compare_dateTime("2000-01-15T00:00:00",
					       "2000-02-15T00:00:00"),
    ?line gt = xmerl_xsd_type:compare_dateTime("2000-02-15T00:00:00",
					       "2000-01-15T00:00:00"),
    
    ?line lt = xmerl_xsd_type:compare_dateTime("2000-01-15T12:00:00",
					       "2000-01-16T12:00:00Z"),
    ?line gt = xmerl_xsd_type:compare_dateTime("2000-01-16T12:00:00Z",
					       "2000-01-15T12:00:00"),
    
    ?line indefinite = xmerl_xsd_type:compare_dateTime("2000-01-01T12:00:00",
						       "1999-12-31T23:00:00Z"),
    ?line indefinite = xmerl_xsd_type:compare_dateTime("1999-12-31T23:00:00Z",
						       "2000-01-01T12:00:00"),

    ?line indefinite = xmerl_xsd_type:compare_dateTime("2000-01-16T12:00:00",
						       "2000-01-16T12:00:00Z"),
    ?line indefinite = xmerl_xsd_type:compare_dateTime("2000-01-16T12:00:00Z",
						       "2000-01-16T12:00:00"),

    ?line indefinite = xmerl_xsd_type:compare_dateTime("2000-01-16T00:00:00",
						       "2000-01-16T12:00:00Z"),
    ?line indefinite = xmerl_xsd_type:compare_dateTime("2000-01-16T12:00:00Z",
						       "2000-01-16T00:00:00"),

    %% example in appendix E.1 in XML Schema part 2.
    ?line {2001,4,17,19,23,17.3000,{pos,0,0}} = 
	xmerl_xsd_type:add_duration2dateTime("2000-01-12T12:13:14Z",
					     "P1Y3M5DT7H10M3.3S").

compare_duration(suite) -> [];
compare_duration(_Config) ->
    %% order relations according to section 3.2.6.2 in XML Schema
    %% part2.
    ?line gt = xmerl_xsd_type:compare_durations("P1Y","P364D"),
    ?line indefinite = xmerl_xsd_type:compare_durations("P1Y","P365D"),
    ?line indefinite = xmerl_xsd_type:compare_durations("P1Y","P366D"),
    ?line lt = xmerl_xsd_type:compare_durations("P1Y","P367D"),

    ?line gt = xmerl_xsd_type:compare_durations("P1M","P27D"),
    ?line indefinite = xmerl_xsd_type:compare_durations("P1M","P28D"),
    ?line indefinite = xmerl_xsd_type:compare_durations("P1M","P29D"),
    ?line indefinite = xmerl_xsd_type:compare_durations("P1M","P30D"),
    ?line indefinite = xmerl_xsd_type:compare_durations("P1M","P31D"),
    ?line lt = xmerl_xsd_type:compare_durations("P1M","P32D"),

    ?line gt = xmerl_xsd_type:compare_durations("P5M","P149D"),
    ?line indefinite = xmerl_xsd_type:compare_durations("P5M","P150D"),
    ?line indefinite = xmerl_xsd_type:compare_durations("P5M","P151D"),
    ?line indefinite = xmerl_xsd_type:compare_durations("P5M","P152D"),
    ?line indefinite = xmerl_xsd_type:compare_durations("P5M","P153D"),
    ?line lt = xmerl_xsd_type:compare_durations("P5M","P154D").


po(suite) -> [];
po(Config) ->
    ?line {E,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),
					   "po.xml"]),[]),
    ?line {E,_} = xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
					      "po.xsd"]),E,[]).

po1(suite) -> [];
po1(Config) ->
    ?line {E,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),
					   "po1.xml"]),[]),
    ?line {E2,_} = xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
					       "po1.xsd"]),E,[]),
    ?line ok = xmerl_test_lib:cmp_element(E,E2).

po2(suite) -> [];
po2(Config) ->
    ?line {E,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),
					   "po2.xml"]),[]),
    ?line {E2,_} = xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
					       "po1.xsd"]),E,[]),
    ?line ok = xmerl_test_lib:cmp_element(E,E2).

ipo(suite) -> [];
ipo(Config) ->
    ?line {E,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),
					   "ipo.xml"]),[]),
    ?line {VE,_} = xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
						"ipo.xsd"]),E,[]),
    ?line ok = xmerl_test_lib:cmp_element(E,VE).

ipo_redefine(suite) -> [];
ipo_redefine(Config) ->
    ?line {E,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),
					   "ipo_redefine.xml"]),[]),
    ?line {VE,_} = xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
						"ipo_redefine.xsd"]),E,[]),
    ?line ok = xmerl_test_lib:cmp_element(E,VE).

'4Q99'(suite) -> [];
'4Q99'(Config) ->
    ?line {E,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),
					   "4Q99.xml"]),[]),
    %% the import in report.xsd lacks schemaLocation, so the imported
    %% namespace definitions have to be loaded separately.
    ?line {ok,S} = 
	xmerl_xsd:process_schema(filename:join([?config(data_dir,Config),
					     "ipo.xsd"])),
    ?line {VE,_} = xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
						"report.xsd"]),E,[{state,S}]),
    ?line ok = xmerl_test_lib:cmp_element(E,VE),
    
    %% report2.xsd has an import element with a schemaLocation attribute
    ?line {VE,_} = xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
						"report2.xsd"]),E,[]).
    
small(suite) -> [];
small(Config) ->
    ?line {E=#xmlElement{},_} = 
	xmerl_scan:file(filename:join([?config(data_dir,Config),
				       "small.xml"]),[]),
    ?line {VE=#xmlElement{},_} = 
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
					  "small.xsd"]),E,[]),
    ?line #xmlElement{attributes=Atts,content=C} = VE,
    ?line C = E#xmlElement.content,
    %% The attribute orderStatus with default value was absent in small.xml

    %% Test of validation "on the fly" when parsing XML.
    ?line {VE,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),
						  "small.xml"]),
				   [{validation,schema},
				    {schemaLocation,[{"small",filename:join(?config(data_dir,Config),"small.xsd")}]}]),
    ?line {VE,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),
						  "small.xml"]),
				   [{validation,schema}]),
    ?line true = lists:keymember(orderStatus,#xmlAttribute.name,Atts).

complexType1(suite) -> [];
complexType1(Config) ->
    ?line {E1=#xmlElement{},_} = 
	xmerl_scan:file(filename:join([?config(data_dir,Config),
				       "complexTypes1.xml"]),[]),
    ?line {VE1=#xmlElement{},_} = 
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
					  "complexTypes.xsd"]),E1,[]),
    ?line ok = xmerl_test_lib:cmp_element(E1,VE1),

    ?line {E2=#xmlElement{},_} = 
	xmerl_scan:file(filename:join([?config(data_dir,Config),
				       "complexTypes2.xml"]),[]),
    ?line {VE2=#xmlElement{},_} = 
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
					  "complexTypes.xsd"]),E2,[]),
    ?line ok = xmerl_test_lib:cmp_element(E2,VE2).

model_group_all(suite) -> [];
model_group_all(Config) ->
    ?line {E=#xmlElement{},_} = 
	xmerl_scan:file(filename:join([?config(data_dir,Config),
				       "po1.xml"]),[]),
    ?line {E,_} = 
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
					  "po1_all.xsd"]),E,[]),

    ?line {E1=#xmlElement{},_} = 
	xmerl_scan:file(filename:join([?config(data_dir,Config),
				       "po1_all1.xml"]),[]),
    ?line {E1,_} = 
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
					  "po1_all.xsd"]),E1,[]),

    ?line {E2=#xmlElement{},_} = 
	xmerl_scan:file(filename:join([?config(data_dir,Config),
				       "po1_all2.xml"]),[]),
    ?line {E2,_} = 
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
				       "po1_all.xsd"]),E2,[]),

    ?line {E3=#xmlElement{},_} = 
	xmerl_scan:file(filename:join([?config(data_dir,Config),
				       "po1_all_err1.xml"]),[]),
    ?line {error,_Reason1} =
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
					  "po1_all.xsd"]),E3,[]),

    
    ?line {E4=#xmlElement{},_} = 
	xmerl_scan:file(filename:join([?config(data_dir,Config),
				       "po1_all_err2.xml"]),[]),
    ?line {error,_Reason2} =
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
					  "po1_all.xsd"]),E4,[]).

substitutionGroup(suite) -> [];
substitutionGroup(Config) ->
    ?line {E,_} =
	xmerl_scan:file(filename:join([?config(data_dir,Config),
				       "ipo_substGroup.xml"]),[]),
    ?line {E,_} =
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
					  "ipo_substGroup.xsd"]),E,[]).
attributeGroup(suite) -> [];
attributeGroup(Config) ->
    ?line {E,_} =
	xmerl_scan:file(filename:join([?config(data_dir,Config),
				       "po_attrGroup.xml"]),[]),
    ?line {E,_} =
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
					  "po_attrGroup.xsd"]),E,[]).
test_key1(suite) -> [];
test_key1(Config) ->
    ?line {E,_} =
	xmerl_scan:file(filename:join([?config(data_dir,Config),
				       "vehicle2.xml"]),[]),
    ?line {E,_} =
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
					  "vehicle.xsd"]),E,[]),

    ?line {E2,_} =
	xmerl_scan:file(filename:join([?config(data_dir,Config),
				       "vehicle.xml"]),[]),
    ?line {error,L2} =
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
					  "vehicle.xsd"]),E2,[]),
    ?line 10 = erlang:length(L2),

    ?line {E3 = #xmlElement{},_} =
	xmerl_scan:file(filename:join([?config(data_dir,Config),
				       "vehicle3.xml"]),[]),
    ?line {E3 = #xmlElement{},_} =
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
					  "vehicle.xsd"]),E3,[]).

sis1(suite) -> []; 
sis1(Config) ->
    ?line {E,_} =
	xmerl_scan:file(filename:join([?config(data_dir,Config),sis,
				       "instance.xml"]),[]),
    ?line {#xmlElement{},_} = 
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),sis,
					  "IntegratedSite.xsd"]),E,[]).
sis2(suite) -> [];
sis2(Config) ->
    ?line {BS_E,_} =
	xmerl_scan:file(filename:join([?config(data_dir,Config),sis,
				       "bs_mim.xml"]),[]),
    ?line {SW_E,_} =
	xmerl_scan:file(filename:join([?config(data_dir,Config),sis,
				       "swm_mim.xml"]),[]),
    ?line {HW_E,_} =
	xmerl_scan:file(filename:join([?config(data_dir,Config),sis,
				       "hwm_mim.xml"]),[]),

    ?line {#xmlElement{},_} = 
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),sis,
						  "mim.xsd"]),BS_E,[]),
    ?line {#xmlElement{},_} = 
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),sis,
						  "mim.xsd"]),SW_E,[]),
    ?line {#xmlElement{},_} = 
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),sis,
						  "mim.xsd"]),HW_E,[]).

state2file_file2state(suite) -> [];
state2file_file2state(Config) ->
    ?line {E,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),
						 "po.xml"]),[]),
    ?line {ok,S} = xmerl_xsd:process_schema(filename:join([?config(data_dir,Config),"po.xsd"])),
    ?line {E,_} = xmerl_xsd:validate(E,S),
    ?line ok = xmerl_xsd:state2file(S),
    ?line {ok,S} = xmerl_xsd:file2state(filename:join([?config(data_dir,Config),"po.xss"])),
    ?line {E,_} = xmerl_xsd:validate(E,S),
    
    ?line ok = xmerl_xsd:state2file(S,filename:join([?config(data_dir,Config),"po_state"])),
    ?line {ok,S} = xmerl_xsd:file2state(filename:join([?config(data_dir,Config),"po_state.xss"])),

    ?line {E,_} = xmerl_xsd:validate(E,S).


union(suite) -> [];
union(Config) ->
    ?line {E,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),
						 "instance.xml"])),

    ?line {_E2 = #xmlElement{},_} = xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),"measCollec.xsd"]),E).
    

ticket_6910(suite) -> [];
ticket_6910(Config) ->
    ?line {E,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),
						 sis,"dummy_action_mim.xml"])),
    ?line {_E2 = #xmlElement{},_} =
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
						  sis,"mim2.xsd"]),E).
ticket_7165(suite) -> [];
ticket_7165(Config) ->
    %% The validation option seems not to work
    ?line {E,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),
						   "ticket_7288.xml"]), 
				    [{validation, schema}]),
    %% The option xsdbase gave {error, enoent}.
    ?line {ok,_} = xmerl_xsd:process_schema("CxDataType_Rel5.xsd", [{xsdbase, ?config(data_dir,Config)}]).



ticket_7190(suite) -> [];
ticket_7190(Config) ->
    ?line {E,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),
						 "int.xml"])),
    ?line {_E2 = #xmlElement{},_} =
	xmerl_xsd:process_validate(filename:join([?config(data_dir,Config),
						  "simple_int.xsd"]),E).
ticket_7288(suite) -> [];
ticket_7288(Config) ->
    %% The schema table in the state where deleted by xmerl_xsd:validate if there was an error.
    ?line {E,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),"ticket_7288.xml"])),
    
    ?line {ok,S} = xmerl_xsd:process_schema(filename:join([?config(data_dir,Config),"CxDataType_Rel5.xsd"])),
    
    ?line {error, EL} = xmerl_xsd:validate(E, S),
    
    ?line {error, EL} = xmerl_xsd:validate(E, S).
 
ticket_7736(suite) -> [];
ticket_7736(Config) ->
    DataDir = ?config(data_dir,Config),
    ?line {ok, State } = 
	xmerl_xsd:process_schema(filename:join([DataDir,"enum_bug.xsd"])),
    
    ?line {Entity ,_} = 
	xmerl_scan:file(filename:join([DataDir,"enum_bug.xml"])),
    
    ?line {#xmlElement{},_} = xmerl_xsd:validate(Entity, State).

ticket_8599(suite) -> [];
ticket_8599(Config) ->
    ?line {E,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),"ticket_8599.xml"])),
    
    ?line {ok, S} = xmerl_xsd:process_schema(filename:join([?config(data_dir,Config),"ticket_8599.xsd"])),
    
    ?line {{xmlElement,persons,persons,_,_,_,_,_,_,_,_,_},_GlobalState} = xmerl_xsd:validate(E, S).


ticket_9410(suite) -> [];
ticket_9410(Config) ->
    file:set_cwd(filename:join([?config(data_dir,Config),".."])),
    ?line {ok, _S} = xmerl_xsd:process_schema("xmerl_xsd_SUITE_data/small.xsd").
