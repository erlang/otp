%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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

-include_lib("common_test/include/ct.hrl").
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
      [{group, xmlXsdAndExample},
       {group, xmlSchemaPrimerExamples},
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
     {xmlXsdAndExample, [],
      [xml_xsd, xml_lang_attr]},
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

suite() ->
    [{timetrap,{minutes,10}}].

init_per_testcase(_TestCase,Config) ->
    {ok,_} = file:read_file_info(filename:join([privdir(Config)])),
    code:add_patha(privdir(Config)),
    Config.

end_per_testcase(_Func,_Config) ->
    ok.


string(_Config) ->
    %% #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
    Str = [16#9,16#A,16#D,16#20,16#D7FF,16#E000,16#FFFD,16#10000, 16#10FFFF],
    {ok,_} = check_simpleType(string,Str,dummy).
    
boolean(_Config) ->
    {ok,_} = check_simpleType(boolean,"1",dummy),
    {ok,_} = check_simpleType(boolean,"0",dummy),
    {ok,_} = check_simpleType(boolean,"true",dummy),
    {ok,_} = check_simpleType(boolean,"false",dummy),
    {error,_Reason} = check_simpleType(boolean,"gurka",dummy).

decimal(_Config) ->
    {ok,_} = check_simpleType(decimal,"-1.23",dummy),
    {ok,_} = check_simpleType(decimal,"12678967.543233",dummy),
    {ok,_} = check_simpleType(decimal,"+100000.00",dummy),
    {ok,_} = check_simpleType(decimal,"210",dummy).

float(_Config) ->
    %% -1E4, 1267.43233E12, 12.78e-2, 12 , -0, 0 , INF, -INF, NaN
    {ok,_} = check_simpleType(float,"-1E4",dummy),
    {ok,_} = check_simpleType(float,"1267.43233E12",dummy),
    {ok,_} = check_simpleType(float,"12.78e-2",dummy),
    {ok,_} = check_simpleType(float,"12",dummy),
    {ok,_} = check_simpleType(float,"-0",dummy),
    {ok,_} = check_simpleType(float,"0",dummy),
    {ok,_} = check_simpleType(float,"INF",dummy),
    {ok,_} = check_simpleType(float,"-INF",dummy),
    {ok,_} = check_simpleType(float,"NaN",dummy).


double(_Config) ->
    %% -1E4, 1267.43233E12, 12.78e-2, 12 , -0, 0 , INF, -INF, NaN
    {ok,_} = check_simpleType(double,"-1E4",dummy),
    {ok,_} = check_simpleType(double,"1267.43233E12",dummy),
    {ok,_} = check_simpleType(double,"12.78e-2",dummy),
    {ok,_} = check_simpleType(double,"12",dummy),
    {ok,_} = check_simpleType(double,"-0",dummy),
    {ok,_} = check_simpleType(double,"0",dummy),
    {ok,_} = check_simpleType(double,"INF",dummy),
    {ok,_} = check_simpleType(double,"-INF",dummy),
    {ok,_} = check_simpleType(double,"NaN",dummy).


duration(_Config) ->
    %% allowed: P1Y2M3DT10H30M -P120D P1347Y P1347M P1Y2MT2H 
    %% P0Y1347M P0Y1347M0D -P1347M 
    %% not allowed: P-1347M P1Y2MT
    {ok,_} = check_simpleType(duration,"P1Y2M3DT10H30M",dummy),
    {ok,_} = check_simpleType(duration,"-P120D",dummy),
    {ok,_} = check_simpleType(duration,"P1347Y",dummy),
    {ok,_} = check_simpleType(duration,"P1347M",dummy),
    {ok,_} = check_simpleType(duration,"P1Y2MT2H",dummy),
    {ok,_} = check_simpleType(duration,"P0Y1347M",dummy),
    {ok,_} = check_simpleType(duration,"P0Y1347M0D",dummy),
    {ok,_} = check_simpleType(duration,"-P1347M",dummy),

    {error,_} = check_simpleType(duration,"P-1347M",dummy),
    {error,_} = check_simpleType(duration,"P1Y2MT",dummy).

%% '-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
dateTime(_Config) ->
    %% 2002-10-10T12:00:00-05:00
    DT1 = "2002-10-10T12:00:00-05:00",
    {ok,_} = check_simpleType(dateTime,DT1,dummy),
    DT2 = "2002-10-10T17:00:00Z",
    {ok,_} = check_simpleType(dateTime,DT2,dummy),
    %% plus sign prohibited
    DT3 = "+2002-10-10T17:00:00Z",
    {error,_Reason3} = check_simpleType(dateTime,DT3,dummy),
    %% leading zeros when year are more than four digits prohibited
    DT4 = "002002-10-10T17:00:00Z",
    {error,_Reason4} = check_simpleType(dateTime,DT4,dummy),
    DT5 = "1953-12-31T12:10:10.10+12:00",
    {ok,_} = check_simpleType(dateTime,DT5,dummy).

time(_Config) ->
    %% hh:mm:ss.sss with optional following time zone indicator.
    T1 = "13:20:00-05:00",
    {ok,_} = check_simpleType(time,T1,dummy),
    %% canonical repr. of midnight
    T2 = "00:00:00",
    {ok,_} = check_simpleType(time,T2,dummy),
    T3 = "12:34:56",
    {ok,_} = check_simpleType(time,T3,dummy),
    T4 = "12:34:56.552",
    {ok,_} = check_simpleType(time,T4,dummy),
    T5 = "12:34:56.552Z",
    {ok,_} = check_simpleType(time,T5,dummy).

date(_Config) ->
    %% '-'? yyyy '-' mm '-' dd zzzzzz?
    %%  is 
    D1 = "2002-10-10+13:00",
    {ok,_} = check_simpleType(date,D1,dummy),
    D2 = "2002-10-09-11:00",
    {ok,_} = check_simpleType(date,D2,dummy),

    D12 = "+2002-13-09-11:00",
    {error,_Reason12} = check_simpleType(date,D12,dummy),
    D13 = "2002-13-09-11:00",
    {error,_Reason13} = check_simpleType(date,D13,dummy),
    D14 = "2002-12-39-11:00",
    {error,_Reason14} = check_simpleType(date,D14,dummy).

gYearMonth(_Config) -> 
    %% '-'? yyyy '-' mm zzzzzz?
    GYM1 = "1955-10",
    {ok,_} = check_simpleType(gYearMonth,GYM1,dummy),
    GYM2 = "-1955-10",
    {ok,_} = check_simpleType(gYearMonth,GYM2,dummy),
    GYM3 = "1955-10Z",
    {ok,_} = check_simpleType(gYearMonth,GYM3,dummy),
    GYM4 = "0055-10+10:00",
    {ok,_} = check_simpleType(gYearMonth,GYM4,dummy),
    GYM5 = "0955-10Z",
    {ok,_} = check_simpleType(gYearMonth,GYM5,dummy),
    GYM6 = "-11955-01",
    {ok,_} = check_simpleType(gYearMonth,GYM6,dummy),

    {error,_} = check_simpleType(gYearMonth,"+2000-10",dummy),
    {error,_} = check_simpleType(gYearMonth,"2000-00",dummy),
    {error,_} = check_simpleType(gYearMonth,"2000-10+10:70",dummy).

gYear(_Config) ->
    %% '-'? yyyy zzzzzz?
    {ok,_} = check_simpleType(gYear,"2000",dummy),
    {ok,_} = check_simpleType(gYear,"2000-11:30",dummy),
    {ok,_} = check_simpleType(gYear,"-2000",dummy),
    {error,_} = check_simpleType(gYear,"0000",dummy).

gMonthDay(_Config) ->
    %% mm '-' dd zzzzzz?
    {ok,_} = check_simpleType(gMonthDay,"--05-03",dummy),
    {ok,_} = check_simpleType(gMonthDay,"--05-03Z",dummy),
    {error,_} = check_simpleType(gMonthDay,"05-00",dummy),
    {error,_} = check_simpleType(gMonthDay,"00-03",dummy),
    {error,_} = check_simpleType(gMonthDay,"-05-03",dummy).

gDay(_Config) ->
    %% dd zzzzzz?
    {ok,_} = check_simpleType(gDay,"---05",dummy),
    {ok,_} = check_simpleType(gDay,"---30+03:00",dummy),
    {error,_} = check_simpleType(gDay,"-30+03:00",dummy),
    {error,_} = check_simpleType(gDay,"---00+03:00",dummy),
    {error,_} = check_simpleType(gDay,"---40+03:00",dummy),
    {error,_} = check_simpleType(gDay,"05",dummy).

gMonth(_Config) ->
    %% mm zzzzzz?
    {ok,_} = check_simpleType(gMonth,"--05",dummy),
    {ok,_} = check_simpleType(gMonth,"--10+03:00",dummy),
    {error,_} = check_simpleType(gMonth,"-10+03:00",dummy),
    {error,_} = check_simpleType(gMonth,"00+03:00",dummy),
    {error,_} = check_simpleType(gMonth,"14",dummy),
    {error,_} = check_simpleType(gMonth,"05",dummy).


hexBinary(_Config) ->
    %% an even number of hexadecimal digits ([0-9a-fA-F]).
    {ok,_} = check_simpleType(hexBinary,"05",dummy),
    {ok,_} = check_simpleType(hexBinary,"aF",dummy),
    {ok,_} = check_simpleType(hexBinary,
					     "0123456789abcdefABCDEF",dummy),
    {error,_} = check_simpleType(hexBinary,
					     "0123456789absdefABCDEF",dummy),
    {error,_} = check_simpleType(hexBinary,"aF5",dummy),
    {error,_} = check_simpleType(hexBinary,"aFG",dummy).

base64Binary(_Config) ->
    %% a-z, A-Z, 0-9, the plus sign (+), the forward slash (/) and the
    %% equal sign (=), together with the characters defined in [XML
    %% 1.0 (Second Edition)] as white space.(16#9, 16#A, 16#D, 16#20)
    {ok,_} = check_simpleType(base64Binary,"05+/AA==",dummy),
    {ok,_} = check_simpleType(base64Binary,"05+/AA= =",dummy),
    {ok,_} = check_simpleType(base64Binary,"05+/A A= =",dummy),
    {ok,_} = check_simpleType(base64Binary,"05+/ AA= =",dummy),
    {error,_} = check_simpleType(base64Binary,"05+/AA== ",dummy),
    B64B1 = "AbCd GhZz 09w=",
    {ok,_} = check_simpleType(base64Binary,B64B1,dummy),
    B64B2 = "AbCd GhZ9 0z8 =",
    {ok,_} = check_simpleType(base64Binary,B64B2,dummy),
    {ok,_} = check_simpleType(base64Binary,"0z8 =",dummy),
    ErrB641 = "AbCd GZ9 0z8 =",
    {error,_} = check_simpleType(base64Binary,ErrB641,dummy).

anyURI(_Config) ->
    URI1 = "ftp://ftp.is.co.za/rfc/rfc1808.txt",
    URI2 = "gopher://spinaltap.micro.umn.edu/00/Weather/California/Los%20Angeles",
    URI3 = "http://www.math.uio.no/faq/compression-faq/part1.html",
    URI4 = "mailto:mduerst@ifi.unizh.ch",
    URI5 = "news:comp.infosystems.www.servers.unix",
    URI6 = "telnet://melvyl.ucop.edu/",
    ok=ok_loop(anyURI,[URI1,URI2,URI3,URI4,URI5,URI6]).


'QName'(_Config) ->
    %%  QName 	   ::= 	(Prefix ':')? LocalPart
    %% 	Prefix 	   ::= 	NCName
    %% 	LocalPart  ::= 	NCName
    {ok,_} = check_simpleType('QName',"abc:def",dummy),
    {ok,_} = check_simpleType('QName',"abc",dummy),
    {ok,_} = check_simpleType('QName',"abc:def:ijk",dummy).

'NOTATION'(_Config) ->
    {ok,_} = check_simpleType('NOTATION',"abc:def",dummy),
    {ok,_} = check_simpleType('NOTATION',"abc",dummy),
    {ok,_} = check_simpleType('NOTATION',"abc:def:ijk",dummy).

normalizedString(_Config) ->
    %% not contain the carriage return (#xD), line feed (#xA) nor tab
    %% (#x9) characters.
    NStr1 = "this string is ok with extra space     between characters",
    NotNStr1 = "this string is not normalized \t",
    NotNStr2 = "neither is this \n string",
    NotNStr3 = "or this \r string",
    {ok,_}  = check_simpleType(normalizedString,NStr1,dummy),
    ok=error_loop(normalizedString,[NotNStr1,NotNStr2,NotNStr3]).

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
    {ok,_}  = check_simpleType(token,T1,dummy),
    ok=error_loop(token,[NotT1,NotT2,NotT3,NotT4,NotT5,NotT6]).

language(_Config) ->
    %% strings that conform to the pattern
    %% [a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*
    L = "Abra-cadabra-123",
    NotL1 = "Abra123-cadabra!",
    NotL2 = "Abra-",
    NotL3 = "Abracadabra",
    NotL4 = "Abra-cadabrrra",
    {ok,_}  = check_simpleType(language,L,dummy),
    ok=error_loop(language,[NotL1,NotL2,NotL3,NotL4]).

'NMTOKEN'(_Config) ->
    N = "name:withoutspace",
    NotN1 = "name with space",
    NotN2 = "namewith#strang/chars)",
    {ok,_}  = check_simpleType('NMTOKEN',N,dummy),
    {error,_} = check_simpleType('NMTOKEN',NotN1,dummy),
    {error,_} = check_simpleType('NMTOKEN',NotN2,dummy).

'NMTOKENS'(_Config) ->
    N1 = "name1 name:2 name:three",
    NotN1 = "name na%me",
    {ok,_}  = check_simpleType('NMTOKENS',N1,dummy),
    {error,_} = check_simpleType('NMTOKENS',NotN1,dummy).

'Name'(_Config) ->
    {ok,_}  = check_simpleType('Name',"_valid_Name",dummy).

'NCName'(_Config) ->
    {ok,_}  = check_simpleType('NCName',"_valid_Name",dummy).

'ID'(_Config) ->
    {ok,_}  = check_simpleType('ID',"_valid_Name",dummy).

'IDREF'(_Config) ->
    {ok,_}  = check_simpleType('IDREF',"_valid_Name",dummy).

'IDREFS'(_Config) ->
    {ok,_}  = check_simpleType('IDREFS',"_valid_Name Name2",dummy).

'ENTITY'(_Config) ->
    {ok,_}  = check_simpleType('ENTITY',"_valid_Name",dummy).

'ENTITIES'(_Config) ->
    {ok,_}  = check_simpleType('ENTITIES',"name name3",dummy).

integer(_Config) ->
    IntList = ["-1", "0", "12678967543233", "+100000"],
    ok = ok_loop(integer,IntList),
    {error,_} = check_simpleType(integer,"1.3",dummy).

nonPositiveInteger(_Config) ->
    NPIList = ["0", "-12678967543233", "-100000"],
    ok = ok_loop(nonPositiveInteger,NPIList),
    {error,_} = check_simpleType(nonPositiveInteger,"1",dummy).

negativeInteger(_Config) ->
    NIList = ["-1", "-12678967543233", "-100000"],
    ok = ok_loop(negativeInteger,NIList),
    {error,_} = check_simpleType(negativeInteger,"1",dummy),
    {error,_} = check_simpleType(negativeInteger,"0",dummy).

long(_Config) ->
    L = ["9223372036854775807","-9223372036854775808","-1", "0",
	 "12678967543233", "+100000"],
    ok = ok_loop(long,L),
    Err = ["9223372036854775808","-9223372036854775809"],
    ok = error_loop(long,Err).

int(_Config) ->
    L = ["2147483647", "-2147483648", "-1", "0", "126789675", "+100000"],
    ok = ok_loop(int,L),
    Err = ["2147483648", "-2147483649"],
    ok = error_loop(int,Err).

short(_Config) ->
    L = ["32767", "-32768", "-1", "0", "12678", "+10000"],
    ok = ok_loop(short,L),
    Err = ["32768", "-32769"],
    ok = error_loop(short,Err).

byte(_Config) ->
    L = ["-1", "0", "126", "+100", "127", "-128"],
    ok = ok_loop(byte,L),
    Err = ["128", "-129"],
    ok = error_loop(byte,Err).

nonNegativeInteger(_Config) ->
    L = ["1", "0", "12678967543233", "+100000"],
    ok = ok_loop(nonNegativeInteger,L),
    {error,_} = check_simpleType(nonNegativeInteger,"-1",dummy).

unsignedLong(_Config) ->
    L = ["0", "12678967543233", "100000", "18446744073709551615"],
    ok = ok_loop(unsignedLong,L),
    Err = ["-1","18446744073709551616"],
    ok = error_loop(unsignedLong,Err).

unsignedInt(_Config) ->
    L = ["4294967295", "0", "1267896754", "100000"],
    ok = ok_loop(unsignedInt,L),
    Err = ["-1","4294967296"],
    ok = error_loop(unsignedInt,Err).

unsignedShort(_Config) ->
    L = ["65535", "0", "12678", "10000"],
    ok = ok_loop(unsignedShort,L),
    Err = ["-1","65536"],
    ok = error_loop(unsignedShort,Err).

unsignedByte(_Config) ->
    L = ["255", "0", "126", "100"],
    ok = ok_loop(unsignedByte,L),
    Err = ["-1","256"],
    ok = error_loop(unsignedByte,Err).

positiveInteger(_Config) ->
    L = ["1", "12678967543233", "+100000"],
    ok = ok_loop(positiveInteger,L),
    Err = ["-1","0"],
    ok = error_loop(positiveInteger,Err).
    
ok_loop(_Type,[]) ->
    ok;
ok_loop(Type,[H|T]) ->
    {ok,_} = check_simpleType(Type,H,dummy),
    ok_loop(Type,T).

error_loop(_T,[]) ->
    ok;
error_loop(Type,[H|T]) ->
    {error,_} = check_simpleType(Type,H,dummy),
    error_loop(Type,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Testing facets
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



length(_Config) ->
    {ok,"string"} = (xmerl_xsd_type:facet_fun(string,{length,"6"}))("string"),
    {error,{length,12,should_be,6}} =
	(xmerl_xsd_type:facet_fun(string,{length,"6"}))("stringstring"),
    ok.
    
minLength(_Config) ->
    {ok,"string"} = (xmerl_xsd_type:facet_fun(string,{minLength,"6"}))("string"),
    {error,{minLength,3,should_at_least_be,6}} =
	(xmerl_xsd_type:facet_fun(string,{minLength,"6"}))("str"),
    ok.

maxLength(_Config) ->
    {ok,"string"} = (xmerl_xsd_type:facet_fun(string,{maxLength,"6"}))("string"),
    {error,{maxLength,12,should_not_be_more_than,6}} =
	(xmerl_xsd_type:facet_fun(string,{maxLength,"6"}))("stringstring"),
    ok.

pattern(_Config) ->
    RE1 = "[a-z]{5}",
    {ok,"calle"} =
	(xmerl_xsd_type:facet_fun(string,{pattern,RE1}))
	  ("calle"),
    {error,{pattern_mismatch,"cal",RE1}} =
	(xmerl_xsd_type:facet_fun(string,{pattern,RE1}))
	  ("cal"),
    RE2 = "[A-Z]{2}\\d\\s\\d[A-Z]{2}",
    {ok,"AY2 3BC"} =
	(xmerl_xsd_type:facet_fun(string,{pattern,RE2}))
	  ("AY2 3BC"),
    {error,{pattern_mismatch,"AY23BC",RE2}} =
	(xmerl_xsd_type:facet_fun(string,{pattern,RE2}))
	  ("AY23BC").

enumeration(_Config) ->
    {ok,"tomat"} =
	(xmerl_xsd_type:facet_fun(string,{enumeration,["gurka","tomat","sallad"]}))("tomat"),
    {error,{enumeration,"morot",should_be_one_of,["gurka","tomat","sallad"]}} =
	(xmerl_xsd_type:facet_fun(string,{enumeration,["gurka","tomat","sallad"]}))("morot"),
    ok.

whiteSpace(_Config) ->
    {ok,"gur ka"} = (xmerl_xsd_type:facet_fun(string,{whiteSpace,"collapse"}))("  gur\tka "),
    {ok," gur ka "} = (xmerl_xsd_type:facet_fun(string,{whiteSpace,"replace"}))(" gur\nka\t"),
    {ok," gurk\na\t"} = (xmerl_xsd_type:facet_fun(string,{whiteSpace,"preserve"}))(" gurk\na\t"),
    ok.

maxInclusive(_Config) ->
    {error,{maxInclusive,"3",should_be_less_than_or_equal_with,"2"}} =
	(xmerl_xsd_type:facet_fun(integer,{maxInclusive,"2"}))("3"),

    {error,{maxInclusive,"3",should_be_less_than_or_equal_with,"2"}} =
	(xmerl_xsd_type:facet_fun(decimal,{maxInclusive,"2"}))("3"),
    {error,{maxInclusive,_,should_be_less_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(decimal,{maxInclusive,"2.234"}))("2.235"),
    {error,{maxInclusive,_,should_be_less_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(decimal,{maxInclusive,"-2.222"}))("-2.221"),

    {error,{maxInclusive,_,should_be_less_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(double,{maxInclusive,"2.333"}))("INF"),
    {error,{maxInclusive,_,should_be_less_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(double,{maxInclusive,"1E3"}))("1001"),

    {error,{maxInclusive,_,should_be_less_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(float,{maxInclusive,"-0.1"}))("-0"),
    {error,{maxInclusive,_,should_be_less_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(float,{maxInclusive,"0"}))("0.01"),

    {ok,"3"} = (xmerl_xsd_type:facet_fun(integer,{maxInclusive,"3"}))("3"),

    {ok,"+100000.00"} =
	(xmerl_xsd_type:facet_fun(decimal,{maxInclusive,"1E6"}))("+100000.00"),
    {ok,"12678967.543222"} =
	(xmerl_xsd_type:facet_fun(decimal,{maxInclusive,"12678967.543233"}))("12678967.543222"),

    {ok,"3.2E-11"} =
	(xmerl_xsd_type:facet_fun(double,{maxInclusive,"2E-10"}))("3.2E-11"),
    {ok,"10E20"} =
	(xmerl_xsd_type:facet_fun(double,{maxInclusive,"INF"}))("10E20"),
    {ok,"0.127"} =
	(xmerl_xsd_type:facet_fun(double,{maxInclusive,"12.78e-2"}))("0.127"),

    {ok,"1267.43233E12"} = (xmerl_xsd_type:facet_fun(float,{maxInclusive,"1267.43233E12"}))("1267.43233E12"),
    {ok,"34E-26"} = (xmerl_xsd_type:facet_fun(float,{maxInclusive,"33E-25"}))("34E-26"),

    {ok,"2007-10-26T12:00:00+03:00"} =
	(xmerl_xsd_type:facet_fun(dateTime,{maxInclusive,"2007-10-26T12:00:00+03:00"}))("2007-10-26T12:00:00+03:00"),
    {ok,"2007-10-26T11:00:00+03:00"} =
	(xmerl_xsd_type:facet_fun(dateTime,{maxInclusive,"2007-10-26T12:00:00+03:00"}))("2007-10-26T11:00:00+03:00"),
    {error,{maxInclusive,_,should_be_less_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(dateTime,{maxInclusive,"2007-10-26T12:00:00+03:00"}))("2007-10-26T12:00:00"),

    {ok,"P1Y2M3DT10H30M"} =
	(xmerl_xsd_type:facet_fun(duration,{maxInclusive,"P1Y2M4D"}))("P1Y2M3DT10H30M"),
    {error,{maxInclusive,_,should_be_less_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(duration,{maxInclusive,"P1Y2M3DT10H"}))("P1Y2M3DT10H30M"),
    ok.
	
maxExclusive(_Config) ->
    {error,{maxExclusive,"2",not_less_than,"2"}} =
	(xmerl_xsd_type:facet_fun(integer,{maxExclusive,"2"}))("2"),
    {error,{maxExclusive,"-19999",not_less_than,"-20000"}} =
	(xmerl_xsd_type:facet_fun(integer,{maxExclusive,"-20000"}))("-19999"),

    {error,{maxExclusive,"3.0000",not_less_than,"2.9999"}} =
	(xmerl_xsd_type:facet_fun(decimal,{maxExclusive,"2.9999"}))("3.0000"),
    {error,{maxExclusive,_,not_less_than,_}} =
	(xmerl_xsd_type:facet_fun(decimal,{maxExclusive,"2.234"}))("2.234"),
    {error,{maxExclusive,_,not_less_than,_}} =
	(xmerl_xsd_type:facet_fun(decimal,{maxExclusive,"-2.22222222"}))("-2.22222222"),

    {error,{maxExclusive,_,not_less_than,_}} =
	(xmerl_xsd_type:facet_fun(double,{maxExclusive,"2.333E23"}))("INF"),
    {error,{maxExclusive,_,not_less_than,_}} =
	(xmerl_xsd_type:facet_fun(double,{maxExclusive,"1E3"}))("1000"),
    {error,{maxExclusive,_,not_less_than,_}} =
	(xmerl_xsd_type:facet_fun(double,{maxExclusive,"1E-13"}))("0.999E-12"),

    {error,{maxExclusive,_,not_less_than,_}} =
	(xmerl_xsd_type:facet_fun(float,{maxExclusive,"-0.1"}))("-0.01E1"),
    {error,{maxExclusive,_,not_less_than,_}} =
	(xmerl_xsd_type:facet_fun(float,{maxExclusive,"-1E-1"}))("-0"),

    {ok,"-4"} = (xmerl_xsd_type:facet_fun(integer,{maxExclusive,"3"}))("-4"),

    {ok,"+100000.00"} =
	(xmerl_xsd_type:facet_fun(decimal,{maxExclusive,"1E6"}))("+100000.00"),
    %% must support 18 digits
    {ok,"12678967.5432323456"} =
	(xmerl_xsd_type:facet_fun(decimal,{maxExclusive,"12678967.5432323457"}))("12678967.5432323456"),

    {ok,"3.2E-11"} =
	(xmerl_xsd_type:facet_fun(double,{maxExclusive,"2E-10"}))("3.2E-11"),
    {ok,"10E20"} =
	(xmerl_xsd_type:facet_fun(double,{maxExclusive,"INF"}))("10E20"),
    {ok,"0.127"} =
	(xmerl_xsd_type:facet_fun(double,{maxExclusive,"12.78e-2"}))("0.127"),

    {ok,"1267.43233E11"} = (xmerl_xsd_type:facet_fun(float,{maxExclusive,"1267.43233E12"}))("1267.43233E11"),
    {ok,"34E-26"} = (xmerl_xsd_type:facet_fun(float,{maxExclusive,"33E-25"}))("34E-26"),
    
    {ok,"P1Y2M3DT10H30M"} = (xmerl_xsd_type:facet_fun(duration,{maxExclusive,"P1Y2M4D"}))("P1Y2M3DT10H30M"),

    {ok,"2006-09-06T19:17:45Z"} = (xmerl_xsd_type:facet_fun(dateTime,{maxExclusive,"2006-09-06T19:17:46Z"}))("2006-09-06T19:17:45Z"),
    ok.

minExclusive(_Config) ->
    {error,{minExclusive,"2",not_greater_than,"2"}} =
	(xmerl_xsd_type:facet_fun(integer,{minExclusive,"2"}))("2"),
    {error,{minExclusive,"-20001",not_greater_than,"-20000"}} =
	(xmerl_xsd_type:facet_fun(integer,{minExclusive,"-20000"}))("-20001"),
    
    {error,{minExclusive,"2.9999",not_greater_than,"2.9999"}} =
	(xmerl_xsd_type:facet_fun(decimal,{minExclusive,"2.9999"}))("2.9999"),
    {error,{minExclusive,_,not_greater_than,_}} =
	(xmerl_xsd_type:facet_fun(decimal,{minExclusive,"-123456789.123456788"}))("-123456789.123456789"),
    {error,{minExclusive,_,not_greater_than,_}} =
	(xmerl_xsd_type:facet_fun(decimal,{minExclusive,"-2.222222000"}))("-2.22222222"),

    {error,{minExclusive,_,not_greater_than,_}} =
	(xmerl_xsd_type:facet_fun(double,{minExclusive,"INF"}))("2.333E23"),
    {error,{minExclusive,_,not_greater_than,_}} =
	(xmerl_xsd_type:facet_fun(double,{minExclusive,"1E3"}))("1000"),
    {error,{minExclusive,_,not_greater_than,_}} =
	(xmerl_xsd_type:facet_fun(double,{minExclusive,"1E-13"}))("0.999E-14"),

    {error,{minExclusive,_,not_greater_than,_}} =
	(xmerl_xsd_type:facet_fun(float,{minExclusive,"-0.1"}))("-0.01E1"),
    {error,{minExclusive,_,not_greater_than,_}} =
	(xmerl_xsd_type:facet_fun(float,{minExclusive,"-0"}))("-1E-1"),

    {ok,"4"} = (xmerl_xsd_type:facet_fun(integer,{minExclusive,"-3"}))("4"),

    {ok,"+1000001.00"} =
	(xmerl_xsd_type:facet_fun(decimal,{minExclusive,"1E6"}))("+1000001.00"),
    %% must support 18 digits
    {ok,"12678967.5432323456"} =
	(xmerl_xsd_type:facet_fun(decimal,{minExclusive,"12678967.54323234555"}))("12678967.5432323456"),

    {ok,"3.2E-11"} =
	(xmerl_xsd_type:facet_fun(double,{minExclusive,"2E-12"}))("3.2E-11"),
    {ok,"10E20"} =
	(xmerl_xsd_type:facet_fun(double,{minExclusive,"-INF"}))("10E20"),
    {ok,"0.1279"} =
	(xmerl_xsd_type:facet_fun(double,{minExclusive,"12.78e-2"}))("0.1279"),

    {ok,"126743.233E11"} = (xmerl_xsd_type:facet_fun(float,{minExclusive,"1267.43233E12"}))("126743.233E11"),
    {ok,"34E-26"} = (xmerl_xsd_type:facet_fun(float,{minExclusive,"33E-27"}))("34E-26"),

    {ok,"P1Y2M3DT10H30M"} = (xmerl_xsd_type:facet_fun(duration,{minExclusive,"P1Y2M3D"}))("P1Y2M3DT10H30M"),

    {ok,"2006-09-06T19:17:45Z"} = (xmerl_xsd_type:facet_fun(dateTime,{minExclusive,"2006-09-06T19:17:44Z"}))("2006-09-06T19:17:45Z"),
    ok.

minInclusive(_Config) ->
    {error,{minInclusive,"1",not_greater_than_or_equal_with,"2"}} =
	(xmerl_xsd_type:facet_fun(integer,{minInclusive,"2"}))("1"),
    {error,{minInclusive,"-20001",not_greater_than_or_equal_with,
	    "-20000"}} =
	(xmerl_xsd_type:facet_fun(integer,{minInclusive,"-20000"}))("-20001"),
    
    {error,{minInclusive,"2.9999",not_greater_than_or_equal_with,
	    "2.99999"}} =
	(xmerl_xsd_type:facet_fun(decimal,{minInclusive,"2.99999"}))("2.9999"),
    {error,{minInclusive,_,not_greater_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(decimal,{minInclusive,"-123456789.123456788"}))("-123456789.123456789"),
    {error,{minInclusive,_,not_greater_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(decimal,{minInclusive,"-2.222222000"}))("-2.22222222"),

    {error,{minInclusive,_,not_greater_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(double,{minInclusive,"2.333E23"}))("-INF"),
    {error,{minInclusive,_,not_greater_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(double,{minInclusive,"1E3"}))("100"),
    {error,{minInclusive,_,not_greater_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(double,{minInclusive,"1E-13"}))("0.999E-14"),

    {error,{minInclusive,_,not_greater_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(float,{minInclusive,"-0.1"}))("-0.1E1"),
    {error,{minInclusive,_,not_greater_than_or_equal_with,_}} =
	(xmerl_xsd_type:facet_fun(float,{minInclusive,"-0"}))("-1E-1"),
    {error,_}=(xmerl_xsd_type:facet_fun(float,{minInclusive,"10E-10"}))("10E-11"),

    {ok,"4"} = (xmerl_xsd_type:facet_fun(integer,{minInclusive,"-3"}))("4"),

    {ok,"+1000000.00"} = (xmerl_xsd_type:facet_fun(decimal,{minInclusive,"1E6"}))("+1000000.00"),
    %% must support 18 digits
    {ok,"12678967.5432323456"} =
	(xmerl_xsd_type:facet_fun(decimal,{minInclusive,"12678967.54323234555"}))("12678967.5432323456"),

    {ok,"3.2E-11"} = (xmerl_xsd_type:facet_fun(double,{minInclusive,"2E-12"}))("3.2E-11"),
    {ok,"10E20"} = (xmerl_xsd_type:facet_fun(double,{minInclusive,"-INF"}))("10E20"),
    {ok,"0.1279"} = (xmerl_xsd_type:facet_fun(double,{minInclusive,"12.78e-2"}))("0.1279"),

    {ok,"126743.233E11"} = (xmerl_xsd_type:facet_fun(float,{minInclusive,"1267.43233E12"}))("126743.233E11"),
    {ok,"34E-26"} = (xmerl_xsd_type:facet_fun(float,{minInclusive,"33E-27"}))("34E-26"),
    {ok,"34E-26"} = (xmerl_xsd_type:facet_fun(float,{minInclusive,"340E-27"}))("34E-26"),

    {ok,"P1Y2M3DT10H30M"} = (xmerl_xsd_type:facet_fun(duration,{minInclusive,"P1Y2M3D"}))("P1Y2M3DT10H30M"),

    {ok,"2006-09-06T19:17:45Z"} = (xmerl_xsd_type:facet_fun(dateTime,{minInclusive,"2006-09-06T19:17:45Z"}))("2006-09-06T19:17:45Z"),
    ok.

totalDigits(_Config) ->
    {error,{totalDigits,4,to_many_digits}} =
	(xmerl_xsd_type:facet_fun(integer,{totalDigits,"3"}))("3456"),
    {error,{totalDigits,4,to_many_digits}} =
	(xmerl_xsd_type:facet_fun(decimal,{totalDigits,"3"}))("00345.600"),
    
    {ok,"555"} = (xmerl_xsd_type:facet_fun(integer,{totalDigits,"3"}))("555"),
    {ok,"555"} = (xmerl_xsd_type:facet_fun(integer,{totalDigits,"7"}))("555"),
    {ok,"555.555"} = (xmerl_xsd_type:facet_fun(decimal,{totalDigits,"7"}))("555.555"),
    {ok,"555.555000000"} = (xmerl_xsd_type:facet_fun(decimal,{totalDigits,"7"}))("555.555000000"),
    ok.
    
fractionDigits(_Config) ->
    {error,{fractionDigits,3,to_many_digits_in,"555.555000000"}} =
	(xmerl_xsd_type:facet_fun(decimal,{fractionDigits,"2"}))("555.555000000"),
    {error,{fractionDigits,6,to_many_digits_in,"555.555001"}} =
	(xmerl_xsd_type:facet_fun(decimal,{fractionDigits,"5"}))("555.555001"),

    {ok,"555.55500"} = (xmerl_xsd_type:facet_fun(decimal,{fractionDigits,"5"}))("555.55500"),
    {ok,"555"} = (xmerl_xsd_type:facet_fun(decimal,{fractionDigits,"5"}))("555"),
    {ok,"555.000"} = (xmerl_xsd_type:facet_fun(decimal,{fractionDigits,"0"}))("555.000"),

    {ok,"555"} = (xmerl_xsd_type:facet_fun(integer,{fractionDigits,"0"}))("555"),
    ok.

%% some block testing of dateTime and duration comparisons
compare_dateTime(_Config) ->
    %% comparison results according to table in section 3.2.7.4 of XML
    %% Schema part 2
    lt = xmerl_xsd_type:compare_dateTime("2000-01-15T00:00:00",
                                         "2000-02-15T00:00:00"),

    gt = xmerl_xsd_type:compare_dateTime("2000-02-15T00:00:00",
                                         "2000-01-15T00:00:00"),

    lt = xmerl_xsd_type:compare_dateTime("2000-01-15T12:00:00",
                                         "2000-01-16T12:00:00Z"),

    gt = xmerl_xsd_type:compare_dateTime("2000-01-16T12:00:00Z",
                                         "2000-01-15T12:00:00"),
    
    indefinite = xmerl_xsd_type:compare_dateTime("2000-01-01T12:00:00",
                                                 "1999-12-31T23:00:00Z"),

    indefinite = xmerl_xsd_type:compare_dateTime("1999-12-31T23:00:00Z",
                                                 "2000-01-01T12:00:00"),

    indefinite = xmerl_xsd_type:compare_dateTime("2000-01-16T12:00:00",
                                                 "2000-01-16T12:00:00Z"),

    indefinite = xmerl_xsd_type:compare_dateTime("2000-01-16T12:00:00Z",
                                                 "2000-01-16T12:00:00"),

    indefinite = xmerl_xsd_type:compare_dateTime("2000-01-16T00:00:00",
                                                 "2000-01-16T12:00:00Z"),
    indefinite = xmerl_xsd_type:compare_dateTime("2000-01-16T12:00:00Z",
                                                 "2000-01-16T00:00:00"),

    %% example in appendix E.1 in XML Schema part 2.
    {2001,4,17,19,23,17.3000,{pos,0,0}} =
	xmerl_xsd_type:add_duration2dateTime("2000-01-12T12:13:14Z",
					     "P1Y3M5DT7H10M3.3S").

compare_duration(_Config) ->
    %% order relations according to section 3.2.6.2 in XML Schema
    %% part2.
    gt = xmerl_xsd_type:compare_durations("P1Y","P364D"),
    indefinite = xmerl_xsd_type:compare_durations("P1Y","P365D"),
    indefinite = xmerl_xsd_type:compare_durations("P1Y","P366D"),
    lt = xmerl_xsd_type:compare_durations("P1Y","P367D"),

    gt = xmerl_xsd_type:compare_durations("P1M","P27D"),
    indefinite = xmerl_xsd_type:compare_durations("P1M","P28D"),
    indefinite = xmerl_xsd_type:compare_durations("P1M","P29D"),
    indefinite = xmerl_xsd_type:compare_durations("P1M","P30D"),
    indefinite = xmerl_xsd_type:compare_durations("P1M","P31D"),
    lt = xmerl_xsd_type:compare_durations("P1M","P32D"),

    gt = xmerl_xsd_type:compare_durations("P5M","P149D"),
    indefinite = xmerl_xsd_type:compare_durations("P5M","P150D"),
    indefinite = xmerl_xsd_type:compare_durations("P5M","P151D"),
    indefinite = xmerl_xsd_type:compare_durations("P5M","P152D"),
    indefinite = xmerl_xsd_type:compare_durations("P5M","P153D"),
    lt = xmerl_xsd_type:compare_durations("P5M","P154D").

xml_xsd(Config) ->
    DataDir = datadir( Config),
    Options = [{fetch_path, [DataDir]}],
    {ok, _} = xmerl_xsd:process_schema("xml.xsd", Options).

xml_lang_attr(Config) ->
    DataDir = datadir( Config),
    {Element, _} = xmerl_scan:file(filename:join([DataDir,"book.xml"])),
    Options = [{fetch_path, [DataDir]}],
    {ok, Schema} = xmerl_xsd:process_schema("book.xsd", Options),
    {Element, _} = xmerl_xsd:validate(Element, Schema).

po(Config) ->
    {E,_} = xmerl_scan:file(datadir_join(Config,["po.xml"]),[]),
    {E,_} = xmerl_xsd:process_validate(datadir_join(Config,["po.xsd"]),E,[]).

po1(Config) ->
    {E,_} = xmerl_scan:file(datadir_join(Config,["po1.xml"]),[]),
    {E2,_} = xmerl_xsd:process_validate(datadir_join(Config,["po1.xsd"]),E,[]),
    ok = xmerl_test_lib:cmp_element(E,E2).

po2(Config) ->
    {E,_} = xmerl_scan:file(datadir_join(Config,["po2.xml"]),[]),
    {E2,_} = xmerl_xsd:process_validate(datadir_join(Config,["po1.xsd"]),E,[]),
    ok = xmerl_test_lib:cmp_element(E,E2).

ipo(Config) ->
    {E,_} = xmerl_scan:file(datadir_join(Config,["ipo.xml"]),[]),
    {VE,_} = xmerl_xsd:process_validate(datadir_join(Config,["ipo.xsd"]),E,[]),
    ok = xmerl_test_lib:cmp_element(E,VE).

ipo_redefine(Config) ->
    {E,_} = xmerl_scan:file(datadir_join(Config,["ipo_redefine.xml"]),[]),
    {VE,_} = xmerl_xsd:process_validate(datadir_join(Config,["ipo_redefine.xsd"]),E,[]),
    ok = xmerl_test_lib:cmp_element(E,VE).

'4Q99'(Config) ->
    {E,_} = xmerl_scan:file(datadir_join(Config,["4Q99.xml"]),[]),
    %% the import in report.xsd lacks schemaLocation, so the imported
    %% namespace definitions have to be loaded separately.
    {ok,S} = xmerl_xsd:process_schema(datadir_join(Config,["ipo.xsd"])),
    {VE,_} = xmerl_xsd:process_validate(datadir_join(Config,["report.xsd"]),E,[{state,S}]),
    ok = xmerl_test_lib:cmp_element(E,VE),
    
    %% report2.xsd has an import element with a schemaLocation attribute
    {VE,_} = xmerl_xsd:process_validate(datadir_join(Config,["report2.xsd"]),E,[]).
    
small(Config) ->
    {E=#xmlElement{},_} = xmerl_scan:file(datadir_join(Config,["small.xml"]),[]),
    {VE=#xmlElement{},_} = xmerl_xsd:process_validate(datadir_join(Config,["small.xsd"]),E,[]),
    #xmlElement{attributes=Atts,content=C} = VE,
    C = E#xmlElement.content,
    %% The attribute orderStatus with default value was absent in small.xml

    %% Test of validation "on the fly" when parsing XML.
    {VE,_} = xmerl_scan:file(datadir_join(Config,["small.xml"]),
                             [{validation,schema},
                              {schemaLocation,[{"small",filename:join(datadir(Config),"small.xsd")}]}]),
    {VE,_} = xmerl_scan:file(datadir_join(Config,["small.xml"]),
                             [{validation,schema}]),
    true = lists:keymember(orderStatus,#xmlAttribute.name,Atts).

complexType1(Config) ->
    {E1=#xmlElement{},_} = xmerl_scan:file(datadir_join(Config,["complexTypes1.xml"]),[]),
    {VE1=#xmlElement{},_} = xmerl_xsd:process_validate(datadir_join(Config,[ "complexTypes.xsd"]),E1,[]),
    ok = xmerl_test_lib:cmp_element(E1,VE1),

    {E2=#xmlElement{},_} = xmerl_scan:file(datadir_join(Config,["complexTypes2.xml"]),[]),
    {VE2=#xmlElement{},_} = xmerl_xsd:process_validate(datadir_join(Config,["complexTypes.xsd"]),E2,[]),
    ok = xmerl_test_lib:cmp_element(E2,VE2).

model_group_all(Config) ->
    {E=#xmlElement{},_} = xmerl_scan:file(datadir_join(Config,["po1.xml"]),[]),
    {E,_} = xmerl_xsd:process_validate(datadir_join(Config,["po1_all.xsd"]),E,[]),

    {E1=#xmlElement{},_} = xmerl_scan:file(datadir_join(Config,["po1_all1.xml"]),[]),
    {E1,_} = xmerl_xsd:process_validate(datadir_join(Config,["po1_all.xsd"]),E1,[]),

    {E2=#xmlElement{},_} = xmerl_scan:file(datadir_join(Config,["po1_all2.xml"]),[]),
    {E2,_} = xmerl_xsd:process_validate(datadir_join(Config,["po1_all.xsd"]),E2,[]),

    {E3=#xmlElement{},_} = xmerl_scan:file(datadir_join(Config,["po1_all_err1.xml"]),[]),
    {error,_Reason1} = xmerl_xsd:process_validate(datadir_join(Config,["po1_all.xsd"]),E3,[]),

    
    {E4=#xmlElement{},_} = xmerl_scan:file(datadir_join(Config,["po1_all_err2.xml"]),[]),
    {error,_Reason2} = xmerl_xsd:process_validate(datadir_join(Config,["po1_all.xsd"]),E4,[]).

substitutionGroup(Config) ->
    {E,_} = xmerl_scan:file(datadir_join(Config,["ipo_substGroup.xml"]),[]),
    {E,_} = xmerl_xsd:process_validate(datadir_join(Config,["ipo_substGroup.xsd"]),E,[]).

attributeGroup(Config) ->
    {E,_} = xmerl_scan:file(datadir_join(Config,["po_attrGroup.xml"]),[]),
    {E,_} = xmerl_xsd:process_validate(datadir_join(Config,["po_attrGroup.xsd"]),E,[]).

test_key1(Config) ->
    {E,_} = xmerl_scan:file(datadir_join(Config,["vehicle2.xml"]),[]),
    {E,_} = xmerl_xsd:process_validate(datadir_join(Config,["vehicle.xsd"]),E,[]),

    {E2,_} = xmerl_scan:file(datadir_join(Config,["vehicle.xml"]),[]),
    {error,L2} = xmerl_xsd:process_validate(datadir_join(Config,["vehicle.xsd"]),E2,[]),
    10 = erlang:length(L2),

    {E3 = #xmlElement{},_} = xmerl_scan:file(datadir_join(Config,["vehicle3.xml"]),[]),
    {E3 = #xmlElement{},_} = xmerl_xsd:process_validate(datadir_join(Config,["vehicle.xsd"]),E3,[]).

sis1(Config) ->
    {E,_} = xmerl_scan:file(datadir_join(Config,[sis,"instance.xml"]),[]),
    {#xmlElement{},_} = xmerl_xsd:process_validate(datadir_join(Config,[sis,"IntegratedSite.xsd"]),E,[]).

sis2(Config) ->
    {BS_E,_} = xmerl_scan:file(datadir_join(Config,[sis,"bs_mim.xml"]),[]),
    {SW_E,_} = xmerl_scan:file(datadir_join(Config,[sis,"swm_mim.xml"]),[]),
    {HW_E,_} = xmerl_scan:file(datadir_join(Config,[sis,"hwm_mim.xml"]),[]),

    {#xmlElement{},_} = xmerl_xsd:process_validate(datadir_join(Config,[sis,"mim.xsd"]),BS_E,[]),
    {#xmlElement{},_} = xmerl_xsd:process_validate(datadir_join(Config,[sis,"mim.xsd"]),SW_E,[]),
    {#xmlElement{},_} = xmerl_xsd:process_validate(datadir_join(Config,[sis,"mim.xsd"]),HW_E,[]).

state2file_file2state(Config) ->
    {E,_} = xmerl_scan:file(datadir_join(Config,[ "po.xml"]),[]),
    {ok,S} = xmerl_xsd:process_schema(datadir_join(Config,["po.xsd"])),
    {E,_} = xmerl_xsd:validate(E,S),
    ok = xmerl_xsd:state2file(S),
    {ok,S} = xmerl_xsd:file2state(datadir_join(Config,["po.xss"])),
    {E,_} = xmerl_xsd:validate(E,S),
    
    ok = xmerl_xsd:state2file(S,datadir_join(Config,["po_state"])),
    {ok,S} = xmerl_xsd:file2state(datadir_join(Config,["po_state.xss"])),

    {E,_} = xmerl_xsd:validate(E,S).


union(Config) ->
    {E,_} = xmerl_scan:file(datadir_join(Config,["instance.xml"])),
    {_E2 = #xmlElement{},_} = xmerl_xsd:process_validate(datadir_join(Config,["measCollec.xsd"]),E).
    
ticket_6910(Config) ->
    {E,_} = xmerl_scan:file(datadir_join(Config,[sis,"dummy_action_mim.xml"])),
    {_E2 = #xmlElement{},_} =
	xmerl_xsd:process_validate(datadir_join(Config,[sis,"mim2.xsd"]),E).

ticket_7165(Config) ->
    %% The validation option seems not to work
    {_E,_} = xmerl_scan:file(datadir_join(Config,["ticket_7288.xml"]),
                             [{validation, schema}]),
    %% The option xsdbase gave {error, enoent}.
    {ok,_} = xmerl_xsd:process_schema("CxDataType_Rel5.xsd", [{xsdbase, datadir(Config)}]).



ticket_7190(Config) ->
    {E,_} = xmerl_scan:file(datadir_join(Config,["int.xml"])),
    {_E2 = #xmlElement{},_} = xmerl_xsd:process_validate(datadir_join(Config,["simple_int.xsd"]),E).

ticket_7288(Config) ->
    %% The schema table in the state where deleted by xmerl_xsd:validate if there was an error.
    {E,_} = xmerl_scan:file(datadir_join(Config,["ticket_7288.xml"])),
    {ok,S} = xmerl_xsd:process_schema(datadir_join(Config,["CxDataType_Rel5.xsd"])),
    {error, EL} = xmerl_xsd:validate(E, S),
    {error, EL} = xmerl_xsd:validate(E, S).
 
ticket_7736(Config) ->
    DataDir = datadir(Config),
    {ok, State } = xmerl_xsd:process_schema(filename:join([DataDir,"enum_bug.xsd"])),
    
    {Entity ,_} = xmerl_scan:file(filename:join([DataDir,"enum_bug.xml"])),
    
    {#xmlElement{},_} = xmerl_xsd:validate(Entity, State).

ticket_8599(Config) ->
    {E,_} = xmerl_scan:file(datadir_join(Config,["ticket_8599.xml"])),
    
    {ok, S} = xmerl_xsd:process_schema(datadir_join(Config,["ticket_8599.xsd"])),
    
    {{xmlElement,persons,persons,_,_,_,_,_,_,_,_,_},_GlobalState} = xmerl_xsd:validate(E, S).


ticket_9410(Config) ->
    file:set_cwd(datadir_join(Config,[".."])),
    {ok, _S} = xmerl_xsd:process_schema("xmerl_xsd_SUITE_data/small.xsd").

%%======================================================================
%% Support Functions
%%======================================================================

privdir(Config) ->
    proplists:get_value(priv_dir, Config).
datadir(Config) ->
    proplists:get_value(data_dir, Config).

datadir_join(Config,Files) ->
    filename:join([datadir(Config)|Files]).
