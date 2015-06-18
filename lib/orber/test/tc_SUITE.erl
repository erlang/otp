%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2011. All Rights Reserved.
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
%% Test suite for the basic typecode functions
%%
%%-----------------------------------------------------------------
-module(tc_SUITE).

-include_lib("test_server/include/test_server.hrl").
-include_lib("orber/src/orber_iiop.hrl").

-define(default_timeout, ?t:minutes(3)).

-define(match(Expr),
        fun() ->
		case (catch (Expr)) of
		    AcTuAlReS when is_binary(AcTuAlReS)->
			io:format("###### ERROR ERROR ######~nRESULT:  ~p~n",
				  [AcTuAlReS]),
			exit(AcTuAlReS);
		    _ ->
			ok
		end
	end()).
-define(SUB_ELIST, [{"null", orber_tc:null()},
		    {"void", orber_tc:void()},
		    {"short", orber_tc:short()},
		    {"unsigned_short", orber_tc:unsigned_short()},
		    {"long", orber_tc:long()},
		    {"unsigned_long", orber_tc:unsigned_long()},
		    {"long_long", orber_tc:long_long()},
		    {"unsigned_long_long", orber_tc:unsigned_long_long()},
		    {"float", orber_tc:'float'()},
		    {"double", orber_tc:double()},
		    {"longdouble", orber_tc:longdouble()},
		    {"boolean", orber_tc:boolean()},
		    {"char", orber_tc:char()},
		    {"wchar", orber_tc:wchar()},
		    {"octet", orber_tc:octet()},
		    {"any", orber_tc:any()},
		    {"typecode", orber_tc:typecode()},
		    {"principal", orber_tc:principal()},
		    {"object_reference", orber_tc:object_reference("Id", "Name")}]).

-define(ELIST, [{"null", orber_tc:null()},
		{"void", orber_tc:void()},
		{"short", orber_tc:short()},
		{"unsigned_short", orber_tc:unsigned_short()},
		{"long", orber_tc:long()},
		{"unsigned_long", orber_tc:unsigned_long()},
		{"long_long", orber_tc:long_long()},
		{"unsigned_long_long", orber_tc:unsigned_long_long()},
		{"float", orber_tc:'float'()},
		{"double", orber_tc:double()},
		{"longdouble", orber_tc:longdouble()},
		{"boolean", orber_tc:boolean()},
		{"char", orber_tc:char()},
		{"wchar", orber_tc:wchar()},
		{"octet", orber_tc:octet()},
		{"any", orber_tc:any()},
		{"typecode", orber_tc:typecode()},
		{"principal", orber_tc:principal()},
		{"object_reference", orber_tc:object_reference("Id", "Name")},
		{"struct", orber_tc:struct("Id", "Name", ?SUB_ELIST)},
		{"enum", orber_tc:enum("Id", "Name", ["E1", "E2"])},
		{"string", orber_tc:string(1)},
		{"wstring", orber_tc:wstring(0)},
		{"sequence", orber_tc:sequence(orber_tc:enum("Id", "Name", 
							     ["E1", "E2"]), 0)},
		{"array", orber_tc:array(orber_tc:enum("Id", "Name",
						       ["E1", "E2"]), 2)},
		{"alias", orber_tc:alias("id", "name", 
					 orber_tc:enum("Id", "Name",
						       ["E1", "E2"]))},
		{"exception", orber_tc:exception("Id", "Name", ?SUB_ELIST)}]).

-define(VELIST, [{"null", orber_tc:null(), 42},
		 {"void", orber_tc:void(), 42},
		 {"short", orber_tc:short(), 42},
		 {"unsigned_short", orber_tc:unsigned_short(), 42},
		 {"long", orber_tc:long(), 42},
		 {"unsigned_long", orber_tc:unsigned_long(), 42},
		 {"long_long", orber_tc:long_long(), 42},
		 {"unsigned_long_long", orber_tc:unsigned_long_long(), 42},
		 {"float", orber_tc:'float'(), 42},
		 {"double", orber_tc:double(), 42},
		 {"longdouble", orber_tc:longdouble(), 42},
		 {"boolean", orber_tc:boolean(), 42},
		 {"char", orber_tc:char(), 42},
		 {"wchar", orber_tc:wchar(), 42},
		 {"octet", orber_tc:octet(), 42},
		 {"any", orber_tc:any(), 42},
		 {"typecode", orber_tc:typecode(), 42},
		 {"principal", orber_tc:principal(), 42},
		 {"object_reference", orber_tc:object_reference("Id", "Name"), 42},
		 {"struct", orber_tc:struct("Id", "Name", ?SUB_ELIST), 42},
		 {"enum", orber_tc:enum("Id", "Name", ["E1", "E2"]), 42},
		 {"string", orber_tc:string(1), 42},
		 {"wstring", orber_tc:wstring(0), 42},
		 {"sequence", orber_tc:sequence(orber_tc:enum("Id", "Name", 
							      ["E1", "E2"]), 0), 42},
		 {"array", orber_tc:array(orber_tc:enum("Id", "Name",
							["E1", "E2"]), 2), 42},
		 {"alias", orber_tc:alias("id", "name", 
					  orber_tc:enum("Id", "Name",
							["E1", "E2"])), 42},
		 {"exception", orber_tc:exception("Id", "Name", ?SUB_ELIST), 42}]).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

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
    [null, void, short, ushort, long, ulong, longlong,
     ulonglong, boolean, char, wchar, octet, float, double,
     longdouble, any, typecode, principal, object_reference,
     struct, union, enum, string, wstring, sequence, array,
     alias, exception, fixed, value, value_box, native,
     abstract_interface, indirection, get_tc].

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
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%-----------------------------------------------------------------
%% Test Case: null test
%% Description: 
%%-----------------------------------------------------------------
null(doc) -> [];
null(suite) -> [];
null(_) ->
    ?line true = orber_tc:check_tc(orber_tc:null()),
    ?line code(orber_tc:null()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: void test
%% Description: 
%%-----------------------------------------------------------------
void(doc) -> [];
void(suite) -> [];
void(_) ->
    ?line true = orber_tc:check_tc(orber_tc:void()),
    ?line code(orber_tc:void()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: short integer test
%% Description: 
%%-----------------------------------------------------------------
short(doc) -> [];
short(suite) -> [];
short(_) ->
    ?line true = orber_tc:check_tc(orber_tc:short()),
    ?line code(orber_tc:short()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: unsigned short integer test
%% Description: 
%%-----------------------------------------------------------------
ushort(doc) -> [];
ushort(suite) -> [];
ushort(_) ->
    ?line true = orber_tc:check_tc(orber_tc:unsigned_short()),
    ?line code(orber_tc:unsigned_short()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: long integer test
%% Description: 
%%-----------------------------------------------------------------
long(doc) -> [];
long(suite) -> [];
long(_) ->
    ?line true = orber_tc:check_tc(orber_tc:long()),
    ?line code(orber_tc:long()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: unsigned long integer test
%% Description: 
%%-----------------------------------------------------------------
ulong(doc) -> [];
ulong(suite) -> [];
ulong(_) -> 
    ?line true = orber_tc:check_tc(orber_tc:unsigned_long()),
    ?line code(orber_tc:unsigned_long()),
    ok.
    

%%-----------------------------------------------------------------
%% Test Case: long integer test
%% Description: 
%%-----------------------------------------------------------------
longlong(doc) -> [];
longlong(suite) -> [];
longlong(_) ->
    ?line true = orber_tc:check_tc(orber_tc:long_long()),
    ?line code(orber_tc:long_long()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: unsigned long integer test
%% Description: 
%%-----------------------------------------------------------------
ulonglong(doc) -> [];
ulonglong(suite) -> [];
ulonglong(_) -> 
    ?line true = orber_tc:check_tc(orber_tc:unsigned_long_long()),
    ?line code(orber_tc:unsigned_long_long()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: float test
%% Description: 
%%-----------------------------------------------------------------
float(doc) -> [];
float(suite) -> [];
float(_) ->
    ?line true = orber_tc:check_tc(orber_tc:'float'()),
    ?line code(orber_tc:'float'()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: double test
%% Description: 
%%-----------------------------------------------------------------
double(doc) -> [];
double(suite) -> [];
double(_) ->
    ?line true = orber_tc:check_tc(orber_tc:double()),
    ?line code(orber_tc:double()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: longdouble test
%% Description: 
%%-----------------------------------------------------------------
longdouble(doc) -> [];
longdouble(suite) -> [];
longdouble(_) ->
    ?line true = orber_tc:check_tc(orber_tc:longdouble()),
    ?line code(orber_tc:longdouble()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: boolean test
%% Description: 
%%-----------------------------------------------------------------
boolean(doc) -> [];
boolean(suite) -> [];
boolean(_) ->
    ?line true = orber_tc:check_tc(orber_tc:boolean()),
    ?line code(orber_tc:boolean()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: character test
%% Description: 
%%-----------------------------------------------------------------
char(doc) -> [];
char(suite) -> [];
char(_) ->
    ?line true = orber_tc:check_tc(orber_tc:char()),
    ?line code(orber_tc:char()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: character test
%% Description: 
%%-----------------------------------------------------------------
wchar(doc) -> [];
wchar(suite) -> [];
wchar(_) ->
    ?line true = orber_tc:check_tc(orber_tc:wchar()),
    ?line code(orber_tc:wchar()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: octet test
%% Description: 
%%-----------------------------------------------------------------
octet(doc) -> [];
octet(suite) -> [];
octet(_) ->
    ?line true = orber_tc:check_tc(orber_tc:octet()),
    ?line code(orber_tc:octet()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: any test
%% Description: 
%%-----------------------------------------------------------------
any(doc) -> [];
any(suite) -> [];
any(_) ->
    ?line true = orber_tc:check_tc(orber_tc:any()),
    ?line code(orber_tc:any()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: typecode test
%% Description: 
%%-----------------------------------------------------------------
typecode(doc) -> [];
typecode(suite) -> [];
typecode(_) ->
    ?line true = orber_tc:check_tc(orber_tc:typecode()),
    ?line code(orber_tc:typecode()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: principal test
%% Description: 
%%-----------------------------------------------------------------
principal(doc) -> [];
principal(suite) -> [];
principal(_) ->
    ?line true = orber_tc:check_tc(orber_tc:principal()),
    ?line code(orber_tc:principal()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: object_reference test
%% Description: 
%%-----------------------------------------------------------------
object_reference(doc) -> [];
object_reference(suite) -> [];
object_reference(_) ->
    ?line true = orber_tc:check_tc(orber_tc:object_reference("Id", "Name")),
    ?line false = orber_tc:check_tc(orber_tc:object_reference(42, "Name")),
    ?line false = orber_tc:check_tc(orber_tc:object_reference("Id", 42)),
    ?line code(orber_tc:object_reference("Id", "Name")),
    ?line ?match(code(orber_tc:object_reference(42, "Name"))),
    ?line ?match(code(orber_tc:object_reference("Id", 42))),
    ok.

%%-----------------------------------------------------------------
%% Test Case: struct
%% Description: 
%%-----------------------------------------------------------------
struct(doc) -> [];
struct(suite) -> [];
struct(_) ->
    ?line true = orber_tc:check_tc(orber_tc:struct("Id", "Name", ?ELIST)),
    ?line false = orber_tc:check_tc(orber_tc:struct(42, "Name", ?ELIST)),
    ?line false = orber_tc:check_tc(orber_tc:struct("Id", false, ?ELIST)),
    ?line false = orber_tc:check_tc(orber_tc:struct("Id", "Name", ?VELIST)),
    ?line false = orber_tc:check_tc(orber_tc:struct("Id", "Name", "wrong")),
    ?line code(orber_tc:struct("Id", "Name", ?ELIST)),
    ?line ?match(code(orber_tc:struct(42, "Name", ?ELIST))),
    ?line ?match(code(orber_tc:struct("Id", false, ?ELIST))),
    ?line ?match(code(orber_tc:struct("Id", "Name", ?VELIST))),
    ?line ?match(code(orber_tc:struct("Id", "Name", "wrong"))),
    ok.

%%-----------------------------------------------------------------
%% Test Case: union
%% Description: 
%%-----------------------------------------------------------------
union(doc) -> [];
union(suite) -> [];
union(_) ->
    ?line true = orber_tc:check_tc(orber_tc:union("Id", "Name", orber_tc:long(), 
						  -1, [{1, "long", orber_tc:long()},
						       {2, "longlong", orber_tc:long()}])),
    ?line false = orber_tc:check_tc(orber_tc:union("Id", "Name", orber_tc:long(), 
						   -1, ?ELIST)),
    ?line false = orber_tc:check_tc(orber_tc:union(42, "Name", orber_tc:long(), 
						   -1, [{1, "long", orber_tc:long()},
							{2, "longlong", orber_tc:long()}])),
    ?line false = orber_tc:check_tc(orber_tc:union("Id", false, orber_tc:long(), 
						   -1, [{1, "long", orber_tc:long()},
							{2, "longlong", orber_tc:long()}])),
    ?line false = orber_tc:check_tc(orber_tc:union("Id", "Name", bad_tc, 
						   -1, [{1, "long", orber_tc:long()},
							{2, "longlong", orber_tc:long()}])),
    ?line false = orber_tc:check_tc(orber_tc:union("Id", "Name", orber_tc:long(), 
						   "wrong", [{1, "long", orber_tc:long()},
							     {2, "longlong", orber_tc:long()}])),

    ?line code(orber_tc:union("Id", "Name", orber_tc:long(), 
			      -1, [{1, "long", orber_tc:long()},
				   {2, "longlong", orber_tc:long()}])),
    ok.


%%-----------------------------------------------------------------
%% Test Case: enum test
%% Description: 
%%-----------------------------------------------------------------
enum(doc) -> [];
enum(suite) -> [];
enum(_) ->
    ?line true = orber_tc:check_tc(orber_tc:enum("Id", "Name", 
						 ["E1", "E2", "E3"])),
    ?line false = orber_tc:check_tc(orber_tc:enum(42, "Name", 
						  ["E1", "E2", "E3"])),
    ?line false = orber_tc:check_tc(orber_tc:enum("Id", false, 
						  ["E1", "E2", "E3"])),
    ?line false = orber_tc:check_tc(orber_tc:enum("Id", "Name", 
						  ["E1", false, "E3"])),
    ?line code(orber_tc:enum("Id", "Name", ["E1", "E2", "E3"])),
    ?line ?match(code(orber_tc:enum(false, "Name", ["E1", "E2", "E3"]))),
    ?line ?match(code(orber_tc:enum("Id", 42, ["E1", "E2", "E3"]))),
    ?line ?match(code(orber_tc:enum("Id", "Name", ["E1", false, "E3"]))),
    ok.

%%-----------------------------------------------------------------
%% Test Case: string
%% Description: 
%%-----------------------------------------------------------------
string(doc) -> [];
string(suite) -> [];
string(_) ->
    ?line true = orber_tc:check_tc(orber_tc:string(0)),
    ?line true = orber_tc:check_tc(orber_tc:string(1)),
    ?line false = orber_tc:check_tc(orber_tc:string("wrong")),
    ?line code(orber_tc:string(0)),
    ?line code(orber_tc:string(1)),
    ?line ?match(code(orber_tc:string(-1))),
    ?line ?match(code(orber_tc:string(?ULONGMAX+1))),
    ?line ?match(code(orber_tc:string("wrong"))),
    ok.

%%-----------------------------------------------------------------
%% Test Case: wstring
%% Description: 
%%-----------------------------------------------------------------
wstring(doc) -> [];
wstring(suite) -> [];
wstring(_) ->
    ?line true = orber_tc:check_tc(orber_tc:wstring(0)),
    ?line true = orber_tc:check_tc(orber_tc:wstring(1)),
    ?line false = orber_tc:check_tc(orber_tc:wstring("wrong")),
    ?line code(orber_tc:wstring(0)),
    ?line code(orber_tc:wstring(1)),
    ?line ?match(code(orber_tc:wstring(-1))),
    ?line ?match(code(orber_tc:wstring(?ULONGMAX+1))),
    ?line ?match(code(orber_tc:wstring(false))),
    ok.

%%-----------------------------------------------------------------
%% Test Case: sequence
%% Description: 
%%-----------------------------------------------------------------
sequence(doc) -> [];
sequence(suite) -> [];
sequence(_) ->
    ?line true = orber_tc:check_tc(orber_tc:sequence(orber_tc:struct("Id", "Name", ?ELIST), 0)),
    ?line code(orber_tc:sequence(orber_tc:struct("Id", "Name", ?ELIST), 0)),
    ok.

%%-----------------------------------------------------------------
%% Test Case: array
%% Description: 
%%-----------------------------------------------------------------
array(doc) -> [];
array(suite) -> [];
array(_) ->
    ?line true = orber_tc:check_tc(orber_tc:array(orber_tc:struct("Id", "Name", ?ELIST), 1)),
    ?line code(orber_tc:array(orber_tc:struct("Id", "Name", ?ELIST), 1)),
    ok.

%%-----------------------------------------------------------------
%% Test Case: alias
%% Description: 
%%-----------------------------------------------------------------
alias(doc) -> [];
alias(suite) -> [];
alias(_) ->
    ?line true = orber_tc:check_tc(orber_tc:alias("Id", "Name", orber_tc:struct("Id", "Name", ?ELIST))),
    ?line false = orber_tc:check_tc(orber_tc:alias(false, "Name", orber_tc:struct("Id", "Name", ?ELIST))),
    ?line false = orber_tc:check_tc(orber_tc:alias("Id", 42, orber_tc:struct("Id", "Name", ?ELIST))),
    ?line false = orber_tc:check_tc(orber_tc:alias("Id", "Name", "wrong")),
    ?line code(orber_tc:alias("Id", "Name", orber_tc:struct("Id", "Name", ?ELIST))),
    ?line ?match(code(orber_tc:alias("Id", "Name", orber_tc:struct("Id", "Name", ?VELIST)))),
    ok.

%%-----------------------------------------------------------------
%% Test Case: exception
%% Description: 
%%-----------------------------------------------------------------
exception(doc) -> [];
exception(suite) -> [];
exception(_) ->
    ?line true = orber_tc:check_tc(orber_tc:exception("Id", "Name", ?ELIST)),
    ?line false = orber_tc:check_tc(orber_tc:exception(42, "Name", ?ELIST)),
    ?line false = orber_tc:check_tc(orber_tc:exception("Id", false, ?ELIST)),
    ?line false = orber_tc:check_tc(orber_tc:exception("Id", "Name", "wrong")),
    ?line code(orber_tc:exception("Id", "Name", ?ELIST)),
    ?line ?match(code(orber_tc:exception(42, "Name", ?ELIST))),
    ?line ?match(code(orber_tc:exception("Id", false, ?ELIST))),
    ?line ?match(code(orber_tc:exception("Id", "Name", "wrong"))),

    ok.

%%-----------------------------------------------------------------
%% Test Case: fixed
%% Description: 
%%-----------------------------------------------------------------
fixed(doc) -> [];
fixed(suite) -> [];
fixed(_) ->
    ?line true = orber_tc:check_tc(orber_tc:fixed(25, 2)),
    ?line code(orber_tc:fixed(25, 2)),
    ok.

%%-----------------------------------------------------------------
%% Test Case: value
%% Description: 
%%-----------------------------------------------------------------
value(doc) -> [];
value(suite) -> [];
value(_) ->
    ?line true = orber_tc:check_tc(orber_tc:value("Id", "Name", 42,
						  orber_tc:fixed(25, 2), ?VELIST)),
    ?line false = orber_tc:check_tc(orber_tc:value(42, "Name", 42,
						   orber_tc:fixed(25, 2), ?VELIST)),
    ?line false = orber_tc:check_tc(orber_tc:value("Id", 42, 42,
						   orber_tc:fixed(25, 2), ?VELIST)),
    ?line false = orber_tc:check_tc(orber_tc:value("Id", "Name", "wrong",
						   orber_tc:fixed(25, 2), ?VELIST)),
    ?line false = orber_tc:check_tc(orber_tc:value("Id", "Name", "42",
						   orber_tc:fixed(25, 2), ?VELIST)),
    ?line false = orber_tc:check_tc(orber_tc:value("Id", "Name", "42",
						   ?VELIST, ?VELIST)),
    ?line false = orber_tc:check_tc(orber_tc:value("Id", "Name", "42",
						   orber_tc:fixed(25, 2), false)),

    ?line code(orber_tc:value("Id", "Name", 42, orber_tc:long(), ?VELIST)),
    ok.

%%-----------------------------------------------------------------
%% Test Case: value_box
%% Description: 
%%-----------------------------------------------------------------
value_box(doc) -> [];
value_box(suite) -> [];
value_box(_) ->
    ?line true = orber_tc:check_tc(orber_tc:value_box("Id", "Name", 
						      orber_tc:fixed(25, 2))),
    ?line false = orber_tc:check_tc(orber_tc:value_box(42, "Name", 
						       orber_tc:fixed(25, 2))),
    ?line false = orber_tc:check_tc(orber_tc:value_box("Id", 42, 
						       orber_tc:fixed(25, 2))),
    ?line false = orber_tc:check_tc(orber_tc:value_box("Id", "Name", "wrong")),
    ?line code(orber_tc:value_box("Id", "Name", orber_tc:long())),
    ?line ?match(code(orber_tc:value_box(42, "Name", orber_tc:short()))),
    ?line ?match(code(orber_tc:value_box("Id", 42, orber_tc:char()))),
    ?line ?match(code(orber_tc:value_box("Id", "Name", false))),
    ok.

%%-----------------------------------------------------------------
%% Test Case: native
%% Description: 
%%-----------------------------------------------------------------
native(doc) -> [];
native(suite) -> [];
native(_) ->
    ?line true = orber_tc:check_tc(orber_tc:native("Id", "Name")),
    ?line false = orber_tc:check_tc(orber_tc:native(42, "Name")),
    ?line false = orber_tc:check_tc(orber_tc:native("Id", 42)),
    ?line code(orber_tc:native("Id", "Name")),
    ?line ?match(code(orber_tc:native(42, "Name"))),
    ?line ?match(code(orber_tc:native("Id", 42))),
    ok.

%%-----------------------------------------------------------------
%% Test Case: abstract_interface
%% Description: 
%%-----------------------------------------------------------------
abstract_interface(doc) -> [];
abstract_interface(suite) -> [];
abstract_interface(_) ->
    ?line true = orber_tc:check_tc(orber_tc:abstract_interface("RepId", "Name")),
    ?line false = orber_tc:check_tc(orber_tc:abstract_interface(false, "Name")),
    ?line false = orber_tc:check_tc(orber_tc:abstract_interface("RepId", 42)),
    ?line code(orber_tc:abstract_interface("RepId", "Name")),
    ?line ?match(code(orber_tc:abstract_interface(42, "Name"))),
    ?line ?match(code(orber_tc:abstract_interface("Id", 42))),
    ok.



%%-----------------------------------------------------------------
%% Test Case: indirection
%% Description: 
%%-----------------------------------------------------------------
indirection(doc) -> [];
indirection(suite) -> [];
indirection(_) ->
    ?line true = orber_tc:check_tc({'none', 42}),
    ok.

%%-----------------------------------------------------------------
%% Test Case: get_tc
%% Description: 
%%-----------------------------------------------------------------
get_tc(doc) -> [];
get_tc(suite) -> [];
get_tc(_) ->
    TC = 'CosNaming_Binding':tc(),
    ?line TC = orber_tc:get_tc({'CosNaming_Binding', 42}),
    ?line ?match(orber_tc:get_tc({'none', 42})),
    ok.

%%-----------------------------------------------------------------
%% MISC Operations
%%-----------------------------------------------------------------
code(Value) ->
    cdr_encode:enc_type({1,2}, tk_TypeCode, Value).
