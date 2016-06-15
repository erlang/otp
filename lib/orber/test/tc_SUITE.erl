%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
%% 
%% Licensed under the Apache Li2cense, Version 2.0 (the "License");
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

-include_lib("common_test/include/ct.hrl").
-include_lib("orber/src/orber_iiop.hrl").

-define(default_timeout, test_server:minutes(3)).

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
    Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    Dog = proplists:get_value(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%-----------------------------------------------------------------
%% Test Case: null test
%% Description: 
%%-----------------------------------------------------------------
null(_) ->
    true = orber_tc:check_tc(orber_tc:null()),
    code(orber_tc:null()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: void test
%% Description: 
%%-----------------------------------------------------------------
void(_) ->
    true = orber_tc:check_tc(orber_tc:void()),
    code(orber_tc:void()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: short integer test
%% Description: 
%%-----------------------------------------------------------------
short(_) ->
    true = orber_tc:check_tc(orber_tc:short()),
    code(orber_tc:short()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: unsigned short integer test
%% Description: 
%%-----------------------------------------------------------------
ushort(_) ->
    true = orber_tc:check_tc(orber_tc:unsigned_short()),
    code(orber_tc:unsigned_short()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: long integer test
%% Description: 
%%-----------------------------------------------------------------
long(_) ->
    true = orber_tc:check_tc(orber_tc:long()),
    code(orber_tc:long()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: unsigned long integer test
%% Description: 
%%-----------------------------------------------------------------
ulong(_) -> 
    true = orber_tc:check_tc(orber_tc:unsigned_long()),
    code(orber_tc:unsigned_long()),
    ok.
    

%%-----------------------------------------------------------------
%% Test Case: long integer test
%% Description: 
%%-----------------------------------------------------------------
longlong(_) ->
    true = orber_tc:check_tc(orber_tc:long_long()),
    code(orber_tc:long_long()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: unsigned long integer test
%% Description: 
%%-----------------------------------------------------------------
ulonglong(_) -> 
    true = orber_tc:check_tc(orber_tc:unsigned_long_long()),
    code(orber_tc:unsigned_long_long()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: float test
%% Description: 
%%-----------------------------------------------------------------
float(_) ->
    true = orber_tc:check_tc(orber_tc:'float'()),
    code(orber_tc:'float'()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: double test
%% Description: 
%%-----------------------------------------------------------------
double(_) ->
    true = orber_tc:check_tc(orber_tc:double()),
    code(orber_tc:double()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: longdouble test
%% Description: 
%%-----------------------------------------------------------------
longdouble(_) ->
    true = orber_tc:check_tc(orber_tc:longdouble()),
    code(orber_tc:longdouble()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: boolean test
%% Description: 
%%-----------------------------------------------------------------
boolean(_) ->
    true = orber_tc:check_tc(orber_tc:boolean()),
    code(orber_tc:boolean()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: character test
%% Description: 
%%-----------------------------------------------------------------
char(_) ->
    true = orber_tc:check_tc(orber_tc:char()),
    code(orber_tc:char()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: character test
%% Description: 
%%-----------------------------------------------------------------
wchar(_) ->
    true = orber_tc:check_tc(orber_tc:wchar()),
    code(orber_tc:wchar()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: octet test
%% Description: 
%%-----------------------------------------------------------------
octet(_) ->
    true = orber_tc:check_tc(orber_tc:octet()),
    code(orber_tc:octet()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: any test
%% Description: 
%%-----------------------------------------------------------------
any(_) ->
    true = orber_tc:check_tc(orber_tc:any()),
    code(orber_tc:any()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: typecode test
%% Description: 
%%-----------------------------------------------------------------
typecode(_) ->
    true = orber_tc:check_tc(orber_tc:typecode()),
    code(orber_tc:typecode()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: principal test
%% Description: 
%%-----------------------------------------------------------------
principal(_) ->
    true = orber_tc:check_tc(orber_tc:principal()),
    code(orber_tc:principal()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: object_reference test
%% Description: 
%%-----------------------------------------------------------------
object_reference(_) ->
    true = orber_tc:check_tc(orber_tc:object_reference("Id", "Name")),
    false = orber_tc:check_tc(orber_tc:object_reference(42, "Name")),
    false = orber_tc:check_tc(orber_tc:object_reference("Id", 42)),
    code(orber_tc:object_reference("Id", "Name")),
    ?match(code(orber_tc:object_reference(42, "Name"))),
    ?match(code(orber_tc:object_reference("Id", 42))),
    ok.

%%-----------------------------------------------------------------
%% Test Case: struct
%% Description: 
%%-----------------------------------------------------------------
struct(_) ->
    true = orber_tc:check_tc(orber_tc:struct("Id", "Name", ?ELIST)),
    false = orber_tc:check_tc(orber_tc:struct(42, "Name", ?ELIST)),
    false = orber_tc:check_tc(orber_tc:struct("Id", false, ?ELIST)),
    false = orber_tc:check_tc(orber_tc:struct("Id", "Name", ?VELIST)),
    false = orber_tc:check_tc(orber_tc:struct("Id", "Name", "wrong")),
    code(orber_tc:struct("Id", "Name", ?ELIST)),
    ?match(code(orber_tc:struct(42, "Name", ?ELIST))),
    ?match(code(orber_tc:struct("Id", false, ?ELIST))),
    ?match(code(orber_tc:struct("Id", "Name", ?VELIST))),
    ?match(code(orber_tc:struct("Id", "Name", "wrong"))),
    ok.

%%-----------------------------------------------------------------
%% Test Case: union
%% Description: 
%%-----------------------------------------------------------------
union(_) ->
    true = orber_tc:check_tc(orber_tc:union("Id", "Name", orber_tc:long(), 
						  -1, [{1, "long", orber_tc:long()},
						       {2, "longlong", orber_tc:long()}])),
    false = orber_tc:check_tc(orber_tc:union("Id", "Name", orber_tc:long(), 
						   -1, ?ELIST)),
    false = orber_tc:check_tc(orber_tc:union(42, "Name", orber_tc:long(), 
						   -1, [{1, "long", orber_tc:long()},
							{2, "longlong", orber_tc:long()}])),
    false = orber_tc:check_tc(orber_tc:union("Id", false, orber_tc:long(), 
						   -1, [{1, "long", orber_tc:long()},
							{2, "longlong", orber_tc:long()}])),
    false = orber_tc:check_tc(orber_tc:union("Id", "Name", bad_tc, 
						   -1, [{1, "long", orber_tc:long()},
							{2, "longlong", orber_tc:long()}])),
    false = orber_tc:check_tc(orber_tc:union("Id", "Name", orber_tc:long(), 
						   "wrong", [{1, "long", orber_tc:long()},
							     {2, "longlong", orber_tc:long()}])),

    code(orber_tc:union("Id", "Name", orber_tc:long(), 
			      -1, [{1, "long", orber_tc:long()},
				   {2, "longlong", orber_tc:long()}])),
    ok.


%%-----------------------------------------------------------------
%% Test Case: enum test
%% Description: 
%%-----------------------------------------------------------------
enum(_) ->
    true = orber_tc:check_tc(orber_tc:enum("Id", "Name", 
						 ["E1", "E2", "E3"])),
    false = orber_tc:check_tc(orber_tc:enum(42, "Name", 
						  ["E1", "E2", "E3"])),
    false = orber_tc:check_tc(orber_tc:enum("Id", false, 
						  ["E1", "E2", "E3"])),
    false = orber_tc:check_tc(orber_tc:enum("Id", "Name", 
						  ["E1", false, "E3"])),
    code(orber_tc:enum("Id", "Name", ["E1", "E2", "E3"])),
    ?match(code(orber_tc:enum(false, "Name", ["E1", "E2", "E3"]))),
    ?match(code(orber_tc:enum("Id", 42, ["E1", "E2", "E3"]))),
    ?match(code(orber_tc:enum("Id", "Name", ["E1", false, "E3"]))),
    ok.

%%-----------------------------------------------------------------
%% Test Case: string
%% Description: 
%%-----------------------------------------------------------------
string(_) ->
    true = orber_tc:check_tc(orber_tc:string(0)),
    true = orber_tc:check_tc(orber_tc:string(1)),
    false = orber_tc:check_tc(orber_tc:string("wrong")),
    code(orber_tc:string(0)),
    code(orber_tc:string(1)),
    ?match(code(orber_tc:string(-1))),
    ?match(code(orber_tc:string(?ULONGMAX+1))),
    ?match(code(orber_tc:string("wrong"))),
    ok.

%%-----------------------------------------------------------------
%% Test Case: wstring
%% Description: 
%%-----------------------------------------------------------------
wstring(_) ->
    true = orber_tc:check_tc(orber_tc:wstring(0)),
    true = orber_tc:check_tc(orber_tc:wstring(1)),
    false = orber_tc:check_tc(orber_tc:wstring("wrong")),
    code(orber_tc:wstring(0)),
    code(orber_tc:wstring(1)),
    ?match(code(orber_tc:wstring(-1))),
    ?match(code(orber_tc:wstring(?ULONGMAX+1))),
    ?match(code(orber_tc:wstring(false))),
    ok.

%%-----------------------------------------------------------------
%% Test Case: sequence
%% Description: 
%%-----------------------------------------------------------------
sequence(_) ->
    true = orber_tc:check_tc(orber_tc:sequence(orber_tc:struct("Id", "Name", ?ELIST), 0)),
    code(orber_tc:sequence(orber_tc:struct("Id", "Name", ?ELIST), 0)),
    ok.

%%-----------------------------------------------------------------
%% Test Case: array
%% Description: 
%%-----------------------------------------------------------------
array(_) ->
    true = orber_tc:check_tc(orber_tc:array(orber_tc:struct("Id", "Name", ?ELIST), 1)),
    code(orber_tc:array(orber_tc:struct("Id", "Name", ?ELIST), 1)),
    ok.

%%-----------------------------------------------------------------
%% Test Case: alias
%% Description: 
%%-----------------------------------------------------------------
alias(_) ->
    true = orber_tc:check_tc(orber_tc:alias("Id", "Name", orber_tc:struct("Id", "Name", ?ELIST))),
    false = orber_tc:check_tc(orber_tc:alias(false, "Name", orber_tc:struct("Id", "Name", ?ELIST))),
    false = orber_tc:check_tc(orber_tc:alias("Id", 42, orber_tc:struct("Id", "Name", ?ELIST))),
    false = orber_tc:check_tc(orber_tc:alias("Id", "Name", "wrong")),
    code(orber_tc:alias("Id", "Name", orber_tc:struct("Id", "Name", ?ELIST))),
    ?match(code(orber_tc:alias("Id", "Name", orber_tc:struct("Id", "Name", ?VELIST)))),
    ok.

%%-----------------------------------------------------------------
%% Test Case: exception
%% Description: 
%%-----------------------------------------------------------------
exception(_) ->
    true = orber_tc:check_tc(orber_tc:exception("Id", "Name", ?ELIST)),
    false = orber_tc:check_tc(orber_tc:exception(42, "Name", ?ELIST)),
    false = orber_tc:check_tc(orber_tc:exception("Id", false, ?ELIST)),
    false = orber_tc:check_tc(orber_tc:exception("Id", "Name", "wrong")),
    code(orber_tc:exception("Id", "Name", ?ELIST)),
    ?match(code(orber_tc:exception(42, "Name", ?ELIST))),
    ?match(code(orber_tc:exception("Id", false, ?ELIST))),
    ?match(code(orber_tc:exception("Id", "Name", "wrong"))),

    ok.

%%-----------------------------------------------------------------
%% Test Case: fixed
%% Description: 
%%-----------------------------------------------------------------
fixed(_) ->
    true = orber_tc:check_tc(orber_tc:fixed(25, 2)),
    code(orber_tc:fixed(25, 2)),
    ok.

%%-----------------------------------------------------------------
%% Test Case: value
%% Description: 
%%-----------------------------------------------------------------
value(_) ->
    true = orber_tc:check_tc(orber_tc:value("Id", "Name", 42,
						  orber_tc:fixed(25, 2), ?VELIST)),
    false = orber_tc:check_tc(orber_tc:value(42, "Name", 42,
						   orber_tc:fixed(25, 2), ?VELIST)),
    false = orber_tc:check_tc(orber_tc:value("Id", 42, 42,
						   orber_tc:fixed(25, 2), ?VELIST)),
    false = orber_tc:check_tc(orber_tc:value("Id", "Name", "wrong",
						   orber_tc:fixed(25, 2), ?VELIST)),
    false = orber_tc:check_tc(orber_tc:value("Id", "Name", "42",
						   orber_tc:fixed(25, 2), ?VELIST)),
    false = orber_tc:check_tc(orber_tc:value("Id", "Name", "42",
						   ?VELIST, ?VELIST)),
    false = orber_tc:check_tc(orber_tc:value("Id", "Name", "42",
						   orber_tc:fixed(25, 2), false)),

    code(orber_tc:value("Id", "Name", 42, orber_tc:long(), ?VELIST)),
    ok.

%%-----------------------------------------------------------------
%% Test Case: value_box
%% Description: 
%%-----------------------------------------------------------------
value_box(_) ->
    true = orber_tc:check_tc(orber_tc:value_box("Id", "Name", 
						      orber_tc:fixed(25, 2))),
    false = orber_tc:check_tc(orber_tc:value_box(42, "Name", 
						       orber_tc:fixed(25, 2))),
    false = orber_tc:check_tc(orber_tc:value_box("Id", 42, 
						       orber_tc:fixed(25, 2))),
    false = orber_tc:check_tc(orber_tc:value_box("Id", "Name", "wrong")),
    code(orber_tc:value_box("Id", "Name", orber_tc:long())),
    ?match(code(orber_tc:value_box(42, "Name", orber_tc:short()))),
    ?match(code(orber_tc:value_box("Id", 42, orber_tc:char()))),
    ?match(code(orber_tc:value_box("Id", "Name", false))),
    ok.

%%-----------------------------------------------------------------
%% Test Case: native
%% Description: 
%%-----------------------------------------------------------------
native(_) ->
    true = orber_tc:check_tc(orber_tc:native("Id", "Name")),
    false = orber_tc:check_tc(orber_tc:native(42, "Name")),
    false = orber_tc:check_tc(orber_tc:native("Id", 42)),
    code(orber_tc:native("Id", "Name")),
    ?match(code(orber_tc:native(42, "Name"))),
    ?match(code(orber_tc:native("Id", 42))),
    ok.

%%-----------------------------------------------------------------
%% Test Case: abstract_interface
%% Description: 
%%-----------------------------------------------------------------
abstract_interface(_) ->
    true = orber_tc:check_tc(orber_tc:abstract_interface("RepId", "Name")),
    false = orber_tc:check_tc(orber_tc:abstract_interface(false, "Name")),
    false = orber_tc:check_tc(orber_tc:abstract_interface("RepId", 42)),
    code(orber_tc:abstract_interface("RepId", "Name")),
    ?match(code(orber_tc:abstract_interface(42, "Name"))),
    ?match(code(orber_tc:abstract_interface("Id", 42))),
    ok.



%%-----------------------------------------------------------------
%% Test Case: indirection
%% Description: 
%%-----------------------------------------------------------------
indirection(_) ->
    true = orber_tc:check_tc({'none', 42}),
    ok.

%%-----------------------------------------------------------------
%% Test Case: get_tc
%% Description: 
%%-----------------------------------------------------------------
get_tc(_) ->
    TC = 'CosNaming_Binding':tc(),
    TC = orber_tc:get_tc({'CosNaming_Binding', 42}),
    ?match(orber_tc:get_tc({'none', 42})),
    ok.

%%-----------------------------------------------------------------
%% MISC Operations
%%-----------------------------------------------------------------
code(Value) ->
    cdr_encode:enc_type({1,2}, tk_TypeCode, Value).
