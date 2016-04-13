%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
-module(m_i_impl).
-include("m.hrl").

-export([init/1, terminate/2, void_test/1, long_test/2, ushort_test/2,
	 longlong_test/2, ulong_test/2, ulonglong_test/2,
	 double_test/2, char_test/2, wchar_test/2, octet_test/2,
	 bool_test/2, struct_test/2, struct2_test/2, seq1_test/2,
	 seq2_test/2, seq3_test/2, seq4_test/2, seq5_test/2,
	 array1_test/2, array2_test/2, enum_test/2, string1_test/2,
	 string2_test/2, string3_test/2, string4_test/2, pid_test/2,
	 port_test/2, ref_test/2, term_test/2, typedef_test/3,
	 inline_sequence_test/2, '_set_l'/2, '_get_l'/1,
	 term_struct_test/2, term_sequence_test/2, wstring1_test/2]).

-define(PRINTDEBUG(Case),
	io:format("erl_server: case: ~p~n"
		  "erl_server: location: ~p~n", [Case, [?FILE, ?LINE]])).
-define(PRINTDEBUG2(Case, Msg),
	io:format("erl_server: case: ~p~n"
		  "erl_server: Msg: ~p~n"
		  "erl_server: location: ~p~n", [Case, Msg, [?FILE, ?LINE]])).

init(Env) ->
    {ok, []}.

terminate(F, R) ->
    ok.

'_get_l'(State) ->
    ?PRINTDEBUG("_get_l"),
    {reply, State, State}.
void_test(State) ->
    ?PRINTDEBUG("void_test"),
    {reply, ok, State}.

'_set_l'(State, V) ->
    ?PRINTDEBUG2("_set_l", V),
    {reply, ok, V}.
ushort_test(State, V) ->
    ?PRINTDEBUG2("ushort_test", V),
    {reply, {V, V}, State}.
long_test(State, V) ->
    ?PRINTDEBUG2("long_test", V),
    {reply, {V, V}, State}.
longlong_test(State, V) ->
    ?PRINTDEBUG2("longlong_test", V),
    {reply, {V, V}, State}.
ulong_test(State, V) ->
    ?PRINTDEBUG2("ulong_test", V),
    {reply, {V, V}, State}.
ulonglong_test(State, V) ->
    ?PRINTDEBUG2("ulonglong_test", V),
    {reply, {V, V}, State}.
double_test(State, V) ->
    ?PRINTDEBUG2("double_test", V),
    {reply, {V, V}, State}.
char_test(State, V) ->
    ?PRINTDEBUG2("char_test", V),
    {reply, {V, V}, State}.
wchar_test(State, V) ->
    ?PRINTDEBUG2("wchar_test", V),
    {reply, {V, V}, State}.
octet_test(State, V) ->
    ?PRINTDEBUG2("octet_test", V),
    {reply, {V, V}, State}.
bool_test(State, V) ->
    ?PRINTDEBUG2("bool_test", V),
    {reply, {V, V}, State}.

struct_test(State, V) ->
    ?PRINTDEBUG2("struct_test", V),
    {reply, {V, V}, State}.
struct2_test(State, V) ->
    ?PRINTDEBUG2("struct2_test", V),
    {reply, {V, V}, State}.
seq1_test(State, V) ->
    ?PRINTDEBUG2("seq1_test", V),
    {reply, {V, V}, State}.
seq2_test(State, V) ->
    ?PRINTDEBUG2("seq2_test", V),
    {reply, {V, V}, State}.
seq3_test(State, V) ->
    ?PRINTDEBUG2("seq3_test", V),
    {reply, {V, V}, State}.
seq4_test(State, V) ->
    ?PRINTDEBUG2("seq4_test", V),
    {reply, {V, V}, State}.
seq5_test(State, V) ->
    ?PRINTDEBUG2("seq5_test", V),
    {reply, {V, V}, State}.
array1_test(State, V) ->
    ?PRINTDEBUG2("array1_test", V),
    {reply, {V, V}, State}.
array2_test(State, V) ->
    ?PRINTDEBUG2("array2_test", V),
    {reply, {V, V}, State}.
enum_test(State, V) ->
    ?PRINTDEBUG2("enum_test", V),
    {reply, {V, V}, State}.
string1_test(State, V) ->
    ?PRINTDEBUG2("string1_test", V),
    {reply, {V, V}, State}.
string2_test(State, V) ->
    ?PRINTDEBUG2("string2_test", V),
    {reply, {V, V}, State}.
string3_test(State, V) ->
    ?PRINTDEBUG2("string3_test", V),
    {reply, {V, V}, State}.
string4_test(State, V) ->
    ?PRINTDEBUG2("string4_test", V),
    {reply, {V, V}, State}.
pid_test(State, V) ->
    ?PRINTDEBUG2("pid_test", V),
    {reply, {V, V}, State}.
port_test(State, V) ->
    ?PRINTDEBUG2("port_test", binary_to_list(term_to_binary(V))),
    {reply, {V, V}, State}.
ref_test(State, V) ->
    ?PRINTDEBUG2("ref_test", binary_to_list(term_to_binary(V))),
    {reply, {V, V}, State}.
term_test(State, V) ->
    ?PRINTDEBUG2("term_test", V),
    {reply, {V, V}, State}.
typedef_test(State, A, B) ->
    ?PRINTDEBUG2("typedef_test", [A,B]),
    {reply, {4711, A, B}, State}.
inline_sequence_test(State, V) ->
    ?PRINTDEBUG2("inline_sequence_test", V),
    {reply, {V, V}, State}.
term_sequence_test(State, V) ->
    ?PRINTDEBUG2("term_sequence_test", V),
    {reply, {V, V}, State}.
term_struct_test(State, V) ->
    ?PRINTDEBUG2("term_struct_test", V),
    {reply, {V, V}, State}.
wstring1_test(State, V) ->
    ?PRINTDEBUG2("wstring1_test", V),
    {reply, {V, V}, State}.




