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
-module(erl_client).

-export([void_test/2, long_test/2, longlong_test/2, ushort_test/2,
	 ulong_test/2, ulonglong_test/2, double_test/2, char_test/2,
	 wchar_test/2, octet_test/2, bool_test/2, struct_test/2,
	 struct2_test/2, seq1_test/2, seq2_test/2, seq3_test/2,
	 seq4_test/2, seq5_test/2, array1_test/2, array2_test/2,
	 enum_test/2, string1_test/2, wstring1_test/2, string2_test/2,
	 string3_test/2, string4_test/2, pid_test/2, port_test/2,
	 ref_test/2, term_test/2, typedef_test/2,
	 inline_sequence_test/2, term_sequence_test/2,
	 term_struct_test/2

]).

-include("m.hrl").
-include("m_i.hrl").
-include("oe_erl_c_test.hrl").

%%b
void_test(Node, Timeout) ->
    Ret = m_i:void_test({olsson, Node}, Timeout),
    Ret == void.				% XXX Not documented
%%e

%%b
long_test(Node, Timeout) ->
    In = max_long(),
    {Ret, Out} = m_i:long_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
longlong_test(Node, Timeout) ->
    In = 65537,
    {Ret, Out} = m_i:longlong_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
ushort_test(Node, Timeout) ->
    In = max_ushort(),
    {Ret, Out} = m_i:ushort_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
ulong_test(Node, Timeout) ->
    In = max_ulong(),
    {Ret, Out} = m_i:ulong_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
ulonglong_test(Node, Timeout) ->
    In = 65537,
    {Ret, Out} = m_i:ulonglong_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
double_test(Node, Timeout) ->
    In = 37768.93,
    {Ret, Out} = m_i:double_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
char_test(Node, Timeout) ->
    In = 80,
    {Ret, Out} = m_i:char_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
wchar_test(Node, Timeout) ->
    In = 4097,
    {Ret, Out} = m_i:wchar_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
octet_test(Node, Timeout) ->
    In = 255,
    {Ret, Out} = m_i:octet_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
bool_test(Node, Timeout) ->
    In = false,
    {Ret, Out} = m_i:bool_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
struct_test(Node, Timeout) ->
    In = #m_b{l = max_long(), c = $a},
    {Ret, Out} = m_i:struct_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
struct2_test(Node, Timeout) ->
    In = #m_es{ f = banana, l = max_long()},
    {Ret, Out} = m_i:struct2_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
seq1_test(Node, Timeout) ->
    B1 = #m_b{l = max_long(), c = $a},
    B2 = #m_b{l = min_long(), c = $b},
    In = [B1, B2],
    {Ret, Out} = m_i:seq1_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
seq2_test(Node, Timeout) ->
    B = #m_b{l = max_long(), c = $a},
    A = #m_a{l = min_long(), y = [B, B], d = 4711.31},
    In = [A, A, A],
    {Ret, Out} = m_i:seq2_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
seq3_test(Node, Timeout) ->
    In = [max_long(), min_long(), max_long()],
    {Ret, Out} = m_i:seq3_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
seq4_test(Node, Timeout) ->
    In = [["hej", "hopp"], ["ditt", "feta", "nylle"]],
    {Ret, Out} = m_i:seq4_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
seq5_test(Node, Timeout) ->
    Arr3 = mk_array(3, max_long()),
    In = [[Arr3, Arr3], [Arr3, Arr3, Arr3]],
    {Ret, Out} = m_i:seq5_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
array1_test(Node, Timeout) ->
    In = mk_array(500, min_long()),
    {Ret, Out} = m_i:array1_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
array2_test(Node, Timeout) ->
    In = mk_array(2, mk_array(3, min_long())),
    {Ret, Out} = m_i:array2_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
enum_test(Node, Timeout) ->
    In = banana,
    {Ret, Out} = m_i:enum_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
string1_test(Node, Timeout) ->
    In = "Die Paula muss beim Tango immer weinen", 
    {Ret, Out} = m_i:string1_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
wstring1_test(Node, Timeout) ->
    In = [1047| "ie Paula muss beim Tango immer weinen"], 
    {Ret, Out} = m_i:wstring1_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
string2_test(Node, Timeout) ->
    In = ["Lass doch die Blumen,", "Konrad!"],
    {Ret, Out} = m_i:string2_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
string3_test(Node, Timeout) ->
    In = "Seeman, lass uns freuden!", 
    {Ret, Out} = m_i:string3_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
string4_test(Node, Timeout) ->
    
    In = #m_strRec{
      bb = true, 
      str4 = "Paula war zu Hause in ihrem Stadtchen als die beste Tanzerin"
      "bekannt",
      str7 = mk_array(3, mk_array(2, max_long())),
      str5 = [$a, $b, $c, $d, $e, $f],
      str6 = "123456789012",
      str8 = {$x, $y, $x},
      str9 = "123456789012",
      str10 = [$a, $b, $c, $d, $e, $f]
      },
    {Ret, Out} = m_i:string4_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
pid_test(Node, Timeout) ->
    In = self(),
    {Ret, Out} = m_i:pid_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
port_test(Node, Timeout) ->
    In = get(port_test_port),
    {Ret, Out} = m_i:port_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
ref_test(Node, Timeout) ->
    In = make_ref(),
    {Ret, Out} = m_i:ref_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
term_test(Node, Timeout) ->
    In = {[a, b], 17, kalle},
    {Ret, Out} = m_i:term_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
typedef_test(Node, Timeout) ->
    In1 = {nisse, [1, 2], olsson},
    In2 = get(port_test_port),
    {Ret, Out1, Out2} = m_i:typedef_test({olsson, Node}, Timeout, In1, In2),
    %% XXX Should check that Ret is an integer.
    (Out1 == In1) and (Out2 == In2).
%%e

%%b
inline_sequence_test(Node, Timeout) ->
    In = #m_s{l = min_long(), sl = [max_long(), min_long()]}, 
    {Ret, Out} = m_i:inline_sequence_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
term_sequence_test(Node, Timeout) ->
    In = lists:duplicate(17, {nisse, [1, 2], {kalle, olsson}}),
    {Ret, Out} = m_i:term_sequence_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e

%%b
term_struct_test(Node, Timeout) ->
    In = #m_et{e = {nisse, ["abcde"], {kalle, olsson}}, l = 4711},
    {Ret, Out} = m_i:term_struct_test({olsson, Node}, Timeout, In),
    (Ret == In) and (Out == In).
%%e


%% Locals

mk_array(Es) ->
    list_to_tuple(Es).

mk_array(N, E) ->
    mk_array(lists:duplicate(N, E)). 

%% max_short() ->
%%     power_of_two(15) - 1.
max_long() ->
    power_of_two(31) - 1.
max_longlong() ->
    power_of_two(63) - 1.
max_ushort() ->
    power_of_two(16) - 1.
max_ulong() ->
    power_of_two(32) - 1.
max_ulonglong() ->
    power_of_two(64) - 1.

%% min_short() ->
%%     -power_of_two(15).
min_long() ->
    -power_of_two(31).
%% min_longlong() ->
%%     -power_of_two(63).
%% min_ushort() ->
%%     0.
%% min_ulong() ->
%%     0.
%% min_ulonglong() ->
%%     0.

power_of_two(N) -> 
    round(math:pow(2, N)).

