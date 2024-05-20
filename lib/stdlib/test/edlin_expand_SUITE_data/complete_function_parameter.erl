%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2024. All Rights Reserved.
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
-module(complete_function_parameter).

-export(
    [a_fun_name/2,
     an_untyped_fun/2,
     a_deeplist_fun/1,
     a_zero_arity_fun/0,
     multi_arity_fun/0,
     multi_arity_fun/1,
     multi_arity_fun/2,
     different_multi_arity_fun/1,
     different_multi_arity_fun/2,
     advanced_nested_parameter/1,
     test_year/1,
    'emoji_function🤯'/1,
    map_parameter_function/1,
    map_parameter_function/2,
     map_variable_parameter_function/2,
    tuple_parameter_function/2,
    list_parameter_function/2,
    non_empty_list_parameter_function/2,
    binary_parameter_function/2,
    neg_integer_parameter_function/2,
    non_neg_integer_parameter_function/2,
    integer_parameter_function/2,
    float_parameter_function/2,
    port_parameter_function/2,
    pid_parameter_function/2,
    record_parameter_function/2,
    function_parameter_function/2,
    reference_parameter_function/2,
    any_parameter_function/2,
    ann_type_parameter_function/2,
    ann_type_parameter_function2/2,
    atom_parameter_function/2,
    caseSensitiveFunctionName/2,
    caseSensitiveFunction/2,
    casesensitivefunction/2
    ]).
-record(a_record, {}).
%% test first and second parameter
    %% test multiple arities with same type on first parameter
    %% test multiple arities with different type on first parameter
    %% test that recursive types does not trigger endless loop
    %% test that getting type of out of bound parameter does not trigger crash
-spec a_fun_name(Start, End) -> Return when
    Start :: integer(),
    End :: integer(),
    Return:: integer().
a_fun_name(_Start, _End) -> 0.

an_untyped_fun(_Start, _End) -> 1.

-spec a_deeplist_fun(Deeplist) -> integer() when
    Deeplist :: T | [Deeplist],
    T :: term().
a_deeplist_fun(Deeplist) -> lists:flatten(Deeplist).

a_zero_arity_fun() -> 0.

-spec multi_arity_fun() -> integer().
multi_arity_fun() -> 0.

-spec multi_arity_fun(T1) -> integer() when
    T1 :: integer().
multi_arity_fun(_T1) -> 1.

-spec multi_arity_fun(T1,T2) -> integer() when
    T1 :: integer(),
    T2 :: boolean().
multi_arity_fun(_T1, _T2) -> 2.

-spec different_multi_arity_fun(T1) -> integer() when
    T1 :: integer().
different_multi_arity_fun(_T1) -> 1.
-spec different_multi_arity_fun(B1, T1) -> integer() when
    B1 :: boolean(),
    T1 :: integer().
different_multi_arity_fun(_T1, _T2) -> 2.

-spec advanced_nested_parameter(T1) -> integer() when
    T1 :: {atom1, {non_neg_integer(), non_neg_integer()}} | 'atom1' | 'atom2' | ['atom4' | 'atom5'].
advanced_nested_parameter(_T1) -> 0.

-spec test_year(Y) -> integer() when
    Y :: calendar:year().
test_year(_Y) -> 0.

-spec 'emoji_function🤯'(integer()) -> integer(). 
'emoji_function🤯'(_) -> 0.

-spec map_parameter_function(Map) -> boolean() when
    Map :: #{ integer() := a, a => 1, b => 2, c => 3, d => error}.
map_parameter_function(_) -> false.
-spec map_parameter_function(Map, any()) -> boolean() when
    Map :: #{a => 1, b => 2, c => 3, d => error}.
map_parameter_function(_,_) -> false.
-spec map_variable_parameter_function(Key, Map) -> Value when
    Map :: #{ Key => Value, _ => _}.
map_variable_parameter_function(_, _) ->
     false.

-spec binary_parameter_function(binary(), any()) -> boolean().
binary_parameter_function(_,_) -> false.

-spec tuple_parameter_function(tuple(), any()) -> boolean().
tuple_parameter_function(_,_) -> false.

-spec list_parameter_function(list(), any()) -> boolean().
list_parameter_function(_,_) -> false.

-spec non_empty_list_parameter_function(nonempty_list(), any()) -> boolean().
non_empty_list_parameter_function(_,_) -> false.

-spec integer_parameter_function(integer(), any()) -> boolean().
integer_parameter_function(_,_) -> false.

-spec non_neg_integer_parameter_function(non_neg_integer(), any()) -> boolean().
non_neg_integer_parameter_function(_,_) -> false.

-spec neg_integer_parameter_function(neg_integer(), any()) -> boolean().
neg_integer_parameter_function(_,_) -> false.

-spec float_parameter_function(float(), any()) -> boolean().
float_parameter_function(_,_) -> false.

-spec pid_parameter_function(pid(), any()) -> boolean().
pid_parameter_function(_,_) -> false.

-spec port_parameter_function(port(), any()) -> boolean().
port_parameter_function(_,_) -> false.

-spec record_parameter_function(A, any()) -> boolean() when
    A :: #a_record{}.
record_parameter_function(_,_) -> false.

-spec function_parameter_function(fun((any()) -> any()), any()) -> boolean().
function_parameter_function(_,_) -> false.

-spec reference_parameter_function(reference(), any()) -> boolean().
reference_parameter_function(_,_) -> false.

-spec any_parameter_function(any(), any()) -> boolean().
any_parameter_function(_,_) -> false.

-spec atom_parameter_function(atom, any()) -> boolean().
atom_parameter_function(_,_) -> false.

-spec ann_type_parameter_function(V::atom(), W::any()) -> boolean().
ann_type_parameter_function(_,_) -> false.

-spec ann_type_parameter_function2(W::any(), V::atom()) -> boolean().
ann_type_parameter_function2(_,_) -> false.

-spec caseSensitiveFunctionName(A::atom(), B::any()) -> boolean().
caseSensitiveFunctionName(_,_) -> false.
-spec caseSensitiveFunction(A::atom(), B::any()) -> boolean().
caseSensitiveFunction(_,_) -> false.
-spec casesensitivefunction(A::atom(), B::any()) -> boolean().
casesensitivefunction(_,_) -> false.
