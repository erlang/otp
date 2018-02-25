%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2017. All Rights Reserved.
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
%% Purpose: Information about the Erlang built-in functions.

-module(erl_bifs).

-export([is_pure/3, is_safe/3, is_exit_bif/3]).

%% =====================================================================
%% is_pure(Module, Name, Arity) -> boolean()
%%
%%	    Module = Name = atom()
%%	    Arity = integer()
%%
%%	Returns `true' if the function `Module:Name/Arity' does not
%%	affect the state, nor depend on the state, although its
%%	evaluation is not guaranteed to complete normally for all input.

-spec is_pure(atom(), atom(), arity()) -> boolean().

is_pure(erlang, '*', 2) -> true;
is_pure(erlang, '+', 1) -> true;    % (even for non-numbers)
is_pure(erlang, '+', 2) -> true;
is_pure(erlang, '++', 2) -> true;
is_pure(erlang, '-', 1) -> true;
is_pure(erlang, '-', 2) -> true;
is_pure(erlang, '--', 2) -> true;
is_pure(erlang, '/', 2) -> true;
is_pure(erlang, '/=', 2) -> true;
is_pure(erlang, '<', 2) -> true;
is_pure(erlang, '=/=', 2) -> true;
is_pure(erlang, '=:=', 2) -> true;
is_pure(erlang, '=<', 2) -> true;
is_pure(erlang, '==', 2) -> true;
is_pure(erlang, '>', 2) -> true;
is_pure(erlang, '>=', 2) -> true;
is_pure(erlang, 'and', 2) -> true;
is_pure(erlang, 'band', 2) -> true;
is_pure(erlang, 'bnot', 1) -> true;
is_pure(erlang, 'bor', 2) -> true;
is_pure(erlang, 'bsl', 2) -> true;
is_pure(erlang, 'bsr', 2) -> true;
is_pure(erlang, 'bxor', 2) -> true;
is_pure(erlang, 'div', 2) -> true;
is_pure(erlang, 'not', 1) -> true;
is_pure(erlang, 'or', 2) -> true;
is_pure(erlang, 'rem', 2) -> true;
is_pure(erlang, 'xor', 2) -> true;
is_pure(erlang, abs, 1) -> true;
is_pure(erlang, atom_to_binary, 2) -> true;
is_pure(erlang, atom_to_list, 1) -> true;
is_pure(erlang, binary_part, 2) -> true;
is_pure(erlang, binary_part, 3) -> true;
is_pure(erlang, binary_to_atom, 2) -> true;
is_pure(erlang, binary_to_float, 1) -> true;
is_pure(erlang, binary_to_integer, 1) -> true;
is_pure(erlang, binary_to_list, 1) -> true;
is_pure(erlang, binary_to_list, 3) -> true;
is_pure(erlang, bit_size, 1) -> true;
is_pure(erlang, byte_size, 1) -> true;
is_pure(erlang, ceil, 1) -> true;
is_pure(erlang, element, 2) -> true;
is_pure(erlang, float, 1) -> true;
is_pure(erlang, float_to_list, 1) -> true;
is_pure(erlang, float_to_binary, 1) -> true;
is_pure(erlang, floor, 1) -> true;
is_pure(erlang, hd, 1) -> true;
is_pure(erlang, integer_to_binary, 1) -> true;
is_pure(erlang, integer_to_list, 1) -> true;
is_pure(erlang, is_atom, 1) -> true;
is_pure(erlang, is_boolean, 1) -> true;
is_pure(erlang, is_binary, 1) -> true;
is_pure(erlang, is_bitstring, 1) -> true;
%% erlang:is_builtin/3 depends on the state (i.e. the version of the emulator).
is_pure(erlang, is_float, 1) -> true;
is_pure(erlang, is_function, 1) -> true;
is_pure(erlang, is_integer, 1) -> true;
is_pure(erlang, is_list, 1) -> true;
is_pure(erlang, is_map, 1) -> true;
is_pure(erlang, is_number, 1) -> true;
is_pure(erlang, is_pid, 1) -> true;
is_pure(erlang, is_port, 1) -> true;
is_pure(erlang, is_record, 2) -> true;
is_pure(erlang, is_record, 3) -> true;
is_pure(erlang, is_reference, 1) -> true;
is_pure(erlang, is_tuple, 1) -> true;
is_pure(erlang, length, 1) -> true;
is_pure(erlang, list_to_atom, 1) -> true;
is_pure(erlang, list_to_binary, 1) -> true;
is_pure(erlang, list_to_float, 1) -> true;
is_pure(erlang, list_to_integer, 1) -> true;
is_pure(erlang, list_to_pid, 1) -> true;
is_pure(erlang, list_to_tuple, 1) -> true;
is_pure(erlang, max, 2) -> true;
is_pure(erlang, make_fun, 3) -> true;
is_pure(erlang, min, 2) -> true;
is_pure(erlang, phash, 2) -> false;
is_pure(erlang, pid_to_list, 1) -> true;
is_pure(erlang, round, 1) -> true;
is_pure(erlang, setelement, 3) -> true;
is_pure(erlang, size, 1) -> true;
is_pure(erlang, split_binary, 2) -> true;
is_pure(erlang, term_to_binary, 1) -> true;
is_pure(erlang, tl, 1) -> true;
is_pure(erlang, trunc, 1) -> true;
is_pure(erlang, tuple_size, 1) -> true;
is_pure(erlang, tuple_to_list, 1) -> true;
is_pure(lists, append, 2) -> true;
is_pure(lists, subtract, 2) -> true;
is_pure(math, acos, 1) -> true;
is_pure(math, acosh, 1) -> true;
is_pure(math, asin, 1) -> true;
is_pure(math, asinh, 1) -> true;
is_pure(math, atan, 1) -> true;
is_pure(math, atan2, 2) -> true;
is_pure(math, atanh, 1) -> true;
is_pure(math, ceil, 1) -> true;
is_pure(math, cos, 1) -> true;
is_pure(math, cosh, 1) -> true;
is_pure(math, erf, 1) -> true;
is_pure(math, erfc, 1) -> true;
is_pure(math, exp, 1) -> true;
is_pure(math, floor, 1) -> true;
is_pure(math, fmod, 2) -> true;
is_pure(math, log, 1) -> true;
is_pure(math, log2, 1) -> true;
is_pure(math, log10, 1) -> true;
is_pure(math, pow, 2) -> true;
is_pure(math, sin, 1) -> true;
is_pure(math, sinh, 1) -> true;
is_pure(math, sqrt, 1) -> true;
is_pure(math, tan, 1) -> true;
is_pure(math, tanh, 1) -> true;
is_pure(math, pi, 0) -> true;
is_pure(_, _, _) -> false.


%% =====================================================================
%% is_safe(Module, Name, Arity) -> boolean()
%%
%%	    Module = Name = atom()
%%	    Arity = integer()
%%
%%	Returns `true' if the function `Module:Name/Arity' is completely
%%	effect free, i.e., if its evaluation always completes normally
%%	and does not affect the state (although the value it returns
%%	might depend on the state).
%%
%%      Note: is_function/2 and is_record/3 are NOT safe: is_function(X, foo)
%%      and is_record(X, foo, bar) will fail.

-spec is_safe(atom(), atom(), arity()) -> boolean().

is_safe(erlang, '/=', 2) -> true;
is_safe(erlang, '<', 2) -> true;
is_safe(erlang, '=/=', 2) -> true;
is_safe(erlang, '=:=', 2) -> true;
is_safe(erlang, '=<', 2) -> true;
is_safe(erlang, '==', 2) -> true;
is_safe(erlang, '>', 2) -> true;
is_safe(erlang, '>=', 2) -> true;
is_safe(erlang, date, 0) -> true;
is_safe(erlang, get, 0) -> true;
is_safe(erlang, get, 1) -> true;
is_safe(erlang, get_cookie, 0) -> true;
is_safe(erlang, get_keys, 1) -> true;
is_safe(erlang, group_leader, 0) -> true;
is_safe(erlang, is_alive, 0) -> true;
is_safe(erlang, is_atom, 1) -> true;
is_safe(erlang, is_boolean, 1) -> true;
is_safe(erlang, is_binary, 1) -> true;
is_safe(erlang, is_bitstring, 1) -> true;
is_safe(erlang, is_float, 1) -> true;
is_safe(erlang, is_function, 1) -> true;
is_safe(erlang, is_integer, 1) -> true;
is_safe(erlang, is_list, 1) -> true;
is_safe(erlang, is_number, 1) -> true;
is_safe(erlang, is_pid, 1) -> true;
is_safe(erlang, is_port, 1) -> true;
is_safe(erlang, is_reference, 1) -> true;
is_safe(erlang, is_tuple, 1) -> true;
is_safe(erlang, make_ref, 0) -> true;
is_safe(erlang, make_fun, 3) -> true;
is_safe(erlang, max, 2) -> true;
is_safe(erlang, min, 2) -> true;
is_safe(erlang, node, 0) -> true;
is_safe(erlang, nodes, 0) -> true;
is_safe(erlang, ports, 0) -> true;
is_safe(erlang, pre_loaded, 0) -> true;
is_safe(erlang, processes, 0) -> true;
is_safe(erlang, registered, 0) -> true;
is_safe(erlang, self, 0) -> true;
is_safe(erlang, term_to_binary, 1) -> true;
is_safe(erlang, time, 0) -> true;
is_safe(_, _, _) -> false.


%% =====================================================================
%% is_exit_bif(Module, Name, Arity) -> boolean()
%%
%%	    Module = Name = atom()
%%	    Arity = integer()
%%
%%	Returns `true' if the function `Module:Name/Arity' never returns
%%	normally, i.e., if it always causes an exception regardless of
%%	its arguments.

-spec is_exit_bif(atom(), atom(), arity()) -> boolean().

is_exit_bif(erlang, exit, 1) -> true;
is_exit_bif(erlang, throw, 1) -> true;
is_exit_bif(erlang, error, 1) -> true;
is_exit_bif(erlang, error, 2) -> true;
is_exit_bif(_, _, _) -> false.
