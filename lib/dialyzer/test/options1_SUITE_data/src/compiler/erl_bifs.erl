%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: erl_bifs.erl,v 1.2 2009/09/17 09:46:19 kostis Exp $
%%
%% Purpose: Information about the Erlang built-in functions.

-module(erl_bifs).

-export([is_bif/3, is_guard_bif/3, is_pure/3, is_safe/3]).


%% =====================================================================
%% is_bif(Module, Name, Arity) -> boolean()
%%
%%	    Module = Name = atom()
%%	    Arity = integer()
%%
%%	Returns `true' if the function `Module:Name/Arity' is a Built-In
%%	Function (BIF) of Erlang. BIFs "come with the implementation",
%%	and can be assumed to exist and have the same behaviour in any
%%	later versions of the same implementation of the language. Being
%%	a BIF does *not* imply that the function belongs to the module
%%	`erlang', nor that it is implemented in C or assembler (cf.
%%	`erlang:is_builtin/3'), or that it is auto-imported by the
%%	compiler (cf. `erl_internal:bif/3').

is_bif(erlang, '!', 2) -> true;
is_bif(erlang, '*', 2) -> true;
is_bif(erlang, '+', 1) -> true;
is_bif(erlang, '+', 2) -> true;
is_bif(erlang, '++', 2) -> true;
is_bif(erlang, '-', 1) -> true;
is_bif(erlang, '-', 2) -> true;
is_bif(erlang, '--', 2) -> true;
is_bif(erlang, '/', 2) -> true;
is_bif(erlang, '/=', 2) -> true;
is_bif(erlang, '<', 2) -> true;
is_bif(erlang, '=/=', 2) -> true;
is_bif(erlang, '=:=', 2) -> true;
is_bif(erlang, '=<', 2) -> true;
is_bif(erlang, '==', 2) -> true;
is_bif(erlang, '>', 2) -> true;
is_bif(erlang, '>=', 2) -> true;
is_bif(erlang, 'and', 2) -> true;
is_bif(erlang, 'band', 2) -> true;
is_bif(erlang, 'bnot', 1) -> true;
is_bif(erlang, 'bor', 2) -> true;
is_bif(erlang, 'bsl', 2) -> true;
is_bif(erlang, 'bsr', 2) -> true;
is_bif(erlang, 'bxor', 2) -> true;
is_bif(erlang, 'div', 2) -> true;
is_bif(erlang, 'not', 1) -> true;
is_bif(erlang, 'or', 2) -> true;
is_bif(erlang, 'rem', 2) -> true;
is_bif(erlang, 'xor', 2) -> true;
is_bif(erlang, abs, 1) -> true;
is_bif(erlang, append_element, 2) -> true;
is_bif(erlang, apply, 2) -> true;
is_bif(erlang, apply, 3) -> true;
is_bif(erlang, atom_to_list, 1) -> true;
is_bif(erlang, binary_to_list, 1) -> true;
is_bif(erlang, binary_to_list, 3) -> true;
is_bif(erlang, binary_to_term, 1) -> true;
is_bif(erlang, cancel_timer, 1) -> true;
is_bif(erlang, concat_binary, 1) -> true;
is_bif(erlang, date, 0) -> true;
is_bif(erlang, demonitor, 1) -> true;
is_bif(erlang, disconnect_node, 1) -> true;
is_bif(erlang, display, 1) -> true;
is_bif(erlang, element, 2) -> true;
is_bif(erlang, erase, 0) -> true;
is_bif(erlang, erase, 1) -> true;
is_bif(erlang, error, 1) -> true;
is_bif(erlang, error, 2) -> true;
is_bif(erlang, exit, 1) -> true;
is_bif(erlang, exit, 2) -> true;
is_bif(erlang, fault, 1) -> true;
is_bif(erlang, fault, 2) -> true;
is_bif(erlang, float, 1) -> true;
is_bif(erlang, float_to_list, 1) -> true;
is_bif(erlang, fun_info, 1) -> true;
is_bif(erlang, fun_info, 2) -> true;
is_bif(erlang, fun_to_list, 1) -> true;
is_bif(erlang, get, 0) -> true;
is_bif(erlang, get, 1) -> true;
is_bif(erlang, get_cookie, 0) -> true;
is_bif(erlang, get_keys, 1) -> true;
is_bif(erlang, group_leader, 0) -> true;
is_bif(erlang, group_leader, 2) -> true;
is_bif(erlang, halt, 0) -> false;
is_bif(erlang, halt, 1) -> false;
is_bif(erlang, hash, 2) -> false;
is_bif(erlang, hd, 1) -> true;
is_bif(erlang, info, 1) -> true;
is_bif(erlang, integer_to_list, 1) -> true;
is_bif(erlang, is_alive, 0) -> true;
is_bif(erlang, is_atom, 1) -> true;
is_bif(erlang, is_binary, 1) -> true;
is_bif(erlang, is_boolean, 1) -> true;
is_bif(erlang, is_builtin, 3) -> true;
is_bif(erlang, is_constant, 1) -> true;
is_bif(erlang, is_float, 1) -> true;
is_bif(erlang, is_function, 1) -> true;
is_bif(erlang, is_integer, 1) -> true;
is_bif(erlang, is_list, 1) -> true;
is_bif(erlang, is_number, 1) -> true;
is_bif(erlang, is_pid, 1) -> true;
is_bif(erlang, is_port, 1) -> true;
is_bif(erlang, is_process_alive, 1) -> true;
is_bif(erlang, is_record, 3) -> true;
is_bif(erlang, is_reference, 1) -> true;
is_bif(erlang, is_tuple, 1) -> true;
is_bif(erlang, length, 1) -> true;
is_bif(erlang, link, 1) -> true;
is_bif(erlang, list_to_atom, 1) -> true;
is_bif(erlang, list_to_binary, 1) -> true;
is_bif(erlang, list_to_float, 1) -> true;
is_bif(erlang, list_to_integer, 1) -> true;
is_bif(erlang, list_to_pid, 1) -> true;
is_bif(erlang, list_to_tuple, 1) -> true;
is_bif(erlang, loaded, 0) -> true;
is_bif(erlang, localtime, 0) -> true;
is_bif(erlang, localtime_to_universaltime, 1) -> true;
is_bif(erlang, make_ref, 0) -> true;
is_bif(erlang, make_tuple, 2) -> true;
is_bif(erlang, md5, 1) -> true;
is_bif(erlang, md5_final, 1) -> true;
is_bif(erlang, md5_init, 0) -> true;
is_bif(erlang, md5_update, 2) -> true;
is_bif(erlang, monitor, 2) -> true;
is_bif(erlang, monitor_node, 2) -> true;
is_bif(erlang, node, 0) -> true;
is_bif(erlang, node, 1) -> true;
is_bif(erlang, nodes, 0) -> true;
is_bif(erlang, now, 0) -> true;
is_bif(erlang, open_port, 2) -> true;
is_bif(erlang, phash, 2) -> true;
is_bif(erlang, pid_to_list, 1) -> true;
is_bif(erlang, port_close, 2) -> true;
is_bif(erlang, port_command, 2) -> true;
is_bif(erlang, port_connect, 2) -> true;
is_bif(erlang, port_control, 3) -> true;
is_bif(erlang, port_info, 2) -> true;
is_bif(erlang, port_to_list, 1) -> true;
is_bif(erlang, ports, 0) -> true;
is_bif(erlang, pre_loaded, 0) -> true;
is_bif(erlang, process_display, 2) -> true;
is_bif(erlang, process_flag, 2) -> true;
is_bif(erlang, process_flag, 3) -> true;
is_bif(erlang, process_info, 1) -> true;
is_bif(erlang, process_info, 2) -> true;
is_bif(erlang, processes, 0) -> true;
is_bif(erlang, put, 2) -> true;
is_bif(erlang, read_timer, 1) -> true;
is_bif(erlang, ref_to_list, 1) -> true;
is_bif(erlang, register, 2) -> true;
is_bif(erlang, registered, 0) -> true;
is_bif(erlang, resume_process, 1) -> true;
is_bif(erlang, round, 1) -> true;
is_bif(erlang, self, 0) -> true;
is_bif(erlang, send_after, 3) -> true;
is_bif(erlang, set_cookie, 2) -> true;
is_bif(erlang, setelement, 3) -> true;
is_bif(erlang, size, 1) -> true;
is_bif(erlang, spawn, 1) -> true;
is_bif(erlang, spawn, 2) -> true;
is_bif(erlang, spawn, 3) -> true;
is_bif(erlang, spawn, 4) -> true;
is_bif(erlang, spawn_link, 1) -> true;
is_bif(erlang, spawn_link, 2) -> true;
is_bif(erlang, spawn_link, 3) -> true;
is_bif(erlang, spawn_link, 4) -> true;
is_bif(erlang, spawn_opt, 4) -> true;
is_bif(erlang, split_binary, 2) -> true;
is_bif(erlang, start_timer, 3) -> true;
is_bif(erlang, statistics, 1) -> true;
is_bif(erlang, suspend_process, 1) -> true;
is_bif(erlang, system_flag, 2) -> true;
is_bif(erlang, system_info, 1) -> true;
is_bif(erlang, term_to_binary, 1) -> true;
is_bif(erlang, term_to_binary, 2) -> true;
is_bif(erlang, throw, 1) -> true;
is_bif(erlang, time, 0) -> true;
is_bif(erlang, tl, 1) -> true;
is_bif(erlang, trace, 3) -> true;
is_bif(erlang, trace_info, 2) -> true;
is_bif(erlang, trace_pattern, 2) -> true;
is_bif(erlang, trace_pattern, 3) -> true;
is_bif(erlang, trunc, 1) -> true;
is_bif(erlang, tuple_to_list, 1) -> true;
is_bif(erlang, universaltime, 0) -> true;
is_bif(erlang, universaltime_to_localtime, 1) -> true;
is_bif(erlang, unlink, 1) -> true;
is_bif(erlang, unregister, 1) -> true;
is_bif(erlang, whereis, 1) -> true;
is_bif(erlang, yield, 0) -> true;
is_bif(lists, append, 2) -> true;
is_bif(lists, reverse, 1) -> true;
is_bif(lists, reverse, 2) -> true;
is_bif(lists, subtract, 2) -> true;
is_bif(math, acos, 1) -> true;
is_bif(math, acosh, 1) -> true;
is_bif(math, asin, 1) -> true;
is_bif(math, asinh, 1) -> true;
is_bif(math, atan, 1) -> true;
is_bif(math, atan2, 2) -> true;
is_bif(math, atanh, 1) -> true;
is_bif(math, cos, 1) -> true;
is_bif(math, cosh, 1) -> true;
is_bif(math, erf, 1) -> true;
is_bif(math, erfc, 1) -> true;
is_bif(math, exp, 1) -> true;
is_bif(math, log, 1) -> true;
is_bif(math, log10, 1) -> true;
is_bif(math, pow, 2) -> true;
is_bif(math, sin, 1) -> true;
is_bif(math, sinh, 1) -> true;
is_bif(math, sqrt, 1) -> true;
is_bif(math, tan, 1) -> true;
is_bif(math, tanh, 1) -> true;
is_bif(_, _, _) -> false.


%% =====================================================================
%% is_guard_bif(Module, Name, Arity) -> boolean()
%%
%%	    Module = Name = atom()
%%	    Arity = integer()
%%
%%	Returns `true' if the built-in function `Module:Name/Arity' may
%%	be called from a clause guard. Note that such "guard BIFs" are
%%	not necessarily "pure", since some (notably `erlang:self/0') may
%%	depend on the current state, nor "safe", since many guard BIFs
%%	can fail. Also note that even a "pure" function could be
%%	unsuitable for calling from a guard because of its time or space
%%	complexity.

is_guard_bif(erlang, '*', 2) -> true;
is_guard_bif(erlang, '+', 1) -> true;
is_guard_bif(erlang, '+', 2) -> true;
is_guard_bif(erlang, '-', 1) -> true;
is_guard_bif(erlang, '-', 2) -> true;
is_guard_bif(erlang, '/', 2) -> true;
is_guard_bif(erlang, '/=', 2) -> true;
is_guard_bif(erlang, '<', 2) -> true;
is_guard_bif(erlang, '=/=', 2) -> true;
is_guard_bif(erlang, '=:=', 2) -> true;
is_guard_bif(erlang, '=<', 2) -> true;
is_guard_bif(erlang, '==', 2) -> true;
is_guard_bif(erlang, '>', 2) -> true;
is_guard_bif(erlang, '>=', 2) -> true;
is_guard_bif(erlang, 'and', 2) -> true;
is_guard_bif(erlang, 'band', 2) -> true;
is_guard_bif(erlang, 'bnot', 1) -> true;
is_guard_bif(erlang, 'bor', 2) -> true;
is_guard_bif(erlang, 'bsl', 2) -> true;
is_guard_bif(erlang, 'bsr', 2) -> true;
is_guard_bif(erlang, 'bxor', 2) -> true;
is_guard_bif(erlang, 'div', 2) -> true;
is_guard_bif(erlang, 'not', 1) -> true;
is_guard_bif(erlang, 'or', 2) -> true;
is_guard_bif(erlang, 'rem', 2) -> true;
is_guard_bif(erlang, 'xor', 2) -> true;
is_guard_bif(erlang, abs, 1) -> true;
is_guard_bif(erlang, element, 2) -> true;
is_guard_bif(erlang, error, 1) -> true;  % unorthodox
is_guard_bif(erlang, exit, 1) -> true;  % unorthodox
is_guard_bif(erlang, fault, 1) -> true;  % unorthodox
is_guard_bif(erlang, float, 1) -> true;  % (the type coercion function)
is_guard_bif(erlang, hd, 1) -> true;
is_guard_bif(erlang, is_atom, 1) -> true;
is_guard_bif(erlang, is_boolean, 1) -> true;
is_guard_bif(erlang, is_binary, 1) -> true;
is_guard_bif(erlang, is_constant, 1) -> true;
is_guard_bif(erlang, is_float, 1) -> true;
is_guard_bif(erlang, is_function, 1) -> true;
is_guard_bif(erlang, is_integer, 1) -> true;
is_guard_bif(erlang, is_list, 1) -> true;
is_guard_bif(erlang, is_number, 1) -> true;
is_guard_bif(erlang, is_pid, 1) -> true;
is_guard_bif(erlang, is_port, 1) -> true;
is_guard_bif(erlang, is_reference, 1) -> true;
is_guard_bif(erlang, is_tuple, 1) -> true;
is_guard_bif(erlang, length, 1) -> true;
is_guard_bif(erlang, list_to_atom, 1) -> true;  % unorthodox
is_guard_bif(erlang, node, 0) -> true;  % (not pure)
is_guard_bif(erlang, node, 1) -> true;  % (not pure)
is_guard_bif(erlang, round, 1) -> true;
is_guard_bif(erlang, self, 0) -> true;  % (not pure)
is_guard_bif(erlang, size, 1) -> true;
is_guard_bif(erlang, throw, 1) -> true;  % unorthodox
is_guard_bif(erlang, tl, 1) -> true;
is_guard_bif(erlang, trunc, 1) -> true;
is_guard_bif(math, acos, 1) -> true;  % unorthodox
is_guard_bif(math, acosh, 1) -> true;  % unorthodox
is_guard_bif(math, asin, 1) -> true;  % unorthodox
is_guard_bif(math, asinh, 1) -> true;  % unorthodox
is_guard_bif(math, atan, 1) -> true;  % unorthodox
is_guard_bif(math, atan2, 2) -> true;  % unorthodox
is_guard_bif(math, atanh, 1) -> true;  % unorthodox
is_guard_bif(math, cos, 1) -> true;  % unorthodox
is_guard_bif(math, cosh, 1) -> true;  % unorthodox
is_guard_bif(math, erf, 1) -> true;  % unorthodox
is_guard_bif(math, erfc, 1) -> true;  % unorthodox
is_guard_bif(math, exp, 1) -> true;  % unorthodox
is_guard_bif(math, log, 1) -> true;  % unorthodox
is_guard_bif(math, log10, 1) -> true;  % unorthodox
is_guard_bif(math, pow, 2) -> true;  % unorthodox
is_guard_bif(math, sin, 1) -> true;  % unorthodox
is_guard_bif(math, sinh, 1) -> true;  % unorthodox
is_guard_bif(math, sqrt, 1) -> true;  % unorthodox
is_guard_bif(math, tan, 1) -> true;  % unorthodox
is_guard_bif(math, tanh, 1) -> true;  % unorthodox
is_guard_bif(_, _, _) -> false.


%% =====================================================================
%% is_pure(Module, Name, Arity) -> boolean()
%%
%%	    Module = Name = atom()
%%	    Arity = integer()
%%
%%	Returns `true' if the function `Module:Name/Arity' does not
%%	affect the state, nor depend on the state, although its
%%	evaluation is not guaranteed to complete normally for all input.

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
is_pure(erlang, atom_to_list, 1) -> true;
is_pure(erlang, binary_to_list, 1) -> true;
is_pure(erlang, binary_to_list, 3) -> true;
is_pure(erlang, concat_binary, 1) -> true;
is_pure(erlang, element, 2) -> true;
is_pure(erlang, float, 1) -> true;
is_pure(erlang, float_to_list, 1) -> true;
is_pure(erlang, hash, 2) -> false;
is_pure(erlang, hd, 1) -> true;
is_pure(erlang, integer_to_list, 1) -> true;
is_pure(erlang, is_atom, 1) -> true;
is_pure(erlang, is_boolean, 1) -> true;
is_pure(erlang, is_binary, 1) -> true;
is_pure(erlang, is_builtin, 3) -> true;
is_pure(erlang, is_constant, 1) -> true;
is_pure(erlang, is_float, 1) -> true;
is_pure(erlang, is_function, 1) -> true;
is_pure(erlang, is_integer, 1) -> true;
is_pure(erlang, is_list, 1) -> true;
is_pure(erlang, is_number, 1) -> true;
is_pure(erlang, is_pid, 1) -> true;
is_pure(erlang, is_port, 1) -> true;
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
is_pure(erlang, phash, 2) -> false;
is_pure(erlang, pid_to_list, 1) -> true;
is_pure(erlang, round, 1) -> true;
is_pure(erlang, setelement, 3) -> true;
is_pure(erlang, size, 1) -> true;
is_pure(erlang, split_binary, 2) -> true;
is_pure(erlang, term_to_binary, 1) -> true;
is_pure(erlang, tl, 1) -> true;
is_pure(erlang, trunc, 1) -> true;
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
is_pure(math, cos, 1) -> true;
is_pure(math, cosh, 1) -> true;
is_pure(math, erf, 1) -> true;
is_pure(math, erfc, 1) -> true;
is_pure(math, exp, 1) -> true;
is_pure(math, log, 1) -> true;
is_pure(math, log10, 1) -> true;
is_pure(math, pow, 2) -> true;
is_pure(math, sin, 1) -> true;
is_pure(math, sinh, 1) -> true;
is_pure(math, sqrt, 1) -> true;
is_pure(math, tan, 1) -> true;
is_pure(math, tanh, 1) -> true;
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
is_safe(erlang, is_constant, 1) -> true;
is_safe(erlang, is_float, 1) -> true;
is_safe(erlang, is_function, 1) -> true;
is_safe(erlang, is_integer, 1) -> true;
is_safe(erlang, is_list, 1) -> true;
is_safe(erlang, is_number, 1) -> true;
is_safe(erlang, is_pid, 1) -> true;
is_safe(erlang, is_port, 1) -> true;
is_safe(erlang, is_record, 3) -> true;
is_safe(erlang, is_reference, 1) -> true;
is_safe(erlang, is_tuple, 1) -> true;
is_safe(erlang, make_ref, 0) -> true;
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
