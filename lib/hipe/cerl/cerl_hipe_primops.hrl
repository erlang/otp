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
%% @copyright 2000 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @doc Predefined Core Erlang primitive operations used by HiPE.

%% These definitions give the names of Core Erlang primops recognized by
%% HiPE. Many of them (e.g., 'not'/'and'/'or', and the type tests), are
%% not primops on the Icode level, but are inline-expanded by the
%% translation from Core Erlang to Icode, or are renamed/rewritten to a
%% corresponding ICode primop; they only exist to help the translation.

%%-define(PRIMOP_IDENTITY, identity).		% arity 1
-define(PRIMOP_NOT, 'not').			% arity 1
-define(PRIMOP_AND, 'and').			% arity 2
-define(PRIMOP_OR, 'or').			% arity 2
-define(PRIMOP_XOR, 'xor').			% arity 2
-define(PRIMOP_ADD, '+').			% arity 2
-define(PRIMOP_SUB, '-').			% arity 2
-define(PRIMOP_NEG, neg).			% arity 1
-define(PRIMOP_MUL, '*').			% arity 2
-define(PRIMOP_DIV, '/').			% arity 2
-define(PRIMOP_INTDIV, 'div').			% arity 2
-define(PRIMOP_REM, 'rem').			% arity 2
-define(PRIMOP_BAND, 'band').			% arity 2
-define(PRIMOP_BOR, 'bor').			% arity 2
-define(PRIMOP_BXOR, 'bxor').			% arity 2
-define(PRIMOP_BNOT, 'bnot').			% arity 1
-define(PRIMOP_BSL, 'bsl').			% arity 2
-define(PRIMOP_BSR, 'bsr').			% arity 2
-define(PRIMOP_EQ, '==').			% arity 2
-define(PRIMOP_NE, '/=').			% arity 2
-define(PRIMOP_EXACT_EQ, '=:=').		% arity 2
-define(PRIMOP_EXACT_NE, '=/=').		% arity 2
-define(PRIMOP_LT, '<').			% arity 2
-define(PRIMOP_GT, '>').			% arity 2
-define(PRIMOP_LE, '=<').			% arity 2
-define(PRIMOP_GE, '>=').			% arity 2
-define(PRIMOP_IS_ATOM, 'is_atom').		% arity 1
-define(PRIMOP_IS_BIGNUM, 'is_bignum').		% arity 1
-define(PRIMOP_IS_BINARY, 'is_binary').		% arity 1
-define(PRIMOP_IS_FIXNUM, 'is_fixnum').		% arity 1
-define(PRIMOP_IS_FLOAT, 'is_float').		% arity 1
-define(PRIMOP_IS_FUNCTION, 'is_function').	% arity 1
-define(PRIMOP_IS_INTEGER, 'is_integer').	% arity 1
-define(PRIMOP_IS_LIST, 'is_list').		% arity 1
-define(PRIMOP_IS_NUMBER, 'is_number').		% arity 1
-define(PRIMOP_IS_PID, 'is_pid').		% arity 1
-define(PRIMOP_IS_PORT, 'is_port').		% arity 1
-define(PRIMOP_IS_REFERENCE, 'is_reference').	% arity 1
-define(PRIMOP_IS_TUPLE, 'is_tuple').		% arity 1
-define(PRIMOP_IS_RECORD, 'is_record').		% arity 3
-define(PRIMOP_EXIT, exit).			% arity 1
-define(PRIMOP_THROW, throw).			% arity 1
-define(PRIMOP_ERROR, error).			% arity 1,2
-define(PRIMOP_RETHROW, raise).			% arity 2
-define(PRIMOP_RECEIVE_SELECT, receive_select).	% arity 0
-define(PRIMOP_RECEIVE_NEXT, receive_next).	% arity 0
-define(PRIMOP_ELEMENT, element).		% arity 2
-define(PRIMOP_DSETELEMENT, dsetelement).	% arity 3
-define(PRIMOP_MAKE_FUN, make_fun).		% arity 6
-define(PRIMOP_APPLY_FUN, apply_fun).		% arity 2
-define(PRIMOP_FUN_ELEMENT, closure_element).	% arity 2
-define(PRIMOP_SET_LABEL, set_label).           % arity 1
-define(PRIMOP_GOTO_LABEL, goto_label).         % arity 1
-define(PRIMOP_REDUCTION_TEST, reduction_test). % arity 0
-define(PRIMOP_BS_CONTEXT_TO_BINARY, bs_context_to_binary). % arity 1
