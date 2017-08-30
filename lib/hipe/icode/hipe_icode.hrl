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
%%=====================================================================
%%
%% Contains type and record definitions for all Icode data structures.
%%
%%=====================================================================

%%---------------------------------------------------------------------
%% Include files needed for the compilation of this header file
%%---------------------------------------------------------------------

-include("../misc/hipe_consttab.hrl").

%%---------------------------------------------------------------------
%% Icode argument types
%%---------------------------------------------------------------------

-type simple_const()     :: atom() | [] | integer() | float().
-type structured_const() :: list() | tuple().

-type icode_lbl() :: non_neg_integer().

%%---------------------------------------------------------------------
%% Icode records
%%---------------------------------------------------------------------

-record(flat, {value :: simple_const() | structured_const() | binary()}).

-record(icode_const, {value :: #flat{}}).

-type variable_annotation() :: {atom(), any(), fun((any()) -> string())}.

-record(icode_variable, {name :: non_neg_integer(), 
			 kind :: 'var' | 'reg' | 'fvar',
			 annotation = [] :: [] | variable_annotation()}). 

%%---------------------------------------------------------------------
%% Type declarations for Icode instructions
%%---------------------------------------------------------------------

-type icode_if_op()  :: '>' | '<' | '>=' | '=<' | '=:=' | '=/=' | '==' | '/='
                      | 'fixnum_eq' | 'fixnum_neq' | 'fixnum_lt'
                      | 'fixnum_le' | 'fixnum_ge' | 'fixnum_gt' 
                      | 'op_exact_eqeq_2' | 'suspend_msg_timeout'.

-type icode_type_test()	:: 'atom' | 'bignum' | 'binary' | 'bitstr' | 'boolean'
                         | 'cons' | 'fixnum' | 'float'  | 'function'
                         | 'function2' | 'integer' | 'list' | 'map' | 'nil'
                         | 'number' | 'pid' | 'port' | 'reference' | 'tuple'
                         | {'atom', atom()} | {'integer', integer()}
                         | {'record', atom(), non_neg_integer()}
			 | {'tuple', non_neg_integer()}.
 
-type icode_primop()	:: atom() | tuple(). % XXX: temporarily, I hope
-type icode_funcall()   :: mfa() | icode_primop().

-type icode_var()	:: #icode_variable{kind::'var'}.
-type icode_reg()	:: #icode_variable{kind::'reg'}.
-type icode_fvar()	:: #icode_variable{kind::'fvar'}.
-type icode_argument()	:: #icode_const{} | #icode_variable{}.
-type icode_term_arg()	:: icode_var() | #icode_const{}.

-type icode_switch_case() :: {#icode_const{}, icode_lbl()}.

-type icode_call_type()   :: 'local' | 'primop' | 'remote'.
-type icode_exit_class()  :: 'error' | 'exit' | 'rethrow' | 'throw'.

-type icode_comment_text() :: atom() | string().

-type icode_info() :: [{'arg_types', [erl_types:erl_type()]}].

%%---------------------------------------------------------------------
%% Icode instructions
%%---------------------------------------------------------------------

-record(icode_label, {name :: icode_lbl()}).

-record(icode_if, {op          :: icode_if_op(),
		   args        :: [icode_term_arg()],
		   true_label  :: icode_lbl(),
		   false_label :: icode_lbl(),
		   p           :: float()}).

-record(icode_switch_val, {term       :: icode_var(),
			   fail_label :: icode_lbl(),
			   length     :: non_neg_integer(),
			   cases      :: [icode_switch_case()]}).

-record(icode_switch_tuple_arity, {term       :: icode_var(),
				   fail_label :: icode_lbl(),
				   length     :: non_neg_integer(),
				   cases      :: [icode_switch_case()]}).

-record(icode_type, {test        :: icode_type_test(),
		     args        :: [icode_term_arg()],
		     true_label  :: icode_lbl(),
		     false_label :: icode_lbl(),
		     p           :: float()}).

-record(icode_goto, {label :: icode_lbl()}).

-record(icode_move, {dst :: #icode_variable{},
		     src :: #icode_variable{} | #icode_const{}}).

-record(icode_phi, {dst     :: #icode_variable{},
		    id      :: #icode_variable{},
		    arglist :: [{icode_lbl(), #icode_variable{}}]}).

-record(icode_call, {dstlist            :: [#icode_variable{}],
		     'fun'              :: icode_funcall(),
		     args               :: [icode_argument()],
		     type               :: icode_call_type(),
		     continuation       :: [] | icode_lbl(),
		     fail_label = []    :: [] | icode_lbl(),
		     in_guard   = false :: boolean()}).

-record(icode_enter, {'fun' :: icode_funcall(),
		      args  :: [icode_term_arg()],
		      type  :: icode_call_type()}).

-record(icode_return, {vars :: [icode_var()]}).

-record(icode_begin_try, {label :: icode_lbl(), successor :: icode_lbl()}).

-record(icode_end_try, {}).

-record(icode_begin_handler, {dstlist :: [icode_var()]}).

%% TODO: Remove [] from fail_label
-record(icode_fail, {class            :: icode_exit_class(),
		     args             :: [icode_term_arg()],
		     fail_label = []  :: [] | icode_lbl()}).

-record(icode_comment, {text :: icode_comment_text()}).

%%---------------------------------------------------------------------
%% Icode instructions
%%---------------------------------------------------------------------

-type icode_instr()  :: #icode_begin_handler{} | #icode_begin_try{}
		      | #icode_call{} | #icode_comment{} | #icode_end_try{}
		      | #icode_enter{} | #icode_fail{}
                      | #icode_goto{} | #icode_if{} | #icode_label{}
		      | #icode_move{} | #icode_phi{} | #icode_return{}
                      | #icode_switch_tuple_arity{} | #icode_switch_val{}
		      | #icode_type{}.
-type icode_instrs() :: [icode_instr()].

%%---------------------------------------------------------------------
%% The Icode data structure
%%---------------------------------------------------------------------

-record(icode, {'fun'		:: mfa(),
		params		:: hipe_icode:params(),
		%% TODO: merge is_closure and closure_arity into one field
		is_closure	:: boolean(),
		closure_arity = none	:: 'none' | arity(),
		is_leaf 	:: boolean(),
		code = []	:: icode_instrs(),
		data		:: hipe_consttab(),
		var_range	:: {non_neg_integer(), non_neg_integer()},
		label_range	:: {icode_lbl(), icode_lbl()},
		info = []       :: icode_info()}).
-type icode() :: #icode{}.

%%---------------------------------------------------------------------
