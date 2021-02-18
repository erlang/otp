%%% This is an -*- Erlang -*- file.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------
%%% File    : dialyzer.hrl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%%           Kostis Sagonas <kostis@it.uu.se>
%%% Description : Header file for Dialyzer.
%%%
%%% Created : 1 Oct 2004 by Kostis Sagonas <kostis@it.uu.se>
%%%-------------------------------------------------------------------

-define(RET_NOTHING_SUSPICIOUS, 0).
-define(RET_INTERNAL_ERROR, 1).
-define(RET_DISCREPANCIES, 2).

-type dial_ret() :: ?RET_NOTHING_SUSPICIOUS
                  | ?RET_INTERNAL_ERROR
                  | ?RET_DISCREPANCIES.

%%--------------------------------------------------------------------
%% Warning classification
%%--------------------------------------------------------------------

-define(WARN_RETURN_NO_RETURN, warn_return_no_exit).
-define(WARN_RETURN_ONLY_EXIT, warn_return_only_exit).
-define(WARN_NOT_CALLED, warn_not_called).
-define(WARN_NON_PROPER_LIST, warn_non_proper_list).
-define(WARN_FUN_APP, warn_fun_app).
-define(WARN_MATCHING, warn_matching).
-define(WARN_OPAQUE, warn_opaque).
-define(WARN_FAILING_CALL, warn_failing_call).
-define(WARN_BIN_CONSTRUCTION, warn_bin_construction).
-define(WARN_CONTRACT_TYPES, warn_contract_types).
-define(WARN_CONTRACT_SYNTAX, warn_contract_syntax).
-define(WARN_CONTRACT_NOT_EQUAL, warn_contract_not_equal).
-define(WARN_CONTRACT_SUBTYPE, warn_contract_subtype).
-define(WARN_CONTRACT_SUPERTYPE, warn_contract_supertype).
-define(WARN_CONTRACT_RANGE, warn_contract_range).
-define(WARN_CALLGRAPH, warn_callgraph).
-define(WARN_UNMATCHED_RETURN, warn_umatched_return).
-define(WARN_RACE_CONDITION, warn_race_condition).
-define(WARN_BEHAVIOUR, warn_behaviour).
-define(WARN_UNDEFINED_CALLBACK, warn_undefined_callbacks).
-define(WARN_UNKNOWN, warn_unknown).
-define(WARN_MAP_CONSTRUCTION, warn_map_construction).

%%
%% The following type has double role:
%%   1. It is the set of warnings that will be collected.
%%   2. It is also the set of tags for warnings that will be returned.
%%
-type dial_warn_tag() :: ?WARN_BEHAVIOUR | ?WARN_BIN_CONSTRUCTION
                       | ?WARN_CALLGRAPH | ?WARN_CONTRACT_NOT_EQUAL
                       | ?WARN_CONTRACT_RANGE | ?WARN_CONTRACT_SUBTYPE
                       | ?WARN_CONTRACT_SUPERTYPE | ?WARN_CONTRACT_SYNTAX
                       | ?WARN_CONTRACT_TYPES | ?WARN_FAILING_CALL
                       | ?WARN_FUN_APP | ?WARN_MAP_CONSTRUCTION
                       | ?WARN_MATCHING | ?WARN_NON_PROPER_LIST
                       | ?WARN_NOT_CALLED | ?WARN_OPAQUE
                       | ?WARN_RACE_CONDITION | ?WARN_RETURN_NO_RETURN
                       | ?WARN_RETURN_ONLY_EXIT | ?WARN_UNDEFINED_CALLBACK
                       | ?WARN_UNKNOWN | ?WARN_UNMATCHED_RETURN.

%%
%% This is the representation of each warning as they will be returned
%% to dialyzer's callers
%%
-type file_location() :: {File :: file:filename(),
                          Location :: erl_anno:location()}.
-type dial_warning() :: {Tag :: dial_warn_tag(),
                         Id :: file_location(),
                         Msg :: {atom(), [term()]}}.

%%
%% This is the representation of each warning before suppressions have
%% been applied
%%
-type m_or_mfa()     :: module() % warnings not associated with any function
                      | mfa().
-type warning_info() :: {File :: file:filename(),
                         Location :: erl_anno:location(),
                         ModuleOrMFA :: m_or_mfa()}.
-type raw_warning()  :: {Tag :: dial_warn_tag(),
                         Id :: warning_info(),
                         Msg :: {atom(), [term()]}}.

%%
%% This is the representation of dialyzer's internal errors
%%
-type dial_error()   :: any().    %% XXX: underspecified

%%--------------------------------------------------------------------
%% Basic types used either in the record definitions below or in other
%% parts of the application
%%--------------------------------------------------------------------

-type anal_type()     :: 'succ_typings' | 'plt_build'.
-type anal_type1()    :: anal_type() | 'plt_add' | 'plt_check' | 'plt_remove'.
-type contr_constr()  :: {'subtype', erl_types:erl_type(), erl_types:erl_type()}.
-type contract_pair() :: {erl_types:erl_type(), [contr_constr()]}.
-type dial_define()   :: {atom(), term()}.
-type warn_option()   :: 'error_handling'
                       | 'no_behaviours'
                       | 'no_contracts'
                       | 'no_fail_call'
                       | 'no_fun_app'
                       | 'no_improper_lists'
                       | 'no_match'
                       | 'no_missing_calls'
                       | 'no_opaque'
                       | 'no_return'
                       | 'no_undefined_callbacks'
                       | 'no_underspecs'
                       | 'no_unused'
                       | 'race_conditions'
                       | 'underspecs'
                       | 'unknown'
                       | 'unmatched_returns'
                       | 'overspecs'
                       | 'specdiffs'.
-type dial_option()   :: {'files', [FileName :: file:filename()]}
                       | {'files_rec', [DirName :: file:filename()]}
                       | {'defines', [{Macro :: atom(), Value :: term()}]}
                       | {'from', 'src_code' | 'byte_code'}
                       | {'init_plt', FileName :: file:filename()}
                       | {'plts', [FileName :: file:filename()]}
                       | {'include_dirs', [DirName :: file:filename()]}
                       | {'output_file', FileName :: file:filename()}
                       | {'output_plt', FileName :: file:filename()}
                       | {'check_plt', boolean()}
                       | {'analysis_type', 'succ_typings' |
                                           'plt_add' |
                                           'plt_build' |
                                           'plt_check' |
                                           'plt_remove'}
                       | {'warnings', [warn_option()]}
                       | {'get_warnings', boolean()}
                       | {'error_location', error_location()}.
-type dial_options()  :: [dial_option()].
-type filename_opt()  :: 'basename' | 'fullpath'.
-type error_location():: 'column' | 'line'.
-type format()        :: 'formatted' | 'raw'.
-type iopt()          :: boolean().
-type label()	      :: non_neg_integer().
-type dial_warn_tags():: ordsets:ordset(dial_warn_tag()).
-type rep_mode()      :: 'quiet' | 'normal' | 'verbose'.
-type start_from()    :: 'byte_code' | 'src_code'.
-type mfa_or_funlbl() :: label() | mfa().
-type solver()        :: 'v1' | 'v2'.

%%--------------------------------------------------------------------
%% Record declarations used by various files
%%--------------------------------------------------------------------

-define(INDENT_OPT, true).
-define(ERROR_LOCATION, column).

-type doc_plt() :: 'undefined' | dialyzer_plt:plt().

-record(analysis, {analysis_pid			   :: pid() | 'undefined',
		   type		  = succ_typings   :: anal_type(),
		   defines	  = []		   :: [dial_define()],
		   doc_plt                         :: doc_plt(),
		   files          = []		   :: [file:filename()],
		   include_dirs	  = []		   :: [file:filename()],
		   start_from     = byte_code	   :: start_from(),
		   plt                             :: dialyzer_plt:plt(),
		   use_contracts  = true           :: boolean(),
		   race_detection = false	   :: boolean(),
		   behaviours_chk = false          :: boolean(),
		   timing         = false          :: boolean() | 'debug',
		   timing_server  = none           :: dialyzer_timing:timing_server(),
		   callgraph_file = ""             :: file:filename(),
                   solvers                         :: [solver()]}).

-record(options, {files           = []		   :: [file:filename()],
		  files_rec       = []		   :: [file:filename()],
		  analysis_type   = succ_typings   :: anal_type1(),
		  timing          = false          :: boolean() | 'debug',
		  defines         = []		   :: [dial_define()],
		  from            = byte_code	   :: start_from(),
		  get_warnings    = maybe          :: boolean() | 'maybe',
		  init_plts       = []	           :: [file:filename()],
		  include_dirs    = []		   :: [file:filename()],
		  output_plt      = none           :: 'none' | file:filename(),
		  legal_warnings  = ordsets:new()  :: dial_warn_tags(),
		  report_mode     = normal	   :: rep_mode(),
		  erlang_mode     = false	   :: boolean(),
		  use_contracts   = true           :: boolean(),
		  output_file     = none	   :: 'none' | file:filename(),
		  output_format   = formatted      :: format(),
		  filename_opt	  = basename       :: filename_opt(),
                  indent_opt      = ?INDENT_OPT    :: iopt(),
		  callgraph_file  = ""             :: file:filename(),
		  check_plt       = true           :: boolean(),
                  error_location  = ?ERROR_LOCATION :: error_location(),
                  solvers         = []             :: [solver()]}).

-record(contract, {contracts	  = []		   :: [contract_pair()],
		   args		  = []		   :: [erl_types:erl_type()],
		   forms	  = []		   :: [{_, _}]}).

%%--------------------------------------------------------------------

-define(timing(Server, Msg, Var, Expr),
	begin
	    dialyzer_timing:start_stamp(Server, Msg),
	    Var = Expr,
	    dialyzer_timing:end_stamp(Server),
	    Var
	end).
-define(timing(Server, Msg, Expr), ?timing(Server, Msg, _T, Expr)).
