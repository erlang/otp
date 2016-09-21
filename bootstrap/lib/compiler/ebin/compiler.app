% This is an -*- erlang -*- file.
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

{application, compiler,
 [{description, "ERTS  CXC 138 10"},
  {vsn, "7.0.1"},
  {modules, [
	     beam_a,
	     beam_asm,
	     beam_block,
	     beam_bool,
	     beam_bs,
	     beam_bsm,
	     beam_clean,
	     beam_dead,
	     beam_dict,
	     beam_disasm,
	     beam_except,
	     beam_flatten,
	     beam_jump,
	     beam_listing,
	     beam_opcodes,
	     beam_peep,
	     beam_receive,
	     beam_reorder,
	     beam_split,
	     beam_trim,
	     beam_type,
	     beam_utils,
	     beam_validator,
	     beam_z,
	     cerl,
	     cerl_clauses,
	     cerl_inline,
             cerl_sets,
	     cerl_trees,
	     compile,
	     core_scan,
	     core_lint,
	     core_parse,
	     core_pp,
	     core_lib,
	     erl_bifs,
	     rec_env,
	     sys_core_dsetel,
	     sys_core_fold,
	     sys_core_fold_lists,
	     sys_core_inline,
	     sys_pre_attributes,
	     sys_pre_expand,
	     v3_codegen,
	     v3_core,
	     v3_kernel,
	     v3_kernel_pp,
	     v3_life
	    ]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {env, []},
  {runtime_dependencies, ["stdlib-2.5","kernel-4.0","hipe-3.12","erts-7.0",
			  "crypto-3.6"]}]}.
