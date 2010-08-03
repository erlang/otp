% This is an -*- erlang -*- file.
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%

{application, compiler,
 [{description, "ERTS  CXC 138 10"},
  {vsn, "4.7"},
  {modules, [
	     beam_asm,
	     beam_block,
	     beam_bool,
	     beam_bsm,
	     beam_clean,
	     beam_dead,
	     beam_dict,
	     beam_disasm,
	     beam_flatten,
	     beam_jump,
	     beam_listing,
	     beam_opcodes,
	     beam_peep,
	     beam_receive,
	     beam_trim,
	     beam_type,
	     beam_utils,
	     beam_validator,
	     cerl,
	     cerl_clauses,
	     cerl_inline,
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
	     sys_core_inline,
	     sys_expand_pmod,
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
  {env, []}]}.
