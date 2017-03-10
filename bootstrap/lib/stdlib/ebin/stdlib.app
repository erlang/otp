%% This is an -*- erlang -*- file.
%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
{application, stdlib,
 [{description, "ERTS  CXC 138 10"},
  {vsn, "3.2"},
  {modules, [array,
	     base64,
	     beam_lib,
	     binary,
	     c,
	     calendar,
	     dets,
             dets_server,
	     dets_sup,
	     dets_utils,
	     dets_v8,
	     dets_v9,
	     dict,
	     digraph,
	     digraph_utils,
	     edlin,
	     edlin_expand,
	     epp,
	     eval_bits,
             erl_anno,
	     erl_bits,
	     erl_compile,
	     erl_eval,
             erl_expand_records,
	     erl_internal,
	     erl_lint,
	     erl_parse,
	     erl_posix_msg,
	     erl_pp,
	     erl_scan,
	     erl_tar,
	     error_logger_file_h,
	     error_logger_tty_h,
	     escript,
	     ets,
	     file_sorter,
	     filelib,
	     filename,
	     gb_trees,
	     gb_sets,
	     gen,
	     gen_event,
	     gen_fsm,
	     gen_server,
	     gen_statem,
	     io,
	     io_lib,
	     io_lib_format,
	     io_lib_fread,
	     io_lib_pretty,
	     lib,
	     lists,
	     log_mf_h,
	     maps,
	     math,
	     ms_transform,
	     orddict,
	     ordsets,
	     otp_internal,
	     pool,
	     proc_lib,
	     proplists,
             qlc,
             qlc_pt,
	     queue,
	     rand,
	     random,
	     re,
	     sets,
	     shell,
	     shell_default,
	     slave,
	     sofs,
	     string,
	     supervisor,
	     supervisor_bridge,
	     sys,
	     timer,
	     unicode,
	     win32reg,
	     zip]},
  {registered,[timer_server,rsh_starter,take_over_monitor,pool_master,
               dets]},
  {applications, [kernel]},
  {env, []},
  {runtime_dependencies, ["sasl-3.0","kernel-5.0","erts-8.0","crypto-3.3",
			  "compiler-5.0"]}
]}.

