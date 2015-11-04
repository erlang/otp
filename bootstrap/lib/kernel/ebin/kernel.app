%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2015. All Rights Reserved.
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
%% This is an -*- erlang -*- file.
%%
{application, kernel,
 [
  {description, "ERTS  CXC 138 10"},
  {vsn, "4.1"},
  {modules, [application,
	     application_controller,
	     application_master,
	     application_starter,
	     auth,
	     code,
	     code_server,
	     dist_util,
	     erl_boot_server,
	     erl_distribution,
	     erl_reply,
	     error_handler,
	     error_logger,
	     file,
             file_server,
             file_io_server,
	     global,
	     global_group,
	     global_search,
	     group,
	     heart,
	     hipe_unified_loader,
	     inet6_tcp,
	     inet6_tcp_dist,
	     inet6_udp,
	     inet6_sctp,
	     inet_config,
	     inet_hosts,
	     inet_gethost_native,
	     inet_tcp_dist,
	     kernel,
	     kernel_config,
	     net,
	     net_adm,
	     net_kernel,
	     os,
	     ram_file,
	     rpc,
	     user,
	     user_drv,
	     user_sup,
             disk_log,
             disk_log_1,
             disk_log_server,
             disk_log_sup,
             dist_ac,
             erl_ddll,
             erl_epmd,
	     erts_debug,
             gen_tcp,
             gen_udp,
	     gen_sctp,
             inet,
             inet_db,
             inet_dns,
             inet_parse,
             inet_res,
             inet_tcp,
             inet_udp,
	     inet_sctp,
             pg2,
	     seq_trace,
	     standard_error,
	     wrap_log_reader]},
  {registered, [application_controller,
		erl_reply,
		auth,
		boot_server,
		code_server,
		disk_log_server,
		disk_log_sup,
		erl_prim_loader,
		error_logger,
		file_server_2,
		fixtable_server,
		global_group,
		global_name_server,
		heart,
		init,
		kernel_config,
		kernel_sup,
		net_kernel,
		net_sup,
		rex,
		user,
	        os_server,
                ddll_server,
                erl_epmd,
                inet_db,
                pg2]},
  {applications, []},
  {env, [{error_logger, tty}]},
  {mod, {kernel, []}},
  {runtime_dependencies, ["erts-7.0", "stdlib-2.6", "sasl-2.6"]}
 ]
}.
