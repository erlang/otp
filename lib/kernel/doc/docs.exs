# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2024-2025. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# %CopyrightEnd%
[
  groups_for_modules: [
    "Code & System": [
      :application,
      :code,
      :erl_ddll,
      :erl_debugger,
      :error_handler,
      :heart,
      :os
    ],
    Distribution: [
      :net_adm,
      :net_kernel,
      :auth,
      :erl_boot_server,
      :erl_epmd,
      :erpc,
      :global,
      :global_group,
      :pg,
      :rpc
    ],
    "Files & Networking": [
      :file,
      :gen_tcp,
      :gen_udp,
      :gen_sctp,
      :socket,
      :inet,
      :inet_res,
      :net
    ],
    Logging: [
      :logger,
      :disk_log,
      :error_logger,
      :logger_disk_log_h,
      :logger_filters,
      :logger_formatter,
      :logger_handler,
      :logger_std_h,
      :wrap_log_reader
    ],
    Tracing: [
      :seq_trace,
      :trace
    ]
  ],
  skip_code_autolink_to: ["t:file_descriptor/0"],
  ## The order of these items determine
  ## how they are listed in the docs
  extras: [
    "guides/introduction_chapter.md",
    "guides/socket_usage.md",
    "guides/logger_chapter.md",
    "guides/logger_cookbook.md",
    "guides/eep48_chapter.md",
    "references/app.md",
    "references/config.md",
    "kernel_app.md"
  ]
]
