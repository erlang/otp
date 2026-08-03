<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
-->
### Archives

The following features for archives are deprecated:

* Using archives for packaging a single application or parts of a
  single application into an archive file that is included in the code
  path.

* All functionality to handle archives in module
  [`erl_prim_loader`](https://www.erlang.org/doc/man/erl_prim_loader).

* The `-code_path_choice` flag for `erl`.

Using a single archive file for holding BEAM files and other data
files in an Escript is **not** deprecated. However, to access files in
the archive the `escript:extract/2` function has to be used.

### erl flags

The following erl flags are deprecated:

* `-epmd_module Module` - deprecated in favour of the `kernel` application
  parameter `epmd_module`.

* `-erl_epmd_port Port` - deprecated in favour of the `kernel` application
  parameter `erl_epmd_node_listen_port`.
