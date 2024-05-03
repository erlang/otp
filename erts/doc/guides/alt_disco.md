<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# How to Implement an Alternative Node Discovery for Erlang Distribution

This section describes how to implement an alternative node discovery mechanism
for Erlang distribution. Node discovery is normally done using DNS and the
Erlang Port Mapper Daemon (EPMD) for port registration and lookup.

> #### Note {: .info }
>
> Support for alternative node discovery mechanisms was added in Erlang/OTP 21.

## Introduction

To implement your own node discovery module you have to write your own EPMD
module. The [EPMD module](`m:erl_epmd`) is responsible for providing the
location of another node. The distribution modules
(`inet_tcp_dist`/`inet_tls_dist`) call the EPMD module to get the IP address and
port of the other node. The EPMD module that is part of Erlang/OTP will resolve
the hostname using DNS and uses the EPMD unix process to get the port of another
node. The EPMD unix process does this by connecting to the other node on a
well-known port, port 4369.

## Discovery module

The discovery module needs to implement the same API as the regular
[EPMD module](`m:erl_epmd`). However, instead of communicating with EPMD you can
connect to any service to find out connection details of other nodes. A
discovery module is enabled by setting [\-epmd_module](erl_cmd.md) when starting
erlang. The discovery module must implement the following callbacks:

- **[start_link/0](`erl_epmd:start_link/0`)** - Start any processes needed by
  the discovery module.

- **[names/1](`erl_epmd:names/1`)** - Return node names held by the registrar
  for the given host.

- **[register_node/2](`erl_epmd:register_node/2`)** - Register the given node
  name with the registrar.

- **[port_please/3](`erl_epmd:port_please/3`)** - Return the distribution port
  used by the given node.

The discovery module may implement the following callback:

- **[address_please/3](`erl_epmd:address_please/3`)** - Return the address of
  the given node. If not implemented, `erl_epmd:address_please/3` will be used
  instead.

  This callback may also return the port of the given node. In that case
  [port_please/3](`erl_epmd:port_please/3`) may be omitted.

- **[listen_port_please/2](`erl_epmd:listen_port_please/2`)** - Return the port
  the local node should listen to. If not implemented,
  `erl_epmd:listen_port_please/2` will be used instead.
