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
# Implementation examples

## A simple Media Gateway Controller

In megaco/examples/simple/megaco_simple_mgc.erl there is an example of a simple
MGC that listens on both text and binary standard ports and is prepared to
handle a Service Change Request message to arrive either via TCP/IP or UDP/IP.
Messages received on the text port are decoded using a text decoder and messages
received on the binary port are decoded using a binary decoder.

The Service Change Reply is encoded in the same way as the request and sent back
to the MG with the same transport mechanism UDP/IP or TCP/IP.

After this initial service change message the connection between the MG and MGC
is fully established and supervised.

The MGC, with its four listeners, may be started with:

```text
      cd megaco/examples/simple
      erl -pa ../../../megaco/ebin -s megaco_filter -s megaco
      megaco_simple_mgc:start().
```

or simply 'gmake mgc'.

The -s megaco_filter option to erl implies, the event tracing mechanism to be
enabled and an interactive sequence chart tool to be started. This may be quite
useful in order to visualize how your MGC interacts with the Megaco/H.248
protocol stack.

The event traces may alternatively be directed to a file for later analyze. By
default the event tracing is disabled, but it may dynamically be enabled without
any need for re-compilation of the code.

## A simple Media Gateway

In megaco/examples/simple/megaco_simple_mg.erl there is an example of a simple
MG that connects to an MGC, sends a Service Change Request and waits
synchronously for a reply.

After this initial service change message the connection between the MG and MGC
is fully established and supervised.

Assuming that the MGC is started on the local host, four different MG's, using
text over TCP/IP, binary over TCP/IP, text over UDP/IP and binary over UDP/IP
may be started on the same Erlang node with:

```text
      cd megaco/examples/simple
      erl -pa ../../../megaco/ebin -s megaco_filter -s megaco
      megaco_simple_mg:start().
```

or simply 'gmake mg'.

If you "only" want to start a single MG which tries to connect an MG on a host
named "baidarka", you may use one of these functions (instead of the
megaco_simple_mg:start/0 above):

```erlang
      megaco_simple_mg:start_tcp_text("baidarka", []).
      megaco_simple_mg:start_tcp_binary("baidarka", []).
      megaco_simple_mg:start_udp_text("baidarka", []).
      megaco_simple_mg:start_udp_binary("baidarka", []).
```

The -s megaco_filter option to erl implies, the event tracing mechanism to be
enabled and an interactive sequence chart tool to be started. This may be quite
useful in order to visualize how your MG interacts with the Megaco/H.248
protocol stack.

The event traces may alternatively be directed to a file for later analyze. By
default the event tracing is disabled, but it may dynamically be enabled without
any need for re-compilation of the code.
