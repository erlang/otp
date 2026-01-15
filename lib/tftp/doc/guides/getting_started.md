<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2023-2025. All Rights Reserved.

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
# Examples


The [start/1](`tftp:start/1`) function starts a daemon process listening for UDP
packets on a port. When it receives a request for read or write, it spawns a
temporary server process handling the transfer.

This is a simple example of starting the TFTP server and reading the content of
a sample file using the TFTP client.

_Step 1._ Create a sample file to be used for the transfer:

```text
      $ echo "Erlang/OTP 21" > file.txt
```

_Step 2._ Start the TFTP server:

```erlang
      1> {ok, Pid} = tftp:start([{port, 19999}]).
      {ok,<0.65.0>}
```

_Step 3._ Start the TFTP client (in another shell):

```erlang
      1> tftp:read_file("file.txt", binary, [{port, 19999}]).
      {ok,<<"Erlang/OTP 21\n">>}
```
