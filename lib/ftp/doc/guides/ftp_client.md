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
# FTP Client

## Getting Started

FTP clients are considered to be rather temporary. Thus, they are only started
and stopped during runtime and cannot be started at application startup. The FTP
client API is designed to allow some functions to return intermediate results.
This implies that only the process that started the FTP client can access it
with preserved sane semantics. If the process that started the FTP session dies,
the FTP client process terminates.

The client supports IPv6 as long as the underlying mechanisms also do so.

The following is a simple example of an FTP session, where the user `guest` with
password `password` logs on to the remote host `erlang.org`:

```erlang
      1> ftp:start().
      ok
      2> {ok, Pid} = ftp:open([{host, "erlang.org"}]).
      {ok,<0.22.0>}
      3> ftp:user(Pid, "guest", "password").
      ok
      4> ftp:pwd(Pid).
      {ok, "/home/guest"}
      5> ftp:cd(Pid, "appl/examples").
      ok
      6> ftp:lpwd(Pid).
      {ok, "/home/fred"}.
      7> ftp:lcd(Pid, "/home/eproj/examples").
      ok
      8> ftp:recv(Pid, "appl.erl").
      ok
      9> ftp:close(Pid).
      ok
      10> ftp:stop().
      ok
```

The file `appl.erl` is transferred from the remote to the local host. When the
session is opened, the current directory at the remote host is `/home/guest`,
and `/home/fred` at the local host. Before transferring the file, the current
local directory is changed to `/home/eproj/examples`, and the remote directory
is set to `/home/guest/appl/examples`.
