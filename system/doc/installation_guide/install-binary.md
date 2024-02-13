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
# Installing the Binary Release

## Windows

The system is delivered as a Windows Installer executable. Get it from
[https://erlang.org/downloads](https://erlang.org/downloads).

### Installing

The installation procedure is automated. Double-click the `.exe` file icon and
follow the instructions.

### Verifying

- Start Erlang/OTP by double-clicking on the Erlang shortcut icon on the
  desktop.

  Expect a command-line window to pop up with an output looking something like
  this:

  ```text
    Erlang/OTP 17 [erts-6.0] [64-bit] [smp:2:2]

    Eshell V6.0  (abort with ^G)
    1>
  ```

- Exit by entering the command `halt/0`.

  ```text
    2> halt().
  ```

  This closes the Erlang/OTP shell.
