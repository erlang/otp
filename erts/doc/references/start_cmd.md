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
# start

OTP start script example for Unix.

## Description

The `start` script is an example script on how to start up the Erlang system in
embedded mode on Unix.

For more information about the use, see the
[Embedded System User's Guide](`e:system:embedded_solaris.md`) in System
Documentation.

```text
start [ data_file ]
```

Argument:

- **`data_file`** - Optional. Specifies what `start_erl.data` file to use.

Environment variable `RELDIR` can be set before calling this example, which sets
the directory where to find the release files.

## See Also

[`run_erl(1)`](run_erl_cmd.md), [`start_erl(1)`](start_erl_cmd.md)
