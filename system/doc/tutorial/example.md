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
# Problem Example

## Description

A common interoperability situation is when you want to incorporate a piece of
code, solving a complex problem, in your Erlang program. Suppose for example,
that you have the following C functions that you would like to call from Erlang:

```text

/* complex.c */

int foo(int x) {
  return x+1;
}

int bar(int y) {
  return y*2;
}
```

The functions are deliberately kept as simple as possible, for readability
reasons.

From an Erlang perspective, it is preferable to be able to call `foo` and `bar`
without having to bother about that they are C functions:

```erlang
% Erlang code
...
Res = complex:foo(X),
...
```

Here, the communication with C is hidden in the implementation of `complex.erl`.
In the following sections, it is shown how this module can be implemented using
the different interoperability mechanisms.
