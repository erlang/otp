%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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

-module(ct_doctest_module_preproc_mod).
-moduledoc """
Test that module code blocks in doctests support preprocessor features
like macros and records via epp.
""".

-export([uses_module_macro/0, uses_define/0, uses_record/0]).

-doc """
Module code block using ?MODULE macro:

```erlang
-module(macro_mod).
-export([name/0]).
name() -> ?MODULE.
```

```erlang
1> macro_mod:name().
macro_mod
```
""".
uses_module_macro() ->
    ok.

-doc """
Module code block using -define macro:

```erlang
-module(define_mod).
-export([value/0]).
-define(VALUE, 42).
value() -> ?VALUE.
```

```erlang
1> define_mod:value().
42
```
""".
uses_define() ->
    ok.

-doc """
Module code block using -record:

```erlang
-module(record_mod).
-export([new/2, name/1, age/1]).
-record(person, {name, age}).
new(Name, Age) -> #person{name = Name, age = Age}.
name(#person{name = N}) -> N.
age(#person{age = A}) -> A.
```

```erlang
1> P = record_mod:new(alice, 30).
2> record_mod:name(P).
alice
3> record_mod:age(P).
30
```
""".
uses_record() ->
    ok.
