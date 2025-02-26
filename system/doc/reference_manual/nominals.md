<!--
%CopyrightBegin%

Copyright Ericsson AB 2023. All Rights Reserved.

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
# Nominals

## Rationale and Syntax

For user-defined types defined with `-type`, their type names do not matter in
any Erlang type-checker. This means existing type systems for Erlang are all
structural type systems. Two types are seen as equivalent if their structures
are the same. Type comparison are based on the structures of the types, not on
how the user explicitly defines them. In the following example, `meter()` and
`foot()` are equivalent, and neither differs from the basic type `integer()`.

````
-type meter() :: integer().
-type foot() :: integer().
````

Nominal typing is an alternative type system implemented in Dialyzer, where two
types are equivalent if and only if they are declared with the same type name.
If `meter()` and `foot()` are defined as nominal types, they will no longer be
compatible. When a function expects type `meter()`, passing in type `foot()`
will result in an error.

````
-nominal meter() :: integer().
-nominal foot() :: integer().
````

The main use case of nominal types is to prevent accidental misuse of types with
the same structure. Type checking for nominal types is done in Dialyzer. The
compiler does not perform extra type-checking for nominals.

## Nominal Type-Checking Rules

In general, if two nominal types have different names, and one is not derived
from the other, then they are not compatible.

If we continue from the example above:

````
-spec meter_ctor(integer()) -> meter().
meter_ctor(X) -> X.

-spec foo() -> foot().
foo() -> meter_ctor(24).
% Output type: meter().
% Expected type according to the spec: foot().
% Result: Dialyzer error.
````

On the other hand, a nominal type is compatible with a non-opaque, non-nominal
type with the same structure. This compatibility goes both ways, meaning that
passing a structural type when a nominal is expected is allowed, and vice versa.

````
-spec qaz() -> integer().
qaz() -> meter_ctor(meter_ctor(24)). 
% Input of the outer meter_ctor(): meter(). 
% Expected type: integer().
% Result: No error.
````

There is one exception where two nominal types with different names can be
compatible: when one is derived from the other. For nominal types `s()` and
`t()`, `s()` can be derived from `t()` in the two following ways:

1. If `s()` is directly derived from `t()`.

````
-nominal s() :: t().
````

2. If `s()` is derived from other nominal types, which are derived from `t()`.

````
-nominal s() :: nominal_1().
-nominal nominal_1() :: nominal_2().
-nominal nominal_2() :: t().
````

In both cases, `s()` and `t()` are compatible nominal types even though they
have different names. Defining them in different modules does not affect
compatiblity.

In summary, nominal type-checking rules are as follows:

A function that has a `-spec` that states an argument or a return type to be
nominal type `a/0` (or any other arity), accepts or may return:

- Nominal type `a/0`
- A compatible nominal type `b/0`
- A compatible structural type

A function that has a `-spec` that states an argument or a return type to be a
structural type `b/0` (or any other arity), accepts or may return:

- A compatible structural type
- A compatible nominal type

**When deciding if a type should be nominal, here are some suggestions:**

- If there are other types in the same module with the same structure, and they
should never be mixed, all of them can benefit from being nominal types.
- If an opaque type does not require its type information to be hidden, it can
benefit from being redefined as a nominal type.
