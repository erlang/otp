<!--
%CopyrightBegin%

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
# Nominals

## Rationale and Syntax

For [user-defined types](typespec.md#type-declarations-of-user-defined-types)
defined with `-type`, the Erlang compiler will ignore their type names. This
means the Erlang compiler uses a structural type system. Two types are seen as
equivalent if their structures are the same. Type comparison is based on the
structures of the types, not on how the user explicitly defines them. In the
following example, `meter()` and `foot()` are equivalent, and neither differs
from the basic type `integer()`.

````
-type meter() :: integer().
-type foot() :: integer().
````

Nominal typing is an alternative type system. Two nominal types are equivalent
if and only if they are declared with the same type name. The syntax for
declaring nominal types is `-nominal`.

If `meter()` and `foot()` are defined as nominal types, they will no longer be
compatible. When a function expects type `meter()`, passing in type `foot()`
will result in a warning raised by the type checker.

````
-nominal meter() :: integer().
-nominal foot() :: integer().
````

The main use case of nominal types is to prevent accidental misuse of types with
the same structure. Within OTP, nominal type-checking is done in Dialyzer. The
Erlang compiler does not perform nominal type-checking.

## Nominal Type-Checking Rules

In general, if two nominal types have different names, and one is not derived
from the other, they are not compatible. Dialyzer's nominal type-checking
aligns with the examples' expected results in this section.

If we continue from the example above:

````
-spec int_to_meter(integer()) -> meter().
int_to_meter(X) -> X.

-spec foo() -> foot().
foo() -> int_to_meter(24).
````
A type checker that performs nominal type-checking should raise a warning.
According to the specification, `foo/0` should return a `foot()` type. However,
the function `int_to_meter/1` returns a `meter()` type, so `foo/0` will also
return a `meter()` type. Because `meter()` and `foot()` are incompatible
nominal types, Dialyzer raises the following warning for `foo/0`:

````
Invalid type specification for function foo/0.
The success typing is foo() -> (meter() :: integer())
But the spec is foo() -> foot()
The return types do not overlap
````

On the other hand, a nominal type is compatible with a non-opaque, non-nominal
type with the same structure. This compatibility goes both ways, meaning that
passing a structural type when a nominal type is expected is allowed, and
vice versa.

````
-spec qaz() -> integer().
qaz() -> int_to_meter(24).
````
A type checker that performs nominal type-checking should not raise a warning
in this case. The specification says that `qaz/0` should return an `integer()`
type. However, the function `int_to_meter/1` returns a `meter()` type, so
`qaz/0` will also return a `meter()` type. `integer()` is not a nominal type.
The structure of `meter()` is compatible with `integer()`. Dialyzer can
analyze the function above without raising a warning.

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
- If a type represents a unit like meter, second, byte, and so on, defining it
as a nominal type is always more useful than `-type`. You get the nice
guarantee that you cannot mix them up with other units defined as nominal
types.
- If an opaque type does not require its type information to be hidden, it can
benefit from being redefined as a nominal type. This makes Dialyzer's analysis
faster.