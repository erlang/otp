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
# Opaques

## Opaque Type Aliases

The main use case for opacity in Erlang is to hide the implementation of a data
type, enabling evolving the API while minimizing the risk of breaking consumers.
The runtime does not check opacity. Dialyzer provides some opacity-checking, but
the rest is up to convention.

> #### Change {: .info }
>
> Since Erlang/OTP 28, Dialyzer checks opaques in their defining module in the
> same way as nominals. Outside of the defining module, Dialyzer checks
> opaques for opacity violations.

This document explains what Erlang opacity is (and the trade-offs involved) via
the example of the [`sets:set()`](`t:sets:set/0`) data type. This type _was_
defined in the `sets` module like this:

```erlang
-opaque set(Element) :: #set{segs :: segs(Element)}.
```

OTP 24 changed the definition to the following in
[this commit](https://github.com/erlang/otp/commit/e66941e8d7c47b973dff94c0308ea85a6be1958e).

```erlang
-opaque set(Element) :: #set{segs :: segs(Element)} | #{Element => ?VALUE}.
```

And this change was safer and more backwards-compatible than if the type had
been defined with `-type` instead of `-opaque`. Here is why: when a module
defines an `-opaque`, the contract is that only the defining module should rely
on the definition of the type: no other modules should rely on the definition.

This means that code that pattern-matched on `set` as a record/tuple technically
broke the contract, and opted in to being potentially broken when the definition
of `set()` changed. Before OTP 24, this code printed `ok`. In OTP 24 it may
error:

```erlang
case sets:new() of
    Set when is_tuple(Set) ->
        io:format("ok")
end.
```

**When working with an opaque defined in another module, here are some
recommendations:**

- Don't examine the underlying type using pattern-matching, guards, or functions
  that reveal the type, such as [`tuple_size/1`](`tuple_size/1`) . One exception
  is that `=:=` and `=/=` can be used between two opaques with the same name, or
  between an opaque and `any()`, as those comparisons do not reveal underlying
  types.
- Use functions provided by the module for working with the type. For
  example, `sets` module provides `sets:new/0`, `sets:add_element/2`,
  `sets:is_element/2`, and so on.
- [`sets:set(a)`](`t:sets:set/1`) is a subtype of `sets:set(a | b)` and not the
  other way around. Generally, you can rely on the property that `the_opaque(T)`
  is a subtype of `the_opaque(U)` when T is a subtype of U.

**When defining your own opaques, here are some recommendations:**

- Since consumers are expected to not rely on the definition of the opaque type,
  you must provide functions for constructing, querying, and deconstructing
  instances of your opaque type. For example, sets can be constructed with
  `sets:new/0`, `sets:from_list/1`, `sets:add_element/2`, queried with
  `sets:is_element/2`, and deconstructed with`sets:to_list/1`.
- Don't define an opaque with a type variable in parameter position. This breaks
  the normal and expected behavior that (for example) `my_type(a)` is a subtype
  of `my_type(a | b)`
- Don't write case statements that can produce either an opaque or a non-opaque
  output.
- Add [specs](typespec.md) to exported functions that use the opaque type

> #### Change {: .info }
>
> Since Erlang/OTP 28, a Dialyzer option `opaque_union` has been added, so that
> Dialyzer can raise a warning whenever a union of opaque and non-opaque types
> is produced outside the opaque's defining module.

Note that opaques can be harder to work with for consumers, since the consumer
is expected not to pattern-match and must instead use functions that the author
of the opaque type provides to use instances of the type.

Also, opacity in Erlang is skin-deep: the runtime does not enforce
opacity-checking. So now that sets are implemented in terms of maps, an
[`is_map/1`](`is_map/1`) check on a set _will_ pass. The opacity rules are only
enforced by convention and by additional tooling such as Dialyzer, and this
enforcement is not total. A determined consumer of `sets` can still reveal the
structure of the set, for example by printing, serializing, or using a set as a
`t:term/0` and inspecting it via functions like [`is_map/1`](`is_map/1`) or
`maps:get/2`. Also, Dialyzer must make some
[approximations](https://github.com/erlang/otp/issues/5118).
