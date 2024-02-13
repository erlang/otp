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
# Features

[](){: #features } Introduced in OTP 25, Erlang has the concept of selectable
features. A feature can change, add or remove behaviour of the language and/or
runtime system. Examples can include

- Adding new syntactical constructs to the language
- Change the semantics of an existing construct
- Change the behaviour of some runtime aspect

A feature will start out with a status of experimental part of OTP, making it
possible to try out for users and give feedback. The possibility to try out
features is enabled by options to the compiler, directives in a module and
options to the runtime system. Even when a feature is not experimental it will
still be possible to enable or disable it. This makes it possible to adapt a
code base at a suitable pace instead of being forced when changing to a new
release.

The status of a feature will eventually end up as being either a permanent part
of OTP or rejected, being removed and no longer selectable.

## Life cycle of features

A feature is in one of four possible states:

- **Experimental** - The initial state, is meant for trying out and collecting
  feedback. The feature can be enabled but is disabled by default.

- **Approved** - The feature has been finalised and is now part of OTP. By
  default it is enabled, but can be disabled.

- **Permanent** - The feature is now a permanent part of OTP. It can no longer
  be disabled.

- **Rejected** - The feature never reached the approved state and will not be
  part of OTP. It cannot be enabled.

After leaving the experimental state, a feature can enter any of the other three
states, and if the next state is approved, the feature will eventually end up in
the permanent state. A feature can change state only in connection with a
release.

A feature may be in the approved state for several releases.

| State        | Default  | Configurable | Available |
| ------------ | -------- | ------------ | --------- |
| Experimental | disabled | yes          | yes       |
| Approved     | enabled  | yes          | yes       |
| Permanent    | enabled  | no           | yes       |
| Rejected     | disabled | no           | no        |

_Table: Feature States_

- Being configurable means the possibility to enable or disable the feature by
  means of compiler options and directives in the file being compiled.
- Being available can be seen using the `FEATURE_AVAILABLE` macro.

## Enabling and Disabling Features

To use a feature that is in the experimental state, it has to be enabled during
compilation. This can be done in a number of different ways:

- **Options to `erlc`** - Options
  [`-enable-feature`](`e:erts:erlc_cmd.md#enable-feature`) and
  [`-disable-feature`](`e:erts:erlc_cmd.md#disable-feature`) can be used to
  enable or disable individal features.

- **Compiler options** - The compiler option
  [`{feature, <feature>, enable|disable}`](`m:compile#feature-option`) can be
  used either as a `+<term>` option to `erlc` or in the options argument to
  functions in the `compile` module.

- **The feature directive** - Inside a prefix of a module, one can use a
  [`-feature(<feature>, enable|disable)`](macros.md#feature-directive)
  directive. This is the preferred method of enabling and disabling features.

> #### Change {: .info }
>
> In Erlang/OTP 25, in order to load a module with a feature enabled, it was
> necessary to also enable the feature in the runtime. This was done using
> option [`-enable-feature`](`e:erts:erl_cmd.md#enable-feature`) to `erl`. This
> requirement was removed in Erlang/OTP 26. However, if you want to use features
> directly in shell, you still need to enable them in the runtime.

## Preprocessor Additions

To allow for conditional compilation during transitioning of a code base and/or
trying out experimental features
[feature](`e:system:macros.md#predefined-macros`) `predefined macros`
`?FEATURE_AVAILABLE(Feature)` and `?FEATURE_ENABLED(Feature)` are available.

## Information about Existing Features

The module `erl_features` `m:erl_features` exports a number of functions that
can be used to obtain information about current features as well as the features
used when compiling a module.

One can also use the `erlc` options
[`-list-features`](`e:erts:erlc_cmd.md#list-features`) and
[`-describe-feature <feature>`](`e:erts:erlc_cmd.md#describe-feature`) to get
information about existing features.

Additionally, there is the compiler option
[`warn_keywords`](`m:compile#warn-keywords`) that can be used to find atoms in
the code base that might collide with keywords in features not yet enabled.

## Existing Features

The following configurable features exist:

- **`maybe_expr` (experimental)** - Implementation of the
  [`maybe`](expressions.md#maybe) expression proposed in
  [EEP 49](https://www.erlang.org/eeps/eep-0049).
