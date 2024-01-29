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
# Support, Compatibility, Deprecations, and Removal

## Introduction

This document describes strategy regarding supported Releases, compatibility,
deprecations and removal of functionality. This document was introduced in
OTP 21. Actions taken regarding these issues before OTP 21 did not adhere this
document.

[](){: #supported_releases }

## Supported Releases

In general, bugs are only fixed on the latest
[release](versions.md#releases_and_patches), and new features are introduced in
the upcoming release that is under development. However, when we, due to
internal reasons, fix bugs on older releases, these will be available and
announced as well.

Due to the above, pull requests are only accepted on the `maint` and the
`master` branches in our [git repository](https://github.com/erlang/otp). The
`maint` branch contains changes planned for the next
[maintenance patch package](versions.md#releases_and_patches) on the latest OTP
release and the `master` branch contain changes planned for the upcoming OTP
release.

[](){: #compatibility }

## Compatibility

We always strive to remain as compatible as possible even in the cases where we
give no compatibility guarantees.

Different parts of the system will be handled differently regarding
compatibility. The following items describe how different parts of the system
are handled.

- **Erlang Distribution** - Erlang nodes can communicate across at least two
  preceding and two subsequent releases.

- **Compiled BEAM Code, NIF Libraries and Drivers** - Compiled code can be
  loaded on at least two subsequent releases.

  Loading on previous releases is _not_ supported.

- **APIs** - Compatible between releases.

- **Compiler Warnings** - New warnings may be issued between releases.

- **Command Line Arguments** - Incompatible changes may occur between releases.

- **OTP Build Procedures** - Incompatible changes may occur between releases.

Under certain circumstances incompatible changes might be introduced even in
parts of the system that should be compatible between releases. Things that
might trigger incompatible changes like this are:

- **Security Issues** - It might be necessary to introduce incompatible changes
  in order to solve a security issue. This kind of incompatibility might occur
  in a patch.

- **Bug Fixes** - We will not be bug-compatible. A bug fix might introduce
  incompatible changes. This kind of incompatibility might occur in a patch.

- **Severe Previous Design Issues** - Some parts of OTP were designed a very
  long time ago and did not necessarily take today's computing environments into
  account. In some cases the consequences of those design decisions are too
  severe. This may be performance wise, scalability wise, etc. If we deem the
  consequences too severe, we might introduce incompatible changes. This kind of
  incompatibility will not be introduced in a patch, but instead in the next
  release.

Peripheral, trace, and debug functionality is at greater risk of being changed
in an incompatible way than functionality in the language itself and core
libraries used during operation.

[](){: #deprecation }

## Deprecation

Functionality is deprecated when new functionality is introduced that is
preferred to be used instead of the old functionality that is being deprecated.
The deprecation does _not_ imply removal of the functionality unless an upcoming
removal is explicitly stated in the deprecation.

Deprecated functionality will be documented as deprecated, and compiler warnings
will be issued, when appropriate, as early as possible. That is, the new
preferred functionality will appear at the same time as the deprecation is
issued. A new deprecation will at least be announced in a release note and the
documentation.

[](){: #removal }

## Removal

Legacy solutions may eventually need to be removed. In such cases, they will be
phased out on a long enough time period to give users the time to adapt. Before
removal of functionality it will be deprecated at least during one release with
an explicit announcement about the upcoming removal. A new deprecation will at
least be announced in a release note and the documentation.

Peripheral, trace, and debug functionality is at greater risk of removal than
functionality in the language itself and core libraries used during operation.
