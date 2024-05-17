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
# Support, Compatibility, Deprecations, and Removal

## Introduction

This document describes the strategy regarding supported Releases,
compatibility, deprecations, and removal of functionality.

> #### Change {: .info }
>
> This document and the strategy it describes was introduced in
> Erlang/OTP 21.

[](){: #supported_releases }

## Supported Releases

In general, bugs are only fixed on the latest
[release](versions.md#releases_and_patches), and new features are introduced in
the upcoming release that is under development. However, when we, for
internal reasons, fix bugs on older releases, these will be available and
announced as well.

Pull requests are only accepted on the `maint` and the `master`
branches in our [git repository](https://github.com/erlang/otp). The
`maint` branch contains changes planned for the next [maintenance
patch package](versions.md#releases_and_patches) on the latest OTP
release and the `master` branch contain changes planned for the
upcoming OTP release.

## Compatibility

We strive to remain as compatible as possible, even in cases where we
give no compatibility guarantees.

Different parts of the system will be handled differently regarding
compatibility. The following items describe how different parts of the system
are handled.

- **Erlang Distribution** - Erlang nodes can communicate across at least two
  preceding and two subsequent releases.

- **Compiled BEAM Code, NIF Libraries, and Drivers** - Compiled code
  can be loaded on at least two subsequent releases. To achive the
  highest possible performance for Erlang code, ensure it is compiled
  using the same release as the one it will be deployed on.

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

- **Severe Previous Design Issues** - Some parts of OTP were designed
  a very long time ago and did not necessarily take today's computing
  environments into account. Consequently, the ramifications of these
  design choices can be quite significant, impacting performance,
  scalability, and more. If we determine that these consequences are
  too substantial, we may implement incompatible changes. Such changes
  are never introduced in a patch, but in the subsequent release.

Peripheral, trace, and debug functionality is at greater risk of being changed
in an incompatible way than functionality in the language itself and core
libraries used during operation.

There is a page in the documentation regarding incompatibilities:

* [Upcoming Potential Incompatibilities](`e:general_info:upcoming_incompatibilities.md`) -
  lists all upcoming potential incompatibilities.

## Deprecation

Deprecation of functionality occurs when newer, preferred alternatives
are introduced. The deprecation does **not** imply future removal of the
functionality unless an upcoming removal is explicitly stated in the
deprecation notice.

Deprecated functionality will be documented as deprecated and highlighted
in a release note as early possible. If appropriate, the compiler will
issue warnings when the deprecated functionality is used.

There is a page in the documentation regarding deprecations:

* [Deprecations](`e:general_info:deprecations.md`) - lists all
  deprecated functionality.

## Removal

It can become necessary to remove legacy solutions. In such instances,
they will be gradually phased out over a sufficient period to allow
users to adjust. Before functionality is removed, it will be
deprecated for at least one release, with an explicit announcement
about the upcoming removal.

Peripheral, trace, and debug functionality is at greater risk of removal than
functionality in the language itself and core libraries used during operation.

There are two pages in the documentation regarding removal:

* [Scheduled for Removal](`e:general_info:scheduled_for_removal.md`) - lists
  all functionality that is schedule for removal in upcoming releases.

* [Removed Functionality](`e:general_info:removed.md`) - lists
  functionality that has been removed.
