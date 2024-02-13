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
# Upgrade when Erlang/OTP has Changed

[](){: #upgrade-section }

## Introduction

[](){: #upgrade }

As of Erlang/OTP 17, most applications deliver a valid application upgrade file
(`appup`). In earlier releases, a majority of the applications in Erlang/OTP did
not support upgrade. Many of the applications use the `restart_application`
instruction. These are applications for which it is not crucial to support real
soft upgrade, for example, tools and library applications. The
`restart_application` instruction ensures that all modules in the application
are reloaded and thereby running the new code.

## Upgrade of Core Applications

The core applications ERTS, Kernel, STDLIB, and SASL never allow real soft
upgrade, but require the Erlang emulator to be restarted. This is indicated to
the `release_handler` by the upgrade instruction `restart_new_emulator`. This
instruction is always the very first instruction executed, and it restarts the
emulator with the new versions of the above mentioned core applications and the
old versions of all other applications. When the node is back up, all other
upgrade instructions are executed, making sure each application is finally
running its new version.

It might seem strange to do a two-step upgrade instead of just restarting the
emulator with the new version of all applications. The reason for this design
decision is to allow `code_change` functions to have side effects, for example,
changing data on disk. It also guarantees that the upgrade mechanism for
non-core applications does not differ depending on whether or not core
applications are changed at the same time.

If, however, the more brutal variant is preferred, the the release upgrade file
can be handwritten using only the single upgrade instruction `restart_emulator`.
This instruction, in contrast to `restart_new_emulator`, causes the emulator to
restart with the new versions of _all_ applications.

_Note:_ If other instructions are included before `restart_emulator` in the
handwritten `relup` file, they are executed in the old emulator. This is a big
risk since there is no guarantee that new beam code can be loaded into the old
emulator. Adding instructions after `restart_emulator` has no effect as the
`release_handler` will not execute them.

For information about the release upgrade file, see the
[relup(4)](`e:sasl:relup.md`) manual page in SASL. For more information about
upgrade instructions, see the [appup(4)](`e:sasl:appup.md`) manual page in SASL.

## Applications that Still do Not Allow Code Upgrade

A few applications, such as erl_interface, do not support upgrade. This is
indicated by an application upgrade file containing only `{Vsn,[],[]}`. Any
attempt at creating a release upgrade file with such input fails. The only way
to force an upgrade involving applications like this is to handwrite the file
`relup`, preferably as described above with only the `restart_emulator`
instruction.
