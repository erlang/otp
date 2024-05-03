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
# Communication in Erlang

Communication in Erlang is conceptually performed using asynchronous signaling.
All different executing entities, such as processes and ports, communicate
through asynchronous signals. The most commonly used signal is a message. Other
common signals are exit, link, unlink, monitor, and demonitor signals.

## Passing of Signals

This information has been moved to the
[_Signals_ section of the _Processes_ chapter in the _Erlang Reference Manual_](`e:system:ref_man_processes.md#signal-delivery`).

## Synchronous Communication

This information has been moved to the
[_Signals_ section of the _Processes_ chapter in the _Erlang Reference Manual_](`e:system:ref_man_processes.md#sync-comm`).

## Implementation

The implementation of different asynchronous signals in the virtual machine can
vary over time, but the behavior always respects this concept of asynchronous
signals being passed between entities as described above.

By inspecting the implementation, you might notice that some specific signal
gives a stricter guarantee than described above. It is of vital importance that
such knowledge about the implementation is _not_ used by Erlang code, as the
implementation can change at any time without prior notice.

Examples of major implementation changes:

- As from ERTS 5.5.2 exit signals to processes are truly asynchronously
  delivered.
- As from ERTS 5.10 all signals from processes to ports are truly asynchronously
  delivered.
