<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2025. All Rights Reserved.

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

# Checking for I/O events

An I/O event in ERTS is any event triggered by a [file descriptor]
on Unix or any OBJECT HANDLE that can be passed to [WaitForMultipleObjects] on Windows.
The check I/O infrastructure is used by linked-in drivers through [driver_select](erl_driver.md#driver_select)
and by NIFs through [enif_select](erl_nif.md#enif_select).

The main user of the check I/O subsystem is network communication through
`m:gen_tcp`, `m:gen_udp`, `m:gen_sctp` and `m:socket` on Unix (on Windows
`m:socket` used its own internal check I/O implementation based on completion ports).
It is also used by various other parts, such as when doing `os:cmd/1` or
reading from the terminal.

This document gives an overview of how the check I/O subsystem works.

The check I/O subsystem consists of a platform specific ([erl_poll](#polling))
and platform agnostic part ([check_io](#check-i-o)).

[erl_poll] is the basic mechanisms for checking if any events have been signalled
and allows waiting for a timeout if needed. The implementation of polling is very
platform specific and lives in [erts/emulator/sys/common/erl_poll.c] for Unix and
[erts/emulator/sys/win32/erl_poll.c] for Windows.

[check_io](#check-i-o) is the cross-platform part of the check I/O subsystem
that makes sure that [erl_poll] has the correct state and dispatches events to
the correct entity. The implementation can be found in [erts/emulator/sys/common/erl_check_io.c].

check_io is then used by ports and NIFs to listen to events. Ports are
communicated to through [port signals](PortSignals.md) and are delivered through
the [ready_input](driver_entry.md#ready_input) and [ready_output](driver_entry.md#ready_output) callbacks.
[NIFs](erl_nif.md) get an Erlang message whenever an event is triggered.

[file descriptor]: https://en.wikipedia.org/wiki/File_descriptor
[erl_poll]: #polling
[erts/emulator/sys/common/erl_poll.c]: https://github.com/erlang/otp/blob/master/erts/emulator/sys/common/erl_poll.c
[erts/emulator/sys/win32/erl_poll.c]: https://github.com/erlang/otp/blob/master/erts/emulator/sys/win32/erl_poll.c
[erts/emulator/sys/common/erl_check_io.c]: https://github.com/erlang/otp/blob/master/erts/emulator/sys/common/erl_check_io.c
[erts/emulator/beam/erl_port_task.c]: https://github.com/erlang/otp/blob/master/erts/emulator/beam/erl_port_task.c
[erl_check_io.c]: https://github.com/erlang/otp/blob/master/erts/emulator/sys/common/erl_check_io.c
[erts/emulator/beam/erl_port_task.c]: https://github.com/erlang/otp/blob/master/erts/emulator/beam/erl_port_task.c
[WaitForMultipleObjects]: https://learn.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-waitformultipleobjects

## Polling

The polling subsystem basically has two API functions; erts_poll_control and
erts_poll_wait. erts_poll_control is used to update a ErtsPollSet and
erts_poll_wait is used to wait for an event in the pollset to be triggered or a
timeout to happen (the timeout can be 0 if just checking). Only a single thread
usually calls erts_poll_wait at the same time, but multiple threads calls
erts_poll_control at any time.

The implementation of Unix and Windows are completely different as Windows does
not really have a concept of "polling" for an event.

### Polling on Unix

The poll implemention on Unix support a variety of different polling mechanisms.
At the writing of this document they are: [epoll] (Linux), [kqueue] (MacOS + *Bsd),
[/dev/poll] (Solaris), [poll] and [select]. [epoll]+[kqueue]+[/dev/poll] are
referred to as "kernel polling" methods, as the information about which FDs are currently monitored
lives in the OS kernel, while [poll]+[select] are "user polling" methods as the
caller needs to supply all FDs of interest to the kernel everything erts_poll_wait
is called.

By default all Unix'es use a "kernel polling" method, but has a fallback pollset that
uses "user polling" for FDs that the "kernel polling" mechanism does not
support (for example the stdin FD on Linux cannot be monitored by [epoll]).

As the kernel polling methods have their monitoring information in the kernel
it is possible to update these in parallel and without waking the thread that
is currently waiting for events. For user polling a queue of needed updates
is managed by each pollset and whenever an update is done to that queue the
thread waiting on events is woken up to update the set of file descriptors it
is waiting on.

When using kernel polling it is possible to have multiple poll threads
(using the [+IOt](erl_cmd.md#+IOt) flag) that read events from the same pollset.
This can be useful for very busy systems with many many FDs that become active alot.
If the kernel you are using is not very good at allowing multiple threads to
check in the same pollset (this primarily applied to old versions of Linux),
then it is also possible to configure erts to use separate
pollsets for each pollthread ((using the [+IOp](erl_cmd.md#+IOp) flag)).

When an event is triggered it is removed from the pollset and needs to be
re-enabled before any new events are triggered. If [ONESHOT] or equivalent is
available then kernel polling uses that flag, otherwise erl_poll will update
the pollset as the event is triggered.

[epoll]: https://man7.org/linux/man-pages/man7/epoll.7.html
[kqueue]: https://man.freebsd.org/cgi/man.cgi?kqueue
[/dev/poll]: https://docs.oracle.com/cd/E88353_01/html/E37851/poll-4d.html
[poll]: https://man7.org/linux/man-pages/man2/poll.2.html
[select]: https://man7.org/linux/man-pages/man2/select.2.html
[ONESHOT]: https://man7.org/linux/man-pages/man2/epoll_ctl.2.html#:~:text=EPOLLONESHOT

### Polling on Windows

Polling on Windows uses similar mechanism to "user polling" on Unix, except
that because WaitForMultipleObjects is limited to wait for 64 handles it
also needs to manage a thread pool. New threads are created as needed, so
if the system only ever listens for events on less then 64 handles only 1
thread will be created, but as the number of concurrent handles grow more
and more threads will be created.

The thread pool is never shrunk, so if the system at any point uses 1000
handles, there will forever be 16 threads in the thread pool.

## Check I/O

Checking for I/O is done by dedicated polling threads. By default, one
thread will always be waiting for I/O events using default polling method
and the "aux" thread will be waiting in the fallback pollset if such exists.

When an event is triggered it is dispatched to the correct port or process
depending on whether it is a driver or nif that has requested the event.
As the pollsets use [ONESHOT], the event is disabled until the port/NIF
registers a new interest in the event.

When you do a driver_select in a linked-in driver, that select will
be active until it is disabled. Because of this we need to insert the
FD back into the pollset when a driver_select event has been handled.
This is done by the port re-inserting the FD in the pollset after
a ready_input/ready_output event is called. For NIFs you need to call
enif_select for each event that you want, so no such mechanism needs
to exist for NIFs.

### Scheduler pollset

For very active FDs the fact that we need to re-insert events each time
they trigger can lead to quite a lot of overhead. Because of this there
is an optimization that places FDs that are never deselected into a
special pollset managed that is not checked by the poll threads, but
instead checked by the normal schedulers. In this pollset, the FDs no
longer use the [ONESHOT] mechanism, instead they trigger as soon as there
is data. For this to work, and not re-trigger on FDs before the port/nif has
handled the event, there is a global counter called erts_port_task_outstanding_io_tasks
that is incremented for each FD that is dispatched from the scheduler pollset.
That counter is then decremented as the FDs are handled by the ports/processes
that have subscribed to the event. When it reaches 0, we know that it is
safe to check for new events. This increases the latency for how quickly
we check for new events by a bit, but drastically reduces the CPU usage
for very active FDs.