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
# Time and Time Correction in Erlang

## Extended Time Functionality

As of Erlang/OTP 18 (ERTS 7.0) the time functionality was extended. This
includes a [new API](time_correction.md#new-time-api) for time and
[time warp modes](time_correction.md#time-warp-modes) that change the system
behavior when system time changes.

> #### Note {: .info }
>
> As of Erlang/OTP 26 (ERTS 14.0) the
> [multi time warp mode](time_correction.md#multi-time-warp-mode) is enabled by
> default. This assumes that all code executing on the system is
> [time warp safe](time_correction.md#time-warp-safe-code).
>
> If you have old code in the system that is not time warp safe, you now
> explicitly need to start the system in
> [no time warp mode](time_correction.md#no-time-warp-mode) (or
> [singe time warp mode](time_correction.md#single-time-warp-mode) if it is
> partially time warp safe) in order to avoid problems. When starting the system
> in no time warp mode, the system behaves as it did prior to the introduction
> of the extended time functionality introduced in OTP 18.
>
> If you have code that is not time warp safe, you are strongly encouraged to
> change this so that you can use multi time warp mode. Compared to no time warp
> mode, multi time warp mode improves scalability and performance as well as
> accuracy and precision of time measurements.

## Terminology

To make it easier to understand this section, some terms are defined. This is a
mix of our own terminology (Erlang/OS system time, Erlang/OS monotonic time,
time warp) and globally accepted terminology.

### Monotonically Increasing

In a monotonically increasing sequence of values, all values that have a
predecessor are either larger than or equal to its predecessor.

### Strictly Monotonically Increasing

In a strictly monotonically increasing sequence of values, all values that have
a predecessor are larger than its predecessor.

### UT1

Universal Time. UT1 is based on the rotation of the earth and conceptually means
solar time at 0° longitude.

### UTC

Coordinated Universal Time. UTC almost aligns with
[UT1](time_correction.md#ut1). However, UTC uses the SI definition of a second,
which has not exactly the same length as the second used by UT1. This means that
UTC slowly drifts from UT1. To keep UTC relatively in sync with UT1, leap
seconds are inserted, and potentially also deleted. That is, an UTC day can be
86400, 86401, or 86399 seconds long.

### POSIX Time

Time since
[Epoch](http://pubs.opengroup.org/onlinepubs/9699919799/xrat/V4_xbd_chap03.html#tag_21_03_00_17).
Epoch is defined to be 00:00:00 [UTC](time_correction.md#utc), 1970-01-01.
[A day in POSIX time](http://pubs.opengroup.org/onlinepubs/009604499/basedefs/xbd_chap04.html#tag_04_14)
is defined to be exactly 86400 seconds long. Strangely enough, Epoch is defined
to be a time in UTC, and UTC has another definition of how long a day is.
Quoting the Open Group
["POSIX time is therefore not necessarily UTC, despite its appearance"](http://pubs.opengroup.org/onlinepubs/9699919799/xrat/V4_xbd_chap04.html#tag_21_04_15).
The effect of this is that when an UTC leap second is inserted, POSIX time
either stops for a second, or repeats the last second. If an UTC leap second
would be deleted (which has not happened yet), POSIX time would make a one
second leap forward.

### Time Resolution

The shortest time interval that can be distinguished when reading time values.

### Time Precision

The shortest time interval that can be distinguished repeatedly and reliably
when reading time values. Precision is limited by the
[resolution](time_correction.md#time-resolution), but resolution and precision
can differ significantly.

### Time Accuracy

The correctness of time values.

### Time Warp

A time warp is a leap forwards or backwards in time. That is, the difference of
time values taken before and after the time warp does not correspond to the
actual elapsed time.

### OS System Time

The operating systems view of [POSIX time](time_correction.md#posix-time). To
retrieve it, call [`os:system_time()`](`os:system_time/0`). This may or may not
be an accurate view of POSIX time. This time may typically be adjusted both
backwards and forwards without limitation. That is,
[time warps](time_correction.md#time-warp) may be observed.

To get information about the Erlang runtime system's source of OS system time,
call
[`erlang:system_info(os_system_time_source)`](`m:erlang#system_info_os_system_time_source`).

### OS Monotonic Time

A monotonically increasing time provided by the OS. This time does not leap and
has a relatively steady frequency although not completely correct. However, it
is not uncommon that OS monotonic time stops if the system is suspended. This
time typically increases since some unspecified point in time that is not
connected to [OS system time](time_correction.md#os-system-time). This type of
time is not necessarily provided by all OSs.

To get information about the Erlang runtime system's source of OS monotonic
time, call
[`erlang:system_info(os_monotonic_time_source)`](`m:erlang#system_info_os_monotonic_time_source`).

### Erlang System Time

The Erlang runtime systems view of [POSIX time](time_correction.md#posix-time).
To retrieve it, call [`erlang:system_time()`](`erlang:system_time/0`).

This time may or may not be an accurate view of POSIX time, and may or may not
align with [OS system time](time_correction.md#os-system-time). The runtime
system works towards aligning the two system times. Depending on the
[time warp mode](time_correction.md#time-warp-modes) used, this can be achieved
by letting Erlang system time perform a
[time warp](time_correction.md#time-warp).

### Erlang Monotonic Time

A monotonically increasing time provided by the Erlang runtime system. Erlang
monotonic time increases since some unspecified point in time. To retrieve it,
call [`erlang:monotonic_time()`](`erlang:monotonic_time/0`).

The [accuracy](time_correction.md#time-accuracy) and
[precision](time_correction.md#time-precision) of Erlang monotonic time heavily
depends on the following:

- Accuracy and precision of
  [OS monotonic time](time_correction.md#os-monotonic-time)
- Accuracy and precision of [OS system time](time_correction.md#os-system-time)
- [time warp mode](time_correction.md#time-warp-modes) used

On a system without OS monotonic time, Erlang monotonic time guarantees
monotonicity, but cannot give other guarantees. The frequency adjustments made
to Erlang monotonic time depend on the time warp mode used.

Internally in the runtime system, Erlang monotonic time is the "time engine"
that is used for more or less everything that has anything to do with time. All
timers, regardless of it is a `receive ... after` timer, BIF timer, or a timer
in the `m:timer` module, are triggered relative Erlang monotonic time. Even
[Erlang system time](time_correction.md#erlang-system-time) is based on Erlang
monotonic time. By adding current Erlang monotonic time with current time
offset, you get current Erlang system time.

To retrieve the current time offset, call `erlang:time_offset/0`.

## Introduction

Time is vital to an Erlang program and, more importantly, _correct_ time is
vital to an Erlang program. As Erlang is a language with soft real-time
properties and we can express time in our programs, the Virtual Machine and the
language must be careful about what is considered a correct time and in how time
functions behave.

When Erlang was designed, it was assumed that the wall clock time in the system
showed a monotonic time moving forward at exactly the same pace as the
definition of time. This more or less meant that an atomic clock (or better time
source) was expected to be attached to your hardware and that the hardware was
then expected to be locked away from any human tinkering forever. While this can
be a compelling thought, it is simply never the case.

A "normal" modern computer cannot keep time, not on itself and not unless you
have a chip-level atomic clock wired to it. Time, as perceived by your computer,
must normally be corrected. Hence the Network Time Protocol (NTP) protocol,
together with the `ntpd` process, does its best to keep your computer time in
sync with the correct time. Between NTP corrections, usually a less potent
time-keeper than an atomic clock is used.

However, NTP is not fail-safe. The NTP server can be unavailable, `ntp.conf` can
be wrongly configured, or your computer can sometimes be disconnected from
Internet. Furthermore, you can have a user (or even system administrator) who
thinks the correct way to handle Daylight Saving Time is to adjust the clock one
hour two times a year (which is the incorrect way to do it). To complicate
things further, this user fetched your software from Internet and has not
considered what the correct time is as perceived by a computer. The user does
not care about keeping the wall clock in sync with the correct time. The user
expects your program to have unlimited knowledge about the time.

Most programmers also expect time to be reliable, at least until they realize
that the wall clock time on their workstation is off by a minute. Then they set
it to the correct time, but most probably not in a smooth way.

The number of problems that arise when you always expect the wall clock time on
the system to be correct can be immense. Erlang therefore introduced the
"corrected estimate of time", or the "time correction", many years ago. The time
correction relies on the fact that most operating systems have some kind of
monotonic clock, either a real-time extension or some built-in "tick counter"
that is independent of the wall clock settings. This counter can have
microsecond resolution or much less, but it has a drift that cannot be ignored.

## Time Correction

If time correction is enabled, the Erlang runtime system makes use of both
[OS system time](time_correction.md#os-system-time) and
[OS monotonic time](time_correction.md#os-monotonic-time), to adjust the
frequency of the Erlang monotonic clock. Time correction ensures that
[Erlang monotonic time](time_correction.md#erlang-monotonic-time) does not warp
and that the frequency is relatively accurate. The type of frequency adjustments
depends on the time warp mode used. Section
[Time Warp Modes](time_correction.md#time-warp-modes) provides more details.

By default time correction is enabled if support for it exists on the specific
platform. Support for it includes both OS monotonic time, provided by the OS,
and an implementation in the Erlang runtime system using OS monotonic time. To
check if your system has support for OS monotonic time, call
[`erlang:system_info(os_monotonic_time_source)`](`m:erlang#system_info_os_monotonic_time_source`).
To check if time correction is enabled on your system, call
[`erlang:system_info(time_correction)`](`m:erlang#system_info_time_correction`).

To enable or disable time correction, pass command-line argument
[`+c [true|false]`](erl_cmd.md#+c) to [`erl(1)`](erl_cmd.md).

If time correction is disabled, Erlang monotonic time can warp forwards or stop,
or even freeze for extended periods of time. There are then no guarantees that
the frequency of the Erlang monotonic clock is accurate or stable.

_You typically never want to disable time correction_. Previously a performance
penalty was associated with time correction, but nowadays it is usually the
other way around. If time correction is disabled, you probably get bad
scalability, bad performance, and bad time measurements.

## Time Warp Safe Code

Time warp safe code can handle a [time warp](time_correction.md#time-warp) of
[Erlang system time](time_correction.md#erlang-system-time).

`erlang:now/0` behaves bad when Erlang system time warps. When Erlang system
time does a time warp backwards, the values returned from `erlang:now/0` freeze
(if you disregard the microsecond increments made because of the actual call)
until OS system time reaches the point of the last value returned by
`erlang:now/0`. This freeze can continue for a long time. It can take years,
decades, and even longer until the freeze stops.

All uses of `erlang:now/0` are not necessarily time warp unsafe. If you do not
use it to get time, it is time warp safe. However, _all uses of `erlang:now/0`
are suboptimal_ from a performance and scalability perspective. So you really
want to replace the use of it with other functionality. For examples of how to
replace the use of `erlang:now/0`, see section
[How to Work with the New API](time_correction.md#Dos_and_Donts).

## Time Warp Modes

Current [Erlang system time](time_correction.md#erlang-system-time) is
determined by adding the current
[Erlang monotonic time](`erlang:monotonic_time/0`) with current
[time offset](`erlang:time_offset/0`). The time offset is managed differently
depending on which time warp mode you use.

To set the time warp mode, pass command-line argument
[`+C [no_time_warp|single_time_warp|multi_time_warp]`](erl_cmd.md#+C_) to
[`erl(1)`](erl_cmd.md).

### No Time Warp Mode

The time offset is determined at runtime system start and does not change later.
This is the same behavior as was default prior to OTP 26 (ERTS 14.0), and the
only behavior prior to OTP 18 (ERTS 7.0).

As the time offset is not allowed to change, time correction must adjust the
frequency of the Erlang monotonic clock to align Erlang system time with OS
system time smoothly. A significant downside of this approach is that we on
purpose will use a faulty frequency on the Erlang monotonic clock if adjustments
are needed. This error can be as large as 1%. This error will show up in all
time measurements in the runtime system.

If time correction is not enabled, Erlang monotonic time freezes when OS system
time leaps backwards. The freeze of monotonic time continues until OS system
time catches up. The freeze can continue for a long time. When OS system time
leaps forwards, Erlang monotonic time also leaps forward.

### Single Time Warp Mode

This mode is more or less a backward compatibility mode as from its
introduction.

On an embedded system it is not uncommon that the system has no power supply,
not even a battery, when it is shut off. The system clock on such a system is
typically way off when the system boots. If
[no time warp mode](time_correction.md#no-time-warp-mode) is used, and the
Erlang runtime system is started before OS system time has been corrected,
Erlang system time can be wrong for a long time, centuries or even longer.

If you need to use Erlang code that is not
[time warp safe](time_correction.md#time-warp-safe-code), and you need to start
the Erlang runtime system before OS system time has been corrected, you may want
to use the single time warp mode.

> #### Note {: .info }
>
> There are limitations to when you can execute time warp unsafe code using this
> mode. If it is possible to use time warp safe code only, it is _much_ better
> to use the [multi-time warp mode](time_correction.md#multi-time-warp-mode)
> instead.

Using the single time warp mode, the time offset is handled in two phases:

- **Preliminary Phase** - This phase starts when the runtime system starts. A
  preliminary time offset based on current OS system time is determined. This
  offset is from now on to be fixed during the whole preliminary phase.

  If time correction is enabled, adjustments to the Erlang monotonic clock are
  made to keep its frequency as correct as possible. However, _no_ adjustments
  are made trying to align Erlang system time and OS system time. That is,
  during the preliminary phase Erlang system time and OS system time can diverge
  from each other, and no attempt is made to prevent this.

  If time correction is disabled, changes in OS system time affects the
  monotonic clock the same way as when the
  [no time warp mode](time_correction.md#no-time-warp-mode) is used.

- **Final Phase** - This phase begins when the user finalizes the time offset by
  calling
  [`erlang:system_flag(time_offset, finalize)`](`m:erlang#system_flag_time_offset`).
  The finalization can only be performed once.

  During finalization, the time offset is adjusted and fixed so that current
  Erlang system time aligns with the current OS system time. As the time offset
  can change during the finalization, Erlang system time can do a time warp at
  this point. The time offset is from now on fixed until the runtime system
  terminates. If time correction has been enabled, the time correction from now
  on also makes adjustments to align Erlang system time with OS system time.
  When the system is in the final phase, it behaves exactly as in
  [no time warp mode](time_correction.md#no-time-warp-mode).

In order for this to work properly, the user must ensure that the following two
requirements are satisfied:

- **Forward Time Warp** - The time warp made when finalizing the time offset can
  only be done forwards without encountering problems. This implies that the
  user must ensure that OS system time is set to a time earlier or equal to
  actual POSIX time before starting the Erlang runtime system.

  If you are not sure that OS system time is correct, set it to a time that is
  guaranteed to be earlier than actual POSIX time before starting the Erlang
  runtime system, just to be safe.

- **Finalize Correct OS System Time** - OS system time must be correct when the
  user finalizes the time offset.

If these requirements are not fulfilled, the system may behave very bad.

Assuming that these requirements are fulfilled, time correction is enabled, and
OS system time is adjusted using a time adjustment protocol such as NTP, only
small adjustments of Erlang monotonic time are needed to keep system times
aligned after finalization. As long as the system is not suspended, the largest
adjustments needed are for inserted (or deleted) leap seconds.

> #### Warning {: .warning }
>
> To use this mode, ensure that all Erlang code that will execute in both phases
> is [time warp safe](time_correction.md#time-warp-safe-code).
>
> Code executing only in the final phase does not have to be able to cope with
> the time warp.

### Multi-Time Warp Mode

_Multi-time warp mode in combination with time correction is the preferred
configuration_. This as the Erlang runtime system have better performance, scale
better, and behave better on almost all platforms. Also, the accuracy and
precision of time measurements are better. Only Erlang runtime systems executing
on ancient platforms benefit from another configuration. As of OTP 26 (ERTS
14.0) this is also the default.

The time offset can change at any time without limitations. That is, Erlang
system time can perform time warps both forwards and backwards at _any_ time. As
we align Erlang system time with OS system time by changing the time offset, we
can enable a time correction that tries to adjust the frequency of the Erlang
monotonic clock to be as correct as possible. This makes time measurements using
Erlang monotonic time more accurate and precise.

If time correction is disabled, Erlang monotonic time leaps forward if OS system
time leaps forward. If OS system time leaps backwards, Erlang monotonic time
stops briefly, but it does not freeze for extended periods of time. This as the
time offset is changed to align Erlang system time with OS system time.

> #### Warning {: .warning }
>
> To use this mode, ensure that all Erlang code that will execute on the runtime
> system is [time warp safe](time_correction.md#time-warp-safe-code).

## New Time API

The old time API is based on `erlang:now/0`. `erlang:now/0` was intended to be
used for many unrelated things. This tied these unrelated operations together
and caused issues with performance, scalability, accuracy, and precision for
operations that did not need to have such issues. To improve this, the new API
spreads different functionality over multiple functions.

To be backward compatible, `erlang:now/0` remains "as is", but _you are strongly
discouraged from using it_. Many use cases of `erlang:now/0` prevents you from
using the new [multi-time warp mode](time_correction.md#multi-time-warp-mode),
which is an important part of this new time functionality improvement.

Some of the new BIFs on some systems, perhaps surprisingly, return negative
integer values on a newly started runtime system. This is not a bug, but a
memory use optimization.

The new API consists of the following new BIFs:

- `erlang:convert_time_unit/3`
- `erlang:monotonic_time/0`
- `erlang:monotonic_time/1`
- `erlang:system_time/0`
- `erlang:system_time/1`
- `erlang:time_offset/0`
- `erlang:time_offset/1`
- `erlang:timestamp/0`
- `erlang:unique_integer/0`
- `erlang:unique_integer/1`
- `os:system_time/0`
- `os:system_time/1`

The new API also consists of extensions of the following existing BIFs:

- [`erlang:monitor(time_offset, clock_service)`](`erlang:monitor/2`)
- [`erlang:system_flag(time_offset, finalize)`](`m:erlang#system_flag_time_offset`)
- [`erlang:system_info(os_monotonic_time_source)`](`m:erlang#system_info_os_monotonic_time_source`)
- [`erlang:system_info(os_system_time_source)`](`m:erlang#system_info_os_system_time_source`)
- [`erlang:system_info(time_offset)`](`m:erlang#system_info_time_offset`)
- [`erlang:system_info(time_warp_mode)`](`m:erlang#system_info_time_warp_mode`)
- [`erlang:system_info(time_correction)`](`m:erlang#system_info_time_correction`)
- [`erlang:system_info(start_time)`](`m:erlang#system_info_start_time`)
- [`erlang:system_info(end_time)`](`m:erlang#system_info_end_time`)

### New Erlang Monotonic Time

Erlang monotonic time as such is new as from ERTS 7.0. It is introduced to
detach time measurements, such as elapsed time from calendar time. In many use
cases there is a need to measure elapsed time or specify a time relative to
another point in time without the need to know the involved times in UTC or any
other globally defined time scale. By introducing a time scale with a local
definition of where it starts, time that do not concern calendar time can be
managed on that time scale. Erlang monotonic time uses such a time scale with a
locally defined start.

The introduction of Erlang monotonic time allows us to adjust the two Erlang
times (Erlang monotonic time and Erlang system time) separately. By doing this,
the accuracy of elapsed time does not have to suffer just because the system
time happened to be wrong at some point in time. Separate adjustments of the two
times are only performed in the time warp modes, and only fully separated in the
[multi-time warp mode](time_correction.md#multi-time-warp-mode). All other modes
than the multi-time warp mode are for backward compatibility reasons. When using
these modes, the accuracy of Erlang monotonic time suffer, as the adjustments of
Erlang monotonic time in these modes are more or less tied to Erlang system
time.

The adjustment of system time could have been made smother than using a time
warp approach, but we think that would be a bad choice. As we can express and
measure time that is not connected to calendar time by the use of Erlang
monotonic time, it is better to expose the change in Erlang system time
immediately. This as the Erlang applications executing on the system can react
on the change in system time as soon as possible. This is also more or less
exactly how most operating systems handle this (OS monotonic time and OS system
time). By adjusting system time smoothly, we would just hide the fact that
system time changed and make it harder for the Erlang applications to react to
the change in a sensible way.

To be able to react to a change in Erlang system time, you must be able to
detect that it happened. The change in Erlang system time occurs when the
current time offset is changed. We have therefore introduced the possibility to
monitor the time offset using
[`erlang:monitor(time_offset, clock_service)`](`erlang:monitor/2`). A process
monitoring the time offset is sent a message on the following format when the
time offset is changed:

```text
{'CHANGE', MonitorReference, time_offset, clock_service, NewTimeOffset}
```

### Unique Values

Besides reporting time, `erlang:now/0` also produces unique and strictly
monotonically increasing values. To detach this functionality from time
measurements, we have introduced
[`erlang:unique_integer()`](`erlang:unique_integer/1`).

[](){: #Dos_and_Donts }

### How to Work with the New API

Previously `erlang:now/0` was the only option for doing many things. This
section deals with some things that `erlang:now/0` can be used for, and how you
use the new API.

#### Retrieve Erlang System Time

> #### Dont {: .error }
>
> Use `erlang:now/0` to retrieve the current Erlang system time.

> #### Do {: .tip }
>
> Use `erlang:system_time/1` to retrieve the current Erlang system time on the
> [time unit](`t:erlang:time_unit/0`) of your choice.
>
> If you want the same format as returned by `erlang:now/0`, use
> `erlang:timestamp/0`.

#### Measure Elapsed Time

> #### Dont {: .error }
>
> Take time stamps with `erlang:now/0` and calculate the difference in time with
> `timer:now_diff/2`.

> #### Do {: .tip }
>
> Take time stamps with `erlang:monotonic_time/0` and calculate the time
> difference using ordinary subtraction. The result is in `native`
> [time unit](`t:erlang:time_unit/0`). If you want to convert the result to
> another time unit, you can use `erlang:convert_time_unit/3`.
>
> An easier way to do this is to use `erlang:monotonic_time/1` with the desired
> time unit. However, you can then lose accuracy and precision.

#### Determine Order of Events

> #### Dont {: .error }
>
> Determine the order of events by saving a time stamp with `erlang:now/0` when
> the event occurs.

> #### Do {: .tip }
>
> Determine the order of events by saving the integer returned by
> [`erlang:unique_integer([monotonic])`](`erlang:unique_integer/1`) when the
> event occurs. These integers are strictly monotonically ordered on current
> runtime system instance corresponding to creation time.

#### Determine Order of Events with Time of the Event

> #### Dont {: .error }
>
> Determine the order of events by saving a time stamp with `erlang:now/0` when
> the event occurs.

> #### Do {: .tip }
>
> Determine the order of events by saving a tuple containing
> [monotonic time](`erlang:monotonic_time/0`) and a
> [strictly monotonically increasing integer](`erlang:unique_integer/1`) as
> follows:
>
> ```erlang
> Time = erlang:monotonic_time(),
> UMI = erlang:unique_integer([monotonic]),
> EventTag = {Time, UMI}
> ```
>
> These tuples are strictly monotonically ordered on the current runtime system
> instance according to creation time. It is important that the monotonic time
> is in the first element (the most significant element when comparing
> two-tuples). Using the monotonic time in the tuples, you can calculate time
> between events.
>
> If you are interested in Erlang system time at the time when the event
> occurred, you can also save the time offset before or after saving the events
> using `erlang:time_offset/0`. Erlang monotonic time added with the time offset
> corresponds to Erlang system time.
>
> If you are executing in a mode where time offset can change, and you want to
> get the actual Erlang system time when the event occurred, you can save the
> time offset as a third element in the tuple (the least significant element
> when comparing three-tuples).

#### Create a Unique Name

> #### Dont {: .error }
>
> Use the values returned from `erlang:now/0` to create a name unique on the
> current runtime system instance.

> #### Do {: .tip }
>
> Use the value returned from `erlang:unique_integer/0` to create a name unique
> on the current runtime system instance. If you only want positive integers,
> you can use [`erlang:unique_integer([positive])`](`erlang:unique_integer/1`).

#### Seed Random Number Generation with a Unique Value

> #### Dont {: .error }
>
> Seed random number generation using `erlang:now/0`.

> #### Do {: .tip }
>
> Seed random number generation using a combination of
> [`erlang:monotonic_time/0`](`erlang:monotonic_time/0`),
> [`erlang:time_offset/0`](`erlang:time_offset/0`),
> [`erlang:unique_integer/0`](`erlang:unique_integer/0`), and other
> functionality.

To sum up this section: _Do not use `erlang:now/0`._

## Support of Both New and Old OTP Releases

It can be required that your code must run on a variety of OTP installations of
different OTP releases. If so, you cannot use the new API out of the box, as it
will not be available on releases before OTP 18. The solution is _not_ to avoid
using the new API, as your code would then not benefit from the scalability and
accuracy improvements made. Instead, use the new API when available, and fall
back on `erlang:now/0` when the new API is unavailable.

Fortunately most of the new API can easily be implemented using existing
primitives, except for:

- [`erlang:system_info(start_time)`](`m:erlang#system_info_start_time`)
- [`erlang:system_info(end_time)`](`m:erlang#system_info_end_time`)
- [`erlang:system_info(os_monotonic_time_source)`](`m:erlang#system_info_os_monotonic_time_source`)
- [`erlang:system_info(os_system_time_source)`](`m:erlang#system_info_os_system_time_source`)

By wrapping the API with functions that fall back on `erlang:now/0` when the new
API is unavailable, and using these wrappers instead of using the API directly,
the problem is solved. These wrappers can, for example, be implemented as in
[$ERL_TOP/erts/example/time_compat.erl](assets/time_compat.erl).
