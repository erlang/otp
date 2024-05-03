### VxWorks Support

Some parts of OTP has had limited VxWorks support, such as for example
[`erl_interface`](`e:erl_interface:index.html`). This support is as of OTP 22
formally deprecated and has also been [removed in OTP 23](removed.md#otp-23).

### Legacy parts of erl_interface

The old legacy [`erl_interface`](`e:erl_interface:index.html`) library
(functions with prefix `erl_`) is deprecated as of OTP 22. These parts of
`erl_interface` has been informally deprecated for a very long time. You
typically want to replace the usage of the `erl_interface` library with the use
of the `ei` library which also is part of the `erl_interface` application. The
old legacy [`erl_interface`](`e:erl_interface:index.html`) library has also been
[removed in OTP 23](removed.md#otp-23).

### System Events

The format of "System Events" as defined in the man page for `m:sys` has been
clarified and cleaned up. Due to this, code that relied on the internal badly
documented previous (before this change) format of OTP's "System Events", needs
to be changed.

In the wake of this the function `sys:get_debug/3` that returns data with
undocumented and internal format (and therefore is practically useless) has been
deprecated, and a new function `sys:get_log/1` has been added, that hopefully
does what the deprecated function was intended for.

### Functions Deprecated in OTP 22

-   `net:broadcast/3` (use rpc:eval_everywhere/3 instead)
-   `net:call/4` (use rpc:call/4 instead)
-   `net:cast/4` (use rpc:cast/4 instead)
-   `net:ping/1` (use net_adm:ping/1 instead)
-   `net:sleep/1` (use 'receive after T -> ok end' instead)
-   `sys:get_debug/3` (incorrectly documented and only for internal use. Can
    often be replaced with sys:get_log/1)
