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

# Introduction

This section describes the issues that are specific for running Erlang on an UNIX
embedded system. It describes the differences in installing and starting Erlang
compared to how it is done for a non-embedded system.

For details on how to create a target system, see [Creating and Upgrading a Target System]
in the System Principles section.

When running on Windows, so special considerations need to be made. Starting Erlang
should be done via [`erlsrv`](`e:erts:erlsrv_cmd.md`).

## Installing an Embedded System

This section is about installing an embedded system. The following topics are
considered:

- Creating user and installation directory
- Installing an embedded system
- Configuring automatic start at boot
- Changing permission for reboot
- Setting TERM environment variable

Several of the procedures in this section require expert knowledge of the
operating system. For most of them super user privilege is needed.

### Creating User and Installation Directory

It is recommended that the embedded environment is run by an ordinary user, that
is, a user who does not have super user privileges.

In this section, it is assumed that the username is `otpuser` and that the home
directory of that user is:

```text
/home/otpuser
```

It is also assumed that in the home directory of `otpuser`, there is a directory
named `otp`, the full path of which is:

```text
/home/otpuser/otp
```

This directory is the _installation directory_ of the embedded environment.

### Installing an Embedded System

The procedure for installing an embedded system is the same as for an ordinary
system (see [Installation Guide](installation_guide/INSTALL.md) and [Creating and Upgrading a Target System]
in the System Principles section), except for the following:

- The (compressed) archive file is to be extracted in the installation
  directory defined above.
- It is not needed to link the start script to a standard directory like
  `/usr/local/bin`.

### Configuring Automatic Start at Boot

A true embedded system must start when the system boots. This section accounts
for the necessary configurations needed to achieve that using `init.d` start
scripts.

The embedded system and all the applications start automatically if the script
file shown below is added to directory `/etc/rc3.d`. The file must be owned and
readable by `root`. Its name cannot be arbitrarily assigned; the following name
is recommended:

```text
S75otp.system
```

For more details on initialization (and termination) scripts, and naming
thereof, see the `init.d` documentation on your OS.

```text
#!/bin/sh
#
#  File name:  S75otp.system
#  Purpose:    Automatically starts Erlang and applications when the
#              system starts
#  Author:     janne@erlang.ericsson.se
#  Resides in: /etc/rc3.d
#

if [ ! -d /usr/bin ]
then                    # /usr not mounted
        exit
fi

killproc() {            # kill the named process(es)
        pid=`/usr/bin/ps -e |
             /usr/bin/grep -w $1 |
             /usr/bin/sed -e 's/^  *//' -e 's/ .*//'`
        [ "$pid" != "" ] && kill $pid
}

# Start/stop processes required for Erlang

case "$1" in
'start')
        # Start the Erlang emulator
        #
        su - otpuser -c "/home/otpuser/otp/bin/start" &
        ;;
'stop')
        killproc beam
        ;;
*)
        echo "Usage: $0 { start | stop }"
        ;;
esac
```

File `/home/otpuser/otp/bin/start` referred to in the above script is
precisely the `start` script described in [_Starting Erlang_](#starting-erlang).
The script variable `$OTPROOT` in that `start` script corresponds to the following example path used
in this section:

```text
/home/otpuser/otp
```

The `start` script is to be edited accordingly.

Use of the `killproc` procedure in the above script can be combined with a call
to `erl_call`, for example:

```text
$SOME_PATH/erl_call -n Node init stop
```

To take Erlang down gracefully, see the
[`erl_call(1)`](`e:erl_interface:erl_call_cmd.md`) manual page in
`erl_interface` for details on the use of `erl_call`. However, that requires
that Erlang runs as a distributed node, which is not always the case.

The `killproc` procedure is not to be removed. The purpose is here to move from
run level 3 (multi-user mode with networking resources) to run level 2
(multi-user mode without such resources), in which Erlang is not to run.

### Changing Permissions for Reboot

If the `HEART_COMMAND` environment variable is to be set in the `start` script
in [_Starting Erlang_](#starting-erlang), and if the value is to be set to the path of the
`reboot` command, that is:

```text
HEART_COMMAND=/usr/sbin/reboot
```

then the ownership and file permissions for `/usr/sbin/reboot` must be changed
as follows:

```text
chown 0 /usr/sbin/reboot
chmod 4755 /usr/sbin/reboot
```

See also the `m:heart` manual page in Kernel.

### Setting TERM Environment Variable

When the Erlang runtime system is automatically started from the `S75otp.system`
script, the `TERM` environment variable must be set. The following is a minimal
setting:

```text
TERM=dumb
```

This is to be added to the `start` script.

## Starting Erlang

This section describes how an embedded system is started. Four programs are
involved and they normally reside in the directory `<ERL_INSTALL_DIR>/bin`. The
only exception is the [`start`](`e:erts:start_cmd.md`) program, which can be located anywhere, and is
also the only program that must be modified by the user.

In an embedded system, there is usually no interactive shell. However, an
operator can attach to the Erlang system by command [`to_erl`](#to_erl).
The operator is then connected to the Erlang shell and can give ordinary Erlang commands. All
interaction with the system through this shell is logged in a special directory.

Basically, the procedure is as follows:

- The [`start`](`e:erts:start_cmd.md`)) program is called when the machine is started.
- It calls [`run_erl`](#run_erl), which sets up things so the operator can attach to the
  system.
- It calls [`start_erl`](`e:erts:start_erl_cmd.md`), which calls the correct version of
  `erlexec` (which is located in `<ERL_INSTALL_DIR>/erts-EVsn/bin`) with the correct `boot` and
  `config` files.

## Programs

### start

This program is called when the machine is started. It can be modified or
rewritten to suit a special system. By default, it must be called `start` and
reside in `<ERL_INSTALL_DIR>/bin`. Another start program can be used, by using
configuration parameter `start_prg` in application SASL.

The start program must call [`run_erl`](#run_erl) as shown below. It must also take an
optional parameter, which defaults to
`<ERL_INSTALL_DIR>/releases/start_erl.data`.

This program is to set static parameters and environment variables such as
`-sname Name` and `HEART_COMMAND` to reboot the machine.

The `<RELDIR>` directory is where new release packets are installed, and where
the release handler keeps information about releases. For more information, see
the `m:release_handler` manual page in SASL.

The following script illustrates the default behaviour of the program:

```text
#!/bin/sh
# Usage: start [DataFile]
#
ROOTDIR=/usr/local/otp

if [ -z "$RELDIR" ]
then
   RELDIR=$ROOTDIR/releases
fi

START_ERL_DATA=${1:-$RELDIR/start_erl.data}

$ROOTDIR/bin/run_erl /tmp/ $ROOTDIR/log "exec $ROOTDIR/bin/start_erl \
                     $ROOTDIR $RELDIR $START_ERL_DATA" > /dev/null 2>&1 &
```

The following script illustrates a modification where the node is given the name
`cp1`, and where the environment variables `HEART_COMMAND` and `TERM` have been
added to the previous script:

```text
#!/bin/sh
# Usage: start [DataFile]
#
HEART_COMMAND=/usr/sbin/reboot
TERM=dumb
export HEART_COMMAND TERM

ROOTDIR=/usr/local/otp

if [ -z "$RELDIR" ]
then
   RELDIR=$ROOTDIR/releases
fi

START_ERL_DATA=${1:-$RELDIR/start_erl.data}

$ROOTDIR/bin/run_erl /tmp/ $ROOTDIR/log "exec $ROOTDIR/bin/start_erl \
      $ROOTDIR $RELDIR $START_ERL_DATA -heart -sname cp1" > /dev/null 2>&1 &
```

If a diskless and/or read-only client node is about to start, file
`start_erl.data` is located in the client directory at the master node. Thus,
the `START_ERL_DATA` line is to look like:

```text
CLIENTDIR=$ROOTDIR/clients/clientname
START_ERL_DATA=${1:-$CLIENTDIR/bin/start_erl.data}
```

### run_erl

This program is used to start the emulator, but you will not be connected to the
shell. `to_erl` is used to connect to the Erlang shell.

```text
Usage: run_erl pipe_dir/ log_dir "exec command [parameters ...]"
```

Here:

- `pipe_dir/` is to be `/tmp/` (`to_erl` uses this name by default).
- `log_dir` is where the log files are written.
- `command [parameters]` is executed.
- Everything written to `stdin` and `stdout` is logged in `log_dir`.

Log files are written in `log_dir`. Each log file has a name of the form
`erlang.log.N`, where N is a generation number, ranging from 1 to 5. Each log
file holds up to 100 kB text. As time goes by, the following log files are found
in the log file directory:

```text
erlang.log.1
erlang.log.1, erlang.log.2
erlang.log.1, erlang.log.2, erlang.log.3
erlang.log.1, erlang.log.2, erlang.log.3, erlang.log.4
erlang.log.2, erlang.log.3, erlang.log.4, erlang.log.5
erlang.log.3, erlang.log.4, erlang.log.5, erlang.log.1
...
```

The most recent log file is the rightmost in each row. That is, the most recent
file is the one with the highest number, or if there are already four files, the
one before the skip.

When a log file is opened (for appending or created), a time stamp is written to
the file. If nothing has been written to the log files for 15 minutes, a record
is inserted that says that we are still alive.

For more details see [`run_erl`](`e:erts:run_erl_cmd.md`) in the ERTS documentation.

### to_erl

This program is used to attach to a running Erlang runtime system, started with
`run_erl`.

```text
Usage: to_erl [pipe_name | pipe_dir]
```

Here `pipe_name` defaults to `/tmp/erlang.pipe.N`.

To disconnect from the shell without exiting the Erlang system, type `Ctrl-D`.

### start_erl

This program starts the Erlang emulator with parameters `-boot` and `-config`
set. It reads data about where these files are located from a file named
`start_erl.data`, which is located in `<RELDIR>`. Each new release introduces a
new data file. This file is automatically generated by the release handler in
Erlang.

The following script illustrates the behaviour of the program:

```text
#!/bin/sh
#
# This program is called by run_erl. It starts
# the Erlang emulator and sets -boot and -config parameters.
# It should only be used at an embedded target system.
#
# Usage: start_erl RootDir RelDir DataFile [ErlFlags ...]
#
ROOTDIR=$1
shift
RELDIR=$1
shift
DataFile=$1
shift

ERTS_VSN=`awk '{print $1}' $DataFile`
VSN=`awk '{print $2}' $DataFile`

BINDIR=$ROOTDIR/erts-$ERTS_VSN/bin
EMU=beam
PROGNAME=`echo $0 | sed 's/.*\///'`
export EMU
export ROOTDIR
export BINDIR
export PROGNAME
export RELDIR

exec $BINDIR/erlexec -boot $RELDIR/$VSN/start -config $RELDIR/$VSN/sys $*
```

If a diskless and/or read-only client node with the SASL configuration parameter
`static_emulator` set to `true` is about to start, the `-boot` and `-config`
flags must be changed.

As such a client cannot read a new `start_erl.data` file (the file cannot be
changed dynamically). The boot and config files are always fetched from the same
place (but with new contents if a new release has been installed).

The `release_handler` copies these files to the `bin` directory in the client
directory at the master nodes whenever a new release is made permanent.

Assuming the same `CLIENTDIR` as above, the last line is to look like:

```text
exec $BINDIR/erlexec -boot $CLIENTDIR/bin/start \
     -config $CLIENTDIR/bin/sys $*
```

[Creating and Upgrading a Target System]: create_target.md
