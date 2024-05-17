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
# Examples

The following examples use the utility function `ssh:start/0` to start all
needed applications (`crypto`, `public_key`, and `ssh`). All examples are run in
an Erlang shell, or in a bash shell, using **OpenSSH** to illustrate how the `ssh`
application can be used. The examples are run as the user `otptest` on a local
network where the user is authorized to log in over `ssh` to the host **ssh.example.com**.

If nothing else is stated, it is presumed that the `otptest` user has an entry
in the **authorized\_keys** file of **ssh.example.com** (allowed to log in over `ssh` without
entering a password). Also, **ssh.example.com** is a known host in the `known_hosts` file
of the user `otptest`. This means that host-verification can be done without
user-interaction.

## Using the Erlang ssh Terminal Client

The user `otptest`, which has bash as default shell, uses the `ssh:shell/1`
client to connect to the **OpenSSH** daemon running on a host called **ssh.example.com**:

```erlang
1> ssh:start().
ok
2> {ok, S} = ssh:shell("ssh.example.com").
otptest@ssh.example.com:> pwd
/home/otptest
otptest@ssh.example.com:> exit
logout
3>
```

## Running an Erlang ssh Daemon

The [`system_dir`](`t:ssh_file:system_dir_daemon_option/0`) option must be a
directory containing a host key file and it defaults to `/etc/ssh`. For details,
see Section Configuration in [ssh](ssh_app.md).

> #### Note {: .info }
>
> Normally, the `/etc/ssh` directory is only readable by root.

The option [`user_dir`](`t:ssh_file:user_dir_common_option/0`) defaults to
directory `~/.ssh`.

_Step 1._ To run the example without root privileges, generate new keys and host
keys:

```text
$bash> ssh-keygen -t rsa -f /tmp/ssh_daemon/ssh_host_rsa_key
[...]
$bash> ssh-keygen -t rsa -f /tmp/otptest_user/.ssh/id_rsa
[...]
```

_Step 2._ Create the file `/tmp/otptest_user/.ssh/authorized_keys` and add the
content of `/tmp/otptest_user/.ssh/id_rsa.pub`.

[](){: #start-daemon-step3 }

_Step 3._ Start the Erlang `ssh` daemon:

```erlang
1> ssh:start().
ok
2> {ok, Sshd} = ssh:daemon(8989, [{system_dir, "/tmp/ssh_daemon"},
                                  {user_dir, "/tmp/otptest_user/.ssh"}]).
{ok,<0.54.0>}
3>
```

_Step 4._ Use the **OpenSSH** client from a shell to connect to the Erlang `ssh`
daemon:

```text
$bash> ssh ssh.example.com -p 8989  -i /tmp/otptest_user/.ssh/id_rsa \
                  -o UserKnownHostsFile=/tmp/otptest_user/.ssh/known_hosts
The authenticity of host 'ssh.example.com' can't be established.
RSA key fingerprint is 14:81:80:50:b1:1f:57:dd:93:a8:2d:2f:dd:90:ae:a8.
Are you sure you want to continue connecting (yes/no)? yes
Warning: Permanently added 'ssh.example.com' (RSA) to the list of known hosts.
Eshell V5.10  (abort with ^G)
1>
```

There are two ways of shutting down an `ssh` daemon, see _Step 5a_ and _Step
5b_.

_Step 5a._ Shut down the Erlang `ssh` daemon so that it stops the listener but
leaves existing connections, started by the listener, operational:

```erlang
3> ssh:stop_listener(Sshd).
ok
4>
```

_Step 5b._ Shut down the Erlang `ssh` daemon so that it stops the listener and
all connections started by the listener:

```erlang
3> ssh:stop_daemon(Sshd).
ok
4>
```

## One-Time Execution

[](){: #simple-client-example }

### Erlang client contacting OS standard ssh server

In the following example, the Erlang shell is the client process that receives
the channel replies as Erlang messages.

Do an one-time execution of a remote OS command ("pwd") over `ssh` to the ssh
server of the OS at the host "ssh.example.com":

```erlang
1> ssh:start().
ok
2> {ok, ConnectionRef} = ssh:connect("ssh.example.com", 22, []).
{ok,<0.57.0>}
3> {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity).
{ok,0}
4> success = ssh_connection:exec(ConnectionRef, ChannelId, "pwd", infinity).
5> flush(). % Get all pending messages. NOTE: ordering may vary!
Shell got {ssh_cm,<0.57.0>,{data,0,0,<<"/home/otptest\n">>}}
Shell got {ssh_cm,<0.57.0>,{eof,0}}
Shell got {ssh_cm,<0.57.0>,{exit_status,0,0}}
Shell got {ssh_cm,<0.57.0>,{closed,0}}
ok
6> ssh:connection_info(ConnectionRef, channels).
{channels,[]}
7>
```

See `m:ssh_connection` and `ssh_connection:exec/4` for finding documentation of
the channel messages.

To collect the channel messages in a program, use `receive...end` instead of
`flush/1`:

```erlang
5> receive
5>     {ssh_cm, ConnectionRef, {data, ChannelId, Type, Result}} when Type == 0 ->
5>         {ok,Result}
5>     {ssh_cm, ConnectionRef, {data, ChannelId, Type, Result}} when Type == 1 ->
5>         {error,Result}
5> end.
{ok,<<"/home/otptest\n">>}
6>
```

Note that only the exec channel is closed after the one-time execution. The
connection is still up and can handle previously opened channels. It is also
possible to open a new channel:

```erlang
% try to open a new channel to check if the ConnectionRef is still open
7> {ok, NewChannelId} = ssh_connection:session_channel(ConnectionRef, infinity).
{ok,1}
8>
```

To close the connection, call the function
[`ssh:close(ConnectionRef)`](`ssh:close/1`). As an alternative, set the option
[`{idle_time, 1}`](`t:ssh:max_idle_time_common_option/0`) when opening the
connection. This will cause the connection to be closed automatically when there
are no channels open for the specified time period, in this case 1 ms.

### OS standard client and Erlang daemon (server)

An Erlang SSH daemon could be called for one-time execution of a "command". The
"command" must be as if entered into the erlang shell, that is a sequence of
Erlang [expressions](`e:system:expressions.md`) ended by a period (.). Variables
bound in that sequence will keep their bindings throughout the expression
sequence. The bindings are disposed when the result is returned.

Here is an example of a suitable expression sequence:

```erlang
A=1, B=2, 3 == (A + B).
```

It evaluates to `true` if submitted to the Erlang daemon started in
[Step 3](using_ssh.md#start-daemon-step3) above:

```text
$bash> ssh ssh.example.com -p 8989 "A=1, B=2, 3 == (A + B)."
true
$bash>
```

The same example but now using the Erlang ssh client to contact the Erlang
server:

```erlang
1> {ok, ConnectionRef} = ssh:connect("ssh.example.com", 8989, []).
{ok,<0.216.0>}
2> {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity).
{ok,0}
3> success = ssh_connection:exec(ConnectionRef, ChannelId,
                                 "A=1, B=2, 3 == (A + B).",
                                 infinity).
success
4> flush().
Shell got {ssh_cm,<0.216.0>,{data,0,0,<<"true">>}}
Shell got {ssh_cm,<0.216.0>,{exit_status,0,0}}
Shell got {ssh_cm,<0.216.0>,{eof,0}}
Shell got {ssh_cm,<0.216.0>,{closed,0}}
ok
5>
```

Note that Erlang shell specific functions and control sequences like for example
`h().` are not supported.

### I/O from a function called in an Erlang ssh daemon

Output to stdout on the server side is also displayed as well as the resulting
term from the function call:

```text
$bash> ssh ssh.example.com -p 8989 'io:format("Hello!~n~nHow are ~p?~n",[you]).'
Hello!

How are you?
ok
$bash>
```

And similar for reading from stdin. As an example we use `io:read/1` which
displays the argument as a prompt on stdout, reads a term from stdin and returns
it in an ok-tuple:

```text
$bash> ssh ssh.example.com -p 8989 'io:read("write something: ").'
write something: [a,b,c].
{ok,[a,b,c]}
$bash>
```

The same example but using the Erlang ssh client:

```erlang

Eshell V10.5.2  (abort with ^G)
1> ssh:start().
ok
2> {ok, ConnectionRef} = ssh:connect(loopback, 8989, []).
{ok,<0.92.0>}
3> {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity).
{ok,0}
4> success = ssh_connection:exec(ConnectionRef, ChannelId,
                                 "io:read(\"write something: \").",
                                 infinity).
success
5> flush().
Shell got {ssh_cm,<0.92.0>,{data,0,0,<<"write something: ">>}}
ok
% All data is sent as binaries with string contents:
6> ok = ssh_connection:send(ConnectionRef, ChannelId, <<"[a,b,c].">>).
ok
7> flush().
ok
%% Nothing is received, because the io:read/1
%% requires the input line to end with a newline.

%% Send a newline (it could have been included in the last send):
8> ssh_connection:send(ConnectionRef, ChannelId, <<"\n">>).
ok
9> flush().
Shell got {ssh_cm,<0.92.0>,{data,0,0,<<"{ok,[a,b,c]}">>}}
Shell got {ssh_cm,<0.92.0>,{exit_status,0,0}}
Shell got {ssh_cm,<0.92.0>,{eof,0}}
Shell got {ssh_cm,<0.92.0>,{closed,0}}
ok
10>
```

### Configuring the server's (daemon's) command execution

Every time a daemon [is started](using_ssh.md#running-an-erlang-ssh-daemon), it
enables one-time execution of commands as described in the
[previous section](using_ssh.md#simple-client-example) unless explicitly
disabled.

There is often a need to configure some other exec evaluator to tailor the input
language or restrict the possible functions to call. There are two ways of doing
this which will be shown with examples below. See
[ssh:daemon/2,3](`ssh:daemon/2`) and
[exec_daemon_option()](`t:ssh:exec_daemon_option/0`) for details.

Examples of the two ways to configure the exec evaluator:

1. Disable one-time execution.  
   To modify the daemon start example above to reject one-time execution
   requests, we change [Step 3](using_ssh.md#start-daemon-step3) by adding the
   option `{exec, disabled}` to:

```erlang
1> ssh:start().
ok
2> {ok, Sshd} = ssh:daemon(8989, [{system_dir, "/tmp/ssh_daemon"},
                                  {user_dir, "/tmp/otptest_user/.ssh"},
                                  {exec, disabled}
                                 ]).
{ok,<0.54.0>}
3>
```

A call to that daemon will return the text "Prohibited." on stderr (depending on
the client and OS), and the exit status 255:

```text
$bash> ssh ssh.example.com -p 8989 "test."
Prohibited.
$bash> echo $?
255
$bash>
```

And the Erlang client library also returns the text "Prohibited." on data type 1
instead of the normal 0 and exit status 255:

```erlang
2> {ok, ConnectionRef} = ssh:connect(loopback, 8989, []).
{ok,<0.92.0>}
3> {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity).
{ok,0}
4> success = ssh_connection:exec(ConnectionRef, ChannelId, "test."
success
5> flush().
Shell got {ssh_cm,<0.106.0>,{data,0,1,<<"Prohibited.">>}}
Shell got {ssh_cm,<0.106.0>,{exit_status,0,255}}
Shell got {ssh_cm,<0.106.0>,{eof,0}}
Shell got {ssh_cm,<0.106.0>,{closed,0}}
ok
6>
```

1. Install an alternative evaluator.  
   Start the damon with a reference to a `fun()` that handles the evaluation:

```erlang
1> ssh:start().
ok
2> MyEvaluator = fun("1") -> {ok, some_value};
                    ("2") -> {ok, some_other_value};
                    ("3") -> {ok, V} = io:read("input erlang term>> "),
                             {ok, V};
                    (Err) -> {error,{bad_input,Err}}
                 end.
3> {ok, Sshd} = ssh:daemon(1234, [{system_dir, "/tmp/ssh_daemon"},
                                  {user_dir, "/tmp/otptest_user/.ssh"},
                                  {exec, {direct,MyEvaluator}}
                                 ]).
{ok,<0.275.0>}
4>
```

and call it:

```text
$bash> ssh localhost -p 1234 1
some_value
$bash> ssh localhost -p 1234 2
some_other_value
# I/O works:
$bash> ssh localhost -p 1234 3
input erlang term>> abc.
abc
# Check that Erlang evaluation is disabled:
$bash> ssh localhost -p 1234 1+ 2.
**Error** {bad_input,"1+ 2."}
$bash>
```

Note that spaces are preserved and that no point (.) is needed at the end - that
was required by the default evaluator.

The error return in the Erlang client (The text as data type 1 and exit_status
255):

```erlang
2> {ok, ConnectionRef} = ssh:connect(loopback, 1234, []).
{ok,<0.92.0>}
3> {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity).
{ok,0}
4> success = ssh_connection:exec(ConnectionRef, ChannelId, "1+ 2.").
success
5> flush().
Shell got {ssh_cm,<0.106.0>,{data,0,1,<<"**Error** {bad_input,\"1+ 2.\"}">>}}
Shell got {ssh_cm,<0.106.0>,{exit_status,0,255}}
Shell got {ssh_cm,<0.106.0>,{eof,0}}
Shell got {ssh_cm,<0.106.0>,{closed,0}}
ok
6>
```

The `fun()` in the exec option could take up to three arguments (`Cmd`, `User`
and `ClientAddress`). See the
[exec_daemon_option()](`t:ssh:exec_daemon_option/0`) for the details.

> #### Note {: .info }
>
> An old, discouraged and undocumented way of installing an alternative
> evaluator exists.
>
> It still works, but lacks for example I/O possibility. It is because of that
> compatibility we need the `{direct,...}` construction.

## SFTP Server

Start the Erlang `ssh` daemon with the SFTP subsystem:

```erlang
1> ssh:start().
ok
2> ssh:daemon(8989, [{system_dir, "/tmp/ssh_daemon"},
                     {user_dir, "/tmp/otptest_user/.ssh"},
                     {subsystems, [ssh_sftpd:subsystem_spec(
                                            [{cwd, "/tmp/sftp/example"}])
                                  ]}]).
{ok,<0.54.0>}
3>
```

Run the OpenSSH SFTP client:

```text
$bash> sftp -oPort=8989 -o IdentityFile=/tmp/otptest_user/.ssh/id_rsa \
            -o UserKnownHostsFile=/tmp/otptest_user/.ssh/known_hosts ssh.example.com
Connecting to ssh.example.com...
sftp> pwd
Remote working directory: /tmp/sftp/example
sftp>
```

## SFTP Client

Fetch a file with the Erlang SFTP client:

```erlang
1> ssh:start().
ok
2> {ok, ChannelPid, Connection} = ssh_sftp:start_channel("ssh.example.com", []).
{ok,<0.57.0>,<0.51.0>}
3> ssh_sftp:read_file(ChannelPid, "/home/otptest/test.txt").
{ok,<<"This is a test file\n">>}
```

## SFTP Client with TAR Compression

### Basic example

This is an example of writing and then reading a tar file:

```erlang
{ok,HandleWrite} = ssh_sftp:open_tar(ChannelPid, ?tar_file_name, [write]),
ok = erl_tar:add(HandleWrite, .... ),
ok = erl_tar:add(HandleWrite, .... ),
...
ok = erl_tar:add(HandleWrite, .... ),
ok = erl_tar:close(HandleWrite),

%% And for reading
{ok,HandleRead} = ssh_sftp:open_tar(ChannelPid, ?tar_file_name, [read]),
{ok,NameValueList} = erl_tar:extract(HandleRead,[memory]),
ok = erl_tar:close(HandleRead),
```

### Example with encryption

The previous [Basic example](using_ssh.md#basic-example) can be extended with
encryption and decryption as follows:

```erlang
%% First three parameters depending on which crypto type we select:
Key = <<"This is a 256 bit key. abcdefghi">>,
Ivec0 = crypto:strong_rand_bytes(16),
DataSize = 1024,  % DataSize rem 16 = 0 for aes_cbc

%% Initialization of the CryptoState, in this case it is the Ivector.
InitFun = fun() -> {ok, Ivec0, DataSize} end,

%% How to encrypt:
EncryptFun =
    fun(PlainBin,Ivec) ->
        EncryptedBin = crypto:block_encrypt(aes_cbc256, Key, Ivec, PlainBin),
        {ok, EncryptedBin, crypto:next_iv(aes_cbc,EncryptedBin)}
    end,

%% What to do with the very last block:
CloseFun =
    fun(PlainBin, Ivec) ->
        EncryptedBin = crypto:block_encrypt(aes_cbc256, Key, Ivec,
                                            pad(16,PlainBin) %% Last chunk
                                           ),
       {ok, EncryptedBin}
    end,

Cw = {InitFun,EncryptFun,CloseFun},
{ok,HandleWrite} = ssh_sftp:open_tar(ChannelPid, ?tar_file_name, [write,{crypto,Cw}]),
ok = erl_tar:add(HandleWrite, .... ),
ok = erl_tar:add(HandleWrite, .... ),
...
ok = erl_tar:add(HandleWrite, .... ),
ok = erl_tar:close(HandleWrite),

%% And for decryption (in this crypto example we could use the same InitFun
%% as for encryption):
DecryptFun =
    fun(EncryptedBin,Ivec) ->
        PlainBin = crypto:block_decrypt(aes_cbc256, Key, Ivec, EncryptedBin),
       {ok, PlainBin, crypto:next_iv(aes_cbc,EncryptedBin)}
    end,

Cr = {InitFun,DecryptFun},
{ok,HandleRead} = ssh_sftp:open_tar(ChannelPid, ?tar_file_name, [read,{crypto,Cw}]),
{ok,NameValueList} = erl_tar:extract(HandleRead,[memory]),
ok = erl_tar:close(HandleRead),
```

[](){: #usersguide_creating_a_subsystem }

## Creating a Subsystem

A small `ssh` subsystem that echoes N bytes can be implemented as shown in the
following example:

```erlang
-module(ssh_echo_server).
-behaviour(ssh_server_channel). % replaces ssh_daemon_channel
-record(state, {
	  n,
	  id,
	  cm
	 }).
-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2]).

init([N]) ->
    {ok, #state{n = N}}.

handle_msg({ssh_channel_up, ChannelId, ConnectionManager}, State) ->
    {ok, State#state{id = ChannelId,
		     cm = ConnectionManager}}.

handle_ssh_msg({ssh_cm, CM, {data, ChannelId, 0, Data}}, #state{n = N} = State) ->
    M = N - size(Data),
    case M > 0 of
	true ->
	   ssh_connection:send(CM, ChannelId, Data),
	   {ok, State#state{n = M}};
	false ->
	   <<SendData:N/binary, _/binary>> = Data,
           ssh_connection:send(CM, ChannelId, SendData),
           ssh_connection:send_eof(CM, ChannelId),
	   {stop, ChannelId, State}
    end;
handle_ssh_msg({ssh_cm, _ConnectionManager,
		{data, _ChannelId, 1, Data}}, State) ->
    error_logger:format(standard_error, " ~p~n", [binary_to_list(Data)]),
    {ok, State};

handle_ssh_msg({ssh_cm, _ConnectionManager, {eof, _ChannelId}}, State) ->
    {ok, State};

handle_ssh_msg({ssh_cm, _, {signal, _, _}}, State) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    {ok, State};

handle_ssh_msg({ssh_cm, _, {exit_signal, ChannelId, _, _Error, _}},
	       State) ->
    {stop, ChannelId,  State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, _Status}}, State) ->
    {stop, ChannelId, State}.

terminate(_Reason, _State) ->
    ok.
```

The subsystem can be run on the host **ssh.example.com** with the generated keys, as
described in Section
[Running an Erlang ssh Daemon](using_ssh.md#running-an-erlang-ssh-daemon):

```erlang
1> ssh:start().
ok
2> ssh:daemon(8989, [{system_dir, "/tmp/ssh_daemon"},
                     {user_dir, "/tmp/otptest_user/.ssh"}
                     {subsystems, [{"echo_n", {ssh_echo_server, [10]}}]}]).
{ok,<0.54.0>}
3>
```

```erlang
1> ssh:start().
ok
2> {ok, ConnectionRef} = ssh:connect("ssh.example.com", 8989,
                                    [{user_dir, "/tmp/otptest_user/.ssh"}]).
 {ok,<0.57.0>}
3> {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity).
4> success = ssh_connection:subsystem(ConnectionRef, ChannelId, "echo_n", infinity).
5> ok = ssh_connection:send(ConnectionRef, ChannelId, "0123456789", infinity).
6> flush().
{ssh_msg, <0.57.0>, {data, 0, 1, "0123456789"}}
{ssh_msg, <0.57.0>, {eof, 0}}
{ssh_msg, <0.57.0>, {closed, 0}}
7> {error, closed} = ssh_connection:send(ConnectionRef, ChannelId, "10", infinity).
```

See also `m:ssh_client_channel` (replaces ssh_channel(3)).
