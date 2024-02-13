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
# Terminology

## General Information

In the following terms that may cause confusion are explained.

## The term "user"

A "user" is a term that everyone understands intuitively. However, the
understandings may differ which can cause confusion.

The term is used differently in [OpenSSH](http://www.openssh.com) and SSH in
Erlang/OTP. The reason is the different environments and use cases that are not
immediately obvious.

This chapter aims at explaining the differences and giving a rationale for why
Erlang/OTP handles "user" as it does.

### In OpenSSH

Many have been in contact with the command 'ssh' on a Linux machine (or similar)
to remotly log in on another machine. One types

```text
ssh host
```

to log in on the machine named `host`. The command prompts for your password on
the remote `host` and then you can read, write and execute as your _user name_
has rights on the remote `host`. There are stronger variants with
pre-distributed keys or certificates, but that are for now just details in the
authentication process.

You could log in as the user `anotheruser` with

```text
ssh anotheruser@host
```

and you will then be enabled to act as `anotheruser` on the `host` if authorized
correctly.

So what does _"your user name has rights"_ mean? In a UNIX/Linux/etc context it
is exactly as that context: The _user_ could read, write and execute programs
according to the OS rules. In addition, the user has a home directory (`$HOME`)
and there is a `$HOME/.ssh/` directory with ssh-specific files.

#### SSH password authentication

When SSH tries to log in to a host, the ssh protocol communicates the user name
(as a string) and a password. The remote ssh server checks that there is such a
user defined and that the provided password is acceptable.

If so, the user is authorized.

#### SSH public key authentication

This is a stronger method where the ssh protocol brings the user name, the
user's public key and some cryptographic information which we could ignore here.

The ssh server on the remote host checks:

- That the _user_ has a home directory,
- that home directory contains a .ssh/ directory and
- the .ssh/ directory contains the public key just received in the
  `authorized_keys` file

if so, the user is authorized.

#### The SSH server on UNIX/Linux/etc after a successful authentication

After a successful incoming authentication, a new process runs as the just
authenticated user.

Next step is to start a service according to the ssh request. In case of a
request of a shell, a new one is started which handles the OS-commands that
arrives from the client (that's "you").

In case of a sftp request, an sftp server is started in with the user's rights.
So it could read, write or delete files if allowed for that user.

### In Erlang/OTP SSH

For the Erlang/OTP SSH server the situation is different. The server executes in
an Erlang process in the Erlang emulator which in turn executes in an OS
process. The emulator does not try to change its user when authenticated over
the SSH protocol. So the remote user name is only for authentication purposes in
the Erlang/OTP SSH application.

#### Password authentication in Erlang SSH

The Erlang/OTP SSH server checks the user name and password in the following
order:

1. If a [`pwdfun`](`m:ssh#option-pwdfun`) is defined, that one is called and the
   returned boolean is the authentication result.
1. Else, if the [`user_passwords`](`m:ssh#option-user_passwords`) option is
   defined and the username and the password matches, the authentication is a
   success.
1. Else, if the option [`password`](`m:ssh#option-password`) is defined and
   matches the password the authentication is a success. Note that the use of
   this option is not recommended in non-test code.

#### Public key authentication in Erlang SSH

The user name, public key and cryptographic data (a signature) that is sent by
the client, are used as follows (some steps left out for clearity):

1. A callback module is selected using the options
   [`key_cb`](`t:ssh:key_cb_common_option/0`).
1. The callback module is used to check that the provided public key is one of
   the user's pre-stored. In case of the default callback module, the files
   `authorized_keys` and `authorized_keys2` are searched in a directory found in
   the following order:

- If the option [`user_dir_fun`](`t:ssh_file:user_dir_fun_common_option/0`) is
  defined, that fun is called and the returned directory is used,
- Else, If the option [`user_dir`](`t:ssh_file:user_dir_common_option/0`) is
  defined, that directory is used,
- Else the subdirectory `.ssh` in the home directory of the user executing the
  OS process of the Erlang emulator is used.

If the provided public key is not found, the authentication fails.

1. Finally, if the provided public key is found, the signature provided by the
   client is checked with the public key.

#### The Erlang/OTP SSH server after a successful authentication

After a successful authentication an _Erlang process_ is handling the service
request from the remote ssh client. The rights of that process are those of the
user of the OS process running the Erlang emulator.

If a shell service request arrives to the server, an _Erlang shell_ is opened in
the server's emulator. The rights in that shell is independent of the just
authenticated user.

In case of an sftp request, an sftp server is started with the rights of the
user of the Erlang emulator's OS process. So with sftp the authenticated user
does not influence the rights.

So after an authentication, the user name is not used anymore and has no
influence.
