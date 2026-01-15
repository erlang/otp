<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->
# Introduction

This section describes how to build, install and patch Erlang/OTP on UNIX and Windows.

* **[Building and Installing Erlang/OTP](INSTALL.md)** - Describes how to build and install Erlang/OTP
  on any UNIX platform, that is Linux, macOS, any BSD, Solaris and so on.
* **[Cross Compiling Erlang/OTP](INSTALL-CROSS.md)** - Describes how to use a [cross compiler] to build
  Erlang/OTP on any UNIX platform.
* **[Building Erlang/OTP on Windows](INSTALL-WIN32.md)** - Describes how to build Erlang/OTP for on
  Windows 10 using WSL.

There are also various other guides for other OS located in the
[Erlang/OTP HOWTO folder](https://github.com/erlang/otp/blob/master/HOWTO/).

> #### Note {: .info }
>
> Depending on the Operating System and how familiar you are with using GNU configure/make
> it can be difficult to build Erlang/OTP. Therefore it is recommended to first go to
> <https://erlang.org/downloads> and check if a pre-built Erlang/OTP can be used.

If the purpose of building Erlang/OTP is to contribute to its development it is recommended
to have a look at
[Contributing to Erlang/OTP](https://github.com/erlang/otp/blob/master/CONTRIBUTING.md)
and [Developing Erlang/OTP](https://github.com/erlang/otp/blob/master/HOWTO/DEVELOPMENT.md).

[cross compiler]: https://en.wikipedia.org/wiki/Cross_compiler
