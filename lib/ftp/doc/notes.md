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
# FTP Release Notes

## Ftp 1.2.1

### Fixed Bugs and Malfunctions

- Replaced unintentional Erlang Public License 1.1 headers in some files with
  the intended Apache License 2.0 header.

  Own Id: OTP-18815 Aux Id: PR-7780

## Ftp 1.2

### Fixed Bugs and Malfunctions

- Fixes the documentation for the `ftp` module and updates the typing of `ftp`
  functions that return errors.

  The documentation has been improved and the types of the functions are now
  read from source code, instead of being hard-coded in XML.

  Functions returning errors of the form `{error, Reason :: 'ehost' | ...}` are
  now similar to other modules, i.e., `{error, Reason :: term()}`. If one wants
  to understand the error, one must call the function
  `ftp:formaterror({error, Reason})`.

  Own Id: OTP-18359 Aux Id: PR-6545

### Improvements and New Features

- Deprecates `dbg:stop_clear/0` because it is simply a function alias to
  `dbg:stop/0`

  Own Id: OTP-18478 Aux Id: GH-6903

- Remove deprecated functions in OTP-26

  Own Id: OTP-18541

## Ftp 1.1.4

### Improvements and New Features

- Replace size/1 with either tuple_size/1 or byte_size/1

  The [`size/1`](`size/1`) BIF is not optimized by the JIT, and its use can
  result in worse types for Dialyzer.

  When one knows that the value being tested must be a tuple,
  [`tuple_size/1`](`tuple_size/1`) should always be preferred.

  When one knows that the value being tested must be a binary,
  [`byte_size/1`](`byte_size/1`) should be preferred. However,
  [`byte_size/1`](`byte_size/1`) also accepts a bitstring (rounding up size to a
  whole number of bytes), so one must make sure that the call to `byte_size/` is
  preceded by a call to [`is_binary/1`](`is_binary/1`) to ensure that bitstrings
  are rejected. Note that the compiler removes redundant calls to
  [`is_binary/1`](`is_binary/1`), so if one is not sure whether previous code
  had made sure that the argument is a binary, it does not harm to add an
  [`is_binary/1`](`is_binary/1`) test immediately before the call to
  [`byte_size/1`](`byte_size/1`).

  Own Id: OTP-18432 Aux Id:
  GH-6672,PR-6793,PR-6784,PR-6787,PR-6785,PR-6682,PR-6800,PR-6797,PR-6798,PR-6799,PR-6796,PR-6813,PR-6671,PR-6673,PR-6684,PR-6694,GH-6677,PR-6696,PR-6670,PR-6674

## Ftp 1.1.3

### Fixed Bugs and Malfunctions

- Fixes calls to `ftp:nlist/2` returning `{error, epath}` when the file / folder
  exists

  Own Id: OTP-18409 Aux Id: PR-6721,ERIERL-908

## Ftp 1.1.2

### Fixed Bugs and Malfunctions

- fix unexpected result `ok` when calling `ftp:nlist` repeatedly

  Own Id: OTP-18252 Aux Id: GH-5823

## Ftp 1.1.1

### Fixed Bugs and Malfunctions

- An unexpected timeout message on the FTP control channel was observed in a
  real system and could not be associated with anything that was expected to
  happen, so we will ignore but info log such unexpected messages.

  Own Id: OTP-17989 Aux Id: ERIERL-767

## Ftp 1.1

### Fixed Bugs and Malfunctions

- Use OTP supervisor as intended, avoiding surprising behavior as the killing of
  the user's process. Also, FTP state handling logic is improved to avoid race
  conditions that could result in unexpected errors.

  Own Id: OTP-16926 Aux Id: ERL-1450, GH-4473

- Missing runtime dependencies has been added to this application.

  Own Id: OTP-17243 Aux Id: PR-4557

### Improvements and New Features

- Add support for FTPES (explicit FTP over TLS).

  Own Id: OTP-15523 Aux Id: OTP-15352, PR-1968

## Ftp 1.0.5

### Fixed Bugs and Malfunctions

- Avoid timing issue when setting active once on a socket that is being closed
  by the peer.

  Own Id: OTP-16734 Aux Id: OTP-16697, ERIERL-496

## Ftp 1.0.4.1

### Fixed Bugs and Malfunctions

- Avoid timing issue when setting active once on a socket that is being closed
  by the peer.

  Own Id: OTP-16734 Aux Id: OTP-16697, ERIERL-496

## Ftp 1.0.4

### Fixed Bugs and Malfunctions

- A possibly infinite loop is removed.

  Own Id: OTP-16243 Aux Id: PR-2436, OTP-16056

### Improvements and New Features

- Removed compiler warnings.

  Own Id: OTP-16318 Aux Id: OTP-16183

## Ftp 1.0.3

### Fixed Bugs and Malfunctions

- A possibly infinite loop when receiving messages divided in parts is removed.

  Own Id: OTP-16056

## Ftp 1.0.2.2

### Fixed Bugs and Malfunctions

- A possibly infinite loop is removed.

  Own Id: OTP-16243 Aux Id: PR-2436, OTP-16056

## Ftp 1.0.2.1

### Fixed Bugs and Malfunctions

- A possibly infinite loop when receiving messages divided in parts is removed.

  Own Id: OTP-16056

## Ftp 1.0.2

### Fixed Bugs and Malfunctions

- Fixed timing related bug that could make ftp functions behave badly.

  Own Id: OTP-15659 Aux Id: ERIERL-316

## Ftp 1.0.1

### Fixed Bugs and Malfunctions

- Improved documentation.

  Own Id: OTP-15190

## FTP 1.0

### First released version

- Inets application was split into multiple smaller protocol specific
  applications. The FTP application is a standalone FTP client with the same
  functionality as FTP client in Inets.

  Own Id: OTP-14113
