<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2026. All Rights Reserved.
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

### Old aliases for type tests in guards

The type test aliases `atom/1`, `binary/1`, `float/1`, `function/1`,
`integer/1`, `list/1`, `number/1`, `pid/1`, `port/1`, `record/2`,
`reference/1`, and `tuple/1`, for the respective tests `is_atom/1`,
`is_binary/1`, `is_float/1`, ..., `is_tuple/2`, have been deprecated since
before OTP R13 and have been scheduled for removal in OTP 30. Note that
these aliases could only be used as guard tests at the top level. For
example, in `{X, Y} when float(X), X =:= float(Y) -> ...` the first use of
`float/1` is a type test alias, but the second use is the normal
integer-to-float conversion BIF, because it occurs inside the `=:=`. The
aliases were never recognized outside guards.

### Distribution Control Messages

#### `ALIAS_SEND` and `ALIAS_SEND_TT`

The [`ALIAS_SEND`](`e:erts:erl_dist_protocol.md#ALIAS_SEND`) and
[`ALIAS_SEND_TT`](`e:erts:erl_dist_protocol.md#ALIAS_SEND_TT`) distribution
control messages are as of OTP 28 deprecated and have been scheduled for removal in
OTP 30. The support for these control messages are indicated by the
[`DFLAG_ALIAS`](`e:erts:erl_dist_protocol.md#DFLAG_ALIAS`) distribution flag.

The `ALIAS_SEND` and `ALIAS_SEND_TT` control messages are as of OTP 28 replaced
by the [`ALTACT_SIG_SEND`](`e:erts:erl_dist_protocol.md#ALTACT_SIG_SEND`)
control message. Support for the `ALTACT_SIG_SEND` control message is indicated
by the [`DFLAG_ALTACT_SIG`](`e:erts:erl_dist_protocol.md#DFLAG_ALTACT_SIG`)
distribution flag.

### Archives

The following features of archives will be removed:

* Using archives for packaging a single application or parts of a single application
  into an archive file that is included in the code path.

* All functionality to handle archives in module `m:erl_prim_loader`.

* The `-code_path_choice` flag for `erl`.

The functionality to use a single archive file in Escripts is **not**
deprecated and will continue to work.  However, to access files in the
archive, the `escript:extract/2` function has to be used.

### LEGACY (D)TLS Versions TLS-1.0, TLS-1.1 and DTLS-1.0

TLS-1.0, TLS-1.1 and DTLS-1.0 are effectively end-of-life, with major
industry players disabling them by late 2025 due to security
vulnerabilities, hence we plan to remove support for these protocol
versions in OTP 30.

### LEGACY TLS Option Handling in Erlang Distribution over TLS

All TLS options specified for Erlang distribution as `ERL_FLAGS` on
the format `{ssl_dist_opt, Value}`.  After OTP 20 only the
`ssl_dist_optfile` flag should be used to configure TLS for Erlang
distribution and most options can only be configured that way anyway.

### SSH zlib Compression Algorithm

The use of the `zlib` compression algorithm in the SSH application is
deprecated. Use `none` or `zlib@openssh.com` instead. The `zlib`
algorithm has not been included in the default SSH algorithms for some
time and support for it in SSH is scheduled for removal in OTP 30.0.
