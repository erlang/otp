<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

SPDX-FileCopyrightText: Copyright (C) 2025 Richard Carlsson <carlsson.richard@gmail.com>
Copyright Ericsson AB 2026. All Rights Reserved.

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

### LEGACY (D)TLS Versions TLS-1.0, TLS-1.1 and DTLS-1.0

TLS-1.0, TLS-1.1 and DTLS-1.0 are effectively end-of-life, with major
industry players disabling them by late 2025 due to security
vulnerabilities, these legacy protocol versions have not been
supported by default for many years and are hereby formally
deprecated.

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

