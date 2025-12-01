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

# Vulnerabilities

[](){: #vulnerabilities }

## Introduction

This section describes how Erlang/OTP reports vulnerabilities for Erlang/OTP
CVEs and third party dependencies on which Erlang/OTP builds upon.

Erlang/OTP reports all vulnerabilities using the [OpenVEX
specification](https://github.com/openvex/spec). This specification allows to
easily describe which CVEs affect which Erlang/OTP versions and specific OTP
applications. It also records which CVEs from third parties affect (or do not
affect) Erlang/OTP.

Erlang/OTP releases OpenVEX statements under `vex/otp-<version>.openvex.json` in
the [Erlang/OTP Github repository](https://github.com/erlang/otp) in the `master` branch, where
`<version>` corresponds to the number of the Erlang/OTP release.

## Erlang/OTP VEX Statements

Erlang/OTP OpenVEX statements specify which Erlang/OTP versions are affected/fixed (e.g.,
`pkg:otp/erlang@27.3.1`), as well as the specific Erlang/OTP application number
of all affected versions (e.g., `pkg:otp/ssh@5.2.9`).

As an example, a snippet of the `vex/otp-27.openvex.json` contains the
vulnerability identified by `CVE-2025-32433`, following by the status of the
vulnerability (`affected`), the affected Erlang/OTP releases, namely `27.3.1`
and `27.3.2`, and the Erlang/OTP application that was vulnerable, `ssh@5.2.9`.
The affected versions are reported using the release version and the
application because it is possible to update the application independently
from the release.
In some cases, there may be an optional action statement that describes a workaround
to avoid the mentioned vulnerability.

```
{
  "vulnerability": {
    "name": "CVE-2025-32433"
  },
  "timestamp": "2025-06-18T12:18:16.661272703+02:00",
  "products": [
       { "@id": "pkg:otp/erlang@27.3.1" },
       { "@id": "pkg:otp/erlang@27.3.2" },
       { "@id": "pkg:otp/ssh@5.2.9" }
  ],
  "status": "affected",
  "action_statement": "A temporary workaround involves disabling the SSH server or to prevent access via firewall rules.",
  "action_statement_timestamp": "2025-06-18T12:18:16.661272703+02:00"
},
```

The fixed version will be reported in a similar fashion as follows, in the same document.
As an example, there is a new statement for `CVE-2025-32433` with status `fixed`,
that links to the versions that do not suffer from `CVE-2025-32433`, namely
`erlang@27.3.3` and `otp/ssh@5.2.10`. 

```
{
  "vulnerability": {
    "name": "CVE-2025-32433"
  },
  "timestamp": "2025-06-18T12:18:16.676540081+02:00",
  "products": [
     { "@id": "pkg:otp/erlang@27.3.3" },
     { "@id": "pkg:otp/ssh@5.2.10" }
   ],
  "status": "fixed"
},
```

## Third Party VEX Statements

Erlang/OTP generates statements for 3rd parties from which the project depends
on. It is really important to understand the scope of the third party
applications, since Erlang/OTP vendors some libraries as part of the runtime.

Vendoring means that Erlang/OTP code contains a local copy of a library.
There are numerous use cases for why this is necessary, and we will not cover the use cases here.

**This excludes dynamically or statically linked libraries during the Erlang/OTP build process. For instance, any security related Erlang application will rely on dynamically or statically linked version of OpenSSL cryptolib.**

Erlang/OTP reports vulnerabilities for any source code that is vulnerable and
included in the Erlang/OTP release.

The OpenVEX statements for our third party libraries specify the affected/fixed
version using the commit SHA1 from their respective repository. This is simply
because our third party dependencies are in C/C++ and vulnerability scanners
such as OSV report vulnerabilities in ranges.

As an example, we mention that the OpenSSL code that Erlang/OTP vendors
is not susceptible for `CVE-2023-6129`, as follows:

```
{
  "vulnerability": {
    "name": "CVE-2023-6129"
  },
  "timestamp": "2025-06-18T12:18:16.47247833+02:00",
  "products": [
     { "@id": "pkg:github/openssl/openssl@01d5e2318405362b4de5e670c90d9b40a351d053" }
  ],
  "status": "not_affected",
  "justification": "vulnerable_code_not_present"
}
```

Diving into the example, this means that Erlang/OTP vendors a version of `openssl` taken from commit `01d5e2318405362b4de5e670c90d9b40a351d053` from the repository `https://github.com/openssl/openssl/commit/01d5e2318405362b4de5e670c90d9b40a351d053` (version of OpenSSL 3.1.4). The `openssl` code that Erlang/OTP vendors can be found in `./lib/erl_interface/src/openssl/` and `./erts/emulator/openssl/`. The OpenVEX statement claims that the code in those folders is not susceptible to `CVE-2023-6129`. The claim is towards **source code existing in Erlang/OTP**.

In other words, the `not_affected` status refers to the library that Erlang/OTP vendors for OpenSSL (the library that comes
included with Erlang/OTP). If you build Erlang/OTP and link to any OpenSSL version (e.g., 3.5.2 or even 3.1.4) during the building process,
your project has now a new build and runtime dependency and may be subject to `CVE-2023-6129`.

## Windows Binaries

For the time being, Erlang/OTP Windows binaries are not reported in the OpenVEX
specification.

