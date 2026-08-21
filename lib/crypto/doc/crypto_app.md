<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2023-2026. All Rights Reserved.

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
# Crypto Application

## Description

The purpose of the Crypto application is to provide an Erlang API to
cryptographic functions, see `m:crypto`. Note that the API is on a fairly low
level and there are some corresponding API functions available in
`m:public_key`, on a higher abstraction level, that uses the crypto application
in its implementation.

## DEPENDENCIES

The current crypto implementation uses nifs to interface OpenSSLs crypto library
and may work with limited functionality with as old versions as _OpenSSL_
0\.9.8c. FIPS mode support requires at least version 1.0.1 and a FIPS capable
OpenSSL installation. We recommend using a version that is officially supported
by the OpenSSL project. API compatible backends like LibreSSL should also work.

The crypto app is tested daily with at least one version of each of the OpenSSL
1.0.1, 1.0.2, 1.1.0, 1.1.1 and 3.0. FIPS mode is also tested for 1.0.1, 1.0.2
and 3.0.

Using OpenSSL 3.0 with Engines is supported since OTP 26.2.

Source releases of OpenSSL can be downloaded from the
[OpenSSL](http://www.openssl.org) project home page, or mirror sites listed
there.

## CONFIGURATION

The following configuration parameters are defined for the crypto application.
See [`app(3)`](`e:kernel:app.md`) for more information about configuration
parameters.

[](){: #fips_mode }
- **`fips_mode = boolean()`** - Specifies whether to run crypto in FIPS mode.
  This setting will take effect when the nif module is loaded. If FIPS mode is
  requested but not available at run time the nif module and thus the crypto
  module will fail to load. This mechanism prevents the accidental use of
  non-validated algorithms.

- **`rand_cache_size = integer()`** - Sets the cache size in bytes to use by
  [`crypto:rand_seed_alg(crypto_cache)` ](`crypto:rand_seed_alg/1`)and
  [`crypto:rand_seed_alg_s(crypto_cache)` ](`crypto:rand_seed_alg_s/1`). This
  parameter is read when a seed function is called, and then kept in generators
  state object. It has a rather small default value that causes reads of strong
  random bytes about once per hundred calls for a random value. The set value is
  rounded up to an integral number of words of the size these seed functions
  use.

> #### Change {: .info }
>
> From Erlang/OTP 29, in [interactive mode](`e:system:system_principles.md#code_loading`),
> application `crypto` will be automatically loaded if needed when module
> `crypto` is loaded. In [embedded mode](`e:system:system_principles.md#code_loading`),
> module `crypto` will fail to load if application `crypto` has not been
> loaded by the boot script. This is all done to make sure `crypto` is not
> initialized with incorrect configuration parameters.
>
> In Erlang/OTP 28 and earlier, no automatic loading of the application is done.
> If module `crypto` is then loaded (by a first call for example) before the
> application has been loaded, it will initialize with possibly incorrect
> configuration parameters. In particular, `fips_mode` will then default to
> `false` even if configured to be `true`.

## SEE ALSO

application(3)
