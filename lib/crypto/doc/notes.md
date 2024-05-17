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
# Crypto Release Notes

This document describes the changes made to the Crypto application.

## Crypto 5.5

### Improvements and New Features

- The documentation has been migrated to use Markdown and ExDoc.

  Own Id: OTP-18955 Aux Id: [PR-8026]

- Removed functions `crypto_dyn_iv_init/3` and `crypto_dyn_iv_update/3` which were marked as deprecated since OTP 25.

  Own Id: OTP-18973

- Add support for sm3 hash and hmac.

  Own Id: OTP-18975 Aux Id: [PR-6658]

- `OPENSSL_thread_stop`  is called when `crypto` is purged to not leak thread specific data.

  Own Id: OTP-18978 Aux Id: [PR-7809]

- Add SM4 block cipher implemented according to GB/T 32907-2016.

  Own Id: OTP-19005 Aux Id: [PR-8168]

- The existing function `ssl:key_exporter_materials/4` is now documented and supported.

  *** POTENTIAL INCOMPATIBILITY ***

  Own Id: OTP-19016 Aux Id: [PR-8233]

- Due to another attack on PKCS #1 v1.5 padding, known as the Marvin attack, about which we were alerted by Hubert Kario from Red Hat. You can find more details about the attack at
  https://people.redhat.com/~hkario/marvin/
  Functions that may be vulnerable are now deprecated. 
  
  Note that you might mitigate the problem 
  by using appropriate versions of OpenSSL together with our software, but we recommend not using them at all. 
  
  Also avoid using TLS versions prior to TLS-1.2 (not supported by default) and
  do not enable RSA-key exchange cipher suites (not supported by default).

  *** POTENTIAL INCOMPATIBILITY ***

  Own Id: OTP-19075

[PR-8026]: https://github.com/erlang/otp/pull/8026
[PR-6658]: https://github.com/erlang/otp/pull/6658
[PR-7809]: https://github.com/erlang/otp/pull/7809
[PR-8168]: https://github.com/erlang/otp/pull/8168
[PR-8233]: https://github.com/erlang/otp/pull/8233

## Crypto 5.4.2

### Fixed Bugs and Malfunctions

* Fix building with `--enable-fips` with OpenSSL 3 on MacOS.

  Own Id: OTP-19038 Aux Id: GH-8271, PR-8277

## Crypto 5.4.1

### Fixed Bugs and Malfunctions

* Fix compile error when OPENSSL_NO_DES is defined.

  Own Id: OTP-18921
* The function `crypto:pbkdf2_hmac` will no longer block the main schedulers. If the iteration count or block size parameters are such that the function is likely to take a long time to execute, the function will be scheduled to run on a dirty CPU scheduler.

  Own Id: OTP-18996 Aux Id: PR-8173, PR-8174

## Crypto 5.4

### Fixed Bugs and Malfunctions

- Fixed some benign compile warnings on Windows.

  Own Id: OTP-18895

### Improvements and New Features

- Enable engine support for OpenSSL versions 3.

  Own Id: OTP-18832 Aux Id: PR-7763

## Crypto 5.3

### Fixed Bugs and Malfunctions

- Fix VM crash caused by `crypto being` purged and reloaded (by init:restart for
  example) on OS with musl libc (such as Alpine linux).

  Own Id: OTP-18670 Aux Id: GH-7436, PR-7450

- Improved understanding of LibreSSL versions. Support chacha20 and
  chacha20_poly1305 for LibreSSL 3.7. Reflect removal of support for the DSS/DSA
  algorithm which was done in LibreSSL 2.6.1.

  Own Id: OTP-18758 Aux Id: PR-7209

### Improvements and New Features

- FIPS supported by `crypto` for OpenSSL 3.0._ and 3.1._.

  Own Id: OTP-18666 Aux Id: PR-7392

## Crypto 5.2

### Fixed Bugs and Malfunctions

- Fix cmac_update aes_128_cbc for LibreSSL.

  Own Id: OTP-18571

### Improvements and New Features

- Add support for SHAKE128 and SHAKE256.

  Own Id: OTP-18204 Aux Id: PR-6203

- Make the `-DOPENSSL_API_COMPAT` flag work without warnings.

  Own Id: OTP-18206 Aux Id: PR-6167

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

  Own Id: OTP-18405 Aux Id:
  GH-6672,PR-6702,PR-6768,PR-6700,PR-6769,PR-6812,PR-6814

- Handling of `on_load` modules during boot has been improved by adding an extra
  step in the boot order for embedded mode that runs all `on_load` handlers,
  instead of relying on explicit invocation of them, later, when the kernel
  supervision tree starts.

  This is mostly a code improvement and OTP internal simplification to avoid
  future bugs and to simplify code maintenance.

  Own Id: OTP-18447

## Crypto 5.1.4.3

### Fixed Bugs and Malfunctions

* Fix building with `--enable-fips` with OpenSSL 3 on MacOS.

  Own Id: OTP-19038 Aux Id: GH-8271, PR-8277

## Crypto 5.1.4.2

### Fixed Bugs and Malfunctions

* The function `crypto:pbkdf2_hmac` will no longer block the main schedulers. If the iteration count or block size parameters are such that the function is likely to take a long time to execute, the function will be scheduled to run on a dirty CPU scheduler.

  Own Id: OTP-18996 Aux Id: PR-8173, PR-8174

## Crypto 5.1.4.1

### Fixed Bugs and Malfunctions

- Fix VM crash caused by `crypto being` purged and reloaded (by init:restart for
  example) on OS with musl libc (such as Alpine linux).

  Own Id: OTP-18670 Aux Id: GH-7436, PR-7450

## Crypto 5.1.4

### Fixed Bugs and Malfunctions

- With this change, random errors are fixed for crypto:generate_key calls with
  OpenSSL 3.

  Own Id: OTP-18555

## Crypto 5.1.3

### Fixed Bugs and Malfunctions

- A user defined runtime library path configured using
  `--with-ssl-rpath=<PATHS>` could fail to be enabled.

  Own Id: OTP-18384 Aux Id: PR-6596

- Ensure that `configure` fails if a user defined runtime library path has been
  passed by the user, but cannot set.

  Own Id: OTP-18408

## Crypto 5.1.2

### Fixed Bugs and Malfunctions

- Fix configure with `--with-ssl` and `--disable-dynamic-ssl-lib` on Windows.

  Own Id: OTP-18147 Aux Id: GH-6024, PR-6056

- Remove all references correctly in the garbage collection if an engine handle
  was not explicit unloaded.

  Own Id: OTP-18152

- Changed the behaviour of the engine load/unload functions

  The engine load/unload functions have got changed semantics to get a more
  consistent behaviour and work correct when variables are garbage collected.

  The load functions now don't register the methods for the engine to replace.
  That will now be handled with the new functions
  engine_register/engine_unregister if needed.

  Some functions are removed from the documentation and therefor the API, but
  they are left in the code for compatibility.

  - engine_load/4: is now the same as engine_load/3
  - engine_unload/2: is now the same as engine_unload/1
  - ensure_engine_loaded/3: is now the same as ensure_engine_loaded/2
  - ensure_engine_unloaded/1, ensure_engine_unloaded/2: is now the same as
    engine_unload/1

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18172 Aux Id: ERIERL-826

- Fixed a naming bug for AES-CFB and Blowfish-CFB/OFB when linked with OpenSSL
  3.0 cryptolib.

  Own Id: OTP-18200

- Sign/verify does now behave as in OTP-24 and earlier for eddsa.

  Own Id: OTP-18205 Aux Id: GH-6219

### Improvements and New Features

- Pass elliptic curve names from crypto.erl to crypto's nif.

  Own Id: OTP-18037

- The configure option `--disable-deprecated-warnings` is removed. It was used
  for some releases when the support for OpenSSL 3.0 was not completed. It is
  not needed in OTP 25.

  Own Id: OTP-18133

- Crypto is now considered to be usable with the OpenSSL 3.0 cryptolib for
  production code.

  ENGINE and FIPS are not yet fully functional.

  Own Id: OTP-18153

- Do not exit if the legacy provider is missing in libcrypto 3.0.

  Own Id: OTP-18217

## Crypto 5.1.1

### Fixed Bugs and Malfunctions

- Note in the documentation that MODP (rfc3526) groups and OpenSSL 3.0 can give
  an error if a call to `crypto:generate_key/2` specifies a key length, and that
  length is to small.

  Own Id: OTP-18046

### Improvements and New Features

- The cmac now uses only the 3.0 API

  Own Id: OTP-18010

- Documentation is now updated with which OpenSSL cryptolib versions that OTP
  currently is tested.

  Own Id: OTP-18132

## Crypto 5.1

### Fixed Bugs and Malfunctions

- Fix timing bug in ensure_engine_loaded

  When two ensure_engine_loaded() calls were done in parallel there was a
  possibility that a crypto lib function was called by both instead of just one
  of them which resulted in an error. This is solved by moving the
  implementation from erlang down into a NIF function that uses a mutex to
  protect the sensitive part.

  Own Id: OTP-17858 Aux Id: ERIERL-728

- Remove faulty types `run_time_error()` and `descriptive_error()`.

  Own Id: OTP-17984

### Improvements and New Features

- Input for `configure` scripts adapted to `autoconf` 2\.71.

  Own Id: OTP-17414 Aux Id: PR-4967

- Add crypto:hash_equals/2

  Own Id: OTP-17471 Aux Id: PR-4750

- Add /opt/homebrew/opt/openssl to standard locations to search for OpenSSL
  cryptolib.

  Own Id: OTP-17561

- `crypto_dyn_iv_init/3` and `crypto_dyn_iv_update/3` are deprecated.

  Own Id: OTP-17870

- The information in error messages are increased.

  Previously the error was signaled with en error class exception `badarg`,
  `notsup` or `error`, and also in some more ways like an other exception or a
  return value in a non-standardized format.

  Now it is an error-class exception
  `{notsup|badarg|error, InfoFromCfile, Description::string()}`.

  The `InfoFromCfile` is a term with name and line number of the C-file where
  the error was found. This is primarily intended for a crypto maintainer or an
  advanced user to find the cause of complicated errors - maybe in crypto
  itself. The contents of that term might be changed in the future.

  The `Description` is a clear text string that describes the error. In case of
  `badarg` and `notsup` the intention is that it should help the user to find
  the cause ("Bad key size" as an example). Specially for some `error` that are
  unlikely, the string may not be possible to understand without deep knowledge
  of the underlying cryptolib. Such messages are intended for a crypto
  maintainer.

  The first element on call stack (the `S` in
  `try ... catch error:E:S .... end`) gives more information like the actual
  argument list in the call of crypto and the argument number (if possible) in
  the call to the NIF inside crypto.

  The functions in crypto affected by this change are:

  sign/4, sign/5, verify/5, verify/6,

  generate_key/2, generate_key/3, compute_key/4,

  hash/2, hash/4, hash_init/1, hash_update/4, hash_final/1,

  mac/3,4, mac_init/3, mac_update/2, mac_final/2,

  pbkdf2_hmac/5,

  public_encrypt/4, private_decrypt/4, private_encrypt/4, public_decrypt/4

  This schema was introduced earlier in:

  crypto_init/3, crypto_init/4, crypto_update/2, crypto_final/1,
  crypto_get_data/1,

  crypto_one_time/4, crypto_one_time/5, crypto_one_time_aead/6,
  crypto_one_time_aead/7

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17965

- Add Output Feedback mode (OFB) support for AES encryption / decryption for key
  sizes of 128, 192 and 256 bits.

  Own Id: OTP-18067 Aux Id: PR-5866

- The cryptolib API deprecated in OpenSSL 3.0 is now no longer used with a few
  exceptions listed below.

  Although OpenSSL 3.0.x itself is stable, its usage in OTP/crypto should still
  not be considered suitable for production code.

  The use of ENGINEs is still disabled by default when using 3.0.

  Deprecated functions are still called in the otp_test_engine.c (only used in
  tests), in mac.c (EVP_PKEY_new_CMAC_key) and five function calls in ec.c
  (EVP_PKEY_assign, EC_KEY_get_conv_form, EVP_PKEY_get1_EC_KEY,
  EC_KEY_get0_group and EC_KEY_set_public_key).

  Own Id: OTP-18086 Aux Id: OTP-16282, OTP-16643, OTP-16644, OTP-17701,
  OTP-17702, OTP-17704

## Crypto 5.0.6.5

### Fixed Bugs and Malfunctions

* The function `crypto:pbkdf2_hmac` will no longer block the main schedulers. If the iteration count or block size parameters are such that the function is likely to take a long time to execute, the function will be scheduled to run on a dirty CPU scheduler.

  Own Id: OTP-18996 Aux Id: PR-8173, PR-8174

## Crypto 5.0.6.4

### Fixed Bugs and Malfunctions

- Fix VM crash caused by `crypto being` purged and reloaded (by init:restart for
  example) on OS with musl libc (such as Alpine linux).

  Own Id: OTP-18670 Aux Id: GH-7436, PR-7450

## Crypto 5.0.6.3

### Fixed Bugs and Malfunctions

- Changed the behaviour of the engine load/unload functions

  The engine load/unload functions have got changed semantics to get a more
  consistent behaviour and work correct when variables are garbage collected.

  The load functions now don't register the methods for the engine to replace.
  That will now be handled with the new functions
  engine_register/engine_unregister if needed.

  Some functions functions are removed from the documentation and therefor the
  API, but they are left in the code for compatibility.

  - engine_load/4: is now the same as engine_load/3
  - engine_unload/2: is now the same as engine_unload/1
  - ensure_engine_loaded/3: is now the same as ensure_engine_loaded/2
  - ensure_engine_unloaded/1, ensure_engine_unloaded/2: is now the same as
    engine_unload/1

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18172 Aux Id: ERIERL-826

## Crypto 5.0.6.2

### Fixed Bugs and Malfunctions

- Fix configure with `--with-ssl` and `--disable-dynamic-ssl-lib` on Windows.

  Own Id: OTP-18147 Aux Id: GH-6024, PR-6056

## Crypto 5.0.6.1

### Fixed Bugs and Malfunctions

- Fix timing bug in ensure_engine_loaded

  When two ensure_engine_loaded() calls were done in parallel there was a
  possibility that a crypto lib function was called by both instead of just one
  of them which resulted in an error. This is solved by moving the
  implementation from erlang down into a NIF function that uses a mutex to
  protect the sensitive part.

  Own Id: OTP-17858 Aux Id: ERIERL-728

## Crypto 5.0.6

### Improvements and New Features

- The crypto app in OTP can since OTP-24.2 be compiled, linked and used with the
  new OpenSSL 3.0 cryptolib.

  The crypto app has 3.0 support has been improved, but is still \*\*not
  recommended\*\* for other usages than experiments and alpha testing. There are
  not yet any guaranties that it works, not even together with other OTP
  applications like for example SSL and SSH, although there are no known errors.

  Since the previous release, OTP-24.2, the following improvements have been
  done:

  \- It has been tested during nearly every nightly test on the OTP lab

  \- The hash algorithms `md4` and `ripemd160` have been enabled with OpenSSL
  3.0.

  \- The ciphers `blowfish_cbc`, `blowfish_ecb`, `des_cbc`, `des_cfb`,
  `des_ecb`, `rc2_cbc` and `rc4` have been enabled with OpenSSL 3.0.

  Disabled or unsupported with OpenSSL 3.0 are still:

  \- ENGINE support

  \- FIPS mode

  \- Other providers than the built-in ones

  \- Compiling and linking with OpenSSL 3.0 cryptolib in compatibility modes
  (for example to behave as 1.1.1)

  and, the ciphers `blowfish_cfb64` and `blowfish_ofb64` are not supported and
  will not be either.

  Deprecated functions in the OpenSSL 3.0 cryptolib must not be disabled as
  OTP/crypto still uses some of the deprecated API functions. The gcc flag
  `-Wno-deprecated-declarations` is set to prevent deprecation warnings to be
  printed when compiling.

  Own Id: OTP-17812 Aux Id: OTP-16646, OTP-16282

- Crypto is adapted to LibreSSL 3.5.0 on OpenBSD.

  Own Id: OTP-17941 Aux Id: OTP-17942

- New configure option ( `--disable-otp-test-engine`) to prohibit the build of
  the OTP test engine used in some test suites.

  The reason is that the test engine could be hard to compile on for instance
  LibreSSL 3.5.0. For that particular cryptolib version (or higher), this
  configure option is set automatically.

  Own Id: OTP-17942 Aux Id: OTP-17941

## Crypto 5.0.5

### Fixed Bugs and Malfunctions

- Fixed the C-warning "implicit declaration of function 'OpenSSL_version_num'"
  if compiling with an early LibreSSL version.

  Own Id: OTP-17637

- FIPS availability was not checked correctly for AEAD ciphers.

  Own Id: OTP-17740

- Fixed that cipher aliases (like aes_cbc etc) could be present even if the
  aliased cipher(s) (like aes_128_cbc, aes_256_cbc,... etc) was missing.

  Own Id: OTP-17741

### Improvements and New Features

- The crypto app in OTP can now be compiled, linked and used with the new
  OpenSSL 3.0 cryptolib.

  It has not yet been extensively tested and is in this release \*\*not
  recommended\*\* for other usages than experiments and alpha testing. There are
  not yet any guaranties that it works, not even together with other OTP
  applications like for example SSL and SSH, although there are no known errors.

  Compiling and linking with OpenSSL 3.0 cryptolib in compatibility modes (for
  example to behave as 1.1.1) are not tested. It is not tested with external
  providers.

  The support for FIPS mode does not yet work, and is disabled when compiled
  with OpenSSL 3.0.

  Deprecated functions in the OpenSSL 3.0 cryptolib must not be disabled as
  OTP/crypto still uses some of the deprecated API functions. The gcc flag
  `-Wno-deprecated-declarations` is set to prevent deprecation warnings to be
  printed when compiling.

  The hash algorithms `md4` and `ripemd160` are disabled temporarily when
  compiled with OpenSSL 3.0.

  The ciphers `blowfish_cbc`, `blowfish_cfb64`, `blowfish_ecb`,
  `blowfish_ofb64`, `des_cbc`, `des_cfb`, `des_ecb`, `rc2_cbc` and `rc4` are
  disabled temporarily when compiled with OpenSSL 3.0.

  Own Id: OTP-16646 Aux Id: OTP-16282

- The error handling in crypto is partly refactored using the new error
  reporting support. Errors earlier propagated like exceptions are still so, but
  when the failing function is called from the terminal - for example during
  failure hunting - a more descriptive text is produced.

  Own Id: OTP-17241

- A new function `crypto:info/0` which presents some data about the compilation
  and linkage of the crypto nif is added.

  Own Id: OTP-17603

- Added the `pbkdf2_hmac/5` function to the crypto module.

  It calls the `PKCS5_PBKDF2_HMAC` function which implements PBKD2 with HMAC in
  an efficient way.

  Own Id: OTP-17808 Aux Id: PR-5421

## Crypto 5.0.4

### Fixed Bugs and Malfunctions

- Fixed minor memory leak at `crypto` module purge.

  Own Id: OTP-17668 Aux Id: PR-5245

- Fix possible inconsistency in fips mode when linking with some cryptolibs.

  Own Id: OTP-17672

## Crypto 5.0.3

### Fixed Bugs and Malfunctions

- Fix bug in `crypto:ensure_engine_unloaded`. Also fixed minor memory leak
  related to engine unloading.

  Own Id: OTP-17593 Aux Id: ERIERL-679

- Fixes that FIPS enable and disable (or vice versa) on Windows sometimes leads
  to core dump at the time of process exit.

  Own Id: OTP-17618 Aux Id: PR-5126, GH-4920

### Improvements and New Features

- Disable fips if cryptolib < 1.0.1 and OTP/crypto is configured with
  --enable-fips

  If not, there could be compiling or loading problems with antique OpenSSL
  versions.

  Own Id: OTP-17389

## Crypto 5.0.2

### Fixed Bugs and Malfunctions

- EC keys are now zero-padded to the expected length if needed.

  Own Id: OTP-17442 Aux Id: GH-4861

## Crypto 5.0.1

### Fixed Bugs and Malfunctions

- Removed a risk for coredump.

  Own Id: OTP-17391 Aux Id: GH-4810

- Fixed and documented the `DED_LDFLAGS_CONFTEST` configuration variable in
  `$ERL_TOP/HOWTO/INSTALL.md`.

  Own Id: OTP-17419 Aux Id: GH-4821

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## Crypto 5.0

### Fixed Bugs and Malfunctions

- Add `/usr/local/opt/openssl` to the openssl configure search path. This path
  is where some tools on OS X place openssl.

  Own Id: OTP-16882

- Fix compiler warnings produced by the clang compiler.

  Own Id: OTP-17105 Aux Id: PR-2872

- The `configure` scripts in `crypto` and `erts` now fail if a requested feature
  cannot be enabled.

  Large parts of the `configure` script of `crypto` have been rewritten with
  various improvements and bug fixes. It is now better at finding usable OpenSSL
  libraries, but will in the following cases fail to detect OpenSSL libraries
  where it previously sometimes detected the libraries by chance:

  - OpenSSL installations with `include` directory and `lib` directory parts
    installed in different base directories. In order to detect such
    installations after this change, the user must explicitly specify the
    locations using the
    [`--with-ssl=<path>`](`e:system:install.md#advanced-configuration-and-build-of-erlang-otp_configuring`)
    and the
    [`--with-ssl-incl=<path>`](`e:system:install.md#advanced-configuration-and-build-of-erlang-otp_configuring`)
    `configure` command line arguments.
  - When building with old `gcc` compilers or other compilers on Debian
    derivatives with multiarch directories under the `lib` directory. In order
    to detect such installations after this change, the user must explicitly
    specify the multiarch directory name using the
    [`--with-ssl-lib-subdir=lib/<multiarch-dir>`](`e:system:install.md#advanced-configuration-and-build-of-erlang-otp_configuring`)
    `configure` command line argument.

  Own Id: OTP-17254 Aux Id: ERIERL-618, GH-4230

- The value 'none' was missing in the specs of crypto:sign/4 and
  crypto:verify/6.

  Own Id: OTP-17312 Aux Id: PR-4723

### Improvements and New Features

- The functions and cipher names that were deprecated in OTP-23.0 are now
  removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16656

- Removed installed directory priv/obj/ containing superfluous object files.

  Own Id: OTP-17001 Aux Id: PR-2852

- TLS connections now support EdDSA certificates.

  Own Id: OTP-17142 Aux Id: PR-4756, GH-4637, GH-4650

- Add prop_aead attribute to map from crypto:cipher_info/1.

  Own Id: OTP-17313 Aux Id: PR-4686

## Crypto 4.9.0.4

### Fixed Bugs and Malfunctions

- Changed the behaviour of the engine load/unload functions

  The engine load/unload functions have got changed semantics to get a more
  consistent behaviour and work correct when variables are garbage collected.

  The load functions now don't register the methods for the engine to replace.
  That will now be handled with the new functions
  engine_register/engine_unregister if needed.

  Some functions functions are removed from the documentation and therefor the
  API, but they are left in the code for compatibility.

  - engine_load/4: is now the same as engine_load/3
  - engine_unload/2: is now the same as engine_unload/1
  - ensure_engine_loaded/3: is now the same as ensure_engine_loaded/2
  - ensure_engine_unloaded/1, ensure_engine_unloaded/2: is now the same as
    engine_unload/1

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18172 Aux Id: ERIERL-826

## Crypto 4.9.0.3

### Fixed Bugs and Malfunctions

- Fix timing bug in ensure_engine_loaded

  When two ensure_engine_loaded() calls were done in parallel there was a
  possibility that a crypto lib function was called by both instead of just one
  of them which resulted in an error. This is solved by moving the
  implementation from erlang down into a NIF function that uses a mutex to
  protect the sensitive part.

  Own Id: OTP-17858 Aux Id: ERIERL-728

- Remove all references correctly in the garbage collection if an engine handle
  was not explicit unloaded.

  Own Id: OTP-18152

## Crypto 4.9.0.2

### Fixed Bugs and Malfunctions

- EC keys are now zero-padded to the expected length if needed.

  Own Id: OTP-17442 Aux Id: GH-4861

## Crypto 4.9.0.1

### Fixed Bugs and Malfunctions

- Removed a risk for coredump.

  Own Id: OTP-17391 Aux Id: GH-4810

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## Crypto 4.9

### Fixed Bugs and Malfunctions

- Fix minor memory leaks in crypto ENGINE and robustify the code.

  Own Id: OTP-17212

- The otp_test_engine no longer fails if NO_EC\* is set in the OpenSSL
  configuration.

  Own Id: OTP-17256 Aux Id: PR-4580, GH-4573

### Improvements and New Features

- Various address sanitizer support.

  Own Id: OTP-16959 Aux Id: PR-2965

- EVP is now disabled for OpenSSL cryptolib versions up to and including 1.0.2

  Own Id: OTP-17116 Aux Id: PR-2972

- Warning for unused C function removed

  Own Id: OTP-17145 Aux Id: OTP-17105, PR-2872

## Crypto 4.8.3

### Fixed Bugs and Malfunctions

- Adding missing flag in BN-calls in SRP.

  Own Id: OTP-17107

## Crypto 4.8.2

### Fixed Bugs and Malfunctions

- Fixed usage of `AC_CONFIG_AUX_DIRS()` macros in configure script sources.

  Own Id: OTP-17093 Aux Id: ERL-1447, PR-2948

## Crypto 4.8.1

### Fixed Bugs and Malfunctions

- Build the supported curves cache in the NIF when crypto is loaded, no matter
  how it is loaded.

  This prevents a possible problem with different processes starting the crypto
  application concurrently.

  Own Id: OTP-16819 Aux Id: PR-2720

- It is now possible to build with crypto and openssl gprof-enabled and
  statically link them into the VM.

  Own Id: OTP-17029

### Improvements and New Features

- Fixed performance loss in HMAC when using older OpenSSL due to mutex issues.

  A workaround is implemented to allow fallback from using the EVP API for HMAC
  operations. On some architectures this may improve the performance, especially
  with old OpenSSL versions. This fallback to low-level functions is always
  enabled for openssl versions before 1.0.2.

  Own Id: OTP-17025 Aux Id: ERL-1400, PR-2877

## Crypto 4.8

### Fixed Bugs and Malfunctions

- Fix type spec bug in crypto for crypto_init and crypto:one_time

  Own Id: OTP-16658 Aux Id: OTP-15884, ERL-1257

- The deprecation message for crypto:rand_uniform/2 indicated a non-existent
  function. The correct one (rand:uniform/1) is now suggested.

  Own Id: OTP-16846 Aux Id: PR-2741

### Improvements and New Features

- Implemented a workaround to allow fallback from using the EVP API for
  Diffie-Hellman key generation

  Own Id: OTP-16771 Aux Id: ERIERL-509

- The internal Diffie-Hellman high level API for key generation was slow in old
  and by OpenSSL now unsupported cryptolib versions (1.0.1 and earlier).

  If such a cryptolib is used anyhow, the low-level API is used internally in
  the crypto application.

  Own Id: OTP-16774

## Crypto 4.7

### Fixed Bugs and Malfunctions

- Crypto reported unsupported elliptic curves as supported on e.g Fedora
  distros.

  Own Id: OTP-16579 Aux Id: ERL-825

### Improvements and New Features

- Support for ed25519 and ed448 added to `crypto:generate_key`.

  Own Id: OTP-15967 Aux Id: PR-2329

- The [new crypto functions api](new_api.md#the-new-api) (crypto_init,
  crypto_update and crypto_one_time) has been updated.

  There is now a function [`crypto_final/1`](`crypto:crypto_final/1`) and a
  possibility to set options in [`crypto_init/3`](`crypto:crypto_init/3`) and
  [`crypto_init/4`](`crypto:crypto_init/4`). See the manual for details.

  Own Id: OTP-16160

- As [announced](notes.md#crypto-4-5) in OTP 22.0, a New API was introduced in
  CRYPTO. See the [_New and Old API_](new_api.md) chapter in the CRYPTO User's
  Guide for more information and suggested replacement functions.

  [The Old API](new_api.md#the-old-api) is now deprecated in OTP-23.0 and will
  be removed in OTP-24.0.

  This deprecation includes cipher names. See the section
  [Retired cipher names](new_api.md#retired-cipher-names) in the crypto User's
  Guide, chapter [The Old API](new_api.md#the-old-api).

  Own Id: OTP-16232

- Fix C-compilation without deprecated OpenSSL cryptolib APIs

  Own Id: OTP-16369 Aux Id: PR-2474

- Refactored the internal handling of deprecated and removed functions.

  Own Id: OTP-16469

- Added missing 'eddh' to [crypto:supports(public_keys)](`crypto:supports/1`).

  Own Id: OTP-16583

## Crypto 4.6.5.4

### Fixed Bugs and Malfunctions

- EC keys are now zero-padded to the expected length if needed.

  Own Id: OTP-17442 Aux Id: GH-4861

## Crypto 4.6.5.3

### Fixed Bugs and Malfunctions

- Removed a risk for coredump.

  Own Id: OTP-17391 Aux Id: GH-4810

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## Crypto 4.6.5.2

### Fixed Bugs and Malfunctions

- Adding missing flag in BN-calls in SRP.

  Own Id: OTP-17107

## Crypto 4.6.5.1

### Improvements and New Features

- Implemented a workaround to allow fallback from using the EVP API for
  Diffie-Hellman key generation

  Own Id: OTP-16771 Aux Id: ERIERL-509

## Crypto 4.6.5

### Fixed Bugs and Malfunctions

- Fixed potential memory leaks involving calls to the crypto ng_api.

  Own Id: OTP-16428 Aux Id: PR-2511

## Crypto 4.6.4

### Fixed Bugs and Malfunctions

- Constant time comparisons added.

  Own Id: OTP-16376

## Crypto 4.6.3

### Improvements and New Features

- The ciphers aes_cfb8 and aes_cfb128 are now using the EVP interface. The
  supported key lengths are 128, 192 and 256 bits.

  Own Id: OTP-16133 Aux Id: PR-2407

- The ciphers aes_cfb8 and aes_cfb128 are now available in FIPS enabled mode.

  Own Id: OTP-16134 Aux Id: PR-2407

## Crypto 4.6.2

### Fixed Bugs and Malfunctions

- The AEAD tag was not previously checked on decrypt with chacha20_poly1305

  Own Id: OTP-16242 Aux Id: ERL-1078

## Crypto 4.6.1

### Fixed Bugs and Malfunctions

- FIxed a bug if the erlang emulator was linked with a very old cryptolib
  version (1.0.1 or earlier).

  The bug now fixed could have triggered a core dump if an unknown cipher name
  was used in crypto functions.

  Own Id: OTP-16202

## Crypto 4.6

### Fixed Bugs and Malfunctions

- The implementation of `crypto_one_time/4` is adjusted to match the type
  specification. The spec and the black-box behaviour of the function are
  unchanged.

  Some details: Both the spec and the implementation were correct seen
  separately. But with both of them combined simultaneously with
  `crypto_one_time/5` which was called by the implementation of
  `crypto_one_time/4`, an (obvious) error was detected by a Dialyzer with more
  thorough checking than usual.

  Own Id: OTP-15884 Aux Id: ERL-974

- When using crypto with FIPS mode enabled, the digests were not correctly
  handled.

  Own Id: OTP-15911

- A memory leak in error handling code in `ng_crypto_init_nif` is fixed.

  Own Id: OTP-15924

- Fixed the broken static build of the crypto nifs

  Own Id: OTP-15928 Aux Id: PR-2296

### Improvements and New Features

- The Message Authentication Codes (MAC) CMAC, HMAC and Poly1305 are unified
  into common functions in the New Crypto API. See the manual for CRYPTO.

  Own Id: OTP-13872

## Crypto 4.5.1

### Fixed Bugs and Malfunctions

- The cipher aes-ctr was disabled by mistake in crypto:supports for cryptolibs
  before 1.0.1. It worked however in the encrypt and decrypt functions.

  Own Id: OTP-15829

## Crypto 4.5

### Fixed Bugs and Malfunctions

- Fixed a bug in error return for `crypto:poly1305/2`. It returned the atom
  `notsup` instead of the exception `notsup`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15677

- The cipher chacha20 was introduced in OpenSSL 1.1.0. However, it could in a
  very odd situation, fail for versions less than OpenSSL 1.1.0d. It is
  therefore disabled for those versions.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15678

### Improvements and New Features

- A new `rand` module algorithm, `exro928ss` (Xoroshiro928\*\*), has been
  implemented. It has got a really long period and good statistical quality for
  all output bits, while still being only about 50% slower than the default
  algorithm.

  The same generator is also used as a long period counter in a new `crypto`
  plugin for the `rand` module, algorithm `crypto_aes`. This plugin uses AES-256
  to scramble the counter which buries any detectable statistical artifacts.
  Scrambling is done in chunks which are cached to get good amortized speed
  (about half of the default algorithm).

  Own Id: OTP-14461 Aux Id: PR-1857

- Crypto's single C-file is split into multiple files. The different coding
  styles in the different parts are unified into a single style.

  Own Id: OTP-14732 Aux Id: PR-2068, PR-2095

- Build configuration of the `crypto` application has been moved from the `erts`
  application into the `crypto` application.

  Own Id: OTP-15129

- Adds two hash functions `blake2b` and `blake2s` (64 bit hash and 32 bit hash
  respectively). These are modern and standard hash functions used in
  blockchains and encrypted communication protocols. The hash functions are
  available in OpenSSL since version 1.1.1.

  Own Id: OTP-15564 Aux Id: PR-2129

- A new API is implemented in crypto. See the CRYPTO user's guide, chapter _New
  and Old API_ for more information.

  The old api with the `crypto:block_*` and `crypto:stream_*` interfaces are
  kept for compatibility, but implemented with the new api. Please note that
  since the error checking is more thorough, there _might_ be arguments with for
  example faulty lengths that are no longer accepted.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15644 Aux Id: OTP-14732 , OTP-15451, PR-1857 , PR-2068, PR-2095

- The new hash_info/1 and cipher_info/1 functions returns maps with information
  about the hash or cipher in the argument.

  Own Id: OTP-15655 Aux Id: PR-2173, ERL-864, PR-2186

- Obey additional OpenSSL configure flags when compiling the C-part of the
  CRYPTO application: `no-bf`, `no-blake2`, `no-chacha`, `no-cmac`, `no-dh`,
  `no-dsa`, `no-md4`, `no-poly1305`, `no-rc2`, `no-rc4` and `no-rmd160`.

  Own Id: OTP-15683

- A new function `crypto:supports/1` is introduced. The single argument takes an
  atom as argument: `hashes`, `public_keys`, `ciphers`, `macs`, `curves` or
  `rsa_opts`. The return value is a list of supported algorithms.

  The difference with the existing `crypto:supports/0` is, apart from the
  argument and the return value, that the old function reports what is supported
  by the old api, and the new function reports algorithms in the new api.

  Own Id: OTP-15771

## Crypto 4.4.2.3

### Fixed Bugs and Malfunctions

- Adding missing flag in BN-calls in SRP.

  Own Id: OTP-17107

## Crypto 4.4.2.2

### Fixed Bugs and Malfunctions

- Constant time comparisons added.

  Own Id: OTP-16376

## Crypto 4.4.2.1

### Improvements and New Features

- The ciphers aes_cfb8 and aes_cfb128 are now using the EVP interface. The
  supported key lengths are 128, 192 and 256 bits.

  Own Id: OTP-16133 Aux Id: PR-2407

- The ciphers aes_cfb8 and aes_cfb128 are now available in FIPS enabled mode.

  Own Id: OTP-16134 Aux Id: PR-2407

## Crypto 4.4.2

### Fixed Bugs and Malfunctions

- Fixed build link error on Windows. Unresolved symbol 'bcmp'.

  Own Id: OTP-15750 Aux Id: ERL-905

## Crypto 4.4.1

### Fixed Bugs and Malfunctions

- Fixes a bug that caused `crypto:sign` and `crypto:verify` to return the error
  message `badarg` instead of `notsup` in one case. That case was when signing
  or verifying with eddsa keys (that is, ed15519 or ed448), but only when FIPS
  was supported and enabled.

  Own Id: OTP-15634

### Improvements and New Features

- Added a crypto benchmark test suite.

  Own Id: OTP-15447

## Crypto 4.4

### Fixed Bugs and Malfunctions

- Updated the RSA options part in the crypto application's C-code, documentation
  and tests.

  Own Id: OTP-15302

### Improvements and New Features

- Added ed25519 and ed448 sign/verify.

  Requires OpenSSL 1.1.1 or higher as cryptolib under the OTP application
  `crypto`.

  Own Id: OTP-15419 Aux Id: OTP-15094

- Fixed valgrind warnings.

  Own Id: OTP-15467

## Crypto 4.3.3

### Fixed Bugs and Malfunctions

- The RSA options `rsa_mgf1_md`, `rsa_oaep_md`, and `rsa_oaep_label` were always
  disabled. They will now be enabled when a suitable cryptolib is used.

  They are still experimental and may change without prior notice.

  Own Id: OTP-15212 Aux Id: ERL-675, PR1899, PR838

- The ciphers `aes_ige256` and `blowfish_cbc` had naming issues in
  `crypto:next_iv/2`.

  Own Id: OTP-15283

- the `RSA_SSLV23_PADDING` is disabled if LibreSSL is used as cryptlib. This is
  due to compilation problems.

  This will be investigated further in the future.

  Own Id: OTP-15303

### Improvements and New Features

- The supported named elliptic curves are now reported in `crypto:supports/0` in
  a new entry tagged by `'curves'`.

  The function `crypto:ec_curves/0` is kept for compatibility.

  Own Id: OTP-14717 Aux Id: OTP-15244

- The typing in the CRYPTO and PUBLIC_KEY applications are reworked and a few
  mistakes are corrected.

  The documentation is now generated from the typing and some clarifications are
  made.

  A new chapter on Algorithm Details such as key sizes and availability is added
  to the CRYPTO User's Guide.

  Own Id: OTP-15134

- Support for SHA3 both as a separate hash and in HMAC is now available if
  OpenSSL 1.1.1 or higher is used as cryptolib.

  Available lengths are reported in the `'hashs'` entry in `crypto:supports/0`
  as `sha3_*`.

  Own Id: OTP-15153

- The mac algorithm `poly1305` and the cipher algorithm `chacha20` are now
  supported if OpenSSL 1.1.1 or higher is used as cryptolib.

  Own Id: OTP-15164 Aux Id: OTP-15209

- The key exchange Edward curves `x25519` and `x448` are now supported if
  OpenSSL 1.1.1 or higher is used as cryptolib.

  Own Id: OTP-15240 Aux Id: OTP-15133

- The supported RSA options for sign/verify and encrypt/decrypt are now reported
  in `crypto:supports/0` in a new entry tagged by '`rsa_opts`'.

  The exakt set is still experimental and may change without prior notice.

  Own Id: OTP-15260

- The cipher `aes_ccm` is added.

  Own Id: OTP-15286

## Crypto 4.3.2

### Fixed Bugs and Malfunctions

- Update the crypto engine functions to handle multiple loads of an engine.

  `engine_load/3/4` is updated so it doesn't add the engine ID to OpenSSLs
  internal list of engines which makes it possible to run the engine_load more
  than once if it doesn't contain global data.

  Added `ensure_engine_loaded/2/3` which guarantees that the engine just is
  loaded once and the following calls just returns a reference to it. This is
  done by add the ID to the internal OpenSSL list and check if it is already
  registered when the function is called.

  Added `ensure_engine_unloaded/1/2` to unload engines loaded with
  ensure_engine_loaded.

  Then some more utility functions are added.

  `engine_add/1`, adds the engine to OpenSSL internal list

  `engine_remove/1`, remove the engine from OpenSSL internal list

  `engine_get_id/1`, fetch the engines id

  `engine_get_name/1`, fetch the engine name

  Own Id: OTP-15233

## Crypto 4.3.1

### Fixed Bugs and Malfunctions

- Fixed a node crash in `crypto:compute_key(ecdh, ...)` when passing a wrongly
  typed Others argument.

  Own Id: OTP-15194 Aux Id: ERL-673

## Crypto 4.3

### Fixed Bugs and Malfunctions

- Removed two undocumented and erroneous functions
  (`crypto:dh_generate_parameters/2` and `crypto:dh_check/1`).

  Own Id: OTP-14956 Aux Id: ERL-579

- Fixed bug causing VM crash if doing runtime upgrade of a crypto module built
  against OpenSSL older than 0.9.8h. Bug exists since OTP-20.2.

  Own Id: OTP-15088

### Improvements and New Features

- A new `rand` plugin algorithm has been implemented in `crypto`, that is:
  `crypto_cache`. It uses strong random bytes as randomness source and caches
  them to get good speed. See `crypto:rand_seed_alg/1`.

  Own Id: OTP-13370 Aux Id: PR-1573

- Diffie-Hellman key functions are re-written with the EVP_PKEY api.

  Own Id: OTP-14864

## Crypto 4.2.2.4

### Fixed Bugs and Malfunctions

- Constant time comparisons added.

  Own Id: OTP-16376

## Crypto 4.2.2.3

### Improvements and New Features

- The ciphers aes_cfb8 and aes_cfb128 are now using the EVP interface. The
  supported key lengths are 128, 192 and 256 bits.

  Own Id: OTP-16133 Aux Id: PR-2407

## Crypto 4.2.2.1

### Fixed Bugs and Malfunctions

- Fixed a node crash in `crypto:compute_key(ecdh, ...)` when passing a wrongly
  typed Others argument.

  Own Id: OTP-15194 Aux Id: ERL-673

## Crypto 4.2.2

### Fixed Bugs and Malfunctions

- If OPENSSL_NO_EC was set, the compilation of the crypto nifs failed.

  Own Id: OTP-15073

- C-compile errors for LibreSSL 2.7.0 - 2.7.2 fixed

  Own Id: OTP-15074 Aux Id: ERL-618

## Crypto 4.2.1

### Fixed Bugs and Malfunctions

- Fix build error caused by removed RSA padding functions in LibreSSL >= 2.6.1

  Own Id: OTP-14873

## Crypto 4.2

### Fixed Bugs and Malfunctions

- The compatibility function `void HMAC_CTX_free` in `crypto.c` erroneously
  tried to return a value.

  Own Id: OTP-14720

### Improvements and New Features

- Rewrite public and private key encode/decode with EVP api. New RSA padding
  options added. This is a modified half of PR-838.

  Own Id: OTP-14446

- The crypto API is extended to use private/public keys stored in an Engine for
  sign/verify or encrypt/decrypt operations.

  The ssl application provides an API to use this new engine concept in TLS.

  Own Id: OTP-14448

- Add support to plug in alternative implementations for some or all of the
  cryptographic operations supported by the OpenSSL Engine API. When configured
  appropriately, OpenSSL calls the engine's implementation of these operations
  instead of its own.

  Own Id: OTP-14567

- Replaced a call of the OpenSSL deprecated function `DH_generate_parameters` in
  `crypto.c`.

  Own Id: OTP-14639

- Documentation added about how to use keys stored in an Engine.

  Own Id: OTP-14735 Aux Id: OTP-14448

- Add engine\_ ctrl_cmd_string/3,4 the OpenSSL Engine support in crypto.

  Own Id: OTP-14801

## Crypto 4.1

### Fixed Bugs and Malfunctions

- On macOS, `crypto` would crash if `observer` had been started before `crypto`.
  On the beta for macOS 10.13 (High Sierra), `crypto` would crash. Both of those
  bugs have been fixed.

  Own Id: OTP-14499 Aux Id: ERL-251 ERL-439

### Improvements and New Features

- Extend crypto:sign, crypto:verify, public_key:sign and public_key:verify with:

  \* support for RSASSA-PS padding for signatures and for saltlength setting  
  \* X9.31 RSA padding.  
  \* sha, sha224, sha256, sha384, and sha512 for dss signatures as mentioned in
  NIST SP 800-57 Part 1.  
  \* ripemd160 to be used for rsa signatures.

  This is a manual merge of half of the pull request 838 by potatosalad from
  Sept 2015.

  Own Id: OTP-13704 Aux Id: PR838

- A new tuple in `crypto:supports/0` reports supported MAC algorithms.

  Own Id: OTP-14504

## Crypto 4.0

### Fixed Bugs and Malfunctions

- LibreSSL can now be used by the modernized crypto app.

  Own Id: OTP-14247

- Add compile option `-compile(no_native)` in modules with `on_load` directive
  which is not yet supported by HiPE.

  Own Id: OTP-14316 Aux Id: PR-1390

- Fix a bug in aes cfb128 function introduced by the bug fix in GitHub pull
  request [\#1393](https://github.com/erlang/otp/pull/1393).

  Own Id: OTP-14435 Aux Id: PR-1462, PR-1393, OTP-14313

### Improvements and New Features

- Add basic support for CMAC

  Own Id: OTP-13779 Aux Id: ERL-82 PR-1138

- Removed functions deprecated in crypto-3.0 first released in OTP-R16B01

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13873

- The `crypto` application now supports OpenSSL 1.1.

  Own Id: OTP-13900

- Allow Erlang/OTP to use OpenSSL in FIPS-140 mode, in order to satisfy specific
  security requirements (mostly by different parts of the US federal
  government).

  See the new crypto users guide "FIPS mode" chapter about building and using
  the FIPS support which is disabled by default.

  (Thanks to dszoboszlay and legoscia)

  Own Id: OTP-13921 Aux Id: PR-1180

- Crypto chacha20-poly1305 as in RFC 7539 enabled for OpenSSL >= 1.1.

  Thanks to mururu.

  Own Id: OTP-14092 Aux Id: PR-1291

- RSA key generation added to `crypto:generate_key/2`. Thanks to wiml.

  An interface is also added to `public_key:generate_key/1`.

  Own Id: OTP-14140 Aux Id: ERL-165, PR-1299

- Raised minimum requirement for OpenSSL version to OpenSSL-0.9.8.c although we
  recommend a much higher version, that is a version that is still maintained
  officially by the OpenSSL project. Note that using such an old version may
  restrict the crypto algorithms supported.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14171

- Deprecate crypto:rand_uniform/2 as it is not cryptographically strong

  Own Id: OTP-14274

- The Crypto application now supports generation of cryptographically strong
  random numbers (floats < 1.0 and integer arbitrary ranges) as a plugin to the
  'rand' module.

  Own Id: OTP-14317 Aux Id: PR-1372

- This replaces the hard coded test values for AES, CMAC and GCM ciphers with
  the full validation set from NIST's CAVP program.

  Own Id: OTP-14436 Aux Id: PR-1396

## Crypto 3.7.4

### Fixed Bugs and Malfunctions

- Fix a bug with AES CFB 128 for 192 and 256 bit keys. Thanks to kellymclaughlin
  \!

  Own Id: OTP-14313 Aux Id: PR-1393

## Crypto 3.7.3

### Improvements and New Features

- The implementation of the key exchange algorithms
  diffie-hellman-group-exchange-sha\* are optimized, up to a factor of 11 for
  the slowest ( = biggest and safest) group size.

  Own Id: OTP-14169 Aux Id: seq-13261

## Crypto 3.7.2

### Fixed Bugs and Malfunctions

- The crypto application has been fixed to not use RC2 against OpenSSL built
  with RC2 disabled.

  Own Id: OTP-13895 Aux Id: PR-1163

- The crypto application has been fixed to not use RC4 against OpenSSL built
  with RC4 disabled.

  Own Id: OTP-13896 Aux Id: PR-1169

### Improvements and New Features

- To ease troubleshooting, `erlang:load_nif/2` now includes the return value
  from a failed call to load/reload/upgrade in the text part of the error tuple.
  The `crypto` NIF makes use of this feature by returning the source line
  where/if the initialization fails.

  Own Id: OTP-13951

## Crypto 3.7.1

### Fixed Bugs and Malfunctions

- Crypto has been fixed to work against OpenSSL versions with disabled DES
  ciphers. Correct spelling of cipher algorithm 'des3_cfb' has been introduced;
  the previous misspeling still works.

  Own Id: OTP-13783 Aux Id: ERL-203

- The size of an internal array in crypto has been fixed to not segfault when
  having all possible ciphers. Bug fix by Duncan Overbruck.

  Own Id: OTP-13789 Aux Id: PR-1140

## Crypto 3.7

### Improvements and New Features

- Refactor `crypto` to use the EVP interface of OpenSSL, which is the
  recommended interface that also enables access to hardware acceleration for
  some operations.

  Own Id: OTP-12217

- Add support for 192-bit keys for the `aes_cbc` cipher.

  Own Id: OTP-13206 Aux Id: pr 832

- Add support for 192-bit keys for `aes_ecb`.

  Own Id: OTP-13207 Aux Id: pr829

- Deprecate the function `crypto:rand_bytes` and make sure that
  `crypto:strong_rand_bytes` is used in all places that are cryptographically
  significant.

  Own Id: OTP-13214

- Enable AES-GCM encryption/decryption to change the tag length between 1 to 16
  bytes.

  Own Id: OTP-13483 Aux Id: PR-998

## Crypto 3.6.3

### Fixed Bugs and Malfunctions

- Fix bug for `aes_ecb` block crypto when data is larger than 16 bytes.

  Own Id: OTP-13249

- Improve portability of ECC tests in Crypto and SSL for "exotic" OpenSSL
  versions.

  Own Id: OTP-13311

## Crypto 3.6.2

### Fixed Bugs and Malfunctions

- Small documentation fixes

  Own Id: OTP-13017

## Crypto 3.6.1

### Fixed Bugs and Malfunctions

- Make `crypto:ec_curves/0` return empty list if elliptic curve is not supported
  at all.

  Own Id: OTP-12944

## Crypto 3.6

### Fixed Bugs and Malfunctions

- Enhance crypto:generate_key to calculate ECC public keys from private key.

  Own Id: OTP-12394

- Fix bug in `crypto:generate_key` for `ecdh` that could cause VM crash for
  faulty input.

  Own Id: OTP-12733

### Improvements and New Features

- Use the EVP API for AES-CBC crypto to enables the use of hardware acceleration
  for AES-CBC crypto on newer Intel CPUs (AES-NI), among other platforms.

  Own Id: OTP-12380

- Add AES ECB block encryption.

  Own Id: OTP-12403

## Crypto 3.5

### Improvements and New Features

- Extend block_encrypt/decrypt for aes_cfb8 and aes_cfb128 to accept keys of
  length 128, 192 and 256 bits. Before only 128 bit keys were accepted.

  Own Id: OTP-12467

## Crypto 3.4.2

### Improvements and New Features

- Add configure option --with-ssl-incl=PATH to support OpenSSL installations
  with headers and libraries at different places.

  Own Id: OTP-12215 Aux Id: seq12700

- Add configure option --with-ssl-rpath to control which runtime library path to
  use for dynamic linkage toward OpenSSL.

  Own Id: OTP-12316 Aux Id: seq12753

## Crypto 3.4.1

### Fixed Bugs and Malfunctions

- Make `crypto` verify major version number of OpenSSL header files and runtime
  library. Loading of `crypto` will fail if there is a version mismatch.

  Own Id: OTP-12146 Aux Id: seq12700

## Crypto 3.4

### Fixed Bugs and Malfunctions

- Fix memory leak in `crypto:hmac_init/upgrade/final` functions for all data and
  in `crypto:hmac/3/4` for data larger than 20000 bytes. Bug exists since OTP
  17.0.

  Own Id: OTP-11953

- Fix memory leak in `crypto` for elliptic curve.

  Own Id: OTP-11999

### Improvements and New Features

- Add `aes_cfb8` cypher to `crypto:block_encrypt` and `block_decrypt`.

  Own Id: OTP-11911

## Crypto 3.3

### Fixed Bugs and Malfunctions

- Fix memory leaks and invalid deallocations in `mod_pow`, `mod_exp` and
  `generate_key(srp,...)` when bad arguments are passed. (Thanks to Florian
  Zumbiehi)

  Own Id: OTP-11550

- Correction of the word 'ChipherText' throughout the documentation (Thanks to
  Andrew Tunnell-Jones)

  Own Id: OTP-11609

- Fix fatal bug when using a hmac context variable in more than one call to
  `hmac_update` or `hmac_final`. The reuse of hmac contexts has never worked as
  the underlying OpenSSL implementation does not support it. It is now
  documented as having undefined behaviour, but it does not crash or corrupt the
  VM anymore.

  Own Id: OTP-11724

- Crypto handles out-of-memory with a controlled abort instead of
  crash/corruption. (Thanks to Florian Zumbiehi)

  Own Id: OTP-11725

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

### Improvements and New Features

- By giving --enable-static-\{nifs,drivers\} to configure it is now possible to
  statically linking of nifs and drivers to the main Erlang VM binary. At the
  moment only the asn1 and crypto nifs of the Erlang/OTP nifs and drivers have
  been prepared to be statically linked. For more details see the Installation
  Guide in the System documentation.

  Own Id: OTP-11258

- Add IGE mode for AES cipher in crypto (Thanks to Yura Beznos).

  Own Id: OTP-11522

- Moved elliptic curve definition from the crypto NIF/OpenSSL into Erlang code,
  adds the RFC-5639 brainpool curves and makes TLS use them (RFC-7027).

  Thanks to Andreas Schultz

  Own Id: OTP-11578

- Remove all obsolete application processes from crypto and make it into a pure
  library application.

  Own Id: OTP-11619

## Crypto 3.2

### Fixed Bugs and Malfunctions

- Fix uninitialized pointers in crypto (Thanks to Anthony Ramine)

  Own Id: OTP-11510

## Crypto 3.1

### Improvements and New Features

- Refactor ecdsa cipher to simplify code and improve performance.

  Own Id: OTP-11320

## Crypto 3.0

### Improvements and New Features

- Integrate elliptic curve contribution from Andreas Schultz

  In order to be able to support elliptic curve cipher suites in SSL/TLS,
  additions to handle elliptic curve infrastructure has been added to public_key
  and crypto.

  This also has resulted in a rewrite of the crypto API to gain consistency and
  remove unnecessary overhead. All OTP applications using crypto has been
  updated to use the new API.

  Impact: Elliptic curve cryptography (ECC) offers equivalent security with
  smaller key sizes than other public key algorithms. Smaller key sizes result
  in savings for power, memory, bandwidth, and computational cost that make ECC
  especially attractive for constrained environments.

  Own Id: OTP-11009

- Fixed a spelling mistake in crypto docs. Thanks to Klaus Trainer

  Own Id: OTP-11058

### Known Bugs and Problems

- Make the crypto functions interruptible by chunking input when it is very
  large and bumping reductions in the nifs.

  Not yet implemented for block_encrypt|decrypt/4

  Impact: Individual calls to crypto functions may take longer time but over all
  system performance should improve as crypto calls will not become throughput
  bottlenecks.

  Own Id: OTP-11142

## Crypto 2.3

### Improvements and New Features

- Enable runtime upgrade of crypto including the OpenSSL library used by crypto.

  Own Id: OTP-10596

- Improve documentation and tests for hmac functions in crypto. Thanks to Daniel
  White

  Own Id: OTP-10640

- Added ripemd160 support to crypto. Thanks to Michael Loftis

  Own Id: OTP-10667

## Crypto 2.2

### Fixed Bugs and Malfunctions

- Remove unnecessary dependency to libssl from crypto NIF library. This
  dependency was introduced by accident in R14B04.

  Own Id: OTP-10064

### Improvements and New Features

- Add crypto and public_key support for the hash functions SHA224, SHA256,
  SHA384 and SHA512 and also hmac and rsa_sign/verify support using these hash
  functions. Thanks to Andreas Schultz for making a prototype.

  Own Id: OTP-9908

- Optimize RSA private key handling in `crypto` and `public_key`.

  Own Id: OTP-10065

- Make `crypto:aes_cfb_128_encrypt` and `crypto:aes_cfb_128_decrypt` handle data
  and cipher with arbitrary length. (Thanks to Stefan Zegenhagen)

  Own Id: OTP-10136

## Crypto 2.1

### Improvements and New Features

- public_key, ssl and crypto now supports PKCS-8

  Own Id: OTP-9312

- Erlang/OTP can now be built using parallel make if you limit the number of
  jobs, for instance using '`make -j6`' or '`make -j10`'. '`make -j`' does not
  work at the moment because of some missing dependencies.

  Own Id: OTP-9451

- Add DES and Triple DES cipher feedback (CFB) mode functions to `crypto`.
  (Thanks to Paul Guyot)

  Own Id: OTP-9640

- Add sha256, sha384 and sha512 support for `crypto:rsa_verify`.

  Own Id: OTP-9778

## Crypto 2.0.4

### Fixed Bugs and Malfunctions

- `crypto:rand_uniform` works correctly for negative integers. Fails with
  `badarg` exception for invalid ranges (when `Hi =< Lo`) instead of returning
  incorrect output.

  Own Id: OTP-9526

- Fix win32 OpenSSL static linking (Thanks to Dave Cottlehuber)

  Own Id: OTP-9532

## Crypto 2.0.3

### Fixed Bugs and Malfunctions

- Various small documentation fixes (Thanks to Bernard Duggan)

  Own Id: OTP-9172

### Improvements and New Features

- New `crypto` support for streaming of AES CTR and HMAC. (Thanks to Travis
  Jensen)

  Own Id: OTP-9275

- Due to standard library DLL mismatches between versions of OpenSSL and
  Erlang/OTP, OpenSSL is now linked statically to the crypto driver on Windows.
  This fixes problems starting crypto when running Erlang as a service on all
  Windows versions.

  Own Id: OTP-9280

## Crypto 2.0.2.2

### Improvements and New Features

- Strengthened random number generation. (Thanks to Geoff Cant)

  Own Id: OTP-9225

## Crypto 2.0.2.1

### Improvements and New Features

- Misc. Updates.

  Own Id: OTP-9132

## Crypto 2.0.2

### Improvements and New Features

- AES CTR encryption support in `crypto`.

  Own Id: OTP-8752 Aux Id: seq11642

## Crypto 2.0.1

### Fixed Bugs and Malfunctions

- Crypto dialyzer type error in md5_mac and sha_mac.

  Own Id: OTP-8718

- RC4 stream cipher didn't work. This since the new NIF implementation of
  `crypto:rc4_encrypt_with_state/2` introduced in `crypto-2.0` didn't return an
  updated state. (Thanks to Paul Guyot)

  Own Id: OTP-8781

- A number of memory leaks in the crypto NIF library have been fixed.

  Own Id: OTP-8810

### Improvements and New Features

- Added erlang:system_info(build_type) which makes it easier to chose drivers,
  NIF libraries, etc based on build type of the runtime system.

  The NIF library for crypto can now be built for valgrind and/or debug as
  separate NIF libraries that will be automatically loaded if the runtime system
  has been built with a matching build type.

  Own Id: OTP-8760

## Crypto 2.0

### Improvements and New Features

- crypto application changed to use NIFs instead of driver.

  Own Id: OTP-8333

- des_ecb_encrypt/2 and des_ecb_decrypt/2 has been added to the crypto module.
  The crypto:md4/1 function has been documented.

  Own Id: OTP-8551

- The undocumented, unsupported, and deprecated function `lists:flat_length/1`
  has been removed.

  Own Id: OTP-8584

- New variants of `crypto:dss_sign` and `crypto:dss_verify` with an extra
  argument to control how the digest is calculated.

  Own Id: OTP-8700

## Crypto 1.6.4

### Improvements and New Features

- Cross compilation improvements and other build system improvements.

  Most notable:

  - Lots of cross compilation improvements. The old cross compilation support
    was more or less non-existing as well as broken. Please, note that the cross
    compilation support should still be considered as experimental. Also note
    that old cross compilation configurations cannot be used without
    modifications. For more information on cross compiling Erlang/OTP see the
    `$ERL_TOP/INSTALL-CROSS.md` file.
  - Support for staged install using
    [DESTDIR](http://www.gnu.org/prep/standards/html_node/DESTDIR.html). The old
    broken `INSTALL_PREFIX` has also been fixed. For more information see the
    `$ERL_TOP/INSTALL.md` file.
  - Documentation of the `release` target of the top `Makefile`. For more
    information see the `$ERL_TOP/INSTALL.md` file.
  - `make install` now by default creates relative symbolic links instead of
    absolute ones. For more information see the `$ERL_TOP/INSTALL.md` file.
  - `$ERL_TOP/configure --help=recursive` now works and prints help for all
    applications with `configure` scripts.
  - Doing `make install`, or `make release` directly after `make all` no longer
    triggers miscellaneous rebuilds.
  - Existing bootstrap system is now used when doing `make install`, or
    `make release` without a preceding `make all`.
  - The `crypto` and `ssl` applications use the same runtime library path when
    dynamically linking against `libssl.so` and `libcrypto.so`. The runtime
    library search path has also been extended.
  - The `configure` scripts of `erl_interface` and `odbc` now search for thread
    libraries and thread library quirks the same way as ERTS do.
  - The `configure` script of the `odbc` application now also looks for odbc
    libraries in `lib64` and `lib/64` directories when building on a 64-bit
    system.
  - The `config.h.in` file in the `erl_interface` application is now
    automatically generated in instead of statically updated which reduces the
    risk of `configure` tests without any effect.

  (Thanks to Henrik Riomar for suggestions and testing)

  (Thanks to Winston Smith for the AVR32-Linux cross configuration and testing)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8323

- The crypto module now supports Blowfish in ECB, CBC and OFB modes. (Thanks to
  Paul Oliver.)

  Own Id: OTP-8331

- The documentation is now possible to build in an open source environment after
  a number of bugs are fixed and some features are added in the documentation
  build process.

  \- The arity calculation is updated.

  \- The module prefix used in the function names for bif's are removed in the
  generated links so the links will look like
  "http://www.erlang.org/doc/man/erlang.html#append_element-2" instead of
  "http://www.erlang.org/doc/man/erlang.html#erlang:append_element-2".

  \- Enhanced the menu positioning in the html documentation when a new page is
  loaded.

  \- A number of corrections in the generation of man pages (thanks to Sergei
  Golovan)

  \- The legal notice is taken from the xml book file so OTP's build process can
  be used for non OTP applications.

  Own Id: OTP-8343

## Crypto 1.6.3

### Fixed Bugs and Malfunctions

- Suppressed false valgrind errors caused by libcrypto using uninitialized data
  as entropy.

  Own Id: OTP-8200

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8201

- When the crypto application failed to load the OpenSSL/LibEAY shared object,
  error indication was sparse. Now a more specific error message is sent to the
  error logger.

  Own Id: OTP-8281

## Crypto 1.6.2

### Fixed Bugs and Malfunctions

- Fixed emulator crash caused by crypto using an old openssl version that did
  not cope with large file descriptors.

  Own Id: OTP-8261 Aux Id: seq11434

## Crypto 1.6.1

### Fixed Bugs and Malfunctions

- `Makefile.in` has been updated to use the LDFLAGS environment variable (if
  set). (Thanks to Davide Pesavento.)

  Own Id: OTP-8157

### Improvements and New Features

- Support for Blowfish cfb64 added to `crypto`.

  Own Id: OTP-8096

- New function `crypto:aes_cbc_ivec`

  Own Id: OTP-8141

## Crypto 1.6

### Fixed Bugs and Malfunctions

- The `dh_compute_key` sometimes returned a SharedSecret of incorrect size.

  Own Id: OTP-7674

### Improvements and New Features

- Optimization for drivers by creating small binaries direct on process heap.

  Own Id: OTP-7762

## Crypto 1.5.3

### Improvements and New Features

- Added new functions: dss_verify/3, rsa_verify/3, rsa_verify/4, dss_sign/2,
  rsa_sign/2, rsa_sign/3, rsa_public_encrypt, rsa_private_decrypt/3,
  rsa_private_encrypt/3, rsa_public_decrypt/3, dh_generate_key/1,
  dh_generate_key/2, dh_compute_key/3.

  Own Id: OTP-7545

## Crypto 1.5.2.1

### Improvements and New Features

- Minor performance optimization.

  Own Id: OTP-7521

## Crypto 1.5.2

### Fixed Bugs and Malfunctions

- ./configure has been improved to find 64-bit OpenSSL libraries.

  Own Id: OTP-7270

### Improvements and New Features

- crypto and zlib drivers improved to allow concurrent smp access.

  Own Id: OTP-7262

## Crypto 1.5.1.1

### Improvements and New Features

- The linked in driver for the crypto application is now linked statically
  against the OpenSSL libraries, to avoid installation and runtime problems in
  connection to the OpenSSL library locations.

  Own Id: OTP-6680

- Minor Makefile changes.

  Own Id: OTP-6689

## Crypto 1.5

### Improvements and New Features

- It is now explicitly checked at start-up that the crypto driver is properly
  loaded (Thanks to Claes Wikstrom).

  Own Id: OTP-6109

## Crypto 1.4

### Improvements and New Features

- The previously undocumented and UNSUPPORTED `ssh` application has been updated
  and documented. This release of the `ssh` application is still considered to
  be a beta release and (if necessary) there could still be changes in its API
  before it reaches 1.0.

  Also, more cryptographic algorithms have been added to the `crypto`
  application.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-5631

## Crypto 1.3

### Improvements and New Features

- Added support for RFC 3826 - The Advanced Encryption Standard (AES) Cipher
  Algorithm in the SNMP User-based Security Model.  
  Martin Björklund

## Crypto 1.2.3

### Fixed Bugs and Malfunctions

- Linked in drivers in the crypto, and asn1 applications are now compiled with
  the -D_THREAD_SAFE and -D_REENTRANT switches on unix when the emulator has
  thread support enabled.

  Linked in drivers on MacOSX are not compiled with the undocumented -lbundle1.o
  switch anymore. Thanks to Sean Hinde who sent us a patch.

  Linked in driver in crypto, and port programs in ssl, now compiles on OSF1.

  Minor makefile improvements in runtime_tools.

  Own Id: OTP-5346

## Crypto 1.2.2

### Improvements and New Features

- Corrected error handling. If the port to the driver that crypto uses is
  unexpectedly closed (which should not happen during normal operation of
  crypto), crypto will terminate immediately (rather than crashing the next time
  crypto is used). Also corrected build problems on Mac OS X.

  Own Id: OTP-5279

## Crypto 1.2.1

### Fixed Bugs and Malfunctions

- It was not possible in R9 to relink the crypto driver. The object file was
  missing as well as an example makefile. The crypto driver object file is now
  released with the application (installed in priv/obj). An example makefile has
  also been added to the priv/obj directory. The makefile serves as an example
  of how to relink the driver on Unix (crypto_drv.so) or Windows
  (crypto_drv.dll).

  Own Id: OTP-4828 Aux Id: seq8193

## Crypto 1.2

### Improvements and New Features

- Previous versions of Crypto where delivered with statically linked binaries
  based on SSLeay. That is not longer the case. The current version of Crypto
  requires dynamically linked OpenSSL libraries that the user has to install.
  The library needed is `libcrypto.so` (Unix) or `libeay32.[lib|dll]` (Win32).
  For further details see the crypto(6) application manual page.
- This version of Crypto uses the new DES interface of OpenSSL 0.9.7, which is
  not backward compatible with earlier versions of OpenSSL.
- The functions `des_ede3_cbc_encrypt/5` and `des_ede3_cbc_decrypt/5` have been
  renamed to `des3_cbc_encrypt/5` and `des3_cbc_decrypt/5`, respectively. The
  old functions have been retained (they are deprecated and not listed in the
  crypto(3) manual page).

### Reported Fixed Bugs and Malfunctions

- The start of crypto failed on Windows, due to erroneous addition of a DES3
  algorithm.

  Own Id: OTP-4684  
  Aux Id: seq7864

## Crypto 1.1.3

### Reported Fixed Bugs and Malfunctions

- To obtain backward compatibility with the old SSLeay package, and with earlier
  versions of OpenSSL, the macro OPENSSL_DES_LIBDES_COMPATIBILITY has been added
  to `crypto_drv.c`. This is of importance only for the open source version of
  Crypto.

## Crypto 1.1.2

### Reported Fixed Bugs and Malfunctions

- In the manual page `m:crypto` the function names `md5_finish` and `sha_finish`
  have been changed to `md5_final` and `sha_final` to correctly document the
  implementation.

  Own Id: OTP-3409

## Crypto 1.1.1

Code replacement in runtime is supported. Upgrade can be done from from version
1.1 and downgrade to version 1.1.

### Improvements and New Features

- The driver part of the Crypto application has been updated to use the
  erl_driver header file. Version 1.1.1 requires emulator version 4.9.1 or
  later.

## Crypto 1.1

### Reported Fixed Bugs and Malfunctions

- On Windows the crypto_drv was incorrectly linked to static run-time libraries
  instead of dynamic ones.

  Own Id: OTP-3240

## Crypto 1.0

New application.
