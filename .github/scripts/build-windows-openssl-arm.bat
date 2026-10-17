@echo off
:: %CopyrightBegin%
::
:: SPDX-License-Identifier: Apache-2.0
::
:: Copyright Ericsson AB 2026. All Rights Reserved.
::
:: Licensed under the Apache License, Version 2.0 (the "License");
:: you may not use this file except in compliance with the License.
:: You may obtain a copy of the License at
::
::     http://www.apache.org/licenses/LICENSE-2.0
::
:: Unless required by applicable law or agreed to in writing, software
:: distributed under the License is distributed on an "AS IS" BASIS,
:: WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
:: See the License for the specific language governing permissions and
:: limitations under the License.
::
:: %CopyrightEnd%

:: Build a static OpenSSL for Windows ARM64 (aarch64) from official
:: upstream source and install it into C:\OpenSSL-Win64.
::
:: We build from the official OpenSSL *source* tarball (not a
:: third-party prebuilt binary) and verify its SHA-256 before building,
:: so no unverified binary is introduced into the crypto dependency.
::
:: erts/crypto.ac (host_os=win32 branch) does NOT recognize a plain
:: source-build install layout on Windows -- it only probes four fixed
:: paths under lib\VC. For a static pre-3.5 OpenSSL it expects:
::     C:\OpenSSL-Win64\lib\VC\static\libcrypto64MD.lib
::     C:\OpenSSL-Win64\lib\VC\static\libssl64MD.lib
:: (the Shining Light packaging layout). A source build installs the
:: static libs as lib\libcrypto.lib / lib\libssl.lib, so after
:: installing we lay them out under lib\VC\static with the 64MD names.
:: The 64MD name implies linkage against the dynamic CRT (/MD), which
:: is what the VC-WIN64-ARM target uses by default -- consistent with
:: how the crypto NIF is linked.
::
:: Must run inside a Visual Studio ARM64 cross environment, i.e. after
::   vcvarsall.bat x64_arm64
:: Requires Perl on PATH (OpenSSL Configure prerequisite). GitHub's
:: Windows runner images ship Strawberry Perl, so no extra install is
:: expected. The VC-WIN64-ARM target uses the MSVC assembler (armasm),
:: not NASM, so NASM is not required.

setlocal enabledelayedexpansion

:: OpenSSL 3.1.x is pinned deliberately: OTP has known issues with
:: OpenSSL versions later than 3.1 (the x64 Windows job also pins
:: 3.1.1).
set OPENSSL_VERSION=3.1.1
:: SHA-256 of openssl-3.1.1.tar.gz (official OpenSSL GitHub release).
set OPENSSL_SHA256=b3aa61334233b852b63ddb048df181177c2c659eb9d4376008118f9c08d07674
set OPENSSL_TARBALL=openssl-%OPENSSL_VERSION%.tar.gz
set OPENSSL_URL=https://github.com/openssl/openssl/releases/download/openssl-%OPENSSL_VERSION%/%OPENSSL_TARBALL%
set PREFIX=C:\OpenSSL-Win64

echo Downloading OpenSSL from %OPENSSL_URL%
curl --fail -L -o "%OPENSSL_TARBALL%" "%OPENSSL_URL%" || exit /b 1

echo Verifying SHA-256 of %OPENSSL_TARBALL%
set GOT_SHA256=
for /f "usebackq delims=" %%H in (`certutil -hashfile "%OPENSSL_TARBALL%" SHA256 ^| findstr /r "^[0-9a-f][0-9a-f]*$"`) do set GOT_SHA256=%%H
if /i not "%GOT_SHA256%"=="%OPENSSL_SHA256%" (
  echo ERROR: SHA-256 mismatch for %OPENSSL_TARBALL%
  echo   expected: %OPENSSL_SHA256%
  echo   got:      %GOT_SHA256%
  exit /b 1
)
echo SHA-256 OK

tar -xf "%OPENSSL_TARBALL%" || exit /b 1
cd "openssl-%OPENSSL_VERSION%" || exit /b 1

:: VC-WIN64-ARM is OpenSSL's ARM64 Windows target.
:: no-shared -> static libs; no-tests/no-makedepend -> faster CI build.
echo Configuring OpenSSL (VC-WIN64-ARM, static)
perl Configure VC-WIN64-ARM no-shared no-tests no-makedepend --prefix=%PREFIX% --openssldir=%PREFIX% || exit /b 1

echo Building OpenSSL
nmake || exit /b 1

echo Installing OpenSSL into %PREFIX%
nmake install_sw || exit /b 1

:: Relayout static libs into the path crypto.ac probes for (see header).
mkdir "%PREFIX%\lib\VC\static" 2>nul
copy /y "%PREFIX%\lib\libcrypto.lib" "%PREFIX%\lib\VC\static\libcrypto64MD.lib" || exit /b 1
copy /y "%PREFIX%\lib\libssl.lib"    "%PREFIX%\lib\VC\static\libssl64MD.lib"    || exit /b 1

echo OpenSSL %OPENSSL_VERSION% (ARM64, static) installed into %PREFIX%
endlocal
