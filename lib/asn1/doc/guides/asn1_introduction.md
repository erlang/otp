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
# Introduction

The ASN.1 application provides the following:

- An ASN.1 compiler for Erlang, which generates encode and decode functions to
  be used by Erlang programs sending and receiving ASN.1 specified data.
- Runtime functions used by the generated code.
- Support for the following encoding rules:
  - Basic Encoding Rules (BER)
  - Distinguished Encoding Rules (DER), a specialized form of BER that is used
    in security-conscious applications
  - Packed Encoding Rules (PER), both the aligned and unaligned variant

## Scope

This application covers all features of ASN.1 up to the 1997 edition of the
specification. In the 2002 edition, new features were introduced. The following
features of the 2002 edition are fully or partly supported:

- Decimal notation (for example, `"1.5e3`) for REAL values. The NR1, NR2, and
  NR3 formats as explained in ISO 6093 are supported.
- The `RELATIVE-OID` type for relative object identifiers is fully supported.
- The subtype constraint (`CONTAINING`/`ENCODED BY`) to constrain the content of
  an octet string or a bit string is parsed when compiling, but no further
  action is taken. This constraint is not a PER-visible constraint.
- The subtype constraint by regular expressions (`PATTERN`) for character string
  types is parsed when compiling, but no further action is taken. This
  constraint is not a PER-visible constraint.
- Multiple-line comments as in C, `/* ... */`, are supported.

## Prerequisites

It is assumed that the reader is familiar with the Erlang programming language,
concepts of OTP, and is familiar with the ASN.1 notation. The ASN.1 notation is
documented in the standard definition X.680, which is the primary text. It can
also be helpful, but not necessary, to read the standard definitions X.681,
X.682, X.683, X.690, and X.691.

A good book explaining those reference texts is Dubuisson: ASN.1 - Communication
Between Heterogeneous Systems, is free to download at
[http://www.oss.com/asn1/dubuisson.html](http://www.oss.com/asn1/dubuisson.html).
