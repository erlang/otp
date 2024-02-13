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

## Purpose

The Public Key application deals with public-key related file formats, digital
signatures, and [X-509 certificates](http://www.ietf.org/rfc/rfc5280.txt). It is
a library application that provides encode/decode, sign/verify, encrypt/decrypt,
and similar functionality. It does not read or write files, it expects or
returns file contents or partial file contents as binaries.

## Prerequisites

It is assumed that the reader is familiar with the Erlang programming language
and has a basic understanding of the concepts of using public-keys and digital
certificates.

## Performance Tips

The Public Key decode- and encode-functions try to use the NIFs in the ASN.1
compilers runtime modules, if they can be found. Thus, to have the ASN1
application in the path of your system gives the best performance.
