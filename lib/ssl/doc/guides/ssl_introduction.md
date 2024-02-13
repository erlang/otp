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

Transport Layer Security (TLS) and its predecessor, the Secure Sockets Layer
(SSL), are cryptographic protocols designed to provide communications security
over a computer network. The protocols use X.509 certificates and hence public
key (asymmetric) cryptography to authenticate the counterpart with whom they
communicate, and to exchange a symmetric key for payload encryption. The
protocol provides data/message confidentiality (encryption), integrity (through
message authentication code checks) and host verification (through certificate
path validation). DTLS (Datagram Transport Layer Security) that is based on TLS
but datagram oriented instead of stream oriented.

## Prerequisites

It is assumed that the reader is familiar with the Erlang programming language,
the concepts of OTP, and has a basic understanding of TLS/DTLS.
