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

The Trivial File Transfer Protocol or TFTP is a very simple protocol used to
transfer files.

It has been implemented on top of the User Datagram protocol (UDP) so it may be
used to move files between machines on different networks implementing UDP. It
is designed to be small and easy to implement. Therefore, it lacks most of the
features of a regular FTP. The only thing it can do is read and write files (or
mail) from/to a remote server. It cannot list directories, and currently has no
provisions for user authentication.

The `tftp` application implements the following IETF standards:

- RFC 1350, The TFTP Protocol (revision 2)
- RFC 2347, TFTP Option Extension
- RFC 2348, TFTP Blocksize Option
- RFC 2349, TFTP Timeout Interval and Transfer Size Options

The only feature that not is implemented is the `netascii` transfer mode.

## Prerequisites

It is assumed that the reader is familiar with the Erlang programming language,
concepts of OTP, and has a basic understanding of the TFTP protocol.
