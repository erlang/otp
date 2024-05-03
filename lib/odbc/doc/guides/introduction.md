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

The purpose of the Erlang ODBC application is to provide the programmer with an
ODBC interface that has a Erlang/OTP touch and feel. So that the programmer may
concentrate on solving his/her actual problem instead of struggling with
pointers and memory allocation which is not very relevant for Erlang. This user
guide will give you some information about technical issues and provide some
examples of how to use the Erlang ODBC interface.

## Prerequisites

It is assumed that the reader is familiar with the Erlang programming language,
concepts of OTP and has a basic understanding of relational databases and SQL.

## About ODBC

Open Database Connectivity (ODBC) is a Microsoft standard for accessing
relational databases that has become widely used. The ODBC standard provides a
c-level application programming interface (API) for database access. It uses
Structured Query Language (SQL) as its database access language.

## About the Erlang ODBC application

Provides an Erlang interface to communicate with relational SQL-databases. It is
built on top of Microsofts ODBC interface and therefore requires that you have
an ODBC driver to the database that you want to connect to. The Erlang ODBC
application is designed using the version 3.0 of the ODBC-standard, however
using the option `{scrollable_cursors, off} `for a connection has been known to
make it work for at least some 2.X drivers.
