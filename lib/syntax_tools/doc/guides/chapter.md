<!--
%CopyrightBegin%

Copyright Ericsson AB 2023. All Rights Reserved.

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
# Erlang Syntax and Metaprogramming tools

## Overview

This package contains modules for handling abstract syntax trees (ASTs) in
Erlang, in a way that is compatible with the "abstract format" parse trees of
the stdlib module `erl_parse`, together with utilities for reading source files,
[pretty-printing syntax trees](`m:erl_prettypr`), and doing
[metaprogramming](`m:merl`) in Erlang.

The abstract layer (defined in `m:erl_syntax`) is nicely structured and the node
types are context-independent. The layer makes it possible to transparently
attach source-code comments and user annotations to nodes of the tree. Using the
abstract layer makes applications less sensitive to changes in the `m:erl_parse`
data structures, only requiring the `erl_syntax` module to be up-to-date.

The pretty printer `m:erl_prettypr` is implemented on top of the library module
`m:prettypr`: this is a powerful and flexible generic pretty printing library,
which is also distributed separately.

For a short demonstration of parsing and pretty-printing, simply compile the
included module [assets/demo.erl](assets/demo.erl), and execute `demo:run()` from the Erlang
shell. It will compile the remaining modules and give you further instructions.
