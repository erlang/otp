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
# edoc_cmd

EDoc command line interface EScript.

## Description

This script is a command line entry point to both `edoc:application/2` and
`edoc:files/2` functions. It also allows to generate just the EEP-48 doc chunks
(using the `-chunks` flag) instead of the complete HTML documentation.

## edoc -app <app> \[-chunks]

EDoc is invoked via `edoc:application/2`, with the default set of options. If
`-chunks` is given, then only doc chunks will be generated, not the full HTML
documentation.

Run the script with no arguments to get the full list of options.

## edoc -files <erl_file>... \[-chunks]

EDoc is invoked via `edoc:files/2`, with the default set of options. If
`-chunks` is given, then only doc chunks will be generated, not the full HTML
documentation.

Run the script with no arguments to get the full list of options.
