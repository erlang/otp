%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2004-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(snmpa_error_report).
-moduledoc """
Behaviour module for reporting SNMP agent errors

[](){: #desc } This module defines the behaviour of the agent error reporting. A
`snmpa_error_report` compliant module must export the following functions:

- config_err/2
- user_err/2

The semantics of them and their exact signatures are explained below.
""".

-doc """
The function is called if an error occurs during the configuration phase, for
example if a syntax error is found in a configuration file.

`Format` and `Args` are as in `io:format(Format, Args)`.
""".
-callback config_err(Format, Args) ->
    snmp:void() when
      Format :: string(),
      Args   :: [term()].

-doc """
The function is called if a user related error occurs at run-time, for example
if a user defined instrumentation function returns erroneous.

`Format` and `Args` are as in `io:format(Format, Args)`.
""".
-callback user_err(Format, Args) ->
    snmp:void() when
      Format :: string(),
      Args   :: [term()].
