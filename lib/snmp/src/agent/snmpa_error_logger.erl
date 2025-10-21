%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
-module(snmpa_error_logger).
-moduledoc """
Functions for Reporting SNMP Errors through the error_logger

The module `snmpa_error_logger` implements the `snmpa_error_report` behaviour
(see `m:snmpa_error_report`) containing two callback functions which are called
in order to report SNMP errors.

This module provides a simple mechanism for reporting SNMP errors. Errors are
sent to the `error_logger` after a size check. Messages are truncated after 1024
chars. It is provided as an example.

This module is the default error report module, but can be explicitly
configured, see [snmpa_error](`m:snmpa_error#desc`) and
[configuration parameters](snmp_config.md#configuration_params).

## See Also

error_logger(3)
""".

-behaviour(snmpa_error_report).


%%%-----------------------------------------------------------------
%%% Implements different error mechanisms.
%%%-----------------------------------------------------------------
-export([user_err/2, config_err/2]).


%%-----------------------------------------------------------------
%% This function is called when there is an error in a user
%% supplied item, e.g. instrumentation function.
%%-----------------------------------------------------------------

-doc """
The function is called if a user related error occurs at run-time, for example
if a user defined instrumentation function returns erroneous.

`Format` and `Args` are as in `io:format(Format, Args)`.
""".
-spec user_err(Format, Args) -> snmp:void() when
      Format :: string(),
      Args   :: list().

user_err(F, A) -> 
    error_msg("** User error: ", F, A).


%%-----------------------------------------------------------------
%% This function is called when there is a configuration error,
%% either at startup (in a conf-file) or at run-time (e.g. when 
%% information in the configuration tables are inconsistent.)
%%-----------------------------------------------------------------

-doc """
The function is called if an error occurs during the configuration phase, for
example if a syntax error is found in a configuration file.

`Format` and `Args` are as in `io:format(Format, Args)`.
""".
-spec config_err(Format, Args) -> snmp:void() when
      Format :: string(),
      Args   :: list().

config_err(F, A) ->
    error_msg("** Configuration error: ", F, A).


error_msg(P, F, A) ->
    S = snmp_misc:format(1024, lists:concat([P, F, "\n"]), A),
    catch error_logger:error_msg("~s", [S]).

