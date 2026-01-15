%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2002-2025. All Rights Reserved.
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
-module(snmpa_error).
-moduledoc """
Functions for Reporting SNMP Errors

[](){: #desc }

The module `snmpa_error` contains two callback functions which are called if an
error occurs at different times during agent operation. These functions in turn
calls the corresponding function in the configured error report module, which
implements the actual report functionality.

Two simple implementation(s) is provided with the toolkit; the modules
`m:snmpa_error_logger` which is the default and `m:snmpa_error_io`.

The error report module is configured using the directive `error_report_mod`,
see [configuration parameters](snmp_config.md#configuration_params).
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

user_err(Format, Args) -> 
    report_err(user_err, Format, Args).


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

config_err(Format, Args) ->
    report_err(config_err, Format, Args).


%% -----------------------------------------------------------------


report_err(Func, Format, Args) ->
    case report_module() of
	{ok, Mod} ->
	    (catch Mod:Func(Format, Args));
	_ ->
	    ok
    end.
       

    
report_module() ->
    case (catch ets:lookup(snmp_agent_table, error_report_mod)) of
	[{error_report_mod, Mod}] ->
	    {ok, Mod};
	_ ->
	    error
    end.
