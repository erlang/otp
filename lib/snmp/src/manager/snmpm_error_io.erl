-module(snmpm_error_io).
-moduledoc """
Functions for Reporting SNMP Errors on stdio

The module `snmpm_error_io` implements the `snmpm_error_report` behaviour (see
`m:snmpm_error_report`) containing two callback functions which are called in
order to report SNMP errors.

This module provides a simple mechanism for reporting SNMP errors. Errors are
written to stdout using the `io` module. It is provided as an simple example.

This module needs to be explicitly configured, see
[snmpm_error](`m:snmpm_error#desc`) and
[configuration parameters](snmp_config.md#configuration_params).
""".

-behavior(snmpm_error_report).

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
    error_msg("User error", Format, Args).

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
    error_msg("Configuration error", Format, Args).

error_msg(P, F, A) ->
    io:format("*** SNMP ~s *** ~n" ++ F ++ "~n", [P|A]).
