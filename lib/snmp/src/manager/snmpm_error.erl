-module(snmpm_error).
-moduledoc """
Functions for Reporting SNMP Errors

[](){: #desc }

The module `snmpm_error` contains two callback functions which are called if an
error occurs at different times during manager operation. These functions in turn
calls the corresponding function in the configured error report module, which
implements the actual report functionality.

Two simple implementation(s) is provided with the toolkit; the modules
`m:snmpm_error_logger` which is the default and `m:snmpm_error_io`.

The error report module is configured using the directive `error_report_mod`,
see [configuration parameters](snmp_config.md#configuration_params).
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

report_err(Func, Format, Args) ->
    case report_module() of
        {ok, Mod} ->
            try Mod:Func(Format, Args)
            catch _:_ -> ok
            end;
        _ -> ok
    end.

report_module() ->
    try ets:lookup(snmpm_config_table, error_report_mod) of
        [{error_report_mod, Mod}] -> {ok, Mod}
    catch
        _:_:_ -> error
    end.
