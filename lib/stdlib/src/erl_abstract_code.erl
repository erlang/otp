-module(erl_abstract_code).
-export([debug_info/4]).

debug_info(_Format, _Module, {none,_CompilerOpts}, _Opts) ->
    {error, missing};
debug_info(erlang_v1, _Module, {AbstrCode,_CompilerOpts}, _Opts) ->
    {ok, AbstrCode};
debug_info(core_v1, _Module, {AbstrCode,CompilerOpts}, Opts) ->
    CoreOpts = add_core_returns(delete_reports(CompilerOpts ++ Opts)),
    try compile:noenv_forms(AbstrCode, CoreOpts) of
	{ok, _, Core, _} -> {ok, Core};
	_What -> {error, failed_conversion}
    catch
	error:_ -> {error, failed_conversion}
    end;
debug_info(_, _, _, _) ->
    {error, unknown_format}.

delete_reports(Opts) ->
    [Opt || Opt <- Opts, not is_report_option(Opt)].

is_report_option(report) -> true;
is_report_option(report_errors) -> true;
is_report_option(report_warnings) -> true;
is_report_option(_) -> false.

add_core_returns(Opts) ->
    [to_core, return_errors, return_warnings] ++ Opts.
