%% =====================================================================
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%
%% Copyright 2001-2006 Richard Carlsson
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% %CopyrightEnd%
%%
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @end
%% =====================================================================

%% NOTES:
%%
%% * It's OK if the result does not parse - then at least nothing
%% strange happens, and the user can resort to full preprocessing.
%% However, we must avoid generating a token stream that is accepted by
%% the parser, but has a different meaning than the intended. A typical
%% example is when someone uses token-level string concatenation with
%% macros, as in `"foo" ?bar' (where `?bar' expands to a string). If we
%% replace the tokens `? bar' with `( ... )', to preserve precedence,
%% the result will be parsed as an application `"foo" ( ... )' and cause
%% trouble later on. We must detect such cases and report an error.
%%
%% * It is pointless to add a mechanism for tracking which macros are
%% known to take arguments, and which are known to take no arguments,
%% since a lot of the time we will not have seen the macro definition
%% anyway (it's usually in a header file). Hence, we try to use
%% heuristics instead. In most cases, the token sequence `? foo ('
%% indicates that it is a call of a macro that is supposed to take
%% arguments, but e.g., in the context `: ? foo (', the argument list
%% typically belongs to a remote function call, as in `m:?f(...)' and
%% should be parsed as `m:(?f)(...)' unless it is actually a try-clause
%% pattern such as `throw:?f(...) ->'.
%%
%% * We do our best to make macros without arguments pass the parsing
%% stage transparently. Atoms are accepted in most contexts, but
%% variables are not, so we use only atoms to encode these macros.
%% Sadly, the parsing sometimes discards even the location info from
%% atom tokens, so we can only use the actual characters for this.
%%
%% * We recognize `?m(...' at the start of a form and prevent this from
%% being interpreted as a macro with arguments, since it is probably a
%% function definition. Likewise with attributes `-?m(...'.

-module(epp_dodger).
-moduledoc """
Bypassing the Erlang preprocessor.

This module tokenises and parses most Erlang source code without expanding
preprocessor directives and macro applications, as long as these are
syntactically "well-behaved". Because the normal parse trees of the `erl_parse`
module cannot represent these things (normally, they are expanded by the Erlang
preprocessor [`//stdlib/epp`](`m:epp`) before the parser sees them), an extended
syntax tree is created, using the `m:erl_syntax` module.
""".

-compile(nowarn_deprecated_catch).

-export([parse_file/1, quick_parse_file/1, parse_file/2,
	 quick_parse_file/2, parse/1, quick_parse/1, parse/2,
	 quick_parse/2, parse/3, quick_parse/3, parse_form/2,
	 parse_form/3, quick_parse_form/2, quick_parse_form/3,
	 format_error/1, tokens_to_string/1]).


%% The following should be: 1) pseudo-uniquely identifiable, and 2)
%% cause nice looking error messages when the parser has to give up.

-define(macro_call, '? <macro> (').
-define(atom_prefix, "? ").
-define(var_prefix, "?,").
-define(pp_form, '?preprocessor declaration?').


-type errorinfo() :: erl_scan:error_info().

-type option() :: atom() | {atom(), term()}.

-doc #{equiv => parse_file(File, [])}.
-spec parse_file(file:filename()) ->
        {'ok', erl_syntax:forms()} | {'error', errorinfo()}.

parse_file(File) ->
    parse_file(File, []).

-doc """
Reads and parses a file.

If successful, `{ok, Forms}` is returned, where `Forms` is a list of
abstract syntax trees representing the "program forms" of the file
(see `erl_syntax:is_form/1`). Otherwise, `{error, errorinfo()}` is
returned, typically if the file could not be opened. Note that parse
errors show up as error markers in the returned list of forms; they do
not cause this function to fail or return `{error, errorinfo()}`.

Options:

- **`{no_fail, boolean()}`** - If `true`, this makes `epp_dodger` replace any
  program forms that could not be parsed with nodes of type `text` (see
  `erl_syntax:text/1`), representing the raw token sequence of the form, instead
  of reporting a parse error. The default value is `false`.

- **`{clever, boolean()}`** - If set to `true`, this makes `epp_dodger` try to
  repair the source code as it seems fit, in certain cases where parsing would
  otherwise fail. Currently, it inserts `++` operators between string literals
  and macros where it looks like concatenation was intended. The default value
  is `false`.

_See also: _`parse/2`, `quick_parse_file/1`, `erl_syntax:is_form/1`.
""".
-spec parse_file(file:filename(), [option()]) ->
        {'ok', erl_syntax:forms()} | {'error', errorinfo()}.

parse_file(File, Options) ->
    parse_file(File, fun parse/3, Options).

-doc #{equiv => quick_parse_file(File, [])}.
-spec quick_parse_file(file:filename()) ->
        {'ok', erl_syntax:forms()} | {'error', errorinfo()}.

quick_parse_file(File) ->
    quick_parse_file(File, []).


-doc """
Similar to `parse_file/2`, but does a more quick-and-dirty processing of the
code.

Macro definitions and other preprocessor directives are discarded, and all
macro calls are replaced with atoms. This is useful when only the main structure
of the code is of interest, and not the details. Furthermore, the quick-parse
method can usually handle more strange cases than the normal, more exact
parsing.

Options: see `parse_file/2`. However, note that for
[`quick_parse_file/2`](`quick_parse_file/2`), the option `no_fail` is `true` by
default.

_See also: _`parse_file/2`, `quick_parse/2`.
""".
-spec quick_parse_file(file:filename(), [option()]) ->
        {'ok', erl_syntax:forms()} | {'error', errorinfo()}.

quick_parse_file(File, Options) ->
    parse_file(File, fun quick_parse/3, Options ++ [no_fail]).

parse_file(File, Parser, Options) ->
    case do_parse_file(utf8, File, Parser, Options) of
        {ok, Forms}=Ret ->
            case find_invalid_unicode(Forms) of
                none ->
                    Ret;
                invalid_unicode ->
                    case epp:read_encoding(File) of
                        utf8 ->
                            Ret;
                        _ ->
                            do_parse_file(latin1, File, Parser, Options)
                    end
            end;
        Else ->
            Else
    end.

do_parse_file(DefEncoding, File, Parser, Options) ->
    case file:open(File, [read]) of
        {ok, Dev} ->
            _ = epp:set_encoding(Dev, DefEncoding),
            try Parser(Dev, 1, Options)
            after ok = file:close(Dev)
	    end;
        {error, Error} ->
            {error, {0, file, Error}}  % defer to file:format_error/1
    end.

find_invalid_unicode([H|T]) ->
    case H of
	{error, {_Location, file_io_server, invalid_unicode}} ->
	    invalid_unicode;
	_Other ->
	    find_invalid_unicode(T)
    end;
find_invalid_unicode([]) -> none.

%% =====================================================================

-doc #{equiv => parse(IODevice, 1)}.
-spec parse(file:io_device()) -> {'ok', erl_syntax:forms()}.

parse(Dev) ->
    parse(Dev, 1).

-doc #{equiv => parse(IODevice, StartLocation, [])}.
-spec parse(file:io_device(), erl_anno:location()) -> {'ok', erl_syntax:forms()}.

parse(Dev, L) ->
    parse(Dev, L, []).

-doc """
Reads and parses program text from an I/O stream.

Characters are read from `IODevice` until end-of-file; apart from
this, the behavior is the same as for `parse_file/2`. `StartLocation`
is the initial location.

_See also: _`parse/2`, `parse_file/2`, `parse_form/2`, `quick_parse/3`.
""".
-spec parse(file:io_device(), erl_anno:location(), [option()]) ->
        {'ok', erl_syntax:forms()}.

parse(Dev, L0, Options) ->
    parse(Dev, L0, fun parse_form/3, Options).

-doc #{equiv => quick_parse(IODevice, 1)}.
-spec quick_parse(file:io_device()) ->
        {'ok', erl_syntax:forms()}.

quick_parse(Dev) ->
    quick_parse(Dev, 1).

-doc #{equiv => quick_parse(IODevice, StartLocation, [])}.
-spec quick_parse(file:io_device(), erl_anno:location()) ->
        {'ok', erl_syntax:forms()}.

quick_parse(Dev, L) ->
    quick_parse(Dev, L, []).

-doc """
Similar to `parse/3`, but does a more quick-and-dirty processing of the code.

See `quick_parse_file/2` for details.

_See also: _`parse/3`, `quick_parse/2`, `quick_parse_file/2`,
`quick_parse_form/2`.
""".
-spec quick_parse(file:io_device(), erl_anno:location(), [option()]) ->
        {'ok', erl_syntax:forms()}.

quick_parse(Dev, L0, Options) ->
    parse(Dev, L0, fun quick_parse_form/3, Options).

parse(Dev, L0, Parser, Options) ->
    parse(Dev, L0, [], Parser, Options).

parse(Dev, L0, Fs, Parser, Options) ->
    case Parser(Dev, L0, Options) of
        {ok, none, L1} ->
            parse(Dev, L1, Fs, Parser, Options);
        {ok, F, L1} ->
            parse(Dev, L1, [F | Fs], Parser, Options);
        {error, IoErr, L1} ->
            parse(Dev, L1, [{error, IoErr} | Fs], Parser, Options);
        {eof, _L1} ->
            {ok, lists:reverse(Fs)}
    end.


-doc #{equiv => parse_form(IODevice, StartLocation, [])}.
-spec parse_form(file:io_device(), erl_anno:location()) ->
        {'ok', erl_syntax:forms(), erl_anno:location()}
      | {'eof', erl_anno:location()} | {'error', errorinfo(), erl_anno:location()}.

parse_form(Dev, L0) ->
    parse_form(Dev, L0, []).

-doc """
Reads and parses a single program form from an I/O stream.

Characters are read from `IODevice` until an end-of-form marker is
found (a period character followed by whitespace), or until
end-of-file; apart from this, the behavior is similar to that of
[`parse/3`](`parse/3`), except that the return values also contain the
final location given that `StartLocation` is the initial location, and
that `{eof, Location}` may be returned.

_See also: _`parse/3`, `parse_form/2`, `quick_parse_form/3`.
""".
-spec parse_form(file:io_device(), erl_anno:location(), [option()]) ->
        {'ok', erl_syntax:forms(), erl_anno:location()}
      | {'eof', erl_anno:location()} | {'error', errorinfo(), erl_anno:location()}.

parse_form(Dev, L0, Options) ->
    parse_form(Dev, L0, fun normal_parser/2, Options).

-doc #{equiv => quick_parse_form(IODevice, StartLocation, [])}.
-spec quick_parse_form(file:io_device(), erl_anno:location()) ->
        {'ok', erl_syntax:forms(), erl_anno:location()}
      | {'eof', erl_anno:location()} | {'error', errorinfo(), erl_anno:location()}.

quick_parse_form(Dev, L0) ->
    quick_parse_form(Dev, L0, []).

-doc """
Similar to `parse_form/3`, but does a more quick-and-dirty processing of the
code. See `quick_parse_file/2` for details.

_See also: _`parse/3`, `parse_form/3`, `quick_parse_form/2`.
""".
-spec quick_parse_form(file:io_device(), erl_anno:location(), [option()]) ->
        {'ok', erl_syntax:forms(), erl_anno:location()}
      | {'eof', erl_anno:location()} | {'error', errorinfo(), erl_anno:location()}.

quick_parse_form(Dev, L0, Options) ->
    parse_form(Dev, L0, fun quick_parser/2, Options).

-record(opt, {clever = false :: boolean()}).

parse_form(Dev, L0, Parser, Options) ->
    NoFail = proplists:get_bool(no_fail, Options),
    Opt = #opt{clever = proplists:get_bool(clever, Options)},

    %% This as the *potential* to read options for enabling/disabling
    %% features for the parsing of the file.
    {ok, {_Ftrs, ResWordFun}} =
        erl_features:keyword_fun(Options, fun reserved_word/1),

    case io:scan_erl_form(Dev, "", L0, [{reserved_word_fun,ResWordFun}]) of
        {ok, Ts, L1} ->
            case catch {ok, Parser(Ts, Opt)} of
                {'EXIT', Term} ->
                    {error, io_error(L1, {unknown, Term}), L1};
                {error, Term} ->
		    IoErr = io_error(L1, Term),
		    {error, IoErr, L1};
                {parse_error, _IoErr} when NoFail ->
		    {ok, erl_syntax:set_pos(
			   erl_syntax:text(tokens_to_string(Ts)),
			   erl_anno:new(start_pos(Ts, L1))),
		     L1};
                {parse_error, IoErr} ->
		    {error, IoErr, L1};
                {ok, F} ->
                    {ok, F, L1}
            end;
        {error, _IoErr, _L1} = Err -> Err;
        {error, _Reason} -> {eof, L0}; % This is probably encoding problem
        {eof, _L1} = Eof -> Eof
    end.

io_error(L, Desc) ->
    {L, ?MODULE, Desc}.

start_pos([T | _Ts], _L) ->
    erl_anno:location(element(2, T));
start_pos([], L) ->
    L.

%% Exception-throwing wrapper for the standard Erlang parser stage

parse_tokens(Ts) ->
    parse_tokens(Ts, fun fix_form/1).

parse_tokens(Ts, Fix) ->
    case erl_parse:parse_form(Ts) of
        {ok, Form} ->
            Form;
        {error, IoErr} ->
	    case Fix(Ts) of
		{form, Form} ->
		    Form;
		{retry, Ts1, Fix1} ->
		    parse_tokens(Ts1, Fix1);
		error ->
		    throw({parse_error, IoErr})
	    end
    end.

%% ---------------------------------------------------------------------
%% Quick scanning/parsing - deletes macro definitions and other
%% preprocessor directives, and replaces all macro calls with atoms.

quick_parser(Ts, _Opt) ->
    filter_form(parse_tokens(quickscan_form(Ts))).

quickscan_form([{'-', _Anno}, {atom, AnnoA, define} | _Ts]) ->
    kill_form(AnnoA);
quickscan_form([{'-', _Anno}, {atom, AnnoA, undef} | _Ts]) ->
    kill_form(AnnoA);
quickscan_form([{'-', _Anno}, {atom, AnnoA, include} | _Ts]) ->
    kill_form(AnnoA);
quickscan_form([{'-', _Anno}, {atom, AnnoA, include_lib} | _Ts]) ->
    kill_form(AnnoA);
quickscan_form([{'-', _Anno}, {atom, AnnoA, ifdef} | _Ts]) ->
    kill_form(AnnoA);
quickscan_form([{'-', _Anno}, {atom, AnnoA, ifndef} | _Ts]) ->
    kill_form(AnnoA);
quickscan_form([{'-', _Anno}, {'if', AnnoA} | _Ts]) ->
    kill_form(AnnoA);
quickscan_form([{'-', _Anno}, {atom, AnnoA, elif} | _Ts]) ->
    kill_form(AnnoA);
quickscan_form([{'-', _Anno}, {atom, AnnoA, 'else'} | _Ts]) ->
    kill_form(AnnoA);
quickscan_form([{'-', _Anno}, {'else', AnnoA} | _Ts]) ->
    kill_form(AnnoA);
quickscan_form([{'-', _Anno}, {atom, AnnoA, endif} | _Ts]) ->
    kill_form(AnnoA);
quickscan_form([{'-', _Anno}, {atom, AnnoA, feature} | _Ts]) ->
   kill_form(AnnoA);
quickscan_form([{'-', Anno}, {'?', _}, {Type, _, _}=N | [{'(', _} | _]=Ts])
  when Type =:= atom; Type =:= var ->
    %% minus, macro and open parenthesis at start of form - assume that
    %% the macro takes no arguments; e.g. `-?foo(...).'
    quickscan_macros_1(N, Ts, [{'-', Anno}]);
quickscan_form([{'?', _Anno}, {Type, _, _}=N | [{'(', _} | _]=Ts])
  when Type =:= atom; Type =:= var ->
    %% macro and open parenthesis at start of form - assume that the
    %% macro takes no arguments (see scan_macros for details)
    quickscan_macros_1(N, Ts, []);
quickscan_form(Ts) ->
    quickscan_macros(Ts).

kill_form(A) ->
    [{atom, A, ?pp_form}, {'(', A}, {')', A}, {'->', A}, {atom, A, kill},
     {dot, A}].

quickscan_macros(Ts) ->
    quickscan_macros(Ts, []).

quickscan_macros([{'?',_}, {Type, _, A} | Ts], [{string, AnnoS, S} | As])
  when Type =:= atom; Type =:= var ->
    %% macro after a string literal: change to a single string
    {_, Ts1} = skip_macro_args(Ts),
    S1 = S ++ quick_macro_string(A),
    quickscan_macros(Ts1, [{string, AnnoS, S1} | As]);
quickscan_macros([{'?',_}, {Type, _, _}=N | [{'(',_}|_]=Ts],
		 [{':',_}|_]=As)
  when Type =:= atom; Type =:= var ->
    %% macro and open parenthesis after colon - check the token
    %% following the arguments (see scan_macros for details)
    Ts1 = case skip_macro_args(Ts) of
	      {_, [{'->',_} | _] = Ts2} -> Ts2;
	      {_, [{'when',_} | _] = Ts2} -> Ts2;
	      {_, [{':',_} | _] = Ts2} -> Ts2;
	      _ -> Ts    %% assume macro without arguments
	  end,
    quickscan_macros_1(N, Ts1, As);
quickscan_macros([{'?',_}, {Type, _, _}=N | Ts], As)
  when Type =:= atom; Type =:= var ->
    %% macro with or without arguments
    {_, Ts1} = skip_macro_args(Ts),
    quickscan_macros_1(N, Ts1, As);
quickscan_macros([T | Ts], As) ->
    quickscan_macros(Ts, [T | As]);
quickscan_macros([], As) ->
    lists:reverse(As).

%% (after a macro has been found and the arglist skipped, if any)
quickscan_macros_1({_Type, _, A}, [{string, AnnoS, S} | Ts], As) ->
    %% string literal following macro: change to single string
    S1 = quick_macro_string(A) ++ S,
    quickscan_macros(Ts, [{string, AnnoS, S1} | As]);
quickscan_macros_1({_Type, AnnoA, A}, Ts, As) ->
    %% normal case - just replace the macro with an atom
    quickscan_macros(Ts, [{atom, AnnoA, quick_macro_atom(A)} | As]).

quick_macro_atom(A) ->
    list_to_atom("?" ++ atom_to_list(A)).

quick_macro_string(A) ->
    "(?" ++ atom_to_list(A) ++ ")".

%% Skipping to the end of a macro call, tracking open/close constructs.
%% @spec (Tokens) -> {Skipped, Rest}

skip_macro_args([{'(',_}=T | Ts]) ->
    skip_macro_args(Ts, [')'], [T]);
skip_macro_args(Ts) ->
    {[], Ts}.

skip_macro_args([{'(',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, [')' | Es], [T | As]);
skip_macro_args([{'{',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, ['}' | Es], [T | As]);
skip_macro_args([{'[',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, [']' | Es], [T | As]);
skip_macro_args([{'<<',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, ['>>' | Es], [T | As]);
skip_macro_args([{'begin',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, ['end' | Es], [T | As]);
skip_macro_args([{'if',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, ['end' | Es], [T | As]);
skip_macro_args([{'case',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, ['end' | Es], [T | As]);
skip_macro_args([{'receive',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, ['end' | Es], [T | As]);
skip_macro_args([{'try',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, ['end' | Es], [T | As]);
skip_macro_args([{E,_}=T | Ts], [E], As) ->		%final close
    {lists:reverse([T | As]), Ts};
skip_macro_args([{E,_}=T | Ts], [E | Es], As) ->	%matching close
    skip_macro_args(Ts, Es, [T | As]);
skip_macro_args([T | Ts], Es, As) ->
    skip_macro_args(Ts, Es, [T | As]);
skip_macro_args([], _Es, _As) ->
    throw({error, macro_args}).

filter_form({function, _, ?pp_form, _,
	     [{clause, _, [], [], [{atom, _, kill}]}]}) ->
    none;
filter_form(T) ->
    T.


%% ---------------------------------------------------------------------
%% Normal parsing - try to preserve all information

normal_parser(Ts0, Opt) ->
    case scan_form(Ts0, Opt) of
	Ts when is_list(Ts) ->
	    rewrite_form(parse_tokens(Ts));
	Node ->
	    Node
    end.

scan_form([{'-', _Anno}, {atom, AnnoA, define} | Ts], Opt) ->
    [{atom, AnnoA, ?pp_form}, {'(', AnnoA}, {')', AnnoA}, {'->', AnnoA},
     {atom, AnnoA, define} | scan_macros(Ts, Opt)];
scan_form([{'-', _Anno}, {atom, AnnoA, undef} | Ts], Opt) ->
    [{atom, AnnoA, ?pp_form}, {'(', AnnoA}, {')', AnnoA}, {'->', AnnoA},
     {atom, AnnoA, undef} | scan_macros(Ts, Opt)];
scan_form([{'-', _Anno}, {atom, AnnoA, include} | Ts], Opt) ->
    [{atom, AnnoA, ?pp_form}, {'(', AnnoA}, {')', AnnoA}, {'->', AnnoA},
     {atom, AnnoA, include} | scan_macros(Ts, Opt)];
scan_form([{'-', _Anno}, {atom, AnnoA, include_lib} | Ts], Opt) ->
    [{atom, AnnoA, ?pp_form}, {'(', AnnoA}, {')', AnnoA}, {'->', AnnoA},
     {atom, AnnoA, include_lib} | scan_macros(Ts, Opt)];
scan_form([{'-', _Anno}, {atom, AnnoA, ifdef} | Ts], Opt) ->
    [{atom, AnnoA, ?pp_form}, {'(', AnnoA}, {')', AnnoA}, {'->', AnnoA},
     {atom, AnnoA, ifdef} | scan_macros(Ts, Opt)];
scan_form([{'-', _Anno}, {atom, AnnoA, ifndef} | Ts], Opt) ->
    [{atom, AnnoA, ?pp_form}, {'(', AnnoA}, {')', AnnoA}, {'->', AnnoA},
     {atom, AnnoA, ifndef} | scan_macros(Ts, Opt)];
scan_form([{'-', _Anno}, {'if', AnnoA} | Ts], Opt) ->
    [{atom, AnnoA, ?pp_form}, {'(', AnnoA}, {')', AnnoA}, {'->', AnnoA},
     {atom, AnnoA, 'if'} | scan_macros(Ts, Opt)];
scan_form([{'-', _Anno}, {atom, AnnoA, elif} | Ts], Opt) ->
    [{atom, AnnoA, ?pp_form}, {'(', AnnoA}, {')', AnnoA}, {'->', AnnoA},
     {atom, AnnoA, 'elif'} | scan_macros(Ts, Opt)];
scan_form([{'-', _Anno}, {atom, AnnoA, 'else'} | Ts], Opt) ->
    [{atom, AnnoA, ?pp_form}, {'(', AnnoA}, {')', AnnoA}, {'->', AnnoA},
     {atom, AnnoA, 'else'} | scan_macros(Ts, Opt)];
scan_form([{'-', _Anno}, {'else', AnnoA} | Ts], Opt) ->
    [{atom, AnnoA, ?pp_form}, {'(', AnnoA}, {')', AnnoA}, {'->', AnnoA},
     {atom, AnnoA, 'else'} | scan_macros(Ts, Opt)];
scan_form([{'-', _Anno}, {atom, AnnoA, endif} | Ts], Opt) ->
    [{atom, AnnoA, ?pp_form}, {'(', AnnoA}, {')', AnnoA}, {'->', AnnoA},
     {atom, AnnoA, endif} | scan_macros(Ts, Opt)];
scan_form([{'-', _Anno}, {atom, AnnoA, error} | Ts], _Opt) ->
    Desc = build_info_string("-error", Ts),
    ErrorInfo = {erl_anno:location(AnnoA), ?MODULE, {error, Desc}},
    erl_syntax:error_marker(ErrorInfo);
scan_form([{'-', _Anno}, {atom, AnnoA, warning} | Ts], _Opt) ->
    Desc = build_info_string("-warning", Ts),
    ErrorInfo = {erl_anno:location(AnnoA), ?MODULE, {warning, Desc}},
    erl_syntax:error_marker(ErrorInfo);
scan_form([{'-', A}, {'?', A1}, {Type, _, _}=N | [{'(', _} | _]=Ts], Opt)
  when Type =:= atom; Type =:= var ->
    %% minus, macro and open parenthesis at start of form - assume that
    %% the macro takes no arguments; e.g. `-?foo(...).'
    macro(A1, N, Ts, [{'-', A}], Opt);
scan_form([{'?', A}, {Type, _, _}=N | [{'(', _} | _]=Ts], Opt)
  when Type =:= atom; Type =:= var ->
    %% macro and open parenthesis at start of form - assume that the
    %% macro takes no arguments; probably a function declaration on the
    %% form `?m(...) -> ...', which will not parse if it is rewritten as
    %% `(?m(...)) -> ...', so it must be handled as `(?m)(...) -> ...'
    macro(A, N, Ts, [], Opt);
scan_form(Ts, Opt) ->
    scan_macros(Ts, Opt).

build_info_string(Prefix, Ts0) ->
    Ts = lists:droplast(Ts0),
    String = lists:droplast(tokens_to_string(Ts)),
    Prefix ++ " " ++ String ++ ".".

scan_macros(Ts, Opt) ->
    scan_macros(Ts, [], Opt).

scan_macros([{'?', _}=M, {Type, _, _}=N | Ts], [{string, AnnoS, _}=S | As],
 	    #opt{clever = true}=Opt)
  when Type =:= atom; Type =:= var ->
    %% macro after a string literal: be clever and insert ++
    scan_macros([M, N | Ts], [{'++', AnnoS}, S | As], Opt);
scan_macros([{'?', Anno}, {Type, _, _}=N | [{'(',_}|_]=Ts],
	    [{':',_}|_]=As, Opt)
  when Type =:= atom; Type =:= var ->
    %% macro and open parentheses after colon - probably a call
    %% `m:?F(...)' so the argument list might belong to the call, not
    %% the macro - but it could also be a try-clause pattern
    %% `...:?T(...) ->' - we need to check the token following the
    %% arguments to decide
    {Args, Rest} = skip_macro_args(Ts),
    case Rest of
	[{'->',_} | _] ->
	    macro_call(Args, Anno, N, Rest, As, Opt);
	[{'when',_} | _] ->
	    macro_call(Args, Anno, N, Rest, As, Opt);
	[{':',_} | _] ->
	    macro_call(Args, Anno, N, Rest, As, Opt);
	_ ->
	    macro(Anno, N, Ts, As, Opt)
    end;
scan_macros([{'?', Anno}, {Type, _, _}=N | [{'(',_}|_]=Ts], As, Opt)
  when Type =:= atom; Type =:= var ->
    %% macro with arguments
    {Args, Rest} = skip_macro_args(Ts),
    macro_call(Args, Anno, N, Rest, As, Opt);
scan_macros([{'?', Anno }, {Type, _, _}=N | Ts], As, Opt)
  when Type =:= atom; Type =:= var ->
    %% macro without arguments
    macro(Anno, N, Ts, As, Opt);
scan_macros([T | Ts], As, Opt) ->
    scan_macros(Ts, [T | As], Opt);
scan_macros([], As, _Opt) ->
    lists:reverse(As).

%% Rewriting to a tuple which will be recognized by the post-parse pass
%% (we insert parentheses to preserve the precedences when parsing).

macro(Anno, {Type, _, A}, Rest, As, Opt) ->
    scan_macros_1([], Rest, [{atom,Anno,macro_atom(Type,A)} | As], Opt).

macro_call([{'(',_}, {')',_}], Anno, {_, AnnoN, _}=N, Rest, As, Opt) ->
    {Open, Close} = parentheses(As),
    scan_macros_1([], Rest,
                  %% {'?macro_call', N }
                  lists:reverse(Open ++ [{'{', Anno},
                                         {atom, Anno, ?macro_call},
                                         {',', Anno},
                                         N,
                                         {'}', AnnoN}] ++ Close,
                                As), Opt);
macro_call([{'(',_} | Args], Anno, {_, AnnoN, _}=N, Rest, As, Opt) ->
    {Open, Close} = parentheses(As),
    %% drop closing parenthesis
    {')', _} = lists:last(Args), %% assert
    Args1 = lists:droplast(Args),
    %% note that we must scan the argument list; it may not be skipped
    scan_macros_1(Args1 ++ [{'}', AnnoN} | Close],
                  Rest,
                  %% {'?macro_call', N, Arg1, ... }
                  lists:reverse(Open ++ [{'{', Anno},
                                         {atom, Anno, ?macro_call},
                                         {',', Anno},
                                         N,
                                         {',', AnnoN}],
                                As), Opt).

macro_atom(atom, A) ->
    list_to_atom(?atom_prefix ++ atom_to_list(A));
macro_atom(var, A) ->
    list_to_atom(?var_prefix ++ atom_to_list(A)).

%% don't insert parentheses after a string token, to avoid turning
%% `"string" ?macro' into a "function application" `"string"(...)'
%% (see note at top of file)
parentheses([{string, _, _} | _]) ->
    {[], []};
parentheses(_) ->
    {[{'(',0}], [{')',0}]}.

%% (after a macro has been found and the arglist skipped, if any)
scan_macros_1(Args, [{string, AnnoS, _} | _]=Rest, As,
	      #opt{clever = true}=Opt) ->
    %% string literal following macro: be clever and insert ++
    scan_macros(Args ++ [{'++', AnnoS} | Rest],  As, Opt);
scan_macros_1(Args, Rest, As, Opt) ->
    %% normal case - continue scanning
    scan_macros(Args ++ Rest, As, Opt).

rewrite_form({function, Anno, ?pp_form, _,
              [{clause, _, [], [], [{call, _, A, As}]}]}) ->
    erl_syntax:set_pos(erl_syntax:attribute(A, rewrite_list(As)), Anno);
rewrite_form({function, Anno, ?pp_form, _, [{clause, _, [], [], [A]}]}) ->
    erl_syntax:set_pos(erl_syntax:attribute(A), Anno);
rewrite_form(T) ->
    rewrite(T).

rewrite_list([T | Ts]) ->
    [rewrite(T) | rewrite_list(Ts)];
rewrite_list([]) ->
    [].

%% Note: as soon as we start using erl_syntax:subtrees/1 and similar
%% functions, we cannot assume that we know the exact representation of
%% the syntax tree anymore - we must use erl_syntax functions to analyze
%% and decompose the data.

rewrite(Node) ->
    case erl_syntax:type(Node) of
	atom ->
	    case atom_to_list(erl_syntax:atom_value(Node)) of
		?atom_prefix ++ As ->
		    A1 = list_to_atom(As),
		    N = erl_syntax:copy_pos(Node, erl_syntax:atom(A1)),
		    erl_syntax:copy_pos(Node, erl_syntax:macro(N));
		?var_prefix ++ As ->
		    A1 = list_to_atom(As),
		    N = erl_syntax:copy_pos(Node, erl_syntax:variable(A1)),
		    erl_syntax:copy_pos(Node, erl_syntax:macro(N));
		_ ->
		    Node
	    end;
        tuple ->
            case erl_syntax:tuple_elements(Node) of
                [MagicWord, A | As] ->
                    case erl_syntax:type(MagicWord) of
                        atom ->
                            case erl_syntax:atom_value(MagicWord) of
                                ?macro_call ->
                                    M = erl_syntax:macro(A, rewrite_list(As)),
                                    erl_syntax:copy_pos(Node, M);
                                _ ->
                                    rewrite_1(Node)
                            end;
                        _ ->
                            rewrite_1(Node)
                    end;
                _ ->
                    rewrite_1(Node)
            end;
	_ ->
	    rewrite_1(Node)
    end.

rewrite_1(Node) ->
    case erl_syntax:subtrees(Node) of
	[] ->
	    Node;
	Gs ->
	    Node1 = erl_syntax:make_tree(erl_syntax:type(Node),
					 [[rewrite(T) || T <- Ts]
					  || Ts <- Gs]),
	    erl_syntax:copy_pos(Node, Node1)
    end.

%% attempting a rescue operation on a token sequence for a single form
%% if it could not be parsed after the normal treatment

fix_form([{atom, _, ?pp_form}, {'(', _}, {')', _}, {'->', _},
	  {atom, _, define}, {'(', _} | _]=Ts) ->
    case lists:reverse(Ts) of
	[{dot, _}, {')', _} | _] ->
	    {retry, Ts, fun fix_define/1};
	[{dot, Anno} | Ts1] ->
	    Ts2 = lists:reverse([{dot, Anno}, {')', Anno} | Ts1]),
	    {retry, Ts2, fun fix_define/1};
	_ ->
	    error
    end;
fix_form(_Ts) ->
    error.

fix_define([{atom, Anno, ?pp_form}, {'(', _}, {')', _}, {'->', _},
	    {atom, AnnoA, define}, {'(', _}, N, {',', _} | Ts]) ->
    [{dot, _}, {')', _} | Ts1] = lists:reverse(Ts),
    S = tokens_to_string(lists:reverse(Ts1)),
    A = erl_syntax:set_pos(erl_syntax:atom(define), AnnoA),
    Txt = erl_syntax:set_pos(erl_syntax:text(S), AnnoA),
    {form, erl_syntax:set_pos(erl_syntax:attribute(A, [N, Txt]), Anno)};
fix_define(_Ts) ->
    error.

-doc """
Generates a string corresponding to the given token sequence.

The string can be re-tokenized to yield the same token list again.
""".
-spec tokens_to_string([term()]) -> string().

tokens_to_string([{atom,_,A} | Ts]) ->
    io_lib:write_atom(A) ++ " " ++ tokens_to_string(Ts);
tokens_to_string([{string, _, S} | Ts]) ->
    io_lib:write_string(S) ++ " " ++ tokens_to_string(Ts);
tokens_to_string([{char, _, C} | Ts]) ->
    io_lib:write_char(C) ++ " " ++ tokens_to_string(Ts);
tokens_to_string([{float, _, F} | Ts]) ->
    float_to_list(F) ++ " " ++ tokens_to_string(Ts);
tokens_to_string([{integer, _, N} | Ts]) ->
    integer_to_list(N) ++ " " ++ tokens_to_string(Ts);
tokens_to_string([{var, _, A} | Ts]) ->
    atom_to_list(A) ++ " " ++ tokens_to_string(Ts);
tokens_to_string([{dot, _} | Ts]) ->
    ".\n" ++ tokens_to_string(Ts);
tokens_to_string([{A, _} | Ts]) ->
    atom_to_list(A) ++ " " ++ tokens_to_string(Ts);
tokens_to_string([]) ->
    "".


-doc false.
-spec format_error(term()) -> string().

format_error(macro_args) ->
    errormsg("macro call missing end parenthesis");
format_error({error, Error}) ->
    Error;
format_error({warning, Error}) ->
    Error;
format_error({unknown, Reason}) ->
    errormsg(io_lib:format("unknown error: ~tP", [Reason, 15])).

errormsg(String) ->
    io_lib:format("~s: ~ts", [?MODULE, String]).


%% =====================================================================

%% See #7266: The dodger currently does not process feature attributes
%% correctly, so temporarily consider the `else` and `maybe` atoms
%% always as keywords
-spec reserved_word(Atom :: atom()) -> boolean().
reserved_word('else') -> true;
reserved_word('maybe') -> true;
reserved_word(Atom) -> erl_scan:f_reserved_word(Atom).
