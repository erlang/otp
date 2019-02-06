%% =====================================================================
%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
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
%% @copyright 2001-2006 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @end
%% =====================================================================

%% @doc `epp_dodger' - bypasses the Erlang preprocessor.
%%
%% <p>This module tokenises and parses most Erlang source code without
%% expanding preprocessor directives and macro applications, as long as
%% these are syntactically "well-behaved". Because the normal parse
%% trees of the `erl_parse' module cannot represent these things
%% (normally, they are expanded by the Erlang preprocessor {@link
%% //stdlib/epp} before the parser sees them), an extended syntax tree
%% is created, using the {@link erl_syntax} module.</p>


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
%% Sadly, the parsing sometimes discards even the line number info from
%% atom tokens, so we can only use the actual characters for this.
%%
%% * We recognize `?m(...' at the start of a form and prevent this from
%% being interpreted as a macro with arguments, since it is probably a
%% function definition. Likewise with attributes `-?m(...'.

-module(epp_dodger).

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


%% @type errorinfo() = {ErrorLine::integer(),
%%                      Module::atom(),
%%                      Descriptor::term()}.
%% 
%% This is a so-called Erlang I/O ErrorInfo structure; see the {@link
%% //stdlib/io} module for details.

-type errorinfo() :: {integer(), atom(), term()}.

-type option() :: atom() | {atom(), term()}.

%% =====================================================================
%% @spec parse_file(File) -> {ok, Forms} | {error, errorinfo()}
%%       File = file:filename()
%%       Forms = [erl_syntax:syntaxTree()]
%% 
%% @equiv parse_file(File, [])

-spec parse_file(file:filename()) ->
        {'ok', erl_syntax:forms()} | {'error', errorinfo()}.

parse_file(File) ->
    parse_file(File, []).

%% @spec parse_file(File, Options) -> {ok, Forms} | {error, errorinfo()}
%%       File = file:filename()
%%       Options = [term()]
%%       Forms = [erl_syntax:syntaxTree()]
%% 
%% @doc Reads and parses a file. If successful, `{ok, Forms}'
%% is returned, where `Forms' is a list of abstract syntax
%% trees representing the "program forms" of the file (cf.
%% `erl_syntax:is_form/1'). Otherwise, `{error, errorinfo()}' is
%% returned, typically if the file could not be opened. Note that
%% parse errors show up as error markers in the returned list of
%% forms; they do not cause this function to fail or return
%% `{error, errorinfo()}'.
%%
%% Options:
%% <dl>
%%   <dt>{@type {no_fail, boolean()@}}</dt>
%%   <dd>If `true', this makes `epp_dodger' replace any program forms
%%   that could not be parsed with nodes of type `text' (see {@link
%%   erl_syntax:text/1}), representing the raw token sequence of the
%%   form, instead of reporting a parse error. The default value is
%%   `false'.</dd>
%%   <dt>{@type {clever, boolean()@}}</dt>
%%   <dd>If set to `true', this makes `epp_dodger' try to repair the
%%   source code as it seems fit, in certain cases where parsing would
%%   otherwise fail. Currently, it inserts `++'-operators between string
%%   literals and macros where it looks like concatenation was intended.
%%   The default value is `false'.</dd>
%% </dl>
%%
%% @see parse/2
%% @see quick_parse_file/1
%% @see erl_syntax:is_form/1

-spec parse_file(file:filename(), [option()]) ->
        {'ok', erl_syntax:forms()} | {'error', errorinfo()}.

parse_file(File, Options) ->
    parse_file(File, fun parse/3, Options).

%% @spec quick_parse_file(File) -> {ok, Forms} | {error, errorinfo()}
%%       File = file:filename()
%%       Forms = [erl_syntax:syntaxTree()]
%%
%% @equiv quick_parse_file(File, [])

-spec quick_parse_file(file:filename()) ->
        {'ok', erl_syntax:forms()} | {'error', errorinfo()}.

quick_parse_file(File) ->
    quick_parse_file(File, []).

%% @spec quick_parse_file(File, Options) ->
%%           {ok, Forms} | {error, errorinfo()}
%%       File = file:filename()
%%       Options = [term()]
%%       Forms = [erl_syntax:syntaxTree()]
%%
%% @doc Similar to {@link parse_file/2}, but does a more quick-and-dirty
%% processing of the code. Macro definitions and other preprocessor
%% directives are discarded, and all macro calls are replaced with
%% atoms. This is useful when only the main structure of the code is of
%% interest, and not the details. Furthermore, the quick-parse method
%% can usually handle more strange cases than the normal, more exact
%% parsing.
%%
%% Options: see {@link parse_file/2}. Note however that for
%% `quick_parse_file/2', the option `no_fail' is `true' by default.
%%
%% @see quick_parse/2
%% @see parse_file/2

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
	{error, {_Line, file_io_server, invalid_unicode}} ->
	    invalid_unicode;
	_Other ->
	    find_invalid_unicode(T)
    end;
find_invalid_unicode([]) -> none.

%% =====================================================================
%% @spec parse(IODevice) -> {ok, Forms} | {error, errorinfo()}
%% @equiv parse(IODevice, 1)

-spec parse(file:io_device()) -> {'ok', erl_syntax:forms()}.

parse(Dev) ->
    parse(Dev, 1).

%% @spec parse(IODevice, StartLine) -> {ok, Forms} | {error, errorinfo()}
%%       IODevice = pid()
%%       StartLine = integer()
%%       Forms = [erl_syntax:syntaxTree()]
%%
%% @equiv parse(IODevice, StartLine, [])
%% @see parse/1

-spec parse(file:io_device(), integer()) -> {'ok', erl_syntax:forms()}.

parse(Dev, L) ->
    parse(Dev, L, []).

%% @spec parse(IODevice, StartLine, Options) ->
%%           {ok, Forms} | {error, errorinfo()}
%%       IODevice = pid()
%%       StartLine = integer()
%%       Options = [term()]
%%       Forms = [erl_syntax:syntaxTree()]
%%
%% @doc Reads and parses program text from an I/O stream. Characters are
%% read from `IODevice' until end-of-file; apart from this, the
%% behaviour is the same as for {@link parse_file/2}. `StartLine' is the
%% initial line number, which should be a positive integer.
%%
%% @see parse/2
%% @see parse_file/2
%% @see parse_form/2
%% @see quick_parse/3

-spec parse(file:io_device(), integer(), [option()]) ->
        {'ok', erl_syntax:forms()}.

parse(Dev, L0, Options) ->
    parse(Dev, L0, fun parse_form/3, Options).

%% @spec quick_parse(IODevice) -> {ok, Forms} | {error, errorinfo()}
%% @equiv quick_parse(IODevice, 1)

-spec quick_parse(file:io_device()) ->
        {'ok', erl_syntax:forms()}.

quick_parse(Dev) ->
    quick_parse(Dev, 1).

%% @spec quick_parse(IODevice, StartLine) ->
%%           {ok, Forms} | {error, errorinfo()}
%%       IODevice = pid()
%%       StartLine = integer()
%%       Forms = [erl_syntax:syntaxTree()]
%%
%% @equiv quick_parse(IODevice, StartLine, [])
%% @see quick_parse/1

-spec quick_parse(file:io_device(), integer()) ->
        {'ok', erl_syntax:forms()}.

quick_parse(Dev, L) ->
    quick_parse(Dev, L, []).

%% @spec (IODevice, StartLine, Options) ->
%%           {ok, Forms} | {error, errorinfo()}
%%       IODevice = pid()
%%       StartLine = integer()
%%       Options = [term()]
%%       Forms = [erl_syntax:syntaxTree()]
%% 
%% @doc Similar to {@link parse/3}, but does a more quick-and-dirty
%% processing of the code. See {@link quick_parse_file/2} for details.
%%
%% @see quick_parse/2
%% @see quick_parse_file/2
%% @see quick_parse_form/2
%% @see parse/3

-spec quick_parse(file:io_device(), integer(), [option()]) ->
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


%% =====================================================================
%% @spec parse_form(IODevice, StartLine) -> {ok, Form, LineNo}
%%                                        | {eof, LineNo}
%%                                        | {error, errorinfo(), LineNo}
%%       IODevice = pid()
%%       StartLine = integer()
%%       Form = erl_syntax:syntaxTree()
%%       LineNo = integer()
%%
%% @equiv parse_form(IODevice, StartLine, [])
%%
%% @see quick_parse_form/2

-spec parse_form(file:io_device(), integer()) ->
        {'ok', erl_syntax:forms(), integer()}
      | {'eof', integer()} | {'error', errorinfo(), integer()}.

parse_form(Dev, L0) ->
    parse_form(Dev, L0, []).

%% @spec parse_form(IODevice, StartLine, Options) ->
%%           {ok, Form, LineNo}
%%         | {eof, LineNo}
%%         | {error, errorinfo(), LineNo}
%% 
%%       IODevice = pid()
%%       StartLine = integer()
%%       Options = [term()]
%%       Form = erl_syntax:syntaxTree()
%%       LineNo = integer()
%%
%% @doc Reads and parses a single program form from an I/O stream.
%% Characters are read from `IODevice' until an end-of-form
%% marker is found (a period character followed by whitespace), or until
%% end-of-file; apart from this, the behaviour is similar to that of
%% `parse/3', except that the return values also contain the
%% final line number given that `StartLine' is the initial
%% line number, and that `{eof, LineNo}' may be returned.
%%
%% @see parse/3
%% @see parse_form/2
%% @see quick_parse_form/3

-spec parse_form(file:io_device(), integer(), [option()]) ->
        {'ok', erl_syntax:forms(), integer()}
      | {'eof', integer()} | {'error', errorinfo(), integer()}.

parse_form(Dev, L0, Options) ->
    parse_form(Dev, L0, fun normal_parser/2, Options).

%% @spec quick_parse_form(IODevice, StartLine) ->
%%           {ok, Form, LineNo}
%%         | {eof, LineNo}
%%         | {error, errorinfo(), LineNo}
%%       IODevice = pid()
%%       StartLine = integer()
%%       Form = erl_syntax:syntaxTree() | none
%%       LineNo = integer()
%%
%% @equiv quick_parse_form(IODevice, StartLine, [])
%%
%% @see parse_form/2

-spec quick_parse_form(file:io_device(), integer()) ->
        {'ok', erl_syntax:forms(), integer()}
      | {'eof', integer()} | {'error', errorinfo(), integer()}.

quick_parse_form(Dev, L0) ->
    quick_parse_form(Dev, L0, []).

%% @spec quick_parse_form(IODevice, StartLine, Options) ->
%%           {ok, Form, LineNo}
%%         | {eof, LineNo}
%%         | {error, errorinfo(), LineNo}
%%
%%       IODevice = pid()
%%       StartLine = integer()
%%       Options = [term()]
%%       Form = erl_syntax:syntaxTree()
%%       LineNo = integer()
%%
%% @doc Similar to {@link parse_form/3}, but does a more quick-and-dirty
%% processing of the code. See {@link quick_parse_file/2} for details.
%%
%% @see parse/3
%% @see quick_parse_form/2
%% @see parse_form/3

-spec quick_parse_form(file:io_device(), integer(), [option()]) ->
        {'ok', erl_syntax:forms(), integer()}
      | {'eof', integer()} | {'error', errorinfo(), integer()}.

quick_parse_form(Dev, L0, Options) ->
    parse_form(Dev, L0, fun quick_parser/2, Options).

-record(opt, {clever = false :: boolean()}).

parse_form(Dev, L0, Parser, Options) ->
    NoFail = proplists:get_bool(no_fail, Options),
    Opt = #opt{clever = proplists:get_bool(clever, Options)},
    case io:scan_erl_form(Dev, "", L0) of
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
			   start_pos(Ts, L1)),
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
    erl_anno:line(element(2, T));
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

quickscan_form([{'-', _L}, {atom, La, define} | _Ts]) ->
    kill_form(La);
quickscan_form([{'-', _L}, {atom, La, undef} | _Ts]) ->
    kill_form(La);
quickscan_form([{'-', _L}, {atom, La, include} | _Ts]) ->
    kill_form(La);
quickscan_form([{'-', _L}, {atom, La, include_lib} | _Ts]) ->
    kill_form(La);
quickscan_form([{'-', _L}, {atom, La, ifdef} | _Ts]) ->
    kill_form(La);
quickscan_form([{'-', _L}, {atom, La, ifndef} | _Ts]) ->
    kill_form(La);
quickscan_form([{'-', _L}, {'if', La} | _Ts]) ->
    kill_form(La);
quickscan_form([{'-', _L}, {atom, La, elif} | _Ts]) ->
    kill_form(La);
quickscan_form([{'-', _L}, {atom, La, else} | _Ts]) ->
    kill_form(La);
quickscan_form([{'-', _L}, {atom, La, endif} | _Ts]) ->
    kill_form(La);
quickscan_form([{'-', L}, {'?', _}, {Type, _, _}=N | [{'(', _} | _]=Ts])
  when Type =:= atom; Type =:= var ->
    %% minus, macro and open parenthesis at start of form - assume that
    %% the macro takes no arguments; e.g. `-?foo(...).'
    quickscan_macros_1(N, Ts, [{'-', L}]);
quickscan_form([{'?', _L}, {Type, _, _}=N | [{'(', _} | _]=Ts])
  when Type =:= atom; Type =:= var ->
    %% macro and open parenthesis at start of form - assume that the
    %% macro takes no arguments (see scan_macros for details)
    quickscan_macros_1(N, Ts, []);
quickscan_form(Ts) ->
    quickscan_macros(Ts).

kill_form(L) ->
    [{atom, L, ?pp_form}, {'(', L}, {')', L}, {'->', L}, {atom, L, kill},
     {dot, L}].

quickscan_macros(Ts) ->
    quickscan_macros(Ts, []).

quickscan_macros([{'?',_}, {Type, _, A} | Ts], [{string, L, S} | As])
  when Type =:= atom; Type =:= var ->
    %% macro after a string literal: change to a single string
    {_, Ts1} = skip_macro_args(Ts),
    S1 = S ++ quick_macro_string(A),
    quickscan_macros(Ts1, [{string, L, S1} | As]);
quickscan_macros([{'?',_}, {Type, _, _}=N | [{'(',_}|_]=Ts],
		 [{':',_}|_]=As)
  when Type =:= atom; Type =:= var ->
    %% macro and open parenthesis after colon - check the token
    %% following the arguments (see scan_macros for details)
    Ts1 = case skip_macro_args(Ts) of
	      {_, [{'->',_} | _] = Ts2} -> Ts2;
	      {_, [{'when',_} | _] = Ts2} -> Ts2;
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
quickscan_macros_1({_Type, _, A}, [{string, L, S} | Ts], As) ->
    %% string literal following macro: change to single string
    S1 = quick_macro_string(A) ++ S,
    quickscan_macros(Ts, [{string, L, S1} | As]);
quickscan_macros_1({_Type, L, A}, Ts, As) ->
    %% normal case - just replace the macro with an atom
    quickscan_macros(Ts, [{atom, L, quick_macro_atom(A)} | As]).

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
skip_macro_args([{'cond',_}=T | Ts], Es, As) ->
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

scan_form([{'-', _L}, {atom, La, define} | Ts], Opt) ->
    [{atom, La, ?pp_form}, {'(', La}, {')', La}, {'->', La},
     {atom, La, define} | scan_macros(Ts, Opt)];
scan_form([{'-', _L}, {atom, La, undef} | Ts], Opt) ->
    [{atom, La, ?pp_form}, {'(', La}, {')', La}, {'->', La},
     {atom, La, undef} | scan_macros(Ts, Opt)];
scan_form([{'-', _L}, {atom, La, include} | Ts], Opt) ->
    [{atom, La, ?pp_form}, {'(', La}, {')', La}, {'->', La},
     {atom, La, include} | scan_macros(Ts, Opt)];
scan_form([{'-', _L}, {atom, La, include_lib} | Ts], Opt) ->
    [{atom, La, ?pp_form}, {'(', La}, {')', La}, {'->', La},
     {atom, La, include_lib} | scan_macros(Ts, Opt)];
scan_form([{'-', _L}, {atom, La, ifdef} | Ts], Opt) ->
    [{atom, La, ?pp_form}, {'(', La}, {')', La}, {'->', La},
     {atom, La, ifdef} | scan_macros(Ts, Opt)];
scan_form([{'-', _L}, {atom, La, ifndef} | Ts], Opt) ->
    [{atom, La, ?pp_form}, {'(', La}, {')', La}, {'->', La},
     {atom, La, ifndef} | scan_macros(Ts, Opt)];
scan_form([{'-', _L}, {'if', La} | Ts], Opt) ->
    [{atom, La, ?pp_form}, {'(', La}, {')', La}, {'->', La},
     {atom, La, 'if'} | scan_macros(Ts, Opt)];
scan_form([{'-', _L}, {atom, La, elif} | Ts], Opt) ->
    [{atom, La, ?pp_form}, {'(', La}, {')', La}, {'->', La},
     {atom, La, 'elif'} | scan_macros(Ts, Opt)];
scan_form([{'-', _L}, {atom, La, else} | Ts], Opt) ->
    [{atom, La, ?pp_form}, {'(', La}, {')', La}, {'->', La},
     {atom, La, else} | scan_macros(Ts, Opt)];
scan_form([{'-', _L}, {atom, La, endif} | Ts], Opt) ->
    [{atom, La, ?pp_form}, {'(', La}, {')', La}, {'->', La},
     {atom, La, endif} | scan_macros(Ts, Opt)];
scan_form([{'-', _L}, {atom, La, error} | Ts], _Opt) ->
    Desc = build_info_string("-error", Ts),
    ErrorInfo = {La, ?MODULE, {error, Desc}},
    erl_syntax:error_marker(ErrorInfo);
scan_form([{'-', _L}, {atom, La, warning} | Ts], _Opt) ->
    Desc = build_info_string("-warning", Ts),
    ErrorInfo = {La, ?MODULE, {warning, Desc}},
    erl_syntax:error_marker(ErrorInfo);
scan_form([{'-', L}, {'?', L1}, {Type, _, _}=N | [{'(', _} | _]=Ts], Opt)
  when Type =:= atom; Type =:= var ->
    %% minus, macro and open parenthesis at start of form - assume that
    %% the macro takes no arguments; e.g. `-?foo(...).'
    macro(L1, N, Ts, [{'-', L}], Opt);
scan_form([{'?', L}, {Type, _, _}=N | [{'(', _} | _]=Ts], Opt)
  when Type =:= atom; Type =:= var ->
    %% macro and open parenthesis at start of form - assume that the
    %% macro takes no arguments; probably a function declaration on the
    %% form `?m(...) -> ...', which will not parse if it is rewritten as
    %% `(?m(...)) -> ...', so it must be handled as `(?m)(...) -> ...'
    macro(L, N, Ts, [], Opt);
scan_form(Ts, Opt) ->
    scan_macros(Ts, Opt).

build_info_string(Prefix, Ts0) ->
    Ts = lists:droplast(Ts0),
    String = lists:droplast(tokens_to_string(Ts)),
    Prefix ++ " " ++ String ++ ".".

scan_macros(Ts, Opt) ->
    scan_macros(Ts, [], Opt).

scan_macros([{'?', _}=M, {Type, _, _}=N | Ts], [{string, L, _}=S | As],
 	    #opt{clever = true}=Opt)
  when Type =:= atom; Type =:= var ->
    %% macro after a string literal: be clever and insert ++
    scan_macros([M, N | Ts], [{'++', L}, S | As], Opt);
scan_macros([{'?', L}, {Type, _, _}=N | [{'(',_}|_]=Ts],
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
	    macro_call(Args, L, N, Rest, As, Opt);
	[{'when',_} | _] ->
	    macro_call(Args, L, N, Rest, As, Opt);
	_ ->
	    macro(L, N, Ts, As, Opt)
    end;
scan_macros([{'?', L}, {Type, _, _}=N | [{'(',_}|_]=Ts], As, Opt)
  when Type =:= atom; Type =:= var ->
    %% macro with arguments
    {Args, Rest} = skip_macro_args(Ts),
    macro_call(Args, L, N, Rest, As, Opt);
scan_macros([{'?', L }, {Type, _, _}=N | Ts], As, Opt)
  when Type =:= atom; Type =:= var ->
    %% macro without arguments
    macro(L, N, Ts, As, Opt);
scan_macros([T | Ts], As, Opt) ->
    scan_macros(Ts, [T | As], Opt);
scan_macros([], As, _Opt) ->
    lists:reverse(As).

%% Rewriting to a call which will be recognized by the post-parse pass
%% (we insert parentheses to preserve the precedences when parsing).

macro(L, {Type, _, A}, Rest, As, Opt) ->
    scan_macros_1([], Rest, [{atom,L,macro_atom(Type,A)} | As], Opt).

macro_call([{'(',_}, {')',_}], L, {_, Ln, _}=N, Rest, As, Opt) ->
    {Open, Close} = parentheses(As),
    scan_macros_1([], Rest,
		  lists:reverse(Open ++ [{atom,L,?macro_call},
					 {'(',L}, N, {')',Ln}] ++ Close,
				As), Opt);
macro_call([{'(',_} | Args], L, {_, Ln, _}=N, Rest, As, Opt) ->
    {Open, Close} = parentheses(As),
    %% note that we must scan the argument list; it may not be skipped
    scan_macros_1(Args ++ Close,
		  Rest,
		  lists:reverse(Open ++ [{atom,L,?macro_call},
					 {'(',L}, N, {',',Ln}],
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
scan_macros_1(Args, [{string, L, _} | _]=Rest, As,
	      #opt{clever = true}=Opt) ->
    %% string literal following macro: be clever and insert ++
    scan_macros(Args ++ [{'++', L} | Rest],  As, Opt);
scan_macros_1(Args, Rest, As, Opt) ->
    %% normal case - continue scanning
    scan_macros(Args ++ Rest, As, Opt).

rewrite_form({function, L, ?pp_form, _,
              [{clause, _, [], [], [{call, _, A, As}]}]}) ->
    erl_syntax:set_pos(erl_syntax:attribute(A, rewrite_list(As)), L);
rewrite_form({function, L, ?pp_form, _, [{clause, _, [], [], [A]}]}) ->
    erl_syntax:set_pos(erl_syntax:attribute(A), L);
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
	application ->
	    F = erl_syntax:application_operator(Node),
	    case erl_syntax:type(F) of
		atom ->
		    case erl_syntax:atom_value(F) of
			?macro_call ->
			    [A | As] = erl_syntax:application_arguments(Node),
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
	[{dot, L} | Ts1] ->
	    Ts2 = lists:reverse([{dot, L}, {')', L} | Ts1]),
	    {retry, Ts2, fun fix_define/1};
	_ ->
	    error
    end;
fix_form(_Ts) ->
    error.

fix_define([{atom, L, ?pp_form}, {'(', _}, {')', _}, {'->', _},
	    {atom, La, define}, {'(', _}, N, {',', _} | Ts]) ->
    [{dot, _}, {')', _} | Ts1] = lists:reverse(Ts),
    S = tokens_to_string(lists:reverse(Ts1)),
    A = erl_syntax:set_pos(erl_syntax:atom(define), La),
    Txt = erl_syntax:set_pos(erl_syntax:text(S), La),
    {form, erl_syntax:set_pos(erl_syntax:attribute(A, [N, Txt]), L)};
fix_define(_Ts) ->
    error.

%% @spec tokens_to_string(Tokens::[term()]) -> string()
%% 
%% @doc Generates a string corresponding to the given token sequence.
%% The string can be re-tokenized to yield the same token list again.

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


%% @spec format_error(Descriptor::term()) -> string()
%% @hidden
%% @doc Callback function for formatting error descriptors. Not for
%% normal use.

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
