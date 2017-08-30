%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
%%

-module(ic_error).

-include_lib("ic/src/ic.hrl").
-include_lib("ic/src/ic_debug.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([error/2, 
	 fatal_error/2, 
	 init_errors/1, 
	 return/1, 
	 warn/2, 
	 get_error_count/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------

%%--------------------------------------------------------------------
%%
%% Error and warning utilities.
%%
%% Note that errors are somewhat brutal and that warnings are kept in
%% a list for the user to extract at a later stage. The handling of
%% warnings is entirely up to the user while handling of errors is
%% never left to the user.
%%
%%--------------------------------------------------------------------

return(G) ->
    case ic_options:get_opt(G, silent2) of
	true ->
	    case get_error_count(G) of
		0 -> {ok, get_list(G, warn_list)};
		_X -> {error, get_list(G, warn_list), get_list(G, error_list)}
	    end;
	false ->
	    case get_error_count(G) of
		0 -> ok;
		X -> print_error(G, {error, g, ic_genobj:idlfile(G), {error_count, X}}),
		     error
	    end
    end.


get_list(G, ListName) ->
    ?lookup(G#genobj.options, ListName).


%% Public function for reporting an error
error(G, Err) ->
    Error = {error, g, ic_genobj:idlfile(G), Err},
    case insert_in_list(G, Error, error_list) of
	new ->
	    print_error(G, Error),
	    MaxErrs = ic_options:get_opt(G, maxerrs),
	    case incr_counter(G, error_count) of
		X when X >= MaxErrs ->
		    fatal_error(G, {error_count_exceeded, X});
		_ -> Error
	    end;
	old -> 
	    Error
    end.

%% Public function for reporting an error. NOTE: also stops execution
fatal_error(G, Err) ->
    Error = {error, g, ic_genobj:idlfile(G), Err},
    insert_in_list(G, Error, error_list),
    incr_counter(G, error_count),
    print_error(G, Error),
    throw(Error).


%% Public function for reporting a warning
warn(G, Warn) ->
    Warning = {warn, g, ic_genobj:idlfile(G), Warn},
    case insert_in_list(G, Warning, warn_list) of
	new ->
	    print_warn(G, Warning),
	    MaxErrs = ic_options:get_opt(G, maxwarns),
	    case incr_counter(G, warn_count) of
		X when X >= MaxErrs ->
		    fatal_error(G, {warn_count_exceeded, X});
		_ -> ok
	    end;
	old -> ok
end.


%% Initialisation of all counters and lists associated with errors and
%% warnings.
init_errors(G) ->
    reset_counter(G, error_count),
    reset_counter(G, warn_count),
    reset_list(G, error_list),
    reset_list(G, warn_list),
    ok.



%%--------------------------------------------------------------------
%% Counter and list (warn and error) handling
%%

incr_counter(G, Counter) ->
    Num = ?lookup(G#genobj.options, Counter) + 1,
    ?insert(G#genobj.options, Counter, Num),
    Num.

reset_counter(G, Counter) ->
    ?insert(G#genobj.options, Counter, 0).

get_error_count(G) ->
    ?lookup(G#genobj.options, error_count).

reset_list(G, ListName) ->
    ?insert(G#genobj.options, ListName, []).

insert_in_list(G, Item, ListName) ->
    List = ?lookup(G#genobj.options, ListName),
    case lists:member(Item, List) of
	true -> old;
	false ->
	    ?insert(G#genobj.options, ListName, [Item| List]),
	    new
    end.


%%--------------------------------------------------------------------
%% 
%% Nice printouts of errors and warnings
%%


%% Errors

print_error(G, Error) ->
    case {ic_options:get_opt(G, silent), ic_options:get_opt(G, silent2)} of
	{true, _} -> ok;
	{_, true} -> ok;
	_ -> format_error(Error)
    end,
    error.

format_error({error, _, File, {parse_error, Line, Args}}) ->
    Fmt = lists:foldl(fun(_, Acc) -> [$~, $s | Acc] end, [], Args),
    display(File, Line, Fmt, Args);
format_error({error, _, File, {error_count, X}}) ->
    display(File, "~p errors found", [X]);
format_error({error, _, File, {error_count_exceeded, X}}) ->
    display(File, "too many errors found (~p)", [X]);
format_error({error, _, File, {warn_count_exceeded, X}}) ->
    display(File, "too many warnings found (~p)", [X]);
format_error({error, _, File, {inherit_name_collision, 
			       {Orig, Item}, {Base, NewItem}}}) ->
    display(File, ic_forms:get_line(Item), "~s collides with ~s", 
	    [pp([ic_forms:get_id2(Item) | Orig]), pp([ic_forms:get_id2(NewItem) | Base])]);
format_error({error, _, File, {unsupported_op, {'~', Line}}}) ->
    display(File, Line, "unsupported unary operation ~~", []);
format_error({error, _, File, {multiply_defined, X}}) ->
    display(File, ic_forms:get_line(X), "multiple defined identifier ~p", [ic_forms:get_id2(X)]);
format_error({error, _, File, {illegal_spelling, X}}) ->
    display(File, ic_forms:get_line(X), 
%	    "illegal spelling of identifier ~s (capitalisation?)", 
	    "identifier ~p multiply declared - differs in case only",
	    [ic_forms:get_id2(X)]);
format_error({error, _, File, {illegal_enumerant_value, X}}) ->
    display(File, ic_forms:get_line(X), 
	    "Enumerant ~s's value collide by name with other type", 
	    [ic_forms:get_id2(X)]);
format_error({error, _, File, {illegal_forward, X}}) ->
    display(File, ic_forms:get_line(X), 
	    "cannot inherit from forwarded interface ~s", [ic_forms:get_id2(X)]);
format_error({error, _, File, {illegal_const_t, X, Type}}) ->
    display(File, ic_forms:get_line(X), 
	    "Illegal constant type ~s of ~s", [pp(Type), ic_forms:get_id2(X)]);
format_error({error, _, File, {multiple_cases, X}}) ->
    display(File, ic_forms:get_line(X), "multiple case values ~s", [pp(X)]);
format_error({error, _, File, {symtab_not_found, X}}) ->
    display(File, ic_forms:get_line(X), "undeclared identifier ~s", [ic_forms:get_id2(X)]);
format_error({error, _, File, {preproc, Lines}}) ->
    display(File, "preprocessor error: ~s", [hd(Lines)]);
format_error({error, _, File, {ic_pp_error, Lines}}) ->
    display(File, "preprocessor error: ~s", [Lines]);
format_error({error, _, File, {illegal_float, Line}}) ->
    display(File, Line, "illegal floating point number", []);
format_error({error, _, File, {bad_type_combination, E, V1, V2}}) ->
    display(File, ic_forms:get_line(E), "incompatible types, ~p and ~p", [V1, V2]);
format_error({error, _, File, {bad_oneway_type, X, _TK}}) ->
    display(File, ic_forms:get_line(X), "oneway operations must be declared void", []);
format_error({error, _, File, {inout_spec_for_c, X, Arg}}) ->
    display(File, ic_forms:get_line(X), "inout parameter ~s specified in native c mode",
	    [Arg]);
format_error({error, _, File, {sequence_not_defined, X, Arg}}) ->
    display(File, ic_forms:get_line(X), "sequence ~s not defined", [Arg]);
format_error({error, _, File, {illegal_typecode_for_c, Arg}}) ->
    display(File, not_specified, "illegal typecode ~s used in native c mode",
	    [Arg]);
format_error({error, _, File, {name_not_found, N}}) ->
    display(File, not_specified, "name ~s not found", [N]);
format_error({error, _, File, {illegal_typecode_for_c, Arg, N}}) ->
    display(File, not_specified, "illegal typecode ~p used for ~p in native c mode", [Arg, N]);
format_error({error, _, File, {oneway_outparams, X}}) ->
    display(File, ic_forms:get_line(X), 
	    "oneway operations may not have out or inout parameters", []);
format_error({error, _, File, {oneway_raises, X}}) ->
    display(File, ic_forms:get_line(X), "oneway operations may not raise exceptions",
	    []);
format_error({error, _, File, {bad_tk_match, T, TK, V}}) ->
    display(File, ic_forms:get_line(T),
	    "value ~p does not match declared type ~s", [V, pp(TK)]);
format_error({error, _, File, {bad_scope_enum_case, ScopedId}}) ->
    display(File, ic_forms:get_line(ScopedId),
	    "scoped enum identifiers not allowed as case (~s)", 
	    [pp(ScopedId)]);
format_error({error, _, File, {bad_type, Expr, Op, _TypeList, V}}) ->
    display(File, ic_forms:get_line(Expr),
	    "parameter value ~p to ~s is of illegal type", [V, pp(Op)]);
format_error({error, _, File, {bad_case_type, TK, X, Val}}) ->
    display(File, ic_forms:get_line(X), 
	    "case value ~s does not match discriminator type ~s", 
	    [case_pp(X, Val), pp(TK)]);
format_error({error, _, File, {tk_not_found, X}}) ->
    display(File, ic_forms:get_line(X), "undeclared identifier ~s", [pp(X)]);
%%% New format_errors
format_error({error, _, File, {bad_fixed, Format, Args, Line}}) ->
    display(File, Line, Format, Args);
format_error({error, _, File, {illegal_switch_t, Arg, _N}}) ->
    display(File, ic_forms:get_line(Arg), "illegal switch", []);  
format_error({error, _, File, {inherit_resolve, Arg, N}}) ->
    display(File, ic_forms:get_line(Arg), "cannot resolve ~s", [N]);
format_error({error, _, File, {bad_escape_character, Line, Char}}) ->
    display(File, Line, "bad escape character \"~c\"", [Char]);  
format_error({error, _, File, {pragma_code_opt_bad_option_list, Line}}) ->
    display(File, Line, "bad option list on pragma \"CODEOPT\"", []);
format_error({error, _, File, {bad_string, Line}}) ->
    display(File, Line, "bad string", []);
format_error({error, _, File, {create_dir, Path, Reason}}) ->
    display(File, not_specified, "couldn't create directory ~p due to ~p", [Path, Reason]);
format_error({error, _, File, {open_file, Path, Reason}}) ->
    display(File, not_specified, "couldn't open ~p due to ~p", [Path, Reason]);
format_error({error, _, File, {plain_error_string, ErrString}}) ->
    display(File, not_specified, "~s", [ErrString]);
format_error({error, _, File, {plain_error_string, T, ErrString}}) ->
    display(File, ic_forms:get_line(T), "~s", [ErrString]);
format_error({error, _, File, {ErrString, Line}}) ->
    display(File, Line, ErrString, []).


%% Warnings
print_warn(G, Warn) ->
    case {ic_options:get_opt(G, silent), ic_options:get_opt(G, silent2)} of
	{true, _} -> ok;
	{_, true} -> ok;
	_ -> format_warn(Warn)
    end.

format_warn({warn, _, File, {ic_pp_warning, Lines}}) ->
    display(File, "preprocessor warning: ~s", [Lines]);
format_warn({warn, _, _File, {cfg_open, _Reason, File}}) ->
    display(File, "warning: could not open file: ~p", [File]);
format_warn({warn, _, _File, {cfg_read, File}}) ->
    display(File, "warning: syntax error in configuration file", []);
format_warn({warn, _, File, {multi_modules, Id}}) ->
    display(File, ic_forms:get_line(Id), "warning: multiple modules in file", []);
format_warn({warn, _, File, {illegal_opt, Opt}}) ->
    display(File, "warning: unrecognised option: ~p", [Opt]);
format_warn({warn, _, File, {nested_mod, Id}}) ->
    display(File, ic_forms:get_line(Id), "warning: nested module: ~s", [ic_forms:get_id(Id)]);
format_warn({warn, _, File, {inherit_name_shadow, {Orig, Item}, 
			     {Base, NewItem}}}) ->
    display(File, ic_forms:get_line(Item), 
	    "warning: ~s shadows ~s", [pp([ic_forms:get_id2(Item) | Orig]),
				       pp([ic_forms:get_id2(NewItem) | Base])]);
format_warn({warn, _, File, {internal_307, X, Y}}) ->
    %% If global Scope variable is not [] at top level constant
    display(File, ic_forms:get_line(X), "warning: internal 307: ~p ~p", [X, Y]);
format_warn({warn, _, File, {WarnString, Line}}) ->
    display(File, Line, WarnString, []).

%% Display an error or warning
display(File, not_specified, F, A) ->
    io:format("~p : ~s~n", [File, io_lib:format(F, A)]);
display(File, Line, F, A) ->
    io:format("~p on line ~p: ~s~n", [File, Line, io_lib:format(F, A)]).
display(File, F, A) ->
    io:format("~p: ~s~n", [File, io_lib:format(F, A)]).



%%format_warn2(G, WarnStr) ->
%%    case {ic_options:get_opt(G, silent), ic_options:get_opt(G, silent2), 
%%          ic_options:get_opt(G, nowarn)} of
%%	{false, false, false} ->
%%	    io:format("~p: warning: ~s~n", [ic_genobj:idlfile(G), WarnStr]);
%%	_ -> ok
%%    end.

%%format_warn2(G, Line, WarnStr) ->
%%    case {ic_options:get_opt(G, silent), ic_options:get_opt(G, silent2),
%%          ic_options:get_opt(G, nowarn)} of
%%	{false, false, false} ->
%%	    io:format("~p on line ~p: warning: ~s~n", 
%%		      [ic_genobj:idlfile(G), Line, WarnStr]);
%%	_ -> ok
%%    end.




%% pretty print various stuff

pp({tk_string, _}) -> "string";
pp({tk_wstring, _}) -> "wstring";
pp(tk_long) -> "long";
pp(tk_short) -> "short";
pp(tk_ushort) -> "unsigned short";
pp(tk_ulong) -> "unsigned long";
pp(tk_float) -> "float";
pp(tk_double) -> "double";
pp(tk_boolean) -> "boolean";
pp(tk_char) -> "char";
pp(tk_wchar) -> "wchar";
pp(tk_octet) -> "octet";
pp(tk_null) -> "null";
pp(tk_void) -> "void";
pp(tk_any) -> "any";
pp({tk_fixed, _, _}) -> "fixed";
pp({tk_objref, _, _}) -> "object reference";
pp(rshift) -> ">>";
pp(lshift) -> "<<";
pp(X) when element(1, X) == tk_enum -> "enum";
pp(X) when is_record(X, scoped_id) -> ic_util:to_colon(X);
pp(X) when element(1, X) == '<identifier>' -> ic_forms:get_id(X);
pp(X) when is_list(X) andalso is_list(hd(X)) -> ic_util:to_colon(X);
pp({_, Num, Beef}) when is_integer(Num) -> Beef;
pp({Beef, Num}) when is_integer(Num) -> ic_util:to_list(Beef);
pp(X) -> ic_util:to_list(X).

%% special treatment of case label names
case_pp(X, _Val) when is_record(X, scoped_id) -> pp(X);
case_pp(_X, Val) -> pp(Val).



%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
