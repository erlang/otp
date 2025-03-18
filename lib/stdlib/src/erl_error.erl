%%
%% %CopyrightBegin%
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
-module(erl_error).
-moduledoc """
This module provides functions for pretty-printing errors and exceptions. It is
used by both the `m:shell` and by `m:proc_lib` to print exceptions.

It is possible for the module raising an error to provide additional information
by calling [`error/3`](`erlang:error/3`) with extra error information. More
details about this mechanism is described in
[EEP-54](https://www.erlang.org/erlang-enhancement-proposals/eep-0054.html).

## Callback Functions

The following functions are to be exported from an Error Info handler.
""".
-moduledoc(#{since => "OTP 24.0"}).

%% Supported and documented exported functions in this module.
-export([format_exception/3, format_exception/4]).

-export_type([format_options/0, format_fun/0, stack_trim_fun/0]).

%% Only for internal OTP use.
-export([format_exception/6, format_exception/7, format_exception/8,
         format_stacktrace/4, format_stacktrace/5,
         format_call/4, format_call/5, format_fun/1, format_fun/2]).

-doc "Start column number. Default is 1.".
-type column() :: pos_integer().
-doc """
A fun used to trim the end of the stacktrace. It is called with module,
function, and arity from an entry from the stacktrace. The fun is to return
`true` if the entry should be trimmed, and `false` otherwise. The default value
is:

```text
fun(_, _, _) -> false end
```
""".
-type stack_trim_fun() :: fun((module(), atom(), arity()) -> boolean()).
-doc """
A fun used to format function arguments for BIF and function calls. By default
the following fun will be used:

```erlang
fun(Term, I) -> io_lib:print(Term, I, 80, 30) end
```
""".
-type format_fun() :: fun((term(), column()) -> iolist()).

-doc "A map with formatting options.".
-type format_options() :: #{column => column(),
                            stack_trim_fun => stack_trim_fun(),
                            format_fun => format_fun()}.

-doc """
This callback is called when `format_exception/4` or similar functionality wants
to provide extra information about an error. The `Module`:`Function` called is
the one specificed by the `error_info` map.

The function should return a map with additional information about what have
caused the exception. The possible keys of the map are:

- **`ArgumentPosition = pos_integer()`** - The position of the argument that
  caused the error starting at 1.

- **`general`** - An error that is not associated with any argument caused the
  error.

- **`reason`** - If the `Reason` should be printed differently than the default
  way.

If the text returned includes new-lines, `format_exception/4` will indent the
text correctly.

Example:

```erlang
-module(my_error_module).
-export([atom_to_string/1, format_error/2]).

atom_to_string(Arg) when is_atom(Arg) ->
  atom_to_list(Arg);
atom_to_string(Arg) ->
  erlang:error(badarg,[Arg],
               [{error_info,#{ module => ?MODULE,
                               cause => #{ 1 => "should be an atom" }}}]).

format_error(Reason, [{_M,_F,_As,Info}|_]) ->
  ErrorInfo = proplists:get_value(error_info, Info, #{}),
  ErrorMap = maps:get(cause, ErrorInfo),
  ErrorMap#{ general => "optional general information",
             reason => io_lib:format("~p: ~p",[?MODULE, Reason]) }.
```

```erlang
1> c(my_error_module).
{ok,my_error_module}
2> my_error_module:atom_to_string(1).
** exception error: my_error_module: badarg
     in function  my_error_module:atom_to_string/1
        called as my_error_module:atom_to_string(1)
        *** argument 1: should be an atom
        *** optional general information
```
""".
-doc(#{since => <<"OTP 24.0">>}).
-callback format_error(Reason, StackTrace) -> ErrorDescription when
      Reason :: term(),
      StackTrace :: erlang:stacktrace(),
      ArgumentPosition :: pos_integer(),
      ErrorDescription :: #{ ArgumentPosition =>
                                 unicode:chardata(),
                             general => unicode:chardata(),
                             reason => unicode:chardata()}.

-doc(#{equiv => format_exception/4}).
-doc(#{since => <<"OTP 24.0">>}).
-spec format_exception(Class, Reason, StackTrace) -> unicode:chardata() when
      Class :: 'error' | 'exit' | 'throw',
      Reason :: term(),
      StackTrace :: erlang:stacktrace().

format_exception(Class, Reason, StackTrace) ->
    format_exception(Class, Reason, StackTrace, #{}).

-doc """
Format the error reason and stack back-trace caught using `try` ... `catch` in
the same style as the shell formats them.

Example:

```erlang
try
    do_something()
catch
    C:R:Stk ->
        Message = erl_error:format_exception(C, R, Stk),
        io:format(LogFile, "~ts\n", [Message])
end
```

If `error_info` is provided with the exception, `format_exception` will use that
information to provide additional information about the exception.

Example:

```erlang
try
  erlang:raise(badarg,[],[{error_info,#{}}])
catch
    C:R:Stk ->
        Message = erl_error:format_exception(C, R, Stk),
        io:format(LogFile, "~ts\n", [Message])
end
```

See `erlang:error/3` for details on how to raise an exception with `error_info`
included.
""".
-doc(#{since => <<"OTP 24.0">>}).
-spec format_exception(Class, Reason, StackTrace, Options) -> unicode:chardata() when
      Class :: 'error' | 'exit' | 'throw',
      Reason :: term(),
      StackTrace :: erlang:stacktrace(),
      Options :: format_options().

format_exception(Class, Reason, StackTrace, Options) ->
    Column = maps:get(column, Options, 1),
    StackFun0 = fun(_, _, _) -> false end,
    StackFun = maps:get(stack_trim_fun, Options, StackFun0),
    FormatFun0 = fun(Term, I) -> io_lib:print(Term, I, 80, 30) end,
    FormatFun = maps:get(format_fun, Options, FormatFun0),
    format_exception(Column, Class, Reason, StackTrace, StackFun, FormatFun, unicode).

%%%
%%% Internal functions.
%%%

%%% Formatting of exceptions, mfa:s and funs.

%% -> iolist() (no \n at end)
%% I is the current column, starting from 1 (it will be used
%%   as indentation whenever newline has been inserted);
%% Class, Reason and StackTrace are the exception;
%% FormatFun = fun(Term, I) -> iolist() formats terms;
%% StackFun = fun(Mod, Fun, Arity) -> boolean() is used for trimming the
%%   end of the stack (typically calls to erl_eval are skipped).
-doc false.
format_exception(I, Class, Reason, StackTrace, StackFun, FormatFun) ->
    format_exception(I, Class, Reason, StackTrace, StackFun, FormatFun,
                     latin1).

%% -> iolist() | unicode:charlist() (no \n at end)
%% FormatFun = fun(Term, I) -> iolist() | unicode:charlist().
-doc false.
format_exception(I, Class, Reason, StackTrace, StackFun, FormatFun, Encoding) ->
    FF = wrap_format_fun_2(FormatFun),
    format_exception(I, Class, Reason, StackTrace, StackFun, FF, Encoding, -1).

-doc false.
format_exception(I, Class, Reason, StackTrace, StackFun, FormatFun, Encoding,
                 CharsLimit)
            when is_integer(I), I >= 1, is_function(StackFun, 3), 
                 is_function(FormatFun, 3), is_integer(CharsLimit) ->
    S = n_spaces(I-1),
    {Term,Trace1,Trace} = analyze_exception(Class, Reason, StackTrace),
    StLimit = if
                  CharsLimit < 0 ->
                      CharsLimit;
                  true ->
                      %% Reserve one third for the stacktrace.
                      CharsLimit div 3
              end,

    ErrorMap = get_extended_error(Reason, Trace),

    St = format_stacktrace1(S, Trace, FormatFun, StackFun,
                            Encoding, StLimit, Reason, ErrorMap),
    Lim = sub(sub(CharsLimit, exited(Class), latin1), St, Encoding),
    DefaultReason = explain_reason(Term, Class, Trace1, FormatFun, S, Encoding, Lim),
    Expl0 = maps:get(reason, ErrorMap, DefaultReason),

    FormatString = case Encoding of
                       latin1 -> "~s~s";
                       _ -> "~s~ts"
                   end,
    Expl = io_lib:fwrite(FormatString, [exited(Class), Expl0]),
    case St of
        [] -> Expl;
        _ -> [Expl, $\n, St]
    end.

%% -> iolist() (no \n at end)
-doc false.
format_stacktrace(I, StackTrace, StackFun, FormatFun) ->
    format_stacktrace(I, StackTrace, StackFun, FormatFun, latin1).

%% -> iolist() | unicode:charlist()  (no \n at end)
-doc false.
format_stacktrace(I, StackTrace, StackFun, FormatFun, Encoding)
            when is_integer(I), I >= 1, is_function(StackFun, 3), 
                 is_function(FormatFun, 2) ->
    S = n_spaces(I-1),
    FF = wrap_format_fun_2(FormatFun),
    format_stacktrace1(S, StackTrace, FF, StackFun, Encoding, -1, none).

%% -> iolist() (no \n at end)
-doc false.
format_call(I, ForMForFun, As, FormatFun) ->
    format_call(I, ForMForFun, As, FormatFun, latin1).

%% -> iolist() | unicode:charlist()  (no \n at end)
-doc false.
format_call(I, ForMForFun, As, FormatFun, Enc)
       when is_integer(I), I >= 1, is_list(As), is_function(FormatFun, 2) ->
    FF = wrap_format_fun_2(FormatFun),
    format_call("", n_spaces(I-1), ForMForFun, As, FF, Enc).

%% -> iolist() (no \n at end)
-doc false.
format_fun(Fun) ->
    format_fun(Fun, latin1).

%% -> iolist() (no \n at end)
-doc false.
format_fun(Fun, Enc) when is_function(Fun) ->
    {module, M} = erlang:fun_info(Fun, module),
    {name, F} = erlang:fun_info(Fun, name),
    {arity, A} = erlang:fun_info(Fun, arity),
    case erlang:fun_info(Fun, type) of
        {type, local} when F =:= "" ->
            io_lib:fwrite(<<"~w">>, [Fun]);
        {type, local} when M =:= erl_eval ->
            io_lib:fwrite(<<"interpreted function with arity ~w">>, [A]);
        {type, local} ->
            mfa_to_string(M, F, A, Enc);
        {type, external} ->
            mfa_to_string(M, F, A, Enc)
    end.

wrap_format_fun_2(FormatFun) ->
    fun(T, I1, CL) -> {FormatFun(T, I1), CL} end.

analyze_exception(error, Term, Stack) ->
    case {is_stacktrace(Stack), Stack, Term} of
        {true, [{_,_,As,_}=MFAL|MFAs], function_clause} when is_list(As) ->
            {Term,[MFAL],MFAs};
        {true, [{shell,F,A,_}], function_clause} when is_integer(A) ->
            {Term, [{F,A}], []};
        {true, [{_,_,_,_}=MFAL|MFAs], undef} ->
            {Term,[MFAL],MFAs};
	{true, _, _} ->
	    {Term,[],Stack};
	{false, _, _} ->
	    {{Term,Stack},[],[]}
    end;
analyze_exception(_Class, Term, Stack) ->
    case is_stacktrace(Stack) of
        true ->
            {Term,[],Stack};
        false ->
            {{Term,Stack},[],[]}
    end.

is_stacktrace([]) ->
    true;
is_stacktrace([{M,F,A,I}|Fs])
  when is_atom(M), is_atom(F), is_integer(A), is_list(I) ->
    is_stacktrace(Fs);
is_stacktrace([{M,F,As,I}|Fs])
  when is_atom(M), is_atom(F), length(As) >= 0, is_list(I) ->
    is_stacktrace(Fs);
is_stacktrace(_) ->
    false.

%% ERTS exit codes (some of them are also returned by erl_eval):
explain_reason(badarg, error, [], _PF, _S, _Enc, _CL) ->
    <<"bad argument">>;
explain_reason({badarg,V}, error=Cl, [], PF, S, _Enc, CL) -> % orelse, andalso
    format_value(V, <<"bad argument: ">>, Cl, PF, S, CL);
explain_reason({badkey,V}, error=Cl, [], PF, S, _Enc, CL) ->
    format_value(V, <<"bad key: ">>, Cl, PF, S, CL);
explain_reason({badmap,V}, error=Cl, [], PF, S, _Enc, CL) ->
    format_value(V, <<"bad map: ">>, Cl, PF, S, CL);
explain_reason(badarith, error, [], _PF, _S, _Enc, _CL) ->
    <<"an error occurred when evaluating an arithmetic expression">>;
explain_reason({badarity,{Fun,As}}, error, [], _PF, _S, Enc, _CL)
                                      when is_function(Fun) ->
    %% Only the arity is displayed, not the arguments As.
    io_lib:fwrite(<<"~ts called with ~s">>,
                  [format_fun(Fun, Enc), argss(length(As))]);
explain_reason({badfun,Term}, error=Cl, [], PF, S, _Enc, CL) ->
    format_value(Term, <<"bad function ">>, Cl, PF, S, CL);
explain_reason({badmatch,Term}, error=Cl, [], PF, S, _Enc, CL) ->
    Str = <<"no match of right hand side value ">>,
    format_value(Term, Str, Cl, PF, S, CL);
explain_reason({case_clause,V}, error=Cl, [], PF, S, _Enc, CL) ->
    %% "there is no case clause with a true guard sequence and a
    %% pattern matching..."
    format_value(V, <<"no case clause matching ">>, Cl, PF, S, CL);
explain_reason({else_clause,V}, error=Cl, [], PF, S, _Enc, CL) ->
    format_value(V, <<"no else clause matching ">>, Cl, PF, S, CL);
explain_reason(function_clause, error, [{F,A}], _PF, _S, _Enc, _CL) ->
    %% Shell commands
    FAs = io_lib:fwrite(<<"~w/~w">>, [F, A]),
    [<<"no function clause matching call to ">> | FAs];
explain_reason(function_clause, error=Cl, [{M,F,As,Loc}], PF, S, Enc, CL) ->
    Str = <<"no function clause matching ">>,
    [format_errstr_call(Str, Cl, {M,F}, As, PF, S, Enc, CL),$\s|location(Loc)];
explain_reason(if_clause, error, [], _PF, _S, _Enc, _CL) ->
    <<"no true branch found when evaluating an if expression">>;
explain_reason(noproc, error, [], _PF, _S, _Enc, _CL) ->
    <<"no such process or port">>;
explain_reason(notalive, error, [], _PF, _S, _Enc, _CL) ->
    <<"the node cannot be part of a distributed system">>;
explain_reason(system_limit, error, [], _PF, _S, _Enc, _CL) ->
    <<"a system limit has been reached">>;
explain_reason(timeout_value, error, [], _PF, _S, _Enc, _CL) ->
    <<"bad receive timeout value">>;
explain_reason({try_clause,V}, error=Cl, [], PF, S, _Enc, CL) ->
    %% "there is no try clause with a true guard sequence and a
    %% pattern matching..."
    format_value(V, <<"no try clause matching ">>, Cl, PF, S, CL);
explain_reason(undef, error, [{M,F,A,_}], _PF, _S, Enc, _CL) ->
    %% Only the arity is displayed, not the arguments, if there are any.
    io_lib:fwrite(<<"undefined function ~ts">>,
                  [mfa_to_string(M, F, n_args(A), Enc)]);
explain_reason({shell_undef,F,A,_}, error, [], _PF, _S, Enc, _CL) ->
    %% Give nicer reports for undefined shell functions
    %% (but not when the user actively calls shell_default:F(...)).
    FS = to_string(F, Enc),
    io_lib:fwrite(<<"undefined shell command ~ts/~w">>, [FS, n_args(A)]);
%% Exit codes returned by erl_eval only:
explain_reason({argument_limit,_Fun}, error, [], _PF, _S, _Enc, _CL) ->
    io_lib:fwrite(<<"limit of number of arguments to interpreted function"
                    " exceeded">>, []);
explain_reason({bad_filter,V}, error=Cl, [], PF, S, _Enc, CL) ->
    format_value(V, <<"bad filter ">>, Cl, PF, S, CL);
explain_reason({bad_generator,V}, error=Cl, [], PF, S, _Enc, CL) ->
    format_value(V, <<"bad generator ">>, Cl, PF, S, CL);
explain_reason({bad_generators,V}, error=Cl, [], PF, S, _Enc, CL) ->
    format_value(V, <<"bad generators: ">>, Cl, PF, S, CL);
explain_reason({unbound,V}, error, [], _PF, _S, _Enc, _CL) ->
    io_lib:fwrite(<<"variable ~w is unbound">>, [V]);
%% Exit codes local to the shell module (restricted shell):
explain_reason({restricted_shell_bad_return, V}, exit=Cl, [], PF, S, _Enc, CL) ->
    Str = <<"restricted shell module returned bad value ">>,
    format_value(V, Str, Cl, PF, S, CL);
explain_reason({restricted_shell_disallowed,{ForMF,As}}, 
               exit=Cl, [], PF, S, Enc, CL) ->
    %% ForMF can be a fun, but not a shell fun.
    Str = <<"restricted shell does not allow ">>,
    format_errstr_call(Str, Cl, ForMF, As, PF, S, Enc, CL);
explain_reason(restricted_shell_started, exit, [], _PF, _S, _Enc, _CL) ->
    <<"restricted shell starts now">>;
explain_reason(restricted_shell_stopped, exit, [], _PF, _S, _Enc, _CL) ->
    <<"restricted shell stopped">>;
explain_reason(calling_self, exit, [], _PF, _S, _Enc, _CL) ->
    <<"the current process attempted to call itself">>;
%% Other exit code:
explain_reason(Reason, Class, [], PF, S, _Enc, CL) ->
    {L, _} = PF(Reason, (iolist_size(S)+1) + exited_size(Class), CL),
    L.

n_args(A) when is_integer(A) ->
    A;
n_args(As) when is_list(As) ->
    length(As).

argss(0) ->
    <<"no arguments">>;
argss(1) ->
    <<"one argument">>;
argss(2) ->
    <<"two arguments">>;
argss(I) ->
    io_lib:fwrite(<<"~w arguments">>, [I]).

format_stacktrace1(Sep, Stack, PF, SF, Enc, CL, Reason) ->
    format_stacktrace1(Sep, Stack, PF, SF, Enc, CL, Reason,
                       get_extended_error(Reason, Stack)).
format_stacktrace1(S0, Stack0, PF, SF, Enc, CL, Reason, ErrorMap) ->
    Stack1 = lists:dropwhile(fun({M,F,A,_}) -> SF(M, F, A)
                             end, lists:reverse(Stack0)),
    S = ["  " | S0],
    Stack = lists:reverse(Stack1),
    format_stacktrace2(S, Stack, 1, PF, Enc, CL, Reason, ErrorMap).

format_stacktrace2(_S, _Stack, _N, _PF, _Enc, _CL=0, _Reason, _ErrorMap) ->
    [];
format_stacktrace2(S, [{M,F,A,L}|Fs], N, PF, Enc, CL, Reason, ErrorMap)
  when is_integer(A) ->
    FormattedError = call_format_error(ErrorMap, A, sep(N, S)),
    Cs = io_lib:fwrite(<<"~s~s ~ts ~ts~ts">>,
                       [sep(N, S), origin(N, M, F, A),
                        mfa_to_string(M, F, A, Enc),
                        location(L), FormattedError]),
    CL1 = sub(CL, Cs, Enc),
    [Cs | format_stacktrace2(S, Fs, N + 1, PF, Enc, CL1, Reason, #{})];
format_stacktrace2(S, [{M,F,As,_Info}|Fs], N, PF, Enc, CL, Reason, ErrorMap)
  when is_list(As) ->
    A = length(As),
    CalledAs = [S,<<"   called as ">>],
    C = format_call("", CalledAs, {M,F}, As, PF, Enc, CL),
    FormattedError = call_format_error(ErrorMap, As, sep(N, S)),
    Cs = io_lib:fwrite(<<"~s~s ~ts\n~s~ts~ts">>,
                       [sep(N, S), origin(N, M, F, A),
                        mfa_to_string(M, F, A, Enc),
                        CalledAs, C, FormattedError]),
    CL1 = sub(CL, Cs, Enc),
    [Cs | format_stacktrace2(S, Fs, N + 1, PF, Enc, CL1, Reason, #{})];
format_stacktrace2(_S, [], _N, _PF, _Enc, _CL, _Reason, _ErrorMap) ->
    "".

call_format_error(ErrorMap, As, Sep) ->
    case format_arg_errors(1, As, ErrorMap) of
        [] ->
            [];
        [_|_]=Errors ->
            lists:join([$\n,Sep,<<"   ">>], [""|Errors])
    end.

get_extended_error(Reason, [{M,_F,_As,Info}|_] = StackTrace) ->
    case lists:keyfind(error_info, 1, Info) of
        {error_info,ErrorInfoMap} when is_map(ErrorInfoMap) ->
            FormatModule = maps:get(module, ErrorInfoMap, M),
            FormatFunction = maps:get(function, ErrorInfoMap, format_error),
            try
                FormatModule:FormatFunction(Reason, StackTrace)
            catch
                error:_ ->
                    %% Silently ignore all errors.
                    #{}
            end;
        _ ->
            %% There is no extended error information available.
            #{}
    end;
get_extended_error(_Reason, []) ->
    #{}.

format_arg_errors(ArgNum, A, ErrorMap) when is_integer(A) ->
    format_arg_errors(ArgNum, lists:duplicate(A, A), ErrorMap);
format_arg_errors(ArgNum, [_|As], ErrorMap) ->
    case ErrorMap of
        #{ArgNum := Err} ->
            %% Split the error on newlines and then recombine it
            %% so that each line is correctly indented by the
            %% printer of this error
            [FirstLine | Lines] = string:lexemes(Err, "\r\n"),
            ArgStr = io_lib:format(<<"*** argument ~w: ">>, [ArgNum]),
            [io_lib:format(<<"~ts~ts">>, [ArgStr, FirstLine])] ++
                [io_lib:format(<<"~*ts~ts">>,[string:length(ArgStr), "", Line])
                 || Line <- Lines] ++
                format_arg_errors(ArgNum + 1, As, ErrorMap);
        #{} ->
            format_arg_errors(ArgNum + 1, As, ErrorMap)
    end;
format_arg_errors(_, _, ErrorMap) ->
    case ErrorMap of
        #{ general := Err } ->
            [io_lib:format(<<"*** ~ts">>, [Err])];
        #{} ->
            []
    end.

location(L) ->
    File = proplists:get_value(file, L),
    Line = proplists:get_value(line, L),
    if
	File =/= undefined, Line =/= undefined ->
	    io_lib:format("(~ts:~w)", [File, Line]);
	true ->
	    ""
    end.

sep(1, S) -> S;
sep(_, S) -> [$\n | S].

origin(1, M, F, A) ->
    case is_op({M, F}, n_args(A)) of
        {yes, F} -> <<"in operator ">>;
        no -> <<"in function ">>
    end;
origin(_N, _M, _F, _A) ->
    <<"in call from">>.

format_errstr_call(ErrStr, Class, ForMForFun, As, PF, Pre0, Enc, CL) ->
    Pre1 = [Pre0 | n_spaces(exited_size(Class))],
    format_call(ErrStr, Pre1, ForMForFun, As, PF, Enc, CL).

format_call(ErrStr, Pre1, ForMForFun, As, PF, Enc) ->
    format_call(ErrStr, Pre1, ForMForFun, As, PF, Enc, -1).

format_call(ErrStr, Pre1, ForMForFun, As, PF, Enc, CL) ->
    Arity = length(As),
    [ErrStr |
     case is_op(ForMForFun, Arity) of
         {yes,Op} -> 
             format_op(ErrStr, Pre1, Op, As, PF, Enc, CL);
         no ->
             MFs = mf_to_string(ForMForFun, Arity, Enc),
             I1 = string:length([Pre1,ErrStr|MFs]),
             S1 = pp_arguments(PF, As, I1, Enc, CL),
             S2 = pp_arguments(PF, As, string:length([Pre1|MFs]), Enc, CL),
             S3 = pp_arguments(PF, [a2345,b2345], I1, Enc, CL),
             Long = count_nl(S3) > 0,
             case Long or (count_nl(S2) < count_nl(S1)) of
                 true ->
                     [$\n, Pre1, MFs, S2];
                 false ->
                     [MFs, S1]
             end
    end].

format_op(ErrStr, Pre, Op, [A1], PF, _Enc, CL) ->
    OpS = io_lib:fwrite(<<"~s ">>, [Op]),
    I1 = iolist_size([ErrStr,Pre,OpS]),
    {S, _} = PF(A1, I1+1, CL),
    [OpS | S];
format_op(ErrStr, Pre, Op, [A1, A2], PF, Enc, CL) ->
    I1 = iolist_size([ErrStr,Pre]),
    {S1, CL1} = PF(A1, I1+1, CL),
    {S2, _} = PF(A2, I1+1, CL1),
    OpS = atom_to_list(Op),
    Pre1 = [$\n | n_spaces(I1)],
    case count_nl(S1) > 0 of
        true -> 
            [S1,Pre1,OpS,Pre1|S2];
        false ->
            OpS2 = io_lib:fwrite(<<" ~s ">>, [Op]),
            Size1 = iolist_size([ErrStr,Pre|OpS2]),
            Size2 = size(Enc, S1),
            {S2_2, _} = PF(A2, Size1+Size2+1, CL1),
            case count_nl(S2) < count_nl(S2_2) of
                true ->
                    [S1,Pre1,OpS,Pre1|S2];
                false ->
                    [S1,OpS2|S2_2]
            end
    end.

pp_arguments(PF, As, I, Enc, CL) ->
    case {As, printable_list(Enc, As)} of
        {[Int | T], true} ->
            L = integer_to_list(Int),
            Ll = length(L),
            A = list_to_atom(lists:duplicate(Ll, $a)),
            {S0, _} = PF([A | T], I+1, CL),
            S = unicode:characters_to_list(S0, Enc),
            brackets_to_parens([$[,L,string:slice(S, 1+Ll)], Enc);
        _ -> 
            {S, _CL1} = PF(As, I+1, CL),
            brackets_to_parens(S, Enc)
    end.

brackets_to_parens(S, Enc) ->
    B = unicode:characters_to_binary(S, Enc),
    Sz = byte_size(B) - 2,
    <<$[,R:Sz/binary,$]>> = B,
    [$(,R,$)].

printable_list(latin1, As) ->
    io_lib:printable_latin1_list(As);
printable_list(_, As) ->
    io_lib:printable_list(As).

mfa_to_string(M, F, A, Enc) ->
    io_lib:fwrite(<<"~ts/~w">>, [mf_to_string({M, F}, A, Enc), A]).

mf_to_string({M, F}, A, Enc) ->
    case erl_internal:bif(M, F, A) of
        true ->
            io_lib:fwrite(<<"~w">>, [F]);
        false ->
            case is_op({M, F}, A) of
                {yes, '/'} ->
                    io_lib:fwrite(<<"~w">>, [F]);
                {yes, F} ->
                    atom_to_list(F);
                no ->
                    FS = to_string(F, Enc),
                    io_lib:fwrite(<<"~w:~ts">>, [M, FS])
            end
    end;
mf_to_string(Fun, _A, Enc) when is_function(Fun) ->
    format_fun(Fun, Enc);
mf_to_string(F, _A, Enc) ->
    FS = to_string(F, Enc),
    io_lib:fwrite(<<"~ts">>, [FS]).

format_value(V, ErrStr, Class, PF, S, CL) ->
    Pre1Sz = exited_size(Class),
    {S1, _} = PF(V, Pre1Sz + iolist_size([S, ErrStr]) + 1, CL),
    [ErrStr | case count_nl(S1) of
                  N1 when N1 > 1 ->
                      {S2, _} = PF(V, iolist_size(S) + 1 + Pre1Sz, CL),
                      case count_nl(S2) < N1 of
                          true ->
                              [$\n, S, n_spaces(Pre1Sz) | S2];
                          false ->
                              S1
                      end;
                  _ ->
                      S1
              end].

%% Handles deep lists, but not all iolists.
count_nl([E | Es]) ->
    count_nl(E) + count_nl(Es);
count_nl($\n) ->
    1;
count_nl(Bin) when is_binary(Bin) ->
    count_nl(binary_to_list(Bin));
count_nl(_) ->
    0.

n_spaces(N) ->
    lists:duplicate(N, $\s).

is_op(ForMForFun, A) ->
    try 
        {erlang,F} = ForMForFun,
        _ = erl_internal:op_type(F, A), 
        {yes,F}
    catch error:_ -> no
    end.

exited_size(Class) ->
    iolist_size(exited(Class)).

exited(error) ->
    <<"exception error: ">>;
exited(exit) ->
    <<"exception exit: ">>;
exited(throw) ->
    <<"exception throw: ">>.

to_string(A, latin1) ->
    io_lib:write_atom_as_latin1(A);
to_string(A, _) ->
    io_lib:write_atom(A).

%% Make sure T does change sign.
sub(T, _, _Enc) when T < 0 -> T;
sub(T, S, Enc) ->
    sub(T, size(Enc, S)).

sub(T, Sz) when T >= Sz ->
    T - Sz;
sub(_T, _Sz) ->
    0.

size(latin1, S) ->
    iolist_size(S);
size(_, S) ->
    string:length(S).
