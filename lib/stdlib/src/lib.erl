%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(lib).

-export([flush_receive/0, error_message/2, progname/0, nonl/1, send/2,
	 sendw/2, eval_str/1]).

-export([format_exception/6, format_stacktrace/4, 
         format_call/4, format_fun/1]).

-spec flush_receive() -> 'ok'.

flush_receive() ->
    receive
	_Any ->
	    flush_receive()
    after
	0 ->
	    ok
    end.

%%
%% Functions for doing standard system format i/o.
%%
-spec error_message(atom() | string() | binary(), [term()]) -> 'ok'.

error_message(Format, Args) ->
    io:format(<<"** ~s **\n">>, [io_lib:format(Format, Args)]).

%% Return the name of the script that starts (this) erlang 
%%
-spec progname() -> atom().

progname() ->
    case init:get_argument(progname) of
	{ok, [[Prog]]} ->
	    list_to_atom(Prog);
	_Other ->
	    no_prog_name
    end.

-spec nonl(string()) -> string().

nonl([10]) -> [];
nonl([]) -> [];
nonl([H|T]) -> [H|nonl(T)].

-spec send(pid() | atom() | {atom(), node()}, term()) -> term().

send(To, Msg) -> To ! Msg.

-spec sendw(pid() | atom() | {atom(), node()}, term()) -> term().

sendw(To, Msg) ->
    To ! {self(), Msg},
    receive 
	Reply -> Reply
    end.

%% eval_str(InStr) -> {ok, OutStr} | {error, ErrStr'}
%%   InStr must represent a body

-define(result(F,D), lists:flatten(io_lib:format(F, D))).

-spec eval_str(string() | binary()) -> {'ok', string()} | {'error', string()}.

eval_str(Str) when is_list(Str) ->
    case erl_scan:tokens([], Str, 0) of
	{more, _} ->
	    {error, "Incomplete form (missing .<cr>)??"};
	{done, {ok, Toks, _}, Rest} ->
	    case all_white(Rest) of
		true ->
		    case erl_parse:parse_exprs(Toks) of
			{ok, Exprs} ->
			    case catch erl_eval:exprs(Exprs, []) of
				{value, Val, _} ->
				    {ok, Val};
				Other ->
				    {error, ?result("*** eval: ~p", [Other])}
			    end;
			{error, {_Line, Mod, Args}} ->
                            Msg = ?result("*** ~s",[Mod:format_error(Args)]),
                            {error, Msg}
		    end;
		false ->
		    {error, ?result("Non-white space found after "
				    "end-of-form :~s", [Rest])}
		end
    end;
eval_str(Bin) when is_binary(Bin) ->
    eval_str(binary_to_list(Bin)).

all_white([$\s|T]) -> all_white(T);
all_white([$\n|T]) -> all_white(T);
all_white([$\t|T]) -> all_white(T);
all_white([])      -> true;
all_white(_)       -> false.

%%% Formatting of exceptions, mfa:s and funs.

%% -> iolist() (no \n at end)
%% I is the current column, starting from 1 (it will be used
%%   as indentation whenever newline has been inserted);
%% Class, Reason and StackTrace are the exception;
%% FormatFun = fun(Term, I) -> iolist() formats terms;
%% StackFun = fun(Mod, Fun, Arity) -> bool() is used for trimming the
%%   end of the stack (typically calls to erl_eval are skipped).
format_exception(I, Class, Reason, StackTrace, StackFun, FormatFun) 
            when is_integer(I), I >= 1, is_function(StackFun, 3), 
                 is_function(FormatFun, 2) ->
    S = n_spaces(I-1),
    {Term,Trace1,Trace} = analyze_exception(Class, Reason, StackTrace),
    Expl0 = explain_reason(Term, Class, Trace1, FormatFun, S),
    Expl = io_lib:fwrite(<<"~s~s">>, [exited(Class), Expl0]),
    case format_stacktrace1(S, Trace, FormatFun, StackFun) of
        [] -> Expl;
        Stack -> [Expl, $\n, Stack]
    end.

%% -> iolist() (no \n at end)
format_stacktrace(I, StackTrace, StackFun, FormatFun) 
            when is_integer(I), I >= 1, is_function(StackFun, 3), 
                 is_function(FormatFun, 2) ->
    S = n_spaces(I-1),
    format_stacktrace1(S, StackTrace, FormatFun, StackFun).

%% -> iolist() (no \n at end)
format_call(I, ForMForFun, As, FormatFun) when is_integer(I), I >= 1,
                                               is_list(As),
                                               is_function(FormatFun, 2) ->
    format_call("", n_spaces(I-1), ForMForFun, As, FormatFun).

%% -> iolist() (no \n at end)
format_fun(Fun) when is_function(Fun) ->
    {module, M} = erlang:fun_info(Fun, module),
    {name, F} = erlang:fun_info(Fun, name),
    {arity, A} = erlang:fun_info(Fun, arity),
    case erlang:fun_info(Fun, type) of
        {type, local} when F =:= "" ->
            io_lib:fwrite(<<"~w">>, [Fun]);
        {type, local} when M =:= erl_eval ->
            io_lib:fwrite(<<"interpreted function with arity ~w">>, [A]);
        {type, local} ->
            mfa_to_string(M, F, A);
        {type, external} ->
            mfa_to_string(M, F, A)
    end.

analyze_exception(error, Term, Stack) ->
    case {is_stacktrace(Stack), Stack, Term} of
        {true, [{_M,_F,As}=MFA|MFAs], function_clause} when is_list(As) -> 
            {Term,[MFA],MFAs};
        {true, [{shell,F,A}], function_clause} when is_integer(A) ->
            {Term, [{F,A}], []};
        {true, [{_M,_F,_AorAs}=MFA|MFAs], undef} ->
            {Term,[MFA],MFAs};
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
is_stacktrace([{M,F,A}|Fs]) when is_atom(M), is_atom(F), is_integer(A) ->
    is_stacktrace(Fs);
is_stacktrace([{M,F,As}|Fs]) when is_atom(M), is_atom(F), length(As) >= 0 ->
    is_stacktrace(Fs);
is_stacktrace(_) ->
    false.

%% ERTS exit codes (some of them are also returned by erl_eval):
explain_reason(badarg, error, [], _PF, _S) ->
    <<"bad argument">>;
explain_reason({badarg,V}, error=Cl, [], PF, S) -> % orelse, andalso
    format_value(V, <<"bad argument: ">>, Cl, PF, S);
explain_reason(badarith, error, [], _PF, _S) ->
    <<"bad argument in an arithmetic expression">>;
explain_reason({badarity,{Fun,As}}, error, [], _PF, _S) 
                                      when is_function(Fun) ->
    %% Only the arity is displayed, not the arguments As.
    io_lib:fwrite(<<"~s called with ~s">>, 
                  [format_fun(Fun), argss(length(As))]);
explain_reason({badfun,Term}, error=Cl, [], PF, S) ->
    format_value(Term, <<"bad function ">>, Cl, PF, S);
explain_reason({badmatch,Term}, error=Cl, [], PF, S) ->
    format_value(Term, <<"no match of right hand side value ">>, Cl, PF, S);
explain_reason({case_clause,V}, error=Cl, [], PF, S) ->
    %% "there is no case clause with a true guard sequence and a
    %% pattern matching..."
    format_value(V, <<"no case clause matching ">>, Cl, PF, S);
explain_reason(function_clause, error, [{F,A}], _PF, _S) ->
    %% Shell commands
    FAs = io_lib:fwrite(<<"~w/~w">>, [F, A]),
    [<<"no function clause matching call to ">> | FAs];
explain_reason(function_clause, error=Cl, [{M,F,As}], PF, S) ->
    Str = <<"no function clause matching ">>,
    format_errstr_call(Str, Cl, {M,F}, As, PF, S);
explain_reason(if_clause, error, [], _PF, _S) ->
    <<"no true branch found when evaluating an if expression">>;
explain_reason(noproc, error, [], _PF, _S) ->
    <<"no such process or port">>;
explain_reason(notalive, error, [], _PF, _S) ->
    <<"the node cannot be part of a distributed system">>;
explain_reason(system_limit, error, [], _PF, _S) ->
    <<"a system limit has been reached">>;
explain_reason(timeout_value, error, [], _PF, _S) ->
    <<"bad receive timeout value">>;
explain_reason({try_clause,V}, error=Cl, [], PF, S) ->
    %% "there is no try clause with a true guard sequence and a
    %% pattern matching..."
    format_value(V, <<"no try clause matching ">>, Cl, PF, S);
explain_reason(undef, error, [{M,F,A}], _PF, _S) ->
    %% Only the arity is displayed, not the arguments, if there are any.
    io_lib:fwrite(<<"undefined function ~s">>, 
                  [mfa_to_string(M, F, n_args(A))]);
explain_reason({shell_undef,F,A}, error, [], _PF, _S) ->
    %% Give nicer reports for undefined shell functions
    %% (but not when the user actively calls shell_default:F(...)).
    io_lib:fwrite(<<"undefined shell command ~s/~w">>, [F, n_args(A)]);
%% Exit codes returned by erl_eval only:
explain_reason({argument_limit,_Fun}, error, [], _PF, _S) ->
    io_lib:fwrite(<<"limit of number of arguments to interpreted function"
                    " exceeded">>, []);
explain_reason({bad_filter,V}, error=Cl, [], PF, S) ->
    format_value(V, <<"bad filter ">>, Cl, PF, S);
explain_reason({bad_generator,V}, error=Cl, [], PF, S) ->
    format_value(V, <<"bad generator ">>, Cl, PF, S);
explain_reason({unbound,V}, error, [], _PF, _S) ->
    io_lib:fwrite(<<"variable ~w is unbound">>, [V]);
%% Exit codes local to the shell module (restricted shell):
explain_reason({restricted_shell_bad_return, V}, exit=Cl, [], PF, S) ->
    Str = <<"restricted shell module returned bad value ">>,
    format_value(V, Str, Cl, PF, S);
explain_reason({restricted_shell_disallowed,{ForMF,As}}, 
               exit=Cl, [], PF, S) ->
    %% ForMF can be a fun, but not a shell fun.
    Str = <<"restricted shell does not allow ">>,
    format_errstr_call(Str, Cl, ForMF, As, PF, S);
explain_reason(restricted_shell_started, exit, [], _PF, _S) ->
    <<"restricted shell starts now">>;
explain_reason(restricted_shell_stopped, exit, [], _PF, _S) ->
    <<"restricted shell stopped">>;
%% Other exit code:
explain_reason(Reason, Class, [], PF, S) ->
    PF(Reason, (iolist_size(S)+1) + exited_size(Class)).

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

format_stacktrace1(S0, Stack0, PF, SF) ->
    Stack1 = lists:dropwhile(fun({M,F,A}) -> SF(M, F, A)
                             end, lists:reverse(Stack0)),
    S = ["  " | S0],
    Stack = lists:reverse(Stack1),
    format_stacktrace2(S, Stack, 1, PF).

format_stacktrace2(S, [{M,F,A}|Fs], N, PF) when is_integer(A) ->
    [io_lib:fwrite(<<"~s~s ~s">>, 
                   [sep(N, S), origin(N, M, F, A), mfa_to_string(M, F, A)])
     | format_stacktrace2(S, Fs, N + 1, PF)];
format_stacktrace2(S, [{M,F,As}|Fs], N, PF) when is_list(As) ->
    A = length(As),
    CalledAs = [S,<<"   called as ">>],
    C = format_call("", CalledAs, {M,F}, As, PF),
    [io_lib:fwrite(<<"~s~s ~s\n~s~s">>,
		   [sep(N, S), origin(N, M, F, A), mfa_to_string(M, F, A),
                    CalledAs, C])
     | format_stacktrace2(S, Fs, N + 1, PF)];
format_stacktrace2(_S, [], _N, _PF) ->
    "".

sep(1, S) -> S;
sep(_, S) -> [$\n | S].

origin(1, M, F, A) ->
    case is_op({M, F}, n_args(A)) of
        {yes, F} -> <<"in operator ">>;
        no -> <<"in function ">>
    end;
origin(_N, _M, _F, _A) ->
    <<"in call from">>.

format_errstr_call(ErrStr, Class, ForMForFun, As, PF, Pre0) ->
    Pre1 = [Pre0 | n_spaces(exited_size(Class))],
    format_call(ErrStr, Pre1, ForMForFun, As, PF).

format_call(ErrStr, Pre1, ForMForFun, As, PF) ->
    Arity = length(As),
    [ErrStr |
     case is_op(ForMForFun, Arity) of
         {yes,Op} -> 
             format_op(ErrStr, Pre1, Op, As, PF);
         no ->
             MFs = mf_to_string(ForMForFun, Arity),
             I1 = iolist_size([Pre1,ErrStr|MFs]),
             S1 = pp_arguments(PF, As, I1),
             S2 = pp_arguments(PF, As, iolist_size([Pre1|MFs])),
             Long = count_nl(pp_arguments(PF, [a2345,b2345], I1)) > 0,
             case Long or (count_nl(S2) < count_nl(S1)) of
                 true ->
                     [$\n, Pre1, MFs, S2];
                 false ->
                     [MFs, S1]
             end
    end].

format_op(ErrStr, Pre, Op, [A1], PF) ->
    OpS = io_lib:fwrite(<<"~s ">>, [Op]),
    I1 = iolist_size([ErrStr,Pre,OpS]),
    [OpS | PF(A1, I1+1)];
format_op(ErrStr, Pre, Op, [A1, A2], PF) ->
    I1 = iolist_size([ErrStr,Pre]),
    S1 = PF(A1, I1+1),
    S2 = PF(A2, I1+1),
    OpS = atom_to_list(Op),
    Pre1 = [$\n | n_spaces(I1)],
    case count_nl(S1) > 0 of
        true -> 
            [S1,Pre1,OpS,Pre1|S2];
        false ->
            OpS2 = io_lib:fwrite(<<" ~s ">>, [Op]),
            S2_2 = PF(A2, iolist_size([ErrStr,Pre,S1|OpS2])+1),
            case count_nl(S2) < count_nl(S2_2) of
                true ->
                    [S1,Pre1,OpS,Pre1|S2];
                false ->
                    [S1,OpS2|S2_2]
            end
    end.

pp_arguments(PF, As, I) ->
    case {As, io_lib:printable_list(As)} of
        {[Int | T], true} ->
            L = integer_to_list(Int),
            Ll = length(L),
            A = list_to_atom(lists:duplicate(Ll, $a)),
            S0 = binary_to_list(iolist_to_binary(PF([A | T], I+1))),
            brackets_to_parens([$[,L,string:sub_string(S0, 2+Ll)]);
        _ -> 
            brackets_to_parens(PF(As, I+1))
    end.

brackets_to_parens(S) ->
    B = iolist_to_binary(S),
    Sz = byte_size(B) - 2,
    <<$[,R:Sz/binary,$]>> = B,
    [$(,R,$)].

mfa_to_string(M, F, A) ->
    io_lib:fwrite(<<"~s/~w">>, [mf_to_string({M, F}, A), A]).

mf_to_string({M, F}, A) ->
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
                    io_lib:fwrite(<<"~w:~w">>, [M, F])
            end
    end;
mf_to_string(Fun, _A) when is_function(Fun) ->
    format_fun(Fun);
mf_to_string(F, _A) ->
    io_lib:fwrite(<<"~w">>, [F]).

format_value(V, ErrStr, Class, PF, S) ->
    Pre1Sz = exited_size(Class),
    S1 = PF(V, Pre1Sz + iolist_size([S, ErrStr])+1),
    [ErrStr | case count_nl(S1) of
                  N1 when N1 > 1 ->
                      S2 = PF(V, iolist_size(S) + 1 + Pre1Sz),
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
