%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
-module(lib).

-export([flush_receive/0, error_message/2, progname/0, nonl/1, send/2,
	 sendw/2, eval_str/1]).

-export([extended_parse_exprs/1, extended_parse_term/1,
         subst_values_for_vars/2]).

-export([format_exception/6, format_exception/7,
         format_stacktrace/4, format_stacktrace/5,
         format_call/4, format_call/5, format_fun/1]).

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
-spec error_message(Format, Args) -> 'ok' when
      Format :: io:format(),
      Args :: [term()].

error_message(Format, Args) ->
    io:format(<<"** ~ts **\n">>, [io_lib:format(Format, Args)]).

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

-spec nonl(String1) -> String2 when
      String1 :: string(),
      String2 :: string().

nonl([10]) -> [];
nonl([]) -> [];
nonl([H|T]) -> [H|nonl(T)].

-spec send(To, Msg) -> Msg when
      To :: pid() | atom() | {atom(), node()},
      Msg :: term().

send(To, Msg) -> To ! Msg.

-spec sendw(To, Msg) -> term() when
      To :: pid() | atom() | {atom(), node()},
      Msg :: term().

sendw(To, Msg) ->
    To ! {self(), Msg},
    receive 
	Reply -> Reply
    end.

%% eval_str(InStr) -> {ok, OutStr} | {error, ErrStr'}
%%   InStr must represent a body
%%   Note: If InStr is a binary it has to be a Latin-1 string.
%%   If you have a UTF-8 encoded binary you have to call
%%   unicode:characters_to_list/1 before the call to eval_str().

-define(result(F,D), lists:flatten(io_lib:format(F, D))).

-spec eval_str(string() | unicode:latin1_binary()) ->
                      {'ok', string()} | {'error', string()}.

eval_str(Str) when is_list(Str) ->
    case erl_scan:tokens([], Str, 0) of
	{more, _} ->
	    {error, "Incomplete form (missing .<cr>)??"};
	{done, {ok, Toks, _}, Rest} ->
	    case all_white(Rest) of
		true ->
		    case erl_parse:parse_exprs(Toks) of
			{ok, Exprs} ->
			    case catch erl_eval:exprs(Exprs, erl_eval:new_bindings()) of
				{value, Val, _} ->
				    {ok, Val};
				Other ->
				    {error, ?result("*** eval: ~p", [Other])}
			    end;
			{error, {_Line, Mod, Args}} ->
                            Msg = ?result("*** ~ts",[Mod:format_error(Args)]),
                            {error, Msg}
		    end;
		false ->
		    {error, ?result("Non-white space found after "
				    "end-of-form :~ts", [Rest])}
		end
    end;
eval_str(Bin) when is_binary(Bin) ->
    eval_str(binary_to_list(Bin)).

all_white([$\s|T]) -> all_white(T);
all_white([$\n|T]) -> all_white(T);
all_white([$\t|T]) -> all_white(T);
all_white([])      -> true;
all_white(_)       -> false.

%% `Tokens' is assumed to have been scanned with the 'text' option.
%% The annotations of the returned expressions are locations.
%%
%% Can handle pids, ports, references, and external funs ("items").
%% Known items are represented by variables in the erl_parse tree, and
%% the items themselves are stored in the returned bindings.

-spec extended_parse_exprs(Tokens) ->
                {'ok', ExprList, Bindings} | {'error', ErrorInfo} when
      Tokens :: [erl_scan:token()],
      ExprList :: [erl_parse:abstract_expr()],
      Bindings :: erl_eval:binding_struct(),
      ErrorInfo :: erl_parse:error_info().

extended_parse_exprs(Tokens) ->
    Ts = tokens_fixup(Tokens),
    case erl_parse:parse_exprs(Ts) of
        {ok, Exprs0} ->
            {Exprs, Bs} = expr_fixup(Exprs0),
            {ok, reset_expr_anno(Exprs), Bs};
        _ErrorInfo ->
            erl_parse:parse_exprs(reset_token_anno(Ts))
    end.

tokens_fixup([]) -> [];
tokens_fixup([T|Ts]=Ts0) ->
    try token_fixup(Ts0) of
        {NewT, NewTs} ->
            [NewT|tokens_fixup(NewTs)]
    catch
        _:_ ->
            [T|tokens_fixup(Ts)]
    end.

token_fixup(Ts) ->
    {AnnoL, NewTs, FixupTag} = unscannable(Ts),
    String = lists:append([erl_anno:text(A) || A <- AnnoL]),
    _ = (fixup_fun(FixupTag))(String),
    NewAnno = erl_anno:set_text(fixup_text(FixupTag), hd(AnnoL)),
    {{string, NewAnno, String}, NewTs}.

unscannable([{'#', A1}, {var, A2, 'Fun'}, {'<', A3}, {atom, A4, _},
             {'.', A5}, {float, A6, _}, {'>', A7}|Ts]) ->
    {[A1, A2, A3, A4, A5, A6, A7], Ts, function};
unscannable([{'#', A1}, {var, A2, 'Fun'}, {'<', A3}, {atom, A4, _},
             {'.', A5}, {atom, A6, _}, {'.', A7}, {integer, A8, _},
             {'>', A9}|Ts]) ->
    {[A1, A2, A3, A4, A5, A6, A7, A8, A9], Ts, function};
unscannable([{'<', A1}, {float, A2, _}, {'.', A3}, {integer, A4, _},
             {'>', A5}|Ts]) ->
    {[A1, A2, A3, A4, A5], Ts, pid};
unscannable([{'#', A1}, {var, A2, 'Port'}, {'<', A3}, {float, A4, _},
             {'>', A5}|Ts]) ->
    {[A1, A2, A3, A4, A5], Ts, port};
unscannable([{'#', A1}, {var, A2, 'Ref'}, {'<', A3}, {float, A4, _},
             {'.', A5}, {float, A6, _}, {'>', A7}|Ts]) ->
    {[A1, A2, A3, A4, A5, A6, A7], Ts, reference}.

expr_fixup(Expr0) ->
    {Expr, Bs, _} = expr_fixup(Expr0, erl_eval:new_bindings(), 1),
    {Expr, Bs}.

expr_fixup({string,A,S}=T, Bs0, I) ->
    try string_fixup(A, S) of
        Value ->
            Var = new_var(I),
            Bs = erl_eval:add_binding(Var, Value, Bs0),
            {{var, A, Var}, Bs, I+1}
    catch
        _:_ ->
            {T, Bs0, I}
    end;
expr_fixup(Tuple, Bs0, I0) when is_tuple(Tuple) ->
    {L, Bs, I} = expr_fixup(tuple_to_list(Tuple), Bs0, I0),
    {list_to_tuple(L), Bs, I};
expr_fixup([E0|Es0], Bs0, I0) ->
    {E, Bs1, I1} = expr_fixup(E0, Bs0, I0),
    {Es, Bs, I} = expr_fixup(Es0, Bs1, I1),
    {[E|Es], Bs, I};
expr_fixup(T, Bs, I) ->
    {T, Bs, I}.

string_fixup(A, S) ->
    Text = erl_anno:text(A),
    FixupTag = fixup_tag(Text, S),
    (fixup_fun(FixupTag))(S).

new_var(I) ->
    list_to_atom(lists:concat(['__ExtendedParseExprs_', I, '__'])).

reset_token_anno(Tokens) ->
    [setelement(2, T, (reset_anno())(element(2, T))) || T <- Tokens].

reset_expr_anno(Exprs) ->
    [erl_parse:map_anno(reset_anno(), E) || E <- Exprs].

reset_anno() ->
    fun(A) -> erl_anno:new(erl_anno:location(A)) end.

fixup_fun(function)  -> fun function/1;
fixup_fun(pid)       -> fun erlang:list_to_pid/1;
fixup_fun(port)      -> fun erlang:list_to_port/1;
fixup_fun(reference) -> fun erlang:list_to_ref/1.

function(S) ->
    %% External function.
    {ok, [_, _, _,
          {atom, _, Module}, _,
          {atom, _, Function}, _,
          {integer, _, Arity}|_], _} = erl_scan:string(S),
    erlang:make_fun(Module, Function, Arity).

fixup_text(function)  -> "function";
fixup_text(pid)       -> "pid";
fixup_text(port)      -> "port";
fixup_text(reference) -> "reference".

fixup_tag("function",  "#"++_) -> function;
fixup_tag("pid",       "<"++_) -> pid;
fixup_tag("port",      "#"++_) -> port;
fixup_tag("reference", "#"++_) -> reference.

%%% End of extended_parse_exprs.

%% `Tokens' is assumed to have been scanned with the 'text' option.
%%
%% Can handle pids, ports, references, and external funs.

-spec extended_parse_term(Tokens) ->
                {'ok', Term} | {'error', ErrorInfo} when
      Tokens :: [erl_scan:token()],
      Term :: term(),
      ErrorInfo :: erl_parse:error_info().

extended_parse_term(Tokens) ->
    case extended_parse_exprs(Tokens) of
        {ok, [Expr], Bindings} ->
            try normalise(Expr, Bindings) of
                Term ->
                    {ok, Term}
            catch
                _:_ ->
                    Loc = erl_anno:location(element(2, Expr)),
                    {error,{Loc,?MODULE,"bad term"}}
            end;
        {ok, [_,Expr|_], _Bindings} ->
                Loc = erl_anno:location(element(2, Expr)),
                {error,{Loc,?MODULE,"bad term"}};
        {error, _} = Error ->
            Error
    end.

%% From erl_parse.
normalise({var, _, V}, Bs) ->
    {value, Value} = erl_eval:binding(V, Bs),
    Value;
normalise({char,_,C}, _Bs) -> C;
normalise({integer,_,I}, _Bs) -> I;
normalise({float,_,F}, _Bs) -> F;
normalise({atom,_,A}, _Bs) -> A;
normalise({string,_,S}, _Bs) -> S;
normalise({nil,_}, _Bs) -> [];
normalise({bin,_,Fs}, Bs) ->
    {value, B, _} =
	eval_bits:expr_grp(Fs, [],
			   fun(E, _) ->
				   {value, normalise(E, Bs), []}
			   end, [], true),
    B;
normalise({cons,_,Head,Tail}, Bs) ->
    [normalise(Head, Bs)|normalise(Tail, Bs)];
normalise({tuple,_,Args}, Bs) ->
    list_to_tuple(normalise_list(Args, Bs));
normalise({map,_,Pairs}, Bs) ->
    maps:from_list(lists:map(fun
		%% only allow '=>'
		({map_field_assoc,_,K,V}) ->
                                     {normalise(K, Bs),normalise(V, Bs)}
	    end, Pairs));
%% Special case for unary +/-.
normalise({op,_,'+',{char,_,I}}, _Bs) -> I;
normalise({op,_,'+',{integer,_,I}}, _Bs) -> I;
normalise({op,_,'+',{float,_,F}}, _Bs) -> F;
normalise({op,_,'-',{char,_,I}}, _Bs) -> -I;   %Weird, but compatible!
normalise({op,_,'-',{integer,_,I}}, _Bs) -> -I;
normalise({op,_,'-',{float,_,F}}, _Bs) -> -F;
normalise({'fun',_,{function,{atom,_,M},{atom,_,F},{integer,_,A}}}, _Bs) ->
    %% Since "#Fun<M.F.A>" is recognized, "fun M:F/A" should be too.
    fun M:F/A.

normalise_list([H|T], Bs) ->
    [normalise(H, Bs)|normalise_list(T, Bs)];
normalise_list([], _Bs) ->
    [].

%% To be used on ExprList and Bindings returned from extended_parse_exprs().
%% Substitute {value, A, Item} for {var, A, ExtendedParseVar}.
%% {value, A, Item} is a shell/erl_eval convention, and for example
%% the linter cannot handle it.

-spec subst_values_for_vars(ExprList, Bindings) -> [term()] when
      ExprList :: [erl_parse:abstract_expr()],
      Bindings :: erl_eval:binding_struct().

subst_values_for_vars({var, A, V}=Var, Bs) ->
    case erl_eval:binding(V, Bs) of
        {value, Value} ->
            {value, A, Value};
        unbound ->
            Var
    end;
subst_values_for_vars(L, Bs) when is_list(L) ->
    [subst_values_for_vars(E, Bs) || E <- L];
subst_values_for_vars(T, Bs) when is_tuple(T) ->
    list_to_tuple(subst_values_for_vars(tuple_to_list(T), Bs));
subst_values_for_vars(T, _Bs) ->
    T.

%%% Formatting of exceptions, mfa:s and funs.

%% -> iolist() (no \n at end)
%% I is the current column, starting from 1 (it will be used
%%   as indentation whenever newline has been inserted);
%% Class, Reason and StackTrace are the exception;
%% FormatFun = fun(Term, I) -> iolist() formats terms;
%% StackFun = fun(Mod, Fun, Arity) -> boolean() is used for trimming the
%%   end of the stack (typically calls to erl_eval are skipped).
format_exception(I, Class, Reason, StackTrace, StackFun, FormatFun) ->
    format_exception(I, Class, Reason, StackTrace, StackFun, FormatFun,
                     latin1).

%% -> iolist() | unicode:charlist() (no \n at end)
%% FormatFun = fun(Term, I) -> iolist() | unicode:charlist().
format_exception(I, Class, Reason, StackTrace, StackFun, FormatFun, Encoding)
            when is_integer(I), I >= 1, is_function(StackFun, 3), 
                 is_function(FormatFun, 2) ->
    S = n_spaces(I-1),
    {Term,Trace1,Trace} = analyze_exception(Class, Reason, StackTrace),
    Expl0 = explain_reason(Term, Class, Trace1, FormatFun, S, Encoding),
    FormatString = case Encoding of
                       latin1 -> "~s~s";
                       _ -> "~s~ts"
                   end,
    Expl = io_lib:fwrite(FormatString, [exited(Class), Expl0]),
    case format_stacktrace1(S, Trace, FormatFun, StackFun, Encoding) of
        [] -> Expl;
        Stack -> [Expl, $\n, Stack]
    end.

%% -> iolist() (no \n at end)
format_stacktrace(I, StackTrace, StackFun, FormatFun) ->
    format_stacktrace(I, StackTrace, StackFun, FormatFun, latin1).

%% -> iolist() | unicode:charlist()  (no \n at end)
format_stacktrace(I, StackTrace, StackFun, FormatFun, Encoding)
            when is_integer(I), I >= 1, is_function(StackFun, 3), 
                 is_function(FormatFun, 2) ->
    S = n_spaces(I-1),
    format_stacktrace1(S, StackTrace, FormatFun, StackFun, Encoding).

%% -> iolist() (no \n at end)
format_call(I, ForMForFun, As, FormatFun) ->
    format_call(I, ForMForFun, As, FormatFun, latin1).

%% -> iolist() | unicode:charlist()  (no \n at end)
format_call(I, ForMForFun, As, FormatFun, Enc)
       when is_integer(I), I >= 1, is_list(As), is_function(FormatFun, 2) ->
    format_call("", n_spaces(I-1), ForMForFun, As, FormatFun, Enc).

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
explain_reason(badarg, error, [], _PF, _S, _Enc) ->
    <<"bad argument">>;
explain_reason({badarg,V}, error=Cl, [], PF, S, _Enc) -> % orelse, andalso
    format_value(V, <<"bad argument: ">>, Cl, PF, S);
explain_reason(badarith, error, [], _PF, _S, _Enc) ->
    <<"an error occurred when evaluating an arithmetic expression">>;
explain_reason({badarity,{Fun,As}}, error, [], _PF, _S, _Enc)
                                      when is_function(Fun) ->
    %% Only the arity is displayed, not the arguments As.
    io_lib:fwrite(<<"~s called with ~s">>, 
                  [format_fun(Fun), argss(length(As))]);
explain_reason({badfun,Term}, error=Cl, [], PF, S, _Enc) ->
    format_value(Term, <<"bad function ">>, Cl, PF, S);
explain_reason({badmatch,Term}, error=Cl, [], PF, S, _Enc) ->
    Str = <<"no match of right hand side value ">>,
    format_value(Term, Str, Cl, PF, S);
explain_reason({case_clause,V}, error=Cl, [], PF, S, _Enc) ->
    %% "there is no case clause with a true guard sequence and a
    %% pattern matching..."
    format_value(V, <<"no case clause matching ">>, Cl, PF, S);
explain_reason(function_clause, error, [{F,A}], _PF, _S, _Enc) ->
    %% Shell commands
    FAs = io_lib:fwrite(<<"~w/~w">>, [F, A]),
    [<<"no function clause matching call to ">> | FAs];
explain_reason(function_clause, error=Cl, [{M,F,As,Loc}], PF, S, Enc) ->
    Str = <<"no function clause matching ">>,
    [format_errstr_call(Str, Cl, {M,F}, As, PF, S, Enc),$\s|location(Loc)];
explain_reason(if_clause, error, [], _PF, _S, _Enc) ->
    <<"no true branch found when evaluating an if expression">>;
explain_reason(noproc, error, [], _PF, _S, _Enc) ->
    <<"no such process or port">>;
explain_reason(notalive, error, [], _PF, _S, _Enc) ->
    <<"the node cannot be part of a distributed system">>;
explain_reason(system_limit, error, [], _PF, _S, _Enc) ->
    <<"a system limit has been reached">>;
explain_reason(timeout_value, error, [], _PF, _S, _Enc) ->
    <<"bad receive timeout value">>;
explain_reason({try_clause,V}, error=Cl, [], PF, S, _Enc) ->
    %% "there is no try clause with a true guard sequence and a
    %% pattern matching..."
    format_value(V, <<"no try clause matching ">>, Cl, PF, S);
explain_reason(undef, error, [{M,F,A,_}], _PF, _S, _Enc) ->
    %% Only the arity is displayed, not the arguments, if there are any.
    io_lib:fwrite(<<"undefined function ~s">>, 
                  [mfa_to_string(M, F, n_args(A))]);
explain_reason({shell_undef,F,A,_}, error, [], _PF, _S, _Enc) ->
    %% Give nicer reports for undefined shell functions
    %% (but not when the user actively calls shell_default:F(...)).
    io_lib:fwrite(<<"undefined shell command ~s/~w">>, [F, n_args(A)]);
%% Exit codes returned by erl_eval only:
explain_reason({argument_limit,_Fun}, error, [], _PF, _S, _Enc) ->
    io_lib:fwrite(<<"limit of number of arguments to interpreted function"
                    " exceeded">>, []);
explain_reason({bad_filter,V}, error=Cl, [], PF, S, _Enc) ->
    format_value(V, <<"bad filter ">>, Cl, PF, S);
explain_reason({bad_generator,V}, error=Cl, [], PF, S, _Enc) ->
    format_value(V, <<"bad generator ">>, Cl, PF, S);
explain_reason({unbound,V}, error, [], _PF, _S, _Enc) ->
    io_lib:fwrite(<<"variable ~w is unbound">>, [V]);
%% Exit codes local to the shell module (restricted shell):
explain_reason({restricted_shell_bad_return, V}, exit=Cl, [], PF, S, _Enc) ->
    Str = <<"restricted shell module returned bad value ">>,
    format_value(V, Str, Cl, PF, S);
explain_reason({restricted_shell_disallowed,{ForMF,As}}, 
               exit=Cl, [], PF, S, Enc) ->
    %% ForMF can be a fun, but not a shell fun.
    Str = <<"restricted shell does not allow ">>,
    format_errstr_call(Str, Cl, ForMF, As, PF, S, Enc);
explain_reason(restricted_shell_started, exit, [], _PF, _S, _Enc) ->
    <<"restricted shell starts now">>;
explain_reason(restricted_shell_stopped, exit, [], _PF, _S, _Enc) ->
    <<"restricted shell stopped">>;
%% Other exit code:
explain_reason(Reason, Class, [], PF, S, _Enc) ->
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

format_stacktrace1(S0, Stack0, PF, SF, Enc) ->
    Stack1 = lists:dropwhile(fun({M,F,A,_}) -> SF(M, F, A)
                             end, lists:reverse(Stack0)),
    S = ["  " | S0],
    Stack = lists:reverse(Stack1),
    format_stacktrace2(S, Stack, 1, PF, Enc).

format_stacktrace2(S, [{M,F,A,L}|Fs], N, PF, Enc) when is_integer(A) ->
    [io_lib:fwrite(<<"~s~s ~s ~s">>,
                   [sep(N, S), origin(N, M, F, A),
		    mfa_to_string(M, F, A),
		    location(L)])
     | format_stacktrace2(S, Fs, N + 1, PF, Enc)];
format_stacktrace2(S, [{M,F,As,_}|Fs], N, PF, Enc) when is_list(As) ->
    A = length(As),
    CalledAs = [S,<<"   called as ">>],
    C = format_call("", CalledAs, {M,F}, As, PF, Enc),
    [io_lib:fwrite(<<"~s~s ~s\n~s~ts">>,
		   [sep(N, S), origin(N, M, F, A), mfa_to_string(M, F, A),
                    CalledAs, C])
     | format_stacktrace2(S, Fs, N + 1, PF, Enc)];
format_stacktrace2(_S, [], _N, _PF, _Enc) ->
    "".

location(L) ->
    File = proplists:get_value(file, L),
    Line = proplists:get_value(line, L),
    if
	File =/= undefined, Line =/= undefined ->
	    io_lib:format("(~s, line ~w)", [File, Line]);
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

format_errstr_call(ErrStr, Class, ForMForFun, As, PF, Pre0, Enc) ->
    Pre1 = [Pre0 | n_spaces(exited_size(Class))],
    format_call(ErrStr, Pre1, ForMForFun, As, PF, Enc).

format_call(ErrStr, Pre1, ForMForFun, As, PF, Enc) ->
    Arity = length(As),
    [ErrStr |
     case is_op(ForMForFun, Arity) of
         {yes,Op} -> 
             format_op(ErrStr, Pre1, Op, As, PF, Enc);
         no ->
             MFs = mf_to_string(ForMForFun, Arity),
             I1 = iolist_size([Pre1,ErrStr|MFs]),
             S1 = pp_arguments(PF, As, I1, Enc),
             S2 = pp_arguments(PF, As, iolist_size([Pre1|MFs]), Enc),
             Long = count_nl(pp_arguments(PF, [a2345,b2345], I1, Enc)) > 0,
             case Long or (count_nl(S2) < count_nl(S1)) of
                 true ->
                     [$\n, Pre1, MFs, S2];
                 false ->
                     [MFs, S1]
             end
    end].

format_op(ErrStr, Pre, Op, [A1], PF, _Enc) ->
    OpS = io_lib:fwrite(<<"~s ">>, [Op]),
    I1 = iolist_size([ErrStr,Pre,OpS]),
    [OpS | PF(A1, I1+1)];
format_op(ErrStr, Pre, Op, [A1, A2], PF, Enc) ->
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
            Size1 = iolist_size([ErrStr,Pre|OpS2]),
            {Size2,S1_2} = size(Enc, S1),
            S2_2 = PF(A2, Size1+Size2+1),
            case count_nl(S2) < count_nl(S2_2) of
                true ->
                    [S1_2,Pre1,OpS,Pre1|S2];
                false ->
                    [S1_2,OpS2|S2_2]
            end
    end.

pp_arguments(PF, As, I, Enc) ->
    case {As, printable_list(Enc, As)} of
        {[Int | T], true} ->
            L = integer_to_list(Int),
            Ll = length(L),
            A = list_to_atom(lists:duplicate(Ll, $a)),
            S0 = unicode:characters_to_list(PF([A | T], I+1), Enc),
            brackets_to_parens([$[,L,string:sub_string(S0, 2+Ll)], Enc);
        _ -> 
            brackets_to_parens(PF(As, I+1), Enc)
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

size(latin1, S) ->
    {iolist_size(S),S};
size(_, S0) ->
    S = unicode:characters_to_list(S0, unicode),
    true = is_list(S),
    {length(S),S}.
